{== Unit StreamIO =====================================================}
{: Implements a text-file device driver that allows textfile-style I/O
   on streams.
@author Dr. Peter Below
@desc   Version 1.0 created 4 Januar 2001<BR>
        Current revision 1.0<BR>
        Last modified       4 Januar 2001<P>                           }
{======================================================================}
Unit StreamIO;

Interface
Uses classes;

{-- AssignStream ------------------------------------------------------}
{: Attach a stream to a Textfile to allow I/O via WriteLn/ReadLn
@Param F is the textfile to attach the stream to
@Param S is the stream
@Precondition  S <> nil
@Desc The passed streams position will be set to 0 by Reset and Rewrite
  and to the streams end by Append. The stream is not freed when the
  textfile is closed via CloseFile and it has to stay in existence as
  long as the textfile is open.
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Procedure AssignStream( Var F: Textfile; S: TStream );

Implementation

Uses sysutils;

{-- GetDevStream ------------------------------------------------------}
{: Get the stream reference stored in the textrec userdata area
@Param F is the textfile record
@Returns the stream reference
@Postcondition result <> nil
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function GetDevStream( Var F: TTextRec ): TStream;
  Begin { GetDevStream }
    Move( F.Userdata, Result, Sizeof( Result ));
    Assert( Assigned( Result ));
  End; { GetDevStream }

{-- DevIn -------------------------------------------------------------}
{: Called by Read, ReadLn etc. to fill the textfiles buffer from the
   stream.
@Param F is the textfile to operate on
@Returns 0 (no error)
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function DevIn( Var F: TTextRec ): Integer;
  Begin { DevIn }
    Result := 0;
    With F Do Begin
      BufEnd := GetDevStream(F).Read( BufPtr^, BufSize );
      BufPos := 0;
    End; { With }
  End; { DevIn }

{-- DevFlushIn --------------------------------------------------------}
{: A dummy method, flush on input does nothing.
@Param F is the textfile to operate on
@Returns 0 (no error)
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function DevFlushIn( Var F: TTextRec ): Integer;
  Begin { DevFlushIn }
    Result := 0;
  End; { DevFlushIn }

{-- DevOut ------------------------------------------------------------}
{: Write the textfile buffers content to the stream. Called by Write,
   WriteLn when the buffer becomes full. Also called by Flush.
@Param F is the textfile to operate on
@Returns 0 (no error)
@Raises EStreamError if the write failed for some reason.
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function DevOut( Var F: TTextRec ): Integer;
  Begin { DevOut }
    Result := 0;
    With F Do
      If BufPos > 0 Then Begin
        GetDevStream(F).WriteBuffer( BufPtr^, BufPos );
        BufPos := 0;
      End; { If }
  End; { DevOut }

{-- DevClose ----------------------------------------------------------}
{: Called by Closefile. Does nothing here.
@Param F is the textfile to operate on
@Returns 0 (no error)
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function DevClose( Var F: TTextRec ): Integer;
  Begin { DevClose }
    Result := 0;
  End; { DevClose }

{-- DevOpen -----------------------------------------------------------}
{: Called by Reset, Rewrite, or Append to prepare the textfile for I/O
@Param F is the textfile to operate on
@Returns 0 (no error)
}{ Created 4.1.2001 by P. Below
-----------------------------------------------------------------------}
Function DevOpen( Var F: TTextRec ): Integer;
  Begin { DevOpen }
    Result := 0;
    With F Do Begin
      Case Mode Of
        fmInput: Begin { Reset }
            InOutFunc := @DevIn;
            FlushFunc := @DevFlushIn;
            BufPos := 0;
            BufEnd := 0;
            GetDevStream( F ).Position := 0;
          End; { Case fmInput }
        fmOutput: Begin { Rewrite }
            InOutFunc := @DevOut;
            FlushFunc := @DevOut;
            BufPos := 0;
            BufEnd := 0;
            GetDevStream( F ).Position := 0;
          End; { Case fmOutput }
        fmInOut: Begin { Append }
            Mode := fmOutput;
            DevOpen( F );
            GetDevStream(F).Seek( 0, soFromEnd );
          End; { Case fmInOut }
      End; { Case }
    End; { With }
  End; { DevOpen }

Procedure AssignStream( Var F: Textfile; S: TStream );
  Begin { AssignStream }
    Assert( Assigned( S ));
    With TTextRec(F) Do Begin
      Mode := fmClosed;
      BufSize := SizeOf(Buffer);
      BufPtr := @Buffer;
      OpenFunc := @DevOpen;
      CloseFunc := @DevClose;
      Name[0] := #0;
      { Store stream reference into Userdata area }
      Move( S, Userdata, Sizeof(S));
    End; { With }
  End; { AssignStream }

End { StreamIO }.
