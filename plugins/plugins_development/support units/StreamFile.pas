unit StreamFile;
{
        Unix Stream File Interface
        Copyright 1996 by John Miano Software
        miano@worldnet.att.net

}
interface

Uses
  SysUtils ;

Procedure AssignStreamFile (var F : Text ; Filename : String) ;

implementation

Const
  BufferSize = 128; // was: 128

Type
  TStreamBuffer = Array [1..High (Integer)] of Char ;
  TStreamBufferPointer = ^TStreamBuffer ;

  TStreamFileRecord = Record
    Case Integer Of
      1:
        (
          Filehandle : Integer ;
          Buffer : TStreamBufferPointer ;
          BufferOffset : Integer ;
          ReadCount : Integer ;
        ) ;
      2:
        (
          Dummy : Array [1 .. 32] Of Char
        )
    End ;


Function StreamFileOpen (var F : TTextRec) : Integer ;
  Var
    Status : Integer ;
  Begin
  With TStreamFileRecord (F.UserData) Do
    Begin
    GetMem (Buffer, BufferSize) ;
    Case F.Mode Of
      fmInput:
        FileHandle := FileOpen (StrPas (F.Name), fmShareDenyNone) ;
      fmOutput:
        FileHandle := FileCreate (StrPas (F.Name)) ;
      fmInOut:
        Begin
        FileHandle := FileOpen (StrPas (F.Name), fmShareDenyNone Or
fmOpenWrite or fmOpenRead) ;
        If FileHandle <> -1 Then
          status := FileSeek (FileHandle, 0, 2) ; { Move to end of file. }
        F.Mode := fmOutput ;
        End ;
      End ;
    BufferOffset := 0 ;
    ReadCount := 0 ;
    F.BufEnd := 0 ;  { If this is not here it thinks we are at eof. }
    If FileHandle = -1 Then
      Result := -1
    Else
      Result := 0 ;
    End ;
  End ;

Function StreamFileInOut (var F : TTextRec) : Integer ;
  Procedure Read (var Data : TStreamFileRecord) ;
    Procedure CopyData ;
      Begin
      While (F.BufEnd < Sizeof (F.Buffer) - 2)
            And (Data.BufferOffset <= Data.ReadCount)
            And (Data.Buffer [Data.BufferOffset] <> #10) Do
        Begin
        F.Buffer [F.BufEnd] := Data.Buffer^ [Data.BufferOffset] ;
        Inc (Data.BufferOffset) ;
        Inc (F.BufEnd) ;
        End ;
      If Data.Buffer [Data.BufferOffset] = #10 Then
        Begin
        F.Buffer [F.BufEnd] := #13 ;
        Inc (F.BufEnd) ;
        F.Buffer [F.BufEnd] := #10 ;
        Inc (F.BufEnd) ;
        Inc (Data.BufferOffset) ;
        End ;
      End ;

    Begin

    F.BufEnd := 0 ;
    F.BufPos := 0 ;
    F.Buffer := '' ;
    Repeat
      Begin
      If (Data.ReadCount = 0) Or (Data.BufferOffset > Data.ReadCount) Then
        Begin
        Data.BufferOffset := 1 ;
        Data.ReadCount := FileRead (Data.FileHandle, Data.Buffer^, BufferSize);
        End ;
      CopyData ;
      End Until (Data.ReadCount = 0)
                Or (F.BufEnd >= Sizeof (F.Buffer) - 2) ;
    Result := 0 ;
    End ;

  Procedure Write (var Data : TStreamFileRecord) ;
    Var
      Status : Integer ;
      Destination : Integer ;
      II : Integer ;
    Begin
    With TStreamFileRecord (F.UserData) Do
      Begin
      Destination := 0 ;
      For II := 0 To F.BufPos - 1 Do
        Begin
        If F.Buffer [II] <> #13 Then
          Begin
          Inc (Destination) ;
          Buffer^[Destination] := F.Buffer [II] ;
          End ;
        End ;
      Status := FileWrite (FileHandle, Buffer^, Destination) ;
      F.BufPos := 0 ;
      Result := 0 ;
      End ;
    End ;
  Begin
  Case F.Mode Of
    fmInput:
      Read (TStreamFileRecord (F.UserData)) ;
    fmOutput:
      Write (TStreamFileRecord (F.UserData)) ;
    End ;
  End ;

Function StreamFileFlush (var F : TTextRec) : Integer ;
  Begin
  Result := 0 ;
  End ;

Function StreamFileClose (var F : TTextRec) : Integer ;
  Begin
  With TStreamFileRecord (F.UserData) Do
    Begin
    FreeMem (Buffer) ;
    FileClose (FileHandle) ;
    End ;
  Result := 0 ;
  End ;

Procedure AssignStreamFile (var F : Text ; Filename : String) ;
  Begin
  With TTextRec (F) Do
    Begin
    Mode := fmClosed ;
    BufPtr := @Buffer ;
    BufSize := Sizeof (Buffer) ;
    OpenFunc := @StreamFileOpen ;
    InOutFunc := @StreamFileInOut ;
    FlushFunc := @StreamFileFlush ;
    CloseFunc := @StreamFileClose ;
    StrPLCopy (Name, FileName, Sizeof(Name) - 1) ;
    End ;
  End ;
end.
