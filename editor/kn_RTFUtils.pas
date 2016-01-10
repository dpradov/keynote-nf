unit kn_RTFUtils;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland) [^1]
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^2]

 [^1]: Based on "CR_UORichEdit.pas";
       Enhanced TRichEdit support for DelphiDrop by UnitOOPS SOftware.
       Based on code examples by Peter Below and Ralph Friedman (TeamB).

 [^2]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

 
interface

(*
Note: Replaced by EM_SETTEXTEX Message
*)

uses Windows, Messages, Controls, Classes,
     ComCtrls, SysUtils, RichEdit, RxRichEd, Graphics;


procedure PutRichText(
    const aRTFString: string;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );

procedure PutRichTextW(
    const aRTFString: WideString;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );

function GetRichText(
    const aRichEdit : TRxRichEdit;
    const asRTF, selectionOnly : boolean
  ) : string;

function URLToRTF( fn : wideString; enTextoURL: boolean ) : wideString;
function URLFromRTF( fn : wideString ) : wideString;

function TextToUseInRTF( UnicodeText : wideString ): string;
function GetRTFColor(Color: TColor): string;

function CleanRTF(const RTF: string; var nRTF: string): boolean;

function GetAuxiliarEditorControl(LoadInitiallyFromEditor: TRxRichEdit= nil;
                                  FromSelection: Boolean= True): TRxRichEdit;
procedure FreeAuxiliarEditorControl(FlushToEditor: TRxRichEdit= nil;
                                    DoInsert: boolean= true;
                                    FreeAndNilControl: boolean= false);


implementation
uses WideStrUtils, StrUtils, TntSystem, kn_Main;

var
   RTFAux : TRxRichEdit;


function EMStreamInCallback(dwCookie: Longint; pbBuff: PByte;
                            cb: Longint; var pcb: Longint): Longint; stdcall;
// Callback function for EM_STREAMIN handling.
// This code, and the following "AddRTFToRichEdit" procedure based on corrected
// versions of code examples by Ralph Friedman (TeamB).
var
  theStream: TStringStream;
begin
  // dwCookie is Application-defined, so we're passing the stream containing
  // the formatted text to be added.
  theStream := TStringStream(dwCookie);
  result := 0;

  with theStream do
  begin
    // Deal with the precise case where we get to the end of the buffer.
    // Happens for very small buffers (probably never for RTF due to all of the
    // junk that surrounds the smallest piece of RTF), but definitely for text!
    // This case was missing from the original code.
    if (Size = position) then
    begin
      pcb := 0;
      exit;
    end;

    if (Size - Position) <= cb then
    begin
      pcb := Size;
      Read(pbBuff^, Size);
    end
    else
    begin
      pcb := cb;
      Read(pbBuff^, cb);
    end;
  end;
end;


function EMStreamOutCallback(dwCookie: Longint; pbBuff: PByte;
                            cb: Longint; var pcb: Longint): Longint; stdcall;
// Callback function for EM_STREAMOUT handling.
var
  theStream: TStringStream;//TMemoryStream;
begin
  // dwCookie is Application-defined, so we're passing the stream
  // that will get the streamed text.
  theStream := TStringStream(dwCookie);//TMemoryStream(dwCookie);
  Result := 0;

  with theStream do
  begin
    pcb := cb;
    Write(pbBuff^, cb);
  end;
end;

//Note: It would be possible to use directly the following message: EM_SETTEXTEX Message (if RichEditVersion >= 3)
// See: http://msdn.microsoft.com/en-us/library/bb774284(VS.85).aspx

procedure AnsiPutRichText(
    const aRTFString: string;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );
var
  aES : TEditStream;
  aMS : TStringStream;
  SF_FLAGS : integer;
begin
  if not assigned(aRichEdit) then exit;

  with aRichEdit do
  begin
    // Put the new RTF into a string stream
    aMS := TStringStream.Create(aRTFString);
    try
      // Set the stream to zero (paranoia)
      aMS.Position := 0;

      // Set up the TEditStream structure
      aES.dwCookie    := longint(aMS);
      aES.dwError     := 0;
      aES.pfnCallback := @EMStreamInCallback;

      // Build the correct flags for EM_STREAMIN
      SF_FLAGS := SFF_SELECTION;
      if isRTF then
        SF_FLAGS := SF_FLAGS or SF_RTF
      else
        SF_FLAGS := SF_FLAGS or SF_TEXT;

      // If we're appending, we need to set our selection at the end of
      // the control
      if ( not DoInsert ) then
      begin
        aRichEdit.SelStart := -1;
        aRichEdit.selLength := 0;
      end;

      // Send the EM_STREAMIN message.
      aRichEdit.Perform(EM_STREAMIN, SF_FLAGS, longint(@aES));
    finally
      // free the stream
      aMS.Free;
    end;
  end; // with
end; // AnsiPutRichText


function GetRichText(
    const aRichEdit : TRxRichEdit;
    const asRTF, selectionOnly : boolean
  ) : string;
var
  aES: TEditStream;
  aMS: TStringStream;
  SF_FLAGS: integer;
begin
  result := ''; // Fall-through value

  if not assigned(aRichEdit) then exit;

  with aRichEdit do
  begin
    // Put the new RTF into a memory stream
    aMS := TStringStream.Create('');
    try
      aMS.Position := 0;

      // Set up the TEditStream structure
      aES.dwCookie    := longint(aMS);
      aES.dwError     := 0;
      aES.pfnCallback := @EMStreamOutCallback;

      // Build the correct flags for EM_STREAMOUT
      SF_FLAGS := 0;

      if asRTF then
        SF_FLAGS := SF_FLAGS or SF_RTF
      else
        SF_FLAGS := SF_FLAGS or SF_TEXT;

      if selectionOnly then
        SF_FLAGS := SF_FLAGS or SFF_SELECTION;

      // Send the EM_STREAMOUT message.
      aRichEdit.Perform(EM_STREAMOUT, SF_FLAGS, longint(@aES));

      // Now we have the content on aMS.  Put it on a string instead.
      result := PChar(aMS.DataString);

    finally
      // free the stream
      aMS.Free;
    end;
  end; // with
end; // GetRichText;


// To use the filename in {\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink}}}
// we must convert each '\' to four '\' or to '/'. Example: D:\kk.txt -> D:\\\\kk.txt or D:/kk.txt
// In "hyperlink" \\192.168.0.1\folder\leeme.txt -> "\\\\\\\\192.168.0.1/folder/leeme.txt" or "\\\\\\\\192.168.0.1\\folder\\leeme.txt"
// In textOfHyperlink \\192.168.0.1\folder\leeme.txt -> "\\\\192.168.0.1\\folder\\leeme.txt"
function URLToRTF( fn : wideString; enTextoURL: boolean ) : wideString;
begin
  if enTextoURL then begin
      result:= WideStringReplace(fn,'\','\\', [rfReplaceAll]);
      end
  else begin
      result:= WideStringReplace(fn,'\\',chr(1), [rfReplaceAll]);
      result:= WideStringReplace(result,'\','\\', [rfReplaceAll]);
      result:= WideStringReplace(result,chr(1),'\\\\\\\\', [rfReplaceAll]);
  end;
end; // URLToRTF

function URLFromRTF( fn : wideString ) : wideString;
begin
  result:= WideStringReplace(fn,'\\\\',chr(1), [rfReplaceAll]);
  result:= WideStringReplace(result,'\\','\', [rfReplaceAll]);
  result:= WideStringReplace(result, chr(1), '\\\\', [rfReplaceAll]);
end; // URLFromRTF

//-----------------------


// Requirements: Rich Edit 3.0

const
  EM_SETTEXTEX =       WM_USER + 97;
  ST_DEFAULT =           $00000000;
  ST_KEEPUNDO =          $00000001;
  ST_SELECTION =         $00000002;

type
  _SetTextEx = record
    flags: DWORD;              {Option flags. It can be any reasonable combination of the following flags.
                                ST_DEFAULT
                                    Deletes the undo stack, discards rich-text formatting, replaces all text.
                                ST_KEEPUNDO
                                    Keeps the undo stack.
                                ST_SELECTION
                                    Replaces selection and keeps rich-text formatting.
                               }
    codepage: UINT;            { code page for translation (CP_ACP for default,
                                 1200 for Unicode 					 }
  end;
  TSetTextEX = _SetTextEx;


procedure PutRichText(
    const aRTFString: string;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );
var
  aSTE: TSetTextEx;
begin
  if not assigned(aRichEdit) or (aRTFString = '') then exit;


  if (RichEditVersion < 3) then begin
     AnsiPutRichText(aRTFString, aRichEdit, isRTF, DoInsert);
     exit;
  end;

  try
    aSTE.flags:= ST_SELECTION or ST_KEEPUNDO;
    aSTE.codepage:= CP_ACP;  // CP_ACP= ANSI codepage => use the currently set default Windows ANSI codepage.

    // If we're appending, we need to set our selection at the end of the control
    if ( not DoInsert ) then
    begin
      aRichEdit.SelStart := -1;
      aRichEdit.selLength := 0;
    end;

    SendMessage(aRichEdit.Handle, EM_SETTEXTEX, longint(@aSTE), longint(PChar(aRTFString)));
  finally
  end;
end; // PutRichText


procedure PutRichTextW(
    const aRTFString: wideString;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );
var
  aSTE: TSetTextEx;
  S: AnsiString;
begin
  if not assigned(aRichEdit) or (aRTFString = '') then exit;

  if (RichEditVersion < 3) then begin
     S:= aRTFString;
     AnsiPutRichText(S, aRichEdit, isRTF, DoInsert);
     exit;
  end;

  try
    aSTE.flags:= ST_SELECTION or ST_KEEPUNDO;
    aSTE.codepage:= CP_UTF8;  // 1200 (Unicode); With 1200 didn't work

    // If we're appending, we need to set our selection at the end of the control
    if ( not DoInsert ) then
    begin
      aRichEdit.SelStart := -1;
      aRichEdit.selLength := 0;
    end;

    S:= WideStringToUTF8(aRTFString);
    SendMessage(aRichEdit.Handle, EM_SETTEXTEX, longint(@aSTE), longint(PChar(S)));
  finally
  end;
end; // PutRichTextW


// Returns a text in ASCII with characters with 8 bits escaped with \' and unicode characters
// escaped with \u. Tab character is also converted to \tab
//  Examples: "có" --> "c\'f3"  "Việt" --> "Vi\u7879?t"
//
//  References: Special characters in RTF: http://www.biblioscape.com/rtf15_spec.htm#Heading46
//             Unicode characters in RTF: http://www.biblioscape.com/rtf15_spec.htm#Heading9
//  Delphi: http://www.delphibasics.co.uk/RTL.asp?Name=Char
//          http://www.delphibasics.co.uk/ByFunction.asp?Main=Strings

function TextToUseInRTF( UnicodeText : wideString ): string;
var
  i, val: Integer;
begin
  Result:= '';

  UnicodeText:= WideStringReplace(UnicodeText,'\','\\', [rfReplaceAll]);
  UnicodeText:= WideStringReplace(UnicodeText,'{','\{', [rfReplaceAll]);
  UnicodeText:= WideStringReplace(UnicodeText,'}','\}', [rfReplaceAll]);

  for i := 1 to length(UnicodeText) do
  begin
    val:= integer(UnicodeText[i]);   // Ord(..) is equivalent
    if val = 9 then
       Result := Result + '\tab '

    else if (val = 13) then
       Result := Result + '\par '

    else if (val = 10) then
        // do nothing
        
    else if val >= 127 then
       if val <= 255 then
           Result := Result + '\''' + IntToHex(val, 2)
       else
           Result := Result + '\u' + IntToStr(val) + '?'
    else
       Result := Result + Char(UnicodeText[i]);
  end;
end;

function GetRTFColor(Color: TColor): string;
var
  R, G, B: byte;
begin
  R := GetRValue (Color); {red}
  G := GetGValue (Color); {green}
  B := GetBValue (Color); {blue}
  Result:= '\red' + IntToStr(R) + '\green' + IntToStr(G) + '\blue' + IntToStr(B) + ';';
end;


(*
This function resolve the problem described in issues #59 and #524:
   https://github.com/dpradov/keynote-nf/issues/59

#59: Certain hyperlinks may cause knt file to grow in size suddenly (geometrical increase..)
----------
{\field{\*\fldinst{HYPERLINK "hyperlink"
\\\\\\\\t "_blank" }}{\fldrslt{\cf2\lang255\ul textOfHyperlink}}}

Correct, "clean" hyperlink:
{\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink}}}

The "_blank" (can be any string) is inoffensive. But the frament \\\\\\\\x
gives too much problem with RichText control. Each time that RTF is readen by
the a RichText control that string is duplicated:
the \\\\\\\\t  is converted to \\\\\\\\\\\\\\\\t, then to
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\t,  etc).

If you don't save the file doesn't matter but if you save the file then
the duplicated strings are saved, and the file can blow out.

#524 : Problem with MS Sans Serif in Windows 10 in certain situations
----
In some ocassions that the generated code is not properly managed. Example:
Bad: {\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang3082{\fonttbl{\f0\fnil MS Sans Serif;}} .....
Ok: {\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang3082{\fonttbl{\f0\fnil\fcharset0 MS Sans Serif;}} .....
In Windows XP and in Windows 7 (at least) this is not a problem, because the
RichEdit control manage both rtf code correctly.
For new nodes the problem is avoided using a default font different to MS Sans Serif.
But for existing nodes with that problem (not occurs in all) the solution is
to replace "\f0\fnil " with "\f0\fnil\fcharset0 ".
*)

function CleanRTF(const RTF: string; var nRTF: string): boolean;
var
  p, p2, pHR : integer;
begin
    Result:= false;

    if ( pos('\f0\fnil ', RTF) > 0 ) then begin
        nRTF:= ReplaceStr(RTF, '\f0\fnil ', '\f0\fnil\fcharset0 ');
        Result:= true;
    end;

    if ( pos('\\\\', RTF) <= 0 ) then exit;

    if not Result then
       nRTF:= RTF;

    p:= 1;
    repeat
       p:= posEx('\fldinst{HYPERLINK', nRTF, p);  // (length "\fldinst{HYPERLINK": 18)
       if ( p > 0 ) then begin
          Inc(p, 19);
          if (nRTF[p] <> '"') then begin
             // RichEdit versions >=5 allow only one space between HYPERLINK and "
             p2:= p;
             while (p <= Length(nRTF)) and (nRTF[p] In [' ',#13,#10]) do
                Inc(p);
             if p > Length(nRTF) then exit;
             if (nRTF[p] = '"') then begin
                nRTF[p2-1]:= ' ';
                delete(nRTF, p2, p-p2);
                Result:= true;
             end
             else
                continue;
          end;
          pHR:= posEx('"', nRTF, p+1);
          if pHR <= 0 then continue;
          if nRTF[pHR+1] <> '}' then begin
             p2:= posEx('}', nRTF, pHR+1);
             if p2 > 0 then begin
                delete(nRTF, pHR+1, p2-pHR-1);
                Result:= true;
             end;
             p:= pHR+1;
          end;
       end;

    until p <= 0;

end;


function GetAuxiliarEditorControl(LoadInitiallyFromEditor: TRxRichEdit= nil;
                                  FromSelection: Boolean= True): TRxRichEdit;
var
  Stream: TStream;
  Str: String;
begin
   if not assigned(RTFAux) then begin
      RTFAux := TRxRichEdit.Create(Form_Main);
      RTFAux.Visible:= False;
      RTFAux.OnProtectChangeEx:= Form_Main.RxRTFAuxiliarProtectChangeEx;
      RTFAux.Parent:= Form_Main;
   end;
   RTFAux.Clear;
   RTFAux.WordWrap:= false;
   RTFAux.StreamMode := [];
   if assigned(LoadInitiallyFromEditor) then begin
      Str:= GetRichText(LoadInitiallyFromEditor, true, FromSelection);
      Stream:= TStringStream.Create(Str);
      try
         RTFAux.Lines.LoadFromStream(Stream);
      finally
         Stream.Free;
      end;
   end;
   Result:= RTFAux;
end;

procedure FreeAuxiliarEditorControl(FlushToEditor: TRxRichEdit= nil;
                                    DoInsert: boolean= true;
                                    FreeAndNilControl: boolean= false);
var
  Str: String;
begin
   if assigned(RTFAux) then begin
      if assigned(FlushToEditor) then begin
        Str:= GetRichText( RTFAux, true, false);
        PutRichText( Str, FlushToEditor, true, DoInsert);
      end;

      RTFAux.Clear;
      if FreeAndNilControl then
         FreeAndNil(RTFAux);
   end;
end;

end.
