unit kn_RTFUtils;
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

interface

(*
Based on "CR_UORichEdit.pas";
Enhanced TRichEdit support for DelphiDrop by UnitOOPS SOftware.
Based on code examples by Peter Below and Ralph Friedman (TeamB).

Note: Replaced by EM_SETTEXTEX Message
*)

uses Windows, Messages, Controls, Classes,
     ComCtrls, SysUtils, RichEdit, RxRichEd;


procedure PutRichText(
    const aRTFString: wideString;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );

function GetRichText(
    const aRichEdit : TRxRichEdit;
    const asRTF, selectionOnly : boolean
  ) : string;

function URLToRTF( fn : wideString; enTextoURL: boolean ) : wideString;
function URLFromRTF( fn : wideString ) : wideString;

implementation
uses WideStrUtils, TntSystem;


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

//Note: It would be possible to use directly the following message: EM_SETTEXTEX Message
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
      result := aMS.DataString;

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
    const aRTFString: wideString;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );
var
  aSTE: TSetTextEx;
  S: AnsiString;
begin
  if not assigned(aRichEdit) then exit;

  S:= aRTFString;
  if (RichEditVersion < 3) or (S = aRTFstring) then begin
     AnsiPutRichText(S, aRichEdit, isRTF, DoInsert);
     exit;
  end;

  try
    aSTE.flags:= ST_SELECTION or ST_KEEPUNDO;
    aSTE.codepage:= CP_UTF8;  // 1200 (Unicode); CP_UTF8; CP_ACP;   With 1200 didn't work

    // If we're appending, we need to set our selection at the end of the control
    if ( not DoInsert ) then
    begin
      aRichEdit.SelStart := -1;
      aRichEdit.selLength := 0;
    end;

    S:= WideStringToUTF8(aRTFString);
    SendMessage(aRichEdit.Handle, EM_SETTEXTEX, longint(@aSTE), longint(PWideChar(S)));
  finally
  end;
end; // PutRichText

end.
