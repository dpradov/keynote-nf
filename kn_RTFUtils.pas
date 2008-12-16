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
*)

uses Windows, Messages, Controls, Classes,
     ComCtrls, SysUtils, RichEdit, RxRichEd;


procedure PutRichText(
    const aRTFString: string;
    const aRichEdit: TRxRichEdit;
    const isRTF, DoInsert: boolean
  );

function GetRichText(
    const aRichEdit : TRxRichEdit;
    const asRTF, selectionOnly : boolean
  ) : string;

function URLToRTF( fn : string; enTextoURL: boolean ) : string;
function URLFromRTF( fn : string ) : string;

implementation

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

procedure PutRichText(
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
      aRichEdit.Perform(EM_STREAMIN, SF_FLAGS, longint(@aES))

    finally
      // free the stream
      aMS.Free;
    end;
  end; // with
end; // PutRichText

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
function URLToRTF( fn : string; enTextoURL: boolean ) : string;
begin
  if enTextoURL then begin
      result:= StringReplace(fn,'\','\\', [rfReplaceAll]);
      end
  else begin
      result:= StringReplace(fn,'\\',chr(1), [rfReplaceAll]);
      result:= StringReplace(result,'\','\\', [rfReplaceAll]);
      result:= StringReplace(result,chr(1),'\\\\\\\\', [rfReplaceAll]);
  end;
end; // URLToRTF

function URLFromRTF( fn : string ) : string;
begin
  result:= StringReplace(fn,'\\\\',chr(1), [rfReplaceAll]);
  result:= StringReplace(result,'\\','\', [rfReplaceAll]);
  result:= StringReplace(result, chr(1), '\\\\', [rfReplaceAll]);
end; // URLFromRTF


end.
