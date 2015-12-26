unit kn_ClipUtils;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface
uses Windows, Dialogs, Classes, SysUtils, clipbrd;

const
  CFHtml : word = 0;

type
   TClipBoardW= class(TClipboard)
      private
        function GetAsTextW: wideString;
        procedure SetAsTextW(const Value: wideString);
        function GetAsRTF: string;
        procedure SetAsRTF(const Value: string);
        function GetAsHTML: string;
      public
        property AsTextW: wideString read GetAsTextW write SetAsTextW;
        property AsRTF: string read GetAsRTF write SetAsRTF;
        property AsHTML: string read GetAsHTML;
   end;

function Clipboard: TClipBoardW;

function ClipboardAsString : string;
function ClipboardAsStringW : WideString;
function FirstLineFromClipboard( const MaxLen : integer ) : WideString;


function ClipboardHasHTMLformat : boolean;
function ClipboardHasRTFformat : boolean;
function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
function GetURLFromHTMLClipboard (const HTMLClipboard: string): string;
function GetTitleFromHTMLClipboard (const HTMLClipboard: string): string;
procedure TrimMetadataFromHTMLClipboard (var HTMLClipboard: string);
function TestCRCForDuplicates(ClpStr: WideString; UpdateLastCalculated: boolean = true): boolean;

implementation
uses StrUtils, WideStrings, CRC32, gf_strings, RichEdit,
     kn_global;

resourcestring
  STR_28 = 'CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message: ';


var
  CFRtf: Integer;

type
  TClipboardContent = (
    ccRTF, ccUNICODE, ccHtml
  );


function RetryGetClipboardData(const ClipbrdContent: integer): THandle;
var
  RetryCount: integer;
begin
  Result:= 0;
  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Clipboard.Open;
      try
        Result := GetClipboardData(ClipbrdContent);
        RetryCount:= 99;   // Ok, salimos
      finally
        Clipboard.Close;
      end;

    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


function ClipboardHasHTMLformat : boolean;
begin
  result := ( CFHtml <> 0 ) and Clipboard.HasFormat( CFHtml );
end; // ClipboardHasHTMLformat

function ClipboardHasRTFformat : boolean;
begin
  result := ( CFRtf <> 0 ) and Clipboard.HasFormat( CFRtf );
end; // ClipboardHasRTFformat


function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
// code by Peter Below (modified DPV)
var
  hMem: THandle;
  pMem: Pointer;
begin
  result := false;
  hMem := RetryGetClipboardData(fmt);  //[DPV]
  if hMem <> 0 then begin
    pMem := GlobalLock( hMem );
    if pMem <> Nil Then Begin
      try
        Stm.Write( pMem^, GlobalSize( hMem ));
        result := true;
      finally
        GlobalUnlock( hMem );
      end;
    end
  end;
end; // ClipboardToStream


function GetURLFromHTMLClipboard (const HTMLClipboard: string): string;
const
  HTML_FMT_SRCURL  = 'SourceURL:';
  l = length(HTML_FMT_SRCURL);
var
  p, pF: integer;
begin
   Result := '';

   p:= pos( HTML_FMT_SRCURL, HTMLClipboard);
   if p >= 1 then begin
     pF:= p;
     while HTMLClipboard[pF] <> #13 do
         pF := pF + 1;
     Result:= Copy( HTMLClipboard, p+l, pF-p-l);
   end;
   if Result = 'about:blank' then
      Result:= '';
end; // GetURLFromHTMLClipboard


function GetTitleFromHTMLClipboard (const HTMLClipboard: string): string;
const
  TITLE  = '<TITLE>';
  l = length(TITLE);
var
   p, pF: integer;
begin
   Result := '';

   p:= pos( TITLE, HTMLClipboard);
   if p >= 1 then begin
     pF:= PosEx( '</TITLE>', HTMLClipboard, p+l);
     if pF > p  then
        Result:= Copy( HTMLClipboard, p+l, pF-p-l);
   end;
end; // GetURLFromHTMLClipboard


procedure TrimMetadataFromHTMLClipboard (var HTMLClipboard: string);
var
  p : integer;
begin
   p:= pos( '<html>', HTMLClipboard);
   if p <= 0 then p:= pos( '<HTML>', HTMLClipboard);
   if p <= 0 then p:= pos( '<HTML ', HTMLClipboard);
   if p <= 0 then p:= pos( '<html ', HTMLClipboard);

   if p >= 1 then
     delete( HTMLClipboard, 1, p-1);
end;


function ClipboardAsString : string;
begin
  if ( Clipboard.HasFormat( CF_TEXT )) then
    result := Clipboard.AsText
  else
    result := '';
end; // ClipboardAsString

function ClipboardAsStringW : WideString;
begin
  if ( Clipboard.HasFormat( CF_TEXT )) then
    result := Clipboard.AsTextW
  else
    result := '';
end; // ClipboardAsStringW

function FirstLineFromClipboard( const MaxLen : integer ) : WideString;
begin
  Result:= FirstLineFromString(trimleft( ClipboardAsStringW ), MaxLen);
end; // FirstLineFromClipboard


{
Calculates CRC on ClpStr and compare it with last calculated CRC, returning true if it is equal
Last calculated CRC (ClipCapCRC32) is then updated (if 'UpdateLastCalculated' = true)
}
function TestCRCForDuplicates(ClpStr: WideString; UpdateLastCalculated: boolean = true): boolean;
var
   thisClipCRC32: DWORD;
begin
   Result:= false;

   if ( ClpStr <> '' ) then begin
      try
        CalcCRC32( addr( ClpStr[1] ), length( ClpStr ), thisClipCRC32 );
      except
        on E : Exception do begin
          messagedlg( STR_28 + E.Message, mtError, [mbOK], 0 );
          ClipOptions.TestDupClips := false;
          exit;
        end;
      end;
      if ( thisClipCRC32 = ClipCapCRC32 ) then
          Result:= true;

      if UpdateLastCalculated then
         ClipCapCRC32 := thisClipCRC32; // set value for next test
   end;

end;


var
  FClipboardW: TClipboardW;

function Clipboard: TClipboardW;
begin
  if FClipboardW = nil then
     FClipboardW := TClipboardW.Create;
  Result := FClipboardW;
end;


function TClipboardW.GetAsTextW: WideString;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CF_UNICODETEXT);
   if Data <> 0 then begin
      Result := PWideChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;

procedure TClipboardW.SetAsTextW(const Value: wideString);
begin
  SetBuffer(CF_UNICODETEXT, PWideChar(Value)^, 2*Length(Value) + 2);
end;

procedure TClipboardW.SetAsRTF(const Value: string);
begin
  SetBuffer(CFRtf, PChar(Value)^, Length(Value) + 1);
end;

function TClipboardW.GetAsRTF: string;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFRtf);
   if Data <> 0 then begin
      Result := PChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;

function TClipboardW.GetAsHTML: string;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFHtml);
   if Data <> 0 then begin
      Result := PChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;


Initialization

  CFHtml := RegisterClipboardFormat('HTML Format');
  CFRtf  := RegisterClipboardFormat(CF_RTF);
end.
