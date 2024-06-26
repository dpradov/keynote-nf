unit kn_ClipUtils;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   Winapi.Windows,
   Winapi.RichEdit,
   Winapi.ShellAPI,
   System.Classes,
   System.StrUtils,
   System.SysUtils,
   Vcl.Dialogs,
   Vcl.Clipbrd ,
   RxRichEd;

var
  CFHtml : Integer; // = 0;
  CFRtf: Integer;
  LastCopiedIDImage:   integer;
  LastCopyFromScratchpad: boolean;


type
   TClipboardHelper = class helper for TClipboard
     private
       function RetryGetClipboardData(const ClipbrdContent: integer): THandle;
     protected
       function GetAsRTF: AnsiString;
       procedure SetAsRTF(const Value: AnsiString);
       function GetAsHTML: AnsiString;
     public
       function TryAsText: string;
       property AsRTF: AnsiString read GetAsRTF write SetAsRTF;
       property AsHTML: AnsiString read GetAsHTML;
       // function GetTitleFromHTML (const HTMLClipboard: AnsiString): string;
       function GetURLFromHTML (const HTMLClipboard: AnsiString): string;
       procedure TrimMetadataFromHTML (var HTMLClipboard: AnsiString);
       function ToStream (Fmt : Word; Stm : TStream ) : boolean;
       function HasRTFformat: boolean;
       function HasHTMLformat: boolean;
       function TryGetFirstLine(const MaxLen : integer): string;
       function TryOfferRTF (const HTMLText: AnsiString=''; TextAttrib: TRxTextAttributes = nil): AnsiString;
       function TryGetAsHandle(Format: Word): THandle;
   end;


  function TestCRCForDuplicates(ClpStr: string; UpdateLastCalculated: boolean = true): boolean;

  procedure LogRTFHandleInClipboard();
  function ClipboardContentWasCopiedByKNT: boolean;
  function GetDropFiles(hDrop: THANDLE): TStringList;

  procedure CopyToClipboard (Editor: TRxRichEdit);
  procedure CutToClipboard (Editor: TRxRichEdit);


implementation

uses
   CRC32,
   gf_strings,
   kn_global,
   kn_ExportImport,
   kn_RTFUtils,
   knt.App
   ;


resourcestring
  STR_28 = 'CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message: ';


type
  TClipboardContent = (
    ccRTF, ccUNICODE, ccHtml
  );


var
   LastCopiedRTFHandle: HGLOBAL;
   //LastCopiedRTFPtr:    Pointer;


procedure LogRTFHandleInClipboard();
begin
   LastCopiedRTFHandle := Clipboard.TryGetAsHandle(CFRtf);
end;

{
procedure LogRTFHandleInClipboard();
var
  RTFHandle: HGLOBAL;
  RTFPtr: Pointer;
  RTFSize: DWORD;
  str: AnsiString;
begin
    Clipboard.Open;
    try
      RTFHandle := Clipboard.GetAsHandle(CFRtf);
      RTFPtr := GlobalLock(RTFHandle);
      RTFSize := GlobalSize(RTFHandle);
      Str := Ansistring(PAnsiChar(RTFPtr));
      LastCopiedRTFHandle :=RTFHandle;
      LastCopiedRTFPtr:= RTFPtr;

    finally
      GlobalUnlock(RTFHandle);
      Clipboard.Close;
    end;
end;
}


function ClipboardContentWasCopiedByKNT: boolean;
var
  RTFHandle: HGLOBAL;
begin
   Result:= False;
   RTFHandle := Clipboard.TryGetAsHandle(CFRtf);
   if LastCopiedRTFHandle = RTFHandle then
      Result:= True
   else
      LastCopiedIDImage:= 0;
end;


procedure CopyToClipboard (Editor: TRxRichEdit);
begin
  _IS_COPYING_TO_CLIPBOARD:= true;
  try
     Editor.CopyToClipboard;
  finally
    _IS_COPYING_TO_CLIPBOARD:= false;
  end;
end;

procedure CutToClipboard (Editor: TRxRichEdit);
begin
  _IS_COPYING_TO_CLIPBOARD:= true;
  try
     Editor.CutToClipboard;
  finally
    _IS_COPYING_TO_CLIPBOARD:= false;
  end;
end;


function GetDropFiles(hDrop: THANDLE): TStringList;
var
  CFileName : array[0..MAX_PATH] of Char;
  FileList : TStringList;
  i, count : integer;
begin
  FileList := TStringList.Create;

  try
    count := DragQueryFile( hDrop, $FFFFFFFF, CFileName, MAX_PATH );

    if ( count > 0 ) then begin
      for i := 0 to count-1 do begin
        DragQueryFile( hDrop, i, CFileName, MAX_PATH );
        FileList.Add( CFileName );
      end;
    end;

    Result:= FileList;

  finally
    DragFinish(hDrop);
  end;

end;

function TClipboardHelper.HasHTMLformat: boolean;
begin
   result := (CFHtml <> 0) and Clipboard.HasFormat( CFHtml );
end;

function TClipboardHelper.HasRTFformat: boolean;
begin
   result := (CFRtf <> 0) and Clipboard.HasFormat( CFRtf );
end;



function TClipboardHelper.RetryGetClipboardData(const ClipbrdContent: integer): THandle;
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
end;  // RetryGetClipboardData



function TClipboardHelper.ToStream( Fmt : Word; Stm : TStream ) : boolean;
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


function TClipboardHelper.GetURLFromHTML (const HTMLClipboard: AnsiString): string;
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

{  At the beginning some browsers included title on the clipboard, but currently it does not.
function TClipboardHelper.GetTitleFromHTML (const HTMLClipboard: AnsiString): string;
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
}

procedure TClipboardHelper.TrimMetadataFromHTML (var HTMLClipboard: AnsiString);
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


{
Calculates CRC on ClpStr and compare it with last calculated CRC, returning true if it is equal
Last calculated CRC (ClipCapCRC32) is then updated (if 'UpdateLastCalculated' = true)
}
function TestCRCForDuplicates(ClpStr: String; UpdateLastCalculated: boolean = true): boolean;
var
   thisClipCRC32: DWORD;
begin
   Result:= false;

   if ( ClpStr <> '' ) then begin
      try
        CalculateCRC32( addr(ClpStr[1]), length(ClpStr) * SizeOf(Char), thisClipCRC32 );
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



procedure TClipboardHelper.SetAsRTF(const Value: AnsiString);
begin
  SetBuffer(CFRtf, PAnsiChar(Value)^, Length(Value) + 1);
end;

function TClipboardHelper.GetAsRTF: AnsiString;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFRtf);
   if Data <> 0 then begin
      Result := PAnsiChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;

function TClipboardHelper.GetAsHTML: AnsiString;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFHtml);
   if Data <> 0 then begin
      Result := PAnsiChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;


function TClipboardHelper.TryGetFirstLine(const MaxLen : integer): string;
begin
    Result:= FirstLineFromString(TrimLeft(TryAsText), MaxLen);
end;

function TClipboardHelper.TryAsText: string;
var
  RetryCount: integer;
begin
  Result:= '';
  if not Clipboard.HasFormat (CF_TEXT) then exit;

  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Result:= Clipboard.AsText;
      break;
    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


function TClipboardHelper.TryGetAsHandle(Format: Word): THandle;
var
  RetryCount: integer;
begin
  Result:= 0;
  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Clipboard.Open;
      try
        Result:= Clipboard.GetAsHandle(Format);
        break;
      finally
        Clipboard.Close;
      end;

    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


{ Try to convert the HTML received (or available in clipboard) to RTF
 If conversion is possible, it will return RTF text, and also it will be available as RTF format in the clipboard }

function TClipboardHelper.TryOfferRTF (const HTMLText: AnsiString=''; TextAttrib: TRxTextAttributes = nil): AnsiString;
var
   HTMLTextToConvert: AnsiString;
   p, len: integer;
begin
   { *1  The RTF available in the clipboard, if any, probably has been converted by us (with the help of TWebBrowser)
         We have been reusing directly this RTF format, but now, since the introduction of changes in ConvertHTMLToRTF to allow
         defining the font and size by default, it is convenient not to do it, because these changes are no reintroduced
         in the clipboard.
         It is not necessary to do the conversion again, but simply apply the changes to set the default font and size.
   }

    Result:= '';
    if _ConvertHTMLClipboardToRTF {and (not Clipboard.HasRTFformat) *1} and Clipboard.HasHTMLformat  then begin

       if Clipboard.HasRTFformat then begin
          Result:= Clipboard.AsRTF;
          SetDefaultFontAndSizeInRTF(Result, TextAttrib)
       end
       else begin
          if HTMLText = '' then
             HTMLTextToConvert:= Clipboard.AsHTML
          else
             HTMLTextToConvert:= HTMLText;

          Clipboard.TrimMetadataFromHTML(HTMLTextToConvert);
          ConvertHTMLToRTF(HTMLTextToConvert, Result);

          len:= Length(Result);
          p:= Pos('\line\par\pard}', Result, len - 16);
          if p = 0 then
             p:= Pos(' \par\pard}', Result, len - 12);
          if p > 0 then begin
             delete(Result, p, len-p+1);
             insert('}', Result, p);
          end;

          SetDefaultFontAndSizeInRTF(Result, TextAttrib);
       end;
    end;
end;



Initialization

  CFHtml := RegisterClipboardFormat('HTML Format');
  CFRtf  := RegisterClipboardFormat(CF_RTF);

end.
