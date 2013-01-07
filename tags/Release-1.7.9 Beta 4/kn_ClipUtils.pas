unit kn_ClipUtils;

interface
uses Windows, Classes, SysUtils, clipbrd;

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
procedure TrimMetadataFromHTMLClipboard (var HTMLClipboard: string);

implementation
uses WideStrings, gf_strings, RichEdit;

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
end; // ClipboardHasHTMLformat


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
end; // GetURLFromHTMLClipboard


procedure TrimMetadataFromHTMLClipboard (var HTMLClipboard: string);
var
  p : integer;
begin
   p:= pos( '<html>', HTMLClipboard);
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
