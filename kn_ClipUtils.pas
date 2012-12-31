unit kn_ClipUtils;

interface
uses Windows, Classes, SysUtils, clipbrd;

const
  CFHtml : word = 0;

type
   TClipBoardW= class(TClipboard)
      private
        function RetryGetClipboardData(ClipbrdContent: integer): THandle;
        function GetAsTextW: wideString;
        procedure SetAsTextW(const Value: wideString);
        function GetAsRTF: string;
        procedure SetAsRTF(const Value: string);
      public
        property AsTextW: wideString read GetAsTextW write SetAsTextW;
        property AsRTF: string read GetAsRTF write SetAsRTF;
   end;

function Clipboard: TClipBoardW;

function ClipboardAsString : string;
function ClipboardAsStringW : WideString;
function FirstLineFromClipboard( const MaxLen : integer ) : WideString;


function ClipboardHasHTMLformat : boolean;
function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
function GetURLFromHTMLClipboard : string;

implementation
uses WideStrings, gf_strings, RichEdit;

var
  CFRtf: Integer;

type
  TClipboardContent = (
    ccRTF, ccUNICODE
  );


function ClipboardHasHTMLformat : boolean;
begin
  result := ( CFHtml <> 0 ) and Clipboard.HasFormat( CFHtml );
end; // ClipboardHasHTMLformat

function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
// code by Peter Below
var
  hMem: THandle;
  pMem: Pointer;
begin
  result := false;
  hMem := Clipboard.GetAsHandle( fmt );
  if hMem <> 0 then
  begin
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

function GetURLFromHTMLClipboard : string;
const
  HTML_FMT_SRCURL  = 'SourceURL:';
  MAX_TEST_LINES = 15;
var
  List : TStringList;
  i : integer;
  Stm : TMemoryStream;
  line : string;
begin

  result := '';
  if ClipboardHasHTMLformat then
  begin
    Stm := TMemoryStream.Create;
    try
      ClipboardToStream( CFHtml, Stm );
      Stm.Position := 0;
      list := TStringList.Create;
      try
        list.LoadFromStream( Stm );
        for i := 0 to list.count-1 do
        begin
          if ( i > MAX_TEST_LINES ) then
            break;
          line := list[i];
          if ( pos( HTML_FMT_SRCURL, line ) = 1 ) then
          begin
            delete( line, 1, length( HTML_FMT_SRCURL ));
            result := line;
            break;
          end;
        end;
      finally
        list.Free;
      end;
    finally
      Stm.Free;
    end;
  end;

end; // GetURLFromHTMLClipboard

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


function TClipboardW.RetryGetClipboardData(ClipbrdContent: integer): THandle;
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
        if Result <> 0 then GlobalUnlock(Result);
        Clipboard.Close;
      end;

    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


function TClipboardW.GetAsTextW: WideString;
var
  Data: THandle;
begin
   Data:= RetryGetClipboardData(CF_UNICODETEXT);
   if Data <> 0 then
      Result := PWideChar(GlobalLock(Data))
   else
      Result:= '';
end;

procedure TClipboardW.SetAsTextW(const Value: wideString);
begin
  SetBuffer(CF_UNICODETEXT, PChar(Value)^, 2*Length(Value) + 2);
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
   if Data <> 0 then
      Result := PChar(GlobalLock(Data))
   else
      Result:= '';
end;


Initialization

  CFHtml := RegisterClipboardFormat( 'HTML Format' );
  CFRtf := RegisterClipboardFormat(CF_RTF);
end.
