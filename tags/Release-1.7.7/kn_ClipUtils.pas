unit kn_ClipUtils;

interface
uses Windows, Classes, SysUtils, clipbrd;

const
  CF_HTML : word = 0;

type
   TClipBoardW= class(TClipboard)
      private
        function GetAsTextW: wideString;
        procedure SetAsTextW(const Value: wideString);
      public
        property AsTextW: wideString read GetAsTextW write SetAsTextW;
   end;

function Clipboard: TClipBoardW;

function ClipboardAsString : string;
function ClipboardAsStringW : WideString;
function FirstLineFromClipboard( const MaxLen : integer ) : WideString;


function ClipboardHasHTMLformat : boolean;
function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
function GetURLFromHTMLClipboard : wideString;

implementation
uses WideStrings;

function ClipboardHasHTMLformat : boolean;
begin
  result := ( CF_HTML <> 0 ) and Clipboard.HasFormat( CF_HTML );
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

function GetURLFromHTMLClipboard : wideString;
const
  HTML_FMT_SRCURL  = 'SourceURL:';
  MAX_TEST_LINES = 15;
var
  List : TWideStringList;
  i : integer;
  Stm : TMemoryStream;
  line : WideString;
begin

  result := '';
  if ClipboardHasHTMLformat then
  begin
    Stm := TMemoryStream.Create;
    try
      ClipboardToStream( CF_HTML, Stm );
      Stm.Position := 0;
      list := TWideStringList.Create;
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
var
  i, l, max : integer;
begin
  result := trimleft( ClipboardAsStringW );
  l := length( result );
  if ( l > 0 ) then
  begin
    if ( MaxLen < l ) then
      max := MaxLen
    else
      max := l;
    for i := 1 to max do
    begin
      if ( result[i] < #32 ) then
      begin
        delete( result, i, l );
        break;
      end;
    end;
  end;
end; // FirstLineFromClipboard

var
  FClipboardW: TClipboardW;

function Clipboard: TClipboardW;
begin
  if FClipboardW = nil then
     FClipboardW := TClipboardW.Create;
  Result := FClipboardW;
end;


function TClipboardW.GetAsTextW: wideString;
var
  Data: THandle;
begin
    try
      Clipboard.Open;
      try
        Data := GetClipboardData(CF_UNICODETEXT);
        if Data <> 0 then begin
          Result := PWideChar(GlobalLock(Data));
        end
        else
          Result := '';
      finally
        if Data <> 0 then GlobalUnlock(Data);
        Clipboard.Close;
      end;
    except
       Result:= '';
    end;
end;

procedure TClipboardW.SetAsTextW(const Value: wideString);
begin
  SetBuffer(CF_UNICODETEXT, PChar(Value)^, 2*Length(Value) + 2);
end;


Initialization

  CF_HTML := RegisterClipboardFormat( 'HTML Format' );

end.
