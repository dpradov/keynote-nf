unit kn_ClipUtils;

interface
uses Windows, Classes, SysUtils, clipbrd;

const
  CF_HTML : word = 0;

function ClipboardHasHTMLformat : boolean;
function ClipboardToStream( Fmt : word; Stm : TStream ) : boolean;
function GetURLFromHTMLClipboard : string;

implementation

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
      ClipboardToStream( CF_HTML, Stm );
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

Initialization

  CF_HTML := RegisterClipboardFormat( 'HTML Format' );

end.
