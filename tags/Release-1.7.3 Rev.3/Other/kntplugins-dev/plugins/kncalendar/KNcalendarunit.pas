unit KNcalendarunit;

interface
uses SysUtils, Forms, Windows, ShellAPI,
  Controls, kn_PluginBase,
  Clipbrd, Messages;

function KNTGetPluginName( buf : pointer; size : longint ) : longint stdcall;
function KNTGetPluginVersion : longint stdcall;
function KNTGetPluginDescription( buf : pointer; size : longint ) : longint stdcall;
function KNTConfigurePlugin( OwnerHWND : HWND ) : longint; stdcall;
function KNTGetPluginFeatures : longint; stdcall;
function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint; stdcall;
function KNTPluginCleanup : longint; stdcall;

implementation
uses gf_Bits, KNCalendarform;

var
  OutTextPtr : pointer;
  OutTextSize : longint;

const
  _PLUGIN_NAME    = 'Date selector';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'Calendar - date selector plugin for KeyNote.';


function KNTGetPluginName( buf : pointer; size : longint ) : longint;
begin
  StrLCopy( Buf, _PLUGIN_NAME, size-1 );
  result := 0;
end; // KNTGetPluginName

function KNTGetPluginVersion : longint;
begin
  result := 1;
end; // KNTGetPluginVersion

function KNTGetPluginDescription( buf : pointer; size : longint ) : longint;
begin
  StrLCopy( Buf, _PLUGIN_DESCRIPTION, size-1 );
  result := 0;
end; // KNTGetPluginDescription

function KNTConfigurePlugin( OwnerHWND : HWND ) : longint;
begin
  result := 0;
  if ( OwnerHWND = 0 ) then
    OwnerHWND := FindWindow( 'GFKeyNote10', nil );
  MessageBox( OwnerHWND,
    PChar( Format(
    '%s %s' + #13 + 'by General Frenetics',
    [_PLUGIN_NAME, _PLUGIN_VERSION] )),
  'KeyNote plugin', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);

end; // KNTConfigurePlugin

function KNTGetPluginFeatures : longint;
begin
  result := 0;
  result := BitOn( result, ord ( plOK ));
  result := BitOn( result, ord( plReturnsData ));

end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint;
var
  s : string;
  Form_Cal: TForm_Cal;
begin
  result := 0;
  s := '';

  OutText := nil;
  Application.Handle := AppHandle;

  Form_Cal := TForm_Cal.Create( Application );


  try
    try

      if ( Form_Cal.ShowModal = mrOK ) then
      begin

        if Form_Cal.CB_LongDate.Checked then
          s := FormatDateTime( LongDateFormat, Form_Cal.DTP.Date )
        else
          s := FormatDateTime( ShortDateFormat, Form_Cal.DTP.Date );

        OutTextSize := length( s );
        result := OutTextSize;

        GetMem( OutText, OutTextSize );
        move( s[1], OutText^, OutTextSize );
        OutTextPtr := OutText;
      end; 

    except
      result := -1;
    end;
  finally
    Form_Cal.Free;
    Application.Handle := 0;
  end;

end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;

  // deallocate any memory that was allocated in
  // KNTPluginExecute
  try
    if (( OutTextPtr <> nil ) and ( OutTextSize > 0 )) then
      FreeMem( OutTextPtr, OutTextSize );
  except
    // forgive and forget
  end;
end; // KNTPluginCleanup


Initialization
  OutTextPtr := nil;
  OutTextSize := 0;

Finalization

end.
