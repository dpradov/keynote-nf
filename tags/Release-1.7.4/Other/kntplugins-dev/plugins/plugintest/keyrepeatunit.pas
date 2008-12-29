unit keyrepeatunit;

interface
uses SysUtils, ShellAPI, Windows, kn_PluginBase, keyrepeatform;


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
procedure KNTSetPluginID( ID : longint ); stdcall;

implementation
uses gf_Bits;


const
  _PLUGIN_NAME    = 'Key autorepeater';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'This plugin repeatedly sends keyboard shortcuts to KeyNote. Use it to insert a character a specified number of times, or to implement an auto-scroll function.';


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
  result := BitOn( result, ord( plOK ));
  result := BitOn( result, ord( plStaysResident ));
end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint;
begin
  result := 0;
  OutText := nil;

  Form_Key := TForm_Key.Create( nil );

  try
    Form_Key.KeyNote_HWND := OwnerHWND;
    Form_Key.Show;
  finally
    // We CANNOT free the form here
  end;



end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;
  // nothing to cleanup here, but this procedure would
  // deallocate any memory that was allocated in
  // KNTPluginExecute
end; // KNTPluginCleanup

procedure KNTSetPluginID( ID : longint );
begin
  ResidentPluginID := ID;
end;

end.
