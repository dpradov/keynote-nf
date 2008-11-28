(*
WARNING:
This plugin cannot be used if version 3.0 of the "riched20.dll"
library is installed on your system. With that version of the
richedit library, KeyNote will crash when you drag and drop
text from KeyNote to the scratchpad or vice versa. I have no
idea why. Can someone help?
*)

unit scratchpadunit;

interface
uses SysUtils, ShellAPI, Windows,
  kn_PluginBase, scratchpadform;


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
  _PLUGIN_NAME    = 'Scratchpad';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'Scratchpad for taking quick notes and drag-drop editing';


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
  MyAppHandle := AppHandle;

  Form_RTF := TForm_RTF.Create( nil );

  try
    Form_RTF.KeyNote_HWND := OwnerHWND;
    Form_RTF.Show;
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
end; // KNTSetPluginID

end.
