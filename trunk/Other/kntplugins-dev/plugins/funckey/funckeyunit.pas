unit funckeyunit;

interface
uses SysUtils, Windows, ShellAPI,
  Forms, Controls, kn_PluginBase,
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
uses gf_Bits, kn_Msgs, funckeyform;

const
  _PLUGIN_NAME    = 'Function key assignment';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'Allows user to customize function keys in KeyNote.';


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
end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint;
var
  Form_FuncKey : TForm_FuncKey;
  CopyData : TCopyDataStruct;
  msg : TKeyNoteMsg;
begin
  result := 0;

  OutText := nil;
  Application.Handle := AppHandle;
  Form_FuncKey := TForm_FuncKey.Create( Application );

  try
    try

      if ( Form_FuncKey.ShowModal = mrOK ) then
      begin
        msg.strData := '';
        msg.intData1 := _CFG_RELOAD_KEYS;
        copydata.dwData := KNT_MSG_RELOADCONFIG; // declared in "kn_Msgs.pas"
        copydata.cbData := sizeof( msg );
        copydata.lpData := @msg;

        SendMessage( OwnerHWND,  
          WM_COPYDATA,
          Form_FuncKey.Handle,
          integer( @copydata )
        );

      end;


    except
      result := -1;
    end;
  finally
    Form_FuncKey.Free;
    Application.Handle := 0;
  end;

end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;

end; // KNTPluginCleanup


end.
