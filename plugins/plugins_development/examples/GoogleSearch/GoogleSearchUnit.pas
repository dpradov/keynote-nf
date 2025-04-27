unit GoogleSearchUnit;

interface
uses SysUtils, ShellAPI, Windows,
//   System.Win.Registry,
//   Vcl.Controls, Vcl.Forms,           // Using Screen.Cursor and TRegistry increments DLL up to more than 3MB!
     kn_PluginBase;

// Procedures exported.
// Each plugin MUST export at least these procedures.

function KNTGetPluginName( buf : pointer; size : longint ) : longint stdcall;
// Returns the internal name of the plugin in Buf.
// Size indicates how large the Buf is, so the plugin cannot
// return a string longer than Size.

function KNTGetPluginVersion : longint stdcall;
// Returns version number. Currently, all plugins MUST return "1".
// KeyNote will refuse to run a plugin with a higher version number.

function KNTGetPluginDescription( buf : pointer; size : longint ) : longint stdcall;
// Like KNTGetPluginName, but returns a description string

function KNTConfigurePlugin( OwnerHWND : HWND ) : longint; stdcall;
// OwnerHWND is the handle of KeyNote's main window

function KNTGetPluginFeatures : longint; stdcall;
// This is the most important function besides KNTPluginExecute.
// See the Implementation and "macro-dev.txt" for details

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PAnsiChar;
  InText : PAnsiChar;
  var OutText : pointer ) : longint; stdcall;
// AppHandle is Application.Handle for KeyNote
// OwnerHWND is the handle of KeyNote's main window
// RichEditHWND is the handle of the TRichEdit control in active note
// ActiveFileName is the name of currently loaded .KNT file
// ActiveNoteName is the name of active note
// InText is the text that plugin requested via KNTGetPluginFeatures,
//   supplied in the format the plugin specified
// OutText is a pointer where the plugin will put returned text, if any,
// RETURN VALUE: length of returned text placed on the OutText pointer.
//  On error, return the error code as a negative number.
//  Return 0 to tell KeyNote the plugin executed OK but did not
//  return any text.

function KNTPluginCleanup : longint stdcall;
// This function will ALWAYS be called immediately after
// KNTPluginExecute. If KNTPluginExecute is not called,
// this function will not be called either.

implementation
uses gf_Bits;

var
  OutTextPtr : pointer; // text we return to KeyNote will be placed here
  OutTextSize : longint; // length of text placed on the OutTextPtr pointer

const
  _PLUGIN_NAME    = 'KeyNote GoogleSearch';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'Runs Google search for text or phrase selected in KeyNote';

(*
function GetDefaultBrowserPath(const RemoveArgs : boolean = true): String;
var
  S : string;
  p : integer;
begin
 // https://superuser.com/questions/1111122/path-variable-for-default-browser-in-windows

{
 Example:
 HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.html\UserChoice\ProgID
  =>"FirefoxHTML-308046B0AF4A39CB"

-> HKEY_CLASSES_ROOT\FirefoxHTML-308046B0AF4A39CB
-> HKEY_CLASSES_ROOT\FirefoxHTML-308046B0AF4A39CB\shell\open\command
  => "C:\Program Files\Mozilla Firefox\firefox.exe" -osint -url "%1"

}

  S := '';
  with TRegistry.Create(KEY_READ) do
  try
     RootKey := HKEY_CURRENT_USER;
     if OpenKey( '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.html\UserChoice', False ) then begin
       S := ReadString( 'ProgID' );
       CloseKey;

       if S <> '' Then begin
         RootKey := HKEY_CLASSES_ROOT;
         if OpenKey( '\' + S + '\shell\open\command', False ) then
            S := ReadString( '' )
         else
            S := '';
       end;
    end;

  finally
    Free;
  end;

  if (S <> '') and RemoveArgs then begin
    p := pos( ' "%1"', s );
    if ( p > 0 ) then
       delete( s, p, length( s ) );
  end;

  if ( s <> '' ) then  begin
     if ( s[1] = '"' ) then begin
         delete( s, 1, 1 );
         delete( s, pos( '"', s ), length( s ));
     end;
  end;

  result := s;

end; // GetDefaultBrowserPath
*)

function KNTGetPluginName( buf : pointer; size : longint ) : longint;
begin
  // pass name of the plugin to KeyNote.
  // String must not be longer than 'size'
  StrLCopy( Buf, AnsiString(_PLUGIN_NAME), size-1 );
  result := 0;
end; // KNTGetPluginName

function KNTGetPluginVersion : longint;
begin
  // At present (0.999), ALL PLUGINS MUST RETURN '1'
  result := 1;
end; // KNTGetPluginVersion


function KNTGetPluginDescription( buf : pointer; size : longint ) : longint;
begin
  // pass plugin description to KeyNote.
  // String must not be longer than 'size'
  StrLCopy( Buf, AnsiString(_PLUGIN_DESCRIPTION), size-1 );
  result := 0;
end; // KNTGetPluginDescription


function KNTConfigurePlugin( OwnerHWND : HWND ) : longint;
begin
  result := 0;

  // This is how you can get the handle of KeyNote's
  // main window, e.g. to pass messages to it:
  if ( OwnerHWND = 0 ) then
    OwnerHWND := FindWindow( 'GFKeyNote10', nil );

  // In this plugin, there is no actual configuration.
  // Instead, we just show a dialog box here:
  MessageBox( OwnerHWND,
    PChar( Format(
    '%s %s' + #13 + 'by Daniel Prado Velasco',
    [_PLUGIN_NAME, _PLUGIN_VERSION] )),
    'KeyNote plugin', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);

end; // KNTConfigurePlugin


function KNTGetPluginFeatures : longint;
begin
  result := 0;

  // Here we tell KeyNote what the plugin wants
  // and what it does.

  // The bitwise functions are declared in "gf_bits.pas"

  result := BitOn( result, ord( plOK ));
  // plugin MUST set "plOK", otherwie KeyNote will not run it.
  // You could perform some status checking here, and NOT set "plOK"
  // if the plugin decides that it cannot be executed for some reason,
  // e.g. because a file is missing, etc.

  result := BitOn( result, ord( plGetsData ));
  // tell KeyNote plugin wants to receive text

  result := BitOn( result, ord( plGetsSelection ));
  // tell KeyNote plugin wants selected text
  // (i.e. not all note text)

  result := BitOn( result, ord( plNeedsSelection ));
  { If plGetsSelection is also set, KeyNote will check if any text is selected in active note before running the plugin.
    If no text is selected, KeyNote will display an error message and refuse to execute the plugin.
  }

  //result := BitOn( result, ord( plReturnsData ));
  // tell KeyNote plugin will return text

  //result := BitOn( result, ord( plWantsDlgBox ));
  // tell KeyNote plugin wants the returned text to be displayed in a dialog box

end; // KNTGetPluginFeatures


function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PAnsiChar;
  InText : PAnsiChar;
  var OutText : pointer ) : longint;
var
  URL: string;
  browser : string;
  Parameters: string;
  ShellExecResult: integer;

const
  mrOk = 1;

  {
  function GetHTTPClient : string;
  begin
    result := '';
    if ( not KeyOptions.URLSystemBrowser ) then
       result := NormalFN( KeyOptions.URLAltBrowserPath );
    if ( result = '' ) then
       result := GetDefaultBrowserPath();
  end;
  }

begin
  // Here we perform the actual plugin function.

  OutText := nil;

  if Length(InText) > 100 then begin
     if MessageBox( OwnerHWND,
       PChar('Too long selected text: "' + InText + '"'), _PLUGIN_NAME, MB_YESNOCANCEL + MB_ICONASTERISK+MB_DEFBUTTON3+MB_APPLMODAL) <> mrOK then
       exit;
  end;

  try
     URL:= 'https://www.google.com/search?sourceid=navclient&q=' + InText;
     ShellExecResult := ShellExecute( 0, 'open', PChar(URL), nil, nil, SW_NORMAL );

     {
     browser := GetHTTPClient();

     if browser <> '' then begin
        //Parameters:= '--new-window ' + URL;
        Parameters:= URL;
        Screen.Cursor := crAppStart;
        try
           ShellExecResult := ShellExecute( 0, 'open', PChar(browser), PChar(URL), nil, SW_NORMAL );
        finally
           Screen.Cursor := crDefault;
        end;

     end
     else
       MessageBox( OwnerHWND, PChar('Not found default browser'),'KeyNote GoogleSearch plugin', MB_OK);
     }

  except
    on E: Exception do
      MessageBox( OwnerHWND, PChar(E.Message), _PLUGIN_NAME, MB_OK);
  end;


  // the return value of this function is the length of the text we are passing back to KeyNote
  result := 0;

end;


function KNTPluginCleanup : longint;
begin
  result := 0;
end;

Initialization
  // Initialize these values, so we will know if we need to deallocate memory later.

Finalization
  // Cleanup could also be done here!

end.
