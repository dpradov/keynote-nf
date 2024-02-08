unit plugintestunit;

interface
uses SysUtils, ShellAPI, Windows, kn_PluginBase;

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
  _PLUGIN_NAME    = 'KeyNote Test Plugin';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'This is a sample plugin for KeyNote. It doesn''t do anything very useful yet, but it could!';


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

  // In this example plugin, there is no actual configuration.
  // Instead, we just show a dialog box here:
  MessageBox( OwnerHWND,
    PChar( Format(
    '%s %s' + #13 + 'by General Frenetics',
    [_PLUGIN_NAME, _PLUGIN_VERSION] )),
  'KeyNote plugin test', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);

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
  // if thep lugin decides that it cannot be executed for some reason,
  // e.g. because a file is missing, etc.

  result := BitOn( result, ord( plGetsData ));
  // tell KeyNote plugin wants to receive text

  result := BitOn( result, ord( plGetsSelection ));
  // tell KeyNote plugin wants selected text
  // (i.e. not all note text)

  result := BitOn( result, ord( plReturnsData ));
  // tell KeyNote plugin will return text

  result := BitOn( result, ord( plWantsDlgBox ));
  // tell KeyNote plugin wants the returned text
  // to be displayed in a dialog box

end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PAnsiChar;
  InText : PAnsiChar;
  var OutText : pointer ) : longint;
var
  s : AnsiString;
  i : longint;
  total, vowels, consonants : longint;
begin
  result := 0;
  vowels := 0;
  consonants := 0;
  total := 0;

  OutText := nil;

  // Here we perform the actual plugin function.
  // In this case, we just do a lame vowel/consonant count.
  if ( InText <> nil ) then
  begin
    s := InText;
    total := length( s );

    for i := 1 to total do
      if ( s[i] in ['a','e','i','o','u','y'] ) then
        inc( vowels )
      else
      if ( s[i] in ['b', 'c', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z'] ) then
        inc( consonants );

  end;

  // format the text that will be returned to keynote
  s := 'This text was generated by "plugintest.dll", a test plugin for KeyNote.' + #13 +
      format( 'Text length: %d  Vowels: %d  Consonants: %d  Other: %d',
        [total,vowels,consonants,(total-(vowels+consonants))] );

  // Allocate memory for the return text
  OutTextSize := length( s );
  GetMem( OutText, OutTextSize );
  move( s[1], OutText^, OutTextSize );
  OutTextPtr := OutText;

  // the return value of this function is the length
  // of the text we are passing back to KeyNote
  result := OutTextSize;

end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;
  // deallocate the memory that was allocated in KNTPluginExecute.
  // This could also be done in Finalization instead.
  try
    if (( OutTextPtr <> nil ) and ( OutTextSize > 0 )) then
      FreeMem( OutTextPtr, OutTextSize );
  except
    // forgive and forget, this is only a simple test
  end;
end; // KNTPluginCleanup

Initialization
  // Initialize these values, so we will know
  // if we need to deallocate memory later.
  OutTextPtr := nil;
  OutTextSize := 0;

Finalization
  // Cleanup could also be done here!

end.
