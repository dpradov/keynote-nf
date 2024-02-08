

unit palmexportunit;

interface
uses SysUtils, Controls, Forms, ShellAPI, Windows,
  gf_files, Dialogs, kn_PluginBase, palmexportform,
  DocFile;


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
uses gf_Bits;

const
  _PLUGIN_NAME    = 'Export to Palm';
  _PLUGIN_VERSION = '1.0';
  _PLUGIN_DESCRIPTION = 'Export selected text to PalmOS "doc" format';


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
    [_PLUGIN_NAME, _PLUGIN_VERSION] ) + #13#13 +
    'Based on Delphi code for "DocReader" by Mike Pickering (http://www.alltel.net/~mpicker0/).'
    ),
  'KeyNote plugin', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);

end; // KNTConfigurePlugin

function KNTGetPluginFeatures : longint;
begin
  result := 0;
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

  result := BitOn( result, ord( plNeedsSelection ));

end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint;
var
  Form_PalmExp : TForm_PalmExp;
  ExportFN, SuggestedName, ExportTitle : string;
  Doc: TDocFile;
  p, len : integer;
begin
  result := 0;
  OutText := nil;
  Application.Handle := AppHandle;

  // derive name from first line of text
  // SuggestedName := trim( copy( InText, 1, 127 ));
  SuggestedName := ActiveNoteName;
  len := length( SuggestedName );
  if ( len > 0 ) then
  begin
    for p := 1 to len do
    begin
      if ( SuggestedName[p] < #32 ) then
      begin
        delete( SuggestedName, p, len );
        break;
      end;
    end;
  end;


  Form_PalmExp := TForm_PalmExp.Create( Application );

  try
    try
      Form_PalmExp.SuggestedName := MakeValidFilename( SuggestedName, [], 127 ) + '.pdb';

      if ( Form_PalmExp.ShowModal = mrOK ) then
      begin
        ExportFN := Form_PalmExp.ExportFN;
        ExportTitle := Form_PalmExp.ExportTitle;

        screen.Cursor := crHourGlass;
        try
          Doc := TDocFile.CreateNewDoc( ExportFN, ExportTitle, True );
          Doc.WriteDocument( InText, nil );

          if Form_PalmExp.AutoInstall then
          begin
            ShellExecute( 0, 'open', PChar( ExportFN ), nil, nil, SW_NORMAL ); 
          end;

        finally
          screen.Cursor := crDefault;
        end;

      end;
    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    Application.Handle := 0;
    Form_PalmExp.Free;
  end;

end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;
  // nothing to cleanup here, but this procedure would
  // deallocate any memory that was allocated in
  // KNTPluginExecute
end; // KNTPluginCleanup


end.
