unit kn_Plugins;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface
uses
   Winapi.Windows,
   Winapi.ShellAPI,
   System.SysUtils,
   System.IniFiles,
   System.Classes,
   Vcl.Dialogs,
   gf_Bits,
   kn_PluginBase;


type
  TPlugin = class(TObject)
    FileName : string;
    Name : string;
    Version : integer;
    Info : string;
    Features : TPluginFeatures;
  end;

type
  TLoadedPlugin = class(TObject)
    DllInstance : THandle;
    ID : longint;
    ExecResult : longint;
  end;

function GetPluginInfo(
  const FN : string;
  var aName : string;
  var aVersion : integer;
  var aInfo : string;
  var aFeatures : TPluginFeatures ) : boolean;

function ExecutePluginConfig( const FN : string; OwnerHandle : HWND ) : boolean;
function GetPluginFeatures( const FN : string ) : TPluginFeatures;
procedure ClearPluginList;

var
  Plugin_Folder : string;
  Plugin_List : TStringList; // holds names of available plugins
  Loaded_Plugins : TStringList; // holds handles of loaded, resident plugins

implementation

resourcestring
  STR_01 = 'Unexpected error from DLL: ';


function GetPluginInfo(
  const FN : string;
  var aName : string;
  var aVersion : integer;
  var aInfo : string;
  var aFeatures : TPluginFeatures ) : boolean;

const
  BUF_Size = 1024;
var
  KNTGetPluginName : KNTGetPluginNameProc;
  KNTGetPluginVersion : KNTGetPluginVersionProc;
  KNTGetPluginDescription : KNTGetPluginDescriptionProc;
  KNTGetPluginFeatures : KNTGetPluginFeaturesProc;
  hDLLInst : THandle;
  buf : array[0..BUF_Size] of AnsiChar;     // For compatibility with existing plugins, but should change to Char
  size, features : integer;
  pl : TPluginFeature;
begin

  result := false;
  aName := '';
  aVersion := 0;
  aInfo := '';
  size := BUF_Size;
  aFeatures := [];
  features := 0;

  hDLLInst := LoadLibrary( PChar( FN ));
  if ( hDLLInst <= 0 ) then
    exit;

  try
    try
      @KNTGetPluginName := GetProcAddress( hDLLInst, 'KNTGetPluginName' );
      if assigned( KNTGetPluginName ) then
      begin
        KNTGetPluginName( @buf, size );
        aName := buf;

        @KNTGetPluginVersion := GetProcAddress( hDLLInst, 'KNTGetPluginVersion' );
        if assigned( KNTGetPluginVersion ) then begin
          aVersion := KNTGetPluginVersion;
          result := true;

          @KNTGetPluginDescription := GetProcAddress( hDLLInst, 'KNTGetPluginDescription' );
          if assigned( KNTGetPluginDescription ) then begin
            KNTGetPluginDescription( @buf, size );
            aInfo := buf;
          end;
        end;

        @KNTGetPluginFeatures := GetProcAddress( hDLLInst, 'KNTGetPluginFeatures' );
        if assigned( KNTGetPluginFeatures ) then begin
          Features := KNTGetPluginFeatures;
          for pl := low( TPluginFeature ) to high( TPluginFeature ) do
          begin
            if IsBitSet( Features, ord( pl )) then
              include( aFeatures, pl );
          end;
        end;

      end;

    except
      On E : Exception do begin
        messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;

  finally
    FreeLibrary( hDLLInst );
  end;

end; // GetPluginInfo


function ExecutePluginConfig( const FN : string; OwnerHandle : HWND ) : boolean;
var
  KNTConfigurePlugin : KNTConfigurePluginProc;
  hDLLInst : THandle;
begin
  result := false;

  hDLLInst := LoadLibrary( PChar( FN ));
  if ( hDLLInst <= 0 ) then
    exit;

  try
    try
      @KNTConfigurePlugin := GetProcAddress( hDLLInst, 'KNTConfigurePlugin' );
      if assigned( KNTConfigurePlugin ) then begin
        KNTConfigurePlugin( OwnerHandle );
        result := true;
      end;

    except
      On E : Exception do  begin
        messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;

  finally
    FreeLibrary( hDLLInst );
  end;
end; // ExecutePluginConfig


function GetPluginFeatures( const FN : string ) : TPluginFeatures;
var
  KNTGetPluginFeatures : KNTGetPluginFeaturesProc;
  hDLLInst : THandle;
  Features : integer;
  pl : TPluginFeature;
begin
  result := [];
  Features := 0;

  hDLLInst := LoadLibrary( PChar( FN ));
  if ( hDLLInst <= 0 ) then
    exit;

  try
    try
      @KNTGetPluginFeatures := GetProcAddress( hDLLInst, 'KNTGetPluginFeatures' );
      if assigned( KNTGetPluginFeatures ) then
         Features := KNTGetPluginFeatures;

    except
      On E : Exception do begin
        messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;

  finally
    FreeLibrary( hDLLInst );
  end;

  for pl := low( TPluginFeature ) to high( TPluginFeature ) do
  begin
    if IsBitSet( Features, ord( pl )) then
       include( result, pl );
  end;

end; // GetPluginFeatures

procedure ClearLoadedPlugins;
var
  i : integer;
  LoadedPlugin : TLoadedPlugin;
begin
  if ( not assigned( Loaded_Plugins )) then exit;

  for i := 1 to Loaded_Plugins.Count do
  begin
    LoadedPlugin := TLoadedPlugin( Loaded_Plugins.Objects[pred( i )] );
    Loaded_Plugins.Objects[pred( i )].Free;
  end;
end; // ClearLoadedPlugins


procedure ClearPluginList;
var
  i : integer;
begin
  try
    try
      for i := 1 to Plugin_List.Count do
      begin
        TPlugin( Plugin_List.Objects[pred( i )] ).Free;
      end;
    except
    end;
  finally
    Plugin_List.Clear;
  end;
end; // ClearPluginList

Initialization

  Plugin_List := TStringList.Create;
  with Plugin_List do
  begin
    sorted := true;
    duplicates := dupIgnore;
  end;

  Loaded_Plugins := TStringList.Create;
  Loaded_Plugins.Sorted := true;
  Loaded_Plugins.Duplicates := dupError;

Finalization
  try
    ClearPluginList;
    ClearLoadedPlugins;
    Plugin_List.Free;
    if assigned( Loaded_Plugins ) then
      Loaded_Plugins.Free;
  except
  end;

end.
