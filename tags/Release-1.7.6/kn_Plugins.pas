
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

unit kn_Plugins;

interface
uses Windows, SysUtils, ShellAPI, IniFiles,
  dialogs, Classes, gf_Bits, kn_PluginBase;


type
  TPlugin = class( TObject )
    FileName : string;
    Name : string;
    Version : integer;
    Info : string;
    Features : TPluginFeatures;
  end;

type
  TLoadedPlugin = class( TObject )
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
  buf : array[0..BUF_Size] of char;
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
        if assigned( KNTGetPluginVersion ) then
        begin
          aVersion := KNTGetPluginVersion;
          result := true;

          @KNTGetPluginDescription := GetProcAddress( hDLLInst, 'KNTGetPluginDescription' );
          if assigned( KNTGetPluginDescription ) then
          begin
            KNTGetPluginDescription( @buf, size );
            aInfo := buf;
          end;
        end;

        @KNTGetPluginFeatures := GetProcAddress( hDLLInst, 'KNTGetPluginFeatures' );
        if assigned( KNTGetPluginFeatures ) then
        begin
          Features := KNTGetPluginFeatures;
          for pl := low( TPluginFeature ) to high( TPluginFeature ) do
          begin
            if IsBitSet( Features, ord( pl )) then
              include( aFeatures, pl );
          end;
        end;

      end;

    except
      On E : Exception do
      begin
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
      if assigned( KNTConfigurePlugin ) then
      begin
        KNTConfigurePlugin( OwnerHandle );
        result := true;
      end;
    except
      On E : Exception do
      begin
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
      begin
        Features := KNTGetPluginFeatures;
      end;
    except
      On E : Exception do
      begin
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
