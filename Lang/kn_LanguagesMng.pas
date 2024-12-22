unit kn_LanguagesMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain)

  Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
  in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   Winapi.Windows,
   Winapi.Messages,
   System.Classes,
   System.SysUtils,
   System.IniFiles,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs
   ;


  function LoadLanguagesAvailable( FN : string ): boolean;
  procedure ClearLanguagesList;
  function ApplyLanguageUI ( LanguageUI : string ): boolean;

type
  TLanguageInfo = class( TObject )
    Name : string;
    LangFile : string;
    TipFile : string;
    Translator : string;
    RTL : boolean;
  end;

var
  LanguagesAvailables : TStringList;


implementation
uses
   KDL.Localizer,
   gf_misc,
   kn_info,
   kn_Const,
   kn_global,
   kn_Main,
   knt.App,
   knt.RS;


function LoadLanguagesAvailable( FN : string ): boolean;
var
  IniFile : TMemIniFile;
  i : integer;
  Info : TLanguageInfo;
  s, section : string;
  sections : TStringList;
  path: string;

begin
  result := false;
  if ( not assigned( LanguagesAvailables )) then exit;

  ClearLanguagesList;
  Info := TLanguageInfo.Create;
  Info.Name := GetRS(LANGUAGE_DEFAULT);
  LanguagesAvailables.AddObject( GetRS(LANGUAGE_DEFAULT), Info );

  path:= ExtractFilePath( Application.ExeName ) + _LANGUAGE_FOLDER;

  if ( FN = '' ) then
      FN:= path + changefileext( ExtractFileName(Application.ExeName), ext_LAN );
  FN := normalFN( FN );
  if ( not fileexists( FN )) then exit;



  IniFile := TMemIniFile.Create( fn, TEncoding.UTF8 );
  sections := TStringList.Create;


  with IniFile do
  try
    try
      ReadSections( sections );
      if ( sections.Count > 0 ) then
      begin
        for i := 0 to pred( sections.count ) do
        begin
          section := sections[i];
          s := normalFN( readstring( section, 'LangFile', '' ));
          if ( not fileexists( path + s )) then continue;

          Info := TLanguageInfo.Create;
          Info.Name := readstring( section, 'Name', '' );
          Info.LangFile := s;
          Info.TipFile := readstring( section, 'TipFile', '' );
          Info.Translator := readstring( section, 'Translator', '' );
          Info.RTL := readbool( section, 'RTL', false);

          LanguagesAvailables.AddObject( s, Info );
        end;
      end;

    except
      on E : Exception do
      begin
        messagedlg( 'Loading Languages file  "' + FN + '"' + #13#13 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    IniFile.Free;
    sections.Free;
  end;

  result := ( LanguagesAvailables.Count > 0 );
end;

function ApplyLanguageUI ( LanguageUI : string ): boolean;
var
  i : integer;
  Info : TLanguageInfo;
  path: string;
  FN : string;
  TipFile: string;
  RTL: boolean;
begin
  result := false;
  if ( not assigned( LanguagesAvailables )) then exit;

  path:= ExtractFilePath( Application.ExeName ) + _LANGUAGE_FOLDER;


  try
      FN := '';
      TIP_FN := '';

      if (LanguageUI = GetRS(LANGUAGE_DEFAULT)) or (LanguageUI = '') then begin
         if FreeLocalizer.LanguageFile <> '' then
            messagedlg( GetRS(sLng01), mtInformation, [mbOK], 0 );
         result:= True;
      end
      else begin
          TipFile:= '';
          for i := 0 to LanguagesAvailables.Count -1 do begin
             Info := TLanguageInfo( LanguagesAvailables.Objects[i] );
             if Info.Name = LanguageUI then begin
                FN:= Info.LangFile;
                TipFile:= Info.TipFile;
                RTL:= Info.RTL;
                break;
             end;
          end;

          App.UI_RTL:= RTL;
          if Form_Main <> nil then
             App.ApplyBiDiMode;

          if FN <> '' then begin
             if not fileexists( path + FN ) then
                messagedlg( GetRS(sLng02) + path + FN, mtError, [mbOK], 0 )
             else begin
                 if FreeLocalizer.LanguageFile <> path + FN then begin
                    FreeLocalizer.LanguageFile:= path + FN;
                    DefineConst;
                    AddSearchModes;
                    AddSearchScopes;
                    AddSearchChkModes;
                    if assigned (Form_Main) then
                       Form_Main.SetupUIHints;
                 end;
                 result:= True;
             end;
          end;
          if TipFile <> '' then begin
             if not fileexists( path + TipFile ) then
                App.DoMessageBox( GetRS(sLng03) + path + TipFile, mtError, [mbOK] )
             else
                TIP_FN := path + TipFile;
          end;
      end;

      if TIP_FN = '' then
         TIP_FN := normalFN( changefileext( Application.ExeName, ext_TIP ));   // Default

  except
    on E : Exception do
    begin
      App.DoMessageBox( GetRS(sLng04) + path + FN + '"' + #13#13 + E.Message, mtError, [mbOK]);
      exit;
    end;
  end;
end;


procedure ClearLanguagesList;
var
  i : integer;
begin
  if assigned( LanguagesAvailables ) then
  begin
    if ( LanguagesAvailables.Count > 0 ) then
    begin
      for i := 0 to pred( LanguagesAvailables.Count ) do
          LanguagesAvailables.Objects[i].Free;
    end;
  LanguagesAvailables.Clear;
  end;
end; // ClearLanguagesList

procedure FreeLanguagesList;
begin
  ClearLanguagesList;
  if assigned( LanguagesAvailables ) then
    LanguagesAvailables.Free;
end;

Initialization
  LanguagesAvailables := TStringList.Create;
  LanguagesAvailables.Sorted := true;
  LanguagesAvailables.Duplicates := dupIgnore;
  LoadLanguagesAvailable('');

  FreeLocalizer.ErrorProcessing := epMessage;
  FreeLocalizer.AutoTranslate := True;


Finalization
  FreeLanguagesList;

end.
