unit kn_LanguagesMng;

interface
uses
 Classes, kn_FileObj;

  function LoadLanguagesAvailable( FN : string ): boolean;
  procedure ClearLanguagesList;
  function ApplyLanguageUI ( LanguageUI : string ): boolean;

type
  TLanguageInfo = class( TObject )
    Name : string;
    LangFile : string;
    TipFile : string;
    Translator : string;
  end;

var
  LanguagesAvailables : TStringList;

implementation
uses
  Windows, Messages, SysUtils,
  Graphics, Controls, Forms, Dialogs,
  uFreeLocalizer,
  gf_misc, kn_info, kn_Const, IniFiles, kn_global;

resourcestring
  STR_01 = 'Internal Language (English) will be established next time you start KeyNote NF';
  STR_02 = 'Language file not found: ';
  STR_03 = 'Tip file not found: ';
  STR_04 = 'Applying Language file  "';

function LoadLanguagesAvailable( FN : string ): boolean;
var
  IniFile : TIniFile;
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
  Info.Name := LANGUAGE_DEFAULT;
  LanguagesAvailables.AddObject( LANGUAGE_DEFAULT, Info );

  if ( FN = '' ) then
      FN := changefileext( application.exename, ext_LAN );
  FN := normalFN( FN );
  if ( not fileexists( FN )) then exit;

  path:= ExtractFilePath( Application.ExeName ) + _LANGUAGE_FOLDER;

  IniFile := TIniFile.Create( fn );
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
begin
  result := false;
  if ( not assigned( LanguagesAvailables )) then exit;

  path:= ExtractFilePath( Application.ExeName ) + _LANGUAGE_FOLDER;


  try
      FN := '';
      TIP_FN := '';

      if (LanguageUI = LANGUAGE_DEFAULT) or (LanguageUI = '') then begin
         if FreeLocalizer.LanguageFile <> '' then
            messagedlg( STR_01, mtInformation, [mbOK], 0 );
         result:= True;
      end
      else begin
          TipFile:= '';
          for i := 0 to LanguagesAvailables.Count -1 do begin
             Info := TLanguageInfo( LanguagesAvailables.Objects[i] );
             if Info.Name = LanguageUI then begin
                FN:= Info.LangFile;
                TipFile:= Info.TipFile;
                break;
             end;
          end;

          if FN <> '' then begin
             if not fileexists( path + FN ) then
                messagedlg( STR_02 + path + FN, mtError, [mbOK], 0 )
             else begin
                 if FreeLocalizer.LanguageFile <> path + FN then begin
                    FreeLocalizer.LanguageFile:= path + FN;
                    DefineConst;
                    AddSearchModes;
                 end;
                 result:= True;
             end;
          end;
          if TipFile <> '' then begin
             if not fileexists( path + TipFile ) then
                messagedlg( STR_03 + path + TipFile, mtError, [mbOK], 0 )
             else
                TIP_FN := path + TipFile;
          end;
      end;

      if TIP_FN = '' then
         TIP_FN := normalFN( changefileext( Application.ExeName, ext_TIP ));   // Default

  except
    on E : Exception do
    begin
      messagedlg( STR_04 + path + FN + '"' + #13#13 + E.Message, mtError, [mbOK], 0 );
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
