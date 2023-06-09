{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2000 UIL                        }
{                                                       }
{ Adopted Plug-ins tool by UIL                          }
{*******************************************************}
unit RxPluginWizard;

interface

uses
  Windows, ToolsAPI;

type
  TRxPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
      IOTAMenuWizard, IOTAProjectWizard)
  public
    PluginMainMenu: IOTAComponent;

    { IOTAWizard Methods }
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual;

    { IOTARepositoryWizard Methods }
    function GetAuthor: string; virtual;
    function GetComment: string; virtual;
    function GetPage: string; virtual;
    function GetGlyph: Cardinal; virtual;

    { IOTAMenuWizard methods }
    function GetMenuText: string; virtual;
  end;

  TRxPluginProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator) // both interfaces needed !!!!
  public
    Wizard: TRxPluginWizard;
    { 0 = dll; 1 = dpk }
    PlugType: Integer;
    { Private variables which will be used to store some properties for
      the TRxPlugin }
    PlugName: string;
    PlugDesc: string;
    PlugAuth: string;
    PlugCopy: string;
    PlugUID: string;
    Project: IOTAModule;
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  end;

  TRxPluginModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    Wizard: TRxPluginWizard;
    { 0 = dll; 1 = dpk }
    PlugType: Integer;
    Project: IOTAModule;
    { Private variables which will be used to store some properties for
      the TRxPlugin }
    PlugName: string;
    PlugDesc: string;
    PlugAuth: string;
    PlugCopy: string;
    PlugUID: string;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TRxOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const Source: string);
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses
  Controls, SysUtils, Dialogs, Classes, ActnList, Menus, DesignIntf, DesignEditors,
  RxPlugin, RxPluginParamsForm;

{$R RxPluginWizard.res}

resourcestring
  RsRxPluginWizard = 'RX Plugin Wizard';
  RsProjects = 'Projects';
  RsNewPlugin = 'New Plugin';
  RsPrivateDeclarations = '{ Private declarations }';
  RsPublicDeclarations = '{ Public declarations }';
  RsJediPuginWizard = 'RX Plugin Wizard';
  RsPluginWizardIDString = 'RX.JvPluginWizard';

const
  CrLf = sLineBreak;
  CrLf2 = CrLf + CrLf;
  cPlgPrefix = 'Plg';
  cPluginPrefix = 'Plugin';

{ TRxPluginWizard }

function TRxPluginWizard.GetIDString: string;
begin
  Result := RsPluginWizardIDString;
end;

function TRxPluginWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TRxPluginWizard.GetMenuText: string;
begin
  Result := RsJediPuginWizard;
end;

function TRxPluginWizard.GetName: string;
begin
  Result := RsRxPluginWizard;
end;

function TRxPluginWizard.GetPage: string;
begin
  Result := RsProjects;
end;

function TRxPluginWizard.GetAuthor: string;
begin
  Result := 'UIL/Nadra';
end;

function TRxPluginWizard.GetComment: string;
begin
  Result := RsNewPlugin;
end;

function TRxPluginWizard.GetGlyph: Cardinal;
begin
  Result := LoadIcon(HInstance, 'XRXPLUGINWIZ');
end;

procedure TRxPluginWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectCreator: TRxPluginProjectCreator;
begin
  with TfrmPluginParams.Create(nil) do
  try
    if ShowModal = mrOk then
    begin
      if Assigned(BorlandIDEServices) and
        (BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) = S_OK) then
      begin
        ProjectCreator := TRxPluginProjectCreator.Create;
        ProjectCreator.Wizard := Self;

          { rbDll checked     => dll     => PlugType = 0 = Ord(False)
            rbPackage checked => package => PlugType = 1 = Ord(True)
          }
        ProjectCreator.PlugType := Ord(rbPackage.Checked); //  radPluginType.ItemIndex;
        ProjectCreator.PlugName := Trim(edtPluginName.Text);

        ProjectCreator.PlugAuth := 'Nadra Bank'; //Trim(edtPluginAuthor.Text);
        ProjectCreator.PlugCopy := 'Nadra Bank'; //Trim(edtPluginCopyright.Text);
        ProjectCreator.PlugDesc := Trim(mmoDescripton.Text);
        ProjectCreator.PlugUID := Trim(edtPluginUID.Text);

        ModuleServices.CreateModule(ProjectCreator);
      end;
    end;
  finally
    Free;
  end;
end;

{ TRxPluginProjectCreator }

// left empty this makes problems !!

function TRxPluginProjectCreator.GetFileName: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    Result := GetCurrentDir + '\' + cPlgPrefix + PlugName + '.dpr'
  else
    Result := GetCurrentDir + '\' + cPlgPrefix + PlugName + '.dpk';
end;

function TRxPluginProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TRxPluginProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

procedure TRxPluginProjectCreator.NewDefaultModule;
var
  Module: IOTAModule;
  ModuleCreator: TRxPluginModuleCreator;
begin
  ModuleCreator := TRxPluginModuleCreator.Create;
  ModuleCreator.Wizard := Wizard;
  ModuleCreator.PlugType := PlugType;
  ModuleCreator.PlugName := PlugName;
  ModuleCreator.PlugAuth := PlugAuth;
  ModuleCreator.PlugDesc := PlugDesc;
  ModuleCreator.PlugCopy := PlugCopy;
  ModuleCreator.PlugUID := PlugUID;
  ModuleCreator.Project := Project;
  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

function TRxPluginProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TRxPluginProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TRxPluginProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
var
  S: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    S := 'library ' + ProjectName + ';' + CrLf +
      CrLf +
      'uses' + CrLf +
      {$IFDEF RX_D10}
      '  SimpleShareMem,'
      {$ELSE}
      '  ShareMem,' //must be use with concerting BorlndMM.dll
      {$ENDIF}
      + cPluginPrefix + PlugName + ';' + CrLf +
      CrLf +
      '{$R *.res}' + CrLf +
      CrLf +
      'exports' + CrLf +
      '  RegisterPlugin;' + CrLf +
      CrLf +
      'begin' + CrLf +
      'end.'
  else // Package-Library
    S := 'package ' + ProjectName + ';' + CrLf2 +

    '{$DESCRIPTION ''RX Plugin Package''}' + CrLf +
      '{$RUNONLY}' + CrLf +
      '{$IMPLICITBUILD ON}' + CrLf2 +

    'requires' + CrLf +
      '  vcl;' + CrLf +
      'end.';

  Result := TRxOTAFile.Create(S);
end;

function TRxPluginProjectCreator.GetCreatorType: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    Result := sLibrary
  else
    Result := sPackage;
end;

function TRxPluginProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRxPluginProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function GetCurrentProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  I: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[I];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

function TRxPluginProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProjectGroup; // nil
end;

function TRxPluginProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

 { TRxOTAFile }

constructor TRxOTAFile.Create(const Source: string);
begin
  inherited Create;
  FSource := Source;
end;

function TRxOTAFile.GetAge: TDateTime;
begin
  Result := -1; // new
end;

function TRxOTAFile.GetSource: string;
begin
  Result := FSource;
end;

procedure RegisterContainerModule;
begin
  RegisterCustomModule(TRxPlugIn, TCustomModule);
end;

 { TRxPluginModuleCreator }

procedure TRxPluginModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  with TRxPlugIn(INTAComponent(FormEditor.GetRootComponent).GetComponent) do
  begin
    Author := PlugAuth;
    Description := PlugDesc;
    Copyright := PlugCopy;
    PluginID := PlugUID;
  end;
end;

function TRxPluginModuleCreator.GetAncestorName: string;
begin
  Result := 'RxPlugin';
end;

function TRxPluginModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TRxPluginModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TRxPluginModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TRxPluginModuleCreator.GetFormName: string;
begin
  Result := cPluginPrefix + PlugName;
end;

function TRxPluginModuleCreator.GetImplFileName: string;
begin
  Result := GetCurrentDir + '\' + cPluginPrefix + PlugName + '.pas'
end;

function TRxPluginModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TRxPluginModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TRxPluginModuleCreator.GetOwner: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  NewModule: IOTAModule;
begin
  // You may prefer to return the project group's ActiveProject instead
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Module := ModuleServices.CurrentModule;

  if Module <> nil then
  begin
    if Module.QueryInterface(IOTAProject, NewModule) = S_OK then
      Result := NewModule
    else if Module.OwnerModuleCount > 0 then
    begin
      NewModule := Module.OwnerModules[0];
      if NewModule <> nil then
        if NewModule.QueryInterface(IOTAProject, Result) <> S_OK then
          Result := nil;
    end;
  end;
end;

function TRxPluginModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TRxPluginModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TRxPluginModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TRxPluginModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TRxPluginModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
  TypeName: string;
  Ancestor: string;
  Source: string;
  ClassNameOfPlugin: string;
begin
  ClassNameOfPlugin := 'T' + FormIdent;

  TypeName := FormIdent;
  Ancestor := AncestorIdent;

  TypeName := PlugName;

  Source :=
    'unit ' + ModuleIdent + ';' + CrLf2 +

  'interface' + CrLf2 +

    // (rom) fixed missing "," after "Controls"
  'uses' + CrLf +
    '  Windows, Messages, SysUtils, Classes, Dialogs, Forms, Controls,' + CrLf +
    '  RxPlugin;' + CrLf2 +

  'type' + CrLf +
    '  ' + ClassNameOfPlugin + ' = class(T' + Ancestor + ')' + CrLf +
    //    '  T' + TypeName + ' = class(T' + Ancestor + ')' + CrLf +
  '  private' + CrLf +
    '    ' + RsPrivateDeclarations + CrLf +
    '  public' + CrLf +
    '    ' + RsPublicDeclarations + CrLf +
    '  end;' + CrLf2 +

    //'function RegisterPlugin: TRxPlugin; stdcall;' + CrLf2;
  'function RegisterPlugin: ' + ClassNameOfPlugin + '; stdcall;' + CrLf2;

  { 0 = dll; 1 = dpk }
  if PlugType <> 0 then
    Source := Source + 'exports RegisterPlugin;' + CrLf2;

  Source := Source +
    'implementation' + CrLf2 +

  '{$R *.dfm}' + CrLf2 +

  '// IMPORTANT NOTE: If you change the name of the Plugin container,' + CrLf +
    '// you must set the type below to the same type. (Delphi changes' + CrLf +
    '// the declaration, but not the procedure itself. Both the return' + CrLf +
    '// type and the type created must be the same as the declared type above.' + CrLf +
  //'function RegisterPlugin: TRxPlugin;' + CrLf +
  'function RegisterPlugin: ' + ClassNameOfPlugin + ';' + CrLf +
    'begin' + CrLf +
    '  Result := ' + ClassNameOfPlugin + '.Create(nil);' + CrLf +
    'end;' + CrLf2 +

  'end.';

  Result := TRxOTAFile.Create(Source);
end;

function TRxPluginModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

initialization
  RegisterContainerModule;

end.
