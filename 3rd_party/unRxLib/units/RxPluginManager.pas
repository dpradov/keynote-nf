{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2000 UIL                        }
{                                                       }
{ Adopted Plug-ins tool by UIL                          }
{*******************************************************}
unit RxPluginManager;

interface

{$I Rx.inc}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, Graphics, Forms, SysUtils, Classes, RxPlugin;

type
  TNewCommandEvent = procedure(Sender: TObject; ACaption, AHint, AData: string;
    AShortCut: TShortCut; ABitmap: TBitmap;
    AEvent: TNotifyEvent) of object;

  TRxBeforeLoadEvent = procedure(Sender: TObject; FileName: string; var AllowLoad: Boolean) of object;
  TRxAfterLoadEvent = procedure(Sender: TObject; FileName: string;
    const ALibHandle: THandle; var AllowLoad: Boolean) of object;
  TRxBeforeCommandsEvent = procedure(Sender: TObject; APlugIn: TRxPlugIn) of object;
  TRxAfterCommandsEvent = procedure(Sender: TObject; APlugIn: TRxPlugIn) of object;
  TRxPlgInErrorEvent = procedure(Sender: TObject; AError: Exception) of object;

  ERxPlugInError = class(Exception);
  ERxLoadPluginError = class(ERxPlugInError);
  ERxExtensionPlugInError = class(ERxPlugInError);
  ERxInitializationPlugInError = class(ERxPlugInError);
  ERxInitializationCustomPlugInError = class(ERxPlugInError);
  ERxCantRegisterPlugInError = class(ERxPlugInError);

  TRxPluginKind = (plgDLL, plgPackage, plgCustom);

  TRxPluginInfo = class(TObject)
  public
    PluginKind: TRxPluginKind;
    Handle: HINST;
    PlugIn: TRxPlugIn;
  end;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxPluginManager = class(TComponent)
  private
    FExtension: string;
    FPluginFolder: string;
    FPluginKind: TRxPluginKind;
    FPluginInfos: TList;
    FOnBeforeLoad: TRxBeforeLoadEvent;
    FOnAfterLoad: TRxAfterLoadEvent;
    FOnNewCommand: TNewCommandEvent;

    FOnBeforeNewCommand: TRxBeforeCommandsEvent;
    FOnAfterNewCommand: TRxAfterCommandsEvent;
    FOnPlugInError: TRxPlgInErrorEvent;
    procedure SetRxPluginKind(const Value: TRxPluginKind);
    procedure UnloadLibrary(Kind: TRxPluginKind; LibHandle: Integer);
  protected
    procedure SetExtension(NewValue: string);
    function GetPlugin(Index: Integer): TRxPlugIn;
    function GetPluginCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPlugin(FileName: string; PlgKind: TRxPluginKind);
    procedure LoadPlugins;
    procedure UnloadPlugin(Index: Integer);
    procedure GetLoadedPlugins(PlugInList: TStrings);
    property Plugins[Index: Integer]: TRxPlugIn read GetPlugin;
    property PluginCount: Integer read GetPluginCount;
    procedure SendMessage(PluginMessage: LongInt; PluginParams: string);
    function AddCustomPlugin(PlugIn: TRxPlugIn): Boolean;
    function FindPlugin(const PluginID: string): TRxPlugIn;
  published
    property PluginFolder: string read FPluginFolder write FPluginFolder;
    property Extension: string read FExtension write SetExtension;
    property PluginKind: TRxPluginKind read FPluginKind write SetRxPluginKind default plgDLL;
    property OnBeforeLoad: TRxBeforeLoadEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnNewCommand: TNewCommandEvent read FOnNewCommand write FOnNewCommand;
    property OnAfterLoad: TRxAfterLoadEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeNewCommand: TRxBeforeCommandsEvent read FOnBeforeNewCommand write FOnBeforeNewCommand;
    property OnAfterNewCommand: TRxAfterCommandsEvent read FOnAfterNewCommand write FOnAfterNewCommand;
    property OnPlugInError: TRxPlgInErrorEvent read FOnPlugInError write FOnPlugInError;
  end;

implementation

const
  C_REGISTER_PLUGIN = 'RegisterPlugin';
  C_Extensions: array[plgDLL..plgCustom] of PChar = ('dll', 'bpl', 'xxx');

resourcestring
  RsEErrEmptyExt = 'Extension must be between 1 and 3 characters.';
  RsEPluginPackageNotFound = 'Plugin package not found: %s.';
  RsERegisterPluginNotFound = 'Register plugin package not found %s.';
  RsERegisterPluginFailed = 'Plugin register failed.';

constructor TRxPluginManager.Create(AOwner: TComponent);
begin
  try
    inherited Create(AOwner);
    FPluginInfos := TList.Create;
    FPluginKind := plgDLL;
    FExtension := C_Extensions[FPluginKind];
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
end;

destructor TRxPluginManager.Destroy;
begin
  while FPluginInfos.Count > 0 do
    UnloadPlugin(0);
  FPluginInfos.Free;
  inherited Destroy;
end;

procedure TRxPluginManager.SetExtension(NewValue: string);
begin
  try
    if FExtension <> NewValue then
    begin
      if Length(NewValue) < 1 then
        raise ERxPlugInError.CreateRes(@RsEErrEmptyExt)
      else
        FExtension := NewValue;
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
end;

procedure TRxPluginManager.SetRxPluginKind(const Value: TRxPluginKind);
begin
  if FPluginKind <> Value then
  begin
    if FExtension = C_Extensions[FPluginKind] then
      FExtension := C_Extensions[Value];
    FPluginKind := Value;
  end;
end;

function TRxPluginManager.GetPluginCount: Integer;
begin
  Result := FPluginInfos.Count;
end;

function TRxPluginManager.GetPlugin(Index: Integer): TRxPlugIn;
var
  PlgI: TRxPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  Result := PlgI.PlugIn;
end;

procedure TRxPluginManager.GetLoadedPlugins(PlugInList: TStrings);
var
  I: Integer;
begin
  PlugInList.BeginUpdate;
  try
    PlugInList.Clear;
    for I := 0 to FPluginInfos.Count - 1 do
      PlugInList.Add(Plugins[I].Name);
  finally
    PlugInList.EndUpdate;
  end;
end;

function TRxPluginManager.AddCustomPlugin(PlugIn: TRxPlugIn): Boolean;
var
  PlgInfo: TRxPluginInfo;
  Counter: Integer;
begin
  Result := False;
  try
    Result := PlugIn.Initialize(Self, Application, 'CustomPlugin');
    if not Result then
      Exit;

    PlgInfo := TRxPluginInfo.Create;
    PlgInfo.PluginKind := plgCustom;
    PlgInfo.PlugIn := PlugIn;

    FPluginInfos.Add(PlgInfo);

    try
      if Assigned(FOnBeforeNewCommand) then
        FOnBeforeNewCommand(Self, PlugIn);

      // Events for all new commands
      if Assigned(FOnNewCommand) then
        for Counter := 0 to PlugIn.Commands.Count - 1 do
          with TRxPlugInCommand(PlugIn.Commands.Items[Counter]) do
            FOnNewCommand(Self, Caption, Hint, Data, ShortCut, Bitmap, OnExecute);
    finally
      if Assigned(FOnAfterNewCommand) then
        FOnAfterNewCommand(Self, PlugIn);
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
end;

// Load a Plugin - either DLL or package

procedure TRxPluginManager.LoadPlugin(FileName: string; PlgKind: TRxPluginKind);
type
  TSxRegisterPlugin = function: TRxPlugIn; stdcall;
var
  Counter: Integer;
  LibHandle: Integer;
  RegisterProc: TSxRegisterPlugin;
  PlugIn: TRxPlugIn;
  NumCopies: Integer;
  PlgInfo: TRxPluginInfo;
  AllowLoad: Boolean;
begin
  LibHandle := 0;
  AllowLoad := True;
  if Assigned(FOnBeforeLoad) then
    FOnBeforeLoad(Self, FileName, AllowLoad);

  if AllowLoad then
  begin
    try
      LibHandle := 0;
      PlugIn := nil;
      if not FileExists(FileName) then
      begin
        if (System.DebugHook = 0) then raise ERxLoadPluginError.CreateResFmt(@RsEPluginPackageNotFound, [FileName]);
        Exit;
      end;
      //LY end
      case PlgKind of
        plgDLL:
          LibHandle := LoadLibrary(PChar(FileName));
        plgPackage:
          LibHandle := LoadPackage(FileName);
      end;

      if LibHandle = 0 then
        raise ERxLoadPluginError.CreateResFmt(@RsEPluginPackageNotFound, [FileName]);

      AllowLoad := True;
      if Assigned(FOnAfterLoad) then
        FOnAfterLoad(Self, FileName, LibHandle, AllowLoad);

      if not AllowLoad then
      begin
        UnloadLibrary(PluginKind, LibHandle);
        Exit;
      end;

      // Load the registration procedure
      RegisterProc := GetProcAddress(LibHandle, C_REGISTER_PLUGIN);
      if not Assigned(RegisterProc) then
        raise ERxLoadPluginError.CreateResFmt(@RsERegisterPluginNotFound, [C_REGISTER_PLUGIN, FileName]);

      // register the plugin
      PlugIn := RegisterProc;
      if PlugIn = nil then
        raise ERxCantRegisterPlugInError.CreateResFmt(@RsERegisterPluginFailed, [C_REGISTER_PLUGIN, FileName]);

      // make sure we don't load more copies of the plugin than allowed
      if PlugIn.InstanceCount > 0 then // 0 = unlimited
      begin
        NumCopies := 0;
        for Counter := 0 to FPluginInfos.Count - 1 do
          if Plugins[Counter].PluginID = PlugIn.PluginID then
            Inc(NumCopies);

        if NumCopies >= PlugIn.InstanceCount then
        begin
          PlugIn.Free;
          Exit; // Todo : Don't know what Skipload does here
        end;
      end; // if Plugin.InstanceCount > 0

      // initialize the plugin and add to list
      if AddCustomPlugin(PlugIn) then
      begin
        PlgInfo := FPluginInfos.Last;
        PlgInfo.PluginKind := PlgKind;
        PlgInfo.Handle := LibHandle;
      end;
    except
      on E: Exception do
      begin
        FreeAndNil(PlugIn);
        if LibHandle <> 0 then
          UnloadLibrary(PlgKind, LibHandle);
        if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end;
    end;
  end;
end;

// Load all plugins in the plugin-folder
// exceptions can only be seen through the OnErrorLoading-Event

procedure TRxPluginManager.LoadPlugins;
var
  FileName: string;
  Found: Integer;
  Path: string;
  Sr: TSearchRec;
begin
  // if the PluginPath is blank, we load from the app's folder.
  if FPluginFolder = '' then
    Path := ExtractFilePath(Application.ExeName)
  else
    Path := FPluginFolder;

  Path := IncludeTrailingPathDelimiter(Path);

  try
    try
      Found := FindFirst(Path + '*.' + FExtension, 0, Sr);
      while Found = 0 do
      begin
        FileName := Sr.Name;
        //! If one plugin made problems -> no other plugins where loaded
        //! To avoid that the try-except block was wrapped around here...
        try
          LoadPlugin(Path + FileName, PluginKind);
        except
        end;
        Found := FindNext(Sr);
      end;
    except
      on E: Exception do
      begin
        if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end;
    end;
  finally
    FindClose(Sr);
  end;
end;

procedure TRxPluginManager.UnloadPlugin(Index: Integer);
var
  PlgI: TRxPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  PlgI.PlugIn.Free;
  UnloadLibrary(PlgI.PluginKind, PlgI.Handle);
  PlgI.Free;
  FPluginInfos.Delete(Index);
end;

procedure TRxPluginManager.SendMessage(PluginMessage: LongInt; PluginParams: string);
var
  I: Integer;
begin
  for I := 0 to FPluginInfos.Count - 1 do
    Plugins[I].SendPluginMessage(PluginMessage, PluginParams);
end;

procedure TRxPluginManager.UnloadLibrary(Kind: TRxPluginKind; LibHandle: Integer);
begin
  case Kind of
    plgDLL:
      FreeLibrary(LibHandle);
    plgPackage:
      UnloadPackage(LibHandle);
  end;
end;

function TRxPluginManager.FindPlugin(const PluginID: string): TRxPlugIn;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to PluginCount - 1 do
  begin
    if Assigned(Plugins[i]) and (AnsiUpperCase(Plugins[i].PluginID) = AnsiUpperCase(PluginID)) then
    begin
      Result := Plugins[i];
      Break;
    end;
  end;
end;

end.