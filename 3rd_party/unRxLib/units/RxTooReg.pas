{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and component added by JB.                   }
{*******************************************************}

{ Note:
  - in Delphi 4.0 you must add DCLSTD40 to the requires page of the
    package you install this components into.
  - in Delphi 3.0 you must add DCLSTD30 to the requires page of the
    package you install this components into.
  - in C++Builder 3.0 you must add DCLSTD35 to the requires page of the
    package you install this components into. }

unit RxTooReg;

{$I RX.INC}
{$D-,L-,S-}

interface

procedure Register;

implementation

{$R *.dcr}

uses
  Classes, SysUtils, Controls, Graphics, TypInfo, Consts, Dialogs, ExtCtrls,
  RxPictEdit, RxHook, RxPicClip, RxPlacemnt, RxPresrDsn, RxMinMaxEd, RxDualList,
  RxClipView, RxSpeedbar, RxSbEdit, RxDataConv, RXCalc, RxPageMngr, RxPgMngrEd, RxMrgMngr,
  RxStrHlder, RXShell, RxAppEvent, RxVCLUtils, RxTimerLst, RxTimLstEd, RxIcoList, RxIcoLEdit,
  {$IFDEF RX_D6}RxPlugin, RxPluginManager, RxPluginWizard, RxPluginParamsForm, {$ENDIF}
  {$IFDEF USE_RX_GIF}RxGIF, RxGIFCtrl, {$ENDIF}RxResConst, RXCtrls, RxRichPopup, RxCalcEditDlg,
  {$IFDEF RX_D3}RxResExp, {$ENDIF}RxMenus, RxMRUList, RxMailBox, RxTranslate, RxNTSecurity,
  {$IFNDEF VER80}RxNotify, RxGrdCpt, RxGradEdit, {$ENDIF}RxHintProp, ToolsAPI, RxViewer,
  {$IFDEF RX_D6}RTLConsts, DesignIntf, DesignEditors, VCLEditors{$ELSE}DsgnIntf{$ENDIF}; // Polaris

{ TStringsEditor }

type
  TStringsEditor = class(TDefaultEditor)
  public
    {$IFDEF RX_D6} // Polaris
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
  end;

  {$IFDEF RX_D6} // Polaris

procedure TStringsEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}

procedure TStringsEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'STRINGS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

{ TComponentFormProperty }

type
  TComponentFormProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure TComponentFormProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  {$IFDEF RX_D6} // Polaris
  if (Designer.Root is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Root.Name <> '') then Proc(Designer.Root.Name);
  {$ELSE}
  if (Designer.Form is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Form.Name <> '') then Proc(Designer.Form.Name);
  {$ENDIF}
end;

procedure TComponentFormProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  {$IFNDEF VER80}
  Component := Designer.GetComponent(Value);
  {$ELSE}
  Component := Designer.Form.FindComponent(Value);
  {$ENDIF}
  {$IFDEF RX_D6} // Polaris
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Root.Name, Value) = 0) then
  begin
    if not (Designer.Root is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(LongInt(Designer.Root));
    {$ELSE}
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Form.Name, Value) = 0) then
  begin
    if not (Designer.Form is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(LongInt(Designer.Form));
    {$ENDIF}
  end
  else
    inherited SetValue(Value);
end;

{ TRxTranslatorEditor }

type
  TRxTranslatorEditor = class(TComponentEditor)
  private
    procedure CreateLanguageFile;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TRxTranslatorEditor }

procedure TRxTranslatorEditor.CreateLanguageFile;
var
  fs: TSaveDialog;
begin
  fs := TSaveDialog.Create(nil);
  try
    fs.FileName := TRxTranslator(Component).LanguageFileName;
    fs.InitialDir := ExtractFilePath(fs.FileName);
    if (ExtractFileName(fs.FileName) = '') then
      fs.FileName := '';
    fs.Filter := 'Ini files (*.ini)|*.ini|All files (*.*)|*.*';
    fs.Options := fs.Options + [ofHideReadOnly];
    if fs.Execute then
    begin
      TRxTranslator(Component).CreateLanguageFile(fs.FileName, True);
    end;
  finally
    fs.Free;
  end;
end;

procedure TRxTranslatorEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: CreateLanguageFile;
  end;
end;

function TRxTranslatorEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Create &language file...'
  end;
end;

function TRxTranslatorEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ Designer registration }

procedure Register;
const
  srRXTools = 'RX Tools';
begin
{ Components }
  RegisterComponents(srRXTools, [TPicClip, TFormStorage,
    TFormPlacement, TRxWindowHook, TAppEvents, TSpeedbar, TRxCalculator,
      TRxTimerList, TPageManager, TMergeManager, TMRUManager, TSecretPanel, TRxTrayIconEx,
      TStrHolder, TRxTrayIcon, TRxMainMenu, TRxPopupMenu, TRxRichPopUpMenu, TRxViewer,
      {$IFDEF RX_D6}TRxPluginManager, {$ENDIF}
    {$IFNDEF VER80}TRxFolderMonitor, {$ENDIF}TClipboardViewer, TRxTranslator, TRxMailBoxManager,
    {$IFNDEF VER80}TRxGradientCaption, {$ENDIF}TDualListDialog, TRxCalcEditDlg
    {$IFNDEF RX_D4}, TConverter{$ENDIF}]);

  {$IFDEF RX_D3}
  RegisterNonActiveX([TPicClip, TFormPlacement, TFormStorage, TRxWindowHook,
    TDualListDialog, TSecretPanel, TSpeedbar, TClipboardViewer, TRxMailBoxManager,
      TPageManager, TMergeManager, TMRUManager, TAppEvents, TRxTimerList,
      TRxTrayIcon, TRxFolderMonitor, TRxGradientCaption], axrComponentOnly);
  {$ENDIF RX_D3}

  {$IFDEF RX_D6}
{ TRxPluginCommand }
  RegisterPropertyEditor(TypeInfo(TShortCut), TRxPluginCommand, 'ShortCut', TShortCutProperty);
  RegisterPackageWizard(TRxPluginWizard.Create);
  {$ENDIF RX_D6}

{ TPicClip }
  RegisterComponentEditor(TPicClip, TGraphicsEditor);

{ TStrHolder }
  RegisterComponentEditor(TStrHolder, TStringsEditor);

{ TFormPlacement }
  RegisterPropertyEditor(TypeInfo(TWinMinMaxInfo), TFormPlacement,
    'MinMaxInfo', TMinMaxProperty);

{ TFormStorage }
  RegisterComponentEditor(TFormStorage, TFormStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TFormStorage, 'StoredProps',
    TStoredPropsProperty);

{ TRxWindowHook }
  RegisterPropertyEditor(TypeInfo(TWinControl), TRxWindowHook,
    'WinControl', TComponentFormProperty);

{ TSpeedbar }
  RegisterNoIcon([TSpeedItem, TSpeedbarSection]);
  RegisterComponentEditor(TSpeedbar, TSpeedbarCompEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TSpeedItem, 'BtnCaption', THintProperty);

{ TPageManager }
  RegisterNoIcon([TPageProxy]);
  RegisterComponentEditor(TPageManager, TPageManagerEditor);
  RegisterPropertyEditor(TypeInfo(TList), TPageManager, 'PageProxies',
    TProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TPageProxy, 'PageName',
    TPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TPageManager, 'PriorBtn',
    TPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TPageManager, 'NextBtn',
    TPageBtnProperty);

{ TMergeManager }
  RegisterPropertyEditor(TypeInfo(TWinControl), TMergeManager,
    'MergeFrame', TComponentFormProperty);

{ TRxTimerList }
  RegisterNoIcon([TRxTimerEvent]);
  RegisterComponentEditor(TRxTimerList, TTimersCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TList), TRxTimerList, 'Events',
    TTimersItemListProperty);

{ TRxTrayIcon }
  RegisterPropertyEditor(TypeInfo(TIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxTrayIcon, 'Hint',
    TStringProperty);
  {$IFDEF RX_D4}

{ RxMenus }
  RegisterPropertyEditor(TypeInfo(Boolean), TRxMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TRxPopupMenu, 'OwnerDraw', nil);
  {$ENDIF}

  {$IFDEF USE_RX_GIF}
{ TRxGIFAnimator }
  RegisterComponentEditor(TRxGIFAnimator, TGraphicsEditor);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TGraphicsEditor);

  RegisterComponentEditor(TRxTranslator, TRxTranslatorEditor);
  {$IFNDEF VER80}
{ TRxGradientCaption }
  RegisterComponentEditor(TRxGradientCaption, TGradientCaptionEditor);
  {$IFNDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(TRxCaptionList), TRxGradientCaption, '',
    TGradientCaptionsProperty);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF RX_D3}
{ Project Resource Expert }
  RegisterResourceExpert;
  {$ENDIF}
end;

end.