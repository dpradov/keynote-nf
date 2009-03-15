{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
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

{$IFDEF WIN32}
{$R *.D32}
{$ELSE}
{$R *.D16}
{$ENDIF}

uses Classes, SysUtils, Controls, Graphics, TypInfo, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Consts,
  ExtCtrls, PictEdit, RxHook, PicClip, Placemnt, PresrDsn, MinMaxEd, DualList,
  ClipView, Speedbar,  DataConv, RXCalc, PageMngr, PgMngrEd, MrgMngr,
  StrHlder, RXShell, AppEvent, VCLUtils, TimerLst, TimLstEd, IcoList, IcoLEdit,
  {$IFDEF USE_RX_GIF} RxGIF, GIFCtrl, {$ENDIF} RXLConst, RXCtrls,
  {$IFDEF RX_D3} RxResExp, {$ENDIF} RxMenus, MRUList,
  {$IFDEF WIN32} RxNotify, RxGrdCpt, GradEdit, {$ENDIF} HintProp;

{ TStringsEditor }

type
  TStringsEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

procedure TStringsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'STRINGS') = 0) then begin
    Prop.Edit;
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
  if (Designer.Root is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Root.Name <> '') then Proc(Designer.Root.Name);
end;

procedure TComponentFormProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
{$IFDEF WIN32}
  Component := Designer.GetComponent(Value);
{$ELSE}
  Component := Designer.Root.FindComponent(Value);
{$ENDIF}
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Root.Name, Value) = 0) then
  begin
    if not (Designer.Root is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(Longint(Designer.Root));
  end
  else inherited SetValue(Value);
end;

{ Designer registration }

procedure Register;
begin
{ Components }
  RegisterComponents(LoadStr(srRXTools), [TPicClip, TFormStorage,
    TFormPlacement, TRxWindowHook, TAppEvents, TSpeedbar, TRxCalculator,
    TRxTimerList, TPageManager, TMergeManager, TMRUManager, TSecretPanel,
    TStrHolder, TRxTrayIcon, TRxMainMenu, TRxPopupMenu,
    {$IFDEF WIN32} TRxFolderMonitor, {$ENDIF} TClipboardViewer,
    {$IFDEF WIN32} TRxGradientCaption, {$ENDIF} TDualListDialog
    {$IFNDEF RX_D4}, TConverter {$ENDIF}]);

{$IFDEF RX_D3}
  RegisterNonActiveX([TPicClip, TFormPlacement, TFormStorage, TRxWindowHook,
    TDualListDialog, TSecretPanel, TSpeedbar, TClipboardViewer,
    TPageManager, TMergeManager, TMRUManager, TAppEvents, TRxTimerList, 
    TRxTrayIcon, TRxFolderMonitor, TRxGradientCaption], axrComponentOnly);
{$ENDIF RX_D3}

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
  // OF RegisterComponentEditor(TSpeedbar, TSpeedbarCompEditor);
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

{$IFDEF WIN32}
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