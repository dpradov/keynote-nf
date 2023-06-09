{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit AppEvent;

{$C PRELOAD}
{$I RX.INC}

interface

uses SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms
  {$IFDEF RX_D4}, ActnList {$ENDIF};

const
{$IFDEF WIN32}
  DefHintColor = clInfoBk;
  DefHintPause = 500;
  DefHintShortPause = DefHintPause div 10;
  DefHintHidePause = DefHintPause * 5;
{$ELSE}
  DefHintColor = $80FFFF;
  DefHintPause = 800;
{$ENDIF}

{ TAppEvents }

type
  TAppEvents = class(TComponent)
  private
    { Private declarations }
    FChained: Boolean;
    FHintColor: TColor;
    FHintPause: Integer;
    FShowHint: Boolean;
    FCanvas: TCanvas;
    FUpdateFormatSettings: Boolean;
{$IFDEF WIN32}
    FHintShortPause: Integer;
    FHintHidePause: Integer;
    FShowMainForm: Boolean;
{$ENDIF}
{$IFDEF RX_D3}
    FUpdateMetricSettings: Boolean;
{$ENDIF}
{$IFDEF RX_D4}
    FHintShortCuts: Boolean;
    FBiDiMode: TBiDiMode;
    FMouseDragImmediate: Boolean;
    FMouseDragThreshold: Integer;
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
{$ENDIF}
{$IFDEF RX_D5}
    FBiDiKeyboard: string;
    FNonBiDiKeyboard: string; 
{$ENDIF}
    FOnPaintIcon: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnMessage: TMessageEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnSettingsChanged: TNotifyEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
    procedure UpdateAppProps;
    function GetCanvas: TCanvas;
    function GetHintColor: TColor;
    function GetHintPause: Integer;
    function GetShowHint: Boolean;
    procedure SetHintColor(Value: TColor);
    procedure SetHintPause(Value: Integer);
    procedure SetShowHint(Value: Boolean);
    function GetUpdateFormatSettings: Boolean;
    procedure SetUpdateFormatSettings(Value: Boolean);
{$IFDEF WIN32}
    function GetHintShortPause: Integer;
    function GetHintHidePause: Integer;
    function GetShowMainForm: Boolean;
    procedure SetHintShortPause(Value: Integer);
    procedure SetHintHidePause(Value: Integer);
    procedure SetShowMainForm(Value: Boolean);
{$ENDIF WIN32}
{$IFDEF RX_D3}
    function GetUpdateMetricSettings: Boolean;
    procedure SetUpdateMetricSettings(Value: Boolean);
{$ENDIF}
{$IFDEF RX_D4}
    function GetHintShortCuts: Boolean;
    function GetBiDiMode: TBiDiMode;
    procedure SetHintShortCuts(Value: Boolean);
    procedure SetBiDiMode(Value: TBiDiMode);
    function GetMouseDragImmediate: Boolean;
    function GetMouseDragThreshold: Integer;
    procedure SetMouseDragImmediate(Value: Boolean);
    procedure SetMouseDragThreshold(Value: Integer);
{$ENDIF}
{$IFDEF RX_D5}
    function GetBiDiKeyboard: string;
    function GetNonBiDiKeyboard: string; 
    procedure SetBiDiKeyboard(const Value: string);
    procedure SetNonBiDiKeyboard(const Value: string);
{$ENDIF}
  protected
    procedure Loaded; override;
    procedure PaintIcon; virtual;
    procedure SettingsChanged; dynamic;
    function MessageHook(var Msg: TMessage): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas; { for painting the icon }
  published
    property Chained: Boolean read FChained write FChained default True;
    property HintColor: TColor read GetHintColor write SetHintColor default DefHintColor;
    property HintPause: Integer read GetHintPause write SetHintPause default DefHintPause;
    property ShowHint: Boolean read GetShowHint write SetShowHint default True;
    property UpdateFormatSettings: Boolean read GetUpdateFormatSettings
      write SetUpdateFormatSettings default True;
{$IFDEF WIN32}
    property HintShortPause: Integer read GetHintShortPause write SetHintShortPause
      default DefHintShortPause;
    property HintHidePause: Integer read GetHintHidePause write SetHintHidePause
      default DefHintHidePause;
    property ShowMainForm: Boolean read GetShowMainForm write SetShowMainForm
      default True;
{$ENDIF}
{$IFDEF RX_D3}
    property UpdateMetricSettings: Boolean read GetUpdateMetricSettings
      write SetUpdateMetricSettings default True;
{$ENDIF}
{$IFDEF RX_D4}
    property HintShortCuts: Boolean read GetHintShortCuts write SetHintShortCuts
      default True;
    property BiDiMode: TBiDiMode read GetBiDiMode write SetBiDiMode
      default bdLeftToRight;
    property MouseDragImmediate: Boolean read GetMouseDragImmediate
      write SetMouseDragImmediate default True;
    property MouseDragThreshold: Integer read GetMouseDragThreshold
      write SetMouseDragThreshold default 5;
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
{$ENDIF}
{$IFDEF RX_D5}
    property BiDiKeyboard: string read GetBiDiKeyboard write SetBiDiKeyboard;
    property NonBiDiKeyboard: string read GetNonBiDiKeyboard write SetNonBiDiKeyboard; 
{$ENDIF}
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnPaintIcon: TNotifyEvent read FOnPaintIcon write FOnPaintIcon;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
    property OnActiveControlChange: TNotifyEvent read FOnActiveControlChange write FOnActiveControlChange;
    property OnActiveFormChange: TNotifyEvent read FOnActiveFormChange write FOnActiveFormChange;
  end;

implementation

uses AppUtils, VclUtils;

{ TAppEventList }

type
  TAppEventList = class(TObject)
  private
    FAppEvents: TList;
    FHooked: Boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnIdle: TIdleEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnMessage: TMessageEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShowHint: TShowHintEvent;
    FOnActiveControlChange: TNotifyEvent;
    FOnActiveFormChange: TNotifyEvent;
{$IFDEF RX_D4}
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnShortCut: TShortCutEvent;
{$ENDIF}
    procedure AddEvents(App: TAppEvents);
    procedure RemoveEvents(App: TAppEvents);
    procedure ClearEvents;
  protected
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    function DoHelp(Command: Word; Data: Longint;
      var CallHelp: Boolean): Boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoActiveControlChange(Sender: TObject);
    procedure DoActiveFormChange(Sender: TObject);
{$IFDEF RX_D4}
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoShortCut(var Msg: TWMKey; var Handled: Boolean);
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TAppEventList.Create;
begin
  inherited Create;
  FAppEvents := TList.Create;
end;

destructor TAppEventList.Destroy;
begin
  ClearEvents;
  FAppEvents.Free;
  inherited Destroy;
end;

procedure TAppEventList.ClearEvents;
begin
  if FHooked then begin
    Application.OnActivate := nil;
    Application.OnDeactivate := nil;
    Application.OnException := nil;
    Application.OnIdle := nil;
    Application.OnHelp := nil;
    Application.OnHint := nil;
    Application.OnMessage := nil;
    Application.OnMinimize := nil;
    Application.OnRestore := nil;
    Application.OnShowHint := nil;
{$IFDEF RX_D4}
    Application.OnActionExecute := nil;
    Application.OnActionUpdate := nil;
    Application.OnShortCut := nil;
{$ENDIF}
    if Screen <> nil then begin
      Screen.OnActiveControlChange := nil;
      Screen.OnActiveFormChange := nil;
    end;
  end;
end;

procedure TAppEventList.AddEvents(App: TAppEvents);
begin
  if (App <> nil) and (FAppEvents.IndexOf(App) = -1) then begin
    FAppEvents.Add(App);
    if not (csDesigning in App.ComponentState) and (FAppEvents.Count = 1) then
    begin
      FOnActivate := Application.OnActivate;
      FOnDeactivate := Application.OnDeactivate;
      FOnException := Application.OnException;
      FOnIdle := Application.OnIdle;
      FOnHelp := Application.OnHelp;
      FOnHint := Application.OnHint;
      FOnMessage := Application.OnMessage;
      FOnMinimize := Application.OnMinimize;
      FOnRestore := Application.OnRestore;
      FOnShowHint := Application.OnShowHint;
{$IFDEF RX_D4}
      FOnActionExecute := Application.OnActionExecute;
      FOnActionUpdate := Application.OnActionUpdate;
      FOnShortCut := Application.OnShortCut;
      Application.OnActionExecute := DoActionExecute;
      Application.OnActionUpdate := DoActionUpdate;
      Application.OnShortCut := DoShortCut;
{$ENDIF}
      Application.OnActivate := DoActivate;
      Application.OnDeactivate := DoDeactivate;
      Application.OnException := DoException;
      Application.OnIdle := DoIdle;
      Application.OnHelp := DoHelp;
      Application.OnHint := DoHint;
      Application.OnMessage := DoMessage;
      Application.OnMinimize := DoMinimize;
      Application.OnRestore := DoRestore;
      Application.OnShowHint := DoShowHint;
      if Screen <> nil then begin
        FOnActiveControlChange := Screen.OnActiveControlChange;
        FOnActiveFormChange := Screen.OnActiveFormChange;
        Screen.OnActiveControlChange := DoActiveControlChange;
        Screen.OnActiveFormChange := DoActiveFormChange;
      end;
      FHooked := True;
    end;
  end;
end;

procedure TAppEventList.RemoveEvents(App: TAppEvents);
begin
  if FAppEvents.IndexOf(App) >= 0 then FAppEvents.Remove(App);
  if not (csDesigning in App.ComponentState) and (FAppEvents.Count = 0) then
    ClearEvents;
end;

procedure TAppEventList.DoActivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnActivate) then
      TAppEvents(FAppEvents[I]).FOnActivate(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnActivate) then FOnActivate(Sender);
end;

procedure TAppEventList.DoDeactivate(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnDeactivate) then
      TAppEvents(FAppEvents[I]).FOnDeactivate(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnDeactivate) then FOnDeactivate(Sender);
end;

procedure TAppEventList.DoException(Sender: TObject; E: Exception);
var
  I: Integer;
  Handled: Boolean;
begin
  Handled := False;
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnException) then begin
      TAppEvents(FAppEvents[I]).FOnException(Sender, E);
      Handled := True;
    end;
    if not TAppEvents(FAppEvents[I]).Chained then begin
      if not Handled then Application.ShowException(E);
      Exit;
    end;
  end;
  if Assigned(FOnException) then begin
    FOnException(Sender, E);
    Handled := True;
  end;
  if not Handled then Application.ShowException(E);
end;

procedure TAppEventList.DoIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnIdle) then
      TAppEvents(FAppEvents[I]).FOnIdle(Sender, Done);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnIdle) then FOnIdle(Sender, Done);
end;

function TAppEventList.DoHelp(Command: Word; Data: Longint;
  var CallHelp: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnHelp) then
      Result := TAppEvents(FAppEvents[I]).FOnHelp(Command, Data, CallHelp);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnHelp) then Result := FOnHelp(Command, Data, CallHelp);
end;

procedure TAppEventList.DoHint(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnHint) then
      TAppEvents(FAppEvents[I]).FOnHint(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnHint) then FOnHint(Sender);
end;

procedure TAppEventList.DoMessage(var Msg: TMsg; var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnMessage) then
      TAppEvents(FAppEvents[I]).FOnMessage(Msg, Handled);
    if not TAppEvents(FAppEvents[I]).Chained or Handled then Exit;
  end;
  if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
end;

procedure TAppEventList.DoMinimize(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnMinimize) then
      TAppEvents(FAppEvents[I]).FOnMinimize(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnMinimize) then FOnMinimize(Sender);
end;

procedure TAppEventList.DoRestore(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnRestore) then
      TAppEvents(FAppEvents[I]).FOnRestore(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnRestore) then FOnRestore(Sender);
end;

procedure TAppEventList.DoShowHint(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnShowHint) then
      TAppEvents(FAppEvents[I]).FOnShowHint(HintStr, CanShow, HintInfo);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
end;

procedure TAppEventList.DoActiveControlChange(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnActiveControlChange) then
      TAppEvents(FAppEvents[I]).FOnActiveControlChange(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnActiveControlChange) then FOnActiveControlChange(Sender);
end;

procedure TAppEventList.DoActiveFormChange(Sender: TObject);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnActiveFormChange) then
      TAppEvents(FAppEvents[I]).FOnActiveFormChange(Sender);
    if not TAppEvents(FAppEvents[I]).Chained then Exit;
  end;
  if Assigned(FOnActiveFormChange) then FOnActiveFormChange(Sender);
end;

{$IFDEF RX_D4}

procedure TAppEventList.DoActionExecute(Action: TBasicAction;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnActionExecute) then
      TAppEvents(FAppEvents[I]).FOnActionExecute(Action, Handled);
    if not TAppEvents(FAppEvents[I]).Chained or Handled then Exit;
  end;
  if Assigned(FOnActionExecute) then FOnActionExecute(Action, Handled);
end;

procedure TAppEventList.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnActionUpdate) then
      TAppEvents(FAppEvents[I]).FOnActionUpdate(Action, Handled);
    if not TAppEvents(FAppEvents[I]).Chained or Handled then Exit;
  end;
  if Assigned(FOnActionUpdate) then FOnActionUpdate(Action, Handled);
end;

procedure TAppEventList.DoShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  I: Integer;
begin
  for I := FAppEvents.Count - 1 downto 0 do begin
    if Assigned(TAppEvents(FAppEvents[I]).FOnShortCut) then
      TAppEvents(FAppEvents[I]).FOnShortCut(Msg, Handled);
    if not TAppEvents(FAppEvents[I]).Chained or Handled then Exit;
  end;
  if Assigned(FOnShortCut) then FOnShortCut(Msg, Handled);
end;

{$ENDIF RX_D4}

const
  AppList: TAppEventList = nil;

{ TAppEvents }

constructor TAppEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AppList = nil then AppList := TAppEventList.Create;
  FChained := True;
  FHintColor := DefHintColor;
  FHintPause := DefHintPause;
  FShowHint := True;
{$IFDEF RX_D3}
  FUpdateMetricSettings := True;
{$ENDIF}
{$IFDEF WIN32}
  FHintShortPause := DefHintShortPause;
  FHintHidePause := DefHintHidePause;
  FShowMainForm := True;
{$ENDIF}
{$IFDEF RX_D4}
  FHintShortCuts := True;
  FBiDiMode := bdLeftToRight;
  FMouseDragImmediate := True;
  FMouseDragThreshold := 5;
{$ENDIF}
  FUpdateFormatSettings := True;
  if not (csDesigning in ComponentState) then
    Application.HookMainWindow(MessageHook);
  AppList.AddEvents(Self);
end;

destructor TAppEvents.Destroy;
begin
  if not (csDesigning in ComponentState) then
    Application.UnhookMainWindow(MessageHook);
  if Self <> nil then AppList.RemoveEvents(Self);
  FCanvas.Free;
  inherited Destroy;
end;

procedure TAppEvents.Loaded;
begin
  inherited Loaded;
  UpdateAppProps;
end;

function TAppEvents.GetCanvas: TCanvas;
begin
  if FCanvas = nil then FCanvas := TCanvas.Create;
  Result := FCanvas;
end;

procedure TAppEvents.PaintIcon;
var
  PS: TPaintStruct;
begin
  BeginPaint(Application.Handle, PS);
  try
    if FCanvas <> nil then FCanvas.Free;
    FCanvas := TCanvas.Create;
    try
      Canvas.Handle := PS.hDC;
      Canvas.Brush.Color := clBackground;
      if PS.fErase then Canvas.FillRect(PS.rcPaint);
      if Assigned(FOnPaintIcon) then FOnPaintIcon(Self);
    finally
      FCanvas.Free;
      FCanvas := nil;
    end;
  finally
    EndPaint(Application.Handle, PS);
  end;
end;

procedure TAppEvents.SettingsChanged;
begin
  if Assigned(FOnSettingsChanged) then FOnSettingsChanged(Self);
end;

function TAppEvents.MessageHook(var Msg: TMessage): Boolean;
begin
  Result := False;
  case Msg.Msg of
    WM_WININICHANGE:
      begin
{$IFNDEF WIN32}
        if UpdateFormatSettings then GetFormatSettings;
{$ELSE}
  {$IFNDEF RX_D3}
        if Application.ShowHint then begin
          Application.ShowHint := False;
          Application.ShowHint := True;
        end;
  {$ENDIF}
{$ENDIF}
        try
          SettingsChanged;
        except
          Application.HandleException(Self);
        end;
      end;
{$IFNDEF WIN32}
    WM_ENDSESSION: if WordBool(Msg.wParam) then Halt;
{$ENDIF}
    WM_PAINT:
      if Assigned(FOnPaintIcon) and IsIconic(Application.Handle) then
      begin
        PaintIcon;
        Result := True;
      end;
  end;
end;

function TAppEvents.GetHintColor: TColor;
begin
  if (csDesigning in ComponentState) then Result := FHintColor
  else Result := Application.HintColor;
end;

function TAppEvents.GetHintPause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintPause
  else Result := Application.HintPause;
end;

function TAppEvents.GetShowHint: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FShowHint
  else Result := Application.ShowHint;
end;

procedure TAppEvents.SetHintColor(Value: TColor);
begin
  FHintColor := Value;
  if not (csDesigning in ComponentState) then Application.HintColor := Value;
end;

procedure TAppEvents.SetHintPause(Value: Integer);
begin
  FHintPause := Value;
  if not (csDesigning in ComponentState) then Application.HintPause := Value;
end;

procedure TAppEvents.SetShowHint(Value: Boolean);
begin
  FShowHint := Value;
  if not (csDesigning in ComponentState) then Application.ShowHint := Value;
end;

function TAppEvents.GetUpdateFormatSettings: Boolean;
begin
{$IFDEF WIN32}
  if (csDesigning in ComponentState) then Result := FUpdateFormatSettings
  else Result := Application.UpdateFormatSettings;
{$ELSE}
  Result := FUpdateFormatSettings;
{$ENDIF}
end;

procedure TAppEvents.SetUpdateFormatSettings(Value: Boolean);
begin
  FUpdateFormatSettings := Value;
{$IFDEF WIN32}
  if not (csDesigning in ComponentState) then
    Application.UpdateFormatSettings := Value;
{$ENDIF}
end;

{$IFDEF WIN32}

function TAppEvents.GetHintShortPause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintShortPause
  else Result := Application.HintShortPause;
end;

function TAppEvents.GetHintHidePause: Integer;
begin
  if (csDesigning in ComponentState) then Result := FHintHidePause
  else Result := Application.HintHidePause;
end;

function TAppEvents.GetShowMainForm: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FShowMainForm
  else Result := Application.ShowMainForm;
end;

procedure TAppEvents.SetHintShortPause(Value: Integer);
begin
  FHintShortPause := Value;
  if not (csDesigning in ComponentState) then Application.HintShortPause := Value;
end;

procedure TAppEvents.SetHintHidePause(Value: Integer);
begin
  FHintHidePause := Value;
  if not (csDesigning in ComponentState) then Application.HintHidePause := Value;
end;

procedure TAppEvents.SetShowMainForm(Value: Boolean);
begin
  FShowMainForm := Value;
  if not (csDesigning in ComponentState) then Application.ShowMainForm := Value;
end;

{$ENDIF WIN32}

{$IFDEF RX_D3}

function TAppEvents.GetUpdateMetricSettings: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FUpdateMetricSettings
  else Result := Application.UpdateMetricSettings;
end;

procedure TAppEvents.SetUpdateMetricSettings(Value: Boolean);
begin
  FUpdateMetricSettings := Value;
  if not (csDesigning in ComponentState) then
    Application.UpdateMetricSettings := Value;
end;

{$ENDIF RX_D3}

{$IFDEF RX_D4}

function TAppEvents.GetHintShortCuts: Boolean;
begin
  if (csDesigning in ComponentState) then Result := FHintShortCuts
  else Result := Application.HintShortCuts;
end;

function TAppEvents.GetBiDiMode: TBiDiMode;
begin
  if (csDesigning in ComponentState) then Result := FBiDiMode
  else Result := Application.BiDiMode;
end;

function TAppEvents.GetMouseDragImmediate: Boolean;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragImmediate
  else Result := Mouse.DragImmediate;
end;

function TAppEvents.GetMouseDragThreshold: Integer;
begin
  if (csDesigning in ComponentState) or (Mouse = nil) then
    Result := FMouseDragThreshold
  else Result := Mouse.DragThreshold;
end;

procedure TAppEvents.SetMouseDragImmediate(Value: Boolean);
begin
  FMouseDragImmediate := Value;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragImmediate := Value;
end;

procedure TAppEvents.SetMouseDragThreshold(Value: Integer);
begin
  FMouseDragThreshold := Value;
  if not (csDesigning in ComponentState) and (Mouse <> nil) then
    Mouse.DragThreshold := Value;
end;

procedure TAppEvents.SetHintShortCuts(Value: Boolean);
begin
  FHintShortCuts := Value;
  if not (csDesigning in ComponentState) then
    Application.HintShortCuts := Value;
end;

procedure TAppEvents.SetBiDiMode(Value: TBiDiMode);
begin
  FBiDiMode := Value;
  if not (csDesigning in ComponentState) then
    Application.BiDiMode := Value;
end;

{$ENDIF RX_D4}

{$IFDEF RX_D5}

function TAppEvents.GetBiDiKeyboard: string;
begin
  if (csDesigning in ComponentState) then Result := FBiDiKeyboard
  else Result := Application.BiDiKeyboard;
end;

function TAppEvents.GetNonBiDiKeyboard: string; 
begin
  if (csDesigning in ComponentState) then Result := FNonBiDiKeyboard
  else Result := Application.NonBiDiKeyboard;
end;

procedure TAppEvents.SetBiDiKeyboard(const Value: string);
begin
  FBiDiKeyboard := Value;
  if not (csDesigning in ComponentState) then
    Application.BiDiKeyboard := Value;
end;

procedure TAppEvents.SetNonBiDiKeyboard(const Value: string);
begin
  FNonBiDiKeyboard := Value;
  if not (csDesigning in ComponentState) then
    Application.NonBiDiKeyboard := Value;
end;

{$ENDIF RX_D5}

procedure TAppEvents.UpdateAppProps;
begin
  if not (csDesigning in ComponentState) then begin
    with Application do begin
      HintColor := FHintColor;
      HintPause := FHintPause;
      ShowHint := FShowHint;
{$IFDEF WIN32}
      HintShortPause := FHintShortPause;
      HintHidePause := FHintHidePause;
      ShowMainForm := FShowMainForm;
      UpdateFormatSettings := FUpdateFormatSettings;
{$ENDIF}
{$IFDEF RX_D3}
      UpdateMetricSettings := FUpdateMetricSettings;
{$ENDIF}
{$IFDEF RX_D4}
      HintShortCuts := FHintShortCuts;
      BiDiMode := FBiDiMode;
      with Mouse do begin
        DragImmediate := FMouseDragImmediate;
        DragThreshold := FMouseDragThreshold;
      end;
{$ENDIF}
{$IFDEF RX_D5}
      BiDiKeyboard := FBiDiKeyboard;
      NonBiDiKeyboard := FNonBiDiKeyboard;      
{$ENDIF}
    end;
  end;
end;

procedure DestroyLocals; far;
begin
  if AppList <> nil then begin
    AppList.Free;
    AppList := nil;
  end;
end;

initialization
{$IFDEF WIN32}
finalization
  DestroyLocals;
{$ELSE}
  AddExitProc(DestroyLocals);
{$ENDIF}
end.