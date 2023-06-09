{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

{.$DEFINE USE_TIMER}
{ - Use Windows timer instead thread to the animated TrayIcon }

{$IFDEF VER80}
{$DEFINE USE_TIMER} { - Always use timer in 16-bit version }
{$ENDIF}

unit RxShell;

{$I RX.INC}
{$P+,W-,R-}

interface

{$I RX.INC}

uses
  {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}Messages,
  Classes, Graphics, SysUtils, Forms, Controls, Menus, ShellAPI, ActiveX, ShlObj, ComObj,
  {$IFDEF USE_TIMER}ExtCtrls, {$ENDIF}RxIcoList, RxStrUtils;

type
  {$IFDEF VER80}
  PNotifyIconData = ^TNotifyIconData;
  TNotifyIconData = record
    cbSize: LongInt;
    Wnd: LongInt;
    uID: LongInt;
    uFlags: LongInt;
    uCallbackMessage: LongInt;
    hIcon: LongInt;
    szTip: array[0..63] of Char;
  end;
  {$ENDIF}

  TMouseButtons = set of TMouseButton;

{ TRxTrayIcon }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxTrayIcon = class(TComponent)
  private
    FHandle: HWnd;
    FActive: Boolean;
    FAdded: Boolean;
    FAnimated: Boolean;
    FEnabled: Boolean;
    FClicked: TMouseButtons;
    FIconIndex: Integer;
    FInterval: Word;
    FIconData: TNotifyIconData;
    FIcon: TIcon;
    FIconList: TIconList;
    {$IFDEF USE_TIMER}
    FTimer: TTimer;
    {$ELSE}
    FTimer: TThread;
    {$ENDIF}
    FHint: string;
    FShowDesign: Boolean;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    procedure ChangeIcon;
    {$IFDEF USE_TIMER}
    procedure Timer(Sender: TObject);
    {$ELSE}
    procedure Timer;
    {$ENDIF}
    procedure SendCancelMode;
    function CheckMenuPopup(X, Y: Integer): Boolean;
    function CheckDefaultMenuItem: Boolean;
    procedure SetHint(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetIconList(Value: TIconList);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure Activate;
    procedure Deactivate;
    procedure SetActive(Value: Boolean);
    function GetAnimated: Boolean;
    procedure SetAnimated(Value: Boolean);
    procedure SetShowDesign(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure IconChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage);
    function GetActiveIcon: TIcon;
  protected
    procedure DblClick; dynamic;
    procedure DoClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateNotifyData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    property Handle: HWnd read FHandle;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TIconList read FIconList write SetIconList;
    { Ensure Icons is declared before Animated }
    property Animated: Boolean read GetAnimated write SetAnimated default False;
    property Interval: Word read FInterval write SetInterval default 150;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowDesign: Boolean read FShowDesign write SetShowDesign stored False;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

  {  TDuplicateComponent  }

  TDuplicateComponent = class(Exception);

  { Define a Form not Owner object exception }

  TFormNotOwner = class(Exception);

  {  TRxTrayIconEx  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxTrayIconEx = class(TRxTrayIcon)
  private
    FHideForm: Boolean;
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    procedure SetHideForm(Value: Boolean);
    procedure HookParent;
    procedure UnhookParent;
    procedure HookWndProc(var Message: TMessage);
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure ProcessEnabled;
  published
    property HideForm: Boolean read FHideForm write SetHideForm default True;
  end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
procedure WinAbout(const AppName, Stuff: string);

type
  TExecState = (esNormal, esMinimized, esMaximized, esHidden);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;

type
  TShowCmd = (scNormal, scMinimized, scMaximized);

  {  TRxShellLinker  }

  TRxShellLinker = class
  private
    FLnkFile, FCmdLine, FArg, FWorkDir, FDescr, FIconFile: string;
    FShowCmd: TShowCmd;
    FHotKey: Word;
    FIconIndex: Integer;
  public
    property FileName: string read FLnkFile write FLnkFile;
    property CmdLine: string read FCmdLine write FCmdLine;
    property Arguments: string read FArg write FArg;
    property WorkDir: string read FWorkDir write FWorkDir;
    property ShowCmd: TShowCmd read FShowCmd write FShowCmd;
    property HotKey: Word read FHotKey write FHotKey;
    property Description: string read FDescr write FDescr;
    property IconFile: string read FIconFile write FIconFile;
    property IconIndex: Integer read FIconIndex write FIconIndex;
    function Load: Boolean;
    function LoadFromFile(FileName: string): Boolean;
    function Save: Boolean;
    function SaveToFile(FileName: string): Boolean;
  end;

function CreateShellLink(const LinkFile, CmdLine, Args, WorkDir: string;
  ShowCmd: TShowCmd): Boolean;

implementation

uses
  RxConst, RxResConst, RxVCLUtils, RxMaxMin;

{$IFDEF VER80}
const
  Shell = 'shell';

function ExtractAssociatedIcon(hInst: THandle; lpIconPath: PChar;
  var lpiIcon: Word): HIcon; far; external Shell;

function ShellAbout(Wnd: HWnd; App, Stuff: PChar; Icon: HIcon): Integer;
  far; external Shell;
{$ENDIF}

procedure WinAbout(const AppName, Stuff: string);
var
  {$IFDEF VER80}
  szApp, szStuff: array[0..255] of Char;
  {$ENDIF}
  Wnd: HWnd;
  Icon: HIcon;
begin
  if Application.MainForm <> nil then
    Wnd := Application.MainForm.Handle
  else
    Wnd := 0;
  Icon := Application.Icon.Handle;
  if Icon = 0 then Icon := LoadIcon(0, IDI_APPLICATION);
  {$IFNDEF VER80}
  ShellAbout(Wnd, PChar(AppName), PChar(Stuff), Icon);
  {$ELSE}
  StrPLCopy(szApp, AppName, SizeOf(szApp) - 1);
  StrPLCopy(szStuff, Stuff, SizeOf(szStuff) - 1);
  ShellAbout(Wnd, szApp, szStuff, Icon);
  {$ENDIF}
end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
var
  S: array[0..255] of Char;
  IconHandle: HIcon;
  Index: Word;
begin
  Result := TIcon.Create;
  try
    StrPLCopy(S, FileName, SizeOf(S) - 1);
    IconHandle := ExtractIcon(hInstance, S, Id);
    if IconHandle < 2 then
    begin
      Index := Id;
      IconHandle := ExtractAssociatedIcon(hInstance, S, Index);
    end;
    if IconHandle < 2 then
    begin
      if IconHandle = 1 then
        raise EResNotFound.Create(RxLoadStr(SFileNotExec))
      else
      begin
        Result.Free;
        Result := nil;
      end;
    end
    else
      Result.Handle := IconHandle;
  except
    Result.Free;
    raise;
  end;
end;

const
  ShowCommands: array[TExecState] of Integer =
  (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_HIDE);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
{$IFNDEF VER80}
begin
  Result := ShellExecute(Application.Handle, nil, PChar(FileName),
    PChar(Params), PChar(StartDir), ShowCommands[InitialState]);
end;
{$ELSE}
var
  cFileName, cParams, cPath: array[0..80] of Char;
begin
  Result := ShellExecute(Application.Handle, nil, StrPCopy(cFileName,
    FileName), StrPCopy(cParams, Params), StrPCopy(cPath, StartDir),
    ShowCommands[InitialState]);
end;
{$ENDIF}

function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;
{$IFNDEF VER80}
var
  Info: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  with Info do
  begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(FileName);
    lpParameters := PChar(Params);
    lpDirectory := PChar(StartDir);
    nShow := ShowCommands[InitialState];
  end;
  if ShellExecuteEx(@Info) then
  begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(Info.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    Result := ExitCode;
  end
  else
    Result := -1;
end;
{$ELSE}
var
  Task: THandle;
begin
  Result := 0;
  Task := FileExecute(FileName, Params, StartDir, InitialState);
  if Task >= HINSTANCE_ERROR then
  begin
    repeat
      Application.ProcessMessages;
    until (GetModuleUsage(Task) = 0) or Application.Terminated;
  end
  else
    Result := -1;
end;
{$ENDIF}

{$IFNDEF USE_TIMER}

{ TTimerThread }

type
  TTimerThread = class(TThread)
  private
    FOwnerTray: TRxTrayIcon;
  protected
    procedure Execute; override;
  public
    constructor Create(TrayIcon: TRxTrayIcon; CreateSuspended: Boolean);
  end;

constructor TTimerThread.Create(TrayIcon: TRxTrayIcon; CreateSuspended: Boolean);
begin
  FOwnerTray := TrayIcon;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwnerTray = nil);
  end;

begin
  while not Terminated do
  begin
    if not ThreadClosed then
      if SleepEx(FOwnerTray.FInterval, False) = 0 then
      begin
        if not ThreadClosed and FOwnerTray.Animated then
          FOwnerTray.Timer;
      end;
  end;
end;

{$ENDIF USE_TIMER}

{$IFDEF VER80}

type
  TLoadLibrary32 = function(FileName: PChar; Handle, Special: LongInt): LongInt;
  TFreeLibrary32 = function(Handle: THandle): Bool;
  TGetAddress32 = function(Handle: THandle; ProcName: PChar): Pointer;
  TCallProc32 = function(Msg: LongInt; Data: PNotifyIconData; ProcHandle: Pointer;
    AddressConvert, Params: LongInt): LongInt;

const
  NIM_ADD = $00000000;
  NIM_MODIFY = $00000001;
  NIM_DELETE = $00000002;

  NIF_MESSAGE = $00000001;
  NIF_ICON = $00000002;
  NIF_TIP = $00000004;

const
  Shell32: LongInt = 0;
  ProcAddr: Pointer = nil;
  FreeLib32: TFreeLibrary32 = nil;
  CallPrc32: TCallProc32 = nil;

procedure FreeHandles; far;
begin
  if (ProcAddr <> nil) and Assigned(FreeLib32) then FreeLib32(Shell32);
end;

procedure InitHandles;
var
  Kernel16: THandle;
  LoadLib32: TLoadLibrary32;
  GetAddr32: TGetAddress32;
begin
  Kernel16 := GetModuleHandle('kernel');
  @LoadLib32 := GetProcAddress(Kernel16, 'LoadLibraryEx32W');
  @FreeLib32 := GetProcAddress(Kernel16, 'FreeLibrary32W');
  @GetAddr32 := GetProcAddress(Kernel16, 'GetProcAddress32W');
  @CallPrc32 := GetProcAddress(Kernel16, 'CallProc32W');
  if (@LoadLib32 <> nil) and (@FreeLib32 <> nil) and (@GetAddr32 <> nil) and (@CallPrc32 <> nil) then
  begin
    Shell32 := LoadLib32('shell32', 0, 0);
    if Shell32 >= HINSTANCE_ERROR then
    begin
      ProcAddr := GetAddr32(Shell32, 'Shell_NotifyIcon');
      if ProcAddr = nil then
      begin
        FreeLib32(Shell32);
        Shell32 := 1;
      end
      else
        AddExitProc(FreeHandles);
    end
    else
      Shell32 := 1;
  end;
end;

function Shell_NotifyIcon(dwMessage: LongInt; lpData: PNotifyIconData): Bool;
begin
  if (ProcAddr = nil) and (Shell32 <> 1) then InitHandles;
  if ProcAddr <> nil then
    Result := Bool(CallPrc32(dwMessage, lpData, ProcAddr, $01, 2));
end;

{$ENDIF}

{ TRxTrayIcon }

constructor TRxTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := {$IFDEF RX_D6}Classes.{$ENDIF}AllocateHWnd(WndProc); // Polaris
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FIconList := TIconList.Create;
  FIconList.OnChange := IconChanged;
  FIconIndex := -1;
  FEnabled := True;
  FInterval := 150;
  FActive := True;
end;

destructor TRxTrayIcon.Destroy;
begin
  Destroying;
  FEnabled := False;
  FIconList.OnChange := nil;
  FIcon.OnChange := nil;
  SetAnimated(False);
  Deactivate;
  {$IFDEF RX_D6}Classes.{$ENDIF}DeallocateHWnd(FHandle); // Polaris
  FIcon.Free;
  FIcon := nil;
  FIconList.Free;
  FIconList := nil;
  inherited Destroy;
end;

procedure TRxTrayIcon.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then Activate;
end;

procedure TRxTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TRxTrayIcon.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
end;

procedure TRxTrayIcon.SendCancelMode;
var
  F: TForm;
begin
  if not (csDestroying in ComponentState) then
  begin
    F := Screen.ActiveForm;
    if F = nil then F := Application.MainForm;
    if F <> nil then F.SendCancelMode(nil);
  end;
end;

function TRxTrayIcon.CheckMenuPopup(X, Y: Integer): Boolean;
begin
  Result := False;
  if not (csDesigning in ComponentState) and Active and (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    PopupMenu.PopupComponent := Self;
    SendCancelMode;
    SwitchToWindow(FHandle, False);
    Application.ProcessMessages;
    try
      PopupMenu.Popup(X, Y);
    finally
      {$IFNDEF VER80}
      SwitchToWindow(FHandle, False);
      {$ENDIF}
    end;
    Result := True;
  end;
end;

function TRxTrayIcon.CheckDefaultMenuItem: Boolean;
{$IFNDEF VER80}
var
  Item: TMenuItem;
  I: Integer;
  {$ENDIF}
begin
  Result := False;
  {$IFNDEF VER80}
  if not (csDesigning in ComponentState) and Active and (PopupMenu <> nil) and (PopupMenu.Items <> nil) then
  begin
    I := 0;
    while (I < PopupMenu.Items.Count) do
    begin
      Item := PopupMenu.Items[I];
      if Item.Default and Item.Enabled then
      begin
        Item.Click;
        Result := True;
        Break;
      end;
      Inc(I);
    end;
  end;
  {$ENDIF}
end;

procedure TRxTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TRxTrayIcon.SetIconList(Value: TIconList);
begin
  FIconList.Assign(Value);
end;

function TRxTrayIcon.GetActiveIcon: TIcon;
begin
  Result := FIcon;
  if (FIconList <> nil) and (FIconList.Count > 0) and Animated then
    Result := FIconList[Max(Min(FIconIndex, FIconList.Count - 1), 0)];
end;

function TRxTrayIcon.GetAnimated: Boolean;
begin
  Result := FAnimated;
end;

procedure TRxTrayIcon.SetAnimated(Value: Boolean);
begin
  Value := Value and Assigned(FIconList) and (FIconList.Count > 0);
  if Value <> Animated then
  begin
    if Value then
    begin
      {$IFDEF USE_TIMER}
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := FAdded;
      FTimer.Interval := FInterval;
      FTimer.OnTimer := Timer;
      {$ELSE}
      FTimer := TTimerThread.Create(Self, not FAdded);
      {$ENDIF}
      FAnimated := True;
    end
    else
    begin
      FAnimated := False;
      {$IFDEF USE_TIMER}
      FTimer.Free;
      FTimer := nil;
      {$ELSE}
      TTimerThread(FTimer).FOwnerTray := nil;
      while FTimer.Suspended do
        FTimer.{$IFDEF RX_D14}Start{$ELSE}Resume{$ENDIF};
      FTimer.Terminate;
      {$ENDIF}
    end;
    FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.SetActive(Value: Boolean);
begin
  if (Value <> FActive) then
  begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
      if Value then
        Activate
      else
        Deactivate;
  end;
end;

procedure TRxTrayIcon.Show;
begin
  Active := True;
end;

procedure TRxTrayIcon.Hide;
begin
  Active := False;
end;

procedure TRxTrayIcon.SetShowDesign(Value: Boolean);
begin
  if (csDesigning in ComponentState) then
  begin
    if Value then
      Activate
    else
      Deactivate;
    FShowDesign := FAdded;
  end;
end;

procedure TRxTrayIcon.SetInterval(Value: Word);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    {$IFDEF USE_TIMER}
    if Animated then FTimer.Interval := FInterval;
    {$ENDIF}
  end;
end;

{$IFDEF USE_TIMER}

procedure TRxTrayIcon.Timer(Sender: TObject);
{$ELSE}

procedure TRxTrayIcon.Timer;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) and Animated then
  begin
    Inc(FIconIndex);
    if (FIconList = nil) or (FIconIndex >= FIconList.Count) then
      FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.IconChanged(Sender: TObject);
begin
  ChangeIcon;
end;

procedure TRxTrayIcon.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    ChangeIcon;
  end;
end;

procedure TRxTrayIcon.UpdateNotifyData;
var
  Ico: TIcon;
begin
  with FIconData do
  begin
    cbSize := {$IFDEF RX_D15}TNotifyIconData.SizeOf{$ELSE}SizeOf(TNotifyIconData){$ENDIF};
    Wnd := FHandle;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    Ico := GetActiveIcon;
    if Ico <> nil then
      hIcon := Ico.Handle
    else
      hIcon := {$IFNDEF VER80}INVALID_HANDLE_VALUE{$ELSE}0{$ENDIF};
    StrPLCopy(szTip, GetShortHint(FHint), System.SizeOf(szTip) - 1);
    uCallbackMessage := CM_TRAYICON;
    uID := 0;
  end;
end;

procedure TRxTrayIcon.Activate;
var
  Ico: TIcon;
begin
  Deactivate;
  Ico := GetActiveIcon;
  if (Ico <> nil) and not Ico.Empty then
  begin
    FClicked := [];
    UpdateNotifyData;
    FAdded := Shell_NotifyIcon(NIM_ADD, @FIconData);
    if (GetShortHint(FHint) = '') and FAdded then
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
    {$IFDEF USE_TIMER}
    if Animated then FTimer.Enabled := True;
    {$ELSE}
    if Animated then
      while FTimer.Suspended do
        FTimer.{$IFDEF RX_D14}Start{$ELSE}Resume{$ENDIF};
    {$ENDIF}
  end;
end;

procedure TRxTrayIcon.Deactivate;
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
  FAdded := False;
  FClicked := [];
  {$IFDEF USE_TIMER}
  if Animated then FTimer.Enabled := False;
  {$ELSE}
  if Animated and not FTimer.Suspended then
    FTimer.Suspend;
  {$ENDIF}
end;

procedure TRxTrayIcon.ChangeIcon;
var
  Ico: TIcon;
begin
  if (FIconList = nil) or (FIconList.Count = 0) then SetAnimated(False);
  if FAdded then
  begin
    Ico := GetActiveIcon;
    if (Ico <> nil) and not Ico.Empty then
    begin
      UpdateNotifyData;
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
    end
    else
      Deactivate;
  end
  else
  begin
    if ((csDesigning in ComponentState) and FShowDesign) or
      (not (csDesigning in ComponentState) and FActive) then
      Activate;
  end;
end;

procedure TRxTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TRxTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.DblClick;
begin
  if not CheckDefaultMenuItem and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TRxTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and CheckMenuPopup(X, Y) then Exit;
  if Assigned(FOnClick) then FOnClick(Self, Button, Shift, X, Y);
end;

procedure TRxTrayIcon.WndProc(var Message: TMessage);

  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  end;

var
  P: TPoint;
  Shift: TShiftState;
begin
  try
    with Message do
      if (Msg = CM_TRAYICON) and Self.FEnabled then
      begin
        case lParam of
          WM_LBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_RBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MOUSEMOVE:
            begin
              GetCursorPos(P);
              MouseMove(GetShiftState, P.X, P.Y);
            end;
          WM_LBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssLeft], P.X, P.Y);
              Include(FClicked, mbLeft);
            end;
          WM_LBUTTONUP:
            begin
              Shift := GetShiftState + [ssLeft];
              GetCursorPos(P);
              if mbLeft in FClicked then
              begin
                Exclude(FClicked, mbLeft);
                DoClick(mbLeft, Shift, P.X, P.Y);
              end;
              MouseUp(mbLeft, Shift, P.X, P.Y);
            end;
          WM_RBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssRight], P.X, P.Y);
              Include(FClicked, mbRight);
            end;
          WM_RBUTTONUP:
            begin
              Shift := GetShiftState + [ssRight];
              GetCursorPos(P);
              if mbRight in FClicked then
              begin
                Exclude(FClicked, mbRight);
                DoClick(mbRight, Shift, P.X, P.Y);
              end;
              MouseUp(mbRight, Shift, P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              MouseUp(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
        end;
      end
      else
        Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

{===============================================================================
Component 1.0 Copyright © 1997 Andrey Abakumov (aga@oficina.rnd.su)

Thank for idea and text of a component
 Eric Lawrence
 Lead Programmer
 Delta Programming Group
 deltagrp@juno.com or deltagrp@keynetcorp.net
This is generally intended to be used with one of the TASKTRAY components.

Component is inherited from RXTrayIconBase, but add one
the very useful thing - is possible to remove display
of a task of taskbar

Set the property HideForm for removals of a task with
taskbar
===============================================================================}

constructor TRxTrayIconEx.Create(AOwner: TComponent);
var
  i: Word;
  CompCount: Byte;
begin
  inherited Create(AOwner);
  FHideForm := True;
  NewWndProc := nil;
  OldWndProc := nil;
  CompCount := 0; { Initialise Component Count to zero }
  { If we are designing at present }
  if (csDesigning in ComponentState) then
    if (AOwner is TForm) then
      with (AOwner as TForm) do
      begin
        for i := 0 to ComponentCount - 1 do { Check if there is already one of us! }
          if Components[i] is TRxTrayIconEx then Inc(CompCount);

        if CompCount > 1 then
          raise TDuplicateComponent.Create('There is already a TRxTrayIconEx component on this Form');
      end
    else
      raise TFormNotOwner.Create('The owner of TRxTrayIconEx component is not a TForm');
  HookParent;
end;

destructor TRxTrayIconEx.Destroy;
begin
  UnhookParent;
  inherited Destroy;
end;

procedure TRxTrayIconEx.Loaded;
begin
  inherited Loaded; { Always call inherited Loaded method }
  if not (csDesigning in ComponentState) then ProcessEnabled;
end;

procedure TRxTrayIconEx.SetHideForm(Value: Boolean);
begin
  if Value <> FHideForm then
  begin
    FHideForm := Value;
    ProcessEnabled;
  end;
end;

procedure TRxTrayIconEx.ProcessEnabled;
begin
  if FHideform then
    ShowWindow(FindWindow(nil, @Application.Title[1]), sw_hide)
  else
    ShowWindow(FindWindow(nil, @Application.Title[1]), sw_restore);
end;

procedure TRxTrayIconEx.HookParent;
begin
  if owner = nil then Exit;
  OldWndProc := TFarProc(GetWindowLong((owner as TForm).Handle, GWL_WNDPROC));
  NewWndProc := {$IFDEF RX_D11}Classes.{$ENDIF}MakeObjectInstance(HookWndProc);
  SetWindowLong((owner as TForm).Handle, GWL_WNDPROC, {$IFDEF RX_D16}LONG_PTR{$ELSE}Longint{$ENDIF}(NewWndProc));
end;

procedure TRxTrayIconEx.UnhookParent;
begin
  if (owner <> nil) and Assigned(OldWndProc) then
    SetWindowLong((owner as TForm).Handle, GWL_WNDPROC, {$IFDEF RX_D16}LONG_PTR{$ELSE}Longint{$ENDIF}(OldWndProc));
  if Assigned(NewWndProc) then
    {$IFDEF RX_D11}Classes.{$ENDIF}FreeObjectInstance(NewWndProc);
  NewWndProc := nil;
  OldWndProc := nil;
end;

procedure TRxTrayIconEx.HookWndProc(var Message: TMessage);
begin
  if owner = nil then Exit;
  if (Message.msg = wm_showwindow) then
  begin
    if (Message.wparam <> 0) then ProcessEnabled;
  end;
  Message.Result := CallWindowProc(OldWndProc, (owner as TForm).Handle, Message.Msg, Message.wParam, Message.lParam);
end;

{  TRxShellLinker  }

function TRxShellLinker.Load: Boolean;
var
  Obj: IUnknown;
  wfn: WideString;
  fd: TWin32FindData;
  I: Integer;
begin
  Result := False;
  if not FileExists(FLnkFile) then
    Exit;
  Obj := CreateComObject(CLSID_ShellLink);
  wfn := FLnkFile;
  if (Obj as IPersistFile).Load(PWideChar(wfn), STGM_READ) <> S_OK then
    Exit;
  with Obj as IShellLink do
  begin
    // get command line
    SetLength(FCmdLine, MAX_PATH);
    if GetPath(PChar(FCmdLine), Length(FCmdLine), fd, 0) <> NOERROR then
      Exit;
    FCmdLine := string(PChar(FCmdLine));
    // get arguments
    SetLength(FArg, MAX_PATH);
    if GetArguments(PChar(FArg), Length(FArg)) <> NOERROR then
      Exit;
    FArg := string(PChar(FArg));
    // get working directory
    SetLength(FWorkDir, MAX_PATH);
    if GetWorkingDirectory(PChar(FWorkDir), Length(FWorkDir)) <> NOERROR then
      Exit;
    FWorkDir := string(PChar(FWorkDir));
    // get show command
    if GetShowCmd(I) <> NOERROR then
      Exit;
    case I of
      SW_SHOWMAXIMIZED:
        FShowCmd := scMaximized;
      SW_SHOWMINNOACTIVE:
        FShowCmd := scMinimized;
    else
      FShowCmd := scNormal;
    end;
    // get hot key
    if GetHotKey(FHotKey) <> NOERROR then
      Exit;
    // get description
    SetLength(FDescr, MAX_PATH);
    if GetDescription(PChar(FDescr), Length(FDescr)) <> NOERROR then
      Exit;
    FDescr := string(PChar(FDescr));
    // get icon location
    SetLength(FIconFile, MAX_PATH);
    if GetIconLocation(PChar(FIconFile), Length(FIconFile), FIconIndex) <> NOERROR then
      Exit;
    FIconFile := string(PChar(FIconFile));
  end;
  Result := True;
end;

function TRxShellLinker.Save: Boolean;
var
  Obj: IUnknown;
  wfn: WideString;
  I: Integer;
begin
  Result := False;
  Obj := CreateComObject(CLSID_ShellLink);
  with Obj as IShellLink do
  begin
    // set command line
    if SetPath(PChar(ExtractQuotedString(FCmdLine, '"'))) <> NOERROR then
      Exit;
    // set arguments
    if SetArguments(PChar(FArg)) <> NOERROR then
      Exit;
    // set working directory
    if SetWorkingDirectory(PChar(ExtractQuotedString(FWorkDir, '"'))) <> NOERROR then
      Exit;
    // set show command
    case FShowCmd of
      scMaximized:
        I := SW_SHOWMAXIMIZED;
      scMinimized:
        I := SW_SHOWMINNOACTIVE;
    else
      I := SW_SHOWNORMAL;
    end;
    if SetShowCmd(I) <> NOERROR then
      Exit;
    // set hot key
    if SetHotKey(FHotKey) <> NOERROR then
      Exit;
    // set description
    if SetDescription(PChar(FDescr)) <> NOERROR then
      Exit;
    // set icon location
    if SetIconLocation(PChar(FIconFile), FIconIndex) <> NOERROR then
      Exit;
  end;
  wfn := FLnkFile;
  if (Obj as IPersistFile).Save(PWideChar(wfn), False) <> S_OK then
    Exit;
  Result := True;
end;

function TRxShellLinker.LoadFromFile(FileName: string): Boolean;
begin
  FLnkFile := FileName;
  Result := Load;
end;

function TRxShellLinker.SaveToFile(FileName: string): Boolean;
begin
  FLnkFile := FileName;
  Result := Save;
end;

function CreateShellLink(const LinkFile, CmdLine, Args, WorkDir: string;
  ShowCmd: TShowCmd): Boolean;
var
  Lnk: TRxShellLinker;
begin
  Lnk := TRxShellLinker.Create;
  try
    Lnk.CmdLine := CmdLine;
    Lnk.Arguments := Args;
    Lnk.WorkDir := WorkDir;
    Lnk.ShowCmd := ShowCmd;
    Result := Lnk.SaveToFile(LinkFile);
  finally
    Lnk.Free;
  end;
end;

initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
end.