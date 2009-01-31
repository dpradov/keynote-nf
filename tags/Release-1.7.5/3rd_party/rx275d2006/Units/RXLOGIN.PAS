{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit RxLogin;

{$I RX.INC}

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} SysUtils,
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type
  TUpdateCaption = (ucNoChange, ucAppTitle, ucFormCaption);
  TRxLoginEvent = procedure(Sender: TObject; const UserName, Password: string;
    var AllowLogin: Boolean) of object;
  TCheckUnlockEvent = function(const Password: string): Boolean of object;
  TUnlockAppEvent = procedure(Sender: TObject; const UserName,
    Password: string; var AllowUnlock: Boolean) of object;

  TRxLoginForm = class;

{ TRxCustomLogin }

  TRxCustomLogin = class(TComponent)
  private
    FActive: Boolean;
    FAttemptNumber: Integer;
    FLoggedUser: String;
    FMaxPasswordLen: Integer;
    FAllowEmpty: Boolean;
    FUpdateCaption: TUpdateCaption;
    FIniFileName: String;
    FUseRegistry: Boolean;
    FLocked: Boolean;
    FUnlockDlgShowing: Boolean;
    FSaveOnRestore: TNotifyEvent;
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FOnUnlock: TCheckUnlockEvent;
    FOnUnlockApp: TUnlockAppEvent;
    FOnIconDblClick: TNotifyEvent;
    function GetLoggedUser: string;
    function GetIniFileName: string;
    procedure SetIniFileName(const Value: string);
    function UnlockHook(var Message: TMessage): Boolean;
  protected
    function CheckUnlock(const UserName, Password: string): Boolean; dynamic;
    function CreateLoginForm(UnlockMode: Boolean): TRxLoginForm; virtual;
    procedure DoAfterLogin; dynamic;
    procedure DoBeforeLogin; dynamic;
    procedure DoIconDblCLick(Sender: TObject); dynamic;
    function DoLogin(var UserName: string): Boolean; virtual; abstract;
    function DoUnlockDialog: Boolean; virtual;
    procedure SetLoggedUser(const Value: string);
    procedure DoUpdateCaption;
    procedure UnlockOkClick(Sender: TObject);
    property Active: Boolean read FActive write FActive default True;
    property AllowEmptyPassword: Boolean read FAllowEmpty write FAllowEmpty default True;
    property AttemptNumber: Integer read FAttemptNumber write FAttemptNumber default 3;
    property IniFileName: string read GetIniFileName write SetIniFileName;
    property MaxPasswordLen: Integer read FMaxPasswordLen write FMaxPasswordLen default 0;
    property UpdateCaption: TUpdateCaption read FUpdateCaption write FUpdateCaption default ucNoChange;
    property UseRegistry: Boolean read FUseRegistry write FUseRegistry default False;
    property AfterLogin: TNotifyEvent read FAfterLogin write FAfterLogin;
    property BeforeLogin: TNotifyEvent read FBeforeLogin write FBeforeLogin;
    property OnUnlock: TCheckUnlockEvent read FOnUnlock write FOnUnlock; { obsolete }
    property OnUnlockApp: TUnlockAppEvent read FOnUnlockApp write FOnUnlockApp;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Login: Boolean; virtual;
    procedure TerminateApplication;
    procedure Lock;
    property LoggedUser: string read GetLoggedUser;
  end;

{ TRxLoginDialog }

  TRxLoginDialog = class(TRxCustomLogin)
  private
    FOnCheckUser: TRxLoginEvent;
    procedure OkButtonClick(Sender: TObject);
    procedure WriteUserName(const UserName: string);
    function ReadUserName(const UserName: string): string;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
  published
    property Active;
    property AttemptNumber;
    property IniFileName;
    property MaxPasswordLen;
    property UpdateCaption;
    property UseRegistry;
    property OnCheckUser: TRxLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

{ TRxLoginForm }

  TRxLoginForm = class(TForm)
    AppIcon: TImage;
    KeyImage: TImage;
    HintLabel: TLabel;
    UserNameLabel: TLabel;
    PasswordLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    AppTitleLabel: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    CustomLabel: TLabel;
    CustomCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSelectDatabase: Boolean;
    FUnlockMode: Boolean;
    FAttempt: Integer;
    FOnFormShow: TNotifyEvent;
    FOnOkClick: TNotifyEvent;
  public
    { Public declarations }
    AttemptNumber: Integer;
    property Attempt: Integer read FAttempt;
    property SelectDatabase: Boolean read FSelectDatabase write FSelectDatabase;
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnOkClick: TNotifyEvent read FOnOkClick write FOnOkClick;
  end;

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TRxLoginForm;

implementation

uses {$IFDEF WIN32} Registry, {$ENDIF} IniFiles, AppUtils, RxDConst,
  Consts, VclUtils, RxConst;

{$R *.DFM}

const
  keyLoginSection  = 'Login Dialog';
  keyLastLoginUserName = 'Last Logged User';

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
  FormShowEvent, OkClickEvent: TNotifyEvent): TRxLoginForm;
begin
  Result := TRxLoginForm.Create(Application);
  with Result do begin
    FSelectDatabase := ASelectDatabase;
    FUnlockMode := UnlockMode;
    if FUnlockMode then begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else begin
      FormStyle := fsStayOnTop;
    end;
    OnFormShow := FormShowEvent;
    OnOkClick := OkClickEvent;
  end;
end;

{ TRxCustomLogin }

constructor TRxCustomLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniFileName := EmptyStr;
  FLoggedUser := EmptyStr;
  FActive := True;
  FAttemptNumber := 3;
  FAllowEmpty := True;
  FUseRegistry := False;
end;

destructor TRxCustomLogin.Destroy;
begin
  if FLocked then begin
    Application.UnhookMainWindow(UnlockHook);
    FLocked := False;
  end;
  //DisposeStr(FLoggedUser);
  //DisposeStr(FIniFileName);
  inherited Destroy;
end;

function TRxCustomLogin.GetIniFileName: string;
begin
  Result := FIniFileName;
  if (Result = '') and not (csDesigning in ComponentState) then begin
{$IFDEF WIN32}
    if UseRegistry then Result := GetDefaultIniRegKey
    else Result := GetDefaultIniName;
{$ELSE}
    Result := GetDefaultIniName;
{$ENDIF}
  end;
end;

procedure TRxCustomLogin.SetIniFileName(const Value: string);
begin
  FIniFileName := Value;
end;

function TRxCustomLogin.GetLoggedUser: string;
begin
  Result := FLoggedUser;
end;

procedure TRxCustomLogin.SetLoggedUser(const Value: string);
begin
  FLoggedUser := Value;
end;

procedure TRxCustomLogin.DoAfterLogin;
begin
  if Assigned(FAfterLogin) then FAfterLogin(Self);
end;

procedure TRxCustomLogin.DoBeforeLogin;
begin
  if Assigned(FBeforeLogin) then FBeforeLogin(Self);
end;

procedure TRxCustomLogin.DoIconDblCLick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then FOnIconDblClick(Self);
end;

procedure TRxCustomLogin.DoUpdateCaption;
var
  F: TForm;
begin
  F := Application.MainForm;
  if (F = nil) and (Owner is TForm) then F := Owner as TForm;
  if (F <> nil) and (LoggedUser <> '') then
    case UpdateCaption of
      ucAppTitle:
        F.Caption := Format('%s (%s)', [Application.Title, LoggedUser]);
      ucFormCaption:
        begin
          F.Caption := Format('%s (%s)', [F.Caption, LoggedUser]);
          UpdateCaption := ucNoChange;
        end;
    end;
end;

function TRxCustomLogin.Login: Boolean;
var
  LoginName: string;
begin
  LoginName := EmptyStr;
  DoBeforeLogin;
  Result := DoLogin(LoginName);
  if Result then begin
    SetLoggedUser(LoginName);
    DoUpdateCaption;
    DoAfterLogin;
  end;
end;

procedure TRxCustomLogin.Lock;
begin
  FSaveOnRestore := Application.OnRestore;
  Application.Minimize;
  Application.HookMainWindow(UnlockHook);
  FLocked := True;
end;

procedure TRxCustomLogin.TerminateApplication;
begin
  with Application do begin
{$IFDEF WIN32}
    ShowMainForm := False;
{$ENDIF}
    if Handle <> 0 then ShowOwnedPopups(Handle, False);
    Terminate;
  end;
{$IFDEF RX_D3}
  CallTerminateProcs;
{$ENDIF}
{$IFNDEF RX_D3}
  Halt(10);
{$ENDIF}
end;

procedure TRxCustomLogin.UnlockOkClick(Sender: TObject);
var
  Ok: Boolean;
begin
  with TRxLoginForm(Sender) do begin
    Ok := False;
    try
      Ok := CheckUnlock(UserNameEdit.Text, PasswordEdit.Text);
    except
      Application.HandleException(Self);
    end;
    if Ok then ModalResult := mrOk
    else ModalResult := mrCancel;
  end;
end;

function TRxCustomLogin.CheckUnlock(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnUnlockApp) then
    FOnUnlockApp(Self, UserName, Password, Result)
  else if Assigned(FOnUnlock) then
    Result := FOnUnlock(Password);
end;

function TRxCustomLogin.CreateLoginForm(UnlockMode: Boolean): TRxLoginForm;
begin
  Result := TRxLoginForm.Create(Application);
  with Result do begin
    FUnlockMode := UnlockMode;
    if FUnlockMode then begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else FormStyle := fsStayOnTop;
    if Assigned(Self.FOnIconDblClick) then begin
      with AppIcon do begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
      with KeyImage do begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
    end;
    PasswordEdit.MaxLength := FMaxPasswordLen;
    AttemptNumber := Self.AttemptNumber;
  end;
end;

function TRxCustomLogin.DoUnlockDialog: Boolean;
begin
  with CreateLoginForm(True) do
  try
    OnFormShow := nil;
    OnOkClick := UnlockOkClick;
    with UserNameEdit do begin
      Text := LoggedUser;
      ReadOnly := True;
      Font.Color := clGrayText;
    end;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

function TRxCustomLogin.UnlockHook(var Message: TMessage): Boolean;

  function DoUnlock: Boolean;
  var
    Popup: HWnd;
  begin
    with Application do
      if IsWindowVisible(Handle) and IsWindowEnabled(Handle) then
{$IFDEF WIN32}
        SetForegroundWindow(Handle);
{$ELSE}
        BringWindowToTop(Handle);
{$ENDIF}
    if FUnlockDlgShowing then begin
      Popup := GetLastActivePopup(Application.Handle);
      if (Popup <> 0) and IsWindowVisible(Popup) and
        (WindowClassName(Popup) = TRxLoginForm.ClassName) then
      begin
{$IFDEF WIN32}
        SetForegroundWindow(Popup);
{$ELSE}
        BringWindowToTop(Popup);
{$ENDIF}
      end;
      Result := False;
      Exit;
    end;
    FUnlockDlgShowing := True;
    try
      Result := DoUnlockDialog;
    finally
      FUnlockDlgShowing := False;
    end;
    if Result then begin
      Application.UnhookMainWindow(UnlockHook);
      FLocked := False;
    end;
  end;

begin
  Result := False;
  if not FLocked then Exit;
  with Message do begin
    case Msg of
      WM_QUERYOPEN:
        begin
          UnlockHook := not DoUnlock;
        end;
      WM_SHOWWINDOW:
        if Bool(WParam) then begin
          UnlockHook := not DoUnlock;
        end;
      WM_SYSCOMMAND:
        if (WParam and $FFF0 = SC_RESTORE) or
          (WParam and $FFF0 = SC_ZOOM) then
        begin
          UnlockHook := not DoUnlock;
        end;
    end;
  end;
end;

{ TRxLoginDialog }

procedure TRxLoginDialog.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then begin
    if Active and not Login then
      TerminateApplication;
  end;
end;

procedure TRxLoginDialog.OkButtonClick(Sender: TObject);
var
  SetCursor: Boolean;
begin
  with TRxLoginForm(Sender) do begin
{$IFDEF WIN32}
    SetCursor := GetCurrentThreadID = MainThreadID;
{$ELSE}
    SetCursor := True;
{$ENDIF}
    try
      if SetCursor then Screen.Cursor := crHourGlass;
      try
        if DoCheckUser(UserNameEdit.Text, PasswordEdit.Text) then
          ModalResult := mrOk
        else ModalResult := mrNone;
      finally
        if SetCursor then Screen.Cursor := crDefault;
      end;
    except
      Application.HandleException(Self);
    end;
  end;
end;

function TRxLoginDialog.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TRxLoginDialog.WriteUserName(const UserName: string);
var
  Ini: TObject;
begin
  try
{$IFDEF WIN32}
    if UseRegistry then Ini := TRegIniFile.Create(IniFileName)
    else Ini := TIniFile.Create(IniFileName);
{$ELSE}
    Ini := TIniFile.Create(IniFileName);
{$ENDIF}
    try
      IniWriteString(Ini, keyLoginSection, keyLastLoginUserName, UserName);
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TRxLoginDialog.ReadUserName(const UserName: string): string;
var
  Ini: TObject;
begin
  try
{$IFDEF WIN32}
    if UseRegistry then begin
      Ini := TRegIniFile.Create(IniFileName);
{$IFDEF RX_D5}
      TRegIniFile(Ini).Access := KEY_READ;
{$ENDIF}
    end
    else 
      Ini := TIniFile.Create(IniFileName);
{$ELSE}
    Ini := TIniFile.Create(IniFileName);
{$ENDIF}
    try
      Result := IniReadString(Ini, keyLoginSection, keyLastLoginUserName,
        UserName);
    finally
      Ini.Free;
    end;
  except
    Result := UserName;
  end;
end;

function TRxLoginDialog.DoLogin(var UserName: string): Boolean;
begin
  try
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      UserName := ReadUserName(UserName);
      UserNameEdit.Text := UserName;
      Result := (ShowModal = mrOk);
      if Result then begin
        UserName := UserNameEdit.Text;
        WriteUserName(UserName);
      end;
    finally
      Free;
    end;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

{ TRxLoginForm }

procedure TRxLoginForm.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  if Icon.Empty then Icon.Handle := LoadIcon(0, IDI_APPLICATION);
  AppIcon.Picture.Assign(Icon);
  AppTitleLabel.Caption := FmtLoadStr(SAppTitleLabel, [Application.Title]);
  PasswordLabel.Caption := LoadStr(SPasswordLabel);
  UserNameLabel.Caption := LoadStr(SUserNameLabel);
  OkBtn.Caption := ResStr(SOKButton);
  CancelBtn.Caption := ResStr(SCancelButton);
end;

procedure TRxLoginForm.OkBtnClick(Sender: TObject);
begin
  Inc(FAttempt);
  if Assigned(FOnOkClick) then FOnOkClick(Self)
  else ModalResult := mrOk;
  if (ModalResult <> mrOk) and (FAttempt >= AttemptNumber) then
    ModalResult := mrCancel;
end;

procedure TRxLoginForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if FSelectDatabase then begin
    ClientHeight := CustomCombo.Top + PasswordEdit.Top - UserNameEdit.Top;
    S := LoadStr(SDatabaseName);
    I := Pos(':', S);
    if I = 0 then I := Length(S);
    CustomLabel.Caption := '&' + Copy(S, 1, I);
  end
  else begin
    ClientHeight := PasswordEdit.Top + PasswordEdit.Top - UserNameEdit.Top;
    CustomLabel.Visible := False;
    CustomCombo.Visible := False;
  end;
  if not FUnlockMode then begin
    HintLabel.Caption := LoadStr(SHintLabel);
    Caption := LoadStr(SRegistration);
  end
  else begin
    HintLabel.Caption := LoadStr(SUnlockHint);
    Caption := LoadStr(SUnlockCaption);
  end;
  if (UserNameEdit.Text = EmptyStr) and not FUnlockMode then
    ActiveControl := UserNameEdit
  else 
    ActiveControl := PasswordEdit;
  if Assigned(FOnFormShow) then FOnFormShow(Self);
  FAttempt := 0;
end;

end.