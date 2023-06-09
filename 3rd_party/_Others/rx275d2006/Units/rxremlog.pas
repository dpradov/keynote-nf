{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxRemLog;

interface

{$I RX.INC}

{$IFDEF RX_D3}
{$IFDEF RX_MIDAS}

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, RxLogin, DBClient;

{ TRxRemoteLogin }

type
  TRxRemoteLogin = class(TRxCustomLogin)
  private
    FRemoteServer: TCustomRemoteServer;
    FPrepared: Boolean;
    FUsername: string;
    FInLogin: Boolean;
    FOnCheckUser: TRxLoginEvent;
    FSaveAfterConnect: TNotifyEvent;
    procedure AbortConnection;
    procedure OkButtonClick(Sender: TObject);
    procedure PrepareRemoteServer;
    procedure UnprepareRemoteServer;
    procedure ServerAfterConnect(Sender: TObject);
    procedure SetRemoteServer(Value: TCustomRemoteServer);
    procedure WriteUserName(const UserName: string);
    function ReadUserName(const UserName: string): string;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RemoteServer: TCustomRemoteServer read FRemoteServer write SetRemoteServer;
    property Active;
    property AllowEmptyPassword;
    property AttemptNumber;
    property IniFileName;
    property MaxPasswordLen;
    property UpdateCaption;
    property UseRegistry;
    property OnCheckUser: TRxLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlock;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

{$ENDIF RX_MIDAS}
{$ENDIF RX_D3}

implementation

{$IFDEF RX_D3}
{$IFDEF RX_MIDAS}

uses IniFiles, Registry, AppUtils, VclUtils {$IFDEF RX_D4}, MConnect {$ENDIF};

const
  keyLoginSection  = 'Remote Login';
  keyLastLoginUserName = 'Last User';

type
  TServer = class(TCustomRemoteServer);

{ TRxRemoteLogin }

constructor TRxRemoteLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TRxRemoteLogin.Destroy;
begin
  UnprepareRemoteServer;
  inherited Destroy;
end;

procedure TRxRemoteLogin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RemoteServer) then
    RemoteServer := nil;
end;

procedure TRxRemoteLogin.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading and
    Assigned(FRemoteServer) then
  begin
    if not Active then PrepareRemoteServer
    else if not Login then TerminateApplication;
  end;
end;

procedure TRxRemoteLogin.SetRemoteServer(Value: TCustomRemoteServer);
begin
  if FRemoteServer <> Value then begin
    UnprepareRemoteServer;
    FRemoteServer := Value;
    if Value <> nil then begin
      Value.FreeNotification(Self);
      if not (csLoading in ComponentState) then PrepareRemoteServer;
    end;
  end;
end;

procedure TRxRemoteLogin.PrepareRemoteServer;
begin
  if Assigned(FRemoteServer) and not FPrepared then
    with TServer(RemoteServer) do begin
{$IFDEF RX_D4}
      if RemoteServer is TDispatchConnection then
        TDispatchConnection(RemoteServer).LoginPrompt := False;
{$ENDIF}
      FSaveAfterConnect := AfterConnect;
      AfterConnect := ServerAfterConnect;
      FPrepared := True;
    end;
end;

procedure TRxRemoteLogin.UnprepareRemoteServer;
begin
  if Assigned(FRemoteServer) and FPrepared then
    with TServer(RemoteServer) do begin
      AfterConnect := FSaveAfterConnect;
      FPrepared := False;
    end;
end;

procedure TRxRemoteLogin.OkButtonClick(Sender: TObject);
var
  SetCursor: Boolean;
begin
  with TRxLoginForm(Sender) do begin
    SetCursor := GetCurrentThreadID = MainThreadID;
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

procedure TRxRemoteLogin.ServerAfterConnect(Sender: TObject);
{$IFDEF RX_D4}
var
  OnGetUser: TGetUsernameEvent;
{$ENDIF}
begin
  if Sender = FRemoteServer then begin
    if not FInLogin then DoBeforeLogin;
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      FUsername := ReadUserName(FUsername);
{$IFDEF RX_D4}
      if FRemoteServer is TDispatchConnection then begin
        OnGetUser := TDispatchConnection(FRemoteServer).OnGetUsername;
        if Assigned(OnGetUser) then OnGetUser(FRemoteServer, FUsername);
      end;
{$ENDIF}
      UserNameEdit.Text := FUsername;
      if ShowModal = mrOk then begin
        FUsername := UserNameEdit.Text;
        WriteUserName(FUsername);
        if not FInLogin then begin
          SetLoggedUser(FUsername);
          DoUpdateCaption;
          DoAfterLogin;
        end;
      end
      else begin
        AbortConnection;
        SysUtils.Abort;
      end;
    finally
      Free;
    end;
  end;
end;

function TRxRemoteLogin.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TRxRemoteLogin.WriteUserName(const UserName: string);
var
  Ini: TObject;
begin
  try
    if UseRegistry then Ini := TRegIniFile.Create(IniFileName)
    else Ini := TIniFile.Create(IniFileName);
    try
      IniWriteString(Ini, keyLoginSection, keyLastLoginUserName, UserName);
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TRxRemoteLogin.ReadUserName(const UserName: string): string;
var
  Ini: TObject;
begin
  try
    if UseRegistry then begin
      Ini := TRegIniFile.Create(IniFileName);
{$IFDEF RX_D5}
      TRegIniFile(Ini).Access := KEY_READ;
{$ENDIF}
    end
    else Ini := TIniFile.Create(IniFileName);
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

procedure TRxRemoteLogin.AbortConnection;
var
  OnAfterDisconnect, OnBeforeDisconnect: TNotifyEvent;
begin
  if Assigned(FRemoteServer) and TServer(FRemoteServer).Connected then
  try
    OnAfterDisconnect := TServer(FRemoteServer).AfterDisconnect;
    OnBeforeDisconnect := TServer(FRemoteServer).BeforeDisconnect;
    try
      TServer(FRemoteServer).AfterDisconnect := nil;
      TServer(FRemoteServer).BeforeDisconnect := nil;
      TServer(FRemoteServer).Connected := False;
    finally
      TServer(FRemoteServer).AfterDisconnect := OnAfterDisconnect;
      TServer(FRemoteServer).BeforeDisconnect := OnBeforeDisconnect;
    end;
  except
  end;
end;

function TRxRemoteLogin.DoLogin(var UserName: string): Boolean;
begin
  Result := False;
  if not Assigned(FRemoteServer) then Exit;
  PrepareRemoteServer;
  FUsername := UserName;
  try
    FInLogin := True;
    try
      TServer(FRemoteServer).Connected := True;
      Result := TServer(FRemoteServer).Connected;
      UserName := FUsername;
      FUsername := '';
    finally
      FInLogin := False;
    end;
  except
    Application.HandleException(Self);
    Result := False;
    FUsername := '';
    AbortConnection;
  end;
  if Result and Assigned(FSaveAfterConnect) then
    FSaveAfterConnect(FRemoteServer);
end;

{$ENDIF RX_MIDAS}
{$ENDIF RX_D3}

end.