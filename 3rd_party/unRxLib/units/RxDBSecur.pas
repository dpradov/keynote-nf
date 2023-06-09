{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDBSecur;

interface

{$I RX.INC}

uses
  SysUtils, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, DB, DBTables,
  RxLogin, RxLoginDlg, RxChPswDlg;

type
  TCheckUserEvent = function(UsersTable: TTable;
    const Password: string): Boolean of object;

{ TDBSecurity }

  TDBSecurity = class(TRxCustomLogin)
  private
    FDatabase: TDatabase;
    FUsersTableName: TFileName;
    {$IFDEF RX_D4} // Polaris
    FLoginNameField: string;
    {$ELSE}
    FLoginNameField: PString;
    {$ENDIF}
    FSelectAlias: Boolean;
    FOnCheckUser: TCheckUserEvent;
    FOnChangePassword: TChangePasswordEvent;
    procedure SetDatabase(Value: TDatabase);
    procedure SetUsersTableName(const Value: TFileName);
    function GetLoginNameField: string;
    procedure SetLoginNameField(const Value: string);
  protected
    function DoCheckUser(UsersTable: TTable; const UserName,
      Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ChangePassword: Boolean;
  published
    property Database: TDatabase read FDatabase write SetDatabase;
    property LoginNameField: string read GetLoginNameField write SetLoginNameField;
    property SelectAlias: Boolean read FSelectAlias write FSelectAlias default False;
    property UsersTableName: TFileName read FUsersTableName write SetUsersTableName;
    property Active;
    property AllowEmptyPassword;
    property AttemptNumber;
    property IniFileName;
    property MaxPasswordLen;
    property UpdateCaption;
    {$IFNDEF VER80}
    property UseRegistry;
    {$ENDIF}
    property OnCheckUser: TCheckUserEvent read FOnCheckUser write FOnCheckUser;
    property OnChangePassword: TChangePasswordEvent read FOnChangePassword
      write FOnChangePassword;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlock;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

implementation

uses RxAppUtils, RxVCLUtils; // Polaris

{ TDBSecurity }

constructor TDBSecurity.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectAlias := False;
  {$IFDEF RX_D4} // Polaris
  FLoginNameField := EmptyStr;
  {$ELSE}
  FLoginNameField := NullStr;
  {$ENDIF}
end;

destructor TDBSecurity.Destroy;
begin
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FLoginNameField);
  {$ENDIF}
  inherited Destroy;
end;

procedure TDBSecurity.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Database) then Database := nil;
end;

procedure TDBSecurity.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and Active and
    (Database <> nil) then
  begin
    Database.LoginPrompt := True;
    if not Login then
    begin
      TerminateApplication;
    end;
  end;
end;

procedure TDBSecurity.SetDatabase(Value: TDatabase);
begin
  if FDatabase <> Value then
  begin
    FDatabase := Value;
    {$IFNDEF VER80}
    if Value <> nil then Value.FreeNotification(Self);
    {$ENDIF}
  end;
end;

procedure TDBSecurity.SetUsersTableName(const Value: TFileName);
begin
  if FUsersTableName <> Value then FUsersTableName := Value;
end;

function TDBSecurity.GetLoginNameField: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FLoginNameField;
  {$ELSE}
  Result := FLoginNameField^;
  {$ENDIF}
end;

procedure TDBSecurity.SetLoginNameField(const Value: string);
begin
  {$IFDEF RX_D4} // Polaris
  FLoginNameField := Value;
  {$ELSE}
  AssignStr(FLoginNameField, Value);
  {$ENDIF}
end;

function TDBSecurity.DoCheckUser(UsersTable: TTable; const UserName,
  Password: string): Boolean;
var
  SaveLoggedUser: string;
begin
  if Assigned(FOnCheckUser) then
  begin
    SaveLoggedUser := LoggedUser;
    try
      SetLoggedUser(UserName);
      Result := FOnCheckUser(UsersTable, Password);
    finally
      SetLoggedUser(SaveLoggedUser);
    end;
  end
  else
    Result := True;
end;

function TDBSecurity.DoLogin(var UserName: string): Boolean;
var
  IconClick: TNotifyEvent;
begin
  IconClick := OnIconDblClick;
  if Assigned(IconClick) then IconClick := DoIconDblClick;
  Result := LoginDialog(Database, AttemptNumber, UsersTableName,
    LoginNameField, MaxPasswordLen, DoCheckUser, IconClick, UserName,
    IniFileName, UseRegistry, SelectAlias);
end;

function TDBSecurity.ChangePassword: Boolean;
begin
  Result := ChangePasswordDialog(Database, AttemptNumber, UsersTableName,
    LoginNameField, LoggedUser, MaxPasswordLen, AllowEmptyPassword,
    FOnChangePassword);
end;

end.