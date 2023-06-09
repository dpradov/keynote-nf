{*******************************************************}
{                                                       }
{       Delphi VCL Extensions (RX)                      }
{                                                       }
{       Copyright (c) 1997, 1998 Master-Bank            }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxExcptDlg;

{$I RX.INC}
{$IFDEF RX_D6}
{$WARN SYMBOL_PLATFORM OFF} // Polaris
{$ENDIF}

interface

uses
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF RX_D16}System.UITypes, {$ENDIF}
  StdCtrls, ExtCtrls, RxCtrls;

type
  TErrorEvent = procedure(Error: Exception; var Msg: string) of object;

  TRxErrorDialog = class(TForm)
    BasicPanel: TPanel;
    ErrorText: TLabel;
    IconPanel: TPanel;
    IconImage: TImage;
    TopPanel: TPanel;
    RightPanel: TPanel;
    DetailsPanel: TPanel;
    MessageText: TMemo;
    ErrorAddress: TEdit;
    ErrorType: TEdit;
    ButtonPanel: TPanel;
    DetailsBtn: TButton;
    OKBtn: TButton;
    AddrLabel: TRxLabel;
    TypeLabel: TRxLabel;
    BottomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure ErrorInfo(var LogicalAddress: Pointer; var ModuleName: string);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    Details: Boolean;
    DetailsHeight: Integer;
    ExceptObj: Exception;
    FPrevOnException: TExceptionEvent;
    FOnErrorMsg: TErrorEvent;
    {$IFNDEF VER80}
    FHelpFile: string;
    {$ENDIF}
    procedure GetErrorMsg(var Msg: string);
    procedure ShowError;
    procedure SetShowDetails(Value: Boolean);
    {$IFNDEF VER80}
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    {$ENDIF}
  public
    procedure ShowException(Sender: TObject; E: Exception);
    property OnErrorMsg: TErrorEvent read FOnErrorMsg write FOnErrorMsg;
  end;

const
  ErrorDlgHelpCtx: THelpContext = 0;

var
  RxErrorDialog: TRxErrorDialog;

procedure RxErrorIntercept;

implementation

uses
  {$IFNDEF VER80}
  Windows, {$IFDEF RX_D3}ComObj, {$ELSE}OleAuto, {$ENDIF RX_D3}
  {$ELSE}
  WinProcs, WinTypes, ToolHelp, RxStr16,
  {$ENDIF}
  Consts, RxResConst, RxVCLUtils, RxStrUtils; // Polaris

{$R *.DFM}

{$IFDEF RX_D3}
resourcestring
  {$ELSE}
const
  {$ENDIF}
  SCodeError = '%s.'#13#10'Error Code: %.8x (%1:d).';
  SModuleError = 'Exception in module %s.'#13#10'%s';

const
  CRLF = #13#10;

procedure RxErrorIntercept;
begin
  if RxErrorDialog <> nil then RxErrorDialog.Free;
  RxErrorDialog := TRxErrorDialog.Create(Application);
end;

{ TRxErrorDialog }

procedure TRxErrorDialog.ShowException(Sender: TObject; E: Exception);
begin
  Screen.Cursor := crDefault;
  Application.NormalizeTopMosts;
  try
    if Assigned(FPrevOnException) then
      FPrevOnException(Sender, E)
    else if (ExceptObj = nil) and not Application.Terminated then
    begin
      ExceptObj := E;
      try
        ShowModal;
      finally
        ExceptObj := nil;
      end;
    end
    else
    begin
      if NewStyleControls then
        Application.ShowException(E)
      else
        MessageDlg(E.Message + '.', mtError, [mbOk], 0);
    end;
  except
    { ignore any exceptions }
  end;
  Application.RestoreTopMosts;
end;

{$IFNDEF VER80}

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        TEST    EAX,EAX
        JE      @@1
        SUB     EAX, $1000
@@1:
end;

procedure TRxErrorDialog.ErrorInfo(var LogicalAddress: Pointer;
  var ModuleName: string);
var
  Info: TMemoryBasicInformation;
  Temp, ModName: array[0..MAX_PATH] of Char;
begin
  VirtualQuery(ExceptAddr, Info, SizeOf(Info));
  if (Info.State <> MEM_COMMIT) or
    (GetModuleFilename(THandle(Info.AllocationBase), Temp, {$IFDEF UNICODE}Length(Temp){$ELSE}SizeOf(Temp){$ENDIF}) = 0) then
  begin
    GetModuleFileName(HInstance, Temp, {$IFDEF UNICODE}Length(Temp){$ELSE}SizeOf(Temp){$ENDIF});
    LogicalAddress := ConvertAddr(LogicalAddress);
  end
  else
    {$IFDEF WIN64}NativeInt{$ELSE}Integer{$ENDIF}(LogicalAddress) := {$IFDEF WIN64}NativeInt{$ELSE}Integer{$ENDIF}(LogicalAddress) -
    Integer(Info.AllocationBase);
  {$IFDEF RX_D3}
  StrLCopy(ModName, AnsiStrRScan(Temp, {$IFDEF RX_D6}PathDelim{$ELSE} '\'{$ENDIF}) + 1, SizeOf(ModName) - 1);
  {$ELSE}
  StrLCopy(ModName, StrRScan(Temp, PathDelim) + 1, SizeOf(ModName) - 1);
  {$ENDIF}
  ModuleName := StrPas(ModName);
end;

{$ELSE}

function ConvertAddr(Address: Pointer): Pointer; assembler;
asm
        MOV     AX,Address.Word[0]
        MOV     DX,Address.Word[2]
        MOV     CX,DX
        OR      CX,AX
        JE      @@1
        CMP     DX,0FFFFH
        JE      @@1
        MOV     ES,DX
        MOV     DX,ES:Word[0]
@@1:
end;

procedure TRxErrorDialog.ErrorInfo(var LogicalAddress: Pointer;
  var ModuleName: string);
var
  GlobalEntry: TGlobalEntry;
  hMod: THandle;
  ModName: array[0..15] of Char;
  Buffer: array[0..MAX_PATH] of Char;
begin
  GlobalEntry.dwSize := SizeOf(GlobalEntry);
  if GlobalEntryHandle(@GlobalEntry, THandle(PtrRec(LogicalAddress).Seg)) then
    with GlobalEntry do
    begin
      hMod := hOwner;
      if wType in [GT_CODE, GT_DATA, GT_DGROUP] then
        PtrRec(LogicalAddress).Seg := wData;
    end
  else
    LogicalAddress := ConvertAddr(LogicalAddress);
  GetModuleFileName(hMod, Buffer, {$IFDEF UNICODE}Length(Buffer){$ELSE}SizeOf(Buffer) - 1{$ENDIF});
  StrLCopy(ModName, StrRScan(Buffer, PathDelim) + 1, SizeOf(ModName) - 1);
  ModuleName := StrPas(ModName);
end;

{$ENDIF}

procedure TRxErrorDialog.ShowError;
var
  S, ModuleName: string;
  P: Pointer;
begin
  P := ExceptAddr;
  ModuleName := '';
  ErrorInfo(P, ModuleName);
  AddrLabel.Enabled := (P <> nil);
  ErrorAddress.Text := Format('%p', [ExceptAddr]);
  ErrorType.Text := ExceptObj.ClassName;
  TypeLabel.Enabled := ErrorType.Text <> '';
  S := Trim(ExceptObj.Message);
  if Pos(CRLF, S) = 0 then
    S := ReplaceStr(S, #10, CRLF);
  if ExceptObj is EInOutError then
    S := Format(SCodeError, [S, EInOutError(ExceptObj).ErrorCode])
    {$IFNDEF VER80}
  else if ExceptObj is EOleException then
  begin
    with EOleException(ExceptObj) do
      if (Source <> '') and (AnsiCompareText(S, Trim(Source)) <> 0) then
        S := S + CRLF + Trim(Source);
    S := Format(SCodeError, [S, EOleException(ExceptObj).ErrorCode])
  end
  else if ExceptObj is EOleSysError then
    S := Format(SCodeError, [S, EOleSysError(ExceptObj).ErrorCode])
  else if ExceptObj is EExternalException then
    S := Format(SCodeError, [S,
      EExternalException(ExceptObj).ExceptionRecord^.ExceptionCode])
      {$ENDIF}
    {$IFDEF RX_D3}
    {$IFDEF RX_D6} // Polaris
  else if ExceptObj is EOSError then
    S := Format(SCodeError, [S, EOSError(ExceptObj).ErrorCode])
    {$ELSE}
  else if ExceptObj is EWin32Error then
    S := Format(SCodeError, [S, EWin32Error(ExceptObj).ErrorCode])
    {$ENDIF}
    {$ENDIF}
  else
    S := S + '.';
  MessageText.Text := Format(SModuleError, [ModuleName, S]);
end;

procedure TRxErrorDialog.SetShowDetails(Value: Boolean);
begin
  DisableAlign;
  try
    if Value then
    begin
      DetailsPanel.Height := DetailsHeight;
      ClientHeight := DetailsPanel.Height + BasicPanel.Height;
      DetailsBtn.Caption := '<< &' + RxLoadStr(SDetails);
      ShowError;
    end
    else
    begin
      ClientHeight := BasicPanel.Height;
      DetailsPanel.Height := 0;
      DetailsBtn.Caption := '&' + RxLoadStr(SDetails) + ' >>';
    end;
    DetailsPanel.Enabled := Value;
    Details := Value;
  finally
    EnableAlign;
  end;
end;

procedure TRxErrorDialog.GetErrorMsg(var Msg: string);
var
  I: Integer;
begin
  I := Pos(CRLF, Msg);
  if I > 0 then System.Delete(Msg, I, MaxInt);
  if Assigned(FOnErrorMsg) then
  try
    FOnErrorMsg(ExceptObj, Msg);
  except
  end;
end;

{$IFNDEF VER80}

procedure TRxErrorDialog.WMHelp(var Message: TWMHelp);
var
  AppHelpFile: string;
begin
  AppHelpFile := Application.HelpFile;
  try
    if FHelpFile <> '' then
      Application.HelpFile := FHelpFile;
    inherited;
  finally
    Application.HelpFile := AppHelpFile;
  end;
end;
{$ENDIF}

procedure TRxErrorDialog.FormCreate(Sender: TObject);
begin
  {$IFNDEF VER80}
  BorderIcons := [biSystemMenu, biHelp];
  {$ELSE}
  BorderIcons := [];
  {$ENDIF}
  DetailsHeight := DetailsPanel.Height;
  Icon.Handle := LoadIcon(0, IDI_HAND);
  IconImage.Picture.Icon := Icon;
  { Load string resources }
  Caption := ResStr(SMsgDlgError);
  OKBtn.Caption := ResStr(SOKButton);
//  AddrLabel.Caption := RxLoadStr();
//  TypeLabel.Caption := RxLoadStr();

  { Set exception handler }
  FPrevOnException := Application.OnException;
  Application.OnException := ShowException;
end;

procedure TRxErrorDialog.FormDestroy(Sender: TObject);
begin
  Application.OnException := FPrevOnException;
end;

procedure TRxErrorDialog.FormShow(Sender: TObject);
var
  S: string;
  {$IFNDEF VER80}
  ExStyle: LongInt;
  {$ENDIF}
begin
  if ExceptObj.HelpContext <> 0 then
    HelpContext := ExceptObj.HelpContext
  else
    HelpContext := ErrorDlgHelpCtx;
  {$IFNDEF VER80}
  if ExceptObj is EOleException then
    FHelpFile := EOleException(ExceptObj).HelpFile
  else
    FHelpFile := '';
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  if (HelpContext <> 0) then
    ExStyle := ExStyle or WS_EX_CONTEXTHELP
  else
    ExStyle := ExStyle and not WS_EX_CONTEXTHELP;
  SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
  {$ENDIF}
  S := Trim(ExceptObj.Message) + '.';
  GetErrorMsg(S);
  ErrorText.Caption := S;
  SetShowDetails(False);
  DetailsBtn.Enabled := True;
end;

procedure TRxErrorDialog.DetailsBtnClick(Sender: TObject);
begin
  SetShowDetails(not Details);
end;

procedure TRxErrorDialog.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{$IFNDEF VER80}
var
  Info: THelpInfo;
  {$ENDIF}
begin
  if (Key = VK_F1) and (HelpContext <> 0) then
  begin
    {$IFNDEF VER80}
    with Info do
    begin
      cbSize := SizeOf(THelpInfo);
      iContextType := HELPINFO_WINDOW;
      iCtrlId := 0;
      hItemHandle := Handle;
      dwContextId := HelpContext;
      GetCursorPos(MousePos);
    end;
    Perform(WM_HELP, 0, LPARAM(@Info));
    {$ELSE}
    Application.HelpContext(HelpContext);
    {$ENDIF}
  end;
end;

initialization
  RxErrorDialog := nil;
end.
