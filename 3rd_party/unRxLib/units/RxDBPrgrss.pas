{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxDBPrgrss;

interface

{$I RX.INC}
{$T-}

uses
  Classes, {$IFNDEF VER80} Bde, {$ELSE} DbiTypes, DbiProcs, {$ENDIF}
  {$IFDEF RX_D6}Types,{$ENDIF}
  {$IFDEF RX_D18}System.AnsiStrings,{$ENDIF}
  Controls, DB, DBTables, RxTimer {$IFDEF RX_D10}, DBCommonTypes{$ENDIF};

type
  TOnMessageChange = procedure(Sender: TObject; const Msg: string) of object;
  TOnPercentChange = procedure(Sender: TObject; PercentDone: Integer) of object;
  TOnProgressEvent = procedure(Sender: TObject; var AbortQuery: Boolean) of object;
{$IFNDEF VER80}
  TOnTraceEvent = procedure(Sender: TObject; Flag: TTraceFlag;
    const Msg: string) of object;
{$ENDIF}

{ TDBProgress }

  TDBProgress = class(TComponent)
  private
    FActive: Boolean;
    FStartTime: Longint;
    FTimer: TRxTimer;
    FWaitCursor: TCursor;
    FGauge: TControl;
    FMessageControl: TControl;
    FStreamedValue: Boolean;
    FGenProgressCallback: TObject;
    FQryProgressCallback: TObject;
    FOnMessageChange: TOnMessageChange;
    FOnPercentChange: TOnPercentChange;
    FOnProgress: TOnProgressEvent;
{$IFNDEF VER80}
    FTraceFlags: TTraceFlags;
    FTraceCallback: TObject;
    FTrace: Boolean;
    FOnTrace: TOnTraceEvent;
    FSessionName: string;
    FSessionLink: TObject;
    procedure SetTrace(Value: Boolean);
    procedure SetTraceFlags(Value: TTraceFlags);
    function TraceCallBack(CBInfo: Pointer): CBRType;
    function GetDBSession: TSession;
    procedure SetSessionName(const Value: string);
    procedure Activate;
    procedure Deactivate;
{$ENDIF}
    procedure FreeTimer;
    procedure StartTimer;
    procedure TimerExpired(Sender: TObject);
    function GenProgressCallback(CBInfo: Pointer): CBRType;
    function QryProgressCallback(CBInfo: Pointer): CBRType;
    procedure SetActive(Value: Boolean);
    procedure SetPercent(Value: Integer);
    procedure SetMessage(const Value: string);
    procedure SetMessageControl(Value: TControl);
    procedure SetGauge(Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ProgressMsgValue(const Msg: string): Longint;
  published
    property Active: Boolean read FActive write SetActive default True;
    property WaitCursor: TCursor read FWaitCursor write FWaitCursor default crHourGlass;
    property MessageControl: TControl read FMessageControl write SetMessageControl;
    property Gauge: TControl read FGauge write SetGauge;
{$IFNDEF VER80}
    property SessionName: string read FSessionName write SetSessionName;
    property Trace: Boolean read FTrace write SetTrace default False;
    property TraceFlags: TTraceFlags read FTraceFlags write SetTraceFlags default [];
    property OnTrace: TOnTraceEvent read FOnTrace write FOnTrace;
{$ENDIF}
    property OnMessageChange: TOnMessageChange read FOnMessageChange write FOnMessageChange;
    property OnPercentChange: TOnPercentChange read FOnPercentChange write FOnPercentChange;
    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;
  end;

{ TDBCallback - for internal use only }

type
  TDBCallbackEvent = function(CBInfo: Pointer): CBRType of object;
  TDBCallbackChain = (dcOnlyOnce, dcChain, dcReplace);

  TDBCallback = class(TObject)
  private
    FOwner: TObject;
    FCBType: CBType;
    FCBBuf: Pointer;
    FCBBufLen: Cardinal;
    FOldCBData: Longint;
    FOldCBBuf: Pointer;
    FOldCBBufLen: Word;
    FOldCBFunc: Pointer;
    FInstalled: Boolean;
    FChain: TDBCallbackChain;
    FCallbackEvent: TDBCallbackEvent;
  protected
    function Invoke(CallType: CBType; var CBInfo: Pointer): CBRType;
  public
    constructor Create(AOwner: TObject; CBType: CBType;
      CBBufSize: Cardinal; CallbackEvent: TDBCallbackEvent;
      Chain: TDBCallbackChain);
    destructor Destroy; override;
  end;

implementation

uses {$IFNDEF VER80} Windows, {$ELSE} WinTypes, WinProcs, Str16, {$ENDIF}
  Forms, SysUtils, StdCtrls, Dialogs, RxMaxMin, RxPrgrss, RxBdeUtils;

const
  cbQRYPROGRESS = cbRESERVED4;

{ TDBCallback }

function BdeCallBack(CallType: CBType; Data: Longint;
  {$IFDEF VER80} var {$ENDIF} CBInfo: Pointer): CBRType;
  {$IFNDEF VER80} stdcall; {$ELSE} export; {$ENDIF}
begin
  if Data <> 0 then begin
    Result := TDBCallback(Data).Invoke(CallType, CBInfo);
  end
  else Result := cbrUSEDEF;
end;

constructor TDBCallback.Create(AOwner: TObject; CBType: CBType;
  CBBufSize: Cardinal; CallbackEvent: TDBCallbackEvent;
  Chain: TDBCallbackChain);
begin
  FOwner := AOwner;
  FCBType := CBType;
  FCallbackEvent := CallbackEvent;
{$IFNDEF VER80}
  DbiGetCallBack(nil, FCBType, @FOldCBData, @FOldCBBufLen, @FOldCBBuf,
    pfDBICallBack(FOldCBFunc));
{$ELSE}
  DbiGetCallBack(nil, FCBType, FOldCBData, FOldCBBufLen, FOldCBBuf,
    @FOldCBFunc);
{$ENDIF}
  FChain := Chain;
  if not Assigned(FOldCBFunc) then FOldCBBufLen := 0;
  if not Assigned(FOldCBFunc) or (FChain in [dcChain, dcReplace]) then begin
    FCBBufLen := Max(CBBufSize, FOldCBBufLen);
    FCBBuf := AllocMem(FCBBufLen);
    Check(DbiRegisterCallback(nil, FCBType, Longint(Self), FCBBufLen,
      FCBBuf, BdeCallBack));
    FInstalled := True;
  end;
end;

destructor TDBCallback.Destroy;
begin
  if FInstalled then begin
    if Assigned(FOldCBFunc) and (FChain = dcChain) then
    try
      DbiRegisterCallback(nil, FCBType, FOldCBData, FOldCBBufLen,
        FOldCBBuf, pfDBICallback(FOldCBFunc));
    except
    end
    else DbiRegisterCallback(nil, FCBType, 0, 0, nil, nil);
  end;
  if FCBBuf <> nil then FreeMem(FCBBuf, FCBBufLen);
end;

function TDBCallback.Invoke(CallType: CBType; var CBInfo: Pointer): CBRType;
begin
  Result := cbrUSEDEF;
  if CallType = FCBType then
  try
{$IFNDEF VER80}
    Result := FCallbackEvent(CBInfo);
{$ELSE}
    Result := FCallbackEvent(@CBInfo);
{$ENDIF}
  except
    Application.HandleException(Self);
  end;
  if Assigned(FOldCBFunc) and (FChain = dcChain) then
    Result := pfDBICallBack(FOldCBFunc)(CallType, FOldCBData, CBInfo);
end;

{ ProgressList }

const
  ProgressList: TList = nil;

procedure SetWaitCursor;
begin
{$IFNDEF VER80}
  if (GetCurrentThreadID = MainThreadID) then
{$ENDIF}
    Screen.Cursor := TDBProgress(ProgressList.Items[
      ProgressList.Count - 1]).WaitCursor;
end;

procedure AddProgress(Progress: TDBProgress);
begin
  if ProgressList = nil then ProgressList := TList.Create;
  if ProgressList.IndexOf(Progress) = -1 then ProgressList.Add(Progress);
end;

procedure RemoveProgress(Progress: TDBProgress);
begin
  if ProgressList <> nil then begin
    ProgressList.Remove(Progress);
    if ProgressList.Count = 0 then begin
      ProgressList.Free;
      ProgressList := nil;
      Screen.Cursor := crDefault;
    end;
  end;
end;

{$IFNDEF VER80}

{ TSessionLink }

type
  TSessionLink = class(TDatabase)
  private
    FProgress: TDBProgress;
  public
    destructor Destroy; override;
  end;

destructor TSessionLink.Destroy;
begin
  if FProgress <> nil then begin
    FProgress.FSessionLink := nil;
    FProgress.Trace := False;
    FProgress.Active := False;
  end;
  inherited Destroy;
end;

{$ENDIF}

{ TDBProgress }

constructor TDBProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaitCursor := crHourGlass;
  FActive := True;
end;

destructor TDBProgress.Destroy;
begin
{$IFNDEF VER80}
  FOnTrace := nil;
  Trace := False;
{$ENDIF}
  Active := False;
  FreeTimer;
  FTimer.Free;
  inherited Destroy;
end;

procedure TDBProgress.Loaded;
begin
  inherited Loaded;
  FStreamedValue := True;
  try
    SetActive(FActive);
{$IFNDEF VER80}
    SetTrace(FTrace);
{$ENDIF}
  finally
    FStreamedValue := False;
  end;
end;

procedure TDBProgress.TimerExpired(Sender: TObject);
begin
  FreeTimer;
  SetPercent(0);
  SetMessage('');
end;

procedure TDBProgress.FreeTimer;
begin
  if FTimer <> nil then begin
    FTimer.Enabled := False;
    FStartTime := 0;
  end;
  Screen.Cursor := crDefault;
  SetCursor(Screen.Cursors[crDefault]); { force update cursor }
end;

procedure TDBProgress.StartTimer;
begin
  if (FTimer = nil) then begin
    FTimer := TRxTimer.Create(Self);
    FTimer.Interval := 500;
  end;
  with FTimer do begin
    if not Enabled then FStartTime := GetTickCount;
    OnTimer := TimerExpired;
    Enabled := True;
  end;
end;

procedure TDBProgress.SetPercent(Value: Integer);
begin
  if Gauge <> nil then begin
    SetProgressMax(Gauge, 100);
    SetProgressValue(Gauge, Value);
  end;
  if Assigned(FOnPercentChange) then FOnPercentChange(Self, Value);
end;

procedure TDBProgress.SetMessage(const Value: string);
begin
  if MessageControl <> nil then begin
    TLabel(MessageControl).Caption := Value;
    MessageControl.Refresh;
  end;
  if Assigned(FOnMessageChange) then FOnMessageChange(Self, Value);
end;

procedure TDBProgress.SetActive(Value: Boolean);
begin
  if (FActive <> Value) or FStreamedValue then begin
    if not (csDesigning in ComponentState) then begin
      if Value then AddProgress(Self) else RemoveProgress(Self);
      if (FGenProgressCallback = nil) and Value then begin
{$IFNDEF VER80}
        Activate;
{$ENDIF}
        FGenProgressCallback := TDBCallback.Create(Self, cbGENPROGRESS,
          Max(SizeOf(CBPROGRESSDesc), SizeOf(DBIPATH) + SizeOf(Integer) * 4),
          GenProgressCallback, dcChain);
        FQryProgressCallback := TDBCallback.Create(Self, cbQRYPROGRESS,
          SizeOf(DBIQryProgress), QryProgressCallback, dcChain);
      end
      else if not Value and (FGenProgressCallback <> nil) then begin
{$IFNDEF VER80}
        Sessions.CurrentSession := GetDBSession;
{$ENDIF}
        FGenProgressCallback.Free;
        FGenProgressCallback := nil;
        FQryProgressCallback.Free;
        FQryProgressCallback := nil;
        FreeTimer;
{$IFNDEF VER80}
        if not Trace then Deactivate;
{$ENDIF}
      end;
    end;
    FActive := Value;
  end;
end;

{$IFNDEF VER80}

procedure TDBProgress.Activate;
var
  S: TSession;
begin
  if FSessionLink = nil then begin
    S := Sessions.List[SessionName];
    S.Open;
    Sessions.CurrentSession := S;
    FSessionLink := TSessionLink.Create(S);
    try
      TSessionLink(FSessionLink).Temporary := True;
      TSessionLink(FSessionLink).KeepConnection := False;
      TSessionLink(FSessionLink).FProgress := Self;
    except
      FSessionLink.Free;
      FSessionLink := nil;
      raise;
    end;
  end
  else Sessions.CurrentSession := TDatabase(FSessionLink).Session;
end;

procedure TDBProgress.Deactivate;
begin
  if FSessionLink <> nil then begin
    TSessionLink(FSessionLink).FProgress := nil;
    FSessionLink.Free;
    FSessionLink := nil;
  end;
end;

function TDBProgress.GetDBSession: TSession;
begin
  Result := Sessions.FindSession(SessionName);
  if Result = nil then
{$IFDEF RX_D3}
    Result := DBTables.Session;
{$ELSE}
    Result := DB.Session;
{$ENDIF}
end;

procedure TDBProgress.SetSessionName(const Value: string);
var
  KeepActive, KeepTrace: Boolean;
begin
  if Value <> SessionName then begin
    if not (csDesigning in ComponentState) then begin
      KeepActive := Active;
      KeepTrace := Trace;
      Active := False;
      Trace := False;
      FSessionName := Value;
      Active := KeepActive;
      Trace := KeepTrace;
    end
    else FSessionName := Value;
  end;
end;

procedure TDBProgress.SetTrace(Value: Boolean);
begin
  if (FTrace <> Value) or (FStreamedValue and Value) then begin
    if not (csDesigning in ComponentState) then begin
      if Value then begin
        Activate;
        GetDBSession.TraceFlags := FTraceFlags;
        FTraceCallback := TDBCallback.Create(Self, cbTRACE,
          smTraceBufSize, TraceCallBack, dcReplace);
      end
      else if (FTraceCallback <> nil) then begin
        Sessions.CurrentSession := GetDBSession;
        FTraceCallback.Free;
        FTraceCallback := nil;
        if not Active then Deactivate;
      end;
      FTrace := (FTraceCallback <> nil);
    end
    else FTrace := Value;
  end;
end;

procedure TDBProgress.SetTraceFlags(Value: TTraceFlags);
begin
  FTraceFlags := Value;
  if Trace then GetDBSession.TraceFlags := FTraceFlags;
end;

function TDBProgress.TraceCallBack(CBInfo: Pointer): CBRType;
var
  CurFlag: TTraceFlag;
begin
  Result := cbrUSEDEF;
  if Trace and Assigned(FOnTrace) then begin
    case PTraceDesc(CBInfo)^.eTraceCat of
      traceQPREPARE: CurFlag := tfQPrepare;
      traceQEXECUTE: CurFlag := tfQExecute;
      traceERROR: CurFlag := tfError;
      traceSTMT: CurFlag := tfStmt;
      traceCONNECT: CurFlag := tfConnect;
      traceTRANSACT: CurFlag := tfTransact;
      traceBLOB: CurFlag := tfBlob;
      traceMISC: CurFlag := tfMisc;
      traceVENDOR: CurFlag := tfVendor;
{$IFDEF RX_D3}
      traceDATAIN: CurFlag := tfDataIn;
      traceDATAOUT: CurFlag := tfDataOut;
{$ENDIF RX_D3}
      else Exit;
    end;
    if (CurFlag in TraceFlags) then
      FOnTrace(Self, CurFlag, string({$IFDEF RX_D18}System.AnsiStrings.{$ENDIF}StrPas(PTraceDesc(CBInfo)^.pszTrace)));
  end;
end;

{$ENDIF}

procedure TDBProgress.SetMessageControl(Value: TControl);
begin
  FMessageControl := Value;
{$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TDBProgress.SetGauge(Value: TControl);
begin
  FGauge := Value;
{$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TDBProgress.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then begin
    if AComponent = Gauge then Gauge := nil
    else if AComponent = MessageControl then MessageControl := nil;
  end;
end;

function TDBProgress.GenProgressCallback(CBInfo: Pointer): CBRType;
var
  CallInfo: pCBPROGRESSDesc absolute CBInfo;
  AbortOp: Boolean;
begin
  Result := cbrUSEDEF;
  StartTimer;
  if (FTimer <> nil) and FTimer.Enabled {and (GetTickCount > FStartTime)} then
    SetWaitCursor;
  if Assigned(FOnProgress) then begin
    AbortOp := False;
    FOnProgress(Self, AbortOp);
    if AbortOp then Result := cbrABORT;
  end;
  if CallInfo^.iPercentDone >= 0 then SetPercent(CallInfo^.iPercentDone)
  else SetMessage(string({$IFDEF RX_D18}System.AnsiStrings.{$ENDIF}StrPas(CallInfo^.szMsg)));
end;

function TDBProgress.QryProgressCallback(CBInfo: Pointer): CBRType;
var
  CallInfo: pDBIQryProgress absolute CBInfo;
  AbortOp: Boolean;
  PcntDone: Double;
begin
  Result := cbrUSEDEF;
  StartTimer;
  {if (FTimer <> nil) and FTimer.Enabled then SetWaitCursor;}
  if Assigned(FOnProgress) then begin
    AbortOp := False;
    FOnProgress(Self, AbortOp);
    if AbortOp then Result := cbrABORT;
  end;
  with CallInfo^ do begin
    PcntDone := (stepsCompleted / Max(1, stepsInQry)) *
      (elemCompleted / Max(1, totElemInStep));
  end;
  SetPercent(Round(PcntDone * 100));
end;

function TDBProgress.ProgressMsgValue(const Msg: string): Longint;
begin
  if Msg <> '' then
    Result := StrToIntDef(Trim(Copy(Msg, Pos(':', Msg) + 1, MaxInt)), -1)
  else Result := -1;
end;

end.