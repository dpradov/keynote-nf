{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{         Copyright (c) 2001,2002 SGB Software          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxTimer;

interface

{$I RX.INC}
{$IFDEF RX_D6}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
uses
  Windows, Messages, SysUtils, Classes, ExtCtrls;

type

  {  TRxTimerEventTime  }

  TRxTimerEventTime = (tetPre, tetPost);

  {  TRxTimer  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FSyncEvent: Boolean;
    FThreaded: Boolean;
    FTimerThread: TThread;
    FTimer: TTimer;
    FEventTime: TRxTimerEventTime;
    FThreadPriority: TThreadPriority;
    FInTimerEvent: Boolean;
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetThreaded(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateTimer;
  protected
    procedure DoTimer(Sender: TObject);
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Synchronize(Method: TThreadMethod);
  published
    property EventTime: TRxTimerEventTime read FEventTime write FEventTime default tetPre;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property SyncEvent: Boolean read FSyncEvent write FSyncEvent default True;
    property Threaded: Boolean read FThreaded write SetThreaded default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority default tpNormal;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  {  TCustomThread  }

  TCustomThread = class(TThread)
  private
    FThreadName: ShortString;
    FOnExecute: TNotifyEvent;
    procedure SetThreadName(const Value: string); virtual;
    function GetThreadName: string; virtual;
  protected
    procedure Execute; override;
  public
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property Terminated;
    property ThreadName: string read GetThreadName write SetThreadName;
  end;

{$IFNDEF UNICODE}

  {  TRxThread  }

  TRxThread = class(TComponent)
  private
    FThread: TCustomThread;
    FSyncMethod: TNotifyEvent;
    FSyncParams: Pointer;
    FStreamedSuspended, FCycled: Boolean;
    FOnExecute, FOnException: TNotifyEvent;
    FInterval: Integer;
    procedure InternalSynchronize;
    function GetHandle: THandle;
    function GetThreadID: THandle;
    function GetOnTerminate: TNotifyEvent;
    procedure SetOnTerminate(Value: TNotifyEvent);
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function GetReturnValue: Integer;
    procedure SetReturnValue(Value: Integer);
    function GetSuspended: Boolean;
    procedure SetSuspended(Value: Boolean);
    function GetTerminated: boolean;
  protected
    procedure DoExecute(Sender: TObject); virtual;
    procedure DoException(Sender: TObject); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Synchronize(Method: TThreadMethod);
    procedure SynchronizeEx(Method: TNotifyEvent; Params: Pointer);
    procedure Suspend;
    procedure Resume;
    procedure Terminate;
    function TerminateWaitFor: Integer;
    procedure TerminateHard;
    function WaitFor: Integer;
    property ReturnValue: Integer read GetReturnValue write SetReturnValue;
    property Handle: THandle read GetHandle;
    property ThreadID: THandle read GetThreadID;
    property Terminated: Boolean read GetTerminated;
    procedure Delay(MSecs: Longint);
  published
    property OnTerminate: TNotifyEvent read GetOnTerminate write SetOnTerminate;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Suspended: Boolean read GetSuspended write SetSuspended default True;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnException: TNotifyEvent read FOnException write FOnException;
    property Interval: Integer read FInterval write FInterval;
    property Cycled: Boolean read FCycled write FCycled;
  end;

{$ENDIF}

implementation

uses
  Forms, SyncObjs, Consts, RxVCLUtils;

{ TTimerThread }

type
  TTimerThread = class(TCustomThread)
  private
    FOwner: TRxTimer;
    FInterval: Cardinal;
    FException: Exception;
    FPaused: Boolean;
    FPauseSection: TCriticalSection;
    FCurrentDuration: Cardinal;
    procedure HandleException;
    procedure SetPaused(const Value: Boolean);
    function GetPaused: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Timer: TRxTimer; Enabled: Boolean);
    destructor Destroy; override;
    property Terminated;
    property Paused: Boolean read GetPaused write SetPaused;
  end;

constructor TTimerThread.Create(Timer: TRxTimer; Enabled: Boolean);
begin
  FOwner := Timer;
  FPauseSection := TCriticalSection.Create;
  inherited Create(not Enabled);
  FInterval := 1000;
  FreeOnTerminate := False;
  ThreadName := Format('%s: %s',[ClassName, Timer.Name]);
end;

procedure TTimerThread.HandleException;
begin
  if not (FException is EAbort) then
    Application.HandleException(Self);
end;

procedure TTimerThread.SetPaused(const Value: Boolean);
begin
  if FPaused <> Value then
  begin
    FPauseSection.Acquire;
    FPaused := Value;
    FPauseSection.Release;

    if not FPaused and Suspended then
      Suspended := False;
  end;
end;

destructor TTimerThread.Destroy;
begin
  FPauseSection.Free;
  inherited Destroy;
end;

procedure TTimerThread.Execute;
const
  Step = 10;  // Time of a wait slot, in milliseconds
var
  EventTime: TRxTimerEventTime;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwner = nil);
  end;

begin
  {$IFDEF RX_D15}
  NameThreadForDebugging(AnsiString(ThreadName));
  {$ENDIF}
  //NameThread(ThreadName);
  repeat
    EventTime := FOwner.EventTime;

    if EventTime = tetPost then
    begin
      { Wait first and then trigger the event }
      FCurrentDuration := 0;
      while not ThreadClosed and (FCurrentDuration < FInterval) do
      begin
        SleepEx(Step, False);
        Inc(FCurrentDuration, Step);
      end;
    end;

    if not ThreadClosed and not ThreadClosed and FOwner.FEnabled then
    begin
      if FOwner.SyncEvent then
      begin
        Synchronize(FOwner.Timer)
      end
      else
      begin
        try
          FOwner.Timer;
        except
          on E: Exception do
          begin
            FException := E;
            HandleException;
          end;
        end;
      end;
    end;

    if EventTime = tetPre then
    begin
      { Wait after the event was triggered }
      FCurrentDuration := 0;
      while not ThreadClosed and (FCurrentDuration < FInterval) do
      begin
        SleepEx(Step, False);
        Inc(FCurrentDuration, Step);
      end;
    end;

    // while we are paused, we do not do anything. However, we do call SleepEx
    // in the alertable state to avoid 100% CPU usage. Note that the delay
    // should not be 0 as it may lead to 100% CPU in that case. 10 is a safe
    // value that is small enough not to have a big impact on restart.
    while not Terminated and Paused do
      SleepEx(10, True);
  until Terminated;
end;

function TTimerThread.GetPaused: Boolean;
begin
  FPauseSection.Acquire;
  Result := FPaused;
  FPauseSection.Release;
end;

{ TRxTimer }

constructor TRxTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventTime := tetPre;
  FEnabled := True;
  FInterval := 1000;
  FSyncEvent := True;
  FThreaded := True;
  FThreadPriority := tpNormal;
  FTimerThread := nil;
  FTimer := nil;
end;

destructor TRxTimer.Destroy;
begin
  Destroying;
  FEnabled := False;
  FOnTimer := nil;
  {TTimerThread(FTimerThread).FOwner := nil;}
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    (FTimerThread as TTimerThread).Paused := False;
    FTimerThread.Free;
  end;
  FTimer.Free;
  inherited Destroy;
end;

procedure TRxTimer.DoTimer(Sender: TObject);
begin
  Timer;
end;

procedure TRxTimer.UpdateTimer;
begin
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
  begin
    if FThreaded then
    begin
      FreeAndNil(FTimer);
      if not Assigned(FTimerThread) then
        FTimerThread := TTimerThread.Create(Self, False);
      TTimerThread(FTimerThread).Paused := True;
      TTimerThread(FTimerThread).FCurrentDuration := 0;
      TTimerThread(FTimerThread).FInterval := FInterval;
      FTimerThread.Priority := FThreadPriority;
      TTimerThread(FTimerThread).Paused := False;
    end
    else
    begin
      FreeAndNil(FTimerThread);

      if not Assigned(FTimer) then
        FTimer := TTimer.Create(Self);
      FTimer.Interval := FInterval;
      FTimer.OnTimer := DoTimer;
      FTimer.Enabled := True;
    end;
  end
  else
  begin
    { Don't destroy the thread or the timer if we are currently in the event }
    if FInTimerEvent then
      Exit;

    if (FTimerThread <> nil) then
    begin
      TTimerThread(FTimerThread).Terminate;
      FreeAndNil(FTimerThread);
    end;

    if (FTimer <> nil) then
    begin
      FTimer.Enabled := False;
      FreeAndNil(FTimer);
    end;
  end;
end;

procedure TRxTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetThreaded(Value: Boolean);
begin
  if Value <> FThreaded then
  begin
    if FInTimerEvent then
      raise Exception.Create('Cannot create thread...');
    FThreaded := Value;
    UpdateTimer;
  end;
end;

procedure TRxTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value <> FThreadPriority then
  begin
    FThreadPriority := Value;
    if FThreaded then UpdateTimer;
  end;
end;

procedure TRxTimer.Synchronize(Method: TThreadMethod);
begin
  if (FTimerThread <> nil) then
  begin
    with TTimerThread(FTimerThread) do
    begin
      if Suspended or Terminated then Method
      else TTimerThread(FTimerThread).Synchronize(Method);
    end;
  end
  else Method;
end;

procedure TRxTimer.SetOnTimer(Value: TNotifyEvent);
begin
  if Assigned(FOnTimer) <> Assigned(Value) then
  begin
    FOnTimer := Value;
    UpdateTimer;
  end
  else
    FOnTimer := Value;
end;

procedure TRxTimer.Timer;
begin
  if FEnabled and not (csDestroying in ComponentState) and Assigned(FOnTimer) then
  begin
    FInTimerEvent := True;
    try
      FOnTimer(Self);
    finally
      FInTimerEvent := False;
    end;
  end;
end;

{  TCustomThread  }

procedure TCustomThread.Execute;
begin
  if Assigned(FOnExecute) then FOnExecute(Self);
end;

function TCustomThread.GetThreadName: string;
begin
  Result := string(FThreadName);
end;

procedure TCustomThread.SetThreadName(const Value: string);
begin
  FThreadName := AnsiString(Value);
end;

{$IFNDEF UNICODE}

{  TThreadHack  }

type  TThreadHack = class(TCustomThread);

{  TRxThread  }

constructor TRxThread.Create(AOwner: TComponent);
begin
  inherited;
  FStreamedSuspended := True;
  FThread := TCustomThread.Create(True);
  FThread.OnExecute := DoExecute;
  FInterval := 0;
  FCycled := False;
end;

destructor TRxThread.Destroy;
begin
  if not FStreamedSuspended then
    FThread.Suspend;
  FThread.Free;
  inherited;
end;

procedure TRxThread.DoExecute(Sender: TObject);
begin
  repeat
    Delay(FInterval);
    if FThread.Terminated then Break;
    try
      if Assigned(FOnExecute) then FOnExecute(Self);
    except
      SynchronizeEx(DoException, ExceptObject);
    end;
  until FThread.Terminated or not (Cycled);
end;

procedure TRxThread.DoException(Sender: TObject);
var
  s: string;
begin
  if Assigned(FOnException) then
    FOnException(Sender)
  else
  begin
    s := Format('Thread %s raised exception class %s with message ''%s''.',
      [Name, Exception(Sender).ClassName, Exception(Sender).Message]);
    Application.MessageBox(PChar(s), PChar(Application.Title),
      MB_ICONERROR or MB_SETFOREGROUND or MB_APPLMODAL);
  end;
end;

function TRxThread.GetHandle: THandle;
begin
  Result := FThread.Handle;
end;

function TRxThread.GetOnTerminate: TNotifyEvent;
begin
  Result := FThread.OnTerminate;
end;

function TRxThread.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TRxThread.GetReturnValue: Integer;
begin
  Result := TThreadHack(FThread).ReturnValue;
end;

function TRxThread.GetSuspended: Boolean;
begin
  if not (csDesigning in ComponentState) then
    Result := FThread.Suspended
  else
    Result := FStreamedSuspended;
end;

procedure TRxThread.Execute;
begin
  TerminateHard;
  FThread.Resume; //{$IFDEF RX_D14}Start{$ELSE}Resume{$ENDIF};
end;

procedure TRxThread.Loaded;
begin
  inherited;
  SetSuspended(FStreamedSuspended);
end;

procedure TRxThread.SetOnTerminate(Value: TNotifyEvent);
begin
  FThread.OnTerminate := Value;
end;

procedure TRxThread.SetPriority(Value: TThreadPriority);
begin
  FThread.Priority := Value;
end;

procedure TRxThread.SetReturnValue(Value: Integer);
begin
  TThreadHack(FThread).ReturnValue := Value;
end;

procedure TRxThread.SetSuspended(Value: Boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    if (csLoading in ComponentState) then
      FStreamedSuspended := Value
    else
      FThread.Suspended := Value;
  end
  else
    FStreamedSuspended := Value;
end;

procedure TRxThread.Suspend;
begin
  FThread.Suspend;
end;

procedure TRxThread.Synchronize(Method: TThreadMethod);
begin
  TThreadHack(FThread).Synchronize(Method);
end;

procedure TRxThread.InternalSynchronize;
begin
  FSyncMethod(FSyncParams);
end;

procedure TRxThread.SynchronizeEx(Method: TNotifyEvent; Params: Pointer);
begin
  if not Assigned(FSyncMethod) then
  begin
    FSyncMethod := Method; FSyncParams := Params;
    try
      TThreadHack(FThread).Synchronize(InternalSynchronize);
    finally
      FSyncMethod := nil; FSyncParams := nil;
    end;
  end;
end;

procedure TRxThread.Resume;
begin
  FThread.Resume; //{$IFDEF RX_D14}Start{$ELSE}Resume{$ENDIF};
end;

procedure TRxThread.Terminate;
begin
  FThread.Terminate;
end;

function TRxThread.TerminateWaitFor: Integer;
begin
  Terminate;
  Result := WaitFor;
end;

procedure TRxThread.TerminateHard;
var
  FTmp: TCustomThread;
begin
  TerminateThread(FThread.Handle, 0);
  FTmp := TCustomThread.Create(True);
  try
    FTmp.Priority := Self.Priority;
    FTmp.OnExecute := DoExecute;
    FTmp.OnTerminate := Self.OnTerminate;
  except
    FTmp.Free;
    raise;
  end;
  FThread.Free;
  FThread := FTmp;
end;

function TRxThread.WaitFor: Integer;
begin
  Result := FThread.WaitFor;
end;

function TRxThread.GetTerminated: boolean;
begin
  Result := FThread.Terminated;
end;

function TRxThread.GetThreadID: THandle;
begin
  Result := FThread.ThreadID;
end;

procedure TRxThread.Delay(MSecs: Longint);
var
  FirstTickCount, Now: Longint;
begin
  if MSecs < 0 then exit;
  FirstTickCount := GetTickCount;
  repeat
    Sleep(1);
    Now := GetTickCount;
  until (Now - FirstTickCount >= MSecs) or (Now < FirstTickCount) or Terminated;
end;

{$ENDIF}

end.