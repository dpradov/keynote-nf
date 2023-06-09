{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxQuery;

{$I RX.INC}
{$P+,W-,R-}

interface

uses {$IFNDEF VER80} Bde, Windows, {$ELSE} DbiTypes, DbiProcs, {$ENDIF}
  Classes, SysUtils, DB, DBTables, RxBdeUtils,
  {$IFDEF RX_D6} RTLConsts,{$ENDIF} RxStrUtils; // Polaris

{.$DEFINE DEBUG}

const
  DefaultMacroChar = '%';
  DefaultTermChar  = '/';

{ TRxQuery }

type
{$IFDEF RX_D12}
  TBookmarkType = TBookmark;
  TBookmarkPointerType = Pointer;
  TBuffer = TRecordBuffer;
{$ELSE}
  TBookmarkType = TBookmarkStr;
  TBookmarkPointerType = TBookmark;
  TBuffer = PChar;
{$ENDIF}

  TQueryOpenStatus = (qsOpened, qsExecuted, qsFailed);

  TRxQuery = class(TQuery)
  private
    FDisconnectExpected: Boolean;
    FSaveQueryChanged: TNotifyEvent;
    FMacroChar: Char;
    FMacros: TParams;
    FSQLPattern: TStrings;
    FStreamPatternChanged: Boolean;
    FPatternChanged: Boolean;
    FOpenStatus: TQueryOpenStatus;
{$IFDEF VER80}
    FParamCheck: Boolean;
{$ENDIF}
    function GetMacros: TParams;
    procedure SetMacros(Value: TParams);
    procedure SetSQL(Value: TStrings);
    procedure PatternChanged(Sender: TObject);
    procedure QueryChanged(Sender: TObject);
    procedure RecreateMacros;
    procedure CreateMacros(List: TParams; const Value: PChar);
    procedure Expand(Query: TStrings);
    function GetMacroCount: Word;
    procedure SetMacroChar(Value: Char);
    function GetRealSQL: TStrings;
{$IFDEF DEBUG}
    procedure SetRealSQL(Value: TStrings);
{$ENDIF DEBUG}
  protected
{$IFDEF RX_D3}
    procedure InternalFirst; override;
    function GetRecord(Buffer: TBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
{$ENDIF}
    procedure Loaded; override;
    function CreateHandle: HDBICur; override;
    procedure OpenCursor {$IFDEF RX_D3} (InfoQuery: Boolean) {$ENDIF}; override;
    procedure Disconnect; override;
{$IFDEF RX_D5}
  protected
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetTableName: string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExpandMacros;
    procedure ExecSQL;
    procedure Prepare;
    procedure OpenOrExec(ChangeLive: Boolean);
    procedure ExecDirect;
    function MacroByName(const Value: string): TParam;
{$IFNDEF RX_D3}
    function IsEmpty: Boolean;
{$ENDIF RX_D3}
    property MacroCount: Word read GetMacroCount;
    property OpenStatus: TQueryOpenStatus read FOpenStatus;
{$IFNDEF DEBUG}
    property RealSQL: TStrings read GetRealSQL;
{$ENDIF DEBUG}
  published
{$IFDEF VER80}
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
{$ENDIF}
    property MacroChar: Char read FMacroChar write SetMacroChar default DefaultMacroChar;
    property SQL: TStrings read FSQLPattern write SetSQL;
{$IFDEF DEBUG}
    property RealSQL: TStrings read GetRealSQL write SetRealSQL stored False;
{$ENDIF DEBUG}
    property Macros: TParams read GetMacros write SetMacros;
  end;

{$IFNDEF VER80}

{ TRxQueryThread }

  TRunQueryMode = (rqOpen, rqExecute, rqExecDirect, rqOpenOrExec);

  TRxQueryThread = class(TThread)
  private
    FData: TBDEDataSet;
    FMode: TRunQueryMode;
    FPrepare: Boolean;
    FException: TObject;
    procedure DoHandleException;
  protected
    procedure ModeError; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure HandleException; virtual;
  public
    constructor Create(Data: TBDEDataSet; RunMode: TRunQueryMode;
      Prepare, CreateSuspended: Boolean);
  end;

{$ENDIF}

{ TSQLScript }

  TScriptAction = (saFail, saAbort, saRetry, saIgnore, saContinue);

  TScriptErrorEvent = procedure(Sender: TObject; E: EDatabaseError;
    LineNo, StatementNo: Integer; var Action: TScriptAction) of object;

  TSQLScript = class(TComponent)
  private
    FSQL: TStrings;
    FParams: TParams;
    FQuery: TRxQuery;
    FTransaction: Boolean;
    FSemicolonTerm: Boolean;
    FIgnoreParams: Boolean;
    FTerm: Char;
    FBeforeExec: TNotifyEvent;
    FAfterExec: TNotifyEvent;
    FOnScriptError: TScriptErrorEvent;
{$IFNDEF VER80}
    function GetSessionName: string;
    procedure SetSessionName(const Value: string);
    function GetDBSession: TSession;
    function GetText: string;
{$ENDIF}
{$IFDEF RX_D4}
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
{$ENDIF RX_D4}
    function GetDatabase: TDatabase;
    function GetDatabaseName: string;
    procedure SetDatabaseName(const Value: string);
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    function GetParamsCount: Cardinal;
  protected
{$IFDEF RX_D4}
    procedure DefineProperties(Filer: TFiler); override;
{$ENDIF RX_D4}
    procedure CheckExecQuery(LineNo, StatementNo: Integer);
    procedure ExecuteScript(StatementNo: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    procedure ExecStatement(StatementNo: Integer);
    function ParamByName(const Value: string): TParam;
{$IFNDEF VER80}
    property DBSession: TSession read GetDBSession;
    property Text: string read GetText;
{$ELSE}
    function GetText: PChar;
{$ENDIF}
    property Database: TDatabase read GetDatabase;
    property ParamCount: Cardinal read GetParamsCount;
  published
    property DatabaseName: string read GetDatabaseName write SetDatabaseName;
    property IgnoreParams: Boolean read FIgnoreParams write FIgnoreParams default False;
    property SemicolonTerm: Boolean read FSemicolonTerm write FSemicolonTerm default True;
{$IFNDEF VER80}
    property SessionName: string read GetSessionName write SetSessionName;
{$ENDIF}
    property Term: Char read FTerm write FTerm default DefaultTermChar;
    property SQL: TStrings read FSQL write SetQuery;
    property Params: TParams read FParams write SetParamsList {$IFDEF RX_D4} stored False {$ENDIF};
    property Transaction: Boolean read FTransaction write FTransaction;
    property BeforeExec: TNotifyEvent read FBeforeExec write FBeforeExec;
    property AfterExec: TNotifyEvent read FAfterExec write FAfterExec;
    property OnScriptError: TScriptErrorEvent read FOnScriptError write FOnScriptError;
  end;

const
  dbfExecScript = dbfTable;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);

implementation

uses
  RxDBUtils, Consts, DBConsts, Forms {$IFDEF RX_D3}, BDEConst {$ENDIF}
  {$IFDEF VER80}, RxStr16 {$ENDIF}, RxVclUtils;

{ Parse SQL utility routines }

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := CharInSet(C, [' ', ',', ';', ')', #13, #10]) or CharInSet(C, Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := CharInSet(C, ['''', '"']);
end;

procedure CreateQueryParams(List: TParams; const Value: PChar; Macro: Boolean;
  SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if Assigned(List) then begin
{$IFDEF RX_D4}
        if List.FindParam(Name) = nil then begin
{$ENDIF RX_D4}
          if Macro then
            List.CreateParam(ftString, Name, ptInput).AsString := TrueExpr
          else List.CreateParam(ftUnknown, Name, ptUnknown);
{$IFDEF RX_D4}
        end;
{$ENDIF RX_D4}
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{ TRxQuery }

constructor TRxQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF VER80}
  FParamCheck := True;
{$ENDIF}
  FOpenStatus := qsFailed;
  FSaveQueryChanged := TStringList(inherited SQL).OnChange;
  TStringList(inherited SQL).OnChange := QueryChanged;
  FMacroChar := DefaultMacroChar;
  FSQLPattern := TStringList.Create;
  TStringList(SQL).OnChange := PatternChanged;
  FMacros := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
end;

destructor TRxQuery.Destroy;
begin
  Destroying;
  Disconnect;
  FMacros.Free;
  FSQLPattern.Free;
  inherited Destroy;
end;

procedure TRxQuery.Loaded;
begin
  inherited Loaded;
  GetMacros; {!! trying this way}
end;

{$IFDEF RX_D3}

procedure TRxQuery.InternalFirst;
begin
  if not (UniDirectional and BOF) then
    inherited InternalFirst;
end;

function TRxQuery.GetRecord(Buffer: TBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  //!!!!!!
  if UniDirectional and (GetMode in [gmPrior, gmNext]) then DoCheck := False;
  Result := inherited GetRecord(Buffer, GetMode, DoCheck);
end;

{$ENDIF}

function TRxQuery.CreateHandle: HDBICur;
begin
  FOpenStatus := qsFailed;
  Result := inherited CreateHandle;
  if Result = nil then FOpenStatus := qsExecuted
  else FOpenStatus := qsOpened;
end;

procedure TRxQuery.OpenCursor;
begin
  ExpandMacros;
  inherited OpenCursor{$IFDEF RX_D3}(InfoQuery){$ENDIF};
end;

procedure TRxQuery.ExecSQL;
begin
  ExpandMacros;
  inherited ExecSQL;
end;

procedure TRxQuery.Prepare;
begin
  ExpandMacros;
  inherited Prepare;
end;

procedure TRxQuery.OpenOrExec(ChangeLive: Boolean);

  procedure TryOpen;
  begin
    try
      Open;
    except
      if OpenStatus <> qsExecuted then raise;
    end;
  end;

begin
  try
    TryOpen;
  except
    on E: EDatabaseError do
      if RequestLive and ChangeLive then begin
        RequestLive := False;
        try
          TryOpen;
        except
          on E: EDatabaseError do
            if OpenStatus <> qsOpened then
              ExecDirect
            else begin
              FOpenStatus := qsFailed;
              raise;
            end;
          else raise;
        end;
      end
      else begin
        if OpenStatus <> qsOpened then
          ExecDirect
        else begin
          FOpenStatus := qsFailed;
          raise;
        end;
      end;
    else raise;
  end;
end;

procedure TRxQuery.ExecDirect;
{$IFDEF VER80}
var
  P: PChar;
{$ENDIF}
begin
  CheckInactive;
  SetDBFlag(dbfExecSQL, True);
  try
    if SQL.Count > 0 then begin
      FOpenStatus := qsFailed;
{$IFNDEF VER80}
      Check(DbiQExecDirect(DBHandle, qryLangSQL, PAnsiChar(AnsiString(inherited SQL.Text)), nil));
{$ELSE}
      P := inherited SQL.GetText;
      try
        Check(DbiQExecDirect(DBHandle, qryLangSQL, P, nil));
      finally
        StrDispose(P);
      end;
{$ENDIF}
      FOpenStatus := qsExecuted;
    end
    else _DBError(SEmptySQLStatement);
  finally
    SetDBFlag(dbfExecSQL, False);
  end;
end;

procedure TRxQuery.Disconnect;
var
  Strings: TStrings;
  Event1, Event2: TNotifyEvent;
begin
  inherited Disconnect;
  if (csDestroying in ComponentState) then Exit;
  Strings := inherited SQL;
  Event1 := TStringList(Strings).OnChange;
  Event2 := QueryChanged;
  if @Event1 <> @Event2 then begin
    if not FDisconnectExpected then SQL := inherited SQL;
    TStringList(inherited SQL).OnChange := QueryChanged;
  end;
end;

procedure TRxQuery.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TRxQuery.GetMacros: TParams;
begin
  if FStreamPatternChanged then begin
    FStreamPatternChanged := False;
    PatternChanged(nil);
  end;
  Result := FMacros;
end;

procedure TRxQuery.SetMacros(Value: TParams);
begin
  FMacros.AssignValues(Value);
end;

procedure TRxQuery.SetSQL(Value: TStrings);
begin
  inherited Disconnect;
  TStringList(FSQLPattern).OnChange := nil;
  FSQLPattern.Assign(Value);
  TStringList(FSQLPattern).OnChange := PatternChanged;
  PatternChanged(nil);
end;

procedure TRxQuery.PatternChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then begin
    FStreamPatternChanged := True;
    Exit;
  end;
  inherited Disconnect;
  RecreateMacros;
  FPatternChanged := True;
  try
    ExpandMacros;
  finally
    FPatternChanged := False;
  end;
end;

procedure TRxQuery.QueryChanged(Sender: TObject);
{$IFDEF VER80}
var
  List: TParams;
  SaveParams: Boolean;
{$ENDIF}
begin
{$IFNDEF VER80}
  FSaveQueryChanged(Sender);
{$ELSE}
  SaveParams := not (ParamCheck or (csDesigning in ComponentState));
  if SaveParams then List := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
  try
    if SaveParams then List.Assign(Params);
    FSaveQueryChanged(Sender);
    if SaveParams then Params.Assign(List);
  finally
    if SaveParams then List.Free;
  end;
{$ENDIF}
  if not FDisconnectExpected then begin
    SQL := inherited SQL;
  end;
end;

procedure TRxQuery.ExpandMacros;
var
  ExpandedSQL: TStringList;
begin
  if not FPatternChanged and not FStreamPatternChanged and
    (MacroCount = 0) then Exit;
  ExpandedSQL := TStringList.Create;
  try
    Expand(ExpandedSQL);
    FDisconnectExpected := True;
    try
      inherited SQL := ExpandedSQL;
    finally
      FDisconnectExpected := False;
    end;
  finally
    ExpandedSQL.Free;
  end;
end;

procedure TRxQuery.RecreateMacros;
var
  List: TParams;
{$IFDEF VER80}
  P: PChar;
{$ENDIF}
begin
{$IFDEF RX_D4}
  if not (csReading in ComponentState) then begin
{$ENDIF RX_D4}
    List := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
    try
  {$IFNDEF VER80}
      CreateMacros(List, PChar(FSQLPattern.Text));
  {$ELSE}
      P := FSQLPattern.GetText;
      try
        CreateMacros(List, P);
      finally
        StrDispose(P);
      end;
  {$ENDIF}
      List.AssignValues(FMacros);
  {$IFDEF RX_D4}
      FMacros.Clear;
      FMacros.Assign(List);
    finally
  {$ELSE}
      FMacros.Free;
      FMacros := List;
    except
  {$ENDIF RX_D4}
      List.Free;
    end;
{$IFDEF RX_D4}
  end
  else begin
    FMacros.Clear;
    CreateMacros(FMacros, PChar(FSQLPattern.Text));
  end;
{$ENDIF RX_D4}
end;

procedure TRxQuery.CreateMacros(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, True, MacroChar, ['.']);
end;

procedure TRxQuery.Expand(Query: TStrings);

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TParam;
    Found: Boolean;
  begin
    Result := S;
    for I := Macros.Count - 1 downto 0 do begin
      Param := Macros[I];
      if Param.DataType = ftUnknown then Continue;
      repeat
        P := Pos(MacroChar + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1], ['.']));
        if Found then begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then begin
            Result := Copy(Result, 1, P - 1) + Param.Text + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to FSQLPattern.Count - 1 do
    Query.Add(ReplaceString(FSQLPattern[I]));
end;

function TRxQuery.GetMacroCount: Word;
begin
  Result := FMacros.Count;
end;

function TRxQuery.MacroByName(const Value: string): TParam;
begin
  Result := FMacros.ParamByName(Value);
end;

{$IFNDEF RX_D3}
function TRxQuery.IsEmpty: Boolean;
begin
  Result := IsDataSetEmpty(Self);
end;
{$ENDIF RX_D3}

function TRxQuery.GetRealSQL: TStrings;
begin
  try
    ExpandMacros;
  except
  end;
  Result := inherited SQL;
end;

{$IFDEF RX_D5}

{ TRxQuery.IProviderSupport }

function TRxQuery.PSGetDefaultOrder: TIndexDef;
begin
  ExpandMacros;
  Result := inherited PSGetDefaultOrder;
end;

function TRxQuery.PSGetTableName: string;
begin
  ExpandMacros;
  Result := inherited PSGetTableName;
end;

procedure TRxQuery.PSExecute;
begin
  ExecSQL;
end;

{$ENDIF RX_D5}

{$IFDEF DEBUG}
procedure TRxQuery.SetRealSQL(Value: TStrings);
begin
end;
{$ENDIF DEBUG}

{$IFNDEF VER80}

{ TRxQueryThread }

constructor TRxQueryThread.Create(Data: TBDEDataSet; RunMode: TRunQueryMode;
  Prepare, CreateSuspended: Boolean);
begin
  inherited Create(True);
  FData := Data;
  FMode := RunMode;
  FPrepare := Prepare;
  FreeOnTerminate := True;
  FData.DisableControls;
  if not CreateSuspended then {$IFDEF RX_D14}Start{$ELSE}Resume{$ENDIF};
end;

procedure TRxQueryThread.DoTerminate;
begin
  Synchronize(FData.EnableControls);
  inherited DoTerminate;
end;

procedure TRxQueryThread.ModeError;
begin
  SysUtils.Abort;
end;

procedure TRxQueryThread.DoHandleException;
begin
  if (FException is Exception) and not (FException is EAbort) then begin
    if Assigned(Application.OnException) then
      Application.OnException(FData, Exception(FException))
    else
      Application.ShowException(Exception(FException));
  end;
end;

procedure TRxQueryThread.HandleException;
begin
  FException := TObject(ExceptObject);
  Synchronize(DoHandleException);
end;

procedure TRxQueryThread.Execute;
begin
  try
    if FPrepare and not (FMode in [rqExecDirect]) then begin
      if FData is TRxQuery then TRxQuery(FData).Prepare
      else if FData is TQuery then TQuery(FData).Prepare
      else if FData is TStoredProc then TStoredProc(FData).Prepare;
    end;
    case FMode of
      rqOpen:
        FData.Open;
      rqExecute:
        begin
          if FData is TRxQuery then TRxQuery(FData).ExecSQL
          else if FData is TQuery then TQuery(FData).ExecSQL
          else if FData is TStoredProc then TStoredProc(FData).ExecProc
          else ModeError;
        end;
      rqExecDirect:
        begin
          if FData is TRxQuery then TRxQuery(FData).ExecDirect
          else ModeError;
        end;
      rqOpenOrExec:
        begin
          if FData is TRxQuery then TRxQuery(FData).OpenOrExec(True)
          else FData.Open;
        end;
    end;
  except
    HandleException;
  end;
end;

{$ENDIF}

{ TSQLScript }

constructor TSQLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  TStringList(SQL).OnChange := QueryChanged;
  FParams := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
  FQuery := TRxQuery.Create(Self);
  FSemicolonTerm := True;
  FTerm := DefaultTermChar;
end;

destructor TSQLScript.Destroy;
begin
  FQuery.Free;
  FSQL.Free;
  FParams.Free;
  inherited Destroy;
end;

function TSQLScript.GetDatabase: TDatabase;
begin
  Result := FQuery.Database;
end;

function TSQLScript.GetDatabaseName: string;
begin
  Result := FQuery.DatabaseName;
end;

procedure TSQLScript.SetDatabaseName(const Value: string);
begin
  FQuery.DatabaseName := Value;
end;

{$IFNDEF VER80}
function TSQLScript.GetSessionName: string;
begin
  Result := FQuery.SessionName;
end;

procedure TSQLScript.SetSessionName(const Value: string);
begin
  FQuery.SessionName := Value;
end;

function TSQLScript.GetDBSession: TSession;
begin
  Result := FQuery.DBSession;
end;
{$ENDIF}

procedure TSQLScript.CheckExecQuery(LineNo, StatementNo: Integer);
var
  Done: Boolean;
  Action: TScriptAction;
  I: Integer;
  Param: TParam;
{$IFDEF VER80}
  Msg: array[0..255] of Char;
{$ENDIF}
  S: string;
begin
  Done := False;
  repeat
    try
      if IgnoreParams then FQuery.ExecDirect
      else begin
        for I := 0 to FQuery.Params.Count - 1 do begin
          Param := FQuery.Params[I];
          Param.Assign(Params.ParamByName(Param.Name));
        end;
        FQuery.ExecSQL;
      end;
      Done := True;
    except
      on E: EDatabaseError do begin
        Action := saFail;
        S := Format(ResStr(SParseError), [ResStr(SMsgdlgError), LineNo]);
        if E is EDBEngineError then
          TDBError.Create(EDBEngineError(E), 0, LineNo,
            {$IFNDEF VER80} PChar(S) {$ELSE} StrPCopy(Msg, S) {$ENDIF})
        else begin
          if E.Message <> '' then E.Message := E.Message + '. ';
          E.Message := E.Message + S;
        end;
        if Assigned(FOnScriptError) then
          FOnScriptError(Self, E, LineNo, StatementNo, Action);
        if Action = saFail then raise;
        if Action = saAbort then SysUtils.Abort;
        if Action = saContinue then begin
          Application.HandleException(Self);
          Done := True;
        end
        else if Action = saIgnore then Done := True;
      end;
    end;
  until Done;
end;

procedure TSQLScript.ExecuteScript(StatementNo: Integer);
var
  S, LastStr: string;
  IsTrans, SQLFilled, StmtFound: Boolean;
  I, P, CurrStatement: Integer;
begin
  IsTrans := FTransaction {$IFDEF VER80} and Database.IsSQLBased {$ENDIF}
    and not TransActive(Database) and (StatementNo < 0);
  LastStr := '';
  try
    if IsTrans then begin
{$IFNDEF VER80}
      if not Database.IsSQLBased then
        Database.TransIsolation := tiDirtyRead;
{$ENDIF}
      Database.StartTransaction;
    end;
  except
    IsTrans := False;
  end;
  try
    I := 0;
    CurrStatement := 0;
    StmtFound := False;
    while I < SQL.Count do begin
      FQuery.SQL.BeginUpdate;
      try
        FQuery.SQL.Clear;
        SQLFilled := False;
        repeat
          if LastStr <> '' then begin
            FQuery.SQL.Add(LastStr);
            LastStr := '';
          end;
          if I < SQL.Count then begin
            S := Trim(SQL[I]);
            Inc(I);
            P := Pos(';', S);
            if (P > 0) and FSemicolonTerm then begin
              LastStr := Trim(Copy(S, P + 1, MaxInt));
              S := Copy(S, 1, P - 1);
              if S <> '' then FQuery.SQL.Add(S);
              SQLFilled := True;
            end
            else begin
              if (S = Term) then SQLFilled := True
              else if S <> '' then FQuery.SQL.Add(S);
            end;
          end
          else SQLFilled := True;
        until SQLFilled;
      finally
        FQuery.SQL.EndUpdate;
      end;
      if FQuery.SQL.Count > 0 then begin
        if (StatementNo < 0) or (StatementNo = CurrStatement) then begin
          StmtFound := True;
          CheckExecQuery(I - 1, CurrStatement);
          if StatementNo = CurrStatement then Break;
        end;
        Inc(CurrStatement);
      end;
    end;
    if not StmtFound then begin
{$IFDEF RX_D3}
      DatabaseError(Format(SListIndexError, [StatementNo]));
{$ELSE}
      DatabaseError(Format('%s: %d', [{LoadStr(}SListIndexError{)}, StatementNo]));
{$ENDIF RX_D3}
    end;
    if IsTrans then Database.Commit;
  except
    if IsTrans then Database.Rollback;
    raise;
  end;
end;

procedure TSQLScript.ExecStatement(StatementNo: Integer);
begin
  if FSQL.Count = 0 then _DBError(SEmptySQLStatement);
  FQuery.SetDBFlag(dbfExecScript, True);
  try
    if not Database.Connected then _DBError(SDatabaseClosed);
    if Assigned(FBeforeExec) then FBeforeExec(Self);
    ExecuteScript(StatementNo);
    if Assigned(FAfterExec) then FAfterExec(Self);
  finally
    FQuery.SetDBFlag(dbfExecScript, False);
  end;
end;

procedure TSQLScript.ExecSQL;
begin
  ExecStatement(-1);
end;

procedure TSQLScript.CreateParams(List: TParams; const Value: PChar);
begin
  CreateQueryParams(List, Value, False, ':', []);
end;

procedure TSQLScript.SetQuery(Value: TStrings);
begin
  TStringList(SQL).OnChange := nil;
  FSQL.Assign(Value);
  TStringList(SQL).OnChange := QueryChanged;
  QueryChanged(nil);
end;

function TSQLScript.GetText: {$IFNDEF VER80} string {$ELSE} PChar {$ENDIF};
begin
{$IFNDEF VER80}
  Result := SQL.Text;
{$ELSE}
  Result := SQL.GetText;
{$ENDIF}
end;

procedure TSQLScript.QueryChanged(Sender: TObject);
var
  List: TParams;
{$IFDEF VER80}
  P: PChar;
{$ENDIF}
begin
{$IFDEF RX_D4}
  if not (csReading in ComponentState) then begin
{$ENDIF RX_D4}
    List := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
    try
  {$IFNDEF VER80}
      CreateParams(List, PChar(Text));
  {$ELSE}
      P := GetText;
      try
        CreateParams(List, P);
      finally
        StrDispose(P);
      end;
  {$ENDIF}
      List.AssignValues(FParams);
  {$IFDEF RX_D4}
      FParams.Clear;
      FParams.Assign(List);
    finally
  {$ELSE}
      FParams.Free;
      FParams := List;
    except
  {$ENDIF RX_D4}
      List.Free;
    end;
{$IFDEF RX_D4}
  end
  else begin
    FParams.Clear;
    CreateParams(FParams, PChar(Text));
  end;
{$ENDIF RX_D4}
end;

function TSQLScript.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TSQLScript.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

function TSQLScript.GetParamsCount: Cardinal;
begin
  Result := FParams.Count;
end;

{$IFDEF RX_D4}
procedure TSQLScript.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, True);
end;

procedure TSQLScript.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TSQLScript.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;
{$ENDIF RX_D4}

end.