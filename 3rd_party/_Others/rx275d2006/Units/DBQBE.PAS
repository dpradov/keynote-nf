{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit DBQBE;

{*************************************************************************}
{ The Delphi TQBEQuery component.                                         }
{ This component derives from TDBDataSet and is much like TQuery except   }
{ the language used for Query is QBE (Query by example).                  }
{ You can create the QBE queries from Paradox or DatabaseDesktop and then }
{ load or paste the query strings in the QBE property of TQBEQuery.       }
{*************************************************************************}

{$I RX.INC}
{$N+,P+,S-}

interface

uses SysUtils, {$IFDEF WIN32} Windows, Bde, {$ELSE} WinTypes, WinProcs,
  DbiErrs, DbiTypes, DbiProcs, {$ENDIF} Classes, Controls, DB, DBTables;

const
  DefQBEStartParam = '#';

type
  TCheckType = (ctNone, ctCheck, ctCheckPlus, ctCheckDesc, ctCheckGroup);

{ TQBEQuery }

  TQBEQuery = class(TDBDataSet)
  private
    FStmtHandle: HDBIStmt;
    FQBE: TStrings;
    FPrepared: Boolean;
    FParams: TParams;
    FStartParam: Char;
    FAuxiliaryTables: Boolean;
{$IFDEF WIN32}
    FText: string;
    FRowsAffected: Integer;
{$ELSE}
    FText: PChar;
{$ENDIF}
{$IFDEF RX_D3}
    FConstrained: Boolean;
{$ENDIF}
    FLocal: Boolean;
    FRequestLive: Boolean;
    FBlankAsZero: Boolean;
    FParamCheck: Boolean;
    function CreateCursor(GenHandle: Boolean): HDBICur;
    procedure ReplaceParams(QBEText: TStrings);
    procedure CreateParams(List: TParams; const Value: PChar);
    procedure FreeStatement;
    function GetQueryCursor(GenHandle: Boolean): HDBICur;
    procedure GetStatementHandle(QBEText: PChar);
    procedure PrepareQBE(Value: PChar);
    procedure QueryChanged(Sender: TObject);
    procedure SetQuery(Value: TStrings);
    procedure SetParamsList(Value: TParams);
    procedure SetPrepared(Value: Boolean);
    procedure SetPrepare(Value: Boolean);
    procedure SetStartParam(Value: Char);
{$IFDEF RX_D4}
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);
{$ENDIF}
{$IFDEF WIN32}
    function GetRowsAffected: Integer;
{$ENDIF}
{$IFDEF RX_D5}
  protected
    { IProviderSupport }
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    procedure PSSetCommandText(const CommandText: string); override;
    procedure PSSetParams(AParams: TParams); override;
{$ENDIF}
  protected
    function CreateHandle: HDBICur; override;
    procedure Disconnect; override;
    function GetParamsCount: Word;
{$IFDEF RX_D4}
    procedure DefineProperties(Filer: TFiler); override;
{$ENDIF}
{$IFDEF RX_V110}
    function SetDBFlag(Flag: Integer; Value: Boolean): Boolean; override;
{$ELSE}
    procedure SetDBFlag(Flag: Integer; Value: Boolean); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetQBEText: PChar;
    procedure ExecQBE;
    function ParamByName(const Value: string): TParam;
    procedure Prepare;
    procedure RefreshQuery;
    procedure UnPrepare;
{$IFNDEF RX_D3}
    function IsEmpty: Boolean;
{$ENDIF}
    property Local: Boolean read FLocal;
    property ParamCount: Word read GetParamsCount;
    property Prepared: Boolean read FPrepared write SetPrepare;
    property StmtHandle: HDBIStmt read FStmtHandle;
{$IFDEF WIN32}
    property Text: string read FText;
    property RowsAffected: Integer read GetRowsAffected;
{$ELSE}
    property Text: PChar read FText;
{$ENDIF}
  published
{$IFDEF RX_D5}
    property AutoRefresh;
{$ENDIF}
    property AuxiliaryTables: Boolean read FAuxiliaryTables write FAuxiliaryTables default True;
    property ParamCheck: Boolean read FParamCheck write FParamCheck default True;
    property StartParam: Char read FStartParam write SetStartParam default DefQBEStartParam;
    { Ensure StartParam is declared before QBE }
    property QBE: TStrings read FQBE write SetQuery;
    { Ensure QBE is declared before Params }
    property BlankAsZero: Boolean read FBlankAsZero write FBlankAsZero default False;
    property Params: TParams read FParams write SetParamsList {$IFDEF RX_D4} stored False {$ENDIF};
    property RequestLive: Boolean read FRequestLive write FRequestLive default False;
    property UpdateMode;
{$IFDEF WIN32}
    property UpdateObject;
  {$IFDEF RX_D3}
    property Constrained: Boolean read FConstrained write FConstrained default False;
    property Constraints stored ConstraintsStored;
  {$ENDIF}
{$ENDIF}
  end;

implementation

uses DBConsts, {$IFDEF RX_D3} BDEConst, {$ENDIF} DBUtils, BdeUtils;

function NameDelimiter(C: Char): Boolean;
begin
  Result := C in [' ', ',', ';', ')', '.', #13, #10];
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := C in ['''', '"'];
end;

{ TQBEQuery }

constructor TQBEQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQBE := TStringList.Create;
  TStringList(QBE).OnChange := QueryChanged;
  FParams := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
  FStartParam := DefQBEStartParam;
  FParamCheck := True;
  FAuxiliaryTables:= True;
{$IFNDEF WIN32}
  FText := nil;
{$ELSE}
  FRowsAffected := -1;
{$ENDIF}
  FRequestLive := False;
end;

destructor TQBEQuery.Destroy;
begin
  Destroying;
  Disconnect;
  QBE.Free;
{$IFNDEF WIN32}
  StrDispose(FText);
{$ENDIF}
  FParams.Free;
  inherited Destroy;
end;

procedure TQBEQuery.Disconnect;
begin
  Close;
  UnPrepare;
end;

procedure TQBEQuery.RefreshQuery;
var
  Bookmark: TBookmark;
begin
  DisableControls;
  Bookmark := GetBookmark;
  try
    Close;
    Open;
    try
      GotoBookmark(Bookmark);
    except
      { ignore exceptions }
    end;
  finally
    FreeBookmark(Bookmark);
    EnableControls;
  end;
end;

procedure TQBEQuery.SetPrepare(Value: Boolean);
begin
  if Value then Prepare
  else UnPrepare;
end;

procedure TQBEQuery.Prepare;
begin
  SetDBFlag(dbfPrepared, True);
  SetPrepared(True);
end;

procedure TQBEQuery.UnPrepare;
begin
  SetPrepared(False);
  SetDBFlag(dbfPrepared, False);
end;

procedure TQBEQuery.SetStartParam(Value: Char);
begin
  if Value <> FStartParam then begin
    FStartParam := Value;
    QueryChanged(nil);
  end;
end;

procedure TQBEQuery.SetQuery(Value: TStrings);
begin
{$IFDEF WIN32}
  if QBE.Text <> Value.Text then begin
{$ENDIF}
    Disconnect;
    TStringList(QBE).OnChange := nil;
    QBE.Assign(Value);
    TStringList(QBE).OnChange := QueryChanged;
    QueryChanged(nil);
{$IFDEF WIN32}
  end;
{$ENDIF}
end;

procedure TQBEQuery.QueryChanged(Sender: TObject);
var
  List: TParams;
begin
{$IFDEF RX_D4}
  if not (csReading in ComponentState) then begin
{$ENDIF RX_D4}
    Disconnect;
  {$IFDEF WIN32}
    FText := QBE.Text;
  {$ELSE}
    StrDispose(FText);
    FText := QBE.GetText;
  {$ENDIF WIN32}
    if ParamCheck or (csDesigning in ComponentState) then begin
      List := TParams.Create{$IFDEF RX_D4}(Self){$ENDIF};
      try
        CreateParams(List, PChar(Text));
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
    end;
{$IFDEF RX_D4}
    DataEvent(dePropertyChange, 0);
  end
  else begin
    FText := QBE.Text;
    FParams.Clear;
    CreateParams(FParams, PChar(Text));
  end;
{$ENDIF RX_D4}
end;

procedure TQBEQuery.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{$IFDEF RX_D4}
procedure TQBEQuery.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, True);
end;

procedure TQBEQuery.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

procedure TQBEQuery.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;
{$ENDIF}

function TQBEQuery.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

procedure TQBEQuery.ReplaceParams(QBEText: TStrings);

  function ReplaceString(const S: string): string;
  var
    I, J, P, LiteralChars: Integer;
    Param: TParam;
    Temp: string;
    Found: Boolean;
  begin
    Result := S;
    for I := Params.Count - 1 downto 0 do begin
      Param := Params[I];
      if Param.DataType = ftUnknown then
        Continue; { ignore undefined params }
      repeat
        P := Pos(StartParam + Param.Name, Result);
        Found := (P > 0) and ((Length(Result) = P + Length(Param.Name)) or
          NameDelimiter(Result[P + Length(Param.Name) + 1]));
        if Found then begin
          LiteralChars := 0;
          for J := 1 to P - 1 do
            if IsLiteral(Result[J]) then Inc(LiteralChars);
          Found := LiteralChars mod 2 = 0;
          if Found then begin
            Temp := Param.Text;
            if Temp = '' then begin
              if (Param.DataType = ftString) and not Param.IsNull then
                Temp := '""'
              else Temp := 'BLANK'; { special QBE operator }
            end;
            Result := Copy(Result, 1, P - 1) + Temp + Copy(Result,
              P + Length(Param.Name) + 1, MaxInt);
          end;
        end;
      until not Found;
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to QBEText.Count - 1 do
    QBEText[I] := ReplaceString(QBEText[I]);
end;

procedure TQBEQuery.SetPrepared(Value: Boolean);
var
  TempQBE: TStrings;
  AText: PChar;
begin
  if Handle <> nil then _DBError(SDataSetOpen);
  if (Value <> Prepared) or (ParamCount > 0) then begin
    if Value then begin
{$IFDEF WIN32}
      FRowsAffected := -1;
{$ENDIF}
      if ParamCount > 0 then begin
        TempQBE := TStringList.Create;
        try
          TempQBE.Assign(QBE);
          ReplaceParams(TempQBE);
{$IFDEF WIN32}
          AText := PChar(TempQBE.Text);
{$ELSE}
          AText := TempQBE.GetText;
{$ENDIF}
          try
            FreeStatement;
            if StrLen(AText) > 1 then PrepareQBE(AText)
            else _DBError(SEmptySQLStatement);
          finally
{$IFNDEF WIN32}
            StrDispose(AText);
{$ENDIF}
          end;
        finally
          TempQBE.Free;
        end;
      end
      else begin
        if StrLen(PChar(Text)) > 1 then PrepareQBE(PChar(Text))
        else _DBError(SEmptySQLStatement);
      end;
    end
    else begin
{$IFDEF WIN32}
      FRowsAffected := RowsAffected;
{$ENDIF}
      FreeStatement;
    end;
    FPrepared := Value;
  end;
end;

procedure TQBEQuery.FreeStatement;
begin
  if StmtHandle <> nil then begin
    DbiQFree(FStmtHandle);
    FStmtHandle := nil;
  end;
end;

function TQBEQuery.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

procedure TQBEQuery.CreateParams(List: TParams; const Value: PChar);
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
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = FStartParam) and not Literal and
      ((CurPos + 1)^ <> FStartParam) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar)) do
      begin
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
{$IFDEF RX_D4}
      if List.FindParam(Name) = nil then
{$ENDIF}
        List.CreateParam(ftUnknown, Name, ptUnknown);
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = FStartParam) and not Literal
      and ((CurPos + 1)^ = FStartParam) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{$IFNDEF RX_D3}
function TQBEQuery.IsEmpty: Boolean;
begin
  Result := IsDataSetEmpty(Self);
end;
{$ENDIF}

function TQBEQuery.CreateCursor(GenHandle: Boolean): HDBICur;
begin
  if QBE.Count > 0 then begin
    SetPrepared(True);
    Result := GetQueryCursor(GenHandle);
  end
  else Result := nil;
end;

function TQBEQuery.CreateHandle: HDBICur;
begin
  Result := CreateCursor(True)
end;

procedure TQBEQuery.ExecQBE;
begin
  CheckInActive;
  SetDBFlag(dbfExecSQL, True);
  try
    CreateCursor(False);
  finally
    SetDBFlag(dbfExecSQL, False);
  end;
end;

function TQBEQuery.GetQueryCursor(GenHandle: Boolean): HDBICur;
var
  PCursor: phDBICur;
begin
  Result := nil;
  if GenHandle then PCursor := @Result
  else PCursor := nil;
  Check(DbiQExec(StmtHandle, PCursor));
end;

{$IFDEF RX_V110}
function TQBEQuery.SetDBFlag(Flag: Integer; Value: Boolean): Boolean;
{$ELSE}
procedure TQBEQuery.SetDBFlag(Flag: Integer; Value: Boolean);
{$ENDIF}
var
  NewConnection: Boolean;
begin
  if Value then begin
    NewConnection := DBFlags = [];
{$IFDEF RX_V110}
    Result := inherited SetDBFlag(Flag, Value);
{$ELSE}
    inherited SetDBFlag(Flag, Value);
{$ENDIF}
    if not (csReading in ComponentState) and NewConnection then
      FLocal := not Database.IsSQLBased;
  end
  else begin
    if DBFlags - [Flag] = [] then SetPrepared(False);
{$IFDEF RX_V110}
    Result := inherited SetDBFlag(Flag, Value);
{$ELSE}
    inherited SetDBFlag(Flag, Value);
{$ENDIF}
  end;
end;

procedure TQBEQuery.PrepareQBE(Value: PChar);
begin
  GetStatementHandle(Value);
end;

procedure TQBEQuery.GetStatementHandle(QBEText: PChar);
const
  DataType: array[Boolean] of LongInt = (Ord(wantCanned), Ord(wantLive));
begin
{$IFDEF WIN32}
  Check(DbiQAlloc(DBHandle, qrylangQBE, FStmtHandle));
  try
    Check(DBiSetProp(hDbiObj(StmtHandle), stmtLIVENESS,
      DataType[RequestLive and not ForceUpdateCallback]));
    Check(DBiSetProp(hDbiObj(StmtHandle), stmtAUXTBLS, Longint(FAuxiliaryTables)));
{$IFDEF RX_D3}
    if Local and RequestLive and Constrained then
      Check(DBiSetProp(hDbiObj(StmtHandle), stmtCONSTRAINED, LongInt(True)));
{$ENDIF}
    if FBlankAsZero then
      Check(DbiSetProp(hDbiObj(StmtHandle), stmtBLANKS, Longint(True)));
    while not CheckOpen(DbiQPrepare(FStmtHandle, QBEText)) do {Retry};
  except
    DbiQFree(FStmtHandle);
    FStmtHandle := nil;
    raise;
  end;
{$ELSE}
  if Local then begin
    while not CheckOpen(DbiQPrepare(DBHandle, qrylangQBE, QBEText,
      FStmtHandle)) do {Retry};
    Check(DBiSetProp(hDbiObj(StmtHandle), stmtLIVENESS, DataType[RequestLive]));
  end
  else begin
    if RequestLive then
      Check(DbiQPrepareExt(DBHandle, qrylangQBE, QBEText, qprepFORUPDATE, FStmtHandle))
    else Check(DbiQPrepare(DBHandle, qrylangQBE, QBEText, FStmtHandle));
  end;
  Check(DBiSetProp(hDbiObj(StmtHandle), stmtAUXTBLS, Longint(FAuxiliaryTables)));
  if FBlankAsZero then
    Check(DbiSetProp(hDbiObj(StmtHandle), stmtBLANKS, LongInt(True)));
{$ENDIF}
end;

function TQBEQuery.GetQBEText: PChar;
var
  BufLen: Word;
  I: Integer;
  StrEnd: PChar;
  StrBuf: array[0..255] of Char;
begin
  BufLen := 1;
  for I := 0 to QBE.Count - 1 do
    Inc(BufLen, Length(QBE.Strings[I]) + 1);
  Result := StrAlloc(BufLen);
  try
    StrEnd := Result;
    for I := 0 to QBE.Count - 1 do begin
      StrPCopy(StrBuf, QBE.Strings[I]);
      StrEnd := StrECopy(StrEnd, StrBuf);
      StrEnd := StrECopy(StrEnd, ' ');
    end;
  except
    StrDispose(Result);
    raise;
  end;
end;

{$IFDEF WIN32}
function TQBEQuery.GetRowsAffected: Integer;
var
  Length: Word;
begin
  if Prepared then
    if DbiGetProp(hDBIObj(StmtHandle), stmtROWCOUNT, @Result, SizeOf(Result),
      Length) <> 0 then Result := -1
    else
  else Result := FRowsAffected;
end;
{$ENDIF}

{$IFDEF RX_D5}

{ TQBEQuery.IProviderSupport }

function TQBEQuery.PSGetParams: TParams;
begin
  Result := Params;
end;

procedure TQBEQuery.PSSetParams(AParams: TParams);
begin
  if AParams.Count <> 0 then
    Params.Assign(AParams);
  Close;
end;

procedure TQBEQuery.PSExecute;
begin
  ExecQBE;
end;

procedure TQBEQuery.PSSetCommandText(const CommandText: string);
begin
  if CommandText <> '' then
    QBE.Text := CommandText;
end;

{$ENDIF RX_D5}

end.