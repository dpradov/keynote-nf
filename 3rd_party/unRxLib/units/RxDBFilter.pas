{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and component added by JB.                   }
{*******************************************************}

unit RxDBFilter;

interface

{$I RX.INC}
{$T-}

{$IFNDEF VER80}
uses
  SysUtils, Windows, Messages, Classes, Controls, Forms, RxDBCtrl,
  Graphics, Menus, StdCtrls, ExtCtrls, Bde, DB, DBTables;
{$ELSE}
uses SysUtils, WinTypes, WinProcs, Messages, Classes, Controls, Forms,
  Graphics, Menus, StdCtrls, ExtCtrls, DBITypes, DB, DBTables;
{$ENDIF}

type

{ TRxDBFilter }

  TFilterLogicCond = (flAnd, flOr); { for captured DataSet }
  {$IFNDEF VER80}
  {$IFDEF RX_D6}
  TDBFilterOption = TFilterOption;
  TDBFilterOptions = TFilterOptions;
  {$ELSE}
  TDBFilterOption = type TFilterOption;
  TDBFilterOptions = type TFilterOptions;
  {$ENDIF}
  {$ELSE}
  TDBFilterOption = (foCaseInsensitive, foNoPartialCompare);
  TDBFilterOptions = set of TDBFilterOption;
  {$ENDIF}
  TFilterEvent = function(Sender: TObject; DataSet: TDataSet): Boolean of object;

  TDataSetStorage = record { for internal use only }
    FBof: Boolean;
    FEof: Boolean;
    State: TDataSetState;
    CanModify: Boolean;
    BeforePost: TDataSetNotifyEvent;
    BeforeCancel: TDataSetNotifyEvent;
    BeforeInsert: TDataSetNotifyEvent;
    BeforeEdit: TDataSetNotifyEvent;
  end;

  TRxDBFilter = class(TComponent)
  private
    FParser: TObject;
    FDataLink: TDataLink;
    FIgnoreDataEvents: Boolean;
    FPriority: Word;
    FOptions: TDBFilterOptions;
    FLogicCond: TFilterLogicCond;
    FFilter: TStrings;
    FExprHandle: hDBIFilter;
    FFuncHandle: hDBIFilter;
    FDataHandle: hDBICur;
    FActive: Boolean;
    FCaptured: Boolean;
    FStreamedActive: Boolean;
    FActivating: Boolean;
    FStorage: TDataSetStorage;
    FOnFiltering: TFilterEvent;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnSetCapture: TNotifyEvent;
    FOnReleaseCapture: TNotifyEvent;
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource;
    function BuildTree: Boolean;
    procedure DestroyTree;
    procedure SetFilter(Value: TStrings);
    procedure SetOptions(Value: TDBFilterOptions);
    procedure SetOnFiltering(const Value: TFilterEvent);
    procedure SetPriority(Value: Word);
    procedure SetLogicCond(Value: TFilterLogicCond);
    function GetFilterText: PChar;
    procedure FilterChanged(Sender: TObject);
    function CreateExprFilter: hDBIFilter;
    function CreateFuncFilter: hDBIFilter;
    procedure DropFilters;
    procedure SetFilterHandle(var Filter: HDBIFilter; Value: HDBIFilter);
    procedure RecreateExprFilter;
    procedure RecreateFuncFilter;
    procedure ActivateFilters;
    procedure DeactivateFilters;
    function RecordFilter(RecBuf: Pointer; RecNo: LongInt): SmallInt; {$IFNDEF VER80} stdcall; {$ENDIF}
    procedure BeforeDataPost(DataSet: TDataSet);
    procedure BeforeDataChange(DataSet: TDataSet);
    procedure BeforeDataCancel(DataSet: TDataSet);
    procedure SetActive(Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoActivate; dynamic;
    procedure DoDeactivate; dynamic;
    procedure ActiveChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure UpdateFuncFilter;
    procedure Activate;
    procedure Deactivate;
    procedure SetCapture;
    procedure ReleaseCapture;
    procedure ReadCaptureControls;
    property Captured: Boolean read FCaptured;
    property Handle: hDBIFilter read FExprHandle; { obsolete, use ExprFilter }
    property ExprFilter: hDBIFilter read FExprHandle;
    property FuncFilter: hDBIFilter read FFuncHandle;
  published
    property Active: Boolean read FActive write SetActive default False;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Filter: TStrings read FFilter write SetFilter;
    property LogicCond: TFilterLogicCond read FLogicCond write SetLogicCond default flAnd;
    property Options: TDBFilterOptions read FOptions write SetOptions default [];
    property Priority: Word read FPriority write SetPriority default 0;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnFiltering: TFilterEvent read FOnFiltering write SetOnFiltering;
    property OnSetCapture: TNotifyEvent read FOnSetCapture write FOnSetCapture;
    property OnReleaseCapture: TNotifyEvent read FOnReleaseCapture write FOnReleaseCapture;
  end;

  EFilterError = class(Exception);

procedure DropAllFilters(DataSet: TDataSet);
{$IFNDEF RX_D3}
function SetLookupFilter(DataSet: TDataSet; Field: TField;
  const Value: string; CaseSensitive, Exact: Boolean): HDBIFilter;
{$ENDIF}

{  TRxDBGridSorter  }
type
  TRxDBGridSorter = class(TComponent)
  private
    { Private declarations }
    FRxDBGrid: TRxDBGrid;
    FMarkers: Boolean;
    sm: TSortMarker;
    OldFName, FName: string;
    FORDERstring: string;

    procedure DoGridSorted(G: TRxDBGrid);
    procedure MakeGridSorted(G: TRxDBGrid);
    procedure SortIt;
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    {--demonstration code-->}
    procedure TitleButtonClick(Sender: TObject; ACol: Integer; Field: TField);
    procedure DBGridGetBtnParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
      IsDown: Boolean);
    {<--demonstration code--}
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property RxDBGrid: TRxDBGrid read FRxDBGrid write FRxDBGrid;
    property Markers: Boolean read FMarkers write FMarkers;
    property OrderString: string read FORDERstring write FORDERstring;
  end;

implementation

uses
  {$IFDEF VER80}DBIErrs, DBIProcs, Str16, {$ENDIF}DBConsts, Dialogs,
  {$IFDEF RX_D3}DbCommon, {$ENDIF}RxResConst, RxVCLUtils, RxDBUtils, RxBdeUtils;

procedure DropAllFilters(DataSet: TDataSet);
begin
  if (DataSet <> nil) and DataSet.Active then
  begin
    {$IFNDEF VER80}
    DataSet.Filtered := False;
    {$ENDIF}
    DbiDropFilter((DataSet as TBDEDataSet).Handle, nil);
    DataSet.CursorPosChanged;
    DataSet.Resync([]);
  end;
end;

{ DBFilter exceptions }

procedure FilterError(Ident: string {Word}); near;
begin
  raise EFilterError.Create {Res}(Ident);
end;

procedure FilterErrorFmt(Ident: string {Word}; const Args: array of const); near;
begin
  raise EFilterError.CreateFmt(Ident, Args);
  //CreateResFmt(Ident, Args);
end;

const
  SExprNothing = '""'; { nothing token name          }
  cQuota = ''''; { qoutas for string constants }
  cFldQuotaLeft = '['; { left qouta for field names  }
  cFldQuotaRight = ']'; { right qouta for field names }

  {$IFNDEF RX_D3} {DbCommon.pas}

{ TFilterExpr }

type
  TExprNodeKind = (enField, enConst, enOperator);

  PExprNode = ^TExprNode;
  TExprNode = record
    FNext: PExprNode;
    FKind: TExprNodeKind;
    FPartial: Boolean;
    FOperator: CanOp;
    FData: string;
    FLeft: PExprNode;
    FRight: PExprNode;
  end;

  TFilterExpr = class
  private
    FDataSet: TDataSet;
    FOptions: TDBFilterOptions;
    FNodes: PExprNode;
    FExprBuffer: PCANExpr;
    FExprBufSize: Integer;
    FExprNodeSize: Integer;
    FExprDataSize: Integer;
    function FieldFromNode(Node: PExprNode): TField;
    function GetExprData(Pos, Size: Integer): PChar;
    function PutCompareNode(Node: PExprNode): Integer;
    function PutConstStr(const Value: string): Integer;
    function PutConstNode(DataType: Integer; Data: PChar;
      Size: Integer): Integer;
    function PutData(Data: PChar; Size: Integer): Integer;
    function PutExprNode(Node: PExprNode): Integer;
    function PutFieldNode(Field: TField): Integer;
    function PutNode(NodeType: NodeClass; OpType: CanOp;
      OpCount: Integer): Integer;
    procedure SetNodeOp(Node, Index, Data: Integer);
  public
    constructor Create(DataSet: TDataSet; Options: TDBFilterOptions);
    destructor Destroy; override;
    function NewCompareNode(Field: TField; Operator: CanOp;
      const Value: string): PExprNode;
    function NewNode(Kind: TExprNodeKind; Operator: CanOp;
      const Data: string; Left, Right: PExprNode): PExprNode;
    function GetFilterData(Root: PExprNode): PCANExpr;
  end;

constructor TFilterExpr.Create(DataSet: TDataSet; Options: TDBFilterOptions);
begin
  FDataSet := DataSet;
  FOptions := Options;
end;

destructor TFilterExpr.Destroy;
var
  Node: PExprNode;
begin
  if (FExprBuffer <> nil) then FreeMem(FExprBuffer, FExprBufSize);
  while FNodes <> nil do
  begin
    Node := FNodes;
    FNodes := Node^.FNext;
    Dispose(Node);
  end;
end;

function TFilterExpr.FieldFromNode(Node: PExprNode): TField;
begin
  Result := FDataSet.FieldByName(Node^.FData);
  if Result.Calculated then
    FilterErrorFmt(SExprBadField, [Result.FieldName]);
end;

function TFilterExpr.GetExprData(Pos, Size: Integer): PChar;
begin
  {$IFNDEF VER80}
  ReallocMem(FExprBuffer, FExprBufSize + Size);
  {$ELSE}
  FExprBuffer := ReallocMem(FExprBuffer, FExprBufSize, FExprBufSize + Size);
  {$ENDIF}
  Move(PChar(FExprBuffer)[Pos], PChar(FExprBuffer)[Pos + Size],
    FExprBufSize - Pos);
  Inc(FExprBufSize, Size);
  Result := PChar(FExprBuffer) + Pos;
end;

function TFilterExpr.GetFilterData(Root: PExprNode): PCANExpr;
begin
  FExprBufSize := SizeOf(CANExpr);
  GetMem(FExprBuffer, FExprBufSize);
  PutExprNode(Root);
  with FExprBuffer^ do
  begin
    iVer := CANEXPRVERSION;
    iTotalSize := FExprBufSize;
    iNodes := $FFFF;
    iNodeStart := SizeOf(CANExpr);
    iLiteralStart := FExprNodeSize + SizeOf(CANExpr);
  end;
  Result := FExprBuffer;
end;

function TFilterExpr.NewCompareNode(Field: TField; Operator: CanOp;
  const Value: string): PExprNode;
var
  Left, Right: PExprNode;
begin
  Left := NewNode(enField, canNOTDEFINED, Field.FieldName, nil, nil);
  Right := NewNode(enConst, canNOTDEFINED, Value, nil, nil);
  Result := NewNode(enOperator, Operator, EmptyStr, Left, Right);
end;

function TFilterExpr.NewNode(Kind: TExprNodeKind; Operator: CanOp;
  const Data: string; Left, Right: PExprNode): PExprNode;
begin
  New(Result);
  with Result^ do
  begin
    FNext := FNodes;
    FKind := Kind;
    FPartial := False;
    FOperator := Operator;
    FData := Data;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
end;

function TFilterExpr.PutCompareNode(Node: PExprNode): Integer;
const
  ReverseOperator: array[canEQ..canLE] of CanOp = (
    canEQ, canNE, canLT, canGT, canLE, canGE);
var
  Operator: CanOp;
  Left, Right, Temp: PExprNode;
  Field: TField;
  FieldPos, ConstPos, CaseInsensitive, PartialLength, L: Integer;
  S: string;
  Buf: PChar;
begin
  Operator := Node^.FOperator;
  Left := Node^.FLeft;
  Right := Node^.FRight;
  if (Left^.FKind <> enConst) and (Right^.FKind <> enConst) then
  begin
    if FDataSet.FindField(Left^.FData) = nil then
      Left^.FKind := enConst
    else if FDataSet.FindField(Right^.FData) = nil then
      Right^.FKind := enConst;
  end;
  if (Left^.FKind <> enField) and (Right^.FKind <> enField) then
  begin
    if FDataSet.FindField(Left^.FData) <> nil then
      Left^.FKind := enField
    else if FDataSet.FindField(Right^.FData) <> nil then
      Right^.FKind := enField;
  end;
  if Right^.FKind = enField then
  begin
    Temp := Left;
    Left := Right;
    Right := Temp;
    Operator := ReverseOperator[Operator];
  end;
  if (Left^.FKind <> enField) or (Right^.FKind <> enConst) then
    FilterError(SExprBadCompare);
  Field := FieldFromNode(Left);
  if Right^.FData = EmptyStr then
  begin
    case Operator of
      canEQ: Operator := canISBLANK;
      canNE: Operator := canNOTBLANK;
    else
      FilterError(SExprBadNullTest);
    end;
    Result := PutNode(nodeUNARY, Operator, 1);
    SetNodeOp(Result, 0, PutFieldNode(Field));
  end
  else
  begin
    if ((Operator = canEQ) or (Operator = canNE)) and
      (Field.DataType = ftString) then
    begin
      S := Right^.FData;
      L := Length(S);
      if L <> 0 then
      begin
        CaseInsensitive := 0;
        PartialLength := 0;
        if foCaseInsensitive in FOptions then CaseInsensitive := 1;
        if Node^.FPartial then
          PartialLength := L
        else
        begin
          if not (foNoPartialCompare in FOptions) and (L > 1) and
            (S[L] = '*') then
          begin
            Delete(S, L, 1);
            PartialLength := L - 1;
          end;
        end;
        if (CaseInsensitive <> 0) or (PartialLength <> 0) then
        begin
          Result := PutNode(nodeCOMPARE, Operator, 4);
          SetNodeOp(Result, 0, CaseInsensitive);
          SetNodeOp(Result, 1, PartialLength);
          SetNodeOp(Result, 2, PutFieldNode(Field));
          SetNodeOp(Result, 3, PutConstStr(S));
          Exit;
        end;
      end;
    end;
    Result := PutNode(nodeBINARY, Operator, 2);
    FieldPos := PutFieldNode(Field);
    S := Right^.FData;
    Buf := AllocMem(Field.DataSize);
    try
      ConvertStringToLogicType((FDataSet as TBDEDataSet).Locale,
        FieldLogicMap(Field.DataType), Field.DataSize, Field.FieldName,
        Right^.FData, Buf);
      ConstPos := PutConstNode(FieldLogicMap(Field.DataType), Buf,
        Field.DataSize);
      SetNodeOp(Result, 0, FieldPos);
      SetNodeOp(Result, 1, ConstPos);
    finally
      FreeMem(Buf, Field.DataSize);
    end;
  end;
end;

function TFilterExpr.PutConstNode(DataType: Integer; Data: PChar;
  Size: Integer): Integer;
begin
  Result := PutNode(nodeCONST, canCONST2, 3);
  SetNodeOp(Result, 0, DataType);
  SetNodeOp(Result, 1, Size);
  SetNodeOp(Result, 2, PutData(Data, Size));
end;

function TFilterExpr.PutConstStr(const Value: string): Integer;
var
  Buffer: array[0..255] of Char;
begin
  AnsiToNative((FDataSet as TBDEDataSet).Locale, Value, Buffer,
    SizeOf(Buffer) - 1);
  Result := PutConstNode(fldZSTRING, Buffer, StrLen(Buffer) + 1);
end;

function TFilterExpr.PutData(Data: PChar; Size: Integer): Integer;
begin
  Move(Data^, GetExprData(FExprBufSize, Size)^, Size);
  Result := FExprDataSize;
  Inc(FExprDataSize, Size);
end;

function TFilterExpr.PutExprNode(Node: PExprNode): Integer;
const
  BoolFalse: WordBool = False;
var
  Field: TField;
begin
  Result := 0;
  case Node^.FKind of
    enField:
      begin
        Field := FieldFromNode(Node);
        if Field.DataType <> ftBoolean then
          FilterErrorFmt(SExprNotBoolean, [Field.FieldName]);
        Result := PutNode(nodeBINARY, canNE, 2);
        SetNodeOp(Result, 0, PutFieldNode(Field));
        SetNodeOp(Result, 1, PutConstNode(fldBOOL, @BoolFalse,
          SizeOf(WordBool)));
      end;
    enOperator:
      case Node^.FOperator of
        canEQ..canLE:
          Result := PutCompareNode(Node);
        canAND, canOR:
          begin
            Result := PutNode(nodeBINARY, Node^.FOperator, 2);
            SetNodeOp(Result, 0, PutExprNode(Node^.FLeft));
            SetNodeOp(Result, 1, PutExprNode(Node^.FRight));
          end;
      else
        Result := PutNode(nodeUNARY, canNOT, 1);
        SetNodeOp(Result, 0, PutExprNode(Node^.FLeft));
      end; { case Node^.FOperator }
  else
    FilterError(SExprIncorrect);
  end; { case Node^.FKind }
end;

function TFilterExpr.PutFieldNode(Field: TField): Integer;
var
  Buffer: array[0..255] of Char;
begin
  AnsiToNative((FDataSet as TBDEDataSet).Locale, Field.FieldName, Buffer,
    SizeOf(Buffer) - 1);
  Result := PutNode(nodeFIELD, canFIELD2, 2);
  SetNodeOp(Result, 0, Field.FieldNo);
  SetNodeOp(Result, 1, PutData(Buffer, StrLen(Buffer) + 1));
end;

function TFilterExpr.PutNode(NodeType: NodeClass; OpType: CanOp;
  OpCount: Integer): Integer;
var
  Size: Integer;
begin
  Size := SizeOf(CANHdr) + OpCount * SizeOf(Word);
  with PCANHdr(GetExprData(SizeOf(CANExpr) + FExprNodeSize, Size))^ do
  begin
    nodeClass := NodeType;
    canOp := OpType;
  end;
  Result := FExprNodeSize;
  Inc(FExprNodeSize, Size);
end;

procedure TFilterExpr.SetNodeOp(Node, Index, Data: Integer);
begin
  PWordArray(PChar(FExprBuffer) + (SizeOf(CANExpr) + Node +
    SizeOf(CANHdr)))^[Index] := Data;
end;

{ SetLookupFilter }

function SetLookupFilter(DataSet: TDataSet; Field: TField;
  const Value: string; CaseSensitive, Exact: Boolean): HDBIFilter;
var
  Options: TDBFilterOptions;
  Filter: TFilterExpr;
  Node: PExprNode;
begin
  if not CaseSensitive then
    Options := [foNoPartialCompare, foCaseInsensitive]
  else
    Options := [foNoPartialCompare];
  Filter := TFilterExpr.Create(DataSet, Options);
  try
    Node := Filter.NewCompareNode(Field, canEQ, Value);
    if not Exact then Node^.FPartial := True;
    Check(DbiAddFilter((DataSet as TBDEDataSet).Handle, 0, 2, False,
      Filter.GetFilterData(Node), nil, Result));
    DataSet.CursorPosChanged;
    DataSet.Resync([]);
  finally
    Filter.Free;
  end;
end;

{ TExprParser }

type
  TExprToken = (etEnd, etSymbol, etName, etLiteral, etLParen, etRParen,
    etEQ, etNE, etGE, etLE, etGT, etLT);

  TExprParser = class
  private
    FFilter: TFilterExpr;
    FText: PChar;
    FSourcePtr: PChar;
    FTokenPtr: PChar;
    FTokenString: string;
    FToken: TExprToken;
    FFilterData: PCANExpr;
    FDataSize: Integer;
    procedure NextToken;
    function ParseExpr: PExprNode;
    function ParseExpr2: PExprNode;
    function ParseExpr3: PExprNode;
    function ParseExpr4: PExprNode;
    function ParseExpr5: PExprNode;
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
  public
    constructor Create(DataSet: TDataSet; const Text: PChar;
      Options: TDBFilterOptions);
    destructor Destroy; override;
    property FilterData: PCANExpr read FFilterData;
    property DataSize: Integer read FDataSize;
  end;

constructor TExprParser.Create(DataSet: TDataSet; const Text: PChar;
  Options: TDBFilterOptions);
var
  Root: PExprNode;
begin
  FFilter := TFilterExpr.Create(DataSet, Options);
  FText := Text;
  FSourcePtr := Text;
  NextToken;
  Root := ParseExpr;
  if FToken <> etEnd then FilterError(SExprTermination);
  FFilterData := FFilter.GetFilterData(Root);
  FDataSize := FFilter.FExprBufSize;
end;

destructor TExprParser.Destroy;
begin
  FFilter.Free;
end;

procedure TExprParser.NextToken;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;

begin
  FTokenString := '';
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do
    Inc(P);
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$FE:
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
      end;
    cFldQuotaLeft:
      begin
        Inc(P);
        TokenStart := P;
        while (P^ <> cFldQuotaRight) and (P^ <> #0) do
          Inc(P);
        if P^ = #0 then FilterError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    cQuota: { '''' }
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then FilterError(SExprStringError);
          if P^ = cQuota then
          begin
            Inc(P);
            if P^ <> cQuota then Break;
          end;
          if L < SizeOf(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etLiteral;
      end;
    '-', '0'..'9':
      begin
        TokenStart := P;
        Inc(P);
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
          Inc(P);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etLiteral;
      end;
    '(':
      begin
        Inc(P);
        FToken := etLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := etRParen;
      end;
    '<':
      begin
        Inc(P);
        case P^ of
          '=':
            begin
              Inc(P);
              FToken := etLE;
            end;
          '>':
            begin
              Inc(P);
              FToken := etNE;
            end;
        else
          FToken := etLT;
        end;
      end;
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then
        begin
          Inc(P);
          FToken := etGE;
        end
        else
          FToken := etGT;
      end;
    #0: FToken := etEnd;
  else
    FilterErrorFmt(SExprInvalidChar, [P^]);
  end;
  FSourcePtr := P;
end;

function TExprParser.ParseExpr: PExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canOR, EmptyStr,
      Result, ParseExpr2);
  end;
end;

function TExprParser.ParseExpr2: PExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canAND, EmptyStr,
      Result, ParseExpr3);
  end;
end;

function TExprParser.ParseExpr3: PExprNode;
begin
  if TokenSymbolIs('NOT') then
  begin
    NextToken;
    Result := FFilter.NewNode(enOperator, canNOT, EmptyStr,
      ParseExpr4, nil);
  end
  else
    Result := ParseExpr4;
end;

function TExprParser.ParseExpr4: PExprNode;
const
  Operators: array[etEQ..etLT] of CanOp = (
    canEQ, canNE, canGE, canLE, canGT, canLT);
var
  Operator: CanOp;
begin
  Result := ParseExpr5;
  if FToken in [etEQ..etLT] then
  begin
    Operator := Operators[FToken];
    NextToken;
    Result := FFilter.NewNode(enOperator, Operator, EmptyStr,
      Result, ParseExpr5);
  end;
end;

function TExprParser.ParseExpr5: PExprNode;
begin
  Result := nil;
  case FToken of
    etSymbol:
      if TokenSymbolIs('NULL') then
        Result := FFilter.NewNode(enConst, canNOTDEFINED, EmptyStr, nil, nil)
      else
        Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
    etName:
      Result := FFilter.NewNode(enField, canNOTDEFINED, FTokenString, nil, nil);
    etLiteral:
      Result := FFilter.NewNode(enConst, canNOTDEFINED, FTokenString, nil, nil);
    etLParen:
      begin
        NextToken;
        Result := ParseExpr;
        if FToken <> etRParen then FilterErrorFmt(SExprNoRParen, [TokenName]);
      end;
  else
    FilterErrorFmt(SExprExpected, [TokenName]);
  end;
  NextToken;
end;

function TExprParser.TokenName: string;
begin
  if (FSourcePtr = FTokenPtr) then
    Result := SExprNothing
  else
  begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TExprParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (CompareText(FTokenString, S) = 0);
end;

{$ENDIF RX_D3} {DbCommon.pas}

{$IFNDEF VER80}
{$HINTS OFF}
{$ENDIF}

type
  THackDataSet = class(TDataSet);

{ TNastyDataSet }

{*******************************************************}
{ !! ATTENTION Nasty implementation                     }
{*******************************************************}
{                                                       }
{ These class definitions were copied from TDataSet     }
{ (DB.PAS) and TBDEDataSet (DBTABLES.PAS).              }
{ It is needed to access FState, FBOF, FEOF, FBuffers,  }
{ FRecordCount, FActiveRecord, FCanModify private       }
{ fields of TDataSet.                                   }
{                                                       }
{ Any changes in the underlying classes may cause       }
{ errors in this implementation!                        }
{                                                       }
{*******************************************************}

  {$IFDEF RX_D3}

  {$IFDEF RX_D4}

  PBufferList = TBufferList;

  TNastyDataSet = class(TComponent)
  private
    FFields: TFields;
    FAggFields: TFields;
    FFieldDefs: TFieldDefs;
    FFieldDefList: TFieldDefList;
    FFieldList: TFieldList;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: TBufferList;
    FCalcBuffer: PChar;
    FBookmarkSize: Integer;
    FCalcFieldsSize: Integer;
    FDesigner: TDataSetDesigner;
    FDisableCount: Integer;
    FBlobFieldCount: Integer;
    FFilterText: string;
    FBlockReadSize: Integer;
    FConstraints: TCheckConstraints;
    FDataSetField: TDataSetField;
    FNestedDataSets: TList;
    FNestedDatasetClass: TClass;
    FReserved: Pointer;
    FFieldNoOfs: Integer;
    { Byte sized data members (for alignment) }
    FFilterOptions: TFilterOptions;
    FState: TDataSetState;
    FEnableEvent: TDataEvent;
    FDisableState: TDataSetState;
    FBOF: Boolean;
    FEOF: Boolean;
  end;

  TBDENastyDataSet = class(TDataSet)
  private
    FHandle: HDBICur;
    FStmtHandle: HDBIStmt;
    FRecProps: RecProps;
    FLocale: TLocale;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldMap: DBIKey;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
  end;

  {$ELSE RX_D4}

  TNastyDataSet = class(TComponent)
  private
    FFields: TList;
    FFieldDefs: TFieldDefs;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: PBufferList;
    FCalcBuffer: PChar;
    FBufListSize: Integer;
    FBookmarkSize: Integer;
    FCalcFieldsSize: Integer;
    FBOF: Boolean;
    FEOF: Boolean;
    FModified: Boolean;
    FStreamedActive: Boolean;
    FInternalCalcFields: Boolean;
    FState: TDataSetState;
  end;

  TBDENastyDataSet = class(TDataSet)
  private
    FHandle: HDBICur;
    FRecProps: RecProps;
    FLocale: TLocale;
    FExprFilter: HDBIFilter;
    FFuncFilter: HDBIFilter;
    FFilterBuffer: PChar;
    FIndexFieldMap: DBIKey;
    FExpIndex: Boolean;
    FCaseInsIndex: Boolean;
    FCachedUpdates: Boolean;
    FInUpdateCallback: Boolean;
    FCanModify: Boolean;
  end;

  {$ENDIF RX_D4}

  {$ELSE RX_D3}

  TNastyDataSet = class(TComponent)
  private
    FFields: TList;
    FDataSources: TList;
    FFieldDefs: TFieldDefs;
    FBuffers: PBufferList;
    FBufListSize: Integer;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FHandle: HDBICur;
    FBOF: Boolean;
    FEOF: Boolean;
    FState: TDataSetState;
    FAutoCalcFields: Boolean;
    FDefaultFields: Boolean;
    FCanModify: Boolean;
  end;
  TBDENastyDataSet = TNastyDataSet;

  {$ENDIF RX_D3}

  {$IFNDEF VER80}
  {$HINTS ON}
  {$ENDIF}

procedure dsSetState(DataSet: TDataSet; Value: TDataSetState);
begin
  TNastyDataSet(DataSet).FState := Value;
end;

procedure dsSetBOF(DataSet: TDataSet; Value: Boolean);
begin
  TNastyDataSet(DataSet).FBOF := Value;
end;

procedure dsSetEOF(DataSet: TDataSet; Value: Boolean);
begin
  TNastyDataSet(DataSet).FEOF := Value;
end;

{$IFDEF RX_D4}

procedure AssignBuffers(const Source: TBufferList; var Dest: TBufferList);
var
  Len: Integer;
begin
  Len := High(Source) + 1;
  SetLength(Dest, Len);
  Move(Pointer(Source)^, Pointer(Dest)^, Len * SizeOf(PChar));
end;

procedure dsGetBuffers(DataSet: TDataSet; var ABuf: TBufferList);
begin
  with TNastyDataSet(DataSet) do
    AssignBuffers(FBuffers, ABuf);
end;

procedure dsSetBuffers(DataSet: TDataSet; const Value: TBufferList);
begin
  AssignBuffers(Value, TNastyDataSet(DataSet).FBuffers);
end;

{$ELSE RX_D4}

procedure dsGetBuffers(DataSet: TDataSet; var ABuf: PBufferList);
begin
  ABuf := TNastyDataSet(DataSet).FBuffers;
end;

procedure dsSetBuffers(DataSet: TDataSet; const Value: PBufferList);
begin
  TNastyDataSet(DataSet).FBuffers := Value;
end;

{$ENDIF RX_D4}

function dsGetRecordCount(DataSet: TDataSet): Integer;
begin
  Result := TNastyDataSet(DataSet).FRecordCount;
end;

procedure dsSetRecordCount(DataSet: TDataSet; Value: Integer);
begin
  TNastyDataSet(DataSet).FRecordCount := Value;
end;

function dsGetActiveRecord(DataSet: TDataSet): Integer;
begin
  Result := TNastyDataSet(DataSet).FActiveRecord;
end;

procedure dsSetActiveRecord(DataSet: TDataSet; Value: Integer);
begin
  TNastyDataSet(DataSet).FActiveRecord := Value;
end;

function dsGetCanModify(DataSet: TBDEDataSet): Boolean;
begin
  Result := TBDENastyDataSet(DataSet).FCanModify;
end;

procedure dsSetCanModify(DataSet: TBDEDataSet; Value: Boolean);
begin
  TBDENastyDataSet(DataSet).FCanModify := Value;
end;

{ TFilterDataLink }

type
  TFilterDataLink = class(TDataLink)
  private
    FFilter: TRxDBFilter;
  protected
    procedure ActiveChanged; override;
  public
    constructor Create(Filter: TRxDBFilter);
    destructor Destroy; override;
  end;

constructor TFilterDataLink.Create(Filter: TRxDBFilter);
begin
  inherited Create;
  FFilter := Filter;
end;

destructor TFilterDataLink.Destroy;
begin
  FFilter := nil;
  inherited Destroy;
end;

procedure TFilterDataLink.ActiveChanged;
begin
  if FFilter <> nil then FFilter.ActiveChanged;
end;

{$IFDEF VER80}
type
  TFilterOption = TDBFilterOption;
  TFilterOptions = TDBFilterOptions;

function FilterCallback(pDBFilter: LongInt; RecBuf: Pointer;
  RecNo: LongInt): SmallInt; export;
begin
  Result := TRxDBFilter(pDBFilter).RecordFilter(RecBuf, RecNo);
end;
{$ENDIF}

{ TRxDBFilter }

constructor TRxDBFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFilterDataLink.Create(Self);
  FFilter := TStringList.Create;
  TStringList(FFilter).OnChange := FilterChanged;
  FLogicCond := flAnd;
  FIgnoreDataEvents := False;
end;

destructor TRxDBFilter.Destroy;
begin
  TStringList(FFilter).OnChange := nil;
  Deactivate;
  DropFilters;
  FFilter.Free;
  FDataLink.Free;
  inherited Destroy;
end;

procedure TRxDBFilter.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then Active := True;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

function TRxDBFilter.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRxDBFilter.SetDataSource(Value: TDataSource);
var
  DSChange: Boolean;
begin
  if not (csLoading in ComponentState) then ReleaseCapture;
  DSChange := True;
  if (Value <> nil) and (DataSource <> nil) then
    DSChange := (Value.DataSet <> FDataLink.DataSet);
  FIgnoreDataEvents := not DSChange;
  try
    if not (csLoading in ComponentState) then ActiveChanged;
    FDataLink.DataSource := Value;
    {$IFNDEF VER80}
    if Value <> nil then Value.FreeNotification(Self);
    {$ENDIF}
  finally
    FIgnoreDataEvents := False;
  end;
end;

procedure TRxDBFilter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) then
  begin
    if AComponent = DataSource then DataSource := nil;
  end;
end;

function TRxDBFilter.CreateExprFilter: hDBIFilter;
begin
  Result := nil;
  if (FFilter.Count > 0) then
    if BuildTree then
    try
      Check(DbiAddFilter((FDatalink.DataSet as TBDEDataSet).Handle,
        LongInt(Self), FPriority, False, pCANExpr(TExprParser(FParser).FilterData), nil,
        Result));
      FDataHandle := TBDEDataSet(FDatalink.DataSet).Handle;
    finally
      DestroyTree;
    end;
end;

function TRxDBFilter.CreateFuncFilter: hDBIFilter;
var
  FuncPriority: Word;
begin
  if (FPriority < $FFFF) and (FExprHandle <> nil) then
    FuncPriority := FPriority + 1
  else
    FuncPriority := FPriority;
  {$IFNDEF VER80}
  Check(DbiAddFilter((FDataLink.DataSet as TBDEDataSet).Handle, LongInt(Self),
    FuncPriority, False, nil, PFGENFilter(@TRxDBFilter.RecordFilter),
    Result));
  {$ELSE}
  Check(DbiAddFilter(FDataLink.DataSet.Handle, LongInt(Self), FuncPriority,
    False, nil, FilterCallback, Result));
  {$ENDIF}
  FDataHandle := TBDEDataSet(FDatalink.DataSet).Handle;
end;

procedure TRxDBFilter.SetFilterHandle(var Filter: HDBIFilter;
  Value: HDBIFilter);
var
  Info: FilterInfo;
begin
  if FActive and FDataLink.Active then
  begin
    FDataLink.DataSet.CursorPosChanged;
    DbiSetToBegin((FDataLink.DataSet as TBDEDataSet).Handle);
    if (Filter <> nil) and (Filter <> Value) then
      DbiDropFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
    Filter := Value;
    if Filter <> nil then
      DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
  end
  else if FActive and (Filter <> nil) and (FDataHandle <> nil) and
    (FDataLink.DataSet = nil) and (Value = nil) then
  begin
    if DbiGetFilterInfo(FDataHandle, Filter, 0, 0, Info) = DBIERR_NONE then
      DbiDeactivateFilter(FDataHandle, Filter);
    Filter := Value;
  end
  else
  begin
    {$IFDEF VER80}
    if (Filter <> nil) and FDatalink.Active then
      DbiDropFilter((FDataLink.DataSet as TBDEDataSet).Handle, Filter);
    {$ENDIF}
    Filter := Value;
  end;
end;

procedure TRxDBFilter.DropFilters;
begin
  SetFilterHandle(FExprHandle, nil);
  SetFilterHandle(FFuncHandle, nil);
  FDataHandle := nil;
  FActive := False;
end;

procedure TRxDBFilter.ActivateFilters;
begin
  if FExprHandle <> nil then
    DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, FExprHandle);
  if FFuncHandle <> nil then
    DbiActivateFilter((FDataLink.DataSet as TBDEDataSet).Handle, FFuncHandle);
end;

procedure TRxDBFilter.DeactivateFilters;
begin
  if (FFuncHandle <> nil) then
    DbiDeactivateFilter(TBDEDataSet(FDatalink.DataSet).Handle, FFuncHandle);
  if (FExprHandle <> nil) then
    DbiDeactivateFilter(TBDEDataSet(FDatalink.DataSet).Handle, FExprHandle);
end;

function TRxDBFilter.RecordFilter(RecBuf: Pointer; RecNo: LongInt): SmallInt;
var
  ACanModify: Boolean;
  Buffers: PBufferList;
  {$IFDEF RX_D4}
  BufPtr: TBufferList;
  {$ENDIF}
  ActiveRecord: Integer;
  RecCount: Integer;
  DS: TBDEDataSet;
begin
  Result := Ord(True);
  if Assigned(FOnFiltering) and (FFuncHandle <> nil) then
  try
    DS := FDatalink.DataSet as TBDEDataSet;
    { save current DataSet's private fields values }
    dsGetBuffers(DS, Buffers);
    ActiveRecord := dsGetActiveRecord(DS);
    RecCount := dsGetRecordCount(DS);
    ACanModify := dsGetCanModify(DS);
    try
      dsSetActiveRecord(DS, 0);
      dsSetRecordCount(DS, 1); { FActiveRecord + 1 }
      dsSetCanModify(DS, False);
      {$IFDEF RX_D4}
      SetLength(BufPtr, 1);
      BufPtr[0] := RxDBUtils.TBuffer(RecBuf);
      dsSetBuffers(DS, BufPtr);
      {$ELSE}
      dsSetBuffers(DS, @PChar(RecBuf));
      {$ENDIF}
      { call user defined function }
      Result := Ord(FOnFiltering(Self, DS));
    finally
      dsSetCanModify(DS, ACanModify);
      dsSetActiveRecord(DS, ActiveRecord);
      dsSetRecordCount(DS, RecCount);
      dsSetBuffers(DS, Buffers);
    end;
  except
    Application.HandleException(Self);
    Result := Abort; { BDE constant, not SysUtils.pas procedure }
  end;
end;

procedure TRxDBFilter.FilterChanged(Sender: TObject);
begin
  RecreateExprFilter;
end;

procedure TRxDBFilter.SetOnFiltering(const Value: TFilterEvent);
begin
  if Assigned(FOnFiltering) <> Assigned(Value) then
  begin
    FOnFiltering := Value;
    RecreateFuncFilter;
  end
  else
    FOnFiltering := Value;
end;

procedure TRxDBFilter.RecreateFuncFilter;
var
  Filter: HDBIFilter;
begin
  if FDataLink.Active and not (csReading in ComponentState) then
  begin
    if not FCaptured then FDataLink.DataSet.CheckBrowseMode;
    if Assigned(FOnFiltering) then
      Filter := CreateFuncFilter
    else
      Filter := nil;
    SetFilterHandle(FFuncHandle, Filter);
  end;
  if FDataLink.Active and Active and not FCaptured then
    FDataLink.DataSet.First;
end;

procedure TRxDBFilter.RecreateExprFilter;
var
  Filter: HDBIFilter;
begin
  if FDataLink.Active and not (csReading in ComponentState) then
  begin
    if not FCaptured then FDataLink.DataSet.CheckBrowseMode;
    if (FFilter.Count > 0) then
    try
      Filter := CreateExprFilter;
    except
      if Active or FActivating then
        raise
      else
        Filter := nil;
    end
    else
      Filter := nil;
    SetFilterHandle(FExprHandle, Filter);
  end;
  if FDataLink.Active and Active and not FCaptured then
    FDataLink.DataSet.First;
end;

procedure TRxDBFilter.SetFilter(Value: TStrings);
begin
  FFilter.Assign(Value);
end;

procedure TRxDBFilter.SetOptions(Value: TDBFilterOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    RecreateExprFilter;
  end;
end;

procedure TRxDBFilter.SetLogicCond(Value: TFilterLogicCond);
begin
  FLogicCond := Value;
end;

procedure TRxDBFilter.SetPriority(Value: Word);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    Update;
  end;
end;

function TRxDBFilter.GetFilterText: PChar;
var
  BufLen: Word;
  I: Integer;
  StrEnd: PChar;
  StrBuf: array[0..255] of Char;
begin
  BufLen := 1;
  for I := 0 to FFilter.Count - 1 do
    Inc(BufLen, Length(Filter.Strings[I]) + 1);
  Result := StrAlloc(BufLen);
  try
    StrEnd := Result;
    for I := 0 to Filter.Count - 1 do
    begin
      if Filter.Strings[I] <> '' then
      begin
        StrPCopy(StrBuf, Filter.Strings[I]);
        StrEnd := StrECopy(StrEnd, StrBuf);
        StrEnd := StrECopy(StrEnd, ' ');
      end;
    end;
  except
    StrDispose(Result);
    raise;
  end;
end;

procedure TRxDBFilter.DestroyTree;
begin
  if FParser <> nil then
  begin
    FParser.Free;
    FParser := nil;
  end;
end;

procedure TRxDBFilter.BeforeDataPost(DataSet: TDataSet);
begin
  ReadCaptureControls;
  ReleaseCapture;
  Activate;
  SysUtils.Abort;
end;

procedure TRxDBFilter.BeforeDataChange(DataSet: TDataSet);
begin
  FilterError(RxLoadStr(SCaptureFilter));
end;

procedure TRxDBFilter.BeforeDataCancel(DataSet: TDataSet);
begin
  ReleaseCapture;
end;

function TRxDBFilter.BuildTree: Boolean;
var
  Expr: PChar;
  I: Integer;
begin
  Result := True;
  if not FDataLink.Active then _DBError(SDataSetClosed);
  TStringList(FFilter).OnChange := nil;
  try
    for I := FFilter.Count - 1 downto 0 do
      if FFilter[I] = '' then FFilter.Delete(I);
  finally
    TStringList(FFilter).OnChange := FilterChanged;
  end;
  if FFilter.Count = 0 then
  begin
    Result := False;
    Exit;
  end;
  Expr := GetFilterText;
  try
    if StrLen(Expr) = 0 then
    begin
      Result := False;
      Exit;
    end;
    FParser := TExprParser.Create(FDataLink.DataSet, Expr,
      TFilterOptions(FOptions){$IFDEF RX_D4}, [], '', nil{$ENDIF}
      {$IFDEF RX_D5}, FldTypeMap{$ENDIF});
  finally
    StrDispose(Expr);
  end;
end;

procedure TRxDBFilter.DoActivate;
begin
  if Assigned(FOnActivate) then FOnActivate(Self);
end;

procedure TRxDBFilter.DoDeactivate;
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Self);
end;

procedure TRxDBFilter.SetActive(Value: Boolean);
var
  Bookmark: TBookmark;
begin
  if (csReading in ComponentState) then
    FStreamedActive := Value
  else if FDatalink.Active then
  begin
    FDatalink.DataSet.CheckBrowseMode;
    if FActive <> Value then
    begin
      if Value then
      begin
        FActivating := True;
        try
          if FCaptured then FilterError(RxLoadStr(SCaptureFilter));
          DbiSetToBegin((FDatalink.DataSet as TBDEDataSet).Handle);
          if FExprHandle = nil then RecreateExprFilter;
          if FFuncHandle = nil then RecreateFuncFilter;
          ActivateFilters;
          FDatalink.DataSet.First;
          FActive := Value;
          DoActivate;
        finally
          FActivating := False;
        end;
      end
      else
      begin
        if not IsDataSetEmpty(FDatalink.DataSet) then
          Bookmark := FDatalink.DataSet.GetBookmark
        else
          Bookmark := nil;
        try
          DbiSetToBegin((FDatalink.DataSet as TBDEDataSet).Handle);
          DeactivateFilters;
          if not SetToBookmark(FDatalink.DataSet, Bookmark) then
            FDatalink.DataSet.First;
        finally
          FDatalink.DataSet.FreeBookmark(Bookmark);
        end;
        FActive := Value;
        DoDeactivate;
      end;
      FActive := Value;
    end;
  end
  else
    FActive := Value;
end;

procedure TRxDBFilter.Activate;
begin
  SetActive(True);
end;

procedure TRxDBFilter.Deactivate;
begin
  SetActive(False);
end;

procedure TRxDBFilter.SetCapture;
begin
  if not FCaptured and (FDataLink <> nil) then
  begin
    if not FDataLink.Active then _DBError(SDataSetClosed);
    DataSource.DataSet.CheckBrowseMode;
    Deactivate;
    FIgnoreDataEvents := True;
    { store private fields values }
    with FStorage do
    begin
      FBof := DataSource.DataSet.Bof;
      FEof := DataSource.DataSet.Eof;
      State := DataSource.DataSet.State;
      CanModify := dsGetCanModify(FDatalink.DataSet as TBDEDataSet);
      BeforePost := DataSource.DataSet.BeforePost;
      BeforeCancel := DataSource.DataSet.BeforeCancel;
      BeforeInsert := DataSource.DataSet.BeforeInsert;
      BeforeEdit := DataSource.DataSet.BeforeEdit;
    end;
    DbiInitRecord((DataSource.DataSet as TBDEDataSet).Handle,
      Pointer(DataSource.DataSet.ActiveBuffer));
    dsSetBOF(DataSource.DataSet, True);
    dsSetEOF(DataSource.DataSet, True);
    dsSetState(DataSource.DataSet, dsEdit);
    dsSetCanModify(DataSource.DataSet as TBDEDataSet, True);
    DataSource.DataSet.BeforeCancel := BeforeDataCancel;
    DataSource.DataSet.BeforePost := BeforeDataPost;
    DataSource.DataSet.BeforeInsert := BeforeDataChange;
    DataSource.DataSet.BeforeEdit := BeforeDataChange;
    THackDataSet(DataSource.DataSet).DataEvent(deUpdateState, 0);
    THackDataSet(DataSource.DataSet).DataEvent(deDataSetChange, 0);
    {DataSource.DataSet := DataSource.DataSet;}
    FCaptured := True;
    if Assigned(FOnSetCapture) then FOnSetCapture(Self);
  end;
end;

procedure TRxDBFilter.ReleaseCapture;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) and FCaptured then
  begin
    { restore private fields values stored in SetCapture }
    with FStorage do
    begin
      dsSetBOF(DataSource.DataSet, FBof);
      dsSetEOF(DataSource.DataSet, FEof);
      dsSetState(DataSource.DataSet, State);
      dsSetCanModify(DataSource.DataSet as TBDEDataSet, CanModify);
      DataSource.DataSet.BeforePost := BeforePost;
      DataSource.DataSet.BeforeCancel := BeforeCancel;
      DataSource.DataSet.BeforeInsert := BeforeInsert;
      DataSource.DataSet.BeforeEdit := BeforeEdit;
    end;
    FCaptured := False;
    FIgnoreDataEvents := False;
    DataSource.DataSet.Resync([]);
    THackDataSet(DataSource.DataSet).DataEvent(deUpdateState, 0);
    THackDataSet(DataSource.DataSet).DataEvent(deDataSetChange, 0);
    {DataSource.DataSet := DataSource.DataSet;}
    if Assigned(FOnReleaseCapture) then FOnReleaseCapture(Self);
    ActiveChanged;
  end;
end;

procedure TRxDBFilter.ReadCaptureControls;
const
  LogicStr: array[TFilterLogicCond] of string = (' AND', ' OR');
var
  I: Integer;
  Field: TField;
  S: string;
begin
  if FCaptured then
  begin
    FFilter.BeginUpdate;
    try
      FFilter.Clear;
      with FDatalink.DataSet do
      begin
        UpdateRecord;
        for I := 0 to FieldCount - 1 do
        begin
          Field := Fields[I];
          if not (Field.IsNull or Field.Calculated{$IFNDEF VER80}
            or Field.Lookup{$ENDIF}) then
          begin
            S := '(' + cFldQuotaLeft + Field.FieldName + cFldQuotaRight +
              '=' + cQuota + Field.AsString + cQuota + ')';
            if FFilter.Count > 0 then S := S + LogicStr[FLogicCond];
            FFilter.Insert(0, S);
          end;
        end;
      end;
    finally
      FFilter.EndUpdate;
    end;
  end
  else
    FilterError(RxLoadStr(SNotCaptureFilter));
end;

procedure TRxDBFilter.UpdateFuncFilter;
begin
  if FDataLink.Active and Active and (FFuncHandle <> nil) then
    with FDatalink.DataSet as TBDEDataSet do
    begin
      DisableControls;
      try
        DbiDeactivateFilter(Handle, FFuncHandle);
        DbiActivateFilter(Handle, FFuncHandle);
        {CursorPosChanged; Resync([]);}
        First;
      finally
        EnableControls;
      end;
    end;
end;

procedure TRxDBFilter.Update;
begin
  if FDataLink.Active and Active then
  begin
    FDatalink.DataSet.DisableControls;
    try
      RecreateExprFilter;
      RecreateFuncFilter;
      {DeactivateFilters; ActivateFilters;}
    finally
      FDatalink.DataSet.EnableControls;
    end;
  end
  else
    DeactivateFilters;
end;

procedure TRxDBFilter.ActiveChanged;
var
  WasActive: Boolean;
begin
  if not FIgnoreDataEvents then
  begin
    WasActive := Active;
    DropFilters;
    if not (csDestroying in ComponentState) then
    begin
      RecreateExprFilter;
      RecreateFuncFilter;
      if WasActive then Activate;
    end;
  end;
end;

{  TRxDBGridSorter  }

function GoodField(F: TField): Boolean; //Checking if it is a GOOD field
begin
  GoodField := (F.FieldKind = fkData) and
    ((F is TStringField) or (F is TFloatField) or (F is TIntegerField)
    or (F is TBooleanField) or (F is TDateField) or (F is TDateTimeField));
end;

constructor TRxDBGridSorter.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then //we drop component onto form
  begin
    //Start initialization
    Markers := True;
  end;
end;

procedure TRxDBGridSorter.DoGridSorted(G: TRxDBGrid);
{$IFDEF USE_QUERY}
var
  Q: TQuery;
  A: Boolean;
begin
  //Initialization of variables
  FName := '';
  OldFName := '';
  G.TitleButtons := True;
  Q := TQuery(G.DataSource.DataSet);
  A := Q.Active; //Remember if Query was ACTIVE
  //Adding MACRO(TParam) by which we will sort
  Q.SQL.Add(' ' + ORDERstring);
  G.OnTitleBtnClick := TBC; //Title Button Click Event
  G.OnGetBtnParams := GBP; //Get Button params Event
  if A then Q.Open; //if Query was opened
  {$ELSE}
begin
  {$ENDIF}
end;

procedure TRxDBGridSorter.MakeGridSorted(G: TRxDBGrid);
begin
//Check possible access violation errors
  if Assigned(G) then
    if Assigned(G.DataSource) then
      if Assigned(G.DataSource.DataSet) then
        if (G.DataSource.DataSet is TQuery) then //DataSet !MUST! be TQuery
          DoGridSorted(G);
end;

procedure TRxDBGridSorter.SortIt;
var
  s, FN: string;
  Q: TQuery;
  V: Variant;
  i: Integer;
  Good: Boolean; //If there is any GOOD field then TRUE
begin
  Good := False;
  Q := TQuery(RxDBGrid.DataSource.DataSet);
  for i := 0 to Q.FieldCount - 1 do
    if GoodField(Q.Fields[i]) then //remember where is the active record
    begin
      Good := True;
      FN := Q.Fields[i].FieldName;
      V := Q.Fields[i].AsVariant;
      Break;
    end;
  Q.DisableControls; //if we don't do it grid will flash
  s := ' ORDER BY ' + FName; //We sorting be FName field
  if sm = smUp then
    s := s + ' DESC';
  Q.SQL.Delete(Q.SQL.Count - 1); //Replace ORDER BY Statement
  Q.SQL.Add(s);
  Q.Open;
  if Good then
    Q.Locate(FN, V, []); //Going to record that we remember
  Q.EnableControls;
end;

procedure TRxDBGridSorter.DBGridGetBtnParams(Sender: TObject; Field: TField;
  AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
  IsDown: Boolean);
begin
  if Markers then //if "Markers" property set to TRUE we draw sort markers
    if Assigned(Field) then
      if Field.FieldName = FName then
        SortMarker := sm;
end;

procedure TRxDBGridSorter.TitleButtonClick(Sender: TObject; ACol: Integer; Field: TField);
begin
  if GoodField(Field) then //Field must be GOOD
  begin
    OldFName := FName;
    FName := Field.FieldName; //Field by which we will sort
    if (sm = smDown) and (FName = OldFName) then
      sm := smUp
    else
      sm := smDown;
    SortIt; //Perform sorting in the Query
  end;
end;

procedure TRxDBGridSorter.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    MakeGridSorted(RxDBGrid);
end;

end.