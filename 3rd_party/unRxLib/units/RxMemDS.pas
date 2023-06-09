{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision for unicode and enhacenment added by JB.     }
{                                                       }
{ 17-06-2011 JB. + pchar buffer revision                }
{ 29-08-2011 JB. + blob revision for Delphi 2009/2010/XE}
{*******************************************************}

unit RxMemDS;

{$I RX.INC}
{$WARNINGS OFF}
interface

{$IFDEF RX_D3}

uses
  Windows, SysUtils, Classes, Controls,
  {$IFDEF RX_D6}Types,{$ENDIF}
  {$IFDEF RX_D17}System.Generics.Collections, {$ENDIF}
  DB, RxDBUtils;

{ TRxMemoryData }

type
  TMemBlobData = AnsiString;
  TMemBlobArray = array[0..0] of TMemBlobData;
  TBlobDataArrayType = ^TMemBlobArray;
  TBlobDataType = TMemBlobData;
  PMemBlobArray = ^TMemBlobArray;

  TMemoryRecord = class;
  TLoadMode = (lmCopy, lmAppend);
  TCompareRecords = function (Item1, Item2: TMemoryRecord): Integer of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxMemoryData = class(TDataSet)
  private
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBookmarkOfs: Integer;
    FBlobOfs: Integer;
    FRecBufSize: Integer;
    FOffsets: PWordArray;
    FLastID: Integer;
    FAutoInc: Longint;
    FActive: Boolean;
    FRecords: TList;
    FIndexList: TList;
    FCaseInsensitiveSort: Boolean;
    FDescendingSort: Boolean;
    function AddRecord: TMemoryRecord;
    function InsertRecord(Index: Integer): TMemoryRecord;
    function FindRecordID(ID: Integer): TMemoryRecord;
    procedure CreateIndexList(const FieldNames: string);
    procedure FreeIndexList;
    procedure QuickSort(L, R: Integer; Compare: TCompareRecords);
    procedure Sort;
    function CalcRecordSize: Integer;
    function FindFieldData(Buffer: Pointer; Field: TField): Pointer;
    function GetMemoryRecord(Index: Integer): TMemoryRecord;
    function GetCapacity: Integer;
    function RecordFilter: Boolean;
    procedure SetCapacity(Value: Integer);
    procedure ClearRecords;
    procedure InitBufferPointers(GetProps: Boolean);
    procedure RecordToBuffer(Rec: TMemoryRecord; Buffer: TBuffer);
  protected
    procedure AssignMemoryRecord(Rec: TMemoryRecord; Buffer: TBuffer);
    function GetActiveRecBuf(var RecBuf: TBuffer): Boolean; virtual;
    procedure InitFieldDefsFromFields;
    procedure SetMemoryRecordData(Buffer: TBuffer; Pos: Integer); virtual;
    procedure SetAutoIncFields(Buffer: TBuffer); virtual;
    function CompareRecords(Item1, Item2: TMemoryRecord): Integer; virtual;
    function GetBlobData(Field: TField; Buffer: TBuffer): TBlobDataType;
    procedure SetBlobData(Field: TField; Buffer: TBuffer; Value: TBlobDataType);
    function AllocRecordBuffer: TBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TBuffer); override;
    {$IFNDEF RX_D5}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; override;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
      Decimals: Integer): Boolean; override;
    {$ENDIF}
    procedure InternalInitRecord(Buffer: TBuffer); override;
    procedure ClearCalcFields(Buffer: TBuffer); override;
    function GetRecord(Buffer: TBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    {$IFDEF RX_D17}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    {$ENDIF}
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure CloseBlob(Field: TField); override;
    procedure GetBookmarkData(Buffer: TBuffer; Data: Pointer); override;
    {$IFDEF RX_D17}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; override;
    {$ENDIF}
    function GetBookmarkFlag(Buffer: TBuffer): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: TBookmarkPointerType); override;
    procedure InternalSetToRecord(Buffer: TBuffer); override;
    procedure SetBookmarkFlag(Buffer: TBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TBuffer; Data: Pointer); override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InitRecord(Buffer: TBuffer); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    {$IFDEF RX_D17}
    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); overload; override;
    {$ENDIF}
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    property Records[Index: Integer]: TMemoryRecord read GetMemoryRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    {$IFDEF RX_D17}
    function GetFieldData(Field: TField; {$IFDEF RX_D18}var {$ENDIF}Buffer: TValueBuffer): Boolean; overload; override;
    {$ENDIF}
    function GetCurrentRecord(Buffer: TBuffer): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    procedure SortOnFields(const FieldNames: string;
{$IFDEF RX_D4}
      CaseInsensitive: Boolean = True; Descending: Boolean = False);
{$ELSE}
      CaseInsensitive, Descending: Boolean);
{$ENDIF}
    procedure EmptyTable;
    procedure CopyStructure(Source: TDataSet);
    function LoadFromDataSet(Source: TDataSet; RecordCount: Integer;
      Mode: TLoadMode): Integer;
    function SaveToDataSet(Dest: TDataSet; RecordCount: Integer): Integer;
  published
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property Active;
    property AutoCalcFields;
    property Filtered;
{$IFDEF RX_D4}
    property FieldDefs;
    property ObjectView default False;
{$ENDIF}
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

{ TMemBlobStream }

  TMemBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TRxMemoryData;
    FBuffer: TBuffer;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FCached: Boolean;
    function GetBlobSize: Longint;
    function GetBlobFromRecord(Field: TField): TBlobDataType;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    {$IFDEF RX_D17}
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint;  overload; override;
    {$ENDIF}
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$IFDEF RX_D16}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ENDIF}
    procedure Truncate;
  end;

{ TMemoryRecord }

  TMemoryRecord = class(TPersistent)
  private
    FMemoryData: TRxMemoryData;
    FID: Integer;
    FData: Pointer;
    FBlobs: Pointer;
    function GetIndex: Integer;
    procedure SetMemoryData(Value: TRxMemoryData; UpdateParent: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(MemoryData: TRxMemoryData); virtual;
    constructor CreateEx(MemoryData: TRxMemoryData; UpdateParent: Boolean); virtual;
    destructor Destroy; override;
    property MemoryData: TRxMemoryData read FMemoryData;
    property ID: Integer read FID write FID;
    property Index: Integer read GetIndex write SetIndex;
    property Data: Pointer read FData;
  end;

{
 * @module HRxMemoryData.h - HVclDb BPL |
 * Contains Definitions, classes, and prototypes of the visual interface
 * component HRxMemoryData
 * Copyright (c)2000-2001 MeMyselfAndI, All Rights Reserved
 * Hofi (hofi@fw.hu)
 ** convert to Delphi by Jaro Benes (c)2005
}
  TSortOrder = (soAsc, soDesc);
//Possible sorting case sensitivity values
// sensitive sorting - insensitive sorting
  TSortCaseSens = (scYes, scNo);
//---------------------------------------------------------------------------
//
// HRxMemoryData is an improved RxLib TRxMemoryData design & runtime component.
// This is a memory table that doesn't need nor use the BDE nor the DBClient DLL
// It acts as a TDataSet and can be connected to all standard Delphi
// data-aware components.
//New features :
// record count also actualized in filtered state
// automatic refreshing on filter changes
// automatic sort on open
// sort fields, sort order and sort case sensitivity properties

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxMemoryDataEx = class(TRxMemoryData)
  private
    fSortFields: string;
    fSortOrder: TSortOrder;
    fSortCaseSens: TSortCaseSens;
    fAutoSortOnOpen: Boolean;
    fAutoRefreshOnFilterChanged: Boolean;
    fFilteredRecordCount: Integer;
    function GetFilteredRecordCount: Integer;
  protected
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure InternalRefresh; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure RefreshFilteredRecordCount;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ReSortOnFields(pSortOrder: TSortOrder; fields: string {= ''});
    function LoadFromDataSet(Source: TDataSet; RecordCount: Integer; Mode: TLoadMode): Integer; // ^
    procedure EmptyTable;
    procedure CopyStructure(Source: TDataSet);
    function IsSortField(field: TField): Boolean;
  published
    property SortOrder: TSortOrder read fSortOrder write fSortOrder;
    property SortCaseSens: TSortCaseSens read fSortCaseSens write fSortCaseSens;
    property SortFields: string read fSortFields write fSortFields nodefault;
    property AutoSortOnOpen: Boolean read fAutoSortOnOpen write fAutoSortOnOpen;
    property AutoRefreshOnFilterChanged: Boolean read fAutoRefreshOnFilterChanged write fAutoRefreshOnFilterChanged;
    property RecordCount: Integer read GetFilteredRecordCount;
  end;

{$ENDIF RX_D3}

implementation

{$IFDEF RX_D3}

uses
  Forms,
  RxResConst,
  DbConsts {$IFDEF RX_D5}, ComObj {$ENDIF}
  {$IFDEF RX_D6} , Variants {$ENDIF}; // Polaris

const
  ftBlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary {$IFDEF RX_D5}, ftOraBlob, ftOraClob {$ENDIF}];

  ftSupported = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftBCD, ftBytes,
    ftVarBytes {$IFDEF RX_D4}, ftADT, ftFixedChar, ftWideString,
    ftLargeint {$ENDIF} {$IFDEF RX_D5}, ftVariant, ftGuid {$ENDIF}] + 
    ftBlobTypes;

  fkStoredFields = [fkData];

{$IFDEF RX_D5}
  GuidSize = 38;
{$ENDIF}

{ Utility routines }

function CompareFields(Data1, Data2: Pointer; FieldType: TFieldType;
  CaseInsensitive: Boolean): Integer;
begin
  Result := 0;
  case FieldType of
    ftString:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftSmallint:
      if SmallInt(Data1^) > SmallInt(Data2^) then Result := 1
      else if SmallInt(Data1^) < SmallInt(Data2^) then Result := -1;
    ftInteger, ftDate, ftTime, ftAutoInc:
      if Longint(Data1^) > Longint(Data2^) then Result := 1
      else if Longint(Data1^) < Longint(Data2^) then Result := -1;
    ftWord:
      if Word(Data1^) > Word(Data2^) then Result := 1
      else if Word(Data1^) < Word(Data2^) then Result := -1;
    ftBoolean:
      if WordBool(Data1^) and not WordBool(Data2^) then Result := 1
      else if not WordBool(Data1^) and WordBool(Data2^) then Result := -1;
    ftFloat, ftCurrency:
      if Double(Data1^) > Double(Data2^) then Result := 1
      else if Double(Data1^) < Double(Data2^) then Result := -1;
    ftDateTime:
      if TDateTime(Data1^) > TDateTime(Data2^) then Result := 1
      else if TDateTime(Data1^) < TDateTime(Data2^) then Result := -1;
    {$IFDEF RX_D4}
    ftFixedChar:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftWideString:
      if CaseInsensitive then
        Result := AnsiCompareText(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)))
      else
        Result := AnsiCompareStr(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)));
    ftLargeint: 
      if Int64(Data1^) > Int64(Data2^) then Result := 1
      else if Int64(Data1^) < Int64(Data2^) then Result := -1;
    {$ENDIF}
    {$IFDEF RX_D5}
    ftVariant:
      Result := 0;
    ftGuid:
      Result := AnsiCompareText(PChar(Data1), PChar(Data2));
    {$ENDIF}
  end;
end;

function CalcFieldLen(FieldType: TFieldType; Size: Word): Word;
begin
  if not (FieldType in ftSupported) then
    Result := 0
  else
  if (FieldType in ftBlobTypes) then
    Result := SizeOf(Longint)
  else
  begin
    Result := Size;
    case FieldType of
      ftString: Inc(Result);
      ftSmallint: Result := SizeOf(SmallInt);
      ftInteger: Result := SizeOf(Longint);
      ftWord: Result := SizeOf(Word);
      ftBoolean: Result := SizeOf(WordBool);
      ftFloat: Result := SizeOf(Double);
      ftCurrency: Result := SizeOf(Double);
      ftBCD: Result := 34;
      ftDate, ftTime: Result := SizeOf(Longint);
      ftDateTime: Result := SizeOf(TDateTime);
      ftBytes: Result := Size;
      ftVarBytes: Result := Size + 2;
      ftAutoInc: Result := SizeOf(Longint);
      {$IFDEF RX_D4}
      ftADT: Result := 0;
      ftFixedChar: Inc(Result);
      ftWideString: Result := (Result + 1) * 2;
      ftLargeint: Result := SizeOf(Int64);
      {$ENDIF}
      {$IFDEF RX_D5}
      ftVariant: Result := SizeOf(Variant);
      ftGuid: Result := GuidSize + 1;
      {$ENDIF}
    end;
  end;
end;

procedure CalcDataSize(FieldDef: TFieldDef; var DataSize: Integer);
{$IFDEF RX_D4}
var
  I: Integer;
{$ENDIF}
begin
  with FieldDef do
  begin
    if (DataType in ftSupported - ftBlobTypes) then
      Inc(DataSize, CalcFieldLen(DataType, Size) + 1);
    {$IFDEF RX_D4}
    for I := 0 to ChildDefs.Count - 1 do
      CalcDataSize(ChildDefs[I], DataSize);
    {$ENDIF}
  end;
end;

procedure Error(const Msg: string);
begin
  DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg: string; const Args: array of const);
begin
  DatabaseErrorFmt(Msg, Args);
end;

type
  TBookmarkData = Integer;
  PMemBookmarkInfo = ^TMemBookmarkInfo;
  TMemBookmarkInfo = packed record
    BookmarkData: TBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end;

{ TMemoryRecord }

constructor TMemoryRecord.Create(MemoryData: TRxMemoryData);
begin
  CreateEx(MemoryData, True);
end;

constructor TMemoryRecord.CreateEx(MemoryData: TRxMemoryData;
  UpdateParent: Boolean);
begin
  inherited Create;
  SetMemoryData(MemoryData, UpdateParent);
end;

destructor TMemoryRecord.Destroy;
begin
  SetMemoryData(nil, True);
  inherited Destroy;
end;

function TMemoryRecord.GetIndex: Integer;
begin
  if FMemoryData <> nil then Result := FMemoryData.FRecords.IndexOf(Self)
  else Result := -1;
end;

procedure TMemoryRecord.SetMemoryData(Value: TRxMemoryData; UpdateParent: Boolean);
var
  I: Integer;
  DataSize: Integer;
begin
  if FMemoryData <> Value then
  begin
    if FMemoryData <> nil then
    begin
      FMemoryData.FRecords.Remove(Self);
      if FMemoryData.BlobFieldCount > 0 then
        Finalize(PMemBlobArray(FBlobs)[0], FMemoryData.BlobFieldCount);
      ReallocMem(FBlobs, 0);
      ReallocMem(FData, 0);
      FMemoryData := nil;
    end;
    if Value <> nil then
    begin
      if UpdateParent then
      begin
        Value.FRecords.Add(Self);
        Inc(Value.FLastID);
        FID := Value.FLastID;
      end;
      FMemoryData := Value;
      if Value.BlobFieldCount > 0 then
      begin
        ReallocMem(FBlobs, Value.BlobFieldCount * SizeOf(Pointer));
        Initialize(PMemBlobArray(FBlobs)[0], Value.BlobFieldCount);
      end;
      DataSize := 0;
      for I := 0 to Value.FieldDefs.Count - 1 do
        CalcDataSize(Value.FieldDefs[I], DataSize);
      ReallocMem(FData, DataSize);
    end;
  end;
end;

procedure TMemoryRecord.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FMemoryData.FRecords.Move(CurIndex, Value);
end;

{ TRxMemoryData }

constructor TRxMemoryData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordPos := -1;
  FLastID := Low(Integer);
  FAutoInc := 1;
  FRecords := TList.Create;
end;

destructor TRxMemoryData.Destroy;
begin
  FreeIndexList;
  ClearRecords;
  FRecords.Free;
  ReallocMem(FOffsets, 0);
  inherited Destroy;
end;

{ Records Management }

function TRxMemoryData.GetCapacity: Integer;
begin
  if FRecords <> nil then Result := FRecords.Capacity
  else Result := 0;
end;

procedure TRxMemoryData.SetCapacity(Value: Integer);
begin
  if FRecords <> nil then FRecords.Capacity := Value;
end;

function TRxMemoryData.AddRecord: TMemoryRecord;
begin
  Result := TMemoryRecord.Create(Self);
end;

function TRxMemoryData.FindRecordID(ID: Integer): TMemoryRecord;
var
  I: Integer;
begin
  for I := 0 to FRecords.Count - 1 do
  begin
    Result := TMemoryRecord(FRecords[I]);
    if Result.ID = ID then Exit;
  end;
  Result := nil;
end;

function TRxMemoryData.InsertRecord(Index: Integer): TMemoryRecord;
begin
  Result := AddRecord;
  Result.Index := Index;
end;

function TRxMemoryData.GetMemoryRecord(Index: Integer): TMemoryRecord;
begin
  Result := TMemoryRecord(FRecords[Index]);
end;

{ Field Management }

{$IFNDEF RX_D5}
function TRxMemoryData.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TRxMemoryData.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;
{$ENDIF RX_D5}

procedure TRxMemoryData.InitFieldDefsFromFields;
var
  I: Integer;
  Offset: Word;
begin
  if FieldDefs.Count = 0 then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind in fkStoredFields) and not (DataType in ftSupported) then
          ErrorFmt(SUnknownFieldType, [DisplayName]);
    end;
    FreeIndexList;
  end;
  Offset := 0;
{$IFDEF RX_D4}
  inherited InitFieldDefsFromFields;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefList.Count * SizeOf(Word));
  for I := 0 to FieldDefList.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefList[I] do
    begin
      if (DataType in ftSupported - ftBlobTypes) then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
{$ELSE}
  { Create FieldDefs from persistent fields if needed }
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind = fkData) then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefs.Count * SizeOf(Word));
  for I := 0 to FieldDefs.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefs[I] do
    begin
      if (DataType in ftSupported - ftBlobTypes) then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
{$ENDIF}
end;

function TRxMemoryData.FindFieldData(Buffer: Pointer; Field: TField): Pointer;
var
  Index: Integer;
  DataType: TFieldType;
begin
  Result := nil;
  Index := FieldDefList.IndexOf(Field.FullName);
  if (Index >= 0) and (Buffer <> nil) then
  begin
    DataType := FieldDefList[Index].DataType;
    if DataType in ftSupported then
      if DataType in ftBlobTypes then
        Result := Pointer(GetBlobData(Field, Buffer))
      else
        Result := (PAnsiChar(Buffer) + FOffsets[Index]);
  end;
end;

{ Buffer Manipulation }

function TRxMemoryData.CalcRecordSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FieldDefs.Count - 1 do
    CalcDataSize(FieldDefs[I], Result);
end;

procedure TRxMemoryData.InitBufferPointers(GetProps: Boolean);
begin
  if GetProps then FRecordSize := CalcRecordSize;
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBlobOfs := FBookmarkOfs + SizeOf(TMemBookmarkInfo);
  FRecBufSize := FBlobOfs + BlobFieldCount * SizeOf(Pointer);
end;

procedure TRxMemoryData.ClearRecords;
begin
  while FRecords.Count > 0 do TObject(FRecords.Last).Free;
  FLastID := Low(Integer);
  FRecordPos := -1;
end;

function TRxMemoryData.AllocRecordBuffer: TBuffer;
begin
{$IFDEF RX_D12}
  Result := AllocMem(FRecBufSize);
{$ELSE}
  Result := StrAlloc(FRecBufSize);
{$ENDIF}
  if BlobFieldCount > 0 then
    Initialize(PMemBlobArray(Result + FBlobOfs)[0], BlobFieldCount);
end;

procedure TRxMemoryData.FreeRecordBuffer(var Buffer: TBuffer);
begin
  if BlobFieldCount > 0 then
    Finalize(PMemBlobArray(Buffer + FBlobOfs)[0], BlobFieldCount);
{$IFDEF RX_D12}
  FreeMem(Buffer);
{$ELSE}
  StrDispose(Buffer);
{$ENDIF}
  Buffer := nil;
end;

procedure TRxMemoryData.ClearCalcFields(Buffer: TBuffer);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TRxMemoryData.InternalInitRecord(Buffer: TBuffer);
var
  I: Integer;
begin
  FillChar(Buffer^, FBlobOfs, 0);
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)[I] := '';
end;

procedure TRxMemoryData.InitRecord(Buffer: TBuffer);
begin
  inherited InitRecord(Buffer);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Low(Integer);
    BookmarkFlag := bfInserted;
  end;
end;

function TRxMemoryData.GetCurrentRecord(Buffer: TBuffer): Boolean;
begin
  Result := False;
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
      Result := True;
    end;
  end;
end;

procedure TRxMemoryData.RecordToBuffer(Rec: TMemoryRecord; Buffer: TBuffer);
var
  I: Integer;
begin
  Move(Rec.Data^, Buffer^, FRecordSize);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Rec.ID;
    BookmarkFlag := bfCurrent;
  end;
  for I := 0 to BlobFieldCount - 1 do
    TBlobDataArrayType(Buffer + FBlobOfs)[I] := TBlobDataArrayType(Rec.FBlobs)[I];
  GetCalcFields(Buffer);
end;

function TRxMemoryData.GetRecord(Buffer: TBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
begin
  Result := grOk;
  Accept := True;
  case GetMode of
    gmPrior:
      if FRecordPos <= 0 then
      begin
        Result := grBOF;
        FRecordPos := -1;
      end
      else
      begin
        repeat
          Dec(FRecordPos);
          if Filtered then Accept := RecordFilter;
        until Accept or (FRecordPos < 0);
        if not Accept then
        begin
          Result := grBOF;
          FRecordPos := -1;
        end;
      end;
    gmCurrent:
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        Result := grError
      else
      if Filtered then
      begin
        if not RecordFilter then Result := grError;
      end;
    gmNext:
      if FRecordPos >= RecordCount - 1 then Result := grEOF
      else
      begin
        repeat
          Inc(FRecordPos);
          if Filtered then Accept := RecordFilter;
        until Accept or (FRecordPos > RecordCount - 1);
        if not Accept then
        begin
          Result := grEOF;
          FRecordPos := RecordCount - 1;
        end;
      end;
  end;
  if Result = grOk then RecordToBuffer(Records[FRecordPos], Buffer)
  else if (Result = grError) and DoCheck then Error(RxLoadStr(SMemNoRecords));
end;

function TRxMemoryData.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TRxMemoryData.GetActiveRecBuf(var RecBuf: TBuffer): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then RecBuf := nil
      else RecBuf := TBuffer(ActiveBuffer);
    dsEdit, dsInsert: RecBuf := TBuffer(ActiveBuffer);
    dsCalcFields: RecBuf := TBuffer(CalcBuffer);
    dsFilter: RecBuf := TBuffer(TempBuffer);
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TRxMemoryData.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf, Data: TBuffer;
{$IFDEF RX_D5}
  VarData: Variant;
{$ENDIF}
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then Exit;
  if Field.FieldNo > 0 then
  begin
    Data := FindFieldData(RecBuf, Field);
    if Data <> nil then
    begin
      Result := Boolean(Data[0]);
      Inc(Data);
      if Field.DataType in [ftString
        {$IFDEF RX_D4}, ftFixedChar, ftWideString {$ENDIF}
        {$IFDEF RX_D5}, ftGuid {$ENDIF}]
      then
        Result := Result and {$IFDEF RX_D12}(Data <> nil){$ELSE}(StrLen(Data) > 0){$ENDIF};
      if Result and (Buffer <> nil) then
        {$IFDEF RX_D5}
        if Field.DataType = ftVariant then
        begin
          VarData := PVariant(Data)^;
          PVariant(Buffer)^ := VarData;
        end
        else
        {$ENDIF}
          Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
    end;
  end
  else
  begin
    if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
        Move(RecBuf[1], Buffer^, Field.DataSize);
    end;
  end;
end;

{$IFDEF RX_D17}
function TRxMemoryData.GetFieldData(Field: TField; {$IFDEF RX_D18}var {$ENDIF}Buffer: TValueBuffer): Boolean;
begin
  Result := Self.GetFieldData(Field, Pointer(Buffer))
end;
{$ENDIF}

procedure TRxMemoryData.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, Data: TBuffer;
{$IFDEF RX_D5}
  VarData: Variant;
{$ENDIF}
begin
  if not (State in dsWriteModes) then Error(SNotEditing);
  GetActiveRecBuf(RecBuf);
  with Field do
  begin
    if FieldNo > 0 then
    begin
      if State in [dsCalcFields, dsFilter] then Error(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        ErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then
        begin
        {$IFDEF RX_D5}
          if DataType = ftVariant then
          begin
            if Buffer <> nil then
              VarData := PVariant(Buffer)^
            else
              VarData := EmptyParam;
            Boolean(Data[0]) := LongBool(Buffer) and not (VarIsNull(VarData) or VarIsEmpty(VarData));
            if Boolean(Data[0]) then
            begin
              Inc(Data);
              PVariant(Data)^ := VarData;
            end
            else FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end
          else
        {$ENDIF}
          begin
            Boolean(Data[0]) := LongBool(Buffer);
            Inc(Data);
            if LongBool(Buffer) then
              Move(Buffer^, Data^, CalcFieldLen(DataType, Size))
            else FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end;
        end;
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[0]) then Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, {$IFDEF WIN64}NativeInt{$ELSE}LongInt{$ENDIF}(Field));
  end;
end;

{$IFDEF RX_D17}
procedure TRxMemoryData.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  Self.SetFieldData(Field, Pointer(Buffer));
end;
{$ENDIF}

{ Filter }

procedure TRxMemoryData.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      First;
    end;
  end
  else inherited SetFiltered(Value);
end;

procedure TRxMemoryData.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then
  begin
    CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Filtered then First;
  end
  else inherited SetOnFilterRecord(Value);
end;

function TRxMemoryData.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(Records[FRecordPos], TBuffer(TempBuffer));
        if Assigned(OnFilterRecord) then
          OnFilterRecord(Self, Result);
      except
        Application.HandleException(Self);
      end;
      RestoreState(SaveState);
    end
    else Result := False;
  end;
end;

{ Blobs }

function TRxMemoryData.GetBlobData(Field: TField; Buffer: TBuffer): TBlobDataType;
begin
  Result := TBlobDataArrayType(Buffer + FBlobOfs)[Field.Offset];
end;

procedure TRxMemoryData.SetBlobData(Field: TField; Buffer: TBuffer;
  Value: TBlobDataType);
begin
  if Buffer = TBuffer(ActiveBuffer) then
  begin
    if State = dsFilter then Error(SNotEditing);
    TBlobDataArrayType(Buffer + FBlobOfs)[Field.Offset] := Value;
  end;
end;

procedure TRxMemoryData.CloseBlob(Field: TField);
begin
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and (State = dsEdit) then
    TBlobDataArrayType(ActiveBuffer + FBlobOfs)[Field.Offset] :=
      TBlobDataArrayType(Records[FRecordPos].FBlobs)[Field.Offset]
  else
    TBlobDataArrayType(ActiveBuffer + FBlobOfs)[Field.Offset] := '';
end;

function TRxMemoryData.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TMemBlobStream.Create(Field as TBlobField, Mode);
end;

{ Bookmarks }

function TRxMemoryData.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
{$IFDEF RX_D12}
  Result := FActive and (PInteger(Bookmark)^ > Low(Integer)) and
    (PInteger(Bookmark)^ <= FLastID);
{$ELSE}
  Result := FActive and (TBookmarkData(Bookmark^) > Low(Integer)) and
    (TBookmarkData(Bookmark^) <= FLastID);
{$ENDIF}
end;

function TRxMemoryData.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then Result := 0
  else if (Bookmark1 <> nil) and (Bookmark2 = nil) then Result := 1
  else if (Bookmark1 = nil) and (Bookmark2 <> nil) then Result := -1
  else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then
    Result := 1
  else if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then
    Result := -1
  else Result := 0;
end;

procedure TRxMemoryData.GetBookmarkData(Buffer: TBuffer; Data: Pointer);
begin
  Move(PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^, SizeOf(TBookmarkData));
end;

{$IFDEF RX_D17}
procedure TRxMemoryData.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  Self.GetBookmarkData(Buffer, PByte(@Data[0]));
end;
{$ENDIF}

procedure TRxMemoryData.SetBookmarkData(Buffer: TBuffer; Data: Pointer);
begin
  Move(Data^, PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, SizeOf(TBookmarkData));
end;

{$IFDEF RX_D17}
procedure TRxMemoryData.SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  Self.SetBookmarkData(Buffer,PByte(@Data[0]));
end;
{$ENDIF}

function TRxMemoryData.GetBookmarkFlag(Buffer: TBuffer): TBookmarkFlag;
begin
  Result := PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

procedure TRxMemoryData.SetBookmarkFlag(Buffer: TBuffer; Value: TBookmarkFlag);
begin
  PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

procedure TRxMemoryData.InternalGotoBookmark(Bookmark: TBookmarkPointerType);
var
  Rec: TMemoryRecord;
  SavePos: Integer;
  Accept: Boolean;
begin
  Rec := FindRecordID(TBookmarkData(Bookmark^));
  if Rec <> nil then
  begin
    Accept := True;
    SavePos := FRecordPos;
    try
      FRecordPos := Rec.Index;
      if Filtered then Accept := RecordFilter;
    finally
      if not Accept then FRecordPos := SavePos;
    end;
  end;
end;

{ Navigation }

procedure TRxMemoryData.InternalSetToRecord(Buffer: TBuffer);
begin
  InternalGotoBookmark(@PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

procedure TRxMemoryData.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TRxMemoryData.InternalLast;
begin
  FRecordPos := FRecords.Count;
end;

{ Data Manipulation }

procedure TRxMemoryData.AssignMemoryRecord(Rec: TMemoryRecord; Buffer: TBuffer);
var
  I: Integer;
begin
  Move(Buffer^, Rec.Data^, FRecordSize);
  for I := 0 to BlobFieldCount - 1 do
    TBlobDataArrayType(Rec.FBlobs)[I] := TBlobDataArrayType(Buffer + FBlobOfs)[I];
end;

procedure TRxMemoryData.SetMemoryRecordData(Buffer: TBuffer; Pos: Integer);
var
  Rec: TMemoryRecord;
begin
  if State = dsFilter then Error(SNotEditing);
  Rec := Records[Pos];
  AssignMemoryRecord(Rec, Buffer);
end;

procedure TRxMemoryData.SetAutoIncFields(Buffer: TBuffer);
var
  I, Count: Integer;
  Data: TBuffer;
begin
  Count := 0;
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind in fkStoredFields) and (Fields[I].DataType = ftAutoInc) then
    begin
      Data := FindFieldData(Buffer, Fields[I]);
      if Data <> nil then
      begin
        Boolean(Data[0]) := True;
        Inc(Data);
        Move(FAutoInc, Data^, SizeOf(Longint));
        Inc(Count);
      end;
    end;
  if Count > 0 then Inc(FAutoInc);
end;

procedure TRxMemoryData.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
  Rec: TMemoryRecord;
begin
  if Append then
  begin
    Rec := AddRecord;
    FRecordPos := FRecords.Count - 1;
  end
  else
  begin
    if FRecordPos = -1 then RecPos := 0
    else RecPos := FRecordPos;
    Rec := InsertRecord(RecPos);
    FRecordPos := RecPos;
  end;
  SetAutoIncFields(Buffer);
  SetMemoryRecordData(Buffer, Rec.Index);
end;

{$IFDEF RX_D17}
procedure TRxMemoryData.InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean);
begin
  Self.InternalAddRecord(Pointer(Buffer), Append);
end;
{$ENDIF}

procedure TRxMemoryData.InternalDelete;
var
  Accept: Boolean;
begin
  Records[FRecordPos].Free;
  if FRecordPos >= FRecords.Count then Dec(FRecordPos);
  Accept := True;
  repeat
    if Filtered then Accept := RecordFilter;
    if not Accept then Dec(FRecordPos);
  until Accept or (FRecordPos < 0);
  if FRecords.Count = 0 then FLastID := Low(Integer);
end;

procedure TRxMemoryData.InternalPost;
var
  RecPos: Integer;
begin
  if State = dsEdit then
    SetMemoryRecordData(TBuffer(ActiveBuffer), FRecordPos)
  else
  begin
    if State in [dsInsert] then SetAutoIncFields(TBuffer(ActiveBuffer));
    if FRecordPos >= FRecords.Count then
    begin
      SetMemoryRecordData(TBuffer(ActiveBuffer), AddRecord.Index);
      FRecordPos := FRecords.Count - 1;
    end
    else
    begin
      if FRecordPos = -1 then RecPos := 0
      else RecPos := FRecordPos;
      SetMemoryRecordData(TBuffer(ActiveBuffer), InsertRecord(RecPos).Index);
      FRecordPos := RecPos;
    end;
  end;
end;

procedure TRxMemoryData.OpenCursor(InfoQuery: Boolean);
begin
  if not InfoQuery then
  begin
    if FieldCount > 0 then FieldDefs.Clear;
    InitFieldDefsFromFields;
  end;
  FActive := True;
  inherited OpenCursor(InfoQuery);
end;

procedure TRxMemoryData.InternalOpen;
begin
  BookmarkSize := SizeOf(TBookmarkData);
{$IFDEF RX_D4}
  if DefaultFields then CreateFields;
{$ELSE}
  if DefaultFields then Error(SInvalidFields);
{$ENDIF}
  BindFields(True);
  InitBufferPointers(True);
  InternalFirst;
end;

procedure TRxMemoryData.InternalClose;
begin
  ClearRecords;
  FAutoInc := 1;
  BindFields(False);
{$IFDEF RX_D4}
  if DefaultFields then DestroyFields;
{$ENDIF}
  FreeIndexList;
  FActive := False;
end;

procedure TRxMemoryData.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TRxMemoryData.InternalInitFieldDefs;
begin
end;

function TRxMemoryData.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

{ Informational }

function TRxMemoryData.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TRxMemoryData.GetRecNo: Integer;
begin
  CheckActive;
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then Result := 1
  else Result := FRecordPos + 1;
end;

procedure TRxMemoryData.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    DoBeforeScroll;
    FRecordPos := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

function TRxMemoryData.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

function TRxMemoryData.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

{ Table Manipulation }

function TRxMemoryData.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  FieldCount: Integer;
  Fields: TList{$IFDEF RX_D17}<TField>{$ENDIF};
  Fld: TField;
  SaveState: TDataSetState;
  I: Integer;
  Matched: Boolean;

  function CompareField(var Field: TField; Value: Variant): Boolean; {BG}
  var
    S: string;
  begin
    if Field.DataType = ftString then
    begin
      if Value = Null then
        Result := Field.IsNull
      else
      begin
        S := Field.AsString;
        Result := AnsiSameStr(S, Value);
      end;
    end
    else
      Result := (Field.Value = Value);
  end;

  function CompareRecord: Boolean;
  var
    I: Integer;
  begin
    if FieldCount = 1 then
    begin
      Fld := TField(Fields.First);
      Result := CompareField(Fld, KeyValues);
    end
    else
    begin
      Result := True;
      for I := 0 to FieldCount - 1 do
      begin
        Fld := TField(Fields[I]);
        Result := Result and CompareField(Fld, KeyValues[I]);
      end;
    end;
  end;

begin
  Result := Null;
  CheckBrowseMode;
  if IsEmpty then
    Exit;

  Fields := TList{$IFDEF RX_D17}<TField>{$ENDIF}.Create;
  try
    GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;
    Matched := CompareRecord;
    if Matched then
      Result := FieldValues[ResultFields]
    else
    begin
      SaveState := SetTempState(dsCalcFields);
      try
        try
          for I := 0 to RecordCount - 1 do
          begin
            RecordToBuffer(Records[I], TBuffer(TempBuffer));
            CalculateFields(TempBuffer);
            Matched := CompareRecord;
            if Matched then
              Break;
          end;
        finally
          if Matched then
            Result := FieldValues[ResultFields];
        end;
      finally
        RestoreState(SaveState);
      end;
    end;
  finally
    Fields.Free;
  end;
end;

procedure TRxMemoryData.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    ClearRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TRxMemoryData.CopyStructure(Source: TDataSet);

  procedure CheckDataTypes(FieldDefs: TFieldDefs);
  var
    I: Integer;
  begin
    for I := FieldDefs.Count - 1 downto 0 do
    begin
      if not (FieldDefs.Items[I].DataType in ftSupported) then
        FieldDefs.Items[I].Free
      {$IFDEF RX_D4}
      else
        CheckDataTypes(FieldDefs[I].ChildDefs);
      {$ENDIF}
    end;
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do Fields[I].Free;
  if (Source = nil) then Exit;
  Source.FieldDefs.Update;
  FieldDefs := Source.FieldDefs;
  CheckDataTypes(FieldDefs);
{$IFDEF RX_D4}
  CreateFields;
{$ELSE}
  for I := 0 to FieldDefs.Count - 1 do
  begin
    if (csDesigning in ComponentState) and (Owner <> nil) then
      FieldDefs.Items[I].CreateField(Owner)
    else
      FieldDefs.Items[I].CreateField(Self);
  end;
{$ENDIF}
end;

function TRxMemoryData.LoadFromDataSet(Source: TDataSet; RecordCount: Integer;
  Mode: TLoadMode): Integer;
var
  SourceActive: Boolean;
  MovedCount: Integer;
begin
  Result := 0;
  if Source = Self then Exit;
  SourceActive := Source.Active;
  Source.DisableControls;
  try
    DisableControls;
    try
      Filtered := False;
      with Source do
      begin
        Open;
        CheckBrowseMode;
        UpdateCursorPos;
      end;
      if Mode = lmCopy then
      begin
        Close;
        CopyStructure(Source);
      end;
      FreeIndexList;
      if not Active then Open;
      CheckBrowseMode;
      if RecordCount > 0 then MovedCount := RecordCount
      else
      begin
        Source.First;
        MovedCount := MaxInt;
      end;
      try
        while not Source.EOF do
        begin
          Append;
          AssignRecord(Source, Self, True);
          Post;
          Inc(Result);
          if Result >= MovedCount then Break;
          Source.Next;
        end;
      finally
        First;
      end;
    finally
      EnableControls;
    end;
  finally
    if not SourceActive then Source.Close;
    Source.EnableControls;
  end;
end;

function TRxMemoryData.SaveToDataSet(Dest: TDataSet; RecordCount: Integer): Integer;
var
  MovedCount: Integer;
begin
  Result := 0;
  if Dest = Self then Exit;
  CheckBrowseMode;
  UpdateCursorPos;
  Dest.DisableControls;
  try
    DisableControls;
    try
      if not Dest.Active then Dest.Open
      else Dest.CheckBrowseMode;
      if RecordCount > 0 then MovedCount := RecordCount
      else
      begin
        First;
        MovedCount := MaxInt;
      end;
      try
        while not EOF do
        begin
          Dest.Append;
          AssignRecord(Self, Dest, True);
          Dest.Post;
          Inc(Result);
          if Result >= MovedCount then Break;
          Next;
        end;
      finally
        Dest.First;
      end;
    finally
      EnableControls;
    end;
  finally
    Dest.EnableControls;
  end;
end;

{ Index Related }

procedure TRxMemoryData.SortOnFields(const FieldNames: string;
{$IFDEF RX_D4}
  CaseInsensitive: Boolean = True; Descending: Boolean = False);
{$ELSE}
  CaseInsensitive, Descending: Boolean);
{$ENDIF}
begin
  CreateIndexList(FieldNames);
  FCaseInsensitiveSort := CaseInsensitive;
  FDescendingSort := Descending;
  try
    Sort;
  except
    FreeIndexList;
    raise;
  end;
end;

procedure TRxMemoryData.Sort;
var
  Pos: TBookmarkType;
begin
  if Active and (FRecords <> nil) and (FRecords.Count > 0) then
  begin
    Pos := Bookmark;
    try
      QuickSort(0, FRecords.Count - 1, CompareRecords);
      SetBufListSize(0);
      InitBufferPointers(False);
      try
        SetBufListSize(BufferCount + 1);
      except
        SetState(dsInactive);
        CloseCursor;
        raise;
      end;
    finally
      Bookmark := Pos;
    end;
    Resync([]);
  end;
end;

procedure TRxMemoryData.QuickSort(L, R: Integer; Compare: TCompareRecords);
var
  I, J: Integer;
  P: TMemoryRecord;
begin
  repeat
    I := L;
    J := R;
    P := Records[(L + R) shr 1];
    repeat
      while Compare(Records[I], P) < 0 do Inc(I);
      while Compare(Records[J], P) > 0 do Dec(J);
      if I <= J then
      begin
        FRecords.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TRxMemoryData.CompareRecords(Item1, Item2: TMemoryRecord): Integer;
var
  Data1, Data2: PAnsiChar;
  F: TField;
  I: Integer;
begin
  Result := 0;
  if FIndexList <> nil then
  begin
    for I := 0 to FIndexList.Count - 1 do
    begin
      F := TField(FIndexList[I]);
      Data1 := FindFieldData(Item1.Data, F);
      if Data1 <> nil then
      begin
        Data2 := FindFieldData(Item2.Data, F);
        if Data2 <> nil then
        begin
          if Boolean(Data1[0]) and Boolean(Data2[0]) then
          begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, F.DataType,
              FCaseInsensitiveSort);
          end
          else if Boolean(Data1[0]) then
            Result := 1
          else if Boolean(Data2[0]) then
            Result := -1;
          if FDescendingSort then
            Result := -Result;
        end;
      end;
      if Result <> 0 then
        Exit;
    end;
  end;
  if (Result = 0) then
  begin
    if Item1.ID > Item2.ID then
      Result := 1
    else if Item1.ID < Item2.ID then
      Result := -1;
    if FDescendingSort then
      Result := -Result;
  end;
end;

function TRxMemoryData.GetIsIndexField(Field: TField): Boolean;
begin
  if FIndexList <> nil then
    Result := FIndexList.IndexOf(Field) >= 0
  else Result := False;
end;

procedure TRxMemoryData.CreateIndexList(const FieldNames: string);
var
  Pos: Integer;
  F: TField;
begin
  if FIndexList = nil then FIndexList := TList.Create
  else FIndexList.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    F := FieldByName(ExtractFieldName(FieldNames, Pos));
    if (F.FieldKind = fkData) and (F.DataType in ftSupported - ftBlobTypes) then
      FIndexList.Add(F)
    else ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
  end;
end;

procedure TRxMemoryData.FreeIndexList;
begin
  FIndexList.Free;
  FIndexList := nil;
end;

{ TMemBlobStream }

constructor TMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TRxMemoryData;
  if not FDataSet.GetActiveRecBuf(FBuffer) then Exit;
  if not FField.Modified and (Mode <> bmRead) then
  begin
    if FField.ReadOnly then ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then Error(SNotEditing);
    FCached := True;
  end
  else FCached := (FBuffer = TBuffer(FDataSet.ActiveBuffer));
  FOpened := True;
  if Mode = bmWrite then Truncate;
end;

destructor TMemBlobStream.Destroy;
begin
  if FOpened and FModified then FField.Modified := True;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, {$IFDEF WIN64}NativeInt{$ELSE}LongInt{$ENDIF}(FField));
  except
    Application.HandleException(Self);
  end;
end;

function TMemBlobStream.GetBlobFromRecord(Field: TField): TBlobDataType;
var
  Rec: TMemoryRecord;
  Pos: Integer;
begin
  Result := '';
  Pos := FDataSet.FRecordPos;
  if (Pos < 0) and (FDataSet.RecordCount > 0) then Pos := 0
  else if Pos >= FDataSet.RecordCount then Pos := FDataSet.RecordCount - 1;
  if (Pos >= 0) and (Pos < FDataSet.RecordCount) then
  begin
    Rec := FDataSet.Records[Pos];
    if Rec <> nil then
      Result := TBlobDataArrayType(Rec.FBlobs)[FField.Offset];
  end;
end;

function TMemBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then Result := Size - FPosition
    else Result := Count;
    if Result > 0 then
    begin
      if FCached then
      begin
        Move(PAnsiChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end
      else
      begin
        Move(PAnsiChar(GetBlobFromRecord(FField))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end;
    end;
  end;
end;

function TMemBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: TBlobDataType;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PAnsiChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;

{$IFDEF RX_D17}
function TMemBlobStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
var
  Temp: TBlobDataType;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PAnsiChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;
{$ENDIF}

function TMemBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0: FPosition := Offset;
    1: Inc(FPosition, Offset);
    2: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

{$IFDEF RX_D16}
function TMemBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;
{$ENDIF}

procedure TMemBlobStream.Truncate;
begin
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    FDataSet.SetBlobData(FField, FBuffer, '');
    FModified := True;
  end;
end;

function TMemBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer))
    else
      Result := Length(GetBlobFromRecord(FField))
end;

{$ENDIF RX_D3}

{ TRxMemoryDataEx }

procedure TRxMemoryDataEx.CopyStructure(Source: TDataSet);
begin
  inherited CopyStructure(source);
  RefreshFilteredRecordCount;
end;

constructor TRxMemoryDataEx.Create(AOwner: TComponent);
begin
  inherited;
  fSortOrder := soAsc;
  fSortCaseSens := scYes;
  fAutoSortOnOpen := true;
  fAutoRefreshOnFilterChanged := true;
  fFilteredRecordCount := 0;
end;

procedure TRxMemoryDataEx.EmptyTable;
begin
  inherited EmptyTable;
  RefreshFilteredRecordCount();
end;

function TRxMemoryDataEx.GetFilteredRecordCount: Integer;
begin
  if Filtered and Active then
    Result := fFilteredRecordCount
  else
    result := inherited GetRecordCount;
end;

procedure TRxMemoryDataEx.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
  inherited InternalAddRecord(buffer, append);
  if Active and Filtered then
    inc(fFilteredRecordCount);
end;

procedure TRxMemoryDataEx.InternalDelete;
begin
  inherited InternalDelete;
  if Active and Filtered then
    Dec(fFilteredRecordCount);
end;

procedure TRxMemoryDataEx.InternalPost;
var accept: Boolean;
begin
  inherited InternalPost;
  if Active and Filtered then
  begin
    accept := true;

    if assigned(OnFilterRecord) then
    begin
      OnFilterRecord(self, accept);
      if not accept then
        Dec(fFilteredRecordCount);
    end
  end
end;

procedure TRxMemoryDataEx.InternalRefresh;
begin
  inherited InternalRefresh;
  if Active and Filtered then
  begin
    RefreshFilteredRecordCount;
    First;
  end;
end;

function TRxMemoryDataEx.IsSortField(field: TField): Boolean;
var
  s, whatToSearch: string;
  fieldNameEnd: Boolean;
  i: integer;
begin
  Result := False;
  whatToSearch := LowerCase(Trim(field.FieldName));
  fieldNameEnd := false;
  i := 1; s := '';
  while (i <= Length(fSortFields)) do
  begin
    case fSortFields[i] of
      ';':
        begin
          fieldNameEnd := true;
        end;
      ' ': ;
    else
      s := s + fSortFields[i];
    end;
    if ((i + 1) > length(fSortFields)) or fieldNameEnd then
    begin
      {s = s.strip(HString::both).to_lower();}
      if s <> '' then
      begin
        if LowerCase(s) = whatToSearch then
        begin
          Result := True;
          exit;
        end
      end;
      fieldNameEnd := false;
      s := ''; ;
    end;
    Inc(i);
  end
end;

function TRxMemoryDataEx.LoadFromDataSet(Source: TDataSet;
  RecordCount: Integer; Mode: TLoadMode): Integer;
var wasFiltered: boolean;
begin
  wasFiltered := Filtered;
  result := inherited LoadFromDataSet(source, recordCount, mode);

  Filtered := wasFiltered;

  if fAutoSortOnOpen then
    ReSortOnFields(fSortOrder, fSortFields);
  RefreshFilteredRecordCount();
  First();
end;

procedure TRxMemoryDataEx.RefreshFilteredRecordCount;
var
  t: TDataSetState;
  savePlace: TBookmark;
  i: integer;
  _afterScroll, _beforeScroll: TDataSetNotifyEvent;
  dsCountingFilteredRecordCount: TDataSetState;
begin
//#define dsCountingFilteredRecordCount ((TDataSetState)(dsOpening+1))
//type TDataSetState = (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc, dsOpening);
  fFilteredRecordCount := 0;

  if Filtered and Active then
  begin
    i := 0;

    savePlace := GetBookmark();
    _afterScroll := AfterScroll;
    _beforeScroll := BeforeScroll;
    dsCountingFilteredRecordCount := TDataSetState(Ord(dsOpening) + 1);
    {store last state}
    t := SetTempState(dsCountingFilteredRecordCount);

    try
      AfterScroll := nil;
      BeforeScroll := nil;
      DisableControls;
      First;
      while not Eof do
      begin
        inc(i);
        Next();
      end;
      fFilteredRecordCount := i;

    finally
      if (fFilteredRecordCount > 0) and assigned(savePlace) and BookmarkValid(savePlace) then
        GotoBookmark(savePlace);
      FreeBookmark(savePlace);
      AfterScroll := _afterScroll;
      BeforeScroll := _beforeScroll;
      {restore state here}
      RestoreState(t);
      EnableControls;
    end
  end
end;

procedure TRxMemoryDataEx.ReSortOnFields(pSortOrder: TSortOrder;
  fields: string);
var
  sAfterScroll: TDataSetNotifyEvent;
  savePlace: TBookmark;
  b: boolean;
  oldSortFiels: string;
begin
  if fields = '' then
    fields := sortFields;

  fSortOrder := pSortOrder; // new sort order
  oldSortFiels := fSortFields;
  fSortFields := fields;

  sAfterScroll := AfterScroll;
  AfterScroll := nil;
  DisableControls();
  try
    savePlace := GetBookmark();
    b := not boolean(fSortCaseSens);
    SortOnFields(fSortFields, b, fSortOrder = soDesc);

    if assigned(savePlace) then
      GotoBookmark(savePlace);
    FreeBookmark(savePlace);
  finally
    fSortFields := oldSortFiels;
    EnableControls();
    AfterScroll := sAfterScroll;
  end;
end;

procedure TRxMemoryDataEx.SetFiltered(Value: Boolean);
begin
  inherited SetFiltered(Value);
  if (Active and fAutoRefreshOnFilterChanged) then
    Refresh()
  else
    RefreshFilteredRecordCount();
end;

procedure TRxMemoryDataEx.SetOnFilterRecord(
  const Value: TFilterRecordEvent);
begin
  inherited SetOnFilterRecord(value);
  if (fAutoRefreshOnFilterChanged) then
    InternalRefresh()
  else
    RefreshFilteredRecordCount();
end;

end.