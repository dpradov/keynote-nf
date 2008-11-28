{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit DBLists;

{$I RX.INC}
{$N+,P+,S-}

interface

uses SysUtils, Classes, DB, DBTables, DBUtils, BdeUtils,
{$IFDEF WIN32} 
  Windows, Bde;
{$ELSE}
  WinTypes, WinProcs, DbiTypes, DbiProcs, DbiErrs;
{$ENDIF}

type

{ TBDEItems }

  TBDEItemType = (bdDatabases, bdDrivers, bdLangDrivers, bdUsers 
    {$IFDEF WIN32}, bdRepositories {$ENDIF});

  TCustomBDEItems = class(TBDEDataSet)
  private
    FItemType: TBDEItemType;
{$IFDEF WIN32}
    FSessionName: string;
    FSessionLink: TDatabase;
    function GetDBSession: TSession;
    procedure SetSessionName(const Value: string);
{$ENDIF}
    procedure SetItemType(Value: TBDEItemType);
  protected
{$IFDEF WIN32}
    function GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE}
      Integer; override {$ENDIF};
    procedure OpenCursor {$IFDEF RX_D3}(InfoQuery: Boolean){$ENDIF}; override;
    procedure CloseCursor; override;
{$ENDIF}
    function CreateHandle: HDBICur; override;
    property ItemType: TBDEItemType read FItemType write SetItemType
      default bdDatabases;
  public
{$IFDEF WIN32}
  {$IFDEF RX_D3}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  {$ENDIF}
    property DBSession: TSession read GetDBSession;
  {$IFNDEF RX_D3}
    property RecordCount: Longint read GetRecordCount;
  {$ENDIF}
  published
    property SessionName: string read FSessionName write SetSessionName;
{$ENDIF WIN32}
  end;

  TBDEItems = class(TCustomBDEItems)
  published
    property ItemType;
  end;

{ TDBListDataSet }

  TDBListDataSet = class(TDBDataSet)
{$IFDEF WIN32}
  protected
    function GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE}
      Integer; override {$ENDIF};
  public
  {$IFDEF RX_D3}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  {$ELSE}
    property RecordCount: Longint read GetRecordCount;
  {$ENDIF}
{$ENDIF}
  end;

{ TDatabaseItems }

  TDBItemType = (dtTables, dtStoredProcs, dtFiles {$IFDEF WIN32},
    dtFunctions {$ENDIF});

  TCustomDatabaseItems = class(TDBListDataSet)
  private
    FExtended: Boolean;
    FSystemItems: Boolean;
    FFileMask: string;
    FItemType: TDBItemType;
    procedure SetFileMask(const Value: string);
    procedure SetExtendedInfo(Value: Boolean);
    procedure SetSystemItems(Value: Boolean);
    procedure SetItemType(Value: TDBItemType);
  protected
    function CreateHandle: HDBICur; override;
    function GetItemName: string;
    property ItemType: TDBItemType read FItemType write SetItemType
      default dtTables;
    property ExtendedInfo: Boolean read FExtended write SetExtendedInfo
      default False;
    property FileMask: string read FFileMask write SetFileMask;
    property SystemItems: Boolean read FSystemItems write SetSystemItems
      default False;
  public
    property ItemName: string read GetItemName;
  end;

  TDatabaseItems = class(TCustomDatabaseItems)
  published
    property ItemType;
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

{ TTableItems }

  TTabItemType = (dtFields, dtIndices, dtValChecks, dtRefInt,
    dtSecurity, dtFamily);

  TCustomTableItems = class(TDBListDataSet)
  private
    FTableName: TFileName;
    FItemType: TTabItemType;
    FPhysTypes: Boolean;
    procedure SetTableName(const Value: TFileName);
    procedure SetItemType(Value: TTabItemType);
    procedure SetPhysTypes(Value: Boolean);
  protected
    function CreateHandle: HDBICur; override;
    property ItemType: TTabItemType read FItemType write SetItemType
      default dtFields;
    property PhysTypes: Boolean read FPhysTypes write SetPhysTypes
      default False; { for dtFields only }
  published
    property TableName: TFileName read FTableName write SetTableName;
  end;

  TTableItems = class(TCustomTableItems)
  published
    property ItemType;
    property PhysTypes;
  end;

{ TDatabaseDesc }

  TDatabaseDesc = class(TObject)
  private
    FDescription: DBDesc;
  public
    constructor Create(const DatabaseName: string);
    property Description: DBDesc read FDescription;
  end;

{ TDriverDesc }

  TDriverDesc = class(TObject)
  private
    FDescription: DRVType;
  public
    constructor Create(const DriverType: string);
    property Description: DRVType read FDescription;
  end;

{*************************************************************************}

{$IFNDEF CBUILDER}
{ Obsolete classes, for backward compatibility only }

type

  TDatabaseList = class(TCustomBDEItems);

  TLangDrivList = class(TCustomBDEItems)
    constructor Create(AOwner: TComponent); override;
  end;

  TTableList = class(TCustomDatabaseItems)
  public
    function GetTableName: string;
  published
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

  TStoredProcList = class(TCustomDatabaseItems)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ExtendedInfo;
    property SystemItems;
  end;

  TFieldList = class(TCustomTableItems);

  TIndexList = class(TCustomTableItems)
    constructor Create(AOwner: TComponent); override;
  end;

{$ENDIF CBUILDER}

implementation

uses DBConsts, {$IFDEF RX_D3} BDEConst, {$ENDIF} RxDConst;

{ Utility routines }

function dsGetRecordCount(DataSet: TBDEDataSet): Longint;
begin
  if DataSet.State = dsInactive then _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(DataSet.Handle, Result));
end;

{$IFDEF WIN32}
type
  TSessionLink = class(TDatabase)
  private
    FList: TCustomBDEItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TSessionLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (AOwner <> nil) and (AOwner is TSession) then
    SessionName := TSession(AOwner).SessionName;
  Temporary := True;
  KeepConnection := False;
end;

destructor TSessionLink.Destroy;
begin
  if FList <> nil then begin
    FList.FSessionLink := nil;
    FList.Close;
  end;
  inherited Destroy;
end;
{$ENDIF}

{ TCustomBDEItems }

procedure TCustomBDEItems.SetItemType(Value: TBDEItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
  end;
end;

function TCustomBDEItems.CreateHandle: HDBICur;
begin
  case FItemType of
    bdDatabases: Check(DbiOpenDatabaseList(Result));
    bdDrivers: Check(DbiOpenDriverList(Result));
    bdLangDrivers: Check(DbiOpenLdList(Result));
    bdUsers: Check(DbiOpenUserList(Result));
{$IFDEF WIN32}
    bdRepositories: Check(DbiOpenRepositoryList(Result));
{$ENDIF}
  end;
end;

{$IFDEF WIN32}
function TCustomBDEItems.GetDBSession: TSession;
begin
  Result := Sessions.FindSession(SessionName);
  if Result = nil then
{$IFDEF RX_D3}
    Result := DBTables.Session;
{$ELSE}
    Result := DB.Session;
{$ENDIF}
end;

procedure TCustomBDEItems.SetSessionName(const Value: string);
begin
  CheckInactive;
  FSessionName := Value;
  DataEvent(dePropertyChange, 0);
end;

procedure TCustomBDEItems.OpenCursor;
var
  S: TSession;
begin
  S := Sessions.List[SessionName];
  S.Open;
  Sessions.CurrentSession := S;
  FSessionLink := TSessionLink.Create(S);
  try
    TSessionLink(FSessionLink).FList := Self;
    inherited OpenCursor{$IFDEF RX_D3}(InfoQuery){$ENDIF};
  except
    FSessionLink.Free;
    FSessionLink := nil;
    raise;
  end;
end;

procedure TCustomBDEItems.CloseCursor;
begin
  inherited CloseCursor;
  if FSessionLink <> nil then begin
    TSessionLink(FSessionLink).FList := nil;
    FSessionLink.Free;
    FSessionLink := nil;
  end;
end;
{$ENDIF WIN32}

{$IFDEF WIN32}
function TCustomBDEItems.GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;
{$ENDIF WIN32}

{$IFDEF RX_D3}
function TCustomBDEItems.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;
{$ENDIF RX_D3}

{ TDBListDataSet }

{$IFDEF RX_D3}
function TDBListDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;
{$ENDIF RX_D3}

{$IFDEF WIN32}
function TDBListDataSet.GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;
{$ENDIF WIN32}

{ TCustomDatabaseItems }

procedure TCustomDatabaseItems.SetItemType(Value: TDBItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomDatabaseItems.SetFileMask(const Value: string);
begin
  if FileMask <> Value then begin
    if Active and (FItemType in [dtTables, dtFiles]) then begin
      DisableControls;
      try
        Close;
        FFileMask := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FFileMask := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomDatabaseItems.SetExtendedInfo(Value: Boolean);
begin
  if FExtended <> Value then begin
    CheckInactive;
    FExtended := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomDatabaseItems.SetSystemItems(Value: Boolean);
begin
  if FSystemItems <> Value then begin
    if Active and (FItemType in [dtTables, dtStoredProcs]) then begin
      DisableControls;
      try
        Close;
        FSystemItems := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FSystemItems := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TCustomDatabaseItems.CreateHandle: HDBICur;
var
  WildCard: PChar;
  Pattern: array[0..DBIMAXTBLNAMELEN] of Char;
begin
  WildCard := nil;
  if FileMask <> '' then
    WildCard := AnsiToNative(DBLocale, FileMask, Pattern, SizeOf(Pattern) - 1);
  case FItemType of
    dtTables: Check(DbiOpenTableList(DBHandle, FExtended, FSystemItems, WildCard, Result));
    dtStoredProcs:
      if DataBase.IsSQLBased then
        Check(DbiOpenSPList(DBHandle, FExtended, FSystemItems, nil, Result))
      else DatabaseError(LoadStr(SLocalDatabase));
    dtFiles: Check(DbiOpenFileList(DBHandle, WildCard, Result));
{$IFDEF WIN32}
    dtFunctions:
      if DataBase.IsSQLBased then
        Check(DbiOpenFunctionList(DBHandle, DBIFUNCOpts(FExtended), @Result))
      else DatabaseError(LoadStr(SLocalDatabase));
{$ENDIF}
  end;
end;

function TCustomDatabaseItems.GetItemName: string;
const
  sObjListNameField = 'NAME';
  sFileNameField = 'FILENAME';
  sTabListExtField  = 'EXTENSION';
var
  Temp: string;
  Field: TField;
begin
  Result := '';
  if not Active then Exit;
  if FItemType = dtFiles then Field := FindField(sFileNameField)
  else Field := FindField(sObjListNameField);
  if Field = nil then Exit;
  Result := Field.AsString;
  if FItemType in [dtTables, dtFiles] then begin
    Field := FindField(sTabListExtField);
    if Field = nil then Exit;
    Temp := Field.AsString;
    if Temp <> '' then begin
      if Temp[1] <> '.' then Temp := '.' + Temp;
      Result := Result + Temp;
    end;
  end;
end;

{ TCustomTableItems }

procedure TCustomTableItems.SetItemType(Value: TTabItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomTableItems.SetPhysTypes(Value: Boolean);
begin
  if Value <> PhysTypes then begin
    if Active and (ItemType = dtFields) then begin
      DisableControls;
      try
        Close;
        FPhysTypes := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FPhysTypes := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TCustomTableItems.SetTableName(const Value: TFileName);
begin
  if Value <> FTableName then begin
    if Active then begin
      DisableControls;
      try
        Close;
        FTableName := Value;
        if FTableName <> '' then Open;
      finally
        EnableControls;
      end;
    end
    else FTableName := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TCustomTableItems.CreateHandle: HDBICur;
var
  STableName: PChar;
begin
  if FTableName = '' then _DBError(SNoTableName);
  STableName := StrAlloc(Length(FTableName) + 1);
  try
    AnsiToNative(DBLocale, FTableName, STableName, Length(FTableName));
    case FItemType of
      dtFields:
        while not CheckOpen(DbiOpenFieldList(DBHandle, STableName, nil,
          FPhysTypes, Result)) do {Retry};
      dtIndices:
        while not CheckOpen(DbiOpenIndexList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtValChecks:
        while not CheckOpen(DbiOpenVchkList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtRefInt:
        while not CheckOpen(DbiOpenRintList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtSecurity:
        while not CheckOpen(DbiOpenSecurityList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtFamily:
        while not CheckOpen(DbiOpenFamilyList(DBHandle, STableName, nil,
          Result)) do {Retry};
    end;
  finally
    StrDispose(STableName);
  end;
end;

{ TDatabaseDesc }

constructor TDatabaseDesc.Create(const DatabaseName: string);
var
  Buffer: PChar;
begin
  Buffer := StrPCopy(StrAlloc(Length(DatabaseName) + 1), DatabaseName);
  try
    Check(DbiGetDatabaseDesc(Buffer, @FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

{ TDriverDesc }

constructor TDriverDesc.Create(const DriverType: string);
var
  Buffer: PChar;
begin
  Buffer := StrPCopy(StrAlloc(Length(DriverType) + 1), DriverType);
  try
    Check(DbiGetDriverDesc(Buffer, FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

{*************************************************************************}

{$IFNDEF CBUILDER}

{ TLangDrivList }

constructor TLangDrivList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := bdLangDrivers;
end;

{ TTableList }

function TTableList.GetTableName: string;
begin
  Result := ItemName;
end;

{ TStoredProcList }

constructor TStoredProcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtStoredProcs;
end;

{ TIndexList }

constructor TIndexList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtIndices;
end;

{$ENDIF CBUILDER}

end.