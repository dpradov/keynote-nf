{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit MemTable;

{$I RX.INC}
{$N+,P+,S-}

interface

uses SysUtils, Classes, Controls, {$IFDEF WIN32} Bde, {$ELSE} DbiTypes,
  DbiProcs, DbiErrs, {$ENDIF} DB, DBTables;

type

{ TMemoryTable }

  TMemoryTable = class(TDBDataSet)
  private
    FTableName: TFileName;
    FMoveHandle: HDBICur;
    FEnableDelete: Boolean;
    FDisableEvents: Boolean;
    procedure EncodeFieldDesc(var FieldDesc: FLDDesc;
      const Name: string; DataType: TFieldType; Size
      {$IFDEF RX_D4}, Precision {$ENDIF}: Word);
    procedure SetTableName(const Value: TFileName);
    function SupportedFieldType(AType: TFieldType): Boolean;
    procedure DeleteCurrentRecord;
  protected
    function CreateHandle: HDBICur; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
{$IFDEF RX_D3}
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;
{$ENDIF}
{$IFDEF WIN32}
    function GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE}
      Integer; override {$ENDIF};
{$ENDIF}
{$IFDEF RX_D3}
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalDelete; override;
{$ELSE}
    procedure DoBeforeDelete; override;
    function GetRecordNumber: Longint; {$IFNDEF VER80} override; {$ENDIF}
    procedure SetRecNo(Value: Longint);
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function BatchMove(ASource: TDataSet; AMode: TBatchMode;
      ARecordCount: Longint): Longint;
    procedure CopyStructure(ASource: TDataSet);
    procedure CreateTable;
    procedure DeleteTable;
    procedure EmptyTable;
    procedure GotoRecord(RecordNo: Longint);
{$IFDEF RX_D3}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
{$ENDIF}
    procedure SetFieldValues(const FieldNames: array of string;
      const Values: array of const);
{$IFNDEF RX_D3}
{$IFNDEF VER80}
    property RecordCount: Longint read GetRecordCount;
{$ENDIF}
{$ENDIF}
{$IFNDEF RX_D3}
    property RecNo: Longint read GetRecordNumber write SetRecNo;
{$ENDIF}
  published
    property EnableDelete: Boolean read FEnableDelete write FEnableDelete
      default True;
    property TableName: TFileName read FTableName write SetTableName;
  end;

implementation

uses DBConsts, DBUtils, BdeUtils, {$IFDEF RX_D3} BDEConst, {$ENDIF} 
  Forms, MaxMin;

{ Memory tables are created in RAM and deleted when you close them. They
  are much faster and are very useful when you need fast operations on
  small tables. Memory tables do not support certain features (like
  deleting records, referntial integrity, indexes, autoincrement fields
  and BLOBs) }

{ TMemoryTable }

constructor TMemoryTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableDelete := True;
end;

function TMemoryTable.BatchMove(ASource: TDataSet; AMode: TBatchMode;
  ARecordCount: Longint): Longint;
var
  SourceActive: Boolean;
  MovedCount: Longint;
begin
  if (ASource = nil) or (Self = ASource) or
    not (AMode in [batCopy, batAppend]) then _DBError(SInvalidBatchMove);
  SourceActive := ASource.Active;
  try
    ASource.DisableControls;
    DisableControls;
    ASource.Open;
    ASource.CheckBrowseMode;
    ASource.UpdateCursorPos;
    if AMode = batCopy then begin
      Close;
      CopyStructure(ASource);
    end;
    if not Active then Open;
    CheckBrowseMode;
    if ARecordCount > 0 then begin
      ASource.UpdateCursorPos;
      MovedCount := ARecordCount;
    end
    else begin
      ASource.First;
      MovedCount := MaxLongint;
    end;
    try
      Result := 0;
      while not ASource.EOF do begin
        Append;
        AssignRecord(ASource, Self, True);
        Post;
        Inc(Result);
        if Result >= MovedCount then Break;
        ASource.Next;
      end;
    finally
      Self.First;
    end;
  finally
    if not SourceActive then ASource.Close;
    Self.EnableControls;
    ASource.EnableControls;
  end;
end;

procedure TMemoryTable.CopyStructure(ASource: TDataSet);

  procedure CreateField(FieldDef: TFieldDef; AOwner: TComponent);
  begin
{$IFDEF RX_D4}
    FieldDef.CreateField(AOwner, nil, FieldDef.Name, True);
{$ELSE}
    FieldDef.CreateField(AOwner);
{$ENDIF}
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do Fields[I].Free;
  if (ASource = nil) then Exit;
  ASource.FieldDefs.Update;
  FieldDefs := ASource.FieldDefs;
  for I := 0 to FieldDefs.Count - 1 do begin
    if SupportedFieldType(FieldDefs.Items[I].DataType) then begin
      if (csDesigning in ComponentState) and (Owner <> nil) then
        CreateField(FieldDefs.Items[I], Owner)
      else
        CreateField(FieldDefs.Items[I], Self);
    end;
  end;
end;

procedure TMemoryTable.DeleteCurrentRecord;
var
  CurRecNo, CurRec: Longint;
  Buffer: Pointer;
  iFldCount: Word;
  FieldDescs: PFLDDesc;
begin
  CurRecNo := RecNo;
  iFldCount := FieldDefs.Count;
  FieldDescs := AllocMem(iFldCount * SizeOf(FLDDesc));
  try
    Check(DbiGetFieldDescs(Handle, FieldDescs));
    Check(DbiCreateInMemTable(DBHandle, '$InMem$', iFldCount, FieldDescs,
      FMoveHandle));
    try
      DisableControls;
      Buffer := AllocMem(RecordSize);
      try
        First;
        CurRec := 0;
        while not Self.EOF do begin
          Inc(CurRec);
          if CurRec <> CurRecNo then begin
            DbiInitRecord(FMoveHandle, Buffer);
            Self.GetCurrentRecord(Buffer);
            Check(DbiAppendRecord(FMoveHandle, Buffer));
          end;
          Self.Next;
        end;
        FDisableEvents := True;
        try
          Close;
          Open;
          FMoveHandle := nil;
        finally
          FDisableEvents := False;
        end;
      finally
        FreeMem(Buffer, RecordSize);
      end;
    except
      DbiCloseCursor(FMoveHandle);
      FMoveHandle := nil;
      raise;
    end;
    GotoRecord(CurRecNo - 1);
  finally
    if FieldDescs <> nil then
      FreeMem(FieldDescs, iFldCount * SizeOf(FLDDesc));
    FMoveHandle := nil;
    EnableControls;
  end;
end;

{$IFDEF RX_D3}

function TMemoryTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PChar;
begin
  Result := inherited GetFieldData(Field, Buffer);
  if not Result then begin
    RecBuf := nil;
    case State of
      dsBrowse: if not IsEmpty then RecBuf := ActiveBuffer;
      dsEdit, dsInsert: RecBuf := ActiveBuffer;
      dsCalcFields: RecBuf := CalcBuffer;
    end;
    if RecBuf = nil then Exit;
    with Field do
      if (FieldNo > 0) then begin
        Check(DbiGetField(Handle, FieldNo, RecBuf, nil, IsBlank));
        Result := not IsBlank;
      end;
  end;
end;

procedure TMemoryTable.InternalDelete;
begin
  if EnableDelete then DeleteCurrentRecord
  else inherited;
end;

function TMemoryTable.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TMemoryTable.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := False;
end;

{$ELSE}

procedure TMemoryTable.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  if EnableDelete then begin
    DeleteCurrentRecord;
    DoAfterDelete;
    SysUtils.Abort;
  end;
end;

{$ENDIF}

procedure TMemoryTable.DoAfterClose;
begin
  if not FDisableEvents then inherited DoAfterClose;
end;

procedure TMemoryTable.DoAfterOpen;
begin
  if not FDisableEvents then inherited DoAfterOpen;
end;

procedure TMemoryTable.DoBeforeClose;
begin
  if not FDisableEvents then inherited DoBeforeClose;
end;

procedure TMemoryTable.DoBeforeOpen;
begin
  if not FDisableEvents then inherited DoBeforeOpen;
end;

{$IFDEF RX_D3}

procedure TMemoryTable.DoBeforeScroll;
begin
  if not FDisableEvents then inherited DoBeforeScroll;
end;

procedure TMemoryTable.DoAfterScroll;
begin
  if not FDisableEvents then inherited DoAfterScroll;
end;

{$ENDIF}

function TMemoryTable.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := not (AType in [ftUnknown {$IFDEF RX_D4}, ftWideString {$ENDIF}
    {$IFDEF RX_D5}, ftOraBlob, ftOraClob, ftVariant, ftInterface, 
    ftIDispatch, ftGuid {$ENDIF}] + ftNonTextTypes);
end;

function TMemoryTable.CreateHandle: HDBICur;
var
  I: Integer;
{$IFDEF RX_D4}
  FldDescList: TFieldDescList;
  FieldDescs: PFLDDesc;
{$ELSE}
  FieldDescs: PFLDDesc;
{$ENDIF}
  iFldCount: Cardinal;
  szTblName: DBITBLNAME;
begin
  if (FMoveHandle <> nil) then begin
    Result := FMoveHandle;
    Exit;
  end;
  if FieldCount > 0 then FieldDefs.Clear;
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do begin
      if not SupportedFieldType(Fields[I].DataType) then
{$IFDEF RX_D3}
 {$IFDEF RX_D4}
        DatabaseErrorFmt(SUnknownFieldType, [Fields[I].FieldName]);
 {$ELSE}
        DatabaseErrorFmt(SFieldUnsupportedType, [Fields[I].FieldName]);
 {$ENDIF}
{$ELSE}
        DBErrorFmt(SFieldUnsupportedType, [Fields[I].FieldName]);
{$ENDIF}
      with Fields[I] do
        if not (Calculated {$IFDEF WIN32} or Lookup {$ENDIF}) then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
{$IFNDEF RX_D4}
  FieldDescs := nil;
{$ENDIF}
  iFldCount := FieldDefs.Count;
  SetDBFlag(dbfTable, True);
  try
    if TableName = '' then
      AnsiToNative(Locale, '$RxInMem$', szTblName, SizeOf(szTblName) - 1)
    else
      AnsiToNative(Locale, TableName, szTblName, SizeOf(szTblName) - 1);
{$IFDEF RX_D4}
    SetLength(FldDescList, iFldCount);
    FieldDescs := BDE.PFLDDesc(FldDescList);
{$ELSE}
    FieldDescs := AllocMem(iFldCount * SizeOf(FLDDesc));
{$ENDIF}
    for I := 0 to FieldDefs.Count - 1 do begin
      with FieldDefs[I] do
{$IFDEF RX_D4}
        EncodeFieldDesc(FldDescList[I], Name, DataType, Size, Precision);
{$ELSE}
        EncodeFieldDesc(PFieldDescList(FieldDescs)^[I], Name, DataType, Size);
{$ENDIF}
    end;
    Check(DbiTranslateRecordStructure(nil, iFldCount, FieldDescs, nil, nil,
      FieldDescs {$IFDEF WIN32}, False {$ENDIF}));
    Check(DbiCreateInMemTable(DBHandle, szTblName, iFldCount, FieldDescs,
      Result));
  finally
{$IFNDEF RX_D4}
    if FieldDescs <> nil then FreeMem(FieldDescs, iFldCount * SizeOf(FLDDesc));
{$ENDIF}
    SetDBFlag(dbfTable, False);
  end;
end;

procedure TMemoryTable.CreateTable;
begin
  CheckInactive;
  Open;
end;

procedure TMemoryTable.DeleteTable;
begin
  CheckBrowseMode;
  Close;
end;

procedure TMemoryTable.EmptyTable;
begin
  if Active then begin
    CheckBrowseMode;
    DisableControls;
    FDisableEvents := True;
    try
      Close;
      Open;
    finally
      FDisableEvents := False;
      EnableControls;
    end;
  end;
end;

procedure TMemoryTable.EncodeFieldDesc(var FieldDesc: FLDDesc;
  const Name: string; DataType: TFieldType; Size
  {$IFDEF RX_D4}, Precision {$ENDIF}: Word);
begin
  with FieldDesc do begin
    FillChar(szName, SizeOf(szName), 0);
    AnsiToNative(Locale, Name, szName, SizeOf(szName) - 1);
    iFldType := FieldLogicMap(DataType);
    iSubType := FieldSubtypeMap(DataType);
{$IFDEF WIN32}
    if iSubType = fldstAUTOINC then iSubType := 0;
{$ENDIF WIN32}
    case DataType of
{$IFDEF RX_D4}
      ftString, ftFixedChar, ftBytes, ftVarBytes, ftBlob..ftTypedBinary:
{$ELSE}
      ftString, ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic
      {$IFDEF WIN32}, ftFmtMemo, ftParadoxOle, ftDBaseOle,
      ftTypedBinary {$ENDIF}:
{$ENDIF}
        iUnits1 := Size;
      ftBCD:
        begin
{$IFDEF RX_D4}
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then iUnits1 := Precision
          else iUnits1 := 32;
{$ELSE}
          iUnits1 := 32;
{$ENDIF}
          iUnits2 := Size;  {Scale}
        end;
    end;
  end;
end;

{$IFDEF WIN32}
function TMemoryTable.GetRecordCount: {$IFNDEF RX_D3} Longint {$ELSE} Integer {$ENDIF};
begin
  if State = dsInactive then _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(Handle, Result));
end;
{$ENDIF WIN32}

procedure TMemoryTable.SetRecNo(Value: {$IFDEF RX_D3} Integer {$ELSE} Longint {$ENDIF});
var
  Rslt: DBIResult;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Rslt := DbiSetToSeqNo(Handle, Value);
  if Rslt = DBIERR_EOF then Last
  else if Rslt = DBIERR_BOF then First
  else begin
    Check(Rslt);
    Resync([rmExact, rmCenter]);
  end;
end;

{$IFDEF RX_D3}
function TMemoryTable.GetRecNo: Integer;
{$ELSE}
function TMemoryTable.GetRecordNumber: Longint;
{$ENDIF}
var
  Rslt: DBIResult;
begin
  Result := -1;
  if State in [dsBrowse, dsEdit] then begin
    UpdateCursorPos;
    Rslt := DbiGetSeqNo(Handle, Result);
    if (Rslt = DBIERR_EOF) or (Rslt = DBIERR_BOF) then Exit
    else Check(Rslt);
  end;
end;

procedure TMemoryTable.GotoRecord(RecordNo: Longint);
begin
  RecNo := RecordNo;
end;

{$IFDEF RX_D3}
function TMemoryTable.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;
{$ENDIF RX_D3}

procedure TMemoryTable.SetFieldValues(const FieldNames: array of string;
  const Values: array of const);
var
  I: Integer;
  Pos: Longint;
begin
  Pos := RecNo;
  DisableControls;
  try
    First;
    while not EOF do begin
      Edit;
      for I := 0 to Max(High(FieldNames), High(Values)) do
        FieldByName(FieldNames[I]).AssignValue(Values[I]);
      Post;
      Next;
    end;
    GotoRecord(Pos);
  finally
    EnableControls;
  end;
end;

procedure TMemoryTable.SetTableName(const Value: TFileName);
begin
  CheckInactive;
  FTableName := Value;
  DataEvent(dePropertyChange, 0);
end;

end.
