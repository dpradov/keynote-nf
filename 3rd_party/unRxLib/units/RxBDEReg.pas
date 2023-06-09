{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

{ Note:
  - in Delphi 5.0 you must add DCLRX5 to the requires page of the
    package you install this components into.
  - in Delphi 4.0 you must add DCLRX4 to the requires page of the
    package you install this components into.
  - in Delphi 3.0 you must add DCLRXCTL to the requires page of the
    package you install this components into.
  - in C++Builder 4.0 you must add DCLRX4 to the requires page of the
    package you install this components into.
  - in C++Builder 3.0 you must add DCLRXCTL to the requires page of the
    package you install this components into. }

unit RxBDEReg;

{$I RX.INC}
{$D-,L-,S-}

interface

uses Classes, SysUtils, DB, DBTables,
  {$IFDEF RX_D6}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF}; // Polaris

{ Register data aware custom controls }

procedure Register;

implementation

{$R *.dcr}

uses TypInfo, RxDBLists, RXResConst, RxDBQBE, RxDBFilter, RxDBIndex, RxDBPrgrss,
  RxLogin, RxDBSecur, RXQuery, RxVCLUtils, RxDBExcpt, RxDsgn,
  {$IFDEF DCS}RxSelDSFrm, {$ENDIF}{$IFDEF RX_MIDAS}RxRemLog, {$ENDIF}
  {$IFDEF RX_D3}RxQBndDlg, {$ELSE}
  {$IFDEF VER80}RxQBndDlg, {$ELSE}QBindDlg, {$ENDIF}{$ENDIF}
  Consts, LibHelp, RxMemTable;

{$IFNDEF VER80}

{ TSessionNameProperty }

type
  TSessionNameProperty = class(TRxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

{$ENDIF}

{ TDatabaseNameProperty }

type
  TDatabaseNameProperty = class(TRxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDatabaseNameProperty.GetValueList(List: TStrings);
{$IFNDEF VER80}
var
  S: TSession;
  {$ENDIF}
begin
  {$IFNDEF VER80}
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else if (GetComponent(0) is TSQLScript) then
  begin
    S := Sessions.FindSession((GetComponent(0) as TSQLScript).SessionName);
    if S = nil then S := Session;
    S.GetDatabaseNames(List);
  end;
  {$ELSE}
  Session.GetDatabaseNames(List);
  {$ENDIF}
end;

{ TTableNameProperty }
{ For TFieldList, TIndexList components }

type
  TTableNameProperty = class(TRxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TTableNameProperty.GetValueList(List: TStrings);
begin
  {$IFNDEF VER80}
  (GetComponent(0) as TCustomTableItems).DBSession.GetTableNames((GetComponent(0)
    as TCustomTableItems).DatabaseName, '', True, False, List);
  {$ELSE}
  Session.GetTableNames((GetComponent(0) as TCustomTableItems).DatabaseName,
    '', True, False, List);
  {$ENDIF}
end;

{$IFNDEF RX_D4}

{$IFNDEF VER90}
{$IFNDEF VER93}

function EditQueryParams(DataSet: TDataSet; List: TParams): Boolean;
begin
  Result := QBndDlg.EditQueryParams(DataSet, List, hcDQuery);
end;
{$ENDIF}
{$ENDIF}

{ TRxParamsProperty }

type
  TRxParamsProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

function TRxParamsProperty.GetValue: string;
var
  Params: TParams;
begin
  Params := TParams(Pointer(GetOrdValue));
  if Params.Count > 0 then
    {$IFNDEF VER80}
    Result := Format('(%s)', [GetPropInfo.Name])
      {$ELSE}
    Result := Format('(%s)', [GetPropInfo^.Name])
      {$ENDIF}
  else
    Result := ResStr(srNone);
end;

function TRxParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog];
end;

procedure TRxParamsProperty.Edit;
var
  List, Params: TParams;
  Query: TDataSet;
  QueryCreated: Boolean;
  I: Integer;
begin
  QueryCreated := False;
  if GetComponent(0) is TDataSet then
    Query := GetComponent(0) as TDataSet
  else
  begin
    Query := TQuery.Create(GetComponent(0) as TComponent);
    QueryCreated := True;
  end;
  try
    Params := TParams(GetOrdProp(GetComponent(0), GetPropInfo));
    if QueryCreated then TQuery(Query).Params := Params;
    List := TParams.Create;
    try
      List.Assign(Params);
      if EditQueryParams(Query, List){$IFNDEF VER80} and not
      List.IsEqual(Params){$ENDIF} then
      begin
        {$IFNDEF VER80}
        Modified;
        {$ELSE}
        if Designer <> nil then Designer.Modified;
        {$ENDIF}
        Query.Close;
        for I := 0 to PropCount - 1 do
        begin
          Params := TParams(GetOrdProp(GetComponent(I),
            TypInfo.GetPropInfo(GetComponent(I).ClassInfo,
            {$IFNDEF VER80}
            GetPropInfo.Name)));
          {$ELSE}
            GetPropInfo^.Name)));
          {$ENDIF}
          Params.AssignValues(List);
        end;
      end;
    finally
      List.Free;
    end;
  finally
    if QueryCreated then Query.Free;
  end;
end;

{$ENDIF RX_D4}

{ TUserTableNameProperty }
{ For TDBSecurity component }

type
  TUserTableNameProperty = class(TRxDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TUserTableNameProperty.GetValueList(List: TStrings);
var
  Security: TDBSecurity;
begin
  Security := GetComponent(0) as TDBSecurity;
  if Security.Database <> nil then
  begin
    {$IFNDEF VER80}
    Security.Database.Session.GetTableNames(Security.Database.DatabaseName,
      '*.*', True, False, List);
    {$ELSE}
    Session.GetTableNames(Security.Database.DatabaseName, '*.*',
      True, False, List);
    {$ENDIF}
  end;
end;

{ TLoginNameFieldProperty }
{ For TDBSecurity component }

type
  TLoginNameFieldProperty = class(TRxDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLoginNameFieldProperty.GetValueList(List: TStrings);
var
  Security: TDBSecurity;
  Table: TTable;
begin
  Security := GetComponent(0) as TDBSecurity;
  if (Security.Database <> nil) and (Security.UsersTableName <> '') then
  begin
    Table := TTable.Create(Security);
    try
      Table.DatabaseName := Security.Database.DatabaseName;
      Table.TableName := Security.UsersTableName;
      Table.GetFieldNames(List);
    finally
      Table.Free;
    end;
  end;
end;

{$IFDEF DCS}

{ TMemoryTableEditor }

type
  TMemoryTableEditor = class(TMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TMemoryTableEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TMemoryTable;
  if Result then
    TMemoryTable(Dest).CopyStructure(Source);
end;

{$ENDIF DCS}

{ Designer registration }

procedure Register;
const
  srRXDBAware = 'RX DBAware';
begin
  {$IFDEF RX_D4}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then Exit;
  {$ENDIF}

{ Data aware components and controls }
  RegisterComponents(srRXDBAware, [TRxQuery, TSQLScript, TRxDBGridSorter,
    TMemoryTable, TQBEQuery, TRxDBFilter, TDBIndexCombo, TDBProgress,
      TDBSecurity]);
  {$IFDEF RX_MIDAS}
{ MIDAS components }
  RegisterComponents(srRXDBAware, [TRxRemoteLogin]);
  RegisterNonActiveX([TRxRemoteLogin], axrComponentOnly);
  {$ENDIF}
{ Database lists }
  RegisterComponents(srRXDBAware, [TBDEItems, TDatabaseItems,
    TTableItems]);
  {$IFNDEF CBUILDER}
  {$IFDEF USE_OLD_DBLISTS}
  RegisterComponents(srRXDBAware, [TDatabaseList, TLangDrivList,
    TTableList, TStoredProcList, TFieldList, TIndexList]);
  {$ENDIF USE_OLD_DBLISTS}
  {$ENDIF CBUILDER}

  {$IFDEF RX_D3}
  RegisterNonActiveX([TRxQuery, TSQLScript, TMemoryTable, TQBEQuery,
    TRxDBFilter, TDBIndexCombo, TDBProgress, TDBSecurity, TBDEItems,
      TDatabaseItems, TTableItems], axrComponentOnly);
  {$ENDIF RX_D3}

{ Property and component editors for data aware controls }

  RegisterPropertyEditor(TypeInfo(TFileName), TCustomTableItems, 'TableName',
    TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TDBSecurity,
    'UsersTableName', TUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBSecurity,
    'LoginNameField', TLoginNameFieldProperty);

  {$IFDEF DCS}
  RegisterComponentEditor(TMemoryTable, TMemoryTableEditor);
  {$ENDIF}

  {$IFNDEF RX_D4}
  RegisterPropertyEditor(TypeInfo(TParams), TQBEQuery, 'Params',
    TRxParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TRxQuery, 'Macros',
    TRxParamsProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TSQLScript, 'Params',
    TRxParamsProperty);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TSQLScript, 'DatabaseName',
    TDatabaseNameProperty);
  {$IFNDEF VER80}
  RegisterPropertyEditor(TypeInfo(string), TCustomBDEItems, 'SessionName',
    TSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TSQLScript, 'SessionName',
    TSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBProgress, 'SessionName',
    TSessionNameProperty);
  {$ELSE}
  DbErrorIntercept;
  {$ENDIF}

end;

end.
