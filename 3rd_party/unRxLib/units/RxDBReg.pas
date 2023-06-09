{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Revision and component added by JB.                   }
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

unit RxDBReg;

{$I RX.INC}
{$D-,L-,S-}

interface

uses
  Classes, SysUtils, DB, RxHintProp, ColnEdit,
  {$IFDEF RX_D6}DesignIntf, DesignEditors{$ELSE}DsgnIntf{$ENDIF}; // Polaris

{ Register data aware custom controls and components }

procedure Register;

implementation

{$R *.dcr}

uses
  TypInfo, RXResConst, RXDBCtrl, RXLookup, RxLogin, RXDBComb, RxVCLUtils,
  {$IFNDEF RX_D3}DBTables, {$ENDIF}{$IFDEF DCS}RxSelDSFrm, {$ENDIF}
  {$IFDEF RX_D3}RxMemDS, {$ENDIF}{$IFNDEF VER80}RxDBRichEd, {$ENDIF}
  Consts, LibHelp, RxDsgn, RxDBCurrEdit, RxRecordGrid;

{ TRxFieldProperty }
{ For TRxDBLookupList, TRxDBLookupCombo components }

type
  TRxFieldProperty = class(TRxDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
    function GetDataSourcePropName: string; virtual;
  end;

function TRxFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

procedure TRxFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{$IFDEF DCS}
{$IFDEF RX_D3}

{ TMemoryDataEditor }

type
  TMemoryDataEditor = class(TMemDataSetEditor)
  protected
    function CopyStructure(Source, Dest: TDataSet): Boolean; override;
  end;

function TMemoryDataEditor.CopyStructure(Source, Dest: TDataSet): Boolean;
begin
  Result := Dest is TRxMemoryData;
  if Result then
    TRxMemoryData(Dest).CopyStructure(Source);
end;

{$ENDIF RX_D3}
{$ENDIF DCS}

{$IFDEF RX_D4}

{ TDBStringProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TRxDBRecordGrid }

type
  TRxRecFieldNameProperty = class(TDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

  TRowsEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TRxDBRecordGrid }

procedure TRowsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
        TRxDBRecordGrid(Component).Rows, 'Columns');
  end
end;

function TRowsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Rows Editor';
  end;
end;

function TRowsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TRxRecFieldNameProperty.GetValueList(List: TStrings);
var
  RecordGrid: TRxDBRecordGrid;
begin
  if (GetComponent(0) is TRow) then
  begin
    RecordGrid := TRow(GetComponent(0)).GetGrid;
    if (RecordGrid <> nil) and (RecordGrid.DataLink <> nil)
      and (RecordGrid.DataLink.Dataset <> nil) then
      RecordGrid.DataLink.Dataset.GetFieldNames(List);
  end;
end;
{$ENDIF}

{ Designer registration }

procedure Register;
const
  srRXDBAware = 'RX DBAware';
  srRXTools = 'RX Tools';
begin

  {$IFDEF RX_D4}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then Exit;
  {$ENDIF}

{ Data aware components and controls }
  RegisterComponents(srRXDBAware, [
    {$IFDEF RX_D3}TRxMemoryData, TRxMemoryDataEx, {$ENDIF}
    TRxDBGrid, TRxDBLookupList, TRxDBLookupCombo, TRxLookupEdit, TDBDateEdit, TRxDBLookupEditEx, TDBCurrencyEdit,
      TRxDBCalcEdit, TRxDBComboEdit, {$IFNDEF VER80}TRxDBRichEdit, {$ENDIF}
    TDBStatusLabel, TRxLoginDialog,
      TRxDBComboBox{$IFDEF RX_D4}, TRxDBRecordGrid{$ENDIF}]);
  //RegisterComponents(srRXTools, [TRxLoginDialog]);
  {$IFDEF RX_D3}
  RegisterNonActiveX([TRxMemoryData, TRxDBGrid, TDBDateEdit,
    TDBStatusLabel, TRxDBComboBox, TRxDBLookupList,
      TRxDBLookupCombo, TRxLookupEdit, TRxDBComboEdit, TRxDBCalcEdit,
      TRxDBRichEdit, TCustomDBComboBox, TRxLookupControl, TRxLoginDialog],
      axrComponentOnly);
  {$ENDIF RX_D3}
{ Property and component editors for data aware components }
  RegisterPropertyEditor(TypeInfo(string), TRxLookupControl, 'LookupField',
    TRxFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TRxLookupEdit, 'LookupField',
    TRxFieldProperty);
  {$IFDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(Integer), TRxDBGrid, 'RowsHeight', nil);
  {$IFDEF DCS}
  RegisterComponentEditor(TRxMemoryData, TMemoryDataEditor);
  {$ENDIF DCS}
  {$ENDIF RX_D3}
  {$IFDEF RX_D4}
{ TRxDBRecordGrid }
  RegisterPropertyEditor(TypeInfo(string), TRow, 'FieldName',
    TRxRecFieldNameProperty);
  RegisterComponentEditor(TRxDBRecordGrid, TRowsEditor);
  {$ENDIF}
end;

end.