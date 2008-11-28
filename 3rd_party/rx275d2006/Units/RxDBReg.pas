{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
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

uses Classes, RTLConsts, DesignIntf, DesignEditors, VCLEditors, SysUtils, DB;

{ Register data aware custom controls and components }

procedure Register;

implementation

{$IFDEF WIN32}
 {$R *.D32}
{$ELSE}
 {$R *.D16}
{$ENDIF}

uses TypInfo, RXLConst, RXDBCtrl, RXLookup, RxLogin, RXDBComb, VCLUtils,
  {$IFNDEF RX_D3} DBTables, {$ENDIF} {$IFDEF DCS} SelDSFrm, {$ENDIF} 
  {$IFDEF RX_D3} RxMemDS, {$ENDIF} {$IFDEF WIN32} DBRichEd, {$ENDIF} 
  Consts, LibHelp, RxDsgn;

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

{ Designer registration }

procedure Register;
begin

{$IFDEF RX_D4}
  { Database Components are excluded from the STD SKU }
  if GDAL = LongWord(-16) then Exit;
{$ENDIF}

{ Data aware components and controls }
  RegisterComponents(LoadStr(srRXDBAware), [
    {$IFDEF RX_D3} TRxMemoryData, {$ENDIF}
    TRxDBGrid, TRxDBLookupList, TRxDBLookupCombo, TRxLookupEdit, TDBDateEdit, 
    TRxDBCalcEdit, TRxDBComboEdit, {$IFDEF WIN32} TRxDBRichEdit, {$ENDIF}
    TDBStatusLabel, TRxDBComboBox]);
  RegisterComponents(LoadStr(srRXTools), [TRxLoginDialog]);
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

end;

end.