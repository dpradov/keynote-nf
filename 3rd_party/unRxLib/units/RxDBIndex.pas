{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDBIndex;

interface

{$I RX.INC}

uses
  SysUtils, {$IFNDEF VER80}Windows,{$ELSE}WinTypes, WinProcs,{$ENDIF} Messages,
  Classes, Controls, Forms, Graphics, Menus, StdCtrls, ExtCtrls, DB, DBTables;

type

  TIdxDisplayMode = (dmFieldLabels, dmFieldNames, dmIndexName);

{ TDBIndexCombo }

  TDBIndexCombo = class(TCustomComboBox)
  private
    FDataLink: TDataLink;
    FUpdate: Boolean;
    {$IFDEF RX_D4} // Polaris
    FNoIndexItem: string;
    {$ELSE}
    FNoIndexItem: PString;
    {$ENDIF}
    FEnableNoIndex: Boolean;
    FChanging: Boolean;
    FDisplayMode: TIdxDisplayMode;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetIndexFieldName(var AName: string): Boolean;
    procedure SetNoIndexItem(const Value: string);
    function GetNoIndexItem: string;
    procedure SetEnableNoIndex(Value: Boolean);
    procedure SetDisplayMode(Value: TIdxDisplayMode);
    procedure ActiveChanged;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure FillIndexList(List: TStrings);
    procedure Change; override;
    procedure UpdateList; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { published properties }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property NoIndexItem: string read GetNoIndexItem write SetNoIndexItem;
    property EnableNoIndex: Boolean read FEnableNoIndex write SetEnableNoIndex default False;
    property DisplayMode: TIdxDisplayMode read FDisplayMode write SetDisplayMode default dmFieldLabels;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Color;
    property Ctl3D;
    property DropDownCount;
    property Font;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFNDEF VER80}
    {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property ItemHeight;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

implementation

uses
  {$IFNDEF VER80}Bde, {$ELSE}DbiErrs, DbiTypes, DbiProcs, {$ENDIF}
  DBConsts, RxStrUtils, RxDBUtils, RxBdeUtils;

{ TKeyDataLink }

type
  TKeyDataLink = class(TDataLink)
  private
    FCombo: TDBIndexCombo;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ACombo: TDBIndexCombo);
    destructor Destroy; override;
  end;

constructor TKeyDataLink.Create(ACombo: TDBIndexCombo);
begin
  inherited Create;
  FCombo := ACombo;
end;

destructor TKeyDataLink.Destroy;
begin
  FCombo := nil;
  inherited Destroy;
end;

procedure TKeyDataLink.ActiveChanged;
begin
  if FCombo <> nil then FCombo.ActiveChanged;
end;

procedure TKeyDataLink.DataSetChanged;
begin
  if FCombo <> nil then FCombo.ActiveChanged;
end;

procedure TKeyDataLink.DataSetScrolled(Distance: Integer);
begin
  { ignore this data event }
end;

{ TDBIndexCombo }

constructor TDBIndexCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TKeyDataLink.Create(Self);
  Style := csDropDownList;
  FUpdate := False;
  {$IFDEF RX_D4} // Polaris
  FNoIndexItem := EmptyStr;
  {$ELSE}
  FNoIndexItem := NullStr;
  {$ENDIF}
  FEnableNoIndex := False;
end;

destructor TDBIndexCombo.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FNoIndexItem);
  FNoIndexItem := NullStr;
  {$ENDIF}
  inherited Destroy;
end;

procedure TDBIndexCombo.SetNoIndexItem(const Value: string);
begin
  {$IFDEF RX_D4} // Polaris
  if Value <> FNoIndexItem then
  begin
    FNoIndexItem := Value;
    {$ELSE}
  if Value <> FNoIndexItem^ then
  begin
    AssignStr(FNoIndexItem, Value);
    {$ENDIF}
    if not (csLoading in ComponentState) then ActiveChanged;
  end;
end;

procedure TDBIndexCombo.SetEnableNoIndex(Value: Boolean);
begin
  if FEnableNoIndex <> Value then
  begin
    FEnableNoIndex := Value;
    if not (csLoading in ComponentState) then ActiveChanged;
  end;
end;

procedure TDBIndexCombo.SetDisplayMode(Value: TIdxDisplayMode);
begin
  if (Value <> FDisplayMode) then
  begin
    FDisplayMode := Value;
    if not (csLoading in ComponentState) then UpdateList;
  end;
end;

function TDBIndexCombo.GetNoIndexItem: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FNoIndexItem;
  {$ELSE}
  Result := FNoIndexItem^;
  {$ENDIF}
end;

function TDBIndexCombo.GetDataSource: TDataSource;
begin
  if FDataLink <> nil then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;

procedure TDBIndexCombo.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
  if not (csLoading in ComponentState) then ActiveChanged;
end;

procedure TDBIndexCombo.ActiveChanged;
begin
  if not (Enabled and FDataLink.Active and
    FDataLink.DataSet.InheritsFrom(TTable)) then
  begin
    Clear;
    ItemIndex := -1;
  end
  else
    UpdateList;
end;

procedure TDBIndexCombo.Loaded;
begin
  inherited Loaded;
  ActiveChanged;
end;

procedure TDBIndexCombo.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBIndexCombo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then ActiveChanged;
end;

function TDBIndexCombo.GetIndexFieldName(var AName: string): Boolean;
begin
  Result := True;
  if ItemIndex >= 0 then
  begin
    if EnableNoIndex and (Items[ItemIndex] = NoIndexItem) then
      AName := ''
    else
    begin
      AName := TIndexDef(Items.Objects[ItemIndex]).Fields;
      if AName = '' then
      begin
        AName := TIndexDef(Items.Objects[ItemIndex]).Name;
        Result := False;
      end;
    end;
  end
  else
    AName := '';
end;

procedure TDBIndexCombo.FillIndexList(List: TStrings);
var
  AFld: string;
  Pos: Integer;
  I: Integer;
begin
  List.Clear;
  if not FDataLink.Active then Exit;
  with FDataLink.DataSet as TTable do
  begin
    for I := 0 to IndexDefs.Count - 1 do
      with IndexDefs[I] do
        if not (ixExpression in Options) then
        begin
          if FDisplayMode = dmIndexName then
            AFld := Name
          else
          begin
            AFld := '';
            Pos := 1;
            while Pos <= Length(Fields) do
            begin
              if AFld <> '' then AFld := AFld + '; ';
              case FDisplayMode of
                dmFieldLabels:
                  AFld := AFld + FieldByName(ExtractFieldName(Fields, Pos)).DisplayLabel;
                dmFieldNames:
                  AFld := AFld + FieldByName(ExtractFieldName(Fields, Pos)).FieldName;
              end;
            end;
          end;
          if List.IndexOf(AFld) < 0 then List.AddObject(AFld, IndexDefs[I]);
        end;
  end;
  if EnableNoIndex then
    if List.IndexOf(NoIndexItem) < 0 then List.AddObject(NoIndexItem, nil);
end;

procedure TDBIndexCombo.Change;
var
  ABookmark: TBookmark;
  AName: string;
begin
  if Enabled and FDataLink.Active and not FChanging and
    FDataLink.DataSet.InheritsFrom(TTable) and
    not (csLoading in ComponentState) then
  begin
    ABookmark := nil;
    with FDataLink.DataSet as TTable do
    begin
      if Database.IsSQLBased then ABookmark := GetBookmark;
      try
        if GetIndexFieldName(AName) then
        begin
          IndexFieldNames := AName;
          if (AName = '') and (IndexDefs.Count > 0) then
            IndexName := '';
        end
        else
        begin
          if AName = '' then IndexFieldNames := '';
          IndexName := AName;
        end;
        if (ABookmark <> nil) then
          SetToBookmark(TTable(Self.FDataLink.DataSet), ABookmark);
      finally
        if ABookmark <> nil then FreeBookmark(ABookmark);
      end;
    end;
  end;
  inherited Change;
end;

procedure TDBIndexCombo.UpdateList;

  function FindIndex(Table: TTable): Integer;
  var
    I: Integer;
    IdxFields: string;
  begin
    Result := -1;
    IdxFields := '';
    if Table.IndexFieldNames <> '' then
      for I := 0 to Table.IndexFieldCount - 1 do
      begin
        if IdxFields <> '' then IdxFields := IdxFields + ';';
        IdxFields := IdxFields + Table.IndexFields[I].FieldName;
      end;
    for I := 0 to Items.Count - 1 do
    begin
      if (Items.Objects[I] <> nil) and
        (((IdxFields <> '') and
        (AnsiCompareText(TIndexDef(Items.Objects[I]).Fields, IdxFields) = 0)) or
        ((Table.IndexName <> '') and
        (AnsiCompareText(TIndexDef(Items.Objects[I]).Name, Table.IndexName) = 0))) then
      begin
        Result := I;
        Exit;
      end;
    end;
    if EnableNoIndex and FDataLink.Active then
      if (Table.IndexFieldNames = '') and (Table.IndexName = '') then
        Result := Items.IndexOf(NoIndexItem);
  end;

begin
  if Enabled and FDataLink.Active then
  try
    Items.BeginUpdate;
    try
      if FDataLink.DataSet.InheritsFrom(TTable) then
      begin
        TTable(FDataLink.DataSet).IndexDefs.Update;
        FillIndexList(Items);
        ItemIndex := FindIndex(TTable(FDataLink.DataSet));
        FChanging := True;
      end
      else
        Items.Clear;
    finally
      Items.EndUpdate;
    end;
  finally
    FChanging := False;
  end;
end;

end.