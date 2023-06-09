{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDBComb;

{$I RX.INC}

interface

uses
  {$IFNDEF VER80}Windows, DbCtrls, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Menus, Graphics, Classes, Controls, DB,
  {$IFNDEF RX_D3}DBTables, {$ENDIF}StdCtrls, DBConsts,
  {$IFDEF RX_D6}VDBConsts, {$ENDIF}RxStrUtils; // Polaris

type

{ TCustomDBComboBox }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCustomDBComboBox = class(TCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    {$IFNDEF VER80}
    FPaintControl: TPaintControl;
    {$ENDIF}
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function GetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditReadOnly;
    {$IFNDEF RX_D6} // Polaris
    procedure SetItems(const Value: TStrings); // Polaris  const added
    {$ENDIF}
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    function GetComboText: string; virtual;
    procedure SetComboText(const Value: string); virtual;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    {$IFNDEF VER80}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ELSE}
    function GetStyle: TComboBoxStyle;
    {$ENDIF}
  protected
    procedure Change; override;
    procedure Click; override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
      ComboProc: Pointer); override;
    procedure CreateWnd; override;
    procedure DropDown; override;
    function GetPaintText: string; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetStyle(Value: TComboBoxStyle); {$IFNDEF VER80} override{$ELSE} virtual{$ENDIF};
    {$IFDEF RX_D6} // Polaris
    procedure SetItems(const Value: TStrings); override;
    {$ENDIF}
    procedure WndProc(var Message: TMessage); override;
    property ComboText: string read GetComboText write SetComboText;
    {$IFDEF VER80}
    property Style: TComboBoxStyle read GetStyle write SetStyle default csDropDown;
    {$ENDIF}
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$ENDIF}
    property Field: TField read GetField;
    property Items write SetItems;
    property Text;
  end;

{ TRxDBComboBox }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBComboBox = class(TCustomDBComboBox)
  private
    FValues: TStrings;
    FEnableValues: Boolean;
    procedure SetEnableValues(Value: Boolean);
    procedure SetValues(Value: TStrings);
    procedure ValuesChanged(Sender: TObject);
  protected
    procedure SetStyle(Value: TComboBoxStyle); override;
    function GetComboText: string; override;
    function GetPaintText: string; override;
    procedure SetComboText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align; // Polaris
    property Style { must be published before Items }
      default csDropDownList; // Polaris
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property EnableValues: Boolean read FEnableValues write SetEnableValues
      default True; // Polaris
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
    property Items;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Values: TStrings read FValues write SetValues;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

implementation

uses RxDBUtils, SysUtils;

{ TCustomDBComboBox }

constructor TCustomDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnEditingChange := EditingChange;
  {$IFNDEF VER80}
  FPaintControl := TPaintControl.Create(Self, 'COMBOBOX');
  {$ENDIF}
end;

destructor TCustomDBComboBox.Destroy;
begin
  {$IFNDEF VER80}
  FPaintControl.Free;
  {$ENDIF}
  FDataLink.OnDataChange := nil;
  FDataLink.OnUpdateData := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCustomDBComboBox.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TCustomDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TCustomDBComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SetEditReadOnly;
end;

procedure TCustomDBComboBox.DataChange(Sender: TObject);
begin
  if DroppedDown then Exit;
  if FDataLink.Field <> nil then
    ComboText := FDataLink.Field.Text
  else if csDesigning in ComponentState then
    ComboText := Name
  else
    ComboText := '';
end;

procedure TCustomDBComboBox.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := ComboText;
end;

procedure TCustomDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          I := -1
        else
          I := Items.IndexOf(Value);
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

function TCustomDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if Style in [csDropDown, csSimple] then
    Result := Text
  else
  begin
    I := ItemIndex;
    if I < 0 then
      Result := ''
    else
      Result := Items[I];
  end;
end;

procedure TCustomDBComboBox.Change;
begin
  FDataLink.Edit;
  inherited Change;
  FDataLink.Modified;
end;

procedure TCustomDBComboBox.Click;
begin
  FDataLink.Edit;
  inherited Click;
  FDataLink.Modified;
end;

procedure TCustomDBComboBox.DropDown;
begin
  {$IFDEF VER80}
  FDataLink.Edit;
  {$ENDIF}
  inherited DropDown;
end;

function TCustomDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBComboBox.SetDataSource(Value: TDataSource);
begin
  {$IFDEF RX_D4}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    {$ENDIF}
    FDataLink.DataSource := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
end;

function TCustomDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TCustomDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TCustomDBComboBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBComboBox.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TCustomDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCustomDBComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key in [VK_BACK, VK_DELETE, VK_UP, VK_DOWN, 32..255] then
  begin
    if not FDataLink.Edit and (Key in [VK_UP, VK_DOWN]) then
      Key := 0;
  end;
end;

procedure TCustomDBComboBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= ' ') and (FDataLink.Field <> nil) and
    not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  if (Key >= ' ') then FDataLink.Edit
  else
  case Key of
    ^H, ^V, ^X:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        {$IFDEF VER80}
        Key := #0;
        {$ENDIF}
      end;
  end;
end;

procedure TCustomDBComboBox.EditingChange(Sender: TObject);
begin
  SetEditReadOnly;
end;

procedure TCustomDBComboBox.SetEditReadOnly;
begin
  if (Style in [csDropDown, csSimple]) and HandleAllocated then
    SendMessage({$IFNDEF VER80}EditHandle{$ELSE}FEditHandle{$ENDIF},
      EM_SETREADONLY, Ord(not FDataLink.Editing), 0);
end;

procedure TCustomDBComboBox.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_COMMAND:
        if TWMCommand(Message).NotifyCode = CBN_SELCHANGE then
          if not FDataLink.Edit then
          begin
            if Style <> csSimple then
              PostMessage(Handle, CB_SHOWDROPDOWN, 0, 0);
            Exit;
          end;
      CB_SHOWDROPDOWN:
        if Message.WParam <> 0 then
          FDataLink.Edit
        else if not FDataLink.Editing then
          DataChange(Self); {Restore text}
      {$IFNDEF VER80}
      WM_CREATE,
        WM_WINDOWPOSCHANGED,
        CM_FONTCHANGED:
        FPaintControl.DestroyHandle;
      {$ENDIF}
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWnd;
  ComboProc: Pointer);
begin
  if not (csDesigning in ComponentState) then
    case Message.Msg of
      WM_LBUTTONDOWN:
        {$IFNDEF VER80}
        if (Style = csSimple) and (ComboWnd <> EditHandle) then
          {$ELSE}
        if (Style = csSimple) and (ComboWnd <> FEditHandle) then
          {$ENDIF}
          if not FDataLink.Edit then Exit;
    end;
  inherited ComboWndProc(Message, ComboWnd, ComboProc);
end;

procedure TCustomDBComboBox.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  inherited;
end;

{$IFNDEF VER80}

procedure TCustomDBComboBox.CMGetDatalink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure TCustomDBComboBox.WMPaint(var Message: TWMPaint);
var
  S: string;
  R: TRect;
  P: TPoint;
  Child: HWND;
begin
  if csPaintCopy in ControlState then
  begin
    S := GetPaintText;
    if Style = csDropDown then
    begin
      SendMessage(FPaintControl.Handle, WM_SETTEXT, 0, LPARAM(PChar(S)));
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
      Child := GetWindow(FPaintControl.Handle, GW_CHILD);
      if Child <> 0 then
      begin
        Windows.GetClientRect(Child, R);
        Windows.MapWindowPoints(Child, FPaintControl.Handle, R.TopLeft, 2);
        GetWindowOrgEx(Message.DC, P);
        SetWindowOrgEx(Message.DC, P.X - R.Left, P.Y - R.Top, nil);
        IntersectClipRect(Message.DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);
        SendMessage(Child, WM_PAINT, Message.DC, 0);
      end;
    end
    else
    begin
      SendMessage(FPaintControl.Handle, CB_RESETCONTENT, 0, 0);
      if Items.IndexOf(S) <> -1 then
      begin
        SendMessage(FPaintControl.Handle, CB_ADDSTRING, 0, LPARAM(PChar(S)));
        SendMessage(FPaintControl.Handle, CB_SETCURSEL, 0, 0);
      end;
      SendMessage(FPaintControl.Handle, WM_PAINT, Message.DC, 0);
    end;
  end
  else
    inherited;
end;
{$ENDIF}

function TCustomDBComboBox.GetPaintText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.Text
  else
    Result := '';
end;

procedure TCustomDBComboBox.SetItems(const Value: TStrings);
begin
  {$IFNDEF RX_D6}
  Items.Assign(Value);
  {$ELSE}
  inherited SetItems(Value);
  {$ENDIF}
  DataChange(Self);
end;

{$IFDEF VER80}

function TCustomDBComboBox.GetStyle: TComboBoxStyle;
begin
  Result := inherited Style;
end;
{$ENDIF}

procedure TCustomDBComboBox.SetStyle(Value: TComboBoxStyle);
begin
  {$IFNDEF VER80}
  if (Value = csSimple) and Assigned(FDatalink) and FDatalink.DatasourceFixed then
    _DBError(SNotReplicatable);
  inherited SetStyle(Value);
  {$ELSE}
  if Value = csSimple then
    ControlStyle := ControlStyle - [csFixedHeight]
  else
    ControlStyle := ControlStyle + [csFixedHeight];
  inherited Style := Value;
  RecreateWnd;
  {$ENDIF}
end;

{$IFDEF RX_D4}

function TCustomDBComboBox.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TCustomDBComboBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TCustomDBComboBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TRxDBComboBox }

constructor TRxDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues := TStringList.Create;
  TStringList(FValues).OnChange := ValuesChanged;
//  EnableValues := False;
  FEnableValues := True; // Polaris
  Style := csDropDownList; // Polaris
end;

destructor TRxDBComboBox.Destroy;
begin
  TStringList(FValues).OnChange := nil;
  FValues.Free;
  inherited Destroy;
end;

procedure TRxDBComboBox.ValuesChanged(Sender: TObject);
begin
  if FEnableValues then DataChange(Self);
end;

function TRxDBComboBox.GetPaintText: string;
var
  I: Integer;
begin
  Result := '';
  if FDataLink.Field <> nil then
  begin
    if FEnableValues then
    begin
      I := Values.IndexOf(FDataLink.Field.Text);
      if I >= 0 then
        Result := Items.Strings[I]
    end
    else
      Result := FDataLink.Field.Text;
  end;
end;

function TRxDBComboBox.GetComboText: string;
var
  I: Integer;
begin
  if (Style in [csDropDown, csSimple]) and (not FEnableValues) then
    Result := Text
  else
  begin
    I := ItemIndex;
    if (I < 0) or (FEnableValues and (FValues.Count < I + 1)) then
      Result := ''
    else if FEnableValues then
      Result := FValues[I]
    else
      Result := Items[I];
  end;
end;

procedure TRxDBComboBox.SetComboText(const Value: string);
var
  I: Integer;
  Redraw: Boolean;
begin
  if Value <> ComboText then
  begin
    if Style <> csDropDown then
    begin
      Redraw := (Style <> csSimple) and HandleAllocated;
      if Redraw then SendMessage(Handle, WM_SETREDRAW, 0, 0);
      try
        if Value = '' then
          I := -1
        else if FEnableValues then
          I := Values.IndexOf(Value)
        else
          I := Items.IndexOf(Value);
        if I >= Items.Count then I := -1;
        ItemIndex := I;
      finally
        if Redraw then
        begin
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
          Invalidate;
        end;
      end;
      if I >= 0 then Exit;
    end;
    if Style in [csDropDown, csSimple] then Text := Value;
  end;
end;

procedure TRxDBComboBox.SetEnableValues(Value: Boolean);
begin
  if FEnableValues <> Value then
  begin
    if Value and (Style in [csDropDown, csSimple]) then
      Style := csDropDownList;
    FEnableValues := Value;
    DataChange(Self);
  end;
end;

procedure TRxDBComboBox.SetValues(Value: TStrings);
begin
  FValues.Assign(Value);
end;

procedure TRxDBComboBox.SetStyle(Value: TComboboxStyle);
begin
  if (Value in [csSimple, csDropDown]) and FEnableValues then
//    Value := csDropDownList;
    FEnableValues := False; // Polaris
  inherited SetStyle(Value);
end;

end.