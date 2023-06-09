{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{                                                       }
{       Copyright (c) 1995 Borland International        }
{       Portions copyright (c) 1997 Master-Bank         }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxQBndDlg;

interface

{$I RX.INC}

uses
  SysUtils, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, DB
  {$IFNDEF RX_D4}, DBTables{$ENDIF};

type
  TQueryParamsDialog = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ParamValue: TEdit;
    Label2: TLabel;
    NullValue: TCheckBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label3: TLabel;
    TypeList: TComboBox;
    ParamList: TListBox;
    HelpBtn: TButton;
    procedure ParamListChange(Sender: TObject);
    procedure TypeListChange(Sender: TObject);
    procedure ParamValueExit(Sender: TObject);
    procedure NullValueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    InitList: TParams;
    PressedOK: Boolean;
    InValueExit: Boolean;
    InParamChange: Boolean;
    procedure CheckValue;
    procedure Edit;
    procedure Unbind;
  end;

function EditQueryParams(DataSet: TDataSet; List: TParams;
  AHelpContext: THelpContext{$IFDEF RX_D4} = 0{$ENDIF}): Boolean;

implementation

uses DbConsts, {$IFDEF RX_D3}BdeConst, {$ENDIF}RxVCLUtils;

{$R *.DFM}

var
  {$IFDEF RX_D4} // Polaris
  FieldTypes: array[TFieldType] of string;
  {$ELSE}
  FieldTypes: array[TFieldType] of PString;
  {$ENDIF}

procedure FillFieldTypes;
var
  ParamString: string;
  I: Integer;
  J: TFieldType;
begin
  for J := Low(TFieldType) to High(TFieldType) do
    {$IFDEF RX_D4} // Polaris
    FieldTypes[J] := '';
  {$ELSE}
    FieldTypes[J] := nil;
  {$ENDIF}
  ParamString := ResStr(SDataTypes);
  J := Low(TFieldType);
  I := 1;
  while I <= Length(ParamString) do
  begin
    {$IFDEF RX_D4} // Polaris
    FieldTypes[J] := ExtractFieldName(ParamString, I);
    {$ELSE}
    AssignStr(FieldTypes[J], ExtractFieldName(ParamString, I));
    {$ENDIF}
    Inc(J);
  end;
end;

function GetFieldType(const Value: string): TFieldType;
begin
  for Result := Low(TFieldType) to High(TFieldType) do
    {$IFDEF RX_D4} // Polaris
    if FieldTypes[Result] = Value then
      {$ELSE}
    if Assigned(FieldTypes[Result]) and (FieldTypes[Result]^ = Value) then
      {$ENDIF}
      Exit;
  Result := ftUnknown;
end;

procedure ClearFieldTypes;
var
  I: TFieldType;
begin
  for I := Low(TFieldType) to High(TFieldType) do
  begin
    {$IFDEF RX_D4} // Polaris
    FieldTypes[I] := '';
    {$ELSE}
    DisposeStr(FieldTypes[I]);
    FieldTypes[I] := nil;
    {$ENDIF}
  end;
end;

procedure DoneQBind; far;
begin
  ClearFieldTypes;
end;

function EditQueryParams(DataSet: TDataSet; List: TParams;
  AHelpContext: THelpContext{$IFDEF RX_D4} = 0{$ENDIF}): Boolean;
begin
  with TQueryParamsDialog.Create(Application) do
  try
    HelpContext := AHelpContext;
    if HelpContext = 0 then
    begin
      HelpBtn.Visible := False;
      OkBtn.Left := OkBtn.Left + HelpBtn.Width div 2;
      CancelBtn.Left := CancelBtn.Left + HelpBtn.Width div 2;
    end;
    if (csDesigning in DataSet.ComponentState) then
      Caption := Format(ResStr(SParamEditor),
        {$IFDEF RX_D3}
        {$IFDEF CBUILDER}
        [DataSet.Owner.Name, '->', DataSet.Name]);
    {$ELSE}
        {$IFDEF RX_D4}
        [DataSet.Owner.Name, '.', DataSet.Name]);
    {$ELSE}
        [DataSet.Owner.Name, DataSet.Name]);
    {$ENDIF}
    {$ENDIF}
    {$ELSE}
        [DataSet.Owner.Name, DataSet.Name]);
    {$ENDIF}
    InitList := List;
    Edit;
    Result := PressedOk;
  finally
    Free;
  end;
end;

procedure TQueryParamsDialog.Edit;
var
  I: Integer;
  J: TFieldType;
begin
  for J := Low(TFieldType) to High(TFieldType) do
    {$IFDEF RX_D4} // Polaris
    if FieldTypes[J] <> '' then
      TypeList.Items.Add(FieldTypes[J]);
  {$ELSE}
    if Assigned(FieldTypes[J]) and (FieldTypes[J]^ <> '') then
      TypeList.Items.Add(FieldTypes[J]^);
  {$ENDIF}
  if InitList.Count = 0 then
  begin
    ParamValue.Enabled := False;
    NullValue.Enabled := False;
    TypeList.Enabled := False;
    ParamList.Enabled := False;
  end
  else
  begin
    for I := 0 to InitList.Count - 1 do
      if ParamList.Items.IndexOf(InitList[I].Name) = -1 then
        ParamList.Items.Add(InitList[I].Name);
    ParamList.ItemIndex := 0;
    ParamListChange(Self);
    ActiveControl := OkBtn;
  end;
  PressedOk := ShowModal = mrOk;
end;

procedure TQueryParamsDialog.ParamListChange(Sender: TObject);
begin
  InParamChange := True;
  try
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      {$IFDEF RX_D4} // Polaris
      if FieldTypes[DataType] <> '' then
      begin
        with TypeList do
          ItemIndex := Items.IndexOf(FieldTypes[DataType]);
        {$ELSE}
      if Assigned(FieldTypes[DataType]) and (FieldTypes[DataType]^ <> '') then
      begin
        with TypeList do
          ItemIndex := Items.IndexOf(FieldTypes[DataType]^);
        {$ENDIF}
        if Bound then
          ParamValue.Text := AsString
        else
          ParamValue.Text := '';
      end
      else
      begin
        TypeList.ItemIndex := -1;
        ParamValue.Text := '';
      end;
      NullValue.Checked := IsNull;
    end;
  finally
    InParamChange := False;
  end;
end;

procedure TQueryParamsDialog.TypeListChange(Sender: TObject);
begin
  with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
  begin
    DataType := GetFieldType(TypeList.Text);
    ParamValue.Text := '';
    NullValue.Checked := IsNull;
  end;
end;

procedure TQueryParamsDialog.ParamValueExit(Sender: TObject);
begin
  if InValueExit or (ActiveControl = CancelBtn) then Exit;
  InValueExit := True;
  try
    if ParamValue.Text <> '' then NullValue.Checked := False;
    if (TypeList.Text = '') and TypeList.CanFocus then
    begin
      TypeList.SetFocus;
      raise Exception.Create(ResStr(SInvalidParamFieldType));
    end;
    if ParamValue.Text = '' then
      with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
      begin
        if NullValue.Checked then
          Clear
        else
          Unbind;
      end
    else
      CheckValue;
  finally
    InValueExit := False;
  end;
end;

procedure TQueryParamsDialog.CheckValue;
begin
  try
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      if (DataType in [ftDate, ftTime, ftDateTime]) and
        (CompareText(ParamValue.Text, 'Now') = 0) then
      begin
        case DataType of
          ftDate: Text := DateToStr(SysUtils.Date);
          ftTime: Text := TimeToStr(SysUtils.Time);
          ftDateTime: Text := DateTimeToStr(SysUtils.Now);
        end;
      end
      else
        Text := ParamValue.Text;
    end;
  except
    with ParamValue do
    begin
      if CanFocus then SetFocus;
      SelectAll;
    end;
    raise;
  end;
end;

procedure TQueryParamsDialog.Unbind;
begin
  with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
  begin
    AsInteger := 1;
    DataType := GetFieldType(TypeList.Text);
    Bound := False;
  end;
end;

procedure TQueryParamsDialog.NullValueClick(Sender: TObject);
begin
  if InParamChange then Exit;
  if NullValue.Checked then
    with InitList.ParamByName(ParamList.Items[ParamList.ItemIndex]) do
    begin
      Clear;
      ParamValue.Text := '';
    end
  else
    Unbind;
end;

procedure TQueryParamsDialog.OkBtnClick(Sender: TObject);
begin
  if not TypeList.Enabled then Exit;
  try
    ParamValueExit(Sender);
  except
    ModalResult := 0;
    raise;
  end;
end;

procedure TQueryParamsDialog.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TQueryParamsDialog.FormCreate(Sender: TObject);
begin
  {$IFDEF VER80}
  Font.Style := [fsBold];
  {$ENDIF}
end;

initialization
  FillFieldTypes;
  {$IFNDEF VER80}
finalization
  DoneQBind;
  {$ELSE}
  AddExitProc(DoneQBind);
  {$ENDIF}
end.
