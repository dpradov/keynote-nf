{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxSbEdit;

{$I RX.INC}

interface

uses
  {$IFNDEF VER80}
  Windows,
  {$ELSE}
  WinTypes, WinProcs,
  {$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, RxSpeedbar,
  Menus, RxPlacemnt, RxConst, RxCtrls, RxVCLUtils,
  {$IFDEF RX_D6}DesignIntf, DesignWindows, DesignEditors, Types
  {$ELSE}LibIntf, DsgnWnds, DsgnIntf{$ENDIF}; // Polaris

type

{ TSpeedbarEditor }

  TSelectData = record
    bRowCount: Integer;
    bRow: Integer;
    sRowCount: Integer;
    sRow: Integer;
  end;

  TSpeedbarEditor = class(TDesignWindow)
    SectionsBox: TGroupBox;
    NewSection: TButton;
    DelSection: TButton;
    ButtonsBox: TGroupBox;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    AddButton: TButton;
    RemoveButton: TButton;
    CloseBtn: TButton;
    SectionName: TEdit;
    SectionNameLabel: TLabel;
    SectionList: TDrawGrid;
    ButtonsList: TDrawGrid;
    LabelHint: TLabel;
    PopupMenu: TPopupMenu;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    CutMenu: TMenuItem;
    FormPlacement1: TFormPlacement;
    procedure DelSectionClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SectionNameExit(Sender: TObject);
    procedure SectionListSelectCell(Sender: TObject; Col, Row: LongInt;
      var CanSelect: Boolean);
    procedure SectionListDrawCell(Sender: TObject; Col, Row: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure ButtonsListDblClick(Sender: TObject);
    procedure ButtonsListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ButtonsListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListSelectCell(Sender: TObject; Col, Row: LongInt;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewSectionClick(Sender: TObject);
    procedure SectionNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonsListDrawCell(Sender: TObject; Col, Row: LongInt;
      Rect: TRect; State: TGridDrawState);
    procedure SectionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SectionListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SectionListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SectionListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CopyMenuClick(Sender: TObject);
    procedure PasteMenuClick(Sender: TObject);
    procedure CutMenuClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FButton: TBtnControl;
    FImage: TButtonImage;
    FBar: TSpeedBar;
    FDrag: Boolean;
    FDragItem: TSpeedItem;
    FLocked: Integer;
    FSelectData: TSelectData;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure OnPasteItem(Item: TObject);
    procedure SaveSelection;
    procedure RestoreSelection;
    procedure SelectButton(Section: Integer; Item: TSpeedItem; SelectBar: Boolean);
    procedure UpdateEnabled(BtnRow, Section: Integer);
    function CheckSpeedBar: Boolean;
    function ConfirmDelete: Boolean;
    function CurrentSection: Integer;
    function GetForm: TCustomForm;
    procedure SetSection(Section: Integer);
    procedure UpdateData;
    procedure UpdateListHeight;
    procedure SetSpeedBar(Value: TSpeedBar);
    function ItemByRow(Row: Integer): TSpeedItem;
    function SectionByRow(Row: Integer): TSpeedbarSection;
    function ItemBySectionRow(Section, Row: Integer): TSpeedItem;
    procedure CMSpeedBarChanged(var Message: TMessage); message CM_SPEEDBARCHANGED;
  protected
    procedure Activated; override;
    function UniqueName(Component: TComponent): string; override;
  public
    { Public declarations }
    {$IFNDEF RX_D6} // Polaris
    procedure FormModified; override;
    {$IFDEF RX_D3}
    procedure FormClosed(Form: TCustomForm); override;
    {$ELSE}
    procedure FormClosed(Form: TForm); override;
    {$ENDIF}
    {$ENDIF}
    function GetEditState: TEditState; override;
    {$IFDEF RX_D6} // Polaris
    function EditAction(Action: TEditAction): Boolean; override;
    {$ELSE}
    procedure EditAction(Action: TEditAction); override;
    {$ENDIF}
    property SpeedBar: TSpeedBar read FBar write SetSpeedBar;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TSpeedbarCompEditor }

  TSpeedbarCompEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses TypInfo, RxMaxMin, RXResConst, RxProps, RxDsgn;

{$R *.DFM}

{$IFNDEF VER80}
{$D-}
{$ENDIF}

{$IFDEF RX_D4}
type
  TDesigner = IDesigner;
  {$IFDEF RX_D6} // Polaris
  TFormDesigner = IDesigner;
  {$ELSE}
  TFormDesigner = IFormDesigner;
  {$ENDIF}
{$ENDIF}

{ Utility routines }

function FindEditor(Speedbar: TSpeedbar): TSpeedbarEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is TSpeedbarEditor then
    begin
      if TSpeedbarEditor(Screen.Forms[I]).SpeedBar = SpeedBar then
      begin
        Result := TSpeedbarEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowSpeedbarDesigner(Designer: TDesigner; Speedbar: TSpeedbar);
var
  Editor: TSpeedbarEditor;
begin
  if Speedbar = nil then Exit;
  Editor := FindEditor(Speedbar);
  if Editor <> nil then
  begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  end
  else
  begin
    Editor := TSpeedbarEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.Speedbar := Speedbar;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end;
end;

{ TSpeedbarCompEditor }

procedure TSpeedbarCompEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowSpeedbarDesigner(Designer, TSpeedbar(Component));
  end;
end;

function TSpeedbarCompEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := RxLoadStr(srSpeedbarDesigner);
  end;
end;

function TSpeedbarCompEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TSpeedbarEditor }

const
  MaxBtnListHeight = 158;

function TSpeedbarEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
  {$IFDEF VER80}
  Comp: TComponent;
  I: Integer;
  {$ENDIF}
begin
  Result := '';
  if (Component <> nil) then
    Temp := Component.ClassName
  else
    Temp := TSpeedItem.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
  {$IFNDEF VER80}
  Result := Designer.UniqueName(Temp);
  {$ELSE}
  I := 1;
  repeat
    Result := Temp + IntToStr(I);
    Comp := OwnerForm.FindComponent(Result);
    Inc(I);
  until (Comp = nil) or (Comp = Component);
  {$ENDIF}
end;

function TSpeedbarEditor.GetEditState: TEditState;
begin
  Result := [];
  if RemoveButton.Enabled then
  begin
    Result := [esCanDelete, esCanCut, esCanCopy];
  end;
  if AddButton.Enabled and ClipboardComponents then
    Include(Result, esCanPaste);
end;

{$IFDEF RX_D6} // Polaris

function TSpeedbarEditor.EditAction(Action: TEditAction): Boolean;
{$ELSE}

procedure TSpeedbarEditor.EditAction(Action: TEditAction);
{$ENDIF}
begin
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: RemoveButtonClick(Self);
  end;
  {$IFDEF RX_D6} // Polaris
  Result := True;
  {$ENDIF}
end;

procedure TSpeedbarEditor.SelectButton(Section: Integer; Item: TSpeedItem;
  SelectBar: Boolean);
var
  {$IFDEF RX_D6} // Polaris
  FCompList: IDesignerSelections;
  {$ELSE}
  FCompList: TDesignerSelectionList;
  {$ENDIF}
  Sect: TSpeedbarSection;
begin
  if CheckSpeedBar and Active then
  begin
    {$IFDEF RX_D6} // Polaris
    FCompList := CreateSelectionlist;
    {$ELSE}
    FCompList := TDesignerSelectionList.Create;
    {$ENDIF}
    if not SelectBar then
    begin
      if (ActiveControl = SectionList) or (ActiveControl = SectionName) then
      begin
        Sect := SectionByRow(Section);
        if Sect <> nil then FCompList.Add(Sect);
      end;
      if (FCompList.Count = 0) and (Item <> nil) then FCompList.Add(Item);
    end;
    if (FBar <> nil) and (FCompList.Count = 0) then FCompList.Add(FBar);
    SetSelection(FCompList);
  end;
end;

{$IFNDEF RX_D6} // Polaris
{$IFDEF RX_D3}

procedure TSpeedbarEditor.FormClosed(Form: TCustomForm);
{$ELSE}

procedure TSpeedbarEditor.FormClosed(Form: TForm);
{$ENDIF}
begin
  if Form = OwnerForm then Free;
end;

procedure TSpeedbarEditor.FormModified;
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;
{$ENDIF}

procedure TSpeedbarEditor.Activated;
begin
  SelectButton(CurrentSection, ItemByRow(ButtonsList.Row), False);
  PasteMenu.Enabled := CheckSpeedBar and (FBar.SectionCount > 0) and
    ClipboardComponents;
end;

function TSpeedbarEditor.ConfirmDelete: Boolean;
begin
  Result := MessageDlg(RxLoadStr(srConfirmSBDelete), mtWarning, mbYesNoCancel, 0) = mrYes;
end;

procedure TSpeedbarEditor.SaveSelection;
begin
  with FSelectData do
  begin
    bRowCount := ButtonsList.RowCount;
    bRow := ButtonsList.Row;
    sRowCount := SectionList.RowCount;
    sRow := SectionList.Row;
  end;
end;

procedure TSpeedbarEditor.RestoreSelection;
var
  NewSRow, NewBRow: Integer;
begin
  NewSRow := FSelectData.sRow;
  if (SectionList.RowCount > FSelectData.sRowCount) or
    (NewSRow > SectionList.RowCount - 1) then
    NewSRow := SectionList.RowCount - 1;
  if NewSRow < 0 then NewSRow := 0;
  SectionList.Row := NewSRow;
  SetSection(SectionList.Row); { set ButtonsList to current section }
  NewBRow := FSelectData.bRow;
  if (ButtonsList.RowCount > FSelectData.bRowCount) or
    (NewBRow > ButtonsList.RowCount - 1) then
    NewBRow := ButtonsList.RowCount - 1;
  if NewBRow < 0 then NewBRow := 0;
  ButtonsList.Row := NewBRow;
end;

procedure TSpeedbarEditor.UpdateEnabled(BtnRow, Section: Integer);
var
  EnableSect, EnableBtn: Boolean;
begin
  EnableSect := CheckSpeedBar and (FBar.SectionCount > 0);
  EnableBtn := EnableSect and (BtnRow >= 0) and (ItemBySectionRow(Section,
    BtnRow) <> nil);
  DelSection.Enabled := EnableSect;
  SectionName.Enabled := EnableSect;
  AddButton.Enabled := EnableSect;
  RemoveButton.Enabled := EnableBtn;
  CopyMenu.Enabled := EnableBtn;
  CutMenu.Enabled := EnableBtn;
  PasteMenu.Enabled := EnableSect and ClipboardComponents;
  UpBtn.Enabled := EnableBtn and (BtnRow > 0);
  DownBtn.Enabled := EnableBtn and (BtnRow < ButtonsList.RowCount - 1);
end;

function TSpeedbarEditor.CheckSpeedBar: Boolean;
begin
  Result := (FBar <> nil) and (FBar.Owner <> nil) and (FBar.Parent <> nil)
    and (Designer.{$IFDEF RX_D6}Root{$ELSE}Form{$ENDIF} <> nil); // Polaris
end;

function TSpeedbarEditor.CurrentSection: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then
    Result := SectionList.Row
  else
    Result := -1;
end;

procedure TSpeedbarEditor.SetSection(Section: Integer);
var
  I: Integer;
begin
  if CheckSpeedBar then
  begin
    I := Section;
    if (I >= 0) and (I < FBar.SectionCount) then
    begin
      SectionName.Text := TSpeedbarSection(FBar.Sections[I]).Caption;
      ButtonsList.RowCount := FBar.ItemsCount(I);
    end
    else
    begin
      SectionName.Text := '';
      ButtonsList.RowCount := 0;
    end;
    SectionList.DefaultColWidth := SectionList.ClientWidth;
    ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
  end;
end;

procedure TSpeedbarEditor.UpdateData;
begin
  Inc(FLocked);
  try
    SaveSelection;
    if CheckSpeedBar then
      SectionList.RowCount := FBar.SectionCount
    else
      SectionList.RowCount := 0;
    RestoreSelection; { set section }
  finally
    Dec(FLocked);
  end;
  UpdateEnabled(ButtonsList.Row, SectionList.Row);
  SelectButton(CurrentSection, ItemByRow(ButtonsList.Row), False);
end;

function TSpeedbarEditor.GetForm: TCustomForm;
begin
  {$IFDEF RX_D6} // Polaris
  Result := TCustomForm(Designer.Root);
  {$ELSE}
  Result := Designer.Form; { GetParentForm(FBar) }
  {$ENDIF}
end;

procedure TSpeedbarEditor.UpdateListHeight;
var
  Cnt: Integer;
  MaxHeight: Integer;
begin
  Canvas.Font := Font;
  MaxHeight := MulDiv(MaxBtnListHeight, Screen.PixelsPerInch, 96);
  ButtonsList.DefaultRowHeight := FBar.BtnHeight + 2;
  Cnt := Max(1, Max(ButtonsList.ClientHeight, MaxHeight) div
    (FBar.BtnHeight + 2));
  ButtonsList.ClientHeight := Min(ButtonsList.DefaultRowHeight * Cnt,
    MaxHeight);
  SectionList.DefaultRowHeight := Canvas.TextHeight('Wg') + 2;
end;

procedure TSpeedbarEditor.SetSpeedBar(Value: TSpeedBar);
var
  I: Integer;
begin
  if FBar <> Value then
  begin
    if FBar <> nil then FBar.SetEditing(0);
    FBar := Value;
    if FBar <> nil then FBar.SetEditing(Handle);
    Inc(FLocked);
    try
      if FBar <> nil then UpdateListHeight;
      if FBar.SectionCount = 0 then
        NewSectionClick(Self)
      else
        for I := 0 to FBar.SectionCount - 1 do
        begin
          if FBar.Sections[I].Name = '' then
          begin
            FBar.Sections[I].Name := UniqueName(FBar.Sections[I]);
            Designer.Modified;
          end;
        end;
      if ButtonsList.RowCount > 0 then
        ActiveControl := ButtonsList
      else
        ActiveControl := SectionList;
      UpdateData;
      ButtonsList.Row := 0;
    finally
      Dec(FLocked);
    end;
    SectionList.Row := 0;
  end;
end;

procedure TSpeedbarEditor.CMSpeedBarChanged(var Message: TMessage);
begin
  if Pointer(Message.LParam) = FBar then
  begin
    case Message.WParam of
      SBR_CHANGED: Designer.Modified;
      SBR_DESTROYED: Close;
      SBR_BTNSIZECHANGED: if FBar <> nil then UpdateListHeight;
    end;
  end
  else if (Message.WParam = SBR_BTNSELECT) and CheckSpeedBar then
  begin
    SelectButton(-1, nil, True);
    Designer.Modified;
  end;
end;

function TSpeedbarEditor.ItemBySectionRow(Section, Row: Integer): TSpeedItem;
begin
  if CheckSpeedBar then
    Result := FBar.Items(Section, Row)
  else
    Result := nil;
end;

function TSpeedbarEditor.SectionByRow(Row: Integer): TSpeedbarSection;
begin
  if CheckSpeedBar and (Row >= 0) and (Row < FBar.SectionCount) then
    Result := FBar.Sections[Row]
  else
    Result := nil;
end;

function TSpeedbarEditor.ItemByRow(Row: Integer): TSpeedItem;
begin
  Result := ItemBySectionRow(CurrentSection, Row);
end;

procedure TSpeedbarEditor.NewSectionClick(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  if CheckSpeedBar then
  begin
    I := 0;
    repeat
      S := Format(RxLoadStr(srNewSectionName), [I]);
      Inc(I);
    until FBar.SearchSection(S) < 0;
    I := NewSpeedSection(FBar, S);
    if I >= 0 then FBar.Sections[I].Name := UniqueName(FBar.Sections[I]);
    ActiveControl := SectionName;
    Designer.Modified;
  end;
end;

procedure TSpeedbarEditor.DelSectionClick(Sender: TObject);
var
  Sect: Integer;
  Item: TSpeedItem;
begin
  if CheckSpeedBar and ConfirmDelete then
  begin
    Sect := SectionList.Row;
    if (Sect >= 0) and (Sect < FBar.SectionCount) then
    begin
      {$IFDEF RX_D6} // Polaris
      Self.ValidateRename(FBar.Sections[Sect],
        FBar.Sections[Sect].Name, '');
      {$ELSE}
      Designer.ValidateRename(FBar.Sections[Sect],
        FBar.Sections[Sect].Name, '');
      {$ENDIF}
      try
        while FBar.ItemsCount(Sect) > 0 do
        begin
          Item := FBar.Items(Sect, 0);
          if Item <> nil then
          begin
            OwnerForm.RemoveComponent(Item);
            Item.Free;
          end;
        end;
        FBar.RemoveSection(Sect);
      finally
        Designer.Modified;
      end;
    end;
  end;
end;

procedure TSpeedbarEditor.Copy;
var
  {$IFDEF RX_D6} // Polaris
  CompList: IDesignerSelections;
  {$ELSE}
  CompList: TDesignerSelectionList;
  {$ENDIF}
  Item: TSpeedItem;
begin
  {$IFDEF RX_D6} // Polaris
  CompList := CreateSelectionlist;
  {$ELSE}
  CompList := TDesignerSelectionList.Create;
  {$ENDIF}
  try
    Item := ItemByRow(ButtonsList.Row);
    if Item <> nil then
    begin
      Item.InvalidateItem;
      CompList.Add(Item);
      CopyComponents(OwnerForm, CompList);
      Item.UpdateSection;
    end;
  finally
    {$IFNDEF RX_D6} // Polaris
    CompList.Free;
    {$ENDIF}
  end;
end;

procedure TSpeedbarEditor.Paste;
var
  {$IFDEF RX_D6} // Polaris
  CompList: IDesignerSelections;
  {$ELSE}
  CompList: TDesignerSelectionList;
  {$ENDIF}
begin
  if CheckSpeedBar then
  begin
    {$IFDEF RX_D6} // Polaris
    CompList := CreateSelectionlist;
    {$ELSE}
    CompList := TDesignerSelectionList.Create;
    {$ENDIF}
    try
      FBar.OnAddItem := OnPasteItem;
      try
        PasteComponents(OwnerForm, FBar, CompList);
      finally
        FBar.OnAddItem := nil;
      end;
      UpdateData;
    finally
      {$IFNDEF RX_D6} // Polaris
      CompList.Free;
      {$ENDIF}
    end;
  end;
end;

procedure TSpeedbarEditor.Cut;
begin
  Copy;
  RemoveButtonClick(Self);
end;

procedure TSpeedbarEditor.OnPasteItem(Item: TObject);
begin
  if (Item <> nil) then
  begin
    if CheckSpeedBar and (Item is TSpeedItem) then
    begin
      TSpeedItem(Item).ASection := CurrentSection;
      TSpeedItem(Item).Visible := False;
    end
  end;
end;

procedure TSpeedbarEditor.AddButtonClick(Sender: TObject);
var
  I: Integer;
  Item: TSpeedItem;
begin
  I := CurrentSection;
  if I < 0 then Exit;
  Item := TSpeedItem.Create(OwnerForm);
  if Item <> nil then
  try
    FBar.AddItem(I, Item);
    Item.Name := UniqueName(Item);
    Designer.Modified;
    if (Sender <> nil) then ActivateInspector(#0);
  except
    Item.Free;
    raise;
  end
  else
    raise ESpeedbarError.Create(RxLoadStr(srSBItemNotCreate));
end;

procedure TSpeedbarEditor.RemoveButtonClick(Sender: TObject);
var
  Item: TSpeedItem;
begin
  Item := ItemByRow(ButtonsList.Row);
  if Item <> nil then
  begin
    {$IFDEF RX_D6} // Polaris
    Self.ValidateRename(Item, Item.Name, '');
    {$ELSE}
    Designer.ValidateRename(Item, Item.Name, '');
    {$ENDIF}
    OwnerForm.RemoveComponent(Item);
    Item.Free;
    Designer.Modified;
  end;
end;

procedure TSpeedbarEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TSpeedbarEditor.UpBtnClick(Sender: TObject);
var
  I, Sect: Integer;
begin
  if CheckSpeedBar and FBar.FindItem(ItemByRow(ButtonsList.Row), Sect, I) then
  begin
    if I > 0 then
    begin
      FBar.Sections[Sect].List.Move(I, I - 1);
      Designer.Modified;
      ButtonsList.Invalidate;
      ButtonsList.Row := ButtonsList.Row - 1;
    end;
  end;
end;

procedure TSpeedbarEditor.DownBtnClick(Sender: TObject);
var
  I, Sect: Integer;
begin
  if CheckSpeedBar and FBar.FindItem(ItemByRow(ButtonsList.Row), Sect, I) then
  begin
    if I < FBar.ItemsCount(Sect) - 1 then
    begin
      FBar.Sections[Sect].List.Move(I, I + 1);
      Designer.Modified;
      ButtonsList.Invalidate;
      ButtonsList.Row := ButtonsList.Row + 1;
    end;
  end;
end;

procedure TSpeedbarEditor.CopyMenuClick(Sender: TObject);
begin
  Copy;
end;

procedure TSpeedbarEditor.PasteMenuClick(Sender: TObject);
begin
  Paste;
end;

procedure TSpeedbarEditor.CutMenuClick(Sender: TObject);
begin
  Cut;
end;

procedure TSpeedbarEditor.SectionNameExit(Sender: TObject);
var
  I: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then
  begin
    I := CurrentSection;
    if I >= 0 then
    begin
      FBar.Sections[I].Caption := SectionName.Text;
      Designer.Modified;
    end;
  end;
end;

procedure TSpeedbarEditor.SectionListSelectCell(Sender: TObject; Col,
  Row: LongInt; var CanSelect: Boolean);
begin
  CanSelect := False;
  if CheckSpeedBar and (Row < FBar.SectionCount) and (Row >= 0) then
  begin
    if FLocked = 0 then
    begin
      SetSection(Row);
      UpdateEnabled(ButtonsList.Row, Row);
      ButtonsList.Invalidate;
      SelectButton(Row, ItemBySectionRow(Row, ButtonsList.Row), False);
    end;
    CanSelect := True;
  end;
end;

procedure TSpeedbarEditor.SectionListDrawCell(Sender: TObject; Col,
  Row: LongInt; Rect: TRect; State: TGridDrawState);
begin
  if CheckSpeedBar then
  begin
    if (Row < FBar.SectionCount) and (Row >= 0) then
    begin
      DrawCellText(Sender as TDrawGrid, Col, Row,
        FBar.Sections[Row].Caption, Rect, taLeftJustify, vaCenter
        {$IFDEF RX_D4}, TDrawGrid(Sender).IsRightToLeft{$ENDIF});
    end;
  end;
end;

procedure TSpeedbarEditor.SectionListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: if SectionByRow(SectionList.Row) <> nil then ActivateInspector(#0);
    VK_DELETE: DelSectionClick(Self);
    VK_INSERT, VK_ADD: NewSectionClick(Self);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TSpeedbarEditor.ButtonsListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN: if ItemByRow(ButtonsList.Row) <> nil then ActivateInspector(#0);
    VK_DELETE: RemoveButtonClick(Self);
    VK_INSERT, VK_ADD: AddButtonClick(Self);
  else
    Exit;
  end;
  Key := 0;
end;

procedure TSpeedbarEditor.ButtonsListDblClick(Sender: TObject);
type
  PParamData = ^TParamData;
  TParamData = record
    Flags: TParamFlags;
    ParamNameAndType: array[0..100] of Char;
  end;
const
  {$IFDEF CBUILDER}
  sSender: string[7] = '*Sender';
  {$ELSE}
  sSender: string[6] = 'Sender';
  {$ENDIF}
  sObject: string[7] = 'TObject';
var
  Btn: TSpeedItem;
  I, Num: Integer;
  MethodName: string;
  Method: TMethod;
  TypeData: PTypeData;
  ParamData: PParamData;
  PropInfo: PPropInfo;
  Candidates: TPropInfoList;
begin
  Btn := ItemByRow(ButtonsList.Row);
  if Btn = nil then Exit;
  Candidates := TPropInfoList.Create(Btn, [tkMethod]);
  try
    for I := Candidates.Count - 1 downto 0 do
    begin
      PropInfo := Candidates[I];
      if CompareText(string(PropInfo^.Name), 'OnClick') = 0 then
      begin
        Method := GetMethodProp(Btn, PropInfo);
        MethodName := TFormDesigner(Designer).GetMethodName(Method);
        if MethodName = '' then
        begin
          MethodName := Btn.Name + 'Click';
          Num := 0;
          while TFormDesigner(Designer).MethodExists(MethodName) do
          begin
            MethodName := Btn.Name + 'Click' + IntToStr(Num);
            Inc(Num);
          end;
          TypeData := AllocMem(SizeOf(TTypeData));
          try
            TypeData^.MethodKind := mkProcedure;
            TypeData^.ParamCount := 1;
            ParamData := PParamData(@TypeData^.ParamList);
            with ParamData^ do
            begin
              Flags := [];
              ParamNameAndType[0] := Char(Length(sSender));
              Move(sSender[1], ParamNameAndType[1], Length(sSender));
              ParamNameAndType[Length(sSender) + 1] := Char(Length(sObject));
              Move(sObject[1], ParamNameAndType[Length(sSender) + 2],
                Length(sObject));
            end;
            Method := TFormDesigner(Designer).CreateMethod(MethodName, TypeData);
            Method.Data := OwnerForm;
          finally
            FreeMem(TypeData, SizeOf(TTypeData));
          end;
          Btn.OnClick := TNotifyEvent(Method);
          Designer.Modified;
        end;
        if (MethodName <> '') and TFormDesigner(Designer).MethodExists(MethodName) then
          TFormDesigner(Designer).ShowMethod(MethodName);
        Break;
      end;
    end;
  finally
    Candidates.Free;
  end;
end;

procedure TSpeedbarEditor.ButtonsListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TSpeedItem;
begin
  if (X < FBar.BtnWidth + 2) and (Button = mbLeft) then
  begin
    Item := ItemByRow(ButtonsList.Row);
    if Item <> nil then
    begin
      FDrag := True;
      if Item.Visible then
        FDragItem := nil
      else
      begin
        FDragItem := Item;
        if FButton = nil then
        begin
          FButton := TBtnControl.Create(Self);
          TBtnControl(FButton).AssignSpeedItem(Item);
        end;
      end;
    end;
  end;
end;

procedure TSpeedbarEditor.ButtonsListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (FButton <> nil) and (FDragItem <> nil) then
  begin
    P := (Sender as TControl).ClientToScreen(Point(X, Y));
    X := P.X - (FButton.Width {div 2});
    Y := P.Y - (FButton.Height {div 2});
    FButton.Activate(Bounds(X, Y, FBar.BtnWidth, FBar.BtnHeight));
  end
  else if FDrag then
    SetCursor(Screen.Cursors[crNoDrop]);
end;

procedure TSpeedbarEditor.ButtonsListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (Button = mbLeft) then
  try
    if (FDragItem <> nil) and (FButton <> nil) then
    begin
      Dec(X, FButton.Width {div 2});
      Dec(Y, FButton.Height {div 2});
      P := (Sender as TControl).ClientToScreen(Point(X, Y));
      FButton.Free;
      FButton := nil;
      if CheckSpeedBar and (FBar = FindSpeedBar(P)) then
      begin
        P := FBar.ScreenToClient(P);
        if FBar.AcceptDropItem(FDragItem, P.X, P.Y) then
        begin
          Designer.Modified;
        end;
      end;
    end
    else
      SetCursor(Screen.Cursors[ButtonsList.Cursor]);
  finally
    FDrag := False;
    FDragItem := nil;
  end;
end;

procedure TSpeedbarEditor.ButtonsListSelectCell(Sender: TObject; Col,
  Row: LongInt; var CanSelect: Boolean);
var
  Item: TSpeedItem;
begin
  Item := ItemByRow(Row);
  CanSelect := not FDrag and (Item <> nil);
  if FLocked = 0 then
  begin
    if CanSelect then
    begin
      UpdateEnabled(Row, SectionList.Row);
      SelectButton(CurrentSection, Item, False);
    end
    else if not FDrag then
    begin
      UpdateEnabled(-1, SectionList.Row);
      SelectButton(-1, nil, True);
    end;
  end;
end;

procedure TSpeedbarEditor.FormCreate(Sender: TObject);
begin
  FImage := TButtonImage.Create;
  FButton := nil;
  FBar := nil;
  FDrag := False;
  if NewStyleControls then Font.Style := [];
  {$IFNDEF VER80}
  with FormPlacement1 do
  begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
  {$ENDIF}
end;

procedure TSpeedbarEditor.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TSpeedbarEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FButton.Free;
  FButton := nil;
  if FBar <> nil then
  begin
    FBar.SetEditing(0);
    SelectButton(-1, nil, True);
    FBar.Invalidate;
  end;
  FBar := nil;
end;

procedure TSpeedbarEditor.SectionNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = (VK_RETURN) then
  begin
    SectionNameExit(SectionName);
    Key := 0;
    ActiveControl := SectionList;
  end;
end;

procedure TSpeedbarEditor.ButtonsListDrawCell(Sender: TObject; Col,
  Row: LongInt; Rect: TRect; State: TGridDrawState);
var
  I: Integer;
begin
  I := CurrentSection;
  if (I >= 0) and (Row < FBar.ItemsCount(I)) then
    DrawCellButton(Sender as TDrawGrid, Rect, ItemByRow(Row), FImage
      {$IFDEF RX_D4}, TDrawGrid(Sender).IsRightToLeft{$ENDIF});
end;

procedure TSpeedbarEditor.SectionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: LongInt;
begin
  if (Button = mbLeft) then
    with (Sender as TDrawGrid) do
    begin
      MouseToCell(X, Y, ACol, ARow);
      Tag := Row;
      BeginDrag(False);
    end;
end;

procedure TSpeedbarEditor.SectionListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Col, Row: LongInt;
begin
  try
    (Sender as TDrawGrid).MouseToCell(X, Y, Col, Row);
    FBar.Sections[(Sender as TDrawGrid).Tag].Index := Row;
    Designer.Modified;
    UpdateData;
    SectionList.Row := Row;
  finally
    (Sender as TDrawGrid).Tag := 0;
  end;
end;

procedure TSpeedbarEditor.SectionListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Col, Row: LongInt;
begin
  (Sender as TDrawGrid).MouseToCell(X, Y, Col, Row);
  Accept := (Row >= 0) and (Row <> (Sender as TDrawGrid).Tag);
end;

procedure TSpeedbarEditor.FormShow(Sender: TObject);
begin
  if FBar <> nil then UpdateListHeight;
  SectionList.DefaultColWidth := SectionList.ClientWidth;
  ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
end;

end.
