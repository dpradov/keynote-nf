{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit TimLstEd;

interface

{$I RX.INC}

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} SysUtils,
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Grids, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Menus, RXCtrls, VCLUtils, Placemnt,
  TimerLst, DesignWindows;

type
  TTimerItemsEditor = class(TDesignWindow)
    BtnPanel: TPanel;
    ClientPanel: TPanel;
    NewBtn: TButton;
    DeleteBtn: TButton;
    DrawGrid: TDrawGrid;
    PopupMenu: TPopupMenu;
    CutMenu: TMenuItem;
    CopyMenu: TMenuItem;
    PasteMenu: TMenuItem;
    FormStorage: TFormPlacement;
    DeleteMenu: TMenuItem;
    N1: TMenuItem;
    NewMenu: TMenuItem;
    ClearBtn: TButton;
    Panel1: TPanel;
    CloseBtn: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DrawGridDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGridSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure CloseBtnClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure PasteClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FTimersCollection: TRxTimerList;
    function GetForm: TCustomForm;
    function CheckCollection: Boolean;
    function ItemByRow(Row: Integer): TRxTimerEvent;
    procedure SelectItem(Item: TRxTimerEvent);
    procedure UpdateData;
    procedure SetTimersCollection(Value: TRxTimerList);
    procedure Copy;
    procedure Cut;
    procedure Paste;
  protected
    function UniqueName(Component: TComponent): string; override;
    procedure Activated; override;
  public
    function EditAction(Action: TEditAction):Boolean; override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    function GetEditState: TEditState; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    property TimersCollection: TRxTimerList read FTimersCollection
      write SetTimersCollection;
    property OwnerForm: TCustomForm read GetForm;
  end;

{ TTimersItemListProperty }

  TTimersItemListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

{ TTimersCollectionEditor }

  TTimersCollectionEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses Consts, {$IFDEF WIN32} RxConst, {$ENDIF} RxLConst, RxDsgn;

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

{$IFDEF RX_D4}
type
  TDesigner = IDesigner;
  TFormDesigner = IDesigner;
{$ENDIF}

function FindEditor(ATimersCollection: TRxTimerList): TTimerItemsEditor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TTimerItemsEditor then begin
      if TTimerItemsEditor(Screen.Forms[I]).TimersCollection = ATimersCollection then
      begin
        Result := TTimerItemsEditor(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowItemsEditor(Designer: TDesigner;
  ATimersCollection: TRxTimerList);
var
  Editor: TTimerItemsEditor;
begin
  if ATimersCollection = nil then Exit;
  Editor := FindEditor(ATimersCollection);
  if Editor = nil then begin
    Editor := TTimerItemsEditor.Create(Application);
    try
      Editor.Designer := TFormDesigner(Designer);
      Editor.TimersCollection := ATimersCollection;
      Editor.Show;
    except
      Editor.Free;
      raise;
    end;
  end
  else begin
    Editor.Show;
    if Editor.WindowState = wsMinimized then
      Editor.WindowState := wsNormal;
  end;
end;

{ TTimersItemListProperty }

function TTimersItemListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TTimersItemListProperty.GetValue: string;
var
  List: TList;
begin
  List := TList(Pointer(GetOrdValue));
  if (List = nil) or (List.Count = 0) then
    Result := ResStr(srNone)
  else FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TTimersItemListProperty.Edit;
begin
  ShowItemsEditor(Designer, TRxTimerList(GetComponent(0)));
end;

{ TTimersCollectionEditor }

procedure TTimersCollectionEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowItemsEditor(Designer, TRxTimerList(Component));
  end;
end;

function TTimersCollectionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := LoadStr(srTimerDesigner);
  end;
end;

function TTimersCollectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TTimerItemsEditor }

procedure TTimerItemsEditor.SetTimersCollection(Value: TRxTimerList);
begin
  if FTimersCollection <> Value then begin
    FTimersCollection := Value;
    UpdateData;
  end;
end;

function TTimerItemsEditor.UniqueName(Component: TComponent): string;
var
  Temp: string;
{$IFNDEF WIN32}
  I: Integer;
  Comp: TComponent;
{$ENDIF}
begin
  if (Component <> nil) then Temp := Component.ClassName
  else Temp := TRxTimerEvent.ClassName;
  if (UpCase(Temp[1]) = 'T') and (Length(Temp) > 1) then
    System.Delete(Temp, 1, 1);
{$IFDEF WIN32}
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

function TTimerItemsEditor.GetEditState: TEditState;
begin
  Result := [];
  if DeleteBtn.Enabled then Result := [esCanDelete, esCanCut, esCanCopy];
  if ClipboardComponents then Include(Result, esCanPaste);
end;

procedure TTimerItemsEditor.DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean);
begin
  if Designer.Root = OwnerForm then Free;
end;

procedure TTimerItemsEditor.ItemsModified(const Designer: IDesigner);
begin
  if not (csDestroying in ComponentState) then UpdateData;
end;

procedure TTimerItemsEditor.Activated;
begin
  SelectItem(ItemByRow(DrawGrid.Row - 1));
end;

procedure TTimerItemsEditor.UpdateData;
var
  Empty: Boolean;
begin
  if CheckCollection then begin
    Caption := Format(LoadStr(srTimerEvents), [TimersCollection.Name]);
    Empty := TimersCollection.Count = 0;
  end
  else Empty := True;
  if Empty then begin
    DrawGrid.RowCount := 2;
    SelectItem(nil);
  end
  else DrawGrid.RowCount := TimersCollection.Count + 1;
  DeleteBtn.Enabled := not Empty;
  ClearBtn.Enabled := not Empty;
  DeleteMenu.Enabled := not Empty;
  CopyMenu.Enabled := not Empty;
  CutMenu.Enabled := not Empty;
  PasteMenu.Enabled := ClipboardComponents;
  DrawGrid.Invalidate;
end;

function TTimerItemsEditor.GetForm: TCustomForm;
begin
  Result := GetParentForm(ClientPanel); //Designer.Form;
end;

procedure TTimerItemsEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

function TTimerItemsEditor.CheckCollection: Boolean;
begin
  Result := (TimersCollection <> nil) and (TimersCollection.Owner <> nil)
    and (Designer.Root <> nil);
end;

procedure TTimerItemsEditor.SelectItem(Item: TRxTimerEvent);
var
  FComponents: IDesignerSelections;
begin
  if CheckCollection and Active then begin
    FComponents := CreateSelectionList;
    if Item <> nil then FComponents.Add(Item)
    else FComponents.Add(TimersCollection);
    SetSelection(FComponents);
  end;
end;

function TTimerItemsEditor.ItemByRow(Row: Integer): TRxTimerEvent;
begin
  Result := nil;
  if CheckCollection and (Row >= 0) and
    (Row < TimersCollection.Count) then
  begin
    Result := TRxTimerEvent(TimersCollection.Events[Row]);
  end;
end;

procedure TTimerItemsEditor.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  if Item = TimersCollection then begin
    TimersCollection := nil;
    Close;
  end;
end;

procedure TTimerItemsEditor.DrawGridDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  CellText: string;
  Item: TRxTimerEvent;
begin
  CellText := '';
  if gdFixed in State then CellText := 'Item name'
  else begin
    Item := ItemByRow(Row - 1);
    if Item <> nil then CellText := Item.Name;
  end;
  DrawCellText(DrawGrid, Col, Row, CellText, Rect, taLeftJustify, vaCenter);
end;

procedure TTimerItemsEditor.DrawGridSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  SelectItem(ItemByRow(Row - 1));
end;

procedure TTimerItemsEditor.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TTimerItemsEditor.DeleteClick(Sender: TObject);
var
  Item: TRxTimerEvent;
begin
  Item := ItemByRow(DrawGrid.Row - 1);
  if Item <> nil then begin
    Self.ValidateRename(Item, Item.Name, '');
    TimersCollection.Delete(Item.Handle);
    if TimersCollection.Count > 0 then begin
      Item := ItemByRow(DrawGrid.Row - 1);
      SelectItem(Item);
    end
    else SelectItem(nil);
    UpdateData;
    Designer.Modified;
  end;
end;

procedure TTimerItemsEditor.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_RETURN: if ItemByRow(DrawGrid.Row - 1) <> nil then ActivateInspector(#0);
      VK_DELETE: DeleteClick(nil);
    end;
end;

procedure TTimerItemsEditor.FormCreate(Sender: TObject);
begin
  TimersCollection := nil;
  if NewStyleControls then Font.Style := [];
{$IFDEF WIN32}
  with FormStorage do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
{$ENDIF}
end;

procedure TTimerItemsEditor.FormResize(Sender: TObject);
begin
  with DrawGrid do ColWidths[0] := ClientWidth;
end;

function TTimerItemsEditor.EditAction(Action: TEditAction) : Boolean;
begin
  Result := True;
  case Action of
    eaCut: Cut;
    eaCopy: Copy;
    eaPaste: Paste;
    eaDelete: DeleteClick(Self);
  end;
end;

procedure TTimerItemsEditor.NewClick(Sender: TObject);
var
  I: Integer;
  Item: TRxTimerEvent;
begin
  Item := TRxTimerEvent.Create(TimersCollection.Owner);
  if Item <> nil then
    try
      Item.Name := UniqueName(Item);
      with TimersCollection do
        I := ItemIndexByHandle(AddItem(Item));
      SelectItem(Item);
      Designer.Modified;
      ActivateInspector(#0);
      DrawGrid.Row := I + 1;
    except
      Item.Free;
      raise;
    end
  else raise Exception.CreateRes(srEventNotCreate);
end;

procedure TTimerItemsEditor.CutClick(Sender: TObject);
begin
  Cut;
  UpdateData;
end;

procedure TTimerItemsEditor.CopyClick(Sender: TObject);
begin
  Copy;
  UpdateData;
end;

procedure TTimerItemsEditor.PasteClick(Sender: TObject);
begin
  Paste;
  UpdateData;
end;

procedure TTimerItemsEditor.Cut;
begin
  Copy;
  DeleteClick(Self);
end;

procedure TTimerItemsEditor.Copy;
var
  CompList: IDesignerSelections;
  Item: TRxTimerEvent;
begin
  CompList := CreateSelectionList;
  try
    Item := ItemByRow(DrawGrid.Row - 1);
    if Item <> nil then begin
      CompList.Add(Item);
      CopyComponents(OwnerForm, CompList);
    end;
  finally
    //CompList.Free;
  end;
end;

procedure TTimerItemsEditor.Paste;
var
  CompList: IDesignerSelections;
begin
  if CheckCollection then begin
    CompList := CreateSelectionList;
    try
      PasteComponents(OwnerForm, TimersCollection, CompList);
      UpdateData;
    finally
      //CompList.Free;
    end;
  end;
end;

procedure TTimerItemsEditor.ClearBtnClick(Sender: TObject);
var
  Item: TRxTimerEvent;
begin
  while TimersCollection.Events.Count > 0 do begin
    Item := TRxTimerEvent(TimersCollection.Events[0]);
    if Item <> nil then Self.ValidateRename(Item, Item.Name, '');
    TimersCollection.Events.Delete(0);
    Item.Free;
  end;
  UpdateData;
end;

end.
