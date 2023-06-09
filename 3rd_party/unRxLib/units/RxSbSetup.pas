{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxSbSetup;

interface

{$I RX.INC}

uses
{$IFNDEF VER80}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF RX_D6}Types,{$ENDIF}
  StdCtrls, Buttons, Grids, RxCtrls, RxSpeedbar, ExtCtrls, RxConst;

type
  TSpeedbarSetupWindow = class(TForm)
    ButtonsList: TDrawGrid;
    ButtonsLabel: TLabel;
    SectionList: TDrawGrid;
    CategoriesLabel: TLabel;
    Bevel1: TBevel;
    HintLabel: TLabel;
    CloseBtn: TButton;
    HelpBtn: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SectionListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure SectionListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure ButtonsListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ButtonsListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonsListSelectCell(Sender: TObject; Col, Row: Longint;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonsListDrawCell(Sender: TObject; Col, Row: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure CloseBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FButton: TBtnControl;
    FImage: TButtonImage;
    FBar: TSpeedBar;
    FDrag: Boolean;
    FDragItem: TSpeedItem;
    procedure UpdateHint(Section, Row: Integer);
    function CheckSpeedBar: Boolean;
    function CurrentSection: Integer;
    procedure SetSection(Section: Integer);
    procedure UpdateCurrentSection;
    procedure UpdateData(Section: Integer);
    procedure UpdateListHeight;
    procedure SetSpeedBar(Value: TSpeedBar);
    function ItemByRow(Row: Integer): TSpeedItem;
    procedure CMSpeedBarChanged(var Message: TMessage); message CM_SPEEDBARCHANGED;
  public
    { Public declarations }
    property SpeedBar: TSpeedBar read FBar write SetSpeedBar;
  end;

procedure ShowSpeedbarSetupWindow(Speedbar: TSpeedbar; HelpCtx: THelpContext);

implementation

uses RxVCLUtils, RxMaxMin, Consts, RXResConst;

{$R *.DFM}

function FindEditor(Speedbar: TSpeedbar): TSpeedbarSetupWindow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do begin
    if Screen.Forms[I] is TSpeedbarSetupWindow then begin
      if TSpeedbarSetupWindow(Screen.Forms[I]).SpeedBar = SpeedBar then
      begin
        Result := TSpeedbarSetupWindow(Screen.Forms[I]);
        Break;
      end;
    end;
  end;
end;

procedure ShowSpeedbarSetupWindow(Speedbar: TSpeedbar; HelpCtx: THelpContext);
var
  Editor: TSpeedbarSetupWindow;
begin
  if Speedbar = nil then Exit;
  Editor := FindEditor(Speedbar);
  if Editor = nil then begin
    Editor := TSpeedbarSetupWindow.Create(Application);
    Editor.Speedbar := Speedbar;
  end;
  try
    if HelpCtx > 0 then Editor.HelpContext := HelpCtx;
{$IFNDEF VER80}
    Editor.BorderIcons := [biSystemMenu];
{$ENDIF}
    Editor.HelpBtn.Visible := (HelpCtx > 0);
    Editor.Show;
    if Editor.WindowState = wsMinimized then Editor.WindowState := wsNormal;
  except
    Editor.Free;
    raise;
  end;
end;

{ TSpeedbarSetupWindow }

const
  MaxBtnListHeight = 186;

function TSpeedbarSetupWindow.CheckSpeedBar: Boolean;
begin
  Result := (FBar <> nil) and (FBar.Owner <> nil) and
    (FBar.Parent <> nil);
end;

function TSpeedbarSetupWindow.CurrentSection: Integer;
begin
  if CheckSpeedBar and (FBar.SectionCount > 0) then
    Result := SectionList.Row
  else Result := -1;
end;

procedure TSpeedbarSetupWindow.SetSection(Section: Integer);
var
  I: Integer;
begin
  if CheckSpeedBar then begin
    I := Section;
    if (I >= 0) and (FBar.SectionCount > 0) then
      ButtonsList.RowCount := FBar.ItemsCount(I)
    else ButtonsList.RowCount := 0;
    SectionList.DefaultColWidth := SectionList.ClientWidth;
    ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
    UpdateHint(I, ButtonsList.Row);
  end;
end;

procedure TSpeedbarSetupWindow.UpdateCurrentSection;
begin
  SetSection(CurrentSection);
end;

procedure TSpeedbarSetupWindow.UpdateData(Section: Integer);
begin
  if CheckSpeedBar then begin
    SectionList.RowCount := FBar.SectionCount;
    UpdateCurrentSection;
    if (Section >= 0) and (Section < SectionList.RowCount) then
      SectionList.Row := Section;
  end
  else begin
    SectionList.RowCount := 0;
    ButtonsList.RowCount := 0;
  end;
end;

procedure TSpeedbarSetupWindow.UpdateListHeight;
var
  Cnt: Integer;
  MaxHeight: Integer;
begin
  Canvas.Font := Font;
  MaxHeight := MulDiv(MaxBtnListHeight, Screen.PixelsPerInch, 96);
  ButtonsList.DefaultRowHeight := FBar.BtnHeight + 2;
  Cnt := Max(1, Max(ButtonsList.ClientHeight, MaxHeight) div
    (FBar.BtnHeight + 2));
  ButtonsList.ClientHeight := Min(MaxHeight,
    ButtonsList.DefaultRowHeight * Cnt);
  SectionList.ClientHeight := ButtonsList.ClientHeight;
  SectionList.DefaultRowHeight := Canvas.TextHeight('Wg') + 2;
end;

procedure TSpeedbarSetupWindow.SetSpeedBar(Value: TSpeedBar);
begin
  if FBar <> Value then begin
    if FBar <> nil then FBar.SetEditing(0);
    FBar := Value;
    if FBar <> nil then begin
      FBar.SetEditing(Handle);
      UpdateListHeight;
    end;
    UpdateData(-1);
  end;
end;

procedure TSpeedbarSetupWindow.CMSpeedBarChanged(var Message: TMessage);
begin
  if Pointer(Message.LParam) = FBar then begin
    case Message.WParam of
      SBR_CHANGED: UpdateData(CurrentSection);
      SBR_DESTROYED: Close;
      SBR_BTNSIZECHANGED: if FBar <> nil then UpdateListHeight;
    end;
  end;
end;

function TSpeedbarSetupWindow.ItemByRow(Row: Integer): TSpeedItem;
begin
  Result := FBar.Items(CurrentSection, Row);
end;

procedure TSpeedbarSetupWindow.UpdateHint(Section, Row: Integer);
var
  Item: TSpeedItem;
begin
  Item := FBar.Items(Section, Row);
  if Item <> nil then Hint := Item.Hint
  else Hint := '';
end;

procedure TSpeedbarSetupWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FButton.Free;
  FButton := nil;
  if FBar <> nil then FBar.SetEditing(0);
  FBar := nil;
end;

procedure TSpeedbarSetupWindow.SectionListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := False;
  SetSection(Row);
  CanSelect := True;
end;

procedure TSpeedbarSetupWindow.SectionListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
begin
  if CheckSpeedBar then begin
    if Row < FBar.SectionCount then begin
      DrawCellText(Sender as TDrawGrid, Col, Row,
        FBar.Sections[Row].Caption, Rect, taLeftJustify, vaCenter
        {$IFDEF RX_D4}, TDrawGrid(Sender).IsRightToLeft {$ENDIF});
    end;
  end;
end;

procedure TSpeedbarSetupWindow.ButtonsListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TSpeedItem;
begin
  Item := ItemByRow(ButtonsList.Row);
  if (Item <> nil) and (X < FBar.BtnWidth + 2) and (Button = mbLeft) then
  begin
    FDrag := True;
    if Item.Visible then FDragItem := nil
    else begin
      FDragItem := Item;
      if FButton = nil then begin
        FButton := TBtnControl.Create(Self);
        FButton.AssignSpeedItem(Item);
      end;
    end;
  end;
end;

procedure TSpeedbarSetupWindow.ButtonsListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (FButton <> nil) and (FDragItem <> nil) then begin
    P := (Sender as TControl).ClientToScreen(Point(X, Y));
    X := P.X - (FButton.Width {div 2});
    Y := P.Y - (FButton.Height {div 2});
    FButton.Activate(Bounds(X, Y, FBar.BtnWidth, FBar.BtnHeight));
  end
  else if FDrag then SetCursor(Screen.Cursors[crNoDrop]);
end;

procedure TSpeedbarSetupWindow.ButtonsListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if FDrag and (Button = mbLeft) then
  try
    if (FDragItem <> nil) and (FButton <> nil) then begin
      Dec(X, FButton.Width {div 2});
      Dec(Y, FButton.Height {div 2});
      P := (Sender as TControl).ClientToScreen(Point(X, Y));
      FButton.Free;
      FButton := nil;
      if CheckSpeedBar and (FBar = FindSpeedBar(P)) then begin
        P := FBar.ScreenToClient(P);
        if FBar.AcceptDropItem(FDragItem, P.X, P.Y) then
          UpdateCurrentSection;
      end;
    end
    else SetCursor(Screen.Cursors[ButtonsList.Cursor]);
  finally
    FDrag := False;
    FDragItem := nil;
  end;
end;

procedure TSpeedbarSetupWindow.ButtonsListSelectCell(Sender: TObject; Col,
  Row: Longint; var CanSelect: Boolean);
begin
  CanSelect := not FDrag or (Row = ButtonsList.Row);
  if CanSelect then UpdateHint(CurrentSection, Row)
  else Hint := '';
end;

procedure TSpeedbarSetupWindow.FormCreate(Sender: TObject);
begin
  FImage := TButtonImage.Create;
  FButton := nil;
  FBar := nil;
  FDrag := False;
  CloseBtn.Default := False;
  if NewStyleControls then Font.Style := [];
  { Load string resources }
  CloseBtn.Caption := ResStr(SOKButton);
  HelpBtn.Caption := ResStr(SHelpButton);
  Caption := RxLoadStr(SCustomizeSpeedbar);
  CategoriesLabel.Caption := RxLoadStr(SSpeedbarCategories);
  ButtonsLabel.Caption := RxLoadStr(SAvailButtons);
  HintLabel.Caption := RxLoadStr(SSpeedbarEditHint);
end;

procedure TSpeedbarSetupWindow.FormDestroy(Sender: TObject);
begin
  FImage.Free;
end;

procedure TSpeedbarSetupWindow.ButtonsListDrawCell(Sender: TObject; Col,
  Row: Longint; Rect: TRect; State: TGridDrawState);
var
  I: Integer;
begin
  I := CurrentSection;
  if (I >= 0) and (Row < FBar.ItemsCount(I)) then
    DrawCellButton(Sender as TDrawGrid, Rect, ItemByRow(Row), FImage
      {$IFDEF RX_D4}, TDrawGrid(Sender).IsRightToLeft {$ENDIF});
end;

procedure TSpeedbarSetupWindow.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TSpeedbarSetupWindow.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TSpeedbarSetupWindow.FormShow(Sender: TObject);
begin
  if FBar <> nil then UpdateListHeight;
  SectionList.DefaultColWidth := SectionList.ClientWidth;
  ButtonsList.DefaultColWidth := ButtonsList.ClientWidth;
end;

end.