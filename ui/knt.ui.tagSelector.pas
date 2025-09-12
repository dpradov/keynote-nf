unit knt.ui.tagSelector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls,

  VirtualTrees, VirtualTrees.Types, VirtualTrees.BaseTree, VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL,

  knt.ui.selector,
  knt.model.note
  ;


type

  TTagSelector = class(TSelector)
  private
    CursorBeginPosTag: TPoint;
    FontHeight, SpaceBef: integer;

  protected
    procedure TV_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TV_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    function GetSelectedTagName: string;

  public
    TV: TVirtualStringTree;
    EditorCtrlUI: TWinControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowTagSelector(WidthName, WidthDesc: integer; OwnerHandle: HWND);
    procedure CloseTagSelector(EndTagEdition: boolean);
    procedure SelectTag (Key: integer);
    property SelectedTagName: string read GetSelectedTagName;
  end;



const
   TAG_MAX_LENGTH = 65;

type
   TIntroducingTagsState = (itDisabled, itNoTags, itHashTyped, itWithTagSelector, itWithoutTagSelector);

var
   IntroducingTagsState: TIntroducingTagsState;
   CaretPosTag: integer;
   TagSubstr: string;
   cTagSelector: TTagSelector;
   PotentialNTags: TNoteTagList;
   IgnoreTagSubstr: string;           // If we identify an unregistered tag, and KeyOptions.AutoDiscoverTags=False -> save here its name, to not register it.
   IgnoreSelectorForTagSubsr: string;  { If we press ESC and thus hide the selector, we can take note of which tag is there right now, so as not to show the selector again
                                         if it doesn't change, and thus not offer it again when the selection changes (for example by pressing <- or -> )
                                         We also use it to avoid CheckSelectingRegisteredTag being called continuously (from TForm_Main.ApplicationEventsIdle)
                                         once we have checked that there is no tag at the cursor position. }



implementation

uses gf_miscvcl,
     knt.ui.editor,
     kn_KntFile,
     knt.App,
     knt.RS
     ;

const
   MARGIN_LEFT_TAGSEL_TV = 6;


constructor TTagSelector.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   Width:= 10;
   Height:= 10;

   StyleElements:= [];
   Font.Name := 'Tahoma';
   Font.Size:= 10;

   TV:= TVirtualStringTree.Create(AOwner);
   with TV do begin
     Parent:= Self;
     BorderStyle:= bsNone;
     Anchors:= [];
     WantTabs:= True;
     OnGetText:= TV_GetText;
     OnPaintText:= TV_PaintText;
     TreeOptions.PaintOptions := [toThemeAware];
     ScrollBarOptions.ScrollBars:= ssVertical;
     Colors.UnfocusedColor:= Colors.SelectionTextColor;
     Colors.UnfocusedSelectionColor:= Colors.FocusedSelectionColor;
     Colors.UnfocusedSelectionBorderColor:= Colors.FocusedSelectionBorderColor;
   end;

   with TV.Header do begin
       with Columns.Add do
           Position:= 0;
       with Columns.Add do
           Position:= 1;
       MainColumn:= 0;
   end;

end;

destructor TTagSelector.Destroy;
begin
  inherited;
end;

procedure TTagSelector.CloseTagSelector (EndTagEdition: boolean);
begin
   if Visible then
      inherited HideSelector;

   if PotentialNTags <> nil then
      PotentialNTags.Clear;
   PotentialNTags:= nil;
   if EndTagEdition then begin
      IntroducingTagsState := itNoTags;
      EditorCtrlUI:= nil;
   end
   else
      IntroducingTagsState := itWithoutTagSelector;
end;

procedure TTagSelector.SelectTag (Key: integer);
var
  Node: PVirtualNode;
begin
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  if (Key = VK_UP) and (Node.Index > 0) then
     TV.FocusedNode:= TV.GetPrevious(Node)
  else
  if (Key = VK_DOWN) and (Node.Index < TV.RootNodeCount-1) then
     TV.FocusedNode:= TV.GetNext(Node);

  if Node <> TV.FocusedNode then begin
     TV.ClearSelection;
     TV.Selected[TV.FocusedNode]:= True;
     TV.ScrollIntoView(TV.FocusedNode, false);
  end;
end;

function TTagSelector.GetSelectedTagName: string;
begin
   Result:= PotentialNTags[TV.FocusedNode.Index].Name;
end;

procedure TTagSelector.ShowTagSelector(WidthName, WidthDesc: integer; OwnerHandle: HWND);
var
  Monitor: TMonitor;
  MonitorWorkArea: TRect;
  CursorPos: TPoint;
  SS, SL: integer;
  NumItems, SelectorHeight, ScrollW, OffsetX: integer;
  MarginR: integer;
  CEdit: TEdit;
begin
    if not Visible then begin
       if EditorCtrlUI is TKntRichEdit then begin
          with TKntRichEdit(EditorCtrlUI) do begin
              CursorPos:= ClientToScreen(GetCharPos(CaretPosTag-1));
              BeginUpdate;
              SS:= SelStart;
              SL:= SelLength;
              SelLength:= 1;
              FontHeight:= Round(Abs(SelAttributes.Height) * (ZoomCurrent/100));
              SetSelection(SS, SS+SL, True);
              SpaceBef:= Paragraph.SpaceBefore;
              EndUpdate;
          end;
       end
       else begin
          CEdit:= TEdit(EditorCtrlUI);
          OffsetX:= -10;
          with CEdit do begin
              CursorPos:= GetEditCaretPos(CEdit, CaretPosTag-1);
              FontHeight:= Abs(CEdit.Font.Height);
              SpaceBef:= 0;
          end;
       end;
       CursorBeginPosTag:= CursorPos;
    end
    else
       CursorPos:= CursorBeginPosTag;


    Monitor := Screen.MonitorFromPoint(Point(CursorPos.X, CursorPos.Y));
    MonitorWorkArea := Monitor.WorkareaRect;

    OffsetX:= 0;
    if EditorCtrlUI is TEdit then
       OffsetX:= -8;

    ScrollW:= 0;
    MarginR:= MARGIN_LEFT_TAGSEL_TV;
    NumItems:= PotentialNTags.Count;

    if NumItems > 10 then begin
       NumItems:= 10;
       ScrollW:= EditorCtrlUI.GetSystemMetrics(SM_CXVSCROLL) + 5;
       MarginR:= ScrollW;
    end;

    SelectorHeight:= (NumItems * TV.DefaultNodeHeight) + 4;
    Height:= SelectorHeight;
    TV.Height:= SelectorHeight-4;

    Width:= WidthName + WidthDesc + MARGIN_LEFT_TAGSEL_TV + MarginR;
    TV.Width:= WidthName + WidthDesc + ScrollW;
    TV.Header.Columns[0].Width:= WidthName;
    TV.Header.Columns[1].Width:= WidthDesc;
    TV.Top:= 2;
    TV.Left:= MARGIN_LEFT_TAGSEL_TV;

    if (CursorPos.X + Width - MARGIN_LEFT_TAGSEL_TV) > MonitorWorkArea.Right then
        Left:= CursorPos.X + (MonitorWorkArea.Right -(CursorPos.X + Width - MARGIN_LEFT_TAGSEL_TV)) + OffsetX
    else
        Left:= CursorPos.X - MARGIN_LEFT_TAGSEL_TV + OffsetX;

    if (CursorPos.Y + SelectorHeight + 1.5 * FontHeight + SpaceBef) > MonitorWorkArea.Bottom   then
       Top:= CursorPos.Y - SelectorHeight - 5 - SpaceBef
    else
       Top:= CursorPos.Y + Round(1.5 * FontHeight) + SpaceBef;

    if not Visible then
       inherited ShowSelector(Left, Top, OwnerHandle);

    IntroducingTagsState := itWithTagSelector;
end;


procedure TTagSelector.TV_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NTag: TNoteTag;
begin
   NTag:= PotentialNTags[Node.Index];
   case Column of
     -1, 0: CellText:= NTag.Name;
         1: CellText:= NTag.Description;
    end;
end;

procedure TTagSelector.TV_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Color: TColor;
  NTag: TNoteTag;
begin
   if TV.Selected[Node] and (Column <= 0) then exit;
   NTag:= PotentialNTags[Node.Index];
   if (ActiveFile.NoteTagsTemporalAdded.Count > 0) and (ActiveFile.NoteTagsTemporalAdded.IndexOf(NTag) >= 0) then
     TargetCanvas.Font.Color := clGreen
   else
   if not AnsiStartsText(TagSubstr, NTag.Name) then
      TargetCanvas.Font.Color := $FF4D4D;
end;


initialization
   cTagSelector:= nil;
   PotentialNTags:= nil;

end.