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


function GetTagSubstr(Txt: string): string;
procedure CreateTagSelector;
procedure CheckEndTagIntroduction(OnTypedHash: boolean= false);
procedure EndTagIntroduction;
procedure GetTagsMaxWidth(NTags: TNoteTagList; var MaxNameWidth: integer; var MaxDescWidth: integer);
procedure UpdateTagSelector;
function IsValidTagName (var Name: string): boolean;


const
   TAG_MAX_LENGTH = 65;

type
   TIntroducingTagsMode = (stNoTags, stHashTyped, stWithTagSelector, stWithoutTagSelector);

var
   SelectingTagsMode: TIntroducingTagsMode;
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
     knt.App
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

   TV:= TVirtualStringTree.Create(Form_Main);
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
      SelectingTagsMode := stNoTags;
      EditorCtrlUI:= nil;
   end
   else
      SelectingTagsMode := stWithoutTagSelector;
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

    if (CursorPos.X + Width - MARGIN_LEFT_TAGSEL_TV) > Screen.WorkAreaRect.Right then
        Left:= CursorPos.X + (Screen.WorkAreaRect.Right -(CursorPos.X + Width - MARGIN_LEFT_TAGSEL_TV)) + OffsetX
    else
        Left:= CursorPos.X - MARGIN_LEFT_TAGSEL_TV + OffsetX;

    if (CursorPos.Y + SelectorHeight + 1.5 * FontHeight + SpaceBef) > Screen.WorkAreaRect.Bottom   then
       Top:= CursorPos.Y - SelectorHeight - 5 - SpaceBef
    else
       Top:= CursorPos.Y + Round(1.5 * FontHeight) + SpaceBef;

    if not Visible then
       inherited ShowSelector(Left, Top, OwnerHandle);

    SelectingTagsMode := stWithTagSelector;
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


//----------------------------------------

function GetTagSubstr(Txt: string): string;
var
   i: integer;
begin
   Result:= '';
   for i:= 2 to Length(Txt) do begin
      if Txt[i] in TagCharsDelimiters then begin
         Result:= Copy(Txt, 2, i-2);
         exit;
      end;
   end;
end;


procedure CreateTagSelector;
begin
  if cTagSelector <> nil then exit;

  cTagSelector:= TTagSelector.Create(Form_Main);
end;


procedure UpdateTagSelector;
var
  MaxNameWidth, MaxDescWidth: integer;
  Node: PVirtualNode;
begin
   if (IgnoreSelectorForTagSubsr <> '') and (AnsiCompareText(TagSubstr, IgnoreSelectorForTagSubsr) = 0) then begin
      cTagSelector.CloseTagSelector(false);
      exit;
   end;

   IgnoreSelectorForTagSubsr:= '';
   if TagSubstr <> '' then
      ActiveFile.UpdateNTagsMatching(TagSubstr, PotentialNTags);

   if (TagSubstr <> '') and (PotentialNTags.Count > 0) then begin
      GetTagsMaxWidth(PotentialNTags, MaxNameWidth, MaxDescWidth);
      cTagSelector.ShowTagSelector(MaxNameWidth, MaxDescWidth, Form_Main.Handle);
      with cTagSelector do begin
         TV.RootNodeCount := PotentialNTags.Count;
         Node:= TV.GetFirst;
         TV.FocusedNode:= Node;
         TV.ClearSelection;
         TV.Selected[Node] := True;
         TV.Invalidate;
      end;
   end
   else
     cTagSelector.CloseTagSelector(false);
end;


procedure CheckEndTagIntroduction(OnTypedHash: boolean= false);
var
   SS, EndRange: integer;
   Editor: TKntRichEdit;
   CEdit: TEdit;
   TxtFromCaretPos: string;
   Txt: string;
   PosTag: integer;     // including initial #

begin
   Editor:= nil;
   if cTagSelector.EditorCtrlUI is TKntRichEdit then begin
      Editor:= TKntRichEdit(cTagSelector.EditorCtrlUI);
      SS:= Editor.SelStart;
      if (SelectingTagsMode = stHashTyped) and (SS = CaretPosTag) or
         (SS = CaretPosTag-1) and (Editor.GetTextRange(SS, SS+1) = '#') then
         exit;
      TxtFromCaretPos:= Editor.GetTextRange(CaretPosTag-1, CaretPosTag + TAG_MAX_LENGTH);
      PosTag:= CaretPosTag;
   end
   else begin
      CEdit:= TEdit(cTagSelector.EditorCtrlUI);
      SS:= CEdit.SelStart;
      TxtFromCaretPos:= Copy(CEdit.Text, CaretPosTag);
      if (SS = CaretPosTag-1) and (TxtFromCaretPos <> '') then
         exit;
      if (TxtFromCaretPos <> '') and not (TxtFromCaretPos[1] in ['#',' ']) then
         TxtFromCaretPos:= '#' + TxtFromCaretPos;
      PosTag:= CaretPosTag-1;
   end;

   if (TxtFromCaretPos = '') or (TxtFromCaretPos[1] <> '#') then begin
      cTagSelector.CloseTagSelector(true);
      exit;
   end;

   if (cTagSelector.Active or cTagSelector.EditorCtrlUI.Focused) and (SS >= CaretPosTag) and (SS <= CaretPosTag + TAG_MAX_LENGTH) then
      Txt:= Copy(TxtFromCaretPos, 1, SS-PosTag+1)
   else
      Txt:= TxtFromCaretPos;

   if (Txt <> '') then begin
      if (SS < CaretPosTag) or OnTypedHash then
         Txt:= Txt + ' ';    // If the label is written right at the end and the cursor is moved to the left of # -> we will add it
      Txt:= GetTagSubstr(Txt);
      if not ( (Txt = '') and (SS < CaretPosTag) ) then begin
         if (Txt <> '') then begin
            TagSubstr:= Txt;
            EndTagIntroduction;
         end
         else begin
            Txt:= TxtFromCaretPos + ' ';
            Txt:= GetTagSubstr(Txt);
            TagSubstr:= Txt;
            UpdateTagSelector;
         end;
         exit;
      end;
   end;

   cTagSelector.CloseTagSelector(true);
end;


procedure EndTagIntroduction;
var
  NTag: TNoteTag;
begin
   cTagSelector.CloseTagSelector (true);
   if TagSubstr <> '' then begin
      if TagSubstr[Length(TagSubstr)] = '.' then
         delete(TagSubstr, Length(TagSubstr), 1);

      if ( AnsiCompareText( TagSubstr, IgnoreTagSubstr ) <> 0 ) then begin
         NTag:= ActiveFile.AddNTag(TagSubstr, '');
         if NTag <> nil then
            ActiveFile.NoteTagsTemporalAdded.Add(NTag)
         else begin
            NTag:= ActiveFile.GetNTagByName(TagSubstr, ActiveFile.NoteTagsTemporalAdded);
            if NTag <> nil then begin
               NTag.Name:= TagSubstr;      // If you are adding it temporarily and you change its spelling, we assume that you want to correct it. Eg: "TOdo" and then "ToDO"
               if App.TagsState = tsVisible then
                  Form_Main.TVTags.Invalidate;
            end;
         end;
      end;
   end;
   TagSubstr:= '';
   IgnoreTagSubstr:= '';
   IgnoreSelectorForTagSubsr:= '';
end;


function IsValidTagName (var Name: string): boolean;
var
   i: integer;
begin
   if Name = '' then exit(False);

   Result:= True;
   for i:= 1 to Length(Name) do begin
      if Name[i] in TagCharsDelimiters then
         exit (False);
   end;

   if Name[Length(Name)] = '.' then
      delete(Name, Length(Name), 1);
end;


procedure GetTagsMaxWidth(NTags: TNoteTagList; var MaxNameWidth: integer; var MaxDescWidth: integer);
var
  i: integer;
  NTag: TNoteTag;
  W, iName, iDesc: integer;
begin
  // -> Approximated widths
  MaxNameWidth:= 0;             // We will initially store the maximum length of the strings, not the width...
  MaxDescWidth:= 0;
  iDesc:= -1;
  if NTags <> nil then begin
     for i := 0 to NTags.Count-1 do begin
        NTag:= NTags[i];
        W:= Length(NTag.Name);
        if W > MaxNameWidth then begin
           MaxNameWidth:= W;
           iName:= i;
        end;

        if NTag.Description <> '' then begin
           W:= Length(NTag.Description);
           if W > MaxDescWidth then begin
              MaxDescWidth:= W;
              iDesc:= i;
           end;
        end;
     end;
  end;
  MaxNameWidth:= cTagSelector.TV.Canvas.TextWidth(NTags[iName].Name) + 20;
  if iDesc >= 0 then
     MaxDescWidth:= cTagSelector.TV.Canvas.TextWidth(NTags[iDesc].Description) + 20;
end;


end.