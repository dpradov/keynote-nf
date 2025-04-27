unit knt.ui.tree;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ActiveX,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  System.Contnrs, System.Actions, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Clipbrd,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.ImgList,

  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,

  TB97Ctls,
  RxRichEd,
  gf_misc,
  kn_Info,
  kn_Const,
  knt.model.note,
  knt.ui.TagMng
  ;


type
   TVirtualStringTreeHelper = class helper for TVirtualStringTree
   public
     function GetPreviousNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
     function GetNextNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
     function GetNextChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
     function GetNextNotChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
     function GetNextVisibleNotChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
     function GetNextNotChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;

     function IsVerticalScrollBarVisible: boolean;
   end;


  TNoteTagArray_Array = Array of TNoteTagArray;

  TTreeTransferAction = (ttCopy, ttCut, ttPaste, ttClear);

  TVTree = TVirtualStringTree;

  TNodeList = class (TObject)
  private
    FList: TList;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(const Node: PVirtualNode): integer; inline;
    function IndexOf(const Node: PVirtualNode): Integer; inline;
    procedure Clear; inline;
    function Count: integer; inline;
  end;


  TKntTreeUI = class(TFrame)
    TV: TVirtualStringTree;
    CheckImages: TImageList;
    PnlInf: TPanel;
    txtFilter: TEdit;
    TB_FilterTree: TToolbarButton97;
    TB_HideChecked: TToolbarButton97;
    TB_FilterUnflagged: TToolbarButton97;
    txtTags: TEdit;


  private class var
    fSourceTVSelectedNodes: TNodeArray;
    fiNextSourceTVNode: integer;
    fCutSubtrees: boolean;                       // Copy or cut operation
    fCopyingAsLinked: boolean;
    fTVCopiedNodes: TNodeList;
    fVirtualNodesConvertedOnCopy: integer;
    fChildCountOnTargetNode: integer;
    fMovingToOtherTree: boolean;
    fTargetFolder: TFolderObj;
    fNNodesInSubtree: TNoteNodeArray;
    fDropTargetNode: PVirtualNode;
    fDropTargetNodeInsMode: TNodeInsertMode;

    fTreeWidthExpanded: boolean;
    fTreeWidth_N: Cardinal;
    fTreeWidthNodeTouched: boolean;
    fSplitterNoteMoving: boolean;

    fTempFilterTagApplying: boolean;

  public
    class constructor Create;
    class destructor Destroy;
    class procedure ClearGlobalData;

    class property DropTargetNode: PVirtualNode read fDropTargetNode write fDropTargetNode;
    class property DropTargetNodeInsMode: TNodeInsertMode read fDropTargetNodeInsMode write fDropTargetNodeInsMode;

  private
    fFolder: TObject;
    fSplitterNote : TSplitter;
    fPopupMenu : TPopupMenu;
    FDateColor: TColor;

    fReadOnly: boolean;
    fLastNodeSelected : PVirtualNode;
    fNumberingDepthLimit: Byte;

    fFindFilterApplied: boolean;
    fTreeFilterApplied: boolean;
    fFilterOutUnflaggedApplied: boolean;
    fFindTags: TFindTags;
    fChangesInFlagged: boolean;
    fNNodesFlagged: boolean;
    fLastTreeSearch: string;

    fParentNodesWithInheritedTags: TNodeList;
    fInheritedTags: TNoteTagArray_Array;
    fTagsNonUsed: TNoteTagList;
    fTagsInUse: TNoteTagList;                   // Depends on filtering
    fTagsNumberOfUses: TIntegerList;
    fShownFlaggedColumnForTagFilter: boolean;
    fShowUseOfTags: boolean;
    fTagsFilterModeOR: boolean;
    fUseOfTagsCalcWithInheritedTags: boolean;

    fTimer: TTimer;
    fColsSizeAdjusting: integer;


  protected
    // Create. Destroy
    procedure SetFolder(aFolder: TObject);
    procedure SetSplitterNote(aSplitter: TSplitter);
    procedure SetPopupMenu(value: TPopupMenu);

    procedure FrameResize(Sender: TObject);

    // TreeView Handlers
    //procedure TV_SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TV_FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TV_Checked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TV_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TV_KeyPress(Sender: TObject; var Key: Char);
    procedure TV_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
                         var CellText: string);
    procedure TV_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
                           Column: TColumnIndex; TextType: TVSTTextType);
    procedure TV_GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
                               var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure TV_GetImageText(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
                               var ImageText: string);

    procedure TV_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
                                Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TV_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    //procedure TV_SavingTree(Sender: TObject; Node: PVirtualNode; var S: string);

    procedure TV_Resize(Sender: TObject);
    procedure TV_HeaderCustomDrawTreeHeaderDrawQueryElements(
                                    Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure TV_AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);

    procedure TV_HeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
    procedure SimulateDoubleClick(Sender: TObject);
    procedure TV_NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);

    function GetFocused: boolean;
    function GetIconKind : TNodeIconKind;
    function GetShowAllCheckboxes: boolean;
    function GetHideCheckedNodes: boolean;
    function GetFocusedNode: PVirtualNode;
    procedure SetFocusedNode(value: PVirtualNode);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTreeOptions;
  protected
    procedure PopulateTV;
    procedure SetupTreeHandlers;
    procedure CheckNotifyAccesibleFocusedItem (event: CARDINAL);

  public
    procedure ApplyBiDiMode;
    procedure UpdateTreeChrome;
    procedure UpdateTreeColumns (ResizeCols: boolean = True);
    procedure ShowAdditionalColumns (Show: boolean);
    procedure ShowFlaggedColumnForTagFilter (Show: boolean);
    function AdditionalColumnsAreVisible: boolean;
    function GetSizeOfColDate: integer;

    function GetNNode(Node: PVirtualNode): TNoteNode; inline;
    procedure SetNNode(Node: PVirtualNode; NNode: TNoteNode); inline;
    function GetFocusedNNode: TNoteNode;
    function GetFirstNode: PVirtualNode;
    property FocusedNode: PVirtualNode read GetFocusedNode write SetFocusedNode;
    procedure SelectAlone(Node: PVirtualNode);

    procedure SaveToFile(const FileName: TFileName);

    property Folder: TObject read fFolder write SetFolder;
    property SplitterNote : TSplitter read fSplitterNote write SetSplitterNote;
    property PopupMenu : TPopupMenu read fPopupMenu write SetPopupMenu;

    property ReadOnly: boolean read fReadOnly write fReadOnly;
    function CheckReadOnly: boolean;
    property Focused: boolean read GetFocused;
    function IsEditing: boolean;

    property ShowAllCheckboxes: boolean read GetShowAllCheckboxes;
    property HideCheckedNodes: boolean read GetHideCheckedNodes;
    property IconKind : TNodeIconKind read GetIconKind;

    procedure ShowOrHideIcons;
    procedure SetNodeColor(Node: PVirtualNode; NodeColor: TColor; AsTextColor, DoChildren : boolean); overload;
    procedure SetNodeColor(const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean); overload;
    procedure ToggleNodeFlagged(Node: PVirtualNode);
    procedure SetNodeBold(const DoChildren : boolean); overload;
    procedure SetNodeBold(Node: PVirtualNode; Bold: boolean; const DoChildren : boolean); overload;
    procedure SetNodeCustomImage; overload;
    procedure SetNodeCustomImage(Node: PVirtualNode; NewIdx: Integer; DoChildren: Boolean); overload;
    function GetNodeFontFace (Node: PVirtualNode): string;
    procedure SetNodeFontFace(const ResetDefault, DoChildren : boolean); overload;
    procedure SetNodeFontFace(Node: PVirtualNode; const FontFace : string; const DoChildren: boolean); overload;
  //procedure SetNodeFontSize(TreeNode: PVirtualNode; const ResetDefault, DoChildren : boolean);

  public
    function GetNodePath(aNode: PVirtualNode; const aDelimiter: string; const TopToBottom: boolean) : string;
    function GetNodeAncestorsPath(aNode: PVirtualNode; const aDelimiter: string;
                                  MaxDepth: integer; StartAtLevel: integer = 1) : string;
    procedure CopyNodePath(myTreeNode : PVirtualNode; const InsertInEditor : boolean);
    procedure PasteNodeName(Node : PVirtualNode; const PasteMode : TPasteNodeNameMode);
    procedure CopyNodeName(const IncludeNoteText: boolean);
    procedure RenameFocusedNode;
   protected
    procedure TV_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TV_CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TV_Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TV_NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);

   // Outline numbering
   protected
    procedure SetNumberingDepthLimit(value: Byte);
   public
    function GetNodeCaption(Node: PVirtualNode): string;
    procedure SetNumberingMethod (Node: PVirtualNode);
    function OutlineNumber(Node: PVirtualNode): string;
    procedure OutlineNumberNodes;
    property NumberingDepthLimit: Byte read fNumberingDepthLimit write SetNumberingDepthLimit;

   // Sort - Expand - Navigate
   public
    procedure SortSubtree(Node : PVirtualNode);
    procedure SortTree;
    procedure SetNodeExpandState(const TopLevelOnly: boolean);
    procedure FullCollapse;
    procedure FullExpand;
    procedure Navigate(NavDirection: TNavDirection);

    // Create new nodes
    function AddNode(aInsMode: TNodeInsertMode) : TNoteNode;
    function NewNode(aInsMode: TNodeInsertMode; OriginNode: PVirtualNode; const aNewNodeName: string;
                      const aDefaultNode: boolean) : TNoteNode;
    procedure CreateParentNode(Node: PVirtualNode);
    procedure CreateNodefromSelection;
    procedure SetupNewTreeNode(const Node : PVirtualNode; Loaded: boolean = false; RemoveFiltered: boolean = false);

    // Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees  | Drag and Drop
   public
    function MoveTreeNode(MovingNode : PVirtualNode; const aDir : TDirection): boolean; overload;
    procedure MoveTreeNode(const aDir : TDirection); overload;
    function DeleteNode(myTreeNode: PVirtualNode; const DeleteOnlyChildren: boolean;
                        const AskForConfirmation: boolean = true;
                        const ConfirmationOnlyForChildren: boolean = false): boolean; overload;
    procedure DeleteNode(const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true); overload;
    procedure CopySubtrees (TargetNode: PVirtualNode; Prompt: boolean; PasteAsLinkedNNode: boolean; AttachMode: TVTNodeAttachMode= amAddChildLast);
    procedure MoveSubtrees (TargetNode: PVirtualNode; Prompt: boolean; AttachMode: TVTNodeAttachMode= amAddChildLast);
    function TreeTransferProc(XferAction: TTreeTransferAction; Prompt: boolean; PasteAsVirtualKNTNode: boolean) : boolean;
    procedure InsertLinkedNNode(Node: PVirtualNode);
   protected
    class procedure GetSelectedSubtrees (SourceTV: TBaseVirtualTree);
    function IsIncludedInSelectedSubtrees(Node: PVirtualNode): boolean;
    function HasVirtualNotesInSelectedSubtrees: boolean;
    procedure LoadNNodesInSubtree(Node: PVirtualNode);
    procedure TV_NodeCopying(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
    procedure TV_NodeMoving(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
    procedure TV_NodeMoved (Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TV_StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TV_DragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
                          Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure TV_DragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
                          Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure TV_EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TV_FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);


    // Check State
   public
    procedure ShowOrHideCheckBoxes;
    procedure ToggleChildrenCheckbox(Node : PVirtualNode);
    procedure ToggleCheckNode(Node : PVirtualNode);
    procedure HideChildNodesUponCheckState (ParentNode: PVirtualNode; Checked: boolean);
    procedure ShowNonFilteredNodes (ParentNode: PVirtualNode);
    procedure TB_HideCheckedClick(Sender: TObject);
  protected
    procedure ShowOrHideChildrenCheckBoxes(const NNode : TNoteNode);
    procedure ChangeCheckedState(Node: PVirtualNode; Checked: Boolean);


    // Filter nodes
  public
    procedure MakePathVisible (Node: PVirtualNode);
    procedure MakePathNonFiltered (Node: PVirtualNode);
    procedure ClearAllFindMatch;
    procedure ClearAllTreeMatch;
    procedure SetFilteredNodes;
    procedure ReapplyFilter;
    procedure ApplyFilters (Apply: boolean);
    function GetTagFilterApplied: boolean;
    property FindFilterApplied: boolean read fFindFilterApplied write fFindFilterApplied;
    property TreeFilterApplied: boolean read fTreeFilterApplied write fTreeFilterApplied;
    property TagFilterApplied:  boolean read GetTagFilterApplied;
    property FilterOutUnflaggedApplied: boolean read fFilterOutUnflaggedApplied;
    function NoFindFilterMatch: boolean;
    property NNodesFlagged: boolean read fNNodesFlagged;

    procedure FilterApplied (Applied: boolean);   // [dpv]
    procedure ApplyFilterOnFolder;   // [dpv]
    procedure ClearFindFilter;
    procedure CheckFilterOff;
    procedure CheckFocusedNode;
    procedure ExecuteTreeFiltering(ForceReapplying: boolean= True);
    procedure txtFilterChange(Sender: TObject);
    procedure FilterOutUnflagged (Apply: boolean);
    procedure ActivateFilter;
    procedure TB_FilterTreeClick(Sender: TObject);

    // Filter nodes: Tags
  protected
    procedure txtTagsEnter(Sender: TObject);
    procedure AddUseOfTags (NNode: TNoteNode; InheritedParentTags: TNoteTagArray);
    procedure PopulateNonUsedTags;
  public
    property FindTags: TFindTags read fFindTags;
    procedure OnChangeFindTagsIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnEndFindTagsIntroduction(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure CheckFilterNotesOnTags (Temp: boolean = True);
    procedure AdjustTxtTagsWidth (AllowEdition: boolean = False);
    procedure PopulateInheritedTags;
    property TagsInUse: TNoteTagList read FTagsInUse;                     // Depends on filtering
    property TagsNumberOfUses: TIntegerList read FTagsNumberOfUses;
    property TagsNonUsed: TNoteTagList read fTagsNonUsed;
    property ShowUseOfTags: boolean read fShowUseOfTags write fShowUseOfTags;
    property UseOfTagsCalcWithInheritedTags: boolean read fUseOfTagsCalcWithInheritedTags write fUseOfTagsCalcWithInheritedTags;
    property TagsFilterModeOR: boolean read fTagsFilterModeOR write fTagsFilterModeOR;
    property ParentNodesWithInheritedTags: TNodeList read fParentNodesWithInheritedTags;
    property InheritedParentTags: TNoteTagArray_Array read fInheritedTags;


    // Tree width expansion
  public
    function CheckRestoreTreeWidth: boolean;
  protected
    procedure OnTreeExpanded(Expanded: boolean);
    procedure OnAfterChangesOnTreeWidth;
    procedure SplitterNoteMoved(Sender: TObject);
    procedure CheckingTreeExpansion;
    procedure CheckExpandTreeWidth;
    procedure NoteUIEnter(Sender: TObject);
    procedure NoteUIMouseMove(Sender: TObject);
    procedure NoteUIMouseUp(Sender: TObject);
    procedure TV_Click(Sender: TObject);
    procedure TV_MouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
    procedure TV_MouseLeave(Sender: TObject);
    procedure TV_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure DoEnter; override;


    // Virtual nodes
  public
    procedure VirtualNoteRefresh( const DoPrompt : boolean );
    procedure VirtualNoteUnlink;

  end;



implementation
uses
   VirtualTrees.Accessibility_MOD,
   gf_strings,
   gf_streams,
   gf_miscvcl,
   gf_files,
   gf_Lang,
   kn_Global,
   kn_EditorUtils,
   knt.ui.editor,
   kn_Chest,
   kn_clipUtils,
   kn_ImagePicker,
   kn_NodeNum,
   kn_Macro,
   kn_Main,
   kn_ExportNew,
   kn_LinksMng,
   kn_MacroMng,
   kn_NoteFileMng,
   kn_KntFolder,
   kn_AlertMng,
   kn_FindReplaceMng,
   Knt.App,
   knt.RS
   ;

{$R *.dfm}
{$R resources\VTcheckLight.RES}



const
  TREE_DEFAULT_WIDTH = 200;
  TREE_WIDTH_MOUSE_TIMEOUT = 2500;

  FILTER_PANEL_WIDTH = 175;
  FILTER_PANEL_MAX_WIDTH_ALIGNBOTTOM = 420;

  MIN_WIDTH_NAME_COL = 200;
  FLAGGED_COL_WIDTH = 20;




// Create  / Destroy =========================================

{$REGION Create / Destroy}

class constructor TKntTreeUI.Create;
begin
  fTreeWidthExpanded:= false;
  fTreeWidth_N:= 0;
  fTreeWidthNodeTouched:= false;
  fSplitterNoteMoving:= false;

  fSourceTVSelectedNodes:= nil;
  fTVCopiedNodes:= TNodeList.Create;
  fiNextSourceTVNode:= 0;
  fVirtualNodesConvertedOnCopy:= 0;
  fCutSubtrees:= false;
  fMovingToOtherTree:= false;

  fDropTargetNode:= nil;
  fDropTargetNodeInsMode:= tnAddLast;

  fTempFilterTagApplying:= false;
end;


class destructor TKntTreeUI.Destroy;
begin
  ClearGlobalData;
end;

class procedure TKntTreeUI.ClearGlobalData;
begin
  fSourceTVSelectedNodes:= nil;
  fTVCopiedNodes.Clear;
end;


constructor TKntTreeUI.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   fLastNodeSelected := nil;
   fFindFilterApplied:= false;
   fTreeFilterApplied:= false;
   fFilterOutUnflaggedApplied:= false;
   fChangesInFlagged:= false;
   fColsSizeAdjusting:= 0;

   fFindTags:= nil;
   fShownFlaggedColumnForTagFilter:= false;
   fParentNodesWithInheritedTags:= TNodeList.Create;
   fTagsNonUsed:= TNoteTagList.Create;
   fTagsInUse:= TNoteTagList.Create;
   fTagsNumberOfUses:= TIntegerList.Create;
   fTagsFilterModeOR:= false;
   fShowUseOfTags:= false;

   fTimer:= nil;

   txtTags.Text:= EMPTY_TAGS;
   txtTags.Font.Color:= clGray;

   fNumberingDepthLimit:= 2;

   with TV do begin
     DefaultText := DEFAULT_NEW_NOTE_NAME;
     HelpContext:= 27;  // Tree-type Notes
   end;

   TB_HideChecked.Hint := Form_Main.MMViewHideCheckedNodes.Hint;    // [dpv]
   LoadGifFromResource(CheckImages, 'VTCHECKIMGS');

   with TV.Header do begin
       Height := 18;
       Options:= [hoColumnResize,hoDblClickResize,hoDrag,hoShowHint, hoOwnerDraw];
       AutoSize := False;
       with Columns.Add do begin
           Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coStyleColor, coVisible, coSmartResize];
           Text:= GetRS(sTree62);       // Name
           Hint:= GetRS(sTree63);       // Note name
           Position:= 1;
           Style:= vsOwnerDraw;
       end;
       with Columns.Add do begin
           Text:= GetRS(sTree64);      // Date
           Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coStyleColor];
           Hint:= GetRS(sTree65);      // Note creation date
           Position:= 2;
           Style:= vsOwnerDraw;
       end;
       with Columns.Add do begin
           Text:= '*';
           Hint:= GetRS(sTree66);  // Flagged
           Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coShowDropMark, coStyleColor];
           Width:= FLAGGED_COL_WIDTH;
           Position:= 0;
           Style:= vsOwnerDraw;
       end;
       MainColumn:= 0;
   end;

end;


destructor TKntTreeUI.Destroy;
begin
   TV.OnChange := nil;
   // When removing a folder or closing the file, don't free each node and note guided by Virtual TreeView
   TV.OnFreeNode:= nil;
   TV.Free;
   fParentNodesWithInheritedTags.Free;
   fTagsNonUsed.Free;
   fTagsInUse.Free;
   fTagsNumberOfUses.Free;

   inherited;
end;


procedure TKntTreeUI.SetSplitterNote(aSplitter: TSplitter);
begin
   fSplitterNote:= aSplitter;
   fSplitterNote.OnMoved:= SplitterNoteMoved;
end;


procedure TKntTreeUI.SetPopupMenu(value: TPopupMenu);
begin
   fPopupMenu:= value;
   TV.PopupMenu:= value;
end;


procedure TKntTreeUI.SetFolder(aFolder: TObject);
var
  Folder: TKntFolder;
  BiDiMode: TBiDiMode;
begin
  fFolder:= aFolder;
  Folder:= TKntFolder(fFolder);

  fReadOnly:= Folder.ReadOnly;

   if Folder.VerticalLayout then
     Align := alTop
   else
     Align := alLeft;

  // *1 Commented because: when this is checked, the window has not yet been resized to its final size.
  //    This verification is removed as it seems unnecessary.

   if Folder.VerticalLayout then begin
     if ( Folder.TreeWidth < 30 )
         // or ( Folder.TreeWidth > ( Pages.Height - 30 )))   // *1
     then
       Height := TREE_DEFAULT_WIDTH
     else
       Height := Folder.TreeWidth;
     Folder.TreeWidth := Height; // store corrected value
   end
   else begin
     if ( Folder.TreeWidth < 30 )
        // or ( Folder.TreeWidth > ( Pages.Width - 30 )))       // *1
     then
        Width := TREE_DEFAULT_WIDTH
     else
        Width := Folder.TreeWidth;
     Folder.TreeWidth := Width; // store corrected value
   end;

   FrameResize(nil);

   TB_FilterUnflagged.Images:= Form_Main.IMG_TV;
   TB_FilterUnflagged.Action:= Form_Main.actTVFilterOutUnflagged;
   TB_FilterUnflagged.Caption:= '';

   UpdateTreeOptions;
   UpdateTreeChrome;
   ApplyBiDiMode;

   PopulateTV;
   SetupTreeHandlers;

   TB_HideChecked.Down:= Folder.HideCheckedNodes;

   if Folder.TreeMaxWidth > Folder.TreeWidth then
      SplitterNote.Color:= clLtGray;

   UpdateTreeColumns (False);

   fTimer := TTimer.Create(Self);
   fTimer.Interval := 5;
   fTimer.OnTimer := TV_Resize;
   fTimer.Tag:= 9999;
   fTimer.Enabled:= True;
end;

procedure TKntTreeUI.ApplyBiDiMode;
var
   BiDi: TBiDiMode;
begin
   if TKntFolder(fFolder).RTL or IsRightToLeftLanguage then
      Self.BiDiMode:= bdRightToLeft;   // It doesn't affect TV, where ParentBiDiMode=False

   BiDi:= bdLeftToRight;
   if App.UI_RTL or IsRightToLeftLanguage then
      BiDi:= bdRightToLeftNoAlign;
   if TKntFolder(fFolder).RTL then
      BiDi:= bdRightToLeft;

   TV.BiDiMode:= BiDi;
end;

procedure TKntTreeUI.FrameResize(Sender: TObject);
var
   Folder: TKntFolder;
   LeftShift: integer;
   VerticalScrollBarWidth: integer;
begin
   if (Folder = nil) or fTempFilterTagApplying then exit;

   Folder:= TKntFolder(Self.Folder);
   if Folder.VerticalLayout then begin
     if Self.Width <= FILTER_PANEL_MAX_WIDTH_ALIGNBOTTOM then
        PnlInf.Align:= alBottom

     else begin
        PnlInf.Align:= alNone;
        PnlInf.Width:= FILTER_PANEL_WIDTH;
        PnlInf.Left:= Width - FILTER_PANEL_WIDTH;
        PnlInf.Top:= Height - PnlInf.Height - Folder.Splitter.Height + 2;
        LeftShift:= 2;
        if TV.IsVerticalScrollBarVisible  then begin
           VerticalScrollBarWidth := GetSystemMetrics(SM_CXVSCROLL);
           LeftShift:= VerticalScrollBarWidth + 3;
        end;
        PnlInf.Left:= PnlInf.Left - LeftShift;
     end;
   end;
   AdjustTxtTagsWidth(txtTags.Focused)
end;


procedure TKntTreeUI.TV_Resize(Sender: TObject);
var
   W: integer;
   VertScrollBarWidth: integer;
   ColNamePos: integer;
   ColNameLastPos: boolean;
   TVWidth: integer;
begin
   if fColsSizeAdjusting = 1 then exit;
   fColsSizeAdjusting:= 1;

   if (fTimer <> nil) and (fTimer.Tag = 9999) then begin
       fTimer.Free;
       fTimer:= nil;
       TV.OnResize:= TV_Resize;
   end;

   TV.BeginUpdate;

   W:= 0;
   ColNameLastPos:= True;

   with TV.Header do begin
      TV.Header.AutoFitColumns(False,smaUseColumnOption);
      ColNamePos:= Columns[0].Position;
      Columns[2].Width:= FLAGGED_COL_WIDTH;

      if coVisible in Columns[1].Options then begin
         W:= Columns[1].Width;
         if Columns[1].Position > ColNamePos then
            ColNameLastPos:= false;
      end;
      if coVisible in Columns[2].Options then begin
         inc(W, FLAGGED_COL_WIDTH);
         if Columns[2].Position > ColNamePos then
            ColNameLastPos:= false;
      end;

      VertScrollBarWidth:= 0;
      if TV.IsVerticalScrollBarVisible  then
         VertScrollBarWidth := GetSystemMetrics(SM_CXVSCROLL) + 8;

      TVWidth:= TV.Width;
      if ((W > 0) and (not ColNameLastPos)) or (Columns[0].Width + W + VertScrollBarWidth < TVWidth) then begin
         W:= TVWidth - W- VertScrollBarWidth;
         Columns[0].Width:= W - 5;
      end;

   end;

   TB_FilterUnflagged.Visible:= TVWidth >= 118;
   TB_FilterTree.Visible:= TVWidth > 92;
   if (TVWidth > 92) and (TVWidth < 118) then
      txtFilter.Width:= TB_FilterTree.Left - txtFilter.Left
   else
   if (TVWidth <= 92) then
      txtFilter.Width:= TVWidth - 2 * TB_HideChecked.Width
   else
      txtFilter.Width:= TB_FilterUnflagged.Left - txtFilter.Left;

   TV.EndUpdate;
   fColsSizeAdjusting:= 2;
end;


procedure TKntTreeUI.UpdateTreeOptions;
begin
  // Updates options for current folder's tree based on global tree options

  // Note: If there are no columns created (the default column, -1, is used), the tree appears to behave as if the
  // "toDisableAutoscrollOnFocus" option was checked, even if it is not.
  // But as soon as there is any column, even if it is just one, it is essential to activate that option if we want
  // "Disable scrolling a node or column into view if it gets focused."
  // That is what I can see in the Advanced\SpeedDemo example (there are no columns defined), as well as in the
  // Advanced\GeneralAbilitiesDemo demo, where there are columns, and where only after removing those columns (and adjusting
  // the code to use the -1 column) does the behavior become that way (without setting toDisableAutoscrollOnFocus).

  with TV do begin
      DragMode := dmAutomatic;

      TreeOptions.MiscOptions := [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave,
                                  toToggleOnDblClick, toWheelPanning, toEditOnClick];
      TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons,
                                  toAutoDeleteMovedNodes, toAutoChangeScale, toDisableAutoscrollOnFocus];
      TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toAlwaysSelectNode, toSelectNextNodeOnRemoval];
      TreeOptions.StringOptions := [toAutoAcceptEditChange];

      // Default:
      // TreeOptions.PaintOptions := [toShowButtons,toShowDropmark,toShowRoot,toShowTreeLines,toThemeAware,toUseBlendedImages]
      // TreeOptions.AnimationOptions := [];
      // EditOptions:= toDefaultEdit
      // ExportMode:= emAll

      // - [toUseBlendedImages, toThemeAware];
      TreeOptions.PaintOptions:= [toShowButtons,toShowDropmark,toShowRoot,toShowTreeLines];

//    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoExpand];

      if KntTreeOptions.EditInPlace then
        TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable];

      if KntTreeOptions.AutoScroll then
        TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoScroll];

      if KntTreeOptions.HotTrack then
        TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toHotTrack];

      if KntTreeOptions.FullRowSelect then
        TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];

      IncrementalSearch:= isVisibleOnly;

      CheckImageKind:= ckCustom;
      HintMode:= hmTooltip;
  end;


end; // UpdateTreeOptions


procedure TKntTreeUI.PopulateTV;
var
  myFolder: TKntFolder;
  i: integer;
  tNode, myTreeNode, LastTreeNodeAssigned : PVirtualNode;
  L, NodeLevel, LastNodeLevel: integer;
  j, numChilds, AuxLevel, ChildLevel : integer;
  NNode: TNoteNode;

begin
   myFolder:= TKntFolder(Self.Folder);

   with TV do begin

      myFolder.UpdateTree; // do this BEFORE creating nodes


      // Create TreeNodes for all notes in the folder
      LastTreeNodeAssigned := nil;
      LastNodeLevel := 0;

      if ( myFolder.NNodes.Count > 0 ) then begin

        BeginUpdate;
        try

           for i := 0 to myFolder.NNodes.Count-1 do begin
              NNode := myFolder.NNodes[i];
              if NNode.FindFilterMatch then
                 FindFilterApplied:= True;
              if NNode.TreeFilterMatch then
                 TreeFilterApplied:= True;
              if NNode.Flagged then
                 fNNodesFlagged:= True;

              NodeLevel:= myFolder.LoadingLevels[i];
              case NodeLevel of
                0 : begin
                  myTreeNode := AddChild(nil);          // Adds a node to the root of the Tree.
                  LastNodeLevel := 0;
                end
                else begin
                  case DoTrinaryCompare(NodeLevel, LastNodeLevel) of
                    trinGreater:
                      myTreeNode := AddChild(LastTreeNodeAssigned);    // Adds a node as the last child of the given node
                    trinEqual:
                      myTreeNode := AddChild(LastTreeNodeAssigned.Parent);
                    trinSmaller: begin  // myNote.Level is SMALLER than LastNodeLevel, i.e. we're moving LEFT in the tree
                       for L := LastNodeLevel downto NodeLevel do begin
                          if assigned(LastTreeNodeAssigned) then begin
                            if L <= NodeLevel then break;
                            LastTreeNodeAssigned := LastTreeNodeAssigned.Parent;
                          end
                          else
                            break;
                       end;
                       myTreeNode := TV.InsertNode(LastTreeNodeAssigned, amInsertAfter);
                    end;
                  end;
                end;
              end;

              LastTreeNodeAssigned := myTreeNode;
              LastNodeLevel := NodeLevel;

              SetNNode(myTreeNode, NNode);
              SetupNewTreeNode(myTreeNode, True);
              NNode.TVNode:= myTreeNode;
           end;


        finally
          Log_StoreTick( 'After created TreeNodes', 3 );

          myFolder.LoadingLevels.Clear;

          ShowOrHideIcons;

          // If filtering was applied, keep the matches (and node highlighting), but keep the filter disabled by default
          myFolder.Filtered:= FindFilterApplied or TreeFilterApplied;
          if myFolder.Filtered then begin
             SetFilteredNodes;
             TB_FilterTree.Enabled := True;
             Form_Main.MMViewFilterTree.Enabled:= True;
             TB_FilterTreeClick(nil);
          end;

          if myFolder.HideCheckedNodes then
             HideChildNodesUponCheckState (nil, true);

          EndUpdate;

          Log_StoreTick( 'After HideFilteredNodes, HideCheckNodes', 3 );
        end;

        // restore selected node: this block must be
        // OUTSIDE the beginupdate..endupdate range

        tNode:= nil;

        // *1
        // Solution for a posible bug in Virtual Tree View detected when importing folders (merging) from
        // a certain knt file. The file could be opened normally but a Range Check Error exception was generated when
        // that particular file whas merged in any other file.
        // With all other files (small and big ones), merging folders worked totally ok.
        // After many tests on the problematic file I detected that the exception was not raised if all the nodes where
        // added to the root of the Tree, or all of them where created as child of the precedent.. ¿? It only
        // happened if the folder included more than certain number of nodes, etc.
        // Finally, after *many hours* looking for a reason to this strange behaviour I founded that when merging,
        // the folder kept SavedSelectedIndex unitialized (-1) (it is also the value saved if TV.FocusedNode was nil).
        // When setting SavedSelectedIndex=0 before calling this method all worked ok.
        // The clear difference was that the line *1 was executed. But that was not the problem!. When
        // SavedSelectedIndex=-1 the condition 'myFolder.SavedSelectedIndex < TV.TotalCount' was not verified and
        // so TV.TotalCount was not executed. It seems that TV.TotalCount do some kind of necessary initialization,
        // (that I suppose that it will be done also in other situations). Even with SavedSelectedIndex=-1, if
        // TV.TotalCount is first executed, all works ok.

        i:= TV.TotalCount;

        if TV.VisibleCount > 0 then begin
          if (( KntTreeOptions.ExpandMode <> txmFullCollapse ) and // SaveActiveNode and
             ( myFolder.SavedSelectedIndex >= 0 ) and
             ( myFolder.SavedSelectedIndex < TV.TotalCount )) then
            // restore the node which was selected when file was saved
            tNode:= myFolder.NNodes[myFolder.SavedSelectedIndex].TVNode;    // *1

          if (tNode = nil) or (not TV.IsVisible[tNode]) then begin
            tNode := GetFirst;
            if not TV.IsVisible[tNode] then tNode:= GetNextNotHidden(tNode);
          end;
          SelectAlone(tNode);
        end;

        Log_StoreTick( 'After Restored selected node', 3 );

        // *2
        // Virtual TreeView bug? When loading some trees the control is not calculating
        // correctly the visible height, and the last node is partially visible. In that situation,
        // executing FullCollapse and then FullExpand corrects it, but also seems to be enough to
        // expand and collapse the first node (at least on this point in code)
        // Note: The tree is not populated in a virtual way, but it should not be a problem. I have only
        // detected with a single folder ("Knt Help")
        {  *2
        tNode:= TV.GetFirst();
        if tNode <> nil then begin
           TV.Expanded[tNode]:= True;
           TV.Expanded[tNode]:= False;
        end;
        }
        // Instead of the above lines to resolve *2, which works but not always, it seems that simply enclosing the
        // expanding settings between BeginUpdate and EndUpdate works and is probably better.

       TV.BeginUpdate;

        case KntTreeOptions.ExpandMode of
          txmFullCollapse : begin
            // nothing
          end;
          txmActiveNode : begin
            if assigned( FocusedNode ) then
              TV.Expanded[FocusedNode]:= True;
          end;
          txmTopLevelOnly, txmExact : begin
            try
               SetNodeExpandState((KntTreeOptions.ExpandMode = txmTopLevelOnly));
            except
              // nothing
            end;
          end;
          txmFullExpand : begin
            FullExpand;
          end;
        end;

        TV.EndUpdate;

        Form_Main.UpdateTreeVisible( myFolder ); // [f]

        if assigned(FocusedNode) then begin
          MakePathVisible(FocusedNode);
          fLastNodeSelected:= FocusedNode;
          TV.InvalidateNode(FocusedNode);
        end;


        for i := 0 to myFolder.NNodes.Count-1 do begin
           NNode:= myFolder.NNodes[i];
           if NNode.NodeBGColor <> clNone then
              TV.ReinitNode(NNode.TVNode, false);
        end;

        Log_StoreTick( 'After UpdateTreeVisible', 3 );
      end;
   end;
end;


procedure TKntTreeUI.SetupTreeHandlers;
begin
   txtFilter.OnChange := txtFilterChange;
   txtTags.OnEnter:= txtTagsEnter;
   TB_HideChecked.OnClick:= TB_HideCheckedClick;
   TB_FilterTree.OnClick:= TB_FilterTreeClick;
   OnResize := FrameResize;

   with TV do begin
     OnGetText:= TV_GetText;
     OnPaintText:= TV_PaintText;
     OnGetImageIndex := TV_GetImageIndex;
     OnGetImageText:= TV_GetImageText;
     OnBeforeCellPaint:= TV_BeforeCellPaint;


     OnKeyDown := TV_KeyDown;
     OnKeyPress := TV_KeyPress;
     //OnChange := TV_SelectionChange;          // selection change
     OnFocusChanged:= TV_FocusChanged;        // called when the focus goes to a new node and/or column

     OnChecked := TV_Checked;
     OnEditing := TV_Editing;
     OnCreateEditor:= TV_CreateEditor;
     OnEdited := TV_Edited;
     OnNewText:= TV_NewText;
     OnNodeCopying:= TV_NodeCopying;
     OnNodeMoving:= TV_NodeMoving;
     OnNodeMoved:= TV_NodeMoved;
     OnFreeNode:= TV_FreeNode;
     OnClick := TV_Click;
     OnDragDrop := TV_DragDrop;
     OnDragOver := TV_DragOver;
     OnEndDrag := TV_EndDrag;
     OnStartDrag := TV_StartDrag;
     OnGetHint := TV_GetHint;
     OnMouseMove:= TV_MouseMove;
     OnMouseLeave:= TV_MouseLeave;
     OnCompareNodes:= TV_CompareNodes;
     OnNodeClick:= TV_NodeClick;

     OnHeaderDrawQueryElements:= TV_HeaderCustomDrawTreeHeaderDrawQueryElements;
     OnAdvancedHeaderDraw:= TV_AdvancedHeaderDraw;
     OnHeaderDragged:= TV_HeaderDragged;
   end;

   TKntFolder(fFolder).NoteUI.SetOnEnter(NoteUIEnter);
end;

{$ENDREGION}



procedure TKntTreeUI.UpdateTreeChrome;
var
  Folder: TKntFolder;
  FontColor: TColor;
begin
   Folder:= TKntFolder(Self.Folder);

   TV.Color := Folder.TreeChrome.BGColor;
   FontInfoToFont(Folder.TreeChrome.Font, TV.Font);
   TV.Header.Font:= TV.Font;
   TV.Header.Font.Style:= [];

   FontColor:= DarkenColor(TV.Color, 90);
   if FontColor = TV.Color then
      FontColor:= LightenColor(Color, 90);
   TV.Header.Font.Color:= FontColor;

   TV.Colors.HotColor:= GetHotColorFor(TV.Color, TV.Font.Color);

   FDateColor:= LightenColor(TV.Color, 160);
   if FDateColor = clWhite then
      FDateColor:= DarkenColor(TV.Color, 160);

   TV.Invalidate;
end;


function TKntTreeUI.GetSizeOfColDate: integer;
var
  oldFont: TFont;
begin
  OldFont := TFont.Create();
  try
    OldFont.Assign(TV.Canvas.Font);
    with TV.Canvas.Font do begin
       Name:= 'Tahoma';
       Style:= [];
    end;
    Result:= TV.Canvas.TextWidth(FormatDateTime(FormatSettings.ShortDateFormat, EncodeDate(2024, 06, 06))) + 15;
    TV.Canvas.Font.Assign(OldFont);
  finally
    OldFont.Free();
  end;

end;

procedure TKntTreeUI.UpdateTreeColumns (ResizeCols: boolean = True);
var
  Folder: TKntFolder;
  NamePos: integer;

begin
   Folder:= TKntFolder(Self.Folder);

   NamePos:= 0;
   with TV.Header do begin
      With Columns[2] do begin                         // Flagged
         if Folder.PosFlaggedCol >= 1 then begin
            Options:= Options + [coVisible];
            Position:= Folder.PosFlaggedCol - 1;

            if Position = 0 then
               NamePos:= 1;
         end
         else
            Options:= Options - [coVisible]
      end;

      With Columns[1] do begin                        // Date
         if Folder.PosDateCol >= 1 then begin
            Options:= Options + [coVisible];
            Width:= GetSizeOfColDate;
            Position:= Folder.PosDateCol - 1;
            if Position = NamePos then
               NamePos:= Position + 1;
         end
         else
            Options:= Options - [coVisible]
      end;

      Columns[0].Position:= NamePos;
      if Folder.RTL then
         Columns[0].BidiMode := bdRightToLeft
      else
         Columns[0].BidiMode := bdLeftToRight;

      if (Folder.PosDateCol >= 1) or (Folder.PosFlaggedCol >= 1) then
         Options:= Options + [hoVisible]
      else
         Options:= Options - [hoVisible];

   end;

   if ResizeCols then
      TV_Resize(nil);
end;


function TKntTreeUI.AdditionalColumnsAreVisible: boolean;
begin
   Result:= (coVisible in TV.Header.Columns[1].Options) or (coVisible in TV.Header.Columns[2].Options);
end;


procedure TKntTreeUI.ShowFlaggedColumnForTagFilter (Show: boolean);
var
  FlaggedColumn: TVirtualTreeColumn;
  IncWidth: integer;
begin
  FlaggedColumn:= TV.Header.Columns[2];
  if Show = (coVisible in FlaggedColumn.Options) then exit;

  IncWidth:= 0;
  if Show then begin
     fShownFlaggedColumnForTagFilter:= True;
     FlaggedColumn.Position:= 0;
     FlaggedColumn.Options:= FlaggedColumn.Options + [coVisible];
     IncWidth:= FlaggedColumn.Width;
  end
  else
  if fShownFlaggedColumnForTagFilter then begin
     fShownFlaggedColumnForTagFilter:= false;
     FlaggedColumn.Options:= FlaggedColumn.Options - [coVisible];
     IncWidth:= - FlaggedColumn.Width;
  end;

  if AdditionalColumnsAreVisible then
     TV.Header.Options:= TV.Header.Options + [hoVisible]
  else
     TV.Header.Options:= TV.Header.Options - [hoVisible];

  Self.Width:= Self.Width + IncWidth;
end;


// This action will not be saved. To configure if any of the additional columns must be shown by default
// -> Folder properties..
procedure TKntTreeUI.ShowAdditionalColumns (Show: boolean);
var
   Folder: TKntFolder;
   CurrentlyConfigAsVisible: boolean;
   incWidth: integer;
   i: integer;
   PosCol: array [1..2] of integer;

begin
   if Show = AdditionalColumnsAreVisible then exit;

   Folder:= TKntFolder(Self.Folder);

   incWidth:= 0;
   PosCol[1]:= Folder.PosDateCol;
   PosCol[2]:= Folder.PosFlaggedCol;

   CurrentlyConfigAsVisible:= (PosCol[1] > 0) or (PosCol[2] > 0);

   for i:= 1 to 2 do begin
      if Show then begin
         if not CurrentlyConfigAsVisible or (PosCol[i] > 0) then begin
            if i = 1 then
               TV.Header.Columns[i].Width:= GetSizeOfColDate;
            inc(incWidth, TV.Header.Columns[i].Width);
            TV.Header.Columns[i].Options:= TV.Header.Columns[i].Options + [coVisible];
         end;
      end
      else begin
         if coVisible in TV.Header.Columns[i].Options then begin
            dec(incWidth, TV.Header.Columns[i].Width);
            TV.Header.Columns[i].Options:= TV.Header.Columns[i].Options - [coVisible];
         end;
      end;
   end;

   if not CurrentlyConfigAsVisible then
      TV.Header.Columns[2].Position:= 0;

   if Show then
      TV.Header.Options:= TV.Header.Options + [hoVisible]
   else
      TV.Header.Options:= TV.Header.Options - [hoVisible];

   Self.Width:= Self.Width + IncWidth;
end;


function TKntTreeUI.CheckReadOnly: boolean;
begin
    Result:= False;

    if ReadOnly then begin
       App.ShowInfoInStatusBar(Format(GetRS(sTree52), [TKntFolder(Self.Folder).Name]));
       Result:= True;
       exit;
    end;
end;


function TKntTreeUI.GetShowAllCheckboxes: boolean;
begin
   Result:= TKntFolder(Folder).Checkboxes;
end;


function TKntTreeUI.GetHideCheckedNodes: boolean;
begin
   Result:= TKntFolder(Folder).HideCheckedNodes;
end;


function TKntTreeUI.GetIconKind : TNodeIconKind;
begin
   Result:= TKntFolder(Folder).IconKind;
end;


function TKntTreeUI.IsEditing: boolean;
begin
   Result:= TV.IsEditing;
end;


function TKntTreeUI.GetFocused: boolean;
begin
   Result:= TV.Focused;
end;



// Nodes, NNodes. Help methods  ================================

{$REGION Nodes, NNodes. Help methods}


function TKntTreeUI.GetFirstNode: PVirtualNode;
begin
   Result:= TV.GetFirst;
end;


function TKntTreeUI.GetFocusedNode: PVirtualNode;
begin
   Result:= TV.FocusedNode;
end;


procedure TKntTreeUI.SetFocusedNode(value: PVirtualNode);
begin
   TV.FocusedNode:= value;
end;


procedure TKntTreeUI.SelectAlone(Node: PVirtualNode);
begin
   TV.FocusedNode:= Node;
   TV.ClearSelection;
   TV.Selected[Node] := True;
end;


function TKntTreeUI.GetNNode(Node: PVirtualNode): TNoteNode;
begin
   Result:= TV.GetNodeData<TNoteNode>(Node);
end;

procedure TKntTreeUI.SetNNode(Node: PVirtualNode; NNode: TNoteNode);
begin
   TV.SetNodeData<TNoteNode>(Node, NNode);
end;


function TKntTreeUI.GetFocusedNNode: TNoteNode;
var
   FocusedNode: PVirtualNode;
begin
   FocusedNode:= TV.FocusedNode;
   if assigned(FocusedNode) then
      Result:= GetNNode(FocusedNode)
   else
      Result:= nil;
end;

{$ENDREGION }



// General TreeView Handlers ================================

{$REGION TreeView Handlers}

procedure TKntTreeUI.TV_HeaderDragged(Sender: TVTHeader; Column: TColumnIndex; OldPosition: Integer);
begin
  if not Assigned(fTimer) then begin
    fTimer := TTimer.Create(Self);
    fTimer.Interval := 10;                // Short delay to allow time for internal actions to complete
    fTimer.OnTimer := SimulateDoubleClick;
  end;

  fTimer.Tag := Column;
  fTimer.Enabled := True;
end;


procedure TKntTreeUI.SimulateDoubleClick(Sender: TObject);
var
  P: TPoint;
  LP: LPARAM;
  ColumnIndex: Integer;
begin
  ColumnIndex := fTimer.Tag;
  fTimer.Free;                   // Stop and delete the timer
  fTimer:= nil;
  P:= TV.Header.Columns[ColumnIndex].GetRect.CenterPoint;
  LP := MakeLParam(P.X, P.Y);
  TV.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LP);
  TV.Perform(WM_LBUTTONUP, 0, LP);

  TV.Perform(WM_LBUTTONDOWN, MK_LBUTTON, LP);
  TV.Perform(WM_LBUTTONUP, 0, LP);

  TV_Resize(nil);

  with TV.Header do begin
     if coVisible in Columns[1].Options then
        TKntFolder(Folder).PosDateCol:= Columns[1].Position + 1;
     if coVisible in Columns[2].Options then
        TKntFolder(Folder).PosFlaggedCol:= Columns[2].Position + 1;
  end;
end;


procedure TKntTreeUI.TV_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Date: TDateTime;
begin
  // Column: main column: -1 if columns are hidden, 0 if they are shown

  case Column of
     -1, 0: CellText:= GetNodeCaption(Node);          // Name column (main)
         1: begin                                     // Date column
            Date:= GetNNode(Node).Note.DateCreated;
            if Date = 0 then
               CellText:= ''
            else
               CellText:= FormatDateTime( FormatSettings.ShortDateFormat, Date);
         end;
         2: CellText:= '';                           // Flagged column
  end;

end;


procedure TKntTreeUI.TV_NodeClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
   with HitInfo do begin
       if HitColumn = 2 then
          ToggleNodeFlagged(HitNode);
   end;
end;


procedure TKntTreeUI.TV_AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
  const Elements: THeaderPaintElements);
var
  Color: TColor;
begin
  with PaintInfo do begin
    // First check the column member. If it is NoColumn then it's about the header background.
//    if Column = nil then begin
//      if hpeBackground in Elements then begin
//        TargetCanvas.Brush.Color := TV.Color;// clBackground;
//        TargetCanvas.FillRect(PaintRectangle);
//      end;
//    end
//    else begin
        Color:=     DarkenColor(TV.Color, 10);
        if Color = TV.Color then
           Color:= LightenColor(Color, 15);
        TargetCanvas.Brush.Color := Color;
        TargetCanvas.FillRect(PaintRectangle);
//    end;
  end;

end;


procedure TKntTreeUI.TV_HeaderCustomDrawTreeHeaderDrawQueryElements(Sender: TVTHeader;
                                                                    var PaintInfo: THeaderPaintInfo;
                                                                    var Elements: THeaderPaintElements);
// This event tells the tree which part we want to draw ourselves. Don't forget to enable custom drawing in the header
// options and switch the Style property of every column, which we handle here to vsOwnerDraw.
begin

  with PaintInfo do begin
    if Column = nil then
       Elements := [hpeBackground] // No other flag is recognized for the header background.
    else begin
      Elements := [hpeBackground{, hpeText, hpeDropMark, hpeHeaderGlyph, hpeSortGlyph}];
      // Using the index here ensures a column, regardless of its position, always has the same draw style.
      // By using the Position member, we could make a certain column place stand out, regardless of the column order.
      // Don't forget to change the AdvancedHeaderDraw event body accordingly after you changed the indicator here.
      (*
      case Column.Index of
        0: // Default drawing.       - Name
          ;
        1: // Full customization (well, quite).  - Date
          Elements := [hpeBackground, hpeText{, hpeDropMark, hpeHeaderGlyph, hpeSortGlyph}];
        2: // Background only customization.     - Flagged
          Include(Elements, hpeBackground);
      end;
      *)
    end;
  end;
end;


procedure TKntTreeUI.TV_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
                                        Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NNode: PNoteNode;
  CellR: TRect;
  T: integer;
  InheritedParentTags: TNoteTagArray;
  iNode: integer;
begin
  NNode := Sender.GetNodeData(Node);

  if (Column = 2) then begin
     if TagFilterApplied then begin
        InheritedParentTags:= nil;
        if FindOptions.InheritedTags then begin
           iNode:= fParentNodesWithInheritedTags.IndexOf(Node.Parent);
           if iNode >= 0 then
              InheritedParentTags:= fInheritedTags[iNode];
        end;
        if NNode.MatchesTags(fFindTags, InheritedParentTags) then begin
           TargetCanvas.Brush.Color :=  clWebLightCyan;
           TargetCanvas.FillRect(CellRect);
        end;
     end;
     if NNode.Flagged then begin
        T:= (CellRect.Height - Form_Main.IMG_TV.Height) div 2;         // Center vertically
        Form_Main.IMG_TV.Draw(TargetCanvas, CellRect.Left, T, 10);
     end;
     exit;
  end;

  if CellPaintMode = cpmGetContentMargin then exit;

  if fTreeFilterApplied and NNode.TreeFilterMatch then begin
     TargetCanvas.Brush.Color := clWebLemonChiffon;
     TargetCanvas.FillRect(ContentRect);
  end;

  if vsSelected in Node.States then exit;

  if (NNode.NodeBGColor <> clNone) and (Column <= 0) then begin
     CellR:= ContentRect;
     CellR.Width:= Sender.GetDisplayRect(Node,Column,True,   True).Width;
     TargetCanvas.Brush.Color := NNode.NodeBGColor;
     TargetCanvas.FillRect(CellR);
  end;

end;


procedure TKntTreeUI.TV_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);

var
  NNode: TNoteNode;
  Color: TColor;

begin
  NNode:= GetNNode(Node);

  if nnsBold in NNode.States then
     TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];

  Color:= clNone;
  if fFindFilterApplied and NNode.FindFilterMatch and not (TV.Selected[Node] and (Column <= 0)) then
     Color:= clBlue
  else
     Color:= NNode.NodeColor;

  if Color <> clNone then
     TargetCanvas.Font.Color := Color;
  if NNode.NodeFontFace <> '' then
     TargetCanvas.Font.Name := NNode.NodeFontFace;

  if Column = 1 then begin                          // Date column
     TargetCanvas.Font.Name:= 'Tahoma';
     TargetCanvas.Font.Style:= [];
     TargetCanvas.Font.Color := FDateColor;
  end;
end;



procedure TKntTreeUI.TV_GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
   NNode: TNoteNode;
begin
  if Column > 0 then exit;

  if not (Kind in [TVTImageKind.ikNormal, TVTImageKind.ikSelected]) then exit;

  NNode:= GetNNode(Node);
  case IconKind of

    niStandard : begin
      if NNode.IsVirtual then
         ImageIndex := ICON_VIRTUAL

      else begin
          if (NNode.Note.NumNNodes > 1) and (NNode <> NNode.Note.NNodes[0].NNode) then
             ImageIndex := ICON_VIRTUAL_KNT_NODE
          else
          if Node.ChildCount > 0 then begin
            if Sender.GetNodeLevel(Node) > 0 then
               ImageIndex := ICON_BOOK
            else
               ImageIndex := ICON_FOLDER;
          end
          else
             ImageIndex := ICON_NOTE;
      end;

      if Kind = TVTImageKind.ikSelected then
        inc(ImageIndex);
    end;

    niCustom : begin
      ImageIndex := NNode.ImageIndex;
    end;

    niNone : begin
      ImageIndex := -1;
    end;
  end;

end;


procedure TKntTreeUI.TV_GetImageText(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var ImageText: string);
var
   NNode: TNoteNode;
begin
  if Column = 2 then begin
     NNode:= GetNNode(Node);
     if NNode.Flagged then
        ImageText:= GetRS(sTree66);  // "Flagged"
  end;
end;



procedure TKntTreeUI.TV_FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if (not assigned(Node)) then exit;

  TV.ScrollIntoView(Node, false);

  if (Node <> fLastNodeSelected) then begin
     TKntFolder(Folder).NodeSelected(Node, fLastNodeSelected);
     fLastNodeSelected:= Node;
     App.NNodeFocused(GetNNode(Node));
  end;
end;


//procedure TKntTreeUI.TV_SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
//begin
//end;


procedure TKntTreeUI.TV_Checked(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   ChangeCheckedState(Node, Node.CheckState = csCheckedNormal);
end;


procedure TKntTreeUI.TV_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ptCursor : TPoint;
begin
  if TV.IsEditing then exit;

  case key of
    VK_F10 : if ( shift = [ssShift] ) then begin
      key := 0;
      GetCursorPos( ptCursor );
      TV.PopupMenu.Popup(ptCursor.X, ptCursor.Y);
    end;

    VK_SPACE, VK_F2 : if ( Shift = [] ) then begin
      key := 0;
      RenameFocusedNode;
    end;

    220 : if ( Shift = [ssCtrl] ) then begin // backslash
      Key := 0;
      TKntFolder(Folder).SetFocusOnNoteEditor;
    end;

  end;

end;


procedure TKntTreeUI.TV_KeyPress(Sender: TObject; var Key: Char);
begin
  if TV.IsEditing then begin
     if ( key = KNTLINK_SEPARATOR ) then
        key := #0; // disallow | character in node name, because it breaks KNT links (which use | as delimiter)
  end;
end; // TVKeyPress

{
procedure TKntTreeUI.TV_SavingTree(Sender: TObject; Node: PVirtualNode; var S: string);
var
   NNode: TNoteNode;
begin
   if not ShowHiddenMarkers then exit;

   NNode:= GetNNode(Node);
   S:= Format('%s [%u]', [S, NNode.GID]);
   if NNode.ID > 0 then
      S:= S + Format('(id:%d)', [NNode.ID]);
end;
}

procedure TKntTreeUI.SaveToFile(const FileName: TFileName);
var
  SaveTree: TVTGetNodeProc;
  NNode: TNoteNode;
  tf: TTextFile;

begin
  SaveTree :=
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    var
       Indent, S: string;
    begin
      NNode:= GetNNode(Node);
      Indent:= StringOfChar(#9, TV.GetNodeLevel(Node));
      if ShowHiddenMarkers then begin
         S:= Format(' [%u]', [NNode.GID]);
         if NNode.ID > 0 then
            S:= S + Format('(id:%d)', [NNode.ID]);
      end;
      tf.WriteLine(Indent + NNode.NodeName(Self) + S, true );
    end;

    tf:= TTextFile.Create();
    tf.assignfile(FileName);
    try
      tf.rewrite();
      tf.Write(UTF8_BOM[1], length(UTF8_BOM));
      TV.IterateSubtree(nil, SaveTree, nil);
    finally
      tf.closefile();
    end;
end;



procedure TKntTreeUI.TV_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  NNode1, NNode2: TNoteNode;
begin
  NNode1:= GetNNode(Node1);
  NNode2:= GetNNode(Node2);
  Result := CompareText(NNode1.NodeName(Self), NNode2.NodeName(Self));
end;


{$ENDREGION}


// ShowOrHideIcons,  NodeColor, NodeBold, NodeCustomImage, NodeFontFace,  ==============================

{$REGION ShowOrHideIcons,  NodeColor, NodeBold, NodeCustomImage, NodeFontFace}

procedure TKntTreeUI.ShowOrHideIcons;
var
  myTreeNode : PVirtualNode;
begin
  TV.BeginUpdate;
  try
    case IconKind of
       niNone :     TV.Images := nil;
       niStandard : TV.Images := Form_Main.IMG_TV;
       niCustom :   TV.Images := Chest.IMG_Categories;
    end;

    TV.Invalidate;

  finally
    TV.EndUpdate;
  end;
end; // ShowOrHideIcons


procedure TKntTreeUI.SetNodeColor(Node: PVirtualNode; NodeColor: TColor; AsTextColor, DoChildren : boolean);

   procedure SetColor(Node: PVirtualNode);
   var
     NNode : TNoteNode;
   begin
     NNode:= GetNNode(Node);
     if AsTextColor then
        NNode.NodeColor:= NodeColor
     else
        NNode.NodeBGColor:= NodeColor;

     if DoChildren and (vsHasChildren in Node.States) then
         for Node in TV.ChildNodes(Node) do
            SetColor(Node);
   end;

begin
  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  try
     SetColor(Node);
     TV.InvalidateNode(Node);
     if DoChildren then
        TV.InvalidateChildren(Node, true);

  finally
     TKntFolder(Folder).Modified:= true;
  end;

end;


procedure TKntTreeUI.SetNodeColor(const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean);
var
  TVSelectedNodes: TNodeArray;
  Node: PVirtualNode;
  NNode: TNoteNode;
  NodeColor, DefaultColor, NewColor: TColor;
  i: integer;
begin
  if CheckReadOnly then exit;
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  NNode:= GetNNode(Node);

  if AsTextColor then begin
     NodeColor:= NNode.NodeColor;
     DefaultColor:= TV.Font.Color;
     NewColor:= Form_Main.TB_Color.ActiveColor;
  end
  else begin
     NodeColor:= NNode.NodeBGColor;
     DefaultColor:= TV.Color;
     NewColor := Form_Main.TB_Hilite.ActiveColor;
  end;

  if UseColorDlg then begin
    NodeColor:= NNode.NodeColor;
    if NodeColor = clNone then
       NodeColor := DefaultColor;

    Form_Main.ColorDlg.Color:= NodeColor;
    if Form_Main.ColorDlg.Execute then begin
       NodeColor := Form_Main.ColorDlg.Color;
       if ColorToRGB(NodeColor) = ColorToRGB(DefaultColor) then
          NodeColor:= clNone;
    end
    else
       exit;
  end
  else begin
    if ResetDefault then
       NodeColor := clNone
    else
       NodeColor := NewColor;
  end;

  if TV.SelectedCount = 0 then
     SetNodeColor(Node, NodeColor, AsTextColor, DoChildren)

  else
     if DoChildren then begin
        TVSelectedNodes:= TV.GetSortedSelection(True);
        for i := 0 to High(TVSelectedNodes) do
           SetNodeColor(TVSelectedNodes[i], NodeColor, AsTextColor, DoChildren)
     end
     else
        for Node in TV.SelectedNodes() do
           SetNodeColor(Node, NodeColor, AsTextColor, DoChildren);
end;


procedure TKntTreeUI.ToggleNodeFlagged(Node: PVirtualNode);
var
  Flagged: boolean;
  OnlyNodeIn: boolean;
  NNode: TNoteNode;
  i: integer;

   procedure SetNodeFlagged;
   begin
     NNode:= GetNNode(Node);
     NNode.Flagged:= Flagged;
     TV.InvalidateNode(Node);
   end;

begin
  if CheckReadOnly then exit;

  OnlyNodeIn:= (Node <> nil);
  if Node = nil then
     Node:= TV.FocusedNode;

  if Node = nil then exit;

  if ShiftDown then begin
      TV.ClearSelection;
      if App.DoMessageBox (GetRS(sTree59), mtWarning, [mbYes,mbNo]) = mrYes then begin
        fNNodesFlagged:= False;
        for i := 0 to TKntFolder(Folder).NNodes.Count-1 do
           TKntFolder(Folder).NNodes[i].Flagged:= False;
      end;
      exit;
  end;


  Flagged:= not GetNNode(Node).Flagged;
  if OnlyNodeIn or (TV.SelectedCount = 0) then
     SetNodeFlagged
  else
     for Node in TV.SelectedNodes() do
        SetNodeFlagged;

  TKntFolder(Folder).Modified:= true;

   fChangesInFlagged:= true;
   fNNodesFlagged:= False;

   if Flagged then
      fNNodesFlagged:= True

   else begin
      for i := 0 to TKntFolder(Folder).NNodes.Count-1 do begin
         if TKntFolder(Folder).NNodes[i].Flagged then begin
            fNNodesFlagged:= True;
            break;
         end;
      end;
   end;


   if CtrlDown then begin
      if Flagged then begin
         fTreeFilterApplied:= false;
         txtFilter.Text:= '';
         txtTags.Text:= '';
         fFindTags:= nil;
      end;
      FilterOutUnflagged(Flagged);
   end;

end;


procedure TKntTreeUI.SetNodeBold(Node: PVirtualNode; Bold: boolean; const DoChildren : boolean);

   procedure BoldNNode (Node: PVirtualNode);
   var
     NNode : TNoteNode;
   begin
     NNode:= GetNNode(Node);
     NNode.Bold := Bold;
     if DoChildren and (vsHasChildren in Node.States) then
         for Node in TV.ChildNodes(Node) do
            BoldNNode(Node);
   end;

begin
  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  BoldNNode(Node);
  TV.InvalidateNode(Node);
  if DoChildren then
    TV.InvalidateChildren(Node, true);
  TKntFolder(Folder).Modified:= true;
end;


procedure TKntTreeUI.SetNodeBold(const DoChildren : boolean);
var
  TVSelectedNodes: TNodeArray;
  Node: PVirtualNode;
  Bold: boolean;
  i: integer;
begin
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  Bold:= not GetNNode(Node).Bold;

  if TV.SelectedCount = 0 then
     SetNodeBold(Node, Bold, DoChildren)

  else
     if DoChildren then begin
        TVSelectedNodes:= TV.GetSortedSelection(True);
        for i := 0 to High(TVSelectedNodes) do
           SetNodeBold(TVSelectedNodes[i], Bold, DoChildren)
     end
     else
        for Node in TV.SelectedNodes() do
           SetNodeBold(Node, Bold, DoChildren);
end;


procedure TKntTreeUI.SetNodeCustomImage(Node: PVirtualNode; NewIdx: Integer; DoChildren: Boolean);
var
  ImgIdx : integer;
  NNode : TNoteNode;

   procedure SetCustomImage (Node: PVirtualNode);
   begin
     NNode:= GetNNode(Node);
     NNode.ImageIndex := NewIdx;
     if DoChildren and (vsHasChildren in Node.States) then
         for Node in TV.ChildNodes(Node) do
            SetCustomImage(Node);
   end;

begin
  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  NNode:= GetNNode(Node);
  ImgIdx := NNode.ImageIndex;

  if NewIdx < 0 then begin
     DoChildren := Node.ChildCount > 0;
     NewIdx := PickImage(ImgIdx, DoChildren);
  end;

  if (NewIdx = -1) or ((NewIdx = ImgIdx) and not DoChildren) then exit;

  SetCustomImage(Node);
  TV.InvalidateNode(Node);
  if DoChildren then
    TV.InvalidateChildren(Node, true);
  TKntFolder(Folder).Modified:= true;
end;


procedure TKntTreeUI.SetNodeCustomImage;
var
  TVSelectedNodes: TNodeArray;
  Node: PVirtualNode;
  DoChildren: boolean;
  NewIdx, ImgIdx: integer;
  i: integer;
begin
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  ImgIdx := GetNNode(Node).ImageIndex;
  DoChildren := (Node.ChildCount > 0) or (TV.SelectedCount > 0);
  NewIdx := PickImage(ImgIdx, DoChildren);
  if (NewIdx = -1) then exit;


  if TV.SelectedCount = 0 then
     SetNodeCustomImage(Node, NewIdx, DoChildren)

  else
     if DoChildren then begin
        TVSelectedNodes:= TV.GetSortedSelection(True);
        for i := 0 to High(TVSelectedNodes) do
           SetNodeCustomImage(TVSelectedNodes[i], NewIdx, DoChildren)
     end
     else
        for Node in TV.SelectedNodes() do
           SetNodeCustomImage(Node, NewIdx, DoChildren);
end;



function TKntTreeUI.GetNodeFontFace (Node: PVirtualNode): string;
var
   NNode: TNoteNode;

begin
   Result:= '';
   if not assigned(Node) then exit;

   NNode:= GetNNode(Node);
   Result:= NNode.NodeFontFace;
   if Result = '' then
      Result:= TV.Font.Name;
end;


procedure TKntTreeUI.SetNodeFontFace(Node: PVirtualNode; const FontFace : string; const DoChildren: boolean);
var
  SetFontInSubtree: TVTGetNodeProc;

begin

  SetFontInSubtree :=
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    begin
      Abort := not DoChildren;                           // Continue iteration?
      GetNNode(Node).NodeFontFace:= FontFace;
    end;

  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  try
    TV.IterateSubtree(Node, SetFontInSubtree, nil);
    TV.InvalidateNode(Node);
    if DoChildren then
       TV.InvalidateChildren(Node, true);

  finally
    TKntFolder(Folder).Modified:= true;
  end;

end; // SetNodeFontFace


procedure TKntTreeUI.SetNodeFontFace(const ResetDefault, DoChildren : boolean);
var
  TVSelectedNodes: TNodeArray;
  Node: PVirtualNode;
  FontFace : string;
  i: integer;
begin
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  FontFace:= '';
  if not ResetDefault then
     FontFace := Form_Main.Combo_Font.FontName;


  if TV.SelectedCount = 0 then
     SetNodeFontFace(Node, FontFace, DoChildren)

  else
     if DoChildren then begin
        TVSelectedNodes:= TV.GetSortedSelection(True);
        for i := 0 to High(TVSelectedNodes) do
           SetNodeFontFace(TVSelectedNodes[i], FontFace, DoChildren)
     end
     else
        for Node in TV.SelectedNodes() do
           SetNodeFontFace(Node, FontFace, DoChildren);
end;

//procedure TKntTreeUI.SetNodeFontSize(TreeNode: PVirtualNode; const ResetDefault, DoChildren : boolean);
//var
//  myTreeNode : PVirtualNode;
//  myNote : TNoteNode;
//begin
//  myTreeNode:= TreeNode;
//  if not Assigned(myTreeNode) then
//     myTreeNode := TV.FocusedNode;
//  if not Assigned(myTreeNode) or CheckReadOnly then exit;
//
//  myNote := GetNNode(myTreeNode);
//  try
//    myTreeNode.Font.Size := strtoint(Form_Main.Combo_FontSize.Text);
//    if (myTreeNode.Font.Size >= (TV.ItemHeight -4)) then
//       TV.ItemHeight := myTreeNode.Font.Size+6;
//  except
//  end;
//
//end;

{$ENDREGION }


// NodePath, NodeName:  Copy, Paste, Rename

{$REGION NodePath, NodeName }

function TKntTreeUI.GetNodePath(aNode: PVirtualNode; const aDelimiter: string; const TopToBottom: boolean) : string;
var
  s : string;
  NNode : TNoteNode;
begin
  result := '';

  while assigned(aNode) and (aNode <> TV.RootNode) do begin
    NNode := GetNNode(aNode);

    if (s = '') then
      s := NNode.NoteName
    else begin
      case TopToBottom of
        false : begin
          s := s + aDelimiter + NNode.NoteName;
        end;
        true : begin
          s := NNode.NoteName + aDelimiter + s;
        end;
      end;
    end;
    aNode := aNode.Parent;
  end;

  result := s;

end; // GetNodePath


function TKntTreeUI.GetNodeAncestorsPath(aNode: PVirtualNode; const aDelimiter: string;
                                         MaxDepth: integer; StartAtLevel: integer = 1) : string;
var
  s : string;
  NNode : TNoteNode;
  Level: integer;
  Len: integer;
  CheckTruncate: boolean;
begin
  result := '';

  CheckTruncate:= True;
  Len:= 0;
  if assigned(aNode) then
     Level:= TV.GetNodeLevel(aNode) + 1;

  while assigned(aNode) and (aNode <> TV.RootNode) and (Level >= StartAtLevel) do begin
    NNode := GetNNode(aNode);

    if CheckTruncate and ((Level - StartAtLevel + 1) <= MaxDepth) then begin
       Len:= Length(s);
       CheckTruncate:= false;
    end;

    if (s = '') then
       s := NNode.NoteName
    else begin
       s := NNode.NoteName + aDelimiter + s;
    end;

    aNode := aNode.Parent;
    dec(Level);
  end;

  if Len > 0 then
     s:= Copy(s, 1, Length(s)-Len-Length(aDelimiter));
  result := s;

end; // GetNodeAncestorsPath


procedure TKntTreeUI.CopyNodePath(myTreeNode : PVirtualNode; const InsertInEditor : boolean);
var
  s : string;
begin
  if not Assigned(myTreeNode) then
     myTreeNode := TV.FocusedNode;
  if not Assigned(myTreeNode) then exit;

  s := GetNodePath(myTreeNode, KntTreeOptions.NodeDelimiter, KntTreeOptions.PathTopToBottom);
  ClipBoard.SetTextBuf(PChar(s));
  if InsertInEditor then begin
    ActiveEditor.SelText := s + ' ';
    ActiveEditor.SelLength := 0;
  end
end; // CopyNodePath


procedure TKntTreeUI.PasteNodeName(Node : PVirtualNode; const PasteMode : TPasteNodeNameMode);
var
  myNewName: string;
  p : integer;
  s : string;
  NNode: TNoteNode;
begin
  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  myNewName := '';

  case PasteMode of
    pnnClipboard :  myNewName := Trim(Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH));
    pnnDate :       myNewName := FormatDateTime(KeyOptions.DateFmt, now);
    pnnTime :       myNewName := FormatDateTime(KeyOptions.TimeFmt, now);
    pnnDateTime :   myNewName := FormatDateTime(KeyOptions.DateFmt + #32 + KeyOptions.TimeFmt, now);
    pnnSelection : begin
      if (ActiveEditor.SelLength = 0) then begin
        App.WarnNoTextSelected;
        exit;
      end;
      s := ActiveEditor.SelVisibleText;
      p := Pos(#13, s);
      if (p > 0) then
        delete(s, p, length(s));
      myNewName := s;
      myNewName := trim(myNewName);
    end;
  end;

  if (myNewName <> '') then begin
    try
       NNode:= GetNNode(Node);
       NNode.Note.Name:= myNewName;
       TV.InvalidateNode(Node);
    finally
      TKntFolder(Folder).Modified:= true;
    end;
  end;

end; // PasteNodeName


procedure TKntTreeUI.CopyNodeName(const IncludeNoteText : boolean);
var
  Node : PVirtualNode;
  NNode: TNoteNode;
begin
  Node:= TV.FocusedNode;
  if (not assigned(Node)) then exit;

  NNode:= GetNNode(Node);
  if IncludeNoteText then
     ClipBoard.AsText:=  NNode.NodeName(Self) + #13#13 + RemoveKNTHiddenCharactersInText(ActiveEditor.Text)
  else
     ClipBoard.AsText:= NNode.NodeName(Self);
end;


procedure TKntTreeUI.RenameFocusedNode;
var
  Node: PVirtualNode;
  NNode: TNoteNode;
  myNote : TNote;
  myName : string;
  EditInPlace: boolean;
begin
  if CheckReadOnly then exit;

  Node:= TV.FocusedNode;
  if Node = nil then exit;
  NNode:= GetNNode(Node);
  if NNode = nil then exit;

  EditInPlace:= KntTreeOptions.EditInPlace;

  if not Focused  then
     EditInPlace:= false;

  if EditInPlace then
     TV.EditNode(Node, TV.Header.MainColumn )

  else begin
     myNote := NNode.Note;
     myName := myNote.Name;
     if InputQuery( GetRS(sTree53), GetRS(sTree54), myName ) then begin
        myName := trim( myName );
        if (myName <> '') then begin
           myNote.Name := myName;
           TV.InvalidateNode(Node);
           TKntFolder(Folder).Modified := true;
        end
        else
           App.ErrorPopup(GetRS(sTree50));
     end;
  end;

end;


procedure TKntTreeUI.TV_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := not CheckReadOnly;
end;

{ Never called in VirtualTree...
  Suppose related with issue #896 (https://github.com/JAM-Software/Virtual-TreeView/issues/896):
    Merge DoCancelEdit() and DoEndEdit() in one procedure with boolean parameter
procedure TKntTreeUI.TVEditCanceled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  TV.PopupMenu := PopupMenu;
end;
}

procedure TKntTreeUI.TV_CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   // To create and use the generic tag editor. This handler lets us know when editing is started, to disable 
   // the context menu, and avoid interference from associated shortcuts.
   // We can't use TV_Editing because the OnEditing event is called from DoCanEdit, not DoEdit, and what it's
   // looking for is to know if editing is allowed.
    EditLink:= nil;

    TV.PopupMenu := nil;                   // stop menu events triggered by shortcut keys
end;


// Called in VirtualTree, when edit ended ok and also when was cancelled..
procedure TKntTreeUI.TV_Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TV.PopupMenu := PopupMenu;               // Restore menu -> resume menu events triggered by shortcut keys
  if GetNNode(Node).NumberingMethod <> NoNumbering then
     TV.ReinitNode(Node, false);
  TV_Resize(nil);
end;


procedure TKntTreeUI.TV_NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    NewText: string);
begin
  NewText := Trim(copy(NewText, 1, TREENODE_NAME_LENGTH));

  if (NewText = '' ) then begin
    App.ShowInfoInStatusBar(GetRS(sTree50));
    exit;
  end;

  _ALLOW_VCL_UPDATES := false;
  try
    GetNNode(Node).Note.Name:= NewText;        // {N} must add outline numbering, if any
    App.ShowInfoInStatusBar(GetRS(sTree51));
    TKntFolder(Folder).Modified := true;
  finally
    _ALLOW_VCL_UPDATES := true;
  end;
end;


{$ENDREGION }


// Outline numbering

{$REGION Outline numbering }


function TKntTreeUI.GetNodeCaption(Node: PVirtualNode): string;
var
   NNode: TNoteNode;
begin
  // TV.PopupMenu = nil => Is editing node caption

  Result:= '';
  NNode:= GetNNode(Node);
  if (NNode.NumberingMethod <> NoNumbering) and not ((TV.PopupMenu = nil) and (vsSelected in Node.States)) then
     Result:= OutlineNumber(Node);     // It can be '' if the set depth is exceeded

  if (nnsShowOutlineNumberAndName in NNode.States) or (Result = '') then begin
     if (Result <> '') then
        Result:= Result + ' - ';
     Result:= Result + NNode.NoteName;
  end;

end;


procedure TKntTreeUI.SetNumberingDepthLimit(value: Byte);
begin
    fNumberingDepthLimit:= value;
    TV.ReinitChildren(nil, true);
    TV.Invalidate;
end;


procedure TKntTreeUI.SetNumberingMethod (Node: PVirtualNode);
var
  RefNode: PVirtualNode;
  NNode: TNoteNode;
  OldNumb: TNumberingMethod;
begin
   assert(Node <> nil);

   RefNode:= Node.PrevSibling;
   if (RefNode = nil) or (GetNNode(RefNode) = nil) then
      RefNode:= Node.NextSibling;
   if (RefNode = nil) or (GetNNode(RefNode) = nil) then
      RefNode:= Node.Parent;

   NNode:= GetNNode(Node);
   OldNumb:= NNode.NumberingMethod;

   if (RefNode <> nil) and (RefNode <> TV.RootNode) and (GetNNode(RefNode) <> nil) then
      NNode.NumberingMethod:= GetNNode(RefNode).NumberingMethod
   else
      NNode.NumberingMethod:= NoNumbering;

   if OldNumb <> NNode.NumberingMethod then
      TV.ReinitNode(Node, false);
end;


function TKntTreeUI.OutlineNumber(Node: PVirtualNode): string;
var
   NNode: TNoteNode;

   function GetOutlineNumberDepth (Node: PVirtualNode): integer;
   var
      ParentNNode: TNoteNode;
   begin
      Result:= 0;
      repeat
         Node:= Node.Parent;
         inc(Result);
      until (Node = TV.RootNode) or GetNNode(Node).CustomNumberingSubtree;
   end;

   function GetFormatStr(MaxValue: Cardinal): string;
   begin
      if MaxValue < 10 then
         Result:= '%d'
      else
      if MaxValue < 100 then
         Result:= '%.2d'
      else
         Result:= '%.3d'
   end;

begin
   NNode:= GetNNode(Node);
   Result:= '';
   if (NNode.NumberingMethod = NoNumbering) or (GetOutlineNumberDepth(Node) > fNumberingDepthLimit) then exit;

   if assigned(Node.Parent) and (Node.Parent <> TV.RootNode) then
      Result:= OutlineNumber(Node.Parent);

   if Result <> '' then
      Result:= Result + '.';

   Result:= Result + Format(GetFormatStr(Node.Parent.ChildCount), [succ(Node.Index)]);
end;


procedure TKntTreeUI.OutlineNumberNodes;
var
  Form_NodeNum : TForm_NodeNum;
  StartNode: PVirtualNode;
  StartNNode: TNoteNode;
  SubtreeOnly : boolean;
  SetNumberingInSubtree: TVTGetNodeProc;
  Numbering, BakNumbering: TNumberingMethod;

begin
  if CheckReadOnly then exit;
  if (TV.TotalCount = 0) then exit;

  Form_NodeNum := TForm_NodeNum.Create(Form_Main);
  try
      if (Form_NodeNum.ShowModal <> mrOK) then exit;

      with Form_NodeNum do begin
        SubtreeOnly := (RG_Scope.ItemIndex = 0);
        if SubtreeOnly then begin
           StartNode := TV.FocusedNode;
           StartNNode:= GetNNode(StartNode);
        end
        else begin
           StartNode := nil;
           StartNNode:= nil;
        end;
        if CB_FullDepth.Checked then
           fNumberingDepthLimit := 255                // add numbers to all levels
        else
           fNumberingDepthLimit := Spin_Depth.Value;  // descend only DepthLimit levels

       Numbering:= TNumberingMethod(RG_Method.ItemIndex);
      end;

      if (SubtreeOnly and not assigned(StartNode)) then begin
        App.ErrorPopup(GetRS(sTree04));
        exit;
      end;


  SetNumberingInSubtree :=
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    var
       NNode: TNoteNode;
    begin
       NNode:= GetNNode(Node);
       NNode.NumberingMethod:= Numbering;
       NNode.CustomNumberingSubtree:= false;
    end;

    if StartNNode <> nil then
       BakNumbering:= StartNNode.NumberingMethod;       // Modify only children and grandchildren, etc, not the node itself

    TV.IterateSubtree(StartNode, SetNumberingInSubtree, nil);
    if StartNNode <> nil then begin
       StartNNode.NumberingMethod:= BakNumbering;
       StartNNode.CustomNumberingSubtree:= true;
    end;

    TV.ReinitChildren(StartNode, true);
    TV.InvalidateToBottom(StartNode);

    TKntFolder(Folder).Modified := true;

  finally
    Form_NodeNum.Free;
  end;

end; // OutlineNumberNodes


{$ENDREGION }


// Sort - Expand - Navigate

{$REGION Sort - Expand - Navigate }


procedure TKntTreeUI.SortTree;
begin
  if CheckReadOnly then exit;

  if (App.DoMessageBox(GetRS(sTree49), mtConfirmation, [mbYes,mbNo]) <> mrYes) then exit;

  TV.SortTree(-1, sdAscending);
  TKntFolder(Folder).Modified:= true;
end;


procedure TKntTreeUI.SortSubtree(Node : PVirtualNode);
begin
  if CheckReadOnly then exit;

  if Node = nil then
     Node := TV.FocusedNode;

  if not assigned(Node) or (Node.ChildCount = 0) then exit;

  TV.Sort(Node, -1, sdAscending);
  TKntFolder(Folder).Modified:= true;
end;


procedure TKntTreeUI.SetNodeExpandState(const TopLevelOnly : boolean );
var
  Node : PVirtualNode;
  NNode: TNoteNode;
begin
   Node := TV.GetFirst;
   while assigned(Node) do begin
      NNode:= GetNNode(Node);
      if TopLevelOnly then begin
         TV.Expanded[Node]:= true;
         Node := TV.GetNextSibling(Node)
      end
      else begin
         if nnsSaved_Expanded in NNode.States then
            TV.Expanded[Node] := true;
         Node := TV.GetNext(Node)
      end;
   end;

end;


procedure TKntTreeUI.FullCollapse;
begin
  TV.FullCollapse;
end;


procedure TKntTreeUI.FullExpand;
begin
  TV.FullExpand;
end;


procedure TKntTreeUI.Navigate(NavDirection: TNavDirection);
var
  VKey : Word;
begin
   VKey := 0;

   case NavDirection of
     navUp  :  VKey := VK_UP;
     navDown:  VKey := VK_DOWN;
     navLeft:  VKey := VK_LEFT;
     navRight: VKey := VK_RIGHT;
   end;

   with ActiveFolder.TV do begin
     Perform(WM_KEYDOWN, VKey, 0);
     Perform(WM_KEYUP, VKey, 0);
   end;

   CheckNotifyAccesibleFocusedItem (EVENT_OBJECT_NAMECHANGE);

end; // Navigate


{$ENDREGION }


// Create new nodes ================================================================

{$REGION Create new nodes}

function TKntTreeUI.AddNode(aInsMode: TNodeInsertMode) : TNoteNode;
begin
  result := NewNode(aInsMode, nil, '', false);
  if (KeyOptions.RunAutoMacros and assigned(result)) then
     ExecuteMacro(_MACRO_AUTORUN_NEW_NODE, '');
end;


function TKntTreeUI.NewNode(aInsMode : TNodeInsertMode; OriginNode : PVirtualNode;
                            const aNewNodeName : string;
                            const aDefaultNode : boolean) : TNoteNode;
var
  myNote, myParentNote : TNote;
  myTreeNode, mySiblingNode : PVirtualNode;
  myName : string;
  p : integer;
  AddingFirstNode, addnumber : boolean;
  Folder: TKntFolder;
  NewNNode: TNoteNode;
begin
  result := nil;
  if CheckReadOnly then exit;

  myTreeNode := nil; { just to avoid }
  myNote := nil;     { compiler warning }

  Folder:= TKntFolder(Self.Folder);

  //addnumber := Folder.AutoNumberNodes;
  if (aNewNodeName <> '') then
    myName := aNewNodeName

  else begin
    myName := Folder.DefaultNoteName;

    // check for special tokens
    p := pos(NODEINSDATE, myName);
    if (p > 0) then begin
      delete(myName, p, length(NODEINSDATE));
      insert(FormatDateTime(KeyOptions.DateFmt, now), myName, p);
      addnumber := false;
    end;

    p := pos(NODEINSTIME, myName);
    if (p > 0) then begin
      delete(myName, p, length(NODEINSTIME));
      insert(FormatDateTime(KeyOptions.TimeFmt, now), myName, p);
      //addnumber := false;
    end;

    p := pos(NODECOUNT, myName);
    if (p > 0) then begin
      delete(myName, p, length(NODECOUNT));
      insert(IntToStr(succ(TV.TotalCount)), myName, p);
      //addnumber := false;
    end;

    //if addnumber then
    //  myName := Folder.DefaultNoteName + #32 + IntToStr(succ(TV.TotalCount));

  end;

  if (TV.TotalCount = 0) or (not assigned(TV.FocusedNode)) then begin
    AddingFirstNode := true;
    aInsMode := tnTop; // no children or siblings if no nodes
  end
  else
    AddingFirstNode := false;

  try
    TV.BeginUpdate;             // Otherwise, as soon as we insert the new virtual nodes, OnGetText (and others) will be triggered, and we still will not have our NNode assigned
    try
      TV.OnFocusChanged := nil;
      if (OriginNode = nil) then
         OriginNode := TV.FocusedNode;

      case aInsMode of
        tnTop :      myTreeNode := TV.AddChild(nil);
        tnAddAbove : myTreeNode := TV.InsertNode(OriginNode, amInsertBefore);
        tnAddChild : myTreeNode := TV.AddChild(OriginNode);
        tnAddBelow : myTreeNode := TV.InsertNode(OriginNode, amInsertAfter);
        tnAddLast :  myTreeNode := TV.AddChild(OriginNode.Parent);
      end;


      // these tokens can be expanded only after the node was created
      if (aNewNodeName = '') then begin
         if (pos('%', myName) > 0) then begin
           p := pos(NODELEVEL, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODELEVEL));
             insert(IntToStr(TV.GetNodeLevel(myTreeNode)), myName, p);
           end;

           p := pos(NODEINDEX, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEINDEX));
             insert(IntToStr(succ(myTreeNode.Index)), myName, p);
           end;

           p := pos(NODEABSINDEX, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEABSINDEX));
             insert(IntToStr(succ(TV.AbsoluteIndex(myTreeNode))), myName, p);
           end;

           p := pos(NODEPARENT, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEPARENT));
             if assigned(myTreeNode.Parent) then
               insert(GetNNode(myTreeNode.Parent).NoteName, myName, p)
             else
               insert('<NONE>', myName, p);
           end;

           p := pos(NODENOTENAME, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODENOTENAME));
             insert(RemoveAccelChar(Folder.Name), myName, p);
           end;

           p := pos(NODEFILENAME, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEFILENAME));
             insert(ExtractFilename(ActiveFile.FileName), myName, p);
           end;

         end;
      end;


      NewNNode:= TKntFolder(Folder).AddNewNote(nil, OriginNode);
      NewNNode.TVNode:= myTreeNode;
      SetNNode(myTreeNode, NewNNode);
      NewNNode.Note.Name:= myName;
      SetNumberingMethod(myTreeNode);

      Result := NewNNode;



      if AddingFirstNode then begin
        ShowOrHideIcons;
        ShowOrHideCheckBoxes;
      end;

      SetupNewTreeNode(myTreeNode);

    except
      on E : Exception do begin
        App.ErrorPopup(GetRS(sTree01) + E.Message);
        // if assigned(myTreeNode) then myTreeNode.Free;
        // if assigned(myNote) then myNote.Free;
      end;
    end;

  finally
    TV.OnFocusChanged := TV_FocusChanged;
    MakePathVisible(myTreeNode);
    TV.EndUpdate;
    SelectAlone(myTreeNode);

  end;

  if ((not aDefaultNode) and assigned(myTreeNode) and KntTreeOptions.EditNewNodes) then
     RenameFocusedNode;

end;


procedure TKntTreeUI.CreateParentNode(Node: PVirtualNode);
var
  NewParentNNode: TNoteNode;
  NextNode, NewParentNode: PVirtualNode;
begin
  if CheckReadOnly then exit;

  if Node = nil then
     Node:= TV.GetFirst();
  NewParentNNode := NewNode(TNodeInsertMode.tnAddAbove, Node, '', true);

  if assigned(NewParentNNode) then begin
     NewParentNode:= NewParentNNode.TVNode;

     TV.BeginUpdate;
     try         
       Node:= TV.GetNext(NewParentNode);
       while assigned(Node) do begin
         NextNode := TV.GetNextSibling(Node);
         TV.MoveTo(Node, NewParentNode, TVTNodeAttachMode.amAddChildLast, False);
         Node:= NextNode;
       end;

     finally       
       TV.EndUpdate;
       TV.SetFocus;
       SelectAlone(NewParentNode);
       TV.Expanded[NewParentNode]:= True;
     end;

  end;

end;


procedure TKntTreeUI.CreateNodefromSelection;
var
  NNode: TNoteNode;
  Note: TNote;
  NEntry: TNoteEntry;
  myRTFText: AnsiString;
  myNodeName : string;

begin
   if CheckReadOnly then exit;
   if (ActiveEditor.SelLength = 0) then begin
     App.WarnNoTextSelected;
     exit;
   end;

  if not assigned(TV.FocusedNode) then begin
    App.ShowInfoInStatusBar(GetRS(sTree17));
    exit;
  end;

  myNodeName:= FirstLineFromString(TrimLeft(ActiveEditor.SelText), TREENODE_NAME_LENGTH_CAPTURE);
  myRTFText := ActiveEditor.RtfSelText;

  NNode := NewNode(tnAddBelow, nil, '', true);        // => New node will be focused -> Folder.NodeSelected
  if assigned(NNode) then begin
    try
      Note:= NNode.Note;
      if (myNodeName <> '') then begin  // can be blank
         Note.Name := myNodeName;
         TV.InvalidateNode(NNode.TVNode);
      end;

      NEntry:= Note.Entries[0];                                      // %%%
      NEntry.Stream.Position := 0;
      NEntry.Stream.WriteBuffer(myRTFTExt[1], length(myRTFTExt));
      TKntFolder(Folder).ReloadEditorFromDataModel;

      MakePathVisible(NNode.TVNode);

    finally
      TKntFolder(Folder).Modified := true;
    end;
  end;

end; // CreateNodefromSelection


procedure TKntTreeUI.SetupNewTreeNode(const Node : PVirtualNode; Loaded: boolean = false; RemoveFiltered: boolean = false);
var
  NNode, ParentNNode: TNoteNode;
  ParentNode : PVirtualNode;

begin
   if ShowAllCheckboxes then
      Node.CheckType  := ctCheckBox

   else begin
      ParentNNode:= nil;
      ParentNode:= Node.Parent;
      if ParentNode <> TV.RootNode then
         ParentNNode:= GetNNode(ParentNode);

      if (ParentNNode <> nil) and ParentNNode.ChildrenCheckbox then
         Node.CheckType  := ctCheckBox
      else
         Node.CheckType  := ctNone;
   end;

   if Loaded then begin
      if (Node.CheckType = ctCheckBox) then begin
         NNode:= GetNNode(Node);
         if nnsSaved_Checked in NNode.States then
            Node.CheckState := csCheckedNormal
         else
            Node.CheckState := csUncheckedNormal;
      end;
   end
   else begin
      // This NNode can be the result of a copy or move operation from other tree. The source node could be filtered.
      // It is convenient that it is visible after operation
      if RemoveFiltered then begin
         NNode:= GetNNode(Node);
         TV.IsFiltered[Node]:= False;
         NNode.FindFilterMatch:= False;
         NNode.TreeFilterMatch:= False;
      end;
   end;

end;



{$ENDREGION}


// Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees | Drag and Drop

{$REGION Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees}

function TKntTreeUI.MoveTreeNode(MovingNode : PVirtualNode; const aDir : TDirection): boolean;
var
  t : string;
  PreviousParent, theSibling : PVirtualNode;
  WasExpanded: boolean;

begin
  if not Assigned(MovingNode) then
     MovingNode := TV.FocusedNode;
  if not Assigned(MovingNode) or CheckReadOnly then exit;

  t := GetRS(sTree05);
  Result:= false;

  TV.BeginUpdate;

  try
    try
      PreviousParent := MovingNode.Parent;
      case aDir of
        // UP / DOWN move node only within its siblings
        // (cannot change level)
        dirUp : begin
          theSibling := TV.GetPreviousVisibleSibling(MovingNode);
          if assigned(theSibling) then begin
            TV.MoveTo(MovingNode, theSibling, TVTNodeAttachMode.amInsertBefore, False);
            t := '';
          end;
        end;
        dirDown : begin
          theSibling := TV.GetNextVisibleSibling(MovingNode);
          if assigned(theSibling) then begin
            TV.MoveTo(MovingNode, theSibling, TVTNodeAttachMode.amInsertAfter, False);
            t := '';
          end;
        end;

        // LEFT promotes node 1 level up
        // RIGHT demotes node 1 level down
        dirLeft : begin
          if MovingNode.Parent <> TV.RootNode then begin
            WasExpanded:= TV.Expanded[MovingNode];
            TV.Expanded[MovingNode]:= false;
            // becomes its parent's sibling
            TV.MoveTo(MovingNode, MovingNode.Parent, TVTNodeAttachMode.amInsertAfter, False);
            SetNumberingMethod(MovingNode);
            SetupNewTreeNode(MovingNode);
            t := '';
            TV.Expanded[MovingNode]:= WasExpanded;
          end;
        end;
        dirRight : begin
          theSibling := TV.GetPreviousSibling(MovingNode);
          if assigned(theSibling) then begin
            // becomes the last child of its previous sibling
            TV.MoveTo(MovingNode, theSibling, TVTNodeAttachMode.amAddChildLast, False);
            SetNumberingMethod(MovingNode);
            SetupNewTreeNode(MovingNode);
            TV.Expanded[theSibling]:= true;
            t := '';
          end;
        end;
      end;


      if (t = '') then begin // means node was successfully moved
        // update node icon
        Result:= true;
        TV.Invalidate;
      end;

    except
      on E : Exception do
        App.ErrorPopup(E, GetRS(sTree06));
    end;
  finally
    TKntFolder(Folder).Modified:= true;
    TV.EndUpdate;

    App.ShowInfoInStatusBar(Format(GetRS(sTree07), [GetNNode(MovingNode).NoteName, t, DIRECTION_NAMES[aDir]]));
  end;

end; // MoveTreeNode


procedure TKntTreeUI.MoveTreeNode(const aDir : TDirection);
var
  TVSelectedNodes: TNodeArray;
  Node, LastNode: PVirtualNode;
  LastMovedOk: boolean;
  i: integer;
begin
  Node:= TV.FocusedNode;
  if Node = nil then exit;

  if TV.SelectedCount = 0 then
     MoveTreeNode(Node, aDir)

  else begin
     LastMovedOk:= true;
     TVSelectedNodes:= TV.GetSortedSelection(True);

     if aDir in [dirDown, dirLeft] then
        for i := High(TVSelectedNodes) downto 0 do begin
           Node:= TVSelectedNodes[i];
           if not LastMovedOk and (LastNode = Node.NextSibling) then begin
              LastNode:= Node;
              continue;
           end;
           LastNode:= Node;
           LastMovedOk:= MoveTreeNode(Node, aDir);
        end
     else begin
        for i := 0 to High(TVSelectedNodes) do begin
           Node:= TVSelectedNodes[i];
           if not LastMovedOk and (LastNode = Node.PrevSibling) then begin
              LastNode:= Node;
              continue;
           end;
           LastNode:= Node;
           LastMovedOk:= MoveTreeNode(Node, aDir);
        end;
     end;
  end;

  TV.ScrollIntoView(TV.FocusedNode, false);
end;



function TKntTreeUI.DeleteNode(myTreeNode: PVirtualNode; const DeleteOnlyChildren: boolean;
                               const AskForConfirmation: boolean = true;
                               const ConfirmationOnlyForChildren: boolean = false): boolean;
var
  myTreeParent, myTreeChild, myNextChild : PVirtualNode;
  NNode: TNoteNode;
  KeepChildNodes : boolean;
  Folder: TKntFolder;

begin
  with Form_Main do begin
      KeepChildNodes := false;
      Result:= false;

      if CheckReadOnly then exit;

      if not Assigned(myTreeNode) then
         myTreeNode := TV.FocusedNode;
      if not Assigned(myTreeNode) then exit;

      NNode:= GetNNode(myTreeNode);
      myTreeParent := myTreeNode.Parent;
      Folder:= TKntFolder(Self.Folder);

      Result:= true;

      if AskForConfirmation then begin

         if DeleteOnlyChildren then begin
            // command was to delete CHILDREN of focused node
            if vsHasChildren in myTreeNode.States then begin
              case App.DoMessageBox(Format(GetRS(sTree12), [myTreeNode.ChildCount, NNode.NodeName(Self)]) + GetRS(sTree08),
                   mtWarning, [mbYes, mbNo, mbCancel], def2 ) of
                  mrNo: exit;
                  mrCancel: exit(false);
              end;
            end
            else begin
              if not ConfirmationOnlyForChildren then
                 App.InfoPopup(GetRS(sTree13));
              exit;
            end;
         end
         else begin
            // delete focused node and all its children, if any

            if vsHasChildren in myTreeNode.States then begin
              // ALWAYS warn if node has children (except if we are moving to another location)

              case App.DoMessageBox(Format(GetRS(sTree09), [NNode.NodeName(Self), myTreeNode.ChildCount]) + GetRS(sTree08),
                   mtWarning, [mbYes, mbNo, mbCancel], def3 ) of
                mrYes : KeepChildNodes := false;
                mrNo  : KeepChildNodes := true;
                else
                  exit (false);
              end;
            end
            else begin
              if not ConfirmationOnlyForChildren and KntTreeOptions.ConfirmNodeDelete then begin
                if (App.DoMessageBox(Format(GetRS(sTree10), [NNode.NodeName(Self)]) + GetRS(sTree08), mtWarning, [mbYes,mbNo]) <> mrYes) then exit;
              end;
            end;
         end;
      end;

      with TV do begin
        OnFocusChanged := nil;
        BeginUpdate;
      end;

      try
        try
          if DeleteOnlyChildren then
             TV.DeleteChildren(myTreeNode, true)    // The NoteNodes in the subtree will be removed from TV_FreeNode

          else begin
             if KeepChildNodes then begin
               myTreeChild:= myTreeNode.FirstChild;
               TV.MoveTo(myTreeNode, myTreeParent, TVTNodeAttachMode.amAddChildLast, True);
             end;
             TV.DeleteNode(myTreeNode);
          end;

        except
          on E : Exception do
            App.ErrorPopup(E, GetRS(sTree14));
        end;

      finally
        fLastNodeSelected:= nil;

        with TV do begin
          OnStructureChange:= nil;
          OnFocusChanged := TV_FocusChanged;
          EndUpdate;
        end;

        if KeepChildNodes then
           SelectAlone(myTreeChild);           // By default it selects the previous sibling

        if TV.FocusedNode = nil then
           Folder.NoNodeInTree;

        Folder.Modified:= true;
      end;
  end;

end; // DeleteNode


procedure TKntTreeUI.DeleteNode(const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true);
var
  TVSelectedNodes: TNodeArray;
  Node: PVirtualNode;
  OnlyChildren: string;
  i: integer;
begin
  if CheckReadOnly then exit;

  Node:= TV.FocusedNode;
  if Node = nil then exit;

  if TV.SelectedCount <= 1 then
     DeleteNode(Node, DeleteOnlyChildren, AskForConfirmation)

  else begin
     if DeleteOnlyChildren then
        OnlyChildren:= 'CHILD NODES of ';

     if (App.DoMessageBox(Format(GetRS(sTree11), [OnlyChildren]) + GetRS(sTree08), mtWarning, [mbYes,mbNo]) <> mrYes) then exit;

     TVSelectedNodes:= TV.GetSortedSelection(True);
     for i := 0 to High(TVSelectedNodes) do
        if not (DeleteNode(TVSelectedNodes[i], DeleteOnlyChildren, True, True)) then exit;
  end;

end;



procedure TKntTreeUI.InsertLinkedNNode(Node: PVirtualNode);
var
  NNode, NewNNode: TNoteNode;
  NewNode: PVirtualNode;
begin
   TV.BeginUpdate;

   NNode:= GetNNode(Node);
   NewNNode:= TKntFolder(Folder).AddNewNNode(NNode.Note, NNode);
   NewNode := TV.InsertNode(Node, amInsertBefore);
   SetNNode(NewNode, NewNNode);
   NewNNode.TVNode:= NewNode;
   NewNode.CheckType:= Node.CheckType;

   TV.EndUpdate;
end;



procedure TKntTreeUI.CopySubtrees (TargetNode: PVirtualNode; Prompt: boolean; PasteAsLinkedNNode: boolean; AttachMode: TVTNodeAttachMode= amAddChildLast);
var
  Node: PVirtualNode;
  SourceTV: TBaseVirtualTree;
  i: integer;
  TargetDesc, Obs: string;

begin
   if TargetNode = nil then begin
      TargetDesc:= '<ROOT>';
      TargetNode:= TV.RootNode;
   end
   else
      TargetDesc:= GetNNode(TargetNode).NodeName(Self);

   fChildCountOnTargetNode:= TargetNode.ChildCount;

   SourceTV:= TreeFromNode(fSourceTVSelectedNodes[0]);
   Prompt:= (Prompt or ((TV <> SourceTV) and KeyOptions.DropNodesOnTabPrompt));

   if PasteAsLinkedNNode then begin
      if Prompt and (App.DoMessageBox(Format(GetRS(sTree23), [Length(fSourceTVSelectedNodes), GetRS(sTree26), TargetDesc]),
                     mtConfirmation, [mbYes,mbNo]) <> mrYes) then
            exit;
   end
   else begin
       if HasVirtualNotesInSelectedSubtrees then
          Obs:= GetRS(sTree22);

       if Prompt and (App.DoMessageBox(Format(GetRS(sTree23), [Length(fSourceTVSelectedNodes), '', TargetDesc]) + Obs,
                         mtConfirmation, [mbYes,mbNo]) <> mrYes) then // {N}
          exit;
   end;

   fCopyingAsLinked:= PasteAsLinkedNNode;
   fTargetFolder:= Folder;
   TV.BeginUpdate;

   for i := 0 to High(fSourceTVSelectedNodes) do
      SourceTV.CopyTo(fSourceTVSelectedNodes[i], TargetNode, AttachMode, False);

   TV.Expanded[TargetNode]:= true;
   SelectAlone(TargetNode);
   TV.EndUpdate;
   fiNextSourceTVNode:= 0;
   fTVCopiedNodes.Clear;
   App.ShowInfoInStatusBar(Format(GetRS(sTree24), [Length(fSourceTVSelectedNodes)]));
   if (fVirtualNodesConvertedOnCopy > 0) then
      App.InfoPopup(Format(GetRS(sTree25),[fVirtualNodesConvertedOnCopy]));

  TKntFolder(Folder).Modified:= true;
  if TargetNode = TV.RootNode then
     CheckFocusedNode;

end;



procedure TKntTreeUI.MoveSubtrees (TargetNode: PVirtualNode; Prompt: boolean; AttachMode: TVTNodeAttachMode= amAddChildLast);
var
 SourceFolder: TKntFolder;
 SourceTreeUI: TKntTreeUI;
 SourceTV: TBaseVirtualTree;
 SourceNode: PVirtualNode;
 i: integer;
 TargetDesc: string;

begin
   if TargetNode = nil then begin
      TargetDesc:= '<ROOT>';
      TargetNode:= TV.RootNode;
   end
   else begin
      TargetDesc:= GetNNode(TargetNode).NodeName(Self);
      if IsIncludedInSelectedSubtrees(TargetNode) then begin
         App.WarningPopup(GetRS(sTree19));
         exit;
      end;
   end;

  SourceFolder:= ActiveFile.GetFolderByTreeNode(fSourceTVSelectedNodes[0]);
  Assert(assigned(SourceFolder));
  if SourceFolder.TreeUI.CheckReadOnly then exit;

  SourceTV:= TreeFromNode(fSourceTVSelectedNodes[0]);
  Prompt:= (Prompt or ((TV <> SourceTV) and KeyOptions.DropNodesOnTabPrompt));

  if Prompt and (App.DoMessageBox(Format(GetRS(sTree16),  [Length(fSourceTVSelectedNodes), TargetDesc]),
                       mtConfirmation, [mbYes,mbNo]) <> mrYes) then
      exit;

  fMovingToOtherTree:= (SourceTV <> TV);
  fTargetFolder:= Folder;

  for i := 0 to High(fSourceTVSelectedNodes) do begin
     SourceNode:= fSourceTVSelectedNodes[i];
     SourceTV.MoveTo(SourceNode, TargetNode, AttachMode, False);
     if not fMovingToOtherTree then begin
        SetNumberingMethod(SourceNode);
        SetupNewTreeNode(SourceNode);
     end;
  end;

  SelectAlone(TargetNode);
  TV.Expanded[TargetNode]:= true;

  fSourceTVSelectedNodes:= nil;
  fNNodesInSubtree:= nil;
  fCutSubtrees:= false;
  fMovingToOtherTree:= false;

  SourceFolder.Modified:= true;
  TKntFolder(Folder).Modified:= true;

  if TargetNode = TV.RootNode then
     CheckFocusedNode;

end;


function TKntTreeUI.TreeTransferProc(XferAction: TTreeTransferAction; Prompt: boolean; PasteAsVirtualKNTNode: boolean) : boolean;
var
  FocNode: PVirtualNode;

begin

   case XferAction of
      ttClear: begin
        result := true;
        if (App.DoMessageBox(Format(GetRS(sTree18), [GetRS(sTree20)]), mtConfirmation, [mbYes,mbNo]) = mrYes) then begin
           fSourceTVSelectedNodes:= nil;
           fTVCopiedNodes.Clear;
           fiNextSourceTVNode:= 0;
           fVirtualNodesConvertedOnCopy:= 0;
           exit;
        end;
      end;

      ttCopy, ttCut: begin
         if (TV.SelectedCount <= 0) then begin
           App.InfoPopup(GetRS(sTree15));
           exit;
         end;
         GetSelectedSubtrees (TV);
         App.ShowInfoInStatusBar(Length(fSourceTVSelectedNodes).ToString + GetRS(sTree20));
         fCutSubtrees:= (XferAction = ttCut);
      end;

      ttPaste: begin
         if (fSourceTVSelectedNodes = nil) then begin
            App.InfoPopup(GetRS(sTree21));
            exit;
         end;
         FocNode:= TV.FocusedNode;
         if TV.IsEffectivelyFiltered[FocNode] then
            FocNode := nil;
         if CheckReadOnly then exit;

         if fCutSubtrees then
            MoveSubtrees (FocNode, Prompt, TVTNodeAttachMode.amAddChildLast)
         else
            CopySubtrees (FocNode, Prompt, PasteAsVirtualKNTNode, TVTNodeAttachMode.amAddChildLast);
      end;
   end;

end;



class procedure TKntTreeUI.GetSelectedSubtrees (SourceTV: TBaseVirtualTree);
begin
{
  var
   fSourceTVSelectedNodes: TNodeList
   i, j: integer;
   Node: PVirtualNode;

   fSourceTVSelectedNodes.Clear;
   for Node in SourceTV.SelectedNodes() do
      fSourceTVSelectedNodes.Add(Node);

   for i := fSourceTVSelectedNodes.Count -1 downto 0 do begin
       for j := 0 to fSourceTVSelectedNodes.Count -1 do begin
           if SourceTV.HasAsParent(fSourceTVSelectedNodes[i], fSourceTVSelectedNodes[j]) then begin // ¿Node[j] is parent of Node[i]?
              fSourceTVSelectedNodes.Delete(i);
              break;
           end;
       end;
   end;
   Result:= fSourceTVSelectedNodes;
}

   fSourceTVSelectedNodes:= SourceTV.GetSortedSelection(True);   // The above is equivalent to using GetSortedSelection(True)...

   fTVCopiedNodes.Clear;
   fiNextSourceTVNode:= 0;
   fVirtualNodesConvertedOnCopy:= 0;
end;


function TKntTreeUI.IsIncludedInSelectedSubtrees (Node: PVirtualNode): boolean;
var
   i: integer;
begin
   Result:= false;
   for i := 0 to High(fSourceTVSelectedNodes) do begin
        if TV.HasAsParent(Node, fSourceTVSelectedNodes[i]) then begin
           Result:= true;
           break;
        end;
   end;
end;


function TKntTreeUI.HasVirtualNotesInSelectedSubtrees: boolean;
var
   i: integer;

   function HasVirtualNotes (Node: PVirtualNode): boolean;
   var
      NNode: TNoteNode;
   begin
      NNode:= GetNNode(Node);
      if NNode.Note.IsVirtual then
         exit(True)
      else begin
         Result:= false;
         if (vsHasChildren in Node.States) then
             for Node in TV.ChildNodes(Node) do
                 if HasVirtualNotes(Node) then
                    exit(True);
      end;
   end;

begin
   Result:= false;
   for i := 0 to High(fSourceTVSelectedNodes) do
      if HasVirtualNotes(fSourceTVSelectedNodes[i]) then
         exit(true);
end;


procedure TKntTreeUI.LoadNNodesInSubtree(Node: PVirtualNode);
var
  LoadSubtree: TVTGetNodeProc;
  i: integer;

begin
  LoadSubtree :=
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    begin
      fNNodesInSubtree[i]:= GetNNode(Node);
      inc(i);
    end;

  i:= 0;
  SetLength(fNNodesInSubtree, Node.TotalCount);
  TV.IterateSubtree(Node, LoadSubtree, nil);
end;




procedure TKntTreeUI.TV_NodeCopying(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
var
   NNodeS, NNodeC: TNoteNode;
   NodeS, NodeC, SrcNode: PVirtualNode;
   TargetFolder: TKntFolder;
   CopyingToOtherTree: boolean;

begin
   // *2 When copying subtrees, hidden nodes will be included.
   //   So the use of TV.GetNext(NodeS) and TV.GetNextNotChild(NodeS, true) instead of TV.GetNextNotHidden(NodeS)
   //   and NodeS:= TV.GetNextVisibleNotChild(NodeS)
   //   To include hidden nodes is because the subtree is copied using SourceTV.CopyTo(..), a method supported
   //   by VirtualTreeView.
   //   Here we manage each created [tree] node, associating to it a Note Node, configured correctly. The method
   //   CopyTo(..) makes a copy of each node of the subtree, hidden or not. If in this method we ignore the hidden
   //   nodes, we simply will get an exception when the tree needs to paint or access the hidden node.
   //   Anyway with this tree control is now very easy to delete several nodes at once, if necessary.
   //
   // *3
   //  Also consider something like the following:
   //    - Parent
   //       - AA
   //       - BB
   //       - CC
   //
   // If we select AA and CC and copy them to CC, we need to check if NodeS is one of the created nodes
   // (by looking at fTVCopiedNodes) several times (-> while).
   // And also if Target node is the same node as one of the source nodes (subtrees), we must annotate its original
   // child count, because VirtualTreView can get confused and assign as child one of the nodes created in the
   // copy process.


   SrcNode:= fSourceTVSelectedNodes[fiNextSourceTVNode];
   inc(fiNextSourceTVNode);
   fTVCopiedNodes.Add(Node);

   NodeS:= SrcNode;
   NodeC:= Node;
   TargetFolder:= TKntFolder(fTargetFolder);
   CopyingToOtherTree:= (TargetFolder.TreeUI <> Self);

   if NodeS = Target then begin                                  // *3
      TV.ChildCount[NodeC]:= fChildCountOnTargetNode;
      if fChildCountOnTargetNode = 0 then
         NodeC.States := NodeC.States - [vsHasChildren];
      //TV.ReinitNode(NodeC, false, false);                 Unnecessary
   end;

   repeat
      NNodeS:= GetNNode(NodeS);
      if fCopyingAsLinked or NNodeS.Note.IsVirtual then begin
         NNodeC:= TargetFolder.AddNewNNode(NNodeS.Note, NNodeS);
         if not fCopyingAsLinked then
            inc(fVirtualNodesConvertedOnCopy);
      end
      else
         NNodeC:= TargetFolder.AddNewNote(NNodeS);

      SetNNode(NodeC, NNodeC);
      NNodeC.TVNode:= NodeC;
      TargetFolder.TreeUI.SetNumberingMethod(NodeC);
      TargetFolder.TreeUI.SetupNewTreeNode(NodeC, false, CopyingToOtherTree);
      NodeS:= TV.GetNext(NodeS);                      // *2 *3
      while fTVCopiedNodes.IndexOf(NodeS) >= 0 do     // It is being pasted within the same tree it was copied from
         NodeS:= TV.GetNextNotChild(NodeS, true);     // True: include filtered

      NodeC:= TV.GetNext(NodeC);                     // *2
   until (NodeS = nil) or not TV.HasAsParent(NodeS, SrcNode);
end;


procedure TKntTreeUI.TV_NodeMoving(Sender: TBaseVirtualTree; Node, Target: PVirtualNode; var Allowed: Boolean);
begin
   if fMovingToOtherTree then
      LoadNNodesInSubtree(Node);
end;


procedure TKntTreeUI.TV_NodeMoved (Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  MoveSubtree: TVTGetNodeProc;
  i: integer;

begin
   if fMovingToOtherTree then begin
     MoveSubtree :=
       procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
       var
         NNode: TNoteNode;
       begin
         NNode:= fNNodesInSubtree[i];
         SetNNode(Node, NNode);
         NNode.TVNode:= Node;
         TKntFolder(fTargetFolder).TreeUI.SetNumberingMethod(Node);
         TKntFolder(fTargetFolder).TreeUI.SetupNewTreeNode(Node, false, true);
         NNode.Note.UpdateFolderInNNode(NNode, fTargetFolder);
         TKntFolder(fFolder).RemoveNNode(NNode);
         TKntFolder(fTargetFolder).AddNNode(NNode);
         inc(i);
       end;

      i:= 0;
      TV.IterateSubtree(Node, MoveSubtree, nil);
   end;

end;


procedure TKntTreeUI.TV_StartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  fDropTargetNode:= nil;
  fDropTargetNodeInsMode:= tnAddLast;
end;


procedure TKntTreeUI.TV_DragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
  Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);

  procedure DetermineEffect;      // Determine the drop effect to use if the source is a Virtual Treeview.
  begin
    if (ssAlt in Shift) then
      Effect := DROPEFFECT_LINK
    else
      if (ssCtrl in Shift) then
        Effect := DROPEFFECT_COPY
      else
        Effect := DROPEFFECT_MOVE;
  end;

begin
  Accept := not fReadOnly;
  DetermineEffect;
end;

// Already used in commit b4bd2f0bd50 for inclusion in RichEdit:
{
function ContainFormat(ADataObject: IDataObject; AFormat: TClipFormat;
  ATymed: Longint; AAspect: LongInt = DVASPECT_CONTENT; AIndex: LongInt = -1): Boolean;
var Format: TFormatEtc;
begin
  ZeroMemory(@Format, SizeOf(Format));
  Format.cfFormat := AFormat;
  Format.dwAspect := AAspect;
  Format.lindex := AIndex;
  Format.tymed := ATymed;
  Result := ADataObject.QueryGetData(Format) = S_OK;
end;
}

procedure TKntTreeUI.TV_DragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
  // Note: Consider the example of OLE Drag and Drop from OLE demo, in Virtual Treeview library
  procedure DetermineEffect;      // Determine the drop effect to use if the source is a Virtual Treeview.
  begin
    if (ssAlt in Shift) then
      Effect := DROPEFFECT_LINK
    else
      if (ssCtrl in Shift) then
        Effect := DROPEFFECT_COPY
      else
        Effect := DROPEFFECT_MOVE;
  end;

var
  Attachmode: TVTNodeAttachMode;

begin
  if CheckReadOnly then begin
    if (Sender is TBaseVirtualTree) then
       TBaseVirtualTree(Sender).EndDrag(false);
    exit;
  end;

  // Translate the drop position into an node attach mode.
  case Mode of
    dmAbove:
      AttachMode := amInsertBefore;
    dmOnNode:
      AttachMode := amAddChildLast;
    dmBelow:
      AttachMode := amInsertAfter;
  else
    AttachMode := amNowhere;
  end;



  if Source is TBaseVirtualTree then begin
      // We can ignore the drop event entirely and use VT mechanisms (Regardless of whether it is VCL or OLE drag'n drop)
      DetermineEffect;
      GetSelectedSubtrees (TBaseVirtualTree(Source));
      if Sender.DropTargetNode = nil then
         AttachMode := amAddChildLast;

      if (Effect = DROPEFFECT_COPY) or (Effect = DROPEFFECT_LINK) then
         CopySubtrees (Sender.DropTargetNode, False, (Effect = DROPEFFECT_LINK), AttachMode)
      else
         MoveSubtrees (Sender.DropTargetNode, False, AttachMode);

      Effect := DROPEFFECT_NONE;       // We have already managed it and we do not want anything more to be done
  end
  else
     if DataObject <> nil then begin
       { Determine action in advance even if we don't use the dropped data.
         Note: The Effect parameter is a variable which must be set to the action we will actually take, to notify the
         sender of the drag operation about remaining actions.
         This value determines what the caller will do after the method returns, e.g. if DROPEFFECT_MOVE is returned then the source data will be deleted }
       if Sender is TBaseVirtualTree then begin
          fDropTargetNode:= Sender.DropTargetNode;
          case Mode of
             dmAbove:
               fDropTargetNodeInsMode:= tnAddAbove;
             dmOnNode:
               fDropTargetNodeInsMode:= tnAddChild;
             dmBelow:
               fDropTargetNodeInsMode:= tnAddBelow;
           else
             fDropTargetNodeInsMode:= tnAddLast;
           end;
       end
       else
         { Prefer copy if allowed for every other drag source. Alone from Effect you cannot determine the standard action
           of the sender, but we assume if copy is allowed then it is also the standard action (e.g. as in TRichEdit).}
         if Boolean(Effect and DROPEFFECT_COPY) then
           Effect := DROPEFFECT_COPY
         else
           Effect := DROPEFFECT_MOVE;

        { The TBaseVirtualTreeNo class does not handle the WM_DROPFILE event. But we don't need to do DragAcceptFiles( TV.handle, true )
          Currently we have already called DragAcceptFiles(Handle, true ) from TForm_Main.CreateWnd
          As soon as we do Effect:= DROPEFFECT_NONE indicating that we are not managing this Drag and Drop, the TForm_Main.WMDropFiles method is called
          Note: At the time I included references and observations related to Drag and Drop and the use of DragAcceptFiles and WM_DROPFILE from comment
          *3 in the file 3rd_party\unRxLib\units\RxRichEd.pas  }
      //  if ContainFormat(DataObject, CF_HDROP, TYMED_HGLOBAL) then
      //     Result := S_FALSE



       Effect:= DROPEFFECT_NONE;

     end;

end;


procedure TKntTreeUI.TV_EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  CheckReadOnly;
end;


procedure TKntTreeUI.TV_FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NNode: TNoteNode;
begin

  if not fMovingToOtherTree then begin
     NNode:= GetNNode(Node);
     if NNode <> nil then
        TKntFolder(Folder).DeleteNNode(NNode);
  end;
end;


{$ENDREGION}


// Check State ==============================

{$REGION Check State}

procedure TKntTreeUI.ShowOrHideCheckBoxes;
var
  Node : PVirtualNode;
begin
   TV.BeginUpdate;

   if ShowAllCheckboxes then begin
      for Node in TV.Nodes do
         Node.CheckType:= ctCheckBox;
   end
   else
     for Node in TV.ChildNodes(nil) do begin
        Node.CheckType:= ctNone;
        ShowOrHideChildrenCheckBoxes(GetNNode(Node));
     end;

   TV.EndUpdate;
end;


procedure TKntTreeUI.ShowOrHideChildrenCheckBoxes(const NNode: TNoteNode);
var
  Node : PVirtualNode;
  ChkType: TCheckType;

begin
   ChkType:= ctNone;
   if NNode.ChildrenCheckbox then
      ChkType:= ctCheckBox;

   TV.BeginUpdate;
   for Node in TV.ChildNodes(NNode.TVNode) do begin
      Node.CheckType:= ChkType;
      ShowOrHideChildrenCheckBoxes(GetNNode(Node));
   end;
   TV.EndUpdate;
end;


procedure TKntTreeUI.ToggleChildrenCheckbox(Node : PVirtualNode);
var
  NNode : TNoteNode;
begin
  if CheckReadOnly then exit;
  if not assigned(Node) then exit;

  NNode:= GetNNode(Node);
  NNode.ChildrenCheckbox:= not NNode.ChildrenCheckbox;
  ShowOrHideChildrenCheckBoxes (NNode);
end;


procedure TKntTreeUI.ToggleCheckNode(Node : PVirtualNode);
begin
  if CheckReadOnly then exit;
  if not assigned(Node) then exit;

  if Node.CheckState = csCheckedNormal then
     Node.CheckState:= csUncheckedNormal
  else
     Node.CheckState:= csCheckedNormal;

  TV.InvalidateNode(Node);
end;


procedure TKntTreeUI.HideChildNodesUponCheckState (ParentNode: PVirtualNode; Checked: boolean);
var
  Node : PVirtualNode;
  Enum: TVTVirtualNodeEnumeration;
begin
   TV.BeginUpdate;
   if ParentNode = nil then
      Enum:= TV.Nodes
   else
      Enum:= TV.ChildNodes(ParentNode);

   for Node in Enum do
      if (Node.CheckState.IsChecked = Checked) then   // ¿csCheckedNormal o CheckState.IsChecked?
         TV.IsVisible[Node]:= False;
   TV.EndUpdate;
end;


procedure TKntTreeUI.ShowNonFilteredNodes (ParentNode: PVirtualNode);
var
  Node : PVirtualNode;
  Enum: TVTVirtualNodeEnumeration;
begin
   TV.BeginUpdate;
   if ParentNode = nil then
      Enum:= TV.Nodes
   else
      Enum:= TV.ChildNodes(ParentNode);

   for Node in Enum do
      TV.IsVisible[Node]:= True;       // If the node is Filtered and the filter is being applied, it will not be seen
   TV.EndUpdate;
end;


{ A node that is shown in one tree (or a certain branch of a tree) as a 'task' can do so as a 'documentation' in another,
  so it does not necessarily make sense for the Checked status applied to a node to be move to all linked nodes. }

procedure TKntTreeUI.ChangeCheckedState(Node: PVirtualNode; Checked: Boolean);

    procedure CheckChildren(ParentNode : PVirtualNode);
    var
       Node : PVirtualNode;
    begin
      for Node in TV.ChildNodes(ParentNode) do begin
         Node.CheckState:= ParentNode.CheckState;
         if vsHasChildren in Node.States then
            CheckChildren(Node);
      end;

    end;

begin
    try
      if (Shiftdown and (vsHasChildren in Node.States)) then
         CheckChildren(node);

      if HideCheckedNodes then
          TV.IsVisible[Node]:= not Checked;

    finally
        TKntFolder(Folder).Modified:= true;
    end;
end;


procedure TKntTreeUI.TB_HideCheckedClick(Sender: TObject);
var
  myFolder : TKntFolder;
  Hide: boolean;
begin
  myFolder := ActiveFolder;

  if assigned(myFolder) then begin
    Hide:= not myFolder.HideCheckedNodes;
    if CtrlDown and Hide then
       Hide:= False;

    myFolder.HideCheckedNodes := Hide;
    Form_Main.MMViewHideCheckedNodes.Checked:= Hide;
    TB_HideChecked.Down := Hide;

    if Hide then
       myFolder.TreeUI.HideChildNodesUponCheckState (nil, true)
    else begin
       myFolder.TreeUI.ShowNonFilteredNodes (nil);
       fLastTreeSearch:= '';
       txtFilterChange(nil);
    end;
  end;

end;


{$ENDREGION}


// Filter nodes ==============================


{$REGION Filter nodes }

procedure TKntTreeUI.MakePathVisible (Node: PVirtualNode);
begin
{ It doesn't work: VisiblePath[Node]:= True;
  Because what it does is ensure the expansion of the parents. Here we need to ensure that vsVisible is set }
   TV.IsVisible[Node]:= True;
   repeat
     Node := Node.Parent;
     if Node = TV.RootNode then Break;
     TV.IsVisible[Node]:= True;
   until False;
end;


procedure TKntTreeUI.MakePathNonFiltered (Node: PVirtualNode);
begin
   TV.IsFiltered [Node]:= False;
   repeat
     Node := Node.Parent;
     if (Node = TV.RootNode) or not TV.IsFiltered[Node] then Break;
     TV.IsFiltered[Node]:= False;
   until False;
end;


procedure TKntTreeUI.ClearAllFindMatch;
var
  NNode: TNoteNode;
  i: integer;
begin
   for i := 0 to TKntFolder(Folder).NNodes.Count-1 do
       TKntFolder(Folder).NNodes[i].FindFilterMatch:= False;
end;

procedure TKntTreeUI.ClearAllTreeMatch;
var
  NNode: TNoteNode;
  i: integer;
begin
   for i := 0 to TKntFolder(Folder).NNodes.Count-1 do
       TKntFolder(Folder).NNodes[i].TreeFilterMatch:= False;
end;


procedure TKntTreeUI.ApplyFilters (Apply: boolean);
begin
   if Apply then
      TV.TreeOptions.PaintOptions := TV.TreeOptions.PaintOptions - [TVTPaintOption.toShowFilteredNodes]
   else
      TV.TreeOptions.PaintOptions := TV.TreeOptions.PaintOptions + [TVTPaintOption.toShowFilteredNodes];

   ShowFlaggedColumnForTagFilter(Apply and TagFilterApplied);
   FrameResize(nil);
end;

procedure TKntTreeUI.ReapplyFilter;
begin
   if TKntFolder(Folder).Filtered then
      SetFilteredNodes;
end;


function TKntTreeUI.NoFindFilterMatch: boolean;
var
  Node: PVirtualNode;
begin
   for Node in TV.Nodes() do
      if GetNNode(Node).FindFilterMatch then exit(false);
   Result:= true;
end;


procedure TKntTreeUI.PopulateInheritedTags;
var
  I: integer;

  procedure PopulateInheritedTags(ParentNode : PVirtualNode; ParentInheritedTags: TNoteTagArray);
  var
     Node : PVirtualNode;
     NNode: TNoteNode;
     NEntry: TNoteEntry;
     InheritedTagsForChildren: ^TNoteTagArray;
  begin
    for Node in TV.ChildNodes(ParentNode) do begin
       if vsHasChildren in Node.States then begin
          NNode:= GetNNode(Node);
          NEntry:= NNode.Note.Entries[0];                          //%%%
          if (NEntry.Tags <> nil) then begin
             fParentNodesWithInheritedTags.Add(Node);
             InheritedTagsForChildren:= @fInheritedTags[I];
             if ParentInheritedTags <> nil then
                TNoteTagArrayUtils.AddTags(InheritedTagsForChildren^, ParentInheritedTags);
             TNoteTagArrayUtils.AddTags(InheritedTagsForChildren^, NEntry.Tags);
             inc(I);

             PopulateInheritedTags(Node, InheritedTagsForChildren^);
          end;
       end;
    end;
  end;

begin
  I:= 0;
  fParentNodesWithInheritedTags.Clear;
  fInheritedTags:= nil;
  SetLength(fInheritedTags, TV.TotalCount);
  PopulateInheritedTags(TV.RootNode, nil);
  SetLength(fInheritedTags, I);
end;


procedure TKntTreeUI.AddUseOfTags (NNode: TNoteNode; InheritedParentTags: TNoteTagArray);
var
   i: integer;
   NodeTags: TNoteTagArray;

   procedure AddUseOfTag (NTag: TNoteTag);
   var
     iTag: integer;
   begin
      iTag:= TagsInUse.IndexOf(NTag);
      if iTag >= 0 then
         TagsNumberOfUses[iTag]:= TagsNumberOfUses[iTag] + 1
      else begin
         TagsInUse.Add(NTag);
         TagsNumberOfUses.Add(1);
      end;
   end;

begin
  NodeTags:= NNode.Note.Entries[0].Tags;                  //%%%

  for i:= 0 to High(NodeTags) do
     AddUseOfTag(NodeTags[i]);

  if InheritedParentTags <> nil then
     for i:= 0 to High(InheritedParentTags) do begin
        if not TNoteTagArrayUtils.HasTag(NodeTags, InheritedParentTags[i]) then
           AddUseOfTag(InheritedParentTags[i]);
     end;
end;


procedure TKntTreeUI.PopulateNonUsedTags;
var
   i, j: integer;
   NNodes: TNoteNodeList;
   NTag: TNoteTag;
   Used: boolean;
begin
   NNodes:= TKntFolder(Folder).NNodes;
   for i:= 0 to ActiveFile.NoteTags.Count-1 do begin
       NTag:= ActiveFile.NoteTags[i];
       Used:= false;
       for j := 0 to NNodes.Count-1 do begin
           if NNodes[j].Note.Entries[0].HasTag(NTag) then begin
              Used:= True;
              break;
           end;
       end;
       if not Used then
          fTagsNonUsed.Add(NTag);
   end;

end;


procedure TKntTreeUI.SetFilteredNodes;
var
  Node, NodeParent: PVirtualNode;
  NNode: TNoteNode;
  InitialFiltered: boolean;
  ConsiderInheritedTags, ShowTagsUse: boolean;
  InheritedParentTags: TNoteTagArray;
  iNode: integer;

  function IsFilterMatch(NNode: TNoteNode): boolean;
  begin
     Result:= (fFindFilterApplied and NNode.FindFilterMatch) or
              (fTreeFilterApplied and NNode.TreeFilterMatch ) or
              (fFilterOutUnflaggedApplied and NNode.Flagged ) or
              (TagFilterApplied and NNode.MatchesTags(fFindTags));
  end;

  procedure CheckShowChildsOfNode (Node: PVirtualNode);
  var
    ChildNode : PVirtualNode;
    ChildNNode: TNoteNode;

  begin
    ChildNode:= Node.FirstChild;
    while ChildNode <> nil do begin
       ChildNNode:= GetNNode(ChildNode);
       if TV.IsFiltered[ChildNode] and not IsFilterMatch(ChildNNode) then begin      // IsFilterMatch... It will processed in main loop
          TV.IsFiltered[ChildNode]:= false;
          CheckShowChildsOfNode(ChildNode);             // Recursive
       end;
       ChildNode:= ChildNode.NextSibling;
    end;
  end;

  function AnyParentIsFlagged (Node: PVirtualNode): boolean;
  var
    ParentNode: PVirtualNode;
  begin
     Result:= false;
     ParentNode:= Node.Parent;

     if ParentNode <> TV.RootNode then begin
        if GetNNode(ParentNode).Flagged then
           Result:= true
        else
           Result:= AnyParentIsFlagged(ParentNode);
     end;
  end;


begin
   if TV.TotalCount = 0 then exit;

   TV.BeginUpdate;

   InitialFiltered:= fTreeFilterApplied or fFindFilterApplied or fFilterOutUnflaggedApplied or TagFilterApplied;
   ShowTagsUse:= Self.ShowUseOfTags and (App.TagsState = tsVisible);
   if ShowTagsUse then begin
      TagsInUse.Clear;
      TagsNumberOfUses.Clear;
      TagsNonUsed.Clear;
      PopulateNonUsedTags;
   end;

   for Node in TV.Nodes() do
      TV.IsFiltered [Node]:= InitialFiltered;

   if fFindFilterApplied then
      for Node in TV.Nodes() do begin
         NNode:= GetNNode(Node);
         if NNode.FindFilterMatch then
            MakePathNonFiltered(Node);
      end;

   if fTreeFilterApplied or fFilterOutUnflaggedApplied or TagFilterApplied or ShowTagsUse then

      ConsiderInheritedTags:= FindOptions.InheritedTags and (TagFilterApplied or ShowTagsUse);
      if ConsiderInheritedTags then
         PopulateInheritedTags;


      Node:= TV.GetFirst();
      repeat
         if not (fFindFilterApplied and TV.IsFiltered[Node]) then begin
            NNode:= GetNNode(Node);

            InheritedParentTags:= nil;
            if ConsiderInheritedTags then begin
                iNode:= fParentNodesWithInheritedTags.IndexOf(Node.Parent);
                if iNode >= 0 then
                   InheritedParentTags:= fInheritedTags[iNode];
            end;

            if not ( (not fTreeFilterApplied or NNode.TreeFilterMatch ) and
                     (not fFilterOutUnflaggedApplied or (NNode.Flagged or (FindOptions.ShowChildren and AnyParentIsFlagged(Node) ) )) and
                     (not TagFilterApplied or (NNode.MatchesTags(fFindTags, InheritedParentTags) ))
                    ) then
               TV.IsFiltered[Node]:= true

            else begin
               MakePathNonFiltered(Node);
               if ShowTagsUse then
                  AddUseOfTags(NNode, InheritedParentTags);

               if fFindFilterApplied then begin
                  NodeParent:= Node.Parent;
                  Node:= Node.NextSibling;
                  if (Node = nil) and (NodeParent <> TV.RootNode) then
                     Node:= NodeParent.NextSibling;
                  continue;
               end;
            end;
         end;
         Node:= TV.GetNext(Node);
      until Node = nil;


   if FindOptions.ShowChildren then begin    // show children of matching nodes
      Node:= TV.GetFirst();
      repeat
         if not TV.IsFiltered[Node] then begin
            NNode:= GetNNode(Node);
            if IsFilterMatch(NNode) then
               CheckShowChildsOfNode(Node);
         end;
         Node:= TV.GetNext(Node);
      until Node = nil;
   end;

   ShowFlaggedColumnForTagFilter(TagFilterApplied);

   TV.EndUpdate;
   FrameResize(nil);

   fChangesInFlagged:= false;

   TB_FilterTree.Enabled := fFindFilterApplied or fTreeFilterApplied or fFilterOutUnflaggedApplied or TagFilterApplied;
   Form_Main.MMViewFilterTree.Enabled:= TB_FilterTree.Enabled;
end;

procedure TKntTreeUI.ApplyFilterOnFolder;   // [dpv]
begin
   assert(assigned(ActiveFolder));

   ApplyFilters(ActiveFolder.Filtered);
   FilterApplied(ActiveFolder.Filtered);
end;


procedure TKntTreeUI.CheckFilterOff;
begin
   if not (FindFilterApplied or TreeFilterApplied or FilterOutUnflaggedApplied or TagFilterApplied) then begin
      TKntFolder(Folder).Filtered:= False;
      ApplyFilterOnFolder;
   end;

   CheckFocusedNode;
   TV.Invalidate;
end;

procedure TKntTreeUI.ClearFindFilter;
begin
   if not FindFilterApplied then exit;

   FindFilterApplied:= false;
   ClearAllFindMatch;
   SetFilteredNodes;

   CheckFilterOff;
end;


procedure TKntTreeUI.ExecuteTreeFiltering (ForceReapplying: boolean= True);
var
   myFindOptions: TFindOptions;
   ApplyFilter: boolean;
   Node: PVirtualNode;
   str: string;

begin
  str:= trim(txtFilter.Text).ToUpper;
  myFindOptions.Pattern:= str;
  myFindOptions.LastModifFrom:= 0;
  PreprocessTextPattern(myFindOptions);     // Can modify Pattern and set .LastModifFrom

  if (Length(str) < 3) and (myFindOptions.LastModifFrom = 0) then begin
     if TreeFilterApplied then begin
        fLastTreeSearch:= '';
        TreeFilterApplied:= false;
        ClearAllTreeMatch;
        SetFilteredNodes;
        txtFilter.Hint:= '';

        CheckFilterOff;
        FrameResize(nil);
        if Self.ShowUseOfTags then
           Form_Main.CheckFilterTags;
     end;
  end
  else
  if (str <> fLastTreeSearch) or ForceReapplying then begin
     fLastTreeSearch:= str;
     myFindOptions.LastModifUntil:= 0;
     myFindOptions.CreatedFrom:= 0;
     myFindOptions.CreatedUntil:= 0;
     myFindOptions.MatchCase := false;
     myFindOptions.WholeWordsOnly := false;
     myFindOptions.AllTabs := false;
     myFindOptions.CurrentNodeAndSubtree := false;
     myFindOptions.SearchScope := ssOnlyNodeName;
     myFindOptions.SearchMode := smAll;
     myFindOptions.CheckMode := scAll;
     myFindOptions.HiddenNodes:= false;
     myFindOptions.SearchPathInNodeNames := FindOptions.SearchPathInNodeNames;
     str:= str.ToUpper;
     if myFindOptions.LastModifFrom <> 0 then
        txtFilter.Hint:= Format(GetRS(sTree60), [FormatDateTime(FormatSettings.ShortDateFormat, myFindOptions.LastModifFrom)])
     else
        txtFilter.Hint:= '';

     RunFindAllEx (myFindOptions, true, true);

     if Self.ShowUseOfTags then
        Form_Main.CheckFilterTags;

     //CheckFocusedNode;      // RunFindAllEx (from here, and from normal Find All -with Apply Filter) will call ActivateFilter ( -> CheckFocusedNode)
  end;
end;


procedure TKntTreeUI.txtFilterChange(Sender: TObject);
begin
  ExecuteTreeFiltering (false);
end;


procedure TKntTreeUI.FilterOutUnflagged (Apply: boolean);
begin
   fFilterOutUnflaggedApplied:= Apply;
   if Apply then
      ActivateFilter

   else begin
      SetFilteredNodes;
      CheckFilterOff;
   end;
end;


procedure TKntTreeUI.CheckFocusedNode;
var
  Node: PVirtualNode;
begin
     Node:= TV.FocusedNode;
     if TV.IsEffectivelyFiltered[Node] then begin
        Node := TV.GetNextNotHidden(Node);                     // By default, IncludeFiltered=False
        if Node = nil then
           Node := TV.GetPreviousNotHidden(TV.FocusedNode);    // ,,
        if Node <> nil then
           SelectAlone(Node)
        else begin
           fLastNodeSelected:= nil;
           TKntFolder(Folder).NoNodeInTree;
           TV.TreeOptions.SelectionOptions:= TV.TreeOptions.SelectionOptions - [toAlwaysSelectNode];
        end;
     end
     else begin
        TV.TreeOptions.SelectionOptions:= TV.TreeOptions.SelectionOptions + [toAlwaysSelectNode];
        TV_FocusChanged(TV, TV.FocusedNode, -1);
     end;

     if Node <> nil then
        TV.ScrollIntoView(Node, true);
end;


procedure TKntTreeUI.ActivateFilter;
begin
  SetFilteredNodes;
  TKntFolder(Self.Folder).Filtered:= True;
  ApplyFilterOnFolder;

  CheckFocusedNode;
end;

procedure TKntTreeUI.TB_FilterTreeClick(Sender: TObject);
var
  Node: PVirtualNode;
  Folder: TKntFolder;
begin
   if FindFilterApplied and (CtrlDown or NoFindFilterMatch) then
      ClearFindFilter;

   Folder:= TKntFolder(Self.Folder);

   Folder.Filtered:= not Folder.Filtered;
   if fChangesInFlagged then
      SetFilteredNodes;
   ApplyFilters(Folder.Filtered);
   FilterApplied(Folder.Filtered);

   CheckFocusedNode;
end;

procedure TKntTreeUI.FilterApplied (Applied: boolean);   // [dpv]
var
   HintStr: string;
begin
   TB_FilterTree.Down:= Applied;
   Form_Main.MMViewFilterTree.Checked:= Applied;

   if Applied then
      HintStr:= GetRS(sTree55)
   else
      HintStr:= GetRS(sTree56);

   if FindFilterApplied then
      HintStr:= HintStr + GetRS(sTree57);
   TB_FilterTree.Hint:= HintStr;
end;


{$ENDREGION}


// Filter nodes: Tags =================================

{$REGION Filter nodes: Tags }

function TKntTreeUI.GetTagFilterApplied: boolean;
begin
   Result:= fFindTags <> nil;
end;

procedure TKntTreeUI.txtTagsEnter(Sender: TObject);
begin
   if Self.ShowUseOfTags then
      Form_Main.chkFilterOnTags.Checked:= False;

   if Self.ShowUseOfTags or CtrlDown then begin
       txtTags.Text:= '';
       fFindTags:= nil;
       SetFilteredNodes;
   end;

   TagMng.StartTxtFindTagIntrod(txtTags, OnEndFindTagsIntroduction, OnChangeFindTagsIntrod, False);
   AdjustTxtTagsWidth(True);
end;

procedure TKntTreeUI.OnChangeFindTagsIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   fFindTags:= FindTags;
   CheckFilterNotesOnTags(True);
end;

procedure TKntTreeUI.OnEndFindTagsIntroduction(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
begin
  if PressedReturn and not Self.ShowUseOfTags then
     txtFilter.SetFocus;

   fFindTags:= FindTags;
   AdjustTxtTagsWidth;
   CheckFilterNotesOnTags(false);
end;

procedure TKntTreeUI.CheckFilterNotesOnTags(Temp: boolean = True);
begin
   if Temp then
      fTempFilterTagApplying:= True;
   if fFindTags <> nil then
      ActivateFilter

   else begin
      SetFilteredNodes;
      CheckFilterOff;
   end;
   fTempFilterTagApplying:= False;
end;


procedure TKntTreeUI.AdjustTxtTagsWidth (AllowEdition: boolean = False);
const
  MinFilterWidth = 30;
  MinTagsWidth = 17;
var
  MaxAvailableWidth, MaxAvailableForTags: integer;
  TagsWidth, FilterWidth: integer;
  N: integer;

begin
  TagsWidth:=   MinTagsWidth;
  FilterWidth:= MinFilterWidth;
  if txtTags.Text <> EMPTY_TAGS then
     TagsWidth:= TagMng.GetTextWidth(txtTags.Text, txtTags) + 10;

  if TagsWidth < MinTagsWidth then
     TagsWidth:= MinTagsWidth;

  if txtFilter.Text <> '' then
     FilterWidth := TagMng.GetTextWidth(txtFilter.Text, txtFilter) + 10;

  N:= 3;
  if not TB_FilterTree.Visible then
     dec(N);
  if not TB_FilterUnflagged.Visible then
     dec(N);
  MaxAvailableWidth:= Self.Width - N*TB_FilterTree.Width -2;

  if AllowEdition then begin
     TagsWidth:= TagsWidth * 2;
     if TagsWidth < 100 then
        TagsWidth:= 100;
     FilterWidth:= MaxAvailableWidth - TagsWidth;
  end
  else begin
     FilterWidth:= MaxAvailableWidth - TagsWidth;
     if FilterWidth < MinFilterWidth then begin
        FilterWidth := MinFilterWidth;
        TagsWidth:= MaxAvailableWidth - FilterWidth;
        if TagsWidth < MinTagsWidth then
           TagsWidth := MinTagsWidth;
     end;
  end;

  txtFilter.Width:= FilterWidth;
  txtTags.Width:= TagsWidth;
  txtFilter.Left:= txtTags.Left + TagsWidth + 2;
end;


{$ENDREGION}



// Tree width expansion ======================

{$REGION Tree width expansion }


procedure TKntTreeUI.OnTreeExpanded(Expanded: boolean);
begin
   fTreeWidthExpanded:= Expanded;
   if Expanded then begin
      TKntFolder(Folder).NoteUI.SetOnMouseMoveOnNote(NoteUIMouseMove);
      TKntFolder(Folder).NoteUI.SetOnMouseUpOnNote(NoteUIMouseUp)
   end
   else begin
      TKntFolder(Folder).NoteUI.SetOnMouseMoveOnNote(nil);
      TKntFolder(Folder).NoteUI.SetOnMouseUpOnNote(nil);
   end;

   fTreeWidth_N:= 0
end;

procedure TKntTreeUI.OnAfterChangesOnTreeWidth;
var
  color: TColor;
  Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);

  if Folder.TreeMaxWidth < 0 then
     Color:= clSkyBlue
  else
     if Folder.TreeMaxWidth > 0 then
        Color:= clLtGray
     else
        Color:= clBtnFace;

   Folder.Splitter.Color:= Color;

   if KeyOptions.ModifiedOnTreeResized then
      Folder.Modified := true;
end;


procedure TKntTreeUI.SplitterNoteMoved(Sender: TObject);
var
  Folder: TKntFolder;
  W: integer;
begin
   Folder:= TKntFolder(Self.Folder);

   // This method will be called 2 times, from TSplitter.UpdateControlSize and TSplitter.StopSizing
   // We Will ignore the first one
   if not fSplitterNoteMoving then begin
      fSplitterNoteMoving:= true;
      exit;
   end;
   fSplitterNoteMoving:= false;

   if Folder.VerticalLayout then
      W := Height
   else
      W := Width;

   if AltDown then
      Folder.TreeMaxWidth:= -W                              // Enable TreeMaxWidth and set fixed state

   else begin
      if CtrlDown then begin
         Folder.TreeMaxWidth:= W;                           // Change MaxWidth
         OnTreeExpanded(True);
      end
      else begin                                            // Change normal width (MaxWidth not modified)
         Folder.TreeWidth:= W;
         Folder.TreeMaxWidth:= Abs(Folder.TreeMaxWidth);    // Disable fixed state
      end;
   end;

   if KeyOptions.AltMargins then
      Folder.Editor.Refresh;

   OnAfterChangesOnTreeWidth;

   if fColsSizeAdjusting <> 2 then
      TV_Resize(nil);
   fColsSizeAdjusting:= 0;
end;


procedure TKntTreeUI.CheckingTreeExpansion;
var
  N2: Cardinal;
begin
   if fTreeWidth_N = 0 then begin
      fTreeWidth_N:= GetTickCount;           // number of milliseconds that have elapsed since the system was started
      fTreeWidthNodeTouched:= false;
   end
   else begin
      N2:= GetTickCount;
      if N2 > (fTreeWidth_N + TREE_WIDTH_MOUSE_TIMEOUT) then
         fTreeWidth_N:= 0
      else
         if (N2 >= (fTreeWidth_N + 300)) and fTreeWidthNodeTouched then begin
            fTreeWidth_N:= 0;
            CheckExpandTreeWidth;
         end;
   end;
end;


procedure TKntTreeUI.CheckExpandTreeWidth;
var
  Folder: TKntFolder;
  W: integer;
begin
   Folder:= TKntFolder(Self.Folder);

   if Folder.VerticalLayout then
      W := Height
   else
      W := Width;

   if (Folder.TreeMaxWidth > 0) and (Folder.TreeMaxWidth > W) then begin
      if Folder.VerticalLayout then
         Height:= Folder.TreeMaxWidth
      else
         Width:= Folder.TreeMaxWidth;

      if KeyOptions.AltMargins then
         Folder.Editor.Refresh;

      TV.Refresh;
      OnTreeExpanded(True);
   end;
end;


function TKntTreeUI.CheckRestoreTreeWidth: boolean;
var
   W: integer;
   Folder: TKntFolder;
begin
   Result:= False;
   Folder:= TKntFolder(Self.Folder);

   // TV.PopupMenu = nil => Is editing node label
   if (Folder.TreeMaxWidth > 0) and (TV.Visible) and (TV.PopupMenu <> nil) and (not TV.Focused) then begin
      if Folder.VerticalLayout then
         W := Height
      else
         W := Width;

      if (W > Folder.TreeWidth) then begin
          if Folder.VerticalLayout then
             Height:= Folder.TreeWidth
          else
             Width:= Folder.TreeWidth;

          OnTreeExpanded(false);
          Result:= true;
      end;
   end;

end;


procedure TKntTreeUI.NoteUIEnter(Sender: TObject);
begin
   if not ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) then
      CheckRestoreTreeWidth;
   fTreeWidth_N:= 0;
end;


procedure TKntTreeUI.NoteUIMouseMove(Sender: TObject);
begin
   if fTreeWidthExpanded then begin
      if ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) then exit;
      CheckRestoreTreeWidth;
   end;
   fTreeWidth_N:= 0;
end;

procedure TKntTreeUI.NoteUIMouseUp(Sender: TObject);
begin
   if fTreeWidthExpanded then
      CheckRestoreTreeWidth;
   fTreeWidth_N:= 0;
end;


procedure TKntTreeUI.TV_Click(Sender: TObject);
var
   Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);
  if AltDown then begin
     Folder.TreeMaxWidth:= -Folder.TreeMaxWidth;        // Toggle fixed state
     OnTreeExpanded(Folder.TreeMaxWidth > 0);
  end
  else
  if CtrlDown then begin
     CheckRestoreTreeWidth;
     Folder.TreeMaxWidth:= 0;                           // Disable TreeMaxWidth
  end;

  OnAfterChangesOnTreeWidth;
end;


procedure TKntTreeUI.TV_MouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
begin
   if not CtrlDown then
      CheckingTreeExpansion
   else
      fTreeWidth_N:= Cardinal.MaxValue - TREE_WIDTH_MOUSE_TIMEOUT;
end;

procedure TKntTreeUI.TV_MouseLeave(Sender: TObject);
begin
   fTreeWidthNodeTouched:= False;
   fTreeWidth_N:= 0;
end;

procedure TKntTreeUI.TV_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
begin
  { *1 We make sure that CheckingTreeExpansion doesn't end up calling CheckExpandTreeWidth
       until we initialize TreeWidth_N to 0 (from RTFMouseMove and CheckRestoreTreeWidth) }

   if KntTreeOptions.ShowTooltips then
      HintText:= GetNNode(Node).NodeName(Self);

   if not CtrlDown then begin
      fTreeWidthNodeTouched:= True;
      CheckingTreeExpansion;
   end
   else
      fTreeWidth_N:= Cardinal.MaxValue - TREE_WIDTH_MOUSE_TIMEOUT;   // *1

end;


procedure TKntTreeUI.DoEnter;
begin
   CheckNotifyAccesibleFocusedItem (EVENT_OBJECT_FOCUS);

   TKntFolder(Folder).NoteUI.SetOnMouseMoveOnNote(nil);
   App.TreeFocused(Self);
   if not ( (CtrlDown or AltDown) and ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) ) then
      CheckExpandTreeWidth;

  inherited;
end;

procedure TKntTreeUI.CheckNotifyAccesibleFocusedItem (event: CARDINAL);
begin
    if TV.Accessible <> nil then begin
       TVirtualTreeAccessibility(TV.Accessible).IgnoreHoverNode;
       NotifyWinEvent(event, TV.Handle, OBJID_CLIENT, CHILDID_SELF);
    end;
end;


{$ENDREGION}



// Virtual Nodes ========================================================================

{$REGION Virtual nodes}

procedure TKntTreeUI.VirtualNoteUnlink;
begin
  TKntFolder(Self.Folder).VirtualNoteUnlink;
end;


procedure TKntTreeUI.VirtualNoteRefresh( const DoPrompt : boolean );
var
  Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);
  Folder.VirtualNoteRefresh(DoPrompt);
end;


{$ENDREGION}



//=======================================================================
//  TVirtualStringTreeHelper
//=======================================================================

{$REGION TVirtualStringTreeHelper }

function TVirtualStringTreeHelper.GetNextVisibleNotChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetNext(Result);
  until not Assigned(Result) or
     ((vsVisible in Result.States) and (IncludeFiltered or not IsEffectivelyFiltered[Result])
       and not HasAsParent(Result, Node)
     );
end;

function TVirtualStringTreeHelper.GetNextNotChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetNext(Result);
  until not Assigned(Result) or
     ((IncludeFiltered or not IsEffectivelyFiltered[Result]) and not HasAsParent(Result, Node)
     );
end;


function TVirtualStringTreeHelper.GetNextNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
{
 // Returns the next visible after Node (independant of the expand states of its parents)

{
 <<TVirtualNodeState.vsVisible,    // Indicate whether the node is visible or not (independant of the expand states of its parents).>>

 Methods and enums that return visible nodes appear to be based on vsVisible (apart from optionally
 in Filtered) and respect that observation ("independent of the expanded states of its parents")
 For example, VisibleNodes returns nodes marked as visible even though their parents can
 not be expanded and therefore FullyVisible = False.
 The same goes for methods like GetNextVisibleSibling(Node), GetFirstVisibleChild(Node), GetPreviousVisibleSibling, ..
 However TV.GetNextVisible(Node) only returns those nodes marked as visible
 *where FullyVisible = True is true*, as its parents are expanded
 For that reason I added this method "GetNextNotHidden"

 --
 Note: The VisibleNodes enumeration returns all visible nodes starting from a given one, not limited to
 subtree corresponding to the indicated node. For the latter, IterateSubtree could be used.}

begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetNext(Result);
  until not Assigned(Result) or ((vsVisible in Result.States) and
        (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;


function TVirtualStringTreeHelper.GetPreviousNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := GetPrevious(Result);
  until not Assigned(Result) or ((vsVisible in Result.States) and
        (IncludeFiltered or not IsEffectivelyFiltered[Result]));
end;

function TVirtualStringTreeHelper.GetNextNotChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := inherited GetNextChecked(Result, csUncheckedNormal);
  until not Assigned(Result) or ((vsVisible in Result.States) and not IsEffectivelyFiltered[Result]);
end;


function TVirtualStringTreeHelper.GetNextChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
begin
  Assert(Assigned(Node) and (Node <> RootNode), 'Invalid parameter.');

  Result := Node;
  repeat
    Result := inherited GetNextChecked(Result);
  until not Assigned(Result) or ((vsVisible in Result.States) and not IsEffectivelyFiltered[Result]);
end;


function TVirtualStringTreeHelper.IsVerticalScrollBarVisible: boolean;
begin
   Result:= inherited RangeY > ClientHeight;
end;


{$ENDREGION}


// =============================================
// TNodeList
// =============================================

constructor TNodeList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TNodeList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TNodeList.Add(const Node: PVirtualNode): integer;
begin
  Result:= FList.Add(Node);
end;

function TNodeList.IndexOf(const Node: PVirtualNode): Integer;
begin
   Result:= FList.IndexOf(Node);
end;

procedure TNodeList.Clear;
begin
   FList.Clear;
end;

function TNodeList.Count: integer;
begin
   Result:= FList.Count;
end;


initialization

end.

