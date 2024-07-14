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
  System.SysUtils, System.Variants, System.Classes,
  System.Contnrs, System.Actions,  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Clipbrd,
  Vcl.ActnList, Vcl.StdCtrls,

  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,

  RxRichEd,
  kn_Info,
  kn_Const,
  knt.model.note,
  VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL, System.ImageList,
  Vcl.ImgList
  ;


type
   TVirtualStringTreeHelper = class helper for TVirtualStringTree
   public
     function GetPreviousNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
     function GetNextNotHidden(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
     function GetNextChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
     function GetNextNotChecked(Node: PVirtualNode; ConsiderHiddenNodes: boolean= true): PVirtualNode;
     function GetNextVisibleNotChild(Node: PVirtualNode; IncludeFiltered: Boolean = False): PVirtualNode;
   end;


  TTreeTransferAction = (ttCopy, ttCut, ttPaste, ttClear);

  TVTree = TVirtualStringTree;
  TNodeList = TList<PVirtualNode>;


  TKntTreeUI = class(TFrame)
    TV: TVirtualStringTree;
    CheckImages: TImageList;

  private class var
    fSourceTVSelectedNodes: TNodeArray;
    fiNextSourceTVNode: integer;
    fCutSubtrees: boolean;                       // Copy or cut operation
    fCopyingAsLinked: boolean;
    fTVCopiedNodes: TNodeList;
    fVirtualNodesConvertedOnCopy: integer;
    fMovingToOtherTree: boolean;
    fMovingToFolder: TFolderObj;
    fNNodesInSubtree: TNoteNodeArray;
    fDropTargetNode: PVirtualNode;
    fDropTargetNodeInsMode: TNodeInsertMode;

    fTreeWidthExpanded: boolean;
    fTreeWidth_N: Cardinal;
    fTreeWidthNodeTouched: boolean;
    fSplitterNoteMoving: boolean;

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

    fReadOnly: boolean;
    fLastNodeSelected : PVirtualNode;
    fNumberingDepthLimit: Byte;

    fFindFilterApplied: boolean;
    fTreeFilterApplied: boolean;

  protected
    // Create. Destroy
    procedure SetFolder(aFolder: TObject);
    procedure SetSplitterNote(aSplitter: TSplitter);
    procedure SetPopupMenu(value: TPopupMenu);

    // TreeView Handlers
    procedure TV_SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    procedure TV_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
                                Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure TV_CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    //procedure TV_SavingTree(Sender: TObject; Node: PVirtualNode; var S: string);

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
    procedure SetupTVHandlers;

  public
    procedure UpdateTreeChrome;

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
    procedure SetNodeColor(Node: PVirtualNode; const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean);
    procedure SetNodeBold(Node: PVirtualNode; const DoChildren : boolean);
    procedure SetNodeCustomImage(Node: PVirtualNode);
    function GetNodeFontFace (Node: PVirtualNode): string;
    procedure SetNodeFontFace(Node: PVirtualNode; const ResetDefault, DoChildren: boolean);
  //procedure SetNodeFontSize(TreeNode: PVirtualNode; const ResetDefault, DoChildren : boolean);

  public
    function GetNodePath(aNode: PVirtualNode; const aDelimiter: string; const TopToBottom: boolean) : string;
    procedure CopyNodePath(myTreeNode : PVirtualNode; const InsertInEditor : boolean);
    procedure PasteNodeName(Node : PVirtualNode; const PasteMode : TPasteNodeNameMode);
    procedure CopyNodeName(const IncludeNoteText: boolean);
    procedure RenameFocusedNode;
   protected
    procedure TV_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
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
    procedure SetupNewTreeNode(const Node : PVirtualNode);
    procedure SetupLoadedTreeNode(const Node : PVirtualNode);

    // Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees  | Drag and Drop
   public
    procedure MoveTreeNode(MovingNode : PVirtualNode; const aDir : TDirection);
    procedure DeleteNode(myTreeNode: PVirtualNode; const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true);
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
  protected
    procedure ShowOrHideChildrenCheckBoxes(const NNode : TNoteNode);
    procedure ChangeCheckedState(Node: PVirtualNode; Checked: Boolean);


    // Filter nodes
  public
    procedure MakePathVisible (Node: PVirtualNode);
    procedure MakePathNonFiltered (Node: PVirtualNode);
    procedure ClearAllFindMatch;
    procedure SetFilteredNodes;
    procedure ApplyFilters (Apply: boolean);
    property FindFilterApplied: boolean read fFindFilterApplied write fFindFilterApplied;
    property TreeFilterApplied: boolean read fTreeFilterApplied write fTreeFilterApplied;

    // Tree width expansion
  public
    function CheckRestoreTreeWidth: boolean;
  protected
    procedure OnAfterChangesOnTreeWidth;
    procedure SplitterNoteMoved(Sender: TObject);
    procedure CheckingTreeExpansion;
    procedure CheckExpandTreeWidth;
    procedure RxRTFEnter(Sender: TObject);
    procedure RTFMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RTFMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TV_Click(Sender: TObject);
    procedure TV_MouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
    procedure TV_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure DoEnter; override;


    // Virtual nodes
  public
    procedure VirtualNoteRefresh( const DoPrompt : boolean );
    procedure VirtualNoteUnlink;

  end;



implementation
uses
   gf_strings,
   gf_streams,
   gf_miscvcl,
   gf_misc,
   gf_files,
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
   Knt.App
   ;

{$R *.dfm}

resourcestring
  STR_01 = 'Error creating node: ';
  STR_04 = 'Initial node not assigned - select a node and retry.';
  STR_05 = 'cannot be ';
  STR_06 = 'Error moving node: ';
  STR_07 = 'Node "%s" %smoved %s';
  STR_08 = #13#10 + 'This operation cannot be undone.';
  STR_09 = 'Node "%s" has %d child nodes. Delete these child nodes too?';
  STR_10 = 'Warning';
  STR_11 = 'OK to delete node "%s"?';
  STR_12 = 'OK to delete %d CHILD NODES of node "%s"?';
  STR_13 = 'Selected node has no children.';
  STR_14 = 'Error deleting node: ';
  STR_15 = 'No nodes available for copying or pasting data.';
  STR_16 = 'OK to MOVE %d nodes/subtrees to current node "%s"?';
  STR_17 = ' No node is selected';
  STR_18 = 'OK to forget %s?';
  STR_19 = 'Target node is included in one of the subtrees to move';
  STR_20 = ' nodes/subtrees registered for transfer';
  STR_21 = 'No data to paste. Select "Transfer|Copy/Cut Subtree" first.';
  STR_22 = 'One or more nodes being transferred is a Virtual Node. They will be pasted as linked node' + #13#13 + 'Continue?';

  STR_23 = 'OK to PASTE %d nodes/subtrees%s below current node "%s"?' + #13#10 + '(Only not hidden nodes will be pasted)';
  STR_26 = ' as LINKED nodes';

  STR_24 = ' Pasted %d nodes/subtrees';
  STR_25 = '%d virtual nodes have been copied as linked nodes';
  STR_27 = 'Node not found (Folder ID/Node ID): %d/%d';

  STR_30 = 'Copy';
  STR_31 = 'Move';
  STR_32 = 'Link';

  STR_49 = 'OK to sort the entire tree?';
  STR_50 = ' Node name cannot be blank!';
  STR_51= ' Node renamed.';
  STR_52= ' Cannot perform operation: Tree is read-only';
  STR_53 = 'Edit node name';
  STR_54 = 'Enter new name:';


const
  TREE_DEFAULT_WIDTH = 200;
  TREE_WIDTH_MOUSE_TIMEOUT = 2500;






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

   fNumberingDepthLimit:= 2;


   with TV do begin
     DefaultText := DEFAULT_NEW_NOTE_NAME;

     // static options that do not change:
     {  %%%
     SortType := TSortType(stNone); // MUST be stNone; sort by manually calling AlphaSort
     TreeOptions := [
                 //toMultiSelect,                // [dpv]  <<<<<<<<<< PROVISIONAL
                 toRightClickSelect,
                 toInfoTip,
                 // toFullRowSelect,
                 // toHideSelection,
                 // toReadOnly,
                 toToolTips,
                 // toHotTrack, OPTION!
                 toShowButtons,
                 toShowLines,
                 toShowRoot,
                 toEvenHeight,
                 toCheckSupport];
     }


     HelpContext:= 284;  // Tree-type Notes [284]
   end;

end;


destructor TKntTreeUI.Destroy;
begin
   TV.OnChange := nil;
   TV.Free;

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
begin
  fFolder:= aFolder;
  Folder:= TKntFolder(fFolder);

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

   UpdateTreeOptions;

   PopulateTV;
   SetupTVHandlers;

   if Folder.TreeMaxWidth > Folder.TreeWidth then
      SplitterNote.Color:= clLtGray;
end;


procedure TKntTreeUI.UpdateTreeOptions;
begin
  // Updates options for current folder's tree based on global tree options

  with TV do begin
      DragMode := dmAutomatic;

      TreeOptions.MiscOptions := [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave,
                                  toToggleOnDblClick, toWheelPanning, toEditOnClick];
      TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons,
                                  toAutoDeleteMovedNodes, toAutoChangeScale];
      TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toAlwaysSelectNode, toSelectNextNodeOnRemoval];
      TreeOptions.StringOptions := [toAutoAcceptEditChange];

      // Default:
      // TreeOptions.PaintOptions := [toShowButtons,toShowDropmark,toShowRoot,toShowTreeLines,toThemeAware,toUseBlendedImages]
      // TreeOptions.AnimationOptions := [];
      // EditOptions:= toDefaultEdit
      // ExportMode:= emAll

      TreeOptions.PaintOptions:= TreeOptions.PaintOptions - [TVTPaintOption.toUseBlendedImages];

//    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoExpand];

      if KntTreeOptions.EditInPlace then
        TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable];

      if KntTreeOptions.AutoScroll then
        TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoScroll];

      if KntTreeOptions.HotTrack then
        TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toHotTrack];

     if KntTreeOptions.FullRowSelect then
        TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];

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
              SetupLoadedTreeNode(myTreeNode);
              NNode.TVNode:= myTreeNode;
           end;


        finally
          Log_StoreTick( 'After created TreeNodes', 3 );

          myFolder.LoadingLevels.Clear;

          ShowOrHideIcons;

          if myFolder.Filtered then
             SetFilteredNodes;
          if myFolder.HideCheckedNodes then
             HideChildNodesUponCheckState (nil, true);

          EndUpdate;

          Log_StoreTick( 'After HideFilteredNodes, HideCheckNodes', 3 );
        end;

        // restore selected node: this block must be
        // OUTSIDE the beginupdate..endupdate range

        tNode:= nil;
        if TV.VisibleCount > 0 then begin
          if (( KntTreeOptions.ExpandMode <> txmFullCollapse ) and // SaveActiveNode and
             ( myFolder.SavedSelectedIndex >= 0 ) and
             ( myFolder.SavedSelectedIndex < TV.TotalCount )) then
            // restore the node which was selected when file was saved
            tNode:= myFolder.NNodes[myFolder.SavedSelectedIndex].TVNode;

          if (tNode = nil) or (not TV.IsVisible[tNode]) then begin
            tNode := GetFirst;
            if not TV.IsVisible[tNode] then tNode:= GetNextNotHidden(tNode);
          end;
          SelectAlone(tNode);
        end;

        Log_StoreTick( 'After Restored selected node', 3 );


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


procedure TKntTreeUI.SetupTVHandlers;
begin
   with TV do begin
     OnGetText:= TV_GetText;
     OnPaintText:= TV_PaintText;
     OnGetImageIndex := TV_GetImageIndex;
     OnBeforeCellPaint:= TV_BeforeCellPaint;


     OnKeyDown := TV_KeyDown;
     OnKeyPress := TV_KeyPress;
     OnChange := TV_SelectionChange;          // selection change
     OnFocusChanged:= TV_FocusChanged;        // called when the focus goes to a new node and/or column

     OnChecked := TV_Checked;
     OnEditing := TV_Editing;
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
     OnCompareNodes:= TV_CompareNodes;

   end;

   with TKntFolder(Folder).Editor do begin
     OnEnter:= RxRTFEnter;
     OnMouseMove := RTFMouseMove;
     OnMouseUp := RTFMouseUp;
   end;

end;

{$ENDREGION}



procedure TKntTreeUI.UpdateTreeChrome;
var
  Folder: TKntFolder;
begin
   Folder:= TKntFolder(Self.Folder);

   TV.Color := Folder.TreeChrome.BGColor;
   FontInfoToFont(Folder.TreeChrome.Font, TV.Font);
   TV.Invalidate;
end;


function TKntTreeUI.CheckReadOnly: boolean;
begin
    Result:= False;

    if ReadOnly then begin
       App.ShowInfoInStatusBar(STR_52);
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


procedure TKntTreeUI.TV_GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  // Column: main column: -1 if columns are hidden, 0 if they are shown

  CellText:= GetNodeCaption(Node);
end;


procedure TKntTreeUI.TV_BeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
                                        Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NNode: PNoteNode;
  CellR: TRect;
begin
  if vsSelected in Node.States then exit;
  if CellPaintMode = cpmGetContentMargin then exit;

  NNode := Sender.GetNodeData(Node);

  if NNode.NodeBGColor <> clNone then begin
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
  if fFindFilterApplied and NNode.FindFilterMatch then
     Color:= clBlue
  else
     Color:= NNode.NodeColor;

  if Color <> clNone then
     TargetCanvas.Font.Color := Color;
  if NNode.NodeFontFace <> '' then
     TargetCanvas.Font.Name := NNode.NodeFontFace;

end;



procedure TKntTreeUI.TV_GetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
   NNode: TNoteNode;
begin
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



procedure TKntTreeUI.TV_FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if (not assigned(Node)) then exit;

  if (Node <> fLastNodeSelected) then begin
     TKntFolder(Folder).NodeSelected(Node, fLastNodeSelected);
     fLastNodeSelected:= Node;
     App.NNodeFocused(GetNNode(Node));
  end;
end;


procedure TKntTreeUI.TV_SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
end;


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
      TKntFolder(Folder).SetFocusOnEditor;
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


procedure TKntTreeUI.SetNodeColor(Node: PVirtualNode; const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean);
var
  NodeColor, DefaultColor, NewColor: TColor;
  NNode : TNoteNode;

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

     SetColor(Node);
     TV.InvalidateNode(Node);
     if DoChildren then
        TV.InvalidateChildren(Node, true);

  finally
     TKntFolder(Folder).Modified:= true;
  end;

end;


procedure TKntTreeUI.SetNodeBold(Node: PVirtualNode; const DoChildren : boolean);

   procedure BoldNNode (Node: PVirtualNode);
   var
     NNode : TNoteNode;
   begin
     NNode:= GetNNode(Node);
     NNode.Bold := (not NNode.Bold);
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


procedure TKntTreeUI.SetNodeCustomImage(Node: PVirtualNode);
var
  NewIdx, ImgIdx : integer;
  DoChildren : boolean;
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

  DoChildren := Node.ChildCount > 0;
  NNode:= GetNNode(Node);
  ImgIdx := NNode.ImageIndex;
  newIdx := PickImage(ImgIdx, DoChildren);

  if (newIdx = -1) or ((newIdx = ImgIdx) and not DoChildren) then exit;

  SetCustomImage(Node);
  TV.InvalidateNode(Node);
  if DoChildren then
    TV.InvalidateChildren(Node, true);
  TKntFolder(Folder).Modified:= true;
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


procedure TKntTreeUI.SetNodeFontFace(Node: PVirtualNode; const ResetDefault, DoChildren: boolean);
var
  myFontFace : string;
  SetFontInSubtree: TVTGetNodeProc;

begin

  SetFontInSubtree :=
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    begin
      Abort := not DoChildren;                           // Continue iteration?
      GetNNode(Node).NodeFontFace:= myFontFace;
    end;

  if not Assigned(Node) then
     Node := TV.FocusedNode;
  if not Assigned(Node) or CheckReadOnly then exit;

  try
    myFontFace:= '';
    if not ResetDefault then
        myFontFace := Form_Main.Combo_Font.FontName;
    TV.IterateSubtree(Node, SetFontInSubtree, nil);
    TV.InvalidateNode(Node);
    if DoChildren then
       TV.InvalidateChildren(Node, true);

  finally
    TKntFolder(Folder).Modified:= true;
  end;

end; // SetNodeFontFace


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
     TV.EditNode(Node, -1)

  else begin
     myNote := NNode.Note;
     myName := myNote.Name;
     if InputQuery( STR_53, STR_54, myName ) then begin
        myName := trim( myName );
        if (myName <> '') then begin
           myNote.Name := myName;
           TV.InvalidateNode(Node);
           TKntFolder(Folder).Modified := true;
        end
        else
           App.ErrorPopup(STR_50);
     end;
  end;

end;


procedure TKntTreeUI.TV_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  if not CheckReadOnly then
    TV.PopupMenu := nil       // stop menu events triggered by shortcut keys
  else
    Allowed := false;
end;

{ Never called in VirtualTree...
  Suppose related with issue #896 (https://github.com/JAM-Software/Virtual-TreeView/issues/896):
    Merge DoCancelEdit() and DoEndEdit() in one procedure with boolean parameter
procedure TKntTreeUI.TVEditCanceled(Sender: TBaseVirtualTree; Column: TColumnIndex);
begin
  TV.PopupMenu := PopupMenu;
end;
}


// Called in VirtualTree, when edit ended ok and also when was cancelled..
procedure TKntTreeUI.TV_Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TV.PopupMenu := PopupMenu;
  if GetNNode(Node).NumberingMethod <> NoNumbering then
     TV.ReinitNode(Node, false);
end;


procedure TKntTreeUI.TV_NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    NewText: string);
begin
  NewText := Trim(copy(NewText, 1, TREENODE_NAME_LENGTH));

  if (NewText = '' ) then begin
    App.ShowInfoInStatusBar(STR_50);
    exit;
  end;

  _ALLOW_VCL_UPDATES := false;
  try
    GetNNode(Node).Note.Name:= NewText;        // {N} must add outline numbering, if any
    App.ShowInfoInStatusBar(STR_51);
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
        messagedlg(STR_04, mtError, [mbOK], 0);
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

  if (MessageDlg(STR_49, mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then exit;

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
         TV.Expanded[Node]:= false;
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
        messagedlg(STR_01 + E.Message, mtError, [mbOK], 0);
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
    App.ShowInfoInStatusBar(STR_17);
    exit;
  end;

  myNodeName:= FirstLineFromString(TrimLeft(ActiveEditor.SelText), TREENODE_NAME_LENGTH_CAPTURE);
  myRTFText := ActiveEditor.RtfSelText;

  NNode := NewNode(tnAddBelow, nil, '', true);
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
      TKntFolder(Folder).DataStreamToEditor;

      MakePathVisible(NNode.TVNode);

    finally
      TKntFolder(Folder).Modified := true;
    end;
  end;

end; // CreateNodefromSelection


procedure TKntTreeUI.SetupNewTreeNode(const Node : PVirtualNode);
var
  NNode : TNoteNode;
begin
   NNode:= GetNNode(Node);

   if NNode.ChildrenCheckbox or ShowAllCheckboxes then       // sets the check type for this node's children and changes the children's check image
      Node.CheckType  := ctCheckBox
   else
      Node.CheckType  := ctNone;
end;


procedure TKntTreeUI.SetupLoadedTreeNode(const Node : PVirtualNode);
var
  NNode : TNoteNode;
begin
   NNode:= GetNNode(Node);

   if NNode.ChildrenCheckbox or ShowAllCheckboxes then       // sets the check type for this node's children and changes the children's check image
      Node.CheckType  := ctCheckBox
   else
      Node.CheckType  := ctNone;

   if nnsSaved_Checked in NNode.States then
      Node.CheckState := csCheckedNormal
   else
      Node.CheckState := csUncheckedNormal;

end;


{$ENDREGION}


// Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees | Drag and Drop

{$REGION Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees}

procedure TKntTreeUI.MoveTreeNode(MovingNode : PVirtualNode; const aDir : TDirection);
var
  t : string;
  PreviousParent, theSibling : PVirtualNode;

begin
  if not Assigned(MovingNode) then
     MovingNode := TV.FocusedNode;
  if not Assigned(MovingNode) or CheckReadOnly then exit;

  t := STR_05;

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
            // becomes its parent's sibling
            TV.MoveTo(MovingNode, MovingNode.Parent, TVTNodeAttachMode.amInsertAfter, False);
            SetNumberingMethod(MovingNode);
            t := '';
          end;
        end;
        dirRight : begin
          theSibling := TV.GetPreviousSibling(MovingNode);
          if assigned(theSibling) then begin
            // becomes the last child of its previous sibling
            TV.MoveTo(MovingNode, theSibling, TVTNodeAttachMode.amAddChildLast, False);
            SetNumberingMethod(MovingNode);
            TV.Expanded[theSibling]:= true;
            t := '';
          end;
        end;
      end;


      if (t = '') then begin // means node was successfully moved
        // update node icon
        TV.Invalidate;
      end;

    except
      on E : Exception do
        App.ErrorPopup(E, STR_06);
    end;
  finally
    TKntFolder(Folder).Modified:= true;
    TV.EndUpdate;

    App.ShowInfoInStatusBar(Format(STR_07, [GetNNode(MovingNode).NoteName, t, DIRECTION_NAMES[aDir]]));
  end;

end; // MoveTreeNode


procedure TKntTreeUI.DeleteNode(myTreeNode: PVirtualNode; const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true);
var
  myTreeParent, myTreeChild, myNextChild : PVirtualNode;
  NNode: TNoteNode;
  KeepChildNodes : boolean;
  Folder: TKntFolder;

begin
  with Form_Main do begin
      KeepChildNodes := false;

      if CheckReadOnly then exit;

      if not Assigned(myTreeNode) then
         myTreeNode := TV.FocusedNode;
      if not Assigned(myTreeNode) then exit;

      NNode:= GetNNode(myTreeNode);
      myTreeParent := myTreeNode.Parent;
      Folder:= TKntFolder(Self.Folder);

      if AskForConfirmation then begin

         if DeleteOnlyChildren then begin
            // command was to delete CHILDREN of focused node
            if vsHasChildren in myTreeNode.States then begin
              if (DoMessageBox(Format(STR_12, [myTreeNode.ChildCount, NNode.NodeName(Self)]) + STR_08, STR_10,
                   MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES) then exit;
            end
            else begin
              showmessage(STR_13);
              exit;
            end;
         end
         else begin
            // delete focused node and all its children, if any

            if vsHasChildren in myTreeNode.States then begin
              // ALWAYS warn if node has children (except if we are moving to another location)
              case DoMessageBox(
                Format(STR_09, [NNode.NodeName(Self), myTreeNode.ChildCount]) + STR_08, STR_10,
                    MB_YESNOCANCEL+MB_ICONEXCLAMATION+MB_DEFBUTTON3+MB_APPLMODAL) of
                ID_YES : KeepChildNodes := false;
                ID_NO  : KeepChildNodes := true;
                else
                  exit;
              end;
            end
            else begin
              if KntTreeOptions.ConfirmNodeDelete then begin
                if (App.DoMessageBox(Format(STR_11, [NNode.NodeName(Self)]) + STR_08, mtWarning, [mbYes,mbNo], 0) <> mrYes) then exit;
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
            App.ErrorPopup(E, STR_14);
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
  NNode: TNoteNode;
  SourceTV: TBaseVirtualTree;
  i: integer;

begin
   NNode:= GetNNode(TargetNode);

   SourceTV:= TreeFromNode(fSourceTVSelectedNodes[0]);
   Prompt:= (Prompt or ((TV <> SourceTV) and KeyOptions.DropNodesOnTabPrompt));

   if PasteAsLinkedNNode then begin
      if Prompt and (App.DoMessageBox(Format(STR_23, [Length(fSourceTVSelectedNodes), STR_26, NNode.NodeName(Self)]),
                     mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
            exit;
   end
   else begin
       if HasVirtualNotesInSelectedSubtrees then begin
         if Prompt and (App.DoMessageBox(STR_22, mtWarning, [mbYes,mbNo], 0) <> mrYes) then
            exit;
       end
       else begin
          if Prompt and (App.DoMessageBox(Format(STR_23, [Length(fSourceTVSelectedNodes), '', NNode.NodeName(Self)]),
                            mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then // {N}
            exit;
       end;
   end;

   fCopyingAsLinked:= PasteAsLinkedNNode;
   TV.BeginUpdate;

   for i := 0 to High(fSourceTVSelectedNodes) do
      SourceTV.CopyTo(fSourceTVSelectedNodes[i], TargetNode, AttachMode, False);

   TV.Expanded[TargetNode]:= true;
   SelectAlone(TargetNode);
   TV.EndUpdate;
   fiNextSourceTVNode:= 0;
   fTVCopiedNodes.Clear;
   App.ShowInfoInStatusBar(Format(STR_24, [Length(fSourceTVSelectedNodes)]));
   if (fVirtualNodesConvertedOnCopy > 0) then
      App.InfoPopup(Format(STR_25,[fVirtualNodesConvertedOnCopy]));

  TKntFolder(Folder).Modified:= true;
end;



procedure TKntTreeUI.MoveSubtrees (TargetNode: PVirtualNode; Prompt: boolean; AttachMode: TVTNodeAttachMode= amAddChildLast);
var
 SourceFolder: TKntFolder;
 SourceTreeUI: TKntTreeUI;
 SourceTV: TBaseVirtualTree;
 i: integer;

begin
  if IsIncludedInSelectedSubtrees(TargetNode) then begin
     App.WarningPopup(STR_19);
     exit;
  end;

  SourceFolder:= ActiveFile.GetFolderByTreeNode(fSourceTVSelectedNodes[0]);
  Assert(assigned(SourceFolder));
  if SourceFolder.TreeUI.CheckReadOnly then exit;

  SourceTV:= TreeFromNode(fSourceTVSelectedNodes[0]);
  Prompt:= (Prompt or ((TV <> SourceTV) and KeyOptions.DropNodesOnTabPrompt));

  if Prompt and (App.DoMessageBox(Format(STR_16,  [Length(fSourceTVSelectedNodes), GetNNode(TargetNode).NodeName(Self)]),
                       mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
      exit;

  fMovingToOtherTree:= (SourceTV <> TV);
  fMovingToFolder:= Folder;

  for i := 0 to High(fSourceTVSelectedNodes) do begin
     SourceTV.MoveTo(fSourceTVSelectedNodes[i], TargetNode, AttachMode, False);
     if not fMovingToOtherTree then
        SetNumberingMethod(fSourceTVSelectedNodes[i]);
  end;

  SelectAlone(TargetNode);
  TV.Expanded[TargetNode]:= true;

  fSourceTVSelectedNodes:= nil;
  fNNodesInSubtree:= nil;
  fCutSubtrees:= false;
  fMovingToOtherTree:= false;

  SourceFolder.Modified:= true;
  TKntFolder(Folder).Modified:= true;
end;


function TKntTreeUI.TreeTransferProc(XferAction: TTreeTransferAction; Prompt: boolean; PasteAsVirtualKNTNode: boolean) : boolean;
var
  FocNode: PVirtualNode;

begin

   case XferAction of
      ttClear: begin
        result := true;
        if (messagedlg(Format(STR_18, [STR_20]), mtConfirmation, [mbYes,mbNo], 0) = mrYes) then begin
           fSourceTVSelectedNodes:= nil;
           fTVCopiedNodes.Clear;
           fiNextSourceTVNode:= 0;
           fVirtualNodesConvertedOnCopy:= 0;
           exit;
        end;
      end;
   
      ttCopy, ttCut: begin
         if (TV.SelectedCount <= 0) then begin
           App.InfoPopup(STR_15);
           exit;
         end;
         GetSelectedSubtrees (TV);
         App.ShowInfoInStatusBar(Length(fSourceTVSelectedNodes).ToString + STR_20);
         fCutSubtrees:= (XferAction = ttCut);
      end;

      ttPaste: begin
         FocNode:= TV.FocusedNode;
         if (FocNode = nil) or (fSourceTVSelectedNodes = nil) then begin
            App.InfoPopup(STR_21);
            exit;
         end;
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

begin
   SrcNode:= fSourceTVSelectedNodes[fiNextSourceTVNode];
   inc(fiNextSourceTVNode);
   fTVCopiedNodes.Add(Node);

   NodeS:= SrcNode;
   NodeC:= Node;
   repeat
      NNodeS:= GetNNode(NodeS);
      if fCopyingAsLinked or NNodeS.Note.IsVirtual then begin
         NNodeC:= TKntFolder(Folder).AddNewNNode(NNodeS.Note, NNodeS);
         if not fCopyingAsLinked then
            inc(fVirtualNodesConvertedOnCopy);
      end
      else
         NNodeC:= TKntFolder(Folder).AddNewNote(NodeS);

      SetNNode(NodeC, NNodeC);
      NNodeC.TVNode:= NodeC;
      SetNumberingMethod(NodeC);
      NodeS:= TV.GetNextNotHidden(NodeS);           // *1 See remark on GetNextNotHidden on why to use it instead of GetNextVisible
      if fTVCopiedNodes.IndexOf(NodeS) >= 0 then    // It is being pasted within the same tree it was copied from
         NodeS:= TV.GetNextVisibleNotChild(NodeS);

      NodeC:= TV.GetNextNotHidden(NodeC);
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
         SetNumberingMethod(Node);
         NNode.Note.UpdateFolderInNNode(NNode, fMovingToFolder);
         TKntFolder(fFolder).RemoveNNode(NNode);
         TKntFolder(fMovingToFolder).AddNNode(NNode);
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
  Accept := true;
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
   for Node in TV.Nodes do
      if ShowAllCheckboxes then
         Node.CheckType:= ctCheckBox
      else
         ShowOrHideChildrenCheckBoxes(GetNNode(Node));

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
   for Node in TV.ChildNodes(NNode.TVNode) do
      Node.CheckType:= ChkType;
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


procedure TKntTreeUI.ApplyFilters (Apply: boolean);
begin
   if Apply then
      TV.TreeOptions.PaintOptions := TV.TreeOptions.PaintOptions - [TVTPaintOption.toShowFilteredNodes]
   else
      TV.TreeOptions.PaintOptions := TV.TreeOptions.PaintOptions + [TVTPaintOption.toShowFilteredNodes];

end;


procedure TKntTreeUI.SetFilteredNodes;
var
  Node : PVirtualNode;
  NNode: TNoteNode;
begin
   TV.BeginUpdate;

   for Node in TV.Nodes() do
      TV.IsFiltered [Node]:= True;

   for Node in TV.Nodes() do begin
      NNode:= GetNNode(Node);
      if (not fFindFilterApplied or NNode.FindFilterMatch) and (not fTreeFilterApplied or NNode.TreeFilterMatch) then
         MakePathNonFiltered(Node);
   end;

   TV.EndUpdate;
end;



{$ENDREGION}


// Tree witdh expansion ======================

{$REGION Tree witdh expansion }


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
         fTreeWidthExpanded:= True;
      end
      else begin                                            // Change normal width (MaxWidth not modified)
         Folder.TreeWidth:= W;
         Folder.TreeMaxWidth:= Abs(Folder.TreeMaxWidth);    // Disable fixed state
      end;
   end;

   if KeyOptions.AltMargins then
      Folder.Editor.Refresh;

   OnAfterChangesOnTreeWidth;
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
      fTreeWidthExpanded:= True;
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

          fTreeWidthExpanded:= false;
          fTreeWidth_N:= 0;
          Result:= true;
      end;
   end;

end;


procedure TKntTreeUI.RxRTFEnter(Sender: TObject);
begin
   if not ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) then
      CheckRestoreTreeWidth;
end;


procedure TKntTreeUI.RTFMouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
begin
   if fTreeWidthExpanded then begin
      if ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) then exit;
      CheckRestoreTreeWidth;
   end;
   fTreeWidth_N:= 0;
end;


procedure TKntTreeUI.RTFMouseUp(Sender: TObject; Button: TMouseButton; Shift:TShiftState; X,Y: integer);
begin
   if fTreeWidthExpanded then
      CheckRestoreTreeWidth;
end;


procedure TKntTreeUI.TV_Click(Sender: TObject);
var
   Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);
  if AltDown then
     Folder.TreeMaxWidth:= -Folder.TreeMaxWidth        // Toggle fixed state
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
   App.TreeFocused(Self);
   if not ( (CtrlDown or AltDown) and ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) ) then
      CheckExpandTreeWidth;

  inherited;
end;


{$ENDREGION}



// Virtual Nodes ========================================================================

{$REGION Virtual nodes}

procedure TKntTreeUI.VirtualNoteUnlink;
var
  Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);
  Folder.VirtualNoteUnlink(TV.FocusedNode);
end;


procedure TKntTreeUI.VirtualNoteRefresh( const DoPrompt : boolean );
var
  Folder: TKntFolder;
begin
  Folder:= TKntFolder(Self.Folder);
  Folder.VirtualNoteRefresh(TV.FocusedNode, DoPrompt);
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



{$ENDREGION}

initialization

end.

