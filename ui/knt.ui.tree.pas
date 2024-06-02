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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Contnrs, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Clipbrd,
  Vcl.ActnList, Vcl.StdCtrls,

  TreeNT,

  RxRichEd,
  kn_Info,
  kn_Const,
  kn_KntNote
  ;


type
  PKntNote=^TKntNote;

  TTreeTransferAction = (ttCopy, ttPaste, ttClear);

  TKntTreeUI = class(TFrame)
  published
    TV: TTreeNT;

  private class var
    fTransferNodes : TKntNoteList;            // for data transfer (copy tree nodes between tabs)
    fMirrorNodes: TBucketList;
    fMovingTreeNode: TTreeNTNode;             // To use with Paste, after applying Cut on a Tree Node.
    fVirtualUnEncryptWarningDone : boolean;
    fOldNoteName: string;
    fCopyCutFromFolderID: integer;            // Copy or Cut from Folder

    fTreeWidthExpanded: boolean;
    fTreeWidth_N: Cardinal;
    fTreeWidthNodeTouched: boolean;
    fSplitterNoteMoving: boolean;

  public
     class property Virtual_UnEncrypt_Warning_Done: boolean read fVirtualUnEncryptWarningDone write fVirtualUnEncryptWarningDone;
     class constructor Create;
     class destructor Destroy;
     class procedure ClearMirrorNodes;
     class procedure ClearTransferNodes;
     class procedure ClearMovingTreeNode;
     class function IsAParentOf(aPerhapsParent, aChild: TTreeNTNode ): boolean;

  private
    fFolder: TObject;
    fSplitterNote : TSplitter;
    fPopupMenu : TPopupMenu;

    fReadOnly: boolean;
    fLastNodeSelected : TTreeNTNode;
    fIsAnyNodeMoving: boolean;
    fDraggedTreeNode : TTreeNTNode;

  protected
    // Create. Destroy
    procedure SetFolder(aFolder: TObject);
    procedure SetSplitterNote(aSplitter: TSplitter);
    procedure SetPopupMenu(value: TPopupMenu);
    procedure PopulateTV;
    procedure SetupTVHandlers;

    // TreeView Handlers
    procedure TVChange(Sender: TObject; Node: TTreeNTNode);
    procedure TVChecked(Sender: TObject; Node: TTreeNTNode);
    procedure TVChecking(Sender: TObject; Node: TTreeNTNode; var AllowCheck: Boolean);
    procedure TVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TVKeyPress(Sender: TObject; var Key: Char);
    procedure TVEditing(Sender: TObject; Node: TTreeNTNode; var AllowEdit: Boolean);
    procedure TVEditCanceled(Sender: TObject);
    procedure TVEdited(Sender: TObject; Node: TTreeNTNode; var S: string);
    procedure TVDeletion(Sender: TObject; Node: TTreeNTNode);
    procedure TVClick(Sender: TObject);
    procedure TVMouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
    procedure TVOnHint(Sender: TObject; Node: TTreeNTNode; var NewText: string);
    procedure TVStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TVDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TVEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TVSavingTree(Sender: TObject; Node: TTreeNTNode; var S: string);
    procedure DoEnter; override;

    function GetFocused: boolean;
    function GetIconKind : TNodeIconKind;
    function GetShowAllCheckboxes: boolean;
    function GetHideCheckedNodes: boolean;
    function GetSelectedNode: TTreeNTNode;
    procedure SetSelectedNode(value: TTreeNTNode);

    // Check State
    procedure ShowOrHideChildrenCheckBoxes(const ANode : TTreeNTNode);
    procedure ChangeCheckedState(Node: TTreeNTNode; Checked: Boolean; CalledFromMirrorNode: Boolean);

    // Tree witdh expansion
    procedure OnAfterChangesOnTreeWidth;
    procedure SplitterNoteMoved(Sender: TObject);
    procedure CheckingTreeExpansion;
    procedure CheckExpandTreeWidth;
    procedure RxRTFEnter(Sender: TObject);
    procedure RTFMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RTFMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateTreeOptions;

    procedure UpdateTreeChrome;

    property Folder: TObject read fFolder write SetFolder;
    property SplitterNote : TSplitter read fSplitterNote write SetSplitterNote;
    property PopupMenu : TPopupMenu read fPopupMenu write SetPopupMenu;

    property ReadOnly: boolean read fReadOnly write fReadOnly;
    function CheckReadOnly: boolean;
    property Focused: boolean read GetFocused;
    function IsEditing: boolean;
    function GetFirstNode: TTreeNTNode;
    property SelectedNode: TTreeNTNode read GetSelectedNode write SetSelectedNode;

    property ShowAllCheckboxes: boolean read GetShowAllCheckboxes;
    property HideCheckedNodes: boolean read GetHideCheckedNodes;
    property IconKind : TNodeIconKind read GetIconKind;

    procedure ShowOrHideIcons(const UpdateNodes : boolean);
    procedure SelectIconForNode(const myTreeNode: TTreeNTNode);
    procedure SetNodeColor(TreeNode: TTreeNTNode; const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean);
    procedure SetNodeBold(TreeNode: TTreeNTNode; const DoChildren : boolean);
    procedure SetNodeCustomImage(TreeNode: TTreeNTNode);
    function GetNodeFontFace (TreeNode: TTreeNTNode): string;
    procedure SetNodeFontFace(TreeNode: TTreeNTNode; const ResetDefault, DoChildren: boolean);
    procedure SetNodeFontSize(TreeNode: TTreeNTNode; const ResetDefault, DoChildren : boolean);

    function GetNodePath(aNode: TTreeNTNode; const aDelimiter: string; const TopToBottom: boolean) : string;
    procedure CopyNodePath(myTreeNode : TTreeNTNode; const InsertInEditor : boolean);
    procedure PasteNodeName(myTreeNode : TTreeNTNode; const PasteMode : TPasteNodeNameMode);
    procedure CopyNodeName(const IncludeNoteText: boolean);
    procedure RenameFocusedNode;

    procedure OutlineNumberNodes;
    procedure SortSubtree(myTreeNode : TTreeNTNode);
    procedure SortTree;

    procedure GetOrSetNodeExpandState(const AsSet, TopLevelOnly: boolean);
    procedure FullCollapse;
    procedure FullExpand;
    procedure Navigate(NavDirection: TNavDirection);

    // Create new nodes
    function AddNode(aInsMode: TNodeInsertMode) : TTreeNTNode;
    function NewNode(aInsMode: TNodeInsertMode; const aOriginNode: TTreeNTNode; const aNewNodeName: string;
                      const aDefaultNode: boolean) : TTreeNTNode;
    procedure CreateMasterNode;
    procedure CreateNodefromSelection;
    procedure SetupNewTreeNode(const aTreeNode : TTreeNTNode);


    // Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees
    procedure MoveTreeNode(MovingNode : TTreeNTNode; const aDir : TDirection);
    procedure DeleteNode(myTreeNode: TTreeNTNode; const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true);
    procedure PasteSubtree;
    function MoveSubtree(TargetNode: TTreeNTNode): boolean;
    function TreeTransferProc(const XferAction: TTreeTransferAction;
                              const Prompt: boolean; const PasteAsVirtualKNTNode: boolean; const MovingSubtree: boolean) : boolean;

    // Check State
    procedure ShowOrHideCheckBoxes;
    procedure ToggleChildrenCheckbox(myTreeNode : TTreeNTNode);
    procedure ToggleCheckNode(myTreeNode : TTreeNTNode);
    procedure HideChildNodesUponCheckState (ParentNode: TTreeNTNode; CheckState: TCheckState);
    procedure ShowCheckedNodes (ParentNode: TTreeNTNode);

    // Filter nodes
    procedure MarkAllFiltered;
    procedure MarkAllUnfiltered;
    procedure HideFilteredNodes;
    procedure RemoveFilter;

    // Tree witdh expansion
    function CheckRestoreTreeWidth: boolean;

    // Mirror nodes
    procedure InsertMirrorNode(myTreeNode: TTreeNTNode);
    procedure SetupMirrorNodes;
    procedure ManageMirrorNodesOnDeleteTree;
    class function GetMirrorNodes(originalNode: TTreeNTNode): Pointer;
    class procedure AddMirrorNode(MainNode: TTreeNTNode; Mirror_Node: TTreeNTNode);
    class procedure ReplaceNonVirtualNote(MainNode: TTreeNTNode; newNode: TTreeNTNode);
    class procedure RemoveMirrorNode(MainNode: TTreeNTNode; mirror_Node: TTreeNTNode);
    class procedure ManageMirrorNodes(Action: TMirrorAction; node: TTreeNTNode; targetNode: TTreeNTNode);

    // Virtual nodes
    class function GetCurrentVirtualNote: TKntNote;
    procedure VirtualNoteProc( VMode : TVirtualMode; myTreeNode : TTreeNTNode; VirtFN : string );
    procedure VirtualNoteRefresh( const DoPrompt : boolean );
    procedure VirtualNoteUnlink;
    {$IFDEF WITH_IE}
    function VirtualNoteGetMode( const aNote : TKntNote; var newMode : TVirtualMode; var newFN : string ) : boolean;
    {$ENDIF}
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
  STR_03 = 'Auto-detect and strip numbering from all nodes?';
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
  STR_15 = 'No tree node available for copying or pasting data.';
  STR_16 = 'OK to move %d nodes from node "%s" to current node "%s"?';
  STR_17 = ' No node is selected';
  STR_18 = 'OK to forget %d copied nodes?';
  STR_19 = 'No nodes were copied.';
  STR_20 = ' %d nodes copied for transfer';
  STR_21 = 'No data to paste. Select "Transfer|Copy Subtree" first.';
  STR_22 = 'One or more nodes being transferred is a Virtual Node. Each such node will be pasted as normal (non-virtual) node, if another virtual node in this file is already linked to the same file.' + #13#13 + 'Continue?';
  STR_23 = 'OK to paste %d nodes below current node "%s"?';
  STR_24 = ' Pasted %d nodes';
  STR_25 = '%d virtual nodes have been converted to normal nodes, because other virtual nodes in current file already link to the same files.';
  STR_26 = 'OK to paste %d nodes as mirror nodes below current node "%s"?' + #13#10 + '(Only not hidden nodes will be pasted)';
  STR_27 = 'Node not found (Folder ID/Node ID): %d/%d';

  STR_36 = 'Drag: ';
  STR_38_Dragged = '<nothing>';
  STR_39 = 'Cannot drop node %s on itself';
  STR_41 = 'Cannot drop node %s - invalid source';
  STR_42 = 'Cannot drop node %s onto its child %s';
  STR_43 = 'Node "%s" promoted to TOP';
  STR_44 = 'Node "%s" promoted to parent''s level';
  STR_45 = 'Node "%s" moved to top of siblings';
  STR_46 = 'Node "%s" inserted after node "%s"';
  STR_47 = 'Node "%s" made child of node "%s"';
  STR_48 = 'Nothing to drop or invalid drop target';
  STR_49 = 'OK to sort the entire tree?';
  STR_50 = ' Node name cannot be blank!';
  STR_51= ' Node renamed.';
  STR_52= ' Cannot perform operation: Tree is read-only';
  STR_53 = 'Edit node name';
  STR_54 = 'Enter new name:';

  STR_v01 = 'Virtual node "%s" is currently linked to file "%s". Do you want to link the node to a different file?';
  STR_v02 = 'Node "%s" contains text. Do you want to flush this text to a file and make the node virtual?';
  STR_v03 = 'This KeyNote file is encrypted, but ' +
            'disk files linked to virtual nodes ' +
            'will NOT be encrypted.' + #13#13 + 'Continue?';
  STR_v04 = 'Select file for virtual node';
  STR_v05 = 'Only RTF, Text and HTML files can be linked to virtual nodes.';
  STR_v06 = 'Cannot link virtual node to a file on removable drive %s:\ ';
  STR_v07 = 'You are creating a virtual node linked to file on removable drive %s\. The file may not be available at a later time. Continue anyway?';
  STR_v08 = 'Selected file is already linked to a virtual node.';
  STR_v09 = 'Virtual node error: ';
  STR_v10 = 'Node "%s" represents an Internet Explorer node and cannot be unlinked. Nodes of this type can only be deleted.';
  STR_v11 = 'Unlink virtual node "%s"? The contents of the node will be retained, but the link with the file on disk (%s) will be removed.';
  STR_v12 = 'Virtual node %s HAS BEEN modified within KeyNote. ' +
            'If the node is refreshed, the changes will be lost. ' +
            'OK to reload the node from file %s?';
  STR_v13 = 'Virtual node %s has NOT been modified within KeyNote. ' +
            'OK to reload the node from file %s?';
  STR_v14 = 'Error refreshing virtual node: ';
  STR_v15 = ' Virtual node refreshed.';
  STR_v16 = ' Error refreshing node';
  STR_v17 = 'Selected node "%s" is not a virtual node.';
  STR_v18 = 'Unlink mirror node "%s"? The contents of the node will be retained but the link with the non virtual node will be removed.';

const
  TREE_DEFAULT_WIDTH = 100;
  TREE_WIDTH_MOUSE_TIMEOUT = 2500;


// Create  / Destroy =========================================

{$REGION Create / Destroy}

class constructor TKntTreeUI.Create;
begin
  fTransferNodes := nil;
  fMovingTreeNode:= nil;
  fVirtualUnEncryptWarningDone := false;
  fMirrorNodes:= TBucketList.Create();

  fTreeWidthExpanded:= false;
  fTreeWidth_N:= 0;
  fTreeWidthNodeTouched:= false;
  fSplitterNoteMoving:= false;
end;


class destructor TKntTreeUI.Destroy;
begin
  ClearMirrorNodes;
  ClearTransferNodes;
end;


class procedure TKntTreeUI.ClearMirrorNodes;
begin
  fMirrorNodes.Clear;
end;

class procedure TKntTreeUI.ClearTransferNodes;
begin
  if (fTransferNodes <> nil) then begin
    try
      fTransferNodes.Free;
    except
    end;
    fTransferNodes := nil;
  end;
end;


class procedure TKntTreeUI.ClearMovingTreeNode;
begin
  fMovingTreeNode:= nil;
end;


constructor TKntTreeUI.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   fIsAnyNodeMoving:= false;
   fDraggedTreeNode := nil;
   fLastNodeSelected := nil;
   fOldNoteName := DEFAULT_NEW_NOTE_NAME;


   with TV do begin
     // static options that do not change:
     SortType := TreeNT.TSortType(stNone); // MUST be stNone; sort by manually calling AlphaSort
     Options := [
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

     if TreeOptions.FullRowSelect then
       Options := Options + [toFullRowSelect];

     ShowHint := false;
     HelpContext:= 284;  // Tree-type Notes [284]
   end;

end;


destructor TKntTreeUI.Destroy;
begin
   with TV do begin
      OnChange := nil;
      OnDeletion := nil;
   end;
   ManageMirrorNodesOnDeleteTree;
   FreeAndNil(TV);

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
  // updates options for current folder's tree based on global tree options

  with TV do begin
      ColorDropSelected := clInfoBK;
      ColorUnfocusedSelected := cl3DLight;
      DragMode := dmAutomatic;

      Options := Options + [toAutoExpand];

      if TreeOptions.AutoScroll then
        Options := Options + [toAutoScroll]
      else
        Options := Options - [toAutoScroll];

      if TreeOptions.HotTrack then
        Options := Options + [toHotTrack]
      else
        Options := Options - [toHotTrack];

      if TreeOptions.EditInPlace then
        Options := Options - [toReadOnly]
      else
        Options := Options + [toReadOnly];

      if TreeOptions.ShowTooltips then begin
        Options := Options + [toInfoTip];
        Options := Options + [toToolTips];
      end
      else begin
        Options := Options - [toInfoTip];
        Options := Options - [toToolTips];
      end;
  end;

end; // UpdateTreeOptions


procedure TKntTreeUI.PopulateTV;
var
  myFolder: TKntFolder;
  TVFontStyleWithBold : TFontStyles;
  i, loop : integer;
  tNode, myTreeNode, LastTreeNodeAssigned : TTreeNTNode;
  LastNodeLevel: integer;
  j, numChilds, AuxLevel, ChildLevel : integer;
  myNote: TKntNote;

begin
   myFolder:= TKntFolder(Self.Folder);

   with TV do begin

      myFolder.UpdateTree; // do this BEFORE creating nodes

      // Create TreeNodes for all notes in the folder
      LastTreeNodeAssigned := nil;
      LastNodeLevel := 0;

      if ( myFolder.Notes.Count > 0 ) then begin
        TVFontStyleWithBold:= Font.Style + [fsBold];

        Items.BeginUpdate;
        try
           if ShowAllCheckboxes then
              Items.TopLevelCheckType := ctCheckBox
           else
              Items.TopLevelCheckType := ctNone;

           for i := 0 to myFolder.Notes.Count-1 do begin
              myNote := myFolder.Notes[i];

              numChilds:= 0;
              ChildLevel:= myNote.Level+1;
              for j := i+1 to myFolder.Notes.Count-1 do begin
                 AuxLevel:= myFolder.Notes[j].Level;
                 if AuxLevel = ChildLevel then
                    inc(numChilds);
                 if AuxLevel < ChildLevel then
                    break;
              end;

              case myNote.Level of
                0 : begin
                  myTreeNode := Items.Add( nil, myNote.Name, numChilds );
                  LastNodeLevel := 0;
                end
                else begin
                  case DoTrinaryCompare( myNote.Level, LastNodeLevel ) of
                    trinGreater:
                      myTreeNode := Items.AddChild( LastTreeNodeAssigned, myNote.Name );
                    trinEqual:
                      myTreeNode := Items.AddChild( LastTreeNodeAssigned.Parent, myNote.Name );
                    trinSmaller: begin  // myNote.Level is SMALLER than LastNodeLevel, i.e. we're moving LEFT in the tree
                       for loop := 1 to ( LastNodeLevel - myNote.Level ) do begin
                          if assigned( LastTreeNodeAssigned ) then begin
                            if ( LastTreeNodeAssigned.Level <= myNote.Level ) then
                                break;
                            LastTreeNodeAssigned := LastTreeNodeAssigned.Parent;
                          end
                          else
                            break;
                       end;
                       myTreeNode := Items.Add( LastTreeNodeAssigned, myNote.Name, numChilds );
                    end;
                  end;
                end;
              end;

              LastTreeNodeAssigned := myTreeNode;
              LastNodeLevel := myNote.Level;

              myTreeNode.Data := myNote;
              SetupNewTreeNode(myTreeNode);
           end;

        finally
          Log_StoreTick( 'After created TreeNodes', 3 );

          ShowOrHideIcons(true);

          Log_StoreTick( 'After ShowOrHIdeIcons', 3 );

          if myFolder.Filtered then             // [dpv]
             HideFilteredNodes;
          if myFolder.HideCheckedNodes then     // [dpv]
             HideChildNodesUponCheckState (nil, csChecked);

          Items.EndUpdate;

          Log_StoreTick( 'After HideFilteredNodes, HideCheckNodes', 3 );
        end;

        // restore selected node: this block must be
        // OUTSIDE the beginupdate..endupdate range

        //if ( Items.Count > 0 ) then       // [dpv]
        if ( Items.CountNotHidden > 0 ) then begin
          if (( TreeOptions.ExpandMode <> txmFullCollapse ) and // SaveActiveNode and
             ( myFolder.OldSelectedIndex >= 0 ) and
             ( myFolder.OldSelectedIndex < Items.Count )) then
          begin
            // restore the node which was selected when file was saved
            tNode:= Items[myFolder.OldSelectedIndex];
            if tNode.Hidden  then begin  // [dpv]
               tNode := Items.GetFirstNode;
               if tNode.Hidden then tNode:= tNode.GetNextNotHidden;
            end;
          end
          else begin
            tNode := Items.GetFirstNode;
            if tNode.Hidden then tNode:= tNode.GetNextNotHidden;
          end;
          Selected:= tNode;
        end;

        Log_StoreTick( 'After Restored selected node', 3 );


        case TreeOptions.ExpandMode of
          txmFullCollapse : begin
            // nothing
          end;
          txmActiveNode : begin
            if assigned( Selected ) then
              Selected.Expand( false );
          end;
          txmTopLevelOnly, txmExact : begin
            try
               GetOrSetNodeExpandState(true, (TreeOptions.ExpandMode = txmTopLevelOnly));
            except
              // nothing
            end;
          end;
          txmFullExpand : begin
            FullExpand;
          end;
        end;

        Form_Main.UpdateTreeVisible( myFolder ); // [f]

        if assigned(Selected) then begin
          Selected.MakeVisible;
          fLastNodeSelected:= Selected;
        end;

        Log_StoreTick( 'After UpdateTreeVisible', 3 );
      end;
   end;
end;


procedure TKntTreeUI.SetupTVHandlers;
begin
   with TV do begin
     OnKeyDown := TVKeyDown;
     OnKeyPress := TVKeyPress;
     OnChange := TVChange;
     // OnChanging := TVChanging; // unused
     OnChecked := TVChecked;
     OnEditing := TVEditing;
     OnEdited := TVEdited;
     OnEditCanceled := TVEditCanceled;
     //OnDeletion := TVDeletion;
     // OnExit := TVExit;
     OnClick := TVClick;
     //OnDblClick := TVDblClick;
     //OnMouseDown := TVMouseDown;
     OnDragDrop := TVDragDrop;
     OnDragOver := TVDragOver;
     OnEndDrag := TVEndDrag;
     OnStartDrag := TVStartDrag;
     OnHint := TVOnHint;
     //OnEnter:= TVEnter;
     OnSavingTree:= TVSavingTree;
     OnMouseMove:= TVMouseMove;
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
  myTreeNode, selectedTreeNode: TTreeNTNode;
  myNote: TKntNote;
  FOrig: TFont;
  FDest: TFontInfo;
  FontChange: boolean;
  BGColorChange: boolean;
  Styles: TFontStyles;
  Folder: TKntFolder;
begin
   { See comments in the methods TCustomTreeNT.RecreateTreeWindow and TCustomTreeNT.DestroyWnd  (TreeNT.pas)

    The indicated explains why font changes in the tree (from 'Folder Properties...') were not reflected on it
    on many occasions, precisely those in which we had not modified the background color.
    And besides, it was also happening that the font changes, when they were shown, did not give rise to a correct
    resizing of the height of each item.
     * These problems were of tree refresh. Saving and reopening the file would show up correctly.
   }

   Folder:= TKntFolder(Self.Folder);

   FOrig:= TV.Font;
   FDest:= Folder.TreeChrome.Font;

   FontChange:=  (FOrig.Color <> FDest.Color) or (FOrig.Size <> FDest.Size) or
                 (FOrig.Name <> FDest.Name) or (FOrig.Style <> FDest.Style) or
                 (FOrig.Charset <> FDest.Charset);

   BGColorChange:= (TV.Color <> Folder.TreeChrome.BGColor);

   if not FontChange and not BGColorChange then
      Exit;


   if FontChange then begin
      Log_StoreTick('');
      Log_StoreTick('UpdateTreeChrome - Begin changes', 2, +1);

      FontInfoToFont(Folder.TreeChrome.Font, TV.Font);   // This doesn't force any update (it doesn't end up calling RecreateWnd)

      { The above line does not affect nodes with any changes from the default values, we must explicitly update their size, style and font color.
        The background color of the node and the its font.name do not need to be updated.
        Also take into account: if the font of the tree includes the bold style, and we remove it from a node, it will not indicate Bold=True,
        but for the tree we have explicitly modified it, so we need to also look at the property ParentFont of TTreeNTNode
      }

      Log_StoreTick('After FontInfoToFont', 2);

       myTreeNode := TV.Items.GetFirstNode;
       while assigned(myTreeNode) do begin
         myNote:= TKntNote(myTreeNode.Data);
         if assigned(myNote) then begin
           if not myTreeNode.ParentFont
              or myNote.Bold  or myNote.HasNodeFontFace or myNote.HasNodeColor or myNote.HasNodeBGColor then begin

               if not myNote.HasNodeFontFace then
                  myTreeNode.Font.Name := FDest.Name;
               //if myNote.HasNodeBGColor then myTreeNode.Color := myNote.NodeBGColor;       // Not necessary

               myTreeNode.Font.Size := FDest.Size;

               Styles:= FDest.Style;
               if myNote.Bold then
                  Styles := Styles + [fsBold];
               myTreeNode.Font.Style := Styles;

               if myNote.HasNodeColor then
                 myTreeNode.Font.Color := myNote.NodeColor
               else
                 myTreeNode.Font.Color := FDest.Color;
           end;
         end;

         myTreeNode := myTreeNode.GetNext;
       end;
   end;

   Log_StoreTick('After modified individual nodes', 2);

   selectedTreeNode:= TV.Selected;
   TV.OnChange := nil;
   //TV.Items.BeginUpdate;
   try
      TV.Color := Folder.TreeChrome.BGColor;   // It will do nothing if BGColorChange = False
      Log_StoreTick('After changed TV.Color', 2);
      if not BGColorChange and FontChange then begin
         TV.RecreateTreeWindow;
         Log_StoreTick('After RecreateTreeWindow', 2);
      end;

   finally
      //TV.Items.EndUpdate;
      TV.Selected:= selectedTreeNode;
      TV.OnChange := TVChange;
   end;

   Log_StoreTick('UpdateTreeChrome - End changes', 2, -1);
   Log_Flush();

end; // UpdateTreeChrome



function TKntTreeUI.CheckReadOnly: boolean;
begin
    Result:= False;

    if ReadOnly then begin
       App.ShowInfoInStatusBar(STR_52);
       Result:= True;
       exit;
    end;
end;


function TKntTreeUI.GetFocused: boolean;
begin
   Result:= TV.Focused;
end;


function TKntTreeUI.GetFirstNode: TTreeNTNode;
begin
   Result:= TV.Items.GetFirstNode;
end;


function TKntTreeUI.GetSelectedNode: TTreeNTNode;
begin
   Result:= TV.Selected;
end;


procedure TKntTreeUI.SetSelectedNode(value: TTreeNTNode);
begin
   TV.Selected:= value;
end;


function TKntTreeUI.GetIconKind : TNodeIconKind;
begin
   Result:= TKntFolder(Folder).IconKind;
end;


function TKntTreeUI.GetShowAllCheckboxes: boolean;
begin
   Result:= TKntFolder(Folder).Checkboxes;
end;


function TKntTreeUI.GetHideCheckedNodes: boolean;
begin
   Result:= TKntFolder(Folder).HideCheckedNodes;
end;


function TKntTreeUI.IsEditing: boolean;
begin
   Result:= TV.IsEditing;
end;



class function TKntTreeUI.IsAParentOf( aPerhapsParent, aChild : TTreeNTNode ) : boolean;
var
  i, leveldifference : integer;
begin
  result := false;
  if ( not ( assigned( aPerhapsParent ) and assigned( aChild ))) then exit;

  leveldifference := aChild.Level - aPerhapsParent.Level;
  if ( leveldifference < 1 ) then exit; // cannot be a parent if its level is same or greater than "child's"

  for i := 1 to leveldifference do begin
    aChild := aChild.Parent;
    if assigned( aChild ) then begin
       if ( aChild = aPerhapsParent ) then begin
          result := true;
          break;
       end;
    end
    else
       break; // result = false
  end;
end; // IsAParentOf



// TreeView Handlers ================================

{$REGION TreeView Handlers}

procedure TKntTreeUI.TVChange(Sender: TObject; Node: TTreeNTNode);
begin
  if (not assigned(Node)) then exit;

  if (Node <> fLastNodeSelected) then begin
     TKntFolder(Folder).NodeSelected(Node, fLastNodeSelected);
     fLastNodeSelected:= Node;
  end;
end;


procedure TKntTreeUI.TVChecking(Sender: TObject; Node: TTreeNTNode; var AllowCheck: Boolean);
begin
  AllowCheck:= not ReadOnly;
end;


procedure TKntTreeUI.TVChecked(Sender: TObject; Node: TTreeNTNode);
begin
  if assigned(node) and assigned(node.Data) then
     ChangeCheckedState(Node, Node.CheckState = csChecked, false);
end;


procedure TKntTreeUI.TVKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

    VK_SPACE : if ( Shift = [] ) then begin
      key := 0;
      RenameFocusedNode;
    end;

    220 : if ( Shift = [ssCtrl] ) then begin // backslash
      Key := 0;
      TKntFolder(Folder).SetFocusOnEditor;
    end;

  end;

end; // TVKeyDown


procedure TKntTreeUI.TVKeyPress(Sender: TObject; var Key: Char);
begin
  if TV.IsEditing then begin
     if ( key = KNTLINK_SEPARATOR ) then
        key := #0; // disallow | character in node name, because it breaks KNT links (which use | as delimiter)
  end;
end; // TVKeyPress


procedure TKntTreeUI.TVEditing(Sender: TObject; Node: TTreeNTNode; var AllowEdit: Boolean);
begin
  if not CheckReadOnly then begin
    fOldNoteName := node.text;
    TV.PopupMenu := nil;       // stop menu events triggered by shortcut keys
  end
  else
    AllowEdit := false;
end;


procedure TKntTreeUI.TVEditCanceled(Sender: TObject);
begin
  TV.PopupMenu := PopupMenu;
end;


procedure TKntTreeUI.TVEdited(Sender: TObject; Node: TTreeNTNode; var S: string);
begin
  TV.PopupMenu := PopupMenu;               // PopupMenu=nil => IsEditing=True
  S := trim( copy( S, 1, TREENODE_NAME_LENGTH ));

  if ( S = '' ) then begin
    App.ShowInfoInStatusBar(STR_50);
    S := fOldNoteName;
    exit;
  end;

  _ALLOW_VCL_UPDATES := false;
  try
    if assigned( TKntNote( Node.Data )) then
      TKntNote( Node.Data ).Name := S;  // {N} must add outline numbering, if any
    App.ShowInfoInStatusBar(STR_51);
    TKntFolder(Folder).Modified := true;
  finally
    _ALLOW_VCL_UPDATES := true;
  end;
end; // TVEdited


procedure TKntTreeUI.TVDeletion(Sender: TObject; Node: TTreeNTNode);
var
   myFolder : TKntFolder;
   HasMirrorNodes: boolean;
   Note: TKntNote;
begin
  if not assigned(Node) then exit;
  Note:= TKntNote(Node.Data);
  HasMirrorNodes:= Note.HasMirrorNodes;

  myFolder:= TKntFolder(Folder);
  TKntTreeUI.ManageMirrorNodes(maDeleting, Node, nil);
  myFolder.RemoveNote(Note, HasMirrorNodes);
end;


procedure TKntTreeUI.TVClick(Sender: TObject);
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


procedure TKntTreeUI.TVMouseMove(Sender: TObject; Shift:TShiftState; X,Y: integer);
begin
   if not CtrlDown then
      CheckingTreeExpansion
   else
      fTreeWidth_N:= Cardinal.MaxValue - TREE_WIDTH_MOUSE_TIMEOUT;
end;


procedure TKntTreeUI.TVOnHint(Sender: TObject; Node: TTreeNTNode; var NewText: string);
begin
  { *1 We make sure that CheckingTreeExpansion doesn't end up calling CheckExpandTreeWidth
       until we initialize TreeWidth_N to 0 (from RTFMouseMove and CheckRestoreTreeWidth) }

   if not CtrlDown then begin
      fTreeWidthNodeTouched:= True;
      CheckingTreeExpansion;
   end
   else
      fTreeWidth_N:= Cardinal.MaxValue - TREE_WIDTH_MOUSE_TIMEOUT;   // *1

end;


procedure TKntTreeUI.TVStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  fDraggedTreeNode := TV.Selected;
  if assigned( fDraggedTreeNode ) then
     App.ShowInfoInStatusBar(STR_36 + fDraggedTreeNode.Text);
end;


procedure TKntTreeUI.TVDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ThisNode : TTreeNTNode;
begin
  if (( source is TRxRichEdit )) then
     accept := true;

  accept := false;
  if (( not assigned( fDraggedTreeNode )) or ( sender <> source )) then
     exit;

  ThisNode := ( sender as TTreeNT ).GetNodeAt( X, Y );
  if ( not ( assigned( ThisNode ) and ( ThisNode <> fDraggedTreeNode ))) then
     exit;

  Accept := true;
end;


procedure TKntTreeUI.TVDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropTreeNode, PreviousParent : TTreeNTNode;
  s : string;
begin

  if CheckReadOnly then begin
    fDraggedTreeNode := nil;
    if (Sender is TTreeNT) then
       (sender as TTreeNT).EndDrag( false );
    exit;
  end;

  s := STR_38_Dragged;
  DropTreeNode := ( sender as TTreeNT ).GetNodeAt( X, Y );

  try
    if assigned( fDraggedTreeNode ) and assigned( DropTreeNode ) then begin

      // check if we can drop DraggedTreeNode onto DropNode
      // 1. Cannot drop node on itself
      if ( DropTreeNode = fDraggedTreeNode ) then begin
        s := Format( STR_39, [fDraggedTreeNode.Text] );
        exit;
      end;

      // 2. Cannot drop between treeviews
      if ( DropTreeNode.TreeView <> fDraggedTreeNode.TreeView ) then begin
        s := Format( STR_41, [fDraggedTreeNode.Text] );
        exit;
      end;

      // 3. Cannot drop a node onto its own child
      if IsAParentOf( fDraggedTreeNode, DropTreeNode ) then begin
        s := Format( STR_42, [fDraggedTreeNode.Text, DropTreeNode.Text] );
        exit;
      end;

      // now figure out where to move the node
      // 1. If dropping on immediate parent, then make node first child of parent
      // 2. If dropping on any other node, then make node LAST child of that node,
      //    unless SHIFT is down, in which case make node FIRST child of that node

      TV.OnChange := nil; // stop event
      PreviousParent := fDraggedTreeNode.Parent;

      TV.OnChecked := nil;

      if (( DropTreeNode.Level = 0 ) and CtrlDown ) then begin
        // make TOP node
        fDraggedTreeNode.MoveTo( nil, naAddFirst );
        s := Format( STR_43, [fDraggedTreeNode.Text] );
      end
      else
      if ( fDraggedTreeNode.Parent = DropTreeNode ) then begin
         if ShiftDown then begin
           fDraggedTreeNode.MoveTo( DropTreeNode, naInsert );
           s := Format( STR_44, [fDraggedTreeNode.Text] );
         end
         else begin
           fDraggedTreeNode.MoveTo( DropTreeNode, naAddChildFirst );
           s := Format( STR_45, [fDraggedTreeNode.Text] );
         end;
      end
      else begin
         if ShiftDown then begin
           fDraggedTreeNode.MoveTo( DropTreeNode, naInsert );
           s := Format( STR_46, [fDraggedTreeNode.Text, DropTreeNode.Text] );
         end
         else begin
           fDraggedTreeNode.MoveTo( DropTreeNode, naAddChildFirst );
           s := Format( STR_47, [fDraggedTreeNode.Text, DropTreeNode.Text] );
         end;
      end;

      // update node icon
      SelectIconForNode( fDraggedTreeNode);
      SelectIconForNode( fDraggedTreeNode.Parent);
      SelectIconForNode( PreviousParent);
      TV.Invalidate;

    end
    else begin
      s := STR_48;
      fDraggedTreeNode := nil;
      if (Sender is TTreeNT) then
         (sender as TTreeNT).EndDrag( false );
    end;

  finally
    KntFile.Modified := true;
    if TKntNote(fDraggedTreeNode.Data).Checked then begin
       fDraggedTreeNode.CheckState := csChecked;
    end;
    TV.OnChange := TVChange;
    TV.OnChecked:= TVChecked;
    App.ShowInfoInStatusBar(s);
  end;

end; // TVDragDrop


procedure TKntTreeUI.TVEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  fDraggedTreeNode := nil;
end;


procedure TKntTreeUI.TVSavingTree(Sender: TObject; Node: TTreeNTNode; var S: string);
begin
   if not ShowHiddenMarkers then exit;
   if assigned( TKntNote( Node.Data )) then
      S:= Format('%s [%d]', [S, TKntNote(Node.Data).ID]);
end;


procedure TKntTreeUI.DoEnter;
begin
   App.TreeFocused(Self);
   if not ( (CtrlDown or AltDown) and ((GetKeyState(VK_LBUTTON) < 0) or (GetKeyState(VK_RBUTTON) < 0)) ) then
      CheckExpandTreeWidth;

  inherited;
end;

{$ENDREGION}





procedure TKntTreeUI.ShowOrHideIcons(const UpdateNodes : boolean);
var
  myTreeNode : TTreeNTNode;
begin
  TV.Items.BeginUpdate;
  try
    case IconKind of
       niNone :     TV.Images := nil;
       niStandard : TV.Images := Form_Main.IMG_TV;
       niCustom :   TV.Images := Chest.IMG_Categories;
    end;

    if UpdateNodes then begin
       myTreeNode := TV.Items.GetFirstNode;
       while assigned(myTreeNode) do begin
          SelectIconForNode(myTreeNode);
          myTreeNode := myTreeNode.GetNext;
       end;
    end;

  finally
    TV.Items.EndUpdate;
  end;
end; // ShowOrHideIcons


procedure TKntTreeUI.SelectIconForNode(const myTreeNode: TTreeNTNode);
var
   KntNote: TKntNote;
begin
  if not assigned(myTreeNode) then exit;

  KntNote:= myTreeNode.Data;

  case IconKind of

    niStandard : begin
      case KntNote.VirtualMode of
        vmNone : begin
          if myTreeNode.HasChildren then begin
            if (myTreeNode.Level > 0) then
              myTreeNode.ImageIndex := ICON_BOOK
            else
              myTreeNode.ImageIndex := ICON_FOLDER;
          end
          else
            myTreeNode.ImageIndex := ICON_NOTE;
        end;

        vmText, vmRTF, vmHTML: myTreeNode.ImageIndex := ICON_VIRTUAL;
        vmIELocal :            myTreeNode.ImageIndex := ICON_VIRTUALIELOCAL;
        vmIERemote :           myTreeNode.ImageIndex := ICON_VIRTUALIEREMOTE;
        vmKNTNode :            myTreeNode.ImageIndex := ICON_VIRTUAL_KNT_NODE;
      end;
      myTreeNode.SelectedIndex := succ(myTreeNode.ImageIndex);
    end;

    niCustom : begin
      myTreeNode.ImageIndex := TKntNote(myTreeNode.Data).ImageIndex;
      myTreeNode.SelectedIndex := myTreeNode.ImageIndex;
    end;

    niNone : begin
      myTreeNode.ImageIndex := -1;
      myTreeNode.SelectedIndex := -1;
    end;
  end;

end; // SelectIconForNode


procedure TKntTreeUI.SetNodeColor(TreeNode: TTreeNTNode; const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean);
var
  myColor : TColor;
  myHasThisColor : boolean;
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  Folder: TKntFolder;

    procedure ColorChildren(StartNode : TTreeNTNode);
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while (assigned(myChildNode) and assigned(myChildNode.Data)) do begin
        if AsTextColor then begin
          myChildNode.Font.Color := myColor;
          with TKntNote(myChildNode.Data) do begin
            NodeColor := myColor;
            HasNodeColor := myHasThisColor;
          end;
        end
        else begin
          myChildNode.Color := myColor;
          with TKntNote(myChildNode.Data) do begin
            NodeBGColor := myColor;
            HasNodeBGColor := myHasThisColor;
          end;
        end;

        if myChildNode.HasChildren then
          ColorChildren(myChildNode); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild(myChildNode);
      end;
    end;

begin
  myTreeNode:= TreeNode;
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

  Folder:= TKntFolder(Self.Folder);

  myNote := TKntNote(myTreeNode.Data);
  try
    case AsTextColor of
      true : begin
        if UseColorDlg then begin
          if myNote.HasNodeColor then
             myColor := myNote.NodeColor
          else
             myColor := Folder.TreeChrome.Font.Color;

          Form_Main.ColorDlg.Color := myColor;
          if Form_Main.ColorDlg.Execute then
             myColor := Form_Main.ColorDlg.Color
          else
             exit;
        end
        else begin
          if ResetDefault then
             myColor := Folder.TreeChrome.Font.Color
          else
             myColor := Form_Main.TB_Color.ActiveColor;
        end;

        myHasThisColor := (myColor <> Folder.TreeChrome.Font.Color);
        myNote.HasNodeColor := myHasThisColor;

        myNote.NodeColor := myColor;
        myTreeNode.Font.Color := myColor;

      end;

      false: begin
        if UseColorDlg then begin
          if myNote.HasNodeBGColor then
            myColor := myNote.NodeBGColor
          else
            myColor := TV.Color;
          with Form_Main do begin
            ColorDlg.Color := myColor;
            if ColorDlg.Execute then
              myColor := ColorDlg.Color
            else
              exit;
          end;
        end
        else begin
          if ResetDefault then
            myColor := TV.Color
          else
            myColor := Form_Main.TB_Hilite.ActiveColor;
        end;

        myHasThisColor := (myColor <> TV.Color);
        myNote.HasNodeBGColor := myHasThisColor;

        myNote.NodeBGColor := myColor;
        myTreeNode.Color := myColor;
      end;
    end;

    if (DoChildren and myTreeNode.HasChildren) then
       ColorChildren(myTreeNode);

  finally
    Folder.Modified := true;
  end;
end; // SetTreeNodeColor


procedure TKntTreeUI.SetNodeBold(TreeNode: TTreeNTNode; const DoChildren : boolean);
var
  myNote : TKntNote;
  myTreeNode : TTreeNTNode;
  newStyle : TFontStyles;

    procedure BoldChildren(StartNode : TTreeNTNode);
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while (assigned(myChildNode) and assigned(myChildNode.Data)) do begin
        myChildNode.Font.Style := newStyle;
        TKntNote(myChildNode.Data).Bold := myNote.Bold;
        if myChildNode.HasChildren then
          BoldChildren(myChildNode); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild(myChildNode);
      end;
    end;

begin
  myTreeNode:= TreeNode;
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

  myNote := TKntNote(myTreeNode.Data);
  if (not assigned(myNote)) then exit;

  myNote.Bold := (not myNote.Bold);

  newStyle := TV.Font.Style;      // Default TV font can include other styles
  if myNote.Bold then
     newStyle := newStyle + [fsBold];

  myTreeNode.Font.Style := newStyle;

  if DoChildren and myTreeNode.HasChildren then
     BoldChildren(myTreeNode);

  TKntFolder(Folder).Modified:= true;
end; // SetNodeBold


procedure TKntTreeUI.SetNodeCustomImage(TreeNode: TTreeNTNode);
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  NewIdx, ImgIdx : integer;
  DoChildren : boolean;

    procedure SetChildren(StartNode : TTreeNTNode);
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while (assigned(myChildNode) and assigned(myChildNode.Data)) do
      begin
        TKntNote(myChildNode.Data).ImageIndex := newIdx;
        SelectIconForNode(myChildNode);
        if myChildNode.HasChildren then
          SetChildren(myChildNode); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild(myChildNode);
      end;
    end;


begin
  myTreeNode:= TreeNode;
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

  myNote := TKntNote(myTreeNode.Data);
  DoChildren := myTreeNode.HasChildren;
  ImgIdx := myNote.ImageIndex;
  newIdx := PickImage(ImgIdx, DoChildren);

  if ((newIdx <> ImgIdx) and (newIdx <> -1)) then begin
    try
      myNote.ImageIndex := newIdx;
      SelectIconForNode(myTreeNode);

      if DoChildren then begin
         SetChildren(myTreeNode);
         TV.Invalidate;
      end;

    finally
      TKntFolder(Folder).Modified:= true;
    end;
  end;
end; // SetNodeCustomImage


function TKntTreeUI.GetNodeFontFace (TreeNode: TTreeNTNode): string;
var
  Note: TKntNote;

begin
   Result:= '';
   if not assigned(TreeNode) then exit;

   Note:= TKntNote(TreeNode.Data);
   if not assigned(Note) then exit;

   if Note.HasNodeFontFace then
      Result:= Note.NodeFontFace
   else
      Result:= TKntFolder(Folder).TreeChrome.Font.Name;
end;


procedure TKntTreeUI.SetNodeFontFace(TreeNode: TTreeNTNode; const ResetDefault, DoChildren: boolean);
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  myFontFace : string;

    procedure SetFontChildren(StartNode : TTreeNTNode);
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;

      while (assigned(myChildNode) and assigned(myChildNode.Data)) do begin
        myChildNode.Font.Name := myFontFace;

        with TKntNote(myChildNode.Data) do begin
          if ResetDefault then
            NodeFontFace := ''
          else
            NodeFontFace := myFontFace;
        end;

        if myChildNode.HasChildren then
           SetFontChildren(myChildNode); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild(myChildNode);
      end;
    end;


begin
  myTreeNode:= TreeNode;
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

  myNote := TKntNote(myTreeNode.Data);

  try
    if ResetDefault then begin
      myFontFace := TKntFolder(Folder).TreeChrome.Font.Name;
      myNote.NodeFontFace := '';
    end
    else begin
      myFontFace := Form_Main.Combo_Font.FontName;
      myNote.NodeFontFace := myFontFace;
    end;

    myTreeNode.Font.Name := myFontFace;

    if (DoChildren and myTreeNode.HasChildren) then
      SetFontChildren(myTreeNode);

  finally
    TKntFolder(Folder).Modified:= true;
  end;


end; // SetNodeFontFace


procedure TKntTreeUI.SetNodeFontSize(TreeNode: TTreeNTNode; const ResetDefault, DoChildren : boolean);
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
begin
  myTreeNode:= TreeNode;
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

  myNote := TKntNote(myTreeNode.Data);
  try
    myTreeNode.Font.Size := strtoint(Form_Main.Combo_FontSize.Text);
    if (myTreeNode.Font.Size >= (TV.ItemHeight -4)) then
       TV.ItemHeight := myTreeNode.Font.Size+6;
  except
  end;

end; // SetTreeNodeFontSize




function TKntTreeUI.GetNodePath(aNode: TTreeNTNode; const aDelimiter: string; const TopToBottom: boolean) : string;
var
  s : string;
  myNote : TKntNote;
begin
  result := '';

  while assigned(aNode) do begin
    myNote := TKntNote(aNode.Data);

    if (s = '') then
      s := myNote.Name
    else begin
      case TopToBottom of
        false : begin
          s := s + aDelimiter + myNote.Name;
        end;
        true : begin
          s := myNote.Name + aDelimiter + s;
        end;
      end;
    end;
    aNode := aNode.Parent;
  end;

  result := s;

end; // GetNodePath


procedure TKntTreeUI.CopyNodePath(myTreeNode : TTreeNTNode; const InsertInEditor : boolean);
var
  s : string;
begin
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) then exit;

  s := GetNodePath(myTreeNode, TreeOptions.NodeDelimiter, TreeOptions.PathTopToBottom);
  ClipBoard.SetTextBuf(PChar(s));
  if InsertInEditor then begin
    ActiveEditor.SelText := s + ' ';
    ActiveEditor.SelLength := 0;
  end
end; // CopyNodePath


procedure TKntTreeUI.PasteNodeName(myTreeNode : TTreeNTNode; const PasteMode : TPasteNodeNameMode);
var
  myNewName: string;
  p : integer;
  s : string;
begin
  if not Assigned(myTreeNode) then
     myTreeNode := TV.Selected;
  if not Assigned(myTreeNode) or CheckReadOnly then exit;

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
      myTreeNode.Text := myNewName;  // {N}
      TKntNote(myTreeNode.Data).Name := myNewName;
    finally
      TKntFolder(Folder).Modified:= true;
    end;
  end;

end; // PasteNodeName


procedure TKntTreeUI.CopyNodeName(const IncludeNoteText : boolean);
var
  myTreeNode : TTreeNTNode;
begin
  myTreeNode := TV.Selected;
  if (not assigned(myTreeNode)) then exit;

  if IncludeNoteText then
     ClipBoard.AsText:=  myTreeNode.Text + #13#13 + RemoveKNTHiddenCharactersInText(ActiveEditor.Text)
  else
     ClipBoard.AsText:= myTreeNode.Text;
end; // CopyNodeName


procedure TKntTreeUI.RenameFocusedNode;
var
  myFolder: TKntFolder;
  myNote : TKntNote;
  myName : string;
  EditInPlace: boolean;
begin
  if CheckReadOnly then exit;

  myFolder:= TKntFolder(Folder);
  myNote := myFolder.SelectedNote;

  EditInPlace:= TreeOptions.EditInPlace;

  if assigned( myNote ) then begin
    if not Focused  then
       EditInPlace:= false;

    if TreeOptions.EditInPlace then
       TV.Selected.EditText

    else begin
       myName := myNote.Name;
       fOldNoteName := myName;
       if InputQuery( STR_53, STR_54, myName ) then begin
          myName := trim( myName );
          if ( myName <> '' ) then begin
            myNote.Name := myName;
            TV.Selected.Text := myNote.Name;  // TKntNote does NOT update its treenode's properties!
            myFolder.Modified := true;
          end
          else
             App.ErrorPopup(STR_50);
       end;
    end;
  end;
end;




procedure TKntTreeUI.OutlineNumberNodes;
type
  TExistingNumbers = (enNo, enYes, enAuto);
var
  Form_NodeNum : TForm_NodeNum;
  StartNode, myTreeNode, ParentNode : TTreeNTNode;
  SubtreeOnly : boolean;
  StripNames : boolean;
  ExistingNumbers : TExistingNumbers;
  myNote : TKntNote;
  StartNodeLevel, LastNodeLevel, thisNodeLevel : integer;
  StartNumber, thisNumber : integer;
  ParentLevelStr, LevelStr : string;
  DepthLimit : integer;
  ModalResponse : Word;

     function ExtractNodeNumber(const aNode : TTreeNTNode) : string;
     var
       p : integer;
     begin
       result := '';
       if assigned(aNode) then begin
          if StripNames then
            result := aNode.Text
          else begin
            p := pos(#32, aNode.Text);
            if (p > 1) then
              result := copy(aNode.Text, 1, pred(p));
          end;
       end;
     end; // ExtractNodeNumber


     procedure AddNumberToNode;
     var
       tmpstr : string;
       i, SpacePos : integer;

     begin
       if StripNames then
         myNote.Name := LevelStr

       else begin
         case ExistingNumbers of
           enNo : begin
             myNote.Name := LevelStr + #32 + myNote.Name;
           end;
           enYes : begin
             SpacePos := AnsiPos(#32, myNote.Name);
             if (SpacePos > 0) then begin
               tmpstr := myNote.Name;
               delete(tmpstr, 1, SpacePos);
               myNote.Name := LevelStr + #32 + tmpstr;
             end
             else
               myNote.Name := LevelStr;
           end;
           enAuto : begin
             // check if node name begins with a number
             // and if so, strip it
             SpacePos := -1; // flag: node has NO number
             for i := 1 to length(myNote.Name) do begin
                if (i = 1) then begin
                  if (AnsiChar(myNote.Name[1]) in ['0'..'9']) then
                    SpacePos := 0 // flag: node HAS number
                  else
                    break; // node does NOT begin with a number
                end
                else begin
                  if (AnsiChar(myNote.Name[i]) in ['0'..'9', '.']) then
                    continue
                  else
                  if (myNote.Name[i] = #32) then begin
                    SpacePos := i;
                    break;
                  end
                  else begin
                    SpacePos := pred(i);
                    break;
                  end;
                end;
             end;

             if (SpacePos < 0) then begin
               // node name does not have a number
               if (ModalResponse = mrOK) then
                 myNote.Name := LevelStr + #32 + myNote.Name;
             end
             else
             if (SpacePos = 0) then begin
               // whole node name is a number
               if (ModalResponse = mrOK) then
                 myNote.Name := LevelStr;
             end
             else begin
               // node has a number followed by text
               tmpstr := myNote.Name;
               delete(tmpstr, 1, SpacePos);
               if (ModalResponse = mrOK) then
                  myNote.Name := LevelStr + #32 + tmpstr
               else
                  myNote.Name := tmpstr;
             end;
           end;
         end;
       end;
       myTreeNode.Text := myNote.Name;
     end; // AddNumberToNode

begin
  if CheckReadOnly then exit;
  if (TV.Items.Count = 0) then exit;

  Form_NodeNum := TForm_NodeNum.Create(Form_Main);
  try
    ModalResponse := Form_NodeNum.ShowModal;
    if (ModalResponse in [mrOK, mrYesToAll]) then begin

      if (ModalResponse = mrYesToAll) then begin
        if (messagedlg(STR_03, mtConfirmation, [mbOK,mbCancel], 0) <> mrOK) then
          exit;
      end;

      with Form_NodeNum do begin
        SubtreeOnly := (RG_Scope.ItemIndex > 0);
        if SubtreeOnly then
          StartNode := TV.Selected
        else
          StartNode := TV.Items.GetFirstNode;
        StripNames := (RG_Method.ItemIndex > 0);
        ExistingNumbers := TExistingNumbers(RG_CurNum.ItemIndex);
        StartNumber := Spin_StartNum.Value;
        if CB_FullDepth.Checked then
          DepthLimit := 0 // add numbers to all levels
        else
          DepthLimit := Spin_Depth.Value; // descend only DepthLimit levels
      end;

      if (not assigned(StartNode)) then begin
        messagedlg(STR_04, mtError, [mbOK], 0);
        exit;
      end;

      TV.Items.BeginUpdate;
      try

        try

          myTreeNode := StartNode;
          StartNodeLevel := StartNode.Level;
          LastNodeLevel := StartNodeLevel;
          ThisNumber := StartNumber;
          LevelStr := '';

          if (ModalResponse = mrYesToAll) then begin
            StripNames := false;
            SubtreeOnly := false;
            ExistingNumbers := enAuto;
            while assigned(myTreeNode) do begin
              myNote := TKntNote(myTreeNode.Data);
              AddNumberToNode;
              myTreeNode := myTreeNode.GetNext;
            end;
            exit;
          end;


          // first process starting level nodes,
          // because they need different treatment
          // (numbering starts with StartNumber and is not based on .index property)

          while assigned(myTreeNode) do begin
            myNote := TKntNote(myTreeNode.Data);
            LevelStr := IntToStr(ThisNumber);
            inc(ThisNumber);
            AddNumberToNode;

            if SubtreeOnly then
              break; // do not process sibling nodes, we only descend the subtree
            myTreeNode := myTreeNode.GetNextSibling;
          end;

          myTreeNode := StartNode;

          if (DepthLimit <> 1) then begin // only if applying numbers more than 1 level deep

            StartNodeLevel := StartNode.Level; // return to original starting position
            // from now on, we only need to number nodes which are below the
            // initial numbering level and up to the max numbering level

            ParentLevelStr := '';
            LevelStr := '';
            ParentNode := nil;

            while assigned(myTreeNode) do begin
              thisNodeLevel := myTreeNode.Level;
              myNote := TKntNote(myTreeNode.Data);

              if ((DepthLimit = 0) or (thisNodeLevel < (StartNodeLevel + DepthLimit))) then begin
                if (thisNodeLevel > StartNodeLevel) then begin
                   if (ParentNode <> myTreeNode.Parent) then
                      ParentLevelStr := ExtractNodeNumber(myTreeNode.Parent);
                   ParentNode := myTreeNode.Parent;
                   LevelStr := Format('%s.%d', [ParentLevelStr, succ(myTreeNode.Index)]);
                   AddNumberToNode;
                end;
              end;

              // get next node, or bail out
              LastNodeLevel := myTreeNode.Level;
              myTreeNode := myTreeNode.GetNext;

              if SubtreeOnly then begin
                // bail out, we have finished the subtree
                if (assigned(myTreeNode) and (myTreeNode.Level = StartNodeLevel)) then
                  break;
              end;
            end;

          end;

        except
          on E : Exception do
            App.ErrorPopup(E);
        end;

      finally
        TV.Items.EndUpdate;
        TKntFolder(Folder).Modified := true;
      end;
    end;

  finally
    Form_NodeNum.Free;
  end;

end; // OutlineNumberNodes


procedure TKntTreeUI.SortTree;
begin
  if CheckReadOnly then exit;

  if (MessageDlg(STR_49, mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then exit;

  TV.OnChange := nil;
  try
    TV.AlphaSort;
  finally
    TV.OnChange := TVChange;
    TKntFolder(Folder).Modified:= true;
  end;
end; // Sort full tree


procedure TKntTreeUI.SortSubtree(myTreeNode : TTreeNTNode);
begin
  if CheckReadOnly then exit;

  myTreeNode := TV.Selected;
  if (assigned( myTreeNode ) and myTreeNode.HasChildren) then begin
    TV.OnChange := nil;
    try
      myTreeNode.AlphaSort;
    finally
      TV.OnChange := TVChange;
      TKntFolder(Folder).Modified:= true;
    end;
  end;
end;




procedure TKntTreeUI.GetOrSetNodeExpandState(const AsSet, TopLevelOnly : boolean );
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
begin
   // set or get node "expanded" state

   myTreeNode := TV.Items.GetFirstNode;

   while assigned( myTreeNode ) do begin
        myNote := TKntNote( myTreeNode.Data );
        if assigned( myNote ) then begin
           case AsSet of
             true : begin // set
               if TopLevelOnly then begin
                 myTreeNode.Expand( false );
                 myTreeNode := myTreeNode.GetNextSibling
               end
               else begin
                 myTreeNode.Expanded := myNote.Expanded;
                 myTreeNode := myTreeNode.GetNext;
               end;
             end;

             false : begin // get
               myNote.Expanded := myTreeNode.Expanded;
               myTreeNode := myTreeNode.GetNext;
             end;
           end;
        end;
   end;

end; // GetOrSetNodeExpandState


procedure TKntTreeUI.FullCollapse;
begin
  TV.FullCollapse;
  TVChange(nil, TV.Selected);
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




// Create new nodes ================================================================

{$REGION Create new nodes}

function TKntTreeUI.AddNode(aInsMode: TNodeInsertMode) : TTreeNTNode;
begin
  result := NewNode(aInsMode, nil, '', false);
  if (KeyOptions.RunAutoMacros and assigned(result)) then
     ExecuteMacro(_MACRO_AUTORUN_NEW_NODE, '');
end;


function TKntTreeUI.NewNode(aInsMode : TNodeInsertMode; const aOriginNode : TTreeNTNode; const aNewNodeName : string;
                            const aDefaultNode : boolean) : TTreeNTNode;
var
  myNote, myParentNote : TKntNote;
  myTreeNode, myOriginNode, mySiblingNode : TTreeNTNode;
  myName : string;
  p : integer;
  AddingFirstNode, addnumber : boolean;
  Folder: TKntFolder;
begin
  result := nil;
  if CheckReadOnly then exit;

  myTreeNode := nil; { just to avoid }
  myNote := nil;     { compiler warning }

  Folder:= TKntFolder(Self.Folder);

  addnumber := Folder.AutoNumberNodes;
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
      addnumber := false;
    end;

    p := pos(NODECOUNT, myName);
    if (p > 0) then begin
      delete(myName, p, length(NODECOUNT));
      insert(IntToStr(succ(TV.Items.Count)), myName, p);
      addnumber := false;
    end;

    if addnumber then
      myName := Folder.DefaultNoteName + #32 + IntToStr(succ(TV.Items.Count));

  end;

  if (TV.Items.Count = 0) or (not assigned(TV.Selected)) then begin
    AddingFirstNode := true;
    aInsMode := tnTop; // no children or siblings if no nodes
  end
  else
    AddingFirstNode := false;

  try
    try
      TV.OnChange := nil;
      if (aOriginNode = nil) then
         myOriginNode := TV.Selected
      else
         myOriginNode := aOriginNode;

      case aInsMode of
        tnTop :            myTreeNode := TV.Items.AddFirst(nil, myName);
        tnInsertBefore :   myTreeNode := TV.Items.Insert(myOriginNode, myName);
        tnAddLast :        myTreeNode := TV.Items.Add(myOriginNode, myName);
        tnAddChild :       myTreeNode := TV.Items.AddChild(myOriginNode, myName);
        tnAddAfter : begin
            mySiblingNode := myOriginNode.GetNextSibling;
            if assigned(mySiblingNode) then
              myTreeNode := TV.Items.Insert(mySiblingNode, myName)
            else
              myTreeNode := TV.Items.Add(myOriginNode, myName);
        end;
      end;

      result := myTreeNode;

      // these tokens can be expanded only after the node was created
      if (aNewNodeName = '') then begin
         if (pos('%', myName) > 0) then begin
           p := pos(NODELEVEL, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODELEVEL));
             insert(IntToStr(myTreeNode.Level), myName, p);
           end;

           p := pos(NODEINDEX, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEINDEX));
             insert(IntToStr(succ(myTreeNode.Index)), myName, p);
           end;

           p := pos(NODEABSINDEX, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEABSINDEX));
             insert(IntToStr(succ(myTreeNode.AbsoluteIndex)), myName, p);
           end;

           p := pos(NODEPARENT, myName);
           if (p > 0) then begin
             delete(myName, p, length(NODEPARENT));
             if assigned(myTreeNode.Parent) then
               insert(myTreeNode.Parent.Text, myName, p)
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

      Folder.NewNodeAdded(Result, myOriginNode, myName);

      if AddingFirstNode then begin
        ShowOrHideIcons(true);
        ShowOrHideCheckBoxes;
      end;

      SelectIconForNode(myTreeNode.Parent);
      SelectIconForNode(myTreeNode);
      SetupNewTreeNode(myTreeNode);

    except
      on E : Exception do begin
        messagedlg(STR_01 + E.Message, mtError, [mbOK], 0);
        // if assigned(myTreeNode) then myTreeNode.Free;
        // if assigned(myNote) then myNote.Free;
      end;
    end;

  finally
    TV.OnChange := TVChange;
    myTreeNode.MakeVisible;
    TV.Selected := myTreeNode;

  end;

  if ((not aDefaultNode) and assigned(myTreeNode) and TreeOptions.EditNewNodes) then
     RenameFocusedNode;

end; // TreeNewNode


procedure TKntTreeUI.CreateMasterNode;
var
  myNote, nextnode, masternode : TTreeNTNode;
begin
  if CheckReadOnly then exit;

  if (TV.Items.Count > 0) then
    TV.Selected := TV.Items.GetFirstNode;

  masternode := NewNode(tnInsertBefore, nil, '', true);

  if assigned(masternode) then begin
     TV.Items.BeginUpdate;
     try
       myNote := masternode.GetNext;
       while assigned(myNote) do begin
         nextnode := myNote.GetNextSibling;
         myNote.MoveTo(masternode, naAddChild);
         SelectIconForNode(myNote);
         myNote := nextnode;
       end;
     finally
       SelectIconForNode(masternode);
       TV.Items.EndUpdate;
       TV.Selected := masternode;
       TV.SetFocus;
     end;

  end;

end; // CreateMasterNode


procedure TKntTreeUI.CreateNodefromSelection;
var
  myTreeNode, SelectedNode : TTreeNTNode;
  myNote : TKntNote;
  myRTFText: AnsiString;
  myNodeName : string;
  p : integer;
begin
   if CheckReadOnly then exit;
   if (ActiveEditor.SelLength = 0) then begin
     App.WarnNoTextSelected;
     exit;
   end;

  SelectedNode := TV.Selected;
  if (not assigned(SelectedNode)) then begin
    App.ShowInfoInStatusBar(STR_17);
    exit;
  end;

  myNodeName:= FirstLineFromString(TrimLeft(ActiveEditor.SelText), TREENODE_NAME_LENGTH_CAPTURE);

  myRTFText := ActiveEditor.RtfSelText;

  myTreeNode := NewNode(tnAddAfter, nil, '', true);
  if assigned(myTreeNode) then begin
    TV.Items.BeginUpdate;
    try
      TV.Selected := myTreeNode;
      myNote := TKntNote(myTreeNode.Data);

      if (myNodeName <> '') then begin   // can be blank
        myNote.Name := myNodeName;
        myTreeNode.Text := myNote.Name;
      end;

      myNote.Stream.Position := 0;
      myNote.Stream.WriteBuffer(myRTFTExt[1], length(myRTFTExt));
      TKntFolder(Folder).DataStreamToEditor;

      myTreeNode.MakeVisible;

    finally
      TV.Items.EndUpdate;
      TKntFolder(Folder).Modified := true;
    end;
  end;

end; // CreateNodefromSelection


procedure TKntTreeUI.SetupNewTreeNode(const aTreeNode : TTreeNTNode);
var
  myNote : TKntNote;
begin
  if assigned(aTreeNode) then begin

    myNote := TKntNote(aTreeNode.Data);
    if assigned(myNote) then begin
      TV.OnChecked:= nil;

      // [x] FIXME: in many cases by doing this we are setting
      // the treenode text TWICE. In some cases this line is
      // necessary, though. Bad code design.
      aTreeNode.Text := myNote.Name; // {N}

      if myNote.Filtered then
         aTreeNode.Hidden:= True;

      if myNote.Bold then
        aTreeNode.Font.Style := TV.Font.Style + [fsBold];

      if myNote.ChildrenCheckbox or ShowAllCheckboxes then       // sets the check type for this node's children and changes the children's check image
         aTreeNode.CheckType  := ctCheckBox
      else
         aTreeNode.CheckType  := ctNone;

      if myNote.Checked then
        aTreeNode.CheckState := csChecked
      else
        aTreeNode.CheckState := csUnchecked;

      if myNote.HasNodeFontFace then
        aTreeNode.Font.Name := myNote.NodeFontFace;

      if myNote.HasNodeColor then
        aTreeNode.Font.Color := myNote.NodeColor;

      if myNote.HasNodeBGColor then
        aTreeNode.Color := myNote.NodeBGColor;

      TV.OnChecked:= TVChecked;
    end;
  end;
end; // UpdateTreeNode


{$ENDREGION}


// Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees

{$REGION Move Nodes |  Delete nodes/subtrees  |  Cut/Copy/Paste Subtrees}

procedure TKntTreeUI.MoveTreeNode(MovingNode : TTreeNTNode; const aDir : TDirection);
var
  t : string;
  PreviousParent, theSibling : TTreeNTNode;

begin
  if not Assigned(MovingNode) then
     MovingNode := TV.Selected;
  if not Assigned(MovingNode) or CheckReadOnly then exit;

  t := STR_05;
  TV.OnChange := nil;
  TV.OnChecked:= nil;

  fIsAnyNodeMoving:= true;
  try
    try
      PreviousParent := MovingNode.Parent;

      case aDir of
        // UP / DOWN move node only within its siblings
        // (cannot change level)
        dirUp : begin
          //theSibling := MovingNode.GetPrevSibling; [dpv]
          theSibling := MovingNode.GetPrevSiblingNotHidden;
          if assigned(theSibling) then begin
            //theSibling := theSibling.GetPrevSibling; // looks weird but does the Right Thing
            theSibling := theSibling.GetPrevSiblingNotHidden; // looks weird but does the Right Thing
            if assigned(theSibling) then
              MovingNode.MoveTo(theSibling, naInsert)
            else
              MovingNode.MoveTo(MovingNode.Parent, naAddChildFirst);
            t := '';
          end;
        end;
        dirDown : begin
          //theSibling := MovingNode.GetNextSibling;   [dpv]
          theSibling := MovingNode.GetNextSiblingNotHidden;
          if assigned(theSibling) then begin
            MovingNode.MoveTo(theSibling, naInsert);
            t := '';
          end;
        end;

        // LEFT promotes node 1 level up
        // RIGHT demotes node 1 level down
        dirLeft : begin
          if assigned(MovingNode.Parent) then begin
            // becomes its parent's sibling
            MovingNode.Moveto(MovingNode.Parent, naInsert);
            t := '';
          end;
        end;
        dirRight : begin
          theSibling := MovingNode.GetPrevSibling;
          if assigned(theSibling) then begin
            // becomes the last child of its previous sibling
            MovingNode.MoveTo(theSibling, naAddChild);
            t := '';
          end;
        end;
      end;


      if (t = '') then begin // means node was successfully moved
        // update node icon
        SelectIconForNode(PreviousParent);
        SelectIconForNode(MovingNode);
        SelectIconForNode(MovingNode.Parent);
        TV.Invalidate;
      end;

    except
      on E : Exception do
        App.ErrorPopup(E, STR_06);
    end;
  finally
    fIsAnyNodeMoving:= false;
    TKntFolder(Folder).Modified:= true;
    if TKntNote(MovingNode.Data).Checked then
       MovingNode.CheckState := csChecked;

    TV.OnChecked:= TVChecked;
    TV.OnChange := TVChange;

    App.ShowInfoInStatusBar(Format(STR_07, [MovingNode.Text,t,DIRECTION_NAMES[aDir]]));
  end;

end; // MoveTreeNode


procedure TKntTreeUI.DeleteNode(myTreeNode: TTreeNTNode; const DeleteOnlyChildren: boolean; const AskForConfirmation: boolean = true);
var
  myTreeParent, myTreeChild, myNextChild : TTreeNTNode;
  KeepChildNodes : boolean;
  Folder: TKntFolder;
begin
  with Form_Main do begin
      KeepChildNodes := false;

      if CheckReadOnly then exit;

      if not Assigned(myTreeNode) then
         myTreeNode := TV.Selected;
      if not Assigned(myTreeNode) then exit;

      myTreeParent := myTreeNode.Parent;
      Folder:= TKntFolder(Self.Folder);

      if AskForConfirmation then begin

         if DeleteOnlyChildren then begin
            // command was to delete CHILDREN of focused node
            if myTreeNode.HasChildren then begin
              if (DoMessageBox(Format(STR_12, [myTreeNode.Count, myTreeNode.Text]) + STR_08, STR_10,
                   MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES) then exit;
            end
            else begin
              showmessage(STR_13);
              exit;
            end;
         end
         else begin
            // delete focused node and all its children, if any

            if myTreeNode.HasChildren then begin
              // ALWAYS warn if node has children (except if we are moving to another location)
              case DoMessageBox(
                Format(STR_09, [myTreeNode.Text, myTreeNode.Count]) + STR_08, STR_10,
                    MB_YESNOCANCEL+MB_ICONEXCLAMATION+MB_DEFBUTTON3+MB_APPLMODAL) of
                ID_YES : KeepChildNodes := false;
                ID_NO  : KeepChildNodes := true;
                else
                  exit;
              end;
            end
            else begin
              if TreeOptions.ConfirmNodeDelete then begin
                if (App.DoMessageBox(Format(STR_11, [myTreeNode.Text]) + STR_08, mtWarning, [mbYes,mbNo], 0) <> mrYes) then exit;
              end;
            end;
         end;
      end;

      with TV do begin
        OnChange := nil;
        OnDeletion := TVDeletion;
        Items.BeginUpdate;
      end;

      try
        try
          if DeleteOnlyChildren then begin
             myTreeNode.DeleteChildren;
             SelectIconForNode(myTreeNode);
          end
          else begin
             if KeepChildNodes then begin
               myTreeChild := myTreeNode.GetFirstChild;
               while assigned(myTreeChild) do begin
                 myNextChild := myTreeNode.GetNextChild(myTreeChild);
                 myTreeChild.MoveTo(myTreeParent, naAddChild);
                 SelectIconForNode(myTreeChild);
                 myTreeChild := myNextChild;
               end;
             end;

             TV.Items.Delete(myTreeNode);
             SelectIconForNode(myTreeParent);
          end;

        except
          on E : Exception do
            App.ErrorPopup(E, STR_14);
        end;

      finally
        fLastNodeSelected:= nil;

        with TV do begin
          OnDeletion := nil;
          OnChange := TVChange;
          Items.EndUpdate;
        end;

        myTreeNode:= TV.Selected;
        if assigned(myTreeNode) then
           TVChange(nil, myTreeNode)
        else
           Folder.NoNodeInTree;

        Folder.Modified:= true;
      end;
  end;

end; // DeleteNode


procedure TKntTreeUI.PasteSubtree;
begin
  if assigned(fMovingTreeNode) then begin
     if MoveSubtree(nil) then
        fMovingTreeNode:= nil;
  end
  else
     TreeTransferProc(ttPaste, KeyOptions.ConfirmTreePaste, false, false );
end;

function TKntTreeUI.MoveSubtree(TargetNode: TTreeNTNode): boolean;
var
  myTreeParent : TTreeNTNode;
  SourceFolder: TKntFolder;
  SourceTreeUI: TKntTreeUI;

begin
   Result:= false;
   if not assigned(fMovingTreeNode) or CheckReadOnly then exit;

   if not assigned(TargetNode) then
      TargetNode := TV.Selected;
   if not assigned(TargetNode) then begin
      App.WarningPopup(STR_15);
      exit;
   end;
   if (fMovingTreeNode = TargetNode) then exit;

   SourceFolder:= ActiveFile.GetFolderByTreeNode(fMovingTreeNode);
   if SourceFolder = nil then exit;
   SourceTreeUI:= SourceFolder.TreeUI;
   if SourceTreeUI.CheckReadOnly then exit;

   myTreeParent := fMovingTreeNode.Parent;

   if (App.DoMessageBox(Format(STR_16,  [fTransferNodes.Count, fMovingTreeNode.Text, TargetNode.Text]),
                        mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
       exit;

   // Paste
   TreeTransferProc(ttPaste, false, false, true);  // Graft Subtree    (fMovingTreeNode<> nil =>  TransferNodes.Count > 0)

   // .. and Cut
   SourceTreeUI.DeleteNode(fMovingTreeNode, false, false);  // Include children and do not ask for confirmation
   Result:= true;

end; // MoveSubtree


function TKntTreeUI.TreeTransferProc(
                    const XferAction: TTreeTransferAction;
                    const Prompt: boolean; const PasteAsVirtualKNTNode: boolean; const MovingSubtree: boolean) : boolean;
var
  newNote : TKntNote;
  myTreeNode, newTreeNode, LastNodeAssigned, FirstCopiedNode : TTreeNTNode;
  i, loop, PasteCount, StartLevel, LastLevel : integer;
  VirtualNodesConverted : integer;
  movingNoteNode, TransferedNoteNode : TTreeNTNode;
  RTFAux : TRxRichEdit;
  Folder: TKntFolder;

  function CountVisibleTransferNodes: integer;
  var
     i: integer;
     TransferedNoteNode : TTreeNTNode;
  begin
      Result:= 0;
      for i := 0 to pred(fTransferNodes.Count) do begin
          TransferedNoteNode:= ActiveFile.GetTreeNode(fCopyCutFromFolderID, fTransferNodes[i].ID, fTransferNodes[i].GID);
          if assigned(TransferedNoteNode) and not TransferedNoteNode.Hidden then
             Result:= Result+1;
      end;
  end;

begin
  result := false;
  if (XferAction = ttClear) then begin   //  CLEAR -------------
     result := true;
     if assigned(fTransferNodes) then begin
       if (messagedlg(Format(STR_18, [fTransferNodes.Count]), mtConfirmation, [mbYes,mbNo], 0) = mrYes) then
          fTransferNodes.Free;
       fTransferNodes := nil;
     end;
     fMovingTreeNode:= nil;
     exit;
  end;


  myTreeNode:= TV.Selected;
  if (myTreeNode = nil) then begin
    App.InfoPopup(STR_15);
    exit;
  end;

  Folder:= TKntFolder(Self.Folder);

  screen.Cursor := crHourGlass;
  try
    try

      case XferAction of
        ttCopy : begin                                    // COPY subtree ======================================
           fMovingTreeNode:= nil;
           if MovingSubtree then
              fMovingTreeNode:= myTreeNode;

           Folder.EditorToDataStream;
           fCopyCutFromFolderID:= Folder.ID;

           if assigned(fTransferNodes) then
              fTransferNodes.Free;
           fTransferNodes := TKntNoteList.Create;
           StartLevel := myTreeNode.Level;
           while assigned(myTreeNode) do begin
               newNote := TKntNote.Create;
               newNote.Assign(TKntNote(myTreeNode.Data));
               newNote.Level := myTreeNode.Level - StartLevel;
               newNote.ID:= TKntNote(myTreeNode.Data).ID;
               newNote.GID:= TKntNote(myTreeNode.Data).GID;

               fTransferNodes.Add(newNote);
               myTreeNode := myTreeNode.GetNext;
               if ((myTreeNode <> nil) and (myTreeNode.Level <= StartLevel)) then
                   myTreeNode := nil; // end of subtree; break out of loop
           end;

           if (fTransferNodes.Count = 0) then begin
             App.InfoPopup(STR_19);
             fTransferNodes.Free;
             fTransferNodes := nil;
           end
           else begin
             result := true;
             App.ShowInfoInStatusBar(Format(STR_20, [fTransferNodes.Count]));
           end;
        end;


        ttPaste : begin                                   // PASTE subtree ======================================
           if CheckReadOnly then exit;

           if (not assigned(fTransferNodes)) then begin
             App.InfoPopup(STR_21);
             exit;
           end;

           if PasteAsVirtualKNTNode then begin
              if Prompt then
                 if (App.DoMessageBox(Format(STR_26, [CountVisibleTransferNodes, myTreeNode.Text]),
                                      mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
                     exit;
           end
           else begin
               if fTransferNodes.HasVirtualNotes then begin
                 if (messagedlg(STR_22, mtWarning, [mbYes,mbNo], 0) <> mrYes) then
                    exit;
               end
               else begin
                 if Prompt then
                    if (App.DoMessageBox(Format(STR_23, [fTransferNodes.Count,myTreeNode.Text]),
                                         mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then // {N}
                       exit;
               end;
           end;

           Folder.Modified:= true;

           LastNodeAssigned := myTreeNode;
           PasteCount := 0;
           VirtualNodesConverted := 0;
           FirstCopiedNode := nil;

           StartLevel := myTreeNode.Level;
           LastLevel := StartLevel+1;

           TV.Items.BeginUpdate;
           RTFAux:= CreateAuxRichEdit;

           TV.OnChange:= nil;
           try
             for i := 0 to pred(fTransferNodes.Count) do begin
                 TransferedNoteNode:= ActiveFile.GetTreeNode(fCopyCutFromFolderID, fTransferNodes[i].ID, fTransferNodes[i].GID);

                 if not (PasteAsVirtualKNTNode and TransferedNoteNode.Hidden) then begin
                     newNote := TKntNote.Create;
                     newNote.Assign(fTransferNodes[i]);

                     // As indicated in comment *1 in FileDropped (kn_NoteFileMng), we must check if it is neccesary to ensure that
                     // the new node's stream is loaded with RTF and not plain text.
                     if (not Folder.PlainText) and (not NodeStreamIsRTF (newNote.Stream)) then
                         ConvertStreamContent(newNote.Stream, sfPlainText, sfRichText, RTFAux)
                     else
                     if (Folder.PlainText) and (NodeStreamIsRTF (newNote.Stream)) then
                         // This case is not as problematic as the other, but if the new node were not modified, it would save
                         // its RTF content in a plain manner. Example:
                         // ;{{\rtf1\fbidis\ ....
                         // ;\par...
                         ConvertStreamContent(newNote.Stream, sfRichText, sfPlainText, RTFAux);

                     if MovingSubtree then begin
                        newNote.GID:= fTransferNodes[i].GID;
                        AlarmMng.MoveAlarms(KntFile.GetFolderByID(fCopyCutFromFolderID), fTransferNodes[i],  Folder, newNote);
                     end;

                     Folder.AddNote(newNote);
                     newNote.Level := newNote.Level + StartLevel + 1;

                     if (i = 0) then begin
                       newTreeNode := TV.Items.AddChildFirst(myTreeNode, newNote.Name);
                       FirstCopiedNode := newTreeNode;
                     end
                     else begin
                        case DoTrinaryCompare(newNote.Level, LastLevel) of
                          trinGreater:  newTreeNode := TV.Items.AddChild(LastNodeAssigned, newNote.Name);
                          trinEqual:    newTreeNode := TV.Items.Add(LastNodeAssigned, newNote.Name);
                          else begin
                            for loop := 1 to ((LastLevel - newNote.Level)) do
                               LastNodeAssigned := LastNodeAssigned.Parent;
                            newTreeNode := TV.Items.Add(LastNodeAssigned, newNote.Name);
                          end;
                        end;
                     end;

                     if assigned(newNote) then begin
                        LastLevel := newTreeNode.Level;
                        LastNodeAssigned := newTreeNode;
                        newNote.Level := newTreeNode.Level;

                        newTreeNode.Data := newNote;
                        SetupNewTreeNode(newTreeNode);

                        if PasteAsVirtualKNTNode then begin
                           newNote.MirrorNode:= TransferedNoteNode;
                           AddMirrorNode(TransferedNoteNode, newTreeNode);
                        end
                        else
                        if newNote.VirtualMode = vmKNTNode  then begin
                           newNote.MirrorNode:= TKntNote(TransferedNoteNode.Data).MirrorNode;
                           AddMirrorNode(newNote.MirrorNode, newTreeNode);
                        end
                        else begin
                           if (newNote.VirtualMode <> vmNone) then begin
                              if KntFile.HasVirtualNoteByFileName(newNote, newNote.VirtualFN) then begin
                                inc(VirtualNodesConverted);
                                newNote.VirtualMode := vmNone;
                                newNote.VirtualFN := '';
                              end;
                           end
                           else
                           {
                             The purpose of the UpdateImagesCountReferences call is to increase the references corresponding to images that may contain the
                            nodes that are being copied or moved
                              * if the nodes are moved, the corresponding references will end up being decremented when the source nodes are deleted.
                               But it is also possible for the user to totally or partially cancel this deletion, which in this way -increasing and decrementing
                              independently- is not a problem.

                             Even if nodes containing images are copied or moved to a PlainText folder, it is not necessary to expressly address it here,
                            to ensure that image references are not increased since earlier in this TreeTransferProc procedure the movements between PlainText
                             and not PlainText (and vice versa) are managed by calling the ConvertStreamContent(...) method.
                             In the case of converting from sfRichText to sfPlainText, in the node stream, which if it has images they will be stored as imLink,
                             the RTF controls will be eliminated, among which are the labels that identify the images (\v\'11I.. .\v0).
                            Therefore, calling KntFile.UpdateImagesCountReferences() will not increment any references. Anyway, to avoid that image search
                             (quick but unnecessary), we will avoid the call if the destination folder is PlainText.
                           }
                           if not (Folder.PlainText) then
                              ActiveFile.UpdateImagesCountReferences (newNote);

                           if MovingSubtree then begin
                              movingNoteNode:= TransferedNoteNode;
                              if assigned(movingNoteNode) then
                                  ManageMirrorNodes(maMovingToTarget, movingNoteNode, newTreeNode);
                           end
                        end;
                     end;
                     inc(PasteCount);
                 end;     //if not (PasteAsVirtualKNTNode ....

             end;  // for

             newTreeNode := myTreeNode;

             while assigned(newTreeNode) do begin
                SelectIconForNode(newTreeNode);
                newTreeNode := newTreeNode.GetNext;
                if ((newTreeNode <> nil) and (newTreeNode.Level <= StartLevel)) then
                   newTreeNode := nil; // end of subtree; break out of loop
             end;
             result := true;
             App.ShowInfoInStatusBar(Format(STR_24, [PasteCount]));
             if assigned(FirstCopiedNode) then begin
               FirstCopiedNode.Collapse(true);
               FirstCopiedNode.MakeVisible;
               // myFolder.TV.Selected := FirstCopiedNode;
             end;

           finally
             TV.Items.EndUpdate;
             TV.OnChange:= TVChange;
             TV.Selected := myTreeNode;
             RTFAux.Free;
             // myTreeNode.Expand(true);
             if (VirtualNodesConverted > 0) then begin
               showmessage(Format(STR_25,[VirtualNodesConverted]));
             end;
           end;
        end;
      end;

    except
      on E : Exception do
        showmessage(E.Message);
    end;

  finally
    screen.Cursor := crDefault;
  end;

end; // TreeTransferProc

{$ENDREGION}


// Check State ==============================

{$REGION Check State}

procedure TKntTreeUI.ShowOrHideCheckBoxes;
var
  node : TTreeNTNode;
  myNote : TKntNote;
begin
  node:= TV.Selected;

  TV.Items.BeginUpdate;
  try
    if ShowAllCheckboxes then begin
      TV.Items.TopLevelCheckType := ctCheckBox;
      node := TV.Items.GetFirstNode;
      while assigned(node) do begin
        node.CheckType := ctCheckBox;
        myNote := TKntNote(node.Data);
        if myNote.Checked then
           node.CheckState := csChecked
        else
           node.CheckState := csUnchecked;
        node := node.GetNext;
      end;
    end
    else begin
      TV.Items.TopLevelCheckType := ctNone;
      node := TV.Items.GetFirstNode;
      while assigned(node) do begin
        ShowOrHideChildrenCheckBoxes (node);
        node := node.GetNext;
      end;
    end;
  finally
    TV.Items.EndUpdate;
  end;
end; // ShowOrHideCheckBoxes


procedure TKntTreeUI.ShowOrHideChildrenCheckBoxes(const ANode : TTreeNTNode);
var
  myNote : TKntNote;
  node : TTreeNTNode;

begin

 try
  ANode.Owner.BeginUpdate;
  myNote := TKntNote(ANode.Data);
  if myNote.ChildrenCheckbox then begin
    ANode.CheckType  := ctCheckBox;
    node := ANode.GetFirstChild;
    while assigned(node) do begin
      myNote := TKntNote(node.Data);
      if myNote.Checked then
         node.CheckState := csChecked
      else
         node.CheckState := csUnchecked;
      node := node.GetNextSibling;
    end
  end
  else
    ANode.CheckType := ctNone;

 finally
   ANode.Owner.EndUpdate;
 end;
end; // ShowOrHideChildrenCheckBoxes


procedure TKntTreeUI.ToggleChildrenCheckbox(myTreeNode : TTreeNTNode);
var
  myNote : TKntNote;
begin
  if CheckReadOnly then exit;

  myTreeNode := TV.Selected;
  if not assigned(myTreeNode) then exit;
  myNote := TKntNote(myTreeNode.Data);

  myNote.ChildrenCheckbox := ( not myNote.ChildrenCheckbox );
  ShowOrHideChildrenCheckBoxes (myTreeNode);
end;


procedure TKntTreeUI.ToggleCheckNode(myTreeNode : TTreeNTNode);
var
  myNote : TKntNote;
begin
  if CheckReadOnly then exit;

  myTreeNode := TV.Selected;
  if not assigned(myTreeNode) then exit;
  myNote := TKntNote(myTreeNode.Data);

  myNote.Checked := (not myNote.Checked);
  if myNote.Checked then
     myTreeNode.CheckState := csChecked
  else
     myTreeNode.CheckState := csUnchecked;
end;


procedure TKntTreeUI.HideChildNodesUponCheckState (ParentNode: TTreeNTNode; CheckState: TCheckState);
var
  Node : TTreeNTNode;
begin
   TV.Items.BeginUpdate;

   if ParentNode = nil then begin
      Node := TV.Items.GetFirstNode;
      while assigned(Node) do begin // go through all nodes
        if Node.CheckState =  CheckState then
           Node.Hidden := True;
        Node := Node.GetNext; // select next node to search
      end;
   end
   else begin
      Node := ParentNode.GetFirstChild;
      while assigned(Node) do begin
        if Node.CheckState =  CheckState then
           Node.Hidden := True;
        Node := Node.GetNextSibling;
      end

   end;

   TV.Items.EndUpdate;
end;


procedure TKntTreeUI.ShowCheckedNodes (ParentNode: TTreeNTNode);
var
  Node : TTreeNTNode;
begin
  TV.Items.BeginUpdate;
  if ParentNode = nil then begin
     Node := TV.Items.GetFirstNode;
     while Node <> nil do begin
       if not TKntNote(Node.Data).Filtered then
          Node.Hidden := False;
       Node := Node.GetNext;
     end;
  end
  else begin
     Node := ParentNode.GetFirstChild;
     while Node <> nil do begin
       if not TKntNote(Node.Data).Filtered then
          Node.Hidden := False;
       Node := Node.GetNextSibling;
     end;
  end;

  TV.Items.EndUpdate;
end;


procedure TKntTreeUI.ChangeCheckedState(Node: TTreeNTNode; Checked: Boolean; CalledFromMirrorNode: Boolean);
var
  myNote : TKntNote;

    procedure CheckChildren(StartNode : TTreeNTNode);
    var
      childNode : TTreeNTNode;
    begin
      childNode := StartNode.GetFirstChild;
      while (assigned(childNode) and assigned(childNode.Data)) do begin
        childNode.CheckState := node.CheckState;
        TKntNote(childNode.Data).Checked := (node.CheckState = csChecked);
        if childNode.HasChildren then
           CheckChildren(childNode); // RECURSIVE CALL
        childNode := StartNode.GetNextChild(childNode);
      end;
    end;

begin
  if (assigned(node) and assigned(node.Data)) then begin
    TV.OnChecked := nil;

    try
      myNote := TKntNote(node.Data);
      if not CalledFromMirrorNode then
          myNote.Checked := (node.CheckState = csChecked)

      else begin
         myNote.Checked := Checked;
         if Checked then
            node.CheckState := csChecked
         else
            node.CheckState := csUnchecked;
      end;
      if not CalledFromMirrorNode then
         ManageMirrorNodes(maChangingChkState, Node, nil);

      if (not CalledFromMirrorNode and shiftdown and node.HasChildren and not fIsAnyNodeMoving) then
         CheckChildren(node);

      if not fIsAnyNodeMoving and HideCheckedNodes  then
         if (node.CheckState  = csChecked) then
            node.Hidden := True
         else
            node.Hidden := False;

    finally
        TKntFolder(Folder).Modified:= true;
        TV.OnChecked := TVChecked;
    end;
  end;
end;

{$ENDREGION}


// Filter nodes ==============================

{$REGION Filter nodes }

procedure TKntTreeUI.MarkAllFiltered;
var
  Node: TTreeNTNode;
begin
  if not assigned(folder) then exit;

  Node := TV.Items.GetFirstNode;
  while Node <> nil do begin
    TKntNote(Node.Data).Filtered := true;
    Node := Node.GetNext;
  end;
end;


procedure TKntTreeUI.MarkAllUnfiltered;
var
  Node: TTreeNTNode;
begin
  if not assigned(folder) then exit;

  Node := TV.Items.GetFirstNode;
  while Node <> nil do begin
    Node.Font.Color:= TKntNote(Node.Data).NodeColor;
    TKntNote(Node.Data).Filtered := false;
    Node := Node.GetNext;
  end;
end;


procedure TKntTreeUI.RemoveFilter;
begin
  if not assigned(folder) then exit;

  TV.Items.BeginUpdate;
  MarkAllUnfiltered;
  TV.FullNotHidden;
  if HideCheckedNodes then
     HideChildNodesUponCheckState (nil, csChecked);

  TV.Items.EndUpdate;
end;


procedure TKntTreeUI.HideFilteredNodes;
var
  Node : TTreeNTNode;
begin
  TV.Items.BeginUpdate;
  Node := TV.Items.GetFirstNode;
  while Node <> nil do begin
    if TKntNote(Node.Data).Filtered then
       Node.Hidden:= true
    else begin
       Node.MakeVisibilityPosible;
       Node.Font.Color := clBlue;
    end;
    Node := Node.GetNext;
  end;
  TV.Items.EndUpdate;
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
      Folder.TreeMaxWidth:= -W                       // Enable TreeMaxWidth and set fixed state

   else begin
      if CtrlDown then begin
         Folder.TreeMaxWidth:= W;                          // Change MaxWidth
         fTreeWidthExpanded:= True;
      end
      else begin                                                 // Change normal width (MaxWidth not modified)
         Folder.TreeWidth:= W;
         Folder.TreeMaxWidth:= Abs(Folder.TreeMaxWidth);     // Disable fixed state
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


{$ENDREGION}



// Mirror Nodes ========================================================================

{$REGION Mirror nodes management}

procedure TKntTreeUI.InsertMirrorNode(myTreeNode: TTreeNTNode);
var
  mirrorNode: TTreeNTNode;
begin
  if assigned(myTreeNode) then begin
     mirrorNode:= AddNode(tnInsertBefore);

     TKntNote(mirrorNode.Data).Assign(myTreeNode.Data);
     TKntNote(mirrorNode.Data).MirrorNode:= myTreeNode;
     AddMirrorNode(myTreeNode, mirrorNode);

     SetupNewTreeNode(mirrorNode);
     SelectIconForNode(mirrorNode);
     TKntFolder(Folder).DataStreamToEditor;
  end;
end;


procedure TKntTreeUI.SetupMirrorNodes;
var
  Node, Mirror : TTreeNTNode;
  p: integer;
  Folder: TKntFolder;

begin
    Folder:= TKntFolder(Self.Folder);
    Node := TV.Items.GetFirstNode;
    while assigned( Node ) do begin // go through all nodes
        if assigned(Node.Data) and (TKntNote(Node.Data).VirtualMode= vmKNTNode) then begin
           TKntNote(Node.Data).LoadMirrorNode;
           Mirror:= TKntNote(Node.Data).MirrorNode;
           if assigned(Mirror) then
              AddMirrorNode(Mirror, Node)
           else
              SelectIconForNode(Node);
        end;
        Node := Node.GetNext; // select next node to search
    end;

    if (Folder = ActiveFolder) and assigned(TV.Selected)
         and (TKntNote(TV.Selected.Data).VirtualMode = vmKNTNode) then
       Folder.DataStreamToEditor;
end;


procedure TKntTreeUI.ManageMirrorNodesOnDeleteTree;
var
  Node : TTreeNTNode;
begin
    Node := TV.Items.GetFirstNode;
    while assigned( Node ) do begin // go through all nodes
        TKntTreeUI.ManageMirrorNodes(maDeleting, Node, nil);
        Node := Node.GetNext; // select next node to search
    end;
end;


class function TKntTreeUI.GetMirrorNodes(originalNode: TTreeNTNode): Pointer;
var
   p: Pointer;
begin
   p:= nil;
   if TKntNote(originalNode.Data).HasMirrorNodes then
      fMirrorNodes.Find(originalNode, p);

   Result:= p;
end;


class procedure TKntTreeUI.AddMirrorNode(MainNode: TTreeNTNode; Mirror_Node: TTreeNTNode);
var
   p: Pointer;
   o: TObject;
   NodesVirtual: TList;
begin
    if not assigned(MainNode) then exit;

    if TKntNote(MainNode.Data).VirtualMode = vmKNTnode then
       MainNode:= TKntNote(MainNode.Data).MirrorNode;

    p:= nil;
    fMirrorNodes.Find(MainNode, p);
    if assigned(p) then begin
       o:= p;
       if o is TTreeNTNode then begin
          NodesVirtual:= TList.Create();
          NodesVirtual.Add(o);
          NodesVirtual.Add(Mirror_Node);
          fMirrorNodes.Remove(MainNode);
          fMirrorNodes.Add(MainNode, NodesVirtual);
       end
       else begin
          NodesVirtual:= p;
          NodesVirtual.Add(Mirror_Node);
       end;
    end
    else begin        // First mirror of originalNode
      fMirrorNodes.Add(MainNode, Mirror_Node);
      TKntNote(MainNode.Data).AddedMirrorNode;         // mark original node
    end;
end;


class procedure TKntTreeUI.ReplaceNonVirtualNote(MainNode: TTreeNTNode; newNode: TTreeNTNode);
var
   p: Pointer;
begin
   p:= nil;
   fMirrorNodes.Find(MainNode, p);
   if assigned(p) then begin
      fMirrorNodes.Remove(MainNode);
      fMirrorNodes.Add(newNode, p);
      TKntNote(MainNode.Data).RemovedAllMirrorNodes;   // mark node
      TKntNote(newNode.Data).AddedMirrorNode;          // mark node
   end;
end;


class procedure TKntTreeUI.RemoveMirrorNode(MainNode: TTreeNTNode; mirror_Node: TTreeNTNode);
var
   p: Pointer;
   o: TObject;
   NodesVirtual: TList;
   RemovedAllMirrorNodes: boolean;
begin
    p:= nil;
    RemovedAllMirrorNodes:= false;
    fMirrorNodes.Find(MainNode, p);
    if assigned(p) then begin
       o:= p;
       if o is TTreeNTNode then begin        // There was only one mirror of originalNode
          if o = mirror_Node then
             RemovedAllMirrorNodes:= true;
       end
       else begin
          NodesVirtual:= p;
          NodesVirtual.Remove(mirror_Node);
          if NodesVirtual.Count = 0 then begin
             NodesVirtual.Free;      // Free the TList
             RemovedAllMirrorNodes:= true;
          end;
       end;
    end;
    if RemovedAllMirrorNodes then begin
       fMirrorNodes.Remove(MainNode);
       TKntNote(MainNode.Data).RemovedAllMirrorNodes;        // mark original node
    end;
end;


class procedure TKntTreeUI.ManageMirrorNodes(Action: TMirrorAction; node: TTreeNTNode; targetNode: TTreeNTNode);
var
    nonVirtualTreeNode, newNonVirtualTreeNode: TTreeNTNode;
    i: integer;
    myNote: TKntNote;

    p: Pointer;
    o: TObject;
    NodesVirtual: TList;
    TreeUI: TKntTreeUI;

    procedure ManageVirtualNode (NodeVirtual: TTreeNTNode);
    begin
       if not assigned(NodeVirtual) then exit;
       myNote:= NodeVirtual.Data;
       if not assigned(myNote) then exit;
       case Action of
          maMovingToTarget: myNote.MirrorNode:= targetNode;

          maChangingChkState:
             if NodeVirtual <> node then begin
                TreeUI:= GetTreeUI(TTreeNT(NodeVirtual.TreeView));
                if TreeUI <> nil then
                   TreeUI.ChangeCheckedState(NodeVirtual, (node.CheckState = csChecked), true);
             end;

          maDeleting:
             if not assigned(newNonVirtualTreeNode) then begin
                newNonVirtualTreeNode:= NodeVirtual;
                myNote.MirrorNode:= nil;
                TKntNote(node.Data).Stream.SaveToStream(myNote.Stream);
             end
             else
                myNote.MirrorNode:= newNonVirtualTreeNode;
       end;
    end;

begin
   if not assigned(node) or not assigned(node.Data) then exit;

  // maMovingToTarget:    Moving node to targetNode
  // maChangingChkState:  Changed checked state of node
  // maDeleting:          Deleting node
  try
      myNote:= TKntNote(node.Data);
      if myNote.VirtualMode = vmKNTNode then begin
          nonVirtualTreeNode:= myNote.MirrorNode;
          if not assigned(nonVirtualTreeNode) then exit;
          case Action of
            maMovingToTarget: exit;
            maChangingChkState: begin
               TreeUI:= GetTreeUI(TTreeNT(nonVirtualTreeNode.TreeView));
               if TreeUI <> nil then
                  TreeUI.ChangeCheckedState(nonVirtualTreeNode, (node.CheckState = csChecked), true);
            end;
            maDeleting: begin
               RemoveMirrorNode(nonVirtualTreeNode, Node);
               exit;
            end;
          end;
      end
      else
          nonVirtualTreeNode:= node;

      p:= GetMirrorNodes(nonVirtualTreeNode);
      if assigned(p) then begin
         newNonVirtualTreeNode:= nil;
         o:= p;
         if o is TTreeNTNode then
            ManageVirtualNode(TTreeNTNode(p))
         else begin
           NodesVirtual:= p;
           for i := 0 to pred( NodesVirtual.Count ) do
              ManageVirtualNode(NodesVirtual[i]);
         end;

         case Action of
            maMovingToTarget: ReplaceNonVirtualNote(nonVirtualTreeNode, targetNode);
            maDeleting: begin
                 if assigned(newNonVirtualTreeNode) and assigned(newNonVirtualTreeNode.Data) then begin
                   RemoveMirrorNode(nonVirtualTreeNode, newNonVirtualTreeNode);
                   ReplaceNonVirtualNote(nonVirtualTreeNode, newNonVirtualTreeNode);
                   TreeUI:= GetTreeUI(TTreeNT(newNonVirtualTreeNode.TreeView));
                   if TreeUI <> nil then
                      TreeUI.SelectIconForNode(newNonVirtualTreeNode);
                 end;
               end;
         end;
      end;
      if (Action = maDeleting) then
         AlarmMng.RemoveAlarmsOfNode(TKntNote(nonVirtualTreeNode.Data));

  finally
  end;

end;

{$ENDREGION}


// Virtual Nodes ========================================================================

{$REGION Virtual nodes}

class function TKntTreeUI.GetCurrentVirtualNote : TKntNote;
begin
  result := ActiveNote;
  if ( result = nil ) then exit;
  if ( result.VirtualMode = vmNone ) then begin
    App.ErrorPopup(Format(STR_v17, [result.Name] ));
    result := nil;
  end;
end; // GetCurrentVirtualNote


procedure TKntTreeUI.VirtualNoteProc( VMode : TVirtualMode; myTreeNode : TTreeNTNode; VirtFN : string );
var
  Folder: TKntFolder;
  myNote : TKntNote;
  oldDlgFilter : string;
  ext : string;
  IsVNError, IsFlushingData, IsChangingFile : boolean;

begin
  Folder:= TKntFolder(Self.Folder);
  myNote := nil;

  if (myTreeNode = nil) then
     myTreeNode := GetCurrentTreeNode;
  if (assigned(myTreeNode)) then
     myNote := TKntNote(myTreeNode.Data);

  if not assigned(myNote) then exit;


  IsFlushingData := false;
  IsChangingFile := false;
  IsVNError := false;

  if ( myNote.VirtualMode <> vmNone ) then begin
    // Already a virtual node. Ask if user wants
    // to change the file with which the node is linked.
    // Do not prompt if there was an error loading the node
    // (in that case, assume the user DOES want to relink the node)

    {$IFDEF WITH_IE}
    IsChangingFile := true;
    {$ELSE}
    if myNote.HasVNodeError then begin
      IsChangingFile := true;
      IsVNError := true;
    end
    else begin
      if ( App.DoMessageBox( Format(STR_v01, [myNote.Name, myNote.VirtualFN] ),
      mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
        IsChangingFile := true;
    end;

    {$ENDIF}

    if ( not IsChangingFile ) then exit;

  end
  else begin
    // not a virtual node. If it has text, we have to have an additional prompt
    if ( Folder.Editor.Lines.Count > 0 ) then begin
      if (App.DoMessageBox(Format(STR_v02, [myNote.Name]), mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK) then
         exit;
      IsFlushingData := true; // needs a SaveDlg, not an OpenDlg
    end;

  end;

  with Form_Main do begin
      if (( ActiveFile.FileFormat = nffEncrypted ) and ( not Virtual_UnEncrypt_Warning_Done )) then begin
        if ( messagedlg(STR_v03, mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
        fVirtualUnEncryptWarningDone := true;
      end;

      if ( VirtFN = '' ) then begin

        if IsFlushingData then begin
          // use SaveDlg
          // never true for vmIELocal or vmIERemote
          oldDlgFilter := SaveDlg.Filter;
          SaveDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + FILTER_ALLFILES;
          SaveDlg.Title := STR_v04;
          SaveDlg.Filename := myNote.Name;

          try
            if ( not SaveDlg.Execute ) then exit;
          finally
            SaveDlg.Filter := oldDlgFilter;
          end;
          VirtFN := SaveDlg.FileName;
          if ( extractfileext( VirtFN ) = '' ) then
            VirtFN := VirtFN + ext_RTF;
        end
        else begin
          {$IFDEF WITH_IE}
          if ( not VirtualNodeGetMode( myNote, VMode, VirtFN )) then exit;
          {$ELSE}
          // use OpenDlg
          oldDlgFilter := OpenDlg.Filter;
          OpenDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + FILTER_ALLFILES;
          OpenDlg.Title := STR_v04;
          if IsVNError then
            OpenDlg.Filename := copy( myNote.VirtualFN, 2, length( myNote.VirtualFN ))
          else
            OpenDlg.Filename := myNote.VirtualFN;

          try
            if ( not OpenDlg.Execute ) then exit;
          finally
            OpenDlg.Filter := oldDlgFilter;
          end;
          VirtFN := OpenDlg.FileName;
          {$ENDIF}
        end; // if IsFlushingData
      end; // if ( VirtFN = '' );

      if ( VMode <> vmIERemote ) then begin // do not smash case in URLs
        VirtFN := normalFN( VirtFN );

        if directoryexists( VirtFN ) then begin
          // not a file, but a directory - cannot import
          // (user could have drag-dropped a directory, so we must check)
          exit;
        end;

        // these following tests do not apply to IERemote nodes, either
        ext := ExtractFileExt( VirtFN );
        if ( not ( ExtIsRTF( ext ) or ExtIsText( ext ) or ExtIsHTML( ext ))) then begin
          messagedlg( STR_v05, mtError, [mbOK], 0 );
          exit;
        end;

        // It is not reccommended to link files on virtual media (floppies,
        // CD-ROMs, ZIP drives, etc. So we check.
        if IsDriveRemovable( VirtFN ) then begin
          case TreeOptions.RemovableMediaVNodes of
            _REMOVABLE_MEDIA_VNODES_DENY : begin
              MessageDlg( Format(STR_v06,[Extractfiledrive( VirtFN )] ), mtError, [mbOK], 0 );
              exit;
            end;
            _REMOVABLE_MEDIA_VNODES_WARN : begin
              if ( messagedlg( Format(STR_v07,
                [Extractfiledrive( VirtFN )] ), mtWarning, [mbOK,mbCancel], 0 ) <> mrOK ) then
                  exit;
            end;
            { _REMOVABLE_MEDIA_VNODES_ALLOW or any other value: allow }
          end;
        end;


        // any given file can be linked to a virtual node only once
        // per KNT file. So we must check if the selected file already
        // exists as a virtual node in the currently open KNT file.
        if ActiveFile.HasVirtualNoteByFileName( myNote, VirtFN ) then begin
          App.ErrorPopup(STR_v08);
          exit;
        end;

      end;


      Folder.Editor.BeginUpdate;
      try
        try

          if ( IsChangingFile and ( not ( myNote.VirtualMode in [vmIELocal, vmIERemote] ))) then begin
            // Node must save its existing data first:
            if ( not IsVNError ) then begin
              Folder.EditorToDataStream;
              myNote.SaveVirtualFile;
            end;
            // now clear the editor
            Folder.Editor.Clear;
            Folder.Editor.ClearUndo;
          end;

          {$IFDEF WITH_IE}
          myNote.VirtualMode := VMode;
          myNote.VirtualFN := VirtFN;
          {$ELSE}
          if ( myNote.VirtualMode in [vmNone, vmText, vmRTF, vmHTML] ) then
            myNote.VirtualMode := VMode; // so that setting new filename will adjust the vm type
          myNote.VirtualFN := VirtFN;
          {$ENDIF}

          // myNote.Stream.LoadFromFile( myNote.VirtualFN );
          if IsFlushingData then begin
            // never true for vmIELocal or vmIERemote
            Folder.EditorToDataStream;
            myNote.SaveVirtualFile;
          end
          else begin
            myNote.LoadVirtualFile;
            Folder.DataStreamToEditor;
          end;
          myTreeNode := GetCurrentTreeNode;
          SelectIconForNode(myTreeNode);

          if ( TreeOptions.AutoNameVNodes and ( not IsFlushingData )) then begin
            myNote.Name := ExtractFilename( myNote.VirtualFN ); // {N}
            (* [x] ImportFileNamesWithExt ignored for virtual nodes, because it is useful to have extension visible
            if KeyOptions.ImportFileNamesWithExt then
              myNote.Name := ExtractFilename( myNote.VirtualFN ) // {N}
            else
              myNote.Name := ExtractFilenameNoExt( myNote.VirtualFN );
            *)
            myTreeNode.Text := myNote.Name;
          end;

        except
          on E : Exception do begin
            myNote.VirtualFN := '';
            App.ErrorPopup(E, STR_v09);
          end;
        end;

      finally
        Folder.Modified := true;
        Folder.Editor.Modified := false;
        Folder.Editor.EndUpdate;

        Folder.ConfigureEditor;
        App.FolderPropertiesModified(Folder);
        App.EditorPropertiesModified(Folder.Editor);
      end;
  end;
end; // VirtualNoteProc


procedure TKntTreeUI.VirtualNoteUnlink;
var
  Folder: TKntFolder;
  myNote : TKntNote;
  myTreeNode, originalTreeNode : TTreeNTNode;
  Changed: boolean;
begin
  Folder:= TKntFolder(Self.Folder);
  myNote := GetCurrentVirtualNote;
  if ( not assigned( myNote )) then exit;
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;

  // cannot unlink vmIERemote virtual nodes,
  // because there's no local file

  if ( myNote.VirtualMode in [vmIELocal, vmIERemote] ) then
  begin
    App.DoMessageBox( Format(STR_v10, [myNote.Name] ), mtError, [mbOK], 0 );
    exit;
  end;

  Changed:= false;

  if (myNote.VirtualMode= vmKNTNode) then begin
       originalTreeNode:= myNote.MirrorNode;
       if assigned(originalTreeNode) and assigned(TKntNote(originalTreeNode.Data)) then
          if ( App.DoMessageBox( Format(STR_v18, [myNote.Name, myNote.VirtualFN] ),
            mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
          begin
              RemoveMirrorNode(originalTreeNode, myTreeNode);
              myNote.MirrorNode:= nil;
              TKntNote(originalTreeNode.Data).Stream.SaveToStream(myNote.Stream);
              Folder.Modified := true;             // => KntFile.Modified <- True
              SelectIconForNode(myTreeNode);
              Changed:= true;
          end;

  end
  else
      if ( App.DoMessageBox( Format(STR_v11, [myNote.Name, myNote.VirtualFN] ),
        mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
      begin
          myNote.VirtualMode := vmNone;
          myNote.VirtualFN := '';
          Folder.Modified := true;
          SelectIconForNode(myTreeNode);
          Changed:= true;
      end;

  if Changed then begin
     Folder.ConfigureEditor;
     App.FolderPropertiesModified(Folder);
     App.EditorPropertiesModified(Folder.Editor);
  end;

end; // VirtualNoteUnlink


procedure TKntTreeUI.VirtualNoteRefresh( const DoPrompt : boolean );
var
  myNote : TKntNote;
  Editor: TKntRichEdit;
  Folder: TKntFolder;

begin
  myNote := GetCurrentVirtualNote;
  if ( not assigned( myNote )) then exit;

  Folder:= TKntFolder(Self.Folder);

  if myNote.RTFModified then begin
    if ( App.DoMessageBox(Format(STR_v12, [myNote.Name, ExtractFilename( myNote.VirtualFN )] ),
                           mtWarning, [mbOK,mbCancel], 0 ) <> mrOK ) then
    exit;
  end
  else
  if DoPrompt then begin
    if (App.DoMessageBox( Format(STR_v13, [myNote.Name, ExtractFilename( myNote.VirtualFN )] ),
                          mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then
    exit;
  end;

   Editor:= Folder.Editor;
   Editor.BeginUpdate;
   try
     try
       myNote.LoadVirtualFile;
     except
       on E : Exception do begin
         App.ErrorPopup(E, STR_v14);
         exit;
       end;
     end;

     try
       Editor.Clear;
       Editor.ClearUndo;
       Folder.DataStreamToEditor;
       App.ShowInfoInStatusBar(STR_v15);
     except
       App.ShowInfoInStatusBar(STR_v16);
     end;

   finally
     Folder.Editor.EndUpdate;
     Folder.Modified := true;
   end;

end; // VirtualNoteRefresh


{$IFDEF WITH_IE}
function VirtualNoteGetMode( const aNote : TKntNote; var newMode : TVirtualMode; var newFN : string ) : boolean;
var
  Form_VNode : TForm_VNode;
begin
  result := false;
  if ( not assigned( aNote )) then exit;
  Form_VNode := TForm_VNode.Create( self );
  try
    Form_VNode.myVirtualMode := aNote.VirtualMode;
    Form_VNode.myVirtualFN := aNote.VirtualFN;
    Form_VNode.myNodeName := aNote.Name;
    if ( Form_VNode.ShowModal = mrOK ) then begin
      newMode := Form_VNode.myVirtualMode;
      newFN := Form_VNode.myVirtualFN;
      result := ( newFN <> '' );
    end;

  finally
    Form_VNode.Free;
  end;
end; // VirtualNoteGetMode
{$ENDIF}


{$ENDREGION}


initialization

end.

