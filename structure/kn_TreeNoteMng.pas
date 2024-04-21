unit kn_TreeNoteMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Contnrs,
   System.Classes,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Controls,
   Vcl.Graphics,
   Vcl.Clipbrd,
   TreeNT,
   RxRichEd,
   kn_Info,
   kn_Const,
   kn_KntFolder,
   kn_KntNote
   ;


var
    TransferNodes : TKntNoteList; // for data transfer (copy tree nodes between tabs)
    DraggedTreeNode : TTreeNTNode;
    MirrorNodes: TBucketList;

    _LAST_NODE_SELECTED : TTreeNTNode;
    _OLD_NOTE_NAME : string;

    // treenote-related methods:
    function AddNodeToTree( aInsMode : TNodeInsertMode ) : TTreeNTNode;
    function TreeNewNode( const aFolder : TKntFolder; aInsMode : TNodeInsertMode; const aOriginNode : TTreeNTNode; const aNewNodeName : string; const aDefaultNode : boolean ) : TTreeNTNode;
    procedure TreeNodeSelected( Node : TTreeNTNode; OnDeletingNode: boolean= false );
    procedure DeleteTreeNode( const DeleteFocusedNode : boolean );
    function MoveSubtree( myTreeNode : TTreeNTNode ): boolean;
    procedure UpdateTreeNode( const aTreeNode : TTreeNTNode; Folder: TKntFolder );
    procedure CreateMasterNode;
    procedure OutlineNumberNodes;
    procedure CreateNodefromSelection;
    function GetCurrentNote : TKntNote;
    function GetCurrentTreeNode : TTreeNTNode;
    procedure SelectIconForNode( const myTreeNode : TTreeNTNode; const IconKind : TNodeIconKind );
    function GetNodeFontFace (TreeNode: TTreeNTNode): string;
    procedure UpdateTreeChrome( const myFolder : TKntFolder );
    procedure UpdateTreeOptions( const myFolder : TKntFolder );
    procedure MoveTreeNode( MovingNode : TTreeNTNode; const aDir : TDirection );
    procedure PasteNodeName( const PasteMode : TPasteNodeNameMode );
    procedure CopyNodeName( const IncludeNoteText : boolean );
    procedure CopyNodePath( const InsertInEditor : boolean );
    procedure ShowOrHideIcons( const myFolder : TKntFolder; const UpdateNodes : boolean );
    procedure ShowOrHideCheckBoxes( const myFolder : TKntFolder );
    procedure SetTreeNodeColor( const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean );
    procedure SetTreeNodeBold( const DoChildren : boolean );
    procedure SetTreeNodeCustomImage;
    procedure SetTreeNodeFontFace( const ResetDefault, DoChildren : boolean );
    procedure SetTreeNodeFontSize( const ResetDefault, DoChildren : boolean );
    procedure NavigateInTree( NavDirection : TNavDirection );
    function GetNodePath( aNode : TTreeNTNode; const aDelimiter : string; const TopToBottom : boolean ) : string;

    procedure ConvertStreamContent(Stream: TMemoryStream; FromFormat, ToFormat: TRichStreamFormat; RTFAux : TRxRichEdit);
    function TreeTransferProc( const XferAction : integer; const PasteTargetFolder : TKntFolder; const Prompt : boolean ; const PasteAsVirtualKNTNode: boolean; const MovingSubtree: boolean) : boolean;

    procedure HideChildNodesUponCheckState (folder: TKntFolder; ParentNode: TTreeNTNode; CheckState: TCheckState);    // [dpv]
    procedure ShowCheckedNodes (folder: TKntFolder; ParentNode: TTreeNTNode);    // [dpv]
    procedure ShowOrHideChildrenCheckBoxes( const tnode : TTreeNTNode );   // [dpv]

    function IsAnyNodeMoving: boolean;              // [dpv]
    procedure MarkAllFiltered (folder: TKntFolder);    // [dpv]
    procedure MarkAllUnfiltered (folder: TKntFolder);  // [dpv]
    procedure RemoveFilter (folder: TKntFolder);       // [dpv]
    procedure HideFilteredNodes (folder: TKntFolder);  // [dpv]
    function GetTreeNode (FolderID: integer; NodeID: integer): TTreeNTNode;
    procedure ChangeCheckedState(TV: TTreeNT; Node: TTreeNTNode; Checked: Boolean; CalledFromMirrorNode: Boolean);

    function GetMirrorNodes(originalNode: TTreeNTNode): Pointer;
    procedure AddMirrorNode(MainNode: TTreeNTNode; Mirror_Node: TTreeNTNode);
    procedure RemoveMirrorNode(MainNode: TTreeNTNode; mirror_Node: TTreeNTNode);
    procedure ReplaceNonVirtualNote(MainNode: TTreeNTNode; newNode: TTreeNTNode);

implementation
uses
   gf_strings,
   gf_streams,
   gf_miscvcl,
   gf_misc,
   kn_Global,
   kn_EditorUtils,
   knt.ui.editor,
   kn_Chest,
   kn_clipUtils,
   kn_ImagePicker,
   kn_NodeNum,
   kn_Macro,
   kn_Main,
   kn_LinksMng,
   kn_VirtualNodeMng,
   kn_MacroMng,
   kn_NoteFileMng,
   Knt.App
   ;


resourcestring
  STR_01 = 'Error creating node: ';
  STR_02 = ' Virtual: ';
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

var
   __NodeChangeCounter : longint;
   FNodeMoving: boolean;             // [dpv]

function IsAnyNodeMoving: boolean;   // [dpv]
begin
  result:= FNodeMoving;
end;

function AddNodeToTree( aInsMode : TNodeInsertMode ) : TTreeNTNode;
begin
  result := TreeNewNode( nil, aInsMode, nil, '', false );
  if ( KeyOptions.RunAutoMacros and assigned( result )) then
     ExecuteMacro( _MACRO_AUTORUN_NEW_NODE, '' );
end; // AddNodeToTree

function TreeNewNode(
  const aFolder : TKntFolder;
  aInsMode : TNodeInsertMode;
  const aOriginNode : TTreeNTNode;
  const aNewNodeName : string;
  const aDefaultNode : boolean ) : TTreeNTNode;
var
  myNote, myParentNote : TKntNote;
  myTreeNode, myOriginNode, mySiblingNode : TTreeNTNode;
  myFolder : TKntFolder;
  myName : string;
  p : integer;
  AddingFirstNode, addnumber : boolean;
begin
  result := nil;
  if (aFolder = nil) then begin
    if not assigned(ActiveFolder) then exit;
    myFolder := ActiveFolder;
  end
  else
    myFolder := aFolder;

  if Form_Main.FolderIsReadOnly(myFolder, true) then exit;

  myTreeNode := nil; { just to avoid }
  myNote := nil;     { compiler warning }

  addnumber := myFolder.AutoNumberNodes;
  if ( aNewNodeName <> '' ) then
    myName := aNewNodeName

  else begin
    myName := myFolder.DefaultNoteName;

    // check for special tokens
    p := pos( NODEINSDATE, myName );
    if ( p > 0 ) then begin
      delete( myName, p, length( NODEINSDATE ));
      insert( FormatDateTime( KeyOptions.DateFmt, now ), myName, p );
      addnumber := false;
    end;

    p := pos( NODEINSTIME, myName );
    if ( p > 0 ) then begin
      delete( myName, p, length( NODEINSTIME ));
      insert( FormatDateTime( KeyOptions.TimeFmt, now ), myName, p );
      addnumber := false;
    end;

    p := pos( NODECOUNT, myName );
    if ( p > 0 ) then begin
      delete( myName, p, length( NODECOUNT ));
      insert( inttostr( succ( myFolder.TV.Items.Count )), myName, p );
      addnumber := false;
    end;

    if addnumber then
      myName := myFolder.DefaultNoteName + #32 + inttostr( succ( myFolder.TV.Items.Count ));

  end;

  if ( myFolder.TV.Items.Count = 0 ) or ( not assigned( myFolder.TV.Selected )) then begin
    AddingFirstNode := true;
    aInsMode := tnTop; // no children or siblings if no nodes
  end
  else
    AddingFirstNode := false;

  try
    try

      // myFolder.TV.OnChange := nil;
      if ( aOriginNode = nil ) then
         myOriginNode := myFolder.TV.Selected
      else
         myOriginNode := aOriginNode;

      case aInsMode of
         tnTop : begin
           myTreeNode := myFolder.TV.Items.AddFirst( nil, myName );
         end;
         tnInsertBefore : begin
           myTreeNode := myFolder.TV.Items.Insert( myOriginNode, myName );
        end;
        tnAddLast : begin
          myTreeNode := myFolder.TV.Items.Add( myOriginNode, myName );
        end;
        tnAddChild : begin
          myTreeNode := myFolder.TV.Items.AddChild( myOriginNode, myName );
        end;
        tnAddAfter : begin
         mySiblingNode := myOriginNode.GetNextSibling;
         if assigned( mySiblingNode ) then
           myTreeNode := myFolder.TV.Items.Insert( mySiblingNode, myName )
         else
           myTreeNode := myFolder.TV.Items.Add( myOriginNode, myName );
        end;
      end;

      result := myTreeNode;

      // these tokens can be expanded only after the node was created
      if ( aNewNodeName = '' ) then begin
         if ( pos( '%', myName ) > 0 ) then begin
           p := pos( NODELEVEL, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODELEVEL ));
             insert( inttostr( myTreeNode.Level ), myName, p );
           end;

           p := pos( NODEINDEX, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODEINDEX ));
             insert( inttostr( succ( myTreeNode.Index )), myName, p );
           end;

           p := pos( NODEABSINDEX, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODEABSINDEX ));
             insert( inttostr( succ( myTreeNode.AbsoluteIndex )), myName, p );
           end;

           p := pos( NODEPARENT, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODEPARENT ));
             if assigned( myTreeNode.Parent ) then
               insert( myTreeNode.Parent.Text, myName, p )
             else
               insert( '<NONE>', myName, p );
           end;

           p := pos( NODENOTENAME, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODENOTENAME ));
             insert( RemoveAccelChar( myFolder.Name ), myName, p );
           end;

           p := pos( NODEFILENAME, myName );
           if ( p > 0 ) then begin
             delete( myName, p, length( NODEFILENAME ));
             insert( ExtractFilename( ActiveFile.FileName ), myName, p );
           end;

         end;
      end;

      if assigned( myOriginNode ) then
        myParentNote := TKntNote( myOriginNode.Data )
      else
        myParentNote := nil;

      myNote := myFolder.NewNote( myParentNote, myName, TreeOptions.InheritNodeProperties );

      myTreeNode.Data := myNote;

      if AddingFirstNode then begin
        ShowOrHideIcons( myFolder, true );
        ShowOrHideCheckBoxes( myFolder );
      end;

      SelectIconForNode( myTreeNode.Parent, myFolder.IconKind );
      SelectIconForNode( myTreeNode, myFolder.IconKind );

      UpdateTreeNode( myTreeNode, myFolder );

      // assign the color of the currently displayed node
      if TreeOptions.InheritNodeBG then
        myNote.RTFBGColor := ActiveFolder.Editor.Color;


      myFolder.SelectedNote := myNote;
      myFolder.DataStreamToEditor;

      myTreeNode.MakeVisible;
      myFolder.TV.Selected := myTreeNode;

      if _LastZoomValue <> 100 then
         myFolder.Editor.SetZoom(_LastZoomValue, '' );


    except
      on E : Exception do begin
        messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
        // if assigned( myTreeNode ) then myTreeNode.Free;
        // if assigned( myNote ) then myNote.Free;
      end;
    end;

  finally
    myFolder.SelectedNote := myNote;
    myFolder.TV.OnChange := Form_Main.TVChange;
    VirtualNoteUpdateMenu( false, false );
    KntFile.Modified := true;
    UpdateKntFileState( [fscModified] );
  end;

  if (( not aDefaultNode ) and assigned( myTreeNode ) and TreeOptions.EditNewNodes ) then
     Form_Main.MMRenamenodeClick( nil );

end; // TreeNewNode


function GetCurrentNote : TKntNote;                                          // It should be equal to ActiveNote
begin
  result := nil;
  if not assigned(ActiveFolder) then exit;
  if assigned( ActiveFolder.TV.Selected ) then
    result := TKntNote(ActiveFolder.TV.Selected.Data);
end; // GetCurrentNote


function GetCurrentTreeNode : TTreeNTNode;
begin
  result := nil;
  if not assigned(ActiveFolder) then exit;
  result := ActiveFolder.TV.Selected;
end; // GetCurrentTreeNode


procedure TreeNodeSelected( Node : TTreeNTNode; OnDeletingNode: boolean= false );
var
  myFolder : TKntFolder;
  myNote : TKntNote;
  Editor: TKntRichEdit;
  KeepModified: boolean;
  KeepEditorFocused: boolean;
  OnEnterBak: TNotifyEvent;
  ControlWasFocused: TWinControl;

  {$IFDEF WITH_IE}
  NodeControl : TNodeControl;
  {$ENDIF}
begin
  with Form_Main do begin
      if ( not assigned( Node )) then exit;

      myFolder := ActiveFolder;
      if not assigned(myFolder) then exit;

      if (not Initializing) and (Node.TreeView <> myFolder.TV) then exit;

      myNote := TKntNote( Node.Data );
      Editor:= myFolder.Editor;

      KeepModified:= false;
      KeepEditorFocused:= false;

      if assigned(myFolder.SelectedNote) then
         myFolder.SelectedNote.ScrollPosInEditor:= Editor.GetScrollPosInEditor;

      if ( not _Executing_History_Jump ) and (not _Executing_JumpToKNTLocation_ToOtherNote) then begin
          AddHistoryLocation( myFolder, false);        // Add to history the location of current node, before the new node comes to be the selected node
         _LastMoveWasHistory := false;
         UpdateHistoryCommands;
      end;

      myFolder.TV.OnChange := nil;
      Editor.BeginUpdate;         // -> It will also ignore Enter and Change events

      ControlWasFocused:= nil;
      if KeyOptions.FixScrollBars and not ClipCapMng.IsBusy then begin
         if (not Editor.Focused) and Editor.CanFocus then begin
             ControlWasFocused:= Form_Main.ActiveControl;
             Editor.SetFocus;
         end;
      end;

      try
        try

          if not OnDeletingNode then
             myFolder.EditorToDataStream;
          Editor.Clear;
          Editor.ClearUndo;

          myFolder.SelectedNote := myNote;

          if assigned(myNote) then begin

             case myNote.WordWrap of
               wwAsFolder : myFolder.Editor.WordWrap := myFolder.WordWrap;
               wwYes : myFolder.Editor.WordWrap := true;
               wwno : myFolder.Editor.WordWrap := false;
             end;

             myFolder.DataStreamToEditor;

             { The normal thing is to set Editor.Modified = False at the end of the DataStreamToEditor method
               But if hidden marks to be eliminated have been identified (and corrected), it will have been kept as Modified,
               to ensure that this correction ends up persisting. Here we will do the same }
             if Editor.Modified then
                KeepModified:= True;

             if ( myNote.VirtualMode = vmNone ) then begin
                VirtualNoteUpdateMenu( false, false );
                if ( not EditorOptions.TrackStyle ) then begin
                  if TreeOptions.ShowFullPath then
                    StatusBar.Panels[PANEL_HINT].Text := GetNodePath( Node, TreeOptions.NodeDelimiter, TreeOptions.PathTopToBottom ) // {N}
                  else
                    StatusBar.Panels[PANEL_HINT].Text := Node.Text; // {N}
                end;
             end
             else begin
               VirtualNoteUpdateMenu( true, myNote.VirtualMode = vmKNTNode );
               if ( not EditorOptions.TrackStyle ) then
                 StatusBar.Panels[PANEL_HINT].Text := STR_02 + myNote.VirtualFN;
             end;
             TVCheckNode.Checked := myNote.Checked;
             TVBoldNode.Checked :=  myNote.Bold;
             TVChildrenCheckbox.Checked:= myNote.ChildrenCheckbox;   // [dpv]
             if myFolder.Checkboxes or (assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox)) then  // [dpv]
                TVCheckNode.Enabled := true
             else
                TVCheckNode.Enabled := false;

             ShowAlarmStatus;
             UpdateShowImagesState;
          end
          else begin
            VirtualNoteUpdateMenu( false, false );
            if ( not EditorOptions.TrackStyle ) then
               StatusBar.Panels[PANEL_HINT].Text := '';
          end;

        except
          On E : Exception do begin
            myFolder.SelectedNote := nil;
            messagedlg( E.Message, mtError, [mbOK], 0 );
            exit;
          end;
        end;

      finally

        if _LastZoomValue <> 100 then
           Editor.SetZoom(_LastZoomValue, '' );

        if assigned(myNote) and (myNote.ScrollPosInEditor.Y > 0) then
           Editor.SetScrollPosInEditor(myNote.ScrollPosInEditor)
        else
           if KeyOptions.FixScrollBars then
              Editor.Perform( EM_SCROLLCARET, 0, 0 );

        if KeyOptions.FixScrollBars and (ControlWasFocused <> nil) then
           ControlWasFocused.SetFocus;

        Editor.EndUpdate;

        Editor.CheckWordCount(true);

        if not KeepModified then
           Editor.Modified := false;
        Editor.ChangedSelection;
        Editor.Change;
        myFolder.TV.OnChange := TVChange;
        // inc( __NodeChangeCounter );
        // statusbar.panels[0].text := Format( ' %d', [__NodeChangeCounter ]);

      end;
  end;

end; // TreeNodeSelected


procedure CreateMasterNode;
var
  myNote, nextnode, masternode : TTreeNTNode;
  myFolder : TKntFolder;
begin
  if ( not assigned( ActiveFolder)) then exit;
  if Form_Main.FolderIsReadOnly( ActiveFolder, true ) then exit;

  myFolder := ActiveFolder;

  if ( myFolder.TV.Items.Count > 0 ) then
    myFolder.TV.Selected := myFolder.TV.Items.GetFirstNode;

  masternode := TreeNewNode(myFolder, tnInsertBefore, nil, '', true );

  if assigned( masternode ) then begin
     myFolder.TV.Items.BeginUpdate;
     try
       myNote := masternode.GetNext;
       while assigned( myNote ) do begin
         nextnode := myNote.GetNextSibling;
         myNote.MoveTo( masternode, naAddChild );
         SelectIconForNode( myNote, myFolder.IconKind );
         myNote := nextnode;
       end;
     finally
       SelectIconForNode( masternode, myFolder.IconKind );
       myFolder.TV.Items.EndUpdate;
       myFolder.TV.Selected := masternode;
       myFolder.TV.SetFocus;
     end;

  end;

end; // CreateMasterNode


procedure OutlineNumberNodes;
type
  TExistingNumbers = ( enNo, enYes, enAuto );
var
  Form_NodeNum : TForm_NodeNum;
  StartNode, myTreeNode, ParentNode : TTreeNTNode;
  SubtreeOnly : boolean;
  StripNames : boolean;
  ExistingNumbers : TExistingNumbers;
  myFolder : TKntFolder;
  myNote : TKntNote;
  StartNodeLevel, LastNodeLevel, thisNodeLevel : integer;
  StartNumber, thisNumber : integer;
  ParentLevelStr, LevelStr : string;
  DepthLimit : integer;
  ModalResponse : Word;

     function ExtractNodeNumber( const aNode : TTreeNTNode ) : string;
     var
       p : integer;
     begin
       result := '';
       if assigned( aNode ) then begin
          if StripNames then
            result := aNode.Text
          else begin
            p := pos( #32, aNode.Text );
            if ( p > 1 ) then
              result := copy( aNode.Text, 1, pred( p ));
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
             SpacePos := AnsiPos( #32, myNote.Name );
             if ( SpacePos > 0 ) then begin
               tmpstr := myNote.Name;
               delete( tmpstr, 1, SpacePos );
               myNote.Name := LevelStr + #32 + tmpstr;
             end
             else
               myNote.Name := LevelStr;
           end;
           enAuto : begin
             // check if node name begins with a number
             // and if so, strip it
             SpacePos := -1; // flag: node has NO number
             for i := 1 to length( myNote.Name ) do begin
                if ( i = 1 ) then begin
                  if ( AnsiChar(myNote.Name[1]) in ['0'..'9'] ) then
                    SpacePos := 0 // flag: node HAS number
                  else
                    break; // node does NOT begin with a number
                end
                else begin
                  if ( AnsiChar(myNote.Name[i]) in ['0'..'9', '.'] ) then
                    continue
                  else
                  if ( myNote.Name[i] = #32 ) then begin
                    SpacePos := i;
                    break;
                  end
                  else begin
                    SpacePos := pred( i );
                    break;
                  end;
                end;
             end;

             if ( SpacePos < 0 ) then begin
               // node name does not have a number
               if ( ModalResponse = mrOK ) then
                 myNote.Name := LevelStr + #32 + myNote.Name;
             end
             else
             if ( SpacePos = 0 ) then begin
               // whole node name is a number
               if ( ModalResponse = mrOK ) then
                 myNote.Name := LevelStr;
             end
             else begin
               // node has a number followed by text
               tmpstr := myNote.Name;
               delete( tmpstr, 1, SpacePos );
               if ( ModalResponse = mrOK ) then
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
  if ( not assigned( ActiveFolder )) then exit;
  if Form_Main.FolderIsReadOnly( ActiveFolder, true ) then exit;

  myFolder := ActiveFolder;

  if ( myFolder.TV.Items.Count = 0 ) then exit;

  Form_NodeNum := TForm_NodeNum.Create( Form_Main );
  try
    ModalResponse := Form_NodeNum.ShowModal;
    if ( ModalResponse in [mrOK, mrYesToAll] ) then begin

      if ( ModalResponse = mrYesToAll ) then begin
        if ( messagedlg( STR_03, mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then
          exit;
      end;

      with Form_NodeNum do begin
        SubtreeOnly := ( RG_Scope.ItemIndex > 0 );
        if SubtreeOnly then
          StartNode := myFolder.TV.Selected
        else
          StartNode := myFolder.TV.Items.GetFirstNode;
        StripNames := ( RG_Method.ItemIndex > 0 );
        ExistingNumbers := TExistingNumbers( RG_CurNum.ItemIndex );
        StartNumber := Spin_StartNum.Value;
        if CB_FullDepth.Checked then
          DepthLimit := 0 // add numbers to all levels
        else
          DepthLimit := Spin_Depth.Value; // descend only DepthLimit levels
      end;

      if ( not assigned( StartNode )) then begin
        messagedlg( STR_04, mtError, [mbOK], 0 );
        exit;
      end;

      myFolder.TV.Items.BeginUpdate;
      try

        try

          myTreeNode := StartNode;
          StartNodeLevel := StartNode.Level;
          LastNodeLevel := StartNodeLevel;
          ThisNumber := StartNumber;
          LevelStr := '';

          if ( ModalResponse = mrYesToAll ) then begin
            StripNames := false;
            SubtreeOnly := false;
            ExistingNumbers := enAuto;
            while assigned( myTreeNode ) do begin
              myNote := TKntNote( myTreeNode.Data );
              AddNumberToNode;
              myTreeNode := myTreeNode.GetNext;
            end;
            exit;
          end;


          // first process starting level nodes,
          // because they need different treatment
          // (numbering starts with StartNumber and is not based on .index property)

          while assigned( myTreeNode ) do begin
            myNote := TKntNote( myTreeNode.Data );
            LevelStr := inttostr( ThisNumber );
            inc( ThisNumber );
            AddNumberToNode;

            if SubtreeOnly then
              break; // do not process sibling nodes, we only descend the subtree
            myTreeNode := myTreeNode.GetNextSibling;
          end;

          myTreeNode := StartNode;

          if ( DepthLimit <> 1 ) then begin // only if applying numbers more than 1 level deep

            StartNodeLevel := StartNode.Level; // return to original starting position
            // from now on, we only need to number nodes which are below the
            // initial numbering level and up to the max numbering level

            ParentLevelStr := '';
            LevelStr := '';
            ParentNode := nil;

            while assigned( myTreeNode ) do begin
              thisNodeLevel := myTreeNode.Level;
              myNote := TKntNote( myTreeNode.Data );

              if (( DepthLimit = 0 ) or ( thisNodeLevel < ( StartNodeLevel + DepthLimit ))) then begin
                if ( thisNodeLevel > StartNodeLevel ) then begin
                  if ( ParentNode <> myTreeNode.Parent ) then
                    ParentLevelStr := ExtractNodeNumber( myTreeNode.Parent );
                  ParentNode := myTreeNode.Parent;

                  LevelStr := Format(
                    '%s.%d',
                    [ParentLevelStr, succ( myTreeNode.Index )]
                  );
                  AddNumberToNode;
                end;
              end;

              // get next node, or bail out
              LastNodeLevel := myTreeNode.Level;
              myTreeNode := myTreeNode.GetNext;

              if SubtreeOnly then begin
                // bail out, we have finished the subtree
                if ( assigned( myTreeNode ) and ( myTreeNode.Level = StartNodeLevel )) then
                  break;
              end;
            end;

          end;

        except
          on E : Exception do
            messagedlg( e.message, mtError, [mbOK], 0 );
        end;

      finally
        myFolder.TV.Items.EndUpdate;
        KntFile.Modified := true;
        UpdateKntFileState( [fscModified] );
      end;
    end;

  finally
    Form_NodeNum.Free;
  end;

end; // OutlineNumberNodes


function GetNodeFontFace (TreeNode: TTreeNTNode): string;
var
  Note: TKntNote;
  Folder: TKntFolder;
begin
   Result:= '';
   if not assigned(TreeNode) then exit;

   Note:= TKntNote(TreeNode.Data);
   if not assigned(Note) then exit;

   if Note.HasNodeFontFace then
      Result:= Note.NodeFontFace
   else begin
      Folder:= ActiveFile.GetFolderByTreeNode(TreeNode);
      if assigned(Folder) then
        Result:= Folder.TreeChrome.Font.Name;
   end;

end;


procedure UpdateTreeChrome( const myFolder : TKntFolder );
var
  myTreeNode, selectedTreeNode: TTreeNTNode;
  myNote: TKntNote;
  FOrig: TFont;
  FDest: TFontInfo;
  FontChange: boolean;
  BGColorChange: boolean;
  Styles: TFontStyles;

begin
   { See comments in the methods TCustomTreeNT.RecreateTreeWindow and TCustomTreeNT.DestroyWnd  (TreeNT.pas)

    The indicated explains why font changes in the tree (from 'Folder Properties...') were not reflected on it
    on many occasions, precisely those in which we had not modified the background color.
    And besides, it was also happening that the font changes, when they were shown, did not give rise to a correct
    resizing of the height of each item.
     * These problems were of tree refresh. Saving and reopening the file would show up correctly.
   }

   FOrig:= myFolder.TV.Font;
   FDest:= myFolder.TreeChrome.Font;

   FontChange:=  (FOrig.Color <> FDest.Color) or (FOrig.Size <> FDest.Size) or
                 (FOrig.Name <> FDest.Name) or (FOrig.Style <> FDest.Style) or
                 (FOrig.Charset <> FDest.Charset);

   BGColorChange:= (myFolder.TV.Color <> myFolder.TreeChrome.BGColor);

   if not FontChange and not BGColorChange then
      Exit;


   if FontChange then begin
      Log_StoreTick('');
      Log_StoreTick( 'UpdateTreeChrome - Begin changes', 2, +1);

      FontInfoToFont( myFolder.TreeChrome.Font, myFolder.TV.Font );   // This doesn't force any update (it doesn't end up calling RecreateWnd)

      { The above line does not affect nodes with any changes from the default values, we must explicitly update their size, style and font color.
        The background color of the node and the its font.name do not need to be updated.
        Also take into account: if the font of the tree includes the bold style, and we remove it from a node, it will not indicate Bold=True,
        but for the tree we have explicitly modified it, so we need to also look at the property ParentFont of TTreeNTNode
      }

      Log_StoreTick( 'After FontInfoToFont', 2);

       myTreeNode := myFolder.TV.Items.GetFirstNode;
       while assigned( myTreeNode ) do begin
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

   Log_StoreTick( 'After modified individual nodes', 2);

   selectedTreeNode:= myFolder.TV.Selected;
   myFolder.TV.OnChange := nil;
   //myFolder.TV.Items.BeginUpdate;
   try
      myFolder.TV.Color := myFolder.TreeChrome.BGColor;   // It will do nothing if BGColorChange = False
      Log_StoreTick( 'After changed TV.Color', 2);
      if not BGColorChange and FontChange then begin
         myFolder.TV.RecreateTreeWindow;
         Log_StoreTick( 'After RecreateTreeWindow', 2);
      end;

   finally
      //myFolder.TV.Items.EndUpdate;
      myFolder.TV.Selected:= selectedTreeNode;
      myFolder.TV.OnChange := Form_Main.TVChange;
   end;

   Log_StoreTick( 'UpdateTreeChrome - End changes', 2, -1);
   Log_Flush();

end; // UpdateTreeChrome

procedure UpdateTreeOptions( const myFolder : TKntFolder );
begin
  // updates options for current folder's tree
  // based on global tree options

  with myFolder.TV do
  begin
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
      if TreeOptions.ShowTooltips then
      begin
        Options := Options + [toInfoTip];
        Options := Options + [toToolTips];
      end
      else
      begin
        Options := Options - [toInfoTip];
        Options := Options - [toToolTips];
      end;
  end;

end; // UpdateTreeOptions


procedure MoveTreeNode( MovingNode : TTreeNTNode; const aDir : TDirection );
var
  s, t : string;
  PreviousParent, theSibling : TTreeNTNode;
  myFolder : TKntFolder;
begin
  if Form_Main.FolderIsReadOnly(ActiveFolder, true) then exit;
  if ( ActiveFolder.FocusMemory <> focTree ) then exit;

  if not assigned( MovingNode) then // try to get tree's selected node...
     MovingNode := GetCurrentTreeNode;
  if not assigned( MovingNode) then // still nothing, so bail out
     exit;

  s := '';
  t := STR_05;
  myFolder := ActiveFolder;
  myFolder.TV.OnChange := nil;
  myFolder.TV.OnChecked:= nil;

  FNodeMoving:= true;   // [dpv]
  try
    try
      PreviousParent := MovingNode.Parent;

      case aDir of
        // UP / DOWN move node only within its siblings
        // (cannot change level)
        dirUp : begin
          //theSibling := MovingNode.GetPrevSibling; [dpv]
          theSibling := MovingNode.GetPrevSiblingNotHidden;
          if assigned( theSibling ) then begin
            //theSibling := theSibling.GetPrevSibling; // looks weird but does the Right Thing
            theSibling := theSibling.GetPrevSiblingNotHidden; // looks weird but does the Right Thing
            if assigned( theSibling ) then
              MovingNode.MoveTo( theSibling, naInsert )
            else
              MovingNode.MoveTo( MovingNode.Parent, naAddChildFirst );
            t := '';
          end;
        end;
        dirDown : begin
          //theSibling := MovingNode.GetNextSibling;   [dpv]
          theSibling := MovingNode.GetNextSiblingNotHidden;
          if assigned( theSibling ) then begin
            MovingNode.MoveTo( theSibling, naInsert );
            t := '';
          end;
        end;

        // LEFT promotes node 1 level up
        // RIGHT demotes node 1 level down
        dirLeft : begin
          if assigned( MovingNode.Parent ) then begin
            // becomes its parent's sibling
            MovingNode.Moveto( MovingNode.Parent, naInsert );
            t := '';
          end;
        end;
        dirRight : begin
          theSibling := MovingNode.GetPrevSibling;
          if assigned( theSibling ) then begin
            // becomes the last child of its previous sibling
            MovingNode.MoveTo( theSibling, naAddChild );
            t := '';
          end;
        end;
      end;


      if ( t = '' ) then begin // means node was successfully moved
        // update node icon
        SelectIconForNode( PreviousParent, myFolder.IconKind );
        SelectIconForNode( MovingNode, myFolder.IconKind );
        SelectIconForNode( MovingNode.Parent, myFolder.IconKind );
        myFolder.TV.Invalidate;
      end;

    except
      on E : Exception do
      begin
        messagedlg( STR_06 + #13 + E.Message, mtError, [mbOK], 0 );
        s := 'Error moving node!';
      end;
    end;
  finally
    FNodeMoving:= false;       // [dpv]
    KntFile.Modified := true;
    if TKntNote(MovingNode.Data).Checked then
       MovingNode.CheckState := csChecked;

    myFolder.TV.OnChecked:= Form_Main.TVChecked;
    myFolder.TV.OnChange := Form_Main.TVChange;
    UpdateKntFileState( [fscModified] );
    // {N}
    s := Format( STR_07, [MovingNode.Text,t,DIRECTION_NAMES[aDir]] );
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := s;
  end;

end; // MoveTreeNode


procedure CopyNodePath( const InsertInEditor : boolean );
var
  myTreeNode : TTreeNTNode;
  s : string;
begin
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;
  s := GetNodePath( myTreeNode, TreeOptions.NodeDelimiter, TreeOptions.PathTopToBottom );
  ClipBoard.SetTextBuf( PChar( s ));
  if InsertInEditor then
  begin
    ActiveEditor.SelText := s + ' ';
    ActiveEditor.SelLength := 0;
  end
end; // CopyNodePath


procedure PasteNodeName( const PasteMode : TPasteNodeNameMode );
var
  myTreeNode : TTreeNTNode;
  myNewName: string;
  p : integer;
  s : string;
begin
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;

  if Form_Main.FolderIsReadOnly( ActiveFolder, true ) then exit;
  myNewName := '';

  case PasteMode of
    pnnClipboard : begin
      myNewName := Trim( Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH));
    end;
    pnnDate : begin
      myNewName := FormatDateTime( KeyOptions.DateFmt, now );
    end;
    pnnTime : begin
      myNewName := FormatDateTime( KeyOptions.TimeFmt, now );
    end;
    pnnDateTime : begin
      myNewName := FormatDateTime( KeyOptions.DateFmt + #32 + KeyOptions.TimeFmt, now );
    end;
    pnnSelection : begin
      if ( ActiveEditor.SelLength = 0 ) then
      begin
        App.WarnNoTextSelected;
        exit;
      end;
      s := ActiveEditor.SelVisibleText;
      p := Pos( #13, s );
      if ( p > 0 ) then
        delete( s, p, length( s ));
      myNewName := s;
      myNewName := trim( myNewName );
    end;
  end;

  if ( myNewName <> '' ) then begin
    try
      myTreeNode.Text := myNewName;  // {N}
      TKntNote( myTreeNode.Data ).Name := myNewName;
    finally
      KntFile.Modified := true;
      UpdateKntFileState( [fscModified] );
    end;
  end;

end; // PasteNodeName


procedure ShowOrHideIcons( const myFolder : TKntFolder; const UpdateNodes : boolean );
var
  myTreeNode : TTreeNTNode;
begin
  myFolder.TV.Items.BeginUpdate;
  try
   with Form_Main do begin
     case myFolder.IconKind of
       niNone : begin
         myFolder.TV.Images := nil;
         MMViewNodeIcons.Checked := false;
         MMViewCustomIcons.Checked := false;
       end;
       niStandard : begin
         myFolder.TV.Images := IMG_TV;
         MMViewNodeIcons.Checked := true;
         MMViewCustomIcons.Checked := false;
       end;
       niCustom : begin
         myFolder.TV.Images := Chest.IMG_Categories;
         MMViewNodeIcons.Checked := false;
         MMViewCustomIcons.Checked := true;
       end;
    end;
    TVSelectNodeImage.Enabled := ( MMViewCustomIcons.Checked and MMViewCustomIcons.Enabled );

    if UpdateNodes then begin
       myTreeNode := myFolder.TV.Items.GetFirstNode;
       while assigned( myTreeNode ) do begin
         SelectIconForNode( myTreeNode, myFolder.IconKind );
         myTreeNode := myTreeNode.GetNext;
       end;
    end;
   end;

  finally
    myFolder.TV.Items.EndUpdate;
  end;
end; // ShowOrHideIcons


procedure ShowOrHideCheckBoxes( const myFolder : TKntFolder );
var
  node : TTreeNTNode;
  myNote : TKntNote;
  enableCheck: boolean;
begin
  enableCheck:= false;
  if myFolder.Checkboxes then                                 // [dpv]
     enableCheck:= true;

  node:= myFolder.TV.Selected;
  if assigned(node) and assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox) then    // [dpv]
     enableCheck:= true;

 //Form_Main.TVCheckNode.Enabled := myFolder.Checkboxes;  // [dpv]
   Form_Main.TVCheckNode.Enabled := enableCheck;       // [dpv]

  myFolder.TV.Items.BeginUpdate;
  try
    if myFolder.Checkboxes then begin
      myFolder.TV.Items.TopLevelCheckType := ctCheckBox;
      node := myFolder.TV.Items.GetFirstNode;
      while assigned( node ) do begin
        node.CheckType := ctCheckBox;
        myNote := TKntNote( node.Data );
        if myNote.Checked then
          node.CheckState := csChecked
        else
          node.CheckState := csUnchecked;
        node := node.GetNext;
      end;
    end
    else begin
      myFolder.TV.Items.TopLevelCheckType := ctNone;
      node := myFolder.TV.Items.GetFirstNode;
      while assigned( node ) do begin
        ShowOrHideChildrenCheckBoxes (node);    // [dpv]
        //node.CheckType := ctNone;             // [dpv]
        node := node.GetNext;
      end;
    end;
  finally
    myFolder.TV.Items.EndUpdate;
  end;
end; // ShowOrHideCheckBoxes


procedure ShowOrHideChildrenCheckBoxes( const tnode : TTreeNTNode );
var
  myNote : TKntNote;
  node : TTreeNTNode;

begin

 try
  TNode.Owner.BeginUpdate;
  myNote := TKntNote( tnode.Data );
  if myNote.ChildrenCheckbox then begin
    tNode.CheckType  := ctCheckBox;
    node := tnode.GetFirstChild;
    while assigned( node ) do begin
      myNote := TKntNote( node.Data );
      if myNote.Checked then
        node.CheckState := csChecked
      else
        node.CheckState := csUnchecked;
      node := node.GetNextSibling;
     end
  end
  else
    tNode.CheckType := ctNone;

 finally
   TNode.Owner.EndUpdate;
 end;
end; // ShowOrHideChildrenCheckBoxes


procedure SetTreeNodeColor( const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean );
var
  myColor : TColor;
  myHasThisColor : boolean;
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  myFolder : TKntFolder;

    procedure ColorChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do begin
        if AsTextColor then begin
          myChildNode.Font.Color := myColor;
          with TKntNote( myChildNode.Data ) do begin
            NodeColor := myColor;
            HasNodeColor := myHasThisColor;
          end;
        end
        else begin
          myChildNode.Color := myColor;
          with TKntNote( myChildNode.Data ) do begin
            NodeBGColor := myColor;
            HasNodeBGColor := myHasThisColor;
          end;
        end;

        if myChildNode.HasChildren then
          ColorChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;

begin
  myTreeNode := GetCurrentTreeNode;
  if (not assigned( myTreeNode )) or Form_Main.FolderIsReadOnly(ActiveFolder, true) then exit;

  myFolder := ActiveFolder;
  myNote := TKntNote(myTreeNode.Data);
  try
    case AsTextColor of
      true : begin
        if UseColorDlg then begin
          if myNote.HasNodeColor then
            myColor := myNote.NodeColor
          else
            myColor := myFolder.TreeChrome.Font.Color;
          Form_Main.ColorDlg.Color := myColor;
          if Form_Main.ColorDlg.Execute then
            myColor := Form_Main.ColorDlg.Color
          else
            exit;
        end
        else begin
          if ResetDefault then
            myColor := myFolder.TreeChrome.Font.Color
          else
            myColor := Form_Main.TB_Color.ActiveColor;
        end;

        myHasThisColor := ( myColor <> myFolder.TreeChrome.Font.Color );
        myNote.HasNodeColor := myHasThisColor;

        myNote.NodeColor := myColor;
        myTreeNode.Font.Color := myColor;

      end;

      false : begin

        if UseColorDlg then begin
          if myNote.HasNodeBGColor then
            myColor := myNote.NodeBGColor
          else
            myColor := myFolder.TV.Color;
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
            myColor := myFolder.TV.Color
          else
            myColor := Form_Main.TB_Hilite.ActiveColor;
        end;

        myHasThisColor := ( myColor <> myFolder.TV.Color );
        myNote.HasNodeBGColor := myHasThisColor;

        myNote.NodeBGColor := myColor;
        myTreeNode.Color := myColor;
      end;
    end;

    if ( DoChildren and myTreeNode.HasChildren ) then
      ColorChildren( myTreeNode );

  finally
    ActiveFile.Modified := true;
    UpdateKntFileState( [fscModified] );
  end;
end; // SetTreeNodeColor


procedure SetTreeNodeBold( const DoChildren : boolean );
var
  myNote : TKntNote;
  myTreeNode : TTreeNTNode;
  newStyle : TFontStyles;

    procedure BoldChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do begin
        myChildNode.Font.Style := newStyle;
        TKntNote( myChildNode.Data ).Bold := myNote.Bold;
        if myChildNode.HasChildren then
          BoldChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;
begin
  if (not assigned( ActiveFolder )) or Form_Main.FolderIsReadOnly(ActiveFolder, true) then exit;

  myNote := GetCurrentNote;
  if ( not assigned( myNote )) then exit;
  if ( ActiveFolder.FocusMemory <> focTree ) then exit;
  myTreeNode := ActiveFolder.TV.Selected;
  if ( not assigned( myTreeNode )) then exit;

  myNote.Bold := ( not myNote.Bold );
  Form_Main.TVBoldNode.Checked := myNote.Bold;

  newStyle := ActiveFolder.TV.Font.Style;      // Default TV font can include other styles
  if myNote.Bold then
     newStyle := newStyle + [fsBold];

  myTreeNode.Font.Style := newStyle;

  if DoChildren and myTreeNode.HasChildren then
     BoldChildren( myTreeNode );

  ActiveFile.Modified := true;
  UpdateKntFileState( [fscModified] );
end; // SetTreeNodeBold


procedure SetTreeNodeCustomImage;
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  NewIdx, ImgIdx : integer;
  DoChildren : boolean;

    procedure SetChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do
      begin
        TKntNote( myChildNode.Data ).ImageIndex := newIdx;
        SelectIconForNode( myChildNode, niCustom );
        if myChildNode.HasChildren then
          SetChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;


begin
  myTreeNode := GetCurrentTreeNode;
  if (not assigned( myTreeNode )) or Form_Main.FolderIsReadOnly(ActiveFolder, true) then exit;

  myNote := TKntNote( myTreeNode.Data );
  DoChildren := myTreeNode.HasChildren;
  ImgIdx := myNote.ImageIndex;
  newIdx := PickImage( ImgIdx, DoChildren );

  if (( newIdx <> ImgIdx ) and ( newIdx <> -1 )) then begin
    try
      myNote.ImageIndex := newIdx;
      SelectIconForNode( myTreeNode, niCustom );

      if DoChildren then
         SetChildren( myTreeNode );

    finally
      ActiveFile.Modified := true;
      UpdateKntFileState( [fscModified] );
    end;
  end;
end; // SetTreeNodeCustomImage


procedure SetTreeNodeFontFace( const ResetDefault, DoChildren : boolean );
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  myFolder : TKntFolder;
  myFontFace : string;

    procedure SetFontChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;

      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do begin
        myChildNode.Font.Name := myFontFace;

        with TKntNote( myChildNode.Data ) do begin
          if ResetDefault then
            NodeFontFace := ''
          else
            NodeFontFace := myFontFace;
        end;

        if myChildNode.HasChildren then
           SetFontChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;


begin
  myTreeNode := GetCurrentTreeNode;
  if (not assigned( myTreeNode )) or Form_Main.FolderIsReadOnly(ActiveFolder, true) then exit;

  myFolder := ActiveFolder;
  myNote := TKntNote( myTreeNode.Data );

  try
    if ResetDefault then begin
      myFontFace := ActiveFolder.TreeChrome.Font.Name;
      myNote.NodeFontFace := '';
    end
    else begin
      myFontFace := Form_Main.Combo_Font.FontName;
      myNote.NodeFontFace := myFontFace;
    end;

    myTreeNode.Font.Name := myFontFace;

    if ( DoChildren and myTreeNode.HasChildren ) then
      SetFontChildren( myTreeNode );

  finally
    ActiveFile.Modified := true;
    UpdateKntFileState( [fscModified] );
  end;


end; // SetTreeNodeFontFace


procedure SetTreeNodeFontSize( const ResetDefault, DoChildren : boolean );
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
  myFolder : TKntFolder;
begin
  myTreeNode := GetCurrentTreeNode;
  if (not assigned( myTreeNode )) or Form_Main.FolderIsReadOnly(ActiveFolder, false) then exit;

  myFolder := ActiveFolder;
  myNote := TKntNote( myTreeNode.Data );

  try
    myTreeNode.Font.Size := strtoint( Form_Main.Combo_FontSize.Text );
    if ( myTreeNode.Font.Size >= ( ActiveFolder.TV.ItemHeight -4 )) then
      ActiveFolder.TV.ItemHeight := myTreeNode.Font.Size+6;
  except
  end;

end; // SetTreeNodeFontSize


function GetNodePath( aNode : TTreeNTNode; const aDelimiter : string; const TopToBottom : boolean ) : string;
var
  s : string;
  myNote : TKntNote;
begin
  result := '';

  while assigned( aNode ) do begin
    myNote := TKntNote( aNode.Data );

    if ( s = '' ) then
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


procedure SelectIconForNode( const myTreeNode : TTreeNTNode; const IconKind : TNodeIconKind );
begin
  if not assigned(myTreeNode) then exit;

  case IconKind of

    niStandard : begin
      case TKntNote( myTreeNode.Data ).VirtualMode of
        vmNone : begin
          if myTreeNode.HasChildren then begin
            if ( myTreeNode.Level > 0 ) then
              myTreeNode.ImageIndex := ICON_BOOK
            else
              myTreeNode.ImageIndex := ICON_FOLDER;
          end
          else
            myTreeNode.ImageIndex := ICON_NOTE;
        end;

        vmText, vmRTF, vmHTML : begin
          myTreeNode.ImageIndex := ICON_VIRTUAL;
        end;
        vmIELocal : begin
          myTreeNode.ImageIndex := ICON_VIRTUALIELOCAL;
        end;
        vmIERemote : begin
          myTreeNode.ImageIndex := ICON_VIRTUALIEREMOTE;
        end;
        vmKNTNode : begin
          myTreeNode.ImageIndex := ICON_VIRTUAL_KNT_NODE;
        end;
      end;
      myTreeNode.SelectedIndex := succ( myTreeNode.ImageIndex );
    end;

    niCustom : begin
      myTreeNode.ImageIndex := TKntNote( myTreeNode.Data ).ImageIndex;
      myTreeNode.SelectedIndex := myTreeNode.ImageIndex;
    end;

    niNone : begin
      myTreeNode.ImageIndex := -1;
      myTreeNode.SelectedIndex := -1;
    end;
  end;

end; // SelectIconForNode


procedure DeleteTreeNode( const DeleteFocusedNode : boolean );
var
  myTreeNode, myTreeParent, myTreeChild, myNextChild : TTreeNTNode;
  myTV : TTreeNT;
  myFolder : TKntFolder;
  myNote : TKntNote;
  KeepChildNodes : boolean;
begin
  with Form_Main do begin
      KeepChildNodes := true;
      if FolderIsReadOnly( ActiveFolder, true ) then exit;
      myFolder := ActiveFolder;
      myNote := GetCurrentNote;
      if ( not assigned( myNote )) then exit;
      if ( myFolder.FocusMemory <> focTree ) then exit;


      myTV := myFolder.TV;
      myTreeNode := myTV.Selected;
      myTreeParent := myTreeNode.Parent;

      if DeleteFocusedNode then begin
        // delete focused node and all its children, if any

        if myTreeNode.HasChildren then begin
          // ALWAYS warn if node has children
          case DoMessageBox(
            Format( STR_09, [myTreeNode.Text, myTreeNode.Count] ) + STR_08, STR_10,
                MB_YESNOCANCEL+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL ) of
            ID_YES : KeepChildNodes := false;
            ID_NO  : KeepChildNodes := true;
            else
              exit;
          end;
        end
        else begin
          if TreeOptions.ConfirmNodeDelete then begin
            if ( DoMessageBox( Format( STR_11, [myTreeNode.Text] ) + STR_08, mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
          end;
        end;

      end
      else begin
        // command was to delete CHILDREN of focused node
        if myTreeNode.HasChildren then begin
          if ( DoMessageBox(
            Format( STR_12, [myTreeNode.Count, myTreeNode.Text] ) + STR_08, STR_10,
               MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES ) then exit;
         end
        else begin
          showmessage( STR_13 );
          exit;
        end;
      end;

      with myTV do begin
        OnChange := nil;
        OnDeletion := TVDeletion;
        Items.BeginUpdate;
      end;
      try
        try
          if DeleteFocusedNode then begin
            if KeepChildNodes then begin
              myTreeChild := myTreeNode.GetFirstChild;
              while assigned( myTreeChild ) do begin
                myNextChild := myTreeNode.GetNextChild( myTreeChild );
                myTreeChild.MoveTo( myTreeParent, naAddChild );
                SelectIconForNode( myTreeChild, myFolder.IconKind );
                myTreeChild := myNextChild;
              end;
            end;

            myTV.Items.Delete( myTreeNode );
            SelectIconForNode( myTreeParent, myFolder.IconKind );
          end
          else begin
            myTreeNode.DeleteChildren;
            SelectIconForNode( myTreeNode, myFolder.IconKind );
          end;

        except
          on E : Exception do
            messagedlg( STR_14 + #13 + E.Message, mtError, [mbOK], 0 );
        end;

      finally
        with myTV do begin
          OnDeletion := nil;
          OnChange := TVChange;
          Items.EndUpdate;
        end;

        myTreeNode:= myFolder.TV.Selected;
        if assigned( myTreeNode ) then
           TreeNodeSelected(myTreeNode, true)
        else begin
           myFolder.SelectedNote := nil;
           myFolder.Editor.Clear;
           myFolder.Editor.Enabled:= False;
           App.EditorLoaded(myFolder.Editor);
        end;

        ActiveFile.Modified := true;
        UpdateKntFileState( [fscModified] );
      end;
  end;

end; // DeleteTreeNode


function MoveSubtree( myTreeNode : TTreeNTNode ): boolean;
var
  myTreeParent : TTreeNTNode;
  myTV : TTreeNT;
  myFolder : TKntFolder;
  selectedNode: TTreeNTNode;
begin
  Result:= false;
  with Form_Main do begin
      if ( not assigned( myTreeNode )) then exit;
      if FolderIsReadOnly( ActiveFolder, true ) then exit;
      selectedNode := ActiveFolder.TV.Selected;
      if not assigned(selectedNode) then begin
        showmessage( STR_15 );
        exit;
      end;
      if ( myTreeNode = selectedNode ) then exit;

      myFolder:= ActiveFile.GetFolderByTreeNode(myTreeNode);
      if FolderIsReadOnly(myFolder, true) then exit;

      myTV := myFolder.TV;
      myTreeParent := myTreeNode.Parent;

      if (DoMessageBox(Format(STR_16,
        [TransferNodes.Count, myTreeNode.Text, selectedNode.Text] ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes) then
          exit;

      // Paste
      TreeTransferProc(1, nil, false, false, true );  // Graft Subtree

      // .. and Cut
      with myTV do begin
        OnChange := nil;
        OnDeletion := TVDeletion;
        Items.BeginUpdate;
      end;

      try
        try
            myTV.Items.Delete( myTreeNode );
            SelectIconForNode( myTreeParent, myFolder.IconKind );
        except
          on E : Exception do
            messagedlg( STR_14 + #13 + E.Message, mtError, [mbOK], 0 );
        end;

      finally
        with myTV do begin
          OnDeletion := nil;
          OnChange := TVChange;
          Items.EndUpdate;
        end;
        if assigned( myFolder.TV.Selected ) then
          myFolder.SelectedNote := TKntNote( myFolder.TV.Selected.Data )
        else
          myFolder.SelectedNote := nil;

        myFolder.DataStreamToEditor;
      end;

      ActiveFile.Modified := true;
      UpdateKntFileState( [fscModified] );
      Result:= true;
  end;

end; // MoveSubtree


procedure UpdateTreeNode( const aTreeNode : TTreeNTNode; Folder: TKntFolder );
var
  myNote : TKntNote;
begin
  if assigned( aTreeNode ) then begin

    myNote := TKntNote( aTreeNode.Data );
    if assigned( myNote ) then begin
      Folder.TV.OnChecked:= nil;

      // [x] FIXME: in many cases by doing this we are setting
      // the treenode text TWICE. In some cases this line is
      // necessary, though. Bad code design.
      aTreeNode.Text := myNote.Name; // {N}

      if myNote.Filtered then               // [dpv]
         aTreeNode.Hidden:= True;

      if myNote.Bold then
        aTreeNode.Font.Style := Folder.TV.Font.Style + [fsBold];

      if myNote.ChildrenCheckbox then       // [dpv]
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

      Folder.TV.OnChecked:= Form_Main.TVChecked;
    end;
  end;
end; // UpdateTreeNode


procedure CreateNodefromSelection;
var
  myTreeNode, SelectedNode : TTreeNTNode;
  myFolder : TKntFolder;
  myNote : TKntNote;
  myRTFText: AnsiString;
  myNodeName : string;
  p : integer;
begin
  with Form_Main do begin
      if ( not assigned( ActiveFolder )) then exit;
      myFolder := ActiveFolder;
      if FolderIsReadOnly(myFolder, true ) then exit;


      if ( ActiveEditor.SelLength = 0 ) then begin
        App.WarnNoTextSelected;
        exit;
      end;
  end;

  SelectedNode := myFolder.TV.Selected;
  if ( not assigned( SelectedNode )) then begin
    Form_Main.Statusbar.Panels[PANEL_HINT].Text := STR_17;
    exit;
  end;

  myNodeName:= FirstLineFromString(TrimLeft( ActiveEditor.SelText ), TREENODE_NAME_LENGTH_CAPTURE);

  myRTFText := ActiveEditor.RtfSelText;

  myTreeNode := TreeNewNode( nil, tnAddAfter, nil, '', true );
  if assigned( myTreeNode ) then begin
    myFolder.TV.Items.BeginUpdate;
    try

      myFolder.TV.Selected := myTreeNode;
      myNote := TKntNote( myTreeNode.Data );

      if ( myNodeName <> '' ) then begin   // can be blank
        myNote.Name := myNodeName;
        myTreeNode.Text := myNote.Name;
      end;

      myNote.Stream.Position := 0;
      myNote.Stream.WriteBuffer( myRTFTExt[1], length( myRTFTExt ));
      myFolder.SelectedNote := myNote;
      myFolder.DataStreamToEditor;

      myTreeNode.MakeVisible;

    finally
      myFolder.TV.Items.EndUpdate;
      ActiveFile.Modified := true;
      UpdateKntFileState( [fscModified] );
    end;
  end;

end; // CreateNodefromSelection


procedure NavigateInTree( NavDirection : TNavDirection );
var
  VKey : Word;
  SBVal, ScrollVal, ScrollMsg : integer;
begin

  if not assigned( ActiveFolder ) then exit;

  VKey := 0;
  SBVal := SB_LINEDOWN;
  ScrollVal := SB_VERT;
  ScrollMsg := WM_VSCROLL;


  {
    If the tree has focus, Alt+arrow scrolls the editor;
    if the editor is focused, Alt+arrow scrolls the tree
  }

  if ActiveFolder.TV.Focused then begin

    case NavDirection of
      navUp : begin
        SBVal := SB_LINEUP;
        ScrollVal := SB_VERT;
        ScrollMsg := WM_VSCROLL;
      end;
      navDown : begin
        SBVal := SB_LINEDOWN;
        ScrollVal := SB_VERT;
        ScrollMsg := WM_VSCROLL;
      end;
      navLeft : begin
        SBVal := SB_LINELEFT;
        ScrollVal := SB_HORZ;
        ScrollMsg := WM_HSCROLL;
      end;
      navRight : begin
        SBVal := SB_LINERIGHT;
        ScrollVal := SB_HORZ;
        ScrollMsg := WM_HSCROLL;
      end;
    end;

    with ActiveFolder.Editor do begin
      perform( ScrollMsg, MakeLong( SBVal, GetScrollPos( handle, ScrollVal )), 0 );
      perform( ScrollMsg, MakeLong( SB_ENDSCROLL, GetScrollPos( handle, ScrollVal )), 0 );
    end;
  end
  else begin
    case NavDirection of
      navUp  :  VKey := VK_UP;
      navDown:  VKey := VK_DOWN;
      navLeft:  VKey := VK_LEFT;
      navRight: VKey := VK_RIGHT;
    end;
    with ActiveFolder.TV do begin
      Perform( WM_KEYDOWN, VKey, 0 );
      Perform( WM_KEYUP, VKey, 0 );
    end;
  end;
end; // NavigateInTree


procedure CopyNodeName( const IncludeNoteText : boolean );
var
  myTreeNode : TTreeNTNode;
begin
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;

  if IncludeNoteText then
     ClipBoard.AsText:=  myTreeNode.Text + #13#13 + RemoveKNTHiddenCharactersInText(ActiveFolder.Editor.Text)
  else
     ClipBoard.AsText:= myTreeNode.Text;
end; // CopyNodeName


procedure ConvertStreamContent(Stream: TMemoryStream; FromFormat, ToFormat: TRichStreamFormat; RTFAux : TRxRichEdit);
var
  Encoding: TEncoding;
begin
   Encoding:= nil;
   RTFAux.Clear;
   RTFAux.StreamMode := [];

   RTFAux.StreamFormat:= FromFormat;
   RTFAux.Lines.LoadFromStream( Stream );
   RTFAux.StreamFormat := ToFormat;

   if (ToFormat = sfPlainText) and (not CanSaveAsANSI(RTFAux.Text)) then
      Encoding:= TEncoding.UTF8;

   Stream.Clear;
   RTFAux.Lines.SaveToStream(Stream, Encoding);
end;


function TreeTransferProc( const XferAction : integer; const PasteTargetFolder : TKntFolder; const Prompt : boolean ; const PasteAsVirtualKNTNode: boolean; const MovingSubtree: boolean) : boolean;
var
  newNote : TKntNote;
  myTreeNode, newTreeNode, LastNodeAssigned, FirstCopiedNode : TTreeNTNode;
  i, loop, PasteCount, StartLevel, LastLevel : integer;
  myFolder : TKntFolder;
  VirtualNodesConverted : integer;
  movingNoteNode, TransferedNoteNode : TTreeNTNode;
  RTFAux : TRxRichEdit;

  function CountVisibleTransferNodes: integer;
  var
     i: integer;
     TransferedNoteNode : TTreeNTNode;
  begin
      Result:= 0;
      for i := 0 to pred( TransferNodes.Count ) do begin
          TransferedNoteNode:= GetTreeNode(CopyCutFromFolderID, TransferNodes[i].ID);
          if assigned(TransferedNoteNode) and not TransferedNoteNode.Hidden then
             Result:= Result+1;
      end;
  end;

begin
  result := false;
  if ( XferAction = 2 ) then begin   // clear
    result := true;
    if assigned( TransferNodes ) then begin
      if (messagedlg(Format( STR_18,
        [TransferNodes.Count] ), mtConfirmation, [mbYes,mbNo], 0 ) = mrYes ) then
        TransferNodes.Free;
      TransferNodes := nil;
    end;
    exit;
  end;

  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if Form_Main.FolderIsReadOnly( ActiveFolder, true ) then exit;

  if ( PasteTargetFolder = nil ) then
     myTreeNode := GetCurrentTreeNode
  else
     myTreeNode := PasteTargetFolder.TV.Selected;

  if ( myTreeNode = nil ) then begin
    showmessage( STR_15 );
    exit;
  end;

  screen.Cursor := crHourGlass;
  try
    try

      case XferAction of
        0 : begin // COPY subtree

          ActiveFolder.EditorToDataStream;
          CopyCutFromFolderID:= ActiveFolder.ID;

          if assigned( TransferNodes ) then
             TransferNodes.Free;
          TransferNodes := TKntNoteList.Create;
          StartLevel := myTreeNode.Level;
          while assigned( myTreeNode ) do begin
              newNote := TKntNote.Create;
              newNote.Assign( TKntNote( myTreeNode.Data ));
              newNote.Level := myTreeNode.Level - StartLevel;
              newNote.ID:= TKntNote( myTreeNode.Data ).ID;
              TransferNodes.Add( newNote );
              myTreeNode := myTreeNode.GetNext;
              if (( myTreeNode <> nil ) and ( myTreeNode.Level <= StartLevel )) then
                  myTreeNode := nil; // end of subtree; break out of loop
          end;

          if ( TransferNodes.Count = 0 ) then begin
            showmessage( STR_19 );
            TransferNodes.Free;
            TransferNodes := nil;
          end
          else begin
            result := true;
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_20, [TransferNodes.Count] );
          end;
        end;

        1 : begin // PASTE subtree
          if ( not assigned( TransferNodes )) then begin
            showmessage( STR_21 );
            exit;
          end;

          if PasteAsVirtualKNTNode then begin
              if Prompt then
                if ( DoMessageBox( Format(
                  STR_26,
                  [CountVisibleTransferNodes, myTreeNode.Text] ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then
                    exit;
          end
          else begin
              if TransferNodes.HasVirtualNotes then begin
                if ( messagedlg(STR_22, mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then
                   exit;
              end
              else begin
                 if Prompt then
                   if ( DoMessageBox(Format(STR_23,
                     [TransferNodes.Count,myTreeNode.Text] ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then // {N}
                       exit;
              end;
          end;

          ActiveFile.Modified := true;

          myFolder := ActiveFolder;
          LastNodeAssigned := myTreeNode;
          PasteCount := 0;
          VirtualNodesConverted := 0;
          FirstCopiedNode := nil;

          StartLevel := myTreeNode.Level;
          LastLevel := StartLevel+1;

          myFolder.TV.Items.BeginUpdate;
          RTFAux:= CreateAuxRichEdit;

          try
            for i := 0 to pred( TransferNodes.Count ) do begin
                TransferedNoteNode:= GetTreeNode(CopyCutFromFolderID, TransferNodes[i].ID);

                if not (PasteAsVirtualKNTNode and TransferedNoteNode.Hidden) then begin
                    newNote := TKntNote.Create;
                    newNote.Assign( TransferNodes[i] );

                    // As indicated in comment *1 in FileDropped (kn_NoteFileMng), we must check if it is neccesary to ensure that
                    // the new node's stream is loaded with RTF and not plain text.
                    if (not myFolder.PlainText) and (not NodeStreamIsRTF (newNote.Stream)) then
                        ConvertStreamContent(newNote.Stream, sfPlainText, sfRichText, RTFAux)
                    else
                    if (myFolder.PlainText) and (NodeStreamIsRTF (newNote.Stream)) then
                        // This case is not as problematic as the other, but if the new node were not modified, it would save
                        // its RTF content in a plain manner. Example:
                        // ;{{\rtf1\fbidis\ ....
                        // ;\par...
                        ConvertStreamContent(newNote.Stream, sfRichText, sfPlainText, RTFAux);

                    if MovingSubtree then
                       AlarmMng.MoveAlarms(KntFile.GetFolderByID(CopyCutFromFolderID), TransferNodes[i],  myFolder, newNote);

                    myFolder.AddNote( newNote );
                    newNote.Level := newNote.Level + StartLevel + 1;

                    if ( i = 0 ) then begin
                      newTreeNode := myFolder.TV.Items.AddChildFirst( myTreeNode, newNote.Name );
                      FirstCopiedNode := newTreeNode;
                    end
                    else begin
                      case DoTrinaryCompare( newNote.Level, LastLevel ) of
                        trinGreater : begin
                          newTreeNode := myFolder.TV.Items.AddChild( LastNodeAssigned, newNote.Name );
                        end;
                        trinEqual : begin
                          newTreeNode := myFolder.TV.Items.Add( LastNodeAssigned, newNote.Name );
                        end;
                        else begin
                          for loop := 1 to (( LastLevel - newNote.Level )) do
                             LastNodeAssigned := LastNodeAssigned.Parent;
                          newTreeNode := myFolder.TV.Items.Add( LastNodeAssigned, newNote.Name );
                        end;
                      end;
                    end;

                    if assigned( newNote ) then begin

                      LastLevel := newTreeNode.Level;
                      LastNodeAssigned := newTreeNode;
                      newNote.Level := newTreeNode.Level;

                      newTreeNode.Data := newNote;
                      UpdateTreeNode( newTreeNode, myFolder );

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
                              if ( newNote.VirtualMode <> vmNone) then begin
                                 if KntFile.HasVirtualNoteByFileName( newNote, newNote.VirtualFN ) then begin
                                   inc( VirtualNodesConverted );
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
                               if not (myFolder.PlainText) then
                                  ActiveFile.UpdateImagesCountReferences (newNote);

                              if MovingSubtree then begin
                                 movingNoteNode:= TransferedNoteNode;
                                 if assigned(movingNoteNode) then
                                     ActiveFile.ManageMirrorNodes(1, movingNoteNode, newTreeNode);
                              end
                          end;
                    end;
                    inc( PasteCount );

                end;     //if not (PasteAsVirtualKNTNode ....

            end;  // for

            newTreeNode := myTreeNode;

            while assigned( newTreeNode ) do begin
              SelectIconForNode( newTreeNode, myFolder.IconKind );
                newTreeNode := newTreeNode.GetNext;
              if (( newTreeNode <> nil ) and ( newTreeNode.Level <= StartLevel )) then
                newTreeNode := nil; // end of subtree; break out of loop
            end;
            result := true;
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_24, [PasteCount] );
            if assigned( FirstCopiedNode ) then begin
              FirstCopiedNode.Collapse( true );
              FirstCopiedNode.MakeVisible;
              // myFolder.TV.Selected := FirstCopiedNode;
            end;

          finally
            myFolder.TV.Items.EndUpdate;
            myFolder.TV.Selected := myTreeNode;
            TreeNodeSelected( myTreeNode );
            RTFAux.Free;
            // myTreeNode.Expand( true );
            if ( VirtualNodesConverted > 0 ) then begin
              showmessage( Format(STR_25,[VirtualNodesConverted]));
            end;
          end;
        end;
      end;

    except
      on E : Exception do
        showmessage( E.Message );
    end;

  finally
    screen.Cursor := crDefault;
    if (XferAction = 1) and Result then begin
       ActiveFile.Modified := true;
       UpdateKntFileState( [fscModified] );
    end;
  end;

end; // TreeTransferProc


procedure HideChildNodesUponCheckState (folder: TKntFolder; ParentNode: TTreeNTNode; CheckState: TCheckState);    // [dpv]
var
  Node : TTreeNTNode;
begin
    folder.TV.Items.BeginUpdate;

    if ParentNode = nil then begin
       Node := folder.TV.Items.GetFirstNode;
       while assigned( Node ) do begin // go through all nodes
         if Node.CheckState =  CheckState then
            Node.Hidden := True;
         Node := Node.GetNext; // select next node to search
       end;
    end
    else begin
       Node := ParentNode.GetFirstChild;
       while assigned( Node ) do begin
         if Node.CheckState =  CheckState then
            Node.Hidden := True;
         Node := Node.GetNextSibling;
        end

    end;

    folder.TV.Items.EndUpdate;
end;

procedure ShowCheckedNodes (folder: TKntFolder; ParentNode: TTreeNTNode);    // [dpv]
var
  Node : TTreeNTNode;
begin
  folder.TV.Items.BeginUpdate;
  if ParentNode = nil then begin
     Node := folder.TV.Items.GetFirstNode;
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

  folder.TV.Items.EndUpdate;
end;

procedure MarkAllFiltered (folder: TKntFolder);  // [dpv]
var
  Node: TTreeNTNode;
begin
  if not assigned(folder) then exit;

  Node := folder.TV.Items.GetFirstNode;
  while Node <> nil do begin
    TKntNote(Node.Data).Filtered := true;
    Node := Node.GetNext;
  end;
end;

procedure MarkAllUnfiltered (folder: TKntFolder);  // [dpv]
var
  Node: TTreeNTNode;
begin
  if not assigned(folder) then exit;

  Node := folder.TV.Items.GetFirstNode;
  while Node <> nil do begin
    Node.Font.Color:= TKntNote(Node.Data).NodeColor;
    TKntNote(Node.Data).Filtered := false;
    Node := Node.GetNext;
  end;
end;

procedure RemoveFilter (folder: TKntFolder);       // [dpv]
begin
    if not assigned(folder) then exit;

    Folder.TV.Items.BeginUpdate;
    MarkAllUnfiltered (folder);
    folder.TV.FullNotHidden;
    if Folder.HideCheckedNodes  then
       HideChildNodesUponCheckState (folder, nil, csChecked);

    Folder.TV.Items.EndUpdate;
end;

procedure HideFilteredNodes (folder: TKntFolder);  // [dpv]
var
  Node : TTreeNTNode;
begin
  folder.TV.Items.BeginUpdate;
  Node := folder.TV.Items.GetFirstNode;
  while Node <> nil do begin
    if TKntNote(Node.Data).Filtered then
       Node.Hidden:= true
    else begin
       Node.MakeVisibilityPosible;
       Node.Font.Color := clBlue;
    end;
    Node := Node.GetNext;
  end;
  folder.TV.Items.EndUpdate;
end;

function GetTreeNode (FolderID: integer; NodeID: integer): TTreeNTNode;
var
   folder: TKntFolder;
begin
   Result:= nil;
   if ( FolderID <> 0 ) and ( NodeID <> 0 ) then begin
       Folder := KntFile.GetFolderByID( FolderID );
       if assigned(Folder) then
          Result := Folder.GetTreeNodeByID( NodeID );
   end;
end;


procedure ChangeCheckedState(TV: TTreeNT; Node: TTreeNTNode; Checked: Boolean; CalledFromMirrorNode: Boolean);
var
  myNote : TKntNote;
  oldOnChecked : TTVCheckedEvent;

    procedure CheckChildren( StartNode : TTreeNTNode );
    var
      childNode : TTreeNTNode;
    begin
      childNode := StartNode.GetFirstChild;
      while ( assigned( childNode ) and assigned( childNode.Data )) do
      begin
        childNode.CheckState := node.CheckState;
        TKntNote( childNode.Data ).Checked := ( node.CheckState = csChecked );
        if childNode.HasChildren then
          CheckChildren( childNode ); // RECURSIVE CALL
        childNode := StartNode.GetNextChild( childNode );
      end;
    end;

begin
  if ( assigned( node ) and assigned( node.Data )) then
  begin
    oldOnChecked:= nil;
    if assigned(TV.OnChecked) then begin
       oldOnChecked := TV.OnChecked;
       TV.OnChecked := nil;
    end;

    try
      myNote := TKntNote( node.Data );
      if not CalledFromMirrorNode then
          myNote.Checked := ( node.CheckState = csChecked )
      else begin
         myNote.Checked := Checked;
         if Checked then
            node.CheckState := csChecked
         else
            node.CheckState := csUnchecked;
      end;
      if not CalledFromMirrorNode then
         KntFile.ManageMirrorNodes(2, Node, nil);

      if ( not CalledFromMirrorNode
           and shiftdown and node.HasChildren
           and not IsAnyNodeMoving) then     // [dpv]
        CheckChildren( node );

      if not IsAnyNodeMoving
          and KntFile.GetFolderByTreeNode(Node).HideCheckedNodes  then
          if (node.CheckState  = csChecked) then
              node.Hidden := True
          else
              node.Hidden := False;

    finally
        KntFile.Modified := true;
        UpdateKntFileState( [fscModified] );
        if assigned(oldOnChecked) then
           TV.OnChecked := oldOnChecked;

    end;
  end;
end;


procedure AddMirrorNode(MainNode: TTreeNTNode; Mirror_Node: TTreeNTNode);
var
   p: Pointer;
   o: TObject;
   NodesVirtual: TList;
begin
    if not assigned(MainNode) then exit;

    if TKntNote(MainNode.Data).VirtualMode = vmKNTnode then
       MainNode:= TKntNote(MainNode.Data).MirrorNode;

    p:= nil;
    MirrorNodes.Find(MainNode, p);
    if assigned(p) then begin
       o:= p;
       if o is TTreeNTNode then begin
          NodesVirtual:= TList.Create();
          NodesVirtual.Add(o);
          NodesVirtual.Add(Mirror_Node);
          MirrorNodes.Remove(MainNode);
          MirrorNodes.Add(MainNode, NodesVirtual);
          end
       else begin
          NodesVirtual:= p;
          NodesVirtual.Add(Mirror_Node);
       end;
    end
    else begin        // First mirror of originalNode
         MirrorNodes.Add(MainNode, Mirror_Node);
         TKntNote(MainNode.Data).AddedMirrorNode;         // mark original node
    end;
end;

procedure ReplaceNonVirtualNote(MainNode: TTreeNTNode; newNode: TTreeNTNode);
var
   p: Pointer;
begin
   p:= nil;
   MirrorNodes.Find(MainNode, p);
   if assigned(p) then begin
      MirrorNodes.Remove(MainNode);
      MirrorNodes.Add(newNode, p);
      TKntNote(MainNode.Data).RemovedAllMirrorNodes;   // mark node
      TKntNote(newNode.Data).AddedMirrorNode;          // mark node
   end;
end;

procedure RemoveMirrorNode(MainNode: TTreeNTNode; mirror_Node: TTreeNTNode);
var
   p: Pointer;
   o: TObject;
   NodesVirtual: TList;
   RemovedAllMirrorNodes: boolean;
begin
    p:= nil;
    RemovedAllMirrorNodes:= false;
    MirrorNodes.Find(MainNode, p);
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
       MirrorNodes.Remove(MainNode);
       TKntNote(MainNode.Data).RemovedAllMirrorNodes;        // mark original node
    end;
end;

function GetMirrorNodes(originalNode: TTreeNTNode): Pointer;
var
   p: Pointer;
begin
    p:= nil;
    if TKntNote(originalNode.Data).HasMirrorNodes then begin
        MirrorNodes.Find(originalNode, p);
    end;
    Result:= p;
end;


initialization
  __NodeChangeCounter := 0;
  FNodeMoving:= false;     // [dpv]
  TransferNodes := nil;
  _LAST_NODE_SELECTED := nil;
  DraggedTreeNode := nil;
  _OLD_NOTE_NAME := DEFAULT_NEW_NOTE_NAME;

  MirrorNodes:= TBucketList.Create();
end.
