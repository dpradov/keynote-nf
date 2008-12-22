unit kn_TreeNoteMng;

interface

uses
  TreeNT, kn_nodeList, kn_noteObj, kn_Info, kn_Const;

var
    TransferNodes : TNodeList; // for data transfer (copy tree nodes between tabs)
    DraggedTreeNode : TTreeNTNode;
    _LAST_NODE_SELECTED : TTreeNTNode;
    _OLD_NODE_NAME : string;

    // treenote-related methods:
    function AddNodeToTree( aInsMode : TNodeInsertMode ) : TTreeNTNode;
    function TreeNoteNewNode( const aTreeNote : TTreeNote; aInsMode : TNodeInsertMode; const aOriginNode : TTreeNTNode; const aNewNodeName : string; const aDefaultNode : boolean ) : TTreeNTNode;
    procedure TreeNodeSelected( Node : TTreeNTNode );
    procedure DeleteTreeNode( const DeleteFocusedNode : boolean );
    procedure UpdateTreeNode( const aTreeNode : TTreeNTNode );
    procedure CreateMasterNode;
    procedure OutlineNumberNodes;
    procedure CreateNodefromSelection;
    function GetCurrentNoteNode : TNoteNode;
    function GetCurrentTreeNode : TTreeNTNode;
    procedure SelectIconForNode( const myTreeNode : TTreeNTNode; const IconKind : TNodeIconKind );
    procedure UpdateTreeChrome( const myNote : TTreeNote );
    procedure UpdateTreeOptions( const myNote : TTreeNote );
    procedure MoveTreeNode( MovingNode : TTreeNTNode; const aDir : TDirection );
    procedure PasteNodeName( const PasteMode : TPasteNodeNameMode );
    procedure CopyNodeName( const IncludeNoteText : boolean );
    procedure CopyNodePath( const InsertInEditor : boolean );
    procedure ShowOrHideIcons( const tNote : TTreeNote; const UpdateNodes : boolean );
    procedure ShowOrHideCheckBoxes( const tNote : TTreeNote );
    procedure SetTreeNodeColor( const UseColorDlg, AsTextColor, ResetDefault, DoChildren : boolean );
    procedure SetTreeNodeBold( const DoChildren : boolean );
    procedure SetTreeNodeCustomImage;
    procedure SetTreeNodeFontFace( const ResetDefault, DoChildren : boolean );
    procedure SetTreeNodeFontSize( const ResetDefault, DoChildren : boolean );
    procedure NavigateInTree( NavDirection : TNavDirection );
    function GetNodePath( aNode : TTreeNTNode; const aDelimiter : string; const TopToBottom : boolean ) : string;

    function TreeTransferProc( const XferAction : integer; const PasteTargetNote : TTreeNote; const Prompt : boolean ) : boolean;

    procedure HideCheckedNodes (note: TTreeNote);    // [dpv]
    procedure ShowCheckedNodes (note: TTreeNote);    // [dpv]
    procedure ShowOrHideChildrenCheckBoxes( const tnode : TTreeNTNode );   // [dpv]

    function IsAnyNodeMoving: boolean;              // [dpv]
    procedure MarkAllFiltered (note: TTreeNote);    // [dpv]
    procedure MarkAllUnfiltered (note: TTreeNote);  // [dpv]
    procedure RemoveFilter (note: TTreeNote);       // [dpv]
    procedure HideFilteredNodes (note: TTreeNote);  // [dpv]
    function NoteOfNode (node: TTreeNTNode): TTreeNote;  // [dpv]

implementation
uses
   Windows, Messages, Forms, SysUtils, Dialogs,
   Controls, Graphics, Clipbrd,
   gf_strings, gf_miscvcl, gf_misc,
   kn_Global, kn_MacroMng, kn_Main, kn_Chest, kn_NodeNum, kn_RTFUtils, kn_ImagePicker,
   kn_VirtualNodeMng, kn_Macro, kn_FindReplaceMng, kn_NoteFileMng, kn_LinksMng,
   kn_EditorUtils;

var
   __NodeChangeCounter : longint;
   FNodeMoving: boolean;             // [dpv]

function IsAnyNodeMoving: boolean;   // [dpv]
begin
  result:= FNodeMoving;
end;

function AddNodeToTree( aInsMode : TNodeInsertMode ) : TTreeNTNode;
begin
  result := TreeNoteNewNode( nil, aInsMode, nil, '', false );
  if ( KeyOptions.RunAutoMacros and assigned( result )) then
  begin
    if fileexists( Macro_Folder + _MACRO_AUTORUN_NEW_NODE ) then
      ExecuteMacro( _MACRO_AUTORUN_NEW_NODE, '' );
  end;
end; // AddNodeToTree

function TreeNoteNewNode(
  const aTreeNote : TTreeNote;
  aInsMode : TNodeInsertMode;
  const aOriginNode : TTreeNTNode;
  const aNewNodeName : string;
  const aDefaultNode : boolean ) : TTreeNTNode;
var
  myNode, myParentNode : TNoteNode;
  myTreeNode, myOriginNode, mySiblingNode : TTreeNTNode;
  myNote : TTreeNote;
  myName : string;
  p : integer;
  AddingFirstNode, addnumber : boolean;
begin
  result := nil;
  if ( aTreeNote = nil ) then
  begin
    if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;
    myNote := TTreeNote( ActiveNote );
  end
  else
  begin
    myNote := aTreeNote;
  end;
  if Form_Main.NoteIsReadOnly( myNote, true ) then exit;

  myTreeNode := nil; { just to avoid }
  myNode := nil;     { compiler warning }

  addnumber := myNote.AutoNumberNodes;
  if ( aNewNodeName <> '' ) then
  begin
    myName := aNewNodeName;
  end
  else
  begin
    myName := myNote.DefaultNodeName;

    // check for special tokens
    p := pos( NODEINSDATE, myName );
    if ( p > 0 ) then
    begin
      delete( myName, p, length( NODEINSDATE ));
      insert( FormatDateTime( KeyOptions.DateFmt, now ), myName, p );
      addnumber := false;
    end;

    p := pos( NODEINSTIME, myName );
    if ( p > 0 ) then
    begin
      delete( myName, p, length( NODEINSTIME ));
      insert( FormatDateTime( KeyOptions.TimeFmt, now ), myName, p );
      addnumber := false;
    end;

    p := pos( NODECOUNT, myName );
    if ( p > 0 ) then
    begin
      delete( myName, p, length( NODECOUNT ));
      insert( inttostr( succ( myNote.TV.Items.Count )), myName, p );
      addnumber := false;
    end;

    if addnumber then
      myName := myNote.DefaultNodeName + #32 + inttostr( succ( myNote.TV.Items.Count ));

  end;

  if ( myNote.TV.Items.Count = 0 ) or ( not assigned( myNote.TV.Selected )) then
  begin
    AddingFirstNode := true;
    aInsMode := tnTop; // no children or siblings if no nodes
  end
  else
  begin
    AddingFirstNode := false;
  end;

  try
    try

      // myNote.TV.OnChange := nil;
      if ( aOriginNode = nil ) then
        myOriginNode := myNote.TV.Selected
      else
        myOriginNode := aOriginNode;

      case aInsMode of
        tnTop : begin
          myTreeNode := myNote.TV.Items.AddFirst( nil, myName );
        end;
        tnInsertBefore : begin
          myTreeNode := myNote.TV.Items.Insert( myOriginNode, myName );
        end;
        tnAddLast : begin
          myTreeNode := myNote.TV.Items.Add( myOriginNode, myName );
        end;
        tnAddChild : begin
          myTreeNode := myNote.TV.Items.AddChild( myOriginNode, myName );
        end;
        tnAddAfter : begin
          mySiblingNode := myOriginNode.GetNextSibling;
          if assigned( mySiblingNode ) then
            myTreeNode := myNote.TV.Items.Insert( mySiblingNode, myName )
          else
            myTreeNode := myNote.TV.Items.Add( mySiblingNode, myName );
        end;
      end;

      result := myTreeNode;

      // these tokens can be expanded only after the node was created
      if ( aNewNodeName = '' ) then
      begin
        if ( pos( '%', myName ) > 0 ) then
        begin
          p := pos( NODELEVEL, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODELEVEL ));
            insert( inttostr( myTreeNode.Level ), myName, p );
          end;

          p := pos( NODEINDEX, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODEINDEX ));
            insert( inttostr( succ( myTreeNode.Index )), myName, p );
          end;

          p := pos( NODEABSINDEX, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODEABSINDEX ));
            insert( inttostr( succ( myTreeNode.AbsoluteIndex )), myName, p );
          end;

          p := pos( NODEPARENT, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODEPARENT ));
            if assigned( myTreeNode.Parent ) then
              insert( myTreeNode.Parent.Text, myName, p )
            else
              insert( '<NONE>', myName, p );
          end;

          p := pos( NODENOTENAME, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODENOTENAME ));
            insert( RemoveAccelChar( ActiveNote.Name ), myName, p );
          end;

          p := pos( NODEFILENAME, myName );
          if ( p > 0 ) then
          begin
            delete( myName, p, length( NODEFILENAME ));
            insert( extractfileName( NoteFile.FileName ), myName, p );
          end;

        end;
      end;

      if assigned( myOriginNode ) then
        myParentNode := TNoteNode( myOriginNode.Data )
      else
        myParentNode := nil;

      myNode := myNote.NewNode( myParentNode, myName, TreeOptions.InheritNodeProperties );

      myTreeNode.Data := myNode;

      if AddingFirstNode then
      begin
        ShowOrHideIcons( myNote, true );
        ShowOrHideCheckBoxes( myNote );
      end;

      SelectIconForNode( myTreeNode.Parent, myNote.IconKind );
      SelectIconForNode( myTreeNode, myNote.IconKind );

      UpdateTreeNode( myTreeNode );

      if TreeOptions.InheritNodeBG then
        myNode.RTFBGColor := ActiveNote.Editor.Color;
        // assign the color of the currently displayed node

      myNote.SelectedNode := myNode;
      myNote.DataStreamToEditor;

      myTreeNode.MakeVisible;
      myNote.TV.Selected := myTreeNode;

    except
      on E : Exception do
      begin
        messagedlg( 'Error creating node: ' + E.Message, mtError, [mbOK], 0 );
        // if assigned( myTreeNode ) then myTreeNode.Free;
        // if assigned( myNode ) then myNode.Free;
      end;
    end;

  finally
    myNote.SelectedNode := myNode;
    myNote.TV.OnChange := Form_Main.TVChange;
    VirtualNodeUpdateMenu( false );
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;

  if (( not aDefaultNode ) and assigned( myTreeNode ) and TreeOptions.EditNewNodes ) then
  begin
    Form_Main.MMRenamenodeClick( nil );
  end;

end; // TreeNoteNewNode

function GetCurrentNoteNode : TNoteNode;
begin
  result := nil;
  if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;
  if assigned( TTreeNote( ActiveNote ).TV.Selected ) then
    result := TNoteNode( TTreeNote( ActiveNote ).TV.Selected.Data );
end; // GetCurrentNoteNode

function GetCurrentTreeNode : TTreeNTNode;
begin
  result := nil;
  if ( not (( assigned( NoteFile ) and assigned( ActiveNote )) and ( ActiveNote.Kind = ntTree ))) then exit;
  result := TTreeNote( ActiveNote ).TV.Selected;
end; // GetCurrentTreeNode

procedure TreeNodeSelected( Node : TTreeNTNode );
var
  myTreeNote : TTreeNote;
  myNode : TNoteNode;
  {$IFDEF WITH_IE}
  NodeControl : TNodeControl;
  {$ENDIF}
begin
  with Form_Main do begin
      if ( not assigned( Node )) then exit;
      if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;

      myTreeNote := TTreeNote( ActiveNote );

      if ( not _Executing_History_Jump ) then
      begin
        AddHistoryLocation( myTreeNote );
      end;
      _LastMoveWasHistory := false;

      myTreeNote.TV.OnChange := nil;
      ActiveNote.Editor.OnChange := nil;

      ActiveNote.Editor.Lines.BeginUpdate;
      {
      if KeyOptions.FixScrollBars then
        ActiveNote.Editor.ScrollBars := low( TScrollStyle );
      }
      try
        try

          ActiveNote.EditorToDataStream;
          ActiveNote.Editor.Clear;
          ActiveNote.Editor.ClearUndo;

          myNode := TNoteNode( Node.Data );

          myTreeNote.SelectedNode := myNode;

          if assigned( myNode ) then
          begin

            case myNode.WordWrap of
              wwAsNote : ActiveNote.Editor.WordWrap := ActiveNote.WordWrap;
              wwYes : ActiveNote.Editor.WordWrap := true;
              wwno : ActiveNote.Editor.WordWrap := false;
            end;


            ActiveNote.DataStreamToEditor;
            if ( myNode.VirtualMode = vmNone ) then
            begin
              VirtualNodeUpdateMenu( false );
              if ( not EditorOptions.TrackStyle ) then
              begin
                if TreeOptions.ShowFullPath then
                  StatusBar.Panels[PANEL_HINT].Text := GetNodePath( Node, TreeOptions.NodeDelimiter, TreeOptions.PathTopToBottom ) // {N}
                else
                  StatusBar.Panels[PANEL_HINT].Text := Node.Text; // {N}
              end;
            end
            else
            begin
              VirtualNodeUpdateMenu( true );
              if ( not EditorOptions.TrackStyle ) then
                StatusBar.Panels[PANEL_HINT].Text := ' Virtual: ' + myNode.VirtualFN;
            end;
            TVCheckNode.Checked := myNode.Checked;
            TVBoldNode.Checked := myNode.Bold;
            TVChildrenCheckbox.Checked:= myNode.ChildrenCheckbox;   // [dpv]
            if myTreeNote.Checkboxes or (assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox)) then  // [dpv]
               TVCheckNode.Enabled := true
            else
               TVCheckNode.Enabled := false;

            TB_AlarmNode.Down:= (myNode.Alarm <> 0);        // [dpv]
            TVAlarmNode.Checked:= TB_AlarmNode.Down;        // [dpv]
          end
          else
          begin
            VirtualNodeUpdateMenu( false );
            if ( not EditorOptions.TrackStyle ) then
              StatusBar.Panels[PANEL_HINT].Text := '';
          end;
        except
          On E : Exception do
          begin
            TTreeNote( ActiveNote ).SelectedNode := nil;
            messagedlg( E.Message, mtError, [mbOK], 0 );
            exit;
          end;
        end;
      finally
        {
        if KeyOptions.FixScrollBars then
          ActiveNote.Editor.ScrollBars := ssBoth;
        }

        if ( _LoadedRichEditVersion > 2 ) then
        begin
          if ( _LastZoomValue <> 100 ) then
            SetEditorZoom( _LastZoomValue, '' );
        end;

        ActiveNote.Editor.Lines.EndUpdate;

        UpdateWordWrap;

        if KeyOptions.FixScrollBars then
          ActiveNote.Editor.Invalidate; // [x] [?]
        ActiveNote.Editor.Modified := false;
        RxRTFSelectionChange( ActiveNote.Editor );
        RxRTFChange( ActiveNote.Editor );
        ActiveNote.Editor.OnChange := RxRTFChange;
        TTreeNote( ActiveNote ).TV.OnChange := TVChange;
        UpdateHistoryCommands;
        // inc( __NodeChangeCounter );
        // statusbar.panels[0].text := Format( ' %d', [__NodeChangeCounter ]);

      end;
  end;

end; // TreeNodeSelected

procedure CreateMasterNode;
var
  myNode, nextnode, masternode : TTreeNTNode;
  myNote : TTreeNote;
begin
  if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  ActiveNote.FocusMemory := focTree;

  myNote := TTreeNote( ActiveNote );

  if ( myNote.TV.Items.Count > 0 ) then
    myNote.TV.Selected := myNote.TV.Items.GetFirstNode;

  masternode := TreeNoteNewNode( nil, tnInsertBefore, nil, '', true );
  if assigned( masternode ) then
  begin

    myNote.TV.Items.BeginUpdate;
    try
      myNode := masternode.GetNext;
      while assigned( myNode ) do
      begin
        nextnode := myNode.GetNextSibling;
        myNode.MoveTo( masternode, naAddChild );
        SelectIconForNode( myNode, myNote.IconKind );
        myNode := nextnode;
      end;
    finally
      SelectIconForNode( masternode, myNote.IconKind );
      myNote.TV.Items.EndUpdate;
      myNote.TV.Selected := masternode;
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
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
  myTNote : TTreeNote;
  myNoteNode : TNoteNode;
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
              if assigned( aNode ) then
              begin
                if StripNames then
                begin
                  result := aNode.Text;
                end
                else
                begin
                  p := pos( #32, aNode.Text );
                  if ( p > 1 ) then
                  begin
                    result := copy( aNode.Text, 1, pred( p ));
                  end;
                end;
              end;
            end; // ExtractNodeNumber

            procedure AddNumberToNode;
            var
              tmpstr : string;
              i, SpacePos : integer;
            begin
              if StripNames then
              begin
                myNoteNode.Name := LevelStr;
              end
              else
              begin
                case ExistingNumbers of
                  enNo : begin
                    myNoteNode.Name := LevelStr + #32 + myNoteNode.Name;
                  end;
                  enYes : begin
                    SpacePos := pos( #32, myNoteNode.Name );
                    if ( SpacePos > 0 ) then
                    begin
                      tmpstr := myNoteNode.Name;
                      delete( tmpstr, 1, SpacePos );
                      myNoteNode.Name := LevelStr + #32 + tmpstr;
                    end
                    else
                    begin
                      myNoteNode.Name := LevelStr;
                    end;
                  end;
                  enAuto : begin
                    // check if node name begins with a number
                    // and if so, strip it
                    SpacePos := -1; // flag: node has NO number
                    for i := 1 to length( myNoteNode.Name ) do
                    begin
                      if ( i = 1 ) then
                      begin
                        if ( myNoteNode.Name[1] in ['0'..'9'] ) then
                          SpacePos := 0 // flag: node HAS number
                        else
                          break; // node does NOT begin with a number
                      end
                      else
                      begin
                        if ( myNoteNode.Name[i] in ['0'..'9', '.'] ) then
                        begin
                          continue;
                        end
                        else
                        if ( myNoteNode.Name[i] = #32 ) then
                        begin
                          SpacePos := i;
                          break;
                        end
                        else
                        begin
                          SpacePos := pred( i );
                          break;
                        end;
                      end;
                    end;

                    if ( SpacePos < 0 ) then
                    begin
                      // node name does not have a number
                      if ( ModalResponse = mrOK ) then
                        myNoteNode.Name := LevelStr + #32 + myNoteNode.Name;
                    end
                    else
                    if ( SpacePos = 0 ) then
                    begin
                      // whole node name is a number
                      if ( ModalResponse = mrOK ) then
                        myNoteNode.Name := LevelStr;
                    end
                    else
                    begin
                      // node has a number followed by text
                      tmpstr := myNoteNode.Name;
                      delete( tmpstr, 1, SpacePos );
                      if ( ModalResponse = mrOK ) then
                        myNoteNode.Name := LevelStr + #32 + tmpstr
                      else
                        myNoteNode.Name := tmpstr;
                    end;
                  end;
                end;
              end;
              myTreeNode.Text := myNoteNode.Name;
            end; // AddNumberToNode

begin
  if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  myTNote := TTreeNote( ActiveNote );

  if ( myTNote.TV.Items.Count = 0 ) then exit;

  Form_NodeNum := TForm_NodeNum.Create( Form_Main );
  try
    ModalResponse := Form_NodeNum.ShowModal;
    if ( ModalResponse in [mrOK, mrYesToAll] ) then
    begin

      if ( ModalResponse = mrYesToAll ) then
      begin
        if ( messagedlg( 'Auto-detect and strip numbering from all nodes?', mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then
          exit;
      end;

      with Form_NodeNum do
      begin
        SubtreeOnly := ( RG_Scope.ItemIndex > 0 );
        if SubtreeOnly then
          StartNode := myTNote.TV.Selected
        else
          StartNode := myTNote.TV.Items.GetFirstNode;
        StripNames := ( RG_Method.ItemIndex > 0 );
        ExistingNumbers := TExistingNumbers( RG_CurNum.ItemIndex );
        StartNumber := Spin_StartNum.Value;
        if CB_FullDepth.Checked then
          DepthLimit := 0 // add numbers to all levels
        else
          DepthLimit := Spin_Depth.Value; // descend only DepthLimit levels
      end;

      if ( not assigned( StartNode )) then
      begin
        messagedlg( 'Initial node not assigned - select a node and retry.', mtError, [mbOK], 0 );
        exit;
      end;

      myTNote.TV.Items.BeginUpdate;
      try

        try

          myTreeNode := StartNode;
          StartNodeLevel := StartNode.Level;
          LastNodeLevel := StartNodeLevel;
          ThisNumber := StartNumber;
          LevelStr := '';

          if ( ModalResponse = mrYesToAll ) then
          begin
            StripNames := false;
            SubtreeOnly := false;
            ExistingNumbers := enAuto;
            while assigned( myTreeNode ) do
            begin
              myNoteNode := TNoteNode( myTreeNode.Data );
              AddNumberToNode;
              myTreeNode := myTreeNode.GetNext;
            end;
            exit;
          end;


          // first process starting level nodes,
          // because they need different treatment
          // (numbering starts with StartNumber and is not based on .index property)

          while assigned( myTreeNode ) do
          begin
            myNoteNode := TNoteNode( myTreeNode.Data );
            LevelStr := inttostr( ThisNumber );
            inc( ThisNumber );
            AddNumberToNode;

            if SubtreeOnly then
              break; // do not process sibling nodes, we only descend the subtree
            myTreeNode := myTreeNode.GetNextSibling;
          end;

          myTreeNode := StartNode;

          if ( DepthLimit <> 1 ) then // only if applying numbers more than 1 level deep
          begin

            StartNodeLevel := StartNode.Level; // return to original starting position
            // from now on, we only need to number nodes which are below the
            // initial numbering level and up to the max numbering level

            ParentLevelStr := '';
            LevelStr := '';
            ParentNode := nil;

            while assigned( myTreeNode ) do
            begin
              thisNodeLevel := myTreeNode.Level;
              myNoteNode := TNoteNode( myTreeNode.Data );

              if (( DepthLimit = 0 ) or ( thisNodeLevel < ( StartNodeLevel + DepthLimit ))) then
              begin
                if ( thisNodeLevel > StartNodeLevel ) then
                begin
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

              if SubtreeOnly then
              begin
                // bail out, we have finished the subtree
                if ( assigned( myTreeNode ) and ( myTreeNode.Level = StartNodeLevel )) then
                  break;
              end;
            end;

          end;

        except
          on E : Exception do
          begin
            messagedlg( e.message, mtError, [mbOK], 0 );
          end;
        end;

      finally
        myTNote.TV.Items.EndUpdate;
        NoteFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
    end;
  finally
    Form_NodeNum.Free;
  end;

end; // OutlineNumberNodes

procedure UpdateTreeChrome( const myNote : TTreeNote );
begin
  // myNote.TV.Items.BeginUpdate;
  try
    FontInfoToFont( myNote.TreeChrome.Font, myNote.TV.Font );
    myNote.TV.Color := myNote.TreeChrome.BGColor;
    {
    node := myNote.TV.Items.GetFirstNode;
    while assigned( node ) do
    begin
      // node.Font.Assign( myNote.TV.Font );
      node.ParentFont := true;
      node.ParentColor := true;
      // FontInfoToFont( myNote.TreeChrome.Font, node.Font );
      node := node.GetNext;
    end;
    }
  finally
    // myNote.TV.Items.EndUpdate;
    // myNote.TV.Invalidate;
  end;
end; // UpdateTreeChrome

procedure UpdateTreeOptions( const myNote : TTreeNote );
begin
  // updates options for current note's tree
  // based on global tree options

  with myNote.TV do
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
  myTNote : TTreeNote;
begin
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( ActiveNote.FocusMemory <> focTree ) then exit;
  if ( not assigned( MovingNode )) then // try to get tree's selected node...
    MovingNode := GetCurrentTreeNode;
  if ( not assigned( MovingNode )) then // still nothing, so bail out
    exit;

  s := '';
  t := 'cannot be ';
  myTNote := TTreeNote( ActiveNote );
  myTNote.TV.OnChange := nil;

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
          if assigned( theSibling ) then
          begin
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
          if assigned( theSibling ) then
          begin
            MovingNode.MoveTo( theSibling, naInsert );
            t := '';
          end;
        end;

        // LEFT promotes node 1 level up
        // RIGHT demotes node 1 level down
        dirLeft : begin
          if assigned( MovingNode.Parent ) then
          begin
            // becomes its parent's sibling
            MovingNode.Moveto( MovingNode.Parent, naInsert );
            t := '';
          end;
        end;
        dirRight : begin
          theSibling := MovingNode.GetPrevSibling;
          if assigned( theSibling ) then
          begin
            // becomes the last child of its previous sibling
            MovingNode.MoveTo( theSibling, naAddChild );
            t := '';
          end;
        end;
      end;


      if ( t = '' ) then // means node was successfully moved
      begin
        // update node icon
        SelectIconForNode( PreviousParent, myTNote.IconKind );
        SelectIconForNode( MovingNode, myTNote.IconKind );
        SelectIconForNode( MovingNode.Parent, myTNote.IconKind );
        myTNote.TV.Invalidate;
      end;

    except
      on E : Exception do
      begin
        messagedlg( 'Error moving node: ' + #13 + E.Message, mtError, [mbOK], 0 );
        s := 'Error moving node!';
      end;
    end;
  finally
    FNodeMoving:= false;       // [dpv]
    NoteFile.Modified := true;
    myTNote.TV.OnChange := Form_Main.TVChange;
    UpdateNoteFileState( [fscModified] );
    // {N}
    s := Format( 'Node "%s" %smoved %s', [MovingNode.Text,t,DIRECTION_NAMES[aDir]] );
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
    ActiveNote.Editor.SelText := s + ' ';
    ActiveNote.Editor.SelLength := 0;
  end
end; // CopyNodePath

procedure PasteNodeName( const PasteMode : TPasteNodeNameMode );
var
  myTreeNode : TTreeNTNode;
  myNewName : string[TREENODE_NAME_LENGTH];
  p : integer;
  s : string;
begin
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;

  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  myNewName := '';

  case PasteMode of
    pnnClipboard : begin
      myNewName := trim( FirstLineFromClipboard( TREENODE_NAME_LENGTH ));
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
      if ( ActiveNote.Editor.SelLength = 0 ) then
      begin
        Form_Main.ErrNoTextSelected;
        exit;
      end;
      s := ActiveNote.Editor.SelText;
      p := pos( #13, s );
      if ( p > 0 ) then
        delete( s, p, length( s ));
      myNewName := s;
      myNewName := trim( myNewName );
    end;
  end;

  if ( myNewName <> '' ) then
  begin
    try
      myTreeNode.Text := myNewName;  // {N}
      TNoteNode( myTreeNode.Data ).Name := myNewName;
    finally
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
    end;
  end;

end; // PasteNodeName

procedure ShowOrHideIcons( const tNote : TTreeNote; const UpdateNodes : boolean );
var
  myTreeNode : TTreeNTNode;
begin
  tNote.TV.Items.BeginUpdate;
  try
   with Form_Main do
   begin
    case tNote.IconKind of
      niNone : begin
        tNote.TV.Images := nil;
        MMViewNodeIcons.Checked := false;
        MMViewCustomIcons.Checked := false;
      end;
      niStandard : begin
        tNote.TV.Images := IMG_TV;
        MMViewNodeIcons.Checked := true;
        MMViewCustomIcons.Checked := false;
      end;
      niCustom : begin
        tNote.TV.Images := Chest.IMG_Categories;
        MMViewNodeIcons.Checked := false;
        MMViewCustomIcons.Checked := true;
      end;
    end;
    TVSelectNodeImage.Enabled := ( MMViewCustomIcons.Checked and MMViewCustomIcons.Enabled );
    if UpdateNodes then
    begin
      myTreeNode := tNote.TV.Items.GetFirstNode;
      while assigned( myTreeNode ) do
      begin
        SelectIconForNode( myTreeNode, tNote.IconKind );
        myTreeNode := myTreeNode.GetNext;
      end;
    end;
   end;
  finally
    tNote.TV.Items.EndUpdate;
  end;
end; // ShowOrHideIcons

procedure ShowOrHideCheckBoxes( const tNote : TTreeNote );
var
  node : TTreeNTNode;
  myNode : TNoteNode;
  enableCheck: boolean;
begin
  enableCheck:= false;
  if tNote.Checkboxes then                                 // [dpv]
     enableCheck:= true;

  node:= tNote.TV.Selected;
  if assigned(node) and assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox) then    // [dpv]
     enableCheck:= true;

 //Form_Main.TVCheckNode.Enabled := tNote.Checkboxes;  // [dpv]
   Form_Main.TVCheckNode.Enabled := enableCheck;       // [dpv]

  TNote.TV.Items.BeginUpdate;
  try
    if tNote.Checkboxes then
    begin
      TNote.TV.Items.TopLevelCheckType := ctCheckBox;
      node := TNote.TV.Items.GetFirstNode;
      while assigned( node ) do
      begin
        node.CheckType := ctCheckBox;
        myNode := TNoteNode( node.Data );
        if myNode.Checked then
          node.CheckState := csChecked
        else
          node.CheckState := csUnchecked;
        node := node.GetNext;
      end;
    end
    else
    begin
      TNote.TV.Items.TopLevelCheckType := ctNone;
      node := TNote.TV.Items.GetFirstNode;
      while assigned( node ) do
      begin
        ShowOrHideChildrenCheckBoxes (node);    // [dpv]
        //node.CheckType := ctNone;             // [dpv]
        node := node.GetNext;
      end;
    end;
  finally
    TNote.TV.Items.EndUpdate;
  end;
end; // ShowOrHideCheckBoxes

procedure ShowOrHideChildrenCheckBoxes( const tnode : TTreeNTNode );
var
  myNode : TNoteNode;
  node : TTreeNTNode;

begin

 try
  TNode.Owner.BeginUpdate;
  myNode := TNoteNode( tnode.Data );
  if myNode.ChildrenCheckbox then begin
    tNode.CheckType  := ctCheckBox;
    node := tnode.GetFirstChild;
    while assigned( node ) do
    begin
      myNode := TNoteNode( node.Data );
      if myNode.Checked then
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
  myNoteNode : TNoteNode;
  myTNote : TTreeNote;

    procedure ColorChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do
      begin
        if AsTextColor then
        begin
          myChildNode.Font.Color := myColor;
          with TNoteNode( myChildNode.Data ) do
          begin
            NodeColor := myColor;
            HasNodeColor := myHasThisColor;
          end;
        end
        else
        begin
          myChildNode.Color := myColor;
          with TNoteNode( myChildNode.Data ) do
          begin
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
  if (( not assigned( myTreeNode )) or Form_Main.NoteIsReadOnly( ActiveNote, false )) then exit;
  myTNote := TTreeNote( ActiveNote );
  myNoteNode := TNoteNode( myTreeNode.Data );
  try
    case AsTextColor of
      true : begin
        if UseColorDlg then
        begin
          if myNoteNode.HasNodeColor then
            myColor := myNoteNode.NodeColor
          else
            myColor := myTNote.TreeChrome.Font.Color;
          Form_Main.ColorDlg.Color := myColor;
          if Form_Main.ColorDlg.Execute then
            myColor := Form_Main.ColorDlg.Color
          else
            exit;
        end
        else
        begin
          if ResetDefault then
          begin
            myColor := myTNote.TreeChrome.Font.Color;
          end
          else
          begin
            myColor := Form_Main.TB_Color.ActiveColor;
          end;
        end;

        myHasThisColor := ( myColor <> myTNote.TreeChrome.Font.Color );
        myNoteNode.HasNodeColor := myHasThisColor;

        myNoteNode.NodeColor := myColor;
        myTreeNode.Font.Color := myColor;

      end;

      false : begin

        if UseColorDlg then
        begin
          if myNoteNode.HasNodeBGColor then
            myColor := myNoteNode.NodeBGColor
          else
            myColor := myTNote.TV.Color;
          with Form_Main do
          begin
            ColorDlg.Color := myColor;
            if ColorDlg.Execute then
              myColor := ColorDlg.Color
            else
              exit;
          end;
        end
        else
        begin
          if ResetDefault then
          begin
            myColor := myTNote.TV.Color;
          end
          else
          begin
            myColor := Form_Main.TB_Hilite.ActiveColor;
          end;
        end;

        myHasThisColor := ( myColor <> myTNote.TV.Color );
        myNoteNode.HasNodeBGColor := myHasThisColor;

        myNoteNode.NodeBGColor := myColor;
        myTreeNode.Color := myColor;
      end;
    end;

    if ( DoChildren and myTreeNode.HasChildren ) then
      ColorChildren( myTreeNode );

  finally
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;
end; // SetTreeNodeColor


procedure SetTreeNodeBold( const DoChildren : boolean );
var
  myNoteNode : TNoteNode;
  myTreeNode : TTreeNTNode;
  newStyle : TFontStyles;

    procedure BoldChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do
      begin
        myChildNode.Font.Style := newStyle;
        TNoteNode( myChildNode.Data ).Bold := myNoteNode.Bold;
        if myChildNode.HasChildren then
          BoldChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;
begin
  if (( not assigned( ActiveNote )) or Form_Main.NoteIsReadOnly( ActiveNote, false )) then exit;

  myNoteNode := GetCurrentNoteNode;
  if ( not assigned( myNoteNode )) then exit;
  if ( ActiveNote.FocusMemory <> focTree ) then exit;
  myTreeNode := TTreeNote( ActiveNote ).TV.Selected;
  if ( not assigned( myTreeNode )) then exit;

  myNoteNode.Bold := ( not myNoteNode.Bold );
  Form_Main.TVBoldNode.Checked := myNoteNode.Bold;

  if myNoteNode.Bold then
  begin
    newStyle := [fsBold];
    myTreeNode.Font.Style := newStyle;
  end
  else
  begin
    newStyle := [];
    myTreeNode.Font.Style := newStyle;
  end;

  if ( DoChildren and myTreeNode.HasChildren ) then
  begin
    BoldChildren( myTreeNode );
  end;

  NoteFile.Modified := true;
  UpdateNoteFileState( [fscModified] );
end; // SetTreeNodeBold


procedure SetTreeNodeCustomImage;
var
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  NewIdx, ImgIdx : integer;
  DoChildren : boolean;

    procedure SetChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do
      begin
        TNoteNode( myChildNode.Data ).ImageIndex := newIdx;
        SelectIconForNode( myChildNode, niCustom );
        if myChildNode.HasChildren then
          SetChildren( myChildNode ); // RECURSIVE CALL
        myChildNode := StartNode.GetNextChild( myChildNode );
      end;
    end;


begin
  myTreeNode := GetCurrentTreeNode;
  if (( not assigned( myTreeNode )) or Form_Main.NoteIsReadOnly( ActiveNote, false )) then exit;
  myNoteNode := TNoteNode( myTreeNode.Data );
  DoChildren := myTreeNode.HasChildren;
  ImgIdx := myNoteNode.ImageIndex;
  newIdx := PickImage( ImgIdx, DoChildren );
  if (( newIdx <> ImgIdx ) and ( newIdx <> -1 )) then
  begin
    try
      myNoteNode.ImageIndex := newIdx;
      SelectIconForNode( myTreeNode, niCustom );

      if DoChildren then
      begin
        SetChildren( myTreeNode );
      end;

    finally
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
    end;
  end;
end; // SetTreeNodeCustomImage

procedure SetTreeNodeFontFace( const ResetDefault, DoChildren : boolean );
var
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  myTNote : TTreeNote;
  myFontFace : string;

    procedure SetFontChildren( StartNode : TTreeNTNode );
    var
      myChildNode : TTreeNTNode;
    begin
      myChildNode := StartNode.GetFirstChild;
      while ( assigned( myChildNode ) and assigned( myChildNode.Data )) do
      begin

        myChildNode.Font.Name := myFontFace;

        with TNoteNode( myChildNode.Data ) do
        begin
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
  if (( not assigned( myTreeNode )) or Form_Main.NoteIsReadOnly( ActiveNote, false )) then exit;
  myTNote := TTreeNote( ActiveNote );
  myNoteNode := TNoteNode( myTreeNode.Data );

  try

    if ResetDefault then
    begin
      myFontFace := ttreenote( ActiveNote ).TreeChrome.Font.Name;
      myNoteNode.NodeFontFace := '';
    end
    else
    begin
      myFontFace := Form_Main.Combo_Font.FontName;
      myNoteNode.NodeFontFace := myFontFace;
    end;

    myTreeNode.Font.Name := myFontFace;

    if ( DoChildren and myTreeNode.HasChildren ) then
      SetFontChildren( myTreeNode );

  finally
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;


end; // SetTreeNodeFontFace

procedure SetTreeNodeFontSize( const ResetDefault, DoChildren : boolean );
var
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
  myTNote : TTreeNote;
begin
  myTreeNode := GetCurrentTreeNode;
  if (( not assigned( myTreeNode )) or Form_Main.NoteIsReadOnly( ActiveNote, false )) then exit;
  myTNote := TTreeNote( ActiveNote );
  myNoteNode := TNoteNode( myTreeNode.Data );

  try
    myTreeNode.Font.Size := strtoint( Form_Main.Combo_FontSize.Text );
    if ( myTreeNode.Font.Size >= ( ttreenote( ActiveNote ).tv.ItemHeight -4 )) then
      ttreenote( ActiveNote ).tv.ItemHeight := myTreeNode.Font.Size+6;
  except
  end;

end; // SetTreeNodeFontSize

function GetNodePath( aNode : TTreeNTNode; const aDelimiter : string; const TopToBottom : boolean ) : string;
var
  s : string;
  myNoteNode : TNoteNode;
begin
  result := '';

  while assigned( aNode ) do
  begin
    myNoteNode := TNoteNode( aNode.Data );

    if ( s = '' ) then
    begin
      s := myNoteNode.Name;
    end
    else
    begin
      case TopToBottom of
        false : begin
          s := s + aDelimiter + myNoteNode.Name;
        end;
        true : begin
          s := myNoteNode.Name + aDelimiter + s;
        end;
      end;
    end;
    aNode := aNode.Parent;
  end;

  result := s;

end; // GetNodePath

procedure SelectIconForNode( const myTreeNode : TTreeNTNode; const IconKind : TNodeIconKind );
begin
  if assigned( myTreeNode ) then
  begin
    case IconKind of
      niStandard : begin
        case TNoteNode( myTreeNode.Data ).VirtualMode of
          vmNone : begin
            if myTreeNode.HasChildren then
            begin
              if ( myTreeNode.Level > 0 ) then
                myTreeNode.ImageIndex := ICON_BOOK
              else
                myTreeNode.ImageIndex := ICON_FOLDER;
            end
            else
            begin
              myTreeNode.ImageIndex := ICON_NOTE;
            end;
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
        end;
        myTreeNode.SelectedIndex := succ( myTreeNode.ImageIndex );
      end;
      niCustom : begin
        myTreeNode.ImageIndex := TNoteNode( myTreeNode.Data ).ImageIndex;
        myTreeNode.SelectedIndex := myTreeNode.ImageIndex;
      end;
      niNone : begin
        myTreeNode.ImageIndex := -1;
        myTreeNode.SelectedIndex := -1;
      end;
    end;
  end;
end; // SelectIconForNode

procedure DeleteTreeNode( const DeleteFocusedNode : boolean );
const
  NOUNDO = #13#10 + 'This operation cannot be undone.';
var
  myTreeNode, myTreeParent, myTreeChild, myNextChild : TTreeNTNode;
  myTV : TTreeNT;
  myTNote : TTreeNote;
  myNoteNode : TNoteNode;
  KeepChildNodes : boolean;
begin
  with Form_Main do begin
      KeepChildNodes := true;
      if NoteIsReadOnly( ActiveNote, true ) then exit;
      myNoteNode := GetCurrentNoteNode;
      if ( not assigned( myNoteNode )) then exit;
      if ( ActiveNote.FocusMemory <> focTree ) then exit;
      myTNote := TTreeNote( ActiveNote );
      myTV := myTNote.TV;
      myTreeNode := myTV.Selected;
      myTreeParent := myTreeNode.Parent;

      if DeleteFocusedNode then
      begin
        // delete focused node and all its children, if any

        if myTreeNode.HasChildren then
        begin
          // ALWAYS warn if node has children
          case Application.MessageBox(
            PChar( Format( 'Node "%s" has %d child nodes. Delete these child nodes too?', [myTreeNode.Text, myTreeNode.Count] ) + NOUNDO ),
            'Warning',
              MB_YESNOCANCEL+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL ) of
            ID_YES : KeepChildNodes := false;
            ID_NO : KeepChildNodes := true;
            else
              exit;
          end;
        end
        else
        begin
          if TreeOptions.ConfirmNodeDelete then
          begin
            // {N}
            if ( messagedlg( Format( 'OK to delete node "%s"?', [myTreeNode.Text] ) + NOUNDO, mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
          end;
        end;

      end
      else
      begin
        // command was to delete CHILDREN of focused node
        if myTreeNode.HasChildren then
        begin
          if ( Application.MessageBox(
            // {N}
            PChar( Format( 'OK to delete %d CHILD NODES of node "%s"?', [myTreeNode.Count, myTreeNode.Text] ) + NOUNDO ),
            'Warning',
            MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES ) then exit;
         end
        else
        begin
          showmessage( 'Selected node has no children.' );
          exit;
        end;
      end;

      with myTV do
      begin
        OnChange := nil;
        OnDeletion := TVDeletion;
        Items.BeginUpdate;
      end;
      try
        try
          if DeleteFocusedNode then
          begin
            if KeepChildNodes then
            begin
              myTreeChild := myTreeNode.GetFirstChild;
              while assigned( myTreeChild ) do
              begin
                myNextChild := myTreeNode.GetNextChild( myTreeChild );
                myTreeChild.MoveTo( myTreeParent, naAddChild );
                SelectIconForNode( myTreeChild, myTNote.IconKind );
                myTreeChild := myNextChild;
              end;
            end;

            myTV.Items.Delete( myTreeNode );
            SelectIconForNode( myTreeParent, myTNote.IconKind );
          end
          else
          begin
            myTreeNode.DeleteChildren;
            SelectIconForNode( myTreeNode, myTNote.IconKind );
          end;
        except
          on E : Exception do
          begin
            messagedlg( 'Error deleting node: ' + #13 + E.Message, mtError, [mbOK], 0 );
          end;
        end;
      finally
        with myTV do
        begin
          OnDeletion := nil;
          OnChange := TVChange;
          Items.EndUpdate;
        end;
        if assigned( myTNote.TV.Selected ) then
          myTNote.SelectedNode := TNoteNode( myTNote.TV.Selected.Data )
        else
          myTNote.SelectedNode := nil;
        myTNote.DataStreamToEditor;
        NoteFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
  end;

end; // DeleteTreeNode

procedure UpdateTreeNode( const aTreeNode : TTreeNTNode );
var
  myNoteNode : TNoteNode;
begin
  if assigned( aTreeNode ) then
  begin
    myNoteNode := TNoteNode( aTreeNode.Data );
    if assigned( myNoteNode ) then
    begin

      // [x] FIXME: in many cases by doing this we are setting
      // the treenode text TWICE. In some cases this line is
      // necessary, though. Bad code design.
      aTreeNode.Text := myNoteNode.Name; // {N}

      if myNoteNode.Filtered then               // [dpv]
         aTreeNode.Hidden:= True;

      if myNoteNode.Bold then
        aTreeNode.Font.Style := [fsBold];

      if myNoteNode.ChildrenCheckbox then       // [dpv]
         aTreeNode.CheckType  := ctCheckBox
      else
         aTreeNode.CheckType  := ctNone;

      if myNoteNode.Checked then
        aTreeNode.CheckState := csChecked
      else
        aTreeNode.CheckState := csUnchecked;

      if myNoteNode.HasNodeFontFace then
        aTreeNode.Font.Name := myNoteNode.NodeFontFace;

      if myNoteNode.HasNodeColor then
        aTreeNode.Font.Color := myNoteNode.NodeColor;

      if myNoteNode.HasNodeBGColor then
        aTreeNode.Color := myNoteNode.NodeBGColor;
    end;
  end;
end; // UpdateTreeNode

procedure CreateNodefromSelection;
var
  myNode, SelectedNode : TTreeNTNode;
  myNote : TTreeNote;
  myNoteNode : TNoteNode;
  myRTFText, myNodeName : string;
  p : integer;
begin
  with Form_Main do begin
      if ( not ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ))) then exit;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      myNote := TTreeNote( ActiveNote );

      if ( myNote.Editor.SelLength = 0 ) then
      begin
        ErrNoTextSelected;
        exit;
      end;
  end;

  SelectedNode := myNote.TV.Selected;
  if ( not assigned( SelectedNode )) then
  begin
    Form_Main.Statusbar.Panels[PANEL_HINT].Text := ' No node is selected';
    exit;
  end;

  myNodeName := myNote.Editor.SelText;
  p := pos( #13, myNodeName );
  if ( p > 0 ) then
    myNodeName := copy( myNodeName, 1, pred( p ));
  p := length( myNodeName );
  if ( p > TREENODE_NAME_LENGTH ) then
    delete( myNodeName, succ( TREENODE_NAME_LENGTH ), p );
  myNodeName := trim( myNodeName );

  myRTFText := GetRichText( myNote.Editor, ( not myNote.PlainText ), true );

  myNode := TreeNoteNewNode( nil, tnAddAfter, nil, '', true );
  if assigned( myNode ) then
  begin
    myNote.TV.Items.BeginUpdate;
    try

      myNote.TV.Selected := myNode;
      myNoteNode := TNoteNode( MyNode.Data );

      if ( myNodeName <> '' ) then // can be blank
      begin
        myNoteNode.Name := myNodeName;
        myNode.Text := myNoteNode.Name;
      end;

      myNoteNode.Stream.Position := 0;
      myNoteNode.Stream.WriteBuffer( myRTFTExt[1], length( myRTFTExt ));
      myNote.SelectedNode := myNoteNode;
      myNote.DataStreamToEditor;

      myNode.MakeVisible;

    finally
      myNote.TV.Items.EndUpdate;
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
    end;
  end;

end; // CreateNodefromSelection

procedure NavigateInTree( NavDirection : TNavDirection );
var
  VKey : Word;
  SBVal, ScrollVal, ScrollMsg : integer;
begin

  if (( not assigned( ActiveNote )) or ( not ( ActiveNote.Kind = ntTree ))) then exit;

  VKey := 0;
  SBVal := SB_LINEDOWN;
  ScrollVal := SB_VERT;
  ScrollMsg := WM_VSCROLL;


  {
    If the tree has focus, Alt+arrow scrolls the editor;
    if the editor is focused, Alt+arrow scrolls the tree
  }

  if TTreeNote( ActiveNote ).TV.Focused then
  begin

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

    with ActiveNote.Editor do
    begin
      perform( ScrollMsg,
               MakeLong( SBVal, GetScrollPos( handle, ScrollVal )),
               0 );
      perform( ScrollMsg,
               MakeLong( SB_ENDSCROLL, GetScrollPos( handle, ScrollVal )),
               0 );
    end;
  end
  else
  begin
    case NavDirection of
      navUp : begin
        VKey := VK_UP;
      end;
      navDown : begin
        VKey := VK_DOWN;
      end;
      navLeft : begin
        VKey := VK_LEFT;
      end;
      navRight : begin
        VKey := VK_RIGHT;
      end;
    end;
    with TTreeNote( ActiveNote ).TV do
    begin
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
  // if ( ActiveNote.FocusMemory <> focTree ) then exit;

  if IncludeNoteText then
  begin
    // {N}
    ClipBoard.SetTextBuf( PChar( myTreeNode.Text + #13#13 + ActiveNote.Editor.Text ));
  end
  else
  begin
    // {N}
    ClipBoard.SetTextBuf( PChar( myTreeNode.Text ));
  end;
end; // CopyNodeName

function TreeTransferProc( const XferAction : integer; const PasteTargetNote : TTreeNote; const Prompt : boolean ) : boolean;
var
  newNoteNode : TNoteNode;
  myTreeNode, newTreeNode, LastNodeAssigned, FirstCopiedNode : TTreeNTNode;
  i, loop, PasteCount, StartLevel, LastLevel : integer;
  tNote : TTreeNote;
  VirtualNodesConverted : integer;
begin
  result := false;
  if ( XferAction = 2 ) then // clear
  begin
    result := true;
    if assigned( TransferNodes ) then
    begin
      if ( messagedlg( Format(
        'OK to forget %d copied nodes?',
        [TransferNodes.Count] ), mtConfirmation, [mbYes,mbNo], 0 ) = mrYes ) then
        TransferNodes.Free;
      TransferNodes := nil;
    end;
    exit;
  end;

  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  if ( PasteTargetNote = nil ) then
    myTreeNode := GetCurrentTreeNode
  else
    myTreeNode := PasteTargetNote.TV.Selected;

  if ( myTreeNode = nil ) then
  begin
    showmessage( 'No tree node available for copying or pasting data.' );
    exit;
  end;

  screen.Cursor := crHourGlass;
  try
    try

      case XferAction of
        0 : begin // COPY subtree

          ActiveNote.EditorToDataStream;

          if assigned( TransferNodes ) then
            TransferNodes.Free;
          TransferNodes := TNodeList.Create;
          StartLevel := myTreeNode.Level;
          while assigned( myTreeNode ) do
          begin

            newNoteNode := TNoteNode.Create;
            newNoteNode.Assign( TNoteNode( myTreeNode.Data ));
            newNoteNode.Level := myTreeNode.Level - StartLevel;
            TransferNodes.Add( newNoteNode );
            myTreeNode := myTreeNode.GetNext;
            if (( myTreeNode <> nil ) and ( myTreeNode.Level <= StartLevel )) then
              myTreeNode := nil; // end of subtree; break out of loop

            if ( TransferNodes.Count = 0 ) then
            begin
              showmessage( 'No nodes were copied.' );
              TransferNodes.Free;
              TransferNodes := nil;
            end
            else
            begin
              result := true;
              Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( ' %d nodes copied for transfer', [TransferNodes.Count] );
            end;
          end;
        end;

        1 : begin // PASTE subtree
          if ( not assigned( TransferNodes )) then
          begin
            showmessage( 'No data to paste. Select "Transfer|Copy Subtree" first.' );
            exit;
          end;

          if TransferNodes.HasVirtualNodes then
          begin
            if ( messagedlg(
              'One or more nodes being transferred is a Virtual Node. Each such node will be pasted as normal (non-virtual) node, if another virtual node in this file is already linked to the same file.' + #13#13 + 'Continue?',
              mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then
                exit;
          end
          else
          begin
          if Prompt then
            if ( messagedlg( Format(
              'OK to paste %d nodes below current node "%s"?',
              [TransferNodes.Count,myTreeNode.Text] ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then // {N}
                exit;
          end;

          NoteFile.Modified := true;

          tNote := TTreeNote( ActiveNote );
          LastNodeAssigned := myTreeNode;
          PasteCount := 0;
          VirtualNodesConverted := 0;
          FirstCopiedNode := nil;

          StartLevel := myTreeNode.Level;
          LastLevel := StartLevel+1;

          tNote.TV.Items.BeginUpdate;
          try
            for i := 0 to pred( TransferNodes.Count ) do
            begin

              newNoteNode := TNoteNode.Create;
              newNoteNode.Assign( TransferNodes[i] );

              if ( newNoteNode.VirtualMode <> vmNone ) then
              begin
                if NoteFile.HasVirtualNodeByFileName( newNoteNode, newNoteNode.VirtualFN ) then
                begin
                  inc( VirtualNodesConverted );
                  newNoteNode.VirtualMode := vmNone;
                  newNoteNode.VirtualFN := '';
                end;
              end;

              tNote.AddNode( newNoteNode );
              newNoteNode.Level := newNoteNode.Level + StartLevel + 1;

              if ( i = 0 ) then
              begin
                newTreeNode := tNote.TV.Items.AddChildFirst( myTreeNode, newNoteNode.Name );
                FirstCopiedNode := newTreeNode;
              end
              else
              begin
                case DoTrinaryCompare( newNoteNode.Level, LastLevel ) of
                  trinGreater : begin
                    newTreeNode := tNote.TV.Items.AddChild( LastNodeAssigned, newNoteNode.Name );
                  end;
                  trinEqual : begin
                    newTreeNode := tNote.TV.Items.Add( LastNodeAssigned, newNoteNode.Name );
                  end;
                  else
                  begin
                    for loop := 1 to (( LastLevel - newNoteNode.Level )) do
                      LastNodeAssigned := LastNodeAssigned.Parent;
                    newTreeNode := tNote.TV.Items.Add( LastNodeAssigned, newNoteNode.Name );
                  end;
                end;
              end;

              if assigned( newNoteNode ) then
              begin

                LastLevel := newTreeNode.Level;
                LastNodeAssigned := newTreeNode;
                newNoteNode.Level := newTreeNode.Level;

                newTreeNode.Data := newNoteNode;
                UpdateTreeNode( newTreeNode );

              end;

              inc( PasteCount );

            end;

            newTreeNode := myTreeNode;

            while assigned( newTreeNode ) do
            begin
              SelectIconForNode( newTreeNode, tNote.IconKind );
                newTreeNode := newTreeNode.GetNext;
              if (( newTreeNode <> nil ) and ( newTreeNode.Level <= StartLevel )) then
                newTreeNode := nil; // end of subtree; break out of loop
            end;
            result := true;
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( ' Pasted %d nodes', [PasteCount] );
            if assigned( FirstCopiedNode ) then
            begin
              FirstCopiedNode.Collapse( true );
              FirstCopiedNode.MakeVisible;
              // tNote.TV.Selected := FirstCopiedNode;
            end;
          finally
            tNote.TV.Items.EndUpdate;
            // myTreeNode.Expand( true );
            if ( VirtualNodesConverted > 0 ) then
            begin
              showmessage( Format(
                '%d virtual nodes have been converted to normal nodes, because other virtual nodes in current file already link to the same files.',
                [VirtualNodesConverted]
              ));
            end;
          end;
        end;
      end;

    except
      on E : Exception do
      begin
        showmessage( E.Message );
      end;
    end;

  finally
    screen.Cursor := crDefault;
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;

end; // TreeTransferProc


procedure HideCheckedNodes (note: TTreeNote);    // [dpv]
var
  Node : TTreeNTNode;
begin
    note.TV.Items.BeginUpdate;

    Node := note.TV.Items.GetFirstNode;
    while assigned( Node ) do begin // go through all nodes
      if Node.CheckState =  csChecked then
         Node.Hidden := True;
      Node := Node.GetNext; // select next node to search
    end;

    note.TV.Items.EndUpdate;
end;

procedure ShowCheckedNodes (note: TTreeNote);    // [dpv]
var
  Node : TTreeNTNode;
begin
  note.TV.Items.BeginUpdate;
  Node := note.TV.Items.GetFirstNode;
  while Node <> nil do begin
    if not TNoteNode(Node.Data).Filtered then
       Node.Hidden := False;
    Node := Node.GetNext;
  end;
  note.TV.Items.EndUpdate;
end;

procedure MarkAllFiltered (note: TTreeNote);  // [dpv]
var
  Node: TTreeNTNode;
begin
  if not assigned(note) then exit;

  Node := note.TV.Items.GetFirstNode;
  while Node <> nil do begin
    TNoteNode(Node.Data).Filtered := true;
    Node := Node.GetNext;
  end;
end;

procedure MarkAllUnfiltered (note: TTreeNote);  // [dpv]
var
  Node: TTreeNTNode;
begin
  if not assigned(note) then exit;

  Node := note.TV.Items.GetFirstNode;
  while Node <> nil do begin
    Node.Font.Color:= TNoteNode(Node.Data).NodeColor;
    TNoteNode(Node.Data).Filtered := false;
    Node := Node.GetNext;
  end;
end;

procedure RemoveFilter (note: TTreeNote);       // [dpv]
begin
    if not assigned(note) then exit;

    Note.TV.Items.BeginUpdate;
    MarkAllUnfiltered (note);
    note.TV.FullNotHidden;
    if Note.HideCheckedNodes  then
       HideCheckedNodes (note);

    Note.TV.Items.EndUpdate;
end;

procedure HideFilteredNodes (note: TTreeNote);  // [dpv]
var
  Node : TTreeNTNode;
begin
  note.TV.Items.BeginUpdate;
  Node := note.TV.Items.GetFirstNode;
  while Node <> nil do begin
    if TNoteNode(Node.Data).Filtered then
       Node.Hidden:= true
    else begin
       Node.MakeVisibilityPosible;
       Node.Font.Color := clBlue;
    end;
    Node := Node.GetNext;
  end;
  note.TV.Items.EndUpdate;
end;

function NoteOfNode (node: TTreeNTNode): TTreeNote;       // [dpv]
var
   tab: TObject;
   i: Integer;
   note: TTabNote;
begin
   Result:= nil;
   tab:= node.Owner.Owner.Owner;
   for i := 0 to NoteFile.NoteCount - 1 do begin
       note := TTabNote( Form_Main.Pages.Pages[i].PrimaryObject );
       if note.TabSheet = tab then begin
          Result:= TTreeNote(note);
          break;
       end;
   end;
end;


initialization
  __NodeChangeCounter := 0;
  FNodeMoving:= false;     // [dpv]
  TransferNodes := nil;
  _LAST_NODE_SELECTED := nil;
  DraggedTreeNode := nil;
  _OLD_NODE_NAME := DEFAULT_NEW_NODE_NAME;

end.
