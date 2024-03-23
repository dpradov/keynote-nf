unit kn_VCLControlsMng;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface
uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.RichEdit,
   Winapi.ShellAPI,
   Winapi.MMSystem,
   System.SysUtils,
   System.Classes,
   System.IniFiles,
   Vcl.Controls,
   Vcl.ComCtrls,
   Vcl.Graphics,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Menus,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,

   ComCtrls95,
   TB97Ctls,
   TreeNT,

   kn_NoteObj
;



    // dynamically create and destroy controls (note tabs, RichEdits, trees, etc)
    procedure SetUpVCLControls( aFolder : TKntFolder ); // sets up VCL controls for note (events, menus, etc - only stuff that is handled in this unit, not stuff that TTabNote handles internally)
    procedure CreateVCLControls; // creates VCL controls for ALL notes in KntFile object
    procedure SetupAndShowVCLControls;
    procedure CreateVCLControlsForNote( const aFolder : TKntFolder ); // creates VCL controls for specified note
    procedure DestroyVCLControls; // destroys VCL controls for ALL notes in KntFile object
    procedure DestroyVCLControlsForNote( const aFolder : TKntFolder; const KillTabSheet : boolean ); // destroys VCL contols for specified note
    procedure GetOrSetNodeExpandState( const aTV : TTreeNT; const AsSet, TopLevelOnly : boolean );

    // VCL updates when config loaded or changed
    procedure UpdateFormState;
    procedure UpdateTabState;
    procedure UpdateNoteDisplay;
    procedure UpdateCursorPos;

    procedure UpdateResPanelState;
    procedure SetResPanelPosition;
    procedure HideOrShowResPanel( const DoShow : boolean );
    procedure UpdateResPanelContents (ChangedVisibility: boolean);
    procedure LoadResScratchFile;
    procedure StoreResScratchFile;
    function CheckResourcePanelVisible( const DoWarn : boolean ) : boolean;
    procedure RecreateResourcePanel;
    procedure FocusResourcePanel;

    procedure SelectStatusbarGlyph( const HaveNoteFile : boolean );
    procedure SetFilenameInStatusbar(const FN : string );
    procedure SetStatusbarGlyph(const Value: TPicture);
    procedure FocusActiveNote;
    procedure SortTabs;

    procedure LoadTrayIcon( const UseAltIcon : boolean );
    procedure LoadTabImages( const ForceReload : boolean );
{$IFDEF KNT_DEBUG}
    procedure SaveMenusAndButtons;
{$ENDIF}

    procedure EnableCopyFormat(value: Boolean);

var
  SBGlyph: TPicture;


implementation
uses
   MRUFList,
   RxRichEd,
   gf_misc,
   gf_files,
   gf_strings,
   gf_miscvcl,
   kn_Global,
   kn_Main,
   kn_Info,
   kn_Const,
   kn_NodeList,
   kn_Macro,
   kn_Chest,
   kn_EditorUtils,
   kn_TreeNoteMng,
   kn_MacroMng,
   kn_PluginsMng,
   kn_FavoritesMng,
   kn_TemplateMng,
   kn_NoteFileMng;



resourcestring
  STR_00 = 'Click and drag to resize panels (Ctrl: tree max width / Alt: Toggle fixed)';
  STR_01 = 'Error destroying tabsheet ';
  STR_02 = 'Select text color';
  STR_03 = 'Select &Highlight...';
  STR_04 = 'Select highlight color';
  STR_05 = 'Apply current font color to text';
  STR_06 = 'Apply &Highlight';
  STR_07 = 'Apply current highlight color to text';
  STR_08 = 'Minimize application';
  STR_09 = 'Exit application';
  STR_10 = ' L %d / %d  C %d';
  STR_11 = ' Sel: %d  W: %d';
  STR_12 = 'Hide &Resource Panel';
  STR_13 = 'Show &Resource Panel';
  STR_14 = 'The Resource panel must be visible to use this command. Show the Resource panel now?';
  STR_15 = 'Use the mouse to apply the %s to another text or press Esc to cancel';
  STR_16 = 'paragraph formatting';
  STR_17 = 'font formatting';



//=================================================================
// SetUpVCLControls
//=================================================================
procedure SetUpVCLControls( aFolder : TKntFolder );
begin
  with Form_Main do begin
      FindAllResults.OnSelectionChange:= RxFindAllResultsSelectionChange;
      FindAllResults.ShowHint:= False;
      FindAllResults.AutoURLDetect:= False;

      if assigned( aFolder.Editor ) then
      begin

        with aFolder.Editor do
        begin
          PopUpMenu := Menu_RTF;
          OnChange := RxRTFChange;
          // OnEnter := RxRTFEnter;
          // OnExit := RxRTFExit;
          OnKeyDown := RxRTFKeyDown;
          OnKeyPress := RxRTFKeyPress;
          OnMouseDown := RxRTFMouseDown;
          OnMouseUp := RxRTFMouseUp;
          OnProtectChange := RxRTFProtectChange;
          OnProtectChangeEx := RxRTFProtectChangeEx;
          OnSelectionChange := RxRTFSelectionChange;
          OnURLClick := RxRTFURLClick;
          OnMouseMove := RTFMouseMove;
          OnDblClick:= RxRTFDblClick;
          OnEnter:= RxRTFEnter;
          {
            By default DragMode=dmManual, and the mechanism in TRxRichEdit is controlled through the IRichEditOleCallback interface.
            (see comment *3 in RxRichEd.pas)
          //OnDragOver := RxRTFDragOver;
          //DragMode := dmManual; // dmAutomatic;
          OnStartDrag := RxRTFStartDrag;
          OnEndDrag := RxRTFEndDrag;
          }
          OnStartDrag := RxRTFStartDrag;    // See comment *4 in RxRichEd
          OnEndDrag := RxRTFEndDrag;        // ,,

          OnFileDropped := Form_Main.OnFileDropped;

          // AllowObjects := true;                      // Should be assigned when creating the control, to not lead to recreate its window
          // AllowInPlace := false;
          HelpContext:= 282;  // KeyNote Editor [282]
        end;
      end;

      // enable "advanced typography options" for the richedit;
      // this gives us full justification and other goodies.
      if ( _LoadedRichEditVersion > 2 ) then
        SendMessage( aFolder.Editor.Handle, EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY );

      with aFolder.TV do
      begin
        PopupMenu := Menu_TV;
        OnKeyDown := TVKeyDown;
        OnKeyPress := TVKeyPress;
        OnChange := TVChange;
        // OnChanging := TVChanging; // unused
        OnChecked := TVChecked;
        OnFileDropped := Form_Main.OnFileDropped;
        OnEditing := TVEditing;
        OnEdited := TVEdited;
        OnEditCanceled := TVEditCanceled;
        // OnDeletion := TVDeletion;
        // OnExit := TVExit;
        OnDeletion := nil;
        OnClick := TVClick;
        OnDblClick := TVDblClick;
        //OnMouseDown := TVMouseDown;
        OnDragDrop := TVDragDrop;
        OnDragOver := TVDragOver;
        OnEndDrag := TVEndDrag;
        OnStartDrag := TVStartDrag;
        ShowHint := false;
        OnHint := TVOnHint;
        OnEnter:= TVEnter;
        OnSavingTree:= TVSavingTree;
        OnMouseMove:= TVMouseMove;
        HelpContext:= 284;  // Tree-type Notes [284]
      end;

      SetEditorZoom( aFolder.Editor, DefaultEditorProperties.DefaultZoom, '' );
      _LastZoomValue:= DefaultEditorProperties.DefaultZoom;
  end;

end; // SetUpVCLControls

//=================================================================
// CreateVCLControls
//=================================================================
procedure CreateVCLControls;
// creates all VCL controls for a newly loaded Notes file
var
  i : integer;
  myFolder : TKntFolder;
begin
  with Form_Main do begin
      if (( not assigned( KntFile )) or ( KntFile.Folders.Count = 0 )) then exit;

      for i := 0 to pred( KntFile.Folders.Count ) do begin
         myFolder := KntFile.Folders[i];
         CreateVCLControlsForNote( myFolder );
      end;

  end;

end; // CreateVCLControls

//=================================================================
// ShowVCLControls
//=================================================================
procedure SetupAndShowVCLControls;
// Finalize setup and visualization of all VCL controls for a newly loaded Notes file
var
  i : integer;
  myFolder : TKntFolder;
begin
  with Form_Main do begin
        if (( not assigned( KntFile )) or ( KntFile.Folders.Count = 0 )) then exit;

        try

          for i := 0 to pred( KntFile.Folders.Count ) do begin
            myFolder := KntFile.Folders[i];
            myFolder.DataStreamToEditor;
            SetUpVCLControls( myFolder );
          end;

        finally

          // show all tabs (they were created hidden)
          if ( Pages.PageCount > 0 ) then
            for i := 0 to pred( Pages.PageCount ) do
                Pages.Pages[i].TabVisible := true;
        end;

        // restore the note that was active when file was previously saved
        if (( KntFile.ActiveFolderID >= 0 ) and ( KntFile.ActiveFolderID < KntFile.Folders.Count )) then begin
          if ( Pages.PageCount > 0 ) then
            Pages.ActivePage := Pages.Pages[KntFile.ActiveFolderID];
        end
        else
           if ( Pages.PageCount > 0 ) then
              Pages.ActivePage := Pages.Pages[0];
  end;

end; // SetupAndShowVCLControls

//=================================================================
// GetOrSetNodeExpandState
//=================================================================
procedure GetOrSetNodeExpandState( const aTV : TTreeNT; const AsSet, TopLevelOnly : boolean );
var
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
begin
  with Form_Main do begin
        // set or get node "expanded" state

        if ( not assigned( aTV )) then exit;
        myTreeNode := aTV.Items.GetFirstNode;
        while assigned( myTreeNode ) do
        begin
          myNote := TKntNote( myTreeNode.Data );
          if assigned( myNote ) then
          begin
            case AsSet of
              true : begin // set
                if TopLevelOnly then
                begin
                  myTreeNode.Expand( false );
                  myTreeNode := myTreeNode.GetNextSibling
                end
                else
                begin
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
  end;
end; // GetOrSetNodeExpandState

//=================================================================
// CreateVCLControlsForNote
//=================================================================
procedure CreateVCLControlsForNote( const aFolder : TKntFolder );
var
  myTab : TTab95Sheet;
  myEditor : TTabRichEdit;
  // FolderID : string[3];
  myTree : TTreeNT;
  mySplitter : TSplitter;
  myFolder : TKntFolder;
  i, loop : integer;
  tNode, myTreeNode, LastTreeNodeAssigned : TTreeNTNode;
  LastNodeLevel: integer;
  j, numChilds, AuxLevel, ChildLevel : integer;
  myNote: TKntNote;
  {$IFDEF WITH_IE}
  myPanel : TPanel;
  myBrowser : TWebBrowser;
  {$ENDIF}
  TVFontStyleWithBold : TFontStyles;

begin
  Log_StoreTick( 'CreateVCLControlsForNote - Begin', 2, +1);

  with Form_Main do begin
        _ALLOW_VCL_UPDATES := false;
        myFolder := nil;
        {$IFDEF WITH_IE}
        myPanel := nil;
        {$ENDIF}

        try
          if ( aFolder.FocusMemory = focNil ) then
          begin
            aFolder.FocusMemory := focTree;
          end;

          if ( aFolder.TabSheet = nil ) then
          begin
            myTab := TTab95Sheet.Create( Form_Main );
            with myTab do
            begin
              // Visible := false;
              TabVisible := false; // hide tabs initially
              Parent := Pages;
              PageControl := Pages;
              // name := 'Tab' + FolderID;

              if _TABS_ARE_DRAGGABLE then
              begin
                Dragable := true;
                FloatOnTop := false;
              end;

            end;
            aFolder.TabSheet := myTab;
          end
          else
          begin
            myTab := aFolder.TabSheet;
          end;

          myFolder := aFolder;
          // [f] myFolder.FocusMemory := focTree;

          myTree := TTreeNT.Create( myTab );
          with myTree do
          begin

            Parent := myTab;
            if myFolder.VerticalLayout then
              Align := alTop
            else
              Align := alLeft;

            HelpContext := 120;

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

            if myFolder.VerticalLayout then
            begin
              if (( myFolder.TreeWidth < 30 ) or
                  ( myFolder.TreeWidth > ( Pages.Height - 30 ))) then
                Height := ( Pages.Height DIV 3 )
              else
                Height := myFolder.TreeWidth;
              myFolder.TreeWidth := Height; // store corrected value
            end
            else
            begin
              if (( myFolder.TreeWidth < 30 ) or
                  ( myFolder.TreeWidth > ( Pages.Width - 30 ))) then
                Width := ( Pages.Width DIV 3 )
              else
                Width := myFolder.TreeWidth;
              myFolder.TreeWidth := Width; // store corrected value
            end;
          end;

          mySplitter := TSplitter.Create( myTab );
          with mySplitter do
          begin
            Parent := myTab;
            Align := alNone;
            mySplitter.OnMoved:= Form_Main.SplitterNoteMoved;
            if myFolder.VerticalLayout then
            begin
              Top := myTree.Height + 5;
              Align := alTop;
              Cursor := crVSplit;
              Height := 3;
            end
            else
            begin
              Left := myTree.Width + 5;
              Align := alLeft;
              Cursor := crHSplit;
              Width := 4;
            end;
            Hint := STR_00;
          end;

          myFolder.Splitter := mySplitter;

          myFolder.TV := myTree;
          UpdateTreeOptions( myFolder );

          myFolder.UpdateTree; // do this BEFORE creating nodes

          // Create TreeNodes for all nodes in the note
          LastTreeNodeAssigned := nil;
          LastNodeLevel := 0;

          if ( myFolder.Nodes.Count > 0 ) then begin
            TVFontStyleWithBold:= myFolder.TV.Font.Style + [fsBold];

            myTree.Items.BeginUpdate;
            try
               for i := 0 to myFolder.Nodes.Count-1 do begin
                  myNote := myFolder.Nodes[i];

                  numChilds:= 0;
                  ChildLevel:= myNote.Level+1;
                  for j := i+1 to myFolder.Nodes.Count-1 do begin
                     AuxLevel:= myFolder.Nodes[j].Level;
                     if AuxLevel = ChildLevel then
                        inc(numChilds);
                     if AuxLevel < ChildLevel then
                        break;
                  end;

                  case myNote.Level of
                    0 : begin
                      myTreeNode := myTree.Items.Add( nil, myNote.Name, numChilds );
                      LastNodeLevel := 0;
                    end
                    else begin
                      case DoTrinaryCompare( myNote.Level, LastNodeLevel ) of
                        trinGreater:
                          myTreeNode := myTree.Items.AddChild( LastTreeNodeAssigned, myNote.Name );
                        trinEqual:
                          myTreeNode := myTree.Items.AddChild( LastTreeNodeAssigned.Parent, myNote.Name );
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
                           myTreeNode := myTree.Items.Add( LastTreeNodeAssigned, myNote.Name, numChilds );
                        end;
                      end;
                    end;
                  end;

                  LastTreeNodeAssigned := myTreeNode;
                  LastNodeLevel := myNote.Level;

                  myTreeNode.Data := myNote;

                  if myNote.HasNodeFontFace then
                    myTreeNode.Font.Name := myNote.NodeFontFace;

                  if myNote.Bold then
                    myTreeNode.Font.Style := TVFontStyleWithBold;

                  if myNote.HasNodeColor then
                    myTreeNode.Font.Color := myNote.NodeColor;

                  if myNote.HasNodeBGColor then
                    myTreeNode.Color := myNote.NodeBGColor;

                  if myNote.Filtered  then      // [dpv]
                     myFolder.Filtered := True;

               end;

            finally
              Log_StoreTick( 'After created TreeNodes', 3 );

              ShowOrHideIcons( myFolder, true );
              ShowOrHideCheckBoxes( myFolder );

              Log_StoreTick( 'After ShowOrHIdeIcons,CheckBoxes', 3 );

              if myFolder.Filtered then             // [dpv]
                 HideFilteredNodes (myFolder);
              if myFolder.HideCheckedNodes then     // [dpv]
                 HideChildNodesUponCheckState (myFolder, nil, csChecked);

              myTree.Items.EndUpdate;

              Log_StoreTick( 'After HideFilteredNodes, HideCheckNodes', 3 );
            end;

            // restore selected node: this block must be
            // OUTSIDE the beginupdate..endupdate range

            //if ( myTree.Items.Count > 0 ) then       // [dpv]
            if ( myTree.Items.CountNotHidden > 0 ) then
            begin
              if (( TreeOptions.ExpandMode <> txmFullCollapse ) and // SaveActiveNode and
                 ( myFolder.OldSelectedIndex >= 0 ) and
                 ( myFolder.OldSelectedIndex < myTree.Items.Count )) then
              begin
                // restore the node which was selected when file was saved
                tNode:= myTree.Items[myFolder.OldSelectedIndex];
                if tNode.Hidden  then begin  // [dpv]
                   tNode := myTree.Items.GetFirstNode;
                   if tNode.Hidden then tNode:= tNode.GetNextNotHidden;
                end;
              end
              else
              begin
                tNode := myTree.Items.GetFirstNode;
                if tNode.Hidden then tNode:= tNode.GetNextNotHidden;
              end;
              myTree.Selected:= tNode;
              myFolder.SelectedNode := TKntNote( myTree.Selected.Data );
            end
            else
            begin
              myFolder.SelectedNode := nil;
            end;

            Log_StoreTick( 'After Restored selected node', 3 );


            case TreeOptions.ExpandMode of
              txmFullCollapse : begin
                // nothing
              end;
              txmActiveNode : begin
                if assigned( myTree.Selected ) then
                  myTree.Selected.Expand( false );
              end;
              txmTopLevelOnly, txmExact : begin
                try
                  GetOrSetNodeExpandState( myTree, true, ( TreeOptions.ExpandMode = txmTopLevelOnly ));
                except
                  // nothing
                end;
              end;
              txmFullExpand : begin
                myTree.FullExpand;
              end;
            end;


            UpdateTreeVisible( myFolder ); // [f]

            if assigned( myTree.Selected ) then
              myTree.Selected.MakeVisible;

            Log_StoreTick( 'After UpdateTreeVisible', 3 );
          end;

         {$IFDEF WITH_IE}
           myPanel := TPanel.Create( myTab );
           with myPanel do
           begin
            parent := myTab;
            Align := alClient;
            Caption := '';
            ParentFont := false;
            BevelInner := bvNone;
            BevelOuter := bvNone;
            BorderWidth := 1; // [?]
            Visible := true;
           end;

           myFolder.MainPanel := myPanel;

           if _IE4Available then
           begin
           myBrowser := TWebBrowser.Create( myPanel );
           TControl( myBrowser ).Parent := myPanel;
           with myBrowser do
           begin
            Align := alClient;
            Visible := false;
           end;
           myFolder.WebBrowser := myBrowser;
           end
           else
           begin
            myFolder.WebBrowser := nil;
           end;

         {$ENDIF}


          myEditor := TTabRichEdit.Create( myTab );
          with myEditor do
          begin
            {$IFDEF WITH_IE}
            if assigned( myPanel ) then
              Parent := myPanel
            else
              Parent := myTab;
            {$ELSE}
            Parent := myTab;
            {$ENDIF}

            Align := alClient;
            // name := 'RTF' + FolderID;
            // HelpContext := 100;
            HelpContext := 10;
            MaxLength := 0; // unlimited text size
            ParentFont := false;
            WantTabs := true;
            WantReturns := true;
            NoteObj := aFolder;
            AllowInPlace := true;
            AllowObjects := true;
            AutoVerbMenu := true;
            HideSelection := false;
            SelectionBar := true;
            UndoLimit := EditorOptions.UndoLimit;
            WordSelection := EditorOptions.WordSelect;
            RecreateWndProtect := KeyOptions.RichEditv3;
            LangOptions := [];
            if EditorOptions.AutoKeyboard then
              LangOptions := LangOptions + [rlAutoKeyboard];
            if EditorOptions.AutoFont then
              LangOptions := LangOptions + [rlAutoFont];
            ScrollBars := ssBoth;
          end;

          aFolder.Editor := myEditor;
          with myTab do
          begin
            // PrimaryControl := myEditor;
            PrimaryObject := aFolder;
          end;

          Log_StoreTick( 'After Created TTabRichEdit', 3 );

          with aFolder do begin
            UpdateTabSheet;
            UpdateEditor; // do this BEFORE placing RTF text in editor
          end;

        finally
          _ALLOW_VCL_UPDATES := true;
        end;
  end;

  Log_StoreTick( 'CreateVCLControlsForNote - End', 3, -1 );

end; // CreateVCLControlsForNote


procedure DeleteNodes(Folder: TKntFolder);
var
  Node : TTreeNTNode;
begin
    Node := Folder.TV.Items.GetFirstNode;
    while assigned( Node ) do begin // go through all nodes
        KntFile.ManageMirrorNodes(3, Node, nil);
        Node := Node.GetNext; // select next node to search
    end;
end;

//=================================================================
// DestroyVCLControlsForNote
//=================================================================
procedure DestroyVCLControlsForNote( const aFolder : TKntFolder; const KillTabSheet : boolean );
begin
  with Form_Main do begin
        if not assigned( aFolder ) then exit;

        _ALLOW_VCL_UPDATES := false;
        try

          if assigned( aFolder.Editor ) then
          begin
            with aFolder.Editor do
            begin
              OnChange := nil;
              OnSelectionChange := nil;
              OnProtectChange := nil;
              OnEnter := nil;
              OnExit := nil;
              OnKeyDown := nil;
              Free;
            end;
            aFolder.Editor := nil;
          end;

          if assigned( aFolder.Splitter ) then
          begin
            aFolder.Splitter.Free;
            aFolder.Splitter := nil;
          end;
          if assigned( aFolder.TV ) then
          begin
            with aFolder.TV do
            begin
              PopupMenu := nil;
              OnChange := nil;
              OnChanging := nil;
              OnDeletion := nil;
              OnEditing := nil;
              OnEdited := nil;
              OnEnter := nil;
              OnExit := nil;
              OnKeyDown := nil;
              OnEditCanceled := nil;
              OnEdited := nil;
              OnEditing := nil;
            end;
            DeleteNodes(aFolder);

            aFolder.TV.Free;
            aFolder.TV := nil;
          end;

          if ( KillTabSheet and assigned( aFolder.TabSheet )) then
          begin
            aFolder.TabSheet.Free;
          end;
        finally
          _ALLOW_VCL_UPDATES := true;
        end;
  end;
end; // DestroyVCLControlsForNote

//=================================================================
// DestroyVCLControls
//=================================================================
procedure DestroyVCLControls;
var
  i : integer;
  s : string;
  // aFolder : TTabNote;
begin
  with Form_Main do begin
        {
        if (( not assigned( KntFile )) or ( KntFile.Notes.Count = 0 )) then exit;
        for i := 0 to pred( KntFile.Notes.Count ) do
        begin
          DestroyVCLControlsForNote( KntFile.Notes[i]; );
        end;
        }

          if ( pages.pagecount > 0 ) then
          begin
            for i := pred( pages.pagecount ) downto 0 do
            begin
                {
                aFolder := TTabNote( pages.pages[i].PrimaryObject );
                try
                  aFolder.Editor := nil;
                  if ( aFolder.Kind = ntTree ) then
                  begin
                    aFolder.TV := nil;
                    aFolder.Splitter := nil;
                  end;
                except
                  on E : Exception do
                  begin
                    showmessage( 'Error while killing note controls ' + pages.pages[i].Caption + #13#13 +
                     E.Message );
                  end;
                end;
                }
                try
                  s := pages.pages[i].Caption;
                  pages.pages[i].Free;
                except
                  on E : Exception do
                  begin
                    showmessage( STR_01 + s + #13#13 +
                     E.Message );
                  end;
                end;
            end;
          end;
  end;

end; // DestroyVCLControls


//=================================================================
// UpdateFormState
//=================================================================
procedure UpdateFormState;
var
   aux: integer;
begin
  with Form_Main do begin
        _SAVE_RESTORE_CARETPOS := EditorOptions.SaveCaretPos;

        Combo_Font.DropDownCount := KeyOptions.ComboDropDownCount;
        Combo_FontSize.DropDownCount := KeyOptions.ComboDropDownCount;
        Combo_Style.DropDownCount := KeyOptions.ComboDropDownCount;
        Combo_ResFind.DropDownCount := KeyOptions.ComboDropDownCount;
        Combo_Zoom.DropDownCount := KeyOptions.ComboDropDownCount;

        with KeyOptions do
        begin

          // apply settings to VCL stuff

          if ColorDlgBig then
            ColorDlg.Options := [cdFullOpen,cdSolidColor,cdAnyColor]
          else
            ColorDlg.Options := [cdSolidColor,cdAnyColor];

          if LongCombos then
          begin
            Combo_Font.Width := OriginalComboLen + ( OriginalComboLen DIV 4 );
            Combo_Style.Width := Combo_Font.Width;
          end
          else
          begin
            if ( ComboFontLen > MIN_COMBO_LENGTH ) then begin  //***  Directamente da un error interno al compilar �??
               //Combo_Font.Width := ComboFontLen;
                 aux:= ComboFontLen;
                 Combo_Font.Width := aux;
            end;

            if ( ComboStyleLen > MIN_COMBO_LENGTH ) then begin
              //Combo_Style.Width := ComboStyleLen;
              aux:= ComboStyleLen;
              Combo_Style.Width:= aux;
            end;
          end;

          // [style]
          if StyleShowSamples then
            Combo_Style.ItemHeight := 19
          else
            Combo_Style.ItemHeight := 16;


          if EditorOptions.TrackStyle then
          begin
            case EditorOptions.TrackStyleRange of
              srFont : MMViewFormatFont.Checked := true;
              srParagraph : MMViewFormatPara.Checked := true;
              srBoth : MMViewFormatBoth.Checked := true;
            end;
          end
          else
          begin
            MMViewFormatNone.Checked := true;
          end;

          if UseOldColorDlg then
          begin
            MMFormatTextColor.Hint := STR_02;
            MMFormatHighlight.Caption := STR_03;
            MMFormatHighlight.Hint := STR_04;
          end
          else
          begin
            MMFormatTextColor.Hint := STR_05;
            MMFormatHighlight.Caption := STR_06;
            MMFormatHighlight.Hint := STR_07;
          end;

          AppLastActiveTime := now;
          if ( TimerMinimize or TimerClose ) then
            Form_Main.AssignOnMessage
          else
            Application.OnMessage := nil;

          ShowHint := ShowTooltips;
          TrayIcon.Active := UseTray;
          AutoSaveToggled;

          Toolbar_Main.Visible := ToolbarMainShow;
          MMViewTBMain.Checked := ToolbarMainShow;

          Toolbar_Format.Visible := ToolbarFormatShow;
          MMViewTBFormat.Checked := ToolbarFormatShow;

          Toolbar_Insert.Visible := ToolbarInsertShow;
          MMViewTBInsert.Checked := ToolbarInsertShow;

          Toolbar_Macro.Visible := true; // ToolbarMacroShow;
          MMToolsMacroRun.Enabled := Toolbar_Macro.Visible;

          MMViewTBTree.Checked := ToolbarTreeShow;

          Toolbar_Style.Visible := ToolbarStyleShow;
          MMViewTBStyle.Checked := ToolbarStyleShow;

          { // Removed TB_Exit button
          if MinimizeOnClose then
            TB_Exit.Hint := STR_08
          else
            TB_Exit.Hint := STR_09;
          }

        end;

        if KeyOptions.MRUUse then
        begin
          with KeyOptions do
          begin
            MRU.Maximum := MRUCount;
            MRU.AutoSave := true;
            MRU.UseSubmenu := MRUSubmenu;
            if MRUFullPaths then
              MRU.MRUDisplay := mdFullPath
            else
              MRU.MRUDisplay := mdFileNameExt;
          end;
        end
        else
        begin
          with KeyOptions do
          begin
           MRU.Maximum := 0;
           MRU.RemoveAllItems;
           MRU.AutoSave := false;
          end;
        end;

        CB_ResFind_CaseSens.Checked := FindOptions.MatchCase;
        CB_ResFind_WholeWords.Checked := FindOptions.WholeWordsOnly;
        CB_ResFind_AllNotes.Checked := FindOptions.AllTabs;
        CB_ResFind_CurrentNodeAndSubtree.Checked := FindOptions.CurrentNodeAndSubtree;
        CB_ResFind_CurrentNodeAndSubtree.Enabled:= not FindOptions.AllTabs;
        RG_ResFind_Scope.ItemIndex := ord( FindOptions.SearchScope );
        RG_ResFind_Type.ItemIndex := ord( FindOptions.SearchMode );
        RG_ResFind_ChkMode.ItemIndex := ord( FindOptions.CheckMode );

        PlainDefaultPaste_Toggled;
  end;


end; // UpdateFormState

//=================================================================
// UpdateTabState
//=================================================================
procedure UpdateTabState;
begin
  with Form_Main do begin
        Pages.ButtonStyle := false;
        Pages.AllowTabShifting := true;
        Pages.HotTrack := TabOptions.HotTrack;

        if ( TabOptions.Images and MMViewTabIcons.Enabled ) then
          Pages.Images := Chest.IMG_Categories
        else
          Pages.Images := nil;
        MMViewTabIcons.Checked := TabOptions.Images;


        // update these settings only if Initializing,
        // ie before we have any notes loaded. This is
        // to prevent loss of RTF text formatting when
        // tabsheets are recreated. Changes made to these
        // settings will take effect after restarting KeyNote.
        if ( Initializing or ( not KeyOptions.RichEditv3 )) then
        begin
          case TabOptions.TabOrientation of
            tabposTop : begin
              Pages.TabPosition := tpTopLeft;
              Pages.VerticalTabs := false;
              Pages.TextRotation := trHorizontal;
            end;
            tabposBottom : begin
              Pages.TabPosition := tpBottomRight;
              Pages.VerticalTabs := false;
              Pages.TextRotation := trHorizontal;
            end;
            tabposLeft : begin
              Pages.TabPosition := tpTopLeft;
              Pages.VerticalTabs := true;
              Pages.TextRotation := trVertical;
            end;
            tabposRight : begin
              Pages.TabPosition := tpBottomRight;
              Pages.VerticalTabs := true;
              Pages.TextRotation := trVertical;
            end;
          end;
          Pages.MultiLine := TabOptions.Stacked;
          Splitter_ResMoved( Splitter_Res );
        end;

        with Pages.Font do
        begin
          Name := TabOptions.Font.FName;
          Size := TabOptions.Font.FSize;
          Color := TabOptions.Font.FColor;
          Style := TabOptions.Font.FStyle;
          Charset := TabOptions.Font.FCharset;
        end;
        with Pages.TabInactiveFont do
        begin
          Name := TabOptions.Font.FName;
          Size := TabOptions.Font.FSize;
          if TabOptions.ColorAllTabs then
            Color := TabOptions.Font.FColor
          else
            Color := clWindowText;
          Style := [];
          Charset := TabOptions.Font.FCharset;
        end;

        Pages.Color := TabOptions.ActiveColor;
        Pages.TabInactiveColor := TabOptions.InactiveColor;
  end;

end; // UpdateTabState

//=================================================================
// UpdateNoteDisplay
//=================================================================
procedure UpdateNoteDisplay;
var
  s : string;
  myFolder : TKntFolder;
  Node: TTreeNTNode;

begin
  with Form_Main do begin
        s := '';
        if assigned( ActiveKntFolder ) then
        begin
          try
            MMNoteReadOnly.Checked := ActiveKntFolder.ReadOnly;
            TB_ClipCap.Down := ( KntFile.ClipCapFolder = ActiveKntFolder );
            MMNoteClipCapture.Checked := TB_ClipCap.Down;
            ShowAlarmStatus;

            TMClipCap.Checked := MMNoteClipCapture.Checked;

            SetMargins();
            UpdateWordWrap;

            if EditorOptions.WordCountTrack then begin
               if ActiveKntFolder.Editor.TextLength < 2000 then
                  UpdateWordCount
               else
                  CleanWordCount;   // Lo actualizaremos cuando se dispare el Timer, si corresponde
            end;

            // if isWordWrap then s := ' W' else s := ' ';

            RxRTFChange( ActiveKntFolder.Editor );
            RxRTFSelectionChange( ActiveKntFolder.Editor );
            if ActiveKntFolder.ReadOnly then s := 'R';

            if ( _LoadedRichEditVersion > 2 ) then
            begin
              _LastZoomValue := GetEditorZoom;
              Combo_Zoom.Text := Format('%d%%', [_LastZoomValue] );
            end;

            UpdateShowImagesState;

            ShowInsMode;


            myFolder := ActiveKntFolder;
            MMTree_.Visible := true;
            {MMViewTree.Visible := true;}
            MMViewTree.Enabled := true;
            MMViewTree.Checked := myFolder.TV.Visible;
            {MMViewNodeIcons.Visible := true;
            MMViewCustomIcons.Visible := true;
            MMViewCheckboxes.Visible := true;}
            MMViewNodeIcons.Checked := myFolder.IconKind = niStandard;
            MMViewCustomIcons.Checked := myFolder.IconKind = niCustom;
            MMEditPasteAsNewNode.Visible := true;
            MMP_PasteAsNode.Visible := true;
            MMViewCheckboxesAllNodes.Checked := ActiveKntFolder.Checkboxes;
            //TVCheckNode.Enabled := MMViewCheckboxesAllNodes.Checked;              // [dpv]
            node:= myFolder.TV.Selected;
            if myFolder.Checkboxes or (assigned(node) and assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox)) then  // [dpv]
               TVCheckNode.Enabled := true
            else
               TVCheckNode.Enabled := false;


            TVChildrenCheckbox.Enabled := not MMViewCheckboxesAllNodes.Checked;       // [dpv]
            Toolbar_Tree.Visible := KeyOptions.ToolbarTreeShow;
            MMViewNodeIcons.Enabled := MMViewTree.Checked;
            MMViewCustomIcons.Enabled := MMViewTree.Checked;
            MMViewCheckboxesAllNodes.Enabled := MMViewTree.Checked;
            MMViewCustomIcons.Enabled := MMViewTree.Checked;
            TVSelectNodeImage.Enabled := ( MMViewCustomIcons.Checked and MMViewCustomIcons.Enabled );
            MMViewHideCheckedNodes.Enabled := true;                      // [dpv]
            MMViewHideCheckedNodes.Checked:= myFolder.HideCheckedNodes;   // [dpv]
            TB_HideChecked.Down := MMViewHideCheckedNodes.Checked;       // [dpv]
            TB_FilterTree.Down:= myFolder.Filtered;                       // [dpv]
            MMViewFilterTree.Enabled := true;                            // [dpv]
            MMViewFilterTree.Checked :=  myFolder.Filtered;               // [dpv]
            if myFolder.Filtered then FilterApplied (myFolder) else FilterRemoved (myFolder);   // [dpv]


            if MMAlternativeMargins.Checked then
               ActiveKntFolder.Editor.Refresh;

          except
            // showmessage( 'BUG: error in UpdateNoteDisplay' );
          end;
        end
        else
        begin
          MMNoteReadonly.Checked := false;
          MMFormatWordWrap.Checked := false;
          MMSetAlarm.Checked:= false;
          TB_WordWrap.Down := false;
          RTFMWordwrap.Checked := false;
          TB_ClipCap.Down := false;
          MMNoteClipCapture.Checked := false;
          TMClipCap.Checked := false;
          s := '';
          StatusBar.Panels[PANEL_INS].Text := '';

          MMTree_.Visible := false;
          Toolbar_Tree.Visible := false;

        end;
        StatusBar.Panels[PANEL_NOTEINFO].Text := s;
  end;

end; // UpdateNoteDisplay

//=================================================================
// UpdateCursorPos
//=================================================================
procedure UpdateCursorPos;
var
  p : TPoint;
  SelectedVisibleText: string;
  SelLength: integer;
begin
  with Form_Main do begin
       if assigned( ActiveKntFolder ) then begin
          SelLength:= ActiveKntFolder.Editor.SelLength;
          if ( SelLength > 0 ) then ShowingSelectionInformation:= true;

          if EditorOptions.TrackCaretPos then begin
             p := ActiveKntFolder.Editor.CaretPos;
             if ( SelLength = 0 ) then begin
                StatusBar.Panels[PANEL_CARETPOS].Text :=
                    Format( STR_10, [succ( p.y ), ActiveKntFolder.Editor.Lines.Count, succ( p.x )] );
             end
             else begin
                SelectedVisibleText:= ActiveKntFolder.Editor.SelVisibleText;
                StatusBar.Panels[PANEL_CARETPOS].Text :=
                    Format( STR_11, [Length(SelectedVisibleText), GetWordCount( SelectedVisibleText )] );
             end;
          end
          else begin
            if EditorOptions.WordCountTrack  then begin
               if ( SelLength > 0 ) then
                  UpdateWordCount
               else
                  if ShowingSelectionInformation then
                     CheckWordCount(true);
            end else
              StatusBar.Panels[PANEL_CARETPOS].Text := '';
          end;

          if ( SelLength = 0 ) then
             ShowingSelectionInformation:= false;


          if EditorOptions.TrackStyle then begin
            case EditorOptions.TrackStyleRange of
              srFont : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveKntFolder.Editor.FontInfoString;
              end;
              srParagraph : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveKntFolder.Editor.ParaInfoString;
              end;
              srBoth : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveKntFolder.Editor.FontInfoString +
                ActiveKntFolder.Editor.ParaInfoString;
              end;
            end;
          end;

          TB_EditCut.Enabled := ( SelLength > 0 );

          TB_EditCopy.Enabled := TB_EditCut.Enabled;
          MMEditCut.Enabled := TB_EditCut.Enabled;
          MMEditCopy.Enabled := TB_EditCut.Enabled;
          RTFMCut.Enabled := TB_EditCut.Enabled;
          RTFMCopy.Enabled := TB_EditCut.Enabled;

       end
       else begin
          StatusBar.Panels[PANEL_CARETPOS].Text := '';
          StatusBar.Panels[PANEL_HINT].Text := '';
       end;
  end;

end; // UpdateCursorPos


//======================
//      ResPanel
//======================

procedure HideOrShowResPanel( const DoShow : boolean );
begin
  with Form_Main do begin
      if ( DoShow = Pages_Res.Visible ) then exit;

      try
        if KeyOptions.ResPanelLeft then begin
           Splitter_Res.Visible := DoShow;
           Pages_Res.Visible := DoShow;
           end
        else begin
           Pages_Res.Visible := DoShow;
           Splitter_Res.Visible := DoShow;
        end;

      finally
        // must redraw editor, otherwise it displays garbage
        if assigned( ActiveKntFolder ) then
          ActiveKntFolder.Editor.Invalidate;
      end;
  end;

end; // HideOrShowResPanel

procedure SetResPanelPosition;
begin
  with Form_Main do begin
      if ( KeyOptions.ResPanelLeft and ( Splitter_Res.Align = alLeft )) or
         (( not KeyOptions.ResPanelLeft ) and ( Splitter_Res.Align = alRight )) then
        exit;

      // these settings must be applied
      // in order.
      // Will be much easier in D5.

      case KeyOptions.ResPanelLeft of
        false : begin
          Pages.Align := alNone;
          Splitter_Res.Align := alNone;
          Pages_Res.Align := alRight;
          Pages.Align := alClient;
          Splitter_Res.Align := alRight;
        end;
        true : begin
          Pages.Align := alNone;
          Splitter_Res.Align := alNone;
          Pages_Res.Align := alLeft;
          Pages.Align := alClient;
          Splitter_Res.Align := alLeft;
        end;
      end;
  end;
end; // SetResPanelPosition

procedure UpdateResPanelState;
begin
  with Form_Main do begin
        with Pages_Res do
        begin
          Images := nil;
          ButtonStyle := false;
          // AllowTabShifting := false;
          HotTrack := TabOptions.HotTrack;

          // update these settings only if Initializing,
          // ie before we have any notes loaded. This is
          // to prevent loss of RTF text formatting when
          // tabsheets are recreated
          if Initializing then
          begin

            SetResPanelPosition;

            ResMPluginTabClick( nil ); // with nil, settings will not be changed, but tabs will be shown or hidden (normally this is called by TMenuItem)

            MultiLine := ResPanelOptions.Stacked;

            case ResPanelOptions.TabOrientation of
              tabposTop : begin
                TabPosition := tpTopLeft;
                VerticalTabs := false;
                TextRotation := trHorizontal;
              end;
              tabposBottom : begin
                TabPosition := tpBottomRight;
                VerticalTabs := false;
                TextRotation := trHorizontal;
              end;
              tabposLeft : begin
                TabPosition := tpTopLeft;
                VerticalTabs := true;
                TextRotation := trVertical;
              end;
              tabposRight : begin
                TabPosition := tpBottomRight;
                VerticalTabs := true;
                TextRotation := trVertical;
              end;
            end;

            {
            // custom draw for List_ResFind
            if ResPanelOptions.ColorFindList then
            begin
              List_ResFind.Style := lbOwnerDrawFixed;
              List_ResFind.OnDrawItem := List_ResFindDrawItem;
              List_ResFind.ItemHeight := ScaleFromSmallFontsDimension(List_ResFind.ItemHeight);   // http://stackoverflow.com/questions/8296784/how-do-i-make-my-gui-behave-well-when-windows-font-scaling-is-greater-than-100
            end;
            }

            // add history to combo
            DelimTextToStrs( Combo_ResFind.Items, FindOptions.FindAllHistory, HISTORY_SEPARATOR );
            CB_ResFind_AllNotes.Checked := FindOptions.AllTabs;
          end;

          Font.Assign( Form_Main.Pages.Font );
          TabInactiveFont.Assign( Form_Main.Pages.TabInactiveFont );
          Color := TabOptions.ActiveColor;
          TabInactiveColor := TabOptions.InactiveColor;
        end;

        MMViewResPanel.Checked := KeyOptions.ResPanelShow;
        if KeyOptions.ResPanelShow then
          ResMHidepanel.Caption := STR_12
        else
          ResMHidepanel.Caption := STR_13;
        TB_ResPanel.Down := MMViewResPanel.Checked;
  end;

end; // UpdateResPanelState

procedure UpdateResPanelContents (ChangedVisibility: boolean);
begin
  // General idea: do not load all resource panel information
  // when KeyNote starts. Instead, load data only when
  // a tab is viewed, if the tab contains no data. For example,
  // when user clicks the Macros tab and the list of macros
  // is empty, we load he macros.

  with Form_Main do begin
      if KeyOptions.ResPanelShow then begin

        if ChangedVisibility and KeyOptions.ResPanelActiveUpdate then begin
          if Res_RTF.Modified then
             StoreResScratchFile;

          Res_RTF.Clear;

          // if a macro file was copied to macros folder while KeyNote is running, simply hiding and then
          // showing the resourc2e panel will load the new macro (press F9 twice)
          ListBox_ResMacro.Items.Clear;
          ClearMacroList;

          ListBox_ResFav.Items.Clear;

          // clear list of templates
          ListBox_ResTpl.Items.Clear;

          // List of plugns does NOT get cleared, because it takes a long time to initialize.
          // Once loaded, the lisy remains available even after the resource panel is hidden. To reload
          // the list of current plugins, use the "Reload plugins" menu command.
          { ListBox_ResPlugins.Items.Clear; }
        end;

        MMToolsPluginRun.Enabled := ResTab_Plugins.TabVisible;
        MMToolsMacroRun.Enabled := ResTab_Macro.TabVisible;

        if ( Pages_Res.ActivePage = ResTab_Find ) then begin
          // nothing to do
        end
        else
        if ( Pages_Res.ActivePage = ResTab_RTF ) then begin
          if ( Res_RTF.Lines.Count = 0 ) then
            LoadResScratchFile;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Macro ) then begin
          // load macros
          if ( ListBox_ResMacro.Items.Count = 0 ) then
            EnumerateMacros;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Template ) then begin
          // load templates
          if ( ListBox_ResTpl.Items.Count = 0 ) then
            LoadTemplateList;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Plugins ) then begin
          if ( ListBox_ResPlugins.Items.Count = 0 ) then
            DisplayPlugins;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Favorites ) then begin
          if ( ListBox_ResFav.Items.Count = 0 ) then
            DisplayFavorites;
        end;

      end
      else begin
        MMToolsPluginRun.Enabled := false;
        MMToolsMacroRun.Enabled := false;

        try
          if Res_RTF.Modified then
            StoreResScratchFile;
        except
        end;
      end;
  end;

end; // UpdateResPanelContents

procedure RecreateResourcePanel;
begin
  with Form_Main do begin
      KeyOptions.ResPanelShow := false;
      Pages_ResChange( Pages_Res );

      try

        with Pages_Res do
        begin
          MultiLine := ResPanelOptions.Stacked;

          case ResPanelOptions.TabOrientation of
            tabposTop : begin
              TabPosition := tpTopLeft;
              VerticalTabs := false;
              TextRotation := trHorizontal;
            end;
            tabposBottom : begin
              TabPosition := tpBottomRight;
              VerticalTabs := false;
              TextRotation := trHorizontal;
            end;
            tabposLeft : begin
              TabPosition := tpTopLeft;
              VerticalTabs := true;
              TextRotation := trVertical;
            end;
            tabposRight : begin
              TabPosition := tpBottomRight;
              VerticalTabs := true;
              TextRotation := trVertical;
            end;
          end;
          Splitter_ResMoved( Splitter_Res );
        end;

      finally
        KeyOptions.ResPanelShow := true;
        Pages_ResChange( Pages_Res );
      end;
  end;
end; // RecreateResourcePanel

procedure FocusResourcePanel;
begin
  with Form_Main do begin
      if Pages_Res.Visible then
      begin
        try
          if ( Pages_Res.ActivePage = ResTab_Find ) then
          begin
            Combo_ResFind.SetFocus;
          end
          else
          if ( Pages_Res.ActivePage = ResTab_RTF ) then
          begin
            Res_RTF.SetFocus;
          end
          else
          if ( Pages_Res.ActivePage = ResTab_Macro ) then
          begin
            ListBox_ResMacro.SetFocus;
          end
          else
          if ( Pages_Res.ActivePage = ResTab_Template ) then
          begin
            ListBox_ResTpl.SetFocus;
          end
          else
          if ( Pages_Res.ActivePage = ResTab_Plugins ) then
          begin
            ListBox_ResPlugins.SetFocus;
          end
          else
          if ( Pages_Res.ActivePage = ResTab_Favorites ) then
          begin
            ListBox_ResFav.SetFocus;
          end;
        except
          // nothing
        end;
      end;
  end;
end; // FocusResourcePanel

function CheckResourcePanelVisible( const DoWarn : boolean ) : boolean;
begin
  result := Form_Main.Pages_Res.Visible;
  if ( not result ) then
  begin
    if DoWarn then
    begin
      case messagedlg(STR_14, mtConfirmation, [mbYes,mbNo], 0 ) of
        mrYes : begin
          Form_Main.MMViewResPanelClick( Form_Main.MMViewResPanel );
        end;
      end;
    end;
    exit;
  end;
end; // CheckResourcePanelVisible


procedure LoadResScratchFile;
begin
  with Form_Main do begin
      if fileexists( Scratch_FN ) then
      begin
        Res_RTF.Lines.BeginUpdate;
        try
          try
            Res_RTF.Lines.LoadFromFile( Scratch_FN );
          except
          end;
        finally
          Res_RTF.Lines.EndUpdate;
        end;
      end;
  end;
end; // LoadResScratchFile

procedure StoreResScratchFile;
begin
  try
    Form_Main.Res_RTF.Lines.SaveToFile( Scratch_FN );
  except
    // may throw exception e.g. if file is locked by another app,
    // we don't worry about scratchpad errors
  end;
end; // StoreResScratchFile


procedure SortTabs;
var
  p, i, j : integer;
  namei, namej : string;
begin
  with Form_Main do begin
        if ( Pages.PageCount < 2 ) then exit;
        Pages.ActivePage := Pages.Pages[0];
        Pages.Enabled := false;
        try

          for i := 0 to pred( Pages.PageCount ) do
          begin
            for j := succ( i ) to pred( Pages.PageCount ) do
            begin
              namei := pages.Pages[i].Caption;
              namej := pages.Pages[j].Caption;
              p := pos( '&', namei );
              if p > 0 then
                delete( namei, p, 1 );
              p := pos( '&', namej );
              if p > 0 then
                delete( namej, p, 1 );
              if ( ansicomparetext( namei, namej ) > 0 ) then
              begin
                Pages.Pages[j].PageIndex := i;
              end;
            end;
          end;

          // must reassign images, because they get lost on sort
          if ( Pages.Images <> nil ) then
          begin
            for i := 0 to pred( Pages.PageCount ) do
            begin
              Pages.Pages[i].ImageIndex := TKntFolder(Pages.Pages[i].PrimaryObject).ImageIndex;
            end;
          end;

        finally
          Pages.Enabled := true;
          Pages.ActivePage := ActiveKntFolder.TabSheet;
          KntFile.Modified := true;
          UpdateNoteDisplay;
          UpdateNoteFileState( [fscModified] );
        end;
  end;

end; // SortTabs

procedure FocusActiveNote;
begin
    try
      if ( assigned( ActiveKntFolder ) and ( not Initializing )) then
      begin
         if ( ActiveKntFolder.TV.Visible and ( ActiveKntFolder.FocusMemory = focTree )) then
           ActiveKntFolder.TV.SetFocus
         else
           ActiveKntFolder.Editor.SetFocus;
      end;
    except
      // Mostly Harmless
    end;
end; // FocusActiveNote


procedure SetStatusbarGlyph(const Value: TPicture);
begin
   SBGlyph.Assign(Value);

   // This way we force that panel to be updated (and only that panel).
   // If we used .Repaint, .Refresh, .Invalidate or .Update on the StatusBar, it would be updated too,
   // but all the panels would be affected, and also (don't know why), text panel would show bold, until form resized..
   Form_Main.StatusBar.Panels[PANEL_FILEICON].Width:= 10;

end;

procedure SetFilenameInStatusbar(const FN : string );
begin
   with Form_Main.StatusBar do begin
      Panels[PANEL_FILENAME].Text:= FN;
      Panels[PANEL_FILENAME].Width := Canvas.TextWidth(Panels[PANEL_FILENAME].Text) + 8;
   end;
end;

procedure SelectStatusbarGlyph( const HaveNoteFile : boolean );
var
  Glyph : TPicture;
  Index: integer;
begin

  // indicate file state or special program activity
  // with a cute little icon on statusbar

  Glyph := TPicture.Create;
  try

    Index:= NODEIMG_BLANK;

    if HaveNoteFile then
    begin

      if IsRecordingMacro then
        Index:= NODEIMG_MACROREC
      else
      if IsRunningMacro then
        Index:= NODEIMG_MACRORUN
      else
      if KntFile.ReadOnly then
      begin
        case KntFile.FileFormat of
          nffKeyNote :   Index:= NODEIMG_TKN_RO;
          nffKeyNoteZip: Index:= NODEIMG_TKNZIP_RO;
          nffEncrypted : Index:= NODEIMG_ENC_RO;
{$IFDEF WITH_DART}
          nffDartNotes : Index:= NODEIMG_DART_RO;
{$ENDIF}
        end;
      end
      else
      begin
        case KntFile.FileFormat of
          nffKeyNote :   Index:= NODEIMG_TKN;
          nffKeyNoteZip: Index:= NODEIMG_TKNZIP;
          nffEncrypted : Index:= NODEIMG_ENC;
{$IFDEF WITH_DART}
          nffDartNotes : Index:= NODEIMG_DART;
{$ENDIF}
        end;
      end;

    end;

    Chest.MGRImages.GetBitmap( Index, Glyph.Bitmap );
    SetStatusbarGlyph(Glyph);

  finally
    Glyph.Free;
  end;
end; // SelectStatusbarGlyph


procedure LoadTrayIcon( const UseAltIcon : boolean );
var
  Icon: TIcon;
  UseIcon: integer;
  IconFN: string;
begin
{
 If Application.MainFormOnTaskBar=True -> we need to change the icon of the main form
 If it is False -> we need to change the icon of the Application object

 Application.MainFormOnTaskbar= False by default. If we need to set True
   -> After Application.Initialize and before the main form creation
}

  with Form_Main do begin
      UseIcon:= 0;

      if assigned( KntFile ) then begin
         if UseAltIcon then
            UseIcon:= 1        // we're capturing clipboard, so indicate this by using the alternate (orange) tray icon
         else
         if ( KntFile.TrayIconFN <> '' ) then begin
            IconFN:= GetAbsolutePath(KntFile.File_Path, KntFile.TrayIconFN);
            if FileExists( IconFN ) then
               try
                 TrayIcon.Icon.LoadFromFile( IconFN );
                 UseIcon:= 2;
               except
               end
            else
               KntFile.TrayIconFN := '';
         end
      end;

      if UseIcon in [0, 1] then
         TrayIcon.Icon:= TrayIcon.Icons[UseIcon];

      Application.Icon:= TrayIcon.Icon;
      Application.ProcessMessages;
      sleep( 100 );                        // <- Important. Without this line, the icon of the main window changes, but the icon in the taskbar doesn't
  end;


end; // LoadTrayIcon


procedure LoadTabImages( const ForceReload : boolean );
// Typically, we only reload the tab icon file if necessary, i.e.
// if the required set of icons is different from the already loaded
// set. ForceReload tells us to reload anyway.
var
  LoadSuccess : boolean;
begin
  LoadSuccess := false;
  if assigned( KntFile ) then
  begin
    if (( _LOADED_ICON_FILE <> KntFile.TabIconsFN ) or ForceReload ) then
    begin
      if ( KntFile.TabIconsFN = '' ) then // means: use KeyNote default
      begin
        LoadSuccess := LoadCategoryBitmapsUser( ICN_FN );
      end
      else
      begin
        if ( KntFile.TabIconsFN <> _NF_Icons_BuiltIn ) then
          LoadSuccess := LoadCategoryBitmapsUser( KntFile.TabIconsFN );
      end;
      if ( not LoadSuccess ) then
        LoadSuccess := LoadCategoryBitmapsBuiltIn;
    end;
  end
  else
  begin
    if ( opt_NoUserIcons or ( not LoadCategoryBitmapsUser( ICN_FN ))) then
      LoadCategoryBitmapsBuiltIn;
  end;

end; // LoadTabImages


{$IFDEF KNT_DEBUG}
procedure SaveMenusAndButtons;
var
  bl, ml : TStringList;
  comp : TComponent;
  i, cnt : integer;
  fn, s : string;
  tb : TToolbarButton97;
  mi : TMenuItem;
begin
  with Form_Main do begin
        bl := TStringList.Create;
        ml := TStringList.Create;

        try
          try
            bl.Sorted := true;
            ml.Sorted := true;

            cnt := pred( Form_Main.ComponentCount );
            for i := 0 to cnt do
            begin
              comp := Form_Main.Components[i];
              if comp is TMenuItem then
              begin
                mi := ( comp as TMenuItem );
                s := format(
                  '%s = %s = %s = %s',
                  [mi.Name, mi.Caption, mi.Hint, ShortcutToText( mi.Shortcut ) ]
                );
                ml.Add( s );
              end
              else
              if comp is TToolbarButton97 then
              begin
                tb := ( comp as TToolbarButton97 );
                s := Format(
                  '%s = %s = %s',
                  [tb.Name, tb.Caption, tb.Hint]
                );
                bl.add( s );
              end;
            end;

            fn := extractfilepath( application.exename ) + 'buttonnames.txt';
            bl.savetofile( fn );
            fn := extractfilepath( application.exename ) + 'menunames.txt';
            ml.savetofile( fn );

          except
          end;
        finally
          bl.Free;
          ml.Free;
        end;
  end;
end; // SaveMenusAndButtons
{$ENDIF}

procedure EnableCopyFormat(value: Boolean);
var
  i : integer;
  Str: String;
begin
    Form_Main.TB_CopyFormat.Down:= value;

    if value then begin
       Screen.Cursors[crCopyFormat] := LoadCursor(hInstance,'CPFORMAT');
       if (ParaFormatToCopy.dySpaceBefore >= 0) then
          Str:= STR_16
       else
          Str:= STR_17;
       Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(STR_15, [Str]);
    end
    else begin
       CopyFormatMode:= cfDisabled;
       Form_Main.StatusBar.Panels[PANEL_HINT].Text := '';
    end;

    with KntFile do
       for i := 0 to pred(Folders.Count) do begin
          if value then
             Folders[i].Editor.Cursor:= crCopyFormat
          else
             Folders[i].Editor.Cursor:= crDefault;
       end;

end;


end.
