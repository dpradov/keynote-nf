unit kn_VCLControlsMng;

interface
uses
   kn_NoteObj, TreeNT;

    // dynamically create and destroy controls (note tabs, RichEdits, trees, etc)
    procedure SetUpVCLControls( aNote : TTabNote ); // sets up VCL controls for note (events, menus, etc - only stuff that is handled in this unit, not stuff that TTabNote handles internally)
    procedure CreateVCLControls; // creates VCL controls for ALL notes in NoteFile object
    procedure CreateVCLControlsForNote( const aNote : TTabNote ); // creates VCL controls for specified note
    procedure DestroyVCLControls; // destroys VCL controls for ALL notes in NoteFile object
    procedure DestroyVCLControlsForNote( const aNote : TTabNote; const KillTabSheet : boolean ); // destroys VCL contols for specified note
    procedure GetOrSetNodeExpandState( const aTV : TTreeNT; const AsSet, TopLevelOnly : boolean );

    // VCL updates when config loaded or changed
    procedure UpdateFormState;
    procedure UpdateTabState;
    procedure UpdateNoteDisplay;
    procedure UpdateCursorPos;

    procedure UpdateResPanelState;
    procedure SetResPanelPosition;
    procedure HideOrShowResPanel( const DoShow : boolean );
    procedure UpdateResPanelContents;
    procedure LoadResScratchFile;
    procedure StoreResScratchFile;
    function CheckResourcePanelVisible( const DoWarn : boolean ) : boolean;
    procedure RecreateResourcePanel;
    procedure FocusResourcePanel;

    procedure SelectStatusbarGlyph( const HaveNoteFile : boolean );
    procedure FocusActiveNote;
    procedure SortTabs;

    procedure LoadTrayIcon( const UseAltIcon : boolean );
    procedure LoadTabImages( const ForceReload : boolean );
    procedure SaveMenusAndButtons;

implementation
uses
  { Borland units }
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, Spin, ExtDlgs,
  FileCtrl, RichEdit, IniFiles,
  Clipbrd, ShellAPI, StdCtrls,
  ExtCtrls, mmsystem,
  { 3rd-party units }
  ComCtrls95, RxRichEd,MRUFList,
  TB97Ctls,
  { Own units - covered by KeyNote's MPL}
  gf_misc, gf_files, gf_Const,
  gf_strings, gf_miscvcl,
  kn_INI, kn_Cmd, kn_Msgs,
  kn_Info, kn_Const, kn_Global,
  kn_NodeList, kn_TreeNoteMng, kn_MacroMng, kn_Macro,
  kn_PluginsMng, kn_FavoritesMng, kn_TemplateMng,
  kn_Chest, kn_EditorUtils, kn_NoteFileMng, kn_Main;

resourcestring
  STR_00 = 'Click and drag to resize panels';
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
  
//=================================================================
// SetUpVCLControls
//=================================================================
procedure SetUpVCLControls( aNote : TTabNote );
begin
  with Form_Main do begin
      if assigned( aNote.Editor ) then
      begin

        with aNote.Editor do
        begin
          PopUpMenu := Menu_RTF;
          OnChange := RxRTFChange;
          // OnEnter := RxRTFEnter;
          // OnExit := RxRTFExit;
          OnKeyDown := RxRTFKeyDown;
          OnKeyPress := RxRTFKeyPress;
          OnMouseDown := RxRTFMouseDown;
          OnProtectChange := RxRTFProtectChange;
          OnProtectChangeEx := RxRTFProtectChangeEx;
          OnSelectionChange := RxRTFSelectionChange;
          OnURLClick := RxRTFURLClick;
          OnMouseMove := nil; // RTFMouseMove;
          // OnDragOver := RxRTFDragOver;
          DragMode := dmManual; // dmAutomatic;
          OnStartDrag := RxRTFStartDrag;
          OnEndDrag := RxRTFEndDrag;
          OnFileDropped := OnFileDropped;
          // AllowObjects := true;
          // AllowInPlace := false;
        end;
      end;

      // enable "advanced typography options" for the richedit;
      // this gives us full justification and other goodies.
      if ( _LoadedRichEditVersion > 2 ) then
        SendMessage( aNote.Editor.Handle, EM_SETTYPOGRAPHYOPTIONS, TO_ADVANCEDTYPOGRAPHY, TO_ADVANCEDTYPOGRAPHY );

      if ( aNote.Kind = ntTree ) then
      begin
        with TTreeNote( aNote ).TV do
        begin
          PopupMenu := Menu_TV;
          OnKeyDown := TVKeyDown;
          OnKeyPress := TVKeyPress;
          OnChange := TVChange;
          // OnChanging := TVChanging; // unused
          OnChecked := TVChecked;
          // OnEnter := TVEnter;
          OnFileDropped := OnFileDropped;
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
        end;
      end;
  end;

end; // SetUpVCLControls

//=================================================================
// CreateVCLControls
//=================================================================
procedure CreateVCLControls;
// creates all VCL controls for a newly loaded Notes file
var
  i : integer;
  myNote : TTabNote;
begin
  with Form_Main do begin
        if (( not assigned( NoteFile )) or ( NoteFile.Notes.Count = 0 )) then exit;

        try

          for i := 0 to pred( NoteFile.Notes.Count ) do
          begin
            myNote := NoteFile.Notes[i];
            CreateVCLControlsForNote( myNote );
            myNote.DataStreamToEditor;
            SetUpVCLControls( myNote );
          end;

        finally

          // show all tabs (they were created hidden)
          if ( Pages.PageCount > 0 ) then
          begin
            for i := 0 to pred( Pages.PageCount ) do
            begin
              Pages.Pages[i].TabVisible := true;
            end;
          end;

        end;

        // restore the note that was active when file was previously saved
        if (( NoteFile.ActiveNote >= 0 ) and ( NoteFile.ActiveNote < NoteFile.Notes.Count )) then
        begin
          if ( Pages.PageCount > 0 ) then
            Pages.ActivePage := Pages.Pages[NoteFile.ActiveNote];
        end
        else
        begin
          if ( Pages.PageCount > 0 ) then
            Pages.ActivePage := Pages.Pages[0];
        end;
  end;

end; // CreateVCLControls

//=================================================================
// GetOrSetNodeExpandState
//=================================================================
procedure GetOrSetNodeExpandState( const aTV : TTreeNT; const AsSet, TopLevelOnly : boolean );
var
  myTreeNode : TTreeNTNode;
  myNoteNode : TNoteNode;
begin
  with Form_Main do begin
        // set or get node "expanded" state

        if ( not assigned( aTV )) then exit;
        myTreeNode := aTV.Items.GetFirstNode;
        while assigned( myTreeNode ) do
        begin
          myNoteNode := TNoteNode( myTreeNode.Data );
          if assigned( myNoteNode ) then
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
                  myTreeNode.Expanded := myNoteNode.Expanded;
                  myTreeNode := myTreeNode.GetNext;
                end;
              end;
              false : begin // get
                myNoteNode.Expanded := myTreeNode.Expanded;
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
procedure CreateVCLControlsForNote( const aNote : TTabNote );
var
  myTab : TTab95Sheet;
  myEditor : TTabRichEdit;
  // NoteID : string[3];
  myTree : TTreeNT;
  mySplitter : TSplitter;
  tNote : TTreeNote;
  i, loop : integer;
  tNode, myTreeNode, LastTreeNodeAssigned : TTreeNTNode;
  NodeCnt, LastNodeLevel : integer;
  myNode : TNoteNode;
  {$IFDEF WITH_IE}
  myPanel : TPanel;
  myBrowser : TWebBrowser;
  {$ENDIF}

begin
  with Form_Main do begin
        _ALLOW_VCL_UPDATES := false;
        tNote := nil;
        {$IFDEF WITH_IE}
        myPanel := nil;
        {$ENDIF}

        try
          if ( aNote.FocusMemory = focNil ) then
          begin
            case aNote.Kind of
              ntTree : aNote.FocusMemory := focTree;
              else
                aNote.FocusMemory := focRTF;
            end;
          end;

          if ( aNote.TabSheet = nil ) then
          begin
            myTab := TTab95Sheet.Create( Form_Main );
            with myTab do
            begin
              // Visible := false;
              TabVisible := false; // hide tabs initially
              Parent := Pages;
              PageControl := Pages;
              // name := 'Tab' + NoteID;

              if _TABS_ARE_DRAGGABLE then
              begin
                Dragable := true;
                FloatOnTop := false;
              end;

            end;
            aNote.TabSheet := myTab;
          end
          else
          begin
            myTab := aNote.TabSheet;
          end;

          if ( aNote.Kind = ntTree ) then
          begin
            tNote := TTreeNote( aNote );
            // [f] tNote.FocusMemory := focTree;

            myTree := TTreeNT.Create( myTab );
            with myTree do
            begin

              Parent := myTab;
              if tNote.VerticalLayout then
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

              if tNote.VerticalLayout then
              begin
                if (( tNote.TreeWidth < 30 ) or
                    ( tNote.TreeWidth > ( Pages.Height - 30 ))) then
                  Height := ( Pages.Height DIV 3 )
                else
                  Height := tNote.TreeWidth;
                tNote.TreeWidth := Height; // store corrected value
              end
              else
              begin
                if (( tNote.TreeWidth < 30 ) or
                    ( tNote.TreeWidth > ( Pages.Width - 30 ))) then
                  Width := ( Pages.Width DIV 3 )
                else
                  Width := tNote.TreeWidth;
                tNote.TreeWidth := Width; // store corrected value
              end;
            end;

            mySplitter := TSplitter.Create( myTab );
            with mySplitter do
            begin
              Parent := myTab;
              Align := alNone;
              if tNote.VerticalLayout then
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

            tNote.Splitter := mySplitter;

            tNote.TV := myTree;
            UpdateTreeOptions( tNote );

            tNote.UpdateTree; // do this BEFORE creating nodes

            // Create TreeNodes for all nodes in the note
            LastTreeNodeAssigned := nil;
            LastNodeLevel := 0;
            if ( tNote.Nodes.Count > 0 ) then
            begin
              myTree.Items.BeginUpdate;
              try
                NodeCnt := pred( tNote.Nodes.Count );
                for i := 0 to NodeCnt do
                begin
                  myNode := tNote.Nodes[i];
                  case myNode.Level of
                    0 : begin
                      myTreeNode := myTree.Items.Add( nil, myNode.Name );
                      LastNodeLevel := 0;
                    end
                    else
                    begin
                      case DoTrinaryCompare( myNode.Level, LastNodeLevel ) of
                        trinGreater : begin
                          myTreeNode := myTree.Items.AddChild( LastTreeNodeAssigned, myNode.Name );
                        end;
                        trinEqual : begin
                          myTreeNode := myTree.Items.AddChild( LastTreeNodeAssigned.Parent, myNode.Name );
                        end;
                        else
                        begin // myNode.Level is SMALLER than LastNodeLevel, i.e. we're moving LEFT in the tree
                          for loop := 1 to ( LastNodeLevel - myNode.Level ) do
                          begin
                            if assigned( LastTreeNodeAssigned ) then
                            begin
                              if ( LastTreeNodeAssigned.Level <= myNode.Level ) then
                                break;
                              LastTreeNodeAssigned := LastTreeNodeAssigned.Parent;
                            end
                            else
                              break;
                          end;
                          myTreeNode := myTree.Items.Add( LastTreeNodeAssigned, myNode.Name );
                        end;
                      end;
                    end;
                  end;
                  LastTreeNodeAssigned := myTreeNode;
                  LastNodeLevel := myNode.Level;

                  myTreeNode.Data := myNode;

                  if myNode.HasNodeFontFace then
                    myTreeNode.Font.Name := myNode.NodeFontFace;

                  if myNode.Bold then
                    myTreeNode.Font.Style := [fsBold];

                  if myNode.HasNodeColor then
                    myTreeNode.Font.Color := myNode.NodeColor;

                  if myNode.HasNodeBGColor then
                    myTreeNode.Color := myNode.NodeBGColor;

                  if myNode.Filtered  then      // [dpv]
                     tNote.Filtered := True;

                  if myNode.AlarmF <> 0  then      // [dpv*]
                     AlarmManager.AddAlarmNode(myTreeNode);

                end;

              finally
                myTree.Items.EndUpdate;
                ShowOrHideIcons( tNote, true );
                ShowOrHideCheckBoxes( tNote );
                if tNote.Filtered then             // [dpv]
                   HideFilteredNodes (tnote);
                if tNote.HideCheckedNodes then     // [dpv]
                   HideCheckedNodes (tnote);
              end;

              // restore selected node: this block must be
              // OUTSIDE the beginupdate..endupdate range

              //if ( myTree.Items.Count > 0 ) then       // [dpv]
              if ( myTree.Items.CountNotHidden > 0 ) then
              begin
                if (( TreeOptions.ExpandMode <> txmFullCollapse ) and // SaveActiveNode and
                   ( tNote.OldSelectedIndex >= 0 ) and
                   ( tNote.OldSelectedIndex < myTree.Items.Count )) then
                begin
                  // restore the node which was selected when file was saved
                  tNode:= myTree.Items[tNote.OldSelectedIndex];
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
                tNote.SelectedNode := TNoteNode( myTree.Selected.Data );
              end
              else
              begin
                tNote.SelectedNode := nil;
              end;

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


              UpdateTreeVisible( tNote ); // [f]

              if assigned( myTree.Selected ) then
                myTree.Selected.MakeVisible;

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

             tNote.MainPanel := myPanel;

             if _IE4Available then
             begin
             myBrowser := TWebBrowser.Create( myPanel );
             TControl( myBrowser ).Parent := myPanel;
             with myBrowser do
             begin
              Align := alClient;
              Visible := false;
             end;
             tNote.WebBrowser := myBrowser;
             end
             else
             begin
              tNote.WebBrowser := nil;
             end;

           {$ENDIF}

          end; // tree-type note

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
            // name := 'RTF' + NoteID;
            // HelpContext := 100;
            HelpContext := 10;
            MaxLength := 0; // unlimited text size
            ParentFont := false;
            WantTabs := true;
            WantReturns := true;
            NoteObj := aNote;
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
            Language := DefaultEditorChrome.Language;
            ScrollBars := ssBoth;
          end;

          aNote.Editor := myEditor;
          with myTab do
          begin
            // PrimaryControl := myEditor;
            PrimaryObject := aNote;
          end;

          with aNote do
          begin
            UpdateTabSheet;
            UpdateEditor; // do this BEFORE placing RTF text in editor
          end;

        finally
          _ALLOW_VCL_UPDATES := true;
        end;
  end;

end; // CreateVCLControlsForNote

//=================================================================
// DestroyVCLControlsForNote
//=================================================================
procedure DestroyVCLControlsForNote( const aNote : TTabNote; const KillTabSheet : boolean );
begin
  with Form_Main do begin
        if not assigned( aNote ) then exit;

        _ALLOW_VCL_UPDATES := false;
        try

          if assigned( aNote.Editor ) then
          begin
            with aNote.Editor do
            begin
              OnChange := nil;
              OnSelectionChange := nil;
              OnProtectChange := nil;
              OnEnter := nil;
              OnExit := nil;
              OnKeyDown := nil;
              Free;
            end;
            aNote.Editor := nil;
          end;

          if ( aNote.Kind = ntTree ) then
          begin
            if assigned( TTreeNote( aNote ).Splitter ) then
            begin
              TTreeNote( aNote ).Splitter.Free;
              TTreeNote( aNote ).Splitter := nil;
            end;
            if assigned( TTreeNote( aNote ).TV ) then
            begin
              with TTreeNote( aNote ).TV do
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
              TTreeNote( aNote ).TV.Free;
              TTreeNote( aNote ).TV := nil;
            end;
          end;

          if ( KillTabSheet and assigned( aNote.TabSheet )) then
          begin
            aNote.TabSheet.Free;
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
  // aNote : TTabNote;
begin
  with Form_Main do begin
        {
        if (( not assigned( NoteFile )) or ( NoteFile.Notes.Count = 0 )) then exit;
        for i := 0 to pred( NoteFile.Notes.Count ) do
        begin
          DestroyVCLControlsForNote( NoteFile.Notes[i]; );
        end;
        }

          if ( pages.pagecount > 0 ) then
          begin
            for i := pred( pages.pagecount ) downto 0 do
            begin
                {
                aNote := TTabNote( pages.pages[i].PrimaryObject );
                try
                  aNote.Editor := nil;
                  if ( aNote.Kind = ntTree ) then
                  begin
                    TTreeNote( aNote ).TV := nil;
                    TTreeNote( aNote ).Splitter := nil;
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
            if ( ComboFontLen > MIN_COMBO_LENGTH ) then begin  //***  Directamente da un error interno al compilar ¿??
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
            Combo_Style.ItemHeight := 24
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

          if MinimizeOnClose then
            TB_Exit.Hint := STR_08
          else
            TB_Exit.Hint := STR_09;

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
        CB_ResFind_NodeNames.Checked := FindOptions.SearchNodeNames;
        RG_ResFind_Type.ItemIndex := ord( FindOptions.SearchMode );
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
  myTNote : TTreeNote;
  Node: TTreeNTNode;

begin
  with Form_Main do begin
        s := '';
        if assigned( ActiveNote ) then
        begin
          try
            MMNoteReadOnly.Checked := ActiveNote.ReadOnly;
            TB_ClipCap.Down := ( NoteFile.ClipCapNote = ActiveNote );
            MMNoteClipCapture.Checked := TB_ClipCap.Down;
            TMClipCap.Checked := MMNoteClipCapture.Checked;

            UpdateWordWrap;

            // if isWordWrap then s := ' W' else s := ' ';

            RxRTFChange( ActiveNote.Editor );
            RxRTFSelectionChange( ActiveNote.Editor );
            if ActiveNote.ReadOnly then s := 'R';

            if ( _LoadedRichEditVersion > 2 ) then
            begin
              _LastZoomValue := GetEditorZoom;
              Combo_Zoom.Text := Format(
                '%d%%', [_LastZoomValue] );
            end;

            ShowInsMode;


            if ( ActiveNote.Kind = ntTree ) then
            begin
              myTNote := TTreeNote( ActiveNote );
              MMTree_.Visible := true;
              {MMViewTree.Visible := true;}
              MMViewTree.Enabled := true;
              MMViewTree.Checked := myTNote.TV.Visible;
              {MMViewNodeIcons.Visible := true;
              MMViewCustomIcons.Visible := true;
              MMViewCheckboxes.Visible := true;}
              MMViewNodeIcons.Checked := myTNote.IconKind = niStandard;
              MMViewCustomIcons.Checked := myTNote.IconKind = niCustom;
              MMEditPasteAsNewNode.Visible := true;
              MMP_PasteAsNode.Visible := true;
              MMViewCheckboxesAllNodes.Checked := TTreeNote( ActiveNote ).Checkboxes;
              //TVCheckNode.Enabled := MMViewCheckboxesAllNodes.Checked;              // [dpv]
              node:= myTNote.TV.Selected;
              if myTNote.Checkboxes or (assigned(node) and assigned(node.Parent) and (node.Parent.CheckType =ctCheckBox)) then  // [dpv]
                 TVCheckNode.Enabled := true
              else
                 TVCheckNode.Enabled := false;

              if assigned(node) and (TNoteNode(node.Data).Alarm <> 0) then   // [dpv*]
                 TB_AlarmNode.Down:= true
              else
                 TB_AlarmNode.Down:= false;
              TVAlarmNode.Checked:= TB_AlarmNode.Down;  // [dpv]

              TVChildrenCheckbox.Enabled := not MMViewCheckboxesAllNodes.Checked;       // [dpv]
              Toolbar_Tree.Visible := KeyOptions.ToolbarTreeShow;
              MMViewNodeIcons.Enabled := MMViewTree.Checked;
              MMViewCustomIcons.Enabled := MMViewTree.Checked;
              MMViewCheckboxesAllNodes.Enabled := MMViewTree.Checked;
              MMViewCustomIcons.Enabled := MMViewTree.Checked;
              TVSelectNodeImage.Enabled := ( MMViewCustomIcons.Checked and MMViewCustomIcons.Enabled );
              MMViewHideCheckedNodes.Enabled := true;                      // [dpv]
              MMViewHideCheckedNodes.Checked:= myTNote.HideCheckedNodes;   // [dpv]
              TB_HideChecked.Down := MMViewHideCheckedNodes.Checked;       // [dpv]
              TB_FilterTree.Down:= myTNote.Filtered;                       // [dpv]
              MMViewFilterTree.Enabled := true;                            // [dpv]
              MMViewFilterTree.Checked :=  myTNote.Filtered;               // [dpv]
              if myTNote.Filtered then FilterApplied (myTNote) else FilterRemoved (myTNote);   // [dpv]

            end
            else
            begin
              MMTree_.Visible := false;
              { MMViewTree.Visible := false;}
              MMViewTree.Enabled := false;
              {MMViewNodeIcons.Visible := false;
              MMViewCustomIcons.Visible := false;
              MMViewCheckboxes.Visible := false;}
              MMViewNodeIcons.Enabled := false;
              MMViewCheckboxesAllNodes.Enabled := false;
              TVChildrenCheckbox.Enabled := false;       // [dpv]
              MMViewCustomIcons.Enabled := false;
              MMEditPasteAsNewNode.Visible := false;
              MMP_PasteAsNode.Visible := false;
              Toolbar_Tree.Visible := false;
              TVSelectNodeImage.Enabled := false;
              MMViewHideCheckedNodes.Checked := False;  // [dpv]
              MMViewHideCheckedNodes.Enabled := False;  // [dpv]
              MMViewFilterTree.Checked := false; // [dpv]
              MMViewFilterTree.Enabled := false; // [dpv]
            end;

          except
            // showmessage( 'BUG: error in UpdateNoteDisplay' );
          end;
        end
        else
        begin
          MMNoteReadonly.Checked := false;
          MMFormatWordWrap.Checked := false;
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
  cad: string;
begin
  with Form_Main do begin
        if assigned( ActiveNote ) then
        begin
          if EditorOptions.TrackCaretPos then
          begin
            p := ActiveNote.Editor.CaretPos;
            if ( ActiveNote.Editor.SelLength = 0 ) then begin
              //p := ActiveNote.Editor.CaretPos;
              StatusBar.Panels[PANEL_CARETPOS].Text :=
                Format( STR_10, [succ( p.y ), ActiveNote.Editor.Lines.Count, succ( p.x )] );
              end
            else begin
                StatusBar.Panels[PANEL_CARETPOS].Text :=
                Format( STR_11, [ActiveNote.Editor.SelLength, GetWordCount( ActiveNote.Editor.SelText )] );
            end;
          end
          else begin
            if EditorOptions.WordCountTrack  then begin
               if ( ActiveNote.Editor.SelLength > 0 ) then
                  UpdateWordCount;
            end else
              StatusBar.Panels[PANEL_CARETPOS].Text := '';
          end;


          if EditorOptions.TrackStyle then
          begin
            case EditorOptions.TrackStyleRange of
              srFont : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveNote.Editor.FontInfoString;
              end;
              srParagraph : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveNote.Editor.ParaInfoString;
              end;
              srBoth : begin
                StatusBar.Panels[PANEL_HINT].Text := ActiveNote.Editor.FontInfoString +
                ActiveNote.Editor.ParaInfoString;
              end;
            end;
          end;

          TB_EditCut.Enabled := ( ActiveNote.Editor.SelLength > 0 );

          TB_EditCopy.Enabled := TB_EditCut.Enabled;
          MMEditCut.Enabled := TB_EditCut.Enabled;
          MMEditCopy.Enabled := TB_EditCut.Enabled;
          RTFMCut.Enabled := TB_EditCut.Enabled;
          RTFMCopy.Enabled := TB_EditCut.Enabled;

        end
        else
        begin
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
        if assigned( ActiveNote ) then
          ActiveNote.Editor.Invalidate;
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

            // custom draw for List_ResFind
            if ResPanelOptions.ColorFindList then
            begin
              List_ResFind.Style := lbOwnerDrawFixed;
              List_ResFind.OnDrawItem := List_ResFindDrawItem;
            end;

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

procedure UpdateResPanelContents;
begin
  // General idea: do not load all resource panel information
  // when KeyNote starts. Instead, load data only when
  // a tab is viewed, if the tab contains no data. For example,
  // when user clicks the Macros tab and the list of macros
  // is empty, we load he macros.

  with Form_Main do begin
      if KeyOptions.ResPanelShow then
      begin

        MMToolsPluginRun.Enabled := ResTab_Plugins.TabVisible;
        MMToolsMacroRun.Enabled := ResTab_Macro.TabVisible;

        if ( Pages_Res.ActivePage = ResTab_Find ) then
        begin
          // nothing to do
        end
        else
        if ( Pages_Res.ActivePage = ResTab_RTF ) then
        begin
          if ( Res_RTF.Lines.Count = 0 ) then
            LoadResScratchFile;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Macro ) then
        begin
          // load macros
          if ( ListBox_ResMacro.Items.Count = 0 ) then
            EnumerateMacros;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Template ) then
        begin
          // load templates
          if ( ListBox_ResTpl.Items.Count = 0 ) then
            LoadTemplateList;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Plugins ) then
        begin
          if ( ListBox_ResPlugins.Items.Count = 0 ) then
          begin
            DisplayPlugins;
          end;
        end
        else
        if ( Pages_Res.ActivePage = ResTab_Favorites ) then
        begin
          if ( ListBox_ResFav.Items.Count = 0 ) then
          begin
            DisplayFavorites;
          end;
        end;

      end
      else
      begin
        MMToolsPluginRun.Enabled := false;
        MMToolsMacroRun.Enabled := false;

        try
          if Res_RTF.Modified then
            StoreResScratchFile;
          if KeyOptions.ResPanelActiveUpdate then
          begin
            Res_RTF.Clear;

            // if a macro file was copied to macros folder
            // while KeyNote is running, simply hiding and then
            // showing the resourc2e panel will load the new macro
            // (press F9 twice)
            ListBox_ResMacro.Items.Clear;
            ClearMacroList;

            ListBox_ResFav.Clear;

            // clear list of templates
            ListBox_ResTpl.Clear;

            // List of plugns does NOT get cleared,
            // because it takes a long time to initialize.
            // Once loaded, the lisy remains available even
            // after the resource panel is hidden. To reload
            // the list of current plugins, use the "Reload
            // plugins" menu command.
            { ListBox_ResPlugins.Items.Clear; }
          end;
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
              Pages.Pages[i].ImageIndex := TTabNote( Pages.Pages[i].PrimaryObject ).ImageIndex;
            end;
          end;

        finally
          Pages.Enabled := true;
          Pages.ActivePage := ActiveNote.TabSheet;
          NoteFile.Modified := true;
          UpdateNoteDisplay;
          UpdateNoteFileState( [fscModified] );
        end;
  end;

end; // SortTabs

procedure FocusActiveNote;
begin
    try
      if ( assigned( ActiveNote ) and ( not Initializing )) then
      begin
        case ActiveNote.Kind of
          ntRTF : ActiveNote.Editor.SetFocus;
          ntTree : begin
            if ( TTreeNote( ActiveNote ).TV.Visible and ( ActiveNote.FocusMemory = focTree )) then
              TTreeNote( ActiveNote ).TV.SetFocus
            else
              ActiveNote.Editor.SetFocus;
          end;
        end;
      end;
    except
      // Mostly Harmless
    end;
end; // FocusActiveNote


procedure SelectStatusbarGlyph( const HaveNoteFile : boolean );
var
  Glyph : TPicture;
begin

  // indicate file state or special program activity
  // with a cute little icon on statusbar

  Glyph := TPicture.Create;
  try

    if HaveNoteFile then
    begin

      if IsRecordingMacro then
      begin
        Chest.MGRImages.GetBitmap( NODEIMG_MACROREC, Glyph.Bitmap );
      end
      else
      if IsRunningMacro then
      begin
        Chest.MGRImages.GetBitmap( NODEIMG_MACRORUN, Glyph.Bitmap );
      end
      else
      if NoteFile.ReadOnly then
      begin
        case NoteFile.FileFormat of
          nffKeyNote : begin
            Chest.MGRImages.GetBitmap( NODEIMG_TKN_RO, Glyph.Bitmap );
          end;
          nffEncrypted : begin
            Chest.MGRImages.GetBitmap( NODEIMG_ENC_RO, Glyph.Bitmap );
          end;
          nffDartNotes : begin
            Chest.MGRImages.GetBitmap( NODEIMG_DART_RO, Glyph.Bitmap );
          end;
        end;
      end
      else
      begin
        case NoteFile.FileFormat of
          nffKeyNote : begin
            Chest.MGRImages.GetBitmap( NODEIMG_TKN, Glyph.Bitmap );
          end;
          nffEncrypted : begin
            Chest.MGRImages.GetBitmap( NODEIMG_ENC, Glyph.Bitmap );
          end;
          nffDartNotes : begin
            Chest.MGRImages.GetBitmap( NODEIMG_DART, Glyph.Bitmap );
          end;
        end;
      end;

    end
    else
    begin
      Chest.MGRImages.GetBitmap( NODEIMG_BLANK, Glyph.Bitmap );
    end;

    Form_Main.StatusBar.Panels[PANEL_FILEICON].Glyph := Glyph;
  finally
    Glyph.Free;
  end;
end; // SelectStatusbarGlyph


procedure LoadTrayIcon( const UseAltIcon : boolean );
begin
  with Form_Main do begin
        if assigned( NoteFile ) then
        begin
          if UseAltIcon then
          begin
            // we're capturing clipboard, so indicate this
            // by using the alternate (orange) tray icon
            TrayIcon.Icon := TrayIcon.Icons[1];
          end
          else
          if ( NoteFile.TrayIconFN <> '' ) then
          begin
            if fileexists( NoteFile.TrayIconFN ) then
            begin
              try
                TrayIcon.Icon.LoadFromFile( NoteFile.TrayIconFN );
              except
                TrayIcon.Icon := TrayIcon.Icons[0];
              end;
            end
            else
            begin
              NoteFile.TrayIconFN := '';
              TrayIcon.Icon := TrayIcon.Icons[0];
            end;
          end
          else
          begin
            TrayIcon.Icon := TrayIcon.Icons[0];
          end;
        end
        else
        begin
          TrayIcon.Icon := TrayIcon.Icons[0];
        end;
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
  if assigned( NoteFile ) then
  begin
    if (( _LOADED_ICON_FILE <> NoteFile.TabIconsFN ) or ForceReload ) then
    begin
      if ( NoteFile.TabIconsFN = '' ) then // means: use KeyNote default
      begin
        LoadSuccess := LoadCategoryBitmapsUser( ICN_FN );
      end
      else
      begin
        if ( NoteFile.TabIconsFN <> _NF_Icons_BuiltIn ) then
          LoadSuccess := LoadCategoryBitmapsUser( NoteFile.TabIconsFN );
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

end.
