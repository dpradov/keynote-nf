unit kn_NoteMng;

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
   System.SysUtils,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Controls,
   kn_Const,
   kn_Info;

    // folder management
    procedure CreateNewNote;
    function NewNote( const DefaultNote, CanFocus : boolean ) : boolean;
    procedure DeleteNote;
    procedure RenameNote;
    procedure EditNoteProperties( const PropertiesAction : TPropertiesAction );

implementation

uses
   TreeNT,
   gf_miscvcl,
   gf_strings,
   kn_Global,
   kn_EditorUtils,
   kn_Defaults,
   kn_Macro,
   kn_NoteObj,
   kn_FileMgr,
   kn_NewNote,
   kn_TreeNoteMng,
   kn_NoteFileMng,
   kn_MacroMng,
   kn_ConfigMng,
   kn_VCLControlsMng,
   kn_Main;

resourcestring
  STR_01 = ' New folder.';
  STR_02 = 'Are you sure you want to delete folder "%s"?' + #13 + 'This operation cannot be undone.';
  STR_03 = 'Confirm deleting folder';
  STR_04 = ' Folder deleted.';
  STR_05 = ' Folder renamed.';


function NewNote(
  const DefaultNote, CanFocus : boolean) : boolean;
var
  myFolder: TKntFolder;
  Form_NewNote : TForm_NewNote;
  FileWasBusy, TimerWasEnabled : boolean;
begin
  result := false;
  with Form_Main do
  begin
    if ( not HaveKntFolders( true, false )) then exit;
    myFolder := nil;
    FileWasBusy := FileIsBusy;
    FileIsBusy := true;
    StatusBar.Panels[PANEL_HINT].Text := '';
    TimerWasEnabled := Timer.Enabled;
    Timer.Enabled := false;
  end;
  try
    try
      if DefaultNote then
      begin
        myFolder := TKntFolder.Create;
        myFolder.SetEditorProperties( DefaultEditorProperties );
        myFolder.SetTabProperties( DefaultTabProperties );
        myFolder.EditorChrome := DefaultEditorChrome;
        myFolder.SetTreeProperties( DefaultTreeProperties );
        myFolder.TreeChrome := DefaultTreeChrome;
      end
      else
      begin
        Form_NewNote := TForm_NewNote.Create( Form_Main );
        try
          with Form_NewNote do
          begin
            ShowHint := KeyOptions.ShowTooltips;
            myEditorProperties := DefaultEditorProperties;
            myTabProperties := DefaultTabProperties;
            myChrome :=  DefaultEditorChrome;
            myTabNameHistory := KeyOptions.TabNameHistory;
            myNodeNameHistory := KeyOptions.NodeNameHistory;
            myHistoryCnt := FindOptions.HistoryMaxCnt;
            TAB_CHANGEABLE := true;
            myTreeProperties := DefaultTreeProperties;
            myTreeChrome := DefaultTreeChrome;
            myTreeOptions := TreeOptions;
          end;
          if ( Form_NewNote.ShowModal = mrOK ) then
          begin
            KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;
            myFolder := TKntFolder.Create;
            myFolder.SetEditorProperties( Form_NewNote.myEditorProperties );
            myFolder.SetTabProperties( Form_NewNote.myTabProperties );
            myFolder.EditorChrome := Form_NewNote.myChrome;
            KeyOptions.NodeNameHistory := Form_NewNote.myNodeNameHistory;
            with myFolder do begin
              SetTreeProperties( Form_NewNote.myTreeProperties );
              TreeChrome := Form_NewNote.myTreeChrome;
            end;
          end;
        finally
          Form_NewNote.Free;
        end;
      end;
      if assigned( myFolder ) then
      begin
        KntFile.AddFolder( myFolder );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_01;
        try
          with Form_Main do begin
          CreateVCLControlsForNote( myFolder );
          SetUpVCLControls( myFolder );
          AddToFileManager( KntFile.FileName, KntFile ); // update manager (number of notes has changed)
          end;

        finally
          KntFile.Modified := ( not DefaultNote );
          myFolder.TabSheet.TabVisible := true; // was created hidden
          ActiveKntFolder := myFolder;
        end;
        TreeNoteNewNode( nil, tnTop, nil, '', true );
        Form_Main.Pages.ActivePage := myFolder.TabSheet;
        UpdateNoteDisplay;
        if CanFocus then
        begin
          FocusActiveNote;
        end;

      end;
    except
      on E : Exception do
      begin
        {$IFDEF KNT_DEBUG}
         Log.Add( 'Exception in NewNote: ' + E.Message );
        {$ENDIF}
         PopupMessage( E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    Form_Main.Timer.Enabled := TimerWasEnabled;
    FileIsBusy := FileWasBusy;
    KntFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
    result := assigned( myFolder );
   {$IFDEF KNT_DEBUG}
    if assigned( myFolder ) then
      Log.Add( 'Added new folder: ' + myFolder.Name )
    else
      Log.Add( 'New folder NOT added.' );
   {$ENDIF}
  end;
end; // NewNote

procedure DeleteNote;
var
  pidx : integer;
begin
  with Form_Main do
  begin
      if ( not HaveKntFolders( true, true )) then exit;
      if ( not assigned( ActiveKntFolder )) then exit;
      if NoteIsReadOnly( ActiveKntFolder, true ) then exit;

      if KeyOptions.ConfirmTabDelete then
      begin
        if ( DoMessageBox(
            Format( STR_02, [RemoveAccelChar( ActiveKntFolder.Name )] ),
            STR_03,
            MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES ) then exit;
      end;

      try
        // clear Clipboard capture state if this folder is ClipCapFolder
        if ( KntFile.ClipCapFolder = ActiveKntFolder ) then
        begin
          TB_ClipCap.Down := false;
          ClipCapNode := nil;
          Pages.MarkedPage := nil;
          ToggleClipCap( false, ActiveKntFolder ); // turn it OFF
        end;

        { // [dpv]
        // clear all bookmarks pointing to this folder
        for pidx := 0 to MAX_BOOKMARKS do
        begin
          if ( KntFile.Bookmarks[pidx].Folder = ActiveKntFolder ) then
            BookmarkClear( pidx );
        end;
        }

        pidx := ActiveKntFolder.TabSheet.TabIndex;

        Pages.OnChange := nil;

        DestroyVCLControlsForNote( ActiveKntFolder, true );
        KntFile.RemoveImagesCountReferences(ActiveKntFolder);
        KntFile.DeleteFolder( ActiveKntFolder );
        ActiveKntFolder := nil;
        AddToFileManager( KntFile.FileName, KntFile ); // update manager (number of notes has changed)

        try
          if ( Pages.PageCount > 0 ) then
          begin
            if ( pidx = 0 ) then
            begin
              Pages.ActivePage := Pages.Pages[0];
            end
            else
            begin
              if ( pidx > Pages.PageCount ) then
                Pages.ActivePage := Pages.Pages[pred( Pages.PageCount )]
              else
                Pages.ActivePage := Pages.Pages[pred( pidx )];
            end;
          end;

        except
          // occasionally PageControl throws an exception here, don't know why, but it's harmless
        end;

      finally
        try
          PagesChange( Form_Main );
          if assigned( ActiveKntFolder ) then
            ActiveKntFolder.Editor.SetFocus;
        except
        end;
        Pages.OnChange := PagesChange;
        StatusBar.Panels[PANEL_HINT].text := STR_04;
        KntFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
    end;
end; // DeleteNote

procedure CreateNewNote;
begin
  if assigned(ActiveKntFolder) then
     ActiveKntFolder.EditorToDataStream;
  if NewNote( false, true ) then
  begin
    Application.ProcessMessages;
    if KeyOptions.RunAutoMacros then
       ExecuteMacro( _MACRO_AUTORUN_NEW_TREE, '' );
  end;
end; // CreateNewNote

procedure RenameNote;
var
  Form_NewNote : TForm_NewNote;
begin
  with Form_Main do begin
      if ( not HaveKntFolders( true, true )) then exit;
      if ( not assigned( ActiveKntFolder )) then exit;
      if NoteIsReadOnly( ActiveKntFolder, true ) then exit;

      Form_NewNote := TForm_NewNote.Create( Form_Main );
      try
        with Form_NewNote do
        begin
          TAB_CHANGEABLE := false;
          ShowHint := KeyOptions.ShowTooltips;
          myTabProperties.Name := ActiveKntFolder.Name;
          myTabProperties.ImageIndex := ActiveKntFolder.ImageIndex;
          myTabNameHistory := KeyOptions.TabNameHistory;
          myHistoryCnt := FindOptions.HistoryMaxCnt;
          Button_Properties.Enabled := false;
          Button_Properties.Visible := false;
        end;
        if ( Form_NewNote.ShowModal = mrOK ) then
        begin
          KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;
          KntFile.Modified := true;
          ActiveKntFolder.Name := Form_NewNote.myTabProperties.Name;
          ActiveKntFolder.ImageIndex := Form_NewNote.myTabProperties.ImageIndex;
          StatusBar.Panels[PANEL_HINT].Text := STR_05;
        end;
      finally
        Form_NewNote.Free;
        KntFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
  end;
end; // RenameNote

procedure EditNoteProperties( const PropertiesAction : TPropertiesAction );
var
  Form_Defaults : TForm_Defaults;
  i : integer;
  TreeLayoutChanged : boolean;
  oldIconKind : TNodeIconKind;
  oldShowCheckboxes : boolean;
  oldHideChecked: boolean;      // [dpv]
  oldPlainText: boolean;
  EnsuredPlainText: boolean;
  Folder: TKntFolder;
  NewPropertiesAction : TPropertiesAction;

begin

  with Form_Main do begin
      if (( PropertiesAction = propThisNote ) and ( not assigned( ActiveKntFolder ))) then
        exit;

      TreeLayoutChanged := false;

      Form_Defaults := TForm_Defaults.Create( Form_Main );

      oldIconKind := niStandard;
      oldShowCheckboxes := false;

      try

        with Form_Defaults do begin
          ShowHint := KeyOptions.ShowTooltips;
          Action := PropertiesAction;
          DefaultsFN := DEF_FN;
          myTabNameHistory := KeyOptions.TabNameHistory;
          myNoteIsReadOnly := (( PropertiesAction = propThisNote ) and NoteIsReadOnly( ActiveKntFolder, false ));

          myNodeNameHistory := KeyOptions.NodeNameHistory;

          myCurrentFileName:= '';
          if assigned(KntFile) and (KntFile.FileName <> '') then
             myCurrentFileName := ExtractFilename( KntFile.FileName );


          case PropertiesAction of
            propThisNote : begin

              myEditorChrome := ActiveKntFolder.EditorChrome;
              ActiveKntFolder.GetTabProperties( myTabProperties );
              ActiveKntFolder.GetEditorProperties( myEditorProperties );

              myEditorProperties.DefaultZoom:= DefaultEditorProperties.DefaultZoom;    // Just for show it

              // [x] bug workaround: despite the fact that RichEdit has
              // protection against losing styles in RecreateWnd, changing
              // wordwrap HERE still causes all font styles to be lost.
              // I have no time for investigating this anymore, so the
              // wordwrap option is disabled here. It's not needed here
              // anyway, since you can just press Ctrl+W in the editor,
              // which is more convenient and does NOT cause the
              // font style loss.
              {  *1
                 Disabling and hiding the combo did not solve the problem: pressing Ok from the properties
                 of a folder after the state of WordWrap have been changed (in the active node or simply for
                 selecting nodes with different WodWrap state) always caused formatting to be lost. Since version 1.6.5...

              CB_WordWrap.Enabled := false;
              CB_WordWrap.Visible := false;
              }
              CB_WordWrap.Checked:= myEditorProperties.WordWrap;    // *1


              with ActiveKntFolder do begin

                myInheritBGColor:= TreeOptions.InheritNodeBG;
                if TreeOptions.InheritNodeBG  and assigned(SelectedNode) then
                   myEditorChrome.BGColor := SelectedNode.RTFBGColor;

                myTreeChrome := TreeChrome;
                GetTreeProperties( myTreeProperties );
                StartWithEditorTab := ( not TV.Focused );

                oldIconKind := myTreeProperties.IconKind;
                oldShowCheckboxes := myTreeProperties.CheckBoxes;
                oldHideChecked := myTreeProperties.HideChecked;       // [dpv]
              end;
              oldPlainText := ActiveKntFolder.PlainText;
            end;

            propDefaults : begin
              StartWithEditorTab := true;

              myEditorChrome := DefaultEditorChrome;
              myTabProperties := DefaultTabProperties;
              myEditorProperties := DefaultEditorProperties;

              // this picks the BG color of the current node,
              // rather than DEFAULT BG color for whole folder
              if myCurrentFileName <> '' then
                 mySaveFileDefaults := ( DEF_FN <> OrigDEF_FN );

              myTreeChrome := DefaultTreeChrome;
              myTreeProperties := DefaultTreeProperties;
            end;

          end;
        end;

        if ( Form_Defaults.ShowModal = mrOK ) then begin

          with Form_Defaults do begin
            NewPropertiesAction:= Action;        // User can now select the check 'Save as Defaults'

            KeyOptions.TabNameHistory := myTabNameHistory;
            KeyOptions.NodeNameHistory := myNodeNameHistory;

            if (PropertiesAction = propThisNote) and not myNoteIsReadOnly  then begin
                KntFile.Modified:= True;
                ActiveKntFolder.Modified:= True;
                UpdateNoteFileState( [fscModified] );

                ActiveKntFolder.SetTabProperties( myTabProperties, not (NewPropertiesAction = propDefaults));
                ActiveKntFolder.SetEditorProperties( myEditorProperties );
                ActiveKntFolder.EditorChrome := myEditorChrome;

                // reflect changes in controls
                ActiveKntFolder.UpdateEditor;
                ActiveKntFolder.UpdateTabSheet;

                EnsuredPlainText:= False;
                if ActiveKntFolder.PlainText then
                   EnsuredPlainText:= KntFile.EnsurePlainTextAndRemoveImages(ActiveKntFolder);

                with ActiveKntFolder do begin
                  // this will apply the selected BG color to current NODE
                  // besides setting the new default BG color for whole NOTE.
                  if TreeOptions.InheritNodeBG  and assigned(SelectedNode) then begin
                    SelectedNode.RTFBGColor := ActiveKntFolder.EditorChrome.BGColor;
                    ActiveKntFolder.Editor.Color := ActiveKntFolder.EditorChrome.BGColor;
                  end;

                  TreeLayoutChanged := ( VerticalLayout <> myTreeProperties.VerticalLayout );
                  SetTreeProperties( myTreeProperties );
                  TreeChrome := myTreeChrome;
                end;

                // update changes to tree control
                if ( oldIconKind <> myTreeProperties.IconKind ) then
                   ShowOrHideIcons( ActiveKntFolder, true );
                if ( oldShowCheckboxes <> myTreeProperties.CheckBoxes ) then
                   ShowOrHideCheckBoxes( ActiveKntFolder);
                if ( oldHideChecked <> myTreeProperties.HideChecked ) then    // [dpv]
                   if myTreeProperties.HideChecked then
                      HideChildNodesUponCheckState ( ActiveKntFolder, nil, csChecked)
                   else
                      ShowCheckedNodes ( ActiveKntFolder, nil);

                UpdateTreeChrome(ActiveKntFolder);

                // If we have changed to PlainText we will have already updated the editor from EnsurePlainTextAndRemoveImages
                if (not EnsuredPlainText) and (oldPlainText <> ActiveKntFolder.PlainText)
                     and (not TreeLayoutChanged)   then begin  // This is done if TreeLayoutChanged too
                   ActiveKntFolder.EditorToDataStream;  // Save the content of the editor according to the new formatting (Plain text / RTF)
                   ActiveKntFolder.DataStreamToEditor;
                end;

            end;

            if ApplyTreeChromeToAllNotes and HaveKntFolders( false, true ) then begin
                for i := 0 to KntFile.NoteCount -1 do begin
                   Folder:= KntFile.Folders[i];
                   if ((PropertiesAction = propThisNote) and (Folder = ActiveKntFolder)) or (Folder.ReadOnly) then
                       continue;
                   Folder.Modified:= True;
                   Folder.TreeChrome := myTreeChrome;
                   UpdateTreeChrome(Folder);
                end;
                KntFile.Modified:= True;
                UpdateNoteFileState( [fscModified] );
            end;


            if (PropertiesAction = propDefaults) or (NewPropertiesAction = propDefaults) then begin

                // must update all richedits and trees with the modified EditorOptions and TreeOptions:
                if HaveKntFolders( false, true ) then begin
                    for i := 0 to KntFile.NoteCount -1 do begin
                       Folder:= KntFile.Folders[i];
                       Folder.Editor.WordSelection := EditorOptions.WordSelect;
                       Folder.Editor.UndoLimit := EditorOptions.UndoLimit;
                       UpdateTreeOptions(Folder);
                    end;
                end;

                DefaultEditorChrome := myEditorChrome;
                DefaultEditorProperties := myEditorProperties;
                DefaultTabProperties := myTabProperties;
                DEFAULT_NEW_NOTE_NAME := DefaultTabProperties.Name;

                DefaultTreeChrome := myTreeChrome;
                DefaultTreeProperties := myTreeProperties;

                if mySaveFileDefaults then
                   DEF_FN := KntFile.FileName + ext_DEFAULTS

                else begin
                  // if mySaveFileDefaults was true before, and is now false, delete the file-specific .def file
                  if DEF_FN <> OrigDEF_FN then
                     deletefile( DEF_FN );
                  DEF_FN := OrigDEF_FN;
                end;

                SaveDefaults;
            end;
          end;

          if _LastZoomValue <> 100 then
             SetEditorZoom(ActiveKntFolder.Editor, _LastZoomValue, '' );

        end;

      finally
        UpdateCursorPos;
        UpdateNoteDisplay;
        Form_Defaults.Free;
      end;

      if TreeLayoutChanged then begin
        screen.Cursor := crHourGlass;
        Pages.OnChange := nil;
        try
          ActiveKntFolder.TreeWidth := 0;
          ActiveKntFolder.EditorToDataStream;
          ActiveKntFolder.Editor.Clear;
          ActiveKntFolder.Editor.ClearUndo;
          DestroyVCLControlsForNote( ActiveKntFolder, false );
          CreateVCLControlsForNote( ActiveKntFolder );
          ActiveKntFolder.DataStreamToEditor;
          SetUpVCLControls( ActiveKntFolder );
          FocusActiveNote;
        finally
          screen.Cursor := crDefault;
          Pages.OnChange := PagesChange;
        end;

      end;

  end;

end; // EditNoteProperties

end.
