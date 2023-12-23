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
   TreeNT,
   gf_miscvcl,
   gf_strings,
   kn_Defaults,
   kn_Const,
   kn_Info,
   kn_Macro,
   kn_NewNote,
   kn_TreeNoteMng,
   kn_FileMgr,
   kn_NoteObj,
   kn_BookmarksMng,
   kn_NoteFileMng;



    // note management
    procedure CreateNewNote;
    function NewNote( const DefaultNote, CanFocus : boolean; const aKind : TNoteType ) : boolean;
    procedure DeleteNote;
    procedure RenameNote;
    procedure EditNoteProperties( const PropertiesAction : TPropertiesAction );

implementation

uses
   kn_Global,
   kn_EditorUtils,
   kn_MacroMng,
   kn_ConfigMng,
   kn_VCLControlsMng,
   kn_Main;

resourcestring
  STR_01 = ' New note.';
  STR_02 = 'Are you sure you want to delete note "%s"?' + #13 + 'This operation cannot be undone.';
  STR_03 = 'Confirm deleting note';
  STR_04 = ' Note deleted.';
  STR_05 = ' Note renamed.';


function NewNote(
  const DefaultNote, CanFocus : boolean;
  const aKind : TNoteType ) : boolean;
var
  myNote: TTabNote;
  Form_NewNote : TForm_NewNote;
  FileWasBusy, TimerWasEnabled : boolean;
begin
  result := false;
  with Form_Main do
  begin
    if ( not HaveNotes( true, false )) then exit;
    myNote := nil;
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
        case aKind of
          ntRTF : myNote := TTabNote.Create;
          ntTree : myNote := TTreeNote.Create;
        end;
        myNote.SetEditorProperties( DefaultEditorProperties );
        myNote.SetTabProperties( DefaultTabProperties );
        myNote.EditorChrome := DefaultEditorChrome;

        if ( aKind = ntTree ) then
        begin
          TTreeNote( myNote ).SetTreeProperties( DefaultTreeProperties );
          TTreeNote( myNote ).TreeChrome := DefaultTreeChrome;
        end;
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
            TAB_TYPE := KeyOptions.DefaultNoteType;
            TAB_CHANGEABLE := true;
            myTreeProperties := DefaultTreeProperties;
            myTreeChrome := DefaultTreeChrome;
            myTreeOptions := TreeOptions;
          end;
          if ( Form_NewNote.ShowModal = mrOK ) then
          begin
            KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;

            case Form_NewNote.TAB_TYPE of
              ntRTF : myNote := TTabNote.Create;
              ntTree : myNote := TTreeNote.Create;
            end;
            KeyOptions.DefaultNoteType := Form_NewNote.TAB_TYPE;
            myNote.SetEditorProperties( Form_NewNote.myEditorProperties );
            myNote.SetTabProperties( Form_NewNote.myTabProperties );
            myNote.EditorChrome := Form_NewNote.myChrome;
            KeyOptions.NodeNameHistory := Form_NewNote.myNodeNameHistory;
            if ( myNote.Kind = ntTree ) then
            with TTreeNote( myNote ) do
            begin
              SetTreeProperties( Form_NewNote.myTreeProperties );
              TreeChrome := Form_NewNote.myTreeChrome;
            end;
          end;
        finally
          Form_NewNote.Free;
        end;
      end;
      if assigned( myNote ) then
      begin
        NoteFile.AddNote( myNote );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_01;
        try
          with Form_Main do begin
          CreateVCLControlsForNote( myNote );
          SetUpVCLControls( myNote );
          AddToFileManager( NoteFile.FileName, NoteFile ); // update manager (number of notes has changed)
          end;

        finally
          NoteFile.Modified := ( not DefaultNote );
          myNote.TabSheet.TabVisible := true; // was created hidden
          ActiveNote := myNote;
        end;
        if ( myNote.Kind = ntTree ) then
          TreeNoteNewNode( nil, tnTop, nil, '', true );
        Form_Main.Pages.ActivePage := myNote.TabSheet;
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
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
    result := assigned( myNote );
   {$IFDEF KNT_DEBUG}
    if assigned( myNote ) then
      Log.Add( 'Added new note: ' + myNote.Name )
    else
      Log.Add( 'New note NOT added.' );
   {$ENDIF}
  end;
end; // NewNote

procedure DeleteNote;
var
  pidx : integer;
begin
  with Form_Main do
  begin
      if ( not HaveNotes( true, true )) then exit;
      if ( not assigned( ActiveNote )) then exit;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      if KeyOptions.ConfirmTabDelete then
      begin
        if ( DoMessageBox(
            Format( STR_02, [RemoveAccelChar( ActiveNote.Name )] ),
            STR_03,
            MB_YESNO+MB_ICONEXCLAMATION+MB_DEFBUTTON2+MB_APPLMODAL) <> ID_YES ) then exit;
      end;

      try
        // clear Clipboard capture state if this note is ClipCapNote
        if ( NoteFile.ClipCapNote = ActiveNote ) then
        begin
          TB_ClipCap.Down := false;
          ClipCapNode := nil;
          Pages.MarkedPage := nil;
          ToggleClipCap( false, ActiveNote ); // turn it OFF
        end;

        // clear all bookmarks pointing to this note
        for pidx := 0 to MAX_BOOKMARKS do
        begin
          if ( NoteFile.Bookmarks[pidx].Note = ActiveNote ) then
            BookmarkClear( pidx );
        end;

        pidx := ActiveNote.TabSheet.TabIndex;

        Pages.OnChange := nil;

        DestroyVCLControlsForNote( ActiveNote, true );
        NoteFile.RemoveImagesCountReferences(ActiveNote);
        NoteFile.DeleteNote( ActiveNote );
        ActiveNote := nil;
        AddToFileManager( NoteFile.FileName, NoteFile ); // update manager (number of notes has changed)

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
          if assigned( ActiveNote ) then
            ActiveNote.Editor.SetFocus;
        except
        end;
        Pages.OnChange := PagesChange;
        StatusBar.Panels[PANEL_HINT].text := STR_04;
        NoteFile.Modified := true;
        UpdateNoteFileState( [fscModified] );
      end;
    end;
end; // DeleteNote

procedure CreateNewNote;
begin
  if assigned(ActiveNote) then
     ActiveNote.EditorToDataStream;
  if NewNote( false, true, ntRTF ) then
  begin
    Application.ProcessMessages;
    if KeyOptions.RunAutoMacros then
    begin
      case ActiveNote.Kind of
        ntRTF :
          ExecuteMacro( _MACRO_AUTORUN_NEW_NOTE, '' );
        ntTree :
          ExecuteMacro( _MACRO_AUTORUN_NEW_TREE, '' );
      end;
    end;
  end;
end; // CreateNewNote

procedure RenameNote;
var
  Form_NewNote : TForm_NewNote;
begin
  with Form_Main do begin
      if ( not HaveNotes( true, true )) then exit;
      if ( not assigned( ActiveNote )) then exit;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      Form_NewNote := TForm_NewNote.Create( Form_Main );
      try
        with Form_NewNote do
        begin
          TAB_CHANGEABLE := false;
          ShowHint := KeyOptions.ShowTooltips;
          myTabProperties.Name := ActiveNote.Name;
          myTabProperties.ImageIndex := ActiveNote.ImageIndex;
          myTabNameHistory := KeyOptions.TabNameHistory;
          myHistoryCnt := FindOptions.HistoryMaxCnt;
          Button_Properties.Enabled := false;
          Button_Properties.Visible := false;
          TAB_TYPE := ActiveNote.Kind;
        end;
        if ( Form_NewNote.ShowModal = mrOK ) then
        begin
          KeyOptions.TabNameHistory := Form_NewNote.myTabNameHistory;
          NoteFile.Modified := true;
          ActiveNote.Name := Form_NewNote.myTabProperties.Name;
          ActiveNote.ImageIndex := Form_NewNote.myTabProperties.ImageIndex;
          StatusBar.Panels[PANEL_HINT].Text := STR_05;
        end;
      finally
        Form_NewNote.Free;
        NoteFile.Modified := true;
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
  Note: TTabNote;
  NewPropertiesAction : TPropertiesAction;

begin

  with Form_Main do begin
      if (( PropertiesAction = propThisNote ) and ( not assigned( ActiveNote ))) then
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
          myNoteIsReadOnly := (( PropertiesAction = propThisNote ) and NoteIsReadOnly( ActiveNote, false ));

          myNodeNameHistory := KeyOptions.NodeNameHistory;

          myCurrentFileName:= '';
          if assigned(NoteFile) and (NoteFile.FileName <> '') then
             myCurrentFileName := ExtractFilename( NoteFile.FileName );


          case PropertiesAction of
            propThisNote : begin

              myEditorChrome := ActiveNote.EditorChrome;
              NoteKind := ActiveNote.Kind;
              ActiveNote.GetTabProperties( myTabProperties );
              ActiveNote.GetEditorProperties( myEditorProperties );

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
                 of a note after the state of WordWrap have been changed (in the active node or simply for
                 selecting nodes with different WodWrap state) always caused formatting to be lost. Since version 1.6.5...

              CB_WordWrap.Enabled := false;
              CB_WordWrap.Visible := false;
              }
              CB_WordWrap.Checked:= myEditorProperties.WordWrap;    // *1


              if ( ActiveNote.Kind = ntTree ) then begin
                with TTreeNote( ActiveNote ) do begin

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
              end;
              oldPlainText := ActiveNote.PlainText;
            end;

            propDefaults : begin
              StartWithEditorTab := true;

              myEditorChrome := DefaultEditorChrome;
              myTabProperties := DefaultTabProperties;
              myEditorProperties := DefaultEditorProperties;

              // this picks the BG color of the current node,
              // rather than DEFAULT BG color for whole note
              if myCurrentFileName <> '' then
                 mySaveFileDefaults := ( DEF_FN <> OrigDEF_FN );

              NoteKind := ntTree;
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
                NoteFile.Modified:= True;
                ActiveNote.Modified:= True;
                UpdateNoteFileState( [fscModified] );

                ActiveNote.SetTabProperties( myTabProperties, not (NewPropertiesAction = propDefaults));
                ActiveNote.SetEditorProperties( myEditorProperties );
                ActiveNote.EditorChrome := myEditorChrome;

                // reflect changes in controls
                ActiveNote.UpdateEditor;
                ActiveNote.UpdateTabSheet;

                EnsuredPlainText:= False;
                if ActiveNote.PlainText then
                   EnsuredPlainText:= NoteFile.EnsurePlainTextAndRemoveImages(ActiveNote);

                if ( ActiveNote.Kind = ntTree ) then begin
                  with TTreeNote( ActiveNote ) do begin
                    // this will apply the selected BG color to current NODE
                    // besides setting the new default BG color for whole NOTE.
                    if TreeOptions.InheritNodeBG  and assigned(SelectedNode) then begin
                      SelectedNode.RTFBGColor := ActiveNote.EditorChrome.BGColor;
                      ActiveNote.Editor.Color := ActiveNote.EditorChrome.BGColor;
                    end;

                    TreeLayoutChanged := ( VerticalLayout <> myTreeProperties.VerticalLayout );
                    SetTreeProperties( myTreeProperties );
                    TreeChrome := myTreeChrome;
                  end;

                  // update changes to tree control
                  if ( oldIconKind <> myTreeProperties.IconKind ) then
                     ShowOrHideIcons( TTreeNote( ActiveNote ), true );
                  if ( oldShowCheckboxes <> myTreeProperties.CheckBoxes ) then
                     ShowOrHideCheckBoxes( TTreeNote( ActiveNote ));
                  if ( oldHideChecked <> myTreeProperties.HideChecked ) then    // [dpv]
                     if myTreeProperties.HideChecked then
                        HideChildNodesUponCheckState ( TTreeNote( ActiveNote ), nil, csChecked)
                     else
                        ShowCheckedNodes ( TTreeNote( ActiveNote ), nil);

                  UpdateTreeChrome(TTreeNote(ActiveNote));
                end;

                // If we have changed to PlainText we will have already updated the editor from EnsurePlainTextAndRemoveImages
                if (not EnsuredPlainText) and (oldPlainText <> ActiveNote.PlainText)
                     and (not TreeLayoutChanged or (ActiveNote.Kind <> ntTree))   then begin  // This is done if TreeLayoutChanged too
                   ActiveNote.EditorToDataStream;  // Save the content of the editor according to the new formatting (Plain text / RTF)
                   ActiveNote.DataStreamToEditor;
                end;

            end;

            if ApplyTreeChromeToAllNotes and HaveNotes( false, true ) then begin
                for i := 0 to NoteFile.NoteCount -1 do begin
                   Note:= NoteFile.Notes[i];
                   if ((PropertiesAction = propThisNote) and (Note = ActiveNote)) or (Note.ReadOnly) then
                       continue;
                   if Note.Kind = ntTree then begin
                      Note.Modified:= True;
                      TTreeNote(Note).TreeChrome := myTreeChrome;
                      UpdateTreeChrome(TTreeNote(Note));
                   end;
                end;
                NoteFile.Modified:= True;
                UpdateNoteFileState( [fscModified] );
            end;


            if (PropertiesAction = propDefaults) or (NewPropertiesAction = propDefaults) then begin

                // must update all richedits and trees with the modified EditorOptions and TreeOptions:
                if HaveNotes( false, true ) then begin
                    for i := 0 to NoteFile.NoteCount -1 do begin
                       Note:= NoteFile.Notes[i];
                       Note.Editor.WordSelection := EditorOptions.WordSelect;
                       Note.Editor.UndoLimit := EditorOptions.UndoLimit;
                       if Note.Kind = ntTree then
                          UpdateTreeOptions(TTreeNote(Note));
                    end;
                end;

                DefaultEditorChrome := myEditorChrome;
                DefaultEditorProperties := myEditorProperties;
                DefaultTabProperties := myTabProperties;
                DEFAULT_NEW_NOTE_NAME := DefaultTabProperties.Name;

                DefaultTreeChrome := myTreeChrome;
                DefaultTreeProperties := myTreeProperties;

                if mySaveFileDefaults then
                   DEF_FN := NoteFile.FileName + ext_DEFAULTS

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
             SetEditorZoom(ActiveNote.Editor, _LastZoomValue, '' );

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
          TTreeNote( ActiveNote ).TreeWidth := 0;
          ActiveNote.EditorToDataStream;
          ActiveNote.Editor.Clear;
          ActiveNote.Editor.ClearUndo;
          DestroyVCLControlsForNote( ActiveNote, false );
          CreateVCLControlsForNote( ActiveNote );
          ActiveNote.DataStreamToEditor;
          SetUpVCLControls( ActiveNote );
          FocusActiveNote;
        finally
          screen.Cursor := crDefault;
          Pages.OnChange := PagesChange;
        end;

      end;

  end;

end; // EditNoteProperties

end.
