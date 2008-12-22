unit kn_MacroMng;

interface
Uses Classes,
     kn_Macro, kn_Cmd, kn_Info, kn_NoteObj;

var
    StartupMacroFile : string; // macro to be autorun on program startup
    StartupPluginFile : string; // plugin to be autorun on program startup
    IsRecordingMacro : boolean;
    IsRunningMacro : boolean;
    MacroAbortRequest : boolean;

    LastMacroFN : string;
    LastEditCmd : TEditCmd; // last ecXXX command issued

    CommandRecall : TCommandRecall; // record that keeps parameters for last ecXXX command
    MacroRecordingPaused : boolean;
    MacroErrorAbort : boolean;
    ActiveMacro : TMacro;

    FontFormatToCopy : TkntFontAttributes;
    ParaFormatToCopy : TkntParaAttributes;


    // macro-related routines
    procedure MacroInitialize;
    procedure EnumerateMacros;
    procedure EditMacro( const AsNew  : boolean );
    procedure DeleteMacro;
    procedure ExecuteMacro( aFileName, aMacroName : string );
    procedure PlayMacro( const Macro : TMacro );
    procedure RecordMacro;
    procedure PauseRecordingMacro;
    procedure StopRecordingMacro;
    procedure AddMacroKeyPress( const Key : Word; const Shift : TShiftState );
    procedure AddMacroEditCommand( aCmd : TEditCmd );
    function GetCurrentMacro( const DoWarn : boolean ) : TMacro;
    function GetMacroByName( const aName : string; const DoWarn : boolean ) : TMacro;
    procedure ExecuteMacroFile;
    procedure AddUserMacroCommand;
    function MacroProcess( const DoWarn : boolean ) : boolean;
    function GetMacroIconIndex( const Macro : TMacro ) : integer;

    // edit commands
    procedure PerformCmd( aCmd : TEditCmd );
    procedure PerformCmdEx( aCmd : TEditCmd );
    procedure RepeatLastCommand;
    procedure UpdateLastCommand( const aCMD : TEditCmd );
    procedure PerformCustomFuncKey( const Key : Word; const Shift : TShiftState ); // OBSOLETE
    function RunParagraphDialog : boolean;
    function RunLanguageDialog : boolean;


implementation

uses Windows, Messages, SysUtils, Forms, Dialogs, Graphics, StdCtrls, Controls, Clipbrd,
     TreeNT, RichEdit, RxStrUtils, RxRichEd,
     gf_miscvcl, gf_strings, gf_misc,
     kn_MacroCmd, kn_MacroCmdSelect, kn_MacroEdit, kn_Main, kn_Global,
     kn_Const, kn_NodeList, kn_DateTime, kn_LanguageSel, kn_Paragraph,
     kn_FindReplaceMng, kn_StyleMng, kn_FavoritesMng, kn_BookmarksMng,
     kn_TreeNoteMng, kn_NoteMng,kn_PluginsMng, kn_TemplateMng, kn_NoteFileMng,
     kn_EditorUtils, kn_VCLControlsMng;

var
    RecallingCommand : boolean; // if TRUE, we use information in CommandRecall


function GetMacroIconIndex( const Macro : TMacro ) : integer;
begin
  result := MACRO_IMAGE_BASE;
  if ( pos ( _MACRO_AUTORUN_PREFIX, Macro.FileName ) = 1 ) then
    inc( result );
end; // GetMacroIconIndex

procedure EnumerateMacros;
var
  i : integer;
  macro : TMacro;
begin

  // Combo_Macro.Items.BeginUpdate;
  Form_Main.ListBox_ResMacro.Items.BeginUpdate;
  try

    // Combo_Macro.Items.Clear;
    Form_Main.ListBox_ResMacro.Items.Clear;

    ClearMacroList;
    LoadMacroList;

    for i := 1 to Macro_List.Count do
    begin
      Macro :=  TMacro( Macro_List.Objects[pred(i)] );
      Form_Main.ListBox_ResMacro.AddItem( Macro.Name, cbUnchecked, GetMacroIconIndex( Macro ));
      // Combo_Macro.AddItem( Macro.Name, GetMacroIconIndex( Macro ));
    end;

  finally
    // Combo_Macro.Items.EndUpdate;
    Form_Main.ListBox_ResMacro.Items.EndUpdate;
  end;


  { if ( Combo_Macro.Items.Count > 0 ) then
    Combo_Macro.ItemIndex := 0; }

  if ( Form_Main.ListBox_ResMacro.Items.Count > 0 ) then
    Form_Main.ListBox_ResMacro.ItemIndex := 0;

end; // EnumerateMacros

procedure RecordMacro;
begin

  if IsRecordingMacro then
  begin
    StopRecordingMacro;
    exit;
  end;

  if MacroProcess( true ) then exit;
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;

  if ( not CheckFolder( 'Macro', Macro_Folder, true, true )) then exit;

  EditMacro( true ); // create a new macro
  if ( ActiveMacro = nil ) then exit;

  LastEditCmd := ecNone;
  UpdateLastCommand( ecNone );
  try
    ActiveNote.Editor.SetFocus;
  except
  end;

  // preset or clear some global settings
  FindOptions.Pattern := ''; // enforce new search

  IsRecordingMacro := true;
  MacroRecordingPaused := false;
  with Form_Main do
  begin
    TB_MacroRecord.ImageIndex := TB_MacroRecord.ImageIndex + 1;
    TB_MacroRecord.Hint := 'Stop recording macro';
    MacMMacro_Record.Caption := '&Stop Recording Macro';
    MacMMacro_Record.Hint := TB_MacroRecord.Hint;
    MacMMacroUserCommand.Enabled := true;

    Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( ' Recording macro "%s"', [ActiveMacro.Name] );
    SelectStatusbarGlyph( true );
  end;

end; // RecordMacro

procedure AddMacroKeyPress( const Key : Word; const Shift : TShiftState );
begin
  if MacroRecordingPaused then exit;
  if ( not assigned( ActiveMacro )) then exit;

  ActiveMacro.Lines.Add( Format(
    '%d%s%s',
    [Key,_MACRO_DELIMITER_CHAR,ShiftStateToStr( Shift )]
  ));
end; // AddMacroKeyPress

procedure AddMacroEditCommand( aCmd : TEditCmd );
var
  OnPlayShowDlg : boolean;
begin
  if MacroRecordingPaused then exit;
  if ( not assigned( ActiveMacro )) then exit;

  if ( aCmd = ecNone ) then exit;

  if ( aCmd in CommandsProhibitedInMacros ) then
  begin
    messagedlg( Format(
      'Command "%s" cannot be included in macros. This command has been executed but will not be recorded in the macro.',
      [EDITCMD_NAMES[aCmd]]
      ), mtWarning, [mbOK], 0 );
    exit;
  end;

  if ( aCMD in EditCommandsWithNoArgs ) then
  begin
    ActiveMacro.Lines.Add( Format(
      '%s%s',
      [_MACRO_CMD_CHAR,EDITCMD_MACRO_NAMES[aCMD]]
    ));
  end
  else
  if ( aCMD in EditCommandsWithDialogs ) then
  begin
    case messagedlg(
      'You have executed a command which opens a dialog box. KeyNote can ' +
      'remember the values you have just selected and use them when ' +
      'replaying the macro, OR KeyNote can display the dialog box again to let you select values ' +
      'each time the macro is executed. Do you want KeyNote to remember current values?' + #13#13 +
      'Click YES to save current values. Click NO to always open the dialog box. Click CANCEL to ' +
      'skip this command and continue recording macro.',
      mtConfirmation, [mbYes,mbNo,mbCancel], 0 ) of
        mrYes : OnPlayShowDlg := false;
        mrNo : OnPlayShowDlg := true;
        else
          exit;
    end;

    if OnPlayShowDlg then
    begin
      ActiveMacro.Lines.Add( Format(
        '%s%s',
        [_MACRO_CMD_CHAR,
        EDITCMD_MACRO_NAMES[aCMD]
      ]));
    end
    else
    begin
      case aCMD of

        ecBGColorDlg : begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%s',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            ColorToString( CommandRecall.Color )
          ]));
        end;
        ecFontColorDlg : begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%s',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            ColorToString( CommandRecall.Font.Color )
          ]));
        end;
        ecFontDlg : with CommandRecall.Font do
        begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%d%s%s%s%s%s%d%s%s',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            Charset,
            _MACRO_DELIMITER_CHAR,
            ColorToString( Color ),
            _MACRO_DELIMITER_CHAR,
            Name,
            _MACRO_DELIMITER_CHAR,
            Size,
            _MACRO_DELIMITER_CHAR,
            FontStyleToStr( Style )
          ]));
        end;
        ecGoTo : begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%s',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            CommandRecall.GoToidx
          ]));
        end;
        ecHighlightDlg : begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%s',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            ColorToString( CommandRecall.Color )
          ]));
        end;
        ecLanguage : begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%d',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            CommandRecall.Language
          ]));
        end;
        ecParaDlg : with CommandRecall.Para do
        begin
          ActiveMacro.Lines.Add( Format(
            '%s%s%s%d%s%d%s%d%s%d%s%d%s%d%s%d%s%d',
            [_MACRO_CMD_CHAR,
            EDITCMD_MACRO_NAMES[aCMD],
            _MACRO_DELIMITER_CHAR,
            ord( SpacingRule ),
            _MACRO_DELIMITER_CHAR,
            ord( LIndent ),
            _MACRO_DELIMITER_CHAR,
            ord( RIndent ),
            _MACRO_DELIMITER_CHAR,
            ord( FIndent ),
            _MACRO_DELIMITER_CHAR,
            SpaceBefore,
            _MACRO_DELIMITER_CHAR,
            SpaceAfter,
            _MACRO_DELIMITER_CHAR,
            ord( Numbering ),
            _MACRO_DELIMITER_CHAR,
            ord( Alignment )
          ]));
        end;
      end;
    end;

  end
  else
  begin
    // special cases
    case aCMD of
      ecFontName : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%s',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          CommandRecall.Font.Name
        ]));
      end;
      ecFontSize : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%d',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          CommandRecall.Font.Size
        ]));
      end;
      ecFontColor, ecFontColorBtn : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%s',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          ColorToString( CommandRecall.Font.Color )
        ]));
      end;
      ecHighlight, ecHighlightBtn : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%s',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          ColorToString( CommandRecall.Color )
        ]));
      end;
      ecInsCharacter : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%d%s%d%s%s%s%d',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          CommandRecall.CharInfo.Code,
          _MACRO_DELIMITER_CHAR,
          CommandRecall.CharInfo.Count,
          _MACRO_DELIMITER_CHAR,
          CommandRecall.CharInfo.Name,
          _MACRO_DELIMITER_CHAR,
          CommandRecall.CharInfo.Charset
        ]));
      end;
      ecStyleApply : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%s',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          CommandRecall.StyleName
        ]));
      end;
      ecFindText : begin
        ActiveMacro.Lines.Add( Format(
          '%s%s%s%s%s%s%s%s',
          [_MACRO_CMD_CHAR,
          EDITCMD_MACRO_NAMES[aCMD],
          _MACRO_DELIMITER_CHAR,
          FindOptions.Pattern,
          _MACRO_DELIMITER_CHAR,
          BOOLEANSTR[FindOptions.MatchCase],
          _MACRO_DELIMITER_CHAR,
          BOOLEANSTR[FindOptions.WholeWordsOnly]
        ]));
      end;
    end;
  end;


end; // AddMacroEditCommand

procedure PauseRecordingMacro;
begin
  if ( not IsRecordingMacro ) then exit;
  MacroRecordingPaused := ( not MacroRecordingPaused );
  Form_Main.TB_MacroPause.Down := MacroRecordingPaused;
  if MacroRecordingPaused then
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro recording PAUSED'
  else
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro recording RESUMED';
end; // PauseRecordingMacro

procedure StopRecordingMacro;
begin

  try
    if ( ActiveMacro = nil ) then exit;
    try
      if MacroRecordingPaused then
        PauseRecordingMacro;
      Form_Main.MacMMacroUserCommand.Enabled := false;

      if ( ActiveMacro.Lines.Count = 0 ) then
      begin
        Messagedlg( Format(
          'Macro "%s" contains no commands and will be discarded.',
          [ActiveMacro.Name]
        ), mtInformation, [mbOK], 0 );
        ActiveMacro.Free;
        exit;
      end;

      if ( messagedlg( Format(
        'Save new macro "%s" (%d lines)?',
        [ActiveMacro.Name,ActiveMacro.Lines.Count]
        ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrNo ) then
      begin
        if ( not ActiveMacro.Save ) then
        begin
          messagedlg( Format(
            'Error saving macro "%s": %s', [ActiveMacro.FileName,ActiveMacro.LastError]
            ), mtError, [mbOK], 0 );
          ActiveMacro.Free;
          exit;
        end;
        // Combo_Macro.ItemIndex := Combo_Macro.AddItem( ActiveMacro.Name, GetMacroIconIndex( ActiveMacro ));
        Form_Main.ListBox_ResMacro.ItemIndex := Form_Main.ListBox_ResMacro.AddItem( ActiveMacro.Name, cbUnchecked, GetMacroIconIndex( ActiveMacro ));
        Macro_List.AddObject( ActiveMacro.Name, ActiveMacro );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro recorded and saved.'
      end
      else
      begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro discarded.';
        ActiveMacro.Free;
      end;
    except
      on E : Exception do
      begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro error';
        messagedlg( Format(
          'Error adding new macro "%s": %s',
          [ActiveMacro.Name,E.Message] ), mtError, [mbOK], 0 );
        ActiveMacro.Free;
      end;
    end;
  finally
    ActiveMacro := nil;
    IsRecordingMacro := false;
    MacroRecordingPaused := false;
    with Form_Main do
    begin
SelectStatusbarGlyph( true );
TB_MacroRecord.ImageIndex := TB_MacroRecord.ImageIndex - 1;
TB_MacroRecord.Hint := 'Record a new macro';
MacMMacro_Record.Caption := '&Record Macro...';
MacMMacro_Record.Hint := TB_MacroRecord.Hint;
    end;
  end;
end; // StopRecordingMacro

procedure ExecuteMacro( aFileName, aMacroName : string );
var
  macro : TMacro;
  wasNewMacro, wasreadonly : boolean;
begin

  // if aFileName is specified, the macro is loaded from the file
  // specified. Else, if aName is specified, the procedure looks
  // for a macro in Macro_List

  if MacroProcess( true ) then exit;

  Macro := nil;
  wasNewMacro := false;

  // Normally, macros cannot run if there are no notes,
  // Exception: We do allow _MACRO_AUTORUN_NEW_FILE to run,
  // otherwise the feature would be impossible to implement.
  // It is the macro author's responsibility to make sure
  // that the macro dooesn't do anything unreasonable, like
  // inserting text before creating a note
  if ( aFileName <> _MACRO_AUTORUN_NEW_FILE ) then
  begin
    if ( not Form_Main.HaveNotes( true, true )) then exit;
    if ( not assigned( ActiveNote )) then exit;
  end;

  wasreadonly := Form_Main.NoteIsReadOnly( ActiveNote, false );
  if wasreadonly then
  begin
    if ( messagedlg( Format(
      'Active note "%s" is Read-only. Running the macro may cause the note to be modified. Do you want the macro to run anyway?',
      [ActiveNote.Name] ),
      mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
    ActiveNote.ReadOnly := false;
  end;

  if ( aFileName <> '' ) then
  begin

    if ( pos( '\', aFileName ) = 0 ) then
      aFileName := Macro_Folder + aFileName;

    Macro := TMacro.Create;
    Macro.FileName := aFileName;
    if ( not Macro.Load ) then
    begin
      messagedlg( Format(
        'Cannot load macro file "%s". Reason: %s',
        [aFileName,Macro.LastError]
      ), mtError, [mbOK], 0 );
      Macro.Free;
      Macro := nil;
    end;
    wasNewMacro := true;
  end
  else
  begin

    if ( aMacroName = '' ) then
    begin
      Macro := GetCurrentMacro( true );
    end
    else
    begin
      Macro := GetMacroByName( aMacroName, true );
    end;

  end;
  if ( macro = nil ) then
    exit;

  IsRunningMacro := true;
  MacroAbortRequest := false;
  MacroErrorAbort := false;
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(
    'Running macro "%s"', [Macro.Name] );
  screen.Cursor := crAppStart;
  SelectStatusbarGlyph( true );

  try
    ActiveNote.FocusMemory := focRTF;
    ActiveNote.Editor.SetFocus;
  except
  end;

  LastMacroFN := extractfilename( Macro.FileName );
  Form_Main.MMToolsMacroRunLast.Hint := Format(
    'Execute most recent macro "%s"',
    [extractfilename( LastMacroFN )]
  );

  try
    try
      if ( not Macro.Load ) then
      begin
        messagedlg( Format(
          'Error loading macro "%s": %s', [Macro.FileName,Macro.LastError]
          ), mtError, [mbOK], 0 );
      end;

      if ( Macro.Version.Major > _MACRO_VERSION_MAJOR ) then
      begin
        messagedlg( Format(
          'Cannot execute macro "%s": This macro requires a newer version of KeyNote.', [Macro.FileName]
          ), mtError, [mbOK], 0 );
      end;

      PlayMacro( Macro );

    except
      On E : Exception do
      begin
        messagedlg( E.message, mtError, [mbOK], 0 );
        exit;
      end;
    end;

  finally

    if MacroErrorAbort then
    begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Unexpected error while executing macro';
    end
    else
    if MacroAbortRequest then
    begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro was aborted by user';
    end
    else
    begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' Macro done';
    end;
    try
      if wasNewMacro then
        Macro.Free
      else
        Macro.Clear;
    except
    end;
    MacroAbortRequest := false;
    IsRunningMacro := false;
    screen.Cursor := crDefault;
    SelectStatusbarGlyph( true );
    if wasreadonly then
      ActiveNote.ReadOnly := true;
    UpdateNoteFileState( [fscModified] );
  end;

  try
    ActiveNote.FocusMemory := focRTF;
    ActiveNote.Editor.SetFocus;
  except
  end;

end; // ExecuteMacro


procedure EditMacro( const AsNew  : boolean );
var
  Form_Macro: TForm_Macro;
  Macro : TMacro;
begin

  if MacroProcess( true ) then exit;

  if AsNew then
  begin
    Macro := TMacro.Create;
  end
  else
  begin
    Macro := GetCurrentMacro( true );
    if ( Macro = nil ) then
      exit;

    if ( not Macro.Load ) then
    begin
      messagedlg( Format( 'Error loading macro "%s": %s', [Macro.FileName,Macro.LastError] ), mtError, [mbOK], 0 );
      Macro.Free;
      exit;
    end;

  end;

  Form_Macro := TForm_Macro.Create( Form_Main );
  try
    with Form_Macro do
    begin
      MName := Macro.Name;
      MDesc := Macro.Description;
      MDate := Macro.DateModified;
      MFileName := Macro.FileName;
      MAbort := Macro.AbortOnError;
      myNewMacro := AsNew;
    end;

    if ( Form_Macro.ShowModal = mrOK ) then
    begin
      with Form_Macro do
      begin
        Macro.Name := MName;
        Macro.Description := MDesc;
        Macro.DateModified := DateTimeToStr( now );
        Macro.AbortOnError := MAbort;

        if AsNew then
        begin
          Macro.FileName := MFileName;
          ActiveMacro := Macro;
        end
        else
        begin
          // update existing macro
          if ( MName <> OriginalName ) then
          begin

            Form_Main.ListBox_ResMacro.Items.Delete( Form_Main.ListBox_ResMacro.ItemIndex );
            Form_Main.ListBox_ResMacro.ItemIndex := Form_Main.ListBox_ResMacro.AddItem( Macro.Name, cbUnchecked, GetMacroIconIndex( Macro ));

            Macro_List.Delete( Macro_List.IndexOf( OriginalName ));
            Macro_List.AddObject( MName, Macro );
          end;

          if ( not Macro.Save ) then
          begin
            messagedlg( Format(
              'Error saving macro "%s": %s', [Macro.FileName,Macro.LastError]
              ), mtError, [mbOK], 0 );
            exit;
          end;

        end;
      end;
    end
    else
    begin
      ActiveMacro := nil;
      if AsNew then
        Macro.Free;
    end;

  finally
    Form_Macro.Free;
  end;

end; // EditMacro

procedure DeleteMacro;
var
  Macro : TMacro;
  DeleteSuccess : boolean;
  i : integer;
begin

  if MacroProcess( true ) then exit;
  Macro := GetCurrentMacro( true );
  if ( macro = nil ) then exit;

  if ( messagedlg( Format(
    'OK to delete selected macro "%s"?',
    [Macro.Name]
    ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

  DeleteSuccess := false;
  try
    try
      if ( not deletefile( Macro_Folder + Macro.FileName )) then
      begin
        DeleteSuccess := false;
        messagedlg( Format(
          'Cannot delete macro file "%s"',
          [Macro.Filename] ), mtError, [mbOK], 0 );
        exit;
      end;
      i := Form_Main.ListBox_ResMacro.ItemIndex;
      Form_Main.ListBox_ResMacro.Items.Delete( i );
      i := Macro_List.IndexOf( Macro.Name );
      if ( i >= 0 ) then
      begin
        Macro.Free;
        Macro_List.Delete( i );
      end;
      DeleteSuccess := true;

    except
      on E : Exception do
      begin
        DeleteSuccess := false;
        messagedlg( 'Error while deleting macro: ' + E.Message, mtError, [mbOK], 0 );
      end;
    end;
  finally
    if DeleteSuccess then
    begin
      if ( Form_Main.ListBox_ResMacro.Items.Count > 0 ) then
        Form_Main.ListBox_ResMacro.ItemIndex := 0;
    end;
  end;


end; // DeleteMacro

procedure PlayMacro( const Macro : TMacro );
var
  MacroCmds, EditCmds, ArgList : TStringList;
  newMacro : TMacro;
  line : string;
  p, i, linecnt, cmdcnt, ErrorCnt, cmdidx : integer;
  cmdstr, argstr : string;
  argint, counter : integer;
  macrocmd : TMacroCmd;
  editcmd : TEditCmd;
  MacroFinished : boolean;
  myFontStyle : TFontStyle;
  sndKey : word;
  sndShift : TShiftState;


      procedure AbortMacro( const s : string; i : integer; const line : string );
      begin
        messagedlg( Format(
          'Macro aborted on line %d: "%s"' + #13 +
          'Reason: %s',
          [i,copy( line, 1, 127 ),s]
        ), mtError, [mbOK], 0 );
        MacroFinished := true;
        MacroErrorAbort := true;
      end;


begin
  if ( not assigned( Macro )) then exit;
  if ( MacroAbortRequest or MacroErrorAbort ) then exit;

  linecnt := 1;
  ErrorCnt := 0;

  cmdstr := '';
  argstr := '';
  argint := 0;

  myFontStyle := fsBold;

  MacroCmds := TStringList.Create;
  MacroCmds.Capacity := succ( ord( high( TMacroCmd )));
  for macrocmd := low( TMacroCmd ) to high( TMacroCmd ) do
    MacroCmds.Add( MACRO_CMD_NAMES[macrocmd] );

  EditCmds := TStringList.Create;
  for editcmd := low( TEditCmd ) to high( TEditCmd ) do
    EditCmds.Add( EDITCMD_MACRO_NAMES[editcmd] );

  ArgList := TStringList.Create;

  try
    try
      repeat

        MacroFinished := true;
        for i := 1 to Macro.Lines.Count do
        begin

          Application.ProcessMessages;
          if MacroAbortRequest then
          begin
            MacroFinished := true;
            break;
          end;

          linecnt := i;
          line := Macro.Lines[pred( i )];
          if ( line = '' ) then continue;
          case line[1] of
            _MACRO_COMMENT_CHAR : continue;

            _MACRO_CMD_CHAR : begin
              inc( cmdcnt );
              // execute internal command
              delete( line, 1, 1 );
              p := pos( _MACRO_DELIMITER_CHAR, line );
              cmdstr := '';
              argstr := '';
              if ( p = 0 ) then
              begin
                cmdstr := line;
              end
              else
              begin
                cmdstr := copy( line, 1, pred( p ));
                argstr := copy( line, succ( p ), length( line ));
              end;

              cmdstr := lowercase( cmdstr );
              cmdidx := EditCmds.IndexOf( cmdstr );
              if ( cmdidx < 0 ) then
              begin
                if Macro.AbortOnError then
                begin
                  AbortMacro( 'unknown command', i, Macro.Lines[pred( i )] );
                  break;
                end
                else
                begin
                  inc( ErrorCnt );
                  continue;
                end;
              end;

              EditCmd := TEditCmd( cmdidx );
              if ( EditCmd in EditCommandsWithNoArgs ) then
              begin
                LastEditCmd := EditCmd;
                RepeatLastCommand;
              end
              else
              if ( EditCmd in EditCommandsWithDialogs ) then
              begin
                if ( argstr = '' ) then
                begin
                  if ( EditCmd in EditCommandsEx ) then
                    PerformCmdEx( EditCmd )
                  else
                    PerformCmd( EditCmd );
                end
                else
                begin
                  try
                    case EditCmd of
                      ecBGColorDlg : begin
                        CommandRecall.Color := StringToColor( argstr );
                      end;
                      ecFontColorDlg : begin
                        CommandRecall.Font.Color := StringToColor( argstr );
                      end;
                      ecFontDlg : begin
                        CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                        with CommandRecall.Font do
                        begin
                          Charset := strtoint( ArgList[0] );
                          Color := StringToColor( Arglist[1] );
                          Name := ArgList[2];
                          Size := strtoint( ArgList[3] );
                          Style := StrToFontStyle( ArgList[4] );
                        end;
                      end;
                      ecLanguage : begin
                        CommandRecall.Language := strtoint( argstr );
                      end;
                      ecGoTo : begin
                        CommandRecall.GoToIdx := argstr;
                      end;
                      ecHighlightDlg : begin
                        CommandRecall.Color := StringToColor( argstr );
                      end;
                      ecParaDlg : begin
                        CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                        with CommandRecall.Para do
                        begin
                          SpacingRule := TLineSpacingRule( strtoint( ArgList[0] ));
                          LIndent := strtoint( ArgList[1] );
                          RIndent := strtoint( ArgList[2] );
                          FIndent := strtoint( ArgList[3] );
                          SpaceBefore := strtoint( ArgList[4] );
                          SpaceAfter := strtoint( ArgList[5] );
                          Numbering := TRxNumbering( strtoint( ArgList[6] ));
                          Alignment := TParaAlignment( strtoint( ArgList[7] ));
                        end;
                      end;
                    end;
                    LastEditCmd := EditCmd;
                    RepeatLastCommand;
                  except
                    on E : Exception do
                    begin
                      if Macro.AbortOnError then
                      begin
                        AbortMacro( E.Message, i, Macro.Lines[pred( i )] );
                        break;
                      end
                      else
                      begin
                        inc( ErrorCnt );
                        continue;
                      end;
                    end;
                  end;
                end;
              end
              else
              begin
                // special cases
                try
                  // we must set control state here, because these are
                  // not "remembered commands" and PerformCmd will
                  // take values from interface controls
                  // (all except ecInsCharacter)
                  case EditCmd of
                    ecFontName : begin
                      CommandRecall.Font.Name := argstr;
                      Form_Main.Combo_Font.FontName := CommandRecall.Font.Name;
                    end;
                    ecFontSize : begin
                      CommandRecall.Font.Size := strtoint( argstr );
                      Form_Main.Combo_FontSize.Text := argstr;
                    end;
                    ecFontColor, ecFontColorBtn : begin
                      CommandRecall.Font.Color := StringToColor( argstr );
                      Form_Main.TB_Color.ActiveColor := CommandRecall.Font.Color;
                    end;
                    ecHighlight, ecHighlightBtn : begin
                      CommandRecall.Color := StringToColor( argstr );
                      Form_Main.TB_Hilite.ActiveColor := CommandRecall.Color;
                    end;
                    ecInsCharacter : begin
                      CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                      with CommandRecall.CharInfo do
                      begin
                        Code := strtoint( ArgList[0] );
                        Count := strtoint( ArgList[1] );
                        Name := ArgList[2];
                        Charset := strtoint( ArgList[3] );
                      end;
                    end;
                    ecFindText : begin
                      CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                      with FindOptions do
                      begin
                        Pattern := ArgList[0];
                        Text_to_Find := Pattern;
                        MatchCase := ( ArgList[1] = BOOLEANSTR[true] );
                        WholeWordsOnly := ( ArgList[2] = BOOLEANSTR[true] );
                        AllNodes := false;
                        AllTabs := false;
                        EntireScope := false;
                        Wrap := false;
                      end;
                    end;
                  end;
                  LastEditCmd := EditCmd;
                  RepeatLastCommand;
                except
                  on E : Exception do
                  begin
                    if Macro.AbortOnError then
                    begin
                      AbortMacro( E.Message, i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else
                    begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;
                end;
              end;
            end;

            _MACRO_USERCMD_CHAR : begin
              inc( cmdcnt );
              // execute user command
              cmdstr := '';
              argstr := '';
              delete( line, 1, 1 );
              p := pos( '(', line );
              if ( p = 0 ) then
              begin
                cmdstr := uppercase( line );
              end
              else
              begin
                cmdstr := uppercase( copy( line, 1, pred( p )));
                delete( line, 1, p );
                p := pos( ')', line );
                if ( p = 0 ) then
                begin
                  if Macro.AbortOnError then
                  begin
                    AbortMacro( 'syntax error', i, Macro.Lines[pred( i )] );
                    break;
                  end
                  else
                  begin
                    inc( ErrorCnt );
                    continue;
                  end;
                end
                else
                begin
                  argstr := copy( line, 1, pred( p ));
                end;
              end;

              cmdidx := MacroCmds.IndexOf( cmdstr );
              if ( cmdidx < 0 ) then
              begin
                if Macro.AbortOnError then
                begin
                  AbortMacro( 'unknown user command', i, Macro.Lines[pred( i )] );
                  break;
                end
                else
                begin
                  inc( ErrorCnt );
                  continue;
                end;
              end;

              macrocmd := TMacroCmd( cmdidx );

              case MACRO_DEFS[macrocmd].ArgType of
                argString : begin
                  // check argument; use default if not specified
                  if ( argstr = '' ) then
                    argstr := MACRO_DEFS[macrocmd].DefArg;
                  // still no argument?
                  if ( argstr = '' ) then
                  begin
                    if Macro.AbortOnError then
                    begin
                      AbortMacro( 'string argument required', i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else
                    begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;
                end;
                argInteger : begin
                  if ( argstr = '' ) then
                    argstr := MACRO_DEFS[macrocmd].DefArg;
                  try
                    argint := StrToInt( trim( argstr ));
                  except
                    if Macro.AbortOnError then
                    begin
                      AbortMacro( 'integer argument required', i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else
                    begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;
                end;
              end;

              case macrocmd of
                macInsert : begin
                  with ActiveNote.Editor do
                  begin
                    SelText := ExpandMetaChars( argstr );
                    SelStart := SelStart + SelLength;
                    SelLength := 0;
                  end;
                end;
                macWait : begin
                  Sleep( argint );
                  Application.ProcessMessages;
                end;
                macRewind : begin
                  MacroFinished := false;
                  break;
                end;
                macMessage : begin
                  Application.MessageBox(
                    PChar( ExpandMetaChars( argstr )),
                    PChar( Macro.Name ),
                    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL );
                end;
                macStatus : begin
                  Form_Main.StatusBar.Panels[PANEL_HINT].Text := argstr;
                end;
                macPlugin : begin
                end;
                macFontColor : begin
                  Form_Main.NoteSelText.Color := StringToColor( argstr );
                end;
                macBGColor : begin
                  // call PerformCmd here, because setting BG color
                  // for a note involves more work than just
                  // setting the RichEdit.Color property
                  CommandRecall.Color := StringToColor( argstr );
                  LastEditCmd := ecBGColorDlg;
                  RepeatLastCommand;
                end;
                macHighlightColor : begin
                  Form_Main.NoteSelText.BackColor := StringToColor( argstr );
                end;
                macMacro : begin
                  newMacro := TMacro.Create;
                  try
                    if ( extractfileext( lowercase( argstr )) <> ext_Macro ) then
                      argstr := argstr + ext_Macro;
                    newMacro.FileName := argstr;
                    if ( not NewMacro.Load ) then
                    begin
                      if Macro.AbortOnError then
                      begin
                        AbortMacro( Format(
                          'Cannot run embedded macro "%s". Reason: %s',
                          [argstr, Macro.LastError]
                        ), i, Macro.Lines[pred( i )] );
                        break;
                      end
                      else
                      begin
                        inc( ErrorCnt );
                        continue;
                      end;
                    end
                    else
                    begin
                      PlayMacro( newMacro ); // RECURSIVE CALL
                    end;
                  finally
                    newMacro.Free;
                  end;
                end;
                macConfirm : begin
                  if ( Application.MessageBox(
                    PChar( argstr ),
                    PChar( Macro.Name ),
                    MB_OKCANCEL+MB_ICONQUESTION+MB_DEFBUTTON1+MB_APPLMODAL ) = ID_CANCEL ) then
                  begin
                    // user clicked CANCEL, so abort macro
                    MacroFinished := true;
                    break;
                  end;
                end;
                macNoteNewRTF : begin
                  // always abort if fail
                  if ( not NewNote( true, true, ntRTF )) then
                  begin
                    AbortMacro( 'Note creation failed', i, Macro.Lines[pred( i )] );
                    break;
                  end;
                end;
                macNoteNewTree : begin
                  // always abort if fail
                  if ( not NewNote( true, true, ntTree )) then
                  begin
                    AbortMacro( 'Note creation failed', i, Macro.Lines[pred( i )] );
                    break;
                  end;
                end;
                macApplyStyle : begin
                  StyleApply( argstr );
                end;
                macBookmarkSet : begin
                  if argint in [0..9] then
                    BookmarkAdd( argint );
                end;
                macBookmarkJump : begin
                  if argint in [0..9] then
                    BookmarkGoTo( argint );
                end;
                macStyleOn, macStyleOff, macStyleFlip : begin
                  argstr := lowercase( argstr );
                  if argstr = 'bold' then
                    myFontStyle := fsBold
                  else
                  if argstr = 'italic' then
                    myFontStyle := fsItalic
                  else
                  if argstr = 'underline' then
                    myFontStyle := fsUnderline
                  else
                  if argstr = 'strike' then
                    myFontStyle := fsStrikeout
                  else
                  begin
                    if Macro.AbortOnError then
                    begin
                      AbortMacro(
                       'Invalid font style argument',
                        i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else
                    begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;

                  if ( macrocmd = macStyleOn ) then
                  begin
                    with Form_Main.NoteSelText do
                      Style := Style + [myFontStyle];
                  end
                  else
                  if ( macrocmd = macStyleOff ) then
                  begin
                    with Form_Main.NoteSelText do
                      Style := Style - [myFontStyle];
                  end
                  else // macStyleFlip
                  begin
                    with Form_Main.NoteSelText do
                      if myFontStyle in Style then
                        Style := Style - [myFontStyle]
                      else
                        Style := Style + [myFontStyle];
                  end;
                end;

                macGoUp : begin
                  for counter := 1 to argint do
                  with ActiveNote.Editor do
                  begin
                    Perform( WM_KEYDOWN, VK_UP, 0 );
                    Perform( WM_KEYUP, VK_UP, 0 );
                  end;
                end;

                macGoDown : begin
                  for counter := 1 to argint do
                  with ActiveNote.Editor do
                  begin
                    Perform( WM_KEYDOWN, VK_DOWN, 0 );
                    Perform( WM_KEYUP, VK_DOWN, 0 );
                  end;
                end;

                macGoRight : begin
                  for counter := 1 to argint do
                  with ActiveNote.Editor do
                  begin
                    Perform( WM_KEYDOWN, VK_RIGHT, 0 );
                    Perform( WM_KEYUP, VK_RIGHT, 0 );
                  end;
                end;

                macGoLeft : begin
                  for counter := 1 to argint do
                  with ActiveNote.Editor do
                  begin
                    Perform( WM_KEYDOWN, VK_LEFT, 0 );
                    Perform( WM_KEYUP, VK_LEFT, 0 );
                  end;
                end;

                macSelectAll : begin
                  ActiveNote.Editor.SelectAll;
                end;

              end; // case cmd

            end;

            else // bare keypress information
            begin
              while ( not ActiveNote.Editor.Focused ) do
              begin
                // RTF *must* be focused, otherwise keypresses
                // will be sent to the wrong control.
                // while RTF is not focused, we just pause
                Application.ProcessMessages;
                if MacroAbortRequest then
                begin
                  MacroFinished := true;
                  break;
                end;
              end;

              sndKey := 0;
              sndShift := [];

              p := pos( _MACRO_DELIMITER_CHAR, line );
              if ( p = 0 ) then continue;
              try
                sndKey := strtoint( copy( line, 1, pred( p )));
              except
                sndKey := 0;
              end;
              if ( sndKey = 0 ) then continue;
              delete( line, 1, p );
              sndShift := StrToShiftState( line );

              PostKeyEx32( sndKey, sndShift, False )

            end;

          end;
        end; // lines.count

      until MacroFinished;

    except
      On E : Exception do
      begin
        MacroFinished := true;
        MacroErrorAbort := true;
        messagedlg( Format(
          'Unexpected error while executing macro: %s' + #13#13 +
          'Last macro line was: "%s" (line %d)',
          [E.Message,copy( Macro.Lines[pred(linecnt)], 1, 127 ),linecnt]
        ), mtError, [mbOK], 0 );
      end;
    end;
  finally
    MacroCmds.Free;
    EditCmds.Free;
    ArgList.Free;
  end;
end; // PlayMacro

function MacroProcess( const DoWarn : boolean ) : boolean;
begin
  result := ( IsRunningMacro or IsRecordingMacro );
  if ( result and DoWarn ) then
    messagedlg( 'This command cannot be executed while macro is being recorded or replayed.',
      mtInformation, [mbOK], 0 );
end; // MacroProcess

procedure AddUserMacroCommand;
var
  Form_MacroCmd : TForm_MacroCmd;
begin
  if ( not ( assigned( ActiveMacro ) and IsRecordingMacro )) then exit;

  Form_MacroCmd := TForm_MacroCmd.Create( Form_Main );
  try
    if ( Form_MacroCmd.ShowModal = mrOK ) then
    begin
      with Form_MacroCmd do
      begin
        if ( myArgs <> '' ) then
          ActiveMacro.Lines.Add( Format(
            '%s%s(%s)',
            [_MACRO_USERCMD_CHAR,MACRO_CMD_NAMES[myCmd],myArgs]
          ))
        else
          ActiveMacro.Lines.Add( Format(
            '%s%s',
            [_MACRO_USERCMD_CHAR,MACRO_CMD_NAMES[myCmd]] ));
        (*
        if CB_Execute.Checked then
        begin
          // execute this command
        end;
        *)
      end;
    end;
  finally
    Form_MacroCmd.Free;
  end;

end; // AddUserMacroCommand

function GetMacroByName( const aName : string; const DoWarn : boolean ) : TMacro;
var
  i : integer;
begin
  result := nil;
  i := Macro_List.IndexOf( aName );

  if ( i >= 0 ) then
  begin
    result := TMacro( Macro_List.Objects[i] );
  end
  else
  begin
    if DoWarn then
      messagedlg( Format( 'Macro "%s" not found.', [aName] ), mtError, [mbOK], 0 );
  end;

end; // GetMacroByName

function GetCurrentMacro( const DoWarn : boolean ) : TMacro;
begin
  result := nil;

  if ( not CheckResourcePanelVisible( true )) then exit;

  if (( Form_Main.ListBox_ResMacro.Items.Count = 0 ) or ( Form_Main.ListBox_ResMacro.ItemIndex < 0 )) then
  begin
    if DoWarn then
      messagedlg( 'No macros available or none selected.', mtError, [mbOK], 0 );
    exit;
  end;

  try

    try
      result := TMacro(
        Macro_List.Objects[Macro_List.IndexOf( Form_Main.ListBox_ResMacro.Items[Form_Main.ListBox_ResMacro.ItemIndex] )]);
    except
      result := nil;
    end;

  finally
    if (( result = nil ) and DoWarn ) then
      messagedlg( 'Could not access current macro.', mtError, [mbOK], 0 );
  end;
end; // GetCurrentMacro

procedure RepeatLastCommand;
begin
  if ( not assigned( ActiveNote )) then exit;
  if ( LastEditCmd = ecNone ) then exit;
  if ( LastEditCmd in RepeatableEditCommands ) then
  begin
    if ( LastEditCmd in EditCommandsEx ) then
      PerformCmdEx( LastEditCmd )
    else
      PerformCmd( LastEditCmd );
  end
  else
  if ( LastEditCmd in RememberedEditCommands ) then
  begin
    RecallingCommand := true;
    try
      if ( LastEditCmd in EditCommandsEx ) then
        PerformCmdEx( LastEditCmd )
      else
        PerformCmd( LastEditCmd );
    finally
      RecallingCommand := false;
    end;
  end
  else
  begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := ' This command cannot be repeated';
  end;

end; // RepeatLastCommand

procedure PerformCmdEx( aCmd : TEditCmd );
var
  s : string;
begin
  // Perform command on ActiveNote.
  // The command does not modify the note,
  // hence is safe to use when note is set
  // to ReadOnly.
  if ( RTFUpdating or FileIsBusy ) then exit;
  if ( not assigned( ActiveNote )) then
  begin
    if opt_Debug then
    begin
      {$IFDEF MJ_DEBUG}
      Log.Add( 'ActiveNote not assigned in PerformCmd (' + inttostr( ord( aCmd )) + ')' );
      {$ENDIF}
      PopupMessage( Format( 'This action cannot be performed, because there is no active note (%d)', [ord( aCmd )] ), mtError, [mbOK], 0 );
    end;
    exit;
  end;

  try
    try
      case aCMD of
        ecCopy : begin
          ActiveNote.Editor.CopyToClipboard;
        end;
        ecReadOnly : begin
          if ( ActiveNote = NoteFile.ClipCapNote ) then
          begin
            PopupMessage( 'This note cannot be set as Read-only, because it is being used for clipboard capture.', mtError, [mbOK], 0 );
            aCmd := ecNone;
          end
          else
          begin
            ActiveNote.ReadOnly := ( not ActiveNote.ReadOnly );
            UpdateNoteDisplay;
            UpdateCursorPos;
          end;
        end;
        ecFontFormatCopy : begin
          try
            FontAttrsRx2KNT( ActiveNote.Editor.SelAttributes, FontFormatToCopy );
          except
            Popupmessage( 'Failed to assign font attributes.', mtError, [mbOK], 0 );
            aCmd := ecNone;
          end;
        end;
        ecParaFormatCopy : begin
          try
            ParaAttrsRX2KNT( ActiveNote.Editor.Paragraph, ParaFormatToCopy );
          except
            Popupmessage( 'Failed to assign paragraph attributes.', mtError, [mbOK], 0 );
            aCmd := ecNone;
          end;
        end;
        ecInsOvrToggle : begin
          ActiveNote.IsInsertMode := ( not ActiveNote.IsInsertMode );
          Form_Main.ShowInsMode;
        end;
        ecMatchBracket : begin
          MatchBracket;
        end;
        ecSelectWord : begin
          ActiveNote.Editor.GetWordAtCursorNew( true );
        end;
        ecGoTo : begin
          s := CommandRecall.GoToIdx;
          if ( not RecallingCommand ) then
          begin
            if ( not InputQuery( 'Go to line', 'Enter line number or increment (+-):', s )) then
            begin
              s := '';
            end;
          end;
          if ( s <> '' ) then
          begin
            try
              Form_Main.GoToEditorLine( s );
              CommandRecall.GoToIdx := s;
            except
              on E : Exception do
              begin
                aCmd := ecNone;
                messagedlg( E.Message, mtError, [mbOK], 0 );
              end;
            end;
          end
          else
          begin
            // no line number specified
            aCmd := ecNone;
          end;
        end;
        else
        begin
          if RecallingCommand then
          begin
            case aCmd of
              ecFindText : begin
                RunFindNext;
              end;
            end;
          end
          else
          begin
            Form_Main.NotImplemented( 'Unexpected or not implemented command: ' + EDITCMD_NAMES[aCMD] );
            aCmd := ecNone;
          end;
        end;
      end;

      UpdateLastCommand( aCMD );
      if IsRecordingMacro then
        AddMacroEditCommand( aCMD );


    except
      on E : Exception do
      begin
        PopupMessage( 'Cannot perform command:' + #13 + E.Message, mtError, [mbOK], 0 );
        {$IFDEF MJ_DEBUG}
        Log.Add( 'Exception in PerformCmdEx (' + inttostr( ord( aCMD )) + '): ' + E.Message );
        {$ENDIF}
      end;
    end;
  finally
    Form_Main.RxRTFChange( ActiveNote.Editor );
  end;

end; // PerformCmdEx

procedure PerformCmd( aCmd : TEditCmd );
var
  txt, txt2 : string;
  p, i, lineindex : integer;
  templist : TStringList;
  tempChrome : TChrome;
  ShiftWasDown : boolean;
  myTreeNode : TTreeNTNode;
  myPara : TParaFormat2;

    Procedure CmdNumbering(tipo : TRxNumbering);
    var
      actualNumbering : TRxNumbering;
      clearIndent: integer;
    begin
        actualNumbering:= ActiveNote.Editor.Paragraph.Numbering;
        If actualNumbering = tipo Then begin
            ActiveNote.Editor.Paragraph.Numbering := nsNone;
            If actualNumbering = nsBullet Then
                ActiveNote.Editor.Paragraph.FirstIndentRelative:= -2
            Else
                ActiveNote.Editor.Paragraph.FirstIndentRelative:= -4;
        end
        Else begin
            clearIndent := 0;
            If actualNumbering = nsBullet Then  // if convert from bullet to numbering
                clearIndent := -2
            Else If actualNumbering <> nsNone Then
                clearIndent := -4;

            If tipo = nsBullet Then
                ActiveNote.Editor.Paragraph.FirstIndentRelative := 2 + clearIndent
            Else
                ActiveNote.Editor.Paragraph.FirstIndentRelative := 4 + clearIndent;

            ActiveNote.Editor.Paragraph.Numbering := tipo;
            ActiveNote.Editor.Paragraph.NumberingStyle := nsEnclosed;
            ActiveNote.Editor.Paragraph.NumberingStart := 1;
        End
    End;

begin
  // Perform command on ActiveNote.
  // The command MODIFIES the note,
  // hence cannot be executed when note is set
  // to ReadOnly.
  if ( RTFUpdating or FileIsBusy ) then exit;
  if ( not assigned( ActiveNote )) then exit;

  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

    try
      try
        case aCMD of
          ecBold : begin
            with Form_Main.NoteSelText do
            begin
              if fsBold in Style then
                Style := Style - [fsBold]
              else
                Style := Style + [fsBold];
            end;
          end;
          ecUnderline : with Form_Main.NoteSelText do
          begin
            if fsUnderline in Style then
              Style := Style - [fsUnderline]
            else
              Style := Style + [fsUnderline];
          end;
          ecItalics : with Form_Main.NoteSelText do
          begin
            if fsItalic in Style then
              Style := Style - [fsItalic]
            else
              Style := Style + [fsItalic];
          end;
          ecStrikeOut : with Form_Main.NoteSelText do
          begin
            if fsStrikeOut in Style then
              Style := Style - [fsStrikeOut]
            else
              Style := Style + [fsStrikeOut];
          end;
          ecCut : begin
            ActiveNote.Editor.CutToClipboard;
          end;
          ecPaste : begin
            //ActiveNote.Editor.PasteFromClipboard;             //dpv
            if not ActiveNote.Editor.PasteSpecial_RTF then
               ActiveNote.Editor.PasteSpecial_NoAsk;
          end;
          ecPastePlain : begin
            if ( Clipboard.HasFormat( CF_TEXT )) then
            begin
              ActiveNote.Editor.SelText := trim( Clipboard.AsText ); // [paste]
              ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
            end;
          end;
          ecDelete : begin
            // ActiveNote.Editor.Perform( WM_KEYDOWN, VK_DELETE, 0 );
            // ActiveNote.Editor.Perform( WM_KEYUP, VK_DELETE, 0 );
            ActiveNote.Editor.Perform( WM_CLEAR, 0, 0 );
          end;
          ecBullets : begin
            CmdNumbering(nsBullet);
            (* riched20.dll v. 3.0 supports numbering. E.g.:
            if ( ActiveNote.Editor.Paragraph.Numbering = nsNone ) then
              ActiveNote.Editor.Paragraph.Numbering := nsBullet
            else
            if ( ActiveNote.Editor.Paragraph.Numbering = nsBullet ) then
              ActiveNote.Editor.Paragraph.Numbering := nsArabicNumbers
            else
              ActiveNote.Editor.Paragraph.Numbering := nsNone;
            *)

          end;
          ecNumbers : begin
            CmdNumbering(KeyOptions.LastNumbering);
          end;
          ecSpace1 : begin
            ActiveNote.Editor.Paragraph.LineSpacingRule := lsSingle;
            ActiveNote.Editor.Paragraph.LineSpacing := 0;
          end;
          ecSpace15 : begin
            ActiveNote.Editor.Paragraph.LineSpacingRule := lsOneAndHalf;
            ActiveNote.Editor.Paragraph.LineSpacing := 1; // EditorOptions.LineSpcInc;
          end;
          ecSpace2 : begin
            ActiveNote.Editor.Paragraph.LineSpacingRule := lsDouble;
            ActiveNote.Editor.Paragraph.LineSpacing := 2; // 2*EditorOptions.LineSpcInc;
          end;
          ecSpaceBeforeInc : begin
            ActiveNote.Editor.Paragraph.SpaceBefore := ActiveNote.Editor.Paragraph.SpaceBefore + EditorOptions.ParaSpaceInc;
          end;
          ecSpaceBeforeDec : begin
            if ( ActiveNote.Editor.Paragraph.SpaceBefore >= EditorOptions.ParaSpaceInc ) then
              ActiveNote.Editor.Paragraph.SpaceBefore := ActiveNote.Editor.Paragraph.SpaceBefore - EditorOptions.ParaSpaceInc
            else
              ActiveNote.Editor.Paragraph.SpaceBefore := 0;
          end;
          ecSpaceAfterInc : begin
            ActiveNote.Editor.Paragraph.SpaceAfter := ActiveNote.Editor.Paragraph.SpaceAfter + EditorOptions.ParaSpaceInc;
          end;
          ecSpaceAfterDec : begin
            if ( ActiveNote.Editor.Paragraph.SpaceAfter >= EditorOptions.ParaSpaceInc ) then
              ActiveNote.Editor.Paragraph.SpaceAfter := ActiveNote.Editor.Paragraph.SpaceAfter - EditorOptions.ParaSpaceInc
            else
              ActiveNote.Editor.Paragraph.SpaceAfter := 0;
          end;
          ecIndent : begin
            ActiveNote.Editor.Paragraph.LeftIndent := ActiveNote.Editor.Paragraph.LeftIndent + EditorOptions.IndentInc;
          end;
          ecOutdent : begin
            if ( ActiveNote.Editor.Paragraph.LeftIndent >= EditorOptions.IndentInc ) then
              ActiveNote.Editor.Paragraph.LeftIndent := ActiveNote.Editor.Paragraph.LeftIndent - EditorOptions.IndentInc
            else
              ActiveNote.Editor.Paragraph.LeftIndent := 0;
          end;
          ecFirstIndent : begin
               ActiveNote.Editor.Paragraph.FirstIndentRelative := EditorOptions.IndentInc;
          end;
          ecFirstOutdent : begin
            (*  [dpv]
            if ( ActiveNote.Editor.Paragraph.FirstIndent >= EditorOptions.IndentInc ) then
              ActiveNote.Editor.Paragraph.FirstIndent := ActiveNote.Editor.Paragraph.FirstIndent - EditorOptions.IndentInc
            else
              ActiveNote.Editor.Paragraph.FirstIndent := 0;
            *)
            ActiveNote.Editor.Paragraph.FirstIndentRelative := - EditorOptions.IndentInc;
          end;
          ecRightIndent : begin
            ActiveNote.Editor.Paragraph.RightIndent := ActiveNote.Editor.Paragraph.RightIndent + EditorOptions.IndentInc;
          end;
          ecRightOutdent : begin
            if ( ActiveNote.Editor.Paragraph.RightIndent >= EditorOptions.IndentInc ) then
              ActiveNote.Editor.Paragraph.RightIndent := ActiveNote.Editor.Paragraph.RightIndent - EditorOptions.IndentInc
            else
              ActiveNote.Editor.Paragraph.RightIndent := 0;
          end;
          ecFontFormatPaste : begin
            if ( FontFormatToCopy.Name <> '' ) then
              FontAttrsKNT2RX( FontFormatToCopy, ActiveNote.Editor.SelAttributes )
            else
              popupmessage( 'No font attributes to paste from: Use "Copy font attributes" first.', mtError, [mbOK], 0 );
          end;
          ecParaFormatPaste : begin
            if ( ParaFormatToCopy.SpaceBefore >= 0 ) then // if negative, user has not yet COPIED para format
            begin
              ParaAttrsKNT2RX( ParaFormatToCopy, ActiveNote.Editor.Paragraph );
              with ActiveNote.Editor.Paragraph do
            end
            else
            begin
              popupmessage( 'No paragraph attributes to paste from: Use "Copy paragraph attributes" first.', mtError, [mbOK], 0 );
            end;
          end;
          ecAlignLeft : begin
            ActiveNote.Editor.Paragraph.Alignment := paLeftJustify;
          end;
          ecAlignCenter : begin
            ActiveNote.Editor.Paragraph.Alignment := paCenter;
          end;
          ecAlignRight : begin
            ActiveNote.Editor.Paragraph.Alignment := paRightJustify;
          end;
          ecAlignJustify : begin
            ActiveNote.Editor.Paragraph.Alignment := paJustify;
          end;
          ecFontDlg : begin
            if RecallingCommand then
            begin
              FontInfoToFont( CommandRecall.Font, Form_Main.FontDlg.Font );
              Form_Main.NoteSelText.Assign( Form_Main.FontDlg.Font );
            end
            else
            begin
              Form_Main.FontDlg.Font.Assign( Form_Main.NoteSelText );
              if Form_Main.FontDlg.Execute then
              begin
                Form_Main.NoteSelText.Assign( Form_Main.FontDlg.Font );
                FontToFontInfo( Form_Main.FontDlg.Font, CommandRecall.Font );
              end
              else
              begin
                // dialog was canceled, restore previous (possibly repeatable) command
                aCMD := ecNone;
              end;
            end;
          end;
          ecLanguage : begin
            if ( RecallingCommand or RunLanguageDialog ) then
            begin
              ActiveNote.Editor.SelAttributes.Language := CommandRecall.Language;
            end
            else
            begin
              // dialog was canceled, will restore previous (possibly repeatable) command
              aCMD := ecNone;
            end;
          end;
          ecParaDlg : begin
            if ( RecallingCommand or RunParagraphDialog ) then
            begin
              with CommandRecall.Para do
              begin
                ActiveNote.Editor.Paragraph.LineSpacingRule := SpacingRule;
                case SpacingRule of
                  lsSingle : ActiveNote.Editor.Paragraph.LineSpacing := 0;
                  lsOneAndHalf : ActiveNote.Editor.Paragraph.LineSpacing := 1;
                  lsDouble : ActiveNote.Editor.Paragraph.LineSpacing := 2;
                end;
                ActiveNote.Editor.Paragraph.SpaceBefore := SpaceBefore;
                ActiveNote.Editor.Paragraph.SpaceAfter := SpaceAfter;
                ActiveNote.Editor.Paragraph.Numbering := Numbering;
                (*
                if ( Numbering = nsArabicNumbers ) then
                begin
                  ActiveNote.Editor.Paragraph.Numbering := KeyOptions.LastNumbering;
                end
                else
                begin
                  ActiveNote.Editor.Paragraph.Numbering := Numbering;
                end;
                *)
                ActiveNote.Editor.Paragraph.Alignment := Alignment;
                ActiveNote.Editor.Paragraph.FirstIndent := CommandRecall.Para.FIndent;
                ActiveNote.Editor.Paragraph.LeftIndent := CommandRecall.Para.LIndent;
                ActiveNote.Editor.Paragraph.RightIndent := CommandRecall.Para.RIndent;
              end;
            end
            else
            begin
              // dialog was canceled, restore previous (possibly repeatable) command
              aCMD := ecNone;
            end;
          end;
          ecFontName : begin
            if RecallingCommand then
            begin
              Form_Main.NoteSelText.Name := CommandRecall.Font.Name;
            end
            else
            begin
              Form_Main.NoteSelText.Name := Form_Main.Combo_Font.FontName;
              CommandRecall.Font.Name := Form_Main.Combo_Font.FontName;
            end;
          end;
          ecFontSize : begin
            if RecallingCommand then
            begin
              Form_Main.NoteSelText.Size := CommandRecall.Font.Size;
            end
            else
            begin
              try
                Form_Main.NoteSelText.Size := strtoint( Form_Main.Combo_FontSize.Text );
                CommandRecall.Font.Size := Form_Main.NoteSelText.Size;
              except
                messagedlg( Format( '"%s" is not a valid number', [Form_Main.Combo_FontSize.Text] ), mtError, [mbOK], 0 );
                aCmd := ecNone;
              end;
            end;
          end;
          ecFontSizeInc : begin
            Form_Main.NoteSelText.Size := Form_Main.NoteSelText.Size + EditorOptions.FontSizeInc;
          end;
          ecFontSizeDec : begin
            Form_Main.NoteSelText.Size := Form_Main.NoteSelText.Size - EditorOptions.FontSizeInc;
          end;
          ecDisabled : with Form_Main.NoteSelText do
          begin
            Disabled := ( not Disabled );
          end;
          ecSubscript : with Form_Main.NoteSelText do
          begin
            if ( SubscriptStyle <> ssSubscript ) then
              SubscriptStyle := ssSubscript
            else
              SubscriptStyle := ssNone;
          end;
          ecSuperscript : with Form_Main.NoteSelText do
          begin
            if ( SubscriptStyle <> ssSuperscript ) then
              SubscriptStyle := ssSuperscript
            else
              SubscriptStyle := ssNone;
          end;
          ecFontColorBtn : begin
            if RecallingCommand then
              Form_Main.TB_Color.ActiveColor := CommandRecall.Font.Color;
            Form_Main.NoteSelText.Color := Form_Main.TB_Color.ActiveColor;
            CommandRecall.Font.Color := Form_Main.TB_Color.ActiveColor;
            FocusActiveNote; // DFSColorBtn steals focus
            KeyOptions.InitFontColor := CommandRecall.Font.Color;
          end;
          ecHighlightBtn : begin
            if RecallingCommand then
              Form_Main.TB_Hilite.ActiveColor := CommandRecall.Color;
            Form_Main.NoteSelText.BackColor := Form_Main.TB_Hilite.ActiveColor;
            CommandRecall.Color := Form_Main.TB_Hilite.ActiveColor;
            FocusActiveNote; // DFSColorBtn steals focus
            KeyOptions.InitHiColor := CommandRecall.Color;
          end;
          ecFontColorDlg : begin
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Font.Color
            else
              Form_Main.ColorDlg.Color := Form_Main.NoteSelText.Color;
            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then
            begin
              Form_Main.NoteSelText.Color := Form_Main.ColorDlg.Color;
              CommandRecall.Font.Color := Form_Main.ColorDlg.Color;
            end
            else
            begin
              // dialog was canceled
              aCMD := ecNone;
            end;
          end;
          ecHighlightDlg : begin
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Color
            else
              Form_Main.ColorDlg.Color := Form_Main.NoteSelText.BackColor;
            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then
            begin
              Form_Main.NoteSelText.BackColor := Form_Main.ColorDlg.Color;
              CommandRecall.Color := Form_Main.ColorDlg.Color;
            end
            else
            begin
              // dialog was canceled
              aCMD := ecNone;
            end;
          end;
          ecFontColor : begin
            Form_Main.NoteSelText.Color := Form_Main.TB_Color.ActiveColor;
            CommandRecall.Font.Color := Form_Main.TB_Color.ActiveColor;
          end;
          ecHighlight : begin
            Form_Main.NoteSelText.BackColor := Form_Main.TB_Hilite.ActiveColor;
            CommandRecall.Color := Form_Main.TB_Hilite.ActiveColor;
          end;
          ecNoHighlight : begin
            Form_Main.NoteSelText.BackColor := clWindow;
          end;
          ecBGColorDlg : begin
            ShiftWasDown := ShiftDown;
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Color
            else
              Form_Main.ColorDlg.Color := ActiveNote.Editor.Color;
            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then
            begin
              // ActiveNote.Editor.Color := ColorDlg.Color; [x] note updates itself
              // AChrome := ActiveNote.Chrome;
              // AChrome.BGColor := ColorDlg.Color;
              tempChrome := ActiveNote.EditorChrome;
              tempChrome.BGColor := Form_Main.ColorDlg.Color;
              CommandRecall.Color := Form_Main.ColorDlg.Color;

              ActiveNote.Editor.Color := tempChrome.BGColor;
              case ActiveNote.Kind of
                ntRTF : begin
                  ActiveNote.EditorChrome := tempChrome;
                end;
                ntTree : begin
                  if assigned( TTreeNote( ActiveNote ).SelectedNode ) then
                    TTreeNote( ActiveNote ).SelectedNode.RTFBGColor := tempChrome.BGColor;

                  if ( not TreeOptions.InheritNodeBG ) then
                    ActiveNote.EditorChrome := tempChrome;

                  if ShiftWasDown then
                  begin
                    if ( messagedlg( 'New background color will be assigned to ALL TREE NODES in note ' + ActiveNote.Name + #13 + 'Continue?',
                        mtConfirmation, [mbOK,mbCancel], 0 ) = mrOK ) then
                    begin
                      try
                        myTreeNode := TTreeNote( ActiveNote ).TV.Items.GetFirstNode;
                        while assigned( myTreeNode ) do
                        begin
                          TNoteNode( myTreeNode.Data ).RTFBGColor := tempChrome.BGColor;
                          myTreeNode := myTreeNode.GetNext;
                        end;
                      except
                         aCmd := ecNone;
                      end;
                    end;
                  end;
                end;
              end;
            end
            else
            begin
              // dialog was canceled, restore previous (possibly repeatable) command
              aCMD := ecNone;
            end;
          end;
          ecWordWrap : begin
            case ActiveNote.Kind of
              ntRTF : begin
                ActiveNote.WordWrap := ( not ActiveNote.WordWrap );
              end;
              ntTree : begin
                if assigned( TTreeNote( ActiveNote ).SelectedNode ) then
                begin
                  case TTreeNote( ActiveNote ).SelectedNode.WordWrap of
                    wwAsNote : begin
                      if ActiveNote.WordWrap then
                        TTreeNote( ActiveNote ).SelectedNode.WordWrap := wwNo
                      else
                        TTreeNote( ActiveNote ).SelectedNode.WordWrap := wwYes;
                    end;
                    wwYes : TTreeNote( ActiveNote ).SelectedNode.WordWrap := wwNo;
                    wwNo : TTreeNote( ActiveNote ).SelectedNode.WordWrap := wwYes;
                  end;
                  ActiveNote.Editor.WordWrap := ( TTreeNote( ActiveNote ).SelectedNode.WordWrap = wwYes );
                end;
              end;
            end;
            UpdateNoteDisplay;
          end;
          ecSelectAll : begin
            ActiveNote.Editor.SelectAll;
          end;
          ecUndo : begin
            ActiveNote.Editor.Undo;
          end;
          ecRedo : begin
            ActiveNote.Editor.Redo;
          end;
          ecClearFontAttr : begin
            with Form_Main.NoteSelText do
            begin
              Style := [];
              Color := ActiveNote.EditorChrome.Font.Color;
              Name := ActiveNote.EditorChrome.Font.Name;
              Size := ActiveNote.EditorChrome.Font.Size;
              Charset := ActiveNote.EditorChrome.Font.Charset;
              Hidden := false;
              Disabled := false;
              SubscriptStyle := ssNone;
              BackColor := clWindow; // ActiveNote.Editor.Color;
            end;
          end;
          ecClearParaAttr : begin
            with ActiveNote.Editor.Paragraph do
            begin
              Alignment := paLeftJustify;
              Numbering := nsNone;
              LeftIndent := 0;
              FirstIndent := 0;
              RightIndent := 0;
              SpaceBefore := 0;
              SpaceAfter := 0;
              LineSpacingRule := lsSingle;
              LineSpacing := 0;
            end;
          end;
          ecDeleteLine : begin
            ActiveNote.Editor.Lines.BeginUpdate;
            try
              with ActiveNote.Editor do
              begin
                lineindex := perform( EM_EXLINEFROMCHAR, 0, SelStart );
                SelStart  := perform( EM_LINEINDEX, lineindex, 0 );
                SelLength := perform( EM_LINEINDEX, lineindex + 1, 0 ) - SelStart;
                SelText := '';
              end;
            finally
              ActiveNote.Editor.Lines.EndUpdate;
            end;
          end;
          ecSort : begin
            if ( ActiveNote.Editor.SelLength > 1 ) then
            begin
              templist := TStringList.Create;
              try
                templist.Sorted := true;
                templist.Duplicates := dupAccept;
                templist.Text := ActiveNote.Editor.SelText;
                ActiveNote.Editor.SelText := templist.Text;
              finally
                templist.Free;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecReformat : begin
            Form_Main.NotImplemented( '' );
            aCmd := ecNone;
          end;
          ecJoinLines : begin
            if ( ActiveNote.Editor.SelLength > 1 ) then
            begin
              screen.cursor := crHourGlass;
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                CharToChar( txt, #13, #32 );
                p := pos( #32#32, txt );
                while ( p > 0 ) do
                begin
                  delete( txt, p, 1 );
                  p := pos( #32#32, txt );
                end;
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
                screen.cursor := crDefault;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecReverseText : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              txt := ActiveNote.Editor.SelText;
              p:= length( txt );
              txt2 := '';
              SetLength( txt2, p );
              for i := p downto 1 do
                txt2[(p-i)+1] := txt[i];
              ActiveNote.Editor.SelText := txt2;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end
          end;
          ecROT13 : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              screen.cursor := crHourGlass;
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                for i := 1 to length( txt ) do
                begin
                  p := pos( txt[i], alph13 );
                  if ( p > 0 ) then
                  begin
                    txt[i] := alph13[p+13];
                  end
                  else
                  begin
                    p := pos( txt[i], alph13UP );
                    if ( p > 0 ) then
                    begin
                      txt[i] := alph13UP[p+13];
                    end;
                  end;
                end;
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
                screen.cursor := crDefault;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecInvertCase : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                for i := 1 to length( txt ) do
                begin
                  if IsCharUpperA( txt[i] ) then
                    txt2 := ansilowercase( txt[i] )
                  else
                    txt2 := ansiuppercase( txt[i] );
                  txt[i] := txt2[1];
                end;
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecToUpperCase : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                txt := ansiuppercase( txt );
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecToLowerCase : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                txt := ansilowercase( txt );
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecCycleCase : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;

                {
                if ( LastEditCmd <> ecCycleCase ) then
                begin
                  // check case of selected text, and start smartly :)
                  LAST_CASE_CYCLE := GetLetterCase( txt );
                end;
                }

                LAST_CASE_CYCLE := GetLetterCase( txt );
                if ( LAST_CASE_CYCLE = high( LAST_CASE_CYCLE )) then
                  LAST_CASE_CYCLE := low( LAST_CASE_CYCLE )
                else
                  inc( LAST_CASE_CYCLE );

                case LAST_CASE_CYCLE of
                  ccLower : txt := ansilowercase( txt );
                  ccMixed : txt := AnsiProperCase( txt, [#8..#32,',','-','.','?','!',';'] );
                  ccUpper : txt := ansiuppercase( txt );
                end;

                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecToMixedCase : begin
            if ( ActiveNote.Editor.SelLength > 0 ) then
            begin
              ActiveNote.Editor.Lines.BeginUpdate;
              try
                txt := ActiveNote.Editor.SelText;
                txt := AnsiProperCase( txt, [#8..#32,',','-','.','?','!',';'] );
                ActiveNote.Editor.SelText := txt;
              finally
                ActiveNote.Editor.Lines.EndUpdate;
              end;
            end
            else
            begin
              Form_Main.ErrNoTextSelected;
              aCmd := ecNone;
            end;
          end;
          ecInsDate : begin
            if KeyOptions.DTUseLastSelection then
            begin
              if ( KeyOptions.DTLastDateFmt = '' ) then
                KeyOptions.DTLastDateFmt := KeyOptions.DateFmt;
              if ( KeyOptions.DTLastDateFmt = '' ) then
                KeyOptions.DTLastDateFmt := ShortDateFormat;
              ActiveNote.Editor.SelText := GetDateTimeFormatted( KeyOptions.DTLastDateFmt, now ) + #32;
            end
            else
            begin
              ActiveNote.Editor.SelText := FormatDateTime( KeyOptions.DateFmt, now ) + #32;
            end;
            ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
          end;
          ecInsTime : begin
            if KeyOptions.DTUseLastSelection then
            begin
              if ( KeyOptions.DTLastTimeFmt = '' ) then
                KeyOptions.DTLastTimeFmt := KeyOptions.TimeFmt;
              if ( KeyOptions.DTLastTimeFmt = '' ) then
                KeyOptions.DTLastTimeFmt := LongTimeFormat;
              ActiveNote.Editor.SelText := GetDateTimeFormatted( KeyOptions.DTLastTimeFmt, now ) + #32;
            end
            else
            begin
              ActiveNote.Editor.SelText := FormatDateTime( KeyOptions.TimeFmt, now ) + #32;
            end;
            ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
          end;
          ecExpandTerm : begin
            ExpandTermProc;
          end;
          ecEvaluateExpression : begin
            EvaluateExpression;
          end;
          else
          begin
            // other commands, which can be handled here
            // ONLY when RecallingCommand (e.g. from macros)
            if RecallingCommand then
            begin
              case aCmd of
                ecInsCharacter : with CommandRecall.CharInfo do
                begin
                  Form_Main.CharInsertProc( chr( code ), Count, Name, Charset );
                end;
                ecStyleApply : begin
                  StyleApply( CommandRecall.StyleName );
                end;
                else
                begin
                  // we cannot handle this comand here
                  Form_Main.NotImplemented( 'Unexpected or not implemented command: ' + EDITCMD_NAMES[aCMD] );
                  aCmd := ecNone;
                end;
              end;
            end;
          end;
        end;

        UpdateLastCommand( aCMD );
        if IsRecordingMacro then
          AddMacroEditCommand( aCMD );

      except
        on E : Exception do
        begin
          PopupMessage( 'Cannot perform command:' + #13 + E.Message, mtError, [mbOK], 0 );
          {$IFDEF MJ_DEBUG}
          Log.Add( 'Exception in PerformCmd (' + inttostr( ord( aCMD )) + '): ' + E.Message );
          {$ENDIF}
        end;
      end;
    finally
      if ( aCmd <> ecNone ) then
      begin
        ActiveNote.Modified := true;
        NoteFile.Modified := true;
      end;
      Form_Main.RxRTFSelectionChange( ActiveNote.Editor );
      UpdateNoteFileState( [fscModified] );
    end;

end; // PerformCmd

procedure UpdateLastCommand( const aCMD : TEditCmd );
begin
  if (( aCmd in RepeatableEditCommands ) or ( aCmd in RememberedEditCommands )) then
    LastEditCmd := aCmd;
  {
  else
    LastEditCmd := ecNone;
    // commented out, so that we do not erase
    // last command that CAN be repeated
  }

  with Form_Main do
  begin
      MMEditRepeat.Enabled := ( LastEditCmd <> ecNone );
      RTFMRepeatCmd.Enabled := MMEditRepeat.Enabled;
      TB_Repeat.Enabled := MMEditRepeat.Enabled;

      MMEditRepeat.Caption := Format( 'Repeat %s', [EDITCMD_NAMES[LastEditCmd]] );
      RTFMRepeatCmd.Caption := MMEditRepeat.Caption;
      TB_Repeat.Hint := MMEditRepeat.Caption;
  end;
end; // UpdateLastCommand

function RunLanguageDialog : boolean;
var
  Form_Lang : TForm_Lang;
begin
  result := false;

  Form_Lang := TForm_Lang.Create( Form_Main );
  try
    Form_Lang.CurrentLang := ActiveNote.Editor.SelAttributes.Language;
    Form_Lang.Combo_Lang.Language := Form_Lang.CurrentLang;
    Form_Lang.RecentLang := KeyOptions.RecentLanguage;
    Form_Lang.DefaultLang := DefaultEditorChrome.Language;

    if ( Form_Lang.ShowModal = mrOK ) then
    begin
      result := true;
      CommandRecall.Language := Form_Lang.Combo_Lang.Language;
      KeyOptions.RecentLanguage := CommandRecall.Language;
    end;
  finally
    Form_Lang.Free;
  end;

end; // RunLanguageDialog

function RunParagraphDialog : boolean;
var
  Form_Para : TForm_Para;
begin
  result := false;

  with CommandRecall.Para do
  begin
    case ActiveNote.Editor.Paragraph.LineSpacing of
      0 : SpacingRule := lsSingle;
      1 : SpacingRule := lsOneAndHalf;
      else
        SpacingRule := lsDouble;
    end;
    LIndent := ActiveNote.Editor.Paragraph.LeftIndent;
    RIndent := ActiveNote.Editor.Paragraph.RightIndent;
    FIndent := ActiveNote.Editor.Paragraph.FirstIndent;
    SpaceBefore := ActiveNote.Editor.Paragraph.SpaceBefore;
    SpaceAfter := ActiveNote.Editor.Paragraph.SpaceAfter;
    Numbering := ActiveNote.Editor.Paragraph.Numbering;
    Alignment := ActiveNote.Editor.Paragraph.Alignment;
  end;

  Form_Para := TForm_Para.Create( Form_Main );
  try
    Form_Para.Para := CommandRecall.Para;
    Form_Para.CurrentNumbering := ActiveNote.Editor.Paragraph.Numbering;
    if ( Form_Para.CurrentNumbering in [nsNone, nsBullet] ) then
      Form_Para.CurrentNumbering := KeyOptions.LastNumbering;

    with Form_Para do
    begin
      Spin_First.Increment := EditorOptions.IndentInc;
      Spin_Left.Increment := EditorOptions.IndentInc;
      Spin_Right.Increment := EditorOptions.IndentInc;
      Spin_SpcBef.Increment := EditorOptions.ParaSpaceInc;
      Spin_SpcAft.Increment := EditorOptions.ParaSpaceInc;
    end;
    if ( Form_Para.ShowModal = mrOK ) then
    begin
      result := true;
      CommandRecall.Para := Form_Para.Para;

      // QuickFix: Bullets don't set indents automatically,
      // but a bulleted list does need left indent. Therefore,
      // we add an indent if not set already:
      if (( CommandRecall.Para.Numbering <> nsNone ) and
         ( CommandRecall.Para.LIndent = 0 )) then
            CommandRecall.Para.LIndent := EditorOptions.IndentInc;
    end;
  finally
    Form_Para.Free;
  end;

end; // RunParagraphDialog

procedure PerformCustomFuncKey( const Key : Word; const Shift : TShiftState );
var
  s : string;
  p : integer;
  cmd : char;
begin
  // check if function key
  if ( not ( key in [VK_F1..VK_F12] )) then exit;
  s := '';

  if ( Shift = [ssAlt] ) then
  begin
    s := AltFKeys[Key-111]; // get value from 1 to 12
  end
  else
  if ( Shift = [ssShift,ssAlt] ) then
  begin
    s := ShiftAltFKeys[Key-111]; // get value from 1 to 12
  end
  else
  if ( Shift = [ssCtrl,ssAlt] ) then
  begin
    s := CtrlAltFKeys[Key-111];  // get value from 1 to 12
  end;

  if ( s = '' ) then exit;
  p := pos( _KEY_FUNC_DELIMITER, s );
  if ( p < 2 ) then exit;
  cmd := s[1];
  delete( s, 1, p );

  case cmd of
    _KEY_FUNC_MACRO : ExecuteMacro( s, '' );
    _KEY_FUNC_PLUGIN : ExecutePlugin( s );
    _KEY_FUNC_TEMPLATE : InsertTemplate( s );
    _KEY_FUNC_STYLE : StyleApply( s );
    _KEY_FUNC_FONT : begin
      RecallingCommand := true;
      CommandRecall.Font.Name := s;
      PerformCmd( ecFontName );
    end;
  end;

end; // PerformCustomFuncKey

procedure ExecuteMacroFile;
var
  fn, oldFilter : string;
begin

  if ( not CheckFolder( 'Macro', Macro_Folder, false, true )) then exit;

  with Form_Main.OpenDlg do
  begin
    oldFilter := Filter;
    Filter := FILTER_MACROS;
    FilterIndex := 1;
    Title := 'Select macro to execute';
    Options := Options - [ofAllowMultiSelect];
    InitialDir := Macro_Folder;
    FileName := LastMacroFN;
  end;

  try
    if Form_Main.OpenDlg.Execute then
    begin
      fn := normalFN( Form_Main.OpenDlg.Filename );
      ExecuteMacro( fn, '' );
    end;
  finally
    Form_Main.OpenDlg.Filter := oldFilter;
  end;


end; // ExecuteMacroFile

procedure MacroInitialize;
begin
    IsRunningMacro := false;
    IsRecordingMacro := false;
    MacroRecordingPaused := false;
    MacroAbortRequest := false;
    MacroErrorAbort := false;
    ActiveMacro := nil;
    StartupMacroFile := '';
    StartupPluginFile := '';
    Form_Main.MacMMacroUserCommand.Enabled := false;


    LastMacroFN := '';

    LastEditCmd := ecNone;
    UpdateLastCommand( ecNone );
    RecallingCommand := false;

    with FontFormatToCopy do
    begin
      Charset := DEFAULT_CHARSET;
      BackColor := clWindow;
      Color := clWindowText;
      Disabled := false;
      Hidden := false;
      Link := false;
      Name := '';
      Offset := 0;
      Pitch := low( TFontPitch );
      IsProtected := false;
      RevAuthorIndex := 0;
      SubscriptStyle := low( TSubscriptStyle );
      Size := 0;
      Style := [];
      Height := 10;
      UnderlineType := low( TUnderlineType );
    end;

    with ParaFormatToCopy do
    begin
      FirstIndent := 0;
      LeftIndent := 0;
      LineSpacing := 0;
      LineSpacingRule := low( TLineSpacingRule );
      Alignment := low( TParaAlignment );
      Numbering := low( TRxNumbering );
      NumberingStyle := low( TRxNumberingStyle );
      NumberingTab := 0;
      RightIndent := 0;
      SpaceAfter := 0;
      SpaceBefore := -1; // MARKER: signals a not-yet-assigned format
      TabCount := 0;
      // [x] Tab[index : integer]
    end;


    with CommandRecall do
    begin
      Color := clWindow;
      GoToIdx := '';
      StyleName := '';
      with Font do // last font properties applied, for repeating last command
      begin
        Charset := DEFAULT_CHARSET;
        Color := clBlack;
        Name := 'MS Sans Serif';
        Size := 10;
        Style := [];
      end;
      with Para do // last paragraph properties applied
      begin
        SpacingRule := lsSingle;
        LIndent := 0;
        RIndent := 0;
        FIndent := 0;
        SpaceBefore := 0;
        SpaceAfter := 0;
        Numbering := nsNone;
        Alignment := paLeftJustify;
      end;
      with CharInfo do // last Insert Character arguments
      begin
        Code := 0;
        Name := '';
        Count := 0;
        Charset := DEFAULT_CHARSET;
      end;
    end;
end;
end.
