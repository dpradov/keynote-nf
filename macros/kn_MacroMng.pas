unit kn_MacroMng;

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
   Winapi.RichEdit,
   System.Classes,
   System.SysUtils,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Graphics,
   Vcl.StdCtrls,
   Vcl.Controls,
   Vcl.Clipbrd,
   Vcl.Menus,

   VirtualTrees,
   RxRichEd,
   RxStrUtils,

   kn_Cmd,
   kn_Info,
   kn_const,
   kn_Macro,
   kn_KntFolder
   ;



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

    FontFormatToCopy : TCharFormat2;
    ParaFormatToCopy : TParaFormat2;
    CopyFormatMode: TCopyFormatMode;


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
    function GetCurrentMacro( const DoWarn : boolean; var index: integer ) : TMacro;
    function GetMacroByName( const aName : string; const DoWarn : boolean ) : TMacro;
    procedure ExecuteMacroFile;
    procedure AddUserMacroCommand;
    function MacroProcess( const DoWarn : boolean ) : boolean;
    function GetMacroIconIndex( const Macro : TMacro ) : integer;
    function GetMacroIndex (FileName: string): integer;

    // edit commands
    procedure PerformCmd( aCmd : TEditCmd );
    procedure PerformCmdEx( aCmd : TEditCmd );
    procedure RepeatLastCommand;
    procedure UpdateLastCommand( const aCMD : TEditCmd );
    function PerformOtherCommandShortcut( const Key : Word; const Shift : TShiftState ) : boolean;
    function RunParagraphDialog : boolean;
    function RunLanguageDialog : boolean;

    function CmdPaste(const fromButton: boolean; const ForcePlain: boolean): boolean;
    Function CmdCopy: boolean;
    Function CmdCut: boolean;

implementation

uses
   gf_miscvcl,
   gf_strings,
   gf_misc,
   kn_Global,
   kn_Main,
   knt.model.note,
   kn_DateTime,
   kn_LanguageSel,
   kn_Paragraph,
   kn_EditorUtils,
   knt.ui.editor,
   kn_clipUtils,
   kn_MacroCmd,
   kn_MacroCmdSelect,
   kn_MacroEdit,
   dll_keyboard,
   kn_BookmarksMng,
   kn_StyleMng,
   knt.ui.tree,
   kn_NoteFileMng,
   kn_PluginsMng,
   kn_TemplateMng,
   kn_FindReplaceMng,
   kn_VCLControlsMng,
   knt.App
   ;


resourcestring
  STR_01 = 'Stop recording macro';
  STR_02 = '&Stop Recording Macro';
  STR_03 = ' Recording macro "%s"';
  STR_04 = 'Command "%s" cannot be included in macros. This command has been executed but will not be recorded in the macro.';
  STR_05 = 'You have executed a command which opens a dialog box. KeyNote can ' +
      'remember the values you have just selected and use them when ' +
      'replaying the macro, OR KeyNote can display the dialog box again to let you select values ' +
      'each time the macro is executed. Do you want KeyNote to remember current values?' + #13#13 +
      'Click YES to save current values. Click NO to always open the dialog box. Click CANCEL to ' +
      'skip this command and continue recording macro.';
  STR_06 = ' Macro recording PAUSED';
  STR_07 = ' Macro recording RESUMED';
  STR_08 = 'Macro "%s" contains no commands and will be discarded.';
  STR_09 = 'Save new macro "%s" (%d lines)?';
  STR_10 = 'Error saving macro "%s": %s';
  STR_11 = ' Macro recorded and saved.';
  STR_12 = ' Macro discarded.';
  STR_13 = ' Macro error';
  STR_14 = 'Error adding new macro "%s": %s';
  STR_15 = 'Record a new macro';
  STR_16 = '&Record Macro...';
  STR_17 = 'Active folder "%s" is Read-only. Running the macro may cause the folder to be modified. Do you want the macro to run anyway?';
  STR_18 = 'Cannot load macro file "%s". Reason: %s';
  STR_19 = 'Running macro "%s"';
  STR_20 = 'Execute most recent macro "%s"';
  STR_21 = 'Error loading macro "%s": %s';
  STR_22 = 'Cannot execute macro "%s": This macro requires a newer version of KeyNote.';
  STR_23 = ' Unexpected error while executing macro';
  STR_24 = ' Macro was aborted by user';
  STR_25 = ' Macro done';
  STR_26 = 'OK to delete selected macro "%s"?';
  STR_27 = 'Cannot delete macro file "%s"';
  STR_28 = 'Error while deleting macro: ';
  STR_29 = 'Macro aborted on line %d: "%s"' + #13 + 'Reason: %s';
  STR_30 = 'unknown command';
  STR_31 = 'syntax error';
  STR_32 = 'unknown user command';
  STR_33 = 'string argument required';
  STR_34 = 'integer argument required';
  STR_35 = 'Cannot run embedded macro "%s". Reason: %s';
  STR_36 = 'Folder creation failed';
  STR_37 = 'Invalid font style argument';
  STR_38 = 'Unexpected error while executing macro: %s' + #13#13 +
          'Last macro line was: "%s" (line %d)';
  STR_39 = 'This command cannot be executed while macro is being recorded or replayed.';
  STR_40 = 'Macro "%s" not found.';
  STR_41 = 'No macros available or none selected.';
  STR_42 = 'Could not access current macro.';
  STR_43 = ' This command cannot be repeated';
  STR_44 = 'This action cannot be performed, because there is no active folder (%d)';
  STR_45 = 'This folder cannot be set as Read-only, because it is being used for clipboard capture.';
  STR_46 = 'Failed to assign font attributes.';
  STR_47 = 'Failed to assign paragraph attributes.';
  STR_48 = 'Go to line';
  STR_49 = 'Enter line number or increment (+-):';
  STR_51 = 'Cannot perform command:';
  STR_52 = 'No font attributes to paste from: Use "Copy font attributes" first.';
  STR_53 = 'No paragraph attributes to paste from: Use "Copy paragraph attributes" first.';
  STR_54 = '"%s" is not a valid number';
  STR_55 = 'New background color will be assigned to ALL TREE NODES in folder %s' + #13 + 'Continue?';
  STR_56 = 'Repeat %s';
  STR_57 = 'Select macro to execute';
  STR_58 = 'Failed to copy text formatting';

var
    RecallingCommand : boolean; // if TRUE, we use information in CommandRecall


function GetMacroByFileName( FileName : string; var wasNewMacro: boolean ) : TMacro; forward;

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
    LoadMacroList (true);

    for i := 1 to Macro_List.Count do begin
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

  if IsRecordingMacro then begin
    StopRecordingMacro;
    exit;
  end;

  if MacroProcess( true ) then exit;
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;

  if ( not CheckFolder( 'Macro', Macro_Folder, true, true )) then exit;

  EditMacro( true ); // create a new macro
  if ( ActiveMacro = nil ) then exit;

  LastEditCmd := ecNone;
  UpdateLastCommand( ecNone );
  try
    ActiveFolder.Editor.SetFocus;
  except
  end;

  // preset or clear some global settings
  FindOptions.Pattern := ''; // enforce new search

  IsRecordingMacro := true;
  MacroRecordingPaused := false;
  with Form_Main do begin
    TB_MacroRecord.ImageIndex := TB_MacroRecord.ImageIndex + 1;
    TB_MacroRecord.Hint := STR_01;
    MacMMacro_Record.Caption := STR_02;
    MacMMacro_Record.Hint := TB_MacroRecord.Hint;
    MacMMacroUserCommand.Enabled := true;

    Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_03, [ActiveMacro.Name] );
    SelectStatusbarGlyph( true );
  end;

end; // RecordMacro


procedure AddMacroKeyPress( const Key : Word; const Shift : TShiftState );
begin
  if MacroRecordingPaused then exit;
  if ( not assigned( ActiveMacro )) then exit;

  ActiveMacro.Lines.Add( Format('%d%s%s', [Key,_MACRO_DELIMITER_CHAR,ShiftStateToStr( Shift )]
  ));
end; // AddMacroKeyPress


procedure AddMacroEditCommand( aCmd : TEditCmd );
var
  OnPlayShowDlg : boolean;

begin
  if MacroRecordingPaused then exit;
  if ( not assigned( ActiveMacro )) then exit;
  if ( aCmd = ecNone ) then exit;


  if ( aCmd in CommandsProhibitedInMacros ) then begin
    messagedlg( Format(
      STR_04,
      [EDITCMD_NAMES[aCmd]]
      ), mtWarning, [mbOK], 0 );
    exit;
  end;

  if ( aCMD in EditCommandsWithNoArgs ) then begin
    ActiveMacro.Lines.Add( Format(
      '%s%s',
      [_MACRO_CMD_CHAR,EDITCMD_MACRO_NAMES[aCMD]]
    ));
  end
  else
  if ( aCMD in EditCommandsWithDialogs ) then begin
    case messagedlg( STR_05,
      mtConfirmation, [mbYes,mbNo,mbCancel], 0 ) of
        mrYes : OnPlayShowDlg := false;
        mrNo : OnPlayShowDlg := true;
        else
          exit;
    end;

    if OnPlayShowDlg then begin
      ActiveMacro.Lines.Add( Format(
        '%s%s',
        [_MACRO_CMD_CHAR,
        EDITCMD_MACRO_NAMES[aCMD]
      ]));
    end
    else begin
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
        ecFontDlg : with CommandRecall.Font do begin
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
        ecParaDlg : with CommandRecall.Para do begin
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
  else begin
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
      ecInsCharacter, ecInsCharacterU : begin
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
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_06
  else
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_07;
end; // PauseRecordingMacro


procedure StopRecordingMacro;
begin

  try
    if ( ActiveMacro = nil ) then exit;
    try
      if MacroRecordingPaused then
        PauseRecordingMacro;
      Form_Main.MacMMacroUserCommand.Enabled := false;

      if ( ActiveMacro.Lines.Count = 0 ) then begin
        Messagedlg( Format(STR_08,[ActiveMacro.Name]), mtInformation, [mbOK], 0 );
        ActiveMacro.Free;
        exit;
      end;

      if (messagedlg(Format(STR_09, [ActiveMacro.Name,ActiveMacro.Lines.Count]), mtConfirmation, [mbYes,mbNo], 0 ) <> mrNo ) then begin
        if ( not ActiveMacro.Save ) then begin
          messagedlg( Format(STR_10, [ActiveMacro.FileName,ActiveMacro.LastError]), mtError, [mbOK], 0 );
          ActiveMacro.Free;
          exit;
        end;
        Form_Main.ListBox_ResMacro.ItemIndex := Form_Main.ListBox_ResMacro.AddItem( ActiveMacro.Name, cbUnchecked, GetMacroIconIndex( ActiveMacro ));
        Macro_List.AddObject( ActiveMacro.Name, ActiveMacro );
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_11
      end
      else begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_12;
        ActiveMacro.Free;
      end;

    except
      on E : Exception do begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_13;
        messagedlg( Format(STR_14, [ActiveMacro.Name,E.Message] ), mtError, [mbOK], 0 );
        ActiveMacro.Free;
      end;
    end;

  finally
    ActiveMacro := nil;
    IsRecordingMacro := false;
    MacroRecordingPaused := false;
    with Form_Main do begin
      SelectStatusbarGlyph( true );
      TB_MacroRecord.ImageIndex := TB_MacroRecord.ImageIndex - 1;
      TB_MacroRecord.Hint := STR_15;
      MacMMacro_Record.Caption := STR_16;
      MacMMacro_Record.Hint := TB_MacroRecord.Hint;
    end;
  end;
end; // StopRecordingMacro


procedure ExecuteMacro( aFileName, aMacroName : string );
var
  macro : TMacro;
  wasNewMacro, wasreadonly : boolean;
  index: integer;
begin

  // if aFileName is specified, the macro is loaded from the file specified. Else, if aName is specified, the procedure looks
  // for a macro in Macro_List

  if MacroProcess( true ) then exit;

  Macro := nil;
  wasNewMacro := false;

  // Normally, macros cannot run if there are no notes
  // - Exception: We do allow _MACRO_AUTORUN_NEW_FILE to run, otherwise the feature would be impossible to implement.
  // It is the macro author's responsibility to make sure that the macro dooesn't do anything unreasonable, like
  // inserting text before creating a folder
  // - Exception: Executing macro on scratchpad

  if ( aFileName <> _MACRO_AUTORUN_NEW_FILE ) and not (assigned(ActiveEditor) and ActiveEditor.Focused) then begin
    if ( not Form_Main.HaveKntFolders( true, true )) then exit;
    if ( not assigned( ActiveFolder )) then exit;
  end;

  wasreadonly := Form_Main.FolderIsReadOnly(ActiveFolder, false);
  if wasreadonly then begin
    if ( App.DoMessageBox( Format(STR_17,[ActiveFolder.Name]), mtWarning, [mbYes,mbNo] ) <> mrYes ) then
       exit;
    ActiveFolder.ReadOnly := false;
  end;

  if ( aFileName <> '' ) then
    Macro:= GetMacroByFileName (aFileName, wasNewMacro)
  else
    if ( aMacroName = '' ) then
       Macro := GetCurrentMacro( true, index )
    else
       Macro := GetMacroByName( aMacroName, true );

  if ( macro = nil ) then
    exit;

  IsRunningMacro := true;
  MacroAbortRequest := false;
  MacroErrorAbort := false;
  Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format(STR_19, [Macro.Name] );
  screen.Cursor := crAppStart;
  SelectStatusbarGlyph( true );

  try
    if not ActiveEditor.Focused then          // Can be Scratchpad editor
       ActiveFolder.Editor.SetFocus;
  except
  end;

  LastMacroFN := ExtractFilename( Macro.FileName );
  Form_Main.MMToolsMacroRunLast.Hint := Format(STR_20, [LastMacroFN]);

  try
    try
      if ( not Macro.Load ) then begin
         if not IsAutorunMacro(aFileName) then
            App.ErrorPopup(Format(STR_21, [Macro.FileName,Macro.LastError]));
         exit;
      end;

      if ( Macro.Version.Major > _MACRO_VERSION_MAJOR ) then begin
         App.ErrorPopup(Format(STR_22, [Macro.FileName]));
         exit;
      end;

      PlayMacro( Macro );

    except
      On E : Exception do begin
        App.ErrorPopup(E);
        exit;
      end;
    end;

  finally

    if MacroErrorAbort then
       Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_23
    else
    if MacroAbortRequest then
       Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_24
    else
       Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_25;

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
      ActiveFolder.ReadOnly := true;
  end;

  try
    ActiveEditor.SetFocus;
  except
  end;

end; // ExecuteMacro


procedure EditMacro( const AsNew  : boolean );
var
  Form_Macro: TForm_Macro;
  Macro : TMacro;
  index: integer;
begin

  if MacroProcess( true ) then exit;

  if AsNew then
    Macro := TMacro.Create

  else begin
    Macro := GetCurrentMacro( true, index );
    if ( Macro = nil ) then
      exit;

    if ( not Macro.Load ) then begin
      App.ErrorPopup(Format(STR_21, [Macro.FileName, Macro.LastError]));
      Macro.Free;
      exit;
    end;
  end;

  Form_Macro := TForm_Macro.Create( Form_Main );
  try
    with Form_Macro do begin
      MName := Macro.Name;
      MDesc := Macro.Description;
      MDate := Macro.DateModified;
      MFileName := Macro.FileName;
      MProfile := Macro.ProfileSpecific;
      MAbort := Macro.AbortOnError;
      myNewMacro := AsNew;
    end;

    if ( Form_Macro.ShowModal = mrOK ) then begin
      with Form_Macro do begin
        Macro.Name := MName;
        Macro.Description := MDesc;
        Macro.DateModified := DateTimeToStr( now );
        Macro.AbortOnError := MAbort;
        Macro.ProfileSpecific:= MProfile;

        if AsNew then begin
          Macro.FileName := MFileName;
          ActiveMacro := Macro;
        end
        else begin
          // update existing macro

          if ( MName <> OriginalName ) then begin
            Form_Main.ListBox_ResMacro.Items.Delete( Index );
            Form_Main.ListBox_ResMacro.ItemIndex := Form_Main.ListBox_ResMacro.AddItem( Macro.Name, cbUnchecked, GetMacroIconIndex( Macro ));
            Macro_List.Delete( Macro_List.IndexOf( OriginalName ));
            Macro_List.AddObject( MName, Macro );
          end;

          if ( not Macro.Save ) then begin
            App.ErrorPopup(Format(STR_10, [Macro.FileName,Macro.LastError]));
            exit;
          end;
        end;
      end;
    end
    else begin
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
  FilePath: string;
  index: integer;
begin

  if MacroProcess( true ) then exit;
  Macro := GetCurrentMacro( true, index );
  if ( macro = nil ) then exit;

  if ( App.DoMessageBox( Format(STR_26, [Macro.Name]), mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then
     exit;

  DeleteSuccess := false;
  try
    try
      FilePath:= Macro.GetFolder + Macro.FileName;

      if ( not deletefile( FilePath )) then begin
        DeleteSuccess := false;
        App.ErrorPopup(Format(STR_27, [FilePath]));
        exit;
      end;
      Form_Main.ListBox_ResMacro.Items.Delete( index );
      i := Macro_List.IndexOf( Macro.Name );
      if ( i >= 0 ) then begin
        Macro.Free;
        Macro_List.Delete( i );
      end;
      DeleteSuccess := true;

    except
      on E : Exception do begin
        DeleteSuccess := false;
        App.ErrorPopup(E, STR_28);
      end;
    end;

  finally
    if DeleteSuccess then begin
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
     messagedlg( Format(STR_29, [i,copy( line, 1, 127 ),s]), mtError, [mbOK], 0 );
     MacroFinished := true;
     MacroErrorAbort := true;
   end;


begin
  if ( not assigned( Macro )) then exit;
  if ( MacroAbortRequest or MacroErrorAbort ) then exit;

  if ActiveEditor.SupportsRegisteredImages then begin
     if ActiveEditor.SelLength > 0 then
        ActiveEditor.CheckToSelectLeftImageHiddenMark;
  end;

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

        for i := 1 to Macro.Lines.Count do begin
          ArgList.Clear;                               // [dpv]

          Application.ProcessMessages;
          if MacroAbortRequest then begin
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
                cmdstr := line
              else begin
                cmdstr := copy( line, 1, pred( p ));
                argstr := copy( line, succ( p ), length( line ));
              end;

              cmdstr := lowercase( cmdstr );
              cmdidx := EditCmds.IndexOf( cmdstr );
              if ( cmdidx < 0 ) then
                 if Macro.AbortOnError then begin
                   AbortMacro( STR_30, i, Macro.Lines[pred( i )] );
                   break;
                 end
                 else begin
                   inc( ErrorCnt );
                   continue;
                 end;

              EditCmd := TEditCmd( cmdidx );
              if ( EditCmd in EditCommandsWithNoArgs ) then begin
                LastEditCmd := EditCmd;
                RepeatLastCommand;
              end
              else
              if ( EditCmd in EditCommandsWithDialogs ) then begin
                if ( argstr = '' ) then begin
                  if ( EditCmd in EditCommandsEx ) then
                    PerformCmdEx( EditCmd )
                  else
                    PerformCmd( EditCmd );
                end
                else begin
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
                        with CommandRecall.Font do begin
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
                        with CommandRecall.Para do begin
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
                    on E : Exception do begin
                      if Macro.AbortOnError then begin
                        AbortMacro( E.Message, i, Macro.Lines[pred( i )] );
                        break;
                      end
                      else begin
                        inc( ErrorCnt );
                        continue;
                      end;
                    end;
                  end;
                end;
              end

              else begin
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

                    ecInsCharacter, ecInsCharacterU : begin
                      CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                      with CommandRecall.CharInfo do begin
                        Code := strtoint( ArgList[0] );
                        Count := strtoint( ArgList[1] );
                        Name := ArgList[2];
                        Charset := strtoint( ArgList[3] );
                      end;
                    end;

                    ecFindText : begin
                      CSVTextToStrs( ArgList, argstr, _MACRO_DELIMITER_CHAR );
                      with FindOptions do begin
                        Pattern := ArgList[0];
                        Text_to_Find := Pattern;
                        MatchCase := ( ArgList[1] = BOOLEANSTR[true] );
                        WholeWordsOnly := ( ArgList[2] = BOOLEANSTR[true] );
                        AllNodes := false;
                        AllTabs := false;
                        CurrentNodeAndSubtree := false;
                        AllTabs_FindReplace:= false;
                        EntireScope := false;
                        Wrap := false;
                      end;
                    end;

                    ecStyleApply: begin
                      CommandRecall.StyleName:= argstr;
                    end;
                  end;

                  LastEditCmd := EditCmd;
                  RepeatLastCommand;

                except
                  on E : Exception do begin
                    if Macro.AbortOnError then begin
                      AbortMacro( E.Message, i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else begin
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
                cmdstr := AnsiUpperCase( line )
              else begin
                cmdstr := AnsiUpperCase( copy( line, 1, pred( p )));
                delete( line, 1, p );
                p := pos( ')', line );
                if ( p = 0 ) then begin
                  if Macro.AbortOnError then begin
                    AbortMacro( STR_31, i, Macro.Lines[pred( i )] );
                    break;
                  end
                  else begin
                    inc( ErrorCnt );
                    continue;
                  end;
                end
                else
                  argstr := copy( line, 1, pred( p ));
              end;

              cmdidx := MacroCmds.IndexOf( cmdstr );
              if ( cmdidx < 0 ) then begin
                if Macro.AbortOnError then begin
                  AbortMacro( STR_32, i, Macro.Lines[pred( i )] );
                  break;
                end
                else begin
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
                  if ( argstr = '' ) then begin
                    if Macro.AbortOnError then begin
                      AbortMacro( STR_33, i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else begin
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
                    if Macro.AbortOnError then begin
                      AbortMacro( STR_34, i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;
                end;
              end;

              case macrocmd of
                macInsert : begin
                  with ActiveEditor do begin
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
                  DoMessageBox(ExpandMetaChars( argstr ), Macro.Name,
                    MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL );
                end;

                macStatus : begin
                  Form_Main.StatusBar.Panels[PANEL_HINT].Text := argstr;
                end;

                macPlugin : begin
                end;

                macFontColor : begin
                  ActiveEditor.SelAttributes.Color := StringToColor( argstr );
                end;

                macBGColor : begin
                  // call PerformCmd here, because setting BG color
                  // for a folder involves more work than just
                  // setting the RichEdit.Color property
                  CommandRecall.Color := StringToColor( argstr );
                  LastEditCmd := ecBGColorDlg;
                  RepeatLastCommand;
                end;

                macHighlightColor : begin
                  ActiveEditor.SelAttributes.BackColor := StringToColor( argstr );
                end;

                macMacro : begin
                  newMacro := TMacro.Create;
                  try
                    if ( extractfileext( lowercase( argstr )) <> ext_Macro ) then
                      argstr := argstr + ext_Macro;
                    newMacro.FileName := argstr;
                    if ( not NewMacro.Load ) then begin
                      if Macro.AbortOnError then begin
                        AbortMacro( Format(STR_35,[argstr, Macro.LastError]), i, Macro.Lines[pred( i )] );
                        break;
                      end
                      else begin
                        inc( ErrorCnt );
                        continue;
                      end;
                    end
                    else
                      PlayMacro( newMacro ); // RECURSIVE CALL

                  finally
                    newMacro.Free;
                  end;
                end;

                macConfirm : begin
                  if ( DoMessageBox( argstr, Macro.Name,
                    MB_OKCANCEL+MB_ICONQUESTION+MB_DEFBUTTON1+MB_APPLMODAL ) = ID_CANCEL ) then begin
                    // user clicked CANCEL, so abort macro
                    MacroFinished := true;
                    break;
                  end;
                end;

                macNoteNewRTF, macNoteNewTree : begin
                  // always abort if fail
                  if ( not TKntFolder.NewKntFolder( true, true )) then begin
                    AbortMacro( STR_36, i, Macro.Lines[pred( i )] );
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
                  else begin
                    if Macro.AbortOnError then begin
                      AbortMacro(STR_37, i, Macro.Lines[pred( i )] );
                      break;
                    end
                    else begin
                      inc( ErrorCnt );
                      continue;
                    end;
                  end;

                  if ( macrocmd = macStyleOn ) then
                     with ActiveEditor.SelAttributes do
                       Style := Style + [myFontStyle]
                  else
                  if ( macrocmd = macStyleOff ) then
                     with ActiveEditor.SelAttributes do
                        Style := Style - [myFontStyle]
                  else // macStyleFlip
                     with ActiveEditor.SelAttributes do
                       if myFontStyle in Style then
                          Style := Style - [myFontStyle]
                       else
                          Style := Style + [myFontStyle];
                end;

                macGoUp : begin
                  for counter := 1 to argint do
                     with ActiveEditor do begin
                       Perform( WM_KEYDOWN, VK_UP, 0 );
                       Perform( WM_KEYUP, VK_UP, 0 );
                     end;
                end;

                macGoDown : begin
                  for counter := 1 to argint do
                     with ActiveEditor do begin
                       Perform( WM_KEYDOWN, VK_DOWN, 0 );
                       Perform( WM_KEYUP, VK_DOWN, 0 );
                     end;
                end;

                macGoRight : begin
                  for counter := 1 to argint do
                     with ActiveEditor do begin
                       Perform( WM_KEYDOWN, VK_RIGHT, 0 );
                       Perform( WM_KEYUP, VK_RIGHT, 0 );
                     end;
                end;

                macGoLeft : begin
                  for counter := 1 to argint do
                     with ActiveEditor do begin
                       Perform( WM_KEYDOWN, VK_LEFT, 0 );
                       Perform( WM_KEYUP, VK_LEFT, 0 );
                     end;
                end;

                macSelectAll : begin
                  ActiveEditor.SelectAll;
                end;

              end; // case cmd

            end;

            else begin // bare keypress information
              while ( not ActiveEditor.Focused ) do begin
                // RTF *must* be focused, otherwise keypresses
                // will be sent to the wrong control.
                // while RTF is not focused, we just pause
                Application.ProcessMessages;
                if MacroAbortRequest then begin
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

              PostKeyEx( ActiveEditor.Handle, sndKey, sndShift, False )
            end;

          end;
        end; // lines.count

      until MacroFinished;

    except
      On E : Exception do begin
        MacroFinished := true;
        MacroErrorAbort := true;
        messagedlg( Format(STR_38,[E.Message,copy( Macro.Lines[pred(linecnt)], 1, 127 ),linecnt]), mtError, [mbOK], 0 );
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
     messagedlg( STR_39, mtInformation, [mbOK], 0 );
end; // MacroProcess


procedure AddUserMacroCommand;
var
  Form_MacroCmd : TForm_MacroCmd;
begin
  if ( not ( assigned( ActiveMacro ) and IsRecordingMacro )) then exit;

  Form_MacroCmd := TForm_MacroCmd.Create( Form_Main );
  try
    if ( Form_MacroCmd.ShowModal = mrOK ) then begin
      with Form_MacroCmd do begin
        if ( myArgs <> '' ) then
          ActiveMacro.Lines.Add( Format('%s%s(%s)', [_MACRO_USERCMD_CHAR,MACRO_CMD_NAMES[myCmd],myArgs]))
        else
          ActiveMacro.Lines.Add( Format('%s%s', [_MACRO_USERCMD_CHAR,MACRO_CMD_NAMES[myCmd]] ));
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
    result := TMacro( Macro_List.Objects[i] )
  else
    if DoWarn then
       App.ErrorPopup(Format( STR_40, [aName]));

end; // GetMacroByName


function GetCurrentMacro( const DoWarn : boolean; var index: integer) : TMacro;
begin
  result := nil;

  if ( not CheckResourcePanelVisible( true )) then exit;

  index:= Form_Main.ListBox_ResMacro.ItemIndex;

  if (( Form_Main.ListBox_ResMacro.Items.Count = 0 ) or (Index < 0)) then begin
    if DoWarn then
       messagedlg( STR_41, mtError, [mbOK], 0 );
    exit;
  end;

  try
    try
      result := TMacro(
        Macro_List.Objects[Macro_List.IndexOf( Form_Main.ListBox_ResMacro.Items[Index] )]);
    except
      result := nil;
    end;

  finally
    if (( result = nil ) and DoWarn ) then
       messagedlg( STR_42, mtError, [mbOK], 0 );
  end;
end; // GetCurrentMacro


function GetMacroIndex (FileName: string): integer;
var
  i: integer;
  Macro: TMacro;
begin
  FileName:= AnsiLowerCase(FileName);
  for i:= 0 to Macro_List.Count-1 do begin
     Macro:= TMacro(Macro_List.Objects[i]);
     if AnsiLowerCase(Macro.FileName) = FileName then
        exit(i);
  end;
  exit(-1);
end;


function GetMacroByFileName( FileName : string; var wasNewMacro: boolean ) : TMacro;
var
  i : integer;
  Macro: TMacro;
begin
  result := nil;
  wasNewMacro:= false;

  i := GetMacroIndex( FileName );

  if ( i >= 0 ) then
    Macro := TMacro( Macro_List.Objects[i] )

  else begin
    if ( pos( '\', FileName ) = 0 ) then
       FileName := Macro_Folder + FileName;

    Macro := TMacro.Create;
    Macro.FileName := FileName;
    wasNewMacro := true;
  end;

  Result:= Macro;
end;
// GetMacroByFileName



procedure RepeatLastCommand;
begin
  if ( LastEditCmd = ecNone ) then exit;

  if ( LastEditCmd in RepeatableEditCommands ) then begin
    if ( LastEditCmd in EditCommandsEx ) then
      PerformCmdEx( LastEditCmd )
    else
      PerformCmd( LastEditCmd );
  end
  else
  if ( LastEditCmd in RememberedEditCommands ) then begin
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
     Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_43;

end; // RepeatLastCommand


procedure PerformCmdEx( aCmd : TEditCmd );
var
  s : string;
  errorStr: string;
  NotImplemented, Canceled: boolean;
  RTFAux : TRxRichEdit;
  SelStartOrig, SelLengthOrig, p: integer;
  Editor: TKntRichEdit;
begin
  // Perform command on ActiveEditor.
  // The command does not modify the Editor,  (*1)
  // hence is safe to use when Editor is set
  // to ReadOnly.
  // (*1): Can be used when is set to ReadOnly, although the command ecReadOnly
  //       CAN modify the folder.
  if ( App.Kbd.RTFUpdating or ActiveFileIsBusy ) then exit;

  if ( not assigned( ActiveEditor )) then begin
    if App.opt_Debug then begin
      {$IFDEF KNT_DEBUG}
       Log.Add( 'ActiveEditor not assigned in PerformCmd (' + inttostr( ord( aCmd )) + ')' );
      {$ENDIF}
       App.PopupMessage( Format( STR_44, [ord( aCmd )] ), mtError, [mbOK] );
    end;
    exit;
  end;

  Editor:= ActiveEditor;

  errorStr:= '';
  NotImplemented:= false;
  Canceled:= false;

  try
      case aCMD of
          ecCopy : begin
            LastCopyFromScratchpad:= (Editor.NNodeObj=nil);
            LastCopiedIDImage:= 0;
            if Editor.SupportsRegisteredImages then begin
               if Editor.SelLength = 1 then begin
                  SelStartOrig:= -1;
                  LastCopiedIDImage:= Editor.CheckToIdentifyImageID(p);   // We will save the ID associated with the image (if it is and we have it). We want to be able to offer an image format and not just RTF
                  RTFAux:= Editor;                                         // A selected image -> SelLength=1
               end
               else
                 Editor.CheckToSelectLeftImageHiddenMark(SelStartOrig, SelLengthOrig);
            end;

            RTFAux:= Editor.GetRichEditorWithNoKNTHiddenCharacters (hmOnlyBookmarks, true);  // Remove only hidden characters vinculated to bookmarks
            try
               kn_ClipUtils.CopyToClipboard (RTFAux);
               LogRTFHandleInClipboard();

               { Replaced by the use of of LogRTFHandleInClipboard() / ClipboardContentWasCopiedByKNT()
               if EditorOptions.PlainDefaultPaste then
                 TestCRCForDuplicates(Clipboard.TryAsText);
               }
            finally
               if RTFAux <> Editor then
                  RTFAux.Free;

               if Editor.SupportsRegisteredImages then begin
                  if SelStartOrig >= 0 then
                     Editor.SetSelection(SelStartOrig, SelStartOrig + SelLengthOrig, true);
               end;
            end;
          end;

          ecReadOnly : begin                                    // Currently only applies to Folder, not to the Editor
            if not assigned(ActiveFolder) then exit;

            if ( ActiveFolder = ClipCapMng.ClipCapFolder ) then
               errorStr:= STR_45
            else begin
              ActiveFolder.ReadOnly := (not ActiveFolder.ReadOnly);
              Form_Main.UpdateFolderDisplay;
              ActiveFolder.Editor.UpdateCursorPos;
              ActiveFolder.Modified := true;
            end;
          end;

          ecFontFormatCopy :
            try
              Editor.SaveTextAttributes(FontFormatToCopy);
            except
              errorStr:= STR_46;
            end;

          ecParaFormatCopy :
            try
              Editor.SaveParagraphAttributes(ParaFormatToCopy);
            except
              errorStr:= STR_47;
            end;

          ecCopyFormat :
            try
               with Editor do begin
                  Editor.SaveTextAttributes( FontFormatToCopy );
                  if (SelLength = 0) or (ParagraphsSelected) then
                     Editor.SaveParagraphAttributes( ParaFormatToCopy )
                  else
                     ParaFormatToCopy.dySpaceBefore := -1; // MARKER: signals a not-yet-assigned format
                end;

            except
              errorStr:= STR_58;
            end;

          ecInsOvrToggle : begin                                  // Currently only applies to Folder, not to the Editor
            if not assigned(ActiveFolder) then exit;
            ActiveFolder.IsInsertMode := ( not ActiveFolder.IsInsertMode );
            Form_Main.ShowInsMode;
          end;

          ecMatchBracket :
            Editor.MatchBracket;

          ecSelectWord :
            Editor.GetWordAtCursor( true );

          ecGoTo : begin
            s := CommandRecall.GoToIdx;
            if ( not RecallingCommand ) then begin
              if ( not InputQuery( STR_48, STR_49, s )) then
                 s := '';
            end;
            if ( s <> '' ) then begin
              try
                Editor.GoToLine (s);
                CommandRecall.GoToIdx := s;
              except
                on E : Exception do
                  errorStr:= E.Message;
              end;
            end
            else
              Canceled:= true; // no line number specified
          end

          else begin
            if RecallingCommand then begin
              case aCmd of
                ecFindText :
                  RunFindNext;
              end;
            end
            else
              NotImplemented:= true;
          end;

      end;

      if NotImplemented or Canceled or (errorStr <> '') then begin
         if NotImplemented then
            App.WarnCommandNotImplemented( EDITCMD_NAMES[aCMD] )
         else if errorStr <> '' then
            App.PopupMessage( errorStr, mtError, [mbOK] );
         aCmd := ecNone;
      end
      else begin
         UpdateLastCommand( aCMD );
         if IsRecordingMacro then
            AddMacroEditCommand( aCMD );
      end;

  except
      on E : Exception do begin
        App.PopupMessage( STR_51 + #13 + E.Message, mtError, [mbOK] );
       {$IFDEF KNT_DEBUG}
        Log.Add( 'Exception in PerformCmdEx (' + inttostr( ord( aCMD )) + '): ' + E.Message );
       {$ENDIF}
      end;
  end;


end; // PerformCmdEx



procedure PerformCmd( aCmd : TEditCmd );
var
  txt, txt2 : string;
  p, i, lineindex : integer;
  templist : TStringList;
  tempChrome : TChrome;
  ShiftWasDown : boolean;
  myTreeNode : PVirtualNode;
  myPara : TParaFormat2;
  maxIndent: integer;
  ErrNoTextSelected, ErrNotImplemented, Canceled: boolean;
  SelStartOrig, SelLengthOrig: integer;
  Editor: TKntRichEdit;
  Folder: TKntFolder;
  TreeUI: TKntTreeUI;
  NNode: TNoteNode;

    Procedure CmdNumbering(tipo : TRxNumbering);
    var
      actualNumbering : TRxNumbering;
      actualNumberingStyle: TRxNumberingStyle;
      leftIndent, actLeftIndent: integer;
    begin
       with ActiveEditor do begin
          actualNumbering:= Paragraph.Numbering;
          actualNumberingStyle:= Paragraph.NumberingStyle;
          If (actualNumbering = tipo) and (actualNumberingStyle = KeyOptions.LastNumberingStyle) and (NumberingStart = 1) Then
              Paragraph.Numbering := nsNone
          else begin
                LeftIndent:= Round(10* SelAttributes.Size/5.7);
                actLeftIndent:= Paragraph.LeftIndent;
                if actLeftIndent > LeftIndent then
                   LeftIndent:= actLeftIndent;
               if LeftIndent < 13 then LeftIndent := 13;
               Paragraph.SetNumberingList(tipo, KeyOptions.LastNumberingStyle, NumberingStart, leftIndent);
          End
       end;
    End;

begin
  // Perform command on ActiveEditor
  // The command MODIFIES the folder if the ActiveEditor is vinculated to a folder,
  // hence cannot be executed when that folder is set to ReadOnly.
  if ( App.Kbd.RTFUpdating or ActiveFileIsBusy ) then exit;
  if not App.CheckActiveEditorNotReadOnly then exit;

  Editor:= ActiveEditor;
  TreeUI:= ActiveTreeUI;

  Canceled:= False;
  ErrNoTextSelected:= False;
  ErrNotImplemented:= False;

  Editor.BeginUpdate;
  with Editor do begin
    try
      try
        case aCMD of
          ecBold :
            with SelAttributes do
               SetBold(not (fsBold in Style));

          ecUnderline :
            with SelAttributes do
               SetUnderline(not (fsUnderline in Style));

          ecItalics :
            with SelAttributes do
               SetItalic(not (fsItalic in Style));

          ecStrikeOut :
            with SelAttributes do
               SetStrikeOut(not (fsStrikeout in Style));

          ecCut: begin
              if Editor.SupportsRegisteredImages then begin
                 p:= -1;
                 if SelLength = 1 then begin
                    SelStartOrig:= -1;
                    LastCopiedIDImage:= CheckToIdentifyImageID(p);   // We will save the ID associated with the image (if it is and we have it). We want to be able to offer an image format and not just RTF
                 end
                 else begin
                    LastCopiedIDImage:= 0;
                    CheckToSelectLeftImageHiddenMark(SelStartOrig, SelLengthOrig);
                 end;

                 kn_ClipUtils.CutToClipboard (Editor);

                 if p >= 0 then begin
                    // We have not copied (cut) the hidden label along with the image, and we must delete it from the initial point
                    // p: posFirstHiddenChar
                    SetSelection(p, SelStart, true);
                    SelText:= '';
                 end;
                 if SelStartOrig >= 0 then
                    SetSelection(SelStartOrig, SelStartOrig + SelLengthOrig, true);
              end
              else
                 kn_ClipUtils.CutToClipboard (Editor);


              LogRTFHandleInClipboard();
              { Replaced by the use of of LogRTFHandleInClipboard() / ClipboardContentWasCopiedByKNT()
              if EditorOptions.PlainDefaultPaste then
                 TestCRCForDuplicates(Clipboard.TryAsText);
              }
            end;

          ecPaste :
            begin
              if Editor.SupportsRegisteredImages then begin
                 // If we have an image selected (and maybe more elements), we need to make sure we also select the possible
                 // hidden mark with the ID, to replace it along with the image with the pasted text
                 if SelLength > 0 then
                    CheckToSelectLeftImageHiddenMark;
              end;

              var FolderName:= '';
              if assigned(ActiveFolder) then
                 FolderName:= ActiveFolder.Name;

              if not EditorOptions.PlainDefaultPaste or not Clipboard.HasFormat(CF_TEXT) then
                 PasteBestAvailableFormat(FolderName, true, true)

              else begin
                { Replaced by the use of of LogRTFHandleInClipboard() / ClipboardContentWasCopiedByKNT()
                 // We must paste as PlainText (considering also PlainTextMode) if text has been copied from outside KN
                 // If text have been copied from inside KNT then CRC will correspond to the value calculated with last copy operation
                 // Unless the copied text also includes an image, in which case Clipboard.TryAsText may return ''...
                 txt:= Clipboard.TryAsText;
                 if TestCRCForDuplicates(txt, false) then
                    TryPasteRTF(ActiveFolder.Editor)
                 else
                    PerformCmdPastePlain(ActiveFolder, txt);
                 }
                 if ClipboardContentWasCopiedByKNT() then
                    TryPasteRTF('', FolderName)
                 else
                    Editor.PastePlain(txt);
              end;
            end;

          ecPastePlain :
            if ( Clipboard.HasFormat( CF_TEXT )) then
               Editor.PastePlain('','', True);

          ecDelete : begin
            CheckToSelectImageHiddenMarkOnDelete;
            Editor.Perform( WM_CLEAR, 0, 0 );
          end;

          ecBullets :
            CmdNumbering(nsBullet);

          ecNumbers :
            CmdNumbering(KeyOptions.LastNumbering);

          ecSpace1 : begin
            Paragraph.LineSpacingRule := lsSingle;
            Paragraph.LineSpacing := 0;
          end;

          ecSpace15 : begin
            Paragraph.LineSpacingRule := lsOneAndHalf;
            Paragraph.LineSpacing := 1; // EditorOptions.LineSpcInc;
          end;

          ecSpace2 : begin
            Paragraph.LineSpacingRule := lsDouble;
            Paragraph.LineSpacing := 2; // 2*EditorOptions.LineSpcInc;
          end;

          ecSpaceBeforeInc :
            Paragraph.SpaceBefore := Paragraph.SpaceBefore + EditorOptions.ParaSpaceInc;

          ecSpaceBeforeDec :
            if ( Paragraph.SpaceBefore >= EditorOptions.ParaSpaceInc ) then
              Paragraph.SpaceBefore := Paragraph.SpaceBefore - EditorOptions.ParaSpaceInc
            else
              Paragraph.SpaceBefore := 0;

          ecSpaceAfterInc :
            Paragraph.SpaceAfter := Paragraph.SpaceAfter + EditorOptions.ParaSpaceInc;

          ecSpaceAfterDec :
            if ( Paragraph.SpaceAfter >= EditorOptions.ParaSpaceInc ) then
              Paragraph.SpaceAfter := Paragraph.SpaceAfter - EditorOptions.ParaSpaceInc
            else
              Paragraph.SpaceAfter := 0;

          ecIndent :     // Now Left Indent as in MS Word
            Paragraph.FirstIndentRelative := EditorOptions.IndentInc;

          ecOutdent :    // Now Left outdent as in MS Word
            Paragraph.FirstIndentRelative := - EditorOptions.IndentInc;

          ecFirstIndent : begin    // Now behaves as in MS Word
            Paragraph.FirstIndentRelative := EditorOptions.IndentInc;
            Paragraph.LeftIndent := Paragraph.LeftIndent - EditorOptions.IndentInc;
          end;

          ecFirstOutdent : begin
            if Paragraph.FirstIndent < EditorOptions.IndentInc then
               maxIndent:= Paragraph.FirstIndent
            else
               maxIndent:= EditorOptions.IndentInc;
            Paragraph.FirstIndentRelative := - maxIndent;
            Paragraph.LeftIndent := Paragraph.LeftIndent + maxIndent;
          end;

          ecRightIndent :
            Paragraph.RightIndent := Paragraph.RightIndent + EditorOptions.IndentInc;

          ecRightOutdent :
            if ( Paragraph.RightIndent >= EditorOptions.IndentInc ) then
              Paragraph.RightIndent := Paragraph.RightIndent - EditorOptions.IndentInc
            else
              Paragraph.RightIndent := 0;

          ecFontFormatPaste :
            if FontFormatToCopy.szFaceName <> '' then
               ApplyTextAttributes(FontFormatToCopy )
            else
               App.PopupMessage( STR_52, mtError, [mbOK] );

          ecParaFormatPaste :
            if ParaFormatToCopy.dySpaceBefore >= 0 then   // if negative, user has not yet COPIED para format
               ApplyParagraphAttributes(ParaFormatToCopy)
            else
               App.PopupMessage( STR_53, mtError, [mbOK] );

          ecPasteFormat : begin
             if (ParaFormatToCopy.dySpaceBefore >= 0) and         // paragraph formatting was saved
                ((SelLength = 0) or (ParagraphsSelected)) then
                ApplyParagraphAttributes( ParaFormatToCopy );
             ApplyTextAttributes(FontFormatToCopy );
          end;

          ecAlignLeft :
            Paragraph.Alignment := paLeftJustify;

          ecAlignCenter :
            Paragraph.Alignment := paCenter;

          ecAlignRight :
            Paragraph.Alignment := paRightJustify;

          ecAlignJustify :
            Paragraph.Alignment := paJustify;

          ecFontDlg : begin
            if RecallingCommand then begin
              FontInfoToFont( CommandRecall.Font, Form_Main.FontDlg.Font );
              SelAttributes.Assign( Form_Main.FontDlg.Font );
            end
            else begin
              var dpi: integer;
              dpi:= GetSystemPixelsPerInch;
              with Form_Main.FontDlg do begin
                 Font.Assign( SelAttributes );
                 Font.Height:= -MulDiv(Font.Size, dpi, 72);   // See comments to TaskModalDialog in gf_miscvcl
                 if Execute then begin
                   Font.Size:= -MulDiv(Font.Height, 72, dpi);
                   SelAttributes.Assign( Form_Main.FontDlg.Font );
                   FontToFontInfo( Font, CommandRecall.Font );
                 end
                 else
                   Canceled:= True;
              end;
            end;
          end;

          ecLanguage :
            if ( RecallingCommand or RunLanguageDialog ) then
              SelAttributes.Language := CommandRecall.Language
            else
              Canceled:= True;

          ecParaDlg : begin
            if ( RecallingCommand or RunParagraphDialog ) then begin
              with CommandRecall.Para do begin
                Paragraph.LineSpacingRule := SpacingRule;
                case SpacingRule of
                  lsSingle : Paragraph.LineSpacing := 0;
                  lsOneAndHalf : Paragraph.LineSpacing := 1;
                  lsDouble : Paragraph.LineSpacing := 2;
                end;
                Paragraph.SpaceBefore := SpaceBefore;
                Paragraph.SpaceAfter := SpaceAfter;
                Paragraph.Numbering := Numbering;
                Paragraph.NumberingStyle := NumberingStyle;
                Paragraph.Alignment := Alignment;
                Paragraph.FirstIndent := CommandRecall.Para.FIndent;
                Paragraph.LeftIndent := CommandRecall.Para.LIndent;
                Paragraph.RightIndent := CommandRecall.Para.RIndent;
              end;
            end
            else
              Canceled:= True;
          end;

          ecFontName :
            if RecallingCommand then
              SelAttributes.Name := CommandRecall.Font.Name
            else begin
              SelAttributes.Name := Form_Main.Combo_Font.FontName;
              CommandRecall.Font.Name := Form_Main.Combo_Font.FontName;
            end;

          ecFontSize :
            if RecallingCommand then
              SelAttributes.Size := CommandRecall.Font.Size
            else begin
              try
                SelAttributes.Size := strtoint( Form_Main.Combo_FontSize.Text );
                CommandRecall.Font.Size := SelAttributes.Size;
              except
                messagedlg( Format( STR_54, [Form_Main.Combo_FontSize.Text] ), mtError, [mbOK], 0 );
                Canceled:= True;
              end;
            end;

          ecFontSizeInc :
            SelAttributes.Size := SelAttributes.Size + EditorOptions.FontSizeInc;

          ecFontSizeDec :
            SelAttributes.Size := SelAttributes.Size - EditorOptions.FontSizeInc;

          ecDisabled :
             with SelAttributes do begin
                  Disabled := ( not Disabled );
             end;

          ecSubscript :
             with SelAttributes do begin
                if ( SubscriptStyle <> ssSubscript ) then
                   SubscriptStyle := ssSubscript
                else
                   SubscriptStyle := TSubscriptStyle(ssNone);
             end;

          ecSuperscript :
             with SelAttributes do begin
               if ( SubscriptStyle <> ssSuperscript ) then
                 SubscriptStyle := ssSuperscript
               else
                 SubscriptStyle := TSubscriptStyle(ssNone);
             end;

          ecFontColorBtn : begin
            if RecallingCommand then
              Form_Main.TB_Color.ActiveColor := CommandRecall.Font.Color;
            SelAttributes.Color := Form_Main.TB_Color.ActiveColor;
            CommandRecall.Font.Color := Form_Main.TB_Color.ActiveColor;
            KeyOptions.InitFontColor := CommandRecall.Font.Color;
          end;

          ecHighlightBtn : begin
            if RecallingCommand then
              Form_Main.TB_Hilite.ActiveColor := CommandRecall.Color;
            SelAttributes.BackColor := Form_Main.TB_Hilite.ActiveColor;
            CommandRecall.Color := Form_Main.TB_Hilite.ActiveColor;
            KeyOptions.InitHiColor := CommandRecall.Color;
          end;

          ecFontColorDlg : begin
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Font.Color
            else
              Form_Main.ColorDlg.Color := SelAttributes.Color;
            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then begin
              SelAttributes.Color := Form_Main.ColorDlg.Color;
              CommandRecall.Font.Color := Form_Main.ColorDlg.Color;
            end
            else
              Canceled:= True;
          end;

          ecHighlightDlg : begin
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Color
            else
              Form_Main.ColorDlg.Color := SelAttributes.BackColor;
            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then begin
              SelAttributes.BackColor := Form_Main.ColorDlg.Color;
              CommandRecall.Color := Form_Main.ColorDlg.Color;
            end
            else
              Canceled:= True;
          end;

          ecFontColor : begin
            SelAttributes.Color := Form_Main.TB_Color.ActiveColor;
            CommandRecall.Font.Color := Form_Main.TB_Color.ActiveColor;
          end;

          ecHighlight : begin
            SelAttributes.BackColor := Form_Main.TB_Hilite.ActiveColor;
            CommandRecall.Color := Form_Main.TB_Hilite.ActiveColor;
          end;

          ecNoHighlight :
            SelAttributes.BackColor := clWindow;

          ecBGColorDlg : begin
            ShiftWasDown := ShiftDown;
            if RecallingCommand then
              Form_Main.ColorDlg.Color := CommandRecall.Color
            else
              Form_Main.ColorDlg.Color := Editor.Color;

            if ( RecallingCommand or Form_Main.ColorDlg.Execute ) then begin
              tempChrome := Editor.Chrome;
              tempChrome.BGColor  := Form_Main.ColorDlg.Color;
              CommandRecall.Color := Form_Main.ColorDlg.Color;

              Editor.Color := tempChrome.BGColor;

              if assigned(ActiveFolder) then begin
                 ActiveFolder.Modified:= true;
                 NNode:= TreeUI.GetFocusedNNode;
                 if assigned(NNode) then
                    NNode.EditorBGColor := tempChrome.BGColor;

                 if ShiftWasDown then begin
                   if ( messagedlg( format(STR_55, [ActiveFolder.Name]),
                       mtConfirmation, [mbOK,mbCancel], 0 ) = mrOK ) then begin
                     try
                       myTreeNode := TreeUI.TV.GetFirst;
                       while assigned(myTreeNode) do begin
                          TreeUI.GetNNode(myTreeNode).EditorBGColor := tempChrome.BGColor;
                          myTreeNode := TreeUI.TV.GetNext(myTreeNode);
                       end;
                     except
                        //aCmd := ecNone;
                     end;
                   end;
                 end;  // if ShiftWasDown

              end;
            end
            else
              Canceled:= True;
          end;

          ecWordWrap : begin
             var wwActive: boolean;
             NNode:= TNoteNode(NNodeObj);
             if NNode <> nil then begin
                Folder:= TKntFolder(FolderObj);
                case NNode.WordWrap of
                   wwAsFolder :
                    if Folder.WordWrap then
                       NNode.WordWrap := wwNo
                    else
                       NNode.WordWrap := wwYes;
                   wwYes : NNode.WordWrap := wwNo;
                   wwNo  : NNode.WordWrap := wwYes;
                end;
                wwActive:= (NNode.WordWrap = wwYes);
             end
             else
                wwActive:= not Editor.WordWrap;

             WordWrap := wwActive;
             if assigned(Folder) then begin
               Form_Main.UpdateFolderDisplay;
               if (Editor.PlainText) then
                  Folder.UpdateEditor (Folder.NoteUI, false);
             end;

          end;

          ecSelectAll :
            SelectAll;

          ecUndo :
            Undo;

          ecRedo :
            Redo;

          ecClearFontAttr :
            with SelAttributes do begin
              if not assigned(ActiveFolder) then exit;

              Style := [];
              Color :=   ActiveFolder.EditorChrome.Font.Color;
              Name :=    ActiveFolder.EditorChrome.Font.Name;
              Size :=    ActiveFolder.EditorChrome.Font.Size;
              Charset := ActiveFolder.EditorChrome.Font.Charset;
              Hidden  := false;
              Disabled := false;
              SubscriptStyle := TSubscriptStyle(ssNone);
              BackColor := clWindow; // ActiveFolder.Editor.Color;
              HideKNTHiddenMarks(true);
            end;

          ecClearParaAttr :
            with Paragraph do begin
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

          ecDeleteLine :
              begin
                lineindex := perform( EM_EXLINEFROMCHAR, 0, SelStart );
                SelStart  := perform( EM_LINEINDEX, lineindex, 0 );
                SelLength := perform( EM_LINEINDEX, lineindex + 1, 0 ) - SelStart;
                SelText := '';
              end;

          ecSort :
            if ( SelLength > 1 ) then begin
               templist := TStringList.Create;
               try
                 templist.Sorted := true;
                 templist.Duplicates := dupAccept;
                 templist.Text := SelText;
                 SelText := templist.Text;
                 HideKNTHiddenMarks(true);
               finally
                 templist.Free;
               end;
            end
            else
              ErrNoTextSelected:= True;

          ecReformat :
            ErrNotImplemented:= True;

          ecJoinLines :
            if ( SelLength > 1 ) then begin
              screen.cursor := crHourGlass;
              try
                txt := SelText;
                CharToChar( txt, #13, #32 );
                p := pos( #32#32, txt );
                while ( p > 0 ) do begin
                  delete( txt, p, 1 );
                  p := pos( #32#32, txt );
                end;
                SelText := txt;
                HideKNTHiddenMarks(true);
              finally
                screen.cursor := crDefault;
              end;
            end
            else
              ErrNoTextSelected:= True;

          ecReverseText : begin
            if ( SelLength > 0 ) then begin
              txt := SelText;
              txt:= KeepOnlyLeadingKNTHiddenCharacters(txt);

              p:= length( txt );
              txt2 := '';
              SetLength( txt2, p );
              for i := p downto 1 do
                txt2[(p-i)+1] := txt[i];
              SelText := txt2;
            end
            else
              ErrNoTextSelected:= True;
          end;

          ecROT13 :
            if ( SelLength > 0 ) then begin
              screen.cursor := crHourGlass;
              try
                txt := SelText;
                txt:= KeepOnlyLeadingKNTHiddenCharacters(txt);

                for i := 1 to length( txt ) do begin
                  p := pos( txt[i], alph13 );
                  if ( p > 0 ) then
                    txt[i] := Char(alph13[p+13])
                  else begin
                    p := pos( txt[i], alph13UP );
                    if ( p > 0 ) then
                       txt[i] := Char(alph13UP[p+13]);
                  end;
                end;
                SelText := txt;
              finally
                screen.cursor := crDefault;
              end;
            end
            else
              ErrNoTextSelected:= True;


          ecInvertCase :
            if ( SelLength > 0 ) then begin
               txt := SelText;
               for i := 1 to length( txt ) do begin
                 if IsCharUpper( txt[i] ) then
                    txt2 := AnsiLowercase( txt[i] )
                 else
                    txt2 := AnsiUpperCase( txt[i] );
                 txt[i] := txt2[1];
               end;
               SelText := txt;
               HideKNTHiddenMarks(true);
            end
            else
               ErrNoTextSelected:= True;

          ecToUpperCase :
            if ( SelLength > 0 ) then begin
                txt := SelText;
                txt := AnsiUpperCase( txt );
                SelText := txt;
                HideKNTHiddenMarks(true);
            end
            else
              ErrNoTextSelected:= True;

          ecToLowerCase :
            if ( SelLength > 0 ) then begin
                txt := SelText;
                txt := AnsiLowercase( txt );
                SelText := txt;
                HideKNTHiddenMarks(true);
            end
            else
               ErrNoTextSelected:= True;

          ecCycleCase :
            if ( SelLength > 0 ) then begin
               txt := SelText;

               LAST_CASE_CYCLE := GetLetterCase( txt );
               if ( LAST_CASE_CYCLE = high( LAST_CASE_CYCLE )) then
                 LAST_CASE_CYCLE := low( LAST_CASE_CYCLE )
               else
                 inc( LAST_CASE_CYCLE );

               case LAST_CASE_CYCLE of
                 ccLower : txt := AnsiLowercase( txt );
                 ccMixed : txt := AnsiProperCase( txt, [#8..#32,',','-','.','?','!',';'] );
                 ccUpper : txt := AnsiUpperCase( txt );
               end;

               SelText := txt;
               HideKNTHiddenMarks(true);
            end
            else
               ErrNoTextSelected:= True;

          ecToMixedCase :
            if ( SelLength > 0 ) then begin
               txt := SelText;
               txt := AnsiProperCase( txt, [#8..#32,',','-','.','?','!',';'] );
               SelText := txt;
               HideKNTHiddenMarks(true);
            end
            else
               ErrNoTextSelected:= True;

          ecInsDate : begin
            if SupportsRegisteredImages then begin
               if SelLength > 0 then
                  CheckToSelectLeftImageHiddenMark;
            end;

            if KeyOptions.DTUseLastSelection then begin
               if ( KeyOptions.DTLastDateFmt = '' ) then
                  KeyOptions.DTLastDateFmt := KeyOptions.DateFmt;
               if ( KeyOptions.DTLastDateFmt = '' ) then
                  KeyOptions.DTLastDateFmt := FormatSettings.ShortDateFormat;
               SelText := GetDateTimeFormatted( KeyOptions.DTLastDateFmt, now ) + #32;
            end
            else
               SelText := FormatDateTime( KeyOptions.DateFmt, now ) + #32;
            SelStart := SelStart + SelLength;
          end;

          ecInsTime : begin
            if SupportsRegisteredImages then begin
              if SelLength > 0 then
                 CheckToSelectLeftImageHiddenMark;
            end;

            if KeyOptions.DTUseLastSelection then begin
              if ( KeyOptions.DTLastTimeFmt = '' ) then
                 KeyOptions.DTLastTimeFmt := KeyOptions.TimeFmt;
              if ( KeyOptions.DTLastTimeFmt = '' ) then
                 KeyOptions.DTLastTimeFmt := FormatSettings.LongTimeFormat;
              SelText := GetDateTimeFormatted( KeyOptions.DTLastTimeFmt, now ) + #32;
            end
            else
              SelText := FormatDateTime( KeyOptions.TimeFmt, now ) + #32;
            SelStart := SelStart + SelLength;
          end;

          ecExpandTerm :
            ExpandTermProc;

          ecEvaluateExpression :
            EvaluateExpression;

          else begin
            // other commands, which can be handled here
            // ONLY when RecallingCommand (e.g. from macros)
            if RecallingCommand then begin
              case aCmd of
                 ecInsCharacter, ecInsCharacterU :
                   with CommandRecall.CharInfo do
                      TKntRichEdit.CharInsertProc(Chr(code), Count, Name, Charset, (aCmd=ecInsCharacterU) );

                 ecStyleApply :
                    StyleApply( CommandRecall.StyleName );

                 else
                    ErrNotImplemented:= true; // we cannot handle this comand here
              end;
            end;
          end;   // else - Other commands

        end;

        if ErrNoTextSelected or Canceled or ErrNotImplemented then begin
           // Canceled: dialog was canceled, restore previous (possibly repeatable) command
           if ErrNoTextSelected then
              App.WarnNoTextSelected
           else if ErrNotImplemented then
              App.WarnCommandNotImplemented(EDITCMD_NAMES[aCMD]);

           aCmd := ecNone;
        end
        else begin
           UpdateLastCommand( aCMD );
           if IsRecordingMacro then
              AddMacroEditCommand( aCMD );
        end;


      except
        on E : Exception do begin
          App.PopupMessage( STR_51 + #13 + E.Message, mtError, [mbOK] );
         {$IFDEF KNT_DEBUG}
          Log.Add( 'Exception in PerformCmd (' + inttostr( ord( aCMD )) + '): ' + E.Message );
         {$ENDIF}
        end;
      end;

    finally
      Editor.EndUpdate;

      if ( aCmd <> ecNone ) then begin
        Editor.Change;
        Editor.ChangedSelection;
        if (aCmd = ecWordWrap) and assigned(Editor.NNodeObj) then
           App.ChangeInEditor(Editor);

        if (CopyFormatMode = cfEnabledMulti) and (aCmd = ecPasteFormat)  then
           EnableCopyFormat(True);
      end;

    end;
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

  with Form_Main do begin
      MMEditRepeat.Enabled := ( LastEditCmd <> ecNone );
      RTFMRepeatCmd.Enabled := MMEditRepeat.Enabled;
      TB_Repeat.Enabled := MMEditRepeat.Enabled;

      MMEditRepeat.Caption := Format( STR_56, [EDITCMD_NAMES[LastEditCmd]] );
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
    Form_Lang.CurrentLang := ActiveEditor.SelAttributes.Language;
    Form_Lang.Combo_Lang.Language := Form_Lang.CurrentLang;
    Form_Lang.RecentLang := KeyOptions.RecentLanguage;
    Form_Lang.DefaultLang := DefaultEditorChrome.Language;

    if ( Form_Lang.ShowModal = mrOK ) then begin
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
  Editor: TKntRichEdit;
begin
  result := false;

  Editor:= ActiveEditor;

  with CommandRecall.Para do begin
    case Editor.Paragraph.LineSpacing of
      0 : SpacingRule := lsSingle;
      1 : SpacingRule := lsOneAndHalf;
      else
        SpacingRule := lsDouble;
    end;
    LIndent := Editor.Paragraph.LeftIndent;
    RIndent := Editor.Paragraph.RightIndent;
    FIndent := Editor.Paragraph.FirstIndent;
    SpaceBefore := Editor.Paragraph.SpaceBefore;
    SpaceAfter := Editor.Paragraph.SpaceAfter;
    Numbering := Editor.Paragraph.Numbering;
    NumberingStyle := Editor.Paragraph.NumberingStyle;
    Alignment := Editor.Paragraph.Alignment;
  end;

  Form_Para := TForm_Para.Create( Form_Main );
  try
    Form_Para.Para := CommandRecall.Para;
    Form_Para.CurrentNumbering := Editor.Paragraph.Numbering;
    Form_Para.CurrentNumberingStyle := Editor.Paragraph.NumberingStyle;
    if ( Form_Para.CurrentNumbering in [nsNone, nsBullet] ) then
      Form_Para.CurrentNumbering := KeyOptions.LastNumbering;

    if ( Form_Para.CurrentNumbering = nsNone ) then
      Form_Para.CurrentNumberingStyle := KeyOptions.LastNumberingStyle;

    with Form_Para do begin
      Spin_First.Increment := EditorOptions.IndentInc;
      Spin_Left.Increment := EditorOptions.IndentInc;
      Spin_Right.Increment := EditorOptions.IndentInc;
      Spin_SpcBef.Increment := EditorOptions.ParaSpaceInc;
      Spin_SpcAft.Increment := EditorOptions.ParaSpaceInc;
    end;

    if ( Form_Para.ShowModal = mrOK ) then begin
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



function PerformOtherCommandShortcut( const Key : Word; const Shift : TShiftState ): boolean;
var
  i: integer;
  kOC: TKeyOtherCommandItem;
  Category: TOtherCommandCategory;
  Command: string;
  aShortcut: TShortCut;

begin
  Result:= false;
  aShortCut:= ShortCut(Key, Shift);

  for i:= 0 to App.Kbd.OtherCommandsKeys.Count-1 do begin
     kOC:= App.Kbd.OtherCommandsKeys[i];
     if (kOC.Shortcut = aShortcut) then begin
        Category:= kOC.Category;
        Command:= kOC.Name;
        Result:= true;
        break;
     end;
  end;

  if not Result then exit;

  if CopyFormatMode <> cfDisabled then
     EnableCopyFormat(False);

  case Category of
    ccMacro    : ExecuteMacro('', Command);
    ccPlugin   : ExecutePlugin(Command);
    ccTemplate : InsertTemplate(Command);
    ccStyle    : StyleApply(Command);
    ccFont : begin
       RecallingCommand := true;
       CommandRecall.Font.Name := Command;
       PerformCmd( ecFontName );
    end;
  end;

  Result:= true;
end; // PerformOtherCommandShortcut


procedure ExecuteMacroFile;
var
  fn, oldFilter : string;
begin

  if ( not CheckFolder( 'Macro', Macro_Folder, false, true )) then exit;

  if CopyFormatMode <> cfDisabled then
     EnableCopyFormat(False);

  with Form_Main.OpenDlg do begin
    oldFilter := Filter;
    Filter := FILTER_MACROS;
    FilterIndex := 1;
    Title := STR_57;
    Options := Options - [ofAllowMultiSelect];
    InitialDir := Macro_Folder;
    FileName := LastMacroFN;
  end;

  try
    if Form_Main.OpenDlg.Execute then begin
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
    Form_Main.MacMMacroUserCommand.Enabled := false;


    LastMacroFN := '';

    LastEditCmd := ecNone;
    UpdateLastCommand( ecNone );
    RecallingCommand := false;

    FontFormatToCopy.szFaceName:= '';

    ParaFormatToCopy.dySpaceBefore := -1; // MARKER: signals a not-yet-assigned format

    CopyFormatMode:= cfDisabled;

    with CommandRecall do begin
      Color := clWindow;
      GoToIdx := '';
      StyleName := '';
      with Font do begin  // last font properties applied, for repeating last command
        Charset := DEFAULT_CHARSET;
        Color := clBlack;
        Name := 'Tahoma';
        Size := 10;
        Style := [];
      end;
      with Para do begin  // last paragraph properties applied
        SpacingRule := lsSingle;
        LIndent := 0;
        RIndent := 0;
        FIndent := 0;
        SpaceBefore := 0;
        SpaceAfter := 0;
        Numbering := nsNone;
        Alignment := paLeftJustify;
      end;
      with CharInfo do begin // last Insert Character arguments
        Code := 0;
        Name := '';
        Count := 0;
        Charset := DEFAULT_CHARSET;
      end;
    end;

    LoadMacroList (false);
end;


Function CmdPaste(const fromButton: boolean; const ForcePlain: boolean): boolean;
var
   executed: Boolean;
begin
    executed:= False;
    if assigned(ActiveEditor) and ActiveEditor.Focused then begin
       executed:= true;
       if fromButton then begin
           if CtrlDown then
             PasteIntoNew( true )
           else
           if AltDown then
             Form_Main.MMEditPasteSpecialClick( nil )
           else
           if ShiftDown or ActiveEditor.PlainText then
             PerformCmd( ecPastePlain )
           else
             PerformCmd( ecPaste );
       end
       else
          if ForcePlain or not ActiveEditor.SupportsImages then
             PerformCmd( ecPastePlain )
          else
             PerformCmd( ecPaste );
    end
    else begin
        if not assigned(ActiveTreeUI) then exit;
        if ActiveTreeUI.IsEditing then begin
           if ActiveTreeUI.CheckReadOnly then
              executed:= true;
        end
        else if ActiveTreeUI.Focused then begin
           executed:= true;
           ActiveTreeUI.TreeTransferProc(ttPaste, KeyOptions.ConfirmTreePaste, ForcePlain);
        end;
    end;

    Result:= Executed;
end;


Function CmdCopy: boolean;
var
   executed: Boolean;
begin
    executed:= False;
    if assigned(ActiveEditor) and ActiveEditor.Focused then begin
       executed:= true;
       PerformCmdEx(ecCopy);
    end
    else begin
        if not assigned(ActiveTreeUI) then exit;
        if ActiveTreeUI.IsEditing then begin
           executed:= false;       // will be managed by the Tree component
        end
        else if ActiveTreeUI.Focused then begin
           executed:= true;
           ActiveFolder.TreeUI.TreeTransferProc(ttCopy, KeyOptions.ConfirmTreePaste, false);  // Lift Subtree
        end;
    end;
    Result:= Executed;
end;


Function CmdCut: boolean;
var
   executed: Boolean;
begin
    executed:= False;
    if assigned(ActiveEditor) and ActiveEditor.Focused then begin
       executed:= true;
       PerformCmd(ecCut);
    end
    else begin
        if not assigned(ActiveTreeUI) then exit;
        if ActiveTreeUI.IsEditing then begin
           executed:= false;     // will be managed by the Tree component
        end
        else if ActiveTreeUI.Focused then begin
             ActiveTreeUI.TreeTransferProc(ttCut, KeyOptions.ConfirmTreePaste, false);  // Copy Subtree for moving
             executed:= true;
        end;
    end;
    Result:= Executed;
end;

Initialization
    StartupMacroFile := '';
    StartupPluginFile := '';

end.
