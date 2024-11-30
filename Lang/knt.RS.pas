unit knt.RS;

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


resourcestring

  STR_minute = 'minute';
  STR_minutes = 'minutes';
  STR_hour = 'hour';
  STR_hours = 'hours';
  STR_day = 'day';
  STR_days = 'days';
  STR_week = 'week';
  STR_weeks = 'weeks';
  STR_ERR_OUTOFRESOURCES = 'The operating system is out of memory or resources.';
  STR_ERROR_FILE_NOT_FOUND = 'The specified file was not found.';
  STR_ERROR_PATH_NOT_FOUND = 'The specified path was not found.';
  STR_ERROR_BAD_FORMAT = 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  STR_SE_ERR_ACCESSDENIED = 'The operating system denied access to the specified URL.';
  STR_SE_ERR_ASSOCINCOMPLETE = 'The filename association is incomplete or invalid.';
  STR_SE_ERR_DDEBUSY = 'The DDE transaction could not be completed because other DDE transactions were being processed.';
  STR_SE_ERR_DDEFAIL = 'The DDE transaction failed.';
  STR_SE_ERR_DDETIMEOUT = 'The DDE transaction could not be completed because the request timed out.';
  STR_SE_ERR_DLLNOTFOUND = 'The specified dynamic-link library was not found.';
  STR_SE_ERR_NOASSOC = 'There is no application associated with the given filename extension.';
  STR_SE_ERR_OOM = 'There was not enough memory to complete the operation.';
  STR_SE_ERR_SHARE = 'A sharing violation occurred';
  STR_UNKNOWN_ERROR = 'Unknown error.';


  // kn_about.pas ======================================================================
  sAB_ = 'kn_about.pas';
  sAB00 = 'About - ';
  sAB01 = 'Double-click to send email; Right-click to copy' + #13 + '(No HTML-formatted email, PLEASE!)';
  sAB02 = 'Double-click to visit home page; Right-click to copy';
  sAB03 = 'Keynote was inspired by a fantastic freeware prog: DaRT Notes' + #13 + 'by Andre v.d. Merwe (See "dart.txt" for information)';
  sAB04 = 'KeyNote NF is an evolution of KeyNote (by Marek)';
  Program_Desc = 'Tabbed notebook for Windows';


  // kn_UpdateVersion.pas ========================================================================
  sUpd01 = 'You already have the latest version installed';
  sUpd02 = 'There is a new version !';
  sUpd03 = 'No Internet access';


  // kn_Const.pas ========================================================================
  FILTER_ALLFILES    = 'All files (*.*)|*.*';
  FILTER_EXECUTABLES = 'Programs|*.exe;*.com';
  LANGUAGE_DEFAULT = 'English (Internal)';
  sINFUrlAct1 = 'Open';
  sINFUrlAct2 = 'Open in new window';
  sINFUrlAct3 = 'Copy to clipboard';
  sINFUrlAct4 = 'Both (open and copy)';
  sINFUrlAct5 = 'Prompt';
  sINFUrlAct6 = 'Do nothing';
  sINFUrlAct7 = 'Create or Modify';
  sINFDIR1 = 'Up';
  sINFDIR2 = 'Down';
  sINFDIR3 = 'Left';
  sINFDIR4 = 'Right';
  sINFPOS1 = 'Top';
  sINFPOS2 = 'Bottom';
  sINFPOS3 = 'Left';
  sINFPOS4 = 'Right';
  sINFDefaults1 = 'New folder';
  sINFDefaults2 = 'New node';
  sINFFormats1 = 'Keynote native';
  sINFFormats2 = 'Keynote encrypted';
  sINFFormats3 = 'Keynote compressed';
{$IFDEF WITH_DART}
  sINFFormats4 = 'Dart Notes';
{$ENDIF}
  sINFSrchMode1 = 'Exact phrase';
  sINFSrchMode2 = 'All the words';
  sINFSrchMode3 = 'Any of the words';
  sINFSrchScope1 = 'Only node names';
  sINFSrchScope2 = 'Only note contents';
  sINFSrchScope3 = 'All';
  sINFSrchChk1 = 'Only non checked nodes';
  sINFSrchChk2 = 'Only checked nodes';
  sINFSrchChk3 = 'All';
  sINFTreeSel1 = 'Current node';
  sINFTreeSel2 = 'Current node and subtree';
  sINFTreeSel3 = 'Checked nodes';
  sINFTreeSel4 = 'Full tree';
  sINFExptFrmt1 = 'Plain text';
  sINFExptFrmt2 = 'Rich text (RTF)';
  sINFExptFrmt3 = 'KeyNote File (knt)';
  sINFIconKind1 = 'None';
  sINFIconKind2 = 'Standard icons';
  sINFIconKind3 = 'Custom icons';
  sINFLinkType1 = 'Internet address';
  sINFLinkType2 = 'Email address';
  sINFLinkType3 = 'File or folder';
  sINFLinkType4 = 'KeyNote location';
  sINFExpnd1 = 'Show tree fully collapsed';
  sINFExpnd2 = 'Expand only last active node';
  sINFExpnd3 = 'Expand only top level nodes';
  sINFExpnd4 = 'Restore expanded state of all nodes';
  sINFExpnd5 = 'Show tree fully expanded';
  sINFClipNdNam1 = 'Default node name';
  sINFClipNdNam2 = 'Use clipboard text';
  sINFClipNdNam3 = 'Use current date and time';
  sINFDrop1 = 'Open file in KeyNote';
  sINFDrop2 = 'Execute (macro or plugin)';
  sINFDrop3 = 'Merge folders into current file';
  sINFDrop4 = 'Import as a new folder';
  sINFDrop5 = 'Create hyperlink to file';
  sINFDrop6 = 'Import as tree nodes';
  sINFDrop7 = 'Import as virtual tree nodes';
 {$IFDEF WITH_IE}
  sINFDrop8 = 'Import as Internet Explorer virtual node';
 {$ENDIF}
  sINFDrop9 = 'Insert content at caret position';
  sINFImpHTML1 = 'No conversion (HTML source)';
  sINFImpHTML2 = 'Use Shared HTML Text Converter (html32.cnv + msconv97.dll)';  // HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Shared Tools\Text Converters
  sINFImpHTML3 = 'Use MS Word Converter';
  sINFImpHTML4 = 'Use Internet Explorer';
  sINFImpHTML5 = 'Use Microsoft HTML Converter (html.iec)';
  sINFSymb0 = 'Euro';
  sINFSymb1 = 'Copyright';
  sINFSymb2 = 'Registered trademark';
  sINFSymb3 = 'Trademark';
  sINFSymb4 = 'Paragraph';
  sINFSymb5 = 'Degree';
  sINFSymb6 = 'Plus/minus';
  sINFSymb7 = 'Dots';
  sINFSymb8 = 'French parenthesis (left)';
  sINFSymb9 = 'French parenthesis (right)';
  sINFCompres1 = 'None';
  sINFCompres2 = 'Fastest';
  sINFCompres3 = 'Default';
  sINFCompres4 = 'Max';
  sINFClipPlainTxt1 = 'Plain (without any formatting)';
  sINFClipPlainTxt2 = 'Only hyperlinks (without other formatting)';
  sINFClipPlainTxt3 = 'Only font style (bold, italic, ...)';
  sINFClipPlainTxt4 = 'Only font (without paragraph formatting)';
  sINFImgSM1 = 'Embedded RTF';
  sINFImgSM2 = 'Embedded KNT';
  sINFImgSM3 = 'External (Folder or Zip)';
  sINFImgSM4 = 'External + Embedded KNT';
  sINFImgSM5 = 'No export images';
  sINFImgExtSt1 = 'Folder';
  sINFImgExtSt2 = 'ZIP';
  sINFCtrlUD1 = 'Moves cursor to prev or next paragraph';
  sINFCtrlUD2 = 'Shift view one line up or down';
  sINFCtrlUD3 = 'Smoothly moves scroll bar vertically';


  // kn_Defaults.pas ======================================================================
  sDef_ = 'kn_Defaults.pas';
  sDef00 = 'OK';
  sDef0B = 'Accept changes and close dialog box';
  sDef01 = 'Folder Properties: %s';
  sDef02 = 'Close';
  sDef03 = 'Folder is Read-Only: cannot change properties';
  sDef04 = ' [RO]';
  sDef05 = ' View properties for current folder ';
  sDef06 = 'Change properties for current folder';
  sDef07 = '&Save as default for "%s"';
  sDef08 = 'Defaults for ';
  sDef09 = 'Change Defaults for NEW folders in THIS FILE';
  sDef10 = 'Defaults for all files';
  sDef11 = 'Change default properties for all NEW folders';
  sDef12 = 'Folder name cannot be blank. Please enter a name.';
  sDef13 = 'Folder name cannot contain the "%s" character';
  sDef14 = 'Node name cannot contain the "%s" character';
  sDef15 = 'OK to reset Editor font and color settings to default values?';
  sDef16 = 'OK to reset Tree font and color settings to default values?';
  sDef17 = 'Tokens for autonaming tree nodes:';
  sDef18 = '(must be UPPERCASE)';
  sDef19 = ' = current date';
  sDef20 = ' = current time';
  sDef21 = ' = total number of nodes';
  sDef22 = ' = new node''s level';
  sDef23 = ' = new node''s index';
  sDef24 = ' = new node''s absolute index';
  sDef25 = ' = parent node''s name';
  sDef26 = ' = name of active folder';
  sDef27 = ' = name of currently open file';
  sDef28 = '<no icon>';
  sDef29 = 'Invalid zoom ratio: ';
  sDef30 = ' (and apply to "%s" folder)';

  // kn_LanguagesMng.pas ========================================================================
  sLng01 = 'Internal Language (English) will be established next time you start KeyNote NF';
  sLng02 = 'Language file not found: ';
  sLng03 = 'Tip file not found: ';
  sLng04 = 'Applying Language file  "';


  // kn_OptionsNew.pas ======================================================================
  sOpt_ = 'kn_OptionsNew.pas';
  sOptS00 = 'General Settings';
  sOptS01 = 'Rich Text Editor';
  sOptS02 = 'Images';
  sOptS03 = 'Tree Panel';
  sOptS04 = 'KeyNote Files';
  sOptS05 = 'File Options';
  sOptS06 = 'Backup Options';
  sOptS07 = 'Actions';
  sOptS08 = 'Confirmations';
  sOptS09 = 'Chrome';
  sOptS10 = 'Tab Icons';
  sOptS11 = 'Advanced';
  sOptS12 = 'Formats';
  sOptS13 = 'Clipboard';
  sOptS14 = 'File Types';
  sOptS15 = 'Other';
  //
  sOpt01 = ' Custom icons are DISABLED ';
  sOpt02 = 'Maximum size for clipboard capture text is not a valid integer value.';
  sOpt03 = '(invalid date format)';
  sOpt04 = '(invalid time format)';
  sOpt05 = 'OK to reset tab fonts and colors to default state?';
  sOpt06 = ' icon %d ';
  sOpt07 = 'Icons: %d';
  sOpt08 = 'Failed to get icon from ';
  sOpt09 = 'Failed to get bitmap from "%s"';
  sOpt10 = 'Cannot delete this icon: List must contain at least 1 icon.';
  sOpt11 = 'OK to delete the selected icon?';
  sOpt12 = 'Failed to delete icon: ';
  sOpt13 = 'OK to restore factory default icons?';
  sOpt14 = 'Divider string can contain the following tokens:' +#13+
    '(CASE SENSITIVE)' +#13#13+
     '%s = current date' +#13+
     '%s = current time' +#13+
     '%s = replaced with one line break' +#13+
     '%s = encloses what to show only if source is included' +#13+
     '%s = source URL (with title)' +#13+
     '%s = source URL (with title, limited waiting time (*))' +#13+
     '%s = source URL (without title)'  +#13+
     '%s = source server/domain (e.g.: "[YouTube]")'  +#13+
     '%s = delimits divider for 2nd+ (same URL)'  +#13#13+
     'Remember: Source tokens will be ignored if source is not shown. Also, with %%|' +
     ' you can vary the effective divider depending on whether source URL is shown or not'
     ;
   {'(*) If title is not figured out in less that 2 seconds (normally will be much faster) only URL will be used' +#13+
     '    A small cache will be used for titles, so even certain delays will only apply to the first capture on a URL.' }
  sOpt15 = 'The Auto-Close function will work ONLY if Auto-Save is turned ON, and if no dialog box is open at the time KeyNote tries to automatically close the file. (Auto-Save is currently DISABLED.)';
  sOpt16 = 'Error in TVChange: PageIndex %d  Node.AbsIdx %d';


  // GFTipDlg.pas ========================================================================
  sTip01  = 'Tip of the day';
  sTip02  ='Did you know...';
  sTip03  = '(Tips not found.)';


  // kn_DLLmng.pas ========================================================================
  sDll01 = 'Error while attempting to load runtime library "%s". Please reinstall KeyNote.';
  sDll02 = 'Procedure "%s" not found in runtime library "%s". Please reinstall KeyNote.';

  // kn_INI.pas ==========================================
  {$IFNDEF EXCLUDEEMAIL}
  sINIMail = 'Attached file: %F';
  {$ENDIF}

  // kn_URL.pas ==========================================
  sURL01 = 'OK';
  sURL02 = 'Create Hyperlink';
  sURL03 = 'Modify';
  sURL04 = 'Choose Action for Hyperlink';

  // kn_pass.pas ========================================================================
  sPass01 = 'Passphrase cannot be blank.';
  sPass02 = 'File "%s" is encrypted';

  // kn_PluginBase.pas, knt_Plugins.pas ========================================================================
  sPlg_ = 'kn_PluginBase.pas';
  sPlg01 = 'StatusOK';
  sPlg02 = 'Gets data';
  sPlg03 = 'Gets RTF';
  sPlg04 = 'Gets selection';
  sPlg05 = 'Returns data';
  sPlg06 = 'Returns RTF';
  sPlg07 = 'Returns clipboard';
  sPlg08 = 'Needs selection';
  sPlg09 = 'Wants new note';
  sPlg10 = 'Wants dialog box';
  sPlg11 = 'Wants saved file';
  sPlg12 = 'Reload file';
  sPlg13 = 'Stays resident';
  sPlg14 = 'Unexpected error from DLL: ';

  // kn_PluginsMng.pas.pas ========================================================================
  sPlgM_ = 'kn_PluginsMng.pas';
  sPlgM01 = 'Select plugin to display information';
  sPlgM02 = '(version %d)';
  sPlgM03 = 'Filename: ';
  sPlgM04 = 'No plugins available.';
  sPlgM05 = 'Could not execute plugin ';
  sPlgM06 = 'No plugins available or none selected.';
  sPlgM07 = 'Cannot execute plugin - file not found. (%s)';
  sPlgM08 = 'Execute most recent plugin "%s"';
  sPlgM09 = 'Could not obtain plugin information from "%s". Make sure that the file exists.';
  sPlgM10 = 'Plugin "%s" (%s) reports wrong version number %d. A newer version of KeyNote is required to run this plugin.';
  sPlgM11 = 'Plugin "%s" (%s) refuses to execute.';
  sPlgM12 = 'Could not load plugin "%s" (%s).';
  sPlgM13 = 'Could not execute plugin "%s" (%s).';
  sPlgM14 = 'Resident plugin "%s" is already running. Shut down the running plugin first.';
  sPlgM15 = 'Plugin "%s" tried to go resident, but attempt to set resident plugin ID failed. Plugin will be shut down.';
  sPlgM16 = 'Plugin "%s" requires that text is selected in active note. Select some text and try again.';
  sPlgM17 = 'Plugin returned error code "%d"';
  sPlgM18 = 'Plugin "%s" is now running';
  sPlgM19 = 'Plugin copied data to clipboard. Ok to paste into active note?';
  sPlgM20 = 'Active note "%s" is Read-only. Inserting plugin output will modify the note. Insert anyway?';
  sPlgM21 = 'Unexpected error during plugin cleanup: ';
  sPlgM22 = 'Unexpected plugin error: ';
  sPlgM23 = 'Resident plugin "%s" unloaded';
  sPlgM24 = 'Unexpected error while shutting down resident plugin "%s": %s';


  // kn_Macro.pas ========================================================================
  sMac_  = 'kn_Macro.pas';
  sMac01 = 'Invalid macro header';
  sMac02 = 'Invalid macro version information';
  sMac03 = 'Error while loading macro "%s": %s' + #13#13 + 'Continue loading macros?';
  sMac04 = 'Unexpected error while loading macro "%s": %s';

  // kn_MacroEdit.pas ========================================================================
  sMacE01 = 'New macro';
  sMacE02 = 'Macro name cannot be blank.';
  sMacE03 = 'Another macro with this name already exists. Macro names must be unique.';

  // kn_MacroCmd.pas ========================================================================
  sMacC_ = 'kn_MacroCmd.pas';
  sMacC01 = 'string';
  sMacCms = 'miliseconds';
  sMacCfn = 'filename';
  sMacCint = 'integer';
  sMacCcn = 'color name';
  sMacCstN = 'style name';
  sMacC10 = 'Inserts a line of text into active note';
  sMacC11 = 'Pauses for a specified period (miliseconds)';
  sMacC12 = 'Rewinds macro to beginning and starts again (macro will run in infinite loop until ESC key pressed)';
  sMacC13 = 'Displays a dialog box with a message text';
  sMacC14 = 'Displays message in status bar';
  sMacC15 = 'Executes the specified plugin';
  sMacC16 = 'Executes the specified macro';
  sMacC17 = 'Displays an OK / CANCEL dialog box and aborts macro if CANCEL is pressed';
  sMacC18 = 'Turns ON specified font style';
  sMacC19 = 'Turns OFF specified font style';
  sMacC20 = 'Toggles specified font style';
  sMacC21 = 'Moves caret left';
  sMacC22 = 'Moves caret right';
  sMacC23 = 'Moves caret down';
  sMacC24 = 'Moves caret up';
  sMacC25 = 'Selects all text in active note';
  sMacC26 = 'Selects new font color';
  sMacC27 = 'Selects new background color';
  sMacC28 = 'Selects new highlight color';
  sMacC29 = 'Creates a new standard RTF note';
  sMacC30 = 'Creates a new tree-type note';
  sMacC31 = 'Applies named style to text';
  sMacC32 = 'Sets a new bookmark';
  sMacC33 = 'Jumps to previously set bookmark';


  // kn_MacroMng.pas ========================================================================
  sMacM_ = 'kn_MacroMng.pas';
  sMacM01 = 'Stop recording macro';
  sMacM02 = '&Stop Recording Macro';
  sMacM03 = ' Recording macro "%s"';
  sMacM04 = 'Command "%s" cannot be included in macros. This command has been executed but will not be recorded in the macro.';
  sMacM05 = 'You have executed a command which opens a dialog box. KeyNote can ' +
      'remember the values you have just selected and use them when ' +
      'replaying the macro, OR KeyNote can display the dialog box again to let you select values ' +
      'each time the macro is executed. Do you want KeyNote to remember current values?' + #13#13 +
      'Click YES to save current values. Click NO to always open the dialog box. Click CANCEL to ' +
      'skip this command and continue recording macro.';
  sMacM06 = ' Macro recording PAUSED';
  sMacM07 = ' Macro recording RESUMED';
  sMacM08 = 'Macro "%s" contains no commands and will be discarded.';
  sMacM09 = 'Save new macro "%s" (%d lines)?';
  sMacM10 = 'Error saving macro "%s": %s';
  sMacM11 = ' Macro recorded and saved.';
  sMacM12 = ' Macro discarded.';
  sMacM13 = ' Macro error';
  sMacM14 = 'Error adding new macro "%s": %s';
  sMacM15 = 'Record a new macro';
  sMacM16 = '&Record Macro...';
  sMacM17 = 'Active folder "%s" is Read-only. Running the macro may cause the folder to be modified. Do you want the macro to run anyway?';
  sMacM18 = 'Cannot load macro file "%s". Reason: %s';
  sMacM19 = 'Running macro "%s"';
  sMacM20 = 'Execute most recent macro "%s"';
  sMacM21 = 'Error loading macro "%s": %s';
  sMacM22 = 'Cannot execute macro "%s": This macro requires a newer version of KeyNote.';
  sMacM23 = ' Unexpected error while executing macro';
  sMacM24 = ' Macro was aborted by user';
  sMacM25 = ' Macro done';
  sMacM26 = 'OK to delete selected macro "%s"?';
  sMacM27 = 'Cannot delete macro file "%s"';
  sMacM28 = 'Error while deleting macro: ';
  sMacM29 = 'Macro aborted on line %d: "%s"' + #13 + 'Reason: %s';
  sMacM30 = 'unknown command';
  sMacM31 = 'syntax error';
  sMacM32 = 'unknown user command';
  sMacM33 = 'string argument required';
  sMacM34 = 'integer argument required';
  sMacM35 = 'Cannot run embedded macro "%s". Reason: %s';
  sMacM36 = 'Folder creation failed';
  sMacM37 = 'Invalid font style argument';
  sMacM38 = 'Unexpected error while executing macro: %s' + #13#13 +
          'Last macro line was: "%s" (line %d)';
  sMacM39 = 'This command cannot be executed while macro is being recorded or replayed.';
  sMacM40 = 'Macro "%s" not found.';
  sMacM41 = 'No macros available or none selected.';
  sMacM42 = 'Could not access current macro.';
  sMacM43 = ' This command cannot be repeated';
  sMacM44 = 'This action cannot be performed, because there is no active folder (%d)';
  sMacM45 = 'This folder cannot be set as Read-only, because it is being used for clipboard capture.';
  sMacM46 = 'Failed to assign font attributes.';
  sMacM47 = 'Failed to assign paragraph attributes.';
  sMacM48 = 'Go to line';
  sMacM49 = 'Enter line number or increment (+-):';
  sMacM51 = 'Cannot perform command:';
  sMacM52 = 'No font attributes to paste from: Use "Copy font attributes" first.';
  sMacM53 = 'No paragraph attributes to paste from: Use "Copy paragraph attributes" first.';
  sMacM54 = '"%s" is not a valid number';
  sMacM55 = 'New background color will be assigned to ALL TREE NODES in folder %s' + #13 + 'Continue?';
  sMacM56 = 'Repeat %s';
  sMacM57 = 'Select macro to execute';
  sMacM58 = 'Failed to copy text formatting';


  // kn_StyleObj.pas ========================================================================
  sSty01 = 'Face: %s' + #13 +
            'Size: %s'  + #13 +
            'Style: %s' + #13 +
            'Color: %s' + #13 +
            'Other: %s';
  sSty02 = ' %s, %s space, %s, L:%d F:%d R:%d, Bef:%d Aft:%d';
  sSty03 = 'Alignment: %s'    + #13 +
            'Line spacing: %s' + #13 +
            'Numbering: %s'    + #13 +
            'Left indent: %s'  + #13 +
            'First indent: %s' + #13 +
            'Right indent: %s' + #13 +
            'Space before: %s' + #13 +
            'Space after: %s';
  sSty04 = 'Single';
  sSty05 = 'Double';
  sSty06 = 'other';
  sSty07 = 'Bullets';
  sSty08 = 'Left';
  sSty09 = 'Right';
  sSty10 = 'Center';
  sSty11 = 'Justify';
  sSty12 = 'superscript';
  sSty13 = 'subscript';
  sSty14 = 'Subscript ';
  sSty15 = 'Supercript ';
  sSty16 = 'Disabled ';
  sSty17 = 'Highlighted ';

  // kn_StyleMng.pas ========================================================================
  sStyM_ = 'kn_StyleMng.pas';
  sStyM01 = 'Style in active note: ';
  sStyM02 = 'Font: ';
  sStyM03 = 'Paragraph: ';
  sStyM04 = 'Named style: ';
  sStyM05 = 'Range: ';
  sStyM06 = 'Error: StyleManager does not exist.';
  sStyM07 = 'Create "%s" style';
  sStyM08 = 'Enter name for new style:';
  sStyM09 = '%s style "%s" already exists. ' + #13 + 'Redefine the existing style with new properties?';
  sStyM10 = ' Style "%s" created (%s)';
  sStyM11 = 'Error creating style: ';
  sStyM12 = 'Error: Cannot access style information for "%s"';
  sStyM13 = 'Rename style';
  sStyM14 = 'Enter new name for style:';
  sStyM15 = 'Cannot rename: a style by that name already exists.';
  sStyM16 = 'Error renaming style';
  sStyM17 = 'OK to delete %s style "%s"?';
  sStyM18 = 'Error deleting style';


  // kn_TemplateMng.pas ========================================================================
  sTpl01 = 'Template "%s" already exists. Overwrite existing template?';
  sTpl02 = 'Template "%s" created.';
  sTpl03 = 'Select template to insert';
  sTpl04 = 'OK to delete selected template "%s"?';


  // kn_ConfigMng.pas ========================================================================
  sCfg_ = 'kn_ConfigMng.pas';
  sCfg01 = 'Error in keyboard customization procedure: ';
  sCfg02 = ' Customize Tab icons (%s) ';
  sCfg03 = 'Invalid command line arguments:';
  sCfg04 = 'Error while loading custom keyboard configuration from %s: "%s"';
  sCfg05  = 'There was a non-fatal error while loading defaults: ' + #13 +
            '%s' + #13#13 +  'Some settings may have been reset to defaults.';

  // kn_ImagePicker.pas ========================================================================
  sImgP_ = 'kn_ImagePicker.pas';
  sImgP01 = ' icon %d ';


  // kn_ImageForm.pas ========================================================================
  sImgF_ = 'kn_ImageForm.pas';
  sImgF01 = 'Image no available. Change in caption will not saved';
  sImgF02 = 'Save image file as';
  sImgF03 = 'All image files';
  sImgF04 = 'Open image file  (Ctrl -> open file location)';


  // kn_ImagesUtils.pas ========================================================================
  sImgU01 = 'Error creating RTF for image insertion on editor: ';
  sImgU02 = 'Error processing RTF visible image (\pict) : ';
  sImgU03 = 'Error processing RTF hidden image (hyperlink) : ';
  sImgU04 = 'Error converting image format: ';


  // kn_ImagesMng.pas ========================================================================
  sImg_ = 'kn_ImagesMng.pas';
  sImg01 = 'Invalid Storage definition: ';
  sImg02 = 'Invalid Image definition: ';
  sImg03 = 'Invalid Embedded Image: ';
  sImg04 = 'Image not found: ';
  sImg09 = ' | %d instances';
  sImg05 = 'External storage not ready?' + #13 +
           'New images will be stored provisionally [only] as Embedded KNT when ext.storage not ready on save';
  sImg07 = 'Folder "%s" is not empty or valid';
  sImg08 = 'A file with that name already exists (%s)';
  sImg10 = 'Error %d opening "%s": "%s"';
  sImg11 = 'Folder "%s" does not exist or is empty';
  sImg12 = 'File "%s" does not exist or is not a valid Zip';
  sImg13 = 'All images will be adapted to the storage mode. If selected a new external storage, ' +
           'image files will only be added when you save the KNT file. Current external storage will not be modified.' + #13 +
           'You can change the storage again after saving the KNT file.' + #13+#13 + 'Continue?';
  sImg14 = 'Current external storage is not available or is invalid' + #13 +
            'If you moved the external storage, select "Relocated" and register its new location';
  sImg15 = 'All images have been adapted OK to the storage mode. Changes will be confirmed when KNT file is saved' + #13 +
            '(%d different images have been found)';
  sImg16 = 'Exception creating ZIP archive: ';
  sImg17 = 'Exception adding file to ZIP archive: ';
  sImg18 = 'Exception opening image viewer: ';
  sImg19 = 'Exception changing image storage mode: ';
  sImg20 = 'Exception processing image in RTF: ';
  sImg21 = 'Error saving image "%s" (ID:%d) :' + #13 +
           '  Content lost' + #13 +
           '  Will be removed from Images';
  sImg22 = '< Unregistered image >';


  // kn_FindReplaceMng.pas ========================================================================
  sFnd_ = 'kn_FindReplaceMng.pas';
  sFnd01 = 'Replace this occurrence?';
  sFnd02 = 'Pattern not found: "%s"';
  sFnd03 = 'Folder "%s" does not exist in this file.';
  sFnd04 = 'Tree node "%s" does not exist in folder "%s".';
  sFnd05 = 'Search results are not available.';
  sFnd06 = 'Options';
  sFnd07 = ' Searching - press ESC to abort.';
  sFnd08 = 'An error occurred during search:';
  sFnd09 = ' Pattern found at pos %d (%d occurrence(s))';
  sFnd10 = ' Pattern not found.';
  sFnd11 = ' Replaced %d occurrence(s)';
  sFnd12 = 'Information';
  sFnd13 = ' matches';

  // kn_Glossary.pas ======================================================================
  sGlss00 = 'No item selected.';
  sGlss01 = 'Shortcut term and its expanded definition cannot be blank.';
  sGlss02 = 'Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?';
  sGlss03 = 'Error saving Glossary list: ';
  sGlss04 = 'Glossary terms: %d';
  sGlss05 = 'Error loading Glossary list: ';

  // kn_BookmarksMng.pas ========================================================================
  sBmk01 = ' Bookmark %d assigned.';
  sBmk02 = ' Bookmark %d not assigned!';
  sBmk03 = ' Cannot access bookmark %d - Cleared';
  sBmk04 = ' Jumped to bookmark %d';

  // kn_FavExtDlg.pas ======================================================================
  sFavDlg_ = 'kn_FavExtDlg.pas';
  sFavDlg01 = 'The specified file does not exist. Do you want to use the filename anyway?';

  // kn_FavoritesMng.pas ======================================================================
  sFav_ = 'kn_FavoritesMng.pas';
  sFav01 = 'Error loading Favorites: ';
  sFav02 = 'Rename favorite location';
  sFav03 = 'Enter new name:';
  sFav04 = 'A favorite named "%s" already exists. Choose another name';
  sFav05 = 'Error renaming favorite location: ';
  sFav06 = 'Favorite KeyNote location';
  sFav07 = 'Enter location name:';
  sFav08 = ' or click Cancel to abort.';
  sFav09 = 'Delete "%s" from Favorites?';
  sFav10 = 'Error deleting Favorite: ';
  sFav11 = 'Favorites list error: ';


  // kn_AlertMng.pas ========================================================================
  sAlrt01 = '%d alarms selected';
  sAlrt02 = 'Set Alarm';
  sAlrt03 = '%d Reminders';
  sAlrt04 = 'All Alarms/Events (%d)';
  sAlrt05 = 'Overdue Events (%d)';
  sAlrt06 = 'Pending Reminders (%d)';
  sAlrt07 = 'Discarded Events (%d)';
  sAlrt08 = 'ALARM [%s] :  %s';
  sAlrt09 = '[Sound ON]';
  sAlrt10 = '[Sound OFF]';
  sAlrt11 = '%d pending reminders, %d overdue ';
  sAlrt12 = '[Popup ON]';
  sAlrt13 = '[Popup OFF]';
  sAlrt14 = 'Expiration/Start time and/or Reminder interval are not valid.'+ #13+ 'Please, correct it';
  sAlrt15 = 'OK to discard all this %d alarms?';
  sAlrt16 = 'OK to remove all this %d alarms?';
  sAlrt17 = 'OK to restore all this %d alarms?';
  sAlrt18 = 'OK to remove this alarm?';
  sAlrt19 = 'OK to apply pending changes?';
  sAlrt20 = 'Today';
  sAlrt21 = 'Tomorrow';
  sAlrt22 = 'All';
  sAlrt23 = 'Overdue';
  sAlrt24 = 'Pending';
  sAlrt25 = 'Discarded';
  sAlrt26 = 'All (with discarded)';
  sAlrt27 = 'Show all pending reminders (triggered and ignored, not postponed nor discarded)';
  sAlrt28 = 'Show all overdue events';
  sAlrt29 = 'Show all set alarms (not discarded)';
  sAlrt30 = 'Show all set alarms, including discarded';
  sAlrt31 = 'Show All Dates';
  sAlrt32 = 'Filter on selected Days';
  sAlrt33 = 'Filter on selected Week';
  sAlrt34 = 'Filter on selected Month';
  sAlrt35 = '(Filter applied)';
  sAlrt36 = '(%s overdue)';
  sAlrt37 = '(%s left)';
  sAlrt38 = '(%s before)';


  // kn_LinksMng.pas ======================================================================
  sLnk_ = 'kn_LinksMng.pas';
  sLnk01 = 'Folder ID not found: %d';
  sLnk02 = 'Folder name not found: %s';
  sLnk03 = 'Node ID not found: %d';
  sLnk03b = 'Node GID not found: %d';
  sLnk04 = 'Node name not found: %s';
  sLnk05 = 'Select file to link to';
  sLnk06 = 'Select file to insert';
  sLnk07 = 'The file you selected is not a plain-text or RTF file and cannot be inserted.';
  sLnk08 = 'Cannot insert link to a KeyNote location, because no location has been marked. First, mark a location to which you want to link.';
  sLnk09 = ' Location inserted';
  sLnk10 = ' Current location marked';
  sLnk11 = ' Failed to open location';
  sLnk12 = 'Location does not exist or file cannot be opened: "%s"';
  sLnk13 = 'Invalid location string: %s';
  sLnk14 = ' Invalid location';
  sLnk15 = 'Error executing hyperlink: %s';
//sLnk16 = ' Hold down SHIFT while clicking the URL:  ';
  sLnk17 = ' URL modified';
  sLnk18 = ' URL action canceled';
  sLnk19 = ' URL copied to clipboard';
  sLnk20 = 'Error %d executing hyperlink "%s": "%s"';
  sLnk21 = ' History error';
  sLnk22 = ' Cannot navigate to history location';
  sLnk23 = ' History navigation error';
  sLnk24 = 'Navigate backwards in history';
  sLnk25 = 'Navigate backwards in folder (''local'') history';
  sLnk26 = 'Navigate backwards in global history';
  sLnk27 = 'Navigate forward in history';
  sLnk28 = 'Navigate forward in folder (''local'') history';
  sLnk29 = 'Navigate forward in global history';
  sLnk30 = ' (Ctrl+click: only in folder history)';
  sLnk31 = ' [Mark: %d]';
  sLnk32 = '   (Undo to remove new hidden markers)';
  sLnk33 = 'Action canceled';


  // knt.ui.editor.pas  ======================================================================
  sEdt_ = 'knt.ui.editor.pas / kn_EditorUtils';
  sEdt01 = 'Invalid zoom ratio: ';
  sEdt02 = ' L %d / %d  C %d';
  sEdt03 = ' Sel: %d  W: %d';
  sEdt04 = ' Overwrite mode disabled through INI file';
  sEdt05 = 'Convert decimal to Roman';
  sEdt06 = 'Enter a decimal number:';
  sEdt07 = '%s is not a valid number';
  sEdt08 = 'Convert Roman to decimal';
  sEdt09 = 'Enter a Roman number:';
  sEdt10 = '%s is not a valid Roman number';
  sEdt11 = ' No valid bracket at cursor position ';
  sEdt12 = ' Matching bracket FOUND';
  sEdt13 = ' Matching bracket NOT FOUND';
  sEdt14 = 'OK to trim white space characters in whole note?';
  sEdt15 = 'OK to compress white space characters in whole note?';
  sEdt16 = ' Result: ';
  sEdt17 = 'Paste last eval result: ';
  sEdt18 = 'Expression %s evaluates to: %s' + #13#13 + 'Result was copied to clipboard. Click OK to insert.';
  sEdt19 = 'Select image to insert';
  sEdt20 = 'All image files';
  sEdt21 = ' Function not available';
  sEdt22 = ' No word at cursor';
  sEdt23 = ' Word not in glossary. Use Shift+F7 to add.';
  sEdt24 = 'Term expansion glossary "%s" is not loaded.';
  sEdt25 = 'Glossary term already exists: "%s" -> "%s". OK to redefine term as "%s"?';
  sEdt26 = ' Added to glossary: "%s" -> "%s"';
  sEdt27 = 'Replace editor contents with result from spellchecker?';
  sEdt28 = ' Calculating statistics... Please wait';
  sEdt29 = 'Selected text';
  sEdt30 = 'Folder text';
  sEdt31 = '%s statistics' + #13#13 +
       'Characters: %s' + #13 +
       'Alphabetic: %s' + #13 +
       'Whitespace: %s' + #13#13 +
       'Words: %s' + #13 +
       'Lines: %s';
  sEdt32 = 'Lookup in WordWeb';
  sEdt33 = 'Enter word to look up:';
  sEdt34 = 'Error loading WordWeb. The program may not be installed ' +
            'on your computer. See file "wordweb.txt" for more information.' +
            #13#13 +
            'Error message: ';

            // kn_EditorUtils.pas ---------
  sEdt35 = 'UAS path';
  sEdt36 = 'Please specify full path to uas.exe';
  sEdt37 = 'KeyNote cannot find the location of uas.exe. UltimaShell Autocompletion Server will not be loaded.';
  sEdt38 = ' UltimaShell Autocompletion Server loaded.';
  sEdt39 = 'Cannot load UltimaShell Autocompletion Server. It may not be installed. Would you like to go to the UAS website and download the application?';
  sEdt40 = ' UltimaShell Autocompletion Server unloaded.';
  sEdt41 = ' UltimaShell Autocompletion Server is not loaded.';
  sEdt42 = 'A Read-Only folder cannot be used for clipboard capture.';
  sEdt43 = 'a new node';
  sEdt44 = 'whichever node is currently selected';
  sEdt45 = 'Each copied item will be pasted into %s in the tree. Continue?';
  sEdt46 = ' Clipboard capture is now ';
  sEdt47 = ' Capturing text from clipboard';
  sEdt48 = ' Clipboard capture done';
  sEdt49 = 'Current folder contains more than one node. Do you want to print all nodes? Answer No to only print the selected node.';

            // kn_ClipUtils.pas
  sEdt50 = 'CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message: ';

  // kn_CharsNew.pas ======================================================================
  sChrs_ = 'kn_CharsNew.pas';
  sChrs01 = ' Edit';
  sChrs02 = ' Done';

  // kn_Chest.pas ======================================================================
  sChest_ = 'kn_Chest.pas';
  sChest01 = 'Failed to load built-in category images from resource.';
  sChest02 = 'Failed to load category images from ';
  sChest03 = 'Failed to save category images to ';


  // knt.App.pas ========================================================================
  sApp_ = 'knt.App.pas';
  sApp01 = ' Cannot perform operation: Editor is Read-Only';
  sApp02 = 'There is no active editor';
  sApp03 = 'Function not implemented. ';
  sApp04 = '(none)';
  sApp05 = ' Select some text before issuing this command.';
  sApp06 = 'Unexpected or not implemented command: ';
  sApp07 = 'Unexpected error. ';
  sApp08 = #13#13+'Number of nodes (notes) in tree: %d';
  sApp09 = 'Chars: %d  Alph: %d  Words: %d';
  sApp10 = #13#13+'Clik OK to copy information to clipboard.';
  sApp11 = 'Cannot display Tip of the Day: file "%s" not found.';
  sApp12 = ': Tip of the Day';


  // kn_Main.pas ========================================================================
  sMain_ = 'kn_Main.pas';
  sMain01 = 'Unable to assign "%s" as activation hotkey.';
  sMain02 = 'Unexpected error while turning %s Activation hotkey "%s": %s';
  sMain03 = '&Restore (%s)';
  sMain04 = '&Restore';
  sMain05 = 'Function key assignment updated';
  sMain06 = 'Revert to last saved version of' + #13 + '%s?';
  sMain07 = 'OK to quit %s?';
  sMain08 = 'Unexpected error:  %s' + #13#13 +
           'This message may indicate a bug in KeyNote NF. If the problem persists, please submit a bug reports with the Issue Manager' +
           ' available in KeyNote NF website: %s' + #13#13 +
           'You can continue working or terminate KeyNote NF. ' + #13 +
           'Terminate application?';
  sMain09 = 'KeyNote NF Error';
  sMain10 = 'Cannot perform operation: ';
  sMain11 = ' INS';
  sMain12 = ' OVR';
  sMain13 = 'KeyNote NF have been configured to allow only one instance at a time' + #13 + 'Closing this instance...';
  sMain14 = 'There was a non-fatal error while loading program configuration: ' + #13 + '%s' + #13#13 + 'Some options may have been reset to factory default values. The application will now continue.';

  sMain17 = 'You seem to have upgraded KeyNote from version %s to %s.' + #13 +
           'Files "history.txt" and "%s" contain information about the latest changes and additions.' + #13#13 +
           'Do you want to view the file "history.txt" now?';
  sMain18 = 'History file not found: "%s"';
  sMain19 = 'Custom date formats reloaded (%d)';
  sMain20 = 'Cannot load custom %s formats from %s. Check if the file exists.';
  sMain20a = 'date';
  sMain20b = 'time';
  sMain21 = 'Custom time formats reloaded (%d)';
  sMain25 = 'no file is open';
  sMain26 = 'currently open file has no folders';
  sMain27 = 'Folder is Read-Only';
  sMain29 = ' Printing folder...';
  sMain30 = ' Finished printing folder.';
  sMain31 = ' Preparing to send folder via email...';
  sMain32 = ' Folder sent';
  sMain33 = ' Folder not sent';
  sMain34 = 'Set alarm... (Ctrl:Add  Shift:->Folder)';
  sMain40= 'Untitled';
  sMain41 = 'No file';
  sMain42 = ' (no file)';
  sMain43 = ' Auto';
  sMain44 = ' MOD';
  sMain45 = ' Saved';
  sMain51 = 'Search and register note dates: creation and last modified';
  sMain52 = 'Remove date prefixes from node names';
  sMain53 = ' (Ctrl: Reconsider dates)';
  sMain56= 'Parser stack overflow';
  sMain57= 'Bad cell range';
  sMain58= 'Expected expression';
  sMain59= 'Expected operator';
  sMain60= 'Expected opening parenthesis';
  sMain61= 'Expected operator or closing parenthesis';
  sMain62= 'Invalid numeric expression';
  sMain63= 'Cannot evaluate: ';
  sMain64= 'Error at position ';
  sMain65= 'No notes in file';
  sMain66= 'Find tree node';
  sMain67= 'Find node containing text:';
  sMain68= ' Node not found!';
  sMain69= 'The Style toolbar must be visible to use this command. Show the Style toolbar now?';
  sMain70= 'No style available or none selected';
  sMain71= 'Error: StyleManager does not exist.';
  sMain72= 'Save tree structure to file';
  sMain81= 'Could not open KeyNote file "%s"';
  sMain82= 'This command will start your browser and direct it to KeyNote NF website, where ' +
          'you can download the latest version of the program, read the FAQ, submit bug reports or feature requests with the Issue Manager. ' +  #13+#13+ 'Continue?';
  sMain83= 'Hide &Resource Panel';
  sMain84= 'Show &Resource Panel';
  sMain85= 'Results';
  sMain86= 'Options';
  sMain87= 'Cannot hide the last visible tab. At least one tab must remain visible on the resource panel.';
  sMain88= 'Resource panel position will be updated after KeyNote is restarted.';
  sMain89= 'External: %s';
  sMain90= ' File: ';
  sMain91= ' Node: ';
  sMain92= '%s Folder: %s%s';
  sMain93= 'Double-click to insert selected template';
  sMain94= 'Toolbar configuration file "%s" not found. Default toolbar configuration file has been created.';
  sMain95= 'Saved toolbar layout to "%s".';
  sMain96= 'Starting number for numbered paragraphs:';

  // kn_VCLControlsMng.pas ======================================================================
  sVCL_ = 'kn_VCLControlsMng.pas';
  sVCL00 = 'Click and drag to resize panels (Ctrl: tree max width / Alt: Toggle fixed)';
  sVCL01 = 'Error destroying tabsheet ';
  sVCL02 = 'Select text color';
  sVCL03 = 'Select &Highlight...';
  sVCL04 = 'Select highlight color';
  sVCL05 = 'Apply current font color to text';
  sVCL06 = 'Apply &Highlight';
  sVCL07 = 'Apply current highlight color to text';
  sVCL08 = 'Minimize application';
  sVCL09 = 'Exit application';
  sVCL12 = 'Hide &Resource Panel';
  sVCL13 = 'Show &Resource Panel';
  sVCL14 = 'The Resource panel must be visible to use this command. Show the Resource panel now?';
  sVCL15 = 'Use the mouse to apply the %s to another text or press Esc to cancel';
  sVCL16 = 'paragraph formatting';
  sVCL17 = 'font formatting';


  // kn_FileMgr.pas ========================================================================
  sFmg_ = 'kn_FileMgr.pas';
  sFmg01 = 'Loading file manager from "';
  sFmg02 = 'Error initializing FileManager: ';
  sFmg03 = 'Notes file manager: %d file(s)';
  sFmg04 = 'This file cannot be selected because it does not exist or contains data in a format that %s does not support. Please select another file.';
  sFmg05 = 'FileManager list is empty. This dialog box will now close.';
  sFmg06 = 'never';
  sFmg07 = 'No information is available about this file.';
  sFmg08 = 'This file does not exist or cannot be accessed.';


  // kn_FileInfo.pas ========================================================================
  sFInf_  = 'kn_FileInfo.pas';
  sFInf01 = ' file';
  sFInf02 = 'File properties: ';
  sFInf03 = ' bytes';
  sFInf04 = '(file not saved)';
  sFInf05 = 'never';
  sFInf06 = 'Open "%s" as &Read-Only';
  sFInf07 = '(none)';
  sFInf08 = 'You chose to save the file using DartNotes format. However, this file contains tree-type notes, which are incompatible with DartNotes.' + #13#13+
            'If you click OK, the file will revert to using KeyNote format. Continue?';
  sFInf09 = 'The passphrase you entered is too short: Minimum passphrase length is %d characters';
  sFInf10 = 'The passphrases you entered do not match. Please enter the exact same passphrase twice.';
  sFInf11 = 'You chose to encrypt a file that contains virtual nodes. ' +
                     'Note that the disk files linked to virtual nodes and images saves in external storage (Zip or Folder) ' +
                     'will NOT be encrypted.' + #13#13 + 'Continue?';
  sFInf12 = 'File "%s" was open in READ-ONLY mode. If you uncheck this box, the read-only mode will be turned OFF. Continue?';
  sFInf13 = 'Open images storage folder';
  sFInf14 = 'Open images storage file';
  sFInf15 = 'Set';
  sFInf16 = 'Must save KNT before change images storage again';
  sFInf17 = '(*) Missing current external storage';
  sFInf18 = 'New images will be saved provisionally [only] as Embedded KNT' + #13 +
           'Deletions will be effective when it is available'+ #13#13 +
           '(It may be totally fine if you temporarily lose access to image storage)';
  sFInf19 = 'Current Next ID (%d) cannot be reduced' + #13 + '(Max ID in image list is %d)';
  sFInf20 = 'Max ID in image list is %d and Next ID is %d' + #13#13 +
           'Do you want the NEXT image to be saved with ID = %d ' + #13#13 +
           '* YOU MUST MAKE SURE there are no images with larger IDs on the external storage, perhaps referenced by other knt files ' +
           '(New images could override existing files)'  + #13#13 +
           'CONTINUE?';
  sFInf21 = 'Next ID was changed ok';


  // kn_FileDropAction.pas ======================================================================
  sFDrp_ = 'kn_FileDropAction.pas';
  sFDrp01 = 'file';
  sFDrp02 = 'files';
  sFDrp03 = 'Select import method (%d *%s %s)';
  sFDrp04 = '&General options';
  sFDrp05 = '&Virtual node...';
  sFDrp06 = '&HTML options';
  sFDrp07 = 'Some files will be renamed';


  // kn_KntFile.pas ======================================================================
  sFile_ = 'kn_KntFile';
  sFile01 = 'Cannot open "%s": File not found';
  sFile02 = 'Invalid file header in "%s" (not a KeyNote file)';
  sFile03 = 'Access passphrase not specified: cannot open encrypted file.';
  sFile04 = 'The passphrase is invalid. Try again?';
  sFile05 = '%s: This file was created with a version of KeyNote later than the version you are using. ' +
           'Expected version ID: "%s.%s" This file version ID: "%s.%s"  You need the latest version of KeyNote to open this file.';
  sFile06 = ': This file was created with a version of KeyNote newer than the version you are using. ' +
           'The file can be opened, but some information can be lost or misinterpreted. As a safety measure, the file should be opened in Read-Only mode. ' +
           'Would you like to open the file as Read-Only?';
  sFile07 = '%s: Invalid file header or version, or corrupt file.';
  sFile08 = 'Error loading folder ';
  sFile10 = 'This file contains notes which are not compatible with %s format. Only %s notes can be saved in this format.';
  sFile12 = 'Error: Filename not specified.';
  sFile13 = 'Error while saving folder "%s": %s';
  sFile14 = 'Cannot save: Passphrase not set';
  sFile15 = 'Stream size error: Encrypted file is invalid or corrupt.';
  sFile16 = 'Invalid passphrase: Cannot open encrypted file.';
  sFile18 = 'OK to convert to PLAIN TEXT current note?'+ #13#13 +'ALL IMAGES and FORMATTING will be REMOVED !!';
  sFile19 = 'Exception trying to ensure plain text and removing of images: ';
  sFile20 = 'Virtual note "%s" cannot write file ';
  sFile21 = 'OK to deduce the missing date information?'+ #13;
  sFile22 = 'OK to remove date from note name?'+ #13;
  sFile23 = 'All (or selected) nodes will be considered';
  sFile24 = #13#13 + 'Please read the help file before proceeding. Search for "Deduce Dates"';


  // kn_NoteFileMng.pas ======================================================================
  sFileM01 = 'Cannot create a new file: ';
  sFileM02 = ' New KNT file created.';
  sFileM04 = 'A new KNT file has been created. Would you like to save the new file now?' +#13#13+ '(The Auto Save function will not work until the file is named and saved first.)';
  sFileM05 = 'Open Keynote file';
  sFileM06 = ' Opening ';
  sFileM07 = 'One or more errors occurred while loading the file. The file may not have loaded completely. To minimize the risk of data loss, ' +
             'the file was opened in Read-Only mode. Use the "Save As..." command to save the file.';
  sFileM08 = ' <unknown> ';
  sFileM09 = ' diskette ';
  sFileM10 = ' network ';
  sFileM11 = ' CD-ROM ';
  sFileM12 = ' RAM ';
  sFileM13 = 'File "%s" was opened in Read-Only mode, because it resides on a %s drive "%s".';
  sFileM14 = ' File opened.';
  sFileM15 = ' Error.';
  sFileM16 = 'Folder monitor error: ';
  sFileM17 = ' ERROR %d opening file';
  sFileM18 = 'This file will be saved as a %s file. This format does not support some features which are unique to %s.' + #13#13 +
             'OK to save the file in Dart Notes format? If you answer NO, the file will be saved as a %s file.';
  sFileM19 = ' Saving ';
  sFileM20 = 'Specified backup directory "%s" does not exist. Backup files will be created in the original file''s directory.';
  sFileM21 = 'Cannot create backup file (error %d: %s). Current file will not be backed up. Proceed anyway?'+ #13#13 +' (Note: File was temporary saved in %s)';
  sFileM22 = ' File saved (%d folders, %d notes)';
  sFileM23 = ' Error %d while saving file.';
  sFileMInfSaving = '* NOTE:' +  #13 +
                  '  - The .knt file in disk must not have been modified from last correct save.'  + #13 +
                  '  - You should have multiple backup files in the folder %s, specially if you selected the option "Backup at regular intervals" (highly recommended)';
  sFileM24 = 'Error %d occurred while saving to a temporal folder (%s). The contents of the file in memory are perhaps partially corrupted.'  + #13#13 +
            '-> Please, retry, and if you can''nt save to a .knt file, try to recover the nodes/notes with unsaved changes using, for example, File -> Export...'  + #13#13#13;
  sFileM25 = 'Failed to create output file "%s" (Error: %d)' + #13 + 'File was temporary saved in %s' + #13#13#13 ;
  sFileM26 = 'The Auto-Save option was turned OFF, to prevent KeyNote from automatically saving the (perhaps) damaged file.';
  sFileM27 = ' ERROR saving file';
  sFileM28 = 'Saving "';
  sFileM29 = 'Folder monitoring has been disabled due to the following error: ';
  sFileM30 = ' File closed.';
  sFileM31 = 'Revert to last saved version of' + #13 + '%s?';
  sFileM32 = 'Select backup folder';
  sFileM33 = 'Cannot copy file to its own directory.';
  sFileM34 = 'The file %s already exists. OK to overwrite existing file?';
  sFileM35 = ' Copying file...';
  sFileM36 = ' File copied.';
  sFileM37 = 'Successfully copied KNT file to';
  sFileM38 = 'Copying failed (';
  sFileM39 = 'Select file to merge folders from';
  sFileM40 = 'There was an error while loading merge file.';
  sFileM41 = 'The file you selected does not contain any folders.';
  sFileM42 = 'Error while loading merge file: ';
  sFileM43 = 'Folders in %s';
  sFileM44 = 'You did not select any folder: nothing to merge.';
  sFileM45 = ' Merging folders...';
  sFileM46 = 'Error while adding folders: ';
  sFileM47 = 'Merged %d folders from "%s"';
  sFileM48 = 'No folders were merged';
  sFileM83 = '%d Links or Mirror nodes couldn''t be adapted'+#13+'Links can be found searching for "file///<%d"';
  sFileM49 = 'Another application has modified the knt file %s. Reload the file from disk?';
  sFileM50 = '%s folder "%s" does not exist';
  sFileM51 = '. Create the folder now?';
  sFileM52 = 'Could not create folder: %s';
  sFileM53 = ' File modified by external application.';
  sFileM54 = 'Folders were modified. Save file before continuing?' +#13+ 'If you answer No, you will lose all changes made since last save.';
  sFileM55 = 'Current file has not been saved. If you continue, changes will be lost.'+ #13 + 'Proceed anyway?';
  sFileM56 = 'Warning!';
  sFileM57 = 'Select files for importing';
  sFileM58 = 'The file "%s" does not appear to be a text file (nor image). The result of importing it may be unpredictable.' + #13#13 +
             'Import as a plain text file, anyway?';
  sFileM59 = ' Importing ';
  sFileM60 = 'Failed to convert HTML file "%s" to RTF';
  sFileM61 = 'Error importing ';
  sFileM62 = ' Finished importing.';
  sFileM63 = 'Cannot select methods for handling files.';
  sFileM65 = 'Cannot import a directory "%s"';
  sFileM67 = 'Unknown or unexpected file action (%d)';
  sFileM68 = 'Error while importing files: ';
  sFileM75 = 'Successfully created %s registry entries';
  sFileM76 = 'There was an error while creating file type associations: ';
  sFileM77 = 'This file is Read-Only. Use "Save As" command to save it with a new name.';
  sFileM78 = 'Backup at %s before any modification in "%s"';
  sFileM79 = 'File is not modified. Nothing to save';
  sFileM80 = #13#13 + 'Option "Autoregister file type" will be unchecked';
  sFileM81 = 'Cannot insert images in a plain text folder';
  sFileM82 = 'The file must first be saved (with Save or Save As)';


  // kn_ExportImport.pas ======================================================================
  sExp01 = 'Error while importing HTML text: ';
  sExp02 = 'Error while exporting to HTML (method= ';

  // kn_ExportNew.pas ======================================================================
  sExpFrm_ = 'kn_ExportNew.pas';
  sExpFrm00 = 'Export node content';
  sExpFrm01 = 'Exporting is underway. OK to abort?';
  sExpFrm02 = 'Please select a valid directory for exported files.';
  sExpFrm03 = 'Specified output directory does not not exit. Please select a valid directory.';
  sExpFrm04 = 'You did not select any foldersnotes for exporting.';
  sExpFrm11 = 'Error while exporting folders: ';
  sExpFrm12 = 'Exported  %d folders (%d notes).';
  sExpFrm13 = 'Exporting was aborted due to an error.';
  sExpFrm14 = 'Exporting was aborted at user request.';
  sExpFrm15 = 'The following token can be used in headings:' + #13#13 +
                  '%s%s - Filename'  + #13 +
                  '%s%s - Folder name' + #13 +
                  '%s%s - Node name' + #13 +
                  '%s%s - Node level' + #13 +
                  '%s%s - Node index'  + #13 +
                  '%s%s - Line break'  + #13 +
                  '%s%s - Symbols, increasing'  + #13 +
                  '%s%s - Symbols, decreasing' + #13#13 +
                  'F1 => More INFO and usage examples';
  sExpFrm16 = 'No active tree node: select a node first.';
  sExpFrm17 = 'Current node has no text: nothing to export.';
  sExpFrm18 = ' Node exported to ';
  sExpFrm19 = 'Error exporting node: ';
  sExpFrm20 = '''Current node'' will be managed as ''Current node and subtree'' for KeyNote format'+ #13 +' Continue?';


  // kn_KntFolder_New.pas ======================================================================
  sFldN_ = 'kn_KntFolder_New.pas';
  sFldN01 = '<no icon>';
  sFldN02 = 'Rename folder';
  sFldN03 = 'Folder name cannot be blank. Please enter a name.';
  sFldN04 = 'Folder name cannot contain the "%s" character';

  // kn_KntFolder.pas ======================================================================
  sFld_ = 'kn_KntFolder.pas';
  sFld01 = ' Virtual: ';
  sFld05 = 'Problem while saving folder "%s": Note count mismatch (Folder: %d  Internal: %d) ' +
           'The note may not be saved correctly. Continue?';
  sFld07 = 'Node count mismatch.';
  sFld09 = 'Folder contains %d notes, but only %d were saved.';
  sFld10 = 'Could not load Virtual Node file:';
  sFld11 = 'Failed to open TreePad file ';
  sFld21 = ' New folder.';
  sFld22 = 'Are you sure you want to DELETE FOLDER "%s"?' + #13 + 'This operation cannot be undone.';
  sFld24 = ' Folder deleted.';
  sFld25 = ' Folder renamed.';
  sFld31 = 'Virtual node "%s" is currently linked to file "%s". Do you want to link the node to a different file?';
  sFld32 = 'Node "%s" contains text. Do you want to flush this text to a file and make the node virtual?';
  sFld33 = 'This KeyNote file is encrypted, but disk files linked to virtual nodes will NOT be encrypted.' + #13#13 + 'Continue?';
  sFld34 = 'Select file for virtual node';
  sFld35 = 'Only RTF, Text and HTML files can be linked to virtual nodes.';
  sFld36 = 'Cannot link virtual node to a file on removable drive %s:\ ';
  sFld37 = 'You are creating a virtual node linked to file on removable drive %s\. The file may not be available at a later time. Continue anyway?';
  sFld38 = 'Selected file is already linked to a virtual node' + #13 + '(Note: You can create a linked node to it)';
  sFld39 = 'Virtual node error: ';
  sFld40 = 'OK to reload the node from file %s?';
  sFld41 = 'Unlink virtual node "%s"? The contents of the node will be retained, but the link with the file on disk (%s) will be removed.';
  sFld42 = 'Virtual node %s HAS BEEN modified within KeyNote. If the node is refreshed, the changes will be lost' + #13;
  sFld43 = 'Virtual node %s has NOT been modified within KeyNote' + #13;
  sFld44 = 'Error refreshing virtual node: ';
  sFld45 = ' Virtual node refreshed.';
  sFld46 = ' Error refreshing node';
  sFld47 = 'Selected node "%s" is not a virtual node.';


  // knt.ui.tree.pas ======================================================================
  sTree_ = 'knt.ui.tree.pas';
  sTree01 = 'Error creating node: ';
  sTree04 = 'Initial node not assigned - select a node and retry.';
  sTree05 = 'cannot be ';
  sTree06 = 'Error moving node: ';
  sTree07 = 'Node "%s" %smoved %s';
  sTree08 = #13#10 + 'This operation cannot be undone.';
  sTree09 = 'Node "%s" has %d child nodes. Delete these child nodes too?';
  sTree10 = 'OK to delete node "%s"?';
  sTree11 = 'OK to delete %sALL SELECTED nodes?'+ #13 +' (Confirmation will be requested for each node with children)' + #13;
  sTree12 = 'OK to delete %d CHILD NODES of node "%s"?';
  sTree13 = 'Selected node has no children.';
  sTree14 = 'Error deleting node: ';
  sTree15 = 'No nodes available for copying or pasting data.';
  sTree16 = 'OK to MOVE %d nodes/subtrees to current node "%s"?';
  sTree17 = ' No node is selected';
  sTree18 = 'OK to forget %s?';
  sTree19 = 'Target node is included in one of the subtrees to move';
  sTree20 = ' nodes/subtrees registered for transfer';
  sTree21 = 'No data to paste. Select "Transfer|Copy/Cut Subtree" first.';
  sTree22 = #13#13 + '* One or more nodes being copied is a Virtual Node. They will be pasted as linked nodes' + #13#13 + 'Continue?';
  sTree23 = 'OK to PASTE %d nodes/subtrees%s below current node "%s"?' + #13#10 + '(Hidden nodes will ALSO be pasted)';
  sTree26 = ' as LINKED nodes';
  sTree24 = ' Pasted %d nodes/subtrees';
  sTree25 = '%d virtual nodes have been copied as linked nodes';
  sTree27 = 'Node not found (Folder ID/Node ID): %d/%d';
  sTree30 = 'Copy';
  sTree31 = 'Move';
  sTree32 = 'Link';
  sTree49 = 'OK to sort the entire tree?';
  sTree50 = ' Node name cannot be blank!';
  sTree51= ' Node renamed.';
  sTree52= ' Cannot perform operation: ''%s'' folder is read-only';
  sTree53 = 'Edit node name';
  sTree54 = 'Enter new name:';
  sTree55 = 'Disable Filter on tree';
  sTree56 = 'Apply Filter on tree';
  sTree57 = ' (Ctrl: Clear Find filter)';
  sTree59 = 'OK to remove all Flags in folder?';
  sTree60 = 'Last modified >= "%s"';
  sTree62= 'Name';
  sTree63= 'Note name';
  sTree64= 'Date';
  sTree65= 'Note creation date';
  sTree66= 'Flagged';


  // knt.ui.note.pas ======================================================================
  sUInote_ = 'knt.ui.note.pas';
  //sUInote01 = 'Entry created: %s  ##  Note last modified: %s';     // TODO.. Entries
  sUInote01 = 'Created: %s  ==  Last modified: %s';


  // kn_Defaults.pas -------------------
  sDef31 =  'REMEMBER:' + #13#13 +
            '- Folder settings apply only to NEW notes, except:' + #13 +
            '   - ''Plain note only'': modifies ALL the notes' + #13 +
            '   - ''WordWrap'': affects ALL the notes' + #13 +
            '       (not explicitly set WordWrap previously)' + #13#13 +
            '>> More info in Help File (F1)'
            ;
  sDef32 =  'REMEMBER:' + #13#13 +
            '- Font change affect only to NEW nodes (all if Plain note)' + #13 +
            '- BG Color depends on ''Inherit BG color from active node'':' + #13 +
            '   - If set, background color of selected node is shown' + #13 +
            '     (=> BG color of its new child nodes)' + #13 +
            '   - If not set, default BG color for all NEW nodes is shown' + #13 +
            '   * To edit this option -> F5 | General settings| Rich Text editor' + #13#13 +
            '- BG Color can be changed for ALL nodes in a Folder:' + #13 +
            '    [Shift] + "Format | Background color"' + #13#13 +
            '>> More info in Help File (F1)'
            ;
  sDef33 =  'REMEMBER:' + #13#13 +
            '- BG Color sets backgroud color for the Tree Panel and' + #13 +
            '  default BG color of tree nodes' + #13 +
            '- Previous changes to individual nodes won''t be affected' + #13 +
            '- ''Inherit properties from active node'' option is' + #13 +
            '  considered in NEW nodes' + #13 +
            '- Font and BG color can be changed for ALL tree panels at once:' + #13 +
            '    "Apply to ALL folders"' + #13#13 +
            '- Note: ''Inherit BG color from active node'' option does NOT' + #13 +
            '  affect (refers to Editor) ' + #13#13 +
            '>> More info in Help File (F1)'
            ;



implementation



end.
