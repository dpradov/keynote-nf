unit kn_Global;

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
   Winapi.ShellAPI,
   System.Classes,
   System.SysUtils,
   System.IOUtils,
   Vcl.Graphics,
   Vcl.Forms,
   Vcl.Menus,
   Vcl.Controls,
   Vcl.Dialogs,

   TreeNT,
   RxRichEd,
   UWebBrowserWrapper,
   RichPrint,
   TB97,

   gf_files,
   gf_strings,
   kn_History,
   kn_const,
   kn_Info,
   kn_CharsNew,
   kn_LocationObj,
   kn_AlertMng,
   kn_NoteObj,
   kn_FileObj,
   kn_NodeList,
   kn_Cmd,
   kn_Main,
   kn_ImagesMng
 {$IFDEF KNT_DEBUG}
   ,GFLog
 {$ENDIF}
 ;

const
  _TIMER_INTERVAL = 2000; // two seconds

   procedure InitializeKeynote (Form_Main: TForm_Main);
   procedure InitializeOptions;
   procedure LoadRicheditLibrary;
   procedure AddSearchModes;
   procedure AddSearchScopes;
   function NoteSupportsRegisteredImages (AdmitVmRTF: boolean= false): boolean;
   function NoteSupportsImages: boolean;

   function ActiveKeyNoteHelp(Folder, Node, Marker: integer): Boolean; overload;
   function ActiveKeyNoteHelp(Node: integer): Boolean; overload;
   function ActiveKeyNoteHelp(Node_Marker: PChar): Boolean; overload;
   function ActiveKeyNoteHelp_FormHelp(Command: Word; Data: NativeInt): Boolean;

   procedure Log_StoreTick (const Msg : string; const DbgLevel: integer= 0; DetailLevel: integer = 0); {$IFNDEF KNT_DEBUG} inline; {$ENDIF}
   procedure Log_Flush;                                                                                {$IFNDEF KNT_DEBUG} inline; {$ENDIF}

var

    KntFile : TKntFile; // main data structure
    ActiveKntFolder : TKntFolder; // the Folder (Note Folder) that is currently visible (can be nil)

    RichEditLibraryPath : string;

     //======================================= FILES
    INI_FN : string; // main keynote.ini file (alternate filename may be given on command line)
    MRU_FN : string; // MRU file list and form position/size info
    TIP_FN : string; // tip of the day file
    DEF_FN : string; // defaults for new notes
{$IFDEF KNT_DEBUG}
    LOG_FN : string; // main log file (unused unless built with KNT_DEBUG)
{$ENDIF}
    MGR_FN : string; // file manager data
    ICN_FN : string; // custom icons
    //KEY_FN : string; // keyboard customization file (old - via plugin)
    FAV_FN : string; // favorites storage file
    Scratch_FN  : string; // scratchpad filename
    Style_FN    : string; // custom styles
    Toolbar_FN  : string; // toolbar configuration file
    Keyboard_FN : string; // keyboard customization file (NEW - all menu items)
    OrigDEF_FN : string;
{$IFNDEF EXCLUDEEMAIL}
    MailINI_FN : string; // INI file for email options (keymail.ini)
{$ENDIF}
    Glossary_FN : string;
    NoteHeadingTpl_FN: string;     // Template for note heading, when exporting (notehead.rtf)
    NodeHeadingTpl_FN: string;     // Template for node heading, when exporting (nodehead.rtf)

    Help_FN : string;     // Help file (.knt)
    HelpINI_FN : string;  // INI file to use with Help file (.knt)
    Launcher_FN: string;  // kntLauncher.exe

    //================================================== OPTIONS
    { These options are seperate from KeyOptions, because then
      may also be set via commandline. Basically, the logic is:
      opt_XXX := ( commandline_argument_XXX OR inifile_options_XXX );
    }
    opt_Minimize : boolean; // minimize on startup
    //opt_Setup : boolean; // run setup (OBSOLETE, unused)
    opt_Debug : boolean; // debug info
    opt_NoRegistry : boolean; // use .MRU file instead, do not use registry
    opt_NoReadOpt : boolean; // do not read config files (if TRUE, then opt_NoSaveOpt is also set to TRUE)
    opt_NoSaveOpt : boolean; // do not save config files
    opt_NoDefaults : boolean; // do not load .DEF file (editor and tree defaults)
    opt_RegExt : boolean; // register .KNT and .KNE extensions
    opt_SaveDefaultIcons : boolean; // save default tab icons to file
    opt_NoUserIcons : boolean; // do not use custom .ICN file
    opt_SaveToolbars : boolean; // save default toolbar state (debug)
    opt_SaveMenus : boolean; // save menu item information
    opt_DoNotDisturb : boolean; // Ignore for purposes of "SingleInstance"
    opt_Title: string; // Title to use in main window (mainly for its use with kntLauncher)

    opt_Clean : boolean; // Clean the file, actually looking for invalid hyperlinks (see issue #59: http://code.google.com/p/keynote-nf/issues/detail?id=59

    // these are declared in kn_Info.pas
    KeyOptions : TKeyOptions; // general program config
    TabOptions : TTabOptions; // options related to tabs, icons etc
    ClipOptions : TClipOptions; // clipboard capture options
    EditorOptions : TEditorOptions;
    ResPanelOptions : TResPanelOptions;
    TreeOptions : TKNTTreeOptions;
    FindOptions : TFindOptions;

    LastExportFilterIndex : integer;
    ShowHiddenMarkers: boolean;

    //================================================== COMMAND LINE
    KntFileToLoad : string; // name of KNT file we are supposed to open (options + commandline + passed from other instance, etc)
    CmdLineFileName : string; // other filename passed on command line (macro, plugin, etc)


    //================================================== KEYBOARD / HOTKEY
    HotKeySuccess : boolean; // if true, we registered the hotkey successully, so we will remember to unregister it when we shut down
    AltFKeys : TFuncKeys;      // primitive, but that's all we can do for now (0.999)
    ShiftAltFKeys : TFuncKeys; // these records keep custom assignments for Alt, Shift+Alt and Ctrl+Alt function key combos.
    CtrlAltFKeys : TFuncKeys;  // They can be modified manually (keynote.key) or by using the FUNCKEY plugin.

    OtherCommandsKeys: TList;      // List of TKeyOtherCommandItem

    LastRTFKey : TKeyCode;
    RxRTFKeyProcessed : boolean; // for TAB handling; some tabs are eaten by TRichEdit, others must not be
    RTFUpdating : boolean; // TRUE while in RxRTFSelectionChange; some things cannot be done during that time


    //==================================================
    //InsCharFont : TFontInfo;
    //Form_Chars : TForm_Chars; // GLOBAL FORM!
    Form_Chars : TForm_CharsNew; // GLOBAL FORM!


    //================================================== DEFAULT PROPERTIES
    DefaultEditorProperties : TFolderEditorProperties;
    DefaultTabProperties : TFolderTabProperties;
    DefaultEditorChrome : TChrome;
    DefaultTreeChrome : TChrome;
    DefaultTreeProperties : TFolderTreeProperties;


    //================================================== APPLICATION STATE
    FirstTimeRun : boolean; // true if INI file not found; assume fresh install. (load "sample.knt", etc.)

    Initializing : boolean; // true from main form's CREATE up to exit from ACTIVATE. Certain things cannot be done during that time, such as focusing controls
    FileIsBusy : boolean; // if TRUE, file is being saved or opened, so we can't mess with it
    FileState : TFileState; // for file change notification (remembers previous file size, date and time)
    FileChangedOnDisk : boolean; // for file change notification. If true, we will prompt to reload at nearest opportunity.
    LastImportFilter : integer; // just so we can set the OpenDlg filterindex property
    TerminateClick : boolean;  // true ONLY on File->Exit click and TrayIcon menu Exit click
    ClosedByWindows : boolean; // true on WM_QUERYENDSESSION message, windows is shutting down
    AppIsClosing : boolean;    // true if OnCloseQuery exits with CanClose := true
    ClosedOnPreviousInstance : boolean;  // if TRUE, we are being closed because another instance of KeyNote is already running

    LastFileModifiedFlag : boolean;

   //================================================== TIMER
    Timer_Tick : integer; // timer counter, for autosave
    Timer_TickAlarm: integer;  // timer counter, for checking alarms
    AppLastActiveTime : TDateTime; // for auto-minimizing and auto-closing on timeout
(*
    {$IFDEF WITH_TIMER}
    ThisTick, LastTick : integer;
    TickList : TStringList;
    {$ENDIF}
*)

    //================================================== CLIPBOARD
    _IS_CAPTURING_CLIPBOARD : boolean;
    _IS_CHAINING_CLIPBOARD : boolean;
    _IS_COPYING_TO_CLIPBOARD : boolean;
    ClipCapNextInChain : HWnd;
    LastEvalExprResult : string; // remembered, so that we can paste it

    ClipCapActive : boolean; // TRUE if we have a clipboard capture note
    ClipCapNote : TKntNote;
    ClipCapCRC32 : DWORD;

    AppIsActive : boolean; // used with Clipboard Capture to ignore copy events coming from Keynote itself
    _ConvertHTMLClipboardToRTF: boolean;

    //================================================== TREE
    MovingTreeNode: TTreeNTNode;            // To use with Paste, after applying Cut on a Tree Node.
    CopyCutFromNoteID: integer;             // Copy or Cut from Folder

    //================================================== VARIOS

    { *1
     When the user hides the Tree Panel pressing Ctrl (usually: Ctr + Shift+F11  or Ctrl + [View | Tree Panel] ) then,
     instead of letting the editor panel grow to the left occuping the width of the tree panel, the application window will
     reduce its width temporarily so that the editor maintain its width and position.

     Once shown the Tree panel again, the application window's size and position will be restored.

     If, after hiding the tree panel in this way (the window's width is now reduced), the user selects another note, then
     the application will automatically make the tree panel visible and restore the aplication window width, before changing
     to the other note.

     Opening onother file or closing the application while the windows's width is reduced, will restore it to it's correct size.

     This functionality can be disabled in keynote.ini, with: IgnoreCtrHideTrePanel = 1

     * _WindowWidthIncToRestore will save the width of the Tree Panel, as a way to know that the user hided the Tree Panel pressing Ctrl

    }

    _GLOBAL_URLText : string;
    _IS_FAKING_MOUSECLICK : boolean;
    _Global_Location : TLocation;
    _REOPEN_AUTOCLOSED_FILE : boolean;
    //_Is_Dragging_Text : boolean;
    _LastZoomValue : integer;
    _WindowWidthIncToRestore: integer;             // *1
    NumberingStart: integer;

    OriginalComboLen : integer;

    UAS_Window_Handle : HWND;
    LAST_CASE_CYCLE : TCaseCycle;

    _SYSTEM_IS_WIN95 : boolean;
    _SYSTEM_IS_WINXP : boolean;

    AlarmManager: TAlarmManager;    // [dpv]
    ImagesManager: TImageManager;
    RTFAux_Note: TTabRichEdit;       // For exclusive use of TKntFolder when obtaining TextPlain
    _DllHandle : THandle;
    _IE: TWebBrowserWrapper;

    History : TKNTHistory;         // Global navigation history

    ShowingSelectionInformation: boolean;
    ShowingImageOnTrack: boolean;

    DraggingImageID: integer;
    DraggingImage_PosImage: integer;
    DraggingImage_PosFirstHiddenChar: integer;

    LastGoTo : string; // last line number for the "Go to line" command



  {$IFDEF KNT_DEBUG}
    Log : TGFLog;
  {$ENDIF}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

implementation
uses
   gf_misc,
   kn_ini,
   kn_msgs,
   kn_Glossary,
   kn_StyleObj,
   kn_EditorUtils,
   kn_fileMgr,
   kn_Chest,
   kn_Macro,
   kn_plugins,
   kn_ConfigMng,
   kn_FindReplaceMng,
   kn_TemplateMng,
   kn_StyleMng,
   kn_NoteFileMng,
   kn_MacroMng,
   kn_VCLControlsMng,
   kn_LinksMng
   ;



resourcestring
  STR_01 = 'KeyNote NF have been configured to allow only one instance at a time' + #13 + 'Closing this instance...';
  STR_02 = 'There was a non-fatal error while loading program configuration: ' + #13 + '%s' + #13#13 + 'Some options may have been reset to factory default values. The application will now continue.';


//-----------------

function NoteSupportsRegisteredImages (AdmitVmRTF: boolean= false): boolean;
var
  Folder: TKntFolder;
  treeNTNode: TTreeNTNode;
begin
   Result:= false;

   Folder:= ActiveKntFolder;
   if not assigned(Folder) then exit;
   if Folder.PlainText then exit;

   treeNTNode:= Folder.TV.Selected;
   if not assigned(treeNTNode) then exit;

   if (TKntNote(treeNTNode.Data).VirtualMode in [vmNone, vmKNTNode]) or (AdmitVmRTF and (TKntNote(treeNTNode.Data).VirtualMode = vmRTF)) then
      Result:= true;
end;


function NoteSupportsImages: boolean;
begin
  Result:= NoteSupportsRegisteredImages (true);
end;



procedure AddSearchModes;
var
   sm : TSearchMode;
begin
  if not assigned(Form_Main) then exit;
  with Form_Main do begin
      RG_ResFind_Type.Items.Clear;
      for sm := low( TSearchMode ) to high( TSearchMode ) do
      begin
        RG_ResFind_Type.Items.Add( SEARCH_MODES[sm] );
      end;
      RG_ResFind_Type.ItemIndex := 0;
  end;
end;

procedure AddSearchScopes;
var
   ss : TSearchScope;
begin
  if not assigned(Form_Main) then exit;
  with Form_Main do begin
      RG_ResFind_Scope.Items.Clear;
      for ss := low( TSearchScope ) to high( TSearchScope ) do
      begin
        RG_ResFind_Scope.Items.Add( SEARCH_SCOPES[ss] );
      end;
      RG_ResFind_Scope.ItemIndex := 0;
  end;
end;

procedure AddSearchChkModes;
var
   sc : TSearchCheckMode;
begin
  if not assigned(Form_Main) then exit;
  with Form_Main do begin
      RG_ResFind_ChkMode.Items.Clear;
      for sc := low( TSearchCheckMode ) to high( TSearchCheckMode ) do
      begin
        RG_ResFind_ChkMode.Items.Add( SEARCH_CHKMODES[sc] );
      end;
      RG_ResFind_ChkMode.ItemIndex := 0;
  end;
end;


//====================================================================
procedure InitializeKeynote (Form_Main: TForm_Main);
var
  debugmenu : TMenuItem;
  s : string;
  sm : TSearchMode;
  V : TOSVersionInfo;
begin
  MovingTreeNode:= nil;
  NumberingStart:= 1;

  History := TKNTHistory.Create (_MAX_GLOBAL_NAV_HISTORY);

  with Form_Main do begin
      SBGlyph:= TPicture.Create;

     {$IFDEF KNT_DEBUG}
      Log.Add( Format('DEFAULT_CHARSET:%d  Default CODEPAGE:%d  System_LANGUAGE: %d', [DEFAULT_CHARSET, TEncoding.Default.CodePage, GetSystemDefaultLCID]), 0);
     {$ENDIF}

      Log_StoreTick( 'FormCreate - Begin', 0, +1);

      // check Windows version
      _SYSTEM_IS_WIN95 := false;
      _SYSTEM_IS_WINXP := false;

      V.dwOSVersionInfoSize := sizeof( V );
      if GetVersionEx( V ) then
      begin
        case V.dwPlatformId of
          VER_PLATFORM_WIN32_WINDOWS : begin
            _SYSTEM_IS_WIN95 := ( V.dwMinorVersion = 0 );
          end;
          VER_PLATFORM_WIN32_NT : begin
            _SYSTEM_IS_WINXP := ( V.dwMinorVersion = 1 );
          end;
        end;
      end;

      AlarmManager:= TAlarmManager.Create;   // [dpv]
      ImagesManager:= TImageManager.Create;  // [dpv]
      RTFAux_Note:= CreateRTFAuxEditorControl;

      ShowingSelectionInformation:= false;

      AppIsActive := true;
      try
        // Used to be used for instance management, but isn't anymore.
        // It is still necessary for the Setup program, so that it knows
        // KeyNote is running and won't try to install over a running .exe
        CreateMutex( nil, False, UniqueAppName_KEYNOTE10 );
      except
      end;

      _MainFormHandle := Handle;
      Font.Charset := DEFAULT_CHARSET; // seems to be necessary, lest EASTER_EUROPEAN charset got encoded and used on non-EE systems
      Initializing := true;
      Caption := Format( '%s %s', [Program_Name, Program_Version] );
    {$IFDEF KNT_DEBUG}
      Caption := Caption + ' (debug)';
    {$ENDIF}
      Application.Title := Caption;
      FolderMon.Active := false;
      Ntbk_ResFind.PageIndex := 1;

      ActiveKntFolder := nil;
      ClipCapNextInChain := 0;
      RTFUpdating := false;
      FileIsBusy := false;
      LastEvalExprResult := '';
      FileChangedOnDisk := false;
      TerminateClick := false;
      ClosedByWindows := false;
      AppIsClosing := false;
      LastGoTo := '';
      LastImportFilter := 1;

      //_Is_Dragging_Text := false;
      DraggingImageID:= 0;
      _WindowWidthIncToRestore := 0;
      _LastZoomValue := 100;
      Combo_Zoom.Text := '100%';

      s := GetFolderPath( fpPersonal );
      OpenDlg.InitialDir := s;
      SaveDlg.InitialDir := s;

      // Note: Compiled with Delphi 3, these shortcuts will not
      // be displayed with the menu item, although they will work.
      // Only Delphi 5 and higher can display these shortcuts.
      MMTreeFullExpand.Shortcut := ShortCut(VK_ADD, [ssShift]);
      MMTreeFullCollapse.Shortcut := ShortCut(VK_SUBTRACT, [ssShift]);
      MMInsertURL.Shortcut := 24665;   // Shift + Ctrl + Y

      Log_StoreTick( 'Begin init', 2 );

      ClipCapActive := false;
      ClipCapCRC32 := 0;
      ClipCapNote := nil;
      _ConvertHTMLClipboardToRTF:= true;
      ClosedOnPreviousInstance := false;
      OriginalComboLen := Combo_Font.Width;
      Pages.MarkedPage := nil;

      //_GLOBAL_URLText := '';                 // Can be set in ReadCmdLine, called from InitializeOptions
      _Global_Location := nil;
      _IS_CAPTURING_CLIPBOARD := false;
      _IS_CHAINING_CLIPBOARD := false;
      _IS_COPYING_TO_CLIPBOARD:= false;
      _IS_FAKING_MOUSECLICK := false;
      _REOPEN_AUTOCLOSED_FILE := false;

      TB_Color.AutomaticColor := clWindowText;
      TB_Hilite.AutomaticColor := clWindow; // to remove highlighting

      try                                     // [DPV]
         Form_Main.RichPrinter := TRichPrinter.Create(Form_Main);
      except
        On E : Exception do
        begin
          //showmessage( E.Message );
        end;
      end;

      with LastRTFKey do
      begin
        Key := 0;
        Shift := [];
        Special := false;
      end;

      MacroInitialize;

      // the menu item tags are occasionally used
      // for flow control, when several items ahre
      // the same onclick event
      LastStyleRange := srBoth;
      MSStyleBoth.Checked := true;
      MSStyleFont.Tag := ord( srFont );
      MSStylePara.Tag := ord( srParagraph );
      MSStyleBoth.Tag := ord( srBoth );

      MMTreeNavUp.Tag := ord( navUp );
      MMTreeNavDown.Tag := ord( navDown );
      MMTreeNavLeft.Tag := ord( navLeft );
      MMTreeNavRight.Tag := ord( navRight );

      TVPasteNodeName.Tag := ord( pnnClipboard );
      TVPasteNodeNameAsDate.Tag := ord( pnnDate );
      TVPasteNodeNameAsTime.Tag := ord( pnnTime );
      TVPasteNodeNameAsDateTime.Tag := ord( pnnDateTime );
      TVPasteNodeNameAsSel.Tag := ord( pnnSelection );

      MMTreeNodeNamePaste.Tag := ord( pnnClipboard );
      MMTreeNodeNameAsDate.Tag := ord( pnnDate );
      MMTreeNodeNameAsTime.Tag := ord( pnnTime );
      MMTreeNodeNameAsDateTime.Tag := ord( pnnDateTime );
      MMTreeNodeNameAsSel.Tag := ord( pnnSelection );

      MMViewFormatFont.Tag := ord( srFont );
      MMViewFormatPara.Tag := ord( srParagraph );
      MMViewFormatBoth.Tag := ord( srBoth );
      MMViewFormatNone.Tag := -1;

      MMFormatApplyStyle.Tag := ITEM_STYLE_APPLY;
      MSStyleApply.Tag := ITEM_STYLE_APPLY;
      MSStyleRename.Tag := ITEM_STYLE_RENAME;
      MSStyleDelete.Tag := ITEM_STYLE_DELETE;
      MSStyleRedef.Tag := ITEM_STYLE_REDEFINE;
      MSStyleDescribe.Tag := ITEM_STYLE_DESCRIBE;

      MMEditTrimLeft.Tag := ITEM_TAG_TRIMLEFT;
      MMEditTrimRight.Tag := ITEM_TAG_TRIMRIGHT;
      MMEditTrimBoth.Tag := ITEM_TAG_TRIMBOTH;

      TVRefreshVirtualNode.Enabled := false;
      TVUnlinkVirtualNode.Enabled := false;

      HotKeySuccess := false;
      RxRTFKeyProcessed := false;
      SearchNode_Text := '';
      SearchNode_TextPrev := '';

      Timer.Enabled := false;
      Timer_Tick := 0;
      Timer_TickAlarm:= 0;
      Timer.Interval := _TIMER_INTERVAL;


      // Register message ID for DLL (plugin) notifications
      _KNT_WINMSG_ID := RegisterWindowMessage( KeyNote_WinMsgIdStr );

      UAS_Window_Handle := 0;
      LAST_CASE_CYCLE := low( LAST_CASE_CYCLE );

      LastFileModifiedFlag := false;
      LastExportFilterIndex := 1;

      AppLastActiveTime := now;

      // set up application events
      Application.OnMinimize := AppMinimize;
      Application.OnRestore := AppRestore;
      Application.OnHint := DisplayAppHint;
      Application.OnException := ShowException;
      //Application.HelpFile := normalFN( changefileext( Application.ExeName, ext_HLP ));         //*1
      Application.HelpFile := normalFN( changefileext( Application.ExeName, ext_CHM ));
      OpenDlg.Filter := FILTER_NOTEFILES + '|' + FILTER_DARTFILES + '|' + FILTER_ALLFILES;

      AddSearchModes;
      AddSearchScopes;
      AddSearchChkModes;

      Form_Chars := nil;
      {                                    // Unncessary with TForm_CharsNew
      InsCharFont.Name := '';
      InsCharFont.Size := 0;
      InsCharFont.Charset := DEFAULT_CHARSET;
      }

      // [x] PRE-RELEASE FIXES
      // MMNotePrintPreview_.Visible := false;

      Log_StoreTick( 'End init', 2 );

      try
        {$IFDEF KNT_DEBUG}
        if opt_SaveMenus then
          SaveMenusAndButtons;
        {$ENDIF}
        if opt_SaveToolbars then
          SaveToolbars
        else
          LoadToolbars; // toolbar.ini

        {$IFDEF EXCLUDEEMAIL}
        MMNoteEmail.Visible := false;
        MMNoteEmail.Enabled := false;
        TB_EmailNote.Visible := false;
        TB_EmailNote.Enabled := false;
        {$ENDIF}


        OtherCommandsKeys:= TList.Create;
        LoadCustomKeyboard; // keyboard.ini

        if opt_NoReadOpt then
          opt_NoSaveOpt := true; // "no read" implies "no save"

        // set some options for which there is no UI
        Combo_Font.UseFonts := KeyOptions.ShowFonts;
        TB_Hilite.ActiveColor := KeyOptions.InitHiColor;
        TB_Color.ActiveColor := KeyOptions.InitFontColor;
        TB_AlarmMode.Down:= (not KeyOptions.DisableAlarmPopup);

      except
        on E : Exception do
        begin
         {$IFDEF KNT_DEBUG}
           Log.Add( 'Exception from ReadOptions:' + E.Message );
         {$ENDIF}
           PopupMessage( Format(STR_02, [e.Message]), mtInformation, [mbOK], 0 );
        end;
      end;

      SetupToolbarButtons;
      ResolveToolbarRTFv3Dependencies;

      MMArabicNumbers.Tag := ord( nsArabicNumbers );
      MMLoLetter.Tag := ord( nsLoCaseLetter );
      MMUpLetter.Tag := ord( nsUpCaseLetter );
      MMLoRoman.Tag := ord( nsLoCaseRoman );
      MMUpRoman.Tag := ord( nsUpCaseRoman );

      case KeyOptions.LastNumbering of
        nsLoCaseLetter : MMLoLetter.Checked := true;
        nsUpCaseLetter : MMUpLetter.Checked := true;
        nsLoCaseRoman : MMLoRoman.Checked := true;
        nsUpCaseRoman : MMUpRoman.Checked := true;
        else
          MMArabicNumbers.Checked := true;
      end;

      MMRightParenthesis.Tag := integer(nsParenthesis);
      MMEnclosed.Tag := integer(nsEnclosed);
      MMPeriod.Tag := integer(nsPeriod);
      MMOnlyNumber.Tag := integer(nsSimple);
      MMWithoutNextNumber.Tag := integer(nsNoNumber);
      MMStartsNewNumber.Tag := integer(nsNewNumber);

      case KeyOptions.LastNumberingStyle of
        nsParenthesis : MMRightParenthesis.Checked := true;
        nsEnclosed : MMEnclosed.Checked := true;
        nsSimple : MMOnlyNumber.Checked := true;
        else
          MMPeriod.Checked := true;
      end;


      Log_StoreTick( 'End config', 2 );

      // check other instance, and do the job is necessary
      if ( KeyOptions.SingleInstance and ( _OTHER_INSTANCE_HANDLE <> 0 )) then begin
        if KeyOptions.WarnSingleInstance then
           Messagedlg(STR_01, mtWarning, [mbOK], 0 );
        ClosedOnPreviousInstance := true;
        Application.ShowMainForm := false;
        try
          try
            ActivatePreviousInstance;

          except
            on E : Exception do begin
              showmessage( 'Error on ActivatePreviousInstance: ' + E.Message );
              Halt;
            end;
          end;
        finally
          // ShowWindow(Application.Handle, SW_HIDE);
          opt_NoSaveOpt := true; // do not save any config
          MRU.AutoSave := false; // MRU throws exception when trying to save here
          OnActivate := nil;
          OnDestroy := nil;
          OnClose := nil;
          OnCloseQuery := nil;
          PostMessage( Application.Handle, WM_QUIT, 0, 0 );
          // PostQuitMessage( 0 );
          // Application.Terminate;
        end;
        exit;
      end;

      Log_StoreTick( 'End instance check', 2 );

      opt_Debug := ( opt_Debug or KeyOptions.Debug );
      opt_NoRegistry := ( opt_NoRegistry or opt_Debug or KeyOptions.NoRegistry );

      if KeyOptions.ResolveLNK then
        OpenDlg.Options := OpenDlg.Options - [ofNoDereferenceLinks];

      const RegPath = 'Software\General Frenetics\KeyNote';

      if opt_NoRegistry then begin
        // don't clutter the registry with garbage file names and settings
        IniLoadToolbarPositions( Form_Main, MRU_FN, 'TB97a' );
        FormStorage.UseRegistry := false;
        FormStorage.IniFileName := MRU_FN;
        MRU.UseRegistry := false;
        MRU.AutoSaveName := MRU_FN;
        _FORMPOS_USE_REGISTRY := false;
        _FORMPOS_INIFILENAME := MRU_FN;
      end
      else begin
        RegLoadToolbarPositions( Form_Main, RegPath + '\FormPos\TB97a' );
        MRU.UseRegistry := true;
        MRU.AutoSaveName := '\' + RegPath;
        FormStorage.UseRegistry := true;
        FormStorage.IniFileName := RegPath + '\FormPos';
        _FORMPOS_USE_REGISTRY := true;
        _FORMPOS_INIFILENAME := FormStorage.IniFileName;
      end;

      Log_StoreTick( 'End Toolbars', 2 );

    {$IFDEF KNT_DEBUG}
      Log.Active := opt_Debug;
      if not opt_Debug then
         Log.Flush(false);
      Log.AppendToFile := KeyOptions.DebugLogAppend;
    {$ENDIF}

      if opt_Debug then begin
        debugmenu := TMenuItem.Create( Form_Main );
        debugmenu.Caption := '&Debug Information';
        debugmenu.OnClick := DebugMenuClick;
        MMHelp_.Add( debugmenu );
        Splitter_Res.Color := clLime;
      end;

      AssociateKeyKntFile;

      {
      opt_RegExt := ( opt_RegExt or KeyOptions.AutoRegisterFileType );
      if opt_RegExt then
      begin
        s := GetAppFromExt( ext_KeyNote, true );
        if ( s <> '' ) then
          s := #13#13 + 'Current association is: ' +#13+ normalFN( s );
        if (( not KeyOptions.AutoRegisterPrompt ) or
           ( messagedlg( 'Register ' + Program_Name + ' file type (' + ext_KeyNote + ')?' + s, mtConfirmation, [mbYes,mbNo], 0 ) = mrYes )) then
        begin
          try
            AssociateKeyKntFile;
            if KeyOptions.AutoRegisterPrompt then
              messagedlg( Program_Name + ' data file type association created:' +#13+ GetAppFromExt( ext_KeyNote, true ), mtInformation, [mbOK], 0 );
          except
            on E : Exception do
            begin
              PopupMessage( 'There was an error while creating file type association: ' + e.Message + #13#13 + 'The application will now continue.', mtInformation, [mbOK], 0 );
            end;
          end;
        end;
      end;
      }

      HotKeyProc( true );

      Log_StoreTick( 'End hotkey and file assoc', 2 );

      if opt_SaveDefaultIcons then
        SaveDefaultBitmaps; // for developer only

      try
        if ( not opt_NoReadOpt ) then
          LoadFileManagerInfo( MGR_FN );

        // load user icons from "keynote.icn" or
        // load default icon from resource in keynote.exe
        Log_StoreTick( 'End FileMgr', 2);
        LoadTabImages( true );

      except
        On E : Exception do
          showmessage( E.Message );
      end;

      Log_StoreTick( 'End tabicons', 2 );

      // we now have all options set, so apply them
      UpdateFormState;
      UpdateTabState;
      UpdateStatusBarState;
      UpdateResPanelState;

      TrayIcon.Hint := ' Loading file...';
      Application.ProcessMessages; // let user see we're busy workin'

      Log_StoreTick( 'End formupdate', 2 );

      try
        if ( not opt_NoReadOpt ) then begin
          LoadStyleManagerInfo( Style_FN );
          if assigned( StyleManager ) then begin
            StyleManagerToCombo;
            if ( Combo_Style.Items.Count > 0 ) then
              Combo_Style.ItemIndex := 0;
          end;
        end;
      except
        On E : Exception do
          showmessage( 'Error loading Style Manager: ' + E.Message );
      end;

      Log_StoreTick( 'End stylemgr', 2 );


      if ( not opt_NoReadOpt ) then
         kn_Glossary.LoadGlossaryInfo;


      Log_StoreTick( 'End glossary', 2 );

      if FirstTimeRun then begin
        if ( KntFileToLoad = '' ) then begin
          // our INI file was not found, so we're probably being used 1st time
          // after installation. Since no .KNT file was specified, let's show
          // the sample file which is part of the distribution.
          KntFileToLoad := NormalFN( extractfilepath( Application.ExeName ) + SampleFileName );
          if ( not FileExists( KntFileToLoad )) then
            KntFileToLoad := '';
        end;
      end
      else
        // have we been upgraded?
        NewVersionInformation;

      if ( KntFileToLoad = '' ) then begin
        if ( KeyOptions.LoadUserFile and ( KeyOptions.UserFile <> '' )) then
          KntFileToLoad := KeyOptions.UserFile
        else begin
          if ( KeyOptions.LoadLastFile and ( KeyOptions.LastFile <> '' )) then
            KntFileToLoad := KeyOptions.LastFile;
        end;
      end;

      if ( KntFileToLoad <> '' ) then begin
        KntFileToLoad:= GetAbsolutePath(ExtractFilePath(Application.ExeName), KntFileToLoad);

        if ( KntFileOpen( KntFileToLoad ) <> 0 ) then begin
          if KeyOptions.AutoNewFile then
            KntFileNew( 'untitled' );
        end;
      end
      else
        if KeyOptions.AutoNewFile then
          KntFileNew( '' );

      Log_StoreTick( 'FormCreate - END', 0, -1 );


      Timer.Enabled := true;
      FolderMon.OnChange := FolderMonChange;

      if opt_Debug then begin
        // StoreMenuItemIDs;
        // SaveKBD( KBD_FN, KNTMainMenuCmds, KNTTreeMenuCmds ); // in kn_KBD.pas
      end;
  end;

end; // CREATE


// Called from keynote.dpr, after call InitializeOptions (also in kn_Global)
procedure LoadRicheditLibrary;
begin
    _LoadedRichEditVersion:= LoadRichEditDLL(RichEditLibraryPath);
end;

// We want to read LanguageUI before creating Form_Main
procedure InitializeOptions;
var
   Path, ExeFilePath, DefaultProfileFolder: string;
begin
      FirstTimeRun := false;

      KntFileToLoad := '';
      CmdLineFileName := '';

      DEF_FN := '';
      MGR_FN := '';
      ICN_FN := '';
      MRU_FN := '';
      //KEY_FN := '';
      Keyboard_FN := '';

      ExeFilePath:= ExtractFilePath(Application.ExeName);
      DefaultProfileFolder:= ExeFilePath + _DEFAULT_PROFILE_FOLDER;

      if TDirectory.Exists(DefaultProfileFolder) then
         INI_FN := DefaultProfileFolder + normalFN( changefileext( ExtractFileName(Application.ExeName), ext_INI ))
      else begin
         INI_FN := normalFN( changefileext( Application.ExeName, ext_INI ));
         DefaultProfileFolder:= ExeFilePath;
      end;

      Help_FN := ExeFilePath + _KNT_HELP_FILE;
      HelpINI_FN := ExeFilePath + _HELP_PROFILE_FOLDER + 'keynote.ini';
      Launcher_FN := ExeFilePath + _KNT_LAUNCHER;

    {$IFDEF KNT_DEBUG}
      // This is always located in .exe directory
      LOG_FN := normalFN( changefileext( Application.ExeName, ext_LOG ));

      Log := TGFLog.Create( Form_Main );
      Log.MaxLines := MAX_LOG_LINES;
      Log.DateStamp:= false;
      Log.FileName := LOG_FN;
    {$ENDIF}

      opt_Minimize := false;
      //opt_Setup := false;
      opt_Debug := false;
      opt_NoRegistry := false;
      opt_NoReadOpt := false;
      opt_NoSaveOpt := false;
      opt_NoDefaults := false;
      opt_RegExt := false;
      opt_SaveDefaultIcons  := false;
      opt_NoUserIcons := false;
      opt_SaveToolbars := false;
      opt_SaveMenus := false;
      opt_DoNotDisturb:= false;
      opt_Title:= '';

      opt_Clean := false;

      // set up default values for all config options
      InitializeKeyOptions( KeyOptions );
      InitializeTabOptions( TabOptions );
      InitializeFindOptions( FindOptions );
      InitializeClipOptions( ClipOptions );
      InitializeEditorOptions( EditorOptions );
      InitializeResPanelOptions( ResPanelOptions );

      InitializeChrome( DefaultEditorChrome );
      InitializeFolderEditorProperties( DefaultEditorProperties );
      InitializeFolderTabProperties( DefaultTabproperties );

      InitializeChrome( DefaultTreeChrome );
      InitializeTreeOptions( TreeOptions );
      InitializeFolderTreeProperties( DefaultTreeProperties );
      //_OLD_NOTE_NAME := DEFAULT_NEW_NOTE_NAME;

      _GLOBAL_URLText := '';
      ReadCmdLine;

      // Adjust location of all config files to that of the INI file
      // (alternate INI file may have been given on command line)

      Path:= ExtractFilePath(INI_FN);

      if ( MRU_FN = '' ) then
        MRU_FN := changefileext( INI_FN, ext_MRU );
      {if ( KEY_FN = '' ) then
        KEY_FN := changefileext( INI_FN, ext_Key );   }
      if ( ICN_FN = '' ) then
        ICN_FN := changefileext( INI_FN, ext_ICN );
      if ( DEF_FN = '' ) then
        DEF_FN := changefileext( INI_FN, ext_DEFAULTS );
      FAV_FN := changefileext( INI_FN, ext_Favorites );
      OrigDEF_FN := DEF_FN;
      if ( MGR_FN = '' ) then
        MGR_FN := changefileext( INI_FN, ext_MGR );
      Style_FN := changefileext( INI_FN, ext_Style );
      Glossary_FN := changefileext( INI_FN, ext_Expand );
      Scratch_FN :=  Path + 'scratch.rtf';
      Toolbar_FN :=  Path + ToolbarFileName;
      Keyboard_FN := Path + KeyboardFileName;

{$IFNDEF EXCLUDEEMAIL}
      MailINI_FN  := Path + 'keymail' + ext_INI;
{$ENDIF}


      NoteHeadingTpl_FN:= Path + 'notehead.rtf';
      NodeHeadingTpl_FN:= Path + 'nodehead.rtf';
      if DefaultProfileFolder <> Path then begin
         if not TFile.Exists(NoteHeadingTpl_FN) then
            NoteHeadingTpl_FN:= DefaultProfileFolder + 'notehead.rtf';
         if not TFile.Exists(NodeHeadingTpl_FN) then
            NodeHeadingTpl_FN:= DefaultProfileFolder + 'nodehead.rtf';
      end;


      if ( StartupMacroFile = '' ) then // was not given on commandline
        StartupMacroFile := _MACRO_AUTORUN_STARTUP;

      Plugin_Folder := properfoldername( ExeFilePath + _PLUGIN_FOLDER );

      try
        ReadOptions; // keynote.ini (this is decalred in kn_INI.pas)
      except
        on E : Exception do begin
         {$IFDEF KNT_DEBUG}
           Log.Add( 'Exception from ReadOptions:' + E.Message );
         {$ENDIF}
           PopupMessage(Format(STR_02, [e.Message]), mtInformation, [mbOK], 0 );
        end;
      end;

end;


function ActiveKeyNoteHelp(Folder, Node, Marker: integer): Boolean;
var
  Args: string;
  sMarker: string;
begin
   try
      //  file:///*FolderID|NodeID|CursorPosition|SelectionLength|MarkID  -> Ex: file:///*3|16|5|0|1

	  sMarker:= '';
      if Marker > 0 then
         sMarker:= '|0|0|' + Marker.ToString;

      Args:= Format('"%s" "%s" -jmp"file:///*%d|%d%s" -title"%s"', [Help_FN, HelpINI_FN, Folder, Node, sMarker, _KNT_HELP_TITLE]);
      ShellExecute( 0, 'open', PChar(Launcher_FN), PChar(Args), nil, SW_HIDE );

   except
   end;
end;


function ActiveKeyNoteHelp(Node: integer): Boolean;
begin
   ActiveKeyNoteHelp(_KNT_HELP_FILE_NOTE_ID, Node, 0);
end;


function ActiveKeyNoteHelp(Node_Marker: PChar): Boolean;
var
  Node, Marker: integer;
  p: integer;
begin
   // Node_Marker: "<Node>-<Marker>".
   // Ex: "479-5"  ->  Using Find and Find All [479] / Find All: Marker:5
   p:= Pos('-', Node_Marker);
   if p = 0 then exit;

   Node:=   StrToIntDef(Copy(Node_Marker, 1, p-1), _KNT_HELP_FILE_DEFAULT_NODE_ID);
   Marker:= StrToIntDef(Copy(Node_Marker, p+1), 0);

   ActiveKeyNoteHelp(_KNT_HELP_FILE_NOTE_ID, Node, Marker);
end;


function ActiveKeyNoteHelp_FormHelp(Command: Word; Data: NativeInt): Boolean;
begin
   if Command = HELP_CONTEXT then
      ActiveKeyNoteHelp (Data)             // Node
   else
      ActiveKeyNoteHelp (PChar(Data));     // Node_Marker
end;



{
 When compiling without the KNT_DEBUG compilation constant, the Log_StoreTick and Log_Flush methods are defined as inline
 and they won't do anything.
 Calls like the following will not generate code, even if they are not inside a conditional compile block like this:
   StoreTick( 'string', 2);
   StoreTick( 'string' + s, 2);

 Calls like the following do will generate code:

   StoreTick( 'string', 2 * 2);
   StoreTick( 'string' + intToStr(3));
   StoreTick( 'string', GetTickCount);    // If any procedure is called as a parameter, code will be generated, due to possible side effects.
}

procedure Log_StoreTick(const Msg : string; const DbgLevel: integer= 0; DetailLevel: integer = 0);
begin
 {$IFDEF KNT_DEBUG}
    Log.StoreTick(Msg, DbgLevel, DetailLevel);
 {$ENDIF}
end;


procedure Log_Flush;
begin
 {$IFDEF KNT_DEBUG}
    Log.Flush(True);
 {$ENDIF}
end;


(*
{$IFDEF WITH_TIMER}
procedure StoreTick( const Msg : string; const Tick : integer );
const
  tab = #9;
var
  Duration : integer;
begin
  Duration := Tick - LastTick;
  TickList.Add( Format(
    '%s:%s%d',
    [Msg, tab, Duration]
  ));
  LastTick := Tick;
end; // StoreTick

procedure SaveTicks;
const
  tab = #9;
var
  fn : string;
  TotalTicks : integer;

begin
  TotalTicks := GetTickCount - AppStartTime;
  TickList.Add( Format(
    'TOTAL DURATION in miliseconds:%s%d',
    [tab, TotalTicks]
  ));

  fn := makevalidfilename( DateTimeToStr( now ), 127 );

  TickList.SaveToFile( lowercase(
    extractfilepath( application.exename )) +
    'keynote_ticks_' + fn + '.txt' );
  TickList.Clear;
end; // SaveTicks

{$ENDIF}
*)

end.
