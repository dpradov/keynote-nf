unit kn_Global;

{.$DEFINE MJ_DEBUG}

interface
uses
  Windows, SysUtils, Dialogs,
  TreeNT, RxRichEd,
  gf_files,  gf_strings,
  kn_Cmd,
  kn_Info,
  kn_NoteObj, kn_FileObj,kn_NodeList,
  FrmFindReplace,
  kn_Chars,
  kn_Macro,
  kn_LocationObj,
  kn_Main,
  RichPrint,
  {$IFDEF MJ_DEBUG}
  GFLog,
  {$ENDIF}
  kn_AlertMng,
  UWebBrowserWrapper;


const
  _TIMER_INTERVAL = 10000; // ten seconds

   procedure InitializeKeynote (Form_Main: TForm_Main);
   procedure InitializeOptions;
   procedure AddSearchModes;

type
  WideException = class(Exception)
  private
    FWideMessage: WideString;
    function GetMessage: WideString;
    procedure SetMessage(value: WideString);
  public
    constructor Create(const Msg: WideString);
    property Message: WideString read GetMessage write SetMessage;
  end;

  procedure CommunicateException(E: Exception; DlgType: TMsgDlgType= mtWarning; Buttons: TMsgDlgButtons = [mbOK]);
  function GetMessage(E: Exception): wideString;

var

    NoteFile : TNoteFile; // main data structure
    ActiveNote : TTabNote; // the note that is currently visible (can be nil)

     //======================================= FILES
    INI_FN : string; // main keynote.ini file (alternate filename may be given on command line)
    MRU_FN : string; // MRU file list and form position/size info
    TIP_FN : string; // tip of the day file
    DEF_FN : string; // defaults for new notes
    LOG_FN : string; // main log file (unused unless built with MJ_DEBUG)
    MGR_FN : string; // file manager data
    ICN_FN : string; // custom icons
    KEY_FN : string; // keyboard customization file (old - via plugin)
    FAV_FN : string; // favorites storage file
    Scratch_FN  : string; // scratchpad filename
    Style_FN    : string; // custom styles
    Toolbar_FN  : string; // toolbar configuration file
    Keyboard_FN : string; // keyboard customization file (NEW - all menu items)
    OrigDEF_FN : string;
    MailINI_FN : string; // INI file for email options (keymail.ini)

    //================================================== OPTIONS
    { These options are seperate from KeyOptions, because then
      may also be set via commandline. Basically, the logic is:
      opt_XXX := ( commandline_argument_XXX OR inifile_options_XXX );
    }
    opt_Minimize : boolean; // minimize on startup
    opt_Setup : boolean; // run setup (OBSOLETE, unused)
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

    //================================================== COMMAND LINE
    NoteFileToLoad : wideString; // name of KNT file we are supposed to open (options + commandline + passed from other instance, etc)
    CmdLineFileName : wideString; // other filename passed on command line (macro, plugin, etc)


    //================================================== KEYBOARD / HOTKEY
    HotKeySuccess : boolean; // if true, we registered the hotkey successully, so we will remember to unregister it when we shut down
    AltFKeys : TFuncKeys;      // primitive, but that's all we can do for now (0.999)
    ShiftAltFKeys : TFuncKeys; // these records keep custom assignments for Alt, Shift+Alt and Ctrl+Alt function key combos.
    CtrlAltFKeys : TFuncKeys;  // They can be modified manually (keynote.key) or by using the FUNCKEY plugin.

    LastRTFKey : TKeyCode;
    RxRTFKeyProcessed : boolean; // for TAB handling; some tabs are eaten by TRichEdit, others must not be
    RTFUpdating : boolean; // TRUE while in RxRTFSelectionChange; some things cannot be done during that time


    //==================================================
    InsCharFont : TFontInfo;
    Form_Chars : TForm_Chars; // GLOBAL FORM!


    //================================================== DEFAULT PROPERTIES
    DefaultEditorProperties : TNoteEditorProperties;
    DefaultTabProperties : TNoteTabProperties;
    DefaultEditorChrome : TChrome;
    DefaultTreeChrome : TChrome;
    DefaultTreeProperties : TNoteTreeProperties;


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
    {$IFDEF WITH_TIMER}
    ThisTick, LastTick : integer;
    TickList : TStringList;
    {$ENDIF}


    //================================================== CLIPBOARD
    _IS_CAPTURING_CLIPBOARD : boolean;
    _IS_CHAINING_CLIPBOARD : boolean;
    ClipCapNextInChain : HWnd;
    LastEvalExprResult : string; // remembered, so that we can paste it

    ClipCapActive : boolean; // TRUE if we have a clipboard capture note
    ClipCapNode : TNoteNode;
    ClipCapCRC32 : DWORD;

    AppIsActive : boolean; // used with Clipboard Capture to ignore copy events coming from Keynote itself
    _ConvertHTMLClipboardToRTF: boolean;

    //================================================== TREE
    MovingTreeNode: TTreeNTNode;            // To use with Paste, after applying Cut on a Tree Node.
    CopyCutFromNoteID: integer;                 // Copy or Cut from Note

    //================================================== VARIOS

    _GLOBAL_URLText : wideString;
    _IS_FAKING_MOUSECLICK : boolean;
    _Global_Location : TLocation;
    _REOPEN_AUTOCLOSED_FILE : boolean;
    _Is_Dragging_Text : boolean;
    _LastZoomValue : integer;
    NumberingStart: integer;

    OriginalComboLen : integer;

    UAS_Window_Handle : HWND;
    LAST_CASE_CYCLE : TCaseCycle;

    _SYSTEM_IS_WIN95 : boolean;
    _SYSTEM_IS_WINXP : boolean;

    AlarmManager: TAlarmManager;    // [dpv]
    _DllHandle : THandle;
    _IE: TWebBrowserWrapper;


    {$IFDEF MJ_DEBUG}
    Log : TGFLog;
    {$ENDIF}


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

implementation
uses Classes, Messages, Graphics, Forms, Menus, Controls,
     TB97, TntSysUtils,  //RxRichEd,
     gf_const, gf_misc, gf_miscvcl,
     kn_const, kn_msgs, kn_ini, kn_ExpandObj, kn_plugins, kn_fileMgr,
     kn_StyleObj, kn_Chest,
     kn_ConfigFileMng, kn_MacroMng, kn_FindReplaceMng,
     kn_TemplateMng, kn_StyleMng, kn_NoteFileMng, kn_VCLControlsMng;


//-----------------
constructor WideException.Create(const Msg: wideString);
begin
  SetMessage(Msg);
end;

function WideException.GetMessage: WideString;
begin
  if FWideMessage <> '' then
     Result:= FWideMessage
  else
     Result:= inherited Message;
end;

procedure WideException.SetMessage(value: WideString);
begin
    FWideMessage:= value;
    inherited Message:= value;
end;

procedure CommunicateException(E: Exception; DlgType: TMsgDlgType= mtWarning; Buttons: TMsgDlgButtons = [mbOK]);
begin
  if E is WideException then
     DoMessageBox( WideException(E).Message, DlgType, Buttons )
  else
     DoMessageBox( E.Message, DlgType, Buttons );
end;

function GetMessage(E: Exception): wideString;
begin
  if E is WideException then
     Result:= WideException(E).Message
  else
     Result:= E.Message;
end;


//-----------------


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

  with Form_Main do begin
      {$IFDEF WITH_TIMER}
      TickList := TStringList.Create;
      LastTick := AppStartTime;
      StoreTick( 'App start', AppStartTime );
      StoreTick( 'Begin FormCreate', GetTickCount );
      {$ENDIF}

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
      Caption := Format( '%s %s', [Program_Name, Program_VerStr] );
      {$IFDEF MJ_DEBUG}
      Caption := Caption + ' (debug)';
      {$ENDIF}
      Application.Title := Caption;
      FolderMon.Active := false;
      Ntbk_ResFind.PageIndex := 1;

      ActiveNote := nil;
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

      _Is_Dragging_Text := false;
      _LastZoomValue := 100;

      s := GetFolderPath( fpPersonal );
      OpenDlg.InitialDir := s;
      SaveDlg.InitialDir := s;

      // Note: Compiled with Delphi 3, these shortcuts will not
      // be displayed with the menu item, although they will work.
      // Only Delphi 5 and higher can display these shortcuts.
      MMTreeFullExpand.Shortcut := ShortCut(VK_ADD, [ssShift]);
      MMTreeFullCollapse.Shortcut := ShortCut(VK_SUBTRACT, [ssShift]);

      {$IFDEF WITH_TIMER}
      StoreTick( 'Begin init', GetTickCount );
      {$ENDIF}

      ClipCapActive := false;
      ClipCapCRC32 := 0;
      ClipCapNode := nil;
      _ConvertHTMLClipboardToRTF:= true;
      ClosedOnPreviousInstance := false;
      OriginalComboLen := Combo_Font.Width;
      Pages.MarkedPage := nil;

      _GLOBAL_URLText := '';
      _Global_Location := nil;
      _IS_CAPTURING_CLIPBOARD := false;
      _IS_CHAINING_CLIPBOARD := false;
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

      Combo_Zoom.Text := '100%';

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

      Form_Chars := nil;
      InsCharFont.Name := '';
      InsCharFont.Size := 0;
      InsCharFont.Charset := DEFAULT_CHARSET;

      // [x] PRE-RELEASE FIXES
      // MMNotePrintPreview_.Visible := false;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End init - Begin config', GetTickCount );
      {$ENDIF}

      try
        if opt_SaveMenus then
          SaveMenusAndButtons;
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


        LoadCustomKeyboard; // keyboard.ini
        ReadFuncKeys; // keynote.key - will override menu shortcuts

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
          {$IFDEF MJ_DEBUG}
          Log.Add( 'Exception from ReadOptions:' + E.Message );
          {$ENDIF}
          PopupMessage( 'There was a non-fatal error while loading program configuration: ' + #13 + e.Message + #13#13 + 'Some options may have been reset to factory default values. The application will now continue.', mtInformation, [mbOK], 0 );
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


      {$IFDEF WITH_TIMER}
      StoreTick( 'End config - Begin instance check', GetTickCount );
      {$ENDIF}

      // check other instance, and do the job is necessary
      if ( KeyOptions.SingleInstance and ( _OTHER_INSTANCE_HANDLE <> 0 )) then
      begin
        Messagedlg( 'KeyNote NF have been configured to allow only one instance at a time' + #13 + 'Closing this instance...', mtWarning, [mbOK], 0 );
        ClosedOnPreviousInstance := true;
        Application.ShowMainForm := false;
        try
          try
            ActivatePreviousInstance;

          except
            on E : Exception do
            begin
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

      {$IFDEF WITH_TIMER}
      StoreTick( 'End instance check - Begin toolbars', GetTickCount );
      {$ENDIF}

      opt_Debug := ( opt_Debug or KeyOptions.Debug );
      opt_NoRegistry := ( opt_NoRegistry or opt_Debug or KeyOptions.NoRegistry );

      if KeyOptions.ResolveLNK then
        OpenDlg.Options := OpenDlg.Options - [ofNoDereferenceLinks];

      if opt_NoRegistry then
      begin
        // don't clutter the registry with garbage file names and settings
        IniLoadToolbarPositions( Form_Main, MRU_FN, 'TB97a' );
        FormStorage.UseRegistry := false;
        FormStorage.IniFileName := MRU_FN;
        MRU.UseRegistry := false;
        MRU.AutoSaveName := MRU_FN;
        _FORMPOS_USE_REGISTRY := false;
        _FORMPOS_INIFILENAME := MRU_FN;
      end
      else
      begin
        RegLoadToolbarPositions( Form_Main, 'Software\General Frenetics\Keynote\FormPos\TB97a' );
        MRU.UseRegistry := true;
        MRU.AutoSaveName := '\Software\General Frenetics\KeyNote';
        FormStorage.UseRegistry := true;
        FormStorage.IniFileName := 'Software\General Frenetics\KeyNote\FormPos';
        _FORMPOS_USE_REGISTRY := true;
        _FORMPOS_INIFILENAME := FormStorage.IniFileName;
      end;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End Toolbars - Begin hotkey and file assoc', GetTickCount );
      {$ENDIF}

      {$IFDEF MJ_DEBUG}
      Log := TGFLog.Create( Form_Main );
      with Log do
      begin
        FileName := LOG_FN;
        MaxLines := MAX_LOG_LINES;
        AppendToFile := KeyOptions.DebugLogAppend;
        DeactivateOnError := false;
      end;
      Log.Active := opt_Debug;
      {$ENDIF}

      if opt_Debug then
      begin
        debugmenu := TMenuItem.Create( Form_Main );
        debugmenu.Caption := '&Debug Information';
        debugmenu.OnClick := DebugMenuClick;
        MMHelp_.Add( debugmenu );
        Splitter_Res.Color := clLime;
      end;

      AssociateKeyNoteFile;

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
            AssociateKeyNoteFile;
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

      {$IFDEF WITH_TIMER}
      StoreTick( 'End hotkey and file assoc - Begin FileMgr', GetTickCount );
      {$ENDIF}

      if opt_SaveDefaultIcons then
        SaveDefaultBitmaps; // for developer only

      try
        if ( not opt_NoReadOpt ) then
          LoadFileManagerInfo( MGR_FN );

        // load user icons from "keynote.icn" or
        // load default icon from resource in keynote.exe
        {$IFDEF WITH_TIMER}
        StoreTick( 'End FileMgr - Begin tabicons', GetTickCount );
        {$ENDIF}
        LoadTabImages( true );

      except
        On E : Exception do
        begin
          showmessage( E.Message );
        end;
      end;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End tabicons - Begin formupdate', GetTickCount );
      {$ENDIF}

      // we now have all options set, so apply them
      UpdateFormState;
      UpdateTabState;
      UpdateStatusBarState;
      UpdateResPanelState;

      TrayIcon.Hint := ' Loading file...';
      Application.ProcessMessages; // let user see we're busy workin'

      {$IFDEF WITH_TIMER}
      StoreTick( 'End formupdate - Begin stylemgr', GetTickCount );
      {$ENDIF}

      try
        if ( not opt_NoReadOpt ) then
        begin
          LoadStyleManagerInfo( Style_FN );
          if assigned( StyleManager ) then
          begin
            StyleManagerToCombo;
            if ( Combo_Style.Items.Count > 0 ) then
              Combo_Style.ItemIndex := 0;
          end;
        end;
      except
        On E : Exception do
        begin
          showmessage( 'Error loading Style Manager: ' + E.Message );
        end;
      end;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End stylemgr - Begin glossary', GetTickCount );
      {$ENDIF}

      try
        if ( not opt_NoReadOpt ) then
          LoadGlossaryInfo( Glossary_FN );
      except
        On E : Exception do
        begin
          showmessage( 'Error loading Glossary list: ' + E.Message );
          GlossaryList := nil;
        end;
      end;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End glossary - Begin FileOpen', GetTickCount );
      {$ENDIF}

      if FirstTimeRun then
      begin
        if ( NoteFileToLoad = '' ) then
        begin
          // our INI file was not found, so we're probably being used 1st time
          // after installation. Since no .KNT file was specified, let's show
          // the sample file which is part of the distribution.
          NoteFileToLoad := NormalFN( extractfilepath( Application.ExeName ) + SampleFileName );
          if ( not Widefileexists( NoteFileToLoad )) then
            NoteFileToLoad := '';
        end;
      end
      else
      begin
        // have we been upgraded?
        NewVersionInformation;
      end;

      if ( NoteFileToLoad = '' ) then
      begin
        if ( KeyOptions.LoadUserFile and ( KeyOptions.UserFile <> '' )) then
        begin
          NoteFileToLoad := KeyOptions.UserFile;
        end
        else
        begin
          if ( KeyOptions.LoadLastFile and ( KeyOptions.LastFile <> '' )) then
            NoteFileToLoad := KeyOptions.LastFile;
        end;
      end;

      if ( NoteFileToLoad <> '' ) then
      begin
        NoteFileToLoad:= GetAbsolutePath(WideExtractFilePath(Application.ExeName), NoteFileToLoad);

        if ( NoteFileOpen( NoteFileToLoad ) <> 0 ) then
        begin
          // if ( PopupMessage( 'Would you like to create a new Note file?', mtConfirmation, [mbYes,mbNo], 0 ) = mryes ) then
          if KeyOptions.AutoNewFile then
            NoteFileNew( 'untitled' );
        end;
      end
      else
      begin
        if KeyOptions.AutoNewFile then
          NoteFileNew( '' );
      end;

      {$IFDEF WITH_TIMER}
      StoreTick( 'End FileOpen - End FormCreate', GetTickCount );
      {$ENDIF}


      Timer.Enabled := true;
      FolderMon.OnChange := FolderMonChange;

      {$IFDEF MJ_DEBUG}
      Log.Add( 'Exiting CREATE' );
      {$ENDIF}

      if opt_Debug then
      begin
        // StoreMenuItemIDs;
        // SaveKBD( KBD_FN, KNTMainMenuCmds, KNTTreeMenuCmds ); // in kn_KBD.pas
      end;
  end;

end; // CREATE

// We want to read LanguageUI before creating Form_Main
procedure InitializeOptions;
begin
      FirstTimeRun := false;

      NoteFileToLoad := '';
      CmdLineFileName := '';

      DEF_FN := '';
      MGR_FN := '';
      ICN_FN := '';
      MRU_FN := '';
      KEY_FN := '';
      Keyboard_FN := '';

      INI_FN := normalFN( changefileext( Application.ExeName, ext_INI ));

      // This is always located in .exe directory
      LOG_FN := normalFN( changefileext( Application.ExeName, ext_LOG ));

      opt_Minimize := false;
      opt_Setup := false;
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

      opt_Clean := false;

      // set up default values for all config options
      InitializeKeyOptions( KeyOptions );
      InitializeTabOptions( TabOptions );
      InitializeFindOptions( FindOptions );
      InitializeClipOptions( ClipOptions );
      InitializeEditorOptions( EditorOptions );
      InitializeResPanelOptions( ResPanelOptions );

      InitializeChrome( DefaultEditorChrome );
      InitializeNoteEditorProperties( DefaultEditorProperties );
      InitializeNoteTabProperties( DefaultTabproperties );

      InitializeChrome( DefaultTreeChrome );
      InitializeTreeOptions( TreeOptions );
      InitializeNoteTreeProperties( DefaultTreeProperties );
      //_OLD_NODE_NAME := DEFAULT_NEW_NODE_NAME;

      ReadCmdLine;

      // Adjust location of all config files to that of the INI file
      // (alternate INI file may have been given on command line)
      if ( MRU_FN = '' ) then
        MRU_FN := changefileext( INI_FN, ext_MRU );
      if ( KEY_FN = '' ) then
        KEY_FN := changefileext( INI_FN, ext_Key );
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
      Scratch_FN := extractfilepath( INI_FN ) + 'scratch.rtf';
      Toolbar_FN := extractfilepath( INI_FN ) + ToolbarFileName;
      Keyboard_FN := extractfilepath( INI_FN ) + KeyboardFileName;

      MailINI_FN := extractfilepath( INI_FN ) + 'keymail' + ext_INI;

      if ( StartupMacroFile = '' ) then // was not given on commandline
        StartupMacroFile := _MACRO_AUTORUN_STARTUP;

      Plugin_Folder := properfoldername( extractfilepath( application.exename ) + _PLUGIN_FOLDER );

      try
        ReadOptions; // keynote.ini (this is decalred in kn_INI.pas)
      except
        on E : Exception do
        begin
          {$IFDEF MJ_DEBUG}
          Log.Add( 'Exception from ReadOptions:' + E.Message );
          {$ENDIF}
          PopupMessage( 'There was a non-fatal error while loading program configuration: ' + #13 + e.Message + #13#13 + 'Some options may have been reset to factory default values. The application will now continue.', mtInformation, [mbOK], 0 );
        end;
      end;

end;

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

end.
