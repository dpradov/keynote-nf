(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <marekjed@pobox.com> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

unit kn_Info;
{.$DEFINE DLA_DUNI}

interface
uses Windows, Messages, Classes, Graphics, SysUtils,
  RxRichEd, langs, gf_misc, gf_Const, kn_Const;

const
  Program_Name     = 'KeyNote NF';
  Program_VerStr   = '1.7.4 (rev.2)';
  Program_Version  = '1.7.4';  // Used in program caption
  Program_License  = 'Freeware / Open Source';
  {$IFDEF DLA_DUNI}
  Program_Desc     = 'Wersja specjalna dla mojej Ukochanej';
  {$ELSE}
  Program_Desc     = 'Tabbed notebook for Windows 95/98/NT/2000/XP';
  {$ENDIF}
  {
  // these are now stored in GF_Const.pas
  Program_License  = 'This program is Freeware';
  Program_Credit   = 'Copyright (c) General Frenetics, Discorp., 2000';
  Program_Email    = 'eristic@lodz.pdi.net';
  Program_URL      = 'http://www.lodz.pdi.net/~eristic/free/';
  }

{$IFDEF WITH_TIMER}
const
  AppStartTime : integer = 0;
{$ENDIF}

const
  ext_KeyNote    = '.knt'; // KeyNote data file
  ext_Encrypted  = '.kne'; // KeyNote encrypted file
  ext_DEFAULTS   = '.def'; // KeyNote default storage
  ext_MGR        = '.mgr'; // KeyNote file manager
  ext_RTF        = '.rtf';
  ext_Plugin     = '.knl'; // plugin files
  ext_PluginInfo = '.pli'; // plugin information files
  ext_MACRO      = '.knm'; // macro files
  ext_Key        = '.key'; // custom keys for macros, plugins, etc.
  ext_KBD        = '.kbd'; // keyboard customization file
  ext_HTML       = '.html';
  mask_HTML      = '.htm*';
  ext_BAK        = '.bak'; // backup files
  ext_TEMP       = '.k$$'; // temporary files
  ext_Expand     = '.exp'; // Expand Text list
  ext_TXT        = '.txt';
  ext_HLP        = '.hlp'; // Help
  ext_CHM        = '.chm'; // Help       *1
  ext_INI        = '.ini'; // INI file
  ext_LOG        = '.log'; // log file
  ext_ICN        = '.icn'; // custom icons storage
  ext_ADR        = '.adr'; // email addressbook file
  ext_SIG        = '.sig'; // email signature file
  ext_TIP        = '.tip'; // tips of the days
  ext_MRU        = '.mru'; // recently used files; form size & position
  ext_Style      = '.kns'; // style definition file
  ext_DART       = '.n_text'; // DART Notes
  ext_TreePad    = '.hjt'; // ext. for TreePad files
  ext_Favorites  = '.fvr'; // Favorites list
  ext_CNV        = '.cnv'; // code page conversion table
  ext_Shortcut   = '.lnk'; // Windows shortcuts

  KeyboardFileName = 'keyboard.ini'; // custom keyboard config
  ToolbarFileName  = 'toolbar.ini'; // custom toolbar config
  SampleFileName   = 'sample' + ext_KeyNote;

const
  // for file associations
  _KNT_FILETYPE = 'KeyNote file';
  _KNE_FILETYPE = 'KeyNote encrypted file';
  _KNM_FILETYPE = 'KeyNote macro';
  _KNL_FILETYPE = 'KeyNote plugin';

const                  
  swMinimize     = 'min'; // minimize on startup
  swSetup        = 'setup'; // run Setup routine (UNUSED, we have installer now instead)
  swDebug        = 'debug'; // save some debug info
  swNoReadOpt    = 'nro'; // do not read options (IMPLIES 'nso')
  swNoSaveOpt    = 'nso'; // do not save options
  swNoReg        = 'noreg'; // do not use registry for storing MRU and formpos info
  swRegExt       = 'reg'; // register data file extension
  swNoDefaults   = 'nodef'; // do not read or write default entry settings ('keynote.def')
  swSaveDefIcn   = 'saveicn'; // save factory default icons to bitmap file
  swNoUserIcn    = 'noicn'; // do NOT load user icons from external .ICN file
  swUseOldFormat = 'oldfmt'; // save files using older KeyNote file format
  swSaveToolbars = 'savetb'; // save default toolbar button settings (debug)
  swSaveMenus    = 'savemenu'; // save menu information (debug)

const
  // Filters for open/save dialogs
  FILTER_ALLFILES    = 'All files (*.*)|*.*';
  FILTER_TEXTFILES   = 'Text files (*' + ext_txt + ')|*' + ext_txt;
  FILTER_TEMPLATES   = 'Template files|*' + ext_txt + ';*' + ext_rtf;
  FILTER_RTFFILES    = 'RTF files (*' + ext_rtf + ')|*' + ext_rtf;
  FILTER_FILELINK    = 'Documents|*.kn?;*.txt;*.rtf;*.htm*;*.doc;*.xls|' +
                       'Programs|*.exe;*.com;*.bat|' +
                       'Images|*.gif;*.bmp;*.jpg;*.pcx;*.png|' +
                       FILTER_ALLFILES;
  FILTER_HJTFILES    = 'TreePad files (*' + ext_TreePad + ')|*' + ext_TreePad;
  FILTER_HTMLFILES   = 'HTML files (*' + ext_html + ')|*' + ext_html;
  FILTER_PROGRAMS    = 'Programs (*.exe;*.com)|*.exe;*.com;*.bat';
  FILTER_NOTEFILES   = 'Keynote files (*' + ext_KeyNote + ')|*' + ext_KeyNote + ';*' + ext_Encrypted;
  FILTER_DARTFILES   = 'Dart Notes files (*' + ext_DART + ')|*' + ext_DART;
  FILTER_MACROS      = 'Macro files (*' + ext_Macro + ')|*' + ext_Macro;
  FILTER_IMPORT      = FILTER_RTFFILES + '|' +
                       FILTER_TEXTFILES + '|' +
                       FILTER_HJTFILES + '|' +
                       'HTML files (*.htm*)|*.htm*;*.shtm*;*.asp;*.php;*.css|' +
                       FILTER_ALLFILES;
  FILTER_EXPORT      = 'RTF files (*.rtf)|*.rtf|' +
                       'Text files (*.txt)|*.txt|' +
                       'HTML files (*.htm*)|*.htm*;*.css|' +
                       'All files (*.*)|*.*';


  FILTER_FAVORITES = 'Documents|*.doc;*.xls*;*.txt*;*.rtf;*.knt;*.dck;*.oub;*.vis'+
                     '|HTML, CSS, XML files|*.htm*;*.css;*.xml;*.sht*'+
                     '|Images|*.bmp;*.gif;*.jpg;*.jpeg;*.ico;*.png;*.ani;*.cur;*.pcx;*.tga;*.tif*'+
                     '|Sounds|*.wav;*.mp3;*.voc;*.au;*.snd;*.m3u;*.ra*;*.shn'+
                     '|Movies|*.avi;*.mpg;*.mpeg;*.mov;*.asf'+
                     '|Programs|*.exe;*.com;*.bat;*.pif;*.scr'+
                      '|All files|*.*';

const
  // status bar panel indices
  PANEL_FILENAME     = 0; // displays name of currently open file
  PANEL_FILEICON     = 1; // displays icon based on file format (native, encrypted, DartNotes, etc)
  PANEL_CARETPOS     = 2; // caret row/col in editor; number of lines
  PANEL_NOTEINFO     = 3; // word wrap, read only state, etc
  PANEL_STATUS       = 4; // modified, autosave, etc
  PANEL_INS          = 5; // insert/overwrite state
  PANEL_HINT         = 6; // tooltip hints

const
  Def_HTML_Extensions = '.htm.html.shtm.shtml.php.';
  Def_RTF_Extensions  = '.rtf.';
  Def_Text_Extensions = '.txt.rtf.ini.log.pas.c.h.bas.pl.js.css.inc.inf.diz.';


const
  WM_FATALERRORTERMINATE = WM_USER + 1024; // unused
  WM_JUMPTOKNTLINK = WM_USER + 1025;
  WM_JUMPTOLOCATION = WM_USER + 1026;
  WM_TIPOFTHEDAY = WM_USER + 1027;

const
  DEFAULT_HOTKEY = 24699; // Ctrl+Shift+F12
  DEF_CLIP_CAP_MAX_SIZE = 1024; // bytes
  DIV_1_BLANK = '<1 blank line>'; // equivalent to "^", but this is a user-friendly replacement
  DIV_2_BLANK = '<2 blank lines>'; // as above, equivalent to "^^"
  DEF_CLIP_CAP_DIV = DIV_1_BLANK;
  MAX_LOG_LINES = 125;
  MIN_PASS_LEN = 5; // minimum length of passphrase for encrypted files
  HISTORY_SEPARATOR     = '|';
  KNTLINK_SEPARATOR     = '|';
  DEFAULT_HISTORY_COUNT = 9;
  DEF_TIMERMINIMIZEINT = 10;
  DEF_TIMERCLOSEINT = 15;
  STYLE_IMAGE_BASE = 26;
  MACRO_IMAGE_BASE = 33;
  PLUGIN_IMAGE_BASE = 33;
  TEMPLATE_IMAGE_BASE = 35;
  FAVORITE_IMAGE_BASE = 38;
  DF_DOUBLECLICKTICKS = 350;

const
  // ESC key bindings
  ESC_NOTHING  = 0;
  ESC_MINIMIZE = 1;
  ESC_QUIT     = 2;

  // status bar double-click bindings
  DBLCLK_NOTHING  = 0; // do nothing
  DBLCLK_MINIMIZE = 1; // minimize KeyNote
  DBLCLK_FILEPROP = 2; // open File Properties dialog box
  DBLCLK_FILEMGR  = 3; // open File Manager dialog box
  DBLCLK_NOTEPROP = 4; // open Note Properties dialog box
  DBLCLK_NEWNOTE  = 5; // create a new note
  DBLCLK_RESPANEL = 6; // show or hide resource panel


type
  TNodeWordWrap = ( wwAsNote, wwYes, wwNo );


type
  TImportFileType = (
    itText, itRTF, itHTML, itTreePad
  );


type
  TPropertiesAction = (
    propThisNote,
    propDefaults
    //, propThisFile {removed - for efficiency reasons we do not have separate PER-FILE defaults}
    //, propGeneralSettings {unimplemented}
  );

type
  TParaInfo = packed record // used for TStyle
    SpacingRule : TLineSpacingRule;
    LIndent : integer;
    RIndent : integer;
    FIndent : integer;
    SpaceBefore : integer;
    SpaceAfter : integer;
    Numbering : TRxNumbering;
    Alignment : TParaAlignment;
  end;

  TFontInfo = packed record
    Charset : TFontCharset;
    Color : TColor;
    Name : TFontName;
    Size : integer;
    Style : TFontStyles;
  end;

  TCharInfo = packed record
    Code : integer;
    Count : integer;
    Name : TFontName;
    Charset : TFontCharset;
  end;

  TTextInfo = packed record // used for TStyle
    Disabled : boolean;
    SubscriptStyle : TSubscriptStyle;
    HasHighlight : boolean;
    Highlight : TColor;
  end;

type
  TStyleRange = ( srFont, srParagraph, srBoth );

const
  STYLE_RANGES : array[TStyleRange] of string = (
    'Font', 'Paragraph', 'Font and paragraph'
  );

type
  TBooleanNotifyProc = procedure( const Activate : boolean ) of object;

type
  TCommandRecall = packed record
    Font : TFontInfo;
    Color : TColor;
    GoToIdx : string;
    Para : TParaInfo;
    CharInfo : TCharInfo;
    StyleName : string;
    Language : TLanguage;
  end;

type
  TChrome = packed record
    BGColor : TColor;
    HiColor : TColor;
    Font : TFontInfo;
    HiFont : TFontInfo;
    Language : TLanguage; 
  end;

  TKeyStyle = packed record
    Name : string;
    Chrome : TChrome;
  end;

  TNoteTabProperties = packed record
    ImageIndex : integer;
    Name : string;
  end;

  TNoteEditorProperties = packed record
    PlainText : boolean;
    TabSize : byte;
    URLDetect : boolean;
    UseTabChar : boolean;
    WordWrap : boolean;
  end;

type
  // global options; NOT note-specific
  TEditorOptions = packed record
    AutoFont : boolean;
    AutoIndent : boolean;
    AutoKeyboard : boolean;
    DisableINSKey : boolean; // [*] ignore INS key in editor (prevent entering Overwerite mode)
    EditProtected : boolean; // [*] allow editing blocks of protected text
    FontSizeInc : integer;  // increment font size by this value
    IndentInc : integer;    // increment para indent value by this size
    ParaSpaceInc : integer; // increment space before/after para by this value
    SaveCaretPos : boolean;
    TrackCaretPos : boolean;
    TrackStyle : boolean; // configurable through View menu
    TrackStyleRange : TStyleRange;
    UndoLimit : integer;
    WordCountTrack : boolean;
    WordSelect : boolean;
    WordsPerPage : integer;
  end;

type
  TURLAction = (
    urlOpen, urlOpenNew, urlCopy, urlBoth, urlAsk, urlNothing, urlCreateOrModify
  );

const
  URL_ACTIONS : array[TURLAction] of string = (
    'Open', 'Open in new window', 'Copy to clipboard',
    'Both (open and copy)', 'Prompt', 'Do nothing', 'Create or Modify'
  );

(*
type
  TKNTLocation = record
    FileName : string;
    NoteName : string;
    NodeName : string;
    CaretPos : integer;
    SelLength : integer;
    NoteID : longint;
    NodeID : longint;
    Text : string;
  end;
*)

type
  TNoteTreeProperties = packed record
    AutoNumberNodes : boolean;
    DefaultName : string;
    CheckBoxes : boolean;
    IconKind : TNodeIconKind;
    VerticalLayout : boolean;
    HideChecked: boolean;       // [dpv]
  end;

type
  // global options; NOT note-specific
  TKNTTreeOptions = packed record
    AutoNameVNodes : boolean;
    AutoScroll : boolean;
    ConfirmNodeDelete : boolean;
    ConfirmNodeRefresh : boolean;
    ExpandMode : TTreeExpandMode;
    FullRowSelect : boolean; // [*]
    HotTrack : boolean;
    EditInPlace : boolean;
    EditNewNodes : boolean;
    InheritNodeBG : boolean;
    InheritNodeProperties : boolean;
    NodeDelimiter : string; // [*] delimiter used when generating node paths
    PathTopToBottom : boolean;
    RemovableMediaVNodes : integer; // [*]
    ShowFullPath : boolean;
    ShowFullPathSearch : boolean;
    ShowTooltips : boolean;
    TopLevelCheck : boolean;  // NOT IMPLEMENTED
  end;

type
  TExportOptions = packed record
    ConfirmFilenames : boolean;
    ConfirmOverwrite : boolean;
    ExportPath : string;
    ExportSource : TExportSource;
    HTMLNoFormatting : boolean; // not implemented
    IncludeNodeHeadings : boolean;
    IncludeNoteHeadings : boolean;
    {
    IndentNestedNodes : boolean;
    IndentUsingTabs : boolean;
    IndentValue : integer;
    }
    NodeHeading : string;
    NoteHeading : string;
    SingleNodeFiles : boolean;
    TargetFormat : TExportFmt;
    TreePadForceMaster : boolean;
    TreePadRTF : boolean;
    TreePadSingleFile : boolean;
    TreeSelection : TTreeSelection;
    ExcludeHiddenNodes: boolean;    // [dpv]
  end;

type
  TDirection = (
    dirUp, dirDown, dirLeft, dirRight
  );

const
  DIRECTION_NAMES : array[TDirection] of string = (
    'Up', 'Down', 'Left', 'Right'
  );

type
  TFileStateChange = (
    fscNew, fscOpen, fscSave,
    fscClose, fscModified
  );
  TFileStateChangeSet = set of TFileStateChange;

type
  TLockKeys = ({lkCaps, }lkInsert);

const
  // by Peter Below (TeamB)
  lock_states: array [ TLockKeys ] of Boolean = ({false,} true );
  lock_keys  : array [ TLockKeys ] of Integer = ({VK_CAPITAL, }VK_INSERT);
  lock_labels: array [ TLockKeys ] of String[3] = ({'CAP',} 'INS');
  lock_panels: array [ TLockKeys ] of Integer = ( {PANEL_CAPS,} PANEL_INS );

type
  TFuncKeys = array[1..12] of string;


type
  TKeyOptions = packed record
    // [!] not implemented
    // [*] not settable inside the program (edit INI file manually to change)
    // some fields here are state memory, rather than config options
    // (e.g. names of last-used files or folders)
    AlwaysOnTop : boolean;
    AutoNewFile : boolean;
    AutoRegisterFileType : boolean;
    AutoRegisterPrompt : boolean;
    AutoSave : boolean;
    AutoSaveOnFocus : boolean;
    AutoSaveOnTimer : boolean;
    AutoSaveOnTimerInt : integer; // minutes between auto-saves
    Backup : boolean; // make backup copy
    BackupAppendExt : boolean; // append .BAK extention or replace original extension
    BackupDir : string;
    BackupExt : string;
    BackupLevel : integer;
    BackupVNodes : boolean;
    ColorDlgBig : boolean; // [*] auto show color dialogs in "expanded" form
    ComboFontLen : integer;  // [*] | these three settings   |
    ComboMacroLen : integer; // [*] | are read from INI file |
    ComboStyleLen : integer; // [*] | but NOT saved.         |
    ComboDropDownCount : integer; // [*] drop-down count for toolbar comboboxes
    ConfirmExit : boolean;
    ConfirmTreePaste : boolean;
    ConfirmTabDelete : boolean;
    DateFmt : string;
    Debug : boolean; // [*]
    DebugLogAppend : boolean; // [*] append or overwrite previous debug log
    DefaultNoteType : TNoteType; // [*] remembers last type of note created
    DisableFileMon : boolean; // disable file change monitoring
    DropNodesOnTabMove : boolean; // when dropping nodes on another teb, MOVE them (if false, then COPY)
    DropNodesOnTabPrompt : boolean; // prompt before copying nodes to another tab (note)
    DTLastDateFmt : string;
    DTLastTimeFmt : string;
    DTUseLastSelection : boolean; // insert date/time commands will use the format last selected in drop-down menu; otherwise the DateFmt/TimeFmt values are used
    EncFileAltExt : boolean;  // use .KNE extension for encrypted files (not recommended; for TESTING only!)
    EscAction : integer; // ESC key binding
    AutoPasteEval : boolean; // automatically paste Expr. evaluation results
    AutoPastePlugin : boolean; // automatically paste text generated by plugins
    ExtHTML : string; // [*] list of extensions that will be recognized as HTML files
    ExtRTF : string; // [*] list of extensions that will be recognized as RTF files
    ExtText : string; // list of extensions that will be recognized as text files (so that KeyNote knows it can import them, etc)
    FixScrollBars : boolean; // Makes sure RichEdit scrollbars are properly updated in TreeNodeSelected, but it causes flicker, so default is FALSE
    HotKey : word; // activation hotkey
    HotKeyActivate : boolean; // use activation hotkey only if this is set
    HotKeyWarn : boolean;  // warn if failed to register hotkey
    HTMLImportMethod : THTMLImportMethod;
    IgnoreUpgrades : boolean; // do not display info when upgrade detected
    ImportFileNamesWithExt : boolean; // keep filename extension (in node and note names) when importing files
    InitFontColor : TColor; // initial font color
    InitHiColor : TColor; // initial highlight color
    InsCharFullSet : boolean;
    InsCharKeepFont : boolean;
    InsCharWinClose : boolean;
    KeyReplayDelay : integer; // MILIseconds { OBSOLETE, unused }
    LastCopyPath : string;
    LastExportPath : string;
    LastExportFormat : TExportFmt;
    LastExportAsk : boolean;
    LastFile : string;
    LastImportPath : string;
    LastNumbering : TRxNumbering;
    LastVersion : string; // last version of KeyNote. Used to detect upgrades
    LoadLastFile : boolean; // if TRUE, open last used file at startup
    LoadUserFile : boolean; // if TRUE, always open user-specified file at startup
    LongCombos : boolean; // combo boxes on toolbar 25% longer
    MgrFullPaths : boolean;
    MinimizeOnClose : boolean; // minimize instead of quitting when the [x] icon clicked
    MinimizeOnURL : boolean;   // minimize when URL launched
    MRUCount : integer;
    MRUFullPaths : boolean;
    MRUSubmenu : boolean;
    MRUUse : boolean;
    NoComboIcons : boolean; // [*] kill image list for Style combo box
    NodeNameHistory : string;
    NoRegistry : boolean; // do not use registry (equiv. to "-noreg" cmd line switch
    OpenFloppyReadOnly : boolean;
    OpenNetworkReadOnly : boolean;
    OpenReadOnlyWarn : boolean;
    RecentLanguage : TLanguage; // last language selected in Language dialog box
    ResolveLNK : boolean; // [*]
    ResPanelActiveUpdate : boolean; // always clear respanel contents when hiding, and reload when shown again
    ResPanelLeft : boolean; // show res panel on left side
    ResPanelShow : boolean; // show or hide resource panel
    RichEditv3 : boolean; // force version 3 of riched20.dll
    RunAutoMacros : boolean;
    SafePrint : boolean; // print straight from RxRichEdit, without using TRichPrinter
    SaveDARTWarn : boolean; // [*] warn if saving to DartNOtes format, because some properties will be lost which DartNotes doesn't support
    SaveDefaultFormat : TNoteFileFormat; // [*]
    ShellExecuteShowAllErrors : boolean; // [*]
    ShowFonts : boolean ; // show real font styles in fton drop-down combo
    // ShowSplash : boolean;
    ShowTooltips : boolean;
    SingleInstance : boolean;   // allow only one instance of KeyNotes
    SkipNewFilePrompt : boolean;
    StartMinimized : boolean;
    StartNoteType : TNoteType; // [*] type of note to create automatically with each new file
    StatBarDlbClkAction : integer; // [*] what happens when user double-clicks statusbar
    StatBarDlbClkActionShft : integer; // [*] as above, with Shift held down
    StatBarShow : boolean; // show or hide the statusbar
    StyleShowSamples : boolean; // [*] display font style samples in OwnerDrawn combo
    TabNameHistory : string;
    TimeFmt : string;
    TimerMinimize : boolean; // minimize program after X minutes of inactivity
    TimerMinimizeInt : integer; // minutes
    TimerClose : boolean; // close file after X minutes of inactivity
    TimerCloseDialogs : boolean; // when autoclosing, close any dialogs that are open. If this is not set, file will NOT be auto-closed.
    TimerCloseEncOnly : boolean; // auto close only encrypted files
    TimerCloseInt : integer; // minutes
    TimerCloseAutoReopen : boolean; // will try to reopen encrypted files ONLY!
    TipOfTheDay : boolean;
    TipOfTheDayIdx : integer;
    ToolbarFormatShow : boolean;
    ToolbarInsertShow : boolean;
    ToolbarMacroShow : boolean;
    ToolbarMainShow : boolean;
    ToolbarStyleShow : boolean;
    ToolbarTreeShow : boolean;
    UASEnable : boolean;
    UASPath : string;
    URLAction : TURLAction;
    URLAltBrowserPath : string;
    URLFileAuto : boolean;  // always launch file:// URLs, regardless of URLAction setting
    URLFileDecodeSpaces : boolean; // [*]
    URLFileNoPrefix : boolean; // [*] strip file:/// prefix before launching URLs
    URLFileQuoteSpaces : boolean; // [*]
    URLClickShift : boolean; // if TRUE, user must hold down SHIFT while clicking an URL to activate it
    URLSystemBrowser : boolean; 
    UseOldColorDlg : boolean;
    UseOldFileFormat : boolean; // save files using older firmat (GFKNX)
    UseTray : boolean;
    UserFile : string;
    ZoomIncrement : integer;
  end;


type
  TClipOptions = packed record
    ClipNodeNaming : TClipNodeNaming;
    Divider : string;
    IgnoreSelf : boolean;
    InsertSourceURL : boolean; 
    MaxSize : integer;
    PasteAsText : boolean;
    PasteAsNewNode : boolean;
    PlaySound : boolean;
    Recall : boolean;
    SleepTime : integer; // [*] in tenths of second, i.e. 5 = half a second
    SwitchIcon : boolean;
    TestDupClips : boolean;
    TreeClipConfirm : boolean;
    URLOnly : boolean; // [*] [!] NOT IMPLEMENTED (planned: only capture URLs; discard anything else
    WCDivider : string; // [*] divider for webclips
    WCPasteAsText : boolean; // [*] paste web clips as plain text
  end;

type
  TMailOptions = packed record
    AddrBook : string; // not implemented; only default file is used
    AsPlainText : boolean;
    CCAddr : string;
    FirstLine : string;
    FromAddr : string;
    History : string;
    KeepLog : boolean;
    SMTPPort : string;
    SMTPServer : string;
    Subject : string;
    SubjectPrefix : string;
    TextCharSet : string;
    Timeout : integer;
    ToAddr : string;
    XMailer : string; // NOT SETTABLE BY USER (except with a hex editor, that is)
  end;

type
  TTabOrientation = ( tabposTop, tabposBottom, tabposLeft, tabposRight );

const
  TAB_POSITIONS : array[TTabOrientation] of string = (
    'Top', 'Bottom', 'Left', 'Right'
  );

type
  TTabOptions = packed record
    ActiveColor : TColor;
    ColorAllTabs : boolean; // [*]
    InactiveColor : TColor;
    Font : TFontProperties;
    HotTrack : boolean;
    Images : boolean;
    Stacked : boolean;
    TabsAreButtons : boolean; // NOT RECOMMENDED
    TabOrientation : TTabOrientation;
  end;


type
  TResPanelOptions = record
    ColorFindList : boolean; // [*]
    FindListAltColor : TColor; // [*]
    ShowFind : boolean;
    ShowMacro : boolean;
    ShowPlugin : boolean;
    ShowScratch : boolean;
    ShowTemplate : boolean;
    ShowFavorites : boolean;
    Stacked : boolean;
    TabOrientation : TTabOrientation;
  end;

type
  TFindOptions = packed record
    HiddenNodes: boolean;  // consider hidden nodes [dpv]
    AllNodes : boolean; // search all nodes in tree
    AllTabs : boolean; // search all notes in file
    AutoClose : boolean; // auto close find dialog box when Find button clicked
    EntireScope : boolean; // search from top of text rather than from current position in active note
    FindAllHistory : string;
    FindAllMatches : boolean; // used for the "Find all" function (resource panel)
    FindNew : boolean; // internal state flag: begin new search
    History : string; // list of previously searched strings
    HistoryMaxCnt : integer; // number of history strings to keep
    MatchCase : boolean; // search is case-sensitive
    Pattern : string; // what to find
    ReplaceConfirm : boolean;
    ReplaceHistory : string;
    ReplacePattern : string;
    ReplaceWith : string;
    SearchMode : TSearchMode; // for "Find all" (resource panel) ONLY
    SearchNodeNames : boolean; // for "Find all" (resource panel) ONLY
    WholeWordsOnly : boolean; // only match whole words
    WordAtCursor : boolean; // auto-select word at cursor when invoking Find dialog box
    Wrap : boolean; // when reaching bottom of active note, wrap around at search again from top
  end;


var
  _KNT_WINMSG_ID : word; // GLOBAL MESSAGE ID (used for DLL notifications)
  _FORMPOS_USE_REGISTRY : boolean;
  _FORMPOS_INIFILENAME : string;
  _MainFormHandle : HWND;
  _KNTUtilsDLL_FN : string;
  _IE4Available : boolean;
  // _KNTLocation : TKNTLocation;

procedure InitializeChrome( var Struct : TChrome );
procedure InitializeNoteTabProperties( var Struct : TNoteTabProperties );
procedure InitializeNoteEditorProperties( var Struct : TNoteEditorProperties );
procedure InitializeNoteTreeProperties( var Struct : TNoteTreeProperties );

procedure FontInfoToFont( const FI : TFontInfo; aFont : TFont );
procedure FontToFontInfo( const aFont : TFont; var FI : TFontInfo );

function GetKeyNoteIniFile : string;

procedure ClearObjectList( const AList : TList );
procedure ClearObjectStringList( const AList : TStringList );

implementation

procedure FontInfoToFont( const FI : TFontInfo; aFont : TFont );
begin
  with aFont do
  begin
    Name := FI.Name;
    Color := FI.Color;
    Charset := FI.Charset;
    Size := FI.Size;
    Style := FI.Style;
  end;
end; // FontInfoToFont

procedure FontToFontInfo( const aFont : TFont; var FI : TFontInfo );
begin
  with aFont do
  begin
    FI.Name := Name;
    FI.Color := Color;
    FI.Charset := Charset;
    FI.Size := Size;
    FI.Style := Style;
  end;
end; // FontToFontInfo

procedure InitializeChrome( var Struct : TChrome );
begin
  with Struct do
  begin
    Language := GetSystemDefaultLCID;
    BGColor := clWindow;
    HiColor := clHighlight;
    with Font do
    begin
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
      Charset := DEFAULT_CHARSET;
      Color := clWindowText;
    end;
    with HiFont do
    begin
      Name := 'MS Sans Serif';
      Size := 8;
      Style := [];
      Charset := DEFAULT_CHARSET;
      Color := clHighlightText;
    end;
  end;
end; // InitializeChrome

procedure InitializeNoteTabProperties( var Struct : TNoteTabProperties );
begin
  with Struct do
  begin
    ImageIndex := 0;
    Name := DEFAULT_NEW_NOTE_NAME;
  end;
end; // InitializeNoteTabProperties

procedure InitializeNoteEditorProperties( var Struct : TNoteEditorProperties );
begin
  with Struct do
  begin
    PlainText := false;
    TabSize := 4;
    URLDetect := true;
    UseTabChar := true;
    WordWrap := true;
  end;
end; // InitializeNoteEditorProperties

procedure InitializeNoteTreeProperties( var Struct : TNoteTreeProperties );
begin
  with Struct do
  begin
    AutoNumberNodes := false;
    DefaultName := DEFAULT_NEW_NODE_NAME;
    CheckBoxes := false;
    IconKind := niStandard;
    VerticalLayout := false;
  end;
end; // InitializeNoteTreeProperties

function GetKeyNoteIniFile : string;
const
  KNT_INI_FN = 'keynote' + ext_INI;
var
  path : string;
  p : integer;
begin
  // try current directory first
  result := KNT_INI_FN;

  path := properfoldername( extractfilepath( paramstr( 0 )));

  // try application's directory
  if ( not fileexists( result )) then
    result :=  path + KNT_INI_FN;

  // perhaps we are in a plugin, try parent directory
  if ( not fileexists( result )) then
  begin
    delete( path, length( path ), 1 );
    p := lastpos( '\', path );
    delete( path, succ( p ), length( path ));
    result :=  path + KNT_INI_FN;
  end;

  result := normalFN( result );

end; // GetKeyNoteIniFile

procedure ClearObjectList( const AList : TList );
var
  i, cnt : integer;
begin
  cnt := AList.Count;
  for i := 1 to Cnt do
  begin
    if assigned( AList[pred( i )] ) then
      TObject( AList[pred( i )] ).Free;
  end;
  AList.Clear;
end; // ClearObjectList


procedure ClearObjectStringList( const AList : TStringList );
var
  i, cnt : integer;
begin
  cnt := AList.Count;
  for i := 1 to Cnt do
  begin
    if assigned( AList.Objects[pred( i )] ) then
      AList.Objects[pred( i )].Free;
  end;
  AList.Clear;
end; // ClearObjectStringList

Initialization

  _KNTUtilsDLL_FN := extractfilepath( ParamStr( 0 )) + 'kntutils.dll';
  {$IFDEF WITH_IE}
  _IE4Available := IsIE4Installed;
  {$ELSE}
  _IE4Available := false;
  {$ENDIF}



end.

