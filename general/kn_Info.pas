unit kn_Info;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


{.$DEFINE DLA_DUNI}

interface
uses
   Winapi.Windows,
   Winapi.Messages,
   System.Classes,
   System.SysUtils,
   System.Zip,
   Vcl.Graphics,
   RxRichEd,
   SynGdiPlus,
   gf_misc,
   kn_Const,
   knt.model.note,
   knt.RS
   ;


const
  ext_KeyNote    = '.knt'; // KeyNote data file
  ext_Encrypted  = '.kne'; // KeyNote encrypted file
  ext_LockFile   = '.lck'; // KeyNote lock auxiliary file
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
  ext_Fold       = '.fld'; // Pairs of tokens (opening and closing) to define folding blocks
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
  ext_LAN        = '.lan'; // Languages available
  ext_OPML       = '.opml';

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
  //swSetup        = 'setup'; // run Setup routine (UNUSED, we have installer now instead)
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
  swClean        = 'clean'; // clean/repair the file, actually looking for invalid hyperlinks (see issue #59: http://code.google.com/p/keynote-nf/issues/detail?id=59
  swJmp          = 'jmp';  // Jump to the KNT link indicated in quotes (in any of the recognized formats. Ex: "file:///*1|10|201|0")
                           // Will be ignored if no .knt file specified
  swIgnoreSI     = 'ignsi'; // Ignore single instance option for this call
  swDoNotDisturb = 'dnd';  // Ignore for purposes of "SingleInstance"
  swTitle        = 'title';  // Sets the title of the instance
  swConvKNTLinks = 'clnks';  // Convert Knt Links to the new format (using GID)
  swConvLinksForColor = 'clnkc';  // Convert links to allow for proper text color management (see issue #923)

const
  // Filters for open/save dialogs
  FILTER_ALLFILES    = 'All files (*.*)|*.*';
  FILTER_TEXTFILES   = 'Text files (*' + ext_txt + ')|*' + ext_txt;
  FILTER_TEMPLATES   = 'Template files|*' + ext_txt + ';*' + ext_rtf;
  FILTER_RTFFILES    = 'RTF files (*' + ext_rtf + ')|*' + ext_rtf;
  FILTER_FILELINK    = FILTER_ALLFILES + '|Documents|*.kn?;*.txt;*.rtf;*.htm*;*.odt;*.ods;*.doc*;*.xls*|' +
                       'Programs|*.exe;*.com;*.bat|' +
                       'Images|*.bmp;*.gif;*.png;*.jpg;*.jpeg;*.tif*';

  FILTER_HJTFILES    = 'TreePad files (*' + ext_TreePad + ')|*' + ext_TreePad;
  FILTER_HTMLFILES   = 'HTML files (*' + ext_html + ')|*' + ext_html;
  FILTER_OPMLFILES   = 'OPML files (*' + ext_opml + ')|*' + ext_opml;
  FILTER_PROGRAMS    = 'Programs (*.exe;*.com)|*.exe;*.com;*.bat';
  FILTER_NOTEFILES   = 'Keynote files (*' + ext_KeyNote + ')|*' + ext_KeyNote + ';*' + ext_Encrypted;
{$IFDEF WITH_DART}
  FILTER_DARTFILES   = 'Dart Notes files (*' + ext_DART + ')|*' + ext_DART;
{$ENDIF}
  FILTER_MACROS      = 'Macro files (*' + ext_Macro + ')|*' + ext_Macro;
  FILTER_IMPORT      = FILTER_RTFFILES + '|' +
                       FILTER_TEXTFILES + '|' +
                       FILTER_HJTFILES + '|' +
                       'HTML files (*.htm*)|*.htm*;*.shtm*;*.asp;*.php;*.css|' +
                       FILTER_NOTEFILES + '|' +
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

   FILTER_ICONS      = 'Icon files (*.ico)|*.ico';
   FILTER_TABIMAGES  = 'Tab image files (*.icn)|*.icn';
   FILTER_WEB_BROWSER= 'Programs|*.exe|All files|*.*';

   IMAGE_EXTENSIONS_RECOGNIZED =  '|*.JPG;*.JPEG;*.PNG;*.GIF;*.TIF;*.TIFF;*.BMP;*.DIB;*.WMF;*.EMF;*.ICO;|';

   FILTER_IMAGES = IMAGE_EXTENSIONS_RECOGNIZED +
                  'JPEG (*.jpg, *.jpeg)|*.JPG;*.JPEG|PNG (*.png)|*.PNG|GIF (*.gif)|*.GIF|TIFF (*.tif, *.tiff)|*.TIF;*.TIFF|BMP (*.bmp, *.dib)|*.BMP;*.DIB|' +
                  'Metafiles (*.wmf, *.emf)|*.WMF;*.EMF|ICO (*.ico)|*.ICO|' +
                  'All files (*.*)|*.*';

   FILTER_ZIP    = 'Zip files (*.zip)|*.zip';


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
  DEF_CLIP_CAP_MAX_SIZE = 0; // bytes       ' No limit
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
  DBLCLK_FOLDERPROP = 4; // open Folder Properties dialog box
  DBLCLK_NEWFOLDER  = 5; // create a new folder
  DBLCLK_RESPANEL = 6; // show or hide resource panel


type
  TImportFileType = (
    itText, itRTF, itHTML, itTreePad, itImage
  );


type
  TPropertiesAction = (
    propThisFolder,
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
    NumberingStyle : TRxNumberingStyle;
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

var
  STYLE_RANGES : array[TStyleRange] of string;

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

  TFolderTabProperties = packed record
    ImageIndex : integer;
    Name : string;
    RTL : boolean;
  end;

  TFolderEditorProperties = packed record
    PlainText : boolean;
    TabSize : byte;
    URLDetect : boolean;
    UseTabChar : boolean;
    WordWrap : boolean;
    DefaultZoom: integer;
    TagSelectorDisabled: boolean;
  end;

type
  // global options; NOT folder-specific
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
    PlainDefaultPaste: boolean;         // paste as plain text (*2) when copied outside KN.  (*3)
    BulletsInPlainText: string;  // [*] Replace bullets in plain text by the text indicated. Default: ''. Could be something like: ' - '
    CtrlUpDownMode: TCtrlUpDownMode;
    BulletSepFactor: Single;            // Factor (F) used to calculate spacing between bullet and text for all list types: S = 10 * FontSize / F   (See #876)
    LineWidthEditor: boolean;
  end;
     // *2: TClipOptions.PlainTextMode will determine how to show it.
     // *3: Not for Web Clip nor Clipboard Capture, as they have its own options.

type
  TURLAction = (
    urlOpen, urlOpenNew, urlCopy, urlBoth, urlAsk, urlNothing, urlCreateOrModify
  );

var
  URL_ACTIONS : array[TURLAction] of string;

(*
type
  TKNTLocation = record
    FileName : string;
    FolderName : string;
    NodeName : string;
    CaretPos : integer;
    SelLength : integer;
    FolderID : longint;
    NodeID : longint;
    Text : string;
  end;
*)

type
  TFolderTreeProperties = packed record
    DefaultName : string;
    CheckBoxes : boolean;
    IconKind : TNodeIconKind;
    VerticalLayout : boolean;
    HideChecked: boolean;
    PosDateCol: integer;
    PosFlaggedCol: integer;
  end;

type
  // global options; NOT folder-specific
  TKntTreeOptions = packed record
    AutoNameVNodes : boolean;
    AutoScroll : boolean;
    ConfirmNodeDelete : boolean;          // It will also be used to confirm the deletion of entries
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
    CaretInKNTLinks : boolean;   // [*]
    RelativeKNTLinks : boolean;  // [*] 
    TopLevelCheck : boolean;  // NOT IMPLEMENTED
  end;

type
  TExportOptions = packed record
    ConfirmFilenames : boolean;
    ConfirmOverwrite : boolean;
    ExportPath : string;
    ExportSource : TExportSource;
    HTMLExportMethod: THTMLExportMethod;
    IncludeNodeHeadings : boolean;
    IncludeFolderHeadings : boolean;
    NodeLevelTemplates: boolean;
    SymbolsInHeading: string;
    LengthHeading: string;
    AutoFontSizesInHeading: boolean;
    FontSizesInHeading: string;
    IndentNestedNodes : boolean;
    IndentValue : integer;
    IndentUsingTabs : boolean;
    NumbTabInPlainText: string;
    NodeHeading : string;
    FolderHeading : string;
    TargetFormat : TExportFmt;
    TreePadForceMaster : boolean;
    TreePadRTF : boolean;
    TreePadSingleFile : boolean;
    TreeSelection : TTreeSelection;
    ExcludeHiddenNodes: boolean;
    RTFImgsWordPad: boolean;          // 1: Save images in default, not optmized, WordPad format (ifWmetafile8)
    SectionOnDepth: integer;      // Value of "Section for each of the top N levels". >0 => Allows to create groupings of notes with a defined maximum depth level.
    SectionToFile: boolean;       // True -> Each section is saved in a separate file
                                  // False -> All sections go into the same file, starting on a new page
    EachNoteNewPg: boolean;       // True: Start each note on a new page
    FilePerFolder: boolean;
    ShowPageNumber: boolean;
    TopLvlAsPgHeader: integer;    // Use the top N levels as page header. If > 0 and SectionOnDepth > 0 -> Show page header with the detail indicated
    TableContMaxDepth : integer;  // Maximum depth in table of contents hierarchy
    SimpleFileName: boolean;      // 1: Use only the name of the node for file names, like:  "Node.rtf"
                                  // 0: Use files names like: "01 - Folder - Node.rtf"                    (Default: 0)
  end;

type
  TDirection = (
    dirUp, dirDown, dirLeft, dirRight
  );

var
  DIRECTION_NAMES : array[TDirection] of string;

type
  TFileStateChange = (
    fscNew, fscOpen, fscSave,
    fscClose, fscModified
  );
  TFileStateChangeSet = set of TFileStateChange;

(*  Not used. Referentiated from kn_Main.GetKeyStates, not used
type
  TLockKeys = ({lkCaps, }lkInsert);

const
  // by Peter Below (TeamB)
  lock_states: array [ TLockKeys ] of Boolean = ({false,} true );
  lock_keys  : array [ TLockKeys ] of Integer = ({VK_CAPITAL, }VK_INSERT);
  lock_labels: array [ TLockKeys ] of String[3] = ({'CAP',} 'INS');
  lock_panels: array [ TLockKeys ] of Integer = ( {PANEL_CAPS,} PANEL_INS );
*)

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
    Backup : boolean; // make backup copy (cyclic way)
    BackupAppendExt : boolean; // append .BAK extention or replace original extension
    BackupDir : string;
    BackupExt : string;
    BackupLevel : integer;
    BackupVNodes : boolean;
    BackupRegularIntervals : boolean;
    BackupDayLevel : integer;
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
    //DefaultNoteType : TNoteType; // [*] remembers last type of note created
    DisableAlarmPopup: boolean;
    DisableFileMon : boolean; // disable file change monitoring
    //DropNodesOnTabMove : boolean; // when dropping nodes on another teb, MOVE them (if false, then COPY)
    DropNodesOnTabPrompt : boolean; // prompt before copying nodes to another tab (folder)
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
    ImportFileNamesWithExt : boolean; // keep filename extension (in node and folder names) when importing files
    InitFontColor : TColor; // initial font color
    InitHiColor : TColor; // initial highlight color
    //InsCharFullSet : boolean;
    InsCharAutoAddNew: boolean;
    InsCharCustom: string;
    //InsCharKeepFont : boolean;
    InsCharWinClose : boolean;
    KeyReplayDelay : integer; // MILIseconds { OBSOLETE, unused }
    LanguageUI : string;    // Language of ther user interface. Must be in keynote.lan
    LastCopyPath : string;
    LastExportPath : string;
    //LastExportFormat : TExportFmt;
    LastExportFormat : integer;
    //LastExportAsk : boolean;          // unused
    LastPgPrintSettings : string;       // "Orientation,MarginTop,MarginLeft,MarginBottom,MarginRight"  (Orientation: 0=Portrait 1=Landscape)
    LastFile : string;
    LastImportPath : string;
    LastNumbering : TRxNumbering;
    LastNumberingStyle: TRxNumberingStyle;
    LastVersion : string; // last version opened of KeyNote. Used to detect upgrades
    LastInformedVersion : string; // latest version of which it have been informed
    CheckUpdOnStartup: boolean;   // Check for updates on startup
    VersionLastChecked: TDate;
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
    //NoComboIcons : boolean; // [*] kill image list for Style combo box  (Not used)
    NodeNameHistory : string;
    NoRegistry : boolean; // do not use registry (equiv. to "-noreg" cmd line switch
    OpenFloppyReadOnly : boolean;
    OpenNetworkReadOnly : boolean;
    OpenReadOnlyWarn : boolean;
    PlaySoundOnAlarm: boolean;
    RecentLanguage : TLanguage; // last language selected in Language dialog box
    ResolveLNK : boolean; // [*]
    ResPanelActiveUpdate : boolean; // Reload respanel content when showing again
    ResPanelLeft : boolean; // show res panel on left side
    ResPanelShow : boolean; // show or hide resource panel
    RichEditv3 : boolean; // force version 3 of riched20.dll
    RunAutoMacros : boolean;
    //SafePrint : boolean; // print straight from RxRichEdit, without using TRichPrinter   { OBSOLETE, unused }
    //SaveDARTWarn : boolean; // [*] warn if saving to DartNOtes format, because some properties will be lost which DartNotes doesn't support
    SaveDefaultFormat : TKntFileFormat; // [*]
    ShellExecuteShowAllErrors : boolean; // [*]
    ShowFonts : boolean ; // show real font styles in fton drop-down combo
    // ShowSplash : boolean;
    ShowTooltips : boolean;
    SingleInstance : boolean;   // allow only one instance of KeyNotes
    WarnSingleInstance: boolean;  // [*] Give the message: KeyNote NF have been configured to allow only one instance at a time. Closing this instance...
    SkipNewFilePrompt : boolean;
    StartMinimized : boolean;
    //StartNoteType : TNoteType; // [*] type of note to create automatically with each new file
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
    LockOnOpening : boolean;
    TimerFileLckInt : integer;  // minutes
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
    URLCtrlAction: TURLAction;
    URLAltBrowserPath : string;
    URLFileAuto : boolean;  // always launch file:// URLs, regardless of URLAction setting
    URLFileDecodeSpaces : boolean; // [*]
    URLFileNoPrefix : boolean; // [*] strip file:/// prefix before launching URLs
    URLFileQuoteSpaces : boolean; // [*]
    URLFileEncodeName : boolean; // [*]   Names will not be URL-encoded. Specifically, spaces will not be converetd to %20 (link will be enclosed between < and >)
    URLFilePrefNoHyp: boolean; // [*]     Preferably use simple links, not hyperlinks
    URLFileSepParams : string; // [*]
    URLFileSpaceInParams : string; // [*]
    // URLClickShift : boolean; // if TRUE, user must hold down SHIFT while clicking an URL to activate it        (Does not work with recent versions of RichEdit)
    URLSystemBrowser : boolean;
    URLWebDecode : boolean;     // [*] if True -> it will manage %XX in URL as UTF8, finally converting the whole URL to ANSI or UTF8
    ExtKNTLnkInNewInst: boolean; // If true, the KNT links located in another file will be opened in other instance, and not in current one
    UseOldColorDlg : boolean;
    UseOldFileFormat : boolean; // save files using older firmat (GFKNX)
    UseTray : boolean;
    UserFile : string;
    ZoomIncrement : integer;
    UseCtrlHideTreePanel : boolean;  // [*]
    MarginAltLeft  : integer;  // [*]
    MarginAltRight : integer;  // [*]
    AltMargins: boolean;  // [*] Saves the state of View|Alternative margins
    ModifiedOnTreeResized: boolean;  // [*]
    HintsAccesible: boolean;  // [*] 1 => When hints are shown in status bar, other text panels will be shown blank
    EditorInfoPanelTop: boolean;   // [*] 1 => Top  0 => Bottom           ("EditorInfoPanel" = "InfoBar")
    //AutoDiscoverTags: boolean;
    FixEmojisProblem : boolean;  // [*]   (See issue Lost the bullets - how to get back? #896
    RTLkeyShct: boolean;          // [*] 0 => Don't use keyboard shortcut for RTL detection (RTL: RCtrl+RShft  LTR: LCtrl+LShft,  on key release)  (Default: 0)
    IMEAutoKeyboard: boolean;     // [*] 1 => Will keep IMF_AUTOKEYBOARD on when turned on automatically for bidirectional controls  (Default: 1)
                                  //     0 => IMF_AUTOKEYBOARD will be turned off just after setting RTL mode
    IMEAutoFont: boolean;         // [*] 1 => IMF_AUTOFONT will always be turned on.     (Default: 1)
                                  //     0 => IMF_AUTOFONT will be turned off
                                  // See https://learn.microsoft.com/en-us/windows/win32/controls/em-getlangoptions for info about
                                  // IMF_AUTOKEYBOARD and IMF_AUTOFONT, and how it affects the RichText behaviour.

                          { https://learn.microsoft.com/en-us/windows/win32/controls/em-getlangoptions
                            IMF_AUTOFONT
                               If this flag is set, the control automatically changes fonts when the user explicitly changes to a different keyboard layout.
                               It is useful to turn off IMF_AUTOFONT for universal Unicode fonts. This option is turned on by default (1).

                            IMF_AUTOFONTSIZEADJUST
                               If this flag is set, the control scales font-bound font sizes from insertion point size according to script. For example,
                               Asian fonts are slightly larger than Western ones. This option is turned on by default (1).

                            IMF_AUTOKEYBOARD
                              If this flag is set, the control automatically changes the keyboard layout when the user explicitly changes to a different font,
                              or when the user explicitly changes the insertion point to a new location in the text.
                              Will be turned on automatically for bidirectional controls. For all other controls, it is turned off by default.
                              This option is turned off by default (0).



                            If IMEAutoKeyboard = 1, then when setting a paragraph to RTL and navigating through the text, the keyboard layout will change
                            depending on whether the paragraph is configured as RTL or LTR. Even within a single paragraph, if parts of it are written in
                            a certain language, the keyboard may also switch automatically.

                            Even with IMEAutoKeyboard = 0, immediately after setting a paragraph to RTL, the RichEdit control will automatically activate
                            the IMF_AUTOKEYBOARD flag, which causes the keyboard layout to switch to what it identifies as the most suitable for RTL or LTR
                            content. (*)
                            So, what does IMEAutoKeyboard = 0 actually imply?
                            Despite that initial keyboard switch right after changing to RTL, if IMEAutoKeyboard = 0, the keyboard layout will no longer
                            change just by moving around the text. A key implication is that inserting certain special characters like EmDash (—) will not
                            trigger automatic font changes.
                            For example, if IMEAutoKeyboard = 1 (IMF_AUTOKEYBOARD = 1), the RichEdit control associates certain special characters
                            (like \emdash, \endash, \bullet) with the RTL language script—even if you type or paste those characters inside non-RTL text.
                            Additionally, RichEdit may automatically change the font if it deems the current one unsuitable for the detected script. For
                            instance, after inserting an EmDash, it might switch the font to Calibri, assuming that font offers better RTL support.
                            Note however that even the control will propose to use Calibri after that EmDash (for example), if you insert that character
                            using Insert|Character... or with the help of AutoHotkey (in this last case having RTLkeyShct=0), you will continue writing
                            with your font and keyboard. Only if you place the caret just after the special character and continue typing will next
                            characters change to Calibri and other language. If the special character is inserted pasting from clipboard then the change
                            of font and keyboard will be assured...

                            (*) It would have been possible to make KeyNote restore the previous keyboard layout immediately after setting a paragraph to RTL.
                                However, I found that behavior more uncomfortable than the default one. I believe that for users who genuinely need RTL
                                configuration, automatically switching the keyboard layout upon setting RTL is actually helpful. In fact, this is the
                                default behavior of Windows in RTL-aware controls.
                            IMEAutoKeyboard will default to 1, but could be interesting to change to 0 if needed to avoid the behaviour described related
                            the insertion of some special characters.

                            I’ve also observed that regardless of the IMF_AUTOKEYBOARD setting, whenever the End key is pressed from within a given paragraph,
                            the keyboard layout automatically adjusts depending on whether the paragraph is RTL or LTR.
                            So, with IMEAutoKeyboard = 0 (=> IMF_AUTOKEYBOARD = 0), simply moving the cursor between paragraphs won’t cause the keyboard to
                            switch, but pressing End will, adjusting it to the expected layout according to the paragraph’s direction—without needing to
                            manually switch keyboards from the taskbar.
                            (See GitHub issue #816 for a description of the font-switching issue when inserting certain special characters.)

                            The default value for RTLkeyShct has been set to 0 to avoid the complications that RTL handling entails for the RichEdit control,
                            which could cause problems for users who might activate it by mistake (as originally reported in issue #816, due to the use of
                            AutoHotKey).

                          }

    ScratchBGColor:            TColor;

    ImgDefaultStorageMode:     TImagesStorageMode;
    ImgDefaultExternalStorage: TImagesExternalStorage;
    ImgDefaultCompression:     TZipCompression;
    ImgStorageModeOnExport:    TImagesStorageModeOnExport;    // (smeEmbRTF, smeEmbKNT, smeNone);
    ImgFormatInsideRTF:        TImageFormatToRTF;   // [*] Format used to show the images inside RTF: ifWmetafile8, ifAccordingImage (it will use rtfwmetafile8, rtfEmfblip, rtfPngblip, rtfJpegblip )
    ImgDefaultFormatFromClipb: TImageFormat;        // png or Jpg -> When inserting images from the clipboard, to be used when the image is in BMP, WMF, TIF, GIF  (if PNG, JPG or EMF -> same formats)
    ImgBmpPixelFormat:         TPixelFormat;
    ImgMaxAutoWidthGoal:       integer;             // 0: No limit. <0 : Limit the visible width of the editor
    ImgDefaultLinkMode:        boolean;
    ImgLinkRelativePath:       boolean;
    ImgUseRecycleBin:          boolean;
    ImgRatioSizePngVsJPG:      Single;       // Related to ImgDefaultFormatFromClipb. A value > 0 => A conversion will be performed to both formats (Png and Jpg). If the default format is sizer than the alternative format, in a proportion >= to indicated, the alternative format will be used
    ImgCompressionQuality:     integer;      // It is used for gptJPG format saving and is expected to be from 0 to 100
    ImgViewerBGColor:          TColor;
    ImgSingleViewerInstance:   boolean;
    ImgHotTrackViewer:         boolean;
    ImgSaveInSubfolders:       boolean;
    ImgKeepOrigName:           boolean;      // "Try to keep the original file name" -> If checked, a file named 'MyImage.jpg' will be saved (if possible) with that same name and will not be prefixed with the ID (such as 10_MyImage.jpg)
    ImgViewerPath:             string;       // [*]
    ImgAllowEncrExternal:      boolean;      // [*]  An image marked as encrypted will be saved in configured storage, even if it is external (ZIP or Folder).
  end;


type
  TClipOptions = packed record
    ClipNodeNaming : TClipNodeNaming;
    Divider : string;
    IgnoreSelf : boolean;
    InsertSourceURL : boolean;
    MaxSize : integer;
    PasteAsText : boolean;              // (*1)
    PlainTextMode: TClipPlainTextMode;
    PasteAsNewNode : boolean;           // (*1)
    PlaySound : boolean;                // (*1)
    Recall : boolean;
    SleepTime : integer;                // in tenths of second, i.e. 5 = half a second
    SwitchIcon : boolean;
    TestDupClips : boolean;
    TreeClipConfirm : boolean;
    URLOnly : boolean;                  // only capture URLs; discard anything else. Will be interpreted as: if only copied one word, what it's wanted is to paste only the URL
    WCDivider : string;                 // divider for webclips
  end;

  // *1: Only for Clipboard Capture

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

var
  TAB_POSITIONS : array[TTabOrientation] of string;

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
    //ColorFindList : boolean; // [*]
    //FindListAltColor : TColor; // [*]
    FontSizeFindResults: integer;
    ShowFind : boolean;
    ShowMacro : boolean;
    ShowPlugin : boolean;
    ShowScratch : boolean;
    ShowTemplate : boolean;
    ShowFavorites : boolean;
    ShowTags : boolean;
    Stacked : boolean;
    TabOrientation : TTabOrientation;
  end;

type
  TEmphasizedSerch = (esNone, esParagraph, esWords);

type
  TFindOptions = packed record
    HiddenNodes: boolean;  // consider hidden nodes [dpv]
    AllNodes : boolean; // search all nodes in tree
    AllTabs : boolean;   // search all folders in file (resource panel)
    AllTabs_FindReplace : boolean; // search all folders in file (FindReplace dialog)
    AllEntries_FindReplace: boolean;   // search all entries in note (FindReplace dialog)
    CurrentNodeAndSubtree : boolean;   // search only in current node and subtree
    HiddenEntries_FindReplace: boolean;
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
    ReplaceWith : string;
    SearchMode : TSearchMode; // for "Find all" (resource panel) ONLY
    SearchScope : TSearchScope; // for "Find all" (resource panel) ONLY
    CheckMode: TSearchCheckMode;  // for "Find all" (resource panel) ONLY
    FoldedMode: TSearchFoldedMode;   // for "Find all" (resource panel) ONLY
    WholeWordsOnly : boolean; // only match whole words
    WordAtCursor : boolean; // auto-select word at cursor when invoking Find dialog box
    Wrap : boolean; // when reaching bottom of active folder, wrap around at search again from top
    SelectedText: boolean;    // Restrict replacement to actual selection
    SelectionStart: integer;  // Beginning of selection, where to limit replacement if 'Selected Text' is checked
    SelectionEnd: integer;    // End of selection, where to limit replacement if 'Selected Text' is checked
    ResetNextAftN: integer;   // Reset find next after N seconds (0 no reset). If reset, Find Next (F3) will ask for a new pattern
    LastModifFrom: TDate;     // for "Find all" (resource panel) ONLY
    LastModifUntil: TDate;    // ,,
    CreatedFrom: TDate;       // for "Find all" (resource panel) ONLY
    CreatedUntil: TDate;      // ,,
    SearchPathInNodeNames: boolean;      // ,,
    ShowChildren: boolean;      // show children of matching nodes
    InheritedTags: boolean;     // Each node will be considered as having its own tags and the tags of its ancestors
    EmphasizedSearch: TEmphasizedSerch;  // for "Find all" (resource panel) ONLY
    ProtectedNodesAndEntriesOnly: boolean;             //   ,,
    TagSearch: boolean;   // Search for a tag (#MyTag). It must meet certain other criteria.
    FindTagsIncl: TFindTags;   // Include text/notes with ALL or ANY of the selected tags
    FindTagsExcl: TFindTags;   // Exclude text/notes with ANY of the selected tags
    FindTagsInclNotReg: string;
    FindTagsExclNotReg: string;
    FindTagsIncl_FindReplace: TNoteTagArray; // Consider entries with ALL the selected tags (for use with Find/Replace)
    TagsModeOR: boolean;       // Tags include, ¿mode OR? (vs ALL)
    TagsMetadata: boolean;     // Tags will be searched for in the notes' metadata
    TagsEntriesMetadata: boolean;  // Tags will be searched for in all entries' metadata
    TagsText: boolean;         // Tags will be searched for in the notes' text
    DefaultTagsExcl: TNoteTagArray;    // Saved in .ini as string: "TagID1,TagID2,..."
  end;


type
  TTextInterval = record
     PosI:    integer;
     PosF:    integer;
  end;

  TEntryFragments = class
     NumFrag: integer;
     Fragments: Array of TTextInterval;
  end;

  TFoundEntry = class
     NEntry: TNoteEntry;
     FragmentsInEntry: TEntryFragments;
  end;
  TFoundEntryInNoteList = TSimpleObjList<TFoundEntry>;
  TFoundEntriesInNotesList = TSimpleObjList<TFoundEntryInNoteList>;


type
  TExportFoldedTextMode = (fmKeepUnchanged, fmUnfold, fmRemoveAll, fmRemoveTagged );


const
  MainPanels: set of TNEntriesPanel = [Low(TNEntriesMainPanel)..High(TNEntriesMainPanel)];
  TNEntriesPanel_Count = Ord(High(TNEntriesPanel)) - Ord(Low(TNEntriesPanel)) + 1;


type
  TFilterOptionsInPanel = packed record
    Enabled: boolean;
    TagsModeOR: boolean;              // Tags include, ¿mode OR? (vs ALL)
    FindTagsIncl: TFindTags;          // Consider text/entries with ALL/ANY of the selected tags in its metadata
    FindTagsExcl: TFindTags;          // Exclude text/entries with ANY of the selected tags
    TagsText: boolean;                // Tags will be searched for in the notes' text
    TextFilter : string;              // Text/entry to consider must include the pattern
    MatchCase : boolean;              // case-sensitive ("c")
    WholeWordsOnly : boolean;         // only match whole words ("w")
    SearchMode : TSearchMode;         // "e":Exact phrase, "&":All the words, "|":Any of the words
    ConsiderHidden: boolean;          // Consider hidden entries
    ShowExcerpts: boolean;            // (when using TextFilter) "x"

    function Equal(const aFilter: TFilterOptionsInPanel): boolean;
    function Empty: boolean;
  end;

type
  TMEPanelCustomization = packed record         // ME: Multi-Entry
    Content: TContentInMultiEntryMode;
    ShowDateInHeader: boolean;
    ShowTimeInHeader: boolean;
    ShowTagsInHeader: boolean;
    ShowLineInHeader: boolean;
    CompactHeader: boolean;
    DescendingOrder: boolean;
    FoldedTextMode: TExportFoldedTextMode;
    Filter: TFilterOptionsInPanel;

    procedure Initialize;
    function Equal(const aMEPanelCustomiz: TMEPanelCustomization; IgnoreFilterEnabled: boolean = false): boolean;
    function ToString(const PanelCustomizReference: TMEPanelCustomization): string;
    procedure LoadFromString(Str: string);
  end;

 TPanelSizeRatios = record
   Top:    Single;   // Ratio Top vs Other (Center+Bottom)
   Bottom: Single;   // Ratio Bottom vs Other (Center+Top)
   TLTR:   Single;   // Ratio TL vs TR
   BLBR:   Single;   // Ratio BL vs BR

   TopAutoFromHidden: Single;     // Ratio Top vs Other to be applied when entering a panel reduced to hidden
   BottomAutoFromHidden: Single;  // ,,    Bottom

   procedure Initialize;
   function Equal(const SizeRatios: TPanelSizeRatios): boolean;
   function ToString: string;
 end;
 PPanelSizeRatios = ^TPanelSizeRatios;


 TNoteTagArrayPanelArray = array[TNEntriesMainPanel] of TNoteTagArray;
 TNEntriesPanelEnabledArray = array[TNEntriesMainPanel] of boolean;

type
   TNoteAdvancedOptions = packed record

     EnableAdvEditionInSingleEntryNotes: boolean;    // If false -> ignore Ctrl+Enter, Ctrl+Shift+Enter in notes with one entry

     DefaultUseForQL: TNEntriesPanelUseArray;   // QL: QueryLayout
     DefaultUseForEL: TNEntriesPanelUseArray;   // EL: EditingLayout
     DefaultEnabledForQL:  TNEntriesPanelEnabledArray;
     DefaultEnabledForEL:  TNEntriesPanelEnabledArray;

     LinkedTagsForQL: TNoteTagArrayPanelArray;   // Serialized as string: "TagID1,TagID2,...|TagID1,TagID2,...|..."
     LinkedTagsForEL: TNoteTagArrayPanelArray;

     DefaultMECustomizForQL: array[TNEntriesMainPanel] of TMEPanelCustomization;
     DefaultMECustomizForEL: array[TNEntriesMainPanel] of TMEPanelCustomization;

     ShowNewestEntryAtStartup: boolean;   // Show newest or oldest entry at startup, instead of last selected when TEditorOptions.SaveCaretPos = 0 (False)
     NewEntriesAlwaysOnEdLayout: boolean;
     EditTagLinkedEntryInSelectedEntryPanel: boolean;   // Also for new entries
     EditorInfoBarPos:  TEditorInfoBarPos;

     FirstLinesOfText_MaxLength: integer;
     FirstLinesOfText_MaxLines: integer;

     SizeRatiosQL: TPanelSizeRatios;
     SizeRatiosEL: TPanelSizeRatios;
     AutoExpandInPanels: boolean;           // for TL,TR, BL and BR

     public procedure Initialize (OnlyCustomiz: boolean = false);
     public function ToString: String;
     public procedure LoadFromString(Str: String; var ErrorsLoading: boolean);
     public function ValidateLayout(Layout: TPanelLayout): boolean;
  end;


const
   NoteGID_NotConverted = 9999999;

type
  TMergedNotes = class
    type
     TPairValues = record
       oldGID: Cardinal;
       newGID: Cardinal;
     end;

    private
      NoteGIDs: array of TPairValues;
      iNote: integer;

    public
      SameFile: boolean;
      constructor Create(N: integer);
      destructor Destroy;  override;
      procedure AddOldNewGIDs (Old, New: Cardinal);
      function GetNewGID (GID: Cardinal): Cardinal;
  end;

type
   TMergeFolders = record
       oldID: Cardinal;
       newID: Cardinal;
       newFolder: boolean;
   end;

type
  TMirrorAction = (
    maMovingToTarget,                 // Moving node to targetNode
    maChangingChkState,               // Changed checked state of node
    maDeleting                        // Deleting node
  );


var
  _KNT_WINMSG_ID : word; // GLOBAL MESSAGE ID (used for DLL notifications)
  _FORMPOS_USE_REGISTRY : boolean;
  _FORMPOS_INIFILENAME : string;
  _MainFormHandle : HWND;
  _KNTUtilsDLL_FN : string;
  _IE4Available : boolean;
  // _KNTLocation : TKNTLocation;

procedure InitializeChrome( var Struct : TChrome );
procedure InitializeFolderTabProperties( var Struct : TFolderTabProperties );
procedure InitializeFolderEditorProperties( var Struct : TFolderEditorProperties );
procedure InitializeFolderTreeProperties( var Struct : TFolderTreeProperties );
procedure InitializeDefaultNoteAdvOptions(var Struct: TNoteAdvancedOptions);

procedure FontInfoToFont( const FI : TFontInfo; aFont : TFont; dpi: integer = 96);
procedure FontToFontInfo( const aFont : TFont; var FI : TFontInfo; dpi: integer = 96);

function GetKeyNoteIniFile : string;

procedure ClearObjectList( const AList : TList );
procedure ClearObjectStringList( const AList : TStringList );

implementation

uses gf_strings,
     knt.App;

procedure FontInfoToFont( const FI : TFontInfo; aFont : TFont; dpi: integer = 96); // 96: 100% scaling
begin
  with aFont do begin
    Name := FI.Name;
    Color := FI.Color;
    Charset := FI.Charset;
    if dpi = 96 then
       Size := FI.Size
    else
       Height := -MulDiv(FI.Size, dpi, 72);   // See comments to TaskModalDialog in gf_miscvcl

    Style := FI.Style;
  end;
end; // FontInfoToFont

procedure FontToFontInfo( const aFont : TFont; var FI : TFontInfo; dpi: integer = 96);
begin
  with aFont do begin
    FI.Name := Name;
    FI.Color := Color;
    FI.Charset := Charset;
    FI.Size := Size;

    if dpi = 96 then
       FI.Size:= Size
    else
       FI.Size := -MulDiv(Height, 72, dpi);   // See comments to TaskModalDialog in gf_miscvcl

    FI.Style := Style;
  end;
end; // FontToFontInfo

procedure InitializeChrome( var Struct : TChrome );
begin
  with Struct do begin
    Language := GetSystemDefaultLCID;
    BGColor := clWindow;
    HiColor := clHighlight;
    with Font do begin
      Name := 'Tahoma';
      Size := 10;
      Style := [];
      Charset := DEFAULT_CHARSET;
      Color := clWindowText;
    end;
    with HiFont do begin
      Name := 'Tahoma';
      Size := 10;
      Style := [];
      Charset := DEFAULT_CHARSET;
      Color := clHighlightText;
    end;
  end;
end; // InitializeChrome

procedure InitializeFolderTabProperties( var Struct : TFolderTabProperties );
begin
  with Struct do begin
    ImageIndex := 0;
    Name := DEFAULT_NEW_FOLDER_NAME;
    RTL := False;
  end;
end; // InitializeNoteTabProperties

procedure InitializeFolderEditorProperties( var Struct : TFolderEditorProperties );
begin
  with Struct do begin
    PlainText := false;
    TabSize := 4;
    URLDetect := true;
    UseTabChar := true;
    WordWrap := true;
    DefaultZoom:= 100;
  end;
end; // InitializeNoteEditorProperties


procedure InitializeDefaultNoteAdvOptions(var Struct: TNoteAdvancedOptions);
begin
   Struct.Initialize();
end;

procedure InitializeFolderTreeProperties( var Struct : TFolderTreeProperties );
begin
  with Struct do begin
    DefaultName := DEFAULT_NEW_NOTE_NAME;
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
  if ( not fileexists( result )) then begin
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
  for i := 1 to Cnt do begin
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
  for i := 1 to Cnt do begin
    if assigned( AList.Objects[pred( i )] ) then
      AList.Objects[pred( i )].Free;
  end;
  AList.Clear;
end; // ClearObjectStringList


//=====  TMergedNotes ================

constructor TMergedNotes.Create(N: integer);
begin
   SetLength(NoteGIDs, N);
   iNote:= 0;
end;

destructor TMergedNotes.Destroy;
begin
   NoteGIDs:= nil;
end;

procedure TMergedNotes.AddOldNewGIDs (Old, New: Cardinal);
begin
  NoteGIDs[iNote].oldGID:= Old;
  NoteGIDs[iNote].newGID:= New;
  inc(iNote);
end;

function TMergedNotes.GetNewGID (GID: Cardinal): Cardinal;
 var
   i: integer;
begin
  Result:= NoteGID_NotConverted;
  for i := 0 to High(NoteGIDs) do
     if NoteGIDs[i].oldGID = GID then begin
        Result:= NoteGIDs[i].newGID;
        exit;
     end;

  if SameFile then
     Result:= GID;
end;


procedure TPanelSizeRatios.Initialize;
begin
   TLTR:= 0.5;
   BLBR:= 0.5;
   Top:= 0.1354;
   Bottom:= 0.15;

   BottomAutoFromHidden:= 0.15;
   TopAutoFromHidden:= 0.15;
end;

function TPanelSizeRatios.Equal(const SizeRatios: TPanelSizeRatios): boolean;
begin
   Result:= ((Top = 0) or (Top = SizeRatios.Top)) and
            ((Bottom = 0) or (Bottom = SizeRatios.Bottom)) and
            ((TLTR = 0) or (TLTR = SizeRatios.TLTR)) and
            ((BLBR = 0) or (BLBR = SizeRatios.BLBR)) and
            ((BottomAutoFromHidden = 0) or (BottomAutoFromHidden = SizeRatios.BottomAutoFromHidden)) and
            ((TopAutoFromHidden = 0) or (TopAutoFromHidden = SizeRatios.TopAutoFromHidden));
end;


function TPanelSizeRatios.ToString: string;
var
   Config: TFormatSettings;
begin
   Config := TFormatSettings.Create('en-US');                             // -> 0.1234

   Result:= Format('%.4g|%.4g|%.4g|%.4g|%.4g|%.4g',
               [
               Top,
               Bottom,
               TLTR,
               BLBR,
               TopAutoFromHidden,
               BottomAutoFromHidden
               ], Config);
end;



function TFilterOptionsInPanel.Equal(const aFilter: TFilterOptionsInPanel): boolean;
begin
  Result:= False;
  with aFilter do begin
    if Self.TagsModeOR <> TagsModeOR then exit;
    if Self.TextFilter <> TextFilter then exit;
    if Self.MatchCase <> MatchCase then exit;
    if Self.WholeWordsOnly <> WholeWordsOnly then exit;
    if Self.SearchMode <> SearchMode then exit;
    if Self.ConsiderHidden <> ConsiderHidden then exit;
    if Self.ShowExcerpts <> ShowExcerpts then exit;
    if Self.TagsText <> TagsText then exit;
    if not TNoteTagArrayUtils.FindTagsEqual(Self.FindTagsIncl, FindTagsIncl) then exit;
    if not TNoteTagArrayUtils.FindTagsEqual(Self.FindTagsExcl, FindTagsExcl) then exit;
  end;

  Result:= True;
end;

function TFilterOptionsInPanel.Empty: boolean;
begin
   Result:= (TextFilter = '') and (FindTagsIncl = nil) and (FindTagsExcl = nil);
end;


// Set built-in default values

procedure TNoteAdvancedOptions.Initialize(OnlyCustomiz: boolean = false);
var
  p: TNEntriesPanel;
begin
    if not OnlyCustomiz then begin
       EnableAdvEditionInSingleEntryNotes:= True;
       NewEntriesAlwaysOnEdLayout:= false;
       EditTagLinkedEntryInSelectedEntryPanel:= true;

       for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
          DefaultUseForQL[p] := pnuHidePanel;
          DefaultEnabledForQL[p] := false;
          DefaultUseForEL[p] := pnuHidePanel;
          DefaultEnabledForEL[p] := false;

          LinkedTagsForQL[p]:= nil;
          LinkedTagsForEL[p]:= nil;
       end;
       DefaultUseForQL[pnCenter] := pnuShowAllEntries;
       DefaultEnabledForQL[pnCenter] := true;
       DefaultUseForEL[pnTL]     := pnuShowSelectedEntry;
       DefaultUseForEL[pnCenter] := pnuShowAllEntries;
       DefaultEnabledForEL[pnTL] := true;
       DefaultEnabledForEL[pnCenter] := true;
    end;


    // Visual customization:

    for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
       DefaultMECustomizForQL[p].Initialize;
       DefaultMECustomizForEL[p]:= DefaultMECustomizForQL[p];
    end;

    SizeRatiosQL.Initialize;
    SizeRatiosEL.Initialize;

    ShowNewestEntryAtStartup:= true;
    AutoExpandInPanels:= false;
    FirstLinesOfText_MaxLength:= 250;
    FirstLinesOfText_MaxLines:= 3;
    EditorInfoBarPos:= ibpMostExtremeExcVincTag;
 end;


procedure TMEPanelCustomization.Initialize;
begin
    Content:= cmWholeEntry;
    ShowDateInHeader:= true;
    ShowTimeInHeader:= true;
    ShowTagsInHeader:= true;
    ShowLineInHeader:= true;
    CompactHeader:= false;
    DescendingOrder:= True;
    FoldedTextMode:= fmKeepUnchanged;

    with Filter do begin
       Enabled:= False;
       TagsModeOR:= false;
       FindTagsIncl:= nil;
       FindTagsExcl:= nil;
       TagsText:= false;
       TextFilter:= '';
       MatchCase:= false;
       WholeWordsOnly:= false;
       SearchMode:= smPhrase;
       ConsiderHidden:= false;
       ShowExcerpts:= false;
    end;
end;


function TMEPanelCustomization.Equal(const aMEPanelCustomiz: TMEPanelCustomization; IgnoreFilterEnabled: boolean = false): boolean;
begin
   Result:= (Content = aMEPanelCustomiz.Content) and
             (ShowDateInHeader = aMEPanelCustomiz.ShowDateInHeader) and
             (ShowTimeInHeader = aMEPanelCustomiz.ShowTimeInHeader) and
             (ShowTagsInHeader = aMEPanelCustomiz.ShowTagsInHeader) and
             (ShowLineInHeader = aMEPanelCustomiz.ShowLineInHeader) and
             (CompactHeader =    aMEPanelCustomiz.CompactHeader) and
             (DescendingOrder = aMEPanelCustomiz.DescendingOrder)  and
             (FoldedTextMode = aMEPanelCustomiz.FoldedTextMode) and
             (IgnoreFilterEnabled or (Filter.Enabled = aMEPanelCustomiz.Filter.Enabled)) and
             Filter.Equal(aMEPanelCustomiz.Filter);
end;


function TMEPanelCustomization.ToString(const PanelCustomizReference: TMEPanelCustomization): string;
var
   FilterStr: string;
   txtFilter: string;
begin
     Result:= '';
     if not Equal(PanelCustomizReference) then begin
           if not Filter.Equal(PanelCustomizReference.Filter) then begin
              txtFilter:= Filter.TextFilter;
              txtFilter:= StringReplace(txtFilter, '"', '\"', [rfReplaceAll]);
              with Filter do begin
                FilterStr:= Format('%s;%s;%s;%s;%s;%s;%s;%d;"%s";%s;%s',
                  [
                  BOOLEANSTR[Enabled],
                  BOOLEANSTR[TagsModeOR],
                  BOOLEANSTR[TagsText],
                  BOOLEANSTR[MatchCase],
                  BOOLEANSTR[WholeWordsOnly],
                  BOOLEANSTR[ConsiderHidden],
                  BOOLEANSTR[ShowExcerpts],
                  Ord(SearchMode),
                  txtFilter,
                  TNoteTagArrayUtils.ToString(FindTagsIncl),
                  TNoteTagArrayUtils.ToString(FindTagsExcl)
                   ]);
              end;
           end
           else
           if (Filter.Enabled = PanelCustomizReference.Filter.Enabled) then
              FilterStr:= '='
           else
              FilterStr:= '*';



           Result:= Format('%d:%s:%s:%s:%s:%s:%s:%d:%s',
             [
             Ord(Content),
             BOOLEANSTR[ShowDateInHeader],
             BOOLEANSTR[ShowTimeInHeader],
             BOOLEANSTR[ShowTagsInHeader],
             BOOLEANSTR[ShowLineInHeader],
             BOOLEANSTR[CompactHeader],
             BOOLEANSTR[DescendingOrder],
             Ord(FoldedTextMode),
             FilterStr]);
     end;
end;


procedure TMEPanelCustomization.LoadFromString(Str: string);
var
   FilterStr: string;
   txtFilter: string;
   Strs: TStrings;
begin
   if Str = '' then exit;

   Strs:= TStringList.Create;
   try

      Strs.Clear;
      SplitString(Strs, Str, ':', false);

      Content:=            TContentInMultiEntryMode(StrToInt(Strs[0]));
      ShowDateInHeader:=   Strs[1] = BOOLEANSTR[true];
      ShowTimeInHeader:=   Strs[2] = BOOLEANSTR[true];
      ShowTagsInHeader:=   Strs[3] = BOOLEANSTR[true];
      ShowLineInHeader:=   Strs[4] = BOOLEANSTR[true];
      CompactHeader:=      Strs[5] = BOOLEANSTR[true];
      DescendingOrder:=    Strs[6] = BOOLEANSTR[true];
      FoldedTextMode:=     TExportFoldedTextMode(StrToInt(Strs[7]));

      if Strs[8] = '=' then
         // Equal (including Enabled status)
      else
      if Strs[8] = '*' then
         Filter.Enabled:= not Filter.Enabled

      else begin
         FilterStr:= Strs[8];
         Strs.Clear;
         SplitString(Strs, FilterStr, ';', false);

         with Filter do begin
            Enabled:=        Strs[0] = BOOLEANSTR[true];
            TagsModeOR:=     Strs[1] = BOOLEANSTR[true];
            TagsText:=       Strs[2] = BOOLEANSTR[true];
            MatchCase:=      Strs[3] = BOOLEANSTR[true];
            WholeWordsOnly:= Strs[4] = BOOLEANSTR[true];
            ConsiderHidden:= Strs[5] = BOOLEANSTR[true];
            ShowExcerpts:=   Strs[6] = BOOLEANSTR[true];
            SearchMode:=     TSearchMode(StrToInt(Strs[7]));
            txtFilter:=      Strs[8];
            if txtFilter <> '' then begin
               txtFilter:=  StringReplace(txtFilter, '\"', '"', [rfReplaceAll]);
               txtFilter:=  txtFilter.Substring(1, Length(txtFilter)-2);
               TextFilter:=     txtFilter;
            end;
            FindTagsIncl:=   TNoteTagArrayUtils.StringToFindTags(Strs[9]);
            FindTagsExcl:=   TNoteTagArrayUtils.StringToFindTags(Strs[10]);
         end;

      end;

   finally
       Strs.Free;
   end;

end;



function TNoteAdvancedOptions.ToString: String;
var
  p: TNEntriesMainPanel;
  DefUseForQL, DefUseForEL, SEP: String;
  DefEnabledForQL, DefEnabledForEL: String;
  LinkedTagsQL, LinkedTagsEL: String;
  DefMECustomizForQL, DefMECustomizForEL: String;
  PanelCustomizReference: TMEPanelCustomization;

begin
     (*
       QL: QueryLayout     EL: EditingLayout

       EnableAdvEditionInSingleEntryNotes|AutoExpandInPanels|ShowNewestEntryAtStartup|NewEntriesAlwaysOnEdLayout|EditTagLinkedEntryInSelectedEntryPanel|
       EditorInfoBarPos|FirstLinesOfText_MaxLength|FirstLinesOfText_MaxLines|
       Top|Bottom|TLTR|BLBR|TopAutoFromHidden|BottomAutoFromHidden|     <--- [SizeRatiosQL]
       Top|Bottom|TLTR|BLBR|TopAutoFromHidden|BottomAutoFromHidden|     <--- [SizeRatiosEL]
       DefaultUseForQL|DefaultUseForEL|
       LinkedTagsForQL|LinkedTagsForEL|
       DefaultEnabledForQL|DefaultEnabledForEL|
       DefaultMECustomizForQL|DefaultMECustomizForQL

       DefaultUseForQL: PanelUseForQL_TL/PanelUseForQL_TR/PanelUseForQL_BL/PanelUseForQL_BR/PanelUseForQL_Center  (ex:  1/0/2/0/3 )
       DefaultUseForEL:   ,,  for EditingLayout

       LinkedTagsForQL:  TagsForQL_TL/TagsForQL_TR/TagsForQL_BL/TagsForQL_BR/TagsForQL_Center     (ex: 1/2,5///)
       LinkedTagsForEL:   ,, for EditingLayout

       DefaultEnabledForQL:  0|1|....                        (Enabled:1  Not enabled:0)
       DefaultEnabledForEL:  ,, for EditingLayout

       DefaultMECustomizForQL:   PanelCustomForQL_TL/PanelCustomForQL_TR/PanelCustomForQL_BL/PanelCustomForQL_BR/PanelCustomForQL_Center
       DefaultMECustomizForEL:    ,, for EditingLayout


       PanelCustomForX_X:
             '' (Empty, if equals to built-in)  or

             Content:ShowDateInHeader:ShowTimeInHeader:ShowTagsInHeader:ShowLineInHeader:CompactHeader:DescendingOrder:FoldedTextMode:Filter

             Filter:
                  '=' (if equals to built-in)  or
                  '*' (if equals to built-in), BUT with different Enabled state  or

                   Enabled;TagsModeOR;TagsText;MatchCase;WholeWordsOnly;ConsiderHidden;ShowExcerpts;SearchMode;TextFilter;FindTagsIncl;FindTagsExcl
                   FindTagsIncl: Tag1,Tag2,... TagsN&...&Tag1',Tag2',... TagsN&...'    (ex: 3,2&1&1,10 )
                   FindTagsExcl:   ,,

     *)


   PanelCustomizReference.Initialize;

   SEP:= '';
   for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
       DefUseForQL:= DefUseForQL + SEP + Ord(DefaultUseForQL[p]).ToString;
       DefUseForEL:= DefUseForEL + SEP + Ord(DefaultUseForEL[p]).ToString;

       LinkedTagsQL:= LinkedTagsQL + SEP + TNoteTagArrayUtils.ToString(LinkedTagsForQL[p]);
       LinkedTagsEL:= LinkedTagsEL + SEP + TNoteTagArrayUtils.ToString(LinkedTagsForEL[p]);

       DefEnabledForQL:= DefEnabledForQL + SEP + BOOLEANSTR[DefaultEnabledForQL[p]];
       DefEnabledForEL:= DefEnabledForEL + SEP + BOOLEANSTR[DefaultEnabledForEL[p]];

       DefMECustomizForQL:= DefMECustomizForQL + SEP + DefaultMECustomizForQL[p].ToString(PanelCustomizReference);
       DefMECustomizForEL:= DefMECustomizForEL + SEP + DefaultMECustomizForEL[p].ToString(PanelCustomizReference);

       SEP:= '/';
   end;


   Result:= Format('%s|%s|%s|%s|%s|%d|%d|%d|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s',
               [
               BOOLEANSTR[EnableAdvEditionInSingleEntryNotes],
               BOOLEANSTR[AutoExpandInPanels],
               BOOLEANSTR[ShowNewestEntryAtStartup],
               BOOLEANSTR[NewEntriesAlwaysOnEdLayout],
               BOOLEANSTR[EditTagLinkedEntryInSelectedEntryPanel],

               Ord(EditorInfoBarPos),
               FirstLinesOfText_MaxLength,
               FirstLinesOfText_MaxLines,

               SizeRatiosQL.ToString,
               SizeRatiosEL.ToString,

               DefUseForQL,
               DefUseForEL,
               LinkedTagsQL,
               LinkedTagsEL,
               DefEnabledForQL,
               DefEnabledForEL,
               DefMECustomizForQL,
               DefMECustomizForEL
               ]);


end;



procedure TNoteAdvancedOptions.LoadFromString(Str: String; var ErrorsLoading: boolean);
var
  i: integer;
  Strs, Strs2: TStrings;

  p: TNEntriesMainPanel;
  DefUseForQL, DefUseForEL, SEP: String;
  LinkedTagsQL, LinkedTagsEL: String;
  DefEnabledForQL, DefEnabledForEL: String;
  DefMECustomizForQL, DefMECustomizForEL: String;
  PanelCustomiz: TMEPanelCustomization;
  Config: TFormatSettings;
  LayoutOK: boolean;


  procedure LoadMEPanelCustomization(DefMECustomiz: string; Layout: TPanelLayout);
  var
     p: TNEntriesMainPanel;
     PanelCustomiz: ^TMEPanelCustomization;
     FilterStr: string;
     txtFilter: string;
  begin

     Strs.Clear;
     SplitString(Strs, DefMECustomiz, '/', false);
     i:= 0;
     for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
        if Layout = lQL then
            DefaultMECustomizForQL[p]:= DefaultNoteAdvOptions.DefaultMECustomizForQL[p]
        else
            DefaultMECustomizForEL[p]:= DefaultNoteAdvOptions.DefaultMECustomizForEL[p];

        if Strs[i] <> '' then begin
           if Layout = lQL then
              PanelCustomiz:= @DefaultMECustomizForQL[p]
           else
              PanelCustomiz:= @DefaultMECustomizForEL[p];

           Strs2.Clear;
           SplitString(Strs2, Strs[i], ':', false);

           with PanelCustomiz^ do begin
              Content:=            TContentInMultiEntryMode(StrToInt(Strs2[0]));
              ShowDateInHeader:=   Strs2[1] = BOOLEANSTR[true];
              ShowTimeInHeader:=   Strs2[2] = BOOLEANSTR[true];
              ShowTagsInHeader:=   Strs2[3] = BOOLEANSTR[true];
              ShowLineInHeader:=   Strs2[4] = BOOLEANSTR[true];
              CompactHeader:=      Strs2[5] = BOOLEANSTR[true];
              DescendingOrder:=    Strs2[6] = BOOLEANSTR[true];
              FoldedTextMode:=     TExportFoldedTextMode(StrToInt(Strs2[7]));
              if Strs2[8] <> '*' then begin
                 FilterStr:= Strs2[8];
                 Strs2.Clear;
                 SplitString(Strs2, FilterStr, ';', false);

                 with Filter do begin
                    Enabled:=        Strs2[0] = BOOLEANSTR[true];
                    TagsModeOR:=     Strs2[1] = BOOLEANSTR[true];
                    TagsText:=       Strs2[2] = BOOLEANSTR[true];
                    MatchCase:=      Strs2[3] = BOOLEANSTR[true];
                    WholeWordsOnly:= Strs2[4] = BOOLEANSTR[true];
                    ConsiderHidden:= Strs2[5] = BOOLEANSTR[true];
                    ShowExcerpts:=   Strs2[6] = BOOLEANSTR[true];
                    SearchMode:=     TSearchMode(StrToInt(Strs2[7]));
                    txtFilter:=      Strs2[8];
                    if txtFilter <> '' then begin
                       txtFilter:=  StringReplace(txtFilter, '\"', '"', [rfReplaceAll]);
                       txtFilter:=  txtFilter.Substring(1, Length(txtFilter)-2);
                       TextFilter:=     txtFilter;
                    end;
                    FindTagsIncl:=   TNoteTagArrayUtils.StringToFindTags(Strs2[9]);
                    FindTagsExcl:=   TNoteTagArrayUtils.StringToFindTags(Strs2[10]);
                 end;

              end;
           end;

        end;
        inc(i);
     end;

  end;



begin
   Self:= DefaultNoteAdvOptions;

   if Str = '' then exit;

   ErrorsLoading:= false;

   Strs:= TStringList.Create;
   Strs2:= TStringList.Create;
   try
    try
      SplitString(Strs, Str, '|', false);

      EnableAdvEditionInSingleEntryNotes:=      Strs[0] = BOOLEANSTR[true];
      AutoExpandInPanels:=                      Strs[1] = BOOLEANSTR[true];
      ShowNewestEntryAtStartup:=                Strs[2] = BOOLEANSTR[true];
      NewEntriesAlwaysOnEdLayout:=              Strs[3] = BOOLEANSTR[true];
      EditTagLinkedEntryInSelectedEntryPanel:=  Strs[4] = BOOLEANSTR[true];

      EditorInfoBarPos:=            TEditorInfoBarPos(StrToInt(Strs[5]));
      FirstLinesOfText_MaxLength:=     StrToInt(Strs[6]);
      FirstLinesOfText_MaxLines:=      StrToInt(Strs[7]);

      Config := TFormatSettings.Create('en-US');                             // -> 0.1234

      SizeRatiosQL.Top:=                  StrToFloatDef( Strs[8],  0.1354, Config);
      SizeRatiosQL.Bottom:=               StrToFloatDef( Strs[9],  0.15, Config);
      SizeRatiosQL.TLTR:=                 StrToFloatDef( Strs[10], 0.5, Config);
      SizeRatiosQL.BLBR:=                 StrToFloatDef( Strs[11], 0.5, Config);
      SizeRatiosQL.TopAutoFromHidden:=    StrToFloatDef( Strs[12], 0.15, Config);
      SizeRatiosQL.BottomAutoFromHidden:= StrToFloatDef( Strs[13], 0.15, Config);

      SizeRatiosEL.Top:=                  StrToFloatDef( Strs[14], 0.1354, Config);
      SizeRatiosEL.Bottom:=               StrToFloatDef( Strs[15], 0.15, Config);
      SizeRatiosEL.TLTR:=                 StrToFloatDef( Strs[16], 0.5, Config);
      SizeRatiosEL.BLBR:=                 StrToFloatDef( Strs[17], 0.5, Config);
      SizeRatiosEL.TopAutoFromHidden:=    StrToFloatDef( Strs[18], 0.15, Config);
      SizeRatiosEL.BottomAutoFromHidden:= StrToFloatDef( Strs[19], 0.15, Config);

      DefUseForQL:=         Strs[20];
      DefUseForEL:=         Strs[21];
      LinkedTagsQL:=        Strs[22];
      LinkedTagsEL:=        Strs[23];
      DefEnabledForQL:=     Strs[24];
      DefEnabledForEL:=     Strs[25];
      DefMECustomizForQL:=  Strs[26];
      DefMECustomizForEL:=  Strs[27];


      try
          LayoutOK:= false;

          Strs.Clear;
          SplitString(Strs, LinkedTagsQL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             LinkedTagsForQL[p]:=  TNoteTagArrayUtils.StringToTags(Strs[i]);
             inc(i);
          end;

          Strs.Clear;
          SplitString(Strs, DefUseForQL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             DefaultUseForQL[p]:=  TNEntriesPanelUse(StrToIntDef(Strs[i], 0));
             inc(i);
          end;

          Strs.Clear;
          SplitString(Strs, DefEnabledForQL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             DefaultEnabledForQL[p]:=  Strs[i] = BOOLEANSTR[true];
             inc(i);
          end;

          LayoutOK:= ValidateLayout(lQL);

      except
        on E: Exception do begin
           LayoutOK:= false;
           ErrorsLoading:= true;
        end;
      end;

      if not LayoutOK then begin
         DefaultUseForQL:= DefaultNoteAdvOptions.DefaultUseForQL;
         DefaultEnabledForQL:= DefaultNoteAdvOptions.DefaultEnabledForQL;
      end;


      try
          LayoutOK:= false;

          Strs.Clear;
          SplitString(Strs, LinkedTagsEL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             LinkedTagsForEL[p]:=  TNoteTagArrayUtils.StringToTags(Strs[i]);
             inc(i);
          end;

          Strs.Clear;
          SplitString(Strs, DefUseForEL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             DefaultUseForEL[p]:=  TNEntriesPanelUse(StrToIntDef(Strs[i], 0));
             inc(i);
          end;

          Strs.Clear;
          SplitString(Strs, DefEnabledForEL, '/', false);
          i:= 0;
          for p := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
             DefaultEnabledForEL[p]:=  Strs[i] = BOOLEANSTR[true];
             inc(i);
          end;

          LayoutOK:= ValidateLayout(lEL);

      except
        on E: Exception do begin
           LayoutOK:= false;
           ErrorsLoading:= true;
        end;
      end;

      if not LayoutOK then begin
         DefaultUseForEL:= DefaultNoteAdvOptions.DefaultUseForEL;
         DefaultEnabledForEL:= DefaultNoteAdvOptions.DefaultEnabledForEL;
      end;

      LoadMEPanelCustomization(DefMECustomizForQL, lQL);
      LoadMEPanelCustomization(DefMECustomizForEL, lEL);


    except
       On E: Exception do begin
          Self:= DefaultNoteAdvOptions;
          ErrorsLoading:= true;
       end;
    end;

   finally
      Strs.Free;
      Strs2.Free;
   end;
end;



function TNoteAdvancedOptions.ValidateLayout(Layout: TPanelLayout): boolean;
var
  pnl: TNEntriesMainPanel;
  Num_ShowSelectedEntry, Num_ShowAllEntries: integer;
  DefaultUseForLayout: TNEntriesPanelUseArray;
  LinkedTagsForLayout: TNoteTagArrayPanelArray;
  DefaultEnabledForLayout: TNEntriesPanelEnabledArray;
begin
  {
  Review the purposes for "Query Layout". Make sure to:
  - Select "All entries" in one (and only one) panel
  - Do not select more than one panel with "Selected entry"
  - Select "All entries" or "Selected entry" for panel C

  - Specify a tag for each "Linked to the tag[s]:"                       [Query Layout]
  - Specify a tag for each "Only if there is any entry with the tag[s]:" [Editing Layout]
  }

   Result:= False;

   Num_ShowSelectedEntry:= 0;
   Num_ShowAllEntries:= 0;

   if Layout = lQL then begin
      DefaultUseForLayout:=  DefaultUseForQL;
      LinkedTagsForLayout:= LinkedTagsForQL;
      DefaultEnabledForLayout:= DefaultEnabledForQL;
   end
   else begin
      DefaultUseForLayout:=  DefaultUseForEL;
      LinkedTagsForLayout:= LinkedTagsForEL;
      DefaultEnabledForLayout:= DefaultEnabledForEL;
   end;

   for pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
      case DefaultUseForLayout[pnl] of
         pnuShowTagLinkedEntries:   if (pnl = pnCenter) or (LinkedTagsForLayout[pnl] = nil) then exit;
         pnuShowSelectedEntry:      inc(Num_ShowSelectedEntry);
         pnuShowAllEntries:         begin
                                       if not DefaultEnabledForLayout[pnl] then exit;
                                       inc(Num_ShowAllEntries);
                                    end;
         pnuHidePanel:              if (pnl = pnCenter) then exit;
      end;
   end;

   if (Num_ShowAllEntries = 1) and (Num_ShowSelectedEntry <= 1) then
      Result:= True;

end;


Initialization

  _KNTUtilsDLL_FN := extractfilepath( ParamStr( 0 )) + 'kntutils.dll';
  {$IFDEF WITH_IE}
  _IE4Available := IsIE4Installed;
  {$ELSE}
  _IE4Available := false;
  {$ENDIF}



end.

