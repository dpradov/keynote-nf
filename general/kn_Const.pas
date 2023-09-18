unit kn_Const;

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
   Winapi.ShellAPI,
   Winapi.Messages,
   Vcl.Graphics,
   ZLibEx;


resourcestring
  FILTER_ALLFILES    = 'All files (*.*)|*.*';
  FILTER_EXECUTABLES = 'Programs|*.exe;*.com';
  LANGUAGE_DEFAULT = 'English (Internal)';

  STR_01_Defaults = 'New note';
  STR_02_Defaults = 'New node';
  STR_03_Formats = 'Keynote native';
  STR_04_Formats = 'Keynote encrypted';
  STR_06_Formats = 'Keynote compressed';
  STR_05_Formats = 'Dart Notes';
  STR_06_TabnoteKind = 'Standard Rich text editor';
  STR_07_TabnoteKind = 'Multi-level tree';
  STR_08_SearchMode = 'Exact phrase';
  STR_09_SearchMode = 'All the words';
  STR_10_SearchMode = 'Any of the words';
  STR_11_TreeSelection = 'Current node';
  STR_12_TreeSelection = 'Current node and subtree';
  STR_13_TreeSelection = 'Checked nodes';
  STR_14_TreeSelection = 'Full tree';
  STR_15_ExportFormat = 'Plain text';
  STR_16_ExportFormat = 'Rich text (RTF)';
  STR_62_ExportFormat = 'KeyNote File (knt)';
  STR_17_IconKind = 'None';
  STR_18_IconKind = 'Standard icons';
  STR_19_IconKind = 'Custom icons';
  STR_20_LinkType = 'Internet address';
  STR_21_LinkType = 'Email address';
  STR_22_LinkType = 'File or folder';
  STR_23_LinkType = 'KeyNote location';
  STR_24_ExpandMode = 'Show tree fully collapsed';
  STR_25_ExpandMode = 'Expand only last active node';
  STR_26_ExpandMode = 'Expand only top level nodes';
  STR_27_ExpandMode = 'Restore expanded state of all nodes';
  STR_28_ExpandMode = 'Show tree fully expanded';
  STR_29_ClipNodeNaming = 'Default node name';
  STR_30_ClipNodeNaming = 'Use clipboard text';
  STR_31_ClipNodeNaming = 'Use current date and time';
  STR_32_FactStr = 'Open file in KeyNote';
  STR_33_FactStr = 'Execute (macro or plugin)';
  STR_34_FactStr = 'Merge notes into current file';
  STR_35_FactStr = 'Import as a new note';
  STR_36_FactStr = 'Create hyperlink to file';
  STR_37_FactStr = 'Import as tree nodes';
  STR_38_FactStr = 'Import as virtual tree nodes';
  STR_39_FactStr = 'Import as Internet Explorer virtual node';
  STR_40_ImportHTML = 'No conversion (HTML source)';
  STR_41_ImportHTML = 'Use Shared HTML Text Converter (html32.cnv + msconv97.dll)';  // HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Shared Tools\Text Converters
  STR_42_ImportHTML = 'Use MS Word Converter';
  STR_43_ImportHTML = 'Use Internet Explorer';
  STR_58_ImportHTML = 'Use Microsoft HTML Converter (html.iec)';
  STR_44_Symb = 'Euro';
  STR_45_Symb = 'Copyright';
  STR_46_Symb = 'Registered trademark';
  STR_47_Symb = 'Trademark';
  STR_48_Symb = 'Paragraph';
  STR_49_Symb = 'Degree';
  STR_50_Symb = 'Plus/minus';
  STR_51_Symb = 'Dots';
  STR_52_Symb = 'French parenthesis (left)';
  STR_53_Symb = 'French parenthesis (right)';
  STR_54_Compression = 'None';
  STR_55_Compression = 'Fastest';
  STR_56_Compression = 'Default';
  STR_57_Compression = 'Max';
  STR_58_ClipPlainTextMode = 'Plain (without any formatting)';
  STR_59_ClipPlainTextMode = 'Only hyperlinks (without other formatting)';
  STR_60_ClipPlainTextMode = 'Only font style (bold, italic, ...)';
  STR_61_ClipPlainTextMode = 'Only font (without paragraph formatting)';

procedure DefineConst;

const
  Program_Name     = 'KeyNote NF';
  Program_Version  = '1.8.0 Beta 6';
  Program_Version_Number  = '1.8.0.6';
  Program_Version_Date    = '06/09/2023';
  Program_License  = 'Free software, Open Source (Mozilla Public License 2.0)';

  Program_URL     = 'https://github.com/dpradov/keynote-nf'; //'http://keynote.prv.pl';
  Program_Email1  = 'dprado.keynote@gmail.com';
  Program_Email2  = 'marekjed@users.sourceforge.net';
  Program_Credit1 = 'Copyright © 2007-23  Daniel Prado Velasco   (since 1.7.0)';
  Program_Credit2 = 'Copyright © 2000-05  Marek Jedlinski';

  UniqueAppName_KEYNOTE10   = 'GFKeyNote10';

const
  URL_Issues = 'https://github.com/dpradov/keynote-nf/issues';

const
  _GF_CLWINDOW = $D0D0D0;
  _GF_NAVY     = clNavy;
  _GF_PURPLE   = clPurple;
  _GF_BLUE     = clBlue;
  _GF_BLACK    = clBlack;


{ Experimental stuff for RichEdit v.3 }
const
  EM_GETZOOM = WM_USER+224;
  EM_SETZOOM = WM_USER+225;
  EM_SETTYPOGRAPHYOPTIONS	= WM_USER + 202;
  TO_ADVANCEDTYPOGRAPHY = 1;

const
  // various global declarations
  _IS_OLD_KEYNOTE_FILE_FORMAT  : boolean = false;
  _USE_OLD_KEYNOTE_FILE_FORMAT : boolean = false;
  _TEST_KEYNOTE_FILE_VERSION   : boolean = true;
  _ALLOW_VCL_UPDATES           : boolean = false;
  _ALLOW_TREE_FONT_COLOR       : boolean = true;
  _OTHER_INSTANCE_HANDLE       : hwnd = 0;
  _SAVE_RESTORE_CARETPOS       : boolean = false;
  _DEFAULT_KEY_REPLAY_DELAY    : integer = 250; // miliseconds!!
  _LISTBOX_ALT_COLOR           : TColor = clInfoBk;

const
  _PLUGIN_FOLDER     = 'plugins\';
  _MACRO_FOLDER      = 'macros\';
  _TEMPLATE_FOLDER   = 'templates\';
  _LANGUAGE_FOLDER   = 'Lang\';

const
  // help file topics
  _HLP_KNTFILES      = 70;
  // _HELP_DRAGDROP     = ??;

const
  TOKEN_NEWLINE = '\n';
  TOKEN_TAB     = '\t';

const
  _REMOVABLE_MEDIA_VNODES_DENY  = 0;
  _REMOVABLE_MEDIA_VNODES_WARN  = 1;
  _REMOVABLE_MEDIA_VNODES_ALLOW = 2;

const
  NFHDR_ID           = 'GFKNT'; // KeyNote file header ID
  NFHDR_ID_OLD       = 'GFKNX'; // KeyNote token text file header ID
  NFHDR_ID_ENCRYPTED = 'GFKNE'; // encrypted KeyNote file header ID
  NFHDR_ID_COMPRESSED = 'GFKNZ'; // compressed KeyNote file header ID
  NFILEVERSION_MAJOR = '2';     // and version numbers
  // NFILEVERSION_MAJOR = '1';     // non-tree version ID, obsolete
  NFILEVERSION_MINOR = '0';     //
  NENCFILEVER_MAJOR  = '1';     // and version numbers
  NENCFILEVER_MINOR  = '0';     //

const
  FLAGS_STRING_LENGTH  = 24; // can store up to 24 booleans
  DEFAULT_FLAGS_STRING = '000000000000000000000000';
  TREENOTE_FLAG_BASE   = 12; // chars 1-12 are for both types of notes; chars 13-24 are only used by tree-type notes

type
  TFlagsString = string[FLAGS_STRING_LENGTH];

const // IDs for older file versions
  NFILEVERSION_MAJOR_NOTREE = '1'; // MAJOR version number for new format files that do NOT have trees
  NFILEVERSION_MAJOR_OLD    = '1'; // version numbers for old-style file format
  NFILEVERSION_MINOR_OLD    = '0'; // DO NOT CHANGE!

const
  _NF_AID = '!'; // Application ID
  _NF_DCR = 'C'; // Date Created
  _NF_CNT = '*'; // Count of notes
  _NF_FCO = '?'; // File comment
  _NF_FDE = '/'; // File description
  _NF_ACT = '$'; // Active note
  _NF_ReadOnlyOpen  = 'R'; // open file as read-only
  _NF_ShowTabIcons  = 'I'; // per-FILE setting
  _NF_ClipCapNote   = 'L'; // index of ClipCapNote
  _NF_TrayIconFile  = 'T'; // tray icon file to load
  _NF_TabIconsFile  = 'F'; // tab icons file to load
  _NF_FileFlags     = '^'; // "flags" string (24 boolean values)

const
  _NF_Icons_BuiltIn = '@';


const
  BOOLEANSTR            : array[false..true] of AnsiChar = ( '0', '1' );
  DEF_INDENT_LEN        = 12;
  MIN_COMBO_LENGTH      = 15;
  DEF_TAB_SIZE          = 4;  // default tab size
  DEF_UNDO_LIMIT        = 32; // undo levels
  TABNOTE_NAME_LENGTH   = 50; // max name length for the note title (ie Tab caption)
  TREENODE_NAME_LENGTH  = 255; // max name length for a tree node
  TREENODE_NAME_LENGTH_CAPTURE  = 60; // max name length for a tree node, when capturing from Clipboard or from a selection (new node from selection)
  MAX_COMMENT_LENGTH    = 80;
  MAX_FILENAME_LENGTH   = 127;
  MAX_BOOKMARKS         = 9; // ZERO-based!
  DEFAULT_CAPACITY      = 255; // default capacity for RTF Lines property (to speed up loading)
  DEFAULT_NEW_NOTE_NAME : string = STR_01_Defaults; // default name for new notes
  DEFAULT_NEW_NODE_NAME : string = STR_02_Defaults; // default name for new tree nodes
  DEFAULT_NODE_IMGINDEX : integer = 0;
  TRRENODE_SELIDX       = 4;
  MIN_PASS_LEN          = 5; // minimum length of file access passphrase
  MAX_BACKUP_LEVEL      = 9; // max number of backup files to keep
  MAX_NAVHISTORY_COUNT  = 100; // number of navigation history locations to maintain

const
  _TokenChar            = '%';

const
  // tokens for new tree nodes
  NODEINSDATE           = '%D'; // expands to current date
  NODEINSTIME           = '%T'; // expands to current time
  NODECOUNT             = '%C'; // expands to count of nodes
  NODELEVEL             = '%L'; // expands to node's level
  NODEABSINDEX          = '%A'; // expands to node's absolute index
  NODEINDEX             = '%I'; // expands to node's index
  NODEPARENT            = '%P'; // expands to node's parent's name
  NODENOTENAME          = '%N'; // expands to name of current note
  NODEFILENAME          = '%F'; // expands to current file name

const
  // tokens for exporting to text and rtf
  EXP_NOTENAME          = 'N';
  EXP_NODENAME          = 'D';
  EXP_NODELEVEL         = 'L';
  EXP_NODEINDEX         = 'I';
  EXP_FILENAME          = 'F';
  EXP_RTF_HEADING       = _TokenChar + 'HEADING';

  _DefaultNodeHeading = '--- ' + _TokenChar + EXP_NODENAME + ' ---';
  _DefaultNoteHeading = '=== ' + _TokenChar + EXP_NOTENAME + ' ===';



const

  _Default_NoteHeadingTpl =   AnsiString(
    '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil\fcharset1{\*\fname Tahoma;}Tahoma;}}' + #13#10 +
    '{\colortbl ;\red96\green96\blue96;}' + #13#10 +
    '\viewkind4\uc1\pard\cf1\f0\fs32\par' + #13#10 +
    '\b %HEADING\par' + #13#10 +
    '\b0\par' + #13#10 +
    '}');
  _Default_NodeHeadingTpl =  AnsiString(
    '{\rtf1\ansi\deff0{\fonttbl{\f0\fnil\fcharset1{\*\fname Tahoma;}Tahoma;}}' + #13#10 +
    '{\colortbl ;\red96\green96\blue96;}' + #13#10 +
    '\viewkind4\uc1\pard\cf1\f0\fs24\par' + #13#10 +
    '\ul\b %HEADING\ulnone\b0\par' + #13#10 +
    '\par' + #13#10 +
    '}');

const
  _VIRTUAL_NODE_ERROR_CHAR = '?';
  _ZOOM_STEP = 10;
  _ZOOM_MIN = 5;

const
{
   *1  If source URL will not be shown, then it will also be ignored the text included between the enclosure delimiters.
       It is possible to define several fragments enclosed with that delimiters.
       More info in: 'Improvements on Clip Web and Clipboard Capture #532' (https://github.com/dpradov/keynote-nf/issues/532)
   *2  It will try to get the title for the URL with WebBrowser (offline, using cache), without time limit (6 seconds, for security..)
   *3  As *2, but trying only for a limited time (for now 2 seconds)

   Normally the title will be figured out in very short time, but in the event that the delay is annoying, it is possible to
   use %s instead of %S, to wait for 2 seconds as much. Or even, to use %U, to not try to get Title.
   Besides, KeyNote will cache the last URLs/Titles (20/10) to agilize the paste in case you need to paste several times,
   possibly intercalating from several pages

   *4  Only if URL will not visible, because a hyperlink will be created showing the title, and if the server/domain of the URL
       is not contained in the title, then that server domain will be shown, in square brackets. Some examples:

      'https://www.teoria.com/en/tutorials/functions/intro/03-dom-sub.php', 'Harmonic Functions : The Dominant and Subdominant'  => [teoria.com]
      'https://musicnetmaterials.wordpress.com/analisis-musical/', 'Análisis musical | Musicnetmaterials'                        => ''
      'https://stackoverflow.com/questions/1549145/case-insensitive-pos', 'delphi - case insensitive Pos - Stack Overflow'       => ''
      'https://martinfowler.com/eaaCatalog/', 'Catalog of Patterns of Enterprise Application Architecture'                       => [martinfowler.com]
      'https://en.wikipedia.org/wiki/Johann_Sebastian_Bach', 'Johann Sebastian Bach - Wikipedia'                                 => ''
      'https://www.youtube.com/watch?v=r0R6gMw2s44', 'El Círculo de Quintas: Una explicación detallada | Versión 2.0'            => [YouTube]

   *5 Delimits a divider string for the second and following consecutive paste from the same page (same URL). 
      KNT will remember the last URL from wich pasted in the current [Clipboard Capture] session
}

  // tokens for clipboard capture separator line
  CLIPDIVCHAR           = '^';  // replaced with 1 line break
  CLIPDATECHAR          = NODEINSDATE; // inserts date
  CLIPTIMECHAR          = NODEINSTIME; // inserts time
  CLIPSOURCEDELIMITER   = '%|';   // Encloses source (optional)                    *1
  CLIPSOURCE            = '%S';   // insert Source URL (with title)                *2
  CLIPSOURCE_LIMITED    = '%s';   // insert source URL (with title, limited time)  *3
  CLIPSOURCE_ONLY_URL   = '%U';   // insert source URL (without title)
  CLIPSOURCE_DOMAIN     = '%d';   // insert Source server/domain                   *4
  CLIPSECONDDIV         = '%%';   // delimits divider for 2nd+ (same URL)          *5



const
  // tokens for the MailOptions.FirstLine string
  // (placed as first line of the email sent from keyNote)
  // These tokens may also be used in the Subject line of the email
  MAILFILENAME          = '%F'; // expands to name of attached file (not: KeyNote file, because that is irrelevant)
  MAILNOTENAME          = '%N'; // expands to name of current note
  MAILKEYNOTEVERSION    = '%V'; // expands to KeyNote plug
  MAILNOTECOUNT         = '%C'; // expands to number of notes being sent

const
  _NF_TabNote         = '%';    // end-of-record (new TTabNote begins)
  _NF_EOF             = '%%';   // end-of-file (last line in file)
  _NF_RTF             = '%:';   // end of note header; RTF data follows
  _NF_TreeNote        = '%+';   // TTreeNote begins
  _NF_TRN             = '%-';   // Tree node begins (many tree nodes within a single note)
  _NF_COMMENT         = '#';    // comment, but this is really used for file header information
  _NF_WARNING         = _NF_COMMENT + ' This is an automatically generated file. Do not edit.';
  _NF_PLAINTEXTLEADER = ';';

const
  _SHORTDATEFMT  = 'dd-MM-yyyy'; // all these are only internal defaults
  _LONGDATEFMT   = 'd MMMM yyyy';
  _LONGTIMEFMT   = 'HH:mm:ss';
  _DATESEPARATOR = '-';
  _TIMESEPARATOR = ':';
  _CRLF          = #13#10;

const
  // function key customization
  _KEY_FUNC_DELIMITER = '|';
  _KEY_FUNC_MACRO     = 'M';
  _KEY_FUNC_PLUGIN    = 'P';
  _KEY_FUNC_STYLE     = 'S';
  _KEY_FUNC_FONT      = 'F';
  _KEY_FUNC_TEMPLATE  = 'T';


const
  _NODE_COLOR_COUNT = 8; // times 2, dark and bright shades
  _NODE_COLORS_DARK : array[0.._NODE_COLOR_COUNT] of TColor = (
    clDefault, // 0
    clBlack,   // 1
    clMaroon,  // 2
    clGreen,   // 3
    clOlive,   // 4
    clNavy,    // 5
    clPurple,  // 6
    clTeal,    // 7
    clGray     // 8
  );

  _NODE_COLORS_LIGHT : array[0.._NODE_COLOR_COUNT] of TColor = (
    clDefault, // 9 (unused!)
    clSilver,  // 10
    clRed,     // 11
    clLime,    // 12
    clYellow,  // 13
    clBlue,    // 14
    clFuchsia, // 15
    clAqua,    // 16
    clWhite    // 17
  );

{$IFDEF WITH_DART}
const
  // Dart Notes specific defines
  _DART_STOP  = #7;
  _DART_ID    = 'Notes';
  _DART_VER   = '1670';
  _DART_VEROK = '871';
{$ENDIF}

const
  // indices for tree node icons
  ICON_FOLDER          = 0;
  ICON_BOOK            = 2;
  ICON_NOTE            = 4;
  ICON_VIRTUAL         = 6;
  ICON_VIRTUAL_KNT_NODE = 8;

  {
  ICON_VIRTUALTEXT     = 6; // same as ICON_VIRTUAL
  ICON_VIRTUALRTF      = 8;
  ICON_VIRTUALHTML     = 10;
  }
  ICON_VIRTUALIELOCAL  = 8;  { not implemented }
  ICON_VIRTUALIEREMOTE = 10; { not implemented }

type
  TVirtualMode = (
    vmNone, vmText, vmRTF, vmHTML, vmIELocal, vmIERemote, vmKNTNode
  );


const
  // menu item tags for dynamically associated commands
  ITEM_STYLE_APPLY = 90;
  ITEM_STYLE_RENAME = 91;
  ITEM_STYLE_DELETE = 92;
  ITEM_STYLE_REDEFINE = 93;
  ITEM_STYLE_DESCRIBE = 94;

  ITEM_TAG_TRIMLEFT  = 1;
  ITEM_TAG_TRIMRIGHT = 2;
  ITEM_TAG_TRIMBOTH  = 3;


type

  // WITH_DART: From now, DartNotes format is unsupported by default

  // supported save/load formats
  TNoteFileFormat = (
    nffKeyNote, nffKeyNoteZip, nffEncrypted {$IFDEF WITH_DART}, nffDartNotes{$ENDIF}
  );

type
  TNoteType = (
    ntRTF, // standard RichEdit control
    ntTree // tree panel plus richedit control (tree-type note)
  );
  //TNoteNameStr = String[TABNOTE_NAME_LENGTH];
  TNoteNameStr = string;

const
  TABNOTE_KIND_IDS : array[TNoteType] of string = (
    'RTF',
    'TRN'
  );

type
  TNavDirection = (
    navUp, navDown, navLeft, navRight
  );

type
  TSearchMode = (
    smPhrase, smAll, smAny
  );

type
  TPasteNodeNameMode = (
    pnnClipboard, pnnDate, pnnTime, pnnDateTime, pnnSelection
  );

type
  TTreeSelection = (
    tsNode, tsSubtree, tsCheckedNodes, tsFullTree
  );

const
  TREE_SELECTION_NAMES : array[TTreeSelection] of string = (
    STR_11_TreeSelection, STR_12_TreeSelection, STR_13_TreeSelection, STR_14_TreeSelection
  );

type
  TExportFmt = (
    xfPlainText, xfRTF, xfHTML,
    xfKeyNote,
    xfTreePad
  );

const
  EXPORT_FORMAT_NAMES : array[TExportFmt] of string = (
    STR_15_ExportFormat,
    STR_16_ExportFormat,
    'HTML',
    STR_62_ExportFormat,
    'TreePad'
  );

type
  TExportSource = (
    expCurrentNote, expAllNotes, expSelectedNotes
  );

type
  TNodeIconKind = (
    niNone, niStandard, niCustom 
  );

const
  NODE_ICON_KINDS : array[TNodeIconKind] of string = (
    STR_17_IconKind, STR_18_IconKind, STR_19_IconKind
  );

{
const
  KNT_URL_PREFIX = 'knt://'; NOT USED
}

type
  TLinkType = (
    lnkURL, lnkEmail, lnkFile, lnkKNT
  );
  // IMPORTANT: lnkKNT MUST be LAST in the sequence, i.e.
  // must be equal to "high( TLinkType )"
  // kn_Hyperlink.pas relies on it while creating the form controls

const
  LINK_TYPES : array[TLinkType] of string = (
    STR_20_LinkType,
    STR_21_LinkType,
    STR_22_LinkType,
    STR_23_LinkType
  );


const
  KNTLOCATION_MARK_OLD = '?'; // old style links to KNT locations: use note and node names
  KNTLOCATION_MARK_NEW = '*'; // new style links to KNT locations: use note and node IDs

  (* 
    *1
    The hidden strings used by KNT will have a variable length, but will normally have a maximum of the form maximum: HT999999H
    where H:KNT_RTF_HIDDEN_MARK_CHAR, T: Type (B:Bookmark, T:Tag, ...)    
    A target bookmark will normally be something like HB5H, HB15H, ... In this second example it is assumed that there would be
    at least 15 different destinations of internal KNT links in the node.    
    But we can use the format to record other hidden information to process, such as perhaps a tag to associate with a
    text selection. If we save the ID of the tag, it would not be normal for us to go from an ID > 999999.        
    In any case, the objective of this constant is to identify the presence of the character that we use as the beginning of the
    the hidden string, in a -we assume- non-hidden way, by not finding the closing character at an expected distance.       
    We could make sure that the character is really hidden by asking the RichEdit control, but since it's a character
    totally unusual to find in a text, we will follow this criterion in principle.

   Example: \v\'11B5\'12\v0   In Delphi, debugging, it is shown as '#$11'B5'#$12' (counted '#$11' or '#$12' as one character)
   Although we can write RTF in an equivalent way ( {\v\'11B5\'12} ), the RichEdit control will translate it in the old way, with \v and \v0
  *)

  KNT_RTF_HIDDEN_MARK_L = '\''11';
  KNT_RTF_HIDDEN_MARK_R = '\''12';
  KNT_RTF_HIDDEN_MARK_L_CHAR = Chr(17);    // 17 ($11): DC1 (Device Control 1)
  KNT_RTF_HIDDEN_MARK_R_CHAR = Chr(18);    // 18 ($12): DC2 (Device Control 2)
  KNT_RTF_HIDDEN_BOOKMARK = 'B';
  KNT_RTF_HIDDEN_MAX_LENGHT = 10;         // *1

type
  TKNTURL = (
    // urlKNT, UNUSED custom knt:// URL scheme which we use for internal keynote links (only used with version 3 of riched20.dll)
    urlUndefined,
    urlFile, urlHTTP, urlHTTPS, urlFTP, urlMailto,
    urlTelnet, urlNews, urlNNTP, urlGopher, urlWais, urlProspero,
    urlNotes, urlCallto, urlOnenote, urlOutlook, urlTel, urlWebcal, urlOTHER
  );

const
  KNT_URLS : array[TKNTURL] of AnsiString = (
    '',
    {'knt:', } 'file:', 'http:', 'https:', 'ftp:', 'mailto:',
    'telnet:', 'news:', 'nntp:', 'gopher:', 'wais:', 'prospero:',
    'notes:', 'callto:', 'onenote:', 'outlook:', 'tel:', 'webcal:', '????:'
  );

type
  // supported enbcryption algorithms.
  // KeyNote uses DCPCrypt library by David Barton
  // http://www.devarchive.com/DCPcrypt.html
  // (do a web search for "DCPCrypt" if this link goes 404)
  TCryptMethod = (
    tcmBlowfish, tcmIdea
  );

const
  CRYPT_METHOD_NAMES : array[TCryptMethod] of string = (
    'Blowfish', 'Idea'
  );

const
  ID_STR_LENGTH = 5; // GFKNT, GFKNX, GFKNE, GFKNZ, Notes

type
  // In encrypted files, this is saved in cleartext
  TNoteFileVersion = packed record
    ID : array[1..ID_STR_LENGTH] of AnsiChar;
    Major, Minor : AnsiChar;
  end;

type
  // In encrypted files, this is saved in cleartext
  TEncryptedFileInfo = packed record
    Method : TCryptMethod;
    DataSize : integer;
    NoteCount : integer;
  end;

type
  TCommentStr = String;

type
  TNodeInsertMode = (
    tnTop,
    tnInsertBefore,
    tnAddLast,
    tnAddChild,
    tnAddAfter
  );

type
  TTreeExpandMode = (
    txmFullCollapse, // show tree fully collapsed
    txmActiveNode, // expand only the node that was last active
    txmTopLevelOnly, // expand only top level nodes
    txmExact, // show nodes expanded or collapsed exactly as saved
    txmFullExpand // show tree fully expanded
  );

const
  TREE_EXPAND_MODES : array[TTreeExpandMode] of string = (
    STR_24_ExpandMode,
    STR_25_ExpandMode,
    STR_26_ExpandMode,
    STR_27_ExpandMode,
    STR_28_ExpandMode
  );

type
  TClipNodeNaming = (
    clnDefault, clnClipboard, clnDateTime
  );

const
  CLIP_NODE_NAMINGS : array[TClipNodeNaming] of string = (
    STR_29_ClipNodeNaming,
    STR_30_ClipNodeNaming,
    STR_31_ClipNodeNaming
  );


type
  TClipPlainTextMode = (
    clptPlainText,        // paste without any formatting
    clptAllowHyperlink,   // paste without any formatting, just allowing hyperlinks
    clptAllowFontStyle,   // Allow only font styles like bold and italic, e.g
    clptAllowFont         // disallow paragraph formatting, but allow font formatting
  );

const
  CLIP_PLAIN_TEXT_MODE : array[TClipPlainTextMode] of string = (
    STR_58_ClipPlainTextMode,
    STR_59_ClipPlainTextMode,
    STR_60_ClipPlainTextMode,
    STR_61_ClipPlainTextMode
  );


type
   TCopyFormatMode = (cfDisabled, cfEnabled, cfEnabledMulti );

const
   crCopyFormat = 1;


const
  // TREEPAD export/import constants
  _TREEPAD_HEADER_TXT = '<Treepad version 2.7>';
  _TREEPAD_HEADER_RTF = '<Treepad version 3.1>';
  _TREEPAD_MAGIC      = '5P9i0s8y19Z';
  _TREEPAD_NODE       = '<node>';
  _TREEPAD_ENDNODE    = '<end node> ' + _TREEPAD_MAGIC;
  _TREEPAD_TXTNODE    = 'dt=text';
  _TREEPAD_RTFNODE    = 'dt=RTF';

type
  TFocusMemory = ( focNil, focRTF, focTree );

type
  TDropFileAction = (
    factUnknown,
    factOpen,
    factExecute, // macros and plugins only
    factMerge,
    factHyperlink,
    factImport,
    factImportAsNode,
    factMakeVirtualNode
    {$IFDEF WITH_IE}
    ,
    factMakeVirtualIENode
    {$ENDIF}

  );
  TDropFileActions = array[TDropFileAction] of boolean;

const
  FactStrings : array[TDropFileAction] of string = (
    '',
    STR_32_FactStr,
    STR_33_FactStr,
    STR_34_FactStr,
    STR_36_FactStr,
    STR_35_FactStr,
    STR_37_FactStr,
    STR_38_FactStr
    {$IFDEF WITH_IE}
    ,
    STR_39_FactStr
    {$ENDIF}

  );


type
  THTMLImportMethod = (
    htmlSource, htmlSharedTextConv, htmlMSWord, htmlIE
  );

  THTMLExportMethod = (
    htmlExpMicrosoftHTMLConverter, htmlExpMSWord
  );


type
  TDartNotesHdr = packed record
    BlockLen : integer;
    ID : array[1..ID_STR_LENGTH] of char;
    TabCount : integer; // NOT IN DART NOTES FILE HEADER!
    Ver : integer;
    MinVer1 : integer;
    MinVer2 : integer;
    LastTabIdx : integer;
  end;

type
  TNoteChromeIniStr = record
    Section,
    BGColor,
    BGHiColor,
    FontCharset,
    FontColor,
    FontHiColor,
    FontHiStyle,
    FontName,
    FontSize,
    FontStyle : string;
  end;

const
  NoteChromeIniStr : TNoteChromeIniStr = (
    Section : 'NoteChrome';
    BGColor : 'BGColor';
    BGHiColor : 'BGHiColor';
    FontCharset : 'FontCharset';
    FontColor : 'FontColor';
    FontHiColor : 'FontHiColor';
    FontHiStyle : 'FontHiStyle';
    FontName : 'FontName';
    FontSize : 'FontSize';
    FontStyle : 'FontStyle'
  );

const
  // common tokens for all notes
  _AutoIndent = 'AI';
  _BookMarks = 'BM';
  _CHBGColor = 'BG';
  _CHBGHiColor = 'BH';
  _CHFontCHarset = 'CH';
  _CHFontColor = 'FC';
  _CHFontHiColor = 'FH';
  _CHFontHiStyle = 'SH';
  _CHFontName = 'FN';
  _CHFontSize = 'FS';
  _CHFontStyle = 'ST';
  _CHLanguage = 'LN';
  _DateCreated = 'DC';
  _ImageIndex = 'II';
  _Info = 'NI';
  _LineCount = 'LC';
  _Lines = 'LI';
  _NoteKind = 'NK';
  _NoteName = 'NN';
  _NoteID = 'ID';
  _PosX = 'CX';
  _POSY = 'CY';
  _TabIndex = 'TI';
  _TabSize = 'TS';
  _Flags = 'FL';
  _NodeFlags = 'NF';

  (* UNUSED, replaced by _Flags: *)
  _ReadOnly = 'RO';
  _WordWrap = 'WW';
  _URLDetect = 'UD';
  _Visible = 'NV';
  _UseTabChar = 'UT';

  _AutoNumberNodes = 'AN';
  _ShowCheckBoxes = 'SC';
  _ShowIcons = 'SI';


const
  // special TTreeNote tokens
  _NodeName = 'ND';
  _NodeID = 'DI';
  _NodeLevel = 'LV';
  _SelectedNode = 'SN';
  _TreeWidth = 'TW';
  _VirtualFN = 'VF';
  _RelativeVirtualFN = 'RV';
  _NodeRTFBGColor = 'BC';
  _DefaultNodeName = 'EN';
  _NodeSelStart = 'SS';
  _NodeColor = 'HC';
  _NodeBGColor = 'HB';
  _NodeFontFace = 'FF';
  _CHTRBGColor = 'TB';
  _CHTRFontCharset = 'TH';
  _CHTRFontColor = 'TC';
  _CHTRFontName = 'TN';
  _CHTRFontSize = 'TZ';
  _CHTRFontStyle = 'TY';
  _NodeImageIndex = 'IX';
  _NodeAlarm = 'NA';        // [dpv]
  _VirtualNode = 'VN';

const
  // special FlagsStr constant characters
  _FlagHasNodeColor = 'N';
  _FlagHasNodeBGColor = 'B';
  _FlagHasNodeColorBoth = 'A';
  _FlagHasNodeColorNone = '0';


const
  alph13 = AnsiString('abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz');
  alph13UP = AnsiString('ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ');

{$IFDEF WITH_IE}
type
  TIECommand = (
    ieNone, ieNavigate, ieStop, ieRefresh,
    ieGoHome, ieGoBack, ieGoNext, ieGoBlank
  );
{$ENDIF}

(*
t - ShortTieFormat
tt - LongTimeFormat

c - ShortDateFormat + LongTimeFormat

ddddd - ShortDateFormat
dddddd - LongDateFormat


=====================


*)


type
  TSymbolCode = record
    Code : byte;
    Name : string;
  end;

const
  SYMBOL_CODE_LIST : array[1..10] of char = (
    #128, // Euro
    #169, // copy
    #174, // reg
    #153, // tm
    #167, // para
    #176, // deg
    #177, // plusminus
    #133, // dots
    #171, // left french brace
    #187 // right french brace
  );

var
  FILE_FORMAT_NAMES : array[TNoteFileFormat] of string;
  FILE_COMPRESSION_LEVEL : array[TZCompressionLevel] of string;
  TABNOTE_KIND_NAMES : array[TNoteType] of string;
  SEARCH_MODES : array[TSearchMode] of string;
  SYMBOL_NAME_LIST : array[1..10] of string;
  HTMLImportMethods : array[THTMLImportMethod] of string;
  HTMLExportMethods : array[THTMLExportMethod] of string;

implementation

procedure DefineConst;
begin
  FILE_FORMAT_NAMES[nffKeyNote]:=  STR_03_Formats;
  FILE_FORMAT_NAMES[nffKeyNoteZip]:=   STR_06_Formats;
  FILE_FORMAT_NAMES[nffEncrypted]:=    STR_04_Formats;
{$IFDEF WITH_DART}
  FILE_FORMAT_NAMES[nffDartNotes]:=    STR_05_Formats;
{$ENDIF}
  FILE_COMPRESSION_LEVEL[zcNone]:= STR_54_Compression;
  FILE_COMPRESSION_LEVEL[zcFastest]:= STR_55_Compression;
  FILE_COMPRESSION_LEVEL[zcDefault]:= STR_56_Compression;
  FILE_COMPRESSION_LEVEL[zcMax]:= STR_57_Compression;

  TABNOTE_KIND_NAMES[ntRTF]:=    STR_06_TabnoteKind;
  TABNOTE_KIND_NAMES[ntTree]:=   STR_07_TabnoteKind;

  SEARCH_MODES[smPhrase] := STR_08_SearchMode;
  SEARCH_MODES[smAll] := STR_09_SearchMode;
  SEARCH_MODES[smAny] := STR_10_SearchMode;

  SYMBOL_NAME_LIST[1]:=   STR_44_Symb;
  SYMBOL_NAME_LIST[2]:=   STR_45_Symb;
  SYMBOL_NAME_LIST[3]:=   STR_46_Symb;
  SYMBOL_NAME_LIST[4]:=   STR_47_Symb;
  SYMBOL_NAME_LIST[5]:=   STR_48_Symb;
  SYMBOL_NAME_LIST[6]:=   STR_49_Symb;
  SYMBOL_NAME_LIST[7]:=   STR_50_Symb;
  SYMBOL_NAME_LIST[8]:=   STR_51_Symb;
  SYMBOL_NAME_LIST[9]:=   STR_52_Symb;
  SYMBOL_NAME_LIST[10]:=  STR_53_Symb;

  HTMLImportMethods[htmlSource]:=         STR_40_ImportHTML;
  HTMLImportMethods[htmlSharedTextConv]:= STR_41_ImportHTML;
  HTMLImportMethods[htmlMSWord]:=         STR_42_ImportHTML;
  HTMLImportMethods[htmlIE]:=             STR_43_ImportHTML;

  HTMLExportMethods[htmlExpMicrosoftHTMLConverter]:= STR_58_ImportHTML;
  HTMLExportMethods[htmlExpMSWord]:=                 STR_42_ImportHTML;
end;

Initialization
  DefineConst;

end.


