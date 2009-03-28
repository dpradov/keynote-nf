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

unit kn_INI;

interface
uses kn_Info, kn_Const, RxRichEd,
  gf_Const, gf_misc, gf_files;

procedure LoadKeyNoteOptions(
    const INIFileName : string;
    var KeyOptions : TKeyOptions;
    var TabOptions : TTabOptions;
    var FindOptions : TFindOptions;
    var EditorOptions : TEditorOptions;
    var ClipOptions : TClipOptions;
    var TreeOptions : TKNTTreeOptions;
    var ResPanelOptions : TResPanelOptions
  );

procedure SaveKeyNoteOptions(
    const INIFileName : string;
    const KeyOptions : TKeyOptions;
    const TabOptions : TTabOptions;
    const FindOptions : TFindOptions;
    const EditorOptions : TEditorOptions;
    const ClipOptions : TClipOptions;
    const TreeOptions : TKNTTreeOptions;
    const ResPanelOptions : TResPanelOptions
  );


procedure SaveKeyNoteDefaults(
  const INIFileName : string;
  const DefaultEditorProperties : TNoteEditorProperties;
  const DefaultEditorChrome : TChrome;
  const DefaultTabProperties : TNoteTabProperties;
  const DefaultTreeProperties : TNoteTreeProperties;
  const DefaultTreeChrome : TChrome
  );

procedure LoadKeyNoteDefaults(
  const OnlyChrome : boolean;
  const INIFileName : string;
  var DefaultEditorProperties : TNoteEditorProperties;
  var DefaultEditorChrome : TChrome;
  var DefaultTabProperties : TNoteTabProperties;
  var DefaultTreeProperties : TNoteTreeProperties;
  var DefaultTreeChrome : TChrome
  );


type
  TEditorOptionsIniStr = packed record
    Section,
    AutoFont,
    AutoIndent,
    AutoKeyboard,
    DisableINSKey,
    EditProtected,
    FontSizeInc,
    IndentInc,
    ParaSpaceInc,
    SaveCaretPos,
    TrackCaretPos,
    TrackStyle,
    TrackStyleRange,
    UndoLimit,
    WordCountTrack,
    WordSelect,
    WordsPerPage : string;
  end;

const
  EditorOptionsIniStr : TEditorOptionsIniStr = (
    Section : 'EditorOptions';
    AutoFont : 'AutoFont';
    AutoIndent : 'AutoIndent';
    AutoKeyboard : 'AutoKeyboard';
    DisableINSKey : 'DisableINSKey';
    EditProtected : 'EditProtected';
    FontSizeInc : 'FontSizeInc';
    IndentInc : 'IndentInc';
    ParaSpaceInc : 'ParaSpaceInc';
    SaveCaretPos : 'SaveCaretPos';
    TrackCaretPos : 'TrackCaretPos';
    TrackStyle : 'TrackStyle';
    TrackStyleRange : 'TrackStyleRange';
    UndoLimit : 'UndoLimit';
    WordCountTrack : 'WordCountTrack';
    WordSelect : 'WordSelect';
    WordsPerPage : 'WordsPerPage';
  );


type
  TChromeIniStr = packed record
    Section,
    BGColor,
    FontCharset,
    FontColor,
    FontName,
    FontSize,
    FontStyle,
    HiColor,
    HiFontCharset,
    HiFontColor,
    HiFontName,
    HiFontSize,
    HiFontStyle,
    Language : string;
  end;

const
  ChromeIniStr : TChromeIniStr = (
    Section : 'Chrome';
    BGColor : 'BGColor';
    FontCharset : 'FontCharset';
    FontColor : 'FontColor';
    FontName : 'FontName';
    FontSize : 'FontSize';
    FontStyle : 'FontStyle';
    HiColor : 'HiColor';
    HiFontCharset : 'HiFontCharset';
    HiFontColor : 'HiFontColor';
    HiFontName : 'HiFontName';
    HiFontSize : 'HiFontSize';
    HiFontStyle : 'HiFontStyle';
    Language : 'Language';
  );

type
  TNoteEditorPropertiesIniStr = packed record
    Section,
    PlainText,
    TabSize,
    URLDetect,
    UseTabChar,
    WordWrap : string;
  end;

const
  NoteEditorPropertiesIniStr : TNoteEditorPropertiesIniStr = (
    Section : 'NoteEditorProperties';
    PlainText : 'PlainText';
    TabSize : 'TabSize';
    URLDetect : 'URLDetect';
    UseTabChar : 'UseTabChar';
    WordWrap : 'WordWrap'
  );


type
  TNoteTabPropertiesIniStr = packed record
    Section,
    ImageIndex,
    Name : string;
  end;

const
  NoteTabPropertiesIniStr : TNoteTabPropertiesIniStr = (
    Section : 'NoteTabProperties';
    ImageIndex : 'ImageIndex';
    Name : 'Name'
  );

type
  TNoteTreePropertiesIniStr = packed record
    Section,
    AutoNumberNodes,
    DefaultName,
    ShowCheckBoxes,
    IconKind,
    VerticalLayout,
    HideChecked : string;     // [dpv]
  end;

const
  NoteTreePropertiesIniStr : TNoteTreePropertiesIniStr = (
    Section : 'NoteTreeProperties';
    AutoNumberNodes : 'AutoNumberNodes';
    DefaultName : 'DefaultName';
    ShowCheckBoxes : 'ShowCheckBoxes';
    IconKind : 'IconKind';
    VerticalLayout : 'VerticalLayout';
    HideChecked : 'HideChecked';        // [dpv]
  );

procedure SaveMailOptions( const INI_FN : string; const MailOptions : TMailOptions );
procedure LoadMailOptions( const INI_FN : string; var MailOptions : TMailOptions );

procedure InitializeKeyOptions( var Struct : TKeyOptions );
procedure InitializeTabOptions( var Struct : TTabOptions );
procedure InitializeFindOptions( var Struct : TFindOptions );
procedure InitializeMailOptions( var Struct : TMailOptions );
procedure InitializeClipOptions( var Struct : TClipOptions );
procedure InitializeEditorOptions( var Struct : TEditorOptions );
procedure InitializeResPanelOptions( var Struct : TResPanelOptions );
procedure InitializeExportOptions( var Struct : TExportOptions );
procedure InitializeTreeOptions( var Struct : TKNTTreeOptions );

type
  TClipOptionsIniStr = packed record
    Section,
    Divider,
    IgnoreSelf,
    InsertSourceURL,
    MaxSize,
    // NewNodeName,
    ClipNodeNaming,
    PasteAsText,
    PasteAsNewNode,
    PlaySound,
    Recall,
    SleepTime,
    SwitchIcon,
    TestDupClips,
    TreeClipConfirm,
    URLOnly,
    WCDivider,
    WCPasteAsText : string;
  end;

const
  ClipOptionsIniStr : TClipOptionsIniStr = (
    Section : 'Clipboard capture';
    Divider : 'Divider';
    IgnoreSelf : 'IgnoreSelf';
    InsertSourceURL : 'InsertSourceURL';
    MaxSize : 'MaxSize';
    // NewNodeName : 'NewNodeName';
    ClipNodeNaming : 'ClipNodeNaming';
    PasteAsText : 'PasteAsText';
    PasteAsNewNode : 'PasteAsNewNode';
    PlaySound : 'PlaySound';
    Recall : 'Recall';
    SleepTime : 'SleepTime';
    SwitchIcon :'SwitchIcon';
    TestDupClips : 'TestDupClips';
    TreeClipConfirm : 'TreeClipConfirm';
    URLOnly : 'URLOnly';
    WCDivider : 'WCDivider';
    WCPasteAsText : 'WCPasteAsText'
  );


type
  TKeyOptionsIniStr = packed record
    Section,
    AlwaysOnTop,
    AutoNewFile,
    AutoRegisterFileType,
    AutoRegisterPrompt,
    AutoSave,
    AutoSaveOnFocus,
    AutoSaveOnTimer,
    AutoSaveOnTimerInt,
    Backup,
    BackupAppendExt,
    BackupDir,
    BackupExt,
    BackupLevel,
    BackupVNodes,
    ColorDlgBig,
    ComboFontLen,
    ComboMacroLen,
    ComboStyleLen,
    ComboDropDownCount,
    ConfirmExit,
    ConfirmTreePaste,
    ConfirmTabDelete,
    DateFmt,
    Debug,
    DebugLogAppend,
    DefaultNoteType,
    DisableAlarmPopup,
    DisableFileMon,
    DropNodesOnTabMove,
    DropNodesOnTabPrompt,
    DTLastDateFmt,
    DTLastTimeFmt,
    DTUseLastSelection,
    EncFileAltExt,
    EscAction,
    EvalAutoPaste,
    AutoPastePlugin,
    ExtHTML,
    ExtRTF,
    ExtText,
    FixScrollBars,
    HotKey,
    HotKeyActivate,
    HotKeyWarn,
    // HTML32CNVLocation,
    HTMLImportMethod,
    IgnoreUpgrades,
    ImportFileNamesWithExt,
    InitFontColor,
    InitHiColor,
    InsCharFullSet,
    InsCharKeepFont,
    InsCharWinClose,
    KeyReplayDelay,
    LanguageUI,
    LastCopyPath,
    LastExportPath,
    LastExportFormat,
    LastExportAsk,
    LastFile,
    LastImportPath,
    LastNumbering,
    LastVersion,
    LoadLastFile,
    LoadUserFile,
    LongCombos,
    MgrFullPaths,
    MinimizeOnClose,
    MinimizeOnURL,
    MRUCount,
    MRUFullPaths,
    MRUSubmenu,
    MRUUse,
    NodeNameHistory,
    NoRegistry,
    OpenFloppyReadOnly,
    OpenNetworkReadOnly,
    OpenReadOnlyWarn,
    PlaySoundOnAlarm,
    RecentLanguage,
    ResolveLNK,
    ResPanelActiveUpdate,
    ResPanelLeft,
    ResPanelShow,
    RichEditv3,
    RunAutoMacros,
    SafePrint,
    SaveDARTWarn,
    SaveDefaultFormat,
    ShellExecuteShowAllErrors,
    ShowFonts,
    // ShowSplash,
    ShowTooltips,
    SingleInstance,
    SkipNewFilePrompt,
    StartMinimized,
    StartNoteType,
    StatBarDlbClkAction,
    StatBarDlbClkActionShft,
    StatBarShow,
    StyleShowSamples,
    TabNameHistory,
    TimeFmt,
    TimerMinimize,
    TimerMinimizeInt,
    TimerClose,
    TimerCloseDialogs,
    TimerCloseEncOnly,
    TimerCloseInt,
    TimerCloseAutoReopen,
    TipOfTheDay,
    TipOfTheDayIdx,
    ToolbarFormatShow,
    ToolbarInsertShow,
    ToolbarMacroShow,
    ToolbarMainShow,
    ToolbarStyleShow,
    ToolbarTreeShow,
    UASEnable,
    UASPath,
    URLAction,
    URLAltBrowserPath,
    URLFileAuto,
    URLFileDecodeSpaces,
    URLFileNoPrefix,
    URLFileQuoteSpaces,
    URLClickShift,
    URLSystemBrowser, 
    UseOldColorDlg,
    UseOldFileFormat,
    UseNewStyleURL,
    UseTray,
    UserFile,
    ZoomIncrement : string;
  end;

const
  KeyOptionsIniStr : TKeyOptionsIniStr = (
    Section : 'KeyOptions';
    AlwaysOnTop : 'AlwaysOnTop';
    AutoNewFile : 'AutoNewFile';
    AutoRegisterFileType : 'AutoRegisterFileType';
    AutoRegisterPrompt : 'AutoRegisterPrompt';
    AutoSave : 'AutoSave';
    AutoSaveOnFocus : 'AutoSaveOnFocus';
    AutoSaveOnTimer : 'AutoSaveOnTimer';
    AutoSaveOnTimerInt : 'AutoSaveOnTimerInt';
    Backup : 'Backup';
    BackupAppendExt : 'BackupAppendExt';
    BackupDir : 'BackupDir';
    BackupExt : 'BackupExt';
    BackupLevel : 'BackupLevel';
    BackupVNodes : 'BackupVNodes';
    ColorDlgBig : 'ColorDlgBig';
    ComboFontLen : 'ComboFontLen';
    ComboMacroLen : 'ComboMacroLen';
    ComboStyleLen : 'ComboStyleLen';
    ComboDropDownCount : 'ComboDropDownCount';
    ConfirmExit : 'ConfirmExit';
    ConfirmTreePaste : 'ConfirmTreePaste';
    ConfirmTabDelete : 'ConfirmTabDelete';
    DateFmt : 'DateFmt';
    Debug : 'Debug';
    DebugLogAppend : 'DebugLogAppend';
    DefaultNoteType : 'DefaultNoteType';
    DisableAlarmPopup: 'DisableAlarmPopup';
    DisableFileMon : 'DisableFileMon';
    DropNodesOnTabMove : 'DropNodesOnTabMove';
    DropNodesOnTabPrompt : 'DropNodesOnTabPrompt';
    DTLastDateFmt : 'DTLastDateFmt';
    DTLastTimeFmt : 'DTLastTimeFmt';
    DTUseLastSelection : 'DTUseLastSelection';
    EncFileAltExt : 'EncFileAltExt';
    EscAction : 'EscAction';
    EvalAutoPaste : 'EvalAutoPaste';
    AutoPastePlugin : 'AutoPastePlugin';
    ExtHTML : 'ExtHTML';
    ExtRTF : 'ExtRTF';
    ExtText : 'ExtText';
    FixScrollBars : 'FixScrollBars';
    HotKey : 'HotKey';
    HotKeyActivate : 'HotKeyActivate';
    HotKeyWarn : 'HotKeyWarn';
    // HTML32CNVLocation : 'HTML32CNVLocation';
    HTMLImportMethod : 'HTMLImportMethod';
    IgnoreUpgrades : 'IgnoreUpgrades';
    ImportFileNamesWithExt : 'ImportFileNamesWithExt';
    InitFontColor : 'InitFontColor';
    InitHiColor : 'InitHiColor';
    InsCharFullSet : 'InsCharFullSet';
    InsCharKeepFont : 'InsCharKeepFont';
    InsCharWinClose : 'InsCharWinClose';
    KeyReplayDelay : 'KeyReplayDelay';
    LanguageUI : 'LanguageUI';
    LastCopyPath : 'LastCopyPath';
    LastExportPath : 'LastExportPath';
    LastExportFormat : 'LastExportFormat';
    LastExportAsk : 'LastExportAsk';
    LastFile : 'LastFile';
    LastImportPath : 'LastImportPath';
    LastNumbering : 'LastNumbering';
    LastVersion : 'LastVersion';
    LoadLastFile : 'LoadLastFile';
    LoadUserFile : 'LoadUserFile';
    LongCombos : 'LongCombos';
    MgrFullPaths : 'MgrFullPaths';
    MinimizeOnClose : 'MinimizeOnClose';
    MinimizeOnURL : 'MinimizeOnURL';
    MRUCount : 'MRUCount';
    MRUFullPaths : 'MRUFullPaths';
    MRUSubmenu : 'MRUSubmenu';
    MRUUse : 'MRUUse';
    NodeNameHistory : 'NodeNameHistory';
    NoRegistry : 'NoRegistry';
    OpenFloppyReadOnly : 'OpenFloppyReadOnly';
    OpenNetworkReadOnly : 'OpenNetworkReadOnly';
    OpenReadOnlyWarn : 'OpenReadOnlyWarn';
    PlaySoundOnAlarm: 'PlaySoundOnAlarm';
    RecentLanguage : 'RecentLanguage';
    ResolveLNK : 'ResolveLNK';
    ResPanelActiveUpdate : 'ResPanelActiveUpdate';
    ResPanelLeft : 'ResPanelLeft';
    ResPanelShow : 'ResPanelShow';
    RichEditv3 : 'RichEditv3';
    RunAutoMacros : 'RunAutoMacros';
    SafePrint : 'SafePrint';
    SaveDARTWarn : 'SaveDARTWarn';
    SaveDefaultFormat : 'SaveDefaultFormat';
    ShellExecuteShowAllErrors : 'ShellExecuteShowAllErrors';
    ShowFonts : 'ShowFonts';
    // ShowSplash : 'ShowSplash';
    ShowTooltips : 'ShowTooltips';
    SingleInstance : 'SingleInstance';
    SkipNewFilePrompt : 'SkipNewFilePrompt';
    StartMinimized : 'StartMinimized';
    StartNoteType : 'StartNoteType';
    StatBarDlbClkAction : 'StatBarDlbClkAction';
    StatBarDlbClkActionShft : 'StatBarDlbClkActionShft';
    StatBarShow : 'StatBarShow';
    StyleShowSamples : 'StyleShowSamples';
    TabNameHistory : 'TabNameHistory';
    TimeFmt : 'TimeFmt';
    TimerMinimize : 'TimerMinimize';
    TimerMinimizeInt : 'TimerMinimizeInt';
    TimerClose : 'TimerClose';
    TimerCloseDialogs : 'TimerCloseDialogs';
    TimerCloseEncOnly : 'TimerCloseEncOnly';
    TimerCloseInt : 'TimerCloseInt';
    TimerCloseAutoReopen : 'TimerCloseAutoReopen';
    TipOfTheDay : 'TipOfTheDay';
    TipOfTheDayIdx : 'TipOfTheDayIdx';
    ToolbarFormatShow : 'ToolbarFormatShow';
    ToolbarInsertShow : 'ToolbarInsertShow';
    ToolbarMacroShow : 'ToolbarMacroShow';
    ToolbarMainShow : 'ToolbarMainShow';
    ToolbarStyleShow : 'ToolbarStyleShow';
    ToolbarTreeShow : 'ToolbarTreeShow';
    UASEnable : 'UASEnable';
    UASPath : 'UASPath';
    URLAction : 'URLAction';
    URLAltBrowserPath : 'URLAltBrowserPath';
    URLFileAuto : 'URLFileAuto';
    URLFileDecodeSpaces : 'URLFileDecodeSpaces';
    URLFileNoPrefix : 'URLFileNoPrefix';
    URLFileQuoteSpaces : 'URLFileQuoteSpaces';
    URLClickShift : 'URLClickShift';
    URLSystemBrowser : 'URLSystemBrowser';
    UseOldColorDlg : 'UseOldColorDlg';
    UseOldFileFormat : 'UseOldFileFormat';
    UseNewStyleURL : 'UseNewStyleURL';
    UseTray : 'UseTray';
    UserFile : 'UserFile';
    ZoomIncrement : 'ZoomIncrement';
  );


type
  TMailOptionsIniStr = packed record
    Section,
    AddrBook,
    AsPlainText,
    CCAddr,
    FirstLine,
    FromAddr,
    History,
    KeepLog,
    SMTPPort,
    SMTPServer,
    Subject,
    SubjectPrefix,
    TextCharSet,
    Timeout,
    ToAddr,
    XMailer : string;
  end;

const
  MailOptionsIniStr : TMailOptionsIniStr = (
    Section : 'MailOptions';
    AddrBook : 'AddrBook';
    AsPlainText : 'AsPlainText';
    CCAddr : 'CCAddr';
    FirstLine : 'FirstLine';
    FromAddr : 'FromAddr';
    History : 'History';
    KeepLog : 'KeepLog';
    SMTPPort : 'SMTPPort';
    SMTPServer : 'SMTPServer';
    Subject : 'Subject';
    SubjectPrefix : 'SubjectPrefix';
    TextCharSet : 'TextCharSet';
    Timeout : 'Timeout';
    ToAddr : 'ToAddr';
    XMailer : 'Self'
  );


type
  TTabOptionsIniStr = packed record
    Section,
    ActiveColor,
    ColorAllTabs,
    Font,
    HotTrack,
    Images,
    InactiveColor,
    Stacked,
    TabsAreButtons,
    TabOrientation : string;

  end;

const
  TabOptionsIniStr : TTabOptionsIniStr = (
    Section : 'TabOptions';
    ActiveColor : 'ActiveColor';
    ColorAllTabs : 'ColorAllTabs';
    Font : 'Font';
    HotTrack : 'HotTrack';
    Images : 'Images';
    InactiveColor : 'InactiveColor';
    Stacked : 'Stacked';
    TabsAreButtons : 'TabsAreButtons';
    TabOrientation : 'TabOrientation'
  );


type
  TTreeOptionsIniStr = packed record
    Section,
    AutoNameVNodes,
    AutoScroll,
    ConfirmNodeDelete,
    ConfirmNodeRefresh,
    ExpandMode,
    FullRowSelect,
    HotTrack,
    EditInPlace,
    EditNewNodes,
    InheritNodeBG,
    InheritNodeProperties,
    NodeDelimiter,
    PathTopToBottom,
    RemovableMediaVNodes,
    ShowFullPath,
    ShowFullPathSearch,
    ShowTooltips,
    TopLevelCheck : string;
  end;

const
  TreeOptionsIniStr : TTreeOptionsIniStr = (
    Section : 'TreeOptions';
    AutoNameVNodes : 'AutoNameVNodes';
    AutoScroll : 'AutoScroll';
    ConfirmNodeDelete : 'ConfirmNodeDelete';
    ConfirmNodeRefresh : 'ConfirmNodeRefresh';
    ExpandMode : 'ExpandMode';
    FullRowSelect : 'FullRowSelect';
    HotTrack : 'HotTrack';
    EditInPlace : 'EditInPlace';
    EditNewNodes : 'EditNewNodes';
    InheritNodeBG : 'InheritNodeBG';
    InheritNodeProperties : 'InheritNodeProperties';
    NodeDelimiter : 'NodeDelimiter';
    PathTopToBottom : 'PathTopToBottom';
    RemovableMediaVNodes : 'RemovableMediaVNodes';
    ShowFullPath : 'ShowFullPath';
    ShowFullPathSearch : 'ShowFullPathSearch';
    ShowTooltips : 'ShowTooltips';
    TopLevelCheck : 'TopLevelCheck';
  );


type
  TFindOptionsIniStr = packed record
    Section,
    AllNodes,
    AllTabs,
    AutoClose,
    EntireScope,
    FindAllHistory,
    FindAllMatches,
    FindNew,
    History,
    HistoryMaxCnt,
    LastFindCol,
    LastFindRow,
    LastFindTab,
    MatchCase,
    Pattern,
    ReplaceConfirm,
    ReplaceHistory,
    ReplacePattern,
    ReplaceWith,
    SearchMode,
    SearchNodeNames,
    WholeWordsOnly,
    WordAtCursor,
    WordCharacters,
    Wrap : string;
  end;

const
  FindOptionsIniStr : TFindOptionsIniStr = (
    Section : 'FindOptions';
    AllNodes : 'AllNodes';
    AllTabs : 'AllTabs';
    AutoClose : 'AutoClose';
    EntireScope : 'EntireScope';
    FindAllHistory : 'FindAllHistory';
    FindAllMatches : 'FindAllMatches';
    FindNew : 'FindNew';
    History : 'History';
    HistoryMaxCnt : 'HistoryMaxCnt';
    LastFindCol : 'LastFindCol';
    LastFindRow : 'LastFindRow';
    LastFindTab : 'LastFindTab';
    MatchCase : 'MatchCase';
    Pattern : 'Pattern';
    ReplaceConfirm : 'ReplaceConfirm';
    ReplaceHistory : 'ReplaceHistory';
    ReplacePattern : 'ReplacePattern';
    ReplaceWith : 'ReplaceWith';
    SearchMode : 'SearchMode';
    SearchNodeNames : 'SearchNodeNames';
    WholeWordsOnly : 'WholeWordsOnly';
    WordAtCursor : 'WordAtCur';
    WordCharacters : 'WordCharacters';
    Wrap : 'Wrap'
  );

type
  TResPanelOptionsIniStr = record
    Section,
    ColorFindList,
    FindListAltColor,
    ShowFind,
    ShowMacro,
    ShowPlugin,
    ShowScratch,
    ShowTemplate,
    ShowFavorites,
    Stacked,
    TabOrientation : string;
  end;

// --- INISTR const: -------
const
  ResPanelOptionsIniStr : TResPanelOptionsIniStr = (
    Section : 'ResPanelOptions';
    ColorFindList : 'ColorFindList';
    FindListAltColor : 'FindListAltColor';
    ShowFind : 'ShowFind';
    ShowMacro : 'ShowMacro';
    ShowPlugin : 'ShowPlugin';
    ShowScratch : 'ShowScratch';
    ShowTemplate : 'ShowTemplate';
    ShowFavorites : 'ShowFavorites';
    Stacked : 'Stacked';
    TabOrientation : 'TabOrientation'
  );


type
  TExportOptionsIniStr = record
    Section,
    ConfirmFilenames,
    ConfirmOverwrite,
    ExportPath,
    ExportSource,
    HTMLNoFormatting,
    IncludeNodeHeadings,
    IncludeNoteHeadings,
    {
    IndentNestedNodes,
    IndentUsingTabs,
    IndentValue,
    }
    NodeHeading,
    NoteHeading,
    SingleNodeFiles,
    TargetFormat,
    TreePadForceMaster,
    TreePadRTF,
    TreePadSingleFile,
    TreeSelection,
    ExcludeHiddenNodes: string;     // [dpv]
  end;

const
  ExportOptionsIniStr : TExportOptionsIniStr = (
    Section : 'ExportOptions';
    ConfirmFilenames : 'ConfirmFilenames';
    ConfirmOverwrite : 'ConfirmOverwrite';
    ExportPath : 'ExportPath';
    ExportSource : 'ExportSource';
    HTMLNoFormatting : 'HTMLNoFormatting';
    IncludeNodeHeadings : 'IncludeNodeHeadings';
    IncludeNoteHeadings : 'IncludeNoteHeadings';
    {
    IndentNestedNodes : 'IndentNestedNodes';
    IndentUsingTabs : 'IndentUsingTabs';
    IndentValue : 'IndentValue';
    }
    NodeHeading : 'NodeHeading';
    NoteHeading : 'NoteHeading';
    SingleNodeFiles : 'SingleNodeFiles';
    TargetFormat : 'TargetFormat';
    TreePadForceMaster : 'TreePadForceMaster';
    TreePadRTF : 'TreePadRTF';
    TreePadSingleFile : 'TreePadSingleFile';
    TreeSelection : 'TreeSelection';
    ExcludeHiddenNodes: 'ExcludeHiddenNodes';     // [dpv]
  );

implementation
uses Windows, SysUtils, Graphics, Menus, IniFiles, kn_NoteObj;

resourcestring
  STR_INIMail_01 = 'Attached file: %F';


procedure InitializeClipOptions( var Struct : TClipOptions );
begin
  with Struct do
  begin
    Divider := CLIPDIVCHAR;
    IgnoreSelf := true;
    InsertSourceURL := true;
    MaxSize := DEF_CLIP_CAP_MAX_SIZE;
    // NewNodeName := '';
    ClipNodeNaming := clnClipboard;
    PasteAsText := false;
    PasteAsNewNode := true;
    PlaySound := true;
    Recall := false;
    SleepTime := 5; // 5 = half a second
    SwitchIcon := true;
    TestDupClips := true;
    TreeClipConfirm := true;
    URLOnly := false;
    WCDivider := '^--- %D, %T ---^';
    WCPasteAsText := true;
  end;
end; // InitializeClipOptions

procedure InitializeEditorOptions( var Struct : TEditorOptions );
begin
  with Struct do
  begin
    AutoFont := true;
    AutoIndent := false;
    AutoKeyboard := false;
    DisableINSKey := false;
    EditProtected := true;
    FontSizeInc := 1;
    IndentInc := DEF_INDENT_LEN;
    ParaSpaceInc := 2;
    SaveCaretPos := true;
    TrackCaretPos := true;
    TrackStyle := false;
    TrackStyleRange := srBoth;
    UndoLimit := DEF_UNDO_LIMIT;
    WordCountTrack := false;
    WordSelect := true;
    WordsPerPage := 250;
  end;
  _SAVE_RESTORE_CARETPOS := Struct.SaveCaretPos;
end; // InitializeEditorOptions


procedure InitializeKeyOptions( var Struct : TKeyOptions );
begin
  with Struct do
  begin
    AlwaysOnTop := false;
    AutoNewFile := true;
    AutoRegisterFileType := false;
    AutoRegisterPrompt := true;
    AutoSave := true;
    AutoSaveOnFocus := true;
    AutoSaveOnTimer := false;
    AutoSaveOnTimerInt := 10;
    Backup := true;
    BackupAppendExt := true;
    BackupDir := ''; // default: backup in the same directory as original file
    BackupExt := ext_BAK;
    BackupVNodes := true;
    BackupLevel := 1;
    ColorDlgBig := true;
    ComboFontLen := 0;
    ComboMacroLen := 0;
    ComboStyleLen := 0;
    ComboDropDownCount := 16;
    ConfirmExit := false;
    ConfirmTreePaste := true;
    ConfirmTabDelete := true;
    Debug := false;
    DebugLogAppend := true;
    DefaultNoteType := ntTree; // by default, preselect Tree-type note
    DisableAlarmPopup:= false;
    DisableFileMon := false;
    DropNodesOnTabMove := false;
    DropNodesOnTabPrompt := true;
    DTLastDateFmt := '';
    DTLastTimeFmt := '';
    DTUseLastSelection := true;
    EncFileAltExt := false;
    EscAction := ESC_MINIMIZE;
    AutoPasteEval := true;
    AutoPastePlugin := false;
    ExtHTML := Def_HTML_Extensions;
    ExtRTF := Def_RTF_Extensions;
    ExtText := Def_Text_Extensions;
    FixScrollBars := false;
    HotKey := DEFAULT_HOTKEY; // Ctrl+Shift+F12
    HotKeyActivate := true;
    HotKeyWarn := true;
    // HTML32CNVLocation := '';
    HTMLImportMethod := low( HTMLImportMethod );
    IgnoreUpgrades := false;
    ImportFileNamesWithExt := false;
    InitFontColor := clWindowText;
    InitHiColor := clInfoBK;
    InsCharFullSet := false;
    InsCharKeepFont := true;
    InsCharWinClose := false;
    LanguageUI := LANGUAGE_DEFAULT;   // Default (English Internal)
    LastCopyPath := '';
    LastExportPath := '';
    LastExportFormat := low( TExportFmt );
    LastExportAsk := false;
    LastFile := '';
    LastImportPath := '';
    LastNumbering := nsArabicNumbers;
    LastVersion := Program_VerStr;
    LoadLastFile := true;
    LoadUserFile := false;
    LongCombos := false;
    KeyReplayDelay := _DEFAULT_KEY_REPLAY_DELAY;
    DateFmt := _LONGDATEFMT;
    MgrFullPaths := false;
    MinimizeOnClose := false;
    MinimizeOnURL := false;
    MRUCount := 5;
    MRUFullPaths := true;
    MRUSubmenu := true;
    MRUUse := true;
    NodeNameHistory := '';
    NoRegistry := true;
    OpenFloppyReadOnly := false;
    OpenNetworkReadOnly := false;
    OpenReadOnlyWarn := true;
    PlaySoundOnAlarm:= true;
    RecentLanguage := GetSystemDefaultLCID;
    ResolveLNK := false; // for Favorites: keep .LNK, not not resolve to actual file or document
    ResPanelActiveUpdate := true;
    ResPanelLeft := false; // default to resource panel on right side
    ResPanelShow := true;
    RichEditv3 := ( _LoadedRichEditVersion > 2 );
    { if (( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) and
      ( Win32MajorVersion = 4 ) and ( Win32MinorVersion = 0 )) then
      // Windows 95
    else
      // later versions }

    RunAutoMacros := false;
    SafePrint := true;
    SaveDARTWarn := true;
    SaveDefaultFormat := nffKeyNote;
    ShellExecuteShowAllErrors := true;
    ShowFonts := true;
    // ShowSplash := true;
    ShowTooltips := true;
    SingleInstance := true;
    SkipNewFilePrompt := false;
    StartMinimized := false;
    StartNoteType := ntTree;
    StatBarDlbClkAction := DBLCLK_RESPANEL;
    StatBarDlbClkActionShft := DBLCLK_FILEPROP;
    StatBarShow := true;
    StyleShowSamples := false;
    TabNameHistory := '';
    TimeFmt := _LONGTIMEFMT;
    TimerMinimize := false;
    TimerMinimizeInt := DEF_TIMERMINIMIZEINT;
    TimerClose := false;
    TimerCloseDialogs := true;
    TimerCloseEncOnly := true;
    TimerCloseInt := DEF_TIMERCLOSEINT;
    TimerCloseAutoReopen := true;
    TipOfTheDay := true;
    TipOfTheDayIdx := -1;
    ToolbarFormatShow := true;
    ToolbarInsertShow := false;
    ToolbarMacroShow := false;
    ToolbarMainShow := true;
    ToolbarStyleShow := true;
    ToolbarTreeShow := true;
    UASEnable := false;
    UASPath := '';
    URLAction := urlAsk;
    URLAltBrowserPath := GetAppFromExt( ext_HTML, true );
    URLFileAuto := true;
    URLFileDecodeSpaces := true;
    URLFileNoPrefix := true;
    URLFileQuoteSpaces := false;
    URLClickShift := false;
    URLSystemBrowser := true;
    UseOldColorDlg := false;
    UseOldFileFormat := false;
    UseTray := true;
    UserFile := '';
    ZoomIncrement := 10;
  end;
end; // InitializeKeyOptions

procedure InitializeTabOptions( var Struct : TTabOptions );
begin
  with Struct do
  begin
    ActiveColor := clBtnFace;
    ColorAllTabs := true;
    InactiveColor := _GF_CLWINDOW;;
    with Font do
    begin
      FSize := 8;
      FColor := clWindowText;
      FName := 'MS Sans Serif';
      FCharset := DEFAULT_CHARSET;
      FStyle := [];
    end;
    HotTrack := true;
    Images := true;
    Stacked := false;
    TabOrientation := tabposTop;
  end;
end; // InitializeTabOptions


procedure InitializeFindOptions( var Struct : TFindOptions );
begin
  with Struct do
  begin
    HiddenNodes:= false;    // [dpv]
    AllNodes := false;
    AllTabs := false;
    AutoClose := false;
    EntireScope := false;
    FindAllHistory := '';
    FindAllMatches := false;
    FindNew := true;
    History := '';
    HistoryMaxCnt := DEFAULT_HISTORY_COUNT;
    MatchCase := false;
    Pattern := '';
    ReplaceConfirm := false;
    ReplaceHistory := '';
    ReplacePattern := '';
    ReplaceWith := '';
    SearchMode := low( SearchMode );
    SearchNodeNames := false;
    WholeWordsOnly := false;
    WordAtCursor := true;
    Wrap := false;
  end;
end; // InitializeFindOptions

procedure InitializeMailOptions( var Struct : TMailOptions );
begin
  with Struct do
  begin
    AddrBook := '';
    AsPlainText := true;
    CCAddr := '';
    FirstLine := STR_INIMail_01;
    FromAddr := '';
    History := '';
    KeepLog := true;
    SMTPPort := 'smtp'; // 25
    SMTPServer := '';
    Subject := '';
    SubjectPrefix := '';
    TextCharSet := 'iso-8859-1';
    Timeout := 30;
    ToAddr := '';
    XMailer := Program_Name + ' ' + Program_VerStr + '  http://code.google.com/p/keynote-nf/';
  end;
end; // InitializeMailOptions


procedure InitializeTreeOptions( var Struct : TKNTTreeOptions );
begin
  with Struct do
  begin
    AutoNameVNodes := true;
    AutoScroll := false;
    ConfirmNodeDelete := true;
    ConfirmNodeRefresh := true;
    ExpandMode := txmExact;
    FullRowSelect := false;
    HotTrack := true;
    EditInPlace := true;
    EditNewNodes := true;
    InheritNodeBG := false;
    InheritNodeProperties := true;
    NodeDelimiter := ' / ';
    PathTopToBottom := true;
    RemovableMediaVNodes := _REMOVABLE_MEDIA_VNODES_WARN;
    ShowFullPath := true;
    ShowFullPathSearch := false;
    ShowTooltips := true;
    TopLevelCheck := false;
  end;
end; // InitializeTreeOptions


procedure SaveKeyNoteOptions(
    const INIFileName : string;
    const KeyOptions : TKeyOptions;
    const TabOptions : TTabOptions;
    const FindOptions : TFindOptions;
    const EditorOptions : TEditorOptions;
    const ClipOptions : TClipOptions;
    const TreeOptions : TKNTTreeOptions;
    const ResPanelOptions : TResPanelOptions
  );
var
  IniFile : TIniFile;
  section : string;
begin
  IniFile := TIniFile.Create( INIFileName );
  try
    with IniFile do
    begin
      section := KeyOptionsIniStr.section;
      writebool( section, KeyOptionsIniStr.AlwaysOnTop, KeyOptions.AlwaysOnTop );
      writebool( section, KeyOptionsIniStr.AutoNewFile, KeyOptions.AutoNewFile );
      writebool( section, KeyOptionsIniStr.AutoRegisterFileType, KeyOptions.AutoRegisterFileType );
      writebool( section, KeyOptionsIniStr.AutoRegisterPrompt, KeyOptions.AutoRegisterPrompt );
      writebool( section, KeyOptionsIniStr.AutoSave, KeyOptions.AutoSave );
      writebool( section, KeyOptionsIniStr.AutoSaveOnFocus, KeyOptions.AutoSaveOnFocus );
      writebool( section, KeyOptionsIniStr.AutoSaveOnTimer, KeyOptions.AutoSaveOnTimer );
      writeinteger( section, KeyOptionsIniStr.AutoSaveOnTimerInt, KeyOptions.AutoSaveOnTimerInt );
      writebool( section, KeyOptionsIniStr.Backup, KeyOptions.Backup );
      writebool( section, KeyOptionsIniStr.BackupAppendExt, KeyOptions.BackupAppendExt );
      writestring( section, KeyOptionsIniStr.BackupDir, KeyOptions.BackupDir );
      writestring( section, KeyOptionsIniStr.BackupExt, KeyOptions.BackupExt );
      writebool( section, KeyOptionsIniStr.BackupVNodes, KeyOptions.BackupVNodes );
      writeinteger( section, KeyOptionsIniStr.BackupLevel, KeyOptions.BackupLevel );
      writebool( section, KeyOptionsIniStr.ColorDlgBig, KeyOptions.ColorDlgBig );
      writeinteger( section, KeyOptionsIniStr.ComboDropDownCount, KeyOptions.ComboDropDownCount );
      writebool( section, KeyOptionsIniStr.ConfirmExit, KeyOptions.ConfirmExit );
      writebool( section, KeyOptionsIniStr.ConfirmTreePaste, KeyOptions.ConfirmTreePaste );
      writebool( section, KeyOptionsIniStr.ConfirmTabDelete, KeyOptions.ConfirmTabDelete );
      writebool( section, KeyOptionsIniStr.Debug, KeyOptions.Debug );
      writebool( section, KeyOptionsIniStr.DebugLogAppend, KeyOptions.DebugLogAppend );
      writeinteger( section, KeyOptionsIniStr.DefaultNoteType, ord( KeyOptions.DefaultNoteType ));
      writebool( section, KeyOptionsIniStr.DisableAlarmPopup, KeyOptions.DisableAlarmPopup );
      writebool( section, KeyOptionsIniStr.DisableFileMon, KeyOptions.DisableFileMon );
      writebool( section, KeyOptionsIniStr.DropNodesOnTabMove, KeyOptions.DropNodesOnTabMove );
      writebool( section, KeyOptionsIniStr.DropNodesOnTabPrompt, KeyOptions.DropNodesOnTabPrompt );
      writestring( section, KeyOptionsIniStr.DTLastDateFmt, KeyOptions.DTLastDateFmt );
      writestring( section, KeyOptionsIniStr.DTLastTimeFmt, KeyOptions.DTLastTimeFmt );
      writebool( section, KeyOptionsIniStr.DTUseLastSelection, KeyOptions.DTUseLastSelection );
      writebool( section, KeyOptionsIniStr.EncFileAltExt, KeyOptions.EncFileAltExt );
      writeinteger( section, KeyOptionsIniStr.EscAction, KeyOptions.EscAction );
      writebool( section, KeyOptionsIniStr.EvalAutoPaste, KeyOptions.AutoPasteEval );
      writebool( section, KeyOptionsIniStr.AutoPastePlugin, KeyOptions.AutoPastePlugin );
      writestring( section, KeyOptionsIniStr.ExtHTML, KeyOptions.ExtHTML );
      writestring( section, KeyOptionsIniStr.ExtRTF, KeyOptions.ExtRTF );
      writestring( section, KeyOptionsIniStr.ExtText, KeyOptions.ExtText );
      writebool( section, KeyOptionsIniStr.FixScrollBars, KeyOptions.FixScrollBars );
      writestring( section, KeyOptionsIniStr.HotKey, ShortCutToText( KeyOptions.HotKey ));
      writebool( section, KeyOptionsIniStr.HotKeyActivate, KeyOptions.HotKeyActivate );
      writebool( section, KeyOptionsIniStr.HotKeyWarn, KeyOptions.HotKeyWarn );
      // writestring( section, KeyOptionsIniStr.HTML32CNVLocation, KeyOptions.HTML32CNVLocation );
      writeinteger( section, KeyOptionsIniStr.HTMLImportMethod, ord( KeyOptions.HTMLImportMethod ));
      writebool( section, KeyOptionsIniStr.IgnoreUpgrades, KeyOptions.IgnoreUpgrades );
      writebool( section, KeyOptionsIniStr.ImportFileNamesWithExt, KeyOptions.ImportFileNamesWithExt );
      writestring( section, KeyOptionsIniStr.InitFontColor, ColorToString( KeyOptions.InitFontColor ));
      writestring( section, KeyOptionsIniStr.InitHiColor, ColorToString( KeyOptions.InitHiColor ));
      writebool( section, KeyOptionsIniStr.InsCharFullSet, KeyOptions.InsCharFullSet );
      writebool( section, KeyOptionsIniStr.InsCharKeepFont, KeyOptions.InsCharKeepFont );
      writebool( section, KeyOptionsIniStr.InsCharWinClose, KeyOptions.InsCharWinClose );
      writestring( section, KeyOptionsIniStr.LanguageUI, KeyOptions.LanguageUI );
      writestring( section, KeyOptionsIniStr.LastCopyPath, KeyOptions.LastCopyPath );
      writestring( section, KeyOptionsIniStr.LastExportPath, KeyOptions.LastExportPath );
      writeinteger( section, KeyOptionsIniStr.LastExportFormat, ord( KeyOptions.LastExportFormat ));
      writebool( section, KeyOptionsIniStr.LastExportAsk, KeyOptions.LastExportAsk );
      writestring( section, KeyOptionsIniStr.LastFile, KeyOptions.LastFile );
      writestring( section, KeyOptionsIniStr.LastImportPath, KeyOptions.LastImportPath );
      writeinteger( section, KeyOptionsIniStr.LastNumbering, ord( KeyOptions.LastNumbering ));
      writestring( section, KeyOptionsIniStr.LastVersion, Program_VerStr ); // always write current version
      writebool( section, KeyOptionsIniStr.LoadLastFile, KeyOptions.LoadLastFile );
      writebool( section, KeyOptionsIniStr.LoadUserFile, KeyOptions.LoadUserFile );
      writeinteger( section, KeyOptionsIniStr.ZoomIncrement, KeyOptions.ZoomIncrement );
      writebool( section, KeyOptionsIniStr.LongCombos, KeyOptions.LongCombos );
      writeinteger( section, KeyOptionsIniStr.KeyReplayDelay, KeyOptions.KeyReplayDelay );
      writestring( section, KeyOptionsIniStr.DateFmt, KeyOptions.DateFmt );
      writebool( section, KeyOptionsIniStr.MgrFullPaths, KeyOptions.MgrFullPaths );
      writebool( section, KeyOptionsIniStr.MinimizeOnClose, KeyOptions.MinimizeOnClose );
      writebool( section, KeyOptionsIniStr.MinimizeOnURL, KeyOptions.MinimizeOnURL );
      writeinteger( section, KeyOptionsIniStr.MRUCount, KeyOptions.MRUCount );
      writebool( section, KeyOptionsIniStr.MRUFullPaths, KeyOptions.MRUFullPaths );
      writebool( section, KeyOptionsIniStr.MRUSubmenu, KeyOptions.MRUSubmenu );
      writebool( section, KeyOptionsIniStr.MRUUse, KeyOptions.MRUUse );
      writestring( section, KeyOptionsIniStr.NodeNameHistory, '"' + KeyOptions.NodeNameHistory + '"' );
      writebool( section, KeyOptionsIniStr.NoRegistry, KeyOptions.NoRegistry );
      writebool( section, KeyOptionsIniStr.OpenFloppyReadOnly, KeyOptions.OpenFloppyReadOnly );
      writebool( section, KeyOptionsIniStr.OpenNetworkReadOnly, KeyOptions.OpenNetworkReadOnly );
      writebool( section, KeyOptionsIniStr.OpenReadOnlyWarn, KeyOptions.OpenReadOnlyWarn );
      writebool( section, KeyOptionsIniStr.PlaySoundOnAlarm, KeyOptions.PlaySoundOnAlarm );
      writeinteger( section, KeyOptionsIniStr.RecentLanguage, KeyOptions.RecentLanguage );
      writebool( section, KeyOptionsIniStr.ResolveLNK, KeyOptions.ResolveLNK );
      writebool( section, KeyOptionsIniStr.ResPanelActiveUpdate, KeyOptions.ResPanelActiveUpdate );
      writebool( section, KeyOptionsIniStr.ResPanelLeft, KeyOptions.ResPanelLeft );
      writebool( section, KeyOptionsIniStr.ResPanelShow, KeyOptions.ResPanelShow );
      writebool( section, KeyOptionsIniStr.RichEditv3, KeyOptions.RichEditv3 );
      writebool( section, KeyOptionsIniStr.RunAutoMacros, KeyOptions.RunAutoMacros );
      writebool( section, KeyOptionsIniStr.SafePrint, KeyOptions.SafePrint );
      writebool( section, KeyOptionsIniStr.SaveDARTWarn, KeyOptions.SaveDARTWarn );
      writeinteger( section, KeyOptionsIniStr.SaveDefaultFormat, ord( KeyOptions.SaveDefaultFormat ));
      writebool( section, KeyOptionsIniStr.ShellExecuteShowAllErrors, KeyOptions.ShellExecuteShowAllErrors );
      writebool( section, KeyOptionsIniStr.ShowFonts, KeyOptions.ShowFonts );
      // writebool( section, KeyOptionsIniStr.ShowSplash, KeyOptions.ShowSplash );
      writebool( section, KeyOptionsIniStr.ShowTooltips, KeyOptions.ShowTooltips );
      writebool( section, KeyOptionsIniStr.SingleInstance, KeyOptions.SingleInstance );
      writebool( section, KeyOptionsIniStr.SkipNewFilePrompt, KeyOptions.SkipNewFilePrompt );
      writebool( section, KeyOptionsIniStr.StartMinimized, KeyOptions.StartMinimized );
      writeinteger( section, KeyOptionsIniStr.StartNoteType, ord( KeyOptions.StartNoteType ));
      writeinteger( section, KeyOptionsIniStr.StatBarDlbClkAction, KeyOptions.StatBarDlbClkAction );
      writeinteger( section, KeyOptionsIniStr.StatBarDlbClkActionShft, KeyOptions.StatBarDlbClkActionShft );
      writebool( section, KeyOptionsIniStr.StatBarShow, KeyOptions.StatBarShow );
      writebool( section, KeyOptionsIniStr.StyleShowSamples, KeyOptions.StyleShowSamples );
      writestring( section, KeyOptionsIniStr.TabNameHistory, '"' + KeyOptions.TabNameHistory + '"' );
      writestring( section, KeyOptionsIniStr.TimeFmt, KeyOptions.TimeFmt );

      writebool( section, KeyOptionsIniStr.TimerMinimize, KeyOptions.TimerMinimize );
      writeinteger( section, KeyOptionsIniStr.TimerMinimizeInt, KeyOptions.TimerMinimizeInt );
      writebool( section, KeyOptionsIniStr.TimerClose, KeyOptions.TimerClose );
      writebool( section, KeyOptionsIniStr.TimerCloseDialogs, KeyOptions.TimerCloseDialogs );
      writebool( section, KeyOptionsIniStr.TimerCloseEncOnly, KeyOptions.TimerCloseEncOnly );
      writeinteger( section, KeyOptionsIniStr.TimerCloseInt, KeyOptions.TimerCloseInt );
      writebool( section, KeyOptionsIniStr.TimerCloseAutoReopen, KeyOptions.TimerCloseAutoReopen );

      writebool( section, KeyOptionsIniStr.TipOfTheDay, KeyOptions.TipOfTheDay );
      writeinteger( section, KeyOptionsIniStr.TipOfTheDayIdx, KeyOptions.TipOfTheDayIdx );
      writebool( section, KeyOptionsIniStr.ToolbarFormatShow, KeyOptions.ToolbarFormatShow );
      writebool( section, KeyOptionsIniStr.ToolbarInsertShow, KeyOptions.ToolbarInsertShow );
      writebool( section, KeyOptionsIniStr.ToolbarMacroShow, KeyOptions.ToolbarMacroShow );
      writebool( section, KeyOptionsIniStr.ToolbarMainShow, KeyOptions.ToolbarMainShow );
      writebool( section, KeyOptionsIniStr.ToolbarStyleShow, KeyOptions.ToolbarStyleShow );
      writebool( section, KeyOptionsIniStr.ToolbarTreeShow, KeyOptions.ToolbarTreeShow );
      writebool( section, KeyOptionsIniStr.UASEnable, KeyOptions.UASEnable );
      writestring( section, KeyOptionsIniStr.UASPath, KeyOptions.UASPath );
      writeinteger( section, KeyOptionsIniStr.URLAction, ord( KeyOptions.URLAction ));
      writestring( section, KeyOptionsIniStr.URLAltBrowserPath, KeyOptions.URLAltBrowserPath );
      writebool( section, KeyOptionsIniStr.URLFileAuto, KeyOptions.URLFileAuto );
      writebool( section, KeyOptionsIniStr.URLFileDecodeSpaces, KeyOptions.URLFileDecodeSpaces );
      writebool( section, KeyOptionsIniStr.URLFileNoPrefix, KeyOptions.URLFileNoPrefix );
      writebool( section, KeyOptionsIniStr.URLFileQuoteSpaces, KeyOptions.URLFileQuoteSpaces );
      writebool( section, KeyOptionsIniStr.URLClickShift, KeyOptions.URLClickShift );
      writebool( section, KeyOptionsIniStr.URLSystemBrowser, KeyOptions.URLSystembrowser );
      writebool( section, KeyOptionsIniStr.UseOldColorDlg, KeyOptions.UseOldColorDlg );
      writebool( section, KeyOptionsIniStr.UseOldFileFormat, KeyOptions.UseOldFileFormat );
      writebool( section, KeyOptionsIniStr.UseTray, KeyOptions.UseTray );
      writestring( section, KeyOptionsIniStr.UserFile, KeyOptions.UserFile );

      section := EditorOptionsIniStr.section;
      writebool( section, EditorOptionsIniStr.AutoIndent, EditorOptions.AutoIndent );
      writebool( section, EditorOptionsIniStr.EditProtected, EditorOptions.EditProtected );
      writebool( section, EditorOptionsIniStr.AutoFont, EditorOptions.AutoFont );
      writebool( section, EditorOptionsIniStr.AutoKeyboard, EditorOptions.AutoKeyboard );
      writebool( section, EditorOptionsIniStr.DisableINSKey, EditorOptions.DisableINSKey );
      writeinteger( section, EditorOptionsIniStr.FontSizeInc, EditorOptions.FontSizeInc );
      writeinteger( section, EditorOptionsIniStr.IndentInc, EditorOptions.IndentInc );
      writeinteger( section, EditorOptionsIniStr.ParaSpaceInc, EditorOptions.ParaSpaceInc);
      writebool( section, EditorOptionsIniStr.SaveCaretPos, EditorOptions.SaveCaretPos );
      writebool( section, EditorOptionsIniStr.TrackCaretPos, EditorOptions.TrackCaretPos );
      writebool( section, EditorOptionsIniStr.TrackStyle, EditorOptions.TrackStyle );
      writeinteger( section, EditorOptionsIniStr.TrackStyleRange, ord( EditorOptions.TrackStyleRange ));
      writeinteger( section, EditorOptionsIniStr.UndoLimit, EditorOptions.UndoLimit );
      writebool( section, EditorOptionsIniStr.WordCountTrack, EditorOptions.WordCountTrack );
      writebool( section, EditorOptionsIniStr.WordSelect, EditorOptions.WordSelect );
      writeinteger( section, EditorOptionsIniStr.WordsPerPage, EditorOptions.WordsPerPage );


      section := ClipOptionsIniStr.Section;
      writestring( section, ClipOptionsIniStr.WCDivider, ClipOptions.WCDivider );
      writebool( section, ClipOptionsIniStr.WCPasteAsText, ClipOptions.WCPasteAsText );

      writebool( section, ClipOptionsIniStr.URLOnly, ClipOptions.URLOnly );
      writeinteger( section, ClipOptionsIniStr.MaxSize, ClipOptions.MaxSize );
      // writestring( section, ClipOptionsIniStr.NewNodeName, ClipOptions.NewNodeName );
      writeinteger( section, ClipOptionsIniStr.ClipNodeNaming, ord( ClipOptions.ClipNodeNaming ));
      writebool( section, ClipOptionsIniStr.PasteAsText, ClipOptions.PasteAsText );
      writebool( section, ClipOptionsIniStr.PasteAsNewNode, ClipOptions.PasteAsNewNode );
      writebool( section, ClipOptionsIniStr.PlaySound, ClipOptions.PlaySound );
      writebool( section, ClipOptionsIniStr.Recall, ClipOptions.Recall );
      writeinteger( section, ClipOptionsIniStr.SleepTime, ClipOptions.SleepTime );
      writebool( section, ClipOptionsIniStr.SwitchIcon, ClipOptions.SwitchIcon );
      writebool( section, ClipOptionsIniStr.TestDupClips, ClipOptions.TestDupClips );
      writebool( section, ClipOptionsIniStr.TreeClipConfirm, ClipOptions.TreeClipConfirm );
      writestring( section, ClipOptionsIniStr.Divider, ClipOptions.Divider );
      writebool( section, ClipOptionsIniStr.IgnoreSelf, ClipOptions.IgnoreSelf );
      writebool( section, ClipOptionsIniStr.InsertSourceURL, ClipOptions.InsertSourceURL );

      section := TabOptionsIniStr.section;
      writestring( section, TabOptionsIniStr.ActiveColor, ColorToString( TabOptions.ActiveColor ));
      writebool( section, TabOptionsIniStr.ColorAllTabs, TabOptions.ColorAllTabs );
      writestring( section, TabOptionsIniStr.InActiveColor, ColorToString( TabOptions.InActiveColor ));
      writebool( section, TabOptionsIniStr.HotTrack, TabOptions.HotTrack );
      writebool( section, TabOptionsIniStr.Images, TabOptions.Images );
      writebool( section, TabOptionsIniStr.Stacked, TabOptions.Stacked );
      writebool( section, TabOptionsIniStr.TabsAreButtons, TabOptions.TabsAreButtons );
      writeinteger( section, TabOptionsIniStr.TabOrientation, ord( TabOptions.TabOrientation ));

      section := TreeOptionsIniStr.section;
      writebool( section, TreeOptionsIniStr.AutoNameVNodes, TreeOptions.AutoNameVNodes );
      writebool( section, TreeOptionsIniStr.AutoScroll, TreeOptions.AutoScroll );
      writebool( section, TreeOptionsIniStr.ConfirmNodeDelete, TreeOptions.ConfirmNodeDelete );
      writebool( section, TreeOptionsIniStr.ConfirmNodeRefresh, TreeOptions.ConfirmNodeRefresh );
      writeinteger( section, TreeOptionsIniStr.ExpandMode, ord( TreeOptions.ExpandMode ));
      writebool( section, TreeOptionsIniStr.FullRowSelect, TreeOptions.FullRowSelect );
      writebool( section, TreeOptionsIniStr.HotTrack, TreeOptions.HotTrack );
      writebool( section, TreeOptionsIniStr.EditInPlace, TreeOptions.EditInPlace );
      writebool( section, TreeOptionsIniStr.EditNewNodes, TreeOptions.EditNewNodes );
      writebool( section, TreeOptionsIniStr.InheritNodeBG, TreeOptions.InheritNodeBG );
      writebool( section, TreeOptionsIniStr.InheritNodeProperties, TreeOptions.InheritNodeProperties );
      writestring( section, TreeOptionsIniStr.NodeDelimiter, TreeOptions.NodeDelimiter );
      writebool( section, TreeOptionsIniStr.PathTopToBottom, TreeOptions.PathTopToBottom );
      writeinteger( section, TreeOptionsIniStr.RemovableMediaVNodes, TreeOptions.RemovableMediaVNodes );
      writebool( section, TreeOptionsIniStr.ShowFullPath, TreeOptions.ShowFullPath );
      writebool( section, TreeOptionsIniStr.ShowFullPathSearch, TreeOptions.ShowFullPathSearch );
      writebool( section, TreeOptionsIniStr.ShowTooltips, TreeOptions.ShowTooltips );
      writebool( section, TreeOptionsIniStr.TopLevelCheck, TreeOptions.TopLevelCheck );

      section := 'TabFont';
      with TabOptions.Font do
      begin
        writeinteger( section, FontPropertiesIniStr.FCharset, FCharset );
        writestring( section, FontPropertiesIniStr.FColor, ColorToString( FColor ));
        writestring( section, FontPropertiesIniStr.FName, FName );
        writeinteger( section, FontPropertiesIniStr.FSize, FSize );
        writestring( section, FontPropertiesIniStr.FStyle, FontStyleToStr( FStyle ));
      end;

      section := FindOptionsIniStr.section;
      writebool( section, FindOptionsIniStr.AllNodes, FindOptions.AllNodes );
      writebool( section, FindOptionsIniStr.AllTabs, FindOptions.AllTabs );
      writebool( section, FindOptionsIniStr.AutoClose, FindOptions.AutoClose );
      writebool( section, FindOptionsIniStr.EntireScope, FindOptions.EntireScope );
      writestring( section, FindOptionsIniStr.FindAllHistory, '"' + FindOptions.FindAllHistory + '"' );
      writestring( section, FindOptionsIniStr.History, '"' + FindOptions.History + '"' );
      writeinteger( section, FindOptionsIniStr.HistoryMaxCnt, FindOptions.HistoryMaxCnt );
      writebool( section, FindOptionsIniStr.ReplaceConfirm, FindOptions.ReplaceConfirm );
      writestring( section, FindOptionsIniStr.ReplaceHistory, FindOptions.ReplaceHistory );
      writebool( section, FindOptionsIniStr.MatchCase, FindOptions.MatchCase );
      writebool( section, FindOptionsIniStr.WholeWordsOnly, FindOptions.WholeWordsOnly );
      writebool( section, FindOptionsIniStr.WordAtCursor, FindOptions.WordAtCursor );
      writeinteger( section, FindOptionsIniStr.SearchMode, ord( FindOptions.SearchMode ));
      writebool( section, FindOptionsIniStr.Wrap, FindOptions.Wrap );
      writebool( section, FindOptionsIniStr.SearchNodeNames, FindOptions.SearchNodeNames );

      section := ResPanelOptionsIniStr.section;
      writebool( section, ResPanelOptionsIniStr.ColorFindList, ResPanelOptions.ColorFindList );
      writestring( section, ResPanelOptionsIniStr.FindListAltColor, ColorToString( ResPanelOptions.FindListAltColor ));
      writebool( section, ResPanelOptionsIniStr.ShowFind, ResPanelOptions.ShowFind );
      writebool( section, ResPanelOptionsIniStr.ShowMacro, ResPanelOptions.ShowMacro );
      writebool( section, ResPanelOptionsIniStr.ShowPlugin, ResPanelOptions.ShowPlugin );
      writebool( section, ResPanelOptionsIniStr.ShowScratch, ResPanelOptions.ShowScratch );
      writebool( section, ResPanelOptionsIniStr.ShowTemplate, ResPanelOptions.ShowTemplate );
      writebool( section, ResPanelOptionsIniStr.ShowFavorites, ResPanelOptions.ShowFavorites );
      writebool( section, ResPanelOptionsIniStr.Stacked, ResPanelOptions.Stacked );
      writeinteger( section, ResPanelOptionsIniStr.TabOrientation, ord( ResPanelOptions.TabOrientation ));

    end;
  finally
    IniFile.Free;
  end;

end; // SaveKeyNoteOptions

procedure LoadKeyNoteOptions(
    const INIFileName : string;
    var KeyOptions : TKeyOptions;
    var TabOptions : TTabOptions;
    var FindOptions : TFindOptions;
    var EditorOptions : TEditorOptions;
    var ClipOptions : TClipOptions;
    var TreeOptions : TKNTTreeOptions;
    var ResPanelOptions : TResPanelOptions
  );
var
  IniFile : TIniFile;
  section : string;
  i : integer;
begin

  IniFile := TIniFile.Create( INIFileName );
  try
    with IniFile do
    begin
      section := KeyOptionsIniStr.section;
      KeyOptions.AlwaysOnTop := readbool( section, KeyOptionsIniStr.AlwaysOnTop, KeyOptions.AlwaysOnTop );
      KeyOptions.AutoNewFile := readbool( section, KeyOptionsIniStr.AutoNewFile, KeyOptions.AutoNewFile );
      KeyOptions.AutoRegisterFileType := readbool( section, KeyOptionsIniStr.AutoRegisterFileType, KeyOptions.AutoRegisterFileType );
      KeyOptions.AutoRegisterPrompt := readbool( section, KeyOptionsIniStr.AutoRegisterPrompt, KeyOptions.AutoRegisterPrompt );
      KeyOptions.AutoSave := readbool( section, KeyOptionsIniStr.AutoSave, KeyOptions.AutoSave );
      KeyOptions.AutoSaveOnFocus := readbool( section, KeyOptionsIniStr.AutoSaveOnFocus, KeyOptions.AutoSaveOnFocus );
      KeyOptions.AutoSaveOnTimer := readbool( section, KeyOptionsIniStr.AutoSaveOnTimer, KeyOptions.AutoSaveOnTimer );
      KeyOptions.AutoSaveOnTimerInt := readinteger( section, KeyOptionsIniStr.AutoSaveOnTimerInt, KeyOptions.AutoSaveOnTimerInt );
      KeyOptions.Backup := readbool( section, KeyOptionsIniStr.Backup, KeyOptions.Backup );
      KeyOptions.BackupAppendExt := readbool( section, KeyOptionsIniStr.BackupAppendExt, KeyOptions.BackupAppendExt );
      KeyOptions.BackupDir := readstring( section, KeyOptionsIniStr.BackupDir, KeyOptions.BackupDir );
      KeyOptions.BackupExt := readstring( section, KeyOptionsIniStr.BackupExt, KeyOptions.BackupExt );
      KeyOptions.BackupLevel := readinteger( section, KeyOptionsIniStr.BackupLevel, KeyOptions.BackupLevel );
      KeyOptions.BackupVNodes := readbool( section, KeyOptionsIniStr.BackupVNodes, KeyOptions.BackupVNodes );
      KeyOptions.ColorDlgBig := readbool( section, KeyOptionsIniStr.ColorDlgBig, KeyOptions.ColorDlgBig );
      KeyOptions.ComboDropDownCount := readinteger( section, KeyOptionsIniStr.ComboDropDownCount, KeyOptions.ComboDropDownCount );

      // these do not get saved, but they may be modified manually
      KeyOptions.ComboFontLen := readinteger( section, KeyOptionsIniStr.ComboFontLen, 0 );
      KeyOptions.ComboMacroLen := readinteger( section, KeyOptionsIniStr.ComboMacroLen, 0 );
      KeyOptions.ComboStyleLen := readinteger( section, KeyOptionsIniStr.ComboStyleLen, 0 );


      KeyOptions.ConfirmExit := readbool( section, KeyOptionsIniStr.ConfirmExit, KeyOptions.ConfirmExit );
      KeyOptions.ConfirmTreePaste := readbool( section, KeyOptionsIniStr.ConfirmTreePaste, KeyOptions.ConfirmTreePaste );
      KeyOptions.ConfirmTabDelete := readbool( section, KeyOptionsIniStr.ConfirmTabDelete, KeyOptions.ConfirmTabDelete );
      KeyOptions.DateFmt := readstring( section, KeyOptionsIniStr.DateFmt, KeyOptions.DateFmt );
      KeyOptions.Debug := readbool( section, KeyOptionsIniStr.Debug, KeyOptions.Debug );
      KeyOptions.DebugLogAppend := readbool( section, KeyOptionsIniStr.DebugLogAppend, KeyOptions.DebugLogAppend );
      KeyOptions.DefaultNoteType := TNoteType( readinteger( section, KeyOptionsIniStr.DefaultNoteType, ord( KeyOptions.DefaultNoteType )));
      KeyOptions.DisableAlarmPopup := readbool( section, KeyOptionsIniStr.DisableAlarmPopup, KeyOptions.DisableAlarmPopup );
      KeyOptions.DisableFileMon := readbool( section, KeyOptionsIniStr.DisableFileMon, KeyOptions.DisableFileMon );
      KeyOptions.DropNodesOnTabMove := readbool( section, KeyOptionsIniStr.DropNodesOnTabMove, KeyOptions.DropNodesOnTabMove );
      KeyOptions.DropNodesOnTabPrompt := readbool( section, KeyOptionsIniStr.DropNodesOnTabPrompt, KeyOptions.DropNodesOnTabPrompt );
      KeyOptions.DTLastDateFmt := readstring( section, KeyOptionsIniStr.DTLastDateFmt, KeyOptions.DTLastDateFmt );
      KeyOptions.DTLastTimeFmt := readstring( section, KeyOptionsIniStr.DTLastTimeFmt, KeyOptions.DTLastTimeFmt );
      KeyOptions.DTUseLastSelection := readbool( section, KeyOptionsIniStr.DTUseLastSelection, KeyOptions.DTUseLastSelection );
      KeyOptions.EncFileAltExt := readbool( section, KeyOptionsIniStr.EncFileAltExt, KeyOptions.EncFileAltExt );
      KeyOptions.EscAction := readinteger( section, KeyOptionsIniStr.EscAction, KeyOptions.EscAction );
      KeyOptions.AutoPasteEval := readbool( section, KeyOptionsIniStr.EvalAutoPaste, KeyOptions.AutoPasteEval );
      KeyOptions.AutoPastePlugin := readbool( section, KeyOptionsIniStr.AutoPastePlugin, KeyOptions.AutoPastePlugin );
      KeyOptions.ExtHTML := readstring( section, KeyOptionsIniStr.ExtHTML, KeyOptions.ExtHTML );
      KeyOptions.ExtRTF := readstring( section, KeyOptionsIniStr.ExtRTF, KeyOptions.ExtRTF );
      KeyOptions.ExtText := readstring( section, KeyOptionsIniStr.ExtText, KeyOptions.ExtText );
      KeyOptions.FixScrollBars := readbool( section, KeyOptionsIniStr.FixScrollBars, KeyOptions.FixScrollBars );

      try
        KeyOptions.HotKey := TextToShortCut( readstring( section, KeyOptionsIniStr.HotKey, ShortCutToText( KeyOptions.HotKey )));
      except
        KeyOptions.HotKey := DEFAULT_HOTKEY;
      end;
      KeyOptions.HotKeyActivate := ( readbool( section, KeyOptionsIniStr.HotKeyActivate, KeyOptions.HotKeyActivate ) and ( KeyOptions.HotKey > 0 ));
      KeyOptions.HotKeyWarn := readbool( section, KeyOptionsIniStr.HotKeyWarn, KeyOptions.HotKeyWarn );
      // KeyOptions.HTML32CNVLocation := readstring( section, KeyOptionsIniStr.HTML32CNVLocation, KeyOptions.HTML32CNVLocation );
      KeyOptions.HTMLImportMethod := THTMLImportMethod( readinteger( section, KeyOptionsIniStr.HTMLImportMethod, ord( KeyOptions.HTMLImportMethod )));
      KeyOptions.IgnoreUpgrades := readbool( section, KeyOptionsIniStr.IgnoreUpgrades, KeyOptions.IgnoreUpgrades );
      KeyOptions.ImportFileNamesWithExt := readbool( section, KeyOptionsIniStr.ImportFileNamesWithExt, KeyOptions.ImportFileNamesWithExt );

      try
        KeyOptions.InitFontColor := StringToColor( readstring( section, KeyOptionsIniStr.InitFontColor, 'clWindowText' ));
        KeyOptions.InitHiColor := StringToColor( readstring( section, KeyOptionsIniStr.InitHiColor, 'clInfoBK' ));
      except
      end;

      KeyOptions.InsCharFullSet := readbool( section, KeyOptionsIniStr.InsCharFullSet, KeyOptions.InsCharFullSet );
      KeyOptions.InsCharKeepFont := readbool( section, KeyOptionsIniStr.InsCharKeepFont, KeyOptions.InsCharKeepFont );
      KeyOptions.InsCharWinClose := readbool( section, KeyOptionsIniStr.InsCharWinClose, KeyOptions.InsCharWinClose );
      KeyOptions.LanguageUI := readstring( section, KeyOptionsIniStr.LanguageUI, KeyOptions.LanguageUI );
      KeyOptions.LastCopyPath := readstring( section, KeyOptionsIniStr.LastCopyPath, KeyOptions.LastCopyPath );

      KeyOptions.LastExportPath := readstring( section, KeyOptionsIniStr.LastExportPath, KeyOptions.LastExportPath );
      KeyOptions.LastExportAsk := readbool( section, KeyOptionsIniStr.LastExportAsk, KeyOptions.LastExportAsk );

      i := readinteger( section, KeyOptionsIniStr.LastExportFormat, ord( KeyOptions.LastExportFormat ));
      if (( i < 0 ) or ( i > ord( high( TExportFmt )))) then
        KeyOptions.LastExportFormat := low( TExportFmt )
      else
        KeyOptions.LastExportFormat := TExportFmt( i );

      KeyOptions.LastFile := NormalFN( readstring( section, KeyOptionsIniStr.LastFile, KeyOptions.LastFile ));
      KeyOptions.LastImportPath := readstring( section, KeyOptionsIniStr.LastImportPath, KeyOptions.LastImportPath );
      KeyOptions.LastNumbering := TRxNumbering( readinteger( section, KeyOptionsIniStr.LastNumbering, ord( KeyOptions.LastNumbering )));
      KeyOptions.LastVersion := readstring( section, KeyOptionsIniStr.LastVersion, KeyOptions.LastVersion );
      KeyOptions.LoadLastFile := readbool( section, KeyOptionsIniStr.LoadLastFile, KeyOptions.LoadLastFile );
      KeyOptions.LoadUserFile := readbool( section, KeyOptionsIniStr.LoadUserFile, KeyOptions.LoadUserFile );
      KeyOptions.LongCombos := readbool( section, KeyOptionsIniStr.LongCombos, KeyOptions.LongCombos );
      KeyOptions.KeyReplayDelay := readinteger( section, KeyOptionsIniStr.KeyReplayDelay, KeyOptions.KeyReplayDelay );
      KeyOptions.MgrFullPaths := readbool( section, KeyOptionsIniStr.MgrFullPaths, KeyOptions.MgrFullPaths );
      KeyOptions.MinimizeOnClose := readbool( section, KeyOptionsIniStr.MinimizeOnClose, KeyOptions.MinimizeOnClose );
      KeyOptions.MinimizeOnURL := readbool( section, KeyOptionsIniStr.MinimizeOnURL, KeyOptions.MinimizeOnURL );
      KeyOptions.MRUCount := readinteger( section, KeyOptionsIniStr.MRUCount, KeyOptions.MRUCount );
      KeyOptions.MRUFullPaths := readbool( section, KeyOptionsIniStr.MRUFullPaths, KeyOptions.MRUFullPaths );
      KeyOptions.MRUSubmenu := readbool( section, KeyOptionsIniStr.MRUSubmenu, KeyOptions.MRUSubmenu );
      KeyOptions.MRUUse := readbool( section, KeyOptionsIniStr.MRUUse, KeyOptions.MRUUse );
      KeyOptions.NodeNameHistory := readstring( section, KeyOptionsIniStr.NodeNameHistory, KeyOptions.NodeNameHistory );
      KeyOptions.NoRegistry := readbool( section, KeyOptionsIniStr.NoRegistry, KeyOptions.NoRegistry );
      KeyOptions.OpenFloppyReadOnly := readbool( section, KeyOptionsIniStr.OpenFloppyReadOnly, KeyOptions.OpenFloppyReadOnly );
      KeyOptions.OpenNetworkReadOnly := readbool( section, KeyOptionsIniStr.OpenNetworkReadOnly, KeyOptions.OpenNetworkReadOnly );
      KeyOptions.OpenReadOnlyWarn := readbool( section, KeyOptionsIniStr.OpenReadOnlyWarn, KeyOptions.OpenReadOnlyWarn );
      KeyOptions.PlaySoundOnAlarm := readbool( section, KeyOptionsIniStr.PlaySoundOnAlarm, KeyOptions.PlaySoundOnAlarm );
      KeyOptions.RecentLanguage := readinteger( section, KeyOptionsIniStr.RecentLanguage, KeyOptions.RecentLanguage );
      KeyOptions.ResolveLNK := readbool( section, KeyOptionsIniStr.ResolveLNK, KeyOptions.ResolveLNK );
      KeyOptions.ResPanelActiveUpdate := readbool( section, KeyOptionsIniStr.ResPanelActiveUpdate, KeyOptions.ResPanelActiveUpdate );
      KeyOptions.ResPanelLeft := readbool( section, KeyOptionsIniStr.ResPanelLeft, KeyOptions.ResPanelLeft );
      KeyOptions.ResPanelShow := readbool( section, KeyOptionsIniStr.ResPanelShow, KeyOptions.ResPanelShow );

      KeyOptions.RichEditv3 := readbool( section, KeyOptionsIniStr.RichEditv3, KeyOptions.RichEditv3 );

      KeyOptions.RunAutoMacros := readbool( section, KeyOptionsIniStr.RunAutoMacros, KeyOptions.RunAutoMacros );
      KeyOptions.SafePrint := readbool( section, KeyOptionsIniStr.SafePrint, KeyOptions.SafePrint );
      KeyOptions.SaveDARTWarn := readbool( section, KeyOptionsIniStr.SaveDARTWarn, KeyOptions.SaveDARTWarn );
      i := readinteger( section, KeyOptionsIniStr.SaveDefaultFormat, 0 );
      if (( i < 0 ) or ( i > ord( high( TNoteFileFormat )))) then i := 0;
      KeyOptions.ShellExecuteShowAllErrors := readbool( section, KeyOptionsIniStr.ShellExecuteShowAllErrors, KeyOptions.ShellExecuteShowAllErrors );
      KeyOptions.ShowFonts := readbool( section, KeyOptionsIniStr.ShowFonts, KeyOptions.ShowFonts );
      KeyOptions.SaveDefaultFormat := TNoteFileFormat( i );
      // KeyOptions.ShowSplash := readbool( section, KeyOptionsIniStr.ShowSplash, KeyOptions.ShowSplash );
      KeyOptions.ShowTooltips := readbool( section, KeyOptionsIniStr.ShowTooltips, KeyOptions.ShowTooltips );
      KeyOptions.SingleInstance := readbool( section, KeyOptionsIniStr.SingleInstance, KeyOptions.SingleInstance );
      KeyOptions.SkipNewFilePrompt := readbool( section, KeyOptionsIniStr.SkipNewFilePrompt, KeyOptions.SkipNewFilePrompt );
      KeyOptions.StartMinimized := readbool( section, KeyOptionsIniStr.StartMinimized, KeyOptions.StartMinimized );
      KeyOptions.StartNoteType := TNoteType( readinteger( section, KeyOptionsIniStr.StartNoteType, ord( KeyOptions.StartNoteType )));
      KeyOptions.StatBarDlbClkAction := readinteger( section, KeyOptionsIniStr.StatBarDlbClkAction, KeyOptions.StatBarDlbClkAction );
      KeyOptions.StatBarDlbClkActionShft := readinteger( section, KeyOptionsIniStr.StatBarDlbClkActionShft, KeyOptions.StatBarDlbClkActionShft );
      KeyOptions.StatBarShow := readbool( section, KeyOptionsIniStr.StatBarShow, KeyOptions.StatBarShow );
      KeyOptions.StyleShowSamples := readbool( section, KeyOptionsIniStr.StyleShowSamples, KeyOptions.StyleShowSamples );
      KeyOptions.TabNameHistory := readstring( section, KeyOptionsIniStr.TabNameHistory, KeyOptions.TabNameHistory );
      KeyOptions.TimeFmt := readstring( section, KeyOptionsIniStr.TimeFmt, KeyOptions.TimeFmt );

      KeyOptions.TimerMinimize := readbool( section, KeyOptionsIniStr.TimerMinimize, KeyOptions.TimerMinimize );
      KeyOptions.TimerMinimizeInt := readinteger( section, KeyOptionsIniStr.TimerMinimizeInt, KeyOptions.TimerMinimizeInt );
      KeyOptions.TimerClose := readbool( section, KeyOptionsIniStr.TimerClose, KeyOptions.TimerClose );
      KeyOptions.TimerCloseDialogs := readbool( section, KeyOptionsIniStr.TimerCloseDialogs, KeyOptions.TimerCloseDialogs );
      KeyOptions.TimerCloseEncOnly := readbool( section, KeyOptionsIniStr.TimerCloseEncOnly, KeyOptions.TimerCloseEncOnly );
      KeyOptions.TimerCloseInt := readinteger( section, KeyOptionsIniStr.TimerCloseInt, KeyOptions.TimerCloseInt );
      KeyOptions.TimerCloseAutoReopen := readbool( section, KeyOptionsIniStr.TimerCloseAutoReopen, KeyOptions.TimerCloseAutoReopen );

      if KeyOptions.TimerMinimizeInt < 1 then
        KeyOptions.TimerMinimizeInt := DEF_TIMERMINIMIZEINT;
      if KeyOptions.TimerCloseInt < 1 then
        KeyOptions.TimerCloseInt := DEF_TIMERCLOSEINT;

      KeyOptions.TipOfTheDay := readbool( section, KeyOptionsIniStr.TipOfTheDay, KeyOptions.TipOfTheDay );
      KeyOptions.TipOfTheDayIdx := readinteger( section, KeyOptionsIniStr.TipOfTheDayIdx, KeyOptions.TipOfTheDayIdx );
      KeyOptions.ToolbarFormatShow := readbool( section, KeyOptionsIniStr.ToolbarFormatShow, KeyOptions.ToolbarFormatShow );
      KeyOptions.ToolbarInsertShow := readbool( section, KeyOptionsIniStr.ToolbarInsertShow, KeyOptions.ToolbarInsertShow );
      KeyOptions.ToolbarMacroShow := readbool( section, KeyOptionsIniStr.ToolbarMacroShow, KeyOptions.ToolbarMacroShow );
      KeyOptions.ToolbarMainShow := readbool( section, KeyOptionsIniStr.ToolbarMainShow, KeyOptions.ToolbarMainShow );
      KeyOptions.ToolbarStyleShow := readbool( section, KeyOptionsIniStr.ToolbarStyleShow, KeyOptions.ToolbarStyleShow );
      KeyOptions.ToolbarTreeShow := readbool( section, KeyOptionsIniStr.ToolbarTreeShow, KeyOptions.ToolbarTreeShow );
      KeyOptions.UASEnable := readbool( section, KeyOptionsIniStr.UASEnable, KeyOptions.UASEnable );
      KeyOptions.UASPath := readstring( section, KeyOptionsIniStr.UASPath, KeyOptions.UASPath );
      KeyOptions.URLAction := TURLAction( readinteger( section, KeyOptionsIniStr.URLAction, ord( KeyOptions.URLAction )));
      KeyOptions.URLAltBrowserPath := readstring( section, KeyOptionsIniStr.URLAltBrowserPath, KeyOptions.URLAltBrowserPath );
      KeyOptions.URLFileAuto := readbool( section, KeyOptionsIniStr.URLFileAuto, KeyOptions.URLFileAuto );
      KeyOptions.URLFileDecodeSpaces := readbool( section, KeyOptionsIniStr.URLFileDecodeSpaces, KeyOptions.URLFileDecodeSpaces );
      KeyOptions.URLFileNoPrefix := readbool( section, KeyOptionsIniStr.URLFileNoPrefix, KeyOptions.URLFileNoPrefix );
      KeyOptions.URLFileQuoteSpaces := readbool( section, KeyOptionsIniStr.URLFileQuoteSpaces, KeyOptions.URLFileQuoteSpaces );
      KeyOptions.URLClickShift := readbool( section, KeyOptionsIniStr.URLClickShift, KeyOptions.URLClickShift );
      KeyOptions.URLSystembrowser := readbool( section, KeyOptionsIniStr.URLSystembrowser, KeyOptions.URLSystembrowser );
      KeyOptions.UseOldColorDlg := readbool( section, KeyOptionsIniStr.UseOldColorDlg, KeyOptions.UseOldColorDlg );

      KeyOptions.UseOldFileFormat := readbool( section, KeyOptionsIniStr.UseOldFileFormat, KeyOptions.UseOldFileFormat );
      _USE_OLD_KEYNOTE_FILE_FORMAT := ( _USE_OLD_KEYNOTE_FILE_FORMAT or KeyOptions.UseOldFileFormat );

      KeyOptions.UseTray := readbool( section, KeyOptionsIniStr.UseTray, KeyOptions.UseTray );
      KeyOptions.UserFile := NormalFN( readstring( section, KeyOptionsIniStr.UserFile, KeyOptions.UserFile ));

      KeyOptions.ZoomIncrement := readinteger( section, KeyOptionsIniStr.ZoomIncrement, KeyOptions.ZoomIncrement );

      if KeyOptions.SingleInstance then KeyOptions.HotKeyWarn := false;

      section := EditorOptionsIniStr.section;
      EditorOptions.AutoFont := readbool( section, EditorOptionsIniStr.AutoFont, EditorOptions.AutoFont );
      EditorOptions.AutoIndent := readbool( section, EditorOptionsIniStr.AutoIndent, EditorOptions.AutoIndent );
      EditorOptions.EditProtected := readbool( section, EditorOptionsIniStr.EditProtected, EditorOptions.EditProtected );
      EditorOptions.AutoKeyboard := readbool( section, EditorOptionsIniStr.AutoKeyboard, EditorOptions.AutoKeyboard );
      EditorOptions.DisableINSKey := readbool( section, EditorOptionsIniStr.DisableINSKey, EditorOptions.DisableINSKey );
      EditorOptions.FontSizeInc := readinteger( section, EditorOptionsIniStr.FontSizeInc, EditorOptions.FontSizeInc );
      EditorOptions.IndentInc := readinteger( section, EditorOptionsIniStr.IndentInc, EditorOptions.IndentInc );

      EditorOptions.ParaSpaceInc := readinteger( section, EditorOptionsIniStr.ParaSpaceInc, EditorOptions.ParaSpaceInc );
      EditorOptions.SaveCaretPos := readbool( section, EditorOptionsIniStr.SaveCaretPos, EditorOptions.SaveCaretPos );
      EditorOptions.TrackCaretPos := readbool( section, EditorOptionsIniStr.TrackCaretPos, EditorOptions.TrackCaretPos );

      EditorOptions.TrackStyle := readbool( section, EditorOptionsIniStr.TrackStyle, EditorOptions.TrackStyle );
      EditorOptions.TrackStyleRange := TStyleRange( readinteger( section, EditorOptionsIniStr.TrackStyleRange, ord( EditorOptions.TrackStyleRange )));

      EditorOptions.UndoLimit := readinteger( section, EditorOptionsIniStr.UndoLimit, EditorOptions.UndoLimit );
      EditorOptions.WordCountTrack := readbool( section, EditorOptionsIniStr.WordCountTrack, EditorOptions.WordCountTrack );
      EditorOptions.WordSelect := readbool( section, EditorOptionsIniStr.WordSelect, EditorOptions.WordSelect );
      try
        EditorOptions.WordsPerPage := readinteger( section, EditorOptionsIniStr.WordsPerPage, EditorOptions.WordsPerPage );
      except
        EditorOptions.WordsPerPage := 0;
      end;
      
      _SAVE_RESTORE_CARETPOS := EditorOptions.SaveCaretPos;

      section := ClipOptionsIniStr.Section;
      with ClipOptions do
      begin
        WCDivider := readstring( section, ClipOptionsIniStr.WCDivider, WCDivider );
        WCPasteAsText := readbool( section, ClipOptionsIniStr.WCPasteAsText, WCPasteAsText );

        URLOnly := readbool( section, ClipOptionsIniStr.URLOnly, URLOnly );
        try
          MaxSize := readinteger( section, ClipOptionsIniStr.MaxSize, DEF_CLIP_CAP_MAX_SIZE );
        except
          MaxSize := DEF_CLIP_CAP_MAX_SIZE;
        end;
        // ClipOptions.NewNodeName := readstring( section, ClipOptionsIniStr.NewNodeName, ClipOptions.NewNodeName );
        ClipOptions.ClipNodeNaming := TClipNodeNaming( readinteger( section, ClipOptionsIniStr.ClipNodeNaming, ord( ClipOptions.ClipNodeNaming )));
        PasteAsText := readbool( section, ClipOptionsIniStr.PasteAsText, PasteAsText );
        PasteAsNewNode := readbool( section, ClipOptionsIniStr.PasteAsNewNode, ClipOptions.PasteAsNewNode );
        PlaySound := readbool( section, ClipOptionsIniStr.PlaySound, ClipOptions.PlaySound );
        Recall := readbool( section, ClipOptionsIniStr.Recall, ClipOptions.Recall );
        SleepTime := readinteger( section, ClipOptionsIniStr.SleepTime, ClipOptions.SleepTime );
        SwitchIcon := readbool( section, ClipOptionsIniStr.SwitchIcon, ClipOptions.SwitchIcon );
        TestDupClips := readbool( section, ClipOptionsIniStr.TestDupClips, ClipOptions.TestDupClips );
        TreeClipConfirm := readbool( section, ClipOptionsIniStr.TreeClipConfirm, ClipOptions.TreeClipConfirm );
        Divider := readstring( section, ClipOptionsIniStr.Divider, Divider );
        IgnoreSelf := readbool( section, ClipOptionsIniStr.IgnoreSelf, IgnoreSelf );
        InsertSourceURL := readbool( section, ClipOptionsIniStr.InsertSourceURL, ClipOptions.InsertSourceURL );
        if ( Divider = '' ) then Divider := DEF_CLIP_CAP_DIV;
      end;

      section := TabOptionsIniStr.section;
      try
        TabOptions.ActiveColor := StringToColor( readstring( section, TabOptionsIniStr.ActiveColor, 'clWindow' ));
      except
        TabOptions.ActiveColor := clWindow;
      end;
      TabOptions.ColorAllTabs := readbool( section, TabOptionsIniStr.ColorAllTabs, TabOptions.ColorAllTabs );
      try
        TabOptions.InactiveColor := StringToColor( readstring( section, TabOptionsIniStr.InActiveColor, 'clBtnFace' ));
      except
        TabOptions.InactiveColor := clBtnFace;
      end;
      TabOptions.HotTrack := readbool( section, TabOptionsIniStr.HotTrack, TabOptions.HotTrack );
      TabOptions.Images := readbool( section, TabOptionsIniStr.Images, TabOptions.Images );
      TabOptions.Stacked := readbool( section, TabOptionsIniStr.Stacked, TabOptions.Stacked );
      TabOptions.TabsAreButtons := readbool( section, TabOptionsIniStr.TabsAreButtons, TabOptions.TabsAreButtons );
      TabOptions.TabOrientation := TTabOrientation( readinteger( section, TabOptionsIniStr.TabOrientation, ord( TabOptions.TabOrientation )));

      section := 'TabFont';
      with TabOptions.Font do
      begin
        FCharset := readinteger( section, FontPropertiesIniStr.FCharset, FCharset );
        try
          FColor := StringToColor( readstring( section, FontPropertiesIniStr.FColor, 'clWindowText' ));
        except
          FColor := clWindowText;
        end;
        FName := readstring( section, FontPropertiesIniStr.FName, FName );
        FSize := readinteger( section, FontPropertiesIniStr.FSize, FSize );
        FStyle := StrToFontStyle( readstring( section, FontPropertiesIniStr.FStyle, '' ));
      end;

      section := TreeOptionsIniStr.section;
      TreeOptions.AutoNameVNodes := readbool( section, TreeOptionsIniStr.AutoNameVNodes, TreeOptions.AutoNameVNodes );
      TreeOptions.AutoScroll := readbool( section, TreeOptionsIniStr.AutoScroll, TreeOptions.AutoScroll );
      TreeOptions.ConfirmNodeDelete := readbool( section, TreeOptionsIniStr.ConfirmNodeDelete, TreeOptions.ConfirmNodeDelete );
      TreeOptions.ConfirmNodeRefresh := readbool( section, TreeOptionsIniStr.ConfirmNodeRefresh, TreeOptions.ConfirmNodeRefresh );
      TreeOptions.ExpandMode := TTreeExpandMode( readinteger( section, TreeOptionsIniStr.ExpandMode, ord( TreeOptions.ExpandMode )));
      TreeOptions.FullRowSelect := readbool( section, TreeOptionsIniStr.FullRowSelect, TreeOptions.FullRowSelect );
      TreeOptions.HotTrack := readbool( section, TreeOptionsIniStr.HotTrack, TreeOptions.HotTrack );
      TreeOptions.EditInPlace := readbool( section, TreeOptionsIniStr.EditInPlace, TreeOptions.EditInPlace );
      TreeOptions.EditNewNodes := readbool( section, TreeOptionsIniStr.EditNewNodes, TreeOptions.EditNewNodes );
      TreeOptions.InheritNodeBG := readbool( section, TreeOptionsIniStr.InheritNodeBG, TreeOptions.InheritNodeBG );
      TreeOptions.InheritNodeProperties := readbool( section, TreeOptionsIniStr.InheritNodeProperties, TreeOptions.InheritNodeProperties );
      TreeOptions.NodeDelimiter := readstring( section, TreeOptionsIniStr.NodeDelimiter, TreeOptions.NodeDelimiter );
      TreeOptions.PathTopToBottom := readbool( section, TreeOptionsIniStr.PathTopToBottom, TreeOptions.PathTopToBottom );
      TreeOptions.RemovableMediaVNodes := readinteger( section, TreeOptionsIniStr.RemovableMediaVNodes, TreeOptions.RemovableMediaVNodes );
      TreeOptions.ShowFullPath := readbool( section, TreeOptionsIniStr.ShowFullPath, TreeOptions.ShowFullPath );
      TreeOptions.ShowFullPathSearch := readbool( section, TreeOptionsIniStr.ShowFullPathSearch, TreeOptions.ShowFullPathSearch );
      TreeOptions.ShowTooltips := readbool( section, TreeOptionsIniStr.ShowTooltips, TreeOptions.ShowTooltips );
      TreeOptions.TopLevelCheck := readbool( section, TreeOptionsIniStr.TopLevelCheck, TreeOptions.TopLevelCheck );

      section := FindOptionsIniStr.section;
      FindOptions.AllNodes := readbool( section, FindOptionsIniStr.AllNodes, FindOptions.AllNodes );
      FindOptions.AllTabs := readbool( section, FindOptionsIniStr.AllTabs, FindOptions.AllTabs );
      FindOptions.AutoClose := readbool( section, FindOptionsIniStr.AutoClose, FindOptions.AutoClose );
      FindOptions.EntireScope := readbool( section, FindOptionsIniStr.EntireScope, FindOptions.EntireScope );
      FindOptions.FindAllHistory := readstring( section, FindOptionsIniStr.FindAllHistory, FindOptions.FindAllHistory );
      FindOptions.History := readstring( section, FindOptionsIniStr.History, FindOptions.History );
      FindOptions.HistoryMaxCnt := readinteger( section, FindOptionsIniStr.HistoryMaxCnt, FindOptions.HistoryMaxCnt );
      FindOptions.ReplaceConfirm := readbool( section, FindOptionsIniStr.ReplaceConfirm, FindOptions.ReplaceConfirm );
      FindOptions.ReplaceHistory := readstring( section, FindOptionsIniStr.ReplaceHistory, FindOptions.ReplaceHistory );
      FindOptions.MatchCase := readbool( section, FindOptionsIniStr.MatchCase, FindOptions.MatchCase );
      FindOptions.WholeWordsOnly := readbool( section, FindOptionsIniStr.WholeWordsOnly, FindOptions.WholeWordsOnly );
      FindOptions.WordAtCursor := readbool( section, FindOptionsIniStr.WordAtCursor, FindOptions.WordAtCursor );
      FindOptions.Wrap := readbool( section, FindOptionsIniStr.Wrap, FindOptions.Wrap );
      FindOptions.SearchMode := TSearchMode( readinteger( section, FindOptionsIniStr.SearchMode, ord( FindOptions.SearchMode )));
      FindOptions.SearchNodeNames := readbool( section, FindOptionsIniStr.SearchNodeNames, FindOptions.SearchNodeNames );

      section := ResPanelOptionsIniStr.section;
      ResPanelOptions.ColorFindList := readbool( section, ResPanelOptionsIniStr.ColorFindList, ResPanelOptions.ColorFindList );
      try
        ResPanelOptions.FindListAltColor := StringToColor( readstring( section, ResPanelOptionsIniStr.FindListAltColor, ColorToString( ResPanelOptions.FindListAltColor )));
      except
      end;
      ResPanelOptions.ShowFind := readbool( section, ResPanelOptionsIniStr.ShowFind, ResPanelOptions.ShowFind );
      ResPanelOptions.ShowMacro := readbool( section, ResPanelOptionsIniStr.ShowMacro, ResPanelOptions.ShowMacro );
      ResPanelOptions.ShowPlugin := readbool( section, ResPanelOptionsIniStr.ShowPlugin, ResPanelOptions.ShowPlugin );
      ResPanelOptions.ShowScratch := readbool( section, ResPanelOptionsIniStr.ShowScratch, ResPanelOptions.ShowScratch );
      ResPanelOptions.ShowTemplate := readbool( section, ResPanelOptionsIniStr.ShowTemplate, ResPanelOptions.ShowTemplate );
      ResPanelOptions.ShowFavorites := readbool( section, ResPanelOptionsIniStr.ShowFavorites, ResPanelOptions.ShowFavorites );
      ResPanelOptions.Stacked := readbool( section, ResPanelOptionsIniStr.Stacked, ResPanelOptions.Stacked );
      ResPanelOptions.TabOrientation := TTabOrientation( readinteger( section, ResPanelOptionsIniStr.TabOrientation, ord( ResPanelOptions.TabOrientation )));

    end;
  finally
    IniFile.Free;
  end;
end; // LoadKeyNoteOptions

procedure SaveMailOptions( const INI_FN : string; const MailOptions : TMailOptions );
var
  section : string;
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create( INI_FN );
  with IniFile do
  begin
    section := MailOptionsIniStr.section;
    writebool( section, MailOptionsIniStr.AsPlainText, MailOptions.AsPlainText );
    writestring( section, MailOptionsIniStr.FirstLine, MailOptions.FirstLine );
    writestring( section, MailOptionsIniStr.History, MailOptions.History );
    writestring( section, MailOptionsIniStr.FromAddr, MailOptions.FromAddr );
    writestring( section, MailOptionsIniStr.SMTPPort, MailOptions.SMTPPort );
    writestring( section, MailOptionsIniStr.SMTPServer, MailOptions.SMTPServer );
    writestring( section, MailOptionsIniStr.TextCharSet, MailOptions.TextCharSet );
    writeinteger( section, MailOptionsIniStr.Timeout, MailOptions.Timeout );
    writebool( section, MailOptionsIniStr.KeepLog, MailOptions.KeepLog );
    writebool( section, MailOptionsIniStr.KeepLog, MailOptions.KeepLog );
    writestring( section, MailOptionsIniStr.SubjectPrefix, MailOptions.SubjectPrefix  );
  end;
  IniFile.Free;
end; // SaveMailOptions

procedure LoadMailOptions( const INI_FN : string; var MailOptions : TMailOptions );
var
  section : string;
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create( INI_FN );
  with IniFile do
  begin
    section := MailOptionsIniStr.section;
    MailOptions.AsPlainText := readbool( section, MailOptionsIniStr.AsPlainText, MailOptions.AsPlainText );
    MailOptions.FirstLine := readstring( section, MailOptionsIniStr.FirstLine, MailOptions.FirstLine );
    MailOptions.History := readstring( section, MailOptionsIniStr.History, MailOptions.History );
    MailOptions.SMTPPort := readstring( section, MailOptionsIniStr.SMTPPort, MailOptions.SMTPPort );
    MailOptions.SMTPServer := readstring( section, MailOptionsIniStr.SMTPServer, MailOptions.SMTPServer );
    MailOptions.TextCharSet := readstring( section, MailOptionsIniStr.TextCharSet, MailOptions.TextCharSet );
    MailOptions.Timeout := readinteger( section, MailOptionsIniStr.Timeout, MailOptions.Timeout );
    MailOptions.SubjectPrefix := readstring( section, MailOptionsIniStr.SubjectPrefix, MailOptions.SubjectPrefix );
    MailOptions.KeepLog := readbool( section, MailOptionsIniStr.KeepLog, MailOptions.KeepLog );
    MailOptions.FromAddr := readstring( section, MailOptionsIniStr.FromAddr, MailOptions.FromAddr );
    MailOptions.XMailer := readstring( section, MailOptionsIniStr.XMailer, MailOptions.XMailer );
  end;
  IniFile.Free;
end; // LoadMailOptions

procedure SaveKeyNoteDefaults(
  const INIFileName : string;
  const DefaultEditorProperties : TNoteEditorProperties;
  const DefaultEditorChrome : TChrome;
  const DefaultTabProperties : TNoteTabProperties
  ; const DefaultTreeProperties : TNoteTreeProperties;
  const DefaultTreeChrome : TChrome
  );
var
  IniFile : TIniFile;
  section : string;
begin

  IniFile := TIniFile.Create( INIFileName );
  try
    with IniFile do
    begin
      section := NoteEditorPropertiesIniStr.section;
      writebool( section, NoteEditorPropertiesIniStr.PlainText, DefaultEditorProperties.PlainText );
      writeinteger( section, NoteEditorPropertiesIniStr.TabSize, DefaultEditorProperties.TabSize );
      writebool( section, NoteEditorPropertiesIniStr.URLDetect, DefaultEditorProperties.URLDetect );
      writebool( section, NoteEditorPropertiesIniStr.UseTabChar, DefaultEditorProperties.UseTabChar );
      writebool( section, NoteEditorPropertiesIniStr.WordWrap, DefaultEditorProperties.WordWrap );

      section := 'EditorChrome';
      writestring( section, ChromeIniStr.BGColor, ColorToString( DefaultEditorChrome.BGColor ));
      writeinteger( section, ChromeIniStr.FontCharset, DefaultEditorChrome.Font.Charset );
      writestring( section, ChromeIniStr.FontColor, ColorToString( DefaultEditorChrome.Font.Color ));
      writestring( section, ChromeIniStr.FontName, DefaultEditorChrome.Font.Name );
      writeinteger( section, ChromeIniStr.FontSize, DefaultEditorChrome.Font.Size );
      writestring( section, ChromeIniStr.FontStyle, FontStyleToStr( DefaultEditorChrome.Font.Style ));
      writestring( section, ChromeIniStr.HiColor, ColorToString( DefaultEditorChrome.HiColor ));
      writeinteger( section, ChromeIniStr.HiFontCharset, DefaultEditorChrome.HiFont.Charset );
      writestring( section, ChromeIniStr.HiFontColor, ColorToString( DefaultEditorChrome.HiFont.Color ));
      writestring( section, ChromeIniStr.HiFontName, DefaultEditorChrome.HiFont.Name );
      writeinteger( section, ChromeIniStr.HiFontSize, DefaultEditorChrome.HiFont.Size );
      writestring( section, ChromeIniStr.HiFontStyle, FontStyleToStr( DefaultEditorChrome.HiFont.Style ));
      writeinteger( section, ChromeIniStr.Language, DefaultEditorChrome.Language );

      section := NoteTabPropertiesIniStr.section;
      writeinteger( section, NoteTabPropertiesIniStr.ImageIndex, DefaultTabProperties.ImageIndex );
      writestring( section, NoteTabPropertiesIniStr.Name, DefaultTabProperties.Name );


      section := NoteTreePropertiesIniStr.section;
      writestring( section, NoteTreePropertiesIniStr.DefaultName, DefaultTreeProperties.DefaultName );
      writebool( section, NoteTreePropertiesIniStr.ShowCheckBoxes, DefaultTreeProperties.CheckBoxes );
      writeinteger( section, NoteTreePropertiesIniStr.IconKind, ord( DefaultTreeProperties.IconKind ));
      writebool( section, NoteTreePropertiesIniStr.AutoNumberNodes, DefaultTreeProperties.AutoNumberNodes );
      writebool( section, NoteTreePropertiesIniStr.VerticalLayout, DefaultTreeProperties.VerticalLayout );
      writebool( section, NoteTreePropertiesIniStr.HideChecked, DefaultTreeProperties.HideChecked );       // [dpv]

      section := 'TreeChrome';
      writestring( section, ChromeIniStr.BGColor, ColorToString( DefaultTreeChrome.BGColor ));
      writeinteger( section, ChromeIniStr.FontCharset, DefaultTreeChrome.Font.Charset );
      writestring( section, ChromeIniStr.FontColor, ColorToString( DefaultTreeChrome.Font.Color ));
      writestring( section, ChromeIniStr.FontName, DefaultTreeChrome.Font.Name );
      writeinteger( section, ChromeIniStr.FontSize, DefaultTreeChrome.Font.Size );
      writestring( section, ChromeIniStr.FontStyle, FontStyleToStr( DefaultTreeChrome.Font.Style ));
      writestring( section, ChromeIniStr.HiColor, ColorToString( DefaultTreeChrome.HiColor ));
      writeinteger( section, ChromeIniStr.HiFontCharset, DefaultTreeChrome.HiFont.Charset );
      writestring( section, ChromeIniStr.HiFontColor, ColorToString( DefaultTreeChrome.HiFont.Color ));
      writestring( section, ChromeIniStr.HiFontName, DefaultTreeChrome.HiFont.Name );
      writeinteger( section, ChromeIniStr.HiFontSize, DefaultTreeChrome.HiFont.Size );
      writestring( section, ChromeIniStr.HiFontStyle, FontStyleToStr( DefaultTreeChrome.HiFont.Style ));

    end;

  finally
    IniFile.Free;
  end;
end; // SaveKeyNoteDefaults


procedure LoadKeyNoteDefaults(
  const OnlyChrome : boolean;
  const INIFileName : string;
  var DefaultEditorProperties : TNoteEditorProperties;
  var DefaultEditorChrome : TChrome;
  var DefaultTabProperties : TNoteTabProperties
  ; var DefaultTreeProperties : TNoteTreeProperties;
  var DefaultTreeChrome : TChrome
  );
var
  IniFile : TIniFile;
  section : string;
begin

  IniFile := TIniFile.Create( INIFileName );
  try
    with IniFile do
    begin

      section := 'EditorChrome';
      DefaultEditorChrome.Font.Name := readstring( section, ChromeIniStr.FontName, DefaultEditorChrome.Font.Name );
      DefaultEditorChrome.Font.Style := StrToFontStyle( readstring( section, ChromeIniStr.FontStyle, '[]' ));
      DefaultEditorChrome.HiFont.Name := readstring( section, ChromeIniStr.HiFontName, DefaultEditorChrome.HiFont.Name );
      DefaultEditorChrome.HiFont.Style := StrToFontStyle( readstring( section, ChromeIniStr.HiFontStyle, '[]' ));
      try
        DefaultEditorChrome.HiFont.Size := readinteger( section, ChromeIniStr.HiFontSize, DefaultEditorChrome.HiFont.Size );
        DefaultEditorChrome.Font.Size := readinteger( section, ChromeIniStr.FontSize, DefaultEditorChrome.Font.Size );
        DefaultEditorChrome.Font.Charset := readinteger( section, ChromeIniStr.FontCharset, DefaultEditorChrome.Font.Charset );
        DefaultEditorChrome.HiFont.Charset := readinteger( section, ChromeIniStr.HiFontCharset, DefaultEditorChrome.HiFont.Charset );
        DefaultEditorChrome.BGColor := StringToColor( readstring( section, ChromeIniStr.BGColor, ColorToString( DefaultEditorChrome.BGColor )));
        DefaultEditorChrome.HiColor := StringToColor( readstring( section, ChromeIniStr.HiColor, ColorToString( DefaultEditorChrome.HiColor )));
        DefaultEditorChrome.Font.Color := StringToColor( readstring( section, ChromeIniStr.FontColor, ColorToString( DefaultEditorChrome.Font.Color )));
        DefaultEditorChrome.HiFont.Color := StringToColor( readstring( section, ChromeIniStr.HiFontColor, ColorToString( DefaultEditorChrome.HiFont.Color )));
        DefaultEditorChrome.Language := readinteger( section, ChromeIniStr.Language, DefaultEditorChrome.Language );
      except
        // throw AWAY exceptions here
      end;

      section := 'TreeChrome';
      DefaultTreeChrome.Font.Name := readstring( section, ChromeIniStr.FontName, DefaultTreeChrome.Font.Name );
      DefaultTreeChrome.Font.Style := StrToFontStyle( readstring( section, ChromeIniStr.FontStyle, '[]' ));
      DefaultTreeChrome.HiFont.Name := readstring( section, ChromeIniStr.HiFontName, DefaultTreeChrome.HiFont.Name );
      DefaultTreeChrome.HiFont.Style := StrToFontStyle( readstring( section, ChromeIniStr.HiFontStyle, '[]' ));

      try
        DefaultTreeChrome.Font.Size := readinteger( section, ChromeIniStr.FontSize, DefaultTreeChrome.Font.Size );
        DefaultTreeChrome.HiFont.Size := readinteger( section, ChromeIniStr.HiFontSize, DefaultTreeChrome.HiFont.Size );
        DefaultTreeChrome.Font.Charset := readinteger( section, ChromeIniStr.FontCharset, DefaultTreeChrome.Font.Charset );
        DefaultTreeChrome.HiFont.Charset := readinteger( section, ChromeIniStr.HiFontCharset, DefaultTreeChrome.HiFont.Charset );
        DefaultTreeChrome.BGColor := StringToColor( readstring( section, ChromeIniStr.BGColor, ColorToString( DefaultTreeChrome.BGColor )));
        DefaultTreeChrome.HiColor := StringToColor( readstring( section, ChromeIniStr.HiColor, ColorToString( DefaultTreeChrome.HiColor )));
        DefaultTreeChrome.Font.Color := StringToColor( readstring( section, ChromeIniStr.FontColor, ColorToString( DefaultTreeChrome.Font.Color )));
        DefaultTreeChrome.HiFont.Color := StringToColor( readstring( section, ChromeIniStr.HiFontColor, ColorToString( DefaultTreeChrome.HiFont.Color )));
      except
        // throw AWAY exceptions here
      end;

      if OnlyChrome then exit;

      section := NoteTabPropertiesIniStr.section;
      DefaultTabProperties.ImageIndex := readinteger( section, NoteTabPropertiesIniStr.ImageIndex, DefaultTabProperties.ImageIndex );
      DefaultTabProperties.Name := readstring( section, NoteTabPropertiesIniStr.Name, DefaultTabProperties.Name );

      section := NoteEditorPropertiesIniStr.section;
      DefaultEditorProperties.PlainText := readbool( section, NoteEditorPropertiesIniStr.PlainText, DefaultEditorProperties.PlainText );
      DefaultEditorProperties.TabSize := readinteger( section, NoteEditorPropertiesIniStr.TabSize, DefaultEditorProperties.TabSize );
      DefaultEditorProperties.URLDetect := readbool( section, NoteEditorPropertiesIniStr.URLDetect, DefaultEditorProperties.URLDetect );
      DefaultEditorProperties.UseTabChar := readbool( section, NoteEditorPropertiesIniStr.UseTabChar, DefaultEditorProperties.UseTabChar );
      DefaultEditorProperties.WordWrap := readbool( section, NoteEditorPropertiesIniStr.WordWrap, DefaultEditorProperties.WordWrap );

      section := NoteTreePropertiesIniStr.section;
      DefaultTreeProperties.DefaultName := readstring( section, NoteTreePropertiesIniStr.DefaultName, DefaultTreeProperties.DefaultName );
      DefaultTreeProperties.CheckBoxes := readbool( section, NoteTreePropertiesIniStr.ShowCheckBoxes, DefaultTreeProperties.CheckBoxes );
      DefaultTreeProperties.AutoNumberNodes := readbool( section, NoteTreePropertiesIniStr.AutoNumberNodes, DefaultTreeProperties.AutoNumberNodes );
      DefaultTreeProperties.IconKind := TNodeIconKind( readinteger( section, NoteTreePropertiesIniStr.IconKind, ord( DefaultTreeProperties.IconKind )));
      DefaultTreeProperties.VerticalLayout := readbool( section, NoteTreePropertiesIniStr.VerticalLayout, DefaultTreeProperties.VerticalLayout );
      DefaultTreeProperties.HideChecked := readbool( section, NoteTreePropertiesIniStr.HideChecked, DefaultTreeProperties.HideChecked );   // [dpv]

    end;
  finally
    IniFile.Free;
    DEFAULT_NEW_NOTE_NAME := DefaultTabProperties.Name;
  end;

end; // LoadKeyNoteDefaults


procedure InitializeResPanelOptions( var Struct : TResPanelOptions );
begin
  with Struct do
  begin
    ColorFindList := false;
    FindListAltColor := clInfoBk;
    ShowFind := true;
    ShowMacro := true;
    ShowPlugin := true;
    ShowScratch := true;
    ShowTemplate := true;
    ShowFavorites := true;
    Stacked := false;
    TabOrientation := low( TabOrientation );
  end;
end; // InitializeResPanelOptions

procedure InitializeExportOptions( var Struct : TExportOptions );
begin
  with Struct do
  begin
    ConfirmFilenames := true;
    ConfirmOverwrite := true;
    ExportPath := ProperFolderName( extractfilepath( ParamStr( 0 )));
    ExportSource := low( TExportSource );
    HTMLNoFormatting := false;
    IncludeNodeHeadings := true;
    IncludeNoteHeadings := true;
    {
    IndentNestedNodes := false;
    IndentUsingTabs := true;
    IndentValue := 1;
    }
    NodeHeading := _DefaultNodeHeading;
    NoteHeading := _DefaultNoteHeading;
    SingleNodeFiles := true;
    TargetFormat := low( TExportFmt );
    TreePadForceMaster := false;
    TreePadRTF := false;
    TreePadSingleFile := true;
    TreeSelection := low( TTreeSelection );

    ExcludeHiddenNodes:= false;              // [dpv]
  end;
end; // InitializeExportOptions

end.
