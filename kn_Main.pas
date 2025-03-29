unit kn_Main;

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

{.$DEFINE DEBUG_IMG}

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.RichEdit,
   Winapi.ShellAPI,
   Winapi.MMSystem,
   System.SysUtils,
   System.Classes,
   System.IniFiles,
   System.ImageList,
   System.Actions,
   System.DateUtils,
   System.StrUtils,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Menus,
   Vcl.ComCtrls,
   Vcl.Samples.Spin,
   Vcl.ExtDlgs,
   Vcl.FileCtrl,
   Vcl.StdCtrls,
   Vcl.Clipbrd,
   Vcl.ExtCtrls,
   Vcl.HtmlHelpViewer,
   Vcl.ImgList,
   Vcl.ActnList,

   ComCtrls95,
   SystemImageList,
   cmpGFXListBox,
   TB97Ctls,
   TB97,
   TB97Tlbr,
   MRUFList,
   RxPlacemnt,
   RxRichEd,
   knt.ui.editor,
   RXShell,
   RxNotify,
   RXCombos,
   RXCtrls,
   //RxGIF {, jpeg},
   TopWnd,
   ColorPicker,
   VirtualTrees,
   VirtualTrees.Types,
   VirtualTrees.BaseTree,
   VirtualTrees.BaseAncestorVCL,
   VirtualTrees.AncestorVCL,

   kn_Info,
   kn_Msgs,
   kn_KntFolder,
   knt.model.note,
   knt.ui.tree
   ;


type
  // cracker class, enables us to get at the protected
  // .EditorHandle property of the combo box control.
  // We need the handle so that we can send EM_xxx messages
  // to the control for undo, canundo, copy, paste, etc.
  TEditHandleComboBox = class( TCustomComboBox );
  TWordWrapMemo = class( TCustomMemo );

  TRTLCommandToExecute = (rtRTL, rtLTR, rtNone);

type
  TForm_Main = class(TForm)
    Menu_Main: TMainMenu;
    MMFile_: TMenuItem;
    MMEdit_: TMenuItem;
    MMNote_: TMenuItem;
    MMFormat_: TMenuItem;
    MMTools_: TMenuItem;
    MMHelp_: TMenuItem;
    MMSearch_: TMenuItem;
    MMView_: TMenuItem;
    MMHelpTip: TMenuItem;
    MMHelpAbout: TMenuItem;
    MMToolsOptions: TMenuItem;
    MMFolderNew: TMenuItem;
    MMNoteProperties: TMenuItem;
    N2: TMenuItem;
    MMFolderRemove: TMenuItem;
    MMFind: TMenuItem;
    MMFindNext: TMenuItem;
    MMFileNew: TMenuItem;
    MMFileProperties: TMenuItem;
    N3: TMenuItem;
    MMFileOpen: TMenuItem;
    MM_MRUSeparator_: TMenuItem;
    MMFileCopyTo: TMenuItem;
    MMFileExit: TMenuItem;
    FormStorage: TFormStorage;
    MRU: TdfsMRUFileList;
    StatusBar: TStatusBar;
    MMFileSave: TMenuItem;
    MMFileSaveAs: TMenuItem;
    N5: TMenuItem;
    MMFileAutoSave: TMenuItem;
    N6: TMenuItem;
    MMToolsExport: TMenuItem;
    MMToolsImport: TMenuItem;
    MMFileClose: TMenuItem;
    Dock_Top: TDock97;
    Toolbar_Main: TToolbar97;
    TB_FileSave: TToolbarButton97;
    TB_FileNew: TToolbarButton97;
    TB_FileOpen: TToolbarButton97;
    TB_EditCut: TToolbarButton97;
    sm3: TToolbarSep97;
    sm4: TToolbarSep97;
    TB_EditUndo: TToolbarButton97;
    TB_EditCopy: TToolbarButton97;
    TB_EditPaste: TToolbarButton97;
    Timer: TTimer;
    WinOnTop: TTopMostWindow;
    TrayIcon: TRxTrayIcon;
    MMNoteRename: TMenuItem;
    Menu_Tray: TPopupMenu;
    TMRestore: TMenuItem;
    N8: TMenuItem;
    TMExit: TMenuItem;
    Pages: TPage95Control;
    Menu_RTF: TPopupMenu;
    RTFMCut: TMenuItem;
    Menu_TAB: TPopupMenu;
    TAM_NewTab: TMenuItem;
    IMG_Toolbar: TImageList;
    TB_NoteDelete: TToolbarButton97;
    sm5: TToolbarSep97;
    TB_NoteNew: TToolbarButton97;
    TB_NoteEdit: TToolbarButton97;
    sm9: TToolbarSep97;
    //TB_Exit: TToolbarButton97;
    TB_Options: TToolbarButton97;
    sm10: TToolbarSep97;
    MMNoteReadOnly: TMenuItem;
    MMFormatWordWrap: TMenuItem;
    MMEditUndo: TMenuItem;
    N9: TMenuItem;
    MMEditCut: TMenuItem;
    MMEditCopy: TMenuItem;
    MMEditPaste: TMenuItem;
    MMEditDelete: TMenuItem;
    N10: TMenuItem;
    MMEditSelectAll: TMenuItem;
    N11: TMenuItem;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    MMEditRedo: TMenuItem;
    TB_EditRedo: TToolbarButton97;
    MMEditPasteAsText: TMenuItem;
    Toolbar_Format: TToolbar97;
    Combo_Font: TFontComboBox;
    sf1: TToolbarSep97;
    sf2: TToolbarSep97;
    TB_AlignLeft: TToolbarButton97;
    TB_Bold: TToolbarButton97;
    TB_Italics: TToolbarButton97;
    TB_Underline: TToolbarButton97;
    sf3: TToolbarSep97;
    TB_Strikeout: TToolbarButton97;
    TB_Bullets: TToolbarButton97;
    TB_AlignCenter: TToolbarButton97;
    sf4: TToolbarSep97;
    TB_AlignRight: TToolbarButton97;
    TB_Outdent: TToolbarButton97;
    TB_Indent: TToolbarButton97;
    IMG_Format: TImageList;
    TB_FileMgr: TToolbarButton97;
    sm2: TToolbarSep97;
    N12: TMenuItem;
    MMFontStyle_: TMenuItem;
    MMFormatBold: TMenuItem;
    MMFormatItalics: TMenuItem;
    MMFormatUnderline: TMenuItem;
    MMFormatStrikeout: TMenuItem;
    N13: TMenuItem;
    MMFormatClearFontAttr: TMenuItem;
    MMParastyle_: TMenuItem;
    MMAlignment_: TMenuItem;
    MMFormatAlignLeft: TMenuItem;
    MMFormatAlignCenter: TMenuItem;
    MMFormatAlignRight: TMenuItem;
    MMFormatBullets: TMenuItem;
    MMFormatLIndInc: TMenuItem;
    MMFormatLIndDec: TMenuItem;
    N14: TMenuItem;
    MMFormatFont: TMenuItem;
    MMFormatTextColor: TMenuItem;
    MMFormatBGColor: TMenuItem;
    MMViewOnTop: TMenuItem;
    MMFileManager: TMenuItem;
    FontDlg: TFontDialog;
    ColorDlg: TColorDialog;
    N16: TMenuItem;
    TAM_Properties: TMenuItem;
    TAM_Renametab: TMenuItem;
    N17: TMenuItem;
    TAM_Delete: TMenuItem;
    N18: TMenuItem;
    RTFMCopy: TMenuItem;
    RTFMPaste: TMenuItem;
    RTFMPasteAsText: TMenuItem;
    RTFMDelete: TMenuItem;
    N19: TMenuItem;
    RTFMWordwrap: TMenuItem;
    N20: TMenuItem;
    RTFMUndo: TMenuItem;
    N21: TMenuItem;
    RTFMSelectall: TMenuItem;
    N22: TMenuItem;
    MMViewTBMain: TMenuItem;
    MMViewTBFormat: TMenuItem;
    N23: TMenuItem;
    TB_FindNext: TToolbarButton97;
    TB_Find: TToolbarButton97;
    sm6: TToolbarSep97;
    N15: TMenuItem;
    MMInsertDate: TMenuItem;
    MMInsertTime: TMenuItem;
    MMFindGoTo: TMenuItem;
    N25: TMenuItem;
    RTFMProperties: TMenuItem;
    MMCopyFormat_: TMenuItem;
    MMPasteFormat_: TMenuItem;
    MMFormatCopyFont: TMenuItem;
    MMFormatCopyPara: TMenuItem;
    MMFormatPasteFont: TMenuItem;
    MMFormatPastePara: TMenuItem;
    TB_FileInfo: TToolbarButton97;
    sm1: TToolbarSep97;
    N4: TMenuItem;
    MMShiftTab_: TMenuItem;
    MMViewShiftTabLeft: TMenuItem;
    MMViewShiftTabRight: TMenuItem;
    sf7: TToolbarSep97;
    MMFontsize_: TMenuItem;
    MMFormatFontSizeInc: TMenuItem;
    MMFormatFontSizeDec: TMenuItem;
    N24: TMenuItem;
    TAM_ActiveName: TMenuItem;
    FolderMon: TRxFolderMonitor;
    N26: TMenuItem;
    MMEditJoin: TMenuItem;
    MMEditCase_: TMenuItem;
    MMEditUpper: TMenuItem;
    MMEditLower: TMenuItem;
    MMEditMixed: TMenuItem;
    MMEditDelLine: TMenuItem;
    TB_FontDlg: TToolbarButton97;
    MMNotePrint: TMenuItem;
    N27: TMenuItem;
    MMNoteClipCapture: TMenuItem;
    N28: TMenuItem;
    MMFilePageSetup: TMenuItem;
    MMNotePrintPreview_: TMenuItem;
    MMEditCopyAll: TMenuItem;
    MMEditPasteAsNewFolder: TMenuItem;
    MMEditRot13: TMenuItem;
    MMNoteEmail: TMenuItem;
    TB_EmailNote: TToolbarButton97;
    TB_ClipCap: TToolbarButton97;
    sm8: TToolbarSep97;
    N7: TMenuItem;
    MMViewAlphaTabs: TMenuItem;
    MMEditSort: TMenuItem;
    N29: TMenuItem;
    MMViewTabIcons: TMenuItem;
    MRUMenu: TPopupMenu;
    MruM_MRUSeparatorBTN_: TMenuItem;
    N33: TMenuItem;
    MMTree_: TMenuItem;
    MMTreeAddNode_Last: TMenuItem;
    IMG_TV: TImageList;
    MMViewToolbars_: TMenuItem;
    MMViewTBTree: TMenuItem;
    MMTreeAddNode_Above: TMenuItem;
    MMTreeAddNode_Child: TMenuItem;
    MMTreeNodeDelete_: TMenuItem;
    N34: TMenuItem;
    MMTreeNodeRename_: TMenuItem;
    MMNodePaste_: TMenuItem;
    MMTreeNodeNameAsDateTime: TMenuItem;
    MMTreeNodeNameAsTime: TMenuItem;
    MMTreeNodeNameAsDate: TMenuItem;
    MMTreeNodeNamePaste: TMenuItem;
    N36: TMenuItem;
    MMTreeSort_: TMenuItem;
    MMTreeSortFull_: TMenuItem;
    MMTreeSortSubtree_: TMenuItem;
    Dock_Left: TDock97;
    MMTreeFullExpand: TMenuItem;
    MMTreeFullCollapse: TMenuItem;
    MMViewNodeIcons: TMenuItem;
    MMTreeDeleteSubtree_: TMenuItem;
    N41: TMenuItem;
    MMMovenode_: TMenuItem;
    MMTreeMoveNodeRight_: TMenuItem;
    MMTreeMoveNodeLeft_: TMenuItem;
    MMTreeMoveNodeDown_: TMenuItem;
    MMTreeMoveNodeUp_: TMenuItem;
    MMToolsDefaults: TMenuItem;
    MMToolsMerge: TMenuItem;
    N35: TMenuItem;
    N43: TMenuItem;
    RTFMWordWeb: TMenuItem;
    TB_WordWeb: TToolbarButton97;
    sm7: TToolbarSep97;
    N45: TMenuItem;
    MMFormatClearParaAttr: TMenuItem;
    MMFormatRIndInc: TMenuItem;
    MMFormatRIndDec: TMenuItem;
    MMEditLines_: TMenuItem;
    MMEditEvaluate: TMenuItem;
    MMEditEval_: TMenuItem;
    MMEditPasteEval: TMenuItem;
    MMFindNode: TMenuItem;
    MMFindNodeNext: TMenuItem;
    N39: TMenuItem;
    MMFormatSubscript: TMenuItem;
    N47: TMenuItem;
    MMFormatDisabled: TMenuItem;
    MMFormatSpBefInc: TMenuItem;
    MMFormatSpBefDec: TMenuItem;
    MMFormatSpAftInc: TMenuItem;
    MMFormatSpAftDec: TMenuItem;
    N48: TMenuItem;
    MMLineSpacing_: TMenuItem;
    MMFormatLS1: TMenuItem;
    MMFormatLS15: TMenuItem;
    MMFormatLS2: TMenuItem;
    MMEditRepeat: TMenuItem;
    RTFMRepeatCmd: TMenuItem;
    Toolbar_Style: TToolbar97;
    Combo_Style: TComboBox;
    TB_Style: TToolbarButton97;
    MMEditTransform_: TMenuItem;
    MMEditReverse: TMenuItem;
    MMViewTBStyle: TMenuItem;
    Menu_Style: TPopupMenu;
    MSStyleApply: TMenuItem;
    MSStyleFont: TMenuItem;
    MSStylePara: TMenuItem;
    MSStyleBoth: TMenuItem;
    N50: TMenuItem;
    MSStyleRename: TMenuItem;
    MSStyleDelete: TMenuItem;
    ToolbarSep9717: TToolbarSep97;
    N51: TMenuItem;
    MMFormatFIndInc: TMenuItem;
    MMFormatFindDec: TMenuItem;
    Dock_Bottom: TDock97;
    N52: TMenuItem;
    MSStyleDescribe: TMenuItem;
    N53: TMenuItem;
    MMFormatHighlight: TMenuItem;
    MSStyleRedef: TMenuItem;
    N54: TMenuItem;
    MMFormatView_: TMenuItem;
    MMViewFormatFont: TMenuItem;
    MMViewFormatPara: TMenuItem;
    MMViewFormatBoth: TMenuItem;
    N55: TMenuItem;
    MMViewFormatNone: TMenuItem;
    MMFormatNoHighlight: TMenuItem;
    N49: TMenuItem;
    TB_Color: TColorBtn;
    TB_Hilite: TColorBtn;
    MMInsertTerm: TMenuItem;
    MMToolsGlosAddTerm: TMenuItem;
    N58: TMenuItem;
    MMEditTrim_: TMenuItem;
    MMEditTrimLeft: TMenuItem;
    MMEditTrimRight: TMenuItem;
    N60: TMenuItem;
    MMEditTrimBoth: TMenuItem;
    MMEditCompress: TMenuItem;
    MMEditPasteOther_: TMenuItem;
    MMEditInvertCase: TMenuItem;
    N59: TMenuItem;
    N62: TMenuItem;
    MMInsertCharacter: TMenuItem;
    MMFindBracket: TMenuItem;
    N63: TMenuItem;
    MMToolsStatistics: TMenuItem;
    MMFormatSuperscript: TMenuItem;
    N46: TMenuItem;
    MMBkmSet_: TMenuItem;
    MMBkmJump_: TMenuItem;
    MMBkmSet1: TMenuItem;
    MMBkmSet2: TMenuItem;
    MMBkmSet0: TMenuItem;
    MMBkmSet3: TMenuItem;
    MMBkmSet4: TMenuItem;
    MMBkmSet5: TMenuItem;
    MMBkmSet6: TMenuItem;
    MMBkmSet7: TMenuItem;
    MMBkmSet8: TMenuItem;
    MMBkmSet9: TMenuItem;
    MMBkmJ0: TMenuItem;
    MMBkmJ1: TMenuItem;
    MMBkmJ2: TMenuItem;
    MMBkmJ3: TMenuItem;
    MMBkmJ4: TMenuItem;
    MMBkmJ5: TMenuItem;
    MMBkmJ6: TMenuItem;
    MMBkmJ8: TMenuItem;
    MMBkmJ7: TMenuItem;
    MMBkmJ9: TMenuItem;
    MMInsert_: TMenuItem;
    N65: TMenuItem;
    MMInsertWordWeb: TMenuItem;
    N57: TMenuItem;
    MMFormatParagraph: TMenuItem;
    MMInsertLinkToFile: TMenuItem;
    MMInsertObject: TMenuItem;
    MMInsertPicture: TMenuItem;
    MMEditPasteSpecial: TMenuItem;
    MMEditPasteAsWebClip: TMenuItem;
    N67: TMenuItem;
    MMInsertFileContents: TMenuItem;
    N68: TMenuItem;
    N69: TMenuItem;
    MMToolsGlosEdit: TMenuItem;
    N70: TMenuItem;
    RTFMFont: TMenuItem;
    RTFMPara: TMenuItem;
    TB_ParaDlg: TToolbarButton97;
    N72: TMenuItem;
    MMHelpVisitWebsite: TMenuItem;
    MMHelpEmailAuthor: TMenuItem;
    Combo_FontSize: TComboBox;
    MMInsertMarkLocation: TMenuItem;
    MMInsertKNTLink: TMenuItem;
    MMFindReplace: TMenuItem;
    MMFindReplaceNext: TMenuItem;
    N74: TMenuItem;
    MMInsertURL: TMenuItem;
    MMFormatApplyStyle: TMenuItem;
    N73: TMenuItem;
    MMToolsPluginRun: TMenuItem;
    MMToolsPluginRunLast: TMenuItem;
    N75: TMenuItem;
    Toolbar_Macro: TToolbar97;
    ToolbarSep9715: TToolbarSep97;
    TB_Macro: TToolbarButton97;
    TB_MacroPause: TToolbarButton97;
    TB_MacroRecord: TToolbarButton97;
    Menu_Macro: TPopupMenu;
    MacMMacro_Play: TMenuItem;
    N66: TMenuItem;
    MacMMacro_Edit: TMenuItem;
    MacMMacro_Delete: TMenuItem;
    N76: TMenuItem;
    MMViewTBAll: TMenuItem;
    MMViewTBHideAll: TMenuItem;
    Sep9719: TToolbarSep97;
    N77: TMenuItem;
    MMToolsMacroRun: TMenuItem;
    N78: TMenuItem;
    MacMMacroUserCommand: TMenuItem;
    MMToolsMacroSelect: TMenuItem;
    MMTreeSaveToFile: TMenuItem;
    N79: TMenuItem;
    MMHelpKeyboardRef: TMenuItem;
    MMHelpMain: TMenuItem;
    MMTemplates_: TMenuItem;
    MMToolsTemplateCreate: TMenuItem;
    MMToolsTemplateInsert: TMenuItem;
    MMViewCheckboxesAllNodes: TMenuItem;
    MMViewTree: TMenuItem;
    MMFormatLanguage: TMenuItem;
    MMNoteSpell: TMenuItem;
    Pages_Res: TPage95Control;
    Splitter_Res: TSplitter;
    MMViewResPanel: TMenuItem;
    ResTab_Find: TTab95Sheet;
    ResTab_RTF: TTab95Sheet;
    ResTab_Macro: TTab95Sheet;
    ResTab_Template: TTab95Sheet;
    Dock_ResMacro: TDock97;
    ListBox_ResMacro: TGFXListBox;
    Panel_ResFind: TPanel;
    ListBox_ResTpl: TGFXListBox;
    Menu_Template: TPopupMenu;
    TPLMTplInsert: TMenuItem;
    N83: TMenuItem;
    TPLMTplCreate: TMenuItem;
    TPLMTplDelete: TMenuItem;
    MacMMacro_Record: TMenuItem;
    Ntbk_ResFind: TNotebook;
    Label1: TLabel;
    Combo_ResFind: TComboBox;
    Btn_ResFind: TButton;
    Btn_ResFlip: TButton;
    CB_ResFind_CaseSens: TCheckBox;
    CB_ResFind_WholeWords: TCheckBox;
    CB_ResFind_AllNotes: TCheckBox;
    chk_LastModifFrom: TCheckBox;
    chk_LastModifUntil: TCheckBox;
    CB_LastModifFrom: TDateTimePicker;
    CB_LastModifUntil: TDateTimePicker;
    MMEditSelectWord: TMenuItem;
    ResTab_Plugins: TTab95Sheet;
    Dock_ResPlugins: TDock97;
    Toolbar_Plugins: TToolbar97;
    ToolbarSep9720: TToolbarSep97;
    TB_PluginRun: TToolbarButton97;
    TB_PluginConfigure: TToolbarButton97;
    ToolbarSep9721: TToolbarSep97;
    Menu_Plugins: TPopupMenu;
    PLM_RunPlugin: TMenuItem;
    PLM_ConfigurePlugin: TMenuItem;
    N64: TMenuItem;
    PLM_ReloadPlugins: TMenuItem;
    ListBox_ResPlugins: TGFXListBox;
    Splitter_plugins: TSplitter;
    Panel_ResPlugins: TPanel;
    LB_PluginInfo: TLabel;
    Menu_ResPanel: TPopupMenu;
    ResMHidepanel: TMenuItem;
    N71: TMenuItem;
    ResMMultilineTabs: TMenuItem;
    ResMTabPosition: TMenuItem;
    N81: TMenuItem;
    ResMTop: TMenuItem;
    ResMBottom: TMenuItem;
    ResMLeft: TMenuItem;
    ResMRight: TMenuItem;
    ResMFindTab: TMenuItem;
    ResMScratchTab: TMenuItem;
    ResMMacroTab: TMenuItem;
    ResMTemplateTab: TMenuItem;
    ResMPluginTab: TMenuItem;
    //List_ResFind: TTextListBox;
    FindAllResults: TRxRichEdit;
    MMToolsMacroRunLast: TMenuItem;
    ResMPanelPosition: TMenuItem;
    ResMPanelLeft: TMenuItem;
    ResMPanelRight: TMenuItem;
    N84: TMenuItem;
    Menu_StdEdit: TPopupMenu;
    StdEMUndo: TMenuItem;
    N85: TMenuItem;
    StdEMCut: TMenuItem;
    StdEMCopy: TMenuItem;
    StdEMPaste: TMenuItem;
    StdEMDelete: TMenuItem;
    N86: TMenuItem;
    StdEMSelectAll: TMenuItem;
    N87: TMenuItem;
    MMTreeMasterNode: TMenuItem;
    RG_ResFind_Type: TComboBox;
    RG_ResFind_Scope: TComboBox;
    Menu_FindAll: TPopupMenu;
    FAMCopytoEditor: TMenuItem;
    FAMCopyAlltoEditor: TMenuItem;
    MMTreeNodeFromSel: TMenuItem;
    MMTreeAddNode_Below: TMenuItem;
    ResTab_Favorites: TTab95Sheet;
    ListBox_ResFav: TGFXListBox;
    ResMFavTab: TMenuItem;
    Menu_Fav: TPopupMenu;
    FavMJump: TMenuItem;
    N89: TMenuItem;
    FavMAdd: TMenuItem;
    FavMDel: TMenuItem;
    FavMAddExternal: TMenuItem;
    N91: TMenuItem;
    MMEditPasteAsNewNode: TMenuItem;
    StdEMWordWrap: TMenuItem;
    MMTreeOutlineNum: TMenuItem;
    N92: TMenuItem;
    MMHistoryGoBack: TMenuItem;
    MMHistoryGoForward: TMenuItem;
    TB_GoForward: TToolbarButton97;
    TB_GoBack: TToolbarButton97;
    TB_Numbers: TToolbarButton97;
    sf6: TToolbarSep97;
    MMFormatNumbers: TMenuItem;
    Menu_Numbers: TPopupMenu;
    MMArabicNumbers: TMenuItem;
    MMLoLetter: TMenuItem;
    MMUpLetter: TMenuItem;
    MMLoRoman: TMenuItem;
    MMUpRoman: TMenuItem;
    N94: TMenuItem;
    N95: TMenuItem;
    TB_Space2: TToolbarButton97;
    TB_Space1: TToolbarButton97;
    TB_Space15: TToolbarButton97;
    N96: TMenuItem;
    N97: TMenuItem;
    N98: TMenuItem;
    N99: TMenuItem;
    TB_Repeat: TToolbarButton97;
    TB_ResPanel: TToolbarButton97;
    TB_WordWrap: TToolbarButton97;
    sf5: TToolbarSep97;
    TB_Print: TToolbarButton97;
    TB_Spell: TToolbarButton97;
    TB_Replace: TToolbarButton97;
    TB_Subscript: TToolbarButton97;
    TB_Superscript: TToolbarButton97;
    TB_OnTop: TToolbarButton97;
    N100: TMenuItem;
    MMHelpWhatsNew: TMenuItem;
    N101: TMenuItem;
    MMViewTBRefresh: TMenuItem;
    N102: TMenuItem;
    MMViewCustomIcons: TMenuItem;
    MMToolsCustomKBD: TMenuItem;
    N61: TMenuItem;
    N82: TMenuItem;
    MMTreeNodeNameAsSel: TMenuItem;
    MMTreeNav_: TMenuItem;
    MMTreeNavUp: TMenuItem;
    MMTreeNavDown: TMenuItem;
    MMTreeNavLeft: TMenuItem;
    MMTreeNavRight: TMenuItem;
    StdEMPastePlain: TMenuItem;
    MMAddTreeNode_: TMenuItem;
    MMFormatAlignJustify: TMenuItem;
    N38: TMenuItem;
    FavMRef: TMenuItem;
    N105: TMenuItem;
    MMToolsUAS: TMenuItem;
    MMToolsUASConfig: TMenuItem;
    MMEditCycleCase: TMenuItem;
    TB_AlignJustify: TToolbarButton97;
    Combo_Zoom: TComboBox;
    N106: TMenuItem;
    MMViewZoomIn: TMenuItem;
    MMViewZoomOut: TMenuItem;
    MMViewTBSaveConfig: TMenuItem;
    Menu_Paste: TPopupMenu;
    MMP_Paste: TMenuItem;
    MMP_PastePlain: TMenuItem;
    MMP_PasteAsWebClip: TMenuItem;
    N107: TMenuItem;
    MMP_PasteSpecial: TMenuItem;
    MMP_PasteAsNote: TMenuItem;
    MMP_PasteAsNode: TMenuItem;
    Menu_Date: TPopupMenu;
    Menu_Time: TPopupMenu;
    Menu_Symbols: TPopupMenu;
    N108: TMenuItem;
    N109: TMenuItem;
    Toolbar_Insert: TToolbar97;
    ToolbarButton971: TToolbarButton97;
    ToolbarButton972: TToolbarButton97;
    ToolbarButton973: TToolbarButton97;
    MMViewTBInsert: TMenuItem;
    N110: TMenuItem;
    N111: TMenuItem;
    md1: TMenuItem;
    md2: TMenuItem;
    md3: TMenuItem;
    md4: TMenuItem;
    md6: TMenuItem;
    md7: TMenuItem;
    md8: TMenuItem;
    md9: TMenuItem;
    md10: TMenuItem;
    md11: TMenuItem;
    md12: TMenuItem;
    md13: TMenuItem;
    md14: TMenuItem;
    md15: TMenuItem;
    md16: TMenuItem;
    md17: TMenuItem;
    md18: TMenuItem;
    md19: TMenuItem;
    md20: TMenuItem;
    md21: TMenuItem;
    md22: TMenuItem;
    md23: TMenuItem;
    md24: TMenuItem;
    md25: TMenuItem;
    mt1: TMenuItem;
    mt2: TMenuItem;
    mt3: TMenuItem;
    mt4: TMenuItem;
    mt5: TMenuItem;
    mt6: TMenuItem;
    mt7: TMenuItem;
    mt8: TMenuItem;
    mt9: TMenuItem;
    ms1: TMenuItem;
    ms2: TMenuItem;
    ms3: TMenuItem;
    ms4: TMenuItem;
    ms5: TMenuItem;
    ms6: TMenuItem;
    ms7: TMenuItem;
    ms8: TMenuItem;
    ms9: TMenuItem;
    ms10: TMenuItem;
    MMEditDecimalToRoman: TMenuItem;
    N112: TMenuItem;
    MMEditRomanToDecimal: TMenuItem;
    MMViewStatusBar: TMenuItem;
    FavMProperties: TMenuItem;
    N113: TMenuItem;
    TMClipCap: TMenuItem;
    Img_System: TdfsSystemImageList;
    MMViewHideCheckedNodes: TMenuItem;
    CB_ResFind_HiddenNodes: TCheckBox;
    MMViewFilterTree: TMenuItem;
    TB_SetAlarm: TToolbarButton97;
    TB_AlarmMode: TToolbarButton97;
    NU01: TMenuItem;
    MMRightParenthesis: TMenuItem;
    MMEnclosed: TMenuItem;
    MMPeriod: TMenuItem;
    MMOnlyNumber: TMenuItem;
    MMWithoutNextNumber: TMenuItem;
    MMStartsNewNumber: TMenuItem;
    NU02: TMenuItem;
    MMEditPasteAsWebClipText: TMenuItem;
    MMP_PasteAsWebClipText: TMenuItem;
    MMEditPlainDefaultPaste: TMenuItem;
    N116: TMenuItem;
    N117: TMenuItem;
    MMP_PlainDefaultPaste: TMenuItem;
    TB_CopyFormat: TToolbarButton97;
    ToolbarSep971: TToolbarSep97;
    MMFormatCopy: TMenuItem;
    CB_ResFind_CurrentNodeAndSubtree: TCheckBox;
    Btn_ResFind_Prev: TToolbarButton97;
    Btn_ResFind_Next: TToolbarButton97;
    LblFindAllNumResults: TLabel;
    MMAlternativeMargins: TMenuItem;
    MMShowImages: TMenuItem;
    ToolbarSep972: TToolbarSep97;
    TB_Images: TToolbarButton97;
    N118: TMenuItem;
    RTFMRestoreProportions: TMenuItem;
    RG_ResFind_ChkMode: TComboBox;
    N120: TMenuItem;
    MMAlarms: TMenuItem;
    MMSetAlarm: TMenuItem;
    MMShowAlarms: TMenuItem;
    MMAlarmsPopup: TMenuItem;
    MMViewHistory: TMenuItem;
    MMHelpChkUpd: TMenuItem;
    N1: TMenuItem;
    MMTreeFocus_: TMenuItem;
    MMTreeFocusToogle: TMenuItem;
    MMTreeFocusEditor: TMenuItem;
    MMTreeFocusTree: TMenuItem;
    MMFindAll: TMenuItem;
    actList_TV: TActionList;
    actList_TVs: TActionList;
    //-------------- Menu_TV -----------
    Menu_TV: TPopupMenu;
    TVAlarmNode: TMenuItem;
    TVInsertNode: TMenuItem;
    TVAddChildNode: TMenuItem;
    TVAddSibling: TMenuItem;
    TVAddNode: TMenuItem;
    N44: TMenuItem;
    TVChildrenCheckboxes_: TMenuItem;
    TVChildrenCheckbox: TMenuItem;
    N119: TMenuItem;
    TVHideCheckedChildren: TMenuItem;
    TVHideUncheckedChildren: TMenuItem;
    TVShowNonFilteredChildren: TMenuItem;
    TVCheckNode: TMenuItem;
    TVBoldNode: TMenuItem;
    TVNodeColor_: TMenuItem;
    TVNodeTextColor: TMenuItem;
    TVNodeBGColor: TMenuItem;
    N103: TMenuItem;
    TVDefaultNodeFont: TMenuItem;
    TVSelectNodeImage: TMenuItem;
    N80: TMenuItem;
    TVVirtualNode_: TMenuItem;
    TVVirtualNode: TMenuItem;
    TVRefreshVirtualNode: TMenuItem;
    TVInsertLinkedNode: TMenuItem;
    TVNavigateNextLinkedNNode: TMenuItem;
    N90: TMenuItem;
    TVUnlinkVirtualNode: TMenuItem;
    N30: TMenuItem;
    TVMovenode_: TMenuItem;
    TVMoveNodeUp: TMenuItem;
    TVMoveNodeDown: TMenuItem;
    TVMoveNodeLeft: TMenuItem;
    TVMoveNodeRight: TMenuItem;
    TVTransfer_: TMenuItem;
    TVCutSubtree: TMenuItem;
    TVCopySubtree: TMenuItem;
    TVGraftSubtree: TMenuItem;
    TVGraftSubtreeLinked: TMenuItem;
    N56: TMenuItem;
    TVEraseTreeMem: TMenuItem;
    TVExport: TMenuItem;
    N40: TMenuItem;
    TVDeleteNode: TMenuItem;
    TVDeleteChildren: TMenuItem;
    N32: TMenuItem;
    TVRenameNode: TMenuItem;
    TVCopyNode_: TMenuItem;
    TVCopyNodeName: TMenuItem;
    TVCopyNodePath: TMenuItem;
    TVCopyNodeText: TMenuItem;
    TVCopyPathtoEditor: TMenuItem;
    TVPasteNode_: TMenuItem;
    TVPasteNodeName: TMenuItem;
    N42: TMenuItem;
    TVPasteNodeNameAsDate: TMenuItem;
    TVPasteNodeNameAsTime: TMenuItem;
    TVPasteNodeNameAsDateTime: TMenuItem;
    N104: TMenuItem;
    TVPasteNodeNameAsSel: TMenuItem;
    TVSortNodes_: TMenuItem;
    TVSortSubtree: TMenuItem;
    TVSortTree: TMenuItem;
    actTVAlarmNode: TAction;
    actList_File: TActionList;
    actFileSave: TAction;
    TVAddParent: TMenuItem;
    actTVAddNode_Parent: TAction;
    AddParent1: TMenuItem;
    TVLinkedNode_: TMenuItem;
    RTFMPlainText: TMenuItem;
    TVFlaggedNode: TMenuItem;
    N31: TMenuItem;
    TVView_Filter: TMenuItem;
    TVViewAdditColumns: TMenuItem;
    TVFilterOutUnflagged: TMenuItem;
    actTVViewAdditColumns: TAction;
    actTVFilterOutUnflagged: TAction;
    actTVFlaggedNode: TAction;
    actTVBoldNode: TAction;
    actTVCheckNode: TAction;
    actTVChildrenCheckbox: TAction;
    actTVUnlinkVirtualNode: TAction;
    actTVNavigateNextLinkedNNode: TAction;
    actTVRefreshVirtualNode: TAction;
    actTVVirtualNode: TAction;
    Label2: TLabel;
    Label3: TLabel;
    chk_CreatedFrom: TCheckBox;
    CB_CreatedFrom: TDateTimePicker;
    chk_CreatedUntil: TCheckBox;
    CB_CreatedUntil: TDateTimePicker;
    CB_ResFind_Filter: TCheckBox;
    CB_ResFind_PathInNames: TCheckBox;
    N37: TMenuItem;
    TVFilterUsePath: TMenuItem;
    TVFilterShowChildren: TMenuItem;
    MMToolsDeduceDates: TMenuItem;
    MMToolsRemoveDates: TMenuItem;
    MMViewEditorInfoPanel: TMenuItem;
    lblCalNotSup: TLabel;
    RTFM_RTL: TMenuItem;
    MMFilePrint: TMenuItem;
    N88: TMenuItem;
    RTFMFold: TMenuItem;
    RTFMUnfold: TMenuItem;
    CbFindFoldedMode: TComboBox;
    Label4: TLabel;
    ResTab_Tags: TTab95Sheet;
    TVTags: TVirtualStringTree;
    txtFilterTags: TEdit;
    ResMTagsTab: TMenuItem;
    Menu_Tags: TPopupMenu;
    TagsMCreate: TMenuItem;
    mi2: TMenuItem;
    TagsMEdit: TMenuItem;
    TagsMDel: TMenuItem;
    RTFMTags: TMenuItem;
    TagsMAdd: TMenuItem;
    TagsMRemove: TMenuItem;
    chkFilterOnTags: TCheckBox;
    cbTagFilterMode: TComboBox;
    CheckImages: TImageList;
    chkInhTags: TCheckBox;
    lblTg: TLabel;
    lblTg2: TLabel;
    TVFilterInhTags: TMenuItem;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    txtTagsIncl: TEdit;
    lbl8: TLabel;
    cbTagFindMode: TComboBox;
    txtTagsExcl: TEdit;
    lbl9: TLabel;
    chkTagsMetad: TCheckBox;
    chkTagsText: TCheckBox;
    lbl4: TLabel;
    chkInhTagsFind: TCheckBox;
    lbl7: TLabel;
    mi3: TMenuItem;
    TagsMExport: TMenuItem;
    TagsMImport: TMenuItem;
    //---------
    procedure MMStartsNewNumberClick(Sender: TObject);
    procedure MMRightParenthesisClick(Sender: TObject);
    procedure TntFormResize(Sender: TObject);
    procedure TB_AlarmModeMouseEnter(Sender: TObject);
    procedure TB_AlarmModeClick(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure TB_SetAlarmMouseEnter(Sender: TObject);
    procedure TB_SetAlarmClick(Sender: TObject);
    procedure MMViewFilterTreeClick(Sender: TObject);
    procedure PagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MMFileExitClick(Sender: TObject);
    procedure TMRestoreClick(Sender: TObject);
    procedure MMFileNewClick(Sender: TObject);
    procedure MMFileOpenClick(Sender: TObject);
    procedure MMFolderNewClick(Sender: TObject);
    procedure MMHelpTipClick(Sender: TObject);
    procedure MMFileCloseClick(Sender: TObject);
    procedure MMNoteRenameClick(Sender: TObject);
    procedure MMFilePropertiesClick(Sender: TObject);
    procedure MMFileAutoSaveClick(Sender: TObject);
    procedure RxFindAllResultsSelectionChange(Sender: TObject);
    procedure MMEditSelectAllClick(Sender: TObject);
    procedure MMFormatWordWrapClick(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure PagesTabShift(Sender: TObject);
    procedure MMEditCutClick(Sender: TObject);
    procedure MMEditCopyClick(Sender: TObject);
    procedure MMEditPasteClick(Sender: TObject);
    procedure MMEditDeleteClick(Sender: TObject);
    procedure MMEditUndoClick(Sender: TObject);
    procedure MMEditRedoClick(Sender: TObject);
    procedure MMEditPasteAsTextClick(Sender: TObject);
    procedure MMNoteReadOnlyClick(Sender: TObject);
    procedure Combo_FontChange(Sender: TObject);
    procedure MMFormatBoldClick(Sender: TObject);
    procedure MMFormatItalicsClick(Sender: TObject);
    procedure MMFormatUnderlineClick(Sender: TObject);
    procedure MMFormatStrikeoutClick(Sender: TObject);
    procedure MMFormatClearFontAttrClick(Sender: TObject);
    procedure MMFormatBulletsClick(Sender: TObject);
    procedure MMFormatLIndIncClick(Sender: TObject);
    procedure MMFormatLIndDecClick(Sender: TObject);
    procedure MMFormatAlignLeftClick(Sender: TObject);
    procedure MMFormatAlignCenterClick(Sender: TObject);
    procedure MMFormatAlignRightClick(Sender: TObject);
    procedure MMFormatFontClick(Sender: TObject);
    procedure MMFormatTextColorClick(Sender: TObject);
    procedure MMFormatBGColorClick(Sender: TObject);
    procedure MRUMRUItemClick(Sender: TObject; AFilename: string);
    procedure DebugMenuClick(Sender: TObject);
    procedure MMFormatNumbersClick(Sender: TObject);
    procedure MMFormatAlignJustifyClick(Sender: TObject);
    procedure MMToolsOptionsClick(Sender: TObject);
    procedure MMHelpAboutClick(Sender: TObject);
    procedure MMViewOnTopClick(Sender: TObject);
    procedure MMViewTBMainClick(Sender: TObject);
    procedure MMViewTBFormatClick(Sender: TObject);
    procedure MMFileManagerClick(Sender: TObject);
    procedure MMInsertDateClick(Sender: TObject);
    procedure MMInsertTimeClick(Sender: TObject);
    procedure MMFolderRemoveClick(Sender: TObject);
    procedure MMFileCopyToClick(Sender: TObject);
    procedure MMFindGoToClick(Sender: TObject);
    procedure MMToolsImportClick(Sender: TObject);
    procedure MMNotePropertiesClick(Sender: TObject);
    procedure TB_ExitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MMSortClick(Sender: TObject);
    procedure MMFormatCopyClick(Sender: TObject);
    procedure MMFormatCopyFontClick(Sender: TObject);
    procedure MMFormatPasteFontClick(Sender: TObject);
    procedure MMFormatCopyParaClick(Sender: TObject);
    procedure MMFormatPasteParaClick(Sender: TObject);
    procedure MMFindClick(Sender: TObject);
    procedure MMFindNextClick(Sender: TObject);
    procedure MMViewShiftTabLeftClick(Sender: TObject);
    procedure MMViewShiftTabRightClick(Sender: TObject);
    procedure MMFormatFontSizeDecClick(Sender: TObject);
    procedure MMFormatFontSizeIncClick(Sender: TObject);
    procedure MMEditJoinClick(Sender: TObject);
    procedure MMEditUpperClick(Sender: TObject);
    procedure MMEditLowerClick(Sender: TObject);
    procedure MMEditMixedClick(Sender: TObject);
    procedure MMEditDelLineClick(Sender: TObject);
    procedure FolderMonChange(Sender: TObject);
    procedure MMNotePrintClick(Sender: TObject);
    procedure MMNoteClipCaptureClick(Sender: TObject);
    procedure MMFilePageSetupClick(Sender: TObject);
    procedure MMNotePrintPreview_Click(Sender: TObject);
    procedure MMEditCopyAllClick(Sender: TObject);
    procedure MMEditPasteAsNewFolderClick(Sender: TObject);
    procedure MMEditRot13Click(Sender: TObject);
    procedure MMNoteEmailClick(Sender: TObject);
    procedure TB_ClipCapClick(Sender: TObject);
    procedure MMViewAlphaTabsClick(Sender: TObject);
    procedure MMViewTabIconsClick(Sender: TObject);
    procedure MMToolsDefaultsClick(Sender: TObject);
    procedure MMToolsMergeClick(Sender: TObject);
    procedure RTFMWordWebClick(Sender: TObject);
    procedure MMHelp_WhatisClick(Sender: TObject);
    procedure MMFormatClearParaAttrClick(Sender: TObject);
    procedure MMFormatRIndIncClick(Sender: TObject);
    procedure MMFormatRIndDecClick(Sender: TObject);
    procedure MathParserParseError(Sender: TObject; ParseError: Integer);
    procedure MMEditEvaluateClick(Sender: TObject);
    procedure MMEditPasteEvalClick(Sender: TObject);

    procedure MMViewTBTreeClick(Sender: TObject);
    procedure ShowTBTree(Show: boolean);
    procedure MMTreeFullExpandClick(Sender: TObject);
    procedure MMTreeFullCollapseClick(Sender: TObject);
    {
    procedure TVBeforeItemPaint(Sender: TObject;
      Node: PVirtualNode; ItemRect: TRect; NodeStates: TNodeStates;
      var OwnerDraw: Boolean);
    }
    procedure MMViewNodeIconsClick(Sender: TObject);
    procedure PagesDblClick(Sender: TObject );
    procedure MMFindNodeClick(Sender: TObject);
    procedure MMFindNodeNextClick(Sender: TObject);
    procedure MMMergeNotestoFileClick(Sender: TObject);
    procedure MMFormatDisabledClick(Sender: TObject);
    procedure MMFormatSubscriptClick(Sender: TObject);
    procedure MMFormatSpBefIncClick(Sender: TObject);
    procedure MMFormatSpBefDecClick(Sender: TObject);
    procedure MMFormatSpAftIncClick(Sender: TObject);
    procedure MMFormatSpAftDecClick(Sender: TObject);
    procedure MMFormatLS1Click(Sender: TObject);
    procedure MMFormatLS15Click(Sender: TObject);
    procedure MMFormatLS2Click(Sender: TObject);
    procedure MMEditRepeatClick(Sender: TObject);
    procedure MMEditReverseClick(Sender: TObject);
    procedure MMViewTBStyleClick(Sender: TObject);
    procedure Btn_StyleClick(Sender: TObject);
    procedure BtnStyleApplyClick(Sender: TObject);
    procedure MMFormatFIndIncClick(Sender: TObject);
    procedure MMFormatFindDecClick(Sender: TObject);
    procedure Combo_StyleChange(Sender: TObject);
    procedure MMFormatHighlightClick(Sender: TObject);
    procedure MMViewFormatNoneClick(Sender: TObject);
    procedure CheckTrackStyleInfo(Editor: TKntRichEdit);
    procedure MMFormatNoHighlightClick(Sender: TObject);
    procedure TB_ColorClick(Sender: TObject);
    procedure TB_HiliteClick(Sender: TObject);
    procedure PagesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure MMInsertTermClick(Sender: TObject);
    procedure MMToolsGlosAddTermClick(Sender: TObject);
    procedure Toolbar_FormatClose(Sender: TObject);
    procedure MMEditTrimLeftClick(Sender: TObject);
    procedure MMEditCompressClick(Sender: TObject);
    procedure MMEditInvertCaseClick(Sender: TObject);
    procedure MMInsertCharacterClick(Sender: TObject);
    procedure MMFindBracketClick(Sender: TObject);
    procedure MMToolsStatisticsClick(Sender: TObject);
    procedure MMFormatSuperscriptClick(Sender: TObject);
    procedure MMBkmJ9Click(Sender: TObject);
    procedure MMBkmSet9Click(Sender: TObject);
    procedure MMFormatParagraphClick(Sender: TObject);
    procedure MMInsertLinkToFileClick(Sender: TObject);
    procedure MMInsertObjectClick(Sender: TObject);
    procedure MMInsertPictureClick(Sender: TObject);
    procedure MMEditPasteSpecialClick(Sender: TObject);
    procedure MMInsertFileContentsClick(Sender: TObject);
    procedure MMToolsGlosEditClick(Sender: TObject);
    procedure MMHelpVisitWebsiteClick(Sender: TObject);
    //procedure MMHelpEmailAuthorClick(Sender: TObject);
    procedure Combo_FontSizeKeyPress(Sender: TObject; var Key: Char);
    procedure Combo_FontSizeClick(Sender: TObject);
    procedure MMInsertMarkLocationClick(Sender: TObject);
    procedure MMInsertKNTLinkClick(Sender: TObject);
    procedure MMFindReplaceClick(Sender: TObject);
    procedure MMFindReplaceNextClick(Sender: TObject);
    procedure MMInsertURLClick(Sender: TObject);
    procedure Combo_StyleKeyPress(Sender: TObject; var Key: Char);
    procedure Toolbar_StyleRecreating(Sender: TObject);
    procedure Toolbar_StyleRecreated(Sender: TObject);
    procedure MMToolsPluginRunClick(Sender: TObject);
    procedure MMToolsPluginRunLastClick(Sender: TObject);
    procedure MacMMacro_EditClick(Sender: TObject);
    procedure TB_MacroClick(Sender: TObject);
    procedure MacMMacro_DeleteClick(Sender: TObject);
    procedure TB_MacroRecordClick(Sender: TObject);
    procedure TB_MacroPauseClick(Sender: TObject);
    procedure MacMMacroUserCommandClick(Sender: TObject);
    procedure Combo_MacroClick(Sender: TObject);
    procedure Combo_MacroKeyPress(Sender: TObject; var Key: Char);
    procedure MMToolsMacroSelectClick(Sender: TObject);
    procedure MMViewTBHideAllClick(Sender: TObject);
    procedure MMTreeSaveToFileClick(Sender: TObject);
    procedure MMHelpKeyboardRefClick(Sender: TObject);
    procedure MMHelpMainClick(Sender: TObject);
    // procedure MMToolsCalculatorClick(Sender: TObject);
    procedure MMToolsTemplateCreateClick(Sender: TObject);
    procedure MMToolsTemplateInsertClick(Sender: TObject);
    procedure MMViewCheckboxesAllNodesClick(Sender: TObject);
    procedure MMViewTreeClick(Sender: TObject);
    procedure MMFormatLanguageClick(Sender: TObject);
    procedure MMNoteSpellClick(Sender: TObject);
    procedure Markaslink1Click(Sender: TObject);
    procedure Hiddentext1Click(Sender: TObject);
    procedure Combo_FontKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MMViewResPanelClick(Sender: TObject);
    procedure Pages_ResChange(Sender: TObject);
    procedure ListBox_ResMacroDblClick(Sender: TObject);
    procedure ListBox_ResTplDblClick(Sender: TObject);
    procedure TPLMTplDeleteClick(Sender: TObject);
    procedure TPLMTplInsertClick(Sender: TObject);
    procedure Splitter_ResMoved(Sender: TObject);
    procedure Btn_ResFlipClick(Sender: TObject);
    procedure VisibilityControlsFindAllResults (Visible: boolean);
    procedure MMEditSelectWordClick(Sender: TObject);
    procedure PLM_ReloadPluginsClick(Sender: TObject);
    procedure ListBox_ResPluginsClick(Sender: TObject);
    procedure PLM_RunPluginClick(Sender: TObject);
    procedure PLM_ConfigurePluginClick(Sender: TObject);
    procedure ResMRightClick(Sender: TObject);
    procedure ResMMultilineTabsClick(Sender: TObject);
    procedure Menu_ResPanelPopup(Sender: TObject);
    procedure ResMPluginTabClick(Sender: TObject);
    procedure MMToolsMacroRunLastClick(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const R: TRect);
    procedure Combo_ResFindChange(Sender: TObject);
    procedure Btn_ResFindClick(Sender: TObject);
    procedure Combo_ResFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Combo_FontSizeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
{ // [dpv]
    procedure List_ResFindDblClick(Sender: TObject);
    procedure List_ResFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
}
    procedure ResMPanelRightClick(Sender: TObject);
    procedure StdEMSelectAllClick(Sender: TObject);
    procedure Menu_StdEditPopup(Sender: TObject);
    procedure MMTreeMasterNodeClick(Sender: TObject);
    procedure FAMCopytoEditorClick(Sender: TObject);
    procedure FAMCopyAlltoEditorClick(Sender: TObject);
    procedure MMTreeNodeFromSelClick(Sender: TObject);
    procedure FavMJumpClick(Sender: TObject);
    procedure FavMAddClick(Sender: TObject);
    procedure FavMDelClick(Sender: TObject);
    procedure ListBox_ResFavClick(Sender: TObject);
    procedure ListBox_ResTplClick(Sender: TObject);
    procedure FavMAddExternalClick(Sender: TObject);
    procedure MMEditPasteAsNewNodeClick(Sender: TObject);
    procedure MMTreeOutlineNumClick(Sender: TObject);
    procedure MMHistoryGoBackClick(Sender: TObject);
    procedure MMHistoryGoForwardClick(Sender: TObject);
    procedure MMUpRomanClick(Sender: TObject);
    procedure MMHelpWhatsNewClick(Sender: TObject);
    procedure MMViewTBRefreshClick(Sender: TObject);
    procedure Res_RTFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MMToolsCustomKBDClick(Sender: TObject);
    procedure MMTreeNavRightClick(Sender: TObject);
    procedure MMToolsExportExClick(Sender: TObject);
    procedure FavMRefClick(Sender: TObject);
    procedure MMToolsUASClick(Sender: TObject);
    procedure MMToolsUASConfigClick(Sender: TObject);
    procedure MMEditCycleCaseClick(Sender: TObject);
    procedure Combo_ZoomDblClick(Sender: TObject);
    procedure MMViewZoomInClick(Sender: TObject);
    procedure MMViewZoomOutClick(Sender: TObject);
{ // dpv
    procedure List_ResFindDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
}

    procedure MMViewTBSaveConfigClick(Sender: TObject);
    procedure Menu_TimePopup(Sender: TObject);
    procedure Menu_DatePopup(Sender: TObject);
    procedure md25Click(Sender: TObject);
    procedure mt8Click(Sender: TObject);
    procedure ms11Click(Sender: TObject);
    procedure Menu_SymbolsPopup(Sender: TObject);
    procedure MMViewTBInsertClick(Sender: TObject);
    //procedure MMToolsURLClick(Sender: TObject);
    procedure Combo_StyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    //procedure DoBorder1Click(Sender: TObject);
    procedure MMEditDecimalToRomanClick(Sender: TObject);
    procedure MMEditRomanToDecimalClick(Sender: TObject);
    procedure MMViewStatusBarClick(Sender: TObject);
    procedure FavMPropertiesClick(Sender: TObject);
    procedure MMEditPasteAsWebClipClick(Sender: TObject);
    procedure MMEditPasteAsWebClipTextClick(Sender: TObject);
    procedure MMEditPlainDefaultPasteClick(Sender: TObject);
    procedure MMP_PlainDefaultPasteClick(Sender: TObject);
    procedure PlainDefaultPaste_Toggled;

    // Tree commands
    procedure actTVAddNode_AboveExecute(Sender: TObject);
    procedure actTVAddNode_ChildExecute(Sender: TObject);
    procedure actTVAddNode_BelowExecute(Sender: TObject);
    procedure actTVAddNode_LastExecute(Sender: TObject);
    procedure actTVCheckNodeExecute(Sender: TObject);
    procedure actTVBoldNodeExecute(Sender: TObject);
    procedure actTVNodeTextColorExecute(Sender: TObject);
    procedure actTVNodeBGColorExecute(Sender: TObject);
    procedure actTVDefaultNodeFontExecute(Sender: TObject);
    procedure actTVSelectNodeImageExecute(Sender: TObject);
    procedure actTVChildrenCheckboxExecute(Sender: TObject);
    procedure actTVHideCheckedChildrenExecute(Sender: TObject);
    procedure actTVHideUncheckedChildrenExecute(Sender: TObject);
    procedure actTVShowNonFilteredExecute(Sender: TObject);
    procedure actTVDeleteNodeExecute(Sender: TObject);
    procedure actTVDeleteChildrenExecute(Sender: TObject);
    procedure actTVMoveNodeUpExecute(Sender: TObject);
    procedure actTVMoveNodeDownExecute(Sender: TObject);
    procedure actTVMoveNodeLeftExecute(Sender: TObject);
    procedure actTVMoveNodeRightExecute(Sender: TObject);
    procedure actTVPasteNodeNameExecute(Sender: TObject);
    procedure actTVCopyNodeNameExecute(Sender: TObject);
    procedure actTVCopyNodeTextExecute(Sender: TObject);
    procedure actTVCopyNodePathExecute(Sender: TObject);
    procedure actTVCopyPathtoEditorExecute(Sender: TObject);
    procedure actTVAlarmNodeExecute(Sender: TObject);
    procedure actTVVirtualNodeExecute(Sender: TObject);
    procedure actTVRefreshVirtualNodeExecute(Sender: TObject);
    procedure actTVInsertLinkedNNodeExecute(Sender: TObject);
    procedure actTVNavigateNextLinkedNNodeExecute(Sender: TObject);
    procedure actTVUnlinkVirtualNodeExecute(Sender: TObject);
    procedure actTVCutSubtreeExecute(Sender: TObject);
    procedure actTVCopySubtreeExecute(Sender: TObject);
    procedure actTVPasteSubtreeExecute(Sender: TObject);
    procedure actTVPasteSubtreeLinkedExecute(Sender: TObject);
    procedure actTVExportExecute(Sender: TObject);
    procedure actTVSortSubtreeExecute(Sender: TObject);
    procedure actTVSortTreeExecute(Sender: TObject);
    procedure actTVRenameNodeExecute(Sender: TObject);

    // Antes privadas...
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure DisplayAppHint( sender: TObject );
    procedure ShowException( Sender : TObject; E : Exception );
    procedure TB_CopyFormatClick(Sender: TObject);
    procedure CB_ResFind_AllNotesClick(Sender: TObject);
    procedure Btn_ResFind_PrevClick(Sender: TObject);
    procedure Btn_ResFind_NextClick(Sender: TObject);
    procedure FindAllResultsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure Combo_ZoomExit(Sender: TObject);
    procedure Combo_FontSizeExit(Sender: TObject);
    procedure MMAlternativeMarginsClick(Sender: TObject);
    procedure MMShowImagesClick(Sender: TObject);
    procedure TB_ImagesClick(Sender: TObject);
    procedure RTFMRestoreProportionsClick(Sender: TObject);
    procedure MMShowAlarmsClick(Sender: TObject);
    procedure MMAlarmsPopupClick(Sender: TObject);
    procedure MMHelpChkUpdClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure MMTreeFocusToogleClick(Sender: TObject);
    procedure MMTreeFocusEditorClick(Sender: TObject);
    procedure MMTreeFocusTreeClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MMFindAllClick(Sender: TObject);
    procedure FormStorageRestorePlacement(Sender: TObject);

    procedure RxChangedSelection(Sender: TKntRichEdit; ConsiderAllOnPlainText: boolean = false);
    procedure RxRTFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RxResTabRTFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RxRTFKeyPress(Sender: TObject; var Key: Char);
    procedure RxResTabRTFKeyPress(Sender: TObject; var Key: Char);
    procedure actTVEraseTreeMemExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure MMFileSaveAsClick(Sender: TObject);
    procedure actList_FileUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actList_TVsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actTVAddNode_ParentExecute(Sender: TObject);
    procedure MMViewHideCheckedNodesClick(Sender: TObject);
    procedure RTFMPlainTextClick(Sender: TObject);
    procedure actTVFlaggedNodeExecute(Sender: TObject);
    procedure actTVViewAdditColumnsExecute(Sender: TObject);
    procedure actTVFilterOutUnflaggedExecute(Sender: TObject);
    procedure chk_LastModifUntilClick(Sender: TObject);
    procedure chk_LastModifFromClick(Sender: TObject);
    procedure chk_CreatedFromClick(Sender: TObject);
    procedure chk_CreatedUntilClick(Sender: TObject);
    procedure RG_ResFind_ScopeClick(Sender: TObject);
    procedure CB_ResFind_PathInNamesClick(Sender: TObject);
    procedure TVFilterUsePathClick(Sender: TObject);
    procedure TVFilterShowChildrenClick(Sender: TObject);
    procedure MMToolsDeduceDatesClick(Sender: TObject);
    procedure MMToolsRemoveDatesClick(Sender: TObject);
    procedure MMViewEditorInfoPanelClick(Sender: TObject);
    procedure RTFM_RTLClick(Sender: TObject);
    procedure MMFilePrintClick(Sender: TObject);
    procedure RTFMFoldClick(Sender: TObject);
    procedure RTFMUnfoldClick(Sender: TObject);
    procedure TagsMCreateClick(Sender: TObject);
    procedure TagsMEditClick(Sender: TObject);
    procedure TagsMDelClick(Sender: TObject);
    procedure RTFMTagsClick(Sender: TObject);
    procedure TagsMAddClick(Sender: TObject);
    procedure TagsMRemoveClick(Sender: TObject);
    procedure chkFilterOnTagsClick(Sender: TObject);
    procedure chkInhTagsClick(Sender: TObject);
    procedure cbTagFilterModeChange(Sender: TObject);
    procedure TVFilterInhTagsClick(Sender: TObject);
    procedure txtTagsInclEnter(Sender: TObject);
    procedure txtTagsExclEnter(Sender: TObject);
    procedure chkInhTagsFindClick(Sender: TObject);
    procedure chkTagsMetadClick(Sender: TObject);
    procedure chkTagsTextClick(Sender: TObject);
    procedure cbTagFindModeChange(Sender: TObject);
    procedure TagsMExportClick(Sender: TObject);
    procedure TagsMImportClick(Sender: TObject);
//    procedure PagesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);


  private
    FRestoreFocusInEditor: integer;              // See *1 in ApplicationEventsIdle and AppRestore
    FRTLShortcutToExecute: TRTLCommandToExecute;
    FRTLShortcutDetectedAt: TDateTime;

    fChangingInCode: boolean;

    { Private declarations }
    procedure AppMessage( var Msg : TMsg; var Handled : boolean );
    procedure WMActivate( Var msg: TWMActivate ); message WM_ACTIVATE;
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure WMHotkey( Var msg: TWMHotkey ); message WM_HOTKEY; // for Activation Hotkey
    procedure WMQueryEndSession (var Msg : TMessage); message WM_QUERYENDSESSION;
    procedure WndProc( var M : TMessage ); override;
    procedure AppDeactivate( sender : TObject );
    procedure WMChangeCBChain( var Msg : TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard( var Msg : TWMDrawClipboard); message WM_DRAWCLIPBOARD; // for Clipboard capture
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMCopyData(Var msg: TWMCopyData); message WM_COPYDATA; // interprocess comm.
    procedure WMJumpToKNTLink( var Msg : TMessage ); message WM_JUMPTOKNTLINK; // custom
    procedure WMJumpToLocation( var DummyMSG : integer ); message WM_JUMPTOLOCATION; //custom
    procedure WMShowTipOfTheDay( var DummyMSG : integer ); message WM_TIPOFTHEDAY; //custom
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    procedure FormatCopyClick(AlloMultiMode: Boolean);
    procedure ShowFindAllOptions;
    procedure ExecuteCallArgs (fromKntLauncher: boolean);

  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure SetupTagsTab;
    procedure TVTags_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure TVTags_GetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
    procedure TVTags_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure TVTags_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TVTags_Change(Sender: TBaseVirtualTree; Node: PVirtualNode);

    procedure RenameTag;
    function CheckRenameTagInNotes(const OldName, NewName: string): boolean;
    procedure TVTags_CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure TVTags_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure TVTags_Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TVTags_NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure TVTagsSelectAlone(Node: PVirtualNode);
    procedure txtFilterTagsChange(Sender: TObject);
    procedure FilterNotesOnTagsSelection;

    procedure CheckFindAllEnabled;
    procedure CheckTxtTagsEnabled;

  public
    procedure CheckFilterTags;

  public
    Res_RTF: TKntRichEdit;
    ShortcutAltDownMenuItem: TMenuItem;

    PrintDlg: TPrintDialog;
    PageSetupDlg : TPageSetupDialog;

    FindTagsIncl: TFindTags;
    FindTagsExcl: TFindTags;
    FindTagsIncl_NotRegistered: string;
    FindTagsExcl_NotRegistered: string;

    procedure UpdateOpenFile;
    procedure UpdateFileModifiedState;
    procedure UpdateFolderDisplay (OnlyActionsState: boolean = false);
    procedure UpdateWordWrap;
    procedure EnableActionsForEditor(SupportsRTF: boolean); overload;
    procedure EnableActionsForEditor(VinculatedToNote, SupportsImages, SupportsRegImages: boolean); overload;
    procedure EnableActionsForTree(TreeUI: TKntTreeUI; ReadOnly: boolean= false);
    procedure ShowNodeChromeState(TreeUI: TKntTreeUI);
    procedure EnsureCalendarSupported;

(*
    {$IFDEF WITH_TIMER}
    procedure StoreTick( const Msg : string; const Tick : integer );
    procedure SaveTicks;
    {$ENDIF}
*)

    procedure AssignOnMessage;

    procedure ActivatePreviousInstance; // sends message to already-running instance and shuts down this instance
    procedure CloseNonModalDialogs; // close all non-modal dialogs that might be open

    // perform commands requested by messages sent from other instances, plugins, etc.
    procedure ProcessControlMessage( const msgID : integer; kntmsg : TKeyNoteMsg  );

    // misc file-related functions
    procedure OnFileDropped( Sender : TObject; FileList : TStringList );

    // sanity checks. Many functions cannot be performed
    // if we have no active note or if the note is read-only
    function HaveKntFolders( const Warn, CheckCount : boolean ) : boolean;
    function FolderIsReadOnly( const aFolder : TKntFolder; const Warn : boolean ) : boolean;

    // search / replace
    procedure FindNotify( const Active : boolean );
    procedure FindTreeNode;

    // status bar etc. display updates
    procedure ShowInsMode;
    procedure ShowImages(Show: boolean; ForceMode: boolean);

    // config file management
    procedure SetupUIHints;
    procedure ResolveToolbarRTFv3Dependencies;

    // VCL updates when config loaded or changed
    procedure UpdateStatusBarState;

    procedure UpdateTreeVisible( const AFolder : TKntFolder );
    procedure CheckRestoreAppWindowWidth (EnsureTreeVisible: boolean= False);

    procedure UpdateTabAndTreeIconsShow;
    procedure UpdateShowImagesState;
    procedure AutoSaveToggled;
    //procedure GetKeyStates;                   // {Not used}
    procedure OnNoteLoaded( sender : TObject );

    // misc stuff
    procedure ShowAbout;
    procedure NewVersionInformation;
    procedure DisplayHistoryFile;

    procedure AnotherInstance;
    procedure ShiftTab( const ShiftRight : boolean );

    procedure HotKeyProc( const TurnOn : boolean );

    {$IFDEF WITH_IE}
    function SelectVisibleControlForNote( const aNote : TKntNote ) : TNodeControl;
    {$ENDIF}

    procedure UpdateAlarmStatus;
    procedure SetAlarm (ConsiderNoteAlarm: Boolean);

    procedure TagsMAddOrRemove(Add: boolean);
    function GetTagsSelected: TNoteTagArray;
    procedure CheckFilterOnTags(RecoveringTagsSituation: boolean);
    procedure RefreshFilterOnTags;

    procedure OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
    procedure ChangeFindInclToModeOR;

  end;

function GetFilePassphrase( const FN : string ) : string;



implementation
uses
   Parser,
   gf_misc,
   gf_miscvcl,
   gf_Lang,
   kn_Global,
   kn_Pass,
   kn_Macro,
   kn_Plugins,
   kn_filemgr,
   kn_About,
   kn_DateTime,
   kn_Chest,
   kn_clipUtils,
   kn_LocationObj,
   kn_History,
   kn_EditorUtils,
   kn_LinksMng,
   kn_Cmd,
   kn_Const,
   kn_StyleObj,
   kn_ImageForm,
   kn_ImagesUtils,
   kn_UpdateVersion,
   kn_ExportNew,
   kn_MacroMng,
   kn_PluginsMng,
   kn_TemplateMng,
   kn_FindReplaceMng,
   kn_ConfigMng,
   kn_StyleMng,
   kn_FavoritesMng,
   kn_BookmarksMng,
   kn_NoteFileMng,
   kn_AlertMng,
   kn_KntFile,
   kn_VCLControlsMng,
   knt.ui.tagSelector,
   knt.ui.TagMng,
   knt.App,
   knt.RS
   {$IFNDEF EXCLUDEEMAIL}
   ,kn_SendMail,
   {$ENDIF}
 {$IFDEF KNT_DEBUG}
   ,GFLog
 {$ENDIF}
   ;

{$R *.DFM}
{$R .\resources\resources.RES}


const
  _TIMER_INTERVAL = 2000; // 2 seconds


// callback from TKntFile, to prompt for passphrase
// when file is encrypted
function GetFilePassphrase( const FN : string ) : string;
var
  PassForm : TForm_Password;
begin
  result := '';
  PassForm := TForm_Password.Create( Application );
  try
    PassForm.myFileName := FN;
    if ( PassForm.ShowModal = mrOK ) then
      result := PassForm.Edit_Pass.Text;
  finally
    PassForm.Free;
  end;
end; // GetFilePassphrase


procedure TForm_Main.HotKeyProc( const TurnOn : boolean );
var
  HKeyCode : Word;
  HShiftState : TShiftState;
begin
  try

    if TurnOn then
    begin
      // register activation hotkey
      if ( KeyOptions.HotKeyActivate and ( KeyOptions.HotKey > 0 )) then
      begin
        ShortCutToKey( KeyOptions.HotKey, HKeyCode, HShiftState );
        if RegisterHotkey( Handle, 1,
          ShiftStateToHotKey( HShiftState ), HKeyCode
        ) then
        begin
          App.Kbd.HotKeySuccess := true;
        end
        else
        begin
          App.Kbd.HotKeySuccess := false;
          if ( KeyOptions.HotKeyWarn and KeyOptions.SingleInstance ) then
          begin
            // No warning if we can be running more than 1 instance of KeyNote,
            // because only the first instance will be able to register the hotkey,
            // so we would ALWAYS be getting the damn warning on each subsequent instance.
            Messagedlg( Format(
              GetRS(sMain01),
              [ShortCutToText( KeyOptions.HotKey )] ),
              mtWarning, [mbOK], 0 );
          end;
        end;
      end;
    end
    else
    begin
      if App.Kbd.HotKeySuccess then
        UnRegisterHotkey( Handle, 1 );
      App.Kbd.HotKeySuccess := false;
    end;

  except
    On E : Exception do
    begin
      App.Kbd.HotKeySuccess := false;
      messagedlg( Format(
        GetRS(sMain02),
        [TOGGLEARRAY[TurnOn], ShortCutToText( KeyOptions.HotKey ), E.Message]
        ), mtError, [mbOK], 0 );

    end;
  end;

  if App.Kbd.HotKeySuccess then
    TMRestore.Caption := Format(
      GetRS(sMain03), [ShortCutToText( KeyOptions.HotKey )]
    )
  else
    TMRestore.Caption := GetRS(sMain04);

end; // HotKeyProc



procedure TForm_Main.FormCreate(Sender: TObject);
begin

  IMG_Toolbar.Clear;
  IMG_Format.Clear;
  IMG_TV.Clear;
  LoadGifFromResource(IMG_Toolbar, 'TOOLBAR_MAIN');   //,  clFuchsia);
  LoadGifFromResource(IMG_Format, 'TOOLBAR_FORMAT');  //    ,,
  LoadGifFromResource(IMG_TV, 'TV_IMAGES');           //    ,,
  LoadGifFromResource(CheckImages, 'VTCHECKIMGS');

  { The name associated with the secondary icon as a resource must be after (alphabetically) the main one
    (MAINICON) or else the secondary one will appear first in the resource file, and will be the one displayed
    from the system (for example from Explorer).
    OTHER_KNT_ICON => KNT_ALT_ICON ... (keynoteAlt.ico)
   }
  TrayIcon.Icons.LoadResource(HInstance, ['MAINICON', 'OTHER_KNT_ICON']);
  App.ApplyBiDiMode;

  InitializeKeynote(Self);
  MMP_PlainDefaultPaste.Hint:= MMEditPlainDefaultPaste.Hint;

  RegisterDropTarget(Form_Main.Pages);

  fChangingInCode:= False;
  FRestoreFocusInEditor:= 0;
  FRTLShortcutDetectedAt:= 0;
  FRTLShortcutToExecute:= rtNone;
  Application.OnIdle := ApplicationEventsIdle;
  ShortcutAltDownMenuItem:= nil;

  PrintDlg:= TPrintDialog.Create(Self);
  PageSetupDlg := TPageSetupDialog.Create(Self);
  PrintDlg.Options:= [poPrintToFile];
end;
// CREATE

procedure TForm_Main.EnsureCalendarSupported;
var
   MadeVisible: boolean;
begin
  if CB_CreatedFrom.Visible then exit;

  if not CheckCalendarInTDateTimePicker(Self) then begin
     MadeVisible:= PrepareTDateTimePicker(CB_CreatedFrom);
     PrepareTDateTimePicker(CB_CreatedUntil);
     PrepareTDateTimePicker(CB_LastModifFrom);
     PrepareTDateTimePicker(CB_LastModifUntil);
     if not MadeVisible then
       lblCalNotSup.Visible:= True;
  end
  else begin
     try
       CB_CreatedFrom.Visible := true;
       CB_CreatedUntil.Visible := true;
       CB_LastModifFrom.Visible := true;
       CB_LastModifUntil.Visible := true;
     except
       lblCalNotSup.Visible:= True;
     end;
  end;
end;

procedure TForm_Main.FormShow(Sender: TObject);
begin
   EnsureCalendarSupported;
end;

procedure TForm_Main.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;

  Log_StoreTick( 'FormActivate - Begin', 1, +1 );
  OnActivate := nil; // don't return here again

  try
    HideOrShowResPanel( KeyOptions.ResPanelShow );
    UpdateResPanelContents (true);
    Splitter_ResMoved( Splitter_Res );
    // Pages_Res.Visible := KeyOptions.ResPanelShow;
    Btn_ResFind.Enabled := ( Combo_ResFind.Text <> '' );
  except
  end;
  Log_StoreTick( 'After update respanel', 2 );

  EnableOrDisableUAS;

  if KeyOptions.AltMargins then
     MMAlternativeMargins.Checked:= true;

  EnsureNodeAndCaretVisibleInFolders;       // View comment in KntFileOpen

  if _GLOBAL_URLText <> '' then
    JumpToKNTLocation( _GLOBAL_URLText );

  if App.opt_Title <> '' then begin
     Caption:= App.opt_Title;
     App.opt_Title:= '';
  end;

  App.opt_Minimize := ( App.opt_Minimize or KeyOptions.StartMinimized ) and (_GLOBAL_URLText = '');

  Application.ProcessMessages;

  { Before this method is executed, on Form.DoShow: (FormStorage) => TFormPlacement.RestoreFormPlacement -> RestorePlacement -> RxAppUtils.InternalReadFormPlacement => PostMessage(...)
    Although at this point WindowState <> wsMinimized, it will end up being, if we do not queue a subsequent request for it to be restored }

  if (not App.opt_Minimize) then
     Application.Restore;

  {
  if KeyOptions.TipOfTheDay then
    ShowTipOfTheDay;
  }
  TVFilterUsePath.Checked:= FindOptions.SearchPathInNodeNames;
  CB_ResFind_PathInNames.Checked:= FindOptions.SearchPathInNodeNames;
  TVFilterShowChildren.Checked:= FindOptions.ShowChildren;
  TVFilterInhTags.Checked:= FindOptions.InheritedTags;
  TVFilterInhTags.Hint:= chkInhTags.Hint;
  chkInhTags.Checked:= FindOptions.InheritedTags;
  chkInhTagsFind.Checked:= FindOptions.InheritedTags;
  chkInhTagsFind.Hint:= chkInhTags.Hint;
  cbTagFindMode.ItemIndex:= 0;
  cbTagFilterMode.ItemIndex:= 0;

  Initializing := false;

  App.ActivateFolder(nil);            // Activate (and focus) current active folder       View comment in KntFileOpen

  Log_StoreTick( 'After ActiveKntFolder', 2 );

  if ( KeyOptions.RunAutoMacros and ( StartupMacroFile <> '' )) then
  begin
    if ( pos( '\', StartupMacroFile ) = 0 ) then
      StartupMacroFile := Macro_Folder + StartupMacroFile;
    if FileExists( StartupMacroFile ) then
    begin
      Application.ProcessMessages;
      ExecuteMacro( StartupMacroFile, '' );
    end;
  end;

  Log_StoreTick( 'After automacro', 2 );

  if ( StartupPluginFile <> '' ) then
  begin
    if ( pos( '\', StartupPluginFile ) = 0 ) then
      StartupPluginFile := Plugin_Folder + StartupPluginFile;
    if FileExists( StartupPluginFile ) then
    begin
      Application.ProcessMessages;
      ExecutePlugin( StartupPluginFile );
    end;
  end;

  Log_StoreTick( 'After autoplugin', 2 );

  if KeyOptions.TipOfTheDay and not FirstTimeRun then
    postmessage( Handle, WM_TIPOFTHEDAY, 0, 0 );


  MMViewOnTop.Checked := KeyOptions.AlwaysOnTop;
  TB_OnTop.Down := MMViewOnTop.Checked;

  if App.opt_Minimize then
    Application.Minimize
  else
    WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;

  Combo_Font.OnClick  := Combo_FontChange;
  Combo_Font.OnChange := nil;
  Combo_FontSize.OnClick := Combo_FontSizeClick;
  Combo_Zoom.OnClick := Combo_FontSizeClick;

  SetupTagsTab;

  // Timer.Enabled := true;
  Application.OnDeactivate := AppDeactivate;
  // FolderMon.onChange := FolderMonChange;

  Log_StoreTick( 'FormActivate - End', 1, -1 );

end; // ACTIVATE


procedure TForm_Main.FormStorageRestorePlacement(Sender: TObject);
begin
   UpdateResPanelContents (false);
end;



procedure TForm_Main.ActivatePreviousInstance;
var
  CopyData : TCopyDataStruct;
  //msg : TKeyNoteMsg;
  Args: string;
begin
{
  if ( KntFileToLoad <> '' ) then
  begin
    msg.strData := KntFileToLoad;
    copydata.dwData := KNT_MSG_SHOW_AND_LOAD_FILE;
  end
  else
  if ( CmdLineFileName <> '' ) then
  begin
    msg.strData := CmdLineFileName;
    copydata.dwData := KNT_MSG_SHOW_AND_EXECUTE_FILE;
  end
  else
  begin
    msg.strData := NO_FILENAME_TO_LOAD;
    copydata.dwData := KNT_MSG_SHOW;
  end;

  copydata.cbData := sizeof( msg );
  copydata.lpData := @msg;
}

  // The other instance will process the main command line arguments: .KNT, .KNE, .KNL, or .KNM files, as well as the -jmp switch
  // It is more general than the use of KNT_MSG_SHOW_AND_LOAD_FILE, KNT_MSG_SHOW_AND_EXECUTE_FILE, NO_FILENAME_TO_LOAD
  Args:= GetCommandLine;
  copydata.dwData := KNT_MSG_OTHER_INSTANCE_CALL;
  copydata.cbData:= ByteLength(Args)+1;
  copydata.lpData := PChar(Args);

  SendMessage( _OTHER_INSTANCE_HANDLE,
    WM_COPYDATA,
    Handle,
    integer( @copydata ));

  //ShowWindow(_OTHER_INSTANCE_HANDLE, SW_RESTORE);  // See comment to ExecuteCallArgs
  sleep(100);
  SetForegroundWindow(_OTHER_INSTANCE_HANDLE);

end; // ActivatePreviousInstance


procedure TForm_Main.ProcessControlMessage(
  const msgID : integer;
  kntmsg : TKeyNoteMsg
  );
var
  Editor: TKntRichEdit;
begin
  Editor:= ActiveEditor;

  case MsgID of
    KNT_MSG_PERFORMKEY : if App.CheckActiveEditorNotReadOnly then
    begin
      ShortCutToKey( kntmsg.intData1, App.Kbd.LastRTFKey.Key, App.Kbd.LastRTFKey.Shift );
      PostKeyEx( Editor.Handle, App.Kbd.LastRTFKey.Key, App.Kbd.LastRTFKey.Shift, App.Kbd.LastRTFKey.Special );
      Application.ProcessMessages;
    end;
    KNT_MSG_INSERTTEXT : if App.CheckActiveEditorNotReadOnly then
    begin
      Editor.SelLength := 0;
      Text := kntmsg.strData;
      Editor.SelLength := 0;
    end;
    KNT_MSG_MOVECARET : if App.CheckActiveEditor then
    begin
      case kntmsg.intData1 of
        _CARET_RIGHT : with Editor do
        begin
          Perform( WM_KEYDOWN, VK_RIGHT, 0 );
          Perform( WM_KEYUP, VK_RIGHT, 0 );
        end;
        _CARET_LEFT : with Editor do
        begin
          Perform( WM_KEYDOWN, VK_LEFT, 0 );
          Perform( WM_KEYUP, VK_LEFT, 0 );
        end;
        _CARET_UP : with Editor do
        begin
          Perform( WM_KEYDOWN, VK_UP, 0 );
          Perform( WM_KEYUP, VK_UP, 0 );
        end;
        _CARET_DOWN : with Editor do
        begin
          Perform( WM_KEYDOWN, VK_DOWN, 0 );
          Perform( WM_KEYUP, VK_DOWN, 0 );
        end;
      end;
    end;
    KNT_MSG_RELOADCONFIG : begin
      case kntmsg.intData1 of
        _CFG_RELOAD_MAIN : begin
          // [x] not implemented
        end;
        _CFG_RELOAD_KEYS : begin
          // ReadFuncKeys;
          // StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain05;
        end;
      end;
    end;
  end;
end; // ProcessControlMessage


procedure TForm_Main.WMCopyData (Var msg: TWMCopyData);
var
  kntmsg : TKeyNoteMsg;
  TypeMsg: NativeUInt;
  Args: string;
begin
  TypeMsg:= msg.Copydatastruct^.dwData;
  if TypeMsg <> KNT_MSG_LAUNCHER_CALL then
     ReplyMessage( 0 );

  case TypeMsg of
    KNT_MSG_LAUNCHER_CALL, KNT_MSG_OTHER_INSTANCE_CALL: begin
      //PopupMessage( Format('KNT_MSG_LAUNCHER/OTHER_INSTANCE_CALL: %d', [TypeMsg]), mtConfirmation, [mbYes], 0 );
      Args:= PChar(msg.Copydatastruct^.lpData);
      if CheckCallArgs(Args, (TypeMsg = KNT_MSG_LAUNCHER_CALL)) then begin
         ReplyMessage( 0 );
         ExecuteCallArgs((TypeMsg = KNT_MSG_LAUNCHER_CALL));
      end
      else
         ReplyMessage( -1 );
      exit;
    end
    else begin
      kntmsg := PKeyNoteMsg( msg.Copydatastruct^.lpData )^;
      ProcessControlMessage( msg.Copydatastruct^.dwData, kntmsg );
      exit;
    end;
  end;
end; // WMCopyData


procedure TForm_Main.ExecuteCallArgs (fromKntLauncher: boolean);
var
  Open: boolean;
begin
{
  It does not work correctly. It is best that the calling application takes care of this, giving it its focus (-> ActivatePreviousInstance)
  However, restore the window by running ShowWindow(_OTHER_INSTANCE_HANDLE, SW_RESTORE); from the calling instance it is problematic
  he window is restored but not minimized until the other one is closed (or after multiple minimizes/restorations of that one)
}
   Application.Restore;
// Application.BringToFront;


   if not fromKntLauncher and ( KntFileToLoad <> '' ) then begin
      //PopupMessage( Format('KntFileToLoad: %s', [KntFileToLoad]), mtConfirmation, [mbYes] );
      Open:= true;
      if HaveKntFolders( false, false ) then begin
         if ( KntFileToLoad.ToUpper = ActiveFile.FileName.ToUpper ) then begin
           if ( App.PopupMessage( Format(GetRS(sMain06), [ActiveFile.Filename]), mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then
              Open:= false;
           ActiveFile.Modified := false; // to prevent automatic save if modified
         end;
      end;
      if Open then
         KntFileOpen( KntFileToLoad );
   end;


   if _GLOBAL_URLText <> '' then begin
     //PopupMessage( Format('JUMP: %s', [_GLOBAL_URLText]), mtConfirmation, [mbYes] );
     JumpToKNTLocation( _GLOBAL_URLText, urlOpen, true);      // OpenInCurrentFile
   end;


   if StartupMacroFile <> '' then
      ExecuteMacro( StartupMacroFile, '' );

   if StartupPluginFile <> '' then
      ExecutePlugin( StartupPluginFile );
end;


procedure TForm_Main.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if App.opt_DoNotDisturb then
     Params.WinClassName := UniqueAppName_KEYNOTE10_dnd
  else
     Params.WinClassName := UniqueAppName_KEYNOTE10;
end; // CreateParams


procedure TForm_Main.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i : integer;
begin
  if IsRunningMacro then
  begin
    MacroAbortRequest := true;
  end
  else
  if IsRecordingMacro then
  begin
    TB_MacroRecordClick( TB_MacroRecord ); // stop recording
  end;

  CloseNonModalDialogs;

  if ( not ( ClosedByWindows or TerminateClick )) then
  begin
    if KeyOptions.MinimizeOnClose then
    begin
      CanClose := false;
      Application.Minimize;
      AppIsClosing := false;
      exit;
    end;
  end;

  if CanClose then
    if ( KeyOptions.ConfirmExit and ( not ClosedByWindows )) then
      CanClose := ( App.PopupMessage( format(GetRS(sMain07), [Program_Name]), mtConfirmation, [mbYes,mbNo] ) = mrYes );
  { note: if windows is closing, we do not honor the "Confirm exit" config option }

  try
    if CanClose then
       CanClose := CheckModified( not KeyOptions.AutoSave, true );

    if CanClose then
    begin

      try

        FindOptions.FindAllHistory := '';
        for i := 1 to Combo_ResFind.Items.Count do
        begin
          if ( i > FindOptions.HistoryMaxCnt ) then break;
          if ( i > 1 ) then
            FindOptions.FindAllHistory := FindOptions.FindAllHistory + HISTORY_SEPARATOR + AnsiQuotedStr( Combo_ResFind.Items[pred( i )], '"' )
          else
            FindOptions.FindAllHistory := AnsiQuotedStr( Combo_ResFind.Items[0], '"' );
        end;

        if KeyOptions.UseCtrlHideTreePanel then begin
           Visible:= False;
           CheckRestoreAppWindowWidth;
        end;

        SaveOptions;
        if Res_RTF.Modified then
          StoreResScratchFile;

        { SaveFuncKeys is not called, because key assignments
        do not change inside KeyNote }
        // SaveFuncKeys;

        if ( not App.opt_NoSaveOpt ) then
        begin
          SaveFileManagerInfo( MGR_FN );
          CheckSaveStyleManagerInfo;
          SaveFavorites( FAV_FN );
          if App.opt_NoRegistry then
            IniSaveToolbarPositions( Self, MRU_FN, 'TB97a' )
          else
            RegSaveToolbarPositions( Self, 'Software\General Frenetics\Keynote\FormPos\TB97a' );
        end;

      except
      end;
    end;

  finally
    if CanClose then
    begin
     {$IFDEF KNT_DEBUG}
       Log.Flush( true );
       Log.Add( 'CloseQuery result: ' + BOOLARRAY[CanClose] );
     {$ENDIF}
       AppIsClosing := true;
    end;

    ClosedByWindows := false;
    TerminateClick := false;
  end;
end; // CLOSEQUERY

procedure TForm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   HotKeyProc( false ); // unregister hotkey
   App.FileClosed(ActiveFile);

   UnRegisterDropTarget(Form_Main.Pages);

end; // FORM CLOSE

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
  // close shop!!
  ClipCapMng.Free;

  FolderMon.Active := false;
{$IFNDEF KNT_DEBUG}
  // in final release, set the event to NIL (keep only for debugging)
  Application.OnException := nil;
{$ENDIF}
  Application.OnDeactivate := nil;
  Application.OnHint := nil;
  // ClipCapNote := nil;
  Pages.OnChange := nil;

  // [x] The access violation error which happens
  // on closing app if a virtual node was clicked
  // occurs in MRU.RemoveAllItems, because the
  // popup menu has aparently been freed but not nil'ed.
  // So, we just cause MRU to forget it ever had a popup
  // menu, so that it won't try to remove its items.
  TB_FileOpen.DropdownMenu := nil;
  MRU.PopupMenu := nil;

  // TB97CALLCOUNT := 0;

  try
    try
      PrintDlg.Free;
      PageSetupDlg.Free;
	
      DestroyVCLControls;
      if assigned( ActiveFile ) then ActiveFile.Free;

    except
      // at this stage, we can only swallow all exceptions (and pride)
     {$IFDEF KNT_DEBUG}
        on E : Exception do begin
          showmessage( 'Exception in OnDestroy: ' + #13#13 +E.Message );
          if assigned( Log ) then Log.Add( 'Exception in OnDestroy: ' + E.Message );
        end;
     {$ENDIF}
    end;
  finally
    {$IFDEF KNT_DEBUG}
      if assigned( Log ) then
         Log.Free;
      log := nil;
    {$ENDIF}
  end;

end;
// DESTROY



procedure TForm_Main.ResolveToolbarRTFv3Dependencies;
begin
  // this must always be done after "LoadToolbars".
  // Disable buttons and menu items if RichEdit version < 3
  // but also check if they were not previously hidden
  // via toolbar configuration
  TB_Numbers.Visible := TB_Numbers.Visible and ( _LoadedRichEditVersion > 2 );
  TB_AlignJustify.Visible := TB_AlignJustify.Visible and ( _LoadedRichEditVersion > 2 );
  MMFormatNumbers.Visible := ( _LoadedRichEditVersion > 2 );
  MMFormatNumbers.Enabled := ( _LoadedRichEditVersion > 2 );
  MMFormatAlignJustify.Visible := ( _LoadedRichEditVersion > 2 );
  MMFormatAlignJustify.Enabled := ( _LoadedRichEditVersion > 2 );
  Combo_Zoom.Visible := Combo_Zoom.Visible and ( _LoadedRichEditVersion > 2 );
  Combo_Zoom.Enabled := ( _LoadedRichEditVersion > 2 );
  MMViewZoomIn.Visible := ( _LoadedRichEditVersion > 2 );
  MMViewZoomOut.Visible := ( _LoadedRichEditVersion > 2 );
end; // ResolveToolbarRTFv3Dependencies


procedure TForm_Main.SetupUIHints;
begin
  TB_AlignCenter.Hint := MMFormatAlignCenter.Hint;
  TB_AlignLeft.Hint := MMFormatAlignLeft.Hint;
  TB_AlignRight.Hint := MMFormatAlignRight.Hint;
  TB_AlignJustify.Hint := MMFormatAlignJustify.Hint;
  TB_Bold.Hint := MMFormatBold.Hint;
  TB_Bullets.Hint := MMFormatBullets.Hint;
  TB_ClipCap.Hint := MMNoteClipCapture.Hint;
  TB_EditCopy.Hint := MMEditCopy.Hint;
  TB_EditCut.Hint := MMEditCut.Hint;
  TB_EditPaste.Hint := MMEditPaste.Hint;
  TB_EditRedo.Hint := MMEditRedo.Hint;
  TB_EditUndo.Hint := MMEditUndo.Hint;

{$IFNDEF EXCLUDEEMAIL}
  TB_EmailNote.Hint := MMNoteEmail.Hint;
{$ELSE}
  MMNoteEmail.Visible:= False;
  TB_EmailNote.Visible:= False;
{$ENDIF}
  //TB_Exit.Hint := MMFileExit.Hint;
  TB_FileInfo.Hint := MMFileproperties.Hint;
  TB_FileMgr.Hint := MMFileManager.Hint;
  TB_FileNew.Hint := MMFileNew.Hint;
  TB_FileOpen.Hint := MMFileOpen.Hint;
  TB_FileSave.Hint := MMFileSave.Hint;
  TB_Find.Hint := MMFind.Hint;
  TB_FindNext.Hint := MMFindNext.Hint;
  TB_FontDlg.Hint := MMFormatFont.Hint;
  TB_GoBack.Hint := MMHistoryGoBack.Hint;
  TB_GoForward.Hint := MMHistoryGoForward.Hint;
  TB_Indent.Hint := MMFormatLIndInc.Hint;
  TB_Italics.Hint := MMFormatItalics.Hint;
  TB_Macro.Hint := MMToolsMacroRun.Hint;
  // TB_MacroPause.Hint := .Hint;
  // TB_MacroRecord.Hint := .Hint;
  TB_NoteDelete.Hint := MMFolderRemove.Hint;
  TB_NoteEdit.Hint := MMNoteProperties.Hint;
  TB_NoteNew.Hint := MMFolderNew.Hint;
  TB_Numbers.Hint := MMFormatNumbers.Hint;
  TB_OnTop.Hint := MMViewOnTop.Hint;
  TB_Options.Hint := MMToolsOptions.Hint;
  TB_Outdent.Hint := MMFormatLIndDec.Hint;
  TB_ParaDlg.Hint := MMFormatParagraph.Hint;
  // TB_PluginConfigure.Hint := .Hint;
  TB_PluginRun.Hint := MMToolsPluginRun.Hint;
  TB_Print.Hint := MMNotePrint.Hint;
  TB_Repeat.Hint := MMEditRepeat.Hint;
  TB_Replace.Hint := MMFindReplace.Hint;
  TB_ResPanel.Hint := MMViewResPanel.Hint;
  TB_Space1.Hint := MMFormatLS1.Hint;
  TB_Space15.Hint := MMFormatLS15.Hint;
  TB_Space2.Hint := MMFormatLS2.Hint;
  TB_Spell.Hint := MMNoteSpell.Hint;
  TB_Strikeout.Hint := MMFormatStrikeout.Hint;
  TB_Style.Hint := MMFormatApplyStyle.Hint;
  TB_Subscript.Hint := MMFormatSubScript.Hint;
  TB_Superscript.Hint := MMFormatSuperScript.Hint;
  TB_Underline.Hint := MMFormatUnderline.Hint;
  TB_WordWeb.Hint := MMInsertWordWeb.Hint;
  TB_WordWrap.Hint := MMFormatWordWrap.Hint;

  MMToolsDeduceDates.Hint:= GetRS(sMain51) + GetRS(sMain53);
  MMToolsRemoveDates.Hint:= GetRS(sMain52) + GetRS(sMain53);

end; // SetupToolbarButtons



procedure TForm_Main.AppMessage( var Msg : TMsg; var Handled : boolean );
begin
  // keep track of when application was last "active",
  // i.e. the user was doing something in the program
  case Msg.Message of
    WM_KEYFIRST..WM_KEYLAST, WM_LBUTTONDOWN..WM_MOUSELAST : begin
      // Note: we are not using WM_MOUSEFIRST, because that includes
      // WM_MOUSEMOVE, so just freely moving the mouse over the form
      // would trigger this event, which would be counterproductive.
      // IOW, we do not consider mouse movement an "activity"; only
      // actual clicks and keypresses count.
      AppLastActiveTime := Now;
    end;
  end;
end; // AppMessage

procedure TForm_Main.AssignOnMessage;
begin
  Application.OnMessage := AppMessage;
end;


procedure TForm_Main.AppMinimize(Sender: TObject);
begin
  TMRestore.Enabled := true;
  WinOnTop.AlwaysOnTop := false;
  // FormStorage.Options := [fpPosition];
  if KeyOptions.UseTray then
    ShowWindow(Application.Handle, SW_HIDE);
end; // AppMinimize;


{ *1
    I'm seeing a strange behavior of the RichEdit control when the application is restored
    through a Hotkey of the form CTRL+SHIFT+...
    The editor ends up receiving an EN_CHANGE message and the Change method of the control is called.
    From the beginning of that method you can see how the Modified property of the editor is True (^), and
    the position of the scrollbar has also been lost.
    This behavior is perceived as the editor not respecting the position that the scrollbar had,
    but in an apparently random way, since it seems to occur when returning to the application
    from another one, but really (from what I'm observing) it only happens when it is done through that Hotkey.
    With another type of Hotkey combination it doesn't happen (e.g. ALT+F7)
    I understand that this is what is described here (save and restore caret position...not sure what it's
    supposed to do #720 ) and certainly on some occasions I myself have noticed it.

    From what I see, at least in W10, the position problem is in turn linked to the fact
    that the editor perceives the press as a modification (^). In fact, if the file had not been modified yet
    you can see how it changes to display "MOD" (in addition to not respecting the scrollbar position).
    - - -
    I also see that the mere fact of pressing CTRL+SHIFT (without any additional key) when the
    editor has the focus causes the same effect (^). I have been able to easily solve the latter
    from FormShortCut (See line marked with *1 in that method).
    - - -
      (^) At least it appears as modified when I run it from the virtual machine in W10. In Windows 11
         the scrollbar position is altered but it is not being marked as modified. I don't know the reason (*2).
     *2:
        I have already identified the reason. It is because in the virtual machine with W10 I have installed,
        in addition to the Spanish and English languages, the Arabic language, and for this last language,
        pressing Ctrl+Shift causes the text to be managed as RTL (Ctrl-right+Shift-right) or LTR (Ctr-left+Shift-left).
        Therefore, it is not an error that the simple pressing of Ctrl+Shift is treated as a modification.
        But in any case I have to properly control that it is only executed when Ctrl+Shift is pressed individually,
        and not by mistake when trying to press another shortcut that contains Ctrl+Shift. I solve it between the code
        in FormShortCut and new code added to ApplicationEventsIdle

    To solve the problem when pressing the Hotkey:
    In WMHotkey I needed to use SetForegroundWindow(..) instead of Application.BringToFront, and do it
    only when the application does not need to be restored (it is not minimized) nor is it already
    active (that is, doing nothing if the Hotkey is pressed with the application in focus).

    In the case where the application was minimized and had to be restored, and after numerous tests, I needed to
    ensure that the focus was not in the editor so that it did not receive the message associated with that click.
    If the focus is not in the editor, there is no need to do anything, but if it is in the editor, I must
    select another control (I have chosen self.Dock_Top, whose selection does not trigger any special action in
    the application) before executing ShowWindow() and SetForegroundWindow(..), restoring the focus afterwards.
    To restore the editor focus, it is not enough to apply 'pauses' with Sleep() and execute Application.ProcessMessages.
    Although it is possible to make adjustments to make it work apparently well, it easily results in correct behavior
    in W11 and incorrect behavior in W10 (at least from the virtual machine), and vice versa. The only robust solution
    I have found is to use OnIdle, as I have prepared, in two steps (RestoreFocusInEditor = 1 and RestoreFocusInEditor = 2).

    (Before arriving at this solution I have done numerous tests, including clearing keyboard messages in the queue,
    handling WM_SYSCOMMAND or WM_WINDOWPOSCHANGED or WM_ACTIVATE events, using Sleep() and Application.ProcessMessages
    at different points and with different values, etc. In some cases the solution has been valid in W10 and others
    only in W11. It should be noted that the correctness of the behavior could only be verified from outside the IDE
    or inside, but without using breakpoints. Any breakpoint added, even if it did not jump, altered the final behavior)
}


procedure TForm_Main.ApplicationEventsIdle(Sender: TObject; var Done: Boolean);           // *1
begin
  // *2 In the tests it doesn't seem necessary with the Host (W11) but it does with W10 (from virtual machine).
  //    I keep it for safety and because nothing is noticeable, because of its short duration and because at that point
  //    the window is already visible and the editor doesn't show changes (BeginUpdate)

    if (FRestoreFocusInEditor = 2) then begin
       FRestoreFocusInEditor:= 0;
       ActiveEditor.Enabled:= true;
       ActiveControl:= ActiveEditor;
       ActiveEditor.EndUpdate;
    end;

    if (FRestoreFocusInEditor = 1) then begin
       ActiveControl:= nil;
       ActiveEditor.BeginUpdate;
       ActiveEditor.Enabled:= False;
       FRestoreFocusInEditor:= 2;
       Sleep(25);                       // *2
    end;

    if (FRTLShortcutToExecute <> rtNone) then begin          // See comment *1 and *2 in AppRestore
         if (ActiveEditor <> nil) and (not ActiveEditor.ReadOnly) then begin
            if MilliSecondsBetween(FRTLShortcutDetectedAt, now) > 50  then begin
              if (GetKeyState(VK_CONTROL) >= 0) or (GetKeyState(VK_SHIFT) >= 0) then begin
                case FRTLShortcutToExecute of
                   rtRTL: ActiveEditor.Paragraph.RTL:= true;
                   rtLTR: ActiveEditor.Paragraph.RTL:= false;
                end;
                FRTLShortcutToExecute:= rtNone;
              end;
            end;
         end
         else
            FRTLShortcutToExecute:= rtNone;
    end;

    if (IntroducingTagsState = itNoTags) and (FRestoreFocusInEditor = 0) and (ActiveControl = ActiveEditor)
       and (GetAsyncKeyState(VK_LEFT) = 0) and (GetAsyncKeyState(VK_RIGHT) = 0)
       and (GetAsyncKeyState(VK_UP) = 0) and (GetAsyncKeyState(VK_DOWN) = 0) then
       ActiveEditor.CheckSelectingRegisteredTag;

  Done := True;
end;


procedure TForm_Main.AppRestore(Sender: TObject);
begin
    TMRestore.Enabled := false;
    AppLastActiveTime := now;
    FormStorage.Options := [fpState,fpPosition];

    FRestoreFocusInEditor:= 0;
    if (ActiveEditor <> nil) and (ActiveControl = ActiveEditor) then begin        // *1
       ActiveControl:= Self.Dock_Top;
       FRestoreFocusInEditor:= 1;
    end;

    ShowWindow( Application.Handle, SW_SHOW );
    WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;
    //Application.BringToFront;                                                  // *1
    SetForegroundWindow(Application.Handle);

    if _REOPEN_AUTOCLOSED_FILE then
    begin
      _REOPEN_AUTOCLOSED_FILE := false;
      KntFileOpen( KeyOptions.LastFile );
    end;
end; // AppRestore;

procedure TForm_Main.DisplayAppHint( sender: TObject );
var
   i: integer;
begin
  if KeyOptions.StatBarShow then begin
     StatusBar.Panels[PANEL_HINT].Text := GetShortHint( Application.Hint );

     if KeyOptions.HintsAccesible then begin   // When hints are shown in status bar, other text panels will be shown blank
        if Application.Hint <> '' then begin
           for i:= 0 to 5 do
              StatusBar.Panels[I].Style:= psOwnerDraw;
        end
        else begin
           for i:= 0 to 5 do
             StatusBar.Panels[I].Style:= psText;
           StatusBar.Panels[PANEL_FILEICON].Style:= psOwnerDraw;
        end;
     end;
  end;

end; // DisplayAppHint


procedure TForm_Main.TrayIconClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  AppLastActiveTime := now;
  Application.Restore;
  Application.BringToFront;
end;

procedure TForm_Main.MMFileExitClick(Sender: TObject);
begin
  TerminateClick := true;
  Close;
end;

procedure TForm_Main.TMRestoreClick(Sender: TObject);
begin
  Application.Restore;
  Application.BringToFront;
end;

procedure TForm_Main.TntFormResize(Sender: TObject);
begin
  Form_Main.Refresh;
  if assigned(ActiveFolder) then
     ActiveFolder.Editor.Invalidate;
end;

procedure TForm_Main.TimerTimer(Sender: TObject);
const
  bools : array[false..true] of string = ( 'No', 'Yes' );
var
  Hrs, Mins : integer;
  _MillisecondsIdle: DWord;
begin
  if HandlingTimerTick then exit;

  HandlingTimerTick:= True;
  try
     inc( Timer_Tick );
     inc( Timer_TickAlarm);
     {
       timer may trigger THREE kinds of things:
       1. auto-saving current file
       2. minimizing keynote and/or closing file after a period of inactivity.

       3. Show Alarms on nodes           [dpv*]
       4. Invoke ImagesManager.CleanUp   [dpv]
     }
     try
        if ( Timer_Tick >= KeyOptions.AutoSaveOnTimerInt * ( 60000 div _TIMER_INTERVAL ) ) then
        begin
          Timer_Tick := 0;
          if Form_Main.HaveKntFolders( false, false ) then begin
            if (( not ActiveFile.ChangedOnDisk ) and ActiveFile.Modified and KeyOptions.AutoSave and KeyOptions.AutoSaveOnTimer ) then begin
              if (( ActiveFile.FileName <> '' ) and ( not ActiveFile.ReadOnly )) then begin
                // only if saved previously
               {$IFDEF KNT_DEBUG}
                 Log.Add( '-- Saving on TIMER' );
               {$ENDIF}
                 KntFileSave( ActiveFile.FileName );
              end;
            end;
          end;
        end;

        if KeyOptions.TimerClose then begin
          Hrs := ( KeyOptions.TimerCloseInt DIV 60 );
          Mins := ( KeyOptions.TimerCloseInt MOD 60 );
          if (( AppLastActiveTime + EncodeTime( Hrs, Mins, 0, 0 )) < Now ) then begin
            Timer_Tick := 0;
            AutoCloseKntFile;
            // auto-closing minimizes too, so we exit here
            exit;
          end;
        end;

        if KeyOptions.TimerMinimize then begin
          if ( not IsIconic( Application.Handle )) then begin
           Hrs := ( KeyOptions.TimerMinimizeInt DIV 60 );
           Mins := ( KeyOptions.TimerMinimizeInt MOD 60 );
           if (( AppLastActiveTime + EncodeTime( Hrs, Mins, 0, 0 )) < Now ) then
             Application.Minimize;
         end;
       end;


       if ( Timer_TickAlarm >=  ( 60000 div _TIMER_INTERVAL )/4 ) then begin    // Comprobamos cada 15 segundos
           Timer_TickAlarm:= 0;
           AlarmMng.checkAlarms;
           ImageMng.CheckFreeImageStreamsNotRecentlyUsed;     // This method will only work if ImageManager.Enabled and if more than X minutes have passed since the previous cleanup
       end;


       _MillisecondsIdle:= MillisecondsIdle;
       if KeyOptions.CheckUpdOnStartup and (_MillisecondsIdle > 3000) and not FirstTimeRun then
          CheckForUpdate(true);

       if EditorOptions.WordCountTrack and (_MillisecondsIdle >= 450) then
          if assigned(ActiveEditor) then
             ActiveEditor.UpdateWordCount;

     finally
        HandlingTimerTick:= False;
     end;

     if (not UpdatingTextPlain) and (ActiveFile <> nil) and (not ActiveFile.TextPlainVariablesInitialized)
                                                                  and (_MillisecondsIdle >= 450) then
        ActiveFile.UpdateTextPlainVariables(Integer.MaxValue);  // It can continue running for multiple timer ticks

  except
        // drop all exceptions here
  end;

end; // OnTimer


procedure TForm_Main.AppDeactivate( sender : TObject );
begin
   if IntroducingTagsState = itWithTagSelector then
      cTagSelector.CloseTagSelector(false);

  if (ActiveFile <> nil) and ActiveFile.ChangedOnDisk then exit;
  if (( not ( AppIsClosing or Initializing or ActiveFileIsBusy )) and
      ( HaveKntFolders( false, false ))) then
  begin
    if ( KeyOptions.AutoSave and KeyOptions.AutoSaveOnFocus and ActiveFile.Modified ) then begin
      if (( ActiveFile.FileName <> '' ) and ( not ActiveFile.ReadOnly )) then begin
        // only if saved previously
        {$IFDEF KNT_DEBUG}
          Log.Add( '-- Saving on Application DEACTIVATE' );
        {$ENDIF}
          KntFileSave( ActiveFile.FileName );
      end;
    end;
  end;
end; // AppDeactivate

procedure TForm_Main.WMActivate( Var msg: TWMActivate );
begin
  if ( msg.Active <> WA_INACTIVE ) then begin
    if (ActiveFile <> nil) and ActiveFile.ChangedOnDisk then begin
      {$IFDEF KNT_DEBUG}
        Log.Add( 'FileChangedOnDisk!' );
      {$ENDIF}
        ActiveFile.ChangedOnDisk := false;
        SomeoneChangedOurFile;
    end;
    AppIsClosing := false;
  end;
  inherited;
end; // WMActivate

procedure TForm_Main.WMHotkey( Var msg: TWMHotkey );
begin
  if ( msg.hotkey = 1 ) then
  begin
    FRTLShortcutToExecute:= rtNone;
    if IsIconic( Application.Handle ) then
      Application.Restore
    else
      if not Active then
        //Application.BringToFront;
        SetForegroundWindow(Application.Handle);
  end;
end; // WMHotkey

procedure TForm_Main.WMQueryEndSession( var Msg : TMessage );
begin
  ClosedByWindows := true;
  {$IFDEF KNT_DEBUG}
  Log.Add( 'Closed by Windows: WM_QUERYENDSESSION' );
  {$ENDIF}
  Msg.Result := 1;
  inherited;
end; // WMQueryEndSession

procedure TForm_Main.WndProc( var M : TMessage );
begin
  if M.Msg = _KNT_WINMSG_ID then
  begin
    case M.WParam of
      KNT_MSG_PLUGIN_SHUTDOWN : begin
        // resident plugin shuts down and has notified us about it
        UnloadResidentPlugin( M.LParam );
      end;
    end;
  end;
  inherited WndProc( M );
end; // WndProc

procedure TForm_Main.ShowException( Sender : TObject; E : Exception );
begin
  {$IFDEF KNT_DEBUG}
  if assigned( Log ) then
  begin
    Log.Add( '!! Unhandled exception: ' + e.message );
    Log.Flush( true );
  end;
  {$ENDIF}

  If Application.MessageBox(
    PChar(format(GetRS(sMain08), [E.Message, URL_Issues])), PChar(GetRS(sMain09)),
    MB_YESNO+MB_SYSTEMMODAL+MB_ICONHAND+MB_DEFBUTTON2) = ID_YES Then
  begin
    ClosedByWindows := true; // means: don't display exit confirmation dialog
    TerminateClick := true;
    PostMessage( Handle, WM_QUIT, 0, 0 ); // docs say to use PostQuitMessage, but I've had problems with it
    Application.Terminate;
  end;
end; // ShowException


procedure TForm_Main.CloseNonModalDialogs;
begin
  if ( Form_Chars <> nil ) then
    Form_Chars.Close;
  if ( Form_FindReplace <> nil ) then
    Form_FindReplace.Close;
end; // CloseNonModalDialogs

procedure TForm_Main.MMFileNewClick(Sender: TObject);
begin
  KntFileNew( '' );
end;

procedure TForm_Main.MMFileOpenClick(Sender: TObject);
begin
  KntFileOpen( '' );
end; // MMOpenClick

procedure TForm_Main.actFileSaveExecute(Sender: TObject);
begin
{$IFDEF KNT_DEBUG}
  Log.Add( '-- Saving on user request' );
{$ENDIF}
  if ShiftDown then
     KntFileSave('')
  else begin
    if HaveKntFolders(false, false) then
       KntFileSave(ActiveFile.FileName)
    else
       KntFileSave('');
  end;
end;

procedure TForm_Main.MMFileSaveAsClick(Sender: TObject);
begin
  KntFileSave( '' );
end;

procedure TForm_Main.MMFolderNewClick(Sender: TObject);
begin
  TKntFolder.CreateNewKntFolder;
end;

procedure TForm_Main.MMHelpTipClick(Sender: TObject);
begin
  App.ShowTipOfTheDay;
end;

procedure TForm_Main.MMFileCloseClick(Sender: TObject);
begin
  KntFileClose;
end;

procedure TForm_Main.MMNoteRenameClick(Sender: TObject);
begin
  TKntFolder.RenameKntFolder;
end;

procedure TForm_Main.UpdateTabAndTreeIconsShow;
begin
  if assigned( ActiveFile ) then
  begin
    MMViewTabIcons.Enabled := ActiveFile.ShowTabIcons;
    if ( ActiveFile.ShowTabIcons and TabOptions.Images ) then
      Pages.Images := Chest.IMG_Categories
    else
      Pages.Images := nil;
  end
  else
  begin
    MMViewTabIcons.Enabled := true;
    Pages.Images := Chest.IMG_Categories;
  end;

end; // UpdateTabAndTreeIconsShow

procedure TForm_Main.MMFilePropertiesClick(Sender: TObject);
begin
  KntFileProperties;
end;

procedure TForm_Main.MMFileAutoSaveClick(Sender: TObject);
begin
  KeyOptions.AutoSave := ( not KeyOptions.AutoSave );
  AutoSaveToggled;
end;

procedure TForm_Main.AutoSaveToggled;
begin
  MMFileAutoSave.Checked := KeyOptions.AutoSave;
end;

procedure TForm_Main.AnotherInstance;
begin
  Application.Restore;
  Application.BringToFront;
end; // AnotherInstance

procedure TForm_Main.ShowInsMode;
begin
  if ActiveFolder.IsInsertMode then
    StatusBar.Panels[PANEL_INS].Text := GetRS(sMain11)
  else
    StatusBar.Panels[PANEL_INS].Text := GetRS(sMain12);
end; // ShowInsMode

procedure TForm_Main.PagesDblClick(Sender: TObject );
begin
  if ShiftDown then
    TKntFolder.EditKntFolderProperties( propThisFolder )
  else
    TKntFolder.RenameKntFolder;
end; // PagesDblClick



procedure TForm_Main.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of

    VK_TAB :  if ( shift = [ssCtrl] ) then begin // Ctrl+Tab: switch to next tab
       key := 0;
       Pages.SelectNextPage( true );
       // RxRTFKeyProcessed := true;
    end
    else
      if ( shift = [ssCtrl,ssShift] ) then begin
         Key := 0;
         Pages.SelectNextPage( false );
      end;

    VK_PRIOR : if ( shift = [ssCtrl] ) then begin // Page Up
      key := 0;
      Pages.SelectNextPage( false );
    end
    else
    if ( KeyOptions.ResPanelShow and ( shift = [ssAlt] )) then begin
      key := 0;
      Pages_Res.SelectNextPage( false );
    end;

    VK_NEXT : if ( shift = [ssCtrl] ) then begin // Page Down
      key := 0;
      Pages.SelectNextPage( true );
    end
    else
    if ( KeyOptions.ResPanelShow and ( shift = [ssAlt] )) then begin
      key := 0;
      Pages_Res.SelectNextPage( true );
    end;

    VK_ESCAPE : begin
      //_Is_Dragging_Text := false;
      if (( shift = [] ) and ( not (
        Combo_Font.DroppedDown or
        Combo_FontSize.DroppedDown or
        Combo_Style.DroppedDown or
        Combo_ResFind.DroppedDown or
        Combo_Zoom.DroppedDown ))) then
      begin
        // ESC has different functions depending on
        // what's happening at the moment:
        key := 0;
        if ( Is_Replacing or SearchInProgress ) then
          UserBreak := true // will abort search
        else
        if IsRunningMacro then
          MacroAbortRequest := true // will abort macro
        else
        if IsRecordingMacro then
          TB_MacroRecordClick( TB_MacroRecord ) // aborts macro recording
        else
        if CopyFormatMode <> cfDisabled then
           EnableCopyFormat(False)
        else begin
          if ( activecontrol = Combo_FontSize ) or
             ( activecontrol = Combo_Font ) or
             ( activecontrol = FindAllResults ) or (activecontrol = Btn_ResFind) or (activecontrol = Btn_ResFlip) or
             (( activecontrol = combo_ResFind ) and (combo_resfind.Text = '')) or
             (( activecontrol = Res_RTF ) and (KeyOptions.EscAction <> ESC_MINIMIZE)) or    // See "Pressing ESC in Scratchpad editor" #744
             ( activecontrol = ListBox_ResMacro ) or
             ( activecontrol = ListBox_ResTpl ) or
             ( activecontrol = ListBox_ResPlugins ) or
             ( activecontrol = ListBox_ResFav ) or
             ( activecontrol = TVTags ) or
             ( activecontrol = Combo_Zoom ) or
             ( activecontrol = Combo_Style ) or
             ( IntroducingTagsState = itWithTagSelector) or
             ( activecontrol = txtTagsIncl) or
             ( activecontrol = txtTagsExcl) or
             ( activecontrol.Name = 'txtTags') or
             ( activecontrol.Name = 'txtFilter') then
          begin
            // if these controls are focused,
            // switch focus to editor
            if activecontrol = Combo_Zoom then
               Combo_Zoom.Text := '';        // To ignore the value set
            key := 0;
            if IntroducingTagsState = itWithTagSelector then begin
               IgnoreSelectorForTagSubsr := cTagSelector.SelectedTagName;
               cTagSelector.CloseTagSelector(false);
            end
            else
               FocusActiveKntFolder;
          end
          else
          if ( activecontrol = combo_ResFind ) then
            combo_resfind.Text := ''
          else begin
            // otherwise perform the function which
            // is configured in options
            case KeyOptions.EscAction of
              ESC_MINIMIZE : Application.Minimize;
              ESC_QUIT : Close;
            end;
          end;
        end;
      end;
    end;


    VK_INSERT:
       if ( shift = [ssShift] ) then begin
         if CmdPaste(false, false) then key:= 0;
       end
       else if shift = [ssCtrl] then begin
         if CmdCopy then key:= 0;
       end;

    VK_DELETE:
       if ( shift = [ssShift] ) then begin
         if CmdCut then key:= 0;
       end;

// We'll manage CTR-V,CTR-C,CTR-X from FormShortCut. From this only CTR-V can be intercepted
//    ELSE BEGIN
//        ShortCutToKey(16470, myKey, myShift);
//        if (myKey=key) and (myShift=Shift) then begin
//            if CmdPaste(false) then key:= 0;
//        end
//    END;
  end;

  if not (key in [0, 17, 18]) and
           ( (ssCtrl in Shift) or (ssAlt in Shift) ) or
           ( (Shift = [ssShift]) and (key in [VK_F1..VK_F4]) ) then begin
     if PerformOtherCommandShortcut( Key, Shift ) then
         Key := 0;
  end;

end; // KEY DOWN



{ https://answers.microsoft.com/en-us/windows/forum/all/ctrlshift0-in-windows-10/337b6542-4159-4143-83a8-a4496f8f995c
<< it appears that the KeyDown event is not getting through in some cases for Ctrl+Shift+0, but the KeyUp event is. >>
}
procedure TForm_Main.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = Ord('0')) and (Shift = [ssCtrl, ssShift]) then
     PerformCmd( ecClearParaAttr );
end;


function TForm_Main.FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   if AltDown or CtrlDown or ShiftDown then exit;

   // -1 -> 536-1   "Restore image[s] proportions" -> Dimensions and proportions
   // -2 -> 284-11  "Children Checkboxes" -> Tree-type Notes [284] / 11 (Using Hidden nodes)
   // -3 -> 282-9   Menu Number -> KeyNote Editor [282]/ 9 (Using bullets and numbered lists)
   // -4 -> 11-1    Menu ResPanel -> KeyNote Screen [11] / Marker: 1
   // -5 -> 479-5   Find All
   if (Command = HELP_CONTEXT) and (Data < 0) then
       Command:= HELP_COMMAND;
       case Data of
         -1: Data:= NativeInt(PChar('536-1'));
         -2: Data:= NativeInt(PChar('284-11'));
         -3: Data:= NativeInt(PChar('282-9'));
         -4: Data:= NativeInt(PChar('11-1'));
         -5: Data:= NativeInt(PChar('479-5'));
       end;

   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;



procedure TForm_Main.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  ShiftPressed: boolean;
  ShortCut: TShortCut;
  ShortCutItem: TMenuItem;
begin
   if not Active then exit;         // => ImgViewerInstance.Active = True

   ShortCut := ShortCutFromMessage(Msg);
   if ShortCut = MMViewResPanel.ShortCut then begin
       MMViewResPanelClick (nil);
       Handled:= true;
   end;

   if (GetKeyState(VK_CONTROL) < 0) and not (GetKeyState(VK_MENU) < 0) then begin
      ShiftPressed:= (GetKeyState(VK_SHIFT) < 0);


      // CharCode = 16 -> Shift | CharCode = 17 -> Ctrl
      if ShiftPressed then
         if Msg.CharCode in [16..17] then begin                      // See comment *1 and *2 in AppRestore
            FRTLShortcutToExecute:= rtNone;
            if (GetKeyState(VK_RCONTROL) < 0) and (GetKeyState(VK_RSHIFT) < 0) then
               FRTLShortcutToExecute:= rtRTL
            else
            if (GetKeyState(VK_LCONTROL) < 0) and (GetKeyState(VK_LSHIFT) < 0) then
               FRTLShortcutToExecute:= rtLTR;

            FRTLShortcutDetectedAt:= now;
            Handled:= True;
            exit;
         end
         else
            FRTLShortcutToExecute:= rtNone;

      ShortCutItem:= nil;
      if ShiftPressed then begin
         //ShortCut := ShortCutFromMessage(Msg);
         ShortCutItem := Menu.FindItem(ShortCut, fkShortCut);
      end;

      if assigned(ShortCutItem) then exit;

      if Msg.CharCode = Ord('C')  then begin
          if CmdCopy then Handled:= true;
      end else if Msg.CharCode = Ord('V') then begin
          if shiftPressed then begin
              if CmdPaste(false, true)  then Handled:= true;    // Plain text
          end
          else begin
              if CmdPaste(false, false) then Handled:= true;
          end;
      end else if Msg.CharCode = Ord('X') then begin
          if CmdCut then Handled:= true;

      end
      else if (not ShiftPressed) then begin
        if assigned(ActiveEditor) and (Msg.CharCode in [VK_DOWN, VK_UP]) then
           case EditorOptions.CtrlUpDownMode of
              TCtrlUpDownMode.cudDefault: exit;
              TCtrlUpDownMode.cudShiftLine: begin
                 if Msg.CharCode = VK_DOWN then begin
                    ActiveEditor.ScrollLinesBy(1);
                    Handled:= true;
                 end else if Msg.CharCode = VK_UP then begin
                    ActiveEditor.ScrollLinesBy(-1);
                    Handled:= true;
                 end;
              end;

              TCtrlUpDownMode.cudShiftScrollbar: begin
                 var P: TPoint;
                 P:= ActiveEditor.GetScrollPosInEditor;
                 if Msg.CharCode = VK_DOWN then
                    Inc(P.Y, 20)
                 else
                    Dec(P.Y, 20);
                 ActiveEditor.SetScrollPosInEditor(P);
                 Handled:= true;
              end;
           end

        else begin
           if Msg.CharCode = VK_RETURN then begin
              if activeControl = ListBox_ResFav then begin
                JumpToFavorite;
                Handled:= true;
              end;
           end;
        end;

      end;

      if (not Handled) and KeyOptions.UseCtrlHideTreePanel then begin
          // Check if the keys combination, without Ctrl, is a shortcut to MMViewTree
          ShortCutItem := Menu.FindItem(ShortCut - scCtrl, fkShortCut);
          if ShortCutItem = MMViewTree then begin
             MMViewTreeClick(nil);
             Handled:= true;
          end;
      end;

   end
   else begin
      if (Msg.CharCode = VK_DOWN) and (GetKeyState(VK_MENU) < 0) then begin
         { The Alt+Down shortcut (usually assigned to Navigate Down in KeyNote [MMTreeNavDown] ) will only be enabled
           when the tree or editor (including scratchpad) has focus.
           This way, it can be used to expand drop-down controls like Text to Find, Zoom, or Styles, when they are focused }
         ShortcutAltDownMenuItem := Menu.FindItem(ShortCut, fkShortCut);
         if ShortcutAltDownMenuItem <> nil then
            if ((ActiveEditor <> nil) and (ActiveEditor.Focused)) or
               ((ActiveTreeUI <> nil) and (ActiveTreeUI.Focused)) or Form_Main.Res_RTF.Focused
             then
                ShortcutAltDownMenuItem.Enabled:= True
             else
                ShortcutAltDownMenuItem.Enabled:= False;
      end;
   end;

   if Handled then
      FRTLShortcutToExecute:= rtNone;
end;


procedure TForm_Main.Combo_StyleChange(Sender: TObject);
var
  idx : integer;
  name : string;
begin
  idx := Combo_Style.ItemIndex;
  Combo_Style.Hint := '';
  if ( idx < 0 ) then exit;
  name := Combo_Style.Items[idx];
  if ( StyleManager.Count > idx ) then
  begin
    case TStyle( StyleManager.Objects[idx] ).Range of
      srFont : StatusBar.Panels[PANEL_HINT].Text :=
        TStyle( StyleManager.Objects[idx] ).FontInfoToStr( true );
      srParagraph : StatusBar.Panels[PANEL_HINT].Text :=
        TStyle( StyleManager.Objects[idx] ).ParaInfoToStr( true );
      srBoth : StatusBar.Panels[PANEL_HINT].Text :=
        TStyle( StyleManager.Objects[idx] ).FontInfoToStr( true ) +
        TStyle( StyleManager.Objects[idx] ).ParaInfoToStr( true );
    end;
    Combo_Style.Hint := StatusBar.Panels[PANEL_HINT].Text;
  end;

end; // Combo_StyleChange


procedure TForm_Main.UpdateWordWrap;
var
  isWordWrap : boolean;
  Enabled: boolean;
begin
  if assigned(ActiveEditor) then begin
    Enabled:= ActiveEditor.Enabled and not ActiveEditor.ReadOnly;
    MMFormatWordWrap.Enabled:= Enabled;
    TB_WordWrap.Enabled:= Enabled;
    RTFMWordwrap.Enabled:= Enabled;

    isWordWrap := ActiveEditor.WordWrap;
    MMFormatWordWrap.Checked := isWordWrap;
    TB_WordWrap.Down := isWordWrap;
    RTFMWordwrap.Checked := isWordWrap;
  end;
end; // UpdateWordWrap

procedure TForm_Main.MMEditSelectAllClick(Sender: TObject);
begin
    PerformCmd( ecSelectAll );
end;

procedure TForm_Main.MMFormatWordWrapClick(Sender: TObject);
begin
    PerformCmd( ecWordWrap );
end;

procedure TForm_Main.MMNoteReadOnlyClick(Sender: TObject);
begin
  PerformCmdEx( ecReadOnly );
end;

procedure TForm_Main.MMEditEvaluateClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.EvaluateExpression;
end;


procedure TForm_Main.PagesChange(Sender: TObject);
begin
   App.ActivateFolder(Pages.ActivePage.TabIndex);
end;

procedure TForm_Main.PagesTabShift(Sender: TObject);
var
  Folder: TKntFolder;
  Tab: TTab95Sheet;
begin
 Tab:= Pages.ActivePage;
 Folder := TKntFolder( Tab.PrimaryObject );
 if assigned( Folder ) then begin
    Tab.ImageIndex := Folder.ImageIndex;
    App.ActivateFolder(Folder);

    App.FileSetModified;
 end;
end; // TAB SHIFT

procedure TForm_Main.ShiftTab( const ShiftRight : boolean );
var
  i, idx, iLastTab : integer;
  Folder: TKntFolder;
  Tab: TTab95Sheet;

begin
 if ( Pages.PageCount < 2 ) then exit;

 Tab:= Pages.ActivePage;
 Folder := TKntFolder( Tab.PrimaryObject );

 if assigned( Folder ) then begin
    i:= Tab.TabIndex;
    iLastTab:= Pages.PageCount-1;
    if ShiftRight then begin
       if (i < iLastTab) then
          inc(i)
       else
          i:= 0;
    end
    else begin
       if (i >= 1) then
          dec(i)
       else
          i:= iLastTab;
    end;

    Folder.TabIndex:= i;
    Tab.ImageIndex := Folder.ImageIndex;
    App.ActivateFolder(Folder);

    App.FileSetModified;
 end;

end; // ShiftTab

procedure TForm_Main.MMViewShiftTabLeftClick(Sender: TObject);
begin
  ShiftTab( false );
end;

procedure TForm_Main.MMViewShiftTabRightClick(Sender: TObject);
begin
  ShiftTab( true );
end;


procedure TForm_Main.MMEditCutClick(Sender: TObject);
begin
    CmdCut;
end; // CUT

procedure TForm_Main.MMEditCopyClick(Sender: TObject);
begin
    CmdCopy;
end; // COPY

procedure TForm_Main.MMEditPasteClick(Sender: TObject);
begin
    CmdPaste(sender is TToolbarButton97, false);
end; // PASTE

procedure TForm_Main.MMEditDeleteClick(Sender: TObject);
begin
    PerformCmd( ecDelete );
end; // DELETE

procedure TForm_Main.MMEditUndoClick(Sender: TObject);
begin
    PerformCmd( ecUndo );
end;

procedure TForm_Main.MMEditRedoClick(Sender: TObject);
begin
    PerformCmd( ecRedo );
end;

procedure TForm_Main.MMEditPasteAsTextClick(Sender: TObject);
begin
  cmdPaste(false, true);
end;


procedure TForm_Main.NewVersionInformation;
begin
  if ( not KeyOptions.IgnoreUpgrades and ( IsLaterVersion(KeyOptions.LastVersion, Program_Version_Number ) )) then begin
     //KeyOptions.TipOfTheDay := true;
     KeyOptions.TipOfTheDayIdx := -1;
     case messagedlg(
       Format(GetRS(sMain17)
       , [KeyOptions.LastVersion, Program_Version, SampleFileName] ),
       mtInformation, [mbYes,mbNo], 0
     ) of
       mrYes : begin
         DisplayHistoryFile;
       end;
     end;
  end;

end; // NewVersionInformation

procedure TForm_Main.DisplayHistoryFile;
var
  fn : string;
begin
  ActiveKeyNoteHelp(_KNT_HELP_FILE_NOTE_WHATSNEW_ID, 4, 0);   // History.txt [4]
  {
  fn := extractfilepath( Application.Exename ) + 'doc\history.txt';
  if FileExists( fn ) then
    ShellExecute( 0, 'open', PChar( fn ), nil, nil, SW_NORMAL )
  else
    DoMessageBox( Format( GetRS(sMain18, [fn] ), mtError, [mbOK], 0 );
   }
end; // DisplayHistoryFile

procedure TForm_Main.Combo_FontChange(Sender: TObject);
begin
  if assigned(ActiveFolder) and (ActiveFolder.FocusMemory = focTree) then
     ActiveFolder.TreeUI.SetNodeFontFace(false, ShiftDown )
  else
     PerformCmd( ecFontName );
end; // Combo_FontChange

procedure TForm_Main.Combo_FontKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  FocusTree: boolean;
begin
  FocusTree:= false;
  if assigned(ActiveFolder) and (ActiveFolder.FocusMemory = focTree) then
     FocusTree:= true;

  if ( shift = [] ) then begin
       case key of
         VK_RETURN, VK_ESCAPE : begin
           if (key = VK_RETURN) then begin
             if FocusTree then
                ActiveFolder.TreeUI.SetNodeFontFace(false, ShiftDown)
             else
                PerformCmd(ecFontName);
           end;

           key := 0;
           try
             if FocusTree then
                ActiveFolder.TV.SetFocus
             else
             if assigned(ActiveEditor) then
                ActiveEditor.SetFocus;
           except
           end;
         end;
       end;
  end;
end; // Combo_FontKeyDown


procedure TForm_Main.Combo_FontSizeClick(Sender: TObject);
begin
  if (App.Kbd.RTFUpdating or ActiveFileIsBusy) then exit;
  if ((Sender = Combo_FontSize) and (Combo_FontSize.Text = '')) then exit;

  if (Sender = Combo_FontSize) then
     PerformCmd( ecFontSize )

  else if (Sender = Combo_Zoom) then begin
     App.SetEditorZoom(-1, Combo_Zoom.Text );
     FocusActiveEditor;
  end;

end; // Combo_FontSizeClick


procedure TForm_Main.Combo_FontSizeExit(Sender: TObject);
begin
  if Combo_FontSize.Text <> '' then
     PerformCmd(ecFontSize)
  else
    try
       Combo_FontSize.Text := IntToStr( ActiveEditor.SelAttributes.Size );
    except
    end;
end;


procedure TForm_Main.Combo_FontSizeKeyPress(Sender: TObject;
  var Key: Char);
var
  FocusTree: boolean;
begin
  FocusTree:= false;
  if assigned(ActiveFolder) and (ActiveFolder.FocusMemory = focTree) then
     FocusTree:= true;

  if ( not assigned( ActiveFolder )) then exit;

  if not (key in [#8, #9, #13, #27, #37..#40, #46, '0'..'9', '%']) then begin
     key := #0;
     exit;
  end;

  if ( key = #13 ) then begin
     key := #0;

     if (Sender = Combo_Zoom ) then
        App.SetEditorZoom( -1, Combo_Zoom.Text );

     try
       if FocusTree then
          ActiveFolder.TV.SetFocus
       else
       if assigned(ActiveEditor) then
          ActiveEditor.SetFocus;

     except
     end;
  end
  {                                      It's controlled from TForm_Main.FormKeyDown
  else
  if ( key = #27 ) then begin
    key := #0;
    FocusActiveKntFolder;
  end;
  }
end; // Combo_FontSizeKeyPress


procedure TForm_Main.MMFormatBoldClick(Sender: TObject);
var
  BoldWasDown : boolean;
begin
  if (self.ActiveControl is TVTree) and (assigned(ActiveFolder.FocusedNNode)) then begin
     ActiveFolder.TreeUI.SetNodeBold(ShiftDown);
     TB_Bold.Down := ActiveNNode.Bold;           // Change could be rejected and button .Down state has ALREADY changed
  end
  else
     PerformCmd( ecBold );
end;

procedure TForm_Main.MMFormatItalicsClick(Sender: TObject);
begin
  PerformCmd( ecItalics );
end;

procedure TForm_Main.MMFormatUnderlineClick(Sender: TObject);
begin
  PerformCmd( ecUnderline );
end;

procedure TForm_Main.MMFormatStrikeoutClick(Sender: TObject);
begin
  PerformCmd( ecStrikeOut );
end;

procedure TForm_Main.MMFormatClearFontAttrClick(Sender: TObject);
begin
  PerformCmd( ecClearFontAttr );
end;

procedure TForm_Main.MMFormatClearParaAttrClick(Sender: TObject);
begin
  PerformCmd( ecClearParaAttr );
end;

procedure TForm_Main.MMFormatBulletsClick(Sender: TObject);
begin
  PerformCmd( ecBullets );
end;

procedure TForm_Main.MMFormatLIndIncClick(Sender: TObject);
begin
  PerformCmd( ecIndent );
end;

procedure TForm_Main.MMFormatLIndDecClick(Sender: TObject);
begin
  PerformCmd( ecOutdent );
end;

procedure TForm_Main.MMFormatFIndIncClick(Sender: TObject);
begin
    PerformCmd( ecFirstIndent );
end;

procedure TForm_Main.MMFormatFindDecClick(Sender: TObject);
begin
    PerformCmd( ecFirstOutdent );
end;

procedure TForm_Main.MMFormatRIndIncClick(Sender: TObject);
begin
  PerformCmd( ecRightIndent );
end;

procedure TForm_Main.MMFormatRIndDecClick(Sender: TObject);
begin
  PerformCmd( ecRightOutdent )
end;


procedure TForm_Main.MMFormatAlignLeftClick(Sender: TObject);
begin
  PerformCmd( ecAlignLeft );
end;

procedure TForm_Main.MMFormatAlignCenterClick(Sender: TObject);
begin
  PerformCmd( ecAlignCenter );
end;

procedure TForm_Main.MMFormatAlignRightClick(Sender: TObject);
begin
  PerformCmd( ecAlignRight );
end;

procedure TForm_Main.MMFormatNumbersClick(Sender: TObject);
begin
  PerformCmd( ecNumbers );
end;

procedure TForm_Main.MMFormatAlignJustifyClick(Sender: TObject);
begin
  PerformCmd( ecAlignJustify );
end;

procedure TForm_Main.MMFormatFontClick(Sender: TObject);
begin
  PerformCmd( ecFontDlg );
end;

procedure TForm_Main.MMFormatTextColorClick(Sender: TObject);
begin
   if (self.ActiveControl is TVTree) then
      ActiveFolder.TreeUI.SetNodeColor(KeyOptions.UseOldColorDlg, true, false, ShiftDown  )
   else
     if KeyOptions.UseOldColorDlg then
       PerformCmd( ecFontColorDlg ) // bring up color dialog
     else
       PerformCmd( ecFontColor ); // apply last color
end;

procedure TForm_Main.MMFormatHighlightClick(Sender: TObject);
begin
   if (self.ActiveControl is TVTree) then
      ActiveFolder.TreeUI.SetNodeColor(KeyOptions.UseOldColorDlg, false, false, ShiftDown)
   else
     if KeyOptions.UseOldColorDlg then
       PerformCmd( ecHighlightDlg )
     else
       PerformCmd( ecHighlight );
end;

procedure TForm_Main.MMViewFilterTreeClick(Sender: TObject);
begin
   if ActiveTreeUI <> nil then
      ActiveTreeUI.TB_FilterTreeClick(nil);
end;

procedure TForm_Main.MMViewFormatNoneClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
  begin
    case ( sender as TMenuItem ).Tag of
      ord( low( TStyleRange ))..ord( high( TStyleRange )) : begin
        EditorOptions.TrackStyle := true;
        EditorOptions.TrackStyleRange := TStyleRange(( sender as TMenuItem ).Tag );
      end;
      else
      begin
        EditorOptions.TrackStyle := false;
      end;
    end;
    ( sender as TMenuItem ).Checked := true;
    CheckTrackStyleInfo(ActiveEditor);
  end;
end;

procedure TForm_Main.MMViewHideCheckedNodesClick(Sender: TObject);
begin
   if ActiveTreeUI <> nil then
      ActiveTreeUI.TB_HideCheckedClick(Sender);
end;

procedure TForm_Main.CheckTrackStyleInfo (Editor: TKntRichEdit);
begin
    if EditorOptions.TrackStyle then begin
      if Editor = nil then exit;

      case EditorOptions.TrackStyleRange of
        srFont : begin
          StatusBar.Panels[PANEL_HINT].Text := Editor.FontInfoString;
        end;
        srParagraph : begin
          StatusBar.Panels[PANEL_HINT].Text := Editor.ParaInfoString;
        end;
        srBoth : begin
          StatusBar.Panels[PANEL_HINT].Text := Editor.FontInfoString +
          Editor.ParaInfoString;
        end;
      end;
    end;
end;


procedure TForm_Main.MMFormatNoHighlightClick(Sender: TObject);
begin
   if (self.ActiveControl is TVTree) then
      ActiveFolder.TreeUI.SetNodeColor(false, false, true, ShiftDown  )
   else
      PerformCmd( ecNoHighlight );
end;

procedure TForm_Main.TB_ColorClick(Sender: TObject);
begin
  TB_Color.OnClick := nil;
  try
    if (self.ActiveControl is TVTree) then
      ActiveFolder.TreeUI.SetNodeColor(false, true, false, ShiftDown  )
    else
      PerformCmd( ecFontColorBtn );
  finally
    TB_Color.OnClick := TB_ColorClick;
  end;
end;

procedure TForm_Main.TB_HiliteClick(Sender: TObject);
begin
  TB_Hilite.OnClick := nil;
  try
    if (self.ActiveControl is TVTree) then
      ActiveFolder.TreeUI.SetNodeColor(false, false, false, ShiftDown )
    else
      PerformCmd( ecHighlightBtn );
  finally
    TB_Hilite.OnClick := TB_HiliteClick;
  end;
end;

procedure TForm_Main.MMFormatBGColorClick(Sender: TObject);
begin
  PerformCmd( ecBGColorDlg );
end;

procedure TForm_Main.MRUMRUItemClick(Sender: TObject; AFilename: string);
begin
  KntFileOpen( AFilename );
end; // MRUMRUItemClick

procedure TForm_Main.DebugMenuClick(Sender: TObject);
var
  s : string;
  KntFile: TKntFile;
  // i : integer;
begin
{$IFDEF KNT_DEBUG}
  if assigned( Log ) then Log.Flush( true );
{$ENDIF}
  { [debug] }

  KntFile:= ActiveFile;

  s := 'Win32Platform: ' + Win32Platform.ToString + #13 +
       'Win32MajorVersion: ' + Win32MajorVersion.ToString + #13 +
       'Win32MinorVersion: ' + Win32MinorVersion.ToString + #13 +
       'RichEdit version: '  + _LoadedRichEditVersion.ToString + #13 +
       'AllocMemSize: ' + AllocMemSize.ToString;

  if ( messagedlg( s, mtInformation, [mbOK,mbCancel], 0 ) = mrCancel ) then exit;

  if HaveKntFolders( false, false ) then begin
    s := 'Filename: ' + ExtractFilename( KntFile.FileName ) +#13+
         'Notes modified: ' + BOOLARRAY[KntFile.Modified] +#13+
         'Folders count: ' + inttostr( KntFile.FolderCount ) +#13+
         'File Read-Only: ' + BOOLARRAY[KntFile.ReadOnly] +#13+
         'File Busy: ' + BOOLARRAY[ActiveFileIsBusy] +#13+
         'File format: ' + FILE_FORMAT_NAMES[KntFile.FileFormat] + #13#13+
         'FolderMon active: ' + BOOLARRAY[FolderMon.Active] +#13+
         'FolderMon folder: ' + FolderMon.FolderName + #13#13 +
         'IS_OLD_FILE_FMT: ' + BOOLARRAY[_IS_OLD_KEYNOTE_FILE_FORMAT] +#13+
         'USE_OLD_FILE_FMT: ' + BOOLARRAY[_USE_OLD_KEYNOTE_FILE_FORMAT] +#13#13;

    if KntFile.FileFormat = nffEncrypted then
      s := s + 'Encrypted with: ' + CRYPT_METHOD_NAMES[KntFile.CryptMethod] + #13 +
               'Pass: ' + KntFile.PassPhrase +#13#13;

    if assigned( ActiveFolder ) then begin
      s := s +
         'Active note name: ' + ActiveFolder.Name +#13;

      s := s + 'Number of Tree nodes: ' + inttostr( ActiveFolder.TV.TotalCount ) + #13 +
        'Number of Folder notes: ' + inttostr( ActiveFolder.NNodes.Count ) + #13;
      if assigned( ActiveFolder.FocusedNNode) then
        s := s + 'Selected note: ' + ActiveFolder.FocusedNNode.NoteName +#13;
    end
    else
      s := s + 'ActiveFolder NOT assigned' + #13;

    if ( _OTHER_INSTANCE_HANDLE <> 0 ) then
      s := s + 'Found other instance!' + #13;

  end
  else
     s := 'No notes.' + #13;


  if assigned( FileManager ) then
    s := s + #13 + 'File Manager count: ' + inttostr( FileManager.Count )
  else
    s := s + 'File Manager NOT assigned.';

{$IFDEF KNT_DEBUG}
  s := s + #13 + '(Log enabled)';
{$ENDIF}

  if App.DoMessageBox( s, mtInformation, [mbOK,mbCancel] ) = mrCancel then exit;

end; // DEBUG MENU CLICK


procedure TForm_Main.ShowAbout;
var
  AboutBox : TAboutBox;
begin
  AboutBox := TAboutBox.Create( self );
  try
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end; // ShowAbout

procedure TForm_Main.MMToolsOptionsClick(Sender: TObject);
begin
  AdjustOptions;
end;

procedure TForm_Main.MMHelpChkUpdClick(Sender: TObject);
begin
    CheckForUpdate (false);
end;

procedure TForm_Main.MMHelpAboutClick(Sender: TObject);
begin
  ShowAbout;
end;

procedure TForm_Main.MMViewOnTopClick(Sender: TObject);
begin
  with KeyOptions do
  begin
    AlwaysOnTop := ( not AlwaysOnTop );
    WinOnTop.AlwaysOnTop := AlwaysOnTop;
    MMViewOnTop.Checked := AlwaysOnTop;
    TB_OnTop.Down := MMViewOnTop.Checked;
  end;
end; // MMViewOnTopClick

procedure TForm_Main.MMViewTBMainClick(Sender: TObject);
begin
  with KeyOptions do
  begin
    ToolbarMainShow := ( not ToolbarMainShow );
    Toolbar_Main.Visible := ToolbarMainShow;
    MMViewTBMain.Checked := ToolbarMainShow;
  end;
end;

procedure TForm_Main.MMViewTBInsertClick(Sender: TObject);
begin
  with KeyOptions do
  begin
    ToolbarInsertShow := ( not ToolbarInsertShow );
    Toolbar_Insert.Visible := ToolbarInsertShow;
    MMViewTBInsert.Checked := ToolbarInsertShow;
  end;
end;

procedure TForm_Main.MMViewTBFormatClick(Sender: TObject);
begin
  with KeyOptions do
  begin
    ToolbarFormatShow := ( not ToolbarFormatShow );
    Toolbar_Format.Visible := ToolbarFormatShow;
    MMViewTBFormat.Checked := ToolbarFormatShow;
  end;
end;

procedure TForm_Main.MMViewTBStyleClick(Sender: TObject);
begin
  with KeyOptions do
  begin
    ToolbarStyleShow := ( not ToolbarStyleShow );
    Toolbar_Style.Visible := ToolbarStyleShow;
    MMViewTBStyle.Checked := ToolbarStyleShow;
  end;
end;


procedure TForm_Main.MMViewTBTreeClick(Sender: TObject);
begin
  ShowTBTree (not KeyOptions.ToolbarTreeShow)
end;

procedure TForm_Main.ShowTBTree(Show: boolean);
begin
   KeyOptions.ToolbarTreeShow:= Show;
   if ActiveTreeUI <> nil then
      ActiveTreeUI.PnlInf.Visible:= ( Show and assigned( ActiveFolder ));
   MMViewTBTree.Checked := Show;
end;


procedure TForm_Main.MMFileManagerClick(Sender: TObject);
begin
  RunFileManager;
end; // MMFileManagerClick

procedure TForm_Main.MMInsertDateClick(Sender: TObject);
begin
  if ShiftDown and ( sender is TToolbarButton97 ) then
  begin
    if LoadDateFormatsList then
      messagedlg( Format( GetRS(sMain19), [DATE_FORMAT_LIST.Count] ), mtInformation, [mbOK], 0 )
    else
      messagedlg( Format( GetRS(sMain20), [GetRS(sMain20a), _DateFormatsFile] ), mtError, [mbOK], 0 );
  end
  else
    PerformCmd( ecInsDate );
end;

procedure TForm_Main.MMInsertTimeClick(Sender: TObject);
begin
  if ShiftDown and ( sender is TTOolbarButton97 ) then
  begin
    if LoadTimeFormatsList then
      messagedlg( Format( GetRS(sMain21), [TIME_FORMAT_LIST.Count] ), mtInformation, [mbOK], 0 )
    else
      messagedlg( Format( GetRS(sMain20), [GetRS(sMain20b), _TimeFormatsFile] ), mtError, [mbOK], 0 );
  end
  else
    PerformCmd( ecInsTime );
end;

procedure TForm_Main.MMFolderRemoveClick(Sender: TObject);
begin
  TKntFolder.DeleteKntFolder;
end;

procedure TForm_Main.MMFileCopyToClick(Sender: TObject);
var
  SavedNotes, SavedNodes: integer;
begin
  KntFileCopy (SavedNotes, SavedNodes);
end;

procedure TForm_Main.MMFindGoToClick(Sender: TObject);
begin
  PerformCmdEx( ecGoTo );
end;

procedure TForm_Main.MMFormatDisabledClick(Sender: TObject);
begin
  PerformCmd( ecDisabled );
end;

procedure TForm_Main.MMFormatSubscriptClick(Sender: TObject);
begin
  PerformCmd( ecSubscript );
end;

procedure TForm_Main.MMFormatSuperscriptClick(Sender: TObject);
begin
  PerformCmd( ecSuperscript );
end;

procedure TForm_Main.MMFormatSpBefIncClick(Sender: TObject);
begin
  PerformCmd( ecSpaceBeforeInc );
end;

procedure TForm_Main.MMFormatSpBefDecClick(Sender: TObject);
begin
  PerformCmd( ecSpaceBeforeDec );
end;

procedure TForm_Main.MMFormatSpAftIncClick(Sender: TObject);
begin
  PerformCmd( ecSpaceAfterInc );
end;

procedure TForm_Main.MMFormatSpAftDecClick(Sender: TObject);
begin
  PerformCmd( ecSpaceAfterDec );
end;


procedure TForm_Main.MMToolsImportClick(Sender: TObject);
begin
  ImportFiles;
end;

procedure TForm_Main.MMToolsDefaultsClick(Sender: TObject);
begin
  TKntFolder.EditKntFolderProperties( propDefaults );
end;

procedure TForm_Main.MMNotePropertiesClick(Sender: TObject);
begin
  // EditNote;
  TKntFolder.EditKntFolderProperties( propThisFolder );
end;

procedure TForm_Main.TB_ExitClick(Sender: TObject);
begin
  Close; // No "TerminateClick"!
end;


procedure TForm_Main.MMSortClick(Sender: TObject);
begin
  PerformCmd( ecSort );
end;

procedure TForm_Main.MMFormatCopyClick(Sender: TObject);
begin
  FormatCopyClick(False);
end;

procedure TForm_Main.TB_CopyFormatClick(Sender: TObject);
begin
  FormatCopyClick(True);
end;

procedure TForm_Main.FormatCopyClick(AlloMultiMode: Boolean);
begin
  if not assigned( ActiveFile ) then exit;

  if CopyFormatMode= cfDisabled then begin
     if AlloMultiMode and (GetKeyState(VK_CONTROL) < 0) then
        CopyFormatMode:= cfEnabledMulti
     else
        CopyFormatMode:= cfEnabled;

     EnableCopyFormat(True);
     PerformCmdEx(ecCopyFormat)
  end
  else
     EnableCopyFormat(False);
end;


procedure TForm_Main.MMFormatCopyFontClick(Sender: TObject);
begin
  PerformCmdEx( ecFontFormatCopy );
end;

procedure TForm_Main.MMFormatPasteFontClick(Sender: TObject);
begin
  PerformCmd( ecFontFormatPaste );
end;

procedure TForm_Main.MMFormatCopyParaClick(Sender: TObject);
begin
  PerformCmdEx( ecParaFormatCopy );
end;

procedure TForm_Main.MMFormatPasteParaClick(Sender: TObject);
begin
  PerformCmd( ecParaFormatPaste );
end;

procedure TForm_Main.MMFindClick(Sender: TObject);
begin
  if assigned(ActiveEditor) and ( ActiveEditor.Focused ) then
     RunFinder
  else
     MMFindNodeClick(nil);
end;

procedure TForm_Main.MMFindNextClick(Sender: TObject);
begin
  if assigned(ActiveEditor) and ( ActiveEditor.Focused ) then
     DoFindNext
  else
     MMFindNodeNextClick(nil);

end; // MMFindagainClick

procedure TForm_Main.MMFindReplaceClick(Sender: TObject);
begin
  RunReplace;
end; // MMReplaceClick

procedure TForm_Main.MMFindReplaceNextClick(Sender: TObject);
begin
  if ( FindOptions.Pattern = '' ) then
    RunReplace
  else
    RunReplaceNext;
end; // MMReplaceNextClick

procedure TForm_Main.MMFindAllClick(Sender: TObject);
begin
   ShowFindAllOptions;
end;


function TForm_Main.HaveKntFolders( const Warn, CheckCount : boolean ) : boolean;
var
  msg : string;
begin
  result := true;
  msg := '';
  if ( not assigned( ActiveFile )) then begin
    result := false;
    if Warn then
      msg := GetRS(sMain10) + GetRS(sMain25);
  end
  else begin
    if ( CheckCount and (( ActiveFile.Folders.Count < 1 ) or ( Pages.PageCount < 1 ))) then begin
      result := false;
      if Warn then
        msg := GetRS(sMain10) + GetRS(sMain26);
    end;
  end;

  if (( not result ) and Warn ) then
    App.PopupMessage( msg, mtInformation, [mbOK] );

end; // HaveKntFolders

function TForm_Main.FolderIsReadOnly( const aFolder : TKntFolder; const Warn : boolean ) : boolean;
begin
  result := assigned( aFolder ) and aFolder.ReadOnly;
  if ( result and Warn ) then
    StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain10) + GetRS(sMain27);
end; // FolderIsReadOnly

(*  Not used. Referenced by kn_Main.GetKeyStates, not used
procedure TForm_Main.GetKeyStates;
var
  i : TLockKeys;
begin
  for i := low( TLockKeys ) to high( TLockKeys ) do
    if ( odd( GetKeyState( lock_Keys[i] )) <> lock_states[i] ) then
    begin
      lock_states[i] := ( not lock_states[i] );
      StatusBar.Panels[lock_panels[i]].Enabled := lock_states[i];
    end;
end; // GetKeyStates
*)


procedure TForm_Main.MMFormatFontSizeDecClick(Sender: TObject);
begin
  PerformCmd( ecFontSizeDec );
end;

procedure TForm_Main.MMFormatFontSizeIncClick(Sender: TObject);
begin
  PerformCmd( ecFontSizeInc );
end;

procedure TForm_Main.OnNoteLoaded( sender : TObject );
begin
  Application.ProcessMessages;
end; // OnNoteLoaded

procedure TForm_Main.MMEditJoinClick(Sender: TObject);
begin
  PerformCmd( ecJoinLines );
end;

procedure TForm_Main.MMEditUpperClick(Sender: TObject);
begin
  PerformCmd( ecToUpperCase );
end;

procedure TForm_Main.MMEditLowerClick(Sender: TObject);
begin
  PerformCmd( ecToLowerCase );
end;

procedure TForm_Main.MMEditMixedClick(Sender: TObject);
begin
  PerformCmd( ecToMixedCase );
end;

procedure TForm_Main.MMEditCycleCaseClick(Sender: TObject);
begin
  PerformCmd( ecCycleCase );
end;

procedure TForm_Main.MMEditDelLineClick(Sender: TObject);
begin
  PerformCmd( ecDeleteLine )
end;

procedure TForm_Main.FolderMonChange(Sender: TObject);
begin
  FolderChanged;
end; // FOLDERMON CHANGE

procedure TForm_Main.WMChangeCBChain(var Msg: TWMChangeCBChain);
begin
  with ClipCapMng do begin
     if Msg.Remove = ClipCapNextInChain then
        ClipCapNextInChain := Msg.Next
     else
       SendMessage( ClipCapNextInChain, WM_CHANGECBCHAIN, Msg.Remove, Msg.Next );
  end;
  Msg.Result := 0;
end; // WMChangeCBChain

procedure TForm_Main.WMDrawClipboard(var Msg: TWMDrawClipboard);
begin
  SendMessage( ClipCapMng.ClipCapNextInChain, WM_DRAWCLIPBOARD, 0, 0 );
  Msg.Result := 0;

  ClipCapMng.CheckPasteOnClipCap;
end; // WMDrawClipboard

procedure TForm_Main.MMNoteClipCaptureClick(Sender: TObject);
begin
  TB_ClipCap.Down := ( not TB_ClipCap.Down );
  ClipCapMng.Toggle (TB_ClipCap.Down, ActiveEditor);
end;

procedure TForm_Main.MMFilePrintClick(Sender: TObject);
begin
   ExportNotesEx (True);
end;

procedure TForm_Main.MMFilePageSetupClick(Sender: TObject);
begin
  try
     PageSetupDlg.Options:= [psoMargins];
     PageSetupDlg.Execute;

   except
     On E : Exception do
        App.ErrorPopup(E);
   end;
end;

procedure TForm_Main.MMNotePrintClick(Sender: TObject);
begin
   try
      PrintRtfFolder;
   except
     On E : Exception do
        App.ErrorPopup(E);
   end;
end;


procedure TForm_Main.MMNotePrintPreview_Click(Sender: TObject);
begin
  try
     PrintRtfFolder(true);

  except
     On E : Exception do
       App.ErrorPopup(E);
  end;
end;


procedure TForm_Main.MMEditCopyAllClick(Sender: TObject);
begin
  PerformCmd( ecSelectAll );
  PerformCmdEx( ecCopy );
end;

procedure TForm_Main.MMEditPasteAsNewFolderClick(Sender: TObject);
begin
  PasteIntoNew( true );
end;

procedure TForm_Main.MMEditPasteAsNewNodeClick(Sender: TObject);
begin
  PasteIntoNew( false );
end;

procedure TForm_Main.MMEditRot13Click(Sender: TObject);
begin
  PerformCmd( ecROT13 );
end;

procedure TForm_Main.MMNoteEmailClick(Sender: TObject);
{$IFNDEF EXCLUDEEMAIL}
var
  Form_Mail : TForm_Mail;
{$ENDIF}
begin
{$IFNDEF EXCLUDEEMAIL}
  if ( not HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;

  StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain31;
  Form_Mail := TForm_Mail.Create( self );
  try
    with Form_Mail do begin
      ShowHint := KeyOptions.ShowTooltips;
      myKntFolder := ActiveFolder;
      myKntFile := ActiveFile;
      myINI_FN := MailINI_FN;
    end;
    case Form_Mail.ShowModal of
      mrOK : StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain32;
      else
        StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain33;
    end;
  finally
    Application.OnException := ShowException;
    Form_Mail.Free;
  end;
{$ENDIF}
end; // MMEmailnoteClick


procedure TForm_Main.MMToolsDeduceDatesClick(Sender: TObject);
begin
   ActiveFile.TryToDeduceDates(false);
end;

procedure TForm_Main.MMToolsRemoveDatesClick(Sender: TObject);
begin
   if ActiveFile <> nil then
      ActiveFile.TryToDeduceDates(true);
end;



//  Alarm related actions ==================================

{$REGION Alarm related actions}

procedure TForm_Main.UpdateAlarmStatus;
var
   HasFolderAlarms, HasNodeAlarms: Boolean;
   NNode: TNoteNode;

begin
  HasNodeAlarms:= false;
  HasFolderAlarms:= false;

  if assigned(ActiveFolder) then begin
     NNode:= ActiveFolder.FocusedNNode;
     if assigned(NNode) then begin
        HasNodeAlarms:= AlarmMng.HasAlarms(nil, NNode, false);
        TVAlarmNode.Checked:= HasNodeAlarms;
     end;
     HasFolderAlarms:= AlarmMng.HasAlarms(ActiveFolder, nil, false);
  end;

  TB_SetAlarm.Down:= HasFolderAlarms or HasNodeAlarms;
  MMSetAlarm.Checked:= HasFolderAlarms or HasNodeAlarms;
end;


procedure TForm_Main.TB_AlarmModeClick(Sender: TObject);
begin
  if (GetKeyState(VK_CONTROL) < 0) then begin
     if assigned(ActiveFolder) then begin
         TB_AlarmMode.Down:= not KeyOptions.DisableAlarmPopup;
         AlarmMng.ShowAlarms(true);
         UpdateAlarmStatus;
     end;
  end
  else
     MMAlarmsPopupClick (nil);
end;


procedure TForm_Main.MMAlarmsPopupClick(Sender: TObject);
begin
   KeyOptions.DisableAlarmPopup:= not KeyOptions.DisableAlarmPopup;
   MMAlarmsPopup.Checked:= not KeyOptions.DisableAlarmPopup;
   TB_AlarmMode.Down:= (not KeyOptions.DisableAlarmPopup);
   TB_AlarmMode.Hint:= AlarmMng.GetAlarmModeHint;
end;


procedure TForm_Main.MMShowAlarmsClick(Sender: TObject);
begin
   if assigned(ActiveFolder) then begin
      AlarmMng.ShowAlarms(false);
      UpdateAlarmStatus;
   end;
end;


procedure TForm_Main.TB_AlarmModeMouseEnter(Sender: TObject);
begin
    AlarmMng.StopFlashMode;
end;


procedure TForm_Main.TB_SetAlarmClick(Sender: TObject);
begin
   SetAlarm (True);
end;

procedure TForm_Main.SetAlarm(ConsiderNoteAlarm: Boolean);
var
    NNode: TNoteNode;
    HasFolderAlarms, HasNodeAlarms: Boolean;

begin
   if not assigned(ActiveFile) or (not assigned(ActiveFolder)) then exit;

   HasFolderAlarms:= AlarmMng.HasAlarms(ActiveFolder, nil, false);
   HasNodeAlarms:= false;
   NNode:= ActiveFolder.FocusedNNode;
   if NNode <> nil then
      HasNodeAlarms:= AlarmMng.HasAlarms(nil, NNode, false);

   if ConsiderNoteAlarm and (ShiftDown or (HasFolderAlarms and not HasNodeAlarms)) then
      NNode:= nil;

   AlarmMng.EditAlarms (NNode, ActiveFolder, (GetKeyState(VK_CONTROL) < 0));
   UpdateAlarmStatus;
end;


procedure TForm_Main.TB_SetAlarmMouseEnter(Sender: TObject);
var
    NNode: TNoteNode;
    hint: string;
    sep: String;
    I, N: integer;
    Alarms: TAlarmList;

    procedure AddAlarmsToHint;
    begin
        I:= 0;
        while I <= Alarms.Count - 1 do begin
           hint:= hint + sep + FormatAlarmInstant(TAlarm(Alarms[i]).ExpirationDate) + ' [' + StringReplace(TAlarm(Alarms[i]).AlarmNote, #13#10, '. ', [rfReplaceAll]) + ']';
           sep:= ' // ';
           inc(I);
           inc(N);
        end;
    end;

begin
    if not assigned(ActiveFolder) then exit;

    sep:= '';
    hint:= '';
    N:= 0;
    NNode:= ActiveFolder.FocusedNNode;

    if assigned(NNode) and AlarmMng.HasAlarms(nil, NNode, false) then begin
       Alarms:= AlarmMng.GetAlarms(nil, NNode, false);
       AddAlarmsToHint;
    end;
    if AlarmMng.HasAlarms(ActiveFolder, nil, false) then begin
       Alarms:= AlarmMng.GetAlarms(ActiveFolder, nil, false);
       AddAlarmsToHint;
    end;

    if hint = '' then
       hint:= GetRS(sMain34)
    else
       hint:= '(' + N.ToString + ') ' + hint;


    TB_SetAlarm.Hint:= hint;
end;

{$ENDREGION}   //  Alarms ==================================


procedure TForm_Main.TB_ClipCapClick(Sender: TObject);
begin
  ClipCapMng.Toggle (TB_ClipCap.Down, ActiveEditor);
end;

procedure TForm_Main.MMViewAlphaTabsClick(Sender: TObject);
begin
  SortTabs;
end;


procedure TForm_Main.MMViewTabIconsClick(Sender: TObject);
begin
  TabOptions.Images := ( not TabOptions.Images );
  MMViewTabIcons.Checked := TabOptions.Images;
  if TabOptions.Images then
    Pages.Images := Chest.IMG_Categories
  else
    Pages.Images := nil;
end; // MMViewTabIconsClick


procedure TForm_Main.OnFileDropped( Sender : TObject; FileList : TStringList );
begin
    FileDropped( Sender, FileList );
end;


procedure TForm_Main.WMDropFiles(var Msg: TWMDropFiles);
var
  FileList : TStringList;
begin
  FileList := GetDropFiles(Msg.Drop);
  try
    FileDropped( self, FileList );

  finally
    FileList.Free;
  end;

end; // WMDropFiles


procedure TForm_Main.CreateWnd;
begin
  inherited;
  DragAcceptFiles( handle, true );
end;

procedure TForm_Main.DestroyWnd;
begin
  DragAcceptFiles( handle, false );
  inherited;
end;


procedure TForm_Main.MMTreeFocusToogleClick(Sender: TObject);
begin
  if not assigned(ActiveFolder) then exit;

  if ActiveFolder.TV.Focused then
     ActiveFolder.SetFocusOnNoteEditor
  else
     MMTreeFocusTreeClick (nil);
end;

procedure TForm_Main.MMTreeFocusEditorClick(Sender: TObject);
begin
  if not assigned(ActiveFolder) then exit;

  ActiveFolder.SetFocusOnNoteEditor
end;

procedure TForm_Main.MMTreeFocusTreeClick(Sender: TObject);
begin
  if not assigned(ActiveFolder) then exit;

  if ActiveFolder.TreeHidden then
     MMViewTreeClick(nil);

  ActiveFolder.SetFocusOnTree;
end;


procedure TForm_Main.MMTreeFullCollapseClick(Sender: TObject);
begin
  ActiveTreeUI.FullCollapse;
end;

procedure TForm_Main.MMTreeFullExpandClick(Sender: TObject);
begin
  ActiveTreeUI.FullExpand;
end;


procedure TForm_Main.MMViewNodeIconsClick(Sender: TObject);
var
  myFolder : TKntFolder;
  mi : TMenuItem;
begin
  if ( not ( sender is TMenuItem )) then exit;

  mi := ( sender as TMenuItem );
  myFolder := ActiveFolder;

  if assigned(myFolder) then begin
    if mi.Checked then
      myFolder.IconKind := niNone
    else
      myFolder.IconKind := TNodeIconKind( mi.Tag );

    myFolder.TreeUI.ShowOrHideIcons;
    UpdateFolderDisplay(true);
    ActiveFile.Modified := true;
  end;
end; // MMViewNodeIconsClick



procedure TForm_Main.MMViewCheckboxesAllNodesClick(Sender: TObject);
var
  myFolder : TKntFolder;
begin
  // show or hide checkboxes in active note's tree panel
  myFolder := ActiveFolder;

  if assigned(myFolder) then begin
    myFolder.Checkboxes := ( not myFolder.Checkboxes );
    MMViewCheckboxesAllNodes.Checked := myFolder.Checkboxes;
    myFolder.TreeUI.ShowOrHideCheckBoxes;
  end;
end;


procedure TForm_Main.MMToolsMergeClick(Sender: TObject);
begin
  MergeFromKNTFile( '' );
end;

procedure TForm_Main.MMMergeNotestoFileClick(Sender: TObject);
begin
  // MergeToKNTFile;
end;

{ FOLD / UNFOLD
  ToDo:
  - Exporting and folded blocks
  - Editing folded blocks -> control from RxRTFProtectChangeEx (knt.ui.editor)
  - Find All (or Find) with matches in collapsed text
  - Click on '+' link
  - Identifying start and end of a block to collapse from Ctrl+DblClick
  - Blocks and using Tags
  ...
}

procedure TForm_Main.RTFMFoldClick(Sender: TObject);
begin
   ActiveEditor.Fold (True);
end;

procedure TForm_Main.RTFMUnfoldClick(Sender: TObject);
begin
   ActiveEditor.Unfold;
end;

procedure TForm_Main.RTFMTagsClick(Sender: TObject);
begin
   ActiveFolder.NoteUI.EditTags;
end;


procedure TForm_Main.RTFMPlainTextClick(Sender: TObject);
begin
   if ActiveEditor.ReadOnly then begin
      App.WarnEditorIsReadOnly;
      exit;
   end;
   if not Assigned(ActiveEditor.NNodeObj) then exit;

   ActiveFile.TogglePlainText_RTF(ActiveFolder.NoteUI);
end;


procedure TForm_Main.RTFM_RTLClick(Sender: TObject);
begin
   if ActiveEditor.ReadOnly then begin
      App.WarnEditorIsReadOnly;
      exit;
   end;
   ActiveEditor.Paragraph.RTL:= not ActiveEditor.Paragraph.RTL;
end;


procedure TForm_Main.RTFMRestoreProportionsClick(Sender: TObject);
begin
   if Assigned(ActiveEditor.NNodeObj) then
      ActiveFolder.ReconsiderImageDimensionGoalsOnEditor (true)
   else
      ActiveEditor.ReconsiderImageDimensionGoals(true, imImage);        // Scratchpad
end;

procedure TForm_Main.RTFMWordWebClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.WordWebLookup;
end;

procedure TForm_Main.MMHelp_WhatisClick(Sender: TObject);
begin
  Perform( WM_SYSCOMMAND, SC_CONTEXTHELP, 0 );
end;

procedure TForm_Main.MathParserParseError(Sender: TObject;
  ParseError: Integer);
var
  Msg : string;
begin
  if ( not ( sender is TMathParser )) then exit;

  case ParseError of
    1 : Msg := GetRS(sMain56);
    2 : Msg := GetRS(sMain57);
    3 : Msg := GetRS(sMain58);
    4 : Msg := GetRS(sMain59);
    5 : Msg := GetRS(sMain60);
    6 : Msg := GetRS(sMain61);
    7 : Msg := GetRS(sMain62);
  end; { case }
  Msg := GetRS(sMain63) + Msg + #13 + GetRS(sMain64) + IntToStr(( sender as TMathParser ).Position);
  MessageDlg( Msg, mtError, [mbOk], 0 );
end; // MathParserParseError


procedure TForm_Main.MMEditPasteEvalClick(Sender: TObject);
begin
  if not App.CheckActiveEditorNotReadOnly then exit;

  ActiveEditor.SelText := LastEvalExprResult;
end;

procedure TForm_Main.MMFindNodeClick(Sender: TObject);
begin
  SearchNode_TextPrev := SearchNode_Text;
  SearchNode_Text := '';
  FindTreeNode;
end;

procedure TForm_Main.MMFindNodeNextClick(Sender: TObject);
begin
  FindTreeNode;
end;

procedure TForm_Main.FindTreeNode;
var
  myTreeNode : PVirtualNode;
  found : boolean;
  myFolder : TKntFolder;
  selectedNode : PVirtualNode;
  FindAllTabs: boolean;
  FindHiddenNodes: boolean;
  TreeUI: TKntTreeUI;
  TV: TVTree;

  procedure GetFirstNode;
  begin
	  myTreeNode := TV.GetFirst;
	  if assigned(myTreeNode) and not TV.IsVisible[myTreeNode] and (not FindHiddenNodes) then
		  myTreeNode := TV.GetNextNotHidden(myTreeNode);
  end;

  procedure GetNextNode();
  begin
	 if FindHiddenNodes then
      myTreeNode := TV.GetNext(myTreeNode)
	 else
  		myTreeNode := TV.GetNextNotHidden(myTreeNode);
  end;

  procedure GetNextFolder();
  var
     tabidx : integer;
  begin
     if FindAllTabs and (Form_Main.Pages.PageCount > 1) then begin
          tabidx := myFolder.TabSheet.PageIndex;
          if tabidx < pred(Form_Main.Pages.PageCount) then
             inc(tabidx)
          else
             tabidx := 0;

          myFolder := TKntFolder(Form_Main.Pages.Pages[tabidx].PrimaryObject);
          TreeUI:= TKntFolder(myFolder).TreeUI;
          TV:= TreeUI.TV;
          GetFirstNode;
     end;
   end;


begin

  myTreeNode := GetCurrentTreeNode;
  if not assigned(myTreeNode) then begin
    showmessage( GetRS(sMain65) );
    exit;
  end;

  // assigned(myTreeNode) -> ActiveFolder must be assigned

  if ( SearchNode_Text = '' ) then begin
    SearchNode_Text:= ActiveFolder.Editor.SelVisibleText;
    if ( SearchNode_Text = '' ) then
       SearchNode_Text := SearchNode_TextPrev;
    if InputQuery( GetRS(sMain66), GetRS(sMain67), SearchNode_Text ) then
       SearchNode_Text := AnsiLowercase( SearchNode_Text )
    else
       exit;
  end;

  if ( SearchNode_Text = '' ) then exit;

  found := false;
  myFolder := ActiveFolder;
  TreeUI:= TKntFolder(myFolder).TreeUI;
  TV:= TreeUI.TV;

  selectedNode:= myTreeNode;
  GetNextNode;

  FindAllTabs := CB_ResFind_AllNotes.Checked;
  FindHiddenNodes:= CB_ResFind_HiddenNodes.Checked;

  repeat
     if assigned(myTreeNode) then
     	  repeat
					if Pos(SearchNode_Text, AnsiLowercase(TreeUI.GetNNode(myTreeNode).NoteName)) > 0  then	begin
					  found := true;
            if (myFolder <> ActiveFolder) then
               App.ActivateFolder(myFolder);
            if TV.FocusedNode <> myTreeNode then begin
               TreeUI.MakePathVisible(myTreeNode);     // Could be hidden
               TreeUI.SelectAlone(myTreeNode);
            end;
					  break;
					end;
					GetNextNode;
			  until not assigned(myTreeNode) or (myTreeNode = selectedNode);

		 if not found and (myTreeNode <> selectedNode) then begin
	 	    GetNextFolder();
		    GetFirstNode();
		 end;

  until found or (myTreeNode = selectedNode);

  if ( not found ) or (myTreeNode = selectedNode) then
     statusbar.panels[PANEL_HINT].Text := GetRS(sMain68);

end; // FindTreeNode

procedure TForm_Main.CB_ResFind_AllNotesClick(Sender: TObject);
var
  AllNotes: Boolean;
begin
    AllNotes:= CB_ResFind_AllNotes.Checked;
    if AllNotes then
       CB_ResFind_CurrentNodeAndSubtree.Checked := False;

    CB_ResFind_CurrentNodeAndSubtree.Enabled:= not AllNotes;
end;

procedure TForm_Main.CB_ResFind_PathInNamesClick(Sender: TObject);
begin
  FindOptions.SearchPathInNodeNames:= CB_ResFind_PathInNames.Checked;
  TVFilterUsePath.Checked:= FindOptions.SearchPathInNodeNames;
  if ActiveTreeUI <> nil then
     ActiveTreeUI.ReapplyFilter;
end;

procedure TForm_Main.MMFormatLS1Click(Sender: TObject);
begin
  PerformCmd( ecSpace1 );
end;

procedure TForm_Main.MMFormatLS15Click(Sender: TObject);
begin
  PerformCmd( ecSpace15 );
end;

procedure TForm_Main.MMFormatLS2Click(Sender: TObject);
begin
  PerformCmd( ecSpace2 );
end;

procedure TForm_Main.MMEditRepeatClick(Sender: TObject);
begin
  RepeatLastCommand;
end;


procedure TForm_Main.MMEditReverseClick(Sender: TObject);
begin
  PerformCmd( ecReverseText );
end;

procedure TForm_Main.Btn_StyleClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
  begin
    LastStyleRange := TStyleRange(( sender as TMenuItem ).Tag );
    ( sender as TMenuItem ).Checked := true;
    // Btn_MakeStyle.ImageIndex := STYLE_IMAGE_BASE + ord( LastStyleRange );
    StyleCreate( LastStyleRange, nil );
  end;

end;


procedure TForm_Main.BtnStyleApplyClick(Sender: TObject);
begin
  if ( not Toolbar_Style.Visible ) then
  begin
    case messagedlg(
      GetRS(sMain69),
      mtConfirmation, [mbYes,mbNo], 0 ) of
      mrYes : begin
        // [x] this assumes .Visible is always synced with KeyOptions.ShowStyleToolbar
        // which SHOULD always be the case...
        MMViewTBStyleClick( MMViewTBStyle );
      end;
      else
        exit;
    end;
  end;
  if ( Combo_Style.ItemIndex < 0 ) then
  begin
    messagedlg( GetRS(sMain70), mtInformation, [mbOK], 0 );
    exit;
  end;
  if ( not assigned( StyleManager )) then
  begin
    messagedlg( GetRS(sMain71), mtError, [mbOK], 0 );
    exit;
  end;


  if ( sender is TMenuItem ) then
  begin
    case ( sender as TMenuItem ).Tag of
      ITEM_STYLE_APPLY : begin
        StyleApply( Combo_Style.Items[Combo_Style.ItemIndex] );
      end;
      ITEM_STYLE_RENAME : begin // rename style
        StyleRename( Combo_Style.Items[Combo_Style.ItemIndex] );
      end;
      ITEM_STYLE_DELETE : begin // delete style
        StyleDelete( Combo_Style.Items[Combo_Style.ItemIndex] );
      end;
      ITEM_STYLE_REDEFINE : begin // redefine style
        StyleRedefine;
      end;
      ITEM_STYLE_DESCRIBE : begin // describe style
        StyleDescribe( false, true );
      end;
    end;
  end
  else
  begin
    // Apply button was clicked
    StyleApply( Combo_Style.Items[Combo_Style.ItemIndex] );
  end;
end;


procedure TForm_Main.PagesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  i : integer;
begin
  if (Source is TBaseVirtualTree) then begin
     i := Pages.GetTabAt(X,Y);
     App.ActivateFolder(i);
     exit;
  end;
end;

procedure TForm_Main.PagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   ptCursor : TPoint;
begin
  if Button = mbRight then begin         // [dpv]
     GetCursorPos( ptCursor );
     Menu_TAB.Popup(ptCursor.x, ptCursor.y);
  end;

end;


procedure TForm_Main.MMInsertTermClick(Sender: TObject);
begin
   if App.CheckActiveEditor then
      ActiveEditor.ExpandTermProc;
end;


procedure TForm_Main.MMToolsGlosAddTermClick(Sender: TObject);
begin
  AddGlossaryTerm;
end;

procedure TForm_Main.Toolbar_FormatClose(Sender: TObject);
begin
  if ( sender is TToolbar97 ) then
  begin
    case ( sender as TToolbar97 ).Tag of
      1 : begin
        KeyOptions.ToolbarMainShow := false;
        MMViewTBMain.Checked := false;
      end;
      2 : begin
        KeyOptions.ToolbarFormatShow := false;
        MMViewTBFormat.Checked := false;
      end;
      3 : begin
        KeyOptions.ToolbarStyleShow := false;
        MMViewTBStyle.Checked := false;
      end;
      4 : begin
        KeyOptions.ToolbarTreeShow := false;
        MMViewTBTree.Checked := false;
      end;
      5 : begin
        KeyOptions.ToolbarInsertShow := false;
        MMViewTBInsert.Checked := false;
      end;
    end;
  end;

end;


procedure TForm_Main.MMEditTrimLeftClick(Sender: TObject);
begin
  if (sender is TMenuItem) and App.CheckActiveEditor then
     ActiveEditor.TrimBlanks((Sender as TMenuItem).Tag );
end;

procedure TForm_Main.MMEditCompressClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.CompressWhiteSpace;
end;

procedure TForm_Main.MMEditInvertCaseClick(Sender: TObject);
begin
  PerformCmd( ecInvertCase );
end;


procedure TForm_Main.MMInsertCharacterClick(Sender: TObject);
begin
   if App.CheckActiveEditor then
      ActiveEditor.InsertSpecialCharacter;
end;


procedure TForm_Main.MMFindBracketClick(Sender: TObject);
begin
  PerformCmdEx( ecMatchBracket );
end;

procedure TForm_Main.MMToolsStatisticsClick(Sender: TObject);
begin
   App.ShowStatistics;
end;


procedure TForm_Main.MMBkmJ9Click(Sender: TObject);
begin
  // Jump to bookmark
  if ( sender is TMenuItem ) then
  begin
    // See comment in MMBkmSet9Click
    //BookmarkGoTo( ord(( sender as TMenuItem ).Caption[2] ) - 48 );

    BookmarkGoTo( ord(TMenuItem(sender).Name[7]) - 48 );
  end;
end;

procedure TForm_Main.MMBkmSet9Click(Sender: TObject);
begin
  // Set bookmark
  if ( sender is TMenuItem ) then
  begin
    // This line only works after the "Set boomark" menu is first unfold, because only then Caption
    // changes from "1" to "&1" (e.g).  This behaviour is probably specific to TMenuItem.
    // So I will use the Name property instead of Caption (rather than asking for the last character of Caption)
    //BookmarkAdd( ord(( sender as TMenuItem ).Caption[2] ) - 48 );

    BookmarkAdd( ord(TMenuItem(sender).Name[9]) - 48 );
  end;
end;

procedure TForm_Main.MMFormatParagraphClick(Sender: TObject);
begin
  PerformCmd( ecParaDlg );
end;


procedure TForm_Main.MMInsertURLClick(Sender: TObject);
begin
  InsertURL('', '', ActiveEditor);   // Ask the user
end; // Insert URL


procedure TForm_Main.MMInsertLinkToFileClick(Sender: TObject);
begin
  InsertFileOrLink( '', true );
end;

procedure TForm_Main.MMInsertObjectClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.InsertPictureOrObject( false );
end;

procedure TForm_Main.MMInsertPictureClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.InsertPictureOrObject( true );
end;

procedure TForm_Main.WMJumpToKNTLink( var Msg : TMessage );
var
   UrlAction: TURLAction;
begin
  UrlAction:= TURLAction(Msg.WParam);
  JumpToKNTLocation( _GLOBAL_URLText, UrlAction );
end; // WMJumpToKNTLink

procedure TForm_Main.WMShowTipOfTheDay( var DummyMSG : integer );
begin
  App.ShowTipOfTheDay;
end; // WMShowTipOfTheDay


procedure TForm_Main.WMJumpToLocation( var DummyMSG : integer );
begin
  try
    if assigned( _Global_Location ) then begin
       if ( _Global_Location.FileName <> '' ) then begin
          if (( not FileExists( _Global_Location.Filename )) or
             ( KntFileOpen( _Global_Location.Filename ) <> 0 )) then begin
            App.DoMessageBox( Format(GetRS(sMain81),[_Global_Location.Filename] ), mtError, [mbOK] );
            exit;
          end;
       end;
       JumpToLocation( _Global_Location );
    end;

  finally
    _Global_Location := nil;
  end;
end; // WMJumpToLocation


procedure TForm_Main.MMEditPasteSpecialClick(Sender: TObject);
var
  FormatSelected: integer;
  HasImage: boolean;
  rtfText: String;
  FolderName: string;
  Editor: TKntRichEdit;

begin
  if not App.CheckActiveEditorNotReadOnly then exit;

  Editor:= ActiveEditor;

  if assigned(ActiveFolder) then
     FolderName:= ActiveFolder.Name
  else
     FolderName:= '';

  HasImage:= Clipboard.HasFormat(CF_BITMAP);
  if not HasImage then
     Clipboard.TryOfferRTF();

  try
     FormatSelected:= Editor.PasteSpecialDialog (false);      // Will paste the user selecton, except if it is Windows Bitmap

     Editor.BeginUpdate;
     try
         if FormatSelected= 5 then                                       // CF_BMP
            Editor.PasteBestAvailableFormat(FolderName, false, false, true)    // It will insert the image, using ImageManager
         else
            if HasImage and (FormatSelected= 0) and (Editor.SelLength = 1) then begin
               if (Editor.SelLength = 1) then begin
                   rtfText:= Editor.RtfSelText;
                   if not rtfText.Contains('{\object') and rtfText.Contains('\pict{') then
                      Editor.PasteBestAvailableFormat(FolderName, false, false, true);       // It will insert the image, using ImageManager
               end;
            end;
     finally
        Editor.EndUpdate;
     end;

  except
    On E : Exception do begin
       // ActiveFolder.Editor.PasteSpecialDialog can raise an exception if for example RTF format obtained converting HTML is not correct
       // See PasteBestAvailableFormatInEditor (kn_EditorUtils)
       MessageDlg( E.Message, mtError, [mbOK], 0 );
       exit;
    end;
  end;

end;

procedure TForm_Main.MMEditPlainDefaultPasteClick(Sender: TObject);
begin
   EditorOptions.PlainDefaultPaste:= not EditorOptions.PlainDefaultPaste;
   PlainDefaultPaste_Toggled
end;

procedure TForm_Main.MMP_PlainDefaultPasteClick(Sender: TObject);
begin
   EditorOptions.PlainDefaultPaste:= not EditorOptions.PlainDefaultPaste;
   PlainDefaultPaste_Toggled;
end;

procedure TForm_Main.PlainDefaultPaste_Toggled;
begin
    MMP_PlainDefaultPaste.Checked := EditorOptions.PlainDefaultPaste;
    MMEditPlainDefaultPaste.Checked := EditorOptions.PlainDefaultPaste;
end;


procedure TForm_Main.MMInsertFileContentsClick(Sender: TObject);
begin
  InsertFileOrLink( '', false );
end;

procedure TForm_Main.MMToolsGlosEditClick(Sender: TObject);
begin
  EditGlossaryTerms;
end;


procedure TForm_Main.MMHelpVisitWebsiteClick(Sender: TObject);
begin
  if messagedlg(GetRS(sMain82), mtConfirmation, [mbOK,mbCancel], 0) <> mrOK then exit;

  screen.Cursor := crHourGlass;
  ShellExecute( 0, 'open', PChar( Program_URL ), nil, nil, SW_NORMAL );
  screen.Cursor := crDefault;
end;

//procedure TForm_Main.MMHelpEmailAuthorClick(Sender: TObject);
//begin
//  if messagedlg('',
//    mtConfirmation, [mbOK,mbCancel], 0
//    ) <> mrOK then exit;
//  screen.Cursor := crHourGlass;
//  ShellExecute( 0, 'open', PChar( 'mailto:' + Program_Email ), nil, nil, SW_NORMAL );
//  screen.Cursor := crDefault;
//end;


procedure TForm_Main.MMInsertMarkLocationClick(Sender: TObject);
begin
  InsertOrMarkKNTLink( nil, false, '' );
end;

procedure TForm_Main.MMInsertKNTLinkClick(Sender: TObject);
begin
  InsertOrMarkKNTLink( nil, true, ActiveEditor.SelVisibleText);
end;


procedure TForm_Main.Combo_StyleKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = #13 ) then
  begin
    key := #0;
    BtnStyleApplyClick( TB_Style );
  end;
end;

procedure TForm_Main.Toolbar_StyleRecreating(Sender: TObject);
begin
  Combo_Style.Tag := Combo_Style.ItemIndex;
end;

procedure TForm_Main.Toolbar_StyleRecreated(Sender: TObject);
begin
  // when toolbar changes dock, combo loses its ItemIndex.
  // This is how we fix that.
  if ( Combo_Style.Items.Count > 0 ) then
  begin
    if (( Combo_Style.Tag > -1 ) and ( Combo_Style.Tag < Combo_Style.Items.Count )) then
      Combo_Style.ItemIndex := Combo_Style.Tag
    else
      Combo_Style.ItemIndex := 0;
  end;
  Combo_Style.Tag := -1;
end;

procedure TForm_Main.MMToolsPluginRunClick(Sender: TObject);
begin
  ExecutePlugin( '' );
end;

procedure TForm_Main.MMToolsPluginRunLastClick(Sender: TObject);
begin
  if ( LastPluginFN <> '' ) then
    ExecutePlugin( LastPluginFN );
end;

procedure TForm_Main.MacMMacro_EditClick(Sender: TObject);
begin
  EditMacro( false );
end;

procedure TForm_Main.TB_MacroClick(Sender: TObject);
begin
  if IsRunningMacro then
    MacroAbortRequest := true
  else
    ExecuteMacro( '', '' );
end;

procedure TForm_Main.MMToolsMacroRunLastClick(Sender: TObject);
begin
  ExecuteMacro( LastMacroFN, '' );
end;

procedure TForm_Main.MacMMacro_DeleteClick(Sender: TObject);
begin
  DeleteMacro;
end;

procedure TForm_Main.TB_MacroRecordClick(Sender: TObject);
begin
  RecordMacro;
end;

procedure TForm_Main.TB_MacroPauseClick(Sender: TObject);
begin
  if IsRecordingMacro then
    PauseRecordingMacro
  else
    TB_MacroPause.Down := false;
end;

procedure TForm_Main.MacMMacroUserCommandClick(Sender: TObject);
begin
  AddUserMacroCommand;
end;

procedure TForm_Main.Combo_MacroClick(Sender: TObject);
var
  Macro : TMacro;
  index: integer;
begin
  ListBox_ResMacro.Hint := '';
  Macro := GetCurrentMacro( false, index);
  if assigned( macro ) then begin
    StatusBar.Panels[PANEL_HINT].Text := Macro.Description;
    ListBox_ResMacro.Hint := Macro.Description;
  end
  else
    StatusBar.Panels[PANEL_HINT].Text := '';
end;

procedure TForm_Main.Combo_MacroKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = #13 ) then
  begin
    key := #0;
    TB_MacroClick( TB_Macro );
  end;
end;

procedure TForm_Main.MMToolsMacroSelectClick(Sender: TObject);
begin
  ExecuteMacroFile;
end;

procedure TForm_Main.MMViewTBHideAllClick(Sender: TObject);
var
  DoShow : boolean;
begin
  if ( sender is TMenuItem ) then
  begin
    DoShow := (( sender as TMenuItem ).Tag > 0 );
    if ( MMViewTBMain.Checked <> DoShow ) then MMViewTBMainClick( MMViewTBMain );
    if ( MMViewTBFormat.Checked <> DoShow ) then MMViewTBFormatClick( MMViewTBFormat );
    if ( MMViewTBStyle.Checked <> DoShow ) then MMViewTBStyleClick( MMViewTBStyle );
    if ( MMViewTBTree.Checked <> DoShow ) then MMViewTBTreeClick( MMViewTBTree );
    if ( MMViewTBInsert.Checked <> DoShow ) then MMViewTBInsertClick( MMViewTBInsert );
  end;
end;

procedure TForm_Main.MMTreeSaveToFileClick(Sender: TObject);
var
  fn, oldFilter : string;
begin
  if ( not HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;

  ShowHiddenMarkers:= CtrlDown;

  with SaveDlg do begin
    oldFilter := Filter;
    Filter := FILTER_TEXTFILES;
    FilterIndex := 1;
    Title := GetRS(sMain72);
    Options := Options - [ofAllowMultiSelect];
  end;

  try
    if SaveDlg.Execute then begin
      fn := normalFN( SaveDlg.Filename );
      ActiveTreeUI.SaveToFile(fn);
    end;
  finally
    ShowHiddenMarkers:= false;
    SaveDlg.Filter := oldFilter;
  end;

end;


procedure TForm_Main.MMHelpKeyboardRefClick(Sender: TObject);
begin
  ActiveKeyNoteHelp(18);  // Keyboard Reference [18]
  //HtmlHelp(0, PAnsiChar(Application.HelpFile), HH_HELP_CONTEXT, 30);
end;

procedure TForm_Main.MMHelpMainClick(Sender: TObject);
begin
  ActiveKeyNoteHelp(2);  // Welcome to KeyNote NF [2]
  //HtmlHelp(0, PAnsiChar(Application.HelpFile), HH_HELP_CONTEXT, 10);
end;

procedure TForm_Main.MMToolsTemplateCreateClick(Sender: TObject);
begin
  CreateTemplate;
end;


procedure TForm_Main.MMToolsTemplateInsertClick(Sender: TObject);
begin
  InsertTemplate( '' );
end;


procedure TForm_Main.MMViewTreeClick(Sender: TObject);
begin
  if assigned( ActiveFolder) then begin
    ActiveFolder.TreeHidden := ( not ActiveFolder.TreeHidden );
    UpdateTreeVisible( ActiveFolder);
  end;
end;

procedure TForm_Main.MMViewEditorInfoPanelClick(Sender: TObject);
var
   i: integer;
   PanelHiddenInActiveFolder: boolean;
begin
  if ActiveFolder = nil then exit;

  PanelHiddenInActiveFolder:= ActiveFolder.EditorInfoPanelHidden;

  if CtrlDown then begin
     for i := 0 to ActiveFile.Folders.Count -1 do
        ActiveFile.Folders[i].EditorInfoPanelHidden:= not PanelHiddenInActiveFolder;
  end
  else
     ActiveFolder.EditorInfoPanelHidden := not PanelHiddenInActiveFolder;
end;


procedure TForm_Main.MMFormatLanguageClick(Sender: TObject);
begin
  PerformCmd( ecLanguage );
end;

procedure TForm_Main.MMNoteSpellClick(Sender: TObject);
begin
   if App.CheckActiveEditor then
      ActiveEditor.RunSpellchecker;
end;

procedure TForm_Main.Markaslink1Click(Sender: TObject);
begin
  with ActiveEditor.SelAttributes do
    Link := ( not Link );
end;

procedure TForm_Main.Hiddentext1Click(Sender: TObject);
begin
  with ActiveEditor.SelAttributes do
    Hidden := ( not Hidden );
end;



procedure TForm_Main.MMViewResPanelClick(Sender: TObject);
begin
  if (Sender = nil) and KeyOptions.ResPanelShow and assigned(ActiveFolder) then begin
     if ActiveFolder.Editor.Focused or ActiveTreeUI.Focused then begin
        FocusResourcePanel;
        ActiveTreeUI.CheckRestoreTreeWidth;
        exit;
     end;
  end;

  KeyOptions.ResPanelShow := ( not KeyOptions.ResPanelShow );

  UpdateResPanelContents (true);
  HideOrShowResPanel( KeyOptions.ResPanelShow );
  MMViewResPanel.Checked := KeyOptions.ResPanelShow;
  if KeyOptions.ResPanelShow then
    ResMHidepanel.Caption := GetRS(sMain83)
  else
    ResMHidepanel.Caption := GetRS(sMain84);
  TB_ResPanel.Down := MMViewResPanel.Checked;
  if KeyOptions.ResPanelShow then
    FocusResourcePanel
  else
    FocusActiveKntFolder;

  if ActiveTreeUI <> nil then
     ActiveTreeUI.CheckRestoreTreeWidth;
end; // MMViewResPanelClick


procedure TForm_Main.ListBox_ResMacroDblClick(Sender: TObject);
begin
  TB_MacroClick( TB_Macro );
end;

procedure TForm_Main.ListBox_ResTplDblClick(Sender: TObject);
var
  i : integer;
begin
  i := ListBox_ResTpl.ItemIndex;
  if ( i < 0 ) then exit;
  InsertTemplate( Template_Folder + ListBox_ResTpl.Items[i] );
end; // ListBox_ResTplDblClick

procedure TForm_Main.TPLMTplDeleteClick(Sender: TObject);
begin
  RemoveTemplate;
end;

procedure TForm_Main.TPLMTplInsertClick(Sender: TObject);
begin
  ListBox_ResTplDblClick( ListBox_ResTpl );
end;

procedure TForm_Main.Splitter_ResMoved(Sender: TObject);
begin
  UpdateFindAllResultsWidth;

  if assigned(Res_RTF) and (Pages_Res.ActivePage = ResTab_RTF) then
     Res_RTF.Refresh;

  if KeyOptions.AltMargins and assigned(ActiveFolder) then
      ActiveFolder.Editor.Refresh;
end;

procedure TForm_Main.VisibilityControlsFindAllResults (Visible: boolean);
begin
  Btn_ResFind_Prev.Visible := Visible;
  Btn_ResFind_Next.Visible := Visible;
  LblFindAllNumResults.Visible:= Visible;
  Pages_Res.ShowHint:= not Visible;
end;

procedure TForm_Main.Btn_ResFlipClick(Sender: TObject);
var
  IsShownOptions: boolean;
begin
  IsShownOptions:= (Ntbk_ResFind.PageIndex = 0);
  if IsShownOptions then begin
    Btn_ResFlip.Caption := GetRS(sMain85);
    Ntbk_ResFind.PageIndex := 1;
  end
  else begin
    Btn_ResFlip.Caption := GetRS(sMain86);
    Ntbk_ResFind.PageIndex := 0;
  end;

  VisibilityControlsFindAllResults (not IsShownOptions);
end;

procedure TForm_Main.ShowFindAllOptions;
begin
   if not KeyOptions.ResPanelShow then
      MMViewResPanelClick( MMViewResPanel );

   Pages_Res.ActivePage:= ResTab_Find;
   if (Ntbk_ResFind.PageIndex = 0) then
       Btn_ResFlipClick(nil);

   FocusResourcePanel;
end;


procedure TForm_Main.MMEditSelectWordClick(Sender: TObject);
begin
  PerformCmdEx( ecSelectWord );
end;

procedure TForm_Main.Pages_ResChange(Sender: TObject);
begin
  UpdateResPanelContents (false);
  if KeyOptions.ResPanelShow then
    FocusResourcePanel;
end; // Pages_ResChange

procedure TForm_Main.PLM_ReloadPluginsClick(Sender: TObject);
begin
  ListBox_ResPlugins.Items.Clear;
  EnumeratePlugins;
  DisplayPlugins;
end;

procedure TForm_Main.ListBox_ResPluginsClick(Sender: TObject);
begin
  ShowPluginInfo;
end;


procedure TForm_Main.PLM_RunPluginClick(Sender: TObject);
begin
  ExecutePlugin( '' );
end; // RunPlugin

procedure TForm_Main.PLM_ConfigurePluginClick(Sender: TObject);
begin
  ConfigurePlugin( '' );
end; // ConfigurePlugin


//======================================================================================== TAGS


procedure TForm_Main.SetupTagsTab;
begin
   txtFilterTags.OnChange:= txtFilterTagsChange;

   with TVTags do begin
     OnGetCellText:= TVTags_GetCellText;
     OnPaintText:= TVTags_PaintText;
     OnChange:= TVTags_Change;

     OnKeyDown:= TVTags_KeyDown;
     OnEditing := TVTags_Editing;
     OnCreateEditor:= TVTags_CreateEditor;
     OnEdited := TVTags_Edited;
     OnNewText:= TVTags_NewText;

     TreeOptions.PaintOptions := [];

     TreeOptions.MiscOptions := [toFullRepaintOnResize, toInitOnSave, toWheelPanning, toEditOnClick, toEditable, toGridExtensions];
     TreeOptions.SelectionOptions := [toMultiSelect, toRightClickSelect, toAlwaysSelectNode, toSelectNextNodeOnRemoval, toExtendedFocus, toSyncCheckboxesWithSelection];
     TreeOptions.StringOptions := [toAutoAcceptEditChange, toShowStaticText];

     CheckImageKind:= ckCustom;
     HintMode:= hmTooltip;
     IncrementalSearch:= isVisibleOnly;
     PopupMenu:= Menu_Tags;
   end;

   with TVTags.Header do begin
       Options:= [hoAutoResize,hoAutoSpring,hoColumnResize,hoDblClickResize,hoShowHint, hoVisible];
       if Height < 18 then
          Height:= 18;
       AutoSizeIndex:= 1;
       with Columns.Add do begin
           Position:= 0;
           Text:= GetRS(sTag1);       // Name
           Options:= Options + [coVisible];
       end;
       with Columns.Add do begin
           Text:= GetRS(sTag2);       // Description / alias
           Position:= 1;
           Options:= Options + [coVisible, coSmartResize, coAutoSpring];
       end;
       MainColumn:= 0;
   end;
   TVTags.Header.Columns[0].Width:= 100;
end;


procedure TForm_Main.TVTags_GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  NTag: TNoteTag;
begin
   //if not KntTreeOptions.ShowTooltips then exit;
   NTag:= ActiveFile.NoteTagsSorted[Node.Index];
   case Column of
     -1, 0: HintText:= NTag.Name;
         1: HintText:= NTag.Description;
    end;
end;


procedure TForm_Main.TVTags_GetCellText(Sender: TCustomVirtualStringTree; var E: TVSTGetCellTextEventArgs);
var
  NTag: TNoteTag;
  iTag: integer;
begin
   if (ActiveFile = nil) or (ActiveFile.NoteTags=nil) then exit;

   NTag:= ActiveFile.NoteTagsSorted[E.Node.Index];
   case E.Column of
     -1, 0: begin
              E.CellText:= NTag.Name;
              if (ActiveTreeUI <> nil) and (ActiveTreeUI.TagsInUse.Count > 0) then begin
                 iTag:= ActiveTreeUI.TagsInUse.IndexOf(NTag);
                 if iTag >= 0 then
                    E.StaticText := Format('(%d)', [ActiveTreeUI.TagsNumberOfUses[iTag]]);
              end;
            end;
         1: E.CellText:= NTag.Description;
    end;
end;


procedure TForm_Main.TVTags_PaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  Color: TColor;
  NTag: TNoteTag;
  txt: string;
  iTag: integer;
  ShowTagsUse: boolean;
begin
   ShowTagsUse:= (ActiveTreeUI <> nil) and (ActiveTreeUI.TagsInUse.Count > 0);
   if (TVTPaintOption.toShowFilteredNodes in TVTags.TreeOptions.PaintOptions) and
      (ActiveFile.NoteTagsTemporalAdded.Count = 0) and
      (not ShowTagsUse) then exit;

   if (TextType = ttStatic) and not ShowTagsUse then exit;

   if (ActiveFile = nil) or (ActiveFile.NoteTagsSorted = nil) then exit;
   if Sender.Selected[Node] and (Column <= 0) then exit;

   txt:= txtFilterTags.Text;

   NTag:= ActiveFile.NoteTagsSorted[Node.Index];

   case TextType of
      ttStatic:
         if ShowTagsUse then begin
            TargetCanvas.Font.Color := clGray;
            TargetCanvas.Font.Size := 8;
         end;

      ttNormal:
         if (ActiveFile.NoteTagsTemporalAdded.Count > 0) and (ActiveFile.NoteTagsTemporalAdded.IndexOf(NTag) >= 0) then
             TargetCanvas.Font.Color := clGreen
         else
         if not AnsiStartsText(txt, NTag.Name) then
            TargetCanvas.Font.Color := $FF4D4D;
   end;

end;


procedure TForm_Main.txtFilterTagsChange(Sender: TObject);
begin
   CheckFilterTags;
end;

procedure TForm_Main.CheckFilterTags;
var
  Node: PVirtualNode;
  NTag: TNoteTag;
  txt: String;
  Filtered: boolean;
  ShowUseOfTags, ModeOR: boolean;
  iTag: integer;
begin
   if TVTags.TotalCount = 0 then exit;

   txt:= txtFilterTags.Text;

   ShowUseOfTags:= (App.TagsState = tsVisible) and (ActiveTreeUI <> nil) and ActiveTreeUI.ShowUseOfTags;
   ModeOR:= (ActiveTreeUI <> nil) and ActiveTreeUI.TagsFilterModeOR;


   TVTags.BeginUpdate;

   if (txt <> '') or ShowUseOfTags then begin
      for Node in TVTags.Nodes() do begin
         NTag:= ActiveFile.NoteTagsSorted [Node.Index];
         Filtered:= false;
         if txt <> '' then
            Filtered:= not ( ((Length(txt) >= 1) and AnsiStartsText(txt, NTag.Name)) or
                             ((Length(txt) >= 3) and (AnsiContainsText(NTag.Name, txt) or AnsiContainsText(NTag.Description, txt) ))  );

         if not Filtered and ShowUseOfTags then begin
            if ModeOR then begin
               iTag:= ActiveTreeUI.TagsNonUsed.IndexOf(NTag);
               if (iTag >= 0) then
                  Filtered:= True;
            end
            else begin
               iTag:= ActiveTreeUI.TagsInUse.IndexOf(NTag);
               if (iTag < 0) then
                  Filtered:= True;
            end;
         end;
         TVTags.IsFiltered[Node]:= Filtered;
      end;
      TVTags.TreeOptions.PaintOptions := TVTags.TreeOptions.PaintOptions - [TVTPaintOption.toShowFilteredNodes]
   end
   else
      TVTags.TreeOptions.PaintOptions := TVTags.TreeOptions.PaintOptions + [TVTPaintOption.toShowFilteredNodes];

   if not chkFilterOnTags.Checked then
       TVTagsSelectAlone(TVTags.GetFirstVisible());
   TVTags.EndUpdate;
end;


procedure TForm_Main.TVTags_KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ptCursor : TPoint;
begin
  if TVTags.IsEditing then exit;

  case key of
    VK_F10 : if ( shift = [ssShift] ) then begin
      key := 0;
      GetCursorPos( ptCursor );
      TVTags.PopupMenu.Popup(ptCursor.X, ptCursor.Y);
    end;

    VK_SPACE, VK_F2 : if ( Shift = [] ) then begin
      key := 0;
      RenameTag;
    end;
  end;

end;

procedure TForm_Main.TVTags_Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
   if fChangingInCode or not chkFilterOnTags.Checked then exit;
   if ActiveTreeUI = nil then exit;

   RefreshFilterOnTags;
end;


procedure TForm_Main.TVTagsSelectAlone(Node: PVirtualNode);
begin
   TVTags.FocusedNode:= Node;
   TVTags.ClearSelection;
   TVTags.Selected[Node] := True;
end;


procedure TForm_Main.TagsMCreateClick(Sender: TObject);
var
   TagName: string;
   Node: PVirtualNode;
   NTag: TNoteTag;
begin
   if InputQuery( GetRS(sTag6), GetRS(sTag1), TagName ) then begin
      TagName := trim(TagName);
      if TagMng.IsValidTagName(TagName) then begin
         ActiveFile.AddNTag(TagName, '');
         for Node in TVTags.Nodes() do begin
            NTag:= ActiveFile.NoteTagsSorted [Node.Index];
            if NTag.Name = TagName then begin
               TVTagsSelectAlone(Node);
               exit;
            end;
         end;
      end
      else
         App.ErrorPopup(GetRS(sTag3));
   end;
end;

procedure TForm_Main.TagsMDelClick(Sender: TObject);
var
  Node: PVirtualNode;
  SelectedTags: TNoteTagArray;
  NTag: TNoteTag;
  i: integer;
  MsgRemovingRef: string;
  RemoveRefInNotesText: integer;

  function CheckRemovingReferencesInNotesText: integer;
  begin
     if RemoveRefInNotesText = mrNo then
        Result:= App.DoMessageBox(GetRS(sTag10), mtConfirmation, [mbYes,mbNo,mbCancel]);
  end;

begin
  if (ActiveFile = nil) or ActiveFile.ReadOnly then exit;
  if TVTags.SelectedCount = 0 then exit;

  {
   Depending on a new option (to be implemented), AutoDiscoverTags, you may be asked whether to also remove,
   from the text of existing notes, all references to deleted tags.

   If AutoDiscoverTags=True, it makes no sense to ask whether or not to remove references within the text of notes,
   as they would be automatically added back when saving the notes. They will therefore be removed, and this will
   be indicated along with the initial confirmation on whether to delete the tag.
   In the case that AutoDiscoverTags=False KeyNote will ask for confirmation. It could be that we do not want to
   remove references (uses) to tags already applied, which could still be searched for.
   In fact, it could be a way to make it easier to find certain 'sensitive' information, perhaps hidden in folded
   text, tagged with a tag not included in the visible list of tags.
  }
  MsgRemovingRef:= '';
  RemoveRefInNotesText:= mrNo;
  {
  if KeyOptions.AutoDiscoverTags then begin
     MsgRemovingRef:= GetRS(sTag9);
     RemoveRefInNotesText:= mrYes;
  end;
  }

  if TVTags.SelectedCount = 1 then begin
     Node:= TVTags.FocusedNode;
     if Node = nil then exit;
     NTag:= ActiveFile.NoteTagsSorted[Node.Index];
     if (App.DoMessageBox(Format(GetRS(sTag4), [NTag.Name, MsgRemovingRef]), mtWarning, [mbYes,mbNo]) <> mrYes) then exit;
  end
  else
  if (App.DoMessageBox(Format(GetRS(sTag5), [MsgRemovingRef]), mtWarning, [mbYes,mbNo]) <> mrYes) then exit;

  RemoveRefInNotesText:= CheckRemovingReferencesInNotesText();
  if RemoveRefInNotesText = mrCancel then exit;

  SelectedTags:= GetTagsSelected;
  App.TagsState := tsHidden;
  TVTags.BeginUpdate;
  try
     ActiveFile.DeleteNTagsReferences(SelectedTags, (RemoveRefInNotesText=mrYes));
     for i := High(SelectedTags) downto 0 do
        ActiveFile.DeleteNTag(SelectedTags[i]);

  finally
     App.TagsState := tsVisible;
     App.TagsUpdated;
     TVTags.ClearSelection;
     TVTags.EndUpdate;
  end;


  for i:= 0 to ActiveFile.Folders.Count-1 do
     ActiveFile.Folders[i].NoteUI.RefreshTags;

  TVTags.SetFocus;
end;


procedure TForm_Main.TagsMEditClick(Sender: TObject);
begin
   RenameTag;
end;


procedure TForm_Main.TagsMExportClick(Sender: TObject);
begin
   if (ActiveFile <> nil) and (ActiveFile.NoteTags.Count > 0) then
      TagMng.ExportTagsSelected(GetTagsSelected);
end;


procedure TForm_Main.TagsMImportClick(Sender: TObject);
begin
   if (ActiveFile <> nil) then
      TagMng.ImportTagsFromFile('', True);
end;

function CheckConfirmationAddOrRemoveTags(Add: boolean): boolean;
var
 NumTags, NumNotes: integer;
 SelOrVis: string;
 Msg: string;
begin
  Result:= False;
  if (ActiveTreeUI = nil) or ActiveTreeUI.CheckReadOnly then exit;

  NumTags:= Form_Main.TVTags.SelectedCount;

  if NumTags = 0 then
     App.InfoPopup(GetRS(sTag11))

  else begin
     NumNotes:= ActiveTreeUI.TV.SelectedCount;
     if NumNotes = 0 then begin
        NumNotes := ActiveTreeUI.TV.VisibleCount;
        SelOrVis:= GetRS(sTag14);  // visible
     end
     else
        SelOrVis:= GetRS(sTag15);  // selected

     if Add then
        Msg:= GetRS(sTag12)   // OK to APPLY/ADD the %n selected Tags to the %n %s NOTES?
     else
        Msg:= GetRS(sTag13);  // OK to REMOVE the %n selected Tags from the %n %s NOTES?

     if (App.DoMessageBox(Format(Msg, [NumTags, NumNotes, SelOrVis]), mtWarning, [mbYes,mbNo]) = mrYes) then
        Result:= true;

  end;
end;

function TForm_Main.GetTagsSelected: TNoteTagArray;
var
   SelectedTagsInTV: TNodeArray;
   i: integer;
begin
  SetLength(Result, TVTags.SelectedCount);
  SelectedTagsInTV:= TVTags.GetSortedSelection(False);
  for i:= 0 to High(SelectedTagsInTV) do
     Result[i]:= ActiveFile.NoteTagsSorted[SelectedTagsInTV[i].Index];

end;

procedure TForm_Main.TagsMAddOrRemove(Add: boolean);
var
  SelectedTagsInTV, SelectedNodes: TNodeArray;
  SelectedTags: TNoteTagArray;
  Node: PVirtualNode;
  i: integer;

  procedure processNode (Node: PVirtualNode);
  var
    NNode: TNoteNode;
    NEntry: TNoteEntry;
  begin
     NNode:= ActiveTreeUI.GetNNode(Node);
     NEntry:= NNode.Note.Entries[0];                          //%%%
     if Add then
        NEntry.AddTags(SelectedTags)
     else
        NEntry.RemoveTags(SelectedTags);
  end;

begin
  if not CheckConfirmationAddOrRemoveTags(Add) then exit;

  SelectedTags:= GetTagsSelected;

  if ActiveTreeUI.TV.SelectedCount > 0 then begin
     SelectedNodes:= ActiveTreeUI.TV.GetSortedSelection(False);
     for i:= 0 to High(SelectedNodes) do
        ProcessNode(SelectedNodes[i]);
  end
  else begin
     for Node in ActiveTreeUI.TV.VisibleNodes() do
        ProcessNode(Node);
  end;

  ActiveFolder.NoteUI.RefreshTags;
end;


procedure TForm_Main.CheckFilterOnTags(RecoveringTagsSituation: boolean);
var
  FilterOnTags: boolean;
  ModeOR: boolean;

  procedure ShowCheckboxes(show: boolean);
  var
    Node: PVirtualNode;
  begin
     Node:= TVTags.GetFirst;
     while Node <> nil do begin
        if Show then begin
           Node.CheckType:= ctCheckBox;
           Node.CheckState:= csUncheckedNormal;
        end
        else
           Node.CheckType:= ctNone;

        Node:= TVTags.GetNext(Node);
     end;
  end;

  procedure RecoverTagsSelection;
  var
     i, iTag: integer;
     FindTags: TFindTags;
     NTag: TNoteTag;
     Node: PVirtualNode;

     procedure SelectTag;
     begin
        Node:= TVTags.GetFirst;
        while (Node <> nil) and (Node.Index <> iTag) do
           Node:= TVTags.GetNext(Node);

        if Node <> nil then
           TVTags.Selected[Node]:= True;
     end;

   begin
     FindTags:= ActiveTreeUI.FindTags;
     if FindTags <> nil then
        if ModeOR then begin
           for i:= 0 to High(FindTags[0]) do begin
              NTag := FindTags[0][i];
              iTag:= ActiveFile.NoteTagsSorted.IndexOf(NTag);
              if iTag >= 0 then
                 SelectTag;
           end;
        end
        else begin
           for i:= 0 to High(FindTags) do begin
              NTag := FindTags[i][0];
              iTag:= ActiveFile.NoteTagsSorted.IndexOf(NTag);
              if iTag >= 0 then
                 SelectTag;
           end;
        end;

   end;

begin
   if (ActiveTreeUI = nil) or fChangingInCode then exit;

   fChangingInCode:= True;

   TVTags.BeginUpdate;

   if RecoveringTagsSituation then begin
     chkFilterOnTags.Checked:= ActiveTreeUI.ShowUseOfTags;
     ModeOR:= ActiveTreeUI.TagsFilterModeOR;
     if ModeOR then
        cbTagFilterMode.ItemIndex:= 1
     else
        cbTagFilterMode.ItemIndex:= 0;
   end;

   FilterOnTags:= chkFilterOnTags.Checked;
   ActiveTreeUI.ShowUseOfTags:= FilterOnTags;


   if FilterOnTags then begin
      TVTags.TreeOptions.MiscOptions := TVTags.TreeOptions.MiscOptions - [toEditOnClick] + [toCheckSupport];
      TVTags.TreeOptions.SelectionOptions := TVTags.TreeOptions.SelectionOptions - [toAlwaysSelectNode];
      TVTags.ClearSelection;
      ShowCheckboxes (True);
      if RecoveringTagsSituation then
         RecoverTagsSelection;

      if not RecoveringTagsSituation or (ActiveTreeUI.UseOfTagsCalcWithInheritedTags <> FindOptions.InheritedTags) then
         FilterNotesOnTagsSelection;
   end
   else begin
      with ActiveTreeUI do begin
         TagsInUse.Clear;
         TagsNumberOfUses.Clear;
         if Trim(txtTags.Text) = '' then begin
            txtTags.Text := EMPTY_TAGS;
            txtTags.Font.Color:= clGray;
         end;
         ActiveTreeUI.AdjustTxtTagsWidth;
      end;
      TVTags.TreeOptions.MiscOptions := TVTags.TreeOptions.MiscOptions + [toEditOnClick] - [toCheckSupport];
      TVTags.TreeOptions.SelectionOptions := TVTags.TreeOptions.SelectionOptions + [toAlwaysSelectNode];
      ShowCheckboxes (False);
   end;

   cbTagFilterMode.Enabled:= FilterOnTags;
   lblTg.Enabled:= FilterOnTags;
   lblTg2.Enabled:= FilterOnTags;

   CheckFilterTags;

   TVTags.EndUpdate;
   fChangingInCode:= False;
end;


procedure TForm_Main.RefreshFilterOnTags;
begin
   fChangingInCode:= True;
   try
      FilterNotesOnTagsSelection;
      CheckFilterTags;
   finally
      fChangingInCode:= False;
   end;

end;


procedure TForm_Main.chkFilterOnTagsClick(Sender: TObject);
begin
   CheckFilterOnTags(False);
end;


procedure TForm_Main.cbTagFilterModeChange(Sender: TObject);
begin
   if (ActiveTreeUI = nil) or fChangingInCode then exit;

   ActiveTreeUI.TagsFilterModeOR:= (cbTagFilterMode.ItemIndex=1);

   RefreshFilterOnTags;
end;


procedure TForm_Main.chkInhTagsClick(Sender: TObject);
begin
  if Initializing or fChangingInCode then exit;
  FindOptions.InheritedTags:= not FindOptions.InheritedTags;
  TVFilterInhTags.Checked:= FindOptions.InheritedTags;
  chkInhTagsFind.Checked:= FindOptions.InheritedTags;

  if ActiveTreeUI = nil then exit;

  if ActiveTreeUI.ShowUseOfTags then begin
     FilterNotesOnTagsSelection;
     if (App.TagsState = tsVisible) then
        TVTags.Invalidate;
  end
  else
     ActiveTreeUI.ReapplyFilter;
end;

procedure TForm_Main.TagsMAddClick(Sender: TObject);
begin
  TagsMAddOrRemove(True);
end;

procedure TForm_Main.TagsMRemoveClick(Sender: TObject);
begin
  TagsMAddOrRemove(False);
end;


procedure TForm_Main.FilterNotesOnTagsSelection;
var
  SelectedTags: TNoteTagArray;
  i: integer;
  FindTags: TFindTags;
  TagsOR: TTagsOR;
  ModeOR: boolean;

  TagsStr: string;
begin
  if (ActiveTreeUI = nil) then exit;
  ModeOr:= (cbTagFilterMode.ItemIndex = 1);

  ActiveTreeUI.UseOfTagsCalcWithInheritedTags:= FindOptions.InheritedTags;
  SelectedTags:= GetTagsSelected;
  TagsStr:= '';

  if SelectedTags <> nil then
     if not ModeOR then begin
        SetLength(FindTags, Length(SelectedTags));
        for i:= 0 to High(SelectedTags) do begin
            TagsStr:= TagsStr + SelectedTags[i].Name + ' ';
            SetLength(TagsOR, 1);
            TagsOR[0]:= SelectedTags[i];
            FindTags[i]:= TagsOR;
        end;
        TagsStr:= TagsStr;
     end
     else begin
        SetLength(FindTags, 1);
        SetLength(TagsOR, Length(SelectedTags));
        for i:= 0 to High(SelectedTags) do begin
            TagsStr:= TagsStr + SelectedTags[i].Name + ' ';
            TagsOR[i]:= SelectedTags[i];
        end;
        FindTags[0]:= TagsOR;
        TagsStr:= 'ANY: '+ TagsStr;
     end;

  ActiveTreeUI.txtTags.Text:= TagsStr;
  ActiveTreeUI.txtTags.Hint:= TagsStr;
  ActiveTreeUI.txtTags.Font.Color:= clWindowText;
  ActiveTreeUI.OnEndFindTagsIntroduction(true, FindTags, '');
end;


procedure TForm_Main.RenameTag;
var
  Node: PVirtualNode;
begin
  if (ActiveFile = nil) or ActiveFile.ReadOnly then exit;
  if (TVTags.SelectedCount <> 1) or not TVTags.Selected[TVTags.FocusedNode] then exit;

  Node:= TVTags.FocusedNode;
  if Node = nil then exit;

  TVTags.EditNode(Node, TVTags.FocusedColumn)
end;

function TForm_Main.CheckRenameTagInNotes(const OldName, NewName: string): boolean;
begin
     case App.DoMessageBox( Format(GetRS(sTag8), [OldName, NewName]), mtConfirmation, [mbYes,mbNo,mbCancel], def1) of
       mrNo :     Result:= True;
       mrCancel : Result:= False;
       mrYes:
         begin
             Result:= True;
             ReplaceInNotes(OldName, NewName, True, False, False);
         end;
     end;

end;


procedure TForm_Main.TVTags_Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
   Allowed := not ActiveFile.ReadOnly;
end;

procedure TForm_Main.TVTags_CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
   // To create and use the generic tag editor. This handler lets us know when editing is started, to disable
   // the context menu, and avoid interference from associated shortcuts.
   // We can't use TV_Editing because the OnEditing event is called from DoCanEdit, not DoEdit, and what it's
   // looking for is to know if editing is allowed.
    EditLink:= nil;
    TVTags.PopupMenu := nil;                   // stop menu events triggered by shortcut keys
end;


// Called in VirtualTree, when edit ended ok and also when was cancelled..
procedure TForm_Main.TVTags_Edited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  TVTags.PopupMenu := Menu_Tags;               // Restore menu -> resume menu events triggered by shortcut keys
end;


procedure TForm_Main.TVTags_NewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  NTag: TNoteTag;
begin
  NTag:= ActiveFile.NoteTagsSorted [Node.Index];
  case Column of
    -1, 0: if not TagMng.IsValidTagName(NewText) then begin
              App.ShowInfoInStatusBar(GetRS(sTag3));
              exit;
           end
           else
           if CheckRenameTagInNotes(NTag.Name, NewText) then begin
              NTag.Name:= NewText;
              if (ActiveFolder <> nil) then
                 ActiveFolder.NoteUI.RefreshTags;
           end;

        1: NTag.Description:= NewText;
  end;

  App.FileSetModified;
end;

//========================================================================================




procedure TForm_Main.ResMRightClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
  begin
    ResPanelOptions.TabOrientation := TTabOrientation(( sender as TMenuItem ).Tag );
    RecreateResourcePanel;
  end;
end;

procedure TForm_Main.ResMMultilineTabsClick(Sender: TObject);
begin
  ResPanelOptions.Stacked := ( not ResPanelOptions.Stacked );
  RecreateResourcePanel;
end;

procedure TForm_Main.Menu_ResPanelPopup(Sender: TObject);
begin
  with ResPanelOptions do
  begin
    case TabOrientation of
      tabposTop : ResMTop.Checked := true;
      tabposBottom : ResMBottom.Checked := true;
      tabposLeft : ResMLeft.Checked := true;
      tabposRight : ResMRight.Checked := true;
    end;

    if KeyOptions.ResPanelLeft then
      ResMPanelLeft.Checked := true
    else
      ResMPanelRight.Checked := true;

    ResMMultilineTabs.Checked := Stacked;
    ResMFindTab.Checked := ShowFind;
    ResMScratchTab.Checked := ShowScratch;
    ResMMacroTab.Checked := ShowMacro;
    ResMTemplateTab.Checked := ShowTemplate;
    ResMPluginTab.Checked := ShowPlugin;
    ResMFavTab.Checked := ShowFavorites;
    ResMTagsTab.Checked := ShowTags;
  end;
end;

procedure TForm_Main.ResMPluginTabClick(Sender: TObject);
var
  sheet : TTab95Sheet;
  CantHideTab : boolean;
  VisibleTabs : integer;

  procedure CannotHideTabMsg;
  begin
    messagedlg( GetRS(sMain87), mtInformation, [mbOK], 0 );
  end;

begin
  sheet := nil;

  VisibleTabs := 0;
  with ResPanelOptions do
  begin
    if ShowFind then inc( VisibleTabs );
    if ShowScratch then inc( VisibleTabs );
    if ShowMacro then inc( VisibleTabs );
    if ShowTemplate then inc( VisibleTabs );
    if ShowPlugin then inc( VisibleTabs );
    if ShowFavorites then inc( VisibleTabs );
    if ShowTags then inc( VisibleTabs );
  end;
  CantHideTab := ( VisibleTabs < 2 );

  if ( sender is TMenuItem ) then
  begin
    with ResPanelOptions do
    begin
      case ( sender as TMenuItem ).Tag of
        0 : begin
          ShowFind := ( not ShowFind );
          if ( CantHideTab and ( not ShowFind )) then
          begin
            CannotHideTabMsg;
            ShowFind := true;
            exit;
          end;
          sheet := ResTab_Find;
        end;
        1 : begin
          ShowScratch := ( not ShowScratch );
          if ( CantHideTab and ( not ShowScratch )) then
          begin
            CannotHideTabMsg;
            ShowScratch := true;
            exit;
          end;
          sheet := ResTab_RTF;
        end;
        2 : begin
          ShowMacro := ( not ShowMacro );
          if ( CantHideTab and ( not ShowMacro )) then
          begin
            CannotHideTabMsg;
            ShowMacro := true;
            exit;
          end;
          sheet := ResTab_Macro;
        end;
        3 : begin
          ShowTemplate := ( not ShowTemplate );
          if ( CantHideTab and ( not ShowTemplate )) then
          begin
            CannotHideTabMsg;
            ShowTemplate := true;
            exit;
          end;
          sheet := ResTab_Template;
        end;
        4 : begin
          ShowPlugin := ( not ShowPlugin );
          if ( CantHideTab and ( not ShowPlugin )) then
          begin
            CannotHideTabMsg;
            ShowPlugin := true;
            exit;
          end;
          sheet := ResTab_Plugins;
        end;
        5 : begin
          ShowFavorites := ( not ShowFavorites );
          if ( CantHideTab and ( not ShowFavorites )) then
          begin
            CannotHideTabMsg;
            ShowFavorites := true;
            exit;
          end;
          sheet := ResTab_Favorites;
        end;
        6 : begin
          ShowTags := ( not ShowTags );
          if ( CantHideTab and ( not ShowTags )) then
          begin
            CannotHideTabMsg;
            ShowTags := true;
            exit;
          end;
          sheet := ResTab_Tags;
        end;

      end;
    end;
  end;

  with ResPanelOptions do
  begin
    ResTab_Find.TabVisible := ShowFind;
    ResTab_RTF.TabVisible := ShowScratch;
    ResTab_Macro.TabVisible := ShowMacro;
    ResTab_Template.TabVisible := ShowTemplate;
    ResTab_Plugins.TabVisible := ShowPlugin;
    ResTab_Favorites.TabVisible := ShowFavorites;
    ResTab_Tags.TabVisible := ShowTags;
  end;

  if (( sender is TMenuItem ) and assigned( sheet )) then
  begin
    if sheet.TabVisible then
      Pages_Res.ActivePage := sheet
    else
      Pages_Res.SelectNextPage( false );
    UpdateResPanelContents (true);
  end;

end; // ResMPluginTabClick


procedure TForm_Main.StatusBarDblClick(Sender: TObject);
var
  myAction : integer;
begin

  if ShiftDown then
    myAction := KeyOptions.StatBarDlbClkActionShft
  else
    myAction := KeyOptions.StatBarDlbClkAction;

  case myAction of
    DBLCLK_NOTHING : begin end;
    DBLCLK_MINIMIZE : Application.Minimize;
    DBLCLK_FILEPROP : KntFileProperties;
    DBLCLK_FILEMGR : RunFileManager;
    DBLCLK_FOLDERPROP : TKntFolder.EditKntFolderProperties( propThisFolder );
    DBLCLK_NEWFOLDER : TKntFolder.CreateNewKntFolder;
    DBLCLK_RESPANEL : MMViewResPanelClick( MMViewResPanel );
  end;
end;


procedure TForm_Main.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const R: TRect);
var
  Index: integer;
begin

  Index:= Panel.Index;
  if Index <> 1 then
     exit;

  with StatusBar.Canvas do begin
      if SBGlyph.Graphic is TBitmap then  begin
         // Draw it transparently
         StatusBar.Canvas.BrushCopy(Bounds(R.Left, R.Top, SBGlyph.Width, SBGlyph.Height),
                                   SBGlyph.Bitmap,
                                   Rect(0, 0, SBGlyph.Width, SBGlyph.Height),
                                   SBGlyph.Bitmap.Canvas.Pixels[0, SBGlyph.Height-1]);
      end else
         StatusBar.Canvas.Draw(R.Left, R.Top, SBGlyph.Graphic);


      Panel.Width:= SBGlyph.Width + 9;
  end;
end;


procedure TForm_Main.Combo_ResFindChange(Sender: TObject);
begin
  CheckFindAllEnabled;
end;

procedure TForm_Main.Btn_ResFindClick(Sender: TObject);
var
   myFindOptions: TFindOptions;
   ApplyFilter: boolean;

begin
  VisibilityControlsFindAllResults (true);

  // transfer FindAll options to myFindOptions
  myFindOptions.MatchCase := CB_ResFind_CaseSens.Checked;
  myFindOptions.WholeWordsOnly := CB_ResFind_WholeWords.Checked;
  myFindOptions.AllTabs := CB_ResFind_AllNotes.Checked;
  myFindOptions.CurrentNodeAndSubtree := CB_ResFind_CurrentNodeAndSubtree.Checked;
  myFindOptions.SearchScope := TSearchScope( RG_ResFind_Scope.ItemIndex );
  myFindOptions.SearchMode := TSearchMode( RG_ResFind_Type.ItemIndex );
  myFindOptions.CheckMode := TSearchCheckMode( RG_ResFind_ChkMode.ItemIndex );
  myFindOptions.HiddenNodes:= CB_ResFind_HiddenNodes.Checked;
  myFindOptions.SearchPathInNodeNames:= CB_ResFind_PathInNames.Checked;
  myFindOptions.Pattern := Combo_ResFind.Text;
  myFindOptions.FindTagsIncl:= FindTagsIncl;
  myFindOptions.FindTagsExcl:= FindTagsExcl;
  myFindOptions.FindTagsInclNotReg:= FindTagsIncl_NotRegistered;
  myFindOptions.FindTagsExclNotReg:= FindTagsExcl_NotRegistered;
  myFindOptions.TagsMetadata:= chkTagsMetad.Checked;
  myFindOptions.TagsText:= chkTagsText.Checked;
  myFindOptions.InheritedTags:= FindOptions.InheritedTags;
  myFindOptions.TagsModeOR:= (cbTagFindMode.ItemIndex = 1);

  myFindOptions.LastModifFrom := 0;
  myFindOptions.LastModifUntil := 0;
  if CB_LastModifFrom.Enabled then
     myFindOptions.LastModifFrom := CB_LastModifFrom.Date;
  if CB_LastModifUntil.Enabled then
     myFindOptions.LastModifUntil := CB_LastModifUntil.Date;

  myFindOptions.CreatedFrom := 0;
  myFindOptions.CreatedUntil := 0;
  if CB_CreatedFrom.Enabled then
     myFindOptions.CreatedFrom := CB_CreatedFrom.Date;
  if CB_CreatedUntil.Enabled then
     myFindOptions.CreatedUntil := CB_CreatedUntil.Date;

  myFindOptions.EmphasizedSearch:= esNone;
  myFindOptions.FoldedMode:= TSearchFoldedMode(CbFindFoldedMode.ItemIndex);
  

  ApplyFilter:= CB_ResFind_Filter.Checked;

  if RunFindAllEx (myFindOptions, ApplyFilter, false) then
     // add search pattern to history
     if ( Combo_ResFind.Items.IndexOf( myFindOptions.Pattern ) < 0 ) then
       Combo_ResFind.Items.Insert( 0, myFindOptions.Pattern );

end;

procedure TForm_Main.Btn_ResFind_PrevClick(Sender: TObject);
begin
  FindAllResults_SelectMatch (true);
end;

procedure TForm_Main.Btn_ResFind_NextClick(Sender: TObject);
begin
  FindAllResults_SelectMatch (false);
end;

procedure TForm_Main.Combo_ResFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( shift = [] ) then
  begin
    case key of
      VK_RETURN : begin
        key := 0;
        if Btn_ResFind.Enabled then
          Btn_ResFindClick( Btn_ResFind );
      end;
    end;
  end;
end; // Combo_ResFindKeyDown


procedure TForm_Main.RxFindAllResultsSelectionChange(Sender: TObject);
begin
  FindAllResults_OnSelectionChange (sender as TRxRichEdit);
end;

procedure TForm_Main.CheckFindAllEnabled;
begin
   if CB_LastModifFrom.Enabled or CB_LastModifUntil.Enabled or CB_CreatedFrom.Enabled or CB_CreatedUntil.Enabled or
      ( (chkTagsMetad.Checked or chkTagsText.Checked) and
         ((FindTagsIncl <> nil) or (FindTagsExcl <> nil) or (FindTagsIncl_NotRegistered <> '') or (FindTagsExcl_NotRegistered <> '')  )) then
      Btn_ResFind.Enabled := true
   else
      Btn_ResFind.Enabled := ( Combo_ResFind.Text <> '' );
end;

procedure TForm_Main.CheckTxtTagsEnabled;
var
  Enable: boolean;
begin
  Enable:= (chkTagsMetad.Checked or chkTagsText.Checked);
  txtTagsIncl.Enabled:= Enable;
  txtTagsExcl.Enabled:= Enable;
  lbl8.Enabled:= Enable;
  lbl9.Enabled:= Enable;
  cbTagFindMode.Enabled:= Enable;
end;


procedure TForm_Main.chk_CreatedFromClick(Sender: TObject);
begin
  CB_CreatedFrom.Enabled:= chk_CreatedFrom.Checked;
  CheckFindAllEnabled;
end;

procedure TForm_Main.chk_CreatedUntilClick(Sender: TObject);
begin
  CB_CreatedUntil.Enabled:= chk_CreatedUntil.Checked;
  CheckFindAllEnabled;
end;

procedure TForm_Main.chk_LastModifFromClick(Sender: TObject);
begin
  CB_LastModifFrom.Enabled:= chk_LastModifFrom.Checked;
  CheckFindAllEnabled;
end;

procedure TForm_Main.chk_LastModifUntilClick(Sender: TObject);
begin
  CB_LastModifUntil.Enabled:= chk_LastModifUntil.Checked;
  CheckFindAllEnabled;
end;



procedure TForm_Main.Combo_FontSizeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then begin
    key := 0;
    FocusActiveEditor;
  end;
end;

{         // [dpv]
procedure TForm_Main.List_ResFindDblClick(Sender: TObject);
var
  i : integer;
  Location : TLocation;
begin
  i := List_ResFind.ItemIndex;
  if ( i < 0 ) then exit;

  Location := TLocation( List_ResFind.Items.Objects[i] );
  if ( not assigned( Location )) then exit;
  JumpToLocation( Location );
  try
    List_ResFind.SetFocus;
  except
  end;
end; // List_ResFindDblClick

procedure TForm_Main.List_ResFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( shift = [] ) then
  begin
    case key of
      VK_RETURN : begin
        key := 0;
        List_ResFindDblClick( List_ResFind );
      end;
    end;
  end;
end;
}

procedure TForm_Main.ResMPanelRightClick(Sender: TObject);
var
  WillChange : boolean;
begin
  WillChange := false;
  if ( sender is TMenuItem ) then
  begin
    case ( sender as TMenuItem ).Tag of
      0 : begin // Left
        WillChange := ( not KeyOptions.ResPanelLeft );
        KeyOptions.ResPanelLeft := true;
      end;
      1 : begin // Right
        WillChange := KeyOptions.ResPanelLeft;
        KeyOptions.ResPanelLeft := false;
      end;
    end;
    if WillChange then
    begin
      messagedlg( GetRS(sMain88), mtInformation, [mbOK], 0 );
    end;
  end;
end; // ResMPanelRightClick



procedure TForm_Main.StdEMSelectAllClick(Sender: TObject);
var
  Handle : THandle;
  TempRichText : string;
  RTFSelStart : integer;
begin
  if ( sender is TMenuItem ) then
  begin
    Handle := 0;
    if ( Menu_StdEdit.PopupComponent is TCustomMemo ) then
    begin
      Handle := ( Menu_StdEdit.PopupComponent as TCustomMemo ).Handle;
    end
    else
    if ( Menu_StdEdit.PopupComponent is TCustomEdit ) then
    begin
      Handle := ( Menu_StdEdit.PopupComponent as TCustomEdit ).Handle;
    end
    else
    if ( Menu_StdEdit.PopupComponent is TCustomComboBox ) then
    begin
      Handle := TEditHandleComboBox( Menu_StdEdit.PopupComponent as TCustomComboBox ).EditHandle;
      // Handle := ( Menu_StdEdit.PopupComponent as TCustomComboBox ).Handle;
    end;

    if ( Handle = 0 ) then exit;

    case ( sender as TMenuItem ).Tag of
      0 : begin // Undo
        SendMessage( Handle, WM_UNDO, 0, 0 );
      end;
      1 : begin // Cut
        SendMessage( Handle, WM_CUT, 0, 0 );
      end;
      2 : begin // Copy
        SendMessage( Handle, WM_COPY, 0, 0 );
      end;
      3 : begin // Paste
        SendMessage( Handle, WM_PASTE, 0, 0 );
      end;
      4 : begin // Paste as Text
         with TWordWrapMemo(( Menu_StdEdit.PopupComponent as TCustomMemo )) do
         begin
           SelText := Clipboard.TryAsText;
         end;
      end;
      5 : begin // Delete
        SendMessage( Handle, WM_CLEAR, 0, 0 );
      end;
      6 : begin // SelectAll
        if ( Menu_StdEdit.PopupComponent is TCustomComboBox ) then
          ( Menu_StdEdit.PopupComponent as TCustomComboBox ).SelectAll
        else
          SendMessage( Handle, EM_SETSEL, 0, -1 );
      end;
      7 : begin // WordWrap
        (*
        // this is a generic solution, and it works, but because
        // we need to specifically protect the RichEdit from loss
        // of formatting on RecreateWnd, we'll do an ugly hack instead.
        if ( Menu_StdEdit.PopupComponent is TCustomMemo ) then
        begin
          with TWordWrapMemo(( Menu_StdEdit.PopupComponent as TCustomMemo )) do
          begin
            WordWrap := ( not WordWrap );
            ( sender as TMenuItem ).Checked := WordWrap;
          end;
        end;
        *)
        try
          RTFSelStart := Res_RTF.SelStart;
          TempRichText := Res_RTF.RtfText;
          Res_RTF.WordWrap := ( not Res_RTF.WordWrap );
          ( sender as TMenuItem ).Checked := Res_RTF.WordWrap;
          Res_RTF.Lines.BeginUpdate;
          try
            Res_RTF.Lines.Clear;
            Res_RTF.PutRtfText(TempRichText, true);
          finally
            Res_RTF.Lines.EndUpdate;
            Res_RTF.SelStart := RTFSelStart;
            Res_RTF.Perform( EM_SCROLLCARET, 0, 0 );
          end;
        except
        end;
      end;
    end;
  end;
end; // StdEMSelectAllClick

procedure TForm_Main.Menu_StdEditPopup(Sender: TObject);
type
  TSelection = record
    StartPos, EndPos: Integer;
  end;
var
  Handle : THandle;
  Selection: TSelection;
  HasSelection : boolean;
begin
  if ( sender is TPopupMenu ) then
  begin
    HasSelection := false;
    if ( Menu_StdEdit.PopupComponent is TCustomEdit ) then
    begin
      Handle := ( Menu_StdEdit.PopupComponent as TCustomEdit ).Handle;
      StdEMUndo.Enabled := ( sendmessage( Handle, EM_CANUNDO, 0, 0 ) <> 0 );
      SendMessage( Handle, EM_GETSEL, WPARAM( @Selection.StartPos ), LPARAM( @Selection.EndPos ));
      HasSelection := Selection.EndPos > Selection.StartPos;
    end
    else
    if ( Menu_StdEdit.PopupComponent is TCustomComboBox ) then
    begin
      Handle := TEditHandleComboBox( Menu_StdEdit.PopupComponent as TCustomComboBox ).EditHandle;
      StdEMUndo.Enabled := ( sendmessage( Handle, EM_CANUNDO, 0, 0 ) <> 0 );
      HasSelection := (( Menu_StdEdit.PopupComponent as TCustomComboBox ).SelLength > 0 );
    end;

    StdEMCut.Enabled := HasSelection;
    StdEMCopy.Enabled := HasSelection;
    StdEMDelete.Enabled := HasSelection;


    if ( Menu_StdEdit.PopupComponent is TCustomMemo ) then
    begin
      StdEMWordWrap.Visible := true;
      StdEMWordWrap.Checked := TWordWrapMemo(( Menu_StdEdit.PopupComponent as TCustomMemo )).WordWrap;
      StdEMPastePlain.Visible := true;
    end
    else
    begin
      StdEMWordWrap.Visible := false;
      StdEMPastePlain.Visible := false;
    end;

  end;

end; // Menu_StdEditPopup

procedure TForm_Main.MMTreeMasterNodeClick(Sender: TObject);
begin
  ActiveTreeUI.CreateParentNode(nil);
end;

procedure TForm_Main.FAMCopytoEditorClick(Sender: TObject);
begin
  FindResultsToEditor( true );
end;

procedure TForm_Main.FAMCopyAlltoEditorClick(Sender: TObject);
begin
  FindResultsToEditor( false );
end;

procedure TForm_Main.RG_ResFind_ScopeClick(Sender: TObject);
var
  SS: TSearchScope;
begin
  SS:= TSearchScope( RG_ResFind_Scope.ItemIndex );
  CB_ResFind_PathInNames.Enabled:= ( SS <> ssOnlyContent );
  if SS = ssOnlyNodeName then begin
     chkTagsText.Checked:= False;
     chkTagsText.Enabled:= False;
  end
  else
     chkTagsText.Enabled:= True;
end;

procedure TForm_Main.ChangeFindInclToModeOR;
begin
   FindTagsIncl:= FindTagsGetModeOR(FindTagsIncl);
   TagMng.UpdateTxtFindTagsHint(txtTagsIncl, txtTagsIncl.Text, FindTagsIncl, FindTagsIncl_NotRegistered);
end;

procedure TForm_Main.cbTagFindModeChange(Sender: TObject);
begin
   if FindTagsIncl = nil then exit;
   if (cbTagFindMode.ItemIndex = 1) then
      ChangeFindInclToModeOR
   else begin
      // Ensure that the tags are interpreted according to ALL mode:
      txtTagsIncl.SetFocus;
      cbTagFindMode.SetFocus;
   end;
end;


procedure TForm_Main.txtTagsInclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsIncl.Text:= '';
       FindTagsIncl:= nil;
   end;

   TagMng.StartTxtFindTagIntrod(txtTagsIncl, OnEndFindTagsInclIntrod, OnChangeFindTagsInclIntrod, true);
end;

procedure TForm_Main.OnChangeFindTagsInclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   if cbTagFindMode.ItemIndex = 1 then
      ChangeFindInclToModeOR
   else
      FindTagsIncl:= FindTags;

   FindTagsIncl_NotRegistered:= Trim(FindTagsNotRegistered);
   CheckFindAllEnabled;
end;

procedure TForm_Main.OnEndFindTagsInclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   OnChangeFindTagsInclIntrod(FindTags, FindTagsNotRegistered);
   if txtTagsIncl.Focused then
      txtTagsInclEnter(nil);
end;


procedure TForm_Main.txtTagsExclEnter(Sender: TObject);
begin
   if CtrlDown then begin
       txtTagsExcl.Text:= '';
       FindTagsExcl:= nil;
   end;

   TagMng.StartTxtFindTagIntrod(txtTagsExcl, OnEndFindTagsExclIntrod, OnChangeFindTagsExclIntrod, True);
end;


procedure TForm_Main.OnChangeFindTagsExclIntrod(FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   FindTagsExcl:= FindTagsGetModeOR(FindTags);
   FindTagsExcl_NotRegistered:= Trim(FindTagsNotRegistered);
   TagMng.UpdateTxtFindTagsHint(txtTagsExcl, txtTagsExcl.Text, FindTagsExcl, FindTagsExcl_NotRegistered);
   CheckFindAllEnabled;
end;



procedure TForm_Main.OnEndFindTagsExclIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string);
begin
   OnChangeFindTagsExclIntrod(FindTags, FindTagsNotRegistered);
   if txtTagsExcl.Focused then
      txtTagsExclEnter(nil);
end;


procedure TForm_Main.chkInhTagsFindClick(Sender: TObject);
begin
  if Initializing or fChangingInCode then exit;

  fChangingInCode:= True;

  FindOptions.InheritedTags:= not FindOptions.InheritedTags;
  TVFilterInhTags.Checked:= FindOptions.InheritedTags;
  chkInhTags.Checked:= chkInhTagsFind.Checked;

  fChangingInCode:= False;
end;


procedure TForm_Main.chkTagsMetadClick(Sender: TObject);
begin
   CheckFindAllEnabled;
   CheckTxtTagsEnabled;
end;


procedure TForm_Main.chkTagsTextClick(Sender: TObject);
begin
   CheckTxtTagsEnabled;
end;

procedure TForm_Main.MMTreeNodeFromSelClick(Sender: TObject);
begin
  ActiveFolder.TreeUI.CreateNodefromSelection;
end;


{$IFDEF WITH_IE}
function TForm_Main.SelectVisibleControlForNote( const aNote : TKntNote ) : TNodeControl;
begin
  result := ncNone;
  if ( not assigned( aNote )) then exit;

  case aNode.VirtualMode of
    vmNone, vmText, vmRTF, vmHTML : result := ncRTF;
    vmIELocal, vmIERemote : begin
      if _IE4Available then
        result := ncIE;
    end;
  end;
end; // SelectVisibleControlForNote
{$ENDIF}


procedure TForm_Main.FavMJumpClick(Sender: TObject);
begin
  JumpToFavorite;
end;

procedure TForm_Main.FavMAddClick(Sender: TObject);
begin
  AddFavorite( false );
end;

procedure TForm_Main.FavMDelClick(Sender: TObject);
begin
  DeleteFavorite;
end;

procedure TForm_Main.ListBox_ResFavClick(Sender: TObject);
var
  myFav : TLocation;
  fn, fldn, nn : string;
begin
  myFav := GetSelectedFavorite;
  if not assigned(myFav) then exit;

  if myFav.ExternalDoc then begin
    StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sMain89), [myFav.Filename] );
    exit;
  end;

  if (not HaveKntFolders( false, false )) or (AnsiCompareText(ActiveFile.FileName, myFav.FileName) <> 0 ) then
     fn := GetRS(sMain90) + ExtractFilename(myFav.Filename)
  else
     fn := '';

  nn := '';
  fldn := '';

  if myFav.Folder <> nil then begin
     fldn:= myFav.Folder.Name;
     if (myFav.NNode <> nil) then
        nn := GetRS(sMain91) + myFav.NNode.NodeName(myFav.Folder)
     else
        nn := '';
  end;

  StatusBar.Panels[PANEL_HINT].Text := Format(GetRS(sMain92), [fn, fldn, nn]);

end; // ListBox_ResFavClick

procedure TForm_Main.ListBox_ResTplClick(Sender: TObject);
begin
  StatusBar.Panels[PANEL_HINT].Text := GetRS(sMain93);
end;

procedure TForm_Main.FavMAddExternalClick(Sender: TObject);
begin
  AddFavorite( true );
end;


procedure TForm_Main.MMTreeOutlineNumClick(Sender: TObject);
begin
  ActiveFolder.TreeUI.OutlineNumberNodes;
end;

procedure TForm_Main.MMHistoryGoBackClick(Sender: TObject);
begin
  NavigateInHistory( hdBack );
end;

procedure TForm_Main.MMHistoryGoForwardClick(Sender: TObject);
begin
  NavigateInHistory( hdForward );
end;

procedure TForm_Main.MMUpRomanClick(Sender: TObject);
var
  actualNumbering : TRxNumbering;
begin
  KeyOptions.LastNumbering := TRxNumbering(( sender as TMenuItem ).Tag );
  ( sender as TMenuItem ).Checked := true;

  actualNumbering:= ActiveEditor.Paragraph.Numbering;
  if not (actualNumbering in [nsNone, nsBullet]) then
      PerformCmd( ecNumbers );
end;

procedure TForm_Main.MMRightParenthesisClick(Sender: TObject);
var
  actualNumbering : TRxNumbering;
  numberingStyle: TRxNumberingStyle;
  actualNumberingStyle : TRxNumberingStyle;
begin
  numberingStyle:= TRxNumberingStyle(( sender as TMenuItem ).Tag );
  actualNumbering:= ActiveEditor.Paragraph.Numbering;
  actualNumberingStyle:= ActiveEditor.Paragraph.NumberingStyle;

  KeyOptions.LastNumberingStyle := numberingStyle;

  case numberingStyle of
    nsNoNumber :
         try
           PerformCmd( ecNumbers );
         finally
           KeyOptions.LastNumberingStyle:= actualNumberingStyle;   // restore actual numbering style
           NumberingStart:= 1;
         end
    else begin
        ( sender as TMenuItem ).Checked := true;
         if not (actualNumbering in [nsNone, nsBullet]) then begin
            PerformCmd( ecNumbers );
         end;
        end;
  end;
end;

procedure TForm_Main.MMStartsNewNumberClick(Sender: TObject);
var
   actualNumbering : TRxNumbering;
begin
   actualNumbering:= ActiveEditor.Paragraph.Numbering;
   try
      NumberingStart:= StrToInt(InputBox( 'KeyNote NF', GetRS(sMain96), '1' ));
      PerformCmd( ecNumbers );
   except
   end;
   NumberingStart:= 1;
end;


procedure TForm_Main.MMHelpWhatsNewClick(Sender: TObject);
begin
  DisplayHistoryFile;
end;

procedure TForm_Main.MMViewTBRefreshClick(Sender: TObject);
begin
  if fileexists( Toolbar_FN ) then begin
    LoadToolbars;
    ResolveToolbarRTFv3Dependencies;
  end
  else begin
    SaveToolbars;
    messagedlg(Format(GetRS(sMain94), [Toolbar_FN] ), mtError, [mbOK], 0 );
  end;
end;

procedure TForm_Main.MMViewTBSaveConfigClick(Sender: TObject);
begin
  SaveToolbars;
  messagedlg( Format(
    GetRS(sMain95), [Toolbar_FN] ), mtInformation, [mbOK], 0 );
end;


procedure TForm_Main.Res_RTFKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
var
  ptCursor : TPoint;
begin
  case key of
    VK_F10 : if ( shift = [ssShift] ) then
    begin
      key := 0;
      GetCursorPos( ptCursor );
      Menu_StdEdit.Popup( ptCursor.X, ptCursor.Y );
    end;
  end;
end; // Res_RTFKeyDown





procedure TForm_Main.MMToolsCustomKBDClick(Sender: TObject);
begin
  CustomizeKeyboard;
end;



procedure TForm_Main.MMTreeNavRightClick(Sender: TObject);
begin
  {
    If the tree has focus, Alt+arrow scrolls the editor;         (Alt+Up, ALt+Down, Alt+Shift+Left,Alt+Shift+Right)
    if the editor is focused, Alt+arrow scrolls the tree
  }

  if ( sender is TMenuItem ) then begin
    if Assigned(ActiveTreeUI) and ActiveTreeUI.Focused then
       ActiveEditor.Navigate(TNavDirection((sender as TMenuItem ).Tag))
    else
    if Assigned(ActiveEditor) then
       ActiveTreeUI.Navigate(TNavDirection((sender as TMenuItem).Tag ));
  end;
end;

procedure TForm_Main.MMToolsExportExClick(Sender: TObject);
begin
  ExportNotesEx;
end;

procedure TForm_Main.FindAllResultsContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  FindAllResults_RightClick ( Form_Main.FindAllResults.GetCharFromPos(MousePos) );
end;

procedure TForm_Main.FindNotify( const Active : boolean );
begin

  // This procedure is called from non-modal dialogs (Find, Replace)
  // every time the dialog form is actiovated or deactivated. When
  // non-modal dialog form is activated, we disable main menu,
  // because otherwise keypresses in the non-modal dialog trigger
  // main menu commands. (Perhaps context menus should be disabled
  // as well?)
  MMFile_.Enabled := Active;
  MMEdit_.Enabled := Active;
  MMView_.Enabled := Active;
  MMInsert_.Enabled := Active;
  MMFormat_.Enabled := Active;
  MMNote_.Enabled := Active;
  MMTree_.Enabled := Active;
  MMSearch_.Enabled := Active;
  MMTools_.Enabled := Active;
  MMHelp_.Enabled := Active;
end; // FindNotify

procedure TForm_Main.FavMRefClick(Sender: TObject);
begin
  RefreshFavorites;
end;

procedure TForm_Main.MMToolsUASClick(Sender: TObject);
begin
  KeyOptions.UASEnable := ( not KeyOptions.UASEnable );
  EnableOrDisableUAS;
end;

procedure TForm_Main.MMToolsUASConfigClick(Sender: TObject);
begin
  ConfigureUAS;
end;

procedure TForm_Main.Combo_ZoomDblClick(Sender: TObject);
begin
  App.SetEditorZoom( DefaultEditorProperties.DefaultZoom, '' );
  FocusActiveKntFolder;
end;

procedure TForm_Main.Combo_ZoomExit(Sender: TObject);
begin
   if Pos('%', Combo_Zoom.Text, 1) = 0 then
      App.SetEditorZoom( -1, Combo_Zoom.Text );
end;

procedure TForm_Main.MMViewZoomInClick(Sender: TObject);
begin
  if ShiftDown then
    App.SetEditorZoom( DefaultEditorProperties.DefaultZoom, '' )
  else
    App.SetEditorZoom( 0, '', KeyOptions.ZoomIncrement );
end;

procedure TForm_Main.MMViewZoomOutClick(Sender: TObject);
begin
  if ShiftDown then
    App.SetEditorZoom( DefaultEditorProperties.DefaultZoom, '' )
  else
    App.SetEditorZoom( 0, '', - KeyOptions.ZoomIncrement );
end;


procedure TForm_Main.MMAlternativeMarginsClick(Sender: TObject);
begin
  MMAlternativeMargins.Checked:= not MMAlternativeMargins.Checked;
  KeyOptions.AltMargins:= MMAlternativeMargins.Checked;

  if assigned(ActiveFolder) then begin
     EnsureNodeAndCaretVisibleInFolders;
     ActiveFolder.Editor.Refresh;
  end;
end;

procedure TForm_Main.UpdateShowImagesState;
var
  Enabled, Checked: boolean;
begin
   Checked:= (ImageMng.ImagesMode = imImage);
   Enabled:= (ActiveEditor <> nil) and ActiveEditor.SupportsRegisteredImages;

   MMShowImages.Checked:= Checked;
   MMShowImages.Enabled:= Enabled;
   TB_Images.Enabled:=  Enabled;
   TB_Images.Down:=     Checked;
end;



procedure TForm_Main.MMShowImagesClick(Sender: TObject);
var
  ForceMode, Show: boolean;
begin
   ForceMode:= false;
   Show:= not MMShowImages.Checked;

   try
      if CtrlDown then begin
         if ImageMng.ImagesMode = imImage then begin
            ActiveFolder.NoteUI.ReloadImagesOnEditor;
            exit;
         end
         else begin
            Show:= false;
            ForceMode:= true;
         end;
      end;

     ShowImages (Show, ForceMode);

   finally
     ActiveEditor.RestoreZoomCurrent;
   end;

end;


procedure TForm_Main.TB_ImagesClick(Sender: TObject);
var
   SS: integer;
   Editor: TKntRichEdit;
begin
{$IF Defined(DEBUG_IMG) AND Defined(DEBUG)}
  if (CtrlDown and AltDown) then begin
     var str: string;
     var i: integer;

     if ActiveFile <> nil then begin
        str:=       ActiveFolder.NoteTextPlain    + #13 + '--------     ' + #13;
        str:= str + ActiveFolder.Editor.TextPlain + #13 + '--------     ' + #13;
        str:= str + ActiveFolder.Editor.RtfText   + #13 + '--------     ' + #13;
        Form_Main.Res_RTF.Text:= str;
     end;

     TB_Images.Down:= not TB_Images.Down;
     exit;
  end;
{$ENDIF}

   if not assigned(ActiveFolder) and not (assigned(ActiveEditor) and ActiveEditor.SupportsRegisteredImages) then exit;

   Editor:= ActiveEditor;

   if CtrlDown or AltDown then begin
      SS:= Editor.SelStart;
      TB_Images.Down:= not TB_Images.Down;

      if CtrlDown then begin
         if ImageMng.ImagesMode = imImage then
            ActiveFolder.NoteUI.ReloadImagesOnEditor
         else
            ShowImages (False, True);
      end
      else
         ActiveFolder.ReconsiderImageDimensionGoalsOnEditor (Editor.SelLength > 0);

      Editor.SelStart:= SS;
      Application.ProcessMessages;
      Editor.SelLength:= 0;
   end
   else
      ShowImages (TB_Images.Down, False);

   Editor.RestoreZoomCurrent;
end;


procedure TForm_Main.ShowImages(Show: boolean; ForceMode: boolean);
var
   ImageModeDest: TImagesMode;

begin
  MMShowImages.Checked:= Show;
  TB_Images.Down := Show;

  if Show then
     ImageModeDest:= imImage
  else
     ImageModeDest:= imLink;

   ImageMng.ImagesMode:= ImageModeDest;

   ActiveFolder.SetImagesMode (ImageModeDest, ForceMode)
end;


{    // [dpv]
procedure TForm_Main.List_ResFindDrawItem(Control: TWinControl;
  Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  LB : TTextListBox;
  Loc : TLocation;
  theColor : TColor;
begin
  // custom draw for List_ResFind
  LB := ( Control as TTextListBox );

  if ( odSelected in State ) then
  begin
    theColor := clHighlight;
  end
  else
  begin
    if ( Index > 0 ) then
    begin
      Loc := TLocation( LB.Items.Objects[index] );

      case Loc.Tag of
        0 : theColor := clWindow;
        else
          theColor := ResPanelOptions.FindListAltColor;
      end;
    end
    else
    begin
      theColor := clWindow;
    end;
  end;


  with LB do
  begin
    canvas.Brush.Color := theColor;
    canvas.Brush.Style := bsSolid;
    canvas.FillRect( Rect );
    canvas.Brush.Style := bsClear;
    TCanvas(canvas).TextRect( Rect, Rect.Left+2, Rect.Top, Items[index] );
  end;
end;
}



procedure TForm_Main.Menu_TimePopup(Sender: TObject);
var
  ItemTag, i : integer;
  ListCount, MenuCount : integer;
  item : TMenuItem;
  nowDate : TDateTime;
begin
  nowDate := now;
  MenuCount := Menu_Time.Items.Count;
  ListCount := TIME_FORMAT_LIST.Count;

  for i := 1 to MenuCount do
  begin
    item := Menu_Time.Items[pred( i )];
    ItemTag := Item.Tag;
    if ( ItemTag <= ListCount ) then
    begin
      Item.Visible := true;
      case Item.Tag of
        0 : begin { nothing } end;
        1 : item.Caption := GetDateTimeFormatted( KeyOptions.TimeFmt, now );
        else
          item.Caption := GetDateTimeFormatted( TIME_FORMAT_LIST[ItemTag-1], now );
      end;
    end
    else
    begin
      Item.Visible := false;
    end;
  end;
end; // Menu_TimePopup

procedure TForm_Main.Menu_DatePopup(Sender: TObject);
var
  ItemTag, i : integer;
  ListCount, MenuCount : integer;
  item : TMenuItem;
  nowDate : TDateTime;
begin
  nowDate := now;
  MenuCount := Menu_Date.Items.Count;
  ListCount := DATE_FORMAT_LIST.Count;

  for i := 1 to MenuCount do
  begin
    Item := Menu_Date.Items[pred( i )];
    ItemTag := Item.Tag;
    if ( ItemTag <= ListCount ) then
    begin
      Item.Visible := true;
      case ItemTag of
        0 : begin { nothing } end;
        1 : item.Caption := GetDateTimeFormatted( KeyOptions.DateFmt, nowDate );
        else
        begin
          item.Caption := GetDateTimeFormatted( DATE_FORMAT_LIST[ItemTag-1], nowDate );
        end;
      end;
    end
    else
    begin
      Item.Visible := false;
    end;
  end;

end; // Menu_DatePopup

procedure TForm_Main.md25Click(Sender: TObject);
var
  ItemTag : integer;
begin
  if not App.CheckActiveEditorNotReadOnly then exit;

  if (Sender is TMenuItem) then begin
    ItemTag := ( sender as TMenuItem ).Tag;
    if (ItemTag > 0) and (ItemTag <= DATE_FORMAT_LIST.Count) then begin
       case ItemTag of
         1 : KeyOptions.DTLastDateFmt := KeyOptions.DateFmt;
         else
             KeyOptions.DTLastDateFmt := DATE_FORMAT_LIST[ItemTag-1];
       end;

       ActiveEditor.AddText(GetDateTimeFormatted(KeyOptions.DTLastDateFmt, now) + #32);
       (Sender as TMenuItem).Checked := true;
    end;
  end;
end;

procedure TForm_Main.mt8Click(Sender: TObject);
var
  ItemTag : integer;
begin
  if not App.CheckActiveEditorNotReadOnly then exit;

  if (Sender is TMenuItem) then begin
     ItemTag := ( sender as TMenuItem ).Tag;
     if (ItemTag > 0 ) and ( ItemTag <= TIME_FORMAT_LIST.Count) then begin
       case ItemTag of
         1 : KeyOptions.DTLastTimeFmt := KeyOptions.TimeFmt;
         else
             KeyOptions.DTLastTimeFmt := TIME_FORMAT_LIST[ItemTag-1];
       end;

       ActiveEditor.AddText(GetDateTimeFormatted(KeyOptions.DTLastTimeFmt, now) + #32);
       (Sender as TMenuItem).Checked := true;
     end;
  end;
end;

procedure TForm_Main.Menu_SymbolsPopup(Sender: TObject);
var
  i, cnt : integer;
  item : TMenuItem;
begin
  cnt := Menu_Symbols.Items.Count;
  for i := 1 to cnt do begin
    item := Menu_Symbols.Items[pred( i )];
    if ( Item.Tag > 0 ) then
      item.Caption := SYMBOL_NAME_LIST[Item.Tag];
  end;
end;


procedure TForm_Main.ms11Click(Sender: TObject);
var
  t : integer;
begin
  if not App.CheckActiveEditorNotReadOnly then exit;

  if (Sender is TMenuItem) then begin
     t := ( sender as TMenuItem ).Tag;
     if (( t > 0 ) and ( t <= high( SYMBOL_CODE_LIST ))) then begin
        if ActiveEditor.SupportsRegisteredImages then begin
           if ActiveEditor.SelLength > 0 then
              ActiveEditor.CheckToSelectLeftImageHiddenMark;
        end;

       ActiveEditor.AddText(SYMBOL_CODE_LIST[t]);
     end;
  end;

end;


(*    // [dpv] Unnecessary. It is possible to activate the link with ENTER
procedure TForm_Main.MMToolsURLClick(Sender: TObject);
var
  pt : TPoint;
begin
  // fake a mouseclick to simulate clicking a hyperlink

  if ( not assigned( ActiveFolder )) then exit;

  GetCaretPos( pt );
  // pt := ActiveFolder.Editor.ClientToScreen( pt );
  // SetCursorPos( pt.x, pt.y );
  _IS_FAKING_MOUSECLICK := true;

  {
  mouse_event( MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0 );
  mouse_event( MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 );
  }

  // alternate solution, does not move the mouse
  PostMessage( ActiveFolder.Editor.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
               MakeLParam( pt.x, pt.y ));
  PostMessage( ActiveFolder.Editor.Handle, WM_LBUTTONUP, 0,
               MakeLParam( pt.x, pt.y ));

end; // MMToolsURLClick
*)

procedure TForm_Main.Combo_StyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  myIcon : TIcon;
  myIconIdx : integer;
  myOffset : integer;
  myPos : TPoint;
  myText : string;
  myLen : integer;
  myRect : TRect;
  myStyle : TStyle;
  r : integer;
begin
  r := Combo_Style.ItemHeight;
  myOffset := 2;
  myText := Combo_Style.Items[Index];
  myLen := length( myText );

  myIconIdx := STYLE_IMAGE_BASE + ord( TStyle( StyleManager.Objects[Index] ).Range );
  myIcon := TIcon.Create;
  try
      Combo_Style.Canvas.FillRect( Rect );
      IMG_Toolbar.GetIcon( myIconIdx, myIcon );
      myPos := Point( myOffset + 2, Rect.Top + (( Rect.Bottom - Rect.Top - IMG_Toolbar.Height) div 2));
      DrawIconEx( Combo_Style.Canvas.Handle, myPos.X, myPos.Y, myIcon.Handle, IMG_Toolbar.Width, IMG_Toolbar.Height, 0, Combo_Style.Canvas.Brush.Handle, DI_Normal );

      inc( myOffset, myPos.X+IMG_Toolbar.Width+2 );

      SetRect( myRect, myOffset, Rect.Top, Rect.Right, Rect.Bottom);
  finally
      myIcon.Free;
  end;


  if KeyOptions.StyleShowSamples then begin
    // draw style sample (only for styles which have font info)

    myStyle := TStyle( StyleManager.Objects[Index] );
    if ( myStyle.Range <> srParagraph ) then begin
      with Combo_Style.Canvas.Font do
      begin
        Name := myStyle.Font.Name;
        Charset := myStyle.Font.Charset;
        Style := myStyle.Font.Style;
        if ( not ( odSelected in State )) then
          Color := myStyle.Font.Color;
        // Size := myStyle.Font.Size; [x] cannot do this
      end;

      if (( not ( odSelected in State )) and myStyle.Text.HasHighlight ) then
         Combo_Style.Canvas.Brush.Color := myStyle.Text.Highlight;
    end;
    DrawText( Combo_Style.Canvas.Handle, PChar(myText), myLen, myRect, DT_SingleLine or DT_VCenter);
  end
  else  // do not draw style samples, but show style images instead
      DrawText( Combo_Style.Canvas.Handle, PChar(myText), myLen, myRect, DT_SingleLine or DT_VCenter);
end;

{   Unused
procedure TForm_Main.DoBorder1Click(Sender: TObject);
var
  Paragraph: TParaFormat2;
begin
  FillChar(Paragraph, SizeOf(Paragraph), 0);
  Paragraph.cbSize := SizeOf(Paragraph);

  Paragraph.dwMask := PFM_BORDER;
  Paragraph.wBorders := 64;

  SendMessage( ActiveEditor.Handle, EM_SETPARAFORMAT, 0, LPARAM(@Paragraph));
end;
}

procedure TForm_Main.MMEditDecimalToRomanClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.ArabicToRoman;
end;


procedure TForm_Main.MMEditRomanToDecimalClick(Sender: TObject);
begin
  if App.CheckActiveEditor then
     ActiveEditor.RomanToArabic;
end;

procedure TForm_Main.MMViewStatusBarClick(Sender: TObject);
begin
  // show or hide status bar
  KeyOptions.StatBarShow := ( not KeyOptions.StatBarShow );
  UpdateStatusBarState;
end;

procedure TForm_Main.UpdateStatusBarState;
begin
  StatusBar.Visible := KeyOptions.StatBarShow;
  MMViewStatusBar.Checked := KeyOptions.StatBarShow;
end;


procedure TForm_Main.UpdateTreeVisible( const AFolder : TKntFolder );
var
   Inc: Integer;
   TreeVisible: boolean;
begin
    TreeVisible:= not AFolder.TreeHidden;
    AFolder.TreeUI.Visible := TreeVisible;

    if ( AFolder.TreeHidden ) then
       if not AFolder.Editor.Focused then
          AFolder.SetFocusOnNoteEditor;
    AFolder.Editor.Invalidate;

    MMViewTree.Checked := TreeVisible;
    MMViewNodeIcons.Enabled := TreeVisible;
    MMViewCustomIcons.Enabled := TreeVisible;
    MMViewCheckboxesAllNodes.Enabled := TreeVisible;
    MMViewHideCheckedNodes.Enabled := TreeVisible;

    if KeyOptions.UseCtrlHideTreePanel then
        if AFolder.TreeHidden then begin
           if CtrlDown then begin
              _WindowWidthIncToRestore:= AFolder.TreeUI.Width;
              Inc:= _WindowWidthIncToRestore;
              With Form_Main do
                SetBounds(Left + Inc, Top, Width - Inc, Height );
           end;
        end
        else
           CheckRestoreAppWindowWidth;

end; // UpdateTreeVisible


procedure TForm_Main.CheckRestoreAppWindowWidth (EnsureTreeVisible: boolean= False);
var
   Inc: Integer;
begin
    if _WindowWidthIncToRestore > 0 then begin
       if EnsureTreeVisible then
          ActiveFolder.TV.Visible := True;

       Inc:= _WindowWidthIncToRestore;
       _WindowWidthIncToRestore:= 0;
       With Form_Main do
         SetBounds(Left - Inc, Top, Width + Inc, Height );
    end;
end;



procedure TForm_Main.FavMPropertiesClick(Sender: TObject);
begin
  FavoriteEditProperties;
end;



procedure TForm_Main.MMEditPasteAsWebClipClick(Sender: TObject);
begin
  ClipCapMng.PasteAsWebClip(false);
end;

procedure TForm_Main.MMEditPasteAsWebClipTextClick(Sender: TObject);
begin
  ClipCapMng.PasteAsWebClip(true);
end;


procedure TForm_Main.RxRTFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if ( shift = [ssCtrl] ) then begin
    case Key of

      { NUMPAD  *}
      VK_MULTIPLY : if Combo_Style.Visible then
        try
          Combo_Style.SetFocus;
        except
        end;

      { NUMPAD / }
      111 : if KeyOptions.ResPanelShow then  // if Combo_Macro.Visible then
        try
          Pages_Res.ActivePage := ResTab_Macro;
          ListBox_ResMacro.SetFocus;
        except
        end;

      { backslash }
      220 : if ( Shift = [ssCtrl] ) then begin
        Key := 0;
        MMTreeFocusTreeClick (nil);
      end;
    end;

  end;

end; // RxRTFKeyDown

procedure TForm_Main.RxResTabRTFKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if ( Shift = [ssCtrl] ) and (key = 220) then begin   { backslash }
     Key := 0;
     MMTreeFocusEditorClick (nil);
   end;
end;

procedure TForm_Main.RxRTFKeyPress(Sender: TObject; var Key: Char);
begin
   if (Key = #9) and ShiftDown then begin
      Key := #0;
      MMTreeFocusTreeClick (nil);
   end
end;

procedure TForm_Main.RxResTabRTFKeyPress(Sender: TObject; var Key: Char);
begin
   if (Key = #9) and ShiftDown then begin
      Key := #0;
      MMTreeFocusEditorClick (nil);
   end
end;



// Tree panel context menu commands ===========================================

{$REGION 'Tree commands'}


procedure TForm_Main.actTVAlarmNodeExecute(Sender: TObject);
begin
   SetAlarm (false);
end;


procedure TForm_Main.actTVAddNode_ParentExecute(Sender: TObject);
begin
  ActiveTreeUI.CreateParentNode(ActiveTreeUI.FocusedNode);
end;

procedure TForm_Main.actTVAddNode_AboveExecute(Sender: TObject);
begin
  ActiveTreeUI.AddNode(tnAddAbove);
end;

procedure TForm_Main.actTVAddNode_ChildExecute(Sender: TObject);
begin
  ActiveTreeUI.AddNode(tnAddChild)
end;

procedure TForm_Main.actTVAddNode_BelowExecute(Sender: TObject);
begin
  ActiveTreeUI.AddNode(tnAddBelow);
end;

procedure TForm_Main.actTVAddNode_LastExecute(Sender: TObject);
begin
  ActiveTreeUI.AddNode( tnAddLast );
end;

procedure TForm_Main.actTVCheckNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.ToggleCheckNode(ActiveTreeUI.FocusedNode);
end;

procedure TForm_Main.actTVFlaggedNodeExecute(Sender: TObject);
begin
   ActiveTreeUI.ToggleNodeFlagged(nil);
end;


procedure TForm_Main.actTVBoldNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.SetNodeBold(ShiftDown );
  if (ActiveControl is TVTree) and assigned(ActiveNNode) then
     TB_Bold.Down := ActiveNNode.Bold;
end;

procedure TForm_Main.actTVNodeTextColorExecute(Sender: TObject);
begin
  ActiveTreeUI.SetNodeColor(true, true, false, ShiftDown );
end;

procedure TForm_Main.actTVNodeBGColorExecute(Sender: TObject);
begin
  ActiveTreeUI.SetNodeColor(true, false, false, ShiftDown );
end;

procedure TForm_Main.actTVDefaultNodeFontExecute(Sender: TObject);
var
  ShiftWasDown : boolean;
begin
  ShiftWasDown := ShiftDown;
  ActiveTreeUI.SetNodeColor(false, true, true, ShiftWasDown );
  ActiveTreeUI.SetNodeColor(false, false, true, ShiftWasDown );
  ActiveTreeUI.SetNodeFontFace(true, ShiftWasDown );
  ShowNodeChromeState(ActiveTreeUI);
end;

procedure TForm_Main.actTVSelectNodeImageExecute(Sender: TObject);
begin
  ActiveTreeUI.SetNodeCustomImage;
end;


procedure TForm_Main.actTVChildrenCheckboxExecute(Sender: TObject);    // [dpv]
begin
  ActiveTreeUI.ToggleChildrenCheckbox(ActiveTreeUI.FocusedNode);
end;

procedure TForm_Main.actTVHideCheckedChildrenExecute(Sender: TObject);
begin
   ActiveTreeUI.HideChildNodesUponCheckState (ActiveTreeUI.FocusedNode, true);
end;

procedure TForm_Main.actTVHideUncheckedChildrenExecute(Sender: TObject);
begin
   ActiveTreeUI.HideChildNodesUponCheckState (ActiveTreeUI.FocusedNode, false);
end;

procedure TForm_Main.actTVShowNonFilteredExecute(Sender: TObject);
begin
   ActiveTreeUI.ShowNonFilteredNodes (ActiveTreeUI.FocusedNode);
end;

procedure TForm_Main.actTVDeleteNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.DeleteNode( false );
end;

procedure TForm_Main.actTVDeleteChildrenExecute(Sender: TObject);
begin
  ActiveTreeUI.DeleteNode( true );
end;

procedure TForm_Main.actTVMoveNodeUpExecute(Sender: TObject);
begin
  ActiveTreeUI.MoveTreeNode( dirUp );
end;

procedure TForm_Main.actTVMoveNodeDownExecute(Sender: TObject);
begin
  ActiveTreeUI.MoveTreeNode( dirDown );
end;

procedure TForm_Main.actTVMoveNodeLeftExecute(Sender: TObject);
begin
  ActiveTreeUI.MoveTreeNode( dirLeft );
end;

procedure TForm_Main.actTVMoveNodeRightExecute(Sender: TObject);
begin
  ActiveTreeUI.MoveTreeNode( dirRight );
end;

procedure TForm_Main.actTVPasteNodeNameExecute(Sender: TObject);
begin
  if (sender is TAction) then
     ActiveTreeUI.PasteNodeName(nil, TPasteNodeNameMode((sender as TAction).Tag));
end;

procedure TForm_Main.actTVCopyNodeNameExecute(Sender: TObject);
begin
  ActiveTreeUI.CopyNodeName( ShiftDown );
end;

procedure TForm_Main.actTVCopyNodeTextExecute(Sender: TObject);
begin
  ActiveTreeUI.CopyNodeName( true );
end;

procedure TForm_Main.actTVCopyNodePathExecute(Sender: TObject);
begin
  ActiveTreeUI.CopyNodePath(nil, false );
end;

procedure TForm_Main.actTVCopyPathtoEditorExecute(Sender: TObject);
begin
  ActiveTreeUI.CopyNodePath(nil, true );
end;


procedure TForm_Main.actTVVirtualNodeExecute(Sender: TObject);
begin
  ActiveFolder.VirtualNoteProc('');
end;

procedure TForm_Main.actTVRefreshVirtualNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.VirtualNoteRefresh( KntTreeOptions.ConfirmNodeRefresh );
end;

procedure TForm_Main.actTVInsertLinkedNNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.InsertLinkedNNode(ActiveTreeUI.FocusedNode);
end;

procedure TForm_Main.actTVNavigateNextLinkedNNodeExecute(Sender: TObject);
var
  NNode: TNoteNode;
  nnf: TNoteNodeInFolder;
  i: integer;
begin
  NNode:= ActiveFolder.FocusedNNode;
  i:= NNode.Note.GetIndexOfNNodeInFolder(NNode);
  if i = High(NNode.Note.NNodes) then
     i:= 0
  else
     inc(i);

  nnf:= NNode.Note.NNodes[i];
  if nnf.Folder <> ActiveFolder then
     App.ActivateFolder(TKntFolder(nnf.Folder));

  TKntFolder(nnf.Folder).TreeUI.MakePathVisible(nnf.NNode.TVNode);
  TKntFolder(nnf.Folder).TreeUI.SelectAlone(nnf.NNode.TVNode);
end;

procedure TForm_Main.actTVUnlinkVirtualNodeExecute(Sender: TObject);
begin
  ActiveTreeUI.VirtualNoteUnlink;
end;

procedure TForm_Main.actTVCutSubtreeExecute(Sender: TObject);
begin
  CmdCut;
end;

procedure TForm_Main.actTVCopySubtreeExecute(Sender: TObject);
begin
  CmdCopy;
end;

procedure TForm_Main.actTVPasteSubtreeExecute(Sender: TObject);
begin
  CmdPaste (false, false);
end;

procedure TForm_Main.actTVPasteSubtreeLinkedExecute(Sender: TObject);
begin
  ActiveTreeUI.TreeTransferProc(ttPaste, KeyOptions.ConfirmTreePaste, true);
end;

procedure TForm_Main.actTVEraseTreeMemExecute(Sender: TObject);
begin
  ActiveTreeUI.TreeTransferProc(ttClear, false, false);
end;

procedure TForm_Main.actTVExportExecute(Sender: TObject);
begin
  ExportTreeNode;
end;

procedure TForm_Main.actTVSortSubtreeExecute(Sender: TObject);
begin
  ActiveTreeUI.SortSubtree(ActiveTreeUI.FocusedNode);
end;


procedure TForm_Main.actTVSortTreeExecute(Sender: TObject);
begin
  ActiveTreeUI.SortTree;
end; // Sort full tree

procedure TForm_Main.actTVRenameNodeExecute(Sender: TObject);
begin
   ActiveTreeUI.RenameFocusedNode;
end;


procedure TForm_Main.actTVViewAdditColumnsExecute(Sender: TObject);
begin
   ActiveTreeUI.ShowAdditionalColumns(not ActiveTreeUI.AdditionalColumnsAreVisible);
end;


procedure TForm_Main.actTVFilterOutUnflaggedExecute(Sender: TObject);
begin
  ActiveTreeUI.FilterOutUnflagged (not ActiveTreeUI.FilterOutUnflaggedApplied);
end;

procedure TForm_Main.TVFilterUsePathClick(Sender: TObject);
begin
  FindOptions.SearchPathInNodeNames:= not FindOptions.SearchPathInNodeNames;
  TVFilterUsePath.Checked:= FindOptions.SearchPathInNodeNames;
  CB_ResFind_PathInNames.Checked:= FindOptions.SearchPathInNodeNames;
  ActiveTreeUI.ExecuteTreeFiltering;
end;

procedure TForm_Main.TVFilterShowChildrenClick(Sender: TObject);
begin
  FindOptions.ShowChildren:= not FindOptions.ShowChildren;
  TVFilterShowChildren.Checked:= FindOptions.ShowChildren;
  ActiveTreeUI.ReapplyFilter;
end;


procedure TForm_Main.TVFilterInhTagsClick(Sender: TObject);
begin
  chkInhTags.Checked:= not chkInhTags.Checked;
end;



{$ENDREGION}   //  'Tree commands' ==================================


//=================================================================
// Update UI State
//=================================================================

procedure TForm_Main.RxChangedSelection(Sender: TKntRichEdit; ConsiderAllOnPlainText: boolean = false);
var
  SelAttrib: TRxTextAttributes;
  ParagraphAttrib: TRxParaAttributes;
  FontStyles: TFontStyles;
  SubscriptStyle: TSubscriptStyle;
  Numbering: TRxNumbering;
  TextSelected: boolean;

begin
    if (Sender = nil) or (Sender <> ActiveEditor) or (not Sender.Focused) then exit;


    SelAttrib:= Sender.SelAttributes;
    ParagraphAttrib:= Sender.Paragraph;

    TextSelected:= ( Sender.SelLength > 0 );
    TB_EditCut.Enabled := TextSelected;
    TB_EditCopy.Enabled := TextSelected;
    MMEditCut.Enabled := TextSelected;
    MMEditCopy.Enabled := TextSelected;
    RTFMCut.Enabled := TextSelected;
    RTFMCopy.Enabled := TextSelected;
    RTFM_RTL.Checked:= ParagraphAttrib.RTL;

    if Sender.PlainText and not ConsiderAllOnPlainText then exit;


    Combo_Font.FontName := SelAttrib.Name;
    Combo_FontSize.Text := inttostr( SelAttrib.Size );

    FontStyles:= SelAttrib.Style;

    TB_Bold.Down := fsBold in FontStyles;
    MMFormatBold.Checked := TB_Bold.Down;

    TB_Italics.Down := fsItalic in FontStyles;
    MMFormatItalics.Checked := TB_Italics.Down;

    TB_Underline.Down := fsUnderline in FontStyles;
    MMFormatUnderline.Checked := TB_Underline.Down;

    TB_Strikeout.Down := fsStrikeOut in FontStyles;
    MMFormatStrikeout.Checked := TB_Strikeout.Down;

    case ParagraphAttrib.LineSpacing of
      0 : begin
        MMFormatLS1.Checked := true;
        TB_Space1.Down := true;
      end;
      1 : begin
        MMFormatLS15.Checked := true;
        TB_Space15.Down := true;
      end;
      2 : begin
        MMFormatLS2.Checked := true;
        TB_Space2.Down := true;
      end;
    end;

    Numbering:= ParagraphAttrib.Numbering;

    if ( _LoadedRichEditVersion > 2 ) then
    begin
      TB_Bullets.Down := ( Numbering = nsBullet );
      MMFormatBullets.Checked := TB_Bullets.Down;
      TB_Numbers.Down := ( not ( Numbering in [nsNone, nsBullet] ));
      MMFormatNumbers.Checked := TB_Numbers.Down;
    end
    else
    begin
      TB_Bullets.Down := ( Numbering <> nsNone );
      MMFormatBullets.Checked := TB_Bullets.Down;
    end;

    MMFormatDisabled.Checked := SelAttrib.Disabled;

    SubscriptStyle:= SelAttrib.SubscriptStyle;
    MMFormatSubscript.Checked := SubscriptStyle = ssSubscript;
    TB_Subscript.Down := MMFormatSubscript.Checked;
    MMFormatSuperscript.Checked := SubscriptStyle = ssSuperscript;
    TB_Superscript.Down := MMFormatSuperscript.Checked;

    case ParagraphAttrib.Alignment of
      paLeftJustify : begin
        TB_AlignLeft.Down := true;
        MMFormatAlignLeft.Checked := true;
      end;
      paRightJustify : begin
        TB_AlignRight.Down := true;
        MMFormatAlignRight.Checked := true;
      end;
      paCenter : begin
        TB_AlignCenter.Down := true;
        MMFormatAlignCenter.Checked := true;
      end;
      paJustify : begin
        TB_AlignJustify.Down := true;
        MMFormatAlignJustify.Checked := true;
      end;
    end;

    CheckTrackStyleInfo (Sender);

end; // RxChangedSelection


procedure TForm_Main.EnableActionsForEditor(SupportsRTF: boolean);
var
  i: integer;

begin
    for i := 0 to Toolbar_Format.ControlCount - 1 do
       if ((Copy(Toolbar_Format.Controls[i].Name,1,5) <> 'TB_Go'))  then
           Toolbar_Format.Controls[i].Enabled:= SupportsRTF;

    //TB_GoForward.Enabled:= True;
    //TB_GoBack.Enabled:= True;
    TB_WordWrap.Enabled:= True;

    for i := 0 to Toolbar_Style.ControlCount - 1 do
       Toolbar_Style.Controls[i].Enabled:= SupportsRTF;

    RTFMFont.Enabled:= SupportsRTF;
    RTFMPara.Enabled:= SupportsRTF;

    for i := 0 to MMFormat_.Count - 1 do
       MMFormat_.Items[i].Enabled:= SupportsRTF;

    MMFormatLanguage.Enabled:= True;
    MMFormatWordWrap.Enabled:= True;
    MMFormatBGColor.Enabled:= True;

    MMInsertPicture.Enabled:= SupportsRTF;
    MMInsertObject.Enabled:= SupportsRTF;

    RTFMPlainText.Checked:= not SupportsRTF;
    RTFMFold.Enabled:= SupportsRTF;
    RTFMUnfold.Enabled:= SupportsRTF;
end;

procedure TForm_Main.EnableActionsForEditor(VinculatedToNote, SupportsImages, SupportsRegImages: boolean);
begin
    MMInsertMarkLocation.Enabled := VinculatedToNote;
    MMBkmSet_.Enabled:= VinculatedToNote;

    RTFMRestoreProportions.Enabled:= SupportsImages;
    MMShowImages.Enabled:= SupportsRegImages and VinculatedToNote;
    TB_Images.Enabled:=  SupportsRegImages and VinculatedToNote;

    RTFMPlainText.Enabled:= VinculatedToNote;
    RTFMTags.Enabled:= VinculatedToNote;
    if (not VinculatedToNote) then
       RTFMTags.Hint:= '';
end;

procedure TForm_Main.ShowNodeChromeState(TreeUI: TKntTreeUI);
begin
    if assigned(ActiveNNode) then begin
      Combo_Font.FontName := TreeUI.GetNodeFontFace(TreeUI.FocusedNode);
      Combo_FontSize.Text := inttostr( ActiveFolder.TreeChrome.Font.Size);
      TB_Bold.Down := ActiveNNode.Bold;
    end;
end;

procedure TForm_Main.EnableActionsForTree(TreeUI: TKntTreeUI; ReadOnly: boolean= false);
var
  i: integer;

begin
    for i := 0 to Toolbar_Format.ControlCount - 1 do begin
       with Toolbar_Format.Controls[i] do begin
          if (Copy(Name,1,5) = 'TB_Go') then continue;
          if ReadOnly then
             Enabled:= false
       end;
    end;

    for i := 0 to Toolbar_Style.ControlCount - 1 do
       Toolbar_Style.Controls[i].Enabled:= false;

    MMFormatBGColor.Enabled:= True;

    for i := 0 to MMFormat_.Count - 1 do
       MMFormat_.Items[i].Enabled:= false;

    //TB_GoForward.Enabled:= True;
    //TB_GoBack.Enabled:= True;

    if not ReadOnly then begin
       Combo_Font.Enabled:= true;
       TB_Bold.Enabled:= true;
       TB_Color.Enabled:= true;
       TB_Hilite.Enabled:= true;
       MMFormatTextColor.Enabled:= true;
       MMFormatHighlight.Enabled:= true;
       MMFormatNoHighlight.Enabled:= true;
    end;

    ShowNodeChromeState (TreeUI);
end;


procedure TForm_Main.actList_FileUpdate(Action: TBasicAction; var Handled: Boolean);
begin
   UpdateFileModifiedState;
   Handled:= true;
end;


procedure TForm_Main.actList_TVsUpdate(Action: TBasicAction; var Handled: Boolean);
var
  Node: PVirtualNode;
  NNode : TNoteNode;
  Note: TNote;
  IsVirtual : boolean;
  HasSeveralNNodes: boolean;

begin
   Handled:= true;
   if ActiveTreeUI = nil then exit;
   Node:= ActiveTreeUI.FocusedNode;

   IsVirtual := false;
   HasSeveralNNodes:= false;

   if Node <> nil then begin
      NNode:= ActiveTreeUI.GetNNode(Node);
      Note:= NNode.Note;
      IsVirtual:= Note.IsVirtual;
      HasSeveralNNodes := Note.NumNNodes > 1;

      actTVCheckNode.Checked := Node.CheckState.IsChecked;
      actTVChildrenCheckbox.Checked := NNode.ChildrenCheckbox;
      actTVBoldNode.Checked := NNode.Bold;
      actTVFlaggedNode.Checked := NNode.Flagged;
   end
   else
      actTVCheckNode.Checked := false;

   TVVirtualNode_.Checked:= IsVirtual;
   actTVVirtualNode.Checked := IsVirtual;
   actTVRefreshVirtualNode.Enabled := IsVirtual;
   actTVUnlinkVirtualNode.Enabled := IsVirtual;
   actTVNavigateNextLinkedNNode.Enabled := HasSeveralNNodes;
   TVLinkedNode_.Checked:= HasSeveralNNodes;

   actTVChildrenCheckbox.Enabled := not ActiveFolder.Checkboxes;
   actTVCheckNode.Enabled:= assigned(Node) and (Node.CheckType <> ctNone);

   actTVViewAdditColumns.Checked:= ActiveTreeUI.AdditionalColumnsAreVisible;
   actTVFilterOutUnflagged.Checked:= ActiveTreeUI.FilterOutUnflaggedApplied;
   actTVFilterOutUnflagged.Enabled:= ActiveTreeUI.NNodesFlagged;
end;

procedure TForm_Main.UpdateOpenFile;
var
  FN : string;
  F: TKntFile;
begin
   F:= ActiveFile;
   StatusBar.Panels.BeginUpdate;
   try
      if not assigned(F) then begin
        FN:= GetRS(sMain42);                                 // (No file)
        StatusBar.Panels[PANEL_CARETPOS].Text := '';
        StatusBar.Panels[PANEL_NOTEINFO].Text := '';
        StatusBar.Panels[PANEL_STATUS].Text := '';
        StatusBar.Panels[PANEL_FILEICON].Text := '';
        SelectStatusBarGlyph(false);
        StatusBar.Hint := '';
        actFileSave.Enabled:= false;
      end
      else begin
         if (ActiveFile.FileName <> '') then
           FN := F.File_Name
         else
           FN := GetRS(sMain40);

         StatusBar.Hint := #32 + F.FileName;
         SelectStatusbarGlyph(true);
      end;

      Application.Title := Format( '%s - %s', [FN, Program_Name] );
      Caption:= Format('%s  %s - %s', [Program_Name, Program_Version, FN]);
      SetFilenameInStatusbar(#32 + FN + #32);
      TrayIcon.Hint := Program_Name + ': ' + FN;
      UpdateTabAndTreeIconsShow;

   finally
      StatusBar.Panels.EndUpdate;
   end;
end; // UpdateOpenFile


procedure TForm_Main.UpdateFileModifiedState;
var
  status: string;
  WasModif, Modif: boolean;
  F: TKntFile;
begin
   F:= ActiveFile;
   if not assigned(F) then exit;

   WasModif:= actFileSave.Enabled;
   Modif:= ActiveFile.Modified;
   actFileSave.Enabled:= Modif;
   if WasModif = Modif then exit;

   if Modif then
      status:= GetRS(sMain44)      // MOD
   else
   if KeyOptions.AutoSave then
      status:= GetRS(sMain43)   // Auto
   else
      status:= GetRS(sMain45);  // Saved

   Statusbar.Panels[PANEL_STATUS].Text := status;
end;


procedure TForm_Main.UpdateFolderDisplay (OnlyActionsState: boolean = false);
var
  s : string;
  myFolder : TKntFolder;
  Editor: TKntRichEdit;
  Node: PVirtualNode;

begin
  s := '';
  myFolder := ActiveFolder;

  if assigned( myFolder ) then begin
    try
      MMTree_.Visible := true;
      MMNoteReadOnly.Checked := myFolder.ReadOnly;
      MMViewTree.Enabled := true;
      MMViewTree.Checked := myFolder.TreeUI.Visible;
      MMViewEditorInfoPanel.Checked:= not myFolder.EditorInfoPanelHidden;
      MMViewNodeIcons.Checked := myFolder.IconKind = niStandard;
      MMViewCustomIcons.Checked := myFolder.IconKind = niCustom;
      MMEditPasteAsNewNode.Visible := true;
      MMP_PasteAsNode.Visible := true;
      MMViewCheckboxesAllNodes.Checked := myFolder.Checkboxes;

      ActiveTreeUI.PnlInf.Visible:= KeyOptions.ToolbarTreeShow;
      MMViewNodeIcons.Enabled := MMViewTree.Checked;
      MMViewCustomIcons.Enabled := MMViewTree.Checked;
      MMViewCheckboxesAllNodes.Enabled := MMViewTree.Checked;
      MMViewCustomIcons.Enabled := MMViewTree.Checked;
      TVSelectNodeImage.Enabled := ( MMViewCustomIcons.Checked and MMViewCustomIcons.Enabled );
      MMViewHideCheckedNodes.Enabled := true;
      MMViewHideCheckedNodes.Checked:= myFolder.TreeUI.TB_HideChecked.Down;
      MMViewFilterTree.Enabled:= myFolder.TreeUI.TB_FilterTree.Enabled;
      MMViewFilterTree.Checked :=  myFolder.Filtered;
      MMViewFilterTree.Hint:= myFolder.TreeUI.TB_FilterTree.Hint;

      if not OnlyActionsState then begin
         Editor:= myFolder.Editor;
         ClipCapMng.ShowState;
         UpdateAlarmStatus;
         Editor.SetMargins;
         UpdateWordWrap;
         Editor.CheckWordCount(true);
         Editor.Change;                 // It will only refresh UI if there is changes
         RxChangedSelection(Editor, true);

         if myFolder.ReadOnly then s := 'R';

         UpdateShowImagesState;
         ShowInsMode;

         if MMAlternativeMargins.Checked then
            Editor.Refresh;
      end;

    except
      // showmessage( 'BUG: error in UpdateFolderDisplay' );
    end;
  end
  else begin
    MMNoteReadonly.Checked := false;
    MMFormatWordWrap.Checked := false;
    MMSetAlarm.Checked:= false;
    TB_WordWrap.Down := false;
    RTFMWordwrap.Checked := false;
    TB_ClipCap.Down := false;
    MMNoteClipCapture.Checked := false;
    TMClipCap.Checked := false;
    s := '';
    StatusBar.Panels[PANEL_INS].Text := '';

    MMTree_.Visible := false;
  end;

  if not OnlyActionsState then
    StatusBar.Panels[PANEL_NOTEINFO].Text := s;

end; // UpdateFolderDisplay


Initialization
   SBGlyph:= nil;
end.

