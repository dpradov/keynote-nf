unit kn_Main;

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

 ===========
 13 Nov 2007
 ===========
 The adaptation of KeyNote to Delphi 2006 and the new functionalities
 added since version 1.7.0 corresponds to Daniel Prado Velasco
 <dprado.keynote@gmail.com> (Spain)
 Portions adapted by Daniel Prado are
 Copyright (C) 2007, 2008. All Rights Reserved.

  http://code.google.com/p/keynote-nf/
  http://groups.google.com/group/keynote-nf
  <dprado.keynote@gmail.com>

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
  <marekjed@users.sourceforge.net>

************************************************************ *)

{.$DEFINE MJ_DEBUG}

interface

uses
  { Borland units }
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, Spin, ExtDlgs,
  FileCtrl, RichEdit, IniFiles,
  Clipbrd, ShellAPI, StdCtrls,
  ExtCtrls, mmsystem,
  { 3rd-party units }
  ComCtrls95,
  Parser,
  SystemImageList,
  cmpGFXComboBox,
  cmpGFXListBox,
  TB97Ctls, TB97, TB97Tlbr,
  MRUFList,
  BrowseDr,
  dfsStatusBar,
  CRC32,
  gf_FileAssoc,
  Placemnt,
  RxRichEd,
  RXShell,
  RxNotify,
  //StrUtils,       //[dpv]
  RxStrUtils,
  RXCombos,
  RXCtrls,
  // RxCalc,
  TopWnd,
  RichPrint,
  TreeNT,
  ColorPicker,
  Langs,
  { Own units - covered by KeyNote's MPL}
  gf_misc, gf_files, gf_Const,
  gf_strings, gf_miscvcl,
  kn_INI, kn_Cmd, kn_Msgs,
  kn_Info,
  {$IFDEF MJ_DEBUG}
  GFLog,
  {$ENDIF}
  kn_NoteObj,
  kn_FileInfo, kn_Const,
  kn_Defaults,
  kn_About,
  kn_DateTime,
  kn_Chest, kn_TabSelect,
  kn_URL, kn_Find, kn_Replace,
  kn_NodeList,
  kn_StyleObj,
  kn_RTFUtils,
  kn_Pass,
  kn_Macro, kn_MacroEdit, kn_MacroCmd,
  kn_Plugins,
  kn_filemgr,
  {$IFNDEF EXCLUDEEMAIL}
  kn_SendMail,
  {$ENDIF}
  kn_LocationObj,
  kn_LinksMng,
  dll_Keyboard,
  kn_VCLControlsMng,
  //WinHelpViewer,                 //*1 Lo añado y lo quito al final, porque no va fino (no es posible abrir la tabla de contenidos)
  HTMLHelpViewer,                 //*1
  ImgList;


type
  // cracker class, enables us to get at the protected
  // .EditorHandle property of the combo box control.
  // We need the handle so that we can send EM_xxx messages
  // to the control for undo, canundo, copy, paste, etc.
  TEditHandleComboBox = class( TCustomComboBox );
  TWordWrapMemo = class( TCustomMemo );


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
    N1: TMenuItem;
    MMHelpAbout: TMenuItem;
    MMToolsOptions: TMenuItem;
    MMNoteNew: TMenuItem;
    MMNoteProperties: TMenuItem;
    N2: TMenuItem;
    MMNoteRemove: TMenuItem;
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
    StatusBar: TdfsStatusBar;
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
    TB_Exit: TToolbarButton97;
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
    PrintDlg: TPrintDialog;
    N27: TMenuItem;
    MMNoteClipCapture: TMenuItem;
    N28: TMenuItem;
    MMFilePageSetup: TMenuItem;
    MMNotePrintPreview_: TMenuItem;
    MMEditCopyAll: TMenuItem;
    MMEditPasteAsNewNote: TMenuItem;
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
    Menu_TV: TPopupMenu;
    TVInsertNode: TMenuItem;
    TVAddNode: TMenuItem;
    TVAddChildNode: TMenuItem;
    N30: TMenuItem;
    TVDeleteNode: TMenuItem;
    N31: TMenuItem;
    TVSortNodes_: TMenuItem;
    N32: TMenuItem;
    TVRenameNode: TMenuItem;
    TVPasteNode_: TMenuItem;
    TVPasteNodeName: TMenuItem;
    TVPasteNodeNameAsDate: TMenuItem;
    TVPasteNodeNameAsTime: TMenuItem;
    TVPasteNodeNameAsDateTime: TMenuItem;
    TVSortSubtree: TMenuItem;
    TVSortTree: TMenuItem;
    MRUMenu: TPopupMenu;
    MruM_MRUSeparatorBTN_: TMenuItem;
    N33: TMenuItem;
    MMTree_: TMenuItem;
    MMTreeAdd_: TMenuItem;
    Toolbar_Tree: TToolbar97;
    TB_NodeDelete: TToolbarButton97;
    TB_NodeFirst: TToolbarButton97;
    TB_NodeLast: TToolbarButton97;
    TB_NodeChild: TToolbarButton97;
    IMG_TV: TImageList;
    TB_NodeRename: TToolbarButton97;
    MMViewToolbars_: TMenuItem;
    MMViewTBTree: TMenuItem;
    MMTreeInsert_: TMenuItem;
    MMTreeAddChild_: TMenuItem;
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
    N37: TMenuItem;
    MMTreeFullExpand: TMenuItem;
    MMTreeFullCollapse: TMenuItem;
    MMViewNodeIcons: TMenuItem;
    TVDeleteChildren: TMenuItem;
    N40: TMenuItem;
    TVMovenode_: TMenuItem;
    TVMoveNodeUp: TMenuItem;
    TVMoveNodeDown: TMenuItem;
    TVMoveNodeLeft: TMenuItem;
    TVMoveNodeRight: TMenuItem;
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
    TVCopyNodeName: TMenuItem;
    N42: TMenuItem;
    N43: TMenuItem;
    RTFMWordWeb: TMenuItem;
    TB_WordWeb: TToolbarButton97;
    sm7: TToolbarSep97;
    N44: TMenuItem;
    TVVirtualNode: TMenuItem;
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
    TVTransfer_: TMenuItem;
    TVCopySubtree: TMenuItem;
    TVGraftSubtree: TMenuItem;
    N56: TMenuItem;
    TVEraseTreeMem: TMenuItem;
    MMInsertTerm: TMenuItem;
    MMToolsGlosAddTerm: TMenuItem;
    TVExport: TMenuItem;
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
    N67: TMenuItem;
    MMInsertFileContents: TMenuItem;
    N68: TMenuItem;
    N69: TMenuItem;
    MMToolsGlosEdit: TMenuItem;
    N70: TMenuItem;
    RTFMFont: TMenuItem;
    RTFMPara: TMenuItem;
    TB_ParaDlg: TToolbarButton97;
    sf8: TToolbarSep97;
    TVRefreshVirtualNode: TMenuItem;
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
    MMHelpContents: TMenuItem;
    MMHelpKeyboardRef: TMenuItem;
    MMHelpMain: TMenuItem;
    MMTemplates_: TMenuItem;
    MMToolsTemplateCreate: TMenuItem;
    MMToolsTemplateInsert: TMenuItem;
    MMViewCheckboxesAllNodes: TMenuItem;
    N80: TMenuItem;
    TVBoldNode: TMenuItem;
    TVCheckNode: TMenuItem;
    MMViewTree: TMenuItem;
    TVNodeColor_: TMenuItem;
    TVDefaultNodeFont: TMenuItem;
    MMFormatLanguage: TMenuItem;
    MMNoteSpell: TMenuItem;
    Pages_Res: TPage95Control;
    Splitter_Res: TSplitter;
    MMViewResPanel: TMenuItem;
    ResTab_Find: TTab95Sheet;
    ResTab_RTF: TTab95Sheet;
    ResTab_Macro: TTab95Sheet;
    ResTab_Template: TTab95Sheet;
    Res_RTF: TRxRichEdit;
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
    List_ResFind: TTextListBox;
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
    CB_ResFind_NodeNames: TCheckBox;
    RG_ResFind_Type: TRadioGroup;
    TVCopyNode_: TMenuItem;
    TVCopyNodePath: TMenuItem;
    TVCopyNodeText: TMenuItem;
    N88: TMenuItem;
    TVCopyPathtoEditor: TMenuItem;
    Menu_FindAll: TPopupMenu;
    FAMCopytoEditor: TMenuItem;
    FAMCopyAlltoEditor: TMenuItem;
    MMTreeNodeFromSel: TMenuItem;
    TVAddSibling: TMenuItem;
    MMTreeAddSibling_: TMenuItem;
    TVVirtualNode_: TMenuItem;
    ResTab_Favorites: TTab95Sheet;
    ListBox_ResFav: TGFXListBox;
    ResMFavTab: TMenuItem;
    Menu_Fav: TPopupMenu;
    FavMJump: TMenuItem;
    N89: TMenuItem;
    FavMAdd: TMenuItem;
    FavMDel: TMenuItem;
    N90: TMenuItem;
    TVUnlinkVirtualNode: TMenuItem;
    FavMAddExternal: TMenuItem;
    N91: TMenuItem;
    MMEditPasteAsNewNode: TMenuItem;
    StdEMWordWrap: TMenuItem;
    MMTreeOutlineNum: TMenuItem;
    N92: TMenuItem;
    N93: TMenuItem;
    MMTreeGoBack: TMenuItem;
    MMTreeGoForward: TMenuItem;
    Sep9722: TToolbarSep97;
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
    TVSelectNodeImage: TMenuItem;
    N102: TMenuItem;
    MMViewCustomIcons: TMenuItem;
    N103: TMenuItem;
    TVNodeTextColor: TMenuItem;
    TVNodeBGColor: TMenuItem;
    MMToolsCustomKBD: TMenuItem;
    N61: TMenuItem;
    N82: TMenuItem;
    MMTreeNodeNameAsSel: TMenuItem;
    N104: TMenuItem;
    TVPasteNodeNameAsSel: TMenuItem;
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
    MMToolsURL: TMenuItem;
    MMEditDecimalToRoman: TMenuItem;
    N112: TMenuItem;
    MMEditRomanToDecimal: TMenuItem;
    MMViewStatusBar: TMenuItem;
    FavMProperties: TMenuItem;
    N113: TMenuItem;
    TMClipCap: TMenuItem;
    Img_System: TdfsSystemImageList;
    MMEditPasteAsWebClip: TMenuItem;
    MMP_PasteAsWebClip: TMenuItem;
    MMViewHideCheckedNodes: TMenuItem;
    TB_HideChecked: TToolbarButton97;
    TVChildrenCheckbox: TMenuItem;
    CB_ResFind_HiddenNodes: TCheckBox;
    CB_ResFind_Filter: TCheckBox;
    TB_FilterTree: TToolbarButton97;
    MMViewFilterTree: TMenuItem;
    TB_AlarmNode: TToolbarButton97;
    N114: TMenuItem;
    TVAlarmNode: TMenuItem;
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure TVAlarmNodeClick(Sender: TObject);
    procedure TB_AlarmNodeMouseEnter(Sender: TObject);
    procedure TB_AlarmNodeClick(Sender: TObject);
    procedure MMViewFilterTreeClick(Sender: TObject);
    procedure TB_FilterTreeClick(Sender: TObject);
    procedure PagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVChildrenCheckboxClick(Sender: TObject);
    procedure TVHideCheckedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
    procedure MMFileSaveClick(Sender: TObject);
    procedure MMFileSaveAsClick(Sender: TObject);
    procedure MMNoteNewClick(Sender: TObject);
    procedure MMHelpTipClick(Sender: TObject);
    procedure MMFileCloseClick(Sender: TObject);
    procedure MMNoteRenameClick(Sender: TObject);
    procedure MMFilePropertiesClick(Sender: TObject);
    procedure MMFileAutoSaveClick(Sender: TObject);
    procedure RxRTFChange(Sender: TObject);
    procedure RxRTFKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RxRTFProtectChange(Sender: TObject; StartPos,
      EndPos: Integer; var AllowChange: Boolean);
    procedure RxRTFProtectChangeEx(Sender: TObject;
      const Message: TMessage; StartPos, EndPos: Integer;
      var AllowChange: Boolean);
    procedure RxRTFSelectionChange(Sender: TObject);
    procedure RxRTFURLClick(Sender: TObject; const URLText: String; chrg: _charrange; Button: TMouseButton);
    procedure RxRTFKeyPress(Sender: TObject; var Key: Char);
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
    procedure MRUMRUItemClick(Sender: TObject; AFilename: String);
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
    procedure MMNoteRemoveClick(Sender: TObject);
    procedure MMFileCopyToClick(Sender: TObject);
    procedure MMFindGoToClick(Sender: TObject);
    procedure MMToolsImportClick(Sender: TObject);
    procedure MMNotePropertiesClick(Sender: TObject);
    procedure TB_ExitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MMSortClick(Sender: TObject);
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
    procedure RichPrinterBeginDoc(Sender: TObject);
    procedure RichPrinterEndDoc(Sender: TObject);
    procedure MMNotePrintPreview_Click(Sender: TObject);
    procedure MMEditCopyAllClick(Sender: TObject);
    procedure MMEditPasteAsNewNoteClick(Sender: TObject);
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

    procedure TVChange(Sender: TObject; Node: TTreeNTNode);
    procedure TVEdited(Sender: TObject; Node: TTreeNTNode; var S: String);
    procedure TVEditCanceled(Sender: TObject);
    procedure TVEditing(Sender: TObject; Node: TTreeNTNode; var AllowEdit: Boolean);
    procedure TVDblClick(Sender: TObject);
    procedure TVDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TVDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TVEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure TVKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TVStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure TVAddNodeClick(Sender: TObject);
    (* procedure TVM_SelectIconClick(Sender: TObject); REMOVED *)
    procedure TVInsertNodeClick(Sender: TObject);
    procedure TVAddChildNodeClick(Sender: TObject);
    procedure MMRenamenodeClick(Sender: TObject);
    procedure MMViewTBTreeClick(Sender: TObject);
    procedure TVDeleteNodeClick(Sender: TObject);
    procedure TVMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TVDeletion(Sender: TObject; Node: TTreeNTNode);
    procedure MMTreeFullExpandClick(Sender: TObject);
    procedure MMTreeFullCollapseClick(Sender: TObject);
    procedure TVClick(Sender: TObject);
    {
    procedure TVBeforeItemPaint(Sender: TObject;
      Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates;
      var OwnerDraw: Boolean);
    }
    procedure TVSortSubtreeClick(Sender: TObject);
    procedure TVDeleteChildrenClick(Sender: TObject);
    procedure TVSortTreeClick(Sender: TObject);
    procedure TVMoveNodeUpClick(Sender: TObject);
    procedure TVMoveNodeDownClick(Sender: TObject);
    procedure TVMoveNodeLeftClick(Sender: TObject);
    procedure TVMoveNodeRightClick(Sender: TObject);
    procedure TVPasteNodeNameClick(Sender: TObject);
    procedure TVCopyNodeNameClick(Sender: TObject);
    procedure MMViewNodeIconsClick(Sender: TObject);
    procedure TVVirtualNodeClick(Sender: TObject);
    procedure PagesDblClick(Sender: TObject );
    procedure MMFindNodeClick(Sender: TObject);
    procedure MMFindNodeNextClick(Sender: TObject);
    procedure MMMergeNotestoFileClick(Sender: TObject);
    procedure RxRTFMouseDown(Sender: TObject;   Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure MMFormatNoHighlightClick(Sender: TObject);
    procedure TB_ColorClick(Sender: TObject);
    procedure TB_HiliteClick(Sender: TObject);
    procedure TVCopySubtreeClick(Sender: TObject);
    procedure PagesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PagesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MMInsertTermClick(Sender: TObject);
    procedure MMToolsGlosAddTermClick(Sender: TObject);
    procedure Toolbar_FormatClose(Sender: TObject);
    procedure TVExportClick(Sender: TObject);
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
    procedure TVRefreshVirtualNodeClick(Sender: TObject);
    procedure MMHelpVisitWebsiteClick(Sender: TObject);
    procedure MMHelpEmailAuthorClick(Sender: TObject);
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
    procedure MMHelpContentsClick(Sender: TObject);
    procedure MMHelpKeyboardRefClick(Sender: TObject);
    procedure MMHelpMainClick(Sender: TObject);
    // procedure MMToolsCalculatorClick(Sender: TObject);
    procedure MMToolsTemplateCreateClick(Sender: TObject);
    procedure MMToolsTemplateInsertClick(Sender: TObject);
    procedure TVKeyPress(Sender: TObject; var Key: Char);
    procedure MMViewCheckboxesAllNodesClick(Sender: TObject);
    procedure TVChecked(Sender: TObject; Node: TTreeNTNode);
    procedure TVChecking(Sender: TObject; Node: TTreeNTNode;
      var AllowCheck: Boolean);
    procedure TVCheckNodeClick(Sender: TObject);
    procedure TVBoldNodeClick(Sender: TObject);
    procedure MMViewTreeClick(Sender: TObject);
    procedure MMFormatLanguageClick(Sender: TObject);
    procedure MMNoteSpellClick(Sender: TObject);
    procedure Markaslink1Click(Sender: TObject);
    procedure Hiddentext1Click(Sender: TObject);
    procedure MMInsHyperlinkClick(Sender: TObject);
    procedure Combo_FontKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MMViewResPanelClick(Sender: TObject);
    procedure Pages_ResChange(Sender: TObject);
    procedure ListBox_ResMacroDblClick(Sender: TObject);
    procedure ListBox_ResTplDblClick(Sender: TObject);
    procedure TPLMTplDeleteClick(Sender: TObject);
    procedure TPLMTplInsertClick(Sender: TObject);
    procedure Menu_RTFPopup(Sender: TObject);
    procedure Splitter_ResMoved(Sender: TObject);
    procedure Btn_ResFlipClick(Sender: TObject);
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
    procedure Combo_ResFindChange(Sender: TObject);
    procedure Btn_ResFindClick(Sender: TObject);
    procedure Combo_ResFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Combo_FontSizeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure List_ResFindDblClick(Sender: TObject);
    procedure List_ResFindKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ResMPanelRightClick(Sender: TObject);
    procedure StdEMSelectAllClick(Sender: TObject);
    procedure Menu_StdEditPopup(Sender: TObject);
    procedure MMTreeMasterNodeClick(Sender: TObject);
    procedure TVCopyNodePathClick(Sender: TObject);
    procedure TVCopyNodeTextClick(Sender: TObject);
    procedure TVCopyPathtoEditorClick(Sender: TObject);
    procedure FAMCopytoEditorClick(Sender: TObject);
    procedure FAMCopyAlltoEditorClick(Sender: TObject);
    procedure RG_ResFind_TypeClick(Sender: TObject);
    procedure MMTreeNodeFromSelClick(Sender: TObject);
    procedure TVAddSiblingClick(Sender: TObject);
    procedure FavMJumpClick(Sender: TObject);
    procedure FavMAddClick(Sender: TObject);
    procedure FavMDelClick(Sender: TObject);
    procedure ListBox_ResFavClick(Sender: TObject);
    procedure ListBox_ResTplClick(Sender: TObject);
    procedure TVUnlinkVirtualNodeClick(Sender: TObject);
    procedure FavMAddExternalClick(Sender: TObject);
    procedure MMEditPasteAsNewNodeClick(Sender: TObject);
    {
    procedure RTFMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    }
    procedure MMTreeOutlineNumClick(Sender: TObject);
    procedure MMTreeGoBackClick(Sender: TObject);
    procedure MMTreeGoForwardClick(Sender: TObject);
    procedure MMUpRomanClick(Sender: TObject);
    procedure MMHelpWhatsNewClick(Sender: TObject);
    procedure MMViewTBRefreshClick(Sender: TObject);
    procedure TVSelectNodeImageClick(Sender: TObject);
    procedure TVNodeTextColorClick(Sender: TObject);
    procedure TVDefaultNodeFontClick(Sender: TObject);
    procedure TVNodeBGColorClick(Sender: TObject);
    procedure Res_RTFKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RxRTFStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);
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
    procedure List_ResFindDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure Menu_TVPopup(Sender: TObject);
    procedure MMViewTBSaveConfigClick(Sender: TObject);
    procedure Menu_TimePopup(Sender: TObject);
    procedure Menu_DatePopup(Sender: TObject);
    procedure md25Click(Sender: TObject);
    procedure mt8Click(Sender: TObject);
    procedure ms11Click(Sender: TObject);
    procedure Menu_SymbolsPopup(Sender: TObject);
    procedure MMViewTBInsertClick(Sender: TObject);
    procedure MMToolsURLClick(Sender: TObject);
    procedure Combo_StyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure DoBorder1Click(Sender: TObject);
    procedure MMEditDecimalToRomanClick(Sender: TObject);
    procedure MMEditRomanToDecimalClick(Sender: TObject);
    procedure MMViewStatusBarClick(Sender: TObject);
    procedure FavMPropertiesClick(Sender: TObject);
    procedure MMEditPasteAsWebClipClick(Sender: TObject);

    // Antes privadas...
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure DisplayAppHint( sender: TObject );
    procedure ShowException( Sender : TObject; E : Exception );

  private
    { Private declarations }
    procedure AppMessage( var Msg : TMsg; var Handled : boolean );
    procedure WMActivate( Var msg: TWMActivate ); message WM_ACTIVATE;
    procedure WMHotkey( Var msg: TWMHotkey ); message WM_HOTKEY; // for Activation Hotkey
    procedure WMQueryEndSession (var Msg : TMessage); message WM_QUERYENDSESSION;
    procedure WndProc( var M : TMessage ); override;
    procedure AppDeactivate( sender : TObject );
    procedure WMChangeCBChain( var Msg : TWMChangeCBChain); message WM_CHANGECBCHAIN;
    procedure WMDrawClipboard( var Msg : TWMDrawClipboard); message WM_DRAWCLIPBOARD; // for Clipboard capture
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMCopyData(Var msg: TWMCopyData); message WM_COPYDATA; // interprocess comm.
    procedure WMJumpToKNTLink( var DummyMSG : integer ); message WM_JUMPTOKNTLINK; // custom
    procedure WMJumpToLocation( var DummyMSG : integer ); message WM_JUMPTOLOCATION; //custom
    procedure WMShowTipOfTheDay( var DummyMSG : integer ); message WM_TIPOFTHEDAY; //custom
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;

  public

    RichPrinter: TRichPrinter;
    procedure UpdateWordWrap;

    {$IFDEF WITH_TIMER}
    procedure StoreTick( const Msg : string; const Tick : integer );
    procedure SaveTicks;
    {$ENDIF}

    procedure AssignOnMessage;

    function NoteSelText : TRxTextAttributes;

    procedure ActivatePreviousInstance; // sends message to already-running instance and shuts down this instance
    procedure CloseNonModalDialogs; // close all non-modal dialogs that might be open

    // perform commands requested by messages sent from other instances, plugins, etc.
    procedure ProcessControlMessage( const msgID : integer; kntmsg : TKeyNoteMsg  );

    // misc file-related functions
    procedure OnFileDropped( Sender : TObject; FileList : TStringList );

    // sanity checks. Many functions cannot be performed
    // if we have no active note or if the note is read-only
    function HaveNotes( const Warn, CheckCount : boolean ) : boolean;
    function NoteIsReadOnly( const aNote : TTabNote; const Warn : boolean ) : boolean;

    // search / replace
    procedure FindNotify( const Active : boolean );

    procedure GoToEditorLine( s : string );

    procedure FindTreeNode;

    // status bar etc. display updates
    procedure ShowInsMode;

    // config file management
    procedure SetupToolbarButtons;
    procedure ResolveToolbarRTFv3Dependencies;

    function ExtIsRTF( const aExt : string ) : boolean;
    function ExtIsHTML( const aExt : string ) : boolean;
    function ExtIsText( const aExt : string ) : boolean;

    // VCL updates when config loaded or changed
    procedure UpdateStatusBarState;

    procedure UpdateTreeVisible( const ANote : TTreeNote );

    procedure UpdateTabAndTreeIconsShow;
    procedure AutoSaveToggled;
    procedure GetKeyStates;
    procedure OnNoteLoaded( sender : TObject );

    // misc stuff
    procedure ShowAbout;
    procedure NotImplemented( const aStr : string );
    procedure ErrNoTextSelected;
    procedure NewVersionInformation;
    procedure DisplayHistoryFile;

    procedure AnotherInstance;
    procedure ShiftTab( const ShiftRight : boolean );

    procedure NodesDropOnTabProc( const DropTab : TTab95Sheet );

    procedure Form_CharsClosed( sender : TObject );
    procedure CharInsertProc( const ch : char; const Count : integer; const FontName : string; const FontCharset : TFontCharset );

    procedure HotKeyProc( const TurnOn : boolean );

    {$IFDEF WITH_IE}
    function SelectVisibleControlForNode( const aNode : TNoteNode ) : TNodeControl;
    {$ENDIF}

    procedure FilterApplied (note: TTreeNote);   // [dpv]
    procedure FilterRemoved (note: TTreeNote);   // [dpv]
  end;

function GetFilePassphrase( const FN : string ) : string;
function IsAParentOf( aPerhapsParent, aChild : TTreeNTNode ) : boolean;


var
  Form_Main: TForm_Main;

implementation
uses RxGIF{, jpeg}, kn_Global, kn_ExportNew,
     kn_NoteMng, kn_MacroMng, kn_PluginsMng, kn_TreeNoteMng, kn_TemplateMng,
     kn_FindReplaceMng, kn_ConfigFileMng, kn_DLLmng,
     kn_StyleMng, kn_FavoritesMng, kn_BookmarksMng,
     kn_VirtualNodeMng, kn_NoteFileMng, kn_EditorUtils;

{$R *.DFM}
{$R catimages}

const
  _TIMER_INTERVAL = 10000; // ten seconds


// callback from TNoteFile, to prompt for passphrase
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
          HotKeySuccess := true;
        end
        else
        begin
          HotKeySuccess := false;
          if ( KeyOptions.HotKeyWarn and KeyOptions.SingleInstance ) then
          begin
            // No warning if we can be running more than 1 instance of KeyNote,
            // because only the first instance will be able to register the hotkey,
            // so we would ALWAYS be getting the damn warning on each subsequent instance.
            Messagedlg( Format(
              'Unable to assign "%s" as activation hotkey.',
              [ShortCutToText( KeyOptions.HotKey )] ),
              mtWarning, [mbOK], 0 );
          end;
        end;
      end;
    end
    else
    begin
      if HotKeySuccess then
        UnRegisterHotkey( Handle, 1 );
      HotKeySuccess := false;
    end;

  except
    On E : Exception do
    begin
      HotKeySuccess := false;
      messagedlg( Format(
        'Unexpcted error while turning %s Activation hotkey "%s": %s',
        [TOGGLEARRAY[TurnOn], ShortCutToText( KeyOptions.HotKey ), E.Message]
        ), mtError, [mbOK], 0 );

    end;
  end;

  if HotKeySuccess then
    TMRestore.Caption := Format(
      '&Restore (%s)', [ShortCutToText( KeyOptions.HotKey )]
    )
  else
    TMRestore.Caption := '&Restore';

end; // HotKeyProc



procedure TForm_Main.FormCreate(Sender: TObject);
begin
  InitializeKeynote(Self);
end; // CREATE

procedure TForm_Main.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;

  {$IFDEF WITH_TIMER}
  StoreTick( 'Begin FormActivate', GetTickCount );
  {$ENDIF}

  OnActivate := nil; // don't return here again

  try
    {$IFDEF WITH_TIMER}
    StoreTick( 'Begin respanel', GetTickCount );
    {$ENDIF}
    UpdateResPanelContents;
    Splitter_ResMoved( Splitter_Res );
    // Pages_Res.Visible := KeyOptions.ResPanelShow;
    HideOrShowResPanel( KeyOptions.ResPanelShow );
    Btn_ResFind.Enabled := ( Combo_ResFind.Text <> '' );
  except
  end;

  EnableOrDisableUAS;

  {$IFDEF WITH_TIMER}
  StoreTick( 'End respanel - begin display', GetTickCount );
  {$ENDIF}

  Application.ProcessMessages;
  {
  if KeyOptions.TipOfTheDay then
    ShowTipOfTheDay;
  }
  Initializing := false;

  FocusActiveNote;

  {$IFDEF WITH_TIMER}
  StoreTick( 'Begin automacro', GetTickCount );
  {$ENDIF}

  if ( KeyOptions.RunAutoMacros and ( StartupMacroFile <> '' )) then
  begin
    if ( pos( '\', StartupMacroFile ) = 0 ) then
      StartupMacroFile := Macro_Folder + StartupMacroFile;
    if fileexists( StartupMacroFile ) then
    begin
      Application.ProcessMessages;
      ExecuteMacro( StartupMacroFile, '' );
    end;
  end;

  {$IFDEF WITH_TIMER}
  StoreTick( 'End automacro - begin autoplugin', GetTickCount );
  {$ENDIF}

  if ( StartupPluginFile <> '' ) then
  begin
    if ( pos( '\', StartupPluginFile ) = 0 ) then
      StartupPluginFile := Plugin_Folder + StartupPluginFile;
    if fileexists( StartupPluginFile ) then
    begin
      Application.ProcessMessages;
      ExecutePlugin( StartupPluginFile );
    end;
  end;

  {$IFDEF WITH_TIMER}
  StoreTick( 'End autoplugin', GetTickCount );
  {$ENDIF}

  if KeyOptions.TipOfTheDay then
    postmessage( Handle, WM_TIPOFTHEDAY, 0, 0 );

  opt_Minimize := ( opt_Minimize or KeyOptions.StartMinimized );

  MMViewOnTop.Checked := KeyOptions.AlwaysOnTop;
  TB_OnTop.Down := MMViewOnTop.Checked;

  if opt_Minimize then
    Application.Minimize
  else
    WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;

  Combo_Font.OnClick  := Combo_FontChange;
  Combo_Font.OnChange := nil;
  Combo_FontSize.OnClick := Combo_FontSizeClick;
  Combo_Zoom.OnClick := Combo_FontSizeClick;

  {$IFDEF MJ_DEBUG}
  Log.Add( 'Exiting ACTIVATE' );
  {$ENDIF}
  // Timer.Enabled := true;
  Application.OnDeactivate := AppDeactivate;
  // FolderMon.onChange := FolderMonChange;

  {$IFDEF WITH_TIMER}
  StoreTick( 'End FormActivate', GetTickCount );
  SaveTicks;
  {$ENDIF}

end; // ACTIVATE

procedure TForm_Main.ActivatePreviousInstance;
var
  CopyData : TCopyDataStruct;
  msg : TKeyNoteMsg;
begin
  if ( NoteFileToLoad <> '' ) then
  begin
    msg.strData := NoteFileToLoad;
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

  SendMessage( _OTHER_INSTANCE_HANDLE,
    WM_COPYDATA,
    Handle,
    integer( @copydata ));

end; // ActivatePreviousInstance


procedure TForm_Main.ProcessControlMessage(
  const msgID : integer;
  kntmsg : TKeyNoteMsg
  );
        function Check( mustbereadonly : boolean ) : boolean;
        begin
          result := ( assigned( NoteFile ) and
            HaveNotes( false, true ));
          if ( result and mustbereadonly ) then
            result := ( result and ( not NoteIsReadOnly( ActiveNote, false )));
        end;
begin
  case MsgID of
    KNT_MSG_PERFORMKEY : if Check( true ) then
    begin
      ShortCutToKey( kntmsg.intData1, LastRTFKey.Key, LastRTFKey.Shift );
      PostKeyEx( ActiveNote.Editor.Handle, LastRTFKey.Key, LastRTFKey.Shift, LastRTFKey.Special );
      Application.ProcessMessages;
    end;
    KNT_MSG_INSERTTEXT : if Check( true ) then
    begin
      ActiveNote.Editor.SelLength := 0;
      Text := kntmsg.strData;
      ActiveNote.Editor.SelLength := 0;
    end;
    KNT_MSG_MOVECARET : if Check( false ) then
    begin
      case kntmsg.intData1 of
        _CARET_RIGHT : with ActiveNote.Editor do
        begin
          Perform( WM_KEYDOWN, VK_RIGHT, 0 );
          Perform( WM_KEYUP, VK_RIGHT, 0 );
        end;
        _CARET_LEFT : with ActiveNote.Editor do
        begin
          Perform( WM_KEYDOWN, VK_LEFT, 0 );
          Perform( WM_KEYUP, VK_LEFT, 0 );
        end;
        _CARET_UP : with ActiveNote.Editor do
        begin
          Perform( WM_KEYDOWN, VK_UP, 0 );
          Perform( WM_KEYUP, VK_UP, 0 );
        end;
        _CARET_DOWN : with ActiveNote.Editor do
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
          ReadFuncKeys;
          StatusBar.Panels[PANEL_HINT].Text := 'Function key assignment updated';
        end;
      end;
    end;
  end;
end; // ProcessControlMessage


procedure TForm_Main.WMCopyData(Var msg: TWMCopyData);
var
  kntmsg : TKeyNoteMsg;
  ext : string;
begin
  ReplyMessage( 0 );

  // StatusBar.Panels[PANEL_HINT].Text := ' Received message from other instance';

  kntmsg := PKeyNoteMsg( msg.Copydatastruct^.lpData )^;

  case msg.Copydatastruct^.dwData of
    KNT_MSG_SHOW : begin
      NoteFileToLoad := '';
      CmdLineFileName := '';
    end;
    KNT_MSG_SHOW_AND_LOAD_FILE : begin
      if ( kntmsg.strData <> NO_FILENAME_TO_LOAD ) then
        NoteFileToLoad := normalFN( kntmsg.strData )
      else
        NoteFileToLoad := '';
      CmdLineFileName := '';
    end;
    KNT_MSG_SHOW_AND_EXECUTE_FILE : begin
      // CmdLineFileName
      NoteFileToLoad := '';
      CmdLineFileName := normalFN( kntmsg.strData );
    end;
    else
    begin
      ProcessControlMessage( msg.Copydatastruct^.dwData, kntmsg );
      exit;
    end;
  end;

  if IsIconic( Application.Handle ) then
    Application.Restore;
  Application.BringToFront;

  if ( NoteFileToLoad <> '' ) then
  begin
    if HaveNotes( false, false ) then
    begin
      if ( NoteFileToLoad = NoteFile.FileName ) then
      begin
        if ( PopupMessage( 'Revert to last saved version of' + #13 + NoteFile.Filename + '?', mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
        NoteFile.Modified := false; // to prevent automatic save if modified
      end;
    end;
    NoteFileOpen( NoteFileToLoad );
  end
  else
  if ( CmdLineFileName <> '' ) then
  begin
    if HaveNotes( false, false ) then
    begin
      ext := ansilowercase( extractfileext( CmdLineFileName ));
      if ( ext = ext_Macro ) then
      begin
        ExecuteMacro( CmdLineFileName, '' );
      end
      else
      if ( ext = ext_Plugin ) then
      begin
        ExecutePlugin( CmdLineFileName );
      end;
    end;
  end;

end; // WMCopyData

procedure TForm_Main.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
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
      CanClose := ( PopupMessage( 'OK to quit ' + Program_Name + '?', mtConfirmation, [mbYes,mbNo], 0 ) = mrYes );
  { note: if windows is closing, we do not honor the "Confirm exit" config option }

  try
    if CanClose then
    begin
      CanClose := CheckModified( not KeyOptions.AutoSave );

      try

        FindOptions.FindAllHistory := '';
        for i := 1 to Combo_ResFind.Items.Count do
        begin
          if ( i >= FindOptions.HistoryMaxCnt ) then break;
          if ( i > 1 ) then
            FindOptions.FindAllHistory := FindOptions.FindAllHistory + HISTORY_SEPARATOR + ANSIQuotedStr( Combo_ResFind.Items[pred( i )], '"' )
          else
            FindOptions.FindAllHistory := ANSIQuotedStr( Combo_ResFind.Items[0], '"' );
        end;

        SaveOptions;
        if Res_RTF.Modified then
          StoreResScratchFile;

        { SaveFuncKeys is not called, because key assignments
        do not change inside KeyNote }
        // SaveFuncKeys;

        if ( not opt_NoSaveOpt ) then
        begin
          SaveFileManagerInfo( MGR_FN );
          if StylesModified then
          begin
            SaveStyleManagerInfo( Style_FN );
            StylesModified := false;
          end;
          SaveFavorites( FAV_FN );
          if opt_NoRegistry then
            IniSaveToolbarPositions( Self, changefileext( INI_FN, ext_MRU ), 'TB97a' )
          else
            RegSaveToolbarPositions( Self, 'Software\General Frenetics\Keynote\FormPos\TB97a' );
        end;

      except
      end;
    end;

  finally
    if CanClose then
    begin
      {$IFDEF MJ_DEBUG}
      Log.Flush( true );
      Log.Add( 'CloseQuery result: ' + BOOLARRAY[CanClose] );
      {$ENDIF}
    end;
    AppIsClosing := true;
    ClosedByWindows := false;
    TerminateClick := false;
  end;
end; // CLOSEQUERY

procedure TForm_Main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  HotKeyProc( false ); // unregister hotkey
end; // FORM CLOSE

procedure TForm_Main.FormDestroy(Sender: TObject);
begin
  // close shop!!
  ClipCapActive := false;
  if ( ClipCapNextInChain <> 0 ) then
    SetClipCapState( false );

  FolderMon.Active := false;
  {$IFNDEF MJ_DEBUG}
  // in final release, set the event to NIL (keep only for debugging)
  Application.OnException := nil;
  {$ENDIF}
  Application.OnDeactivate := nil;
  Application.OnHint := nil;
  ActiveNote := nil;
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

      if assigned( NoteFile ) then NoteFile.Free;
      if assigned( TransferNodes ) then TransferNodes.Free;
      // DestroyVCLControls;

    except
      // at this stage, we can only swallow all exceptions (and pride)
      {$IFDEF MJ_DEBUG}
      on E : Exception do
      begin
        showmessage( 'Exception in OnDestroy: ' + #13#13 +E.Message );
        if assigned( Log ) then Log.Add( 'Exception in OnDestroy: ' + E.Message );
      end;
      {$ENDIF}
    end;
  finally
    {$IFDEF MJ_DEBUG}
    if assigned( Log ) then
    begin
      Log.Free;
    end;
    log := nil;
    {$ENDIF}
  end;

end; // DESTROY




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


procedure TForm_Main.SetupToolbarButtons;
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
  TB_EmailNote.Hint := MMNoteEmail.Hint;
  TB_Exit.Hint := MMFileExit.Hint;
  TB_FileInfo.Hint := MMFileproperties.Hint;
  TB_FileMgr.Hint := MMFileManager.Hint;
  TB_FileNew.Hint := MMFileNew.Hint;
  TB_FileOpen.Hint := MMFileOpen.Hint;
  TB_FileSave.Hint := MMFileSave.Hint;
  TB_Find.Hint := MMFind.Hint;
  TB_FindNext.Hint := MMFindNext.Hint;
  TB_FontDlg.Hint := MMFormatFont.Hint;
  TB_GoBack.Hint := MMTreeGoBack.Hint;
  TB_GoForward.Hint := MMTreeGoForward.Hint;
  TB_Indent.Hint := MMFormatFIndInc.Hint;
  TB_Italics.Hint := MMFormatItalics.Hint;
  TB_Macro.Hint := MMToolsMacroRun.Hint;
  // TB_MacroPause.Hint := .Hint;
  // TB_MacroRecord.Hint := .Hint;
  TB_NodeChild.Hint := MMTreeAddChild_.Hint;
  TB_NodeDelete.Hint := MMTreeNodeDelete_.Hint;
  TB_NodeFirst.Hint := MMTreeInsert_.Hint;
  TB_NodeLast.Hint := MMTreeAdd_.Hint;
  TB_NodeRename.Hint := MMTreeNodeRename_.Hint;
  TB_HideChecked.Hint := MMViewHideCheckedNodes.Hint;    // [dpv]
  TB_NoteDelete.Hint := MMNoteRemove.Hint;
  TB_NoteEdit.Hint := MMNoteProperties.Hint;
  TB_NoteNew.Hint := MMNoteNew.Hint;
  TB_Numbers.Hint := MMFormatNumbers.Hint;
  TB_OnTop.Hint := MMViewOnTop.Hint;
  TB_Options.Hint := MMToolsOptions.Hint;
  TB_Outdent.Hint := MMFormatFIndDec.Hint;
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

procedure TForm_Main.AppRestore(Sender: TObject);
begin
    TMRestore.Enabled := false;
    AppLastActiveTime := now;
    FormStorage.Options := [fpState,fpPosition];
    ShowWindow( Application.Handle, SW_SHOW );
    WinOnTop.AlwaysOnTop := KeyOptions.AlwaysOnTop;
    Application.BringToFront;
    if _REOPEN_AUTOCLOSED_FILE then
    begin
      _REOPEN_AUTOCLOSED_FILE := false;
      NoteFileOpen( KeyOptions.LastFile );
    end;
end; // AppRestore;

procedure TForm_Main.DisplayAppHint( sender: TObject );
begin
  if KeyOptions.StatBarShow then
    StatusBar.Panels[PANEL_HINT].Text := GetShortHint( application.hint );
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

procedure TForm_Main.TimerTimer(Sender: TObject);
const
  bools : array[false..true] of string = ( 'No', 'Yes' );
var
  Hrs, Mins : integer;
begin
  inc( Timer_Tick );
  {
    timer may trigger THREE kinds of things:
    1. auto-saving current file
    2. minimizing keynote and/or closing file after a period of inactivity.

    3. Show Alarms on nodes           [dpv*]
  }

  if ( Timer_Tick >= KeyOptions.AutoSaveOnTimerInt * ( 60000 div _TIMER_INTERVAL ) ) then
  begin
    Timer_Tick := 0;
    if Form_Main.HaveNotes( false, false ) then
    begin
      if (( not FileChangedOnDisk ) and NoteFile.Modified and KeyOptions.AutoSave and KeyOptions.AutoSaveOnTimer ) then
      begin
        if (( NoteFile.FileName <> '' ) and ( not NoteFile.ReadOnly )) then
        begin
          // only if saved previously
          {$IFDEF MJ_DEBUG}
          Log.Add( '-- Saving on TIMER' );
          {$ENDIF}
          NoteFileSave( NoteFile.FileName );
        end;
      end;
    end;
  end;

  try
    if KeyOptions.TimerClose then
    begin
      Hrs := ( KeyOptions.TimerCloseInt DIV 60 );
      Mins := ( KeyOptions.TimerCloseInt MOD 60 );
      if (( AppLastActiveTime + EncodeTime( Hrs, Mins, 0, 0 )) < Now ) then
      begin
        Timer_Tick := 0;
        AutoCloseFile;
        // auto-closing minimizes too, so we exit here
        exit;
      end;
    end;

    if KeyOptions.TimerMinimize then
    begin
      if ( not IsIconic( Application.Handle )) then
      begin
        Hrs := ( KeyOptions.TimerMinimizeInt DIV 60 );
        Mins := ( KeyOptions.TimerMinimizeInt MOD 60 );
        if (( AppLastActiveTime + EncodeTime( Hrs, Mins, 0, 0 )) < Now ) then
        begin
          Application.Minimize;
        end;
      end;
    end;

    AlarmManager.checkAlarms;    // [dpv]

  except
     // drop all exceptions here
  end;

end; // OnTimer

procedure TForm_Main.AppDeactivate( sender : TObject );
begin
  if FileChangedOnDisk then exit;
  if (( not ( AppIsClosing or Initializing or FileIsBusy )) and
      ( HaveNotes( false, false ))) then
  begin
    if ( KeyOptions.AutoSave and KeyOptions.AutoSaveOnFocus and NoteFile.Modified ) then
    begin
      if (( NoteFile.FileName <> '' ) and ( not NoteFile.ReadOnly )) then
      begin
        // only if saved previously
        {$IFDEF MJ_DEBUG}
        Log.Add( '-- Saving on Application DEACTIVATE' );
        {$ENDIF}
        NoteFileSave( NoteFile.FileName );
      end;
    end;
  end;
end; // AppDeactivate

procedure TForm_Main.WMActivate( Var msg: TWMActivate );
begin
  if ( msg.Active <> WA_INACTIVE ) then
  begin
    AppIsActive := true; // used with ClipCap to ignore copy events coming from Keynote itself
    if FileChangedOnDisk then
    begin
      {$IFDEF MJ_DEBUG}
      Log.Add( 'FileChangedOnDisk!' );
      {$ENDIF}
      FileChangedOnDisk := false;
      SomeoneChangedOurFile;
    end;
    AppIsClosing := false;
  end
  else
  begin
    AppIsActive := false;
  end;
  inherited;
end; // WMActivate

procedure TForm_Main.WMHotkey( Var msg: TWMHotkey );
begin
  if ( msg.hotkey = 1 ) then
  begin
    if IsIconic( Application.Handle ) then
      Application.Restore;
    Application.BringToFront;
  end;
end; // WMHotkey

procedure TForm_Main.WMQueryEndSession( var Msg : TMessage );
begin
  ClosedByWindows := true;
  {$IFDEF MJ_DEBUG}
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
  {$IFDEF MJ_DEBUG}
  if assigned( Log ) then
  begin
    Log.Add( '!! Unhandled exception: ' + e.message );
    Log.Flush( true );
  end;
  {$ENDIF}

  If Application.MessageBox(
    PChar( 'Unexpected error:  ' +
    E.Message + #13#13 +
    'This message may indicate a bug in KeyNote. If the problem persists, please contact the author at <' +
    Program_Email + '>.' + #13#13 +
    'You can continue working or terminate KeyNote. ' + #13 +
    'Terminate application?' ),
    'KeyNote Error',
    MB_YESNO+MB_SYSTEMMODAL+MB_ICONHAND+MB_DEFBUTTON2) = ID_YES Then
  begin
    ClosedByWindows := true; // means: don't display exit confirmation dialog
    TerminateClick := true;
    PostMessage( Handle, WM_QUIT, 0, 0 ); // docs say to use PostQuitMessage, but I've had problems with it
    Application.Terminate;
  end;
end; // ShowException

procedure TForm_Main.NotImplemented( const aStr : string );
begin
  PopupMessage( 'Function not implemented. ' + aStr, mtInformation, [mbOK], 0 );
  {$IFDEF MJ_DEBUG}
  Log.Add( 'Not implemented call: ' + aStr );
  {$ENDIF}
end; // NotImplemented

procedure TForm_Main.CloseNonModalDialogs;
begin
  if ( Form_Chars <> nil ) then
    Form_Chars.Close;
  if ( Form_Find <> nil ) then
    Form_Find.Close;
  if ( Form_Replace <> nil ) then
    Form_Replace.Close;
end; // CloseNonModalDialogs

procedure TForm_Main.MMFileNewClick(Sender: TObject);
begin
  NoteFileNew( '' );
end;

procedure TForm_Main.MMFileOpenClick(Sender: TObject);
begin
  NoteFileOpen( '' );
end; // MMOpenClick

procedure TForm_Main.MMFileSaveClick(Sender: TObject);
begin
  {$IFDEF MJ_DEBUG}
  Log.Add( '-- Saving on user request' );
  {$ENDIF}
  if ShiftDown then
  begin
    NoteFileSave( '' )
  end
  else
  begin
    if HaveNotes( false, false ) then
      NoteFileSave( NoteFile.FileName )
    else
      NoteFileSave( '' );
  end;
end;

procedure TForm_Main.MMFileSaveAsClick(Sender: TObject);
begin
  NoteFileSave( '' );
end;

procedure TForm_Main.MMNoteNewClick(Sender: TObject);
begin
  CreateNewNote;
end;

procedure TForm_Main.MMHelpTipClick(Sender: TObject);
begin
  ShowTipOfTheDay;
end;

procedure TForm_Main.MMFileCloseClick(Sender: TObject);
begin
  NoteFileClose;
end;

procedure TForm_Main.MMNoteRenameClick(Sender: TObject);
begin
  RenameNote;
end;

procedure TForm_Main.UpdateTabAndTreeIconsShow;
begin
  if assigned( NoteFile ) then
  begin
    MMViewTabIcons.Enabled := NoteFile.ShowTabIcons;
    if ( NoteFile.ShowTabIcons and TabOptions.Images ) then
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
  NoteFileProperties;
end;

procedure TForm_Main.MMFileAutoSaveClick(Sender: TObject);
begin
  KeyOptions.AutoSave := ( not KeyOptions.AutoSave );
  AutoSaveToggled;
end;

procedure TForm_Main.AutoSaveToggled;
begin
  MMFileAutoSave.Checked := KeyOptions.AutoSave;
  if ( not Initializing ) then
    UpdateNoteFileState( [fscModified] );
end; // AutoSaveToggled;

procedure TForm_Main.AnotherInstance;
begin
  // rcvStr contains the commandline passed to
  // the new instance. We use it to load
  // a different file.
  Application.Restore;
  Application.BringToFront;
  // NewFileRequest( rcvStr );
end; // AnotherInstance

procedure TForm_Main.ShowInsMode;
begin
  if ActiveNote.IsInsertMode then
    StatusBar.Panels[PANEL_INS].Text := ' INS'
  else
    StatusBar.Panels[PANEL_INS].Text := ' OVR';
end; // ShowInsMode

procedure TForm_Main.PagesDblClick(Sender: TObject );
begin
  if ShiftDown then
    EditNoteProperties( propThisNote )
  else
    RenameNote;
end; // PagesDblClick

procedure TForm_Main.RxRTFMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned( ActiveNote ) then ActiveNote.FocusMemory := focRTF;
end;


procedure TForm_Main.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//var
//  myKey: Word;
//  myShift: TShiftState;
begin
  case key of

    VK_TAB :
    if ( shift = [ssCtrl] ) then // Ctrl+Tab: switch to next tab
    begin
      key := 0;
      Pages.SelectNextPage( true );
      // RxRTFKeyProcessed := true;
    end
    else
    if ( shift = [ssShift] ) then // switch to previous tab
    begin
      Key := 0;
      RxRTFKeyProcessed := true; // do not handle this key in OnKeyPress
      if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
      begin
        TTreeNote( ActiveNote ).TV.SetFocus;
        ActiveNote.FocusMemory := focTree;
      end;
    end
    else
    begin
      if ( shift = [ssCtrl,ssShift] ) then
      begin
        key := 0;
        Pages.SelectNextPage( false );
      end;
    end;

    VK_PRIOR : if ( shift = [ssCtrl] ) then // Page Up
    begin
      key := 0;
      Pages.SelectNextPage( false );
    end
    else
    if ( KeyOptions.ResPanelShow and ( shift = [ssAlt] )) then
    begin
      key := 0;
      Pages_Res.SelectNextPage( false );
    end;

    VK_NEXT : if ( shift = [ssCtrl] ) then // Page Down
    begin
      key := 0;
      Pages.SelectNextPage( true );
    end
    else
    if ( KeyOptions.ResPanelShow and ( shift = [ssAlt] )) then
    begin
      key := 0;
      Pages_Res.SelectNextPage( true );
    end;

    VK_ESCAPE : begin
      _Is_Dragging_Text := false;
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
        begin
          UserBreak := true; // will abort search
        end
        else
        if IsRunningMacro then
        begin
          MacroAbortRequest := true; // will abort macro
        end
        else
        if IsRecordingMacro then
        begin
          TB_MacroRecordClick( TB_MacroRecord ); // aborts macro recording
        end
        else
        begin
          if ( activecontrol = Combo_FontSize ) or
             ( activecontrol = Combo_Font ) or
             ( activecontrol = List_ResFind ) or
             // ( activecontrol = Res_RTF ) or            // *1  (001)
             ( activecontrol = ListBox_ResMacro ) or
             ( activecontrol = ListBox_ResTpl ) or
             ( activecontrol = ListBox_ResPlugins ) or
             ( activecontrol = ListBox_ResFav ) or
             ( activecontrol = Combo_Zoom ) or
             ( activecontrol = Combo_Style ) then
          begin
            // if these controls are focused,
            // switch focus to editor
            key := 0;
            FocusActiveNote;
          end
          else
          if ( activecontrol = combo_resfind ) then
          begin
            combo_resfind.Text := '';
          end
          else
          begin
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

    VK_F1..VK_F12 : begin
      // perform custom Alt+FuncKey assignments
      if (( Key <> VK_F4 ) and ( Shift = [ssAlt] )) then // MUST NOT grab Alt+F4!!
      begin
        PerformCustomFuncKey( Key, Shift );
        Key := 0;
      end
      else
      if ( Shift = [ssShift,ssAlt] ) then
      begin
        PerformCustomFuncKey( Key, Shift );
        Key := 0;
      end
      else
      if ( Shift = [ssCtrl,ssAlt] ) then
      begin
        PerformCustomFuncKey( Key, Shift );
        Key := 0;
      end;
    end;

    VK_INSERT:
       if ( shift = [ssShift] ) then begin
         if CmdPaste(false) then key:= 0;
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

end; // KEY DOWN

procedure TForm_Main.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  myKey: Word;
  myShift: TShiftState;
begin
   if (GetKeyState(VK_CONTROL) < 0) then
   begin
      if Msg.CharCode = Ord('C') then begin
          if CmdCopy then Handled:= true;
      end
      else if Msg.CharCode = Ord('V') then begin
          if CmdPaste(false) then Handled:= true;
      end
      else if Msg.CharCode = Ord('X') then begin
          if CmdCut then Handled:= true;
      end;
   end;
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


procedure TForm_Main.RxRTFKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  line, col, indent : integer;
  s : string;
  ptCursor :  TPoint;
begin
  _IS_FAKING_MOUSECLICK := false;
  if assigned( ActiveNote ) then
    ActiveNote.FocusMemory := focRTF
  else
    exit;

  if ( not ( key in [16..18] )) then // skip bare modifier keys
  begin
    LastRTFKey.Key := Key;
    LastRTFKey.Shift := Shift;
    if IsRecordingMacro then
    begin
      AddMacroKeyPress( Key, Shift );
    end;
  end;

  if ( shift = [] ) then
  begin
    case Key of
      VK_INSERT : begin
        if EditorOptions.DisableINSKey then
        begin
          key := 0;
          StatusBar.Panels[PANEL_HINT].Text := ' Overwrite mode disabled through INI file';
        end
        else
          PerformCmdEx( ecInsOvrToggle );
      end;

      13 : if EditorOptions.AutoIndent
              and (TRxRichEdit(sender).Paragraph.TableStyle = tsNone)  // DPV
           then
      begin
        if NoteIsReadOnly( ActiveNote, true ) then exit;
        with ( sender as TRxRichEdit ) do
        begin
          // figure out line and column position of caret
          line := PerForm( EM_EXLINEFROMCHAR,0, SelStart );
          Col  := SelStart - Perform( EM_LINEINDEX,line,0 );

          // get part of current line in front of caret
          S:= Copy( lines[ line ], 1, col );

          // count blanks and tabs in this string
          indent := 0;
          while (indent < length( S )) and
                (S[indent+1] In  [' ',#9])
          do
            Inc( indent );

          if (indent = length(S)) and (TRxRichEdit(sender).Paragraph.Numbering <> nsNone) then begin

             If TRxRichEdit(sender).Paragraph.Numbering = nsBullet Then
                ActiveNote.Editor.Paragraph.FirstIndentRelative:= -2
             Else
                ActiveNote.Editor.Paragraph.FirstIndentRelative:= -4;

             TRxRichEdit(sender).Paragraph.Numbering :=  nsNone;
             end
          else
            if indent > 0 then begin
              key := 0;
              // insert a linebreak followed by the substring of blanks and tabs
              SelText := #13#10+Copy(S, 1, indent);
              SelStart := ( SelStart+SelLength ) -1;
            end;

        end;
      end;
    end;
  end
  else
  if ( shift = [ssShift] ) then
  begin
    case key of
      VK_F10 : begin
        key := 0;
        GetCursorPos( ptCursor );
        Menu_RTF.Popup( ptCursor.X, ptCursor.Y );
      end;
    end;
  end
  else
  if ( shift = [ssCtrl] ) then
  begin
    case Key of

      { grey  *}
      VK_MULTIPLY : if Combo_Style.Visible then
        try
          Combo_Style.SetFocus;
        except
        end;

      { grey / }
      111 : if KeyOptions.ResPanelShow then  // if Combo_Macro.Visible then
        try
          Pages_Res.ActivePage := ResTab_Macro;
          ListBox_ResMacro.SetFocus;
        except
        end;

      { backslash }
      220 : if ( Shift = [ssCtrl] ) then begin
        Key := 0;
        if ( ActiveNote.Kind = ntTree ) then
        begin
          TTreeNote( ActiveNote ).TV.SetFocus;
          ActiveNote.FocusMemory := focTree;
        end;
      end;
    end;
  {
  end
  else
  if ( ssAlt in Shift ) then
  begin
    case Key of
      VK_UP, VK_DOWN : if ( ActiveNote.Kind = ntTree ) then
      begin
        // do in editor what bare UP/DOWN arrow keys do in the tree
        // this allows user to switch to another
        // tree node without leaving the editor
        if ( not IsRecordingMacro ) then
        with TTreeNote( ActiveNote ).TV do
        begin
          Perform( WM_KEYDOWN, Key, 0 );
          Perform( WM_KEYUP, Key, 0 );
        end;
        key := 0;
      end;

      VK_RIGHT, VK_LEFT : if (( ssShift in Shift ) and ( ActiveNote.Kind = ntTree )) then
      begin
        // do in editor what bare LEFT/RIGHT arrow keys do in the tree
        // this allows user to switch to another
        // tree node without leaving the editor
        if ( not IsRecordingMacro ) then
        with TTreeNote( ActiveNote ).TV do
        begin
          Perform( WM_KEYDOWN, Key, 0 );
          Perform( WM_KEYUP, Key, 0 );
        end;
        key := 0;
      end;
    end;
  }
  end;

end; // RxRTFKeyDown

procedure TForm_Main.RxRTFKeyPress(Sender: TObject; var Key: Char);
var
  i : byte;
begin
  If ( RxRTFKeyProcessed or (( Key = #9 ) and ( GetKeyState( VK_CONTROL ) < 0 ))) Then
  begin
    Key := #0;
    RxRTFKeyProcessed := false;
    exit;
  end;

  case key of

    #9 : if ( GetKeyState( VK_CONTROL ) >= 0 ) then
    begin
      if not ( sender as TTabRichEdit ).UseTabChar then
      begin
        key := #0;
        for i := 1 to ( sender as TTabRichEdit ).TabSize do
          ( sender as TTabRichEdit ).Perform( WM_CHAR, 32, 0 );
      end;
    end;
  end;
end;  // RxRTF_KeyPress


procedure TForm_Main.RxRTFProtectChange(Sender: TObject; StartPos,
  EndPos: Integer; var AllowChange: Boolean);
begin
  AllowChange := EditorOptions.EditProtected;
end; // RxRTF_ProtectChange

procedure TForm_Main.RxRTFProtectChangeEx(Sender: TObject;
  const Message: TMessage; StartPos, EndPos: Integer;
  var AllowChange: Boolean);
begin
  AllowChange := EditorOptions.EditProtected;
end; // RxRTF_ProtectChangeEx

procedure TForm_Main.RxRTFSelectionChange(Sender: TObject);
var
  myRTF : TTabRichEdit;
begin
  RTFUpdating := true;
  try
    myRTF := ( sender as TTabRichEdit );
    Combo_Font.FontName := myRTF.SelAttributes.Name;
    Combo_FontSize.Text := inttostr( myRTF.SelAttributes.Size );

    TB_Bold.Down := fsBold in myRTF.SelAttributes.Style;
    MMFormatBold.Checked := TB_Bold.Down;

    TB_Italics.Down := fsItalic in myRTF.SelAttributes.Style;
    MMFormatItalics.Checked := TB_Italics.Down;

    TB_Underline.Down := fsUnderline in myRTF.SelAttributes.Style;
    MMFormatUnderline.Checked := TB_Underline.Down;

    TB_Strikeout.Down := fsStrikeOut in myRTF.SelAttributes.Style;
    MMFormatStrikeout.Checked := TB_Strikeout.Down;

    case myRTF.Paragraph.LineSpacing of
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

    if ( _LoadedRichEditVersion > 2 ) then
    begin
      TB_Bullets.Down := ( myRTF.Paragraph.Numbering = nsBullet );
      MMFormatBullets.Checked := TB_Bullets.Down;
      TB_Numbers.Down := ( not ( myRTF.Paragraph.Numbering in [nsNone, nsBullet] ));
      MMFormatNumbers.Checked := TB_Numbers.Down;
    end
    else
    begin
      TB_Bullets.Down := ( myRTF.Paragraph.Numbering <> nsNone );
      MMFormatBullets.Checked := TB_Bullets.Down;
    end;

    MMFormatDisabled.Checked := myRTF.SelAttributes.Disabled;
    MMFormatSubscript.Checked := myRTF.SelAttributes.SubscriptStyle = ssSubscript;
    TB_Subscript.Down := MMFormatSubscript.Checked;
    MMFormatSuperscript.Checked := myRTF.SelAttributes.SubscriptStyle = ssSuperscript;
    TB_Superscript.Down := MMFormatSuperscript.Checked;

    case myRTF.Paragraph.Alignment of
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

    UpdateCursorPos;
  finally
    RTFUpdating := false;
  end;
end; // RxRTFSelection Change

procedure TForm_Main.RxRTFChange(Sender: TObject);
begin
  if ( sender as TTabRichEdit ).Modified then
  begin
    NoteFile.Modified := true;
    if ( ActiveNote.Kind = ntTree ) then
    begin
      with TTreeNote( ActiveNote ) do
      begin
        if assigned( SelectedNode ) then
          SelectedNode.RTFModified := true
        else
          messagedlg( 'Tree node not assigned. Text will be lost.' + #13 + 'Please create a tree node first.', mtError, [mbOK], 0 );
      end;
    end;
  end;

  TB_EditUndo.Enabled := ( sender as TTabRichEdit ).CanUndo;
  TB_EditRedo.Enabled := ( sender as TTabRichEdit ).CanRedo;
  RTFMUndo.Enabled := TB_EditUndo.Enabled;

  UpdateWordCount;

end; // RxRTFChange

procedure TForm_Main.UpdateWordWrap;
var
  isWordWrap : boolean;
begin
  if assigned( ActiveNote ) and assigned( ActiveNote.Editor ) then
  begin
    isWordWrap := ActiveNote.Editor.WordWrap;
    MMFormatWordWrap.Checked := isWordWrap;
    TB_WordWrap.Down := isWordWrap;
    RTFMWordwrap.Checked := isWordWrap;
  end;
end; // UpdateWordWrap

(*
procedure TForm_Main.RxRTFDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  // Accept := true;
  statusbar.panels[0].text := inttostr( random( 100 ));
end; // RxRTFDragOver
*)


procedure TForm_Main.MMEditSelectAllClick(Sender: TObject);
begin
  {if Res_RTF.Focused then
    Res_RTF.SelectAll
  else}
    PerformCmd( ecSelectAll );
end;

procedure TForm_Main.MMFormatWordWrapClick(Sender: TObject);
begin
  {
  if Res_RTF.Focused then
  begin
    Res_RTF.WordWrap := ( not Res_RTF.WordWrap );
    if ( _LoadedRichEditVersion > 2 ) then
    begin
      Res_RTF.Undo; // kluge for loss of formatting!
      Res_RTF.SelLength := 0;
    end;
  end
  else
  }
    PerformCmd( ecWordWrap );
end;

procedure TForm_Main.MMNoteReadOnlyClick(Sender: TObject);
begin
  PerformCmdEx( ecReadOnly );
end;

procedure TForm_Main.MMEditEvaluateClick(Sender: TObject);
begin
  EvaluateExpression;
end;


procedure TForm_Main.PagesChange(Sender: TObject);
begin

  try
    if (( Pages.PageCount > 0 ) and assigned( Pages.ActivePage )) then
    begin
      ActiveNote := TTabNote( Pages.ActivePage.PrimaryObject );
      TAM_ActiveName.Caption := ActiveNote.Name;
      TB_Color.AutomaticColor := ActiveNote.EditorChrome.Font.Color;
      FocusActiveNote;
    end
    else
    begin
      ActiveNote := nil;
      TB_Color.AutomaticColor := clWindowText;
      TAM_ActiveName.Caption := '(none)';
    end;
  finally
    UpdateNoteDisplay;
    UpdateHistoryCommands;
    StatusBar.Panels[PANEL_HINT].Text := '';
  end;
end; // PagesChange

procedure TForm_Main.PagesTabShift(Sender: TObject);
begin
  try
    ActiveNote := TTabNote( Pages.ActivePage.PrimaryObject );
    if assigned( ActiveNote ) then
    begin
      ActiveNote.TabIndex := Pages.ActivePage.TabIndex;
      Pages.ActivePage.ImageIndex := ActiveNote.ImageIndex;
      TAM_ActiveName.Caption := ActiveNote.Name;
      FocusActiveNote;
    end
    else
    begin
      TAM_ActiveName.Caption := '';
    end;
  finally
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;

end; // TAB SHIFT

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
    CmdPaste(sender is TToolbarButton97);
end; // PASTE

procedure TForm_Main.MMEditDeleteClick(Sender: TObject);
begin
  {if Res_RTF.Focused then
  begin
    Res_RTF.Perform( WM_CLEAR, 0, 0 );
  end
  else}
    PerformCmd( ecDelete );
end; // DELETE

procedure TForm_Main.MMEditUndoClick(Sender: TObject);
begin
  {if Res_RTF.Focused then
    Res_RTF.Undo
  else}
    PerformCmd( ecUndo );
end;

procedure TForm_Main.MMEditRedoClick(Sender: TObject);
begin
  { if Res_RTF.Focused then
    Res_RTF.Redo
  else}
    PerformCmd( ecRedo );
end;

procedure TForm_Main.MMEditPasteAsTextClick(Sender: TObject);
begin
  PerformCmd( ecPastePlain );
end;

function TForm_Main.NoteSelText : TRxTextAttributes;
begin
  result := ActiveNote.Editor.SelAttributes;

  // this code below is taken from Borland's
  // RichEdit sample, but it's wrong. Even if
  // SelLength is 0, we must still use SelAttributes.
  {
  if ( ActiveNote.Editor.SelLength > 0 ) then
    result := ActiveNote.Editor.SelAttributes
  else
    result := ActiveNote.Editor.DefAttributes;
  }
end; // NoteSelText


procedure TForm_Main.ErrNoTextSelected;
begin
  StatusBar.Panels[PANEL_HINT].Text := ' Select some text before issuing this command.';
end;

procedure TForm_Main.NewVersionInformation;
begin
  if ( KeyOptions.IgnoreUpgrades or ( KeyOptions.LastVersion >= Program_VerStr )) then
    exit;
  KeyOptions.TipOfTheDay := true;
  KeyOptions.TipOfTheDayIdx := -1;
  case messagedlg(
    Format(
    'You seem to have upgraded KeyNote from version %s to %s.' + #13 +
    'Files "history.txt" and "%s" contain information about ' +
    'the latest changes and additions.' + #13#13 +
    'Do you want to view the file "history.txt" now?'
    , [KeyOptions.LastVersion, Program_VerStr, SampleFileName] ),
    mtInformation, [mbYes,mbNo], 0
  ) of
    mrYes : begin
      DisplayHistoryFile;
    end;
  end;

end; // NewVersionInformation

procedure TForm_Main.DisplayHistoryFile;
var
  fn : string;
begin
  fn := extractfilepath( Application.Exename ) + 'doc\history.txt';
  if fileexists( fn ) then
    ShellExecute( 0, 'open', PChar( fn ), nil, nil, SW_NORMAL )
  else
    messagedlg( Format( 'History file not found: "%s"', [fn] ), mtError, [mbOK], 0 );
end; // DisplayHistoryFile

procedure TForm_Main.Combo_FontChange(Sender: TObject);
var
  oldsel : TNotifyEvent;
begin
  oldsel := nil;
  if ( not assigned( ActiveNote )) then exit;
  oldSel := ActiveNote.Editor.OnSelectionChange;
  ActiveNote.Editor.OnSelectionChange := nil;
  try
    if ( ActiveNote.FocusMemory = focTree ) then
    begin
      SetTreeNodeFontFace( false, ShiftDown );
    end
    else
    begin
      PerformCmd( ecFontName );
    end;
    // do not jump when in tree
  finally
    if assigned( oldsel ) then
      ActiveNote.Editor.OnSelectionChange := oldsel;
  end;

end; // Combo_FontChange

procedure TForm_Main.Combo_FontKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( not assigned( ActiveNote )) then exit;
  if ( shift = [] ) then
  begin
    case key of
      VK_RETURN, VK_ESCAPE : begin
        if ( key = VK_RETURN ) then
        begin
          if ( ActiveNote.FocusMemory = focTree ) then
          begin
            SetTreeNodeFontFace( false, ShiftDown );
          end
          else
          begin
            PerformCmd( ecFontName );
          end;
        end;
        key := 0;
        try
          if ( ActiveNote.FocusMemory = focRTF ) then
            ActiveNote.Editor.SetFocus
          else
          if ( ActiveNote.FocusMemory = focTree ) then
            TTreeNote( ActiveNote ).TV.SetFocus;
        except
        end;
      end;
    end;
  end;
end; // Combo_FontKeyDown

procedure TForm_Main.Combo_FontSizeClick(Sender: TObject);
var
  oldsel : TNotifyEvent;
begin
  if ( RTFUpdating or FileIsBusy ) then exit;
  if (( Sender = Combo_FontSize ) and ( Combo_FontSize.Text = '' )) then exit;
  oldsel := nil;
  if assigned( ActiveNote ) then
  begin
    oldSel := ActiveNote.Editor.OnSelectionChange;
    ActiveNote.Editor.OnSelectionChange := nil;
  end;
  try
    {
    if ( ActiveNote.FocusMemory = focTree ) then
      SetTreeNodeFontSize( false, ShiftDown )
    else
    }

    if ( Sender = Combo_FontSize ) then
      PerformCmd( ecFontSize )
    else
    if ( Sender = Combo_Zoom ) then
      SetEditorZoom( -1, Combo_Zoom.Text );

  finally
    if assigned( oldsel ) then
      ActiveNote.Editor.OnSelectionChange := oldsel;
  end;
end; // Combo_FontSizeClick

procedure TForm_Main.Combo_FontSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ( not assigned( ActiveNote )) then exit;

  if ( not ( key in
    [#8, #9, #13, #27, #37..#40, #46, '0'..'9', '%'] )) then
  begin
    key := #0;
    exit;
  end;

  if ( key = #13 ) then
  begin
    key := #0;
    {
    if ( ActiveNote.FocusMemory = focTree ) then
    begin
      SetTreeNodeFontSize( ShiftDown );
    end
    else
    begin
      PerformCmd( ecFontSize );
    end;
    }

    if ( Sender = Combo_FontSize ) then
      PerformCmd( ecFontSize )
    else
    if ( Sender = Combo_Zoom ) then
      SetEditorZoom( -1, Combo_Zoom.Text );

    try
      if ( ActiveNote.FocusMemory = focRTF ) then
        ActiveNote.Editor.SetFocus
      else
      if ( ActiveNote.FocusMemory = focTree ) then
        TTreeNote( ActiveNote ).TV.SetFocus;
    except
    end;
  end
  else
  if ( key = #27 ) then
  begin
    key := #0;
    FocusActiveNote;
  end;
end; // Combo_FontSizeKeyPress


procedure TForm_Main.MMFormatBoldClick(Sender: TObject);
var
  BoldWasDown : boolean;
begin
  if ( self.ActiveControl is TTreeNT ) then
  begin
    BoldWasDown := ( not TB_Bold.Down ); // button .Down state has ALREADY changed
    try
      SetTreeNodeBold( ShiftDown )
    finally
      TB_Bold.Down := BoldWasDown;
    end;
  end
  else
  begin
    PerformCmd( ecBold );
  end;
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
  if ShiftDown then
    PerformCmd( ecRightIndent )
  else
    PerformCmd( ecFirstIndent );
end;

procedure TForm_Main.MMFormatFindDecClick(Sender: TObject);
begin
  if ShiftDown then
    PerformCmd( ecRightOutdent )
  else
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
  if KeyOptions.UseOldColorDlg then
    PerformCmd( ecFontColorDlg ) // bring up color dialog
  else
    PerformCmd( ecFontColor ); // apply last color
end;

procedure TForm_Main.MMFormatHighlightClick(Sender: TObject);
begin
  if KeyOptions.UseOldColorDlg then
    PerformCmd( ecHighlightDlg )
  else
    PerformCmd( ecHighlight );
end;

procedure TForm_Main.MMViewFilterTreeClick(Sender: TObject);
begin
    TB_FilterTreeClick(nil);
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
    UpdateCursorPos;
  end;
end;


procedure TForm_Main.MMFormatNoHighlightClick(Sender: TObject);
begin
  PerformCmd( ecNoHighlight );
end;

procedure TForm_Main.TB_ColorClick(Sender: TObject);
begin
  TB_Color.OnClick := nil;
  try
    if ( self.ActiveControl is TTreeNT ) then
      SetTreeNodeColor( false, true, false, ShiftDown  )
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
    if ( self.ActiveControl is TTreeNT ) then
      SetTreeNodeColor( false, false, false, ShiftDown )
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

procedure TForm_Main.MRUMRUItemClick(Sender: TObject; AFilename: String);
begin
  NoteFileOpen( AFilename );
  FocusActiveNote;
end; // MRUMRUItemClick

procedure TForm_Main.DebugMenuClick(Sender: TObject);
var
  s : string;
  // i : integer;
begin
  {$IFDEF MJ_DEBUG}
  if assigned( Log ) then Log.Flush( true );
  {$ENDIF}
  { [debug] }

  (*
  if ( ShiftDown and ( pages.PageCount > 0 )) then
  begin

    s := '';
    for i := 0 to pred( pages.ActivePage.ControlCount ) do
      s := s + 'Controls[' + inttostr( i ) + '] ClassName: "' +
                pages.ActivePage.Controls[i].ClassName +
                '"  Name: ' + pages.ActivePage.Controls[i].Name + #13;

    s := 'pages.ControlCount: ' + inttostr( pages.ControlCount ) + #13 +
    'ActivePage.ControlCount: ' + inttostr( pages.ActivePage.ControlCount ) + #13#13 +
    s;

    showmessage( s );

    exit;
  end;
  *)

  {
  with ActiveNote.Editor.Font do
    s := Format( 'Name: %s  Size: %d', [Name, Size] ) + #13;
  with ActiveNote.EditorChrome.Font do
    s := s + Format( 'Name: %s  Size: %d', [Name, Size] );
  if ( messagedlg( s, mtInformation, [mbOK,mbCancel], 0 ) = mrCancel ) then exit;
  }


  s := format( 'Win32Platform: %d', [Win32Platform] ) + #13 +
       format( 'Win32MajorVersion: %d ' + #13 +
               'Win32MinorVersion: %d',
                [Win32MajorVersion,Win32MinorVersion] ) + #13 +
       // format( 'RichEdit version from RxLib: %d', [RichEditVersion] ) + #13 +
       format( 'RichEdit version: %d', [LoadedRichEditVersion] ) + #13 +
       format( 'AllocMemSize: %d', [AllocMemSize] );
  if ( messagedlg( s, mtInformation, [mbOK,mbCancel], 0 ) = mrCancel ) then exit;

  if HaveNotes( false, false ) then
  begin
    s := 'Filename: ' + extractfilename( NoteFile.FileName ) +#13+
         'Notes modified: ' + BOOLARRAY[NoteFile.Modified] +#13+
         'Notes count: ' + inttostr( NoteFile.NoteCount ) +#13+
         'File Read-Only: ' + BOOLARRAY[NoteFile.ReadOnly] +#13+
         'File Busy: ' + BOOLARRAY[FileIsBusy] +#13+
         'File format: ' + FILE_FORMAT_NAMES[NoteFile.FileFormat] + #13#13+
         'FolderMon active: ' + BOOLARRAY[FolderMon.Active] +#13+
         'FolderMon folder: ' + FolderMon.FolderName + #13#13 +
         'IS_OLD_FILE_FMT: ' + BOOLARRAY[_IS_OLD_KEYNOTE_FILE_FORMAT] +#13+
         'USE_OLD_FILE_FMT: ' + BOOLARRAY[_USE_OLD_KEYNOTE_FILE_FORMAT] +#13#13;

    if NoteFile.FileFormat = nffEncrypted then
    begin
      s := s + 'Encrypted with: ' + CRYPT_METHOD_NAMES[NoteFile.CryptMethod] + #13 +
               'Pass: ' + NoteFile.PassPhrase +#13#13;
    end;

    if assigned( ActiveNote ) then
    begin
      s := s +
         'Active note name: ' + ActiveNote.Name +#13+
         'Active note kind: ' + TABNOTE_KIND_NAMES[ActiveNote.Kind] +#13;

      if ( ActiveNote.Kind = ntTree ) then
      begin
        s := s + 'Number of Tree nodes: ' + inttostr( TTreeNote( ActiveNote ).TV.Items.Count ) + #13 +
          'Number of Note nodes: ' + inttostr( TTreeNote( ActiveNote ).NodeCount ) + #13;
        if assigned( TTreeNote( ActiveNote ).SelectedNode ) then
          s := s + 'Selected node: ' + TTreeNote( ActiveNote ).SelectedNode.Name +#13;
      end;
    end
    else
    begin
      s := s + 'ActiveNote NOT assigned' + #13;
    end;

    if ( _OTHER_INSTANCE_HANDLE <> 0 ) then
      s := s + 'Found other instance!' + #13;

  end
  else
  begin
    s := 'No notes.' + #13;
  end;


  if assigned( FileManager ) then
  begin
    s := s + #13 + 'File Manager count: ' + inttostr( FileManager.Count );
  end
  else
  begin
    s := s + 'File Manager NOT assigned.';
  end;

  {$IFDEF MJ_DEBUG}
  s := s + #13 + '(Log enabled)';
  {$ENDIF}

  if messagedlg( s, mtInformation, [mbOK,mbCancel], 0 ) = mrCancel then exit;

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
  with KeyOptions do
  begin
    ToolbarTreeShow := ( not ToolbarTreeShow );
    Toolbar_Tree.Visible := ( ToolbarTreeShow and assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree ));
    MMViewTBTree.Checked := ToolbarTreeShow;
  end;
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
      messagedlg( Format( 'Custom date formats reloaded (%d)', [DATE_FORMAT_LIST.Count] ), mtInformation, [mbOK], 0 )
    else
      messagedlg( Format( 'Cannot load custom date formats from %s. Check if the file exists.', [_DateFormatsFile] ), mtError, [mbOK], 0 );
  end
  else
    PerformCmd( ecInsDate );
end;

procedure TForm_Main.MMInsertTimeClick(Sender: TObject);
begin
  if ShiftDown and ( sender is TTOolbarButton97 ) then
  begin
    if LoadTimeFormatsList then
      messagedlg( Format( 'Custom time formats reloaded (%d)', [TIME_FORMAT_LIST.Count] ), mtInformation, [mbOK], 0 )
    else
      messagedlg( Format( 'Cannot load custom time formats from %s. Check if the file exists.', [_TimeFormatsFile] ), mtError, [mbOK], 0 );
  end
  else
    PerformCmd( ecInsTime );
end;

procedure TForm_Main.MMNoteRemoveClick(Sender: TObject);
begin
  DeleteNote;
end;

procedure TForm_Main.MMFileCopyToClick(Sender: TObject);
begin
  NoteFileCopy;
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
  EditNoteProperties( propDefaults );
end;

procedure TForm_Main.MMNotePropertiesClick(Sender: TObject);
begin
  // EditNote;
  EditNoteProperties( propThisNote );
end;

procedure TForm_Main.TB_ExitClick(Sender: TObject);
begin
  Close; // No "TerminateClick"!
end;

procedure TForm_Main.TB_FilterTreeClick(Sender: TObject);
var
   note: TTreeNote;
begin
    if not assigned (ActiveNote) then exit;

    note:= TTreeNote(ActiveNote);
    if not note.Filtered then begin
        MMViewResPanelClick( nil );
        Pages_Res.ActivePage:= ResTab_Find;
        CB_ResFind_Filter.Checked:= true;
        TB_FilterTree.Down:= false;
    end
    else begin
        note:= TTreeNote(ActiveNote);
        RemoveFilter (note);
        FilterRemoved (note);
    end;
end;

procedure TForm_Main.FilterApplied (note: TTreeNote);   // [dpv]
begin
    note.Filtered:= true;
    if note = ActiveNote then begin
      TB_FilterTree.Down:= true;
      MMViewFilterTree.Checked:= true;
      TB_FilterTree.Hint:= 'Remove Filter on tree note';
    end;
end;
procedure TForm_Main.FilterRemoved (note: TTreeNote);   // [dpv]
begin
    note.Filtered:= false;
    if note = ActiveNote then begin
      TB_FilterTree.Down:= false;
      MMViewFilterTree.Checked:= false;
      TB_FilterTree.Hint:= 'Apply Filter on tree note';
    end;
end;

procedure TForm_Main.MMSortClick(Sender: TObject);
begin
  PerformCmd( ecSort );
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
  RunFinder;
end;

procedure TForm_Main.MMFindNextClick(Sender: TObject);
begin
  if ( FindOptions.Pattern = '' ) then
    RunFinder
  else
    RunFindNext;
end; // MMFindagainClick

procedure TForm_Main.MMFindReplaceClick(Sender: TObject);
begin
  RunReplace;
end; // MMReplaceClick

procedure TForm_Main.MMFindReplaceNextClick(Sender: TObject);
begin
  if ( FindOptions.ReplacePattern = '' ) then
    RunReplace
  else
    RunReplaceNext;
end; // MMReplaceNextClick


function TForm_Main.HaveNotes( const Warn, CheckCount : boolean ) : boolean;
var
  msg : string;
begin
  result := true;
  msg := '';
  if ( not assigned( NoteFile )) then
  begin
    result := false;
    if Warn then
      msg := 'This operation cannot be performed because no file is open.';
  end
  else
  begin
    if ( CheckCount and (( NoteFile.Notes.Count < 1 ) or ( Pages.PageCount < 1 ))) then
    begin
      result := false;
      if Warn then
        msg := 'This operation cannot be performed because the currently open file has no notes.';
    end;
  end;

  if (( not result ) and Warn ) then
    PopupMessage( msg, mtInformation, [mbOK], 0 );

end; // HaveNotes

function TForm_Main.NoteIsReadOnly( const aNote : TTabNote; const Warn : boolean ) : boolean;
begin
  result := assigned( ANote ) and ANote.ReadOnly;
  if ( result and Warn ) then
    StatusBar.Panels[PANEL_HINT].Text := ' Cannot perform operation: Note is Read-Only';
end; // NoteIsReadOnly


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

procedure TForm_Main.ShiftTab( const ShiftRight : boolean );
var
  i, idx : integer;
begin
  if ( Pages.PageCount < 2 ) then exit;
  idx := Pages.ActivePage.ImageIndex;
  i := Pages.ActivePage.PageIndex;
  case ShiftRight of
    true : if ( i >= pred( Pages.PageCount )) then
      i := 0
    else
      inc( i );
    false : if ( i < 1 ) then
      i := pred( Pages.PageCount )
    else
      dec( i );
  end;

  Pages.ActivePage.PageIndex := i;
  Pages.ActivePage.ImageIndex := idx;
  NoteFile.Modified := true;
  UpdateNoteFileState( [fscModified] );

end; // ShiftTab

procedure TForm_Main.MMViewShiftTabLeftClick(Sender: TObject);
begin
  ShiftTab( false );
end;

procedure TForm_Main.MMViewShiftTabRightClick(Sender: TObject);
begin
  ShiftTab( true );
end;


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

procedure TForm_Main.MMNotePrintClick(Sender: TObject);
begin
  PrintRTFNote;
end;

procedure TForm_Main.WMChangeCBChain(var Msg: TWMChangeCBChain);
begin
  if Msg.Remove = ClipCapNextInChain then
    ClipCapNextInChain := Msg.Next
  else
    SendMessage( ClipCapNextInChain, WM_CHANGECBCHAIN, Msg.Remove, Msg.Next );
  Msg.Result := 0;
end; // WMChangeCBChain

procedure TForm_Main.WMDrawClipboard(var Msg: TWMDrawClipboard);
var
  ClpStr : string;
  thisClipCRC32 : DWORD;

begin
  SendMessage( ClipCapNextInChain, WM_DRAWCLIPBOARD, 0, 0 );
  Msg.Result := 0;

  if ( _IS_CAPTURING_CLIPBOARD or _IS_CHAINING_CLIPBOARD ) then exit;

  if ( ClipCapActive and assigned( NoteFile ) and ( NoteFile.ClipCapNote <> nil )) then
  begin
    if (( GetActiveWindow <> self.Handle ) or // only when inactive
       (( not ClipOptions.IgnoreSelf ) and // explicitly configured to capture from self...
       ( not ( NoteFile.ClipCapNote = ActiveNote )))) then // but never capture text copied from the note that is being used for capture
       begin
          // test for duplicates
          ClpStr := ClipboardAsString;
          if ( ClipOptions.TestDupClips and ( ClpStr <> '' )) then
          begin
            try
              CalcCRC32( addr( ClpStr[1] ), length( ClpStr ), thisClipCRC32 );
            except
              on E : Exception do
              begin
                messagedlg( 'CRC calculation error in clipboard capture, testing for duplicate clips will be turned off. Message: ' + E.Message, mtError, [mbOK], 0 );
                ClipOptions.TestDupClips := false;
                exit;
              end;
            end;
            if ( thisClipCRC32 = ClipCapCRC32 ) then
              exit; // ignore duplicate clips
            ClipCapCRC32 := thisClipCRC32; // set value for next test
          end;

          // ok to paste
          PasteOnClipCap;
       end;
  end;
end; // WMDrawClipboard

procedure TForm_Main.MMNoteClipCaptureClick(Sender: TObject);
begin
  TB_ClipCap.Down := ( not TB_ClipCap.Down );
  ToggleClipCap( TB_ClipCap.Down, ActiveNote );
end;

procedure TForm_Main.MMFilePageSetupClick(Sender: TObject);
begin
  if ( not assigned( RichPrinter )) then    // [dpv]
  begin
      try                                     // [DPV]
         RichPrinter := TRichPrinter.Create(Form_Main);
      except
        On E : Exception do
        begin
          showmessage( E.Message );
          exit;
        end;
      end;
  end;

   try                                     // [DPV]
      RichPrinter.PageDialog;
    // PageSetupDlg.Execute;
   except
      On E : Exception do
      begin
        showmessage( E.Message );
      end;
   end;

end;

procedure TForm_Main.RichPrinterBeginDoc(Sender: TObject);
begin
  StatusBar.Panels[PANEL_HINT].Text := ' Printing note...';
end;

procedure TForm_Main.RichPrinterEndDoc(Sender: TObject);
begin
  StatusBar.Panels[PANEL_HINT].Text := ' Finished printing note.';
end;

procedure TForm_Main.MMNotePrintPreview_Click(Sender: TObject);
{
var
  Form_Preview: TPreviewForm;
}
begin
  if ( not HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;

  if ( not assigned( RichPrinter )) then    // [dpv]
  begin
      try                                     // [DPV]
         Form_Main.RichPrinter := TRichPrinter.Create(Form_Main);
      except
        On E : Exception do
        begin
          showmessage( E.Message );
          exit;
        end;
      end;
  end;

  RichPrinter.PrintRichEditPreview( TCustomRichEdit( ActiveNote.Editor ));

  // [x] does not work
  // RichPrinter.PrintRichEditPreview( TCustomRichEdit( ActiveNote.RTF ));

  // [x] does not work, either
  {
  Form_Preview := TForm_Preview.Create( self );
  try
    Form_Preview.myRTF := ActiveNote.Editor;
    Form_Preview.ShowModal;
  finally
    Form_Preview.Free;
  end;
  }
end; // MMPrintpreviewClick

procedure TForm_Main.MMEditCopyAllClick(Sender: TObject);
begin
  PerformCmd( ecSelectAll );
  PerformCmdEx( ecCopy );
end;

procedure TForm_Main.MMEditPasteAsNewNoteClick(Sender: TObject);
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
  if ( not HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;

  StatusBar.Panels[PANEL_HINT].Text := ' Preparing to send note via email...';
  Form_Mail := TForm_Mail.Create( self );
  try
    with Form_Mail do
    begin
      ShowHint := KeyOptions.ShowTooltips;
      myActiveNote := ActiveNote;
      myNotes := NoteFile;
      myINI_FN := MailINI_FN;
    end;
    case Form_Mail.ShowModal of
      mrOK : StatusBar.Panels[PANEL_HINT].Text := ' Note sent';
      else
        StatusBar.Panels[PANEL_HINT].Text := ' Note not sent';
    end;
  finally
    Application.OnException := ShowException;
    Form_Mail.Free;
  end;
{$ENDIF}
end; // MMEmailnoteClick

procedure TForm_Main.TB_AlarmNodeClick(Sender: TObject);      // [dpv*]
var
    myNode: TNoteNode;
    node: TTreeNTNode;
begin
    if assigned(ActiveNote) and (ActiveNote.Kind = ntTree) and (assigned(TTreeNote(ActiveNote).TV.Selected)) then begin
       node:= TTreeNote(ActiveNote).TV.Selected;
       myNode:= TNoteNode( node.Data );
       TB_AlarmNode.Down:= (myNode.Alarm <> 0);
       AlarmManager.EditAlarm (node);
    end
    else
       TB_AlarmNode.Down:= false;

    TVAlarmNode.Checked:= TB_AlarmNode.Down;
end;

procedure TForm_Main.TB_AlarmNodeMouseEnter(Sender: TObject);    // [dpv*]
var
    node: TNoteNode;
begin
    if assigned(ActiveNote) and (ActiveNote.Kind = ntTree) and (assigned(TTreeNote(ActiveNote).TV.Selected)) then begin
       node:= TNoteNode( TTreeNote(ActiveNote).TV.Selected.Data );
       if (node.Alarm <> 0) then
           TB_AlarmNode.Hint:= 'Alarm set at: ' + FormatDateTime( 'dddd, d MMMM yyyy ' + #32 + 'HH:mm', node.Alarm )
       else
           TB_AlarmNode.Hint:= 'Set alarm...';
    end
end;

procedure TForm_Main.TB_ClipCapClick(Sender: TObject);
begin
  ToggleClipCap( TB_ClipCap.Down, ActiveNote );
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


procedure TForm_Main.TVChange(Sender: TObject; Node: TTreeNTNode);
begin
  if ( Node <> _LAST_NODE_SELECTED ) then
  begin
    try
      TreeNodeSelected( Node );
    finally
      _LAST_NODE_SELECTED := Node;
    end;
  end;
end; // TVChange

procedure TForm_Main.OnFileDropped( Sender : TObject; FileList : TStringList );
begin
      FileDropped( Sender, FileList );
end;


procedure TForm_Main.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName : array[0..MAX_PATH] of Char;
  FileList : TStringList;
  i, count : integer;
begin
  FileList := TStringList.Create;

  try
    count := DragQueryFile( Msg.Drop, $FFFFFFFF, CFileName, MAX_PATH );

    if ( count > 0 ) then
    begin
      for i := 0 to count-1 do
      begin
        DragQueryFile( Msg.Drop, i, CFileName, MAX_PATH );
        FileList.Add( CFileName );
      end;
    end;

    FileDropped( self, FileList );

  finally
    FileList.Free;
    DragFinish(Msg.Drop);
  end;

end; // WMDropFiles


procedure TForm_Main.CreateWnd;
begin
  inherited;
  DragAcceptFiles( handle, true );
end; // CreateWnd

procedure TForm_Main.DestroyWnd;
begin
  DragAcceptFiles( handle, false );
  inherited;
end; // DestroyWnd

(*
procedure TForm_Main.TVBeforeItemPaint(Sender: TObject;
      Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates;
      var OwnerDraw: Boolean);
begin
  Node.ParentFont := true;
end; // TVBeforeItemPaint
*)

procedure TForm_Main.TVDeletion(Sender: TObject; Node: TTreeNTNode);
var
   myTNote : TTreeNote;
begin
  if not assigned( Node ) then exit;

  myTNote:= TTreeNote(NoteFile.GetNoteByTreeNode(Node));
  if assigned( myTNote ) then begin
    if TNoteNode( Node.Data ).Alarm <> 0 then                     // [dpv*]
       AlarmManager.RemoveAlarmNode(Node);

    myTNote.RemoveNode( TNoteNode( Node.Data ));
  end;
end;

procedure TForm_Main.TVClick(Sender: TObject);
begin
 // ( sender as TTreeNT ).PopupMenu := Menu_TV;
 if assigned( ActiveNote ) then ActiveNote.FocusMemory := focTree;
end;

procedure TForm_Main.TVDblClick(Sender: TObject);
begin
  // MMRenamenodeClick( Sender );
end;

procedure TForm_Main.TVMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  (*
  if ( Button <> mbLeft ) then exit;
  DraggedTreeNode := ( sender as TTreeNT ).GetNodeAt( X, Y );
  if assigned( DraggedTreeNode ) then
  begin
  //  ( sender as TTreeNT ).BeginDrag( false );
  end;
  *)
end; // TVMouseDown


procedure TForm_Main.TVStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  // StatusBar.Panels[PANEL_HINT].Text := 'Drag: ' + DragObject.GetName;
  DraggedTreeNode := TTreeNote( ActiveNote ).TV.Selected;
  if assigned( DraggedTreeNode ) then
  begin
     // {N}
    StatusBar.Panels[PANEL_HINT].Text := 'Drag: ' + DraggedTreeNode.Text;
  end;
end; // TVStartDrag


procedure TForm_Main.TVDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  ThisNode : TTreeNTNode;
begin

  StatusBar.Panels[0].Text := 'TV dragover';

  if (( source is TRxRichEdit )) then
  begin
    accept := true;

  end;

  accept := false;
  if (( not assigned( DraggedTreeNode )) or ( sender <> source )) then
    exit;

  ThisNode := ( sender as TTreeNT ).GetNodeAt( X, Y );
  if ( not ( assigned( ThisNode ) and ( ThisNode <> DraggedTreeNode ))) then
    exit;

  Accept := true;

end; // TVDragOver

function IsAParentOf( aPerhapsParent, aChild : TTreeNTNode ) : boolean;
var
  i, leveldifference : integer;
begin
  result := false;
  if ( not ( assigned( aPerhapsParent ) and assigned( aChild ))) then exit;

  leveldifference := aChild.Level - aPerhapsParent.Level;
  if ( leveldifference < 1 ) then exit; // cannot be a parent if its level is same or greater than "child's"

  for i := 1 to leveldifference do
  begin
    aChild := aChild.Parent;
    if assigned( aChild ) then
    begin
      if ( aChild = aPerhapsParent ) then
      begin
        result := true;
        break;
      end;
    end
    else
    begin
      break; // result = false
    end;
  end;
end; // IsAParentOf

procedure TForm_Main.TVDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  DropTreeNode, PreviousParent : TTreeNTNode;
  s : string;
  myTNote : TTreeNote;
begin
  if NoteIsReadOnly( ActiveNote, true ) then
  begin
    DraggedTreeNode := nil;
    if ( Sender is TTreeNT ) then
      ( sender as TTreeNT ).EndDrag( false );
    exit;
  end;

  s := '<nothing>';
  DropTreeNode := ( sender as TTreeNT ).GetNodeAt( X, Y );

  myTNote := TTreeNote( ActiveNote );

  try

    if assigned( DraggedTreeNode ) and assigned( DropTreeNode ) then
    begin

      // check if we can drop DraggedTreeNode onto DropNode
      // 1. Cannot drop node on itself
      if ( DropTreeNode = DraggedTreeNode ) then
      begin
        // {N}
        s := Format( 'Cannot drop node %s on itself', [DraggedTreeNode.Text] );
        exit;
      end;
      // 2. Cannot drop between treeviews
      if ( DropTreeNode.TreeView <> DraggedTreeNode.TreeView ) then
      begin
        // {N}
        s := Format( 'Cannot drop node %s - invalid source', [DraggedTreeNode.Text] );
        exit;
      end;
      // 3. Cannot drop a node onto its own child
      if IsAParentOf( DraggedTreeNode, DropTreeNode ) then
      begin
        // {N}
        s := Format( 'Cannot drop node %s onto its child %s', [DraggedTreeNode.Text, DropTreeNode.Text] );
        exit;
      end;

      // now figure out where to move the node
      // 1. If dropping on immediate parent, then
      //    make node first child of parent
      // 2. If dropping on any other node, then
      //    make node LAST child of that node,
      //    unless SHIFT is down, in which case
      //    make node FIRST child of that node

      myTNote.TV.OnChange := nil; // stop event
      PreviousParent := DraggedTreeNode.Parent;

      if (( DropTreeNode.Level = 0 ) and CtrlDown ) then
      begin
        // make TOP node
        DraggedTreeNode.MoveTo( nil, naAddFirst );
        // {N}
        s := Format( 'Node "%s" promoted to TOP', [DraggedTreeNode.Text] );
      end
      else
      if ( DraggedTreeNode.Parent = DropTreeNode ) then
      begin
        if ShiftDown then
        begin
          DraggedTreeNode.MoveTo( DropTreeNode, naInsert );
          // {N}
          s := Format( 'Node "%s" promoted to parent''s level', [DraggedTreeNode.Text] );
        end
        else
        begin
          DraggedTreeNode.MoveTo( DropTreeNode, naAddChildFirst );
          // {N}
          s := Format( 'Node "%s" moved to top of siblings', [DraggedTreeNode.Text] );
        end;
      end
      else
      begin
        if ShiftDown then
        begin
          DraggedTreeNode.MoveTo( DropTreeNode, naInsert );
          // {N}
          s := Format( 'Node "%s" inserted after node "%s"', [DraggedTreeNode.Text, DropTreeNode.Text] );
        end
        else
        begin
          DraggedTreeNode.MoveTo( DropTreeNode, naAddChildFirst );
          // {N}
          s := Format( 'Node "%s" made child of node "%s"', [DraggedTreeNode.Text, DropTreeNode.Text] );
        end;
      end;

      // update node icon
      SelectIconForNode( DraggedTreeNode, myTNote.IconKind );
      SelectIconForNode( DraggedTreeNode.Parent, myTNote.IconKind );
      SelectIconForNode( PreviousParent, myTNote.IconKind );
      myTNote.TV.Invalidate;

    end
    else
    begin
      s := 'Nothing to drop or invalid drop target';
      DraggedTreeNode := nil;
      if ( Sender is TTreeNT ) then
        ( sender as TTreeNT ).EndDrag( false );
    end;

  finally
    NoteFile.Modified := true;
    myTNote.TV.OnChange := TVChange;
    UpdateNoteFileState( [fscModified] );
    StatusBar.Panels[PANEL_HINT].Text := s;
  end;

end; // TVDragDrop

procedure TForm_Main.TVEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  // StatusBar.Panels[PANEL_HINT].Text := 'End drag';
  DraggedTreeNode := nil;
end;

procedure TForm_Main.TVVirtualNodeClick(Sender: TObject);
begin
  VirtualNodeProc( vmNone, nil, '' );
end; // TVMVirtualNodeClick

procedure TForm_Main.TVRefreshVirtualNodeClick(Sender: TObject);
begin
  VirtualNodeRefresh( TreeOptions.ConfirmNodeRefresh );
end;


procedure TForm_Main.TVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ptCursor : TPoint;
begin
  if ( not ( assigned( activenote ) and ( activenote.kind = ntTree ))) then
    exit;
  if TTreeNote( ActiveNote ).TV.IsEditing then exit;

  ActiveNote.FocusMemory := focTree;

  case key of
    VK_F10 : if ( shift = [ssShift] ) then
    begin
      key := 0;
      GetCursorPos( ptCursor );
      Menu_TV.Popup( ptCursor.X, ptCursor.Y );
    end;

    VK_SPACE : if ( Shift = [] ) then
    begin
      // rename node
      key := 0;
      MMRenamenodeClick( sender );
    end;

    220 : if ( Shift = [ssCtrl] ) then begin // backslash
      Key := 0;
      ActiveNote.Editor.SetFocus;
    end;

  end;

end; // TVKeyDown


procedure TForm_Main.TVKeyPress(Sender: TObject; var Key: Char);
begin
  if ( sender as TTreeNT ).IsEditing then
  begin
    if ( key = KNTLINK_SEPARATOR ) then
      key := #0; // disallow | character in node name, because it breaks KNT links (which use | as delimiter)
  end;
end; // TVKeyPress

procedure TForm_Main.TVAddNodeClick(Sender: TObject);
begin
  AddNodeToTree( tnAddLast );
end; // TVMAddNodeClick

procedure TForm_Main.TVInsertNodeClick(Sender: TObject);
begin
  AddNodeToTree( tnInsertBefore );
end; // TVMInsertNodeClick

procedure TForm_Main.TVAddChildNodeClick(Sender: TObject);
begin
  AddNodeToTree( tnAddChild );
end;

procedure TForm_Main.TVAddSiblingClick(Sender: TObject);
begin
  AddNodeToTree( tnAddAfter );
end;

procedure TForm_Main.TVAlarmNodeClick(Sender: TObject);
begin
   TB_AlarmNodeClick (nil);
end;

procedure TForm_Main.TVDeleteNodeClick(Sender: TObject);
begin
  DeleteTreeNode( true );
end;

procedure TForm_Main.TVDeleteChildrenClick(Sender: TObject);
begin
  DeleteTreeNode( false );
end;

procedure TForm_Main.TVMoveNodeUpClick(Sender: TObject);
begin
  MoveTreeNode( nil, dirUp );
end;

procedure TForm_Main.TVMoveNodeDownClick(Sender: TObject);
begin
  MoveTreeNode( nil, dirDown );
end;

procedure TForm_Main.TVMoveNodeLeftClick(Sender: TObject);
begin
  MoveTreeNode( nil, dirLeft );
end;

procedure TForm_Main.TVMoveNodeRightClick(Sender: TObject);
begin
  MoveTreeNode( nil, dirRight );
end;

procedure TForm_Main.TVPasteNodeNameClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
    PasteNodeName( TPasteNodeNameMode(( sender as TMenuItem ).Tag ));
end;



procedure TForm_Main.TVCopyNodeNameClick(Sender: TObject);
begin
  CopyNodeName( ShiftDown );
end;

procedure TForm_Main.MMTreeFullExpandClick(Sender: TObject);
begin
  TTreeNote( ActiveNote ).TV.FullExpand;
  TreeNodeSelected( TTreeNote( ActiveNote ).TV.Selected );
  TTreeNote( ActiveNote ).TV.Selected.MakeVisible;
end;

procedure TForm_Main.MMTreeFullCollapseClick(Sender: TObject);
begin
  TTreeNote( ActiveNote ).TV.FullCollapse;
  TreeNodeSelected( TTreeNote( ActiveNote ).TV.Selected );
end;

procedure TForm_Main.TVSortSubtreeClick(Sender: TObject);
var
  myTreeNode : TTreeNTNode;
begin
  if ( ActiveNote.FocusMemory <> focTree ) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;
  myTreeNode := GetCurrentTreeNode;
  if ( assigned( myTreeNode ) and myTreeNode.HasChildren ) then
  begin
    TTreeNote( ActiveNote ).TV.OnChange := nil;
    try
      myTreeNode.AlphaSort;
    finally
      NoteFile.Modified := true;
      TTreeNote( ActiveNote ).TV.OnChange := TVChange;
      UpdateNoteFileState( [fscModified] );
    end;
  end;
end; // Sort Child Nodes


procedure TForm_Main.TVSortTreeClick(Sender: TObject);
begin
  if NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( assigned( ActiveNote ) and ( activenote.Kind = ntTree )) then
  begin

    if ( messagedlg(
      'OK to sort the entire tree?',
      mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

    TTreeNote( ActiveNote ).TV.OnChange := nil;
    try
      TTreeNote( ActiveNote ).TV.AlphaSort;
    finally
      NoteFile.Modified := true;
      TTreeNote( ActiveNote ).TV.OnChange := TVChange;
      UpdateNoteFileState( [fscModified] );
    end;
  end;
end; // Sort full tree



procedure TForm_Main.TVEdited(Sender: TObject; Node: TTreeNTNode;
  var S: String);
begin
  ( sender as TTreeNT ).PopupMenu := Menu_TV;
  S := trim( copy( S, 1, TREENODE_NAME_LENGTH ));
  if ( S = '' ) then
  begin
    StatusBar.Panels[PANEL_HINT].Text := ' Node name cannot be blank!';
    S := _OLD_NODE_NAME;
    exit;
  end;
  _ALLOW_VCL_UPDATES := false;
  try
    if assigned( TNoteNode( Node.Data )) then
      TNoteNode( Node.Data ).Name := S;  // {N} must add outline numbering, if any
    StatusBar.Panels[PANEL_HINT].Text := ' Node renamed.';
    ActiveNote.Modified := true;
  finally
    _ALLOW_VCL_UPDATES := true;
    UpdateNoteFileState( [fscModified] );
  end;
end; // TVEdited

procedure TForm_Main.TVEditCanceled(Sender: TObject);
begin
  ( sender as TTreeNT ).PopupMenu := Menu_TV;
end; // TVEditCanceled

procedure TForm_Main.TVEditing(Sender: TObject; Node: TTreeNTNode;
  var AllowEdit: Boolean);
begin
  if (( assigned( ActiveNote )) and ( not ActiveNote.ReadOnly )) then
  begin
    // {N}
    _OLD_NODE_NAME := node.text;
    // stop menu events triggered by shortcut keys:
    ( sender as TTreeNT ).PopupMenu := nil;
  end
  else
  begin
    StatusBar.Panels[PANEL_HINT].Text := ' Cannot edit: Note is read-only.';
    AllowEdit := false;
  end;
end; // TVEditing

procedure TForm_Main.MMRenamenodeClick(Sender: TObject);
var
  myNode : TNoteNode;
  myName : string;
begin
  if NoteIsReadOnly( ActiveNote, true ) then exit;

  myNode := GetCurrentNoteNode;
  if assigned( myNode ) then
  begin
    if ( ActiveNote.FocusMemory <> focTree ) then exit;
    if TreeOptions.EditInPlace then
    begin
      TTreeNote( ActiveNote ).TV.Selected.EditText;
    end
    else
    begin
      myName := myNode.Name;
      _OLD_NODE_NAME := myName;
      if InputQuery( 'Edit node name', 'Enter new name:', myName ) then
      begin
        myName := trim( myName );
        if ( myName <> '' ) then
        begin
          myNode.Name := myName;
          // {N}
          TTreeNote( ActiveNote ).TV.Selected.Text := myNode.Name; // TNoteNode does NOT update its treenode's properties!
          ActiveNote.Modified := true;
          UpdateNoteFileState( [fscModified] );
        end
        else
        begin
          messagedlg( 'Node name cannot be blank!', mtError, [mbOK], 0 );
        end;
      end;
    end;
  end;
end; // RenameNode


procedure TForm_Main.MMViewNodeIconsClick(Sender: TObject);
var
  tNote : TTreeNote;
  mi : TMenuItem;
begin
  if ( not ( sender is TMenuItem )) then exit;
  mi := ( sender as TMenuItem );
  if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
  begin
    tNote := TTreeNote( ActiveNote );
    if mi.Checked then
    begin
      tNote.IconKind := niNone;
    end
    else
    begin
      tNote.IconKind := TNodeIconKind( mi.Tag );
    end;
    ShowOrHideIcons( tNote, ( tNote.IconKind <> niNone ));
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;
end; // MMViewNodeIconsClick



procedure TForm_Main.MMViewCheckboxesAllNodesClick(Sender: TObject);
var
  tNote : TTreeNote;
begin
  // show or hide checkboxes in active note's tree panel
  if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
  begin
    tNote := TTreeNote( ActiveNote );
    tNote.Checkboxes := ( not tNote.Checkboxes );
    MMViewCheckboxesAllNodes.Checked := tNote.Checkboxes;
    TVChildrenCheckbox.Enabled := not MMViewCheckboxesAllNodes.Checked;       // [dpv]
    ShowOrHideCheckBoxes( tNote );
  end;
end;



procedure TForm_Main.TVChecked(Sender: TObject; Node: TTreeNTNode);
var
  myNode : TNoteNode;
  oldOnChecked : TTVCheckedEvent;

    procedure CheckChildren( StartNode : TTreeNTNode );
    var
      childNode : TTreeNTNode;
    begin
      childNode := StartNode.GetFirstChild;
      while ( assigned( childNode ) and assigned( childNode.Data )) do
      begin
        childNode.CheckState := node.CheckState;
        TNoteNode( childNode.Data ).Checked := ( node.CheckState = csChecked );
        if childNode.HasChildren then
          CheckChildren( childNode ); // RECURSIVE CALL
        childNode := StartNode.GetNextChild( childNode );
      end;
    end;

begin
  if ( assigned( node ) and assigned( node.Data )) then
  begin
    with ( sender as TTreeNT ) do
    begin
      oldOnChecked := OnChecked;
      OnChecked := nil;
    end;
    try
      myNode := TNoteNode( node.Data );
      myNode.Checked := ( node.CheckState = csChecked );

      if ( shiftdown and node.HasChildren
           and not IsAnyNodeMoving) then     // [dpv]
      begin
        CheckChildren( node );
      end;

      if TB_HideChecked.Down and not IsAnyNodeMoving then                    // [dpv]
          if node.CheckState  = csChecked then
             node.Hidden := True;

    finally
      NoteFile.Modified := true;
      UpdateNoteFileState( [fscModified] );
      ( sender as TTreeNT ).OnChecked := oldOnChecked;
    end;
  end;
end; // TVChecked

procedure TForm_Main.TVChecking(Sender: TObject; Node: TTreeNTNode;
  var AllowCheck: Boolean);
begin
  AllowCheck := ( not NoteIsReadOnly( ActiveNote, false ));
end;

procedure TForm_Main.MMToolsMergeClick(Sender: TObject);
begin
  MergeFromKNTFile( '' );
end;

procedure TForm_Main.MMMergeNotestoFileClick(Sender: TObject);
begin
  // MergeToKNTFile;
end;

procedure TForm_Main.RTFMWordWebClick(Sender: TObject);
begin
  WordWebLookup;
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
    1 : Msg := 'Parser stack overflow';
    2 : Msg := 'Bad cell range';
    3 : Msg := 'Expected expression';
    4 : Msg := 'Expected operator';
    5 : Msg := 'Expected opening parenthesis';
    6 : Msg := 'Expected operator or closing parenthesis';
    7 : Msg := 'Invalid numeric expression';
  end; { case }
  Msg := 'Cannot evaluate: ' + Msg + #13 + 'Error at position ' + IntToStr(( sender as TMathParser ).Position);
  MessageDlg( Msg, mtError, [mbOk], 0 );
  LastEvalExprResult := '#ERROR';
end; // MathParserParseError


procedure TForm_Main.MMEditPasteEvalClick(Sender: TObject);
begin
  if ( NoteIsReadOnly( ActiveNote, true )) then exit;
  ActiveNote.Editor.SelText := LastEvalExprResult;
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
  myNode : TTreeNTNode;
  found : boolean;
begin

  myNode := GetCurrentTreeNode;
  if not assigned( myNode ) then
  begin
    showmessage( 'No notes in file, or current note is not a Tree-type note.' );
    exit;
  end;

  if ( SearchNode_Text = '' ) then
  begin
    SearchNode_Text := SearchNode_TextPrev;
    if InputQuery( 'Find tree node', 'Find node containing text:', SearchNode_Text ) then
      SearchNode_Text := ansilowercase( SearchNode_Text )
    else
      exit;
  end;

  if ( SearchNode_Text = '' ) then exit;

  found := false;
  myNode := myNode.GetNext;
  while assigned( myNode ) do
  begin
    if ( pos( SearchNode_Text, ansilowercase( myNode.Text )) > 0 ) then // {N}
    begin
      found := true;
      myNode.TreeView.Selected := myNode;
      break;
    end;
    myNode := myNode.GetNext;
  end;

  if ( not found ) then
    statusbar.panels[PANEL_HINT].Text := ' Node not found!';


end; // FindTreeNode

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
      'The Style toolbar must be visible to use this command. Show the Style toolbar now?',
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
    messagedlg( 'No style available or none selected', mtInformation, [mbOK], 0 );
    exit;
  end;
  if ( not assigned( StyleManager )) then
  begin
    messagedlg( 'Error: StyleManager does not exist.', mtError, [mbOK], 0 );
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


procedure TForm_Main.TVCopySubtreeClick(Sender: TObject);
begin
  TreeTransferProc(( sender as TMenuItem ).Tag, nil, KeyOptions.ConfirmTreePaste );
end;

procedure TForm_Main.NodesDropOnTabProc( const DropTab : TTab95Sheet );
var
  oldPage : TTab95Sheet;
  tNote : TTreeNote;
  s : string;
  DoMoveNodes : boolean;
begin
  DoMoveNodes := ( ShiftDown or KeyOptions.DropNodesOnTabMove );

  if ( DropTab = Pages.ActivePage ) then
  begin
    showmessage( 'Tree nodes can only be dropped on the tree, or on another tab.' );
    exit;
  end;

  if ( TTabNote( DropTab.PrimaryObject ).Kind <> ntTree ) then
  begin
    messagedlg( Format(
      'Cannot transfer nodes to "%s", because it is not a Tree-type note.',
      [DropTab.Caption]
      ), mtError, [mbOK], 0 );
    exit;
  end;

  tNote := TTreeNote( DropTab.PrimaryObject );
  oldPage := Pages.ActivePage;

  if tNote.ReadOnly then
  begin
    messagedlg( Format(
      'Cannot transfer nodes to "%s", because target note is Read-Only.',
      [DropTab.Caption]
    ), mtError, [mbOK], 0 );
    exit;
  end;

  if KeyOptions.DropNodesOnTabPrompt then
  begin
    if DoMoveNodes then
      s := 'Move'
    else
      s := 'Copy';
    if ( messagedlg( Format(
      '%s dragged nodes to tab "%s"?',
      [s, DropTab.Caption]
      ), mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then
      exit;
  end;

  try // [x]
    if TreeTransferProc( 0, nil, false ) then // step 1 - copy
    begin
      Pages.ActivePage := DropTab;
      PagesChange( Pages );
      if TreeTransferProc( 1, tNote, false ) then // step 2 - paste
      begin
        if DoMoveNodes then // MOVE instead of copy, so delete originals
        begin
          Pages.ActivePage := oldPage;
          PagesChange( Pages );
          DeleteTreeNode( true );
        end;
      end;
    end
    else
    begin
      messagedlg( 'Failed to acquire source nodes.', mtError, [mbOK], 0 );
    end;
  finally
    if ( TransferNodes <> nil ) then
    begin
      TransferNodes.Free;
      TransferNodes := nil;
    end;
  end;

end; // NodesDropOnTabProc

procedure TForm_Main.PagesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  i : integer;
  myTab : TTab95Sheet;
begin
  Accept := ( Source is TTreeNT );
  if Accept then
  begin
    i := Pages.GetTabAt(X,Y);
    myTab := Pages.Pages[i];
    if assigned( myTab ) then
      StatusBar.Panels[PANEL_HINT].Text := Format(
        ' Tab %d: %s', [i, myTab.Caption] );
  end;
end;

procedure TForm_Main.PagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin         // [dpv]
     Menu_TAB.Popup(x + Left, y + Top);
  end;

end;

// PagesDragOver

procedure TForm_Main.PagesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropTabIdx : integer;
begin
  DropTabIdx := Pages.GetTabAt( X, Y );
  if (( DropTabIdx >= 0 ) and ( DropTabIdx < Pages.PageCount )) then
  begin
    NodesDropOnTabProc( Pages.Pages[DropTabIdx] );
  end;
end; // PagesDragDrop

procedure TForm_Main.MMInsertTermClick(Sender: TObject);
begin
  ExpandTermProc;
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

procedure TForm_Main.TVExportClick(Sender: TObject);
begin
  ExportTreeNode;
end;

procedure TForm_Main.TVHideCheckedClick(Sender: TObject);   // [dpv]
var
  tNote : TTreeNote;
begin
  if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
  begin
    tNote := TTreeNote( ActiveNote );
    tNote.HideCheckedNodes := ( not tNote.HideCheckedNodes );
    MMViewHideCheckedNodes.Checked:= tNote.HideCheckedNodes;
    TB_HideChecked.Down := tNote.HideCheckedNodes;

    if MMViewHideCheckedNodes.Checked then
       HideCheckedNodes (tNote)
    else
       ShowCheckedNodes (tNote);
  end;
end;

procedure TForm_Main.MMEditTrimLeftClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
    TrimBlanks(( sender as TMenuItem ).Tag );
end;

procedure TForm_Main.MMEditCompressClick(Sender: TObject);
begin
  CompressWhiteSpace;
end;

procedure TForm_Main.MMEditInvertCaseClick(Sender: TObject);
begin
  PerformCmd( ecInvertCase );
end;

procedure TForm_Main.Form_CharsClosed( sender : TObject );
begin
  try
    try
      with Form_Chars.FontDlg.Font do
      begin
        InsCharFont.Name := Name;
        InsCharFont.Charset := Charset;
        InsCharFont.Size := Size;
      end;
      KeyOptions.InsCharFullSet := Form_Chars.myShowFullSet;
      Form_Chars.Release;
    except
    end;
  finally
    Form_Chars := nil;
  end;
end; // Form_CharsClosed

procedure TForm_Main.CharInsertProc( const ch : char; const Count : integer; const FontName : string; const FontCharset : TFontCharset );
var
  s : string;
begin
  if ( not ( HaveNotes( false, true ) and assigned( ActiveNote ))) then
    exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;

  try

    with CommandRecall do
    begin
      CharInfo.Code := ord( ch );
      CharInfo.Name := FontName;
      CharInfo.Count := Count;
      CharInfo.Charset := FontCharset;
    end;
    UpdateLastCommand( ecInsCharacter );
    if IsRecordingMacro then
      AddMacroEditCommand( ecInsCharacter );


    if ( FontName <> '' ) then
    begin
      NoteSelText.Name := FontName;
      NoteSelText.Charset := FontCharset;
    end;

    if ( Count = 1 ) then
    begin
      s := ch;
    end
    else
    begin
      s := '';
      setlength( s, Count );
      fillchar( s[1], Count, ch );
    end;

    with ActiveNote.Editor do
    begin
      SelText := s;
      SelStart := SelStart + Count;
    end;

    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );

  except
    showmessage( 'Unexpected error.' );
  end;

end; // CharInsertProc

procedure TForm_Main.MMInsertCharacterClick(Sender: TObject);
begin
  InsertSpecialCharacter;
end;

procedure TForm_Main.GoToEditorLine( s : string );
var
  curIdx, newIdx : integer;
  P : TPoint;
begin
  p := ActiveNote.Editor.CaretPos;
  curIdx := succ( p.Y ); // zero-based
  if ( s[1] in ['+','-'] ) then
  begin
    newIdx := curIdx + strtoint( s );
  end
  else
  begin
    newIdx := strtoint( s );
  end;

  if ( newIdx < 1 ) then
    newIdx := 1
  else
  if ( newIdx > ActiveNote.Editor.Lines.Count ) then
    newIdx := ActiveNote.Editor.Lines.Count;

  ActiveNote.Editor.selstart := ActiveNote.Editor.Perform( EM_LINEINDEX, pred( newIdx ), 0 );
  ActiveNote.Editor.Perform( EM_SCROLLCARET, 0, 0 );

end; // GoToEditorLine


procedure TForm_Main.MMFindBracketClick(Sender: TObject);
begin
  PerformCmdEx( ecMatchBracket );
end;

procedure TForm_Main.MMToolsStatisticsClick(Sender: TObject);
begin
  ShowStatistics;
end;


procedure TForm_Main.MMBkmJ9Click(Sender: TObject);
begin
  // Jump to bookmark
  if ( sender is TMenuItem ) then
  begin
    BookmarkGoTo( ord(( sender as TMenuItem ).Caption[2] ) - 48 );
  end;
end;

procedure TForm_Main.MMBkmSet9Click(Sender: TObject);
begin
  // Set bookmark
  if ( sender is TMenuItem ) then
  begin
    BookmarkAdd( ord(( sender as TMenuItem ).Caption[2] ) - 48 );
  end;
end;

procedure TForm_Main.MMFormatParagraphClick(Sender: TObject);
begin
  PerformCmd( ecParaDlg );
end;


procedure TForm_Main.MMInsertURLClick(Sender: TObject);
begin
  InsertURL('', '');   // Ask the user
end; // Insert URL


procedure TForm_Main.MMInsertLinkToFileClick(Sender: TObject);
begin
  InsertFileOrLink( '', true );
end;

procedure TForm_Main.MMInsertObjectClick(Sender: TObject);
begin
  InsertPictureOrObject( false );
end;

procedure TForm_Main.MMInsertPictureClick(Sender: TObject);
begin
  InsertPictureOrObject( true );
end;

procedure TForm_Main.WMJumpToKNTLink( var DummyMSG : integer );
begin
  JumpToKNTLocation( _GLOBAL_URLText );
end; // WMJumpToKNTLink

procedure TForm_Main.WMShowTipOfTheDay( var DummyMSG : integer );
begin
  ShowTipOfTheDay;
end; // WMShowTipOfTheDay

procedure TForm_Main.WMJumpToLocation( var DummyMSG : integer );
begin
  try
    if assigned( _Global_Location ) then
    begin
      if ( _Global_Location.FileName <> '' ) then
      begin
        if (( not fileexists( _Global_Location.Filename )) or
           ( NoteFileOpen( _Global_Location.Filename ) <> 0 )) then
        begin
          messagedlg( Format(
            'Could not open KeyNote file "%s"',
            [_Global_Location.Filename] ), mtError, [mbOK], 0 );
          exit;
        end;
      end;
      JumpToLocation( _Global_Location );
      ActiveNote.FocusMemory := focRTF;
      FocusActiveNote;
    end;
  finally
    _Global_Location := nil;
  end;
end; // WMJumpToLocation

procedure TForm_Main.MMEditPasteSpecialClick(Sender: TObject);
begin
  if ( not assigned( ActiveNote )) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;

  if ActiveNote.Editor.PasteSpecialDialog then
  begin
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;
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
  if messagedlg(
    'This command will start your browser and direct it to the author''s website, where ' +
    'you can download the latest version of the program, read the FAQ, and find more ' +
    'information about KeyNote and other freeware programs. Continue?',
    mtConfirmation, [mbOK,mbCancel], 0
    ) <> mrOK then exit;

  screen.Cursor := crHourGlass;
  ShellExecute( 0, 'open', PChar( Program_URL ), nil, nil, SW_NORMAL );
  screen.Cursor := crDefault;
end;

procedure TForm_Main.MMHelpEmailAuthorClick(Sender: TObject);
begin
  if messagedlg(
    'Feel free to send questions, bug reports or feature suggestions for KeyNote. PLEASE DO NOT SEND EMAIL IN HTML FORMAT! Continue?',
    mtConfirmation, [mbOK,mbCancel], 0
    ) <> mrOK then exit;

  screen.Cursor := crHourGlass;
  ShellExecute( 0, 'open', PChar( 'mailto:' + Program_Email ), nil, nil, SW_NORMAL );
  screen.Cursor := crDefault;
end;


procedure TForm_Main.MMInsertMarkLocationClick(Sender: TObject);
begin
  InsertOrMarkKNTLink( nil, false, '' );
end;

procedure TForm_Main.MMInsertKNTLinkClick(Sender: TObject);
begin
  InsertOrMarkKNTLink( nil, true, '');
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
begin
  // Combo_Macro.Hint := '';
  ListBox_ResMacro.Hint := '';
  Macro := GetCurrentMacro( false );
  if assigned( macro ) then
  begin
    StatusBar.Panels[PANEL_HINT].Text := Macro.Description;
    // Combo_Macro.Hint := Macro.Description;
    ListBox_ResMacro.Hint := Macro.Description;
  end
  else
  begin
    StatusBar.Panels[PANEL_HINT].Text := '';
  end;
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
  if ( not HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( ActiveNote.Kind <> ntTree ) then exit;

  with SaveDlg do
  begin
    oldFilter := Filter;
    Filter := FILTER_TEXTFILES;
    FilterIndex := 1;
    Title := 'Save tree structure to file';
    Options := Options - [ofAllowMultiSelect];
  end;

  try
    if SaveDlg.Execute then
    begin
      fn := normalFN( SaveDlg.Filename );
      TTreeNote( ActiveNote ) .TV.SaveToFile( fn, false );
    end;
  finally
    SaveDlg.Filter := oldFilter;
  end;

end;

procedure TForm_Main.MMHelpContentsClick(Sender: TObject);
begin
  //Application.HelpCommand( HELP_FINDER, 0 );    *1
  HtmlHelp(0, PAnsiChar(Application.HelpFile), HH_DISPLAY_TOPIC, 0);
end;

procedure TForm_Main.MMHelpKeyboardRefClick(Sender: TObject);
begin
//  Application.HelpCommand( HELP_CONTEXT, 30 );  *1
  HtmlHelp(0, PAnsiChar(Application.HelpFile), HH_HELP_CONTEXT, 30);
end;

procedure TForm_Main.MMHelpMainClick(Sender: TObject);
begin
//  Application.HelpCommand( HELP_CONTEXT, 10 );  *1
  HtmlHelp(0, PAnsiChar(Application.HelpFile), HH_HELP_CONTEXT, 10);
end;

(*
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
*)

(*
procedure TForm_Main.MMToolsCalculatorClick(Sender: TObject);
var
  RxCalculator : TRxCalculator;
begin
  RxCalculator := TRxCalculator.Create( self );
  try
    if RxCalculator.Execute then
    begin
      if ( assigned( ActiveNote ) and ( not ActiveNote.ReadOnly )) then
        ActiveNote.Editor.SelText := FloatToStr( RxCalculator.Value );
    end;
  finally
    RxCalculator.Free;
  end;
end; // MMCalculatorClick
*)

procedure TForm_Main.MMToolsTemplateCreateClick(Sender: TObject);
begin
  CreateTemplate;
end;


procedure TForm_Main.MMToolsTemplateInsertClick(Sender: TObject);
begin
  InsertTemplate( '' );
end;


procedure TForm_Main.TVCheckNodeClick(Sender: TObject);
var
  myNoteNode : TNoteNode;
  myTreeNode : TTreeNTNode;
begin
  if NoteIsReadOnly( ActiveNote, false ) then exit;
  myNoteNode := GetCurrentNoteNode;
  if ( not assigned( myNoteNode )) then exit;
  if ( ActiveNote.FocusMemory <> focTree ) then exit;
  myTreeNode := TTreeNote( ActiveNote ).TV.Selected;

  myNoteNode.Checked := ( not myNoteNode.Checked );
  TVCheckNode.Checked := myNoteNode.Checked;
  if myNoteNode.Checked then
    myTreeNode.CheckState := csChecked
  else
    myTreeNode.CheckState := csUnchecked;

end;

procedure TForm_Main.TVChildrenCheckboxClick(Sender: TObject);    // [dpv]
var
  myNoteNode : TNoteNode;
  myTreeNode : TTreeNTNode;
begin
  if NoteIsReadOnly( ActiveNote, false ) then exit;
  myNoteNode := GetCurrentNoteNode;
  if ( not assigned( myNoteNode )) then exit;
  if ( ActiveNote.FocusMemory <> focTree ) then exit;
  myTreeNode := TTreeNote( ActiveNote ).TV.Selected;

  myNoteNode.ChildrenCheckbox := ( not myNoteNode.ChildrenCheckbox );
  TVChildrenCheckbox.Checked := myNoteNode.ChildrenCheckbox;
  ShowOrHideChildrenCheckBoxes (myTreeNode);
end;

// TVCheckNodeClick


procedure TForm_Main.TVBoldNodeClick(Sender: TObject);
begin
  SetTreeNodeBold( ShiftDown );
end; // TVMBoldClick

procedure TForm_Main.MMViewTreeClick(Sender: TObject);
begin
  if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
  begin
    MMViewTree.Checked := ( not MMViewTree.Checked );
                                              
    TTreeNote( ActiveNote ).TreeHidden := ( not MMViewTree.Checked );
    UpdateTreeVisible( TTreeNote( ActiveNote ));

    if ( not MMViewTree.Checked ) then
    try
      ActiveNote.Editor.SetFocus;
    except
    end;

    MMViewNodeIcons.Enabled := MMViewTree.Checked;
    MMViewCustomIcons.Enabled := MMViewTree.Checked;
    MMViewCheckboxesAllNodes.Enabled := MMViewTree.Checked;
    MMViewHideCheckedNodes.Enabled := MMViewTree.Checked;     // [dpv]
  end;
end; // MMViewTreeClick




procedure TForm_Main.MMFormatLanguageClick(Sender: TObject);
begin
  PerformCmd( ecLanguage );
end;

procedure TForm_Main.MMNoteSpellClick(Sender: TObject);
begin
  RunSpellcheckerForNote;
end;

procedure TForm_Main.Markaslink1Click(Sender: TObject);
begin
  with NoteSelText do
    Link := ( not Link );
end;

procedure TForm_Main.Hiddentext1Click(Sender: TObject);
begin
  with NoteSelText do
    Hidden := ( not Hidden );
end;


procedure TForm_Main.MMInsHyperlinkClick(Sender: TObject);
begin
  // CreateHyperlink;
end;

procedure TForm_Main.RxRTFURLClick(Sender: TObject; const URLText: String; chrg: _charrange; Button: TMouseButton);
begin
  ClickOnURL (URLText, chrg);
end;

procedure TForm_Main.MMViewResPanelClick(Sender: TObject);
begin
  KeyOptions.ResPanelShow := ( not KeyOptions.ResPanelShow );

  UpdateResPanelContents;
  HideOrShowResPanel( KeyOptions.ResPanelShow );
  MMViewResPanel.Checked := KeyOptions.ResPanelShow;
  if KeyOptions.ResPanelShow then
    ResMHidepanel.Caption := 'Hide &Resource Panel'
  else
    ResMHidepanel.Caption := 'Show &Resource Panel';
  TB_ResPanel.Down := MMViewResPanel.Checked;
  if KeyOptions.ResPanelShow then
    FocusResourcePanel
  else
    FocusActiveNote;

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

procedure TForm_Main.Menu_RTFPopup(Sender: TObject);
begin
  RTFMWordwrap.Checked := ActiveNote.Editor.WordWrap;
end; // Menu_RTFPopup

procedure TForm_Main.Splitter_ResMoved(Sender: TObject);
begin
  {
  if ( ResPanelOptions.TabOrientation in [tabposTop, tabposBottom] ) then
    Combo_ResFind.Width := Pages_Res.Width - 22
  else
    Combo_ResFind.Width := Pages_Res.Width - 40;
  }
  Combo_ResFind.Width := ResTab_Find.Width - 15;
end;

procedure TForm_Main.Btn_ResFlipClick(Sender: TObject);
begin
  if Ntbk_ResFind.PageIndex = 0 then
  begin
    Btn_ResFlip.Caption := 'Results';
    Ntbk_ResFind.PageIndex := 1;
  end
  else
  begin
    Btn_ResFlip.Caption := 'Options';
    Ntbk_ResFind.PageIndex := 0;
  end;
end;

procedure TForm_Main.MMEditSelectWordClick(Sender: TObject);
begin
  PerformCmdEx( ecSelectWord );
end;

procedure TForm_Main.Pages_ResChange(Sender: TObject);
begin
  UpdateResPanelContents;
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
  end;
end;

procedure TForm_Main.ResMPluginTabClick(Sender: TObject);
var
  sheet : TTab95Sheet;
  CantHideTab : boolean;
  VisibleTabs : integer;

  procedure CannotHideTabMsg;
  begin
    messagedlg( 'Cannot hide the last visible tab. At least one tab must remain visible on the resource panel.', mtInformation, [mbOK], 0 );
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
  end;

  if (( sender is TMenuItem ) and assigned( sheet )) then
  begin
    if sheet.TabVisible then
      Pages_Res.ActivePage := sheet
    else
      Pages_Res.SelectNextPage( false );
    UpdateResPanelContents;
  end;

end; // ResMPluginTabClick

function TForm_Main.ExtIsHTML( const aExt : string ) : boolean;
begin
  result := ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtHTML ) > 0 )
end; // ExtIsHTML

function TForm_Main.ExtIsText( const aExt : string ) : boolean;
begin
  result := ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtText ) > 0 )
end; // ExtIsText

function TForm_Main.ExtIsRTF( const aExt : string ) : boolean;
begin
  // result := ( CompareText( aExt, ext_RTF ) = 0  );
  result := ( pos( ansilowercase( aExt )+'.', KeyOptions.ExtRTF ) > 0 )
end; // ExtIsRTF


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
    DBLCLK_FILEPROP : NoteFileProperties;
    DBLCLK_FILEMGR : RunFileManager;
    DBLCLK_NOTEPROP : EditNoteProperties( propThisNote );
    DBLCLK_NEWNOTE : CreateNewNote;
    DBLCLK_RESPANEL : MMViewResPanelClick( nil );
  end;
end;

procedure TForm_Main.Combo_ResFindChange(Sender: TObject);
begin
  Btn_ResFind.Enabled := ( Combo_ResFind.Text <> '' );
end;

procedure TForm_Main.Btn_ResFindClick(Sender: TObject);
begin
  RunFindAllEx;
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

procedure TForm_Main.Combo_FontSizeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    FocusActiveNote;
  end;
end;

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
      messagedlg( 'Resource panel position will be updated after KeyNote is restarted.', mtInformation, [mbOK], 0 );
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
           SelText := Clipboard.AsText;
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
          TempRichText := GetRichtext( Res_RTF, true, false );
          Res_RTF.WordWrap := ( not Res_RTF.WordWrap );
          ( sender as TMenuItem ).Checked := Res_RTF.WordWrap;
          Res_RTF.Lines.BeginUpdate;
          try
            Res_RTF.Lines.Clear;
            PutRichText( TempRichText, Res_RTF, true, true );
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
      SendMessage( Handle, EM_GETSEL, Longint( @Selection.StartPos ),
        Longint( @Selection.EndPos ));
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
  CreateMasterNode;
end;

procedure TForm_Main.TVCopyNodePathClick(Sender: TObject);
begin
  CopyNodePath( false );
end;

procedure TForm_Main.TVCopyNodeTextClick(Sender: TObject);
begin
  CopyNodeName( true );
end;

procedure TForm_Main.TVCopyPathtoEditorClick(Sender: TObject);
begin
  CopyNodePath( true );
end;

procedure TForm_Main.FAMCopytoEditorClick(Sender: TObject);
begin
  FindResultsToEditor( true );
end;

procedure TForm_Main.FAMCopyAlltoEditorClick(Sender: TObject);
begin
  FindResultsToEditor( false );
end;

procedure TForm_Main.RG_ResFind_TypeClick(Sender: TObject);
begin
  CB_ResFind_NodeNames.Enabled := ( TSearchMode( RG_ResFind_Type.ItemIndex ) = smPhrase );
end;

procedure TForm_Main.MMTreeNodeFromSelClick(Sender: TObject);
begin
  CreateNodefromSelection;
end;


{$IFDEF WITH_IE}
function TForm_Main.SelectVisibleControlForNode( const aNode : TNoteNode ) : TNodeControl;
begin
  result := ncNone;
  if ( not assigned( aNode )) then exit;

  case aNode.VirtualMode of
    vmNone, vmText, vmRTF, vmHTML : result := ncRTF;
    vmIELocal, vmIERemote : begin
      if _IE4Available then
        result := ncIE;
    end;
  end;
end; // SelectVisibleControlForNode
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
  fn, nn : string;
begin
  myFav := GetSelectedFavorite;
  if ( not assigned( myFav )) then exit;

  if myFav.ExternalDoc then
  begin
    StatusBar.Panels[PANEL_HINT].Text := Format(
      'External: %s',
      [myFav.Filename] );
    exit;
  end;

  if (( not HaveNotes( false, false )) or
     ( AnsiCompareText( NoteFile.FileName, myFav.FileName ) <> 0 )) then
  begin
    fn := ' File: ' + extractfilename( myFav.Filename );
  end
  else
  begin
    fn := '';
  end;

  if ( myFav.NodeID > 0 ) then
  begin
    nn := ' Node: ' + myFav.NodeName;
  end
  else
  begin
    nn := '';
  end;

  StatusBar.Panels[PANEL_HINT].Text := Format(
    '%s Note: %s%s',
    [fn, myFav.NoteName, nn]
  );

end; // ListBox_ResFavClick

procedure TForm_Main.ListBox_ResTplClick(Sender: TObject);
begin
  StatusBar.Panels[PANEL_HINT].Text := 'Double-click to insert selected template';
end;

procedure TForm_Main.TVUnlinkVirtualNodeClick(Sender: TObject);
begin
  VirtualNodeUnlink;
end;

procedure TForm_Main.FavMAddExternalClick(Sender: TObject);
begin
  AddFavorite( true );
end;



procedure TForm_Main.MMTreeOutlineNumClick(Sender: TObject);
begin
  OutlineNumberNodes;
end;

procedure TForm_Main.MMTreeGoBackClick(Sender: TObject);
begin
  NavigateInHistory( false );
end;

procedure TForm_Main.MMTreeGoForwardClick(Sender: TObject);
begin
  NavigateInHistory( true );
end;

procedure TForm_Main.MMUpRomanClick(Sender: TObject);
begin
  KeyOptions.LastNumbering := TRxNumbering(( sender as TMenuItem ).Tag );
  ( sender as TMenuItem ).Checked := true;
end;

procedure TForm_Main.MMHelpWhatsNewClick(Sender: TObject);
begin
  DisplayHistoryFile;
end;

procedure TForm_Main.MMViewTBRefreshClick(Sender: TObject);
begin
  if fileexists( Toolbar_FN ) then
  begin
    LoadToolbars;
    ResolveToolbarRTFv3Dependencies;
  end
  else
  begin
    SaveToolbars;
    messagedlg( Format(
      'Toolbar configuration file "%s" not found. Default toolbar configuration file has been created.', [Toolbar_FN] ), mtError, [mbOK], 0 );
  end;
end;

procedure TForm_Main.MMViewTBSaveConfigClick(Sender: TObject);
begin
  SaveToolbars;
  messagedlg( Format(
    'Saved toolbar layout to "%s".', [Toolbar_FN] ), mtInformation, [mbOK], 0 );
end;

procedure TForm_Main.TVSelectNodeImageClick(Sender: TObject);
begin
  SetTreeNodeCustomImage;
end;


procedure TForm_Main.TVNodeTextColorClick(Sender: TObject);
begin
  SetTreeNodeColor( true, true, false, ShiftDown );
end;

procedure TForm_Main.TVDefaultNodeFontClick(Sender: TObject);
var
  ShiftWasDown : boolean;
begin
  ShiftWasDown := ShiftDown;
  SetTreeNodeColor( false, true, true, ShiftWasDown );
  SetTreeNodeColor( false, false, true, ShiftWasDown );
  SetTreeNodeFontFace( true, ShiftWasDown );
end;

procedure TForm_Main.TVNodeBGColorClick(Sender: TObject);
begin
  SetTreeNodeColor( true, false, false, ShiftDown );
end;

procedure TForm_Main.Res_RTFKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TForm_Main.RxRTFStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  _Is_Dragging_Text := true;
  // StatusBar.Panels[0].Text := 'RTF start drag';
end;

procedure TForm_Main.RxRTFEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  _Is_Dragging_Text := false;
  // StatusBar.Panels[0].Text := 'RTF end drag';
end;


procedure TForm_Main.MMToolsCustomKBDClick(Sender: TObject);
begin
  CustomizeKeyboard;
end;



procedure TForm_Main.MMTreeNavRightClick(Sender: TObject);
begin
  if ( sender is TMenuItem ) then
    NavigateInTree( TNavDirection(( sender as TMenuItem ).Tag ));
end;

procedure TForm_Main.MMToolsExportExClick(Sender: TObject);
begin
  ExportNotesEx;
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
  SetEditorZoom( 100, '' );
end;

procedure TForm_Main.MMViewZoomInClick(Sender: TObject);
begin
  if ShiftDown then
    SetEditorZoom( 100, '' )
  else
    SetEditorZoom( GetEditorZoom + KeyOptions.ZoomIncrement, '' );
end;

procedure TForm_Main.MMViewZoomOutClick(Sender: TObject);
var
  NewZoom : integer;
begin
  if ShiftDown then
    SetEditorZoom( 100, '' )
  else
  begin
    NewZoom := GetEditorZoom - KeyOptions.ZoomIncrement;
    if ( NewZoom <= 0 ) then
      NewZoom := _ZOOM_MIN;
    SetEditorZoom( NewZoom, '' );
  end;
end;

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
    canvas.TextRect( Rect, Rect.Left+2, Rect.Top, Items[index] );
  end;
end;

procedure TForm_Main.Menu_TVPopup(Sender: TObject);
var
  myNode : TNoteNode;
begin
  myNode := GetCurrentNoteNode;
  if assigned( myNode ) then
    VirtualNodeUpdateMenu( myNode.VirtualMode <> vmNone )
  else
    VirtualNodeUpdateMenu( false );
end;


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
  if ( not assigned( ActiveNote )) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( sender is TMenuItem ) then
  begin
    ItemTag := ( sender as TMenuItem ).Tag;
    if (( ItemTag > 0 ) and ( ItemTag <= DATE_FORMAT_LIST.Count )) then
    begin
      case ItemTag of
        1 : KeyOptions.DTLastDateFmt := KeyOptions.DateFmt;
        else
        begin
          KeyOptions.DTLastDateFmt := DATE_FORMAT_LIST[ItemTag-1];
        end;
      end;

      ActiveNote.Editor.SelText := GetDateTimeFormatted( KeyOptions.DTLastDateFmt, now ) + #32;
      ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
      ( sender as TMenuItem ).Checked := true;
    end;
  end;
end;

procedure TForm_Main.mt8Click(Sender: TObject);
var
  ItemTag : integer;
begin
  if ( not assigned( ActiveNote )) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( sender is TMenuItem ) then
  begin
    ItemTag := ( sender as TMenuItem ).Tag;
    if (( ItemTag > 0 ) and ( ItemTag <= TIME_FORMAT_LIST.Count )) then
    begin
      case ItemTag of
        1 : KeyOptions.DTLastTimeFmt := KeyOptions.TimeFmt;
        else
          KeyOptions.DTLastTimeFmt := TIME_FORMAT_LIST[ItemTag-1];
      end;

      ActiveNote.Editor.SelText := GetDateTimeFormatted( KeyOptions.DTLastTimeFmt, now ) + #32;
      ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + ActiveNote.Editor.SelLength;
      ( sender as TMenuItem ).Checked := true;
    end;
  end;
end;

procedure TForm_Main.Menu_SymbolsPopup(Sender: TObject);
var
  i, cnt : integer;
  item : TMenuItem;
begin
  cnt := Menu_Symbols.Items.Count;
  for i := 1 to cnt do
  begin
    item := Menu_Symbols.Items[pred( i )];
    if ( Item.Tag > 0 ) then
      item.Caption := SYMBOL_NAME_LIST[Item.Tag];
  end;
end;


procedure TForm_Main.ms11Click(Sender: TObject);
var
  t : integer;
begin
  if ( not assigned( ActiveNote )) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;
  if ( sender is TMenuItem ) then
  begin
    t := ( sender as TMenuItem ).Tag;
    if (( t > 0 ) and ( t <= high( SYMBOL_CODE_LIST ))) then
    begin
      ActiveNote.Editor.SelText := SYMBOL_CODE_LIST[t];
      ActiveNote.Editor.SelStart := ActiveNote.Editor.SelStart + 1;
    end;
  end;

end;



procedure TForm_Main.MMToolsURLClick(Sender: TObject);
var
  pt : TPoint;
begin
  // fake a mouseclick to simulate clicking a hyperlink

  if ( not assigned( ActiveNote )) then exit;

  GetCaretPos( pt );
  // pt := ActiveNote.Editor.ClientToScreen( pt );
  // SetCursorPos( pt.x, pt.y );
  _IS_FAKING_MOUSECLICK := true;

  {
  mouse_event( MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0 );
  mouse_event( MOUSEEVENTF_LEFTUP, 0, 0, 0, 0 );
  }

  // alternate solution, does not move the mouse
  PostMessage( ActiveNote.Editor.Handle, WM_LBUTTONDOWN, MK_LBUTTON,
               MakeLParam( pt.x, pt.y ));
  PostMessage( ActiveNote.Editor.Handle, WM_LBUTTONUP, 0,
               MakeLParam( pt.x, pt.y ));

end; // MMToolsURLClick


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


  if KeyOptions.StyleShowSamples then
  begin
    // draw style sample (only for styles which have font info)
    // but do not draw the associated images

    myStyle := TStyle( StyleManager.Objects[Index] );
    if ( myStyle.Range <> srParagraph ) then
    begin
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

    Combo_Style.Canvas.FillRect( Rect );
    inc( Rect.Left, myOffset );

    DrawText( Combo_Style.Canvas.Handle, PChar(myText), myLen, Rect, DT_SingleLine or DT_VCenter);

  end
  else
  begin
    // do not draw style samples,
    // but show style images instead

    myIconIdx := STYLE_IMAGE_BASE + ord( TStyle( StyleManager.Objects[Index] ).Range );

    myIcon := TIcon.Create;
    try
      Combo_Style.Canvas.FillRect( Rect );
      IMG_Toolbar.GetIcon( myIconIdx, myIcon );
      myPos := Point( myOffset + 2, Rect.Top + (( Rect.Bottom - Rect.Top - IMG_Toolbar.Height) div 2));
      DrawIconEx( Combo_Style.Canvas.Handle, myPos.X, myPos.Y, myIcon.Handle, IMG_Toolbar.Width, IMG_Toolbar.Height, 0, Combo_Style.Canvas.Brush.Handle, DI_Normal );

      inc( myOffset, myPos.X+IMG_Toolbar.Width+2 );

      SetRect( myRect, myOffset, Rect.Top, Rect.Right, Rect.Bottom);
      DrawText( Combo_Style.Canvas.Handle, PChar(myText), myLen, myRect, DT_SingleLine or DT_VCenter);

    finally
      myIcon.Free;
    end;

  end;
end;

procedure TForm_Main.DoBorder1Click(Sender: TObject);
var
  Paragraph: TParaFormat2;
begin
  FillChar(Paragraph, SizeOf(Paragraph), 0);
  Paragraph.cbSize := SizeOf(Paragraph);

  Paragraph.dwMask := PFM_BORDER;
  Paragraph.wBorders := 64;

  SendMessage( ActiveNote.Editor.Handle, EM_SETPARAFORMAT, 0, LPARAM(@Paragraph));
end;

procedure TForm_Main.MMEditDecimalToRomanClick(Sender: TObject);
begin
  ArabicToRoman;
end;


procedure TForm_Main.MMEditRomanToDecimalClick(Sender: TObject);
begin
  RomanToArabic;
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
end; // UpdateStatusBarState


procedure TForm_Main.UpdateTreeVisible( const ANote : TTreeNote );
begin
  with ANote do
  begin
    TV.Visible := ( not TreeHidden );
    if TreeHidden then
      FocusMemory := focRTF;
  end;
end; // UpdateTreeVisible

procedure TForm_Main.FavMPropertiesClick(Sender: TObject);
begin
  FavoriteEditProperties;
end;



procedure TForm_Main.MMEditPasteAsWebClipClick(Sender: TObject);
begin
  PasteAsWebClip;
end;

end.


