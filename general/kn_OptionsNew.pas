unit kn_OptionsNew;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

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
   System.Zip,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Samples.Spin,
   Vcl.ExtCtrls,
   Vcl.Mask,
   Vcl.Buttons,
   Vcl.ComCtrls,
   Vcl.ExtDlgs,
   Vcl.FileCtrl,

   ComCtrls95,
   cmpGFXListBox,
   TB97Ctls,
   VirtualTrees, VirtualTrees.BaseTree, VirtualTrees.BaseAncestorVCL, VirtualTrees.AncestorVCL,

   gf_miscvcl,
   kn_Const,
   kn_Info
;


type
  TOption = record
     Caption: string;
     Parent: boolean;
  end;
  POption = ^TOption;


type
  TForm_OptionsNew = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    FontDlg: TFontDialog;
    ColorDlg: TColorDialog;
    IconDlg: TOpenDialog;
    TV: TVirtualStringTree;
    Pages: TNotebook;
    GroupBox_General1: TGroupBox;
    checkbox_IconInTray: TCheckBox;
    checkbox_StartMinimized: TCheckBox;
    CheckBox_MinimizeOnClose: TCheckBox;
    CheckBox_ShowTooltips: TCheckBox;
    CheckBox_SplashScreen: TCheckBox;
    GroupBox_Files1: TGroupBox;
    CB_LoadLastFile: TCheckBox;
    CB_LoadUserFile: TCheckBox;
    Edit_UserFile: TEdit;
    GroupBox_FileOpt1: TGroupBox;
    CheckBox_MRUSubmenu: TCheckBox;
    CheckBox_MRUUse: TCheckBox;
    CheckBox_MRUFullPath: TCheckBox;
    Spin_MRUCount: TSpinEdit;
    GroupBox_General2: TGroupBox;
    Label4: TLabel;
    Combo_EscapeAction: TComboBox;
    Edit_HotKey: THotKey;
    CheckBox_HotkeyActivate: TCheckBox;
    GroupBox_Actions1: TGroupBox;
    Label9: TLabel;
    Label10: TLabel;
    Bevel4: TBevel;
    CheckBox_TimerMinimize: TCheckBox;
    CheckBox_TimerClose: TCheckBox;
    CB_CloseEncOnly: TCheckBox;
    Spin_TimerMinInt: TSpinEdit;
    Spin_TimerCloseInt: TSpinEdit;
    GroupBox_Formats1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label_SampleDate: TLabel;
    Label_SampleTime: TLabel;
    Edit_DateFormat: TComboBox;
    Edit_TimeFormat: TComboBox;
    GroupBox_Adv: TGroupBox;
    Label16: TLabel;
    CheckBox_DisableFileMon: TCheckBox;
    CheckBox_ShowFonts: TCheckBox;
    CheckBox_NoRegistry: TCheckBox;
    CheckBox_UseOldColorDlg: TCheckBox;
    CheckBox_RunAutoMacros: TCheckBox;
    Group_ClipboardCapture: TGroupBox;
    Combo_Divider: TComboBox;
    CB_IgnoreSelf: TCheckBox;
    CB_AsText: TCheckBox;
    BitBtn_TknHlp: TBitBtn;
    CheckBox_ClipRecall: TCheckBox;
    CheckBox_Sound: TCheckBox;
    GroupBox_RTFEdit1: TGroupBox;
    CheckBox_WordSelect: TCheckBox;
    CB_SaveCaretPos: TCheckBox;
    CheckBox_TrackCaretPos: TCheckBox;
    CheckBox_AutoIndent: TCheckBox;
    GBox_TreeGlobal: TGroupBox;
    CheckBox_EditNewNodes: TCheckBox;
    CheckBox_EditInPlace: TCheckBox;
    GroupBox_Files2: TGroupBox;
    Checkbox_AutoSave: TCheckBox;
    CheckBox_AutoSaveOnFocus: TCheckBox;
    CheckBox_AutoSaveOnTimer: TCheckBox;
    Spin_AutoSaveOnTimerInt: TSpinEdit;
    Label_Minutes: TLabel;
    GroupBox_FileOpt2: TGroupBox;
    CheckBox_OpenFloppyReadOnly: TCheckBox;
    CheckBox_OpenReadOnlyWarn: TCheckBox;
    CheckBox_OpenNetworkReadOnly: TCheckBox;
    GroupBox_Chrome2: TGroupBox;
    CheckBox_TabsStacked: TCheckBox;
    CheckBox_TabsHotTrack: TCheckBox;
    CheckBox_TabsImages: TCheckBox;
    CheckBox_HotKeyWarn: TCheckBox;
    GroupBox_FileOpt3: TGroupBox;
    CheckBox_AutoRegisterPrompt: TCheckBox;
    CheckBox_AutoRegisterFileType: TCheckBox;
    CheckBox_EncFileAltExt: TCheckBox;
    GroupBox_Chrome1: TGroupBox;
    Label5: TLabel;
    RB_ActiveTab: TRadioButton;
    RB_InactiveTab: TRadioButton;
    Edit_Sample: TEdit;
    BTN_Font: TBitBtn;
    BTN_Color: TBitBtn;
    BTN_Defaults: TBitBtn;
    GroupBox_TabIcons: TGroupBox;
    Label_ICN: TLabel;
    Button_ICNAdd: TButton;
    Button_ICNInsert: TButton;
    Button_ICNDelete: TButton;
    Button_ICNReset: TButton;
    GroupBox_Formats2: TGroupBox;
    CheckBox_InsCharWinClose: TCheckBox;
    GroupBox_Act2: TGroupBox;
    Label8: TLabel;
    Combo_URLAction: TComboBox;
    //CheckBox_URLShift: TCheckBox;
    CheckBox_MinOnURL: TCheckBox;
    CheckBox_URLFileAuto: TCheckBox;
    GroupBox_Conf: TGroupBox;
    CheckBox_AutoPasteEval: TCheckBox;
    CheckBox_AutoPastePlugin: TCheckBox;
    CheckBox_ConfirmTreePaste: TCheckBox;
    checkbox_ConfirmExit: TCheckBox;
    checkbox_ConfirmDelete: TCheckBox;
    CheckBox_ConfirmNodeDelete: TCheckBox;
    CheckBox_FixScrollBars: TCheckBox;
    CheckBox_AutoNameVNodes: TCheckBox;
    Button_Help: TButton;
    CheckBox_LongCombos: TCheckBox;
    GroupBox_FileTypes: TGroupBox;
    List_TxtExt: TListBox;
    Button_AddTxtExt: TButton;
    Button_DeleteTxtExt: TButton;
    Button_ResetTxtExt: TButton;
    GroupBox_RTFEdit2: TGroupBox;
    Label12: TLabel;
    Label11: TLabel;
    Label1: TLabel;
    Label_UndoLimit: TLabel;
    Spin_UndoLimit: TSpinEdit;
    Spin_IndentInc: TSpinEdit;
    Spin_FontSizeInc: TSpinEdit;
    Spin_ParaSpaceInc: TSpinEdit;
    List_Icn: TGFXListBox;
    Bevel1: TBevel;
    CheckBox_RichEditv3: TCheckBox;
    GroupBox_Other1: TGroupBox;
    CB_AutoFont: TCheckBox;
    CB_AutoKeyboard: TCheckBox;
    Label6: TLabel;
    CB_IgnoreUpgrades: TCheckBox;
    CB_ConfirmNodeRefresh: TCheckBox;
    GroupBox_Back: TGroupBox;
    checkbox_Backup: TCheckBox;
    CheckBox_BackupAppendExt: TCheckBox;
    Edit_BackupExt: TEdit;
    Label_BakDir: TLabel;
    RB_BakOriginalDir: TRadioButton;
    RB_BakUserDir: TRadioButton;
    Edit_BakDir: TEdit;
    Label_MaxBak2: TLabel;
    Combo_BakLevel: TComboBox;
    Bevel3: TBevel;
    Label_MaxBak1: TLabel;
    Label13: TLabel;
    CB_SkipNewFilePrompt: TCheckBox;
    Label15: TLabel;
    Combo_TabPos: TComboBox;
    CheckBox_SingleInstance: TCheckBox;
    CB_InheritNodeProperties: TCheckBox;
    CB_BackupVNodes: TCheckBox;
    CheckBox_HotTrackTree: TCheckBox;
    CheckBox_AutoScroll: TCheckBox;
    CheckBox_TreeTips: TCheckBox;
    CB_ShowFullPath: TCheckBox;
    CB_ShowFullPathSearch: TCheckBox;
    CB_PathTopToBottom: TCheckBox;
    Bevel6: TBevel;
    GroupBox18: TGroupBox;
    CB_DropNodesOnTabPrompt: TCheckBox;
    CB_ResPanelActiveUpdate: TCheckBox;
    Label17: TLabel;
    Combo_ExpandMode: TComboBox;
    GroupBox23: TGroupBox;
    RB_ClipTreeActive: TRadioButton;
    RB_ClipTreeNew: TRadioButton;
    LB_ClipNodeNaming: TLabel;
    Combo_ClipNodeNaming: TComboBox;
    CB_AutoNewFile: TCheckBox;
    CB_TrackWordCount: TCheckBox;
    CB_TreeClipConfirm: TCheckBox;
    CB_TestDupClips: TCheckBox;
    CB_DTUseLastSelection: TCheckBox;
    GroupBox_Other3: TGroupBox;
    CB_WordAtCursor: TCheckBox;
    CB_FindAutoClose: TCheckBox;
    CB_TimerCloseDialogs: TCheckBox;
    CB_TimerCloseAutoReopen: TCheckBox;
    CB_SwitchIcon: TCheckBox;
    GroupBox_Other2: TGroupBox;
    RB_URLSystemBrowser: TRadioButton;
    RB_URLAltBrowser: TRadioButton;
    Edit_URLAltBrowserPath: TEdit;
    Label18: TLabel;
    Combo_Language: TComboBox;
    TB_OpenDlgUserFile: TToolbarButton97;
    TB_OpenDlgURLAltBrowserPath: TToolbarButton97;
    TB_OpenDlgBakDir: TToolbarButton97;
    CB_BackupRegularIntervals: TCheckBox;
    Label7: TLabel;
    Label_MaxSize: TLabel;
    Label_PlainTextMode: TLabel;
    Combo_Size: TComboBox;
    Combo_PlainTextMode: TComboBox;
    Group_WebClip: TGroupBox;
    Label19: TLabel;
    Combo_WCDivider: TComboBox;
    CB_PlainDefaultPaste: TCheckBox;
    BitBtn_TknHlp2: TBitBtn;
    CB_SourceURL: TCheckBox;
    CheckBox_InheritBGColor: TCheckBox;
    Spin_FontSizeFindResults: TSpinEdit;
    Label20: TLabel;
    Label21: TLabel;
    Combo_URLCtrlAction: TComboBox;
    GroupBox_Images: TGroupBox;
    Label23: TLabel;
    chkImgDefaultLinkMode: TCheckBox;
    chkImgLinkRelativePath: TCheckBox;
    CbImgDefaultFormatFromClipb: TComboBox;
    chkImgUseRecycleBin: TCheckBox;
    CbImgDefaultCompression: TComboBox;
    Label24: TLabel;
    CbImgBmpPixelFormat: TComboBox;
    Label25: TLabel;
    CbImgStorageModeOnExport: TComboBox;
    Label27: TLabel;
    txtImgMaxAutoWidthGoal: TEdit;
    Label28: TLabel;
    Label30: TLabel;
    txtImgRatioSizePngVsJPG: TEdit;
    txtImgCompressionQuality: TEdit;
    Label31: TLabel;
    gbViewer: TGroupBox;
    btnBGColor: TBitBtn;
    Label29: TLabel;
    chkImgSingleViewerInstance: TCheckBox;
    chkImgHotTrackViewer: TCheckBox;
    chkImgSaveInSubfolders: TCheckBox;
    gbStorage: TGroupBox;
    CbImgDefaultStorageMode: TComboBox;
    Label26: TLabel;
    cbImgDefaultExternalStorage: TComboBox;
    Label22: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    chkImgKeepOrigName: TCheckBox;
    Label14: TLabel;
    Spin_ResetNextAftN: TSpinEdit;
    CB_ExtKNTLnkInNewInst: TCheckBox;
    cbCtrlUpDownMode: TComboBox;
    Label34: TLabel;
    btnFBNew: TButton;
    btnFBEdit: TButton;
    btnFBDelete: TButton;
    LVfb: TListView;
    lbl1: TLabel;
    txtSepInLists: TEdit;
    lblTab: TLabel;
    lbl2: TLabel;
    CB_LineEdWidth: TCheckBox;
    procedure TB_OpenDlgBakDirClick(Sender: TObject);
    procedure TB_OpenDlgURLAltBrowserPathClick(Sender: TObject);
    procedure TB_OpenDlgUserFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure Checkbox_LoadLastFileClick(Sender: TObject);
    procedure Checkbox_LoadUserFileClick(Sender: TObject);
    procedure CheckBox_MRUUseClick(Sender: TObject);
    procedure checkbox_AutoSaveClick(Sender: TObject);
    procedure CheckBox_AutoSaveOnTimerClick(Sender: TObject);
    procedure Edit_DateFormatChange(Sender: TObject);
    procedure Edit_TimeFormatChange(Sender: TObject);
    procedure CheckBox_ShowTooltipsClick(Sender: TObject);
    procedure BTN_DefaultsClick(Sender: TObject);
    procedure BTN_FontClick(Sender: TObject);
    procedure RB_ActiveTabClick(Sender: TObject);
    procedure BTN_ColorClick(Sender: TObject);
    procedure Button_ICNAddClick(Sender: TObject);
    procedure Button_ICNInsertClick(Sender: TObject);
    procedure Button_ICNDeleteClick(Sender: TObject);
    procedure Button_ICNResetClick(Sender: TObject);
    procedure Combo_SizeKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox_AutoRegisterFileTypeClick(Sender: TObject);
    procedure BitBtn_TknHlpClick(Sender: TObject);
    procedure checkbox_BackupClick(Sender: TObject);
    procedure CheckBox_TimerMinimizeClick(Sender: TObject);
    procedure CheckBox_TimerCloseClick(Sender: TObject);
    procedure CheckBox_HotkeyActivateClick(Sender: TObject);
    procedure Button_AddTxtExtClick(Sender: TObject);
    procedure Button_DeleteTxtExtClick(Sender: TObject);
    procedure Button_ResetTxtExtClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    procedure RB_BakOriginalDirClick(Sender: TObject);
    procedure RB_ClipTreeActiveClick(Sender: TObject);
    procedure CheckBox_TrackCaretPosClick(Sender: TObject);
    procedure CB_TrackWordCountClick(Sender: TObject);
    procedure CheckBox_AutoSaveOnFocusClick(Sender: TObject);
    procedure CB_AsTextClick(Sender: TObject);
    procedure CB_ShowFullPathClick(Sender: TObject);
    procedure btnBGColorClick(Sender: TObject);
    procedure txtImgMaxAutoWidthGoalExit(Sender: TObject);
    procedure txtImgCompressionQualityExit(Sender: TObject);
    procedure txtImgRatioSizePngVsJPGExit(Sender: TObject);
    procedure chkImgSingleViewerInstanceClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure TVChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TVGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TVPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure btnFBNewClick(Sender: TObject);
    procedure btnFBEditClick(Sender: TObject);
    procedure btnFBDeleteClick(Sender: TObject);
    procedure txtSepInListsExit(Sender: TObject);
  private
    fOptions: array[0..16] of TOption;
    procedure CheckImgMaxAutoWidthGoalValue;
    procedure CheckImgCompressionQualityValue;
    procedure CheckImgRatioSizePngVsJPGValue;
    procedure CheckNumbListSepFactor;
    procedure CreateMenu;

  public
    Initializing : boolean;
    OK_Click : boolean;
    myOpts : TKeyOptions;
    myTabOpts : TTabOptions;
    myClipOpts : TClipOptions;
    myTreeOpts : TKntTreeOptions;
    myFindOpts : TFindOptions;

    myEditorOptions : TEditorOptions;
    myTreeOptions : TKntTreeOptions;

    Icons_Changed : boolean;
    Icons_RefList : TStringList;
    Icon_Change_Canceled : boolean;
    Icons_Change_Disable : boolean;
    AutoCloseWarned : boolean;

    FoldingBlocks_Changed : boolean;


    procedure FormToOptions;
    procedure OptionsToForm;
    procedure UpdateDateFmt;
    procedure UpdateTimeFmt;
    procedure ResetChromeDefaults;
    procedure UpdateFontSample;
    procedure LoadIcons;
    procedure SetIcon( const Adding : boolean );
    procedure DeleteIcon;
    procedure ResetIcons;
    procedure ClipCapToForm;
    function FormToClipCap : boolean;
    procedure DeleteBlock;
    procedure EditBlock( const NewBlock : boolean );
  end;


implementation
uses
    SynGdiPlus,
    gf_misc,
    gf_strings,
    kn_EditorUtils,
    kn_ImageForm,
    kn_Chest,
    kn_INI,
    kn_DateTime,
    kn_LanguagesMng,
    kn_global,
    kn_Main,
    kn_FoldBlockDef,
    knt.ui.editor,
    knt.App,
    knt.RS
    ;

{$R *.DFM}


const
  DIVIDER1: string= '^%|^%S%d^%|^%%^- - -^';
  DIVIDER2: string= '^^%|- >  %S %d  %|- - - - - - -%|^%|^%%^- -^';
  DIVIDER3: string= '^^%|=>  %S %d  %|======%|^%|^%%^- - - -^';
  DIVIDER4: string= '^^- - - - - - - - - - - -^ %|>>   %S %d^%|%%^- - -^';
  DIVIDER5: string= '^^--- %S%d^%%^. . .^';
  DIVIDER6: string= '^%|^%|- - - - - - - -  %S%d %|- - - -%|^%%^- - -^';
  DIVIDER7: string= '^- - - - - - - -%|^%S%d%|^%%^--^';
  DIVIDER8: string= '^^-- %D %|-  [ %S ] %|--^%%^- -^';
  DIVIDER9: string= '^^^--- %D, %T %| - - -  %S%|^^%%^. . .^';
  DIVIDER10:string= '^^^--- %D, %T %| -  %U%|^^%%^---^';
  DIVIDER11:string= '^^%%^';
  DIVIDER12:string= '^^%%^^';


procedure TForm_OptionsNew.FormCreate(Sender: TObject);
var
  u : TURLAction;
  i : integer;
  tp : TTabOrientation;
  txm : TTreeExpandMode;
  cln : TClipNodeNaming;
  clpt: TClipPlainTextMode;
  Language : TLanguageInfo;
begin
  Initializing := true;
  App.ApplyBiDiModeOnForm(Self);

  OK_Click := false;
  Pages.PageINdex := 0;
  AutoCloseWarned := false;
  List_Icn.CheckBoxes := false;

  for i := 0 to LanguagesAvailables.Count -1 do begin
    Language := TLanguageInfo( LanguagesAvailables.Objects[i] );
    Combo_Language.Items.Add( Language.Name );
  end;

  with Combo_Divider.Items do begin
    Add( DIVIDER9 );
    Add( DIVIDER8 );
    Add( DIVIDER5 );
    Add( DIVIDER6 );
    Add( DIVIDER2 );
    Add( DIVIDER3 );
    Add( DIVIDER4 );
    Add( DIVIDER1 );
    Add( DIVIDER7 );
    Add( DIVIDER10 );
    Add( DIVIDER11 );
    Add( DIVIDER12 );
    Add( DIV_1_BLANK );
    Add( DIV_2_BLANK );
  end;

  with Combo_WCDivider.Items do begin
    Add( DIVIDER2 );
    Add( DIVIDER3 );
    Add( DIVIDER5 );
    Add( DIVIDER6 );
    Add( DIVIDER8 );
  end;

  for u := low( TURLAction ) to high( TURLAction ) do begin
    Combo_URLAction.Items.Add( URL_ACTIONS[u] );
    Combo_URLCtrlAction.Items.Add( URL_ACTIONS[u] );
  end;
  Combo_URLAction.ItemIndex := 0;
  Combo_URLCtrlAction.ItemIndex := 0;

    InitializeKeyOptions( myOpts );
    InitializeTabOptions( myTabOpts );
    InitializeClipOptions( myClipOpts );
    InitializeTreeOptions( myTreeOpts );
    InitializeEditorOptions( myEditorOptions );
    InitializeTreeOptions( myTreeOptions );

  // Combo_ICN.ImageList := Chest.IMG_Categories;
  List_ICN.ImageList := Chest.IMG_Categories;

  List_ICN.Items.BeginUpdate;
  try
    LoadIcons;
    if ( List_ICN.Items.Count > 0 ) then
      List_ICN.ItemIndex := 0;
  finally
    List_ICN.Items.EndUpdate;
  end;
  Icons_Changed := false;
  Icons_RefList := TStringList.Create;
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Icons_RefList.Add( inttostr( i ));

  for i := 1 to MAX_BACKUP_LEVEL do
  begin
    Combo_BakLevel.Items.Add( inttostr( i ));
  end;
  Combo_BakLevel.ItemIndex := 0;

  // Combo_ICN.ItemIndex := 0;

  for txm := low( txm ) to high( txm ) do
    Combo_ExpandMode.Items.Add( TREE_EXPAND_MODES[txm] );
  Combo_ExpandMode.ItemIndex := 0;

  for cln := low( cln ) to high( cln ) do
    Combo_ClipNodeNaming.Items.Add( CLIP_NODE_NAMINGS[cln] );
  Combo_ClipNodeNaming.ItemIndex := 0;

  for clpt := low( clpt ) to high( clpt ) do
    Combo_PlainTextMode.Items.Add( CLIP_PLAIN_TEXT_MODE[clpt] );
  Combo_PlainTextMode.ItemIndex := 0;

  for tp := low( tp ) to high( tp ) do
    Combo_TabPos.Items.Add( TAB_POSITIONS[tp] );
  Combo_TabPos.ItemIndex := 0;

  Icon_Change_Canceled := false;
  Icons_Change_Disable := false;


  // Images

  for var j : TImageFormatFromClipb := low( TImageFormatFromClipb ) to high( TImageFormatFromClipb ) do
     CbImgDefaultFormatFromClipb.Items.Add( IMAGE_FORMATS_FROM_CLIPB[j].ToUpper );
  CbImgDefaultFormatFromClipb.ItemIndex := 0;

  for var j : TZipCompressionSelec := low( TZipCompressionSelec ) to high( TZipCompressionSelec ) do
     CbImgDefaultCompression.Items.Add( ZIP_COMPRESSION_SELEC[j] );
  CbImgDefaultCompression.ItemIndex := 0;

  for var j : TPixelFormatSelec := low( TPixelFormatSelec ) to high( TPixelFormatSelec ) do
     CbImgBmpPixelFormat.Items.Add( PIXEL_FORMAT_SELEC[j] );
  CbImgBmpPixelFormat.ItemIndex := 1;

  for var j : TImagesStorageMode := low( TImagesStorageMode ) to high( TImagesStorageMode ) do
     CbImgDefaultStorageMode.Items.Add( IMAGES_STORAGE_MODE[j] );
  CbImgDefaultStorageMode.ItemIndex := 1;

  for var j : TImagesExternalStorage := low( TImagesExternalStorage ) to high( TImagesExternalStorage ) do
     cbImgDefaultExternalStorage.Items.Add( EXTERNAL_STORAGE_TYPE[j] );
  cbImgDefaultExternalStorage.ItemIndex := 1;

  for var j : TImagesStorageModeOnExport := low( TImagesStorageModeOnExport ) to high( TImagesStorageModeOnExport ) do
     CbImgStorageModeOnExport.Items.Add( IMAGES_STORAGE_MODE_ON_EXPORT[j] );
  CbImgStorageModeOnExport.ItemIndex := 1;

  for var j : TCtrlUpDownMode := low( TCtrlUpDownMode ) to high( TCtrlUpDownMode ) do
     cbCtrlUpDownMode.Items.Add( CTRL_UP_DOWN_MODE[j] );
  cbCtrlUpDownMode.ItemIndex := 1;


  var Item : TListItem;
  var CaseSens: string;
  var Disposable: string;
  var UseOnExpand: string;

  FoldingBlocks_Changed:= False;
  LVfb.Items.BeginUpdate;
  try
     for i := 0 to Length(FoldBlocks) -1 do begin
        Item := LVfb.Items.Add;
        Item.caption := FoldBlocks[i].Opening;
        Item.subitems.add( FoldBlocks[i].Closing);
        CaseSens:= '';
        if FoldBlocks[i].CaseSensitive then
           CaseSens:= 'x';
        Item.subitems.add(CaseSens);
        Disposable:= '';
        if FoldBlocks[i].Disposable then
           Disposable:= 'x';
        Item.subitems.add(Disposable);
        UseOnExpand:= '';
        if FoldBlocks[i].UseOnExpand then
           UseOnExpand:= 'x';
        Item.subitems.add(UseOnExpand);
     end;
  finally
    LVfb.Items.EndUpdate;
  end;


  CreateMenu;
end; // CREATE

procedure TForm_OptionsNew.CreateMenu;
var
  i: integer;
  ParentNode: PVirtualNode;
begin
{
 General Settings [238]          0
   Rich Text Editor [239]
   Images [573]
   Tree Panel [240]
 KeyNote Files [241]             4
   File Options [242]
   Backup Options [243]
 Actions [244]                   7
   Confirmations [245]
 Chrome [246]                    9
   Tab Icons [247]
 Advanced [248]                 11
   Formats [249]
   Clipboard [250]
   File Types [251]
   Folding Blocks [253]
   Other [252]
}

  TV.TreeOptions.MiscOptions:= [];
  TV.BeginUpdate;

  fOptions[0].Caption:= GetRS(sOptS00);
  fOptions[1].Caption:= GetRS(sOptS01);
  fOptions[2].Caption:= GetRS(sOptS02);
  fOptions[3].Caption:= GetRS(sOptS03);
  fOptions[4].Caption:= GetRS(sOptS04);
  fOptions[5].Caption:= GetRS(sOptS05);
  fOptions[6].Caption:= GetRS(sOptS06);
  fOptions[7].Caption:= GetRS(sOptS07);
  fOptions[8].Caption:= GetRS(sOptS08);
  fOptions[9].Caption:= GetRS(sOptS09);
  fOptions[10].Caption:= GetRS(sOptS10);
  fOptions[11].Caption:= GetRS(sOptS11);
  fOptions[12].Caption:= GetRS(sOptS12);
  fOptions[13].Caption:= GetRS(sOptS13);
  fOptions[14].Caption:= GetRS(sOptS14);
  fOptions[15].Caption:= GetRS(sOptS16);
  fOptions[16].Caption:= GetRS(sOptS15);


  fOptions[0].Parent:= True;
  fOptions[4].Parent:= True;
  fOptions[7].Parent:= True;
  fOptions[9].Parent:= True;
  fOptions[11].Parent:= True;

  for i:= 0 to high(fOptions) do begin
      if fOptions[i].Parent then
         ParentNode:= TV.AddChild(nil, @fOptions[i])
      else
         TV.AddChild(ParentNode, @fOptions[i]);
  end;

  TV.TreeOptions.MiscOptions:= [TVTMiscOption.toReadOnly];
  TV.FullExpand;
  TV.EndUpdate;
end;



procedure TForm_OptionsNew.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;

  App.SetTopMost(Handle, True);

  Initializing := false;
  OptionsToForm;

  UpdateDateFmt;
  UpdateTimeFmt;
  UpdateFontSample;

  if Icons_Change_Disable then
  begin
    GroupBox_TabIcons.Caption := GetRS(sOpt01);
    GroupBox_TabIcons.Enabled := false;
    List_ICN.Enabled := false;
    Label_ICN.Enabled := false;
    Button_ICNAdd.Enabled := false;
    Button_ICNInsert.Enabled := false;
    Button_ICNDelete.Enabled := false;
    Button_ICNReset.Enabled := false;
  end;

  // Clipboard capture options and setup
  ClipCapToForm;

  Checkbox_LoadLastFileClick( CB_LoadLastFile );
  Checkbox_LoadUserFileClick( CB_LoadUserFile );

  CheckBox_AutoSaveOnTimerClick( CheckBox_AutoSaveOnTimer );
  Checkbox_AutoSaveClick( Checkbox_AutoSave );
  checkbox_BackupClick( Checkbox_Backup );

  CheckBox_TimerMinimizeClick( CheckBox_TimerMinimize );
  CheckBox_TimerCloseClick( CheckBox_TimerClose );

  CB_LoadLastFile.OnClick := Checkbox_LoadLastFileClick;
  CB_LoadUserFile.OnClick := Checkbox_LoadUserFileClick;
  Checkbox_Backup.OnClick := Checkbox_BackupClick;
  CB_BackupRegularIntervals.OnClick := Checkbox_BackupClick;
  CheckBox_MRUUse.OnClick := CheckBox_MRUUseClick;
  Checkbox_AutoSave.OnClick := Checkbox_AutoSaveClick;
  CheckBox_AutoSaveOnTimer.OnClick := CheckBox_AutoSaveOnTimerClick;
  CheckBox_ShowTooltips.OnClick := CheckBox_ShowTooltipsClick;
  Edit_DateFormat.OnChange := Edit_DateFormatChange;
  Edit_TimeFormat.OnChange := Edit_TimeFormatChange;
  CheckBox_HotkeyActivate.OnClick := CheckBox_HotkeyActivateClick;
  CheckBox_TimerClose.OnClick := CheckBox_TimerCloseClick;
  CheckBox_TimerMinimize.OnClick := CheckBox_TimerMinimizeClick;
  CheckBox_AutoRegisterFileType.OnClick := CheckBox_AutoRegisterFileTypeClick;
  RB_ActiveTab.OnClick := RB_ActiveTabClick;
  RB_InactiveTab.OnClick := RB_ActiveTabClick;
  BTN_Font.OnClick := BTN_FontClick;
  BTN_Color.OnClick := BTN_ColorClick;
  BTN_Defaults.OnClick := BTN_DefaultsClick;
  Button_ICNAdd.OnClick := Button_ICNAddClick;
  Button_ICNInsert.OnClick := Button_ICNInsertClick;
  Button_ICNDelete.OnClick := Button_ICNDeleteClick;
  Button_ICNReset.OnClick := Button_ICNResetClick;
  BitBtn_TknHlp.OnClick := BitBtn_TknHlpClick;
  BitBtn_TknHlp2.OnClick := BitBtn_TknHlpClick;
  Combo_Size.OnKeyPress := Combo_SizeKeyPress;

  if (LVfb.Items.Count > 0 ) then begin
     LVfb.Selected := LVfb.Items[0];
     LVfb.ItemFocused := LVfb.Selected;
  end;

  TV.FocusedNode:= TV.GetFirst();


end; // ACTIVATE

procedure TForm_OptionsNew.ClipCapToForm;

   procedure DividerComboToForm(combo: TComboBox; const divider: string);
   begin
     if ( divider = CLIPDIVCHAR ) then
       combo.Text := DIV_1_BLANK
     else
        if ( divider = ( CLIPDIVCHAR + CLIPDIVCHAR )) then
          combo.Text := DIV_2_BLANK
        else
          combo.Text := divider;
   end;

begin
  // CheckBox_URLOnly.Checked := myClipOpts.URLOnly;
  Combo_Size.Items.Insert( 0, inttostr( myClipOpts.MaxSize ));
  Combo_Size.Text := inttostr( myClipOpts.MaxSize );
  DividerComboToForm(Combo_Divider, myClipOpts.Divider);
  DividerComboToForm(Combo_WCDivider, myClipOpts.WCDivider);
  CB_IgnoreSelf.Checked := myClipOpts.IgnoreSelf;
  CB_TestDupClips.Checked := myClipOpts.TestDupClips;
  CB_SwitchIcon.Checked := myClipOpts.SwitchIcon;
  CB_AsText.Checked := myClipOpts.PasteAsText;
  CB_SourceURL.Checked := myClipOpts.InsertSourceURL;
  CheckBox_ClipRecall.Checked := myClipOpts.Recall;
  CheckBox_Sound.Checked := myClipOpts.PlaySound;
  CB_TreeClipConfirm.Checked := myClipOpts.TreeClipConfirm;
  if myClipOpts.PasteAsNewNode then
    RB_ClipTreeNew.Checked := true
  else
    RB_ClipTreeActive.Checked := true;
  Combo_ClipNodeNaming.ItemIndex := ord( myClipOpts.ClipNodeNaming );
  Combo_PlainTextMode.ItemIndex:= ord( myClipOpts.PlainTextMode );

  RB_ClipTreeActiveClick( RB_ClipTreeActive );
  RB_ClipTreeActive.OnClick := RB_ClipTreeActiveClick;
  RB_ClipTreeNew.OnClick := RB_ClipTreeActiveClick;
  CB_AsText.OnClick:= CB_AsTextClick;

  CB_AsTextClick(nil);

end; // ClipCapToForm

function TForm_OptionsNew.FormToClipCap : boolean;

   procedure DividerComboToOption(combo: TComboBox; var divider: string);
   begin
      divider := combo.Text;
      if ( divider = DIV_1_BLANK ) then
         divider := CLIPDIVCHAR
      else
         if ( divider = DIV_2_BLANK ) then
            divider := CLIPDIVCHAR + CLIPDIVCHAR;
   end;


var
  Divider, WCDivider: string;

begin
    result := true;

    DividerComboToOption(Combo_Divider, Divider);
    DividerComboToOption(Combo_WCDivider, WCDivider);
    if (Divider <> myClipOpts.Divider) or (WCDivider <> myClipOpts.WCDivider) then
       ClipCapMng.CleanCacheURLs (true);

    myClipOpts.Divider  := Divider;
    myClipOpts.WCDivider:= WCDivider;

    // myClipOpts.URLOnly := CheckBox_URLOnly.Checked;
    myClipOpts.IgnoreSelf := CB_IgnoreSelf.Checked;
    myClipOpts.TestDupClips := CB_TestDupClips.Checked;
    myClipOpts.SwitchIcon := CB_SwitchIcon.Checked;
    myClipOpts.PasteAsText := CB_AsText.Checked;
    myClipOpts.InsertSourceURL := CB_SourceURL.Checked;
    myClipOpts.Recall := CheckBox_ClipRecall.Checked;
    myClipOpts.PlaySound := CheckBox_Sound.Checked;
    myClipOpts.TreeClipConfirm := CB_TreeClipConfirm.Checked;
    myClipOpts.PasteAsNewNode := RB_ClipTreeNew.Checked;

    myClipOpts.ClipNodeNaming := TClipNodeNaming( Combo_ClipNodeNaming.ItemIndex );
    myClipOpts.PlainTextMode  := TClipPlainTextMode( Combo_PlainTextMode.ItemIndex );
    try
      myClipOpts.MaxSize := strtoint( Combo_Size.Text );
    except
      Combo_Size.SetFocus;
      App.ErrorPopup(GetRS(sOpt02));
      result := false;
    end;
end; // FormToClipCap


procedure TForm_OptionsNew.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
  begin
    FormToOptions;
    CanClose := FormToClipCap; // because it may fail verification
  end;
end; // CLOSEQUERY

procedure TForm_OptionsNew.FormDestroy(Sender: TObject);
begin
  try
    Icons_RefList.Free;
  except
  end;
end;

function TForm_OptionsNew.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// DESTROY

procedure TForm_OptionsNew.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_ESCAPE : if ( not (
      Combo_EscapeAction.DroppedDown or
      Combo_Divider.DroppedDown or
      Combo_WCDivider.DroppedDown or
      Combo_URLAction.DroppedDown or
      Combo_URLCtrlAction.DroppedDown or
      Combo_BakLevel.DroppedDown or
      Combo_TabPos.DroppedDown
      )) then
    begin
      if ( shift = [] ) then
      begin
        key := 0;
        OK_Click := false;
        Icon_Change_Canceled := Icons_Changed;
        ModalResult := mrCancel;
      end
      else
      if ( shift = [ssShift] ) then
      begin
        TV.SetFocus;
      end;
    end;
  end;
end; // KEY DOWN

procedure TForm_OptionsNew.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_OptionsNew.Button_CancelClick(Sender: TObject);
begin
  Icon_Change_Canceled := Icons_Changed;
  OK_Click := false;
end;

procedure TForm_OptionsNew.FormToOptions;
var
  i : integer;
  s : string;
begin
  with myOpts do
  begin
    AutoRegisterFileType := CheckBox_AutoRegisterFileType.Checked;
    AutoRegisterPrompt := CheckBox_AutoRegisterPrompt.Checked;
    AutoSave := CheckBox_AutoSave.Checked;
    AutoSaveOnFocus := CheckBox_AutoSaveOnFocus.Checked;
    AutoSaveOnTimer := CheckBox_AutoSaveOnTimer.Checked;

    BackupRegularIntervals := CB_BackupRegularIntervals.Checked;
    Backup := checkbox_Backup.Checked;
    BackupVNodes := CB_BackupVNodes.Checked;
    BackupAppendExt := CheckBox_BackupAppendExt.Checked;
    BackupExt := ansilowercase( trim( Edit_BackupExt.Text ));
    if (( BackupExt = '' ) or ( BackupExt = ext_KeyNote ) or
        ( BackupExt = ext_Encrypted ) or ( BackupExt = ext_DART )) then
      BackupExt := ext_BAK;
    if ( BackupExt[1] <> '.' ) then
      BackupExt := '.' + BackupExt;
    if RB_BakOriginalDir.Checked then
    begin
      BackupDir := '';
    end
    else
    begin
      s := ProperFolderName( trim( Edit_BakDir.text ));
      if (( s <> '' ) and System.Sysutils.Directoryexists( s )) then
        BackupDir := s
      else
        BackupDir := '';
    end;
    BackupLevel := succ( Combo_BakLevel.ItemIndex );

    AutoPasteEval := CheckBox_AutoPasteEval.Checked;
    AutoPastePlugin := CheckBox_AutoPastePlugin.Checked;

    //DropNodesOnTabMove := CB_DropNodesOnTabMove.Checked;
    DropNodesOnTabPrompt := CB_DropNodesOnTabPrompt.CHecked;

    SkipNewFilePrompt := CB_SkipNewFilePrompt.Checked;

    OpenFloppyReadOnly := CheckBox_OpenFloppyReadOnly.Checked;
    OpenNetworkReadOnly := CheckBox_OpenNetworkReadOnly.Checked;
    OpenReadOnlyWarn := CheckBox_OpenReadOnlyWarn.Checked;
    TimerMinimize := CheckBox_TimerMinimize.Checked;
    TimerMinimizeInt := Spin_TimerMinInt.Value;
    TimerClose := CheckBox_TimerClose.Checked;
    TimerCloseInt := Spin_TimerCloseInt.Value;
    TimerClose := Spin_TimerCloseInt.Enabled;
    TimerCloseEncOnly := CB_CloseEncOnly.Checked;
    TimerCloseDialogs := CB_TimerCloseDialogs.Checked;
    TimerCloseAutoReopen := CB_TimerCloseAutoReopen.Checked;
    EncFileAltExt := CheckBox_EncFileAltExt.Checked;
    URLAction := TURLAction( Combo_URLAction.ItemIndex );
    URLCtrlAction := TURLAction( Combo_URLCtrlAction.ItemIndex );
    //URLCLickShift := CheckBox_URLShift.Checked;
    MinimizeOnURL := CheckBox_MinOnURL.Checked;
    URLSystemBrowser := RB_URLSystemBrowser.Checked;
    URLAltBrowserPath := NormalFN( Edit_URLAltBrowserPath.Text );

    DisableFileMon := CheckBox_DisableFileMon.Checked;
    ShowFonts := CheckBox_ShowFonts.Checked;
    FixScrollBars := CheckBox_FixScrollBars.Checked;
    LongCombos := CheckBox_LongCombos.Checked;
    RichEditv3 := CheckBox_RichEditv3.Checked;

    ConfirmTreePaste := CheckBox_ConfirmTreePaste.Checked;
    NoRegistry := CheckBox_NoRegistry.Checked;
    URLFileAuto := CheckBox_URLFileAuto.Checked;
    ExtKNTLnkInNewInst := CB_ExtKNTLnkInNewInst.Checked;

    HotKeyWarn := CheckBox_HotKeyWarn.Checked;
    UseOldColorDlg := CheckBox_UseOldColorDlg.Checked;
    RunAutoMacros := CheckBox_RunAutoMacros.Checked;
    //SafePrint := CheckBox_SafePrint.Checked;           { OBSOLETE, unused }
    IgnoreUpgrades := CB_IgnoreUpgrades.Checked;
    ResPanelActiveUpdate := CB_ResPanelActiveUpdate.Checked;
    // UseNewStyleURL := CB_UseNewStyleURL.Checked;

    ExtText := '';
    for i := 1 to List_TxtExt.Items.Count do
    begin
      ExtText := ExtText + '.' + List_TxtExt.Items[pred( i )];
    end;
    ExtText := ExtText + '.';


    //InsCharKeepFont := ( not CheckBox_InsCharKeepFont.Checked );
    InsCharWinClose := CheckBox_InsCharWinClose.Checked;

    AutoSaveOnTimerInt := Spin_AutoSaveOnTimerInt.Value;
    ConfirmExit := CheckBox_ConfirmExit.Checked;
    ConfirmTabDelete := Checkbox_ConfirmDelete.Checked;
    EscAction := Combo_EscapeAction.ItemIndex;
    if ( EscAction < 0 ) then EscAction := 0;
    UseTray := checkbox_IconInTray.Checked;
    // ShowSplash := CheckBox_SplashScreen.Checked;
    LoadLastFile := CB_LoadLastFile.Checked;
    LoadUserFile := CB_LoadUserFile.Checked;
    AutoNewFile := CB_AutoNewFile.Checked;
    DateFmt := Edit_DateFormat.Text;
    MinimizeOnClose := CheckBox_MinimizeOnClose.Checked;
    MRUCount := Spin_MRUCount.Value;
    MRUSubmenu := CheckBox_MRUSubmenu.Checked;
    MRUFullPaths := CheckBox_MRUFullPath.Checked;
    MRUUse := CheckBox_MRUUse.Checked;
    ShowTooltips := CheckBox_ShowTooltips.Checked;
    SingleInstance := CheckBox_SingleInstance.Checked;
    StartMinimized := CheckBox_StartMinimized.Checked;
    TimeFmt := Edit_TimeFormat.Text;
    DTUseLastSelection := CB_DTUseLastSelection.Checked;
    // ToolbarDrag;
    // ToolbarFlat;
    UserFile := normalFN( Edit_UserFile.text );
    HotKey := Edit_HotKey.HotKey;
    HotkeyActivate := ( CheckBox_HotkeyActivate.Checked and ( HotKey > 0 ));

    LanguageUI := Combo_Language.Items[Combo_Language.ItemIndex];


    case TZipCompressionSelec(cbImgDefaultCompression.ItemIndex) of
     zcsStored:    ImgDefaultCompression:= zcStored;
     zcsDeflate:   ImgDefaultCompression:= zcDeflate;
     zcsDeflate64: ImgDefaultCompression:= zcDeflate64;
    end;

    case TPixelFormatSelec(cbImgBmpPixelFormat.ItemIndex) of
     pfs15bit: ImgBmpPixelFormat:= pf15bit;
     pfs24bit: ImgBmpPixelFormat:= pf24bit;
     pfs32bit: ImgBmpPixelFormat:= pf32bit;
    end;

    case TImageFormatFromClipb(cbImgDefaultFormatFromClipb.ItemIndex) of
     imcPng: ImgDefaultFormatFromClipb:= imgPng;
     imcJpg: ImgDefaultFormatFromClipb:= imgJpg;
    end;

    ImgDefaultStorageMode:=     TImagesStorageMode(cbImgDefaultStorageMode.ItemIndex);
    ImgDefaultExternalStorage:= TImagesExternalStorage(cbImgDefaultExternalStorage.ItemIndex);
    ImgStorageModeOnExport:=    TImagesStorageModeOnExport(cbImgStorageModeOnExport.ItemIndex);
    ImgMaxAutoWidthGoal:=       StrToIntDef(TxtImgMaxAutoWidthGoal.Text, 0);
    ImgSaveInSubfolders:=       chkImgSaveInSubfolders.Checked;
    ImgDefaultLinkMode:=        chkImgDefaultLinkMode.Checked;
    ImgLinkRelativePath:=       chkImgLinkRelativePath.Checked;
    ImgUseRecycleBin :=         chkImgUseRecycleBin.Checked;
    ImgRatioSizePngVsJPG:=      StrToFloatDef( txtImgRatioSizePngVsJPG.Text, 0.0);
    ImgCompressionQuality:=     StrToIntDef( txtImgCompressionQuality.Text, KeyOptions.ImgCompressionQuality);
    if ImgSingleViewerInstance <> chkImgSingleViewerInstance.Checked then begin
       ImgSingleViewerInstance:=  chkImgSingleViewerInstance.Checked;
       ClearImgViewerInstances;
    end;
    ImgHotTrackViewer:= chkImgHotTrackViewer.Checked;
    ImgKeepOrigName:= chkImgKeepOrigName.Checked;
    if not ImgHotTrackViewer then
       App.ShowingImageOnTrack:= false;
  end;

  with myTabOpts do
  begin
    // ActiveColor : TColor;
    // InactiveColor : TColor;
    // Font : TFontProperties;
    HotTrack := CheckBox_TabsHotTrack.Checked;
    Images := CheckBox_TabsImages.Checked;
    Stacked := CheckBox_TabsStacked.Checked;
    TabOrientation := TTabOrientation( Combo_TabPos.ItemIndex );
    // TabsAreButtons := ( CheckBox_TabsTabsAreButtons.Checked and TabsOnTop );
  end;

  with myFindOpts do
  begin
    AutoClose := CB_FindAutoClose.Checked;
    WordAtCursor := CB_WordAtCursor.Checked;
    ResetNextAftN := Spin_ResetNextAftN.Value;
  end;
  ResPanelOptions.FontSizeFindResults:= Spin_FontSizeFindResults.Value;

  with myEditorOptions do
  begin
    SaveCaretPos := CB_SaveCaretPos.Checked;
    WordCountTrack := CB_TrackWordCount.Checked;
    AutoIndent := CheckBox_AutoIndent.Checked;
    TrackCaretPos := CheckBox_TrackCaretPos.Checked;
    UndoLimit := Spin_UndoLimit.Value;
    IndentInc := Spin_IndentInc.Value;
    WordSelect := CheckBox_WordSelect.Checked;
    FontSizeInc := Spin_FontSizeInc.Value;
    ParaSpaceInc := Spin_ParaSpaceInc.Value;
    AutoFont := CB_AutoFont.Checked;
    AutoKeyboard := CB_AutoKeyboard.Checked;
    PlainDefaultPaste := CB_PlainDefaultPaste.Checked;
    CtrlUpDownMode:= TCtrlUpDownMode(cbCtrlUpDownMode.ItemIndex);
    BulletSepFactor:= StrToFloatDef( txtSepInLists.Text, 5.7);
    LineWidthEditor:= CB_LineEdWidth.Checked;
  end;

  with myTreeOptions do
  begin
    EditNewNodes := CheckBox_EditNewNodes.Checked;
    AutoScroll := CheckBox_AutoScroll.Checked;
    EditInPlace := CheckBox_EditInPlace.Checked;
    // AutoExpand := CheckBox_AutoExpandTree.Checked;
    // SaveActiveNode := CheckBox_SaveActiveNode.Checked;
    ConfirmNodeDelete := CheckBox_ConfirmNodeDelete.Checked;
    HotTrack := CheckBox_HotTrackTree.Checked;

    InheritNodeBG := CheckBox_InheritBGColor.Checked;
    InheritNodeProperties := CB_InheritNodeProperties.Checked;
    ShowTooltips := CheckBox_TreeTips.Checked;
    ShowFullPath := CB_ShowFullPath.Checked;
    ShowFullPathSearch := CB_ShowFullPathSearch.Checked;
    PathTopToBottom := ( not CB_PathTopToBottom.Checked );
    AutoNameVNodes := CheckBox_AutoNameVNodes.Checked;
    ConfirmNodeRefresh := CB_ConfirmNodeRefresh.Checked;
    ExpandMode := TTreeExpandMode( Combo_ExpandMode.ItemIndex );
  end;

end; // FormToOptions

procedure TForm_OptionsNew.OptionsToForm;
var
  i: integer;
begin

  with myOpts do
  begin
    CheckBox_AutoRegisterFileType.Checked := AutoRegisterFileType;
    CheckBox_AutoRegisterPrompt.Checked := AutoRegisterPrompt;
    CheckBox_AutoRegisterPrompt.Enabled := CheckBox_AutoRegisterFileType.Checked;
    CheckBox_AutoSave.Checked := AutoSave;
    CheckBox_AutoSaveOnFocus.Checked := AutoSaveOnFocus;
    CheckBox_AutoSaveOnTimer.Checked := AutoSaveOnTimer;
    Checkbox_Backup.Checked := Backup;
    CB_BackupRegularIntervals.Checked := BackupRegularIntervals;
    CB_BackupVNodes.Checked := BackupVNodes;
    CheckBox_BackupAppendExt.Checked := BackupAppendExt;
    Edit_BackupExt.Text := BackupExt;

    //Edit_BakDir.InitialDir := extractfilepath( application.exename );
    if ( BackupDir = '' ) then
    begin
      RB_BakOriginalDir.Checked := true;
    end
    else
    begin
      RB_BakUserDir.Checked := true;
      Edit_BakDir.Text := BackupDir;
    end;
    Combo_BakLevel.ItemIndex := pred( BackupLevel );

    if CheckBox_AutoSaveOnFocus.Checked then
       CheckBox_AutoSaveOnFocus.Font.Color:= clBlue;

    CheckBox_AutoPasteEval.Checked := AutoPasteEval;
    CheckBox_AutoPastePlugin.Checked := AutoPastePlugin;

    //CB_DropNodesOnTabMove.Checked := DropNodesOnTabMove;
    CB_DropNodesOnTabPrompt.Checked := DropNodesOnTabPrompt;

    CB_SkipNewFilePrompt.Checked := SkipNewFilePrompt;

    CheckBox_OpenFloppyReadOnly.Checked := OpenFloppyReadOnly;
    CheckBox_OpenNetworkReadOnly.Checked := OpenNetworkReadOnly;
    CheckBox_OpenReadOnlyWarn.Checked := OpenReadOnlyWarn;
    CheckBox_TimerMinimize.Checked := TimerMinimize;
    Spin_TimerMinInt.Value := TimerMinimizeInt;
    Spin_TimerMinInt.Enabled := TimerMinimize;
    CheckBox_TimerClose.Checked := TimerClose;
    Spin_TimerCloseInt.Value := TimerCloseInt;
    Spin_TimerCloseInt.Enabled := TimerClose;
    CB_CloseEncOnly.Checked := TimerCloseEncOnly;
    CB_CloseEncOnly.Enabled := TimerClose;
    CB_TimerCloseDialogs.Checked := TimerCloseDialogs;
    CB_TimerCloseAutoReopen.Checked := TimerCloseAutoReopen;
    CheckBox_EncFileAltExt.Checked := EncFileAltExt;
    Combo_URLAction.ItemIndex := ord( URLAction );
    Combo_URLCtrlAction.ItemIndex := ord( URLCtrlAction );
    //CheckBox_URLShift.Checked := URLCLickShift;
    CheckBox_MinOnURL.Checked := MinimizeOnURL;

    if URLSystemBrowser then
      RB_URLSystemBrowser.Checked := true
    else
      RB_URLAltBrowser.Checked := true;
    Edit_URLAltBrowserPath.Text := URLAltBrowserPath;

    CheckBox_DisableFileMon.Checked := DisableFileMon;
    CheckBox_ShowFonts.Checked := ShowFonts;
    CheckBox_FixScrollBars.Checked := FixScrollBars;
    CheckBox_LongCombos.Checked := LongCombos;
    CheckBox_RichEditv3.Checked := RichEditv3;
    CheckBox_ConfirmTreePaste.Checked := ConfirmTreePaste;
    CheckBox_NoRegistry.Checked := NoRegistry;
    CheckBox_URLFileAuto.Checked := URLFileAuto;
    CB_ExtKNTLnkInNewInst.Checked := ExtKNTLnkInNewInst;
    CheckBox_HotKeyWarn.Checked := HotKeyWarn;
    CheckBox_UseOldColorDlg.Checked := UseOldColorDlg;
    CheckBox_RunAutoMacros.Checked := RunAutoMacros;
    //CheckBox_SafePrint.Checked := SafePrint;              { OBSOLETE, unused }
    CB_IgnoreUpgrades.Checked := IgnoreUpgrades;
    CB_ResPanelActiveUpdate.Checked := ResPanelActiveUpdate;
    // CB_UseNewStyleURL.Checked := UseNewStyleURL;

    if ( exttext[1] = '.' ) then
      delete( exttext, 1, 1 );
    if ( exttext <> '' ) then
    begin
      if ( exttext[length( exttext )] = '.' ) then
        delete( exttext, length( exttext ), 1 );
    end;
    List_TxtExt.Items.BeginUpdate;
    try
      exttext := ansilowercase( exttext );
      CSVTextToStrs( List_TxtExt.Items, exttext, '.' );
      if ( List_TxtExt.Items.Count > 0 ) then
        List_TxtExt.ItemIndex := 0;
    finally
      List_TxtExt.Items.EndUpdate;
    end;

    //CheckBox_InsCharKeepFont.Checked := ( not InsCharKeepFont );
    CheckBox_InsCharWinClose.Checked := InsCharWinClose;

    Spin_AutoSaveOnTimerInt.Value := AutoSaveOnTimerInt;
    CheckBox_ConfirmExit.Checked := ConfirmExit;
    Checkbox_ConfirmDelete.Checked := ConfirmTabDelete;
    if ( EscAction < 0 ) or ( EscAction > ESC_QUIT ) then EscAction := 0;
    Combo_EscapeAction.ItemIndex := EscAction;
    Checkbox_IconInTray.Checked := UseTray;
    CB_LoadLastFile.Checked := ( LoadLastFile and ( not LoadUserFile ));
    CB_LoadUserFile.Checked := ( LoadUserFile and ( UserFile <> '' ));
    CB_AutoNewFile.Checked := AutoNewFile;
    // CheckBox_SplashScreen.Checked := ShowSplash;

    with Edit_DateFormat.Items do
    begin
      Add( DateFmt );
      Add( FormatSettings.LongDateFormat );
      Add( FormatSettings.ShortDateFormat );
    end;
    Edit_DateFormat.Text := DateFmt;

    with Edit_TimeFormat.Items do
    begin
      Add( TimeFmt );
      Add( FormatSettings.LongTimeFormat );
      Add( FormatSettings.SHortTimeFormat );
    end;
    Edit_TimeFormat.Text := TimeFmt;

    CB_DTUseLastSelection.Checked := DTUseLastSelection;

    CheckBox_MinimizeOnClose.Checked := MinimizeOnClose;
    Spin_MRUCount.Value := MRUCount;
    CheckBox_MRUSubmenu.Checked := MRUSubmenu;
    CheckBox_MRUFullPath.Checked := MRUFullPaths;
    CheckBox_MRUUse.Checked := MRUUse;
    CheckBox_ShowTooltips.Checked := ShowTooltips;
    CheckBox_SingleInstance.Checked := SingleInstance;
    CheckBox_StartMinimized.Checked := StartMinimized;

    CheckBox_IconInTray.Checked := UseTray;
    Edit_UserFile.Text := UserFile;
    Edit_HotKey.HotKey := HotKey;
    CheckBox_HotkeyActivate.Checked := ( HotkeyActivate and ( HotKey > 0 ));
    Edit_HotKey.Enabled := CheckBox_HotkeyActivate.Checked;

    Combo_Language.ItemIndex := 0;
    for i := 0 to Combo_Language.Items.Count -1 do begin
      if Combo_Language.Items[i] = LanguageUI then begin
         Combo_Language.ItemIndex := i;
         break;
      end;
    end;


    var ImgDefaultFormatFromClipbSelec: TImageFormatFromClipb;
    case ImgDefaultFormatFromClipb of
     imgPng: ImgDefaultFormatFromClipbSelec:= imcPng;
     imgJpg: ImgDefaultFormatFromClipbSelec:= imcJpg;
    end;

    var ImgDefaultCompressionSelec: TZipCompressionSelec;
    case ImgDefaultCompression of
     zcStored: ImgDefaultCompressionSelec:= zcsStored;
     zcDeflate: ImgDefaultCompressionSelec:= zcsDeflate;
     zcDeflate64: ImgDefaultCompressionSelec:= zcsDeflate64;
    end;

    var ImgBmpPixelFormatSelec: TPixelFormatSelec;
    case ImgBmpPixelFormat of
     pf15bit: ImgBmpPixelFormatSelec:= pfs15bit;
     pf24bit: ImgBmpPixelFormatSelec:= pfs24bit;
     pf32bit: ImgBmpPixelFormatSelec:= pfs32bit;
    end;

    CbImgDefaultFormatFromClipb.ItemIndex := Ord(ImgDefaultFormatFromClipbSelec);
    CbImgDefaultCompression.ItemIndex := Ord(ImgDefaultCompressionSelec);
    CbImgBmpPixelFormat.ItemIndex := Ord(ImgBmpPixelFormatSelec);
    CbImgDefaultStorageMode.ItemIndex := Ord(ImgDefaultStorageMode);
    cbImgDefaultExternalStorage.ItemIndex:= Ord(ImgDefaultExternalStorage);
    CbImgStorageModeOnExport.ItemIndex := Ord(ImgStorageModeOnExport);

    chkImgSaveInSubfolders.Checked:= ImgSaveInSubfolders;
    chkImgDefaultLinkMode.Checked:= ImgDefaultLinkMode;
    chkImgLinkRelativePath.Checked:=   ImgLinkRelativePath;
    chkImgUseRecycleBin.Checked:= ImgUseRecycleBin;
    txtImgMaxAutoWidthGoal.Text:= ImgMaxAutoWidthGoal.ToString;
    txtImgRatioSizePngVsJPG.Text:=  ImgRatioSizePngVsJPG.ToString(ffGeneral,3,2);
    txtImgCompressionQuality.Text:= ImgCompressionQuality.ToString;
    chkImgSingleViewerInstance.Checked:= ImgSingleViewerInstance;
    chkImgHotTrackViewer.Checked:= ImgHotTrackViewer;
    chkImgHotTrackViewer.Enabled:= ImgSingleViewerInstance;
    chkImgKeepOrigName.Checked:= ImgKeepOrigName;
  end;

  with myTabOpts do
  begin
    // ActiveColor : TColor;
    // InactiveColor : TColor;
    // Font : TFontProperties;
    CheckBox_TabsHotTrack.Checked := HotTrack;
    CheckBox_TabsImages.Checked := Images;
    CheckBox_TabsStacked.Checked := Stacked;
    Combo_TabPos.ItemIndex := ord( TabOrientation );
    // CheckBox_TabsTabsAreButtons.Checked := ( TabsAreButtons and TabsOnTop );
  end;

  with myFindOpts do
  begin
    CB_FindAutoClose.Checked := AutoClose;
    CB_WordAtCursor.Checked := WordAtCursor;
    Spin_ResetNextAftN.Value := ResetNextAftN;
  end;
  Spin_FontSizeFindResults.Value:= ResPanelOptions.FontSizeFindResults;

  with myEditorOptions do
  begin
    CB_SaveCaretPos.Checked := SaveCaretPos;
    CB_TrackWordCount.Checked := WordCountTrack;
    CheckBox_AutoIndent.Checked := AutoIndent;
    CheckBox_TrackCaretPos.Checked := TrackCaretPos;
    Spin_UndoLimit.Value := UndoLimit;
    Spin_IndentInc.Value := IndentInc;
    CheckBox_WordSelect.Checked := WordSelect;
    Spin_FontSizeInc.Value := FontSizeInc;
    Spin_ParaSpaceInc.Value := ParaSpaceInc;
    CB_AutoFont.Checked := AutoFont;
    CB_AutoKeyboard.Checked := AutoKeyboard;
    CB_PlainDefaultPaste.Checked := PlainDefaultPaste;
    cbCtrlUpDownMode.ItemIndex:= Ord(CtrlUpDownMode);
    txtSepInLists.Text:=  BulletSepFactor.ToString(ffGeneral,3,2);
    CB_LineEdWidth.Checked:= LineWidthEditor;
  end;

  with myTreeOptions do
  begin
    CheckBox_EditNewNodes.Checked := EditNewNodes;
    CheckBox_AutoScroll.Checked := AutoScroll;
    CheckBox_EditInPlace.Checked := EditInPlace;
    // CheckBox_AutoExpandTree.Checked := AutoExpand;
    // CheckBox_SaveActiveNode.Checked := SaveActiveNode;
    CheckBox_ConfirmNodeDelete.Checked := ConfirmNodeDelete;
    CheckBox_HotTrackTree.Checked := HotTrack;
    CheckBox_AutoNameVNodes.Checked := AutoNameVNodes;
    CheckBox_TreeTips.Checked := ShowTooltips;
    CB_ShowFullPathSearch.Checked := ShowFullPathSearch;
    CB_ShowFullPath.Checked := ShowFullPath;
    CB_PathTopToBottom.Checked := ( not PathTopToBottom );
    CheckBox_InheritBGColor.Checked := InheritNodeBG;
    CB_InheritNodeProperties.Checked := InheritNodeProperties;
    CB_ConfirmNodeRefresh.Checked := ConfirmNodeRefresh;
    Combo_ExpandMode.ItemIndex := ord( ExpandMode );
  end;

  CB_ShowFullPathClick(nil);

end; // OptionsToForm

procedure TForm_OptionsNew.Checkbox_LoadLastFileClick(Sender: TObject);
begin
  if CB_LoadLastFile.Checked then
  begin
    CB_LoadUserFile.Checked := false;
    Edit_UserFile.Text := myOpts.LastFile;
  end;
end;

procedure TForm_OptionsNew.Checkbox_LoadUserFileClick(Sender: TObject);
begin
  Edit_UserFile.Enabled := CB_LoadUserFile.Checked;
  TB_OpenDlgUserFile.Enabled:= Edit_UserFile.Enabled;
  if CB_LoadUserFile.Checked then
  begin
    CB_LoadLastFile.Checked := false;
    if ( Pages.ActivePage = 'PG_KNTFiles' ) then
    begin
      if ( Edit_UserFile.Text = '' ) then
        Edit_UserFile.Text := myOpts.UserFile;
      Edit_UserFile.SetFocus;
    end;
  end;
end;

procedure TForm_OptionsNew.CheckBox_MRUUseClick(Sender: TObject);
begin
  Spin_MRUCount.Enabled := CheckBox_MRUUse.Checked;
  CheckBox_MRUSubmenu.Enabled := CheckBox_MRUUse.Checked;
  CheckBox_MRUFullPath.Enabled := CheckBox_MRUUse.Checked;
  if ( CheckBox_MRUUse.Checked and ( Pages.ActivePage = 'PG_FileOptions' )) then
    Spin_MRUCount.SetFocus;
end;

procedure TForm_OptionsNew.Checkbox_AutoSaveClick(Sender: TObject);
begin
  CheckBox_AutoSaveOnFocus.Enabled := Checkbox_AutoSave.Checked;
  CheckBox_AutoSaveOnTimer.Enabled := Checkbox_AutoSave.Checked;
  Spin_AutoSaveOnTimerInt.Enabled := ( Checkbox_AutoSave.Checked and CheckBox_AutoSaveOnTimer.Checked );
  Label_Minutes.Enabled := Checkbox_AutoSave.Checked;
end;

procedure TForm_OptionsNew.CheckBox_AutoSaveOnFocusClick(Sender: TObject);
var
   color: TColor;
begin
    if CheckBox_AutoSaveOnFocus.Checked then
       color:= clBlue
    else
       color:= clWindowText;
    CheckBox_AutoSaveOnFocus.Font.Color:= color;
end;

procedure TForm_OptionsNew.CheckBox_AutoSaveOnTimerClick(Sender: TObject);
begin
  Spin_AutoSaveOnTimerInt.Enabled := CheckBox_AutoSaveOnTimer.Checked;
  if ( CheckBox_AutoSaveOnTimer.Checked and ( Pages.ActivePage = 'PG_KNTFiles' )) then
    Spin_AutoSaveOnTimerInt.SetFocus;
end;

procedure TForm_OptionsNew.UpdateDateFmt;
begin
  try
    Label_SampleDate.Caption := GetDateTimeFormatted( Edit_DateFormat.Text, now );
  except
    Label_SampleDate.Caption := GetRS(sOpt03);
  end;
end; // UpdateDateFmt

procedure TForm_OptionsNew.UpdateTimeFmt;
begin
  try
    Label_SampleTime.Caption := GetDateTimeFormatted( Edit_TimeFormat.Text, now );
  except
    Label_SampleTime.Caption := GetRS(sOpt04);
  end;
end; // UpdateTimeFmt

procedure TForm_OptionsNew.Edit_DateFormatChange(Sender: TObject);
begin
  UpdateDateFmt;
end;

procedure TForm_OptionsNew.Edit_TimeFormatChange(Sender: TObject);
begin
  UpdateTimeFmt;
end;

procedure TForm_OptionsNew.CheckBox_ShowTooltipsClick(Sender: TObject);
begin
  ShowHint := CheckBox_ShowTooltips.Checked;
end;

procedure TForm_OptionsNew.BTN_DefaultsClick(Sender: TObject);
begin
  ResetChromeDefaults;
end;

procedure TForm_OptionsNew.ResetChromeDefaults;
begin
  if ( App.DoMessageBox( GetRS(sOpt05), mtConfirmation, [mbYes,mbNo], Def2 ) <> mrYes ) then exit;

  with myTabOpts.Font do
  begin
    FSize := 9;
    FColor := clWindowText;
    FName := 'Tahoma';
    FCharset := DEFAULT_CHARSET;
    FStyle := [];
  end;
  myTabOpts.ActiveColor := _GF_CLWINDOW;
  myTabOpts.InactiveColor := clBtnFace;
  UpdateFontSample;

end; // ResetChromeDefaults

procedure TForm_OptionsNew.BTN_FontClick(Sender: TObject);
var
  dpi: integer;
begin
  if RB_ActiveTab.Checked then
  begin
    dpi:= GetSystemPixelsPerInch;
    FontPropertiesToFont( myTabOpts.Font, FontDlg.Font, dpi);
    if FontDlg.Execute then
    begin
      FontToFontProperties( FontDlg.Font, myTabOpts.Font, dpi);
      UpdateFontSample;
    end;
  end;
end;

procedure TForm_OptionsNew.UpdateFontSample;
begin
  FontPropertiesToFont( myTabOpts.Font, Edit_Sample.Font );
  Edit_Sample.Text := Format( '%s %d pt %s', [myTabOpts.Font.FName, myTabOpts.Font.FSize, FontStyleToStr( myTabOpts.Font.FStyle )] );

  if RB_ActiveTab.Checked then
    Edit_Sample.Color := myTabOpts.ActiveColor
  else
    Edit_Sample.Color := myTabOpts.InactiveColor;
end; // UpdateFontSample

procedure TForm_OptionsNew.RB_ActiveTabClick(Sender: TObject);
begin
  BTN_Font.Enabled := RB_ActiveTab.Checked;
  if RB_ActiveTab.Checked then
    Edit_Sample.Color := myTabOpts.ActiveColor
  else
    Edit_Sample.Color := myTabOpts.InactiveColor;
end;

procedure TForm_OptionsNew.BTN_ColorClick(Sender: TObject);
begin
  if RB_ActiveTab.Checked then
  begin
    ColorDlg.Color := myTabOpts.ActiveColor;
    if ColorDlg.Execute then
      myTabOpts.ActiveColor := ColorDlg.Color;
  end
  else
  begin
    ColorDlg.Color := myTabOpts.InactiveColor;
    if ColorDlg.Execute then
      myTabOpts.InactiveColor := ColorDlg.Color;
  end;
  UpdateFontSample;
end;


procedure TForm_OptionsNew.Button_ICNAddClick(Sender: TObject);
begin
  SetIcon( true );
end;

procedure TForm_OptionsNew.Button_ICNInsertClick(Sender: TObject);
begin
  SetIcon( false );
end;

procedure TForm_OptionsNew.Button_ICNDeleteClick(Sender: TObject);
begin
  DeleteIcon;
end;

procedure TForm_OptionsNew.Button_ICNResetClick(Sender: TObject);
begin
  ResetIcons;
end;


procedure TForm_OptionsNew.LoadIcons;
var
  i : integer;
begin
  List_ICN.Clear;
  if ( Chest.IMG_Categories.Count < 0 ) then exit;
  List_ICN.Items.BeginUpdate;
  try
    for i := 0 to pred( Chest.IMG_Categories.Count ) do
    begin
      // List_ICN.AddItem( Format( ' icon %d ', [succ( i )]), i );
      List_ICN.AddItem( Format( GetRS(sOpt06), [succ( i )]), cbUnchecked, i );
    end;
  finally
    List_ICN.Items.EndUpdate;
  end;

  Label_ICN.Caption := Format( GetRS(sOpt07), [Chest.IMG_Categories.Count] );

end; // LoadIcons


procedure TForm_OptionsNew.SetIcon( const Adding : boolean );
var
  fn : string;
  i : integer;
  icon : TIcon;
  bmp : TBitmap;
  IconH : HIcon;
  iidx, icnt : integer;
begin

  if ( not IconDlg.Execute ) then exit;
  i := 0;

  icnt := IconDlg.Files.Count;
  if ( icnt = 0 ) then exit;

  for iidx := 1 to icnt do
  begin

    fn := IconDlg.Files[pred( iidx )];
    UnquoteString( fn );

    if ( extractfileext( fn ) = '.ico' ) then
    begin
      icon := ticon.Create;
      try
        try
          icon.LoadFromFile( fn );
          if Adding then
          begin
            i := Chest.IMG_Categories.AddIcon( icon );
            Icons_RefList.Add( '-1' );
          end
          else
          begin
            i := List_ICN.ItemIndex;
            Chest.IMG_Categories.InsertIcon( i, icon );
            Icons_RefList.Insert( i, '-1' );
          end;
        except
          on E : Exception do
          begin
            App.ErrorPopup(GetRS(sOpt08) + fn + #13 + E.Message);
            exit;
          end;
        end;
      finally
        icon.free;
      end;
    end
    else
    if ( extractfileext( fn ) = '.bmp' ) then
    begin
      bmp := TBitmap.Create;
      try
        try
          bmp.LoadFromFile( fn );
          if Adding then
          begin
            i := Chest.IMG_Categories.Add( bmp, nil );
            Icons_RefList.Add( '-1' );
          end
          else
          begin
            i := List_ICN.ItemIndex;
            Chest.IMG_Categories.Insert( i, bmp, nil );
            Icons_RefList.Insert( i, '-1' );
          end;
        except
          on E : Exception do
          begin
            App.ErrorPopup(Format(GetRS(sOpt09) + #13 + E.Message, [fn] ));
            exit;
          end;
        end;
      finally
        bmp.free;
      end;
    end
    else
    begin
      // assume exe or dll
      icon := TIcon.Create;
      try
        try
          IconH := ExtractIcon( self.Handle, PChar( fn ), 0 );
          Icon.Handle := IconH;
          if Adding then
          begin
            i := Chest.IMG_Categories.AddIcon( icon );
            Icons_RefList.Add( '-1' );
          end
          else
          begin
            i := List_ICN.ItemIndex;
            Chest.IMG_Categories.InsertIcon( i, icon );
            Icons_RefList.Insert( i, '-1' );
          end;
        except
          on E : Exception do
          begin
            App.ErrorPopup(GetRS(sOpt08) + fn + #13 + E.Message);
            exit;
          end;
        end;
      finally
        icon.free;
      end;
    end;


  end;
  // Icons (*.ico)|*.ico|Bitmaps (*.bmp)|*.bmp|Programs and libraries (*.exe, *.dll)|*.exe;*.dll

  LoadIcons;
  Icons_Changed := true;

  if Adding then
    List_ICN.ItemIndex := pred( List_ICN.Items.Count )
  else
    List_ICN.ItemIndex := i;

end; // SetIcon;


procedure TForm_OptionsNew.DeleteIcon;
var
  i : integer;
begin

  if ( Chest.IMG_Categories.Count < 2 ) then
  begin
    App.InfoPopup(GetRS(sOpt10));
    exit;
  end;

  if ( App.DoMessageBox( GetRS(sOpt11), mtCOnfirmation, [mbYes,mbNo], Def2) <> mrYes ) then exit;

  try
    i := List_ICN.ItemIndex;
    Chest.IMG_Categories.Delete( i );
    Icons_RefList.Delete( i );
  except
    on E : Exception do
    begin
      App.ErrorPopup(E.Message);
      exit;
    end;
  end;

  LoadIcons;

  if ( i >= Chest.IMG_Categories.Count ) then
    List_ICN.ItemIndex := pred( List_ICN.Items.Count )
  else
    List_ICN.ItemIndex := i;

  Icons_Changed := true;
end; // DeleteIcon

procedure TForm_OptionsNew.ResetIcons;
var
  i : integer;
begin
  if ( App.DoMessageBox(GetRS(sOpt13), mtCOnfirmation, [mbYes,mbNo], Def2 ) <> mrYes ) then exit;

  Icons_RefList.Clear;
  LoadCategoryBitmapsBuiltIn;
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Icons_RefList.Add( '-1' );
  Icons_Changed := true;
  LoadIcons;
  List_ICN.ItemIndex := 0;
end; // ResetIcons


procedure TForm_OptionsNew.Combo_SizeKeyPress(Sender: TObject; var Key: Char);
begin
  if ( not ( AnsiChar(Key) in [#8, #9, #13, #27, '0'..'9'] )) then
    Key := #0;
end;


procedure TForm_OptionsNew.CheckBox_AutoRegisterFileTypeClick(
  Sender: TObject);
begin
  CheckBox_AutoRegisterPrompt.Enabled := CheckBox_AutoRegisterFileType.Checked;
end;


procedure TForm_OptionsNew.BitBtn_TknHlpClick(Sender: TObject);
begin
  App.InfoPopup(Format(GetRS(sOpt14), [CLIPDATECHAR, CLIPTIMECHAR, CLIPDIVCHAR,
                              CLIPSOURCEDELIMITER, CLIPSOURCE, CLIPSOURCE_LIMITED, CLIPSOURCE_ONLY_URL, CLIPSOURCE_DOMAIN,
                              CLIPSECONDDIV]) );

end;

procedure TForm_OptionsNew.Checkbox_BackupClick(Sender: TObject);
var
  CyclicBackup, CyclicOrIntervalBackup: Boolean;
begin
  CyclicBackup:= Checkbox_Backup.Checked;
  CyclicOrIntervalBackup:= CyclicBackup or CB_BackupRegularIntervals.Checked;

  CheckBox_BackupAppendExt.Enabled := CyclicBackup;
  Edit_BackupExt.Enabled := CyclicBackup;
  RB_BakOriginalDir.Enabled := CyclicOrIntervalBackup;
  RB_BakUserDir.Enabled := CyclicOrIntervalBackup;
  Label_BakDir.Enabled := CyclicOrIntervalBackup;
  Label_MaxBak1.Enabled := CyclicBackup;
  Label_MaxBak2.Enabled := CyclicBackup;
  Combo_BakLevel.Enabled := CyclicBackup;
  CB_BackupVNodes.Enabled := CyclicBackup;
  Edit_BakDir.Enabled := ( CyclicOrIntervalBackup and RB_BakUserDir.Checked );
  TB_OpenDlgBakDir.Enabled:= Edit_BakDir.Enabled;
end; // Checkbox_BackupClick

procedure TForm_OptionsNew.CheckBox_TimerMinimizeClick(Sender: TObject);
begin
  Spin_TimerMinInt.Enabled := CheckBox_TimerMinimize.Checked;
  if ( Spin_TimerMinInt.Enabled and ( Pages.ActivePage = 'PG_Actions' )) then
    Spin_TimerMinInt.SetFocus;
end;

procedure TForm_OptionsNew.CheckBox_TimerCloseClick(Sender: TObject);
begin
  Spin_TimerCloseInt.Enabled := CheckBox_TimerClose.Checked;
  CB_CloseEncOnly.Enabled := CheckBox_TimerClose.Checked;
  if ( Spin_TimerCloseInt.Enabled and ( Pages.ActivePage = 'PG_Actions' )) then
  begin
    Spin_TimerCloseInt.SetFocus;

    if (( not Checkbox_AutoSave.Checked ) and ( not AutoCloseWarned )) then
    begin
      AutoCloseWarned := true;
      App.WarningPopup(GetRS(sOpt15));
    end;
  end;
end;

procedure TForm_OptionsNew.CheckBox_HotkeyActivateClick(Sender: TObject);
begin
  Edit_HotKey.Enabled := CheckBox_HotkeyActivate.Checked;
  if ( Edit_HotKey.Enabled and ( Pages.ActivePage = 'PG_Interface' )) then
    Edit_HotKey.SetFocus;
end;

procedure TForm_OptionsNew.TB_OpenDlgBakDirClick(Sender: TObject);
var
  Dir: string;
begin
  if SelectDirectory('','', Dir) then
     Edit_BakDir.Text:= Dir;

  TB_OpenDlgBakDir.Down:= false;
end;


procedure TForm_OptionsNew.TB_OpenDlgURLAltBrowserPathClick(Sender: TObject);
begin
  Form_Main.OpenDlg.Filter := FILTER_WEB_BROWSER;
  if Form_Main.OpenDlg.Execute then begin
     Edit_URLAltBrowserPath.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlgURLAltBrowserPath.Down:= false;
end;

procedure TForm_OptionsNew.TB_OpenDlgUserFileClick(Sender: TObject);
begin
  Form_Main.OpenDlg.Filter := FILTER_NOTEFILES {$IFDEF WITH_DART} + '|' + FILTER_DARTFILES + {$ENDIF} + '|' + GetRS(FILTER_ALLFILES);
  if Form_Main.OpenDlg.Execute then begin
     Edit_UserFile.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlgUserFile.Down:= false;
end;

procedure TForm_OptionsNew.TVGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  myData: POption;

begin
   myData:= TV.GetNodeData<POption>(Node);
   CellText:= myData.Caption;
end;

procedure TForm_OptionsNew.TVPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  myData: POption;

begin
   myData:= TV.GetNodeData<POption>(Node);

  if myData.Parent then begin
     TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
     TargetCanvas.Font.Color := clNavy;
  end;

end;

procedure TForm_OptionsNew.TVChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  HC: THelpContext;
  Idx: integer;

begin
  try
    Idx:= TV.AbsoluteIndex(Node);
    Pages.PageIndex := Idx;
    //self.HelpContext := 205 + succ( Pages.PageIndex );
   {
     I can’t access the Help Context values associated with each TPage object. ¿? I do it differently:

     Configuration Options [87](id:295)
       General Settings [88](id:238)
         Rich Text Editor [89](id:239)
         Images [90](id:573)
         Tree Panel [91](id:240)
       KeyNote Files [92](id:241)
         File Options [93](id:242)
         Backup Options [94](id:243)
       Actions [95](id:244)
         Confirmations [96](id:245)
       Chrome [97](id:246)
         Tab Icons [98](id:247)
       Advanced [99](id:248)
         Formats [100](id:249)
         Clipboard [101](id:250)
         File Types [102](id:251)
         Folding Blocks [124]
         Other [103](id:252)

   }
    case Pages.PageIndex of
      0: HC:= 88;
      1: HC:= 89;
      2: HC:= 90;
      3: HC:= 91;
      4: HC:= 92;
      5: HC:= 93;
      6: HC:= 94;
      7: HC:= 95;
      8: HC:= 96;
      9: HC:= 97;
      10: HC:= 98;
      11: HC:= 99;
      12: HC:= 100;
      13: HC:= 101;
      14: HC:= 102;
      15: HC:= 124;
      16: HC:= 103;
    end;
    //self.HelpContext := Pages.HelpContext;
    self.HelpContext := HC;

  except
     App.ErrorPopup(Format( GetRS(sOpt16), [Pages.PageIndex, Idx]));
  end;
end;


procedure TForm_OptionsNew.Button_AddTxtExtClick(Sender: TObject);
var
  ext : string;
begin
  ext := '';
  if InputQuery( GetRS(sOpt17), GetRS(sOpt18), ext ) then
  begin
    if ( ext = '' ) then exit;
    ext := ansilowercase( ext );
    if ( List_TxtExt.Items.IndexOf( ext ) >= 0 ) then
    begin
      App.InfoPopup(Format(GetRS(sOpt19), [ext]));
      exit;
    end;
    List_TxtExt.ItemIndex := List_TxtExt.Items.Add( ext );
  end;
end;

procedure TForm_OptionsNew.Button_DeleteTxtExtClick(Sender: TObject);
var
  i : integer;
begin
  if ( List_TxtExt.Items.Count = 0 ) then exit;
  i := List_TxtExt.ItemIndex;
  if ( i < 0 ) then exit;

  {
  if ( messagedlg( Format(
    'Delete text file extension "%s"?',
    [List_TxtExt.Items[i]]
  ), mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then exit;
  }

  List_TxtExt.Items.Delete( i );
  if ( List_TxtExt.Items.Count > 0 ) then
  begin
    if ( i > 0 ) then
      List_TxtExt.ItemIndex := pred( i )
    else
      List_TxtExt.ItemIndex := 0;
  end;
end;

procedure TForm_OptionsNew.Button_ResetTxtExtClick(Sender: TObject);
begin
  if ( App.DoMessageBox( GetRS(sOpt20),
       mtConfirmation, [mbOK,mbCancel], Def2) <> mrOK ) then exit;
  with myOpts do
  begin
    ExtText := Def_Text_Extensions;
    delete( ExtText, 1, 1 );
    delete( ExtText, length( ExtText ), 1 );
    CSVTextToStrs( List_TxtExt.Items, exttext, '.' );
  end;
end;



procedure TForm_OptionsNew.Button_HelpClick(Sender: TObject);
begin
  ActiveKeyNoteHelp(85);  // Configuring KeyNote [293]
end;


procedure TForm_OptionsNew.RB_BakOriginalDirClick(Sender: TObject);
begin
  Edit_BakDir.Enabled := RB_BakUserDir.Checked;
  TB_OpenDlgBakDir.Enabled:= Edit_BakDir.Enabled;
end;

procedure TForm_OptionsNew.RB_ClipTreeActiveClick(Sender: TObject);
begin
  Combo_ClipNodeNaming.Enabled := RB_ClipTreeNew.Checked;
  LB_ClipNodeNaming.Enabled := RB_ClipTreeNew.Checked;
end;

procedure TForm_OptionsNew.CheckBox_TrackCaretPosClick(Sender: TObject);
begin
  if CheckBox_TrackCaretPos.Checked then
    CB_TrackWordCount.Checked := false;
end;

procedure TForm_OptionsNew.CB_AsTextClick(Sender: TObject);
var
   Plain: boolean;
begin
  Plain:= CB_AsText.Checked;
  Combo_Size.Enabled:= Plain;
  Label_MaxSize.Enabled:= Plain;
end;

procedure TForm_OptionsNew.CB_ShowFullPathClick(Sender: TObject);
begin
  CB_PathTopToBottom.Enabled:= CB_ShowFullPath.Checked;
  if not CB_ShowFullPath.Checked then
     CB_PathTopToBottom.Checked:= False;
end;

procedure TForm_OptionsNew.CB_TrackWordCountClick(Sender: TObject);
begin
  if CB_TrackWordCount.Checked then
    CheckBox_TrackCaretPos.Checked := false;
end;

procedure TForm_OptionsNew.btnBGColorClick(Sender: TObject);
begin
    ColorDlg.Color := myOpts.ImgViewerBGColor;
    if ColorDlg.Execute then
      myOpts.ImgViewerBGColor := ColorDlg.Color;
end;

procedure TForm_OptionsNew.txtSepInListsExit(Sender: TObject);
begin
   CheckNumbListSepFactor;
end;

procedure TForm_OptionsNew.CheckNumbListSepFactor;
var
  SepFactor: Single;
begin
   SepFactor := StrToFloatDef( txtSepInLists.Text, 5.7);
   if SepFactor > 17 then
      SepFactor:= 17
   else
   if SepFactor < 2 then
      SepFactor:= 2;

   txtSepInLists.Text:= SepFactor.ToString(ffGeneral,3,2);
end;


procedure TForm_OptionsNew.txtImgCompressionQualityExit(Sender: TObject);
begin
  CheckImgCompressionQualityValue;
end;

procedure TForm_OptionsNew.txtImgMaxAutoWidthGoalExit(Sender: TObject);
begin
  CheckImgMaxAutoWidthGoalValue;
end;

procedure TForm_OptionsNew.txtImgRatioSizePngVsJPGExit(Sender: TObject);
begin
  CheckImgRatioSizePngVsJPGValue;
end;


procedure TForm_OptionsNew.CheckImgMaxAutoWidthGoalValue;
var
  MaxAutoWidthGoal: integer;
begin
   MaxAutoWidthGoal := strtointDef( txtImgMaxAutoWidthGoal.Text, 0);
   if ( MaxAutoWidthGoal > 2000 ) then
       MaxAutoWidthGoal:= 2000
   else
     if (MaxAutoWidthGoal < -1) then
        MaxAutoWidthGoal:= -1;

   txtImgMaxAutoWidthGoal.Text:= MaxAutoWidthGoal.ToString;
end;

procedure TForm_OptionsNew.CheckImgCompressionQualityValue;
var
  CompressQuality: integer;
begin
   CompressQuality := Round(StrToFloatDef( txtImgCompressionQuality.Text, 80));
   if ( CompressQuality > 100 ) then
       CompressQuality:= 100
   else
     if (CompressQuality < 0) then
        CompressQuality:= 0;

   txtImgCompressionQuality.Text:= CompressQuality.ToString;
end;

procedure TForm_OptionsNew.CheckImgRatioSizePngVsJPGValue;
var
  RatioSize: Single;
begin
   RatioSize := StrToFloatDef( txtImgRatioSizePngVsJPG.Text, 0.0);
   if ( RatioSize > 25 ) then begin
       RatioSize:= 25
   end
   else
     if (RatioSize < 0) then
        RatioSize:= 0;

   txtImgRatioSizePngVsJPG.Text:= RatioSize.ToString(ffGeneral,3,2);
end;


procedure TForm_OptionsNew.chkImgSingleViewerInstanceClick(Sender: TObject);
begin
  chkImgHotTrackViewer.Enabled:= chkImgSingleViewerInstance.Checked;
  if not chkImgSingleViewerInstance.Checked then
     chkImgHotTrackViewer.Checked:= False;
end;


// --------------------------------


procedure TForm_OptionsNew.EditBlock( const NewBlock : boolean );
var
  Form_BlockDef : TForm_FoldBlockDef;
  Opening, Closing, CaseSens, Disposable, UseOnExpand : string;
  Item, dupItem : TListItem;
  dupOnExpand: boolean;
  i : integer;
begin

  Item := nil;

  if NewBlock then begin
     Opening := '';
     Closing := '';
  end
  else begin
     Item := LVfb.Selected;
     if ((not assigned( Item )) or
        (LVfb.Items.Count = 0 )) then begin
        App.InfoPopup(GetRS(sFoldBl0));
        exit;
     end;
     Opening := Item.Caption;
     Closing := Item.Subitems[0];
     CaseSens := Item.Subitems[1];
     Disposable:= Item.Subitems[2];
     UseOnExpand:= Item.Subitems[3];
  end;


  Form_BlockDef := TForm_FoldBlockDef.Create( self );
  try
    with Form_BlockDef do begin
       Edit_Opening.Text := Opening;
       Edit_Closing.Text := Closing;
       chkCaseSens.Checked:= (CaseSens = 'x');
       chkDispos.Checked:= (Disposable = 'x');
       chkOnExpand.Checked:= (UseOnExpand = 'x');
    end;

    if ( Form_BlockDef.ShowModal = mrOK ) then begin
      with Form_BlockDef do begin
         Opening := Edit_Opening.Text;
         Closing := Edit_Closing.Text;
         CaseSens := '';
         Disposable := '';
         UseOnExpand:= '';
         if chkCaseSens.Checked then
            CaseSens := 'x';
         if chkDispos.Checked then
            Disposable := 'x';
         if chkOnExpand.Checked then
            UseOnExpand := 'x';
      end;

      if (Opening = '') or (Closing = '') then begin
         App.ErrorPopup(GetRS(sFoldBl1));
         exit;
      end;

      dupItem := nil;
      dupOnExpand:= false;

      if (LVfb.Items.Count > 0) then begin
         for i := 0 to LVfb.Items.Count -1 do begin
            if Item = LVfb.Items[i] then continue;
            if LVfb.Items[i].Caption = Opening then begin
               dupItem := LVfb.Items[i];
               break;
            end;
            if (UseOnExpand = 'x') and (LVfb.Items[i].Subitems[3] = 'x') then begin
               dupOnExpand:= true;
               break;
            end;
         end;
      end;

      if assigned(dupItem) then begin
         if App.DoMessageBox( Format(GetRS(sFoldBl2), [Opening, dupItem.subitems[0]] ), mtConfirmation, [mbYes,mbNo], def2) <> mrYes then
            exit
         else begin
           if NewBlock then
              Item := dupItem
           else
              LVfb.Items.Delete(LVfb.Items.IndexOf( dupItem ));
         end;
      end;
      if dupOnExpand then begin
         App.WarningPopup( GetRS(sFoldBl5));
         exit;
      end;


      try
        if ( Item = nil ) then
           Item := LVfb.Items.Add;

        Item.Caption := Opening;
        Item.Subitems.Clear;
        Item.Subitems.Add(Closing);
        Item.Subitems.Add(CaseSens);
        Item.Subitems.Add(Disposable);
        Item.Subitems.Add(UseOnExpand);
        LVfb.Selected := Item;

        FoldingBlocks_Changed:= True;

      except
        on E : Exception do
           App.ErrorPopup(E.Message);
      end;

    end;
  finally
    Form_BlockDef.Free;
  end;

  LVfb.SetFocus;
end;


procedure TForm_OptionsNew.DeleteBlock;
var
  item : TListItem;
begin
  item := LVfb.Selected;

  if not assigned(item) then begin
    App.InfoPopup(GetRS(sFoldBl0));
    exit;
  end;

  LVfb.Items.Delete( LVfb.Items.IndexOf( item ));
  FoldingBlocks_Changed:= True;
end;



procedure TForm_OptionsNew.btnFBNewClick(Sender: TObject);
begin
  EditBlock( true );
end;

procedure TForm_OptionsNew.btnFBEditClick(Sender: TObject);
begin
  EditBlock( false );
end;

procedure TForm_OptionsNew.btnFBDeleteClick(Sender: TObject);
begin
  DeleteBlock;
  LVfb.SetFocus;
end;

end.
