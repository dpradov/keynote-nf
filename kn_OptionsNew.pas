unit kn_OptionsNew;

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
 <eristic@lodz.pdi.net> (Poland).
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


interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  gf_misc, gf_Const, kn_Info,
  ComCtrls95, Spin, kn_Chest,
  ExtCtrls, Mask, ToolEdit,
  Buttons, ComCtrls, cmpGFXComboBox,
  kn_Const, kn_INI, ShellAPI,
  gf_strings, ExtDlgs, TreeNT,
  FileCtrl, kn_DateTime,
  cmpGFXListBox, TntStdCtrls, TB97Ctls;

type
  TForm_OptionsNew = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    FontDlg: TFontDialog;
    ColorDlg: TColorDialog;
    IconDlg: TOpenDialog;
    TV: TTreeNT;
    Pages: TNotebook;
    GroupBox1: TTntGroupBox;
    checkbox_IconInTray: TTntCheckBox;
    checkbox_StartMinimized: TTntCheckBox;
    CheckBox_MinimizeOnClose: TTntCheckBox;
    CheckBox_ShowTooltips: TTntCheckBox;
    CheckBox_SplashScreen: TTntCheckBox;
    GroupBox2: TTntGroupBox;
    CB_LoadLastFile: TTntCheckBox;
    CB_LoadUserFile: TTntCheckBox;
    Edit_UserFile: TTntEdit;
    GroupBox3: TTntGroupBox;
    CheckBox_MRUSubmenu: TTntCheckBox;
    CheckBox_MRUUse: TTntCheckBox;
    CheckBox_MRUFullPath: TTntCheckBox;
    Spin_MRUCount: TSpinEdit;
    GroupBox7: TTntGroupBox;
    Label4: TTntLabel;
    Combo_EscapeAction: TTntComboBox;
    Edit_HotKey: THotKey;
    CheckBox_HotkeyActivate: TTntCheckBox;
    GroupBox10: TTntGroupBox;
    Label9: TTntLabel;
    Label10: TTntLabel;
    Bevel4: TBevel;
    CheckBox_TimerMinimize: TTntCheckBox;
    CheckBox_TimerClose: TTntCheckBox;
    CB_CloseEncOnly: TTntCheckBox;
    Spin_TimerMinInt: TSpinEdit;
    Spin_TimerCloseInt: TSpinEdit;
    GroupBox4: TTntGroupBox;
    Label2: TTntLabel;
    Label3: TTntLabel;
    Label_SampleDate: TTntLabel;
    Label_SampleTime: TTntLabel;
    Edit_DateFormat: TTntComboBox;
    Edit_TimeFormat: TTntComboBox;
    GroupBox8: TTntGroupBox;
    Label16: TTntLabel;
    CheckBox_DisableFileMon: TTntCheckBox;
    CheckBox_ShowFonts: TTntCheckBox;
    CheckBox_NoRegistry: TTntCheckBox;
    CheckBox_UseOldColorDlg: TTntCheckBox;
    CheckBox_RunAutoMacros: TTntCheckBox;
    CheckBox_SafePrint: TTntCheckBox;
    GroupBox6: TTntGroupBox;
    Label_MaxSize: TTntLabel;
    Label7: TTntLabel;
    Label_bytes: TTntLabel;
    Combo_Size: TTntComboBox;
    Combo_Divider: TTntComboBox;
    CB_IgnoreSelf: TTntCheckBox;
    CB_AsText: TTntCheckBox;
    BitBtn_TknHlp: TBitBtn;
    CheckBox_ClipRecall: TTntCheckBox;
    CheckBox_Sound: TTntCheckBox;
    GBox_EditorGlobal: TTntGroupBox;
    CheckBox_WordSelect: TTntCheckBox;
    CB_SaveCaretPos: TTntCheckBox;
    CheckBox_TrackCaretPos: TTntCheckBox;
    CheckBox_AutoIndent: TTntCheckBox;
    GBox_TreeGlobal: TTntGroupBox;
    CheckBox_EditNewNodes: TTntCheckBox;
    CheckBox_EditInPlace: TTntCheckBox;
    CheckBox_InheritBGColor: TTntCheckBox;
    GroupBox9: TTntGroupBox;
    Checkbox_AutoSave: TTntCheckBox;
    CheckBox_AutoSaveOnFocus: TTntCheckBox;
    CheckBox_AutoSaveOnTimer: TTntCheckBox;
    Spin_AutoSaveOnTimerInt: TSpinEdit;
    Label_Minutes: TTntLabel;
    GroupBox13: TTntGroupBox;
    CheckBox_OpenFloppyReadOnly: TTntCheckBox;
    CheckBox_OpenReadOnlyWarn: TTntCheckBox;
    CheckBox_OpenNetworkReadOnly: TTntCheckBox;
    GroupBox14: TTntGroupBox;
    CheckBox_TabsStacked: TTntCheckBox;
    CheckBox_TabsHotTrack: TTntCheckBox;
    CheckBox_TabsImages: TTntCheckBox;
    CheckBox_HotKeyWarn: TTntCheckBox;
    GroupBox15: TTntGroupBox;
    CheckBox_AutoRegisterPrompt: TTntCheckBox;
    CheckBox_AutoRegisterFileType: TTntCheckBox;
    CheckBox_EncFileAltExt: TTntCheckBox;
    GroupBox5: TTntGroupBox;
    Label5: TTntLabel;
    RB_ActiveTab: TTntRadioButton;
    RB_InactiveTab: TTntRadioButton;
    Edit_Sample: TTntEdit;
    BTN_Font: TBitBtn;
    BTN_Color: TBitBtn;
    BTN_Defaults: TBitBtn;
    GroupBox_ICN: TTntGroupBox;
    Label_ICN: TTntLabel;
    Button_ICNAdd: TTntButton;
    Button_ICNInsert: TTntButton;
    Button_ICNDelete: TTntButton;
    Button_ICNReset: TTntButton;
    GroupBox17: TTntGroupBox;
    CheckBox_InsCharKeepFont: TTntCheckBox;
    CheckBox_InsCharWinClose: TTntCheckBox;
    GroupBox16: TTntGroupBox;
    Label8: TTntLabel;
    Combo_URLAction: TTntComboBox;
    CheckBox_URLShift: TTntCheckBox;
    CheckBox_MinOnURL: TTntCheckBox;
    CheckBox_URLFileAuto: TTntCheckBox;
    GroupBox12: TTntGroupBox;
    CheckBox_AutoPasteEval: TTntCheckBox;
    CheckBox_AutoPastePlugin: TTntCheckBox;
    CheckBox_ConfirmTreePaste: TTntCheckBox;
    checkbox_ConfirmExit: TTntCheckBox;
    checkbox_ConfirmDelete: TTntCheckBox;
    CheckBox_ConfirmNodeDelete: TTntCheckBox;
    CheckBox_FixScrollBars: TTntCheckBox;
    CheckBox_AutoNameVNodes: TTntCheckBox;
    Button_Help: TTntButton;
    CheckBox_LongCombos: TTntCheckBox;
    GroupBox19: TTntGroupBox;
    List_TxtExt: TListBox;
    Button_AddTxtExt: TTntButton;
    Button_DeleteTxtExt: TTntButton;
    Button_ResetTxtExt: TTntButton;
    GroupBox20: TTntGroupBox;
    Label12: TTntLabel;
    Label11: TTntLabel;
    Label1: TTntLabel;
    Label_UndoLimit: TTntLabel;
    Spin_UndoLimit: TSpinEdit;
    Spin_IndentInc: TSpinEdit;
    Spin_FontSizeInc: TSpinEdit;
    Spin_ParaSpaceInc: TSpinEdit;
    List_Icn: TGFXListBox;
    Bevel1: TBevel;
    CheckBox_RichEditv3: TTntCheckBox;
    GroupBox21: TTntGroupBox;
    CB_AutoFont: TTntCheckBox;
    CB_AutoKeyboard: TTntCheckBox;
    Label6: TTntLabel;
    CB_IgnoreUpgrades: TTntCheckBox;
    CB_ConfirmNodeRefresh: TTntCheckBox;
    GroupBox11: TTntGroupBox;
    Label_Bak: TTntLabel;
    checkbox_Backup: TTntCheckBox;
    CheckBox_BackupAppendExt: TTntCheckBox;
    Edit_BackupExt: TTntEdit;
    Label_BakDir: TTntLabel;
    RB_BakOriginalDir: TTntRadioButton;
    RB_BakUserDir: TTntRadioButton;
    Edit_BakDir: TTntEdit;
    Label_MaxBak2: TTntLabel;
    Combo_BakLevel: TTntComboBox;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label_MaxBak1: TTntLabel;
    Label13: TTntLabel;
    CB_SkipNewFilePrompt: TTntCheckBox;
    Label14: TTntLabel;
    Label15: TTntLabel;
    Combo_TabPos: TTntComboBox;
    CheckBox_SingleInstance: TTntCheckBox;
    CB_InheritNodeProperties: TTntCheckBox;
    CB_BackupVNodes: TTntCheckBox;
    CheckBox_HotTrackTree: TTntCheckBox;
    CheckBox_AutoScroll: TTntCheckBox;
    CheckBox_TreeTips: TTntCheckBox;
    CB_ShowFullPath: TTntCheckBox;
    CB_ShowFullPathSearch: TTntCheckBox;
    CB_PathTopToBottom: TTntCheckBox;
    Bevel6: TBevel;
    GroupBox18: TTntGroupBox;
    CB_DropNodesOnTabPrompt: TTntCheckBox;
    CB_DropNodesOnTabMove: TTntCheckBox;
    CB_ResPanelActiveUpdate: TTntCheckBox;
    Label17: TTntLabel;
    Combo_ExpandMode: TTntComboBox;
    GroupBox23: TTntGroupBox;
    RB_ClipTreeActive: TTntRadioButton;
    RB_ClipTreeNew: TTntRadioButton;
    LB_ClipNodeNaming: TTntLabel;
    Combo_ClipNodeNaming: TTntComboBox;
    CB_AutoNewFile: TTntCheckBox;
    CB_TrackWordCount: TTntCheckBox;
    CB_TreeClipConfirm: TTntCheckBox;
    CB_TestDupClips: TTntCheckBox;
    CB_DTUseLastSelection: TTntCheckBox;
    GroupBox22: TTntGroupBox;
    CB_WordAtCursor: TTntCheckBox;
    CB_FindAutoClose: TTntCheckBox;
    CB_TimerCloseDialogs: TTntCheckBox;
    CB_TimerCloseAutoReopen: TTntCheckBox;
    CB_SwitchIcon: TTntCheckBox;
    CB_SourceURL: TTntCheckBox;
    GroupBox24: TTntGroupBox;
    RB_URLSystemBrowser: TTntRadioButton;
    RB_URLAltBrowser: TTntRadioButton;
    Edit_URLAltBrowserPath: TTntEdit;
    Label18: TTntLabel;
    Combo_Language: TTntComboBox;
    TB_OpenDlgUserFile: TToolbarButton97;
    TB_OpenDlgURLAltBrowserPath: TToolbarButton97;
    TB_OpenDlgBakDir: TToolbarButton97;
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
    procedure CheckBox_AsTextClick(Sender: TObject);
    procedure Combo_SizeKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox_AutoRegisterFileTypeClick(Sender: TObject);
    procedure BitBtn_TknHlpClick(Sender: TObject);
    procedure checkbox_BackupClick(Sender: TObject);
    procedure CheckBox_TimerMinimizeClick(Sender: TObject);
    procedure CheckBox_TimerCloseClick(Sender: TObject);
    procedure CheckBox_HotkeyActivateClick(Sender: TObject);
    procedure TVChange(Sender: TObject; Node: TTreeNTNode);
    procedure Button_AddTxtExtClick(Sender: TObject);
    procedure Button_DeleteTxtExtClick(Sender: TObject);
    procedure Button_ResetTxtExtClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    procedure RB_BakOriginalDirClick(Sender: TObject);
    procedure RB_ClipTreeActiveClick(Sender: TObject);
    procedure CheckBox_TrackCaretPosClick(Sender: TObject);
    procedure CB_TrackWordCountClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Initializing : boolean;
    OK_Click : boolean;
    myOpts : TKeyOptions;
    myTabOpts : TTabOptions;
    myClipOpts : TClipOptions;
    myTreeOpts : TKNTTreeOptions;
    myFindOpts : TFindOptions;

    myEditorOptions : TEditorOptions;
    myTreeOptions : TKNTTreeOptions;

    Icons_Changed : boolean;
    Icons_RefList : TStringList;
    Icon_Change_Canceled : boolean;
    Icons_Change_Disable : boolean;
    AutoCloseWarned : boolean;


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
  end;


implementation
uses kn_LanguagesMng, kn_Main, TntFileCtrl, TntSysUtils;

{$R *.DFM}

resourcestring
  STR_01 = ' Custom icons are DISABLED ';
  STR_02 = 'Maximum size for clipboard capture text is not a valid integer value.';
  STR_03 = '(invalid date format)';
  STR_04 = '(invalid time format)';
  STR_05 = 'OK to reset tab fonts and colors to default state?';
  STR_06 = ' icon %d ';
  STR_07 = 'Icons: %d';
  STR_08 = 'Failed to get icon from ';
  STR_09 = 'Failed to get bitmap from "%s"';
//'Failed to get icon from ';
  STR_10 = 'Cannot delete this icon: List must contain at least 1 icon.';
  STR_11 = 'OK to delete the selected icon?';
  STR_12 = 'Failed to delete icon: ';
  STR_13 = 'OK to restore factory default icons?';
  STR_14 = 'Divider string can contain the following tokens:' +#13+
    '(must be UPPERCASE)' +#13#13+
     '%s = current date' +#13+
     '%s = current time' +#13+
     '%s = replaced with a blank line';
  STR_15 = 'The Auto-Close function will work ONLY if Auto-Save is turned ON, and if no dialog box is open at the time KeyNote tries to automatically close the file. (Auto-Save is currently DISABLED.)';
  STR_16 = 'Error in TVChange: PageIndex %d  Node.AbsIdx %d';

procedure TForm_OptionsNew.FormCreate(Sender: TObject);
var
  u : TURLAction;
  i : integer;
  tp : TTabOrientation;
  txm : TTreeExpandMode;
  cln : TClipNodeNaming;
  Language : TLanguageInfo;
begin
  Initializing := true;
  OK_Click := false;
  Pages.PageINdex := 0;
  AutoCloseWarned := false;
  List_Icn.CheckBoxes := false;

  for i := 0 to LanguagesAvailables.Count -1 do begin
    Language := TLanguageInfo( LanguagesAvailables.Objects[i] );
    Combo_Language.Items.Add( Language.Name );
  end;

  with Combo_Divider.Items do
  begin
    Add( DIV_1_BLANK );
    Add( DIV_2_BLANK );
    Add( '---------------------' );
    Add( '=====================' );
  end;

  for u := low( TURLAction ) to high( TURLAction ) do
    Combo_URLAction.Items.Add( URL_ACTIONS[u] );
  Combo_URLAction.ItemIndex := 0;

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

  for tp := low( tp ) to high( tp ) do
    Combo_TabPos.Items.Add( TAB_POSITIONS[tp] );
  Combo_TabPos.ItemIndex := 0;

  Icon_Change_Canceled := false;
  Icons_Change_Disable := false;
end; // CREATE

procedure TForm_OptionsNew.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;
  Initializing := false;
  OptionsToForm;

  UpdateDateFmt;
  UpdateTimeFmt;
  UpdateFontSample;

  if Icons_Change_Disable then
  begin
    GroupBox_ICN.Caption := STR_01;
    GroupBox_ICN.Enabled := false;
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
  Checkbox_Backup.OnClick := checkbox_BackupClick;
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
  Combo_Size.OnKeyPress := Combo_SizeKeyPress;

  TV.Selected := TV.Items[0];


end; // ACTIVATE

procedure TForm_OptionsNew.ClipCapToForm;
begin
  // CheckBox_URLOnly.Checked := myClipOpts.URLOnly;
  Combo_Size.Items.Insert( 0, inttostr( myClipOpts.MaxSize ));
  Combo_Size.Text := inttostr( myClipOpts.MaxSize );
  if ( myClipOpts.Divider = CLIPDIVCHAR ) then
    Combo_Divider.Text := DIV_1_BLANK
  else
  if ( myClipOpts.Divider = ( CLIPDIVCHAR + CLIPDIVCHAR )) then
    Combo_Divider.Text := DIV_2_BLANK
  else
    Combo_Divider.Text := myClipOpts.Divider;
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

  RB_ClipTreeActiveClick( RB_ClipTreeActive );
  RB_ClipTreeActive.OnClick := RB_ClipTreeActiveClick;
  RB_ClipTreeNew.OnClick := RB_ClipTreeActiveClick;

  CheckBox_AsTextClick( self );
  CB_AsText.OnClick := CheckBox_AsTextClick;
end; // ClipCapToForm

function TForm_OptionsNew.FormToClipCap : boolean;
begin
    result := true;
    myClipOpts.Divider := Combo_Divider.Text;

    if ( myClipOpts.Divider = DIV_1_BLANK ) then
      myClipOpts.Divider := CLIPDIVCHAR
    else
    if ( myClipOpts.Divider = DIV_2_BLANK ) then
      myClipOpts.Divider := CLIPDIVCHAR + CLIPDIVCHAR;

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
    try
      myClipOpts.MaxSize := strtoint( Combo_Size.Text );
    except
      Combo_Size.SetFocus;
      messagedlg( STR_02, mtError, [mbOK], 0 );
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
end; // DESTROY

procedure TForm_OptionsNew.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    VK_ESCAPE : if ( not (
      Combo_EscapeAction.DroppedDown or
      Combo_Divider.DroppedDown or
      Combo_URLAction.DroppedDown or
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
  s : wideString;
begin
  with myOpts do
  begin
    AutoRegisterFileType := CheckBox_AutoRegisterFileType.Checked;
    AutoRegisterPrompt := CheckBox_AutoRegisterPrompt.Checked;
    AutoSave := CheckBox_AutoSave.Checked;
    AutoSaveOnFocus := CheckBox_AutoSaveOnFocus.Checked;
    AutoSaveOnTimer := CheckBox_AutoSaveOnTimer.Checked;

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
      if (( s <> '' ) and WideDirectoryexists( s )) then
        BackupDir := s
      else
        BackupDir := '';
    end;
    BackupLevel := succ( Combo_BakLevel.ItemIndex );

    AutoPasteEval := CheckBox_AutoPasteEval.Checked;
    AutoPastePlugin := CheckBox_AutoPastePlugin.Checked;

    DropNodesOnTabMove := CB_DropNodesOnTabMove.Checked;
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
    URLCLickShift := CheckBox_URLShift.Checked;
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
    HotKeyWarn := CheckBox_HotKeyWarn.Checked;
    UseOldColorDlg := CheckBox_UseOldColorDlg.Checked;
    RunAutoMacros := CheckBox_RunAutoMacros.Checked;
    SafePrint := CheckBox_SafePrint.Checked;
    IgnoreUpgrades := CB_IgnoreUpgrades.Checked;
    ResPanelActiveUpdate := CB_ResPanelActiveUpdate.Checked;
    // UseNewStyleURL := CB_UseNewStyleURL.Checked;

    ExtText := '';
    for i := 1 to List_TxtExt.Items.Count do
    begin
      ExtText := ExtText + '.' + List_TxtExt.Items[pred( i )];
    end;
    ExtText := ExtText + '.';


    InsCharKeepFont := ( not CheckBox_InsCharKeepFont.Checked );
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
  end;

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


    CheckBox_AutoPasteEval.Checked := AutoPasteEval;
    CheckBox_AutoPastePlugin.Checked := AutoPastePlugin;

    CB_DropNodesOnTabMove.Checked := DropNodesOnTabMove;
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
    CheckBox_URLShift.Checked := URLCLickShift;
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
    CheckBox_HotKeyWarn.Checked := HotKeyWarn;
    CheckBox_UseOldColorDlg.Checked := UseOldColorDlg;
    CheckBox_RunAutoMacros.Checked := RunAutoMacros;
    CheckBox_SafePrint.Checked := SafePrint;
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

    CheckBox_InsCharKeepFont.Checked := ( not InsCharKeepFont );
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
      Add( LongDateFormat );
      Add( ShortDateFormat );
    end;
    Edit_DateFormat.Text := DateFmt;

    with Edit_TimeFormat.Items do
    begin
      Add( TimeFmt );
      Add( LongTimeFormat );
      Add( SHortTimeFormat );
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
  end;

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
    Label_SampleDate.Caption := STR_03;
  end;
end; // UpdateDateFmt

procedure TForm_OptionsNew.UpdateTimeFmt;
begin
  try
    Label_SampleTime.Caption := GetDateTimeFormatted( Edit_TimeFormat.Text, now );
  except
    Label_SampleTime.Caption := STR_04;
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
  if ( messagedlg( STR_05, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

  with myTabOpts.Font do
  begin
    FSize := 8;
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
begin 
  if RB_ActiveTab.Checked then
  begin
    FontPropertiesToFont( myTabOpts.Font, FontDlg.Font );
    if FontDlg.Execute then
    begin
      FontToFontProperties( FontDlg.Font, myTabOpts.Font );
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
      List_ICN.AddItem( Format( STR_06, [succ( i )]), cbUnchecked, i );
    end;
  finally
    List_ICN.Items.EndUpdate;
  end;

  Label_ICN.Caption := Format( STR_07, [Chest.IMG_Categories.Count] );

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
            messagedlg( STR_08 + fn + #13 + E.Message, mtError, [mbOK], 0 );
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
            messagedlg( Format(
              STR_09 + #13 + E.Message,
              [fn] ), mtError, [mbOK], 0 );
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
            messagedlg( STR_08 + fn + #13 + E.Message, mtError, [mbOK], 0 );
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
    showmessage( STR_10 );
    exit;
  end;

  if ( messagedlg( STR_11, mtCOnfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

  try
    i := List_ICN.ItemIndex;
    Chest.IMG_Categories.Delete( i );
    Icons_RefList.Delete( i );
  except
    on E : Exception do
    begin
      messagedlg( STR_11 + #13#13 + E.Message, mtError, [mbOK], 0 );
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
  if ( messagedlg( STR_13, mtCOnfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

  Icons_RefList.Clear;
  LoadCategoryBitmapsBuiltIn;
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Icons_RefList.Add( '-1' );
  Icons_Changed := true;
  LoadIcons;
  List_ICN.ItemIndex := 0;
end; // ResetIcons


procedure TForm_OptionsNew.CheckBox_AsTextClick(Sender: TObject);
begin
  Label_MaxSize.Enabled := CB_AsText.Checked;
  Combo_Size.Enabled := Label_MaxSize.Enabled;
  Label_bytes.Enabled := Label_MaxSize.Enabled;
end;

procedure TForm_OptionsNew.Combo_SizeKeyPress(Sender: TObject; var Key: Char);
begin
  if ( not ( Key in [#8, #9, #13, #27, '0'..'9'] )) then
    Key := #0;
end;


procedure TForm_OptionsNew.CheckBox_AutoRegisterFileTypeClick(
  Sender: TObject);
begin
  CheckBox_AutoRegisterPrompt.Enabled := CheckBox_AutoRegisterFileType.Checked;
end;


procedure TForm_OptionsNew.BitBtn_TknHlpClick(Sender: TObject);
begin
  messagedlg( format(STR_14, [CLIPDATECHAR, CLIPTIMECHAR, CLIPDIVCHAR]),
    mtInformation, [mbOK], 0
  );

end;

procedure TForm_OptionsNew.Checkbox_BackupClick(Sender: TObject);
begin
  CheckBox_BackupAppendExt.Enabled := Checkbox_Backup.Checked;
  Edit_BackupExt.Enabled := Checkbox_Backup.Checked;
  Label_Bak.Enabled := Checkbox_Backup.Checked;
  RB_BakOriginalDir.Enabled := Checkbox_Backup.Checked;
  RB_BakUserDir.Enabled := Checkbox_Backup.Checked;
  Label_BakDir.Enabled := Checkbox_Backup.Checked;
  Label_MaxBak1.Enabled := Checkbox_Backup.Checked;
  Label_MaxBak2.Enabled := Checkbox_Backup.Checked;
  Combo_BakLevel.Enabled := Checkbox_Backup.Checked;
  CB_BackupVNodes.Enabled := Checkbox_Backup.Checked;
  Edit_BakDir.Enabled := ( Checkbox_Backup.Checked and RB_BakUserDir.Checked );
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
      messagedlg( STR_15, mtWarning, [mbOK], 0 );
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
  Dir: wideString;
begin
  if WideSelectDirectory('','', Dir) then
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
  Form_Main.OpenDlg.Filter := FILTER_NOTEFILES + '|' + FILTER_DARTFILES + '|' + FILTER_ALLFILES;
  if Form_Main.OpenDlg.Execute then begin
     Edit_UserFile.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlgUserFile.Down:= false;
end;

procedure TForm_OptionsNew.TVChange(Sender: TObject; Node: TTreeNTNode);
begin
  if ( not assigned( Node )) then exit;
  try
    Pages.PageIndex := Node.AbsoluteIndex;
    self.HelpContext := 205 + succ( Pages.PageIndex );
  except
    messagedlg( Format( STR_16, [Pages.PageIndex, Node.AbsoluteIndex]), mtError, [mbOK], 0 );
  end;
end;

procedure TForm_OptionsNew.Button_AddTxtExtClick(Sender: TObject);
var
  ext : string;
begin
  ext := '';
  if InputQuery( 'Add text file extension', 'Enter new extension for text files:', ext ) then
  begin
    if ( ext = '' ) then exit;
    ext := ansilowercase( ext );
    if ( List_TxtExt.Items.IndexOf( ext ) >= 0 ) then
    begin
      showmessage( Format( 'Extension "%s" already listed.', [ext] ));
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
  if ( messagedlg( 'Reset default text file extentions?',
    mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then exit;
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
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
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

procedure TForm_OptionsNew.CB_TrackWordCountClick(Sender: TObject);
begin
  if CB_TrackWordCount.Checked then
    CheckBox_TrackCaretPos.Checked := false;
end;




end.
