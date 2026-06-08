unit kn_Defaults;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2026 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Samples.Spin,
   Vcl.Buttons,
   Vcl.ExtCtrls,
   Vcl.Menus,
   cmpGFXComboBox,
   ComCtrls95,
   RxPlacemnt,
   LCCombo,
   gf_miscvcl,
   kn_Info,
   kn_Const,
   knt.model.note
   ;


type
  TForm_Defaults = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    ColorDlg: TColorDialog;
    FontDlg: TFontDialog;
    Pages: TPage95Control;
    Tab_Main: TTab95Sheet;
    Tab_Tree: TTab95Sheet;
    GBox_Note: TGroupBox;
    Label_TabSize: TLabel;
    CB_WordWrap: TCheckBox;
    CB_URLDetect: TCheckBox;
    CB_UseTabChar: TCheckBox;
    Spin_TabSize: TSpinEdit;
    GBox_Tree: TGroupBox;
    BTN_Font: TBitBtn;
    BTN_Color: TBitBtn;
    BTN_Defaults: TBitBtn;
    Edit_Sample: TEdit;
    CB_TreeCheck: TCheckBox;
    Label5: TLabel;
    Edit_NodeName: TComboBox;
    BitBtn_TknHlp: TBitBtn;
    Label_EditorFonts: TLabel;
    FormPlacement: TFormPlacement;
    CB_Vertical: TCheckBox;
    Button_Help: TButton;
    Label14: TLabel;
    Label2: TLabel;
    Combo_TreeImages: TComboBox;
    Combo_DefEdLang: TLanguagesCombo;
    CB_HideChecked: TCheckBox;
    CB_Zoom: TComboBox;
    LB_Zoom: TLabel;
    Label8: TLabel;
    CB_PlainText: TCheckBox;
    GB_Defaults: TGroupBox;
    CB_SaveDefaults: TCheckBox;
    CB_SaveAsDef: TCheckBox;
    LB_Scope: TLabel;
    Label3: TLabel;
    BitBtn_FolderHelp: TBitBtn;
    BitBtn_FolderChromeHelp: TBitBtn;
    BitBtn_TreeChromeHelp: TBitBtn;
    CB_InheritBGColor: TCheckBox;
    CB_TreeChrome_AllNotes: TCheckBox;
    Edit_FolderName: TComboBox;
    Label1: TLabel;
    Label4: TLabel;
    Combo_Icons: TGFXComboBox;
    gbCols: TGroupBox;
    CB_ShowDateCol: TCheckBox;
    CB_ShowFlagCol: TCheckBox;
    CB_RTL: TCheckBox;
    CB_DisableTagSel: TCheckBox;
    Tab_Advanced: TTab95Sheet;
    PagesAdv: TPage95Control;
    Tab_QL: TTab95Sheet;
    Tab_EL: TTab95Sheet;
    Tab_MultiE: TTab95Sheet;
    lbl9: TLabel;
    lbl7: TLabel;
    lbl6: TLabel;
    ExcerptMaxL: TSpinEdit;
    ExcerptMaxC: TSpinEdit;
    cb_HLine: TCheckBox;
    cb_HDate: TCheckBox;
    cb_HTags: TCheckBox;
    txtTagsOrder: TEdit;
    Tab_AdvGral: TTab95Sheet;
    CB_DescOrd: TCheckBox;
    lbl10: TLabel;
    cEntryCont: TComboBox;
    cb_AutoExp: TCheckBox;
    cUseTLq: TComboBox;
    cUseTRq: TComboBox;
    cUseCq: TComboBox;
    cUseBLq: TComboBox;
    cUseBRq: TComboBox;
    cb_TLq: TCheckBox;
    cb_TRq: TCheckBox;
    cb_Cq: TCheckBox;
    cb_BLq: TCheckBox;
    cb_BRq: TCheckBox;
    TagsTLq: TEdit;
    TagsTRq: TEdit;
    TagsCq: TEdit;
    TagsBLq: TEdit;
    TagsBRq: TEdit;
    lblQL: TLabel;
    cUseTLe: TComboBox;
    cUseTRe: TComboBox;
    cUseCe: TComboBox;
    cUseBLe: TComboBox;
    cUseBRe: TComboBox;
    cb_TLe: TCheckBox;
    cb_TRe: TCheckBox;
    cb_Ce: TCheckBox;
    cb_BLe: TCheckBox;
    cb_BRe: TCheckBox;
    TagsTLe: TEdit;
    TagsTRe: TEdit;
    TagsCe: TEdit;
    TagsBLe: TEdit;
    TagsBRe: TEdit;
    BitBtn_QL: TBitBtn;
    BitBtn_EL: TBitBtn;
    cb_CompHd: TCheckBox;
    cb_NewInEL: TCheckBox;
    cb_VincTagInSel: TCheckBox;
    lbl8: TLabel;
    cInfoBarPos: TComboBox;
    cb_EnableAdv: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure BTN_FontClick(Sender: TObject);
    procedure BTN_ColorClick(Sender: TObject);
    procedure BTN_DefaultsClick(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure CB_UseTabCharClick(Sender: TObject);
    procedure BitBtn_TknHlpClick(Sender: TObject);
    procedure CB_SaveAsDefClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    procedure Edit_FolderNameKeyPress(Sender: TObject; var Key: Char);
    procedure CB_ZoomKeyPress(Sender: TObject; var Key: Char);
    procedure CB_ZoomExit(Sender: TObject);
    procedure CB_SaveDefaultsClick(Sender: TObject);
    procedure BitBtn_FolderChromeHelpClick(Sender: TObject);
    procedure BitBtn_FolderHelpClick(Sender: TObject);
    procedure BitBtn_TreeChromeHelpClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure BitBtn_QLClick(Sender: TObject);
    procedure BitBtn_ELClick(Sender: TObject);

  private
    { Private declarations }
    fDefaultZoom: integer;
    fOriginalAction : TPropertiesAction;

    procedure CheckScope;
    procedure CheckZoomValue;

    procedure cb_ShowEntriesPanelClick(Sender: TObject);
    procedure ComboUseChange(Sender: TObject);
    procedure txtTagsEnter(Sender: TObject);
    procedure OnChangeTagsIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
    procedure OnEndTagsIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);

    function ValidateQueryLayout: boolean;
    function ValidateEditingLayout: boolean;

  public
    { Public declarations }
    Initializing : boolean;
    LoadedForm : boolean;
    Action : TPropertiesAction;
    OK_Click : boolean;
    StartWithEditorTab : boolean;
    DefaultsFN : string;

    myEditorChrome : TChrome;
    myEditorProperties : TFolderEditorProperties;
    myTabProperties : TFolderTabProperties;

    myTreeChrome : TChrome;
    ApplyTreeChromeToAllFolders : boolean;
    myTreeProperties : TFolderTreeProperties;
    myNoteAdvOptions: TNoteAdvancedOptions;

    myTabNameHistory : string;
    myHistoryCnt : integer;
    myNodeNameHistory : string;

    myInheritBGColor: boolean;

    mySaveFileDefaults : boolean;
    myCurrentFileName : string;

    myNoteIsReadOnly : boolean; // prevent changes

    procedure FormToProps;
    procedure PropsToForm;
    procedure UpdateSampleFont;

  end;


implementation
uses
   gf_misc,
   gf_strings,
   kn_global,
   kn_Chest,
   kn_Ini,
   knt.App,
   knt.ui.TagMng,
   knt.RS
  ;

{$R *.DFM}



procedure TForm_Defaults.FormCreate(Sender: TObject);
var
  i : integer;
  nodeicn : TNodeIconKind;
  pu: TNEntriesPanelUse;
  cont: TContentInMultiEntriesMode_Selectable;
  InfoBarPos: TInfoBarPosInMultiEntries;
begin
  Initializing := true;
  LoadedForm:= False;
  Pages.Visible := false; // to avoid flicker
  Pages.TabInactiveColor := _GF_CLWINDOW;
  myNoteIsReadOnly := false;

  mySaveFileDefaults := false;
  myCurrentFileName := '';
  fDefaultZoom:= 100;
  ApplyTreeChromeToAllFolders:= false;

  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  OK_Click := false;
  Action := low( TPropertiesAction );
  StartWithEditorTab := true;
  DefaultsFN := normalFN( changefileext( Application.ExeName, ext_DEFAULTS ));

  myTabNameHistory := '';
  myHistoryCnt := DEFAULT_HISTORY_COUNT;

  InitializeChrome( myEditorChrome );
  InitializeFolderEditorProperties( myEditorProperties );
  InitializeFolderTabProperties( myTabProperties );

  myNodeNameHistory := '';
  InitializeChrome( myTreeChrome );
  InitializeFolderTreeProperties( myTreeProperties );

  Edit_FolderName.MaxLength := TABNOTE_NAME_LENGTH;
  Edit_NodeName.MaxLength := TREENODE_NAME_LENGTH;

  for nodeicn := low( nodeicn ) to high( nodeicn ) do
    Combo_TreeImages.Items.Add( NODE_ICON_KINDS[nodeicn] );
  Combo_TreeImages.ItemIndex := 1;

  Combo_Icons.ImageList := Chest.IMG_Categories;
  Combo_Icons.AddItem( GetRS(sDef28), -1 );
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Combo_Icons.AddItem( ' - ' + inttostr( succ( i )), i );
  Combo_Icons.ItemIndex := 0;

  for pu := low(TNEntriesPanelUse) to high(TNEntriesPanelUse) do begin
     cUseTLq.Items.Add( ENTRIES_PANEL_USES_QL[pu] );
     cUseTRq.Items.Add( ENTRIES_PANEL_USES_QL[pu] );
     cUseCq.Items.Add(  ENTRIES_PANEL_USES_QL[pu] );
     cUseBLq.Items.Add( ENTRIES_PANEL_USES_QL[pu] );
     cUseBRq.Items.Add( ENTRIES_PANEL_USES_QL[pu] );

     cUseTLe.Items.Add( ENTRIES_PANEL_USES_EL[pu] );
     cUseTRe.Items.Add( ENTRIES_PANEL_USES_EL[pu] );
     cUseCe.Items.Add(  ENTRIES_PANEL_USES_EL[pu] );
     cUseBLe.Items.Add( ENTRIES_PANEL_USES_EL[pu] );
     cUseBRe.Items.Add( ENTRIES_PANEL_USES_EL[pu] );
  end;

  for cont := low(cont) to high(cont) do
      cEntryCont.Items.Add(CONTENT_IN_MULTIENTRIES_MODE[cont]);

  for InfoBarPos := low(InfoBarPos) to high(InfoBarPos) do
      cInfoBarPos.Items.Add(INFOBAR_POS_IN_MULTIENTRIES[InfoBarPos]);


  cb_TLq.OnClick:= cb_ShowEntriesPanelClick;
  cb_TRq.OnClick:= cb_ShowEntriesPanelClick;
  cb_Cq.OnClick:= cb_ShowEntriesPanelClick;
  cb_BLq.OnClick:= cb_ShowEntriesPanelClick;
  cb_BRq.OnClick:= cb_ShowEntriesPanelClick;
  cUseTLq.ItemIndex:= 0;
  cUseTRq.ItemIndex:= 0;
  cUseCq.ItemIndex:= 0;
  cUseBLq.ItemIndex:= 0;
  cUseBRq.ItemIndex:= 0;
  TagsTLq.Enabled:= false;
  TagsTRq.Enabled:= false;
  TagsCq.Enabled:= false;
  TagsBLq.Enabled:= false;
  TagsBRq.Enabled:= false;
  TagsTLq.Color:= clBtnFace;
  TagsTRq.Color:= clBtnFace;
  TagsCq.Color:= clBtnFace;
  TagsBLq.Color:= clBtnFace;
  TagsBRq.Color:= clBtnFace;
  cUseTLq.OnChange:= ComboUseChange;
  cUseTRq.OnChange:= ComboUseChange;
  cUseCq.OnChange:=  ComboUseChange;
  cUseBLq.OnChange:= ComboUseChange;
  cUseBRq.OnChange:= ComboUseChange;
  TagsTLq.OnEnter:= txtTagsEnter;
  TagsTRq.OnEnter:= txtTagsEnter;
  TagsCq.OnEnter:= txtTagsEnter;
  TagsBLq.OnEnter:= txtTagsEnter;
  TagsBRq.OnEnter:= txtTagsEnter;

  cb_TLe.OnClick:= cb_ShowEntriesPanelClick;
  cb_TRe.OnClick:= cb_ShowEntriesPanelClick;
  cb_Ce.OnClick:= cb_ShowEntriesPanelClick;
  cb_BLe.OnClick:= cb_ShowEntriesPanelClick;
  cb_BRe.OnClick:= cb_ShowEntriesPanelClick;
  cUseTLe.ItemIndex:= 0;
  cUseTRe.ItemIndex:= 0;
  cUseCe.ItemIndex:= 0;
  cUseBLe.ItemIndex:= 0;
  cUseBRe.ItemIndex:= 0;
  TagsTLe.Enabled:= false;
  TagsTRe.Enabled:= false;
  TagsCe.Enabled:= false;
  TagsBLe.Enabled:= false;
  TagsBRe.Enabled:= false;
  TagsTLe.Color:= clBtnFace;
  TagsTRe.Color:= clBtnFace;
  TagsCe.Color:= clBtnFace;
  TagsBLe.Color:= clBtnFace;
  TagsBRe.Color:= clBtnFace;
  cUseTLe.OnChange:= ComboUseChange;
  cUseTRe.OnChange:= ComboUseChange;
  cUseCe.OnChange:=  ComboUseChange;
  cUseBLe.OnChange:= ComboUseChange;
  cUseBRe.OnChange:= ComboUseChange;
  TagsTLe.OnEnter:= txtTagsEnter;
  TagsTRe.OnEnter:= txtTagsEnter;
  TagsCe.OnEnter:= txtTagsEnter;
  TagsBLe.OnEnter:= txtTagsEnter;
  TagsBRe.OnEnter:= txtTagsEnter;



  App.ApplyBiDiModeOnForm(Self);
end;

function TForm_Defaults.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;


// CREATE


procedure TForm_Defaults.CheckScope;
var
   SaveDefaults: boolean;

begin
    SaveDefaults:= CB_SaveDefaults.Checked;
    CB_SaveAsDef.Enabled:= SaveDefaults and not (myCurrentFileName = '');

    CB_Zoom.Enabled := SaveDefaults;
    LB_Zoom.Enabled := SaveDefaults;

    if SaveDefaults then begin
       Action:= propDefaults;
       if CB_SaveAsDef.Checked then
          LB_Scope.Caption := GetRS(sDef09)
       else
          LB_Scope.Caption := GetRS(sDef11);

    end
    else begin
       Action:= propThisFolder;
       if myNoteIsReadOnly then
          LB_Scope.Caption := GetRS(sDef05)
       else
          LB_Scope.Caption := GetRS(sDef06);
    end;


    Button_OK.Hint := GetRS(sDef0B);

    if (fOriginalAction = propThisFolder) and (myNoteIsReadOnly) then begin
       if Action = propThisFolder then begin
          Button_OK.ModalResult := mrCancel;
          Button_OK.Caption := GetRS(sDef02);
          Button_OK.Hint := GetRS(sDef03);
       end
       else begin
          Button_OK.ModalResult := mrOk;
          Button_OK.Caption := GetRS(sDef00);
       end;
       Button_Cancel.Visible := not (Action = propThisFolder);
    end;


    if   ((fOriginalAction = propThisFolder) and SaveDefaults)
      or ((fOriginalAction = propDefaults) and CB_SaveAsDef.Checked)   then

       LB_Scope.Font.Style:= [fsBold]
    else
       LB_Scope.Font.Style:= [];
end;




procedure TForm_Defaults.FormActivate(Sender: TObject);
var
  tabName: string;
begin
  OnActivate := nil;
  if ( not Initializing ) then exit;
  Initializing := false;
  App.SetTopMost(Handle, True);

  ModalFormWithTxtTagsVisible:= true;

  fOriginalAction:= Action;

  try

    if myCurrentFileName <> '' then
       CB_SaveAsDef.Caption := Format( GetRS(sDef07), [myCurrentFileName] );

    case Action of
      propThisFolder : begin
        CB_SaveDefaults.Enabled := true;
        CB_SaveDefaults.Checked := false;
        CB_SaveAsDef.Checked := False;

        tabName:= RemoveAccelChar( myTabProperties.Name );

        Caption := Format( GetRS(sDef01), [tabName] );
        if myNoteIsReadOnly then
           Caption := Caption + GetRS(sDef04)
        else
           CB_SaveDefaults.Caption := CB_SaveDefaults.Caption + Format( GetRS(sDef30), [tabName] );
      end;

      propDefaults : begin
        CB_SaveDefaults.Enabled := false;
        CB_SaveDefaults.Checked := true;
        CB_SaveAsDef.Checked := mySaveFileDefaults;

       if mySaveFileDefaults then
          Caption := GetRS(sDef08) + myCurrentFileName
       else
          Caption := GetRS(sDef10);

      end;
    end;

    CheckScope;


    Edit_FolderName.Items.BeginUpdate;
    try
      DelimTextToStrs( Edit_FolderName.Items, myTabNameHistory, HISTORY_SEPARATOR );
    finally
      Edit_FolderName.Items.EndUpdate;
    end;

    BitBtn_TknHlp.OnClick := BitBtn_TknHlpClick;
    Tab_Tree.TabVisible := true;
    CB_InheritBGColor.Visible := Tab_Tree.TabVisible;

    Edit_NodeName.Items.BeginUpdate;
    try
      DelimTextToStrs( Edit_NodeName.Items, myNodeNameHistory, HISTORY_SEPARATOR );
    finally
      Edit_NodeName.Items.EndUpdate;
    end;


    if StartWithEditorTab then
      Pages.ActivePage := Tab_Main
    else
      Pages.ActivePage := Tab_Tree;

    PropsToForm;
    UpdateSampleFont;

  finally

    Pages.OnChange := PagesChange;
    Pages.Visible := true;

  end;

  try
    if ( Pages.ActivePage = Tab_Main ) then
    begin
      Edit_FolderName.SetFocus;
      Edit_FolderName.SelectAll;
    end
    else
    begin
      Edit_NodeName.SetFocus;
      Edit_NodeName.SelectAll;
    end;
  except
  end;

end; // ACTIVATE

procedure TForm_Defaults.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i : integer;
  ValidatedQL, ValidatedEL: boolean;
begin
  if OK_Click then
  begin
    OK_Click := false;

    if ( Edit_FolderName.Text = '' ) then
    begin
      CanClose := false;
      App.ErrorPopup( GetRS(sDef12));
      Pages.ActivePage := Tab_Main;
      Edit_FolderName.SetFocus;
      exit;
    end;

    if ( pos( KNTLINK_SEPARATOR, Edit_FolderName.Text ) > 0 ) then
    begin
      CanClose := false;
      App.ErrorPopup( Format(GetRS(sDef13),[KNTLINK_SEPARATOR] ));
      Pages.ActivePage := Tab_Main;
      Edit_FolderName.SetFocus;
      exit;
    end;

    if ( pos( KNTLINK_SEPARATOR, Edit_NodeName.Text ) > 0 ) then
    begin
      CanClose := false;
      App.ErrorPopup( Format(GetRS(sDef14),[KNTLINK_SEPARATOR]));
      Pages.ActivePage := Tab_Tree;
      Edit_NodeName.SetFocus;
      exit;
    end;

    ModalFormWithTxtTagsVisible:= false;

    myTabNameHistory := AnsiQuotedStr( Edit_FolderName.Text, '"' );
    for i := 0 to pred( Edit_FolderName.Items.Count ) do
    begin
      if ( i >= myHistoryCnt ) then break;
      if (( Edit_FolderName.Items[i] <> Edit_FolderName.Text ) and ( Edit_FolderName.Items[i] <> '' )) then
        myTabNameHistory :=  myTabNameHistory + HISTORY_SEPARATOR + AnsiQuotedStr( Edit_FolderName.Items[i], '"' );
    end;
    if ( Edit_NodeName.Text <> '' ) then
      myNodeNameHistory := AnsiQuotedStr( Edit_NodeName.Text, '"' )
    else
      myNodeNameHistory := '';
    for i := 0 to pred( Edit_NodeName.Items.Count ) do
    begin
      if ( i >= myHistoryCnt ) then break;
      if (( Edit_NodeName.Items[i] <> Edit_NodeName.Text ) and ( Edit_NodeName.Items[i] <> '' )) then
        myNodeNameHistory :=  myNodeNameHistory + HISTORY_SEPARATOR + AnsiQuotedStr( Edit_NodeName.Items[i], '"' );
    end;

    FormToProps;


    ValidatedQL:= ValidateQueryLayout;
    ValidatedEL:= ValidateEditingLayout;

    if not ValidatedQL or not ValidatedEL then begin
      var Layout: string;
      var OptAllEntries: string := GetRS(sEntry13);
      var OptSelectedEntry: string := GetRS(sEntry12);
      var OptVincTags: string;

      CanClose := false;
      Pages.ActivePage := Tab_Advanced;
      if not ValidatedQL then begin
         Layout:= GetRS(sEntry18);           // Query layout
         OptVincTags:= GetRS(sEntry10);
         PagesAdv.ActivePage := Tab_QL;
         cb_TLq.SetFocus;
      end
      else begin
         Layout:= GetRS(sEntry19);           // Editing Layout
         OptVincTags:= GetRS(sEntry11);
         PagesAdv.ActivePage := Tab_EL;
         cb_TLe.SetFocus;
      end;
      App.ErrorPopup(Format( GetRS(sEntry17) + GetRS(sEntry20), [Layout, OptAllEntries, OptSelectedEntry, OptAllEntries, OptSelectedEntry, OptVincTags]) );
      exit;
    end;

  end;
  OK_Click := false;
end; // CLOSEQUERY

procedure TForm_Defaults.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not
      ( Combo_Icons.DroppedDown or Edit_FolderName.DroppedDown or Edit_NodeName.DroppedDown ))) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TForm_Defaults.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_Defaults.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_Defaults.FormToProps;
var
   p: TNEntriesMainPanel;
begin

  with myTabProperties do
  begin
    Name := trim( Edit_FolderName.Text );
    ImageIndex := pred( Combo_Icons.ItemIndex );
    RTL := CB_RTL.Checked;
  end;

  CheckZoomValue;
  with myEditorProperties do
  begin
    TabSize := Spin_TabSize.Value;
    PlainText := CB_PlainText.Checked;
    URLDetect := CB_URLDetect.Checked;
    UseTabChar := CB_UseTabChar.Checked;
    WordWrap := CB_WordWrap.Checked;
    DefaultZoom:= fDefaultZoom;
    TagSelectorDisabled:= CB_DisableTagSel.Checked;
  end;

  with myTreeProperties do
  begin
    if ( trim( Edit_NodeName.Text ) <> '' ) then
      DefaultName := trim( Edit_NodeName.Text );
    IconKind := TNodeIconKind( Combo_TreeImages.ItemIndex );
    Checkboxes := CB_TreeCheck.Checked;
    VerticalLayout := CB_Vertical.Checked;
    HideChecked:= CB_HideChecked.Checked;

    if not CB_ShowFlagCol.Checked then
       PosFlaggedCol:= 0
    else
    if (PosFlaggedCol = 0) then
       PosFlaggedCol:= 1;

    if not CB_ShowDateCol.Checked then
       PosDateCol:= 0
    else
    if (PosDateCol = 0) then
       PosDateCol:= 3;
  end;

  with myEditorChrome do
  begin
    Language := Combo_DefEdLang.Language;
  end;

  // myInheritBGColor:= CB_InheritBGColor.Checked;      // -> To modify in Global options form
  ApplyTreeChromeToAllFolders:= CB_TreeChrome_AllNotes.Checked;

  with myNoteAdvOptions do begin
     EnableAdvEditionInSingleEntryNotes:= cb_EnableAdv.Checked;
     DefaultUseForQueryLayout[pnTL]:= TNEntriesPanelUse(cUseTLq.ItemIndex);
     DefaultUseForQueryLayout[pnTR]:= TNEntriesPanelUse(cUseTRq.ItemIndex);
     DefaultUseForQueryLayout[pnCenter]:= TNEntriesPanelUse(cUseCq.ItemIndex);
     DefaultUseForQueryLayout[pnBL]:= TNEntriesPanelUse(cUseBLq.ItemIndex);
     DefaultUseForQueryLayout[pnBR]:= TNEntriesPanelUse(cUseBRq.ItemIndex);

     DefaultUseForEditingLayout[pnTL]:= TNEntriesPanelUse(cUseTLe.ItemIndex);
     DefaultUseForEditingLayout[pnTR]:= TNEntriesPanelUse(cUseTRe.ItemIndex);
     DefaultUseForEditingLayout[pnCenter]:= TNEntriesPanelUse(cUseCe.ItemIndex);
     DefaultUseForEditingLayout[pnBL]:= TNEntriesPanelUse(cUseBLe.ItemIndex);
     DefaultUseForEditingLayout[pnBR]:= TNEntriesPanelUse(cUseBRe.ItemIndex);

     // VinculatedTagsForQueryLayout and VinculatedTagsForEditingLayout are updated from OnChangeTagsIntrod

     NewEntriesAlwaysOnEdLayout:= cb_NewInEL.Checked;
     EditTagVincEntriesInSelectedEntry:= cb_VincTagInSel.Checked;
     ExtractOfText_MaxLength:= ExcerptMaxC.Value;
     ExtractOfText_MaxLines:= ExcerptMaxL.Value;
     AutoExpandInPanels:= cb_AutoExp.Checked;
     InfoBarPosInEntries:=  TInfoBarPosInMultiEntries(cInfoBarPos.ItemIndex);


     //DefaultTagsOrder: TNoteTagArray;
     //ShowNewestEntryAtStartup: boolean;

     MEContent:=          TContentInMultiEntriesMode(cEntryCont.ItemIndex);
     MEShowLineInHeader:= cb_HLine.Checked;
     MEShowTagsInHeader:= cb_HTags.Checked;
     MEShowDateInHeader:= cb_HDate.Checked;
     DescendingOrder:=    CB_DescOrd.Checked;
     MECompactHeader:=    cb_CompHd.Checked;
     //Order:=              FFolder.NoteAdvOptions.Order;
  end;

end;
// FormToProps



procedure TForm_Defaults.PropsToForm;

  procedure LoadPanelConfigQL(Pnl: TNEntriesMainPanel; CB_Panel: TCheckBox; ComboUse: TComboBox; txtTags: TEdit);
  var
    Tags: TNoteTagArray;
  begin
     CB_Panel.Checked:= (myNoteAdvOptions.DefaultUseForQueryLayout[pnl] <> pnuHidePanel);
     ComboUse.ItemIndex:= Ord(myNoteAdvOptions.DefaultUseForQueryLayout[Pnl]);
     ComboUseChange(ComboUse);
     Tags:= myNoteAdvOptions.VinculatedTagsForQueryLayout[Pnl];
     if Tags <> nil then
        txtTags.Text:= TNoteTagArrayUtils.ToNames(Tags);
  end;

  procedure LoadPanelConfigEL(Pnl: TNEntriesMainPanel; CB_Panel: TCheckBox; ComboUse: TComboBox; txtTags: TEdit);
  var
    Tags: TNoteTagArray;
  begin
     CB_Panel.Checked:= (myNoteAdvOptions.DefaultUseForEditingLayout[pnl] <> pnuHidePanel);
     ComboUse.ItemIndex:= Ord(myNoteAdvOptions.DefaultUseForEditingLayout[Pnl]);
     ComboUseChange(ComboUse);
     Tags:= myNoteAdvOptions.VinculatedTagsForEditingLayout[Pnl];
     if Tags <> nil then
        txtTags.Text:= TNoteTagArrayUtils.ToNames(Tags);
  end;


begin

  with myTabProperties do
  begin
    Edit_FolderName.Text := Name;
    Combo_Icons.ItemIndex := succ( ImageIndex );
    CB_RTL.Checked := RTL;
  end;

  with myEditorProperties do
  begin
    Spin_TabSize.Value := TabSize;
    CB_PlainText.Checked := PlainText;
    CB_URLDetect.Checked := URLDetect;
    CB_UseTabChar.Checked := UseTabChar;
    CB_WordWrap.Checked := WordWrap;
    CB_Zoom.Text:= IntToStr(DefaultZoom);
    CB_DisableTagSel.Checked := TagSelectorDisabled;
  end;

  with myTreeProperties do
  begin
    Edit_NodeName.Text := DefaultName;
    Combo_TreeImages.ItemIndex := ord( IconKind );
    CB_TreeCheck.Checked := Checkboxes;
    CB_Vertical.Checked := VerticalLayout;
    CB_HideChecked.Checked := HideChecked;
    CB_ShowDateCol.Checked:= PosDateCol > 0;
    CB_ShowFlagCol.Checked:= PosFlaggedCol > 0;
  end;

  with myEditorChrome do
  begin
    Combo_DefEdLang.Language := Language;
  end;

  CB_InheritBGColor.Checked:= myInheritBGColor;

  with myNoteAdvOptions do begin
     cb_EnableAdv.Checked:= EnableAdvEditionInSingleEntryNotes;
     LoadPanelConfigQL(pnTL, cb_TLq, cUseTLq, TagsTLq);
     LoadPanelConfigQL(pnTR, cb_TRq, cUseTRq, TagsTRq);
     LoadPanelConfigQL(pnCenter, cb_Cq, cUseCq, TagsCq);
     LoadPanelConfigQL(pnBL, cb_BLq, cUseBLq, TagsBLq);
     LoadPanelConfigQL(pnBR, cb_BRq, cUseBRq, TagsBRq);

     LoadPanelConfigEL(pnTL, cb_TLe, cUseTLe, TagsTLe);
     LoadPanelConfigEL(pnTR, cb_TRe, cUseTRe, TagsTRe);
     LoadPanelConfigEL(pnCenter, cb_Ce, cUseCe, TagsCe);
     LoadPanelConfigEL(pnBL, cb_BLe, cUseBLe, TagsBLe);
     LoadPanelConfigEL(pnBR, cb_BRe, cUseBRe, TagsBRe);

     cb_NewInEL.Checked:= NewEntriesAlwaysOnEdLayout;
     cb_VincTagInSel.Checked:= EditTagVincEntriesInSelectedEntry;
     ExcerptMaxC.Value:= ExtractOfText_MaxLength;
     ExcerptMaxL.Value:= ExtractOfText_MaxLines;
     cb_AutoExp.Checked:= AutoExpandInPanels;
     cInfoBarPos.ItemIndex:= Ord(InfoBarPosInEntries);
     //DefaultTagsOrder: TNoteTagArray;
     //ShowNewestEntryAtStartup: boolean;

     cEntryCont.ItemIndex:= Ord(MEContent);
     cb_HLine.Checked:=   MEShowLineInHeader;
     cb_HTags.Checked:=   MEShowTagsInHeader;
     cb_HDate.Checked:=   MEShowDateInHeader;
     CB_DescOrd.Checked:= DescendingOrder;
     cb_CompHd.Checked:=  MECompactHeader;
     //Order:=              FFolder.NoteAdvOptions.Order;
  end;

  LoadedForm:= True;

end; // PropsToForm

procedure TForm_Defaults.UpdateSampleFont;
begin
  if Pages.ActivePage = Tab_Tree then begin
     Edit_Sample.Color := myTreeChrome.BGColor;
     FontInfoToFont( myTreeChrome.Font, Edit_Sample.Font );
     with myTreeChrome do
       Edit_Sample.Text := Font.Name + #32 + inttostr( Font.Size ) + ' pt ' + FontStyleToStr( Font.Style );
  end
  else begin
     Edit_Sample.Color := myEditorChrome.BGColor;
     FontInfoToFont( myEditorChrome.Font, Edit_Sample.Font );
     with myEditorChrome do
       Edit_Sample.Text := Font.Name + #32 + inttostr( Font.Size ) + ' pt ' + FontStyleToStr( Font.Style );
  end;

end; // UpdateSampleFont

procedure TForm_Defaults.BTN_FontClick(Sender: TObject);
var
  dpi: integer;
begin
  dpi:= GetSystemPixelsPerInch;
  if ( Pages.ActivePage = Tab_Main ) then
  begin
    FontDlg.Options := FontDlg.Options + [fdEffects];
    FontInfoToFont( myEditorChrome.Font, FontDlg.Font, dpi );
    if FontDlg.Execute then
      FontToFontInfo( FontDlg.Font, myEditorChrome.Font, dpi );
  end
  else
  begin
    if ( not _ALLOW_TREE_FONT_COLOR ) then
      FontDlg.Options := FontDlg.Options - [fdEffects];
    FontInfoToFont( myTreeChrome.Font, FontDlg.Font, dpi );
    if FontDlg.Execute then
      FontToFontInfo( FontDlg.Font, myTreeChrome.Font, dpi );
  end;
  UpdateSampleFont;
end;

procedure TForm_Defaults.BTN_ColorClick(Sender: TObject);
begin
  if ( Pages.ActivePage = Tab_Main ) then
  begin
    ColorDlg.Color := myEditorChrome.BGColor;
    if ColorDlg.Execute then
      myEditorChrome.BGColor := ColorDlg.Color;
  end
  else
  begin
    ColorDlg.Color := myTreeChrome.BGColor;
    if ColorDlg.Execute then
      myTreeChrome.BGColor := ColorDlg.Color;
  end;
  UpdateSampleFont;
end;

procedure TForm_Defaults.BTN_DefaultsClick(Sender: TObject);
var
  tmpChrome : TChrome;
  ShiftWasDown : boolean;
begin
  ShiftWasDown := ShiftDown;
  if ( Pages.ActivePage = Tab_Main ) then
  begin
    if ( App.DoMessageBox( GetRS(sDef15), mtConfirmation, [mbYes,mbNo], def2 ) <> mrYes ) then exit;

    InitializeChrome( myEditorChrome );

    // If editing properties for active note, restore defaults from
    // keynote.def file rather than original factory defaults,
    // unless SHIFT is pressed
    if (( Action = propThisFolder ) and ( not ShiftWasDown ) and fileexists( DefaultsFN )) then
    begin
      LoadKeyNoteDefaults(
        true, // load ONLY chrome
        DefaultsFN,
        myEditorProperties,
        myEditorChrome, // only THIS will get loaded from file
        myTabProperties,
        myTreeProperties,
        tmpChrome
      );
    end;
  end
  else
  begin
    if ( App.DoMessageBox( GetRS(sDef16), mtConfirmation, [mbYes,mbNo], def2 ) <> mrYes ) then exit;
    InitializeChrome( myTreeChrome );
    InitializeChrome( tmpChrome );

    if (( Action = propThisFolder ) and ( not ShiftWasDown ) and fileexists( DefaultsFN )) then
    begin
      LoadKeyNoteDefaults(
        true, // load ONLY chrome
        DefaultsFN,
        myEditorProperties,
        tmpChrome,
        myTabProperties,
        myTreeProperties,
        myTreeChrome // only THIS will get loaded from file
      );
    end;

  end;

  UpdateSampleFont;

end;

procedure TForm_Defaults.PagesChange(Sender: TObject);
var
  NotInTabAdv: boolean;
begin
  NotInTabAdv:= (Pages.ActivePage <> Tab_Advanced);
  BTN_Font.Visible:= NotInTabAdv;
  BTN_Color.Visible:= NotInTabAdv;
  BTN_Defaults.Visible:= NotInTabAdv;
  Edit_Sample.Visible:= NotInTabAdv;

  UpdateSampleFont;
end;


procedure TForm_Defaults.BitBtn_TknHlpClick(Sender: TObject);
begin
  App.InfoPopup(
    GetRS(sDef17) +#13+
    GetRS(sDef18) +#13#13+
     NODEINSDATE  + GetRS(sDef19) +#13+
     NODEINSTIME  + GetRS(sDef20) +#13+
     NODECOUNT    + GetRS(sDef21) +#13+
     NODELEVEL    + GetRS(sDef22) +#13+
     NODEINDEX    + GetRS(sDef23) +#13+
     NODEABSINDEX + GetRS(sDef24) +#13+
     NODEPARENT   + GetRS(sDef25) +#13+
     NODENOTENAME + GetRS(sDef26) +#13+
     NODEFILENAME + GetRS(sDef27)
  );
end;

procedure TForm_Defaults.CB_UseTabCharClick(Sender: TObject);
begin
  Label_TabSize.Enabled := ( not CB_UseTabChar.Checked );
  Spin_TabSize.Enabled := Label_TabSize.Enabled;

end;


procedure TForm_Defaults.CB_SaveAsDefClick(Sender: TObject);
begin
  mySaveFileDefaults := CB_SaveAsDef.Checked;
  CheckScope;
end;


procedure TForm_Defaults.CB_SaveDefaultsClick(Sender: TObject);
begin
   if not CB_SaveDefaults.Checked then begin
      CB_SaveAsDef.Checked:= false;

      if fOriginalAction = propThisFolder then
         Edit_FolderName.Text := myTabProperties.Name;
   end
   else begin
       Edit_FolderName.Text := DefaultTabProperties.Name;
       CB_PlainText.Checked:= myEditorProperties.PlainText;
   end;

   CheckScope;
end;


procedure TForm_Defaults.CheckZoomValue;
begin
    try
      fDefaultZoom := strtoint( CB_Zoom.Text );
      if ( fDefaultZoom > 1000 ) then begin
          fDefaultZoom := 1000;
          CB_Zoom.Text:= '1000';
      end;

    except
      on E : Exception do begin
        App.ErrorPopup(E, GetRS(sDef29));
        fDefaultZoom := 100;
        CB_Zoom.Text:= '100';
      end;
    end;
end;

procedure TForm_Defaults.CB_ZoomExit(Sender: TObject);
begin
   CheckZoomValue;
end;

procedure TForm_Defaults.CB_ZoomKeyPress(Sender: TObject; var Key: Char);
begin
  if not (key in [#8, #9, #13, #27, '0'..'9']) then begin
    key := #0;
    exit;
  end;
end;


procedure TForm_Defaults.cb_ShowEntriesPanelClick(Sender: TObject);
var
  pnl: TNEntriesMainPanel;
  IsChecked: boolean;
  ComboUse: TComboBox;
  pu: TNEntriesPanelUse;
  idPnl: integer;
  EL: boolean;       // Editing layout
begin
   idPnl:= TCheckBox(Sender).Tag;
   EL:= false;
   if idPnl >= 10 then begin
      EL:= true;
      dec(idPnl, 10);
   end;

   pnl:= TNEntriesMainPanel(idPnl);
   IsChecked:= TCheckBox(Sender).Checked;

   if EL then
       case pnl of
          pnTL: ComboUse:= cUseTLe;
          pnTR: ComboUse:= cUseTRe;
          pnCenter: ComboUse:= cUseCe;
          pnBL: ComboUse:= cUseBLe;
          pnBR: ComboUse:= cUseBRe;
       end
   else
       case pnl of
          pnTL: ComboUse:= cUseTLq;
          pnTR: ComboUse:= cUseTRq;
          pnCenter: ComboUse:= cUseCq;
          pnBL: ComboUse:= cUseBLq;
          pnBR: ComboUse:= cUseBRq;
       end;

   ComboUse.Enabled:= IsChecked;
   if LoadedForm then begin
      pu:= pnuShowVinculatedWithTags;
      if not IsChecked then
         pu:= pnuHidePanel;

      ComboUse.ItemIndex:= Ord(pu);
      ComboUseChange(ComboUse);
   end;
end;


procedure TForm_Defaults.ComboUseChange(Sender: TObject);
var
  pnl: TNEntriesMainPanel;
  pu: TNEntriesPanelUse;
  CheckBox: TCheckBox;
  txtTags: TEdit;
  idPnl: integer;
  EL: boolean;       // Editing layout

begin
   idPnl:= TComboBox(Sender).Tag;
   EL:= false;
   if idPnl >= 10 then begin
      EL:= true;
      dec(idPnl, 10);
   end;

   pnl:= TNEntriesMainPanel(idPnl);
   pu:= TNEntriesPanelUse(TComboBox(Sender).ItemIndex);

   if EL then
       case pnl of
          pnTL: begin
             txtTags:= TagsTLe;
             CheckBox:= cb_TLe;
          end;
          pnTR: begin
             txtTags:= TagsTRe;
             CheckBox:= cb_TRe;
          end;
          pnCenter: begin
             txtTags:= TagsCe;
             CheckBox:= cb_Ce;
          end;
          pnBL: begin
             txtTags:= TagsBLe;
             CheckBox:= cb_BLe;
          end;
          pnBR: begin
             txtTags:= TagsBRe;
             CheckBox:= cb_BRe;
          end;
       end
   else
       case pnl of
          pnTL: begin
             txtTags:= TagsTLq;
             CheckBox:= cb_TLq;
          end;
          pnTR: begin
             txtTags:= TagsTRq;
             CheckBox:= cb_TRq;
          end;
          pnCenter: begin
             txtTags:= TagsCq;
             CheckBox:= cb_Cq;
          end;
          pnBL: begin
             txtTags:= TagsBLq;
             CheckBox:= cb_BLq;
          end;
          pnBR: begin
             txtTags:= TagsBRq;
             CheckBox:= cb_BRq;
          end;
       end;

   if pu = pnuHidePanel then
      CheckBox.Checked:= False;

   txtTags.Enabled:= (pu = pnuShowVinculatedWithTags);
   if pu <> pnuShowVinculatedWithTags then begin
      txtTags.Text:= '';
      OnChangeTagsIntrod(nil, '', txtTags);
      txtTags.Color:= clBtnFace;
   end
   else
      txtTags.Color:= clWindow;
end;


procedure TForm_Defaults.txtTagsEnter(Sender: TObject);
begin
   if CtrlDown then begin
      TEdit(Sender).Text:= '';
      OnChangeTagsIntrod(nil, '', TEdit(Sender));
   end;

   Button_Ok.Default:= False;
   TagMng.StartTxtFindTagIntrod(TEdit(Sender), OnEndTagsIntrod, OnChangeTagsIntrod, false);
end;

procedure TForm_Defaults.OnChangeTagsIntrod(FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
var
  pnl: TNEntriesMainPanel;
  idPnl: integer;
  EL: boolean;       // Editing layout
  Tags: TNoteTagArray;
begin
   idPnl:= txtTags.Tag;
   EL:= false;
   if idPnl >= 10 then begin
      EL:= true;
      dec(idPnl, 10);
   end;
   pnl:= TNEntriesMainPanel(idPnl);

   Tags:= nil;
   if FindTags <> nil then
      Tags:= TNoteTagArrayUtils.FindTagsANDToTags(FindTags);

   if EL then
      myNoteAdvOptions.VinculatedTagsForEditingLayout[pnl]:= Tags
   else
      myNoteAdvOptions.VinculatedTagsForQueryLayout[pnl]:= Tags;
end;


procedure TForm_Defaults.OnEndTagsIntrod(PressedReturn: boolean; FindTags: TFindTags; FindTagsNotRegistered: string; txtTags: TEdit);
begin
   OnChangeTagsIntrod(FindTags, FindTagsNotRegistered, txtTags);
   if PressedReturn then
      SelectNext(txtTags, True, True);

   if txtTags.Focused then
      txtTagsEnter(txtTags)
   else
      Button_Ok.Default:= True;
end;


function TForm_Defaults.ValidateQueryLayout: boolean;
var
  pnl: TNEntriesMainPanel;
  Num_ShowSelectedEntry, Num_ShowAllEntries: integer;
begin
   Result:= False;

   Num_ShowSelectedEntry:= 0;
   Num_ShowAllEntries:= 0;

   with myNoteAdvOptions do begin
      for pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
         case DefaultUseForQueryLayout[pnl] of
             pnuShowVinculatedWithTags: if (pnl = pnCenter) or (VinculatedTagsForQueryLayout[pnl] = nil) then exit;
             pnuShowSelectedEntry:      inc(Num_ShowSelectedEntry);
             pnuShowAllEntries:         inc(Num_ShowAllEntries);
             pnuHidePanel:              if (pnl = pnCenter) then exit;
         end;
      end;

      if (Num_ShowAllEntries = 1) and (Num_ShowSelectedEntry <= 1) then
         Result:= True;
   end;

end;


function TForm_Defaults.ValidateEditingLayout: boolean;
var
  pnl: TNEntriesMainPanel;
  Num_ShowSelectedEntry, Num_ShowAllEntries: integer;
begin
  {
  Review the purposes for "Editing Layout". Make sure to:
  - Select "All entries" in one (and only one) panel                                       (..."sEntry13")
  - Do not select more than one panel with "Newest / oldest / last selected entry"         (..."sEntry12")
  - Select "All entries" or "Newest / oldest / last selected entry" for panel C            (..."sEntry13" y "sEntry12")
  - Specify a tag for each "Vinculated to the tag[s]:" option                              (...QL: "sEntry10"   EL: "sEntry11")
  }

   Result:= False;

   Num_ShowSelectedEntry:= 0;
   Num_ShowAllEntries:= 0;

   with myNoteAdvOptions do begin
      for pnl := Low(TNEntriesMainPanel) to High(TNEntriesMainPanel) do begin
         case DefaultUseForEditingLayout[pnl] of
             pnuShowVinculatedWithTags: if (pnl = pnCenter) or (VinculatedTagsForEditingLayout[pnl] = nil) then exit;
             pnuShowSelectedEntry:      inc(Num_ShowSelectedEntry);
             pnuShowAllEntries:         inc(Num_ShowAllEntries);
             pnuHidePanel:              if (pnl = pnCenter) then exit;
         end;
      end;

      if (Num_ShowAllEntries = 1) and (Num_ShowSelectedEntry <= 1) then
         Result:= True;
   end;

end;


procedure TForm_Defaults.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, Pages.ActivePage.HelpContext );
end;

procedure TForm_Defaults.Edit_FolderNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ( Key = KNTLINK_SEPARATOR ) then
    Key := #0;
end; // Edit_NoteNameKeyPress



procedure TForm_Defaults.BitBtn_FolderHelpClick(Sender: TObject);
begin
  App.InfoPopup(GetRS(sDef31));
end;

procedure TForm_Defaults.BitBtn_QLClick(Sender: TObject);
begin
  var OptAllEntries: string := GetRS(sEntry13);
  var OptSelectedEntry: string := GetRS(sEntry12);
  var OptVincTags: string := GetRS(sEntry10);

  App.InfoPopup(Format( GetRS(sEntry20), [OptAllEntries, OptSelectedEntry, OptAllEntries, OptSelectedEntry, OptVincTags]) );
end;

procedure TForm_Defaults.BitBtn_ELClick(Sender: TObject);
begin
  var OptAllEntries: string := GetRS(sEntry13);
  var OptSelectedEntry: string := GetRS(sEntry12);
  var OptVincTags: string := GetRS(sEntry11);

  App.InfoPopup(Format( GetRS(sEntry20), [OptAllEntries, OptSelectedEntry, OptAllEntries, OptSelectedEntry, OptVincTags]) );
end;

procedure TForm_Defaults.BitBtn_FolderChromeHelpClick(Sender: TObject);
begin
  App.InfoPopup(GetRS(sDef32));
end;

procedure TForm_Defaults.BitBtn_TreeChromeHelpClick(Sender: TObject);
begin
  App.InfoPopup(GetRS(sDef33));
end;



end.
