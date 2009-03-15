
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

unit kn_Defaults;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  gf_misc, kn_Const, kn_Info,
  kn_Chest, Spin, Buttons,
  ExtCtrls, ComCtrls95,
  kn_NoteObj, gf_strings,
  gf_Const, kn_Ini,
  cmpGFXComboBox, Placemnt,
  Menus, gf_LangCombo, LCCombo;

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
    Label1: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    CB_WordWrap: TCheckBox;
    CB_URLDetect: TCheckBox;
    CB_UseTabChar: TCheckBox;
    Spin_TabSize: TSpinEdit;
    Edit_NoteName: TComboBox;
    Combo_Icons: TGFXComboBox;
    GBox_Tree: TGroupBox;
    BTN_Font: TBitBtn;
    BTN_Color: TBitBtn;
    BTN_Defaults: TBitBtn;
    Edit_Sample: TEdit;
    Bevel2: TBevel;
    CB_TreeCheck: TCheckBox;
    Bevel4: TBevel;
    Label5: TLabel;
    Edit_NodeName: TComboBox;
    CB_AutoNumberNodes: TCheckBox;
    Bevel5: TBevel;
    BitBtn_TknHlp: TBitBtn;
    Label_EditorFonts: TLabel;
    Label_TreeFonts: TLabel;
    Label_TreeSettings: TLabel;
    Label_EditorSettings: TLabel;
    FormPlacement: TFormPlacement;
    CB_Vertical: TCheckBox;
    Tab_Adv: TTab95Sheet;
    GroupBox1: TGroupBox;
    CB_SaveAsDef: TCheckBox;
    LB_SaveAsDef: TLabel;
    Button_Help: TButton;
    Label14: TLabel;
    LB_PlainText: TLabel;
    CB_PlainText: TCheckBox;
    Label2: TLabel;
    Combo_TreeImages: TComboBox;
    Combo_DefEdLang: TLanguagesCombo;
    CB_HideChecked: TCheckBox;
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
    procedure AM_SaveAsDefClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
    procedure Edit_NoteNameKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
  public
    { Public declarations }
    Initializing : boolean;
    Action : TPropertiesAction;
    OK_Click : boolean;
    StartWithEditorTab : boolean;
    NoteKind : TNoteType;
    DefaultsFN : string;

    myEditorChrome : TChrome;
    myEditorProperties : TNoteEditorProperties;
    myTabProperties : TNoteTabProperties;

    myTreeChrome : TChrome;
    myTreeProperties : TNoteTreeProperties;

    myTabNameHistory : string;
    myHistoryCnt : integer;
    myNodeNameHistory : string;

    mySaveFileDefaults : boolean;
    myCurrentFileName : string;

    myNoteIsReadOnly : boolean; // prevent changes

    procedure FormToProps;
    procedure PropsToForm;
    procedure UpdateSampleFont;

  end;


implementation

{$R *.DFM}

resourcestring
  STR_01 = 'Note Properties: %s';
  STR_02 = 'Close';
  STR_03 = 'Note is Read-Only: cannot change properties';
  STR_04 = ' [RO]';
  STR_05 = ' View properties for current note ';
  STR_06 = ' Change properties for current note ';
  STR_07 = '&Save as default for "%s"';
  STR_08 = 'Defaults for ';
  STR_09 = ' Defaults for notes in ';
  STR_10 = 'Defaults for all files';
  STR_11 = ' Change default properties for all new notes ';
  STR_12 = 'Note name cannot be blank. Please enter a name.';
  STR_13 = 'Note name cannot contain the "%s" character';
  STR_14 = 'Node name cannot contain the "%s" character';
  STR_15 = 'OK to reset Editor font and color settings to default values?';
  STR_16 = 'OK to reset Tree font and color settings to default values?';
  STR_17 = 'Tokens for autonaming tree nodes:';
  STR_18 = '(must be UPPERCASE)';
  STR_19 = ' = current date';
  STR_20 = ' = current time';
  STR_21 = ' = total number of nodes';
  STR_22 = ' = new node''s level';
  STR_23 = ' = new node''s index';
  STR_24 = ' = new node''s absolute index';
  STR_25 = ' = parent node''s name';
  STR_26 = ' = name of active note';
  STR_27 = ' = name of currently open file';
  STR_28 = '<no icon>';

procedure TForm_Defaults.FormCreate(Sender: TObject);
var
  i : integer;
  nodeicn : TNodeIconKind;
begin
  Initializing := true;
  Pages.Visible := false; // to avoid flicker
  Pages.TabInactiveColor := _GF_CLWINDOW;
  myNoteIsReadOnly := false;

  mySaveFileDefaults := false;
  myCurrentFileName := '';

  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  OK_Click := false;
  Action := low( TPropertiesAction );
  StartWithEditorTab := true;
  NoteKind := ntRTF;
  DefaultsFN := normalFN( changefileext( Application.ExeName, ext_DEFAULTS ));

  myTabNameHistory := '';
  myHistoryCnt := DEFAULT_HISTORY_COUNT;

  InitializeChrome( myEditorChrome );
  InitializeNoteEditorProperties( myEditorProperties );
  InitializeNoteTabProperties( myTabProperties );

  myNodeNameHistory := '';
  InitializeChrome( myTreeChrome );
  InitializeNoteTreeProperties( myTreeProperties );

  Edit_NoteName.MaxLength := TABNOTE_NAME_LENGTH;
  Edit_NodeName.MaxLength := TREENODE_NAME_LENGTH;

  for nodeicn := low( nodeicn ) to high( nodeicn ) do
    Combo_TreeImages.Items.Add( NODE_ICON_KINDS[nodeicn] );
  Combo_TreeImages.ItemIndex := 1;

  Combo_Icons.ImageList := Chest.IMG_Categories;
  Combo_Icons.AddItem( STR_28, -1 );
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Combo_Icons.AddItem( ' - ' + inttostr( succ( i )), i );
  Combo_Icons.ItemIndex := 0;
end; // CREATE

procedure TForm_Defaults.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;
  Initializing := false;


  try

    case Action of
      propThisNote : begin
        // Tab_Adv.TabVisible := false;

        LB_SaveAsDef.Enabled := false;
        CB_SaveAsDef.Enabled := false;

        LB_PlainText.Enabled := true;
        CB_PlainText.Enabled := true;

        Caption := Format( STR_01, [RemoveAccelChar( myTabProperties.Name )] );

        if myNoteIsReadOnly then
        begin
          Button_OK.ModalResult := mrCancel;
          Button_OK.Caption := STR_02;
          Button_OK.Hint := STR_03;
          Button_Cancel.Visible := false;
          Caption := Caption + STR_04;
          GBox_Note.Caption := STR_05;
        end
        else
        begin
          GBox_Note.Caption := STR_06;
        end;
      end;
      propDefaults : begin

        LB_PlainText.Enabled := false;
        CB_PlainText.Enabled := false;

        if ( myCurrentFileName <> '' ) then
        begin
          CB_SaveAsDef.Caption := Format( STR_07, [myCurrentFileName] );
          // Tab_Adv.TabVisible := true;
          LB_SaveAsDef.Enabled := true;
          CB_SaveAsDef.Enabled := true;
        end
        else
        begin
          // Tab_Adv.TabVisible := false;
          LB_SaveAsDef.Enabled := false;
          CB_SaveAsDef.Enabled := false;
        end;
        if ( CB_SaveAsDef.Enabled and mySaveFileDefaults ) then
        begin
          CB_SaveAsDef.Checked := true;
          Caption := STR_08 + myCurrentFileName;
          GBox_Note.Caption := STR_09 + myCurrentFileName + ' ';
        end
        else
        begin
          Caption := STR_10;
          GBox_Note.Caption := STR_11;
        end;
      end;
    end;

    Edit_NoteName.Items.BeginUpdate;
    try
      DelimTextToStrs( Edit_NoteName.Items, myTabNameHistory, HISTORY_SEPARATOR );
    finally
      Edit_NoteName.Items.EndUpdate;
    end;

    GBox_Tree.Caption := GBox_Note.Caption;
    BitBtn_TknHlp.OnClick := BitBtn_TknHlpClick;
    Tab_Tree.TabVisible := (( Action in [propDefaults] ) or ( NoteKind = ntTree ));

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
      Edit_NoteName.SetFocus;
      Edit_NoteName.SelectAll;
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
begin
  if OK_Click then
  begin
    OK_Click := false;

    if ( Edit_NoteName.Text = '' ) then
    begin
      CanClose := false;
      messagedlg( STR_12, mtError, [mbOK], 0 );
      Pages.ActivePage := Tab_Main;
      Edit_NoteName.SetFocus;
      exit;
    end;

    if ( pos( KNTLINK_SEPARATOR, Edit_NoteName.Text ) > 0 ) then
    begin
      CanClose := false;
      messagedlg( Format(
        STR_13,
        [KNTLINK_SEPARATOR]
      ), mtError, [mbOK], 0 );
      Pages.ActivePage := Tab_Main;
      Edit_NoteName.SetFocus;
      exit;
    end;

    if ( pos( KNTLINK_SEPARATOR, Edit_NodeName.Text ) > 0 ) then
    begin
      CanClose := false;
      messagedlg( Format(
        STR_14,
        [KNTLINK_SEPARATOR]
      ), mtError, [mbOK], 0 );
      Pages.ActivePage := Tab_Tree;
      Edit_NodeName.SetFocus;
      exit;
    end;

    myTabNameHistory := ANSIQuotedStr( Edit_NoteName.Text, '"' );
    for i := 0 to pred( Edit_NoteName.Items.Count ) do
    begin
      if ( i >= myHistoryCnt ) then break;
      if (( Edit_NoteName.Items[i] <> Edit_NoteName.Text ) and ( Edit_NoteName.Items[i] <> '' )) then
        myTabNameHistory :=  myTabNameHistory + HISTORY_SEPARATOR + ANSIQuotedStr( Edit_NoteName.Items[i], '"' );
    end;
    if ( Edit_NodeName.Text <> '' ) then
      myNodeNameHistory := ANSIQuotedStr( Edit_NodeName.Text, '"' )
    else
      myNodeNameHistory := '';
    for i := 0 to pred( Edit_NodeName.Items.Count ) do
    begin
      if ( i >= myHistoryCnt ) then break;
      if (( Edit_NodeName.Items[i] <> Edit_NodeName.Text ) and ( Edit_NodeName.Items[i] <> '' )) then
        myNodeNameHistory :=  myNodeNameHistory + HISTORY_SEPARATOR + ANSIQuotedStr( Edit_NodeName.Items[i], '"' );
    end;
    FormToProps;
  end;
  OK_Click := false;
end; // CLOSEQUERY

procedure TForm_Defaults.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not
      ( Combo_Icons.DroppedDown or Edit_NoteName.DroppedDown or Edit_NodeName.DroppedDown ))) then
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
begin

  with myTabProperties do
  begin
    Name := trim( Edit_NoteName.Text );
    ImageIndex := pred( Combo_Icons.ItemIndex );
  end;

  with myEditorProperties do
  begin
    TabSize := Spin_TabSize.Value;
    PlainText := CB_PlainText.Checked;
    URLDetect := CB_URLDetect.Checked;
    UseTabChar := CB_UseTabChar.Checked;
    WordWrap := CB_WordWrap.Checked;
  end;

  if (( Action in [propDefaults] ) or ( NoteKind = ntTree )) then
  begin
    with myTreeProperties do
    begin
      if ( trim( Edit_NodeName.Text ) <> '' ) then
        DefaultName := trim( Edit_NodeName.Text );
      IconKind := TNodeIconKind( Combo_TreeImages.ItemIndex );
      Checkboxes := CB_TreeCheck.Checked;
      VerticalLayout := CB_Vertical.Checked;
      AutoNumberNodes := CB_AutoNumberNodes.Checked;
      HideChecked:= CB_HideChecked.Checked;
    end;
  end;

  with myEditorChrome do
  begin
    Language := Combo_DefEdLang.Language;
  end;

end; // FormToProps

procedure TForm_Defaults.PropsToForm;
begin

  with myTabProperties do
  begin
    Edit_NoteName.Text := Name;
    Combo_Icons.ItemIndex := succ( ImageIndex );
  end;

  with myEditorProperties do
  begin
    Spin_TabSize.Value := TabSize;
    CB_PlainText.Checked := PlainText;
    CB_URLDetect.Checked := URLDetect;
    CB_UseTabChar.Checked := UseTabChar;
    CB_WordWrap.Checked := WordWrap;
  end;

  if (( Action in [propDefaults] ) or ( NoteKind = ntTree )) then
  begin
    with myTreeProperties do
    begin
      Edit_NodeName.Text := DefaultName;
      Combo_TreeImages.ItemIndex := ord( IconKind );
      CB_TreeCheck.Checked := Checkboxes;
      CB_Vertical.Checked := VerticalLayout;
      CB_AutoNumberNodes.Checked := AutoNumberNodes;
      CB_HideChecked.Checked := HideChecked;          // [dpv]
    end;
  end;

  with myEditorChrome do
  begin
    Combo_DefEdLang.Language := Language;
  end;

end; // PropsToForm

procedure TForm_Defaults.UpdateSampleFont;
begin

  if Pages.ActivePage = Tab_Adv then
  begin
    BTN_Font.Visible := false;
    BTN_Color.Visible := false;
    BTN_Defaults.Visible := false;
    Edit_Sample.Visible := false;
  end
  else
  begin

    BTN_Font.Visible := true;
    BTN_Color.Visible := true;
    BTN_Defaults.Visible := true;
    Edit_Sample.Visible := true;

    if Pages.ActivePage = Tab_Tree then
    begin
      Edit_Sample.Color := myTreeChrome.BGColor;
      FontInfoToFont( myTreeChrome.Font, Edit_Sample.Font );
      with myTreeChrome do
      begin
        Edit_Sample.Text := Font.Name + #32 + inttostr( Font.Size ) + ' pt ' + FontStyleToStr( Font.Style );
      end;
    end
    else
    begin
      Edit_Sample.Color := myEditorChrome.BGColor;
      FontInfoToFont( myEditorChrome.Font, Edit_Sample.Font );
      with myEditorChrome do
      begin
        Edit_Sample.Text := Font.Name + #32 + inttostr( Font.Size ) + ' pt ' + FontStyleToStr( Font.Style );
      end;
    end;

  end;
end; // UpdateSampleFont

procedure TForm_Defaults.BTN_FontClick(Sender: TObject);
begin
  if ( Pages.ActivePage = Tab_Main ) then
  begin
    FontDlg.Options := FontDlg.Options + [fdEffects];
    FontInfoToFont( myEditorChrome.Font, FontDlg.Font );
    if FontDlg.Execute then
      FontToFontInfo( FontDlg.Font, myEditorChrome.Font );
  end
  else
  begin
    if ( not _ALLOW_TREE_FONT_COLOR ) then
      FontDlg.Options := FontDlg.Options - [fdEffects];
    FontInfoToFont( myTreeChrome.Font, FontDlg.Font );
    if FontDlg.Execute then
      FontToFontInfo( FontDlg.Font, myTreeChrome.Font );
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
    if ( messagedlg( STR_15, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

    InitializeChrome( myEditorChrome );

    // If editing properties for active note, restore defaults from
    // keynote.def file rather than original factory defaults,
    // unless SHIFT is pressed
    if (( Action = propThisNote ) and ( not ShiftWasDown ) and fileexists( DefaultsFN )) then
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
    if ( messagedlg( STR_16, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
    InitializeChrome( myTreeChrome );
    InitializeChrome( tmpChrome );

    if (( Action = propThisNote ) and ( not ShiftWasDown ) and fileexists( DefaultsFN )) then
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
begin
  UpdateSampleFont;
end;


procedure TForm_Defaults.BitBtn_TknHlpClick(Sender: TObject);
begin
  messagedlg(
    STR_17 +#13+
    STR_18 +#13#13+
     NODEINSDATE  + STR_19 +#13+
     NODEINSTIME  + STR_20 +#13+
     NODECOUNT    + STR_21 +#13+
     NODELEVEL    + STR_22 +#13+
     NODEINDEX    + STR_23 +#13+
     NODEABSINDEX + STR_24 +#13+
     NODEPARENT   + STR_25 +#13+
     NODENOTENAME + STR_26 +#13+
     NODEFILENAME + STR_27,

    mtInformation, [mbOK], 0
  );
end;

procedure TForm_Defaults.CB_UseTabCharClick(Sender: TObject);
begin
  Label_TabSize.Enabled := ( not CB_UseTabChar.Checked );
  Spin_TabSize.Enabled := Label_TabSize.Enabled;

end;


procedure TForm_Defaults.AM_SaveAsDefClick(Sender: TObject);
begin
  mySaveFileDefaults := CB_SaveAsDef.Checked;
end;


procedure TForm_Defaults.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, Pages.ActivePage.HelpContext );
end;

procedure TForm_Defaults.Edit_NoteNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ( Key = KNTLINK_SEPARATOR ) then
    Key := #0;
end; // Edit_NoteNameKeyPress




end.
