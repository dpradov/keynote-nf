unit kn_Defaults;

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
   kn_Const
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
    Label1: TLabel;
    Label4: TLabel;
    CB_WordWrap: TCheckBox;
    CB_URLDetect: TCheckBox;
    CB_UseTabChar: TCheckBox;
    Spin_TabSize: TSpinEdit;
    Edit_FolderName: TComboBox;
    Combo_Icons: TGFXComboBox;
    GBox_Tree: TGroupBox;
    BTN_Font: TBitBtn;
    BTN_Color: TBitBtn;
    BTN_Defaults: TBitBtn;
    Edit_Sample: TEdit;
    CB_TreeCheck: TCheckBox;
    Label5: TLabel;
    Edit_NodeName: TComboBox;
    CB_AutoNumberNodes: TCheckBox;
    BitBtn_TknHlp: TBitBtn;
    Label_EditorFonts: TLabel;
    Label_TreeSettings: TLabel;
    Label_EditorSettings: TLabel;
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
    Label6: TLabel;
    BitBtn_FolderHelp: TBitBtn;
    BitBtn_FolderChromeHelp: TBitBtn;
    BitBtn_TreeChromeHelp: TBitBtn;
    CB_InheritBGColor: TCheckBox;
    LB_PlainText: TLabel;
    CB_TreeChrome_AllNotes: TCheckBox;
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
    procedure CB_PlainTextClick(Sender: TObject);
    procedure LB_PlainTextClick(Sender: TObject);
    procedure BitBtn_FolderChromeHelpClick(Sender: TObject);
    procedure BitBtn_FolderHelpClick(Sender: TObject);
    procedure BitBtn_TreeChromeHelpClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;

  private
    { Private declarations }
    fDefaultZoom: integer;
    fOriginalAction : TPropertiesAction;

    procedure CheckScope;
    procedure CheckZoomValue;

  public
    { Public declarations }
    Initializing : boolean;
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
   kn_Ini
  ;

{$R *.DFM}

resourcestring
  STR_Ok = 'OK';
  STR_Ok_Hint = 'Accept changes and close dialog box';
  STR_01 = 'Folder Properties: %s';
  STR_02 = 'Close';
  STR_03 = 'Folder is Read-Only: cannot change properties';
  STR_04 = ' [RO]';
  STR_05 = ' View properties for current folder ';
  STR_06 = 'Change properties for current folder';
  STR_07 = '&Save as default for "%s"';
  STR_08 = 'Defaults for ';
  STR_09 = 'Change Defaults for NEW folders in THIS FILE';
  STR_10 = 'Defaults for all files';
  STR_11 = 'Change default properties for all NEW folders';
  STR_12 = 'Folder name cannot be blank. Please enter a name.';
  STR_13 = 'Folder name cannot contain the "%s" character';
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
  STR_26 = ' = name of active folder';
  STR_27 = ' = name of currently open file';
  STR_28 = '<no icon>';
  STR_29 = 'Invalid zoom ratio: ';
  STR_30 = ' (and apply to "%s" folder)';
  STR_31 = '&Plain text only  ';
  STR_32 = '(do not save formatting information)';
  STR_33 = '(ALL FORMATTING will be REMOVED)';

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
  Combo_Icons.AddItem( STR_28, -1 );
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Combo_Icons.AddItem( ' - ' + inttostr( succ( i )), i );
  Combo_Icons.ItemIndex := 0;

  LB_PlainText.Caption:= STR_31 + STR_32;
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
    CB_PlainText.Enabled := not SaveDefaults;
    LB_PlainText.Enabled := not SaveDefaults;

    if SaveDefaults then begin
       Action:= propDefaults;
       if CB_SaveAsDef.Checked then
          LB_Scope.Caption := STR_09
       else
          LB_Scope.Caption := STR_11;

    end
    else begin
       Action:= propThisFolder;
       if myNoteIsReadOnly then
          LB_Scope.Caption := STR_05
       else
          LB_Scope.Caption := STR_06;
    end;


    Button_OK.Hint := STR_Ok_Hint;

    if (fOriginalAction = propThisFolder) and (myNoteIsReadOnly) then begin
       if Action = propThisFolder then begin
          Button_OK.ModalResult := mrCancel;
          Button_OK.Caption := STR_02;
          Button_OK.Hint := STR_03;
       end
       else begin
          Button_OK.ModalResult := mrOk;
          Button_OK.Caption := STR_Ok;
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
  if ( not Initializing ) then exit;
  Initializing := false;

  fOriginalAction:= Action;

  try

    if myCurrentFileName <> '' then
       CB_SaveAsDef.Caption := Format( STR_07, [myCurrentFileName] );

    case Action of
      propThisFolder : begin
        CB_SaveDefaults.Enabled := true;
        CB_SaveDefaults.Checked := false;
        CB_SaveAsDef.Checked := False;

        tabName:= RemoveAccelChar( myTabProperties.Name );

        Caption := Format( STR_01, [tabName] );
        if myNoteIsReadOnly then
           Caption := Caption + STR_04
        else
           CB_SaveDefaults.Caption := CB_SaveDefaults.Caption + Format( STR_30, [tabName] );
      end;

      propDefaults : begin
        CB_SaveDefaults.Enabled := false;
        CB_SaveDefaults.Checked := true;
        CB_SaveAsDef.Checked := mySaveFileDefaults;

       if mySaveFileDefaults then
          Caption := STR_08 + myCurrentFileName
       else
          Caption := STR_10;

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
begin
  if OK_Click then
  begin
    OK_Click := false;

    if ( Edit_FolderName.Text = '' ) then
    begin
      CanClose := false;
      messagedlg( STR_12, mtError, [mbOK], 0 );
      Pages.ActivePage := Tab_Main;
      Edit_FolderName.SetFocus;
      exit;
    end;

    if ( pos( KNTLINK_SEPARATOR, Edit_FolderName.Text ) > 0 ) then
    begin
      CanClose := false;
      messagedlg( Format(
        STR_13,
        [KNTLINK_SEPARATOR]
      ), mtError, [mbOK], 0 );
      Pages.ActivePage := Tab_Main;
      Edit_FolderName.SetFocus;
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
begin

  with myTabProperties do
  begin
    Name := trim( Edit_FolderName.Text );
    ImageIndex := pred( Combo_Icons.ItemIndex );
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
  end;

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

  with myEditorChrome do
  begin
    Language := Combo_DefEdLang.Language;
  end;

  // myInheritBGColor:= CB_InheritBGColor.Checked;      // -> To modify in Global options form
  ApplyTreeChromeToAllFolders:= CB_TreeChrome_AllNotes.Checked;

end; // FormToProps

procedure TForm_Defaults.PropsToForm;
begin

  with myTabProperties do
  begin
    Edit_FolderName.Text := Name;
    Combo_Icons.ItemIndex := succ( ImageIndex );
  end;

  with myEditorProperties do
  begin
    Spin_TabSize.Value := TabSize;
    CB_PlainText.Checked := PlainText;
    CB_URLDetect.Checked := URLDetect;
    CB_UseTabChar.Checked := UseTabChar;
    CB_WordWrap.Checked := WordWrap;
    CB_Zoom.Text:= IntToStr(DefaultZoom);
  end;

  with myTreeProperties do
  begin
    Edit_NodeName.Text := DefaultName;
    Combo_TreeImages.ItemIndex := ord( IconKind );
    CB_TreeCheck.Checked := Checkboxes;
    CB_Vertical.Checked := VerticalLayout;
    CB_AutoNumberNodes.Checked := AutoNumberNodes;
    CB_HideChecked.Checked := HideChecked;          // [dpv]
  end;

  with myEditorChrome do
  begin
    Combo_DefEdLang.Language := Language;
  end;

  CB_InheritBGColor.Checked:= myInheritBGColor;

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
    if ( messagedlg( STR_15, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

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
    if ( messagedlg( STR_16, mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
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

procedure TForm_Defaults.CB_PlainTextClick(Sender: TObject);
begin
   if CB_PlainText.Checked and (fOriginalAction = propThisFolder) and (not myNoteIsReadOnly) and (not myEditorProperties.PlainText) then begin
      LB_PlainText.Caption:= STR_31 + STR_33;       // warning
      LB_PlainText.Font.Color:= clRed;
   end
   else begin
      LB_PlainText.Caption:= STR_31 + STR_32;
      LB_PlainText.Font.Color:= clBlack;
   end;
end;

procedure TForm_Defaults.LB_PlainTextClick(Sender: TObject);
begin
  CB_PlainText.Checked:= not CB_PlainText.Checked;
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
        messagedlg( STR_29 + E.Message, mtError, [mbOK], 0 );
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
var
   msg: string;
begin
  msg:= 'REMEMBER:' + #13#13 +
        '- Folder settings apply only to NEW notes, except:' + #13 +
        '   - ''Plain note only'': modifies ALL the notes' + #13 +
        '   - ''WordWrap'': affects ALL the notes' + #13 +
        '       (not explicitly set WordWrap previously)' + #13#13 +
        '>> More info in Help File (F1)'
        ;

  messagedlg(msg , mtInformation, [mbOK], 0  );
end;

procedure TForm_Defaults.BitBtn_FolderChromeHelpClick(Sender: TObject);
var
   msg: string;
begin
  msg:= 'REMEMBER:' + #13#13 +
        '- Font change affect only to NEW nodes (all if Plain note)' + #13 +
        '- BG Color depends on ''Inherit BG color from active node'':' + #13 +
        '   - If set, background color of selected node is shown' + #13 +
        '     (=> BG color of its new child nodes)' + #13 +
        '   - If not set, default BG color for all NEW nodes is shown' + #13 +
        '   * To edit this option -> F5 | General settings| Rich Text editor' + #13#13 +
        '- BG Color can be changed for ALL nodes in a Folder:' + #13 +
        '    [Shift] + "Format | Background color"' + #13#13 +
        '>> More info in Help File (F1)'
        ;

  messagedlg(msg , mtInformation, [mbOK], 0  );
end;


procedure TForm_Defaults.BitBtn_TreeChromeHelpClick(Sender: TObject);
var
   msg: string;
begin
  msg:= 'REMEMBER:' + #13#13 +
        '- BG Color sets backgroud color for the Tree Panel and' + #13 +
        '  default BG color of tree nodes' + #13 +
        '- Previous changes to individual nodes won''t be affected' + #13 +
        '- ''Inherit properties from active node'' option is' + #13 +
        '  considered in NEW nodes' + #13 +
        '- Font and BG color can be changed for ALL tree panels at once:' + #13 +
        '    "Apply to ALL folders"' + #13#13 +
        '- Note: ''Inherit BG color from active node'' option does NOT' + #13 +
        '  affect (refers to Editor) ' + #13#13 +
        '>> More info in Help File (F1)'
        ;

  messagedlg(msg , mtInformation, [mbOK], 0  );
end;



end.
