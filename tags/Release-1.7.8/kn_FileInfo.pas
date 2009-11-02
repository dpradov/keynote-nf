
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

unit kn_FileInfo;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls,
  ComCtrls, gf_misc, gf_files,
  kn_Info, kn_NoteObj, kn_FileObj,
  kn_Const, ExtCtrls, Placemnt,
  ShellAPI, Mask, ToolEdit, TntStdCtrls, TntDialogs, TB97Ctls;

type
  TForm_FileInfo = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Pages: TPageControl;
    Tab_Main: TTabSheet;
    GroupBox1: TTntGroupBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Label3: TTntLabel;
    Label4: TTntLabel;
    Label_Created: TTntLabel;
    Label_Modified: TTntLabel;
    Label5: TTntLabel;
    Label_Count: TTntLabel;
    Edit_Comment: TTntEdit;
    Label7: TTntLabel;
    Label_FileSize: TTntLabel;
    Label11: TTntLabel;
    Label12: TTntLabel;
    Label_FileNotFound: TTntLabel;
    Label6: TTntLabel;
    Edit_Description: TTntEdit;
    Label8: TTntLabel;
    Combo_Format: TTntComboBox;
    Bevel2: TBevel;
    Tab_Pass: TTabSheet;
    GroupBox2: TTntGroupBox;
    Label_Confirm: TTntLabel;
    Label_Pass: TTntLabel;
    Label_Method: TTntLabel;
    Edit_Confirm: TTntEdit;
    Edit_Pass: TTntEdit;
    Combo_Method: TTntComboBox;
    Button_SetPass: TTntButton;
    Label_EnterPass: TTntLabel;
    Tab_Settings: TTabSheet;
    GroupBox3: TTntGroupBox;
    Bevel1: TBevel;
    CB_AsReadOnly: TTntCheckBox;
    Label_IsReadOnly: TTntLabel;
    FormPlacement: TFormPlacement;
    CB_HidePass: TTntCheckBox;
    Edit_FileName: TTntEdit;
    Tab_Icons: TTabSheet;
    GroupBox4: TTntGroupBox;
    CB_ShowTabIcons: TTntCheckBox;
    CB_TrayIcon: TTntCheckBox;
    Image_TrayIcon: TImage;
    Bevel3: TBevel;
    RB_TabImgDefault: TTntRadioButton;
    RB_TabImgBuiltIn: TTntRadioButton;
    RB_TabImgOther: TTntRadioButton;
    Button_System: TTntButton;
    Button_Help: TTntButton;
    LB_RTF3: TTntLabel;
    CB_NoMultiBackup: TTntCheckBox;
    TB_OpenDlgTrayIcon: TToolbarButton97;
    Edit_TrayIcon: TTntEdit;
    TB_OpenDlgTabImg: TToolbarButton97;
    Edit_TabImg: TTntEdit;
    TntLabel1: TTntLabel;
    Combo_CompressLevel: TTntComboBox;
    Bevel4: TBevel;
    procedure TB_OpenDlgTrayIconClick(Sender: TObject);
    procedure TB_OpenDlgTabImgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Combo_MethodChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Combo_FormatChange(Sender: TObject);
    procedure Button_SetPassClick(Sender: TObject);
    procedure Edit_PassChange(Sender: TObject);
    procedure CheckBox_AsReadOnlyClick(Sender: TObject);
    procedure CheckBox_HidePassClick(Sender: TObject);
    procedure CheckBox_TrayIconClick(Sender: TObject);
    procedure CheckBox_ShowTabIconsClick(Sender: TObject);
    procedure RB_TabImgOtherClick(Sender: TObject);
    procedure Button_SystemClick(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myNotes : TNoteFile;
    OK_Click : boolean;
    PassphraseChanged : boolean;
    MinPassLen : integer;
    HidePassText : boolean;

    function Verify : boolean;
    procedure EnablePassControls;
  end;

const
  _FILE_TABIMAGES_SELECTION_CHANGED : boolean = false;

implementation
uses kn_main, gf_miscvcl, TntClasses, TntSysUtils, ZLibEx;

{$R *.DFM}

resourcestring
  STR_01 = ' file';
  STR_02 = 'File properties: ';
  STR_03 = ' bytes';
  STR_04 = '(file not saved)';
  STR_05 = 'never';
  STR_06 = 'Open "%s" as &Read-Only';
  STR_07 = '(none)';
  STR_08 = 'You chose to save the file using DartNotes format. However, this file contains tree-type notes, which are incompatible with DartNotes.' + #13#13+
                     'If you click OK, the file will revert to using KeyNote format. Continue?';
  STR_09 = 'The passphrase you entered is too short: Minimum passphrase length is %d characters';
  STR_10 = 'The passphrases you entered do not match. Please enter the exact same passphrase twice.';
  STR_11 = 'You chose to encrypt a file that contains virtual nodes. ' +
                     'Note that the disk files linked to virtual nodes ' +
                     'will NOT be encrypted.' + #13#13 + 'Continue?';
  STR_12 = 'File "%s" was open in READ-ONLY mode. If you uncheck this box, the read-only mode will be turned OFF. Continue?';


procedure TForm_FileInfo.FormCreate(Sender: TObject);
var
  cm : TCryptMethod;
  ff : TNoteFileFormat;
  cl: TZCompressionLevel;
begin
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  Tab_Pass.TabVisible := false; // [x] while not implemented
  HidePassText := true;

  Label_Pass.Enabled := false;
  Label_Confirm.Enabled := false;
  Label_Method.Enabled := false;
  Edit_Pass.Enabled := false;
  Edit_Confirm.Enabled := false;
  Combo_Method.Enabled := false;
  CB_HidePass.Enabled := false;
  Label_EnterPass.Visible := false;

  Pages.ActivePage := Tab_Main;
  myNotes := nil;
  PassphraseChanged := false;
  MinPassLen := MIN_PASS_LEN;

  for cm := low( TCryptMethod ) to high( TCryptMethod ) do
    Combo_Method.Items.Add( CRYPT_METHOD_NAMES[cm] );
  Combo_Method.ItemIndex := ord( low( TCryptMethod ));
  for ff := low( TNoteFileFormat ) to high( TNoteFileFormat ) do
    Combo_Format.Items.Add( FILE_FORMAT_NAMES[ff] + STR_01 );
  Combo_Format.ItemIndex := 0;
  for cl := low( TZCompressionLevel ) to high( TZCompressionLevel ) do
    Combo_CompressLevel.Items.Add( FILE_COMPRESSION_LEVEL[cl] );
  Combo_CompressLevel.ItemIndex := 0;
  Edit_Comment.MaxLength := MAX_COMMENT_LENGTH;
  Edit_Description.MaxLength := MAX_COMMENT_LENGTH;
  OK_Click := false;
end; // CREATE

procedure TForm_FileInfo.FormActivate(Sender: TObject);
var
  fs : longint;
begin
  if assigned( myNotes ) then
  begin
    // TAB_MAIN
    Caption := STR_02 + WideExtractFilename( myNotes.FileName );
    Edit_FileName.Text := myNotes.FileName;
    Label_Count.Caption := inttostr( myNotes.NoteCount );
    Edit_Comment.Text := myNotes.Comment;
    Edit_Description.Text := myNotes.Description;
    label_Created.Caption := FormatDateTime( LongDateFormat + #32 + LongTimeFormat, myNotes.DateCreated );
    Combo_Format.ItemIndex := ord( myNotes.FileFormat );
    if WideFileexists( myNotes.FileName ) then
    begin
      fs := GetFileSize( myNotes.FileName );
      if ( fs < 1025 ) then
        Label_FileSize.Caption := inttostr( fs ) + STR_03
      else
        Label_FileSize.Caption := inttostr( fs DIV 1024 ) + ' Kb';
      label_Modified.Caption := FormatDateTime( LongDateFormat + #32 + LongTimeFormat, GetFileDateStamp( myNotes.FileName ));
      if myNotes.SavedWithRichEdit3 then
      begin
        // LB_RTF3.Font.Color := clRed;
        LB_RTF3.Visible := true;
      end;
    end
    else
    begin
      Label_FileSize.Caption := STR_04;
      Edit_FileName.Visible := false;
      Label_FileNotFound.Visible := true;
      label_Modified.Caption := STR_05;
    end;
    CB_AsReadOnly.Caption := WideFormat( STR_06, [WideExtractFilename( myNotes.FileName )] );
    CB_AsReadOnly.Checked := ( myNotes.OpenAsReadOnly or myNotes.ReadOnly );
    Label_IsReadOnly.Visible := myNotes.ReadOnly;
    CB_NoMultiBackup.Checked := myNotes.NoMultiBackup;

    // tray icon stuff
    if (( myNotes.TrayIconFN <> '' ) and fileexists( myNotes.TrayIconFN )) then
    begin
      CB_TrayIcon.Checked := true;
      Edit_TrayIcon.Text := myNotes.TrayIconFN;
      Image_TrayIcon.Picture.LoadFromFile( myNotes.TrayIconFN );
    end
    else
    begin
      CB_TrayIcon.Checked := false;
    end;
    CheckBox_TrayIconClick( CB_TrayIcon );
    CB_TrayIcon.OnClick := CheckBox_TrayIconClick;

    // tab icons stuff
    CB_ShowTabIcons.Checked := myNotes.ShowTabIcons;
    if ( myNotes.TabIconsFN = '' ) then // default
    begin
      RB_TabImgDefault.Checked := true;
    end
    else
    begin
      if ( myNotes.TabIconsFN[1] = _NF_Icons_BuiltIn ) then
      begin
        RB_TabImgBuiltIn.Checked := true;
      end
      else
      begin
        Edit_TabImg.Text := myNotes.TabIconsFN;
        if fileexists( myNotes.TabIconsFN ) then
        begin
          RB_TabImgOther.Checked := true;
        end
        else
        begin
          RB_TabImgDefault.Checked := true;
        end;
      end;
    end;
    CheckBox_ShowTabIconsClick( CB_ShowTabIcons );
    RB_TabImgOtherClick( RB_TabImgOther );
    CB_ShowTabIcons.OnClick := CheckBox_ShowTabIconsClick;

  end
  else
  begin
    Edit_FileName.Text := STR_07;
    Label_Count.Caption := '0';
    Edit_Comment.Text := '';
    Edit_Description.Text := '';
    label_Created.Caption := STR_05;
    label_Modified.Caption := STR_05;
    Label_FileSize.Caption := '';
    Label_IsReadOnly.Visible := false;

    Combo_Method.ItemIndex := ord( myNotes.CryptMethod );

  end;

  if Edit_Description.Enabled then
  begin
    Edit_Description.SetFocus;
    Edit_Description.SelectAll;
  end;

  Combo_CompressLevel.ItemIndex := ord( myNotes.CompressionLevel );
  Combo_FormatChange( Combo_Format );

  CB_HidePass.Checked := HidePassText;
  CheckBox_HidePassClick( CB_HidePass );

  CB_HidePass.OnClick := CheckBox_HidePassClick;
  Combo_Method.OnChange := Combo_MethodChange;
  Combo_Format.OnChange := Combo_FormatChange;
  Edit_Pass.OnChange := Edit_PassChange;
  Edit_Confirm.OnChange := Edit_PassChange;
  CB_AsReadOnly.OnClick := CheckBox_AsReadOnlyClick;
  _FILE_TABIMAGES_SELECTION_CHANGED := false;

end; // ACTIVATE


procedure TForm_FileInfo.Combo_MethodChange(Sender: TObject);
begin
  // [x]
end;

function TForm_FileInfo.Verify : boolean;
var
  s : string;
begin
  result := true;

  if (( Combo_Format.ItemIndex = ord( nffDartNotes )) and myNotes.HasExtendedNotes ) then
  begin
    case messagedlg(STR_08, mtWarning, [mbOK,mbCancel], 0 ) of
      mrOK : Combo_Format.ItemIndex := ord( nffKeyNote );
      mrCancel : begin
        result := false;
        exit;
      end;
    end;
  end;



  if ( not ( PassphraseChanged and ( Combo_Format.ItemIndex = ord( nffEncrypted )))) then exit;

  s := '';

  if length( Edit_Pass.Text ) < MinPassLen then
  begin
    s := Format( STR_09, [MinPassLen] );
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end
  else
  if ( Edit_Pass.Text <> Edit_Confirm.Text ) then
  begin
    s := STR_10;
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end;


  if ( s <> '' ) then
  begin
    result := false;
    messagedlg( s, mtError, [mbOK], 0 );
    exit;
  end;

  if myNotes.HasVirtualNodes then
  begin
    result := ( messagedlg(STR_11, mtWarning, [mbYes,mbNo], 0 ) = mrYes );
  end;


end; // Verify;

procedure TForm_FileInfo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
  begin
    CanClose := Verify;
  end;
  OK_Click := false;
end;

procedure TForm_FileInfo.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_FileInfo.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_FileInfo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not ( Combo_Method.DroppedDown or Combo_Format.DroppedDown ))) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // KEY DOWN


procedure TForm_FileInfo.Combo_FormatChange(Sender: TObject);
begin
  Edit_Description.Enabled := Combo_Format.ItemIndex <> ord( nffDartNotes );
  Edit_Comment.Enabled := Edit_Description.Enabled;
  Combo_CompressLevel.Enabled := Combo_Format.ItemIndex = ord( nffKeyNoteZip );
  if not Combo_CompressLevel.Enabled then
     Combo_CompressLevel.ItemIndex := ord( zcNone )
  else
     if (myNotes.FileFormat <> nffKeyNoteZip) then
        Combo_CompressLevel.ItemIndex := ord( zcDefault );

  Tab_Pass.TabVisible := ( Combo_Format.ItemIndex = ord( nffEncrypted ));
  if ( Tab_Pass.TabVisible and ( myNotes.FileFormat <> nffEncrypted )) then
  begin
    // the file was NOT encrypted previously,
    // so now passphrase must be entered.
    EnablePassControls;
    PassphraseChanged := true;
    Button_SetPass.Enabled := false;
  end;
end; // Combo_FormatChange


procedure TForm_FileInfo.Button_SetPassClick(Sender: TObject);
begin
  PassphraseChanged := true;
  EnablePassControls;
  try
    Button_SetPass.Enabled := false;
    Edit_Pass.SetFocus;
  except
  end;
end;

procedure TForm_FileInfo.Edit_PassChange(Sender: TObject);
begin
  PassphraseChanged := true;
end;

procedure TForm_FileInfo.CheckBox_AsReadOnlyClick(Sender: TObject);
begin
  if assigned( myNotes ) then
  begin
    if (( not CB_AsReadOnly.Checked ) and myNotes.ReadOnly ) then
    begin
      if ( DoMessageBox( WideFormat(STR_12,[WideExtractFilename( myNotes.FileName )]), mtWarning, [mbYes,mbNo], 0 ) = mrYes ) then
        CB_AsReadOnly.OnClick := nil
      else
        CB_AsReadOnly.Checked := true;
    end;
  end;
end;


procedure TForm_FileInfo.CheckBox_HidePassClick(Sender: TObject);
begin
  HidePassText := CB_HidePass.Checked;
  if HidePassText then
    Edit_Pass.PasswordChar := '*'
  else
    Edit_Pass.PasswordChar := #0;
  Edit_Confirm.PasswordChar := Edit_Pass.PasswordChar;
end;

procedure TForm_FileInfo.EnablePassControls;
begin
  Label_Pass.Enabled := true;
  Label_Confirm.Enabled := true;
  Label_Method.Enabled := true;
  Edit_Pass.Enabled := true;
  Edit_Confirm.Enabled := true;
  Combo_Method.Enabled := true;
  CB_HidePass.Enabled := true;
  Label_EnterPass.Visible := true;
end; // EnablePassControls


procedure TForm_FileInfo.CheckBox_TrayIconClick(Sender: TObject);
begin
  Edit_TrayIcon.Enabled := CB_TrayIcon.Checked;
  TB_OpenDlgTrayIcon.Enabled := Edit_TrayIcon.Enabled;
  Image_TrayIcon.Visible := Edit_TrayIcon.Enabled;
end;

procedure TForm_FileInfo.CheckBox_ShowTabIconsClick(Sender: TObject);
begin
  RB_TabImgDefault.Enabled := CB_ShowTabIcons.Checked;
  RB_TabImgBuiltIn.Enabled := RB_TabImgDefault.Enabled;
  RB_TabImgOther.Enabled := RB_TabImgDefault.Enabled;
  Edit_TabImg.Enabled := ( RB_TabImgDefault.Enabled and RB_TabImgOther.Checked );
  TB_OpenDlgTabImg.Enabled := Edit_TabImg.Enabled;
end;

procedure TForm_FileInfo.RB_TabImgOtherClick(Sender: TObject);
begin
  _FILE_TABIMAGES_SELECTION_CHANGED := true;
  Edit_TabImg.Enabled := ( RB_TabImgOther.Enabled and RB_TabImgOther.Checked );
  TB_OpenDlgTabImg.Enabled := Edit_TabImg.Enabled;
end;


procedure TForm_FileInfo.TB_OpenDlgTabImgClick(Sender: TObject);
var
  Action: Boolean;
begin
  Form_Main.OpenDlg.Filter:= FILTER_TABIMAGES;
  Action:= Form_Main.OpenDlg.Execute;
  if Action then begin
     Edit_TabImg.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlgTabImg.Down:= false;
  _FILE_TABIMAGES_SELECTION_CHANGED := ( _FILE_TABIMAGES_SELECTION_CHANGED or Action );
end;

procedure TForm_FileInfo.TB_OpenDlgTrayIconClick(Sender: TObject);
var
  fn : String;
  Action: boolean;
begin
  Form_Main.OpenDlg.Filter:= FILTER_ICONS;
  Action:= Form_Main.OpenDlg.Execute;
  fn := normalfn( Form_Main.OpenDlg.Filename );
  Action := ( Action and Fileexists( fn ));
  if Action then begin
    Edit_TrayIcon.Text:= fn;
    Image_TrayIcon.Picture.LoadFromFile( fn );
  end;
  TB_OpenDlgTrayIcon.Down:= false;
end;

procedure TForm_FileInfo.Button_SystemClick(Sender: TObject);
var
  sei: TShellExecuteinfo;
begin
  FillChar(sei,sizeof(sei),0);
  sei.cbSize := sizeof(sei);
  sei.lpFile := Pchar(myNotes.FileName);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@sei);
end;

procedure TForm_FileInfo.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, Pages.ActivePage.HelpContext );
end;

end.
