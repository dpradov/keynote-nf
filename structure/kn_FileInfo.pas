unit kn_FileInfo;

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
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   Vcl.ExtCtrls,
   Vcl.FileCtrl,
   Vcl.Mask,
   RxPlacemnt,
   TB97Ctls,
   ZLibEx,
   gf_misc,
   gf_files,
   kn_Info,
   kn_Const,
   kn_NoteObj,
   kn_FileObj,
   kn_ImagesMng;


type
  TForm_FileInfo = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Pages: TPageControl;
    Tab_Main: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label_Created: TLabel;
    Label_Modified: TLabel;
    Label5: TLabel;
    Label_Count: TLabel;
    Edit_Comment: TEdit;
    Label7: TLabel;
    Label_FileSize: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label_FileNotFound: TLabel;
    Label6: TLabel;
    Edit_Description: TEdit;
    Label8: TLabel;
    Combo_Format: TComboBox;
    Tab_Pass: TTabSheet;
    GroupBox2: TGroupBox;
    Label_Confirm: TLabel;
    Label_Pass: TLabel;
    Label_Method: TLabel;
    Edit_Confirm: TEdit;
    Edit_Pass: TEdit;
    Combo_Method: TComboBox;
    Button_SetPass: TButton;
    Label_EnterPass: TLabel;
    Tab_Settings: TTabSheet;
    GroupBox3: TGroupBox;
    CB_AsReadOnly: TCheckBox;
    FormPlacement: TFormPlacement;
    CB_HidePass: TCheckBox;
    Edit_FileName: TEdit;
    Tab_Icons: TTabSheet;
    GroupBox4: TGroupBox;
    CB_ShowTabIcons: TCheckBox;
    CB_TrayIcon: TCheckBox;
    Image_TrayIcon: TImage;
    RB_TabImgDefault: TRadioButton;
    RB_TabImgBuiltIn: TRadioButton;
    RB_TabImgOther: TRadioButton;
    Button_System: TButton;
    Button_Help: TButton;
    LB_RTF3: TLabel;
    CB_NoMultiBackup: TCheckBox;
    TB_OpenDlgTrayIcon: TToolbarButton97;
    Edit_TrayIcon: TEdit;
    TB_OpenDlgTabImg: TToolbarButton97;
    Edit_TabImg: TEdit;
    Label9: TLabel;
    Combo_CompressLevel: TComboBox;
    Label_IsReadOnly: TLabel;
    cbImgStorageMode: TComboBox;
    Label26: TLabel;
    gbExternalStorage: TGroupBox;
    Label22: TLabel;
    btnOpenDlgExternalPath: TToolbarButton97;
    cbImgExtStorageType: TComboBox;
    txtExtStorageLocation: TEdit;
    Label10: TLabel;
    rbImagesStRelocate: TRadioButton;
    rbImagesStChange: TRadioButton;
    lblImgWarning: TLabel;
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
    procedure cbImgStorageModeChange(Sender: TObject);
    procedure cbImgExtStorageTypeChange(Sender: TObject);
    procedure btnOpenDlgExternalPathClick(Sender: TObject);
    procedure txtExtStorageLocationEnter(Sender: TObject);
    procedure txtExtStorageLocationExit(Sender: TObject);
    procedure rbImagesStRelocateClick(Sender: TObject);
    procedure rbImagesStChangeClick(Sender: TObject);
    procedure txtExtStorageLocationKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }

    procedure CheckExternalStorageEnabled;
    procedure CheckExternalStorageLocation;

  public
    { Public declarations }
    myNotes : TNoteFile;
    OK_Click : boolean;
    PassphraseChanged : boolean;
    MinPassLen : integer;
    HidePassText : boolean;
    ExtStorageLocationFake: boolean;

    function Verify : boolean;
    procedure EnablePassControls;
  end;

const
  _FILE_TABIMAGES_SELECTION_CHANGED : boolean = false;

implementation
uses
   kn_main,
   kn_Global
   ;

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
                     'Note that the disk files linked to virtual nodes and images saves in external storage (Zip or Folder) ' +
                     'will NOT be encrypted.' + #13#13 + 'Continue?';
  STR_12 = 'File "%s" was open in READ-ONLY mode. If you uncheck this box, the read-only mode will be turned OFF. Continue?';
  STR_13 = 'Open images storage folder';
  STR_14 = 'Open images storage file';
  STR_15 = 'Set';
  STR_16 = 'Must save KNT before change images storage again';
  STR_17 = '(*) Missing current external storage';
  STR_18 = 'New images will be saved provisionally [only] as Embedded KNT' + #13 +
           'Deletions will be effective when it is available'+ #13#13 +
           '(It may be totally fine if you temporarily lose access to image storage)';


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

  for var j : TImagesStorageMode := low( TImagesStorageMode ) to high( TImagesStorageMode ) do
     CbImgStorageMode.Items.Add( IMAGES_STORAGE_MODE[j] );
  CbImgStorageMode.ItemIndex := 1;
  for var j : TImagesExternalStorage := low( TImagesExternalStorage ) to high( TImagesExternalStorage ) do
     cbImgExtStorageType.Items.Add( EXTERNAL_STORAGE_TYPE[j] );
  cbImgExtStorageType.ItemIndex := 1;

  ExtStorageLocationFake:= false;

  OK_Click := false;
end; // CREATE

procedure TForm_FileInfo.FormActivate(Sender: TObject);
var
  fs : longint;
begin
  if assigned( myNotes ) then
  begin
    // TAB_MAIN
    Caption := STR_02 + ExtractFilename( myNotes.FileName );
    Edit_FileName.Text := myNotes.FileName;
    Label_Count.Caption := inttostr( myNotes.NoteCount );
    Edit_Comment.Text := myNotes.Comment;
    Edit_Description.Text := myNotes.Description;
    label_Created.Caption := FormatDateTime( FormatSettings.LongDateFormat + #32 + FormatSettings.LongTimeFormat, myNotes.DateCreated );
    Combo_Format.ItemIndex := ord( myNotes.FileFormat );
    if Fileexists( myNotes.FileName ) then begin
      fs := GetFileSize( myNotes.FileName );
      if ( fs < 1025 ) then
        Label_FileSize.Caption := inttostr( fs ) + STR_03
      else
        Label_FileSize.Caption := inttostr( fs DIV 1024 ) + ' Kb';
      label_Modified.Caption := FormatDateTime( FormatSettings.LongDateFormat + #32 + FormatSettings.LongTimeFormat, GetFileDateStamp( myNotes.FileName ));
      if myNotes.SavedWithRichEdit3 then begin
        // LB_RTF3.Font.Color := clRed;
        LB_RTF3.Visible := true;
      end;
    end
    else begin
      Label_FileSize.Caption := STR_04;
      Edit_FileName.Visible := false;
      Label_FileNotFound.Visible := true;
      label_Modified.Caption := STR_05;
      rbImagesStChange.Caption := STR_15;
    end;
    CB_AsReadOnly.Caption := Format( STR_06, [ExtractFilename( myNotes.FileName )] );
    CB_AsReadOnly.Checked := ( myNotes.OpenAsReadOnly or myNotes.ReadOnly );
    Label_IsReadOnly.Visible := myNotes.ReadOnly;
    CB_NoMultiBackup.Checked := myNotes.NoMultiBackup;

    // tray icon stuff
    if (( myNotes.TrayIconFN <> '' ) and fileexists( myNotes.TrayIconFN )) then begin
      CB_TrayIcon.Checked := true;
      Edit_TrayIcon.Text := myNotes.TrayIconFN;
      Image_TrayIcon.Picture.LoadFromFile( myNotes.TrayIconFN );
    end
    else
      CB_TrayIcon.Checked := false;

    CheckBox_TrayIconClick( CB_TrayIcon );
    CB_TrayIcon.OnClick := CheckBox_TrayIconClick;

    // tab icons stuff
    CB_ShowTabIcons.Checked := myNotes.ShowTabIcons;
    if ( myNotes.TabIconsFN = '' ) then // default
      RB_TabImgDefault.Checked := true

    else begin
      if ( myNotes.TabIconsFN[1] = _NF_Icons_BuiltIn ) then
        RB_TabImgBuiltIn.Checked := true
      else begin
        Edit_TabImg.Text := myNotes.TabIconsFN;
        if fileexists( myNotes.TabIconsFN ) then
          RB_TabImgOther.Checked := true
        else
          RB_TabImgDefault.Checked := true;
      end;
    end;
    CheckBox_ShowTabIconsClick( CB_ShowTabIcons );
    RB_TabImgOtherClick( RB_TabImgOther );
    CB_ShowTabIcons.OnClick := CheckBox_ShowTabIconsClick;
  end
  else begin
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

  lblImgWarning.Visible := false;
  if ImagesManager.ChangingImagesStorage then begin
     lblImgWarning.Visible := true;
     lblImgWarning.Caption:= STR_16;
     lblImgWarning.Hint:= '';
  end
  else
  if ImagesManager.ExternalStorageIsMissing then begin
     lblImgWarning.Visible := true;
     lblImgWarning.Caption:= STR_17;
     lblImgWarning.Hint:= STR_18;
  end;
  cbImgStorageMode.ItemIndex := Ord(ImagesManager.StorageMode);
  cbImgExtStorageType.ItemIndex:= Ord(ImagesManager.ExternalStorageType);
  txtExtStorageLocation.Text:= ImagesManager.ExternalStorageLocation;
  ExtStorageLocationFake:= (NoteFile.FileName = '');
  cbImgStorageMode.Enabled:= not ImagesManager.ChangingImagesStorage;
  CheckExternalStorageEnabled;


  if Edit_Description.Enabled then begin
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

{$IFDEF WITH_DART}
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
{$ENDIF}


  if ( not ( PassphraseChanged and ( Combo_Format.ItemIndex = ord( nffEncrypted )))) then exit;

  s := '';

  if length( Edit_Pass.Text ) < MinPassLen then begin
    s := Format( STR_09, [MinPassLen] );
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end
  else
  if ( Edit_Pass.Text <> Edit_Confirm.Text ) then begin
    s := STR_10;
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end;


  if ( s <> '' ) then begin
    result := false;
    messagedlg( s, mtError, [mbOK], 0 );
    exit;
  end;

  if myNotes.HasVirtualNodes then begin
    result := ( messagedlg(STR_11, mtWarning, [mbYes,mbNo], 0 ) = mrYes );
  end;


end; // Verify;

procedure TForm_FileInfo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
    CanClose := Verify;

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
    27 :
      if (( shift = [] ) and ( not ( Combo_Method.DroppedDown or Combo_Format.DroppedDown ))) then begin
        key := 0;
        OK_Click := false;
        Close;
      end;
  end;
end; // KEY DOWN


procedure TForm_FileInfo.Combo_FormatChange(Sender: TObject);
begin
{$IFDEF WITH_DART}
  Edit_Description.Enabled := Combo_Format.ItemIndex <> ord( nffDartNotes );
{$ELSE}
  Edit_Description.Enabled := True;
{$ENDIF}
  Edit_Comment.Enabled := Edit_Description.Enabled;
  Combo_CompressLevel.Enabled := Combo_Format.ItemIndex = ord( nffKeyNoteZip );
  if not Combo_CompressLevel.Enabled then
     Combo_CompressLevel.ItemIndex := ord( zcNone )
  else
     if (myNotes.FileFormat <> nffKeyNoteZip) then
        Combo_CompressLevel.ItemIndex := ord( zcDefault );

  Tab_Pass.TabVisible := ( Combo_Format.ItemIndex = ord( nffEncrypted ));
  if ( Tab_Pass.TabVisible and ( myNotes.FileFormat <> nffEncrypted )) then begin
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
  if assigned( myNotes ) then begin
    if (( not CB_AsReadOnly.Checked ) and myNotes.ReadOnly ) then begin
      if ( DoMessageBox( Format(STR_12,[ExtractFilename( myNotes.FileName )]), mtWarning, [mbYes,mbNo], 0, Self.Handle ) = mrYes ) then
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
  if Action then
     Edit_TabImg.Text := Form_Main.OpenDlg.Filename;

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
  sei: TShellExecuteinfoW;
begin
  FillChar(sei,sizeof(sei),0);
  sei.cbSize := sizeof(sei);
  sei.lpFile := PChar(myNotes.FileName);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExW(@sei);
end;

procedure TForm_FileInfo.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, Pages.ActivePage.HelpContext );
end;


procedure TForm_FileInfo.cbImgStorageModeChange(Sender: TObject);
begin
   if not (TImagesStorageMode(cbImgStorageMode.ItemIndex) in [smExternal, smExternalAndEmbKNT]) then begin
      ExtStorageLocationFake:= (NoteFile.FileName = '');
      txtExtStorageLocation.Text:= '';
      rbImagesStChange.Checked:= false;
   end;
   CheckExternalStorageEnabled;
end;

procedure TForm_FileInfo.cbImgExtStorageTypeChange(Sender: TObject);
begin
   CheckExternalStorageLocation;
end;


procedure TForm_FileInfo.CheckExternalStorageEnabled;
var
  extStorageEnab: boolean;
begin
   extStorageEnab:= cbImgStorageMode.Enabled and (TImagesStorageMode(cbImgStorageMode.ItemIndex) in [smExternal, smExternalAndEmbKNT]);
   rbImagesStChange.Enabled := extStorageEnab;
   rbImagesStRelocate.Enabled := extStorageEnab and (not ImagesManager.FileIsNew) and (ImagesManager.StorageMode in [smExternal, smExternalAndEmbKNT]);
   if extStorageEnab and (txtExtStorageLocation.Text= '') then
      CheckExternalStorageLocation;

   extStorageEnab:= extStorageEnab and (rbImagesStChange.Checked or rbImagesStRelocate.Checked);

   txtExtStorageLocation.Enabled := extStorageEnab;
   cbImgExtStorageType.Enabled := extStorageEnab;
   btnOpenDlgExternalPath.Enabled := extStorageEnab;
end;


procedure TForm_FileInfo.CheckExternalStorageLocation;
var
  extStorageType: TImagesExternalStorage;
begin
   if ExtStorageLocationFake or (txtExtStorageLocation.Text = '') then begin
      extStorageType:= TImagesExternalStorage(cbImgExtStorageType.ItemIndex);
      txtExtStorageLocation.Text:= ImagesManager.GetDefaultExternalLocation(extStorageType);
   end;
end;

procedure TForm_FileInfo.txtExtStorageLocationEnter(Sender: TObject);
begin
    if ExtStorageLocationFake then
       txtExtStorageLocation.Text:= '';
end;

procedure TForm_FileInfo.txtExtStorageLocationExit(Sender: TObject);
begin
   CheckExternalStorageLocation;
end;

procedure TForm_FileInfo.txtExtStorageLocationKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ExtStorageLocationFake:= false;
end;

procedure TForm_FileInfo.rbImagesStChangeClick(Sender: TObject);
begin
   CheckExternalStorageEnabled;
end;

procedure TForm_FileInfo.rbImagesStRelocateClick(Sender: TObject);
begin
   CheckExternalStorageEnabled;
end;


procedure TForm_FileInfo.btnOpenDlgExternalPathClick(Sender: TObject);
var
  Dir: string;

begin
  try
      btnOpenDlgExternalPath.Down:= False;
      Dir:= ExtractFilePath( txtExtStorageLocation.Text);
      if TImagesExternalStorage(cbImgExtStorageType.ItemIndex) = issFolder then begin
         if SelectDirectory(STR_13,'', Dir) then begin
            txtExtStorageLocation.Text:= Dir;
            ExtStorageLocationFake:= false;
         end;
      end
      else begin
       if Dir <> '' then
          Dir:= NoteFile.File_Path;

        with Form_Main.OpenDlg do begin
          Title := STR_14;
          Filter := FILTER_ZIP;
          InitialDir:= Dir;
        end;
        if Form_Main.OpenDlg.Execute then begin
           txtExtStorageLocation.Text := Form_Main.OpenDlg.FileName;
           ExtStorageLocationFake:= false;
        end;
      end;
  except
    on E : Exception do begin
     if E.Message <> '' then
        PopupMessage( E.Message, mtError, [mbOK,mbHelp], _HLP_KNTFILES );
    end;
  end;

end;

end.
