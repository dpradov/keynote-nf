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

   kn_KntFile
   ;


type
  TForm_KntFileInfo = class(TForm)
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
    btnRecalcNextID: TButton;
    lblKeyTr: TLabel;
    txtIter: TEdit;
    btnTestIter: TButton;
    cbEnableEncrCont: TCheckBox;
    cbHideEncrNodes: TCheckBox;
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
    procedure cbEnableEncrCont_Click(Sender: TObject);
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
    function FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
    procedure btnRecalcNextIDClick(Sender: TObject);
    procedure txtIterKeyPress(Sender: TObject; var Key: Char);
    procedure txtIterExit(Sender: TObject);
    procedure btnTestIterClick(Sender: TObject);
  private
    { Private declarations }
    fChangingInCode: boolean;

    procedure CheckExternalStorageEnabled;
    procedure CheckExternalStorageLocation;

  public
    { Public declarations }
    myKntFile : TKntFile;
    OK_Click : boolean;
    PassphraseChanged : boolean;
    MinPassLen : integer;
    HidePassText : boolean;
    ExtStorageLocationFake: boolean;

    function Verify : boolean;
    procedure EnablePassControls(Enable: boolean= true);
  end;

const
  _FILE_TABIMAGES_SELECTION_CHANGED : boolean = false;

implementation
uses
   ZLibEx,
   gf_misc,
   gf_miscvcl,
   gf_files,
   kn_main,
   kn_Global,
   kn_Info,
   kn_Const,
   kn_NoteFileMng,
   Knt.App,
   knt.RS
   ;

{$R *.DFM}


procedure TForm_KntFileInfo.FormCreate(Sender: TObject);
var
  cm : TCryptMethod;
  ff : TKntFileFormat;
  cl: TZCompressionLevel;
begin
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  Tab_Pass.TabVisible := false; // [x] while not implemented
  HidePassText := true;
  fChangingInCode:= false;

  EnablePassControls(False);

  Pages.ActivePage := Tab_Main;
  myKntFile := nil;
  PassphraseChanged := false;
  MinPassLen := MIN_PASS_LEN;

  for cm := low( TCryptMethod ) to high( TCryptMethod ) do
    Combo_Method.Items.Add( CRYPT_METHOD_NAMES[cm] );
  Combo_Method.ItemIndex := ord( low( TCryptMethod ));
  for ff := low( TKntFileFormat ) to high( TKntFileFormat ) do
    Combo_Format.Items.Add( FILE_FORMAT_NAMES[ff] + GetRS(sFInf01) );
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

  App.ApplyBiDiModeOnForm(Self);

  OK_Click := false;
end; // CREATE

procedure TForm_KntFileInfo.FormActivate(Sender: TObject);
var
  fs : longint;
  IconFN: string;
begin
  if assigned( myKntFile ) then
  begin
    App.SetTopMost(Handle, True);
    // TAB_MAIN
    Caption := GetRS(sFInf02) + ExtractFilename( myKntFile.FileName );
    Edit_FileName.Text := myKntFile.FileName;
    Label_Count.Caption := inttostr( myKntFile.FolderCount );
    Edit_Comment.Text := myKntFile.Comment;
    Edit_Description.Text := myKntFile.Description;
    label_Created.Caption := FormatDateTime( FormatSettings.LongDateFormat + #32 + FormatSettings.LongTimeFormat, myKntFile.DateCreated );
    Combo_Format.ItemIndex := ord( myKntFile.FileFormat );
    txtIter.Text := myKntFile.KeyDerivIterations.ToString;
    if Fileexists( myKntFile.FileName ) then begin
      fs := GetFileSize( myKntFile.FileName );
      if ( fs < 1025 ) then
        Label_FileSize.Caption := inttostr( fs ) + GetRS(sFInf03)
      else
        Label_FileSize.Caption := inttostr( fs DIV 1024 ) + ' Kb';
      label_Modified.Caption := FormatDateTime( FormatSettings.LongDateFormat + #32 + FormatSettings.LongTimeFormat, GetFileDateStamp( myKntFile.FileName ));
      if myKntFile.SavedWithRichEdit3 then begin
        // LB_RTF3.Font.Color := clRed;
        LB_RTF3.Visible := true;
      end;
    end
    else begin
      Label_FileSize.Caption := GetRS(sFInf04);
      Edit_FileName.Visible := false;
      Label_FileNotFound.Visible := true;
      label_Modified.Caption := GetRS(sFInf05);
      rbImagesStChange.Caption := GetRS(sFInf15);
    end;
    CB_AsReadOnly.Caption := Format( GetRS(sFInf06), [ExtractFilename( myKntFile.FileName )] );
    CB_AsReadOnly.Checked := ( myKntFile.OpenAsReadOnly or myKntFile.ReadOnly );
    Label_IsReadOnly.Visible := myKntFile.ReadOnly;
    CB_NoMultiBackup.Checked := myKntFile.NoMultiBackup;

    // tray icon stuff
    IconFN:= myKntFile.TrayIconFN;
    if IconFN <> '' then
       IconFN:= GetAbsolutePath(ActiveFile.File_Path, myKntFile.TrayIconFN);
    if (IconFN <> '') and FileExists(IconFN) then begin
       CB_TrayIcon.Checked := true;
       Edit_TrayIcon.Text := myKntFile.TrayIconFN;
       Image_TrayIcon.Picture.LoadFromFile(IconFN);
    end
    else
      CB_TrayIcon.Checked := false;

    CheckBox_TrayIconClick( CB_TrayIcon );
    CB_TrayIcon.OnClick := CheckBox_TrayIconClick;

    // tab icons stuff
    CB_ShowTabIcons.Checked := myKntFile.ShowTabIcons;
    if ( myKntFile.TabIconsFN = '' ) then // default
      RB_TabImgDefault.Checked := true

    else begin
      if ( myKntFile.TabIconsFN[1] = _NF_Icons_BuiltIn ) then
        RB_TabImgBuiltIn.Checked := true
      else begin
        Edit_TabImg.Text := myKntFile.TabIconsFN;
        if fileexists( myKntFile.TabIconsFN ) then
          RB_TabImgOther.Checked := true
        else
          RB_TabImgDefault.Checked := true;
      end;
    end;
    CheckBox_ShowTabIconsClick( CB_ShowTabIcons );
    RB_TabImgOtherClick( RB_TabImgOther );
    CB_ShowTabIcons.OnClick := CheckBox_ShowTabIconsClick;
    cbEnableEncrCont.Enabled:= (myKntFile.FileFormat <> nffEncrypted);
    cbEnableEncrCont.Checked:= myKntFile.EncryptedContentEnabled;
    cbHideEncrNodes.Checked:= myKntFile.HideEncryptedNodes;
    cbHideEncrNodes.Visible:= False;
  end
  else begin
    Edit_FileName.Text := GetRS(sFInf07);
    Label_Count.Caption := '0';
    Edit_Comment.Text := '';
    Edit_Description.Text := '';
    label_Created.Caption := GetRS(sFInf05);
    label_Modified.Caption := GetRS(sFInf05);
    Label_FileSize.Caption := '';
    Label_IsReadOnly.Visible := false;

    Combo_Method.ItemIndex := ord( myKntFile.CryptMethod );
  end;

  lblImgWarning.Visible := false;
  if ImageMng.ChangingImagesStorage then begin
     lblImgWarning.Visible := true;
     lblImgWarning.Caption:= GetRS(sFInf16);
     lblImgWarning.Hint:= '';
  end
  else
  if ImageMng.ExternalStorageIsMissing then begin
     lblImgWarning.Visible := true;
     lblImgWarning.Caption:= GetRS(sFInf17);
     lblImgWarning.Hint:= GetRS(sFInf18);
  end;

  btnRecalcNextID.Visible := not lblImgWarning.Visible;
  btnRecalcNextID.Enabled := (ImageMng.StorageMode <> smEmbRTF) and not ActiveFile.Modified;

  cbImgStorageMode.ItemIndex := Ord(ImageMng.StorageMode);
  cbImgExtStorageType.ItemIndex:= Ord(ImageMng.ExternalStorageType);
  txtExtStorageLocation.Text:= ImageMng.ExternalStorageLocation;
  ExtStorageLocationFake:= (ActiveFile.FileName = '');
  cbImgStorageMode.Enabled:= not ImageMng.ChangingImagesStorage;
  CheckExternalStorageEnabled;


  if Edit_Description.Enabled then begin
    Edit_Description.SetFocus;
    Edit_Description.SelectAll;
  end;

  Combo_CompressLevel.ItemIndex := ord( myKntFile.CompressionLevel );
  Combo_FormatChange( Combo_Format );

  CB_HidePass.Checked := HidePassText;
  CheckBox_HidePassClick( CB_HidePass );

  CB_HidePass.OnClick := CheckBox_HidePassClick;
  Combo_Method.OnChange := Combo_MethodChange;
  Combo_Format.OnChange := Combo_FormatChange;
  Edit_Pass.OnChange := Edit_PassChange;
  Edit_Confirm.OnChange := Edit_PassChange;
  txtIter.OnKeyPress := txtIterKeyPress;
  txtIter.OnExit := txtIterExit;
  cbEnableEncrCont.OnClick:= cbEnableEncrCont_Click;
  CB_AsReadOnly.OnClick := CheckBox_AsReadOnlyClick;

  _FILE_TABIMAGES_SELECTION_CHANGED := false;

end; // ACTIVATE


procedure TForm_KntFileInfo.Combo_MethodChange(Sender: TObject);
begin
  // [x]
end;

function TForm_KntFileInfo.Verify : boolean;
var
  s : string;
begin
  result := true;

{$IFDEF WITH_DART}
  if (( Combo_Format.ItemIndex = ord( nffDartNotes )) and myKntFile.HasExtendedNotes ) then
  begin
    case App.DoMessageBox(GetRS(sFInf08, mtWarning, [mbOK,mbCancel] ) of
      mrOK : Combo_Format.ItemIndex := ord( nffKeyNote );
      mrCancel : begin
        result := false;
        exit;
      end;
    end;
  end;
{$ENDIF}


  if not ( PassphraseChanged and ((Combo_Format.ItemIndex = ord(nffEncrypted)) or cbEnableEncrCont.Checked )) then exit;

  s := '';

  if length( Edit_Pass.Text ) < MinPassLen then begin
    s := Format( GetRS(sFInf09), [MinPassLen] );
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end
  else
  if ( Edit_Pass.Text <> Edit_Confirm.Text ) then begin
    s := GetRS(sFInf10);
    Pages.ActivePage := Tab_Pass;
    Edit_Pass.SetFocus;
  end;


  if ( s <> '' ) then begin
    result := false;
    App.ErrorPopup(s);
    exit;
  end;

  if (Combo_Format.ItemIndex = ord(nffEncrypted)) and myKntFile.HasVirtualNotes then begin
    result := ( App.DoMessageBox(GetRS(sFInf11), mtWarning, [mbYes,mbNo], Def2 ) = mrYes );
  end;


end; // Verify;

procedure TForm_KntFileInfo.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
    CanClose := Verify;

  OK_Click := false;
end;

procedure TForm_KntFileInfo.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_KntFileInfo.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_KntFileInfo.FormKeyDown(Sender: TObject; var Key: Word;
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


procedure TForm_KntFileInfo.Combo_FormatChange(Sender: TObject);
var
  SelectedFormat: TKntFileFormat;

begin
 if fChangingInCode then exit;

 SelectedFormat:= TKntFileFormat(Combo_Format.ItemIndex);

 if cbEnableEncrCont.Checked and (SelectedFormat = nffEncrypted) then
    if not (myKntFile.KeysAreCached or myKntFile.CheckAuthorized(false)) then begin
       fChangingInCode:= True;
       Combo_Format.ItemIndex := ord(myKntFile.FileFormat);
       fChangingInCode:= False;
       exit;
    end
    else
       cbEnableEncrCont.Checked:= False;

{$IFDEF WITH_DART}
  Edit_Description.Enabled := SelectedFormat <> nffDartNotes;
{$ELSE}
  Edit_Description.Enabled := True;
{$ENDIF}
  Edit_Comment.Enabled := Edit_Description.Enabled;
  Combo_CompressLevel.Enabled := SelectedFormat = nffKeyNoteZip;
  if not Combo_CompressLevel.Enabled then
     Combo_CompressLevel.ItemIndex := ord( zcNone )
  else
     if (myKntFile.FileFormat <> nffKeyNoteZip) then
        Combo_CompressLevel.ItemIndex := ord( zcDefault );

  Tab_Pass.TabVisible := (SelectedFormat = nffEncrypted) or cbEnableEncrCont.Checked;
  cbEnableEncrCont.Enabled:= (SelectedFormat <> nffEncrypted);

  if ( Tab_Pass.TabVisible and (( myKntFile.FileFormat <> nffEncrypted ) and not myKntFile.EncryptedContentEnabled ) ) then begin
    // the file was NOT encrypted previously, so now passphrase must be entered.
    EnablePassControls;
    PassphraseChanged := true;
    Button_SetPass.Enabled := false;
  end;

  if cbEnableEncrCont.Checked and myKntFile.KeysAreCached then
     cbHideEncrNodes.Visible:= True;

end; // Combo_FormatChange


procedure TForm_KntFileInfo.cbEnableEncrCont_Click(Sender: TObject);
begin
  if fChangingInCode then exit;

  try
    if cbEnableEncrCont.Checked then begin
       Tab_Pass.TabVisible:= True;
       cbHideEncrNodes.Visible:= True;
       if not myKntFile.KeysAreCached then begin
          EnablePassControls;
          PassphraseChanged := true;
          Button_SetPass.Enabled := false;
       end;
    end
    else begin
       if ( (TKntFileFormat(Combo_Format.ItemIndex) <> nffEncrypted) and myKntFile.EncryptedContentEnabled and
            (App.DoMessageBox(GetRS(sFInf22), mtWarning, [mbYes,mbNo], def2) <> mrYes) ) or
          not myKntFile.CheckAuthorized(True) then begin
          fChangingInCode:= true;
          cbEnableEncrCont.Checked:= True
       end
       else begin
          Tab_Pass.TabVisible:= False;
          EnablePassControls(false);
       end;

    end;

  finally
    fChangingInCode:= false;
  end;

end;

procedure TForm_KntFileInfo.Button_SetPassClick(Sender: TObject);
begin
  if not myKntFile.CheckAuthorized(False) then exit;

  PassphraseChanged := true;
  EnablePassControls;
  try
    Button_SetPass.Enabled := false;
    Edit_Pass.SetFocus;
  except
  end;
end;

procedure TForm_KntFileInfo.Edit_PassChange(Sender: TObject);
begin
  PassphraseChanged := true;
end;

procedure TForm_KntFileInfo.CheckBox_AsReadOnlyClick(Sender: TObject);
begin
  if assigned( myKntFile ) then begin
    if (( not CB_AsReadOnly.Checked ) and myKntFile.ReadOnly ) then begin
      if ( App.DoMessageBox( Format(GetRS(sFInf12),[ExtractFilename( myKntFile.FileName )]), mtWarning, [mbYes,mbNo], def1, 0, Self.Handle ) = mrYes ) then
        CB_AsReadOnly.OnClick := nil
      else
        CB_AsReadOnly.Checked := true;
    end;
  end;
end;


procedure TForm_KntFileInfo.CheckBox_HidePassClick(Sender: TObject);
begin
  HidePassText := CB_HidePass.Checked;
  if HidePassText then
    Edit_Pass.PasswordChar := '*'
  else
    Edit_Pass.PasswordChar := #0;
  Edit_Confirm.PasswordChar := Edit_Pass.PasswordChar;
end;

procedure TForm_KntFileInfo.EnablePassControls (Enable: boolean= true);
begin
  Label_Pass.Enabled := Enable;
  Label_Confirm.Enabled := Enable;
  Label_Method.Enabled := Enable;
  Edit_Pass.Enabled := Enable;
  Edit_Confirm.Enabled := Enable;
  Combo_Method.Enabled := Enable;
  CB_HidePass.Enabled := Enable;
  Label_EnterPass.Visible := Enable;
  lblKeyTr.Enabled := Enable;
  btnTestIter.Enabled:= Enable;
  txtIter.Enabled:= Enable;
  cbHideEncrNodes.Visible:= (cbEnableEncrCont.Checked);
end; // EnablePassControls


procedure TForm_KntFileInfo.txtIterKeyPress(Sender: TObject; var Key: Char);
begin
   if not (Key in ['0'..'9']) then
      Key:= #0;
end;


procedure TForm_KntFileInfo.txtIterExit(Sender: TObject);
var
   L: UInt64;
begin
   if Trim(txtIter.Text) = '' then
      L:= KEY_ITERATIONS_VERIF_DEFAULT

   else begin
     L:= StrToUInt64Def(txtIter.Text, KEY_ITERATIONS_VERIF_MAX);
     if L > KEY_ITERATIONS_VERIF_MAX then
        L:= KEY_ITERATIONS_VERIF_MAX;
     if L = 0 then
        L:= KEY_ITERATIONS_VERIF_DEFAULT;
   end;

   txtIter.Text:= L.ToString;
end;


procedure TForm_KntFileInfo.btnTestIterClick(Sender: TObject);
var
  StartTick: Cardinal;
  ElapsedMs: Cardinal;
  NIter: Cardinal;
  EncryptionKey, VerificationHash: THash;
begin
  NIter:= StrToUIntDef(txtIter.Text, KEY_ITERATIONS_VERIF_DEFAULT);
  StartTick := GetTickCount;
  CalculatePassphraseHashes (Edit_Pass.Text, EncryptionKey, VerificationHash, NIter);

  ElapsedMs := GetTickCount - StartTick;
  App.InfoPopup(Format(GetRS(sFile25), [ElapsedMs]));
end;


procedure TForm_KntFileInfo.CheckBox_TrayIconClick(Sender: TObject);
begin
  Edit_TrayIcon.Enabled := CB_TrayIcon.Checked;
  TB_OpenDlgTrayIcon.Enabled := Edit_TrayIcon.Enabled;
  Image_TrayIcon.Visible := Edit_TrayIcon.Enabled;
end;

procedure TForm_KntFileInfo.CheckBox_ShowTabIconsClick(Sender: TObject);
begin
  RB_TabImgDefault.Enabled := CB_ShowTabIcons.Checked;
  RB_TabImgBuiltIn.Enabled := RB_TabImgDefault.Enabled;
  RB_TabImgOther.Enabled := RB_TabImgDefault.Enabled;
  Edit_TabImg.Enabled := ( RB_TabImgDefault.Enabled and RB_TabImgOther.Checked );
  TB_OpenDlgTabImg.Enabled := Edit_TabImg.Enabled;
end;

procedure TForm_KntFileInfo.RB_TabImgOtherClick(Sender: TObject);
begin
  _FILE_TABIMAGES_SELECTION_CHANGED := true;
  Edit_TabImg.Enabled := ( RB_TabImgOther.Enabled and RB_TabImgOther.Checked );
  TB_OpenDlgTabImg.Enabled := Edit_TabImg.Enabled;
end;


procedure TForm_KntFileInfo.TB_OpenDlgTabImgClick(Sender: TObject);
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

procedure TForm_KntFileInfo.TB_OpenDlgTrayIconClick(Sender: TObject);
var
  fn : String;
  Action: boolean;
begin
  Form_Main.OpenDlg.Filter:= FILTER_ICONS;
  Action:= Form_Main.OpenDlg.Execute;
  fn := normalfn( Form_Main.OpenDlg.Filename );
  Action := ( Action and Fileexists( fn ));
  if Action then begin
    Edit_TrayIcon.Text:= ExtractRelativePath(ActiveFile.File_Path, fn);
    Image_TrayIcon.Picture.LoadFromFile( fn );
  end;
  TB_OpenDlgTrayIcon.Down:= false;
end;

procedure TForm_KntFileInfo.Button_SystemClick(Sender: TObject);
var
  sei: TShellExecuteinfoW;
begin
  FillChar(sei,sizeof(sei),0);
  sei.cbSize := sizeof(sei);
  sei.lpFile := PChar(myKntFile.FileName);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExW(@sei);
end;

function TForm_KntFileInfo.FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
var
  Node, Marker: integer;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_KntFileInfo.Button_HelpClick(Sender: TObject);
begin
{
	File Properties [45]
		Information [46]
		Settings [47]
		File Icons [48]
		Security [49]
}
  ActiveKeyNoteHelp(45);  // Node
end;


procedure TForm_KntFileInfo.cbImgStorageModeChange(Sender: TObject);
begin
   if not (TImagesStorageMode(cbImgStorageMode.ItemIndex) in [smExternal, smExternalAndEmbKNT]) then begin
      ExtStorageLocationFake:= (ActiveFile.FileName = '');
      txtExtStorageLocation.Text:= '';
      rbImagesStChange.Checked:= false;
   end;
   CheckExternalStorageEnabled;
end;

procedure TForm_KntFileInfo.cbImgExtStorageTypeChange(Sender: TObject);
var
  ExtLocation, Ext: string;

begin
   CheckExternalStorageLocation;

   ExtLocation:= txtExtStorageLocation.Text;
   Ext:= ExtractFileExt(ExtLocation).ToUpper ;

   case TImagesExternalStorage(cbImgExtStorageType.ItemIndex) of
    issZip:    if Ext <> '.ZIP' then ExtLocation:= ExtLocation + '.zip';
    issFolder: if Ext  = '.ZIP' then ExtLocation:= ChangeFileExt(ExtLocation, '');
   end;
   txtExtStorageLocation.Text:= ExtLocation;

end;


procedure TForm_KntFileInfo.CheckExternalStorageEnabled;
var
  extStorageEnab: boolean;
begin
   extStorageEnab:= cbImgStorageMode.Enabled and (TImagesStorageMode(cbImgStorageMode.ItemIndex) in [smExternal, smExternalAndEmbKNT]);
   rbImagesStChange.Enabled := extStorageEnab;
   rbImagesStRelocate.Enabled := extStorageEnab and (not ImageMng.FileIsNew) and (ImageMng.StorageMode in [smExternal, smExternalAndEmbKNT]);
   if extStorageEnab and (txtExtStorageLocation.Text= '') then
      CheckExternalStorageLocation;

   extStorageEnab:= extStorageEnab and (rbImagesStChange.Checked or rbImagesStRelocate.Checked);

   txtExtStorageLocation.Enabled := extStorageEnab;
   cbImgExtStorageType.Enabled := extStorageEnab;
   btnOpenDlgExternalPath.Enabled := extStorageEnab;
end;


procedure TForm_KntFileInfo.CheckExternalStorageLocation;
var
  extStorageType: TImagesExternalStorage;
begin
   if ExtStorageLocationFake or (txtExtStorageLocation.Text = '') then begin
      extStorageType:= TImagesExternalStorage(cbImgExtStorageType.ItemIndex);
      txtExtStorageLocation.Text:= ImageMng.GetDefaultExternalLocation(extStorageType);
   end;
end;

procedure TForm_KntFileInfo.txtExtStorageLocationEnter(Sender: TObject);
begin
    if ExtStorageLocationFake then
       txtExtStorageLocation.Text:= '';
end;

procedure TForm_KntFileInfo.txtExtStorageLocationExit(Sender: TObject);
var
  Ext: string;
  extStorageType: TImagesExternalStorage;

begin
   CheckExternalStorageLocation;

   Ext:= ExtractFileExt(txtExtStorageLocation.Text).ToUpper;
   if Ext = '.ZIP' then
      extStorageType:= issZip
   else
      extStorageType:= issFolder;

   cbImgExtStorageType.ItemIndex:= Ord(extStorageType);
end;

procedure TForm_KntFileInfo.txtExtStorageLocationKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ExtStorageLocationFake:= false;
end;

procedure TForm_KntFileInfo.rbImagesStChangeClick(Sender: TObject);
begin
   CheckExternalStorageEnabled;
end;

procedure TForm_KntFileInfo.rbImagesStRelocateClick(Sender: TObject);
begin
   CheckExternalStorageEnabled;
end;


procedure TForm_KntFileInfo.btnRecalcNextIDClick(Sender: TObject);
var
  MaxSavedID: integer;
begin
  MaxSavedID:= ImageMng.GetMaxSavedImageID;

  if MaxSavedID+1 = ImageMng.NextImageID then
     App.InfoPopup(Format(GetRS(sFInf19), [MaxSavedID+1, MaxSavedID]))

  else
    if (App.DoMessageBox( Format(GetRS(sFInf20), [MaxSavedID, ImageMng.NextImageID, MaxSavedID+1]),
       mtInformation, [mbYes,mbNo,mbCancel], Def3 ) = mrYes ) then begin
       if ImageMng.RecalcNextID then
          App.InfoPopup(GetRS(sFInf21));
    end;
end;



procedure TForm_KntFileInfo.btnOpenDlgExternalPathClick(Sender: TObject);
var
  Dir: string;

begin
  try
      btnOpenDlgExternalPath.Down:= False;
      Dir:= ExtractFilePath( txtExtStorageLocation.Text);
      if TImagesExternalStorage(cbImgExtStorageType.ItemIndex) = issFolder then begin
         if SelectDirectory(GetRS(sFInf13),'', Dir) then begin
            txtExtStorageLocation.Text:= Dir;
            ExtStorageLocationFake:= false;
         end;
      end
      else begin
       if Dir <> '' then
          Dir:= ActiveFile.File_Path;

        with Form_Main.OpenDlg do begin
          Title := GetRS(sFInf14);
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
        App.ErrorPopup(E.Message);
    end;
  end;

end;

end.
