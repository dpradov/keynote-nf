unit palmexportform;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, IniFiles,
  Registry, StdCtrls, Mask, ToolEdit,
  Dialogs, gf_misc, FileCtrl, Buttons,
  gf_strings, gf_files;

type
  TForm_PalmExp = class(TForm)
    Btn_OK: TButton;
    Btn_Cancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit_FN: TFilenameEdit;
    Btn_Palm: TBitBtn;
    Btn_MyDoc: TBitBtn;
    Btn_Desktop: TBitBtn;
    Label2: TLabel;
    Edit_Title: TEdit;
    CB_AutoInstall: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit_FNChange(Sender: TObject);
    procedure Btn_PalmClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    IniFN : string;
    FixExtension : boolean;
    StdExtension : string;

    PalmInstallDir : string;
    LastDir : string;
    ExportFN : string;
    ExportTitle : string;
    AutoInstall : boolean;
    AutoName : boolean;
    SuggestedName : string;

  end;

var
  MyAppHandle : THandle;

implementation

{$R *.DFM}


procedure TForm_PalmExp.FormCreate(Sender: TObject);
var
  IniFile : TIniFile;
  reg : TRegistry;
  PalmUser, tempDir : string;
begin

  ExportFN := '';
  ExportTitle := '';
  FixExtension := true;
  StdExtension := '.pdb';
  AutoInstall := true;
  AutoName := true;
  SuggestedName := '';
  // get the Palm Desktop "install" directory for current user

  PalmInstallDir := '';
  reg := TRegistry.Create;
  try
    try
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.OpenKey( 'Software\U.S. Robotics\Pilot Desktop\Preferences', false ) then
      begin
        PalmUser := reg.ReadString( 'DesktopUser' );
        reg.CloseKey;
        if (( PalmUser <> '' ) and reg.OpenKey( 'Software\U.S. Robotics\Pilot Desktop\Core', false )) then
        begin
          PalmInstallDir := reg.ReadString( 'Path' );
          reg.CloseKey;
          if ( PalmInstallDir <> '' ) then
          begin
            PalmInstallDir := PalmInstallDir + '\' + PalmUser + '\Install';
          end;
        end;
      end;
    except
      PalmInstallDir := '';
    end;
  finally
    {
    if (( PalmInstallDir <> '' ) and directoryexists( PalmInstallDir )) then
      LastDir := PalmInstallDir
    else
    }
  end;

  LastDir := GetFolderPath( fpPersonal ); // default to "My Documents"

  // load config
  IniFN := lowercase( extractfilepath( ParamStr( 0 )) + 'plugins\palmexport.ini' );

  if fileexists( IniFN ) then
  begin
    IniFile := TIniFile.Create( IniFN );
    try
      with IniFile do
      begin
        tempDir := readstring( 'PalmExport', 'LastDir', LastDir );
        if (( tempDir <> '' ) and directoryexists( tempDir )) then
          LastDir := tempDir;
        FixExtension := readbool( 'PalmExport', 'FixExtension', FixExtension );
        StdExtension := readstring( 'PalmExport', 'StdExtension', StdExtension );
        if ( StdExtension = '' ) then
          StdExtension := '.pdb';
        AutoInstall := readbool( 'PalmExport', 'AutoInstall', AutoInstall );
        AutoName := readbool( 'PalmExport', 'AutoName', AutoName );
      end;
    finally
      IniFile.Free;
    end;
  end;

  Edit_FN.InitialDir := LastDir;
  Btn_OK.Enabled := ( Edit_FN.Text <> '' );

  Edit_FN.Filter := Format(
    'Palm files|*%s|All files (*.*)|*.*',
    [StdExtension] );

  CB_AutoInstall.Checked := AutoInstall;


end; // CREATE

procedure TForm_PalmExp.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile : TIniFile;
begin

  try
    // store config
    IniFile := TIniFile.Create( IniFN );
    try
      with IniFile do
      begin
        writestring( 'PalmExport', 'LastDir', LastDir );
        writebool( 'PalmExport', 'FixExtension', FixExtension );
        writestring( 'PalmExport', 'StdExtension', StdExtension );
        writebool( 'PalmExport', 'AutoInstall', AutoInstall );
        writebool( 'PalmExport', 'AutoName', AutoName );
      end;
    finally
      IniFile.Free;
    end;
  except
  end;

end; // FormCLOSE


procedure TForm_PalmExp.Edit_FNChange(Sender: TObject);
begin
  Btn_OK.Enabled := ( Edit_FN.Text <> '' );
end;

procedure TForm_PalmExp.Btn_PalmClick(Sender: TObject);
var
  NewFN, NewDir : string;
begin
  case ( sender as TBitBtn ).Tag of
    1 : NewDir := PalmInstallDir;
    2 : NewDir := GetFolderPath( fpPersonal );
    3 : NewDir := GetFolderPath( fpDesktop );
    else
    begin
      NewDir := extractfilepath( Application.Exename );
    end;
  end;

  NewFN := Edit_FN.Text;
  if ( NewFN = '' ) then
  begin
    Edit_FN.InitialDir := NewDir;
  end
  else
  begin
    UnquoteString( NewFN );
    Edit_FN.Text := ProperFolderName( NewDir ) + extractfilename( NewFN );
  end;

end;

procedure TForm_PalmExp.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  tmpext : string;
begin
  if ( ModalResult = mrOK ) then
  begin
    ExportFN := Edit_FN.Text;
    UnquoteString( ExportFN );
    if directoryexists( ExportFN ) then
    begin
      CanClose := false;
      Edit_FN.SetFocus;
      messagedlg( Format( '%s is a directory. Please enter a valid file name', [ExportFN] ), mtError, [mbOK], 0 );
      exit;
    end;

    tmpext := extractfileext( ExportFN );
    if ( tmpext = '' ) then
    begin
      ExportFN := ExportFN + StdExtension;
    end
    else
    begin
      if FixExtension then
      begin
        if ( comparetext( tmpext, StdExtension ) <> 0 ) then
        begin
          case messagedlg( Format( 'Palm data files should have the "%s" extension. The extension you used, "%s", is different. Correct the file extension', [StdExtension, tmpext] ), mtWarning, [mbYes,mbNo,mbCancel], 0 ) of
            mrYes : begin
              ChangeFileExt( ExportFN, StdExtension );
            end;
            mrCancel : begin
              CanClose := false;
              exit;
            end;
          end;
        end;
      end;
    end;
    if CanClose then
    begin
      LastDir := ExtractFilePath( ExportFN );
      AutoInstall := CB_AutoInstall.Checked;
      ExportTitle := trim( Edit_Title.Text );
      if ( ExportTitle = '' ) then
        ExportTitle := ExtractFileNameNoExt( ExportFN );
    end;
  end;
end; // CloseQuery

procedure TForm_PalmExp.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if ( AutoName and ( SuggestedName <> '' )) then
    Edit_FN.Text := Edit_FN.InitialDir + '\' + SuggestedName;

  try
    Edit_FN.SetFocus;
  except
  end;
end; // Activate

end.
