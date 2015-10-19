unit scratchpadform;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, IniFiles,
  ShellAPI, StdCtrls, kn_Msgs, RxRichEd,
  ExtCtrls, Placemnt, Menus, TopWnd;

type
  TForm_RTF = class(TForm)
    FormPlacement: TFormPlacement;
    Panel1: TPanel;
    RTF: TRxRichEdit;
    Menu_RTF: TPopupMenu;
    RTFMUndo: TMenuItem;
    RTFMRepeatCmd: TMenuItem;
    N20: TMenuItem;
    RTFMCut: TMenuItem;
    RTFMCopy: TMenuItem;
    RTFMPaste: TMenuItem;
    RTFMDelete: TMenuItem;
    N19: TMenuItem;
    Selectall1: TMenuItem;
    RTFMWordwrap: TMenuItem;
    N1: TMenuItem;
    RTFM_OnTop: TMenuItem;
    StayOnTop: TTopMostWindow;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RTFMUndoClick(Sender: TObject);
    procedure RTFMRepeatCmdClick(Sender: TObject);
    procedure RTFMCutClick(Sender: TObject);
    procedure RTFMCopyClick(Sender: TObject);
    procedure RTFMPasteClick(Sender: TObject);
    procedure RTFMDeleteClick(Sender: TObject);
    procedure Selectall1Click(Sender: TObject);
    procedure RTFMWordwrapClick(Sender: TObject);
    procedure RTFM_OnTopClick(Sender: TObject);
    procedure RTFKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

  private
    { Private declarations }
    procedure CreateParams( var Params : TCreateParams ); override;
  public
    { Public declarations }
    KeyNote_Notification_Msg_ID : word;
    KeyNote_HWND : HWND;
    IniFN : string;
  end;

var
  ResidentPluginID : longint;
  Form_RTF : TForm_RTF;
  MyAppHandle : THandle;

implementation

{$R *.DFM}


procedure TForm_RTF.FormCreate(Sender: TObject);
var
  fn : string;
  IniFile : TIniFile;
begin
  // Get Windows message ID, so we can tell KeyNote
  // when we shut down (in FormClose)
  KeyNote_Notification_Msg_ID := RegisterWindowMessage( KeyNote_WinMsgIdStr );

  // Application.Handle := MyAppHandle;
  Application.Handle := 0;

  RTF.Font.Size := 10;

  // load config
  IniFN := lowercase( extractfilepath( ParamStr( 0 )) + 'plugins\scratchpad.ini' );

  if fileexists( IniFN ) then
  begin
    IniFile := TIniFile.Create( IniFN );
    try
      with IniFile do
      begin
        RTFMWordwrap.Checked := readbool( 'options', 'wordwrap', true );
        RTFM_OnTop.Checked := readbool( 'options', 'ontop', false );
      end;
    finally
      IniFile.Free;
    end;
  end;

  FormPlacement.IniFileName := INIFN;

  RTF.WordWrap := RTFMWordwrap.Checked;
  StayOnTop.AlwaysOnTop := RTFM_OnTop.Checked;

  // load scratchpad RTF file
  fn := changefileext( IniFN, '.rtf' );
  if fileexists( fn ) then
  try
    RTF.Lines.LoadFromFile( fn );
  except
  end;
  RTF.SelStart := 0;

end;

procedure TForm_RTF.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if (( key = 27 ) and ( shift = [] )) then
  begin
    Key := 0;
    // fancy way to minimize, since we have no
    // Application here
    postmessage( self.handle, WM_SYSCOMMAND, SC_MINIMIZE, 0 );
  end;

end;

procedure TForm_RTF.FormClose(Sender: TObject; var Action: TCloseAction);
var
  IniFile : TIniFile;
begin

  try
    // save scratchpad file
    RTF.Lines.SaveToFile( changefileext( IniFN, '.rtf' ));
  except
  end;

  try
    // store config
    IniFile := TIniFile.Create( IniFN );
    try
      with IniFile do
      begin
        writebool( 'options', 'wordwrap', RTFMWordwrap.Checked );
        writebool( 'options', 'ontop', RTFM_OnTop.Checked );
      end;
    finally
      IniFile.Free;
    end;
  except
  end;


  // Use the message we registered to notify KeyNote that the plugin
  // is shutting down. This allows KeyNote to free the library properly.
  if ( KeyNote_Notification_Msg_ID <> 0 ) then
  begin
    PostMessage(
      Keynote_HWND, // KeyNote main form handle we got in KNTPluginExecute
      KeyNote_Notification_Msg_ID, // Windows message ID we registered
      KNT_MSG_PLUGIN_SHUTDOWN, // declared in "kn_Msgs.pas"
      ResidentPluginID // the plugin ID number we received via KNTSetPluginID
    );
  end;

  // IMPORTANT NOTE: The form cannot be freed here or anywhere within
  // the form's own events. In practice, the form needs not be freed
  // at all, because the memory manager will deallocate this memory
  // when the DLL is unloaded. However, the form CAN be freed safely
  // in the Finalization block of the unit.

end;



procedure TForm_RTF.RTFMUndoClick(Sender: TObject);
begin
  RTF.Undo;
end;

procedure TForm_RTF.RTFMRepeatCmdClick(Sender: TObject);
begin
  RTF.Redo;
end;

procedure TForm_RTF.RTFMCutClick(Sender: TObject);
begin
  RTF.CutToClipboard;
end;

procedure TForm_RTF.RTFMCopyClick(Sender: TObject);
begin
  RTF.CopyToClipboard;
end;

procedure TForm_RTF.RTFMPasteClick(Sender: TObject);
begin
  RTF.PasteFromClipboard;
end;

procedure TForm_RTF.RTFMDeleteClick(Sender: TObject);
begin
  RTF.Perform( WM_KEYDOWN, VK_DELETE, 0 );
  RTF.Perform( WM_KEYUP, VK_DELETE, 0 );
end;

procedure TForm_RTF.Selectall1Click(Sender: TObject);
begin
  RTF.SelectAll;
end;

procedure TForm_RTF.RTFMWordwrapClick(Sender: TObject);
begin
  RTFMWordwrap.Checked := ( not RTFMWordwrap.Checked );
  RTF.WordWrap := RTFMWordwrap.Checked;
end;

procedure TForm_RTF.RTFM_OnTopClick(Sender: TObject);
begin
  RTFM_OnTop.Checked := ( not RTFM_OnTop.Checked );
  StayOnTop.AlwaysOnTop := RTFM_OnTop.Checked;
end;

procedure TForm_RTF.RTFKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (( Shift = [] ) and ( key = VK_F1 )) then
  begin
    key := 0;
    // Application.MessageBox( PChar( inttostr( application.handle )), 'Scratchpad', MB_OK+MB_ICONASTERISK+MB_DEFBUTTON1+MB_APPLMODAL);
  end;
end;

procedure TForm_RTF.CreateParams( var Params : TCreateParams );
begin
  inherited CreateParams( Params );
  // Params.ExStyle := Params.exstyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
  Params.WinClassName := 'KNTScratchpad_10';


  (*
  params.exstyle := params.exstyle or WS_EX_TOOLWINDOW;
  params.style := params.style or WS_POPUP;
  *)
end; // CreateParams


Initialization
  Form_RTF := nil;

Finalization
  try
    if assigned( Form_RTF ) then
      Form_RTF.Free;
  finally
    Application.Handle := 0;
  end;

end.
