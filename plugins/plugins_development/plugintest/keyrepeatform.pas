unit keyrepeatform;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ShellAPI, StdCtrls, IniFiles, kn_Msgs,
  Clipbrd, ExtCtrls, ComCtrls, Spin;

type
  TForm_Key = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    HotKey: THotKey;
    Label2: TLabel;
    Spin_Max: TSpinEdit;
    CB_Delay: TCheckBox;
    Spin_Delay: TSpinEdit;
    CB_Infinite: TCheckBox;
    Button_Send: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_SendClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CB_DelayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HadESC : boolean;
    Running : boolean;
    KeyNote_Notification_Msg_ID : word;
    KeyNote_HWND : HWND;
    IniFN : string;
  end;

var
  ResidentPluginID : longint;
  Form_Key : TForm_Key;

implementation

{$R *.DFM}



procedure TForm_Key.FormCreate(Sender: TObject);
var
  section : string;
  IniFile : TIniFile;
begin
  KeyNote_Notification_Msg_ID := RegisterWindowMessage( KeyNote_WinMsgIdStr );
  Application.Handle := 0;
  HadESC := false;
  Running := false;

  IniFN := lowercase( extractfilepath( ParamStr( 0 )) + 'plugins\keyrepeat.ini' );

  section := 'options';
  if fileexists( IniFN ) then
  begin
    IniFile := TIniFile.Create( IniFN );
    try
      try
        with IniFile do
        begin
          Spin_Max.Value := readinteger( section, 'MaxCount', 5 );
          CB_Infinite.Checked := readbool( section, 'Infinite', false );
          CB_Delay.Checked := readbool( section, 'Delay', false );
          Spin_Delay.Value := readinteger( section, 'DelayTime', 100 );
        end;
      except
      end;
    finally
      IniFile.Free;
    end;
  end;

  Spin_Delay.Enabled := CB_Delay.Checked;

end;

procedure TForm_Key.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if (( key = 27 ) and ( shift = [] )) then
  begin
    if Running then
      HadESC := true
    else
      Close;
    Key := 0;
  end;

end;

procedure TForm_Key.FormClose(Sender: TObject; var Action: TCloseAction);
var
  section : string;
  IniFile : TIniFile;
begin

  // save settings
  section := 'options';
  IniFile := TIniFile.Create( IniFN );
  try
    with IniFile do
    begin
      writeinteger( section, 'MaxCount', Spin_Max.Value );
      writebool( section, 'Infinite', CB_Infinite.Checked );
      writebool( section, 'Delay', CB_Delay.Checked );
      writeinteger( section, 'DelayTime', Spin_Delay.Value );
    end;
  finally
    IniFile.Free;
  end;


  // Use the message we registered to notify KeyNote that the plugin
  // is shutting down. This allows keynote to free the library
  if ( KeyNote_Notification_Msg_ID <> 0 ) then
  begin
    PostMessage( Keynote_HWND, KeyNote_Notification_Msg_ID, KNT_MSG_PLUGIN_SHUTDOWN, ResidentPluginID );
  end;

  // IMPORTANT NOTE: The form cannot be freed here or anywhere within
  // the form's own events. In practice, the form needs not be freed
  // at all, because the memory manager will deallocate this memory
  // when the DLL is unloaded. However, the form CAN be freed safely
  // in the Finalization block of the unit.

end;


procedure TForm_Key.Button_SendClick(Sender: TObject);
var
  CopyData : TCopyDataStruct;
  msg : TKeyNoteMsg;
  count, max, delaytime : integer;
  DoDelay : boolean;
  Finished, Infinite : boolean;
begin

  HadESC := false;
  if ( HotKey.HotKey = 0 ) then exit;

  msg.strData := '';
  msg.intData1 := HotKey.HotKey;
  copydata.dwData := KNT_MSG_PERFORMKEY; // declared in "kn_Msgs.pas"
  copydata.cbData := sizeof( msg );
  copydata.lpData := @msg;
  DoDelay := CB_Delay.Checked;
  delaytime := Spin_Delay.Value;
  Max := Spin_Max.Value;
  Infinite := CB_Infinite.Checked;
  Finished := false;

  Button_Send.Enabled := false;
  Running := true;
  try
    try

      count := 0;
      repeat

        SendMessage( KeyNote_HWND,
          WM_COPYDATA,
          Handle,
          integer( @copydata ));

        if ( not Infinite ) then
          inc( count );

        Application.ProcessMessages;

        if DoDelay then
        begin
          sleep( delaytime );
          Application.ProcessMessages;
        end;

        Finished := ( HadESC or ( count >= max ));

      until Finished;

    except
      exit;
    end;
  finally
    Button_Send.Enabled := true;
    Running := false;
    HadESC := false;
  end;

end;


procedure TForm_Key.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  HadESC := true;
  CanClose := ( not Running );
end;

procedure TForm_Key.CB_DelayClick(Sender: TObject);
begin
  Spin_Delay.Enabled := CB_Delay.Checked;
end;

Initialization
  Form_Key := nil;

Finalization
  if assigned( Form_Key ) then
    Form_Key.Free;

end.
