unit keycodesform;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ShellAPI, StdCtrls, kn_Msgs,
  Clipbrd, ExtCtrls;

type
  TForm_Key = class(TForm)
    Panel1: TPanel;
    LB: TLabel;
    LB_Key: TLabel;
    Panel2: TPanel;
    Button_Copy: TButton;
    Button_Ins: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_CopyClick(Sender: TObject);
    procedure Button_InsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HadESC : boolean;
    KeyNote_Notification_Msg_ID : word;
    KeyNote_HWND : HWND;
  end;

function ShiftStateToStr( const Shift: TShiftState ) : string;
function StrToShiftState( const s : string ) : TShiftState;

var
  ResidentPluginID : longint;
  Form_Key : TForm_Key;

implementation

{$R *.DFM}



procedure TForm_Key.FormCreate(Sender: TObject);
begin
  KeyNote_Notification_Msg_ID := RegisterWindowMessage( KeyNote_WinMsgIdStr );
  Application.Handle := 0;
  HadESC := false;
  with LB_Key.Font do
  begin
    Name := 'Arial';
    Style := [fsBold];
    Size := 22;
    Color := clNavy;
  end;
end;

procedure TForm_Key.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if (( key = 27 ) and ( shift = [] )) then
  begin
    if HadESC then
      Close;
    HadESC := true;
  end
  else
  begin
    HadESC := false;
  end;

  LB_Key.Caption :=Format(
    '%d|%s',
    [Key,ShiftStateToStr( Shift )]
  );


  Key := 0;
end;

procedure TForm_Key.FormClose(Sender: TObject; var Action: TCloseAction);
begin

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

function ShiftStateToStr( const Shift: TShiftState ) : string;
begin
  result := '';
  if ( ssShift in Shift ) then
    result := result + 'S';
  if ( ssCtrl in Shift ) then
    result := result + 'C';
  if ( ssAlt in Shift ) then
    result := result + 'A';
  if ( ssLeft in Shift ) then
    result := result + 'L';
  if ( ssRight in Shift ) then
    result := result + 'R';
  if ( ssMiddle in Shift ) then
    result := result + 'M';
  if ( ssDouble in Shift ) then
    result := result + 'D';
end; // ShiftStateToStr

function StrToShiftState( const s : string ) : TShiftState;
begin
  result := [];
  if pos( 'S', s ) > 0 then
    include( result, ssShift );
  if pos( 'C', s ) > 0 then
    include( result, ssCtrl );
  if pos( 'A', s ) > 0 then
    include( result, ssAlt );
  if pos( 'L', s ) > 0 then
    include( result, ssLeft );
  if pos( 'R', s ) > 0 then
    include( result, ssRight );
  if pos( 'M', s ) > 0 then
    include( result, ssMiddle );
  if pos( 'D', s ) > 0 then
    include( result, ssDouble );
end;



procedure TForm_Key.Button_CopyClick(Sender: TObject);
begin
  Clipboard.SetTextBuf( PChar( LB_Key.Caption ));
end;

procedure TForm_Key.Button_InsClick(Sender: TObject);
var
  CopyData : TCopyDataStruct;
  msg : TKeyNoteMsg;
begin

  msg.strData := LB_Key.Caption;
  copydata.dwData := KNT_MSG_INSERTTEXT;

  copydata.cbData := sizeof( msg );
  copydata.lpData := @msg;

  SendMessage( KeyNote_HWND,
    WM_COPYDATA,
    Handle,
    integer( @copydata ));


  msg.strData := '';
  msg.intData1 := _CARET_RIGHT;
  copydata.dwData := KNT_MSG_MOVECARET;

  copydata.cbData := sizeof( msg );
  copydata.lpData := @msg;

  SendMessage( KeyNote_HWND,
    WM_COPYDATA,
    Handle,
    integer( @copydata ));



end;

Initialization
  Form_Key := nil;

Finalization
  if assigned( Form_Key ) then
    Form_Key.Free;

end.
