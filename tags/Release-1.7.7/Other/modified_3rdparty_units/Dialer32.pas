////////////////////////////////////////////////////////////////////////////////
//                               TDialer Component                            //
//                                 for Win9x/NT                               //
//                    using VARIAN Async32 TComm to connect                   //
//                                                                            //
//                       (c) Alexander B. Bokovikov, 1999                     //
//       http://a-press.parad.ru/pc/bokovikov/delphi/comp/dialer32.zip        //
////////////////////////////////////////////////////////////////////////////////

unit Dialer32;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, CommInt;

type

  TComPort = (dpCOM1,dpCOM2,dpCOM3,dpCOM4);
  TDialMethod  = (dmTone,dmPulse);
  TSpeakerVolume = ( svSilent, svLow, svMedium, svHigh );

type
  TOnDialError = procedure ( sender : TObject; errstr : shortstring ) of  object;

const
  COMPORT_NAMES : array[TComPort] of string = (
    'COM1', 'COM2', 'COM3', 'COM4'
  );

  DIALMETHOD_NAMES : array[TDialMethod] of string = (
    'Tone', 'Pulse'
  );

  VOLUME_NAMES : array[TSpeakerVolume] of string = (
    'Silent', 'Low', 'Medium', 'High'
  );

  VOLUME_CHARS : array[TSpeakerVolume] of char = (
    '0', '1', '2', '3'
  );


type

  TDialer = class(TComponent)
  private
    { Private declarations }
    FComm         : TComm;
    FDialing      : boolean;
    FBuffer       : shortstring;
    FComPort      : TComPort;
    FOKReceived   : boolean;
    FErrReceived  : boolean;
    FNCReceived   : boolean;
    FBusiReceived : boolean;
    FError        : integer;
    FNumberToDial : shortstring;
    FConfirm      : boolean;
    FMethod       : TDialMethod;
    FInitStr      : shortstring;
    FVolume       : TSpeakerVolume;
    FWaitForTone  : boolean;
    FOnDefineProperties : TNotifyEvent;
    FOnNoDialTone : TNotifyEvent;
    FOnDialError : TOnDialError;
    FOnBusy : TNotifyEvent;
    FOnDial : TNotifyEvent;
    FOnHangUp : TNotifyEvent;
    FOnHangUpDone : TNotifyEvent;
    FOnOK : TNotifyEvent;
    FOnNC : TNotifyEvent;
    FOnErr : TNotifyEvent;
    FOnError : TNotifyEvent;
    FOnConnected : TNotifyEvent;
    FResBuffer : shortstring;
  protected
    { Protected declarations }
    procedure DataReceived(Sender: TObject; Count: integer);
    procedure SetComm(Value : TComm);
    procedure SetComPort(Value : TComPort);
    procedure CommError(Sender: TObject; ErrCode : integer);
    procedure ClearErr;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Dial;
    procedure HangUp(Wait : boolean);
    function TestCom(APort : byte) : boolean;
    procedure CloseCom;
    property Dialing : boolean read FDialing;
    property Error : integer read FError;
  published
    { Published declarations }
    property Comm : TComm read FComm
                 write SetComm;
    property ComPort : TComPort read FComPort
                 write SetComPort;
    property Confirm : boolean read FConfirm
                 write FConfirm;
    property InitStr : shortstring read FInitStr
                 write FInitStr;
    property Volume : TSpeakerVolume read FVolume
                 write FVolume;
    property WaitForTone : boolean read FWaitForTone
                 write FWaitForTone;
    property Method  : TDialMethod read FMethod
                 write FMethod;
    property NumberToDial : shortstring read FNumberToDial
                 write FNumberToDial;
    property OnDefineProperties : TNotifyEvent read FOnDefineProperties
                 write FOnDefineProperties;
    property OnNoDialTone : TNotifyEvent read FOnNoDialTone
                 write FOnNoDialTone;
    property OnBusy : TNotifyEvent read FOnBusy
                 write FOnBusy;
    property OnDial : TNotifyEvent read FOnDial
                 write FOnDial;
    property OnHangUp : TNotifyEvent read FOnHangUp
                 write FOnHangUp;
    property OnHangUpDone : TNotifyEvent read FOnHangUpDone
                 write FOnHangUpDone;
    property OnOKReceived : TNotifyEvent read FOnOK
                 write FOnOK;
    property OnNoCarrierReceived : TNotifyEvent read FOnNC
                 write FOnNC;
    property OnErrorReceived : TNotifyEvent read FOnErr
                 write FOnErr;
    property OnError : TNotifyEvent read FOnError
                 write FOnError;
    property OnDialError : TOnDialError read FOnDialError
                 write FOnDialError;
    property OnConnected : TNotifyEvent read FOnConnected
                 write FOnConnected;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Async32', [TDialer]);
end;

constructor TDialer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FComm := nil;
  FDialing := false;
  FVolume := svMedium;
  FWaitForTone := false;
  FOnDefineProperties := nil;
  FOnNoDialTone := nil;
  FOnBusy := nil;
  FOnDial := nil;
  FOnHangUp := nil;
  FOnOK := nil;
  FOnError := nil;
  FOnDialError := nil;
  FOnConnected := nil;
end;

destructor TDialer.Destroy;
begin
  inherited Destroy;
end;

procedure TDialer.SetComPort(Value : TComPort);
begin
  FComPort := Value;
  if FComm <> nil then
    case FComPort of
      dpCom1 : FComm.DeviceName := 'COM1';
      dpCom2 : FComm.DeviceName := 'COM2';
      dpCom3 : FComm.DeviceName := 'COM3';
      dpCom4 : FComm.DeviceName := 'COM4';
    end;
end;

procedure TDialer.SetComm(Value : TComm);
begin
  FComm := Value;
  FComm.BaudRate := br38400;
  FComm.Databits := da8;
  FComm.Parity := paNone;
  FComm.StopBits := sb10;
  FComm.OnRxChar := DataReceived;
  FComm.OnError := CommError;
  FComm.ReadTimeout := 1500; // 1000;
  FComm.WriteTimeout := 1500; // 1000;
  SetComPort(FComPort);
end;

procedure TDialer.DataReceived(Sender: TObject; Count: integer);
var
  Status : Integer;
  s : shortstring;
begin
  if Count = 0 then Exit;
  Status:=FComm.Read(s[1],Count);
  s[0] := chr(Status);
  FBuffer := FBuffer + s;
  if s[Length(s)] = #$0A then
  begin
    s := FBuffer;
    FResBuffer := s;
    FBuffer := '';
    s:=UpperCase(s);
    // writeln( '==>', s );
    if (Pos('NO'  ,s)<>0) and
       (Pos('DIAL',s)<>0) and
       (Pos('TONE',s)<>0)     then
      if Assigned(FOnNoDialTone) then FOnNoDialTone(Self)
                                 else
         raise Exception.Create('No Dialtone')
    else
    if Pos('BUSY',s)<>0 then
    begin
      FBusiReceived := true;
      if Assigned(FOnBusy) then FOnBusy(Self)
                           else
         raise Exception.Create('Line is busy')
    end
    else
    if pos( 'CONNECTED', s ) <> 0 then // [x] THIS NEVER HAPPENS
    begin
      if assigned( FOnConnected ) then
        FOnConnected( Self );
    end
    else
    if Pos('CARRIER',s)<>0 then
    begin
      FNCReceived := true;
      if Assigned(FOnNC) then FOnNC(Self);
    end
    else
    if Pos('OK',s)<>0 then
    begin
      FOKReceived := true;
      if Assigned(FOnOK) then FOnOK(Self);
    end
    else
    if Pos('ERR',s)<>0 then
    begin
      FErrReceived := true;
      if Assigned(FOnErr) then FOnErr(Self);
    end;
  end;
  // [MJ] removed the following, because it prevents
  // dial form from automatically closing. Dunno why.
  // Application.ProcessMessages;
end;

procedure TDialer.CommError(Sender: TObject; ErrCode : integer);
begin
  FError := ErrCode;
  if Assigned(FOnError) then FOnError(Self);
end;

function TDialer.TestCom(APort : byte) : boolean;
var
  s : shortstring;
  Status : Integer;
begin
  Result := false;
  if FComm = nil then Exit;
  CloseCom;
  s := 'COM1';
  s[4]:=Chr(49+APort);
  FComm.DeviceName := s;
  FError := 0;
  FComm.Open;
  if FComm.Enabled then {send test string}
  begin
    s:='AT'+^M^J;
    Status:=FComm.Write(s[1],Length(s));
    Result := Status > 0;
    FComm.Close;
  end
end;

procedure TDialer.CloseCom;
begin
  if FComm = nil then Exit;
  if FComm.Enabled then FComm.Close;
  FDialing := false;
end;

procedure TDialer.ClearErr;
begin
  FOKReceived := false;
  FErrReceived := false;
  FNCReceived := false;
  FBusiReceived := false;
end;

Var
 OldSC : TCursor;

procedure WaitOn;
begin
 Application.ProcessMessages;
 OldSC := Screen.Cursor;
 Screen.Cursor := crHourGlass;
end;

procedure WaitOff;
begin
 Screen.Cursor := OldSC;
end;

procedure TDialer.Dial;
var
  s : shortstring;
  c : shortstring;
  ps,pc : pointer;
  Status : Integer;
begin
  if FComm = nil then Exit;
  if Assigned(FOnDefineProperties) then FOnDefineProperties(Self);
  {Open Com Port}
  SetComPort(FComPort);
  FError := 0;
  FComm.Open;
  if not FComm.Enabled then
  begin
    s := FComm.DeviceName +' Unavailable'#0;
    c := 'Dialing error'#0;
    ps := @s[1];
    pc := @c[1];
    if assigned( FOnDialError ) then
      FOnDialError( self, s )
    else
      Application.MessageBox(ps,pc,MB_ICONHAND+MB_OK);
    Exit;
  end;
  {Under NT it needs to be done somewhy...}
  if (GetVersion and $80000000) = 0 then
  begin
    WaitOn;
    Sleep(2000);
    WaitOff;
  end;
  {Send modem initialization string, if any}
  if FInitStr<>'' then
  begin
    s:=FInitStr+^M^J;
    ClearErr;
    Status:= FComm.Write(s[1],Length(s));
    if Status<0 then
    begin
      c := 'Dialer error'#0;
      s := 'Cannot initialize modem'#0;
      ps := @s[1];
      pc := @c[1];
      if assigned( FOnDialError ) then
        FOnDialError( self, s )
      else
        Application.MessageBox(ps,pc,MB_ICONHAND+MB_OK);
      CloseCom;
      Exit;
    end
    else
      while not(FOKReceived or FErrReceived) do Application.ProcessMessages;
  end;
  if FConfirm then
  begin
    c := 'Dialer confirmation'#0;
    s := 'Ready to dial '+FNumberToDial+'. Continue?'#0;
    ps := @s[1];
    pc := @c[1];
   if Application.MessageBox(ps,pc,MB_ICONINFORMATION+MB_YESNO) <> ID_YES then
   begin
     CloseCom;
     Exit;
   end;
  end;
  {Create a string to send to modem}
  s:= 'ATX4L0DT' + FNumberToDial + ^M^J;
  if FMethod=dmPulse then s[8]:='P';
  s[6] := VOLUME_CHARS[FVolume];
  if FWaitForTone then insert( ',', s, 9 );
  // writeln( 'DIALING:' );
  // writeln( s );
  {Send phone number to modem}
  FDialing := true;
  ClearErr;
  Status:=FComm.Write(s[1],Length(s));
  if Status>=0 then
  begin
    if Assigned(FOnDial) then FOnDial(Self);
  end          else
  begin
    c := 'Dialing error'#0;
    s := 'Error while dialing the number'#0;
    ps := @s[1];
    pc := @c[1];
    if assigned( FOnDialError ) then
      FOnDialError( self, s )
    else
      Application.MessageBox(ps,pc,MB_ICONHAND+MB_OK);
    {Close communication port}
    if FComm.Enabled then HangUp(false);
  end;
end;

procedure TDialer.HangUp(Wait : boolean);
var s : shortstring;
begin
  if FComm = nil then Exit;
  if FComm.Enabled then
  begin
    if Assigned(FOnHangUp) then FOnHangUp(Self);
    if Wait then
    begin
      ClearErr;
      s := 'ATH'^M^J;
      repeat
        while FComm.Write(s[1],5) <> 5 do Application.ProcessMessages;
        if (GetVersion and $80000000) = 0 then Sleep(100);
        Application.ProcessMessages;
      until FNCReceived or FOKReceived or FBusiReceived;
    end;
    CloseCom;
    if Assigned(FOnHangUpDone) then FOnHangUpDone(Self);
    FDialing := false;
  end;
end;

end.
