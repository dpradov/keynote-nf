unit GFTapiDial;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  WinTypes, WinProcs, StdCtrls, ExtCtrls,
  gf_files, GFTapiH, gfDialForm;

type
  TTapiCallDialog = ( tcdNone, tcdWin95 );
  TTapiDialMethod = ( tddTone, tddPulse );

type
  TGFTapiDial = class( TComponent )
  private
    fDialMethod : TTapiDialMethod;
    fCallDialog : TTapiCallDialog;
    fPhoneNumber : string;
    fDialToneDetect : boolean;
    fTapiLineApp : THLineApp;
    fTapiLine : THLine;
    fTapiCall : THCall;
    fCallParams : TLineCallParams;
    fTAPIStatus : TStringList;
    fTapiVersion : longint;
    fBusy : boolean;
    fDevCnt : Longint; // number of devices
    fModemPrefix : string; // prefixed to dial string
    fDialString : string;
    fSaveStatus : boolean;
    fSaveStatusFN : string;
    fDialForm : TForm_Dial;
    fCallName : string;
    fDialFormShow : boolean;
    fDialFormIconShow : boolean;
    fDialFormIconAnimate : boolean;
    fDialFormTitle : string;
    fDialFormAutoClose : boolean;

    procedure TerminateTapi;
    procedure InitializeTapi;
    procedure SetDialMethod( const AMethod : TTapiDialMethod );
    procedure SetCallDialog( const ADialog : TTapiCallDialog );
    procedure SetDialTone( const ATone : boolean );
    procedure SetPhoneNumber( const ANumber : string );
    procedure SetShowDialForm( const AShow : boolean );
    function GetDialString : string;

  public
    property TAPIStatus : TStringList read fTAPIStatus;
    property Busy : boolean read fBusy;
    property DevCnt : longint read fDevCnt;
    property TapiVersion : longint read fTAPIVersion;
    property DialString : string read GetDialString;

    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure Dial;
    procedure HangUp;
    procedure ClearStatus;
    procedure SaveStatusToFile( FN : string );

  published
    property DialMethod : TTapiDialMethod read fDialMethod write SetDialMethod;
    property CallDialog : TTapiCallDialog read fCallDialog write SetCallDialog;
    property PhoneNumber : string read fPhoneNumber write SetPhoneNumber;
    property DialToneDetect : boolean read fDialToneDetect write SetDialTone;
    property ModemPrefix : string read fModemPrefix write fModemPrefix;
    property SaveStatus : boolean read fSaveStatus write fSaveStatus default true;
    property SaveStatusFN : string read fSaveStatusFN write fSaveStatusFN;

    property CallName : string read fCallName write fCallName;
    property DialFormShow : boolean read fDialFormShow write SetShowDialForm default true;
    property DialFormIconShow : boolean read fDialFormIconShow write fDialFormIconShow default true;
    property DialFormIconAnimate : boolean read fDialFormIconAnimate write fDialFormIconAnimate default false;
    property DialFormTitle : string read fDialFormTitle write fDialFormTitle;
    property DialFormAutoClose : boolean read fDialFormAutoClose write fDialFormAutoClose default false;
  end;

procedure lineCallBack( hDevice, dwMsg, dwCallbackInstance,
        dwParam1, dwParam2, dwParam3: LongInt  ); stdcall;
procedure MsgToDialForm( const MsgID : integer );


procedure Register;

implementation

var
  TAPIBuf : array[0..1023] of char;
  CallInfo: TLineCallInfo absolute TAPIBuf;
  _TS : TStringList;
  _DFHANDLE : HWnd;

procedure TGFTapiDial.InitializeTapi;
var
  extid : TLineExtensionID;
  tver : longint;
begin
  fTAPIStatus.Add( 'Initializing TAPI...' );
  fBusy := true;
  FillChar( fCallParams, sizeof( fCallParams ), 0 );

  try
    try
      with fCallParams do
      begin
        dwTotalSize := sizeof( fCallParams );
        dwBearerMode := LINEBEARERMODE_VOICE;
        if ( fCallDialog = tcdWin95 ) then
          dwMediaMode := LINEMEDIAMODE_INTERACTIVEVOICE
        else // tcdNone
          dwMediaMode := LINEMEDIAMODE_DATAMODEM;
      end;

      if ( LineInitialize( fTapiLineApp, HInstance, @lineCallback, nil, fDevCnt ) < 0 ) then
      begin
        fTapiLineApp := 0;
        fTAPIStatus.Add( '! LineInitialize failed.' );
        exit;
      end;

      if ( fDevCnt = 0 ) then
      begin
        lineShutDown( fTapiLineApp );    //No TAPI devices
        fTapiLineApp := 0;
        fTAPIStatus.Add( '! No TAPI devices' );
        exit;
      end;

      if ( lineNegotiateAPIVersion( fTapiLineApp, 0, $00010000, $10000000, tver, extid ) < 0 ) then
      begin
        lineShutDown( fTapiLineApp );
        fTapiLineApp := 0;
        fTAPIStatus.Add( '! LineNegotiateAPIVersion failed.' );
        exit;
      end;

      fTAPIVersion := tver;

      if ( lineOpen( fTapiLineApp, LINEMAPPER, fTapiLine, tver, 0, 0, LINECALLPRIVILEGE_NONE, 0, fCallParams ) < 0 ) then
      begin
        lineShutDown( fTapiLineApp );
        fTapiLineApp := 0;
        fTapiLine := 0;
        fTAPIStatus.Add( '! LineOpen failed.' );
        exit;
      end;

      if ( fTapiLine = 0 ) then
      begin
        fTAPIStatus.Add( '! Unexpected error: TAPILine is 0.' );
        exit;
      end;
    except
      on E : Exception do
      begin
        messagedlg( 'Cannot Initialize TAPI: ' + E.Message, mtError, [mbOK], 0 );
        fTAPIStatus.Add( '!! Exception in Initialize TAPI: ' + E.Message );
        if fSaveStatus then SaveStatusToFile( '' );
        exit;
      end;
    end;
  finally
    fBusy := false;
  end;
  fTAPIStatus.Add( 'TAPI initialized.' );
end; // InitializeTapi

procedure TGFTapiDial.TerminateTapi;
begin
  fTAPIStatus.Add( 'Terminating TAPI...' );
  fBusy := true;
  try
    try
      if ( fTapiLine <> 0 ) then
        lineClose( fTapiLine );
      if ( fTapiLineApp <> 0 ) then
        lineShutDown( fTapiLineApp );
    except
      on E : exception do
      begin
        messagedlg( 'Cannot terminate TAPI: ' + E.Message, mtError, [mbOK], 0 );
        fTAPIStatus.Add( '!! Exception in Terminate TAPI: ' + E.Message );
        if fSaveStatus then SaveStatusToFile( '' );
      end;
    end;
  finally
    fTAPIVersion := 0;
    fDevCnt := 0;
    fBusy := false;
  end;
  fTAPIStatus.Add( 'TAPI terminated.' );
end; // TerminateTapi

constructor TGFTapiDial.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  fBusy := false;
  fPhoneNumber := '';
  fDialMethod := tddPulse;
  fDialToneDetect := true;
  fCallDialog := tcdNone; // tcdWin95;
  fTAPIStatus := TStringList.Create;
  _TS := fTAPIStatus;
  fDialForm := TForm_Dial.Create( self );
  fTapiLine := 0;
  fTapiLineApp := 0;
  fTAPIVersion := 0;
  fDevCnt := 0;
  fModemPrefix := '';
  fDialString := '';
  fSaveStatus := false;
  fSaveStatusFN := '';
  fCallName := '';
  DialFormShow := true;
  fDialFormIconShow := true;
  fDialFormIconAnimate := false;
  fDialFormTitle := _DIALFORMTITLE;
  fDialFormAutoClose := false;
  InitializeTapi;
end; // CREATE

destructor TGFTapiDial.Destroy;
begin
  TerminateTapi;
  if fSaveStatus then SaveStatusToFile( '' );
  if assigned( fTAPIStatus ) then fTAPIStatus.Free;
  if assigned( fDialForm ) then fDialForm.Free;
  inherited Destroy;
end; // DESTROY

procedure TGFTapiDial.Dial;
var
  c : array[0..30] of char;
  WasError : boolean;
begin
  if ( fPhoneNumber = '' ) then exit;
  WasError := false;
  fBusy := true;
  GetDialString; // sets fDialString
  fTAPIStatus.Add( 'Dialing: ' + fDialString );

  try
    with fDialForm do
    begin
      IconVisible := fDialFormIconShow;
      IconAnimate := fDialFormIconAnimate;
      Caption := fDialFormTitle;
      CallNumber := fPhoneNumber;
      CallName := fCallName;
      myAutoClose := fDialFormAutoClose;
    end;
  except;
    SetShowDialForm( false );
  end;

  try
    try
      StrPCopy( c, fDialString );
      if ( lineMakeCall( fTapiLine, fTapiCall, c, 0, fCallParams ) < 0 ) then
      begin
        fTAPIStatus.Add( '! LineMakeCall failed.' );
        WasError := true;
      end;
      //if calling with LINEMEDIAMODE_DATAMODEM, must show HangUp dialog
      if WasError then
      begin
        TerminateTAPI;
        InitializeTAPI;
        exit;
      end;
      if ( fCallDialog = tcdNone ) then
      begin
        if fDialFormShow then
          fDialForm.ShowModal
        else
          messagedlg( 'Calling ' + fPhoneNumber + #13#13 + 'Pick the receiver and click OK', mtInformation, [mbOK], 0 );
        HangUp;
      end;
    except
      on E : Exception do
      begin
        fTAPIStatus.Add( '!! Exception in Dial: ' + E.message );
        showmessage( 'Exception in DIAL: ' + E.Message );
      end;
    end;
  finally
    fBusy := false;
    fTAPIStatus.Add( 'Done dialing.');
  end;
end; // Dial

procedure TGFTapiDial.HangUp;
begin
  fBusy := true;
  fTAPIStatus.Add( 'Hanging up...' );
  try
    if ( LineDrop( fTapiCall, nil, 0 ) < 0 ) then
    begin
      fTAPIStatus.Add( '! LineDrop failed.' );
    end
    else
    begin
      fTAPIStatus.Add( 'Line dropped.' );
    end;
  finally
    fBusy := false;
    fTAPIStatus.Add( 'Hung up.' );
  end;
end; // HangUp

procedure TGFTapiDial.SetDialMethod( const AMethod : TTapiDialMethod );
begin
  if ( AMethod = fDialMethod ) then exit;
  fDialMethod := AMethod;
  if ( fDialMethod = tddTone ) then
    fTAPIStatus.Add( 'Dial method set to TONE' )
  else
    fTAPIStatus.Add( 'Dial method set to PULSE' );
end; // SetDialMethod

procedure TGFTapiDial.SetCallDialog( const ADialog : TTapiCallDialog );
begin
  if ( ADialog = fCallDialog ) then exit;
  fCallDialog := ADialog;
  if ( fCallDialog = tcdWin95 ) then
    fTAPIStatus.Add( 'Call dialog changed to Win95' )
  else
    fTAPIStatus.Add( 'Call dialog changed to None' );
  TerminateTapi;
  InitializeTapi;
end; // SetCallDialog

procedure TGFTapiDial.SetDialTone( const ATone : boolean );
begin
  if ( fDialToneDetect = ATone ) then exit;
  fDialToneDetect := ATone;
  if fDialToneDetect then
    fTAPIStatus.Add( 'Dial tone detection turned ON' )
  else
    fTAPIStatus.Add( 'Dial tone detection turned OFF' );
end; // SetDialTone

procedure TGFTapiDial.SetPhoneNumber( const ANumber : string );
begin
  if ( ANumber = fPhoneNumber ) then exit;
  fPhoneNumber := ANumber;
  fTAPIStatus.Add( 'Phone number set to ' + fPhoneNumber );
end; // SetPhoneNumber

procedure TGFTapiDial.ClearStatus;
begin
  fTAPIStatus.Clear;
end; // ClearStatus

function TGFTapiDial.GetDialString : string;
begin
  fDialString := fModemPrefix;
  if ( fDialMethod = tddTone ) then
    fDialString := fDialString + 'T'
  else
    fDialString := fDialString + 'P';
  if fDialToneDetect then
    fDialString := fDialString + 'W';
  fDialString := fDialString + fPhoneNumber;
  result := fDialString;
end; // GetDialString;

procedure TGFTapiDial.SaveStatusToFile( FN : string );
var
  i : integer;
  f : textfile;
begin
  if ( FN = '' ) then
    FN := fSaveStatusFN;
  if ( FN = '' ) then
    FN := lowercase( extractfilepath( paramstr( 0 )) + 'tapilog.log' );
  assignfile( f, FN );
  if fileexists( FN ) then
    append( f )
  else
    rewrite( f );
  writeln( f, '------ STATUS BEGIN ------' );
  if ( fTAPIStatus.Count > 0 ) then
    for i := 0 to pred( fTAPIStatus.Count ) do
      writeln( f, fTAPIStatus[i] );
  writeln( f, '------ STATUS END ------', #13#10 );
  closefile( f );
end; // SaveStatusToFile

procedure TGFTapiDial.SetShowDialForm( const AShow : boolean );
begin
  fDialFormShow := AShow;
  if fDialFormShow then
    _DFHANDLE := fDialForm.Handle
  else
    _DFHANDLE := 0;
end; // SetShowDialForm

{ ----------------------------------------------- }

procedure Register;
begin
  RegisterComponents( 'System', [TGFTapiDial] );
end;


procedure lineCallback( hDevice, dwMsg, dwCallbackInstance,
        dwParam1, dwParam2, dwParam3: LongInt ); stdcall;
var
   s: string;
   hCall: THCall;
begin

  try
    try
      if ( dwMsg = LINE_REPLY ) then { result of LineMakeCall }
      begin
        if ( dwParam2 < 0 ) then
        begin
          if assigned( _TS ) then _TS.Add( '@ Reply error' );
          MsgToDialForm( WM_DFCALLFAIL );
        end
        else
        begin
          if assigned( _TS ) then _TS.Add( '@ LINE REPLY OK' );
          MsgToDialForm( WM_DFREPLYOK );
        end;
      end
      else
      begin
        if ( dwMsg = LINE_CALLSTATE ) then
        begin
            { change in line state }
            hCall := THCall(hDevice);
            case dwParam1 of
               LINECALLSTATE_IDLE: //call finished
                  if hcall <> 0 then begin
                     lineDeallocateCall(hCall);//deallocating call
                     if assigned( _TS ) then _TS.Add( '@ Idle - call deallocated' );
                     MsgToDialForm( WM_DFCALLHUP );
                  end;
               LINECALLSTATE_CONNECTED:{ Service connected }
                  if hCall <> 0 then begin
                     s := 'Connected: ';
                     callinfo.dwTotalSize := 1024;
                     if lineGetCallInfo(hCall, callinfo) = 0 then
                        if callinfo.dwAppNameSize > 0 then
                           s := s + ( TAPIBuf + callinfo.dwAppNameOffset);
                     if assigned( _TS ) then _TS.Add( '@ ' + s );
                     MsgToDialForm( WM_DFCONNECT );
                  end;
               LINECALLSTATE_PROCEEDING:
               begin
                if assigned( _TS ) then _TS.Add( '@ Proceeding' );
                MsgToDialForm( WM_DFCALLPROG );
               end;
               LINECALLSTATE_DIALING:
               begin
                if assigned( _TS ) then _TS.Add( '@ Dialing' );
                MsgToDialForm( WM_DFCALLINIT );
               end;
               LINECALLSTATE_DISCONNECTED:
               begin
                  s := 'Disconnected: ';
                  if dwParam2 = LINEDISCONNECTMODE_NORMAL then
                     s := s + 'normal'
                  else if dwParam2 = LINEDISCONNECTMODE_BUSY then
                     s := s + 'busy';
                  if assigned( _TS ) then _TS.Add( '@ ' + s );
                  MsgToDialForm( WM_DFDISCONNECT );
                  // MsgToDialForm( WM_CLOSE );
               end;
               LINECALLSTATE_BUSY:
               begin
                if assigned( _TS ) then _TS.Add( '@ Line BUSY' );
                MsgToDialForm( WM_DFBUSY );
                // MsgToDialForm( WM_CLOSE );
               end;
            end;
          end;
      end;
    except
      on E : Exception do
      begin
        if assigned( _TS ) then _TS.Add( '!! Exception in LineCallBack: ' + E.Message );
        showmessage( 'Exception in LINECALLBACK: ' + E.Message );
      end;
    end;
  finally
  end;
end; // lineCallBack

procedure MsgToDialForm( const MsgID : integer );
begin
  if ( _DFHANDLE <> 0 ) then
    PostMessage( _DFHANDLE, MsgID, 0, 0 );
end; // MsgToDialForm

initialization
  _TS := nil;

end.
