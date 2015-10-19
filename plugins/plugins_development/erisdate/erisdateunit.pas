unit erisdateunit;

interface
uses SysUtils, Windows, ShellAPI,
  Controls, eris_datetime, kn_PluginBase,
  Clipbrd, Messages;

function KNTGetPluginName( buf : pointer; size : longint ) : longint stdcall;
function KNTGetPluginVersion : longint stdcall;
function KNTGetPluginDescription( buf : pointer; size : longint ) : longint stdcall;
function KNTConfigurePlugin( OwnerHWND : HWND ) : longint; stdcall;
function KNTGetPluginFeatures : longint; stdcall;
function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint; stdcall;
function KNTPluginCleanup : longint; stdcall;  

implementation
uses gf_Bits, IniFiles, erisdateconfig;

var
  OutTextPtr : pointer;
  OutTextSize : longint;
  DateAndOrTime : integer; // 0 = date; 1 = time; 2 = both
  DateLong : boolean;
  TimeLong : boolean;
  ReturnType : integer; // 0 = insert; 1 = dialog box; 2 = copy to clipboard

const
  _PLUGIN_NAME    = 'Discordian Calendar';
  _PLUGIN_VERSION = '5.23';
  _PLUGIN_DESCRIPTION = 'Discordian Calendar plugin for KeyNote. Hail Eris, All Hail Discordia! Fnord.';


const
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

procedure LoadConfig;
var
  IniFile : TIniFile;
  fn, section : string;
begin
  DateAndOrTime := 0;
  DateLong := true;
  TimeLong := true;
  ReturnType := 0; // insert in note text

  fn := lowercase( extractfilepath( ParamStr( 0 )) + 'plugins\' + 'erisdate.ini' );

  if ( not fileexists( fn )) then exit;

  IniFile := TIniFile.Create( fn );
  section := 'Fnord';

  try
    with IniFile do
    try
      DateAndOrTime := readinteger( section, 'DateAndOrTime', DateAndOrTime );
      DateLong := readbool( section, 'DateLong', DateLong );
      TimeLong := readbool( section, 'TimeLong', TimeLong );
      ReturnType := readinteger( section, 'Return', ReturnType );
    except
    end;
  finally
    IniFile.Free;
  end;

end; // LoadConfig

procedure StoreConfig;
var
  IniFile : TIniFile;
  fn, section : string;
begin
  fn := lowercase( extractfilepath( ParamStr( 0 )) + 'plugins\' + 'erisdate.ini' );

  IniFile := TIniFile.Create( fn );
  section := 'Fnord';

  try
    with IniFile do
    try
      writeinteger( section, 'DateAndOrTime', DateAndOrTime );
      writebool( section, 'DateLong', DateLong );
      writebool( section, 'TimeLong', TimeLong );
      writeinteger( section, 'Return', ReturnType );
    except
    end;
  finally
    IniFile.Free;
  end;
end; // StoreConfig


function GetTimeZoneInfo : integer;
var
  aTimeZoneInfo : TTimeZoneInformation;
  mode : integer;
begin
  result := 0;
  mode := GetTimeZoneInformation(aTimeZoneInfo);

  if ( mode in [TIME_ZONE_ID_UNKNOWN,TIME_ZONE_ID_STANDARD] ) then
  begin
     result := - ( aTimeZoneInfo.Bias DIV 60 );
  end
  else
  if ( mode = TIME_ZONE_ID_DAYLIGHT ) then
  begin
     result := - (( aTimeZoneInfo.Bias + aTimeZoneInfo.DaylightBias ) DIV 60 );
  end;
end; // GetTimeZoneInfo

function KNTGetPluginName( buf : pointer; size : longint ) : longint;
begin
  StrLCopy( Buf, _PLUGIN_NAME, size-1 );
  result := 0;
end; // KNTGetPluginName

function KNTGetPluginVersion : longint;
begin
  result := 1;
end; // KNTGetPluginVersion

function KNTGetPluginDescription( buf : pointer; size : longint ) : longint;
begin
  StrLCopy( Buf, _PLUGIN_DESCRIPTION, size-1 );
  result := 0;
end; // KNTGetPluginDescription

function KNTConfigurePlugin( OwnerHWND : HWND ) : longint;
var
  Form_ErisCfg: TForm_ErisCfg;
begin
  result := 0;
  if ( OwnerHWND = 0 ) then
    OwnerHWND := FindWindow( 'GFKeyNote10', nil );

  // Form_ErisCfg := TForm_ErisCfg.CreateParented( OwnerHWND );
  Form_ErisCfg := TForm_ErisCfg.Create( nil );

  try
    try
      with Form_ErisCfg do
      begin
        RB_What.ItemIndex := DateAndOrTime;
        CB_DateLong.Checked := DateLong;
        CB_TimeLong.Checked := TimeLong;
        RG_OnExec.ItemIndex := ReturnType;
      end;

      if ( Form_ErisCfg.ShowModal = mrOK ) then
      begin
        with Form_ErisCfg do
        begin
          DateAndOrTime := RB_What.ItemIndex;
          DateLong := CB_DateLong.Checked;
          TimeLong := CB_TimeLong.Checked;
          ReturnType := RG_OnExec.ItemIndex;
        end;
      end;

    except
    end
  finally
    Form_ErisCfg.Free;
  end;

end; // KNTConfigurePlugin

function KNTGetPluginFeatures : longint;
begin
  result := 0;
  result := BitOn( result, ord ( plOK ));
  result := BitOn( result, ord( plReturnsData ));

  case ReturnType of
    0 : begin { nothing to set, this is default behavior } end;
    1 : result := BitOn( result, ord( plWantsDlgBox ));
    2 : result := BitOn( result, ord( plReturnsClipboard ));
  end;

end; // KNTGetPluginFeatures

function KNTPluginExecute(
  AppHandle : THandle;
  OwnerHWND : HWND;
  RichEditHWND : HWND;
  ActiveFileName, ActiveNoteName : PChar;
  InText : PChar;
  var OutText : pointer ) : longint;
var
  s : string;
  EDate : TErisianDate;
  ETime : TErisianTime;
begin
  result := 0;

  OutText := nil;

  try

    GetErisianDate( false, EDate );
    GetErisianTime( GetTimeZoneInfo, 1, ETime );

    case DateAndOrTime of
      0 : begin // date only
        s := FormatErisianDate( DateLong, EDate );
      end;
      1 : begin // time only
        s := FormatErisianTime( TimeLong, ETime );
      end;
      2 : begin // both
        s := FormatErisianDate( DateLong, EDate ) + #13 +
             FormatErisianTime( TimeLong, ETime );
      end;
    end;

    OutTextSize := length( s );
    result := OutTextSize;

    if ReturnType = 2 then
    begin
      Clipboard.SetTextBuf( PChar( s ));
    end
    else
    begin
      GetMem( OutText, OutTextSize );
      move( s[1], OutText^, OutTextSize );
      OutTextPtr := OutText;
    end;

  except
    result := -1;
  end;

end; // KNTPluginExecute

function KNTPluginCleanup : longint;
begin
  result := 0;

  // deallocate any memory that was allocated in
  // KNTPluginExecute
  try
    if (( OutTextPtr <> nil ) and ( OutTextSize > 0 )) then
      FreeMem( OutTextPtr, OutTextSize );
  except
    // forgive and forget
  end;
end; // KNTPluginCleanup


Initialization
  OutTextPtr := nil;
  OutTextSize := 0;
  LoadConfig;

Finalization
  StoreConfig;

end.
