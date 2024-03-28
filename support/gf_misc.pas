unit gf_misc;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

{$I gf_base.inc}

interface
uses
   Winapi.Windows,
   Winapi.ShellAPI,
   Winapi.Messages,
   System.Win.Registry,
   System.Classes,
   System.SysUtils,
   System.DateUtils,
   System.StrUtils,
   Vcl.Graphics;



resourcestring
  STR_minute = 'minute';
  STR_minutes = 'minutes';
  STR_hour = 'hour';
  STR_hours = 'hours';
  STR_day = 'day';
  STR_days = 'days';
  STR_week = 'week';
  STR_weeks = 'weeks';
  STR_ERR_OUTOFRESOURCES = 'The operating system is out of memory or resources.';
  STR_ERROR_FILE_NOT_FOUND = 'The specified file was not found.';
  STR_ERROR_PATH_NOT_FOUND = 'The specified path was not found.';
  STR_ERROR_BAD_FORMAT = 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
  STR_SE_ERR_ACCESSDENIED = 'The operating system denied access to the specified URL.';
  STR_SE_ERR_ASSOCINCOMPLETE = 'The filename association is incomplete or invalid.';
  STR_SE_ERR_DDEBUSY = 'The DDE transaction could not be completed because other DDE transactions were being processed.';
  STR_SE_ERR_DDEFAIL = 'The DDE transaction failed.';
  STR_SE_ERR_DDETIMEOUT = 'The DDE transaction could not be completed because the request timed out.';
  STR_SE_ERR_DLLNOTFOUND = 'The specified dynamic-link library was not found.';
  STR_SE_ERR_NOASSOC = 'There is no application associated with the given filename extension.';
  STR_SE_ERR_OOM = 'There was not enough memory to complete the operation.';
  STR_SE_ERR_SHARE = 'A sharing violation occurred';
  STR_UNKNOWN_ERROR = 'Unknown error.';


type
  String255 = string[255];
  String127 = string[127];
  String50   = string[50];
  String5   = string[5];
  SchemeString = string[20];

const
  fpRootKey = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  fpFavorites : String50 = 'FAVORITES';
  fpPrograms  : String50 = 'PROGRAMS';
  fpStartup   : String50 = 'STARTUP';
  fpDesktop   : String50 = 'DESKTOP';
  fpPersonal  : String50 = 'PERSONAL';

const
  cnMaxEnvVarValueSize = 250;
  BOOLARRAY : array[false..true] of string = ( 'No', 'Yes' );
  TOGGLEARRAY : array[false..true] of string = ( 'Off', 'On' );

type
   // used for storing user's font preferences in INI files
  TFontProperties = packed record
    FBGColor : TColor;
    FCharset : TFontCharset;
    FColor : TColor;
    FName : string[32]; //TFontName;
    FPitch : TFontPitch;
    FSize : integer;
    FStyle : TFontStyles;
  end;
  TStrFontProperties = record
    section, fBGColor, fCharset, fColor,
    fName, fPitch, fSize, fStyle : string;
  end;

const
   FontPropertiesIniStr : TStrFontProperties = (
       section  : 'FontProperties';
       fbgcolor : 'BGColor';
       fcharset : 'Charset';
       fcolor   : 'Color';
       fname    : 'Name';
       fpitch   : 'Pitch';
       fsize    : 'Size';
       fstyle   : 'Style'
   );

const
  sstrBold      = 'bold';
  sstrItalic    = 'italic';
  sstrUnderline = 'underline';
  sstrStrikeout = 'strikeout';


type
  SetOfTFontStyle = set of TFontStyle;

const
  // used to detect if browser is running
  MSIEClass1 = 'IEFrame';
  MSIEClass2 = 'Internet Explorer_Hidden';
  NetscapeClass = 'NetscapeSlaveClass';

type
  TTrinaryCompare = ( trinGreater, trinEqual, trinSmaller );

const
   partsOfNumbers = '0123456789.,';
   digitsNumbers = '0123456789';

function RunsOnWindowsNT : boolean;
function GetCommandInterpreter : string;
function TranslateShellExecuteError( const ErrCode : integer ) : string;
function IsIE4Installed : Boolean;
function TColorToHTMLColor( const cl: TColor): string;
function UnixTimeToDateTime(UnixTime: Integer): TDateTime;
function RFC1123_Date(aDate : TDateTime) : String;
function DoTrinaryCompare( const val1, val2 : longint ) : TTrinaryCompare;
procedure SecondsToHMS( TotalSec : integer; var Hour, Min, Sec : word );

function ShiftStateToHotKey( AState : TShiftState ) : word;
function ShiftStateToStr( const Shift: TShiftState ) : string;
function StrToShiftState( const s : string ) : TShiftState;


function StringIsEmpty( const s : string ) : boolean;

function IsWord( const w : string ) : boolean;
function GetExtAlphChar( aChar : char ) : char;
Function FontStyleToStr( const F : SetOfTFontStyle {TFontProperties} ) : string;
Function StrToFontStyle( S : string ) : SetOfTFontStyle;
function FontPropertiesToStr( const font : TFontProperties ) : string;
procedure SetDefaultFont( var theFont : TFontProperties );


procedure FontPropertiesToFont( const fp : TFontProperties; aFont : TFont; dpi: integer = 96 );
procedure FontToFontProperties( const aFont : TFont; var fp : TFontProperties; dpi: integer = 96 );


function GetDefaultBrowserPath(const RemoveArgs : boolean = true): String;
Function GetAppFromExt( Const Ext : String; const RemoveArgs : boolean ) : String;
function GetFolderPath( const FolderName: String50 ): String;
function StripHTML( s : string; var InTag : boolean ) : string;
procedure BrowsersRunning( var IsNetscape, IsMSIE : boolean );
procedure AssociateApplication( const theEXT, theApp, IconSource : string; const IconIndex : integer );
function GetWindowsPath : string;

function LastPos( AChar : char; const AStr : string ) : integer;
function CtrlDown : Boolean;
function ShiftDown : Boolean;
function AltDown : Boolean;
function GetEnvVar( const csVarName : string ) : string;
function GetTimeZone( var offset, mode : longint ) : boolean;
Function TimeDeltaInMinutes( const StartDate, EndDate   : TDateTime): Double;
Function TimeDeltaInSeconds( const StartDate, EndDate : TDateTime): Double;
function DateTimeDiff(Start, Stop : TDateTime) : int64;
function GetTimeIntervalStr(Start, Stop : TDateTime): string;
function IncStrInterval (StartDate: TDateTime; const Interval: string; increment: boolean= true): TDateTime;
function TimeRevised(time: string): string;
function NormalFN( const fn : string ) : string;
function RelativeFN( FN : string ) : string;
function ProperFolderName( folder : string ) : string;
function BareFileName( const FN : string ) : string;
function SlashlessFolderName( const folder : string ) : string;
function ProperFileName( const FN, folder : string ) : string;
function AbsoluteFileName( const FN : string ) : string;
function BoolToStr( const b : boolean ) : string;
function CompareMem( I1, I2: PByte; Size: integer ): boolean;
function DialogFilter( const aName, aMask : string ) : string;
(*
function FormatForHTML( const s : string; const MultiLine : boolean ) : string;
*)


function LongToShortFileName( const FN : String ) : String;
function DateTimeToFileName( const DT : TDateTime ) : string;
function LocalHostName : string;
function MakePercentage( const Step, Max : Longint ) : Longint;

function WindowsErrorString : string;
function DecToRoman( Decimal: Longint): string;
function RomanToDec( const S : string ) : longint;

function RoundTo(n: Extended; decimals: integer): Extended;

function FormatDateTimeEnglish( AFormat : string; ADateTime : TDateTime ) : string;

function GenerateRandomPassphrase(
    const UseTemplate : boolean;
    const Template : string;
    const AllowSpace : boolean;
    const MinLength, MaxLength,
    RndPassUpAlphW,
    RndPassNumW,
    RndPassNonAlphW : integer
  ) : string;

  function DomainFromHttpURL(const URL: string; const Title: string='') : string;


type
  PFindWindowInfo = ^TFindWindowInfo;
  TFindWindowInfo = record
    PID: DWORD;
    WindowHandle: HWND;
  end;

function StartApp(apchOperation, apchFileName, apchParameters, apchDirectory: PChar; awrdShowCmd: Word): Cardinal;
procedure KillProcess(hProcess: Cardinal);
function LoadIconIntoBitmap (filePath: string): TBitmap;
function FindMainWindow(PID: DWORD): HWND;


var
  _OSIsWindowsNT : boolean;

implementation
uses
   SynGdiPlus,
   gf_strings;

const
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;


function GetCommandInterpreter : string;
begin
  result := GetEnvVar( 'COMSPEC' );
  if ( not fileexists( result )) then
  begin
    result := 'C:\WinNT\system32\cmd.exe';
    if ( not fileexists( result )) then
    begin
      result := 'C:\Windows\command.com';
      if ( not fileexists( result )) then
        result := '';
    end;
  end;
end; // GetCommandInterpreter

procedure SecondsToHMS( TotalSec : integer; var Hour, Min, Sec : word );
begin
  Hour := TotalSec div 3600;
  dec( TotalSec, Hour*3600 );
  Min := TotalSec div 60;
  dec( TotalSec, Min*60 );
  Sec := TotalSec;
end; // SecondsToHMS

function DoTrinaryCompare( const val1, val2 : longint ) : TTrinaryCompare;
begin
  if ( val1 > val2 ) then
    result := trinGreater
  else
  if ( val1 < val2 ) then
    result := trinSmaller
  else
    result := trinEqual;
end;



function ShiftStateToHotKey( AState : TShiftState ) : word;
begin
  result := 0;
  if ( ssAlt in AState ) then
    result := result or MOD_ALT;
  if ( ssCtrl in AState ) then
    result := result or MOD_CONTROL;
  if ( ssShift in AState ) then
    result := result or MOD_SHIFT;
end; // ShiftStateToHotKey

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
end; // StrToShiftState


function StringIsEmpty( const s : string ) : boolean;
var
  i : integer;
begin
  result := true;
  for i := 1 to length( s ) do
  begin
    // if ( not ( s[i] in [#9, #10, #13, #32] )) then
    if ( s[i] > #32 ) then
    begin
        result := false;
        break;
    end;
  end;
end; // StringIsEmpty


function FontStyleToStr( const F : SetOfTFontStyle ) : string;
begin
  result := '';
   if fsBold in F then
     result := sstrBold +#32;
   if fsItalic in F then
     result := result + sstrItalic +#32;
   if fsUnderline in F then
     result := result + sstrUnderline +#32;
   if fsStrikeOut in F then
     result := result + sstrStrikeOut;
   result := trim( result );
end; // FontStyleToStr

function StrToFontStyle( S : string ) : SetOfTFontStyle;
var
   F : SetOfTFontStyle;
begin
   F := [];
   if ( pos( sstrBold, S ) > 0 ) then
       include( F, fsBold );
   if ( pos( sstrItalic, S ) > 0 ) then
       include( F, fsItalic );
   if ( pos( sstrUnderline, S ) > 0 ) then
       include( F, fsUnderline );
   if ( pos( sstrStrikeOut, S ) > 0 ) then
       include( F, fsStrikeOut );
   result := F;
end; // StrToFontStyle


function GetDefaultBrowserPath(const RemoveArgs : boolean = true): String;
var
  S : string;
  p : integer;
begin
 // https://superuser.com/questions/1111122/path-variable-for-default-browser-in-windows

{
 Example:
 HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.html\UserChoice\ProgID
  =>"FirefoxHTML-308046B0AF4A39CB"

-> HKEY_CLASSES_ROOT\FirefoxHTML-308046B0AF4A39CB
-> HKEY_CLASSES_ROOT\FirefoxHTML-308046B0AF4A39CB\shell\open\command
  => "C:\Program Files\Mozilla Firefox\firefox.exe" -osint -url "%1"

}

  S := '';
  with TRegistry.Create(KEY_READ) do
  try
     RootKey := HKEY_CURRENT_USER;
     if OpenKey( '\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.html\UserChoice', False ) then begin
       S := ReadString( 'ProgID' );
       CloseKey;

       if S <> '' Then begin
         RootKey := HKEY_CLASSES_ROOT;
         if OpenKey( '\' + S + '\shell\open\command', False ) then
            S := ReadString( '' )
         else
            S := '';
       end;
    end;

  finally
    Free;
  end;

  if (S <> '') and RemoveArgs then begin
    p := pos( ' "%1"', s );
    if ( p > 0 ) then
       delete( s, p, length( s ) );
  end;

  if ( s <> '' ) then  begin
     if ( s[1] = '"' ) then begin
         delete( s, 1, 1 );
         delete( s, pos( '"', s ), length( s ));
     end;
  end;

  result := s;

end; // GetDefaultBrowserPath



function GetAppFromExt( const Ext : string; const RemoveArgs : boolean ) : string;
// adapted from Borland FAQ
var
  S : string;
  p : integer;
begin

  s := '';
  with TRegistry.Create (KEY_READ) do
  try
    RootKey := HKEY_CLASSES_ROOT;

    if OpenKey( '\' + Ext, False ) then
    begin
      S := ReadString( '' );
      if S <> '' Then begin
        if OpenKey( '\' + S + '\shell\open\command', False ) then
          S := ReadString( '' );
      end
      else
      begin If OpenKey( '\' + Ext + '\shell\open\command', False ) Then
          S := ReadString( '' );
      end;
    end;
  finally
    Free;
  end;

  if (S <> '') and RemoveArgs then begin
    p := pos( ' "%1"', s );
    if ( p > 0 ) then
       delete( s, p, length( s ) );
  end;

  if ( s <> '' ) then  begin
     if ( s[1] = '"' ) then begin
         delete( s, 1, 1 );
         delete( s, pos( '"', s ), length( s ));
     end;
  end;
  result := s;
end; // GetAppFromExt


function GetFolderPath( const FolderName : String50 ) : string;
begin
  result := '';
  with TRegistry.Create do
    try
      RootKey:= HKEY_CURRENT_USER;
      OpenKey( fpRootKey, False );
      Result := ReadString( FolderName );
      CloseKey;
    finally
      Free;
    end;
end; // GetFolderPath


function StripHTML( s : string; var InTag : boolean ) : string;
// Simplistic, but works where we need it.
// As a general solution, you probably can't do this
// reliably without parsing HTML first
var
  posbeg, posend : integer;
  temps : string;
begin
  temps := '';
  if ( length( s ) < 3 ) then
  begin
    result := s;
    exit;
  end;

  if InTag then
  begin
    posend := pos( '>', s );
    if ( posend = 0 ) then
    begin
        result := s;
        exit;
    end;
    delete( s, 1, posend );
    InTag := false;
  end;

  posbeg := pos( '<', s );
  while ( posbeg > 0 ) do
  begin
    temps := temps + copy( s, 1, posbeg-1 );
    delete( s, 1, posbeg );
    InTag := true;
    posend := pos( '>', s );
    if ( posend = 0 ) then
    begin
      break;
    end
    else
    begin
      delete( s, 1, posend );
      InTag := false;
    end;
    posbeg := pos( '<', s );
  end;

  if InTag then
    result := temps
  else
    result := temps + s;

end; // StripHTML

procedure BrowsersRunning( var IsNetscape, IsMSIE : boolean );
begin
  IsNetscape := ( FindWindow( PChar( NetscapeClass ), nil ) > 0 );
  IsMSIE := ( FindWindow( PChar( MSIEClass1 ), nil ) > 0 )
             or
            ( FindWindow( PChar( MSIEClass2 ), nil ) > 0 );
end; // BrowsersRunning

procedure AssociateApplication(
                const theEXT, theApp,
                IconSource : string;
                const IconIndex : integer );
{ adapted from Borland Delphi FAQ }
var
  reg : TRegistry;
  cmd, icn : string;
begin
  cmd := theApp;
  if ( pos( #32, cmd ) > 0 ) then
    cmd := LongToShortFileName( cmd );
  cmd := lowercase( cmd );

  if ( IconSource <> '' ) then
    icn := IconSource
  else
    icn := theApp;

  if ( pos( #32, icn ) > 0 ) then
    icn := LongToShortFileName( icn );
  icn := lowercase( icn );

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;
    reg.OpenKey( theEXT + '\shell\open\command', true );
    reg.WriteString('', cmd + ' "%1"');
    reg.CloseKey;
    reg.OpenKey( theEXT + '\DefaultIcon', true );
    reg.WriteString('',
        icn + ',' + inttostr( IconIndex ));
    reg.CloseKey;
  finally
    reg.free;
  end;
end; // AssociateApplication

function GetWindowsPath : string;
var
  folder : array[0..MAX_PATH] of char;
  i : integer;
begin
  folder := '';
  i := GetWindowsDirectory( folder, MAX_PATH );
  if ( i <> 0 ) then
    result := folder
  else
    result := '';
end; // GetWindowsPath

function FontPropertiesToStr( const font : TFontProperties ) : string;
begin
  result := font.fname + #32 + inttostr( font.fsize ) + ' pt ' + FontStyleToStr( font.fstyle );
end; // FontPropertiesToStr

function BareFileName( const FN : string ) : string;
var
  p : integer;
begin
  result := ExtractFilename( FN );
  p := lastpos( '.', result );
  if ( p > 0 ) then
    delete( result, p, length( result ));
end; // BareFileName

function ProperFolderName( folder : string ) : string;
begin
  folder := AnsiLowerCase( trim( folder ));
  if ( folder <> '' ) then
  begin
    if ( folder[length( folder )] <> '\' ) then
       folder := folder + '\';
  end;
  result := folder;
end; // ProperFolderName

function SlashlessFolderName( const folder : string ) : string;
var
  l : integer;
begin
  result := folder;
  l := length( result );
  if ( l > 3 ) then
  begin
    if ( result[l] = '\' ) then
      delete( result, l, 1 );
  end;
end; // SlashlessFolderName

function ProperFileName( const FN, folder : string ) : string;
begin
  if ( fn = '' ) then
  begin
    result := fn;
    exit;
  end;

  if ( extractfilepath( fn ) = '' ) then
  begin
    if ( folder <> '' ) then
      result := properfoldername( folder ) + fn
    else
      result := properfoldername( ParamStr( 0 )) + fn;
  end;
end; // ProperFileName


function LastPos( AChar : char; const AStr : string ) : integer;
var
  i : integer;
begin
  if ( AStr = '' ) then
  begin
    result := 0;
    exit;
  end;
  i := length( AStr );
  while ( i > 0 ) and ( AStr[i] <> Achar ) do
    dec( i );
  result := i;
end; // LastPos

procedure SetDefaultFont( var theFont : TFontProperties );
begin
  with theFont do
  begin
    fcharset := DEFAULT_CHARSET; // OEM_CHARSET;
    fcolor   := clWindowText;
    fname    := 'Tahoma';
    fpitch   := fpDefault;
    fsize    := 10;
    fstyle   := [];
    fBGColor := clWindow;
  end;
end; // SetDefaultFont

function CtrlDown : Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Control] And 128) <> 0);
end;

function ShiftDown : Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Shift] and 128) <> 0);
end;

function AltDown : Boolean;
var
  State : TKeyboardState;
begin
  GetKeyboardState(State);
  Result := ((State[vk_Menu] and 128) <> 0);
end;

function GetEnvVar( const csVarName : string ) : string;
var
  pc2  : PChar;
  res : integer;
begin
  pc2 :=  StrAlloc( cnMaxEnvVarValueSize + 1 );
  try
    res := GetEnvironmentVariable( pchar( csVarName ), pc2, cnMaxEnvVarValueSize );
    if ( res > 0 ) then
     Result := StrPas( pc2 )
    else
    result := '';
  finally
    StrDispose( pc2 );
  end;
end; // GetEnvVar

function GetTimeZone( var offset, mode : longint ) : boolean;
var
  aTimeZoneInfo : TTimeZoneInformation;
begin
  result := false;
  mode := GetTimeZoneInformation(aTimeZoneInfo);
  if ( mode in [TIME_ZONE_ID_UNKNOWN,TIME_ZONE_ID_STANDARD] ) then
  begin
     offset := - ( aTimeZoneInfo.Bias DIV 60 );
     result := true;
  end
  else
  if ( mode = TIME_ZONE_ID_DAYLIGHT ) then
  begin
     offset := - (( aTimeZoneInfo.Bias + aTimeZoneInfo.DaylightBias ) DIV 60 );
     result := true;
  end;
end; // TimeZoneOffset


Function TimeDeltaInMinutes( const StartDate, EndDate : TDateTime): Double;
Var
   Hour, Min, Sec, MSec  : Word;
   Delta : TDateTime;
Begin
   Try
       Delta := EndDate - StartDate;
       DecodeTime(Delta, Hour, Min, Sec, MSec);
       Result := (Hour*60)+Min;
   Except
       Result := 0;
   End;
End; // TimeDeltaInMinutes

Function TimeDeltaInSeconds( const StartDate, EndDate : TDateTime): Double;
Var
  Hour, Min, Sec, MSec  : Word;
  Delta : TDateTime;
Begin
  Try
    Delta := EndDate - StartDate;
    DecodeTime(Delta, Hour, Min, Sec, MSec);
    Result := (((Hour*60)+Min)*60)+Sec;
  Except
    Result := 0;
  End;
End; // TimeDeltaInSeconds


function DateTimeDiff(Start, Stop : TDateTime) : int64;
var TimeStamp : TTimeStamp;
begin
  if Stop >= Start then
     TimeStamp := DateTimeToTimeStamp(Stop - Start)
  else
     TimeStamp := DateTimeToTimeStamp(Start - Stop);
  Dec(TimeStamp.Date, TTimeStamp(DateTimeToTimeStamp(0)).Date);
  Result := (TimeStamp.Date*24*60*60)+(TimeStamp.Time div 1000);
end;


function RoundTo(n: Extended; decimals: integer): Extended;
var
   coef: int64;
begin
  coef:= Round(Exp(decimals*ln(10)));  //  a^b
  if n > 0 then
     Result:= Trunc(n*coef + 0.5 + 0.00000001) / coef
  else
     Result:= Trunc(n*coef - 0.5 - 0.00000001) / coef;
end;


function NormalFN( const fn : string ) : string;
begin
  result := FN;
  if ( result <> '' ) then
  begin
    if ( result[1] = '"' ) then
    begin
      delete( result, 1, 1 );
      delete( result, pos( '"', result ), length( result ));
    end;
    result := trim( result );
    (*
    if NormalizePath then
    begin
      if (( result <> '' ) and ( pos( '\', result ) = 0 )) then
        result := extractfilepath( ParamStr( 0 )) + result;
    end;
    *)
  end;
  //result := ansilowercase( result );
end; // NormalFN


function RelativeFN( FN : string ) : string;
begin
  // given a full path and filename, returns only the
  // filename part IF the path is the same as the application's
  // own directory (ie the file lives where the program does)
  FN := NormalFN( FN );
  if ( ExtractFilepath( FN ) = AnsiLowercase( extractfilepath( ParamStr( 0 )))) then
    result := ExtractFilename( FN )
  else
    result := FN;
end; // relativeFN


function AbsoluteFileName( const FN : string ) : string;
begin
  if ( FN = '' ) then
  begin
    result := '';
    exit;
  end;
  if ( extractfilepath( FN ) = '' ) then
    result := normalFN( extractfilepath( paramstr( 0 )) + FN )
  else
    result := normalFN( FN );
end; // AbsoluteFileName


function BoolToStr( const b : boolean ) : string;
begin
  if b then
    result := 'Yes'
  else
    result := 'No';
end; // BoolToStr;


function CompareMem(I1, I2: PByte; Size: integer): boolean;
// compares 2 memory buffers
// Written by David Barton (davebarton@bigfoot.com)
begin
  Result:= true;
  repeat
    if I1^<> I2^ then
    begin
      Result:= false;
      Exit;
    end;
    Inc(I1);
    Inc(I2);
    Dec(Size);
  until Size= 0;
end; // CompareMem


function DialogFilter( const aName, aMask : string ) : string;
begin
  result := aName + ' (' + aMask + ')|' + aMask;
end; // Dialogfilter;

(*
function FormatForHTML( const s : string; const MultiLine : boolean ) : string;
// "s" must NOT contain any HTML tags!
var
  i : integer;
  ch : char;
begin
  result := '';

  for i := 1 to length( s ) do
  begin
    ch := s[i];
    case ch of
      '&' : begin
        result := result + '&amp;';
      end;
      '<' : begin
        result := result + '&lt;';
      end;
      '>' : begin
        result := result + '&gt;';
      end;
      '"' : begin
        result := result + '&#34;';
      end;
      #13 : begin
        if MultiLine then
          result := result + '<BR>' + #13#10
        else
          result := result + #13#10;
      end;
      #0, #10 : begin
        // ignore
      end;
      else
      begin
        result := result + ch;
      end;
    end;
  end;
end; // FormatForHTML
*)

procedure FontPropertiesToFont( const fp : TFontProperties; aFont : TFont; dpi: integer = 96); // 96: 100% scaling
begin
  with aFont do
  begin
    name := fp.fname;
    pitch := fp.fpitch;
    size := fp.fsize;
    if dpi = 96 then
       Size := fp.fsize
    else
       Height := -MulDiv(fp.fsize, dpi, 72);   // See comments to TaskModalDialog in gf_miscvcl

    color := fp.fcolor;
    charset := fp.fcharset;
    style := fp.FStyle;
  end;
end; // FontPropertiesToFont


procedure FontToFontProperties( const aFont : TFont; var fp : TFontProperties; dpi: integer = 96);
begin
  with aFont do begin
    fp.fname := name;
    fp.fpitch := pitch;
    if dpi = 96 then
       fp.fsize:= Size
    else
       fp.fsize := -MulDiv(Height, 72, dpi);   // See comments to TaskModalDialog in gf_miscvcl

    fp.fcolor := color;
    fp.fcharset := charset;
    fp.fstyle := style;
  end;
end; // FontToFontProperties


function LongToShortFileName( const FN : string ) : string;
// returns short (8.3) filename given a long name
// will return garbage if file does not exist, so
// check before. Ideally, it should also accept
// bare filenames and search the PATH first, but
// that would slow us down somewhat/
var
  Buffer : array [0..255] of char;
begin
  GetShortPathName(
    PChar( FN ),
    @Buffer,
    sizeof(Buffer));
  result := Buffer;
end; // LongToShortFileName


function DateTimeToFileName( const DT : TDateTime ) : string;
var
  i : integer;
begin
  result := DateTimeToStr( DT );
  for i := 1 to length( result ) do
  begin
    if ( result[i] = #32 ) then
      result[i] := '_'
    else
      if ( result[i] = ':' ) then
        result[i] := '-';
  end;
end; // DateTimeToFileName


function GetExtAlphChar( aChar : char ) : char;
begin
  case aChar of
    Char('¹'), '¥' : result := 'a';
    Char('æ'), 'Æ' : result := 'c';
    Char('ê'), 'Ê' : result := 'e';
    Char('³'), '£' : result := 'l';
    Char('ñ'), 'Ñ' : result := 'n';
    Char('ó'), 'Ó' : result := 'o';
    Char('œ'), 'Œ' : result := 's';
    Char('Ÿ'), '' : result := 'z';
    Char('¿'), '¯' : result := 'z';
    else
      result := #0;
  end;
end; // GetExtAlphChar

function IsWord( const w : string ) : boolean;
var
  i : integer;
begin
  result := true;
  if ( w = '' ) then
  begin
    result := false;
    exit;
  end;
  for i := 1 to length( w ) do
    if not IsCharAlpha( w[i] ) then
    begin
      result := false;
      break;
    end;
end; // IsWord


function LocalHostName : string;
var
  s : array[0..128] of char;
  size : DWORD;
begin
  size := 128;
  if GetComputerName( @s, size ) then
    result := s
  else
    result := 'localhost';

end; // LocalHostName


function MakePercentage( const Step, Max : Longint ) : Longint;
begin
  Result := Round((Step * 100.0) / Max);
end;



function UnixTimeToDateTime(UnixTime: Integer): TDateTime;
begin
  Result := EncodeDate(1970, 1, 1) + (UnixTime div 86400);
  Result := Result + ((UnixTime mod 86400) / 86400);
end; // UnixTimeToDateTime


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We cannot use Delphi own function because the date must be specified in   }
{ english and Delphi use the current language.                              }
{ THIS ROUTINE COPIED FROM "httpprot.pas", part of ICS component suite      }
{ by F. Piette                                                              }
function RFC1123_Date(aDate : TDateTime) : String;
const
   StrWeekDay : String = 'MonTueWedThuFriSatSun';
   StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
   Year, Month, Day       : Word;
   Hour, Min,   Sec, MSec : Word;
   DayOfWeek              : Word;
begin
   DecodeDate(aDate, Year, Month, Day);
   DecodeTime(aDate, Hour, Min,   Sec, MSec);
   DayOfWeek := ((Trunc(aDate) - 2) mod 7);
   Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
             Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
                     Year, Hour, Min, Sec]);
end; // RFC1123_Date

function TColorToHTMLColor( const cl: TColor): string;
var
  rgbColor: TColorRef;
begin
  rgbColor := ColorToRGB(cl);
  Result := '#' + Format('%.2x%.2x%.2x',
    [GetRValue(rgbColor),
    GetGValue(rgbColor),
    GetBValue(rgbColor)]);
end;

function IsIE4Installed : Boolean;
var
  Reg : TRegistry;
  S   : string;
begin
  Reg := TRegistry.Create;
  with Reg do
  begin
    RootKey := HKEY_LOCAL_MACHINE;
    try
      OpenKey( 'Software\Microsoft\Internet Explorer', False );
      if ValueExists( 'Version' ) then
        S := ReadString( 'Version' )
      else
        S := '0';
      CloseKey;
    finally
      Free;
    end;
  end;
  try
    Result := ( StrToInt( S[1] ) > 3 );
  except
    Result := false;
  end;
end; // IsIE4Installed


function TranslateShellExecuteError( const ErrCode : integer ) : string;
begin
  case ErrCode of
    0	: result := STR_ERR_OUTOFRESOURCES;
    ERROR_FILE_NOT_FOUND : result := STR_ERROR_FILE_NOT_FOUND;
    ERROR_PATH_NOT_FOUND : result := STR_ERROR_PATH_NOT_FOUND;
    ERROR_BAD_FORMAT : result := STR_ERROR_BAD_FORMAT;
    SE_ERR_ACCESSDENIED : result := STR_SE_ERR_ACCESSDENIED;
    SE_ERR_ASSOCINCOMPLETE : result := STR_SE_ERR_ASSOCINCOMPLETE;
    SE_ERR_DDEBUSY : result := STR_SE_ERR_DDEBUSY;
    SE_ERR_DDEFAIL : result := STR_SE_ERR_DDEFAIL;
    SE_ERR_DDETIMEOUT : result := STR_SE_ERR_DDETIMEOUT;
    SE_ERR_DLLNOTFOUND : result := STR_SE_ERR_DLLNOTFOUND;
    // SE_ERR_FNF : result := 'The specified file was not found.';
    SE_ERR_NOASSOC : result := STR_SE_ERR_NOASSOC;
    SE_ERR_OOM : result := STR_SE_ERR_OOM;
    // SE_ERR_PNF : result := 'The specified path was not found.';
    SE_ERR_SHARE : result := STR_SE_ERR_SHARE;
    else
      result := STR_UNKNOWN_ERROR;
  end;
end; // TranslateShellExecuteError


function GenerateRandomPassphrase(
    const UseTemplate : boolean;
    const Template : string;
    const AllowSpace : boolean;
    const MinLength, MaxLength,
    RndPassUpAlphW,
    RndPassNumW,
    RndPassNonAlphW : integer
  ) : string;
const
  _LoCaseAlph = 'abcdefghijklmnopqrstuvwxyz';
  _UpCaseAlph = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  _Consonants = 'bcdfghjklmnpqrstvwxz';
  _Vowels = 'aeiouy';
  _Nums = '0123456789';
  _NonAlph = ',./<>?!@#$%^&*()-_=+[]{};:';
  _NonAlphSpace = #32 + _NonAlph;
var
  p, r, i, passlength : integer;
  ch, lastch : char;

begin
  result := '';
  lastch := #0;

  if ( UseTemplate and ( Template <> '' )) then
  begin
    passlength := length( Template );
    setlength( result, passlength );
    for i := 1 to passlength do
    begin
      ch := Template[i];
      case ch of
        'c' : begin
          p := random( length( _Consonants )) + 1;
          result[i] := _Consonants[p];
        end;
        'C' : begin
          p := random( length( _Consonants )) + 1;
          result[i] := UpCase( _Consonants[p] );
        end;
        'v' : begin
          p := random( length( _Vowels )) + 1;
          result[i] := _Vowels[p];
        end;
        'V' : begin
          p := random( length( _Vowels )) + 1;
          result[i] := Upcase( _Vowels[p] );
        end;
        '#' : begin
          p := random( length( _Nums )) + 1;
          result[i] := _Nums[p];
        end;
        '.' : begin
          p := random( length( _NonAlph )) + 1;
          result[i] := _NonAlph[p];
        end;
        else
          result[i] := ch;
      end;
    end;
    exit;
  end;

  if (( MaxLength < MinLength ) or ( MaxLength < 1 )) then exit;
  passlength := random( MaxLength-MinLength ) + MinLength;
  if ( passlength < 1 ) then passlength := 1;

  for i := 1 to passlength do
  begin

    repeat

      r := random( 100 );
      if ( r < RndPassUpAlphW ) then
      begin
        p := random( length( _UpCaseAlph )) + 1;
        ch := _UpCaseAlph[p];
      end
      else
      if ( r >= RndPassUpAlphW ) and ( r < RndPassUpAlphW+RndPassNumW ) then
      begin
        p := random( length( _Nums )) + 1;
        ch := _Nums[p];
      end
      else
      if ( r >= RndPassUpAlphW+RndPassNumW ) and ( r < RndPassUpAlphW+RndPassNumW+RndPassNonAlphW ) then
      begin
        if AllowSpace then
        begin
          p := random( length( _NonAlphSpace )) + 1;
          ch := _NonAlphSpace[p];
        end
        else
        begin
          p := random( length( _NonAlph )) + 1;
          ch := _NonAlph[p];
        end;
      end
      else
      begin
        p := random( length( _LoCaseAlph )) + 1;
        ch := _LoCaseAlph[p];
      end;

    until ( ch <> lastch );

    result := result + ch;
    lastch := ch;
  end;
end; // GenerateRandomPassphrase


function WindowsErrorString : string;
var
  err : DWORD;
begin
  err := GetlastError;
  if ( err <> 0 ) then
    result := SysErrorMessage( err )
  else
    result := '';
end; // WindowsErrorString

function RunsOnWindowsNT : boolean;
var
  OSInfo : TOSVersionInfo;
begin
  result := false;

  OSInfo.dwOSVersionInfoSize := sizeof( OSInfo );
  if GetVersionEx( OSInfo ) then
  begin
    result := ( OSInfo.dwPlatformId = VER_PLATFORM_WIN32_NT );
  end;

end; // RunsOnWindowsNT


function DecToRoman( Decimal: Longint): string;
// Author: Thomas Stutz <tom@swissdelphicenter.ch>
// Decimal must be 0..3999
const
  Numbers: array[1..13] of Integer =
    (1, 4, 5, 9, 10, 40, 50, 90, 100,
    400, 500, 900, 1000);
  Romans: array[1..13] of string =
    ('I', 'IV', 'V', 'IX', 'X', 'XL',
    'L', 'XC', 'C', 'CD', 'D', 'CM', 'M');
var
  i: Integer;
begin
  Result := '';
  for i := 13 downto 1 do
    while (Decimal >= Numbers[i]) do
    begin
      Decimal := Decimal - Numbers[i];
      Result  := Result + Romans[i];
    end;
end;


function RomanToDec( const S : string ) : longint;
const Chars = 'IVXLCDMvxlcdm?!' ;
Valu : array [0..Length(Chars)] of longint =
  (0, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000, 100000,
  500000, 1000000, 5000000, 10000000) ;
var Sum, NewV, OldV : longint ; K : byte ;
begin
  result := -1;
  Sum := 0 ; OldV := 0 ;
  for K := Length(S) downto 1 do
  begin
    NewV := Valu[Pos(S[K], Chars)] ;
    if NewV=0 then
    begin
      Sum := -1 ;
      BREAK
    end;
    if NewV<OldV then NewV := - NewV ;
    Inc(Sum, NewV) ; OldV := NewV end ;
  result := Sum ;
end; // RomanToDec


function FormatDateTimeEnglish( AFormat : string; ADateTime : TDateTime ) : string;
type
  TWeekDays = array[1..7] of string;
  TMonths = array[1..12] of string;
const
  WeekDaysShort : TWeekDays = (
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'
  );
  WeekDaysLong : TWeekDays = (
    'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'
  );
  MonthsShort : TMonths = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
  );
  MonthsLong : TMonths = (
    'January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'
  );
var
  oldShortDayNames, oldLongDaynames : TWeekDays;
  oldShortMonthNames, oldLongMonthNames : TMonths;
  i : integer;
begin
  for i := 1 to 7 do
  begin
    oldShortDayNames[i] := FormatSettings.ShortDayNames[i];
    oldLongDaynames[i] := FormatSettings.LongDaynames[i];
  end;
  for i := 1 to 12 do
  begin
    oldShortMonthNames[i] := FormatSettings.ShortMonthNames[i];
    oldLongMonthNames[i] := FormatSettings.LongMonthNames[i];
  end;

  try
    for i := 1 to 7 do
    begin
      FormatSettings.ShortDayNames[i] := WeekDaysShort[i];
      FormatSettings.LongDaynames[i] := WeekDaysLong[i];
    end;
    for i := 1 to 12 do
    begin
      FormatSettings.ShortMonthNames[i] := MonthsShort[i];
      FormatSettings.LongMonthNames[i] := MonthsLong[i];
    end;

    result := FormatDateTime( AFormat, ADateTime );

  finally
    for i := 1 to 7 do
    begin
      FormatSettings.ShortDayNames[i] := oldShortDayNames[i];
      FormatSettings.LongDaynames[i] := oldLongDaynames[i];
    end;
    for i := 1 to 12 do
    begin
      FormatSettings.ShortMonthNames[i] := oldShortMonthNames[i];
      FormatSettings.LongMonthNames[i] := oldLongMonthNames[i];
    end;
  end;

end; // FormatDateTimeEnglish


// Returns a string with the time interval between Reference and Alarm (on the form: "2 minutes", "4 hours", etc)
//
function GetTimeIntervalStr(Start, Stop : TDateTime): string;
var
  secondsDiff: int64;
  n: Extended;
  units: string;
begin
    secondsDiff:= DateTimeDiff(Start, Stop);

    if (secondsDiff = 604800) or (secondsDiff >= 2*604800) then begin      // 1 week: 7d *24h*60m*60s =  604.800 seconds
       n:= secondsDiff / 604800;
       units:= STR_weeks;
       if n = 1 then units:= STR_week;
       end
    else if (secondsDiff = 86400) or (secondsDiff >= 2*86400) then begin  // 1 day:      24h*60m*60s =   86.400 seconds
       n:= secondsDiff / 86400;
       units:= STR_days;
       if n = 1 then units:= STR_day;
       end
    else if (secondsDiff = 3600) or (secondsDiff >= 2*3600)  then begin  // 1 hour:         60m*60s =    3.600 seconds
       n:= secondsDiff / 3600;
       units:= STR_hours;
       if n = 1 then units:= STR_hour;
       end
    else begin
       n:= secondsDiff / 60;
       units:= STR_minutes;
       if n = 1 then units:= STR_minute;
       end;


    Result:= Format('%g %s', [RoundTo(n,1), units]);
end;  // GetTimeIntervalStr

(*
Increments a TDateTime variable by a time interval in a string of the form: "5 minutes", "22 hours", ...
Intervals like "2m", "4hours" "4 ho", ... are also understood
StartDate will be incremented or decremented by interval depending on the parameter "increment"
The numbers can contain decimals
*)
function IncStrInterval (StartDate: TDateTime; const Interval: string; increment: boolean= true): TDateTime;
var
   i, lenS: integer;
   s: string;
   n: Extended;
   afterBefore: integer;
   separatorToReplace: string;
   secondsToInc: integer;

begin
   afterBefore:= 1;
   if not increment then afterBefore := -1;

   Result:= StartDate;
   s:= Trim(Interval);
   if s <> '0' then begin
       i:= 1;
       while (i <= length(s)) and (StrScan(partsOfNumbers, s[i]) <> nil)
       do Inc(i);

       try
           if FormatSettings.DecimalSeparator = ',' then separatorToReplace:= '.' else separatorToReplace:= ',';

           n:= StrToFloat(ReplaceStr(Copy(s, 1, i-1), separatorToReplace, FormatSettings.DecimalSeparator));

           s:= AnsiLowerCase(Trim(copy(s,i,length(s))));
           lenS:= length(s);
           if copy(STR_minutes,1,lenS) = s then
              secondsToInc:= Round(60*n)

           else if copy(STR_hours,1,lenS) = s then
              secondsToInc:= Round(60*60*n)

           else if copy(STR_days,1,lenS) = s then
              secondsToInc:= Round(24*60*60*n)

           else if copy(STR_weeks,1,lenS) = s then
              secondsToInc:= Round(7*24*60*60*n)

           else
              Result:= 0;

           if Result <> 0 then
              Result:= incSecond(Result, Round(secondsToInc*afterBefore));

       except
          Result:= 0;
       end;

   end;
end;     // IncStrInterval


(*
 Returns an interpreted, clean, version from the intput time, if it's possible
 If it isn't the returns the same value
 For example:
    "1521" --> "15:21"
    "941" --> 09:41"
    "15-21" --> "15:21"
    and so on
*)
function TimeRevised(time: string): string;
var
   s: string;
   i: integer;
begin
   try
        s:= '';
        i:= 1;
        while (i <= length(time)) do begin
           if StrScan(digitsNumbers, time[i]) <> nil then
              s:= s + time[i];
           Inc(i);
        end;

        if (length(s) <= 4) and (length(s) > 2) then
           s:= Copy(s, 1, length(s)-2) + ':' + Copy(s, length(s)-1, 2);

        Result:= FormatDateTime('hh:nn', StrToTime(s));
   except
        Result:= time;
   end;
end;

{ Returns the server associated with the URL as long as it is not already included in the title }
function DomainFromHttpURL(const URL: string; const Title: string='') : string;
var
  p, p2: integer;
  Server_Subdom: string;
  titleAux: string;
  ReturnDomain: boolean;

begin
   Result:= '';
   ReturnDomain:= false;

   if (pos('http', URL) = 0) or (Title='') then
      exit;

   try
       p:= pos('//', URL);
       if p <= 7 then begin                 // http: -> 6 https: -> 7
          p:= p + 2;
          if pos('www.', URL, p) > 0 then
             p:= p + 4;
          p2:= pos('/', URL, p);
          Result:= Copy(URL, p, p2-p);

          // If the last part of the domain, the main part, appears in the title, we will not show it
          // https://en.wikipedia.org  --> "wikipedia"
          p2 := NFromLastCharPos(Result, '.', 1);
          p  := NFromLastCharPos(Result, '.', 2);
          Server_Subdom:= Copy(Result, p+1, p2-p-1);
          if Server_Subdom <> '' then begin
             Server_Subdom:= AnsiUpperCase(Server_Subdom);
             titleAux:= AnsiUpperCase( StringReplace(title, ' ', '', [rfReplaceAll]) );

             if pos(Server_Subdom, TitleAux) = 0 then begin
                p2 := NFromLastCharPos(Result, '.', 3);
                Server_Subdom:= Copy(Result, p2+1, p-p2-1);
                if Server_Subdom <> '' then begin
                   Server_Subdom:= AnsiUpperCase(Server_Subdom);
                   if pos(Server_Subdom, TitleAux) = 0 then
                       returnDomain:= true;
                end
                else
                    returnDomain:= true;
             end;
          end;

       end;

       if returnDomain then
          if Result = 'youtube.com' then
             Result:= ' [YouTube]'
          else
             Result:= ' [' + Result + ']'
       else
          Result:= '';

   except
   end;

end;

procedure Test_DomainFromHttpURL;
var
  s: string;
begin
  s:= DomainFromHttpURL('https://www.teoria.com/en/tutorials/functions/intro/03-dom-sub.php','Harmonic Functions : The Dominant and Subdominant');  //-> [teoria.com]
  s:= DomainFromHttpURL('https://www.teoria.com/en/tutorials/functions/intro/03-dom-sub.php','');  //-> ''
  s:= DomainFromHttpURL('https://musicnetmaterials.wordpress.com/analisis-musical/','Análisis musical | Musicnetmaterials'); // -> ''
  s:= DomainFromHttpURL('https://stackoverflow.com/questions/1549145/case-insensitive-pos','delphi - case insensitive Pos - Stack Overflow'); // -> ''
  s:= DomainFromHttpURL('https://es.stackoverflow.com/questions/596857/problema-al-imprimir-objeto-de-javascript-en-consola-de-navegador','Problema al imprimir objeto de Javascript en consola de navegador - Stack Overflow en español'); // -> ''
  s:= DomainFromHttpURL('https://martinfowler.com/eaaCatalog/','Catalog of Patterns of Enterprise Application Architecture');  // -> [martinfowler.com]
  s:= DomainFromHttpURL('https://synopse.info/files/html/Synopse%20mORMot%20Framework%20SAD%201.18.html','Software Architecture Design 1.18');  // -> [synopse.info]
  s:= DomainFromHttpURL('https://www.delphitools.info/2013/05/13/immutable-strings-in-delphi/','Immutable strings… in Delphi? - DelphiTools'); // -> ''
  s:= DomainFromHttpURL('https://en.wikipedia.org/wiki/Johann_Sebastian_Bach','Johann Sebastian Bach - Wikipedia'); // -> ''
  s:= DomainFromHttpURL('http://www.rvelthuis.de/articles/articles-pchars.html','Rudy''s Delphi Corner - PChars: no strings attached'); //-> [rvelthuis.de]
  s:= DomainFromHttpURL('https://learn.microsoft.com/es-es/','Microsoft Learn: adquirir conocimientos que le abran las puertas en su carrera profesional'); //  -> ''
  s:= DomainFromHttpURL('https://www.youtube.com/watch?v=r0R6gMw2s44','El Círculo de Quintas: Una explicación detallada | Versión 2.0'); //  -> [YouTube]
end;


function StartApp(apchOperation, apchFileName, apchParameters, apchDirectory: PChar; awrdShowCmd: Word): Cardinal;
var
  lseiInfo: TShellExecuteInfo;
begin
  Result := 0;
  FillChar(lseiInfo, SizeOf(lseiInfo), Chr(0));
  lseiInfo.cbSize := SizeOf(lseiInfo);
  lseiInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  lseiInfo.lpVerb := apchOperation;
  lseiInfo.lpFile := apchFileName;
  lseiInfo.lpParameters := apchParameters;
  lseiInfo.lpDirectory := apchDirectory;
  lseiInfo.nShow := awrdShowCmd;
  if Boolean(ShellExecuteEx(@lseiInfo)) then
    Result := lseiInfo.hProcess;
end;

procedure KillProcess(hProcess: Cardinal);
Var
  ovExitCode: LongWord;
begin
  try
    if hProcess <> 0 then begin
      GetExitCodeProcess(hProcess, ovExitCode);
      if (ovExitCode = STILL_ACTIVE) or (ovExitCode <> WAIT_OBJECT_0) then
        TerminateProcess(hProcess, ovExitCode);
      CloseHandle(hProcess);
    end;
  except
  end;
end;

function EnumWindowsProc(Wnd: HWND; Info: PFindWindowInfo): BOOL; stdcall;
var
  WindowPID: DWORD;
begin
  GetWindowThreadProcessId(Wnd, @WindowPID);
  if WindowPID = Info^.PID then begin
    Info^.WindowHandle := Wnd;
    Result := False;  // Stop the enumeration
  end
  else
    Result := True;
end;

function FindMainWindow(PID: DWORD): HWND;
var
  Info: TFindWindowInfo;
begin
  Info.PID := PID;
  Info.WindowHandle := 0;
  EnumWindows(@EnumWindowsProc, LPARAM(@Info));
  Result := Info.WindowHandle;
end;


function LoadIconIntoBitmap (filePath: string): TBitmap;
var
  LargeIcon, SmallIcon: HICON;
  Icon: TIcon;
  Pict: TSynPicture;
begin
  Result:= nil;
  Icon := TIcon.Create;
  Pict:= TSynPicture.Create;
  try
    ExtractIconEx(PChar(filePath), 0, LargeIcon, SmallIcon, 1);
    if LargeIcon <> 0 then begin
      Icon.Handle := LargeIcon;
      Pict.Assign(Icon);
      Result:= Pict.ToBitmap;
    end;
  finally
    Icon.Free;
    Pict.Free;
  end;
end;



Initialization
  _OSIsWindowsNT := RunsOnWindowsNT;

  // Test_DomainFromHttpURL;

end.





