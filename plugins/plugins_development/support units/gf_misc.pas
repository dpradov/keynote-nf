unit gf_misc;


(* ************************************************************
 MOZILLA PUBLIC LICENSE STATEMENT
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is "gf_URLParser.pas".

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 To do:
 -----------------------------------------------------------
 Released: 20 August 2001
 -----------------------------------------------------------
 URLs:

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

interface
uses Forms, Classes, SysUtils,
     FileCtrl, Dialogs, Graphics,
     Registry, Windows, Clipbrd,
     ShellAPI, Messages, StdCtrls;


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

function ClipboardAsString : string;
function FirstLineFromClipboard( const MaxLen : integer ) : string;

function StringIsEmpty( const s : string ) : boolean;

function IsWord( const w : string ) : boolean;
function GetExtAlphChar( aChar : char ) : char;
Function FontStyleToStr( const F : SetOfTFontStyle {TFontProperties} ) : string;
Function StrToFontStyle( S : string ) : SetOfTFontStyle;
function FontPropertiesToStr( const font : TFontProperties ) : string;
procedure SetDefaultFont( var theFont : TFontProperties );
function VerdanaInstalled : boolean;
function TahomaInstalled : boolean;
procedure FontPropertiesToFont( const fp : TFontProperties; aFont : TFont );
procedure FontToFontProperties( const aFont : TFont; var fp : TFontProperties );

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

function PopUpMessage( const mStr : string; const mType : TMsgDlgType; const mButtons : TMsgDlgButtons; const mHelpCtx : integer ) : word;
function LongToShortFileName( const FN : string ) : string;
function DateTimeToFileName( const DT : TDateTime ) : string;
procedure SleepWell( const TenthsOfSecond : cardinal );
function LocalHostName : string;
function MakePercentage( const Step, Max : Longint ) : Longint;

Function DefMessageDlg(const aCaption: String;
                       const Msg: string;
                       DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons;
                       DefButton: Integer;
                       HelpCtx: Longint): Integer;


Procedure PostKeyEx32( key: Word; Const shift: TShiftState;
            specialkey: Boolean );

Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift:
            TShiftState; specialkey: Boolean );


function GenerateRandomPassphrase(
    const UseTemplate : boolean;
    const Template : string;
    const AllowSpace : boolean;
    const MinLength, MaxLength,
    RndPassUpAlphW,
    RndPassNumW,
    RndPassNonAlphW : integer
  ) : string;

var
  _TahomaFontInstalled : boolean = false;

implementation

const
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;


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

procedure SleepWell( const TenthsOfSecond : cardinal );
var
  TargetTicks : cardinal;
begin
  TargetTicks := GetTickCount+(100*TenthsOfSecond);
  while (TargetTicks-GetTickCount) >  0 do begin
    sleep(100); // Check for updates 10 times a second.
    Application.ProcessMessages;
  end;
end; // SleepWell


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


function ClipboardAsString : string;
begin
  if ( Clipboard.HasFormat( CF_TEXT )) then
    result := Clipboard.AsText
  else
    result := '';
end; // ClipboardAsString

function FirstLineFromClipboard( const MaxLen : integer ) : string;
var
  i, l, max : integer;
begin
  result := trimleft( ClipboardAsString );
  l := length( result );
  if ( l > 0 ) then
  begin
    if ( MaxLen < l ) then
      max := MaxLen
    else
      max := l;
    for i := 1 to max do
    begin
      if ( result[i] < #32 ) then
      begin
        delete( result, i, l );
        break;
      end;
    end;
  end;
end; // FirstLineFromClipboard

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

function GetAppFromExt( const Ext : string; const RemoveArgs : boolean ) : string;
// adapted from Borland FAQ
var
  S : string;
  p : integer;
begin
  s := '';
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;

    if OpenKey( '\' + Ext, False ) then
    begin
      S := ReadString( '' );
      if S <> '' Then
      begin
        if OpenKey( '\' + S + '\shell\open\command', False ) then
        begin
          S := ReadString( '' );
        end;
      end
      else
      begin
        If OpenKey( '\' + Ext + '\shell\open\command', False ) Then
        begin
          S := ReadString( '' );
        end;
      end;
    end;
  finally
    Free;
  end;

  if (( s <> '' ) and RemoveArgs ) then
  begin
   p := pos( ' "%1"', s );
   if ( p > 0 ) then
       delete( s, p, length( s ) );
  end;
  if ( s <> '' ) then
  begin
   if ( s[1] = '"' ) then
   begin
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
  result := extractfilename( FN );
  p := lastpos( '.', result );
  if ( p > 0 ) then
    delete( result, p, length( result ));
end; // BareFileName

function ProperFolderName( folder : string ) : string;
begin
  folder := ansilowercase( trim( folder ));
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

function VerdanaInstalled : boolean;
var
   i : integer;
begin
  { look BACKWARDS, because the string 'Verdana'
  will be closer to the end of the font list, so
  we will find it faster by looking from the end }
  result := false;
  for i := pred( screen.fonts.count ) downto 0 do
    if ( screen.fonts[i] = 'Verdana' ) then
    begin
      result := true;
      break;
    end;
end; // VerdanaInstalled

function TahomaInstalled : boolean;
var
   i : integer;
begin
  { look BACKWARDS, because the string 'Verdana'
  will be closer to the end of the font list, so
  we will find it faster by looking from the end }
  result := false;
  for i := pred( screen.fonts.count ) downto 0 do
    if ( screen.fonts[i] = 'Tahoma' ) then
    begin
      result := true;
      break;
    end;
end; // TahomaInstalled



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
    fname    := 'MS Sans Serif';
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
  result := ansilowercase( result );
end; // NormalFN

function RelativeFN( FN : string ) : string;
begin
  // given a full path and filename, returns only the
  // filename part IF the path is the same as the application's
  // own directory (ie the file lives where the program does)
  FN := NormalFN( FN );
  if ( extractfilepath( FN ) = ansilowercase( extractfilepath( ParamStr( 0 )))) then
    result := extractfilename( FN )
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

procedure FontPropertiesToFont( const fp : TFontProperties; aFont : TFont );
begin
  with aFont do
  begin
    name := fp.fname;
    pitch := fp.fpitch;
    size := fp.fsize;
    color := fp.fcolor;
    charset := fp.fcharset;
    style := fp.FStyle;
  end;
end; // FontPropertiesToFont

procedure FontToFontProperties( const aFont : TFont; var fp : TFontProperties );
begin
  with aFont do begin
    fp.fname := name;
    fp.fpitch := pitch;
    fp.fsize := size;
    fp.fcolor := color;
    fp.fcharset := charset;
    fp.fstyle := style;
  end;
end; // FontToFontProperties


function PopUpMessage( const mStr : string; const mType : TMsgDlgType; const mButtons : TMsgDlgButtons; const mHelpCtx : integer ) : word;
// Like MessageDlg, but brings application window to front before
// displaying the message, and minimizes application if it was
// minimized before the message was shown.
var
  wasiconic : boolean;
begin
  wasiconic := ( IsIconic(Application.Handle) = TRUE );
  if wasiconic then
    Application.Restore;
  Application.BringToFront;
  result := messagedlg( mStr, mType, mButtons, mHelpCtx );
  if wasiconic then
    Application.Minimize;
end; // PopUpMessage


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

Function DefMessageDlg(const aCaption: String;
                       const Msg: string;
                       DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons;
                       DefButton: Integer;
                       HelpCtx: Longint): Integer;
// code by Peter Below (TeamB)
Var
  i: Integer;
  btn: TButton;
Begin
  With CreateMessageDialog(Msg, DlgType, Buttons) Do
  try
    Caption := aCaption;
    HelpContext := HelpCtx;
    For i := 0 To ComponentCount-1 Do Begin
      If Components[i] Is TButton Then Begin
        btn := TButton(Components[i]);
        btn.Default := btn.ModalResult = DefButton;
        If btn.Default Then
          ActiveControl := Btn;
      End;
    End; { For }
    Result := ShowModal;
  finally
    Free;
  end;
End; // DefMessageDlg

function GetExtAlphChar( aChar : char ) : char;
begin
  case aChar of
    'π', '•' : result := 'a';
    'Ê', '∆' : result := 'c';
    'Í', ' ' : result := 'e';
    '≥', '£' : result := 'l';
    'Ò', '—' : result := 'n';
    'Û', '”' : result := 'o';
    'ú', 'å' : result := 's';
    'ü', 'è' : result := 'z';
    'ø', 'Ø' : result := 'z';
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
    if not IsCharAlphaA( w[i] ) then
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


{************************************************************
 * Procedure PostKeyEx32
 *
 * Parameters:
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 * Description:
 *  Uses keybd_event to manufacture a series of key events matching
 *  the passed parameters. The events go to the control with focus.
 *  Note that for characters key is always the upper-case version of
 *  the character. Sending without any modifier keys will result in
 *  a lower-case character, sending it with [ssShift] will result
 *  in an upper-case character!
 *Created: 17.7.98 by P. Below
 ************************************************************}
Procedure PostKeyEx32( key: Word; Const shift: TShiftState;
                     specialkey: Boolean );
  Type
    TShiftKeyInfo = Record
                      shift: Byte;
                      vkey : Byte;
                    End;
    byteset = Set of 0..7;
  Const
    shiftkeys: Array [1..3] of TShiftKeyInfo =
      ((shift: Ord(ssCtrl); vkey: VK_CONTROL ),
       (shift: Ord(ssShift); vkey: VK_SHIFT ),
       (shift: Ord(ssAlt); vkey: VK_MENU ));
  Var
    flag: DWORD;
    bShift: ByteSet absolute shift;
    i: Integer;
  Begin
    For i := 1 To 3 Do Begin
      If shiftkeys[i].shift In bShift Then
        keybd_event( shiftkeys[i].vkey,
                     MapVirtualKey(shiftkeys[i].vkey, 0),
                     0, 0);
    End; { For }
    If specialkey Then
      flag := KEYEVENTF_EXTENDEDKEY
    Else
      flag := 0;

    keybd_event( key, MapvirtualKey( key, 0 ), flag, 0 );
    flag := flag or KEYEVENTF_KEYUP;
    keybd_event( key, MapvirtualKey( key, 0 ), flag, 0 );

    For i := 3 DownTo 1 Do Begin
      If shiftkeys[i].shift In bShift Then
        keybd_event( shiftkeys[i].vkey,
                     MapVirtualKey(shiftkeys[i].vkey, 0),
                     KEYEVENTF_KEYUP, 0);
    End; { For }
  End; { PostKeyEx32 }


{************************************************************
 * Procedure PostKeyEx
 *
 * Parameters:
 *  hWindow: target window to be send the keystroke
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *           For letters alsways use the upper-case version!
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 *           If this parameter is true, bit 24 of the lparam for
 *           the posted WM_KEY* messages will be set.
 * Description:
 *  This procedure sets up Windows key state array to correctly
 *  reflect the requested pattern of modifier keys and then posts
 *  a WM_KEYDOWN/WM_KEYUP message pair to the target window. Then
 *  Application.ProcessMessages is called to process the messages
 *  before the keyboard state is restored.
 * Error Conditions:
 *  May fail due to lack of memory for the two key state buffers.
 *  Will raise an exception in this case.
 * NOTE:
 *  Setting the keyboard state will not work across applications
 *  running in different memory spaces on Win32.
 *Created: 02/21/96 16:39:00 by P. Below
 ************************************************************}
Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift:
    TShiftState; specialkey: Boolean );
Type
  TBuffers = Array [0..1] of TKeyboardState;
Var
  pKeyBuffers : ^TBuffers;
  lparam: cardinal;
Begin
  (* check if the target window exists *)
  If IsWindow(hWindow) Then Begin
    (* set local variables to default values *)
    pKeyBuffers := Nil;
    lparam := MakeLong(0, MapVirtualKey(key, 0));

    (* modify lparam if special key requested *)
    If specialkey Then
      lparam := lparam or $1000000;

    (* allocate space for the key state buffers *)
    New(pKeyBuffers);
    try
      (* Fill buffer 1 with current state so we can later restore it.
         Null out buffer 0 to get a "no key pressed" state. *)
      GetKeyboardState( pKeyBuffers^[1] );
      FillChar(pKeyBuffers^[0], Sizeof(TKeyboardState), 0);

      (* set the requested modifier keys to "down" state in the buffer
*)
      If ssShift In shift Then
        pKeyBuffers^[0][VK_SHIFT] := $80;
      If ssAlt In shift Then Begin
        (* Alt needs special treatment since a bit in lparam needs also
be set *)
        pKeyBuffers^[0][VK_MENU] := $80;
        lparam := lparam or $20000000;
      End;
      If ssCtrl In shift Then
        pKeyBuffers^[0][VK_CONTROL] := $80;
      If ssLeft In shift Then
        pKeyBuffers^[0][VK_LBUTTON] := $80;
      If ssRight In shift Then
        pKeyBuffers^[0][VK_RBUTTON] := $80;
      If ssMiddle In shift Then
        pKeyBuffers^[0][VK_MBUTTON] := $80;

      (* make out new key state array the active key state map *)
      SetKeyboardState( pKeyBuffers^[0] );

      (* post the key messages *)
      If ssAlt In Shift Then Begin
        PostMessage( hWindow, WM_SYSKEYDOWN, key, lparam);
        PostMessage( hWindow, WM_SYSKEYUP, key, lparam or $C0000000);
      End
      Else Begin
        PostMessage( hWindow, WM_KEYDOWN, key, lparam);
        PostMessage( hWindow, WM_KEYUP, key, lparam or $C0000000);
      End;
      (* process the messages *)
      Application.ProcessMessages;

      (* restore the old key state map *)
      SetKeyboardState( pKeyBuffers^[1] );
    finally
      (* free the memory for the key state buffers *)
      If pKeyBuffers <> Nil Then
        Dispose( pKeyBuffers );
    End; { If }
  End;
End; { PostKeyEx }

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
    0	: result := 'The operating system is out of memory or resources.';
    ERROR_FILE_NOT_FOUND : result := 'The specified file was not found.';
    ERROR_PATH_NOT_FOUND : result := 'The specified path was not found.';
    ERROR_BAD_FORMAT : result := 'The .EXE file is invalid (non-Win32 .EXE or error in .EXE image).';
    SE_ERR_ACCESSDENIED : result := 'The operating system denied access to the specified file.';
    SE_ERR_ASSOCINCOMPLETE : result := 'The filename association is incomplete or invalid.';
    SE_ERR_DDEBUSY : result := 'The DDE transaction could not be completed because other DDE transactions were being processed.';
    SE_ERR_DDEFAIL : result := 'The DDE transaction failed.';
    SE_ERR_DDETIMEOUT : result := 'The DDE transaction could not be completed because the request timed out.';
    SE_ERR_DLLNOTFOUND : result := 'The specified dynamic-link library was not found.';
    // SE_ERR_FNF : result := 'The specified file was not found.';
    SE_ERR_NOASSOC : result := 'There is no application associated with the given filename extension.';
    SE_ERR_OOM : result := 'There was not enough memory to complete the operation.';
    // SE_ERR_PNF : result := 'The specified path was not found.';
    SE_ERR_SHARE : result := 'A sharing violation occurred';
    else
      result := 'Unknown error.';
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

end.





