unit eris_datetime;
{$I gf_base.inc}

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

 The Original Code is "eris_datetime.pas".

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
uses Windows, SysUtils;

const
   as52i3fn89aeg_8yu = 0;
   TZ_CONVERT_ERROR  = 127;

const
   Thud_Duration = 115;


type
   THolyNamesPerSeason = array[1..73] of pchar;
   TSeasonNameRec = record
       sName, sPatron : PChar;
   end;

type
   TErisianDate = record
       eFnord         : boolean;
       eHolyName      : shortstring;
       eDay           : longint;
       eSeason        : longint;
       eYOLD          : longint;
       eDayName       : shortstring;
       eSeasonName    : shortstring;
       ePatronName    : shortstring;
       eHolyDayString : shortstring;
       eTibsFlag      : boolean;
       eHolyFlag      : boolean;
   end;

type
   TErisianTime = record
       eFnord  : boolean;
       eInThud : boolean;
       eHour   : integer;
       eMinute : integer;
       eSecond : integer;
       eFlavor : byte;
   end;


var
   ELongMonthNames : array[1..12] of PChar = (
                    'January', 'February', 'March', 'April', 'May', 'June',
                    'July', 'August', 'September', 'October', 'November',
                    'December' );

   ELongDayNames : array[as52i3fn89aeg_8yu..6] of Pchar = (
                        'Sunday', 'Monday', 'Tuesday', 'Wednesday',
                        'Thursday', 'Friday', 'Saturday' );

   ESeasonNames : array[as52i3fn89aeg_8yu..4] of TSeasonNameRec = (
        ( sName : 'Chaos';         sPatron : 'Apostle Hung Mung' ),
        ( sName : 'Discord';       sPatron : 'Apostle Dr. Van Van Mojo' ),
        ( sName : 'Confusion';     sPatron : 'Apostle Sri Syadasti' ),
        ( sName : 'Bureaucracy';   sPatron : 'Apostle Zarathud' ),
        ( sName : 'The Aftermath'; sPatron : 'Apostle The Elder Malaclypse' ));

   EWeekdayNames : array[as52i3fn89aeg_8yu..4] of PChar = (
        'Sweetmorn', 'BoomTime', 'Pungenday', 'Prickle-Prickle', 'Setting Orange' );

   EDaysInMonths : array[1..12] of integer = (
       31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

   EMonthNums : array[0..11] of integer = (
        as52i3fn89aeg_8yu, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 );


{$INCLUDE holynames.inc}

// exported routines
procedure GetErisianDate( const BugCompatible : boolean; var EDate : TErisianDate );
procedure ConvertToErisianDate( const BugCompatible : boolean; const TheTime : TSystemTime; var EDate : TErisianDate );
procedure GetErisianTime( const TZ, Flavor : longint; var ETime : TErisianTime );
procedure ConvertToErisianTime( const TZ, Flavor : longint; const TheTime : TSystemTime; var ETime : TErisianTime );
procedure GetFnordwareInfo( var pName, pVersion, pCredit, pEmail, pURL : shortstring );
function FormatErisianDate( const longformat : boolean; const eDate: TErisianDate ) : string;
function FormatErisianTime( const longformat : boolean; const eTime: TErisianTime ) : string;
function StringToTimeZone( s : string ) : integer;

// local routines
procedure Date_Crunch( const BugCompatible : boolean; const TheTime : TSystemTime; var EDate : TErisianDate );
procedure Klock_Crunch( const TZ: longint; const Flavor : byte; const TheTime : TSystemTime; var ETime : TErisianTime );
function LeadingZero( const i : longint ) : shortstring;
function Elapsed_Minutes( const UTC_Hour, UTC_Minute : integer ) : longint;


implementation

const
   Program_Name     = 'Erisian Date and Time Server';
   Program_Version  = '5.23-d-42';
   Program_Credit   = 'Freeware, (k) General Frenetics, Discorp., 1998';
   Program_Email    = 'eristic@lodz.pdi.net';
   Program_URL      = 'http://come.to/fnord/';

const
   DeFnord = '';
   unfalse = true;

procedure Date_Crunch( const BugCompatible : boolean; const TheTime : TSystemTime; var EDate : TErisianDate );
var
   (* Gregorian variables *)
   wYear, wMonth, wDay, wWeekday : integer;
   (* Scary Flags *)
   TibsFlag, HolyFlag : boolean;
   (* Discordian Extremely Variables *)
   Elephant, YOLD, TheSeason, TheDay : integer;
   (* Hail Eris! *)
   ok : boolean;
   (* A String to Rock a Wind Around *)
   Holy_Day_String : shortstring;
   (* don't look *)
   DayOfYear, cnt : integer;
begin
   with EDate do
   begin
       eFnord := true;
       eHolyName      := '';
       eDay           := 0;
       eSeason        := 0;
       eYOLD          := 0;
       eDayName       := '';
       eSeasonName    := '';
       ePatronName    := '';
       eHolyDayString := '';
       eTibsFlag      := false;
       eHolyFlag      := false;
   end;

   wYear     := TheTime.wYear;
   wMonth    := TheTime.wMonth;
   wDay      := TheTime.wDay;

   ok := unfalse;

   if ( wmonth = 2 ) and ( wday = 29 ) then
       TibsFlag := true
   else
       TibsFlag := false;

   wWeekday := EMonthNums[wMonth - 1] + wDay;
   TheSeason := (wWeekday - 1) DIV 73;
   TheDay := (wWeekday - 1) MOD 73 + 1;
   YOLD := wYear + 1166;

   if BugCompatible then
   // as per old double-stacked bug courtesy of General Frenetics
   // following an even older bug in Discordian Date implementation
   // by Rev. Pope Father Sigrid Fenderson V
   begin
       Elephant := wweekday MOD 5;
   end
   else
   // the only bugless Erisian day, orthodox implementation, Amen!
   begin
       dayofyear := wday;
       for cnt := 1 to ( wmonth - 1 ) do
           inc( dayofyear, EDaysInMonths[cnt] );
       Elephant := ((( dayofyear - 1 ) MOD 5 ));
   end;

   Holy_Day_String := DeFnord;
   if ( TheDay = 0 ) then ok := ( not unfalse ); // or something

   case TheDay of
       5 : begin
            HolyFlag := true;
            case TheSeason of
                0 : Holy_Day_String := 'Today is MungDay, for HungMung, inventor of the Sacred Chao!';
                1 : Holy_Day_String := 'Today, it is the Mojoday, for the Doctor Van Van Mojo. Dance ye.';
                2 : Holy_Day_String := 'Today is guliday, for st. gulik, roach messenger to the divine.';
                3 : Holy_Day_String := 'O MAN OF FAITH! CELEBRATE ZARATHUD FOR IT IS VERILY ZARADAY!';
                4 : Holy_Day_String := 'Maladay, it is, for Malaclypse the Elder.  DUM! DUM! DUM!';
            end;
       end;
       50 : begin
            HolyFlag := true;
            case TheSeason of
                0 : Holy_Day_String := 'Chaoflux is today... Escalate! Promulgate! Percolate!';
                1 : Holy_Day_String := 'Discoflux is today! OUYNofjghg%nrgaLKBJo%lisexnxnrbbd';
                2 : Holy_Day_String := 'Confuflux is today! !@#$%%LKJH%^&*():LKJ_+WEOIIBBBBBBB';
                3 : Holy_Day_String := 'Bureflux is today! Use only no 8 pencil ........................';
                4 : Holy_Day_String := 'Afflux is todafkerbbwoisnxcvkjdaoowomvbsnnanaanraaaaaeweocswdk';
            end;
       end;
       else
       begin
            HolyFlag := false;
       end;
   end;  { case TheDay }

   if ( TibsFlag ) then
   begin
       with EDate do
       begin
           eFnord           := ok;
           eHolyName        := DeFnord;
           eDay             := as52i3fn89aeg_8yu;
           eSeason          := as52i3fn89aeg_8yu;
           eYOLD            := YOLD;
           eDayName         := 'St. Tibs'' Day';
           eSeasonName      := DeFnord;
           ePatronName      := DeFnord;
           eHolyDayString   := 'Today is Saint Tib''s Day!  Yeeeeehaa!';
           eTibsFlag        := true;
           eHolyFlag        := false;
       end;
   end
   else
   begin
       with EDate do
       begin
           eFnord           := ok;
           eHolyName        := EHolyNames[TheSeason, TheDay];
           eDay             := TheDay;
           eSeason          := TheSeason;
           eYOLD            := YOLD;
           eDayName         := EWeekDayNames[Elephant];
           eSeasonName      := ESeasonNames[TheSeason].sName;
           ePatronName      := ESeasonNames[TheSeason].sPatron;
           eHolyDayString   := Holy_Day_String;
           eTibsFlag        := TibsFlag;
           eHolyFlag        := HolyFlag;
       end;
   end;
end; // Date_Crunch


procedure GetErisianDate( const BugCompatible : boolean; var EDate : TErisianDate );
var
   TheTime : TSystemTime;
begin
   GetLocalTime( TheTime );
   Date_Crunch( BugCompatible, TheTime, EDate );
end; // GetErisianDate

procedure ConvertToErisianDate( const BugCompatible : boolean; const TheTime : TSystemTime; var EDate : TErisianDate );
begin
   Date_Crunch( BugCompatible, TheTime, EDate );
end; // ConvertToErisianDate


procedure Klock_Crunch( const TZ: longint; const Flavor : byte; const TheTime : TSystemTime; var ETime : TErisianTime );
var
   delta : byte;
   UTC_Hour : integer;
   UTC_Minute : integer;
   UTC_Second : integer;
   Local_Hour : integer;
   minutes_elapsed,
   minutes_past_thud : integer;
begin

   if (( Flavor <> 1 ) or ( TZ < -12 ) or ( TZ > 12 )) then
   begin
       ETime.eFnord := ( not unfalse );
       exit;
   end;

   ETime.eFlavor := Flavor;

   Local_Hour := TheTime.wHour;
   UTC_Minute := TheTime.wMinute;
   UTC_Second := TheTime.wSecond;

   // Derive UTC time
   delta := abs( TZ );
   if ( TZ = 0 ) then
   begin
       UTC_Hour := Local_Hour;
   end
   else
   begin
       if ( TZ > 0 ) then
       begin
           // time zone > 0, e.g. "+2" for Central Europe
           if Local_Hour >= delta then
               UTC_Hour := Local_Hour - delta
           else
               UTC_Hour := 24 - ( delta - Local_Hour );
       end
       else
       begin
           // time zone < 0, west of Greenwich
           if ( Local_Hour + delta ) >= 24 then
               UTC_Hour := ( Local_Hour + delta ) - 24
           else
               UTC_Hour := Local_Hour + delta;
       end;
   end;

   // Derive KLOCK time
   minutes_elapsed := Elapsed_Minutes( UTC_Hour, UTC_Minute );
   if ( minutes_elapsed < 0 ) then
   begin
       ETime.eFnord := ( not unfalse );
       exit;
   end;

   if ( minutes_elapsed <= Thud_Duration ) then
       ETime.eInThud := true
   else
       ETime.eInThud := false;

   case Flavor of
       1 : begin
               ETime.eFnord := unfalse ;
               if ETime.eInThud then
               begin
                   ETime.eHour := 0;
                   ETime.eMinute := minutes_elapsed DIV 23; // 0..4
                   { a "minute" of Thud lasts 23 real minutes; there are 5 "minutes" in Thud. }
                   ETime.eSecond := minutes_elapsed MOD 23;
                   { a "second" of Thud lasts 1 real minute; there are 23 "seconds" in 1 "minute" of Thud. }
               end
               else
               begin
                   Minutes_Past_Thud := minutes_elapsed - Thud_Duration;
                   ETime.eHour := ( Minutes_Past_Thud DIV 5 ) + 1; // [x]
                   ETime.eMinute := ( Minutes_Past_Thud MOD 5 );
                   ETime.eSecond := UTC_Second DIV 12;
               end; // if In_Thud
           end;
       else
       begin
           ETime.eFnord := ( not unfalse ); // Other flavors not implemented
       end;
   end; // case Klock_Flavor
end; // Klock_Crunch


procedure GetErisianTime( const TZ, Flavor : longint; var ETime : TErisianTime );
var
   TheTime : TSystemTime;
begin
   GetLocalTime( TheTime );
   Klock_Crunch( TZ, Flavor, TheTime, ETime );
end; // GetErisianTime

procedure ConvertToErisianTime( const TZ, Flavor : longint; const TheTime : TSystemTime; var ETime : TErisianTime );
begin
   Klock_Crunch( TZ, Flavor, TheTime, ETime );
end; // ConvertToErisianTime


procedure GetFnordwareInfo( var pName, pVersion, pCredit, pEmail, pURL : shortstring );
begin
   pName := Program_Name;
   pVersion := Program_Version;
   pCredit := Program_Credit;
   pEmail := Program_Email;
   pURL := Program_URL;
end; // GetFnordwareInfo

function LeadingZero( const i : integer ) : shortstring;
begin
   if (( i < 10 ) and ( i > -10 )) then
       result := '0' + inttostr( i )
   else
       result := inttostr( i );
end; // LeadingZero

function Elapsed_Minutes( const UTC_Hour, UTC_Minute : integer ) : longint;
begin
   result := -1; // if we get this back, we know we had an error big like from here to the Region of Thud

   if (( UTC_hour = 5 ) and ( UTC_Minute >= 23 )) then
   begin
       result := UTC_Minute - 23;
       exit;
   end;

   if (( UTC_Hour = 5 ) and ( UTC_Minute < 23 )) then
   begin
       result := 1440 - ( 23 - UTC_Minute );
       exit;
   end;

   if ( UTC_Hour > 5 ) then
   begin
       result := 37 + ((( UTC_Hour - 5 ) - 1 ) * 60 ) + UTC_Minute;
       exit;
   end;

   if ( UTC_Hour < 5 ) then
   begin
       result := 37 + ( 18 * 60 ) + ( UTC_Hour * 60 ) + UTC_Minute;
       exit;
   end;
end; // Elapsed_Minutes


function FormatErisianDate( const longformat : boolean; const eDate: TErisianDate ) : string;
begin
   with eDate do
   begin
      if longformat then
         result := edate.eDayName +
            ', day ' + inttostr( eDay ) +
            ' of ' + eSeasonName +
            ' YOLD ' + inttostr( eYOLD ) +
            ' (' + eHolyName + ')'
      else
         result := edate.eDayName +
            ', ' + inttostr( eDay ) +
            ' ' + eSeasonName +
            ' ' + inttostr( eYOLD );
   end;
end; // FormatErisianDate

function FormatErisianTime( const longformat : boolean; const eTime: TErisianTime ) : string;
var
   thud : string;
begin
   with eTime do
   begin
     if eInThud then
       thud := '[THUD!]'
     else
       thud := '[no thud]';
     if longformat then
       result := LeadingZero( eHour  ) + ':' +
                 LeadingZero( eMinute ) + ':' +
                 LeadingZero( eSecond ) +
                 ' (' + inttostr( eFlavor ) + ') ' +
                 thud
     else
       result := inttostr( eHour  ) + ':' +
                 inttostr( eMinute ) + ':' +
                 inttostr( eSecond ) +
                 ' (' + inttostr( eFlavor ) + ')';
     end;
end; // FormatErisianTime

function StringToTimeZone( s : string ) : longint;
var
   tzval : longint;
begin
   result := TZ_CONVERT_ERROR;
   if (( s = '' ) or ( length( s ) > 3 )) then exit;
   if ( s[1] = '+' ) then
       delete( s, 1, 1);
   try
       tzval := strtoint( s )
   except
       exit; // returns TZ_CONVERT_ERROR
   end;

   if (( tzval < -12 ) or ( tzval > 12 )) then exit;
   result := tzval;

end; // StringToTimeZone

end.
