unit kn_DateTime;

(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000-2003. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

interface
uses Windows, Classes, SysUtils, gf_misc;

const
  _DTFORMAT_COMMENT_CHAR    = '#';
  _ENGLISH_DATE_PREFIX_CHAR = '@';

  _DateFormatsFile = 'dateformats.txt';
  _TimeFormatsFile = 'timeformats.txt';

  _MAX_DATE_FORMATS = 25;
  _MAX_TIME_FORMATS = 9;

  {
  TIME_FORMAT_LIST : array[1..MAX_TIME_FORMATS] of string = (
    'k',            // time in KeyNote default format
    't',            // time in ShortTimeFormat
    'tt',           // time in LongTimeFormat
    'hh:mm',
    'hh:mm:ss',
    'hh:mm ampm',    // time with system default am/pm specifier
    'hh:mm:ss ampm',
    'hh:mm am/pm',   // time with am/pm specifier
    'hh:mm:ss am/pm'
  );
  }

  {
  DATE_FORMAT_LIST : array[1..MAX_DATE_FORMATS] of string = (
    'k', // date in KeyNote default format
    'ddddd',
    'dddddd',
    'c',
    'dd-MM-yy',
    'dd-MM-yyyy',
    'dddd, dd mmmm yyyy',
    'dd mmmm yyyy (dddd)',
    'dd mmmm yyyy',
    'dd mmmm, yyyy',
    'yy/mm/dd',
    'yyyy/mm/dd',
    'mmmm, dd',
    'mmmm dd, yyyy',
    'dd mmmm',
    'dd/mmm/yy',
    'dd/mmm/yyyy',
    'dd/mm/yyyy hh:mm',
    'dd/mm/yyyy hh:mm:ss',
    'dd mmm yyyy hh:mm',
    'dd mmm yyyy hh:mm:ss',
    'dd mmmm yyyy hh:mm',
    'dd mmmm yyyy hh:mm:ss',
    'dddd, dd mmmm yyyy hh:mm',
    'dddd, dd mmmm yyyy hh:mm:ss'
  );
  }

var
  DATE_FORMAT_LIST : TStringList;
  TIME_FORMAT_LIST : TStringList;

function LoadDateFormatsList : boolean;
function LoadTimeFormatsList : boolean;
function GetDateTimeFormatted( fmtstr : string; const DT : TDateTime ) : WideString;

implementation

function GetDateTimeFormatted( fmtstr : string; const DT : TDateTime ) : WideString;
begin
  if ( fmtstr <> '' ) and ( fmtstr[1] = _ENGLISH_DATE_PREFIX_CHAR ) then
  begin
    delete( fmtstr, 1, 1 );
    result := FormatDateTimeEnglish( fmtstr, DT );
  end
  else
    result := FormatDateTime( fmtstr, DT );
end; // GetDateTimeFormatted

function LoadFormatFile( const fn : string; List : TStrings; MaxCount : integer ) : boolean;
var
  i : integer;
  s : string;
  f : textfile;
begin
  result := false;

  if ( not fileexists( fn )) then exit;

  assignfile( f, fn );

  try
    reset( f );
    try
      while ( not eof( f )) and ( List.Count < MaxCount ) do
      begin
        readln( f, s );
        s := trim( s );
        if ( s = '' ) then continue;
        case s[1] of
          _DTFORMAT_COMMENT_CHAR : continue;
          else
            List.Add( s );
        end;
      end;
      result := true;
    finally
      closefile( f );
    end;
  except
    result := false;
  end;
end; // LoadFormatFile

function LoadDateFormatsList : boolean;
var
  cnt, i : integer;
begin
  DATE_FORMAT_LIST.Clear;

  DATE_FORMAT_LIST.Add( 'k' ); // date in KeyNote default format
  DATE_FORMAT_LIST.Add( 'ddddd' ); // system default short date format
  DATE_FORMAT_LIST.Add( 'dddddd' ); // system default long date format
  DATE_FORMAT_LIST.Add( 'c' ); // short date format + short time format

  result := LoadFormatFile( extractfilepath( ParamStr( 0 )) + _DateFormatsFile, DATE_FORMAT_LIST, _MAX_DATE_FORMATS );

end; // LoadDateFormatsList

function LoadTimeFormatsList : boolean;
var
  cnt, i : integer;
begin
  TIME_FORMAT_LIST.Clear;

  TIME_FORMAT_LIST.Add( 'k' ); // KeyNote default
  TIME_FORMAT_LIST.Add( 't' ); // system default short time format
  TIME_FORMAT_LIST.Add( 'tt' ); // system default long time format

  result := LoadFormatFile( extractfilepath( ParamStr( 0 )) + _TimeFormatsFile, TIME_FORMAT_LIST, _MAX_TIME_FORMATS );

end; // LoadTimeFormatsList


Initialization

  TIME_FORMAT_LIST := TStringList.Create;
  DATE_FORMAT_LIST := TStringList.Create;

  LoadDateFormatsList;
  LoadTimeFormatsList;

Finalization

  TIME_FORMAT_LIST.Free;
  DATE_FORMAT_LIST.Free;


end.


