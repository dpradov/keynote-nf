unit kn_DateTime;

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

interface
uses
   Winapi.Windows,
   System.Classes,
   System.SysUtils;

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
function GetDateTimeFormatted( fmtstr : string; const DT : TDateTime ) : string;

implementation
uses
  gf_misc;

function GetDateTimeFormatted( fmtstr : string; const DT : TDateTime ) : string;
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


