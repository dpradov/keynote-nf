unit gf_strings;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

{$I gf_base.inc}

interface
uses
   Winapi.Windows,
   Winapi.ShellApi,
   System.SysUtils,
   System.Classes,
   System.WideStrUtils;


type
  TCaseCycle = (
    ccLower, ccMixed, ccUpper
  );

function GetLetterCase( const aStr : string ) : TCaseCycle;

procedure DelimTextToStrs(
  AStrs: TStrings;
  const Value: string;
  const AchDelim : Char );



function StrToCSVText(const aStr : string; const aDelim : char; const QuoteAll : boolean ) : string;
procedure CSVTextToStrs(
  aList : TStrings;
  const aStr : string;
  const aDelim : char);

procedure CommandLineToStrings(const CommandLine: String; Strs: TStrings);

procedure SplitString(aList : TStrings; aStr : string; const aDelim : char; ignoreEmptySplit: boolean= true);

function CountChars(const ch : char; const s : string) : integer;
function CountNonControlCharsNoSpace(const s : string) : integer;
procedure CharToChar(var s : string; const oldchar, newchar : char);
function RemoveAccelChar(const s : string) : string;
procedure CollapseSpaces(var s : string);
function TrimPunct(s : string) : string;

function TailMatch(const LongerString, ShorterString : string) : boolean;
procedure UnquoteString(var s : string);
function StringsToString(const AStrs : TStrings; const aDelim : char; const WrapWithDelim : boolean) : string;

function MatchMask(source, pattern: String): Boolean;
procedure StripControlChars(var s : string);
function GetWordChars : string;

function ExpandMetaChars(line : string; AllowNonEscapedBackSlash: boolean = true) : string;
function GetIndentOfLine (const S: string): integer;

function TryUTF8ToUnicodeString(const s: RawByteString): string;
function CanSaveAsANSI(const cad: string): boolean;
function ConvertToUnicodeString(LBuffer: TBytes): string;
function BytesToRawByteString(const bytes: TBytes): RawByteString;

function FirstLineFromString(const str: string; const MaxLen : integer) : string;
function NFromLastCharPos(const S: string; const Chr: char; nthOccurrence: integer= 1): integer;

function ConvertHTMLAsciiCharacters(const S: string): string;

const
  WordDelimiters = [#9, #10, #13, #32];
  UTF16_LE_BOM = AnsiString(#$FF#$FE);
  UTF16_BE_BOM = AnsiString(#$FE#$FF);


function GetWordCount( const t : string ) : longint;
function HasNonAlphaNumericOrWordDelimiter(const s : string) : boolean;
function NumberOfLineFeed(Str: string): integer;



implementation


function GetLetterCase( const aStr : string ) : TCaseCycle;
const
  _max_check_len = 255; // do not waste time checking multi-kilobyte strings
var
  i, len : integer;
  HadUpper, HadLower : boolean;
begin
  result := ccLower;
  HadUpper := false;
  HadLower := false;
  len := length( aStr );
  if ( len > _max_check_len ) then
    len := _max_check_len;
  for i := 1 to len do
  begin
    if IsCharAlpha( aStr[i] ) then
    begin
      if IsCharUpper( aStr[i] ) then
        HadUpper := true
      else
        HadLower := true;
      if ( HadUpper and HadLower ) then
      begin
        result := ccMixed;
        exit;
      end;
    end;
  end;

  if HadUpper then
    result := ccUpper
  else
    result := ccLower;

end; // GetLetterCase

function ExpandMetaChars( line : string; AllowNonEscapedBackSlash: boolean = true ) : string;
var
  i, linelen : integer;
  wasmeta : boolean;
  ch : Char;
begin
  result := '';

  if ( line = '' ) then exit;
  linelen := length( line );
  wasmeta := false;

  if ( linelen > 1 ) then
  begin
    if (( line[1] = '"' ) and ( line[linelen] = '"' )) then
    begin
      delete( line, linelen, 1 );
      delete( line, 1, 1 );
      dec( linelen, 2 );
    end;
  end;

  for i := 1 to linelen do
  begin
    ch := line[i];
    case ch of
      '\' : begin
        if wasmeta then
        begin
          result := result + ch;
          wasmeta := false;
        end
        else
          wasmeta := true;
      end;
      'n' : begin
        if wasmeta then
          result := result + #13
        else
          result := result + ch;
        wasmeta := false;
      end;
      't' : begin
        if wasmeta then
          result := result + #9
        else
          result := result + ch;
        wasmeta := false;
      end;
      else begin
        if wasmeta and AllowNonEscapedBackSlash then
           result := result + '\';

        wasmeta := false;
        result := result + ch;
      end;
    end;
  end;
end; // ExpandMetaChars

procedure StripControlChars( var s : string );
var
  i, l : integer;
begin
  l := length( s );
  for i := l downto 1 do
  begin
    if ( s[i] < #32 ) then
      delete( s, i, 1 );
  end;
end; // StripControlChars

function TailMatch( const LongerString, ShorterString : string ) : boolean;
var
  llonger, lshorter : integer;
begin
  llonger := length( LongerString );
  lshorter := length( shorterstring );

  if ( lshorter > llonger ) then
  begin
    result := false;
    exit;
  end;

  {
  'abcdxxx' 7
  '    xxx' 3, pos 5
  }

  result := ( copy( LongerString, succ( llonger - lshorter ), lshorter ) = ShorterString );
end; // TailMatch

function TrimPunct( s : string ) : string;
var
  slen : integer;
begin
  while ( s <> '' ) do
  begin
    if IsCharAlphaNumeric( s[1] ) then
      break
    else
      delete( s, 1, 1 );
  end;
  slen := length( s );
  while ( slen > 0 ) do
  begin
    if IsCharAlphaNumeric( s[slen] ) then
      break
    else
    begin
      delete( s, slen, 1 );
      dec( slen );
    end;
  end;
  result := s;
end; // TrimPunct


function StrToCSVText(
  const aStr : string;
  const aDelim : char;
  const QuoteAll : boolean ) : string;
// aDelim MUST NOT be double quote '"'
var
  qp : integer;
  MustQuote : boolean;
begin
  result := aStr;
  if ( aStr = '' ) then
  begin
    if QuoteAll then
      result := '""';
    exit;
  end;

  MustQuote := ( pos( aDelim, aStr ) > 0 );

  if ( pos( '"', result ) > 0 ) then
  begin
    MustQuote := true;
    qp := 1;
    repeat
      if ( result[qp] <> '"' ) then
      begin
        inc( qp );
      end
      else
      begin
        insert( '"', result, qp );
        inc( qp, 2 );
      end;
    until ( qp > length( result ));
  end;

  if ( MustQuote or QuoteAll ) then
    result := '"' + result + '"';

end; // StrToCSVText

procedure CSVTextToStrs(
  aList : TStrings;
  const aStr : string;
  const aDelim : char );
// DelimTextToStrs relies on all strings being wrapped in quotes,
// but this is not a requirement for delimited fields in a CSV file.
// (Otherwise, DelimTextToStrs breaks on every space, in addition to
// breaking on delimiter characters)
// This routine does what DelimTextToStrs does but ONLY breaks on
// the delimiter char. Only those strings must be quoted which contain
// quotes or contain the delimiter character.
var
  s : string;
  InQuotes : boolean;
  p, l : integer;
  ch, prevch : char;
begin

  if ( aStr = '' ) then exit;
  s := '';
  p := 0;
  prevch := #0;
  l := length( aStr );
  InQuotes := false;

  aList.BeginUpdate; // so that we can feed e.g. ListBox.Items to this routine
  try
    while ( p < l ) do
    begin
      inc( p );
      ch := aStr[p];
      if ( ch = aDelim ) then
      begin
        if ( InQuotes and ( prevch <> '"' )) then
        begin
          s := s + ch;
        end
        else
        begin
          aList.Add( s );
          s := '';
          InQuotes := false;
          // prevch := #0;
        end;
      end
      else
      if ( ch = '"' ) then
      begin
        if ( prevch = '"' ) then
        begin
          // dooubled quotes are "escaped",
          // i.e. they're real quote characters
          // rather than group words
          s := s + '"';
          ch := #0;
        end
        else
        begin
          if ( InQuotes or ( s <> '' )) then
          begin
            if InQuotes then
            begin
              // nothing; we will see what to do
              // when we get the next char
              if ( p = l ) then
                Ch := aDelim; // otherwise we'll lose the last field if it is a blank string
            end
            else
            begin
              // IMPOSSIBLE: the string was not
              // quoted, so it can't contain
              // embedded quote characters
              raise EConvertError.Create( 'Unmatched double quote in string at pos ' + inttostr( p ));
            end;
          end
          else
          begin
            InQuotes := true;
            ch := #0;
          end;
        end;
      end
      else
      begin
        s := s + ch;
      end;
      prevch := ch;
    end;
    if (( s <> '' ) or ( prevch = aDelim )) then
      aList.Add( s );
  finally
    aList.EndUpdate;
  end;

end; // CSVTextToStrs


{
CommandLineToArgvW: Parses a Unicode command line string and returns an array of pointers to the command line arguments, along with a count of those arguments. This is similar to the standard C1 runtime argv and argc values.
Used to split a command line string into individual arguments, especially when running an application from the command line or passing a command string as an argument to another application.
}
procedure CommandLineToStrings(const CommandLine: String; Strs: TStrings);
var
  Args, ArgsBak: PPWideChar;
  ArgCount, I: Integer;
begin
  Args := CommandLineToArgvW(PChar(CommandLine), ArgCount);
  ArgsBak:= Args;
  try
    for I := 0 to ArgCount - 1 do begin
      Strs.Add(Args^);
      inc(Args);
    end;

  finally
    LocalFree(HLOCAL(ArgsBak));
  end;
end;


procedure SplitString( aList : TStrings; aStr : string; const aDelim : char; ignoreEmptySplit: boolean= true );
var
  p : integer;
  s : string;
begin
// the simplest split routine of all:
// just splits at delimiter, does not care about anything else.
// DO NOT USE FOR PROPERLY FORMATTED CSV DATA!

  p := pos( aDelim, aStr );
  while ( p > 0 ) do  begin
    s := copy( aStr, 1, pred(p));
    delete( aStr, 1, p );
    if not ignoreEmptySplit or (s <> '') then
       aList.Add( s );
    p := pos( aDelim, aStr );
  end;

  if not ignoreEmptySplit or (s <> '') then
    aList.Add( aStr );
end; // SplitString


procedure DelimTextToStrs( AStrs: TStrings;
         const Value: string ;
         const AchDelim : Char );
var
  P, P1   : PChar;
  S   : string;
  chDelim   : char ;
begin
  chDelim := AchDelim ;
  AStrs.BeginUpdate;
  try
  // AStrs.Clear;
  P := PChar(Value);

  while P^ in [#1..' '] do
    P := CharNext(P);

  while P^ <> #0 do
   begin
     if ( P^ = '"' ) then
     S := AnsiExtractQuotedStr(P, '"')
     else
     begin
      P1 := P;
      while (P^ > ' ') and ( P^ <> chDelim ) do
      P := CharNext(P);

      SetString(S, P1, P - P1);
     end;

     AStrs.Add(S);

     while P^ in [#1..' '] do
     P := CharNext(P);

     if P^ = chDelim then // P^ = ',' then
    repeat
     P := CharNext(P);
    until not (P^ in [#1..' ']);

   end;  // while

  finally
    AStrs.EndUpdate;
  end;
end; // DelimTextToStrs




function CountChars( const ch : char; const s : string ) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 1 to length( s ) do
    if s[i] = ch then inc( result );
end; // CountChars


function CountNonControlCharsNoSpace( const s : string ) : integer;
var
  i : integer;
begin
  result := 0;
  for i := 1 to length( s ) do
    if s[i] > #32 then inc(result);
end; // CountNonControlCharsNoSpace


procedure CharToChar( var s : String; const oldchar, newchar : char );
var
  p : integer;
begin
  if ( oldchar = newchar ) then exit;
  p := pos( oldchar, s );
  while ( p > 0 ) do
  begin
    s[p] := Char(newchar);
    p := pos( oldchar, s );
  end;
end; // CharToChar

function RemoveAccelChar( const s : string ) : string;
var
  p : integer;
begin
  result := s;
  p := pos( '&', result );
  if ( p > 0 ) then
    delete( result, p, 1 );
end; // RemoveAccelChar

procedure CollapseSpaces( var s : string );
const
  SPCARRAY : array[1..2] of string = ( #32#32, #9#9 );
var
  i, p : integer;
begin
  for i := 1 to 2 do
  begin
    p := pos( SPCARRAY[i], s );
    while ( p > 0 ) do
    begin
      delete( s, p, 1 );
      p := pos( SPCARRAY[i], s );
    end;
  end;

end; // CollapseSpaces

procedure UnquoteString( var s : string );
var
  l : integer;
begin
  l := length( s );
  if ( l > 2 ) then
  begin
    if (( s[1] = '"' ) and ( s[l] = '"' )) then
    begin
      delete( s, l, 1 );
      delete( s, 1, 1 );
    end;
  end;
end; // UnquoteString

function StringsToString(
  const AStrs : TStrings;
  const aDelim : char;
  const WrapWithDelim : boolean ) : string;
var
  i : integer;
  s : string;
begin
  result := '';
  for i := 1 to AStrs.Count do
  begin
    s := AStrs[pred( i )];
    if ( s <> '' ) then
    begin
      if ( i = 1 ) then
        result := s
      else
        result := result + aDelim + s;
    end;
  end;

  if (( result <> '' ) and WrapWithDelim ) then
    result := Format( '%s%s%s', [aDelim, result, aDelim] );

end; // StringsToString


function MatchMask(source, pattern: String): Boolean;
{
  From: stidolph@magnet.com (David Stidolph)
  Subject: [delphi] String Pattern matching
  Date: Tue, 27 Jun 1995 10:01:18 -0400

  There are many times when you need to compare two strings, but want to use
  wild cards in the match - all last names that begin with 'St', etc.  The
  following is a piece of code I got from Sean Stanley in Tallahassee Florida
  in C.  I translated it into Delphi an am uploading it here for all to use.
  I have not tested it extensivly, but the original function has been tested
  quite thoughly.

  I would love feedback on this routine - or peoples changes to it.  I want to
  forward them to Sean to get him to release more tidbits like this.

  This function takes two strings and compares them.  The first string
  can be anything, but should not contain pattern characters (* or ?).
  The pattern string can have as many of these pattern characters as you want.
  For example: MatchStrings('David Stidolph','*St*') would return True.

  Orignal code by Sean Stanley in C
  Rewritten in Delphi by David Stidolph
}
var
  pSource: Array [0..255] of Char;
  pPattern: Array [0..255] of Char;

  function MatchPattern(element, pattern: PChar): Boolean;

    function IsPatternWild(pattern: PChar): Boolean;
    begin
      Result := StrScan(pattern,'*') <> nil;
      if not Result then Result := StrScan(pattern,'?') <> nil;
    end;

  begin
    if 0 = StrComp(pattern,'*') then
      Result := True
    else if (element^ = Chr(0)) and (pattern^ <> Chr(0)) then
      Result := False
    else if element^ = Chr(0) then
      Result := True
    else begin
      case pattern^ of
      '*': if MatchPattern(element,@pattern[1]) then
             Result := True
           else
             Result := MatchPattern(@element[1],pattern);
      '?': Result := MatchPattern(@element[1],@pattern[1]);
      else
        if element^ = pattern^ then
          Result := MatchPattern(@element[1],@pattern[1])
        else
          Result := False;
      end;
    end;
  end;

begin
  StrPCopy(pSource,source);
  StrPCopy(pPattern,pattern);
  Result := MatchPattern(pSource,pPattern);
end; // MatchMask

function GetWordChars : string;
var
  i : integer;
  ch : char;
begin
  result := '';
  for i := 33 to 255 do
  begin
    ch := chr( i );
    if IsCharAlpha( ch ) then
      result := result + ch;
  end;
end; // GetWordChars

function GetIndentOfLine (const S: string): integer;
begin
    // count blanks and tabs in this string
    Result := 0;
    while (Result < length( S )) and
          (S[Result+1] In  [' ',#9])
    do
      Inc( Result );
end;

function TryUTF8ToUnicodeString(const s: RawByteString): string;
begin
    if s = '' then begin
      Result:= '';
      exit;
    end;

    if IsUTF8String(s) then
       Result := UTF8ToUnicodeString(s)
    else
       Result:= s;
end;


function BytesToRawByteString(const bytes: TBytes): RawByteString;
begin
  SetString(Result, PAnsiChar(@bytes[0]), Length(bytes));
end;


function ConvertToUnicodeString(LBuffer: TBytes): string;
var
  LOffset: Integer;
  LEncoding, DestEncoding: TEncoding;
  s: RawByteString;

begin

  LEncoding:= nil;
  LOffset := TEncoding.GetBufferEncoding(LBuffer, LEncoding);  // Get data encoding of read data.

  if LOffset = 0 then begin
    s:= BytesToRawByteString(LBuffer);
    if IsUTF8String(s) then begin
       Result := UTF8ToUnicodeString(s);
       exit;
    end;
  end;

  DestEncoding := TEncoding.Unicode;
  LBuffer := LEncoding.Convert(LEncoding, DestEncoding, LBuffer, LOffset, Length(LBuffer) - LOffset);
  LOffset := TEncoding.GetBufferEncoding(LBuffer, DestEncoding);
  Result := DestEncoding.GetString(LBuffer, LOffset, Length(LBuffer) - LOffset);
end;


function CanSaveAsANSI(const cad: string): boolean;
var
   ch: Char;
begin
  for ch in cad do
    if Ord (ch) >= 256 then begin
       Result:= false;
       exit;
     end;

   Result:= true;
end;


function FirstLineFromString( const str: string; const MaxLen : integer ) : string;
var
  i, l, max, lineBreakAt : integer;
begin
  result:= str;
  l := length( str );
  if ( l > 0 ) then
  begin
    if ( MaxLen < l ) then
      max := MaxLen
    else
      max := l;

    lineBreakAt:= max+1;
    for i := 1 to max do
    begin
      if ( result[i] < #32 ) then
      begin
        lineBreakAt:= i;
        break;
      end;
    end;

    delete( result, lineBreakAt, l );
    if (lineBreakAt > max) and (l > max) then begin
       result:= trimright(result) + '...';
    end;
  end;
end; // FirstLineFromString


{ Returns the position of the nth occurrence of the character in the string, counting from the end of the string }

function NFromLastCharPos(const S: string; const Chr: char; nthOccurrence: integer= 1): integer;
var
  i, n: Integer;
begin
  n:= 0;
  result := 0;
  for i := length(S) downto 1 do
    if S[i] = Chr then begin
       inc(n);
       if n= nthOccurrence then
          Exit(i);
    end;
end;

{
 ASCII Table (https://www.ascii-code.com/   https://www.rapidtables.com/code/text/ascii-table.html

 Ex: 225 	&#225;  	á

 Ex.
 https://www.microsoft.com/es-es/windows/windows-11
 "Descubre Windows 11: la versi&#243;n m&#225;s reciente de Windows | Microsoft"
-> 
 "Descubre Windows 11: la versión más reciente de Windows | Microsoft"

}

function ConvertHTMLAsciiCharacters(const S: string): string;
var
  p1,p2: integer;
  ch: char;
begin
  Result:= S;
  if S='' then exit;

  try
    p1:= Pos('&#', S);
    if p1 > 0 then begin
       p2:= 0;
       repeat
          p2:= Pos(';', Result, p1+1);
          if p2 > 0 then begin
            ch:= Char(StrToint(Copy(Result, p1+2, p2-p1-2)));
            delete(Result, p1, p2-p1+1);
            insert(ch, Result, p1);

            p1:= Pos('&#', Result, p1+1);
          end;
       until (p1 = 0) or (p2 = 0);
    end;

    Result:= StringReplace(Result, '&quot;','"', [rfReplaceAll]);


  except
    Result:= S;
  end;

end;


function GetWordCount( const t : string ) : longint;
var
  i, len : longint;
begin
  len := length( t );
  result := 0;
  if (len > 0) then begin
    i := 1;
    repeat
      if AnsiChar(t[i]) in WordDelimiters then begin
         inc( i );
         continue;
      end
      else
        inc( result );

      // scan to end of word
      while (i <= len) and (not (AnsiChar(t[i]) in WordDelimiters)) do
        inc( i );
    until ( i > len );
  end;
end; // GetWordCount


function HasNonAlphaNumericOrWordDelimiter(const s : string) : boolean;
var
  i: integer;
begin
  for i := 1 to length(s) do
    if not ( (AnsiChar(s[i]) in WordDelimiters) or IsCharAlphaNumeric(s[i]) or (s[i]='''')) then        // ': don't ...  Normal in english words
        Exit(true);

  Result:= False;
end;

function NumberOfLineFeed(Str: string): integer;
var
  i: integer;
begin
   Result:= 0;
   i:= 1;
   while i <= Length(Str) do begin
      if (Str[i] = #10) then
         Inc(Result);
      Inc(i);
   end;
end;


end.
