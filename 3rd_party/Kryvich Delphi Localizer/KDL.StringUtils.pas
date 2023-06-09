{ String manipulation routines.
  Copyright (C) 2006 - 2018 Aleg Azarouski.
}

unit KDL.StringUtils;
{$I NoRTTI}

interface

const
  ListDivider = #13; // Used in DelphiToStringEx
  defHumanizeDivider = '\'; // Used in StringToHumanized
  defHumanizedLF = '\#10'; 
  UnicodeLabel: array[0..2] of Byte = ($EF, $BB, $BF);

// Split a string by a given divider
// Return in El - left part, in s - rest of a string
procedure SplitBy(var S: string; const Divider: string; var Elem: string);
// Escaped string to string (based on the JEDI Code Library (JCL))
function StrEscapedToString(const S: string): string;
// Encode string to lng file string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DividerLF - substitution for #10
function StringToLng(const S: string; Humanize: Boolean; const DividerCR,
  DividerCRLF, DividerLF: string): string;
// Decode lng file string to string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DividerLF - substitution for #10
// DefaultLineBreak - used when DividerCR = DividerCRLF
function LngToString(const S: string; Humanize: Boolean; const DividerCR,
  DividerCRLF, DividerLF, DefaultLineBreak: string): string;

implementation

uses
  SysUtils, StrUtils;

{$region 'Delphi string conversions'}
procedure SplitBy(var S: string; const Divider: string; var Elem: string);
var
  i: Integer;
begin
  i := Pos(Divider, S);
  if i <= 0 then begin
    Elem := S;
    S := '';
  end else begin
    Elem := Copy(S, 1, i-1);
    Delete(S, 1, i + Length(Divider) - 1);
  end;
end;

// Encode string to delphi style string
function StringToDelphi(const s: string): string;
var
  i: Integer;
  insideStr: Boolean;
  res: string;
  ch: Char;

  procedure SwitchStr;  // '...'
  begin
    insideStr := not insideStr;
    res := res + '''';
  end;

begin
  insideStr := False;
  if s = '' then
    res := ''''''
  else begin
    for i := 1 to Length(s) do begin
      ch := s[i];
      case ch of
        '''': begin
          if insideStr then
            res := res + ''''''
          else begin
            res := res + '''''''';
            insideStr := True;
          end;
        end;
        #0..#31: begin
          if insideStr then SwitchStr;
          res := res + '#' + IntToStr(ord(ch));
        end;
        else begin
          if not insideStr then SwitchStr;
          res := res + ch;
        end;
      end;
    end;
  end;
  if insideStr then SwitchStr;
  Result := res;
end;

type
  EDelphiToStringError = Class(Exception)
  public
    iBadChar: Integer; // Bad character position
  end;

// Decode delphi style string to string
function DelphiToString(const s: string): string;
label
  Err;
var
  i, iOpened: Integer;
  res: string;
  insideStr, insideCode: Boolean;
  ch: Char;

  procedure OpenStr;  // '...
  begin
    insideStr := True;
    iOpened := i;
  end;

  procedure OpenCode; // #13
  begin
    insideCode := True;
    iOpened := i;
  end;

  function CloseCode: Boolean;
  begin
    try
      res := res + Char(StrToInt(Copy(s, iOpened+1, i-iOpened-1)));
      Result := True;
      insideCode := False;
    except
      Result := False;
    end;
  end;

var
  Ex: EDelphiToStringError;
begin
  res := '';
  insideStr := False;
  insideCode := False;
  // 'Method ''%s'' not supported by automation object'
  // 'Exception %s in module %s at %p.'#13#10'%s%s'#13#10
  // '''hallo'     --     'hallo'''
  // 'hal'''#13#10'lo'     --       'hallo''hallo'
  for i := 1 to Length(s) do begin
    ch := s[i];
    if insideStr then begin
      // Str opened, code closed
      if ch = '''' then
        insideStr := False
      else
        res := res + ch;
    end else begin
      if insideCode then begin
        // Str closed, code opened
        case ch of
          '''': begin
            if not CloseCode then
              goto Err;
            OpenStr;
          end;
          '#': begin
            if not CloseCode then
              goto Err;
            OpenCode;
          end;
          '0'..'9':;
          else goto Err;
        end;
      end else begin
        // Str closed, code closed
        case ch of
          '''': begin
            if (i > 1) and (s[i-1] = '''') then
              res := res + '''';
            OpenStr;
          end;
          '#': OpenCode;
          else begin
            Result := res;
            Ex := EDelphiToStringError.Create('Bad decoded string: "' + s + '"');
            Ex.iBadChar := i;
            raise Ex;
          end;
        end;
      end;
    end;
  end;
  if insideStr then begin
    Err:
    raise Exception.Create('Bad decoded string: "' + s + '"');
  end;
  if insideCode then CloseCode;
  Result := res;
end;

// Decode delphi style string and stringlist to string
// Stringlist elements delimited by #13
function DelphiToStringEx(s: string): string;
var
  res, s1: string;

  procedure AddResS1;
  begin
    if res <> '' then
      res := res + ListDivider;
    res := res + S1;
  end;

var
  Ok: Boolean;
begin
  res := '';
  repeat
    Ok := True;
    try
      s1 := DelphiToString(s);
    except
      on E: EDelphiToStringError do begin
        AddResS1;
        s := Trim(Copy(s, E.iBadChar+1, MaxInt));
        Ok := False;
      end;
    end;
  until Ok;
  AddResS1;
  Result := res;
end;
{$endregion}

{$region 'Escaped string conversions'}
function StrEscapedToString(const S: string): string;

  // \x041f --> wide character
  procedure HandleHexEscapeSeq(const S: string; var I: Integer; Len: Integer;
    var Dest: string);
  const
    hexDigits = string('0123456789abcdefABCDEF');
  var
    startI, val, n: Integer;
  begin
    startI := I;
    val := 0;
    while I < StartI + 4 do begin
      n := Pos(S[I+1], hexDigits) - 1;
      if n < 0 then begin
        if startI = I then begin
          // '\x' without hex digit following is not escape sequence
          Dest := Dest + '\x';
          Exit;
        end;
      end else begin
        Inc(I);
        if n >= 16 then
          n := n - 6;
        val := val * 16 + n;
        if val > Ord(High(Char)) then
          raise Exception.CreateFmt(
            'Numeric constant too large (%d) at position %d.', [val, startI]);
      end;
    end;
    Dest := Dest + Char(val);
  end;

  procedure HandleOctEscapeSeq(const S: string; var I: Integer; Len: Integer;
    var Dest: string);
  const
    octDigits = string('01234567');
  var
    startI, val, n: Integer;
  begin
    startI := I;
    // first digit
    val := Pos(S[I], octDigits) - 1;
    if I < Len then
    begin
      n := Pos(S[I + 1], octDigits) - 1;
      if n >= 0 then
      begin
        Inc(I);
        val := val * 8 + n;
      end;
      if I < Len then
      begin
        n := Pos(S[I + 1], octDigits) - 1;
        if n >= 0 then
        begin
          Inc(I);
          val := val * 8 + n;
        end;
      end;
    end;

    if val > Ord(High(Char)) then
      raise Exception.CreateFmt(
        'Numeric constant too large (%d) at position %d.', [val, startI]);

    Dest := Dest + Char(val);
  end;

const
  NativeBell = Char(#7);
  NativeBackspace = Char(#8);
  NativeTab = Char(#9);
  NativeLineFeed = Char(#10);
  NativeVerticalTab = Char(#11);
  NativeFormFeed = Char(#12);
  NativeCarriageReturn = Char(#13);
var
  I, Len: Integer;
begin
  Result := '';
  I := 1;
  Len := Length(S);
  while I <= Len do begin
    if not ((S[I] = '\') and (I < Len)) then
      Result := Result + S[I]
    else begin
      Inc(I); // Jump over escape character
      case S[I] of
        'a':
          Result := Result + NativeBell;
        'b':
          Result := Result + NativeBackspace;
        'f':
          Result := Result + NativeFormFeed;
        'n':
          Result := Result + NativeLineFeed;
        'r':
          Result := Result + NativeCarriageReturn;
        't':
          Result := Result + NativeTab;
        'v':
          Result := Result + NativeVerticalTab;
        '\':
          Result := Result + '\';
        '"':
          Result := Result + '"';
        '''':
          Result := Result + ''''; // Optionally escaped
        '?':
          Result := Result + '?';  // Optionally escaped
        'x':
          if I < Len then
            // Start of hex escape sequence
            HandleHexEscapeSeq(S, I, Len, Result)
          else
            // '\x' at end of string is not escape sequence
            Result := Result + '\x';
        '0'..'7':
          // start of octal escape sequence
          HandleOctEscapeSeq(S, I, Len, Result);
      else
        // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;
{$endregion}

{$region 'Humanized string conversions'}
// Encode string to "humanized" style string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DividerLF - substitution for #10
function StringToHumanized(const s: string; const DividerCR, DividerCRLF,
  DividerLF: string): string;
begin
  if (Pos(DividerCR, s) > 0)
    or (Pos(DividerCRLF, s) > 0)
    or (Pos(DividerLF, s) > 0)
  then
    raise Exception.CreateFmt(
      'String "%s" contains a humanize divider "%s", "%s" or "%s" and can''t'
      + ' be converted properly.'#13#10
      + 'Try set a different string as the divider for this application.',
      [s, DividerCR, DividerCRLF, DividerLF]);
  Result := StringReplace(
    StringReplace(
      StringReplace(s, sLineBreak, DividerCRLF, [rfReplaceAll]),
      #13, DividerCR, [rfReplaceAll]),
    #10, DividerLF, [rfReplaceAll]);
end; 

// Decode "humanized" style string to string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DividerLF - substitution for #10
// DefaultLineBreak - used when DividerCR = DividerCRLF
function HumanizedToString(const s: string; const DividerCR, DividerCRLF,
  DividerLF, DefaultLineBreak: string): string;
begin
  if DividerCR = DividerCRLF then
    Result := StringReplace(s, DividerCR, DefaultLineBreak, [rfReplaceAll])
  else begin
    Result := StringReplace(s, DividerCR, #13, [rfReplaceAll]);
    Result := StringReplace(Result, DividerCRLF, sLineBreak, [rfReplaceAll]);
  end;
  Result := StringReplace(Result, DividerLF, #10, [rfReplaceAll]);
end;
{$endregion}

{$region 'Lng file string conversions'}
function StringToLng(const S: string; Humanize: Boolean; const DividerCR,
  DividerCRLF, DividerLF: string): string;
begin
  if Humanize then
    Result := StringToHumanized(S, DividerCR, DividerCRLF, DividerLF)
  else
    Result := StringToDelphi(S);
end;

function LngToString(const S: string; Humanize: Boolean; const DividerCR,
  DividerCRLF, DividerLF, DefaultLineBreak: string): string;
begin
  if Humanize then
    Result := HumanizedToString(S, DividerCR, DividerCRLF, DividerLF,
      DefaultLineBreak)
  else
    Result := DelphiToStringEx(S);
end;
{$endregion}

end.
