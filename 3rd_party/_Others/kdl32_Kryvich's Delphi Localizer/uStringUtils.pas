{ String manipulation routines.
  Copyright (C) 2006, 2007, 2008 Aleg Azarousky.
}

{$include version.inc}

unit uStringUtils;

interface

uses Windows;

const
  ListDivider = #13; // Used in DelphiToStringEx
  defHumanizeDivider = '\'; // Used in StringToHumanized
  UnicodeLabel = #$EF#$BB#$BF;

// Split a string by a given divider
// Return in El - left part, in s - rest of a string
procedure SplitBy(var s: string; const divider: string; var el: string);
procedure WideSplitBy(var s: WideString; const divider: WideString; var el: WideString);

// ANSI string <--> Wide string consider Code page of ANSI string
procedure AnsiToWideString(const s: AnsiString; var ws: WideString; AnsiCodepage: LongWord);
procedure WideToAnsiString(const ws: WideString; var s: AnsiString; AnsiCodepage: LongWord);

// Escaped string to string (based on the JEDI Code Library (JCL))
function StrEscapedToString(const S: WideString): WideString;
// Encode string to lng file string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
function StringToLng(const s: WideString; Humanize: boolean; const DividerCR,
  DividerCRLF: WideString): WideString;
// Decode lng file string to string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DefaultLineBreak - used when DividerCR = DividerCRLF
function LngToString(const s: WideString; Humanize: boolean; const DividerCR,
  DividerCRLF, DefaultLineBreak: WideString): WideString;

implementation

uses
  SysUtils, {$ifdef D7} uLegacyCode {$else} WideStrUtils {$endif};

{$ifndef D7}{$region 'Delphi string conversions'}{$endif}

procedure SplitBy(var s: string; const divider: string; var el: string);
var
  i: integer;
begin
  i := pos(divider, s);
  if i <= 0 then begin
    el := s;
    s := '';
  end else begin
    el := copy(s, 1, i-1);
    delete(s, 1, i+length(divider)-1);
  end;
end;

procedure WideSplitBy(var s: WideString; const divider: WideString; var el: WideString);
var
  i: integer;
begin
  i := pos(divider, s);
  if i <= 0 then begin
    el := s;
    s := '';
  end else begin
    el := copy(s, 1, i-1);
    delete(s, 1, i+length(divider)-1);
  end;
end;

procedure AnsiToWideString(const s: AnsiString; var ws: WideString; AnsiCodepage: LongWord);
begin
{$ifdef D6}
  My_WStrFromStr(s, ws, AnsiCodepage);
{$else}
  SetMultiByteConversionCodePage(AnsiCodepage);
  ws := s;
  SetMultiByteConversionCodePage(CP_THREAD_ACP);
{$endif}
end;

procedure WideToAnsiString(const ws: WideString; var s: AnsiString; AnsiCodepage: LongWord);
begin
  SetMultiByteConversionCodePage(AnsiCodepage);
  s := ws;
  SetMultiByteConversionCodePage(CP_THREAD_ACP);
end;

// Encode string to delphi style string
function StringToDelphi(const s: WideString): WideString;
var
  i: integer;
  StrOpened: boolean;
  res: WideString;
  ch: WideChar;

  procedure SwitchStr;  // '...'
  begin
    StrOpened := not StrOpened;
    res := res + '''';
  end;

begin
  StrOpened := false;
  if s = ''
  then res := ''''''
  else begin
    for i := 1 to length(s) do begin
      ch := s[i];
      case ch of
        '''': begin
          if StrOpened
          then res := res + ''''''
          else begin
            res := res + '''''''';
            StrOpened := true;
          end;
        end;
        #0..#31: begin
          if StrOpened then SwitchStr;
          res := res + '#' + IntToStr(ord(ch));
        end;
        else begin
          if not StrOpened then SwitchStr;
          res := res + ch;
        end;
      end;
    end;
  end;
  if StrOpened then SwitchStr;
  result := res;
end;

type
  EDelphiToStringError = Class(Exception)
    iBadChar: integer; // Bad character position
  end;

// Decode delphi style string to string
function DelphiToString(const s: WideString): WideString;
label
  Err;
var
  i, iOpened: integer;
  res: WideString;
  StrOpened, CodeOpened: boolean;
  ch: WideChar;

  procedure OpenStr;  // '...
  begin
    StrOpened := true;
    iOpened := i;
  end;

  procedure OpenCode; // #13
  begin
    CodeOpened := true;
    iOpened := i;
  end;

  function CloseCode: boolean;
  begin
    try
      res := res + WideChar(StrToInt(copy(s, iOpened+1, i-iOpened-1)));
      result := true;
      CodeOpened := false;
    except
      result := false;
    end;
  end;

var
  Ex: EDelphiToStringError;
begin
  res := '';
  StrOpened := false;
  CodeOpened := false;
  // 'Method ''%s'' not supported by automation object'
  // 'Exception %s in module %s at %p.'#13#10'%s%s'#13#10
  // '''hallo'     --     'hallo'''
  // 'hal'''#13#10'lo'     --       'hallo''hallo'
  for i := 1 to length(s) do begin
    ch := s[i];
    if StrOpened then begin
      // Str opened, code closed
      if ch = ''''
      then StrOpened := false
      else res := res + ch;
    end else begin
      if CodeOpened then begin
        // Str closed, code opened
        case ch of
          '''': begin if not CloseCode then goto Err; OpenStr; end;
          '#': begin if not CloseCode then goto Err; OpenCode; end;
          '0'..'9':;
          else goto Err;
        end;
      end else begin
        // Str closed, code closed
        case ch of
          '''': begin
            if (i > 1) and (s[i-1] = '''')
            then res := res + '''';
            OpenStr;
          end;
          '#': OpenCode;
          else begin
            result := res;
            Ex := EDelphiToStringError.Create('Bad decoded string: "' + s + '"');
            Ex.iBadChar := i;
            raise Ex;
          end;
        end;
      end;
    end;
  end;
  if StrOpened then begin
    Err:
    raise Exception.Create('Bad decoded string: "' + s + '"');
  end;
  if CodeOpened then CloseCode;
  result := res;
end;

// Decode delphi style string and stringlist to string
// Stringlist elements delimited by #13
function DelphiToStringEx(s: WideString): WideString;
var
  res, s1: WideString;

  procedure AddResS1;
  begin
    if res <> '' then
      res := res + ListDivider;
    res := res + S1;
  end;

var
  Ok: boolean;
begin
  res := '';
  repeat
    Ok := true;
    try
      s1 := DelphiToString(s);
    except
      on E: EDelphiToStringError do begin
        AddResS1;
        s := Trim(copy(s, E.iBadChar+1, MaxInt));
        Ok := false;
      end;
    end;
  until Ok;
  AddResS1;
  result := res;
end;

{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'Escaped string conversions'}{$endif}

function StrEscapedToString(const S: WideString): WideString;
var
  I, Len, N, Val: Integer;

  procedure HandleHexEscapeSeq;
  const
    HexDigits = WideString('0123456789abcdefABCDEF');
  begin
    N := Pos(S[I + 1], HexDigits) - 1;
    if N < 0 then
      // '\x' without hex digit following is not escape sequence
      Result := Result + '\x'
    else begin
      Inc(I); // Jump over x
      if N >= 16 then
        N := N - 6;
      Val := N;
      // Same for second digit
      if I < Len then begin
        N := Pos(S[I + 1], HexDigits) - 1;
        if N >= 0 then begin
          Inc(I); // Jump over first digit
          if N >= 16 then
            N := N - 6;
          Val := Val * 16 + N;
        end;
      end;

      if val > 255 then
        raise Exception.Create('Numeric constant too large');

      Result := Result + WideChar(Val);
    end;
  end;

  procedure HandleOctEscapeSeq;
  const
    OctDigits = WideString('01234567');
  begin
    // first digit
    Val := Pos(S[I], OctDigits) - 1;
    if I < Len then begin
      N := Pos(S[I + 1], OctDigits) - 1;
      if N >= 0 then begin
        Inc(I);
        Val := Val * 8 + N;
      end;
      if I < Len then begin
        N := Pos(S[I + 1], OctDigits) - 1;
        if N >= 0 then begin
          Inc(I);
          Val := Val * 8 + N;
        end;
      end;
    end;

    if val > 255 then
      raise Exception.Create('Numeric constant too large');

    Result := Result + WideChar(Val);
  end;

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
          Result := Result + #7;
        'b':
          Result := Result + #8;
        'f':
          Result := Result + #12;
        'n':
          Result := Result + #10;
        'r':
          Result := Result + #13;
        't':
          Result := Result + #9;
        'v':
          Result := Result + #11;
        '\', '"', '''', '?':
          Result := Result + S[I];
        'x':
          if I < Len then // Start of hex escape sequence
            HandleHexEscapeSeq
          else // '\x' at end of string is not escape sequence
            Result := Result + '\x';
        '0'..'7': // start of octal escape sequence
          HandleOctEscapeSeq;
      else // no escape sequence
        Result := Result + '\' + S[I];
      end;
    end;
    Inc(I);
  end;
end;

{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'Humanized string conversions'}{$endif}

// Encode string to "humanized" style string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
function StringToHumanized(const s: WideString; const DividerCR,
  DividerCRLF: WideString): WideString;
begin
  if (pos(DividerCR, s) > 0) or (pos(DividerCRLF, s) > 0) then
    raise Exception.CreateFmt(
      'String "%s" contains a humanize divider "%s" or "%s" and can''t be converted properly.'#13#10 +
      'Try set a different string as the divider for this application.',
      [s, DividerCR, DividerCRLF]);
  result := WideStringReplace(
    WideStringReplace(s, sLineBreak, DividerCRLF, [rfReplaceAll]),
    #13, DividerCR, [rfReplaceAll]);
end;

// Decode "humanized" style string to string
// DividerCR - substitution for #13
// DividerCRLF - substitution for #13#10
// DefaultLineBreak - used when DividerCR = DividerCRLF
function HumanizedToString(const s: WideString; const DividerCR, DividerCRLF,
  DefaultLineBreak: WideString): WideString;
begin
  if DividerCR = DividerCRLF then
    result := WideStringReplace(s, DividerCR, DefaultLineBreak, [rfReplaceAll])
  else begin
    result := WideStringReplace(s, DividerCR, #13, [rfReplaceAll]);
    result := WideStringReplace(result, DividerCRLF, sLineBreak, [rfReplaceAll]);
  end;
end;

{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'Lng file string conversions'}{$endif}

function StringToLng(const s: WideString; Humanize: boolean; const DividerCR,
  DividerCRLF: WideString): WideString;
begin
  if Humanize then
    result := StringToHumanized(s, DividerCR, DividerCRLF)
  else
    result := StringToDelphi(s);
end;

function LngToString(const s: WideString; Humanize: boolean; const DividerCR,
  DividerCRLF, DefaultLineBreak: WideString): WideString;
begin
  if Humanize then
    result := HumanizedToString(s, DividerCR, DividerCRLF, DefaultLineBreak)
  else
    result := DelphiToStringEx(s);
end;

{$ifndef D7}{$endregion}{$endif}

end.
