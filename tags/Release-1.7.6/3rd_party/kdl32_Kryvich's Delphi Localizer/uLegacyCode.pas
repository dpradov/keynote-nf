{ Functions for legacy Delphi versions.
  Copyright (C) 2006, 2007 Kryvich, Belarusian Linguistic Software team.
}

{$include version.inc}

unit uLegacyCode;

interface

uses Windows, Classes, SysUtils;

{$ifdef D5}
const
  sLineBreak = #13#10;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
function AnsiEndsText(const ASubText, AText: string): Boolean;
{$endif}

{$ifdef D6}
const
  CP_THREAD_ACP = 3;  // current thread's ANSI code page

procedure My_WStrFromStr(const Source: string; var Dest: WideString;
  CodePage: LongWord);
function TStringsGetValueFromIndex(Strings: TStrings; Index: Integer): string;
{$endif}

{$ifdef D7}
// Set a codepage for Wide <--> ANSI convertion operations
procedure SetMultiByteConversionCodePage(cp: UINT);
// Widestring replacement for StringReplace
function WideStringReplace(const S, OldPattern, NewPattern: Widestring;
  Flags: TReplaceFlags): Widestring;
{$endif}

implementation

{$ifdef D5}
function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PWideChar(S), Len);
  if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function AnsiEndsText(const ASubText, AText: string): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') and
     (ByteType(AText, SubTextLocation) <> mbTrailByte) then
    Result := AnsiStrIComp(Pointer(ASubText), Pointer(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;
{$endif}

{$ifdef D6}
procedure My_WStrFromStr(const Source: string; var Dest: WideString;
  CodePage: LongWord);
var
  SourLen, DestLen: Integer;
begin 
  SourLen := length(Source);
  if SourLen <= 0 then begin
    Dest := '';
    Exit;
  end;
  DestLen := MultiByteToWideChar(CodePage, 0, @Source[1], SourLen, Nil, 0);
  SetLength(Dest, DestLen);
  MultiByteToWideChar(CodePage, 0, @Source[1], SourLen, @Dest[1], DestLen);
end;

function TStringsGetValueFromIndex(Strings: TStrings; Index: Integer): string;
var
  s: string;
  i: integer;
begin
  if Index >= 0 then begin
    s := Strings[Index];
    i := pos('=', s);
    if i <= 0 then
      i := MaxInt;
    Result := Copy(s, i+1, MaxInt)
  end else
    Result := '';
end;
{$endif}

{$ifdef D7}
{$ifdef D6}
procedure SetMultiByteConversionCodePage(cp: UINT);
begin
  // Do nothing...
end;
{$else}
// Set a codepage for Wide <--> ANSI convertion operations
procedure SetMultiByteConversionCodePage(cp: UINT);
var
  DefUserCP: ^integer;
begin
  DefUserCP := pointer(integer(@ModuleUnloadList) + $2588);
  DefUserCP^ := cp;
end;
{$endif}

// Widestring replacement for StringReplace
function WideStringReplace(const S, OldPattern, NewPattern: Widestring;
  Flags: TReplaceFlags): Widestring;
var
  SearchStr, Patt, NewStr: Widestring;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then begin
    SearchStr := WideUpperCase(S);
    Patt := WideUpperCase(OldPattern);
  end else begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do begin
    Offset := pos(Patt, SearchStr);
    if Offset = 0 then begin
      Result := Result + NewStr;
      break;
    end;
    Result := Result + copy(NewStr, 1, Offset - 1) + NewPattern;
    delete(NewStr, 1, Offset+length(OldPattern)-1);
    if not (rfReplaceAll in Flags) then begin
      Result := Result+NewStr;
      break;
    end;
    delete(SearchStr, 1, Offset+length(Patt)-1);
  end;
end;
{$endif}

end.
