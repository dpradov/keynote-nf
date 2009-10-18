unit LangsRX;

interface

uses
  Windows, SysUtils, Graphics, Classes;

type
  TLanguage = 0..$FFFF;

  TLangOption = (loLocalized, loEnglish, loNative, loAbbrev);

function LanguageName(Language: TLanguage): String;
function CharSetFromLocale(Language: TLanguage): TFontCharSet;
function CodePageFromLocale(Language: TLanguage): Integer;
function OEMCodePageFromLocale(Language: TLanguage): Integer;
function CharToWide(const S: String; CodePage: Word): WideString;
function WideToChar(const WS: WideString; CodePage: Word): String;
function CharToChar(const S: String; CP1, CP2: Word): String;

implementation

function LanguageName(Language: TLanguage): String;
var
  Buf: array[0..255] of Char;
begin
  GetLocaleInfo(Language, LOCALE_SLanguage, Buf, 255);
  Result:= StrPas(Buf);
end;

function CodePageFromLocale(Language: TLanguage): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result:= StrToIntDef(Buf, GetACP);
end;

function OEMCodePageFromLocale(Language: TLanguage): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultCodePage, Buf, 6);
  Result:= StrToIntDef(Buf, GetOEMCP);
end;

function CharSetFromLocale(Language: TLanguage): TFontCharSet;
var
  CP: Integer;
begin
  CP:= CodePageFromLocale(Language);
{ The proper solution is to use TranslateCharsetInfo. This function
  is described to be exported from user32.dll, but this works
  only in Windows NT. In Windows 95 this function is absent. So...
}
  case CP of
  1250:
    Result:= EastEurope_CharSet;
  1251:
    Result:= Russian_CharSet;
  1252:
    Result:= Ansi_CharSet;
  1253:
    Result:= Greek_CharSet;
  1254:
    Result:= Turkish_CharSet;
  1255:
    Result:= Hebrew_CharSet;
  1256:
    Result:= Arabic_CharSet;
  1257:
    Result:= Baltic_CharSet;
  1258:
    Result:= Vietnamese_CharSet;
  end;
end;

function CharToWide(const S: String; CodePage: Word): WideString;
var
  L: Integer;
begin
  L:= MultiByteToWideChar(CodePage, 0, PChar(@S[1]), -1, nil, 0);
  SetLength(Result, L-1);
  MultiByteToWideChar(CodePage, 0, PChar(@S[1]), -1, PWideChar(@Result[1]), L-1);
end;

function WideToChar(const WS: WideString; CodePage: Word): String;
var
  L: Integer;
begin
  L:= WideCharToMultiByte(CodePage, 0, @WS[1], -1, nil, 0, nil, nil);
  SetLength(Result, L-1);
  WideCharToMultiByte(CodePage, 0, @WS[1], -1, @Result[1], L-1, nil, nil);
end;

function CharToChar(const S: String; CP1, CP2: Word): String;
begin
  Result:= WideToChar(CharToWide(S, CP1), CP2);
end;

end.
