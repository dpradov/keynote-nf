unit gf_Lang;
{$I gf_base.inc}


(****** LICENSE INFORMATION **************************************************

  Partly based on "langs.pas" by Alexander Obukhov <alex@niiomr.belpak.minsk.by>
  Removed bitmap support (AV when compiled with D5, which I have no time to fix).
  Removed design-time property editor (incompatible with D6).

  The original was freeware, and so is this derivation.

  ----------------------------------------
  modified by : Marek Jedlinski <marekjed@users.sourceforge.net>
  date:       : 29-08-2002

  modified by : Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]
  ----------------------------------------

 ======================= 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

 
interface
uses
   Winapi.Windows,
   System.SysUtils,
   Vcl.Graphics
   ;

type
  TgfLanguage     = 0..$FFFF;
  TgfLanguageType = ( ltInstalled, ltSupported, ltPrimary );
  TgfLanguageKind = ( lkLocalized, lkEnglish, lkNative, lkAbbrev );


const
  _LanguageTypes : array[TgfLanguageType] of string = (
    'Installed', 'Supported', 'Primary'
  );
  _LanguageKinds : array[TgfLanguageKind] of string = (
    'Localized', 'English', 'Native', 'Abbreviated'
  );

const
  LANG_URDU                         = $20;   { Primary language ID -> Urdu language}


function SysLanguageName( const Language : TgfLanguage; const LangKind : TgfLanguageKind {$IFDEF DELPHI_5_UP}= lkLocalized{$ENDIF} ) : String;
function CharSetFromLocale(Language: TgfLanguage): TFontCharSet;
function CodePageFromLocale(Language: TgfLanguage): Integer;
function OEMCodePageFromLocale(Language: TgfLanguage): Integer;
function LanguageToIdent(Language: Longint; var Ident: string): Boolean;
function IsRightToLeftLanguage: Boolean;   // [dpv]


implementation


function SysLanguageName( const Language : TgfLanguage; const LangKind : TgfLanguageKind {$IFDEF DELPHI_5_UP}= lkLocalized{$ENDIF} ) : String;
var
  Buf: array[0..255] of Char;
  LCKind : integer;
begin
  case LangKind of
    lkLocalized : LCKind := LOCALE_SLanguage;
    lkEnglish : LCKind := LOCALE_SEngLanguage;
    lkNative : LCKind := LOCALE_SNativeLangName;
    lkAbbrev : LCKind := LOCALE_SAbbrevLangName;
    else
      LCKind := LOCALE_SLanguage;
  end;
  GetLocaleInfo(Language, LCKind, Buf, 255);
  Result:= StrPas(Buf);
end;

function CodePageFromLocale(Language: TgfLanguage): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result:= StrToIntDef(Buf, GetACP);
end;

function OEMCodePageFromLocale(Language: TgfLanguage): Integer;
var
  Buf: array[0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultCodePage, Buf, 6);
  Result:= StrToIntDef(Buf, GetOEMCP);
end;

function CharSetFromLocale(Language: TgfLanguage): TFontCharSet;
var
  CP: Cardinal;
  CHSetInfo : TCHARSETINFO;
begin
  CP:= CodePageFromLocale(Language);
{ The proper solution is to use TranslateCharsetInfo. This function
  is described to be exported from user32.dll, but this works
  only in Windows NT. In Windows 95 this function is absent. So...
}
  result := Ansi_CharSet; // default

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

function LanguageToIdent(Language: Longint; var Ident: string): Boolean;
var
  Buf: array[0..255]of Char;
begin
  Result:= IsValidLocale(Language, LCID_INSTALLED);
  if Result then
    begin
      GetLocaleInfo(Language, LOCALE_SLANGUAGE, Buf, 255);
      SetString(Ident, Buf, StrLen(Buf));
    end;
end;


(*

from MSDN:

Code-Page Identifiers
Identifier Name
037 IBM EBCDIC - U.S./Canada
437 OEM - United States
500 IBM EBCDIC - International
708 Arabic - ASMO 708
709 Arabic - ASMO 449+, BCON V4
710 Arabic - Transparent Arabic
720 Arabic - Transparent ASMO
737 OEM - Greek (formerly 437G)
775 OEM - Baltic
850 OEM - Multilingual Latin I
852 OEM - Latin II
855 OEM - Cyrillic (primarily Russian)
857 OEM - Turkish
858 OEM - Multlingual Latin I + Euro symbol
860 OEM - Portuguese
861 OEM - Icelandic
862 OEM - Hebrew
863 OEM - Canadian-French
864 OEM - Arabic
865 OEM - Nordic
866 OEM - Russian
869 OEM - Modern Greek 
870 IBM EBCDIC - Multilingual/ROECE (Latin-2)
874 ANSI/OEM - Thai (same as 28605, ISO 8859-15) 
875 IBM EBCDIC - Modern Greek 
932 ANSI/OEM - Japanese, Shift-JIS 
936 ANSI/OEM - Simplified Chinese (PRC, Singapore) 
949 ANSI/OEM - Korean (Unified Hangeul Code) 
950 ANSI/OEM - Traditional Chinese (Taiwan; Hong Kong SAR, PRC)  
1026 IBM EBCDIC - Turkish (Latin-5)
1047 IBM EBCDIC - Latin 1/Open System
1140 IBM EBCDIC - U.S./Canada (037 + Euro symbol)
1141 IBM EBCDIC - Germany (20273 + Euro symbol)
1142 IBM EBCDIC - Denmark/Norway (20277 + Euro symbol)
1143 IBM EBCDIC - Finland/Sweden (20278 + Euro symbol)
1144 IBM EBCDIC - Italy (20280 + Euro symbol)
1145 IBM EBCDIC - Latin America/Spain (20284 + Euro symbol)
1146 IBM EBCDIC - United Kingdom (20285 + Euro symbol)
1147 IBM EBCDIC - France (20297 + Euro symbol)
1148 IBM EBCDIC - International (500 + Euro symbol)
1149 IBM EBCDIC - Icelandic (20871 + Euro symbol)
1200 Unicode UCS-2 Little-Endian (BMP of ISO 10646)
1201 Unicode UCS-2 Big-Endian
1250 ANSI - Central European
1251 ANSI - Cyrillic
1252 ANSI - Latin I
1253 ANSI - Greek
1254 ANSI - Turkish
1255 ANSI - Hebrew
1256 ANSI - Arabic
1257 ANSI - Baltic
1258 ANSI/OEM - Vietnamese
1361 Korean (Johab)
10000 MAC - Roman
10001 MAC - Japanese
10002 MAC - Traditional Chinese (Big5)
10003 MAC - Korean
10004 MAC - Arabic
10005 MAC - Hebrew
10006 MAC - Greek I
10007 MAC - Cyrillic
10008 MAC - Simplified Chinese (GB 2312)
10010 MAC - Romania
10017 MAC - Ukraine
10021 MAC - Thai
10029 MAC - Latin II
10079 MAC - Icelandic
10081 MAC - Turkish
10082 MAC - Croatia
12000 Unicode UCS-4 Little-Endian 
12001 Unicode UCS-4 Big-Endian 
20000 CNS - Taiwan  
20001 TCA - Taiwan  
20002 Eten - Taiwan  
20003 IBM5550 - Taiwan  
20004 TeleText - Taiwan  
20005 Wang - Taiwan  
20105 IA5 IRV International Alphabet No. 5 (7-bit) 
20106 IA5 German (7-bit) 
20107 IA5 Swedish (7-bit)
20108 IA5 Norwegian (7-bit) 
20127 US-ASCII (7-bit) 
20261 T.61 
20269 ISO 6937 Non-Spacing Accent 
20273 IBM EBCDIC - Germany 
20277 IBM EBCDIC - Denmark/Norway 
20278 IBM EBCDIC - Finland/Sweden 
20280 IBM EBCDIC - Italy 
20284 IBM EBCDIC - Latin America/Spain 
20285 IBM EBCDIC - United Kingdom 
20290 IBM EBCDIC - Japanese Katakana Extended 
20297 IBM EBCDIC - France 
20420 IBM EBCDIC - Arabic
20423 IBM EBCDIC - Greek
20424 IBM EBCDIC - Hebrew 
20833 IBM EBCDIC - Korean Extended 
20838 IBM EBCDIC - Thai 
20866 Russian - KOI8-R 
20871 IBM EBCDIC - Icelandic 
20880 IBM EBCDIC - Cyrillic (Russian) 
20905 IBM EBCDIC - Turkish 
20924 IBM EBCDIC - Latin-1/Open System (1047 + Euro symbol) 
20932 JIS X 0208-1990 & 0121-1990 
20936 Simplified Chinese (GB2312)
21025 IBM EBCDIC - Cyrillic (Serbian, Bulgarian)
21027 Extended Alpha Lowercase 
21866 Ukrainian (KOI8-U) 
28591 ISO 8859-1 Latin I 
28592 ISO 8859-2 Central Europe 
28593 ISO 8859-3 Latin 3  
28594 ISO 8859-4 Baltic
28595 ISO 8859-5 Cyrillic
28596 ISO 8859-6 Arabic 
28597 ISO 8859-7 Greek 
28598 ISO 8859-8 Hebrew 
28599 ISO 8859-9 Latin 5 
28605 ISO 8859-15 Latin 9 
29001 Europa 3 
38598 ISO 8859-8 Hebrew 
50220 ISO 2022 Japanese with no halfwidth Katakana 
50221 ISO 2022 Japanese with halfwidth Katakana 
50222 ISO 2022 Japanese JIS X 0201-1989 
50225 ISO 2022 Korean  
50227 ISO 2022 Simplified Chinese 
50229 ISO 2022 Traditional Chinese 
50930 Japanese (Katakana) Extended 
50931 US/Canada and Japanese 
50933 Korean Extended and Korean
50935 Simplified Chinese Extended and Simplified Chinese 
50936 Simplified Chinese
50937 US/Canada and Traditional Chinese 
50939 Japanese (Latin) Extended and Japanese 
51932 EUC - Japanese 
51936 EUC - Simplified Chinese 
51949 EUC - Korean 
51950 EUC - Traditional Chinese 
52936 HZ-GB2312 Simplified Chinese
54936 Windows XP: GB18030 Simplified Chinese (4 Byte)  
57002 ISCII Devanagari 
57003 ISCII Bengali 
57004 ISCII Tamil 
57005 ISCII Telugu 
57006 ISCII Assamese 
57007 ISCII Oriya 
57008 ISCII Kannada
57009 ISCII Malayalam
57010 ISCII Gujarati
57011 ISCII Punjabi
65000 Unicode UTF-7
65001 Unicode UTF-8

*)

function IsRightToLeftLanguage: Boolean;
var
  LangID: Word;
  PrimaryLangID: Word;
begin
  LangID := GetUserDefaultUILanguage;      // default user language (can be GetSystemDefaultUILanguage for system global)
  PrimaryLangID := LangID and $3FF;

  case PrimaryLangID of
    LANG_ARABIC,     // Arabic
    LANG_HEBREW,     // Hebrew
    LANG_FARSI,      // Persian
    LANG_URDU:       // Urdu
      Result := True;
  else
    Result := False;
  end;
end;


end.

