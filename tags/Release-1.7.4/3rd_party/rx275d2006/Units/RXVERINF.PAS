{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{                                                       }
{*******************************************************}

unit RxVerInf;

{ Working with VERSIONINFO resourse type }

{$I RX.INC}

{$IFDEF RX_D3}
  {$WEAKPACKAGEUNIT}
{$ENDIF}

interface

uses Windows;

type
  TVersionLanguage = (vlArabic, vlBulgarian, vlCatalan, vlTraditionalChinese,
    vlCzech, vlDanish, vlGerman, vlGreek, vlUSEnglish, vlCastilianSpanish,
    vlFinnish, vlFrench, vlHebrew, vlHungarian, vlIcelandic, vlItalian,
    vlJapanese, vlKorean, vlDutch, vlNorwegianBokmel, vlPolish,
    vlBrazilianPortuguese, vlRhaetoRomanic, vlRomanian, vlRussian,
    vlCroatoSerbian, vlSlovak, vlAlbanian, vlSwedish, vlThai, vlTurkish,
    vlUrdu, vlBahasa, vlSimplifiedChinese, vlSwissGerman, vlUKEnglish,
    vlMexicanSpanish, vlBelgianFrench, vlSwissItalian, vlBelgianDutch,
    vlNorwegianNynorsk, vlPortuguese, vlSerboCroatian, vlCanadianFrench,
    vlSwissFrench, vlUnknown);

  TVersionCharSet = (vcsASCII, vcsJapan, vcsKorea, vcsTaiwan, vcsUnicode,
    vcsEasternEuropean, vcsCyrillic, vcsMultilingual, vcsGreek, vcsTurkish,
    vcsHebrew, vcsArabic, vcsUnknown);

{$IFNDEF WIN32}
  PVSFixedFileInfo = Pvs_FixedFileInfo;
  DWORD = Longint;
{$ENDIF}

  TLongVersion = record
    case Integer of
    0: (All: array[1..4] of Word);
    1: (MS, LS: LongInt);
  end;

{ TVersionInfo }

  TVersionInfo = class(TObject)
  private
    FFileName: PChar;
    FValid: Boolean;
    FSize: DWORD;
    FBuffer: PChar;
    FHandle: DWORD;
    procedure ReadVersionInfo;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetTranslation: Pointer;
    function GetFixedFileInfo: PVSFixedFileInfo;
    function GetFileLongVersion: TLongVersion;
    function GetProductLongVersion: TLongVersion;
    function GetTranslationString: string;
    function GetComments: string;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileVersion: string;
    function GetVersionNum: Longint;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTrademarks: string;
    function GetOriginalFilename: string;
    function GetProductVersion: string;
    function GetProductName: string;
    function GetSpecialBuild: string;
    function GetPrivateBuild: string;
    function GetVersionLanguage: TVersionLanguage;
    function GetVersionCharSet: TVersionCharSet;
    function GetVerFileDate: TDateTime;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function GetVerValue(const VerName: string): string;
    property FileName: string read GetFileName write SetFileName;
    property Valid: Boolean read FValid;
    property FixedFileInfo: PVSFixedFileInfo read GetFixedFileInfo;
    property FileLongVersion: TLongVersion read GetFileLongVersion;
    property ProductLongVersion: TLongVersion read GetProductLongVersion;
    property Translation: Pointer read GetTranslation;
    property VersionLanguage: TVersionLanguage read GetVersionLanguage;
    property VersionCharSet: TVersionCharSet read GetVersionCharSet;
    property VersionNum: Longint read GetVersionNum;
    property Comments: string read GetComments;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVersion: string read GetFileVersion;
    property InternalName: string read GetInternalName;
    property LegalCopyright: string read GetLegalCopyright;
    property LegalTrademarks: string read GetLegalTrademarks;
    property OriginalFilename: string read GetOriginalFilename;
    property ProductVersion: string read GetProductVersion;
    property ProductName: string read GetProductName;
    property SpecialBuild: string read GetSpecialBuild;
    property PrivateBuild: string read GetPrivateBuild;
    property Values[const Name: string]: string read GetVerValue;
    property VerFileDate: TDateTime read GetVerFileDate;
  end;

function LongVersionToString(const Version: TLongVersion): string;
function StringToLongVersion(const Str: string): TLongVersion;
function AppFileName: string;
function AppVerInfo: TVersionInfo;

{ Installation utility routine }

function OkToWriteModule(ModuleName: string; NewVer: Longint): Boolean;

implementation

{$IFDEF WIN32}
uses SysUtils, FileUtil, DateUtil;
{$ELSE}
uses WinTypes, WinProcs, SysUtils, FileUtil, DateUtil, VclUtils;
{$ENDIF}

function MemAlloc(Size: Longint): Pointer;
{$IFDEF WIN32}
begin
  GetMem(Result, Size);
end;
{$ELSE}
var
  Handle: THandle;
begin
  if Size < 65535 then GetMem(Result, Size)
  else begin
    Handle := GlobalAlloc(HeapAllocFlags, Size);
    Result := GlobalLock(Handle);
  end;
end;
{$ENDIF WIN32}

const
  LanguageValues: array[TVersionLanguage] of Word = ($0401, $0402, $0403,
    $0404, $0405, $0406, $0407, $0408, $0409, $040A, $040B, $040C, $040D,
    $040E, $040F, $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F, $0420, $0421,
    $0804, $0807, $0809, $080A, $080C, $0810, $0813, $0814, $0816, $081A,
    $0C0C, $100C, $0000);

const
  CharacterSetValues: array[TVersionCharSet] of Integer = (0, 932, 949, 950,
    1200, 1250, 1251, 1252, 1253, 1254, 1255, 1256, -1);

{ TVersionInfo }

constructor TVersionInfo.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := StrPCopy(StrAlloc(Length(AFileName) + 1), AFileName);
  ReadVersionInfo;
end;

destructor TVersionInfo.Destroy;
begin
  if FBuffer <> nil then FreeMem(FBuffer, FSize);
  StrDispose(FFileName);
  inherited Destroy;
end;

procedure TVersionInfo.ReadVersionInfo;
begin
  FValid := False;
  FSize := GetFileVersionInfoSize(FFileName, FHandle);
  if FSize > 0 then
    try
      FBuffer := MemAlloc(FSize);
      FValid := GetFileVersionInfo(FFileName, FHandle, FSize, FBuffer);
    except
      FValid := False;
      raise;
    end;
end;

function TVersionInfo.GetFileName: string;
begin
  Result := StrPas(FFileName);
end;

procedure TVersionInfo.SetFileName(const Value: string);
begin
  if FBuffer <> nil then FreeMem(FBuffer, FSize);
  FBuffer := nil;
  StrDispose(FFileName);
  FFileName := StrPCopy(StrAlloc(Length(Value) + 1), Value);
  ReadVersionInfo;
end;

function TVersionInfo.GetTranslation: Pointer;
var
{$IFDEF WIN32}
  Len: UINT;
{$ELSE}
  Len: Cardinal;
{$ENDIF}
begin
  Result := nil;
  if Valid then VerQueryValue(FBuffer, '\VarFileInfo\Translation', Result, Len)
  else Result := nil;
end;

function TVersionInfo.GetTranslationString: string;
var
  P: Pointer;
begin
  Result := '';
  P := GetTranslation;
  if P <> nil then
    Result := IntToHex(MakeLong(HiWord(Longint(P^)), LoWord(Longint(P^))), 8);
end;

function TVersionInfo.GetVersionLanguage: TVersionLanguage;
var
  P: Pointer;
begin
  P := GetTranslation;
  for Result := vlArabic to vlUnknown do
    if LoWord(Longint(P^)) = LanguageValues[Result] then Break;
end;

function TVersionInfo.GetVersionCharSet: TVersionCharSet;
var
  P: Pointer;
begin
  P := GetTranslation;
  for Result := vcsASCII to vcsUnknown do
    if HiWord(Longint(P^)) = CharacterSetValues[Result] then Break;
end;

function TVersionInfo.GetFixedFileInfo: PVSFixedFileInfo;
var
{$IFDEF WIN32}
  Len: UINT;
{$ELSE}
  Len: Cardinal;
{$ENDIF}
begin
  Result := nil;
  if Valid then VerQueryValue(FBuffer, '\', Pointer(Result), Len)
  else Result := nil;
end;

function TVersionInfo.GetProductLongVersion: TLongVersion;
begin
  Result.MS := FixedFileInfo^.dwProductVersionMS;
  Result.LS := FixedFileInfo^.dwProductVersionLS;
end;

function TVersionInfo.GetFileLongVersion: TLongVersion;
begin
  Result.MS := FixedFileInfo^.dwFileVersionMS;
  Result.LS := FixedFileInfo^.dwFileVersionLS;
end;

function TVersionInfo.GetVersionNum: Longint;
begin
  if Valid then Result := FixedFileInfo^.dwFileVersionMS
  else Result := 0;
end;

function TVersionInfo.GetVerValue(const VerName: string): string;
var
  szName: array[0..255] of Char;
  Value: Pointer;
{$IFDEF WIN32}
  Len: UINT;
{$ELSE}
  Len: Cardinal;
{$ENDIF}
begin
  Result := '';
  if Valid then begin
    StrPCopy(szName, '\StringFileInfo\' + GetTranslationString + '\' + VerName);
    if VerQueryValue(FBuffer, szName, Value, Len) then
      Result := StrPas(PChar(Value));
  end;
end;

function TVersionInfo.GetComments: string;
begin
  Result := GetVerValue('Comments');
end;

function TVersionInfo.GetCompanyName: string;
begin
  Result := GetVerValue('CompanyName');
end;

function TVersionInfo.GetFileDescription: string;
begin
  Result := GetVerValue('FileDescription');
end;

function TVersionInfo.GetFileVersion: string;
begin
  Result := GetVerValue('FileVersion');
  if (Result = '') and Valid then
    Result := LongVersionToString(FileLongVersion);
end;

function TVersionInfo.GetInternalName: string;
begin
  Result := GetVerValue('InternalName');
end;

function TVersionInfo.GetLegalCopyright: string;
begin
  Result := GetVerValue('LegalCopyright');
end;

function TVersionInfo.GetLegalTrademarks: string;
begin
  Result := GetVerValue('LegalTrademarks');
end;

function TVersionInfo.GetOriginalFilename: string;
begin
  Result := GetVerValue('OriginalFilename');
end;

function TVersionInfo.GetProductVersion: string;
begin
  Result := GetVerValue('ProductVersion');
  if (Result = '') and Valid then
    Result := LongVersionToString(ProductLongVersion);
end;

function TVersionInfo.GetProductName: string;
begin
  Result := GetVerValue('ProductName');
end;

function TVersionInfo.GetSpecialBuild: string;
begin
  Result := GetVerValue('SpecialBuild');
end;

function TVersionInfo.GetPrivateBuild: string;
begin
  Result := GetVerValue('PrivateBuild');
end;

function TVersionInfo.GetVerFileDate: TDateTime;
begin
  if FileExists(FileName) then
    Result := FileDateTime(FileName)
  else Result := NullDate;
end;

{ Long version string routines }

function LongVersionToString(const Version: TLongVersion): string;
begin
  with Version do
    Result := Format('%d.%d.%d.%d', [All[2], All[1], All[4], All[3]]);
end;

function StringToLongVersion(const Str: string): TLongVersion;
var
  Sep: Integer;
  Tmp, Fragment: string;
  I: Word;
begin
  Tmp := Str;
  for I := 1 to 4 do begin
    Sep := Pos('.', Tmp);
    if Sep = 0 then Sep := Pos(',', Tmp);
    if Sep = 0 then Fragment := Tmp
    else begin
      Fragment := Copy(Tmp, 1, Sep - 1);
      Tmp := Copy(Tmp, Sep + 1, MaxInt);
    end;
    if Fragment = '' then Result.All[I] := 0
    else Result.All[I] := StrToInt(Fragment);
  end;
  I := Result.All[1];
  Result.All[1] := Result.All[2];
  Result.All[2] := I;
  I := Result.All[3];
  Result.All[3] := Result.All[4];
  Result.All[4] := I;
end;

function AppFileName: string;
var
  FileName: array[0..255] of Char;
begin
  if IsLibrary then begin
    GetModuleFileName(HInstance, FileName, SizeOf(FileName) - 1);
    Result := StrPas(FileName);
  end
  else Result := ParamStr(0);
end;

function AppVerInfo: TVersionInfo;
begin
  Result := TVersionInfo.Create(AppFileName);
end;

{ Installation utility routines }

function OkToWriteModule(ModuleName: string; NewVer: Longint): Boolean;
{ Return True if it's ok to overwrite ModuleName with NewVer }
begin
  {Assume we should overwrite}
  OkToWriteModule := True;
  with TVersionInfo.Create(ModuleName) do begin
    try
      if Valid then {Should we overwrite?}
        OkToWriteModule := NewVer > VersionNum;
    finally
      Free;
    end;
  end;
end;

end.