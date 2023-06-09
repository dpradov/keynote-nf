{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxIni;

interface

{$I RX.INC}

uses
  {$IFNDEF VER80}Windows, Registry, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  {$IFDEF RX_D16}System.UITypes, {$ENDIF}
  Classes, IniFiles, Graphics;

type
  TReadObjectEvent = function(Sender: TObject; const Section,
    Item, Value: string): TObject of object;
  TWriteObjectEvent = procedure(Sender: TObject; const Section, Item: string;
    Obj: TObject) of object;

{ TRxIniFile }

  TRxIniFile = class(TIniFile)
  private
    {$IFDEF RX_D4} // Polaris
    FListItemName: string;
    {$ELSE}
    FListItemName: PString;
    {$ENDIF}
    FOnReadObject: TReadObjectEvent;
    FOnWriteObject: TWriteObjectEvent;
    function GetItemName: string;
    procedure SetItemName(const Value: string);
    function ReadListParam(const Section: string; Append: Boolean;
      List: TStrings): TStrings;
  protected
    procedure WriteObject(const Section, Item: string; Index: Integer;
      Obj: TObject); dynamic;
    function ReadObject(const Section, Item, Value: string): TObject; dynamic;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Flush;
    {$IFDEF VER80}
    procedure DeleteKey(const Section, Ident: string);
    {$ENDIF}
    { ini-file read and write methods }
    function ReadClearList(const Section: string; List: TStrings): TStrings;
    function ReadList(const Section: string; List: TStrings): TStrings;
    procedure WriteList(const Section: string; List: TStrings);
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
    procedure WriteColor(const Section, Ident: string; Value: TColor);
    function ReadFont(const Section, Ident: string; Font: TFont): TFont;
    procedure WriteFont(const Section, Ident: string; Font: TFont);
    function ReadRect(const Section, Ident: string; const Default: TRect): TRect;
    procedure WriteRect(const Section, Ident: string; const Value: TRect);
    function ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
    procedure WritePoint(const Section, Ident: string; const Value: TPoint);
    { properties }
    property ListItemName: string read GetItemName write SetItemName;
    property OnReadObject: TReadObjectEvent read FOnReadObject write FOnReadObject;
    property OnWriteObject: TWriteObjectEvent read FOnWriteObject write FOnWriteObject;
  end;

function StringToFontStyles(const Styles: string): TFontStyles;
function FontStylesToString(Styles: TFontStyles): string;
function FontToString(Font: TFont): string;
procedure StringToFont(const Str: string; Font: TFont);
function RectToStr(Rect: TRect): string;
function StrToRect(const Str: AnsiString; const Def: TRect): TRect;
function PointToStr(P: TPoint): string;
function StrToPoint(const Str: AnsiString; const Def: TPoint): TPoint;

function DefProfileName: string;
function DefLocalProfileName: string;

const
  idnListItem = 'Item';

implementation

uses
  SysUtils, Forms{$IFDEF VER80}, Str16{$ENDIF}, {$IFDEF RX_D12}AnsiStrings, {$ENDIF}
  {$IFDEF RX_D6}RTLConsts, {$ENDIF}RxStrUtils; // Polaris

const
  idnListCount = 'Count';
  idnDefString = #255#255;
  Lefts = ['[', '{', '('];
  Rights = [']', '}', ')'];

{ Utilities routines }

function DefLocalProfileName: string;
begin
  Result := ChangeFileExt(Application.ExeName, '.INI');
end;

function DefProfileName: string;
begin
  Result := ExtractFileName(DefLocalProfileName);
end;

function FontStylesToString(Styles: TFontStyles): string;
begin
  Result := '';
  if fsBold in Styles then Result := Result + 'B';
  if fsItalic in Styles then Result := Result + 'I';
  if fsUnderline in Styles then Result := Result + 'U';
  if fsStrikeOut in Styles then Result := Result + 'S';
end;

function StringToFontStyles(const Styles: string): TFontStyles;
begin
  Result := [];
  if Pos('B', UpperCase(Styles)) > 0 then Include(Result, fsBold);
  if Pos('I', UpperCase(Styles)) > 0 then Include(Result, fsItalic);
  if Pos('U', UpperCase(Styles)) > 0 then Include(Result, fsUnderline);
  if Pos('S', UpperCase(Styles)) > 0 then Include(Result, fsStrikeOut);
end;

function FontToString(Font: TFont): string;
begin
  with Font do
    Result := Format('%s,%d,%s,%d,%s,%d', [Name, Size,
      FontStylesToString(Style), Ord(Pitch), ColorToString(Color),
        {$IFDEF RX_D3}Charset{$ELSE}0{$ENDIF}]);
end;

type
  THackFont = class(TFont);

procedure StringToFont(const Str: string; Font: TFont);
const
  Delims = [',', ';'];
var
  FontChange: TNotifyEvent;
  Pos: Integer;
  I: Byte;
  S: string;
begin
  FontChange := Font.OnChange;
  Font.OnChange := nil;
  try
    Pos := 1;
    I := 0;
    while Pos <= Length(Str) do
    begin
      Inc(I);
      S := Trim(ExtractSubstr(Str, Pos, Delims));
      case I of
        1: Font.Name := S;
        2: Font.Size := StrToIntDef(S, Font.Size);
        3: Font.Style := StringToFontStyles(S);
        4: Font.Pitch := TFontPitch(StrToIntDef(S, Ord(Font.Pitch)));
        5: Font.Color := StringToColor(S);
        {$IFDEF RX_D3}
        6: Font.Charset := TFontCharset(StrToIntDef(S, Font.Charset));
        {$ENDIF}
      end;
    end;
  finally
    Font.OnChange := FontChange;
    THackFont(Font).Changed;
  end;
end;

function RectToStr(Rect: TRect): string;
begin
  with Rect do
    Result := Format('[%d,%d,%d,%d]', [Left, Top, Right, Bottom]);
end;

function StrToRect(const Str: AnsiString; const Def: TRect): TRect;
var
  S: AnsiString;
  Temp: string[10];
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S[1] in Lefts) and (S[Length(S)] in Rights) then
  begin
    Delete(S, 1, 1); SetLength(S, Length(S) - 1);
  end;
  {$IFDEF RX_D12}
  I := AnsiStrings.PosEx(',', S);
  {$ELSE}
  I := Pos(',', S);
  {$ENDIF}
  if I > 0 then
  begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.Left := StrToIntDef(string(Temp), Def.Left);
    Delete(S, 1, I);
    {$IFDEF RX_D12}
    I := AnsiStrings.PosEx(',', S);
    {$ELSE}
    I := Pos(',', S);
    {$ENDIF}
    if I > 0 then
    begin
      Temp := Trim(Copy(S, 1, I - 1));
      Result.Top := StrToIntDef(string(Temp), Def.Top);
      Delete(S, 1, I);
      {$IFDEF RX_D12}
      I := AnsiStrings.PosEx(',', S);
      {$ELSE}
      I := Pos(',', S);
      {$ENDIF}
      if I > 0 then
      begin
        Temp := Trim(Copy(S, 1, I - 1));
        Result.Right := StrToIntDef(string(Temp), Def.Right);
        Delete(S, 1, I);
        Temp := Trim(S);
        Result.Bottom := StrToIntDef(string(Temp), Def.Bottom);
      end;
    end;
  end;
end;

function PointToStr(P: TPoint): string;
begin
  with P do
    Result := Format('[%d,%d]', [X, Y]);
end;

function StrToPoint(const Str: AnsiString; const Def: TPoint): TPoint;
var
  S: AnsiString;
  Temp: string[10];
  I: Integer;
begin
  Result := Def;
  S := Str;
  if (S[1] in Lefts) and (S[Length(Str)] in Rights) then
  begin
    Delete(S, 1, 1); SetLength(S, Length(S) - 1);
  end;
  {$IFDEF RX_D12}
  I := AnsiStrings.PosEx(',', S);
  {$ELSE}
  I := Pos(',', S);
  {$ENDIF}
  if I > 0 then
  begin
    Temp := Trim(Copy(S, 1, I - 1));
    Result.X := StrToIntDef(string(Temp), Def.X);
    Delete(S, 1, I);
    Temp := Trim(S);
    Result.Y := StrToIntDef(string(Temp), Def.Y);
  end;
end;

{ TRxIniFile }

constructor TRxIniFile.Create(const FileName: string);
begin
  inherited Create(FileName);
  {$IFDEF RX_D4} // Polaris
  FListItemName := idnListItem;
  {$ELSE}
  FListItemName := NewStr(idnListItem);
  {$ENDIF}
  FOnReadObject := nil;
  FOnWriteObject := nil;
end;

destructor TRxIniFile.Destroy;
begin
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FListItemName);
  {$ENDIF}
  inherited Destroy;
end;

procedure TRxIniFile.Flush;
var
  {$IFNDEF VER80}
  CFileName: array[0..MAX_PATH] of WideChar;
  {$ELSE}
  CFileName: array[0..127] of Char;
  {$ENDIF}
begin
  {$IFNDEF VER80}
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    WritePrivateProfileStringW(nil, nil, nil, StringToWideChar(FileName,
      CFileName, MAX_PATH))
  else
    WritePrivateProfileString(nil, nil, nil, PChar(FileName));
  {$ELSE}
  WritePrivateProfileString(nil, nil, nil, StrPLCopy(CFileName,
    FileName, SizeOf(CFileName) - 1));
  {$ENDIF}
end;

{$IFDEF VER80}

procedure TRxIniFile.DeleteKey(const Section, Ident: string);
var
  CSection: array[0..127] of Char;
  CIdent: array[0..127] of Char;
  CFileName: array[0..127] of Char;
begin
  WritePrivateProfileString(StrPLCopy(CSection, Section, SizeOf(CSection) - 1),
    StrPLCopy(CIdent, Ident, SizeOf(CIdent) - 1), nil,
    StrPLCopy(CFileName, FileName, SizeOf(CFileName) - 1));
end;
{$ENDIF}

function TRxIniFile.GetItemName: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FListItemName;
  {$ELSE}
  Result := FListItemName^;
  {$ENDIF}
end;

procedure TRxIniFile.SetItemName(const Value: string);
begin
  {$IFDEF RX_D4} // Polaris
  FListItemName := Value;
  {$ELSE}
  AssignStr(FListItemName, Value);
  {$ENDIF}
end;

procedure TRxIniFile.WriteObject(const Section, Item: string; Index: Integer;
  Obj: TObject);
begin
  if Assigned(FOnWriteObject) then FOnWriteObject(Self, Section, Item, Obj);
end;

function TRxIniFile.ReadObject(const Section, Item, Value: string): TObject;
begin
  Result := nil;
  if Assigned(FOnReadObject) then Result := FOnReadObject(Self, Section, Item, Value);
end;

procedure TRxIniFile.WriteList(const Section: string; List: TStrings);
var
  I: Integer;
begin
  EraseSection(Section);
  WriteInteger(Section, idnListCount, List.Count);
  for I := 0 to List.Count - 1 do
  begin
    WriteString(Section, ListItemName + IntToStr(I), List[I]);
    WriteObject(Section, ListItemName + IntToStr(I), I, List.Objects[I]);
  end;
end;

function TRxIniFile.ReadListParam(const Section: string; Append: Boolean;
  List: TStrings): TStrings;
var
  I, IniCount: Integer;
  AssString: string;
begin
  Result := List;
  IniCount := ReadInteger(Section, idnListCount, -1);
  if IniCount >= 0 then
  begin
    if not Append then List.Clear;
    for I := 0 to IniCount - 1 do
    begin
      AssString := ReadString(Section, ListItemName + IntToStr(I), idnDefString);
      if AssString <> idnDefString then
        List.AddObject(AssString, ReadObject(Section, ListItemName +
          IntToStr(I), AssString));
    end;
  end;
end;

function TRxIniFile.ReadClearList(const Section: string; List: TStrings): TStrings;
begin
  Result := ReadListParam(Section, False, List);
end;

function TRxIniFile.ReadList(const Section: string; List: TStrings): TStrings;
begin
  Result := ReadListParam(Section, True, List);
end;

function TRxIniFile.ReadColor(const Section, Ident: string;
  Default: TColor): TColor;
begin
  try
    Result := StringToColor(ReadString(Section, Ident,
      ColorToString(Default)));
  except
    Result := Default;
  end;
end;

procedure TRxIniFile.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section, Ident, ColorToString(Value));
end;

function TRxIniFile.ReadRect(const Section, Ident: string; const Default: TRect): TRect;
begin
  Result := StrToRect(AnsiString(ReadString(Section, Ident, RectToStr(Default))), Default);
end;

procedure TRxIniFile.WriteRect(const Section, Ident: string; const Value: TRect);
begin
  WriteString(Section, Ident, RectToStr(Value));
end;

function TRxIniFile.ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
begin
  Result := StrToPoint(AnsiString(ReadString(Section, Ident, PointToStr(Default))), Default);
end;

procedure TRxIniFile.WritePoint(const Section, Ident: string; const Value: TPoint);
begin
  WriteString(Section, Ident, PointToStr(Value));
end;

function TRxIniFile.ReadFont(const Section, Ident: string; Font: TFont): TFont;
begin
  Result := Font;
  try
    StringToFont(ReadString(Section, Ident, FontToString(Font)), Result);
  except
    { do nothing, ignore any exceptions }
  end;
end;

procedure TRxIniFile.WriteFont(const Section, Ident: string; Font: TFont);
begin
  WriteString(Section, Ident, FontToString(Font));
end;

end.