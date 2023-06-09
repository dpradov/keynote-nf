{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Patched by JB. for Unicode store                      }
{*******************************************************}

unit RxStrHlder;

interface

{$I RX.INC}

uses
  Windows, SysUtils, Classes;

type

  {$IFDEF RX_D3}

{ TMacro }

  TMacros = class;
  TMacroTextEvent = procedure(Sender: TObject; Data: Variant;
    var Text: string) of object;

  TMacro = class(TCollectionItem)
  private
    FName: string;
    FData: Variant;
    FOnGetText: TMacroTextEvent;
    function IsMacroStored: Boolean;
    function GetText: string;
    function GetMacros: TMacros;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    procedure GetMacroText(var AText: string);
    function GetAsVariant: Variant;
    procedure SetAsVariant(Value: Variant);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function IsEqual(Value: TMacro): Boolean;
    property Macros: TMacros read GetMacros;
    property Text: string read GetText;
  published
    property Name: string read FName write SetDisplayName;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsMacroStored;
    property OnGetText: TMacroTextEvent read FOnGetText write FOnGetText;
  end;

{ TMacros }

  TMacros = class({$IFDEF RX_D4}TOwnedCollection{$ELSE}TCollection{$ENDIF})
  private
    function GetMacroValue(const MacroName: string): Variant;
    procedure SetMacroValue(const MacroName: string;
      const Value: Variant);
    function GetItem(Index: Integer): TMacro;
    procedure SetItem(Index: Integer; Value: TMacro);
  public
    {$IFDEF RX_D4}
    constructor Create(AOwner: TPersistent);
    {$ELSE}
    constructor Create;
    {$ENDIF}
    procedure AssignValues(Value: TMacros);
    procedure AddMacro(Value: TMacro);
    procedure RemoveMacro(Value: TMacro);
    function CreateMacro(const MacroName: string): TMacro;
    procedure GetMacroList(List: TList; const MacroNames: string);
    function IndexOf(const AName: string): Integer;
    function IsEqual(Value: TMacros): Boolean;
    function ParseString(const Value: string; DoCreate: Boolean;
      SpecialChar: Char): string;
    function MacroByName(const Value: string): TMacro;
    function FindMacro(const Value: string): TMacro;
    property Items[Index: Integer]: TMacro read GetItem write SetItem; default;
    property MacroValues[const MacroName: string]: Variant read GetMacroValue write SetMacroValue;
  end;

  {$ENDIF RX_D3}

{ TStrHolder }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TStrHolder = class(TComponent)
  private
    FStrings: TStrings;
    FXorKey: AnsiString;
    FReserved: Integer;
    {$IFDEF RX_D3}
    FMacros: TMacros;
    FMacroChar: Char;
    FOnExpandMacros: TNotifyEvent;
    {$ENDIF}
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(Value: TDuplicates);
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    procedure SetStrings(Value: TStrings);
    procedure StringsChanged(Sender: TObject);
    procedure StringsChanging(Sender: TObject);
    procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter);
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
    {$IFNDEF VER80}
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    {$ENDIF}
    {$IFDEF RX_D3}
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    {$ENDIF}
    {$IFDEF RX_D3}
    procedure SetMacros(Value: TMacros);
    procedure RecreateMacros;
    procedure SetMacroChar(Value: Char);
    {$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed; dynamic;
    procedure Changing; dynamic;
    {$IFDEF RX_D3}
    procedure BeforeExpandMacros; dynamic;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    {$IFDEF RX_D3}
    function MacroCount: Integer;
    function MacroByName(const MacroName: string): TMacro;
    function ExpandMacros: string;
    {$ENDIF}
    {$IFNDEF VER80}
    property CommaText: string read GetCommaText write SetCommaText;
    {$ENDIF}
  published
    {$IFDEF RX_D3}
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property MacroChar: Char read FMacroChar write SetMacroChar default '%';
    property Macros: TMacros read FMacros write SetMacros;
    property OnExpandMacros: TNotifyEvent read FOnExpandMacros write FOnExpandMacros;
    {$ENDIF}
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates
      default dupIgnore;
    property KeyString: AnsiString read FXorKey write FXorKey stored False;
    property Sorted: Boolean read GetSorted write SetSorted default False;
    property Strings: TStrings read FStrings write SetStrings stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

implementation

uses
  {$IFDEF RX_D3}
  Consts,
  {$ENDIF}
  {$IFDEF RX_D6}Variants, RTLConsts, {$ENDIF}RxStrUtils; // Polaris

const
  XorVersion = {$IFDEF UNICODE}2{$ELSE}1{$ENDIF};

  {$IFDEF RX_D3}

function ExtractName(const Items: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Items)) and (Items[I] <> ';') do
    Inc(I);
  Result := Trim(Copy(Items, Pos, I - Pos));
  if (I <= Length(Items)) and (Items[I] = ';') then Inc(I);
  Pos := I;
end;

function NameDelimiter(C: Char; Delims: TCharSet): Boolean;
begin
  Result := CharInSet(C, [' ', ',', ';', ')', #13, #10]) or CharInSet(C, Delims);
end;

function IsLiteral(C: Char): Boolean;
begin
  Result := CharInSet(C, ['''', '"']);
end;

procedure CreateMacros(List: TMacros; const Value: PChar; SpecialChar: Char; Delims: TCharSet);
var
  CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar(Value: Char);
    begin
      if TempBuf^ = Value then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] = Value then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    try
      StrCopy(TempBuf, Buffer);
      StripChar('''');
      StripChar('"');
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

begin
  if SpecialChar = #0 then Exit;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    CurChar := CurPos^;
    if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ <> SpecialChar) then
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter(CurChar, Delims)) do
      begin
        Inc(CurPos);
        CurChar := CurPos^;
        if IsLiteral(CurChar) then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else
        Name := StrPas(StartPos + 1);
      if Assigned(List) then
      begin
        if List.FindMacro(Name) = nil then
          List.CreateMacro(Name);
      end;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = SpecialChar) and not Literal and ((CurPos + 1)^ = SpecialChar) then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral(CurChar) then
      Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;

{ TMacro }

constructor TMacro.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FData := Unassigned;
end;

procedure TMacro.Assign(Source: TPersistent);
begin
  if (Source is TMacro) and (Source <> nil) then
  begin
    if VarIsEmpty(TMacro(Source).FData) then
      Clear
    else
      Value := TMacro(Source).FData;
    Name := TMacro(Source).Name;
  end;
end;

function TMacro.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName
  else
    Result := FName;
end;

procedure TMacro.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TMacros) and (TMacros(Collection).IndexOf(Value) >= 0) then
    raise Exception.Create(SDuplicateString);
  FName := Value;
  inherited;
end;

procedure TMacro.GetMacroText(var AText: string);
begin
  if Assigned(FOnGetText) then FOnGetText(Self, FData, AText);
end;

function TMacro.GetText: string;
begin
  Result := FData;
  GetMacroText(Result);
end;

function TMacro.GetMacros: TMacros;
begin
  if Collection is TMacros then
    Result := TMacros(Collection)
  else
    Result := nil;
end;

procedure TMacro.Clear;
begin
  FData := Unassigned;
end;

function TMacro.IsMacroStored: Boolean;
begin
  Result := not VarIsEmpty(FData);
end;

function TMacro.GetAsVariant: Variant;
begin
  Result := FData;
end;

procedure TMacro.SetAsVariant(Value: Variant);
begin
  FData := Value;
end;

function TMacro.IsEqual(Value: TMacro): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (VarIsEmpty(FData) or (FData = Value.FData)) and
    (Name = Value.Name);
end;

{ TMacros }

{$IFDEF RX_D4}

constructor TMacros.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TMacro);
end;
{$ELSE}

constructor TMacros.Create;
begin
  inherited Create(TMacro);
end;
{$ENDIF}

function TMacros.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TMacro(Items[Result]).Name, AName) = 0 then Exit;
  Result := -1;
end;

function TMacros.GetItem(Index: Integer): TMacro;
begin
  Result := TMacro(inherited Items[Index]);
end;

procedure TMacros.SetItem(Index: Integer; Value: TMacro);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TMacros.AddMacro(Value: TMacro);
begin
  Value.Collection := Self;
end;

procedure TMacros.RemoveMacro(Value: TMacro);
begin
  if Value.Collection = Self then
    Value.Collection := nil;
end;

function TMacros.CreateMacro(const MacroName: string): TMacro;
begin
  Result := Add as TMacro;
  Result.Name := MacroName;
end;

function TMacros.IsEqual(Value: TMacros): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end;
end;

function TMacros.MacroByName(const Value: string): TMacro;
begin
  Result := FindMacro(Value);
  if Result = nil then
    raise Exception.Create(SInvalidPropertyValue);
end;

function TMacros.FindMacro(const Value: string): TMacro;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TMacro(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TMacros.AssignValues(Value: TMacros);
var
  I: Integer;
  P: TMacro;
begin
  BeginUpdate;
  try
    for I := 0 to Value.Count - 1 do
    begin
      P := FindMacro(Value[I].Name);
      if P <> nil then P.Assign(Value[I]);
    end;
  finally
    EndUpdate;
  end;
end;

function TMacros.ParseString(const Value: string; DoCreate: Boolean;
  SpecialChar: Char): string;
var
  Macros: TMacros;
begin
  Result := Value;
  Macros := TMacros.Create{$IFDEF RX_D4}(Self.GetOwner){$ENDIF};
  try
    CreateMacros(Macros, PChar(Result), SpecialChar, ['.']);
    if DoCreate then
    begin
      Macros.AssignValues(Self);
      Self.Assign(Macros);
    end;
  finally
    Macros.Free;
  end;
end;

function TMacros.GetMacroValue(const MacroName: string): Variant;
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then
  begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      Result := VarArrayCreate([0, Macros.Count - 1], varVariant);
      for I := 0 to Macros.Count - 1 do
        Result[I] := TMacro(Macros[I]).Value;
    finally
      Macros.Free;
    end;
  end
  else
    Result := MacroByName(MacroName).Value;
end;

procedure TMacros.SetMacroValue(const MacroName: string;
  const Value: Variant);
var
  I: Integer;
  Macros: TList;
begin
  if Pos(';', MacroName) <> 0 then
  begin
    Macros := TList.Create;
    try
      GetMacroList(Macros, MacroName);
      for I := 0 to Macros.Count - 1 do
        TMacro(Macros[I]).Value := Value[I];
    finally
      Macros.Free;
    end;
  end
  else
    MacroByName(MacroName).Value := Value;
end;

procedure TMacros.GetMacroList(List: TList; const MacroNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(MacroNames) do
    List.Add(MacroByName(ExtractName(MacroNames, Pos)));
end;

{$ENDIF RX_D3}

{ TStrHolder }

constructor TStrHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  {$IFDEF RX_D3}
  FMacros := TMacros.Create{$IFDEF RX_D4}(Self){$ENDIF};
  FMacroChar := '%';
  {$ENDIF}
  TStringList(FStrings).OnChange := StringsChanged;
  TStringList(FStrings).OnChanging := StringsChanging;
end;

destructor TStrHolder.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  {$IFDEF RX_D3}
  FMacros.Free;
  {$ENDIF}
  FStrings.Free;
  inherited Destroy;
end;

procedure TStrHolder.Assign(Source: TPersistent);
begin
  if Source is TStrings then
    FStrings.Assign(Source)
  else if Source is TStrHolder then
    FStrings.Assign(TStrHolder(Source).Strings)
  else
    inherited Assign(Source);
end;

procedure TStrHolder.AssignTo(Dest: TPersistent);
begin
  if Dest is TStrings then
    Dest.Assign(Strings)
  else
    inherited AssignTo(Dest);
end;

procedure TStrHolder.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TStrHolder.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TStrHolder.Clear;
begin
  FStrings.Clear;
end;

{$IFNDEF VER80}

function TStrHolder.GetCommaText: string;
begin
  Result := FStrings.CommaText;
end;

procedure TStrHolder.SetCommaText(const Value: string);
begin
  FStrings.CommaText := Value;
end;
{$ENDIF}

{$IFDEF RX_D3}

function TStrHolder.GetCapacity: Integer;
begin
  Result := FStrings.Capacity;
end;

procedure TStrHolder.SetCapacity(NewCapacity: Integer);
begin
  FStrings.Capacity := NewCapacity;
end;
{$ENDIF RX_D3}

{$IFDEF RX_D3}

procedure TStrHolder.BeforeExpandMacros;
begin
  if Assigned(FOnExpandMacros) then FOnExpandMacros(Self);
end;

procedure TStrHolder.SetMacros(Value: TMacros);
begin
  FMacros.AssignValues(Value);
end;

procedure TStrHolder.RecreateMacros;
begin
  if not (csReading in ComponentState) then
    Macros.ParseString(FStrings.Text, True, MacroChar);
end;

procedure TStrHolder.SetMacroChar(Value: Char);
begin
  if Value <> FMacroChar then
  begin
    FMacroChar := Value;
    RecreateMacros;
  end;
end;

function TStrHolder.MacroCount: Integer;
begin
  Result := Macros.Count;
end;

function TStrHolder.MacroByName(const MacroName: string): TMacro;
begin
  Result := Macros.MacroByName(MacroName);
end;

function TStrHolder.ExpandMacros: string;
var
  I, J, P, LiteralChars: Integer;
  Macro: TMacro;
  Found: Boolean;
begin
  BeforeExpandMacros;
  Result := FStrings.Text;
  for I := Macros.Count - 1 downto 0 do
  begin
    Macro := Macros[I];
    if VarIsEmpty(Macro.FData) then Continue;
    repeat
      P := Pos(MacroChar + Macro.Name, Result);
      Found := (P > 0) and ((Length(Result) = P + Length(Macro.Name)) or
        NameDelimiter(Result[P + Length(Macro.Name) + 1], ['.']));
      if Found then
      begin
        LiteralChars := 0;
        for J := 1 to P - 1 do
          if IsLiteral(Result[J]) then Inc(LiteralChars);
        Found := LiteralChars mod 2 = 0;
        if Found then
        begin
          Result := Copy(Result, 1, P - 1) + Macro.Text + Copy(Result,
            P + Length(Macro.Name) + 1, MaxInt);
        end;
      end;
    until not Found;
  end;
end;
{$ENDIF RX_D3}

procedure TStrHolder.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
    {$IFNDEF VER80}
  var
    I: Integer;
    Ancestor: TStrHolder;
    {$ENDIF}
  begin
    {$IFNDEF VER80}
    Ancestor := TStrHolder(Filer.Ancestor);
    Result := False;
    if (Ancestor <> nil) and (Ancestor.FStrings.Count = FStrings.Count) and
      (KeyString = Ancestor.KeyString) and (FStrings.Count > 0) then
      for I := 0 to FStrings.Count - 1 do
      begin
        Result := CompareText(FStrings[I], Ancestor.FStrings[I]) <> 0;
        if Result then Break;
      end
    else
      Result := (FStrings.Count > 0) or (Length(KeyString) > 0);
    {$ELSE}
    Result := (FStrings.Count > 0) or (Length(KeyString) > 0);
    {$ENDIF}
  end;

begin
  inherited DefineProperties(Filer);
  { for backward compatibility }
  Filer.DefineProperty('InternalVer', ReadVersion, WriteVersion,
    {$IFNDEF VER80}Filer.Ancestor = nil{$ELSE}False{$ENDIF});
  Filer.DefineProperty('StrData', ReadStrings, WriteStrings, DoWrite);
end;

function TStrHolder.GetSorted: Boolean;
begin
  Result := TStringList(FStrings).Sorted;
end;

function TStrHolder.GetDuplicates: TDuplicates;
begin
  Result := TStringList(FStrings).Duplicates;
end;

procedure TStrHolder.ReadStrings(Reader: TReader);
{$IFNDEF UNICODE}
{$IFNDEF RX_D6}

  function Utf8ToAnsi(Source: string): AnsiString;
    (* Converts the given UTF-8 String to Windows ANSI (in current system codepage). *)

    procedure OnErrorInConvert(var r: string; const s: string; const mes: string);
    begin
      MessageBox(0, PChar(Format('Utf8ToAnsi error occured. Message: %s; Code=%u', [mes, GetLastError])), 'Error', MB_OK);
      SetLength(r, Length(s));
      r := s;
    end;

  var
    wideStr: WideString;
    wideLen: Integer;
    converted: Integer;
    errOccured: Boolean;
  begin
    errOccured := False;
    try
      // Determine real size of UTF-8 string in symbols
      wideLen := MultiByteToWideChar(CP_UTF8, 0, PChar(Source), Length(Source), nil, 0);
      if wideLen <> 0 then
      begin
        // Allocate memory for UTF-16 string
        SetLength(wideStr, wideLen);
        // Convert source UTF-8 string to UTF-16 (WideString)
        wideLen := MultiByteToWideChar(CP_UTF8, 0, PChar(Source), Length(Source), PWChar(wideStr), wideLen);
        if wideLen <> 0 then
        begin
          // Allocate memory for result AnsiString
          SetLength(Result, wideLen);
          // Convert UTF-16 (WideString) to AnsiString with current system codepage
          converted := WideCharToMultibyte(CP_ACP, 0, PWChar(wideStr), wideLen, PChar(Result), wideLen, nil, nil);
          if converted = 0 then
            errOccured := True;
        end
        else
          errOccured := True;
      end
      else
        errOccured := True;

      if errOccured then
        OnErrorInConvert(Result, Source, '');
    except on E: Exception do
      begin
        OnErrorInConvert(Result, Source, E.Message);
      end;
    end;
  end;
  {$ENDIF}
  {$ENDIF}
begin
  Reader.ReadListBegin;
  if not Reader.EndOfList then
    KeyString := AnsiString(Reader.ReadString);
  FStrings.Clear;
  {$IFDEF UNICODE}
  while not Reader.EndOfList do
  begin
    { je nizsi verze - obsah je vzdy v ANSII }
    if FReserved = 0 then
    begin
      { otevreny format. nic nekodovano }
      { precti zaznam do ansi a pridej }
      FStrings.Add(string(AnsiString(Reader.ReadString)));
    end
    else if FReserved < XorVersion then
    begin
      { precti zaznam do ANSII a dekoduj }
      { konvertuj do unicode a pridej }
      FStrings.Add(string(XorDecode(KeyString, AnsiString(Reader.ReadString))));
    end
    else if FReserved >= XorVersion then
    begin
      { precti zaznam do string - je to hexadecimalni zapis UTF8 a dekoduj }
      FStrings.Add({$IFDEF RX_D12}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(XorDecode(KeyString, AnsiString(Reader.ReadString))))
    end;
  end;
  {$ELSE}
  while not Reader.EndOfList do
    if FReserved >= XorVersion then
      if FReserved > XorVersion then
        FStrings.Add(Utf8ToAnsi(XorDecode(KeyString, AnsiString(Reader.ReadString))))
      else
        FStrings.Add(XorDecode(KeyString, AnsiString(Reader.ReadString)))
    else
      FStrings.Add(string(XorString(KeyString, AnsiString(Reader.ReadString))));
  {$ENDIF}
  Reader.ReadListEnd;
end;

procedure TStrHolder.SetDuplicates(Value: TDuplicates);
begin
  TStringList(FStrings).Duplicates := Value;
end;

procedure TStrHolder.SetSorted(Value: Boolean);
begin
  TStringList(FStrings).Sorted := Value;
end;

procedure TStrHolder.SetStrings(Value: TStrings);
begin
  FStrings.Assign(Value);
end;

procedure TStrHolder.StringsChanged(Sender: TObject);
begin
  {$IFDEF RX_D3}
  RecreateMacros;
  {$ENDIF}
  if not (csReading in ComponentState) then Changed;
end;

procedure TStrHolder.StringsChanging(Sender: TObject);
begin
  if not (csReading in ComponentState) then Changing;
end;

procedure TStrHolder.WriteStrings(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteString(string(KeyString));
  for I := 0 to FStrings.Count - 1 do
  begin
    {$IFDEF UNICODE}
    Writer.WriteString(string(XorEncode(KeyString, UTF8Encode(FStrings[I]))));
    {$ELSE}
    {$IFNDEF VER80}
    Writer.WriteString(XorEncode(KeyString, AnsiString(FStrings[I])));
    {$ELSE}
    Writer.WriteString(XorString(KeyString, FStrings[I]));
    {$ENDIF}
    {$ENDIF}
  end;
  Writer.WriteListEnd;
end;

procedure TStrHolder.ReadVersion(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TStrHolder.WriteVersion(Writer: TWriter);
begin
  {$IFNDEF VER80}
  Writer.WriteInteger(XorVersion);
  {$ENDIF}
end;

end.