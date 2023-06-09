{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxCurrEdit;

{$I RX.INC}
{$W-}

interface

uses
  SysUtils, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Menus, Forms, StdCtrls, Mask,
  Buttons, RxToolEdit;

type

{ TCustomNumEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCustomNumEdit = class(TCustomComboEdit)
  private
    FCanvas: TControlCanvas;
    FAlignment: TAlignment;
    FFocused: Boolean;
    FValue: Extended;
    FMinValue, FMaxValue: Extended;
    FDecimalPlaces: Cardinal;
    FBeepOnError: Boolean;
    FCheckOnExit: Boolean;
    FZeroEmpty: Boolean;
    FFormatOnEditing: Boolean;
    FFormatting: Boolean;
    {$IFDEF RX_D4} // Polaris
    FDisplayFormat: string;
    {$ELSE}
    FDisplayFormat: PString;
    {$ENDIF}
// Polaris
    FDecimalPlaceRound: Boolean;
    procedure SetDecimalPlaceRound(Value: Boolean);

    procedure SetFocused(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBeepOnError(Value: Boolean);
    procedure SetDisplayFormat(const Value: string);
    function GetDisplayFormat: string;
    procedure SetDecimalPlaces(Value: Cardinal);
    function GetValue: Extended;
    procedure SetValue(AValue: Extended);
    function GetAsInteger: LongInt;
    procedure SetAsInteger(AValue: LongInt);
    procedure SetMaxValue(AValue: Extended);
    procedure SetMinValue(AValue: Extended);
    procedure SetZeroEmpty(Value: Boolean);
    procedure SetFormatOnEditing(Value: Boolean);
    function GetText: string;
    procedure SetText(const AValue: string);
    function TextToValText(const AValue: string): string;
//Polaris    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;
    function IsFormatStored: Boolean;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
//Polaris up to porotected
    function CheckValue(NewValue: Extended; RaiseOnError: Boolean): Extended;

    {$IFNDEF VER80}
    procedure AcceptValue(const Value: Variant); override;
    {$ELSE}
    procedure AcceptValue(const Value: string); override;
    {$ENDIF}
    procedure Change; override;
    procedure ReformatEditText; dynamic;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
    procedure DataChanged; virtual;
    function DefaultDisplayFormat: string; virtual;
    procedure KeyPress(var Key: Char); override;
    function IsValidChar(Key: Char): Boolean; virtual;
    function FormatDisplayText(Value: Extended): string;
    function GetDisplayText: string; virtual;
    procedure Reset; override;
    procedure CheckRange;
    procedure UpdateData;
    procedure UpdatePopup; virtual;
    property Formatting: Boolean read FFormatting;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taRightJustify;
    property BeepOnError: Boolean read FBeepOnError write SetBeepOnError
      default True;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property GlyphKind default gkDefault;
    property ButtonWidth default 21; //Polaris 20;
    property DecimalPlaces: Cardinal read FDecimalPlaces write SetDecimalPlaces
      default 2;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
      stored IsFormatStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property FormatOnEditing: Boolean read FFormatOnEditing
      write SetFormatOnEditing default False;
    property Text: string read GetText write SetText stored False;
    property MaxLength default 0;
    property ZeroEmpty: Boolean read FZeroEmpty write SetZeroEmpty default True;
//Polaris
    property DecimalPlaceRound: Boolean read FDecimalPlaceRound write SetDecimalPlaceRound default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; {$IFDEF RX_D5} override; {$ENDIF}
    property AsInteger: LongInt read GetAsInteger write SetAsInteger;
    property DisplayText: string read GetDisplayText;
    property PopupVisible;
    property Value: Extended read GetValue write SetValue;
  end;

{ TCurrencyEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurrencyEdit = class(TCustomNumEdit)
  protected
    function DefaultDisplayFormat: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
//Polaris
    property Align;
    property DecimalPlaceRound;

    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property CheckOnExit;
    property Color;
    property Ctl3D;
    property DecimalPlaces;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property HideSelection;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFNDEF VER80}
    {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property MaxLength;
    property MaxValue;
    property MinValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

{ TRxCustomCalcEdit }

  TRxCustomCalcEdit = class(TCustomNumEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TRxCalcEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCalcEdit = class(TRxCustomCalcEdit)
  published
//Polaris
    property Align;
    property DecimalPlaceRound;

    property Alignment;
    property AutoSelect;
    property AutoSize;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFNDEF VER80}
    {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property MaxLength;
    property MaxValue;
    property MinValue;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Value;
    property Visible;
    property ZeroEmpty;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

implementation

uses Consts, RxVCLUtils, RxMaxMin, RxCalc, RxStrUtils; // Polaris

{$R *.RES}

const
  sCalcBmp = 'CEDITBMP'; { Numeric editor button glyph }
  CalcBitmap: TBitmap = nil;

type
  THack = class(TPopupWindow);

function IsValidFloat(const Value: string; var RetValue: Extended): Boolean;
var
  I: Integer;
  Buffer: array[0..63] of Char;
begin
  Result := False;
  for I := 1 to Length(Value) do
    if not CharInSet(Value[I], [{$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, '-', '+', '0'..'9', 'e', 'E']) then
      Exit;
  Result := TextToFloat(StrPLCopy(Buffer, Value,
    SizeOf(Buffer) - 1), RetValue{$IFNDEF VER80}, fvExtended{$ENDIF});
end;

function FormatFloatStr(const S: string; Thousands: Boolean): string;
var
  I, MaxSym, MinSym, Group: Integer;
  IsSign: Boolean;
begin
  Result := '';
  MaxSym := Length(S);
  IsSign := (MaxSym > 0) and CharInSet(S[1], ['-', '+']);
  if IsSign then
    MinSym := 2
  else
    MinSym := 1;
  I := Pos({$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, S);
  if I > 0 then MaxSym := I - 1;
  I := Pos('E', AnsiUpperCase(S));
  if I > 0 then MaxSym := Min(I - 1, MaxSym);
  Result := Copy(S, MaxSym + 1, System.MaxInt);
  Group := 0;
  for I := MaxSym downto MinSym do
  begin
    Result := S[I] + Result;
    Inc(Group);
    if (Group = 3) and Thousands and (I > MinSym) then
    begin
      Group := 0;
      Result := {$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator + Result;
    end;
  end;
  if IsSign then Result := S[1] + Result;
end;

{ TCustomNumEdit }

constructor TCustomNumEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FDecimalPlaceRound := False; // Polaris
  MaxLength := 0;
  FBeepOnError := True;
  FAlignment := taRightJustify;
  {$IFDEF RX_D4} // Polaris
  FDisplayFormat := DefaultDisplayFormat;
  {$ELSE}
  FDisplayFormat := NewStr(DefaultDisplayFormat);
  {$ENDIF}
  FDecimalPlaces := 2;
  FZeroEmpty := True;
  inherited Text := '';
  inherited Alignment := taLeftJustify;
  FDefNumGlyphs := 2;
  { forces update }
  DataChanged;
  ControlState := ControlState + [csCreating];
  try
    GlyphKind := gkDefault;
//Polaris ButtonWidth := 20;
    ButtonWidth := 21;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TCustomNumEdit.Destroy;
begin
  FCanvas.Free;
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FDisplayFormat);
  {$ENDIF}
  if FPopup <> nil then
  begin
    TPopupWindow(FPopup).OnCloseUp := nil;
    FPopup.Free;
    FPopup := nil;
  end;
  inherited Destroy;
end;

//Polaris

procedure TCustomNumEdit.SetDecimalPlaceRound(Value: Boolean);
begin
  if FDecimalPlaceRound <> Value then
  begin
    FDecimalPlaceRound := Value;
    SetValue(CheckValue(FValue, False));
    Invalidate;
  end;
end;

function TCustomNumEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if CalcBitmap = nil then
  begin
    CalcBitmap := TBitmap.Create;
    CalcBitmap.Handle := LoadBitmap(hInstance, sCalcBmp);
  end;
  Result := CalcBitmap;
end;

function TCustomNumEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

function TCustomNumEdit.IsFormatStored: Boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

function TCustomNumEdit.IsValidChar(Key: Char): Boolean;
var
  S: string;
  SelStart, SelStop, DecPos: Integer;
  RetValue: Extended;
begin
  Result := False;
  S := EditText;
  GetSel(SelStart, SelStop);
  System.Delete(S, SelStart + 1, SelStop - SelStart);
  System.Insert(Key, S, SelStart + 1);
  S := TextToValText(S);
  DecPos := Pos({$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, S);
  if (DecPos > 0) then
  begin
    SelStart := Pos('E', UpperCase(S));
    if (SelStart > DecPos) then
      DecPos := SelStart - DecPos
    else
      DecPos := Length(S) - DecPos;
    if DecPos > Integer(FDecimalPlaces) then Exit;
  end;
  Result := IsValidFloat(S, RetValue);
  if Result and (FMinValue >= 0) and (FMaxValue > 0) and (RetValue < 0) then
    Result := False;
end;

procedure TCustomNumEdit.KeyPress(var Key: Char);
begin
  if PopupVisible and CharInSet(UpCase(Key), ['0'..'9', {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, '.', ',',
    '+', '-', '*', '/', '_', '=', 'C', 'R', 'Q', '%', #8, #13] -
      [{$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator]) then
  begin
    THack(FPopup).KeyPress(Key);
    Key := #0;
  end;
  if CharInSet(Key, ['.', ','] - [{$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator]) then
    Key := {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator;
  inherited KeyPress(Key);
  if (Key >= ' ') and not IsValidChar(Key) then
  begin
    if BeepOnError then MessageBeep(0);
    Key := #0;
  end
  else if Key = #27 then
  begin
    Reset;
    Key := #0;
  end;
end;

procedure TCustomNumEdit.Reset;
begin
  DataChanged;
  SelectAll;
end;

procedure TCustomNumEdit.SetZeroEmpty(Value: Boolean);
begin
  if FZeroEmpty <> Value then
  begin
    FZeroEmpty := Value;
    DataChanged;
  end;
end;

procedure TCustomNumEdit.SetBeepOnError(Value: Boolean);
begin
  if FBeepOnError <> Value then
  begin
    FBeepOnError := Value;
    UpdatePopup;
  end;
end;

procedure TCustomNumEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCustomNumEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then
  begin
    {$IFDEF RX_D4} // Polaris
    FDisplayFormat := Value;
    {$ELSE}
    AssignStr(FDisplayFormat, Value);
    {$ENDIF}
    Invalidate;
    DataChanged;
  end;
end;

function TCustomNumEdit.GetDisplayFormat: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FDisplayFormat;
  {$ELSE}
  Result := FDisplayFormat^;
  {$ENDIF}
end;

procedure TCustomNumEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    Invalidate;
    FFormatting := True;
    try
      DataChanged;
    finally
      FFormatting := False;
    end;
  end;
end;

procedure TCustomNumEdit.SetFormatOnEditing(Value: Boolean);
begin
  if FFormatOnEditing <> Value then
  begin
    FFormatOnEditing := Value;
    if FFormatOnEditing then
      inherited Alignment := Alignment
    else
      inherited Alignment := taLeftJustify;
    if FFormatOnEditing and FFocused then
      ReformatEditText
    else if FFocused then
    begin
      UpdateData;
      DataChanged;
    end;
  end;
end;

procedure TCustomNumEdit.SetDecimalPlaces(Value: Cardinal);
begin
  if FDecimalPlaces <> Value then
  begin
    FDecimalPlaces := Value;
    SetValue(CheckValue(FValue, False));
    DataChanged;
    Invalidate;
  end;
end;

function TCustomNumEdit.FormatDisplayText(Value: Extended): string;
begin
  if DisplayFormat <> '' then
    Result := FormatFloat(DisplayFormat, Value)
  else
    Result := FloatToStr(Value);
end;

function TCustomNumEdit.GetDisplayText: string;
begin
  Result := FormatDisplayText(FValue);
end;

procedure TCustomNumEdit.Clear;
begin
  Text := '';
end;

procedure TCustomNumEdit.DataChanged;
var
  EditFormat: string;
begin
  EditFormat := '0';
  if FDecimalPlaces > 0 then
    EditFormat := EditFormat + '.' + MakeStr('#', FDecimalPlaces);
  if (FValue = 0.0) and FZeroEmpty then
    EditText := ''
  else
    EditText := FormatFloat(EditFormat, CheckValue(FValue, False));
end;

function TCustomNumEdit.CheckValue(NewValue: Extended;
  RaiseOnError: Boolean): Extended;

  function Sign(Value: Extended): Integer;
  begin
    if Value = 0 then
      Result := 0
    else if Value < 0 then
      Result := -1
    else
      Result := 1;
  end;

var
  DP: Integer;
begin
  if FDecimalPlaceRound then
  begin //Polaris
    DP := FDecimalPlaces;
    NewValue := Int(NewValue * Exp(DP * Ln(10)) + Sign(NewValue) * 0.50000001) * Exp(-DP * Ln(10));
  end;
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if (FMaxValue > FMinValue) then
    begin
      if NewValue < FMinValue then
        Result := FMinValue
      else if NewValue > FMaxValue then
        Result := FMaxValue;
    end
    else
    begin
      if FMaxValue = 0 then
      begin
        if NewValue < FMinValue then Result := FMinValue;
      end
      else if FMinValue = 0 then
      begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
    if RaiseOnError and (Result <> NewValue) then
      raise ERangeError.CreateFmt(ReplaceStr(ResStr(SOutOfRange), '%d', '%.*f'),
        [DecimalPlaces, FMinValue, DecimalPlaces, FMaxValue]);
  end;
end;

procedure TCustomNumEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValue(StrToFloat(TextToValText(EditText)), True);
end;

procedure TCustomNumEdit.UpdateData;
begin
  ValidateEdit;
  FValue := CheckValue(StrToFloat(TextToValText(EditText)), False);
end;

procedure TCustomNumEdit.UpdatePopup;
begin
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, DefCalcPrecision, BeepOnError);
end;

function TCustomNumEdit.GetValue: Extended;
begin
  if not (csDesigning in ComponentState) then
  try
    UpdateData;
  except
    FValue := FMinValue;
  end;
  Result := FValue;
end;

procedure TCustomNumEdit.SetValue(AValue: Extended);
begin
  FValue := CheckValue(AValue, False);
  DataChanged;
  Invalidate;
end;

function TCustomNumEdit.GetAsInteger: LongInt;
begin
  Result := Trunc(Value);
end;

procedure TCustomNumEdit.SetAsInteger(AValue: LongInt);
begin
  SetValue(AValue);
end;

procedure TCustomNumEdit.SetMinValue(AValue: Extended);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    Value := FValue;
  end;
end;

procedure TCustomNumEdit.SetMaxValue(AValue: Extended);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    Value := FValue;
  end;
end;

function TCustomNumEdit.GetText: string;
begin
  Result := inherited Text;
end;

function TCustomNumEdit.TextToValText(const AValue: string): string;
begin
  Result := DelRSpace(AValue);
  if {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator <> {$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator then
  begin
    Result := DelChars(Result, {$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator);
  end;
  if ({$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator <> '.') and ({$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator);
  if ({$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator <> ',') and ({$IFDEF RX_D15}FormatSettings.{$ENDIF}ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator);
  if Result = '' then
    Result := '0'
  else if Result = '-' then
    Result := '-0';
end;

procedure TCustomNumEdit.SetText(const AValue: string);
begin
  if not (csReading in ComponentState) then
  begin
    FValue := CheckValue(StrToFloat(TextToValText(AValue)), False);
    DataChanged;
    Invalidate;
  end;
end;

procedure TCustomNumEdit.ReformatEditText;
var
  S: string;
  IsEmpty: Boolean;
  OldLen, SelStart, SelStop: Integer;
begin
  FFormatting := True;
  try
    S := inherited Text;
    OldLen := Length(S);
    IsEmpty := (OldLen = 0) or (S = '-');
    if HandleAllocated then GetSel(SelStart, SelStop);
    if not IsEmpty then S := TextToValText(S);
    S := FormatFloatStr(S, Pos(',', DisplayFormat) > 0);
    inherited Text := S;
    if HandleAllocated and (GetFocus = Handle) and not
      (csDesigning in ComponentState) then
    begin
      Inc(SelStart, Length(S) - OldLen);
      SetCursor(SelStart);
    end;
  finally
    FFormatting := False;
  end;
end;

procedure TCustomNumEdit.Change;
begin
  if not FFormatting then
  begin
    if FFormatOnEditing and FFocused then ReformatEditText;
    inherited Change;
  end;
end;

{$IFNDEF VER80}

procedure TCustomNumEdit.AcceptValue(const Value: Variant);
{$ELSE}

procedure TCustomNumEdit.AcceptValue(const Value: string);
{$ENDIF}
begin
  inherited AcceptValue(Value);
  Self.Value := CheckValue(Value, False); //Polaris
end;

procedure TCustomNumEdit.WMPaste(var Message: TMessage);
var
  S: string;
begin
  S := EditText;
  try
    inherited;
    UpdateData;
  except
    EditText := S;
    SelectAll;
    if CanFocus then SetFocus;
    if BeepOnError then MessageBeep(0);
  end;
end;

procedure TCustomNumEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  if FFormatOnEditing then ReformatEditText;
  inherited;
end;

procedure TCustomNumEdit.CMExit(var Message: TCMExit);
begin
  try
    CheckRange;
    UpdateData;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  SetCursor(0);
  DoExit;
end;

procedure TCustomNumEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if NewStyleControls and not FFocused then Invalidate;
end;

procedure TCustomNumEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if PopupVisible then
    S := TPopupWindow(FPopup).GetPopupText
  else
    S := GetDisplayText;
  if not PaintComboEdit(Self, S, FAlignment, FFocused and not PopupVisible,
    FCanvas, Message) then inherited;
end;

procedure TCustomNumEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{ TCurrencyEdit }

constructor TCurrencyEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
    ButtonWidth := 0;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

function TCurrencyEdit.DefaultDisplayFormat: string;
var
  CurrStr: string;
  I: Integer;
  C: Char;
begin
  Result := ',0.' + MakeStr('0', {$IFDEF RX_D15}FormatSettings.{$ENDIF}CurrencyDecimals);
  CurrStr := '';
  for I := 1 to Length({$IFDEF RX_D15}FormatSettings.{$ENDIF}CurrencyString) do
  begin
    C := {$IFDEF RX_D15}FormatSettings.{$ENDIF}CurrencyString[I];
    if CharInSet(C, [',', '.']) then
      CurrStr := CurrStr + '''' + C + ''''
    else
      CurrStr := CurrStr + C;
  end;
  if Length(CurrStr) > 0 then
    case {$IFDEF RX_D15}FormatSettings.{$ENDIF}CurrencyFormat of
      0: Result := CurrStr + Result; { '$1' }
      1: Result := Result + CurrStr; { '1$' }
      2: Result := CurrStr + ' ' + Result; { '$ 1' }
      3: Result := Result + ' ' + CurrStr; { '1 $' }
    end;
  Result := Format('%s;-%s', [Result, Result]);
end;

{ TRxCustomCalcEdit }

constructor TRxCustomCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCreating];
  try
    FPopup := TPopupWindow(CreatePopupCalculator(Self
      {$IFDEF RX_D4}, BiDiMode{$ENDIF}));
    TPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    UpdatePopup;
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

procedure DestroyLocals; far;
begin
  CalcBitmap.Free;
  CalcBitmap := nil;
end;

{$IFNDEF VER80}
initialization
finalization
  DestroyLocals;
  {$ELSE}
initialization
  AddExitProc(DestroyLocals);
  {$ENDIF}
end.

