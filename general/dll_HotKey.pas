unit dll_HotKey;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

{$R-,T-,H+,X+}

interface

uses
   Winapi.Messages,
   Winapi.Windows,
   Winapi.CommCtrl,
   System.SysUtils,
   System.Classes,
   Vcl.ComCtrls,
   Vcl.Controls,
   Vcl.Consts,
   Vcl.ActnList,
   Vcl.StdActns;


{ THotKey }

  {
  THKModifier = (hkShift, hkCtrl, hkAlt, hkExt);
  THKModifiers = set of THKModifier;
  THKInvalidKey = (hcNone, hcShift, hcCtrl, hcAlt, hcShiftCtrl,
    hcShiftAlt, hcCtrlAlt, hcShiftCtrlAlt);
  THKInvalidKeys = set of THKInvalidKey;
  }

type
  TKNTCustomHotKey = class(TWinControl)
  private
    FAutoSize: Boolean;
    FModifiers: THKModifiers;
    FInvalidKeys: THKInvalidKeys;
    FHotKey: Word;
    FOnChange: TNotifyEvent;
    procedure AdjustHeight;
    procedure SetAutoSize(Value: Boolean);
    procedure SetInvalidKeys(Value: THKInvalidKeys);
    procedure SetModifiers(Value: THKModifiers);
    procedure UpdateHeight;
    function GetHotKey: TShortCut;
    procedure SetHotKey(Value: TShortCut);
    procedure ShortCutToHotKey(Value: TShortCut);
    function HotKeyToShortCut(Value: Longint): TShortCut;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property InvalidKeys: THKInvalidKeys read FInvalidKeys write SetInvalidKeys;
    property Modifiers: THKModifiers read FModifiers write SetModifiers;
    property HotKey: TShortCut read GetHotKey write SetHotKey;
    property TabStop default True;
    procedure Change; dynamic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Text;
    constructor Create(AOwner: TComponent); override;
  end;

  TKNTHotKey = class(TKNTCustomHotKey)
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property Enabled;
    property Hint;
    property HotKey;
    property InvalidKeys;
    property Modifiers;
    property ParentBiDiMode;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

procedure Register;

implementation

{ THotKey }

constructor TKNTCustomHotKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Height := 25;
  TabStop := True;
  ParentColor := False;
  FAutoSize := True;
  FInvalidKeys := [hcNone, hcShift];
  FModifiers := [hkAlt];
  FHotKey := $0041;     // default - 'Alt+A'
  AdjustHeight;
end;

procedure TKNTCustomHotKey.CreateParams(var Params: TCreateParams);
begin
  InitCommonControl(ICC_HOTKEY_CLASS);
  inherited CreateParams(Params);
  CreateSubClass(Params, HOTKEYCLASS);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TKNTCustomHotKey.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, HKM_SETRULES, Byte(FInvalidKeys), MakeLong(Byte(FModifiers), 0));
  SendMessage(Handle, HKM_SETHOTKEY, MakeWord(Byte(FHotKey), Byte(FModifiers)), 0);
end;

procedure TKNTCustomHotKey.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    UpdateHeight;
  end;
end;

procedure TKNTCustomHotKey.SetModifiers(Value: THKModifiers);
begin
  if Value <> FModifiers then
  begin
    FModifiers := Value;
    SendMessage(Handle, HKM_SETRULES, Byte(FInvalidKeys), MakeLong(Byte(Value), 0));
    SendMessage(Handle, HKM_SETHOTKEY, MakeWord(Byte(FHotKey), Byte(FModifiers)), 0);
  end;
end;

procedure TKNTCustomHotKey.SetInvalidKeys(Value: THKInvalidKeys);
begin
  if Value <> FInvalidKeys then
  begin
    FInvalidKeys := Value;
    SendMessage(Handle, HKM_SETRULES, Byte(Value), MakeLong(Byte(FModifiers), 0));
    SendMessage(Handle, HKM_SETHOTKEY, MakeWord(Byte(FHotKey), Byte(FModifiers)), 0);
  end;
end;

function TKNTCustomHotKey.GetHotKey: TShortCut;
var
  HK: Longint;
begin
  HK := SendMessage(Handle, HKM_GETHOTKEY, 0, 0);
  Result := HotKeyToShortCut(HK);
end;

procedure TKNTCustomHotKey.SetHotKey(Value: TShortCut);
begin
  ShortCutToHotKey(Value);
  SendMessage(Handle, HKM_SETHOTKEY, MakeWord(Byte(FHotKey), Byte(FModifiers)), 0);
end;

procedure TKNTCustomHotKey.UpdateHeight;
begin
  if AutoSize then
  begin
    ControlStyle := ControlStyle + [csFixedHeight];
    AdjustHeight;
  end else
    ControlStyle := ControlStyle - [csFixedHeight];
end;

procedure TKNTCustomHotKey.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
    if Ctl3D then I := 8 else I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
end;

procedure TKNTCustomHotKey.ShortCutToHotKey(Value: TShortCut);
begin
  FHotKey := Value and not (scShift + scCtrl + scAlt);
  FModifiers := [];
  if Value and scShift <> 0 then Include(FModifiers, hkShift);
  if Value and scCtrl <> 0 then Include(FModifiers, hkCtrl);
  if Value and scAlt <> 0 then Include(FModifiers, hkAlt);
end;

function TKNTCustomHotKey.HotKeyToShortCut(Value: Longint): TShortCut;
begin
  Byte(FModifiers) := LoWord(HiByte(Value));
  FHotKey := LoWord(LoByte(Value));
  Result := FHotKey;
  if hkShift in FModifiers then Inc(Result, scShift);
  if hkCtrl in FModifiers then Inc(Result, scCtrl);
  if hkAlt in FModifiers then Inc(Result, scAlt);
end;


procedure TKNTCustomHotKey.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if not HandleAllocated or (GetWindowLong(Handle, GWL_STYLE) and
    ES_MULTILINE <> 0) then Change;
end;

procedure TKNTCustomHotKey.Change;
begin
  inherited Changed;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure Register;
begin
  RegisterComponents('Custom', [TKNTHotKey]);
end;



end.
