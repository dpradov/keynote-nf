{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxSpin;

interface

{$I RX.INC}
//>Polaris
{$DEFINE POLESPIN} {Classic style in RxSpinButton and rxSpinEdit}
//<Polaris

uses
  {$IFNDEF VER80}Windows, ComCtrls, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  Controls, ExtCtrls, Classes, Graphics, Messages, Forms, StdCtrls, Menus,
  SysUtils, Mask;

type

{ TRxSpinButton }

  TSpinButtonState = (sbNotDown, sbTopDown, sbBottomDown);
//>Polaris
  TrSpinButtonStyle = (sbsDefault, sbsClassic);
//<Polaris
  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxSpinButton = class(TGraphicControl)
  private
    FDown: TSpinButtonState;
    FUpBitmap: TBitmap;
    FDownBitmap: TBitmap;
    FDragging: Boolean;
    FInvalidate: Boolean;
    FTopDownBtn: TBitmap;
    FBottomDownBtn: TBitmap;
    FRepeatTimer: TTimer;
    FNotDownBtn: TBitmap;
    FLastDown: TSpinButtonState;
    FFocusControl: TWinControl;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
//>Polaris
    FButtonStyle: TrSpinButtonStyle;
    procedure SetButtonStyle(Value: TrSpinButtonStyle);
//<Polaris
    procedure TopClick;
    procedure BottomClick;
    procedure GlyphChanged(Sender: TObject);
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    procedure SetDown(Value: TSpinButtonState);
    procedure SetFocusControl(Value: TWinControl);
    procedure DrawAllBitmap;
    procedure DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
    procedure TimerExpired(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down: TSpinButtonState read FDown write SetDown default sbNotDown;
  published
    property Align;
//>Polaris
    property ButtonStyle: TrSpinButtonStyle read FButtonStyle write SetButtonStyle default sbsDefault;
//<Polaris
    property DragCursor;
    property DragMode;
    property Enabled;
    property Visible;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowHint;
    property ParentShowHint;
    {$IFDEF RX_D4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

{ TRxCustomSpinEdit }

  {$IFDEF CBUILDER}
  TValueType = (vtInt, vtFloat, vtHex);
  {$ELSE}
  TValueType = (vtInteger, vtFloat, vtHex);
  {$ENDIF}

//>Polaris
{.$IFNDEF VER80
  TSpinButtonKind = (bkStandard, bkDiagonal);
$ENDIF}
  TSpinButtonKind = ({$IFNDEF VER80}bkStandard, {$ENDIF}bkDiagonal, bkClassic);

//Polaris  TRxSpinEdit = class(TCustomEdit)
  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCustomSpinEdit = class(TCustomMaskEdit)
  private
//Polaris
    FFocused,

    FCheckOnExit,
      FLCheckMinValue,
      FLCheckMaxValue,

    FCheckMinValue,
      FCheckMaxValue: Boolean;
    FDisplayFormat: string;
//Polaris
    FAlignment: TAlignment;
    FMinValue: Extended;
    FMaxValue: Extended;
    FIncrement: Extended;
    FDecimal: Byte;
    FChanging: Boolean;
    FEditorEnabled: Boolean;
    FValueType: TValueType;
    FButton: TRxSpinButton;
    FBtnWindow: TWinControl;
    FArrowKeys: Boolean;
    FOnTopClick: TNotifyEvent;
    FOnBottomClick: TNotifyEvent;
    {$IFNDEF VER80}
//    FButtonKind: TSpinButtonKind;
    FUpDown: TCustomUpDown;
//Polaris
    procedure SetMinValue(NewValue: Extended);
    procedure SetMaxValue(NewValue: Extended);
    procedure SetCheckMinValue(NewValue: Boolean);
    function StoreCheckMinValue: Boolean;
    procedure SetCheckMaxValue(NewValue: Boolean);
    function StoreCheckMaxValue: Boolean;

    function CheckDefaultRange(CheckMax: Boolean): Boolean;
    procedure SetDisplayFormat(const Value: string);
    function IsFormatStored: Boolean;
    function TextToValText(const AValue: string): string;
//Polaris
    procedure SetFocused(Value: Boolean);
    procedure CheckRange;
    function GetButtonKind: TSpinButtonKind;
    procedure SetButtonKind(Value: TSpinButtonKind);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    {$ENDIF}
    function GetMinHeight: Integer;
    procedure GetTextHeight(var SysHeight, Height: Integer);
    function GetAsInteger: LongInt;
    function IsIncrementStored: Boolean;
    function IsMaxStored: Boolean;
    function IsMinStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAsInteger(NewValue: LongInt);
    procedure SetDecimal(NewValue: Byte);
    function GetButtonWidth: Integer;

//    procedure RecreateButton;

    procedure ResizeButton;
    procedure SetEditRect;
    procedure SetAlignment(Value: TAlignment);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$IFDEF RX_D4}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    {$ENDIF}
  protected
//Polaris up to protected
    {$IFNDEF VER80}
    FButtonKind: TSpinButtonKind;
    {$ENDIF}
    procedure RecreateButton;
    function CheckValue(NewValue: Extended): Extended;
    function CheckValueRange(NewValue: Extended; RaiseOnError: Boolean): Extended;
    procedure SetValue(NewValue: Extended); virtual; abstract;
    function GetValue: Extended; virtual; abstract;
    procedure SetValueType(NewType: TValueType); virtual;
    procedure DataChanged; virtual;
//Polaris up to protected

//Polaris
    procedure Loaded; override;
    function DefaultDisplayFormat: string; virtual;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat
      stored IsFormatStored;
//    procedure DefinePropertyes(Filer: TFiler); override;
//Polaris

    procedure Change; override;
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick(Sender: TObject); virtual;
    procedure DownClick(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    property ButtonWidth: Integer read GetButtonWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsInteger: LongInt read GetAsInteger write SetAsInteger default 0;
    property Text;
//Polaris  published
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    {$IFNDEF VER80}
    property ButtonKind: TSpinButtonKind read FButtonKind write SetButtonKind
//  {$IFDEF POLESPIN}
//      default bkClassic
//  {$ELSE}
    default bkDiagonal
//  {$ENDIF}
    ;
    {$ENDIF}
    property Decimal: Byte read FDecimal write SetDecimal default 2;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Increment: Extended read FIncrement write FIncrement stored IsIncrementStored;
    property MaxValue: Extended read FMaxValue write SetMaxValue stored IsMaxStored;
    property MinValue: Extended read FMinValue write SetMinValue stored IsMinStored;
//Polaris
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property CheckMinValue: Boolean read FCheckMinValue write SetCheckMinValue stored StoreCheckMinValue; //default False;
    property CheckMaxValue: Boolean read FCheckMaxValue write SetCheckMaxValue stored StoreCheckMaxValue; //default False;
//Polaris
    property ValueType: TValueType read FValueType write SetValueType
      default{$IFDEF CBUILDER}vtInt{$ELSE}vtInteger{$ENDIF};
    property Value: Extended read GetValue write SetValue stored IsValueStored;
    property OnBottomClick: TNotifyEvent read FOnBottomClick write FOnBottomClick;
    property OnTopClick: TNotifyEvent read FOnTopClick write FOnTopClick;
  end;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxSpinEdit = class(TRxCustomSpinEdit)
  protected
    procedure SetValue(NewValue: Extended); override;
    function GetValue: Extended; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
//Polaris

    property CheckOnExit;
    property CheckMinValue;
    property CheckMaxValue;

    property Align;
    property Alignment;
    property ArrowKeys;
    property DisplayFormat;
    {$IFNDEF VER80}
    property ButtonKind default bkDiagonal;
    {$ENDIF}
    property Decimal;
    property EditorEnabled;
    property Increment;
    property MaxValue;
    property MinValue;
    property ValueType;
    property Value;
    property OnBottomClick;
    property OnTopClick;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
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
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
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
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
  end;

  {  TRxTimeEdit  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxTimeEdit = class(TCustomPanel) //by Craig Manley
  private
    FSpinEditHours: TRxSpinEdit;
    FSpinEditMinutes: TRxSpinEdit;
    FSpinEditSeconds: TRxSpinEdit;
    FLabelHours: TLabel;
    FLabelMinutes: TLabel;
    FLabelSeconds: TLabel;
    procedure WMSetTextProc(var Msg: TWMSetText); message WM_SETTEXT;
    procedure CMTextChangedProc(var Msg: TMessage); message CM_TEXTCHANGED;
  protected
    function GetTime: TDateTime;
    procedure SetTime(const AValue: TDateTime);
    function GetHours: Byte;
    procedure SetHours(const AValue: Byte);
    function GetMinutes: Byte;
    procedure SetMinutes(const AValue: Byte);
    function GetSeconds: Byte;
    procedure SetSeconds(const AValue: Byte);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Hint;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager;
    property Visible;
    property Time: TDateTime read GetTime write SetTime;
    property Hours: Byte read GetHours write SetHours default 0;
    property Minutes: Byte read GetMinutes write SetMinutes default 0;
    property Seconds: Byte read GetSeconds write SetSeconds default 0;
  end;

implementation

uses
  {$IFNDEF VER80}CommCtrl, {$ENDIF}RxVCLUtils, RxStrUtils, Consts, //Polaris
  RxResConst;

{$R *.RES}

const
  sSpinUpBtn = 'RXSPINUP';
  sSpinDownBtn = 'RXSPINDOWN';

const
  InitRepeatPause = 400; { pause before repeat timer (ms) }
  RepeatPause = 100;

{ TRxSpinButton }

constructor TRxSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF POLESPIN}
  FButtonStyle := sbsDefault;
  {$ENDIF}
  FUpBitmap := TBitmap.Create;
  FDownBitmap := TBitmap.Create;
  FUpBitmap.OnChange := GlyphChanged;
  FDownBitmap.OnChange := GlyphChanged;
  Height := 20;
  Width := 20;
  FTopDownBtn := TBitmap.Create;
  FBottomDownBtn := TBitmap.Create;
  FNotDownBtn := TBitmap.Create;
  DrawAllBitmap;
  FLastDown := sbNotDown;
end;

destructor TRxSpinButton.Destroy;
begin
  FTopDownBtn.Free;
  FBottomDownBtn.Free;
  FNotDownBtn.Free;
  FUpBitmap.Free;
  FDownBitmap.Free;
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TRxSpinButton.GlyphChanged(Sender: TObject);
begin
  FInvalidate := True;
  Invalidate;
end;

function TRxSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpBitmap;
end;

procedure TRxSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpBitmap.Assign(Value)
//  else FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
//>Polaris
  else
    FUpBitmap.Handle := 0;
end;

function TRxSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownBitmap;
end;

procedure TRxSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownBitmap.Assign(Value)
//  else FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);
//Polaris
  else
    FDownBitmap.Handle := 0;
end;

procedure TRxSpinButton.SetDown(Value: TSpinButtonState);
var
  OldState: TSpinButtonState;
begin
  OldState := FDown;
  FDown := Value;
  if OldState <> FDown then Repaint;
end;

procedure TRxSpinButton.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
end;

procedure TRxSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TRxSpinButton.Paint;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  if (FNotDownBtn.Height <> Height) or (FNotDownBtn.Width <> Width) or
    FInvalidate then DrawAllBitmap;
  FInvalidate := False;
  with Canvas do
    case FDown of
      sbNotDown: Draw(0, 0, FNotDownBtn);
      sbTopDown: Draw(0, 0, FTopDownBtn);
      sbBottomDown: Draw(0, 0, FBottomDownBtn);
    end;
end;

procedure TRxSpinButton.DrawAllBitmap;
begin
  DrawBitmap(FTopDownBtn, sbTopDown);
  DrawBitmap(FBottomDownBtn, sbBottomDown);
  DrawBitmap(FNotDownBtn, sbNotDown);
end;

(*Polaris
procedure TRxSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
var
  R, RSrc: TRect;
  dRect: Integer;
  {Temp: TBitmap;}
begin
  ABitmap.Height := Height;
  ABitmap.Width := Width;
  with ABitmap.Canvas do begin
    R := Bounds(0, 0, Width, Height);
    Pen.Width := 1;
    Brush.Color := clBtnFace;
    Brush.Style := bsSolid;
    FillRect(R);
    { buttons frame }
    Pen.Color := clWindowFrame;
    Rectangle(0, 0, Width, Height);
    MoveTo(-1, Height);
    LineTo(Width, -1);
    { top button }
    if ADownState = sbTopDown then Pen.Color := clBtnShadow
    else Pen.Color := clBtnHighlight;
    MoveTo(1, Height - 4);
    LineTo(1, 1);
    LineTo(Width - 3, 1);
    if ADownState = sbTopDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    if ADownState <> sbTopDown then begin
      MoveTo(1, Height - 3);
      LineTo(Width - 2, 0);
    end;
    { bottom button }
    if ADownState = sbBottomDown then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
    MoveTo(2, Height - 2);
    LineTo(Width - 2, Height - 2);
    LineTo(Width - 2, 1);
    if ADownState = sbBottomDown then Pen.Color := clBtnShadow
      else Pen.Color := clBtnHighlight;
    MoveTo(2, Height - 2);
    LineTo(Width - 1, 1);
    { top glyph }
    dRect := 1;
    if ADownState = sbTopDown then Inc(dRect);
    R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + dRect,
      Round((Height / 4) - (FUpBitmap.Height / 2)) + dRect, FUpBitmap.Width,
      FUpBitmap.Height);
    RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FUpBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
    { bottom glyph }
    R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
      Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
      FDownBitmap.Width, FDownBitmap.Height);
    RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
    {
    if Self.Enabled or (csDesigning in ComponentState) then
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor)
    else begin
      Temp := CreateDisabledBitmap(FDownBitmap, clBlack);
      try
        BrushCopy(R, Temp, RSrc, Temp.TransparentColor);
      finally
        Temp.Free;
      end;
    end;
    }
    BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
    if ADownState = sbBottomDown then begin
      Pen.Color := clBtnShadow;
      MoveTo(3, Height - 2);
      LineTo(Width - 1, 2);
    end;
  end;
end;
*)
type
  TColorArray = array[0..2] of TColor;

procedure TRxSpinButton.DrawBitmap(ABitmap: TBitmap; ADownState: TSpinButtonState);
const
  CColors: TColorArray = (clBtnShadow, clBtnHighlight, clWindowFrame {clBtnFace});

var
  R, RSrc: TRect;
  dRect: Integer;
  Flags: array[0..1] of DWord;
  LColors: TColorArray;
  LGlyph: array[0..1] of Boolean;
  {Temp: TBitmap;}

  procedure RxDraw;
  begin
    { buttons }
    with ABitmap.Canvas do
    begin
      LColors := CColors;
      if ADownState = sbTopDown then
      begin
        LColors[0] := clBtnFace;
        LColors[2] := clBtnHighlight;
        Flags[0] := EDGE_SUNKEN;
      end;
      if ADownState = sbBottomDown then
      begin
        LColors[1] := clWindowFrame;
        LColors[2] := clBtnShadow;
        Flags[1] := EDGE_SUNKEN;
      end;
      DrawEdge(Handle, R, Flags[0], BF_TOPLEFT or BF_SOFT);
      DrawEdge(Handle, R, Flags[1], BF_BOTTOMRIGHT or BF_SOFT);
      InflateRect(R, -1, -1);

      Pen.Color := LColors[0];
      MoveTo(R.Left, R.Bottom - 2);
      LineTo(R.Right - 1, R.Top - 1);

      Pen.Color := LColors[2];
      MoveTo(R.Right - 1, R.Top);
      LineTo(R.Right - 1, R.Top);
      LineTo(R.Left, R.Bottom - 1);

      Pen.Color := LColors[1];
      MoveTo(R.Left + 1, R.Bottom - 1);
      LineTo(R.Right, R.Top);

      { top glyph }
      dRect := 1;
      if ADownState = sbTopDown then Inc(dRect);

      if LGlyph[0] then FUpBitmap.Handle := LoadBitmap(HInstance, sSpinUpBtn);
      if LGlyph[1] then FDownBitmap.Handle := LoadBitmap(HInstance, sSpinDownBtn);

      R := Bounds(Round((Width / 4) - (FUpBitmap.Width / 2)) + dRect,
        Round((Height / 4) - (FUpBitmap.Height / 2)) + dRect, FUpBitmap.Width,
        FUpBitmap.Height);
      RSrc := Bounds(0, 0, FUpBitmap.Width, FUpBitmap.Height);
      BrushCopy(R, FUpBitmap, RSrc, FUpBitmap.TransparentColor);
      { bottom glyph }
      R := Bounds(Round((3 * Width / 4) - (FDownBitmap.Width / 2)) - 1,
        Round((3 * Height / 4) - (FDownBitmap.Height / 2)) - 1,
        FDownBitmap.Width, FDownBitmap.Height);
      RSrc := Bounds(0, 0, FDownBitmap.Width, FDownBitmap.Height);
      BrushCopy(R, FDownBitmap, RSrc, FDownBitmap.TransparentColor);
      FUpBitmap.Handle := 0;
      FDownBitmap.Handle := 0;
    end;
  end;

  {$IFDEF POLESPIN}

  procedure PoleDraw;
  var
    X, Y, I, J, H: Integer;
    R1: TRect;
  begin
    with ABitmap.Canvas do
    begin
      { top glyph }
      H := Height div 2;
      R := Bounds(0, 0, Width, H);
      if ADownState = sbTopDown then
        Flags[0] := EDGE_SUNKEN
      else
        R.Bottom := R.Bottom + 1;
      if ADownState = sbBottomDown then Flags[1] := EDGE_SUNKEN;
      if LGlyph[0] then FUpBitmap.Handle := LoadBitmap(HInstance, 'RSPINUP');
      RSrc := R;
      DrawEdge(Handle, R, Flags[0], BF_RECT or BF_SOFT or BF_ADJUST);
      R1 := Bounds(0, H, Width, Height);
      R1.Bottom := Height;
      DrawEdge(Handle, R1, Flags[1], BF_RECT or BF_SOFT or BF_ADJUST);
      I := R.Bottom - R.Top - 1;
      J := R1.Bottom - R1.Top - 1;
      Y := RSrc.Top + (H - FUpBitmap.Height) div 2;
//      if I >= (J+1) then
      if (ADownState = sbTopDown) then OffsetRect(R1, 0, 1);

      R1.Bottom := R1.Top + I;
      if J - FUpBitmap.Height < 0 then Y := R.Top;
      {Glyph}
      FUpBitmap.Transparent := True;
      X := (Width - FUpBitmap.Width) div 2;
      IntersectClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
      Draw(X, Y, FUpBitmap);
      SelectClipRgn(Handle, 0);
      RSrc := Bounds(0, H, Width, Height);
      RSrc.Bottom := Height;
      if LGlyph[1] then FDownBitmap.Handle := LoadBitmap(HInstance, 'RSPINDOWN');
      FDownBitmap.Transparent := True;
      X := (Width - FDownBitmap.Width) div 2;
      Y := R1.Top + (I - FDownBitmap.Height) div 2;
      if I - FDownBitmap.Height < 0 then
      begin
        Dec(R1.Top);
        Y := R1.Bottom - FDownBitmap.Height
      end;
      IntersectClipRect(Handle, R1.Left, R1.Top, R1.Right, R1.Bottom);
      Draw(X,
        Y,
        FDownBitmap);
      SelectClipRgn(Handle, 0);
    end;
  end;
  {$ENDIF}
begin
  LGlyph[0] := FUpBitmap.Handle = 0;
  LGlyph[1] := FDownBitmap.Handle = 0;
  try
    ABitmap.Height := Height;
    ABitmap.Width := Width;
    FillChar(Flags, SizeOf(Flags), EDGE_RAISED);
    with ABitmap.Canvas do
    begin
      R := Bounds(0, 0, Width, Height);
      Pen.Width := 1;
      Brush.Color := clBtnFace;
      Brush.Style := bsSolid;
      FillRect(R);
    end;
    {$IFDEF POLESPIN}
    if FButtonStyle = sbsClassic then
      PoleDraw
    else
      {$ENDIF}
      RxDraw;
  finally
    if LGlyph[0] then FUpBitmap.Handle := 0;
    if LGlyph[1] then FDownBitmap.Handle := 0;
  end;
end;

procedure TRxSpinButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
//>Polaris
//  FInvalidate := True;
//  Invalidate;
  GlyphChanged(Self);
//<Polaris
end;

//>Polaris

procedure TRxSpinButton.SetButtonStyle(Value: TrSpinButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    GlyphChanged(Self);
  end;
end;
//<Polaris

procedure TRxSpinButton.TopClick;
begin
  if Assigned(FOnTopClick) then
  begin
    FOnTopClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TRxSpinButton.BottomClick;
begin
  if Assigned(FOnBottomClick) then
  begin
    FOnBottomClick(Self);
    if not (csLButtonDown in ControlState) then FDown := sbNotDown;
  end;
end;

procedure TRxSpinButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if (FFocusControl <> nil) and FFocusControl.TabStop and
      FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus;
    if FDown = sbNotDown then
    begin
      FLastDown := FDown;
//>Polaris
      {$IFNDEF POLESPIN}
      if Y > (-(Height / Width) * X + Height) then
      begin
        {$ELSE}
      if ((FButtonStyle = sbsDefault) and (Y > (-(Height / Width) * X + Height))) or
        ((FButtonStyle = sbsClassic) and (Y > (Height div 2))) then
      begin
        {$ENDIF}
        FDown := sbBottomDown;
        BottomClick;
      end
      else
      begin
        FDown := sbTopDown;
        TopClick;
      end;
      if FLastDown <> FDown then
      begin
        FLastDown := FDown;
        Repaint;
      end;
      if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Interval := InitRepeatPause;
      FRepeatTimer.Enabled := True;
    end;
    FDragging := True;
  end;
end;

procedure TRxSpinButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TSpinButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then
    begin
      NewState := FDown;
//>Polaris
      {$IFNDEF POLESPIN}
      if Y > (-(Width / Height) * X + Height) then
      begin
        {$ELSE}
      if ((FButtonStyle = sbsDefault)) and (Y > (-(Width / Height) * X + Height)) or
        ((FButtonStyle = sbsClassic) and (Y > (Height div 2))) then
      begin
        {$ENDIF}
        if (FDown <> sbBottomDown) then
        begin
          if FLastDown = sbBottomDown then
            FDown := sbBottomDown
          else
            FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end
      else
      begin
        if (FDown <> sbTopDown) then
        begin
          if (FLastDown = sbTopDown) then
            FDown := sbTopDown
          else
            FDown := sbNotDown;
          if NewState <> FDown then Repaint;
        end;
      end;
    end
    else if FDown <> sbNotDown then
    begin
      FDown := sbNotDown;
      Repaint;
    end;
  end;
end;

procedure TRxSpinButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) then
    begin
      FDown := sbNotDown;
      FLastDown := sbNotDown;
      Repaint;
    end;
  end;
end;

procedure TRxSpinButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown <> sbNotDown) and MouseCapture then
  begin
    try
      if FDown = sbBottomDown then
        BottomClick
      else
        TopClick;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

function DefBtnWidth: Integer;
begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
  if Result > 15 then Result := 15;
end;

{$IFNDEF VER80}

type
  TRxUpDown = class(TCustomUpDown)
  private
    FChanging: Boolean;
    procedure ScrollMessage(var Message: TWMVScroll);
    procedure WMHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
  end;

constructor TRxUpDown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Orientation := udVertical;
  Min := -1;
  Max := 1;
  Position := 0;
end;

destructor TRxUpDown.Destroy;
begin
  OnClick := nil;
  inherited Destroy;
end;

procedure TRxUpDown.ScrollMessage(var Message: TWMVScroll);
begin
  if Message.ScrollCode = SB_THUMBPOSITION then
  begin
    if not FChanging then
    begin
      FChanging := True;
      try
        if Message.Pos > 0 then
          Click(btNext)
        else if Message.Pos < 0 then
          Click(btPrev);
        if HandleAllocated then
          SendMessage(Handle, UDM_SETPOS, 0, 0);
      finally
        FChanging := False;
      end;
    end;
  end;
end;

procedure TRxUpDown.WMHScroll(var Message: TWMHScroll);
begin
  ScrollMessage(TWMVScroll(Message));
end;

procedure TRxUpDown.WMVScroll(var Message: TWMVScroll);
begin
  ScrollMessage(Message);
end;

procedure TRxUpDown.WMSize(var Message: TWMSize);
begin
  inherited;
  if Width <> DefBtnWidth then Width := DefBtnWidth;
end;
{$ENDIF}

{ TRxCustomSpinEdit }

constructor TRxCustomSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//Polaris
  FFocused := False;

  FCheckOnExit := False;
  FLCheckMinValue := True;
  FLCheckMaxValue := True;
  FCheckMinValue := False;
  FCheckMaxValue := False;
//Polaris
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1.0;
  FDecimal := 2;
  FEditorEnabled := True;
  {$IFNDEF VER80}
  FButtonKind := bkDiagonal;
(*
  {$IFDEF POLESPIN}
  FButtonKind := bkClassic;
  {$ELSE}
  FButtonKind := bkDiagonal;
  {$ENDIF}
*)
  {$ENDIF}
  FArrowKeys := True;
  RecreateButton;
end;

destructor TRxCustomSpinEdit.Destroy;
begin
  Destroying;
  FChanging := True;
  if FButton <> nil then
  begin
    FButton.Free;
    FButton := nil;
    FBtnWindow.Free;
    FBtnWindow := nil;
  end;
  {$IFNDEF VER80}
  if FUpDown <> nil then
  begin
    FUpDown.Free;
    FUpDown := nil;
  end;
  {$ENDIF}
  inherited Destroy;
end;

procedure TRxCustomSpinEdit.RecreateButton;
begin
  if (csDestroying in ComponentState) then Exit;
  FButton.Free;
  FButton := nil;
  FBtnWindow.Free;
  FBtnWindow := nil;
  {$IFNDEF VER80}
  FUpDown.Free;
  FUpDown := nil;
  if GetButtonKind = bkStandard then
  begin
    FUpDown := TRxUpDown.Create(Self);
    with TRxUpDown(FUpDown) do
    begin
      Visible := True;
//Polaris
      SetBounds(0, 1, DefBtnWidth, Self.Height);
      {$IFDEF RX_D4}
      if (BiDiMode = bdRightToLeft) then
        Align := alLeft
      else
        {$ENDIF}
        Align := alRight;
      Parent := Self;
      OnClick := UpDownClick;
    end;
  end
  else
  begin
    {$ENDIF}
    FBtnWindow := TWinControl.Create(Self);
    FBtnWindow.Visible := True;
    FBtnWindow.Parent := Self;
    {$IFDEF POLESPIN}
    if FButtonKind <> bkClassic then
      FBtnWindow.SetBounds(0, 0, DefBtnWidth, Height)
    else
      {$ENDIF}
      FBtnWindow.SetBounds(0, 0, Height, Height);

    FButton := TRxSpinButton.Create(Self);
    FButton.Visible := True;
    {$IFDEF POLESPIN}
    if FButtonKind = bkClassic then FButton.FButtonStyle := sbsClassic;
    {$ENDIF}
    FButton.Parent := FBtnWindow;
    FButton.FocusControl := Self;
    FButton.OnTopClick := UpClick;
    FButton.OnBottomClick := DownClick;
//Polaris
    FButton.SetBounds(1, 1, FBtnWindow.Width - 1, FBtnWindow.Height - 1);
    {$IFNDEF VER80}
  end;
  {$ENDIF}
end;

procedure TRxCustomSpinEdit.SetArrowKeys(Value: Boolean);
begin
  FArrowKeys := Value;
  {$IFNDEF VER80}
  ResizeButton;
  {$ENDIF}
end;

{$IFNDEF VER80}

function TRxCustomSpinEdit.GetButtonKind: TSpinButtonKind;
begin
  if NewStyleControls then
    Result := FButtonKind
      {$IFNDEF POLESPIN}
  else
    Result := bkDiagonal;
  {$ELSE}
//>Polaris
  else
  begin
    Result := bkDiagonal;
    if Assigned(FButton) and (FButton.ButtonStyle = sbsClassic) then Result := bkClassic;
  end;
//<Polaris
  {$ENDIF}
end;

procedure TRxCustomSpinEdit.SetButtonKind(Value: TSpinButtonKind);
var
  OldKind: TSpinButtonKind;
begin
  OldKind := FButtonKind;
  FButtonKind := Value;
  if OldKind <> GetButtonKind then
  begin
    RecreateButton;
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TRxCustomSpinEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if TabStop and CanFocus then SetFocus;
  case Button of
    btNext: UpClick(Sender);
    btPrev: DownClick(Sender);
  end;
end;
{$ENDIF}

function TRxCustomSpinEdit.GetButtonWidth: Integer;
begin
  {$IFNDEF VER80}
  if FUpDown <> nil then
    Result := FUpDown.Width
  else
    {$ENDIF}if FButton <> nil then
      Result := FButton.Width
    else
      Result := DefBtnWidth;
end;

procedure TRxCustomSpinEdit.ResizeButton;
{$IFNDEF VER80}
var
  R: TRect;
  {$ENDIF}
begin
  {$IFNDEF VER80}
  if FUpDown <> nil then
  begin
    FUpDown.Width := DefBtnWidth;
    {$IFDEF RX_D4}
    if (BiDiMode = bdRightToLeft) then
      FUpDown.Align := alLeft
    else
      {$ENDIF}
      FUpDown.Align := alRight;
  end
  else if FButton <> nil then
  begin { bkDiagonal }
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
      {$IFDEF POLESPIN}
      if FButtonKind = bkClassic then
        R := Bounds(Width - DefBtnWidth - 4, -1, DefBtnWidth, Height - 3)
      else
        {$ENDIF}
        R := Bounds(Width - Height - 1, -1, Height - 3, Height - 3)
    else
      {$IFDEF POLESPIN}if FButtonKind = bkClassic then
        R := Bounds(Width - DefBtnWidth, 0, DefBtnWidth, Height)
      else
        {$ENDIF}
        R := Bounds(Width - Height, 0, Height, Height);
    {$IFDEF RX_D4}
    if (BiDiMode = bdRightToLeft) then
    begin
      if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then
      begin
        R.Left := -1;
        R.Right := Height - 4;
      end
      else
      begin
        R.Left := 0;
        R.Right := Height;
      end;
    end;
    {$ENDIF}
    with R do
      FBtnWindow.SetBounds(Left, Top, Right - Left, Bottom - Top);
//Polaris
    FButton.SetBounds(1, 1, FBtnWindow.Width - 1, FBtnWindow.Height - 1);
  end;
  {$ELSE}
  if FButton <> nil then
  begin
    {$IFDEF POLESPIN}
//Polaris
    if FButtonKind = bkClassic then
      FBtnWindow.SetBounds(Width - DefBtnWidth, 0, DefBtnWidth, Height)
    else
//Polaris
      {$ENDIF}
      FBtnWindow.SetBounds(Width - Height, 0, Height, Height);
    FButton.SetBounds(1, 1, FBtnWindow.Width - 1, FBtnWindow.Height - 1);
  end;
  {$ENDIF}
end;

procedure TRxCustomSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if ArrowKeys and (Key in [VK_UP, VK_DOWN]) then
  begin
    if Key = VK_UP then
      UpClick(Self)
    else if Key = VK_DOWN then
      DownClick(Self);
    Key := 0;
  end;
end;

procedure TRxCustomSpinEdit.Change;
begin
  if not FChanging then inherited Change;
end;

procedure TRxCustomSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  //Polaris
  if Key = '.' then Key := {$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator;
  if Key <> #0 then
  begin
    inherited KeyPress(Key);
    if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, WPARAM(Key), 0);
      if Key = Char(VK_RETURN) then Key := #0;
    end;
  end;
end;

function TRxCustomSpinEdit.IsValidChar(Key: Char): Boolean;
var
  ValidChars: set of AnsiChar;
begin
  ValidChars := ['+', '-', '0'..'9'];
  if ValueType = vtFloat then
  begin
    if Pos({$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, Text) = 0 then
      ValidChars := ValidChars + [{$IFDEF RX_D15}FormatSettings.{$ENDIF}DecimalSeparator, '.'];
    if Pos('E', AnsiUpperCase(Text)) = 0 then
      ValidChars := ValidChars + ['e', 'E'];
  end
  else if ValueType = vtHex then
  begin
    ValidChars := ValidChars + ['A'..'F', 'a'..'f'];
  end;
  Result := CharInSet(Key, ValidChars) or (Key < #32);
  if not FEditorEnabled and Result and ((Key >= #32) or
    (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then Result := False;
end;

procedure TRxCustomSpinEdit.CreateParams(var Params: TCreateParams);
const
  {$IFDEF RX_D4}
  Alignments: array[Boolean, TAlignment] of DWORD =
  ((ES_LEFT, ES_RIGHT, ES_CENTER), (ES_RIGHT, ES_LEFT, ES_CENTER));
  {$ELSE}
  Alignments: array[TAlignment] of LongInt = (ES_LEFT, ES_RIGHT, ES_CENTER);
  {$ENDIF}
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style
//    or ES_MULTILINE
  or WS_CLIPCHILDREN or
    {$IFDEF RX_D4}
  Alignments[UseRightToLeftAlignment, FAlignment];
  {$ELSE}
  Alignments[FAlignment];
  {$ENDIF}
end;

procedure TRxCustomSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TRxCustomSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
//Polaris
  {$IFDEF RX_D4}
  if (BiDiMode = bdRightToLeft) then
  begin
    SetRect(Loc, GetButtonWidth + 1, 0, ClientWidth - 1,
      ClientHeight + 1);
    SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(GetButtonWidth, 0));
  end
  else
  begin
    {$ENDIF RX_D4}
    SetRect(Loc, 0, 0, ClientWidth - GetButtonWidth - 2, ClientHeight + 1);
    {$IFDEF RX_D4}
    SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, GetButtonWidth));
  end;
  {$ENDIF RX_D4}

  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
end;

procedure TRxCustomSpinEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }
  if Height < MinHeight then
    Height := MinHeight
  else
  begin
    ResizeButton;
    SetEditRect;
  end;
end;

procedure TRxCustomSpinEdit.GetTextHeight(var SysHeight, Height: Integer);
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  SysHeight := SysMetrics.tmHeight;
  Height := Metrics.tmHeight;
end;

function TRxCustomSpinEdit.GetMinHeight: Integer;
var
  I, H: Integer;
begin
  GetTextHeight(I, H);
  if I > H then I := H;
  Result := H + {$IFDEF VER80}(I div 4) + {$ENDIF}
  (GetSystemMetrics(SM_CYBORDER) * 4) + 1;
end;

procedure TRxCustomSpinEdit.UpClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then
    MessageBeep(0)
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value + FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnTopClick) then FOnTopClick(Self);
  end;
end;

procedure TRxCustomSpinEdit.DownClick(Sender: TObject);
var
  OldText: string;
begin
  if ReadOnly then
    MessageBeep(0)
  else
  begin
    FChanging := True;
    try
      OldText := inherited Text;
      Value := Value - FIncrement;
    finally
      FChanging := False;
    end;
    if CompareText(inherited Text, OldText) <> 0 then
    begin
      Modified := True;
      Change;
    end;
    if Assigned(FOnBottomClick) then FOnBottomClick(Self);
  end;
end;

{$IFDEF RX_D4}

procedure TRxCustomSpinEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;
{$ENDIF}

procedure TRxCustomSpinEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TRxCustomSpinEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  ResizeButton;
  SetEditRect;
end;

procedure TRxCustomSpinEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  {$IFNDEF VER80}
  if FUpDown <> nil then
  begin
    FUpDown.Enabled := Enabled;
    ResizeButton;
  end;
  {$ENDIF}
  if FButton <> nil then FButton.Enabled := Enabled;
end;

procedure TRxCustomSpinEdit.WMPaste(var Message: TWMPaste);
var
  V: Extended;
begin
  if not FEditorEnabled or ReadOnly then Exit;
  V := Value;
  inherited;
  try
    StrToFloat(Text);
  except
    SetValue(V);
  end;
end;

procedure TRxCustomSpinEdit.WMCut(var Message: TWMCut);
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

//Polaris

procedure TRxCustomSpinEdit.SetFocused(Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
    DataChanged;
  end;
end;

procedure TRxCustomSpinEdit.CheckRange;
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValueRange(Value, True);
end;

procedure TRxCustomSpinEdit.CMExit(var Message: TCMExit);
begin
  SetFocused(False);
  try
    CheckRange;
    SetValue(CheckValue(Value));
  except
    SetFocused(True);
    SelectAll;
    if CanFocus then SetFocus;
    raise
  end;
  inherited;
end;

procedure TRxCustomSpinEdit.CMEnter(var Message: TMessage);
begin
  SetFocused(True);
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

function TRxCustomSpinEdit.GetAsInteger: LongInt;
begin
  Result := Trunc(GetValue);
end;

procedure TRxCustomSpinEdit.SetAsInteger(NewValue: LongInt);
begin
  SetValue(NewValue);
end;

procedure TRxCustomSpinEdit.SetValueType(NewType: TValueType);
begin
  if FValueType <> NewType then
  begin
    FValueType := NewType;
    Value := GetValue;
    if FValueType in [{$IFDEF CBUILDER}vtInt{$ELSE}vtInteger{$ENDIF}, vtHex] then
    begin
      FIncrement := Round(FIncrement);
      if FIncrement = 0 then FIncrement := 1;
    end;
  end;
end;

function TRxCustomSpinEdit.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 1.0;
end;

function TRxCustomSpinEdit.IsMaxStored: Boolean;
begin
  Result := (MaxValue <> 0.0);
end;

function TRxCustomSpinEdit.IsMinStored: Boolean;
begin
  Result := (MinValue <> 0.0);
end;

function TRxCustomSpinEdit.IsValueStored: Boolean;
begin
  Result := (GetValue <> 0.0);
end;

procedure TRxCustomSpinEdit.SetDecimal(NewValue: Byte);
begin
  if FDecimal <> NewValue then
  begin
    FDecimal := NewValue;
    Value := GetValue;
  end;
end;

//Polaris

function TRxCustomSpinEdit.CheckValueRange(NewValue: Extended; RaiseOnError: Boolean): Extended;
begin
  Result := CheckValue(NewValue);
  if (FCheckMinValue or FCheckMaxValue) and
    RaiseOnError and (Result <> NewValue) then
    raise ERangeError.CreateFmt(ReplaceStr(ResStr(SOutOfRange), '%d', '%g'),
      [FMinValue, FMaxValue]);
end;

function TRxCustomSpinEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
{
  if (FMaxValue <> FMinValue) then begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
}
  if FCheckMinValue or FCheckMaxValue then
  begin
    if FCheckMinValue and (NewValue < FMinValue) then
      Result := FMinValue;
    if FCheckMaxValue and (NewValue > FMaxValue) then
      Result := FMaxValue;
  end;
end;

//Polaris

procedure TRxCustomSpinEdit.Loaded;
begin
  inherited Loaded;
  FLCheckMinValue := True;
  FLCheckMaxValue := True;
end;

function TRxCustomSpinEdit.CheckDefaultRange(CheckMax: Boolean): Boolean;
begin
  Result := (FMinValue <> 0) or (FMaxValue <> 0);
end;

procedure TRxCustomSpinEdit.SetMinValue(NewValue: Extended);
var
  Z,
    B: Boolean;
begin
  if NewValue <> FMinValue then
  begin
    B := not StoreCheckMinValue;
    Z := (FMinValue = 0) <> (NewValue = 0);
    FMinValue := NewValue;
    if Z and FLCheckMinValue then
    begin
      SetCheckMinValue(CheckDefaultRange(False));
      if B and FLCheckMaxValue then SetCheckMaxValue(CheckDefaultRange(True));
    end;
    SetValue(Value);
  end;
end;

procedure TRxCustomSpinEdit.SetMaxValue(NewValue: Extended);
var
  Z,
    B: Boolean;
begin
  if NewValue <> FMaxValue then
  begin
    B := not StoreCheckMaxValue;
    Z := (FMaxValue = 0) <> (NewValue = 0);
    FMaxValue := NewValue;
    if Z and FLCheckMaxValue then
    begin
      SetCheckMaxValue(CheckDefaultRange(True));
      if B and FLCheckMinValue then SetCheckMinValue(CheckDefaultRange(False));
    end;
    SetValue(Value);
  end;
end;

procedure TRxCustomSpinEdit.SetCheckMinValue(NewValue: Boolean);
begin
  if (FMinValue <> 0) then NewValue := True;
  FCheckMinValue := NewValue;
  if (csLoading in ComponentState) then FLCheckMinValue := False;
  SetValue(Value);
end;

procedure TRxCustomSpinEdit.SetCheckMaxValue(NewValue: Boolean);
begin
  if (FMaxValue <> 0) then NewValue := True;
  FCheckMaxValue := NewValue;
  if (csLoading in ComponentState) then FLCheckMaxValue := False;
  SetValue(Value);
end;

function TRxCustomSpinEdit.StoreCheckMinValue: Boolean;
begin
  Result := (FMinValue = 0) and (FCheckMinValue = (FMaxValue = 0));
end;

function TRxCustomSpinEdit.StoreCheckMaxValue: Boolean;
begin
  Result := (FMaxValue = 0) and (FCheckMaxValue = (FMinValue = 0));
end;

//Polaris

function TRxCustomSpinEdit.DefaultDisplayFormat: string;
begin
  Result := ',0.##';
end;

function TRxCustomSpinEdit.IsFormatStored: Boolean;
begin
  Result := (DisplayFormat <> DefaultDisplayFormat);
end;

procedure TRxCustomSpinEdit.SetDisplayFormat(const Value: string);
begin
  if DisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    Invalidate;
  end;
end;

function TRxCustomSpinEdit.TextToValText(const AValue: string): string;
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

procedure TRxCustomSpinEdit.DataChanged;
var
  EditFormat: string;
begin
  if (ValueType = vtFloat) and FFocused and (FDisplayFormat <> EmptyStr) then
  begin
    EditFormat := '0';
    if FDecimal > 0 then
      EditFormat := EditFormat + '.' + MakeStr('#', FDecimal);
    EditText := FormatFloat(EditFormat, Value);
  end;
end;

{ TRxSpinEdit }

constructor TRxSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//{$IFNDEF VER80}
//  FButtonKind := bkDiagonal;
//{$ENDIF}
  Text := '0';
//  RecreateButton;
end;

procedure TRxSpinEdit.SetValue(NewValue: Extended);
begin
  if ValueType = vtFloat then
    if (FDisplayFormat <> EmptyStr) then
      Text := FormatFloat(FDisplayFormat, CheckValue(NewValue))
    else
      Text := FloatToStrF(CheckValue(NewValue), ffFixed, 15, FDecimal)
  else if ValueType = vtHex then
    Text := IntToHex(Round(CheckValue(NewValue)), 1)
  else
    Text := IntToStr(Round(CheckValue(NewValue)));
  DataChanged;
end;

function TRxSpinEdit.GetValue: Extended;
begin
  try
    if ValueType = vtFloat then
    begin
      if FDisplayFormat <> EmptyStr then
      try
        Result := StrToFloat(TextToValText(Text));
      except
        Result := FMinValue;
      end
      else if not TextToFloat(PChar(Text), Result, fvExtended) then
        Result := FMinValue;
    end
    else if ValueType = vtHex then
      Result := StrToIntDef('$' + Text, Round(FMinValue))
    else
      Result := StrToIntDef(Text, Round(FMinValue));
  except
    if ValueType = vtFloat then
      Result := FMinValue
    else
      Result := Round(FMinValue);
  end;
end;

{  TRxTimeEdit  }

constructor TRxTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  FSpinEditHours := TRxSpinEdit.Create(Self);
  FSpinEditHours.Left := 0;
  FSpinEditHours.Width := 41;
  FSpinEditHours.MaxValue := 23;
  FSpinEditHours.MinValue := 0;
  FSpinEditHours.Parent := Self;
  FLabelHours := TLabel.Create(Self);
  FLabelHours.Left := FSpinEditHours.Left + FSpinEditHours.Width;
  FLabelHours.FocusControl := FSpinEditHours;
  FLabelHours.Caption := RxLoadStr(ccTimeEditHours);
  FLabelHours.Parent := Self;
  FSpinEditMinutes := TRxSpinEdit.Create(Self);
  FSpinEditMinutes.Left := FLabelHours.Left + FLabelHours.Width + 4;
  FSpinEditMinutes.Width := 41;
  FSpinEditMinutes.MaxValue := 59;
  FSpinEditMinutes.MinValue := 0;
  FSpinEditMinutes.Parent := Self;
  FLabelMinutes := TLabel.Create(Self);
  FLabelMinutes.Left := FSpinEditMinutes.Left + FSpinEditMinutes.Width;
  FLabelMinutes.FocusControl := FSpinEditMinutes;
  FLabelMinutes.Caption := RxLoadStr(ccTimeEditMins);
  FLabelMinutes.Parent := Self;
  FSpinEditSeconds := TRxSpinEdit.Create(Self);
  FSpinEditSeconds.Left := FLabelMinutes.Left + FLabelMinutes.Width + 4;
  FSpinEditSeconds.Width := 41;
  FSpinEditSeconds.MaxValue := 59;
  FSpinEditSeconds.MinValue := 0;
  FSpinEditSeconds.Parent := Self;
  FLabelSeconds := TLabel.Create(Self);
  FLabelSeconds.Left := FSpinEditSeconds.Left + FSpinEditSeconds.Width;
  FLabelSeconds.FocusControl := FSpinEditSeconds;
  FLabelSeconds.Caption := RxLoadStr(ccTimeEditSecs);
  FLabelSeconds.Parent := Self;
  Height := FSpinEditHours.Height + 2;
  Width := FLabelSeconds.Left + FLabelSeconds.Width + 2;
end;

destructor TRxTimeEdit.Destroy;
begin
  FLabelSeconds.Free;
  FSpinEditSeconds.Free;
  FLabelMinutes.Free;
  FSpinEditMinutes.Free;
  FLabelHours.Free;
  FSpinEditHours.Free;
  inherited;
end;

procedure TRxTimeEdit.WMSetTextProc(var Msg: TWMSetText);
begin
  Msg.Result := 0;
end;

procedure TRxTimeEdit.CMTextChangedProc(var Msg: TMessage);
begin
  Msg.Result := 0;
end;

function TRxTimeEdit.GetTime: TDateTime;
begin
  Result := EncodeTime(GetHours(), GetMinutes(), GetSeconds(), 0);
end;

procedure TRxTimeEdit.SetTime(const AValue: TDateTime);
var
  h, m, s, ms: Word;
begin
  DecodeTime(AValue, h, m, s, ms);
  SetHours(h);
  SetMinutes(m);
  SetSeconds(s);
end;

function TRxTimeEdit.GetHours: Byte;
begin
  Result := FSpinEditHours.AsInteger;
end;

procedure TRxTimeEdit.SetHours(const AValue: Byte);
begin
  FSpinEditHours.AsInteger := AValue;
end;

function TRxTimeEdit.GetMinutes: Byte;
begin
  Result := FSpinEditMinutes.AsInteger;
end;

procedure TRxTimeEdit.SetMinutes(const AValue: Byte);
begin
  FSpinEditMinutes.AsInteger := AValue;
end;

function TRxTimeEdit.GetSeconds: Byte;
begin
  Result := FSpinEditSeconds.AsInteger;
end;

procedure TRxTimeEdit.SetSeconds(const AValue: Byte);
begin
  FSpinEditSeconds.AsInteger := AValue;
end;

end.