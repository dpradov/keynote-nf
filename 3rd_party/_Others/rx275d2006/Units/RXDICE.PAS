{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit RXDice;

interface

{$I RX.INC}

uses SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Graphics, Messages, Controls, Forms, StdCtrls, ExtCtrls, Menus,
  RxTimer, VCLUtils;

type
  TRxDiceValue = 1..6;

{ TRxDice }

  TRxDice = class(TCustomControl)
  private
    { Private declarations }
    FActive: Boolean;
    FAutoSize: Boolean;
    FBitmap: TBitmap;
    FInterval: Cardinal;
    FAutoStopInterval: Cardinal;
    FOnChange: TNotifyEvent;
    FRotate: Boolean;
    FShowFocus: Boolean;
    FTimer: TRxTimer;
    FTickCount: Longint;
    FValue: TRxDiceValue;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CreateBitmap;
    procedure SetInterval(Value: Cardinal);
    procedure SetRotate(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure SetValue(Value: TRxDiceValue);
    procedure TimerExpired(Sender: TObject);
  protected
    { Protected declarations }
    procedure SetAutoSize(Value: Boolean); override;
    function GetPalette: HPALETTE; override;
    procedure AdjustSize; {$IFDEF RX_D4} override; {$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Change; dynamic;
    procedure DoStart; dynamic;
    procedure DoStop; dynamic;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RandomValue;
  published
    { Published declarations }
    property Align;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property AutoStopInterval: Cardinal read FAutoStopInterval write FAutoStopInterval default 0;
    property Color;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Interval: Cardinal read FInterval write SetInterval default 60;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property Rotate: Boolean read FRotate write SetRotate;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus;
    property ShowHint;
{$IFDEF RX_D4}
    property Anchors;
    property Constraints;
    property DragKind;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Value: TRxDiceValue read FValue write SetValue default 1;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
{$IFDEF WIN32}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

implementation

{$IFDEF WIN32}
 {$R *.R32}
{$ELSE}
 {$R *.R16}
{$ENDIF}

const
  ResName: array [TRxDiceValue] of PChar =
   ('DICE1', 'DICE2', 'DICE3', 'DICE4', 'DICE5', 'DICE6');

{ TRxDice }

constructor TRxDice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
  ControlStyle := [csClickEvents, csSetCaption, csCaptureMouse,
    csOpaque, csDoubleClicks];
  FValue := 1;
  FInterval := 60;
  CreateBitmap;
  FAutoSize := True;
  Width := FBitmap.Width + 2;
  Height := FBitmap.Height + 2;
end;

destructor TRxDice.Destroy;
begin
  FOnChange := nil;
  if FBitmap <> nil then FBitmap.Free;
  inherited Destroy;
end;

function TRxDice.GetPalette: HPALETTE;
begin
  if FBitmap <> nil then Result := FBitmap.Palette
  else Result := 0;
end;

procedure TRxDice.RandomValue;
var
  Val: Byte;
begin
  Val := Random(6) + 1;
  if Val = Byte(FValue) then begin
    if Val = 1 then Inc(Val)
    else Dec(Val);
  end;
  SetValue(TRxDiceValue(Val));
end;

procedure TRxDice.DoStart;
begin
  if Assigned(FOnStart) then FOnStart(Self);
end;

procedure TRxDice.DoStop;
begin
  if Assigned(FOnStop) then FOnStop(Self);
end;

procedure TRxDice.CMFocusChanged(var Message: TCMFocusChanged);
var
  Active: Boolean;
begin
  with Message do Active := (Sender = Self);
  if Active <> FActive then begin
    FActive := Active;
    if FShowFocus then Invalidate;
  end;
  inherited;
end;

procedure TRxDice.WMSize(var Message: TWMSize);
begin
  inherited;
{$IFNDEF RX_D4}
  AdjustSize;
{$ENDIF}
end;

procedure TRxDice.CreateBitmap;
begin
  if FBitmap = nil then FBitmap := TBitmap.Create;
  FBitmap.Handle := LoadBitmap(HInstance, ResName[FValue]);
end;

procedure TRxDice.AdjustSize;
var
  MinSide: Integer;
begin
  if not (csReading in ComponentState) then begin
    if AutoSize and Assigned(FBitmap) and (FBitmap.Width > 0) and
      (FBitmap.Height > 0) then
        SetBounds(Left, Top, FBitmap.Width + 2, FBitmap.Height + 2)
    else begin
      { Adjust aspect ratio if control size changed }
      MinSide := Width;
      if Height < Width then MinSide := Height;
      SetBounds(Left, Top, MinSide, MinSide);
    end;
  end;
end;

procedure TRxDice.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and TabStop and CanFocus then SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxDice.Paint;
var
  ARect: TRect;

  procedure DrawBitmap;
  var
    TmpImage: TBitmap;
    IWidth, IHeight: Integer;
    IRect: TRect;
  begin
    IWidth := FBitmap.Width;
    IHeight := FBitmap.Height;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage := TBitmap.Create;
    try
      TmpImage.Width := IWidth;
      TmpImage.Height := IHeight;
      TmpImage.Canvas.Brush.Color := Self.Brush.Color;
      TmpImage.Canvas.BrushCopy(IRect, FBitmap, IRect, FBitmap.TransparentColor);
      InflateRect(ARect, -1, -1);
      Canvas.StretchDraw(ARect, TmpImage);
    finally
      TmpImage.Free;
    end;
  end;

begin
  ARect := ClientRect;
  if FBitmap <> nil then DrawBitmap;
  if Focused and FShowFocus and TabStop and not (csDesigning in ComponentState) then
  begin
    Canvas.DrawFocusRect(ARect);
  end;
end;

procedure TRxDice.TimerExpired(Sender: TObject);
var
  ParentForm: TCustomForm;
  Now: Longint;
begin
  RandomValue;
  if not FRotate then begin
    FTimer.Free;
    FTimer := nil;
    if (csDesigning in ComponentState) then begin
      ParentForm := GetParentForm(Self);
      if ParentForm <> nil then ParentForm.Designer.Modified;
    end;
    DoStop;
  end
  else if AutoStopInterval > 0 then begin
    Now := GetTickCount;
{$IFDEF RX_D4}
    if (Now - FTickCount >= Integer(AutoStopInterval))
{$ELSE}
    if (Now - FTickCount >= AutoStopInterval)
{$ENDIF}
      or (Now < FTickCount) then Rotate := False;
  end;
end;

procedure TRxDice.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRxDice.SetValue(Value: TRxDiceValue);
begin
  if FValue <> Value then begin
    FValue := Value;
    CreateBitmap;
    Invalidate;
    Change;
  end;
end;

procedure TRxDice.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  FAutoSize := Value;
  AdjustSize;
  Invalidate;
end;

procedure TRxDice.SetInterval(Value: Cardinal);
begin
  if FInterval <> Value then begin
    FInterval := Value;
    if FTimer <> nil then FTimer.Interval := FInterval;
  end;
end;

procedure TRxDice.SetRotate(Value: Boolean);
begin
  if FRotate <> Value then begin
    if Value then begin
      if FTimer = nil then FTimer := TRxTimer.Create(Self);
      try
        with FTimer do begin
          OnTimer := TimerExpired;
          Interval := FInterval;
          Enabled := True;
        end;
        FRotate := Value;
        FTickCount := GetTickCount;
        DoStart;
      except
        FTimer.Free;
        FTimer := nil;
        raise;
      end;
    end
    else FRotate := Value;
  end;
end;

procedure TRxDice.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    if not (csDesigning in ComponentState) then Invalidate;
  end;
end;

end.
