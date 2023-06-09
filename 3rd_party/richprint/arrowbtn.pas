unit arrowbtn;


{********************************************************************
  TArrowButton component for Delphi
  
  Author: Kent Miller  Frederick, MD
  Compuserve: 74113,200
  
  Freeware: Feel free to use and improve
    
    I welcome any comments or suggestions that you may have, however
  I am new to creating components so please be gentle.
*********************************************************************}
  

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs;

type
  TButtonDirection = (arwUP, arwRIGHT, arwLEFT, arwDOWN);

  TArrowButton = class(TGraphicControl)
  private
    kDirection: TButtonDirection;
    kButtonColor: TColor;
    kButtonDown: boolean;
    kpnts: array[1..3] of TPoint;
    kRgn: HRgn;

    procedure SetDirection(value: TButtonDirection);
    procedure SetButtonColor(value: TColor);
    procedure FreeRegion;
  protected
    procedure Paint; override;
    procedure DrawUpArrow;
    procedure DrawRightArrow;
    procedure DrawDownArrow;
    procedure DrawLeftArrow;
    procedure MoveButton; virtual;
    procedure WMLButtonDown(var Message: TWMLButtonDown);
                      message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp);
                      message WM_LBUTTONUP;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ButtonColor: TColor read kButtonColor write SetButtonColor;
    property Direction: TButtonDirection read kDirection write SetDirection;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnClick;
  end;

procedure Register;

implementation

const
  { offset from border of control to corner of button }
  S_OFFSET = 3;

constructor TArrowButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csClickEvents, csCaptureMouse];
  Width := 33;
  Height := 33;
  kDirection := arwUP;
  kButtonColor := clTeal;
  kRgn := 0;
  kButtonDown := False;
end;

destructor TArrowButton.Destroy;
begin
  if kRgn <> 0 then
    FreeRegion;
  inherited Destroy;
end;

procedure TArrowButton.Paint;
begin
  inherited Paint;
  FreeRegion;
  case kDirection of
    arwUP: DrawUpArrow;
    arwRIGHT: DrawRightArrow;
    arwDOWN: DrawDownArrow;
    arwLEFT: DrawLeftArrow;
  end;
end;

procedure TArrowButton.DrawUpArrow;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  { create border region for button }
  kpnts[1] := Point(Width div 2, S_OFFSET);
  kpnts[2] := Point(Width - S_OFFSET, Height - S_OFFSET);
  kpnts[3] := Point(S_OFFSET, Height - S_OFFSET);
  { save region to capture mouse clicks }
  kRgn := CreatePolygonRgn(kpnts, 3, ALTERNATE);
  { draw black border around button }
  FrameRgn(Canvas.Handle, kRgn, Canvas.Brush.Handle, 2, 2);
  { create region within black border for button }
  Inc(kpnts[1].Y, 3);
  Dec(kpnts[2].X, 4);
  Dec(kpnts[2].Y, 2);
  Inc(kpnts[3].X, 3);
  Dec(kpnts[3].Y, 2);
  Canvas.Brush.Color := kButtonColor;
  { draw button }
  Canvas.Polygon(kpnts);
  MoveButton;
end;

procedure TArrowButton.DrawRightArrow;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  kpnts[1] := Point(S_OFFSET, S_OFFSET);
  kpnts[2] := Point(Width - S_OFFSET, Height div 2);
  kpnts[3] := Point(S_OFFSET, Height - S_OFFSET);
  kRgn := CreatePolygonRgn(kpnts, 3, ALTERNATE);
  FrameRgn(Canvas.Handle, kRgn, Canvas.Brush.Handle, 2, 2);
  Inc(kpnts[1].X, 2);
  Inc(kpnts[1].Y, 3);
  Dec(kpnts[2].X, 3);
  Inc(kpnts[3].X, 2);
  Dec(kpnts[3].Y, 3);
  Canvas.Brush.Color := kButtonColor;
  Canvas.Polygon(kpnts);
  MoveButton;
end;

procedure TArrowButton.DrawDownArrow;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  kpnts[1] := Point(Width - S_OFFSET, S_OFFSET);
  kpnts[2] := Point(Width div 2, Height - S_OFFSET);
  kpnts[3] := Point(S_OFFSET, S_OFFSET);
  kRgn := CreatePolygonRgn(kpnts, 3, ALTERNATE);
  FrameRgn(Canvas.Handle, kRgn, Canvas.Brush.Handle, 2, 2);
  Dec(kpnts[1].X, 3);
  Inc(kpnts[1].Y, 2);
  Dec(kpnts[2].Y, 3);
  Inc(kpnts[3].X, 2);
  Inc(kpnts[3].Y, 2);
  Canvas.Brush.Color := kButtonColor;
  Canvas.Polygon(kpnts);
  MoveButton;
end;

procedure TArrowButton.DrawLeftArrow;
begin
  Canvas.Brush.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  kpnts[1] := Point(Width - S_OFFSET, S_OFFSET);
  kpnts[2] := Point(Width - S_OFFSET, Height - S_OFFSET);
  kpnts[3] := Point(S_OFFSET, Height div 2);
  kRgn := CreatePolygonRgn(kpnts, 3, ALTERNATE);
  FrameRgn(Canvas.Handle, kRgn, Canvas.Brush.Handle, 2, 2);
  Dec(kpnts[1].X, 2);
  Inc(kpnts[1].Y, 3);
  Dec(kpnts[2].X, 2);
  Dec(kpnts[2].Y, 2);
  Inc(kpnts[3].X, 3);
  Canvas.Brush.Color := kButtonColor;
  Canvas.Polygon(kpnts);
  MoveButton;
end;

procedure TArrowButton.MoveButton;
begin
  if not kButtonDown then  { button is in up position }
    with Canvas do
      begin
        { draw lines around button for raised look }
        Pen.Color := clBlack;
        MoveTo(kpnts[1].X, kpnts[1].Y);
        LineTo(kpnts[2].X, kpnts[2].Y);
        MoveTo(kpnts[2].X, kpnts[2].Y);
        LineTo(kpnts[3].X, kpnts[3].Y);
        Pen.Color := clWhite;
        MoveTo(kpnts[1].X, kpnts[1].Y);
        LineTo(kpnts[3].X, kpnts[3].Y);
      end
  else  { button is in down position }
    with Canvas do
      begin
        { draw lines around button for sunken look }
        Pen.Color := clBlack;
        MoveTo(kpnts[1].X, kpnts[1].Y);
        LineTo(kpnts[3].X, kpnts[3].Y);
        Pen.Color := kButtonColor;
        MoveTo(kpnts[1].X, kpnts[1].Y);
        LineTo(kpnts[2].X, kpnts[2].Y);
        MoveTo(kpnts[2].X, kpnts[2].Y);
        LineTo(kpnts[3].X, kpnts[3].Y);
      end;
end;

procedure TArrowButton.SetDirection(value: TButtonDirection);
begin
  if value <> kDirection then
    begin
      kDirection := value;
      Invalidate;
    end;
end;

procedure TArrowButton.SetButtonColor(value: TColor);
begin
  if value <> kButtonColor then
    begin
      kButtonColor := value;
      Invalidate;
    end;
end;

procedure TArrowButton.FreeRegion;
begin
  if kRgn <> 0 then
    DeleteObject(kRgn);
  kRgn := 0;
end;

procedure TArrowButton.WMLButtonDown(var Message: TWMLButtonDown);
begin
  { if mouse is clicked on the arrowbutton make it appear sunken }
  if not PtInRegion(kRgn, Message.xPos, Message.yPos) then Exit;
  kButtonDown := True;
  MoveButton;
  inherited;
end;

procedure TArrowButton.WMLButtonUp(var Message: TWMLButtonUp);
begin
  { if arrowbutton is down and mouse is released then
    make arrowbutton appear raised }
  if not kButtonDown then Exit;
  kButtonDown := False;
  MoveButton;
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TArrowButton]);
end;

end.
 