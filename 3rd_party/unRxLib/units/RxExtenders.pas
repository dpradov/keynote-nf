{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1999,2000 Alexey Chudin         }
{                                                       }
{ Adopted from Globus Library by Alexey Chudin          }
{*******************************************************}

unit RxExtenders;

{$I RX.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Math, {$IFDEF RX_D6}Types, {$ENDIF}
  {$IFDEF RX_D16}System.UITypes, {$ENDIF}
  RxGraph, RxVCLUtils;

type
  {  TRxWizardHeader  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxWizardHeader = class(TGraphicControl)
  private
    FComments: TStrings;
    FCaptions: TStrings;
    FPageControl: TPageControl;
    FPageNo: Integer;
    FCommentFont: TFont;
    FCaptionFont: TFont;
    FSymbolFont: TFont;
    FSymbol: string;
    FGradient: TRxGradient;
    FGlyph: TBitmap;
    FBufferedDraw: Boolean;
    procedure SetCaptions(const Value: TStrings);
    procedure SetComments(const Value: TStrings);
    procedure SetPageNo(const Value: Integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCommentFont(const Value: TFont);
    procedure SetSymbolFont(const Value: TFont);
    procedure SetSymbol(const Value: string);
    procedure SetGradient(const Value: TRxGradient);
    procedure SetGlyph(const Value: TBitmap);
    function GetGlyph: TBitmap;
    { Private declarations }
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Align default alTop;
    property Anchors;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CommentFont: TFont read FCommentFont write SetCommentFont;
    property SymbolFont: TFont read FSymbolFont write SetSymbolFont;
    property PageNo: Integer read FPageNo write SetPageNo;
    property Captions: TStrings read FCaptions write SetCaptions;
    property Comments: TStrings read FComments write SetComments;
    property Symbol: string read FSymbol write SetSymbol;
    property Gradient: TRxGradient read FGradient write SetGradient;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property BufferedDraw: Boolean read FBufferedDraw write FBufferedDraw;
  end;

  {  TRxFlexHelpPanel  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxFlexHelpPanel = class(TCustomPanel)
  private
    Rich: {$IFNDEF RX_D6}TMemo{$ELSE}TRichEdit{$ENDIF};
    FStrings: TStrings;
    ButtonRect: TRect;
    FHighlightButton: Boolean;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    fInitializing: Boolean;
    procedure SetStrings(const Value: TStrings);
    procedure SetHighlightButton(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetExpanded(const Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitRichText;
    property HighlightButton: Boolean read FHighlightButton write SetHighlightButton;
  published
    property Align;
    property Alignment;
    {$IFDEF RX_D4}property Anchors; {$ENDIF}
    {$IFDEF RX_D4}property AutoSize; {$ENDIF}
    property BevelInner stored True;
    property BevelOuter stored True;
    property BevelWidth;
    {$IFDEF RX_D4}property BiDiMode; {$ENDIF}
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    {$IFDEF RX_D4}property Constraints; {$ENDIF}
    property Ctl3D;
    {$IFDEF RX_D4}
    property UseDockManager default True;
    property DockSite;
    {$ENDIF}
    property DragCursor;
    {$IFDEF RX_D4}property DragKind; {$ENDIF}
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    {$IFDEF RX_D4}property ParentBiDiMode; {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF RX_D4}property OnCanResize; {$ENDIF}
    property OnClick;
    {$IFDEF RX_D4}property OnConstrainedResize; {$ENDIF}
    {$IFDEF RX_D5}property OnContextPopup; {$ENDIF}
    {$IFDEF RX_D4}property OnDockDrop; {$ENDIF}
    {$IFDEF RX_D4}property OnDockOver; {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF RX_D4}property OnEndDock; {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF RX_D4}property OnGetSiteInfo; {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF RX_D4}
    property OnStartDock;
    property OnUnDock;
    {$ENDIF}
    property OnStartDrag;

    property Expanded: Boolean read FExpanded write SetExpanded default False;
    property Strings: TStrings read FStrings write SetStrings;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
  end;

  {  TRxHoleShape  }

  TRxPointClass = class(TPersistent)
  private
    FX: Integer;
    FY: Integer;
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
  public
    OnChanged: TNotifyEvent; {Catch OnChanged event only for}
  published
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
  end;

  TRxRegionCombineMode = (cmAND, cmCOPY, cmDIFF, cmOR, cmXOR);
  TRxHoleShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare,
    stEllipse, stCircle);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxHoleShape = class(TGraphicControl)
  private
    FShape: TRxHoleShapeType;
    FShapeBitmap: TBitmap;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBoldInner: Boolean;
    FBoldOuter: Boolean;
    FRectEllipse: TRxPointClass;
    FBevelOffset: Integer;
    fNeedUpdateRGN: Boolean;
    fDestroyed: Boolean;
    fRunOnce: Boolean;
    fNeedRebuildBitmapShape: Boolean;
    OldX, OldY, OldW, OldH: Integer;
    procedure SetEnabled(Value: Boolean); reintroduce;
    procedure SetEnabledDT(Value: Boolean);
    procedure SetShape(Value: TRxHoleShapeType);
    procedure SetShapeBitmap(Value: TBitmap);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBoldInner(Value: Boolean);
    procedure SetBoldOuter(Value: Boolean);
    procedure SetCombineMode(Value: TRxRegionCombineMode);
    procedure SetBevelOffset(Value: Integer);
    procedure DoUpdateRegion;
    procedure DoCalcRegions;
    procedure SmthChanged(Sender: TObject);
    procedure SayAllDTEnabledState(EnabledDT: Boolean);
  protected
    procedure Paint; override;
  public
    RGNOuter, RGNInner: HRGN;
    FCombineMode: TRxRegionCombineMode;
    FEnabledDT: Boolean;
    FEnabled: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateRGN;
    procedure Loaded; override;
  published
    property Align;
    property ShowHint;
    property ParentShowHint;
    property PopupMenu;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property EnabledAllInDesignTime: Boolean read FEnabledDT write SetEnabledDT
      default True;
    property Shape: TRxHoleShapeType read FShape write SetShape
      default stEllipse;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner
      default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter
      default bvLowered;
    property BevelInnerBold: Boolean read FBoldInner write SetBoldInner
      default True;
    property BevelOuterBold: Boolean read FBoldOuter write SetBoldOuter
      default True;
    property CombineMode: TRxRegionCombineMode read FCombineMode
      write SetCombineMode default cmDIFF;
    property BevelOffset: Integer read FBevelOffset write SetBevelOffset
      default 0;
    property RectEllipse: TRxPointClass read FRectEllipse write FRectEllipse;
    property ShapeBitmap: TBitmap read FShapeBitmap write SetShapeBitmap;
  end;

  TRxTileOption = (ftoNone, ftoStretch, ftoPropStretch, ftoTile);
  TRxDrawType = (fdtDefault, fdtDisabled, fdtDelicate);
  TRxTranspPixelFrom = (ftpfUser, ftpfLeftTopPixel, ftpfLeftBottomPixel,
    ftpfRightTopPixel, ftpfRightBottomPixel);

{  graphic utilities  }

function IsDrawableBitmap(Bmp: TBitmap): Boolean; {$IFDEF RX_D9}inline; {$ENDIF}

procedure CreateBitmapExt(DC: HDC; {target DC}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: Integer; //...x,y _in_ rect!
  iTileBitmapOption: TRxTileOption;
  iDrawType: TRxDrawType;
  fTransparent: Boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: Integer; //...x,y _in_ rect!
  iTileBitmapOption: TRxTileOption;
  iDrawType: TRxDrawType;
  fTransparent: Boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor); {$IFDEF RX_D9}inline; {$ENDIF}

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TRxTranspPixelFrom): TColor; {$IFDEF RX_D9}inline; {$ENDIF}

implementation

{  graphic utilities  }

function IsDrawableBitmap(Bmp: TBitmap): Boolean;
begin
  with Bmp do
    Result := Assigned(bmp) and (Width <> 0) and (Height <> 0);
end;

procedure CreateBitmapExt(DC: HDC; {target DC}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: Integer; //...x,y _in_ rect!
  iTileBitmapOption: TRxTileOption;
  iDrawType: TRxDrawType;
  fTransparent: Boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);
var
  x_, y_, H, W: Integer;
  D, D_: Double;
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  BmpInfo: Windows.TBitmap;
  ptSize, ptOrg: TPoint;
  MemDC, ImageDC: HDC;
  OldBMP, OldMonoBMP, OldScreenImageBMP, OldMemBMP: HBitmap;
  Mono_BMP, ScreenImageBMP, MemBMP: HBitmap;
  MonoDC, ScreenImageDC: HDC;
  OldBkColor: COLORREF;
  SavedIHeight: Integer;
const
  ROP_DSPDxax = $00E20746;

  procedure BitBltWorks; //*************************************END LOCAL
  begin
    if fTransparent then
    begin
      { create copy of drawing image }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, ImageDC, 0, 0, SRCCOPY);
      if iDrawType = fdtDisabled then TransparentColor := clBtnFace;
      OldBkColor := SetBkColor(MemDC, ColorToRGB(TransparentColor));
      { create monohrome mask: TransparentColor -> white, other color -> black }
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);
      SetBkColor(MemDC, OldBkColor);
      {create copy of screen image}
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, DC, x_, y_, SRCCOPY);
      { put monohrome mask }
      BitBlt(ScreenImageDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      BitBlt(MonoDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, NOTSRCCOPY);
      { put inverse monohrome mask }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, MonoDC, 0, 0, SRCAND);
      { merge Screen screen image(MemDC) and Screen image(ScreenImageDC) }
      BitBlt(MemDC, 0, 0, IWidth, IHeight, ScreenImageDC, 0, 0, SRCPAINT);
      { to screen }
      //DSTINVERT MERGEPAINT
      BitBlt(DC, x_, y_, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);
    end
    else
      BitBlt(DC, x_, y_, IWidth, IHeight, ImageDC, 0, 0, SRCCOPY);

  end; //*******************************************END LOCAL PROC

begin
  if (SourceBitmap.Width or SourceBitmap.Height) = 0 then Exit;

  x := x + r.left; y := y + r.top; x_ := x; y_ := y;

  OldBMP := 0; OldMemBMP := 0; OldMonoBMP := 0; OldScreenImageBMP := 0;
  MemDC := 0; ImageDC := 0;
  //Mono_BMP := 0; ScreenImageBMP := 0; MemBMP := 0;
  MonoDC := 0; ScreenImageDC := 0;

  IWidth := SourceBitmap.Width; //min( SourceBitmap.Width, r.right-r.left );
  IHeight := SourceBitmap.Height; //min( SourceBitmap.Height, r.bottom-r.top );

  TmpImage := TBitmap.Create;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~/
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    ORect := Rect(0, 0, IWidth, IHeight);

    TmpImage.Canvas.Brush.Color := TransparentColor;
    TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));

    case iDrawType of
      fdtDefault: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          BitBlt(TmpImage.Canvas.Handle, 0, 0, IWidth, IHeight,
            SourceBitmap.canvas.Handle, 0, 0, SRCCOPY);
        end;
      fdtDelicate: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          with TmpImage.Canvas do
            BitBlt(Handle, 0, 0, IWidth, IHeight,
              SourceBitmap.canvas.Handle, 0, 0, cmSrcCopy);
          begin
            { Convert white to clBtnHighlight }
            ChangeBitmapColor(TmpImage, clWhite, clBtnHighlight);
            { Convert gray to clBtnShadow }
            ChangeBitmapColor(TmpImage, clGray, clBtnShadow);
            { Convert transparent color to clBtnFace }
            //ChangeBitmapColor(TmpImage,ColorToRGB(}TransparentColor), clBtnFace);
          end;
        end;
      fdtDisabled: //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        begin
          if DisabledMaskColor <> 0 then
            ChangeBitmapColor(TmpImage, DisabledMaskColor, clBlack);
          MonoBmp := Tbitmap.Create;
          try { Create a disabled version }
            with MonoBmp do
            begin
              Assign(SourceBitmap);
              Canvas.Brush.Color := 0;
              Width := IWidth;
              if Monochrome then
              begin
                Canvas.Font.Color := clWhite;
                Monochrome := False;
                Canvas.Brush.Color := clWhite;
              end;
              Monochrome := True;
            end;
            with TmpImage.Canvas do
            begin
              Brush.Color := clBtnFace;
              FillRect(IRect);
              Brush.Color := clBtnHighlight;
              SetTextColor(Handle, 0); SetBkColor(Handle, clWhite);
              BitBlt(Handle, 1, 1, IWidth, IHeight,
                MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              Brush.Color := clBtnShadow;
              SetTextColor(Handle, 0); SetBkColor(Handle, clWhite);
              BitBlt(Handle, 0, 0, IWidth, IHeight,
                MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end;
          finally
            MonoBmp.Free;
          end;
        end;
    end; {CASE}

    with TmpImage.Canvas do
      if (iTileBitmapOption = ftoStretch) or (iTileBitmapOption = ftoPropStretch) then
      begin
        MemDC := CreateCompatibleDC(DC);
        MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, r.right - r.left, r.bottom - r.top);
        OldMemBMP := SelectObject(MemDC, MemBMP);
        W := r.right - r.left; H := r.bottom - r.top;
        if iTileBitmapOption = ftoPropStretch then
        begin
          D_ := W / IWidth; D := H / IHeight;
          if D > D_ then D := D_; //...D == min
          W := Trunc(IWidth * D); H := Trunc(IHeight * D);
        end;
        StretchBlt(MemDC, 0, 0, W, H, Handle,
          0, 0, IWidth, IHeight, SRCCOPY);

        IWidth := W; IHeight := H; TmpImage.Width := W; TmpImage.Height := H;
        BitBlt(Handle, 0, 0, IWidth, IHeight, MemDC, 0, 0, SRCCOPY);

        DeleteObject(SelectObject(MemDC, OldMemBMP));
        DeleteDC(MemDC);
      end;

    ImageDC := CreateCompatibleDC(DC);

    if fTransparent then
    begin
      MemDC := CreateCompatibleDC(DC);
      ScreenImageDC := CreateCompatibleDC(DC);
      MonoDC := CreateCompatibleDC(DC);

      Mono_BMP := CreateBitmap(IWidth, IHeight, 1, 1, nil);
      ScreenImageBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);
      MemBMP := CreateCompatibleBitmap(TmpImage.Canvas.Handle, IWidth, IHeight);

      OldMonoBMP := SelectObject(MonoDC, Mono_BMP);
      OldScreenImageBMP := SelectObject(ScreenImageDC, ScreenImageBMP);
      OldMemBMP := SelectObject(MemDC, MemBMP);
    end;
    OldBMP := SelectObject(ImageDC, TmpImage.Handle);

    if OldBMP <> 0 then
    begin
      SetMapMode(ImageDC, GetMapMode(DC));
      GetObject(TmpImage.Handle, SizeOf(Windows.TBitmap), @BmpInfo);
      ptSize.x := BmpInfo.bmWidth; ptOrg.x := 0;
      ptSize.y := BmpInfo.bmHeight; ptOrg.y := 0;
      if fTransparent then
      begin DPtoLP(DC, ptSize, 1); DPtoLP(MemDC, ptOrg.y, 1); end;
      if iTileBitmapOption = ftoTile then
      begin
        //SavedIWidth:=IWidth;
        SavedIHeight := IHeight;
        while x_ < r.right do
        begin
          //IWidth:=SavedIWidth; //SavedIWidth:=IWidth;
          if x_ + IWidth > r.right then
            IWidth := r.right - x_;
          while y_ < r.bottom do
          begin
            IHeight := SavedIHeight; // SavedIHeight:=IHeight;
            if y_ + IHeight > r.bottom then IHeight := r.bottom - y_;
            BitBltWorks;
            Inc(y_, IHeight);
          end;
          Inc(x_, IWidth); y_ := y;
        end;
      end
      else
        BitBltWorks;
    end;
  finally
    {DeleteObject}(SelectObject(ImageDC, OldBMP));
    DeleteDC(ImageDC);
    if fTransparent then
    begin
      DeleteObject(SelectObject(MonoDC, OldMonoBMP));
      DeleteObject(SelectObject(ScreenImageDC, OldScreenImageBMP));
      DeleteObject(SelectObject(MemDC, OldMemBMP));
      DeleteDC(MonoDC);
      DeleteDC(ScreenImageDC);
      DeleteDC(MemDC);
    end;
    TmpImage.Free;
  end;
end;

procedure DrawBitmapExt(DC: HDC; { DC - background & result}
  SourceBitmap: TBitmap;
  r: TRect;
  x, y: Integer; //...x,y _in_ rect!
  iTileBitmapOption: TRxTileOption;
  iDrawType: TRxDrawType;
  fTransparent: Boolean;
  TransparentColor: TColor;
  DisabledMaskColor: TColor);
begin
  CreateBitmapExt(DC, SourceBitmap, r, x, y, iTileBitmapOption,
    iDrawType, fTransparent, TransparentColor,
    DisabledMaskColor);
end;

function GetTransparentColor(Bitmap: TBitmap; AutoTrColor: TRxTranspPixelFrom): TColor;
var
  x, y: Integer;
begin
  if (AutoTrColor = ftpfUser) or (not IsDrawableBitmap(Bitmap)) then
    Result := 0
  else
  begin
    case AutoTrColor of
      ftpfLeftTopPixel:
        begin
          x := 0;
          y := 0;
        end;
      ftpfLeftBottomPixel:
        begin
          x := 0;
          y := Bitmap.Height - 1;
        end;
      ftpfRightTopPixel:
        begin
          x := Bitmap.Width - 1;
          y := 0;
        end;
    else {ftcRightBottomPixel}
      x := Bitmap.Width - 1;
      y := Bitmap.Height - 1;
    end;
    Result := GetPixel(Bitmap.Canvas.Handle, x, y);
  end;
end;

{ TRxWizardHeader }

constructor TRxWizardHeader.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  Align := alTop;
  Height := 60;
  FCaptions := TStringList.Create;
  FComments := TStringList.Create;
  FGradient := TRxGradient.Create;
  FGradient.Visible := True;
  FGradient.StartColor := clHighlight;
  FGradient.EndColor := clWindow;
  FGradient.Direction := fdLeftToRight;
  FCaptionFont := Font;
  FCaptionFont.Style := [fsBold];
  FCommentFont := TFont.Create;
  FSymbolFont := TFont.Create;
  FSymbolFont.Name := 'Wingdings';
  FSymbolFont.Size := 26;
  FSymbolFont.Color := clHighlightText;
  FSymbolFont.Style := [fsBold];
//  FSymbol := '4';
end;

destructor TRxWizardHeader.Destroy;
begin
  FCaptions.Free;
  FComments.Free;
  FCommentFont.Free;
  FGradient.Free;
  FSymbolFont.Free;
  if Assigned(FGlyph) then FGlyph.Free;
  inherited;
end;

function TRxWizardHeader.GetGlyph: TBitmap;
begin
  if not Assigned(FGlyph) then FGlyph := TBitmap.Create;
  Result := FGlyph;
end;

procedure TRxWizardHeader.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FPageControl) and (Operation = opRemove) then
  begin FPageControl := nil; end;
end;

procedure TRxWizardHeader.Paint;
var
  R, SR: TRect;
  Offset: Integer;
  Buffer: TBitmap;
  Caption, Comment: string;
  TargetCanvas: TCanvas;
begin
  FSymbol := Copy(FSymbol, 1, 1);

  if BufferedDraw then
  begin
    Buffer := TBitmap.Create;
    Buffer.Width := Width;
    Buffer.Height := Height;
    TargetCanvas := Buffer.Canvas;
  end
  else
  begin
    Buffer := nil;
    TargetCanvas := Canvas;
  end;

  try

    if FCaptions.Count = 0 then
      Caption := 'Caption'
    else
      Caption := FCaptions[Min(FCaptions.Count - 1, PageNo)];
    if FComments.Count = 0 then
      Comment := 'Some comment text'
    else
      Comment := FComments[Min(FComments.Count - 1, PageNo)];

    R := ClientRect;

    TargetCanvas.Brush.Color := clWindow;
    TargetCanvas.FillRect(R);

    Inc(R.Left, 20); Dec(R.Right, 60);
    Inc(R.Top, 8); Dec(R.Bottom, 5);

    TargetCanvas.Font.Assign(CaptionFont);
    DrawText(TargetCanvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE);

    Inc(R.Top, TargetCanvas.TextHeight('Hy')); Inc(R.Left, 20);

    TargetCanvas.Font.Assign(CommentFont);
    SR := R;
    DrawText(TargetCanvas.Handle, PChar(Comment), Length(Comment), SR, DT_WORDBREAK or DT_CALCRECT);

    OffsetRect(SR, 0, (R.Bottom - SR.Bottom) div 2);
    DrawText(TargetCanvas.Handle, PChar(Comment), Length(Comment), SR, DT_WORDBREAK);

    if Assigned(FGlyph) and (FGlyph.Width > 0) then
    begin
      R := ClientRect;
      Offset := (Height - FGlyph.Height) div 2;

      //BitBlt(TargetCanvas.Handle, R.Right-FGlyph.Width-Offset, R.Top+Offset, FGlyph.Width, FGlyph.Height, FGlyph.TargetCanvas.Handle, 0, 0, SRCCOPY);
      DrawBitmapExt(TargetCanvas.Handle, FGlyph, R, R.Right - FGlyph.Width - Offset, R.Top + Offset,
        ftoNone, fdtDefault, True, GetTransparentColor(FGlyph, ftpfLeftBottomPixel), 0);
    end
    else if Length(FSymbol) > 0 then
    begin
      TargetCanvas.Brush.Color := clHighlight;
      R := ClientRect;
      SR := Rect(R.Right - 50, R.Top + 5, R.Right - 5, R.Bottom - 5);
      if Assigned(Gradient) and Gradient.Visible then Dec(SR.Bottom, 3);
      TargetCanvas.FillRect(SR);

      TargetCanvas.Font.Assign(SymbolFont);
      SetBkMode(TargetCanvas.Handle, TRANSPARENT);
      DrawText(TargetCanvas.Handle, PChar(Symbol), Length(FSymbol), SR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
    end;

    R := ClientRect;
    DrawEdge(TargetCanvas.Handle, R, EDGE_ETCHED, BF_BOTTOM);

    if Gradient.Visible then
      Gradient.Draw(TargetCanvas, Rect(R.Left, R.Bottom - 5, R.Right, R.Bottom - 1));
      //GradientBox( TargetCanvas.Handle, Rect(R.Left, R.Bottom-5, R.Right, R.Bottom-1), Gradient, 1, 1);

    if BufferedDraw then
      BitBlt(Canvas.Handle, 0, 0, Width, Height, TargetCanvas.Handle, 0, 0, SRCCOPY);

  finally
    if BufferedDraw then Buffer.Free;
  end;
end;

procedure TRxWizardHeader.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
end;

procedure TRxWizardHeader.SetCaptions(const Value: TStrings);
begin
  FCaptions.Assign(Value);
  Invalidate;
end;

procedure TRxWizardHeader.SetCommentFont(const Value: TFont);
begin
  FCommentFont.Assign(Value);
  Invalidate;
end;

procedure TRxWizardHeader.SetComments(const Value: TStrings);
begin
  FComments.Assign(Value);
  Invalidate;
end;

procedure TRxWizardHeader.SetGlyph(const Value: TBitmap);
begin
  if not Assigned(FGlyph) then FGlyph := TBitmap.Create;
  FGlyph.Assign(Value);
end;

procedure TRxWizardHeader.SetGradient(const Value: TRxGradient);
begin
  FGradient.Assign(Value);
end;

procedure TRxWizardHeader.SetPageNo(const Value: Integer);
begin
  FPageNo := Value;
  Paint;
end;

procedure TRxWizardHeader.SetSymbol(const Value: string);
begin
  FSymbol := Value;
  Invalidate;
end;

procedure TRxWizardHeader.SetSymbolFont(const Value: TFont);
begin
  FSymbolFont.Assign(Value);
  Invalidate;
end;

{ TRxFlexHelpPanel }

procedure TRxFlexHelpPanel.CMMouseLeave(var Message: TMessage);
begin
  HighlightButton := False;
end;

constructor TRxFlexHelpPanel.Create(AOwner: TComponent);
begin
  inherited;
  fInitializing := True;

  BevelInner := bvNone;
  BevelOuter := bvNone;

  FStrings := TStringList.Create;
  Height := 70;
  Caption := ' help ';

  //if csDesigning in ComponentState then Align := alBottom;
  Expanded := False;
  fInitializing := False;
  {$IFDEF RX_D6}
  if csDesigning in ComponentState then Exit;
  {$ENDIF}
  Rich := {$IFNDEF RX_D6}TMemo{$ELSE}TRichEdit{$ENDIF}.Create(Self);
  Rich.Parent := Self;
  Rich.ReadOnly := True;

end;

destructor TRxFlexHelpPanel.Destroy;
begin
  FStrings.Free;
  if Assigned(Rich) then Rich.Free;
  inherited;
end;

procedure TRxFlexHelpPanel.Loaded;
begin
  inherited;
  InitRichText;
end;

procedure TRxFlexHelpPanel.InitRichText;
var
  ms: TMemoryStream;
begin
  if not Assigned(Rich) then Exit;
  Rich.BorderStyle := bsNone;
  Rich.SetBounds(12, 16, Width - 24, ExpandedHeight - 22);
  ms := TMemoryStream.Create;
  try
    FStrings.SaveToStream(ms);
    ms.Position := 0;
    Rich.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TRxFlexHelpPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    Expanded := not Expanded;
    if Assigned(onClick) then onClick(Self);
  end;
end;

procedure TRxFlexHelpPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if PtInRect(ButtonRect, Point(X, Y)) then
  begin
    if not HighlightButton then HighlightButton := not HighlightButton;
  end
  else if HighlightButton then
    HighlightButton := not HighlightButton;
end;

procedure TRxFlexHelpPanel.Paint;
const
  WARNING = 'Open context menu to load RTF text. Control shows text at runtime only.';
  {$IFDEF RX_D6}
var
  R: TRect;
  {$ENDIF}
begin
  //inherited;

  Canvas.Brush.Style := bsSolid;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color := clBtnShadow;
  Canvas.FillRect(Bounds(5, 7, Width - 10, 2));

  Canvas.Brush.Color := clWindow;
  Canvas.Pen.Color := clBlack;
  if Expanded then
    Canvas.Rectangle(5, 15, Width - 5, Height - 5);

  ButtonRect := Bounds(Width - 80, 0, 80, 20);

  Canvas.Font.Style := [fsBold];
  if FHighlightButton then
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnShadow));
    SetTextColor(Canvas.Handle, clWhite);
  end
  else
  begin
    SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
    SetTextColor(Canvas.Handle, clBlack);
  end;
  SetBkMode(Canvas.Handle, OPAQUE);
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), ButtonRect, DT_SINGLELINE or DT_RIGHT);
  {$IFDEF RX_D6}
  if csDesigning in ComponentState then
  begin
    R := ClientRect; Inc(R.Top, 20);
    SetBkMode(Canvas.Handle, TRANSPARENT);
    DrawText(Canvas.Handle, WARNING, Length(WARNING), R, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
  end;
  {$ENDIF}
end;

procedure TRxFlexHelpPanel.SetExpanded(const Value: Boolean);
begin
  FExpanded := Value;
  if FExpanded then
    Height := ExpandedHeight
  else
  begin
    FExpandedHeight := Height;
    Height := 16;
  end;

  if not fInitializing then
    if Parent is TForm then
      with (Parent as TForm) do
        if FExpanded then
          Height := Height + ExpandedHeight - 16
        else
          Height := Height - ExpandedHeight + 16;
end;

procedure TRxFlexHelpPanel.SetExpandedHeight(const Value: Integer);
begin
  FExpandedHeight := Value;
end;

procedure TRxFlexHelpPanel.SetHighlightButton(const Value: Boolean);
begin
  FHighlightButton := Value;
  if FHighlightButton then
    Cursor := {$IFDEF RX_D3}crHandPoint{$ELSE}crUpArrow{$ENDIF}
  else
    Cursor := crDefault;
  Repaint;
end;

procedure TRxFlexHelpPanel.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
  InitRichText;
end;

{  TglHoleShape  }

const
  aCombMode: array[0..4] of Integer = (RGN_AND, RGN_COPY, RGN_DIFF, RGN_OR, RGN_XOR);

procedure TRxPointClass.SetX(Value: Integer);
begin
  if FX = Value then Exit; FX := Value;
  if Assigned(OnChanged) then OnChanged(Self);
end;

procedure TRxPointClass.SetY(Value: Integer);
begin
  if FY = Value then Exit; FY := Value;
  if Assigned(OnChanged) then OnChanged(Self);
end;

//________________________________________________________ Methods _

constructor TRxHoleShape.Create(AOwner: TComponent);
begin
  inherited;
  FShapeBitmap := TBitmap.Create;
  FEnabled := (Owner is TWinControl);

  ControlStyle := ControlStyle - [csOpaque];
  FEnabledDT := FEnabled;
  fDestroyed := False;
  FRectEllipse := TRxPointClass.Create;
  FRectEllipse.x := 30; FRectEllipse.y := 30;
  FRectEllipse.OnChanged := SmthChanged;
  FShape := stEllipse;
  FBevelOuter := bvLowered;
  FBevelInner := bvNone;
  FCombineMode := cmDIFF;
  FBoldInner := True; FBoldOuter := True;
  FRectEllipse.y := 45; FRectEllipse.x := 45;
  FBevelOffset := 0;
  Width := 112; Height := 112;
  fNeedUpdateRGN := False;
  fRunOnce := True;
end;

destructor TRxHoleShape.Destroy;
begin
  FShapeBitmap.Free;
  FRectEllipse.Free;
  if not (csDestroying in Owner.ComponentState) then
  begin FEnabledDT := False; FEnabled := False; UpdateRGN(); end;
  inherited;
end;

procedure TRxHoleShape.Paint;
var
  r: TRect;
  H, W, EH, EW, i: Integer;

  procedure DrawShape(Bevel: TPanelBevel; fBold, fRect: Boolean); //_______LOCAL PROC_

    procedure SetPenAndBrush(c: TColor);
    begin
      Canvas.Pen.Color := c;
      if fRect and ((EW and EH) = 0) then
      begin
        Canvas.Brush.Style := bsClear;
      end
      else
      begin
        Canvas.Brush.Color := c;
      end
    end;
  begin
    Canvas.Brush.Style := bsClear; //bsSolid;//bsClear;
    i := Integer(fBold);
    with Canvas do
      case Bevel of
        bvLowered:
          begin
            SetPenAndBrush(clBtnHighlight);
            if fRect then
              RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right, R.Bottom);
            SetPenAndBrush(clBtnShadow);
            if fRect then
              RoundRect(R.Left, R.Top, R.Right - 1, R.Bottom - 1, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right - 1, R.Bottom - 1);
            if FBold then
            begin
              SetPenAndBrush(cl3DDkShadow);
              if fRect then
                RoundRect(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1, EW, EH)
              else
                Ellipse(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
            end;
            InflateRect(R, -1, -1); Inc(R.Left, i); Inc(R.Top, i);
          end;
        bvRaised:
          begin
            SetPenAndBrush(clBtnHighlight);
            if fRect then
              RoundRect(R.Left, R.Top, R.Right, R.Bottom, EW, EH)
            else
              Ellipse(R.Left, R.Top, R.Right, R.Bottom);
            if FBold then
            begin
              SetPenAndBrush(cl3DDkShadow);
              if fRect then
                RoundRect(R.Left + 1, R.Top + 1, R.Right, R.Bottom, EW, EH)
              else
                Ellipse(R.Left + 1, R.Top + 1, R.Right, R.Bottom);
            end;
            SetPenAndBrush(clBtnShadow);
            if fRect then
              RoundRect(R.Left + 1, R.Top + 1, R.Right - i, R.Bottom - i, EW, EH)
            else
              Ellipse(R.Left + 1, R.Top + 1, R.Right - i, R.Bottom - i);
            InflateRect(R, -1, -1); Dec(R.Right, i); Dec(R.Bottom, i);
          end;
      else
        //Brush.Color:=clBlack;
        //FrameRect( Rect(Left, Top, Left+W, Top+H) );
      end;
    SetPenAndBrush(clBtnFace);

  end; //____________________________________END LOCAL PROC_

begin //_________________________________________________________PAINT_
  fNeedUpdateRGN := fNeedUpdateRGN or (OldX <> Left) or (OldY <> Top) or (OldW <> Width) or (OldH <> Height);

  if fNeedUpdateRGN then UpdateRGN();
  OldX := Left;
  OldY := Top;
  OldW := Width;
  OldH := Height;

  if IsDrawableBitmap(FShapeBitmap) then
  begin
    BitBlt(Canvas.handle, -1, -1, Width, Height, FShapeBitmap.Canvas.handle, 0, 0, SRCCopy);
    Exit;
  end;

  case FShape of
    stRectangle, stRoundRect, stEllipse:
      begin
        H := Height;
        W := Width;
      end
  else
    H := Min(Height, Width);
    W := H;
  end;
  R := Bounds(0, 0, W, H);
  with Canvas do
    case FShape of
      stRectangle, stSquare, stRoundRect, stRoundSquare:
        begin
          if (FShape = stRectangle) or (FShape = stSquare) then
          begin
            EW := 0;
            EH := 0;
          end;
          if (FShape = stRoundRect) or (FShape = stRoundSquare) then
          begin
            EW := FRectEllipse.x;
            EH := FRectEllipse.y;
          end;

          DrawShape(FBevelOuter, FBoldOuter, True);
          InflateRect(R, -FBevelOffset, -FBevelOffset);
          DrawShape(FBevelInner, FBoldInner, True);

          //Pen.Color:=clBtnFace;
          //Rect( R.Left, R.Top, R.Right, R.Bottom );
        end;
      stEllipse, stCircle:
        begin
          DrawShape(FBevelOuter, FBoldOuter, False);
          InflateRect(R, -FBevelOffset, -FBevelOffset);
          DrawShape(FBevelInner, FBoldInner, False);
        end;
    end;
end;

procedure TRxHoleShape.DoCalcRegions;
var
  H, W, xOffs, yOffs: Integer;
  R: TRect;
  BmpInfo: Windows.TBitmap;
  BorderStyle: TFormBorderStyle;

  procedure CalcShape(Bevel: TPanelBevel; fBold: Boolean); //____LOCAL PROC_
  var
    i: Integer;
  begin
    i := Integer(fBold);
    case Bevel of
      bvLowered:
        begin
          InflateRect(R, -1, -1);
          Inc(R.Left, i);
          Inc(R.Top, i);
        end;
      bvRaised:
        begin
          InflateRect(R, -1, -1);
          Dec(R.Right, i);
          Dec(R.Bottom, i);
        end;
    end;
  end; //____________________________________END LOCAL PROC_

  procedure CalcBmpRgn(var rgn: HRGN);
  var
    i, j: Integer;
    rgn2: HRGN;
    TransparentColor: TColor;
  begin
    TransparentColor := FShapeBitmap.Canvas.Pixels[0, FShapeBitmap.Height - 1];
    for j := 0 to FShapeBitmap.Height do
      for i := 0 to FShapeBitmap.Width do
      begin
        if FShapeBitmap.Canvas.Pixels[i, j] <> TransparentColor then Continue;
        RGN2 := CreateRectRgn(i, j, i + 1, j + 1);
        CombineRgn(RGN, RGN2, RGN, RGN_OR);
        DeleteObject(RGN2);
      end;
  end; //____________________________________END LOCAL PROC_
begin
  if not FShapeBitmap.Empty then
  begin
    {if fNeedRebuildBitmapShape then}with FShapeBitmap do
    begin
      GetObject(FShapeBitmap.Handle, SizeOf(Windows.TBitmap), @BmpInfo);
      if RGNOuter <> 0 then DeleteObject(RGNOuter);
      if RGNInner <> 0 then DeleteObject(RGNInner);
      RGNInner := CreateRectRgn(0, 0, 0, 0);
      CalcBmpRgn(RGNInner);
      fNeedRebuildBitmapShape := False;
    end;
  end
  else
  begin
    case FShape of
      stRectangle, stRoundRect, stEllipse:
        begin H := Height; W := Width; end
    else
      begin H := Min(Height, Width); W := H; end;
    end;
    R := Bounds(0, 0, W, H);
    if RGNOuter <> 0 then DeleteObject(RGNOuter);
    if RGNInner <> 0 then DeleteObject(RGNInner);

    if FBevelOffset <> 0 then
    begin
      CalcShape(FBevelOuter, FBoldOuter);
      OffsetRect(R, 1, 1);
    end;
    case FShape of
      stRectangle, stSquare:
        RGNOuter := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      stRoundRect, stRoundSquare:
        RGNOuter := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, FRectEllipse.x, FRectEllipse.y);
      stEllipse, stCircle:
        RGNOuter := CreateEllipticRgn(R.Left, R.Top, R.Right, R.Bottom);
    end;
    if FBevelOffset = 0 then CalcShape(FBevelOuter, FBoldOuter);
    InflateRect(R, -FBevelOffset, -FBevelOffset);
    if FBevelOffset = 0 then
      CalcShape(FBevelInner, FBoldInner)
    else
      OffsetRect(R, -1, -1);
    case FShape of
      stRectangle, stSquare:
        RGNInner := CreateRectRgn(R.Left + 1, R.Top + 1, R.Right + 1, R.Bottom + 1);
      stRoundRect, stRoundSquare:
        RGNInner := CreateRoundRectRgn(R.Left + 1, R.Top + 1, R.Right + 2, R.Bottom + 2, FRectEllipse.x, FRectEllipse.y);
      stEllipse, stCircle:
        RGNInner := CreateEllipticRgn(R.Left + 1, R.Top + 1, R.Right + 2, R.Bottom + 2);
    end;
  end;

    { calc offsets }
  if Owner is TForm then
  begin
    if csDesigning in ComponentState then
      BorderStyle := bsSizeable
    else
      BorderStyle := TForm(Owner).BorderStyle;
    case BorderStyle of
      bsSizeable:
        begin
          xOffs := GetSystemMetrics(SM_CXFRAME) - 1;
          yOffs := GetSystemMetrics(SM_CYFRAME) - 1;
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsDialog:
        begin
          xOffs := GetSystemMetrics(SM_CXDLGFRAME) - 1;
          yOffs := GetSystemMetrics(SM_CYDLGFRAME) - 1;
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsSingle:
        begin
          xOffs := GetSystemMetrics(SM_CXBORDER);
          yOffs := GetSystemMetrics(SM_CYBORDER);
          Inc(yOffs, GetSystemMetrics(SM_CYCAPTION));
        end;
      bsToolWindow:
        begin
          xOffs := GetSystemMetrics(SM_CXBORDER);
          yOffs := GetSystemMetrics(SM_CYBORDER);
          Inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
        end;
      bsSizeToolWin:
        begin
          xOffs := GetSystemMetrics(SM_CXSIZEFRAME);
          yOffs := GetSystemMetrics(SM_CYSIZEFRAME);
          Inc(yOffs, GetSystemMetrics(SM_CYSMCAPTION));
        end;
    else
      xOffs := -1;
      yOffs := -1;
    end;

    OffsetRgn(RGNInner, Left + xOffs, Top + yOffs);
    OffsetRgn(RGNOuter, Left + xOffs, Top + yOffs);
  end;

  fRunOnce := False;
end;

procedure TRxHoleShape.SayAllDTEnabledState(EnabledDT: Boolean);
var
  i: Integer;
begin
  for i := 0 to TWinControl(Owner).ControlCount - 1 do
    with TWinControl(Owner) do
    begin
      if (Controls[i] is TRxHoleShape) then
      begin
        TRxHoleShape(Controls[i]).FEnabledDT := EnabledDT;
      end;
    end;
end;

procedure TRxHoleShape.UpdateRGN;
var
  i: Integer;
  NewRGN: HRGN;
begin
  if not (Owner is TWinControl) then Exit;
  NewRGN := CreateRectRgn(0, 0, 2000, 1000);

  for i := 0 to TWinControl(Owner).ControlCount - 1 do
    with TWinControl(Owner) do
    begin
      if Controls[i] is TRxHoleShape then
        with TRxHoleShape(Controls[i]) do
          if ((csDesigning in ComponentState) and FEnabledDT)
            or ((not (csDesigning in ComponentState)) and FEnabled) then
          begin
            DoCalcRegions;
            CombineRgn(NewRGN, NewRGN, RGNInner, aCombMode[Integer(FCombineMode)])
          end;
    end;

  SetWindowRgn(TWinControl(Owner).Handle, NewRGN, True);
  fNeedUpdateRGN := False;
end;

procedure TRxHoleShape.DoUpdateRegion;
begin
  if csLoading in ComponentState then Exit;
  UpdateRGN();
  Refresh;
end;

procedure TRxHoleShape.SmthChanged(Sender: TObject);
begin
  DoUpdateRegion;
end;

procedure TRxHoleShape.SetEnabled(Value: Boolean);
begin
  if (FEnabled = Value) or not (Owner is TWinControl) then Exit;
  FEnabled := Value;
  DoUpdateRegion;
end;

procedure TRxHoleShape.SetEnabledDT(Value: Boolean);
begin
  if (FEnabledDT = Value) or not (Owner is TWinControl) then Exit;
  FEnabledDT := Value;
  SayAllDTEnabledState(FEnabledDT);
  DoUpdateRegion;
end;

procedure TRxHoleShape.SetShape(Value: TRxHoleShapeType);
begin
  if FShape = Value then Exit;
  FShape := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.SetShapeBitmap(Value: TBitmap);
begin
  if FShapeBitmap = Value then Exit;
  fNeedRebuildBitmapShape := True;
  FShapeBitmap.Assign(Value);
  if Assigned(FShapeBitmap) then
  begin
    Width := FShapeBitmap.Width;
    Height := FShapeBitmap.Width;
  end;
  DoUpdateRegion();
end;

procedure TRxHoleShape.SetBevelInner(Value: TPanelBevel);
begin
  if FBevelInner = Value then Exit;
  FBevelInner := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.SetBevelOuter(Value: TPanelBevel);
begin
  if FBevelOuter = Value then Exit;
  FBevelOuter := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.SetBoldInner(Value: Boolean);
begin
  if FBoldInner = Value then Exit;
  FBoldInner := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.SetBoldOuter(Value: Boolean);
begin
  if FBoldOuter = Value then Exit;
  FBoldOuter := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.SetCombineMode(Value: TRxRegionCombineMode);
begin
  if FCombineMode = Value then Exit;
  FCombineMode := Value;
  DoUpdateRegion;
end;

procedure TRxHoleShape.SetBevelOffset(Value: Integer);
begin
  if (FBevelOffset = Value) or (Value < 0) then Exit;
  if (Value > width - 2) or (Value > height - 2) then
    Value := Min(Width, Height) - 2;
  FBevelOffset := Value; DoUpdateRegion;
end;

procedure TRxHoleShape.Loaded;
begin
  inherited;
  fNeedRebuildBitmapShape := True;
  UpdateRGN();
  Refresh;
end;

end.

