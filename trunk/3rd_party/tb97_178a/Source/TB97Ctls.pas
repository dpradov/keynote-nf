unit TB97Ctls;

{
  Toolbar97
  Copyright (C) 1998-2001 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  TToolbarButton97 & TEdit97

  $Id: TB97Ctls.pas,v 1.8 2001/05/01 17:00:49 jr Exp $
}

interface

{$I TB97Ver.inc}

uses
  Windows, Messages, Classes, Controls, Forms, Menus, Graphics, Buttons,
  {$IFDEF TB97D4} ImgList, ActnList, {$ENDIF} StdCtrls, ExtCtrls,
  TB97Vers;

const
  DefaultDropdownArrowWidth = 9;
type
  { TToolbarButton97 }

  TButtonDisplayMode = (dmBoth, dmGlyphOnly, dmTextOnly);
  TButtonState97 = (bsUp, bsDisabled, bsDown, bsExclusive, bsMouseIn);
  TNumGlyphs97 = 1..5;
  TButtonDropdownEvent = procedure(Sender: TObject;
    var ShowMenu, RemoveClicks: Boolean) of object;

  TToolbarButton97 = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FAlignment: TAlignment;
    FCancel: Boolean;
    FDefault: Boolean;
    FDisplayMode: TButtonDisplayMode;
    FDown: Boolean;
    FDropdownAlways: Boolean;
    FDropdownArrow: Boolean;
    FDropdownArrowWidth: Integer;
    FDropdownCombo: Boolean;
    FDropdownMenu: TPopupMenu;
    FFlat: Boolean;
    FGlyph: Pointer;
    FGroupIndex: Integer;
    FHelpContext: THelpContext;
    FHighlightWhenDown: Boolean;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FModalResult: TModalResult;
    FNoBorder: Boolean;
    FOldDisabledStyle: Boolean;
    FOpaque: Boolean;
    FRepeating: Boolean;
    FRepeatDelay, FRepeatInterval: Integer;
    FShowBorderWhenInactive: Boolean;
    FSpacing: Integer;
    FWordWrap: Boolean;
    FOnDropdown: TButtonDropdownEvent;
    FOnMouseEnter, FOnMouseExit: TNotifyEvent;
    { Internal }
    FInClick: Boolean;
    FMouseInControl: Boolean;
    FMouseIsDown: Boolean;
    FMenuIsDown: Boolean;
    FUsesDropdown: Boolean;
    FRepeatTimer: TTimer;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    procedure SetAlignment (Value: TAlignment);
    procedure SetAllowAllUp (Value: Boolean);
    function GetCallDormant: Boolean;
    procedure SetCallDormant (Value: Boolean);
    procedure SetDown (Value: Boolean);
    procedure SetDisplayMode (Value: TButtonDisplayMode);
    procedure SetDropdownAlways (Value: Boolean);
    procedure SetDropdownArrow (Value: Boolean);
    procedure SetDropdownArrowWidth (Value: Integer);
    procedure SetDropdownCombo (Value: Boolean);
    procedure SetDropdownMenu (Value: TPopupMenu);
    procedure SetFlat (Value: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph (Value: TBitmap);
    function GetGlyphMask: TBitmap;
    procedure SetGlyphMask (Value: TBitmap);
    procedure SetGroupIndex (Value: Integer);
    procedure SetHighlightWhenDown (Value: Boolean);
    function GetImageIndex: Integer;
    procedure SetImageIndex (Value: Integer);
    function GetImages: TCustomImageList;
    procedure SetImages (Value: TCustomImageList);
    procedure SetLayout (Value: TButtonLayout);
    procedure SetMargin (Value: Integer);
    procedure SetNoBorder (Value: Boolean);
    function GetNumGlyphs: TNumGlyphs97;
    procedure SetNumGlyphs (Value: TNumGlyphs97);
    procedure SetOldDisabledStyle (Value: Boolean);
    procedure SetOpaque (Value: Boolean);
    procedure SetSpacing (Value: Integer);
    function GetVersion: TToolbar97Version;
    procedure SetVersion (const Value: TToolbar97Version);
    procedure SetWordWrap (Value: Boolean);
    procedure RemoveButtonMouseTimer;
    procedure Redraw (const Erase: Boolean);
    function PointInButton (X, Y: Integer): Boolean;
    procedure ButtonMouseTimerHandler (Sender: TObject);
    procedure RepeatTimerHandler (Sender: TObject);
    {$IFDEF TB97D4}
    function IsCheckedStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsImageIndexStored: Boolean;
    {$ENDIF}
    procedure WMLButtonDblClk (var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar (var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey (var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged (var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange (var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMCancelMode (var Message: TWMCancelMode); message WM_CANCELMODE;
  protected
    FState: TButtonState97;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove (Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    {$IFDEF TB97D4}
    procedure ActionChange (Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure AssignTo (Dest: TPersistent); override;
    {$ENDIF}
  public
    property Canvas;
    property CallDormant: Boolean read GetCallDormant write SetCallDormant;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure MouseEntered;
    procedure MouseLeft;
  published
    {$IFDEF TB97D4}
    property Action;
    {$ENDIF}
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    {$IFDEF TB97D4}
    property Anchors;
    {$ENDIF}
    property Cancel: Boolean read FCancel write FCancel default False;
    property Color default clBtnFace;
    {$IFDEF TB97D4}
    property Constraints;
    {$ENDIF}
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Default: Boolean read FDefault write FDefault default False;
    property DisplayMode: TButtonDisplayMode read FDisplayMode write SetDisplayMode default dmBoth;
    property Down: Boolean read FDown write SetDown {$IFDEF TB97D4} stored IsCheckedStored {$ENDIF} default False;
    property DragCursor;
    property DragMode;
    property DropdownAlways: Boolean read FDropdownAlways write SetDropdownAlways default False;
    property DropdownArrow: Boolean read FDropdownArrow write SetDropdownArrow default True;
    property DropdownArrowWidth: Integer read FDropdownArrowWidth write SetDropdownArrowWidth default DefaultDropdownArrowWidth;
    property DropdownCombo: Boolean read FDropdownCombo write SetDropdownCombo default False;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphMask: TBitmap read GetGlyphMask write SetGlyphMask;
    property HelpContext: THelpContext read FHelpContext write FHelpContext {$IFDEF TB97D4} stored IsHelpContextStored {$ENDIF} default 0;
    property HighlightWhenDown: Boolean read FHighlightWhenDown write SetHighlightWhenDown default True;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex {$IFDEF TB97D4} stored IsImageIndexStored {$ENDIF} default -1;
    property Images: TCustomImageList read GetImages write SetImages;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property NoBorder: Boolean read FNoBorder write SetNoBorder default False;
    property NumGlyphs: TNumGlyphs97 read GetNumGlyphs write SetNumGlyphs default 1;
    property OldDisabledStyle: Boolean read FOldDisabledStyle write SetOldDisabledStyle default False;
    property Opaque: Boolean read FOpaque write SetOpaque default True;
    property ParentFont;
    property ParentColor default False;
    property ParentShowHint;
    property Repeating: Boolean read FRepeating write FRepeating default False;
    property RepeatDelay: Integer read FRepeatDelay write FRepeatDelay default 400;
    property RepeatInterval: Integer read FRepeatInterval write FRepeatInterval default 100;
    property ShowBorderWhenInactive: Boolean read FShowBorderWhenInactive write FShowBorderWhenInactive default False;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Version: TToolbar97Version read GetVersion write SetVersion stored False;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropdown: TButtonDropdownEvent read FOnDropdown write FOnDropdown;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

  { TToolButtonActionLink }

  {$IFDEF TB97D4}
  TToolbarButton97ActionLink = class(TControlActionLink)
  protected
    FClient: TToolbarButton97;
    procedure AssignClient (AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsHelpContextLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked (Value: Boolean); override;
    procedure SetHelpContext (Value: THelpContext); override;
    procedure SetImageIndex (Value: Integer); override;
  end;

  TToolbarButton97ActionLinkClass = class of TToolbarButton97ActionLink;
  {$ENDIF}

  { TEdit97 }

  TEdit97 = class(TCustomEdit)
  private
    MouseInControl: Boolean;
    function GetVersion: TToolbar97Version;
    procedure SetVersion (const Value: TToolbar97Version);
    procedure DrawNCArea (const DrawToDC: Boolean; const ADC: HDC;
      const Clip: HRGN);
    procedure NewAdjustHeight;
    procedure CMEnabledChanged (var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure WMPrint (var Message: TMessage); message WM_PRINT;
    procedure WMPrintClient (var Message: TMessage); message WM_PRINTCLIENT;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AutoSelect;
    {$IFDEF TB97D4}
    property Anchors;
    {$ENDIF}
    property Align;
    {$IFDEF TB97D4}
    property BiDiMode;
    {$ENDIF}
    property CharCase;
    {$IFDEF TB97D4}
    property Constraints;
    {$ENDIF}
    property DragCursor;
    {$IFDEF TB97D4}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    {$IFDEF TB97D3}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property MaxLength;
    property OEMConvert;
    {$IFDEF TB97D4}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Version: TToolbar97Version read GetVersion write SetVersion stored False;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF TB97D4}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF TB97D4}
    property OnStartDock;
    {$ENDIF}
    property OnStartDrag;
  end;

var
  ButtonsStayDown: Boolean = True;
  ButtonMouseInControl: TToolbarButton97 = nil;

function ControlIs97Control (AControl: TControl): Boolean;
procedure Register97ControlClass (AClass: TClass);
procedure Unregister97ControlClass (AClass: TClass);

implementation

uses
  SysUtils, Consts, CommCtrl, TB97Cmn, TntControls;

var
  { See TToolbarButton97.ButtonMouseTimerHandler for info on this }
  ButtonMouseTimer: TTimer = nil;

  Control97List: TList = nil;

  Edit97Count: Integer = 0;

const
  DropdownComboSpace = 2;

function ControlIs97Control (AControl: TControl): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Assigned(AControl) and Assigned(Control97List) then
    for I := 0 to Control97List.Count-1 do
      if AControl is TClass(Control97List[I]) then begin
        Result := True;
        Break;
      end;
end;

procedure Register97ControlClass (AClass: TClass);
begin
  if Control97List = nil then Control97List := TList.Create;
  Control97List.Add (AClass);
end;

procedure Unregister97ControlClass (AClass: TClass);
begin
  if Assigned(Control97List) then begin
    Control97List.Remove (AClass);
    if Control97List.Count = 0 then begin
      Control97List.Free;
      Control97List := nil;
    end;
  end;
end;

{ TToolbarButton97ActionLink - internal }

{$IFDEF TB97D4}
procedure TToolbarButton97ActionLink.AssignClient (AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TToolbarButton97;
end;

function TToolbarButton97ActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Down = (Action as TCustomAction).Checked);
end;

function TToolbarButton97ActionLink.IsHelpContextLinked: Boolean;
begin
  Result := inherited IsHelpContextLinked and
    (FClient.HelpContext = (Action as TCustomAction).HelpContext);
end;

function TToolbarButton97ActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TToolbarButton97ActionLink.SetChecked (Value: Boolean);
begin
  if IsCheckedLinked then FClient.Down := Value;
end;

procedure TToolbarButton97ActionLink.SetHelpContext (Value: THelpContext);
begin
  if IsHelpContextLinked then FClient.HelpContext := Value;
end;

procedure TToolbarButton97ActionLink.SetImageIndex (Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;
{$ENDIF}


{ TToolbarButton97 - internal }

type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize (AWidth, AHeight: Integer);
    destructor Destroy; override;
    function Add (Image, Mask: TBitmap): Integer;
    function AddMasked (Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete (Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TBoolInt = record
    B: Boolean;
    I: Integer;
  end;

  TCustomImageListAccess = class(TCustomImageList);

  TButtonGlyph = class
  private
    FOriginal, FOriginalMask: TBitmap;
    FCallDormant: Boolean;
    FGlyphList: array[Boolean] of TGlyphList;
    FImageIndex: Integer;
    FImageList: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FIndexs: array[Boolean, TButtonState97] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs97;
    FOnChange: TNotifyEvent;
    FOldDisabledStyle: Boolean;
    procedure GlyphChanged (Sender: TObject);
    procedure SetGlyph (Value: TBitmap);
    procedure SetGlyphMask (Value: TBitmap);
    procedure SetNumGlyphs (Value: TNumGlyphs97);
    procedure UpdateNumGlyphs;
    procedure Invalidate;
    function CreateButtonGlyph (State: TButtonState97): TBoolInt;
    procedure DrawButtonGlyph (Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState97);
    procedure DrawButtonText (Canvas: TCanvas;
      const Caption: string; TextBounds: TRect;
      WordWrap: Boolean; Alignment: TAlignment; State: TButtonState97);
    procedure DrawButtonDropArrow (Canvas: TCanvas; const X, Y, AWidth: Integer;
      State: TButtonState97);
    procedure CalcButtonLayout (Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; DrawGlyph, DrawCaption: Boolean;
      const Caption: string; WordWrap: Boolean;
      Layout: TButtonLayout; Margin, Spacing: Integer; DropArrow: Boolean;
      DropArrowWidth: Integer; var GlyphPos, ArrowPos: TPoint;
      var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { returns the text rectangle }
    function Draw (Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      DrawGlyph, DrawCaption: Boolean; const Caption: string; WordWrap: Boolean;
      Alignment: TAlignment; Layout: TButtonLayout; Margin, Spacing: Integer;
      DropArrow: Boolean; DropArrowWidth: Integer; State: TButtonState97): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property GlyphMask: TBitmap read FOriginalMask write SetGlyphMask;
    property NumGlyphs: TNumGlyphs97 read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize (AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.Add (Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace (Result, Image, Mask);
  Inc (FCount);
end;

function TGlyphList.AddMasked (Image: TBitmap; MaskColor: TColor): Integer;
  procedure BugfreeReplaceMasked (Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    procedure CheckImage (Image: TGraphic);
    begin
      if Image = nil then Exit;
      if (Image.Height < Height) or (Image.Width < Width) then
        raise EInvalidOperation.Create({$IFNDEF TB97D3}LoadStr{$ENDIF}(SInvalidImageSize));
    end;
  var
    TempIndex: Integer;
    Image, Mask: TBitmap;
  begin
    if HandleAllocated then begin
      CheckImage(NewImage);
      TempIndex := inherited AddMasked(NewImage, MaskColor);
      if TempIndex <> -1 then
        try
          Image := nil;
          Mask := nil;
          try
            Image := TBitmap.Create;
            Image.Height := Height;
            Image.Width := Width;
            Mask := TBitmap.Create;
            Mask.Monochrome := True;
            { ^ Prevents the "invisible glyph" problem when used with certain
                color schemes. (Fixed in Delphi 3.01) }
            Mask.Height := Height;
            Mask.Width := Width;
            ImageList_Draw (Handle, TempIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
            ImageList_Draw (Handle, TempIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
            if not ImageList_Replace(Handle, Index, Image.Handle, Mask.Handle) then
              raise EInvalidOperation.Create({$IFNDEF TB97D3}LoadStr{$ENDIF}(SReplaceImage));
          finally
            Image.Free;
            Mask.Free;
          end;
        finally
          inherited Delete(TempIndex);
        end
      else
        raise EInvalidOperation.Create({$IFNDEF TB97D3}LoadStr{$ENDIF}(SReplaceImage));
    end;
    Change;
  end;
begin
  Result := AllocateIndex;
  { This works two very serious bugs in the Delphi 2/BCB and Delphi 3
    implementations of the ReplaceMasked method. In the Delphi 2 and BCB
    versions of the ReplaceMasked method, it incorrectly uses ILD_NORMAL as
    the last parameter for the second ImageList_Draw call, in effect causing
    all white colors to be considered transparent also. And in the Delphi 2/3
    and BCB versions it doesn't set Monochrome to True on the Mask bitmap,
    causing the bitmaps to be invisible on certain color schemes. }
  BugfreeReplaceMasked (Result, Image, MaskColor);
  Inc (FCount);
end;

procedure TGlyphList.Delete (Index: Integer);
begin
  if Used[Index] then begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;
  Pattern: TBitmap = nil;
  PatternBtnFace, PatternBtnHighlight: TColor;
  ButtonCount: Integer = 0;

procedure CreateBrushPattern;
var
  X, Y: Integer;
begin
  PatternBtnFace := GetSysColor(COLOR_BTNFACE);
  PatternBtnHighlight := GetSysColor(COLOR_BTNHIGHLIGHT);
  Pattern := TBitmap.Create;
  with Pattern do begin
    Width := 8;
    Height := 8;
    with Canvas do begin
      Brush.Style := bsSolid;
      Brush.Color := clBtnFace;
      FillRect (Rect(0, 0, Width, Height));
      for Y := 0 to 7 do
        for X := 0 to 7 do
          if Odd(Y) = Odd(X) then  { toggles between even/odd pixels }
            Pixels[X, Y] := clBtnHighlight;     { on even/odd rows }
    end;
  end;
end;


{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  B: Boolean;
  I: TButtonState97;
begin
  inherited;
  FCallDormant := True;
  FImageIndex := -1;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FOriginalMask := TBitmap.Create;
  FOriginalMask.OnChange := GlyphChanged;
  FNumGlyphs := 1;
  for B := False to True do
    for I := Low(I) to High(I) do
      FIndexs[B, I] := -1;
  if GlyphCache = nil then
    GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginalMask.Free;
  FOriginal.Free;
  FImageChangeLink.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited;
end;

procedure TButtonGlyph.Invalidate;
var
  B: Boolean;
  I: TButtonState97;
begin
  for B := False to True do begin
    for I := Low(I) to High(I) do 
      if FIndexs[B, I] <> -1 then begin
        FGlyphList[B].Delete (FIndexs[B, I]);
        FIndexs[B, I] := -1;
      end;
    GlyphCache.ReturnList (FGlyphList[B]);
    FGlyphList[B] := nil;
  end;
end;

procedure TButtonGlyph.GlyphChanged (Sender: TObject);
begin
  if (Sender = FOriginal) and (FOriginal.Width <> 0) and (FOriginal.Height <> 0) then
    FTransparentColor := FOriginal.Canvas.Pixels[0, FOriginal.Height-1] or $02000000;
  Invalidate;
  if Assigned(FOnChange) then FOnChange (Self);
end;

procedure TButtonGlyph.UpdateNumGlyphs;
var
  Glyphs: Integer;
begin
  if (FOriginal.Width <> 0) and (FOriginal.Height <> 0) and
     (FOriginal.Width mod FOriginal.Height = 0) then begin
    Glyphs := FOriginal.Width div FOriginal.Height;
    if Glyphs > High(TNumGlyphs97) then Glyphs := 1;
  end
  else
    Glyphs := 1;
  SetNumGlyphs (Glyphs);
end;

procedure TButtonGlyph.SetGlyph (Value: TBitmap);
begin
  Invalidate;
  FOriginal.Assign (Value);
  UpdateNumGlyphs;
end;

procedure TButtonGlyph.SetGlyphMask (Value: TBitmap);
begin
  Invalidate;
  FOriginalMask.Assign (Value);
end;

procedure TButtonGlyph.SetNumGlyphs (Value: TNumGlyphs97);
begin
  Invalidate;
  if (FImageList <> nil) or (Value < Low(TNumGlyphs97)) or
     (Value > High(TNumGlyphs97)) then
    FNumGlyphs := 1
  else
    FNumGlyphs := Value;
  GlyphChanged (nil);
end;

function TButtonGlyph.CreateButtonGlyph (State: TButtonState97): TBoolInt;
const
  ROP_DSPDxax = $00E20746;
  ROP_PSDPxax = $00B8074A;
  ROP_DSna = $00220326;  { D & ~S }

  procedure GenerateMaskBitmapFromDIB (const MaskBitmap, SourceBitmap: TBitmap;
    const SourceOffset, SourceSize: TPoint; TransColors: array of TColor);
  { This a special procedure meant for generating monochrome masks from
    >4 bpp color DIB sections. Because each video driver seems to sport its own
    interpretation of how to handle DIB sections, a workaround procedure like
    this was necessary. }
  type
    TColorArray = array[0..536870910] of TColorRef;
  var
    Info: packed record
      Header: TBitmapInfoHeader;
      Colors: array[0..1] of TColorRef;
    end;
    W, H: Integer;
    I, Y, X: Integer;
    Pixels: ^TColorArray;
    Pixel: ^TColorRef;
    MonoPixels: Pointer;
    MonoPixel, StartMonoPixel: ^Byte;
    MonoScanLineSize, CurBit: Integer;
    DC: HDC;
    MaskBmp: HBITMAP;
  begin
    W := SourceBitmap.Width;
    H := SourceBitmap.Height;
    MonoScanLineSize := SourceSize.X div 8;
    if SourceSize.X mod 8 <> 0 then
      Inc (MonoScanLineSize);
    if MonoScanLineSize mod 4 <> 0 then  { Compensate for scan line boundary }
      MonoScanLineSize := (MonoScanLineSize and not 3) + 4;
    MonoPixels := AllocMem(MonoScanLineSize * SourceSize.Y);  { AllocMem is used because it initializes to zero }
    try
      GetMem (Pixels, W * H * 4);
      try
        FillChar (Info, SizeOf(Info), 0);
        with Info do begin
          with Header do begin
            biSize := SizeOf(TBitmapInfoHeader);
            biWidth := W;
            biHeight := -H;  { negative number makes it a top-down DIB }
            biPlanes := 1;
            biBitCount := 32;
            {biCompression := BI_RGB;}  { implied due to the FillChar zeroing }
          end;
          {Colors[0] := clBlack;}  { implied due to the FillChar zeroing }
          Colors[1] := clWhite;
        end;
        DC := CreateCompatibleDC(0);
        GetDIBits (DC, SourceBitmap.Handle, 0, H, Pixels, PBitmapInfo(@Info)^,
          DIB_RGB_COLORS);
        DeleteDC (DC);

        for I := 0 to High(TransColors) do
          if TransColors[I] = -1 then
            TransColors[I] := Pixels[W * (H-1)] and $FFFFFF;
              { ^ 'and' operation is necessary because the high byte is undefined }

        MonoPixel := MonoPixels;
        for Y := SourceOffset.Y to SourceOffset.Y+SourceSize.Y-1 do begin
          StartMonoPixel := MonoPixel;
          CurBit := 7;
          Pixel := @Pixels[(Y * W) + SourceOffset.X];
          for X := 0 to SourceSize.X-1 do begin
            for I := 0 to High(TransColors) do
              if Pixel^ and $FFFFFF = Cardinal(TransColors[I]) then begin
                { ^ 'and' operation is necessary because the high byte is undefined }
                MonoPixel^ := MonoPixel^ or (1 shl CurBit);
                Break;
              end;
            Dec (CurBit);
            if CurBit < 0 then begin
              Inc (Integer(MonoPixel));
              CurBit := 7;
            end;
            Inc (Integer(Pixel), SizeOf(Longint));  { proceed to the next pixel }
          end;
          Integer(MonoPixel) := Integer(StartMonoPixel) + MonoScanLineSize;
        end;
      finally
        FreeMem (Pixels);
      end;

      { Write new bits into a new HBITMAP, and assign this handle to MaskBitmap }
      MaskBmp := CreateBitmap(SourceSize.X, SourceSize.Y, 1, 1, nil);
      with Info.Header do begin
        biWidth := SourceSize.X;
        biHeight := -SourceSize.Y;  { negative number makes it a top-down DIB }
        biPlanes := 1;
        biBitCount := 1;
      end;
      DC := CreateCompatibleDC(0);
      SetDIBits (DC, MaskBmp, 0, SourceSize.Y, MonoPixels, PBitmapInfo(@Info)^,
        DIB_RGB_COLORS);
      DeleteDC (DC);
    finally
      FreeMem (MonoPixels);
    end;

    MaskBitmap.Handle := MaskBmp;
  end;
  procedure GenerateMaskBitmap (const MaskBitmap, SourceBitmap: TBitmap;
    const SourceOffset, SourceSize: TPoint; const TransColors: array of TColor);
  { Returns handle of a monochrome bitmap, with pixels in SourceBitmap of color
    TransColor set to white in the resulting bitmap. All other colors of
    SourceBitmap are set to black in the resulting bitmap. This uses the
    regular ROP_DSPDxax BitBlt method. }
  var
    CanvasHandle: HDC;
    SaveBkColor: TColorRef;
    DC: HDC;
    MaskBmp, SaveBmp: HBITMAP;
    I: Integer;
  const
    ROP: array[Boolean] of DWORD = (SRCPAINT, SRCCOPY);
  begin
    CanvasHandle := SourceBitmap.Canvas.Handle;

    MaskBmp := CreateBitmap(SourceSize.X, SourceSize.Y, 1, 1, nil);
    DC := CreateCompatibleDC(0);
    SaveBmp := SelectObject(DC, MaskBmp);
    SaveBkColor := GetBkColor(CanvasHandle);
    for I := 0 to High(TransColors) do begin
      SetBkColor (CanvasHandle, ColorToRGB(TransColors[I]));
      BitBlt (DC, 0, 0, SourceSize.X, SourceSize.Y, CanvasHandle,
        SourceOffset.X, SourceOffset.Y, ROP[I = 0]);
    end;
    SetBkColor (CanvasHandle, SaveBkColor);
    SelectObject (DC, SaveBmp);
    DeleteDC (DC);

    MaskBitmap.Handle := MaskBmp;
  end;
  procedure ReplaceBitmapColorsFromMask (const MaskBitmap, DestBitmap: TBitmap;
    const DestOffset, DestSize: TPoint; const ReplaceColor: TColor);
  var
    DestDC: HDC;
    SaveBrush: HBRUSH;
    SaveTextColor, SaveBkColor: TColorRef;
  begin
    DestDC := DestBitmap.Canvas.Handle;

    SaveBrush := SelectObject(DestDC, CreateSolidBrush(ColorToRGB(ReplaceColor)));
    SaveTextColor := SetTextColor(DestDC, clBlack);
    SaveBkColor := SetBkColor(DestDC, clWhite);
    BitBlt (DestDC, DestOffset.X, DestOffset.Y, DestSize.X, DestSize.Y,
      MaskBitmap.Canvas.Handle, 0, 0, ROP_DSPDxax);
    SetBkColor (DestDC, SaveBkColor);
    SetTextColor (DestDC, SaveTextColor);
    DeleteObject (SelectObject(DestDC, SaveBrush));
  end;
  function CopyBitmapToDDB (const SourceBitmap: TBitmap): TBitmap;
  { Makes a device-dependent duplicate of SourceBitmap. The color palette,
    if any, is preserved. }
  var
    SB: HBITMAP;
    SavePalette: HPALETTE;
    DC: HDC;
    BitmapInfo: packed record
      Header: TBitmapInfoHeader;
      Colors: array[0..255] of TColorRef;
    end;
    Bits: Pointer;
  begin
    Result := TBitmap.Create;
    try
      Result.Palette := CopyPalette(SourceBitmap.Palette);
      Result.Width := SourceBitmap.Width;
      Result.Height := SourceBitmap.Height;
      SB := SourceBitmap.Handle;
      if SB = 0 then Exit;  { it would have a null handle if its width or height was zero }
      SavePalette := 0;
      DC := CreateCompatibleDC(0);
      try
        if Result.Palette <> 0 then begin
          SavePalette := SelectPalette(DC, Result.Palette, False);
          RealizePalette (DC);
        end;
        BitmapInfo.Header.biSize := SizeOf(TBitmapInfoHeader);
        BitmapInfo.Header.biBitCount := 0;  { instructs GetDIBits not to fill in the color table }
        { First retrieve the BitmapInfo header only }
        if GetDIBits(DC, SB, 0, 0, nil, PBitmapInfo(@BitmapInfo)^, DIB_RGB_COLORS) <> 0 then begin
          GetMem (Bits, BitmapInfo.Header.biSizeImage);
          try
            { Then read the actual bits }
            if GetDIBits(DC, SB, 0, SourceBitmap.Height, Bits, PBitmapInfo(@BitmapInfo)^, DIB_RGB_COLORS) <> 0 then
              { And copy them to the resulting bitmap }
              SetDIBits (DC, Result.Handle, 0, SourceBitmap.Height, Bits, PBitmapInfo(@BitmapInfo)^, DIB_RGB_COLORS);
          finally
            FreeMem (Bits);
          end;
        end;
      finally
        if SavePalette <> 0 then SelectPalette (DC, SavePalette, False);
        DeleteDC (DC);
      end;
    except
      Result.Free;
      raise;
    end;
  end;
const
  ROPs: array[Boolean] of DWORD = (ROP_PSDPxax, ROP_DSPDxax);
var
  OriginalBmp, OriginalMaskBmp, TmpImage, DDB, MonoBmp, MaskBmp, UseMaskBmp: TBitmap;
  I: TButtonState97;
  B: Boolean;
  AddPixels, IWidth, IHeight, IWidthA, IHeightA: Integer;
  IRect, IRectA, SourceRect, R: TRect;
  DC: HDC;
  UsesMask: Boolean;
{$IFDEF TB97D3}
  IsHighColorDIB: Boolean;
{$ELSE}
const
  IsHighColorDIB = False;
{$ENDIF}
begin
  if (State <> bsDisabled) and (Ord(State) >= NumGlyphs) then
    State := bsUp;
  Result.B := True;
  Result.I := FIndexs[True, State];
  if Result.I = -1 then begin
    Result.B := False;
    Result.I := FIndexs[False, State];
  end;
  if Result.I <> -1 then Exit;
  if FImageList = nil then begin
    if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
    UsesMask := (FOriginalMask.Width <> 0) and (FOriginalMask.Height <> 0);
  end
  else begin
    if (FImageIndex < 0) or (FImageIndex >= FImageList.Count) then Exit;
    UsesMask := False;
  end;
  B := State <> bsDisabled;
  { + AddPixels is to make sure the highlight color on generated disabled glyphs
    doesn't get cut off }
  if FImageList = nil then begin
    IWidthA := FOriginal.Width div FNumGlyphs;
    IHeightA := FOriginal.Height;
  end
  else begin
    IWidthA := TCustomImageListAccess(FImageList).Width;
    IHeightA := TCustomImageListAccess(FImageList).Height;
  end;
  IRectA := Rect(0, 0, IWidthA, IHeightA);
  AddPixels := Ord(State = bsDisabled);
  IWidth := IWidthA + AddPixels;
  IHeight := IHeightA + AddPixels;
  IRect := Rect(0, 0, IWidth, IHeight);
  if FGlyphList[B] = nil then begin
    if GlyphCache = nil then
      GlyphCache := TGlyphCache.Create;
    FGlyphList[B] := GlyphCache.GetList(IWidth, IHeight);
  end;
  {$IFDEF TB97D3}
  IsHighColorDIB := (FImageList = nil) and (FOriginal.PixelFormat > pf4bit);
  {$ENDIF}
  OriginalBmp := nil;
  OriginalMaskBmp := nil;
  TmpImage := nil;
  MaskBmp := nil;
  try
    OriginalBmp := TBitmap.Create;
    OriginalBmp.Assign (FOriginal);
    OriginalMaskBmp := TBitmap.Create;
    OriginalMaskBmp.Assign (FOriginalMask);
    TmpImage := TBitmap.Create;
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    TmpImage.Canvas.Brush.Color := clBtnFace;
    if FImageList = nil then
      TmpImage.Palette := CopyPalette(OriginalBmp.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    SourceRect := Bounds(Ord(I) * IWidthA, 0, IWidthA, IHeightA);
    if FImageList <> nil then begin
      MaskBmp := TBitmap.Create;
      MaskBmp.Monochrome := True;
      MaskBmp.Width := IWidthA;
      MaskBmp.Height := IHeightA;
      ImageList_Draw (FImageList.Handle, FImageIndex, MaskBmp.Canvas.Handle,
        0, 0, ILD_MASK);
    end;

    if State <> bsDisabled then begin
      if FImageList = nil then begin
        TmpImage.Canvas.CopyRect (IRectA, OriginalBmp.Canvas, SourceRect);
        if not UsesMask then begin
          {$IFDEF TB97D3}
          { Use clDefault instead of FTransparentColor whereever possible to
            ensure compatibility with all video drivers when using high-color
            (> 4 bpp) DIB glyphs }
          FIndexs[B, State] := FGlyphList[B].AddMasked(TmpImage, clDefault);
          {$ELSE}
          FIndexs[B, State] := FGlyphList[B].AddMasked(TmpImage, FTransparentColor);
          {$ENDIF}
        end
        else begin
          MonoBmp := TBitmap.Create;
          try
            MonoBmp.Monochrome := True;
            MonoBmp.Width := IWidth;
            MonoBmp.Height := IHeight;
            MonoBmp.Canvas.CopyRect (IRectA, OriginalMaskBmp.Canvas, SourceRect);
            FIndexs[B, State] := FGlyphList[B].Add(TmpImage, MonoBmp);
          finally
            MonoBmp.Free;
          end;
        end;
      end
      else begin
        ImageList_Draw (FImageList.Handle, FImageIndex, TmpImage.Canvas.Handle,
          0, 0, ILD_NORMAL);
        FIndexs[B, State] := FGlyphList[B].Add(TmpImage, MaskBmp);
      end;
    end
    else begin
      MonoBmp := nil;
      DDB := nil;
      try
        MonoBmp := TBitmap.Create;
        { Uses the CopyBitmapToDDB to work around a Delphi 3 flaw. If you copy
          a DIB to a second bitmap via Assign, change the HandleType of the
          second bitmap to bmDDB, then try to read the Handle property, Delphi
          converts it back to a DIB. }
        if FImageList = nil then
          DDB := CopyBitmapToDDB(OriginalBmp)
        else begin
          DDB := TBitmap.Create;
          DDB.Width := IWidthA;
          DDB.Height := IHeightA;
          ImageList_Draw (FImageList.Handle, FImageIndex, DDB.Canvas.Handle,
            0, 0, ILD_NORMAL);
        end;
        if NumGlyphs > 1 then
          with TmpImage.Canvas do begin
            CopyRect (IRectA, DDB.Canvas, SourceRect);

            { Convert white to clBtnHighlight }
            if not IsHighColorDIB then
              GenerateMaskBitmap (MonoBmp, DDB, SourceRect.TopLeft,
                IRectA.BottomRight, [GetNearestColor(OriginalBmp.Canvas.Handle, clWhite)])
            else
              GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, SourceRect.TopLeft,
                IRectA.BottomRight, [clWhite]);
            ReplaceBitmapColorsFromMask (MonoBmp, TmpImage, IRectA.TopLeft,
              IRectA.BottomRight, clBtnHighlight);

            { Convert gray to clBtnShadow }
            if not IsHighColorDIB then
              GenerateMaskBitmap (MonoBmp, DDB, SourceRect.TopLeft,
                IRectA.BottomRight, [GetNearestColor(OriginalBmp.Canvas.Handle, clGray)])
            else
              GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, SourceRect.TopLeft,
                IRectA.BottomRight, [clGray]);
            ReplaceBitmapColorsFromMask (MonoBmp, TmpImage, IRectA.TopLeft,
              IRectA.BottomRight, clBtnShadow);

            if not UsesMask then begin
              { Generate the transparent mask in MonoBmp. The reason why
                it doesn't just use a mask color is because the mask needs
                to be of the glyph -before- the clBtnHighlight/Shadow were
                translated }
              if not IsHighColorDIB then
                GenerateMaskBitmap (MonoBmp, DDB,
                  SourceRect.TopLeft, IRectA.BottomRight, FTransparentColor)
              else
                GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                  SourceRect.TopLeft, IRectA.BottomRight, [-1]);
            end
            else
              MonoBmp.Canvas.CopyRect (IRectA, OriginalMaskBmp.Canvas, SourceRect);
            with MonoBmp do begin
              Width := Width + AddPixels;
              Height := Height + AddPixels;
              { Set the additional bottom and right row on disabled glyph
                masks to white so that it always shines through, since the
                bottom and right row on TmpImage was left uninitialized }
              Canvas.Pen.Color := clWhite;
              Canvas.PolyLine ([Point(0, Height-1), Point(Width-1, Height-1),
                Point(Width-1, -1)]);
            end;

            FIndexs[B, State] := FGlyphList[B].Add(TmpImage, MonoBmp);
          end
        else begin
          { Create a disabled version }
          if FOldDisabledStyle then begin
            { "Old" TSpeedButton style }
            if FImageList = nil then begin
              if not UsesMask then begin
                if IsHighColorDIB then
                  GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                    SourceRect.TopLeft, IRectA.BottomRight, [clBlack])
                else begin
                  with MonoBmp do begin
                    Assign (DDB);  { must be a DDB for this to work right }
                    Canvas.Brush.Color := clBlack;
                    Monochrome := True;
                  end;
                end;
              end
              else begin
                MonoBmp.Assign (DDB);  { must be a DDB for this to work right }
                with TBitmap.Create do
                  try
                    Monochrome := True;
                    Width := OriginalMaskBmp.Width;
                    Height := OriginalMaskBmp.Height;
                    R := Rect(0, 0, Width, Height);
                    Canvas.CopyRect (R, OriginalMaskBmp.Canvas, R);
                    DC := Canvas.Handle;
                    with MonoBmp.Canvas do begin
                      BitBlt (Handle, 0, 0, IWidthA, IHeightA, DC,
                        SourceRect.Left, SourceRect.Top, ROP_DSna);
                      BitBlt (Handle, 0, 0, IWidthA, IHeightA, DC,
                        SourceRect.Left, SourceRect.Top, SRCPAINT);
                    end;
                  finally
                    Free;
                  end;
                MonoBmp.Canvas.Brush.Color := clBlack;
                MonoBmp.Monochrome := True;
              end
            end
            else begin
              with MonoBmp do begin
                Width := IWidthA;
                Height := IHeightA;
                Canvas.Brush.Color := clWhite;
                Canvas.FillRect (IRectA);
                ImageList_Draw (FImageList.Handle, FImageIndex, Canvas.Handle,
                  0, 0, ILD_TRANSPARENT);
                Canvas.Brush.Color := clBlack;
                Monochrome := True;
              end;
            end;
          end
          else begin
            { The new Office 97 / MFC look }
            if not UsesMask and (FImageList = nil) then begin
              with TmpImage.Canvas do begin
                if not IsHighColorDIB then
                  GenerateMaskBitmap (MonoBmp, DDB, IRectA.TopLeft,
                    IRectA.BottomRight, [FTransparentColor, clWhite, clSilver])
                else
                  GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp,
                    SourceRect.TopLeft, IRectA.BottomRight, [-1, clWhite, clSilver]);
              end;
            end
            else begin
              { Generate the mask in MonoBmp. Make clWhite and clSilver transparent. }
              if not IsHighColorDIB then
                GenerateMaskBitmap (MonoBmp, DDB, SourceRect.TopLeft,
                  IRectA.BottomRight, [clWhite, clSilver])
              else
                GenerateMaskBitmapFromDIB (MonoBmp, OriginalBmp, SourceRect.TopLeft,
                  IRectA.BottomRight, [clWhite, clSilver]);
              if FImageList = nil then
                UseMaskBmp := OriginalMaskBmp
              else
                UseMaskBmp := MaskBmp;
              { and all the white colors in UseMaskBmp }
              with TBitmap.Create do
                try
                  Monochrome := True;
                  Width := UseMaskBmp.Width;
                  Height := UseMaskBmp.Height;
                  R := Rect(0, 0, Width, Height);
                  Canvas.CopyRect (R, UseMaskBmp.Canvas, R);
                  DC := Canvas.Handle;
                  with MonoBmp.Canvas do begin
                    BitBlt (Handle, 0, 0, IWidthA, IHeightA, DC,
                      SourceRect.Left, SourceRect.Top, ROP_DSna);
                    BitBlt (Handle, 0, 0, IWidthA, IHeightA, DC,
                      SourceRect.Left, SourceRect.Top, SRCPAINT);
                  end;
                finally
                  Free;
                end;
            end;
          end;

          with TmpImage.Canvas do begin
            Brush.Color := clBtnFace;
            FillRect (IRect);
            Brush.Color := clBtnHighlight;
            DC := Handle;
            SetTextColor (DC, clBlack);
            SetBkColor (DC, clWhite);
            BitBlt (DC, 1, 1, IWidthA, IHeightA,
              MonoBmp.Canvas.Handle, 0, 0, ROPs[FOldDisabledStyle]);
            Brush.Color := clBtnShadow;
            DC := Handle;
            SetTextColor (DC, clBlack);
            SetBkColor (DC, clWhite);
            BitBlt (DC, 0, 0, IWidthA, IHeightA,
              MonoBmp.Canvas.Handle, 0, 0, ROPs[FOldDisabledStyle]);
          end;

          FIndexs[B, State] := FGlyphList[B].AddMasked(TmpImage, clBtnFace);
        end;
      finally
        DDB.Free;
        MonoBmp.Free;
      end;
    end;
  finally
    MaskBmp.Free;
    TmpImage.Free;
    OriginalMaskBmp.Free;
    OriginalBmp.Free;
  end;
  Result.B := B;
  Result.I := FIndexs[B, State];
  { Note: Due to a bug in graphics.pas, Delphi 2's VCL crashes if Dormant is
    called on an empty bitmap, so to prevent this it must check Width/Height
    first }
  if {$IFNDEF TB97D3} (FOriginal.Width <> 0) and (FOriginal.Height <> 0) and {$ENDIF}
     FCallDormant then
    FOriginal.Dormant;
  {$IFNDEF TB97D3} if (FOriginalMask.Width <> 0) and (FOriginalMask.Height <> 0) then {$ENDIF}
    FOriginalMask.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph (Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState97);
var
  Index: TBoolInt;
begin
  Index := CreateButtonGlyph(State);
  if Index.I <> -1 then
    ImageList_DrawEx (FGlyphList[Index.B].Handle, Index.I, Canvas.Handle,
      GlyphPos.X, GlyphPos.Y, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT);
end;

procedure TButtonGlyph.DrawButtonText (Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; WordWrap: Boolean; Alignment: TAlignment;
  State: TButtonState97);
const
  AlignmentFlags: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  Format: UINT;
begin
  Format := DT_VCENTER or AlignmentFlags[Alignment];
  if not WordWrap then
    Format := Format or DT_SINGLELINE
  else
    Format := Format or DT_WORDBREAK;
  with Canvas do begin
    Brush.Style := bsClear;
    if State = bsDisabled then begin
      OffsetRect (TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText (Handle, PChar(Caption), Length(Caption), TextBounds, Format);
      OffsetRect (TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText (Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    end
    else
      DrawText (Handle, PChar(Caption), Length(Caption), TextBounds, Format);
  end;
end;

procedure TButtonGlyph.DrawButtonDropArrow (Canvas: TCanvas;
  const X, Y, AWidth: Integer; State: TButtonState97);
var
  X2: Integer;
begin
  with Canvas do begin
    X2 := X + AWidth div 2;
    if State = bsDisabled then begin
      Pen.Color := clBtnHighlight;
      Brush.Color := clBtnHighlight;
      Polygon ([Point(X2-1, Y+1), Point(X2+3, Y+1), Point(X2+1, Y+3)]);
      Pen.Color := clBtnShadow;
      Brush.Color := clBtnShadow;
      Polygon ([Point(X2-2, Y), Point(X2+2, Y), Point(X2, Y+2)]);
    end
    else begin
      Pen.Color := Font.Color;
      Brush.Color := Font.Color;
      Polygon ([Point(X2-2, Y), Point(X2+2, Y), Point(X2, Y+2)]);
    end;
  end;
end;

procedure TButtonGlyph.CalcButtonLayout (Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; DrawGlyph, DrawCaption: Boolean; const Caption: string;
  WordWrap: Boolean; Layout: TButtonLayout; Margin, Spacing: Integer;
  DropArrow: Boolean; DropArrowWidth: Integer; var GlyphPos, ArrowPos: TPoint;
  var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize, ArrowSize: TPoint;
  HasGlyph: Boolean;
  TotalSize: TPoint;
  Format: UINT;
  Margin1, Spacing1: Integer;
  LayoutLeftOrRight: Boolean;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right-Client.Left, Client.Bottom-Client.Top);

  GlyphSize.X := 0;
  GlyphSize.Y := 0;
  if DrawGlyph then begin
    if FImageList = nil then begin
      if FOriginal <> nil then begin
        GlyphSize.X := FOriginal.Width div FNumGlyphs;
        GlyphSize.Y := FOriginal.Height;
      end;
    end
    else begin
      GlyphSize.X := TCustomImageListAccess(FImageList).Width;
      GlyphSize.Y := TCustomImageListAccess(FImageList).Height;
    end;
  end;
  HasGlyph := (GlyphSize.X <> 0) and (GlyphSize.Y <> 0);

  if DropArrow then begin
    ArrowSize.X := DropArrowWidth;
    ArrowSize.Y := 3;
  end
  else begin
    ArrowSize.X := 0;
    ArrowSize.Y := 0;
  end;

  LayoutLeftOrRight := Layout in [blGlyphLeft, blGlyphRight];
  if not LayoutLeftOrRight and not HasGlyph then begin
    Layout := blGlyphLeft;
    LayoutLeftOrRight := True;
  end;

  if DrawCaption and (Caption <> '') then begin
    TextBounds := Rect(0, 0, Client.Right-Client.Left, 0);
    if LayoutLeftOrRight then
      Dec (TextBounds.Right, ArrowSize.X);
    Format := DT_CALCRECT;
    if WordWrap then begin
      Format := Format or DT_WORDBREAK;
      Margin1 := 4;
      if LayoutLeftOrRight and HasGlyph then begin
        if Spacing = -1 then
          Spacing1 := 4
        else
          Spacing1 := Spacing;
        Dec (TextBounds.Right, GlyphSize.X + Spacing1);
        if Margin <> -1 then
          Margin1 := Margin
        else
        if Spacing <> -1 then
          Margin1 := Spacing;
      end;
      Dec (TextBounds.Right, Margin1 * 2);
    end;
    DrawText (Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Format);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if LayoutLeftOrRight then begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X - ArrowSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
    if not HasGlyph then
      ArrowPos.X := TextPos.X + TextSize.X
    else
      ArrowPos.X := GlyphPos.X + GlyphSize.X;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (TextSize.Y = 0) or not HasGlyph then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then begin
    if Spacing = -1 then begin
      TotalSize := Point(GlyphSize.X + TextSize.X + ArrowSize.X,
        GlyphSize.Y + TextSize.Y);
      if LayoutLeftOrRight then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X + ArrowSize.X,
        GlyphSize.Y + Spacing + TextSize.Y);
      if LayoutLeftOrRight then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else begin
    if Spacing = -1 then begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X + ArrowSize.X),
        ClientSize.Y - (Margin + GlyphSize.Y));
      if LayoutLeftOrRight then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft: begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
        ArrowPos.X := TextPos.X + TextSize.X;
      end;
    blGlyphRight: begin
        ArrowPos.X := ClientSize.X - Margin - ArrowSize.X;
        GlyphPos.X := ArrowPos.X - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop: begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom: begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  Inc (ArrowPos.X);
  if not HasGlyph then
    ArrowPos.Y := TextPos.Y + (TextSize.Y - ArrowSize.Y) div 2
  else
    ArrowPos.Y := GlyphPos.Y + (GlyphSize.Y - ArrowSize.Y) div 2;

  { fixup the result variables }
  with GlyphPos do begin
    Inc (X, Client.Left + Offset.X);
    Inc (Y, Client.Top + Offset.Y);
  end;
  with ArrowPos do begin
    Inc (X, Client.Left + Offset.X);
    Inc (Y, Client.Top + Offset.Y);
  end;
  OffsetRect (TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TButtonGlyph.Draw (Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; DrawGlyph, DrawCaption: Boolean; const Caption: string;
  WordWrap: Boolean; Alignment: TAlignment; Layout: TButtonLayout;
  Margin, Spacing: Integer; DropArrow: Boolean; DropArrowWidth: Integer;
  State: TButtonState97): TRect;
var
  GlyphPos, ArrowPos: TPoint;
begin
  CalcButtonLayout (Canvas, Client, Offset, DrawGlyph, DrawCaption, Caption,
    WordWrap, Layout, Margin, Spacing, DropArrow, DropArrowWidth, GlyphPos,
    ArrowPos, Result);
  if DrawGlyph then
    DrawButtonGlyph (Canvas, GlyphPos, State);
  if DrawCaption then
    DrawButtonText (Canvas, Caption, Result, WordWrap, Alignment, State);
  if DropArrow then
    DrawButtonDropArrow (Canvas, ArrowPos.X, ArrowPos.Y, DropArrowWidth, State);
end;


{ TDropdownList }

{$IFNDEF TB97D4}

type
  TDropdownList = class(TComponent)
  private
    List: TList;
    Window: HWND;
    procedure WndProc (var Message: TMessage);
  protected
    procedure Notification (AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMenu (Menu: TPopupMenu);
  end;
var
  DropdownList: TDropdownList;

constructor TDropdownList.Create (AOwner: TComponent);
begin
  inherited;
  List := TList.Create;
end;

destructor TDropdownList.Destroy;
begin
  inherited;
  if Window <> 0 then
    DeallocateHWnd (Window);
  List.Free;
end;

procedure TDropdownList.WndProc (var Message: TMessage);
{ This procedure is based on code from TPopupList.WndProc (menus.pas) }
var
  I: Integer;
  MenuItem: TMenuItem;
  FindKind: TFindItemKind;
  ContextID: Integer;
begin
  try
    with List do
      case Message.Msg of
        WM_COMMAND:
          for I := 0 to Count-1 do
            if TPopupMenu(Items[I]).DispatchCommand(TWMCommand(Message).ItemID) then
              Exit;
        WM_INITMENUPOPUP:
          for I := 0 to Count-1 do
            if TPopupMenu(Items[I]).DispatchPopup(TWMInitMenuPopup(Message).MenuPopup) then
              Exit;
        WM_MENUSELECT:
          with TWMMenuSelect(Message) do begin
            FindKind := fkCommand;
            if MenuFlag and MF_POPUP <> 0 then
              FindKind := fkHandle;
            for I := 0 to Count-1 do begin
              MenuItem := TPopupMenu(Items[I]).FindItem(IDItem, FindKind);
              if MenuItem <> nil then begin
                Application.Hint := MenuItem.Hint;
                Exit;
              end;
            end;
            Application.Hint := '';
          end;
        WM_HELP:
          with TWMHelp(Message).HelpInfo^ do begin
            for I := 0 to Count-1 do
              if TPopupMenu(Items[I]).Handle = hItemHandle then begin
                ContextID := TPopupMenu(Items[I]).GetHelpContext(iCtrlID, True);
                if ContextID = 0 then
                  ContextID := TPopupMenu(Items[I]).GetHelpContext(hItemHandle, False);
                if Screen.ActiveForm = nil then Exit;
                if (biHelp in Screen.ActiveForm.BorderIcons) then
                  Application.HelpCommand (HELP_CONTEXTPOPUP, ContextID)
                else
                  Application.HelpContext (ContextID);
                Exit;
              end;
          end;
      end;
    with Message do
      Result := DefWindowProc(Window, Msg, wParam, lParam);
  except
    Application.HandleException (Self);
  end;
end;

procedure TDropdownList.AddMenu (Menu: TPopupMenu);
begin
  if List.IndexOf(Menu) = -1 then begin
    if Window = 0 then
      Window := AllocateHWnd(WndProc);
    Menu.FreeNotification (Self);
    List.Add (Menu);
  end;
end;

procedure TDropdownList.Notification (AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    List.Remove (AComponent);
    if (List.Count = 0) and (Window <> 0) then begin
      DeallocateHWnd (Window);
      Window := 0;
    end;
  end;
end;

{$ENDIF}


{ TToolbarButton97 }

procedure ButtonHookProc (Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);
var
  P: TPoint;
begin
  case Code of
    hpSendActivateApp:
      if (WParam = 0) and Assigned(ButtonMouseInControl) and
         not ButtonMouseInControl.FShowBorderWhenInactive then
        ButtonMouseInControl.MouseLeft;
    hpPostMouseMove: begin
        if Assigned(ButtonMouseInControl) then begin
          GetCursorPos (P);
          if FindDragTarget(P, True) <> ButtonMouseInControl then
            ButtonMouseInControl.MouseLeft;
        end;
      end;
  end;
end;

constructor TToolbarButton97.Create (AOwner: TComponent);
begin
  inherited;

  if ButtonMouseTimer = nil then begin
    ButtonMouseTimer := TTimer.Create(nil);
    ButtonMouseTimer.Enabled := False;
    ButtonMouseTimer.Interval := 125;  { 8 times a second }
  end;

  InstallHookProc (ButtonHookProc, [hpSendActivateApp, hpPostMouseMove],
    csDesigning in ComponentState);

  SetBounds (Left, Top, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks, csOpaque];
  Color := clBtnFace;
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  ParentFont := True;
  FAlignment := taCenter;
  FFlat := True;
  FHighlightWhenDown := True;
  FOpaque := True;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FDropdownArrow := True;
  FDropdownArrowWidth := DefaultDropdownArrowWidth;
  FRepeatDelay := 400;
  FRepeatInterval := 100;
  Inc (ButtonCount);
end;

destructor TToolbarButton97.Destroy;
begin
  RemoveButtonMouseTimer;
  TButtonGlyph(FGlyph).Free;
  { The Notification method, which is sometimes called while the component is
    being destroyed, reads FGlyph and expects it to be valid, so it must be
    reset to nil }
  FGlyph := nil;
  UninstallHookProc (ButtonHookProc);
  Dec (ButtonCount);
  if ButtonCount = 0 then begin
    Pattern.Free;
    Pattern := nil;
    ButtonMouseTimer.Free;
    ButtonMouseTimer := nil;
  end;
  inherited;
end;

procedure TToolbarButton97.Paint;
const
  EdgeStyles: array[Boolean, Boolean] of UINT = (
    (EDGE_RAISED, EDGE_SUNKEN),
    (BDR_RAISEDINNER, BDR_SUNKENOUTER));
  FlagStyles: array[Boolean] of UINT = (BF_RECT or BF_SOFT or BF_MIDDLE, BF_RECT);
var
  UseBmp: Boolean;
  Bmp: TBitmap;
  DrawCanvas: TCanvas;
  PaintRect, R: TRect;
  Offset: TPoint;
  StateDownOrExclusive, DropdownComboShown, UseDownAppearance, DrawBorder: Boolean;
begin
  UseBmp := FOpaque or not FFlat;
  if UseBmp then
    Bmp := TBitmap.Create
  else
    Bmp := nil;
  try
    if UseBmp then begin
      Bmp.Width := Width;
      Bmp.Height := Height;
      DrawCanvas := Bmp.Canvas;
      with DrawCanvas do begin
        Brush.Color := Color;
        FillRect (ClientRect);
      end;
    end
    else
      DrawCanvas := Canvas;
    DrawCanvas.Font := Self.Font;
    PaintRect := Rect(0, 0, Width, Height);

    StateDownOrExclusive := FState in [bsDown, bsExclusive];
    DropdownComboShown := FDropdownCombo and FUsesDropdown;
    UseDownAppearance := (FState = bsExclusive) or
      ((FState = bsDown) and (not DropdownComboShown or not FMenuIsDown));
    DrawBorder := (csDesigning in ComponentState) or
      (not FNoBorder and (not FFlat or StateDownOrExclusive or (FMouseInControl and (FState <> bsDisabled))));

    if DropdownComboShown then begin
      if DrawBorder then begin
        R := PaintRect;
        Dec (R.Right, DropdownComboSpace);
        R.Left := R.Right - DropdownArrowWidth;
        DrawEdge (DrawCanvas.Handle, R,
          EdgeStyles[FFlat, StateDownOrExclusive and FMenuIsDown],
          FlagStyles[FFlat]);
      end;
      Dec (PaintRect.Right, DropdownArrowWidth + DropdownComboSpace);
    end;
    if DrawBorder then
      DrawEdge (DrawCanvas.Handle, PaintRect, EdgeStyles[FFlat, UseDownAppearance],
        FlagStyles[FFlat]);
    if not FNoBorder then begin
      if FFlat then
        InflateRect (PaintRect, -1, -1)
      else
        InflateRect (PaintRect, -2, -2);
    end;

    if UseDownAppearance then begin
      if (FState = bsExclusive) and (not FFlat or not FMouseInControl) and
         not FMenuIsDown and FHighlightWhenDown then begin
        if Pattern = nil then CreateBrushPattern;
        DrawCanvas.Brush.Bitmap := Pattern;
        DrawCanvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else begin
      Offset.X := 0;
      Offset.Y := 0;
    end;

    TButtonGlyph(FGlyph).Draw (DrawCanvas, PaintRect, Offset,
      FDisplayMode <> dmTextOnly, FDisplayMode <> dmGlyphOnly,
      Caption, FWordWrap, FAlignment, FLayout, FMargin, FSpacing,
      FDropdownArrow and not FDropdownCombo and FUsesDropdown,
      DropdownArrowWidth, FState);
    if DropdownComboShown then
      TButtonGlyph(FGlyph).DrawButtonDropArrow (DrawCanvas, Width-DropdownArrowWidth-2,
        Height div 2 - 1, DropdownArrowWidth, FState);

    if UseBmp then
      Canvas.Draw (0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TToolbarButton97.RemoveButtonMouseTimer;
begin
  if ButtonMouseInControl = Self then begin
    ButtonMouseTimer.Enabled := False;
    ButtonMouseInControl := nil;
  end;
end;

(* no longer used
procedure TToolbarButton97.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then begin
    GetCursorPos (P);
    { Use FindDragTarget instead of PtInRect since we want to check based on
      the Z order }
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      MouseLeft
    else
      MouseEntered;
  end;
end;
*)

procedure TToolbarButton97.Loaded;
var
  State: TButtonState97;
begin
  inherited;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph (State);
end;

procedure TToolbarButton97.Notification (AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = DropdownMenu then DropdownMenu := nil;
    if Assigned(FGlyph) and (AComponent = Images) then Images := nil;
  end;
end;

function TToolbarButton97.PointInButton (X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and
    (X < ClientWidth-((DropdownArrowWidth+DropdownComboSpace) * Ord(FDropdownCombo and FUsesDropdown))) and
    (Y >= 0) and (Y < ClientHeight);
end;

procedure TToolbarButton97.MouseDown (Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not Enabled then begin
    inherited;
    Exit;
  end;
  if Button <> mbLeft then begin
    MouseEntered;
    inherited;
  end
  else begin
    { We know mouse has to be over the control if the mouse went down. }
    MouseEntered;
    FMenuIsDown := FUsesDropdown and (not FDropdownCombo or
      (X >= Width-(DropdownArrowWidth+DropdownComboSpace)));
    try
      if not FDown then begin
        FState := bsDown;
        Redraw (True);
      end
      else
        if FAllowAllUp then
          Redraw (True);
      if not FMenuIsDown then
        FMouseIsDown := True;
      inherited;
      if FMenuIsDown then
        Click
      else
        if FRepeating then begin
          Click;
          if not Assigned(FRepeatTimer) then
            FRepeatTimer := TTimer.Create(Self);
          FRepeatTimer.Enabled := False;
          FRepeatTimer.Interval := FRepeatDelay;
          FRepeatTimer.OnTimer := RepeatTimerHandler;
          FRepeatTimer.Enabled := True;
        end;
    finally
      FMenuIsDown := False;
    end;
  end;
end;

procedure TToolbarButton97.MouseMove (Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  NewState: TButtonState97;
  PtInButton: Boolean;
begin
  inherited;

  { Check if mouse just entered the control. It works better to check this
    in MouseMove rather than using CM_MOUSEENTER, since the VCL doesn't send
    a CM_MOUSEENTER in all cases
    Use FindDragTarget instead of PtInRect since we want to check based on
    the Z order }
  P := ClientToScreen(Point(X, Y));
  if (ButtonMouseInControl <> Self) and (FindDragTarget(P, True) = Self) then begin
    if Assigned(ButtonMouseInControl) then
      ButtonMouseInControl.MouseLeft;
    { Like Office 97, only draw the active borders when the application is active }
    if FShowBorderWhenInactive or ApplicationIsActive then begin
      ButtonMouseInControl := Self;
      ButtonMouseTimer.OnTimer := ButtonMouseTimerHandler;
      ButtonMouseTimer.Enabled := True;
      MouseEntered;
    end;
  end;

  if FMouseIsDown then begin
    PtInButton := PointInButton(X, Y);
    if PtInButton and Assigned(FRepeatTimer) then
      FRepeatTimer.Enabled := True;
    if FDown then
      NewState := bsExclusive
    else begin
      if PtInButton then
        NewState := bsDown
      else
        NewState := bsUp;
    end;
    if NewState <> FState then begin
      FState := NewState;
      Redraw (True);
    end;
  end;
end;

procedure TToolbarButton97.RepeatTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
  FRepeatTimer.Interval := FRepeatInterval;
  GetCursorPos (P);
  P := ScreenToClient(P);
  if Repeating and FMouseIsDown and MouseCapture and PointInButton(P.X, P.Y) then
    Click
  else
    FRepeatTimer.Enabled := False;
end;

procedure TToolbarButton97.WMCancelMode (var Message: TWMCancelMode);
begin
  FRepeatTimer.Free;
  FRepeatTimer := nil;
  if FMouseIsDown then begin
    FMouseIsDown := False;
    MouseLeft;
  end;
  { Delphi's default processing of WM_CANCELMODE sends a "fake" WM_LBUTTONUP
    message to the control, so inherited must only be called after setting
    FMouseIsDown to False }
  inherited;
end;

procedure TToolbarButton97.MouseUp (Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FRepeatTimer.Free;
  FRepeatTimer := nil;
  { Remove active border when right button is clicked }
  if (Button = mbRight) and Enabled then begin
    FMouseIsDown := False;
    MouseLeft;
  end;
  inherited;
  if (Button = mbLeft) and FMouseIsDown then begin
    FMouseIsDown := False;
    if PointInButton(X, Y) and not FRepeating then
      Click
    else
      MouseLeft;
  end;
end;

procedure TToolbarButton97.Click;
{$IFNDEF TB97D4}
const
  { TPM_RIGHTBUTTON works better on Windows 3.x }
  ButtonFlags: array[Boolean] of UINT = (TPM_RIGHTBUTTON, TPM_LEFTBUTTON);
  AlignFlags: array[TPopupAlignment] of UINT = (TPM_LEFTALIGN, TPM_RIGHTALIGN,
    TPM_CENTERALIGN);
{$ENDIF}
var
  Popup, ShowMenu, RemoveClicks: Boolean;
  SaveAlignment: TPopupAlignment;
  {$IFDEF TB97D4}
  SaveTrackButton: TTrackButton;
  {$ENDIF}
  PopupPt: TPoint;
  RepostList: TList; {pointers to TMsg's}
  Msg: TMsg;
  Repost: Boolean;
  I: Integer;
  P: TPoint;
  Form: {$IFDEF TB97D3} TCustomForm {$ELSE} TForm {$ENDIF};
  DockPos: TGetToolbarDockPosType;
begin
  if FRepeating and not FMenuIsDown then begin
    inherited;
    Exit;
  end;
  FInClick := True;
  try
    if (GroupIndex <> 0) and not FMenuIsDown then
      SetDown (not FDown);

    Popup := FUsesDropdown and (not FDropdownCombo or FMenuIsDown);
    if ButtonsStayDown or Popup then begin
      if FState in [bsUp, bsMouseIn] then begin
        FState := bsDown;
        Redraw (True);
      end;
    end
    else begin
      if FState = bsDown then begin
        if FDown and (FGroupIndex <> 0) then
          FState := bsExclusive
        else
          FState := bsUp;
        Redraw (True);
      end;
    end;

    { Stop tracking }
    MouseLeft;
    if not Popup then begin
      Form := GetParentForm(Self);
      if Form <> nil then Form.ModalResult := ModalResult;
      inherited;
    end
    else begin
      if not FDropdownCombo then
        inherited;
      { It must release its capture before displaying the popup menu since
        this control uses csCaptureMouse. If it doesn't, the VCL seems to
        get confused and think the mouse is still captured even after the
        popup menu is displayed, causing mouse problems after the menu is
        dismissed. }
      MouseCapture := False;
      ShowMenu := Assigned(FDropdownMenu);
      RemoveClicks := True;
      if Assigned(FOnDropdown) then
        FOnDropdown (Self, ShowMenu, RemoveClicks);
      try
        if Assigned(FDropdownMenu) and ShowMenu then begin
          SaveAlignment := DropdownMenu.Alignment;
          {$IFDEF TB97D4}
          SaveTrackButton := DropdownMenu.TrackButton;
          {$ENDIF}
          try
            DropdownMenu.Alignment := paLeft;
            PopupPt := Point(0, Height);
            if Assigned(GetToolbarDockPosProc) then begin
              DockPos := GetToolbarDockPosProc(Parent);
              { Drop out right or left side }
              case DockPos of
                gtpLeft: PopupPt := Point(Width, 0);
                gtpRight: begin
                    PopupPt := Point(0, 0);
                    DropdownMenu.Alignment := paRight;
                  end;
              end;
            end;
            PopupPt := ClientToScreen(PopupPt);
            with DropdownMenu do begin
              PopupComponent := Self;
              { In Delphi versions prior to 4 it avoids using the Popup method
                of TPopupMenu because it always uses the "track right button"
                flag (which disallowed the "click and drag" selecting motion many
                people are accustomed to). Delphi 4 has a TrackButton property
                to control the tracking button, so it can use the Popup method. }
              {$IFNDEF TB97D4}
              if (ClassType = TPopupMenu) and Assigned(DropdownList) then begin
                if Assigned(OnPopup) then
                  OnPopup (DropdownMenu);
                TrackPopupMenu (Handle, AlignFlags[Alignment] or ButtonFlags[NewStyleControls],
                  PopupPt.X, PopupPt.Y, 0, DropdownList.Window, nil)
              end
              else begin
              {$ELSE}
                if NewStyleControls then
                  TrackButton := tbLeftButton
                else
                  TrackButton := tbRightButton;
              {$ENDIF}
                Popup (PopupPt.X, PopupPt.Y);
              {$IFNDEF TB97D4}
              end;
              {$ENDIF}
            end;
          finally
            DropdownMenu.Alignment := SaveAlignment;
            {$IFDEF TB97D4}
            DropdownMenu.TrackButton := SaveTrackButton;
            {$ENDIF}
          end;
        end;
      finally
        if RemoveClicks then begin
          { To prevent a mouse click from redisplaying the menu, filter all
            mouse up/down messages, and repost the ones that don't need
            removing. This is sort of bulky, but it's the only way I could
            find that works perfectly and like Office 97. }
          RepostList := TList.Create;
          try
            while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_MBUTTONDBLCLK,
               PM_REMOVE or PM_NOYIELD) do
               { ^ The WM_LBUTTONDOWN to WM_MBUTTONDBLCLK range encompasses all
                 of the DOWN and DBLCLK messages for the three buttons }
              with Msg do begin
                Repost := True;
                case Message of
                  WM_QUIT: begin
                      { Throw back any WM_QUIT messages }
                      PostQuitMessage (wParam);
                      Break;
                    end;
                  WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
                  WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
                  WM_MBUTTONDOWN, WM_MBUTTONDBLCLK: begin
                      P := SmallPointToPoint(TSmallPoint(lParam));
                      Windows.ClientToScreen (hwnd, P);
                      if FindDragTarget(P, True) = Self then
                        Repost := False;
                    end;
                end;
                if Repost then begin
                  RepostList.Add (AllocMem(SizeOf(TMsg)));
                  PMsg(RepostList.Last)^ := Msg;
                end;
              end;
          finally
            for I := 0 to RepostList.Count-1 do begin
              with PMsg(RepostList[I])^ do
                PostMessage (hwnd, message, wParam, lParam);
              FreeMem (RepostList[I]);
            end;
            RepostList.Free;
          end;
        end;
      end;
    end;
  finally
    FInClick := False;
    if FState = bsDown then
      FState := bsUp;
    { Need to check if it's destroying in case the OnClick handler freed
      the button. If it doesn't check this here, it can sometimes cause an
      access violation }
    if not(csDestroying in ComponentState) then begin
      Redraw (True);
      MouseLeft;
    end;
  end;
end;

function TToolbarButton97.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TToolbarButton97.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TToolbarButton97.SetGlyph (Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Redraw (True);
end;

function TToolbarButton97.GetGlyphMask: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).GlyphMask;
end;

procedure TToolbarButton97.SetGlyphMask (Value: TBitmap);
begin
  TButtonGlyph(FGlyph).GlyphMask := Value;
  Redraw (True);
end;

procedure TToolbarButton97.SetHighlightWhenDown (Value: Boolean);
begin
  if FHighlightWhenDown <> Value then begin
    FHighlightWhenDown := Value;
    if Down then
      Redraw (True);
  end;
end;

function TToolbarButton97.GetImageIndex: Integer;
begin
  Result := TButtonGlyph(FGlyph).FImageIndex;
end;

procedure TToolbarButton97.SetImageIndex (Value: Integer);
begin
  if TButtonGlyph(FGlyph).FImageIndex <> Value then begin
    TButtonGlyph(FGlyph).FImageIndex := Value;
    if Assigned(TButtonGlyph(FGlyph).FImageList) then
      TButtonGlyph(FGlyph).GlyphChanged (nil);
  end;
end;

function TToolbarButton97.GetImages: TCustomImageList;
begin
  Result := TButtonGlyph(FGlyph).FImageList;
end;

procedure TToolbarButton97.SetImages (Value: TCustomImageList);
begin
  with TButtonGlyph(FGlyph) do
    if FImageList <> Value then begin
      if FImageList <> nil then
        FImageList.UnRegisterChanges (FImageChangeLink);
      FImageList := Value;
      if FImageList <> nil then begin
        if FImageChangeLink = nil then begin
          FImageChangeLink := TChangeLink.Create;
          FImageChangeLink.OnChange := GlyphChanged;
        end;
        FImageList.RegisterChanges (FImageChangeLink);
        FImageList.FreeNotification (Self);
      end
      else begin
        FImageChangeLink.Free;
        FImageChangeLink := nil;
      end;
      UpdateNumGlyphs;
    end;
end;

function TToolbarButton97.GetNumGlyphs: TNumGlyphs97;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TToolbarButton97.SetNumGlyphs (Value: TNumGlyphs97);
begin
  if Value < Low(TNumGlyphs97) then
    Value := Low(TNumGlyphs97)
  else
  if Value > High(TNumGlyphs97) then
    Value := High(TNumGlyphs97);
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.GlyphChanged(Sender: TObject);
begin
  Redraw (True);
end;

procedure TToolbarButton97.UpdateExclusive;
var
  I: Integer;
  Ctl: TControl;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
    with Parent do
      for I := 0 to ControlCount-1 do begin
        Ctl := Controls[I];
        if (Ctl <> Self) and (Ctl is TToolbarButton97) then
          with TToolbarButton97(Ctl) do
            if FGroupIndex = Self.FGroupIndex then begin
              if Self.Down and FDown then begin
                FDown := False;
                FState := bsUp;
                Redraw (True);
              end;
              FAllowAllUp := Self.AllowAllUp;
            end;
      end;
end;

procedure TToolbarButton97.SetDown (Value: Boolean);
begin
  if FGroupIndex = 0 then
    Value := False;
  if Value <> FDown then begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if not Enabled then
      FState := bsDisabled
    else begin
      if Value then
        FState := bsExclusive
      else
        FState := bsUp;
    end;
    Redraw (True);
    if Value then UpdateExclusive;
  end;
end;

procedure TToolbarButton97.SetFlat (Value: Boolean);
begin
  if FFlat <> Value then begin
    FFlat := Value;
    if FOpaque or not FFlat then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetGroupIndex (Value: Integer);
begin
  if FGroupIndex <> Value then begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TToolbarButton97.SetLayout (Value: TButtonLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetMargin (Value: Integer);
begin
  if (FMargin <> Value) and (Value >= -1) then begin
    FMargin := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetNoBorder (Value: Boolean);
begin
  if FNoBorder <> Value then begin
    FNoBorder := Value;
    Invalidate;
  end;
end;

procedure TToolbarButton97.SetOldDisabledStyle (Value: Boolean);
begin
  if FOldDisabledStyle <> Value then begin
    FOldDisabledStyle := Value;
    with TButtonGlyph(FGlyph) do begin
      FOldDisabledStyle := Value;
      Invalidate;
    end;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetOpaque (Value: Boolean);
begin
  if FOpaque <> Value then begin
    FOpaque := Value;
    if FOpaque or not FFlat then
      ControlStyle := ControlStyle + [csOpaque]
    else
      ControlStyle := ControlStyle - [csOpaque];
    Invalidate;
  end;
end;

procedure TToolbarButton97.Redraw (const Erase: Boolean);
var
  AddedOpaque: Boolean;
begin
  if FOpaque or not FFlat or not Erase then begin
    { Temporarily add csOpaque to the style. This prevents Invalidate from
      erasing, which isn't needed when Erase is false. }
    AddedOpaque := False;
    if not(csOpaque in ControlStyle) then begin
      AddedOpaque := True;
      ControlStyle := ControlStyle + [csOpaque];
    end;
    try
      Invalidate;
    finally
      if AddedOpaque then
        ControlStyle := ControlStyle - [csOpaque];
    end;
  end
  else
  if not(FOpaque or not FFlat) then
    Invalidate;
end;

procedure TToolbarButton97.SetSpacing (Value: Integer);
begin
  if Value <> FSpacing then begin
    FSpacing := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetAllowAllUp (Value: Boolean);
begin
  if FAllowAllUp <> Value then begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TToolbarButton97.SetDropdownMenu (Value: TPopupMenu);
var
  NewUsesDropdown: Boolean;
begin
  if FDropdownMenu <> Value then begin
    FDropdownMenu := Value;
    if Assigned(Value) then begin
      Value.FreeNotification (Self);
      {$IFNDEF TB97D4}
      if DropdownList = nil then
        DropdownList := TDropdownList.Create(nil);
      DropdownList.AddMenu (Value);
      {$ENDIF}
    end;
    NewUsesDropdown := FDropdownAlways or Assigned(Value);
    if FUsesDropdown <> NewUsesDropdown then begin
      FUsesDropdown := NewUsesDropdown;
      if FDropdownArrow or FDropdownCombo then
        Redraw (True);
    end;
  end;
end;

procedure TToolbarButton97.SetWordWrap (Value: Boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetAlignment (Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetDropdownAlways (Value: Boolean);
var
  NewUsesDropdown: Boolean;
begin
  if FDropdownAlways <> Value then begin
    FDropdownAlways := Value;
    NewUsesDropdown := Value or Assigned(FDropdownMenu);
    if FUsesDropdown <> NewUsesDropdown then begin
      FUsesDropdown := NewUsesDropdown;
      if FDropdownArrow or FDropdownCombo then
        Redraw (True);
    end;
  end;
end;

procedure TToolbarButton97.SetDropdownArrow (Value: Boolean);
begin
  if FDropdownArrow <> Value then begin
    FDropdownArrow := Value;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetDropdownArrowWidth (Value: Integer);
var
  Diff: Integer;
begin
  if Value < 7 then Value := 7;
  if FDropdownArrowWidth <> Value then begin
    Diff := Value - FDropdownArrowWidth;
    FDropdownArrowWidth := Value;
    if not(csLoading in ComponentState) and FDropdownCombo then
      Width := Width + Diff;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetDropdownCombo (Value: Boolean);
var
  W: Integer;
begin
  if FDropdownCombo <> Value then begin
    FDropdownCombo := Value;
    if not(csLoading in ComponentState) then begin
      if Value then
        Width := Width + (DropdownArrowWidth + DropdownComboSpace)
      else begin
        W := Width - (DropdownArrowWidth + DropdownComboSpace);
        if W < 1 then W := 1;
        Width := W;
      end;
    end;
    Redraw (True);
  end;
end;

procedure TToolbarButton97.SetDisplayMode (Value: TButtonDisplayMode);
begin
  if FDisplayMode <> Value then begin
    FDisplayMode := Value;
    Redraw (True);
  end;
end;

function TToolbarButton97.GetCallDormant: Boolean;
begin
  Result := TButtonGlyph(FGlyph).FCallDormant;
end;

procedure TToolbarButton97.SetCallDormant (Value: Boolean);
begin
  TButtonGlyph(FGlyph).FCallDormant := Value;
end;

function TToolbarButton97.GetVersion: TToolbar97Version;
begin
  Result := Toolbar97VersionPropText;
end;

procedure TToolbarButton97.SetVersion (const Value: TToolbar97Version);
begin
  { write method required for the property to show up in Object Inspector }
end;

{$IFDEF TB97D4}
function TToolbarButton97.IsCheckedStored: Boolean;
begin
  Result := (ActionLink = nil) or not TToolbarButton97ActionLink(ActionLink).IsCheckedLinked;
end;

function TToolbarButton97.IsHelpContextStored: Boolean;
begin
  Result := (ActionLink = nil) or not TToolbarButton97ActionLink(ActionLink).IsHelpContextLinked;
end;

function TToolbarButton97.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not TToolbarButton97ActionLink(ActionLink).IsImageIndexLinked;
end;

procedure TToolbarButton97.ActionChange (Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if not CheckDefaults or (Self.Down = False) then
        Self.Down := Checked;
      if not CheckDefaults or (Self.HelpContext = 0) then
        Self.HelpContext := HelpContext;
      if not CheckDefaults or (Self.ImageIndex = -1) then
        Self.ImageIndex := ImageIndex;
    end;
end;

function TToolbarButton97.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TToolbarButton97ActionLink;
end;

procedure TToolbarButton97.AssignTo (Dest: TPersistent);
begin
  inherited;
  if Dest is TCustomAction then
    TCustomAction(Dest).Checked := Self.Down;
end;
{$ENDIF}

procedure TToolbarButton97.WMLButtonDblClk (var Message: TWMLButtonDblClk);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TToolbarButton97.CMEnabledChanged (var Message: TMessage);
begin
  if not Enabled then begin
    FState := bsDisabled;
    FMouseInControl := False;
    FMouseIsDown := False;
    RemoveButtonMouseTimer;
    Perform (WM_CANCELMODE, 0, 0);
  end
  else
  if FState = bsDisabled then
    if FDown and (FGroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Redraw (True);
end;

procedure TToolbarButton97.CMDialogChar (var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Assigned(Parent) and Parent.CanFocus and
       Enabled and Visible and (DisplayMode <> dmGlyphOnly) then begin
      { NOTE: There is a bug in TSpeedButton where accelerator keys are still
        processed even when the button is not visible. The 'and Visible'
        corrects it, so TToolbarButton97 doesn't have this problem. }
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TToolbarButton97.CMDialogKey (var Message: TCMDialogKey);
begin
  with Message do
    if (((CharCode = VK_RETURN) and FDefault) or
        ((CharCode = VK_ESCAPE) and FCancel)) and
       (KeyDataToShiftState(Message.KeyData) = []) and
       Assigned(Parent) and Parent.CanFocus and Enabled and Visible then begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TToolbarButton97.CMFontChanged (var Message: TMessage);
begin
  Redraw (True);
end;

procedure TToolbarButton97.CMTextChanged (var Message: TMessage);
begin
  Redraw (True);
end;

procedure TToolbarButton97.CMSysColorChange (var Message: TMessage);
begin
  inherited;
  if Assigned(Pattern) and
     ((PatternBtnFace <> TColor(GetSysColor(COLOR_BTNFACE))) or
      (PatternBtnHighlight <> TColor(GetSysColor(COLOR_BTNHIGHLIGHT)))) then begin
    Pattern.Free;
    Pattern := nil;
  end;
  with TButtonGlyph(FGlyph) do begin
    Invalidate;
    CreateButtonGlyph (FState);
  end;
end;

procedure TToolbarButton97.MouseEntered;
begin
  if Enabled and not FMouseInControl then begin
    FMouseInControl := True;
    if FState = bsUp then
      FState := bsMouseIn;
    if FFlat or (NumGlyphs >= 5) then
      Redraw (FDown or (NumGlyphs >= 5));
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter (Self);
  end;
end;

procedure TToolbarButton97.MouseLeft;
var
  OldState: TButtonState97;
begin
  if Enabled and FMouseInControl and not FMouseIsDown then begin
    FMouseInControl := False;
    RemoveButtonMouseTimer;
    OldState := FState;
    if (FState = bsMouseIn) or (not FInClick and (FState = bsDown)) then begin
      if FDown and (FGroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;
    end;
    if FFlat or ((NumGlyphs >= 5) or ((OldState = bsMouseIn) xor (FState <> OldState))) then
      Redraw (True);
    if Assigned(FOnMouseExit) then
      FOnMouseExit (Self);
  end;
end;

procedure TToolbarButton97.ButtonMouseTimerHandler (Sender: TObject);
var
  P: TPoint;
begin
  { The button mouse timer is used to periodically check if mouse has left.
    Normally it receives a CM_MOUSELEAVE, but the VCL does not send a
    CM_MOUSELEAVE if the mouse is moved quickly from the button to another
    application's window. For some reason, this problem doesn't seem to occur
    on Windows NT 4 -- only 95 and 3.x.

    The timer (which ticks 8 times a second) is only enabled when the
    application is active and the mouse is over a button, so it uses virtually
    no processing power.

    For something interesting to try: If you want to know just how often this
    is called, try putting a Beep call in here }

  GetCursorPos (P);
  if FindDragTarget(P, True) <> Self then
    MouseLeft;
end;

function TToolbarButton97.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TToolbarButton97.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TToolbarButton97.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;

{ TEdit97 - internal }

constructor TEdit97.Create (AOwner: TComponent);
begin
  inherited;
  AutoSize := False;
  Ctl3D := False;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed]; {fixes a VCL bug with Win 3.x}
  Height := 19;
  if Edit97Count = 0 then
    Register97ControlClass (TEdit97);
  Inc (Edit97Count);
end;

destructor TEdit97.Destroy;
begin
  Dec (Edit97Count);
  if Edit97Count = 0 then
    Unregister97ControlClass (TEdit97);
  inherited;
end;

procedure TEdit97.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  MouseInControl := True;
  DrawNCArea (False, 0, 0);
end;

procedure TEdit97.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  MouseInControl := False;
  DrawNCArea (False, 0, 0);
end;

procedure TEdit97.NewAdjustHeight;
var
  DC: HDC;
  SaveFont: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics (DC, Metrics);
  SelectObject (DC, SaveFont);
  ReleaseDC (0, DC);

  Height := Metrics.tmHeight + 6;
end;

procedure TEdit97.Loaded;
begin
  inherited;
  if not(csDesigning in ComponentState) then
    NewAdjustHeight;
end;

procedure TEdit97.CMEnabledChanged (var Message: TMessage);
const
  EnableColors: array[Boolean] of TColor = (clBtnFace, clWindow);
begin
  inherited;
  Color := EnableColors[Enabled];
  { Ensure non-client area is invalidated as well }
  if HandleAllocated then
    RedrawWindow (Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE or
      RDW_NOCHILDREN);
end;

procedure TEdit97.CMFontChanged (var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then
    NewAdjustHeight;
end;

procedure TEdit97.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    DrawNCArea (False, 0, 0);
end;

procedure TEdit97.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    DrawNCArea (False, 0, 0);
end;

procedure TEdit97.WMNCCalcSize (var Message: TWMNCCalcSize);
begin
  InflateRect (Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TEdit97.WMNCPaint (var Message: TMessage);
begin
  DrawNCArea (False, 0, HRGN(Message.WParam));
end;

procedure TEdit97.DrawNCArea (const DrawToDC: Boolean; const ADC: HDC;
  const Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  if not DrawToDC then
    DC := GetWindowDC(Handle)
  else
    DC := ADC;
  try
    { Use update region }
    if not DrawToDC then
      SelectNCUpdateRgn (Handle, DC, Clip);

    { This works around WM_NCPAINT problem described at top of source code }
    {no!  R := Rect(0, 0, Width, Height);}
    GetWindowRect (Handle, R);  OffsetRect (R, -R.Left, -R.Top);
    BtnFaceBrush := GetSysColorBrush(COLOR_BTNFACE);
    WindowBrush := GetSysColorBrush(COLOR_WINDOW);
    if ((csDesigning in ComponentState) and Enabled) or
       (not(csDesigning in ComponentState) and
        (Focused or (MouseInControl and not ControlIs97Control(Screen.ActiveControl)))) then begin
      DrawEdge (DC, R, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      with R do begin
        FillRect (DC, Rect(Left, Top, Left+1, Bottom-1), BtnFaceBrush);
        FillRect (DC, Rect(Left, Top, Right-1, Top+1), BtnFaceBrush);
      end;
      DrawEdge (DC, R, BDR_SUNKENINNER, BF_BOTTOMRIGHT);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, WindowBrush);
    end
    else begin
      FrameRect (DC, R, BtnFaceBrush);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, BtnFaceBrush);
      InflateRect (R, -1, -1);
      FrameRect (DC, R, WindowBrush);
    end;
  finally
    if not DrawToDC then
      ReleaseDC (Handle, DC);
  end;
end;

procedure EditNCPaintProc (Wnd: HWND; DC: HDC; AppData: Longint);
begin
  TEdit97(AppData).DrawNCArea (True, DC, 0);
end;

procedure TEdit97.WMPrint (var Message: TMessage);
begin
  HandleWMPrint (Handle, Message, EditNCPaintProc, Longint(Self));
end;

procedure TEdit97.WMPrintClient (var Message: TMessage);
begin
  HandleWMPrintClient (Self, Message);
end;

function TEdit97.GetVersion: TToolbar97Version;
begin
  Result := Toolbar97VersionPropText;
end;

procedure TEdit97.SetVersion (const Value: TToolbar97Version);
begin
  { write method required for the property to show up in Object Inspector }
end;


{$IFNDEF TB97D4}
initialization
finalization
  DropdownList.Free;
{$ENDIF}
end.
