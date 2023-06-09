{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxPicClip;

interface

{$I RX.INC}

uses 
  {$IFNDEF VER80} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Controls,
  {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  Graphics {$IFDEF RX_D6}, Types{$ENDIF};

type

{ TPicClip }
  TCellRange = 1..MaxInt;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPicClip = class(TComponent)
  private
    FPicture: TPicture;
    FRows: TCellRange;
    FCols: TCellRange;
    FBitmap: TBitmap;
    FMasked: Boolean;
    FMaskColor: TColor;
    FOnChange: TNotifyEvent;
    procedure CheckIndex(Index: Integer);
    function GetCell(Col, Row: Cardinal): TBitmap;
    function GetGraphicCell(Index: Integer): TBitmap;
    function GetDefaultMaskColor: TColor;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function IsMaskStored: Boolean;
    procedure PictureChanged(Sender: TObject);
    procedure SetHeight(Value: Integer);
    procedure SetPicture(Value: TPicture);
    procedure SetWidth(Value: Integer);
    procedure SetMaskColor(Value: TColor);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetIndex(Col, Row: Cardinal): Integer;
    procedure Draw(Canvas: TCanvas; X, Y, Index: Integer);
    procedure DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
    procedure LoadBitmapRes(Instance: THandle; ResID: PChar);
    property Cells[Col, Row: Cardinal]: TBitmap read GetCell;
    property GraphicCell[Index: Integer]: TBitmap read GetGraphicCell;
    property IsEmpty: Boolean read GetIsEmpty;
    property Count: Integer read GetCount;
  published
    property Cols: TCellRange read FCols write FCols default 1;
    property Height: Integer read GetHeight write SetHeight stored False;
    property Masked: Boolean read FMasked write FMasked default True;
    property Rows: TCellRange read FRows write FRows default 1;
    property Picture: TPicture read FPicture write SetPicture;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored IsMaskStored;
    property Width: Integer read GetWidth write SetWidth stored False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$B-}

uses SysUtils, RxVCLUtils, Consts, RXConst, RxResConst
     {$IFDEF RX_D6} , RTLConsts {$ENDIF}; // Polaris

{ TPicClip }

constructor TPicClip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FBitmap := TBitmap.Create;
  FRows := 1;
  FCols := 1;
  FMaskColor := GetDefaultMaskColor;
  FMasked := True;
end;

destructor TPicClip.Destroy;
begin
  FOnChange := nil;
  FPicture.OnChange := nil;
  FBitmap.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TPicClip.Assign(Source: TPersistent);
begin
  if Source is TPicClip then
  begin
    with TPicClip(Source) do
    begin
      Self.FRows := Rows;
      Self.FCols := Cols;
      Self.FMasked := Masked;
      Self.FMaskColor := MaskColor;
      Self.FPicture.Assign(FPicture);
    end;
  end
  else if (Source is TPicture) or (Source is TGraphic) then
    FPicture.Assign(Source)
  else inherited Assign(Source);
end;

{$IFNDEF VER80}
type
  THack = class(TImageList);
{$ENDIF}

procedure TPicClip.AssignTo(Dest: TPersistent);
{$IFNDEF VER80}
var
  I: Integer;
  SaveChange: TNotifyEvent;
{$ENDIF}
begin
  if (Dest is TPicture) then Dest.Assign(FPicture)
  else if (Dest is TGraphic) and (FPicture.Graphic <> nil) and
    (FPicture.Graphic is TGraphic(Dest).ClassType) then
    Dest.Assign(FPicture.Graphic)
{$IFNDEF VER80}
  else if (Dest is TImageList) and not IsEmpty then
  begin
    with TImageList(Dest) do begin
      SaveChange := OnChange;
      try
        OnChange := nil;
        Clear;
        Width := Self.Width;
        Height := Self.Height;
        for I := 0 to Self.Count - 1 do
        begin
          if Self.Masked and (MaskColor <> clNone) then
            TImageList(Dest).AddMasked(GraphicCell[I], MaskColor)
          else TImageList(Dest).Add(GraphicCell[I], nil);
        end;
        Masked := Self.Masked;
      finally
        OnChange := SaveChange;
      end;
      THack(Dest).Change;
    end;
  end
{$ENDIF}
  else inherited AssignTo(Dest);
end;

procedure TPicClip.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TPicClip.GetIsEmpty: Boolean;
begin
  Result := (Picture.Graphic = nil) or Picture.Graphic.Empty;
end;

function TPicClip.GetCount: Integer;
begin
  if IsEmpty then Result := 0
  else Result := Cols * Rows;
end;

procedure TPicClip.Draw(Canvas: TCanvas; X, Y, Index: Integer);
var
  Image: TGraphic;
begin
  if Index < 0 then Image := Picture.Graphic
  else Image := GraphicCell[Index];
  if (Image <> nil) and not Image.Empty then
  begin
    if FMasked and (FMaskColor <> clNone) and (Picture.Graphic is TBitmap) then
      DrawBitmapTransparent(Canvas, X, Y, TBitmap(Image), FMaskColor)
    else
      Canvas.Draw(X, Y, Image);
  end;
end;

procedure TPicClip.DrawCenter(Canvas: TCanvas; Rect: TRect; Index: Integer);
var
  X, Y: Integer;
begin
  X := (Rect.Left + Rect.Right - Width) div 2;
  Y := (Rect.Bottom + Rect.Top - Height) div 2;
  Draw(Canvas, X, Y, Index);
end;

procedure TPicClip.LoadBitmapRes(Instance: THandle; ResID: PChar);
var
  Bmp: TBitmap;
begin
  Bmp := MakeModuleBitmap(Instance, ResID);
  try
    Picture.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TPicClip.CheckIndex(Index: Integer);
begin
  if (Index >= Cols * Rows) or (Index < 0) then
{$IFDEF RX_D3}
    raise EListError.CreateFmt(SListIndexError, [Index]);
{$ELSE}
    raise EListError.CreateFmt('%s (%d)', [{LoadStr(}SListIndexError{)}, Index]);
{$ENDIF}
end;

function TPicClip.GetIndex(Col, Row: Cardinal): Integer;
begin
  Result := Col + (Row * Cols);
  if (Result >= Cols * Rows) or IsEmpty then Result := -1;
end;

function TPicClip.GetCell(Col, Row: Cardinal): TBitmap;
begin
  Result := GetGraphicCell(GetIndex(Col, Row));
end;

function TPicClip.GetGraphicCell(Index: Integer): TBitmap;
begin
  CheckIndex(Index);
  AssignBitmapCell(Picture.Graphic, FBitmap, Cols, Rows, Index);
{$IFDEF RX_D3}
  if Picture.Graphic is TBitmap then
    if FBitmap.PixelFormat <> pfDevice then
      FBitmap.PixelFormat := TBitmap(Picture.Graphic).PixelFormat;
  FBitmap.TransparentColor := FMaskColor or PaletteMask;
  FBitmap.Transparent := (FMaskColor <> clNone) and Masked;
{$ELSE}
  if Masked and (FMaskColor <> clNone) then
    with FBitmap do
      if not Empty then Canvas.Pixels[0, Height - 1] := FMaskColor;
{$ENDIF}
  Result := FBitmap;
end;

function TPicClip.GetDefaultMaskColor: TColor;
begin
  Result := clOlive;
  if (Picture.Graphic <> nil) and (Picture.Graphic is TBitmap) then
    Result := TBitmap(Picture.Graphic).TransparentColor and
      not PaletteMask;
end;

function TPicClip.GetHeight: Integer;
begin
  Result := Picture.Height div FRows;
end;

function TPicClip.GetWidth: Integer;
begin
  Result := Picture.Width div FCols;
end;

function TPicClip.IsMaskStored: Boolean;
begin
  Result := MaskColor <> GetDefaultMaskColor;
end;

procedure TPicClip.SetMaskColor(Value: TColor);
begin
  if Value <> FMaskColor then
  begin
    FMaskColor := Value;
    Changed;
  end;
end;

procedure TPicClip.PictureChanged(Sender: TObject);
begin
  FMaskColor := GetDefaultMaskColor;
  if not (csReading in ComponentState) then Changed;
end;

procedure TPicClip.SetHeight(Value: Integer);
begin
  if (Value > 0) and (Picture.Height div Value > 0) then
    Rows := Picture.Height div Value;
end;

procedure TPicClip.SetWidth(Value: Integer);
begin
  if (Value > 0) and (Picture.Width div Value > 0) then
    Cols := Picture.Width div Value;
end;

procedure TPicClip.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

end.