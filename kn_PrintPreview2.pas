unit kn_PrintPreview2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Variants,
  Dialogs, stdctrls, ExtCtrls, ToolWin, ComCtrls,
  printers, Richedit, RxRichEd;

type
  TPreviewForm = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    Panel1: TPanel;
    PaintBox1: TPaintBox;
    PrintDialog1: TPrintDialog;
    ScrollBar1: TScrollBar;
    procedure PaintBox1Paint(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { Private declarations }
    FMetafile: TMetafile;
    FRichedit: TRichedit;
    FPages   : Array of richedit.TCharRange;
    FPagerect: TRect;
    FPrintRect: TRect;

    Procedure PreparePreview( aRichedit: TRichedit );
    Procedure Paginate;
    Procedure DrawPage( pagenum: Integer );
    Function RenderPage( pagenum: Integer; render: BOOL = true ): Integer;
    procedure VerifyPagenum(pagenum: Integer);
  public
    { Public declarations }
    Destructor Destroy; override;
    class procedure Preview( arichedit: TRxRichedit );
  end;


implementation

{$R *.DFM}

Type
  TPageInfo = record
    width, height: Integer;   { physical width and height, in dots }
    offsetX, offsetY: Integer;{ nonprintable margin, in dots }
    resX, resY: Integer;      { logical resolution, dots per inch }
  End;

Procedure GetPageinfo(  Var info: TPageInfo; index: Integer = -1 );
  Begin
    If index > -1 Then
      Printer.PrinterIndex := index;
    With Printer Do Begin
      info.resX    := GetDeviceCaps( handle, LOGPIXELSX );
      info.resY    := GetDeviceCaps( handle, LOGPIXELSY );
      info.offsetX := GetDeviceCaps( handle, PHYSICALOFFSETX );
      info.offsetY := GetDeviceCaps( handle, PHYSICALOFFSETY );
      info.width   := GetDeviceCaps( handle, PHYSICALWIDTH );
      info.height  := GetDeviceCaps( handle, PHYSICALHEIGHT );
    End; { With }
  End;

Procedure DotsToTwips( Var value: Integer; dpi: Integer );
  Begin
    value := MulDiv( value, 1440, dpi );
  End;

{-- DrawScaled --------------------------------------------------------}
{: Draw an image scaled isotropically so it fits into the outrect
   of the canvas.
@Param Canvas is the output device
@Param outrect is the available space on the device
@Param image is the graphic element to render
@Param iwidth is the elements width
@Param iheight is the elements height
@Precondition  Canvas <> nil, passed width and height > 0
@Desc If the graphic is drawn scaled to fit the output area, while
  preserving the aspect ratio. If a Nil graphic is passed we exit
  without doing anything.
}{ Created 21.6.2001 by P. Below
-----------------------------------------------------------------------}
Procedure DrawScaled( Canvas: TCanvas; const outrect: TRect;
    image: TGraphic; iwidth, iheight: Integer );
  Var
    imageaspect, outputaspect: Double;
    r: TRect;
    outwidth, outheight: Integer;
  Begin { DrawScaled }
    Assert( Assigned( Canvas ), 'DrawScaled: no canvas');
    Assert( iwidth > 0, 'DrawScaled: width cannot be negative' );
    Assert( iheight > 0, 'DrawScaled: height cannot be negative' );

    If not Assigned( image ) Then Exit;

    outwidth := outrect.right - outrect.left;
    outheight:= outrect.bottom - outrect.top;
    imageaspect:= iwidth / iheight;
    outputaspect := outwidth / outheight;
    r:= Rect( 0,0,0,0);
    If imageaspect > outputaspect Then Begin
      r.right := outwidth;
      r.Bottom := round( outwidth / imageaspect );
      OffsetRect( r,
                  outrect.left,
                  outrect.top + ( outheight - r.bottom  ) div 2
                );
    End { If }
    Else Begin
      r.bottom := outheight;
      r.right  := Round( outheight * imageaspect );
      OffsetRect( r,
                  outrect.left +  ( outwidth - r.right ) div 2,
                  outrect.top
                );
    End; { Else }
    Canvas.StretchDraw( r, image );
  End; { DrawScaled }

destructor TPreviewForm.Destroy;
begin
  PaintBox1.OnPaint := nil;
  Fmetafile.Free;
  inherited;
end;

Procedure TPreviewForm.VerifyPagenum( pagenum: Integer );
Begin
  If pagenum >= Length( FPages ) Then
    SetLength( FPages, pagenum+32 );
End;

procedure TPreviewForm.DrawPage(pagenum: Integer);
  Function VerifyPagination: Boolean;
    Begin
      Result := FPages[0].cpMax > Fpages[0].cpMin;
    End;
begin
  VerifyPagenum( pagenum );
  If not VerifyPagination Then
    Paginate;
  RenderPage( pagenum );
  FRichedit.Perform( EM_FORMATRANGE, 0, 0 );
  PaintBox1.Invalidate;
end;

procedure TPreviewForm.Paginate;
var
  info: TPageInfo;
  pagenum, lastchar, len: Integer;
  Procedure RectToTwips( Var rect: TRect );
  Begin
    DotsToTwips( rect.left, info.resX );
    DotsToTwips( rect.right, info.resX );
    DotsToTwips( rect.top, info.resY );
    DotsToTwips( rect.bottom, info.resY );
  End;
begin
  GetPageinfo( info );
  FPagerect := Rect( 0, 0, info.width, info.height );
  RectToTwips( FPagerect );
  FPrintrect := FRichedit.PageRect;
  If IsRectEmpty( FprintRect ) Then Begin
    // use a default output rect with 1 inch magin top and bottom
    // and 0.75 inch right and left
    Fprintrect := FPagerect;
    InflateRect( FPrintrect, -1080, -1440 );
  End
  Else
    RectToTwips( FPrintrect );

  pagenum  := 0;
  lastchar := 0;
  len      := FRichedit.GetTextLen;
  Screen.Cursor := crHourglass;
  Scrollbar.Min := 0;
  Scrollbar.Max := 0;
  Try
    While lastchar < len  Do Begin
      VerifyPagenum( pagenum );
      FPages[pagenum].cpMin := lastchar;
      FPages[pagenum].cpMax := len;
      lastchar := RenderPage( pagenum, false );
      If lastchar <= FPages[pagenum].cpMin Then
         lastchar := len
      Else Begin
        FPages[pagenum].cpMax := lastchar-1;
        Scrollbar.Max := pagenum;
        Inc( pagenum );
      End;
    End;
  Finally
    Screen.Cursor := crDefault;
  End; { Finally }

end;

procedure TPreviewForm.PaintBox1Paint(Sender: TObject);
var
  cv: TCanvas;
begin
  cv:= (sender as TPaintbox).canvas;
  cv.FillRect( TPaintbox(sender).clientrect);
  DrawScaled( cv, Tpaintbox(sender).clientrect,
              Fmetafile, Fmetafile.mmWidth, Fmetafile.mmHeight );
end;

procedure TPreviewForm.PreparePreview(aRichedit: TRichedit);
begin
  FMetafile:= TMetafile.Create;
  FRichedit:= aRichedit;
  SetLength( FPages, 256 );
  DrawPage( 0 );
end;

class procedure TPreviewForm.Preview(arichedit: TRichedit);
begin
  assert( assigned( arichedit ));
  With TPreviewform.Create( application ) Do
  Try
    PreparePreview( arichedit );
    Showmodal;
  Finally
    Free;
  End; { Finally }

end;

Function TPreviewForm.RenderPage(pagenum: Integer; render: BOOL): Integer;
var
  mfc: TMetafilecanvas;
  fmt: TFormatRange;
begin
  FMetafile.Clear;
  mfc:= TMetafilecanvas.Create( Fmetafile, Printer.handle );
  Try
    mfc.Brush.Color := clWhite;
    mfc.Brush.Style := bsSolid;
    mfc.FillRect( Rect(0,0,Printer.PageWidth, Printer.PageHeight ));
    fmt.hdc := mfc.Handle;
    fmt.hdcTarget := printer.Handle;
    fmt.rc := FPrintrect;
    fmt.rcPage := FPageRect;
    fmt.chrg := Fpages[ pagenum ];
    Result := FRichedit.Perform( EM_FORMATRANGE, WPARAM(render),
                                 LPARAM(@fmt));
  Finally
    mfc.free;
  End; { Finally }
end;

procedure TPreviewForm.ScrollBarChange(Sender: TObject);
begin
  RenderPage( Scrollbar.Position );
  Paintbox1.Invalidate;
end;

procedure TPreviewForm.ToolButton1Click(Sender: TObject);
var
  pagenum: Integer;
  info: TPageInfo;
begin
  With PrintDialog Do Begin
    Minpage := Scrollbar.Min + 1;
    Maxpage := Scrollbar.Max + 1;
    FromPage := MinPage;
    ToPage := MaxPage;
    If not Execute Then Exit;
  End;
  GetPageinfo( info );
  Printer.BeginDoc;
  Try
    Screen.Cursor := crHourglass;
    For pagenum := PrintDialog.Frompage-1 To PrintDialog.ToPage-1
    Do Begin
      RenderPage( pagenum );
      Printer.Canvas.Draw( -info.offsetX,  -info.offsetY, FMetafile );
      If pagenum < (PrintDialog.ToPage-1) Then
        Printer.NewPage;
    End;
  Finally
    Screen.Cursor := crDefault;
    Printer.EndDoc;
  End; { Finally }
end;

end.
