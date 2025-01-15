unit RichPreview;

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.Win.Registry,
   System.SysUtils,
   System.Classes,
   System.ZLib,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   Vcl.Buttons,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   Vcl.Printers,
   Vcl.Mask,
   Richprint,
   arrowbtn;

const
  BufferSize = 4096;
type
  TCompress = class
   InFile: TStream;
   OutFile: TStream;
   ZStream: TCustomZLibStream;
   Buffer: array[0..BufferSize-1] of Byte;
  end;

type
  TRichPreviewForm = class(TForm)
    Panel1: TPanel;
    ArrowButton2: TArrowButton;
    Label1: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Bevel2: TBevel;
    SpeedButton1: TSpeedButton;
    BitBtn1: TBitBtn;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Label4: TLabel;
    ArrowButton1: TArrowButton;
    PrintDialog1: TPrintDialog;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    MaskEdit1: TMaskEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Label5: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ArrowButton2Click(Sender: TObject);
    procedure ArrowButton1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure MaskEdit1Change(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Label7DblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
   ViewBitmap,ClearBitmap: TBitmap;
   LoadingMetafiles,LoadAgain,CompressPreview,FirstLoad,Compact,RichPreview2Exist: Boolean;
   Zoomfactor,ZoomStep: Double;
   Persistent: TMetafile;
   FileNameMetafile: string;
   Procedure ClearPreview1;
   Procedure ClearPreview2;
   procedure DeleteMetafile;
   procedure SetupPgUp(PgUpOn: Boolean);
   procedure SetupPgDown(PgDown: Boolean);
   procedure LabelsVisiblePreview2(Visible: Boolean);
  public
    { Public declarations }
   PaperWidthUnits,PaperHeightUnits: TMeasurement;
   StartPage, EndPage,PreviewPaperHeight,PreviewPaperWidth: Integer;
   ImageIndex,PreviewImageHeight,PreviewImageWidth,MetafileCount: Integer;
   MeasureUnits: TMeasureUnit;
   procedure FillImagelist;
   procedure SetupRulers(RulerForm: TForm);
  end;

var
  RichPreviewForm: TRichPreviewForm;

implementation

{$R *.DFM}

uses
   RichPreview2;

procedure TRichPreviewForm.FormClose(Sender: TObject; var Action: TCloseAction);
var I: Integer;
    FileTyp: file of TMetafile;
    FileNameMetafileExt: string;
    Stoppen: Boolean;
begin
 Screen.Cursor:=crHourGlass;
 ViewBitmap.free;
 Persistent.free;
{ RichPreviewForm.Free;}
 I:=0;
 Stoppen:=false;
 Repeat
  str(I,FileNameMetafileExt);
  FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
  if CompressPreview then AssignFile(FileTyp,FileNameMetafile+'.CPR') else AssignFile(FileTyp,FileNameMetafile);
  {$I-}
  RESET(FileTyp);
 {$I+}
  IF IOResult=0 then
  begin
   CloseFile(FileTyp);
   Erase(FileTyp);
  end else Stoppen:=true;
  I:=I+1;
 until Stoppen or (I>10000);
 Action:=caFree;
 if RichPreview2Exist then Preview2.Close;
 Screen.Cursor:=crDefault;
end;


procedure TRichPreviewForm.FillImagelist;
var FileNameMetafileExt: string;
    FileTyp: file of TMetafile;
    FileCompress: TCompress;

begin
 MetaFileCanvasRichPrint.free;
 str(ImageIndex,FileNameMetafileExt);
 FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
 AssignFile(FileTyp,FileNameMetafile);
 {$I-}
 REWRITE(FileTyp);
 {$I+}
 IF IOResult=0 then
 begin
  CloseFile(FileTyp);
  MetafileRichPrint.SaveToFile(FileNameMetafile);
 end
else MessageDlg('Can not save the page information (MetaFiles)',mtError,[mbOK],0);

 if CompressPreview then
 begin
 {compression}
  FileCompress := TCompress.Create;
  FileCompress.InFile := TFileStream.Create(FileNameMetafile, fmOpenRead);
  FileCompress.OutFile := TFileStream.Create(FileNameMetafile+'.CPR', fmCreate);
  FileCompress.ZStream := TCompressionStream.Create(clFastest, FileCompress.OutFile);
  FileCompress.ZStream.CopyFrom(FileCompress.InFile, 0);
  FileCompress.ZStream.Free;
  FileCompress.OutFile.Free;
  FileCompress.InFile.Free;
  FileCompress.Free;
  DeleteMetafile;
 end;
 MetafileRichPrint.free;
 MetafileCount:=ImageIndex;
 ImageIndex:=ImageIndex+1;
end;

procedure TRichPreviewForm.FormCreate(Sender: TObject);
var ZoomText: String;
    TRegIniFile1: TRegIniFile;
    Default: Boolean;
begin
 Compact:=false;
 FirstLoad:=true;
 RichPreview2Exist:=false;
 Label10.Visible:=false;
 TRegIniFile1:=TRegIniFile.Create('RichPrint.Settings');
 Default:=true;
 with TRegIniFile1 do
 begin
  if OpenKey('Compress',false) then CompressPreview:=ReadBool('','CompressPreview',Default) else CompressPreview:=true;
 end;
 TRegIniFile1.Free;
 {formatting various element on the form}
 PreviewImageHeight:=round(0.95*Screen.Height);
 Height:=PreviewImageHeight;
 ClientHeight:=Height-27;
 if RatioPage <> 0 then PreviewImageWidth:=round((0.98*ClientHeight-87)/RatioPage) else
                        PreviewImageWidth:=round(0.707*(0.98*ClientHeight-87));   {A4 format}
 if round(1.0204*PreviewImageWidth) > Screen.Width then
 begin
  PreviewImageHeight:=round(Screen.Width/1.0204*PreviewImageWidth*PreviewImageHeight);
  PreviewImageWidth:=round(0.98*Screen.Width);
 end;
 Height:=PreviewImageHeight;
 ClientHeight:=Height-27;
 ImageIndex:=0;
 Width:=round(1.0204*PreviewImageWidth);
 ClientWidth:=Width-8;
 if ClientWidth < 417+round(0.02*ClientWidth) Then {in order to function with a screen resolution of 800*600 or even lower}
 begin                                             {preferable the resolution should be 1024*768}
  if ClientWidth < 310+round(0.02*ClientWidth) then
  begin
   Width:=round(1.0204*(310+0.02*ClientWidth));
   ClientWidth:=Width-8;
  end;
  Label8.Visible:=false;
  Label7.Visible:=false;
  Label6.Visible:=false;
  MaskEdit1.Visible:=false;
 end
else
 begin
  Label8.Visible:=true;
  Label7.Visible:=true;
  Label6.Visible:=true;
  MaskEdit1.Visible:=true;
 end;
 Image1.Top:=round(0.01*ClientHeight)+16;
 Image1.Left:=round(0.01*ClientWidth)+18;
 Image1.Height:=round(0.98*ClientHeight)-87;
 Image1.Width:=round(0.98*ClientWidth)-18;
 ScrollBox1.Top:=Image1.Top;
 ScrollBox1.Left:=Image1.Left;
 ScrollBox1.Height:=Image1.Height;
 ScrollBox1.Width:=Image1.Width;
 Panel1.Top:=Image1.Top+Image1.Height+3;
 Panel1.Left:=0;
 Panel1.Width:=Image1.Width+22;
 ViewBitmap:=TBitmap.Create;
 ViewBitmap.Height:=Image1.Height;
 ViewBitmap.Width:=Image1.Width;
 Image1.Picture.Graphic:=ViewBitmap;
 LoadingMetafiles:=true;
 Zoomfactor:=1;
 ZoomStep:=0.25;
 str(ZoomStep:5:3,ZoomText);
 MaskEdit1.Text:=ZoomText;
 str(ZoomFactor:5:3,ZoomText);
 Label7.Caption:=ZoomText;
 LoadAgain:=true;
 Persistent.free;
 Persistent:=TMetafile.Create;
 Edit1.Top:=Panel1.Top+4;
 Edit1.Left:=Panel1.Left+79;
end;

procedure TRichPreviewForm.SetupPgUp(PgUpOn: Boolean);
begin
 if PgUpOn then
 begin
  ArrowButton2.Enabled:=true;
  Label1.Enabled:=true;
  Label3.Enabled:=true;
 end
else
 begin
  ArrowButton2.Enabled:=false;
  Label1.Enabled:=false;
  Label3.Enabled:=false;
 end;
end;

procedure TRichPreviewForm.SetupPgDown(PgDown: Boolean);
begin
 if PgDown then
 begin
  ArrowButton1.Enabled:=true;
  Label2.Enabled:=true;
  Label9.Enabled:=true;
 end
else
 begin
  ArrowButton1.Enabled:=false;
  Label2.Enabled:=false;
  Label9.Enabled:=false;
 end;
end;

procedure TRichPreviewForm.LabelsVisiblePreview2(Visible: Boolean);
begin
 if Visible then
 begin
  Preview2.Label1.Visible:=true;
  Preview2.Label2.Visible:=true;
 end
else
 begin
  Preview2.Label1.Visible:=false;
  Preview2.Label2.Visible:=false;
 end;
end;

procedure TRichPreviewForm.SetupRulers(RulerForm: TForm);
Var Counter,NumberTicksX,NumberTicksY,DeltaXScreen,DeltaYScreen,DeltaXReal,DeltaYReal,Delta: Integer;
    Value: string;
    UsedPart: Double;
    RulerXLeft,RulerXWidth,RulerYTop,RulerYHeight: Integer;
begin
 if (PaperWidthUnits > 0) and (RichPreviewForm.Caption='Print Preview') then
 begin
  if RulerForm=RichPreviewForm then
  begin
   if MeasureUnits in [muMillimeters] then Label5.Caption:='mm' else Label5.Caption:='inch';
  end
 else
  begin
   if RichPreview2Exist then
   begin
    if MeasureUnits in [muMillimeters] then Preview2.Label5.Caption:='mm' else Preview2.Label5.Caption:='inch';
   end;
  end;
  if MeasureUnits in [muMillimeters] then Delta:=10 else Delta:=1;
  with RulerForm.Canvas do {Y-ruler}
  begin
   RulerYTop:=Scrollbox1.Top;
   RulerYHeight:=Scrollbox1.Top+Image1.Height+7;
   Brush.Color:=clBtnFace;
   Brush.Style:=bsSolid;
   Pen.Color:=clDkGray;
   Rectangle(0,RulerYTop,22,RulerYHeight);
   MoveTo(11,RulerYTop);
   LineTo(11,RulerYTop+Image1.Height);
   NumberTicksY:=trunc(RichPreviewForm.PaperHeightUnits/Delta);
   if NumberTicksY=0 then NumberTicksY:=1;
   DeltaYReal:=round(RichPreviewForm.PaperHeightUnits/NumberTicksY);
   UsedPart:=NumberTicksY*DeltaYReal/RichPreviewForm.PaperHeightUnits;
   DeltaYScreen:=round(UsedPart*Image1.Height/NumberTicksY);
   Font.Size:=7;
   Font.Name:='Arial';
   Font.Color:=clBlack;
   for Counter:=0 to NumberTicksY+1 do
   begin
    MoveTo(5,RulerYTop+Counter*DeltaYScreen);
    LineTo(17,RulerYTop+Counter*DeltaYScreen);
    if (Counter > 0) and ((not odd(Counter)) or (MeasureUnits in [muInches]))then
    begin
     str(Counter*DeltaYReal,Value);
     TextOut(5,RulerYTop+Counter*DeltaYScreen-5,Value);
    end;
   end;
   RulerXLeft:=Scrollbox1.Left; {X-ruler}
   RulerXWidth:=Scrollbox1.Left+Image1.Width+7;
   Rectangle(RulerXLeft,0,RulerXWidth,22);
   MoveTo(RulerXLeft,11);
   LineTo(Image1.Width,11);
   NumberTicksX:=trunc(RichPreviewForm.PaperWidthUnits/Delta);
   if NumberTicksX=0 then NumberTicksX:=1;
   DeltaXReal:=round(RichPreviewForm.PaperWidthUnits/NumberTicksX);
   UsedPart:=NumberTicksX*DeltaXReal/RichPreviewForm.PaperWidthUnits;
   DeltaXScreen:=round(UsedPart*Image1.Width/NumberTicksX);
   Font.Size:=7;
   Font.Name:='Arial';
   Font.Color:=clBlack;
   for Counter:=0 to NumberTicksX+1 do
   begin
    MoveTo(RulerXLeft+Counter*DeltaXScreen,5);
    LineTo(RulerXLeft+Counter*DeltaXScreen,17);
    if (Counter > 0) and ((not odd(Counter)) or (MeasureUnits in [muInches]))then
    begin
     str(Counter*DeltaXReal,Value);
     TextOut(RulerXLeft+Counter*DeltaXScreen-5,0,Value);
    end;
   end;
  end;
 end;
end;


procedure TRichPreviewForm.DeleteMetafile;
var  FileTyp: file of TMetafile;
begin
 if CompressPreview then
 begin
  {erasing metafile}
  AssignFile(FileTyp,FileNameMetafile);
  {$I-}
  RESET(FileTyp);
 {$I+}
  IF IOResult=0 then
  begin
   CloseFile(FileTyp);
   Erase(FileTyp);
  end;
 end;
end;

Procedure TRichPreviewForm.ClearPreview1;
begin
 ClearBitmap:=TBitmap.Create;
 ClearBitmap.Width:=Image1.Width;
 ClearBitmap.Height:=Image1.Height;
 Image1.Picture.Bitmap.Canvas.Draw(0,0,ClearBitmap);
 ClearBitmap.free;
end;

Procedure TRichPreviewForm.ClearPreview2;
begin
 if RichPreview2Exist then
 begin
  ClearBitmap:=TBitmap.Create;
  ClearBitmap.Width:=Preview2.Image1.Width;
  ClearBitmap.Height:=Preview2.Image1.Height;
  Preview2.Image1.Picture.Bitmap.Canvas.Draw(0,0,ClearBitmap);
  ClearBitmap.free;
 end;
end;

procedure TRichPreviewForm.FormPaint(Sender: TObject);
var PageText,PageText2,TotalNumber: string;
    ShowMetafile: TMetafile;
    FileNameMetafileExt: string;
    DrawRect: TRect;
    FileTyp: file of TMetafile;
    FileCompress: TCompress;
    Count,FilePresent,CounterCompact: Integer;
    Stop: Boolean;

procedure Scrolling(Screen: Word);
begin
 if Zoomfactor > 1.00001 then
 begin
  case Screen of
  1: begin
      with Scrollbox1 do
      begin
       HorzScrollBar.Range:=Round(Zoomfactor*ScrollBox1.Width);
       VertScrollBar.Range:=Round(Zoomfactor*ScrollBox1.Height);
       Image1.Width:=HorzScrollBar.Range;
       Image1.Height:=VertScrollBar.Range;
      end;
     end;
  2: begin
      if RichPreview2Exist then
      begin
       Preview2.Scrollbox1.HorzScrollBar.Range:=Round(Zoomfactor*ScrollBox1.Width);
       Preview2.Scrollbox1.VertScrollBar.Range:=Round(Zoomfactor*ScrollBox1.Height);
       Preview2.Image1.Width:=HorzScrollBar.Range;
       Preview2.Image1.Height:=VertScrollBar.Range;
      end;
     end;
  end;
 end
else
 begin
  case Screen of
  1: begin
      with Scrollbox1 do
      begin
       HorzScrollBar.Range:=0;
       VertScrollBar.Range:=0;
       Image1.Width:=ScrollBox1.Width;
       Image1.Height:=ScrollBox1.Height;
      end;
     end;
  2: begin
      if RichPreview2Exist then
      begin
       Preview2.Scrollbox1.HorzScrollBar.Range:=0;
       Preview2.Scrollbox1.VertScrollBar.Range:=0;
       Preview2.Image1.Width:=ScrollBox1.Width;
       Preview2.Image1.Height:=ScrollBox1.Height;
      end;
     end;
  end;
 end;
 case screen of
 1: begin
     Image1.Picture.Bitmap.Width:=Image1.Width;
     Image1.Picture.Bitmap.Height:=Image1.Height;
    end;
 2: begin
     if RichPreview2Exist then
     begin
      Preview2.Image1.Picture.Bitmap.Width:=Image1.Width;
      Preview2.Image1.Picture.Bitmap.Height:=Image1.Height;
     end;
    end;
 end;
end;

begin {body FormPaint}
 if Compact then
 begin
  Stop:=false;
  CounterCompact:=0;
  repeat
   if RichPreview2Exist and (CounterCompact=7) then Stop:=true;
   if (not RichPreview2Exist) and (CounterCompact=3) then Stop:=true;
   if LoadingMetafiles then str(ImageIndex-1+CounterCompact,FileNameMetafileExt) else str(ImageIndex+CounterCompact,FileNameMetafileExt);
   FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
   if CompressPreview then AssignFile(FileTyp,FileNameMetafile+'.CPR') else AssignFile(FileTyp,FileNameMetafile);
   {$I-}
   RESET(FileTyp);
   {$I+}
   FilePresent:=IOResult;
   if FilePresent=0 then CloseFile(FileTyp);
   IF (FilePresent=0) and CompressPreview then
   begin
    FileCompress:=TCompress.Create;
    FileCompress.InFile := TFileStream.Create(FileNameMetafile+'.CPR', fmOpenRead);
    FileCompress.OutFile := TFileStream.Create(FileNameMetafile, fmCreate);
    FileCompress.ZStream := TDecompressionStream.Create(FileCompress.InFile);
    while True do
    begin
     Count := FileCompress.ZStream.Read(FileCompress.Buffer, BufferSize);
     if Count <> 0 then FileCompress.OutFile.WriteBuffer(FileCompress.Buffer, Count) else Break;
    end;
    FileCompress.ZStream.Free;
    FileCompress.OutFile.Free;
    FileCompress.InFile.Free;
    FileCompress.Free;
   end;
   IF FilePresent=0 then
   begin
    ShowMetafile:=TMetafile.Create;
    if LoadAgain then
    begin
     {CloseFile(FileTyp);}
     ShowMetafile.LoadFromfile(FileNameMetafile);
     Persistent.Assign(ShowMetafile);
    end else ShowMetafile.Assign(Persistent);
    if CounterCompact <= 3 then
    begin
    {first window}
     Scrolling(1);
     case CounterCompact of
      0: begin
          DrawRect.Top:=0;
          DrawRect.Left:=0;
          DrawRect.Right:=round(Image1.Width/2);
          DrawRect.Bottom:=round(Image1.Height/2);
         end;
      1: begin
          DrawRect.Top:=0;
          DrawRect.Left:=round(Image1.Width/2);
          DrawRect.Right:=Image1.Width;
          DrawRect.Bottom:=round(Image1.Height/2);
         end;
      2: begin
          DrawRect.Top:=round(Image1.Height/2);
          DrawRect.Left:=0;
          DrawRect.Right:=round(Image1.Width/2);
          DrawRect.Bottom:=Image1.Height;
         end;
      3: begin
          DrawRect.Top:=round(Image1.Height/2);
          DrawRect.Left:=round(Image1.Width/2);
          DrawRect.Right:=Image1.Width;
          DrawRect.Bottom:=Image1.Height;
         end;
     end;
     if CounterCompact=0 then ClearPreview1;
     Image1.Picture.Bitmap.Canvas.StretchDraw(DrawRect,ShowMetafile);
     if LoadingMetafiles then STR(ImageIndex{+CounterCompact}:4,PageText) else STR(ImageIndex+1{+CounterCompact}:4,PageText);
     Edit1.Text:=PageText;
     str(MetafileCount+1,TotalNumber);
     Label10.Caption:='('+TotalNumber+')';
     Image1.Refresh;
     ShowMetafile.free;
     DeleteMetafile;
    end
   else
    begin
     {second window}
     Scrolling(2);
     case CounterCompact of
      4: begin
          DrawRect.Top:=0;
          DrawRect.Left:=0;
          DrawRect.Right:=round(Preview2.Image1.Width/2);
          DrawRect.Bottom:=round(Preview2.Image1.Height/2);
         end;
      5: begin
          DrawRect.Top:=0;
          DrawRect.Left:=round(Preview2.Image1.Width/2);
          DrawRect.Right:=Preview2.Image1.Width;
          DrawRect.Bottom:=round(Preview2.Image1.Height/2);
         end;
      6: begin
          DrawRect.Top:=round(Preview2.Image1.Height/2);
          DrawRect.Left:=0;
          DrawRect.Right:=round(Preview2.Image1.Width/2);
          DrawRect.Bottom:=Preview2.Image1.Height;
         end;
      7: begin
          DrawRect.Top:=round(Preview2.Image1.Height/2);
          DrawRect.Left:=round(Preview2.Image1.Width/2);
          DrawRect.Right:=Preview2.Image1.Width;
          DrawRect.Bottom:=Preview2.Image1.Height;
         end;
     end;
     if CounterCompact=4 then ClearPreview2;
     Preview2.Image1.Picture.Bitmap.Canvas.StretchDraw(DrawRect,ShowMetafile);
     if LoadingMetafiles then STR(ImageIndex+4,PageText) else STR(ImageIndex+5,PageText);
     if LoadingMetafiles then STR(ImageIndex+CounterCompact,PageText2) else STR(ImageIndex+1+CounterCompact,PageText2);
     if PageText=PageText2 then Preview2.Label2.Caption:=PageText else Preview2.Label2.Caption:=PageText+'-'+PageText2;
     str(MetafileCount+1,TotalNumber);
     Label10.Caption:='('+TotalNumber+')';
     Image1.Refresh;
     ShowMetafile.free;
     DeleteMetafile;
    end;
   end
  else Stop:=true;
   CounterCompact:=CounterCompact+1;
  until Stop;
  FirstLoad:=false;
 end
else  {not compact mode}
 begin
  if RichPreview2Exist then LabelsVisiblePreview2(true);
  if LoadingMetafiles then str(ImageIndex-1,FileNameMetafileExt) else str(ImageIndex,FileNameMetafileExt);
  FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
  if CompressPreview then AssignFile(FileTyp,FileNameMetafile+'.CPR') else AssignFile(FileTyp,FileNameMetafile);
  {$I-}
  RESET(FileTyp);
  {$I+}
  FilePresent:=IOResult;
  if FilePresent=0 then CloseFile(FileTyp);
  IF (FilePresent=0) and CompressPreview then
  begin
   FileCompress:=TCompress.Create;
   FileCompress.InFile := TFileStream.Create(FileNameMetafile+'.CPR', fmOpenRead);
   FileCompress.OutFile := TFileStream.Create(FileNameMetafile, fmCreate);
   FileCompress.ZStream := TDecompressionStream.Create(FileCompress.InFile);
   while True do
   begin
    Count := FileCompress.ZStream.Read(FileCompress.Buffer, BufferSize);
    if Count <> 0 then FileCompress.OutFile.WriteBuffer(FileCompress.Buffer, Count) else Break;
   end;
   FileCompress.ZStream.Free;
   FileCompress.OutFile.Free;
   FileCompress.InFile.Free;
   FileCompress.Free;
  end;
  IF FilePresent=0 then
  begin
   ShowMetafile:=TMetafile.Create;
   if LoadAgain then
   begin
    {CloseFile(FileTyp);}
    ShowMetafile.LoadFromfile(FileNameMetafile);
    Persistent.Assign(ShowMetafile);
   end else ShowMetafile.Assign(Persistent);
   if Compact and RichPreview2Exist then
   begin
    RichPreview2Exist:=false;
    Preview2.Close;
   end;
   if ((not odd(ImageIndex)) or (not RichPreview2Exist)) then
   begin
    {first window}
    Scrolling(1);
    DrawRect.Top:=0;
    DrawRect.Left:=0;
    DrawRect.Right:=Image1.Width;
    DrawRect.Bottom:=Image1.Height;
    ClearPreview1;
    Image1.Picture.Bitmap.Canvas.StretchDraw(DrawRect,ShowMetafile);
    if LoadingMetafiles then STR(ImageIndex:4,PageText) else STR(ImageIndex+1:4,PageText);
    Edit1.Text:=PageText;
    str(MetafileCount+1,TotalNumber);
    Label10.Caption:='('+TotalNumber+')';
    Image1.Refresh;
    ShowMetafile.free;
   end
  else
   begin
    {second window}
    Scrolling(2);
    DrawRect.Top:=0;
    DrawRect.Left:=0;
    DrawRect.Right:=Image1.Width;
    DrawRect.Bottom:=Image1.Height;
    ClearPreview2;
    Preview2.Image1.Picture.Bitmap.Canvas.StretchDraw(DrawRect,ShowMetafile);
    if LoadingMetafiles then STR(ImageIndex:4,PageText) else STR(ImageIndex+1:4,PageText);
    Preview2.Label2.Caption:=PageText;
    str(MetafileCount+1,TotalNumber);
    Label10.Caption:='('+TotalNumber+')';
    Image1.Refresh;
    ShowMetafile.free;
   end;
   DeleteMetafile;
  end;
  FirstLoad:=false;
 end;
 SetupRulers(Self);
 if RichPreview2Exist then SetUpRulers(Preview2);
end;

procedure TRichPreviewForm.BitBtn1Click(Sender: TObject);
begin
 Close;
end;

procedure TRichPreviewForm.ArrowButton2Click(Sender: TObject);
var
 DeltaNumber: Integer;
begin
 LoadingMetafiles:=false;
 LoadAgain:=true;
 if Compact then
 begin
  if RichPreview2Exist then DeltaNumber:=8 else DeltaNumber:=4;
  ImageIndex:=ImageIndex+DeltaNumber;
  if ImageIndex >= DeltaNumber then SetupPgDown(True);
  if ImageIndex<=0 then SetupPgDown(False);
  if ImageIndex+DeltaNumber-1 >= MetafileCount then SetupPgUp(False) else SetupPgUp(True);
  if RichPreview2Exist then
  begin
   if ImageIndex+DeltaNumber-4 > MetafileCount then
   begin
    LabelsVisiblePreview2(false);
    ClearPreview2;
   end
  else LabelsVisiblePreview2(true);
  end;
 end
else
 begin
  if RichPreview2Exist then
  begin
   LabelsVisiblePreview2(true);
   ImageIndex:=ImageIndex+2;
  end else ImageIndex:=ImageIndex+1;
  if ImageIndex > MetafileCount Then ImageIndex:=MetafileCount;
  if (ImageIndex=MetafileCount) or (RichPreview2Exist and (ImageIndex+1>=MetafileCount)) then
  begin
   SetupPgDown(True);
   SetupPgUp(False);
  end
 else
  begin
   SetupPgDown(True);
   SetupPgUp(True);
  end;
 end;
 if RichPreview2Exist and (not Compact) then
 begin
  LabelsVisiblePreview2(true);
  FormPaint(Sender);
  ImageIndex:=ImageIndex+1;
  if ImageIndex <= MetafileCount then FormPaint(Sender) else
  begin
   ClearPreview2;
   LabelsVisiblePreview2(false);
  end;
  ImageIndex:=ImageIndex-1;
 end
else
 begin
  FormPaint(Sender);
 end;
end;

procedure TRichPreviewForm.ArrowButton1Click(Sender: TObject);
var DeltaNumber: Integer;
begin
 if ImageIndex > MetafileCount Then ImageIndex:=MetafileCount;
 LoadingMetafiles:=false;
 LoadAgain:=true;
 if Compact then
 begin
  if RichPreview2Exist then DeltaNumber:=8 else DeltaNumber:=4;
  ImageIndex:=ImageIndex-DeltaNumber;
 end
else
 begin
  if RichPreview2Exist then ImageIndex:=ImageIndex-2 else ImageIndex:=ImageIndex-1;
 end;
 if ImageIndex < 0 Then ImageIndex:=0;
 if ImageIndex=0 then
 begin
  SetupPgDown(false);
  SetupPgUp(True);
 end
else
 begin
  SetupPgDown(True);
  SetupPgUp(True);
 end;
 if RichPreview2Exist then
 begin
  if MetaFileCount > 3 then LabelsVisiblePreview2(true);
 end;
 if RichPreview2Exist and (Not Compact) then
 begin
  LabelsVisiblePreview2(true);
  FormPaint(Sender);
  ImageIndex:=ImageIndex+1;
  if ImageIndex <= MetafileCount then FormPaint(Sender);
  ImageIndex:=ImageIndex-1;
 end
else
 begin
  FormPaint(Sender);
 end;
end;

procedure TRichPreviewForm.SpeedButton1Click(Sender: TObject);
var Index,Count: Integer;
    Rectangle: TRect;
    FileNameMetafileExt: string;
    PrintMetafile: TMetafile;
    FileTyp: file of TMetafile;
    FileCompress: TCompress;
    I,CounterCompact,PreviewPaperWidthCompact,PreviewPaperHeightCompact,ImageIndexPrev: Integer;
begin
 ImageIndexPrev:=ImageIndex;
 if Compact then
 begin
  LoadingMetafiles:=false;
  LoadAgain:=true;
  Screen.Cursor:=crHourGlass;
  PrintDialog1.FromPage:=1;
  PrintDialog1.ToPage:=trunc((MetafileCount+1)/4)+1;
  PrintDialog1.MinPage:=1;
  PrintDialog1.MaxPage:=trunc((MetafileCount+1)/4)+1;
  if PrintDialog1.Execute then
  begin
   if PrintDialog1.PrintRange=prAllPages then
   begin
    StartPage:=1;
    EndPage:=PrintDialog1.MaxPage;
   end;
   if PrintDialog1.PrintRange=prPageNums then
   begin
    StartPage:=PrintDialog1.FromPage;
    EndPage:=PrintDialog1.ToPage;
   end;
   PrintMetafile:=TMetafile.Create;
   PreviewPaperWidthCompact:=PreviewPaperWidth;
   PreviewPaperHeightCompact:=round(0.95*PreviewPaperHeight);
   for Index:=StartPage-1 to EndPage-1 do
   begin
    ImageIndex:=Index*4;
    CounterCompact:=0;
    Printer.BeginDoc;
    for I:=1 to 4 do
    Begin
     case CounterCompact of
      0: begin
          Rectangle.Top:=0;
          Rectangle.Left:=0;
          Rectangle.Right:=round(PreviewPaperWidthCompact/2);
          Rectangle.Bottom:=round(PreviewPaperHeightCompact/2);
         end;
      1: begin
          Rectangle.Top:=0;
          Rectangle.Left:=round(PreviewPaperWidthCompact/2);
          Rectangle.Right:=PreviewPaperWidthCompact;
          Rectangle.Bottom:=round(PreviewPaperHeightCompact/2);
         end;
      2: begin
          Rectangle.Top:=round(PreviewPaperHeightCompact/2);
          Rectangle.Left:=0;
          Rectangle.Right:=round(PreviewPaperWidthCompact/2);
          Rectangle.Bottom:=PreviewPaperHeightCompact;
         end;
      3: begin
          Rectangle.Top:=round(PreviewPaperHeightCompact/2);
          Rectangle.Left:=round(PreviewPaperWidthCompact/2);
          Rectangle.Right:=PreviewPaperWidthCompact;
          Rectangle.Bottom:=PreviewPaperHeightCompact;
         end;
     end;
     str(ImageIndex+CounterCompact,FileNameMetafileExt);
     FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
     if CompressPreview then AssignFile(FileTyp,FileNameMetafile+'.CPR') else AssignFile(FileTyp,FileNameMetafile);
     {$I-}
     RESET(FileTyp);
      {$I+}
     IF IOResult=0 then
     begin
      CloseFile(FileTyp);
      {compression}
      IF CompressPreview then
      begin
       FileCompress:=TCompress.Create;
       FileCompress.InFile := TFileStream.Create(FileNameMetafile+'.CPR', fmOpenRead);
       FileCompress.OutFile := TFileStream.Create(FileNameMetafile, fmCreate);
       FileCompress.ZStream := TDecompressionStream.Create(FileCompress.InFile);
       while True do
       begin
        Count := FileCompress.ZStream.Read(FileCompress.Buffer, BufferSize);
        if Count <> 0 then FileCompress.OutFile.WriteBuffer(FileCompress.Buffer, Count) else Break;
       end;
       FileCompress.ZStream.Free;
       FileCompress.OutFile.Free;
       FileCompress.InFile.Free;
       FileCompress.Free;
      end;
      PrintMetafile.LoadFromfile(FileNameMetafile);
      Printer.Canvas.StretchDraw(Rectangle,PrintMetafile);
      DeleteMetafile;
     end;
     CounterCompact:=CounterCompact+1;
    end;
    Printer.EndDoc;
   end;
   PrintMetafile.free;
  end;
 end
else
 begin
  LoadingMetafiles:=false;
  LoadAgain:=true;
  Screen.Cursor:=crHourGlass;
  PrintDialog1.FromPage:=1;
  PrintDialog1.ToPage:=MetafileCount+1;
  PrintDialog1.MinPage:=1;
  PrintDialog1.MaxPage:=MetafileCount+1;
  if PrintDialog1.Execute then
  begin
   if PrintDialog1.PrintRange=prAllPages then
   begin
    StartPage:=1;
    EndPage:=PrintDialog1.MaxPage;
   end;
   if PrintDialog1.PrintRange=prPageNums then
   begin
    StartPage:=PrintDialog1.FromPage;
    EndPage:=PrintDialog1.ToPage;
   end;
   PrintMetafile:=TMetafile.Create;
   Rectangle.Top:=0;
   Rectangle.Left:=0;
   Rectangle.Right:=PreviewPaperWidth;
   Rectangle.Bottom:=PreviewPaperHeight;
   for Index:=StartPage-1 to EndPage-1 do
   begin
    ImageIndex:=Index;
    FormPaint(Sender);
    str(Index,FileNameMetafileExt);
    FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
    if CompressPreview then AssignFile(FileTyp,FileNameMetafile+'.CPR') else AssignFile(FileTyp,FileNameMetafile);
    {$I-}
    RESET(FileTyp);
     {$I+}
    IF IOResult=0 then
    begin
     CloseFile(FileTyp);
     {compression}
     IF CompressPreview then
     begin
      FileCompress:=TCompress.Create;
      FileCompress.InFile := TFileStream.Create(FileNameMetafile+'.CPR', fmOpenRead);
      FileCompress.OutFile := TFileStream.Create(FileNameMetafile, fmCreate);
      FileCompress.ZStream := TDecompressionStream.Create(FileCompress.InFile);
      while True do
      begin
       Count := FileCompress.ZStream.Read(FileCompress.Buffer, BufferSize);
       if Count <> 0 then FileCompress.OutFile.WriteBuffer(FileCompress.Buffer, Count) else Break;
      end;
      FileCompress.ZStream.Free;
      FileCompress.OutFile.Free;
      FileCompress.InFile.Free;
      FileCompress.Free;
     end;
     PrintMetafile.LoadFromfile(FileNameMetafile);
     Printer.BeginDoc;
     Printer.Canvas.StretchDraw(Rectangle,PrintMetafile);
     Printer.EndDoc;
     DeleteMetafile;
    end;
   end;
   PrintMetafile.free;
  end;
 end;
 ImageIndex:=ImageIndexPrev;
 Screen.Cursor:=crDefault;
end;


procedure TRichPreviewForm.SpeedButton2Click(Sender: TObject);
var ZoomText: string;

begin
 Screen.Cursor:=crHourGlass;
 LoadingMetafiles:=false;
 if Compact then LoadAgain:=true else LoadAgain:=false;
{ if ImageIndex > MetafileCount Then ImageIndex:=MetafileCount;}
 Zoomfactor:=Zoomfactor+Zoomstep;
 str(Zoomfactor:5:3,ZoomText);
 Label7.Caption:=ZoomText;
 if Zoomfactor > 1 then SpeedButton3.Enabled:=true;
 if RichPreview2Exist and (not Compact) then
 begin
  LoadAgain:=true;
  FormPaint(Sender);
  ImageIndex:=ImageIndex+1;
  if ImageIndex <= MetafileCount then FormPaint(Sender) else ClearPreview2;
  ImageIndex:=ImageIndex-1;
 end
else
 begin
  FormPaint(Sender);
 end;
 if Zoomfactor> 1 then Image1.Hint:='Click right mouse button for original size' else Image1.Hint:='';
 Screen.Cursor:=crDefault;
end;

procedure TRichPreviewForm.SpeedButton3Click(Sender: TObject);
var ZoomText: string;
begin
 Screen.Cursor:=crHourGlass;
 LoadingMetafiles:=false;
 if Compact then LoadAgain:=true else LoadAgain:=false;
 {if ImageIndex > MetafileCount Then ImageIndex:=MetafileCount;}
 Zoomfactor:=Zoomfactor-Zoomstep;
 if Zoomfactor < 1 then Zoomfactor:=1;
 if Zoomfactor=1 then SpeedButton3.Enabled:=false else SpeedButton3.Enabled:=true;
 str(Zoomfactor:5:3,ZoomText);
 Label7.Caption:=ZoomText;
 if RichPreview2Exist and (not Compact) then
 begin
  LoadAgain:=true;
  FormPaint(Sender);
  ImageIndex:=ImageIndex+1;
  if ImageIndex <= MetafileCount then FormPaint(Sender) else ClearPreview2;
  ImageIndex:=ImageIndex-1;
 end
else
 begin
  FormPaint(Sender);
 end;
 if Zoomfactor> 1 then Image1.Hint:='Click right mouse button for original size' else Image1.Hint:='';
 Screen.Cursor:=crDefault;
end;

procedure TRichPreviewForm.MaskEdit1Change(Sender: TObject);
var Code: Integer;
begin
 val(MaskEdit1.Text,Zoomstep,Code);
 if Code <> 0 then ZoomStep:=0.1;
end;

procedure TRichPreviewForm.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ZoomText: string;
begin
 If Button=mbRight then
 begin
  Zoomfactor:=1;
  str(Zoomfactor:5:3,ZoomText);
  Label7.Caption:=ZoomText;
  SpeedButton3.Enabled:=false;
  if RichPreview2Exist and (not Compact) then
  begin
   LoadAgain:=true;
   FormPaint(Sender);
   ImageIndex:=ImageIndex+1;
   if ImageIndex <= MetafileCount then FormPaint(Sender) else ClearPreview2;
   ImageIndex:=ImageIndex-1;
  end
 else
  begin
   FormPaint(Sender);
  end;
  if Zoomfactor> 1 then Image1.Hint:='Click right mouse button for original size' else Image1.Hint:='';
 end;
end;

procedure TRichPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 LoadingMetafiles:=false;
 case Key of
  33: if ArrowButton2.Enabled then ArrowButton2Click(Sender);
  34: if ArrowButton1.Enabled then ArrowButton1Click(Sender);
  35: begin {to the end of the document}
       if (not Compact) and (not RichPreview2Exist) then ImageIndex:=MetafileCount
      else
       begin
        if (not Compact) and RichPreview2Exist then
        begin
         if not odd(MetafileCount) then ImageIndex:=MetafileCount else ImageIndex:=MetafileCount-1;
        end
       else
        begin
         if Compact and RichPreview2Exist then ImageIndex:=trunc(MetafileCount/8)*8 else ImageIndex:=MetafileCount;
        end
       end;
       SetupPgUp(False);
       SetupPgDown(True);
       if RichPreview2Exist and (not Compact) then
       begin
        FormPaint(Sender);
        if ImageIndex+1 > MetafileCount then
        begin
         ClearPreview2;
         LabelsVisiblePreview2(false);
        end
       else
        begin
         ImageIndex:=ImageIndex+1;
         FormPaint(Sender);
         ImageIndex:=ImageIndex-1;
         LabelsVisiblePreview2(true);
        end;
        if MetafileCount <=1 then
        begin
         SetupPgDown(false);
         SetupPgUp(false);
        end;
       end
      else
       begin
        if RichPreview2Exist and Compact then
        begin
         if (MetaFileCount <= 3) then LabelsVisiblePreview2(false);
         if MetaFileCount <= 7 then
         begin
          SetupPgDown(false);
          SetupPgUp(false);
         end
        else
         begin
          if ImageIndex+4 > MetafileCount then
          begin
           LabelsVisiblePreview2(false);
           ClearPreview2;
          end; 
         end;
         FormPaint(Sender);
        end;
       end;
      end;
  36: begin
       ImageIndex:=0; {to the start of the document}
       SetupPgUp(true);
       SetupPgDown(false);
       if RichPreview2Exist and (not Compact) then
       begin
        LabelsVisiblePreview2(true);
        FormPaint(Sender);
        ImageIndex:=ImageIndex+1;
        if ImageIndex <= MetafileCount then FormPaint(Sender) else ClearPreview2;
        ImageIndex:=ImageIndex-1;
        if MetafileCount <=1 then
        begin
         SetupPgDown(false);
         SetupPgUp(false);
        end;
       end
      else
       begin
        if RichPreview2Exist then
        begin
         if MetaFileCount > 3 then
         begin
          LabelsVisiblePreview2(true);
         end
        else
         begin
          SetupPgDown(false);
          SetupPgUp(false);
         end;
        end;
        if MetafileCount <= 7 then
        begin
         SetupPgDown(false);
         SetupPgUp(false);
        end else FormPaint(Sender);
       end;
      end;
  38: with Scrollbox1 do VertScrollBar.Position:=VertScrollBar.Position-VertScrollBar.Increment;
  40: with Scrollbox1 do VertScrollBar.Position:=VertScrollBar.Position+VertScrollBar.Increment;
  37: with Scrollbox1 do HorzScrollBar.Position:=HorzScrollBar.Position-HorzScrollBar.Increment;
  39: with Scrollbox1 do HorzScrollBar.Position:=HorzScrollBar.Position+HorzScrollBar.Increment;
 end;
end;

procedure TRichPreviewForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
 LoadingMetafiles:=false;
 case Key of
  'l','L': SpeedButton2Click(Sender);
  's','S': SpeedButton3Click(Sender);
  'p','P': SpeedButton1Click(Sender);
 end;
end;

procedure TRichPreviewForm.Label7DblClick(Sender: TObject);
var ZoomText: string;
begin
 Zoomfactor:=1;
 str(Zoomfactor:5:3,ZoomText);
 Label7.Caption:=ZoomText;
 SpeedButton3.Enabled:=false;
 if RichPreview2Exist and (not Compact) then
 begin
  LoadAgain:=true;
  FormPaint(Sender);
  ImageIndex:=ImageIndex+1;
  FormPaint(Sender);
  ImageIndex:=ImageIndex-1;
  if ImageIndex <= MetafileCount then FormPaint(Sender) else ClearPreview2;
 end
else
 begin
  FormPaint(Sender);
 end;
end;

procedure TRichPreviewForm.FormActivate(Sender: TObject);
begin
 setfocus;
 if not FirstLoad then
 begin
  Label10.Visible:=true;
  ImageIndex:=0;
  LoadingMetafiles:=false;
  LoadAgain:=true;
  Label2.Enabled:=false;
  Label9.Enabled:=false;
  SetupPgUp(True);
  if (not RichPreview2Exist) and (MetafileCount > 0) then
  begin {two screens}
   Left:=round(0.5*(Screen.Width-2*Width));
   if Left < 0 Then Left:=0;
   if Left+2*Width <= Screen.Width then
   begin
    Preview2:=TPreview2.Create(RichPreviewForm);
    RichPreview2Exist:=true;
    SetFocus;
   end else Left:=round(0.5*(Screen.Width-Width));
  end;
  FormPaint(Sender);
  if RichPreview2Exist and (not Compact) then
  begin
   ImageIndex:=ImageIndex+1;
   FormPaint(Sender);
   ImageIndex:=ImageIndex-1;
  end;
  RichPreviewForm.Caption:='Print Preview';
 end;
 if MetaFileCount > 0 then Edit1.Readonly:=false else Edit1.Readonly:=true;
 if MetaFileCount > 0 Then
 begin
  CheckBox1.Visible:=true;
  if Not Compact then CheckBox1.Checked:=false else CheckBox1.Checked:=true;
 end else CheckBox1.Visible:=false;
 if (MetafileCount <=1) and (not Compact) and RichPreview2Exist then
 begin
  SetupPgDown(false);
  SetupPgUp(false);
 end;
 SetupRulers(Self);
end;


procedure TRichPreviewForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var TextEdit: String;
Var PageNumberInput,Code,DefPage: Integer;
begin
 if Edit1.Modified then
 begin
  val(Edit1.Text,PageNumberInput,code);
  IF Compact then
  begin
   if RichPreview2Exist then
   begin
    if (PageNumberInput > 0) and (PageNumberInput <= MetafileCount+1) then
    begin
     PageNumberInput:=round(PageNumberInput/8)*8+1;
     if PageNumberInput <= 0 then PageNumberInput:=1;
     if PageNumberInput > MetafileCount+1 then PageNumberInput:=MetafileCount+1;
     str(PageNumberInput,TextEdit);
     Edit1.Text:=TextEdit;
     ImageIndex:=PageNumberInput-1;
     FormPaint(Sender);
     if ImageIndex > 0 then SetupPgDown(True) else SetupPgDown(False);
     if ImageIndex >= MetafileCount then SetupPgUp(False) else SetupPgUp(True);
    end;
   end else Edit1.Text:='1';
  end
 else
  begin
   if (PageNumberInput > 0) and (PageNumberInput <= MetafileCount+1) then
   begin
    if (not odd(PageNumberInput-1)) or (not RichPreview2Exist) then
    begin
     {first window}
     ImageIndex:=PageNumberInput-1;
     DefPage:=ImageIndex;
     FormPaint(Sender);
     ImageIndex:=PageNumberInput;
     if IMageIndex > MetaFileCount then
     begin
      ClearPreview2;
      LabelsVisiblePreview2(false);
     end
    else
     begin
      LabelsVisiblePreview2(true);
      FormPaint(Sender);
     end;
    end
   else
    begin
     {second window}
     ImageIndex:=PageNumberInput-2;
     DefPage:=ImageIndex;
     FormPaint(Sender);
     ImageIndex:=ImageIndex+1;
     FormPaint(Sender);
    end;
   end;
   ImageIndex:=DefPage;
   if ImageIndex > 0 then SetupPgDown(True) else SetupPgDown(False);
   if ImageIndex >= MetafileCount then SetupPgUp(False) else SetupPgUp(True);
  end;
 end;
end;

procedure TRichPreviewForm.CheckBox1Click(Sender: TObject);
begin
 if CheckBox1.Checked then Compact:=true else Compact:=false;
 ImageIndex:=0;
 if Compact Then
 begin
  if RichPreview2Exist then
  begin
   if MetaFileCount <= 3 then
   begin
    LabelsVisiblePreview2(false);
    ClearPreview2;
   end;
   if MetaFileCount<=7 then
   begin
    SetupPgUp(False);
    SetupPgDown(False);
   end else SetupPgUp(True);
  end;
 end
else SetupPgUp(True);
 FormPaint(Sender);
 if RichPreview2Exist and (not Compact) then
 begin
  ImageIndex:=ImageIndex+1;
  FormPaint(Sender);
  ImageIndex:=ImageIndex-1;
 end;
end;
 
end.
