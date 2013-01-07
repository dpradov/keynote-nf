unit RichPreview2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TPreview2 = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
   ImageIndex,PreviewImageHeight,PreviewImageWidth: Integer;
  public
    { Public declarations }
 end;

var
  Preview2: TPreview2;

implementation

{$R *.DFM}
uses RichPreview,RichPrint;

procedure TPreview2.FormCreate(Sender: TObject);
begin
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
 ClientHeight:=Height-27;

 ImageIndex:=0;
 Width:=round(1.0204*PreviewImageWidth);
 ClientWidth:=Width-8;

 if ClientWidth < 417+round(0.02*ClientWidth) Then {in order to function with a screen resolution of 800*600 or even lower}
 begin                                             {preferable the resolution should be 1024*768}
  Width:=round(1.0204*(417+0.02*ClientWidth));
  ClientWidth:=Width-8;
 end;
 Image1.Top:=round(0.01*ClientHeight)+16;
 Image1.Left:=round(0.01*ClientWidth)+18;
 Image1.Height:=round(0.98*ClientHeight)-87;
 Image1.Width:=round(0.98*ClientWidth)-18;
 ScrollBox1.Top:=Image1.Top;
 ScrollBox1.Left:=Image1.Left;
 ScrollBox1.Height:=Image1.Height;
 ScrollBox1.Width:=Image1.Width;
 Panel1.Top:=Image1.Top+Image1.Height+4;
 Panel1.Left:=0;
 Panel1.Width:=Image1.Width+22;
end;

procedure TPreview2.FormActivate(Sender: TObject);
begin
 Left:=RichPreviewForm.Left+RichPreviewForm.Width;
 Width:=RichPreviewForm.Width;
 Height:=RichPreviewForm.Height;
 Image1.Top:=RichPreviewForm.Image1.Top;
 Image1.Left:=RichPreviewForm.Image1.Left;
 Image1.Height:=RichPreviewForm.Image1.Height;
 Image1.Width:=RichPreviewForm.Image1.Width;
 ScrollBox1.Top:=RichPreviewForm.ScrollBox1.Top;
 ScrollBox1.Left:=RichPreviewForm.ScrollBox1.Left;
 ScrollBox1.Height:=RichPreviewForm.ScrollBox1.Height;
 ScrollBox1.Width:=RichPreviewForm.ScrollBox1.Width;
 Panel1.Width:=RichPreviewForm.Panel1.Width;
end;

procedure TPreview2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 RichPreviewForm.setfocus;
end;

procedure TPreview2.FormPaint(Sender: TObject);
begin
 RichPreviewForm.SetupRulers(Self);
end;

end.
