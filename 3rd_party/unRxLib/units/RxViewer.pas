unit RxViewer;
{$I RX.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, MPlayer, RxAniFile, RxAnimate, RxGIFCtrl,
  {$IFDEF RX_D16}Vcl.Imaging.jpeg{$ELSE}jpeg{$ENDIF},
  RxRichEd;

const
  AllButtons: TButtonSet = [btPlay, btPause, btStop, btNext, btPrev, btStep, btBack, btRecord, btEject];
  SuppExt = '.bmp;.jpg;.avi;.ico;.emf;.wmf;.wav;.mid;.rmi;.midi;.mov;.ani;.gif;.txt;.rtf;';

type
  {  TRxMediaType  }

  TRxMediaType = (MTNone, MTImage, MTAnimation, MTVideo, MTSound, MTText);

  {  TRxMediaController  }

  TRxMediaController = class(TPersistent)
  protected
    FControl: TControl;
    FOnChange: TNotifyEvent;
    FShow: boolean;
    FLeft, FTop, FWidth, FHeight: integer;
    FVisibleButtons, FEnabledButtons, FColoredButtons: TButtonSet;
    procedure ChangeShow(s: boolean);
    procedure ChangeGeom(i: integer; g: integer);
    procedure ChangeButt(i: integer; b: TButtonSet);
    property Control: TControl read FControl;
    procedure Change; dynamic;
  public
    constructor Create(Control: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Show: boolean read FShow write ChangeShow default True;
    property Left: integer index 1 read FLeft write ChangeGeom default 0;
    property Top: integer index 2 read FTop write ChangeGeom default 0;
    property Width: integer index 3 read FWidth write ChangeGeom default 40;
    property Height: integer index 4 read FHeight write ChangeGeom default 20;
    property VisibleButtons: TButtonSet index 10 read FVisibleButtons write ChangeButt;
    property EnabledButtons: TButtonSet index 11 read FEnabledButtons write ChangeButt;
    property ColoredButtons: TButtonSet index 12 read FColoredButtons write ChangeButt;
  end;

  {  TRxViewport  }

  TRxViewport = class(TPersistent)
  protected
    FControl: TControl;
    FOnChange: TNotifyEvent;
    FLeft, FTop, FWidth, FHeight: integer;
    FStretch: boolean;
    procedure ChangeGeom(i: integer; g: integer);
    procedure ChangeStretch(s: boolean);
    property Control: TControl read FControl;
    procedure Change; dynamic;
  public
    constructor Create(Control: TControl); virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Stretch: boolean read FStretch write ChangeStretch default True;
    property Left: integer index 1 read FLeft write ChangeGeom default 0;
    property Top: integer index 2 read FTop write ChangeGeom default 20;
    property Width: integer index 3 read FWidth write ChangeGeom default 200;
    property Height: integer index 4 read FHeight write ChangeGeom default 200;
  end;

  {  TRxViewer  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxViewer = class(TPanel)
  private
    { Private declarations }

    // other
    FMediaType: TRxMediaType;
    FController: TRxMediaController;
    FFileName: string;
    FError: boolean;
    FViewport: TRxViewport;
    FOnLoad: TNotifyEvent;
    FOnMTC: TNotifyEvent;
    CurHeight, CurWidth: integer;
    procedure SetFileName(FName: string);
    procedure LoadFile;
  protected
    { Protected declarations }
    procedure DoControllerChange(Sender: TObject);
    procedure DoViewportChange(Sender: TObject);
    procedure Resize; override;
  public
    { Public declarations }
    // standard virewing types
    Picture: TImage;
    Media: TMediaPlayer;
    Animate: TAnimatedImage;
    Gif: TRXGifAnimator;
    Edit: TRXRichEdit;

    constructor Create(AOwner: TComponent); override;
    procedure HideAll;
    property ImageWidth: integer read CurWidth;
    property ImageHeight: integer read CurHeight;
    property MediaType: TRxMediaType read FMediaType;
    destructor Destroy; override;
  published
    { Published declarations }
    property FileName: string read FFileName write SetFileName;
    property Controller: TRxMediaController read FController write FController;
    property Viewport: TRxViewport read FViewport write FViewport;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnMediaTypeChange: TNotifyEvent read FOnMTC write FOnMTC;
  end;

implementation

{  TRxViewer  }

constructor TRxViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := '';
  BevelOuter := bvNone;
  Width := 200;
  Height := 220;

  // Creating viewing resources
  Picture := TImage.Create(Self);
  Picture.Parent := Self;
  Picture.Width := 200;
  Picture.Height := 200;
  Picture.Proportional := True;
  Picture.Autosize := True;
  Picture.Stretch := True;

  Media := TMediaPlayer.Create(Self);
  Media.Visible := False;
  Media.Parent := Self;
  Media.Display := Self;
  Media.Shareable := False;

  Animate := TAnimatedImage.Create(Self);
  Animate.Parent := Self;
  Animate.Width := 200;
  Animate.Height := 200;

  Gif := TRXGifAnimator.Create(Self);
  Gif.Parent := Self;
  Gif.Width := 200;
  Gif.Height := 200;

  Edit := TRXRichEdit.Create(Self);
  Edit.Parent := Self;
  Edit.Width := 200;
  Edit.Height := 200;
  Edit.ReadOnly := True;
  Edit.Visible := False;
  Edit.Align := alClient;

  // Setting FController
  FController := TRxMediaController.Create(Self);
  with FController do
  begin
    Show := True;
    Left := 0;
    Top := 0;
    Height := 20;
    Width := 40;
    VisibleButtons := AllButtons;
    ColoredButtons := AllButtons;
    EnabledButtons := AllButtons;
    OnChange := DoControllerChange;
  end;
  DoControllerChange(FController);

  // Setting FViewport
  FViewport := TRxViewport.Create(Self);
  with FViewport do
  begin
    Left := 0;
    Top := 20;
    Height := 200;
    Width := 200;
    OnChange := DoViewportChange;
  end;
  DoViewportChange(FViewport);
end;

destructor TRxViewer.Destroy;
begin
  // Freeing viewing resources
  Picture.Free;
  Media.Free;
  Animate.Free;
  Gif.Free;
  Edit.Free;

  FController.Free;
  FViewport.Free;

  inherited Destroy;
end;

procedure TRxViewer.Resize;
var
  percent: real;
begin
  inherited Resize;
  if (MediaType = MTImage) or (MediaType = MTAnimation) or (MediaType = MTVideo) then
  begin
    if (CurWidth > Width) or (CurHeight > Height) then
    begin
      Percent := CurWidth / CurHeight;
      FViewport.Stretch := True;
      if Percent > 1 then
      begin // CurWidth>CurHeight
        FViewport.Width := Width;
        FViewport.Height := Round(FViewport.Width / Percent);
      end;
      if Percent < 1 then
      begin // CurWidth<CurHeight
        FViewport.Height := Height;
        FViewport.Width := Round(Height * Percent);
      end;
      if Percent = 1 then
      begin // CurWidth=CurHeight
        FViewport.Height := FViewport.Width;
      end;
    end
    else
      if (CurWidth <> 0) or (CurHeight <> 0) then
      begin
        FViewport.Height := CurHeight;
        FViewport.Width := CurWidth;
        FViewPort.Stretch := False;
      end;
  end;

  // do whatever happened
  DoViewportChange(FViewport);
end;

procedure TRxViewer.DoControllerChange(Sender: TObject);
begin
  with (Sender as TRxMediaController) do
  begin
    if Show then
    begin
      Media.SetBounds(Left, Top, Width, Height);
      Media.VisibleButtons := Controller.VisibleButtons;
      Media.ColoredButtons := Controller.ColoredButtons;
      Media.EnabledButtons := Controller.EnabledButtons;
      Media.Visible := True;
    end
    else
    begin
      Media.Visible := False;
    end;
  end;
end;

procedure TRxViewer.DoViewportChange(Sender: TObject);
begin
  with (Sender as TRxViewport) do
  begin
    if Stretch then
      Media.DisplayRect := Rect(Left, Top, Left + Width, Top + Height)
    else
      Media.DisplayRect := Rect(Left, Top, 0 {Left+Width}, 0 {Top+Height});
    Picture.SetBounds(Left, Top, Width, Height);
    Animate.SetBounds(Left, Top, Width, Height);
    Gif.SetBounds(Left, Top, Width, Height);
    Picture.Stretch := Stretch;
    Picture.AutoSize := False;
    Gif.Stretch := Stretch;
    Gif.AutoSize := False;
  end;
end;

///////////////////////////////
// When the file name is set //
// so load and paint it      //
///////////////////////////////

procedure TRxViewer.SetFileName(FName: string);
var
  FE: string;
begin
  if FName = '' then
  begin
    HideAll;
    Exit;
  end;
  if LowerCase(FFileName) = LowerCase(FName) then Exit;
  if not FileExists(FName) then Exit;
  FE := LowerCase(ExtractFileExt(FName));
  if Pos(FE + ';', SuppExt) = 0 then Exit;
  FFileName := FName;
  LoadFile;
end;

procedure TRxViewer.HideAll;
begin
  Media.Hide;
  Media.Close;
  Animate.Hide;
  Picture.Hide;
  Gif.Hide;
  Edit.Hide;
end;

///////////////////////////////////////////
// Loading file into component resources //
///////////////////////////////////////////

procedure TRxViewer.LoadFile;
var
  FileExt: string;
  Img: TAnimatedCursorImage;
begin
  // defining extention
  FileExt := LowerCase(ExtractFileExt(FFileName));

  /////////////////
  // PICTUREFile //
  /////////////////

  if (FileExt = '.bmp') or (FileExt = '.ico') or (FileExt = '.wmf') or
    (FileExt = '.emf') or (FileExt = '.jpg') then
  begin
    FMediaType := MTImage;
    if Assigned(FOnMTC) then FOnMTC(Self);

    HideAll;
    try
      with Picture do
      begin
        Picture.LoadFromFile(FFileName);
        CurWidth := Picture.Width;
        CurHeight := Picture.Height;
        Resize;
        Show;
        BringToFront;
      end;
    except
      FError := True;
    end;
  end
  else

  /////////////
  // ANIFile //
  /////////////

    if (FileExt = '.ani') then
    begin
      FMediaType := MTAnimation;
      if Assigned(FOnMTC) then FOnMTC(Self);

      HideAll;
      try
        Img := TAnimatedCursorImage.Create;
        try
          with Animate do
          begin
            Active := False;
            Img.LoadFromFile(FFileName);
            Img.AssignToBitmap(Animate.Glyph, clFuchsia, False,
              Animate.Orientation = goVertical);
            Interval := Img.DefaultRate;
            TransparentColor := clFuchsia;
            Active := True;
            Show;
            BringToFront;
          end;
        finally
          Img.Free;
        end;
      except
        FError := True;
      end;
    end
    else

  /////////////
  // GIFFile //
  /////////////

      if (FileExt = '.gif') then
      begin
        FMediaType := MTAnimation;
        if Assigned(FOnMTC) then FOnMTC(Self);

        HideAll;
        try
          with Gif do
          begin
            Image.LoadFromFile(FFileName);
            CurWidth := Gif.Image.Width;
            CurHeight := Gif.Image.Height;
            Resize;
            Show;
            BringToFront;
            Animate := True;
          end;
        except
          FError := True;
        end;
      end
      else

  ///////////////
  // VIDEOFile //
  ///////////////

        if (FileExt = '.avi') or (FileExt = '.mov') then
        begin
          FMediaType := MTVideo;
          if Assigned(FOnMTC) then FOnMTC(Self);

          HideAll;
          try
            Media.FileName := FFileName;
            with Media do
            begin
              if Controller.Show then
              begin
                Show;
                BringToFront;
              end;
              Open;
              CurWidth := DisplayRect.Right - DisplayRect.Left;
              CurHeight := DisplayRect.Bottom - DisplayRect.Top;
              Resize;
              DoViewportChange(FViewport);
              Play;
            end;
          except
            FError := True;
          end;
        end
        else

  ///////////////
  // SOUNDFile //
  ///////////////

          if (FileExt = '.wav') or (FileExt = '.mid') or (FileExt = '.rmi') or
            (FileExt = '.midi') then
          begin
            FMediaType := MTSound;
            if Assigned(FOnMTC) then FOnMTC(Self);

            HideAll;
            try
              Media.FileName := FFileName;
              with Media do
              begin
                if Controller.Show then
                begin
                  Show;
                  BringToFront;
                end;
                Open;
                Play;
              end;
            except
              FError := True;
            end;
          end
          else

  //////////////
  // TEXTFile //
  //////////////

            if (FileExt = '.txt') or (FileExt = '.rtf') then
            begin
              FMediaType := MTText;
              if Assigned(FOnMTC) then FOnMTC(Self);

              HideAll;
              try
                with Edit do
                begin
                  Lines.LoadFromFile(FFileName);
                  Show;
                  BringToFront;
                end;
              except
                FError := True;
              end;
            end
            else

  ////////////
  // NOFile //
  ////////////

            begin
              FMediaType := MTNone;
              if Assigned(FOnMTC) then FOnMTC(Self);
            end;

  if Assigned(FOnLoad) then FOnLoad(Self);

end;

{  TRxMediaController  }

constructor TRxMediaController.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
end;

procedure TRxMediaController.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRxMediaController.ChangeShow(s: boolean);
begin
  if s = FShow then Exit;
  FShow := s;
  Change;
end;

procedure TRxMediaController.ChangeGeom(i: integer; g: integer);
begin
  case i of
    1:
      begin
        if g = FLeft then Exit;
        FLeft := g;
        Change;
      end;
    2:
      begin
        if g = FTop then Exit;
        FTop := g;
        Change;
      end;
    3:
      begin
        if g = FWidth then Exit;
        FWidth := g;
        Change;
      end;
    4:
      begin
        if g = FHeight then Exit;
        FHeight := g;
        Change;
      end;
  end;
end;

procedure TRxMediaController.ChangeButt(i: integer; b: TButtonSet);
begin
  case i of
    10:
      begin
        if b = VisibleButtons then Exit;
        FVisibleButtons := b;
        Change;
      end;
    11:
      begin
        if b = EnabledButtons then Exit;
        FEnabledButtons := b;
        Change;
      end;
    12:
      begin
        if b = ColoredButtons then Exit;
        FColoredButtons := b;
        Change;
      end;
  end;
end;

{  TRxViewport  }

constructor TRxViewport.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FWidth := 200;
  FHeight := 200;
  FTop := 20;
  FLeft := 0;
end;

procedure TRxViewport.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRxViewport.ChangeStretch(s: boolean);
begin
  if s = FStretch then Exit;
  FStretch := s;
  Change;
end;

procedure TRxViewport.ChangeGeom(i: integer; g: integer);
begin
  case i of
    1:
      begin
        if g = FLeft then Exit;
        FLeft := g;
        Change;
      end;
    2:
      begin
        if g = FTop then Exit;
        FTop := g;
        Change;
      end;
    3:
      begin
        if g = FWidth then Exit;
        FWidth := g;
        Change;
      end;
    4:
      begin
        if g = FHeight then Exit;
        FHeight := g;
        Change;
      end;
  end;
end;

end.

