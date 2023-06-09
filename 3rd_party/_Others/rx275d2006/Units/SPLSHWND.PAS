{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

unit SplshWnd;

interface

{$I RX.INC}

uses SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, 
  Animate, VCLUtils;

type
  TSplashWindow = class(TForm)
  private
    { Private declarations }
    FTextMessage: TLabel;
    function GetMessageText: string;
    procedure SetMessageText(const Value: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    Image: TImage;
    Animation: TAnimatedImage;
    procedure CenterFor(Form: TCustomForm);
    property MessageText: string read GetMessageText write SetMessageText;
  end;

function ShowSplashWindow(Graphic: TGraphic; const MsgText: string;
  Animate: Boolean; AlignForm: TCustomForm): TSplashWindow;

const
  SplashStayOnTop: Boolean = True;  

implementation

uses MaxMin;

const
  defSplashHeight = 64;
  defImageLeft = 16;
  defImageTop = 16;
  defTextWidth = 238;
  defTextLeft = 56;
  defTextRight = 16;

function CreateSplashWindow: TSplashWindow;
begin
{$IFDEF CBUILDER}
  Result := TSplashWindow.CreateNew(Application, 0);
{$ELSE}
  Result := TSplashWindow.CreateNew(Application);
{$ENDIF}
  with Result do begin
    BorderIcons := [];
    BorderStyle := bsNone;
    if SplashStayOnTop then 
      FormStyle := fsStayOnTop
    else
      FormStyle := fsNormal;
    ClientHeight := defSplashHeight;
    ClientWidth := defImageLeft + defTextRight + 32;
    Enabled := False;
    Font.Height := -11;
    Font.Name := 'MS Sans Serif';
    Font.Style := [];
    Font.Color := clWindowText;
    PixelsPerInch := 96;
    Scaled := True;

    Image := TImage.Create(Result);
    Image.Parent := Result;
    Image.Left := defImageLeft;
    Image.Top := defImageTop;
    Image.Width := 32;
    Image.Height := 32;
    Image.AutoSize := False;
    Image.Stretch := True;
    Image.Visible := False;

    FTextMessage := TLabel.Create(Result);
    FTextMessage.Parent := Result;
    FTextMessage.Left := defTextLeft;
    FTextMessage.Width := defTextWidth;
    FTextMessage.AutoSize := False;
    FTextMessage.Alignment := taCenter;
    FTextMessage.WordWrap := True;

    Animation := TAnimatedImage.Create(Result);
    Animation.Parent := Result;
    Animation.Left := defImageLeft;
    Animation.Top := defImageTop;
    Animation.Width := 32;
    Animation.Height := 32;
    Animation.Active := False;
    Animation.AutoSize := False;
    Animation.Stretch := True;
    Animation.Visible := False;
  end;
end;

function ShowSplashWindow(Graphic: TGraphic; const MsgText: string;
  Animate: Boolean; AlignForm: TCustomForm): TSplashWindow;
begin
  Result := CreateSplashWindow;
  with Result do begin
    if Animate and (Graphic <> nil) then begin
      Animation.Glyph := Graphic as TBitmap;
      Animation.Visible := True;
{$IFDEF RX_D3}
      Animation.AsyncDrawing := True;
{$ENDIF}
      Animation.Active := True;
    end
    else if (Graphic <> nil) then begin
      Image.Picture.Graphic := Graphic;
      Image.Visible := True;
    end
    else begin
      FTextMessage.Left := defImageLeft;
    end;
    FTextMessage.Caption := MsgText;
    MessageText := MsgText;
    CenterFor(AlignForm);
    Show;
    Update;
  end;
end;

procedure TSplashWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_DLGFRAME;
end;

function TSplashWindow.GetMessageText: string;
begin
  Result := FTextMessage.Caption;
end;

procedure TSplashWindow.SetMessageText(const Value: string);
var
  TextRect: TRect;
{$IFNDEF WIN32}
  C: array[0..255] of Char;
{$ENDIF WIN32}
  VertOff: Integer;
begin
  TextRect := Rect(FTextMessage.Left, 0, Max(Screen.Width div 2 - 64,
    defTextWidth), 0);
  DrawText(Canvas.Handle,
    {$IFDEF WIN32} PChar(Value), {$ELSE} StrPCopy(C, Value), {$ENDIF WIN32}
    -1, TextRect, DT_CALCRECT or DT_WORDBREAK);
  VertOff := (ClientHeight div 2) - ((TextRect.Bottom - TextRect.Top) div 2);
  if VertOff < 0 then VertOff := 10;
  TextRect.Top := VertOff;
  TextRect.Bottom := TextRect.Bottom + VertOff;
  FTextMessage.BoundsRect := TextRect;
  ClientWidth := Max(ClientWidth, TextRect.Right + defTextRight);
  ClientHeight := Max(ClientHeight, VertOff * 2);
  if Value <> FTextMessage.Caption then begin
    FTextMessage.Caption := Value;
    Update;
  end;
end;

procedure TSplashWindow.CenterFor(Form: TCustomForm);
var
  NewLeft, NewTop: Integer;
  DstRect: TRect;
begin
  if Form = nil then DstRect := Rect(0, 0, Screen.Width, Screen.Height)
  else DstRect := Form.BoundsRect;
  NewLeft := DstRect.Left + ((DstRect.Right - DstRect.Left) div 2) - (Width div 2);
  NewTop := DstRect.Top + ((DstRect.Bottom - DstRect.Top) div 2) - (Height div 2);
  SetBounds(NewLeft, NewTop, Width, Height);
end;

end.
