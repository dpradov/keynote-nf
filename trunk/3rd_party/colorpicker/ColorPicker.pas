// ColorBtn ver 1.5  15/01/2000
// by SoftCos (Enzo Costantini)

unit ColorPicker;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons;


type

  TColBtn = class(TSpeedButton)
  protected
    procedure paint; override;
  public
    property Canvas;
  published
    property Color;
  end;

  TColorPicker = class(TCustomPanel)
    FColorDlg:TColorDialog;
    FSeparator : TBevel;
  private
    FDDFlat : Boolean;
    FDDAutoColor : TColor;
    FDDIsAuto : Boolean;
    FAutoClicked : Boolean;
    procedure InitButtons;
    procedure UpdateButtons;
    procedure OtherBtnClick(Sender:TObject);
    procedure BtnClick(Sender:TObject);
    procedure BtnRClick(Sender: TObject;
                        Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetDDAutoColor(Value:TColor);
    procedure SetDDIsAuto(Value:boolean);
    procedure SetDDFlat(Value:boolean);
  public
    AutoBtn,
    OtherColBtn:TColBtn;
    ColBtns: array[0..39] of TColBtn;
    CustColBtns: array[0..15] of TColBtn;
    OtherBtn:TSpeedButton;
    DDSelColor : TColor;
    constructor Create(AOwner: TComponent); override;
    property DDAutoColor : TColor read FDDAutoColor write SetDDAutoColor;
    property DDIsAuto : boolean read FDDIsAuto write SetDDIsAuto;
    property AutoClicked:boolean read FAutoClicked default false;
    property DDFlat:Boolean read FDDFlat write SetDDFlat;
  end;

  TColPickDlg = class(TForm)
    ColPick: TColorPicker;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    SendCtrl:TControl;
    CloseOk:boolean;
    OtherOk:boolean;
    procedure WMKILLFOCUS(var message: TWMKILLFOCUS); message WM_KILLFOCUS;
  public
    SelectedColor:TColor;
    procedure Drop(Sender:TControl);
  end;


  TGlyphType = (gtForeground,gtBackground,gtLines,gtCustom);

  TColorBtn = class(TCustomPanel)
  private
    FFlat,
    FDropDownFlat : Boolean;
    FDDArrowWidth : byte;
    FActiveColor : TColor;
    FAutomaticColor : TColor;
    FTargetColor : TColor;
    FGlyphType : TGlyphType;
    FIsAutomatic : Boolean;
    FOnBtnClick,
    FBeforeDropDown : TNotifyEvent;
    FAutoBtnCaption,
    FOtherBtnCaption:TCaption;
    FRegKey : string;
    procedure InitButtons;
    procedure Btn1Click(Sender:TObject);
    procedure Btn2Click(Sender:TObject);
    procedure SetFlat(Value:boolean);
    procedure SetActiveColor(Value:TColor);
    procedure SetGlyphType(Value:TGlyphType);
    procedure SetGlyph(Value:TBitMap);
    procedure SetDDArrowWidth(Value:byte);
    function GetGlyph:TBitMap;
    procedure SetRegKey(Value:string);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
  protected
    Btn1:TColBtn;
    Btn2:TSpeedButton;
    procedure SetEnabled(Value:boolean);
    procedure WriteReg;
    procedure ReadReg;
  public
    AutoClicked:boolean;
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AdjustSize (var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  published
    property Align;
    property ActiveColor : TColor read FActiveColor write SetActiveColor;
    property TargetColor : TColor read FTargetColor write FTargetColor;
    property Flat:Boolean read FFlat write SetFlat;
    property DropDownFlat:Boolean read FDropDownFlat write FDropDownFlat;
    property AutomaticColor : TColor read FAutomaticColor write FAutomaticColor;
    property IsAutomatic : boolean read FIsAutomatic write FIsAutomatic;
    property OnClick : TNotifyEvent read FOnBtnClick write FOnBtnClick;
    property BeforeDropDown : TNotifyEvent read FBeforeDropDown write FBeforeDropDown;
    property GlyphType : TGlyphType read FGlyphType write SetGlyphType default gtForeground;
    property Glyph : TBitMap read GetGlyph write SetGlyph;
    property AutoBtnCaption : TCaption read FAutoBtnCaption write FAutoBtnCaption;
    property OtherBtnCaption:TCaption read FOtherBtnCaption write FOtherBtnCaption;
    property RegKey : string read FRegKey write SetRegKey;
    property DDArrowWidth : byte read FDDArrowWidth write SetDDArrowWidth;
    property Enabled;
    //property Hint;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
    property ShowHint;
    property Visible;
    property ParentShowHint;
    property PopupMenu;
  end;

procedure Register;

implementation
uses Registry, TntControls;
{$R *.Res}
const
     BtnDim=20;
     AutoOffSet=BtnDim+2;
     BtnColors:array[0..39] of TColor
     =($1FFFFFFF,$808080,$000040,$004040,$004000,$404000,$400000,$400040,
         $000000,$909090,$000080,$008080,$008000,$808000,$800000,$800080,
         $202020,$B0B0B0,$0000FF,$00FFFF,$00FF00,$FFFF00,$FF0000,$FF00FF,
         $404040,$D0D0D0,$8080FF,$80FFFF,$80FF80,$FFFF80,$FF8080,$FF80FF,
         $606060,$FFFFFF,$C0C0FF,$C0FFFF,$C0FFC0,$FFFFC0,$FFC0C0,$FFC0FF);
var
     CustBtnColors:array[0..15] of TColor;

procedure Register;
begin
  RegisterComponents('SoftCos', [TColorBtn]);
end;

procedure TColBtn.Paint;
var B,X,Y:integer;
    FColor:TColor;
begin

     if Enabled
     then FColor:=Color
     else FColor:=clGray;
     B:=Height div 5;
     Inherited;
     with Canvas do
     if Glyph.Handle<>0
     then
        begin
         X:=(Width div 2) - 9 + Integer(FState in [bsDown]);
         Y:=(Height div 2)+ 4 + Integer(FState in [bsDown]);
         Pen.color:=FColor;
         Brush.Color:=FColor;
         Rectangle(X,Y,X+17,Y+4);
        end
     else
     if Caption=''
     then
        begin
         Pen.color:=clgray;
         Brush.Color:=FColor;
         Brush.Style:=bsSolid;
         Rectangle(B,B,Width-B,Height-B);
        end
     else
        begin
         Pen.color:=clgray;
         Brush.Style:=bsClear;
         Polygon([Point(B-1,B-1),
                  Point(Width-(B-1),B-1),
                  Point(Width-(B-1),Height-(B-1)),
                  Point(B-1,Height-(B-1))]);
         Pen.color:=clgray;
         Brush.Color:=FColor;
         Brush.Style:=bsSolid;
         Rectangle(B+1,B+1,Height,Height-B);
        end;
end;

constructor TColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := 170;
  Height :=210;
  FColorDlg:=TColorDialog.Create(self);
  FColorDlg.Options:=[cdFullOpen];
  FSeparator := TBevel.Create(self);
  FSeparator.Parent:=self;
  FSeparator.SetBounds(5,135,width-10,2);
  InitButtons;
  FDDIsAuto:=true;
  FDDFlat:=true;
end;

procedure TColorPicker.InitButtons;
var
  I : integer;
  Btn: TColBtn;
  ABtn:TSpeedButton;
  X,Y: Integer;

begin
  Btn:=TColBtn.Create(Self);
  Btn.Parent := Self;
  Btn.Flat:=true;
  Btn.Tag:=100;
  Btn.Color:=ClDefault;
  Btn.GroupIndex:=1;
  Btn.SetBounds(5,4,Width-10,BtnDim);
  Btn.OnClick:=BtnClick;
  AutoBtn:=Btn;

  for I := 0 to 39 do
  begin
    Btn := TColBtn.Create (Self);
    Btn.Parent := Self;
    Btn.Flat:=true;
    Btn.Color:=BtnColors[I];
    Btn.GroupIndex:=1;
    Btn.OnClick:=BtnClick;
    X := 5 + (I  mod 8 ) * BtnDim;
    Y := BtnDim + 10 + BtnDim*(I div 8);
    Btn.SetBounds (X, Y , BtnDim,BtnDim);
    ColBtns[I] := Btn;
  end;

  for I := 0 to 15 do
  begin
    Btn := TColBtn.Create (Self);
    Btn.Parent := Self;
    Btn.Flat:=true;
    Btn.Color:=CustBtnColors[I];
    Btn.GroupIndex:=1;
    Btn.Tag:=I;
    Btn.OnClick:=BtnClick;
    Btn.OnMouseDown:=BtnRClick;
    X := 5 + (I mod 8 ) * BtnDim;
    Y := BtnDim + 120 + BtnDim * (I div 8);
    Btn.SetBounds (X, Y , BtnDim,BtnDim);
    CustColBtns[I] := Btn;
  end;
  Btn:=TColBtn.Create(Self);
  Btn.Parent := Self;
  Btn.Flat:=true;
  Btn.Color:=FColorDlg.Color;
  Btn.SetBounds(5,BtnDim*8+25,BtnDim,BtnDim);
  Btn.GroupIndex:=1;
  Btn.OnClick:=BtnClick;
  OtherColBtn:=Btn;

  ABtn:=TSpeedButton.Create(Self);
  ABtn.Parent := Self;
  ABtn.Flat:=true;
  ABtn.SetBounds(5+BtnDim,BtnDim*8+25,Width-10-BtnDim,BtnDim);
  OtherBtn:=ABtn;
  OtherBtn.OnClick:=OtherBtnClick;
end;

procedure TColorPicker.UpdateButtons;
var I : integer;
begin
     Height:=Height-AutoOffSet;
     for I:=0 to 39
     do ColBtns[I].Top:=ColBtns[I].Top-AutoOffSet;
     FSeparator.Top:=FSeparator.Top-AutoOffSet;
     for I:=0 to 15
     do CustColBtns[I].Top:=CustColBtns[I].Top-AutoOffSet;
     OtherColBtn.Top:=OtherColBtn.Top-AutoOffSet;
     OtherBtn.Top:=OtherBtn.Top-AutoOffSet;
end;


procedure TColorPicker.OtherBtnClick(Sender:TObject);
begin
     FColorDlg.Color:=OtherColBtn.Color;
     TColPickDlg(Owner).OtherOk:=true;
     if FColorDlg.Execute
     then OtherColBtn.Color:=FColorDlg.Color;
     TColPickDlg(Owner).OtherOk:=false;
     SendMessage(TColPickDlg(Owner).Handle,WM_SETFOCUS,0,0);
end;

procedure TColorPicker.BtnClick(Sender:TObject);

begin
     if TControl(Sender).Tag=100
     then FAutoClicked:=true
     else FAutoClicked:=false;
     DDSelColor:=TColBtn(Sender).Color;
     SendMessage(TwinControl(Owner).handle,WM_KeyDown,vk_return,0);

end;

procedure TColorPicker.BtnRClick(Sender: TObject;
                       Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if Button= mbRight
     then
         begin
              FColorDlg.Color:=TColorBtn(Sender).Color;
              TColPickDlg(Owner).OtherOk:=true;
              if FColorDlg.Execute
              then
              begin
                   TColorBtn(Sender).Color:=FColorDlg.Color;
                   CustBtnColors[TColorBtn(Sender).Tag]:=FColorDlg.Color;
              end;
              TColPickDlg(Owner).OtherOk:=false;
              SendMessage(TColPickDlg(Owner).Handle,WM_SETFOCUS,0,0);
         end;
end;

procedure TColorPicker.SetDDAutoColor(Value:TColor);
begin
     if Value<>FDDAutoColor
     then
     begin
          FDDAutoColor:=Value;
          AutoBtn.Color:=Value;
     end;
end;
procedure TColorPicker.SetDDIsAuto(Value:boolean);
begin
     if Value<>FDDIsAuto
     then
     begin
          FDDIsAuto:=Value;
          AutoBtn.visible:=Value;
     end;
     if not FDDIsAuto
     then UpdateButtons;
end;

procedure TColorPicker.SetDDFlat(Value:boolean);
var i:integer;
begin
     if Value<>FDDFlat
     then
     try
        FDDFlat:=Value;
        for i:=0 to 39 do ColBtns[i].Flat:=Value;
        for i:=0 to 15 do CustColBtns[i].Flat:=Value;
        AutoBtn.Flat:=Value;
        OtherBtn.Flat:=Value;
        OtherColBtn.Flat:=Value;
     except
     end;
end;


{TColPickDlg}


procedure TColPickDlg.Drop(Sender:TControl);
begin
     SendCtrl:=Sender;
     Show;
end;

procedure TColPickDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     if Key=vk_escape then Close;
     if Key=vk_return then
                         begin
                           SelectedColor:=ColPick.DDSelColor;
                           CloseOk:=true;
                           Close;
                         end;
     Key:=0;
end;

procedure TColPickDlg.FormShow(Sender: TObject);
var i:integer;
    ok:boolean;
begin
     CloseOk:=false;
     ok:=false;
     for i:=0 to 39 do
     begin
          if BtnColors[i]=SelectedColor
          then
              begin
                ColPick.ColBtns[i].down:=true;
                Ok:=true;
              end;
     end;
     if not Ok then
     for i:=0 to 15 do
     begin
          if CustBtnColors[i]=SelectedColor
          then
              begin
                ColPick.CustColBtns[i].down:=true;
                Ok:=true;
              end;
     end;
     if not Ok
     then
        begin
             ColPick.OtherColBtn.Color:=SelectedColor;
             ColPick.OtherColBtn.Down:=true;
        end;
end;

procedure TColPickDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     if CloseOk
          then
          with TColorBtn(SendCtrl) do
          begin
            Btn1.Color:=SelectedColor;
            FActiveColor:=SelectedColor;
            FTargetColor:=SelectedColor;
            AutoClicked:=ColPick.AutoClicked;
            WriteReg;
            Btn1Click(Sender);
          end;
     Action:=caFree;
end;

procedure TColPickDlg.WMKILLFOCUS(var message: TWMKILLFOCUS);
begin
     if not OtherOk then Self.Close;
end;

{TColorBtn}

constructor TColorBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Height :=22;
  BevelOuter:=bvNone;
  InitButtons;
  FFlat:=false;
  FDropDownFlat:=true;
  FDDArrowWidth:=12;
  FIsAutomatic:=True;
  FActiveColor:=clBlack;
  Btn1.NumGlyphs:=2;
  FAutoBtnCaption:='Automatic';
  FOtherBtnCaption:='&Other colors...';
  FRegKey:='SoftCos';
end;

procedure TColorBtn.InitButtons;

begin
  Btn1:=TColBtn.Create(Self);
  Btn1.Parent := Self;
  Btn1.Color:=FActiveColor;
  Btn1.OnClick:=Btn1Click;
  Btn1.Glyph.Handle:= LoadBitmap(HInstance,'FRCOLOR');
  Btn2:=TSpeedButton.Create(Self);
  Btn2.Parent := Self;
  Btn2.Glyph.Handle:= LoadBitmap(HInstance,'DROPDOWN');
  Btn2.OnClick:=Btn2Click;
end;

procedure TColorBtn.ReadReg;
var ECIni:TRegistry;
    I : Integer;
begin
     ECIni := TRegistry.Create;
     try
     with ECIni do
     if OpenKey('SoftWare',false) and
        OpenKey(FRegKey,false)
     then
     for I:=0 to 15 do
     try
     CustBtnColors[I]:=StrToInt('$'+ReadString('Color'+Char(65+I)));
     except
           CustBtnColors[I]:=$FFFFFF;
     end

     else
     for I:=0 to 15 do
     CustBtnColors[I]:=$FFFFFF;
     finally
     ECIni.Free;
     end;
end;

procedure TColorBtn.WriteReg;
var ECIni:TRegistry;
    I:integer;
begin
     ECIni := TRegistry.Create;
     try
     with ECIni do
     if OpenKey('SoftWare',true) and
        OpenKey(FRegKey,true)
     then
     for I:=0 to 15 do
     WriteString('Color'+Char(65+I),IntToHex(CustBtnColors[I],6));
     finally ECIni.Free; end;
end;

procedure TColorBtn.Btn1Click(Sender:TObject);
begin
     if not (csDesigning in ComponentState) and Assigned(FOnBtnClick) then
     FOnBtnClick(Self);
end;

procedure TColorBtn.Btn2Click(Sender:TObject);
var P:TPoint;
    Dlg:TColPickDlg;
begin
     if not (csDesigning in ComponentState) and
             Assigned(FBeforeDropDown)
     then FBeforeDropDown(Self);
     if not (csDesigning in ComponentState)
     then ReadReg;
     P.X:=TControl(Sender).Left-TControl(Sender).Parent.height;
     P.Y:=TControl(Sender).Top+TControl(Sender).Parent.height;
     P:=ClientToScreen(P);
     Dlg:= TColPickDlg.CreateNew(Application);

     with Dlg do
     begin
          BorderIcons := [];
          BorderStyle := bsNone;
          ColPick:=TColorPicker.Create(Dlg);
          ColPick.Parent := Dlg;
          ColPick.DDAutoColor:=FAutomaticColor;
          ColPick.DDIsAuto:=FIsAutomatic;
          ColPick.DDFlat:=FDropDownFlat;
          ColPick.AutoBtn.Caption:=FAutoBtnCaption;
          ColPick.OtherBtn.Caption:=FOtherBtnCaption;
          ColPick.Left := 0;
          ColPick.Top := 0;
          ClientHeight:=ColPick.Height;
          ClientWidth:=ColPick.Width;
          OnKeyDown:= FormKeyDown;
          OnShow:=FormShow;
          OnClose:=FormClose;
          SelectedColor:=TargetColor;
          Left:=P.X;                      // [dpv] Movido desde el ppio del bloque begin
          Top:=P.Y;                       // [dpv]  Idem
          Drop(TColorBtn(self));
     end;
end;

procedure TColorBtn.SetFlat(value:boolean);
begin
     if Value<>FFlat
     then
     begin
        FFlat:=Value;
        Btn1.Flat:=Value;
        Btn2.Flat:=Value;
     end;
end;

procedure TColorBtn.AdjustSize (var W: Integer; var H: Integer);
begin
  if (csLoading in ComponentState) then Exit;
  if Btn1 = nil then Exit;
  W:=H+FDDArrowWidth;
  Btn1.SetBounds(0,0,H,H);
  Btn2.SetBounds(H,0,FDDArrowWidth,H);
end;

procedure TColorBtn.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
 inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TColorBtn.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  message.Result := 0;
end;

procedure TColorBtn.SetActiveColor(Value:TColor);
begin
     if Value<>FActiveColor
     then
         begin
              FActiveColor:=Value;
              Btn1.Color:=Value;
         end;
end;

procedure TColorBtn.SetGlyphType(Value:TGlyphType);
begin
     if Value<>FGlyphType
     then
       begin
            FGlyphType:=Value;
            case FGlyphType of
            gtForeground:
            Btn1.Glyph.Handle:=LoadBitmap(HInstance,'FRCOLOR');
            gtBackground:
            Btn1.Glyph.Handle:=LoadBitmap(HInstance,'BKCOLOR');
            gtLines:
            Btn1.Glyph.Handle:=LoadBitmap(HInstance,'LNCOLOR');
            gtCustom:
            Btn1.Glyph:=nil;
            end;
            if Btn1.Glyph<>nil then Btn1.NumGlyphs:=2;
            Btn1.Invalidate;
       end;
end;

procedure TColorBtn.SetGlyph(Value:TBitMap);
begin
     FGlyphType:=gtCustom;
     Btn1.Glyph:=Value;
     Btn1.NumGlyphs:=Value.Width div Value.Height;
     Btn1.Invalidate;
end;

function TColorBtn.GetGlyph:TBitMap;
begin
     result:= Btn1.Glyph;
end;

procedure TColorBtn.SetRegKey(Value:string);
begin
     if Value<>FRegKey
     then
     begin
          if Value=''
          then FRegKey:='SoftCos'
          else FRegKey:=Value;
     end;
end;

procedure TColorBtn.SetDDArrowWidth(Value:byte);
var
  Diff: byte;
begin
     if Value < 7 then Value := 7;
     if Value <> FDDArrowWidth then
     begin
          Diff := Value - FDDArrowWidth;
          FDDArrowWidth:=Value;
          if not(csLoading in ComponentState) then
          Width := Width + Diff;
          Invalidate;
     end;
end;


procedure TColorBtn.SetEnabled(Value:boolean);
begin
     Btn1.Enabled:=Value;
     Btn2.Enabled:=Value;
     // inherited;
end;

function TColorBtn.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TColorBtn.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TColorBtn.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;
end.

