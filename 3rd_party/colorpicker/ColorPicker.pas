// ColorBtn ver 1.5  15/01/2000
// by SoftCos (Enzo Costantini)
(* -------------------------------------------------------------------------------
  + Changes by Marek Jedlinski <marek@tranglos.com> (Poland) [mj]
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]
  
   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf     
 --------------------------------------------------------------------------------- *)
{  // [dpv]
  *1:
   When converting the project from Delphi 2006 to Delphi CE 11.3, this control has started to have a strange visual behavior when setting Flat=True (property 
   that in this control should be set in this way, because False does not look good)
   When hovering the cursor on the button (main or auxiliary with the arrow), it was highlighted in blue, but not temporarily while the cursor is over
   (which would be normal). This color was not removed, but 'added', until it was left with an intense blue. It was as to paint with a blue brush on top
   of the bottoms. The same happened with the buttons associated with each individual color, in the grid that is offered when clicking on the auxiliary button with
    the down arrow.

    Both the main buttons and those linked to specific colors are implemented with TSpeedButton controls. The control (TColorBtn) inherits from TCustomPanel, and
    its main buttons (Btn1 and Btn2) are TSpeedButton. The first of these is actually TColBtn, which is nothing more than a derived class that overrides the Paint
    method. These buttons had the Self object (TColorBtn, TCustomPanel) defined as the Parent control.

    In the window with the color grid, these are TColBtn and they also had the control that created them as their parent, another TCustomPanel (TColorPicker).

    I don't quite understand what is happening, because this previously worked perfectly.

    I have verified that the TSpeedButton controls work normally, also when added to a Panel control (I have tried to add a panel to a TToolbarPanel control and to
    the panel a TSpeedButton configured as Flat= True, and the behavior has been normal). The same also happens when the button is added to a panel located anywhere.

    I have done many tests, and the only thing that has worked for me has been to assign as parent of Btn1 and Btn2 buttons the Parent of the control itself
    (Self.Parent). And something similar I have done with the buttons of the grid. I have had to adjust the limits of the buttons accordingly. I have also kept the
    PanelForm itself hidden. In the case of TColorBtn (not TColorPicker) I have needed to offer the initial behavior in case the control is in design mode, or else
    the IDE would give an error (if it did not have the Parent property set), or it would not allow it to be selected (if we keep it a width equal to zero).

  *2:
    Another problem I've run into was that, also with Flat=True, inside the Paint method, and after the call to Inherited, the colors set for the Pen and Brush seemed
    to be ignored: it was painting unfilled rectangles with black borders.
    That happened only after hovering over the TSpeedButton. The first time he painted it did it correctly. The solution in this case has been to call
    Canvas.Refresh after the call to Inherited.
}


unit ColorPicker;

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   Vcl.Buttons;

const
  BtnDim=17;

  COLORS_NUM_ROWS = 7;
  COLORS_NUM_COLS = 11;
  TOP_PREDEF_COLORS = BtnDim + 7;
  TOP_CUSTOM_COLORS = TOP_PREDEF_COLORS + COLORS_NUM_ROWS*BtnDim + BtnDim;
  TOP_SEPARATOR =     TOP_CUSTOM_COLORS - (BtnDim div 2);
  TOP_OTHER_BUTTOM =  TOP_CUSTOM_COLORS + BtnDim + 10;
  MARGIN = 5;

type
  TNumColors = 0..(COLORS_NUM_COLS*COLORS_NUM_ROWS)-1;
  TNumCustomColors = 0..(COLORS_NUM_COLS)-1;

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
    procedure SetParent(AParent: TWinControl); override;                         // [dpv]  *1
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
    ColBtns: array[TNumColors] of TColBtn;
    CustColBtns: array[TNumCustomColors] of TColBtn;
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
    CustColorsModified: boolean;
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
  protected
    Btn1:TColBtn;
    Btn2:TSpeedButton;
    procedure Loaded; override;                                    // [dpv]
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
    property Hint;
    property ShowHint;
    property Visible;
    property ParentShowHint;
    property PopupMenu;
  end;

procedure Register;

implementation

uses
   System.Win.Registry,
   gf_miscvcl;

resourcestring
  STR_01 = 'Right-click to set custom colors';

{$R *.Res}
const
     AutoOffSet=BtnDim+2;
     BtnColors:array[TNumColors] of TColor
{
     =($1FFFFFFF,$808080,$000040,$004040,$004000,$404000,$400000,$400040,
         $000000,$909090,$000080,$008080,$008000,$808000,$800000,$800080,
         $202020,$B0B0B0,$0000FF,$00FFFF,$00FF00,$FFFF00,$FF0000,$FF00FF,
         $404040,$D0D0D0,$8080FF,$80FFFF,$80FF80,$FFFF80,$FF8080,$FF80FF,
         $606060,$FFFFFF,$C0C0FF,$C0FFFF,$C0FFC0,$FFFFC0,$FFC0C0,$FFC0FF);
}
       =(
        $000000,$400000,$402000,$404000,$204000,$004000,$004040,$002040,$000040,$200040,$400040,
        $303030,$800000,$804000,$808000,$408000,$008000,$008080,$004080,$000080,$400080,$800080,
        $606060,$CD0000,$CD6600,$CDCD00,$66CD00,$00CD00,$00CDCD,$0066CD,$0000CD,$6600CD,$CD00CD,
        $808080,$FF0000,$FF8000,$FFFF00,$80FF00,$00FF00,$00FFFF,$0080FF,$0000FF,$7F00FF,$FF00FF,
        $C0C0C0,$FF4D4D,$FFA64D,$FFFF4D,$A6FF4D,$4DFF4D,$4DFFFF,$4DA6FF,$4D4DFF,$A64DFF,$FF4DFF,
        $DEDEDE,$FF8080,$FFC080,$FFFF80,$C0FF80,$80FF80,$80FFFF,$80C0FF,$8080FF,$C080FF,$FF80FF,
        $FFFFFF,$FFB3B3,$FFD8B3,$FFFFB3,$DAFFB3,$B3FFB3,$B3FFFF,$B3DAFF,$B3B3FF,$DAB3FF,$FFB3FF
       );

{
   // Created with the help of https://www.rapidtables.com/web/color/RGB_Color.html

       =(
V
20
25        000000,400000,402000,404000,204000,004000,004040,002040,000040,200040,400040,
50        333333,800000,804000,808000,408000,008000,008080,004080,000080,400080,800080,
75        666666,C00000,C06000,C0C000,60C000,00C000,00C0C0,0060C0,0000C0,6000C0,C000C0,
80        6A6A6A,CD0000,CD6600,CDCD00,66CD00,00CD00,00CDCD,0066CD,0000CD,6600CD,CD00CD,
85        777777,DA0000,DA6D00,DADA00,6DDA00,00DA00,00DADA,006DDA,0000DA,6D00DA,DA00DA,
S
100       808080,FF0000,FF8000,FFFF00,80FF00,00FF00,00FFFF,0080FF,0000FF,7F00FF,FF00FF,
80        A0A0A0,FF3333,FF9933,FFFF33,99FF33,33FF33,33FFFF,3399FF,3333FF,9933FF,FF33FF,
70        B0B0B0,FF4D4D,FFA64D,FFFF4D,A6FF4D,4DFF4D,4DFFFF,4DA6FF,4D4DFF,A64DFF,FF4DFF,
60		    C0C0C0,FF6666,FFB266,FFFF66,B2FF66,66FF66,66FFFF,66B2FF,6666FF,B266FF,FF66FF,
50        D0D0D0,FF8080,FFC080,FFFF80,C0FF80,80FF80,80FFFF,80C0FF,8080FF,C080FF,FF80FF,
40
35        DADADA,FFA6A6,FFD3A6,FFFFA6,D3FFA6,A6FFA6,A6FFFF,A6D3FF,A6A6FF,D3A6FF,FFA6FF,
30		    FFFFFF,FFB3B3,FFD8B3,FFFFB3,DAFFB3,B3FFB3,B3FFFF,B3DAFF,B3B3FF,DAB3FF,FFB3FF
20
         );

}

var
     CustBtnColors:array[TNumCustomColors] of TColor;

procedure Register;
begin
  RegisterComponents('SoftCos', [TColorBtn]);
end;

procedure TColBtn.Paint;
var B,X,Y:integer;
    FColor:TColor;
    PaintRect: TRect;
    P: TPoint;
    FMouseInControl: boolean;

begin

     if Enabled
     then FColor:=Color
     else FColor:=clGray;
     //B:=Height div 5;
     B:= 1;
     Inherited;

     Canvas.Refresh;                              // [dpv] *2

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
         GetCursorPos(P);
         FMouseInControl := (FindDragTarget(P, True) = Self);
         if FMouseInControl then
            Brush.Color := clBlack
         else
            Brush.Color := clBtnFace;
         PaintRect := ClientRect;
         FillRect(PaintRect);

         Pen.color:=clgray;
         Brush.Color:=FColor;
         Brush.Style:=bsSolid;
         Rectangle(B,B,Width-B,Height-B);
        end
     else
        begin
         B:= 2;
         Pen.color:=clgray;
         Brush.Style:=bsClear;
         Polygon([Point(B-1,B-1),
                  Point(Width-(B-1),B-1),
                  Point(Width-(B-1),Height-(B-1)),
                  Point(B-1,Height-(B-1))]);
         Pen.color:=clgray;
         Brush.Color:=FColor;
         Brush.Style:=bsSolid;
         Rectangle(B+1,B+1,Height + 3,Height-B);
        end;
end;

constructor TColorPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := (COLORS_NUM_COLS * BtnDim) + 5*2;
  Height :=TOP_OTHER_BUTTOM + BtnDim + 5;
  FColorDlg:=TColorDialog.Create(self);
  FColorDlg.Options:=[cdFullOpen];
  FSeparator := TBevel.Create(self);
  //FSeparator.Parent:=self;                          // [dpv]
  FSeparator.SetBounds(5,TOP_SEPARATOR,width-10,2);
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
  //Btn.Parent := Self;	                            // [dpv]
  Btn.Flat:=true;
  Btn.Tag:=100;
  Btn.Color:=ClDefault;
  Btn.GroupIndex:=1;
  Btn.SetBounds(MARGIN,4,Width-MARGIN*2,BtnDim);
  Btn.Hint:= ColorInformation(Btn.Color);
  Btn.OnClick:=BtnClick;
  AutoBtn:=Btn;

  for I := 0 to High(TNumColors) do
  begin
    Btn := TColBtn.Create (Self);
    //Btn.Parent := Self;                           // [dpv]
    Btn.Flat:=true;
    Btn.Color:=BtnColors[I];
    Btn.GroupIndex:=1;
    Btn.OnClick:=BtnClick;
    Btn.Hint:= ColorInformation(BtnColors[I]);
    X := MARGIN + (I  mod COLORS_NUM_COLS ) * BtnDim;
    Y := TOP_PREDEF_COLORS + BtnDim*(I div COLORS_NUM_COLS);
    Btn.SetBounds (X, Y , BtnDim,BtnDim);
    ColBtns[I] := Btn;
  end;

  for I := 0 to High(TNumCustomColors) do
  begin
    Btn := TColBtn.Create (Self);
    //Btn.Parent := Self;                            // [dpv]
    Btn.Flat:=true;
    Btn.Color:=CustBtnColors[I];
    Btn.GroupIndex:=1;
    Btn.Tag:=I;
    Btn.OnClick:=BtnClick;
    if CustBtnColors[I]= $FFFFFF then
       Btn.Hint:= STR_01
    else
       Btn.Hint:= ColorInformation(CustBtnColors[I]);
    Btn.OnMouseDown:=BtnRClick;
    X := MARGIN + (I mod COLORS_NUM_COLS ) * BtnDim;
    Y := TOP_CUSTOM_COLORS + BtnDim * (I div COLORS_NUM_COLS);
    Btn.SetBounds (X, Y , BtnDim,BtnDim);
    CustColBtns[I] := Btn;
  end;
  Btn:=TColBtn.Create(Self);
  //Btn.Parent := Self;                               // [dpv]
  Btn.Flat:=true;
  Btn.Color:=FColorDlg.Color;
  Btn.SetBounds(MARGIN,TOP_OTHER_BUTTOM,BtnDim,BtnDim);
  Btn.GroupIndex:=1;
  Btn.OnClick:=BtnClick;
  Btn.Hint:= ColorInformation(Btn.Color);
  OtherColBtn:=Btn;

  ABtn:=TSpeedButton.Create(Self);
  //ABtn.Parent := Self;                              // [dpv]
  ABtn.Flat:=true;
  ABtn.SetBounds(MARGIN+BtnDim+4,TOP_OTHER_BUTTOM,Width-MARGIN*2-4-BtnDim,BtnDim);
  OtherBtn:=ABtn;
  OtherBtn.OnClick:=OtherBtnClick;
end;

procedure TColorPicker.SetParent(AParent: TWinControl);    // [dpv]
var I : integer;
begin
     //inherited;        // Commented: The panel will not hide the buttons, to which we have assigned as parent the own parent of TColorPicker (panel)

     AutoBtn.Parent:= AParent;
     for I:=0 to High(TNumColors) do
       ColBtns[I].Parent:= AParent;

     FSeparator.Parent:= AParent;

     for I:=0 to High(TNumCustomColors) do
       CustColBtns[I].Parent:= AParent;

     OtherColBtn.Parent:= AParent;
     OtherBtn.Parent:= AParent;
end;


procedure TColorPicker.UpdateButtons;
var I : integer;
begin
     Height:=Height-AutoOffSet;
     for I:=0 to High(TNumColors)
     do ColBtns[I].Top:=ColBtns[I].Top-AutoOffSet;
     FSeparator.Top:=FSeparator.Top-AutoOffSet;
     for I:=0 to High(TNumCustomColors)
     do CustColBtns[I].Top:=CustColBtns[I].Top-AutoOffSet;
     OtherColBtn.Top:=OtherColBtn.Top-AutoOffSet;
     OtherBtn.Top:=OtherBtn.Top-AutoOffSet;
end;


procedure TColorPicker.OtherBtnClick(Sender:TObject);
var
   ExecuteOk: boolean;
begin
     FColorDlg.Color:=OtherColBtn.Color;
     TColPickDlg(Owner).OtherOk:=true;
     ExecuteOk:= FColorDlg.Execute;
     if ExecuteOk then
        OtherColBtn.Color:=FColorDlg.Color;

     TColPickDlg(Owner).OtherOk:=false;
     SendMessage(TColPickDlg(Owner).Handle,WM_SETFOCUS,0,0);

     if ExecuteOk then                     // [dpv]
        BtnClick(OtherColBtn);
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
                   TColPickDlg(Owner).CustColorsModified:= true;             // [dpv]
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
        for i:=0 to High(TNumColors) do ColBtns[i].Flat:=Value;
        for i:=0 to High(TNumCustomColors) do CustColBtns[i].Flat:=Value;
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
     CustColorsModified:= false;                       // [dpv]
     ok:=false;
     for i:=0 to High(TNumColors) do
     begin
          if BtnColors[i]=SelectedColor
          then
              begin
                ColPick.ColBtns[i].down:=true;
                Ok:=true;
              end;
     end;
     if not Ok then
     for i:=0 to High(TNumCustomColors) do
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

     if CustColorsModified then           // [dpv]
        TColorBtn(SendCtrl).WriteReg;

     if CloseOk then
          with TColorBtn(SendCtrl) do begin
            Btn1.Color:=SelectedColor;
            FActiveColor:=SelectedColor;
            FTargetColor:=SelectedColor;
            AutoClicked:=ColPick.AutoClicked;
            //WriteReg;                              // [dpv]
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
  //Btn1.Parent := Self;                                  // [dpv]
  Btn1.Color:=FActiveColor;
  Btn1.OnClick:=Btn1Click;
  Btn1.Glyph.Handle:= LoadBitmap(HInstance,'FRCOLOR');

  Btn2:=TSpeedButton.Create(Self);
  //Btn2.Parent := Self;                                  // [dpv]
  Btn2.Glyph.Handle:= LoadBitmap(HInstance,'DROPDOWN');
  Btn2.OnClick:=Btn2Click;

   if (csDesigning in ComponentState) then begin          // [dpv]
      Btn1.Parent := Self;
      Btn2.Parent := Self;
   end;

end;


procedure TColorBtn.Loaded;                              // [dpv]
begin
   inherited Loaded;

   if not (csDesigning in ComponentState) then begin
      Btn1.Hint:= Hint;
      Btn2.Hint:= Hint;
   end;
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
     for I:=0 to High(TNumCustomColors) do
     try
     CustBtnColors[I]:=StrToInt('$'+ReadString('Color'+Char(65+I)));
     except
           CustBtnColors[I]:=$FFFFFF;
     end

     else
     for I:=0 to High(TNumCustomColors) do
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
     for I:=0 to High(TNumCustomColors) do
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
{                                                                   // [dpv]  *1
     P.X:=TControl(Sender).Left-TControl(Sender).Parent.height;
     P.Y:=TControl(Sender).Top+TControl(Sender).Parent.height;
     P:=ClientToScreen(P);
}
     P.X:= 0;                                                       // [dpv]
     P.Y:= TControl(Sender).height;
     P:= Btn1.ClientToScreen(P);
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
          //ColPick.OtherBtn.Hint:= STR_01;  // [dpv]
          ShowHint:= true;
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
var
  L: Integer;
begin
   if (csLoading in ComponentState) then Exit;
   if Btn1 = nil then Exit;
   W:= H + FDDArrowWidth;
// Btn1.SetBounds(0,0,H,H);                                     // [dpv]    
// Btn2.SetBounds(H,0,FDDArrowWidth,H);

   L:= 0;
   if not (csDesigning in ComponentState) then begin            // [dpv]
      L:= Left;
      if Btn1.Parent = nil then begin
         Btn1.Parent:= Self.Parent;
         Btn2.Parent:= Self.Parent;
      end;
   end;
   Btn1.SetBounds(L,0,H,H);
   Btn2.SetBounds(L+H,0,FDDArrowWidth,H);
end;

procedure TColorBtn.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);

  if not (csDesigning in ComponentState) then  W:= 0;          // [dpv] -> Panel will not hide btn1 nor btn2
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

//if (W <> Width) or (H <> Height) then                                         // [dpv]
  if (csDesigning in ComponentState) and ((W <> Width) or (H <> Height)) then
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

end.

