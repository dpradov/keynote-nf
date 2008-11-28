unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ColorPicker, StdCtrls, Buttons, Spin;

type
  TTestDlg = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Shape1: TShape;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    Bevel1: TBevel;
    ColorBtn1: TColorBtn;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure ChangePar(Sender: TObject);
    procedure ColorBtn1BeforeDropDown(Sender: TObject);
    procedure ColorBtn1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestDlg: TTestDlg;

implementation

{$R *.DFM}

procedure TTestDlg.FormShow(Sender: TObject);
begin
     with ColorBtn1 do
     begin
          Edit1.Text:=AutoBtnCaption;
          Edit2.Text:=OtherBtnCaption;
          Checkbox1.Checked:=True;
          Checkbox2.Checked:=false;
          Checkbox3.Checked:=true;
          RadioGroup1.ItemIndex:=0;
          SpinEdit1.Value:=DDArrowWidth;
     end;
end;

procedure TTestDlg.BitBtn1Click(Sender: TObject);
begin
     close;
end;

procedure TTestDlg.ChangePar(Sender: TObject);
begin
     with ColorBtn1 do
     begin
          RegKey:=RadioGroup1.Items[RadioGroup1.ItemIndex];
          Glyphtype:=TGlyphType(RadioGroup1.ItemIndex);
          DropDownFlat:=Checkbox1.Checked;
          Flat:=Checkbox2.Checked;
          IsAutomatic:=Checkbox3.Checked;
          AutoBtnCaption:=Edit1.Text;
          OtherBtnCaption:=Edit2.Text;
     end;
end;

procedure TTestDlg.ColorBtn1BeforeDropDown(Sender: TObject);
begin
     ChangePar(nil);
     with ColorBtn1 do
     case GlyphType of
         gtforeground:begin
                         TargetColor:=Label1.Font.Color;
                         AutomaticColor:=clBlack;
                      end;
         gtbackground:begin
                          TargetColor:=Shape1.Brush.Color;
                          AutomaticColor:=clWhite;
                      end;
         gtlines:     begin
                          TargetColor:=Shape1.pen.Color;
                          AutomaticColor:=clnavy;
                      end;
     end;
end;

procedure TTestDlg.ColorBtn1Click(Sender: TObject);
begin
     case ColorBtn1.GlyphType of
         gtforeground:Label1.Font.Color:=ColorBtn1.ActiveColor;
         gtbackground:Shape1.Brush.Color:=ColorBtn1.ActiveColor;
         gtlines:Shape1.pen.Color:=ColorBtn1.ActiveColor;
     end;
end;

procedure TTestDlg.SpinEdit1Change(Sender: TObject);
begin
     ColorBtn1.DDArrowWidth:=SpinEdit1.Value;
end;

end.
