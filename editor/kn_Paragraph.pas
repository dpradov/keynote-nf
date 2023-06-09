unit kn_Paragraph;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.RichEdit,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   Vcl.Samples.Spin,
   RxRichEd,
   RxPlacemnt,
   kn_Info,
   kn_NoteObj;


type
  TForm_Para = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    FormPlacement: TFormPlacement;
    GroupBox1: TGroupBox;
    Combo_Spc: TComboBox;
    Spin_SpcBef: TSpinEdit;
    Spin_SpcAft: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RB_Align: TRadioGroup;
    GroupBox2: TGroupBox;
    Spin_First: TSpinEdit;
    Spin_Left: TSpinEdit;
    Spin_Right: TSpinEdit;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    GroupBox3: TGroupBox;
    CB_Bullets: TCheckBox;
    CB_Numbers: TCheckBox;
    procedure GroupBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Spin_SpcBefChange(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure CB_NumbersClick(Sender: TObject);
    procedure CB_BulletsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Para : TParaInfo;
    OK_Click : boolean;
    CurrentNumbering : TRxNumbering;
    CurrentNumberingStyle : TRxNumberingStyle;
    procedure ParaToForm;
    procedure FormToPara;
  end;


implementation

{$R *.DFM}


procedure TForm_Para.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  CurrentNumbering := nsNone;
  CurrentNumberingStyle := nsPeriod;

  if ( _LoadedRichEditVersion <= 2 ) then
  begin
    CB_Numbers.Checked := false;
    CB_Numbers.Enabled := false;
  end;

  with Para do
  begin
    SpacingRule := lsSingle;
    LIndent := 0;
    RIndent := 0;
    FIndent := 0;
    SpaceBefore := 0;
    SpaceAfter := 0;
    Numbering := nsNone;
    NumberingStyle := nsPeriod;
    Alignment := paLeftJustify;
  end;


end; // CREATE

procedure TForm_Para.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  ParaToForm;
end; // ACTIVATE


procedure TForm_Para.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
  begin
    FormToPara;
  end;
  OK_Click := false;
end; // CloseQuery


procedure TForm_Para.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( Shift = [] ) then
    begin
      key := 0;
      ModalResult := mrCancel;
    end;
  end;
end;

{
In Rich Edit control:
First indentation: It is the principal indentation, it is set from the left margin in absolute values.
                   Corresponds to the first line.
Left indentation: This indentation is relative to the paragraph's current indentation (First line's indentation),
                  and controls indentation of 2nd and following lines. (can be positive or negative)

MS Word is more intuitive:
Left: Indentation of the paragraph, from the left margin
First indentation: Indentation of the first line regarding to the rest of the lines of the paragraph (can be positive or negative)
}

procedure TForm_Para.Spin_SpcBefChange(Sender: TObject);
var
  mySpin : TSpinEdit;
begin
    if Sender <> Spin_First then begin      // negative values are valid in Spin_First (will be treated as Left Indent)
        mySpin := ( sender as TSpinEdit );
        if ( mySpin.Value < 0 ) then
          mySpin.Value := 0;
    end;
    if Spin_First.Value < - Spin_Left.Value then
       Spin_First.Value:= - Spin_Left.Value;
end;

procedure TForm_Para.ParaToForm;
begin
  with Para do
  begin
    case SpacingRule of
      lsSingle : Combo_Spc.ItemIndex := 0;
      lsOneAndHalf : Combo_Spc.ItemIndex := 1;
      lsDouble : Combo_Spc.ItemIndex := 2;
    end;
    Spin_Left.Value := FIndent + LIndent;
    Spin_Right.Value := RIndent;
    Spin_First.Value := - LIndent;

    Spin_SpcBef.Value := SpaceBefore;
    Spin_SpcAft.Value := SpaceAfter;
    if ( _LoadedRichEditVersion > 2 ) then
    begin
      if ( Numbering = nsBullet ) then
        CB_Bullets.Checked := true
      else
      if ( Numbering <> nsNone ) then
        CB_Numbers.Checked := true;
    end
    else
    begin
      CB_Bullets.Checked := ( Numbering <> nsNone );
    end;
    case Alignment of
      paLeftJustify : RB_Align.ItemIndex := 0;
      paCenter : RB_Align.ItemIndex := 1;
      paRightJustify : RB_Align.ItemIndex := 2;
      paJustify : RB_Align.ItemIndex := 3;
    end;
  end;
end; // ParaToForm

procedure TForm_Para.FormToPara;
begin
  with Para do
  begin
    case Combo_Spc.ItemIndex of
      0 : SpacingRule := lsSingle;
      1 : SpacingRule := lsOneAndHalf;
      2 : SpacingRule := lsDouble;
    end;
    LIndent := - Spin_First.Value;
    FIndent := Spin_Left.Value - LIndent;
    RIndent := Spin_Right.Value;
    SpaceBefore := Spin_SpcBef.Value;
    SpaceAfter := Spin_SpcAft.Value;
    if CB_Numbers.Checked then
      Numbering := CurrentNumbering
    else
    if CB_Bullets.Checked then
      Numbering := nsBullet
    else
      Numbering := nsNone;
    case RB_Align.ItemIndex of
      0 : Alignment := paLeftJustify;
      1 : Alignment := paCenter;
      2 : Alignment := paRightJustify;
      3 : Alignment := paJustify;
    end;
  end;
end; procedure TForm_Para.GroupBox1Click(Sender: TObject);
begin

end;

// FormToPara

procedure TForm_Para.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;


procedure TForm_Para.CB_NumbersClick(Sender: TObject);
begin
  if CB_Numbers.Checked then
    CB_Bullets.Checked := false;
end;

procedure TForm_Para.CB_BulletsClick(Sender: TObject);
begin
  if CB_Bullets.Checked then
    CB_Numbers.Checked := false;
end;

end.
