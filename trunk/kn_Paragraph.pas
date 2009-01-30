unit kn_Paragraph;

(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)


interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  Placemnt, StdCtrls, ExtCtrls, Spin,
  kn_NoteObj, kn_Info, RxRichEd, RichEdit;

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
//  if Sender = Spin_First then begin
//     // negative values are valid in Spin_First (will be treated as Left Indent)
//     if Spin_First.Value < - Spin_Left.Value then
//        Spin_First.Value:= - Spin_Left.Value;
//  end
//  else begin
    if Sender <> Spin_First then begin      // negative values are valid in Spin_First (will be treated as Left Indent)
        mySpin := ( sender as TSpinEdit );
        if ( mySpin.Value < 0 ) then
          mySpin.Value := 0;
    end;
    if Spin_First.Value < - Spin_Left.Value then
        Spin_First.Value:= - Spin_Left.Value;
//  end;
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
