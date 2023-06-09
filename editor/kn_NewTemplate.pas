unit kn_NewTemplate;

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
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   Vcl.StdCtrls,
   gf_files;

type
  TForm_Template = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit_Name: TEdit;
    CB_Formatted: TCheckBox;
    RG_Source: TRadioGroup;
    procedure Edit_NameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit_NameKeyPress(Sender: TObject; var Key: Char);
    procedure Edit_NameChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_Template: TForm_Template;

implementation

{$R *.DFM}


procedure TForm_Template.Edit_NameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TForm_Template.Edit_NameKeyPress(Sender: TObject; var Key: Char);
begin
  if ( not ( IsValidFileChar( Key ) or ( Key < #33 ))) then
    Key := #0;
end;

procedure TForm_Template.Edit_NameChange(Sender: TObject);
begin
  Button_OK.Enabled := ( trim( Edit_Name.Text ) <> '' );
end;

procedure TForm_Template.FormActivate(Sender: TObject);
begin
  Edit_NameChange( Edit_Name );
  Edit_Name.SelectAll;
end;

end.
