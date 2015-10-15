unit kn_NewTemplate;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, gf_files, TntStdCtrls, TntExtCtrls;

type
  TForm_Template = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    GroupBox1: TTntGroupBox;
    Label1: TTntLabel;
    Edit_Name: TTntEdit;
    CB_Formatted: TTntCheckBox;
    RG_Source: TTntRadioGroup;
    procedure Edit_NameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
