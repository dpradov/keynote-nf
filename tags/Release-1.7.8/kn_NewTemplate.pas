unit kn_NewTemplate;
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
 <marekjed@users.sourceforge.net>

************************************************************ *)

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
