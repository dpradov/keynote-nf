unit kn_ExpTermDef;

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
  StdCtrls, TntStdCtrls;

type
  TForm_TermDef = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Edit_Term: TTntEdit;
    Edit_Exp: TTntEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



implementation

{$R *.DFM}

procedure TForm_TermDef.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TForm_TermDef.FormActivate(Sender: TObject);
begin
  try
    if ( Edit_Term.Text = '' ) then
      Edit_Term.SetFocus
    else
      Edit_Exp.SetFocus;
  except
  end;
end;

end.
