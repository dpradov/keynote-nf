unit kn_ExpTermDef;

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
   Vcl.StdCtrls;

type
  TForm_TermDef = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit_Term: TEdit;
    Edit_Exp: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;



implementation
uses
  kn_Global,
  knt.App;

{$R *.DFM}

function TForm_TermDef.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

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
    App.SetTopMost(Handle, True);
    App.ApplyBiDiModeOnForm(Self);
    if ( Edit_Term.Text = '' ) then
      Edit_Term.SetFocus
    else
      Edit_Exp.SetFocus;
  except
  end;
end;

end.
