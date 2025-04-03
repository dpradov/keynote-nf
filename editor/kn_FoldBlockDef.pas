unit kn_FoldBlockDef;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2025 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
   
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
  TForm_FoldBlockDef = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit_Opening: TEdit;
    Edit_Closing: TEdit;
    chkCaseSens: TCheckBox;
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

function TForm_FoldBlockDef.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_FoldBlockDef.FormKeyDown(Sender: TObject; var Key: Word;
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

procedure TForm_FoldBlockDef.FormActivate(Sender: TObject);
begin
  try
    App.SetTopMost(Handle, True);
    App.ApplyBiDiModeOnForm(Self);
    if ( Edit_Opening.Text = '' ) then
      Edit_Opening.SetFocus
    else
      Edit_Closing.SetFocus;
  except
  end;
end;

end.
