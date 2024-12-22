unit kn_NodeNum;

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
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   Vcl.Samples.Spin;

type
  TForm_NodeNum = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    RG_Scope: TRadioGroup;
    RG_Method: TRadioGroup;
    gbDepth: TGroupBox;
    LB_Depth: TLabel;
    Spin_Depth: TSpinEdit;
    CB_FullDepth: TCheckBox;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RG_MethodClick(Sender: TObject);
    procedure CB_FullDepthClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_NodeNum: TForm_NodeNum;

implementation
Uses
  kn_Global,
  knt.App;

{$R *.DFM}


function TForm_NodeNum.FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// create

procedure TForm_NodeNum.FormActivate(Sender: TObject);
begin
  App.ApplyBiDiModeOnForm(Self);
end; // activate

procedure TForm_NodeNum.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

end; // close query

procedure TForm_NodeNum.RG_MethodClick(Sender: TObject);
begin
  CB_FullDepth.Enabled := ( RG_Method.ItemIndex <> 2 );
  LB_Depth.Enabled:= CB_FullDepth.Enabled;
end;


procedure TForm_NodeNum.CB_FullDepthClick(Sender: TObject);
begin
  Spin_Depth.Enabled := ( not CB_FullDepth.Checked );
  LB_Depth.Enabled := Spin_Depth.Enabled ;
end;

end.
