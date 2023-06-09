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
    RG_CurNum: TRadioGroup;
    RG_Method: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Spin_StartNum: TSpinEdit;
    GroupBox2: TGroupBox;
    LB_Depth: TLabel;
    Spin_Depth: TSpinEdit;
    CB_FullDepth: TCheckBox;
    Btn_Remove: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RG_MethodClick(Sender: TObject);
    procedure CB_FullDepthClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_NodeNum: TForm_NodeNum;

implementation

{$R *.DFM}


procedure TForm_NodeNum.FormCreate(Sender: TObject);
begin

end; // create

procedure TForm_NodeNum.FormActivate(Sender: TObject);
begin

end; // activate

procedure TForm_NodeNum.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin

end; // close query

procedure TForm_NodeNum.RG_MethodClick(Sender: TObject);
begin
  RG_CurNum.Enabled := ( RG_Method.ItemIndex = 0 );
end;


procedure TForm_NodeNum.CB_FullDepthClick(Sender: TObject);
begin
  Spin_Depth.Enabled := ( not CB_FullDepth.Checked );
  LB_Depth.Enabled := Spin_Depth.Enabled ;
end;

end.
