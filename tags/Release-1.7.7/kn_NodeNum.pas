unit kn_NodeNum;
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
  StdCtrls, ExtCtrls, Spin, TntStdCtrls, TntExtCtrls;

type
  TForm_NodeNum = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    RG_Scope: TTntRadioGroup;
    RG_CurNum: TTntRadioGroup;
    RG_Method: TTntRadioGroup;
    GroupBox1: TTntGroupBox;
    Label1: TTntLabel;
    Spin_StartNum: TSpinEdit;
    GroupBox2: TTntGroupBox;
    LB_Depth: TTntLabel;
    Spin_Depth: TSpinEdit;
    CB_FullDepth: TTntCheckBox;
    Btn_Remove: TTntButton;
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
