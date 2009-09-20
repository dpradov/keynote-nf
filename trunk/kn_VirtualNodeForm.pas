unit kn_VirtualNodeForm;
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Mask, ToolEdit,
  gf_misc, gf_files, kn_Info, kn_Const;

type
  TForm_VNode = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Label_FN: TLabel;
    Edit_FN: TFilenameEdit;
    Edit_URL: TEdit;
    Label_URL: TLabel;
    RB_vmNormal: TRadioButton;
    RB_vmIELocal: TRadioButton;
    RB_vmIERemote: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure RB_vmNormalClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    myVirtualMode : TVirtualMode;
    myVirtualFN : string;
    myNodeName : string;
  end;

var
  Form_VNode: TForm_VNode;

implementation

{$R *.DFM}

procedure TForm_VNode.FormCreate(Sender: TObject);
begin
  myVirtualMode := vmNone;
  myVirtualFN := '';
  myNodeName := '';
  RB_vmIELocal.Enabled := _IE4Available;
  RB_vmIERemote.Enabled := _IE4Available;
  Edit_FN.InitialDir := GetFolderPath( fpPersonal );
end; // Create

procedure TForm_VNode.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if ( myNodeName <> '' ) then
    Caption := WideFormat( 'Virtual node: "%s"', [myNodeName] )
  else
    Caption := 'Create a virtual node';

  if ( myVirtualMode in [vmNone, vmText] ) then
    Edit_FN.FilterIndex := 1
  else
  if ( myVirtualMode = vmRTF ) then
    Edit_FN.FilterIndex := 2
  else
    Edit_FN.FilterIndex := 3;

  case myVirtualMode of
    vmNone, vmText, vmRTF, vmHTML : begin
      RB_vmNormal.Checked := true;
      Edit_FN.Text := myVirtualFN;
    end;
    vmIELocal : begin
      RB_vmIELocal.Checked := true;
      Edit_FN.Text := myVirtualFN;
    end;
    vmIERemote : begin
      RB_vmIERemote.Checked := true;
      Edit_URL.Text := myVirtualFN;
    end;
  end;

  RB_vmNormalClick( RB_vmNormal );
  RB_vmNormal.OnClick := RB_vmNormalClick;
  RB_vmIELocal.OnClick := RB_vmNormalClick;
  RB_vmIERemote.OnClick := RB_vmNormalClick;
end; // Activate

procedure TForm_VNode.RB_vmNormalClick(Sender: TObject);
begin
  if RB_vmNormal.Checked then
  begin
    Edit_URL.Visible := false;
    Label_URL.Visible := false;
    Edit_FN.Visible := true;
    Label_FN.Visible := true;
  end
  else
  if RB_vmIELocal.Checked then
  begin
    Edit_URL.Visible := false;
    Label_URL.Visible := false;
    Edit_FN.Visible := true;
    Label_FN.Visible := true;
  end
  else
  if RB_vmIERemote.Checked then
  begin
    Edit_FN.Visible := false;
    Label_FN.Visible := false;
    Edit_URL.Visible := true;
    Label_URL.Visible := true;
  end;
end; // RB_vmNormalClick

procedure TForm_VNode.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ( ModalResult = mrOK ) then
  begin
    if RB_vmIELocal.Checked then
    begin
      myVirtualMode := vmIELocal;
      myVirtualFN := Edit_FN.Text;
    end
    else
    if RB_vmIERemote.Checked then
    begin
      myVirtualMode := vmIERemote;
      myVirtualFN := trim( Edit_URL.Text );
    end
    else
    begin
      myVirtualMode := vmNone; // real mode established by node on the basis of filename extension
      myVirtualFN := Edit_FN.Text;
    end;
  end;
end;

end.
