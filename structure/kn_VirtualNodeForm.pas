unit kn_VirtualNodeForm;

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
   Vcl.Mask,
   RxToolEdit,
   gf_misc,
   kn_Info,
   kn_Const;

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
    Caption := Format( 'Virtual node: "%s"', [myNodeName] )
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
