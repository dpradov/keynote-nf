unit kn_ImagePicker;

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
 <cicho@tenbit.pl>

************************************************************ *)


interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  kn_Info, kn_Chest, StdCtrls, cmpGFXListBox,
  ExtCtrls, Placemnt;

type
  TForm_ImgPick = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button_Cancel: TButton;
    Button_OK: TButton;
    List_Icn: TGFXListBox;
    CB_Children: TCheckBox;
    FormPlacement: TFormPlacement;
    procedure FormCreate(Sender: TObject);
    procedure List_IcnDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadIcons;
  end;

function PickImage( const CurrentIdx : integer; var DoChildren : boolean ) : integer;

implementation

{$R *.DFM}
resourcestring
  STR_01 = ' icon %d ';


function PickImage( const CurrentIdx : integer; var DoChildren : boolean ) : integer;
var
  Form_ImgPick: TForm_ImgPick;
  cnt : integer;
begin

  result := CurrentIdx;
  Form_ImgPick := TForm_ImgPick.Create( Application );

  try
    cnt := Form_ImgPick.List_Icn.Items.Count;
    if (( CurrentIdx >= 0 ) and ( CurrentIdx < cnt )) then
      Form_ImgPick.List_Icn.ItemIndex := CurrentIdx
    else
    begin
      if ( cnt > 0 ) then
        Form_ImgPick.List_Icn.ItemIndex := 0;
    end;
    Form_ImgPick.CB_Children.Enabled := DoChildren;
    if ( Form_ImgPick.ShowModal = mrOK ) then
    begin
      result := Form_ImgPick.List_Icn.ItemIndex;
      DoChildren := ( Form_ImgPick.CB_Children.Enabled and Form_ImgPick.CB_Children.Checked );
    end
    else
    begin
      result := -1;
      DoChildren := false;
    end;

  finally

  end;

end; // PickImage

procedure TForm_ImgPick.FormCreate(Sender: TObject);
begin
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  List_Icn.CheckBoxes := false;
  List_ICN.ImageList := Chest.IMG_Categories;

  List_ICN.Items.BeginUpdate;
  try
    LoadIcons;
    if ( List_ICN.Items.Count > 0 ) then
      List_ICN.ItemIndex := 0;
  finally
    List_ICN.Items.EndUpdate;
  end;

end;

procedure TForm_ImgPick.LoadIcons;
var
  i : integer;
begin
  List_ICN.Clear;
  if ( Chest.IMG_Categories.Count < 0 ) then exit;
  List_ICN.Items.BeginUpdate;
  try
    for i := 0 to pred( Chest.IMG_Categories.Count ) do
    begin
      // List_ICN.AddItem( Format( ' icon %d ', [succ( i )]), i );
      List_ICN.AddItem( Format( STR_01, [succ( i )]), cbUnchecked, i );
    end;
  finally
    List_ICN.Items.EndUpdate;
  end;

end; // LoadIcons


procedure TForm_ImgPick.List_IcnDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TForm_ImgPick.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if ( key = #13 ) then
  begin
    key := #0;
    ModalResult := mrOK;
  end;
end;







end.
