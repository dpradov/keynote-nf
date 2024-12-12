unit kn_ImagePicker;

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
   cmpGFXListBox,
   RxPlacemnt
   ;


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
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadIcons;
  end;

function PickImage( const CurrentIdx : integer; var DoChildren : boolean ) : integer;

implementation
uses
   kn_Info,
   kn_Global,
   kn_Chest,
   knt.RS;

{$R *.DFM}


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
      List_ICN.AddItem( Format( GetRS(sImgP01), [succ( i )]), cbUnchecked, i );
    end;
  finally
    List_ICN.Items.EndUpdate;
  end;

end; // LoadIcons


procedure TForm_ImgPick.List_IcnDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

function TForm_ImgPick.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
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
