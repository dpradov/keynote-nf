
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

unit kn_TabSelect;

interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, gf_misc,
  kn_Info, kn_Const, kn_Chest,
  kn_NoteObj, kn_FileObj,
  StdCtrls, cmpGFXListBox, TntStdCtrls;

type
  TForm_SelectTab = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Button_All: TTntButton;
    Button_None: TTntButton;
    Button_Invert: TTntButton;
    List_Tabs: TGFXListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure List_TabsDblClick(Sender: TObject);
    procedure Button_AllClick(Sender: TObject);
    procedure Button_NoneClick(Sender: TObject);
    procedure Button_InvertClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    myNotes : TNoteFile;

    procedure TabsToForm;
    procedure FormToTabs;

  end;

function SelectTabs( const aNoteFile : TNoteFile ) : boolean;

implementation

{$R *.DFM}

function SelectTabs( const aNoteFile : TNoteFile ) : boolean;
var
  Form_SelectTab : TForm_SelectTab;
begin
  Form_SelectTab := TForm_SelectTab.Create( Application );
  try
    Form_SelectTab.myNotes := aNoteFile;
    result := ( Form_SelectTab.ShowModal = mrOK );
  finally
    Form_SelectTab.Free;
  end;
end; // SelectTabs

procedure TForm_SelectTab.FormCreate(Sender: TObject);
begin
  myNotes := nil;
  OK_Click := false;
  List_Tabs.ImageList := Chest.IMG_Categories;
end; // CREATE

procedure TForm_SelectTab.FormActivate(Sender: TObject);
begin
  TabsToForm;
  OnActivate := nil;
end; // ACTIVATE

procedure TForm_SelectTab.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TForm_SelectTab.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
  begin
    FormToTabs;
  end;
  OK_Click := false;
end;

procedure TForm_SelectTab.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_SelectTab.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_SelectTab.TabsToForm;
const
  TreeNoteMarker = '  (T)';
var
  i : integer;
  cb : TCheckBoxState;
  aNote : TTabNote;
begin
  if ( not assigned( myNotes )) then exit;
  if ( myNotes.NoteCount < 1 ) then exit;
  List_Tabs.Items.BeginUpdate;
  try
    for i := 0 to pred( myNotes.NoteCount ) do
    begin
      aNote := myNotes.Notes[i];
      if ( aNote.Info <> 0 ) then
        cb := cbChecked
      else
        cb := cbUnchecked;
      if ( aNote.Kind = ntTree ) then
        List_Tabs.AddItem( aNote.Name + TreeNoteMarker, cb, aNote.ImageIndex )
      else
        List_Tabs.AddItem( aNote.Name, cb, aNote.ImageIndex );
    end;
  finally
    List_Tabs.Items.EndUpdate;
  end;
end; // TabsToForm

procedure TForm_SelectTab.FormToTabs;
var
  i : integer;
begin
  if ( not assigned( myNotes )) then exit;
  if ( myNotes.NoteCount < 1 ) then exit;
  for i := 0 to pred( myNotes.NoteCount ) do
  begin
    if List_Tabs.Checked[i] then
      myNotes.Notes[i].Info := 1
    else
      myNotes.Notes[i].Info := 0;
  end;
end; // FormToTabs

procedure TForm_SelectTab.List_TabsDblClick(Sender: TObject);
begin
  if ( List_Tabs.ItemIndex >= 0 ) then
    List_Tabs.Checked[List_Tabs.ItemIndex] := ( not List_Tabs.Checked[List_Tabs.ItemIndex] );
end;

procedure TForm_SelectTab.Button_AllClick(Sender: TObject);
begin
  List_Tabs.SetAll( cbChecked );
end;

procedure TForm_SelectTab.Button_NoneClick(Sender: TObject);
begin
  List_Tabs.SetAll( cbUnChecked );
end;

procedure TForm_SelectTab.Button_InvertClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to pred( List_Tabs.Items.Count ) do
  begin
    List_Tabs.Checked[i] := ( not List_Tabs.Checked[i] );
  end;
end;

end.
