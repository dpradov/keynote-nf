unit kn_TabSelect;

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
   cmpGFXListBox,
   kn_FileObj
   ;


type
  TForm_SelectTab = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Button_All: TButton;
    Button_None: TButton;
    Button_Invert: TButton;
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
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    myNotes : TKntFile;

    procedure TabsToForm;
    procedure FormToTabs;

  end;

function SelectTabs( const aNoteFile : TKntFile ) : boolean;

implementation
uses
   kn_Const,
   kn_Global,
   kn_Chest,
   kn_NoteObj
   ;


{$R *.DFM}

function SelectTabs( const aNoteFile : TKntFile ) : boolean;
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
end; function TForm_SelectTab.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// CREATE

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
  aNote : TKntFolder;
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

      List_Tabs.AddItem( aNote.Name + TreeNoteMarker, cb, aNote.ImageIndex )
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
