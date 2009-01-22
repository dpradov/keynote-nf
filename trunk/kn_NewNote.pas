
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

unit kn_NewNote;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, gf_misc, kn_Defaults,
  kn_Info, kn_NoteObj, kn_Const, kn_INI,
  gf_strings, kn_Chest, cmpGFXComboBox;

type
  TForm_NewNote = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Combo_TabName: TComboBox;
    Button_Properties: TButton;
    Label_Type: TLabel;
    Combo_TabType: TComboBox;
    Combo_Icons: TGFXComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure Combo_TabNameChange(Sender: TObject);
    procedure Button_PropertiesClick(Sender: TObject);
    procedure Combo_TabNameKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    TAB_TYPE : TNoteType;
    TAB_CHANGEABLE : boolean;
    Initializing : boolean;
    // State : TTextControlState;
    StateChanged : boolean;

    myChrome : TChrome;
    myEditorProperties : TNoteEditorProperties;
    myTabProperties : TNoteTabProperties;
    myTreeProperties : TNoteTreeProperties;
    myTreeChrome : TChrome;
    myTreeOptions : TKNTTreeOptions;

    myTabNameHistory : string;
    myHistoryCnt : integer;
    myNodeNameHistory : string;

    function Verify : boolean;
    procedure ExecuteEditProperties;
  end;

implementation

{$R *.DFM}

resourcestring
  STR_01 = '<no icon>';
  STR_02 = 'Rename note';
  STR_03 = 'Note name cannot be blank. Please enter a name.';
  STR_04 = 'Note name cannot contain the "%s" character';

procedure TForm_NewNote.FormCreate(Sender: TObject);
var
  i : integer;
  t : TNoteType;
begin
  Initializing := true;
  OK_Click := false;

  InitializeChrome( myChrome );
  InitializeNoteEditorProperties( myEditorProperties );
  InitializeNoteTabProperties( myTabProperties );
  InitializeChrome( myTreeChrome );
  InitializeNoteTreeProperties( myTreeProperties );
  InitializeTreeOptions( myTreeOptions );

  TAB_TYPE := ntRTF;
  TAB_CHANGEABLE := true;
  StateChanged := false;
  myTabNameHistory := '';
  myHistoryCnt := DEFAULT_HISTORY_COUNT;
  myNodeNameHistory := '';

  Combo_TabName.MaxLength := TABNOTE_NAME_LENGTH;

  for t := low( TNoteType ) to high( TNoteType ) do
    Combo_TabType.Items.Add( TABNOTE_KIND_NAMES[t] );

  Combo_Icons.ImageList := Chest.IMG_Categories;
  Combo_Icons.AddItem( STR_01, -1 );
  for i := 0 to pred( Chest.IMG_Categories.Count ) do
    Combo_Icons.AddItem( ' - ' + inttostr( succ( i )), i );
  Combo_Icons.ItemIndex := 0;

end; // FORM_CREATE

procedure TForm_NewNote.FormActivate(Sender: TObject);
begin
  if ( not Initializing ) then exit;
  Initializing := false;
  if ( not TAB_CHANGEABLE ) then
  begin
    Caption := STR_02;
  end;

  Combo_TabName.Items.BeginUpdate;
  try
    DelimTextToStrs( Combo_TabName.Items, myTabNameHistory, HISTORY_SEPARATOR );
  finally
    Combo_TabName.Items.EndUpdate;
  end;

  // Combo_TabName.Items.Insert( 0, DEFAULT_NEW_NOTE_NAME );
  Combo_TabName.Text := myTabProperties.Name;
  Combo_Icons.ItemIndex := succ( myTabProperties.ImageIndex );


  with Combo_TabType do
  begin
    ItemIndex := ord( TAB_TYPE );
    Enabled := TAB_CHANGEABLE;
  end;      
  Label_Type.Enabled := Combo_TabType.Enabled;

  Button_Properties.Enabled := ( Combo_TabName.Text <> '' );

  try
    Combo_TabName.SetFocus;
    Combo_TabName.SelectAll;
  except
  end;

end; // FORM_ACTIVATE

function TForm_NewNote.Verify : boolean;
begin
  result := false;
  if ( trim( Combo_TabName.Text ) = '' ) then
  begin
    messagedlg( STR_03, mtError, [mbOK], 0 );
    Combo_TabName.SetFocus;
    exit;
  end;

  if ( pos( KNTLINK_SEPARATOR, Combo_TabName.Text ) > 0 ) then
  begin
    messagedlg( Format(STR_04,[KNTLINK_SEPARATOR]), mtError, [mbOK], 0 );
    Combo_TabName.SetFocus;
    exit;
  end;

  result := true;
end; // Verify

procedure TForm_NewNote.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i : integer;
begin
  if OK_Click then
  begin
    CanClose := Verify;
    if CanClose then
    begin
      myTabProperties.Name := trim( Combo_TabName.Text );
      myTabProperties.ImageIndex := pred( Combo_Icons.ItemIndex );

      TAB_TYPE := TNoteType( Combo_TabType.ItemIndex );
      // TAB_TYPE := low( TNoteType );

      myTabNameHistory := ANSIQuotedStr( Combo_TabName.Text, '"' );
      for i := 0 to pred( Combo_TabName.Items.Count ) do
      begin
        if ( i >= myHistoryCnt ) then break;
        if ( Combo_TabName.Items[i] <> Combo_TabName.Text ) then
          myTabNameHistory :=  myTabNameHistory + HISTORY_SEPARATOR + ANSIQuotedStr( Combo_TabName.Items[i], '"' );
      end;
    end;
  end;
  OK_Click := false;
end; // FORM_CLOSEQUERY

procedure TForm_NewNote.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( shift = [] ) and ( not ( Combo_TabName.DroppedDown or Combo_Icons.DroppedDown or Combo_TabType.DroppedDown ))) then
    begin
      key := 0;
      OK_Click := false;
      Close;
    end;
  end;
end; // FORM_KEYDOWN

procedure TForm_NewNote.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_NewNote.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_NewNote.Combo_TabNameChange(Sender: TObject);
begin
  // Button_Properties.Enabled := ( Combo_TabName.Text <> '' );
end;

procedure TForm_NewNote.ExecuteEditProperties;
var
  Form_Defaults : TForm_Defaults;
begin
  myTabProperties.Name := trim( Combo_TabName.Text );
  myTabProperties.ImageIndex := pred( Combo_Icons.ItemIndex );

  Form_Defaults := TForm_Defaults.Create( self );
  try
    Form_Defaults.StartWithEditorTab := true;
    Form_Defaults.Action := propThisNote;
    Form_Defaults.myEditorChrome := myChrome;
    Form_Defaults.myTabProperties := myTabProperties;
    Form_Defaults.myEditorProperties := myEditorProperties;
    Form_Defaults.myTabNameHistory := myTabNameHistory;
    Form_Defaults.myNodeNameHistory := myNodeNameHistory;
    Form_Defaults.myTreeProperties := myTreeProperties;
    Form_Defaults.myTreeChrome := myTreeChrome;
    Form_Defaults.NoteKind := TNoteType( Combo_TabType.ItemIndex );

    // Form_Defaults.Defaults := false;
    if ( Form_Defaults.ShowModal = mrOK ) then
    begin
      myChrome := Form_Defaults.myEditorChrome;
      myTabProperties := Form_Defaults.myTabProperties;
      myEditorProperties := Form_Defaults.myEditorProperties;

      myTreeProperties := Form_Defaults.myTreeProperties;
      myTreeChrome := Form_Defaults.myTreeChrome;

      Combo_TabName.Text := myTabProperties.Name;
      Combo_Icons.ItemIndex := succ( myTabProperties.ImageIndex );
    end;
  finally
    Form_Defaults.Free;
  end;
end; // ExecuteEditProperties

procedure TForm_NewNote.Button_PropertiesClick(Sender: TObject);
begin
  ExecuteEditProperties;
end;

procedure TForm_NewNote.Combo_TabNameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if ( Key = KNTLINK_SEPARATOR ) then
    Key := #0;
end; // Combo_TabNameKeyPress

end.
