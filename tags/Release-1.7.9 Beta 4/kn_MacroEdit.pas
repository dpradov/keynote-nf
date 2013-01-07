
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

unit kn_MacroEdit;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, gf_misc, gf_strings,
  kn_Const, kn_Info, kn_Macro, TntStdCtrls;

type
  TForm_Macro = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    GroupBox1: TTntGroupBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Edit_Desc: TTntEdit;
    Label3: TTntLabel;
    LB_Date: TTntLabel;
    Edit_Name: TTntEdit;
    Label4: TTntLabel;
    LB_FileName: TTntLabel;
    CB_AbortOnError: TTntCheckBox;
    Button_Help: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit_NameChange(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    myNewMacro : boolean;
    MName, MDesc: WideString;
    MDate, MFileName : string;
    MAbort : boolean;
    OriginalName : wideString;
  end;



implementation

{$R *.DFM}

resourcestring
  STR_01 = 'New macro';
  STR_02 = 'Macro name cannot be blank.';
  STR_03 = 'Another macro with this name already exists. Macro names must be unique.';


procedure TForm_Macro.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  myNewMacro := true;
  MName := '';
  MDesc := '';
  MAbort := true;
  MDate := datetostr( now );
  MFileName := '';
  OriginalName := '';
end;

procedure TForm_Macro.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if myNewMacro then
    Caption := STR_01;
  Edit_Name.Text := MName;
  OriginalName := MName;
  Edit_Desc.text := MDesc;
  CB_AbortOnError.Checked := MAbort;
  LB_FileName.Caption := MFileName;
  LB_Date.Caption := MDate;
  if myNewMacro then
    Edit_Name.OnChange := Edit_NameChange
  else
    Edit_Name.onChange := nil;
end;

procedure TForm_Macro.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( Shift = [] ) then
    begin
      key := 0;
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TForm_Macro.Edit_NameChange(Sender: TObject);
var
  t : wideString;
begin
  t := trim( Edit_Name.Text );
  if ( t <> '' ) then
    LB_FileName.Caption := MakeMacroFileName( t )
  else
    LB_FileName.Caption := '';
end;

procedure TForm_Macro.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_Macro.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_Macro.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  i : integer;
begin
  if OK_Click then
  begin
    OK_Click := false;
    
    MName := trim( Edit_Name.Text );
    MDesc := trim( Edit_Desc.Text );
    MAbort := CB_AbortOnError.Checked;
    MFileName := LB_FileName.Caption;

    if ( MName = '' ) then
    begin
      messagedlg( STR_02, mtError, [mbOK], 0 );
      Edit_Name.SetFocus;
      CanClose := false;
      exit;
    end;


    i := Macro_List.IndexOf( MName );
    if myNewMacro then
      CanClose := ( i < 0 )
    else
      CanClose := (( i < 0 ) or
                  ( Macro_List.IndexOf( OriginalName ) = i ));
    if ( not CanClose ) then
    begin
      messagedlg( STR_03, mtError, [mbOK], 0 );
      Edit_Name.SetFocus;
      exit;
    end;
  end;
end; // CLOSEQUERY

procedure TForm_Macro.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

end.
