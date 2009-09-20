
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

unit kn_MacroCmdSelect;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, gf_misc, gf_strings,
  kn_Const, kn_Info, kn_Macro, kn_MacroCmd, TntStdCtrls;

type
  TForm_MacroCmd = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Combo_Cmd: TTntComboBox;
    Edit_Params: TTntEdit;
    Panel_Help: TPanel;
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    LB_Help: TTntLabel;
    Label3: TTntLabel;
    LB_Syntax: TTntLabel;
    Button_Help: TTntButton;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Combo_CmdClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button_HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    OK_click : boolean;
    myCmd : TMacroCMD;
    myArgs : string;

    MacroCmds : TStringList;

    function CurrentCmd : TMacroCmd;


  end;


implementation

{$R *.DFM}


procedure TForm_MacroCmd.FormCreate(Sender: TObject);
var
  m : TMacroCmd;
begin
  OK_click := false;
  LB_Syntax.Font.Name := 'Courier';
  MacroCmds := TStringList.Create;
  MacroCmds.Capacity := succ( ord( high( TMacroCmd ))); 
  Combo_Cmd.Items.BeginUpdate;
  try
    for m := low( TMacroCmd ) to high( TMacroCmd ) do
    begin
      Combo_Cmd.Items.Add( MACRO_CMD_NAMES[m] );
      MacroCmds.Add( MACRO_CMD_NAMES[m] );
    end;
  finally
    Combo_Cmd.Items.EndUpdate;
  end;
  Combo_Cmd.ItemIndex := 0;


  myCmd := low( TMacroCMD );
  myArgs := '';

end;

procedure TForm_MacroCmd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if (( Shift = [] ) and
      ( not Combo_Cmd.DroppedDown )) then
    begin
      key := 0;
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TForm_MacroCmd.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_MacroCmd.Button_CancelClick(Sender: TObject);
begin
  OK_click := false;
end;

procedure TForm_MacroCmd.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);

begin
  if OK_click then
  begin
    OK_Click := false;
    myCmd := CurrentCmd;
    myArgs := Edit_Params.Text;
    if ( myCmd in MACRO_CMDS_WITH_STRING_ARGS ) then
    begin
      if ( myArgs = '' ) then
      begin
        if ( myCmd in MACRO_CMDS_WITH_OPTIONAL_STRING_ARGS ) then
        begin
        end
        else
        begin
          CanClose := false;
        end;
      end;
    end
    else
    if ( myCmd in MACRO_CMDS_WITH_INTEGER_ARGS ) then
    begin
      if ( myArgs = '' ) then
      begin
        if ( myCmd in MACRO_CMDS_WITH_OPTIONAL_INTEGER_ARGS ) then
        begin
        end
        else
        begin
          CanClose := false;
        end;
      end;
    end;
  end;
end;

procedure TForm_MacroCmd.FormDestroy(Sender: TObject);
begin
  MacroCmds.Free;
end;

function TForm_MacroCmd.CurrentCmd : TMacroCmd;
var
  s : string;
  i : integer;
begin
  s := Combo_Cmd.Items[Combo_Cmd.ItemIndex];
  i := MacroCmds.IndexOf( s );
  result := TMacroCmd( i );
end; // CurrentCmd

procedure TForm_MacroCmd.Combo_CmdClick(Sender: TObject);
begin
  myCmd := CurrentCmd;
  LB_Help.Caption := MACRO_DEFS[myCmd].Help;

  if ( MACRO_DEFS[myCmd].ArgType = argNone ) then
  begin
    LB_Syntax.Caption := MACRO_DEFS[myCmd].Name;
  end
  else
  begin

  LB_Syntax.Caption :=
    Format(
      '%s(%s)',
      [MACRO_DEFS[myCmd].Name, MACRO_DEFS[myCmd].Syntax] );
  end;

  LB_Syntax.Hint := LB_Syntax.Caption;

  Edit_Params.Text := MACRO_DEFS[myCmd].DefArg;

end;

procedure TForm_MacroCmd.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Combo_CmdClick( Combo_Cmd );
end;


procedure TForm_MacroCmd.Button_HelpClick(Sender: TObject);
begin
  Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

end.
