unit kn_MacroCmdSelect;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

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
   kn_MacroCmd;


type
  TForm_MacroCmd = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Combo_Cmd: TComboBox;
    Edit_Params: TEdit;
    Panel_Help: TPanel;
    Button_OK: TButton;
    Button_Cancel: TButton;
    LB_Help: TLabel;
    Label3: TLabel;
    LB_Syntax: TLabel;
    Button_Help: TButton;
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
