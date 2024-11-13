unit kn_MacroEdit;

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
   Vcl.ExtCtrls
   ;


type
  TForm_Macro = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit_Desc: TEdit;
    Label3: TLabel;
    LB_Date: TLabel;
    Edit_Name: TEdit;
    Label4: TLabel;
    LB_FileName: TLabel;
    CB_AbortOnError: TCheckBox;
    Button_Help: TButton;
    chkProfile: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit_NameChange(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button_HelpClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    myNewMacro : boolean;
    MName, MDesc: string;
    MDate, MFileName : string;
    MProfile: boolean;
    MAbort : boolean;
    OriginalName : string;
  end;



implementation
uses
  kn_global,
  kn_Macro,
  knt.RS;

{$R *.DFM}


procedure TForm_Macro.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  myNewMacro := true;
  MName := '';
  MDesc := '';
  MAbort := true;
  MDate := datetostr( now );
  MFileName := '';
  MProfile := false;
  OriginalName := '';
end;

function TForm_Macro.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_Macro.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  if myNewMacro then
    Caption := sMacE01;
  Edit_Name.Text := MName;
  OriginalName := MName;
  Edit_Desc.text := MDesc;
  CB_AbortOnError.Checked := MAbort;
  LB_FileName.Caption := MFileName;
  LB_Date.Caption := MDate;
  chkProfile.Checked:= MProfile;
  if myNewMacro then
    Edit_Name.OnChange := Edit_NameChange
  else
    Edit_Name.onChange := nil;

  if MName <> '' then
    chkProfile.Enabled:= false;
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
  t : string;
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
    MProfile := chkProfile.Checked;

    if ( MName = '' ) then begin
      messagedlg( sMacE02, mtError, [mbOK], 0 );
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

    if ( not CanClose ) then begin
      messagedlg( sMacE03, mtError, [mbOK], 0 );
      Edit_Name.SetFocus;
      exit;
    end;
  end;
end; // CLOSEQUERY

procedure TForm_Macro.Button_HelpClick(Sender: TObject);
begin
   ActiveKeyNoteHelp(self.HelpContext);  // Node
  //Application.HelpCommand( HELP_CONTEXT, self.HelpContext );
end;

end.
