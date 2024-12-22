unit kn_pass;

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
   Vcl.ExtCtrls;

type
  TForm_Password = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Label_FileName: TLabel;
    Label2: TLabel;
    Edit_Pass: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    OK_Click : boolean;
    myTimeout : integer; // seconds
    myFileName : string;
    function VerifyPass : boolean;
  end;

implementation
uses
  kn_Global,
  knt.App,
  knt.RS;

{$R *.DFM}

procedure TForm_Password.FormKeyDown(Sender: TObject; var Key: Word;
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
end;

procedure TForm_Password.FormCreate(Sender: TObject);
begin
  OK_Click := false;
  App.ApplyBiDiModeOnForm(Self);
end;

function TForm_Password.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_Password.Button_OKClick(Sender: TObject);
begin
  OK_Click := true;
end;

procedure TForm_Password.Button_CancelClick(Sender: TObject);
begin
  OK_Click := false;
end;

procedure TForm_Password.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if OK_Click then
    CanClose := VerifyPass;
  OK_Click := false;
end;

function TForm_Password.VerifyPass : boolean;
begin
  result := ( Edit_Pass.Text <> '' );
  if ( not result ) then
  begin
    Edit_Pass.SetFocus;
    messagedlg( GetRS(sPass01), mtError, [mbOK], 0 );
  end;
end; // VerifyPass

procedure TForm_Password.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Label_FileName.Caption := myFileName;
  Caption:= Format(GetRS(sPass02), [ExtractFilename( myFileName )]);

  // when auto-reopening previously auto-closed encrypted files,
  // (see TForm_Main.AutoCloseKntFile) the password window does not
  // properly activate. So we force it to.
  SetForegroundWindow( self.Handle );
end;

end.
