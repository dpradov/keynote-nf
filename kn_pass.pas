
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

unit kn_pass;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TntStdCtrls;

type
  TForm_Password = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    GroupBox1: TTntGroupBox;
    Label_FileName: TTntLabel;
    Label2: TTntLabel;
    Edit_Pass: TTntEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure Button_CancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormActivate(Sender: TObject);
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
uses TntSysUtils;

{$R *.DFM}

resourcestring
  STR_01 = 'Passphrase cannot be blank.';
  STR_02 = 'File "%s" is encrypted';

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
    messagedlg( STR_01, mtError, [mbOK], 0 );
  end;
end; // VerifyPass

procedure TForm_Password.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Label_FileName.Caption := myFileName;
  Caption:= WideFormat(STR_02, [WideExtractFilename( myFileName )]);

  // when auto-reopening previously auto-closed encrypted files,
  // (see TForm_Main.AutoCloseFile) the password window does not
  // properly activate. So we force it to.
  SetForegroundWindow( self.Handle );
end;

end.
