unit kn_FileDropAction;
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

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  kn_Const, kn_Info, TntStdCtrls, TntExtCtrls;

type
  TForm_DropFile = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    PagesImp: TNotebook;
    RG_Action: TTntRadioGroup;
    Btn_HTML: TTntButton;
    RG_HTML: TRadioGroup;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Btn_HTMLClick(Sender: TObject);
    procedure RG_ActionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NumberOfFiles : integer;
    FileExt : string;
  end;


implementation

{$R *.DFM}

resourcestring
  STR_01 = 'file';
  STR_02 = 'files';
  STR_03 = 'Select import method (%d *%s %s)';
  STR_04 = '&General options';
  STR_05 = '&Virtual node...';
  STR_06 = '&HTML options';

procedure TForm_DropFile.FormCreate(Sender: TObject);
var
  m : THTMLImportMethod;
begin
  NumberOfFiles := 0;
  FileExt := '';
  PagesImp.PageIndex := 0;
  for m := low( m ) to high( m ) do
  begin
    RG_HTML.Items.Add( HTMLImportMethods[m] );
  end;
  RG_HTML.ItemIndex := 0;
end; // CREATE


procedure TForm_DropFile.FormActivate(Sender: TObject);
var
  s : string;
begin
  OnActivate := nil;
  if ( NumberOfFiles < 2 ) then
    s := STR_01
  else
    s := STR_02;
  Caption := Format(
    STR_03,
    [NumberOfFiles, FileExt, s] );

  try
    RG_Action.ItemIndex := 0;
    RG_ActionClick( RG_Action );
    RG_Action.OnClick := RG_ActionClick;
    RG_Action.SetFocus;
  except
  end;

end; // ACTIVATE


procedure TForm_DropFile.Btn_HTMLClick(Sender: TObject);
begin
  case PagesImp.PageIndex of
    0 : begin
      Btn_HTML.Caption := STR_04;
      PagesImp.PageIndex := 1;
    end;
    1 : begin
      PagesImp.PageIndex := 0;
      Btn_HTML.Caption := STR_06
    end;
  end;
end;

procedure TForm_DropFile.RG_ActionClick(Sender: TObject);
begin
  if Btn_HTML.Visible then
  begin
    Btn_HTML.Enabled := true;
    Btn_HTML.Caption := STR_06;
  end;
end;


end.
