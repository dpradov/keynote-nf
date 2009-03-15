unit kn_FavExtDlg;
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
 <marekjed@users.sourceforge.net>

************************************************************ *)

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ToolEdit,
  gf_misc, gf_files, kn_Info;

type
  TForm_FavExt = class(TForm)
    GroupBox1: TGroupBox;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Edit_FN: TFilenameEdit;
    Label2: TLabel;
    Edit_Params: TEdit;
    Label3: TLabel;
    Edit_Name: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure Edit_FNChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

resourcestring
  STR_01 = 'The specified file does not exist. Do you want to use the filename anyway?';



procedure TForm_FavExt.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Edit_FN.Filter := FILTER_FAVORITES;
  Edit_FN.SelectAll;
  try
    Edit_FN.SetFocus;
  except
  end;
  Button_OK.Enabled := ( trim( Edit_FN.Text ) <> '' );
end;

procedure TForm_FavExt.Edit_FNChange(Sender: TObject);
begin
  Button_OK.Enabled := ( trim( Edit_FN.Text ) <> '' );
end;

procedure TForm_FavExt.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ( ModalResult = mrOK ) then
  begin
    if ( not fileexists( NormalFN( Edit_FN.Text ))) then
      CanClose := ( messagedlg( STR_01, mtWarning, [mbOK,mbCancel], 0 ) = mrOK );
  end;
end;

end.
