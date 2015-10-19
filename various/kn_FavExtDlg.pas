unit kn_FavExtDlg;

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
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ToolEdit,
  gf_misc, gf_files, kn_Info, TntStdCtrls, TB97Ctls, ImgList, TntDialogs;

type
  TForm_FavExt = class(TForm)
    GroupBox1: TTntGroupBox;
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Edit_Params: TTntEdit;
    Label3: TTntLabel;
    Edit_Name: TTntEdit;
    Edit_FN: TTntEdit;
    TB_OpenDlg: TToolbarButton97;
    procedure TB_OpenDlgClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Edit_FNChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses kn_main, TntSysUtils;

{$R *.DFM}

resourcestring
  STR_01 = 'The specified file does not exist. Do you want to use the filename anyway?';



procedure TForm_FavExt.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Form_Main.OpenDlg.Filter:= FILTER_FAVORITES;
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
    if ( not WideFileexists( NormalFN( Edit_FN.Text ))) then
      CanClose := ( messagedlg( STR_01, mtWarning, [mbOK,mbCancel], 0 ) = mrOK );
  end;
end;

procedure TForm_FavExt.TB_OpenDlgClick(Sender: TObject);
begin
  if Form_Main.OpenDlg.Execute then begin
     Edit_FN.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlg.Down:= false;
end;

end.
