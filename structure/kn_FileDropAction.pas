unit kn_FileDropAction;

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
