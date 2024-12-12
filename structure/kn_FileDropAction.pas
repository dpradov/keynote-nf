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
  TForm_DropFile = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    PagesImp: TNotebook;
    RG_Action: TRadioGroup;
    Btn_HTML: TButton;
    RG_HTML: TRadioGroup;
    chk_ImageLinkMode: TCheckBox;
    txtImgNewName: TEdit;
    lblRenamed: TLabel;
    chk_Relative: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Btn_HTMLClick(Sender: TObject);
    procedure RG_ActionClick(Sender: TObject);
    procedure txtImgNewNameExit(Sender: TObject);
    procedure chk_ImageLinkModeClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
    OfferImageLinkMode: boolean;
  public
    { Public declarations }
    NumberOfFiles : integer;
    FileExt : string;
    ShowNewName: boolean;
    ShowWarningRenamedNames: boolean;
  end;


implementation

uses
  kn_NoteFileMng,
  kn_Const,
  kn_Global,
  knt.App,
  knt.RS;

{$R *.DFM}


procedure TForm_DropFile.FormCreate(Sender: TObject);
var
  m : THTMLImportMethod;
begin
  NumberOfFiles := 0;
  FileExt := '';
  PagesImp.PageIndex := 0;
  for m := low( m ) to high( m ) do
     RG_HTML.Items.Add( HTMLImportMethods[m] );

  RG_HTML.ItemIndex := 0;
end;

function TForm_DropFile.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

// CREATE


procedure TForm_DropFile.FormActivate(Sender: TObject);
var
  s : string;
begin
  OnActivate := nil;
  if ( NumberOfFiles < 2 ) then
    s := GetRS(sFDrp01)
  else
    s := GetRS(sFDrp02);

  Caption := Format(GetRS(sFDrp03), [NumberOfFiles, FileExt, s] );

  try
    OfferImageLinkMode:= chk_ImageLinkMode.Visible;

    RG_ActionClick( RG_Action );
    RG_Action.OnClick := RG_ActionClick;
    RG_Action.SetFocus;

    if ShowNewName then begin
       lblRenamed.Visible:= true;
       txtImgNewName.Visible:= true;
    end
    else
    if ShowWarningRenamedNames then begin
       lblRenamed.Visible:= true;
       lblRenamed.Caption:= GetRS(sFDrp07);
    end;

  except
  end;

end; // ACTIVATE


procedure TForm_DropFile.Btn_HTMLClick(Sender: TObject);
begin
  case PagesImp.PageIndex of
    0 : begin
      Btn_HTML.Caption := GetRS(sFDrp04);
      PagesImp.PageIndex := 1;
    end;
    1 : begin
      PagesImp.PageIndex := 0;
      Btn_HTML.Caption := GetRS(sFDrp06)
    end;
  end;
end;

procedure TForm_DropFile.RG_ActionClick(Sender: TObject);
var
  actionName: string;

begin
  if Btn_HTML.Visible then begin
    Btn_HTML.Enabled := true;
    Btn_HTML.Caption := GetRS(sFDrp06);
  end;

  if Visible then begin
     actionName := RG_Action.Items[RG_Action.ItemIndex];
     chk_ImageLinkMode.Visible:= OfferImageLinkMode
           and ((actionName = FactStrings[factInsertContent])
               or (actionName = FactStrings[factImportAsNode])
               or (actionName = FactStrings[factImportAsFolder]) );
     chk_Relative.Visible:= (actionName = FactStrings[factHyperlink]);
  end;
end;


procedure TForm_DropFile.txtImgNewNameExit(Sender: TObject);
var
  NewName: string;
begin
    NewName:= txtImgNewName.Text;
    if not ImageMng.CheckUniqueName(NewName) then begin
       txtImgNewName.Text:= NewName;
    end;

end;

procedure TForm_DropFile.chk_ImageLinkModeClick(Sender: TObject);
begin
  lblRenamed.Visible:= (ShowNewName or ShowWarningRenamedNames) and not chk_ImageLinkMode.Checked;
  txtImgNewName.Visible:= ShowNewName and not chk_ImageLinkMode.Checked;;
end;


end.
