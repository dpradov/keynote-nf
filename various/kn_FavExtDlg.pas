unit kn_FavExtDlg;

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
   TB97Ctls
   ;


type
  TForm_FavExt = class(TForm)
    GroupBox1: TGroupBox;
    Button_OK: TButton;
    Button_Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit_Params: TEdit;
    Label3: TLabel;
    Edit_Name: TEdit;
    Edit_FN: TEdit;
    TB_OpenDlg: TToolbarButton97;
    procedure TB_OpenDlgClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Edit_FNChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
uses
   gf_misc,
   kn_Info,
   kn_Global,
   kn_main,
   kn_FavoritesMng,
   knt.App,
   knt.RS;

{$R *.DFM}



procedure TForm_FavExt.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  App.SetTopMost(Handle, True);
  App.ApplyBiDiModeOnForm(Self);
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
    if ( not FileExists( NormalFN( AbsolutePath(Edit_FN.Text) ))) then
      CanClose := ( messagedlg( GetRS(sFavDlg01), mtWarning, [mbOK,mbCancel], 0 ) = mrOK );
  end;
end;

function TForm_FavExt.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TForm_FavExt.TB_OpenDlgClick(Sender: TObject);
begin
  if Form_Main.OpenDlg.Execute then begin
     Edit_FN.Text := Form_Main.OpenDlg.Filename;
  end;
  TB_OpenDlg.Down:= false;
end;

end.
