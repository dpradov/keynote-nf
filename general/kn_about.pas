unit kn_about;

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
   Winapi.ShellAPI,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.ExtCtrls,
   Vcl.StdCtrls,
   Vcl.Buttons,
   Vcl.Menus,
   Vcl.Clipbrd
   ;


type
  TAboutBox = class(TForm)
    BTN_Close: TSpeedButton;
    NetMenu: TPopupMenu;
    CopyEmailaddress1: TMenuItem;
    CopyuWebURL1: TMenuItem;
    N1: TMenuItem;
    Cancel1: TMenuItem;
    Panel_Main: TPanel;
    Label_Name: TLabel;
    Label_Desc: TLabel;
    Label_License: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label_URL: TLabel;
    Label_MAILTO: TLabel;
    Label_Dart: TLabel;
    Image1: TImage;
    LB_RichEditVer: TLabel;
    Label_Credit2: TLabel;
    Label_Credit1: TLabel;
    Label_MAILTO2: TLabel;
    Label6: TLabel;
    Label_Version: TLabel;
    Image_Program: TImage;
    Label_Version_Date: TLabel;
    Label_KeynoteNF: TLabel;
    lblDonations: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTN_CloseClick(Sender: TObject);
    procedure Label_MAILTODblClick(Sender: TObject);
    procedure OpenURL(URL: String; Lbl: TLabel);
    procedure Label_URLDblClick(Sender: TObject);
    procedure CopyEmailaddress1Click(Sender: TObject);
    procedure CopyuWebURL1Click(Sender: TObject);
    procedure Label_MAILTOMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label_MAILTOMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Image1DblClick(Sender: TObject);
    procedure lblDonationsClick(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses
   RxRichEd,
   gf_miscvcl,
   kn_Main,
   kn_const,
   kn_Info,
   kn_Global,
   kn_KntFolder,
   knt.RS;


{$R *.DFM}


var
  TahomaFontInstalled : boolean;

procedure TAboutBox.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
var
  nameDLL, pathDLL: string;
  VersionDLL: Single;
  VersionRichEdit: TRichEditVersion;

  Icon: TIcon;

begin

  if TahomaFontInstalled then
    Self.Font.Name := 'Tahoma';

  Panel_Main.Color := _GF_CLWINDOW;
  Label_MAILTO.Font.Color := _GF_PURPLE;
  Label_MAILTO.Font.Style := [fsUnderline];
  Label_MAILTO2.Font.Color := _GF_PURPLE;
  Label_MAILTO2.Font.Style := [fsUnderline];
  Label_URL.Font.Color := _GF_PURPLE;
  Label_URL.Font.Style := [fsUnderline];
  Label_License.Font.Color := _GF_BLACK;
  Label_Dart.Font.Color := _GF_NAVY;
  Label_KeynoteNF.Font.Color := _GF_NAVY;
  lblDonations.Hint:= Hint_Support;


  GetDLLProductVersion(pathDLL, VersionDLL, VersionRichEdit);

  nameDLL := ' (' + ExtractFileName(pathDLL) + ')';

  LB_RichEditVer.Font.Style := [fsBold];
  LB_RichEditVer.Caption := Format(
    'RichEdit DLL ver: %.1f %s',
    [VersionDLL, nameDLL]
  );
  LB_RichEditVer.Hint:= pathDLL;

  Caption := sAB00 + Program_Name;
  Label_Name.Caption := Program_Name;
  Label_Desc.Caption := Program_Desc;

  Label_License.Caption := Program_License;

  Label_Credit1.Caption := Program_Credit1;
  Label_Credit2.Caption := Program_Credit2;
  Label_Mailto2.Caption := Program_Email1;
  Label_Mailto.Caption := Program_Email2;
  Label_Mailto.Hint:=  sAB01;
  Label_Mailto2.Hint:= sAB01;
  Label_URL.Caption :=  Program_URL;
  Label_URL.Hint := sAB02;
  Label_Dart.Caption := sAB03;
  Label_KeyNoteNF.Caption := sAB04;

  Label_Version.Caption:= 'v.' + Program_Version;
  Label_Version.Left:= Label_Name.Left + Label_Name.Width + 10;

  Label_Version_Date.Caption:= '(' + Program_Version_Date + ')';
  Label_Version_Date.Left:= Label_Version.Left + Label_Version.Width + 10;

  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(HInstance, 'MAINICON');
    Image_Program.Picture.Bitmap.Assign(Icon);
  finally
    Icon.Free;
  end;

  LoadGifFromResource(Image1, 'LOGO_DELPHI');
end;

function TAboutBox.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

procedure TAboutBox.BTN_CloseClick(Sender: TObject);
begin
  Close;
end;


procedure TAboutBox.Label_MAILTODblClick(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  ( sender as TLabel ).Font.Color := _GF_BLUE;
  ShellExecute( 0, 'open', PChar( 'mailto:' + ( sender as TLabel ).Caption ), nil, nil, SW_NORMAL );
  ( sender as TLabel ).Font.Color := _GF_PURPLE;
  screen.Cursor := crDefault;
end;

procedure TAboutBox.OpenURL(URL: String; Lbl: TLabel);
begin
  screen.Cursor := crHourGlass;
  //Lbl.Font.Color := _GF_BLUE;
  ShellExecute( 0, 'open', PChar( URL ), nil, nil, SW_NORMAL );
  //Lbl.Font.Color := _GF_PURPLE;
  screen.Cursor := crDefault;
end;

procedure TAboutBox.Label_URLDblClick(Sender: TObject);
begin
  OpenURL (Label_URL.Caption, (sender as TLabel));
end;

procedure TAboutBox.lblDonationsClick(Sender: TObject);
begin
  OpenURL (Program_URL + Program_URL_Donations, (sender as TLabel));
end;

procedure TAboutBox.CopyEmailaddress1Click(Sender: TObject);
begin
  Clipboard.SetTextBuf( PChar( Label_MAILTO.Caption ));
end;

procedure TAboutBox.CopyuWebURL1Click(Sender: TObject);
begin
  Clipboard.SetTextBuf( PChar( Label_URL.Caption ));
end;

procedure TAboutBox.Label_MAILTOMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ( sender as TLabel ).Font.Color := _GF_BLUE;
end;

procedure TAboutBox.Label_MAILTOMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ( sender as TLabel ).Font.Color := _GF_PURPLE;
end;


procedure TAboutBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( shift = [] ) then
    begin
      key := 0;
      Close;
    end;
  end;
end; // KEY DOWN

procedure TAboutBox.Image1DblClick(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  ShellExecute( 0, 'open', 'http://www.embarcadero.com', nil, nil, SW_NORMAL );
  screen.Cursor := crDefault;
end;

end.
