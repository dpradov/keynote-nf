
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

unit kn_about;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus,
  Clipbrd, ShellAPI, kn_Info,
  gf_misc, kn_NoteObj, TntMenus, TntStdCtrls;

type
  TAboutBox = class(TForm)
    BTN_Close: TSpeedButton;
    NetMenu: TTntPopupMenu;
    CopyEmailaddress1: TTntMenuItem;
    CopyuWebURL1: TTntMenuItem;
    N1: TTntMenuItem;
    Cancel1: TTntMenuItem;
    Panel_Main: TPanel;
    Label_Name: TTntLabel;
    Label_Desc: TTntLabel;
    Label_License: TTntLabel;
    Label9: TTntLabel;
    Label11: TTntLabel;
    Label_URL: TLabel;
    Label_MAILTO: TLabel;
    Label_Dart: TTntLabel;
    Image1: TImage;
    LB_RichEditVer: TTntLabel;
    Label_Credit2: TTntLabel;
    Label_Credit1: TTntLabel;
    Label3: TTntLabel;
    Label4: TTntLabel;
    Label_MAILTO2: TLabel;
    Label6: TTntLabel;
    Label_Version: TLabel;
    Image_Program: TImage;
    Label_Version_Date: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BTN_CloseClick(Sender: TObject);
    procedure Label_MAILTODblClick(Sender: TObject);
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation
uses kn_const, kn_Main;

{$R *.DFM}

var
  TahomaFontInstalled : boolean;

procedure TAboutBox.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
var
  nameDLL: string;
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

  if _LoadedRichEditVersion = 4 then
     nameDLL := ' (MSFTEDIT.DLL)'
  else
     nameDLL:='';

  LB_RichEditVer.Font.Style := [fsBold];
  LB_RichEditVer.Caption := Format(
    'RichEdit DLL ver: %d %s',
    [_LoadedRichEditVersion, nameDLL]
  );

  Caption := 'About - ' + Program_Name;
  Label_Name.Caption := Program_Name;
  Label_Desc.Caption := Program_Desc;

  Label_Credit1.Caption := Program_Credit1;
  Label_Credit2.Caption := Program_Credit2;
  Label_Mailto2.Caption := Program_Email1;
  Label_Mailto.Caption := Program_Email2;
  Label_URL.Caption :=  Program_URL;

  Label_Version.Caption:= 'v.' + Program_Version;
  Label_Version.Left:= Label_Name.Left + Label_Name.Width + 10;

  Label_Version_Date.Caption:= '(' + Program_Version_Date + ')';
  Label_Version_Date.Left:= Label_Version.Left + Label_Version.Width + 10;

  Icon := TIcon.Create;
  try
    Icon.LoadFromResourceName(HInstance, 'MAINICON');
    Image_Program.Picture.Icon:= Icon;
  finally
    Icon.Free;
  end;
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

procedure TAboutBox.Label_URLDblClick(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  ( sender as TLabel ).Font.Color := _GF_BLUE;
  ShellExecute( 0, 'open', PChar( Label_URL.Caption ), nil, nil, SW_NORMAL );
  ( sender as TLabel ).Font.Color := _GF_PURPLE;
  screen.Cursor := crDefault;
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
