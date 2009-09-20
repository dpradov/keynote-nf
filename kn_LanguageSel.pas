unit kn_LanguageSel;
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
 <marekjed@pobox.com> (Poland).
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, langs, LCCombo, TntStdCtrls;

type
  TForm_Lang = class(TForm)
    Button_OK: TTntButton;
    Button_Cancel: TTntButton;
    GroupBox1: TTntGroupBox;
    Combo_Lang: TLanguagesCombo;
    RB_Selected: TTntRadioButton;
    RB_Recent: TTntRadioButton;
    RB_Default: TTntRadioButton;
    RB_System: TTntRadioButton;
    procedure RB_SelectedClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentLang, RecentLang, DefaultLang, SystemLang : TLanguage;
  end;


implementation

{$R *.DFM}

procedure TForm_Lang.RB_SelectedClick(Sender: TObject);
begin
  if RB_Selected.Checked then
    Combo_Lang.Language := CurrentLang
  else
  if RB_Recent.Checked then
    Combo_Lang.Language := RecentLang
  else
  if RB_Default.Checked then
    Combo_Lang.Language := DefaultLang
  else
  if RB_System.Checked then
    Combo_Lang.Language := SystemLang;
end;

procedure TForm_Lang.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  {
  RB_Recent.Caption := RB_Recent.Caption + ' ' + LanguageName( RecentLang );
  RB_Default.Caption := RB_Default.Caption + ' ' + LanguageName( DefaultLang );
  RB_System.Caption := RB_System.Caption + ' ' + LanguageName( SystemLang );
  }
end; // Activate

procedure TForm_Lang.FormCreate(Sender: TObject);
begin
  RecentLang := GetSystemDefaultLCID;
  DefaultLang := GetSystemDefaultLCID;
  SystemLang := GetSystemDefaultLCID;
  CurrentLang := GetSystemDefaultLCID;
end;

end.
