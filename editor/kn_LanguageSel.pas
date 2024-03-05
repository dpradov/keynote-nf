unit kn_LanguageSel;

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
   langs,
   LCCombo;

type
  TForm_Lang = class(TForm)
    Button_OK: TButton;
    Button_Cancel: TButton;
    GroupBox1: TGroupBox;
    Combo_Lang: TLanguagesCombo;
    RB_Selected: TRadioButton;
    RB_Recent: TRadioButton;
    RB_Default: TRadioButton;
    RB_System: TRadioButton;
    procedure RB_SelectedClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function FormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    CurrentLang, RecentLang, DefaultLang, SystemLang : TLanguage;
  end;


implementation
uses
 kn_Global;

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

function TForm_Lang.FormHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
   CallHelp:= False;
   ActiveKeyNoteHelp_FormHelp(Command, Data);
end;

end.
