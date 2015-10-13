{ GF ver 1.0 }
unit GFTipDlgForm;
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
{$I gf_base.inc}

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, gf_misc, gf_miscvcl;

type
  TGFTipForm = class(TForm)
    TipPanel: TPanel;
    ShowChk: TCheckBox;
    Button_OK: TButton;
    Button_Next: TButton;
    Button_Prev: TButton;
    TipLbl: TLabel;
    Image_Hint: TImage;
    TipTitleLbl: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button_NextClick(Sender: TObject);
    procedure Button_PrevClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    Tips : TStrings;
    CurrentTip : integer;
    CRLF : string;
    IsRandom : boolean;
    OriginalCaptionText : string;
  end;

var
  GFTipForm: TGFTipForm;

implementation

{$R *.DFM}

procedure TGFTipForm.FormCreate(Sender: TObject);
begin
  if _TahomaFontInstalled then
    Font.Name := 'Tahoma';
  Tips := TStringList.Create;
end;

procedure TGFTipForm.FormDestroy(Sender: TObject);
begin
  Tips.Free;
end;

procedure TGFTipForm.Button_NextClick(Sender: TObject);
var
  i : integer;
begin
  if IsRandom then
  begin
    Randomize;
    i := Round(Random(Tips.Count-1));
    if (i = CurrentTip) and (Tips.Count > 1) then Inc(i);
      CurrentTip := i;
  end
  else
  begin
    if CurrentTip < Tips.Count-1 then
      Inc(CurrentTip)
    else
      CurrentTip := 0;
  end;
  TipLbl.Caption := Tips[CurrentTip];

  Caption := Format( '%s - %d of %d', [OriginalCaptionText,CurrentTip+1,Tips.Count] );
end;

procedure TGFTipForm.Button_PrevClick(Sender: TObject);
begin
  if CurrentTip = 0 then
    CurrentTip := Tips.Count-1
  else
    Dec(CurrentTip);
  TipLbl.Caption := Tips[CurrentTip];
  Caption := Format( '%s - %d of %d', [OriginalCaptionText,CurrentTip+1,Tips.Count] );
end;

procedure TGFTipForm.Button_OKClick(Sender: TObject);
begin
  Close;
end;

procedure TGFTipForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ( key = 27 ) and ( shift = [] ) then
  begin
    key := 0;
    Close;
  end;
end;

end.
