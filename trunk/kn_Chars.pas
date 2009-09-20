
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

unit kn_Chars;

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, clipbrd, kn_Info, Placemnt, TntStdCtrls;


type
  TCharInsertEvent = procedure( const ch : char; const Count : integer; const FontName : string; const FontCharset : TFontCharset ) of object;

type
  TForm_Chars = class(TForm)
    Button_Insert: TTntButton;
    Button_Close: TTntButton;
    Button_Font: TTntButton;
    Chars: TListBox;
    FontDlg: TFontDialog;
    Label_Code: TTntLabel;
    Label1: TTntLabel;
    Spin_Count: TSpinEdit;
    CheckBox_FullSet: TTntCheckBox;
    Button_Copy: TTntButton;
    FormPlacement: TFormPlacement;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button_FontClick(Sender: TObject);
    procedure Button_InsertClick(Sender: TObject);
    procedure CharsClick(Sender: TObject);
    procedure CharsDblClick(Sender: TObject);
    procedure CheckBox_FullSetClick(Sender: TObject);
    procedure Button_CopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_CloseClick(Sender: TObject);
    procedure CharsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    myFontChanged : boolean;
    myShowFullSet : boolean;
    myLastCharCode : integer;

    CharInsertEvent : TCharInsertEvent;
    FormCloseEvent : TNotifyEvent;

    procedure BuildCharList;

  end;

implementation

{$R *.DFM}

const
  CharsLastItemIndex : integer = 0;
  CharsLastCount     : integer = 1;



procedure TForm_Chars.FormCreate(Sender: TObject);
begin
  with FormPlacement do
  begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;
  myFontChanged := true; // MUST be always true now, alas
  myShowFullSet := false;

  CharInsertEvent := nil;
  FormCloseEvent := nil;

  with Label_Code.Font do
  begin
    Name := 'Courier New';
    Size := 18;
    Style := [fsBold];
    // Color := clHighlight;
  end;
end;

procedure TForm_Chars.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  BuildCharList;
  Spin_Count.Value := CharsLastCount;
  CheckBox_FullSet.Checked := myShowFullSet;
  CheckBox_FullSet.OnClick := CheckBox_FullSetClick;
end;                 

procedure TForm_Chars.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    27 : if ( Shift = [] ) then
    begin
      key := 0;
      ModalResult := mrCancel;
      Close;
    end;
  end;
end;

procedure TForm_Chars.BuildCharList;
var
  ch, startch : char;
  olditemindex : integer;
begin

  chars.Items.BeginUpdate;
  Caption := FontDlg.Font.Name;

  olditemindex := CharsLastItemIndex; // chars.itemindex;
  if ( olditemindex < 0 ) then
    olditemindex := 0;

  try
    chars.Clear;
    with chars.Font do
    begin
     Name := FontDlg.Font.Name;
     Charset := FontDlg.Font.Charset;
     Size := FontDlg.Font.Size;
    end;

    if myShowFullSet then
      startch := #33
    else
      startch := #128;

    for ch := startch to #255 do
      chars.items.add( ch );

    if ( olditemindex < chars.items.count ) then
      chars.itemindex := olditemindex
    else
      chars.itemindex := 0;

    CharsClick( chars );

  finally
    chars.Items.EndUpdate;
  end;
end; // BuildCharList

procedure TForm_Chars.Button_FontClick(Sender: TObject);
begin
  if FontDlg.Execute then
  begin
    BuildCharList;
    myFontChanged := true;
  end;
end;

procedure TForm_Chars.Button_InsertClick(Sender: TObject);
begin
  if assigned( CharInsertEvent ) then
  begin
    if myFontChanged then
      CharInsertEvent( chars.items[chars.itemindex][1], Spin_Count.Value, fontdlg.font.name, fontdlg.font.charset )
    else
      CharInsertEvent( chars.items[chars.itemindex][1], Spin_Count.Value, '', 0 );
  end;
  CharsLastCount := Spin_Count.Value;
  if ( Button_Insert.ModalResult <> mrNone ) then
    Close;
end;

procedure TForm_Chars.CharsClick(Sender: TObject);
begin
  // show char code
  Label_Code.Caption := Format( '%.3d', [ord( chars.items[chars.itemindex][1] )] );
  CharsLastItemIndex := chars.ItemIndex;
end;

procedure TForm_Chars.CharsDblClick(Sender: TObject);
begin
  Button_InsertClick( Button_Insert );
end;

procedure TForm_Chars.CheckBox_FullSetClick(Sender: TObject);
begin
  myShowFullSet := CheckBox_FullSet.Checked;
  CharsLastItemIndex := 0;
  BuildCharList;
end;

procedure TForm_Chars.Button_CopyClick(Sender: TObject);
var
  s : string;
begin
  s := '';
  setlength( s, Spin_Count.Value );
  fillchar( s[1], Spin_Count.Value, chars.items[chars.itemindex][1] );
  Clipboard.SetTextBuf( PChar( s ));

end;

procedure TForm_Chars.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end; // CreateParams
            

procedure TForm_Chars.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if assigned( FormCloseEvent ) then
    FormCloseEvent( self );
end;

procedure TForm_Chars.Button_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm_Chars.CharsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    13 : if ( Shift = [] ) then
    begin
      key := 0;
      Button_InsertClick( Button_Insert );
    end;
  end;
end;

end.

