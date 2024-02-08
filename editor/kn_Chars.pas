unit kn_Chars;

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
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Samples.Spin,
   Vcl.Clipbrd,
   RxPlacemnt;


type
  TCharInsertEvent = procedure(const ch : AnsiChar; const Count: integer; const FontName: string; const FontCharset: TFontCharset ) of object;

type
  TForm_Chars = class(TForm)
    Button_Insert: TButton;
    Button_Close: TButton;
    Button_Font: TButton;
    Chars: TListBox;
    FontDlg: TFontDialog;
    Label_Code: TLabel;
    Label1: TLabel;
    Spin_Count: TSpinEdit;
    CheckBox_FullSet: TCheckBox;
    Button_Copy: TButton;
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
uses
  kn_Info;

{$R *.DFM}

const
  CharsLastItemIndex : integer = 0;
  CharsLastCount     : integer = 1;



procedure TForm_Chars.FormCreate(Sender: TObject);
begin
  with FormPlacement do begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;
  myFontChanged := true; // MUST be always true now, alas
  myShowFullSet := false;

  CharInsertEvent := nil;
  FormCloseEvent := nil;

  with Label_Code.Font do begin
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
    27 : if ( Shift = [] ) then begin
            key := 0;
            ModalResult := mrCancel;
            Close;
         end;
  end;
end;

procedure TForm_Chars.BuildCharList;
var
  ch, startch : AnsiChar;
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
  if FontDlg.Execute then begin
    BuildCharList;
    myFontChanged := true;
  end;
end;

procedure TForm_Chars.Button_InsertClick(Sender: TObject);
begin
  if assigned( CharInsertEvent ) then
  begin
    if myFontChanged then
      CharInsertEvent( AnsiString(chars.items[chars.itemindex])[1], Spin_Count.Value, fontdlg.font.name, fontdlg.font.charset )
    else
      CharInsertEvent( AnsiString(chars.items[chars.itemindex])[1], Spin_Count.Value, '', 0 );
  end;
  CharsLastCount := Spin_Count.Value;
  if ( Button_Insert.ModalResult <> mrNone ) then
    Close;
end;

procedure TForm_Chars.CharsClick(Sender: TObject);
var
  ch: AnsiChar;
begin
  // show char code
  //Label_Code.Caption := Format( '%.3d', [Ord( AnsiChar(chars.items[chars.itemindex][1]) )] );
  ch:= AnsiString(chars.items[chars.itemindex])[1];
  Label_Code.Caption := Format( '%.3d', [Ord(ch)] );
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
  s : Ansistring;
begin
  s := '';
  setlength( s, Spin_Count.Value );
  fillchar( s[1], Spin_Count.Value, chars.items[chars.itemindex][1] );
  Clipboard.SetTextBuf( PChar(String(s)));

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
    13 :
      if ( Shift = [] ) then begin
        key := 0;
        Button_InsertClick( Button_Insert );
      end;
  end;
end;

end.

