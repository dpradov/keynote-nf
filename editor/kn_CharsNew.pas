unit kn_CharsNew;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2024 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
   System.SysUtils,
   System.Classes,
   System.Character,
   Winapi.Windows,
   Winapi.ShellAPI,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ExtCtrls,
   Vcl.Buttons,
   Vcl.Samples.Spin,
   Vcl.Clipbrd,
   RxPlacemnt,
   RxRichEd,
   TB97Ctls;


type
  TCharInsertEvent = procedure(const ch : Char; const Count: integer; const FontName: string; const FontCharset: TFontCharset; Unicode: boolean= true ) of object;

type
  TForm_CharsNew = class(TForm)
    FormPlacement: TFormPlacement;
    EditorTable: TRxRichEdit;
    EditorAux: TRxRichEdit;
    Label3: TLabel;
    lblCode: TLabel;
    Label1: TLabel;
    btnInsert: TButton;
    Button_Close: TButton;
    Spin_Count: TSpinEdit;
    btnCopy: TButton;
    btnInsertNew: TButton;
    txtSelectedChar: TRxRichEdit;
    EditorNewChars: TRxRichEdit;
    lblFontName: TLabel;
    chkAutoAddNew: TCheckBox;
    btnPaste: TButton;
    btnEditTable: TToolbarButton97;
    btnOpenCharmap: TBitBtn;
    btnAddToTable: TToolbarButton97;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnInsertClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button_CloseClick(Sender: TObject);
    procedure btnEditTableClick(Sender: TObject);
    procedure EditorTable_SelectionChange(Sender: TObject);
    procedure btnAddToTableClick(Sender: TObject);
    procedure EditorTableKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOpenCharmapClick(Sender: TObject);
    procedure btnPasteClick(Sender: TObject);
    procedure btnInsertNewClick(Sender: TObject);
    procedure chkAutoAddNewClick(Sender: TObject);
  private
    { Private declarations }

    hCharmap: Cardinal;
    fTableModified: boolean;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateTable;
    procedure SelectChar (IndexNewChar: integer);
    procedure LoadCustomCharacters;
    procedure UpdateSelectedChar;
    function EditorContainsChar(Editor: TRxRichEdit; const ch : Char; const FontName: string; const FontCharset: TFontCharset): boolean;
    procedure CleanEditor (Editor: TRxRichEdit);

  public
    { Public declarations }
    SelectedChar_FontName: string;
    SelectedChar_Charset: integer;
    SelectedChar_Char: Char;

    AutoAddNew: boolean;
    RTFCustomChars: string;

    CharInsertEvent : TCharInsertEvent;
    FormCloseEvent : TNotifyEvent;
  end;

implementation
uses
  gf_misc,
  gf_miscvcl,
  kn_Info,
  kn_Const,
  kn_EditorUtils,
  kn_ClipUtils,
  kn_NoteObj,
  kn_Global;

{$R *.DFM}

resourcestring
  STR_01 = ' Edit';
  STR_02 = ' Done';


var
  IconCharmapBmp: TBitmap;

const
  CharsLastItemIndex : integer = 1;
  CharsLastCount     : integer = 1;

const
  NUM_COLS = 15;
  NUM_ROWS_MIN = 10;

  rowIni    = '\trowd\trgaph0\trleft0';
  rowEnd    = '\row';
  cellsHead = '\clvertalc\cellx%d';
  cellsBody = '\pard\intbl\qc  \cell';


procedure TForm_CharsNew.FormCreate(Sender: TObject);
begin
  with FormPlacement do begin
    UseRegistry := _FORMPOS_USE_REGISTRY;
    IniFileName := _FORMPOS_INIFILENAME;
  end;

  CharInsertEvent := nil;
  FormCloseEvent := nil;
  hCharmap:= 0;

  if IconCharmapBmp = nil then
     IconCharmapBmp:= LoadIconIntoBitmap ('C:\WINDOWS\system32\charmap.exe');

  if IconCharmapBmp <> nil then
     btnOpenCharmap.Glyph.Assign(IconCharmapBmp)
  else
     btnOpenCharmap.Caption:= 'Charmap';

  EditorTable.RemoveMargins;
end;


procedure TForm_CharsNew.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WndParent := _MainFormHandle;
end;


procedure TForm_CharsNew.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  Spin_Count.Value := CharsLastCount;
  chkAutoAddNew.Checked:= AutoAddNew;
  try
     if RTFCustomChars <> '' then
        EditorAux.RtfText:= RTFCustomChars;
  except
  end;

  LoadCustomCharacters;
  SelectChar(CharsLastItemIndex);
  fTableModified:= false;
end;


procedure TForm_CharsNew.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fTableModified then begin
     RTFCustomChars:= EditorAux.RtfText;
     RTFCustomChars:= StringReplace(RTFCustomChars, #13#10, '', [rfReplaceAll]);
     if (RTFCustomChars[Length(RTFCustomChars)] = #0) then
        SetLength(RTFCustomChars,Length(RTFCustomChars)-1);
  end;

  if assigned( FormCloseEvent ) then
    FormCloseEvent( self );

  if hCharmap <> 0 then begin
     KillProcess(hCharmap);
     hCharmap:= 0;
  end;

end;



procedure TForm_CharsNew.CreateTable;
const
  rowIni    = '\trowd\trgaph0\trleft0';
  rowEnd    = '\row';
  cellsHead = '\clvertalc\cellx%d';
  cellsBody = '\pard\intbl\qc  \cell';

var
   str, table, row: string;
   nRows: integer;
   iR, iC: integer;
   cellTwipsReach: integer;

begin
   EditorTable.ReadOnly:= False;
   nRows:= 1 + ((EditorAux.TextLength) div NUM_COLS);
   if nRows < NUM_ROWS_MIN then
      nRows := NUM_ROWS_MIN;
   table:= '';

   for iR:= 0 to  nRows-1 do begin
      row:= rowIni;
      cellTwipsReach:= 400;

      for iC:= 0 to  NUM_COLS -1 do
          row:= row + Format(cellsHead, [cellTwipsReach*(iC+1)]);

      for iC:= 0 to  NUM_COLS-1 do
          row:= row + cellsBody;

      row := row + rowEnd;
      table:= table + row;
   end;

   EditorTable.Clear;

   str:= '{\rtf1\ansi{\fonttbl{\f0\fnil\fcharset0 Calibri;}}' +
         '\pard\fs2\par \pard\fs30 ' +
          table + ' \pard}';

   EditorTable.PutRtfText(str, true, true);
   EditorTable.SelStart:= 0;

   EditorTable.ReadOnly:= True;
end;


procedure TForm_CharsNew.CleanEditor (Editor: TRxRichEdit);
var
   i, p: integer;
   Ch: Char;
begin
  for i:= Editor.TextLength - 1 downto 0 do begin
     Editor.SetSelection(i, i+1, false);
     Ch:= Editor.SelText[1];
     if Ch  in [#$D, #9, #32] then
         Editor.SelText:= '';
  end;
end;


procedure TForm_CharsNew.LoadCustomCharacters;
var
   i, p: integer;
   Ch: Char;
begin
    EditorTable.OnSelectionChange:= nil;
    CreateTable;

    EditorTable.Visible:= True;
    EditorAux.Visible:= False;

    EditorTable.BeginUpdate;
    CleanEditor (EditorAux);

    for i:= 0 to EditorAux.TextLength - 1 do begin
        EditorAux.SetSelection(i,i+1, false);
        Ch:= EditorAux.SelText[1];
        if (Ch = #$D) or (Ch = #9) then continue;

        p:= EditorTable.FindText(#32, 0, -1, []);
        EditorTable.SetSelection(p,p+1, false);
        EditorTable.SelText:= Ch;
        EditorTable.SelAttributes.Name:= EditorAux.SelAttributes.Name;
        EditorTable.SelAttributes.Charset:= EditorAux.SelAttributes.Charset;
    end;

    EditorTable.EndUpdate;

    EditorTable.OnSelectionChange:= EditorTable_SelectionChange;
    EditorTable.OnDblClick:= btnInsertClick;

    fTableModified:= true;
end;


function TForm_CharsNew.EditorContainsChar(Editor: TRxRichEdit; const ch : Char; const FontName: string; const FontCharset: TFontCharset): boolean;
var
  i: integer;
  Str: String;
begin
   Str:= Editor.Text;
   for i:= 0 to Length(Str) - 1 do begin
      if Str[i+1]=ch then begin
         Editor.SetSelection(i,i+1, false);
         if (Editor.SelAttributes.Name = FontName) and (Editor.SelAttributes.Charset = FontCharset) then
            exit(true);
      end;
   end;
   exit(false);
end;



procedure TForm_CharsNew.btnEditTableClick(Sender: TObject);
var
 Enable: boolean;
begin
  if not EditorAux.Visible then begin
     btnEditTable.Caption:= STR_02;
     EditorAux.Visible:= True;
  end
  else begin
     btnEditTable.Caption:= STR_01;
     LoadCustomCharacters;
     SelectChar(1);
  end;

  Enable:= not EditorAux.Visible;
  btnAddToTable.Enabled:= Enable;
  btnInsertNew.Enabled:= Enable;
  btnPaste.Enabled:= Enable;
  btnInsert.Enabled:= Enable;
  Spin_Count.Enabled:= Enable;
end;


procedure TForm_CharsNew.btnAddToTableClick(Sender: TObject);
var
  i: integer;
  ch: Char;
  newChars: boolean;
begin
   btnAddToTable.Down:= False;

   newChars:= false;
   for i:= 0 to EditorNewChars.TextLength - 1 do begin
      Ch:= EditorNewChars.Text[i+1];
      if Ch  in [#$D, #9, #32] then continue;
      EditorNewChars.SetSelection(i, i+1, false);

      if not EditorContainsChar(EditorAux, Ch, EditorNewChars.SelAttributes.Name, EditorNewChars.SelAttributes.Charset) then begin
         EditorAux.SelStart:= EditorAux.TextLength;
         EditorAux.PutRtfText(EditorNewChars.RtfSelText, true, true);
         newChars:= true;
      end;
   end;
   EditorNewChars.SelStart:= 0;

  if newChars then begin
     EditorTable.BeginUpdate;
     LoadCustomCharacters;
     EditorTable.EndUpdate;

     SelectChar(EditorAux.TextLength);
  end;
end;



procedure TForm_CharsNew.btnOpenCharmapClick(Sender: TObject);
var
  Result: LongBool;
  MainWindowHandle: HWND;
  PID: Cardinal;
begin
  Result:= False;
  if hCharmap <> 0 then begin
     PID:= GetProcessId(hCharmap);
     MainWindowHandle := FindMainWindow(PID);
     if MainWindowHandle <> 0 then
        Result:= SetForegroundWindow(MainWindowHandle);
  end;

  if not Result then
     hCharmap := StartApp('open', 'charmap.exe', '', '', sw_normal);
end;


procedure TForm_CharsNew.btnPasteClick(Sender: TObject);
begin
   EditorNewChars.SelectAll;
   EditorNewChars.PasteFromClipboard;
   CleanEditor (EditorNewChars);
end;



procedure TForm_CharsNew.EditorTable_SelectionChange(Sender: TObject);
var
  pS, pL, i, ItemIndex: integer;
  NextCh, Str: String;
begin
  EditorTable.OnSelectionChange:= nil;
  EditorTable.BeginUpdate;
  try
     pS:= EditorTable.SelStart;
     NextCh:= EditorTable.GetTextRange(pS,pS+1);

     pL:= ps;
     if (NextCh = #9) or (NextCh = '') then
        pL:= ps-1;

     EditorTable.SetSelection(pL, pL+2, true);
     if EditorTable.SelLength > 2 then begin
        pL:= ps-2;
        EditorTable.SetSelection(pL, pL+2, true);
     end;

     if EditorTable.SelLength > 2 then begin
        SelectChar(EditorAux.TextLength);
     end;


     ItemIndex:= 0;
     Str:= EditorTable.GetTextRange(0, pL);
     for i:= 1 to Length(Str) do
        if Str[i] in [#9, #$D] then
           ItemIndex:= ItemIndex + 1;

     if (ItemIndex >= 1) and (ItemIndex <= EditorAux.TextLength) then begin
        CharsLastItemIndex:= ItemIndex;
        UpdateSelectedChar;
     end
     else
        SelectChar(CharsLastItemIndex);

 finally
    EditorTable.EndUpdate;
    EditorTable.OnSelectionChange:= EditorTable_SelectionChange;
 end;
end;


procedure TForm_CharsNew.UpdateSelectedChar;
var
   p: integer;
begin
   if (CharsLastItemIndex < 1) or (CharsLastItemIndex > EditorAux.TextLength) then exit;

   EditorAux.SetSelection(CharsLastItemIndex-1,CharsLastItemIndex, false);
   SelectedChar_FontName:= EditorAux.SelAttributes.Name;
   SelectedChar_Char:= EditorAux.Text[CharsLastItemIndex];
   SelectedChar_Charset:= EditorAux.SelAttributes.Charset;

   txtSelectedChar.ReadOnly:= False;
   txtSelectedChar.Clear;
   txtSelectedChar.SelText:= SelectedChar_Char;
   txtSelectedChar.SelAttributes.Name:= SelectedChar_FontName;
   txtSelectedChar.SelAttributes.Charset:= SelectedChar_Charset;
   txtSelectedChar.SelAttributes.Size:= 40;
   txtSelectedChar.Paragraph.Alignment := paCenter;
   txtSelectedChar.SelLength:= 0;
   txtSelectedChar.RemoveMargins;
   txtSelectedChar.ReadOnly:= True;

   lblFontName.Caption:= SelectedChar_FontName;
   lblCode.Caption:= Format('U+%s  (%d)', [IntToHex(Ord(SelectedChar_Char)), Ord(SelectedChar_Char)]);
end;


procedure TForm_CharsNew.SelectChar (IndexNewChar: integer);
var
  iCh, numRow, numCol: integer;
begin
  numRow:= 1 + (IndexNewChar-1) div NUM_COLS;
  numCol:= IndexNewChar mod NUM_COLS;
  if numCol = 0 then numCol:= NUM_COLS;

  iCh:= 1+(numCol * 2) + ( (NUM_COLS*2 + 4) * (numRow-1) );

  EditorTable.OnSelectionChange:= nil;
  EditorTable.SetSelection(iCH, iCh+2, true);
  EditorTable.OnSelectionChange:= EditorTable_SelectionChange;

  CharsLastItemIndex:= IndexNewChar;
  UpdateSelectedChar;
end;


procedure TForm_CharsNew.EditorTableKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  IndexNewChar: integer;

begin
  if ( Shift <> [] ) then exit;

  IndexNewChar:= -999;
  case key of
    VK_LEFT  : IndexNewChar:= CharsLastItemIndex - 1;
    VK_RIGHT : IndexNewChar:= CharsLastItemIndex + 1;
    VK_UP    : IndexNewChar:= CharsLastItemIndex - NUM_COLS;
    VK_DOWN  : IndexNewChar:= CharsLastItemIndex + NUM_COLS;
    13 :
      if ( Shift = [] ) then begin
        key := 0;
        btnInsertClick( btnInsert );
      end;
  end;

  if IndexNewChar <> -999  then begin
     key:= 0;
     if (IndexNewChar >=1) and (IndexNewChar <= EditorAux.TextLength) then
        SelectChar(IndexNewChar);
  end;

end;



procedure TForm_CharsNew.btnInsertClick(Sender: TObject);
begin
  if not assigned( CharInsertEvent ) then exit;

  CharInsertEvent( SelectedChar_Char, Spin_Count.Value, SelectedChar_FontName, SelectedChar_CharSet );

  CharsLastCount := Spin_Count.Value;
  if ( btnInsert.ModalResult <> mrNone ) then
      Close;
end;


procedure TForm_CharsNew.btnInsertNewClick(Sender: TObject);
var
   i: integer;
   Ch: Char;
begin
  if not assigned( CharInsertEvent ) then exit;

  ActiveNote.Editor.BeginUpdate;
  try
     for i:= 1 to EditorNewChars.TextLength do begin
        EditorNewChars.SetSelection(i-1,i,false);
        Ch:= EditorNewChars.Text[i];
        CharInsertEvent( Ch, 1, EditorNewChars.SelAttributes.Name, EditorNewChars.SelAttributes.Charset );
     end;
     EditorNewChars.SelStart:= 0;
     if AutoAddNew then
        btnAddToTableClick (nil);
  finally
     ActiveNote.Editor.EndUpdate;
  end;

  CharsLastCount := Spin_Count.Value;
  if ( btnInsert.ModalResult <> mrNone ) then
      Close;
end;


procedure TForm_CharsNew.btnCopyClick(Sender: TObject);
var
  N: integer;
  s : string;
begin
  if EditorAux.Visible then
    N:= 1
  else begin
    if EditorAux.SelLength<> 1 then exit;
    N:= Spin_Count.Value;
  end;

  if N = 1 then
     EditorAux.CopyToClipboard

  else begin
    s:= StringOfChar(SelectedChar_Char, N);
    EditorAux.SelText:= s;
    EditorAux.CopyToClipboard;
    EditorAux.SelText:= SelectedChar_Char;
  end;

  LogRTFHandleInClipboard();
end;

procedure TForm_CharsNew.chkAutoAddNewClick(Sender: TObject);
begin
   AutoAddNew:= chkAutoAddNew.Checked;
end;



procedure TForm_CharsNew.Button_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm_CharsNew.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
    27 : if ( Shift = [] ) then begin
            key := 0;
            ModalResult := mrCancel;
            Close;
         end;
  end;
end;



initialization
  IconCharmapBmp:= nil;

end.

