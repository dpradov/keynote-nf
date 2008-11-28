unit REMain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ClipBrd,
  richprint, RichEdEx,Converters,OleRichEdit, Grids;
  {you don't need the units Converters and OleRichEdit if you only want the RTF format
  see also the two versions of:
  procedure TMainForm.FileOpen(Sender: TObject);
  procedure TMainForm.FileSaveAs(Sender: TObject);
  This present version can only be compiled with Delphi3 and above
  I do have a simpler version available which works in conjuction with Delhi2}

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditUndoItem: TMenuItem;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    HelpContentsItem: TMenuItem;
    HelpSearchItem: TMenuItem;
    HelpHowToUseItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    SpeedBar: TPanel;
    OpenButton: TSpeedButton;
    SaveButton: TSpeedButton;
    PrintButton: TSpeedButton;
    UndoButton: TSpeedButton;
    CutButton: TSpeedButton;
    CopyButton: TSpeedButton;
    PasteButton: TSpeedButton;
    Ruler: TPanel;
    Bevel1: TBevel;
    FontDialog1: TFontDialog;
    FirstInd: TLabel;
    LeftInd: TLabel;
    RulerLine: TBevel;
    RightInd: TLabel;
    N5: TMenuItem;
    miEditFont: TMenuItem;
    BoldButton: TSpeedButton;
    FontName: TComboBox;
    ItalicButton: TSpeedButton;
    LeftAlign: TSpeedButton;
    CenterAlign: TSpeedButton;
    RightAlign: TSpeedButton;
    UnderlineButton: TSpeedButton;
    BulletsButton: TSpeedButton;
    StatusBar: TStatusBar;
    FontSize: TEdit;
    UpDown1: TUpDown;
    pageDialog1: TMenuItem;
    PrintPreview1: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    N6: TMenuItem;
    Objects1: TMenuItem;
    InsertObjects1: TMenuItem;
    RichPrinter1: TRichPrinter;
    RichEdit1: TRichEditEx;

    procedure SelectionChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
   { procedure FileSave(Sender: TObject);}
    procedure FileSaveAs(Sender: TObject);
    procedure FilePrint(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure HelpContents(Sender: TObject);
    procedure HelpSearch(Sender: TObject);
    procedure HelpHowToUse(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure SelectFont(Sender: TObject);
    procedure RulerResize(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure AlignButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RulerItemMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RightIndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pageDialog1Click(Sender: TObject);
    procedure PrintPreview1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure InsertObjects1Click(Sender: TObject);
  private
    FFileName: String;
    FUpdating: Boolean;
    FDragOfs: Integer;
    FDragging: Boolean;
    function CurrText: TTextAttributes;
    procedure GetFontNames;
    procedure SetFileName(const FileName: String);
    procedure CheckFileSave;
    procedure SetupRuler;
    procedure SetEditRect;
  end;

var
  MainForm: TMainForm;

implementation

uses REAbout;

const
  RulerAdj = 4/3;
  GutterWid = 6;

{$R *.DFM}

procedure TMainForm.SelectionChange(Sender: TObject);
begin
  with RichEdit1.Paragraph do
  try
    FUpdating := True;
    FirstInd.Left := Trunc(FirstIndent*RulerAdj)-4+GutterWid;
    LeftInd.Left := Trunc((LeftIndent+FirstIndent)*RulerAdj)-4+GutterWid;
    RightInd.Left := Ruler.ClientWidth-6-Trunc((RightIndent+GutterWid)*RulerAdj);
    BoldButton.Down := fsBold in RichEdit1.SelAttributes.Style;
    ItalicButton.Down := fsItalic in RichEdit1.SelAttributes.Style;
    UnderlineButton.Down := fsUnderline in RichEdit1.SelAttributes.Style;
    BulletsButton.Down := Boolean(Numbering);
    FontSize.Text := IntToStr(RichEdit1.SelAttributes.Size);
    FontName.Text := RichEdit1.SelAttributes.Name;
    case Ord(Alignment) of
      0: LeftAlign.Down := True;
      1: RightAlign.Down := True;
      2: CenterAlign.Down := True;
    end;
  finally
    FUpdating := False;
  end;
end;

function TMainForm.CurrText: TTextAttributes;
begin
  if RichEdit1.SelLength > 0 then
    Result := RichEdit1.SelAttributes
  else
    Result := RichEdit1.DefAttributes;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure TMainForm.GetFontNames;
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontName.Items));
  ReleaseDC(0, DC);
  FontName.Sorted := True;
end;

procedure TMainForm.SetFileName(const FileName: String);
begin
  FFileName := FileName;
  Caption := Format('%s - %s', [ExtractFileName(FileName), Application.Title]);
end;

procedure TMainForm.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not RichEdit1.Modified then Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
  case SaveResp of
    idYes: FileSaveAS(Self);
    idNo: {Nothing};
    idCancel: Abort;
  end;
end;

procedure TMainForm.SetupRuler;
var
  I: Integer;
  S: String;
begin
  SetLength(S, 201);
  I := 1;
  while I < 200 do
  begin
    S[I] := #9;
    S[I+1] := '|';
    Inc(I, 2);
  end;
  Ruler.Caption := S;
end;

procedure TMainForm.SetEditRect;
var
  R: TRect;
begin
  with RichEdit1 do
  begin
    R := Rect(GutterWid, 0, ClientWidth-GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

{ Event Handlers }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SetFileName('Untitled');
  GetFontNames;
  SetupRuler;
  SelectionChange(Self);
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  StatusBar.SimpleText := Application.Hint;
end;

procedure TMainForm.FileNew(Sender: TObject);
begin
  SetFileName('Untitled');
  RichEdit1.Lines.Clear;
  RichEdit1.Modified := False;
end;

// Version for only RTF format
(*procedure TMainForm.FileOpen(Sender: TObject);
begin
  CheckFileSave;
  if OpenDialog.Execute then
  begin
    RichEdit1.Lines.LoadFromFile(OpenDialog.FileName);
    SetFileName(OpenDialog.FileName);
    RichEdit1.SetFocus;
    RichEdit1.Modified := False;
    RichEdit1.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;*)

// Version for various other formats then the RTF format
procedure TMainForm.FileOpen(Sender: TObject);
var
  StrLst: TStringList;
  SelectedFormat: String;

begin
  StrLst := TStringList.Create;

  try
    // Search for all available filters and add them to the filter of your dialog.
    OpenDialog.Filter := BuildFilterList(True);
    OpenDialog.FilterIndex:=2; {RTF format}
    // If we have selected a file, import it.
    if OpenDialog.Execute then
      begin
        BuildConverterList(True, StrLst);
        SelectedFormat := StrLst.Strings[OpenDialog.FilterIndex - 1]; // - 1 while FilterIndex starts at 1.

        StatusBar.SimpleText := 'Loading file...';
        Screen.Cursor:=crHourGlass;
       if Pos('Rich Text', SelectedFormat) > 0 then
          begin
            // Native format. Load directly, without any converter.
            RichEdit1.Lines.LoadFromFile(OpenDialog.FileName);
            StatusBar.SimpleText := 'Done';
            MainForm.Caption:=OpenDialog.FileName;
            Exit;
          end;

        if ImportAsRTF(OpenDialog.FileName, SelectedFormat, TOLEEdit(RichEdit1)) then
          StatusBar.SimpleText := 'Succesfully imported: ' + OpenDialog.FileName;
        RichEdit1.SetFocus;
        RichEdit1.Modified := False;
        RichEdit1.ReadOnly := ofReadOnly in OpenDialog.Options;
      end;
  finally
    Screen.Cursor:=crDefault;
    StrLst.Free;
  end;
end;

 // Version for only RTF format
(*procedure TMainForm.FileSaveAs(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    if FileExists(SaveDialog.FileName) then
      if MessageDlg(Format('OK to overwrite %s', [SaveDialog.FileName]),
        mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
    RichEdit1.Lines.SaveToFile(SaveDialog.FileName);
    SetFileName(SaveDialog.FileName);
    RichEdit1.Modified := False;
  end;
end;*)
// Version for various other formats then the RTF format
procedure TMainForm.FileSaveAs(Sender: TObject);
var
  StrLst: TStringList;
  SelectedFormat: String;
begin
  StrLst := TStringList.Create;
  try
    // Search for all available filters and add them to the filter of your dialog.
    SaveDialog.Filter := BuildFilterList(False);
    SaveDialog.FilterIndex:=2; {RTF format}
    // If we have selected a filename, export to it.
    if SaveDialog.Execute then
      begin
        BuildConverterList(False, StrLst);
        SelectedFormat := StrLst.Strings[SaveDialog.FilterIndex - 1]; // - 1 while FilterIndex starts at 1.

        StatusBar.SimpleText := 'Exporting file...';

        Screen.Cursor:=crHourGlass;
        if Pos('Rich Text', SelectedFormat) > 0 then
          begin
            // Native format. Save directly, without any converter.
            RichEdit1.Lines.SaveToFile(SaveDialog.FileName);
            StatusBar.SimpleText := 'Done';
            Exit;
          end;

        if ExportRTF(SaveDialog.FileName, SelectedFormat, TOLEEDIT(RichEdit1)) then
          StatusBar.SimpleText := 'Succesfully exported: ' + SaveDialog.FileName;
        RichEdit1.Modified := False;
      end;
  finally
    StrLst.Free;
    Screen.Cursor:=crDefault;
  end;
end;



procedure TMainForm.FilePrint(Sender: TObject);
begin
 if PrintDialog.Execute then RichPrinter1.PrintRichEdit(RichEdit1,1);
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditUndo(Sender: TObject);
begin
  with RichEdit1 do
    if HandleAllocated then SendMessage(Handle, EM_UNDO, 0, 0);
end;

procedure TMainForm.EditCut(Sender: TObject);
begin
  RichEdit1.CutToClipboard;
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

procedure TMainForm.EditPaste(Sender: TObject);
begin
  RichEdit1.PasteFromClipboard;
end;

procedure TMainForm.HelpContents(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TMainForm.HelpSearch(Sender: TObject);
const
  EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

procedure TMainForm.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  with TAboutBox.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainForm.SelectFont(Sender: TObject);
begin
  FontDialog1.Font.Assign(RichEdit1.SelAttributes);
  if FontDialog1.Execute then
    CurrText.Assign(FontDialog1.Font);
  RichEdit1.SetFocus;
end;

procedure TMainForm.RulerResize(Sender: TObject);
begin
  RulerLine.Width := Ruler.ClientWidth - (RulerLine.Left*2);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TMainForm.BoldButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if BoldButton.Down then
    CurrText.Style := CurrText.Style + [fsBold]
  else
    CurrText.Style := CurrText.Style - [fsBold];
end;

procedure TMainForm.ItalicButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if ItalicButton.Down then
    CurrText.Style := CurrText.Style + [fsItalic]
  else
    CurrText.Style := CurrText.Style - [fsItalic];
end;

procedure TMainForm.FontSizeChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Size := StrToInt(FontSize.Text);
end;

procedure TMainForm.AlignButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  RichEdit1.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TMainForm.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  CurrText.Name := FontName.Items[FontName.ItemIndex];
end;

procedure TMainForm.UnderlineButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  if UnderlineButton.Down then
    CurrText.Style := CurrText.Style + [fsUnderline]
  else
    CurrText.Style := CurrText.Style - [fsUnderline];
end;

procedure TMainForm.BulletsButtonClick(Sender: TObject);
begin
  if FUpdating then Exit;
  RichEdit1.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

{ Ruler Indent Dragging }

procedure TMainForm.RulerItemMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragOfs := (TLabel(Sender).Width div 2);
  TLabel(Sender).Left := TLabel(Sender).Left+X-FDragOfs;
  FDragging := True;
end;

procedure TMainForm.RulerItemMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDragging then
    TLabel(Sender).Left :=  TLabel(Sender).Left+X-FDragOfs
end;

procedure TMainForm.FirstIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.FirstIndent := Trunc((FirstInd.Left+FDragOfs-GutterWid) / RulerAdj);
  LeftIndMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TMainForm.LeftIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.LeftIndent := Trunc((LeftInd.Left+FDragOfs-GutterWid) / RulerAdj)-RichEdit1.Paragraph.FirstIndent;
  SelectionChange(Sender);
end;

procedure TMainForm.RightIndMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
  RichEdit1.Paragraph.RightIndent := Trunc((Ruler.ClientWidth-RightInd.Left+FDragOfs-2) / RulerAdj)-2*GutterWid;
  SelectionChange(Sender);
end;

procedure TMainForm.pageDialog1Click(Sender: TObject);
begin
 RichPrinter1.PageDialog;
end;

procedure TMainForm.PrintPreview1Click(Sender: TObject);
begin
 RichPrinter1.PrintRichEditPreview(RichEdit1);
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
 PrintPreview1Click(Sender);
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
 pageDialog1Click(Sender);
end;



procedure TMainForm.InsertObjects1Click(Sender: TObject);
begin
 RichEdit1.InsertObject;
end;

end.
