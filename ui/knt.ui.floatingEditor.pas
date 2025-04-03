unit knt.ui.floatingEditor;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2025 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.RichEdit,
  System.SysUtils, System.Classes, System.StrUtils, System.Math,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls,

  RxRichEd,
  knt.ui.editor
  ;


type
  TFloatingEditor = class(TForm)
  private
    MovingForm: Boolean;
    LastMousePos: TPoint;

    procedure CreateParams(var Params: TCreateParams); override;

  protected
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  public
    Editor: TKntRichEdit;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AdjustToContent();
    procedure ShowEditor(X, Y: Integer; FontHeight: integer);
    procedure HideEditor;
    procedure HideNestedFloatingEditor;
  end;


implementation

uses
  gf_miscvcl,
  knt.App;



procedure TFloatingEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or WS_THICKFRAME;
  Params.ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow();
end;

constructor TFloatingEditor.Create(AOwner: TComponent);
var
  Pnl: TPanel;
  TopPanel: TPanel;
  BottomBevel: TBevel;

begin
  inherited CreateNew(AOwner, 0);

  BorderStyle := bsSingle;
  BorderWidth := 1;
  FormStyle := fsStayOnTop;
  DoubleBuffered := True;
  Color := clWhite;
  Ctl3D := False;
  Visible := False;

  MovingForm:= false;

  KeyPreview:= True;
  OnActivate:= FormActivate;
  OnKeyDown := FormKeyDown;

  TopPanel := TPanel.Create(AOwner);
  with TopPanel do begin
    Parent := Self;
    Height := 25;
    Align := alTop;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    OnMouseDown := PanelMouseDown;
    OnMouseMove := PanelMouseMove;
    OnMouseUp := PanelMouseUp;
    BottomBevel := TBevel.Create(TopPanel);
    with BottomBevel do begin
      Parent := TopPanel;
      Height := 10;
      Align := alBottom;
      Shape := bsTopLine;
      Color := clGray;
    end;
  end;

  Editor := TKntRichEdit.Create(AOwner);
  with Editor do begin
     Parent := Self;
     Hint := 'Right-click for menu';
     DrawEndPage := False;
     Align := alClient;
     AllowInPlace := False;
     HideSelection := False;
     ParentFont := False;
     UndoLimit := 10;
     WantTabs := True;
     BorderStyle:= bsNone;
     TRxRichEdit(Editor).SetMargins(10, 15);
     PopupMenu := Form_Main.Menu_RTF;
     OnChangedSelection:= Form_Main.RxChangedSelection;
     OnFileDropped := Form_Main.OnFileDropped;
     OnEnter:= App.AuxEditorFocused;
     OnKeyPress:= Form_Main.RxResTabRTFKeyPress;
     OnKeyDown:= Form_Main.RxResTabRTFKeyDown;

     HelpContext:= 282;  // KeyNote Editor [282]

     SetVinculatedObjs(nil, nil, nil, nil);

     PlainText:= False;
     Chrome:= Knt.App.DefaultEditorChrome;
     SupportsRegisteredImages:= True;
     SupportsImages:= True;
   end;
end;

destructor TFloatingEditor.Destroy;
begin
  inherited;
end;

procedure TFloatingEditor.FormActivate(Sender: TObject);
begin
   HideNestedFloatingEditor;
   App.EditorFocused(Self.Editor);
   Editor.SetFocus;
end;


procedure TFloatingEditor.HideEditor;
begin
  HideNestedFloatingEditor;

  Hide();
end;

procedure TFloatingEditor.HideNestedFloatingEditor;
begin
  if Editor.FloatingEditor <> nil then begin
     TFloatingEditor(Editor.FloatingEditor).HideEditor;
     FreeAndNil(Editor.FloatingEditor);
  end;
end;


procedure TFloatingEditor.ShowEditor(X, Y: Integer; FontHeight: integer);
begin
  AdjustToContent;

  if (X + Width) > Screen.WorkAreaRect.Right then
     Left:= X + (Screen.WorkAreaRect.Right -(X + Width))
  else
     Left:= X;

  if (Y + Height + 1.5 * FontHeight) > Screen.WorkAreaRect.Bottom   then
     Top:= Y - Height - 5
  else
     Top:= Y + Round(1.5 * FontHeight);

  if not Visible then
     Show;

  App.EditorFocused(Self.Editor);
end;


procedure TFloatingEditor.AdjustToContent();
var
  FormatRange: TFormatRange;
  DC: HDC;
  NeededWidth, NeededHeight: Integer;
  PWidth, PHeight: Integer;
  FullPageRect: TRect;
const
  DPI = 96;
begin

  DC := GetDC(Editor.Handle);

  with FormatRange do begin
    try
       FillChar(FormatRange, SizeOf(TFormatRange), 0);

       PWidth :=  Round(Form_Main.Width * 0.70);     // Max width
       PHeight := Round(Form_Main.Height * 0.70);    // Max height

       FullPageRect.Top:= 0;
       FullPageRect.Left:= 0;
       FullPageRect.Right := PWidth;
       FullPageRect.Bottom:= PHeight;

       rc.right  := PWidth  * 1440 div DPI;
       rc.bottom := PHeight * 1440 div DPI;
       rcPage := FullPageRect;
       hdcTarget := DC;
       hdc:= DC;
       chrg.cpMax := -1;
       chrg.cpMin := 0;

       SendMessage(Editor.Handle, EM_FORMATRANGE, 0, 0);                     // flush buffer
       SendMessage(Editor.Handle, EM_FORMATRANGE, 1, LPARAM(@FormatRange));
       NeededWidth :=  MulDiv(FormatRange.rc.Right,  DPI, 1440);
       NeededHeight := MulDiv(FormatRange.rc.Bottom, DPI, 1440);            // Current height in this "page" (Twips -> Pixels)

       Self.Width  := NeededWidth;
       Self.Height := NeededHeight + 60;

     finally
       SendMessage(Editor.Handle, EM_FORMATRANGE, 0, 0);
       ReleaseDC(Editor.Handle, DC);
     end;
  end;
end;


procedure TFloatingEditor.PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    MovingForm := True;
    LastMousePos := Point(X, Y);
    Screen.Cursor := crSizeAll;
  end;
end;

procedure TFloatingEditor.PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if MovingForm then begin
    Left := Left + (X - LastMousePos.X);
    Top := Top + (Y - LastMousePos.Y);
  end;
end;

procedure TFloatingEditor.PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MovingForm := False;
  Screen.Cursor := crDefault;
end;

procedure TFloatingEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key of
     VK_ESCAPE: HideEditor;
  end;
end;


initialization

end.
