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
    TopPanel: TPanel;
    TopAuxPanel: TPanel;
    MovingForm: Boolean;
    LastMousePos: TPoint;

    procedure CreateParams(var Params: TCreateParams); override;

  protected
    FParentEditor: TKntRichEdit;
    FSelStartInParentEditor: integer;

    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  public
    Editor: TKntRichEdit;

    constructor Create(AOwner: TComponent; ParentEdit: TKntRichEdit);
    destructor Destroy; override;

    property SelStartInParentEditor: integer read FSelStartInParentEditor;

    procedure AdjustToContent();
    procedure ShowEditor(X, Y: Integer; FontHeight: integer);
    procedure HideEditor;
    procedure HideNestedFloatingEditor;

    procedure SaveChangesToParentEditor;
  end;


implementation

uses
  gf_miscvcl,
  kn_MacroMng,
  knt.ui.TagMng,
  knt.ui.tagSelector,
  knt.App;



procedure TFloatingEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or WS_THICKFRAME;
  Params.ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow();
end;

constructor TFloatingEditor.Create(AOwner: TComponent; ParentEdit: TKntRichEdit);
var
  BottomBevel: TBevel;

begin
  inherited CreateNew(AOwner, 0);

  FParentEditor:= ParentEdit;
  FSelStartInParentEditor:= ParentEdit.SelStart;        // For security... (in case another folder is selected when this floating editor cannot be saved, because it contains tables with nested cells)

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
  OnKeyUp := Form_Main.FormKeyUp;
  OnShortCut:= Form_Main.FormShortCut;
  OnHelp:= Form_Main.FormHelp;

  TopPanel := TPanel.Create(AOwner);
  with TopPanel do begin
    Parent:= Self;
    Align := alTop;
    Height := 20;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    OnMouseDown := PanelMouseDown;
    OnMouseMove := PanelMouseMove;
    OnMouseUp := PanelMouseUp;
  end;

  TopAuxPanel := TPanel.Create(AOwner);
  with TopAuxPanel do begin
    Parent := Self;
    Height := 10;
    Align := alTop;
    BevelOuter := bvNone;
    BevelInner := bvNone;
    StyleElements:= [];
    BottomBevel := TBevel.Create(TopPanel);
    with BottomBevel do begin
      Parent := TopPanel;
      Height := 2;
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

     HelpContext:= 20;  // KeyNote Editor

     ParentEditor:= ParentEdit;
     SetVinculatedObjs(nil, nil, nil, nil);

     PlainText:= ParentEditor.PlainText;
     Chrome:= Knt.App.DefaultEditorChrome;
     SupportsRegisteredImages:= True;
     SupportsImages:= True;
     SetLangOptions(False);
   end;
end;

destructor TFloatingEditor.Destroy;
begin
  inherited;
end;

procedure TFloatingEditor.FormActivate(Sender: TObject);
begin
   FloatingEditorCannotBeSaved:= False;
   Form_Main.ActiveControl:= nil;
   HideNestedFloatingEditor;
   if FloatingEditorCannotBeSaved and (Editor.FloatingEditor <> nil) then
      TFloatingEditor(Editor.FloatingEditor).SetFocus

   else begin
      App.EditorFocused(Self.Editor);
      Editor.SetFocus;  
   end;
end;


procedure TFloatingEditor.HideEditor;
begin
  TagMng.FreeTagSelector;
  HideNestedFloatingEditor;
  if not FloatingEditorCannotBeSaved then begin
     fParentEditor.SaveChangesFromFloatingEditor(True);       // True -> HidingFloatingEditor: True
     if not FloatingEditorCannotBeSaved then
        Hide();
  end;
end;

procedure TFloatingEditor.HideNestedFloatingEditor;
begin
  if Editor.FloatingEditor <> nil then begin
     TFloatingEditor(Editor.FloatingEditor).HideEditor;
     if not FloatingEditorCannotBeSaved then
        FreeAndNil(Editor.FloatingEditor);
  end;
end;

procedure TFloatingEditor.SaveChangesToParentEditor;
begin
    FloatingEditorCannotBeSaved:= False;
    if Editor.FloatingEditor <> nil then
       TFloatingEditor(Editor.FloatingEditor).SaveChangesToParentEditor;    // Recursive

    if not FloatingEditorCannotBeSaved then
       fParentEditor.SaveChangesFromFloatingEditor(False);    // HidingFloatingEditor: False
end;


procedure TFloatingEditor.ShowEditor(X, Y: Integer; FontHeight: integer);
var
  Monitor: TMonitor;
  MonitorWorkArea: TRect;
begin
  AdjustToContent;

  Monitor := Screen.MonitorFromPoint(Point(X, Y));
  MonitorWorkArea := Monitor.WorkareaRect;

  if (X + Width) > MonitorWorkArea.Right then
     Left:= X + (MonitorWorkArea.Right -(X + Width))
  else
     Left:= X;

  if (Y + Height + 1.5 * FontHeight) > MonitorWorkArea.Bottom   then
     Top:= Y - Height - 5
  else
     Top:= Y + Round(1.5 * FontHeight);

  if Top < 5 then
     Top:= 5;

  if not Visible then
     Show;

  TopAuxPanel.Color:= Editor.Color;
  Self.Editor.Refresh;
  App.EditorFocused(Self.Editor);
end;


procedure TFloatingEditor.AdjustToContent();
var
  FormatRange: TFormatRange;
  DC: HDC;
  NeededWidth, NeededHeight: Integer;
  PWidth, PHeight: Integer;
  FullPageRect: TRect;
  ZoomRatio, InvZoomRatio: Single;

const
  DPI = 96;
begin
  DC := GetDC(Editor.Handle);

  ZoomRatio:= Editor.ZoomGoal/100;
  InvZoomRatio:= 1/ZoomRatio;

  with FormatRange do begin
    try
       FillChar(FormatRange, SizeOf(TFormatRange), 0);

       PWidth :=  Round(Form_Main.Width * 0.70);     // Max width
       PHeight := Round(Form_Main.Height * 0.75);    // Max height

       FullPageRect.Top:= 0;
       FullPageRect.Left:= 0;
       FullPageRect.Right := Round(PWidth * InvZoomRatio);
       FullPageRect.Bottom:= PHeight;

       rc.right  := Round((PWidth  * 1440 div DPI) * InvZoomRatio);
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

       NeededWidth:=  Round(NeededWidth  * ZoomRatio);
       NeededHeight:= Round((NeededHeight+ 55) * ZoomRatio) + 10;
       if NeededHeight > PHeight then
          NeededHeight := PHeight;

       Self.Width  := NeededWidth;
       Self.Height := NeededHeight;

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
     VK_ESCAPE: begin
            key := 0;
            if IntroducingTagsState = itWithTagSelector then begin
               IgnoreSelectorForTagSubsr := cTagSelector.SelectedTagName;
               cTagSelector.CloseTagSelector(false);
            end
            else
               HideEditor;
     end;

    VK_INSERT:
       if ( shift = [ssShift] ) then begin
         if CmdPaste(false, false) then key:= 0;
       end
       else if shift = [ssCtrl] then begin
         if CmdCopy then key:= 0;
       end;

    VK_DELETE:
       if ( shift = [ssShift] ) then begin
         if CmdCut then key:= 0;
       end;

// We'll manage CTR-V,CTR-C,CTR-X from FormShortCut. From this only CTR-V can be intercepted
//    ELSE BEGIN
//        ShortCutToKey(16470, myKey, myShift);
//        if (myKey=key) and (myShift=Shift) then begin
//            if CmdPaste(false) then key:= 0;
//        end
//    END;

  end;

  if not (key in [0, 17, 18]) and
           ( (ssCtrl in Shift) or (ssAlt in Shift) ) or
           ( (Shift = [ssShift]) and (key in [VK_F1..VK_F4]) ) then begin
     if PerformOtherCommandShortcut( Key, Shift ) then
         Key := 0;
  end;


end;


initialization

end.
