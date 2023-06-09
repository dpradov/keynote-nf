//------------------------------------------------------------------------
// TRxBTRichPopUpMenu
// (RxDB-)RxRichedit formatting component
// Copyright 2000 Betasoft
// changed: KoBraSoft
//------------------------------------------------------------------------
unit RxRichPopup;
{$I Rx.inc}
interface

uses
  Classes, Graphics, Dialogs, Menus, ComCtrls, RxRichEd, ExtDlgs {, jpeg, RxGIF};

const
  BTRxRichPopUpMenuVersion = '$Revision: 1.2 $';

type
  {:Supported languages in TBTRichPopUpMenu}
  TRxRichPopUpMenuLanguage = (roEnglish, roGerman, roCzech);
  TRxRichPopUpMenuOption = (rpShowShortcuts);
  {:Options for TBTRichPopUpMenu}
  TRxRichPopUpMenuOptions = set of TRxRichPopUpMenuOption;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxRichPopUpMenu = class(TPopUpMenu)
  private
    FRichEdit: TRxCustomRichEdit;
    FFontDialog: TFontDialog;
    FOpenPictureDialog: TOpenPictureDialog;
    FLanguage: TRxRichPopUpMenuLanguage;
    FOptions: TRxRichPopUpMenuOptions;
    SavedStyle: TFont;
    SavedParagraph: TRxParaAttributes;
    procedure Undo(Sender: TObject);
    procedure Cut(Sender: TObject);
    procedure Copy(Sender: TObject);
    procedure Paste(Sender: TObject);
    procedure DoClear(Sender: TObject);
    procedure SelectAll(Sender: TObject);
    procedure FormatChar(Sender: TObject);
    procedure Bold(Sender: TObject);
    procedure Underlined(Sender: TObject);
    procedure Italic(Sender: TObject);
    procedure Left(Sender: TObject);
    procedure Center(Sender: TObject);
    procedure Right(Sender: TObject);
    procedure Bullet(Sender: TObject);
    procedure Picture(Sender: TObject);
    procedure InsertObject(Sender: TObject);
    procedure Superscript(Sender: TObject);
    procedure Normal(Sender: TObject);
    procedure Subscript(Sender: TObject);
    {:Additional OnPopUp-handler enables/disables the menu items. }
    procedure MyPopup(Sender: TObject);
  public
    miUndo,
      miCut, miCopy, miPaste, miClear,
      miSelectAll,
      miSepEdit,
      miFont,
      miSepFont,
      miLeft, miCenter, miRight,
      miSuperscript, miNormal, miSubscript,
      miSepAlign,
      miBold, miUnderlined, miItalic,
      miSepBUI,
      miBullet,
      miPic,
      miInObj: TMenuItem;
    {:In this method, the BTRichPop menu items are created.}
    procedure Loaded; override;
    procedure Popup(X, Y: Integer); override;
    {:RichEdit component}
    property RichEdit: TRxCustomRichEdit read FRichEdit write FRichEdit;
  published
    {:If FontDialog is assigned, a new menu entry will appear.}
    property FontDialog: TFontdialog read FFontDialog write FFontDialog;
    {:If OpenPictureDialog is assigned, a new menu entry will appear.}
    property OpenPictureDialog: TOpenPictureDialog read FOpenPictureDialog write FOpenPictureDialog;
    {:Select roEnglish or roGerman.}
    property Language: TRxRichPopUpMenuLanguage read FLanguage write FLanguage;
    {:Enable shortcuts. Only works if each RichEdit has its own RichPop!}
    // property PopupOptions: TBTRichPopUpMenuOptions read FOptions write FOptions;
  end;

procedure CharAttr(RichEdit: TRxCustomRichEdit; const newStyle: TFontStyle);
procedure PrepEdit(RichEdit: TRxCustomRichEdit);

implementation

uses
  Clipbrd, Windows, Messages;

procedure CharAttr(RichEdit: TRxCustomRichEdit;
  const newStyle: TFontStyle);
begin
  if Assigned(RichEdit) then
  begin
    PrepEdit(RichEdit);
    with Richedit.SelAttributes do
    begin
      if not (newStyle in Richedit.SelAttributes.style) then
        Style := Style + [newStyle]
      else
        Style := Style - [newStyle];
    end;
  end;
end;

procedure PrepEdit(RichEdit: TRxCustomRichEdit);
{$IFNDEF RX_D3}
var
  SelStartMemo, SelEndMemo: Integer;
  {$ENDIF}
begin
  {$IFNDEF RX_D3}
  if RichEdit is TRXDBRichEdit then
    with TRXDBRichEdit(RichEdit) do
      if DataSource.DataSet.State <> dsEdit then
      begin
        SelStartMemo := SelStart;
        SelEndMemo := SelLength;
        DataSource.DataSet.Edit;
        SelStart := SelStartMemo;
        SelLength := SelEndMemo;
      end;
  {$ENDIF}
end;

{  TBTRxRichPopUpMenu  }

procedure TRxRichPopUpMenu.Loaded;
var
  mi: TMenuItem;
  cCaptionUndo,
    cCaptionCut,
    cCaptionCopy,
    cCaptionPaste,
    cCaptionDelete,
    cCaptionSelectAll,
    cCaptionFont,
    cCaptionBold,
    cCaptionUnderlined,
    cCaptionItalic,
    cCaptionAlignLeft,
    cCaptionAlignCentered,
    cCaptionAlignRight,
    cCaptionSuperscript,
    cCaptionNormal,
    cCaptionSubsript,
    cCaptionBullet,
    cCaptionPic,
    cCaptionInObj: string;
begin
  inherited Loaded;
  if not (csDesigning in Componentstate) then
  begin
    case Language of
      roCzech:
        begin
          cCaptionUndo := 'Zpìt';
          cCaptionCut := 'Vyjmout';
          cCaptionCopy := 'Kopírovat';
          cCaptionPaste := 'Vložit';
          cCaptionDelete := 'Vymazat';
          cCaptionSelectAll := 'Vybrat vše';
          cCaptionFont := 'Font...';
          cCaptionBold := 'Tuèné';
          cCaptionUnderlined := 'Podtržené';
          cCaptionItalic := 'Italic';
          cCaptionAlignLeft := 'Vlevo';
          cCaptionAlignCentered := 'Na støed';
          cCaptionAlignRight := 'Vpravo';
          cCaptionSuperscript := 'Horní index';
          cCaptionNormal := 'Bìžné';
          cCaptionSubsript := 'Dolní index';
          cCaptionBullet := 'Odrážka';
          cCaptionPic := 'Obrázek';
          cCaptionInObj := 'Vložit objekt';
        end;
      roEnglish:
        begin
          cCaptionUndo := 'Undo';
          cCaptionCut := 'Cut';
          cCaptionCopy := 'Copy';
          cCaptionPaste := 'Paste';
          cCaptionDelete := 'Delete';
          cCaptionSelectAll := 'Select all';
          cCaptionFont := 'Font...';
          cCaptionBold := 'Bold';
          cCaptionUnderlined := 'Underlined';
          cCaptionItalic := 'Italic';
          cCaptionAlignLeft := 'Align left';
          cCaptionAlignCentered := 'Centered';
          cCaptionAlignRight := 'Align Right';
          cCaptionSuperscript := 'Superscript';
          cCaptionNormal := 'Normal';
          cCaptionSubsript := 'Subscript';
          cCaptionBullet := 'Bullet';
          cCaptionPic := 'Picture';
          cCaptionInObj := 'Insert Object';
        end;
      roGerman:
        begin
          cCaptionUndo := 'Rückgängig';
          cCaptionCut := 'Ausschneiden';
          cCaptionCopy := 'Kopieren';
          cCaptionPaste := 'Einfügen';
          cCaptionDelete := 'Löschen';
          cCaptionSelectAll := 'Alles markieren';
          cCaptionFont := 'Schriftart...';
          cCaptionBold := 'Fett';
          cCaptionUnderlined := 'Unterstrichen';
          cCaptionItalic := 'Kursiv';
          cCaptionAlignLeft := 'Linksbündig';
          cCaptionAlignCentered := 'Zentriert';
          cCaptionAlignRight := 'Rechtsbündig';
          cCaptionSuperscript := 'Hochgestellt';
          cCaptionNormal := 'Normal';
          cCaptionSubsript := 'Tiefgestellt';
          cCaptionBullet := 'Aufzählung';
          cCaptionPic := 'Bild einfügen';
          cCaptionInObj := 'Objekt einfügen';
        end;
    end; // case

    SavedStyle := TFont.Create;
    SavedParagraph := TRxParaAttributes.Create(Richedit);

    miUndo := TMenuItem.Create(Self);
    with miUndo do
    begin
      Caption := cCaptionUndo;
      OnClick := Undo;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('Z'), [ssCtrl]);
    end;
    Items.Add(miUndo);

    mi := TMenuItem.Create(Self);
    with mi do
    begin
      Caption := '-';
    end;
    Items.Add(mi);

    miCut := TMenuItem.Create(Self);
    with miCut do
    begin
      Caption := cCaptionCut;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('X'), [ssCtrl]);
      OnClick := Cut;
    end;
    Items.Add(miCut);

    miCopy := TMenuItem.Create(Self);
    with miCopy do
    begin
      Caption := cCaptionCopy;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('C'), [ssCtrl]);
      OnClick := Copy;
    end;
    Items.Add(miCopy);

    miPaste := TMenuItem.Create(Self);
    with miPaste do
    begin
      Caption := cCaptionPaste;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('V'), [ssCtrl]);
      OnClick := Paste;
    end;
    Items.Add(miPaste);

    miClear := TMenuItem.Create(Self);
    with miClear do
    begin
      Caption := cCaptionDelete;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word(VK_DELETE), []);
      OnClick := DoClear;
    end;
    Items.Add(miClear);

    miSelectAll := TMenuItem.Create(Self);
    with miSelectAll do
    begin
      Caption := cCaptionSelectAll;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('A'), [ssCtrl]);
      OnClick := SelectAll;
    end;
    Items.Add(miSelectAll);

    miSepEdit := TMenuItem.Create(Self);
    miSepEdit.Caption := '-';
    Items.Add(miSepEdit);

    if not Assigned(FontDialog) then
    begin
      FontDialog := TFontDialog.Create(Self);
    end;

    if Assigned(FontDialog) then
    begin
      miFont := TMenuItem.Create(Self);
      with miFont do
      begin
        Caption := cCaptionFont;
        OnClick := FormatChar;
      end;
      Items.Add(miFont);

      miSepFont := TMenuItem.Create(Self);
      miSepFont.Caption := '-';
      Items.Add(miSepFont);

    end;

    miBold := TMenuItem.Create(Self);
    with miBold do
    begin
      Caption := cCaptionBold;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('B'), [ssCtrl]);
      OnClick := Bold;
    end;
    Items.Add(miBold);

    miUnderlined := TMenuItem.Create(Self);
    with miUnderlined do
    begin
      Caption := cCaptionUnderlined;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('U'), [ssCtrl]);
      OnClick := Underlined;
    end;
    Items.Add(miUnderlined);

    miItalic := TMenuItem.Create(Self);
    with miItalic do
    begin
      Caption := cCaptionItalic;
      if rpShowShortcuts in FOptions then ShortCut := menus.ShortCut(Word('I'), [ssCtrl]);
      OnClick := Italic;
    end;
    Items.Add(miItalic);

    miSepBUI := TMenuItem.Create(Self);
    miSepBUI.Caption := '-';
    Items.Add(miSepBUI);

    miLeft := TMenuItem.Create(Self);
    with miLeft do
    begin
      Caption := cCaptionAlignLeft;
      OnClick := Left;
      RadioItem := True;
      Checked := True;
      GroupIndex := 1;
    end;
    Items.Add(miLeft);

    miCenter := TMenuItem.Create(Self);
    with miCenter do
    begin
      Caption := cCaptionAlignCentered;
      OnClick := Center;
      RadioItem := True;
      GroupIndex := 1;
    end;
    Items.Add(miCenter);

    miRight := TMenuItem.Create(Self);
    with miRight do
    begin
      Caption := cCaptionAlignRight;
      OnClick := Right;
      RadioItem := True;
      GroupIndex := 1;
    end;
    Items.Add(miRight);

    miSepBUI := TMenuItem.Create(Self);
    miSepBUI.Caption := '-';
    Items.Add(miSepBUI);

    miSuperscript := TMenuItem.Create(Self);
    with miSuperscript do
    begin
      Caption := cCaptionSuperscript;
      OnClick := Superscript;
      RadioItem := True;
      GroupIndex := 2;
    end;
    Items.Add(miSuperscript);

    miNormal := TMenuItem.Create(Self);
    with miNormal do
    begin
      Caption := cCaptionNormal;
      OnClick := Normal;
      RadioItem := True;
      Checked := True;
      GroupIndex := 2;
    end;
    Items.Add(miNormal);

    miSubscript := TMenuItem.Create(Self);
    with miSubscript do
    begin
      Caption := cCaptionSubsript;
      OnClick := Subscript;
      RadioItem := True;
      GroupIndex := 2;
    end;
    Items.Add(miSubscript);

    miSepAlign := TMenuItem.Create(Self);
    miSepAlign.Caption := '-';
    Items.Add(miSepAlign);

    miBullet := TMenuItem.Create(Self);
    with miBullet do
    begin
      Caption := cCaptionBullet;
      OnClick := Bullet;
    end;
    Items.Add(miBullet);

    miSepAlign := TMenuItem.Create(Self);
    miSepAlign.Caption := '-';
    Items.Add(miSepAlign);

    if not Assigned(OpenPictureDialog) then
    begin
      OpenPictureDialog := TOpenPictureDialog.Create(Self);
    end;

    if Assigned(OpenPictureDialog) then
    begin
      miPic := TMenuItem.Create(Self);
      with miPic do
      begin
        Caption := cCaptionPic;
        OnClick := Picture;
      end;
      Items.Add(miPic);
    end;
    miInObj := TMenuItem.Create(Self);
    with miInObj do
    begin
      Caption := cCaptionInObj;
      OnClick := InsertObject;
    end;
    Items.Add(miInObj);
  end;
end;

procedure TRxRichPopUpMenu.Popup;
begin
  if Assigned(OnPopUp) then OnPopUp(Self);
  MyPopup(Self);
  inherited Popup(X, Y);
end;

procedure TRxRichPopUpMenu.MyPopup;
var
  {$IFNDEF RX_D3}
  SelStartMemo, SelEndMemo: Integer;
  {$ENDIF}
  canEdit: Boolean;
begin
  canEdit := True;
  {$IFNDEF RX_D3}
  if PopupComponent is TRXDBRichEdit then
    RichEdit := TRxDBRichEdit(PopupComponent)
  else
    RichEdit := TRxRichEdit(PopupComponent);
  if RichEdit is TRXDBRichEdit then
    with TRXDBRichEdit(RichEdit) do
    begin
      canEdit := ReadOnly = False;
      if canEdit and (DataSource.DataSet.State <> dsEdit) then
      begin
        SelStartMemo := SelStart;
        SelEndMemo := SelLength;
        DataSource.DataSet.Edit;
        SelStart := SelStartMemo;
        SelLength := SelEndMemo;
      end;
      miBold.Enabled := not Plaintext and canEdit;
      miUnderlined.Enabled := not Plaintext and canEdit;
      miItalic.Enabled := not Plaintext and canEdit;
      miLeft.Enabled := not Plaintext and canEdit;
      miCenter.Enabled := not Plaintext and canEdit;
      miRight.Enabled := not Plaintext and canEdit;
      miSuperscript.Enabled := not Plaintext and canEdit;
      miNormal.Enabled := not Plaintext and canEdit;
      miSubscript.Enabled := not Plaintext and canEdit;
      miBullet.Enabled := not Plaintext and canEdit;
      if Assigned(miPic) then
        miPic.Enabled := not Plaintext and canEdit;
      miInObj.Enabled := not Plaintext and canEdit;
      if Assigned(miFont) then
        miFont.Enabled := not Plaintext and canEdit;
      miSepEdit.visible := canEdit;
      miBold.Visible := canEdit;
      miUnderlined.Visible := canEdit;
      miItalic.Visible := canEdit;
      miSepBUI.visible := canEdit;
      if Assigned(miPic) then
        miPic.visible := canEdit;
      miInObj.visible := canEdit;
      if Assigned(miFont) then
        miSepFont.visible := canEdit;
      miLeft.Visible := canEdit;
      miCenter.Visible := canEdit;
      miRight.Visible := canEdit;
      miSuperscript.Visible := canEdit;
      miNormal.Visible := canEdit;
      miSubscript.Visible := canEdit;
      miSepAlign.Visible := canEdit;
      miBullet.Visible := canEdit;
    end;
  {$ELSE}
  if not Assigned(RichEdit) then RichEdit := TRxRichEdit(PopupComponent);
  {$ENDIF}
  miUndo.Enabled := (Richedit.Perform(EM_CANUNDO, 0, 0) > 0) and canEdit;
  miCut.Enabled := (RichEdit.SelLength > 0) and canEdit;
  miCopy.Enabled := RichEdit.SelLength > 0;
  miClear.Enabled := (RichEdit.SelLength > 0) and canEdit;
  miPaste.Enabled := ClipBoard.HasFormat(CF_TEXT) and canEdit;
  miSelectAll.Enabled := Length(RichEdit.Text) > RichEdit.SelLength;
  with RichEdit.SelAttributes do
  begin
    miBold.Checked := fsBold in Style;
    miUnderlined.Checked := fsUnderline in Style;
    miItalic.Checked := fsItalic in Style;
  end;
  with RichEdit.Paragraph do
  begin
    miLeft.Checked := Alignment = paLeftJustify;
    miCenter.Checked := Alignment = paCenter;
    miRight.Checked := Alignment = paRightJustify;
    miBullet.Checked := Numbering <> nsNone;
  end;
end;

procedure TRxRichPopUpMenu.Undo;
begin
  Richedit.Perform(WM_UNDO, 0, 0);
end;

procedure TRxRichPopUpMenu.Cut;
begin
  RichEdit.Perform(WM_CUT, 0, 0);
end;

procedure TRxRichPopUpMenu.Copy;
begin
  Richedit.Perform(WM_COPY, 0, 0);
end;

procedure TRxRichPopUpMenu.Paste;
begin
  Richedit.Perform(WM_PASTE, 0, 0);
end;

procedure TRxRichPopUpMenu.DoClear;
begin
  Richedit.Perform(WM_CLEAR, 0, 0);
end;

procedure TRxRichPopUpMenu.SelectAll;
begin
  Richedit.SelectAll;
end;

procedure TRxRichPopUpMenu.Bold;
begin
  CharAttr(RichEdit, fsBold);
end;

procedure TRxRichPopUpMenu.Underlined;
begin
  CharAttr(RichEdit, fsUnderline);
end;

procedure TRxRichPopUpMenu.Italic;
begin
  CharAttr(RichEdit, fsItalic);
end;

procedure TRxRichPopUpMenu.FormatChar;
begin
  if Assigned(FontDialog) then
    with FontDialog do
    begin
      Font := TFont(Richedit.SelAttributes);
      if Execute then RichEdit.SelAttributes := TRxTextAttributes(Font);
    end;
end;

procedure TRxRichPopUpMenu.Bullet;
begin
  with Richedit.Paragraph do
    if Numbering = nsNone then
      Numbering := nsBullet
    else
      Numbering := nsNone
end;

procedure TRxRichPopUpMenu.Picture;
var
  Pict: TPicture;
begin
  if Assigned(OpenPictureDialog) then
    with FOpenPictureDialog do
    begin
      if Execute then
      begin
        Pict := TPicture.Create;
        try
          Pict.LoadFromFile(FileName);
          Clipboard.Assign(Pict);
          Richedit.PasteFromClipboard;
        finally
          Pict.Free;
        end;
      end;
    end;
end;

procedure TRxRichPopUpMenu.InsertObject;
begin
  Richedit.InsertObjectDialog;
end;

procedure TRxRichPopUpMenu.Left;
begin
  Richedit.Paragraph.Alignment := paLeftJustify;
  miLeft.Checked := True;
end;

procedure TRxRichPopUpMenu.Center;
begin
  Richedit.Paragraph.Alignment := paCenter;
  miCenter.Checked := True;
end;

procedure TRxRichPopUpMenu.Right;
begin
  Richedit.Paragraph.Alignment := paRightJustify;
  miRight.Checked := True;
end;

procedure TRxRichPopUpMenu.Superscript;
begin
  Richedit.SelAttributes.SubscriptStyle := ssSuperscript;
end;

procedure TRxRichPopUpMenu.Normal;
begin
  Richedit.SelAttributes.SubscriptStyle := ssNone;
end;

procedure TRxRichPopUpMenu.Subscript;
begin
  Richedit.SelAttributes.SubscriptStyle := ssSubscript;
end;

end.