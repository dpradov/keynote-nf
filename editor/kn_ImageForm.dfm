object Form_Image: TForm_Image
  Left = 306
  Top = 299
  ActiveControl = txtCaption
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Image properties'
  ClientHeight = 414
  ClientWidth = 386
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    386
    414)
  TextHeight = 13
  object lblDetails: TLabel
    Left = 8
    Top = 4
    Width = 315
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 357
  end
  object bGray: TToolbarButton97
    Left = 189
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clGray
    OnClick = bGrayClick
    ExplicitTop = 426
  end
  object bWhite: TToolbarButton97
    Left = 208
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clWhite
    OnClick = bWhiteClick
    ExplicitTop = 426
  end
  object bBlack: TToolbarButton97
    Left = 171
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clBlack
    OnClick = bBlackClick
    ExplicitTop = 426
  end
  object btnOpenFolder: TToolbarButton97
    Left = 29
    Top = 349
    Width = 25
    Height = 22
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Enabled = False
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 1
    Images = Form_Main.IMG_Toolbar
    OnClick = btnOpenFolderClick
    ExplicitTop = 375
  end
  object btnCreateFile: TToolbarButton97
    Left = 55
    Top = 349
    Width = 25
    Height = 22
    Hint = 'Creates a file with the image content'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 2
    Images = Form_Main.IMG_Toolbar
    OnClick = btnCreateFileClick
    ExplicitTop = 375
  end
  object btnZoomOut: TToolbarButton97
    Left = 111
    Top = 349
    Width = 30
    Height = 22
    Hint = 'Zoom Out  (numeric -)'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 58
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomOutClick
    ExplicitTop = 350
  end
  object btnZoomIn: TToolbarButton97
    Left = 84
    Top = 349
    Width = 30
    Height = 22
    Hint = 'Zoom In   (numeric +)'
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 57
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomInClick
    ExplicitTop = 350
  end
  object lblZoom: TLabel
    Left = 171
    Top = 353
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = '3000%'
    FocusControl = txtCaption
    ExplicitTop = 354
  end
  object btnZoomReset: TToolbarButton97
    Left = 142
    Top = 349
    Width = 25
    Height = 22
    Hint = 'Show the image in 100%   (numeric *)'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 0
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomResetClick
    ExplicitTop = 350
  end
  object lblLinked: TLabel
    Left = 329
    Top = 6
    Width = 56
    Height = 13
    Hint = 'Image is linked (not owned by KNT file)'
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Linked'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    ExplicitLeft = 371
  end
  object btnPrevImage: TToolbarButton97
    Left = 3
    Top = 386
    Width = 20
    Height = 22
    Hint = 'Previous image instance in node [Ctrl: prev.ID in file]'
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 38
    Images = Form_Main.IMG_Toolbar
    OnClick = btnPrevImageClick
    ExplicitTop = 422
  end
  object btnNextImage: TToolbarButton97
    Left = 66
    Top = 386
    Width = 20
    Height = 22
    Hint = 'Next image instance in node [Ctrl: next ID in file]'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 39
    Images = Form_Main.IMG_Toolbar
    OnClick = btnNextImageClick
    ExplicitTop = 422
  end
  object btnAlwaysVisible: TToolbarButton97
    Left = 5
    Top = 350
    Width = 20
    Height = 22
    Hint = 'Always visible'
    AllowAllUp = True
    Anchors = [akLeft, akBottom]
    GroupIndex = 1
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 45
    Images = Form_Main.IMG_Toolbar
    OnClick = btnAlwaysVisibleClick
    ExplicitTop = 386
  end
  object Button_Cancel: TButton
    Left = 311
    Top = 383
    Width = 60
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    TabStop = False
    OnClick = Button_CancelClick
    ExplicitLeft = 300
    ExplicitTop = 408
  end
  object txtCaption: TEdit
    Left = 8
    Top = 22
    Width = 365
    Height = 24
    Hint = 'Caption of the image'
    TabStop = False
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 354
  end
  object Button_Modify: TButton
    Left = 244
    Top = 383
    Width = 60
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    TabStop = False
    OnClick = Button_ModifyClick
    ExplicitLeft = 233
    ExplicitTop = 408
  end
  object cScrollBox: TScrollBox
    Left = 8
    Top = 47
    Width = 365
    Height = 298
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnResize = cScrollBoxResize
    ExplicitWidth = 354
    ExplicitHeight = 323
    DesignSize = (
      361
      294)
    object cImage: TImage
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 358
      Height = 292
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akLeft, akTop, akRight, akBottom]
      Proportional = True
      ExplicitWidth = 403
      ExplicitHeight = 328
    end
  end
  object chkExpand: TCheckBox
    Left = 289
    Top = 346
    Width = 62
    Height = 25
    Hint = 'Adjust the image to the form'#39's size'#13#10'(numeric / )'
    TabStop = False
    Anchors = [akRight, akBottom]
    Caption = 'Adjust'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = chkExpandClick
    ExplicitLeft = 282
    ExplicitTop = 372
  end
  object txtID: TEdit
    Left = 25
    Top = 386
    Width = 39
    Height = 21
    Hint = 'Caption of the image'
    TabStop = False
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    OnEnter = txtIDEnter
    OnExit = txtIDExit
    OnKeyDown = txtIDKeyDown
    ExplicitTop = 411
  end
  object chkCompact: TCheckBox
    Left = 95
    Top = 384
    Width = 68
    Height = 25
    Hint = 'Compact mode: Hide image details and title'
    TabStop = False
    Anchors = [akLeft, akBottom]
    Caption = 'Compact'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = chkCompactClick
    ExplicitTop = 409
  end
  object btnHelp: TBitBtn
    Left = 348
    Top = 347
    Width = 25
    Height = 25
    Hint = 
      'Keyboard shortcuts:'#13#10#13#10'Zoom: '#13#10' + In       [ Ctrl ]'#13#10' -  Out    ' +
      ' [ Ctrl ]'#13#10' * 100%'#13#10' / Adjust'#13#10#13#10'Scrolling:'#13#10' Cursors       [ Ct' +
      'rl ]'#13#10' Home, End  [ Ctrl ]'#13#10' Prior, Next'
    Anchors = [akRight, akBottom]
    ImageIndex = 60
    Images = Form_Main.IMG_Toolbar
    TabOrder = 7
    TabStop = False
    OnClick = btnHelpClick
    ExplicitLeft = 341
    ExplicitTop = 373
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 288
    Top = 272
  end
end
