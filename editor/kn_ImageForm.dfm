object Form_Image: TForm_Image
  Left = 306
  Top = 299
  ActiveControl = txtCaption
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Image properties'
  ClientHeight = 450
  ClientWidth = 495
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 313
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    495
    450)
  TextHeight = 13
  object lblDetails: TLabel
    Left = 8
    Top = 9
    Width = 427
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
  end
  object bGray: TToolbarButton97
    Left = 53
    Top = 427
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clGray
    OnClick = bGrayClick
  end
  object bWhite: TToolbarButton97
    Left = 72
    Top = 427
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clWhite
    OnClick = bWhiteClick
  end
  object bBlack: TToolbarButton97
    Left = 35
    Top = 427
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clBlack
    OnClick = bBlackClick
  end
  object btnOpenFolder: TToolbarButton97
    Left = 6
    Top = 378
    Width = 30
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
    ExplicitTop = 376
  end
  object btnCreateFile: TToolbarButton97
    Left = 37
    Top = 378
    Width = 30
    Height = 22
    Hint = 'Creates a file with the image content'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 2
    Images = Form_Main.IMG_Toolbar
    OnClick = btnCreateFileClick
    ExplicitTop = 376
  end
  object btnZoomOut: TToolbarButton97
    Left = 110
    Top = 378
    Width = 27
    Height = 22
    Hint = 'Zoom Out'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 58
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomOutClick
    ExplicitTop = 376
  end
  object btnZoomIn: TToolbarButton97
    Left = 82
    Top = 378
    Width = 27
    Height = 22
    Hint = 'Zoom In'
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 57
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomInClick
    ExplicitTop = 376
  end
  object lblZoom: TLabel
    Left = 177
    Top = 381
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '3000%'
    FocusControl = txtCaption
    ExplicitTop = 379
  end
  object btnZoomReset: TToolbarButton97
    Left = 141
    Top = 378
    Width = 27
    Height = 22
    Hint = 'Show the image in 100%'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 0
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomResetClick
    ExplicitTop = 376
  end
  object lblLinked: TLabel
    Left = 443
    Top = 11
    Width = 39
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
    ExplicitLeft = 437
  end
  object btnPrevImage: TToolbarButton97
    Left = 10
    Top = 33
    Width = 20
    Height = 22
    DropdownArrowWidth = 12
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 38
    Images = Form_Main.IMG_Toolbar
    OnClick = btnPrevImageClick
  end
  object btnNextImage: TToolbarButton97
    Left = 73
    Top = 33
    Width = 20
    Height = 22
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 39
    Images = Form_Main.IMG_Toolbar
    OnClick = btnNextImageClick
  end
  object btnAlwaysVisible: TToolbarButton97
    Left = 5
    Top = 424
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
  end
  object Button_Cancel: TButton
    Left = 397
    Top = 417
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = Button_CancelClick
    ExplicitLeft = 393
    ExplicitTop = 416
  end
  object txtCaption: TEdit
    Left = 105
    Top = 31
    Width = 375
    Height = 24
    Hint = 'Caption of the image'
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 371
  end
  object Button_Modify: TButton
    Left = 295
    Top = 417
    Width = 90
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_ModifyClick
    ExplicitLeft = 291
    ExplicitTop = 416
  end
  object cScrollBox: TScrollBox
    Left = 8
    Top = 60
    Width = 473
    Height = 312
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnResize = cScrollBoxResize
    ExplicitWidth = 469
    ExplicitHeight = 311
    object cImage: TImage
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 466
      Height = 306
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Proportional = True
    end
  end
  object chkExpand: TCheckBox
    Left = 414
    Top = 376
    Width = 76
    Height = 25
    Hint = 'Adjust the image to the form'#39's size'
    Anchors = [akRight, akBottom]
    Caption = 'Expand'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = chkExpandClick
    ExplicitLeft = 410
    ExplicitTop = 375
  end
  object txtID: TEdit
    Left = 32
    Top = 33
    Width = 39
    Height = 21
    Hint = 'Caption of the image'
    Alignment = taCenter
    TabOrder = 5
    OnEnter = txtIDEnter
    OnExit = txtIDExit
    OnKeyDown = txtIDKeyDown
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 216
    Top = 400
  end
end
