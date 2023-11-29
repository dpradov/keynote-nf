object Form_Image: TForm_Image
  Left = 306
  Top = 299
  ActiveControl = txtCaption
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Image properties'
  ClientHeight = 450
  ClientWidth = 427
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 326
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
  OnShow = FormShow
  DesignSize = (
    427
    450)
  TextHeight = 13
  object lblDetails: TLabel
    Left = 8
    Top = 4
    Width = 360
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
    Top = 426
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clGray
    OnClick = bGrayClick
  end
  object bWhite: TToolbarButton97
    Left = 208
    Top = 426
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clWhite
    OnClick = bWhiteClick
  end
  object bBlack: TToolbarButton97
    Left = 171
    Top = 426
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clBlack
    OnClick = bBlackClick
  end
  object btnOpenFolder: TToolbarButton97
    Left = 32
    Top = 385
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
  end
  object btnCreateFile: TToolbarButton97
    Left = 58
    Top = 385
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
  end
  object btnZoomOut: TToolbarButton97
    Left = 117
    Top = 385
    Width = 25
    Height = 22
    Hint = 'Zoom Out'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 58
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomOutClick
  end
  object btnZoomIn: TToolbarButton97
    Left = 92
    Top = 385
    Width = 25
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
  end
  object lblZoom: TLabel
    Left = 173
    Top = 389
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '3000%'
    FocusControl = txtCaption
  end
  object btnZoomReset: TToolbarButton97
    Left = 143
    Top = 385
    Width = 25
    Height = 22
    Hint = 'Show the image in 100%'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 0
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomResetClick
  end
  object lblLinked: TLabel
    Left = 374
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
    Top = 422
    Width = 20
    Height = 22
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 38
    Images = Form_Main.IMG_Toolbar
    OnClick = btnPrevImageClick
  end
  object btnNextImage: TToolbarButton97
    Left = 66
    Top = 422
    Width = 20
    Height = 22
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 39
    Images = Form_Main.IMG_Toolbar
    OnClick = btnNextImageClick
  end
  object btnAlwaysVisible: TToolbarButton97
    Left = 5
    Top = 386
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
    Left = 353
    Top = 419
    Width = 65
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = Button_CancelClick
    ExplicitLeft = 266
  end
  object txtCaption: TEdit
    Left = 8
    Top = 22
    Width = 410
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
    ExplicitWidth = 407
  end
  object Button_Modify: TButton
    Left = 283
    Top = 419
    Width = 65
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_ModifyClick
    ExplicitLeft = 196
  end
  object cScrollBox: TScrollBox
    Left = 8
    Top = 47
    Width = 410
    Height = 334
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnResize = cScrollBoxResize
    ExplicitWidth = 407
    DesignSize = (
      406
      330)
    object cImage: TImage
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 403
      Height = 328
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akLeft, akTop, akRight, akBottom]
      Proportional = True
    end
  end
  object chkExpand: TCheckBox
    Left = 351
    Top = 381
    Width = 68
    Height = 25
    Hint = 'Adjust the image to the form'#39's size'
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
    ExplicitLeft = 348
  end
  object txtID: TEdit
    Left = 25
    Top = 422
    Width = 39
    Height = 21
    Hint = 'Caption of the image'
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    OnEnter = txtIDEnter
    OnExit = txtIDExit
    OnKeyDown = txtIDKeyDown
  end
  object chkCompact: TCheckBox
    Left = 95
    Top = 420
    Width = 68
    Height = 25
    Hint = 'Compact mode: Hide image details and title'
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
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 288
    Top = 272
  end
end
