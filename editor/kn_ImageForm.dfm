object Form_Image: TForm_Image
  Left = 306
  Top = 299
  ActiveControl = txtCaption
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Image properties'
  ClientHeight = 440
  ClientWidth = 407
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 340
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
  OnShow = FormShow
  DesignSize = (
    407
    440)
  TextHeight = 13
  object lblDetails: TLabel
    Left = 8
    Top = 4
    Width = 336
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
    Top = 416
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clGray
    OnClick = bGrayClick
    ExplicitTop = 426
  end
  object bWhite: TToolbarButton97
    Left = 208
    Top = 416
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clWhite
    OnClick = bWhiteClick
    ExplicitTop = 426
  end
  object bBlack: TToolbarButton97
    Left = 171
    Top = 416
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clBlack
    OnClick = bBlackClick
    ExplicitTop = 426
  end
  object btnOpenFolder: TToolbarButton97
    Left = 32
    Top = 375
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
    ExplicitTop = 385
  end
  object btnCreateFile: TToolbarButton97
    Left = 58
    Top = 375
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
    ExplicitTop = 385
  end
  object btnZoomOut: TToolbarButton97
    Left = 119
    Top = 375
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
  end
  object btnZoomIn: TToolbarButton97
    Left = 92
    Top = 375
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
  end
  object lblZoom: TLabel
    Left = 194
    Top = 379
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '3000%'
    FocusControl = txtCaption
  end
  object btnZoomReset: TToolbarButton97
    Left = 150
    Top = 375
    Width = 30
    Height = 22
    Hint = 'Show the image in 100%   (numeric *)'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 0
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomResetClick
  end
  object lblLinked: TLabel
    Left = 350
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
    Top = 412
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
    ExplicitTop = 422
  end
  object btnNextImage: TToolbarButton97
    Left = 66
    Top = 412
    Width = 20
    Height = 22
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
    Top = 376
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
    Left = 332
    Top = 409
    Width = 60
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    TabStop = False
    OnClick = Button_CancelClick
    ExplicitLeft = 328
    ExplicitTop = 408
  end
  object txtCaption: TEdit
    Left = 8
    Top = 22
    Width = 386
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
    ExplicitWidth = 382
  end
  object Button_Modify: TButton
    Left = 265
    Top = 409
    Width = 60
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
    TabStop = False
    OnClick = Button_ModifyClick
    ExplicitLeft = 261
    ExplicitTop = 408
  end
  object cScrollBox: TScrollBox
    Left = 8
    Top = 47
    Width = 386
    Height = 324
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnResize = cScrollBoxResize
    ExplicitWidth = 382
    ExplicitHeight = 323
    DesignSize = (
      382
      320)
    object cImage: TImage
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 379
      Height = 318
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
    Left = 327
    Top = 373
    Width = 68
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
    ExplicitLeft = 323
    ExplicitTop = 372
  end
  object txtID: TEdit
    Left = 25
    Top = 412
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
    Top = 410
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
    Left = 236
    Top = 408
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
    ExplicitLeft = 232
    ExplicitTop = 407
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 288
    Top = 272
  end
end
