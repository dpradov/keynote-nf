object Form_Image: TForm_Image
  Left = 306
  Top = 299
  HelpType = htKeyword
  HelpKeyword = '41-2'
  ActiveControl = txtCaption
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Image properties'
  ClientHeight = 414
  ClientWidth = 382
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 347
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
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    382
    414)
  TextHeight = 13
  object lblZoom: TLabel
    Left = 211
    Top = 351
    Width = 40
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    AutoSize = False
    Caption = '3000%'
    FocusControl = txtCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblDetails: TLabel
    Left = 8
    Top = 4
    Width = 341
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
    Left = 187
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clGray
    OnClick = bGrayClick
  end
  object bWhite: TToolbarButton97
    Left = 206
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clWhite
    OnClick = bWhiteClick
  end
  object bBlack: TToolbarButton97
    Left = 169
    Top = 390
    Width = 15
    Height = 15
    Anchors = [akLeft, akBottom]
    Color = clBlack
    OnClick = bBlackClick
  end
  object btnOpenFolder: TToolbarButton97
    Left = 3
    Top = 348
    Width = 25
    Height = 22
    Anchors = [akLeft, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    Enabled = False
    ImageIndex = 1
    Images = Form_Main.IMG_Toolbar
    OnClick = btnOpenFolderClick
  end
  object btnCreateFile: TToolbarButton97
    Left = 29
    Top = 348
    Width = 25
    Height = 22
    Hint = 'Creates a file with the image content'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    ImageIndex = 2
    Images = Form_Main.IMG_Toolbar
    OnClick = btnCreateFileClick
  end
  object btnZoomOut: TToolbarButton97
    Left = 163
    Top = 348
    Width = 23
    Height = 22
    Hint = 'Zoom Out  (numeric -)'
    Anchors = [akRight, akBottom]
    DropdownCombo = True
    ImageIndex = 58
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomOutClick
  end
  object btnZoomIn: TToolbarButton97
    Left = 138
    Top = 348
    Width = 23
    Height = 22
    Hint = 'Zoom In   (numeric +)'
    Anchors = [akRight, akBottom]
    DropdownArrowWidth = 12
    DropdownCombo = True
    ImageIndex = 57
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomInClick
  end
  object btnZoomReset: TToolbarButton97
    Left = 188
    Top = 348
    Width = 23
    Height = 22
    Hint = 'Show the image in 100%   (numeric *)'
    Anchors = [akRight, akBottom]
    DropdownCombo = True
    ImageIndex = 0
    Images = Form_Main.IMG_Toolbar
    OnClick = btnZoomResetClick
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
    ImageIndex = 39
    Images = Form_Main.IMG_Toolbar
    OnClick = btnNextImageClick
    ExplicitTop = 422
  end
  object btnAlwaysVisible: TToolbarButton97
    Left = 328
    Top = 349
    Width = 20
    Height = 22
    Hint = 'Always visible'
    AllowAllUp = True
    Anchors = [akRight, akBottom]
    GroupIndex = 1
    DropdownCombo = True
    ImageIndex = 45
    Images = Form_Main.IMG_Toolbar
    OnClick = btnAlwaysVisibleClick
  end
  object btnCopy: TToolbarButton97
    Left = 56
    Top = 348
    Width = 25
    Height = 22
    Hint = 'Copy image to clipboard'
    Anchors = [akLeft, akBottom]
    DropdownCombo = True
    ImageIndex = 4
    Images = Form_Main.IMG_Toolbar
    OnClick = btnCopyClick
  end
  object btnEncryp: TToolbarButton97
    Left = 354
    Top = 1
    Width = 20
    Height = 22
    Hint = 'Toogle encrypted/decrypted'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    GroupIndex = 2
    DropdownCombo = True
    ImageIndex = 1
    Images = Form_Main.MGRImages
    OnClick = btnEncrypClick
  end
  object lblLinked: TLabel
    Left = 298
    Top = 6
    Width = 56
    Height = 13
    Hint = 'Image is linked (not owned by KNT file)'
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = ' Linked'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = False
    Visible = False
  end
  object Button_Cancel: TButton
    Left = 315
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
  end
  object txtCaption: TEdit
    Left = 8
    Top = 22
    Width = 366
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
  end
  object Button_Modify: TButton
    Left = 248
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
  end
  object cScrollBox: TScrollBox
    Left = 8
    Top = 47
    Width = 366
    Height = 298
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    OnResize = cScrollBoxResize
    DesignSize = (
      362
      294)
    object cImage: TImage
      AlignWithMargins = True
      Left = 4
      Top = 0
      Width = 354
      Height = 292
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = [akLeft, akTop, akRight, akBottom]
      Proportional = True
      ExplicitWidth = 352
    end
  end
  object chkExpand: TCheckBox
    Left = 263
    Top = 346
    Width = 62
    Height = 25
    Hint = 'Adjust the image to the form'#39's size'#13#10'(numeric / )'
    TabStop = False
    Anchors = [akRight, akBottom]
    Caption = 'Adjust'
    TabOrder = 4
    OnClick = chkExpandClick
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
  end
  object chkCompact: TCheckBox
    Left = 93
    Top = 384
    Width = 68
    Height = 25
    Hint = 'Compact mode: Hide image details and title'
    TabStop = False
    Anchors = [akLeft, akBottom]
    Caption = 'Compact'
    TabOrder = 6
    OnClick = chkCompactClick
  end
  object btnHelp: TBitBtn
    Left = 350
    Top = 347
    Width = 25
    Height = 25
    Hint = 
      'Keyboard shortcuts:'#13#10#13#10'Zoom: '#13#10' + In       [ Ctrl ]'#13#10' -  Out    ' +
      ' [ Ctrl ]'#13#10' * 100%'#13#10' / Adjust'#13#10#13#10'Scrolling:'#13#10' Cursors       [ Ct' +
      'rl ]'#13#10' Home, End  [ Ctrl ]'#13#10' Prior, Next'#13#10#13#10'( F1: More Help )'
    Anchors = [akRight, akBottom]
    ImageIndex = 60
    Images = Form_Main.IMG_Toolbar
    TabOrder = 7
    TabStop = False
    OnClick = btnHelpClick
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 288
    Top = 272
  end
end
