object Form_CharsNew: TForm_CharsNew
  Left = 315
  Top = 236
  HelpContext = 23
  BorderStyle = bsDialog
  Caption = 'Insert character'
  ClientHeight = 402
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 14
  object Label3: TLabel
    Left = 9
    Top = 6
    Width = 184
    Height = 13
    AutoSize = False
    Caption = 'Selected characters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowAccelChar = False
  end
  object lblCode: TLabel
    Left = 446
    Top = 287
    Width = 69
    Height = 29
    Hint = 'Selected character code'
    Alignment = taCenter
    AutoSize = False
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowAccelChar = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 446
    Top = 147
    Width = 47
    Height = 14
    Caption = '&Number:'
    FocusControl = Spin_Count
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lblFontName: TLabel
    Left = 248
    Top = 287
    Width = 184
    Height = 21
    Hint = 'Selected character font name'
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Tahoma'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowAccelChar = False
  end
  object btnEditTable: TToolbarButton97
    Left = 12
    Top = 288
    Width = 67
    Height = 28
    Hint = 'Edit characters in custom table'
    AllowAllUp = True
    GroupIndex = 1
    Caption = ' Edit'
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 5
    Images = Form_Main.IMG_TV
    RepeatInterval = 101
    OnClick = btnEditTableClick
  end
  object btnAddToTable: TToolbarButton97
    Left = 82
    Top = 288
    Width = 67
    Height = 28
    Hint = 'Add new characters to table'
    AllowAllUp = True
    GroupIndex = 2
    Caption = ' Add'
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 19
    Images = Form_Main.IMG_Toolbar
    RepeatInterval = 101
    OnClick = btnAddToTableClick
  end
  object EditorTable: TRxRichEdit
    Left = 8
    Top = 26
    Width = 424
    Height = 253
    DrawEndPage = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
    OnKeyDown = EditorTableKeyDown
  end
  object EditorAux: TRxRichEdit
    AlignWithMargins = True
    Left = 8
    Top = 26
    Width = 424
    Height = 253
    DrawEndPage = False
    Color = 12582911
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    Visible = False
  end
  object btnInsert: TButton
    Left = 446
    Top = 81
    Width = 75
    Height = 25
    Hint = 'Insert selected character'
    Caption = '&Insert'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = btnInsertClick
  end
  object Button_Close: TButton
    Left = 446
    Top = 26
    Width = 75
    Height = 25
    Hint = 'Close this dialog box'
    Caption = '&Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
    OnClick = Button_CloseClick
  end
  object Spin_Count: TSpinEdit
    Left = 446
    Top = 166
    Width = 75
    Height = 23
    Hint = 'Number of characters to insert'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 3
    MaxValue = 999
    MinValue = 1
    ParentFont = False
    TabOrder = 4
    Value = 1
  end
  object btnCopy: TButton
    Left = 446
    Top = 114
    Width = 75
    Height = 25
    Hint = 'Copy character to clipboard'
    Caption = 'C&opy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ImageIndex = 4
    ImageMargins.Left = 4
    Images = Form_Main.IMG_Toolbar
    ParentFont = False
    TabOrder = 5
    OnClick = btnCopyClick
  end
  object btnInsertNew: TButton
    Left = 446
    Top = 332
    Width = 75
    Height = 25
    Hint = 'Insert new characters'
    Caption = '&Insert'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = btnInsertNewClick
  end
  object txtSelectedChar: TRxRichEdit
    AlignWithMargins = True
    Left = 446
    Top = 211
    Width = 74
    Height = 65
    DrawEndPage = False
    Alignment = taCenter
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -53
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssNone
    TabOrder = 7
    WordSelection = False
    WordWrap = False
  end
  object EditorNewChars: TRxRichEdit
    Left = 81
    Top = 332
    Width = 318
    Height = 28
    DrawEndPage = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object chkAutoAddNew: TCheckBox
    Left = 81
    Top = 366
    Width = 235
    Height = 19
    Caption = 'Automatically add new chars inserted'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 9
    WordWrap = True
    OnClick = chkAutoAddNewClick
  end
  object btnPaste: TButton
    Left = 402
    Top = 332
    Width = 30
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ImageAlignment = iaCenter
    ImageIndex = 5
    ImageMargins.Left = 4
    Images = Form_Main.IMG_Toolbar
    ParentFont = False
    TabOrder = 10
    OnClick = btnPasteClick
  end
  object btnOpenCharmap: TBitBtn
    Left = 9
    Top = 329
    Width = 67
    Height = 34
    Hint = 'Open windows tool "Character Map"'
    TabOrder = 11
    OnClick = btnOpenCharmapClick
  end
  object FormPlacement: TFormPlacement
    IniSection = 'CharDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 492
    Top = 379
  end
end
