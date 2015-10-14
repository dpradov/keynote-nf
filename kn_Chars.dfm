object Form_Chars: TForm_Chars
  Left = 315
  Top = 236
  HelpContext = 570
  ActiveControl = Chars
  BorderStyle = bsDialog
  Caption = 'Insert character'
  ClientHeight = 233
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label_Code: TTntLabel
    Left = 365
    Top = 105
    Width = 18
    Height = 13
    Hint = 'Selected character code'
    Caption = '000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowAccelChar = False
  end
  object Label1: TTntLabel
    Left = 365
    Top = 155
    Width = 40
    Height = 13
    Caption = '&Number:'
    FocusControl = Spin_Count
  end
  object Button_Insert: TTntButton
    Left = 365
    Top = 10
    Width = 75
    Height = 25
    Hint = 'Insert selected character'
    Caption = '&Insert'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button_InsertClick
  end
  object Button_Close: TTntButton
    Left = 365
    Top = 70
    Width = 75
    Height = 25
    Hint = 'Close this dialog box'
    Caption = '&Close'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 3
    OnClick = Button_CloseClick
  end
  object Button_Font: TTntButton
    Left = 365
    Top = 200
    Width = 75
    Height = 25
    Hint = 'Select a different font or codepage'
    Caption = '&Font...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    OnClick = Button_FontClick
  end
  object Chars: TListBox
    Left = 8
    Top = 4
    Width = 351
    Height = 221
    Columns = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    ParentFont = False
    TabOrder = 0
    OnClick = CharsClick
    OnDblClick = CharsDblClick
    OnKeyDown = CharsKeyDown
  end
  object Spin_Count: TSpinEdit
    Left = 365
    Top = 170
    Width = 56
    Height = 22
    Hint = 'Number of characters to insert'
    MaxLength = 3
    MaxValue = 999
    MinValue = 1
    TabOrder = 5
    Value = 1
    OnKeyDown = CharsKeyDown
  end
  object CheckBox_FullSet: TTntCheckBox
    Left = 365
    Top = 135
    Width = 69
    Height = 17
    Hint = 'Show full or abbreviated character set'
    Caption = 'Full &Set'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object Button_Copy: TTntButton
    Left = 365
    Top = 40
    Width = 75
    Height = 25
    Hint = 'Copy character to clipboard'
    Caption = 'C&opy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button_CopyClick
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdForceFontExist]
    Left = 20
    Top = 180
  end
  object FormPlacement: TFormPlacement
    IniSection = 'CharDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 20
    Top = 30
  end
end
