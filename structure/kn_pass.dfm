object Form_Password: TForm_Password
  Left = 424
  Top = 369
  HelpContext = 51
  ActiveControl = Edit_Pass
  BorderStyle = bsDialog
  Caption = 'File access passphrase'
  ClientHeight = 129
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 16
  object Button_OK: TButton
    Left = 151
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 239
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 7
    Width = 435
    Height = 81
    Caption = ' Enter access passphrase: '
    TabOrder = 2
    object Label_FileName: TLabel
      Left = 402
      Top = 22
      Width = 12
      Height = 13
      Alignment = taRightJustify
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 15
      Top = 50
      Width = 82
      Height = 13
      AutoSize = False
      Caption = '&Passphrase:'
      FocusControl = Edit_Pass
    end
    object Edit_Pass: TSecureEdit
      Left = 101
      Top = 45
      Width = 313
      Height = 24
      PasswordChar = '*'
      TabOrder = 0
    end
  end
end
