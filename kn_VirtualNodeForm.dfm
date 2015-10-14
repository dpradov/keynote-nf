object Form_VNode: TForm_VNode
  Left = 318
  Top = 316
  ActiveControl = GroupBox1
  BorderStyle = bsDialog
  Caption = 'Virtual node'
  ClientHeight = 183
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button_OK: TButton
    Left = 15
    Top = 150
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button_Cancel: TButton
    Left = 95
    Top = 150
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 0
    Width = 366
    Height = 141
    TabOrder = 0
    object Label_FN: TLabel
      Left = 10
      Top = 15
      Width = 94
      Height = 13
      Caption = '&Document filename:'
      FocusControl = Edit_FN
    end
    object Label_URL: TLabel
      Left = 10
      Top = 15
      Width = 77
      Height = 13
      Caption = '&Document URL:'
      FocusControl = Edit_URL
    end
    object Edit_FN: TFilenameEdit
      Left = 10
      Top = 30
      Width = 341
      Height = 21
      AcceptFiles = True
      Filter = 
        'Rich text files|*.rtf|Text files|*.txt|HTML files|*.htm*|All fil' +
        'es (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist]
      ButtonWidth = 25
      NumGlyphs = 1
      TabOrder = 0
    end
    object Edit_URL: TEdit
      Left = 10
      Top = 30
      Width = 341
      Height = 21
      TabOrder = 1
    end
    object RB_vmNormal: TRadioButton
      Left = 15
      Top = 60
      Width = 301
      Height = 17
      Caption = 'Display file contents in &editor (standard virtual node)'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object RB_vmIELocal: TRadioButton
      Left = 15
      Top = 85
      Width = 301
      Height = 17
      Caption = 'Display local &HTML document in Internet Explorer'
      TabOrder = 3
    end
    object RB_vmIERemote: TRadioButton
      Left = 15
      Top = 110
      Width = 301
      Height = 17
      Caption = 'Display a remote &WWW document in Internet Explorer'
      TabOrder = 4
    end
  end
end
