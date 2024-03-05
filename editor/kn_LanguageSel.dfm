object Form_Lang: TForm_Lang
  Left = 410
  Top = 312
  HelpType = htKeyword
  HelpKeyword = '254-1'
  ActiveControl = Combo_Lang
  BorderStyle = bsDialog
  Caption = 'Select language'
  ClientHeight = 138
  ClientWidth = 250
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnHelp = FormHelp
  TextHeight = 13
  object Button_OK: TButton
    Left = 50
    Top = 109
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button_Cancel: TButton
    Left = 130
    Top = 109
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 0
    Width = 241
    Height = 101
    Caption = ' &Language '
    TabOrder = 2
    object Combo_Lang: TLanguagesCombo
      Left = 10
      Top = 63
      Width = 216
      Height = 22
      Language = 2048
      LanguageType = ltInstalled
      ViewType = lvtLocalized
      ParentShowHint = False
      ShowFlag = False
      ShowHint = True
      TabOrder = 4
    end
    object RB_Selected: TRadioButton
      Left = 10
      Top = 20
      Width = 87
      Height = 17
      Caption = '&Selected'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RB_SelectedClick
    end
    object RB_Recent: TRadioButton
      Left = 106
      Top = 20
      Width = 103
      Height = 17
      Caption = '&Recent'
      TabOrder = 1
      OnClick = RB_SelectedClick
    end
    object RB_Default: TRadioButton
      Left = 10
      Top = 40
      Width = 87
      Height = 17
      Caption = '&Default'
      TabOrder = 2
      OnClick = RB_SelectedClick
    end
    object RB_System: TRadioButton
      Left = 106
      Top = 40
      Width = 103
      Height = 17
      Caption = 'S&ystem'
      TabOrder = 3
      OnClick = RB_SelectedClick
    end
  end
end
