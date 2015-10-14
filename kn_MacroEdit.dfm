object Form_Macro: TForm_Macro
  Left = 496
  Top = 290
  HelpContext = 530
  ActiveControl = Edit_Name
  BorderStyle = bsDialog
  Caption = 'Edit macro'
  ClientHeight = 210
  ClientWidth = 298
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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Button_OK: TTntButton
    Left = 15
    Top = 175
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TTntButton
    Left = 95
    Top = 175
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object GroupBox1: TTntGroupBox
    Left = 5
    Top = 0
    Width = 286
    Height = 166
    TabOrder = 2
    object Label1: TTntLabel
      Left = 10
      Top = 10
      Width = 62
      Height = 13
      Caption = '&Macro name:'
      FocusControl = Edit_Name
    end
    object Label2: TTntLabel
      Left = 10
      Top = 55
      Width = 56
      Height = 13
      Caption = '&Description:'
      FocusControl = Edit_Desc
    end
    object Label3: TTntLabel
      Left = 10
      Top = 100
      Width = 62
      Height = 13
      AutoSize = False
      Caption = 'Modified:'
    end
    object LB_Date: TTntLabel
      Left = 76
      Top = 100
      Width = 9
      Height = 13
      Caption = '...'
      ShowAccelChar = False
    end
    object Label4: TTntLabel
      Left = 10
      Top = 115
      Width = 62
      Height = 13
      AutoSize = False
      Caption = 'Filename:'
    end
    object LB_FileName: TTntLabel
      Left = 76
      Top = 115
      Width = 9
      Height = 13
      Caption = '...'
      ShowAccelChar = False
    end
    object Edit_Desc: TTntEdit
      Left = 10
      Top = 70
      Width = 266
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object Edit_Name: TTntEdit
      Left = 10
      Top = 25
      Width = 266
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = Edit_NameChange
    end
    object CB_AbortOnError: TTntCheckBox
      Left = 10
      Top = 135
      Width = 263
      Height = 17
      Caption = '&Abort macro when error occurs'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object Button_Help: TTntButton
    Left = 175
    Top = 175
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button_HelpClick
  end
end
