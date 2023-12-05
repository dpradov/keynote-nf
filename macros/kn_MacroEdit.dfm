object Form_Macro: TForm_Macro
  Left = 496
  Top = 290
  HelpContext = 530
  ActiveControl = Edit_Name
  BorderStyle = bsDialog
  Caption = 'Edit macro'
  ClientHeight = 249
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Button_OK: TButton
    Left = 62
    Top = 217
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 142
    Top = 217
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 0
    Width = 351
    Height = 207
    TabOrder = 2
    object Label1: TLabel
      Left = 15
      Top = 9
      Width = 62
      Height = 13
      Caption = '&Macro name:'
      FocusControl = Edit_Name
    end
    object Label2: TLabel
      Left = 15
      Top = 60
      Width = 56
      Height = 13
      Caption = '&Description:'
      FocusControl = Edit_Desc
    end
    object Label3: TLabel
      Left = 15
      Top = 114
      Width = 62
      Height = 13
      AutoSize = False
      Caption = 'Modified:'
    end
    object LB_Date: TLabel
      Left = 81
      Top = 114
      Width = 12
      Height = 13
      Caption = '...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
    end
    object Label4: TLabel
      Left = 15
      Top = 134
      Width = 62
      Height = 13
      AutoSize = False
      Caption = 'Filename:'
    end
    object LB_FileName: TLabel
      Left = 81
      Top = 134
      Width = 253
      Height = 28
      AutoSize = False
      Caption = '...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      WordWrap = True
    end
    object Edit_Desc: TEdit
      Left = 15
      Top = 79
      Width = 319
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object Edit_Name: TEdit
      Left = 15
      Top = 28
      Width = 319
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = Edit_NameChange
    end
    object CB_AbortOnError: TCheckBox
      Left = 15
      Top = 181
      Width = 263
      Height = 17
      Caption = '&Abort macro when error occurs'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkProfile: TCheckBox
      Left = 15
      Top = 158
      Width = 263
      Height = 17
      Hint = 
        'Contained in profile'#39's macros folder'#13#10'Profile macros take preced' +
        'ence (when match filename)'
      Caption = '&Profile specific'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object Button_Help: TButton
    Left = 222
    Top = 217
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button_HelpClick
  end
end
