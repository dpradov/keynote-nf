object Form_Para: TForm_Para
  Left = 254
  Top = 310
  HelpType = htKeyword
  HelpKeyword = '282-8'
  ActiveControl = RB_Align
  BorderStyle = bsDialog
  Caption = 'Paragraph properties'
  ClientHeight = 269
  ClientWidth = 413
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Button_OK: TButton
    Left = 135
    Top = 236
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 231
    Top = 236
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object GB_Spacing: TGroupBox
    Left = 4
    Top = 113
    Width = 244
    Height = 111
    Caption = ' Spacing '
    TabOrder = 2
    object Label4: TLabel
      Left = 13
      Top = 20
      Width = 62
      Height = 13
      Caption = 'Line &spacing:'
      FocusControl = Combo_Spc
    end
    object Label5: TLabel
      Left = 13
      Top = 80
      Width = 61
      Height = 13
      Caption = 'S&pace After:'
      FocusControl = Spin_SpcAft
    end
    object Label6: TLabel
      Left = 13
      Top = 50
      Width = 68
      Height = 13
      Caption = 'Space B&efore:'
      FocusControl = Spin_SpcBef
    end
    object Combo_Spc: TComboBox
      Left = 106
      Top = 15
      Width = 119
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Items.Strings = (
        'Single'
        'One and a Half'
        'Double')
    end
    object Spin_SpcBef: TSpinEdit
      Left = 106
      Top = 45
      Width = 51
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = Spin_SpcBefChange
    end
    object Spin_SpcAft: TSpinEdit
      Left = 106
      Top = 75
      Width = 51
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = Spin_SpcBefChange
    end
  end
  object RB_Align: TRadioGroup
    Left = 5
    Top = 3
    Width = 407
    Height = 41
    Caption = ' &Alignment '
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'Left'
      'Center'
      'Right'
      'Full')
    TabOrder = 0
  end
  object GB_Indent: TGroupBox
    Left = 4
    Top = 50
    Width = 408
    Height = 57
    Caption = ' Indent '
    TabOrder = 1
    object Label3: TLabel
      Left = 124
      Top = 24
      Width = 82
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Right:'
      FocusControl = Spin_Right
    end
    object Label2: TLabel
      Left = 7
      Top = 24
      Width = 60
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Left:'
      FocusControl = Spin_Left
    end
    object Label1: TLabel
      Left = 263
      Top = 24
      Width = 82
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&First:'
      FocusControl = Spin_First
    end
    object Spin_First: TSpinEdit
      Left = 349
      Top = 21
      Width = 47
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = Spin_SpcBefChange
    end
    object Spin_Left: TSpinEdit
      Left = 71
      Top = 21
      Width = 47
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = Spin_SpcBefChange
    end
    object Spin_Right: TSpinEdit
      Left = 210
      Top = 21
      Width = 47
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = Spin_SpcBefChange
    end
  end
  object GB_List: TGroupBox
    Left = 254
    Top = 113
    Width = 159
    Height = 111
    Caption = ' List '
    TabOrder = 3
    object CB_Bullets: TCheckBox
      Left = 13
      Top = 42
      Width = 140
      Height = 17
      Caption = '&Bulleted list'
      TabOrder = 1
      OnClick = CB_BulletsClick
    end
    object CB_Numbers: TCheckBox
      Left = 13
      Top = 19
      Width = 140
      Height = 17
      Caption = '&Numbered list'
      TabOrder = 0
      OnClick = CB_NumbersClick
    end
  end
  object FormPlacement: TFormPlacement
    IniSection = 'ParaDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 365
    Top = 181
  end
end
