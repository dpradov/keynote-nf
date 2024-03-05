object Form_NewNote: TForm_NewNote
  Left = 382
  Top = 333
  HelpContext = 317
  ActiveControl = Combo_TabName
  BorderStyle = bsDialog
  Caption = 'New note'
  ClientHeight = 153
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
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
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 15
    Width = 49
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Name:'
    FocusControl = Combo_TabName
  end
  object Label2: TLabel
    Left = 6
    Top = 45
    Width = 49
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Icon:'
  end
  object Label_Type: TLabel
    Left = 6
    Top = 75
    Width = 49
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '&Type:'
    FocusControl = Combo_TabType
  end
  object Button_OK: TButton
    Left = 15
    Top = 111
    Width = 75
    Height = 25
    Hint = 'Accept settings'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 95
    Top = 111
    Width = 75
    Height = 25
    Hint = 'Close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    OnClick = Button_CancelClick
  end
  object Combo_TabName: TComboBox
    Left = 60
    Top = 10
    Width = 237
    Height = 21
    Hint = 'Enter note name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = Combo_TabNameChange
    OnKeyPress = Combo_TabNameKeyPress
  end
  object Button_Properties: TButton
    Left = 216
    Top = 111
    Width = 84
    Height = 25
    Hint = 'Edit properties of the new note'
    Caption = '&Properties'
    TabOrder = 5
    OnClick = Button_PropertiesClick
  end
  object Combo_TabType: TComboBox
    Left = 60
    Top = 70
    Width = 237
    Height = 21
    Hint = 'Select type of note to create'
    Style = csDropDownList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnChange = Combo_TabNameChange
  end
  object Combo_Icons: TGFXComboBox
    Left = 60
    Top = 40
    Width = 80
    Height = 22
    Hint = 'Select icon for note'
    Extended = False
    TabOrder = 1
  end
end
