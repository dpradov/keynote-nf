object Form_FindReplace: TForm_FindReplace
  Left = 390
  Top = 196
  ActiveControl = Combo_Text
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 217
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPage95Control
    Left = 0
    Top = 0
    Width = 490
    Height = 233
    ActivePage = Tab_Find
    FlatSeperators = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    ParentFont = False
    RemoveLastTab = True
    TabOrder = 9
    TabWidth = 80
    OnChange = PagesChange
    object Tab_Find: TTab95Sheet
      Caption = 'Find'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object TntLabel3: TTntLabel
        Left = 3
        Top = 16
        Width = 94
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '&Text to find:'
        FocusControl = Combo_Text
      end
    end
    object Tab_Replace: TTab95Sheet
      Caption = 'Replace'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object TntLabel1: TTntLabel
        Left = 3
        Top = 45
        Width = 94
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Replace &with:'
        FocusControl = Combo_Replace
      end
      object TntLabel2: TTntLabel
        Left = 3
        Top = 16
        Width = 94
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '&Text to find:'
        FocusControl = Combo_Text
      end
    end
  end
  object Button_Find: TTntButton
    Left = 356
    Top = 38
    Width = 117
    Height = 25
    Hint = 'Find next match'
    Caption = '&Find next'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_FindClick
  end
  object Button_Cancel: TTntButton
    Left = 356
    Top = 184
    Width = 117
    Height = 25
    Hint = 'Close this dialog box'
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 7
    OnClick = Button_CancelClick
  end
  object Combo_Text: TTntComboBox
    Left = 104
    Top = 39
    Width = 241
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    MaxLength = 255
    ParentFont = False
    TabOrder = 0
    OnChange = Combo_TextChange
  end
  object GroupBox_Opts: TTntGroupBox
    Left = 10
    Top = 95
    Width = 335
    Height = 114
    Caption = ' Options: '
    TabOrder = 8
    object CheckBox_MatchCase: TTntCheckBox
      Left = 194
      Top = 20
      Width = 135
      Height = 17
      Caption = '&Match case'
      TabOrder = 4
    end
    object CheckBox_EntireScope: TTntCheckBox
      Left = 25
      Top = 20
      Width = 151
      Height = 17
      Caption = 'From T&op of Text'
      TabOrder = 0
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_WholeWordsOnly: TTntCheckBox
      Left = 194
      Top = 39
      Width = 127
      Height = 17
      Caption = 'W&hole words'
      TabOrder = 5
    end
    object CheckBox_AllTabs: TTntCheckBox
      Left = 25
      Top = 46
      Width = 151
      Height = 17
      Caption = 'Search A&ll notes'
      TabOrder = 1
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_AllNodes: TTntCheckBox
      Left = 25
      Top = 65
      Width = 151
      Height = 17
      Caption = 'Search all tree &Nodes'
      TabOrder = 2
      OnClick = CheckBox_AllNodesClick
    end
    object CheckBox_HiddenNodes: TTntCheckBox
      Left = 25
      Top = 85
      Width = 151
      Height = 17
      Caption = 'Search Hi&dden nodes'
      Enabled = False
      TabOrder = 3
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_Wrap: TTntCheckBox
      Left = 194
      Top = 85
      Width = 134
      Height = 17
      Caption = '&Wrap in note'
      TabOrder = 6
    end
  end
  object Combo_Replace: TTntComboBox
    Left = 104
    Top = 68
    Width = 241
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 13
    MaxLength = 255
    ParentFont = False
    TabOrder = 1
    Visible = False
  end
  object Button_Replace: TTntButton
    Tag = 1
    Left = 356
    Top = 68
    Width = 117
    Height = 25
    Hint = 'Replace and find next match'
    Caption = '&Replace'
    TabOrder = 3
    Visible = False
    OnClick = Button_FindClick
  end
  object Button_ReplaceAll: TTntButton
    Tag = 2
    Left = 356
    Top = 100
    Width = 117
    Height = 25
    Hint = 'Find and replace all matching text'
    Caption = 'Replace &All'
    TabOrder = 4
    Visible = False
    OnClick = Button_FindClick
  end
  object CheckBox_SelectedText: TTntCheckBox
    Left = 360
    Top = 129
    Width = 125
    Height = 17
    Hint = 'Restrict replacement to selected text (apply to Replace All)'
    Caption = '&Selected Text'
    TabOrder = 5
    Visible = False
  end
  object CheckBox_Confirm: TTntCheckBox
    Left = 360
    Top = 148
    Width = 123
    Height = 17
    Caption = '&Confirm replace'
    TabOrder = 6
    Visible = False
  end
  object FormPlacement: TFormPlacement
    IniSection = 'ReplaceDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 311
    Top = 175
  end
end
