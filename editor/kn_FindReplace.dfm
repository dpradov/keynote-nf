object Form_FindReplace: TForm_FindReplace
  Left = 390
  Top = 196
  HelpContext = 40
  ActiveControl = Combo_Text
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 236
  ClientWidth = 540
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Pages: TPage95Control
    Left = 0
    Top = 2
    Width = 537
    Height = 233
    ActivePage = Tab_Find
    FlatSeperators = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clBlack
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    ParentFont = False
    RemoveLastTab = True
    TabOrder = 9
    TabWidth = 80
    OnChange = PagesChange
    object Tab_Find: TTab95Sheet
      HelpContext = 40
      Caption = 'Find'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      ParentFont = False
      TabVisible = True
      ExplicitWidth = 504
      object TntLabel3: TLabel
        Left = 3
        Top = 16
        Width = 119
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '&Text to find:'
        FocusControl = Combo_Text
      end
    end
    object Tab_Replace: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '40-4'
      Caption = ' Replace'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      ParentFont = False
      TabVisible = True
      ExplicitWidth = 504
      object TntLabel1: TLabel
        Left = 2
        Top = 45
        Width = 116
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Replace &with:'
        FocusControl = Combo_Replace
      end
      object TntLabel2: TLabel
        Left = 3
        Top = 16
        Width = 116
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = '&Text to find:'
        FocusControl = Combo_Text
      end
    end
  end
  object Button_Find: TButton
    Left = 407
    Top = 40
    Width = 117
    Height = 25
    Hint = 'Find next match'
    Caption = '&Find next'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_FindClick
  end
  object Button_Cancel: TButton
    Left = 407
    Top = 195
    Width = 117
    Height = 25
    Hint = 'Close this dialog box'
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 7
    OnClick = Button_CancelClick
  end
  object Combo_Text: TComboBox
    Left = 132
    Top = 39
    Width = 257
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 0
    OnChange = Combo_TextChange
  end
  object GroupBox_Opts: TGroupBox
    Left = 9
    Top = 95
    Width = 387
    Height = 132
    Caption = ' Options: '
    TabOrder = 8
    object CheckBox_MatchCase: TCheckBox
      Left = 212
      Top = 23
      Width = 170
      Height = 17
      Caption = '&Match case'
      TabOrder = 5
    end
    object CheckBox_EntireScope: TCheckBox
      Left = 212
      Top = 86
      Width = 170
      Height = 17
      Caption = 'From T&op of Text'
      TabOrder = 7
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_WholeWordsOnly: TCheckBox
      Left = 212
      Top = 46
      Width = 170
      Height = 17
      Caption = 'W&hole words'
      TabOrder = 6
    end
    object CheckBox_AllTabs: TCheckBox
      Left = 10
      Top = 23
      Width = 200
      Height = 17
      Caption = 'Search A&ll folders'
      TabOrder = 0
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_AllNodes: TCheckBox
      Left = 10
      Top = 44
      Width = 200
      Height = 17
      Caption = 'Search all tree &Nodes'
      TabOrder = 1
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_HiddenNodes: TCheckBox
      Left = 10
      Top = 87
      Width = 200
      Height = 17
      Caption = 'Search Hi&dden Nodes'
      Enabled = False
      TabOrder = 3
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_Wrap: TCheckBox
      Left = 212
      Top = 108
      Width = 170
      Height = 17
      Caption = '&Wrap'
      TabOrder = 8
    end
    object CB_AllEntries: TCheckBox
      Left = 10
      Top = 65
      Width = 200
      Height = 17
      Caption = 'Search all Entries'
      TabOrder = 2
      OnClick = CheckBox_ScopeChanged
    end
    object CB_HiddenEntries: TCheckBox
      Left = 10
      Top = 108
      Width = 200
      Height = 17
      Caption = 'Search Hidden Entries'
      Enabled = False
      TabOrder = 4
      OnClick = CheckBox_ScopeChanged
    end
  end
  object Combo_Replace: TComboBox
    Left = 132
    Top = 68
    Width = 257
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 1
    Visible = False
  end
  object Button_Replace: TButton
    Tag = 1
    Left = 407
    Top = 70
    Width = 117
    Height = 25
    Hint = 'Replace and find next match'
    Caption = '&Replace'
    TabOrder = 3
    Visible = False
    OnClick = Button_FindClick
  end
  object Button_ReplaceAll: TButton
    Tag = 2
    Left = 407
    Top = 102
    Width = 117
    Height = 25
    Hint = 'Find and replace all matching text'
    Caption = 'Replace &All'
    TabOrder = 4
    Visible = False
    OnClick = Button_FindClick
  end
  object CheckBox_SelectedText: TCheckBox
    Left = 405
    Top = 135
    Width = 127
    Height = 19
    Hint = 'Restrict replacement to selected text (apply to Replace All)'
    Caption = '&Selected Text'
    TabOrder = 5
    Visible = False
  end
  object CheckBox_Confirm: TCheckBox
    Left = 405
    Top = 154
    Width = 127
    Height = 19
    Caption = '&Confirm replace'
    TabOrder = 6
    Visible = False
  end
  object FormPlacement: TFormPlacement
    IniSection = 'ReplaceDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 455
    Top = 7
  end
end
