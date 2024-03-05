object Form_FindReplace: TForm_FindReplace
  Left = 390
  Top = 196
  HelpContext = 479
  ActiveControl = Combo_Text
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 229
  ClientWidth = 506
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
    Top = 0
    Width = 509
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
      HelpContext = 479
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
      object TntLabel3: TLabel
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
      HelpType = htKeyword
      HelpKeyword = '479-4'
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
      object TntLabel1: TLabel
        Left = 2
        Top = 45
        Width = 94
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Replace &with:'
        FocusControl = Combo_Replace
      end
      object TntLabel2: TLabel
        Left = 2
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
  object Button_Find: TButton
    Left = 376
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
    Left = 376
    Top = 189
    Width = 117
    Height = 25
    Hint = 'Close this dialog box'
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 7
    OnClick = Button_CancelClick
  end
  object Combo_Text: TComboBox
    Left = 104
    Top = 39
    Width = 255
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
    Left = 10
    Top = 99
    Width = 354
    Height = 118
    Caption = ' Options: '
    TabOrder = 8
    object CheckBox_MatchCase: TCheckBox
      Left = 193
      Top = 25
      Width = 155
      Height = 17
      Caption = '&Match case'
      TabOrder = 4
    end
    object CheckBox_EntireScope: TCheckBox
      Left = 24
      Top = 25
      Width = 165
      Height = 17
      Caption = 'From T&op of Text'
      TabOrder = 0
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_WholeWordsOnly: TCheckBox
      Left = 193
      Top = 44
      Width = 155
      Height = 17
      Caption = 'W&hole words'
      TabOrder = 5
    end
    object CheckBox_AllTabs: TCheckBox
      Left = 24
      Top = 51
      Width = 165
      Height = 17
      Caption = 'Search A&ll notes'
      TabOrder = 1
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_AllNodes: TCheckBox
      Left = 24
      Top = 70
      Width = 165
      Height = 17
      Caption = 'Search all tree &Nodes'
      TabOrder = 2
      OnClick = CheckBox_AllNodesClick
    end
    object CheckBox_HiddenNodes: TCheckBox
      Left = 24
      Top = 90
      Width = 165
      Height = 17
      Caption = 'Search Hi&dden nodes'
      Enabled = False
      TabOrder = 3
      OnClick = CheckBox_ScopeChanged
    end
    object CheckBox_Wrap: TCheckBox
      Left = 193
      Top = 90
      Width = 155
      Height = 17
      Caption = '&Wrap in note'
      TabOrder = 6
    end
  end
  object Combo_Replace: TComboBox
    Left = 104
    Top = 68
    Width = 255
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
    Left = 376
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
    Left = 376
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
    Left = 377
    Top = 135
    Width = 127
    Height = 19
    Hint = 'Restrict replacement to selected text (apply to Replace All)'
    Caption = '&Selected Text'
    TabOrder = 5
    Visible = False
  end
  object CheckBox_Confirm: TCheckBox
    Left = 377
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
    Left = 311
    Top = 175
  end
end
