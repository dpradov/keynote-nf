object Form_KBD: TForm_KBD
  Left = 298
  Top = 223
  ActiveControl = List_Commands
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Customize keyboard shortcuts'
  ClientHeight = 501
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020100001000400E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000888888888888888888888888888888808F80
    00000000000000000000000000808F8777707707777777777707707770808F8F
    FF70F70FFFFFFFFFF70F70FF70808F8000000000000000000000000000808F87
    70770770770770770770777770808F8FF0F70F70F70F70F70F70FFFF70808F80
    00000000000000000000000000808F8777707707707707707707707770808F8F
    FF70F70F70F70F70F70F70F770808F8000000000000000000000000000808F87
    70770770770770770770770770808F8F70F70F70F70F70F70F70F70F70808F88
    88888888888888888888888888808FFFFFFFFFFFFFFFFFFFFFFFFFFFFF800888
    8888888888888888888888888880008000000000000000000000000000000080
    0000000000000000000000000000000800000000000000000000800000000000
    0800800800800800800008000000000000000000000000000000080000000000
    0000000000000000000000000000000000000000000000000080000000000000
    00000000000000000000000000000000000000000000008B3000000000000000
    0000000000000008B0000000000000000000000000000000800000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF800000010000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000080000001DFFFFFFFDFFFFFFFE492487FF924
    93BFFFFFFFBFFFFFFFBFFFFFFC7FFFFFE3FFFFFFC3FFFFFFE3FFFFFFF7FF}
  KeyPreview = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  DesignSize = (
    486
    501)
  TextHeight = 14
  object Btn_OK: TButton
    Left = 142
    Top = 468
    Width = 92
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
  end
  object Btn_Cancel: TButton
    Left = 248
    Top = 468
    Width = 93
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 5
    Width = 473
    Height = 454
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      473
      454)
    object LB_Cmd: TLabel
      Left = 11
      Top = 9
      Width = 63
      Height = 14
      Caption = '&Commands:'
      FocusControl = List_Commands
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LB_Shortcut: TLabel
      Left = 245
      Top = 284
      Width = 136
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '&New keyboard shortcut:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 18
      Top = 284
      Width = 98
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Current shortcut:'
      FocusControl = Edit_Current
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblAssig: TLabel
      Left = 244
      Top = 360
      Width = 119
      Height = 14
      Anchors = [akLeft, akBottom]
      Caption = 'Currently assigned to:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
    object LB_CurrentlyAssignedTo: TLabel
      Left = 244
      Top = 380
      Width = 217
      Height = 27
      Anchors = [akLeft, akBottom]
      AutoSize = False
      Caption = '(None)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      Visible = False
      WordWrap = True
    end
    object Label1: TLabel
      Left = 320
      Top = 10
      Width = 26
      Height = 14
      Anchors = [akTop, akRight]
      Caption = 'Filter'
      FocusControl = Edit_Current
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object List_Commands: TListBox
      Left = 151
      Top = 38
      Width = 306
      Height = 157
      Hint = 'Right-click for options'
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 14
      ParentFont = False
      PopupMenu = Menu_Items
      TabOrder = 1
      OnClick = List_CommandsClick
    end
    object Btn_Assign: TButton
      Left = 245
      Top = 331
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Assign'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Btn_AssignClick
    end
    object Btn_Remove: TButton
      Left = 18
      Top = 330
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = Btn_RemoveClick
    end
    object Btn_ResetAll: TButton
      Left = 365
      Top = 419
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'R&eset All'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = Btn_ResetAllClick
    end
    object Btn_List: TButton
      Left = 259
      Top = 419
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&List'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = Btn_ListClick
    end
    object GroupBox2: TGroupBox
      Left = 13
      Top = 219
      Width = 445
      Height = 57
      Anchors = [akLeft, akRight, akBottom]
      Caption = ' Description '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      DesignSize = (
        445
        57)
      object LB_CmdHint: TLabel
        Left = 16
        Top = 20
        Width = 417
        Height = 29
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = '(no description)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
    object Edit_Current: TEdit
      Left = 18
      Top = 303
      Width = 212
      Height = 19
      TabStop = False
      Anchors = [akLeft, akBottom]
      Ctl3D = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 6
    end
    object Edit_Filter: TEdit
      Left = 356
      Top = 7
      Width = 101
      Height = 22
      TabStop = False
      Anchors = [akTop, akRight]
      Ctl3D = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnExit = Edit_FilterExit
      OnKeyDown = Edit_FilterKeyDown
    end
    object RBShowMainMenu: TRadioButton
      Left = 12
      Top = 36
      Width = 113
      Height = 17
      Caption = 'Main menu'
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      TabStop = True
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowTreeMenu: TRadioButton
      Left = 12
      Top = 59
      Width = 134
      Height = 17
      Caption = 'Tree context menu'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowMacros: TRadioButton
      Left = 12
      Top = 87
      Width = 113
      Height = 17
      Caption = 'Execute Macro'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowPlugins: TRadioButton
      Left = 12
      Top = 110
      Width = 113
      Height = 17
      Caption = 'Execute Plugin'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowTemplates: TRadioButton
      Left = 12
      Top = 134
      Width = 113
      Height = 17
      Caption = 'Insert Template'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowStyles: TRadioButton
      Left = 12
      Top = 157
      Width = 113
      Height = 17
      Caption = 'Apply Style'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      OnClick = RBShowCommandsCategoryClick
    end
    object RBShowFonts: TRadioButton
      Left = 12
      Top = 181
      Width = 113
      Height = 17
      Caption = 'Apply Font'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      OnClick = RBShowCommandsCategoryClick
    end
    object Combo_Font: TFontComboBox
      Left = 151
      Top = 199
      Width = 306
      Height = 20
      UseFonts = True
      TabOrder = 15
      Visible = False
      OnChange = Combo_FontChange
    end
  end
  object Btn_Help: TButton
    Left = 23
    Top = 424
    Width = 92
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Btn_HelpClick
  end
  object Pnl: TPanel
    Left = 247
    Top = 303
    Width = 227
    Height = 29
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object Edit1: TEdit
      Left = 4
      Top = 3
      Width = 212
      Height = 24
      TabStop = False
      Ctl3D = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      Visible = False
    end
  end
  object Menu_Items: TPopupMenu
    Left = 433
    Top = 467
    object MMConsiderDescriptionOnFilter: TMenuItem
      Caption = 'Consider description when filtering'
      Checked = True
      GroupIndex = 1
      OnClick = MMConsiderDescriptionOnFilterClick
    end
    object N1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MMListUnassigned: TMenuItem
      Caption = 'List Unassigned Commands in HTML'
      GroupIndex = 1
      OnClick = MMListUnassignedClick
    end
  end
end
