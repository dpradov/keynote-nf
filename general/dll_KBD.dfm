object Form_KBD: TForm_KBD
  Left = 298
  Top = 223
  ActiveControl = List_Commands
  Anchors = [akLeft, akTop, akRight]
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Customize keyboard shortcuts'
  ClientHeight = 492
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
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
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  DesignSize = (
    465
    492)
  PixelsPerInch = 96
  TextHeight = 16
  object Btn_OK: TButton
    Left = 142
    Top = 457
    Width = 92
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 1
  end
  object Btn_Cancel: TButton
    Left = 248
    Top = 457
    Width = 93
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 0
    Width = 452
    Height = 444
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      452
      444)
    object LB_Cmd: TLabel
      Left = 11
      Top = 14
      Width = 72
      Height = 16
      Caption = '&Commands:'
      FocusControl = List_Commands
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LB_Shortcut: TLabel
      Left = 45
      Top = 307
      Width = 140
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '&New keyboard shortcut:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 45
      Top = 258
      Width = 94
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Current shortcut:'
      FocusControl = Edit_Current
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 50
      Top = 353
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
    end
    object LB_CurrentlyAssignedTo: TLabel
      Left = 172
      Top = 354
      Width = 33
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '(None)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      ExplicitTop = 372
    end
    object Label1: TLabel
      Left = 304
      Top = 37
      Width = 22
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Filter'
      FocusControl = Edit_Current
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object List_Commands: TListBox
      Left = 12
      Top = 62
      Width = 424
      Height = 119
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
      Left = 321
      Top = 326
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Assign'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Btn_AssignClick
    end
    object Btn_Remove: TButton
      Left = 321
      Top = 273
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = Btn_RemoveClick
    end
    object Btn_ResetAll: TButton
      Left = 321
      Top = 402
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'R&eset All'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = Btn_ResetAllClick
    end
    object Btn_List: TButton
      Left = 215
      Top = 402
      Width = 92
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&List'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = Btn_ListClick
    end
    object GroupBox2: TGroupBox
      Left = 13
      Top = 189
      Width = 424
      Height = 57
      Anchors = [akLeft, akRight, akBottom]
      Caption = ' Description '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      DesignSize = (
        424
        57)
      object LB_CmdHint: TLabel
        Left = 10
        Top = 19
        Width = 402
        Height = 29
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = '(no description)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        ExplicitHeight = 34
      end
    end
    object Edit_Current: TEdit
      Left = 45
      Top = 277
      Width = 252
      Height = 19
      TabStop = False
      Anchors = [akLeft, akBottom]
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 6
    end
    object Edit1: TEdit
      Left = 44
      Top = 328
      Width = 252
      Height = 21
      TabStop = False
      Anchors = [akLeft, akBottom]
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 8
      Visible = False
    end
    object Edit_Filter: TEdit
      Left = 335
      Top = 35
      Width = 101
      Height = 21
      TabStop = False
      Anchors = [akTop, akRight]
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      OnExit = Edit_FilterExit
      OnKeyDown = Edit_FilterKeyDown
    end
    object RBShowMainMenu: TRadioButton
      Left = 17
      Top = 41
      Width = 113
      Height = 17
      Caption = 'Main menu'
      Checked = True
      TabOrder = 9
      TabStop = True
      OnClick = RBShowMainMenuClick
    end
    object RBShowTreeMenu: TRadioButton
      Left = 136
      Top = 41
      Width = 145
      Height = 17
      Caption = 'Tree context menu'
      TabOrder = 10
      OnClick = RBShowTreeMenuClick
    end
  end
  object Btn_Help: TButton
    Left = 20
    Top = 402
    Width = 92
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Btn_HelpClick
  end
  object Menu_Items: TPopupMenu
    Left = 105
    Top = 75
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
