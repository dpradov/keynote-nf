object Form_KBD: TForm_KBD
  Left = 298
  Top = 223
  ActiveControl = List_Commands
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Customize keyboard shortcuts'
  ClientHeight = 393
  ClientWidth = 551
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
  PixelsPerInch = 120
  TextHeight = 16
  object Btn_OK: TButton
    Left = 31
    Top = 356
    Width = 92
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Btn_Cancel: TButton
    Left = 135
    Top = 356
    Width = 93
    Height = 30
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 6
    Top = 0
    Width = 538
    Height = 348
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object LB_Cmd: TLabel
      Left = 12
      Top = 18
      Width = 72
      Height = 16
      Caption = '&Commands:'
      FocusControl = List_Commands
    end
    object LB_Shortcut: TLabel
      Left = 12
      Top = 193
      Width = 140
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '&New keyboard shortcut:'
    end
    object Label4: TLabel
      Left = 197
      Top = 193
      Width = 94
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Current shortcut:'
      FocusControl = Edit_Current
    end
    object Label5: TLabel
      Left = 12
      Top = 249
      Width = 128
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = 'Currently assigned to:'
    end
    object LB_CurrentlyAssignedTo: TLabel
      Left = 148
      Top = 249
      Width = 41
      Height = 16
      Anchors = [akLeft, akBottom]
      Caption = '(None)'
      ShowAccelChar = False
    end
    object List_Commands: TListBox
      Left = 12
      Top = 37
      Width = 409
      Height = 139
      Hint = 'Right-click for options'
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 16
      PopupMenu = Menu_Items
      TabOrder = 0
      OnClick = List_CommandsClick
    end
    object Btn_Assign: TButton
      Left = 432
      Top = 37
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Caption = '&Assign'
      TabOrder = 1
      OnClick = Btn_AssignClick
    end
    object Btn_Remove: TButton
      Left = 432
      Top = 74
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 2
      OnClick = Btn_RemoveClick
    end
    object Btn_ResetAll: TButton
      Left = 432
      Top = 111
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'R&eset All'
      TabOrder = 3
      OnClick = Btn_ResetAllClick
    end
    object Btn_List: TButton
      Left = 432
      Top = 148
      Width = 92
      Height = 30
      Anchors = [akTop, akRight]
      Caption = '&List'
      TabOrder = 4
      OnClick = Btn_ListClick
    end
    object GroupBox2: TGroupBox
      Left = 12
      Top = 271
      Width = 514
      Height = 64
      Anchors = [akLeft, akRight, akBottom]
      Caption = ' Description '
      TabOrder = 6
      object LB_CmdHint: TLabel
        Left = 12
        Top = 18
        Width = 491
        Height = 44
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Caption = '(no description)'
        WordWrap = True
      end
    end
    object Edit_Current: TEdit
      Left = 197
      Top = 212
      Width = 167
      Height = 24
      TabStop = False
      Anchors = [akLeft, akBottom]
      ReadOnly = True
      TabOrder = 5
    end
  end
  object Btn_Help: TButton
    Left = 240
    Top = 356
    Width = 92
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'Help'
    TabOrder = 2
    OnClick = Btn_HelpClick
  end
  object Menu_Items: TPopupMenu
    Left = 105
    Top = 75
    object MMSortbyName: TMenuItem
      Caption = 'Sort by Name'
      GroupIndex = 1
      RadioItem = True
      OnClick = MMSortbyNameClick
    end
    object MMSortbyMenuText: TMenuItem
      Caption = 'Sort by Menu Text'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = MMSortbyMenuTextClick
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
