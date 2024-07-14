object Form_NodeNum: TForm_NodeNum
  Left = 308
  Top = 230
  HelpType = htKeyword
  HelpKeyword = '189-33'
  BorderStyle = bsDialog
  Caption = 'Outline node numbering'
  ClientHeight = 269
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnHelp = FormHelp
  TextHeight = 13
  object Label1: TLabel
    Left = 21
    Top = 187
    Width = 397
    Height = 26
    Caption = 
      'Nodes are renumbered automatically'#13#10'Names are never lost, they s' +
      'imply can be hidden, depending on numbering method'
    Color = clBtnFace
    FocusControl = Spin_Depth
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    StyleElements = [seClient, seBorder]
  end
  object Button_OK: TButton
    Left = 149
    Top = 234
    Width = 77
    Height = 25
    Hint = 'Accept settings and execute'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button_Cancel: TButton
    Left = 232
    Top = 234
    Width = 82
    Height = 25
    Hint = 'Cancel'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object RG_Scope: TRadioGroup
    Left = 8
    Top = 8
    Width = 450
    Height = 57
    Caption = ' &Numbering scope '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Current subtree'
      'Whole tree')
    TabOrder = 0
  end
  object RG_Method: TRadioGroup
    Left = 8
    Top = 74
    Width = 233
    Height = 102
    Hint = 
      'Example of numbers and node name:'#13#10'"1.2 - Node"'#13#10'Only number: "1' +
      '.2"'
    Caption = ' Numbering &method '
    ItemIndex = 0
    Items.Strings = (
      'Show numbers and node names'
      'Show only numbers'
      'Show only names (No numbering)')
    TabOrder = 1
    OnClick = RG_MethodClick
  end
  object gbDepth: TGroupBox
    Left = 247
    Top = 74
    Width = 211
    Height = 102
    Caption = ' Numbering depth'
    TabOrder = 2
    object LB_Depth: TLabel
      Left = 22
      Top = 61
      Width = 81
      Height = 13
      Caption = '&Limit level depth:'
      FocusControl = Spin_Depth
    end
    object Spin_Depth: TSpinEdit
      Left = 112
      Top = 56
      Width = 61
      Height = 22
      Enabled = False
      MaxLength = 4
      MaxValue = 9999
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object CB_FullDepth: TCheckBox
      Left = 22
      Top = 36
      Width = 186
      Height = 17
      Caption = '&Apply numbering to all levels'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CB_FullDepthClick
    end
  end
end
