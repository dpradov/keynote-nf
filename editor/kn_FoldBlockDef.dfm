object Form_FoldBlockDef: TForm_FoldBlockDef
  Left = 337
  Top = 311
  ActiveControl = Edit_Opening
  BorderStyle = bsDialog
  Caption = 'Folding block'
  ClientHeight = 96
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 15
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'Opening:'
    FocusControl = Edit_Opening
  end
  object Label2: TLabel
    Left = 10
    Top = 45
    Width = 71
    Height = 13
    AutoSize = False
    Caption = 'Closing:'
    FocusControl = Edit_Closing
  end
  object Button_OK: TButton
    Left = 302
    Top = 8
    Width = 80
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button_Cancel: TButton
    Left = 302
    Top = 38
    Width = 80
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit_Opening: TEdit
    Left = 86
    Top = 10
    Width = 200
    Height = 21
    MaxLength = 255
    TabOrder = 2
  end
  object Edit_Closing: TEdit
    Left = 86
    Top = 40
    Width = 200
    Height = 21
    MaxLength = 255
    TabOrder = 3
  end
  object chkCaseSens: TCheckBox
    Left = 8
    Top = 71
    Width = 113
    Height = 17
    Caption = 'Case sensitive'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chkDispos: TCheckBox
    Left = 129
    Top = 71
    Width = 128
    Height = 17
    Hint = 'Discard markers on fold'
    Caption = 'Discard markers'
    TabOrder = 5
    OnClick = chkDisposClick
  end
  object chkOnExpand: TCheckBox
    Left = 262
    Top = 71
    Width = 120
    Height = 17
    Hint = 
      'Markers will be added to the beginning and end of the text block' +
      ', on '#39'Expand'#39
    Caption = 'Use on Expand'
    Enabled = False
    TabOrder = 6
  end
end
