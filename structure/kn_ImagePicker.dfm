object Form_ImgPick: TForm_ImgPick
  Left = 318
  Top = 222
  BorderIcons = [biSystemMenu]
  Caption = 'Select node icon'
  ClientHeight = 293
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 428
    Height = 220
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 9
    BorderWidth = 5
    TabOrder = 0
    ExplicitWidth = 424
    ExplicitHeight = 219
    object List_Icn: TGFXListBox
      Left = 5
      Top = 5
      Width = 418
      Height = 210
      Align = alClient
      Columns = 4
      ExtendedSelect = False
      ItemHeight = 20
      TabOrder = 0
      OnDblClick = List_IcnDblClick
      ExplicitWidth = 414
      ExplicitHeight = 209
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 220
    Width = 428
    Height = 73
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 219
    ExplicitWidth = 424
    object Button_Cancel: TButton
      Left = 111
      Top = 38
      Width = 92
      Height = 31
      Hint = 'Close dialog box without selecting an image'
      Cancel = True
      Caption = 'Cancel'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ModalResult = 2
      ParentFont = False
      TabOrder = 0
    end
    object Button_OK: TButton
      Left = 12
      Top = 38
      Width = 93
      Height = 31
      Hint = 'Assign selected image'
      Caption = 'OK'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ModalResult = 1
      ParentFont = False
      TabOrder = 1
    end
    object CB_Children: TCheckBox
      Left = 12
      Top = 5
      Width = 297
      Height = 21
      Caption = '&Also use this icon for all child nodes'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object FormPlacement: TFormPlacement
    IniSection = 'NodeIconDlg'
    Options = [fpPosition]
    Left = 280
    Top = 240
  end
end
