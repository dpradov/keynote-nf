object Form_ImgPick: TForm_ImgPick
  Left = 318
  Top = 222
  BorderIcons = [biSystemMenu]
  Caption = 'Select node icon'
  ClientHeight = 294
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 17
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 432
    Height = 221
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 9
    BorderWidth = 5
    TabOrder = 0
    object List_Icn: TGFXListBox
      Left = 5
      Top = 5
      Width = 422
      Height = 211
      Align = alClient
      Columns = 4
      ExtendedSelect = False
      ItemHeight = 20
      TabOrder = 0
      OnDblClick = List_IcnDblClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 221
    Width = 432
    Height = 73
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button_Cancel: TTntButton
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
    object Button_OK: TTntButton
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
    object CB_Children: TTntCheckBox
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
