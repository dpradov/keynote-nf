object Form_Plugins: TForm_Plugins
  Left = 284
  Top = 363
  HelpContext = 535
  BorderIcons = [biSystemMenu]
  Caption = 'Plugins'
  ClientHeight = 203
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 129
    Width = 360
    Height = 3
    Cursor = crVSplit
    Hint = 'Click and drag to resize panels'
    Align = alBottom
    ExplicitTop = 136
  end
  object Panel_List: TPanel
    Left = 0
    Top = 0
    Width = 360
    Height = 129
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object List: TListBox
      Left = 5
      Top = 5
      Width = 262
      Height = 119
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = ListClick
      OnDblClick = ListDblClick
    end
    object Panel_Btn: TPanel
      Left = 267
      Top = 5
      Width = 88
      Height = 119
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Button_Close: TButton
        Left = 10
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Close'
        ModalResult = 2
        TabOrder = 0
      end
      object Button_Config: TButton
        Left = 10
        Top = 35
        Width = 75
        Height = 25
        Caption = '&Configure'
        TabOrder = 1
        OnClick = Button_ConfigClick
      end
      object Button_Exec: TButton
        Left = 10
        Top = 5
        Width = 75
        Height = 25
        Caption = '&Execute'
        Default = True
        ModalResult = 1
        TabOrder = 2
        OnClick = ListDblClick
      end
      object Button_Help: TButton
        Left = 10
        Top = 95
        Width = 75
        Height = 25
        Caption = '&Help'
        TabOrder = 3
        OnClick = Button_HelpClick
      end
    end
  end
  object Panel_ResPlugins: TPanel
    Left = 0
    Top = 132
    Width = 360
    Height = 71
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
  end
  object FormPlacement: TFormStorage
    IniSection = 'PluginDlg'
    Options = [fpPosition]
    StoredProps.Strings = (
      'Panel_ResPlugins.Height')
    StoredValues = <>
    Left = 80
    Top = 24
  end
end
