object Form_SelectTab: TForm_SelectTab
  Left = 424
  Top = 295
  BorderStyle = bsDialog
  Caption = 'Select Notes'
  ClientHeight = 243
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Button_OK: TButton
    Left = 213
    Top = 10
    Width = 108
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 213
    Top = 40
    Width = 108
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object Button_All: TButton
    Left = 213
    Top = 85
    Width = 108
    Height = 25
    Caption = 'Select &All'
    TabOrder = 2
    OnClick = Button_AllClick
  end
  object Button_None: TButton
    Left = 213
    Top = 115
    Width = 108
    Height = 25
    Caption = 'Select &None'
    TabOrder = 3
    OnClick = Button_NoneClick
  end
  object Button_Invert: TButton
    Left = 213
    Top = 145
    Width = 108
    Height = 25
    Caption = '&Invert selection'
    TabOrder = 4
    OnClick = Button_InvertClick
  end
  object List_Tabs: TGFXListBox
    Left = 10
    Top = 5
    Width = 191
    Height = 231
    ExtendedSelect = False
    ItemHeight = 20
    TabOrder = 5
    OnDblClick = List_TabsDblClick
  end
end
