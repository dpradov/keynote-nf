object Form_URLAction: TForm_URLAction
  Left = 306
  Top = 299
  ActiveControl = Edit_TextURL
  BorderStyle = bsDialog
  Caption = 'Choose action for URL'
  ClientHeight = 114
  ClientWidth = 598
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 23
    Height = 13
    Caption = '&URL:'
    FocusControl = Edit_URL
  end
  object Label2: TLabel
    Left = 8
    Top = 50
    Width = 26
    Height = 13
    Caption = '&Text:'
    FocusControl = Edit_TextURL
  end
  object Button_Copy: TButton
    Left = 282
    Top = 82
    Width = 97
    Height = 25
    Hint = 'Copy to clipboard'
    Caption = '&Copy'
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_CopyClick
  end
  object Button_Cancel: TButton
    Left = 499
    Top = 82
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Button_Open: TButton
    Left = 168
    Top = 82
    Width = 97
    Height = 25
    Caption = '&Open'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button_OpenClick
  end
  object Button_OpenNew: TButton
    Left = 24
    Top = 82
    Width = 137
    Height = 25
    Caption = 'Open in &New window'
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_OpenNewClick
  end
  object Edit_URL: TEdit
    Left = 64
    Top = 17
    Width = 529
    Height = 21
    TabOrder = 4
    OnExit = Edit_URLExit
  end
  object Edit_TextURL: TEdit
    Left = 64
    Top = 46
    Width = 529
    Height = 21
    TabOrder = 5
  end
  object Button_Modify: TButton
    Left = 384
    Top = 82
    Width = 97
    Height = 25
    Hint = 'Modify Hyperlink'
    Cancel = True
    Caption = '&Modify'
    ModalResult = 1
    TabOrder = 6
    OnClick = Button_ModifyClick
  end
end
