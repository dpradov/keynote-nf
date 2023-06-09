object Form_TermDef: TForm_TermDef
  Left = 337
  Top = 311
  HelpContext = 540
  ActiveControl = Edit_Term
  BorderStyle = bsDialog
  Caption = 'Edit Glossary Term'
  ClientHeight = 73
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 15
    Width = 71
    Height = 13
    AutoSize = False
    Caption = '&Shortcut:'
    FocusControl = Edit_Term
  end
  object Label2: TLabel
    Left = 10
    Top = 45
    Width = 71
    Height = 13
    AutoSize = False
    Caption = '&Expansion:'
    FocusControl = Edit_Exp
  end
  object Button_OK: TButton
    Left = 284
    Top = 8
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Button_Cancel: TButton
    Left = 284
    Top = 38
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Edit_Term: TEdit
    Left = 86
    Top = 10
    Width = 181
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 2
  end
  object Edit_Exp: TEdit
    Left = 86
    Top = 40
    Width = 181
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 255
    ParentFont = False
    TabOrder = 3
  end
end
