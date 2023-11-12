object Form_DropFile: TForm_DropFile
  Left = 364
  Top = 312
  BorderStyle = bsDialog
  Caption = 'Select import method'
  ClientHeight = 198
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  TextHeight = 13
  object Button_OK: TButton
    Left = 10
    Top = 171
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button_Cancel: TButton
    Left = 90
    Top = 171
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PagesImp: TNotebook
    Left = 0
    Top = 0
    Width = 356
    Height = 148
    Align = alTop
    PageIndex = 1
    TabOrder = 0
    ExplicitWidth = 352
    object PAGE_METHOD: TPage
      Left = 0
      Top = 0
      Caption = 'PAGE_METHOD'
      object RG_Action: TRadioGroup
        Left = 8
        Top = 17
        Width = 334
        Height = 125
        Margins.Top = 6
        Margins.Bottom = 6
        TabOrder = 0
      end
    end
    object PAGE_HTML: TPage
      Left = 0
      Top = 0
      Caption = 'PAGE_HTML'
      ExplicitWidth = 352
      object RG_HTML: TRadioGroup
        Left = 11
        Top = 17
        Width = 332
        Height = 125
        TabOrder = 0
      end
    end
  end
  object Btn_HTML: TButton
    Left = 236
    Top = 171
    Width = 110
    Height = 25
    Caption = '&HTML options'
    TabOrder = 3
    OnClick = Btn_HTMLClick
  end
  object chk_ImageLinkMode: TCheckBox
    Left = 229
    Top = 148
    Width = 117
    Height = 17
    Caption = 'Images: Link mode'
    TabOrder = 4
  end
end
