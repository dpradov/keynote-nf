object Form_DropFile: TForm_DropFile
  Left = 364
  Top = 312
  HelpType = htKeyword
  HelpKeyword = '312-7'
  BorderStyle = bsDialog
  Caption = 'Select import method'
  ClientHeight = 212
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnHelp = FormHelp
  DesignSize = (
    356
    212)
  TextHeight = 13
  object lblRenamed: TLabel
    Left = 8
    Top = 155
    Width = 60
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Renamed :'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = False
    Visible = False
    StyleElements = [seClient, seBorder]
  end
  object Button_OK: TButton
    Left = 10
    Top = 181
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button_Cancel: TButton
    Left = 90
    Top = 181
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
        OnClick = RG_ActionClick
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
        HelpType = htKeyword
        HelpKeyword = '312-5'
        TabOrder = 0
      end
    end
  end
  object Btn_HTML: TButton
    Left = 236
    Top = 181
    Width = 110
    Height = 25
    Caption = '&HTML options'
    TabOrder = 3
    OnClick = Btn_HTMLClick
  end
  object chk_ImageLinkMode: TCheckBox
    Left = 234
    Top = 155
    Width = 117
    Height = 17
    Caption = 'Images: Link mode'
    TabOrder = 4
    Visible = False
    OnClick = chk_ImageLinkModeClick
  end
  object txtImgNewName: TEdit
    Left = 77
    Top = 151
    Width = 128
    Height = 21
    Hint = 'The initial name is already being used'
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MaxLength = 127
    ParentFont = False
    TabOrder = 5
    Visible = False
    OnExit = txtImgNewNameExit
    ExplicitTop = 150
    ExplicitWidth = 124
  end
  object chk_Relative: TCheckBox
    Left = 264
    Top = 155
    Width = 87
    Height = 17
    Hint = 'Insert link as relative to .knt file'
    Caption = 'Relative'
    TabOrder = 6
    Visible = False
    OnClick = chk_ImageLinkModeClick
  end
end
