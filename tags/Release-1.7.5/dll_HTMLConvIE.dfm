object Form_HtmlConvIE: TForm_HtmlConvIE
  Left = 336
  Top = 243
  BorderIcons = [biMaximize]
  BorderStyle = bsDialog
  Caption = 'HTML to RTF'
  ClientHeight = 321
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 142
    Width = 384
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object RTF: TRxRichEdit
    Left = 0
    Top = 145
    Width = 384
    Height = 136
    Align = alBottom
    AllowInPlace = False
    Language = 3082
    TabOrder = 0
    WantReturns = False
    WordWrap = False
  end
  object IE: TWebBrowser
    Left = 0
    Top = 0
    Width = 384
    Height = 142
    Align = alClient
    TabOrder = 1
    ControlData = {
      4C000000B0270000AD0E00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Panel1: TPanel
    Left = 0
    Top = 281
    Width = 384
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button_ConvertRTF: TButton
      Left = 44
      Top = 10
      Width = 101
      Height = 25
      Caption = 'Convert to &RTF'
      Default = True
      TabOrder = 0
      OnClick = Button_ConvertRTFClick
    end
    object Button1: TButton
      Left = 264
      Top = 10
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object Button_ConvertText: TButton
      Left = 154
      Top = 10
      Width = 101
      Height = 25
      Caption = 'Convert to &Text'
      TabOrder = 2
      OnClick = Button_ConvertTextClick
    end
  end
end
