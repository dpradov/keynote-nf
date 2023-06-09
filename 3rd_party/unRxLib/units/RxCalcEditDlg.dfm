object FormRxCalcEdit: TFormRxCalcEdit
  Left = 515
  Top = 219
  BorderIcons = []
  BorderStyle = bsDialog
  ClientHeight = 72
  ClientWidth = 140
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelText: TLabel
    Left = 7
    Top = 0
    Width = 124
    Height = 13
    AutoSize = False
    Transparent = True
    WordWrap = True
  end
  object RxCalcEdit: TRxCalcEdit
    Left = 7
    Top = 20
    Width = 125
    Height = 19
    AutoSize = False
    CheckOnExit = True
    DecimalPlaces = 0
    DisplayFormat = '0'
    ButtonWidth = 17
    MaxValue = 10.000000000000000000
    NumGlyphs = 2
    TabOrder = 0
  end
  object BitBtnOK: TBitBtn
    Left = 7
    Top = 43
    Width = 60
    Height = 24
    TabOrder = 1
    Kind = bkOK
  end
  object BitBtnCancel: TBitBtn
    Left = 72
    Top = 43
    Width = 60
    Height = 24
    TabOrder = 2
    Kind = bkCancel
  end
end
