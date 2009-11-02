object Form1: TForm1
  Left = 192
  Top = 103
  Width = 537
  Height = 375
  Caption = 'Some Zlib practical examples'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 513
    Height = 145
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 7
    Top = 160
    Width = 514
    Height = 145
    Lines.Strings = (
      '')
    TabOrder = 1
  end
  object Compress: TButton
    Left = 112
    Top = 312
    Width = 97
    Height = 25
    Caption = 'Compress String'
    TabOrder = 2
    OnClick = CompressClick
  end
  object Open: TButton
    Left = 8
    Top = 312
    Width = 97
    Height = 25
    Caption = 'Open file'
    TabOrder = 3
    OnClick = OpenClick
  end
  object Browser: TButton
    Left = 216
    Top = 312
    Width = 97
    Height = 25
    Caption = 'SendToBrowser'
    TabOrder = 4
    OnClick = BrowserClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.txt'
    Filter = 'Text files|*.txt'
    InitialDir = 'c:\'
    Options = [ofReadOnly, ofHideReadOnly, ofEnableSizing]
    Left = 488
    Top = 312
  end
end
