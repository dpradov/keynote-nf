object MinMaxInfoEditDialog: TMinMaxInfoEditDialog
  Left = 264
  Top = 158
  ActiveControl = OkBtn
  BorderStyle = bsDialog
  Caption = 'MinMaxInfo'
  ClientHeight = 163
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 3
    Top = 2
    Width = 306
    Height = 127
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 112
    Top = 15
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Left: '
  end
  object Label2: TLabel
    Left = 192
    Top = 15
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Top: '
  end
  object Label3: TLabel
    Left = 14
    Top = 15
    Width = 93
    Height = 13
    Caption = 'Maximize Position:  '
  end
  object Label4: TLabel
    Left = 112
    Top = 44
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label5: TLabel
    Left = 192
    Top = 44
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label6: TLabel
    Left = 14
    Top = 44
    Width = 75
    Height = 13
    Caption = 'Maximize Size:  '
  end
  object Label7: TLabel
    Left = 112
    Top = 73
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label8: TLabel
    Left = 192
    Top = 73
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label9: TLabel
    Left = 14
    Top = 73
    Width = 81
    Height = 13
    Caption = 'Max Track Size:  '
  end
  object Label10: TLabel
    Left = 112
    Top = 102
    Width = 35
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Width: '
  end
  object Label11: TLabel
    Left = 192
    Top = 102
    Width = 37
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Height: '
  end
  object Label12: TLabel
    Left = 14
    Top = 102
    Width = 77
    Height = 13
    Caption = 'Min Track Size:  '
  end
  object MaxPosBtn: TSpeedButton
    Tag = 1
    Left = 276
    Top = 9
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = #172
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MaxSizeBtn: TSpeedButton
    Tag = 2
    Left = 276
    Top = 38
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = #172
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MaxTrackBtn: TSpeedButton
    Tag = 3
    Left = 276
    Top = 67
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = #172
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MinTrackBtn: TSpeedButton
    Tag = 4
    Left = 276
    Top = 96
    Width = 25
    Height = 24
    Hint = 'Set from current'#13#10'form state|'
    Caption = #172
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
    Layout = blGlyphBottom
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = SetCurrentBtnClick
  end
  object MaxPosLeftEdit: TCurrencyEdit
    Left = 150
    Top = 11
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 0
  end
  object MaxPosTopEdit: TCurrencyEdit
    Left = 232
    Top = 11
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 1
  end
  object MaxSizeWidthEdit: TCurrencyEdit
    Left = 150
    Top = 40
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 2
  end
  object MaxSizeHeightEdit: TCurrencyEdit
    Left = 232
    Top = 40
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 3
  end
  object MaxTrackWidthEdit: TCurrencyEdit
    Left = 150
    Top = 69
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 4
  end
  object MaxTrackHeightEdit: TCurrencyEdit
    Left = 232
    Top = 69
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 5
  end
  object MinTrackWidthEdit: TCurrencyEdit
    Left = 150
    Top = 98
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 6
  end
  object MinTrackHeightEdit: TCurrencyEdit
    Left = 232
    Top = 98
    Width = 37
    Height = 21
    DecimalPlaces = 0
    DisplayFormat = '0'
    TabOrder = 7
  end
  object OkBtn: TButton
    Left = 163
    Top = 135
    Width = 70
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 9
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 239
    Top = 135
    Width = 70
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
  object ClearBtn: TButton
    Left = 5
    Top = 135
    Width = 70
    Height = 23
    Caption = '&Clear'
    TabOrder = 8
    OnClick = ClearBtnClick
  end
end
