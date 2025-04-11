object GFTipForm: TGFTipForm
  Left = 348
  Top = 228
  HelpContext = 10
  BorderStyle = bsDialog
  Caption = 'Tip of the Day'
  ClientHeight = 247
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object TipPanel: TPanel
    Left = 4
    Top = 5
    Width = 348
    Height = 207
    BevelOuter = bvLowered
    BevelWidth = 2
    Color = clInfoBk
    TabOrder = 0
    object TipLbl: TLabel
      Left = 24
      Top = 69
      Width = 309
      Height = 122
      AutoSize = False
      Caption = '...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
      Transparent = True
      WordWrap = True
    end
    object Image_Hint: TImage
      Left = 10
      Top = 17
      Width = 32
      Height = 32
      AutoSize = True
      Picture.Data = {
        07544269746D617076020000424D760200000000000076000000280000002000
        000020000000010004000000000000020000C40E0000C40E0000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00DDDDDDDDDDDDDDDD8DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD888DDDDDDDDDD
        DDDDDDDDDDDDDDDDDD88888DDDDDDDDDDDDDDDDDDDDDDDDDD8880888DDDDDDDD
        DDDDDDDDDDDDDDDD8880E0888DDDDDDDDDDDDDDDDDDDDDD8880EFE0888DDDDDD
        DDDDDDDDDDDDDD8880EFEFE0888DDDDDDDDDDDDDDDDDD8880EFEFEFE0888DDDD
        DDDDDDDDDDDD8880EFE000EFE0888DDDDDDDDDDDDDD8880EFE00000EFE0888DD
        DDDDDDDDDD8880EFE88F7700EFE0888DDDDDDDDDD8880EFEF8F00070FEFE0888
        DDDDDDDDDD80EFEF880F77000FEFE08DDDDDDDDDDD0EFEFE8FF000770EFEFE0D
        DDDDDDDDD0EFEFEF800333000FEFEFE0DDDDDDDDDD8EFEFE033BBB330EFEFE0D
        DDDDDDDDDDD8EFE03BBBBBBB30EFE0DDDDDDDDDDDDDD8EF03BBFBFBF30FE0DDD
        DDDDDDDDDDDDD803BBB808BBB300DDDDDDDDDDDDDDDDD03BBFB000BFBF30DDDD
        DDDDDDDDDDDDD0BBBBF808FBBB30DDDDDDDDDDDDDDDD3BBFBFBFBFBFBFB30DDD
        DDDDDDDDDDDD3BBBFBFB0BFBFBB30DDDDDDDDDDDDDDD3FBFFFFF0FBFBFB30DDD
        DDDDDDDDDDDD3BFFFFF808FBFBB30DDDDDDDDDDDDDDD3BFFFFF000BFBFB30DDD
        DDDDDDDDDDDDD3FFFFF000FBFBB0DDDDDDDDDDDDDDDDD3BFFFF000BFBFB0DDDD
        DDDDDDDDDDDDDD3BFFF808FBBB0DDDDDDDDDDDDDDDDDDDD3BFBFBFBFB3DDDDDD
        DDDDDDDDDDDDDDDD33BBBBB33DDDDDDDDDDDDDDDDDDDDDDDDD33333DDDDDDDDD
        DDDD}
      Transparent = True
    end
    object TipTitleLbl: TLabel
      Left = 62
      Top = 19
      Width = 9
      Height = 13
      Caption = '...'
      ShowAccelChar = False
    end
  end
  object ShowChk: TCheckBox
    Left = 6
    Top = 221
    Width = 310
    Height = 16
    Caption = ' &Show tips at startup'
    TabOrder = 1
  end
  object Button_OK: TButton
    Left = 358
    Top = 8
    Width = 89
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = Button_OKClick
  end
  object Button_Next: TButton
    Left = 358
    Top = 41
    Width = 89
    Height = 25
    Caption = '&Next Tip'
    TabOrder = 3
    OnClick = Button_NextClick
  end
  object Button_Prev: TButton
    Left = 358
    Top = 68
    Width = 89
    Height = 25
    Caption = '&Previous Tip'
    TabOrder = 4
    OnClick = Button_PrevClick
  end
end
