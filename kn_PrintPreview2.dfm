object PreviewForm: TPreviewForm
  Left = 226
  Top = 172
  Caption = 'Print preview'
  ClientHeight = 446
  ClientWidth = 652
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 652
    Height = 29
    ButtonHeight = 21
    ButtonWidth = 28
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Hint = 'ToolButton1'
      Caption = 'Print'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 652
    Height = 417
    Align = alClient
    TabOrder = 1
    object PaintBox1: TPaintBox
      Left = 1
      Top = 1
      Width = 650
      Height = 415
      Align = alClient
      OnPaint = PaintBox1Paint
      ExplicitHeight = 419
    end
    object ScrollBar1: TScrollBar
      Left = 635
      Top = 0
      Width = 16
      Height = 421
      Ctl3D = True
      Kind = sbVertical
      PageSize = 0
      ParentCtl3D = False
      TabOrder = 0
      OnChange = ScrollBar1Change
    end
  end
  object PrintDialog1: TPrintDialog
    Left = 105
    Top = 242
  end
end
