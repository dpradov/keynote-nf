object KntNoteUI: TKntNoteUI
  Left = 0
  Top = 0
  Width = 578
  Height = 480
  Align = alClient
  BiDiMode = bdLeftToRight
  Ctl3D = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentBiDiMode = False
  ParentCtl3D = False
  ParentFont = False
  TabOrder = 0
  OnResize = FrameResize
  object splL: TSplitter
    Left = 97
    Top = 0
    Height = 480
    OnMoved = splLMoved
    ExplicitLeft = 191
    ExplicitTop = -3
  end
  object pnlAuxC: TPanel
    Left = 100
    Top = 0
    Width = 478
    Height = 480
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlAuxC2: TPanel
      Left = 0
      Top = 0
      Width = 478
      Height = 480
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splT: TSplitter
        Left = 0
        Top = 65
        Width = 478
        Height = 3
        Cursor = crVSplit
        Align = alTop
        OnCanResize = splTCanResize
        OnMoved = splTMoved
        ExplicitTop = 97
        ExplicitWidth = 383
      end
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 478
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object splTC: TSplitter
          Left = 233
          Top = 0
          Height = 65
          OnMoved = splTCMoved
          ExplicitLeft = 57
          ExplicitHeight = 129
        end
        object pnlTL: TPanel
          Left = 0
          Top = 0
          Width = 233
          Height = 65
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitHeight = 129
        end
        object pnlTR: TPanel
          Left = 236
          Top = 0
          Width = 242
          Height = 65
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitHeight = 129
        end
      end
      object pnlAuxC3: TPanel
        Left = 0
        Top = 68
        Width = 478
        Height = 412
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 132
        ExplicitHeight = 348
        object splB: TSplitter
          Left = 0
          Top = 337
          Width = 478
          Height = 3
          Cursor = crVSplit
          Align = alTop
          OnMoved = splBMoved
          ExplicitTop = 97
          ExplicitWidth = 283
        end
        object pnlCenter: TPanel
          Left = 0
          Top = 0
          Width = 478
          Height = 337
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
        end
        object pnlBottom: TPanel
          Left = 0
          Top = 340
          Width = 478
          Height = 72
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitTop = 233
          ExplicitHeight = 115
          object splBC: TSplitter
            Left = 233
            Top = 0
            Height = 72
            OnMoved = splBCMoved
            ExplicitLeft = 128
            ExplicitTop = 72
            ExplicitHeight = 100
          end
          object pnlBL: TPanel
            Left = 0
            Top = 0
            Width = 233
            Height = 72
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            ExplicitHeight = 115
          end
          object pnlBR: TPanel
            Left = 236
            Top = 0
            Width = 242
            Height = 72
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            ExplicitHeight = 115
          end
        end
      end
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 97
    Height = 480
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
end
