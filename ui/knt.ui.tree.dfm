object KntTreeUI: TKntTreeUI
  Left = 0
  Top = 0
  Width = 130
  Height = 397
  TabOrder = 0
  object TV: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 130
    Height = 374
    Align = alClient
    AutoExpandDelay = 700
    CustomCheckImages = CheckImages
    DragMode = dmAutomatic
    DragOperations = [doCopy, doMove, doLink]
    EditDelay = 700
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    HintMode = hmTooltip
    HotCursor = crHandPoint
    ParentBiDiMode = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
  object PnlInf: TPanel
    Left = 0
    Top = 374
    Width = 130
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      130
      23)
    object TB_FilterTree: TToolbarButton97
      Left = 106
      Top = 1
      Width = 24
      Height = 22
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 2
      Enabled = False
      ImageIndex = 49
      Images = Form_Main.IMG_Toolbar
      RepeatInterval = 101
    end
    object TB_HideChecked: TToolbarButton97
      Left = 0
      Top = 1
      Width = 24
      Height = 22
      AllowAllUp = True
      GroupIndex = 1
      ImageIndex = 37
      Images = Form_Main.IMG_Toolbar
      RepeatInterval = 101
    end
    object TB_FilterUnflagged: TToolbarButton97
      Left = 81
      Top = 1
      Width = 24
      Height = 22
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 3
      Enabled = False
      RepeatInterval = 101
    end
    object txtFilter: TEdit
      Left = 49
      Top = 2
      Width = 30
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
    end
    object txtTags: TEdit
      Left = 24
      Top = 2
      Width = 24
      Height = 20
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object CheckImages: TImageList
    Left = 72
    Top = 280
  end
end
