object KntTreeUI: TKntTreeUI
  Left = 0
  Top = 0
  Width = 175
  Height = 397
  TabOrder = 0
  object TV: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 175
    Height = 374
    Align = alClient
    AutoExpandDelay = 700
    CustomCheckImages = CheckImages
    DragMode = dmAutomatic
    DragOperations = [doCopy, doMove, doLink]
    EditDelay = 700
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    HintMode = hmTooltip
    HotCursor = crHandPoint
    ParentFont = False
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
    Width = 175
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      175
      23)
    object TB_FilterTree: TToolbarButton97
      Left = 151
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
      Left = 126
      Top = 1
      Width = 24
      Height = 22
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 2
      Enabled = False
      RepeatInterval = 101
    end
    object txtFilter: TEdit
      Left = 24
      Top = 2
      Width = 100
      Height = 23
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object CheckImages: TImageList
    Left = 72
    Top = 280
  end
end
