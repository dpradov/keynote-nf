object KntTreeUI: TKntTreeUI
  Left = 0
  Top = 0
  Width = 175
  Height = 361
  TabOrder = 0
  object TV: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 175
    Height = 361
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
  object CheckImages: TImageList
    Left = 72
    Top = 280
  end
end
