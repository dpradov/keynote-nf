object Form_Main: TForm_Main
  Left = 188
  Top = 178
  HelpContext = 2
  Caption = 'KeyNote'
  ClientHeight = 794
  ClientWidth = 1005
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = Menu_Main
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnResize = TntFormResize
  OnShortCut = FormShortCut
  OnShow = FormShow
  TextHeight = 13
  object Splitter_Res: TSplitter
    Left = 792
    Top = 54
    Height = 695
    Hint = 'Click and drag to resize panels'
    Align = alRight
    Color = clBtnFace
    MinSize = 100
    ParentColor = False
    OnMoved = Splitter_ResMoved
    ExplicitLeft = 564
    ExplicitHeight = 374
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 775
    Width = 1005
    Height = 19
    Panels = <
      item
        Width = 6
      end
      item
        Style = psOwnerDraw
        Width = 4
      end
      item
        Width = 115
      end
      item
        Width = 35
      end
      item
        Width = 50
      end
      item
        Width = 35
      end
      item
        Width = 50
      end>
    OnDblClick = StatusBarDblClick
    OnDrawPanel = StatusBarDrawPanel
    ExplicitTop = 774
    ExplicitWidth = 1001
  end
  object Dock_Top: TDock97
    Left = 0
    Top = 0
    Width = 1005
    Height = 54
    BoundLines = [blTop, blBottom]
    FixAlign = True
    ExplicitWidth = 1001
    object Toolbar_Main: TToolbar97
      Tag = 1
      Left = 0
      Top = 0
      HelpContext = 305
      ActivateParent = False
      Caption = 'Main Toolbar'
      DefaultDock = Dock_Top
      DockableTo = [dpTop, dpBottom, dpLeft]
      DockPos = 0
      DragHandleStyle = dhSingle
      TabOrder = 0
      OnClose = Toolbar_FormatClose
      object TB_FileSave: TToolbarButton97
        Left = 60
        Top = 0
        Width = 23
        Height = 22
        Action = actFileSave
        DisplayMode = dmGlyphOnly
        Images = IMG_Toolbar
      end
      object TB_FileNew: TToolbarButton97
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 0
        Images = IMG_Toolbar
        NoBorder = True
        OnClick = MMFileNewClick
      end
      object TB_FileOpen: TToolbarButton97
        Left = 23
        Top = 0
        Width = 37
        Height = 22
        HelpContext = 539
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = MRUMenu
        ImageIndex = 1
        Images = IMG_Toolbar
        OnClick = MMFileOpenClick
      end
      object TB_EditCut: TToolbarButton97
        Left = 148
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 3
        Images = IMG_Toolbar
        OnClick = MMEditCutClick
      end
      object sm3: TToolbarSep97
        Left = 142
        Top = 0
      end
      object sm4: TToolbarSep97
        Left = 231
        Top = 0
      end
      object TB_EditUndo: TToolbarButton97
        Left = 237
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 6
        Images = IMG_Toolbar
        OnClick = MMEditUndoClick
      end
      object TB_EditCopy: TToolbarButton97
        Left = 171
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 4
        Images = IMG_Toolbar
        OnClick = MMEditCopyClick
      end
      object TB_EditPaste: TToolbarButton97
        Left = 194
        Top = 0
        Width = 37
        Height = 22
        HelpContext = 582
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Paste
        ImageIndex = 5
        Images = IMG_Toolbar
        OnClick = MMEditPasteClick
      end
      object TB_NoteDelete: TToolbarButton97
        Left = 357
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 9
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMFolderRemoveClick
      end
      object sm5: TToolbarSep97
        Left = 306
        Top = 0
      end
      object TB_NoteNew: TToolbarButton97
        Left = 312
        Top = 0
        Width = 22
        Height = 22
        ImageIndex = 7
        Images = IMG_Toolbar
        OnClick = MMFolderNewClick
      end
      object TB_NoteEdit: TToolbarButton97
        Left = 334
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 8
        Images = IMG_Toolbar
        OnClick = MMNotePropertiesClick
      end
      object sm9: TToolbarSep97
        Left = 663
        Top = 0
      end
      object TB_Options: TToolbarButton97
        Left = 669
        Top = 0
        Width = 22
        Height = 22
        ImageIndex = 10
        Images = IMG_Toolbar
        OnClick = MMToolsOptionsClick
      end
      object sm10: TToolbarSep97
        Left = 777
        Top = 0
      end
      object TB_EditRedo: TToolbarButton97
        Left = 260
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 13
        Images = IMG_Toolbar
        OnClick = MMEditRedoClick
      end
      object TB_FileMgr: TToolbarButton97
        Left = 118
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 12
        Images = IMG_Toolbar
        OnClick = MMFileManagerClick
      end
      object sm2: TToolbarSep97
        Left = 112
        Top = 0
      end
      object TB_FindNext: TToolbarButton97
        Left = 410
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 15
        Images = IMG_Toolbar
        OnClick = MMFindNextClick
      end
      object TB_Find: TToolbarButton97
        Left = 387
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 14
        Images = IMG_Toolbar
        OnClick = MMFindClick
      end
      object sm6: TToolbarSep97
        Left = 381
        Top = 0
      end
      object TB_FileInfo: TToolbarButton97
        Left = 89
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 16
        Images = IMG_Toolbar
        OnClick = MMFilePropertiesClick
      end
      object sm1: TToolbarSep97
        Left = 83
        Top = 0
      end
      object TB_EmailNote: TToolbarButton97
        Left = 640
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 17
        Images = IMG_Toolbar
        OnClick = MMNoteEmailClick
      end
      object TB_ClipCap: TToolbarButton97
        Left = 516
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        ImageIndex = 18
        Images = IMG_Toolbar
        OnClick = TB_ClipCapClick
      end
      object sm8: TToolbarSep97
        Left = 510
        Top = 0
      end
      object TB_WordWeb: TToolbarButton97
        Left = 463
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 24
        Images = IMG_Toolbar
        OnClick = RTFMWordWebClick
      end
      object sm7: TToolbarSep97
        Left = 457
        Top = 0
      end
      object TB_Repeat: TToolbarButton97
        Left = 283
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 40
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMEditRepeatClick
      end
      object TB_ResPanel: TToolbarButton97
        Left = 691
        Top = 0
        Width = 38
        Height = 22
        HelpType = htKeyword
        HelpKeyword = '11-1'
        AllowAllUp = True
        GroupIndex = 4
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_ResPanel
        ImageIndex = 41
        Images = IMG_Toolbar
        OnClick = MMViewResPanelClick
      end
      object TB_Print: TToolbarButton97
        Left = 617
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 44
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMNotePrintClick
      end
      object TB_Spell: TToolbarButton97
        Left = 487
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 43
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMNoteSpellClick
      end
      object TB_Replace: TToolbarButton97
        Left = 434
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 42
        Images = IMG_Toolbar
        OnClick = MMFindReplaceClick
      end
      object TB_OnTop: TToolbarButton97
        Left = 539
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 15
        ImageIndex = 45
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMViewOnTopClick
      end
      object TB_AlarmMode: TToolbarButton97
        Left = 753
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 3
        ImageIndex = 51
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TB_AlarmModeClick
        OnMouseEnter = TB_AlarmModeMouseEnter
      end
      object TB_SetAlarm: TToolbarButton97
        Left = 729
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 17
        ImageIndex = 50
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TB_SetAlarmClick
        OnMouseEnter = TB_SetAlarmMouseEnter
      end
      object TB_Images: TToolbarButton97
        Left = 783
        Top = 0
        Width = 23
        Height = 22
        Hint = 
          'Show or hide images (Ctrl: Reapply [hidden] /  Reload  -- Alt: R' +
          'econsider "Max.auto width")'
        AllowAllUp = True
        GroupIndex = 16
        ImageIndex = 59
        Images = IMG_Toolbar
        OnClick = TB_ImagesClick
      end
      object Combo_Zoom: TComboBox
        Left = 562
        Top = 0
        Width = 55
        Height = 21
        Hint = 'Zoom text in editor (Ctrl: apply only to active folder)'
        HelpType = htKeyword
        HelpKeyword = '282-5'
        AutoComplete = False
        TabOrder = 0
        OnClick = Combo_FontSizeClick
        OnDblClick = Combo_ZoomDblClick
        OnExit = Combo_ZoomExit
        OnKeyDown = Combo_FontSizeKeyDown
        OnKeyPress = Combo_FontSizeKeyPress
        Items.Strings = (
          '1000%'
          '750%'
          '500%'
          '400%'
          '300%'
          '250%'
          '200%'
          '150%'
          '125%'
          '110%'
          '105%'
          '100%'
          '95%'
          '90%'
          '85%'
          '80%'
          '75%'
          '70%'
          '50%'
          '25%'
          '10%')
      end
    end
    object Toolbar_Format: TToolbar97
      Tag = 2
      Left = 0
      Top = 26
      HelpContext = 128
      Caption = 'Formatting Toolbar'
      DefaultDock = Dock_Top
      DockableTo = [dpTop, dpBottom]
      DockPos = 0
      DockRow = 1
      DragHandleStyle = dhSingle
      TabOrder = 1
      OnClose = Toolbar_FormatClose
      object sf1: TToolbarSep97
        Left = 70
        Top = 0
      end
      object sf2: TToolbarSep97
        Left = 144
        Top = 0
      end
      object TB_AlignLeft: TToolbarButton97
        Left = 498
        Top = 0
        Width = 24
        Height = 22
        GroupIndex = 2
        ImageIndex = 4
        Images = IMG_Format
        OnClick = MMFormatAlignLeftClick
      end
      object TB_Bold: TToolbarButton97
        Left = 354
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 5
        ImageIndex = 0
        Images = IMG_Format
        RepeatInterval = 101
        OnClick = MMFormatBoldClick
      end
      object TB_Italics: TToolbarButton97
        Left = 378
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 6
        ImageIndex = 1
        Images = IMG_Format
        OnClick = MMFormatItalicsClick
      end
      object TB_Underline: TToolbarButton97
        Left = 401
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 7
        ImageIndex = 2
        Images = IMG_Format
        OnClick = MMFormatUnderlineClick
      end
      object sf3: TToolbarSep97
        Left = 492
        Top = 0
      end
      object TB_Strikeout: TToolbarButton97
        Left = 423
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 8
        ImageIndex = 3
        Images = IMG_Format
        OnClick = MMFormatStrikeoutClick
      end
      object TB_Bullets: TToolbarButton97
        Left = 597
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        ImageIndex = 7
        Images = IMG_Format
        OnClick = MMFormatBulletsClick
      end
      object TB_AlignCenter: TToolbarButton97
        Left = 522
        Top = 0
        Width = 23
        Height = 22
        GroupIndex = 2
        ImageIndex = 5
        Images = IMG_Format
        OnClick = MMFormatAlignCenterClick
      end
      object sf4: TToolbarSep97
        Left = 591
        Top = 0
      end
      object TB_AlignRight: TToolbarButton97
        Left = 545
        Top = 0
        Width = 22
        Height = 22
        GroupIndex = 2
        ImageIndex = 6
        Images = IMG_Format
        OnClick = MMFormatAlignRightClick
      end
      object TB_Outdent: TToolbarButton97
        Left = 762
        Top = 0
        Width = 23
        Height = 22
        ImageIndex = 9
        Images = IMG_Format
        OnClick = MMFormatLIndDecClick
      end
      object TB_Indent: TToolbarButton97
        Left = 785
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 8
        Images = IMG_Format
        OnClick = MMFormatLIndIncClick
      end
      object sf7: TToolbarSep97
        Left = 809
        Top = 0
      end
      object TB_FontDlg: TToolbarButton97
        Left = 815
        Top = 0
        Width = 22
        Height = 22
        ImageIndex = 11
        Images = IMG_Format
        OnClick = MMFormatFontClick
      end
      object TB_ParaDlg: TToolbarButton97
        Left = 837
        Top = 0
        Width = 24
        Height = 22
        ImageIndex = 13
        Images = IMG_Format
        OnClick = MMFormatParagraphClick
      end
      object TB_Numbers: TToolbarButton97
        Left = 619
        Top = 0
        Width = 38
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Numbers
        ImageIndex = 14
        Images = IMG_Format
        OnClick = MMFormatNumbersClick
      end
      object sf6: TToolbarSep97
        Left = 756
        Top = 0
      end
      object TB_Space2: TToolbarButton97
        Left = 732
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        ImageIndex = 17
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS2Click
      end
      object TB_Space1: TToolbarButton97
        Left = 686
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        ImageIndex = 15
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS1Click
      end
      object TB_Space15: TToolbarButton97
        Left = 709
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        ImageIndex = 16
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS15Click
      end
      object TB_WordWrap: TToolbarButton97
        Left = 657
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 3
        ImageIndex = 18
        Images = IMG_Format
        OnClick = MMFormatWordWrapClick
      end
      object sf5: TToolbarSep97
        Left = 680
        Top = 0
      end
      object TB_Subscript: TToolbarButton97
        Left = 470
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 9
        ImageIndex = 20
        Images = IMG_Format
        OnClick = MMFormatSubscriptClick
      end
      object TB_Superscript: TToolbarButton97
        Left = 446
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 9
        ImageIndex = 19
        Images = IMG_Format
        OnClick = MMFormatSuperscriptClick
      end
      object TB_AlignJustify: TToolbarButton97
        Left = 567
        Top = 0
        Width = 24
        Height = 22
        GroupIndex = 2
        ImageIndex = 21
        Images = IMG_Format
        OnClick = MMFormatAlignJustifyClick
      end
      object TB_CopyFormat: TToolbarButton97
        Left = 46
        Top = 0
        Width = 24
        Height = 22
        Hint = 
          'Copy the formatting from of a text and apply it to another (Ctrl' +
          '+click for applying it several times)'
        AllowAllUp = True
        GroupIndex = 5
        ImageIndex = 22
        Images = IMG_Format
        RepeatInterval = 101
        OnClick = TB_CopyFormatClick
      end
      object TB_GoForward: TToolbarButton97
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        HelpContext = 511
        Enabled = False
        ImageIndex = 39
        Images = IMG_Toolbar
        OnClick = MMHistoryGoForwardClick
      end
      object TB_GoBack: TToolbarButton97
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        HelpContext = 511
        Enabled = False
        ImageIndex = 53
        Images = IMG_Toolbar
        OnClick = MMHistoryGoBackClick
      end
      object ToolbarSep971: TToolbarSep97
        Left = 291
        Top = 0
      end
      object ToolbarSep972: TToolbarSep97
        Left = 348
        Top = 0
      end
      object Combo_Font: TFontComboBox
        Left = 150
        Top = 1
        Width = 141
        Height = 20
        Hint = 'Select font face'
        TabOrder = 0
        TabStop = False
        OnKeyDown = Combo_FontKeyDown
      end
      object TB_Color: TColorBtn
        Left = 76
        Top = 0
        Width = 34
        Height = 22
        Hint = 'Click to change text color'
        HelpKeyword = '282-14'
        HelpContext = 115
        ActiveColor = clBlack
        TargetColor = clBlack
        Flat = True
        DropDownFlat = True
        AutomaticColor = clWindowText
        IsAutomatic = True
        OnClick = TB_ColorClick
        AutoBtnCaption = 'Default color'
        OtherBtnCaption = '&Other colors...'
        RegKey = 'General Frenetics\KeyNote\ColorBtn1'
        DDArrowWidth = 12
      end
      object TB_Hilite: TColorBtn
        Left = 110
        Top = 0
        Width = 34
        Height = 22
        Hint = 'Click to add or remove highlight'
        HelpType = htKeyword
        HelpKeyword = '282-14'
        ActiveColor = clInfoBk
        TargetColor = clBlack
        Flat = True
        DropDownFlat = True
        AutomaticColor = clWindow
        IsAutomatic = True
        OnClick = TB_HiliteClick
        GlyphType = gtBackground
        AutoBtnCaption = 'No Highlight'
        OtherBtnCaption = '&Other colors...'
        RegKey = 'General Frenetics\KeyNote\ColorBtn2'
        DDArrowWidth = 12
      end
      object Combo_FontSize: TComboBox
        Left = 297
        Top = 0
        Width = 51
        Height = 21
        DropDownCount = 16
        TabOrder = 3
        OnExit = Combo_FontSizeExit
        OnKeyDown = Combo_FontSizeKeyDown
        OnKeyPress = Combo_FontSizeKeyPress
        Items.Strings = (
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '16'
          '18'
          '20'
          '22'
          '24'
          '26'
          '28'
          '32'
          '36'
          '40'
          '48'
          '72')
      end
    end
  end
  object Pages: TPage95Control
    Left = 9
    Top = 54
    Width = 783
    Height = 695
    HelpContext = 282
    AllowTabShifting = True
    Align = alClient
    FlatSeperators = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    HotTrack = False
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    ParentFont = False
    PopupMenu = Menu_TAB
    RemoveLastTab = True
    TabOrder = 1
    OnChange = PagesChange
    OnDragOver = PagesDragOver
    OnMouseDown = PagesMouseDown
    OnTabShift = PagesTabShift
  end
  object Dock_Left: TDock97
    Left = 0
    Top = 54
    Width = 9
    Height = 695
    LimitToOneRow = True
    Position = dpLeft
    ExplicitHeight = 694
  end
  object Dock_Bottom: TDock97
    Left = 0
    Top = 749
    Width = 1005
    Height = 26
    Position = dpBottom
    ExplicitTop = 748
    ExplicitWidth = 1001
    object Toolbar_Style: TToolbar97
      Tag = 3
      Left = 250
      Top = 0
      ActivateParent = False
      Caption = 'Style Toolbar'
      DefaultDock = Dock_Bottom
      DockPos = 250
      DragHandleStyle = dhSingle
      TabOrder = 0
      OnClose = Toolbar_FormatClose
      OnRecreated = Toolbar_StyleRecreated
      OnRecreating = Toolbar_StyleRecreating
      object TB_Style: TToolbarButton97
        Left = 167
        Top = 0
        Width = 38
        Height = 22
        Hint = 'Apply style to selection (click arrow for menu commands)'
        HelpContext = 301
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Style
        ImageIndex = 12
        Images = IMG_Format
        OnClick = BtnStyleApplyClick
      end
      object ToolbarSep9717: TToolbarSep97
        Left = 161
        Top = 0
      end
      object Combo_Style: TComboBox
        Left = 0
        Top = 0
        Width = 161
        Height = 22
        HelpContext = 301
        Style = csOwnerDrawFixed
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        PopupMenu = Menu_Style
        TabOrder = 0
        TabStop = False
        OnChange = Combo_StyleChange
        OnDrawItem = Combo_StyleDrawItem
        OnKeyPress = Combo_StyleKeyPress
      end
    end
    object Toolbar_Insert: TToolbar97
      Tag = 5
      Left = 484
      Top = 0
      ActivateParent = False
      Caption = 'Insert Toolbar'
      DefaultDock = Dock_Bottom
      DockPos = 484
      DragHandleStyle = dhSingle
      TabOrder = 1
      OnClose = Toolbar_FormatClose
      object ToolbarButton971: TToolbarButton97
        Left = 74
        Top = 0
        Width = 37
        Height = 22
        Hint = 'Insert symbol (click arrow for menu)'
        HelpContext = 309
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Symbols
        ImageIndex = 48
        Images = IMG_Toolbar
        OnClick = MMInsertCharacterClick
      end
      object ToolbarButton972: TToolbarButton97
        Left = 0
        Top = 0
        Width = 37
        Height = 22
        Hint = 'Insert date (click arrow for menu)'
        HelpType = htKeyword
        HelpKeyword = '282-12'
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Date
        ImageIndex = 46
        Images = IMG_Toolbar
        OnClick = MMInsertDateClick
      end
      object ToolbarButton973: TToolbarButton97
        Left = 37
        Top = 0
        Width = 37
        Height = 22
        Hint = 'Insert time (click arrow for menu)'
        HelpType = htKeyword
        HelpKeyword = '282-12'
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Time
        ImageIndex = 47
        Images = IMG_Toolbar
        OnClick = MMInsertTimeClick
      end
    end
  end
  object Pages_Res: TPage95Control
    Left = 795
    Top = 54
    Width = 210
    Height = 695
    Hint = 'Right-click for resource panel options'
    ActivePage = ResTab_Tags
    Align = alRight
    FlatSeperators = False
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    MultiLine = True
    PopupMenu = Menu_ResPanel
    RemoveLastTab = True
    TabOrder = 2
    OnChange = Pages_ResChange
    object ResTab_Find: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '479-5'
      Caption = 'Find'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Panel_ResFind: TPanel
        Left = 0
        Top = 0
        Width = 202
        Height = 90
        Align = alTop
        BevelOuter = bvLowered
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        DesignSize = (
          202
          90)
        object Label1: TLabel
          Left = 7
          Top = 7
          Width = 61
          Height = 13
          Caption = '&Find All text:'
          FocusControl = Combo_ResFind
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Btn_ResFind_Prev: TToolbarButton97
          Left = 162
          Top = 5
          Width = 18
          Height = 19
          Hint = 'Previous match'
          Anchors = [akTop, akRight]
          ImageIndex = 55
          Images = IMG_Toolbar
          Visible = False
          OnClick = Btn_ResFind_PrevClick
          ExplicitLeft = 262
        end
        object Btn_ResFind_Next: TToolbarButton97
          Left = 181
          Top = 5
          Width = 18
          Height = 19
          Hint = 'Next match'
          Anchors = [akTop, akRight]
          ImageIndex = 56
          Images = IMG_Toolbar
          Visible = False
          OnClick = Btn_ResFind_NextClick
          ExplicitLeft = 281
        end
        object LblFindAllNumResults: TLabel
          Left = 148
          Top = 7
          Width = 3
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Transparent = False
          ExplicitLeft = 248
        end
        object CB_ResFind_Filter: TCheckBox
          Left = 10
          Top = 64
          Width = 97
          Height = 17
          Hint = 'Show or hide nodes based on search conditions'
          Caption = 'Filter folder[s]'
          TabOrder = 2
        end
        object Combo_ResFind: TComboBox
          Left = 34
          Top = 30
          Width = 162
          Height = 22
          Hint = 'Type text to search for'
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          PopupMenu = Menu_StdEdit
          TabOrder = 0
          OnChange = Combo_ResFindChange
          OnKeyDown = Combo_ResFindKeyDown
        end
        object Btn_ResFind: TButton
          Left = 5
          Top = 29
          Width = 28
          Height = 25
          Hint = 'Search for text and display all matches'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ImageAlignment = iaCenter
          ImageIndex = 14
          Images = IMG_Toolbar
          ParentFont = False
          TabOrder = 1
          OnClick = Btn_ResFindClick
        end
        object Btn_ResFlip: TButton
          Left = 114
          Top = 58
          Width = 83
          Height = 25
          Hint = 'Toggle options and search results display'
          Anchors = [akTop, akRight]
          Caption = 'Results'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = Btn_ResFlipClick
        end
      end
      object Ntbk_ResFind: TNotebook
        Left = 0
        Top = 90
        Width = 202
        Height = 559
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        PageIndex = 1
        ParentFont = False
        TabOrder = 1
        ExplicitHeight = 558
        object PAGE_RES_FIND: TPage
          Left = 0
          Top = 0
          Caption = 'PAGE_RES_FIND'
          object FindAllResults: TRxRichEdit
            Left = 0
            Top = 0
            Width = 202
            Height = 559
            DrawEndPage = False
            Align = alClient
            AllowInPlace = False
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Calibri'
            Font.Style = []
            HideSelection = False
            ParentFont = False
            PopupMenu = Menu_FindAll
            ReadOnly = True
            TabOrder = 0
            UndoLimit = 0
            WantTabs = True
            WordSelection = False
            OnContextPopup = FindAllResultsContextPopup
          end
        end
        object PAGE_RES_FIND_OPT: TPage
          Left = 0
          Top = 0
          Caption = 'PAGE_RES_FIND_OPT'
          ExplicitHeight = 558
          object Pnl: TPanel
            Left = 0
            Top = 0
            Width = 202
            Height = 559
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
            ExplicitHeight = 558
            DesignSize = (
              202
              559)
            object Label2: TLabel
              Left = 9
              Top = 367
              Width = 80
              Height = 13
              Caption = 'Last modification'
              FocusControl = Combo_ResFind
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object Label3: TLabel
              Left = 9
              Top = 440
              Width = 66
              Height = 13
              Caption = 'Creation date'
              FocusControl = Combo_ResFind
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object lblCalNotSup: TLabel
              Left = 50
              Top = 424
              Width = 114
              Height = 13
              Caption = 'Calendar not supported'
              FocusControl = Combo_ResFind
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              Visible = False
            end
            object Label4: TLabel
              Left = 10
              Top = 518
              Width = 183
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 'Folded mode:'
              FocusControl = CbFindFoldedMode
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
            end
            object CB_ResFind_CaseSens: TCheckBox
              Left = 10
              Top = 9
              Width = 192
              Height = 17
              Hint = 'Distinguish between lowercase and uppercase letters'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Match case'
              TabOrder = 0
            end
            object CB_ResFind_WholeWords: TCheckBox
              Left = 10
              Top = 29
              Width = 192
              Height = 17
              Hint = 'Find only complete words'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Whole words only'
              TabOrder = 1
            end
            object CB_ResFind_AllNotes: TCheckBox
              Left = 10
              Top = 56
              Width = 192
              Height = 17
              Hint = 'Search through all folders in current file'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Search all folders'
              Checked = True
              State = cbChecked
              TabOrder = 2
              OnClick = CB_ResFind_AllNotesClick
            end
            object CB_ResFind_CurrentNodeAndSubtree: TCheckBox
              Left = 10
              Top = 76
              Width = 192
              Height = 17
              Hint = 'Search through current node and subtree in active folder'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Current node and subtree'
              TabOrder = 3
            end
            object RG_ResFind_Type: TRadioGroup
              Left = 6
              Top = 122
              Width = 187
              Height = 66
              Hint = 'Select type of search to perform'
              Anchors = [akLeft, akTop, akRight]
              DefaultHeaderFont = False
              HeaderFont.Charset = DEFAULT_CHARSET
              HeaderFont.Color = clWindowText
              HeaderFont.Height = -5
              HeaderFont.Name = 'Tahoma'
              HeaderFont.Style = []
              TabOrder = 5
              OnClick = RG_ResFind_TypeClick
            end
            object RG_ResFind_Scope: TRadioGroup
              Left = 7
              Top = 197
              Width = 187
              Height = 66
              Hint = 'Select scope of search to perform'
              Anchors = [akLeft, akTop, akRight]
              DefaultHeaderFont = False
              HeaderFont.Charset = DEFAULT_CHARSET
              HeaderFont.Color = clWindowText
              HeaderFont.Height = -5
              HeaderFont.Name = 'Tahoma'
              HeaderFont.Style = []
              TabOrder = 6
              OnClick = RG_ResFind_ScopeClick
            end
            object RG_ResFind_ChkMode: TRadioGroup
              Left = 7
              Top = 293
              Width = 187
              Height = 66
              Hint = 'Select whether to consider nodes based on checked status'
              Anchors = [akLeft, akTop, akRight]
              DefaultHeaderFont = False
              HeaderFont.Charset = DEFAULT_CHARSET
              HeaderFont.Color = clWindowText
              HeaderFont.Height = -5
              HeaderFont.Name = 'Tahoma'
              HeaderFont.Style = []
              TabOrder = 8
            end
            object CB_ResFind_HiddenNodes: TCheckBox
              Left = 10
              Top = 96
              Width = 192
              Height = 17
              Hint = 'Consider hidden nodes'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Search hidden nodes'
              Checked = True
              State = cbChecked
              TabOrder = 4
            end
            object chk_LastModifFrom: TCheckBox
              Left = 10
              Top = 388
              Width = 17
              Height = 17
              TabOrder = 9
              OnClick = chk_LastModifFromClick
            end
            object CB_LastModifFrom: TDateTimePicker
              Left = 32
              Top = 387
              Width = 80
              Height = 21
              Hint = 'From'
              Checked = False
              Enabled = False
              TabOrder = 10
              Visible = False
            end
            object chk_LastModifUntil: TCheckBox
              Left = 10
              Top = 413
              Width = 17
              Height = 17
              TabOrder = 11
              OnClick = chk_LastModifUntilClick
            end
            object CB_LastModifUntil: TDateTimePicker
              Left = 32
              Top = 413
              Width = 80
              Height = 21
              Hint = 'Until'
              Enabled = False
              TabOrder = 12
              Visible = False
            end
            object chk_CreatedFrom: TCheckBox
              Left = 10
              Top = 459
              Width = 17
              Height = 17
              TabOrder = 13
              OnClick = chk_CreatedFromClick
            end
            object CB_CreatedFrom: TDateTimePicker
              Left = 32
              Top = 458
              Width = 80
              Height = 21
              Hint = 'From'
              Checked = False
              Enabled = False
              TabOrder = 14
              Visible = False
            end
            object chk_CreatedUntil: TCheckBox
              Left = 10
              Top = 484
              Width = 17
              Height = 17
              TabOrder = 15
              OnClick = chk_CreatedUntilClick
            end
            object CB_CreatedUntil: TDateTimePicker
              Left = 32
              Top = 484
              Width = 80
              Height = 21
              Hint = 'Until'
              Enabled = False
              TabOrder = 16
              Visible = False
            end
            object CB_ResFind_PathInNames: TCheckBox
              Left = 10
              Top = 267
              Width = 194
              Height = 17
              Hint = 'Search in whole path of nodes'
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Use path of node'
              TabOrder = 7
              OnClick = CB_ResFind_PathInNamesClick
            end
            object CbFindFoldedMode: TComboBox
              Left = 10
              Top = 536
              Width = 183
              Height = 21
              Hint = 
                'Include or exclude depending on the pattern found in a folded te' +
                'xt'
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 17
            end
          end
        end
      end
    end
    object ResTab_RTF: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '11-1'
      Caption = 'Scratch'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
    end
    object ResTab_Macro: TTab95Sheet
      HelpContext = 304
      Caption = 'Macros'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Dock_ResMacro: TDock97
        Left = 0
        Top = 0
        Width = 202
        Height = 26
        object Toolbar_Macro: TToolbar97
          Left = 0
          Top = 0
          ActivateParent = False
          Caption = 'Macro toolbar'
          DefaultDock = Dock_ResMacro
          DockMode = dmCannotFloat
          DockPos = 0
          DragHandleStyle = dhSingle
          FullSize = True
          TabOrder = 0
          object ToolbarSep9715: TToolbarSep97
            Left = 91
            Top = 0
          end
          object TB_Macro: TToolbarButton97
            Left = 0
            Top = 0
            Width = 40
            Height = 22
            Hint = 'Run selected macro (click arrow for menu commands)'
            DropdownArrowWidth = 15
            DropdownCombo = True
            DropdownMenu = Menu_Macro
            ImageIndex = 29
            Images = IMG_Toolbar
            OnClick = TB_MacroClick
          end
          object TB_MacroPause: TToolbarButton97
            Left = 46
            Top = 0
            Width = 22
            Height = 22
            Hint = 'Pause recording macro'
            AllowAllUp = True
            GroupIndex = 1
            ImageIndex = 30
            Images = IMG_Toolbar
            OnClick = TB_MacroPauseClick
          end
          object TB_MacroRecord: TToolbarButton97
            Left = 68
            Top = 0
            Width = 23
            Height = 22
            Hint = 'Record a new macro'
            ImageIndex = 31
            Images = IMG_Toolbar
            OnClick = TB_MacroRecordClick
          end
          object Sep9719: TToolbarSep97
            Left = 40
            Top = 0
          end
        end
      end
      object ListBox_ResMacro: TGFXListBox
        Left = 0
        Top = 26
        Width = 202
        Height = 623
        Hint = 'Double-click to run macro; right-click for menu'
        ImageList = IMG_Toolbar
        CheckBoxes = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ParentFont = False
        PopupMenu = Menu_Macro
        Sorted = True
        TabOrder = 1
        OnClick = Combo_MacroClick
        OnDblClick = ListBox_ResMacroDblClick
      end
    end
    object ResTab_Template: TTab95Sheet
      HelpContext = 307
      Caption = 'Templates'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object ListBox_ResTpl: TGFXListBox
        Left = 0
        Top = 0
        Width = 202
        Height = 649
        Hint = 'Double-click to insert template; right-click for menu'
        ImageList = IMG_Toolbar
        CheckBoxes = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ParentFont = False
        PopupMenu = Menu_Template
        Sorted = True
        TabOrder = 0
        OnClick = ListBox_ResTplClick
        OnDblClick = ListBox_ResTplDblClick
      end
    end
    object ResTab_Plugins: TTab95Sheet
      HelpContext = 305
      Caption = 'Plugins'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Splitter_plugins: TSplitter
        Left = 0
        Top = 593
        Width = 202
        Height = 3
        Cursor = crVSplit
        Hint = 'Click and drag to resize panels'
        Align = alBottom
        ExplicitTop = -3
        ExplicitWidth = 0
      end
      object Dock_ResPlugins: TDock97
        Left = 0
        Top = 0
        Width = 202
        Height = 26
        LimitToOneRow = True
        object Toolbar_Plugins: TToolbar97
          Left = 0
          Top = 0
          ActivateParent = False
          Caption = 'Macro toolbar'
          DefaultDock = Dock_ResPlugins
          DockMode = dmCannotFloat
          DockPos = 0
          DragHandleStyle = dhSingle
          FullSize = True
          TabOrder = 0
          object ToolbarSep9720: TToolbarSep97
            Left = 68
            Top = 0
          end
          object TB_PluginRun: TToolbarButton97
            Left = 0
            Top = 0
            Width = 40
            Height = 22
            Hint = 'Execute selected plugin (click arrow for menu commands)'
            DropdownArrowWidth = 15
            DropdownCombo = True
            DropdownMenu = Menu_Plugins
            ImageIndex = 29
            Images = IMG_Toolbar
            OnClick = PLM_RunPluginClick
          end
          object TB_PluginConfigure: TToolbarButton97
            Left = 46
            Top = 0
            Width = 22
            Height = 22
            Hint = 'Configure selected plugin'
            AllowAllUp = True
            ImageIndex = 37
            Images = IMG_Toolbar
            OnClick = PLM_ConfigurePluginClick
          end
          object ToolbarSep9721: TToolbarSep97
            Left = 40
            Top = 0
          end
        end
      end
      object ListBox_ResPlugins: TGFXListBox
        Left = 0
        Top = 26
        Width = 202
        Height = 567
        Hint = 'Double-click to run plugin; right-click for menu'
        ImageList = IMG_Toolbar
        CheckBoxes = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ParentFont = False
        PopupMenu = Menu_Plugins
        Sorted = True
        TabOrder = 1
        OnClick = ListBox_ResPluginsClick
        OnDblClick = PLM_RunPluginClick
      end
      object Panel_ResPlugins: TPanel
        Left = 0
        Top = 596
        Width = 202
        Height = 53
        Align = alBottom
        BevelOuter = bvLowered
        BorderWidth = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object LB_PluginInfo: TLabel
          Left = 3
          Top = 3
          Width = 196
          Height = 47
          Align = alClient
          Caption = '...'
          WordWrap = True
          ExplicitWidth = 12
          ExplicitHeight = 14
        end
      end
    end
    object ResTab_Favorites: TTab95Sheet
      HelpContext = 601
      Caption = 'Favorites'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object ListBox_ResFav: TGFXListBox
        Left = 0
        Top = 0
        Width = 202
        Height = 649
        Hint = 'Double-click to jump; right-click for menu'
        ImageList = Img_System
        CheckBoxes = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ParentFont = False
        PopupMenu = Menu_Fav
        Sorted = True
        TabOrder = 0
        OnClick = ListBox_ResFavClick
        OnDblClick = FavMJumpClick
      end
    end
    object ResTab_Tags: TTab95Sheet
      Caption = 'Tags'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitHeight = 648
      DesignSize = (
        202
        649)
      object TVTags: TVirtualStringTree
        Left = 0
        Top = 2
        Width = 200
        Height = 622
        Anchors = [akLeft, akTop, akRight, akBottom]
        Header.AutoSizeIndex = 0
        Header.MainColumn = -1
        TabOrder = 0
        Columns = <>
      end
      object txtFilterTags: TEdit
        Left = 0
        Top = 627
        Width = 200
        Height = 22
        TabStop = False
        Anchors = [akLeft, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
  end
  object actList_TV: TActionList
    Images = IMG_TV
    Left = 448
    Top = 464
    object actTVAlarmNode: TAction
      Caption = 'Set alarm on node...'
      HelpContext = 567
      Hint = 'Set or remove alarm on node'
      OnExecute = actTVAlarmNodeExecute
    end
    object actTVAddNode_Parent: TAction
      Caption = 'Add Parent'
      Hint = 'Creates a node as parent of selected one and its next siblings'
      OnExecute = actTVAddNode_ParentExecute
    end
    object actTVAddNode_Above: TAction
      Caption = 'Add &Above'
      Hint = 'Insert a Sibling node Above the selected one'
      OnExecute = actTVAddNode_AboveExecute
    end
    object actTVAddNode_Child: TAction
      Caption = 'Add &Child'
      Hint = 'Add a Child node of selected one'
      OnExecute = actTVAddNode_ChildExecute
    end
    object actTVAddNode_Below: TAction
      Caption = 'Add B&elow'
      Hint = 'Add a Sibling node immediately Below selected one'
      OnExecute = actTVAddNode_BelowExecute
    end
    object actTVAddNode_Last: TAction
      Caption = 'Add &Last sibling'
      Hint = 'Add a Sibling node in the bottom (Last)'
      OnExecute = actTVAddNode_LastExecute
    end
    object actTVHideCheckedChildren: TAction
      Caption = 'Hide &Checked'
      Hint = 'Hide checked children of selected node'
      OnExecute = actTVHideCheckedChildrenExecute
    end
    object actTVHideUncheckedChildren: TAction
      Caption = 'Hide &Unchecked'
      Hint = 'Hide unchecked children of selected node'
      OnExecute = actTVHideUncheckedChildrenExecute
    end
    object actTVShowNonFilteredChildren: TAction
      Caption = '&Show non filtered'
      Hint = 'Show hidden children of selected node, non filtered by search'
      OnExecute = actTVShowNonFilteredExecute
    end
    object actTVNodeTextColor: TAction
      Caption = 'Tree &node Text...'
      Hint = 'Select custom color for tree node text'
      OnExecute = actTVNodeTextColorExecute
    end
    object actTVNodeBGColor: TAction
      Caption = 'Tree node &Background...'
      Hint = 'Select custom color for tree node background'
      OnExecute = actTVNodeBGColorExecute
    end
    object actTVDefaultNodeFont: TAction
      Caption = '&Reset to Default'
      Hint = 'Reset node color and font face to default'
      OnExecute = actTVDefaultNodeFontExecute
    end
    object actTVInsertLinkedNode: TAction
      Caption = 'Insert &Linked node'
      HelpContext = 383
      Hint = 
        'Insert new node as linked node (share same note -content and nam' +
        'e)'
      ImageIndex = 8
      OnExecute = actTVInsertLinkedNNodeExecute
    end
    object actTVMoveNodeUp: TAction
      Caption = '&Up'
      Hint = 'Move node UP'
      OnExecute = actTVMoveNodeUpExecute
    end
    object actTVMoveNodeDown: TAction
      Caption = '&Down'
      Hint = 'Move node DOWN'
      OnExecute = actTVMoveNodeDownExecute
    end
    object actTVMoveNodeLeft: TAction
      Caption = '&Left'
      Hint = 'Shift node LEFT by 1 level'
      OnExecute = actTVMoveNodeLeftExecute
    end
    object actTVMoveNodeRight: TAction
      Caption = '&Right'
      Hint = 'Shift node RIGHT by 1 level'
      OnExecute = actTVMoveNodeRightExecute
    end
    object actTVCutSubtree: TAction
      Caption = 'C&ut Subtree'
      Hint = 
        'Cut selected node and its children (to move with Paste) (Ctrl+X,' +
        ' Shift+Supr)'
      OnExecute = actTVCutSubtreeExecute
    end
    object actTVCopySubtree: TAction
      Caption = '&Copy Subtree'
      Hint = 'Copy selected node and its children (Ctrl+C, Ctrl+Ins)'
      OnExecute = actTVCopySubtreeExecute
    end
    object actTVGraftSubtree: TAction
      Caption = '&Paste Subtree'
      Hint = 
        'Paste previously copied nodes at selected position (Ctrl+V, Shif' +
        't+Ins)'
      OnExecute = actTVPasteSubtreeExecute
    end
    object actTVGraftSubtreeLinked: TAction
      Caption = 'Paste Subtree as &Linked nodes'
      Hint = 
        'Paste previously copied nodes at selected position as linked nod' +
        'es'
      OnExecute = actTVPasteSubtreeLinkedExecute
    end
    object actTVEraseTreeMem: TAction
      Caption = '&Erase Memory'
      Hint = 'Forget previously copied nodes'
      OnExecute = actTVEraseTreeMemExecute
    end
    object actTVExport: TAction
      Caption = 'E&xport...'
      HelpContext = 313
      Hint = 'Export node contents (or selection) to file'
      OnExecute = actTVExportExecute
    end
    object actTVDeleteNode: TAction
      Caption = '&Delete Node[s]'
      Hint = 'Delete selected nodes'
      OnExecute = actTVDeleteNodeExecute
    end
    object actTVDeleteChildren: TAction
      Caption = 'Delete C&hildren'
      Hint = 'Delete child nodes of selected nodes'
      OnExecute = actTVDeleteChildrenExecute
    end
    object actTVRenameNode: TAction
      Caption = '&Rename Node'
      Hint = 'Rename node'
      OnExecute = actTVRenameNodeExecute
    end
    object actTVCopyNodeName: TAction
      Caption = '&Node Name'
      Hint = 'Copy node name to clipboard'
      OnExecute = actTVCopyNodeNameExecute
    end
    object actTVCopyNodePath: TAction
      Caption = 'Node &Path'
      Hint = 'Copy full node path to clipboard'
      OnExecute = actTVCopyNodePathExecute
    end
    object actTVCopyNodeText: TAction
      Caption = 'Node &Text'
      Hint = 'Copy full contents of node to clipboard'
      OnExecute = actTVCopyNodeTextExecute
    end
    object actTVCopyPathtoEditor: TAction
      Caption = 'Path to &Editor'
      Hint = 'Insert full node path in editor'
      OnExecute = actTVCopyPathtoEditorExecute
    end
    object actTVPasteNodeName: TAction
      Caption = 'From &Clipboard'
      Hint = 'Use clipboard contents as selected node name'
      OnExecute = actTVPasteNodeNameExecute
    end
    object actTVPasteNodeNameAsDate: TAction
      Tag = 1
      Caption = 'As &Date'
      Hint = 'Use current date as selected node name'
      OnExecute = actTVPasteNodeNameExecute
    end
    object actTVPasteNodeNameAsTime: TAction
      Tag = 2
      Caption = 'As &Time'
      Hint = 'Use current time as selected node name'
      OnExecute = actTVPasteNodeNameExecute
    end
    object actTVPasteNodeNameAsDateTime: TAction
      Tag = 3
      Caption = '&As Date and Time'
      Hint = 'Use current date and time as selected node name'
      OnExecute = actTVPasteNodeNameExecute
    end
    object actTVPasteNodeNameAsSel: TAction
      Tag = 4
      Caption = 'From &Selected Text'
      Hint = 'Rename node using selected text'
      OnExecute = actTVPasteNodeNameExecute
    end
    object actTVSortSubtree: TAction
      Caption = '&Sort Subtree'
      Hint = 'Sort children of current node'
      OnExecute = actTVSortSubtreeExecute
    end
    object actTVSortTree: TAction
      Caption = 'Sort Entire &Tree'
      Hint = 'Sort all nodes in tree'
      OnExecute = actTVSortTreeExecute
    end
  end
  object actList_TVs: TActionList
    Images = IMG_TV
    OnUpdate = actList_TVsUpdate
    Left = 512
    Top = 464
    object actTVCheckNode: TAction
      Caption = 'Ch&ecked'
      Hint = 'Check or uncheck selected node'
      OnExecute = actTVCheckNodeExecute
    end
    object actTVRefreshVirtualNode: TAction
      Caption = 'Re&fresh'
      Hint = 'Refresh contents from original file on disk'
      OnExecute = actTVRefreshVirtualNodeExecute
    end
    object actTVNavigateNextLinkedNNode: TAction
      Caption = 'Navigate to &Next Linked'
      HelpContext = 383
      Hint = 'Navigate to the next node linked to the same note'
      OnExecute = actTVNavigateNextLinkedNNodeExecute
    end
    object actTVUnlinkVirtualNode: TAction
      Caption = '&Unlink Node'
      Hint = 'Unlink the virtual node from file on disk'
      OnExecute = actTVUnlinkVirtualNodeExecute
    end
    object actTVChildrenCheckbox: TAction
      Caption = 'Show Check&boxes'
      Hint = 'Show or hide Checkboxes in children of selected node'
      OnExecute = actTVChildrenCheckboxExecute
    end
    object actTVSelectNodeImage: TAction
      Caption = 'C&ustom icon...'
      Hint = 'Choose custom icon for selected nodes'
      OnExecute = actTVSelectNodeImageExecute
    end
    object actTVViewAdditColumns: TAction
      Caption = 'Show columns'
      Hint = 'Show additional columns (Flagged and/or Date)'
      OnExecute = actTVViewAdditColumnsExecute
    end
    object actTVFilterOutUnflagged: TAction
      Caption = 'Filter out unflagged'
      Hint = 'Hide unflagged nodes'
      ImageIndex = 10
      OnExecute = actTVFilterOutUnflaggedExecute
    end
    object actTVFlaggedNode: TAction
      Caption = '&Flagged'
      Hint = 
        'Toggle Flag in selected nodes (Ctrl:Also toggle on/off '#39'Filter u' +
        'nflagged'#39' -- Shift: Clear ALL flags)'
      OnExecute = actTVFlaggedNodeExecute
    end
    object actTVBoldNode: TAction
      Caption = '&Bold'
      Hint = 
        'Toggle bold in selected nodes (Shift: also in children, recursiv' +
        'e)'
      OnExecute = actTVBoldNodeExecute
    end
    object actTVVirtualNode: TAction
      Caption = 'Make &Virtual'
      Hint = 'Link a file on disk to selected node'
      ImageIndex = 6
      OnExecute = actTVVirtualNodeExecute
    end
  end
  object Menu_Main: TMainMenu
    Images = IMG_Toolbar
    Left = 286
    Top = 65
    object MMFile_: TMenuItem
      Caption = '&File'
      SubMenuImages = IMG_Toolbar
      HelpContext = 29
      Hint = 'File management commands'
      object MMFileNew: TMenuItem
        Caption = '&New'
        Hint = 'Create a new Keynote file'
        ImageIndex = 0
        ShortCut = 24654
        OnClick = MMFileNewClick
      end
      object MMFileOpen: TMenuItem
        Caption = '&Open...'
        Hint = 'Open a Keynote file'
        ImageIndex = 1
        ShortCut = 16463
        OnClick = MMFileOpenClick
      end
      object MMFileSave: TMenuItem
        Action = actFileSave
        ShortCut = 16467
      end
      object MMFileSaveAs: TMenuItem
        Caption = 'Save &As...'
        Hint = 'Save Keynote file with a new name'
        OnClick = MMFileSaveAsClick
      end
      object MMFileClose: TMenuItem
        Caption = 'Clos&e'
        Hint = 'Close currently open file'
        ShortCut = 16465
        OnClick = MMFileCloseClick
      end
      object MM_MRUSeparator_: TMenuItem
        Caption = '-'
      end
      object MMFileAutoSave: TMenuItem
        Caption = 'A&uto Save'
        Hint = 'Toggle automatic saving'
        OnClick = MMFileAutoSaveClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MMFileProperties: TMenuItem
        Caption = '&Properties'
        Hint = 'Edit Keynote file properties'
        ImageIndex = 16
        ShortCut = 32781
        OnClick = MMFilePropertiesClick
      end
      object MMFileManager: TMenuItem
        Caption = 'File &Manager'
        Hint = 'Open file manager'
        ImageIndex = 12
        ShortCut = 123
        OnClick = MMFileManagerClick
      end
      object N28: TMenuItem
        Caption = '-'
      end
      object MMToolsImport: TMenuItem
        Caption = '&Import...'
        Hint = 'Import files as folders'
        OnClick = MMToolsImportClick
      end
      object MMToolsExport: TMenuItem
        Caption = '&Export...'
        Hint = 'Export text to another format'
        OnClick = MMToolsExportExClick
      end
      object N105: TMenuItem
        Caption = '-'
      end
      object MMFilePageSetup: TMenuItem
        Caption = 'Pa&ge Setup...'
        Hint = 'View or change page setup'
        OnClick = MMFilePageSetupClick
      end
      object MMFilePrint: TMenuItem
        Caption = 'Print...'
        Hint = 'Print with advanced options'
        ImageIndex = 44
        OnClick = MMFilePrintClick
      end
      object MMFileCopyTo: TMenuItem
        Caption = '&Copy To...'
        Hint = 'Copy current file to another directory'
        OnClick = MMFileCopyToClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MMFileExit: TMenuItem
        Caption = 'E&xit'
        Hint = 'Close KeyNote'
        OnClick = MMFileExitClick
      end
    end
    object MMEdit_: TMenuItem
      Caption = '&Edit'
      SubMenuImages = IMG_Toolbar
      HelpContext = 46
      Hint = 'Text editing commands'
      object MMEditUndo: TMenuItem
        Caption = '&Undo'
        Hint = 'Undo last change'
        ImageIndex = 6
        ShortCut = 32776
        OnClick = MMEditUndoClick
      end
      object MMEditRedo: TMenuItem
        Caption = '&Redo'
        Hint = 'Redo the last Undone command'
        ImageIndex = 13
        ShortCut = 16397
        OnClick = MMEditRedoClick
      end
      object MMEditRepeat: TMenuItem
        Caption = 'R&epeat Last'
        Hint = 'Repeat last editing command'
        ImageIndex = 40
        ShortCut = 16575
        OnClick = MMEditRepeatClick
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object MMEditCut: TMenuItem
        Caption = 'Cu&t'
        Hint = 'Cut text to clipboard (Ctrl+X, Shift+Supr)'
        ImageIndex = 3
        OnClick = MMEditCutClick
      end
      object MMEditCopy: TMenuItem
        Caption = '&Copy'
        Hint = 'Copy text to clipboard (Ctrl+C, Ctrl+Ins)'
        ImageIndex = 4
        OnClick = MMEditCopyClick
      end
      object MMEditPaste: TMenuItem
        Caption = '&Paste'
        Hint = 'Paste text from clipboard (Ctrl+V, Shift+Ins)'
        ImageIndex = 5
        OnClick = MMEditPasteClick
      end
      object MMEditPasteOther_: TMenuItem
        Caption = 'Paste &Other'
        Hint = 'Paste in other formats'
        object MMEditPasteAsText: TMenuItem
          Caption = 'Paste as &Text'
          Hint = 'Paste text from clipboard without formatting'
          ShortCut = 24621
          OnClick = MMEditPasteAsTextClick
        end
        object MMEditPasteAsWebClip: TMenuItem
          Caption = 'Paste as &Web Clip'
          ShortCut = 16471
          OnClick = MMEditPasteAsWebClipClick
        end
        object MMEditPasteAsWebClipText: TMenuItem
          Caption = 'Paste as Web &Clip (Text)'
          Hint = 'Paste as Web Clip without formatting'
          ShortCut = 24663
          OnClick = MMEditPasteAsWebClipTextClick
        end
        object MMEditPasteSpecial: TMenuItem
          Caption = 'Paste &Special...'
          Hint = 'Select special format to paste'
          OnClick = MMEditPasteSpecialClick
        end
        object N116: TMenuItem
          Caption = '-'
        end
        object MMEditPlainDefaultPaste: TMenuItem
          Caption = 'Paste &External as Plain text'
          Hint = 
            'Default paste as plain text when copied from outside KN, accordi' +
            'ng to '#39'Plain text mode'#39
          OnClick = MMEditPlainDefaultPasteClick
        end
        object N67: TMenuItem
          Caption = '-'
        end
        object MMEditPasteAsNewFolder: TMenuItem
          Caption = 'Paste &Into New Folder'
          Hint = 'Create a new folder and paste text from clipboard'
          OnClick = MMEditPasteAsNewFolderClick
        end
        object MMEditPasteAsNewNode: TMenuItem
          Caption = 'Paste Into New &Node'
          Hint = 'Create a new tree node and paste text from clipboard'
          OnClick = MMEditPasteAsNewNodeClick
        end
      end
      object MMEditDelete: TMenuItem
        Caption = '&Delete'
        Hint = 'Remove selected text'
        OnClick = MMEditDeleteClick
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object MMEditLines_: TMenuItem
        Caption = '&Lines'
        Hint = 'Line operations'
        object MMEditDelLine: TMenuItem
          Caption = '&Delete Line'
          Hint = 'Deletes line at cursor'
          ShortCut = 16473
          OnClick = MMEditDelLineClick
        end
        object N58: TMenuItem
          Caption = '-'
        end
        object MMEditSort: TMenuItem
          Caption = '&Sort Lines'
          Hint = 'Sort selected lines in active note'
          ShortCut = 24659
          OnClick = MMSortClick
        end
        object MMEditJoin: TMenuItem
          Caption = '&Join Lines'
          Hint = 'Join selected lines (reformat pagagraph)'
          ShortCut = 16458
          OnClick = MMEditJoinClick
        end
      end
      object MMEditCase_: TMenuItem
        Caption = 'C&hange Case'
        Hint = 'Adjust lettercase'
        object MMEditUpper: TMenuItem
          Caption = 'To &UPPERCASE'
          Hint = 'Sets selected text to uppercase'
          ShortCut = 24661
          OnClick = MMEditUpperClick
        end
        object MMEditLower: TMenuItem
          Caption = 'To &lowercase'
          Hint = 'Sets selected text to lowercase'
          ShortCut = 24652
          OnClick = MMEditLowerClick
        end
        object MMEditMixed: TMenuItem
          Caption = 'To &Mixed Case'
          Hint = 'Sets selected text to mixed (Title) case'
          ShortCut = 24653
          OnClick = MMEditMixedClick
        end
        object N59: TMenuItem
          Caption = '-'
        end
        object MMEditInvertCase: TMenuItem
          Caption = '&Invert Case'
          Hint = 'Invert case of selected text'
          ShortCut = 24649
          OnClick = MMEditInvertCaseClick
        end
        object MMEditCycleCase: TMenuItem
          Caption = '&Cycle Case'
          Hint = 'Cyclically changes case (lower -> Mixed -> UPPER)'
          ShortCut = 8306
          OnClick = MMEditCycleCaseClick
        end
      end
      object MMEditTrim_: TMenuItem
        Caption = '&White Space'
        Hint = 'Trim white space'
        object MMEditTrimLeft: TMenuItem
          Tag = 1
          Caption = 'Trim &Left'
          Hint = 'Left-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object MMEditTrimRight: TMenuItem
          Tag = 2
          Caption = 'Trim &Right'
          Hint = 'Right-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object MMEditTrimBoth: TMenuItem
          Tag = 3
          Caption = 'Trim &Both'
          Hint = 'Left- and right-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object N60: TMenuItem
          Caption = '-'
        end
        object MMEditCompress: TMenuItem
          Caption = '&Compress White Space'
          Hint = 'Compress white space'
          OnClick = MMEditCompressClick
        end
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object MMEditTransform_: TMenuItem
        Caption = 'Transfor&m'
        Hint = 'Transform text'
        object MMEditRot13: TMenuItem
          Caption = 'Apply &Rot-13'
          Hint = 'Apply ROT-13 coding to selected text'
          ShortCut = 24627
          OnClick = MMEditRot13Click
        end
        object MMEditReverse: TMenuItem
          Caption = 'Re&verse Text'
          Hint = 'Order selected text backwards'
          ShortCut = 24628
          OnClick = MMEditReverseClick
        end
        object N112: TMenuItem
          Caption = '-'
        end
        object MMEditDecimalToRoman: TMenuItem
          Caption = '&Decimal to Roman'
          Hint = 'Convert decimal numeral to Roman'
          OnClick = MMEditDecimalToRomanClick
        end
        object MMEditRomanToDecimal: TMenuItem
          Caption = 'R&oman to Decimal'
          Hint = 'Convert Roman numeral to decimal'
          OnClick = MMEditRomanToDecimalClick
        end
      end
      object MMEditEval_: TMenuItem
        Caption = 'E&xpression'
        Hint = 'Expression evaluation'
        object MMEditEvaluate: TMenuItem
          Caption = '&Evaluate'
          Hint = 'Evaluate selected text as mathematical expression'
          ShortCut = 16571
          OnClick = MMEditEvaluateClick
        end
        object MMEditPasteEval: TMenuItem
          Caption = '&Paste Last Result'
          Hint = 'Paste result of last expression evaluated'
          ShortCut = 32813
          OnClick = MMEditPasteEvalClick
        end
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object MMEditSelectWord: TMenuItem
        Caption = 'Select Word'
        Hint = 'Select current word'
        ShortCut = 49239
        OnClick = MMEditSelectWordClick
      end
      object MMEditCopyAll: TMenuItem
        Caption = 'Cop&y All'
        Hint = 'Select and copy all text in active note'
        OnClick = MMEditCopyAllClick
      end
      object MMEditSelectAll: TMenuItem
        Caption = 'Select &All'
        Hint = 'Select all text in note'
        ShortCut = 16449
        OnClick = MMEditSelectAllClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
    object MMView_: TMenuItem
      Caption = '&View'
      SubMenuImages = IMG_Toolbar
      HelpContext = 85
      Hint = 'Show or hide interface elements'
      object MMViewOnTop: TMenuItem
        Caption = '&Always on Top'
        Hint = 'Keep program window on top of other windows'
        ImageIndex = 45
        ShortCut = 119
        OnClick = MMViewOnTopClick
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object MMViewResPanel: TMenuItem
        Caption = '&Resource Panel'
        Hint = 'Show or hide resource panel'
        ImageIndex = 41
        ShortCut = 120
        OnClick = MMViewResPanelClick
      end
      object MMViewToolbars_: TMenuItem
        Caption = '&Toolbars'
        Hint = 'Show or hide toolbars'
        object MMViewTBMain: TMenuItem
          Caption = 'Show &Main Toolbar'
          Hint = 'Display or hide main toolbar'
          OnClick = MMViewTBMainClick
        end
        object MMViewTBFormat: TMenuItem
          Caption = 'Show &Format Toolbar'
          Hint = 'Display or hide the Formatting toolbar'
          OnClick = MMViewTBFormatClick
        end
        object MMViewTBStyle: TMenuItem
          Caption = 'Show &Style Toolbar'
          Hint = 'Display or hide the Style toolbar'
          OnClick = MMViewTBStyleClick
        end
        object MMViewTBTree: TMenuItem
          Caption = 'Show &Tree Toolbar'
          Hint = 'Display or hide the Tree toolbar'
          OnClick = MMViewTBTreeClick
        end
        object MMViewTBInsert: TMenuItem
          Caption = 'Show &Insert Toolbar'
          Hint = 'Display or hide the Insert toolbar'
          OnClick = MMViewTBInsertClick
        end
        object N76: TMenuItem
          Caption = '-'
        end
        object MMViewTBAll: TMenuItem
          Tag = 1
          Caption = 'S&how All Toolbars'
          Hint = 'Display all toolbars'
          OnClick = MMViewTBHideAllClick
        end
        object MMViewTBHideAll: TMenuItem
          Caption = '&Hide All Toolbars'
          Hint = 'Hide all toolbars'
          OnClick = MMViewTBHideAllClick
        end
        object N101: TMenuItem
          Caption = '-'
        end
        object MMViewTBRefresh: TMenuItem
          Caption = '&Refresh Configuration'
          Hint = 'Refresh toolbar button configuration from "toolbar.ini" file'
          OnClick = MMViewTBRefreshClick
        end
        object MMViewTBSaveConfig: TMenuItem
          Caption = 'S&ave Configuration'
          Hint = 'Save toolbar layout to "toolbar.ini" file'
          OnClick = MMViewTBSaveConfigClick
        end
      end
      object MMViewStatusBar: TMenuItem
        Caption = 'Stat&us Bar'
        Hint = 'Show or hide the status bar'
        OnClick = MMViewStatusBarClick
      end
      object MMShowImages: TMenuItem
        Caption = 'Show Images'
        Checked = True
        Enabled = False
        ImageIndex = 59
        OnClick = MMShowImagesClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMViewTabIcons: TMenuItem
        Caption = 'Tab &Icons'
        Hint = 'Show or hide icons on folder tabs'
        OnClick = MMViewTabIconsClick
      end
      object MMViewTree: TMenuItem
        Caption = 'Tree &Panel'
        Hint = 
          'Show or hide tree panel (Ctrl -> Temporarily preserve editor wid' +
          'th)'
        ImageIndex = 20
        ShortCut = 8314
        OnClick = MMViewTreeClick
      end
      object MMViewEditorInfoPanel: TMenuItem
        Caption = 'Editor Info'
        Hint = 'Show or hide the editor information panel (Ctrl: All Folders)'
        OnClick = MMViewEditorInfoPanelClick
      end
      object MMViewHistory: TMenuItem
        Caption = 'History'
        SubMenuImages = IMG_Toolbar
        object MMHistoryGoBack: TMenuItem
          Caption = '&Go Back'
          Hint = 'Navigate backwards in history'
          ImageIndex = 38
          ShortCut = 32805
          OnClick = MMHistoryGoBackClick
        end
        object MMHistoryGoForward: TMenuItem
          Caption = 'Go &Forward'
          Hint = 'Navigate forward in history'
          ImageIndex = 39
          ShortCut = 32807
          OnClick = MMHistoryGoForwardClick
        end
      end
      object N102: TMenuItem
        Caption = '-'
      end
      object MMViewNodeIcons: TMenuItem
        Tag = 1
        Caption = 'Sta&ndard Tree Icons'
        Hint = 'Show or hide standard tree icons'
        OnClick = MMViewNodeIconsClick
      end
      object MMViewCustomIcons: TMenuItem
        Tag = 2
        Caption = 'C&ustom Tree Icons'
        Hint = 'Show or hide custom tree icons'
        OnClick = MMViewNodeIconsClick
      end
      object MMViewCheckboxesAllNodes: TMenuItem
        Caption = 'All nodes &Checkboxes'
        Hint = 'Show or hide tree checkboxes in all nodes'
        OnClick = MMViewCheckboxesAllNodesClick
      end
      object MMViewHideCheckedNodes: TMenuItem
        Caption = '&Hide Checked Nodes'
        Hint = 'Keep checked nodes hidden / Show non filtered [Ctrl]'
        ImageIndex = 37
        OnClick = MMViewHideCheckedNodesClick
      end
      object MMViewFilterTree: TMenuItem
        Caption = 'Filter nodes'
        ImageIndex = 49
        OnClick = MMViewFilterTreeClick
      end
      object N106: TMenuItem
        Caption = '-'
      end
      object MMViewZoomIn: TMenuItem
        Caption = '&Zoom In'
        Hint = 'Increase zoom  (Ctrl: apply only to active folder)'
        ImageIndex = 57
        OnClick = MMViewZoomInClick
      end
      object MMViewZoomOut: TMenuItem
        Caption = 'Zoom &Out'
        Hint = 'Decrease zoom  (Ctrl: apply only to active folder)'
        ImageIndex = 58
        OnClick = MMViewZoomOutClick
      end
      object MMAlternativeMargins: TMenuItem
        Caption = 'Alternative &Margins'
        Hint = 'Toggle between default (minimum) and defined margins in .ini'
        OnClick = MMAlternativeMarginsClick
      end
      object N54: TMenuItem
        Caption = '-'
      end
      object MMFormatView_: TMenuItem
        Caption = 'Show &Formatting'
        Hint = 'Show or hide formatting information in Status bar'
        object MMViewFormatFont: TMenuItem
          Caption = 'Show &Font Formatting'
          GroupIndex = 1
          Hint = 'Display font formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatPara: TMenuItem
          Caption = 'Show &Paragraph Formatting'
          GroupIndex = 1
          Hint = 'Display paragraph formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatBoth: TMenuItem
          Caption = 'Show &All Formatting'
          GroupIndex = 1
          Hint = 'Display font and paragraph formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object N55: TMenuItem
          Caption = '-'
          GroupIndex = 1
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatNone: TMenuItem
          Caption = '&Hide Formatting'
          GroupIndex = 1
          Hint = 'Display no formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object MMViewAlphaTabs: TMenuItem
        Caption = 'Alpha&betize Tabs'
        Hint = 'Sort notes alphabetically by name'
        OnClick = MMViewAlphaTabsClick
      end
      object MMShiftTab_: TMenuItem
        Caption = '&Shift Active Tab'
        Hint = 'Move tab left or right'
        object MMViewShiftTabLeft: TMenuItem
          Caption = 'Shift Tab &Left'
          Hint = 'Move active tab left'
          OnClick = MMViewShiftTabLeftClick
        end
        object MMViewShiftTabRight: TMenuItem
          Caption = 'Shift Tab &Right'
          Hint = 'Move active tab right'
          OnClick = MMViewShiftTabRightClick
        end
      end
    end
    object MMInsert_: TMenuItem
      Caption = '&Insert'
      SubMenuImages = IMG_Toolbar
      HelpContext = 115
      Hint = 'Insert commands'
      object MMInsertDate: TMenuItem
        Caption = 'Insert &Date'
        Hint = 'Insert date into the note'
        ImageIndex = 46
        ShortCut = 24644
        OnClick = MMInsertDateClick
      end
      object MMInsertTime: TMenuItem
        Caption = 'Insert &Time'
        Hint = 'Insert time into the note'
        ImageIndex = 47
        ShortCut = 24660
        OnClick = MMInsertTimeClick
      end
      object N62: TMenuItem
        Caption = '-'
      end
      object MMInsertCharacter: TMenuItem
        Caption = '&Character...'
        Hint = 'Insert special characters'
        ImageIndex = 48
        ShortCut = 24643
        OnClick = MMInsertCharacterClick
      end
      object MMInsertFileContents: TMenuItem
        Caption = '&File Contents...'
        Hint = 'Insert contents of a text file'
        ImageIndex = 0
        OnClick = MMInsertFileContentsClick
      end
      object N65: TMenuItem
        Caption = '-'
      end
      object MMInsertURL: TMenuItem
        Caption = '&URL...'
        Hint = 'Insert an Internet URL'
        OnClick = MMInsertURLClick
      end
      object MMInsertLinkToFile: TMenuItem
        Caption = '&Link to File...'
        Hint = 'Insert a hyperlink to a local file'
        OnClick = MMInsertLinkToFileClick
      end
      object N84: TMenuItem
        Caption = '-'
      end
      object MMInsertMarkLocation: TMenuItem
        Caption = '&Mark KeyNote Location'
        Hint = 'Mark place to which the link will jump'
        ShortCut = 16501
        OnClick = MMInsertMarkLocationClick
      end
      object MMInsertKNTLink: TMenuItem
        Caption = '&Insert KeyNote Link'
        Hint = 'Insert link to the previously marked location'
        ShortCut = 8309
        OnClick = MMInsertKNTLinkClick
      end
      object N68: TMenuItem
        Caption = '-'
      end
      object MMInsertPicture: TMenuItem
        Caption = '&Picture...'
        Hint = 'Insert an image'
        ImageIndex = 59
        OnClick = MMInsertPictureClick
      end
      object MMInsertObject: TMenuItem
        Caption = '&Object...'
        Hint = 'Inserts an OLE object'
        OnClick = MMInsertObjectClick
      end
      object N57: TMenuItem
        Caption = '-'
      end
      object MMInsertTerm: TMenuItem
        Caption = '&Expand Term'
        Hint = 'Replace current term with its Glossary definition'
        ShortCut = 118
        OnClick = MMInsertTermClick
      end
      object MMInsertWordWeb: TMenuItem
        Caption = '&WordWeb...'
        Hint = 'Look up word in WordWeb thesaurus'
        ImageIndex = 24
        ShortCut = 16506
        OnClick = RTFMWordWebClick
      end
    end
    object MMFormat_: TMenuItem
      Caption = 'F&ormat'
      SubMenuImages = IMG_Format
      HelpContext = 128
      Hint = 'Text formatting commands'
      object MMFormatFont: TMenuItem
        Caption = '&Font...'
        Hint = 'View or change font settings'
        ImageIndex = 11
        ShortCut = 16468
        OnClick = MMFormatFontClick
      end
      object MMFormatParagraph: TMenuItem
        Caption = '&Paragraph...'
        Hint = 'View or change paragraph settings'
        ImageIndex = 13
        ShortCut = 16464
        OnClick = MMFormatParagraphClick
      end
      object MMFormatLanguage: TMenuItem
        Caption = 'Lan&guage...'
        Hint = 'Specify language for selected text'
        OnClick = MMFormatLanguageClick
      end
      object N69: TMenuItem
        Caption = '-'
      end
      object MMFontStyle_: TMenuItem
        Caption = 'F&ont Style'
        Hint = 'Font style attributes'
        object MMFormatBold: TMenuItem
          Caption = '&Bold'
          Hint = 'Apply bold attribute'
          ImageIndex = 0
          ShortCut = 16450
          OnClick = MMFormatBoldClick
        end
        object MMFormatItalics: TMenuItem
          Caption = '&Italics'
          Hint = 'Apply italic attribute'
          ImageIndex = 1
          ShortCut = 16457
          OnClick = MMFormatItalicsClick
        end
        object MMFormatUnderline: TMenuItem
          Caption = '&Underline'
          Hint = 'Apply underline attribute'
          ImageIndex = 2
          ShortCut = 16469
          OnClick = MMFormatUnderlineClick
        end
        object MMFormatStrikeout: TMenuItem
          Caption = '&Strikeout'
          Hint = 'Apply strikeout attribute'
          ImageIndex = 3
          ShortCut = 16459
          OnClick = MMFormatStrikeoutClick
        end
        object N47: TMenuItem
          Caption = '-'
        end
        object MMFormatDisabled: TMenuItem
          Caption = '&Disabled'
          Hint = 'Apply disabled attribute'
          ShortCut = 16439
          OnClick = MMFormatDisabledClick
        end
        object MMFormatSuperscript: TMenuItem
          Caption = 'Superscr&ipt'
          Hint = 'Apply superscript attribute'
          ImageIndex = 19
          ShortCut = 16573
          OnClick = MMFormatSuperscriptClick
        end
        object MMFormatSubscript: TMenuItem
          Caption = 'Subsc&ript'
          Hint = 'Apply subscript attribute'
          ImageIndex = 20
          ShortCut = 24765
          OnClick = MMFormatSubscriptClick
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object MMFormatClearFontAttr: TMenuItem
          Caption = '&Clear Font Attributes'
          Hint = 'Restore default font style attributes'
          ShortCut = 16432
          OnClick = MMFormatClearFontAttrClick
        end
      end
      object MMFontsize_: TMenuItem
        Caption = 'Font &Size'
        Hint = 'Grow/Shrink font'
        object MMFormatFontSizeInc: TMenuItem
          Caption = '&Increase Font Size'
          Hint = 'Increase font size'
          ShortCut = 16605
          OnClick = MMFormatFontSizeIncClick
        end
        object MMFormatFontSizeDec: TMenuItem
          Caption = '&Decrease Font Size'
          Hint = 'Shrink font size'
          ShortCut = 16603
          OnClick = MMFormatFontSizeDecClick
        end
      end
      object N49: TMenuItem
        Caption = '-'
      end
      object MMAlignment_: TMenuItem
        Caption = '&Alignment'
        Hint = 'Text alignment commands'
        object MMFormatAlignLeft: TMenuItem
          Caption = 'Align &Left'
          GroupIndex = 1
          HelpContext = 5
          Hint = 'Align lines to the left'
          ImageIndex = 4
          RadioItem = True
          ShortCut = 49189
          OnClick = MMFormatAlignLeftClick
        end
        object MMFormatAlignCenter: TMenuItem
          Caption = 'Align &Center'
          GroupIndex = 1
          HelpContext = 10
          Hint = 'Center lines'
          ImageIndex = 5
          RadioItem = True
          ShortCut = 49190
          OnClick = MMFormatAlignCenterClick
        end
        object MMFormatAlignRight: TMenuItem
          Caption = 'Align &Right'
          GroupIndex = 1
          HelpContext = 15
          Hint = 'Align lines to the right'
          ImageIndex = 6
          RadioItem = True
          ShortCut = 49191
          OnClick = MMFormatAlignRightClick
        end
        object MMFormatAlignJustify: TMenuItem
          Caption = '&Justify'
          GroupIndex = 1
          Hint = 'Align lines with full justification'
          ImageIndex = 21
          ShortCut = 49192
          OnClick = MMFormatAlignJustifyClick
        end
      end
      object MMLineSpacing_: TMenuItem
        Caption = '&Line Spacing'
        Hint = 'Line spacing commands'
        object MMFormatLS1: TMenuItem
          Caption = 'Line Spacing &Single'
          GroupIndex = 1
          Hint = 'Select single line spacing'
          ImageIndex = 15
          RadioItem = True
          ShortCut = 16433
          OnClick = MMFormatLS1Click
        end
        object MMFormatLS15: TMenuItem
          Caption = '&Line Spacing 1.5'
          GroupIndex = 1
          Hint = 'Select one and a half line spacing'
          ImageIndex = 16
          RadioItem = True
          ShortCut = 16437
          OnClick = MMFormatLS15Click
        end
        object MMFormatLS2: TMenuItem
          Caption = 'Line Spacing &Double'
          GroupIndex = 1
          Hint = 'Select double line spacing'
          ImageIndex = 17
          RadioItem = True
          ShortCut = 16434
          OnClick = MMFormatLS2Click
        end
      end
      object MMParastyle_: TMenuItem
        Caption = 'Paragraph St&yle'
        Hint = 'Paragraph style attributes'
        object MMFormatBullets: TMenuItem
          Caption = '&Bullets'
          Hint = 'Apply bullet list style'
          ImageIndex = 7
          ShortCut = 16453
          OnClick = MMFormatBulletsClick
        end
        object MMFormatNumbers: TMenuItem
          Caption = '&Numbers'
          Hint = 'Apply numbered list style'
          ImageIndex = 14
          ShortCut = 24645
          OnClick = MMFormatNumbersClick
        end
        object N18: TMenuItem
          Caption = '-'
        end
        object MMFormatFIndInc: TMenuItem
          Caption = '&Increase First Indent'
          Hint = 'Increase indentation of first line'
          ShortCut = 16570
          OnClick = MMFormatFIndIncClick
        end
        object MMFormatFindDec: TMenuItem
          Caption = '&Decrease First Indent'
          Hint = 'Decrease indentation of first line'
          ShortCut = 24762
          OnClick = MMFormatFindDecClick
        end
        object N96: TMenuItem
          Caption = '-'
        end
        object MMFormatLIndInc: TMenuItem
          Caption = 'Increase &Left Indent'
          Hint = 'Increase left indentation'
          ImageIndex = 8
          ShortCut = 32954
          OnClick = MMFormatLIndIncClick
        end
        object MMFormatLIndDec: TMenuItem
          Caption = 'Decrease L&eft Indent'
          Hint = 'Decrease left indentation'
          ImageIndex = 9
          ShortCut = 41146
          OnClick = MMFormatLIndDecClick
        end
        object N97: TMenuItem
          Caption = '-'
        end
        object MMFormatRIndInc: TMenuItem
          Caption = 'Increase &Right Indent'
          Hint = 'Increase right indent'
          ShortCut = 16606
          OnClick = MMFormatRIndIncClick
        end
        object MMFormatRIndDec: TMenuItem
          Caption = 'Decrease Ri&ght Indent'
          Hint = 'Decrease right indent'
          ShortCut = 24798
          OnClick = MMFormatRIndDecClick
        end
        object N48: TMenuItem
          Caption = '-'
        end
        object MMFormatSpBefInc: TMenuItem
          Caption = 'Increase S&pace Before'
          Hint = 'Increase space before paragraph'
          ShortCut = 16572
          OnClick = MMFormatSpBefIncClick
        end
        object MMFormatSpBefDec: TMenuItem
          Caption = 'Decrease Space Be&fore'
          Hint = 'Decrease space before paragraph'
          ShortCut = 24764
          OnClick = MMFormatSpBefDecClick
        end
        object N98: TMenuItem
          Caption = '-'
        end
        object MMFormatSpAftInc: TMenuItem
          Caption = 'Increase Space &After'
          Hint = 'Increase space after paragraph'
          ShortCut = 16574
          OnClick = MMFormatSpAftIncClick
        end
        object MMFormatSpAftDec: TMenuItem
          Caption = 'Decrease Space Af&ter'
          Hint = 'Decrease space after paragraph'
          ShortCut = 24766
          OnClick = MMFormatSpAftDecClick
        end
        object N45: TMenuItem
          Caption = '-'
        end
        object MMFormatClearParaAttr: TMenuItem
          Caption = '&Clear Paragraph Attributes'
          Hint = 'Restore default paragraph style attributes'
          ShortCut = 24624
          OnClick = MMFormatClearParaAttrClick
        end
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object MMFormatTextColor: TMenuItem
        Caption = '&Text Color'
        ImageIndex = 23
        ShortCut = 16466
        OnClick = MMFormatTextColorClick
      end
      object MMFormatBGColor: TMenuItem
        Caption = '&Background Color'
        Hint = 
          'Change background color in note (Shift: all notes in active fold' +
          'er)'
        ShortCut = 16452
        OnClick = MMFormatBGColorClick
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object MMFormatHighlight: TMenuItem
        Caption = 'Apply &Highlight'
        ImageIndex = 24
        ShortCut = 16456
        OnClick = MMFormatHighlightClick
      end
      object MMFormatNoHighlight: TMenuItem
        Caption = '&No Highlight'
        Hint = 'Remove text highlighting'
        ShortCut = 24648
        OnClick = MMFormatNoHighlightClick
      end
      object N99: TMenuItem
        Caption = '-'
      end
      object MMFormatApplyStyle: TMenuItem
        Caption = 'Appl&y Selected Style'
        Hint = 'Apply current style to selection'
        ImageIndex = 12
        ShortCut = 8307
        OnClick = BtnStyleApplyClick
      end
      object N53: TMenuItem
        Caption = '-'
      end
      object MMCopyFormat_: TMenuItem
        Caption = '&Copy Format'
        Hint = 'Copy format for pastring'
        object MMFormatCopy: TMenuItem
          Caption = '&Copy Formatting'
          Hint = 'Copy the formatting from of a text and apply it to another'
          ImageIndex = 22
          ShortCut = 16502
          OnClick = MMFormatCopyClick
        end
        object MMFormatCopyFont: TMenuItem
          Caption = 'Copy &Font Attributes'
          Hint = 'Lift font attributes'
          ShortCut = 16503
          OnClick = MMFormatCopyFontClick
        end
        object MMFormatCopyPara: TMenuItem
          Caption = 'Copy &Paragraph Attributes'
          Hint = 'Lift paragraph attributes'
          ShortCut = 16504
          OnClick = MMFormatCopyParaClick
        end
      end
      object MMPasteFormat_: TMenuItem
        Caption = 'Paste For&mat'
        Hint = 'Paste previously copied format'
        object MMFormatPasteFont: TMenuItem
          Caption = 'Paste &Font Attributes'
          Hint = 'Apply copied font attributes'
          ShortCut = 8311
          OnClick = MMFormatPasteFontClick
        end
        object MMFormatPastePara: TMenuItem
          Caption = 'Paste &Paragraph Attributes'
          Hint = 'Apply copied paragraph attributes'
          ShortCut = 8312
          OnClick = MMFormatPasteParaClick
        end
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object MMFormatWordWrap: TMenuItem
        Caption = '&Word Wrap'
        Hint = 'Toggle wrapping at end of lines'
        ImageIndex = 18
        ShortCut = 32855
        OnClick = MMFormatWordWrapClick
      end
    end
    object MMNote_: TMenuItem
      Caption = '&Folder'
      SubMenuImages = IMG_Toolbar
      HelpContext = 179
      Hint = 'Folder (Tab) related commands'
      object MMFolderNew: TMenuItem
        Caption = '&New Folder (Tab) ...'
        Hint = 'Add a new folder (tab)'
        ImageIndex = 7
        ShortCut = 16462
        OnClick = MMFolderNewClick
      end
      object MMNoteRename: TMenuItem
        Caption = '&Rename Folder...'
        Hint = 'Rename current folder'
        ShortCut = 113
        OnClick = MMNoteRenameClick
      end
      object MMNoteProperties: TMenuItem
        Caption = 'Folder &Properties...'
        Hint = 'Edit current folder (tab) properties'
        ImageIndex = 8
        ShortCut = 115
        OnClick = MMNotePropertiesClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MMFolderRemove: TMenuItem
        Caption = 'R&emove Folder'
        Hint = 'Delete current folder'
        ImageIndex = 9
        OnClick = MMFolderRemoveClick
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object MMNotePrintPreview_: TMenuItem
        Caption = 'Print Pre&view'
        Hint = 'View how the folder will be printed'
        OnClick = MMNotePrintPreview_Click
      end
      object MMNotePrint: TMenuItem
        Caption = 'Pr&int Folder...'
        Hint = 'Print text of current folder'
        ImageIndex = 44
        ShortCut = 24656
        OnClick = MMNotePrintClick
      end
      object MMNoteEmail: TMenuItem
        Caption = '&Email Folder...'
        Hint = 'Send current folder via e-mail'
        ImageIndex = 17
        ShortCut = 57413
        OnClick = MMNoteEmailClick
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object MMNoteClipCapture: TMenuItem
        Caption = '&Clipboard Capture'
        Hint = 'Toggle clipboard capture for active folder'
        ImageIndex = 18
        ShortCut = 122
        OnClick = MMNoteClipCaptureClick
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object MMNoteSpell: TMenuItem
        Caption = 'C&heck Spelling'
        Hint = 'Check spelling in active note'
        ImageIndex = 43
        OnClick = MMNoteSpellClick
      end
      object MMNoteReadOnly: TMenuItem
        Caption = 'Read &Only'
        Hint = 'Make current folder read-only'
        ShortCut = 24658
        OnClick = MMNoteReadOnlyClick
      end
    end
    object MMTree_: TMenuItem
      Caption = 'T&ree'
      HelpContext = 189
      Hint = 'Tree-related commands'
      object MMAddTreeNode_: TMenuItem
        Caption = '&Add Tree Node'
        object AddParent1: TMenuItem
          Action = actTVAddNode_Parent
        end
        object MMTreeAddNode_Above: TMenuItem
          Action = actTVAddNode_Above
        end
        object MMTreeAddNode_Child: TMenuItem
          Action = actTVAddNode_Child
        end
        object MMTreeAddNode_Below: TMenuItem
          Action = actTVAddNode_Below
        end
        object MMTreeAddNode_Last: TMenuItem
          Action = actTVAddNode_Last
        end
      end
      object MMMovenode_: TMenuItem
        Caption = '&Move Node[s]'
        Hint = 'Move selected nodes in tree'
        object MMTreeMoveNodeUp_: TMenuItem
          Action = actTVMoveNodeUp
          Caption = 'Move Node &Up'
        end
        object MMTreeMoveNodeDown_: TMenuItem
          Action = actTVMoveNodeDown
          Caption = 'Move Node &Down'
        end
        object MMTreeMoveNodeLeft_: TMenuItem
          Action = actTVMoveNodeLeft
          Caption = 'Move Node &Left'
        end
        object MMTreeMoveNodeRight_: TMenuItem
          Action = actTVMoveNodeRight
          Caption = 'Move Node &Right'
        end
      end
      object MMTreeMasterNode: TMenuItem
        Caption = 'Create Ma&ster Node'
        Hint = 'Create a node as parent of all existing nodes'
        OnClick = MMTreeMasterNodeClick
      end
      object MMTreeNodeFromSel: TMenuItem
        Caption = 'Ne&w Node from Selection'
        Hint = 'Create a new node using selected text'
        ShortCut = 32890
        OnClick = MMTreeNodeFromSelClick
      end
      object N87: TMenuItem
        Caption = '-'
      end
      object MMTreeFullExpand: TMenuItem
        Caption = '&Expand Tree'
        Hint = 'Fully expand tree'
        OnClick = MMTreeFullExpandClick
      end
      object MMTreeFullCollapse: TMenuItem
        Caption = 'Co&llapse Tree'
        Hint = 'Fully collapse tree'
        OnClick = MMTreeFullCollapseClick
      end
      object MMTreeNav_: TMenuItem
        Caption = '&Navigate'
        object MMTreeNavUp: TMenuItem
          Caption = 'Go &Up'
          Hint = 'Move to previous node'
          ShortCut = 32806
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavDown: TMenuItem
          Caption = 'Go &Down'
          Hint = 'Move to next node'
          ShortCut = 32808
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavLeft: TMenuItem
          Caption = 'Go &Left'
          Hint = 'Move left in tree'
          ShortCut = 40997
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavRight: TMenuItem
          Caption = 'Go &Right'
          Hint = 'Move right in tree'
          ShortCut = 40999
          OnClick = MMTreeNavRightClick
        end
      end
      object N41: TMenuItem
        Caption = '-'
      end
      object MMTreeNodeDelete_: TMenuItem
        Action = actTVDeleteNode
      end
      object MMTreeDeleteSubtree_: TMenuItem
        Action = actTVDeleteChildren
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object MMTreeNodeRename_: TMenuItem
        Action = actTVRenameNode
      end
      object MMNodePaste_: TMenuItem
        Caption = '&Paste Node Name'
        Hint = 'Rename node from clipboard'
        object MMTreeNodeNamePaste: TMenuItem
          Action = actTVPasteNodeName
        end
        object N61: TMenuItem
          Caption = '-'
        end
        object MMTreeNodeNameAsDate: TMenuItem
          Action = actTVPasteNodeNameAsDate
        end
        object MMTreeNodeNameAsTime: TMenuItem
          Action = actTVPasteNodeNameAsTime
        end
        object MMTreeNodeNameAsDateTime: TMenuItem
          Action = actTVPasteNodeNameAsDateTime
        end
        object N82: TMenuItem
          Caption = '-'
        end
        object MMTreeNodeNameAsSel: TMenuItem
          Action = actTVPasteNodeNameAsSel
        end
      end
      object N36: TMenuItem
        Caption = '-'
      end
      object MMTreeSaveToFile: TMenuItem
        Caption = 'Sa&ve Tree to File...'
        Hint = 'Save tree structure to file (Ctrl: Show nodes ID)'
        OnClick = MMTreeSaveToFileClick
      end
      object MMTreeSort_: TMenuItem
        Caption = 'S&ort'
        Hint = 'Sort nodes'
        object MMTreeSortSubtree_: TMenuItem
          Action = actTVSortSubtree
        end
        object MMTreeSortFull_: TMenuItem
          Action = actTVSortTree
        end
      end
      object N92: TMenuItem
        Caption = '-'
      end
      object MMTreeOutlineNum: TMenuItem
        Caption = 'O&utline Numbering...'
        Hint = 'Add sequential numbers to node names'
        OnClick = MMTreeOutlineNumClick
      end
      object MMTreeFocus_: TMenuItem
        Caption = 'Set Focus'
        object MMTreeFocusToogle: TMenuItem
          Caption = 'Toggle focus: Editor / Tree panel'
          ShortCut = 16411
          OnClick = MMTreeFocusToogleClick
        end
        object MMTreeFocusEditor: TMenuItem
          Caption = 'Set focus in Editor panel'
          OnClick = MMTreeFocusEditorClick
        end
        object MMTreeFocusTree: TMenuItem
          Caption = 'Set focus in Tree panel'
          OnClick = MMTreeFocusTreeClick
        end
      end
    end
    object MMSearch_: TMenuItem
      Caption = '&Search'
      SubMenuImages = IMG_Toolbar
      HelpContext = 225
      Hint = 'Search commands'
      object MMFind: TMenuItem
        Caption = '&Find...'
        Hint = 'Search for text in notes'
        ImageIndex = 14
        ShortCut = 16454
        OnClick = MMFindClick
      end
      object MMFindNext: TMenuItem
        Caption = 'Find &Next'
        Hint = 'Repeat last search command'
        ImageIndex = 15
        ShortCut = 114
        OnClick = MMFindNextClick
      end
      object MMFindAll: TMenuItem
        Caption = 'Find &All...'
        Hint = 'Find all occurrences of the searched text'
        OnClick = MMFindAllClick
      end
      object N74: TMenuItem
        Caption = '-'
      end
      object MMFindReplace: TMenuItem
        Caption = '&Replace...'
        Hint = 'Replace text in notes'
        ImageIndex = 42
        ShortCut = 16498
        OnClick = MMFindReplaceClick
      end
      object MMFindReplaceNext: TMenuItem
        Caption = 'Re&place Next'
        Hint = 'Repeat last replace command'
        OnClick = MMFindReplaceNextClick
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object MMBkmSet_: TMenuItem
        Caption = '&Set Bookmark'
        Hint = 'Mark bookmarks in note'
        object MMBkmSet0: TMenuItem
          Caption = '0'
          Hint = 'Set bookmark '
          ShortCut = 41008
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet1: TMenuItem
          Caption = '1'
          Hint = 'Set bookmark '
          ShortCut = 41009
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet2: TMenuItem
          Caption = '2'
          Hint = 'Set bookmark '
          ShortCut = 41010
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet3: TMenuItem
          Caption = '3'
          Hint = 'Set bookmark '
          ShortCut = 41011
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet4: TMenuItem
          Caption = '4'
          Hint = 'Set bookmark '
          ShortCut = 41012
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet5: TMenuItem
          Caption = '5'
          Hint = 'Set bookmark '
          ShortCut = 41013
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet6: TMenuItem
          Caption = '6'
          Hint = 'Set bookmark '
          ShortCut = 41014
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet7: TMenuItem
          Caption = '7'
          Hint = 'Set bookmark '
          ShortCut = 41015
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet8: TMenuItem
          Caption = '8'
          Hint = 'Set bookmark '
          ShortCut = 41016
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet9: TMenuItem
          Caption = '9'
          Hint = 'Set bookmark '
          ShortCut = 41017
          OnClick = MMBkmSet9Click
        end
      end
      object MMBkmJump_: TMenuItem
        Caption = '&Jump to Bookmark'
        Hint = 'Jump to previously marked bookmarks'
        object MMBkmJ0: TMenuItem
          Caption = '0'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32816
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ1: TMenuItem
          Caption = '1'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32817
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ2: TMenuItem
          Caption = '2'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32818
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ3: TMenuItem
          Caption = '3'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32819
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ4: TMenuItem
          Caption = '4'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32820
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ5: TMenuItem
          Caption = '5'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32821
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ6: TMenuItem
          Caption = '6'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32822
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ7: TMenuItem
          Caption = '7'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32823
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ8: TMenuItem
          Caption = '8'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32824
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ9: TMenuItem
          Caption = '9'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32825
          OnClick = MMBkmJ9Click
        end
      end
      object N46: TMenuItem
        Caption = '-'
      end
      object MMFindBracket: TMenuItem
        Caption = '&Match Bracket'
        Hint = 'Find matching bracket'
        ShortCut = 16461
        OnClick = MMFindBracketClick
      end
      object MMFindGoTo: TMenuItem
        Caption = '&Go To Line...'
        Hint = 'Move to a specific line in note'
        ShortCut = 16455
        OnClick = MMFindGoToClick
      end
      object N39: TMenuItem
        Caption = '-'
      end
      object MMFindNode: TMenuItem
        Caption = 'Find N&ode...'
        Hint = 'Find tree node'
        ShortCut = 24646
        OnClick = MMFindNodeClick
      end
      object MMFindNodeNext: TMenuItem
        Caption = 'Find N&ext Node'
        Hint = 'Repeat last Find node command'
        OnClick = MMFindNodeNextClick
      end
    end
    object MMTools_: TMenuItem
      Caption = '&Tools'
      SubMenuImages = IMG_Toolbar
      HelpContext = 236
      Hint = 'Configuration and miscellaneous tools'
      object MMToolsOptions: TMenuItem
        Caption = '&Configuration Options...'
        Hint = 'Adjust program options'
        ImageIndex = 10
        ShortCut = 116
        OnClick = MMToolsOptionsClick
      end
      object MMToolsDefaults: TMenuItem
        Caption = '&Default Settings...'
        Hint = 'Set defaults for all newly created files'
        ShortCut = 117
        OnClick = MMToolsDefaultsClick
      end
      object MMToolsCustomKBD: TMenuItem
        Caption = 'Customize Keyboard...'
        Hint = 'Assign custom keyboard shortcuts to menu items'
        OnClick = MMToolsCustomKBDClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MMToolsMacroRun: TMenuItem
        Caption = '&Run Macro'
        Hint = 'Execute selected macro'
        ShortCut = 8308
        OnClick = TB_MacroClick
      end
      object MMToolsMacroRunLast: TMenuItem
        Caption = 'Run Last Macro'
        Hint = 'Execute most recent macro'
        ShortCut = 16500
        OnClick = MMToolsMacroRunLastClick
      end
      object MMToolsMacroSelect: TMenuItem
        Caption = '&Select Macro...'
        Hint = 'Select and run macro'
        OnClick = MMToolsMacroSelectClick
      end
      object N77: TMenuItem
        Caption = '-'
      end
      object MMToolsPluginRun: TMenuItem
        Caption = 'Run &Plugin'
        Hint = 'Execute selected plugin'
        ShortCut = 8315
        OnClick = MMToolsPluginRunClick
      end
      object MMToolsPluginRunLast: TMenuItem
        Caption = 'Run &Last Plugin'
        Hint = 'Execute most recent plugin used'
        ShortCut = 16507
        OnClick = MMToolsPluginRunLastClick
      end
      object N75: TMenuItem
        Caption = '-'
      end
      object MMToolsGlosAddTerm: TMenuItem
        Caption = 'Add &Glossary Term'
        Hint = 'Create a new glossary term'
        ShortCut = 8310
        OnClick = MMToolsGlosAddTermClick
      end
      object MMToolsGlosEdit: TMenuItem
        Caption = 'Edit Glossary...'
        Hint = 'Edit glossary terms'
        OnClick = MMToolsGlosEditClick
      end
      object N73: TMenuItem
        Caption = '-'
      end
      object MMToolsMerge: TMenuItem
        Caption = '&Merge Folders...'
        Hint = 'Add folders from a knt file on disk'
        OnClick = MMToolsMergeClick
      end
      object MMTemplates_: TMenuItem
        Caption = 'Te&mplates'
        Hint = 'Template commands'
        object MMToolsTemplateCreate: TMenuItem
          Caption = '&Create Template...'
          Hint = 'Create new template based on selected text'
          OnClick = MMToolsTemplateCreateClick
        end
        object MMToolsTemplateInsert: TMenuItem
          Caption = '&Insert Template...'
          Hint = 'Insert template into current note'
          OnClick = MMToolsTemplateInsertClick
        end
      end
      object N120: TMenuItem
        Caption = '-'
      end
      object MMAlarms: TMenuItem
        Caption = '&Alarms'
        object MMSetAlarm: TMenuItem
          Caption = 'Set alarm...'
          Hint = 'Add or edit an alarm (Ctrl: Add  Shift: Folder alarm)'
          ImageIndex = 50
          OnClick = TB_SetAlarmClick
        end
        object MMShowAlarms: TMenuItem
          Caption = 'Show alarms...'
          OnClick = MMShowAlarmsClick
        end
        object MMAlarmsPopup: TMenuItem
          Caption = 'Reminder Popups'
          Hint = 'Modal window will be shown on alarms trigger'
          OnClick = MMAlarmsPopupClick
        end
      end
      object N35: TMenuItem
        Caption = '-'
      end
      object MMToolsUAS: TMenuItem
        Caption = 'UAS Integration'
        Hint = 'Enable or disable UltimaShell Autocompletion Server integration'
        OnClick = MMToolsUASClick
      end
      object MMToolsUASConfig: TMenuItem
        Caption = 'Configure UAS'
        Hint = 'Configure UltimaShell Autocompletion Server'
        OnClick = MMToolsUASConfigClick
      end
      object N63: TMenuItem
        Caption = '-'
      end
      object MMToolsDeduceDates: TMenuItem
        Caption = 'Deduce [missing] date information'
        OnClick = MMToolsDeduceDatesClick
      end
      object MMToolsRemoveDates: TMenuItem
        Caption = 'Remove date prefixes'
        OnClick = MMToolsRemoveDatesClick
      end
      object MMToolsStatistics: TMenuItem
        Caption = '&Text Statistics'
        Hint = 'Display folder statistics'
        OnClick = MMToolsStatisticsClick
      end
    end
    object MMHelp_: TMenuItem
      Caption = '&Help'
      SubMenuImages = IMG_Toolbar
      HelpContext = 272
      Hint = 'Information about the program'
      object MMHelpMain: TMenuItem
        Caption = '&General Help'
        Hint = 'Display general help topics'
        ImageIndex = 23
        OnClick = MMHelpMainClick
      end
      object MMHelpKeyboardRef: TMenuItem
        Caption = '&Keyboard Reference'
        Hint = 'Display keyboard reference'
        OnClick = MMHelpKeyboardRefClick
      end
      object N79: TMenuItem
        Caption = '-'
      end
      object MMHelpTip: TMenuItem
        Caption = '&Tip of the Day'
        Hint = 'Display the Tip of the Day dialog'
        ImageIndex = 60
        OnClick = MMHelpTipClick
      end
      object MMHelpWhatsNew: TMenuItem
        Caption = '&What'#39's New'
        Hint = 'Display KeyNote revision history'
        OnClick = MMHelpWhatsNewClick
      end
      object N100: TMenuItem
        Caption = '-'
      end
      object MMHelpVisitWebsite: TMenuItem
        Caption = '&Visit KeyNote NF website'
        Hint = 
          'Go to KeyNote NF website, for information, to submit bug report,' +
          ' feature request, etc.'
        OnClick = MMHelpVisitWebsiteClick
      end
      object MMHelpEmailAuthor: TMenuItem
        Caption = '&Email to Author'
        Hint = 'Start mail client and send email to keyNote author'
        Visible = False
      end
      object N72: TMenuItem
        Caption = '-'
      end
      object MMHelpChkUpd: TMenuItem
        Caption = 'Check for &Updates'
        OnClick = MMHelpChkUpdClick
      end
      object MMHelpAbout: TMenuItem
        Caption = '&About KeyNote'
        Hint = 'Information about the program'
        OnClick = MMHelpAboutClick
      end
    end
  end
  object FormStorage: TFormStorage
    IniFileName = 'Software\General Frenetics\KeyNote\FormPos'
    IniSection = 'Main'
    UseRegistry = True
    OnRestorePlacement = FormStorageRestorePlacement
    StoredProps.Strings = (
      'Pages_Res.Width'
      'Pages_Res.ActivePage')
    StoredValues = <>
    Left = 622
    Top = 164
  end
  object MRU: TdfsMRUFileList
    UseRegistry = False
    RemoveOnClick = False
    UseSubmenu = True
    SubmenuName = '&Recent Files'
    ClearItemName = '&Clear MRU List'
    RemoveObsoleteName = '&Remove Obsolete'
    OnMRUItemClick = MRUMRUItemClick
    FileMenu = MM_MRUSeparator_
    PopupMenu = MRUMenu
    AutoSaveName = '\Software\My Application'
    AutoSaveKey = 'MRU Items'
    Left = 515
    Top = 148
  end
  object Timer: TTimer
    Interval = 60000
    OnTimer = TimerTimer
    Left = 623
    Top = 202
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 623
    Top = 80
  end
  object TrayIcon: TRxTrayIcon
    Active = False
    PopupMenu = Menu_Tray
    OnClick = TrayIconClick
    Left = 616
    Top = 379
  end
  object Menu_Tray: TPopupMenu
    HelpContext = 238
    Left = 584
    Top = 379
    object TMRestore: TMenuItem
      Caption = '&Restore'
      OnClick = TMRestoreClick
    end
    object TMClipCap: TMenuItem
      Caption = '&Clipboard capture'
      HelpContext = 310
      OnClick = MMNoteClipCaptureClick
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object TMExit: TMenuItem
      Caption = 'E&xit'
      OnClick = MMFileExitClick
    end
  end
  object Menu_RTF: TPopupMenu
    HelpContext = 282
    Images = IMG_Format
    ParentBiDiMode = False
    Left = 361
    Top = 120
    object RTFMUndo: TMenuItem
      Caption = '&Undo'
      Hint = 'Undo last change'
      OnClick = MMEditUndoClick
    end
    object RTFMRepeatCmd: TMenuItem
      Caption = '&Repeat Last Command'
      Hint = 'Repeat last editing command'
      OnClick = MMEditRepeatClick
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object RTFMCut: TMenuItem
      Caption = 'Cu&t'
      Hint = 'Cut text to clipboard'
      OnClick = MMEditCutClick
    end
    object RTFMCopy: TMenuItem
      Caption = '&Copy'
      Hint = 'Copy text to clipboard'
      OnClick = MMEditCopyClick
    end
    object RTFMPaste: TMenuItem
      Caption = '&Paste'
      Hint = 'Paste text from clipboard'
      OnClick = MMEditPasteClick
    end
    object RTFMPasteAsText: TMenuItem
      Caption = 'Paste as Te&xt'
      Hint = 'Paste text from clipboard without formatting'
      OnClick = MMEditPasteAsTextClick
    end
    object RTFMDelete: TMenuItem
      Caption = '&Delete'
      Hint = 'Delete selected text'
      OnClick = MMEditDeleteClick
    end
    object N70: TMenuItem
      Caption = '-'
    end
    object RTFMFont: TMenuItem
      Caption = '&Font...'
      ImageIndex = 11
      OnClick = MMFormatFontClick
    end
    object RTFMPara: TMenuItem
      Caption = 'P&aragraph...'
      ImageIndex = 13
      OnClick = MMFormatParagraphClick
    end
    object N43: TMenuItem
      Caption = '-'
    end
    object RTFMWordWeb: TMenuItem
      Caption = '&Look Up in WordWeb'
      HelpContext = 302
      Hint = 'Look up word in WordWeb thesaurus'
      OnClick = RTFMWordWebClick
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object RTFMProperties: TMenuItem
      Caption = 'Folder P&roperties...'
      Hint = 'Edit folder properties'
      OnClick = MMNotePropertiesClick
    end
    object RTFMPlainText: TMenuItem
      Caption = 'Plain Text'
      Hint = 'Convert to plain text or rich text format (RTF)'
      OnClick = RTFMPlainTextClick
    end
    object RTFM_RTL: TMenuItem
      Caption = 'Reading from right to left (RTL)'
      OnClick = RTFM_RTLClick
    end
    object N88: TMenuItem
      Caption = '-'
    end
    object RTFMFold: TMenuItem
      Caption = 'Fold'
      Hint = 'Collapse selected text'
      OnClick = RTFMFoldClick
    end
    object RTFMUnfold: TMenuItem
      Caption = 'Unfold'
      Hint = 'Expand folded/collapsed text'
      OnClick = RTFMUnfoldClick
    end
    object RTFMTags: TMenuItem
      Caption = 'Tags...'
      OnClick = RTFMTagsClick
    end
    object N19: TMenuItem
      Caption = '-'
    end
    object RTFMSelectall: TMenuItem
      Caption = 'Select &All'
      Hint = 'Select all text'
      OnClick = MMEditSelectAllClick
    end
    object RTFMWordwrap: TMenuItem
      Caption = '&Word Wrap'
      Hint = 'Toggle wrapping at end of lines'
      ImageIndex = 18
      OnClick = MMFormatWordWrapClick
    end
    object N118: TMenuItem
      Caption = '-'
    end
    object RTFMRestoreProportions: TMenuItem
      Caption = 'Restore &image[s] proportions'
      HelpContext = -1
      Hint = 
        'Restore proportions and reconsider "Max.auto width" on selected ' +
        'image[s] (if no selection: all images in editor)'
      OnClick = RTFMRestoreProportionsClick
    end
  end
  object Menu_TAB: TPopupMenu
    AutoPopup = False
    HelpContext = 317
    Images = IMG_Toolbar
    Left = 359
    Top = 83
    object TAM_ActiveName: TMenuItem
      Caption = '(none)'
      Enabled = False
    end
    object N24: TMenuItem
      Caption = '-'
    end
    object TAM_NewTab: TMenuItem
      Caption = '&New Folder (Tab) ...'
      Hint = 'Create a new folder (Tab)'
      ImageIndex = 7
      OnClick = MMFolderNewClick
    end
    object N16: TMenuItem
      Caption = '-'
    end
    object TAM_Renametab: TMenuItem
      Caption = '&Rename Folder'
      Hint = 'Rename current folder'
      OnClick = MMNoteRenameClick
    end
    object TAM_Properties: TMenuItem
      Caption = '&Properties'
      Hint = 'Edit folder properties'
      ImageIndex = 8
      OnClick = MMNotePropertiesClick
    end
    object N17: TMenuItem
      Caption = '-'
    end
    object TAM_Delete: TMenuItem
      Caption = 'R&emove Folder'
      Hint = 'Delete current folder'
      ImageIndex = 9
      OnClick = MMFolderRemoveClick
    end
  end
  object IMG_Toolbar: TImageList
    Left = 90
    Top = 75
  end
  object OpenDlg: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoDereferenceLinks]
    Left = 28
    Top = 105
  end
  object SaveDlg: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Title = 'Save Keynote file'
    Left = 27
    Top = 205
  end
  object IMG_Format: TImageList
    Left = 89
    Top = 109
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 28
    Top = 170
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 28
    Top = 138
  end
  object FolderMon: TRxFolderMonitor
    Filter = [fnSize, fnLastWrite]
    MonitorSubtree = False
    OnChange = FolderMonChange
    Left = 622
    Top = 117
  end
  object MRUMenu: TPopupMenu
    HelpContext = 539
    Left = 509
    Top = 74
    object N33: TMenuItem
      Caption = '-'
    end
    object MruM_MRUSeparatorBTN_: TMenuItem
      Caption = '&File Manager...'
      OnClick = MMFileManagerClick
    end
  end
  object IMG_TV: TImageList
    Left = 91
    Top = 141
  end
  object Menu_Style: TPopupMenu
    HelpContext = 301
    Images = IMG_Toolbar
    Left = 41
    Top = 340
    object MSStyleApply: TMenuItem
      Caption = '&Apply Selected Style'
      Hint = 'Apply style to selection'
      ImageIndex = 25
      OnClick = BtnStyleApplyClick
    end
    object N51: TMenuItem
      Caption = '-'
    end
    object MSStyleFont: TMenuItem
      Caption = 'Create &Font Style'
      GroupIndex = 1
      Hint = 'Create style for Font properties'
      ImageIndex = 26
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object MSStylePara: TMenuItem
      Caption = 'Create &Paragraph Style'
      GroupIndex = 1
      Hint = 'Create style for Paragraph properties'
      ImageIndex = 27
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object MSStyleBoth: TMenuItem
      Caption = '&Create Combined Style'
      GroupIndex = 1
      Hint = 'Create style for Font and Paragraph properties'
      ImageIndex = 28
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object N50: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MSStyleDescribe: TMenuItem
      Tag = 93
      Caption = 'D&escribe'
      GroupIndex = 1
      Hint = 'Display properties for selected style'
      OnClick = BtnStyleApplyClick
    end
    object MSStyleRedef: TMenuItem
      Tag = 92
      Caption = 'Rede&fine'
      GroupIndex = 1
      Hint = 'Redefine style based on selection'
      OnClick = BtnStyleApplyClick
    end
    object N52: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MSStyleRename: TMenuItem
      Tag = 90
      Caption = '&Rename Style'
      GroupIndex = 1
      Hint = 'Rename style definition'
      OnClick = BtnStyleApplyClick
    end
    object MSStyleDelete: TMenuItem
      Tag = 91
      Caption = '&Delete Style'
      GroupIndex = 1
      Hint = 'Delete style definition'
      OnClick = BtnStyleApplyClick
    end
  end
  object Menu_Macro: TPopupMenu
    HelpContext = 304
    Images = IMG_Toolbar
    Left = 387
    Top = 345
    object MacMMacro_Play: TMenuItem
      Caption = '&Play Macro'
      Hint = 'Run selected macro'
      ImageIndex = 29
      ShortCut = 13
      OnClick = TB_MacroClick
    end
    object N66: TMenuItem
      Caption = '-'
    end
    object MacMMacro_Edit: TMenuItem
      Caption = '&Edit Macro'
      Hint = 'Rename selected macro'
      ShortCut = 32
      OnClick = MacMMacro_EditClick
    end
    object MacMMacro_Delete: TMenuItem
      Caption = '&Delete Macro'
      Hint = 'Delete selected macro'
      ShortCut = 46
      OnClick = MacMMacro_DeleteClick
    end
    object N78: TMenuItem
      Caption = '-'
    end
    object MacMMacro_Record: TMenuItem
      Caption = '&Record Macro...'
      Hint = 'Record a new macro'
      ImageIndex = 31
      ShortCut = 45
      OnClick = TB_MacroRecordClick
    end
    object MacMMacroUserCommand: TMenuItem
      Caption = '&User Command...'
      Hint = 'Add user command to macro'
      ShortCut = 187
      OnClick = MacMMacroUserCommandClick
    end
  end
  object Menu_Template: TPopupMenu
    HelpContext = 307
    Left = 352
    Top = 344
    object TPLMTplInsert: TMenuItem
      Caption = '&Insert Template'
      Hint = 'Insert selected template in active note'
      ShortCut = 13
      OnClick = TPLMTplInsertClick
    end
    object N83: TMenuItem
      Caption = '-'
    end
    object TPLMTplCreate: TMenuItem
      Caption = '&Create Template...'
      Hint = 'Create a new template based on active note text'
      ShortCut = 45
      OnClick = MMToolsTemplateCreateClick
    end
    object TPLMTplDelete: TMenuItem
      Caption = '&Delete Template'
      Hint = 'Delete selected template'
      ShortCut = 46
      OnClick = TPLMTplDeleteClick
    end
  end
  object Menu_Plugins: TPopupMenu
    HelpContext = 305
    Images = IMG_Toolbar
    Left = 317
    Top = 343
    object PLM_RunPlugin: TMenuItem
      Caption = '&Run Plugin'
      Hint = 'Execute selected plugin'
      ImageIndex = 29
      ShortCut = 13
      OnClick = PLM_RunPluginClick
    end
    object PLM_ConfigurePlugin: TMenuItem
      Caption = '&Configure Plugin'
      Hint = 'Configure selected plugin'
      ShortCut = 32
      OnClick = PLM_ConfigurePluginClick
    end
    object N64: TMenuItem
      Caption = '-'
    end
    object PLM_ReloadPlugins: TMenuItem
      Caption = 'R&efresh List'
      Hint = 'Re-read plugin information'
      OnClick = PLM_ReloadPluginsClick
    end
  end
  object Menu_ResPanel: TPopupMenu
    HelpContext = -4
    OnPopup = Menu_ResPanelPopup
    Left = 266
    Top = 308
    object ResMHidepanel: TMenuItem
      Caption = 'Hide &Resource Panel'
      Hint = 'Show or hide Resource panel (press F9)'
      OnClick = MMViewResPanelClick
    end
    object N71: TMenuItem
      Caption = '-'
    end
    object ResMMultilineTabs: TMenuItem
      Caption = 'M&ultiline Tabs'
      Hint = 'Arrange tabs in rows'
      OnClick = ResMMultilineTabsClick
    end
    object ResMTabPosition: TMenuItem
      Caption = 'T&ab Position'
      object ResMTop: TMenuItem
        Caption = '&Top'
        GroupIndex = 1
        Hint = 'Place tabs on top'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMBottom: TMenuItem
        Tag = 1
        Caption = '&Bottom'
        GroupIndex = 1
        Hint = 'Place tabs at bottom'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMLeft: TMenuItem
        Tag = 2
        Caption = '&Left'
        GroupIndex = 1
        Hint = 'Place tabs left'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMRight: TMenuItem
        Tag = 3
        Caption = '&Right'
        GroupIndex = 1
        Hint = 'Place tabs right'
        RadioItem = True
        OnClick = ResMRightClick
      end
    end
    object ResMPanelPosition: TMenuItem
      Caption = 'Pa&nel Position'
      object ResMPanelLeft: TMenuItem
        Caption = '&Left'
        GroupIndex = 2
        RadioItem = True
        OnClick = ResMPanelRightClick
      end
      object ResMPanelRight: TMenuItem
        Tag = 1
        Caption = '&Right'
        GroupIndex = 2
        RadioItem = True
        OnClick = ResMPanelRightClick
      end
    end
    object N81: TMenuItem
      Caption = '-'
    end
    object ResMFindTab: TMenuItem
      Caption = '&Find Tab'
      Hint = 'Show or hide the "Find" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMScratchTab: TMenuItem
      Tag = 1
      Caption = '&Scratch Tab'
      Hint = 'Show or hide the "Scratch" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMMacroTab: TMenuItem
      Tag = 2
      Caption = '&Macros Tab'
      Hint = 'Show or hide the "Macro" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMTemplateTab: TMenuItem
      Tag = 3
      Caption = '&Templates Tab'
      Hint = 'Show or hide the "Template" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMPluginTab: TMenuItem
      Tag = 4
      Caption = '&Plugins Tab'
      Hint = 'Show or hide the "Plugin" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMFavTab: TMenuItem
      Tag = 5
      Caption = 'Fa&vorites Tab'
      Hint = 'Show or hide the "Favorites" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMTagsTab: TMenuItem
      Tag = 6
      Caption = 'Ta&gs Tab'
      Hint = 'Show or hide the "Tags" tab'
      OnClick = ResMPluginTabClick
    end
  end
  object Menu_StdEdit: TPopupMenu
    HelpContext = 282
    Images = IMG_Toolbar
    OnPopup = Menu_StdEditPopup
    Left = 441
    Top = 71
    object StdEMUndo: TMenuItem
      Caption = '&Undo'
      HelpContext = 282
      Hint = 'Undo last editing operation'
      ImageIndex = 6
      ShortCut = 16474
      OnClick = StdEMSelectAllClick
    end
    object N85: TMenuItem
      Caption = '-'
    end
    object StdEMCut: TMenuItem
      Tag = 1
      Caption = 'Cu&t'
      Hint = 'Cut selection to clipboard (Ctrl+X, Shift+Supr)'
      ImageIndex = 3
      OnClick = StdEMSelectAllClick
    end
    object StdEMCopy: TMenuItem
      Tag = 2
      Caption = '&Copy'
      Hint = 'Copy selection to clipboard (Ctrl+C, Ctrl+Ins)'
      ImageIndex = 4
      OnClick = StdEMSelectAllClick
    end
    object StdEMPaste: TMenuItem
      Tag = 3
      Caption = '&Paste'
      Hint = 'Paste text from clipboard (Ctrl+V, Shift+Ins)'
      ImageIndex = 5
      OnClick = StdEMSelectAllClick
    end
    object StdEMPastePlain: TMenuItem
      Tag = 4
      Caption = 'Paste as Text'
      Hint = 'Paste text without formatting'
      ShortCut = 16468
      OnClick = StdEMSelectAllClick
    end
    object StdEMDelete: TMenuItem
      Tag = 5
      Caption = '&Delete'
      Hint = 'Delete selection'
      OnClick = StdEMSelectAllClick
    end
    object N86: TMenuItem
      Caption = '-'
    end
    object StdEMSelectAll: TMenuItem
      Tag = 6
      Caption = 'Select &All'
      Hint = 'Select all text'
      ShortCut = 16449
      OnClick = StdEMSelectAllClick
    end
    object StdEMWordWrap: TMenuItem
      Tag = 7
      Caption = 'Word Wrap'
      ShortCut = 16471
      OnClick = StdEMSelectAllClick
    end
  end
  object Menu_FindAll: TPopupMenu
    HelpContext = -5
    Left = 250
    Top = 358
    object FAMCopytoEditor: TMenuItem
      Caption = '&Insert in Note'
      Hint = 'Insert selected location as hyperlink in note'
      OnClick = FAMCopytoEditorClick
    end
    object FAMCopyAlltoEditor: TMenuItem
      Caption = 'Insert &All in Note'
      Hint = 'Insert all locations as hyperlinks in note'
      OnClick = FAMCopyAlltoEditorClick
    end
  end
  object Menu_Fav: TPopupMenu
    HelpContext = 601
    Left = 421
    Top = 345
    object FavMJump: TMenuItem
      Caption = '&Jump to Location'
      Hint = 'Jump to selected Favorite location (Ctrl:  Ext.-> other inst.)'
      ShortCut = 13
      OnClick = FavMJumpClick
    end
    object N89: TMenuItem
      Caption = '-'
    end
    object FavMAdd: TMenuItem
      Caption = '&Add Location'
      Hint = 'Add current location as Favorite'
      ShortCut = 45
      OnClick = FavMAddClick
    end
    object FavMAddExternal: TMenuItem
      Caption = 'Add &External...'
      Hint = 'Add external program or document as favorite'
      ShortCut = 8237
      OnClick = FavMAddExternalClick
    end
    object N113: TMenuItem
      Caption = '-'
    end
    object FavMProperties: TMenuItem
      Caption = '&Properties...'
      ShortCut = 32
      OnClick = FavMPropertiesClick
    end
    object N91: TMenuItem
      Caption = '-'
    end
    object FavMDel: TMenuItem
      Caption = '&Delete Location'
      Hint = 'Delete selected location from Favorites'
      ShortCut = 46
      OnClick = FavMDelClick
    end
    object N38: TMenuItem
      Caption = '-'
    end
    object FavMRef: TMenuItem
      Caption = 'Refresh &List'
      Hint = 'Reload Favorites from disk'
      OnClick = FavMRefClick
    end
  end
  object Menu_Numbers: TPopupMenu
    HelpContext = -3
    Left = 151
    Top = 297
    object MMArabicNumbers: TMenuItem
      Caption = '&Arabic Numbers'
      Checked = True
      Hint = 'Select Arabic number numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object N94: TMenuItem
      Caption = '-'
    end
    object MMLoLetter: TMenuItem
      Caption = '&Lowercase Letter'
      Hint = 'Select lowercase letter numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object MMUpLetter: TMenuItem
      Caption = '&Uppercase Letter'
      Hint = 'Select uppercase letter numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object N95: TMenuItem
      Caption = '-'
    end
    object MMLoRoman: TMenuItem
      Caption = 'Lowercase &Roman'
      Hint = 'Select lowercase Roman numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object MMUpRoman: TMenuItem
      Caption = 'U&ppercase Roman'
      Hint = 'Select uppercase Roman numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object NU01: TMenuItem
      Caption = '-'
    end
    object MMRightParenthesis: TMenuItem
      Caption = 'Right parenthesis'
      GroupIndex = 1
      Hint = 'Follows the number with a right parenthesis'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMEnclosed: TMenuItem
      Caption = 'Enclosed in parenthesis'
      GroupIndex = 1
      Hint = 'Encloses the number in parentheses'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMPeriod: TMenuItem
      Caption = 'Period'
      Checked = True
      GroupIndex = 1
      Hint = 'Follows the number with a period'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMOnlyNumber: TMenuItem
      Caption = 'Only number'
      GroupIndex = 1
      Hint = 'Displays only the number'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object NU02: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MMWithoutNextNumber: TMenuItem
      Caption = 'No Next number or bullet'
      GroupIndex = 2
      Hint = 
        'Continues a numbered list without applying the next number or bu' +
        'llet'
      OnClick = MMRightParenthesisClick
    end
    object MMStartsNewNumber: TMenuItem
      Caption = 'Starts New number...'
      GroupIndex = 2
      Hint = 'Selects a new starting number'
      OnClick = MMStartsNewNumberClick
    end
  end
  object Menu_Paste: TPopupMenu
    HelpContext = 582
    Left = 475
    Top = 73
    object MMP_Paste: TMenuItem
      Caption = '&Paste'
      Hint = 'Paste from clipboard'
      OnClick = MMEditPasteClick
    end
    object MMP_PastePlain: TMenuItem
      Caption = 'Paste as &Text'
      Hint = 'Paste from clipboard as plain text'
      OnClick = MMEditPasteAsTextClick
    end
    object MMP_PasteAsWebClip: TMenuItem
      Caption = 'Paste as &Web Clip'
      OnClick = MMEditPasteAsWebClipClick
    end
    object MMP_PasteAsWebClipText: TMenuItem
      Caption = 'Paste as Web &Clip (Text)'
      Hint = 'Paste as Web Clip without formatting'
      OnClick = MMEditPasteAsWebClipTextClick
    end
    object MMP_PasteSpecial: TMenuItem
      Caption = 'Paste &Special...'
      Hint = 'Select special format to paste'
      OnClick = MMEditPasteSpecialClick
    end
    object N117: TMenuItem
      Caption = '-'
    end
    object MMP_PlainDefaultPaste: TMenuItem
      Caption = 'Paste &external as Plain text'
      OnClick = MMP_PlainDefaultPasteClick
    end
    object N107: TMenuItem
      Caption = '-'
    end
    object MMP_PasteAsNote: TMenuItem
      Caption = 'Paste &Into New Folder'
      Hint = 'Create a new folder and paste text from clipboard'
      OnClick = MMEditPasteAsNewFolderClick
    end
    object MMP_PasteAsNode: TMenuItem
      Caption = 'Paste Into New &Node'
      Hint = 'Create a new tree node and paste text from clipboard'
      OnClick = MMEditPasteAsNewNodeClick
    end
  end
  object Menu_Date: TPopupMenu
    HelpContext = 249
    OnPopup = Menu_DatePopup
    Left = 40
    Top = 295
    object md1: TMenuItem
      Tag = 1
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object N110: TMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object md2: TMenuItem
      Tag = 2
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md3: TMenuItem
      Tag = 3
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md4: TMenuItem
      Tag = 4
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object N111: TMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object md5: TMenuItem
      Tag = 5
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md6: TMenuItem
      Tag = 6
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md7: TMenuItem
      Tag = 7
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md8: TMenuItem
      Tag = 8
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md9: TMenuItem
      Tag = 9
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md10: TMenuItem
      Tag = 10
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md11: TMenuItem
      Tag = 11
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md12: TMenuItem
      Tag = 12
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md13: TMenuItem
      Tag = 13
      Break = mbBarBreak
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md14: TMenuItem
      Tag = 14
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md15: TMenuItem
      Tag = 15
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md16: TMenuItem
      Tag = 16
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md17: TMenuItem
      Tag = 17
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md18: TMenuItem
      Tag = 18
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md19: TMenuItem
      Tag = 19
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md20: TMenuItem
      Tag = 20
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md21: TMenuItem
      Tag = 21
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md22: TMenuItem
      Tag = 22
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md23: TMenuItem
      Tag = 23
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md24: TMenuItem
      Tag = 24
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md25: TMenuItem
      Tag = 25
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
  end
  object Menu_Time: TPopupMenu
    HelpContext = 249
    OnPopup = Menu_TimePopup
    Left = 74
    Top = 295
    object mt1: TMenuItem
      Tag = 1
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object N108: TMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object mt2: TMenuItem
      Tag = 2
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt3: TMenuItem
      Tag = 3
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object N109: TMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object mt4: TMenuItem
      Tag = 4
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt5: TMenuItem
      Tag = 5
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt6: TMenuItem
      Tag = 6
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt7: TMenuItem
      Tag = 7
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt8: TMenuItem
      Tag = 8
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt9: TMenuItem
      Tag = 9
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
  end
  object Menu_Symbols: TPopupMenu
    HelpContext = 309
    OnPopup = Menu_SymbolsPopup
    Left = 108
    Top = 295
    object ms1: TMenuItem
      Tag = 1
      OnClick = ms11Click
    end
    object ms2: TMenuItem
      Tag = 2
      OnClick = ms11Click
    end
    object ms3: TMenuItem
      Tag = 3
      OnClick = ms11Click
    end
    object ms4: TMenuItem
      Tag = 4
      OnClick = ms11Click
    end
    object ms5: TMenuItem
      Tag = 5
      OnClick = ms11Click
    end
    object ms6: TMenuItem
      Tag = 6
      OnClick = ms11Click
    end
    object ms7: TMenuItem
      Tag = 7
      OnClick = ms11Click
    end
    object ms8: TMenuItem
      Tag = 8
      OnClick = ms11Click
    end
    object ms9: TMenuItem
      Tag = 9
      OnClick = ms11Click
    end
    object ms10: TMenuItem
      Tag = 10
      OnClick = ms11Click
    end
  end
  object Img_System: TdfsSystemImageList
    AllocBy = 32
    ShareImages = True
    ImageSize = isSmall
    Left = 89
    Top = 176
  end
  object Menu_TV: TPopupMenu
    HelpContext = 284
    Images = IMG_TV
    ParentBiDiMode = False
    Left = 266
    Top = 130
    object TVAddParent: TMenuItem
      Action = actTVAddNode_Parent
    end
    object TVInsertNode: TMenuItem
      Action = actTVAddNode_Above
      ShortCut = 45
    end
    object TVAddChildNode: TMenuItem
      Action = actTVAddNode_Child
      ShortCut = 8205
    end
    object TVAddSibling: TMenuItem
      Action = actTVAddNode_Below
      ShortCut = 16397
    end
    object TVAddNode: TMenuItem
      Action = actTVAddNode_Last
      ShortCut = 13
    end
    object N44: TMenuItem
      Caption = '-'
    end
    object TVBoldNode: TMenuItem
      Action = actTVBoldNode
      ShortCut = 16450
    end
    object TVCheckNode: TMenuItem
      Action = actTVCheckNode
      ShortCut = 16576
    end
    object TVChildrenCheckboxes_: TMenuItem
      Caption = 'Children Checkboxes'
      HelpContext = -2
      object TVChildrenCheckbox: TMenuItem
        Action = actTVChildrenCheckbox
      end
      object N119: TMenuItem
        Caption = '-'
      end
      object TVHideCheckedChildren: TMenuItem
        Action = actTVHideCheckedChildren
      end
      object TVHideUncheckedChildren: TMenuItem
        Action = actTVHideUncheckedChildren
      end
      object TVShowNonFilteredChildren: TMenuItem
        Action = actTVShowNonFilteredChildren
      end
    end
    object TVNodeColor_: TMenuItem
      Caption = 'Co&lor / Reset'
      Hint = 
        'Choose color for selected nodes (Shift: also in children, recurs' +
        'ive)'
      object TVNodeTextColor: TMenuItem
        Action = actTVNodeTextColor
      end
      object TVNodeBGColor: TMenuItem
        Action = actTVNodeBGColor
      end
      object N103: TMenuItem
        Caption = '-'
      end
      object TVDefaultNodeFont: TMenuItem
        Action = actTVDefaultNodeFont
        ShortCut = 16452
      end
    end
    object TVSelectNodeImage: TMenuItem
      Action = actTVSelectNodeImage
    end
    object TVAlarmNode: TMenuItem
      Action = actTVAlarmNode
    end
    object TVFlaggedNode: TMenuItem
      Action = actTVFlaggedNode
    end
    object N80: TMenuItem
      Caption = '-'
    end
    object TVVirtualNode_: TMenuItem
      Caption = '&Virtual Node'
      HelpContext = 285
      object TVVirtualNode: TMenuItem
        Action = actTVVirtualNode
        ImageName = '6'
      end
      object TVRefreshVirtualNode: TMenuItem
        Action = actTVRefreshVirtualNode
        ShortCut = 16466
      end
      object N90: TMenuItem
        Caption = '-'
      end
      object TVUnlinkVirtualNode: TMenuItem
        Action = actTVUnlinkVirtualNode
      end
    end
    object TVLinkedNode_: TMenuItem
      Caption = 'Linked Node'
      object TVInsertLinkedNode: TMenuItem
        Action = actTVInsertLinkedNode
      end
      object TVNavigateNextLinkedNNode: TMenuItem
        Action = actTVNavigateNextLinkedNNode
      end
    end
    object N30: TMenuItem
      Caption = '-'
    end
    object TVMovenode_: TMenuItem
      Caption = '&Move Node[s]'
      Hint = 'Move selected nodes in tree'
      object TVMoveNodeUp: TMenuItem
        Action = actTVMoveNodeUp
        ShortCut = 8230
      end
      object TVMoveNodeDown: TMenuItem
        Action = actTVMoveNodeDown
        ShortCut = 8232
      end
      object TVMoveNodeLeft: TMenuItem
        Action = actTVMoveNodeLeft
        ShortCut = 8229
      end
      object TVMoveNodeRight: TMenuItem
        Action = actTVMoveNodeRight
        ShortCut = 8231
      end
    end
    object TVTransfer_: TMenuItem
      Caption = '&Transfer Subtree'
      Hint = 'Copy node and its children; then paste it in another tree'
      object TVCutSubtree: TMenuItem
        Action = actTVCutSubtree
        ShortCut = 16472
      end
      object TVCopySubtree: TMenuItem
        Action = actTVCopySubtree
        ShortCut = 16451
      end
      object TVGraftSubtree: TMenuItem
        Tag = 1
        Action = actTVGraftSubtree
        ShortCut = 16470
      end
      object TVGraftSubtreeLinked: TMenuItem
        Action = actTVGraftSubtreeLinked
        ShortCut = 24662
      end
      object N56: TMenuItem
        Caption = '-'
      end
      object TVEraseTreeMem: TMenuItem
        Tag = 2
        Action = actTVEraseTreeMem
      end
    end
    object TVExport: TMenuItem
      Action = actTVExport
    end
    object TVSortNodes_: TMenuItem
      Caption = '&Sort'
      HelpContext = 313
      Hint = 'Sort nodes'
      object TVSortSubtree: TMenuItem
        Action = actTVSortSubtree
        ShortCut = 24661
      end
      object TVSortTree: TMenuItem
        Action = actTVSortTree
        ShortCut = 24659
      end
    end
    object N31: TMenuItem
      Caption = '-'
    end
    object TVView_Filter: TMenuItem
      Caption = 'View / Filter'
      object TVViewAdditColumns: TMenuItem
        Action = actTVViewAdditColumns
      end
      object N37: TMenuItem
        Caption = '-'
      end
      object TVFilterUsePath: TMenuItem
        Caption = 'Use path of nodes'
        Hint = 'Search on whole path of nodes'
        OnClick = TVFilterUsePathClick
      end
      object TVFilterShowChildren: TMenuItem
        Caption = 'Show children'
        Hint = 'Filtering: show children of matching nodes'
        OnClick = TVFilterShowChildrenClick
      end
      object TVFilterOutUnflagged: TMenuItem
        Action = actTVFilterOutUnflagged
      end
    end
    object N40: TMenuItem
      Caption = '-'
    end
    object TVDeleteNode: TMenuItem
      Action = actTVDeleteNode
      ShortCut = 46
    end
    object TVDeleteChildren: TMenuItem
      Action = actTVDeleteChildren
    end
    object N32: TMenuItem
      Caption = '-'
    end
    object TVRenameNode: TMenuItem
      Action = actTVRenameNode
      ShortCut = 113
    end
    object TVCopyNode_: TMenuItem
      Caption = 'C&opy node inf.'
      object TVCopyNodeName: TMenuItem
        Action = actTVCopyNodeName
      end
      object TVCopyNodePath: TMenuItem
        Action = actTVCopyNodePath
        ShortCut = 16464
      end
      object TVCopyNodeText: TMenuItem
        Action = actTVCopyNodeText
        ShortCut = 16468
      end
      object TVCopyPathtoEditor: TMenuItem
        Action = actTVCopyPathtoEditor
        ShortCut = 16453
      end
    end
    object TVPasteNode_: TMenuItem
      Caption = '&Paste Name'
      object TVPasteNodeName: TMenuItem
        Action = actTVPasteNodeName
      end
      object N42: TMenuItem
        Caption = '-'
      end
      object TVPasteNodeNameAsDate: TMenuItem
        Action = actTVPasteNodeNameAsDate
        ShortCut = 32836
      end
      object TVPasteNodeNameAsTime: TMenuItem
        Action = actTVPasteNodeNameAsTime
        ShortCut = 32852
      end
      object TVPasteNodeNameAsDateTime: TMenuItem
        Action = actTVPasteNodeNameAsDateTime
        ShortCut = 41028
      end
      object N104: TMenuItem
        Caption = '-'
      end
      object TVPasteNodeNameAsSel: TMenuItem
        Action = actTVPasteNodeNameAsSel
      end
    end
  end
  object actList_File: TActionList
    Images = IMG_Toolbar
    OnUpdate = actList_FileUpdate
    Left = 328
    Top = 472
    object actFileSave: TAction
      Caption = '&Save'
      Hint = 'Save Keynote file'
      ImageIndex = 2
      OnExecute = actFileSaveExecute
    end
  end
  object Menu_Tags: TPopupMenu
    HelpContext = 307
    Left = 472
    Top = 344
    object TagsMCreate: TMenuItem
      Caption = '&Create new Tag'
      ShortCut = 45
      OnClick = TagsMCreateClick
    end
    object TagsMEdit: TMenuItem
      Caption = '&Edit Tag'
      Hint = 'Edit selected tag'
      ShortCut = 113
      OnClick = TagsMEditClick
    end
    object TagsMDel: TMenuItem
      Caption = '&Delete Tag[s]'
      Hint = 'Delete selected tag[s]'
      ShortCut = 46
      OnClick = TagsMDelClick
    end
    object MenuItem2: TMenuItem
      Caption = '-'
    end
    object TagsMAdd: TMenuItem
      Caption = '&Add Tags to selected Notes'
      OnClick = TagsMAddClick
    end
    object TagsMRemove: TMenuItem
      Caption = '&Remove Tags from selected Notes'
      OnClick = TagsMRemoveClick
    end
  end
end
