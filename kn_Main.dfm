object Form_Main: TForm_Main
  Left = 188
  Top = 178
  Caption = 'KeyNote'
  ClientHeight = 665
  ClientWidth = 924
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = Menu_Main
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnResize = TntFormResize
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter_Res: TSplitter
    Left = 683
    Top = 54
    Height = 566
    Hint = 'Click and drag to resize panels'
    Align = alRight
    Color = clBtnFace
    MinSize = 100
    ParentColor = False
    OnMoved = Splitter_ResMoved
    ExplicitLeft = 564
    ExplicitHeight = 374
  end
  object StatusBar: TdfsStatusBar
    Left = 0
    Top = 646
    Width = 924
    Height = 19
    Panels = <
      item
        Enabled = True
        Width = 6
        AutoFit = True
      end
      item
        PanelType = sptGlyph
        Enabled = True
        Width = 4
        AutoFit = True
      end
      item
        Enabled = True
        Width = 115
        AutoFit = False
      end
      item
        Enabled = True
        Width = 35
        AutoFit = False
      end
      item
        Enabled = True
        Width = 50
        AutoFit = False
      end
      item
        Enabled = True
        Width = 35
        AutoFit = False
      end
      item
        Enabled = True
        Width = 50
        AutoFit = False
      end>
    OnDblClick = StatusBarDblClick
  end
  object Dock_Top: TDock97
    Left = 0
    Top = 0
    Width = 924
    Height = 54
    BoundLines = [blTop, blBottom]
    FixAlign = True
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 2
        Images = IMG_Toolbar
        OnClick = MMFileSaveClick
      end
      object TB_FileNew: TToolbarButton97
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = MRUMenu
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 1
        Images = IMG_Toolbar
        OnClick = MMFileOpenClick
      end
      object TB_EditCut: TToolbarButton97
        Left = 148
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 6
        Images = IMG_Toolbar
        OnClick = MMEditUndoClick
      end
      object TB_EditCopy: TToolbarButton97
        Left = 171
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 4
        Images = IMG_Toolbar
        OnClick = MMEditCopyClick
      end
      object TB_EditPaste: TToolbarButton97
        Left = 194
        Top = 0
        Width = 37
        Height = 22
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Paste
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 5
        Images = IMG_Toolbar
        OnClick = MMEditPasteClick
      end
      object TB_NoteDelete: TToolbarButton97
        Left = 357
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 9
        Images = IMG_Toolbar
        OnClick = MMNoteRemoveClick
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 7
        Images = IMG_Toolbar
        OnClick = MMNoteNewClick
      end
      object TB_NoteEdit: TToolbarButton97
        Left = 334
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 8
        Images = IMG_Toolbar
        OnClick = MMNotePropertiesClick
      end
      object sm9: TToolbarSep97
        Left = 663
        Top = 0
      end
      object TB_Exit: TToolbarButton97
        Left = 759
        Top = 0
        Width = 22
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 11
        Images = IMG_Toolbar
        Margin = 1
        OnClick = TB_ExitClick
      end
      object TB_Options: TToolbarButton97
        Left = 669
        Top = 0
        Width = 22
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 10
        Images = IMG_Toolbar
        OnClick = MMToolsOptionsClick
      end
      object sm10: TToolbarSep97
        Left = 753
        Top = 0
      end
      object TB_EditRedo: TToolbarButton97
        Left = 260
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 13
        Images = IMG_Toolbar
        OnClick = MMEditRedoClick
      end
      object TB_FileMgr: TToolbarButton97
        Left = 118
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 15
        Images = IMG_Toolbar
        OnClick = MMFindNextClick
      end
      object TB_Find: TToolbarButton97
        Left = 387
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        AllowAllUp = True
        GroupIndex = 4
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_ResPanel
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 41
        Images = IMG_Toolbar
        OnClick = MMViewResPanelClick
      end
      object TB_Print: TToolbarButton97
        Left = 617
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 45
        Images = IMG_Toolbar
        Visible = False
        OnClick = MMViewOnTopClick
      end
      object TB_AlarmMode: TToolbarButton97
        Left = 729
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 3
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 51
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TB_AlarmModeClick
        OnMouseEnter = TB_AlarmModeMouseEnter
      end
      object Combo_Zoom: TTntComboBox
        Left = 562
        Top = 0
        Width = 55
        Height = 21
        Hint = 'Zoom text in editor'
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = Combo_ZoomDblClick
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
      HelpContext = 325
      Caption = 'Formatting Toolbar'
      DefaultDock = Dock_Top
      DockableTo = [dpTop, dpBottom]
      DockPos = 0
      DockRow = 1
      DragHandleStyle = dhSingle
      TabOrder = 1
      OnClose = Toolbar_FormatClose
      object sf1: TToolbarSep97
        Left = 146
        Top = 0
      end
      object sf2: TToolbarSep97
        Left = 203
        Top = 0
      end
      object TB_AlignLeft: TToolbarButton97
        Left = 353
        Top = 0
        Width = 24
        Height = 22
        GroupIndex = 2
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 4
        Images = IMG_Format
        OnClick = MMFormatAlignLeftClick
      end
      object TB_Bold: TToolbarButton97
        Left = 209
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 5
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 0
        Images = IMG_Format
        RepeatInterval = 101
        OnClick = MMFormatBoldClick
      end
      object TB_Italics: TToolbarButton97
        Left = 233
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 6
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 1
        Images = IMG_Format
        OnClick = MMFormatItalicsClick
      end
      object TB_Underline: TToolbarButton97
        Left = 256
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 7
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 2
        Images = IMG_Format
        OnClick = MMFormatUnderlineClick
      end
      object sf3: TToolbarSep97
        Left = 347
        Top = 0
      end
      object TB_Strikeout: TToolbarButton97
        Left = 278
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 8
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 3
        Images = IMG_Format
        OnClick = MMFormatStrikeoutClick
      end
      object TB_Bullets: TToolbarButton97
        Left = 452
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 7
        Images = IMG_Format
        OnClick = MMFormatBulletsClick
      end
      object TB_AlignCenter: TToolbarButton97
        Left = 377
        Top = 0
        Width = 23
        Height = 22
        GroupIndex = 2
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 5
        Images = IMG_Format
        OnClick = MMFormatAlignCenterClick
      end
      object sf4: TToolbarSep97
        Left = 446
        Top = 0
      end
      object TB_AlignRight: TToolbarButton97
        Left = 400
        Top = 0
        Width = 22
        Height = 22
        GroupIndex = 2
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 6
        Images = IMG_Format
        OnClick = MMFormatAlignRightClick
      end
      object TB_Outdent: TToolbarButton97
        Left = 617
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 9
        Images = IMG_Format
        OnClick = MMFormatLIndDecClick
      end
      object TB_Indent: TToolbarButton97
        Left = 640
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 8
        Images = IMG_Format
        OnClick = MMFormatLIndIncClick
      end
      object sf7: TToolbarSep97
        Left = 664
        Top = 0
      end
      object TB_FontDlg: TToolbarButton97
        Left = 670
        Top = 0
        Width = 22
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 11
        Images = IMG_Format
        OnClick = MMFormatFontClick
      end
      object TB_ParaDlg: TToolbarButton97
        Left = 692
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 13
        Images = IMG_Format
        OnClick = MMFormatParagraphClick
      end
      object sf8: TToolbarSep97
        Left = 716
        Top = 0
      end
      object TB_Numbers: TToolbarButton97
        Left = 474
        Top = 0
        Width = 38
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Numbers
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 14
        Images = IMG_Format
        OnClick = MMFormatNumbersClick
      end
      object sf6: TToolbarSep97
        Left = 611
        Top = 0
      end
      object TB_Space2: TToolbarButton97
        Left = 587
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 17
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS2Click
      end
      object TB_Space1: TToolbarButton97
        Left = 541
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 15
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS1Click
      end
      object TB_Space15: TToolbarButton97
        Left = 564
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 10
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 16
        Images = IMG_Format
        Visible = False
        OnClick = MMFormatLS15Click
      end
      object TB_WordWrap: TToolbarButton97
        Left = 512
        Top = 0
        Width = 23
        Height = 22
        AllowAllUp = True
        GroupIndex = 3
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 18
        Images = IMG_Format
        OnClick = MMFormatWordWrapClick
      end
      object sf5: TToolbarSep97
        Left = 535
        Top = 0
      end
      object TB_Subscript: TToolbarButton97
        Left = 325
        Top = 0
        Width = 22
        Height = 22
        AllowAllUp = True
        GroupIndex = 9
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 20
        Images = IMG_Format
        OnClick = MMFormatSubscriptClick
      end
      object TB_Superscript: TToolbarButton97
        Left = 301
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 9
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 19
        Images = IMG_Format
        OnClick = MMFormatSuperscriptClick
      end
      object TB_AlignJustify: TToolbarButton97
        Left = 422
        Top = 0
        Width = 24
        Height = 22
        GroupIndex = 2
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 21
        Images = IMG_Format
        OnClick = MMFormatAlignJustifyClick
      end
      object Combo_Font: TFontComboBox
        Left = 0
        Top = 1
        Width = 146
        Height = 20
        Hint = 'Select font face'
        TabOrder = 0
        TabStop = False
        OnKeyDown = Combo_FontKeyDown
      end
      object TB_Color: TColorBtn
        Left = 722
        Top = 0
        Width = 34
        Height = 22
        Hint = 'Click to change text color'
        ActiveColor = clBlack
        TargetColor = clBlack
        Flat = True
        DropDownFlat = True
        AutomaticColor = clWindowText
        IsAutomatic = True
        OnClick = TB_ColorClick
        Glyph.Data = {
          42040000424D4204000000000000420000002800000020000000100000000100
          1000030000000004000000000000000000000000000000000000007C0000E003
          00001F000000F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EFF7FFF7FFF7FFF7FF75EF75EF75EFF7FFF7FFF7FFF7FFF7F
          FF7FF75EF75E0F000F000F000F00F75EF75EF75E0F000F000F000F000F000F00
          F75EF75EF75EEF3DEF3DEF3DEF3DF75EF75EF75EEF3DEF3DEF3DEF3DEF3DEF3D
          F75EF75EF75EF75EEF3D0F00F75EF75EF75EF75EF75EEF3D0F000F00EF3DF75E
          F75EF75EF75EF75EF75EEF3DFF7FFF7FF75EF75EF75EF75EEF3DEF3DFF7FF75E
          F75EF75EF75EF75EF75E0F000F00F75EF75EF75EF75EEF3D0F000F00F75EF75E
          F75EF75EF75EF75EF75EEF3DEF3DFF7FF75EF75EF75EF75EEF3DEF3DF75EF75E
          F75EF75EF75EF75EF75EEF3D0F00F75EF75EF75EF75E0F000F00EF3DF75EF75E
          F75EF75EF75EF75EF75EF75EEF3DFF7FFF7FFF7FFF7FEF3DEF3DFF7FF75EF75E
          F75EF75EF75EF75EF75EF75E0F000F000F000F000F000F000F00F75EF75EF75E
          F75EF75EF75EF75EF75EF75EEF3DEF3DEF3DEF3DEF3DEF3DEF3DF75EF75EF75E
          F75EF75EF75EF75EF75EF75EEF3D0F00F75EF75E0F000F00EF3DF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EEF3DFF7FFF7FEF3DEF3DFF7FF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75E0F000F00F75E0F000F00F75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EEF3DEF3DFF7FEF3DEF3DF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EEF3D0F000F000F00EF3DF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3DEF3DEF3DFF7FF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75E0F000F000F00F75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3DEF3DEF3DF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3D0F00EF3DF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EEF3DFF7FF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75E0F00F75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EEF3DF75EF75EF75EF75EF75EF75E
          F75EF75EF75E}
        AutoBtnCaption = 'Default color'
        OtherBtnCaption = '&Other colors...'
        RegKey = 'General Frenetics\KeyNote\ColorBtn1'
        DDArrowWidth = 12
      end
      object TB_Hilite: TColorBtn
        Left = 756
        Top = 0
        Width = 34
        Height = 22
        Hint = 'Click to add or remove highlight'
        ActiveColor = clInfoBk
        TargetColor = clBlack
        Flat = True
        DropDownFlat = True
        AutomaticColor = clWindow
        IsAutomatic = True
        OnClick = TB_HiliteClick
        GlyphType = gtBackground
        Glyph.Data = {
          42040000424D4204000000000000420000002800000020000000100000000100
          1000030000000004000000000000000000000000000000000000007C0000E003
          00001F000000F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75E0000F75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75E0000EF3D0000F75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EEF3DF75EEF3DF75EFF7FF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75E0000EF3DEF3DEF3D0000F75EF75EF75E
          EF3DF75EF75EF75EF75EF75EF75EF75EEF3DF75EF75EF75EEF3DF75EFF7FF75E
          F75EFF7FF75EF75EF75EF75EF75E0000F75EF75EF75EEF3DEF3D0000F75EF75E
          0F00EF3DF75EF75EF75EF75EF75EEF3DF75EF75EF75EF75EF75EEF3DF75EFF7F
          EF3DFF7FFF7FF75EF75EF75E0000FF7FF75EF75EF75EF75EEF3DEF3D0000F75E
          0F000F00F75EF75EF75EF75EEF3DF75EF75EF75EF75EF75EF75EF75EEF3DF75E
          EF3DEF3DFF7FF75EF75E0000F75EFF7FFF7FF75EF75EF75EF75EEF3DEF3D0F00
          0F000F00F75EF75EF75EEF3DF75EFF7FF75EF75EF75EF75EFF7FF75EF75EEF3D
          EF3DEF3DFF7FF75EF75EF75E0000F75EFF7FFF7FF75E0000F75EF75EEF3D0F00
          0F000F00F75EF75EF75EF75EEF3DF75EFF7FF75EF75EEF3DFF7FFF7FF75EEF3D
          EF3DEF3DFF7FF75EF75EF75EF75E0000F75EFF7F00000F000000F75E0F000F00
          0F000F00F75EF75EF75EF75EF75EEF3DF75EFF7FEF3DEF3DEF3DF75EEF3DEF3D
          EF3DEF3DF75EF75EF75EF75EF75EF75E0000F75EFF7F0F00F75EEF3D0F000F00
          0F00F75EF75EF75EF75EF75EF75EF75EEF3DFF7FFF7FEF3DFF7FF75EEF3DEF3D
          EF3DF75EF75EF75EF75EF75EF75EF75E0F000000F75E0F00EF3D0000EF3DF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3DEF3DF75EEF3DFF7FEF3DF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75E0F00F75E00000F000000EF3DF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3DFF7FEF3DEF3DEF3DF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75E0F00F75EF75E0F00EF3DF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3DF75EFF7FEF3DF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EEF3D0F000F00EF3DF75EF75EF75EF75E
          F75EF75EF75EF75EF75EF75EF75EF75EF75EEF3DEF3DF75EF75EF75EF75EF75E
          F75EF75EF75E}
        AutoBtnCaption = 'No Highlight'
        OtherBtnCaption = '&Other colors...'
        RegKey = 'General Frenetics\KeyNote\ColorBtn2'
        DDArrowWidth = 12
      end
      object Combo_FontSize: TTntComboBox
        Left = 152
        Top = 0
        Width = 51
        Height = 21
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 3
        OnKeyDown = Combo_FontSizeKeyDown
        OnKeyPress = Combo_FontSizeKeyPress
        Items.Strings = (
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
    Width = 674
    Height = 566
    AllowTabShifting = True
    Align = alClient
    FlatSeperators = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
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
    OnDragDrop = PagesDragDrop
    OnDragOver = PagesDragOver
    OnMouseDown = PagesMouseDown
    OnTabShift = PagesTabShift
  end
  object Dock_Left: TDock97
    Left = 0
    Top = 54
    Width = 9
    Height = 566
    LimitToOneRow = True
    Position = dpLeft
  end
  object Dock_Bottom: TDock97
    Left = 0
    Top = 620
    Width = 924
    Height = 26
    Position = dpBottom
    object Toolbar_Tree: TToolbar97
      Tag = 4
      Left = 247
      Top = 0
      HelpContext = 335
      ActivateParent = False
      Caption = 'Tree Toolbar'
      DefaultDock = Dock_Top
      DockPos = 247
      DragHandleStyle = dhSingle
      TabOrder = 0
      OnClose = Toolbar_FormatClose
      object TB_NodeDelete: TToolbarButton97
        Left = 144
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 23
        Images = IMG_Toolbar
        OnClick = TVDeleteNodeClick
      end
      object TB_NodeFirst: TToolbarButton97
        Left = 52
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 19
        Images = IMG_Toolbar
        OnClick = TVInsertNodeClick
      end
      object TB_NodeLast: TToolbarButton97
        Left = 75
        Top = 0
        Width = 24
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 20
        Images = IMG_Toolbar
        OnClick = TVAddNodeClick
      end
      object TB_NodeChild: TToolbarButton97
        Left = 99
        Top = 0
        Width = 22
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 21
        Images = IMG_Toolbar
        OnClick = TVAddChildNodeClick
      end
      object TB_NodeRename: TToolbarButton97
        Left = 121
        Top = 0
        Width = 23
        Height = 22
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 22
        Images = IMG_Toolbar
        OnClick = MMRenamenodeClick
      end
      object Sep9722: TToolbarSep97
        Left = 46
        Top = 0
      end
      object TB_GoForward: TToolbarButton97
        Left = 23
        Top = 0
        Width = 23
        Height = 22
        Enabled = False
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 39
        Images = IMG_Toolbar
        OnClick = MMTreeGoForwardClick
      end
      object TB_GoBack: TToolbarButton97
        Left = 0
        Top = 0
        Width = 23
        Height = 22
        Enabled = False
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 38
        Images = IMG_Toolbar
        OnClick = MMTreeGoBackClick
      end
      object TB_HideChecked: TToolbarButton97
        Left = 192
        Top = 0
        Width = 24
        Height = 22
        AllowAllUp = True
        GroupIndex = 1
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 37
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TVHideCheckedClick
      end
      object TB_FilterTree: TToolbarButton97
        Left = 216
        Top = 0
        Width = 24
        Height = 22
        Hint = 'Apply or Remove Filter on tree note'
        AllowAllUp = True
        GroupIndex = 2
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 49
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TB_FilterTreeClick
      end
      object TB_AlarmNode: TToolbarButton97
        Left = 168
        Top = 0
        Width = 24
        Height = 22
        Hint = 'Set alarm on node...'
        AllowAllUp = True
        GroupIndex = 3
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 50
        Images = IMG_Toolbar
        RepeatInterval = 101
        OnClick = TB_AlarmNodeClick
        OnMouseEnter = TB_AlarmNodeMouseEnter
      end
    end
    object Toolbar_Style: TToolbar97
      Tag = 3
      Left = 5
      Top = 0
      ActivateParent = False
      Caption = 'Style Toolbar'
      DefaultDock = Dock_Bottom
      DockPos = 5
      DragHandleStyle = dhSingle
      TabOrder = 1
      OnClose = Toolbar_FormatClose
      OnRecreated = Toolbar_StyleRecreated
      OnRecreating = Toolbar_StyleRecreating
      object TB_Style: TToolbarButton97
        Left = 167
        Top = 0
        Width = 38
        Height = 22
        Hint = 'Apply style to selection (click arrow for menu commands)'
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Style
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 25
        Images = IMG_Toolbar
        OnClick = BtnStyleApplyClick
      end
      object ToolbarSep9717: TToolbarSep97
        Left = 161
        Top = 0
      end
      object Combo_Style: TTntComboBox
        Left = 0
        Top = 0
        Width = 161
        Height = 22
        HelpContext = 515
        Style = csOwnerDrawFixed
        DropDownCount = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 16
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
      Left = 556
      Top = 0
      ActivateParent = False
      Caption = 'Insert Toolbar'
      DefaultDock = Dock_Bottom
      DockPos = 556
      DragHandleStyle = dhSingle
      TabOrder = 2
      OnClose = Toolbar_FormatClose
      object ToolbarButton971: TToolbarButton97
        Left = 74
        Top = 0
        Width = 37
        Height = 22
        Hint = 'Insert symbol (click arrow for menu)'
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Symbols
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Date
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
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
        DropdownAlways = True
        DropdownArrowWidth = 12
        DropdownCombo = True
        DropdownMenu = Menu_Time
        Glyph.Data = {00000000}
        GlyphMask.Data = {00000000}
        ImageIndex = 47
        Images = IMG_Toolbar
        OnClick = MMInsertTimeClick
      end
    end
  end
  object Pages_Res: TPage95Control
    Left = 686
    Top = 54
    Width = 238
    Height = 566
    Hint = 'Right-click for resource panel options'
    ActivePage = ResTab_Find
    Align = alRight
    FlatSeperators = False
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'MS Sans Serif'
    TabInactiveFont.Style = []
    MultiLine = True
    PopupMenu = Menu_ResPanel
    RemoveLastTab = True
    TabOrder = 2
    OnChange = Pages_ResChange
    object ResTab_Find: TTab95Sheet
      Caption = 'Find'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Panel_ResFind: TPanel
        Left = 0
        Top = 0
        Width = 230
        Height = 81
        Align = alTop
        BevelOuter = bvLowered
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label1: TTntLabel
          Left = 5
          Top = 5
          Width = 47
          Height = 13
          Caption = '&Find text:'
          FocusControl = Combo_ResFind
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Combo_ResFind: TTntComboBox
          Left = 5
          Top = 20
          Width = 215
          Height = 22
          Hint = 'Type text to search for'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 14
          MaxLength = 255
          ParentFont = False
          PopupMenu = Menu_StdEdit
          TabOrder = 0
          OnChange = Combo_ResFindChange
          OnKeyDown = Combo_ResFindKeyDown
        end
        object Btn_ResFind: TTntButton
          Left = 5
          Top = 47
          Width = 92
          Height = 25
          Hint = 'Search for text and display all matches'
          Caption = 'Find All'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = Btn_ResFindClick
        end
        object Btn_ResFlip: TTntButton
          Left = 102
          Top = 47
          Width = 92
          Height = 25
          Hint = 'Toggle options and search results display'
          Caption = 'Results'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = Btn_ResFlipClick
        end
      end
      object Ntbk_ResFind: TNotebook
        Left = 0
        Top = 81
        Width = 230
        Height = 439
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object PAGE_RES_FIND: TPage
          Left = 0
          Top = 0
          Caption = 'PAGE_RES_FIND'
          object List_ResFind: TTextListBox
            Left = 0
            Top = 0
            Width = 230
            Height = 439
            Hint = 'Double-click to display the matching text; right-click for menu'
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Tahoma'
            Font.Style = []
            ItemHeight = 14
            ParentFont = False
            PopupMenu = Menu_FindAll
            Style = lbOwnerDrawFixed
            TabOrder = 0
            OnDblClick = List_ResFindDblClick
            OnDrawItem = List_ResFindDrawItem
            OnKeyDown = List_ResFindKeyDown
          end
        end
        object PAGE_RES_FIND_OPT: TPage
          Left = 0
          Top = 0
          Caption = 'PAGE_RES_FIND_OPT'
          ExplicitWidth = 0
          ExplicitHeight = 0
          object CB_ResFind_CaseSens: TTntCheckBox
            Left = 10
            Top = 15
            Width = 213
            Height = 17
            Hint = 'Distinguish between lowercase and uppercase letters'
            Caption = 'Match case'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object CB_ResFind_WholeWords: TTntCheckBox
            Left = 10
            Top = 34
            Width = 210
            Height = 17
            Hint = 'Find only complete words'
            Caption = 'Whole words only'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
          object CB_ResFind_AllNotes: TTntCheckBox
            Left = 10
            Top = 62
            Width = 213
            Height = 17
            Hint = 'Search through all notes in current file'
            Caption = 'Search all notes'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 2
          end
          object CB_ResFind_NodeNames: TTntCheckBox
            Left = 10
            Top = 101
            Width = 213
            Height = 17
            Hint = 'Include tree node names in search'
            Caption = 'Search node names'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
          end
          object RG_ResFind_Type: TTntRadioGroup
            Left = 5
            Top = 161
            Width = 215
            Height = 76
            Hint = 'Select type of search to perform'
            Caption = ' Search type '
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 4
            OnClick = RG_ResFind_TypeClick
          end
          object CB_ResFind_HiddenNodes: TTntCheckBox
            Left = 10
            Top = 81
            Width = 210
            Height = 17
            Hint = 'Considerar hidden nodes'
            Caption = 'Search hidden nodes'
            Checked = True
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            State = cbChecked
            TabOrder = 5
          end
          object CB_ResFind_Filter: TTntCheckBox
            Left = 10
            Top = 129
            Width = 213
            Height = 17
            Hint = 'Show or hide nodes according to search conditions'
            Caption = 'Filter Tree note'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 6
          end
        end
      end
    end
    object ResTab_RTF: TTab95Sheet
      Caption = 'Scratch'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Res_RTF: TRxRichEdit
        Left = 0
        Top = 0
        Width = 230
        Height = 520
        Hint = 'Right-click for menu'
        Align = alClient
        AllowInPlace = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        HideSelection = False
        Language = 1034
        ParentFont = False
        PopupMenu = Menu_StdEdit
        TabOrder = 0
        UndoLimit = 10
        WantTabs = True
        OnKeyDown = Res_RTFKeyDown
        OnProtectChange = RxRTFProtectChange
        OnProtectChangeEx = RxRTFProtectChangeEx
        OnURLClick = RxRTFURLClick
      end
    end
    object ResTab_Macro: TTab95Sheet
      Caption = 'Macros'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Dock_ResMacro: TDock97
        Left = 0
        Top = 0
        Width = 230
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
            Glyph.Data = {00000000}
            GlyphMask.Data = {00000000}
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
            Glyph.Data = {00000000}
            GlyphMask.Data = {00000000}
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
            Glyph.Data = {00000000}
            GlyphMask.Data = {00000000}
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
        Width = 230
        Height = 494
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
      Caption = 'Templates'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object ListBox_ResTpl: TGFXListBox
        Left = 0
        Top = 0
        Width = 230
        Height = 520
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
      Caption = 'Plugins'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object Splitter_plugins: TSplitter
        Left = 0
        Top = 464
        Width = 230
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
        Width = 230
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
            Glyph.Data = {00000000}
            GlyphMask.Data = {00000000}
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
            Glyph.Data = {00000000}
            GlyphMask.Data = {00000000}
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
        Width = 230
        Height = 438
        Hint = 'Double-click to run plugin; right-click for menu'
        ImageList = IMG_Toolbar
        CheckBoxes = False
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
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
        Top = 467
        Width = 230
        Height = 53
        Align = alBottom
        BevelOuter = bvLowered
        BorderWidth = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        ExplicitTop = 427
        object LB_PluginInfo: TTntLabel
          Left = 3
          Top = 3
          Width = 9
          Height = 13
          Align = alClient
          Caption = '...'
          WordWrap = True
        end
      end
    end
    object ResTab_Favorites: TTab95Sheet
      Caption = 'Favorites'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object ListBox_ResFav: TGFXListBox
        Left = 0
        Top = 0
        Width = 230
        Height = 520
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
  end
  object Menu_Main: TTntMainMenu
    Left = 300
    Top = 80
    object MMFile_: TTntMenuItem
      Caption = '&File'
      Hint = 'File management commands'
      object MMFileNew: TTntMenuItem
        Caption = '&New'
        Hint = 'Create a new Keynote file'
        ShortCut = 24654
        OnClick = MMFileNewClick
      end
      object MMFileOpen: TTntMenuItem
        Caption = '&Open...'
        Hint = 'Open a Keynote file'
        ShortCut = 16463
        OnClick = MMFileOpenClick
      end
      object MMFileSave: TTntMenuItem
        Caption = '&Save'
        Hint = 'Save Keynote file'
        ShortCut = 16467
        OnClick = MMFileSaveClick
      end
      object MMFileSaveAs: TTntMenuItem
        Caption = 'Save &As...'
        Hint = 'Save Keynote file with a new name'
        OnClick = MMFileSaveAsClick
      end
      object MMFileClose: TTntMenuItem
        Caption = 'Clos&e'
        Hint = 'Close currently open file'
        ShortCut = 16465
        OnClick = MMFileCloseClick
      end
      object MM_MRUSeparator_: TTntMenuItem
        Caption = '-'
      end
      object MMFileAutoSave: TTntMenuItem
        Caption = 'A&uto Save'
        Hint = 'Toggle automatic saving'
        OnClick = MMFileAutoSaveClick
      end
      object N3: TTntMenuItem
        Caption = '-'
      end
      object MMFileProperties: TTntMenuItem
        Caption = '&Properties'
        Hint = 'Edit Keynote file properties'
        ShortCut = 32781
        OnClick = MMFilePropertiesClick
      end
      object MMFileManager: TTntMenuItem
        Caption = 'File &Manager'
        Hint = 'Open file manager'
        ShortCut = 123
        OnClick = MMFileManagerClick
      end
      object N28: TTntMenuItem
        Caption = '-'
      end
      object MMToolsImport: TTntMenuItem
        Caption = '&Import...'
        Hint = 'Import files as notes'
        OnClick = MMToolsImportClick
      end
      object MMToolsExport: TTntMenuItem
        Caption = '&Export...'
        Hint = 'Export text to another format'
        OnClick = MMToolsExportExClick
      end
      object N105: TTntMenuItem
        Caption = '-'
      end
      object MMFilePageSetup: TTntMenuItem
        Caption = 'Pa&ge Setup...'
        Hint = 'View or change page setup'
        OnClick = MMFilePageSetupClick
      end
      object MMFileCopyTo: TTntMenuItem
        Caption = '&Copy To...'
        Hint = 'Copy current file to another directory'
        OnClick = MMFileCopyToClick
      end
      object N5: TTntMenuItem
        Caption = '-'
      end
      object MMFileExit: TTntMenuItem
        Caption = 'E&xit'
        Hint = 'Close KeyNote'
        OnClick = MMFileExitClick
      end
    end
    object MMEdit_: TTntMenuItem
      Caption = '&Edit'
      Hint = 'Text editing commands'
      object MMEditUndo: TTntMenuItem
        Caption = '&Undo'
        Hint = 'Undo last change'
        ShortCut = 32776
        OnClick = MMEditUndoClick
      end
      object MMEditRedo: TTntMenuItem
        Caption = '&Redo'
        Hint = 'Redo the last Undone command'
        ShortCut = 16397
        OnClick = MMEditRedoClick
      end
      object MMEditRepeat: TTntMenuItem
        Caption = 'R&epeat Last'
        Hint = 'Repeat last editing command'
        ShortCut = 16575
        OnClick = MMEditRepeatClick
      end
      object N9: TTntMenuItem
        Caption = '-'
      end
      object MMEditCut: TTntMenuItem
        Caption = 'Cu&t'
        Hint = 'Cut text to clipboard (Ctrl+X, Shift+Supr)'
        OnClick = MMEditCutClick
      end
      object MMEditCopy: TTntMenuItem
        Caption = '&Copy'
        Hint = 'Copy text to clipboard (Ctrl+C, Ctrl+Ins)'
        OnClick = MMEditCopyClick
      end
      object MMEditPaste: TTntMenuItem
        Caption = '&Paste'
        Hint = 'Paste text from clipboard (Ctrl+P, Shift+Ins)'
        OnClick = MMEditPasteClick
      end
      object MMEditPasteOther_: TTntMenuItem
        Caption = 'Paste &Other'
        Hint = 'Paste in other formats'
        object MMEditPasteAsText: TTntMenuItem
          Caption = 'Paste as &Text'
          Hint = 'Paste text from clipboard without formatting'
          ShortCut = 24621
          OnClick = MMEditPasteAsTextClick
        end
        object MMEditPasteAsWebClip: TTntMenuItem
          Caption = 'Paste as &Web Clip'
          ShortCut = 24663
          OnClick = MMEditPasteAsWebClipClick
        end
        object MMEditPasteSpecial: TTntMenuItem
          Caption = 'Paste &Special...'
          Hint = 'Select special format to paste'
          OnClick = MMEditPasteSpecialClick
        end
        object N67: TTntMenuItem
          Caption = '-'
        end
        object MMEditPasteAsNewNote: TTntMenuItem
          Caption = 'Paste &Into New Note'
          Hint = 'Create a new note and paste text from clipboard'
          OnClick = MMEditPasteAsNewNoteClick
        end
        object MMEditPasteAsNewNode: TTntMenuItem
          Caption = 'Paste Into New &Node'
          Hint = 'Create a new tree node and paste text from clipboard'
          OnClick = MMEditPasteAsNewNodeClick
        end
      end
      object MMEditDelete: TTntMenuItem
        Caption = '&Delete'
        Hint = 'Remove selected text'
        OnClick = MMEditDeleteClick
      end
      object N15: TTntMenuItem
        Caption = '-'
      end
      object MMEditLines_: TTntMenuItem
        Caption = '&Lines'
        Hint = 'Line operations'
        object MMEditDelLine: TTntMenuItem
          Caption = '&Delete Line'
          Hint = 'Deletes line at cursor'
          ShortCut = 16473
          OnClick = MMEditDelLineClick
        end
        object N58: TTntMenuItem
          Caption = '-'
        end
        object MMEditSort: TTntMenuItem
          Caption = '&Sort Lines'
          Hint = 'Sort selected lines in active note'
          ShortCut = 24659
          OnClick = MMSortClick
        end
        object MMEditJoin: TTntMenuItem
          Caption = '&Join Lines'
          Hint = 'Join selected lines (reformat pagagraph)'
          ShortCut = 16458
          OnClick = MMEditJoinClick
        end
      end
      object MMEditCase_: TTntMenuItem
        Caption = 'C&hange Case'
        Hint = 'Adjust lettercase'
        object MMEditUpper: TTntMenuItem
          Caption = 'To &UPPERCASE'
          Hint = 'Sets selected text to uppercase'
          ShortCut = 24661
          OnClick = MMEditUpperClick
        end
        object MMEditLower: TTntMenuItem
          Caption = 'To &lowercase'
          Hint = 'Sets selected text to lowercase'
          ShortCut = 24652
          OnClick = MMEditLowerClick
        end
        object MMEditMixed: TTntMenuItem
          Caption = 'To &Mixed Case'
          Hint = 'Sets selected text to mixed (Title) case'
          ShortCut = 24653
          OnClick = MMEditMixedClick
        end
        object N59: TTntMenuItem
          Caption = '-'
        end
        object MMEditInvertCase: TTntMenuItem
          Caption = '&Invert Case'
          Hint = 'Invert case of selected text'
          ShortCut = 24649
          OnClick = MMEditInvertCaseClick
        end
        object MMEditCycleCase: TTntMenuItem
          Caption = '&Cycle Case'
          Hint = 'Cyclically changes case (lower -> Mixed -> UPPER)'
          ShortCut = 8306
          OnClick = MMEditCycleCaseClick
        end
      end
      object MMEditTrim_: TTntMenuItem
        Caption = '&White Space'
        Hint = 'Trim white space'
        object MMEditTrimLeft: TTntMenuItem
          Tag = 1
          Caption = 'Trim &Left'
          Hint = 'Left-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object MMEditTrimRight: TTntMenuItem
          Tag = 2
          Caption = 'Trim &Right'
          Hint = 'Right-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object MMEditTrimBoth: TTntMenuItem
          Tag = 3
          Caption = 'Trim &Both'
          Hint = 'Left- and right-trim white space'
          OnClick = MMEditTrimLeftClick
        end
        object N60: TTntMenuItem
          Caption = '-'
        end
        object MMEditCompress: TTntMenuItem
          Caption = '&Compress White Space'
          Hint = 'Compress white space'
          OnClick = MMEditCompressClick
        end
      end
      object N26: TTntMenuItem
        Caption = '-'
      end
      object MMEditTransform_: TTntMenuItem
        Caption = 'Transfor&m'
        Hint = 'Transform text'
        object MMEditRot13: TTntMenuItem
          Caption = 'Apply &Rot-13'
          Hint = 'Apply ROT-13 coding to selected text'
          ShortCut = 24627
          OnClick = MMEditRot13Click
        end
        object MMEditReverse: TTntMenuItem
          Caption = 'Re&verse Text'
          Hint = 'Order selected text backwards'
          ShortCut = 24628
          OnClick = MMEditReverseClick
        end
        object N112: TTntMenuItem
          Caption = '-'
        end
        object MMEditDecimalToRoman: TTntMenuItem
          Caption = '&Decimal to Roman'
          Hint = 'Convert decimal numeral to Roman'
          OnClick = MMEditDecimalToRomanClick
        end
        object MMEditRomanToDecimal: TTntMenuItem
          Caption = 'R&oman to Decimal'
          Hint = 'Convert Roman numeral to decimal'
          OnClick = MMEditRomanToDecimalClick
        end
      end
      object MMEditEval_: TTntMenuItem
        Caption = 'E&xpression'
        Hint = 'Expression evaluation'
        object MMEditEvaluate: TTntMenuItem
          Caption = '&Evaluate'
          Hint = 'Evaluate selected text as mathematical expression'
          ShortCut = 16571
          OnClick = MMEditEvaluateClick
        end
        object MMEditPasteEval: TTntMenuItem
          Caption = '&Paste Last Result'
          Hint = 'Paste result of last expression evaluated'
          ShortCut = 32813
          OnClick = MMEditPasteEvalClick
        end
      end
      object N10: TTntMenuItem
        Caption = '-'
      end
      object MMEditSelectWord: TTntMenuItem
        Caption = 'Select Word'
        Hint = 'Select current word'
        ShortCut = 49239
        OnClick = MMEditSelectWordClick
      end
      object MMEditCopyAll: TTntMenuItem
        Caption = 'Cop&y All'
        Hint = 'Select and copy all text in active note'
        OnClick = MMEditCopyAllClick
      end
      object MMEditSelectAll: TTntMenuItem
        Caption = 'Select &All'
        Hint = 'Select all text in note'
        ShortCut = 16449
        OnClick = MMEditSelectAllClick
      end
    end
    object MMView_: TTntMenuItem
      Caption = '&View'
      Hint = 'Show or hide interface elements'
      object MMViewOnTop: TTntMenuItem
        Caption = '&Always on Top'
        Hint = 'Keep program window on top of other windows'
        ShortCut = 119
        OnClick = MMViewOnTopClick
      end
      object N22: TTntMenuItem
        Caption = '-'
      end
      object MMViewResPanel: TTntMenuItem
        Caption = '&Resource Panel'
        Hint = 'Show or hide resource panel'
        ShortCut = 120
        OnClick = MMViewResPanelClick
      end
      object MMViewToolbars_: TTntMenuItem
        Caption = '&Toolbars'
        Hint = 'Show or hide toolbars'
        object MMViewTBMain: TTntMenuItem
          Caption = 'Show &Main Toolbar'
          Hint = 'Display or hide main toolbar'
          OnClick = MMViewTBMainClick
        end
        object MMViewTBFormat: TTntMenuItem
          Caption = 'Show &Format Toolbar'
          Hint = 'Display or hide the Formatting toolbar'
          OnClick = MMViewTBFormatClick
        end
        object MMViewTBStyle: TTntMenuItem
          Caption = 'Show &Style Toolbar'
          Hint = 'Display or hide the Style toolbar'
          OnClick = MMViewTBStyleClick
        end
        object MMViewTBTree: TTntMenuItem
          Caption = 'Show &Tree Toolbar'
          Hint = 'Display or hide the Tree toolbar'
          OnClick = MMViewTBTreeClick
        end
        object MMViewTBInsert: TTntMenuItem
          Caption = 'Show &Insert Toolbar'
          Hint = 'Display or hide the Insert toolbar'
          OnClick = MMViewTBInsertClick
        end
        object N76: TTntMenuItem
          Caption = '-'
        end
        object MMViewTBAll: TTntMenuItem
          Tag = 1
          Caption = 'S&how All Toolbars'
          Hint = 'Display all toolbars'
          OnClick = MMViewTBHideAllClick
        end
        object MMViewTBHideAll: TTntMenuItem
          Caption = '&Hide All Toolbars'
          Hint = 'Hide all toolbars'
          OnClick = MMViewTBHideAllClick
        end
        object N101: TTntMenuItem
          Caption = '-'
        end
        object MMViewTBRefresh: TTntMenuItem
          Caption = '&Refresh Configuration'
          Hint = 'Refresh toolbar button configuration from "toolbar.ini" file'
          OnClick = MMViewTBRefreshClick
        end
        object MMViewTBSaveConfig: TTntMenuItem
          Caption = 'S&ave Configuration'
          Hint = 'Save toolbar layout to "toolbar.ini" file'
          OnClick = MMViewTBSaveConfigClick
        end
      end
      object MMViewStatusBar: TTntMenuItem
        Caption = 'Stat&us Bar'
        Hint = 'Show or hide the status bar'
        OnClick = MMViewStatusBarClick
      end
      object N4: TTntMenuItem
        Caption = '-'
      end
      object MMViewTabIcons: TTntMenuItem
        Caption = 'Tab &Icons'
        Hint = 'Show or hide icons on note tabs'
        OnClick = MMViewTabIconsClick
      end
      object MMViewTree: TTntMenuItem
        Caption = 'Tree &Panel'
        Hint = 'Show or hide tree panel'
        ShortCut = 8314
        OnClick = MMViewTreeClick
      end
      object N102: TTntMenuItem
        Caption = '-'
      end
      object MMViewNodeIcons: TTntMenuItem
        Tag = 1
        Caption = 'Sta&ndard Tree Icons'
        Hint = 'Show or hide standard tree icons'
        OnClick = MMViewNodeIconsClick
      end
      object MMViewCustomIcons: TTntMenuItem
        Tag = 2
        Caption = 'C&ustom Tree Icons'
        Hint = 'Show or hide custom tree icons'
        OnClick = MMViewNodeIconsClick
      end
      object MMViewCheckboxesAllNodes: TTntMenuItem
        Caption = 'All nodes &Checkboxes'
        Hint = 'Show or hide tree checkboxes in all nodes'
        OnClick = MMViewCheckboxesAllNodesClick
      end
      object MMViewHideCheckedNodes: TTntMenuItem
        Caption = '&Hide Checked Nodes'
        Hint = 'Show or Hide checked nodes'
        OnClick = TVHideCheckedClick
      end
      object MMViewFilterTree: TTntMenuItem
        Caption = 'Filter nodes'
        Hint = 'Apply or Remove Filter on tree note'
        OnClick = MMViewFilterTreeClick
      end
      object N106: TTntMenuItem
        Caption = '-'
      end
      object MMViewZoomIn: TTntMenuItem
        Caption = '&Zoom In'
        Hint = 'Increase zoom'
        OnClick = MMViewZoomInClick
      end
      object MMViewZoomOut: TTntMenuItem
        Caption = 'Zoom &Out'
        Hint = 'Decrease zoom'
        OnClick = MMViewZoomOutClick
      end
      object N54: TTntMenuItem
        Caption = '-'
      end
      object MMFormatView_: TTntMenuItem
        Caption = 'Show &Formatting'
        Hint = 'Show or hide formatting information in Status bar'
        object MMViewFormatFont: TTntMenuItem
          Caption = 'Show &Font Formatting'
          GroupIndex = 1
          Hint = 'Display font formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatPara: TTntMenuItem
          Caption = 'Show &Paragraph Formatting'
          GroupIndex = 1
          Hint = 'Display paragraph formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatBoth: TTntMenuItem
          Caption = 'Show &All Formatting'
          GroupIndex = 1
          Hint = 'Display font and paragraph formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
        object N55: TTntMenuItem
          Caption = '-'
          GroupIndex = 1
          OnClick = MMViewFormatNoneClick
        end
        object MMViewFormatNone: TTntMenuItem
          Caption = '&Hide Formatting'
          GroupIndex = 1
          Hint = 'Display no formatting'
          RadioItem = True
          OnClick = MMViewFormatNoneClick
        end
      end
      object N29: TTntMenuItem
        Caption = '-'
      end
      object MMViewAlphaTabs: TTntMenuItem
        Caption = 'Alpha&betize Tabs'
        Hint = 'Sort notes alphabetically by name'
        OnClick = MMViewAlphaTabsClick
      end
      object MMShiftTab_: TTntMenuItem
        Caption = '&Shift Active Tab'
        Hint = 'Move tab left or right'
        object MMViewShiftTabLeft: TTntMenuItem
          Caption = 'Shift Tab &Left'
          Hint = 'Move active tab left'
          OnClick = MMViewShiftTabLeftClick
        end
        object MMViewShiftTabRight: TTntMenuItem
          Caption = 'Shift Tab &Right'
          Hint = 'Move active tab right'
          OnClick = MMViewShiftTabRightClick
        end
      end
    end
    object MMInsert_: TTntMenuItem
      Caption = '&Insert'
      Hint = 'Insert commands'
      object MMInsertDate: TTntMenuItem
        Caption = 'Insert &Date'
        Hint = 'Insert date into the note'
        ShortCut = 24644
        OnClick = MMInsertDateClick
      end
      object MMInsertTime: TTntMenuItem
        Caption = 'Insert &Time'
        Hint = 'Insert time into the note'
        ShortCut = 24660
        OnClick = MMInsertTimeClick
      end
      object N62: TTntMenuItem
        Caption = '-'
      end
      object MMInsertCharacter: TTntMenuItem
        Caption = '&Character...'
        Hint = 'Insert special characters'
        ShortCut = 24643
        OnClick = MMInsertCharacterClick
      end
      object MMInsertFileContents: TTntMenuItem
        Caption = '&File Contents...'
        Hint = 'Insert contents of a text file'
        OnClick = MMInsertFileContentsClick
      end
      object N65: TTntMenuItem
        Caption = '-'
      end
      object MMInsertURL: TTntMenuItem
        Caption = '&URL...'
        Hint = 'Insert an Internet URL'
        OnClick = MMInsertURLClick
      end
      object MMInsertLinkToFile: TTntMenuItem
        Caption = '&Link to File...'
        Hint = 'Insert a hyperlink to a local file'
        OnClick = MMInsertLinkToFileClick
      end
      object N84: TTntMenuItem
        Caption = '-'
      end
      object MMInsertMarkLocation: TTntMenuItem
        Caption = '&Mark KeyNote Location'
        Hint = 'Mark place to which the link will jump'
        ShortCut = 16501
        OnClick = MMInsertMarkLocationClick
      end
      object MMInsertKNTLink: TTntMenuItem
        Caption = '&Insert KeyNote Link'
        Hint = 'Insert link to the previously marked location'
        ShortCut = 8309
        OnClick = MMInsertKNTLinkClick
      end
      object N68: TTntMenuItem
        Caption = '-'
      end
      object MMInsertPicture: TTntMenuItem
        Caption = '&Picture...'
        Hint = 'Insert an image'
        OnClick = MMInsertPictureClick
      end
      object MMInsertObject: TTntMenuItem
        Caption = '&Object...'
        Hint = 'Inserts an OLE object'
        OnClick = MMInsertObjectClick
      end
      object N57: TTntMenuItem
        Caption = '-'
      end
      object MMInsertTerm: TTntMenuItem
        Caption = '&Expand Term'
        Hint = 'Replace current term with its Glossary definition'
        ShortCut = 118
        OnClick = MMInsertTermClick
      end
      object MMInsertWordWeb: TTntMenuItem
        Caption = '&WordWeb...'
        Hint = 'Look up word in WordWeb thesaurus'
        ShortCut = 16506
        OnClick = RTFMWordWebClick
      end
    end
    object MMFormat_: TTntMenuItem
      Caption = 'F&ormat'
      Hint = 'Text formatting commands'
      object MMFormatFont: TTntMenuItem
        Caption = '&Font...'
        Hint = 'View or change font settings'
        ShortCut = 16468
        OnClick = MMFormatFontClick
      end
      object MMFormatParagraph: TTntMenuItem
        Caption = '&Paragraph...'
        Hint = 'View or change paragraph settings'
        ShortCut = 16464
        OnClick = MMFormatParagraphClick
      end
      object MMFormatLanguage: TTntMenuItem
        Caption = 'Lan&guage...'
        Hint = 'Specify language for selected text'
        OnClick = MMFormatLanguageClick
      end
      object N69: TTntMenuItem
        Caption = '-'
      end
      object MMFontStyle_: TTntMenuItem
        Caption = 'F&ont Style'
        Hint = 'Font style attributes'
        object MMFormatBold: TTntMenuItem
          Caption = '&Bold'
          Hint = 'Apply bold attribute'
          ShortCut = 16450
          OnClick = MMFormatBoldClick
        end
        object MMFormatItalics: TTntMenuItem
          Caption = '&Italics'
          Hint = 'Apply italic attribute'
          ShortCut = 16457
          OnClick = MMFormatItalicsClick
        end
        object MMFormatUnderline: TTntMenuItem
          Caption = '&Underline'
          Hint = 'Apply underline attribute'
          ShortCut = 16469
          OnClick = MMFormatUnderlineClick
        end
        object MMFormatStrikeout: TTntMenuItem
          Caption = '&Strikeout'
          Hint = 'Apply strikeout attribute'
          ShortCut = 16459
          OnClick = MMFormatStrikeoutClick
        end
        object N47: TTntMenuItem
          Caption = '-'
        end
        object MMFormatDisabled: TTntMenuItem
          Caption = '&Disabled'
          Hint = 'Apply disabled attribute'
          ShortCut = 16439
          OnClick = MMFormatDisabledClick
        end
        object MMFormatSuperscript: TTntMenuItem
          Caption = 'Superscr&ipt'
          Hint = 'Apply superscript attribute'
          ShortCut = 16573
          OnClick = MMFormatSuperscriptClick
        end
        object MMFormatSubscript: TTntMenuItem
          Caption = 'Subsc&ript'
          Hint = 'Apply subscript attribute'
          ShortCut = 24765
          OnClick = MMFormatSubscriptClick
        end
        object N13: TTntMenuItem
          Caption = '-'
        end
        object MMFormatClearFontAttr: TTntMenuItem
          Caption = '&Clear Font Attributes'
          Hint = 'Restore default font style attributes'
          ShortCut = 16432
          OnClick = MMFormatClearFontAttrClick
        end
      end
      object MMFontsize_: TTntMenuItem
        Caption = 'Font &Size'
        Hint = 'Grow/Shrink font'
        object MMFormatFontSizeInc: TTntMenuItem
          Caption = '&Increase Font Size'
          Hint = 'Increase font size'
          ShortCut = 16605
          OnClick = MMFormatFontSizeIncClick
        end
        object MMFormatFontSizeDec: TTntMenuItem
          Caption = '&Decrease Font Size'
          Hint = 'Shrink font size'
          ShortCut = 16603
          OnClick = MMFormatFontSizeDecClick
        end
      end
      object N49: TTntMenuItem
        Caption = '-'
      end
      object MMAlignment_: TTntMenuItem
        Caption = '&Alignment'
        Hint = 'Text alignment commands'
        object MMFormatAlignLeft: TTntMenuItem
          Caption = 'Align &Left'
          GroupIndex = 1
          HelpContext = 5
          Hint = 'Align lines to the left'
          RadioItem = True
          ShortCut = 49189
          OnClick = MMFormatAlignLeftClick
        end
        object MMFormatAlignCenter: TTntMenuItem
          Caption = 'Align &Center'
          GroupIndex = 1
          HelpContext = 10
          Hint = 'Center lines'
          RadioItem = True
          ShortCut = 49190
          OnClick = MMFormatAlignCenterClick
        end
        object MMFormatAlignRight: TTntMenuItem
          Caption = 'Align &Right'
          GroupIndex = 1
          HelpContext = 15
          Hint = 'Align lines to the right'
          RadioItem = True
          ShortCut = 49191
          OnClick = MMFormatAlignRightClick
        end
        object MMFormatAlignJustify: TTntMenuItem
          Caption = '&Justify'
          GroupIndex = 1
          Hint = 'Align lines with full justification'
          ShortCut = 49192
          OnClick = MMFormatAlignJustifyClick
        end
      end
      object MMLineSpacing_: TTntMenuItem
        Caption = '&Line Spacing'
        Hint = 'Line spacing commands'
        object MMFormatLS1: TTntMenuItem
          Caption = 'Line Spacing &Single'
          GroupIndex = 1
          Hint = 'Select single line spacing'
          RadioItem = True
          ShortCut = 16433
          OnClick = MMFormatLS1Click
        end
        object MMFormatLS15: TTntMenuItem
          Caption = '&Line Spacing 1.5'
          GroupIndex = 1
          Hint = 'Select one and a half line spacing'
          RadioItem = True
          ShortCut = 16437
          OnClick = MMFormatLS15Click
        end
        object MMFormatLS2: TTntMenuItem
          Caption = 'Line Spacing &Double'
          GroupIndex = 1
          Hint = 'Select double line spacing'
          RadioItem = True
          ShortCut = 16434
          OnClick = MMFormatLS2Click
        end
      end
      object MMParastyle_: TTntMenuItem
        Caption = 'Paragraph St&yle'
        Hint = 'Paragraph style attributes'
        object MMFormatBullets: TTntMenuItem
          Caption = '&Bullets'
          Hint = 'Apply bullet list style'
          ShortCut = 16453
          OnClick = MMFormatBulletsClick
        end
        object MMFormatNumbers: TTntMenuItem
          Caption = '&Numbers'
          Hint = 'Apply numbered list style'
          ShortCut = 24645
          OnClick = MMFormatNumbersClick
        end
        object N18: TTntMenuItem
          Caption = '-'
        end
        object MMFormatFIndInc: TTntMenuItem
          Caption = '&Increase First Indent'
          Hint = 'Increase indentation of first line'
          ShortCut = 16570
          OnClick = MMFormatFIndIncClick
        end
        object MMFormatFindDec: TTntMenuItem
          Caption = '&Decrease First Indent'
          Hint = 'Decrease indentation of first line'
          ShortCut = 24762
          OnClick = MMFormatFindDecClick
        end
        object N96: TTntMenuItem
          Caption = '-'
        end
        object MMFormatLIndInc: TTntMenuItem
          Caption = 'Increase &Left Inden'
          Hint = 'Increase left indentation'
          ShortCut = 32954
          OnClick = MMFormatLIndIncClick
        end
        object MMFormatLIndDec: TTntMenuItem
          Caption = 'Decrease L&eft Indent'
          Hint = 'Decrease left indentation'
          ShortCut = 41146
          OnClick = MMFormatLIndDecClick
        end
        object N97: TTntMenuItem
          Caption = '-'
        end
        object MMFormatRIndInc: TTntMenuItem
          Caption = 'Increase &Right Indent'
          Hint = 'Increase right indent'
          ShortCut = 16606
          OnClick = MMFormatRIndIncClick
        end
        object MMFormatRIndDec: TTntMenuItem
          Caption = 'Decrease Ri&ght Indent'
          Hint = 'Decrease right indent'
          ShortCut = 24798
          OnClick = MMFormatRIndDecClick
        end
        object N48: TTntMenuItem
          Caption = '-'
        end
        object MMFormatSpBefInc: TTntMenuItem
          Caption = 'Increase S&pace Before'
          Hint = 'Increase space before paragraph'
          ShortCut = 16572
          OnClick = MMFormatSpBefIncClick
        end
        object MMFormatSpBefDec: TTntMenuItem
          Caption = 'Decrease Space Be&fore'
          Hint = 'Decrease space before paragraph'
          ShortCut = 24764
          OnClick = MMFormatSpBefDecClick
        end
        object N98: TTntMenuItem
          Caption = '-'
        end
        object MMFormatSpAftInc: TTntMenuItem
          Caption = 'Increase Space &After'
          Hint = 'Increase space after paragraph'
          ShortCut = 16574
          OnClick = MMFormatSpAftIncClick
        end
        object MMFormatSpAftDec: TTntMenuItem
          Caption = 'Decrease Space Af&ter'
          Hint = 'Decrease space after paragraph'
          ShortCut = 24766
          OnClick = MMFormatSpAftDecClick
        end
        object N45: TTntMenuItem
          Caption = '-'
        end
        object MMFormatClearParaAttr: TTntMenuItem
          Caption = '&Clear Paragraph Attributes'
          Hint = 'Restore default paragraph style attributes'
          ShortCut = 24624
          OnClick = MMFormatClearParaAttrClick
        end
      end
      object N14: TTntMenuItem
        Caption = '-'
      end
      object MMFormatTextColor: TTntMenuItem
        Caption = '&Text Color'
        ShortCut = 16466
        OnClick = MMFormatTextColorClick
      end
      object MMFormatBGColor: TTntMenuItem
        Caption = '&Background Color'
        Hint = 'Change background color'
        ShortCut = 16452
        OnClick = MMFormatBGColorClick
      end
      object N12: TTntMenuItem
        Caption = '-'
      end
      object MMFormatHighlight: TTntMenuItem
        Caption = 'Apply &Highlight'
        ShortCut = 16456
        OnClick = MMFormatHighlightClick
      end
      object MMFormatNoHighlight: TTntMenuItem
        Caption = '&No Highlight'
        Hint = 'Remove text highlighting'
        ShortCut = 24648
        OnClick = MMFormatNoHighlightClick
      end
      object N99: TTntMenuItem
        Caption = '-'
      end
      object MMFormatApplyStyle: TTntMenuItem
        Caption = 'Appl&y Selected Style'
        Hint = 'Apply current style to selection'
        ShortCut = 8307
        OnClick = BtnStyleApplyClick
      end
      object N53: TTntMenuItem
        Caption = '-'
      end
      object MMCopyFormat_: TTntMenuItem
        Caption = '&Copy Format'
        Hint = 'Copy format for pastring'
        object MMFormatCopyFont: TTntMenuItem
          Caption = 'Copy &Font Attributes'
          Hint = 'Lift font attributes'
          ShortCut = 16503
          OnClick = MMFormatCopyFontClick
        end
        object MMFormatCopyPara: TTntMenuItem
          Caption = 'Copy &Paragraph Attributes'
          Hint = 'Lift paragraph attributes'
          ShortCut = 16504
          OnClick = MMFormatCopyParaClick
        end
      end
      object MMPasteFormat_: TTntMenuItem
        Caption = 'Paste For&mat'
        Hint = 'Paste previously copied format'
        object MMFormatPasteFont: TTntMenuItem
          Caption = 'Paste &Font Attributes'
          Hint = 'Apply copied font attributes'
          ShortCut = 8311
          OnClick = MMFormatPasteFontClick
        end
        object MMFormatPastePara: TTntMenuItem
          Caption = 'Paste &Paragraph Attributes'
          Hint = 'Apply copied paragraph attributes'
          ShortCut = 8312
          OnClick = MMFormatPasteParaClick
        end
      end
      object N25: TTntMenuItem
        Caption = '-'
      end
      object MMFormatWordWrap: TTntMenuItem
        Caption = '&Word Wrap'
        Hint = 'Toggle wrapping at end of lines'
        ShortCut = 16471
        OnClick = MMFormatWordWrapClick
      end
    end
    object MMNote_: TTntMenuItem
      Caption = '&Note'
      Hint = 'Note-related commands'
      object MMNoteNew: TTntMenuItem
        Caption = '&New Note...'
        Hint = 'Add a new note'
        ShortCut = 16462
        OnClick = MMNoteNewClick
      end
      object MMSetAlarm: TTntMenuItem
        Caption = 'Set &alarm...'
        Hint = 
          'Adds or edit an alarm directly to the note (CTR+click forces to ' +
          'add)'
        OnClick = TAM_SetAlarmClick
      end
      object MMNoteRename: TTntMenuItem
        Caption = '&Rename Note...'
        Hint = 'Rename current note'
        ShortCut = 113
        OnClick = MMNoteRenameClick
      end
      object MMNoteProperties: TTntMenuItem
        Caption = 'Note &Properties...'
        Hint = 'Edit current note properties'
        ShortCut = 115
        OnClick = MMNotePropertiesClick
      end
      object N2: TTntMenuItem
        Caption = '-'
      end
      object MMNoteRemove: TTntMenuItem
        Caption = 'R&emove Note'
        Hint = 'Delete current note'
        OnClick = MMNoteRemoveClick
      end
      object N27: TTntMenuItem
        Caption = '-'
      end
      object MMNotePrintPreview_: TTntMenuItem
        Caption = 'Print Pre&view'
        Hint = 'View how the note will be printed'
        OnClick = MMNotePrintPreview_Click
      end
      object MMNotePrint: TTntMenuItem
        Caption = 'Pr&int Note...'
        Hint = 'Print text of current note'
        ShortCut = 24656
        OnClick = MMNotePrintClick
      end
      object MMNoteEmail: TTntMenuItem
        Caption = '&Email Note...'
        Hint = 'Send current note via e-mail'
        ShortCut = 57413
        OnClick = MMNoteEmailClick
      end
      object N23: TTntMenuItem
        Caption = '-'
      end
      object MMNoteClipCapture: TTntMenuItem
        Caption = '&Clipboard Capture'
        Hint = 'Toggle clipboard capture for active note'
        ShortCut = 122
        OnClick = MMNoteClipCaptureClick
      end
      object N7: TTntMenuItem
        Caption = '-'
      end
      object MMNoteSpell: TTntMenuItem
        Caption = 'C&heck Spelling'
        Hint = 'Check spelling in active note'
        OnClick = MMNoteSpellClick
      end
      object MMNoteReadOnly: TTntMenuItem
        Caption = 'Read &Only'
        Hint = 'Make current note read-only'
        ShortCut = 24658
        OnClick = MMNoteReadOnlyClick
      end
    end
    object MMTree_: TTntMenuItem
      Caption = 'T&ree'
      Hint = 'Tree-related commands'
      object MMAddTreeNode_: TTntMenuItem
        Caption = '&Add Tree Node'
        object MMTreeInsert_: TTntMenuItem
          Caption = '&Insert Node'
          Hint = 'Insert new tree node'
          OnClick = TVInsertNodeClick
        end
        object MMTreeAdd_: TTntMenuItem
          Caption = '&Add Node'
          Hint = 'Add new tree node'
          OnClick = TVAddNodeClick
        end
        object MMTreeAddChild_: TTntMenuItem
          Caption = 'Add &Child'
          Hint = 'Add new tree node as child'
          OnClick = TVAddChildNodeClick
        end
        object MMTreeAddSibling_: TTntMenuItem
          Caption = 'Add &Sibling'
          Hint = 'Add node immediately after current node'
          OnClick = TVAddSiblingClick
        end
      end
      object MMMovenode_: TTntMenuItem
        Caption = '&Move Node'
        Hint = 'Move selected node in tree'
        object MMTreeMoveNodeUp_: TTntMenuItem
          Caption = 'Move Node &Up'
          Hint = 'Shift node up'
          OnClick = TVMoveNodeUpClick
        end
        object MMTreeMoveNodeDown_: TTntMenuItem
          Caption = 'Move Node &Down'
          Hint = 'Shift node down'
          OnClick = TVMoveNodeDownClick
        end
        object MMTreeMoveNodeLeft_: TTntMenuItem
          Caption = 'Move Node &Left'
          Hint = 'Shift node left by 1 level'
          OnClick = TVMoveNodeLeftClick
        end
        object MMTreeMoveNodeRight_: TTntMenuItem
          Caption = 'Move Node &Right'
          Hint = 'Shift node right by 1 level'
          OnClick = TVMoveNodeRightClick
        end
      end
      object N87: TTntMenuItem
        Caption = '-'
      end
      object MMTreeGoBack: TTntMenuItem
        Caption = '&Go Back'
        Hint = 'Navigate backwards in history'
        ShortCut = 32805
        OnClick = MMTreeGoBackClick
      end
      object MMTreeGoForward: TTntMenuItem
        Caption = 'Go &Forward'
        Hint = 'Navigate forward in history'
        ShortCut = 32807
        OnClick = MMTreeGoForwardClick
      end
      object MMTreeNav_: TTntMenuItem
        Caption = '&Navigate'
        object MMTreeNavUp: TTntMenuItem
          Caption = 'Go &Up'
          Hint = 'Move to previous node'
          ShortCut = 32806
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavDown: TTntMenuItem
          Caption = 'Go &Down'
          Hint = 'Move to next node'
          ShortCut = 32808
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavLeft: TTntMenuItem
          Caption = 'Go &Left'
          Hint = 'Move left in tree'
          ShortCut = 40997
          OnClick = MMTreeNavRightClick
        end
        object MMTreeNavRight: TTntMenuItem
          Caption = 'Go &Right'
          Hint = 'Move right in tree'
          ShortCut = 40999
          OnClick = MMTreeNavRightClick
        end
      end
      object N93: TTntMenuItem
        Caption = '-'
      end
      object MMTreeMasterNode: TTntMenuItem
        Caption = 'Create Ma&ster Node'
        Hint = 'Create a node as parent of all existing nodes'
        OnClick = MMTreeMasterNodeClick
      end
      object MMTreeNodeFromSel: TTntMenuItem
        Caption = 'Ne&w Node from Selection'
        Hint = 'Create a new node using selected text'
        ShortCut = 32890
        OnClick = MMTreeNodeFromSelClick
      end
      object N41: TTntMenuItem
        Caption = '-'
      end
      object MMTreeNodeDelete_: TTntMenuItem
        Caption = '&Delete Node'
        Hint = 'Delete selected node'
        OnClick = TVDeleteNodeClick
      end
      object MMTreeDeleteSubtree_: TTntMenuItem
        Caption = 'Delete C&hild Nodes'
        Hint = 'Delete children of selected node'
        OnClick = TVDeleteChildrenClick
      end
      object N34: TTntMenuItem
        Caption = '-'
      end
      object MMTreeNodeRename_: TTntMenuItem
        Caption = '&Rename Node'
        Hint = 'Rename node'
        OnClick = MMRenamenodeClick
      end
      object MMNodePaste_: TTntMenuItem
        Caption = '&Paste Node Name'
        Hint = 'Rename node from clipboard'
        object MMTreeNodeNamePaste: TTntMenuItem
          Caption = 'From &Clipboard'
          Hint = 'Use clipboard contents as node name'
          OnClick = TVPasteNodeNameClick
        end
        object N61: TTntMenuItem
          Caption = '-'
        end
        object MMTreeNodeNameAsDate: TTntMenuItem
          Caption = 'As &Date'
          Hint = 'Use current date as node name_'
          OnClick = TVPasteNodeNameClick
        end
        object MMTreeNodeNameAsTime: TTntMenuItem
          Caption = 'As &Time'
          Hint = 'Use current time as node name'
          OnClick = TVPasteNodeNameClick
        end
        object MMTreeNodeNameAsDateTime: TTntMenuItem
          Caption = '&As Date and Time'
          Hint = 'Use current date and time as node name'
          OnClick = TVPasteNodeNameClick
        end
        object N82: TTntMenuItem
          Caption = '-'
        end
        object MMTreeNodeNameAsSel: TTntMenuItem
          Caption = 'From &Selected Text'
          Hint = 'Rename node using selected text'
          OnClick = TVPasteNodeNameClick
        end
      end
      object N36: TTntMenuItem
        Caption = '-'
      end
      object MMTreeSaveToFile: TTntMenuItem
        Caption = 'Sa&ve Tree to File...'
        Hint = 'Save tree structure to file'
        OnClick = MMTreeSaveToFileClick
      end
      object MMTreeSort_: TTntMenuItem
        Caption = 'S&ort'
        Hint = 'Sort nodes'
        object MMTreeSortSubtree_: TTntMenuItem
          Caption = '&Sort Subtree'
          Hint = 'Sort child nodes (subtree)'
          OnClick = TVSortSubtreeClick
        end
        object MMTreeSortFull_: TTntMenuItem
          Caption = 'Sort Full &Tree'
          Hint = 'Sort all nodes in tree'
          OnClick = TVSortTreeClick
        end
      end
      object N92: TTntMenuItem
        Caption = '-'
      end
      object MMTreeOutlineNum: TTntMenuItem
        Caption = 'O&utline Numbering...'
        Hint = 'Add sequential numbers to node names'
        OnClick = MMTreeOutlineNumClick
      end
      object N37: TTntMenuItem
        Caption = '-'
      end
      object MMTreeFullExpand: TTntMenuItem
        Caption = '&Expand Tree'
        Hint = 'Fully expand tree'
        OnClick = MMTreeFullExpandClick
      end
      object MMTreeFullCollapse: TTntMenuItem
        Caption = 'Co&llapse Tree'
        Hint = 'Fully collapse tree'
        OnClick = MMTreeFullCollapseClick
      end
    end
    object MMSearch_: TTntMenuItem
      Caption = '&Search'
      Hint = 'Search commands'
      object MMFind: TTntMenuItem
        Caption = '&Find...'
        Hint = 'Search for text in notes'
        ShortCut = 16454
        OnClick = MMFindClick
      end
      object MMFindNext: TTntMenuItem
        Caption = 'Find &Next'
        Hint = 'Repeat last search command'
        ShortCut = 114
        OnClick = MMFindNextClick
      end
      object N74: TTntMenuItem
        Caption = '-'
      end
      object MMFindReplace: TTntMenuItem
        Caption = '&Replace...'
        Hint = 'Replace text in notes'
        ShortCut = 16498
        OnClick = MMFindReplaceClick
      end
      object MMFindReplaceNext: TTntMenuItem
        Caption = 'Re&place Next'
        Hint = 'Repeat last replace command'
        OnClick = MMFindReplaceNextClick
      end
      object N11: TTntMenuItem
        Caption = '-'
      end
      object MMBkmSet_: TTntMenuItem
        Caption = '&Set Bookmark'
        Hint = 'Mark bookmarks in note'
        object MMBkmSet0: TTntMenuItem
          Caption = '0'
          Hint = 'Set bookmark '
          ShortCut = 41008
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet1: TTntMenuItem
          Caption = '1'
          Hint = 'Set bookmark '
          ShortCut = 41009
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet2: TTntMenuItem
          Caption = '2'
          Hint = 'Set bookmark '
          ShortCut = 41010
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet3: TTntMenuItem
          Caption = '3'
          Hint = 'Set bookmark '
          ShortCut = 41011
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet4: TTntMenuItem
          Caption = '4'
          Hint = 'Set bookmark '
          ShortCut = 41012
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet5: TTntMenuItem
          Caption = '5'
          Hint = 'Set bookmark '
          ShortCut = 41013
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet6: TTntMenuItem
          Caption = '6'
          Hint = 'Set bookmark '
          ShortCut = 41014
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet7: TTntMenuItem
          Caption = '7'
          Hint = 'Set bookmark '
          ShortCut = 41015
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet8: TTntMenuItem
          Caption = '8'
          Hint = 'Set bookmark '
          ShortCut = 41016
          OnClick = MMBkmSet9Click
        end
        object MMBkmSet9: TTntMenuItem
          Caption = '9'
          Hint = 'Set bookmark '
          ShortCut = 41017
          OnClick = MMBkmSet9Click
        end
      end
      object MMBkmJump_: TTntMenuItem
        Caption = '&Jump to Bookmark'
        Hint = 'Jump to previously marked bookmarks'
        object MMBkmJ0: TTntMenuItem
          Caption = '0'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32816
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ1: TTntMenuItem
          Caption = '1'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32817
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ2: TTntMenuItem
          Caption = '2'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32818
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ3: TTntMenuItem
          Caption = '3'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32819
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ4: TTntMenuItem
          Caption = '4'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32820
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ5: TTntMenuItem
          Caption = '5'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32821
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ6: TTntMenuItem
          Caption = '6'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32822
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ7: TTntMenuItem
          Caption = '7'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32823
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ8: TTntMenuItem
          Caption = '8'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32824
          OnClick = MMBkmJ9Click
        end
        object MMBkmJ9: TTntMenuItem
          Caption = '9'
          Enabled = False
          Hint = 'Jump to bookmark'
          ShortCut = 32825
          OnClick = MMBkmJ9Click
        end
      end
      object N46: TTntMenuItem
        Caption = '-'
      end
      object MMFindBracket: TTntMenuItem
        Caption = '&Match Bracket'
        Hint = 'Find matching bracket'
        ShortCut = 16461
        OnClick = MMFindBracketClick
      end
      object MMFindGoTo: TTntMenuItem
        Caption = '&Go To Line...'
        Hint = 'Move to a specific line in note'
        ShortCut = 16455
        OnClick = MMFindGoToClick
      end
      object N39: TTntMenuItem
        Caption = '-'
      end
      object MMFindNode: TTntMenuItem
        Caption = 'Find N&ode...'
        Hint = 'Find tree node'
        ShortCut = 24646
        OnClick = MMFindNodeClick
      end
      object MMFindNodeNext: TTntMenuItem
        Caption = 'Find N&ext Node'
        Hint = 'Repeat last Find node command'
        ShortCut = 8306
        OnClick = MMFindNodeNextClick
      end
    end
    object MMTools_: TTntMenuItem
      Caption = '&Tools'
      Hint = 'Configuration and miscellaneous tools'
      object MMToolsOptions: TTntMenuItem
        Caption = '&Configuration Options...'
        Hint = 'Adjust program options'
        ShortCut = 116
        OnClick = MMToolsOptionsClick
      end
      object MMToolsDefaults: TTntMenuItem
        Caption = '&Default Settings...'
        Hint = 'Set defaults for all newly created files'
        ShortCut = 117
        OnClick = MMToolsDefaultsClick
      end
      object MMToolsCustomKBD: TTntMenuItem
        Caption = 'Customize Keyboard...'
        Hint = 'Assign custom keyboard shortcuts to menu items'
        OnClick = MMToolsCustomKBDClick
      end
      object N6: TTntMenuItem
        Caption = '-'
      end
      object MMToolsMacroRun: TTntMenuItem
        Caption = '&Run Macro'
        Hint = 'Execute selected macro'
        ShortCut = 8308
        OnClick = TB_MacroClick
      end
      object MMToolsMacroRunLast: TTntMenuItem
        Caption = 'Run Last Macro'
        Hint = 'Execute most recent macro'
        ShortCut = 16500
        OnClick = MMToolsMacroRunLastClick
      end
      object MMToolsMacroSelect: TTntMenuItem
        Caption = '&Select Macro...'
        Hint = 'Select and run macro'
        OnClick = MMToolsMacroSelectClick
      end
      object N77: TTntMenuItem
        Caption = '-'
      end
      object MMToolsPluginRun: TTntMenuItem
        Caption = 'Run &Plugin'
        Hint = 'Execute selected plugin'
        ShortCut = 8315
        OnClick = MMToolsPluginRunClick
      end
      object MMToolsPluginRunLast: TTntMenuItem
        Caption = 'Run &Last Plugin'
        Hint = 'Execute most recent plugin used'
        ShortCut = 16507
        OnClick = MMToolsPluginRunLastClick
      end
      object N75: TTntMenuItem
        Caption = '-'
      end
      object MMToolsGlosAddTerm: TTntMenuItem
        Caption = '&Add Glossary Term'
        Hint = 'Create a new glossary term'
        ShortCut = 8310
        OnClick = MMToolsGlosAddTermClick
      end
      object MMToolsGlosEdit: TTntMenuItem
        Caption = 'Edit &Glossary...'
        Hint = 'Edit glossary terms'
        OnClick = MMToolsGlosEditClick
      end
      object N73: TTntMenuItem
        Caption = '-'
      end
      object MMToolsMerge: TTntMenuItem
        Caption = '&Merge Notes...'
        Hint = 'Add notes from a file on disk'
        OnClick = MMToolsMergeClick
      end
      object MMTemplates_: TTntMenuItem
        Caption = 'Te&mplates'
        Hint = 'Template commands'
        object MMToolsTemplateCreate: TTntMenuItem
          Caption = '&Create Template...'
          Hint = 'Create new template based on selected text'
          OnClick = MMToolsTemplateCreateClick
        end
        object MMToolsTemplateInsert: TTntMenuItem
          Caption = '&Insert Template...'
          Hint = 'Insert template into current note'
          OnClick = MMToolsTemplateInsertClick
        end
      end
      object N35: TTntMenuItem
        Caption = '-'
      end
      object MMToolsUAS: TTntMenuItem
        Caption = 'UAS Integration'
        Hint = 'Enable or disable UltimaShell Autocompletion Server integration'
        OnClick = MMToolsUASClick
      end
      object MMToolsUASConfig: TTntMenuItem
        Caption = 'Configure UAS'
        Hint = 'Configure UltimaShell Autocompletion Server'
        OnClick = MMToolsUASConfigClick
      end
      object N63: TTntMenuItem
        Caption = '-'
      end
      object MMToolsStatistics: TTntMenuItem
        Caption = '&Text Statistics'
        Hint = 'Display note statistics'
        OnClick = MMToolsStatisticsClick
      end
      object MMToolsURL: TTntMenuItem
        Caption = 'Activate URL'
        Hint = 'Activate URL hyperlink under cursor'
        OnClick = MMToolsURLClick
      end
    end
    object MMHelp_: TTntMenuItem
      Caption = '&Help'
      Hint = 'Information about the program'
      object MMHelpTip: TTntMenuItem
        Caption = '&Tip of the Day'
        Hint = 'Display the Tip of the Day dialog'
        OnClick = MMHelpTipClick
      end
      object N1: TTntMenuItem
        Caption = '-'
      end
      object MMHelpContents: TTntMenuItem
        Caption = '&Help Contents'
        Hint = 'Display contents of online help'
        OnClick = MMHelpContentsClick
      end
      object MMHelpMain: TTntMenuItem
        Caption = '&General Help'
        Hint = 'Display general help topics'
        OnClick = MMHelpMainClick
      end
      object MMHelpKeyboardRef: TTntMenuItem
        Caption = '&Keyboard Reference'
        Hint = 'Display keyboard reference'
        OnClick = MMHelpKeyboardRefClick
      end
      object N79: TTntMenuItem
        Caption = '-'
      end
      object MMHelpWhatsNew: TTntMenuItem
        Caption = '&What'#39's New'
        Hint = 'Display KeyNote revision history'
        OnClick = MMHelpWhatsNewClick
      end
      object N100: TTntMenuItem
        Caption = '-'
      end
      object MMHelpVisitWebsite: TTntMenuItem
        Caption = '&Visit KeyNote NF website'
        Hint = 
          'Go to KeyNote NF website, for information, to submit bug report,' +
          ' feature request, etc.'
        OnClick = MMHelpVisitWebsiteClick
      end
      object MMHelpEmailAuthor: TTntMenuItem
        Caption = '&Email to Author'
        Hint = 'Start mail client and send email to keyNote author'
        Visible = False
      end
      object N72: TTntMenuItem
        Caption = '-'
      end
      object MMHelpAbout: TTntMenuItem
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
    StoredProps.Strings = (
      'Pages_Res.Width'
      'Pages_Res.ActivePage'
      'Res_RTF.WordWrap')
    StoredValues = <>
    Left = 230
    Top = 75
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
    Left = 95
    Top = 225
  end
  object Timer: TTimer
    Interval = 60000
    OnTimer = TimerTimer
    Left = 230
    Top = 120
  end
  object WinOnTop: TTopMostWindow
    AlwaysOnTop = False
    Left = 40
    Top = 180
  end
  object TrayIcon: TRxTrayIcon
    Active = False
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000000020000000000000000000000000000000000000000
      0000000080000080000000808000800000008000800080800000C0C0C0008080
      80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
      0008888888888888888888888800000000087777777777777777777777000000
      0888888888888888888888888780000008777777777777777777777778800000
      888888888888888888888888788000008FFFFFFFFFFFFFFFFFFFFFF877800000
      08FFFFFFFFFFFFFFFFFFFFFF8780000008FFFFFFFFFFFFFFFFFFFFFF87800000
      08FFFF7777777F777777777F87800000008FFF7FFFF7FF777FF777FFF8800000
      008FFF7FFFF7FF777FF777FFF8800000008FFF7777777F777FF777FFF8800000
      008FFFFFFFF7FF77777777FFF8800000008888888FF7FF777FF777FFF8800000
      0000000087777F777FF777FFF8800880000000008FF7FFFFFFF777FFF8808008
      800777778FF7FFFF7FFFFFFFF88000000770000087777777777777FFF8800000
      000000008FF7FFFF7FFFF7FFF8800000000000008FF7FFFF7FFFF7FFF8800008
      8000000087777777777777FFF8800880077000008FF7FFFF7FFFF7FFF8800000
      000777778FF7FFFF7FFFF7FFF88000000000000087777777777777FFF8800000
      00000000FFFFFFFFFFFFFFFFF8800000008FFFFFFFFFFFFFFFFFFFFFF8800000
      008F8F8F8F8F8F8F8F8F8F8F888000000037B7B7B7B7B7B7B7B7B7B7B3800000
      003B7B7B7B7B7B7B7B7B7B7B73300000003FFFFFFFFFFFFFFFFFFFFFF3300000
      003B7B7B7B7B7B7B7B7B7B7B7B3000000003333333333333333333333330FE00
      0003FE000001F8000000F8000000F0000000F0000000F8000000F8000000F800
      0000FC000000FC000000FC000000FC000000FC000000F8000000980000000000
      0000600000007800000078000000600000000000000098000000F8000000F800
      0000FC000000FC000000FC000000FC000000FC000000FC000000FE000000}
    Icons.Icons = {
      02000000FE0200000000010001002020100000000000E8020000160000002800
      0000200000004000000001000400000000000002000000000000000000000000
      0000000000000000000000008000008000000080800080000000800080008080
      0000C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF
      0000FFFFFF000000000888888888888888888888880000000008777777777777
      7777777777000000088888888888888888888888878000000877777777777777
      7777777778800000888888888888888888888888788000008FFFFFFFFFFFFFFF
      FFFFFFF87780000008FFFFFFFFFFFFFFFFFFFFFF8780000008FFFFFFFFFFFFFF
      FFFFFFFF8780000008FFFF7777777F777777777F87800000008FFF7FFFF7FF77
      7FF777FFF8800000008FFF7FFFF7FF777FF777FFF8800000008FFF7777777F77
      7FF777FFF8800000008FFFFFFFF7FF77777777FFF8800000008888888FF7FF77
      7FF777FFF88000000000000087777F777FF777FFF8800880000000008FF7FFFF
      FFF777FFF8808008800777778FF7FFFF7FFFFFFFF88000000770000087777777
      777777FFF8800000000000008FF7FFFF7FFFF7FFF8800000000000008FF7FFFF
      7FFFF7FFF88000088000000087777777777777FFF8800880077000008FF7FFFF
      7FFFF7FFF8800000000777778FF7FFFF7FFFF7FFF88000000000000087777777
      777777FFF880000000000000FFFFFFFFFFFFFFFFF8800000008FFFFFFFFFFFFF
      FFFFFFFFF8800000008F8F8F8F8F8F8F8F8F8F8F888000000037B7B7B7B7B7B7
      B7B7B7B7B3800000003B7B7B7B7B7B7B7B7B7B7B73300000003FFFFFFFFFFFFF
      FFFFFFFFF3300000003B7B7B7B7B7B7B7B7B7B7B7B3000000003333333333333
      333333333330FE000003FE000001F8000000F8000000F0000000F0000000F800
      0000F8000000F8000000FC000000FC000000FC000000FC000000FC000000F800
      0000980000000000000060000000780000007800000060000000000000009800
      0000F8000000F8000000FC000000FC000000FC000000FC000000FC000000FC00
      0000FE000000BE0800000000010001002020000000000000A808000016000000
      2800000020000000400000000100080000000000800400000000000000000000
      0001000000000000000000000000800000800000008080008000000080008000
      80800000C0C0C000C0DCC000F0CAA600D4F0FF00B1E2FF008ED4FF006BC6FF00
      48B8FF0025AAFF0000AAFF000092DC00007AB90000629600004A730000325000
      D4E3FF00B1C7FF008EABFF006B8FFF004873FF002557FF000055FF000049DC00
      003DB900003196000025730000195000D4D4FF00B1B1FF008E8EFF006B6BFF00
      4848FF002525FF000000FE000000DC000000B900000096000000730000005000
      E3D4FF00C7B1FF00AB8EFF008F6BFF007348FF005725FF005500FF004900DC00
      3D00B900310096002500730019005000F0D4FF00E2B1FF00D48EFF00C66BFF00
      B848FF00AA25FF00AA00FF009200DC007A00B900620096004A00730032005000
      FFD4FF00FFB1FF00FF8EFF00FF6BFF00FF48FF00FF25FF00FE00FE00DC00DC00
      B900B900960096007300730050005000FFD4F000FFB1E200FF8ED400FF6BC600
      FF48B800FF25AA00FF00AA00DC009200B9007A009600620073004A0050003200
      FFD4E300FFB1C700FF8EAB00FF6B8F00FF487300FF255700FF005500DC004900
      B9003D00960031007300250050001900FFD4D400FFB1B100FF8E8E00FF6B6B00
      FF484800FF252500FE000000DC000000B9000000960000007300000050000000
      FFE3D400FFC7B100FFAB8E00FF8F6B00FF734800FF572500FF550000DC490000
      B93D0000963100007325000050190000FFF0D400FFE2B100FFD48E00FFC66B00
      FFB84800FFAA2500FFAA0000DC920000B97A000096620000734A000050320000
      FFFFD400FFFFB100FFFF8E00FFFF6B00FFFF4800FFFF2500FEFE0000DCDC0000
      B9B90000969600007373000050500000F0FFD400E2FFB100D4FF8E00C6FF6B00
      B8FF4800AAFF2500AAFF000092DC00007AB90000629600004A73000032500000
      E3FFD400C7FFB100ABFF8E008FFF6B0073FF480057FF250055FF000049DC0000
      3DB90000319600002573000019500000D4FFD400B1FFB1008EFF8E006BFF6B00
      48FF480025FF250000FE000000DC000000B90000009600000073000000500000
      D4FFE300B1FFC7008EFFAB006BFF8F0048FF730025FF570000FF550000DC4900
      00B93D00009631000073250000501900D4FFF000B1FFE2008EFFD4006BFFC600
      48FFB80025FFAA0000FFAA0000DC920000B97A000096620000734A0000503200
      D4FFFF00B1FFFF008EFFFF006BFFFF0048FFFF0025FFFF0000FEFE0000DCDC00
      00B9B900009696000073730000505000F2F2F200E6E6E600DADADA00CECECE00
      C2C2C200B6B6B600AAAAAA009E9E9E0092929200868686007A7A7A006E6E6E00
      62626200565656004A4A4A003E3E3E0032323200262626001A1A1A000E0E0E00
      F0FBFF00A4A0A000808080000000FF0000FF000000FFFF00FF000000FF00FF00
      FFFF0000FFFFFF0007070707070707F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8
      F8F8F8F8F8F8070707070707070707F807070707070707070707070707070707
      07070707070700070707070707F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8
      F8F8F8F8F807F8000707070707F8070707070707070707070707070707070707
      0707070707F8F80007070707F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8F8
      F8F8F8F807F8F80007070707F8DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
      DCDCDCF80707F8000707070707F8DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
      DCDCDCDCF807F8000707070707F8DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
      DCDCDCDCF807F8000707070707F8DCDCDCDC07070707070707DC070707070707
      070707DCF807F800070707070707F8DCDCDC07FFFFFFFF07DCDC070707FFFF07
      0707DCDCDCF8F800070707070707F8DCDCDC07FFFFFFFF07DCDC070707FFFF07
      0707DCDCDCF8F800070707070707F8DCDCDC07070707070707DC070707FFFF07
      0707DCDCDCF8F800070707070707F8DCDCDCDCDCDCDCDC07DCDC070707070707
      0707DCDCDCF8F800070707070707F8F8F8F8F8F8F8DCDC07DCDC070707DCDC07
      0707DCDCDCF8F800070707070700000000000000F807070707DC070707DCDC07
      0707DCDCDCF8F80007F8F8070700000000000000F8FFFF07DCDCDCDCDCDCDC07
      0707DCDCDCF8F800F80000F8F800000707070707F8FFFF07DCDCDCDC07DCDCDC
      DCDCDCDCDCF8F800000707000007073535353535F80707070707070707070707
      0707DCDCDCF8F800000707070735353535353535F8FFFF07FFFFFFFF07FFFFFF
      FF07DCDCDCF8F800000707070735353535353535F8FFFF07FFFFFFFF07FFFFFF
      FF07DCDCDCF8F800000707F8F835353535353535F80707070707070707070707
      0707DCDCDCF8F80000F8F8000007073535353535F8FFFF07FFFFFFFF07FFFFFF
      FF07DCDCDCF8F800070000070700000707070707F8FFFF07FFFFFFFF07FFFFFF
      FF07DCDCDCF8F800070707070700000000000000F80707070707070707070707
      0707DCDCDCF8F800070707070700000000000000DCDCDCDCDCDCDCDCDCDCDCDC
      DCDCDCDCDCF8F800070707070707F8DCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDCDC
      DCDCDCDCDCF8F800070707070707F8DCF8DCF8DCF8DCF8DCF8DCF8DCF8DCF8DC
      F8DCF8DCF8F8F8000707070707070307FB07FB07FB07FB07FB07FB07FB07FB07
      FB07FB07FB03F80007070707070703FB07FB07FB07FB07FB07FB07FB07FB07FB
      07FB07FB0703030007070707070703FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFF03030007070707070703FB07FB07FB07FB07FB07FB07FB07FB07FB
      07FB07FB07FB0300070707070707070303030303030303030303030303030303
      0303030303030300000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000}
    PopupMenu = Menu_Tray
    OnClick = TrayIconClick
    Left = 230
    Top = 170
  end
  object Menu_Tray: TTntPopupMenu
    Left = 330
    Top = 200
    object TMRestore: TTntMenuItem
      Caption = '&Restore'
      OnClick = TMRestoreClick
    end
    object TMClipCap: TTntMenuItem
      Caption = '&Clipboard capture'
      OnClick = MMNoteClipCaptureClick
    end
    object N8: TTntMenuItem
      Caption = '-'
    end
    object TMExit: TTntMenuItem
      Caption = 'E&xit'
      OnClick = MMFileExitClick
    end
  end
  object Menu_RTF: TTntPopupMenu
    OnPopup = Menu_RTFPopup
    Left = 360
    Top = 135
    object RTFMUndo: TTntMenuItem
      Caption = '&Undo'
      Hint = 'Undo last change'
      OnClick = MMEditUndoClick
    end
    object RTFMRepeatCmd: TTntMenuItem
      Caption = '&Repeat Last Command'
      Hint = 'Repeat last editing command'
      OnClick = MMEditRepeatClick
    end
    object N20: TTntMenuItem
      Caption = '-'
    end
    object RTFMCut: TTntMenuItem
      Caption = 'Cu&t'
      Hint = 'Cut text to clipboard'
      OnClick = MMEditCutClick
    end
    object RTFMCopy: TTntMenuItem
      Caption = '&Copy'
      Hint = 'Copy text to clipboard'
      OnClick = MMEditCopyClick
    end
    object RTFMPaste: TTntMenuItem
      Caption = '&Paste'
      Hint = 'Paste text from clipboard'
      OnClick = MMEditPasteClick
    end
    object RTFMPasteAsText: TTntMenuItem
      Caption = 'Paste as Te&xt'
      Hint = 'Paste text from clipboard without formatting'
      OnClick = MMEditPasteAsTextClick
    end
    object RTFMDelete: TTntMenuItem
      Caption = '&Delete'
      Hint = 'Delete selected text'
      OnClick = MMEditDeleteClick
    end
    object N70: TTntMenuItem
      Caption = '-'
    end
    object RTFMFont: TTntMenuItem
      Caption = '&Font...'
      OnClick = MMFormatFontClick
    end
    object RTFMPara: TTntMenuItem
      Caption = 'P&aragraph...'
      OnClick = MMFormatParagraphClick
    end
    object N43: TTntMenuItem
      Caption = '-'
    end
    object RTFMWordWeb: TTntMenuItem
      Caption = '&Look Up in WordWeb'
      Hint = 'Look up word in WordWeb thesaurus'
      OnClick = RTFMWordWebClick
    end
    object N21: TTntMenuItem
      Caption = '-'
    end
    object RTFMProperties: TTntMenuItem
      Caption = 'Note P&roperties...'
      Hint = 'Edit note properties'
      OnClick = MMNotePropertiesClick
    end
    object N19: TTntMenuItem
      Caption = '-'
    end
    object RTFMSelectall: TTntMenuItem
      Caption = 'Select &All'
      Hint = 'Select all text'
      OnClick = MMEditSelectAllClick
    end
    object RTFMWordwrap: TTntMenuItem
      Caption = '&Word Wrap'
      Hint = 'Toggle wrapping at end of lines'
      OnClick = MMFormatWordWrapClick
    end
  end
  object Menu_TAB: TTntPopupMenu
    AutoPopup = False
    Left = 425
    Top = 140
    object TAM_ActiveName: TTntMenuItem
      Caption = '(none)'
      Enabled = False
    end
    object N24: TTntMenuItem
      Caption = '-'
    end
    object TAM_NewTab: TTntMenuItem
      Caption = '&New Note...'
      Hint = 'Create a new note'
      OnClick = MMNoteNewClick
    end
    object N16: TTntMenuItem
      Caption = '-'
    end
    object TAM_SetAlarm: TTntMenuItem
      Caption = 'Set &alarm...'
      Hint = 
        'Adds or edit an alarm directly to the note (CTR+click forces to ' +
        'add)'
      OnClick = TAM_SetAlarmClick
    end
    object TAM_Renametab: TTntMenuItem
      Caption = '&Rename Note'
      Hint = 'Rename current note'
      OnClick = MMNoteRenameClick
    end
    object TAM_Properties: TTntMenuItem
      Caption = '&Properties'
      Hint = 'Edit note properties'
      OnClick = MMNotePropertiesClick
    end
    object N17: TTntMenuItem
      Caption = '-'
    end
    object TAM_Delete: TTntMenuItem
      Caption = 'R&emove Note'
      Hint = 'Delete current note'
      OnClick = MMNoteRemoveClick
    end
  end
  object IMG_Toolbar: TImageList
    Left = 90
    Top = 75
    Bitmap = {
      494C010135003600040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000E0000000010020000000000000E0
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080400000804000008040
      0000804000008040000080400000804000008040000080400000804000008040
      0000804000008040000080400000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF008080800000408000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF000080
      8000008080000080800000808000008080000080800000808000008080000080
      800000808000FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080
      800000008000FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF008080
      800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080008000
      8000FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080008000
      8000FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080008000
      8000FFFFFF00FFFFFF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000000080000000
      80008080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080008000
      8000800080008000800000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080008000
      80000000FF000000FF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF008080800000FFFF0000FFFF0000FFFF0000808000404000000000
      FF000000FF000000FF0000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000800000FFFF0000808000000080000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000808080000000000000000000848484008484
      8400848484008484840084848400000000000000000000000000848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080400000804000008040
      0000804000008040000080400000804000008040000080400000804000008040
      0000804000008040000080400000808080000000000084000000840000008400
      0000840000008400000084848400000000000000000084000000840000008400
      0000840000008400000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000084000000840000008400
      0000840000008400000084848400000000000000000084000000840000008400
      0000840000008400000084848400000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000080
      8000008080000080800000808000008080000080800000808000008080000080
      8000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000084000000000000000000
      0000840000008400000000000000000000000000000084000000840000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000080
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00808080000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      0000840000000000000000000000000000000000000084000000848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000080
      8000000000000000000000000000000000000000000000000000C0C0C0008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000008080000000000000000000808080000000000000000000000000008400
      0000848484000000000000000000000000000000000000000000840000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00804000000000000000000000808080000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000840000008400
      0000848484008484840000000000000000000000000000000000000000000000
      00000000000000000000FF0000000000000000000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008040
      0000C0C0C0000000000000000000808080000000000084000000840000008484
      8400000000000000000000000000000000000000000000000000000000008400
      0000840000008484840000000000000000000000000000000000000000000000
      000000000000FF00000000000000000000000000000000000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008040
      0000000000000000000000000000808080000000000084000000840000008484
      8400000000000000000000000000000000000000000000000000000000008400
      0000840000008484840000000000000000000000000000000000000000000000
      0000FF000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008040
      0000000000000000000000000000808080000000000084000000840000008484
      8400848484000000000000000000000000000000000000000000000000008400
      000084000000000000000000000000000000000000000000000000000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000FFFFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000040400000404000008040
      000080808000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00808080008040
      0000404000008040000000000000808080000000000000000000840000008400
      0000848484008484840000000000000000000000000000000000840000008400
      000084848400000000000000000000000000000000000000000000000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      000080808000FFFFFF0000FFFF0000FFFF0000FFFF0000808000008080000000
      00000000000000000000000000000000000000000000FF800000FF800000FF80
      0000FF800000FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C00080808000FF80
      0000FF800000FF80000000000000808080000000000000000000840000008400
      0000840000008484840084848400848484008484840084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      00000000000080808000FFFFFF0000FFFF000080800000000000000000000000
      00000000000000000000000000000000000000000000FF800000FF800000FF80
      0000FF800000C0C0C000FFFFFF00FFFFFF00FFFFFF00C0C0C00040400000FF80
      0000FF800000FF80000000000000808080000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000FF800000FF800000FF80
      0000FF80000080808000C0C0C000FFFFFF00C0C0C00040400000FF800000FF80
      0000FF800000FF80000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004242
      42004242420042424200A5636300A5636300A5636300A5636300424242004242
      4200424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004242
      42006321000084420000A5A56300A5A56300A5A56300A5A56300844200006321
      0000424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000210000008442
      0000B5B59400C6C6C600C6C6C600B5949400B5949400C6C6C600C6C6C600B5B5
      9400844200002100000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084420000EFEF
      EF00C6C6C60063393900C6C6C60094CECE00B5949400C6C6C60063393900C6C6
      C600C6C6C6008442000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000063210000BDBD9C00CECE
      CE00EFEFEF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600B5B5940063210000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000FFFFFF000000000000007B0000007B00FFFF
      FF0000000000FFFFFF0000000000000000000000000084420000000000006339
      3900CECECE00EFEFEF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C60063393900C6C6C60084420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000007B0000007B000000
      0000FFFFFF0000000000FFFFFF000000000042000000A5A56300EFEFEF000000
      0000CECECE00CECECE00B5949400B594940063636300C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600A5A56300420000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF007B7B7B000000
      7B0000007B000000000000000000FFFFFF0000000000FFFFFF0000007B000000
      7B0000000000FFFFFF00000000000000000042000000A5A56300B5949400DEBD
      BD0000000000CECECE00C6C6C600523131003131310063636300636363009494
      9400B5949400B5949400A5A56300420000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000007B000000
      7B00FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      7B0000007B0000000000FFFFFF000000000042000000A5A5630094CECE00B594
      9400EFEFEF0000000000CECECE00C6C6C60052313100B5949400C6C6C600C6C6
      C60094CECE00B5949400A5A5630042000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF007B7B7B000000
      7B0000007B000000000000000000FFFFFF0000007B0000007B0000007B000000
      7B0000007B00FFFFFF00000000000000000042000000A5A56300C6C6C600C6C6
      C600C6C6C600EFEFEF0000000000C6C6C60063636300C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600A5A56300420000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000007B000000
      7B00FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000084420000C6C6C6006339
      3900C6C6C600C6C6C600CECECE00C6C6C60063636300C6C6C600CECECE00C6C6
      C60063393900C6C6C60084420000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF007B7B7B000000
      7B0000007B00000000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000007B0000007B000000000000000000000063210000B5B59400C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60094949400C6C6C600CECECE00EFEF
      EF00C6C6C600B5B5940063210000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF00000000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000007B0000007B00000000000000000000000000000084420000C6C6
      C600C6C6C60063393900C6C6C600B5949400B5949400C6C6C60063393900CECE
      CE00EFEFEF008442000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000210000008442
      0000B5B59400C6C6C600C6C6C60094CECE00B5949400C6C6C600C6C6C600B5B5
      9400844200002100000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      00007B0000007B0000007B0000007B0000007B0000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000004242
      42006321000084420000A5A56300A5A56300A5A56300A5A56300844200006321
      0000424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004242
      42004242420042424200A5636300A5636300A5636300A5636300424242004242
      4200424242000000000000000000000000000000000000000000000000000000
      0000000000008484840084000000840000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008400000084848400848484008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000000000000000000000000
      00000000000084000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000840000008400000084000000840000008400000000000000FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000848484000000000000000000000000000000
      0000840000008400000084000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400840000000000000000000000000000000000000084000000848484000000
      000000000000000000000000000000000000FF00000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000000000
      000000000000FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000848484000000000000000000000000008400
      0000840000008400000084000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000848484000000
      000000000000000000000000000000000000FF00000084000000840000008400
      000084000000840000008400000084000000840000008400000084000000FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000000000000840000008400
      000084000000FFFFFF008400000084000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FFFFFF000000
      0000FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000848484000000000084000000840000008400
      0000FFFFFF0000000000000000008400000084000000FFFFFF00000000000000
      0000000000000000000000000000000000008484840084848400848484008400
      0000848484008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400000000000000000084848400000000000000000084000000FFFF
      FF000000000000000000000000000000000084000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00848484008400
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00000000000000000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FFFFFF000000000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000FFFFFF0000000000000000000000000000000000000000008400
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FF00
      0000FF000000FF000000FF000000FF000000FFFFFF0000000000000000000000
      000000000000FFFFFF0084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000FFFFFF00000000000000000000000000000000008484
      8400840000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000008484840000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000840000000000000000000000000000008484840084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FF000000FFFFFF00FFFFFF00FFFFFF0000000000848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008400000084848400848484008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000084848400000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084000000840000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000848484000000
      000000000000848484000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00FFFFFF0000008400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      000084848400000000000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00000084000000840000008400FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000008400000084000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF000000840000008400000084000000840000008400FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000840000008400000000000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000008484
      840000000000000000000000000000000000000000008484840000008400FFFF
      FF000000840000008400FFFFFF00000084000000840000008400FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000008400
      0000840000000000000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF0000008400FFFFFF00FFFFFF00FFFFFF00000084000000840000008400FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000840000008400000000000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000848484000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000840000008400FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000008400000084000000000000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000008400FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000084848400000000000000
      000000000000000000000000000000000000000000008484840000008400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000084000000
      8400000084000000840000008400000084000000840000008400000084000000
      840000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      840084848400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084000000840000008400
      0000848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000008400000084000000
      840000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000008400000084000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000008400000084000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000008400000000000000
      0000000000000000000000000000000000000000000084848400840000008400
      0000848484000000000000000000848484008400000084000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008484
      8400848484008484840084000000840000008400000084000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084848400848484008400000084000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      00008400000084848400848484008484840084000000FF000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF000000FF0000008484840084848400FF00000084000000FF0000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000840000008484840084000000FF000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF000000FF000000FF000000FF000000FF0000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000084000000840000008400000084000000840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF00000084000000FF000000840000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000008400000000000000840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00007B0000007B0000007B0000007B0000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000084000000840000008400000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF0000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000840000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF0000008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      0000000000000000000000000000000000000000000000000000000000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000000000000000000000000000000000000000007B000000000000007B00
      0000000000007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B0000007B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000007B0000000000000000000000000000000000000000007B0000007B00
      00007B000000000000000000000000000000000000007B0000007B0000007B00
      0000000000007B0000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B0000007B0000007B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000007B0000007B0000000000000000007B0000007B0000007B0000007B00
      00007B0000007B0000007B00000000000000000000007B7B7B007B0000000000
      000000000000000000007B0000007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B0000007B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000007B0000000000000000000000000000000000000000007B0000007B00
      00007B00000000000000000000000000000000000000000000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B0000007B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      7B0000000000000000000000000000000000000000007B000000000000007B00
      0000000000007B000000000000000000000000000000000000007B7B7B007B00
      0000000000007B0000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B0000007B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840000000000000000000000840000008400000000000000
      0000000000000000000000000000000000000000000000007B0000007B000000
      0000000000000000000000000000000000000000000000000000000000007B00
      0000000000000000000000000000000000000000000000000000000000007B00
      00007B0000007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000007B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000007B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B0000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B0000000000000000000000000000008400000084000000840000000000
      0000000000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B00000000000000000000000000000000000000000000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      00000000000000000000840000000000000000000000000000007B0000007B00
      00007B0000007B0000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000007B00000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B000000000000000000000000000000000000007B0000007B00
      00007B0000007B0000007B0000007B0000007B00000000000000000000000000
      0000840000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B00000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000840000000000000000000000000000007B0000007B00
      00007B0000007B0000007B0000000000000000000000000000007B0000007B00
      00007B0000007B00000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B00000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B000000000000000000000000000000000000007B0000007B00
      00007B0000007B0000007B0000007B0000007B0000007B0000007B0000000000
      0000000000000000000084000000000000000000000000000000000000000000
      00007B0000007B00000000000000000000000000000000000000000000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B0000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B0000007B000000000000007B0000007B0000007B000000FF00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B0000007B000000000000000000000000000000000000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B000000FF00
      00007B0000007B0000007B000000000000007B0000007B0000007B000000FF00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B000000FF00
      00007B0000007B0000007B0000007B0000007B0000007B0000007B000000FF00
      0000FF0000007B0000007B0000007B0000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      00000000000000000000000000007B0000007B00000000000000000000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B0000007B0000007B00
      00007B0000007B00000000000000000000007B0000007B0000007B000000FF00
      0000FF0000007B0000007B0000007B0000007B0000007B0000007B000000FF00
      0000FF0000007B0000007B0000007B000000000000000000FF000000000000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B000000000000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B000000FF00
      0000FF0000007B0000007B0000007B0000007B0000007B0000007B000000FF00
      0000FF0000007B0000007B0000007B000000000000000000FF000000000000FF
      FF0000FFFF0000FFFF000000000000000000000000007B0000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B000000FF00
      0000FF000000FF0000007B0000007B0000007B0000007B0000007B000000FF00
      0000FF000000FF0000007B0000007B000000000000000000000000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B000000FF00
      0000FF000000FF0000007B0000007B0000007B0000007B0000007B000000FF00
      0000FF000000FF0000007B0000007B00000000000000FFFFFF0000000000FF00
      0000FF000000FF0000000000000000000000000000007B0000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B000000FF00
      0000FF000000FF000000FF0000007B0000007B0000007B0000007B000000FF00
      0000FF000000FF000000FF0000007B00000000000000FFFFFF0000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B000000FF00
      0000FF000000FF000000FF0000007B0000007B0000007B0000007B000000FF00
      0000FF000000FF000000FF0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF00000000000000FF00000000FFFF00FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF000000FF00
      000000000000FF00000000FFFF00FF0000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF00000000000000FF0000FFFF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000000000000000
      000000000000FF000000FF000000FF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000FF000000FF000000FF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000FF000000FF00
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      00007B7B7B00FF0000007B7B7B00000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000FF000000FF00
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00FF0000007B7B7B000000
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF00
      0000FFFFFF00FFFFFF00FFFFFF00000000007B7B7B0000000000000000000000
      00000000000000000000000000007B7B7B00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF00000000000000FF000000FF00
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000FFFF00000000000000
      00000000000000BDBD0000BDBD0000000000FFFFFF00FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000FF00000000000000FF00000000FFFF00FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF00BDBDBD0000000000000000000000000000FFFF0000FF
      FF0000BDBD0000BDBD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF000000FF000000FF000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00BDBDBD00BDBDBD000000000000000000000000000000000000BD
      BD0000BDBD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FF00
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000000000FF
      FF0000BDBD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B0000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000BD
      BD0000BDBD000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF000000000000BDBD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B0000007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000007B00
      0000000000007B000000000000007B000000000000007B0000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF00FFFFFF00FFFFFF0000FF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B0000007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF0000000000000000000000000000000000BD00
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000000000000000000000FFFF00FFFFFF0000000000FFFFFF0000FF
      FF000000000000000000FFFFFF000000000000000000000000007B7B7B00BD00
      00000000000000000000000000000000000000000000BD000000BD000000BD00
      0000BD000000BD00000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF000000000000FFFF0000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000BD0000000000
      0000000000000000000000000000000000000000000000000000BD000000BD00
      0000BD000000BD00000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF000000000000FFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000BD0000000000
      000000000000000000000000000000000000000000000000000000000000BD00
      0000BD000000BD00000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF000000000000000000FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000BD0000000000
      0000000000000000000000000000000000000000000000000000BD0000000000
      0000BD000000BD00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      000000000000000000007B7B7B00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0000000000FFFFFF000000000000FFFF00FFFFFF0000000000FFFFFF0000FF
      FF000000000000000000FFFFFF000000000000000000000000007B7B7B00BD00
      000000000000000000000000000000000000BD000000BD000000000000000000
      000000000000BD0000000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000FFFFFF000000000000FFFF0000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000007B7B
      7B00BD000000BD000000BD000000BD0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B000000
      000000000000000000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B7B
      7B00000000000000000000000000000000000000000000007B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B7B7B0000000000000000000000000000007B000000FF00FFFFFF000000
      00000000000000007B007B007B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B0000007B7B7B000000000000000000000000000000FF0000007B00FFFF
      FF0000007B000000FF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B0000007B0000007B7B7B000000000000000000000000000000FF007B00
      7B000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF0000007B00000000007B7B7B000000000000000000000000000000
      00007B7B7B0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B0000007B00
      00007B0000007B0000007B0000000000000000000000000000007B007B000000
      FF0000007B00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF0000007B0000007B00000000007B7B7B00000000000000FF000000
      7B00000000007B7B7B00000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      00007B0000007B00000000000000000000007B007B000000FF000000FF000000
      00000000000000007B00FFFFFF0000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000007B0000007B007B7B7B000000FF0000007B000000
      7B000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF000000000000000000000000007B0000007B00
      00007B00000000000000000000000000000000007B007B007B00000000000000
      0000000000000000000000007B0000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF0000007B0000007B000000FF0000007B0000007B000000
      7B000000000000000000000000000000000000000000FFFFFF00000000007B7B
      7B007B7B7B007B7B7B00000000007B7B7B007B7B7B007B7B7B007B0000007B00
      00000000000000000000000000000000000000000000FFFFFF00000000007B7B
      7B007B7B7B007B7B7B00000000007B7B7B007B7B7B007B7B7B007B7B7B00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000007B0000007B0000007B0000007B000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF007B7B7B00FFFFFF00FFFFFF00FFFFFF007B7B7B007B7B7B007B000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF007B7B7B00FFFFFF00FFFFFF00FFFFFF007B7B7B007B7B7B0000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF000000
      00000000000000000000000000007B0000000000000000000000000000000000
      00000000000000000000000000000000FF0000007B0000007B0000007B000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B0000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      00007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B0000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      000000000000000000000000FF0000007B000000000000007B0000007B000000
      00007B7B7B0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007B7B7B00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007B7B7B00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      000000000000000000000000FF0000007B00000000000000FF0000007B000000
      7B007B7B7B007B7B7B00000000000000000000000000FFFFFF00000000000000
      0000000000007B7B7B00FFFFFF007B7B7B007B7B7B000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000007B7B7B00FFFFFF007B7B7B007B7B7B000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      0000000000000000FF0000007B000000000000000000000000000000FF000000
      7B00000000007B7B7B007B7B7B000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      000000000000000000000000FF00000000000000000000000000000000000000
      000000007B0000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000007B7B7B007B7B7B00000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000007B7B7B007B7B7B00000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000007B0000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B0000007B0000007B0000007B0000007B00
      00007B0000007B0000007B0000007B0000000000000000000000000000000000
      000000000000000000007B000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000000000007B7B7B00007B7B007B7B
      7B00007B7B007B7B7B007B000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF007B00000000000000007B7B007B7B7B00007B
      7B007B7B7B00007B7B007B000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000000000007B7B7B00007B7B007B7B
      7B00007B7B007B7B7B007B000000FFFFFF00000000000000000000000000FFFF
      FF007B0000007B0000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B00000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF007B00000000000000007B7B007B7B7B00007B
      7B007B7B7B00007B7B007B000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B000000FFFFFF007B0000000000000000000000000000007B0000007B00
      00007B0000007B0000007B000000000000000000000000000000000000000000
      00007B00000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF0000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000007B000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000000000007B7B7B00007B7B007B7B
      7B00007B7B007B7B7B007B000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B0000007B000000000000000000000000000000000000007B0000007B00
      00007B0000007B00000000000000000000000000000000000000000000000000
      0000000000007B000000000000000000000000000000FFFFFF00000000007B7B
      7B007B7B7B007B7B7B00000000007B7B7B007B7B7B007B7B7B007B7B7B00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000FFFFFF000000000000000000FFFF
      FF007B0000007B0000007B0000007B00000000000000007B7B007B7B7B00007B
      7B007B7B7B00007B7B007B0000007B0000007B0000007B0000007B0000007B00
      00007B00000000000000000000000000000000000000000000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000007B000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF007B7B7B00FFFFFF00FFFFFF00FFFFFF007B7B7B007B7B7B0000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000007B000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B000000FFFFFF007B00000000000000000000007B7B7B00007B7B007B7B
      7B00007B7B007B7B7B00007B7B007B7B7B00007B7B007B7B7B00007B7B007B7B
      7B00007B7B0000000000000000000000000000000000000000007B0000007B00
      0000000000007B00000000000000000000000000000000000000000000000000
      0000000000007B000000000000000000000000000000FFFFFF00000000000000
      00007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B0000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B0000007B000000000000000000000000000000007B7B007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B0000000000000000000000000000000000000000007B0000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      00007B00000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007B7B7B00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF00000000007B0000007B0000007B0000007B0000007B00
      00007B000000000000000000000000000000000000007B7B7B007B7B7B000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00007B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000007B7B7B00FFFFFF007B7B7B007B7B7B000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      00000000000000000000000000000000000000000000007B7B007B7B7B00007B
      7B000000000000FFFF00000000000000000000FFFF00000000007B7B7B00007B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000007B7B7B007B7B7B00000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B00007B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      00007B00000000000000000000007B00000000000000000000007B0000007B00
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000007B7B00007B
      7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B000000
      0000000000000000000000000000000000000000000000000000007B7B00007B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      00007B00000000000000000000007B000000000000007B000000000000000000
      00007B000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF0000000000007B
      7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B00000000000000000000000000000000000000000000000000007B7B00007B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      00007B00000000000000000000007B000000000000007B000000000000000000
      00007B000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF000000
      0000007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B00007B7B000000000000000000000000000000000000000000007B7B00007B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      0000000000007B0000007B0000007B000000000000007B000000000000000000
      00007B000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF0000000000007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B00007B7B00007B7B0000000000000000000000000000000000007B7B00007B
      7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B00007B7B00007B7B0000000000000000000000000000000000000000000000
      00000000000000000000000000007B000000000000007B0000007B0000007B00
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B00007B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000007B7B00007B7B0000000000000000000000000000000000000000000000
      00000000000000000000000000007B000000000000007B000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000FFFF00FFFFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000007B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000007B7B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000E00000000100010000000000000700000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000C000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00010000000000000001000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFC000
      C1C1FFFFFFFF00008181FFFFFE7F7FFC8181FE7FC0037FFCB39BFE7FE0077C7C
      F78FFE7FE00F4004E7C7FE7FF00F6004CFC3FDBFF00F60048FE3FBDFF00F700C
      8FE3F7EFF00F700C87E7EFF7F00F0000C3C7E007F01F0000C00FFFF8F83F0000
      F03FFFF8FE7F0001FFFFFFFFFFFF0001FFFFFFFFFFFFE007C007FFFBF800E007
      BFEBFFF7F954C0030005FE6F0202C0037E31FD9F515480017E35FD5F028AA001
      0006FD2F511410007FEAFA0F028A08008014F41F41440400C00AC8FF02020200
      E00191FF41548001E0078BFF00008001F007C3FF5000C003F003E7FF0000C003
      F803FFFF001FE007FFFFFFFF001FE007F87FF800FFFFFFFFF03FF000FF71F9FF
      F7BF8020FF36F0FFE79F0018F516E0FFEFDF0000DF31C07FCFFF8010FF76863F
      0001F000BFF6CF3FCFFF8018FFD1FF9F00018000DFFFFFCFCFCF8030FFBFFFE7
      CF8F800077FFA9C3EF0F803877BFAAB9E78F800007FF89BDF73F8001AFD7AABF
      F03F803F8FE7D9CFF87F803FDFC7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF8003FFFFFFFFC381BFFBFEDFFB7FE7C3800BFC9FF93FF3C7800BF81FF81F
      F3C7800BF001800FF80F800BE0018007F98F800BC0018003FC9F800BE0018007
      FC1F800BF001800FFE3F800BF81FF81FFE3F800BFC9FF93FFF7F800BFEDFFB7F
      FFFF8003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0FFB0FFCFFFFFFFF0FF90FF03
      FFFFFF00FF00FE00FFFFFEF0FE90E701F01FFDFFFDBF8601F01FFDFFFDFF0001
      F01FFDFFFDFFC001F01FFEFFFEFFE001F01FFF7FFF7FF001F01FFFBFFFBFF801
      F01FFFBFFFBFFC01F01FFFBFFDBFFE01FFFF0F7F097FFF01FFFF00FF00FFFF81
      FFFF0FFF09FFFFC1FFFF0FFF0DFFFFF1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFFF33FBFFF8001FCFFF33F9FEF
      FFFFFC7FF33F8FABFFFFFC3FF33F87C78881FC1FF33F83019CFFFC3FF33F87C7
      C1FFFC7FF33F8FABC981FCFFF33F9FEFE3FFFDFFFFFFBFFFE3FFFFFFFFFFFFFF
      F781FFFFFFFFFFFFFFFFFFFFFFFFFFFF00008718FFFFFFFF0000FFBDFFFFFFFF
      000081C1FFFF81D70000FFEDC003FFD700008075FFFFFFD70000FFF9C1C381D7
      0000801DF3E7FF170000FFFFF9E7FF170000FFFFFC07811700000101FE67FF03
      000001FFFF27FFFF00000101FF878003000001FFFFC7FFFF00000101FFE7FFFF
      000001FFFFFFF803000001FFFFFFFFFFFE3FFE3FFE3FFE3FC23FC23FC23FC23F
      DE3FDE3FDE3FDE3FDFFFDFFFDFFFDFFFDE3FDFF8D801DFF8C23FDF08C001DF08
      DE3FDF78D801DF78DFFFDFFFDFFFDFFFDE3FDE3FDE3FDE3FC23FC23FC23FC23F
      DE3FDE3FDE3FDE3FFFFFFFFFFFFFFFFF8FFF8FFF8FFF8FFF8FFF8FFF8FFF8FFF
      8FFF8FFF8FFF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3F24FFFFFFFFFFC23F
      24FFFFFF8001DE3FFFFFFFFF8001DFFFFFFFF0008001DE3FFFFC40008001C23F
      FBF8F0008001DE3FB1F140008001DFFF1BE3F0000003DE3FBFC740003807C23F
      FF8FF000800FDE3FEF1F0000C3FFFFFFC63FF000C3FF8FFFEC7FFFFFC3FF8FFF
      F8FFFFFF89FF8FFFF9FFFFFF81FFFFFF0000FFFFFFFFFFFF001FFFFFFFFFFFF7
      7FDFFFFF07C1FFF33C00FFFF07C1EA815C00FFFF07C1FFF36C00E7FF0101FFF7
      4000CF830001FFFF1400DFC302011C7F2800DFE302011C7F9400DFD38003087F
      C800CF3BC1074A7FE400E0FFC107007FF000FFFFE38F80FFFC00FFFFE38FC9FF
      FC00FFFFE38FC9FFFFFFFFFFFFFFFFFFFFCFBFFFFFFFFFFFFFC719FF000FFDFF
      FFC381FF000FF8FF0001C607000FF0F70001C207000FF0433FC319E7000FF807
      1DC73DE7000FF80F22072207000FFC0F00070027008EFE0F302730271144FC07
      004700470AB8FC8338673867057CF8C100870087FAFCFDF33CE73CE7FDF8FFF7
      00070007FE04FFFF00070007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFC00FFFFFFFF
      FE008000FFFFFFFFFE000000FFFF0007FE000000FFFF000780000000FFF73FE7
      80000001C1F71DE780000003C3FB220780000003C7FB002780010003CBFB3027
      80030003DCF7004780070FC3FF0F3867807F0003FFFF008780FF8007FFFF3CE7
      81FFF87FFFFF0007FFFFFFFFFFFF0007FFFFFFFFFFFFFFFFFFFFFFFFC001F9FF
      C007001F8031F6CFC007000F8031F6B7C00700078031F6B7C00700038001F8B7
      C00700018001FE8FC00700008001FE3FC007001F8FF1FF7FC007001F8FF1FE3F
      C007001F8FF1FEBFC0078FF18FF1FC9FC00FFFF98FF1FDDFC01FFF758FF5FDDF
      C03FFF8F8001FDDFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object OpenDlg: TTntOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoDereferenceLinks]
    Left = 180
    Top = 70
  end
  object SaveDlg: TTntSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Title = 'Save Keynote file'
    Left = 180
    Top = 115
  end
  object IMG_Format: TImageList
    Left = 90
    Top = 130
    Bitmap = {
      494C010116001800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000006000000001002000000000000060
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000008400000084000000840000008400000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000008400000000000000840000008400000000000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000840000008400000084000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B00000000000000000000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B000000000000FF000000FF
      000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00
      FF00FF00FF00000000007B7B7B007B7B7B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B000000000000FF000000FF
      000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00
      FF00FF00FF00000000007B7B7B007B7B7B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B000000000000FF000000FF
      000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00FF00FF00
      FF00FF00FF00000000007B7B7B007B7B7B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B0000007B0000000000
      00007B0000000000000000000000000000007B0000007B0000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000007B7B7B007B7B
      7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000000000
      00007B000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000007B7B7B007B7B
      7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000000000
      00007B00000000000000000000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000007B7B7B007B7B
      7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B0000007B0000007B00
      00007B0000007B00000000000000000000007B0000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000000000FF000000
      FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF000000FF00
      0000FF000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000000000FF000000
      FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF000000FF00
      0000FF000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B00000000000000FF000000
      FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF000000FF00
      0000FF000000000000007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B0000007B0000007B0000007B0000007B0000007B0000007B0000007B00
      00007B0000007B0000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
      00007B0000007B0000007B0000007B0000007B00000000000000000000000000
      00007B0000007B0000007B0000007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
      000000000000000000007B0000007B0000000000000000000000000000000000
      0000000000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      000000FF000000FF000000000000FFFF0000FFFF0000FFFF000000000000FF00
      FF00FF00FF00FF00FF0000000000000000000000000000000000000000000000
      00000000000000000000000000007B0000007B00000000000000000000000000
      0000000000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000007B0000007B0000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000000000
      0000000000007B0000007B0000000000000000000000000000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000007B00
      0000000000007B0000007B000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B0000007B0000007B0000007B00
      00007B0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B0000000000FFFFFF00FFFFFF00FFFFFF00000000000000
      00000000000000000000000000000000000000000000000000007B7B7B000000
      00007B7B7B000000000000000000000000000000000000000000000000007B00
      00007B0000007B0000007B0000000000000000000000000000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00000000000000
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      00007B0000007B0000007B0000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B0000007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF000000000000FFFF0000FFFF0000FFFF0000000000FF00
      0000FF000000FF000000000000000000000000000000000000007B7B7B000000
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B0000007B0000007B00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000600000000100010000000000000300000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFF000300000000FF8FFFFF00000000FFBF000300000000
      FFDFFFFF00000000FFEF000300000000CC9FFFFF00000000E1FF000300000000
      F3FFFFFF00000000E1FF000300000000CCFFFFFF00000000FFFF000300000000
      FFFFFFFF00000000FFFFFFFF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFF043FFFFFFFFFFFFD043E67FFFFFC001BFFFF0FF
      C001FFFFBFFFF9FFFFFFFFFFBFFFF0FFFFFFFFFFCA53E67FC001C001FFFDFFC7
      FFFFFFFFFFFDFFDFFFFFFFFFFFFDFFEFFFFFFFFFC20BFFF7FFFFFFFFC20FFFCF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE000FFFFFFFFFFFFC000FFFF3FFFFFFF
      800081D7DFFFFFFF0000FFD7B803FFFF0000FFD71FFFFFFF000081D7FFFFFFFF
      0000FF171FFFFFFF0000FF17BFFFC00100008117D803FFFF0000FF033FFFC001
      0000FFFFFFFFFFFF000080031FFFFFFF0000FFFFBFFFFFFF0001FFFF3803FFFF
      0003F803BFFFFFFF0007FFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFEFFFFFFF000
      FFFFFFFFC001FFFFC27FC27FC001F070FFFFFFFFC001FCF9C200C200C001FE79
      FFFFFFFFC001FF01DE07DE07C001FF99CE079E07C001BBC907FF07FFC00193E1
      CE009E00C00183F1DE00DE00C001D7F9FFFFFFFFC001D7FFC200C200C001C7FF
      FFFFFFFFC001EFFFFEFFFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFF8FFFC007C007C0078C03FFFFFFFFFFFF8FFFC03FF83FF807FFFF
      FFFFFFFFFFFFFFFFC007C007C0078FFFFFFFFFFFFFFF8C03C03FF01FF8078FFF
      FFFFFFFFFFFFFFFFC007C007C007FFFFFFFFFFFFFFFF8FFFC03FF83FF8078C03
      FFFFFFFFFFFF8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFE00FFFFFFFFFFFFFFFFFFFFFF00F81FFF83FFFFFF8C7E3FFF39F1038
      F8C7F1FFF39FBAD7F8C7F8FFF39F0000F80FFC7FF39FD637F8C7FE3FF39FC6D7
      F8C7FF1FF39FEED6F8C7FF8FF39FEC38F00FFF03E10FFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 135
    Top = 175
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 135
    Top = 125
  end
  object FolderMon: TRxFolderMonitor
    Filter = [fnSize, fnLastWrite]
    MonitorSubtree = False
    OnChange = FolderMonChange
    Left = 135
    Top = 75
  end
  object PrintDlg: TPrintDialog
    FromPage = 1
    MinPage = 1
    MaxPage = 9999
    Options = [poPrintToFile, poSelection]
    ToPage = 9999
    Left = 40
    Top = 125
  end
  object Menu_TV: TTntPopupMenu
    OnPopup = Menu_TVPopup
    Left = 360
    Top = 80
    object TVAlarmNode: TTntMenuItem
      Caption = 'Set alarm on node...'
      Hint = 'Set or remove alarm on node'
      OnClick = TVAlarmNodeClick
    end
    object N114: TTntMenuItem
      Caption = '-'
    end
    object TVInsertNode: TTntMenuItem
      Caption = 'Insert &Node'
      Hint = 'Add node before current node'
      ShortCut = 45
      OnClick = TVInsertNodeClick
    end
    object TVAddNode: TTntMenuItem
      Caption = '&Add Node'
      Hint = 'Add node as last sibling'
      ShortCut = 13
      OnClick = TVAddNodeClick
    end
    object TVAddChildNode: TTntMenuItem
      Caption = 'Add &Child'
      Hint = 'Add node as child of current node'
      ShortCut = 8205
      OnClick = TVAddChildNodeClick
    end
    object TVAddSibling: TTntMenuItem
      Caption = 'Add S&ibling'
      Hint = 'Add node immediately after current node'
      ShortCut = 16397
      OnClick = TVAddSiblingClick
    end
    object N44: TTntMenuItem
      Caption = '-'
    end
    object TVChildrenCheckbox: TTntMenuItem
      Caption = 'Children Checkbo&x'
      Hint = 'Show or hide Checkboxes in children of selected node'
      OnClick = TVChildrenCheckboxClick
    end
    object TVCheckNode: TTntMenuItem
      Caption = 'Ch&ecked'
      Hint = 'Check or uncheck selected node'
      ShortCut = 16576
      OnClick = TVCheckNodeClick
    end
    object TVBoldNode: TTntMenuItem
      Caption = '&Bold'
      Hint = 'Make selected node bold'
      ShortCut = 16450
      OnClick = TVBoldNodeClick
    end
    object TVNodeColor_: TTntMenuItem
      Caption = 'Co&lor'
      Hint = 'Choose color for selected node'
      object TVNodeTextColor: TTntMenuItem
        Caption = '&Node Text...'
        Hint = 'Select custom color for node text'
        OnClick = TVNodeTextColorClick
      end
      object TVNodeBGColor: TTntMenuItem
        Caption = 'Node &Background...'
        Hint = 'Select custom color for node background'
        OnClick = TVNodeBGColorClick
      end
      object N103: TTntMenuItem
        Caption = '-'
      end
      object TVDefaultNodeFont: TTntMenuItem
        Caption = '&Reset to Default'
        Hint = 'Reset node color to default'
        ShortCut = 16452
        OnClick = TVDefaultNodeFontClick
      end
    end
    object TVSelectNodeImage: TTntMenuItem
      Caption = 'C&ustom icon...'
      Hint = 'Choose custom icon for selected node'
      OnClick = TVSelectNodeImageClick
    end
    object N80: TTntMenuItem
      Caption = '-'
    end
    object TVVirtualNode_: TTntMenuItem
      Caption = '&Virtual Node'
      object TVVirtualNode: TTntMenuItem
        Caption = 'Make &Virtual'
        Hint = 'Link a file on disk to selected node'
        OnClick = TVVirtualNodeClick
      end
      object TVRefreshVirtualNode: TTntMenuItem
        Caption = 'Re&fresh'
        Hint = 'Refresh contents from original file on disk'
        ShortCut = 16466
        OnClick = TVRefreshVirtualNodeClick
      end
      object N115: TTntMenuItem
        Caption = '-'
      end
      object TVInsertMirrorNode: TTntMenuItem
        Caption = 'Insert &Mirror node'
        Hint = 
          'Insert duplicate node as mirror node (virtual node linked to ano' +
          'ther KeyNote node)'
        OnClick = TVInsertMirrorNodeClick
      end
      object TVNavigateNonVirtualNode: TTntMenuItem
        Caption = 'Navigate to &Non Virtual'
        Hint = 
          'Navigate to the non virtual node to wich this mirror node is lin' +
          'ked'
        OnClick = TVNavigateNonVirtualNodeClick
      end
      object N90: TTntMenuItem
        Caption = '-'
      end
      object TVUnlinkVirtualNode: TTntMenuItem
        Caption = '&Unlink Node'
        Hint = 
          'Unlink the virtual node from file on disk or non virtual node (m' +
          'irror nodes)'
        OnClick = TVUnlinkVirtualNodeClick
      end
    end
    object N30: TTntMenuItem
      Caption = '-'
    end
    object TVMovenode_: TTntMenuItem
      Caption = '&Move Node'
      Hint = 'Move selected node in tree'
      object TVMoveNodeUp: TTntMenuItem
        Caption = '&Up'
        Hint = 'Move node UP'
        ShortCut = 8230
        OnClick = TVMoveNodeUpClick
      end
      object TVMoveNodeDown: TTntMenuItem
        Caption = '&Down'
        Hint = 'Move node DOWN'
        ShortCut = 8232
        OnClick = TVMoveNodeDownClick
      end
      object TVMoveNodeLeft: TTntMenuItem
        Caption = '&Left'
        Hint = 'Shift node LEFT by 1 level'
        ShortCut = 8229
        OnClick = TVMoveNodeLeftClick
      end
      object TVMoveNodeRight: TTntMenuItem
        Caption = '&Right'
        Hint = 'Shift node RIGHT by 1 level'
        ShortCut = 8231
        OnClick = TVMoveNodeRightClick
      end
    end
    object TVTransfer_: TTntMenuItem
      Caption = '&Transfer Subtree'
      Hint = 'Copy node and its children; then paste it in another tree'
      object TVCopySubtree: TTntMenuItem
        Caption = '&Lift Subtree'
        Hint = 'Copy selected node and its children'
        ShortCut = 16473
        OnClick = TVCopySubtreeClick
      end
      object TVGraftSubtree: TTntMenuItem
        Tag = 1
        Caption = '&Graft Subtree Here'
        Hint = 'Paste previously copied nodes at selected position'
        ShortCut = 16455
        OnClick = TVCopySubtreeClick
      end
      object TVGraftSubtreeMirror: TTntMenuItem
        Caption = 'Graft Subtree Here as &Mirror'
        Hint = 
          'Paste previously copied nodes at selected position as mirror nod' +
          'es'
        OnClick = TVGraftSubtreeMirrorClick
      end
      object N56: TTntMenuItem
        Caption = '-'
      end
      object TVEraseTreeMem: TTntMenuItem
        Tag = 2
        Caption = '&Erase Memory'
        Hint = 'Forget previously copied nodes'
        OnClick = TVCopySubtreeClick
      end
    end
    object TVExport: TTntMenuItem
      Caption = 'E&xport...'
      Hint = 'Export node contents to file'
      OnClick = TVExportClick
    end
    object N40: TTntMenuItem
      Caption = '-'
    end
    object TVDeleteNode: TTntMenuItem
      Caption = '&Delete Node'
      Hint = 'Delete selected node'
      ShortCut = 46
      OnClick = TVDeleteNodeClick
    end
    object TVDeleteChildren: TTntMenuItem
      Caption = 'Delete C&hildren'
      Hint = 'Delete child nodes of selected node'
      OnClick = TVDeleteChildrenClick
    end
    object N32: TTntMenuItem
      Caption = '-'
    end
    object TVRenameNode: TTntMenuItem
      Caption = '&Rename Node'
      Hint = 'Rename node'
      ShortCut = 32
      OnClick = MMRenamenodeClick
    end
    object TVCopyNode_: TTntMenuItem
      Caption = 'C&opy'
      object TVCopyNodeName: TTntMenuItem
        Caption = '&Node Name'
        Hint = 'Copy node name to clipboard'
        OnClick = TVCopyNodeNameClick
      end
      object TVCopyNodePath: TTntMenuItem
        Caption = 'Node &Path'
        Hint = 'Copy full node path to clipboard'
        ShortCut = 16464
        OnClick = TVCopyNodePathClick
      end
      object TVCopyNodeText: TTntMenuItem
        Caption = 'Node &Text'
        Hint = 'Copy full contents of node to clipboard'
        ShortCut = 16468
        OnClick = TVCopyNodeTextClick
      end
      object N88: TTntMenuItem
        Caption = '-'
      end
      object TVCopyPathtoEditor: TTntMenuItem
        Caption = 'Path to &Editor'
        Hint = 'Insert full node path in editor'
        ShortCut = 16453
        OnClick = TVCopyPathtoEditorClick
      end
    end
    object TVPasteNode_: TTntMenuItem
      Caption = '&Paste Name'
      object TVPasteNodeName: TTntMenuItem
        Caption = 'From &Clipboard'
        Hint = 'Use clipboard contents as selected node name'
        OnClick = TVPasteNodeNameClick
      end
      object N42: TTntMenuItem
        Caption = '-'
      end
      object TVPasteNodeNameAsDate: TTntMenuItem
        Caption = 'As &Date'
        Hint = 'Use current date as selected node name'
        ShortCut = 32836
        OnClick = TVPasteNodeNameClick
      end
      object TVPasteNodeNameAsTime: TTntMenuItem
        Caption = 'As &Time'
        Hint = 'Use current time as selected node name'
        ShortCut = 32852
        OnClick = TVPasteNodeNameClick
      end
      object TVPasteNodeNameAsDateTime: TTntMenuItem
        Caption = '&As Date and Time'
        Hint = 'Use current date and time as selected node name'
        ShortCut = 41028
        OnClick = TVPasteNodeNameClick
      end
      object N104: TTntMenuItem
        Caption = '-'
      end
      object TVPasteNodeNameAsSel: TTntMenuItem
        Caption = 'From &Selected Text'
        Hint = 'Rename node using selected text'
        OnClick = TVPasteNodeNameClick
      end
    end
    object N31: TTntMenuItem
      Caption = '-'
    end
    object TVSortNodes_: TTntMenuItem
      Caption = '&Sort'
      Hint = 'Sort nodes'
      object TVSortSubtree: TTntMenuItem
        Caption = '&Sort Subtree'
        Hint = 'Sort children of current node'
        ShortCut = 24661
        OnClick = TVSortSubtreeClick
      end
      object TVSortTree: TTntMenuItem
        Caption = 'Sort Entire &Tree'
        Hint = 'Sort all nodes in tree'
        ShortCut = 24659
        OnClick = TVSortTreeClick
      end
    end
  end
  object MRUMenu: TTntPopupMenu
    Left = 430
    Top = 80
    object N33: TTntMenuItem
      Caption = '-'
    end
    object MruM_MRUSeparatorBTN_: TTntMenuItem
      Caption = '&File Manager...'
      OnClick = MMFileManagerClick
    end
  end
  object IMG_TV: TImageList
    Left = 95
    Top = 170
    Bitmap = {
      494C01010A000E00040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0039393900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00393939007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000393939007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B003939390063636300636363006363
      6300313131007B7B7B007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B006B6B6B00C6C6C600C6C6
      C600636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF0000000000BDBD
      BD00FFFFFF00000000007B7B7B00000000007B7B7B00000000007B7B7B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE006B6B6B00FFFFFF006B6B6B00C6C6
      C600636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF0000000000BDBD
      BD0000000000BDBDBD007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B00FFFFFF00FFFFFF006B6B
      6B00636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B000000
      0000BDBDBD007B00000000000000FFFFFF00BDBDBD007B7B7B007B7B7B000000
      00007B7B7B00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00ADADAD007B7B7B007B7B7B007B7B
      7B00393939007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B007B7B
      7B00FFFFFF00FFFFFF007B000000FF00000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00C0C0C000C0C0
      C00080808000808080008080800080808000DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF007B7B
      7B00FFFFFF007B7B7B00FFFF00007B000000FFFF00007B00000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0080808000FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00C0C0C000C0C0C0008080
      80008080800080808000C0C0C000FFFF00007B000000BDBDBD007B0000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF000000
      000000FFFF0000FFFF0000FFFF0000FFFF0080808000DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0080808000FFFF00007B7B00007B7B0000FFFFFF007B00
      000000000000FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008080800000FFFF000000
      000000FFFF0000FFFF000000000000FFFF0080808000DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD0000FFFF000000000000FF
      FF0000FFFF0000FFFF0000FFFF0080808000FFFF00007B7B00007B7B0000FFFF
      FF007B00000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF000080
      800000000000000000000000000000FFFF0080808000FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD0000FFFF000000000000FF
      FF0000FFFF000000000000FFFF00808080007B7B7B00FFFF00007B7B00007B7B
      0000FFFFFF007B00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF0000FF
      FF0000000000000000000000000000FFFF0080808000DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD0000FFFF00008080000000
      0000000000000000000000FFFF0080808000FFFFFF007B7B7B00FFFF00007B7B
      00007B7B0000FFFFFF007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF000000
      000000000000000000000000000000FFFF0080808000DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD0000FFFF0000FFFF000000
      0000000000000000000000FFFF00808080007B7B7B00FFFFFF007B7B7B00FFFF
      00007B7B00007B7B0000FFFFFF007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0080808000FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B000000000000000000BDBDBD0000FFFF00000000000000
      0000000000000000000000FFFF0080808000FFFFFF00FFFFFF00FFFFFF007B7B
      7B00FFFF00007B7B00007B7B0000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000393939007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B0039393900000000000000000000000000BDBDBD0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0080808000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B7B7B00FFFF00007B7B00007B7B00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0039393900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00393939007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000393939007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B003939390063636300636363006363
      6300313131007B7B7B007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000000000007B7B0000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B006B6B6B00C6C6C600C6C6
      C600636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF0000000000BDBD
      BD00FFFFFF00000000007B7B7B00000000007B7B7B00000000007B7B7B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000007B7B00007B7B00007B7B0000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE006B6B6B00FFFFFF006B6B6B00C6C6
      C600636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF0000000000BDBD
      BD0000000000BDBDBD007B7B7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B00FFFFFF00FFFFFF006B6B
      6B00636363007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B000000
      0000BDBDBD007B00000000000000FFFFFF00BDBDBD007B7B7B007B7B7B000000
      00007B7B7B00FFFFFF00FFFFFF00000000007B7B7B00BDBDBD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00ADADAD007B7B7B007B7B7B007B7B
      7B00393939007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B007B7B
      7B00FFFFFF00FFFFFF007B000000FF00000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000007B7B7B00BDBDBD007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00BDBDBD007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF00FFFFFF007B7B
      7B00FFFFFF007B7B7B00FFFF00007B000000FFFF00007B00000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B007B7B7B00000000007B7B7B00BDBDBD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD007B7B7B000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B000000
      00007B7B7B00FFFFFF00BDBDBD00FFFF00007B000000BDBDBD007B0000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00007B
      0000BDBDBD007B7B7B007B7B7B00000000007B7B7B00BDBDBD007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00BDBDBD007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B007B7B
      7B0000000000FFFFFF007B7B7B00FFFF00007B7B00007B7B0000FFFFFF007B00
      000000000000FFFFFF00FFFFFF00000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF00000000007B7B
      7B007B7B7B00FFFFFF00FFFFFF007B7B7B00FFFF00007B7B00007B7B0000FFFF
      FF007B00000000000000FFFFFF0000000000000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00007B
      000000FF00007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00FFFFFF007B7B7B00FFFF00007B7B00007B7B
      0000FFFFFF007B000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B00FFFF
      FF00FFFFFF00FFFFFF007B7B7B007B7B7B00FFFFFF007B7B7B00FFFF00007B7B
      00007B7B0000FFFFFF007B000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B7B7B0000000000000000007B7B7B00FFFFFF00DEDE
      DE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00FFFF
      FF007B7B7B007B7B7B007B7B7B0000000000BDBDBD00FFFFFF007B7B7B007B7B
      7B00000000007B7B7B007B7B7B007B7B7B007B7B7B00FFFFFF007B7B7B00FFFF
      00007B7B00007B7B0000FFFFFF007B0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B0000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B000000000000000000BDBDBD00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B
      7B00FFFF00007B7B00007B7B0000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000393939007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B0039393900000000000000000000000000BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B7B7B00FFFF00007B7B00007B7B00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B007B007B007B007B007B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B007B00000000007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B007B0000000000000000007B007B007B007B007B007B007B007B007B00
      7B007B007B007B007B007B007B00000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B00000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000007B007B007B007B0000000000BDBDBD00BDBDBD00BDBDBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B007B00BDBDBD000000000000000000000000007B7B7B00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD007B007B00000000007B7B7B00FFFFFF0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF007B7B7B00000000000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF007B7B7B000000000000000000000000000000000000000000000000000000
      00007B007B007B007B007B007B00000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD000000000000000000000000000000000000000000000000007B00
      7B007B007B00BDBDBD0000000000BDBDBD00BDBDBD007B7B7B00000000000000
      000000000000BDBDBD007B007B00000000007B7B7B00FFFFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD007B7B7B000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD00000000007B7B7B0000000000000000000000000000000000000000007B00
      7B007B007B007B007B007B007B00BDBDBD0000000000000000007B7B7B00BDBD
      BD00BDBDBD00BDBDBD00000000000000000000000000000000007B007B007B00
      7B00BDBDBD00BDBDBD000000000000FFFF00BDBDBD00BDBDBD00BDBDBD0000FF
      FF007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF007B7B7B000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B
      7B000000000000000000000000000000000000000000000000007B007B007B00
      7B007B007B007B007B00BDBDBD007B007B007B007B007B007B00000000000000
      00007B7B7B00BDBDBD007B7B7B00BDBDBD0000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD0000000000BDBDBD00BDBDBD0000FFFF00BDBDBD00BDBD
      BD007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD007B7B7B0000000000000000007B7B7B00FFFFFF0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF000000
      000000000000BDBDBD000000000000000000000000007B007B007B007B007B00
      7B007B007B00BDBDBD007B007B007B007B007B007B007B007B007B007B007B00
      7B0000000000000000007B7B7B00BDBDBD0000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD000000000000FFFF00BDBDBD00BDBDBD00BDBDBD0000FF
      FF007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF007B7B7B0000000000000000007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B7B000000
      0000FF000000FFFFFF00BDBDBD0000000000000000007B007B007B007B007B00
      7B00BDBDBD007B007B007B007B007B007B007B007B007B007B007B007B007B00
      7B007B007B007B007B00000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD0000000000BDBDBD00BDBDBD0000FFFF00BDBDBD00BDBD
      BD007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD007B7B7B0000000000000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00FFFFFF00FFFFFF000000000000000000000000007B007B007B007B00BDBD
      BD007B007B007B007B007B007B007B007B007B007B007B007B007B007B007B00
      7B007B007B007B007B00000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD000000000000FFFF00BDBDBD00BDBDBD00BDBDBD0000FF
      FF007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF007B7B7B000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD007B7B7B00FFFFFF00FFFFFF00FF000000FFFFFF00FF000000FFFF
      FF00FFFFFF00000000000000000000000000000000007B007B00BDBDBD007B00
      7B007B007B007B007B007B007B007B007B007B007B007B007B007B007B007B00
      7B007B007B0000000000000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD0000000000BDBDBD00BDBDBD0000FFFF00BDBDBD00BDBD
      BD007B7B7B00BDBDBD007B007B00000000007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007B7B7B000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD0000FFFF00BDBDBD007B7B7B00FFFFFF00FFFFFF00FF000000FFFFFF00FFFF
      FF00000000007B7B7B00000000000000000000000000BDBDBD007B007B007B00
      7B007B007B007B007B007B007B007B007B007B007B007B007B007B007B007B00
      7B000000000000000000000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD00BDBDBD007B7B7B007B7B7B00BDBDBD00BDBDBD00BDBDBD0000FF
      FF007B7B7B000000000000000000000000007B7B7B00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD007B7B7B00FFFFFF00FFFFFF00FFFFFF000000
      00007B7B7B007B7B7B0000000000000000000000000000000000000000007B00
      7B007B007B007B007B007B007B007B007B007B007B007B007B007B007B000000
      00000000000000000000000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD007B7B7B0000000000000000007B7B7B007B7B7B00BDBDBD00BDBD
      BD007B7B7B00000000000000000000000000000000007B7B7B00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD007B7B7B000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF007B7B7B007B7B7B00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B007B007B007B007B007B007B007B007B007B00000000000000
      00000000000000000000000000000000000000000000000000007B007B00BDBD
      BD00BDBDBD007B7B7B00000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B0000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B007B007B007B0000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00BDBD
      BD007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000E001000000000000C001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      8001000000000000800100000000000080010000000000008001000000000000
      80030000000000008007000000000000E0010000FFFFFFFFC0010000FFFFFC3F
      80010000FFFF0100800100008003FC3F800100000001FF7F800100003FF0FE7F
      80010000000080038001000000003FF18001000000000000800100007FF80000
      800100008000000080010000C0017FF880010000FFFF800080010000FFFFC001
      80030000FFFFFFFF80070000FFFFFFFFFFFFFFFFFFFFF1FFFFFFFFFFFC7FE000
      8001E001F81FE0000001C001F007E0000001C001E001C00000018001C0008000
      0001800180008000000100090000800000010000000080000001000100018000
      0001800100038000000180010007800100038003800F830780FFC01FE01F83C7
      C1FFE1BFF83FC7FFFFFFFFFFFE7FEFFF00000000000000000000000000000000
      000000000000}
  end
  object Menu_Style: TTntPopupMenu
    Left = 295
    Top = 140
    object MSStyleApply: TTntMenuItem
      Caption = '&Apply Selected Style'
      Hint = 'Apply style to selection'
      OnClick = BtnStyleApplyClick
    end
    object N51: TTntMenuItem
      Caption = '-'
    end
    object MSStyleFont: TTntMenuItem
      Caption = 'Create &Font Style'
      GroupIndex = 1
      Hint = 'Create style for Font properties'
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object MSStylePara: TTntMenuItem
      Caption = 'Create &Paragraph Style'
      GroupIndex = 1
      Hint = 'Create style for Paragraph properties'
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object MSStyleBoth: TTntMenuItem
      Caption = '&Create Combined Style'
      GroupIndex = 1
      Hint = 'Create style for Font and Paragraph properties'
      RadioItem = True
      OnClick = Btn_StyleClick
    end
    object N50: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MSStyleDescribe: TTntMenuItem
      Tag = 93
      Caption = 'D&escribe'
      GroupIndex = 1
      Hint = 'Display properties for selected style'
      OnClick = BtnStyleApplyClick
    end
    object MSStyleRedef: TTntMenuItem
      Tag = 92
      Caption = 'Rede&fine'
      GroupIndex = 1
      Hint = 'Redefine style based on selection'
      OnClick = BtnStyleApplyClick
    end
    object N52: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MSStyleRename: TTntMenuItem
      Tag = 90
      Caption = '&Rename Style'
      GroupIndex = 1
      Hint = 'Rename style definition'
      OnClick = BtnStyleApplyClick
    end
    object MSStyleDelete: TTntMenuItem
      Tag = 91
      Caption = '&Delete Style'
      GroupIndex = 1
      Hint = 'Delete style definition'
      OnClick = BtnStyleApplyClick
    end
  end
  object Menu_Macro: TTntPopupMenu
    Left = 290
    Top = 350
    object MacMMacro_Play: TTntMenuItem
      Caption = '&Play Macro'
      Hint = 'Run selected macro'
      ShortCut = 13
      OnClick = TB_MacroClick
    end
    object N66: TTntMenuItem
      Caption = '-'
    end
    object MacMMacro_Edit: TTntMenuItem
      Caption = '&Edit Macro'
      Hint = 'Rename selected macro'
      ShortCut = 32
      OnClick = MacMMacro_EditClick
    end
    object MacMMacro_Delete: TTntMenuItem
      Caption = '&Delete Macro'
      Hint = 'Delete selected macro'
      ShortCut = 46
      OnClick = MacMMacro_DeleteClick
    end
    object N78: TTntMenuItem
      Caption = '-'
    end
    object MacMMacro_Record: TTntMenuItem
      Caption = '&Record Macro...'
      Hint = 'Record a new macro'
      ShortCut = 45
      OnClick = TB_MacroRecordClick
    end
    object MacMMacroUserCommand: TTntMenuItem
      Caption = '&User Command...'
      Hint = 'Add user command to macro'
      ShortCut = 187
      OnClick = MacMMacroUserCommandClick
    end
  end
  object Menu_Template: TTntPopupMenu
    Left = 225
    Top = 350
    object TPLMTplInsert: TTntMenuItem
      Caption = '&Insert Template'
      Hint = 'Insert selected template in active note'
      ShortCut = 13
      OnClick = TPLMTplInsertClick
    end
    object N83: TTntMenuItem
      Caption = '-'
    end
    object TPLMTplCreate: TTntMenuItem
      Caption = '&Create Template...'
      Hint = 'Create a new template based on active note text'
      ShortCut = 45
      OnClick = MMToolsTemplateCreateClick
    end
    object TPLMTplDelete: TTntMenuItem
      Caption = '&Delete Template'
      Hint = 'Delete selected template'
      ShortCut = 46
      OnClick = TPLMTplDeleteClick
    end
  end
  object Menu_Plugins: TTntPopupMenu
    Left = 155
    Top = 350
    object PLM_RunPlugin: TTntMenuItem
      Caption = '&Run Plugin'
      Hint = 'Execute selected plugin'
      ShortCut = 13
      OnClick = PLM_RunPluginClick
    end
    object PLM_ConfigurePlugin: TTntMenuItem
      Caption = '&Configure Plugin'
      Hint = 'Configure selected plugin'
      ShortCut = 32
      OnClick = PLM_ConfigurePluginClick
    end
    object N64: TTntMenuItem
      Caption = '-'
    end
    object PLM_ReloadPlugins: TTntMenuItem
      Caption = 'R&efresh List'
      Hint = 'Re-read plugin information'
      OnClick = PLM_ReloadPluginsClick
    end
  end
  object Menu_ResPanel: TTntPopupMenu
    OnPopup = Menu_ResPanelPopup
    Left = 240
    Top = 285
    object ResMHidepanel: TTntMenuItem
      Caption = 'Hide &Resource Panel'
      Hint = 'Show or hide Resource panel (press F9)'
      OnClick = MMViewResPanelClick
    end
    object N71: TTntMenuItem
      Caption = '-'
    end
    object ResMMultilineTabs: TTntMenuItem
      Caption = 'M&ultiline Tabs'
      Hint = 'Arrange tabs in rows'
      OnClick = ResMMultilineTabsClick
    end
    object ResMTabPosition: TTntMenuItem
      Caption = 'T&ab Position'
      object ResMTop: TTntMenuItem
        Caption = '&Top'
        GroupIndex = 1
        Hint = 'Place tabs on top'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMBottom: TTntMenuItem
        Tag = 1
        Caption = '&Bottom'
        GroupIndex = 1
        Hint = 'Place tabs at bottom'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMLeft: TTntMenuItem
        Tag = 2
        Caption = '&Left'
        GroupIndex = 1
        Hint = 'Place tabs left'
        RadioItem = True
        OnClick = ResMRightClick
      end
      object ResMRight: TTntMenuItem
        Tag = 3
        Caption = '&Right'
        GroupIndex = 1
        Hint = 'Place tabs right'
        RadioItem = True
        OnClick = ResMRightClick
      end
    end
    object ResMPanelPosition: TTntMenuItem
      Caption = 'Pa&nel Position'
      object ResMPanelLeft: TTntMenuItem
        Caption = '&Left'
        GroupIndex = 2
        RadioItem = True
        OnClick = ResMPanelRightClick
      end
      object ResMPanelRight: TTntMenuItem
        Tag = 1
        Caption = '&Right'
        GroupIndex = 2
        RadioItem = True
        OnClick = ResMPanelRightClick
      end
    end
    object N81: TTntMenuItem
      Caption = '-'
    end
    object ResMFindTab: TTntMenuItem
      Caption = '&Find Tab'
      Hint = 'Show or hide the "Find" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMScratchTab: TTntMenuItem
      Tag = 1
      Caption = '&Scratch Tab'
      Hint = 'Show or hide the "Scratch" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMMacroTab: TTntMenuItem
      Tag = 2
      Caption = '&Macros Tab'
      Hint = 'Show or hide the "Macro" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMTemplateTab: TTntMenuItem
      Tag = 3
      Caption = '&Templates Tab'
      Hint = 'Show or hide the "Template" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMPluginTab: TTntMenuItem
      Tag = 4
      Caption = '&Plugins Tab'
      Hint = 'Show or hide the "Plugin" tab'
      OnClick = ResMPluginTabClick
    end
    object ResMFavTab: TTntMenuItem
      Tag = 5
      Caption = 'Fa&vorites Tab'
      Hint = 'Show or hide the "Favorites" tab'
      OnClick = ResMPluginTabClick
    end
  end
  object Menu_StdEdit: TTntPopupMenu
    OnPopup = Menu_StdEditPopup
    Left = 415
    Top = 200
    object StdEMUndo: TTntMenuItem
      Caption = '&Undo'
      Hint = 'Undo last editing operation'
      ShortCut = 16474
      OnClick = StdEMSelectAllClick
    end
    object N85: TTntMenuItem
      Caption = '-'
    end
    object StdEMCut: TTntMenuItem
      Tag = 1
      Caption = 'Cu&t'
      Hint = 'Cut selection to clipboard (Ctrl+X, Shift+Supr)'
      OnClick = StdEMSelectAllClick
    end
    object StdEMCopy: TTntMenuItem
      Tag = 2
      Caption = '&Copy'
      Hint = 'Copy selection to clipboard (Ctrl+C, Ctrl+Ins)'
      OnClick = StdEMSelectAllClick
    end
    object StdEMPaste: TTntMenuItem
      Tag = 3
      Caption = '&Paste'
      Hint = 'Paste text from clipboard (Ctrl+V, Shift+Ins)'
      OnClick = StdEMSelectAllClick
    end
    object StdEMPastePlain: TTntMenuItem
      Tag = 4
      Caption = 'Paste as Text'
      Hint = 'Paste text without formatting'
      ShortCut = 16468
      OnClick = StdEMSelectAllClick
    end
    object StdEMDelete: TTntMenuItem
      Tag = 5
      Caption = '&Delete'
      Hint = 'Delete selection'
      OnClick = StdEMSelectAllClick
    end
    object N86: TTntMenuItem
      Caption = '-'
    end
    object StdEMSelectAll: TTntMenuItem
      Tag = 6
      Caption = 'Select &All'
      Hint = 'Select all text'
      ShortCut = 16449
      OnClick = StdEMSelectAllClick
    end
    object StdEMWordWrap: TTntMenuItem
      Tag = 7
      Caption = 'Word Wrap'
      ShortCut = 16471
      OnClick = StdEMSelectAllClick
    end
  end
  object Menu_FindAll: TTntPopupMenu
    Left = 85
    Top = 350
    object FAMCopytoEditor: TTntMenuItem
      Caption = '&Insert in Note'
      Hint = 'Insert selected location as hyperlink in note'
      OnClick = FAMCopytoEditorClick
    end
    object FAMCopyAlltoEditor: TTntMenuItem
      Caption = 'Insert &All in Note'
      Hint = 'Insert all locations as hyperlinks in note'
      OnClick = FAMCopyAlltoEditorClick
    end
  end
  object Menu_Fav: TTntPopupMenu
    Left = 350
    Top = 350
    object FavMJump: TTntMenuItem
      Caption = '&Jump to Location'
      Hint = 'Jump to selected Favorite location'
      ShortCut = 13
      OnClick = FavMJumpClick
    end
    object N89: TTntMenuItem
      Caption = '-'
    end
    object FavMAdd: TTntMenuItem
      Caption = '&Add Location'
      Hint = 'Add current location as Favorite'
      ShortCut = 45
      OnClick = FavMAddClick
    end
    object FavMAddExternal: TTntMenuItem
      Caption = 'Add &External...'
      Hint = 'Add external program or document as favorite'
      ShortCut = 8237
      OnClick = FavMAddExternalClick
    end
    object N113: TTntMenuItem
      Caption = '-'
    end
    object FavMProperties: TTntMenuItem
      Caption = '&Properties...'
      ShortCut = 32
      OnClick = FavMPropertiesClick
    end
    object N91: TTntMenuItem
      Caption = '-'
    end
    object FavMDel: TTntMenuItem
      Caption = '&Delete Location'
      Hint = 'Delete selected location from Favorites'
      ShortCut = 46
      OnClick = FavMDelClick
    end
    object N38: TTntMenuItem
      Caption = '-'
    end
    object FavMRef: TTntMenuItem
      Caption = 'Refresh &List'
      Hint = 'Reload Favorites from disk'
      OnClick = FavMRefClick
    end
  end
  object Menu_Numbers: TTntPopupMenu
    Left = 415
    Top = 285
    object MMArabicNumbers: TTntMenuItem
      Caption = '&Arabic Numbers'
      Checked = True
      Hint = 'Select Arabic number numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object N94: TTntMenuItem
      Caption = '-'
    end
    object MMLoLetter: TTntMenuItem
      Caption = '&Lowercase Letter'
      Hint = 'Select lowercase letter numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object MMUpLetter: TTntMenuItem
      Caption = '&Uppercase Letter'
      Hint = 'Select uppercase letter numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object N95: TTntMenuItem
      Caption = '-'
    end
    object MMLoRoman: TTntMenuItem
      Caption = 'Lowercase &Roman'
      Hint = 'Select lowercase Roman numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object MMUpRoman: TTntMenuItem
      Caption = 'U&ppercase Roman'
      Hint = 'Select uppercase Roman numbering style'
      RadioItem = True
      OnClick = MMUpRomanClick
    end
    object NU01: TTntMenuItem
      Caption = '-'
    end
    object MMRightParenthesis: TTntMenuItem
      Caption = 'Right parenthesis'
      GroupIndex = 1
      Hint = 'Follows the number with a right parenthesis'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMEnclosed: TTntMenuItem
      Caption = 'Enclosed in parenthesis'
      GroupIndex = 1
      Hint = 'Encloses the number in parentheses'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMPeriod: TTntMenuItem
      Caption = 'Period'
      Checked = True
      GroupIndex = 1
      Hint = 'Follows the number with a period'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object MMOnlyNumber: TTntMenuItem
      Caption = 'Only number'
      GroupIndex = 1
      Hint = 'Displays only the number'
      RadioItem = True
      OnClick = MMRightParenthesisClick
    end
    object NU02: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object MMWithoutNextNumber: TTntMenuItem
      Caption = 'No Next number or bullet'
      GroupIndex = 2
      Hint = 
        'Continues a numbered list without applying the next number or bu' +
        'llet'
      OnClick = MMRightParenthesisClick
    end
    object MMStartsNewNumber: TTntMenuItem
      Caption = 'Starts New number...'
      GroupIndex = 2
      Hint = 'Selects a new starting number'
      OnClick = MMStartsNewNumberClick
    end
  end
  object Menu_Paste: TTntPopupMenu
    Left = 480
    Top = 80
    object MMP_Paste: TTntMenuItem
      Caption = '&Paste'
      Hint = 'Paste from clipboard'
      OnClick = MMEditPasteClick
    end
    object MMP_PastePlain: TTntMenuItem
      Caption = 'Paste as &Text'
      Hint = 'Paste fro clipboard as plain text'
      OnClick = MMEditPasteAsTextClick
    end
    object MMP_PasteAsWebClip: TTntMenuItem
      Caption = 'Paste as &Web Clip'
      OnClick = MMEditPasteAsWebClipClick
    end
    object MMP_PasteSpecial: TTntMenuItem
      Caption = 'Paste &Special...'
      Hint = 'Select special format to paste'
      OnClick = MMEditPasteSpecialClick
    end
    object N107: TTntMenuItem
      Caption = '-'
    end
    object MMP_PasteAsNote: TTntMenuItem
      Caption = 'Paste &Into New Note'
      Hint = 'Create a new note and paste text from clipboard'
      OnClick = MMEditPasteAsNewNoteClick
    end
    object MMP_PasteAsNode: TTntMenuItem
      Caption = 'Paste Into New &Node'
      Hint = 'Create a new tree node and paste text from clipboard'
      OnClick = MMEditPasteAsNewNodeClick
    end
  end
  object Menu_Date: TTntPopupMenu
    OnPopup = Menu_DatePopup
    Left = 40
    Top = 295
    object md1: TTntMenuItem
      Tag = 1
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object N110: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object md2: TTntMenuItem
      Tag = 2
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md3: TTntMenuItem
      Tag = 3
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md4: TTntMenuItem
      Tag = 4
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object N111: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object md5: TTntMenuItem
      Tag = 5
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md6: TTntMenuItem
      Tag = 6
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md7: TTntMenuItem
      Tag = 7
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md8: TTntMenuItem
      Tag = 8
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md9: TTntMenuItem
      Tag = 9
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md10: TTntMenuItem
      Tag = 10
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md11: TTntMenuItem
      Tag = 11
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md12: TTntMenuItem
      Tag = 12
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md13: TTntMenuItem
      Tag = 13
      Break = mbBarBreak
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md14: TTntMenuItem
      Tag = 14
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md15: TTntMenuItem
      Tag = 15
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md16: TTntMenuItem
      Tag = 16
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md17: TTntMenuItem
      Tag = 17
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md18: TTntMenuItem
      Tag = 18
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md19: TTntMenuItem
      Tag = 19
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md20: TTntMenuItem
      Tag = 20
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md21: TTntMenuItem
      Tag = 21
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md22: TTntMenuItem
      Tag = 22
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md23: TTntMenuItem
      Tag = 23
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md24: TTntMenuItem
      Tag = 24
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
    object md25: TTntMenuItem
      Tag = 25
      GroupIndex = 1
      RadioItem = True
      OnClick = md25Click
    end
  end
  object Menu_Time: TTntPopupMenu
    OnPopup = Menu_TimePopup
    Left = 100
    Top = 295
    object mt1: TTntMenuItem
      Tag = 1
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object N108: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object mt2: TTntMenuItem
      Tag = 2
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt3: TTntMenuItem
      Tag = 3
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object N109: TTntMenuItem
      Caption = '-'
      GroupIndex = 1
      RadioItem = True
    end
    object mt4: TTntMenuItem
      Tag = 4
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt5: TTntMenuItem
      Tag = 5
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt6: TTntMenuItem
      Tag = 6
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt7: TTntMenuItem
      Tag = 7
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt8: TTntMenuItem
      Tag = 8
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
    object mt9: TTntMenuItem
      Tag = 9
      GroupIndex = 1
      RadioItem = True
      OnClick = mt8Click
    end
  end
  object Menu_Symbols: TTntPopupMenu
    OnPopup = Menu_SymbolsPopup
    Left = 160
    Top = 295
    object ms1: TTntMenuItem
      Tag = 1
      OnClick = ms11Click
    end
    object ms2: TTntMenuItem
      Tag = 2
      OnClick = ms11Click
    end
    object ms3: TTntMenuItem
      Tag = 3
      OnClick = ms11Click
    end
    object ms4: TTntMenuItem
      Tag = 4
      OnClick = ms11Click
    end
    object ms5: TTntMenuItem
      Tag = 5
      OnClick = ms11Click
    end
    object ms6: TTntMenuItem
      Tag = 6
      OnClick = ms11Click
    end
    object ms7: TTntMenuItem
      Tag = 7
      OnClick = ms11Click
    end
    object ms8: TTntMenuItem
      Tag = 8
      OnClick = ms11Click
    end
    object ms9: TTntMenuItem
      Tag = 9
      OnClick = ms11Click
    end
    object ms10: TTntMenuItem
      Tag = 10
      OnClick = ms11Click
    end
  end
  object Img_System: TdfsSystemImageList
    AllocBy = 32
    ShareImages = True
    ImageSize = isSmall
    Left = 180
    Top = 240
  end
end
