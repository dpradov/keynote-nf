object Form_Alarm: TForm_Alarm
  Left = 330
  Top = 208
  HelpContext = 590
  Caption = 'p'
  ClientHeight = 407
  ClientWidth = 991
  Color = clBtnFace
  Constraints.MinHeight = 416
  Constraints.MinWidth = 994
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    991
    407)
  TextHeight = 13
  object lblFilter: TLabel
    Left = 629
    Top = 9
    Width = 63
    Height = 13
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Filter:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitLeft = 615
  end
  object Button_ClearFilter: TToolbarButton97
    Left = 892
    Top = 6
    Width = 17
    Height = 21
    Hint = 'Clear Filter'
    Anchors = [akTop, akRight]
    Caption = 'X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ImageIndex = 0
    ParentFont = False
    OnClick = Button_ClearFilterClick
    ExplicitLeft = 878
  end
  object TntLabel2: TLabel
    Left = 10
    Top = 9
    Width = 93
    Height = 13
    AutoSize = False
    Caption = 'Show mode:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button_Sound: TToolbarButton97
    Left = 948
    Top = 4
    Width = 25
    Height = 24
    Hint = 'Enable or disable sound when alarm goes off'
    AllowAllUp = True
    Anchors = [akTop, akRight]
    GroupIndex = 1
    Flat = False
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 50
    Images = Form_Main.IMG_Toolbar
    OnClick = Button_SoundClick
    ExplicitLeft = 934
  end
  object TB_ClipCap: TToolbarButton97
    Left = 922
    Top = 4
    Width = 25
    Height = 24
    Hint = 'Copy selected alarms to the clipboard'
    Anchors = [akTop, akRight]
    Flat = False
    Glyph.Data = {00000000}
    GlyphMask.Data = {00000000}
    ImageIndex = 18
    Images = Form_Main.IMG_Toolbar
    OnClick = TB_ClipCapClick
    ExplicitLeft = 908
  end
  object Panel3: TPanel
    Left = 0
    Top = 120
    Width = 991
    Height = 287
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 119
    ExplicitWidth = 969
    DesignSize = (
      991
      287)
    object Bevel1: TBevel
      Left = 6
      Top = 35
      Width = 765
      Height = 246
      Anchors = [akLeft, akTop, akRight]
      Shape = bsFrame
      ExplicitWidth = 774
    end
    object lblSubject: TLabel
      Left = 460
      Top = 80
      Width = 46
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Subject:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblExpiration: TLabel
      Left = 18
      Top = 79
      Width = 141
      Height = 13
      Alignment = taRightJustify
      Caption = 'Event or Expiration Time:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblExpirationStatus: TLabel
      Left = 32
      Top = 119
      Width = 201
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblReminder: TLabel
      Left = 334
      Top = 80
      Width = 87
      Height = 13
      Caption = 'Next Reminder:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblReminderStatus: TLabel
      Left = 322
      Top = 119
      Width = 119
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Today_8AM: TToolbarButton97
      Left = 16
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '8 AM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Label3: TLabel
      Left = 46
      Top = 227
      Width = 122
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Today at:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Today_12AM: TToolbarButton97
      Left = 56
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '12 AM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_3PM: TToolbarButton97
      Left = 96
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '3 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_6PM: TToolbarButton97
      Left = 137
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '6 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_8PM: TToolbarButton97
      Left = 178
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '8 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Label1: TLabel
      Left = 232
      Top = 227
      Width = 200
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Tomorrow at:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Tomorrow_8AM: TToolbarButton97
      Left = 231
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '8 AM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Tomorrow_12AM: TToolbarButton97
      Left = 272
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '12 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Tomorrow_3PM: TToolbarButton97
      Left = 313
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '3 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Tomorrow_6PM: TToolbarButton97
      Left = 354
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '6 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Tomorrow_8PM: TToolbarButton97
      Left = 395
      Top = 248
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '8 PM'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_5min: TToolbarButton97
      Left = 17
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '5 min'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_10min: TToolbarButton97
      Left = 70
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '10 min'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_15min: TToolbarButton97
      Left = 123
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '15 min'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_30min: TToolbarButton97
      Left = 177
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '30 min'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_1h: TToolbarButton97
      Left = 230
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '1 h'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_2h: TToolbarButton97
      Left = 284
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '2 h'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_3h: TToolbarButton97
      Left = 339
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '3 h'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object Today_5h: TToolbarButton97
      Left = 394
      Top = 199
      Width = 38
      Height = 21
      AllowAllUp = True
      GroupIndex = 1
      Caption = '5 h'
      ImageIndex = 37
      OldDisabledStyle = True
      RepeatInterval = 101
      ShowBorderWhenInactive = True
      OnClick = Today_5minClick
      OnDblClick = Today_5minDblClick
    end
    object TntLabel3: TLabel
      Left = 18
      Top = 143
      Width = 114
      Height = 13
      Caption = 'Proposed Reminder:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = cl3DDkShadow
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblProposedReminder: TLabel
      Left = 144
      Top = 142
      Width = 249
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object PanelCalendar: TPanel
      Left = 784
      Top = 0
      Width = 195
      Height = 287
      Anchors = [akTop, akRight]
      BevelEdges = [beRight]
      BevelOuter = bvNone
      TabOrder = 16
      ExplicitLeft = 762
      object cCalendar: TMonthCalendar
        Left = -11
        Top = 113
        Width = 215
        Height = 160
        MultiSelect = True
        Date = 40429.000000000000000000
        EndDate = 40429.000000000000000000
        TabOrder = 0
        OnClick = cCalendarClick
        OnExit = cCalendarExit
        OnGetMonthInfo = cCalendarGetMonthInfo
      end
      object CB_FilterDates: TComboBox
        Left = 1
        Top = 81
        Width = 188
        Height = 21
        HelpContext = 535
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = CB_FilterDatesChange
      end
    end
    object Button_Restore: TButton
      Left = 114
      Top = 0
      Width = 100
      Height = 25
      Hint = 'Restore the discarded alarms'
      Caption = 'Res&tore'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = Button_RestoreClick
    end
    object Button_SelectAll: TButton
      Left = 876
      Top = 0
      Width = 97
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Select All'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = Button_SelectAllClick
    end
    object Button_Remove: TButton
      Left = 10
      Top = 0
      Width = 96
      Height = 25
      Hint = 'Remove selected alarms (only if discarded)'
      Caption = '&Remove'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = Button_RemoveClick
    end
    object Button_Show: TButton
      Left = 285
      Top = 0
      Width = 99
      Height = 25
      Hint = 'Show location of alarm'
      Caption = '&Show'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = Button_ShowClick
    end
    object cIdentifier: TEdit
      Left = 16
      Top = 42
      Width = 744
      Height = 22
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 18
      Text = 'NODO'
      ExplicitWidth = 722
    end
    object txtSubject: TMemo
      Left = 459
      Top = 96
      Width = 301
      Height = 141
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 11
      WantTabs = True
      OnChange = txtSubjectChange
      ExplicitWidth = 279
    end
    object Button_New: TButton
      Left = 400
      Top = 0
      Width = 95
      Height = 25
      Hint = 
        'Create new alarm (in the same node/note that the item selected o' +
        'r in the active note, if no one is selected)'
      Caption = '&New'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = Button_NewClick
    end
    object Button_Discard: TButton
      Left = 674
      Top = 0
      Width = 97
      Height = 25
      Hint = 'Discard selected alarms'
      Anchors = [akTop, akRight]
      Caption = '&Discard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = Button_DiscardClick
    end
    object Button_Apply: TButton
      Left = 458
      Top = 246
      Width = 99
      Height = 25
      Caption = '&Apply'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      OnClick = Button_ApplyClick
    end
    object CB_ExpirationTime: TComboBox
      Left = 260
      Top = 95
      Width = 62
      Height = 21
      HelpContext = 535
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      TabStop = False
      OnCloseUp = CB_ExpirationTimeCloseUp
      OnDropDown = CB_ExpirationTimeDropDown
      OnSelect = CB_ExpirationTimeSelect
    end
    object cExpirationTime: TEdit
      Left = 263
      Top = 97
      Width = 40
      Height = 16
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      OnExit = cExpirationTimeExit
    end
    object CB_ExpirationDate: TDateTimePicker
      Left = 38
      Top = 95
      Width = 218
      Height = 21
      Date = 39404.000000000000000000
      Time = 39404.000000000000000000
      Checked = False
      DateFormat = dfLong
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnChange = CB_ExpirationDateChange
    end
    object chk_Expiration: TCheckBox
      Left = 17
      Top = 96
      Width = 17
      Height = 17
      TabOrder = 7
      OnClick = chk_ExpirationClick
    end
    object cReminder: TEdit
      Left = 334
      Top = 97
      Width = 115
      Height = 19
      TabStop = False
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      TabOrder = 12
      Text = 'cReminder'
    end
    object CB_ProposedIntervalReminder: TComboBox
      Left = 16
      Top = 166
      Width = 95
      Height = 21
      HelpContext = 535
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 13
      TabStop = False
      OnChange = CB_ProposedIntervalReminderChange
      OnExit = CB_ProposedIntervalReminderExit
    end
    object rb_Before: TRadioButton
      Left = 122
      Top = 165
      Width = 97
      Height = 26
      Caption = 'Before event'
      TabOrder = 14
      OnClick = rb_FromNowClick
    end
    object rb_FromNow: TRadioButton
      Left = 218
      Top = 165
      Width = 113
      Height = 26
      Caption = 'From now'
      TabOrder = 15
      OnClick = rb_FromNowClick
    end
    object chk_AppyOnExit: TCheckBox
      Left = 563
      Top = 252
      Width = 83
      Height = 17
      Hint = 
        'Automatically apply pending changes on exit (for example pressin' +
        'g ESC)'
      Caption = 'Apply on exit'
      TabOrder = 17
    end
  end
  object Grid: TListView
    Left = 8
    Top = 31
    Width = 965
    Height = 77
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Note'
        Width = 107
      end
      item
        Caption = 'Node'
        Width = 207
      end
      item
        Caption = 'Expirat./Start  Date'
        Width = 108
      end
      item
        Caption = 'Time'
      end
      item
        Caption = 'Subject'
        Width = 275
      end
      item
        Caption = 'Reminder Date'
        Width = 86
      end
      item
        Caption = 'Time'
        Width = 44
      end
      item
        Caption = 'Disc.'
        Width = 38
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    GridLines = True
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 2
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = GridAdvancedCustomDrawItem
    OnAdvancedCustomDrawSubItem = GridAdvancedCustomDrawSubItem
    OnColumnClick = GridColumnClick
    OnDblClick = GridDblClick
    OnEnter = GridEnter
    OnSelectItem = GridSelectItem
    ExplicitWidth = 943
    ExplicitHeight = 76
  end
  object cFilter: TEdit
    Left = 694
    Top = 5
    Width = 196
    Height = 21
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnChange = cFilterChange
    OnExit = cFilterExit
    ExplicitLeft = 672
  end
  object CB_ShowMode: TComboBox
    Left = 103
    Top = 6
    Width = 146
    Height = 21
    HelpContext = 535
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnChange = CB_ShowModeChange
  end
  object Panel1: TPanel
    Left = 652
    Top = 370
    Width = 111
    Height = 23
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 4
    object TB_Bold: TToolbarButton97
      Left = 3
      Top = 0
      Width = 24
      Height = 22
      AllowAllUp = True
      GroupIndex = 5
      Glyph.Data = {00000000}
      GlyphMask.Data = {00000000}
      ImageIndex = 0
      Images = Form_Main.IMG_Format
      RepeatInterval = 101
      OnClick = TB_BoldClick
    end
    object TB_Color: TColorBtn
      Left = 34
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
      Left = 74
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
  end
end
