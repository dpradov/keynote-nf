object Form_Alarm: TForm_Alarm
  Left = 330
  Top = 208
  HelpContext = 567
  Margins.Left = 0
  Caption = 'p'
  ClientHeight = 540
  ClientWidth = 804
  Color = clBtnFace
  Constraints.MinHeight = 527
  Constraints.MinWidth = 617
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    804
    540)
  TextHeight = 13
  object lblFilter: TLabel
    Left = 451
    Top = 8
    Width = 67
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Filter:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitLeft = 259
  end
  object Button_ClearFilter: TToolbarButton97
    Left = 700
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
    ExplicitLeft = 813
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
    Left = 756
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
    ExplicitLeft = 869
  end
  object TB_ClipCap: TToolbarButton97
    Left = 730
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
    ExplicitLeft = 843
  end
  object Panel: TPanel
    Left = 1
    Top = 134
    Width = 792
    Height = 405
    Anchors = [akLeft, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 133
    ExplicitWidth = 788
    DesignSize = (
      792
      405)
    object PanelAlarm: TPanel
      Left = 6
      Top = 39
      Width = 578
      Height = 359
      Anchors = [akLeft, akRight, akBottom]
      Constraints.MinWidth = 574
      TabOrder = 2
      ExplicitWidth = 574
      DesignSize = (
        578
        359)
      object lblExpiration: TLabel
        Left = 12
        Top = 150
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
        Left = 124
        Top = 194
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
        Left = 344
        Top = 151
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
        Left = 348
        Top = 194
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
        Left = 41
        Top = 327
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
      object LblToday: TLabel
        Left = 71
        Top = 306
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
        Left = 81
        Top = 327
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
        Left = 121
        Top = 327
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
        Left = 162
        Top = 327
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
        Left = 203
        Top = 327
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
      object LblTomorrow: TLabel
        Left = 257
        Top = 306
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
        Left = 256
        Top = 327
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
        Left = 297
        Top = 327
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
        Left = 338
        Top = 327
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
        Left = 379
        Top = 327
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
        Left = 421
        Top = 327
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
        Left = 42
        Top = 276
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
        Left = 95
        Top = 276
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
        Left = 148
        Top = 276
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
        Left = 202
        Top = 276
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
        Left = 255
        Top = 276
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
        Left = 309
        Top = 276
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
        Left = 364
        Top = 276
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
        Left = 419
        Top = 276
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
        Left = 12
        Top = 219
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
        Left = 138
        Top = 219
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
      object CB_ExpirationTime: TComboBox
        Left = 263
        Top = 170
        Width = 62
        Height = 21
        HelpContext = 535
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        TabStop = False
        OnCloseUp = CB_ExpirationTimeCloseUp
        OnDropDown = CB_ExpirationTimeDropDown
        OnSelect = CB_ExpirationTimeSelect
      end
      object cExpirationTime: TEdit
        Left = 266
        Top = 172
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
        TabOrder = 1
        OnExit = cExpirationTimeExit
      end
      object CB_ExpirationDate: TDateTimePicker
        Left = 41
        Top = 170
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
        TabOrder = 2
        OnChange = CB_ExpirationDateChange
      end
      object chk_Expiration: TCheckBox
        Left = 20
        Top = 171
        Width = 17
        Height = 17
        TabOrder = 3
        OnClick = chk_ExpirationClick
      end
      object cReminder: TEdit
        Left = 351
        Top = 172
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
        TabOrder = 4
        Text = 'cReminder'
      end
      object CB_ProposedIntervalReminder: TComboBox
        Left = 41
        Top = 243
        Width = 95
        Height = 21
        HelpContext = 535
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        TabStop = False
        OnChange = CB_ProposedIntervalReminderChange
        OnExit = CB_ProposedIntervalReminderExit
      end
      object rb_Before: TRadioButton
        Left = 156
        Top = 242
        Width = 100
        Height = 26
        Caption = 'Before event'
        TabOrder = 6
        OnClick = rb_FromNowClick
      end
      object rb_FromNow: TRadioButton
        Left = 263
        Top = 242
        Width = 116
        Height = 26
        Caption = 'From now'
        TabOrder = 7
        OnClick = rb_FromNowClick
      end
      object Button_Apply: TButton
        Left = 479
        Top = 172
        Width = 84
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Apply'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 8
        OnClick = Button_ApplyClick
        ExplicitLeft = 475
      end
      object chk_ApplyOnExitChange: TCheckBox
        Left = 486
        Top = 202
        Width = 85
        Height = 27
        Hint = 
          'Automatically apply pending changes on exit (ex. pressing ESC) a' +
          'nd on selection change'#13#10#13#10'Note: Double Click on Reminder buttons' +
          ', also apply changes'
        Anchors = [akTop, akRight]
        Caption = 'Apply Auto'
        Checked = True
        State = cbChecked
        TabOrder = 9
        WordWrap = True
        OnClick = chk_ApplyOnExitChangeClick
        ExplicitLeft = 482
      end
      object txtSubject: TMemo
        Left = 9
        Top = 40
        Width = 554
        Height = 87
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 10
        WantTabs = True
        OnChange = txtSubjectChange
        ExplicitWidth = 550
      end
      object cIdentifier: TEdit
        Left = 10
        Top = 12
        Width = 551
        Height = 22
        TabStop = False
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentCtl3D = False
        ParentFont = False
        ReadOnly = True
        TabOrder = 11
        Text = 'NODO'
        ExplicitWidth = 547
      end
      object PanelFormat: TPanel
        Left = 457
        Top = 128
        Width = 119
        Height = 23
        Anchors = [akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 12
        ExplicitLeft = 453
        object TB_Bold: TToolbarButton97
          Left = 0
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
      object btnExpandWindow: TButton
        Left = 530
        Top = 330
        Width = 33
        Height = 22
        Hint = 
          'Change width, to include or not full grid size and calendar filt' +
          'er'
        Anchors = [akTop, akRight]
        Caption = '<<'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 13
        OnClick = btnExpandWindowClick
        ExplicitLeft = 526
      end
    end
    object PanelCalendar: TPanel
      Left = 595
      Top = 44
      Width = 195
      Height = 204
      Anchors = [akRight, akBottom]
      BevelEdges = [beRight]
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 591
      object cCalendar: TMonthCalendar
        Left = -11
        Top = 37
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
        Top = 5
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
    object pnlButtons: TPanel
      Left = 2
      Top = -1
      Width = 783
      Height = 37
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 779
      DesignSize = (
        783
        37)
      object Button_Remove: TButton
        Left = 603
        Top = 2
        Width = 84
        Height = 25
        Hint = 'Remove selected alarms (only if discarded)'
        Anchors = [akTop, akRight]
        Caption = '&Remove'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = Button_RemoveClick
        ExplicitLeft = 599
      end
      object Button_Restore: TButton
        Left = 696
        Top = 2
        Width = 84
        Height = 25
        Hint = 'Restore the discarded alarms'
        Anchors = [akTop, akRight]
        Caption = 'Res&tore'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = Button_RestoreClick
        ExplicitLeft = 692
      end
      object Button_Show: TButton
        Left = 5
        Top = 2
        Width = 84
        Height = 25
        Hint = 'Show location of alarm'
        Caption = '&Show'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = Button_ShowClick
      end
      object Button_New: TButton
        Left = 192
        Top = 2
        Width = 84
        Height = 25
        Hint = 
          'Create new alarm (in the same node/folder that the item selected' +
          ' or in the active folder, if no one is selected)'
        Caption = '&New'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = Button_NewClick
      end
      object Button_Discard: TButton
        Left = 98
        Top = 2
        Width = 84
        Height = 25
        Hint = 'Discard selected alarms  (remove on empty alarms)'
        Caption = '&Discard'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = Button_DiscardClick
      end
      object Button_SelectAll: TButton
        Left = 294
        Top = 2
        Width = 84
        Height = 25
        Caption = '&Select All'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = Button_SelectAllClick
      end
      object btnShowHideDetails: TButton
        Left = 384
        Top = 2
        Width = 21
        Height = 25
        Hint = 'Hide or show details of selected alarm(s)'
        Caption = #218
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Symbol'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        OnClick = btnShowHideDetailsClick
      end
    end
  end
  object Grid: TListView
    Left = 8
    Top = 31
    Width = 776
    Height = 98
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Subject'
        Width = 273
      end
      item
        Caption = 'Reminder Date'
        Width = 86
      end
      item
        Caption = 'Time'
        Width = 49
      end
      item
        Caption = 'Expirat./Start  Date'
        Width = 108
      end
      item
        Caption = 'Time'
      end
      item
        Caption = 'Folder'
        Width = 103
      end
      item
        Caption = 'Note'
        Width = 203
      end
      item
        Caption = 'Disc.'
        Width = 35
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
  end
  object cFilter: TEdit
    Left = 524
    Top = 5
    Width = 174
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
    ExplicitLeft = 520
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
end
