object Form_OptionsNew: TForm_OptionsNew
  Left = 310
  Top = 273
  HelpContext = 205
  ActiveControl = TV
  BorderStyle = bsDialog
  Caption = 'Keynote OPTIONS'
  ClientHeight = 376
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Button_OK: TTntButton
    Left = 252
    Top = 347
    Width = 76
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_OKClick
  end
  object Button_Cancel: TTntButton
    Left = 342
    Top = 347
    Width = 76
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = Button_CancelClick
  end
  object TV: TTreeNT
    Left = 5
    Top = 5
    Width = 156
    Height = 336
    Indent = 19
    InsertMarkColor = clScrollBar
    Items.Data = {
      000500000070000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF01000000000001
      010000000000020000000000000001010000000000F809A30000000000000000
      000100000000000000000000003A042500000000FF66FF94FD00000000A0F219
      00A0F2190113EFBBBF47656E6572616C2073657474696E677370000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFF01000000000001000000000000000000000000
      000001010000000000F809A30000000000000000000100000000000000000000
      003A042500000000FF66FF94FD00000000A0F21900A0F2190113EFBBBF526963
      68205465787420656469746F726A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FF01000000000001000000000000000000000000000001010000000000F809A3
      0000000000000000000100000000000000000000003A042500000000FF66FF94
      FD00000000A0F21900A0F219010DEFBBBF547265652050616E656C6D000000FF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0100000000000101000000000002000000
      0000000001010000000000F809A3000000000000000000010000000000000000
      0000003A042500000000FF66FF94FD00000000A0F21900A0F2190110EFBBBF4B
      65794E6F74652066696C65736C000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      01000000000001000000000000000000000000000001010000000000F809A300
      00000000000000000100000000000000000000003A042500000000FF66FF94FD
      00000000A0F21900A0F219010FEFBBBF46696C65206F7074696F6E736E000000
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF01000000000001000000000000000000
      000000000001010000000000F809A30000000000000000000100000000000000
      000000003A042500000000FF66FF94FD00000000A0F21900A0F2190111EFBBBF
      4261636B7570206F7074696F6E7367000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF01000000000001010000000000010000000000000001010000000000F809
      A30000000000000000000100000000000000000000003A042500000000FF66FF
      94FD00000000A0F21900A0F219010AEFBBBF416374696F6E736D000000FFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFF01000000000001000000000000000000000000
      000001010000000000F809A30000000000000000000100000000000000000000
      003A042500000000FF66FF94FD00000000A0F21900A0F2190110EFBBBF436F6E
      6669726D6174696F6E7366000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0100
      0000000001010000000000010000000000000001010000000000F809A3000000
      0000000000000100000000000000000000003A042500000000FF66FF94FD0000
      0000A0F21900A0F2190109EFBBBF4368726F6D6569000000FFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF010000000000010000000000000000000000000000010100
      00000000F809A30000000000000000000100000000000000000000003A042500
      000000FF66FF94FD00000000A0F21900A0F219010CEFBBBF5461622069636F6E
      7368000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0100000000000101000000
      0000040000000000000001010000000000F809A3000000000000000000010000
      0000000000000000003A042500000000FF66FF94FD00000000A0F21900A0F219
      010BEFBBBF416476616E63656467000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FF01000000000001000000000000000000000000000001010000000000F809A3
      0000000000000000000100000000000000000000003A042500000000FF66FF94
      FD00000000A0F21900A0F219010AEFBBBF466F726D61747371000000FFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFF0100000000000100000000000000000000000000
      0001010000000000F809A3000000000000000000010000000000000000000000
      3A042500000000FF66FF94FD00000000A0F21900A0F2190114EFBBBF436C6970
      626F61726420636170747572656A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FF01000000000001000000000000000000000000000001010000000000F809A3
      0000000000000000000100000000000000000000003A042500000000FF66FF94
      FD00000000A0F21900A0F219010DEFBBBF46696C6520747970657365000000FF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0100000000000100000000000000000000
      0000000001010000000000F809A3000000000000000000010000000000000000
      0000003A042500000000FF66FF94FD00000000A0F21900A0F2190108EFBBBF4F
      74686572}
    ItemHeight = 16
    Options = [toAutoExpand, toEvenHeight, toHotTrack, toReadOnly, toShowButtons, toShowLines, toShowRoot]
    ParentColor = False
    ScrollTime = 0
    TabOrder = 0
    OnChange = TVChange
  end
  object Pages: TNotebook
    Left = 165
    Top = 0
    Width = 341
    Height = 343
    PageIndex = 5
    TabOrder = 1
    object PG_Interface: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Interface'
      object GroupBox1: TTntGroupBox
        Left = 5
        Top = 5
        Width = 326
        Height = 148
        Caption = ' General interface options '
        TabOrder = 0
        object Label18: TTntLabel
          Left = 183
          Top = 101
          Width = 51
          Height = 13
          Caption = 'Language:'
          FocusControl = Combo_Language
        end
        object checkbox_IconInTray: TTntCheckBox
          Left = 13
          Top = 23
          Width = 293
          Height = 17
          Hint = 'Place program icon in System tray'
          Caption = '&Minimize to system tray'
          TabOrder = 0
        end
        object checkbox_StartMinimized: TTntCheckBox
          Left = 13
          Top = 43
          Width = 293
          Height = 17
          Hint = 'Launch program in iconized state'
          Caption = 'Start program minimi&zed'
          TabOrder = 1
        end
        object CheckBox_MinimizeOnClose: TTntCheckBox
          Left = 13
          Top = 63
          Width = 279
          Height = 17
          Hint = 'Minimize instead of exiting when Close button clicked'
          Caption = 'Don'#39't &close; minimize instead'
          TabOrder = 2
        end
        object CheckBox_ShowTooltips: TTntCheckBox
          Left = 13
          Top = 83
          Width = 279
          Height = 17
          Hint = 'Display tooltip hints (like this one)'
          Caption = 'Show &tooltip hints'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object CheckBox_SplashScreen: TTntCheckBox
          Left = 13
          Top = 121
          Width = 285
          Height = 17
          Hint = 'Display the About box when starting KeyNote'
          Caption = 'Show spl&ash screen'
          Enabled = False
          TabOrder = 3
          Visible = False
        end
        object CheckBox_SingleInstance: TTntCheckBox
          Left = 13
          Top = 102
          Width = 165
          Height = 17
          Hint = 'Allow only 1 copy of Keynote at a time'
          Caption = 'Allow only o&ne instance'
          TabOrder = 5
        end
        object Combo_Language: TTntComboBox
          Left = 182
          Top = 117
          Width = 138
          Height = 21
          Hint = 'Select action to perform when ESC key is pressed'
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 6
        end
      end
      object GroupBox7: TTntGroupBox
        Left = 5
        Top = 160
        Width = 324
        Height = 136
        Caption = ' Program hotkeys '
        TabOrder = 1
        object Label4: TTntLabel
          Left = 12
          Top = 23
          Width = 75
          Height = 13
          Caption = 'On &Escape key:'
          FocusControl = Combo_EscapeAction
        end
        object Combo_EscapeAction: TTntComboBox
          Left = 12
          Top = 38
          Width = 171
          Height = 21
          Hint = 'Select action to perform when ESC key is pressed'
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            'Do nothing'
            'Minimize main window'
            'Exit program')
        end
        object Edit_HotKey: THotKey
          Left = 28
          Top = 85
          Width = 171
          Height = 19
          Hint = 
            'Hotkey used to activate Keynote when iconized'#13#10'Use BACKSPACE to ' +
            'delete existing hotkey, then'#13#10'press the key combination you want' +
            ' to use'
          HotKey = 24699
          InvalidKeys = [hcNone]
          Modifiers = [hkShift, hkCtrl]
          TabOrder = 2
        end
        object CheckBox_HotkeyActivate: TTntCheckBox
          Left = 12
          Top = 65
          Width = 279
          Height = 17
          Caption = 'Use activation &Hotkey:'
          TabOrder = 1
        end
        object CheckBox_HotKeyWarn: TTntCheckBox
          Left = 28
          Top = 110
          Width = 293
          Height = 17
          Hint = 'Show message when activation hotkey cannot be acquired'
          Caption = '&Warn when cannot assign activation hotkey'
          TabOrder = 3
        end
      end
    end
    object PG_Editor: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Editor'
      object GBox_EditorGlobal: TTntGroupBox
        Left = 5
        Top = 5
        Width = 324
        Height = 164
        Caption = ' Global editor settings'
        TabOrder = 0
        object CheckBox_WordSelect: TTntCheckBox
          Left = 10
          Top = 27
          Width = 303
          Height = 17
          Hint = 'Double-click select whole word'
          Caption = '&Automaticaly select whole words'
          TabOrder = 0
        end
        object CB_SaveCaretPos: TTntCheckBox
          Left = 10
          Top = 91
          Width = 303
          Height = 17
          Hint = 'Restore caret positions in notes when the file is opened'
          Caption = 'Save and restore caret &position'
          TabOrder = 3
        end
        object CheckBox_TrackCaretPos: TTntCheckBox
          Left = 10
          Top = 51
          Width = 303
          Height = 17
          Hint = 'Display cursor row and column in status bar'
          Caption = '&Show caret position in status bar'
          TabOrder = 1
          OnClick = CheckBox_TrackCaretPosClick
        end
        object CheckBox_AutoIndent: TTntCheckBox
          Left = 10
          Top = 116
          Width = 303
          Height = 17
          Hint = 'Indent new line to match previous'
          Caption = 'Auto &indent lines'
          TabOrder = 4
        end
        object CB_TrackWordCount: TTntCheckBox
          Left = 10
          Top = 71
          Width = 303
          Height = 17
          Hint = 'Display word and page counts in status bar'
          Caption = 'Show &word count in status bar'
          TabOrder = 2
          OnClick = CB_TrackWordCountClick
        end
        object CB_PlainDefaultPaste: TTntCheckBox
          Left = 10
          Top = 137
          Width = 303
          Height = 17
          Hint = 
            'Default paste (CTR+V or equivalent) as plain text when copied fr' +
            'om outside KN.  How text is shown is determined by '#39'Plain text m' +
            'ode'#39' (see Advanced / Clipboard capture)'
          Caption = 'Paste external as plain &text'
          TabOrder = 5
        end
      end
      object GroupBox20: TTntGroupBox
        Left = 5
        Top = 170
        Width = 324
        Height = 145
        TabOrder = 1
        object Label12: TTntLabel
          Left = 10
          Top = 120
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'Incr&ement para space by:'
          FocusControl = Spin_ParaSpaceInc
        end
        object Label11: TTntLabel
          Left = 10
          Top = 90
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'Increment &font size by:'
          FocusControl = Spin_FontSizeInc
        end
        object Label1: TTntLabel
          Left = 10
          Top = 60
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'I&ncrement indents by:'
          FocusControl = Spin_IndentInc
        end
        object Label_UndoLimit: TTntLabel
          Left = 10
          Top = 30
          Width = 159
          Height = 13
          AutoSize = False
          Caption = '&Undo buffer limit:'
          FocusControl = Spin_UndoLimit
        end
        object Spin_UndoLimit: TSpinEdit
          Left = 175
          Top = 23
          Width = 56
          Height = 22
          Hint = 'Number of editing operations that can be undone'
          MaxLength = 3
          MaxValue = 255
          MinValue = 1
          TabOrder = 0
          Value = 4
        end
        object Spin_IndentInc: TSpinEdit
          Left = 175
          Top = 53
          Width = 56
          Height = 22
          Hint = 'Increment line indents by this value'
          MaxLength = 3
          MaxValue = 255
          MinValue = 2
          TabOrder = 1
          Value = 8
        end
        object Spin_FontSizeInc: TSpinEdit
          Left = 175
          Top = 83
          Width = 56
          Height = 22
          Hint = 'Increment font size by this value'
          MaxLength = 3
          MaxValue = 255
          MinValue = 1
          TabOrder = 2
          Value = 2
        end
        object Spin_ParaSpaceInc: TSpinEdit
          Left = 175
          Top = 113
          Width = 56
          Height = 22
          Hint = 'Increment "space before/after paragraph" by this value'
          MaxLength = 3
          MaxValue = 255
          MinValue = 1
          TabOrder = 3
          Value = 2
        end
      end
    end
    object PG_Tree: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Tree'
      object GBox_TreeGlobal: TTntGroupBox
        Left = 5
        Top = 5
        Width = 326
        Height = 292
        Caption = ' Global tree panel settings '
        TabOrder = 0
        object Label17: TTntLabel
          Left = 10
          Top = 23
          Width = 121
          Height = 13
          Caption = 'Initial tree e&xpand mode:'
          FocusControl = Combo_ExpandMode
        end
        object CheckBox_EditNewNodes: TTntCheckBox
          Left = 10
          Top = 65
          Width = 311
          Height = 17
          Hint = 'Always edit name of newly created nodes'
          Caption = '&After creating a new node, switch to edit mode'
          TabOrder = 1
        end
        object CheckBox_EditInPlace: TTntCheckBox
          Left = 10
          Top = 85
          Width = 311
          Height = 17
          Hint = 'Edit node names directly in tree'
          Caption = '&Edit nodes "in place" (no dialog box)'
          TabOrder = 2
        end
        object CheckBox_InheritBGColor: TTntCheckBox
          Left = 10
          Top = 133
          Width = 311
          Height = 17
          Hint = 'Newly added node inherits BG color from selected node'
          Caption = 'Inherit &BG color from active node'
          TabOrder = 4
        end
        object CheckBox_AutoNameVNodes: TTntCheckBox
          Left = 10
          Top = 113
          Width = 311
          Height = 17
          Hint = 'Use linked file name as virtual node name'
          Caption = 'Auto&matically name nodes where possible'
          TabOrder = 3
        end
        object CB_InheritNodeProperties: TTntCheckBox
          Left = 10
          Top = 153
          Width = 311
          Height = 17
          Hint = 'Newly added node inherits checked, bold and color state'
          Caption = 'Inherit &properties from active node'
          TabOrder = 5
        end
        object CheckBox_HotTrackTree: TTntCheckBox
          Left = 10
          Top = 178
          Width = 311
          Height = 17
          Hint = 'Tree selection follows mouse pointer'
          Caption = '&Hot track tree nodes'
          TabOrder = 6
        end
        object CheckBox_AutoScroll: TTntCheckBox
          Left = 10
          Top = 198
          Width = 311
          Height = 17
          Hint = 'Auto scroll tree when text doesn'#39't fit in window'
          Caption = 'A&uto scroll tree on mouse movement'
          TabOrder = 7
        end
        object CheckBox_TreeTips: TTntCheckBox
          Left = 10
          Top = 225
          Width = 311
          Height = 17
          Hint = 'Show node name in tooltip when too long to fit in window '
          Caption = 'Show &tooltips in tree'
          TabOrder = 8
        end
        object CB_ShowFullPath: TTntCheckBox
          Left = 10
          Top = 245
          Width = 204
          Height = 17
          Hint = 'Display only name or full path of selected node'
          Caption = 'Sh&ow full node path in status bar'
          TabOrder = 9
        end
        object CB_ShowFullPathSearch: TTntCheckBox
          Left = 10
          Top = 265
          Width = 311
          Height = 17
          Hint = 'Display name or full path in search results panel'
          Caption = 'Show full node path in search &results'
          TabOrder = 11
        end
        object CB_PathTopToBottom: TTntCheckBox
          Left = 216
          Top = 245
          Width = 105
          Height = 17
          Hint = 'Display node path in reverse order'
          Caption = 'Reverse or&der'
          TabOrder = 10
        end
        object Combo_ExpandMode: TTntComboBox
          Left = 10
          Top = 38
          Width = 236
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
    object PG_KNTFiles: TPage
      Left = 0
      Top = 0
      Caption = 'PG_KNTFiles'
      object GroupBox2: TTntGroupBox
        Left = 5
        Top = 5
        Width = 325
        Height = 108
        Caption = ' On program startup... '
        TabOrder = 0
        object TB_OpenDlgUserFile: TToolbarButton97
          Left = 287
          Top = 51
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgUserFileClick
        end
        object CB_LoadLastFile: TTntCheckBox
          Left = 10
          Top = 25
          Width = 143
          Height = 17
          Hint = 'Open the file which was used the last time'
          Caption = '&Load last-used file'
          TabOrder = 0
        end
        object CB_LoadUserFile: TTntCheckBox
          Left = 160
          Top = 25
          Width = 145
          Height = 17
          Hint = 'Open the specified file on startup'
          Caption = 'Load &specific file:'
          TabOrder = 1
        end
        object Edit_UserFile: TTntEdit
          Left = 10
          Top = 50
          Width = 279
          Height = 21
          Hint = 'Enter a filename'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object CB_AutoNewFile: TTntCheckBox
          Left = 10
          Top = 80
          Width = 279
          Height = 17
          Caption = '&Create a blank new file if no file is loaded'
          TabOrder = 3
        end
      end
      object GroupBox9: TTntGroupBox
        Left = 5
        Top = 118
        Width = 325
        Height = 126
        Caption = ' Auto save '
        TabOrder = 1
        object Label_Minutes: TTntLabel
          Left = 156
          Top = 66
          Width = 37
          Height = 13
          Caption = 'minutes'
        end
        object Checkbox_AutoSave: TTntCheckBox
          Left = 10
          Top = 23
          Width = 303
          Height = 17
          Hint = 'Automatically save note files when necessary'
          Caption = '&Automatically save changes in KeyNote file:'
          TabOrder = 0
        end
        object CheckBox_AutoSaveOnFocus: TTntCheckBox
          Left = 26
          Top = 45
          Width = 287
          Height = 17
          Hint = 'Save file when KeyNote loses focus'
          Caption = '&When you switch to another application'
          TabOrder = 1
        end
        object CheckBox_AutoSaveOnTimer: TTntCheckBox
          Left = 26
          Top = 65
          Width = 63
          Height = 17
          Hint = 'Save file at regular intervals'
          Caption = '&Every'
          TabOrder = 2
        end
        object Spin_AutoSaveOnTimerInt: TSpinEdit
          Left = 89
          Top = 63
          Width = 61
          Height = 22
          Hint = 'How often to save changes automatically'
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 3
          Value = 10
        end
        object CB_SkipNewFilePrompt: TTntCheckBox
          Left = 10
          Top = 98
          Width = 311
          Height = 17
          Caption = '&Do not prompt to save new files'
          TabOrder = 4
        end
      end
    end
    object PG_FileOptions: TPage
      Left = 0
      Top = 0
      Caption = 'PG_FileOptions'
      object GroupBox3: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 93
        Caption = ' Recently used files list '
        TabOrder = 0
        object CheckBox_MRUSubmenu: TTntCheckBox
          Left = 9
          Top = 44
          Width = 304
          Height = 17
          Hint = 'Display recent files in a separate submenu'
          Caption = 'Display as &Submenu'
          TabOrder = 2
        end
        object CheckBox_MRUUse: TTntCheckBox
          Left = 9
          Top = 24
          Width = 227
          Height = 17
          Hint = 'Keep list of most recently used note files'
          Caption = '&Remember recently used files:'
          TabOrder = 0
        end
        object CheckBox_MRUFullPath: TTntCheckBox
          Left = 9
          Top = 64
          Width = 312
          Height = 17
          Hint = 'Display full path in recent files list'
          Caption = 'Show f&ull path in MRU menu'
          TabOrder = 3
        end
        object Spin_MRUCount: TSpinEdit
          Left = 240
          Top = 21
          Width = 61
          Height = 22
          Hint = 'Number of files in MRU list'
          MaxLength = 1
          MaxValue = 9
          MinValue = 1
          TabOrder = 1
          Value = 5
        end
      end
      object GroupBox13: TTntGroupBox
        Left = 5
        Top = 107
        Width = 327
        Height = 86
        Caption = ' Open in Read-Only mode : '
        TabOrder = 1
        object CheckBox_OpenFloppyReadOnly: TTntCheckBox
          Left = 10
          Top = 20
          Width = 311
          Height = 17
          Hint = 'Open files on diskettes in read-only mode'
          Caption = 'Files on &floppy disks'
          TabOrder = 0
        end
        object CheckBox_OpenReadOnlyWarn: TTntCheckBox
          Left = 10
          Top = 60
          Width = 311
          Height = 17
          Hint = 'Display warning when file is opened as read-only'
          Caption = '&Warn when opening file in  Read-Only mode'
          TabOrder = 1
        end
        object CheckBox_OpenNetworkReadOnly: TTntCheckBox
          Left = 10
          Top = 40
          Width = 311
          Height = 17
          Hint = 'Open files on network drives in read-only mode'
          Caption = 'Files on &network drives'
          TabOrder = 2
        end
      end
      object GroupBox15: TTntGroupBox
        Left = 5
        Top = 203
        Width = 327
        Height = 91
        Caption = ' Registered file types '
        TabOrder = 2
        object CheckBox_AutoRegisterPrompt: TTntCheckBox
          Left = 10
          Top = 40
          Width = 311
          Height = 17
          Hint = 'Prompt before registering Keynote file type'
          Caption = '&Prompt before creating file association'
          TabOrder = 0
        end
        object CheckBox_AutoRegisterFileType: TTntCheckBox
          Left = 10
          Top = 20
          Width = 311
          Height = 17
          Hint = 'Register Keynote file extension at startup'
          Caption = 'Aut&o register file type (associate .KNT files with KeyNote)'
          TabOrder = 1
        end
        object CheckBox_EncFileAltExt: TTntCheckBox
          Left = 10
          Top = 60
          Width = 311
          Height = 17
          Hint = 'Change extension to .KNE for encrypted files'
          Caption = 'Use &alternate extension (.KNE) for encrypted files'
          TabOrder = 2
        end
      end
    end
    object PG_BackupOptions: TPage
      Left = 0
      Top = 0
      Caption = 'PG_BackupOptions'
      object GroupBox11: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 336
        Caption = ' Backup options '
        TabOrder = 0
        object Label_BakDir: TTntLabel
          Left = 16
          Top = 230
          Width = 124
          Height = 13
          Caption = '&Directory for backup files:'
        end
        object Label_MaxBak2: TTntLabel
          Left = 51
          Top = 175
          Width = 122
          Height = 13
          AutoSize = False
          Caption = '&Max backup level:'
          FocusControl = Combo_BakLevel
        end
        object Bevel3: TBevel
          Left = 14
          Top = 214
          Width = 289
          Height = 5
          Shape = bsTopLine
        end
        object Label_MaxBak1: TTntLabel
          Left = 29
          Top = 139
          Width = 297
          Height = 26
          AutoSize = False
          Caption = 
            'Up to 9 most recent backup files can be kept. '#13#10'Oldest backups w' +
            'ill be recycled automatically.'
        end
        object TB_OpenDlgBakDir: TToolbarButton97
          Left = 285
          Top = 295
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgBakDirClick
        end
        object checkbox_Backup: TTntCheckBox
          Left = 10
          Top = 59
          Width = 311
          Height = 17
          Hint = 'Create backup file when saving'
          Caption = '&Backup original file when saving changes'
          TabOrder = 1
        end
        object CheckBox_BackupAppendExt: TTntCheckBox
          Left = 29
          Top = 82
          Width = 228
          Height = 17
          Hint = 'Add backup extension to end of original filename'
          Caption = '&Append extension to original filename:'
          TabOrder = 2
        end
        object Edit_BackupExt: TTntEdit
          Left = 258
          Top = 78
          Width = 44
          Height = 21
          Hint = 'Extension to use for Backup files'
          MaxLength = 9
          TabOrder = 3
        end
        object RB_BakOriginalDir: TTntRadioButton
          Left = 22
          Top = 252
          Width = 242
          Height = 17
          Hint = 'Create backup file in the same folder as original file'
          Caption = '&Original file'#39's directory'
          Checked = True
          TabOrder = 5
          TabStop = True
          OnClick = RB_BakOriginalDirClick
        end
        object RB_BakUserDir: TTntRadioButton
          Left = 22
          Top = 272
          Width = 242
          Height = 17
          Hint = 'Create backup files in specified folder'
          Caption = '&Separate directory:'
          TabOrder = 6
          OnClick = RB_BakOriginalDirClick
        end
        object Edit_BakDir: TTntEdit
          Left = 48
          Top = 295
          Width = 231
          Height = 21
          Hint = 'Specify folder for backup files'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
        end
        object Combo_BakLevel: TTntComboBox
          Left = 176
          Top = 172
          Width = 65
          Height = 21
          Style = csDropDownList
          DropDownCount = 10
          ItemHeight = 13
          TabOrder = 8
        end
        object CB_BackupVNodes: TTntCheckBox
          Left = 29
          Top = 105
          Width = 295
          Height = 17
          Hint = 'Backup files linked to virtual nodes'
          Caption = 'Also back up &virtual node files'
          TabOrder = 4
        end
        object CB_BackupRegularIntervals: TTntCheckBox
          Left = 10
          Top = 29
          Width = 311
          Height = 17
          Hint = 
            'Create copies of the file montly, weekly and daily. Montly copie' +
            's will not be deleted nor replaced by KN'
          Caption = '&Backup at regular intervals'
          TabOrder = 0
        end
      end
    end
    object PG_Actions: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Actions'
      object GroupBox10: TTntGroupBox
        Left = 5
        Top = 5
        Width = 326
        Height = 164
        Caption = ' Actions on program inactivity '
        TabOrder = 0
        object Label9: TTntLabel
          Left = 226
          Top = 26
          Width = 56
          Height = 13
          Caption = 'minutes idle'
        end
        object Label10: TTntLabel
          Left = 226
          Top = 66
          Width = 56
          Height = 13
          Caption = 'minutes idle'
        end
        object Bevel4: TBevel
          Left = 11
          Top = 54
          Width = 291
          Height = 5
          Shape = bsTopLine
        end
        object CheckBox_TimerMinimize: TTntCheckBox
          Left = 11
          Top = 26
          Width = 155
          Height = 17
          Hint = 'Automatically hide program when inactive'
          Caption = '&Minimize KeyNote after '
          TabOrder = 0
        end
        object CheckBox_TimerClose: TTntCheckBox
          Left = 11
          Top = 66
          Width = 155
          Height = 17
          Hint = 'Automatically close file when inactive (AutoSave must be ON!)'
          Caption = '&Close Notes file after'
          TabOrder = 2
        end
        object CB_CloseEncOnly: TTntCheckBox
          Left = 27
          Top = 91
          Width = 291
          Height = 17
          Hint = 'Apply AutoClose function only to encrypted files'
          Caption = 'Auto-close &only encrypted files'
          TabOrder = 4
        end
        object Spin_TimerMinInt: TSpinEdit
          Left = 168
          Top = 24
          Width = 49
          Height = 22
          Hint = 'How long to wait before minimizing program'
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 10
        end
        object Spin_TimerCloseInt: TSpinEdit
          Left = 168
          Top = 64
          Width = 49
          Height = 22
          Hint = 'How long to wait before auto-closing files'
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 3
          Value = 10
        end
        object CB_TimerCloseDialogs: TTntCheckBox
          Left = 11
          Top = 119
          Width = 291
          Height = 17
          Hint = 'If unchecked, auto-close will abort of any dialog boxes are open'
          Caption = 'If any &dialog boxes are open, force them to close'
          TabOrder = 5
        end
        object CB_TimerCloseAutoReopen: TTntCheckBox
          Left = 11
          Top = 139
          Width = 291
          Height = 17
          Hint = 'Prompt for password and reopen auto-closed encrypted files'
          Caption = '&Reopen auto-closed files on restore (encrypted only)'
          TabOrder = 6
        end
      end
      object GroupBox16: TTntGroupBox
        Left = 5
        Top = 178
        Width = 326
        Height = 118
        Caption = ' URL actions '
        TabOrder = 1
        object Label8: TTntLabel
          Left = 10
          Top = 25
          Width = 87
          Height = 13
          AutoSize = False
          Caption = 'On &URL click:'
          FocusControl = Combo_URLAction
        end
        object Combo_URLAction: TTntComboBox
          Left = 101
          Top = 20
          Width = 166
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object CheckBox_URLShift: TTntCheckBox
          Left = 11
          Top = 49
          Width = 310
          Height = 17
          Hint = 'Respond to URL clicks only is Dhift key held down'
          Caption = '&Shift key must be held down when URL is clicked'
          TabOrder = 1
        end
        object CheckBox_MinOnURL: TTntCheckBox
          Left = 11
          Top = 69
          Width = 310
          Height = 17
          Hint = 'Minimize program on launching a client application for URL'
          Caption = 'Minimize &KeyNote on URL launch'
          TabOrder = 2
        end
        object CheckBox_URLFileAuto: TTntCheckBox
          Left = 11
          Top = 89
          Width = 310
          Height = 17
          Hint = 'Never prompt when file:// URL clicked'
          Caption = '&Launch "file://" URLs without prompting'
          TabOrder = 3
        end
      end
    end
    object PG_Confirmations: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Confirmations'
      object GroupBox12: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 293
        Caption = ' Confirmation options '
        TabOrder = 0
        object Bevel6: TBevel
          Left = 10
          Top = 151
          Width = 311
          Height = 6
          Shape = bsTopLine
        end
        object CheckBox_AutoPasteEval: TTntCheckBox
          Left = 10
          Top = 162
          Width = 311
          Height = 17
          Hint = 'Immediately paste evaluation result into note'
          Caption = '&Automatically paste expression evaluation results'
          TabOrder = 6
        end
        object CheckBox_AutoPastePlugin: TTntCheckBox
          Left = 10
          Top = 182
          Width = 311
          Height = 17
          Hint = 'Immediately insert text generated by plugins'
          Caption = 'A&utomatically insert text from plugins'
          TabOrder = 7
        end
        object CheckBox_ConfirmTreePaste: TTntCheckBox
          Left = 10
          Top = 88
          Width = 311
          Height = 17
          Hint = 'Ask for confirmation before pasting tree nodes'
          Caption = 'Confirm &pasting tree nodes'
          TabOrder = 3
        end
        object checkbox_ConfirmExit: TTntCheckBox
          Left = 10
          Top = 28
          Width = 311
          Height = 17
          Hint = 'Prompt before closing program'
          Caption = '&Confirm closing KeyNote'
          TabOrder = 0
        end
        object checkbox_ConfirmDelete: TTntCheckBox
          Left = 10
          Top = 48
          Width = 311
          Height = 17
          Hint = 'Prompt before removing a note'
          Caption = 'Confirm &deleting notes'
          TabOrder = 1
        end
        object CheckBox_ConfirmNodeDelete: TTntCheckBox
          Left = 10
          Top = 68
          Width = 311
          Height = 17
          Hint = 'Prompt before removing a node'
          Caption = 'Confirm deleting tree &nodes'
          TabOrder = 2
        end
        object CB_ConfirmNodeRefresh: TTntCheckBox
          Left = 10
          Top = 108
          Width = 311
          Height = 17
          Hint = 'Ask for confirmation when "Refresh" command used'
          Caption = 'Confirm refreshing &virtual nodes'
          TabOrder = 4
        end
        object GroupBox18: TTntGroupBox
          Left = 10
          Top = 210
          Width = 307
          Height = 71
          Caption = ' When dropping tree nodes on another tab: '
          TabOrder = 8
          object CB_DropNodesOnTabPrompt: TTntCheckBox
            Left = 10
            Top = 20
            Width = 287
            Height = 17
            Caption = 'C&onfirm before transferring nodes'
            TabOrder = 0
          end
          object CB_DropNodesOnTabMove: TTntCheckBox
            Left = 10
            Top = 40
            Width = 287
            Height = 17
            Caption = '&Move nodes (delete from original tree)'
            TabOrder = 1
          end
        end
        object CB_TreeClipConfirm: TTntCheckBox
          Left = 10
          Top = 128
          Width = 311
          Height = 17
          Caption = 'Confirm starting clipboard capture in tree-t&ype notes'
          TabOrder = 5
        end
      end
    end
    object PG_Chrome: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Chrome'
      object GroupBox14: TTntGroupBox
        Left = 5
        Top = 152
        Width = 326
        Height = 143
        Caption = ' Note tabs: Options '
        TabOrder = 0
        object Label14: TTntLabel
          Left = 10
          Top = 70
          Width = 276
          Height = 13
          Caption = 'Settings below will take effect after KeyNote is restarted.'
        end
        object Label15: TTntLabel
          Left = 10
          Top = 90
          Width = 90
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = '&Tab position:'
          FocusControl = Combo_TabPos
        end
        object CheckBox_TabsStacked: TTntCheckBox
          Left = 18
          Top = 113
          Width = 303
          Height = 17
          Hint = 'Arrange tabs in rows when necesary'
          Caption = '&Multiline tabs (arrange in rows when cannot fit)'
          TabOrder = 3
        end
        object CheckBox_TabsHotTrack: TTntCheckBox
          Left = 10
          Top = 40
          Width = 311
          Height = 17
          Hint = 'Hot-track note tabs with the mouse'
          Caption = '&Hot track in tabs'
          TabOrder = 1
        end
        object CheckBox_TabsImages: TTntCheckBox
          Left = 10
          Top = 20
          Width = 311
          Height = 17
          Hint = 'Display pictures on note tabs'
          Caption = '&Show icons on tabs'
          TabOrder = 0
        end
        object Combo_TabPos: TTntComboBox
          Left = 104
          Top = 87
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object GroupBox5: TTntGroupBox
        Left = 5
        Top = 5
        Width = 326
        Height = 140
        Caption = ' Note tabs: Font and colors '
        TabOrder = 1
        object Label5: TTntLabel
          Left = 15
          Top = 24
          Width = 60
          Height = 13
          Caption = 'Settings for:'
        end
        object RB_ActiveTab: TTntRadioButton
          Left = 23
          Top = 42
          Width = 127
          Height = 17
          Hint = 'Settings for active tab'
          Caption = '&Active tab'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_InactiveTab: TTntRadioButton
          Left = 156
          Top = 42
          Width = 146
          Height = 17
          Hint = 'Settings for inactive tabs'
          Caption = '&Inactive tabs'
          TabOrder = 1
        end
        object Edit_Sample: TTntEdit
          Left = 13
          Top = 98
          Width = 303
          Height = 21
          TabStop = False
          MaxLength = 127
          TabOrder = 5
          Text = 'Sample font'
        end
        object BTN_Font: TBitBtn
          Left = 15
          Top = 64
          Width = 90
          Height = 25
          Hint = 'Select tab font attributes'
          Caption = '&Font'
          TabOrder = 2
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
            DADAADAD444444444444DADADADADADADADAADAD44444DAD4444DADADA44DADA
            D44AADADADA44DADA44DDADADADA4444444AADADADADA44DA44DD0DAD0DADA44
            D44AA07D70ADADA4444DD70007DADADA444AAD0D0DADADADA44DDA0A0ADADADA
            DADAAD707DADADADADADDAD0DADADADADADAADADADADADADADAD}
        end
        object BTN_Color: TBitBtn
          Left = 110
          Top = 64
          Width = 90
          Height = 25
          Hint = 'Select tab color'
          Caption = '&Color'
          TabOrder = 3
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000000000000000000000000000000000009F609F3F003F
            3F003F3F003F3F003F0000000000000000003F00003F00003F00003F00003F00
            003F00003F0000000000DF9FDFBF3FBF7F007F7F007F7F007F00000000000000
            00003F00007F00007F00007F00007F00007F00007F00003F0000DF9FDFFF7FFF
            FF00FFFF00FFFF00FF7F7F007F7F007F7F00BF3F00FF0000FF0000FF0000FF00
            00FF00007F00003F0000DF9FDFFF7FFFFF00FFFF00FFFF00FF7F7F007F7F007F
            7F00BF3F00FF0000FF0000FF0000FF0000FF00007F00003F0000DF9FDFFF7FFF
            FF00FFFF00FFFF00FF7F7F007F7F007F7F00BF3F00FF0000FF0000FF0000FF00
            00FF00007F00003F0000DF9FDFFF7FFFFF00FFFF00FFFF00FF7F7F007F7F007F
            7F00BF3F00FF0000FF0000FF0000FF0000FF00007F00003F0000DF9FDFFF7FFF
            FF00FFFF00FFFF00FF3FBF003FBF003FBF00BF7F00FF0000FF0000FF0000FF00
            00FF00007F00003F0000DF9FDFFF7FFFFF00FFFF00FFFF00FF00FF0000FF0000
            FF007F7F00FF0000FF0000FF0000FF0000FF00007F00003F0000DF9FDFFF7FFF
            FF00FFFF00FFFF00FF00FF0000FF0000FF003FBF007F7F00AF6F30AF3030AF30
            30AF30303F00003F00009F9FDF7F7FFF0000FF0000FF0000FF00FF0000FF0000
            FF0000FF0000FF0030AF306060606060606060600000000000009F9FDF7F7FFF
            0000FF0000FF0000FF00FF0000FF0000FF0000FF0000FF0030AF306060606060
            606060600000000000009F9FDF7F7FFF0000FF0000FF0000FF007F7F007F7F20
            DF5F20DF5F20DF5F209F5F508F8F508F8F508F8F0000000000009F9FDF7F7FFF
            0000FF0000FF0000FF0000FF0000FF40BFBF40BFBF40BFBF40BFBF40BFBF40BF
            BF40BFBF0000000000009F9FDF7F7FFF0000FF0000FF0000FF0000FF0000FF40
            BFBF40BFBF40BFBF40BFBF40BFBF40BFBF40BFBF0000000000009F9FDF7F7FFF
            7F7FFF7F7FFF7F7FFF7F7FFF7F7FFF7FFFFF7FFFFF7FFFFF7FFFFF7FFFFF7FFF
            FF7FFFFF3FBFBF000000CFCFCF9F9FDF9F9FDF9F9FDF9F9FDF9F9FDF9F9FDF9F
            DFDF9FDFDF9FDFDF9FDFDF9FDFDF9FDFDF9FDFDF9FDFDF609F9F}
        end
        object BTN_Defaults: TBitBtn
          Left = 205
          Top = 64
          Width = 90
          Height = 25
          Hint = 'Restore default tab font and color'
          Caption = '&Reset'
          TabOrder = 4
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777777777777777777777777777777
            7777777777777778477777444447777748777744447777777477774447777777
            7477774474777777747777477744777748777777777744448777777777777777
            7777777777777777777777777777777777777777777777777777}
        end
      end
    end
    object PG_Icons: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Icons'
      object GroupBox_ICN: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 336
        Caption = ' &Tab icons '
        TabOrder = 0
        object Label_ICN: TTntLabel
          Left = 179
          Top = 308
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Button_ICNAdd: TTntButton
          Left = 174
          Top = 35
          Width = 91
          Height = 25
          Hint = 'Add a new icon to end of list'
          Caption = '&Add...'
          TabOrder = 1
          OnClick = Button_ICNAddClick
        end
        object Button_ICNInsert: TTntButton
          Left = 174
          Top = 65
          Width = 91
          Height = 25
          Hint = 'Insert a new icon at this position'
          Caption = '&Insert...'
          TabOrder = 2
          OnClick = Button_ICNInsertClick
        end
        object Button_ICNDelete: TTntButton
          Left = 174
          Top = 95
          Width = 91
          Height = 25
          Hint = 'Remove selected icon from list'
          Caption = '&Delete'
          TabOrder = 3
          OnClick = Button_ICNDeleteClick
        end
        object Button_ICNReset: TTntButton
          Left = 174
          Top = 125
          Width = 91
          Height = 25
          Hint = 'Restore factory default icons'
          Caption = '&Reset'
          TabOrder = 4
          OnClick = Button_ICNResetClick
        end
        object List_Icn: TGFXListBox
          Left = 10
          Top = 25
          Width = 143
          Height = 296
          ExtendedSelect = False
          ItemHeight = 20
          TabOrder = 0
        end
      end
    end
    object PG_Advanced: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Advanced'
      object GroupBox8: TTntGroupBox
        Left = 5
        Top = 5
        Width = 326
        Height = 286
        Caption = ' Advanced settings '
        TabOrder = 0
        object Label16: TTntLabel
          Left = 10
          Top = 22
          Width = 292
          Height = 13
          Caption = 'Settings marked (*) will take effect after restarting KeyNote.'
        end
        object Bevel1: TBevel
          Left = 10
          Top = 130
          Width = 311
          Height = 6
          Shape = bsTopLine
        end
        object CheckBox_DisableFileMon: TTntCheckBox
          Left = 10
          Top = 45
          Width = 311
          Height = 17
          Hint = 'Do not monitor .KNT file for changes (compatibility option)'
          Caption = '&Disable folder monitor (*)'
          TabOrder = 0
        end
        object CheckBox_ShowFonts: TTntCheckBox
          Left = 10
          Top = 65
          Width = 311
          Height = 17
          Hint = 'Display font names using actual font samples'
          Caption = '&Show font samples in Font combo box (*)'
          TabOrder = 1
        end
        object CheckBox_NoRegistry: TTntCheckBox
          Left = 10
          Top = 220
          Width = 311
          Height = 17
          Hint = 'Keep form data in .MRU file'
          Caption = 'Do not save &window settings to registry'
          TabOrder = 8
        end
        object CheckBox_UseOldColorDlg: TTntCheckBox
          Left = 10
          Top = 140
          Width = 311
          Height = 17
          Hint = 
            'Use traditional color selection dialogs or apply current button ' +
            'color'
          Caption = '&Old style color selection dialogs'
          TabOrder = 4
        end
        object CheckBox_RunAutoMacros: TTntCheckBox
          Left = 10
          Top = 160
          Width = 311
          Height = 17
          Hint = 'Execute Auto-run macros on creating files and notes'
          Caption = '&Allow Auto-run macros'
          TabOrder = 5
        end
        object CheckBox_SafePrint: TTntCheckBox
          Left = 10
          Top = 180
          Width = 311
          Height = 17
          Hint = 'Use simpler printing logic (compatibility fix)'
          Caption = 'Safe &print mode'
          TabOrder = 6
        end
        object CheckBox_FixScrollBars: TTntCheckBox
          Left = 10
          Top = 200
          Width = 311
          Height = 17
          Hint = 
            'Always update scrollbars in tree-type notes (causes some flicker' +
            ')'
          Caption = '&Fix scroll bars in tree-type notes'
          TabOrder = 7
        end
        object CheckBox_LongCombos: TTntCheckBox
          Left = 10
          Top = 85
          Width = 311
          Height = 17
          Hint = 'Display slightly wider drop-down lists on toolbars'
          Caption = '&Wider drop-down lists on toolbars (*)'
          TabOrder = 2
        end
        object CheckBox_RichEditv3: TTntCheckBox
          Left = 10
          Top = 105
          Width = 311
          Height = 17
          Hint = 'Prevent formatting loss when using version 3.0 of riched20.dll'
          Caption = 'Enable &Rich Edit version 3.0 fixes (*)'
          TabOrder = 3
        end
        object CB_IgnoreUpgrades: TTntCheckBox
          Left = 10
          Top = 240
          Width = 311
          Height = 17
          Hint = 'Do not display any information when KeyNote is upgraded'
          Caption = 'Do not display &upgrade information'
          TabOrder = 9
        end
        object CB_ResPanelActiveUpdate: TTntCheckBox
          Left = 10
          Top = 260
          Width = 311
          Height = 17
          Caption = 'Load Resource panel data only when &necessary'
          TabOrder = 10
        end
      end
    end
    object PG_Formats: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Formats'
      object GroupBox4: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 166
        Caption = ' Date and time formats '
        TabOrder = 0
        object Label2: TTntLabel
          Left = 10
          Top = 35
          Width = 87
          Height = 13
          AutoSize = False
          Caption = '&Date format:'
          FocusControl = Edit_DateFormat
        end
        object Label3: TTntLabel
          Left = 10
          Top = 85
          Width = 87
          Height = 13
          AutoSize = False
          Caption = '&Time format:'
          FocusControl = Edit_TimeFormat
        end
        object Label_SampleDate: TTntLabel
          Left = 101
          Top = 55
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Label_SampleTime: TTntLabel
          Left = 101
          Top = 105
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Edit_DateFormat: TTntComboBox
          Left = 101
          Top = 30
          Width = 204
          Height = 21
          Hint = 'Format to use when inserting current date'
          ItemHeight = 13
          TabOrder = 0
        end
        object Edit_TimeFormat: TTntComboBox
          Left = 101
          Top = 80
          Width = 204
          Height = 21
          Hint = 'Format to use when inserting current time'
          ItemHeight = 13
          MaxLength = 99
          TabOrder = 1
        end
        object CB_DTUseLastSelection: TTntCheckBox
          Left = 10
          Top = 125
          Width = 295
          Height = 17
          Caption = '&Use last format selected in drop-down menu'
          TabOrder = 2
        end
      end
      object GroupBox17: TTntGroupBox
        Left = 5
        Top = 180
        Width = 328
        Height = 77
        Caption = '  Insert Character dialog box  '
        TabOrder = 1
        object CheckBox_InsCharKeepFont: TTntCheckBox
          Left = 10
          Top = 24
          Width = 295
          Height = 17
          Hint = 'Always start with current note font'
          Caption = 'D&efault to current text font'
          TabOrder = 0
        end
        object CheckBox_InsCharWinClose: TTntCheckBox
          Left = 10
          Top = 44
          Width = 295
          Height = 17
          Hint = 'Automatically close dialog box when characters inserted'
          Caption = '&Close dialog box on insert'
          TabOrder = 1
        end
      end
    end
    object PG_ClipCap: TPage
      Left = 0
      Top = 0
      Caption = 'PG_ClipCap'
      object GroupBox6: TTntGroupBox
        Left = 5
        Top = 5
        Width = 330
        Height = 336
        Caption = ' Clipboard Capture / Web Clip settings '
        TabOrder = 0
        object Label_MaxSize: TTntLabel
          Left = 183
          Top = 171
          Width = 59
          Height = 13
          Caption = '&Limit size to:'
        end
        object Label7: TTntLabel
          Left = 31
          Top = 24
          Width = 205
          Height = 13
          Caption = '&Divider string placed before captured text:'
          FocusControl = Combo_Divider
        end
        object Label_PlainTextMode: TTntLabel
          Left = 12
          Top = 147
          Width = 78
          Height = 13
          Caption = 'Plain text &mode:'
        end
        object TntLabel1: TTntLabel
          Left = 44
          Top = 73
          Width = 46
          Height = 13
          Alignment = taRightJustify
          Caption = 'Web Clip:'
          FocusControl = Combo_ClipNodeNaming
        end
        object TntLabel2: TTntLabel
          Left = 42
          Top = 48
          Width = 48
          Height = 13
          Alignment = taRightJustify
          Caption = 'Clip.Capt:'
          FocusControl = Combo_ClipNodeNaming
        end
        object Combo_Size: TTntComboBox
          Left = 248
          Top = 167
          Width = 73
          Height = 21
          Hint = 
            'Maximum length of text to capture, in bytes (when paste as plain' +
            ' text)'
          ItemHeight = 13
          MaxLength = 6
          TabOrder = 8
          Items.Strings = (
            '0'
            '256'
            '512'
            '1024'
            '2048'
            '4096'
            '16384')
        end
        object Combo_Divider: TTntComboBox
          Left = 96
          Top = 43
          Width = 226
          Height = 21
          Hint = 'Text to place between captured items'
          ItemHeight = 13
          TabOrder = 1
        end
        object CB_IgnoreSelf: TTntCheckBox
          Left = 12
          Top = 101
          Width = 159
          Height = 17
          Hint = 'Do not capture text copied from Keynote itself'
          Caption = 'Ignore clips from &Keynote'
          TabOrder = 3
        end
        object CB_AsText: TTntCheckBox
          Left = 12
          Top = 168
          Width = 127
          Height = 17
          Hint = 'Paste text without any formatting (only for Clipboard Capture)'
          Caption = '&Paste as plain text (*)'
          TabOrder = 7
        end
        object BitBtn_TknHlp: TBitBtn
          Left = 250
          Top = 14
          Width = 25
          Height = 25
          Hint = 'Click to display list of substitution tokens'
          TabOrder = 0
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
            0ADAADADADADADA00DADDADA7777770B07DAADA0000000FF007DDA0FFFBFBFBF
            FF07AD0FBFFF44FFBF07DA0FFFBFFFBFFF07AD0FBFFF47FFBF07DA0FFFBF748F
            FF07AD0FBFFFB747BF07DA0FFF47FF44FF07AD0FBF44B844BF07DA0FFF844448
            FF07AD0FBFFFBFFFBF07DA0FFFBFFFBFFF0AADA00000000000AD}
        end
        object CheckBox_ClipRecall: TTntCheckBox
          Left = 12
          Top = 309
          Width = 287
          Height = 17
          Hint = 'When opening a file, restore last active Capture note'
          Caption = '&Remember capturing note across program sessions'
          TabOrder = 12
        end
        object CheckBox_Sound: TTntCheckBox
          Left = 12
          Top = 290
          Width = 148
          Height = 17
          Hint = 'Indicate capture events with sound ("clip.wav")'
          Caption = 'Play &sound on capture (*)'
          TabOrder = 10
        end
        object GroupBox23: TTntGroupBox
          Left = 11
          Top = 196
          Width = 309
          Height = 87
          Caption = ' &When capturing into a tree note (*) :'
          TabOrder = 9
          object LB_ClipNodeNaming: TTntLabel
            Left = 28
            Top = 62
            Width = 81
            Height = 13
            Alignment = taRightJustify
            Caption = '&New node name:'
            FocusControl = Combo_ClipNodeNaming
          end
          object RB_ClipTreeActive: TTntRadioButton
            Left = 16
            Top = 19
            Width = 266
            Height = 17
            Hint = 
              'Clips will be stored in the node which is active when text is ca' +
              'ptured'
            Caption = 'Paste into currently selected node'
            TabOrder = 0
          end
          object RB_ClipTreeNew: TTntRadioButton
            Left = 16
            Top = 38
            Width = 266
            Height = 17
            Hint = 'New tree node will be created for each new clip'
            Caption = 'Create a new node and paste into it'
            Checked = True
            TabOrder = 1
            TabStop = True
          end
          object Combo_ClipNodeNaming: TTntComboBox
            Left = 114
            Top = 57
            Width = 161
            Height = 21
            Hint = 'Choose how new node will be named'
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 2
          end
        end
        object CB_TestDupClips: TTntCheckBox
          Left = 12
          Top = 118
          Width = 170
          Height = 17
          Hint = 'Discard exact copies of most recently captured text'
          Caption = 'I&gnore duplicate clips'
          TabOrder = 4
        end
        object CB_SwitchIcon: TTntCheckBox
          Left = 166
          Top = 292
          Width = 143
          Height = 15
          Hint = 'Show alternate tray icon when clipboard capture is active'
          Caption = 'Use &alternate tray icon'
          TabOrder = 11
        end
        object CB_SourceURL: TTntCheckBox
          Left = 190
          Top = 118
          Width = 136
          Height = 17
          Hint = 
            'Insert clip source URL, with title if available (only for Clipbo' +
            'ard Capture)'
          Caption = 'Include source &URL (*)'
          TabOrder = 5
        end
        object Combo_PlainTextMode: TTntComboBox
          Left = 96
          Top = 143
          Width = 226
          Height = 21
          Hint = 'How to paste when used plain text mode'
          ItemHeight = 13
          MaxLength = 6
          TabOrder = 6
        end
        object Combo_WCDivider: TTntComboBox
          Left = 96
          Top = 70
          Width = 226
          Height = 21
          Hint = 'If empty, '#39'Clip.Capt.'#39' divider string will be used'
          ItemHeight = 13
          TabOrder = 2
        end
      end
    end
    object PG_FileTypes: TPage
      Left = 0
      Top = 0
      Caption = 'PG_FileTypes'
      object GroupBox19: TTntGroupBox
        Left = 5
        Top = 5
        Width = 329
        Height = 316
        Caption = ' &Text file extensions '
        TabOrder = 0
        object Label13: TTntLabel
          Left = 10
          Top = 262
          Width = 287
          Height = 39
          AutoSize = False
          Caption = 
            'Enter extensions which KeyNote should treat as '#13#10'plain text file' +
            's. This allows KeyNote to recognize '#13#10'plain text files when impo' +
            'rting them as notes.'
        end
        object List_TxtExt: TListBox
          Left = 10
          Top = 25
          Width = 101
          Height = 231
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
        end
        object Button_AddTxtExt: TTntButton
          Left = 120
          Top = 35
          Width = 89
          Height = 25
          Caption = '&Add...'
          TabOrder = 1
          OnClick = Button_AddTxtExtClick
        end
        object Button_DeleteTxtExt: TTntButton
          Left = 120
          Top = 65
          Width = 89
          Height = 25
          Caption = '&Delete'
          TabOrder = 2
          OnClick = Button_DeleteTxtExtClick
        end
        object Button_ResetTxtExt: TTntButton
          Left = 120
          Top = 95
          Width = 89
          Height = 25
          Caption = '&Reset'
          TabOrder = 3
          OnClick = Button_ResetTxtExtClick
        end
      end
    end
    object PG_Language: TPage
      Left = 0
      Top = 0
      Caption = 'PG_Language'
      object GroupBox21: TTntGroupBox
        Left = 5
        Top = 5
        Width = 327
        Height = 106
        Caption = ' Advaned editor language options '
        TabOrder = 0
        object Label6: TTntLabel
          Left = 10
          Top = 65
          Width = 303
          Height = 36
          AutoSize = False
          Caption = 
            'These settings will take effect after KeyNote is restarted '#13#10'or ' +
            'current file is closed and re-opened.'
          WordWrap = True
        end
        object CB_AutoFont: TTntCheckBox
          Left = 10
          Top = 20
          Width = 307
          Height = 17
          Caption = '&Automatically select font language '
          TabOrder = 0
        end
        object CB_AutoKeyboard: TTntCheckBox
          Left = 10
          Top = 40
          Width = 307
          Height = 17
          Caption = '&Automatically select &keyboard'
          TabOrder = 1
        end
      end
      object GroupBox22: TTntGroupBox
        Left = 5
        Top = 225
        Width = 327
        Height = 64
        Caption = ' Find options '
        TabOrder = 1
        object CB_WordAtCursor: TTntCheckBox
          Left = 10
          Top = 20
          Width = 309
          Height = 17
          Hint = 'Prefills search pattern with current word'
          Caption = 'Search for &word at cursor'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CB_FindAutoClose: TTntCheckBox
          Left = 10
          Top = 40
          Width = 309
          Height = 17
          Hint = 'Close the "Find" dialog box when button clicked'
          Caption = 'Automatically &close Find dialog box'
          TabOrder = 1
        end
      end
      object GroupBox24: TTntGroupBox
        Left = 5
        Top = 115
        Width = 327
        Height = 106
        Caption = ' Web browser options '
        TabOrder = 2
        object TB_OpenDlgURLAltBrowserPath: TToolbarButton97
          Left = 287
          Top = 71
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgURLAltBrowserPathClick
        end
        object RB_URLSystemBrowser: TTntRadioButton
          Left = 10
          Top = 25
          Width = 311
          Height = 17
          Caption = '&Use system default web browser'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_URLAltBrowser: TTntRadioButton
          Left = 10
          Top = 45
          Width = 311
          Height = 17
          Caption = 'Use the following &web browser:'
          TabOrder = 1
        end
        object Edit_URLAltBrowserPath: TTntEdit
          Left = 20
          Top = 70
          Width = 269
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
    end
  end
  object Button_Help: TTntButton
    Left = 8
    Top = 347
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button_HelpClick
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 589
    Top = 344
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 554
    Top = 344
  end
  object IconDlg: TOpenDialog
    Filter = 
      'Icons (*.ico)|*.ico|Bitmaps (*.bmp)|*.bmp|Programs and libraries' +
      ' (*.exe, *.dll)|*.exe;*.dll|All files|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist]
    Title = 'Add or insert custom icon'
    Left = 620
    Top = 345
  end
end
