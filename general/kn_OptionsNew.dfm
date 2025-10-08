object Form_OptionsNew: TForm_OptionsNew
  Left = 310
  Top = 273
  HelpContext = 85
  BorderStyle = bsDialog
  Caption = 'Keynote OPTIONS'
  ClientHeight = 421
  ClientWidth = 524
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Button_OK: TButton
    Left = 252
    Top = 389
    Width = 76
    Height = 27
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 342
    Top = 389
    Width = 76
    Height = 27
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = Button_CancelClick
  end
  object Pages: TNotebook
    Left = 167
    Top = 6
    Width = 364
    Height = 377
    PageIndex = 15
    TabOrder = 1
    object PG_Interface: TPage
      Left = 0
      Top = 0
      HelpContext = 88
      Caption = 'PG_Interface'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_General1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 180
        Caption = ' General interface options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label18: TLabel
          Left = 3
          Top = 147
          Width = 67
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Language:'
          FocusControl = Combo_Language
        end
        object CheckBox_SplashScreen: TCheckBox
          Left = 219
          Top = 16
          Width = 128
          Height = 17
          Hint = 'Display the About box when starting KeyNote'
          Caption = 'Show spl&ash screen'
          Enabled = False
          TabOrder = 3
          Visible = False
        end
        object checkbox_IconInTray: TCheckBox
          Left = 13
          Top = 29
          Width = 293
          Height = 17
          Hint = 'Place program icon in System tray'
          Caption = '&Minimize to system tray'
          TabOrder = 0
        end
        object checkbox_StartMinimized: TCheckBox
          Left = 13
          Top = 49
          Width = 293
          Height = 17
          Hint = 'Launch program in iconized state'
          Caption = 'Start program minimi&zed'
          TabOrder = 1
        end
        object CheckBox_MinimizeOnClose: TCheckBox
          Left = 13
          Top = 69
          Width = 279
          Height = 17
          Hint = 'Minimize instead of exiting when Close button clicked'
          Caption = 'Don'#39't &close; minimize instead'
          TabOrder = 2
        end
        object CheckBox_ShowTooltips: TCheckBox
          Left = 13
          Top = 95
          Width = 317
          Height = 17
          Hint = 'Display tooltip hints (like this one)'
          Caption = 'Show &tooltip hints'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
        end
        object CheckBox_SingleInstance: TCheckBox
          Left = 13
          Top = 116
          Width = 300
          Height = 17
          Hint = 'Allow only 1 copy of Keynote at a time'
          Caption = 'Allow only o&ne instance'
          TabOrder = 5
        end
        object Combo_Language: TComboBox
          Left = 76
          Top = 144
          Width = 206
          Height = 21
          Hint = 'Language in which the interface will be displayed'
          Style = csDropDownList
          TabOrder = 6
        end
      end
      object GroupBox_General2: TGroupBox
        Left = 5
        Top = 191
        Width = 350
        Height = 178
        Caption = ' Program hotkeys '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label4: TLabel
          Left = 13
          Top = 28
          Width = 75
          Height = 13
          Caption = 'On &Escape key:'
          FocusControl = Combo_EscapeAction
        end
        object Combo_EscapeAction: TComboBox
          Left = 12
          Top = 45
          Width = 171
          Height = 21
          Hint = 'Select action to perform when ESC key is pressed'
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Do nothing'
            'Minimize main window'
            'Exit program')
        end
        object Edit_HotKey: THotKey
          Left = 28
          Top = 98
          Width = 171
          Height = 21
          Hint = 
            'Hotkey used to activate Keynote when iconized'#13#10'Use BACKSPACE to ' +
            'delete existing hotkey, then'#13#10'press the key combination you want' +
            ' to use'
          HotKey = 24699
          InvalidKeys = [hcNone]
          Modifiers = [hkShift, hkCtrl]
          TabOrder = 2
          StyleName = 'Windows'
        end
        object CheckBox_HotkeyActivate: TCheckBox
          Left = 12
          Top = 75
          Width = 279
          Height = 17
          Caption = 'Use activation &Hotkey:'
          TabOrder = 1
        end
        object CheckBox_HotKeyWarn: TCheckBox
          Left = 28
          Top = 125
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
      HelpContext = 89
      Caption = 'PG_Editor'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_RTFEdit1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 207
        Caption = ' Global editor settings'
        DefaultHeaderFont = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label34: TLabel
          Left = 12
          Top = 177
          Width = 74
          Height = 13
          AutoSize = False
          Caption = 'Ctrl+Up/Down'
          FocusControl = cbCtrlUpDownMode
          StyleElements = [seClient, seBorder]
        end
        object CheckBox_WordSelect: TCheckBox
          Left = 10
          Top = 27
          Width = 303
          Height = 17
          Hint = 'Double-click select whole word'
          Caption = '&Automaticaly select whole words'
          TabOrder = 0
        end
        object CB_SaveCaretPos: TCheckBox
          Left = 10
          Top = 95
          Width = 303
          Height = 17
          Hint = 'Restore caret positions in notes when the file is opened'
          Caption = 'Save and restore caret &position'
          TabOrder = 3
        end
        object CheckBox_TrackCaretPos: TCheckBox
          Left = 10
          Top = 55
          Width = 303
          Height = 17
          Hint = 'Display cursor row and column in status bar'
          Caption = '&Show caret position in status bar'
          TabOrder = 1
          OnClick = CheckBox_TrackCaretPosClick
        end
        object CheckBox_AutoIndent: TCheckBox
          Left = 10
          Top = 121
          Width = 303
          Height = 17
          Hint = 'Indent new line to match previous'
          Caption = 'Auto &indent lines'
          TabOrder = 4
        end
        object CB_TrackWordCount: TCheckBox
          Left = 10
          Top = 75
          Width = 303
          Height = 17
          Hint = 'Display word and page counts in status bar'
          Caption = 'Show &word count in status bar'
          TabOrder = 2
          OnClick = CB_TrackWordCountClick
        end
        object CheckBox_InheritBGColor: TCheckBox
          Left = 10
          Top = 147
          Width = 311
          Height = 17
          Hint = 
            'Editor background color of newly added node inherits from select' +
            'ed node'
          Caption = 'Inherit &BG color from active node  (new nodes)'
          TabOrder = 5
        end
        object cbCtrlUpDownMode: TComboBox
          Left = 100
          Top = 173
          Width = 221
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
      end
      object GroupBox_RTFEdit2: TGroupBox
        Left = 5
        Top = 211
        Width = 350
        Height = 156
        TabOrder = 1
        object Label12: TLabel
          Left = 10
          Top = 99
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'Incr&ement para space by:'
          FocusControl = Spin_ParaSpaceInc
        end
        object Label11: TLabel
          Left = 10
          Top = 73
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'Increment &font size by:'
          FocusControl = Spin_FontSizeInc
        end
        object Label1: TLabel
          Left = 10
          Top = 46
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'I&ncrement indents by:'
          FocusControl = Spin_IndentInc
        end
        object Label_UndoLimit: TLabel
          Left = 10
          Top = 17
          Width = 159
          Height = 12
          AutoSize = False
          Caption = '&Undo buffer limit:'
          FocusControl = Spin_UndoLimit
        end
        object lbl1: TLabel
          Left = 10
          Top = 130
          Width = 159
          Height = 13
          AutoSize = False
          Caption = 'Spacing between bullet and text:'
          FocusControl = txtSepInLists
        end
        object Spin_UndoLimit: TSpinEdit
          Left = 175
          Top = 11
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
          Top = 43
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
          Top = 71
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
          Top = 99
          Width = 56
          Height = 22
          Hint = 'Increment "space before/after paragraph" by this value'
          MaxLength = 3
          MaxValue = 255
          MinValue = 1
          TabOrder = 3
          Value = 2
        end
        object txtSepInLists: TEdit
          Left = 175
          Top = 127
          Width = 56
          Height = 21
          Hint = 
            'Factor (F) used to calculate spacing between bullet and text for' +
            ' all list types: '#13#10'S = 10 * FontSize / F'
          TabOrder = 4
          OnExit = txtSepInListsExit
        end
      end
    end
    object PG_Images: TPage
      Left = 0
      Top = 0
      HelpContext = 90
      Caption = 'PG_Images'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Images: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = 'Images in Editor'
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label23: TLabel
          Left = 5
          Top = 151
          Width = 153
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Default Format from clipboard'
          FocusControl = CbImgDefaultFormatFromClipb
        end
        object Label24: TLabel
          Left = 13
          Top = 118
          Width = 146
          Height = 13
          AutoSize = False
          Caption = 'Compression in ZIP Storage'
          FocusControl = CbImgDefaultCompression
        end
        object Label25: TLabel
          Left = 71
          Top = 178
          Width = 88
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Bmp pixel format'
          FocusControl = CbImgBmpPixelFormat
        end
        object Label27: TLabel
          Left = 13
          Top = 209
          Width = 146
          Height = 13
          AutoSize = False
          Caption = 'Max. auto width on Insert'
          FocusControl = txtImgMaxAutoWidthGoal
        end
        object Label28: TLabel
          Left = 13
          Top = 91
          Width = 145
          Height = 13
          AutoSize = False
          Caption = 'Storage mode on export'
          FocusControl = CbImgStorageModeOnExport
        end
        object Label30: TLabel
          Left = 232
          Top = 150
          Width = 60
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Ratio size'
          FocusControl = txtImgRatioSizePngVsJPG
        end
        object Label31: TLabel
          Left = 235
          Top = 171
          Width = 58
          Height = 37
          Hint = 
            '>0 => will  compare JPG and PNG conversions, using alt.format if' +
            ' Size(Selected)/ Size(Alt) > ratio'
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Compress quality'
          FocusControl = txtImgCompressionQuality
          WordWrap = True
        end
        object chkImgDefaultLinkMode: TCheckBox
          Left = 11
          Top = 294
          Width = 187
          Height = 17
          Hint = 
            'Linked files: accesed from their locations. Never deleted'#13#10'Non L' +
            'inked: copied to KNT storage[s]. Can be deleted'
          Caption = '&Link to files by default (no copies)'
          TabOrder = 10
        end
        object chkImgLinkRelativePath: TCheckBox
          Left = 11
          Top = 317
          Width = 187
          Height = 17
          Hint = 
            'Will be saved relative to document'#39's path (applies to new images' +
            ')'
          Caption = '&Relative path in Linked files'
          TabOrder = 11
        end
        object CbImgDefaultFormatFromClipb: TComboBox
          Left = 165
          Top = 147
          Width = 61
          Height = 21
          Hint = 'How to save [BMP] images when inserted from clipboard'
          Style = csDropDownList
          TabOrder = 3
        end
        object chkImgUseRecycleBin: TCheckBox
          Left = 11
          Top = 340
          Width = 187
          Height = 17
          Caption = 'Use Recycle bin'
          TabOrder = 12
        end
        object CbImgDefaultCompression: TComboBox
          Left = 166
          Top = 114
          Width = 116
          Height = 21
          Hint = 'Applies to new files added, individually'
          Style = csDropDownList
          TabOrder = 2
        end
        object CbImgBmpPixelFormat: TComboBox
          Left = 165
          Top = 174
          Width = 61
          Height = 21
          Hint = 'Color depth used on images from clipboard'
          Style = csDropDownList
          TabOrder = 5
        end
        object CbImgStorageModeOnExport: TComboBox
          Left = 166
          Top = 87
          Width = 116
          Height = 21
          Style = csDropDownList
          TabOrder = 1
        end
        object txtImgMaxAutoWidthGoal: TEdit
          Left = 165
          Top = 205
          Width = 61
          Height = 21
          Hint = 
            'Max. visible width used on  inserting a new image:'#13#10'0: no  limit' +
            '  <0 : limit to visible size of editor >0: as indicated'
          Alignment = taCenter
          MaxLength = 127
          TabOrder = 7
          OnExit = txtImgMaxAutoWidthGoalExit
        end
        object txtImgRatioSizePngVsJPG: TEdit
          Left = 301
          Top = 147
          Width = 38
          Height = 21
          Hint = 
            '>0 => will  compare JPG and PNG conversions, using alt.format if' +
            ' Size(Selected)/ Size(Alt) > ratio'
          Alignment = taCenter
          MaxLength = 127
          TabOrder = 4
          OnExit = txtImgRatioSizePngVsJPGExit
        end
        object txtImgCompressionQuality: TEdit
          Left = 300
          Top = 174
          Width = 39
          Height = 21
          Hint = 'For JPG format:  0 - 100'
          Alignment = taCenter
          MaxLength = 127
          TabOrder = 6
          OnExit = txtImgCompressionQualityExit
        end
        object gbViewer: TGroupBox
          Left = 204
          Top = 266
          Width = 137
          Height = 91
          Caption = ' Viewer '
          TabOrder = 13
          object Label29: TLabel
            Left = 41
            Top = 70
            Width = 60
            Height = 13
            AutoSize = False
            Caption = 'BG Color'
          end
          object btnBGColor: TBitBtn
            Left = 10
            Top = 63
            Width = 25
            Height = 23
            Hint = 'Background color in image viewer'
            ImageIndex = 10
            Images = Form_Main.IMG_Format
            TabOrder = 2
            OnClick = btnBGColorClick
          end
          object chkImgSingleViewerInstance: TCheckBox
            Left = 18
            Top = 19
            Width = 110
            Height = 17
            Caption = 'Single instance'
            TabOrder = 0
            OnClick = chkImgSingleViewerInstanceClick
          end
          object chkImgHotTrackViewer: TCheckBox
            Left = 40
            Top = 39
            Width = 89
            Height = 17
            Hint = 
              'If the viewer is open, show any image by selecting or clicking i' +
              't (link or image) -- Editor retains focus'
            Caption = 'Hot track'
            TabOrder = 1
          end
        end
        object chkImgSaveInSubfolders: TCheckBox
          Left = 11
          Top = 250
          Width = 187
          Height = 17
          Hint = 
            'In external storage:'#13#10'Save files in subfolders, using the name o' +
            'f the note folder where the image is first added'
          Caption = 'Save in subfolders'
          TabOrder = 8
        end
        object gbStorage: TGroupBox
          Left = 6
          Top = 21
          Width = 338
          Height = 55
          TabOrder = 0
          object Label26: TLabel
            Left = 1
            Top = 24
            Width = 44
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Mode'
            FocusControl = CbImgDefaultStorageMode
            StyleElements = [seClient, seBorder]
          end
          object Label22: TLabel
            Left = 225
            Top = 24
            Width = 41
            Height = 13
            AutoSize = False
            Caption = 'External'
            FocusControl = cbImgDefaultExternalStorage
          end
          object Label32: TLabel
            Left = 2
            Top = -2
            Width = 202
            Height = 13
            Alignment = taRightJustify
            Caption = '  Changes in current file:  File | Properties '
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGray
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            StyleElements = [seClient, seBorder]
          end
          object Label33: TLabel
            Left = 216
            Top = -2
            Width = 117
            Height = 13
            Alignment = taCenter
            AutoSize = False
            Caption = ' Storage in New files  '
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clNavy
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            Transparent = False
            StyleElements = [seClient, seBorder]
          end
          object CbImgDefaultStorageMode: TComboBox
            Left = 53
            Top = 20
            Width = 157
            Height = 21
            Hint = 'Storage mode to use in new KNT files by default'
            Style = csDropDownList
            TabOrder = 0
          end
          object cbImgDefaultExternalStorage: TComboBox
            Left = 272
            Top = 19
            Width = 60
            Height = 21
            Hint = 'Default external storage type to use on new files'
            Style = csDropDownList
            TabOrder = 1
          end
        end
        object chkImgKeepOrigName: TCheckBox
          Left = 11
          Top = 271
          Width = 187
          Height = 17
          Hint = 
            'Try to keep the original name of unlinked files on storage'#13#10'(if ' +
            'not set, the ID will be added as a prefix)'
          Caption = 'Keep original file name'
          TabOrder = 9
        end
      end
    end
    object PG_Tree: TPage
      Left = 0
      Top = 0
      HelpContext = 91
      Caption = 'PG_Tree'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GBox_TreeGlobal: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' Global tree panel settings '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label17: TLabel
          Left = 10
          Top = 30
          Width = 121
          Height = 13
          Caption = 'Initial tree e&xpand mode:'
          FocusControl = Combo_ExpandMode
        end
        object CheckBox_EditNewNodes: TCheckBox
          Left = 10
          Top = 84
          Width = 311
          Height = 17
          Hint = 'Always edit name of newly created nodes'
          Caption = '&After creating a new node, switch to edit mode'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object CheckBox_EditInPlace: TCheckBox
          Left = 10
          Top = 104
          Width = 311
          Height = 17
          Hint = 'Edit node names directly in tree'
          Caption = '&Edit nodes "in place" (no dialog box)'
          TabOrder = 2
        end
        object CheckBox_AutoNameVNodes: TCheckBox
          Left = 10
          Top = 132
          Width = 311
          Height = 17
          Hint = 'Use linked file name as virtual node name'
          Caption = 'Auto&matically name nodes where possible'
          TabOrder = 3
        end
        object CB_InheritNodeProperties: TCheckBox
          Left = 10
          Top = 156
          Width = 311
          Height = 17
          Hint = 'Newly added node inherits checked, bold and color state'
          Caption = 'Inherit &properties from active node'
          TabOrder = 4
        end
        object CheckBox_HotTrackTree: TCheckBox
          Left = 10
          Top = 200
          Width = 311
          Height = 17
          Hint = 'Tree selection follows mouse pointer'
          Caption = '&Hot track tree nodes'
          TabOrder = 5
        end
        object CheckBox_AutoScroll: TCheckBox
          Left = 10
          Top = 220
          Width = 311
          Height = 17
          Hint = 'Auto scroll tree when text doesn'#39't fit in window'
          Caption = 'A&uto scroll tree on mouse movement'
          TabOrder = 6
        end
        object CheckBox_TreeTips: TCheckBox
          Left = 10
          Top = 256
          Width = 311
          Height = 17
          Hint = 'Show node name in tooltip when too long to fit in window '
          Caption = 'Show &tooltips in tree'
          TabOrder = 7
        end
        object CB_ShowFullPath: TCheckBox
          Left = 10
          Top = 276
          Width = 204
          Height = 17
          Hint = 'Display only name or full path of selected node'
          Caption = 'Sh&ow full node path in status bar'
          TabOrder = 8
          OnClick = CB_ShowFullPathClick
        end
        object CB_ShowFullPathSearch: TCheckBox
          Left = 10
          Top = 296
          Width = 311
          Height = 17
          Hint = 'Display name or full path in search results panel'
          Caption = 'Show full node path in search &results'
          TabOrder = 10
        end
        object CB_PathTopToBottom: TCheckBox
          Left = 226
          Top = 276
          Width = 113
          Height = 17
          Hint = 'Display node path in reverse order (for use only in status bar)'
          Caption = 'Reverse or&der'
          TabOrder = 9
        end
        object Combo_ExpandMode: TComboBox
          Left = 10
          Top = 51
          Width = 303
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object PG_KNTFiles: TPage
      Left = 0
      Top = 0
      HelpContext = 92
      Caption = 'PG_KNTFiles'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Files1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 145
        Caption = ' On program startup... '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object TB_OpenDlgUserFile: TToolbarButton97
          Left = 318
          Top = 76
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgUserFileClick
        end
        object CB_LoadLastFile: TCheckBox
          Left = 14
          Top = 31
          Width = 143
          Height = 17
          Hint = 'Open the file which was used the last time'
          Caption = '&Load last-used file'
          TabOrder = 0
        end
        object CB_LoadUserFile: TCheckBox
          Left = 14
          Top = 55
          Width = 145
          Height = 17
          Hint = 'Open the specified file on startup'
          Caption = 'Load &specific file'
          TabOrder = 1
        end
        object Edit_UserFile: TEdit
          Left = 33
          Top = 77
          Width = 279
          Height = 21
          Hint = 'Enter a filename'
          TabOrder = 2
        end
        object CB_AutoNewFile: TCheckBox
          Left = 14
          Top = 107
          Width = 325
          Height = 17
          Caption = '&Create a blank new file if no file is loaded'
          TabOrder = 3
        end
      end
      object GroupBox_Files2: TGroupBox
        Left = 5
        Top = 167
        Width = 350
        Height = 152
        Caption = ' Auto save '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label_Minutes: TLabel
          Left = 176
          Top = 79
          Width = 37
          Height = 13
          Caption = 'minutes'
        end
        object Checkbox_AutoSave: TCheckBox
          Left = 14
          Top = 31
          Width = 325
          Height = 17
          Hint = 'Automatically save note files when necessary'
          Caption = '&Automatically save changes in KeyNote file:'
          TabOrder = 0
        end
        object CheckBox_AutoSaveOnFocus: TCheckBox
          Left = 37
          Top = 58
          Width = 287
          Height = 17
          Hint = 
            'Save file when KeyNote loses focus'#13#10#13#10'This option is not recomme' +
            'nded in normal situations, specially in big files.'#13#10#13#10'It is bett' +
            'er to autosave every X minutes '#13#10'AND to set the option "Backup a' +
            't regular intervals" (highly recommended) '#13#10'and "Backup original' +
            ' file when saving changes"'
          Caption = '&When you switch to another application (*)'
          TabOrder = 1
          StyleElements = [seClient, seBorder]
          OnClick = CheckBox_AutoSaveOnFocusClick
        end
        object CheckBox_AutoSaveOnTimer: TCheckBox
          Left = 37
          Top = 78
          Width = 63
          Height = 17
          Hint = 'Save file at regular intervals'
          Caption = '&Every'
          TabOrder = 2
        end
        object Spin_AutoSaveOnTimerInt: TSpinEdit
          Left = 109
          Top = 76
          Width = 61
          Height = 22
          Hint = 'How often to save changes automatically'
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 3
          Value = 10
        end
        object CB_SkipNewFilePrompt: TCheckBox
          Left = 14
          Top = 111
          Width = 325
          Height = 17
          Caption = '&Do not prompt to save new files'
          TabOrder = 4
        end
      end
    end
    object PG_FileOptions: TPage
      Left = 0
      Top = 0
      HelpContext = 93
      Caption = 'PG_FileOptions'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_FileOpt1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 108
        Caption = ' Recently used files list '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object CheckBox_MRUSubmenu: TCheckBox
          Left = 13
          Top = 49
          Width = 330
          Height = 17
          Hint = 'Display recent files in a separate submenu'
          Caption = 'Display as &Submenu'
          TabOrder = 2
        end
        object CheckBox_MRUUse: TCheckBox
          Left = 13
          Top = 29
          Width = 227
          Height = 17
          Hint = 'Keep list of most recently used knt files'
          Caption = '&Remember recently used files:'
          TabOrder = 0
        end
        object CheckBox_MRUFullPath: TCheckBox
          Left = 13
          Top = 69
          Width = 330
          Height = 17
          Hint = 'Display full path in recent files list'
          Caption = 'Show f&ull path in MRU menu'
          TabOrder = 3
        end
        object Spin_MRUCount: TSpinEdit
          Left = 244
          Top = 26
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
      object GroupBox_FileOpt2: TGroupBox
        Left = 5
        Top = 124
        Width = 350
        Height = 101
        Caption = ' Open in Read-Only mode : '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object CheckBox_OpenFloppyReadOnly: TCheckBox
          Left = 13
          Top = 25
          Width = 330
          Height = 17
          Hint = 'Open files on diskettes in read-only mode'
          Caption = 'Files on &floppy disks'
          TabOrder = 0
        end
        object CheckBox_OpenReadOnlyWarn: TCheckBox
          Left = 13
          Top = 65
          Width = 330
          Height = 17
          Hint = 'Display warning when file is opened as read-only'
          Caption = '&Warn when opening file in  Read-Only mode'
          TabOrder = 1
        end
        object CheckBox_OpenNetworkReadOnly: TCheckBox
          Left = 13
          Top = 45
          Width = 330
          Height = 17
          Hint = 'Open files on network drives in read-only mode'
          Caption = 'Files on &network drives'
          TabOrder = 2
        end
      end
      object GroupBox_FileOpt3: TGroupBox
        Left = 5
        Top = 238
        Width = 350
        Height = 107
        Caption = ' Registered file types '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 2
        object CheckBox_AutoRegisterPrompt: TCheckBox
          Left = 13
          Top = 46
          Width = 330
          Height = 17
          Hint = 'Prompt before registering Keynote file type'
          Caption = '&Prompt before creating file association'
          TabOrder = 0
        end
        object CheckBox_AutoRegisterFileType: TCheckBox
          Left = 13
          Top = 26
          Width = 330
          Height = 17
          Hint = 'Register Keynote file extension at startup'
          Caption = 'Aut&o register file type (associate .KNT files with KeyNote)'
          TabOrder = 1
        end
        object CheckBox_EncFileAltExt: TCheckBox
          Left = 13
          Top = 66
          Width = 330
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
      HelpContext = 94
      Caption = 'PG_BackupOptions'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Back: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' Backup options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label_BakDir: TLabel
          Left = 16
          Top = 240
          Width = 145
          Height = 13
          Caption = '&Directory for backup files:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label_MaxBak2: TLabel
          Left = 142
          Top = 178
          Width = 122
          Height = 13
          AutoSize = False
          Caption = '&Max backup level:'
          FocusControl = Combo_BakLevel
        end
        object Bevel3: TBevel
          Left = 14
          Top = 214
          Width = 320
          Height = 2
          Shape = bsTopLine
        end
        object Label_MaxBak1: TLabel
          Left = 32
          Top = 142
          Width = 297
          Height = 26
          AutoSize = False
          Caption = 
            'Up to 9 most recent backup files can be kept. '#13#10'Oldest backups w' +
            'ill be recycled automatically.'
        end
        object TB_OpenDlgBakDir: TToolbarButton97
          Left = 309
          Top = 317
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgBakDirClick
        end
        object checkbox_Backup: TCheckBox
          Left = 13
          Top = 62
          Width = 311
          Height = 17
          Hint = 'Create backup file when saving'
          Caption = '&Backup original file when saving changes'
          TabOrder = 1
        end
        object CheckBox_BackupAppendExt: TCheckBox
          Left = 33
          Top = 88
          Width = 228
          Height = 17
          Hint = 'Add backup extension to end of original filename'
          Caption = '&Append extension to original filename:'
          TabOrder = 2
        end
        object Edit_BackupExt: TEdit
          Left = 274
          Top = 86
          Width = 44
          Height = 21
          Hint = 'Extension to use for Backup files'
          MaxLength = 9
          TabOrder = 3
        end
        object RB_BakOriginalDir: TRadioButton
          Left = 22
          Top = 267
          Width = 242
          Height = 17
          Hint = 'Create backup file in the same folder as original file'
          Caption = '&Original file'#39's directory'
          Checked = True
          TabOrder = 5
          TabStop = True
          OnClick = RB_BakOriginalDirClick
        end
        object RB_BakUserDir: TRadioButton
          Left = 22
          Top = 294
          Width = 242
          Height = 17
          Hint = 'Create backup files in specified folder'
          Caption = '&Separate directory:'
          TabOrder = 6
          OnClick = RB_BakOriginalDirClick
        end
        object Edit_BakDir: TEdit
          Left = 48
          Top = 317
          Width = 255
          Height = 21
          Hint = 'Specify folder for backup files'
          TabOrder = 7
        end
        object Combo_BakLevel: TComboBox
          Left = 272
          Top = 175
          Width = 45
          Height = 21
          Style = csDropDownList
          DropDownCount = 10
          TabOrder = 8
        end
        object CB_BackupVNodes: TCheckBox
          Left = 33
          Top = 111
          Width = 295
          Height = 17
          Hint = 'Backup files linked to virtual nodes'
          Caption = 'Also back up &virtual node files'
          TabOrder = 4
        end
        object CB_BackupRegularIntervals: TCheckBox
          Left = 13
          Top = 33
          Width = 311
          Height = 17
          Hint = 
            'Create copies of the file daily, weekly and montly. '#13#10'Montly cop' +
            'ies will not be deleted nor replaced by KN'#13#10#13#10'* Highly recommend' +
            'ed'
          Caption = '&Backup at regular intervals (*)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          StyleElements = [seClient, seBorder]
        end
      end
    end
    object PG_Actions: TPage
      Left = 0
      Top = 0
      HelpContext = 95
      Caption = 'PG_Actions'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Actions1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 192
        Caption = ' Actions on program inactivity '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label9: TLabel
          Left = 226
          Top = 34
          Width = 56
          Height = 13
          Caption = 'minutes idle'
        end
        object Label10: TLabel
          Left = 226
          Top = 74
          Width = 56
          Height = 13
          Caption = 'minutes idle'
        end
        object Bevel4: TBevel
          Left = 11
          Top = 62
          Width = 330
          Height = 2
          Shape = bsTopLine
        end
        object CheckBox_TimerMinimize: TCheckBox
          Left = 11
          Top = 34
          Width = 155
          Height = 17
          Hint = 'Automatically hide program when inactive'
          Caption = '&Minimize KeyNote after '
          TabOrder = 0
        end
        object CheckBox_TimerClose: TCheckBox
          Left = 11
          Top = 74
          Width = 155
          Height = 17
          Hint = 'Automatically close file when inactive (AutoSave must be ON!)'
          Caption = '&Close Notes file after'
          TabOrder = 2
        end
        object CB_CloseEncOnly: TCheckBox
          Left = 29
          Top = 101
          Width = 291
          Height = 17
          Hint = 'Apply AutoClose function only to encrypted files'
          Caption = 'Auto-close &only encrypted files'
          TabOrder = 4
        end
        object Spin_TimerMinInt: TSpinEdit
          Left = 168
          Top = 32
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
          Top = 72
          Width = 49
          Height = 22
          Hint = 'How long to wait before auto-closing files'
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 3
          Value = 10
        end
        object CB_TimerCloseDialogs: TCheckBox
          Left = 11
          Top = 134
          Width = 330
          Height = 17
          Hint = 'If unchecked, auto-close will abort of any dialog boxes are open'
          Caption = 'If any &dialog boxes are open, force them to close'
          TabOrder = 5
        end
        object CB_TimerCloseAutoReopen: TCheckBox
          Left = 11
          Top = 157
          Width = 330
          Height = 17
          Hint = 'Prompt for password and reopen auto-closed encrypted files'
          Caption = '&Reopen auto-closed files on restore (encrypted only)'
          TabOrder = 6
        end
      end
      object GroupBox_Act2: TGroupBox
        Left = 5
        Top = 205
        Width = 350
        Height = 164
        Caption = ' URL actions '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label8: TLabel
          Left = 10
          Top = 36
          Width = 87
          Height = 13
          AutoSize = False
          Caption = 'On &URL click:'
          FocusControl = Combo_URLAction
        end
        object Label21: TLabel
          Left = 11
          Top = 59
          Width = 86
          Height = 13
          AutoSize = False
          Caption = '  Ctrl + click:'
          FocusControl = Combo_URLCtrlAction
        end
        object Combo_URLAction: TComboBox
          Left = 101
          Top = 31
          Width = 220
          Height = 21
          Hint = 'Remember: '#13#10'  Right Click  => Prompt'#13#10'  ALT+ Click  => Copy'
          Style = csDropDownList
          TabOrder = 0
        end
        object CheckBox_MinOnURL: TCheckBox
          Left = 11
          Top = 89
          Width = 330
          Height = 17
          Hint = 'Minimize program on launching a client application for URL'
          Caption = 'Minimize &KeyNote on URL launch'
          TabOrder = 2
        end
        object CheckBox_URLFileAuto: TCheckBox
          Left = 11
          Top = 113
          Width = 330
          Height = 17
          Hint = 'Never prompt when file:// URL clicked'
          Caption = '&Launch "file://" URLs without prompting'
          TabOrder = 3
        end
        object Combo_URLCtrlAction: TComboBox
          Left = 101
          Top = 57
          Width = 220
          Height = 21
          Style = csDropDownList
          TabOrder = 1
        end
        object CB_ExtKNTLnkInNewInst: TCheckBox
          Left = 11
          Top = 137
          Width = 330
          Height = 17
          Hint = 
            'KNT links to another .knt file will be opened in other instance,' +
            ' not in current one'
          Caption = 'Open external KNT links in other &instance'
          TabOrder = 4
        end
      end
    end
    object PG_Confirmations: TPage
      Left = 0
      Top = 0
      HelpContext = 96
      Caption = 'PG_Confirmations'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Conf: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' Confirmation options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Bevel6: TBevel
          Left = 10
          Top = 179
          Width = 325
          Height = 2
          Shape = bsTopLine
        end
        object CheckBox_AutoPasteEval: TCheckBox
          Left = 10
          Top = 200
          Width = 311
          Height = 17
          Hint = 'Immediately paste evaluation result into note'
          Caption = '&Automatically paste expression evaluation results'
          TabOrder = 6
        end
        object CheckBox_AutoPastePlugin: TCheckBox
          Left = 10
          Top = 220
          Width = 311
          Height = 17
          Hint = 'Immediately insert text generated by plugins'
          Caption = 'A&utomatically insert text from plugins'
          TabOrder = 7
        end
        object CheckBox_ConfirmTreePaste: TCheckBox
          Left = 10
          Top = 104
          Width = 311
          Height = 17
          Hint = 'Ask for confirmation before pasting tree nodes'
          Caption = 'Confirm &pasting tree nodes'
          TabOrder = 3
        end
        object checkbox_ConfirmExit: TCheckBox
          Left = 10
          Top = 30
          Width = 311
          Height = 17
          Hint = 'Prompt before closing program'
          Caption = '&Confirm closing KeyNote'
          TabOrder = 0
        end
        object checkbox_ConfirmDelete: TCheckBox
          Left = 10
          Top = 55
          Width = 311
          Height = 17
          Hint = 'Prompt before removing a folder'
          Caption = 'Confirm &deleting folders'
          TabOrder = 1
        end
        object CheckBox_ConfirmNodeDelete: TCheckBox
          Left = 10
          Top = 76
          Width = 311
          Height = 17
          Hint = 'Prompt before removing a node'
          Caption = 'Confirm deleting tree &nodes'
          TabOrder = 2
        end
        object CB_ConfirmNodeRefresh: TCheckBox
          Left = 10
          Top = 127
          Width = 311
          Height = 17
          Hint = 'Ask for confirmation when "Refresh" command used'
          Caption = 'Confirm refreshing &virtual nodes'
          TabOrder = 4
        end
        object GroupBox18: TGroupBox
          Left = 10
          Top = 261
          Width = 319
          Height = 60
          Caption = ' When dropping tree nodes on another tab: '
          TabOrder = 8
          object CB_DropNodesOnTabPrompt: TCheckBox
            Left = 15
            Top = 23
            Width = 300
            Height = 17
            Caption = 'C&onfirm before transferring nodes'
            TabOrder = 0
          end
        end
        object CB_TreeClipConfirm: TCheckBox
          Left = 10
          Top = 147
          Width = 311
          Height = 17
          Caption = 'Confirm starting clipboard capture when tree panel is visible'
          TabOrder = 5
        end
      end
    end
    object PG_Chrome: TPage
      Left = 0
      Top = 0
      HelpContext = 97
      Caption = 'PG_Chrome'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Chrome2: TGroupBox
        Left = 5
        Top = 184
        Width = 350
        Height = 185
        Caption = ' Folder tabs:  Options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label15: TLabel
          Left = 17
          Top = 101
          Width = 90
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = '&Tab position:'
          FocusControl = Combo_TabPos
        end
        object lblTab: TLabel
          Left = 116
          Top = 163
          Width = 217
          Height = 13
          Caption = '(*) : Will take effect after restarting KeyNote'
        end
        object lbl2: TLabel
          Left = 311
          Top = 112
          Width = 14
          Height = 13
          Caption = '(*)'
        end
        object CheckBox_TabsStacked: TCheckBox
          Left = 27
          Top = 125
          Width = 294
          Height = 17
          Hint = 'Arrange tabs in rows when necesary'
          Caption = '&Multiline tabs (arrange in rows when cannot fit)'
          TabOrder = 3
        end
        object CheckBox_TabsHotTrack: TCheckBox
          Left = 17
          Top = 53
          Width = 320
          Height = 17
          Hint = 'Hot-track folder tabs with the mouse'
          Caption = '&Hot track in tabs'
          TabOrder = 1
        end
        object CheckBox_TabsImages: TCheckBox
          Left = 17
          Top = 33
          Width = 320
          Height = 17
          Hint = 'Display pictures on folder tabs'
          Caption = '&Show icons on tabs'
          TabOrder = 0
        end
        object Combo_TabPos: TComboBox
          Left = 111
          Top = 98
          Width = 185
          Height = 21
          Style = csDropDownList
          TabOrder = 2
        end
      end
      object GroupBox_Chrome1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 168
        Caption = ' Folder tabs:  Font and colors '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label5: TLabel
          Left = 15
          Top = 33
          Width = 60
          Height = 13
          Caption = 'Settings for:'
        end
        object RB_ActiveTab: TRadioButton
          Left = 23
          Top = 56
          Width = 127
          Height = 17
          Hint = 'Settings for active tab'
          Caption = '&Active tab'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_InactiveTab: TRadioButton
          Left = 156
          Top = 56
          Width = 146
          Height = 17
          Hint = 'Settings for inactive tabs'
          Caption = '&Inactive tabs'
          TabOrder = 1
        end
        object Edit_Sample: TEdit
          Left = 25
          Top = 118
          Width = 303
          Height = 21
          TabStop = False
          MaxLength = 127
          TabOrder = 5
          Text = 'Sample font'
        end
        object BTN_Font: TBitBtn
          Left = 27
          Top = 82
          Width = 90
          Height = 25
          Hint = 'Select tab font attributes'
          Caption = '&Font'
          ImageIndex = 11
          Images = Form_Main.IMG_Format
          TabOrder = 2
        end
        object BTN_Color: TBitBtn
          Left = 131
          Top = 82
          Width = 90
          Height = 25
          Hint = 'Select tab color'
          Caption = '&Color'
          ImageIndex = 10
          Images = Form_Main.IMG_Format
          TabOrder = 3
        end
        object BTN_Defaults: TBitBtn
          Left = 237
          Top = 82
          Width = 90
          Height = 25
          Hint = 'Restore default tab font and color'
          Caption = '&Reset'
          ImageIndex = 6
          Images = Form_Main.IMG_Toolbar
          TabOrder = 4
        end
      end
    end
    object PG_Icons: TPage
      Left = 0
      Top = 0
      HelpContext = 98
      Caption = 'PG_Icons'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_TabIcons: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' &Tab icons '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label_ICN: TLabel
          Left = 191
          Top = 340
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Button_ICNAdd: TButton
          Left = 186
          Top = 31
          Width = 91
          Height = 25
          Hint = 'Add a new icon to end of list'
          Caption = '&Add...'
          TabOrder = 1
          OnClick = Button_ICNAddClick
        end
        object Button_ICNInsert: TButton
          Left = 186
          Top = 61
          Width = 91
          Height = 25
          Hint = 'Insert a new icon at this position'
          Caption = '&Insert...'
          TabOrder = 2
          OnClick = Button_ICNInsertClick
        end
        object Button_ICNDelete: TButton
          Left = 186
          Top = 91
          Width = 91
          Height = 25
          Hint = 'Remove selected icon from list'
          Caption = '&Delete'
          TabOrder = 3
          OnClick = Button_ICNDeleteClick
        end
        object Button_ICNReset: TButton
          Left = 186
          Top = 121
          Width = 91
          Height = 25
          Hint = 'Restore factory default icons'
          Caption = '&Reset'
          TabOrder = 4
          OnClick = Button_ICNResetClick
        end
        object List_Icn: TGFXListBox
          Left = 22
          Top = 31
          Width = 143
          Height = 322
          ExtendedSelect = False
          ItemHeight = 20
          TabOrder = 0
        end
      end
    end
    object PG_Advanced: TPage
      Left = 0
      Top = 0
      HelpContext = 99
      Caption = 'PG_Advanced'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Adv: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' Advanced settings '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label16: TLabel
          Left = 42
          Top = 334
          Width = 292
          Height = 13
          Caption = 'Settings marked (*) will take effect after restarting KeyNote.'
        end
        object Bevel1: TBevel
          Left = 10
          Top = 141
          Width = 325
          Height = 2
          Shape = bsTopLine
        end
        object CheckBox_DisableFileMon: TCheckBox
          Left = 10
          Top = 33
          Width = 330
          Height = 17
          Hint = 'Do not monitor .KNT file for changes (compatibility option)'
          Caption = '&Disable folder monitor (*)'
          TabOrder = 0
        end
        object CheckBox_ShowFonts: TCheckBox
          Left = 10
          Top = 60
          Width = 330
          Height = 17
          Hint = 'Display font names using actual font samples'
          Caption = '&Show font samples in Font combo box (*)'
          TabOrder = 1
        end
        object CheckBox_NoRegistry: TCheckBox
          Left = 10
          Top = 246
          Width = 330
          Height = 17
          Hint = 'Keep form data in .MRU file'
          Caption = 'Do not save &window settings to registry'
          TabOrder = 7
        end
        object CheckBox_UseOldColorDlg: TCheckBox
          Left = 10
          Top = 166
          Width = 330
          Height = 17
          Hint = 
            'Use traditional color selection dialogs or apply current button ' +
            'color'
          Caption = '&Old style color selection dialogs'
          TabOrder = 4
        end
        object CheckBox_RunAutoMacros: TCheckBox
          Left = 10
          Top = 186
          Width = 330
          Height = 17
          Hint = 'Execute Auto-run macros on creating files, folders and notes'
          Caption = '&Allow Auto-run macros'
          TabOrder = 5
        end
        object CheckBox_FixScrollBars: TCheckBox
          Left = 10
          Top = 226
          Width = 330
          Height = 17
          Hint = 
            'Trying to resolve the issue of scrollbars not being updated (eg.' +
            ' on Linux-Wine)'
          Caption = '&Fix scroll bars'
          TabOrder = 6
        end
        object CheckBox_LongCombos: TCheckBox
          Left = 10
          Top = 81
          Width = 330
          Height = 17
          Hint = 'Display slightly wider drop-down lists on toolbars'
          Caption = '&Wider drop-down lists on toolbars (*)'
          TabOrder = 2
        end
        object CheckBox_RichEditv3: TCheckBox
          Left = 10
          Top = 104
          Width = 330
          Height = 17
          Hint = 'Prevent formatting loss when using version 3.0 of riched20.dll'
          Caption = 'Enable &Rich Edit version 3.0 fixes (*)'
          TabOrder = 3
        end
        object CB_IgnoreUpgrades: TCheckBox
          Left = 10
          Top = 266
          Width = 330
          Height = 17
          Hint = 'Do not display any information when KeyNote is upgraded'
          Caption = 'Do not display &upgrade information'
          TabOrder = 9
        end
        object CB_ResPanelActiveUpdate: TCheckBox
          Left = 10
          Top = 286
          Width = 330
          Height = 17
          Caption = 'Reload Resource panel data when displaying'
          TabOrder = 8
        end
      end
    end
    object PG_Formats: TPage
      Left = 0
      Top = 0
      HelpContext = 100
      Caption = 'PG_Formats'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Formats1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 172
        Caption = ' Date and time formats '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label2: TLabel
          Left = 22
          Top = 43
          Width = 87
          Height = 13
          AutoSize = False
          Caption = '&Date format:'
          FocusControl = Edit_DateFormat
        end
        object Label3: TLabel
          Left = 22
          Top = 93
          Width = 87
          Height = 13
          AutoSize = False
          Caption = '&Time format:'
          FocusControl = Edit_TimeFormat
        end
        object Label_SampleDate: TLabel
          Left = 113
          Top = 63
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Label_SampleTime: TLabel
          Left = 113
          Top = 113
          Width = 20
          Height = 13
          Caption = '(...)'
        end
        object Edit_DateFormat: TComboBox
          Left = 113
          Top = 38
          Width = 204
          Height = 21
          Hint = 'Format to use when inserting current date'
          TabOrder = 0
        end
        object Edit_TimeFormat: TComboBox
          Left = 113
          Top = 88
          Width = 204
          Height = 21
          Hint = 'Format to use when inserting current time'
          MaxLength = 99
          TabOrder = 1
        end
        object CB_DTUseLastSelection: TCheckBox
          Left = 22
          Top = 139
          Width = 295
          Height = 17
          Caption = '&Use last format selected in drop-down menu'
          TabOrder = 2
        end
      end
      object GroupBox_Formats2: TGroupBox
        Left = 5
        Top = 191
        Width = 350
        Height = 90
        Caption = '  Insert Character dialog box  '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object CheckBox_InsCharWinClose: TCheckBox
          Left = 22
          Top = 28
          Width = 320
          Height = 19
          Hint = 'Automatically close dialog box when characters inserted'
          Caption = '&Close dialog box on insert'
          TabOrder = 0
        end
      end
    end
    object PG_ClipCap: TPage
      Left = 0
      Top = 0
      HelpContext = 101
      Caption = 'PG_ClipCap'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label_PlainTextMode: TLabel
        Left = 8
        Top = 348
        Width = 92
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Plain text &mode:'
      end
      object Group_ClipboardCapture: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 251
        Caption = 'Clipboard Capture '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label7: TLabel
          Left = 18
          Top = 25
          Width = 37
          Height = 13
          Alignment = taRightJustify
          Caption = 'Divider:'
          FocusControl = Combo_ClipNodeNaming
        end
        object Label_MaxSize: TLabel
          Left = 200
          Top = 228
          Width = 59
          Height = 13
          Alignment = taRightJustify
          Caption = '&Limit size to:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Combo_Size: TComboBox
          Left = 266
          Top = 224
          Width = 58
          Height = 21
          Hint = 
            'Maximum length of text to capture, in bytes (when paste as plain' +
            ' text, and '#39'Plain text mode'#39' is '#39'Plain (without any formatting)'#39 +
            ' )'#13#10#13#10'(only for Clipboard Capture)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 6
          ParentColor = True
          ParentFont = False
          TabOrder = 9
        end
        object Combo_Divider: TComboBox
          Left = 62
          Top = 22
          Width = 240
          Height = 21
          Hint = 
            'Text to place between captured items'#13#10#13#10'Only selected divider (c' +
            'an be customized) will be saved in .ini file'
          DropDownCount = 14
          DropDownWidth = 290
          TabOrder = 1
        end
        object CB_IgnoreSelf: TCheckBox
          Left = 18
          Top = 66
          Width = 160
          Height = 17
          Hint = 'Do not capture text copied from Keynote itself'
          Caption = 'Ignore clips from &Keynote'
          TabOrder = 2
        end
        object CB_AsText: TCheckBox
          Left = 18
          Top = 226
          Width = 127
          Height = 16
          Hint = 
            'Paste text as defined by '#39'Plain text mode'#39'   (only for Clipboard' +
            ' Capture)'
          Caption = '&Paste as plain text'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object BitBtn_TknHlp: TBitBtn
          Left = 308
          Top = 20
          Width = 25
          Height = 25
          Hint = 'Click to display list of substitution tokens'
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 0
        end
        object CheckBox_ClipRecall: TCheckBox
          Left = 18
          Top = 86
          Width = 320
          Height = 17
          Hint = 'When opening a file, restore last active Capture folder'
          Caption = '&Remember capturing folder across program sessions'
          TabOrder = 8
        end
        object CheckBox_Sound: TCheckBox
          Left = 18
          Top = 47
          Width = 148
          Height = 17
          Hint = 'Indicate capture events with sound ("clip.wav")'
          Caption = 'Play &sound on capture'
          TabOrder = 6
        end
        object GroupBox23: TGroupBox
          Left = 15
          Top = 111
          Width = 317
          Height = 91
          Caption = '  When tree panel is visible :'
          DefaultHeaderFont = False
          HeaderFont.Charset = DEFAULT_CHARSET
          HeaderFont.Color = clNavy
          HeaderFont.Height = -11
          HeaderFont.Name = 'Tahoma'
          HeaderFont.Style = []
          TabOrder = 5
          StyleElements = [seClient, seBorder]
          object LB_ClipNodeNaming: TLabel
            Left = 48
            Top = 63
            Width = 81
            Height = 13
            Alignment = taRightJustify
            Caption = '&New node name:'
            FocusControl = Combo_ClipNodeNaming
          end
          object RB_ClipTreeActive: TRadioButton
            Left = 22
            Top = 19
            Width = 266
            Height = 17
            Hint = 
              'Clips will be stored in the node which is active when text is ca' +
              'ptured'
            Caption = 'Paste into currently selected node'
            TabOrder = 0
          end
          object RB_ClipTreeNew: TRadioButton
            Left = 22
            Top = 40
            Width = 266
            Height = 17
            Hint = 'New tree node will be created for each new clip'
            Caption = 'Create a new node and paste into it'
            Checked = True
            TabOrder = 1
            TabStop = True
          end
          object Combo_ClipNodeNaming: TComboBox
            Left = 135
            Top = 62
            Width = 174
            Height = 21
            Hint = 'Choose how new node will be named'
            Style = csDropDownList
            TabOrder = 2
          end
        end
        object CB_TestDupClips: TCheckBox
          Left = 196
          Top = 66
          Width = 155
          Height = 17
          Hint = 'Discard exact copies of most recently captured text'
          Caption = 'I&gnore duplicate clips'
          TabOrder = 3
        end
        object CB_SwitchIcon: TCheckBox
          Left = 196
          Top = 48
          Width = 149
          Height = 15
          Hint = 'Show alternate tray icon when clipboard capture is active'
          Caption = 'Use &alternate tray icon'
          TabOrder = 7
        end
        object CB_SourceURL: TCheckBox
          Left = 18
          Top = 207
          Width = 136
          Height = 17
          Hint = 
            'Insert clip source URL, with title (unless Divider includes toke' +
            'n %U) '#13#10#13#10'* only affects Clipboard Capture'
          Caption = 'Include source &URL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
        end
      end
      object Group_WebClip: TGroupBox
        Left = 5
        Top = 263
        Width = 350
        Height = 52
        Caption = '&Web Clip'
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 2
        object Label19: TLabel
          Left = 13
          Top = 23
          Width = 54
          Height = 13
          Alignment = taRightJustify
          Caption = 'Divider (*):'
          FocusControl = Combo_ClipNodeNaming
        end
        object Combo_WCDivider: TComboBox
          Left = 70
          Top = 20
          Width = 240
          Height = 21
          Hint = 
            'Text to place between captured items'#13#10'(*) If empty, '#39'Clip.Capt.'#39 +
            ' divider string will be used'#13#10#13#10'Only selected divider (can be cu' +
            'stomized) will be saved in .ini file'
          DropDownWidth = 290
          TabOrder = 0
        end
        object BitBtn_TknHlp2: TBitBtn
          Left = 316
          Top = 18
          Width = 25
          Height = 25
          Hint = 'Click to display list of substitution tokens'
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 1
        end
      end
      object Combo_PlainTextMode: TComboBox
        Left = 104
        Top = 346
        Width = 247
        Height = 21
        Hint = 
          'How to paste when used plain text mode'#13#10#13#10'Applies to all kind of' +
          ' paste (normal, clipboard capture and web clip)'
        MaxLength = 6
        TabOrder = 1
      end
      object CB_PlainDefaultPaste: TCheckBox
        Left = 20
        Top = 323
        Width = 333
        Height = 17
        Hint = 
          'Default paste (CTR+V or equivalent) as plain text when copied fr' +
          'om outside KN.'#13#10'How text is shown is determined by '#39'Plain text m' +
          'ode'#39
        Caption = 'Paste external as plain &text'
        TabOrder = 3
      end
    end
    object PG_FileTypes: TPage
      Left = 0
      Top = 0
      HelpContext = 102
      Caption = 'PG_FileTypes'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_FileTypes: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' &Text file extensions '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label13: TLabel
          Left = 11
          Top = 301
          Width = 334
          Height = 58
          AutoSize = False
          Caption = 
            'Enter extensions which KeyNote should treat as plain text files.' +
            ' '#13#10'This allows to recognize plain text files '#13#10'when importing th' +
            'em as folders.'
        end
        object List_TxtExt: TListBox
          Left = 22
          Top = 36
          Width = 101
          Height = 253
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
        end
        object Button_AddTxtExt: TButton
          Left = 145
          Top = 36
          Width = 89
          Height = 25
          Caption = '&Add...'
          TabOrder = 1
          OnClick = Button_AddTxtExtClick
        end
        object Button_DeleteTxtExt: TButton
          Left = 145
          Top = 66
          Width = 89
          Height = 25
          Caption = '&Delete'
          TabOrder = 2
          OnClick = Button_DeleteTxtExtClick
        end
        object Button_ResetTxtExt: TButton
          Left = 145
          Top = 96
          Width = 89
          Height = 25
          Caption = '&Reset'
          TabOrder = 3
          OnClick = Button_ResetTxtExtClick
        end
      end
    end
    object PG_FoldedBl: TPage
      Left = 0
      Top = 0
      HelpContext = 253
      Caption = 'PG_FoldedBl'
      object GroupBox_FoldingBl: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 364
        Caption = ' Folding Blocks '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object btnFBNew: TButton
          Left = 15
          Top = 327
          Width = 85
          Height = 25
          Caption = '&New..'
          TabOrder = 0
          OnClick = btnFBNewClick
        end
        object btnFBEdit: TButton
          Left = 108
          Top = 327
          Width = 85
          Height = 25
          Caption = '&Edit...'
          TabOrder = 1
          OnClick = btnFBEditClick
        end
        object btnFBDelete: TButton
          Left = 252
          Top = 327
          Width = 85
          Height = 25
          Caption = '&Delete'
          TabOrder = 2
          OnClick = btnFBDeleteClick
        end
        object LVfb: TListView
          Left = 15
          Top = 23
          Width = 322
          Height = 298
          Columns = <
            item
              Caption = 'Opening'
              Width = 55
            end
            item
              Caption = 'Closing'
              Width = 55
            end
            item
              Caption = 'Case Sens.'
              Width = 66
            end
            item
              Caption = 'Discard'
              Width = 52
            end
            item
              Caption = 'Add on Expand'
              Width = 85
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 3
          ViewStyle = vsReport
        end
      end
    end
    object PG_Language: TPage
      Left = 0
      Top = 0
      HelpContext = 103
      Caption = 'PG_Language'
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox_Other1: TGroupBox
        Left = 5
        Top = 5
        Width = 350
        Height = 121
        Caption = ' Advanced editor language options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object Label6: TLabel
          Left = 36
          Top = 85
          Width = 303
          Height = 32
          Alignment = taRightJustify
          AutoSize = False
          Caption = 
            'These settings will take effect after KeyNote is restarted '#13#10'or ' +
            'current file is closed and re-opened.'
          WordWrap = True
        end
        object CB_AutoFont: TCheckBox
          Left = 10
          Top = 30
          Width = 325
          Height = 17
          Caption = '&Automatically select font language '
          TabOrder = 0
        end
        object CB_AutoKeyboard: TCheckBox
          Left = 10
          Top = 50
          Width = 325
          Height = 17
          Caption = '&Automatically select &keyboard'
          TabOrder = 1
        end
      end
      object GroupBox_Other3: TGroupBox
        Left = 5
        Top = 252
        Width = 350
        Height = 117
        Caption = ' Find options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label20: TLabel
          Left = 13
          Top = 93
          Width = 126
          Height = 13
          Caption = 'Font size in Find All results'
        end
        object Label14: TLabel
          Left = 13
          Top = 69
          Width = 104
          Height = 13
          Hint = 'Request a new search pattern after N seconds (0, no timeout)'
          Caption = 'Reset Find Next after'
        end
        object CB_WordAtCursor: TCheckBox
          Left = 10
          Top = 24
          Width = 325
          Height = 17
          Hint = 'Prefills search pattern with current word'
          Caption = 'Search for &word at cursor'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CB_FindAutoClose: TCheckBox
          Left = 10
          Top = 45
          Width = 325
          Height = 17
          Hint = 'Close the "Find" dialog box when button clicked'
          Caption = 'Automatically &close Find dialog box'
          TabOrder = 1
        end
        object Spin_FontSizeFindResults: TSpinEdit
          Left = 208
          Top = 89
          Width = 49
          Height = 22
          MaxLength = 2
          MaxValue = 36
          MinValue = 8
          TabOrder = 3
          Value = 10
        end
        object Spin_ResetNextAftN: TSpinEdit
          Left = 208
          Top = 63
          Width = 49
          Height = 22
          MaxLength = 2
          MaxValue = 600
          MinValue = 0
          TabOrder = 2
          Value = 90
        end
      end
      object GroupBox_Other2: TGroupBox
        Left = 5
        Top = 132
        Width = 350
        Height = 111
        Caption = ' Web browser options '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 2
        object TB_OpenDlgURLAltBrowserPath: TToolbarButton97
          Left = 312
          Top = 76
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgURLAltBrowserPathClick
        end
        object RB_URLSystemBrowser: TRadioButton
          Left = 10
          Top = 27
          Width = 325
          Height = 17
          Caption = '&Use system default web browser'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_URLAltBrowser: TRadioButton
          Left = 10
          Top = 50
          Width = 325
          Height = 17
          Caption = 'Use the following &web browser:'
          TabOrder = 1
        end
        object Edit_URLAltBrowserPath: TEdit
          Left = 20
          Top = 75
          Width = 277
          Height = 21
          TabOrder = 2
        end
      end
    end
  end
  object Button_Help: TButton
    Left = 8
    Top = 389
    Width = 75
    Height = 27
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button_HelpClick
  end
  object TV: TVirtualStringTree
    Left = 5
    Top = 5
    Width = 156
    Height = 370
    DefaultNodeHeight = 17
    Header.AutoSizeIndex = 0
    Header.Height = 13
    Header.MainColumn = -1
    TabOrder = 0
    TreeOptions.SelectionOptions = [toAlwaysSelectNode, toSelectNextNodeOnRemoval]
    OnChange = TVChange
    OnGetText = TVGetText
    OnPaintText = TVPaintText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
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
