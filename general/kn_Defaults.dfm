object Form_Defaults: TForm_Defaults
  Left = 379
  Top = 248
  HelpContext = 30
  BorderStyle = bsDialog
  Caption = 'Defaults'
  ClientHeight = 500
  ClientWidth = 425
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
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object LB_Scope: TLabel
    Left = 3
    Top = 7
    Width = 410
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Change properties for current folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
    StyleElements = [seClient, seBorder]
  end
  object Label1: TLabel
    Left = 14
    Top = 32
    Width = 279
    Height = 13
    AutoSize = False
    Caption = '&Folder name:'
    FocusControl = Edit_FolderName
  end
  object Label4: TLabel
    Left = 337
    Top = 32
    Width = 76
    Height = 13
    AutoSize = False
    Caption = 'Folder &icon:'
    FocusControl = Combo_Icons
  end
  object BTN_Font: TBitBtn
    Left = 29
    Top = 337
    Width = 30
    Height = 25
    Hint = 'Change initial Font'
    ImageIndex = 11
    Images = Form_Main.IMG_Format
    TabOrder = 3
    OnClick = BTN_FontClick
  end
  object BTN_Color: TBitBtn
    Left = 62
    Top = 337
    Width = 30
    Height = 25
    Hint = 'Change Background Color'
    ImageIndex = 10
    Images = Form_Main.IMG_Format
    TabOrder = 4
    OnClick = BTN_ColorClick
  end
  object BTN_Defaults: TBitBtn
    Left = 99
    Top = 337
    Width = 27
    Height = 25
    Hint = 'Reset factory default fonts and colors'
    ImageIndex = 6
    Images = Form_Main.IMG_Toolbar
    TabOrder = 5
    OnClick = BTN_DefaultsClick
  end
  object Edit_Sample: TEdit
    Left = 140
    Top = 341
    Width = 254
    Height = 21
    TabStop = False
    MaxLength = 127
    ReadOnly = True
    TabOrder = 7
  end
  object Button_OK: TButton
    Left = 119
    Top = 471
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 207
    Top = 471
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object Pages: TPage95Control
    Left = 8
    Top = 90
    Width = 405
    Height = 320
    ActivePage = Tab_Tree
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    ParentFont = False
    TabOrder = 2
    object Tab_Tree: TTab95Sheet
      HelpContext = 32
      Caption = 'Tree settings'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GBox_Tree: TGroupBox
        Left = 4
        Top = 3
        Width = 389
        Height = 285
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label5: TLabel
          Left = 11
          Top = 21
          Width = 164
          Height = 13
          Caption = 'Default &Name for new tree nodes:'
          FocusControl = Edit_NodeName
          WordWrap = True
        end
        object Label2: TLabel
          Left = 19
          Top = 158
          Width = 95
          Height = 13
          Caption = 'I&mage icons in tree:'
          FocusControl = Combo_TreeImages
        end
        object Label8: TLabel
          Left = 13
          Top = 200
          Width = 302
          Height = 13
          AutoSize = False
          Caption = 'Default font and background color (Tree): '
          FocusControl = BitBtn_TreeChromeHelp
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Transparent = False
        end
        object CB_TreeCheck: TCheckBox
          Left = 16
          Top = 89
          Width = 230
          Height = 17
          Hint = 'Display or hide checkboxes in ALL nodes'
          Caption = '&Show checkboxes in all nodes'
          TabOrder = 2
        end
        object Edit_NodeName: TComboBox
          Left = 208
          Top = 19
          Width = 134
          Height = 21
          Hint = 'Enter default name for nodes added to tree'
          TabOrder = 0
        end
        object BitBtn_TknHlp: TBitBtn
          Left = 352
          Top = 17
          Width = 25
          Height = 25
          Hint = 'Help for auto-naming tree nodes'
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 6
          TabStop = False
        end
        object CB_Vertical: TCheckBox
          Left = 16
          Top = 66
          Width = 230
          Height = 17
          Hint = 'Check to show tree ABOVE the editor'
          Caption = '&Vertical layout (tree on top)'
          TabOrder = 1
        end
        object Combo_TreeImages: TComboBox
          Left = 150
          Top = 155
          Width = 227
          Height = 21
          Style = csDropDownList
          TabOrder = 5
        end
        object CB_HideChecked: TCheckBox
          Left = 16
          Top = 113
          Width = 230
          Height = 17
          Hint = 'Show or hide checked nodes'
          Caption = '&Hide checked nodes'
          TabOrder = 3
        end
        object BitBtn_TreeChromeHelp: TBitBtn
          Left = 353
          Top = 194
          Width = 25
          Height = 25
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 7
          TabStop = False
          OnClick = BitBtn_TreeChromeHelpClick
        end
        object CB_TreeChrome_AllNotes: TCheckBox
          Left = 21
          Top = 260
          Width = 320
          Height = 17
          Hint = 
            'Font and BG color will be changed in the panels of ALL folders i' +
            'n current file'
          TabStop = False
          Caption = 'A&pply to ALL folders'
          TabOrder = 8
        end
        object gbCols: TGroupBox
          Left = 252
          Top = 56
          Width = 125
          Height = 85
          Caption = ' Additional Columns '
          TabOrder = 4
          object CB_ShowDateCol: TCheckBox
            Left = 7
            Top = 25
            Width = 112
            Height = 17
            Hint = 'Note creation date'
            Caption = 'Date'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object CB_ShowFlagCol: TCheckBox
            Left = 7
            Top = 48
            Width = 112
            Height = 17
            Hint = 'Note creation date'
            Caption = 'Flagged'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
          end
        end
      end
    end
    object Tab_Main: TTab95Sheet
      HelpContext = 31
      Caption = 'Editor settings'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GBox_Note: TGroupBox
        Left = 4
        Top = 3
        Width = 389
        Height = 285
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label_TabSize: TLabel
          Left = 228
          Top = 42
          Width = 95
          Height = 14
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Tab &Size:'
          Color = clBtnFace
          FocusControl = Spin_TabSize
          ParentColor = False
        end
        object Label_EditorFonts: TLabel
          Left = 13
          Top = 200
          Width = 302
          Height = 13
          AutoSize = False
          Caption = 'Default font and background color (Editor): '
          FocusControl = BitBtn_FolderChromeHelp
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Transparent = False
        end
        object Label14: TLabel
          Left = 11
          Top = 123
          Width = 95
          Height = 18
          AutoSize = False
          Caption = '&Language:'
          FocusControl = Combo_DefEdLang
        end
        object LB_Zoom: TLabel
          Left = 284
          Top = 73
          Width = 92
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Default Zoom (%)'
          Color = clBtnFace
          FocusControl = CB_Zoom
          ParentColor = False
        end
        object Label3: TLabel
          Left = 308
          Top = 92
          Width = 17
          Height = 22
          Hint = 
            'Allows to use a default value other than 100%'#13#10'( Only as default' +
            's )'
          AutoSize = False
          Caption = '(*)'
        end
        object CB_WordWrap: TCheckBox
          Left = 11
          Top = 14
          Width = 265
          Height = 17
          Hint = 'Apply word-wrapping to long lines'
          Caption = '&Wrap long lines'
          TabOrder = 0
        end
        object CB_URLDetect: TCheckBox
          Left = 11
          Top = 63
          Width = 265
          Height = 17
          Hint = 'Highlight URLs in editor'
          Caption = 'Detect and highlight &URLs in editor'
          TabOrder = 3
        end
        object CB_UseTabChar: TCheckBox
          Left = 11
          Top = 39
          Width = 265
          Height = 17
          Hint = 'Insert TAB character (#9) when Tab key pressed'
          Caption = 'Use &Tab character'
          TabOrder = 1
          OnClick = CB_UseTabCharClick
        end
        object Spin_TabSize: TSpinEdit
          Left = 327
          Top = 37
          Width = 49
          Height = 22
          Hint = 'Number of spaces to insert when Tab key pressed'
          MaxLength = 2
          MaxValue = 32
          MinValue = 1
          TabOrder = 2
          Value = 4
        end
        object Combo_DefEdLang: TLanguagesCombo
          Left = 109
          Top = 120
          Width = 204
          Height = 22
          Language = 2048
          LanguageType = ltInstalled
          ViewType = lvtLocalized
          ParentShowHint = False
          ShowFlag = False
          ShowHint = True
          TabOrder = 6
        end
        object CB_Zoom: TComboBox
          Left = 327
          Top = 92
          Width = 49
          Height = 21
          Hint = 'Allows to use a default value other than 100%'
          TabOrder = 5
          OnExit = CB_ZoomExit
          OnKeyPress = CB_ZoomKeyPress
          Items.Strings = (
            '250'
            '200'
            '150'
            '125'
            '110'
            '100'
            '90'
            '80'
            '75')
        end
        object CB_PlainText: TCheckBox
          Left = 11
          Top = 151
          Width = 366
          Height = 17
          Hint = 
            'If checked, new notes (or new entries of existing notes) will de' +
            'fault to plain text only, instead of Rich Text.'
          Caption = 'Default &Plain text only (do not save formatting information)'
          TabOrder = 7
          StyleElements = [seClient, seBorder]
        end
        object BitBtn_FolderHelp: TBitBtn
          Left = 353
          Top = 5
          Width = 25
          Height = 25
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 8
          TabStop = False
          OnClick = BitBtn_FolderHelpClick
        end
        object BitBtn_FolderChromeHelp: TBitBtn
          Left = 353
          Top = 192
          Width = 25
          Height = 25
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 9
          TabStop = False
          OnClick = BitBtn_FolderChromeHelpClick
        end
        object CB_InheritBGColor: TCheckBox
          Left = 131
          Top = 256
          Width = 209
          Height = 17
          TabStop = False
          Caption = 'Inherit &BG color from active node'
          Enabled = False
          TabOrder = 10
        end
        object CB_DisableTagSel: TCheckBox
          Left = 11
          Top = 86
          Width = 265
          Height = 17
          Hint = 'Disables tag selector and auto-tag registration'
          Caption = 'Disable Tag selector'
          TabOrder = 4
        end
      end
    end
    object Tab_Advanced: TTab95Sheet
      Caption = 'Advanced'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object PagesAdv: TPage95Control
        Left = 4
        Top = 3
        Width = 389
        Height = 285
        ActivePage = Tab_QL
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        HotTrack = False
        TabInactiveColor = clBtnFace
        TabInactiveFont.Charset = DEFAULT_CHARSET
        TabInactiveFont.Color = clGray
        TabInactiveFont.Height = -11
        TabInactiveFont.Name = 'Segoe UI'
        TabInactiveFont.Style = []
        ParentFont = False
        TabOrder = 0
        TabPosition = tpBottomRight
        object Tab_QL: TTab95Sheet
          Caption = 'Query Layout  '
          GripAlign = gaLeft
          ImageIndex = -1
          StaticPageIndex = -1
          TabVisible = True
          object lblQL: TLabel
            Left = 6
            Top = 2
            Width = 375
            Height = 13
            AutoSize = False
            Caption = 
              'Display the panels in Query Layout under the specified condition' +
              's:'
          end
          object cUseTLq: TComboBox
            Tag = 1
            Left = 45
            Top = 27
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 0
          end
          object cUseTRq: TComboBox
            Tag = 2
            Left = 45
            Top = 50
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 1
          end
          object cb_TLq: TCheckBox
            Tag = 1
            Left = 3
            Top = 29
            Width = 40
            Height = 17
            Caption = 'TL'
            TabOrder = 2
          end
          object cb_TRq: TCheckBox
            Tag = 2
            Left = 3
            Top = 52
            Width = 40
            Height = 17
            Caption = 'TR'
            TabOrder = 3
          end
          object cb_BLq: TCheckBox
            Tag = 3
            Left = 3
            Top = 104
            Width = 40
            Height = 17
            Caption = 'BL'
            TabOrder = 4
          end
          object cUseBRq: TComboBox
            Tag = 4
            Left = 45
            Top = 125
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 5
          end
          object cb_BRq: TCheckBox
            Tag = 4
            Left = 3
            Top = 127
            Width = 40
            Height = 17
            Caption = 'BR'
            TabOrder = 6
          end
          object cUseBLq: TComboBox
            Tag = 3
            Left = 45
            Top = 102
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 7
          end
          object cUseCq: TComboBox
            Tag = 5
            Left = 45
            Top = 76
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 8
          end
          object cb_Cq: TCheckBox
            Tag = 5
            Left = 3
            Top = 78
            Width = 40
            Height = 17
            Caption = 'C'
            TabOrder = 9
          end
          object TagsTLq: TEdit
            Tag = 1
            Left = 290
            Top = 28
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 10
          end
          object TagsTRq: TEdit
            Tag = 2
            Left = 290
            Top = 51
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 11
          end
          object TagsCq: TEdit
            Tag = 5
            Left = 290
            Top = 77
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 12
          end
          object TagsBLq: TEdit
            Tag = 3
            Left = 290
            Top = 103
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 13
          end
          object TagsBRq: TEdit
            Tag = 4
            Left = 290
            Top = 126
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 14
          end
          object cb_NewInEL: TCheckBox
            Left = 3
            Top = 171
            Width = 352
            Height = 17
            Caption = 'Switch to Editing Layout when creating a new entry'
            TabOrder = 15
          end
          object BitBtn_QL: TBitBtn
            Left = 353
            Top = 151
            Width = 25
            Height = 25
            ImageIndex = 60
            Images = Form_Main.IMG_Toolbar
            TabOrder = 16
            TabStop = False
            OnClick = BitBtn_QLClick
          end
        end
        object Tab_EL: TTab95Sheet
          Caption = 'Editing Layout  '
          GripAlign = gaLeft
          ImageIndex = -1
          StaticPageIndex = -1
          TabVisible = True
          object lblEL: TLabel
            Left = 6
            Top = 2
            Width = 375
            Height = 13
            AutoSize = False
            Caption = 
              'Display the panels in Editing Layout under the specified conditi' +
              'ons:'
          end
          object cUseTLe: TComboBox
            Tag = 11
            Left = 45
            Top = 27
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 0
          end
          object cUseTRe: TComboBox
            Tag = 12
            Left = 45
            Top = 50
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 1
          end
          object cb_TLe: TCheckBox
            Tag = 11
            Left = 3
            Top = 29
            Width = 40
            Height = 17
            Caption = 'TL'
            TabOrder = 2
          end
          object cb_TRe: TCheckBox
            Tag = 12
            Left = 3
            Top = 52
            Width = 40
            Height = 17
            Caption = 'TR'
            TabOrder = 3
          end
          object cb_BLe: TCheckBox
            Tag = 13
            Left = 3
            Top = 104
            Width = 40
            Height = 17
            Caption = 'BL'
            TabOrder = 4
          end
          object cUseBRe: TComboBox
            Tag = 14
            Left = 45
            Top = 125
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 5
          end
          object cb_BRe: TCheckBox
            Tag = 14
            Left = 3
            Top = 127
            Width = 40
            Height = 17
            Caption = 'BR'
            TabOrder = 6
          end
          object cUseBLe: TComboBox
            Tag = 13
            Left = 45
            Top = 102
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 7
          end
          object cUseCe: TComboBox
            Tag = 15
            Left = 45
            Top = 76
            Width = 240
            Height = 21
            Style = csDropDownList
            Enabled = False
            TabOrder = 8
          end
          object cb_Ce: TCheckBox
            Tag = 15
            Left = 3
            Top = 78
            Width = 40
            Height = 17
            Caption = 'C'
            TabOrder = 9
          end
          object TagsTLe: TEdit
            Tag = 11
            Left = 290
            Top = 28
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 10
          end
          object TagsTRe: TEdit
            Tag = 12
            Left = 290
            Top = 51
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 11
          end
          object TagsCe: TEdit
            Tag = 15
            Left = 290
            Top = 77
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 12
          end
          object TagsBLe: TEdit
            Tag = 13
            Left = 290
            Top = 103
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 13
          end
          object TagsBRe: TEdit
            Tag = 14
            Left = 290
            Top = 126
            Width = 87
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 14
          end
          object BitBtn_EL: TBitBtn
            Left = 352
            Top = 151
            Width = 25
            Height = 25
            ImageIndex = 60
            Images = Form_Main.IMG_Toolbar
            TabOrder = 15
            TabStop = False
            OnClick = BitBtn_ELClick
          end
        end
        object Tab_MultiE: TTab95Sheet
          Caption = 'Multi-entries Panels  '
          GripAlign = gaLeft
          ImageIndex = -1
          StaticPageIndex = -1
          TabVisible = True
          object lbl9: TLabel
            Left = 160
            Top = 32
            Width = 213
            Height = 13
            Caption = 'Excerpt from entries. Max characters / Lines'
          end
          object lbl7: TLabel
            Left = 10
            Top = 32
            Width = 68
            Height = 13
            Caption = 'Entry Header:'
          end
          object lbl6: TLabel
            Left = 7
            Top = 143
            Width = 99
            Height = 13
            Caption = 'Tags order in entries'
          end
          object lbl10: TLabel
            Left = 10
            Top = 6
            Width = 145
            Height = 13
            Caption = 'Display initially for each entry:'
          end
          object ExcerptMaxL: TSpinEdit
            Left = 325
            Top = 52
            Width = 49
            Height = 22
            MaxLength = 1
            MaxValue = 7
            MinValue = 1
            TabOrder = 0
            Value = 3
          end
          object ExcerptMaxC: TSpinEdit
            Left = 262
            Top = 51
            Width = 49
            Height = 22
            Increment = 10
            MaxLength = 3
            MaxValue = 750
            MinValue = 100
            TabOrder = 1
            Value = 250
          end
          object cb_HLine: TCheckBox
            Left = 16
            Top = 51
            Width = 120
            Height = 17
            Hint = 'Displays a line at the beginning of the header'
            Caption = 'Line'
            TabOrder = 2
          end
          object cb_HDate: TCheckBox
            Left = 16
            Top = 97
            Width = 120
            Height = 17
            Hint = 'Display the date and time the entry was created'
            Caption = 'Date / time'
            TabOrder = 3
          end
          object cb_HTags: TCheckBox
            Left = 16
            Top = 74
            Width = 120
            Height = 17
            Hint = 'Displays entry tags'
            Caption = 'Tags'
            TabOrder = 4
          end
          object txtTagsOrder: TEdit
            Left = 156
            Top = 141
            Width = 218
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 5
          end
          object CB_DescOrd: TCheckBox
            Left = 7
            Top = 166
            Width = 183
            Height = 17
            Hint = 
              'Entries are displayed in descending order according to their cre' +
              'ation date (most recent at the top)'
            Caption = 'Descending order'
            TabOrder = 6
          end
          object cEntryCont: TComboBox
            Left = 189
            Top = 3
            Width = 185
            Height = 21
            Style = csDropDownList
            TabOrder = 7
          end
        end
        object Tab_AdvOther: TTab95Sheet
          Caption = 'Other  '
          GripAlign = gaLeft
          ImageIndex = -1
          StaticPageIndex = -1
          TabVisible = True
          object cb_AutoExp: TCheckBox
            Left = 8
            Top = 4
            Width = 230
            Height = 17
            Hint = 'Auto expand TL,TR, BL, BR panels when focusing'
            Caption = 'Auto expand panels'
            TabOrder = 0
          end
        end
      end
    end
  end
  object Button_Help: TButton
    Left = 338
    Top = 471
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 8
    OnClick = Button_HelpClick
  end
  object GB_Defaults: TGroupBox
    Left = 5
    Top = 412
    Width = 404
    Height = 53
    TabOrder = 6
    object CB_SaveDefaults: TCheckBox
      Left = 10
      Top = 7
      Width = 386
      Height = 17
      Caption = 'Save as Defaults'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      StyleElements = [seClient, seBorder]
      OnClick = CB_SaveDefaultsClick
    end
    object CB_SaveAsDef: TCheckBox
      Left = 31
      Top = 30
      Width = 365
      Height = 17
      Hint = 
        'Normally, properties are saved as defaults for all new folders y' +
        'ou create. '#13#10'You can define it as default only for the current f' +
        'ile.'
      Caption = 'Only for'
      TabOrder = 1
      StyleElements = [seClient, seBorder]
      OnClick = CB_SaveAsDefClick
    end
  end
  object Edit_FolderName: TComboBox
    Left = 12
    Top = 49
    Width = 281
    Height = 21
    Hint = 'Enter name for new folder'
    TabOrder = 9
    OnKeyPress = Edit_FolderNameKeyPress
  end
  object Combo_Icons: TGFXComboBox
    Left = 334
    Top = 49
    Width = 79
    Height = 22
    Hint = 'Click to select icon for folder'
    Extended = False
    DropDownCount = 10
    TabOrder = 10
  end
  object CB_RTL: TCheckBox
    Left = 247
    Top = 75
    Width = 167
    Height = 17
    Hint = 
      'Defines default bidrectional mode in folder (tree and new notes)' +
      ', for RTL languages'
    BiDiMode = bdRightToLeft
    Caption = 'RTL (Right to Left)'
    ParentBiDiMode = False
    TabOrder = 11
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 16
    Top = 470
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 65525
    Top = 473
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 40
    Top = 469
  end
end
