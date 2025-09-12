object Form_Defaults: TForm_Defaults
  Left = 379
  Top = 248
  HelpContext = 30
  BorderStyle = bsDialog
  Caption = 'Defaults'
  ClientHeight = 500
  ClientWidth = 383
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
    Left = 14
    Top = 7
    Width = 348
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
    Width = 246
    Height = 13
    AutoSize = False
    Caption = '&Folder name:'
    FocusControl = Edit_FolderName
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 297
    Top = 32
    Width = 76
    Height = 13
    AutoSize = False
    Caption = 'Folder &icon:'
    FocusControl = Combo_Icons
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Button_OK: TButton
    Left = 15
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
    Left = 103
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
    Width = 369
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
        Width = 354
        Height = 285
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label5: TLabel
          Left = 17
          Top = 19
          Width = 108
          Height = 26
          Caption = 'Default &Name for new tree nodes:'
          FocusControl = Edit_NodeName
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Label2: TLabel
          Left = 19
          Top = 158
          Width = 95
          Height = 13
          Caption = 'I&mage icons in tree:'
          FocusControl = Combo_TreeImages
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
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
          Width = 208
          Height = 17
          Hint = 'Display or hide checkboxes in ALL nodes'
          Caption = '&Show checkboxes in all nodes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object Edit_NodeName: TComboBox
          Left = 132
          Top = 17
          Width = 176
          Height = 21
          Hint = 'Enter default name for nodes added to tree'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CB_AutoNumberNodes: TCheckBox
          Left = 158
          Top = 18
          Width = 188
          Height = 17
          Hint = 'When adding a node, append sequential number to its name'
          Caption = '&Append sequential number'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          Visible = False
        end
        object BitBtn_TknHlp: TBitBtn
          Left = 321
          Top = 15
          Width = 25
          Height = 25
          Hint = 'Help for auto-naming tree nodes'
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 7
          TabStop = False
        end
        object CB_Vertical: TCheckBox
          Left = 16
          Top = 66
          Width = 208
          Height = 17
          Hint = 'Check to show tree ABOVE the editor'
          Caption = '&Vertical layout (tree on top)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object Combo_TreeImages: TComboBox
          Left = 150
          Top = 155
          Width = 194
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
        object CB_HideChecked: TCheckBox
          Left = 16
          Top = 113
          Width = 208
          Height = 17
          Hint = 'Show or hide checked nodes'
          Caption = '&Hide checked nodes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object BitBtn_TreeChromeHelp: TBitBtn
          Left = 321
          Top = 194
          Width = 25
          Height = 25
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 8
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
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
        end
        object gbCols: TGroupBox
          Left = 232
          Top = 56
          Width = 115
          Height = 85
          Caption = ' Additional Columns '
          TabOrder = 5
          object CB_ShowDateCol: TCheckBox
            Left = 7
            Top = 25
            Width = 100
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
            Width = 100
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
        Width = 354
        Height = 285
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label_TabSize: TLabel
          Left = 283
          Top = 12
          Width = 60
          Height = 14
          AutoSize = False
          Caption = 'Tab &Size:'
          FocusControl = Spin_TabSize
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
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
          Left = 15
          Top = 150
          Width = 73
          Height = 18
          AutoSize = False
          Caption = '&Language:'
          FocusControl = Combo_DefEdLang
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object LB_Zoom: TLabel
          Left = 248
          Top = 65
          Width = 92
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Default Zoom (%)'
          FocusControl = CB_Zoom
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 260
          Top = 84
          Width = 17
          Height = 22
          Hint = 
            'Allows to use a default value other than 100%'#13#10'( Only as default' +
            's )'
          AutoSize = False
          Caption = '(*)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object CB_WordWrap: TCheckBox
          Left = 13
          Top = 14
          Width = 229
          Height = 17
          Hint = 'Apply word-wrapping to long lines'
          Caption = '&Wrap long lines'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CB_URLDetect: TCheckBox
          Left = 13
          Top = 63
          Width = 229
          Height = 17
          Hint = 'Highlight URLs in editor'
          Caption = 'Detect and highlight &URLs in editor'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object CB_UseTabChar: TCheckBox
          Left = 13
          Top = 39
          Width = 229
          Height = 17
          Hint = 'Insert TAB character (#9) when Tab key pressed'
          Caption = 'Use &Tab character'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = CB_UseTabCharClick
        end
        object Spin_TabSize: TSpinEdit
          Left = 284
          Top = 32
          Width = 56
          Height = 22
          Hint = 'Number of spaces to insert when Tab key pressed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 2
          MaxValue = 32
          MinValue = 1
          ParentFont = False
          TabOrder = 4
          Value = 4
        end
        object Combo_DefEdLang: TLanguagesCombo
          Left = 92
          Top = 147
          Width = 216
          Height = 22
          Language = 2048
          LanguageType = ltInstalled
          ViewType = lvtLocalized
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowFlag = False
          ShowHint = True
          TabOrder = 7
        end
        object CB_Zoom: TComboBox
          Left = 284
          Top = 84
          Width = 56
          Height = 21
          Hint = 'Allows to use a default value other than 100%'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
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
          Left = 13
          Top = 120
          Width = 326
          Height = 17
          Hint = 
            'If checked, new notes (or new entries of existing notes) will de' +
            'fault to plain text only, instead of Rich Text.'
          Caption = 'Default &Plain text only (do not save formatting information)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          StyleElements = [seClient, seBorder]
        end
        object BitBtn_FolderHelp: TBitBtn
          Left = 317
          Top = 145
          Width = 25
          Height = 25
          ImageIndex = 60
          Images = Form_Main.IMG_Toolbar
          TabOrder = 8
          TabStop = False
          OnClick = BitBtn_FolderHelpClick
        end
        object BitBtn_FolderChromeHelp: TBitBtn
          Left = 317
          Top = 194
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
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
        end
        object CB_DisableTagSel: TCheckBox
          Left = 13
          Top = 86
          Width = 229
          Height = 17
          Hint = 'Disables tag selector and auto-tag registration'
          Caption = 'Disable Tag selector'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
      end
    end
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
    Top = 337
    Width = 218
    Height = 21
    TabStop = False
    MaxLength = 127
    ReadOnly = True
    TabOrder = 7
  end
  object Button_Help: TButton
    Left = 298
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
    Width = 370
    Height = 53
    TabOrder = 6
    object CB_SaveDefaults: TCheckBox
      Left = 10
      Top = 7
      Width = 341
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
      Left = 44
      Top = 28
      Width = 277
      Height = 17
      Hint = 
        'Normally, properties are saved as defaults for all new folders y' +
        'ou create. '#13#10'You can define it as default only for the current f' +
        'ile.'
      Caption = 'Only for'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      StyleElements = [seClient, seBorder]
      OnClick = CB_SaveAsDefClick
    end
  end
  object Edit_FolderName: TComboBox
    Left = 12
    Top = 49
    Width = 248
    Height = 22
    Hint = 'Enter name for new folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
    OnKeyPress = Edit_FolderNameKeyPress
  end
  object Combo_Icons: TGFXComboBox
    Left = 294
    Top = 49
    Width = 79
    Height = 22
    Hint = 'Click to select icon for folder'
    Extended = False
    DropDownCount = 10
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 10
  end
  object CB_RTL: TCheckBox
    Left = 207
    Top = 75
    Width = 167
    Height = 17
    Hint = 
      'Defines default bidrectional mode in folder (tree and new notes)' +
      ', for RTL languages'
    BiDiMode = bdRightToLeft
    Caption = 'RTL (Right to Left)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    TabOrder = 11
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 221
    Top = 470
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 194
    Top = 473
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 245
    Top = 469
  end
end
