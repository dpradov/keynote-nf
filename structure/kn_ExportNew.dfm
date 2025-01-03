object Form_ExportNew: TForm_ExportNew
  Left = 330
  Top = 208
  HelpContext = 313
  BorderStyle = bsDialog
  Caption = 'Export note Folders'
  ClientHeight = 419
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHelp = FormHelp
  TextHeight = 13
  object Button_OK: TButton
    Left = 6
    Top = 387
    Width = 75
    Height = 25
    Hint = 'Begin exporting note folders'
    Default = True
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 173
    Top = 387
    Width = 69
    Height = 25
    Hint = 'Cancel and close this dialog box'
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
  end
  object Button_Help: TButton
    Left = 247
    Top = 387
    Width = 69
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = Button_HelpClick
  end
  object Pages: TPage95Control
    Left = 5
    Top = 5
    Width = 311
    Height = 374
    ActivePage = Tab_Main
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    TabOrder = 3
    object Tab_Main: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '313-1'
      Caption = 'Source and target'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GroupBox_Source: TGroupBox
        Left = 2
        Top = 5
        Width = 292
        Height = 146
        Caption = ' Source: What to export? '
        TabOrder = 0
        object RB_CurrentNote: TRadioButton
          Left = 15
          Top = 20
          Width = 148
          Height = 17
          Hint = 'Click to export active folder only'
          Caption = '&Current folder'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_AllNotes: TRadioButton
          Left = 15
          Top = 65
          Width = 119
          Height = 17
          Hint = 'Click to export all folders in the knt file'
          Caption = '&All folders'
          TabOrder = 2
        end
        object RB_SelectedNotes: TRadioButton
          Left = 15
          Top = 87
          Width = 116
          Height = 17
          Hint = 'Click to export only selected folders'
          Caption = '&Selected folders'
          TabOrder = 3
        end
        object Button_Select: TButton
          Left = 159
          Top = 83
          Width = 111
          Height = 25
          Hint = 'Choose which folders to export'
          Caption = 'Select &Folders...'
          TabOrder = 4
          OnClick = Button_SelectClick
        end
        object Combo_TreeSelection: TComboBox
          Left = 35
          Top = 40
          Width = 235
          Height = 21
          Hint = 'Select what part of folder to export'
          Style = csDropDownList
          TabOrder = 1
        end
        object CheckBox_ExcludeHiddenNodes: TCheckBox
          Left = 15
          Top = 118
          Width = 218
          Height = 17
          Hint = 'Don'#39't export nodes hidden'
          Caption = 'Exclude &hidden nodes'
          TabOrder = 5
        end
      end
      object GroupBox_Target: TGroupBox
        Left = 2
        Top = 161
        Width = 292
        Height = 161
        Caption = ' Target: Where and how to export?  '
        TabOrder = 1
        object Label1: TLabel
          Left = 15
          Top = 20
          Width = 124
          Height = 13
          Caption = '&Format for exported files:'
          FocusControl = Combo_Format
        end
        object Label2: TLabel
          Left = 15
          Top = 65
          Width = 134
          Height = 13
          Caption = '&Directory for exported files:'
          FocusControl = Edit_Folder
        end
        object TB_OpenDlgDir: TToolbarButton97
          Left = 245
          Top = 80
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgDirClick
        end
        object Combo_Format: TComboBox
          Left = 15
          Top = 35
          Width = 255
          Height = 21
          Hint = 'Select format for exported files'
          Style = csDropDownList
          TabOrder = 0
          OnClick = Combo_FormatClick
        end
        object CheckBox_PromptOverwrite: TCheckBox
          Left = 15
          Top = 110
          Width = 236
          Height = 17
          Hint = 'Warn if file by the same name already exists'
          Caption = '&Prompt before overwriting files'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object Edit_Folder: TEdit
          Left = 15
          Top = 80
          Width = 231
          Height = 21
          Hint = 'Folder where exported files will be placed'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object CheckBox_Ask: TCheckBox
          Left = 15
          Top = 130
          Width = 236
          Height = 17
          Hint = 'Manually supply name for each exported file'
          Caption = 'Pro&mpt for individual filenames'
          TabOrder = 3
        end
      end
    end
    object Tab_Options: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '313-4'
      Caption = 'Options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object lblIndent: TLabel
        Left = 172
        Top = 137
        Width = 60
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Indents by'
        FocusControl = Spin_Indent
      end
      object lblTblCont: TLabel
        Left = 134
        Top = 187
        Width = 104
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Max level depth'
        FocusControl = Spin_MaxLevTbl
      end
      object CB_UseTab: TCheckBox
        Left = 134
        Top = 135
        Width = 43
        Height = 17
        Hint = 
          'Indent using TAB characters'#13#10'(False: use n'#186' spaces defined in No' +
          'te'#39's Tab size)'
        HelpType = htKeyword
        HelpKeyword = '313-7'
        Caption = 'Tab'
        TabOrder = 2
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 4
        Width = 293
        Height = 122
        Caption = ' Optional headings '
        TabOrder = 0
        object lblSymbols: TLabel
          Left = 19
          Top = 100
          Width = 39
          Height = 13
          Alignment = taRightJustify
          Caption = 'Symbols'
          FocusControl = Edit_Folder
        end
        object lblLength: TLabel
          Left = 143
          Top = 99
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = 'Length heading'
          FocusControl = Edit_Folder
        end
        object CB_IncNoteHeading: TCheckBox
          Left = 12
          Top = 20
          Width = 106
          Height = 17
          Hint = 'Include folder headings'
          HelpType = htKeyword
          HelpKeyword = '313-6'
          Caption = '&Folder headings'
          TabOrder = 0
        end
        object CB_IncNodeHeading: TCheckBox
          Left = 12
          Top = 46
          Width = 106
          Height = 17
          Hint = 'Include node headings'
          HelpType = htKeyword
          HelpKeyword = '313-6'
          Caption = 'No&de headings'
          TabOrder = 2
          OnClick = CB_IncNodeHeadingClick
        end
        object Edit_NodeHead: TComboBox
          Left = 121
          Top = 44
          Width = 158
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '313-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object Edit_NoteHead: TComboBox
          Left = 121
          Top = 19
          Width = 158
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '313-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object CB_LevelTemplates: TCheckBox
          Left = 12
          Top = 72
          Width = 103
          Height = 17
          Hint = 'Try to use files "nodehead_1.rtf", "nodehead_2.rtf", ...'
          HelpType = htKeyword
          HelpKeyword = '313-10'
          Caption = 'Level templates'
          TabOrder = 4
        end
        object CB_FontSizes: TCheckBox
          Left = 131
          Top = 73
          Width = 80
          Height = 17
          Hint = 
            'Set font size of headings based on level (including folder): Max' +
            ', Dec, Min'
          HelpType = htKeyword
          HelpKeyword = '313-10'
          Caption = 'Font sizes'
          TabOrder = 5
          OnClick = CB_FontSizesClick
        end
        object Edit_FontSizes: TEdit
          Left = 224
          Top = 70
          Width = 55
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '313-10'
          Alignment = taCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
        object Edit_LengthHeading: TEdit
          Left = 224
          Top = 96
          Width = 55
          Height = 21
          Hint = 
            'Length of heading to be reached with %> token, based on node lev' +
            'el: Max, Dec, Min'
          HelpType = htKeyword
          HelpKeyword = '313-9'
          Alignment = taCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
        end
        object Edit_Symbols: TEdit
          Left = 64
          Top = 94
          Width = 55
          Height = 21
          Hint = 'Symbols to use with %< or %>:  Level1, Level2, ...'
          HelpType = htKeyword
          HelpKeyword = '313-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          OnExit = Edit_SymbolsExit
        end
      end
      object RG_NodeMode: TRadioGroup
        Left = 3
        Top = 212
        Width = 292
        Height = 66
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -5
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = []
        ItemIndex = 0
        Items.Strings = (
          'Put all notes (nodes) in a single target file'
          'Export each note (node) to a separate target file')
        TabOrder = 11
      end
      object RG_NodePrint: TRadioGroup
        Left = 3
        Top = 212
        Width = 292
        Height = 66
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -5
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = []
        ItemIndex = 0
        Items.Strings = (
          'Put all notes (nodes) contiguous'
          'Start each note (node) on a new page')
        TabOrder = 10
        Visible = False
      end
      object RG_HTML: TRadioGroup
        Left = 3
        Top = 285
        Width = 292
        Height = 57
        Margins.Top = 6
        Margins.Bottom = 6
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -5
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = []
        TabOrder = 12
        OnClick = RG_HTMLClick
      end
      object Btn_TknHlp: TBitBtn
        Left = 276
        Top = 1
        Width = 24
        Height = 22
        ImageIndex = 60
        Images = Form_Main.IMG_Toolbar
        TabOrder = 13
        OnClick = Btn_TknHlpClick
      end
      object GB_Additional: TGroupBox
        Left = 3
        Top = 285
        Width = 292
        Height = 57
        TabOrder = 9
        Visible = False
        object CB_ShowHiddenMarkers: TCheckBox
          Left = 12
          Top = 14
          Width = 253
          Height = 17
          Hint = 
            'Makes hidden marks (KNT Links, bmk and img) visible'#13#10'Also shows ' +
            'Folder and Note IDs'
          HelpType = htKeyword
          HelpKeyword = '313-11'
          Caption = 'Show hidden marks and IDs'
          TabOrder = 2
          Visible = False
        end
        object CB_SaveImgDefWP: TCheckBox
          Left = 12
          Top = 13
          Width = 253
          Height = 17
          Hint = 
            'If *not* checked, use optimized formats; WordPad will prompt you' +
            ' to unblock to display images. Word will not'#13#10'(More in Help, F1)'
          HelpType = htKeyword
          HelpKeyword = '576-4'
          Caption = 'Save images in default WordPad format'
          TabOrder = 1
          Visible = False
        end
        object CB_ShowPageNumber: TCheckBox
          Left = 12
          Top = 13
          Width = 253
          Height = 17
          Caption = 'Insert page numbers'
          TabOrder = 0
          Visible = False
        end
      end
      object Edit_Sample: TEdit
        Left = 135
        Top = 159
        Width = 147
        Height = 21
        HelpType = htKeyword
        HelpKeyword = '313-13'
        TabStop = False
        Enabled = False
        MaxLength = 127
        ReadOnly = True
        TabOrder = 6
      end
      object BTN_Font: TBitBtn
        Left = 98
        Top = 157
        Width = 30
        Height = 25
        Enabled = False
        ImageIndex = 11
        Images = Form_Main.IMG_Format
        TabOrder = 5
        TabStop = False
        OnClick = BTN_FontClick
      end
      object CB_Font: TCheckBox
        Left = 12
        Top = 160
        Width = 57
        Height = 17
        Hint = 'Set an unified font for all content'
        HelpType = htKeyword
        HelpKeyword = '313-13'
        Caption = 'Font'
        TabOrder = 4
        OnClick = CB_FontClick
      end
      object Spin_Indent: TSpinEdit
        Left = 242
        Top = 133
        Width = 40
        Height = 22
        HelpType = htKeyword
        HelpKeyword = '313-7'
        MaxLength = 3
        MaxValue = 50
        MinValue = 2
        TabOrder = 3
        Value = 16
      end
      object CB_IndentNodes: TCheckBox
        Left = 12
        Top = 135
        Width = 102
        Height = 17
        Hint = 'Indent nested nodes (heading and content)'
        HelpType = htKeyword
        HelpKeyword = '313-7'
        Caption = 'Indent nodes'
        TabOrder = 1
        OnClick = CB_IndentNodesClick
      end
      object CB_TableCont: TCheckBox
        Left = 12
        Top = 186
        Width = 125
        Height = 17
        HelpType = htKeyword
        HelpKeyword = '313-13'
        Caption = 'Table of Contents'
        TabOrder = 7
        OnClick = CB_TableContClick
      end
      object Spin_MaxLevTbl: TSpinEdit
        Left = 242
        Top = 184
        Width = 40
        Height = 22
        HelpType = htKeyword
        HelpKeyword = '313-13'
        Enabled = False
        MaxLength = 2
        MaxValue = 99
        MinValue = 1
        TabOrder = 8
        Value = 16
      end
    end
    object Tab_TreePad: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '313-5'
      Caption = 'TreePad options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RG_TreePadVersion: TRadioGroup
        Left = 5
        Top = 5
        Width = 289
        Height = 66
        Caption = ' Target &TreePad version '
        ItemIndex = 0
        Items.Strings = (
          'Export to TreePad freeware (text only)'
          'Export to Treepad shareware (formatted text)')
        TabOrder = 0
      end
      object RG_TreePadMode: TRadioGroup
        Left = 5
        Top = 75
        Width = 288
        Height = 66
        Caption = ' T&arget file mode '
        ItemIndex = 0
        Items.Strings = (
          'Create a TreePad file for each exported folder'
          'Create a Treepad file containing all exported folders')
        TabOrder = 1
      end
      object RG_TreePadMaster: TRadioGroup
        Left = 5
        Top = 145
        Width = 288
        Height = 66
        Caption = ' &Compatibility top-level node '
        ItemIndex = 0
        Items.Strings = (
          'Create only when necessary'
          'Always create')
        TabOrder = 2
      end
    end
  end
  object Button_Preview: TButton
    Left = 85
    Top = 387
    Width = 75
    Height = 25
    Caption = 'Pre&view'
    Default = True
    TabOrder = 1
    Visible = False
    OnClick = Button_PreviewClick
  end
  object SaveDlg: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist]
    Title = 'Select target filename'
    Left = 273
    Top = 316
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 242
    Top = 316
  end
end
