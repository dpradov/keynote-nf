object Form_ExportNew: TForm_ExportNew
  Left = 330
  Top = 208
  HelpContext = 57
  BorderStyle = bsDialog
  Caption = 'Export note Folders'
  ClientHeight = 463
  ClientWidth = 676
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
    Left = 267
    Top = 431
    Width = 90
    Height = 25
    Hint = 'Begin exporting note folders'
    Default = True
    ImageMargins.Left = 4
    Images = Form_Main.IMG_Toolbar
    TabOrder = 3
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 370
    Top = 431
    Width = 90
    Height = 25
    Hint = 'Cancel and close this dialog box'
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object Button_Help: TButton
    Left = 569
    Top = 431
    Width = 90
    Height = 25
    Caption = 'Help'
    TabOrder = 5
    OnClick = Button_HelpClick
  end
  object Pages: TPage95Control
    Left = 5
    Top = 5
    Width = 663
    Height = 417
    ActivePage = Tab_Options
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    TabOrder = 0
    object Tab_Main: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '57-1'
      Caption = 'Source and target'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitWidth = 646
      object GroupBox_Source: TGroupBox
        Left = 5
        Top = 8
        Width = 322
        Height = 376
        HelpType = htKeyword
        HelpKeyword = '57-1'
        Caption = ' Source: What to export? '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        DesignSize = (
          322
          376)
        object lbl3: TLabel
          Left = 12
          Top = 307
          Width = 117
          Height = 13
          AutoSize = False
          Caption = 'Folded text:'
        end
        object RB_CurrentNote: TRadioButton
          Left = 9
          Top = 26
          Width = 304
          Height = 17
          Hint = 'Click to export active folder only'
          Caption = '&Current folder'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_AllNotes: TRadioButton
          Left = 9
          Top = 73
          Width = 173
          Height = 17
          Hint = 'Click to export all folders in the knt file'
          Caption = '&All folders'
          TabOrder = 2
        end
        object RB_SelectedNotes: TRadioButton
          Left = 9
          Top = 95
          Width = 181
          Height = 17
          Hint = 'Click to export only selected folders'
          Caption = '&Selected folders'
          TabOrder = 3
        end
        object Button_Select: TButton
          Left = 188
          Top = 91
          Width = 125
          Height = 25
          Hint = 'Choose which folders to export'
          Caption = 'Select &Folders...'
          TabOrder = 4
          OnClick = Button_SelectClick
        end
        object Combo_TreeSelection: TComboBox
          Left = 29
          Top = 46
          Width = 286
          Height = 21
          Hint = 'Select what part of folder to export'
          Style = csDropDownList
          TabOrder = 1
        end
        object CheckBox_ExcludeHiddenNodes: TCheckBox
          Left = 9
          Top = 120
          Width = 288
          Height = 17
          Hint = 'Don'#39't export nodes hidden'
          Caption = 'Exclude &hidden nodes'
          TabOrder = 5
        end
        object CB_ShowHiddenMarkers: TCheckBox
          Left = 9
          Top = 353
          Width = 260
          Height = 17
          Hint = 
            'Makes hidden marks (KNT Links, bmk and img) visible'#13#10'Also shows ' +
            'Folder and Note IDs'
          HelpType = htKeyword
          HelpKeyword = '57-11'
          Caption = 'Show hidden marks and IDs'
          TabOrder = 10
        end
        object Spin_TblMaxDepth: TSpinEdit
          Left = 273
          Top = 330
          Width = 40
          Height = 22
          HelpType = htKeyword
          HelpKeyword = '57-14'
          Enabled = False
          MaxLength = 2
          MaxValue = 99
          MinValue = 1
          TabOrder = 9
          Value = 3
        end
        object CB_TableCont: TCheckBox
          Left = 9
          Top = 332
          Width = 260
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-14'
          Caption = 'Top N levels on a Table of Contents'
          TabOrder = 8
        end
        object cbFoldedText: TComboBox
          Left = 134
          Top = 304
          Width = 179
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '57-18'
          Style = csDropDownList
          Anchors = [akTop, akRight]
          TabOrder = 7
          OnChange = cbTagFindModeChange
          Items.Strings = (
            'Keep unchanged'
            'Unfold'
            'Remove "tagged"'
            'Remove All')
          ExplicitLeft = 130
        end
        object PnlTags: TPanel
          Left = 9
          Top = 148
          Width = 306
          Height = 145
          HelpType = htKeyword
          HelpKeyword = '57-17'
          BevelOuter = bvLowered
          TabOrder = 6
          object lbl4: TLabel
            Left = 9
            Top = 7
            Width = 135
            Height = 13
            AutoSize = False
            Caption = 'TAGs'
          end
          object lbl9: TLabel
            Left = 20
            Top = 55
            Width = 50
            Height = 13
            Hint = 'Exclude text/notes with ANY of the selected tags'
            AutoSize = False
            Caption = 'Without:'
          end
          object lbl8: TLabel
            Left = 20
            Top = 31
            Width = 50
            Height = 13
            Hint = 'Include text/notes with ALL or ANY of the selected tags'
            AutoSize = False
            Caption = 'With:'
          end
          object txtTagsIncl: TEdit
            Left = 162
            Top = 28
            Width = 137
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 1
            OnEnter = txtTagsInclEnter
          end
          object chkRemoveTags: TCheckBox
            Left = 162
            Top = 6
            Width = 137
            Height = 17
            Hint = 'Remove tags from the final text (after processing)'
            Caption = 'Remove'
            TabOrder = 0
          end
          object chkInhTagsFind: TCheckBox
            Left = 25
            Top = 121
            Width = 272
            Height = 17
            Caption = 'Inherited tags (metadata)'
            TabOrder = 5
          end
          object chkTagsText: TCheckBox
            Left = 25
            Top = 98
            Width = 272
            Height = 17
            Hint = 
              'Export will only include text fragments that meet the tag criter' +
              'ia'
            Caption = 'Consider extracts from notes'
            TabOrder = 4
            OnClick = chkTagsTextClick
          end
          object chkTagsMetad: TCheckBox
            Left = 25
            Top = 77
            Width = 272
            Height = 17
            Hint = 'Notes'#39' metadata will be used to verify the tag criteria'
            Caption = 'Consider metadata of notes'
            Checked = True
            State = cbChecked
            TabOrder = 3
            OnClick = chkTagsMetadClick
          end
          object txtTagsExcl: TEdit
            Left = 162
            Top = 53
            Width = 137
            Height = 19
            Ctl3D = False
            ParentCtl3D = False
            TabOrder = 2
            OnEnter = txtTagsExclEnter
          end
          object cbTagFindMode: TComboBox
            Left = 70
            Top = 28
            Width = 87
            Height = 21
            Style = csDropDownList
            TabOrder = 6
            OnChange = cbTagFindModeChange
            Items.Strings = (
              'ALL'
              'ANY')
          end
        end
      end
      object GroupBox_Target: TGroupBox
        Left = 333
        Top = 8
        Width = 319
        Height = 376
        HelpType = htKeyword
        HelpKeyword = '57-2'
        Caption = ' Target: Where and how to export?  '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 1
        object Label1: TLabel
          Left = 10
          Top = 29
          Width = 295
          Height = 13
          AutoSize = False
          Caption = '&Format for exported files:'
          FocusControl = Combo_Format
        end
        object Label2: TLabel
          Left = 10
          Top = 75
          Width = 282
          Height = 13
          AutoSize = False
          Caption = '&Directory for exported files:'
          FocusControl = Edit_Folder
        end
        object TB_OpenDlgDir: TToolbarButton97
          Left = 285
          Top = 91
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
          Left = 10
          Top = 46
          Width = 295
          Height = 21
          Hint = 'Select format for exported files'
          Style = csDropDownList
          TabOrder = 0
        end
        object CheckBox_PromptOverwrite: TCheckBox
          Left = 10
          Top = 120
          Width = 302
          Height = 17
          Hint = 'Warn if file by the same name already exists'
          Caption = '&Prompt before overwriting files'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object Edit_Folder: TEdit
          Left = 18
          Top = 91
          Width = 262
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
          Left = 10
          Top = 142
          Width = 302
          Height = 17
          Hint = 'Manually supply name for each exported file'
          Caption = 'Pro&mpt for individual filenames'
          TabOrder = 3
        end
        object CB_Section: TCheckBox
          Left = 10
          Top = 189
          Width = 258
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Caption = 'Section for each of the top N levels:'
          TabOrder = 4
        end
        object Spin_SectDepth: TSpinEdit
          Left = 270
          Top = 187
          Width = 40
          Height = 22
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Enabled = False
          MaxLength = 2
          MaxValue = 99
          MinValue = 1
          TabOrder = 5
          Value = 4
        end
        object CB_SaveImgDefWP: TCheckBox
          Left = 10
          Top = 353
          Width = 302
          Height = 17
          Hint = 
            'If *not* checked, use optimized formats; WordPad will prompt you' +
            ' to unblock to display images. Word will not'#13#10'(More in Help, F1)'
          HelpType = htKeyword
          HelpKeyword = '44-4'
          Caption = 'Save images in default WordPad format'
          TabOrder = 10
        end
        object CB_SectionToFile: TCheckBox
          Left = 25
          Top = 212
          Width = 245
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Caption = 'Save each Section to a new File'
          TabOrder = 6
          OnClick = CB_SectionToFileClick
        end
        object CB_SectionNewPg: TCheckBox
          Left = 25
          Top = 235
          Width = 245
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Caption = 'Start each Section on a new Page'
          TabOrder = 7
          OnClick = CB_SectionNewPgClick
        end
        object CB_NoteNewPg: TCheckBox
          Left = 10
          Top = 279
          Width = 302
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-16'
          Caption = 'Start each Note on a new Page'
          TabOrder = 9
          OnClick = CB_SectionNewPgClick
        end
        object CB_FolderNewFile: TCheckBox
          Left = 10
          Top = 306
          Width = 302
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Caption = 'Save each Folder to a new File'
          TabOrder = 8
          OnClick = CB_SectionNewPgClick
        end
      end
    end
    object Tab_Options: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '57-4'
      Caption = 'Options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitWidth = 646
      object gbHeadings: TGroupBox
        Left = 14
        Top = 11
        Width = 272
        Height = 246
        Caption = ' Optional headings '
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 0
        object lblSymbols: TLabel
          Left = 25
          Top = 210
          Width = 148
          Height = 13
          Caption = 'Symbols'
          FocusControl = Edit_Folder
        end
        object lblLength: TLabel
          Left = 25
          Top = 186
          Width = 148
          Height = 13
          Caption = 'Length heading'
          FocusControl = Edit_Folder
        end
        object CB_IncNoteHeading: TCheckBox
          Left = 25
          Top = 28
          Width = 182
          Height = 17
          Hint = 'Include folder headings'
          HelpType = htKeyword
          HelpKeyword = '57-6'
          Caption = '&Folder headings'
          TabOrder = 0
        end
        object CB_IncNodeHeading: TCheckBox
          Left = 25
          Top = 78
          Width = 182
          Height = 17
          Hint = 'Include node headings'
          HelpType = htKeyword
          HelpKeyword = '57-6'
          Caption = 'No&de headings'
          TabOrder = 2
        end
        object Edit_NodeHead: TComboBox
          Left = 48
          Top = 99
          Width = 179
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '57-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object Edit_NoteHead: TComboBox
          Left = 48
          Top = 49
          Width = 181
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '57-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object CB_LevelTemplates: TCheckBox
          Left = 25
          Top = 129
          Width = 183
          Height = 17
          Hint = 'Try to use files "nodehead_1.rtf", "nodehead_2.rtf", ...'
          HelpType = htKeyword
          HelpKeyword = '57-10'
          Caption = 'Level templates'
          TabOrder = 4
        end
        object CB_FontSizes: TCheckBox
          Left = 25
          Top = 157
          Width = 148
          Height = 17
          Hint = 
            'Set font size of headings based on level (including folder): Max' +
            ', Dec, Min'
          HelpType = htKeyword
          HelpKeyword = '57-10'
          Caption = 'Font sizes'
          TabOrder = 5
        end
        object Edit_FontSizes: TEdit
          Left = 172
          Top = 154
          Width = 55
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '57-10'
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
          Left = 172
          Top = 181
          Width = 55
          Height = 21
          Hint = 
            'Length of heading to be reached with %> token, based on node lev' +
            'el: Max, Dec, Min'
          HelpType = htKeyword
          HelpKeyword = '57-9'
          Alignment = taCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
        end
        object Edit_Symbols: TEdit
          Left = 172
          Top = 208
          Width = 55
          Height = 21
          Hint = 'Symbols to use with %< or %>:  Level1, Level2, ...'
          HelpType = htKeyword
          HelpKeyword = '57-9'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          OnExit = Edit_SymbolsExit
        end
      end
      object Btn_TknHlp: TBitBtn
        Left = 252
        Top = 59
        Width = 24
        Height = 22
        ImageIndex = 60
        Images = Form_Main.IMG_Toolbar
        TabOrder = 1
        OnClick = Btn_TknHlpClick
      end
      object gbOther: TGroupBox
        Left = 296
        Top = 11
        Width = 345
        Height = 246
        Caption = 'Other formatting options'
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 2
        object lblIndent: TLabel
          Left = 176
          Top = 54
          Width = 99
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Indents by'
          FocusControl = Spin_Indent
        end
        object Edit_Sample: TEdit
          Left = 71
          Top = 109
          Width = 250
          Height = 21
          HelpType = htKeyword
          HelpKeyword = '57-16'
          TabStop = False
          Enabled = False
          MaxLength = 127
          ReadOnly = True
          TabOrder = 5
        end
        object CB_IndentNodes: TCheckBox
          Left = 12
          Top = 31
          Width = 293
          Height = 17
          Hint = 'Indent nested nodes (heading and content)'
          HelpType = htKeyword
          HelpKeyword = '57-7'
          Caption = 'Indent nodes'
          TabOrder = 0
        end
        object Spin_Indent: TSpinEdit
          Left = 281
          Top = 51
          Width = 40
          Height = 22
          HelpType = htKeyword
          HelpKeyword = '57-7'
          MaxLength = 3
          MaxValue = 50
          MinValue = 2
          TabOrder = 2
          Value = 16
        end
        object CB_Font: TCheckBox
          Left = 12
          Top = 79
          Width = 301
          Height = 24
          Hint = 'Set an unified font for all content'
          HelpType = htKeyword
          HelpKeyword = '57-16'
          Caption = 'Body Font'
          TabOrder = 3
          OnClick = CB_FontClick
        end
        object BTN_Font: TBitBtn
          Left = 36
          Top = 107
          Width = 30
          Height = 25
          Enabled = False
          ImageIndex = 11
          Images = Form_Main.IMG_Format
          TabOrder = 4
          TabStop = False
          OnClick = BTN_FontClick
        end
        object CB_UseTab: TCheckBox
          Left = 31
          Top = 53
          Width = 106
          Height = 17
          Hint = 
            'Indent using TAB characters'#13#10'(False: use n'#186' spaces defined in No' +
            'te'#39's Tab size)'
          HelpType = htKeyword
          HelpKeyword = '57-7'
          Caption = 'Tab'
          TabOrder = 1
        end
        object CB_Header: TCheckBox
          Left = 12
          Top = 148
          Width = 323
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Caption = 'Top N levels as Page Header (in sections)'
          TabOrder = 6
        end
        object Spin_PgHdDepth: TSpinEdit
          Left = 36
          Top = 171
          Width = 40
          Height = 22
          HelpType = htKeyword
          HelpKeyword = '57-15'
          Enabled = False
          MaxLength = 1
          MaxValue = 5
          MinValue = 1
          TabOrder = 7
          Value = 1
        end
        object CB_ShowPageNumber: TCheckBox
          Left = 12
          Top = 208
          Width = 323
          Height = 17
          HelpType = htKeyword
          HelpKeyword = '57-16'
          Caption = 'Insert Page Number as Footer'
          TabOrder = 8
        end
      end
      object gbHTML: TGroupBox
        Left = 296
        Top = 269
        Width = 345
        Height = 103
        Caption = 'HTML Options'
        DefaultHeaderFont = False
        HeaderFont.Charset = DEFAULT_CHARSET
        HeaderFont.Color = clWindowText
        HeaderFont.Height = -11
        HeaderFont.Name = 'Tahoma'
        HeaderFont.Style = [fsBold]
        TabOrder = 3
        object RG_HTML: TRadioGroup
          Left = 10
          Top = 31
          Width = 287
          Height = 58
          Margins.Top = 6
          Margins.Bottom = 6
          DefaultHeaderFont = False
          HeaderFont.Charset = DEFAULT_CHARSET
          HeaderFont.Color = clWindowText
          HeaderFont.Height = -5
          HeaderFont.Name = 'Tahoma'
          HeaderFont.Style = []
          ShowFrame = False
          TabOrder = 0
          OnClick = RG_HTMLClick
        end
      end
    end
    object Tab_TreePad: TTab95Sheet
      HelpType = htKeyword
      HelpKeyword = '57-5'
      Caption = 'TreePad options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitWidth = 646
      object RG_TreePadVersion: TRadioGroup
        Left = 37
        Top = 23
        Width = 548
        Height = 66
        Caption = ' Target &TreePad version '
        ItemIndex = 0
        Items.Strings = (
          'Export to TreePad freeware (text only)'
          'Export to Treepad shareware (formatted text)')
        TabOrder = 0
      end
      object RG_TreePadMode: TRadioGroup
        Left = 37
        Top = 114
        Width = 548
        Height = 66
        Caption = ' T&arget file mode '
        ItemIndex = 0
        Items.Strings = (
          'Create a TreePad file for each exported folder'
          'Create a Treepad file containing all exported folders')
        TabOrder = 1
      end
      object RG_TreePadMaster: TRadioGroup
        Left = 37
        Top = 205
        Width = 548
        Height = 68
        Caption = ' &Compatibility top-level node '
        ItemIndex = 0
        Items.Strings = (
          'Create only when necessary'
          'Always create')
        TabOrder = 2
      end
    end
  end
  object btnPreview: TButton
    Left = 133
    Top = 431
    Width = 92
    Height = 25
    Caption = 'Pre&view'
    Default = True
    Images = Form_Main.IMG_Toolbar
    TabOrder = 2
    Visible = False
    OnClick = btnPreviewClick
  end
  object btnPageSetup: TButton
    Left = 9
    Top = 431
    Width = 110
    Height = 25
    Caption = 'Page &Setup...'
    TabOrder = 1
    Visible = False
    OnClick = btnPageSetupClick
  end
  object SaveDlg: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist]
    Title = 'Select target filename'
    Left = 610
    Top = 332
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 579
    Top = 332
  end
end
