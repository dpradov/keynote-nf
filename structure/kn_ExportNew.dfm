object Form_ExportNew: TForm_ExportNew
  Left = 330
  Top = 208
  HelpContext = 590
  BorderStyle = bsDialog
  Caption = 'Export notes'
  ClientHeight = 397
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
  TextHeight = 13
  object Button_OK: TButton
    Left = 20
    Top = 364
    Width = 75
    Height = 25
    Hint = 'Begin exporting notes'
    Caption = 'E&xport'
    Default = True
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 100
    Top = 364
    Width = 75
    Height = 25
    Hint = 'Cancel and close this dialog box'
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 1
  end
  object Button_Help: TButton
    Left = 179
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 2
    OnClick = Button_HelpClick
  end
  object Pages: TPage95Control
    Left = 5
    Top = 5
    Width = 311
    Height = 348
    ActivePage = Tab_Options
    HotTrack = False
    TabInactiveColor = clBtnFace
    TabInactiveFont.Charset = DEFAULT_CHARSET
    TabInactiveFont.Color = clWindowText
    TabInactiveFont.Height = -11
    TabInactiveFont.Name = 'Tahoma'
    TabInactiveFont.Style = []
    TabOrder = 3
    object Tab_Main: TTab95Sheet
      Caption = 'Source and target'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GroupBox_Source: TGroupBox
        Left = 2
        Top = 5
        Width = 292
        Height = 141
        Caption = ' Source: What to export? '
        TabOrder = 0
        object RB_CurrentNote: TRadioButton
          Left = 15
          Top = 20
          Width = 148
          Height = 17
          Hint = 'Click to export active note only'
          Caption = '&Current note'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_AllNotes: TRadioButton
          Left = 15
          Top = 65
          Width = 119
          Height = 17
          Hint = 'Click to export all notes in the file'
          Caption = '&All notes'
          TabOrder = 2
        end
        object RB_SelectedNotes: TRadioButton
          Left = 15
          Top = 85
          Width = 116
          Height = 17
          Hint = 'Click to export only selected notes'
          Caption = '&Selected notes'
          TabOrder = 3
        end
        object Button_Select: TButton
          Left = 159
          Top = 75
          Width = 111
          Height = 25
          Hint = 'Choose which notes to export'
          Caption = 'Select &Notes...'
          TabOrder = 4
          OnClick = Button_SelectClick
        end
        object Combo_TreeSelection: TComboBox
          Left = 35
          Top = 40
          Width = 235
          Height = 21
          Hint = 'For tree notes, select what part of tree to export'
          Style = csDropDownList
          TabOrder = 1
        end
        object CheckBox_ExcludeHiddenNodes: TCheckBox
          Left = 15
          Top = 113
          Width = 218
          Height = 17
          Hint = 'Don'#39't export nodes hidden'
          Caption = 'Exclude &hidden nodes'
          TabOrder = 5
        end
      end
      object GroupBox_Target: TGroupBox
        Left = 2
        Top = 152
        Width = 292
        Height = 157
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
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
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
      Caption = 'Options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GroupBox1: TGroupBox
        Left = 3
        Top = 5
        Width = 293
        Height = 151
        Caption = ' Optional headings '
        TabOrder = 0
        object Label3: TLabel
          Left = -4
          Top = 126
          Width = 62
          Height = 13
          Alignment = taRightJustify
          Caption = 'Symbols'
          FocusControl = Edit_Folder
        end
        object Label4: TLabel
          Left = 143
          Top = 125
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = 'Length heading'
          FocusControl = Edit_Folder
        end
        object lblIndent: TLabel
          Left = 120
          Top = 99
          Width = 97
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Indents by'
          FocusControl = Spin_Indent
        end
        object CB_IncNoteHeading: TCheckBox
          Left = 12
          Top = 20
          Width = 106
          Height = 17
          Hint = 'Include note headings'
          Caption = '&Note headings'
          TabOrder = 0
        end
        object CB_IncNodeHeading: TCheckBox
          Left = 12
          Top = 44
          Width = 106
          Height = 17
          Hint = 'Include node headings'
          Caption = 'No&de headings'
          TabOrder = 2
        end
        object Edit_NodeHead: TComboBox
          Left = 121
          Top = 44
          Width = 158
          Height = 21
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
          Caption = 'Level templates'
          TabOrder = 4
        end
        object CB_FontSizes: TCheckBox
          Left = 143
          Top = 73
          Width = 80
          Height = 17
          Hint = 'Set font size of node headings based on level: Max, Dec, Min'
          Caption = 'Font sizes'
          TabOrder = 5
          OnClick = CB_FontSizesClick
        end
        object Edit_FontSizes: TEdit
          Left = 224
          Top = 70
          Width = 55
          Height = 21
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
          Top = 122
          Width = 55
          Height = 21
          Hint = 
            'Length of heading to be reached with %> token, based on level: M' +
            'ax, Dec, Min'
          Alignment = taCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
        end
        object Edit_Symbols: TEdit
          Left = 65
          Top = 122
          Width = 55
          Height = 21
          Hint = 'Symbols to use with %< or %>:  Note, Node L1, ...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
        end
        object CB_IndentNodes: TCheckBox
          Left = 12
          Top = 97
          Width = 120
          Height = 17
          Hint = 'Indent nested nodes (heading and content)'
          Caption = 'Indent nodes'
          TabOrder = 7
          OnClick = CB_IndentNodesClick
        end
        object Spin_Indent: TSpinEdit
          Left = 224
          Top = 95
          Width = 55
          Height = 22
          MaxLength = 3
          MaxValue = 50
          MinValue = 2
          TabOrder = 8
          Value = 16
        end
      end
      object RG_NodeMode: TRadioGroup
        Left = 3
        Top = 160
        Width = 292
        Height = 66
        Caption = ' &When exporting tree-type notes...'
        ItemIndex = 0
        Items.Strings = (
          'Put all tree nodes in a single target file'
          'Export each node to a separate target file')
        TabOrder = 1
      end
      object RG_HTML: TRadioGroup
        Left = 3
        Top = 233
        Width = 292
        Height = 81
        Margins.Top = 6
        Margins.Bottom = 6
        Caption = ' &Method for exporting HTML: '
        TabOrder = 2
        OnClick = RG_HTMLClick
      end
      object Btn_TknHlp: TBitBtn
        Left = 275
        Top = 1
        Width = 24
        Height = 22
        ImageIndex = 60
        Images = Form_Main.IMG_Toolbar
        TabOrder = 3
        OnClick = Btn_TknHlpClick
      end
    end
    object Tab_TreePad: TTab95Sheet
      Caption = 'TreePad options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
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
          'Create a TreePad file for each exported note'
          'Create a Treepad file containing all exported notes')
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
  object SaveDlg: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist]
    Title = 'Select target filename'
    Left = 265
    Top = 364
  end
end
