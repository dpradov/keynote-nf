object Form_ExportNew: TForm_ExportNew
  Left = 330
  Top = 208
  HelpContext = 590
  BorderStyle = bsDialog
  Caption = 'Export notes'
  ClientHeight = 400
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button_OK: TTntButton
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
  object Button_Cancel: TTntButton
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
  object Button_Help: TTntButton
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
    Width = 301
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 312
      object GroupBox_Source: TTntGroupBox
        Left = 3
        Top = 5
        Width = 281
        Height = 141
        Caption = ' Source: What to export? '
        TabOrder = 0
        object RB_CurrentNote: TTntRadioButton
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
        object RB_AllNotes: TTntRadioButton
          Left = 15
          Top = 65
          Width = 119
          Height = 17
          Hint = 'Click to export all notes in the file'
          Caption = '&All notes'
          TabOrder = 2
        end
        object RB_SelectedNotes: TTntRadioButton
          Left = 15
          Top = 85
          Width = 116
          Height = 17
          Hint = 'Click to export only selected notes'
          Caption = '&Selected notes'
          TabOrder = 3
        end
        object Button_Select: TTntButton
          Left = 140
          Top = 75
          Width = 111
          Height = 25
          Hint = 'Choose which notes to export'
          Caption = 'Select &Notes...'
          TabOrder = 4
          OnClick = Button_SelectClick
        end
        object Combo_TreeSelection: TTntComboBox
          Left = 35
          Top = 40
          Width = 216
          Height = 21
          Hint = 'For tree notes, select what part of tree to export'
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
        end
        object CheckBox_ExcludeHiddenNodes: TTntCheckBox
          Left = 15
          Top = 113
          Width = 218
          Height = 17
          Hint = 'Don'#39't export nodes hidden'
          Caption = 'Exclude &hidden nodes'
          TabOrder = 5
        end
      end
      object GroupBox_Target: TTntGroupBox
        Left = 3
        Top = 152
        Width = 281
        Height = 157
        Caption = ' Target: Where and how to export?  '
        TabOrder = 1
        object Label1: TTntLabel
          Left = 15
          Top = 20
          Width = 124
          Height = 13
          Caption = '&Format for exported files:'
          FocusControl = Combo_Format
        end
        object Label2: TTntLabel
          Left = 15
          Top = 65
          Width = 134
          Height = 13
          Caption = '&Directory for exported files:'
          FocusControl = Edit_Folder
        end
        object TB_OpenDlgDir: TToolbarButton97
          Left = 241
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
        object Combo_Format: TTntComboBox
          Left = 15
          Top = 35
          Width = 251
          Height = 21
          Hint = 'Select format for exported files'
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 0
          OnClick = Combo_FormatClick
        end
        object CheckBox_PromptOverwrite: TTntCheckBox
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
        object Edit_Folder: TTntEdit
          Left = 15
          Top = 80
          Width = 227
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
        object CheckBox_Ask: TTntCheckBox
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
      object GroupBox1: TTntGroupBox
        Left = 5
        Top = 5
        Width = 281
        Height = 151
        Caption = ' Optional headings '
        TabOrder = 0
        object CB_IncNoteHeading: TTntCheckBox
          Left = 15
          Top = 20
          Width = 218
          Height = 17
          Caption = 'Include &note headings'
          TabOrder = 0
        end
        object CB_IncNodeHeading: TTntCheckBox
          Left = 15
          Top = 65
          Width = 218
          Height = 17
          Caption = 'Include no&de headings'
          TabOrder = 2
        end
        object Edit_NodeHead: TTntComboBox
          Left = 35
          Top = 85
          Width = 206
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 3
        end
        object Edit_NoteHead: TTntComboBox
          Left = 35
          Top = 40
          Width = 206
          Height = 21
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 1
        end
        object Btn_TknHlp: TBitBtn
          Left = 35
          Top = 115
          Width = 206
          Height = 25
          Hint = 'Help for auto-naming tree nodes'
          Caption = '&Help on headings'
          TabOrder = 4
          OnClick = Btn_TknHlpClick
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
      end
      object RG_NodeMode: TTntRadioGroup
        Left = 5
        Top = 160
        Width = 281
        Height = 66
        Caption = ' &When exporting tree-type notes...'
        ItemIndex = 0
        Items.Strings = (
          'Put all tree nodes in a single target file'
          'Export each node to a separate target file')
        TabOrder = 1
      end
      object RG_HTML: TTntRadioGroup
        Left = 5
        Top = 232
        Width = 288
        Height = 81
        Margins.Top = 6
        Margins.Bottom = 6
        Caption = ' &Method for exporting HTML: '
        TabOrder = 2
        OnClick = RG_HTMLClick
      end
    end
    object Tab_TreePad: TTab95Sheet
      Caption = 'TreePad options'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object RG_TreePadVersion: TTntRadioGroup
        Left = 5
        Top = 5
        Width = 281
        Height = 66
        Caption = ' Target &TreePad version '
        ItemIndex = 0
        Items.Strings = (
          'Export to TreePad freeware (text only)'
          'Export to Treepad shareware (formatted text)')
        TabOrder = 0
      end
      object RG_TreePadMode: TTntRadioGroup
        Left = 5
        Top = 75
        Width = 281
        Height = 66
        Caption = ' T&arget file mode '
        ItemIndex = 0
        Items.Strings = (
          'Create a TreePad file for each exported note'
          'Create a Treepad file containing all exported notes')
        TabOrder = 1
      end
      object RG_TreePadMaster: TTntRadioGroup
        Left = 5
        Top = 145
        Width = 281
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
  object SaveDlg: TTntSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist]
    Title = 'Select target filename'
    Left = 265
    Top = 364
  end
end
