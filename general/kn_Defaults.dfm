object Form_Defaults: TForm_Defaults
  Left = 379
  Top = 248
  HelpContext = 250
  BorderStyle = bsDialog
  Caption = 'Defaults'
  ClientHeight = 391
  ClientWidth = 328
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
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Button_OK: TButton
    Left = 14
    Top = 360
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 102
    Top = 360
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object Pages: TPage95Control
    Left = 5
    Top = 5
    Width = 326
    Height = 340
    ActivePage = Tab_Tree
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
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
    object Tab_Main: TTab95Sheet
      HelpContext = 251
      Caption = 'Note settings'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GBox_Note: TGroupBox
        Left = 5
        Top = 0
        Width = 306
        Height = 309
        Caption = ' Default properties for new notes '
        TabOrder = 0
        object Bevel1: TBevel
          Left = 10
          Top = 86
          Width = 286
          Height = 5
          Shape = bsTopLine
        end
        object Label_TabSize: TLabel
          Left = 189
          Top = 152
          Width = 44
          Height = 13
          Alignment = taRightJustify
          Caption = 'Tab &Size:'
          FocusControl = Spin_TabSize
        end
        object Label1: TLabel
          Left = 10
          Top = 25
          Width = 74
          Height = 13
          AutoSize = False
          Caption = '&Note name:'
          FocusControl = Edit_NoteName
        end
        object Label4: TLabel
          Left = 10
          Top = 55
          Width = 74
          Height = 13
          AutoSize = False
          Caption = 'Note &icon:'
          FocusControl = Combo_Icons
        end
        object Bevel2: TBevel
          Left = 10
          Top = 228
          Width = 286
          Height = 5
          Shape = bsTopLine
        end
        object Label_EditorFonts: TLabel
          Left = 35
          Top = 221
          Width = 174
          Height = 13
          Caption = ' Default font and background color: '
          Enabled = False
          Transparent = False
        end
        object Label14: TLabel
          Left = 6
          Top = 183
          Width = 68
          Height = 13
          AutoSize = False
          Caption = '&Language:'
        end
        object Label_EditorSettings: TLabel
          Left = 35
          Top = 80
          Width = 104
          Height = 13
          Caption = ' RTF editor  settings: '
          Enabled = False
          Transparent = False
        end
        object CB_WordWrap: TCheckBox
          Left = 20
          Top = 100
          Width = 269
          Height = 17
          Hint = 'Apply word-wrapping to long lines'
          Caption = '&Wrap long lines'
          TabOrder = 2
        end
        object CB_URLDetect: TCheckBox
          Left = 20
          Top = 125
          Width = 269
          Height = 17
          Hint = 'Highlight URLs in editor'
          Caption = 'Detect and highlight &URLs in editor'
          TabOrder = 3
        end
        object CB_UseTabChar: TCheckBox
          Left = 20
          Top = 150
          Width = 149
          Height = 17
          Hint = 'Insert TAB character (#9) when Tab key pressed'
          Caption = 'Use &Tab character'
          TabOrder = 4
          OnClick = CB_UseTabCharClick
        end
        object Spin_TabSize: TSpinEdit
          Left = 236
          Top = 149
          Width = 56
          Height = 22
          Hint = 'Number of spaces to insert when Tab key pressed'
          MaxLength = 2
          MaxValue = 32
          MinValue = 1
          TabOrder = 5
          Value = 4
        end
        object Edit_NoteName: TComboBox
          Left = 88
          Top = 20
          Width = 198
          Height = 21
          Hint = 'Enter name for new note'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnKeyPress = Edit_NoteNameKeyPress
        end
        object Combo_Icons: TGFXComboBox
          Left = 88
          Top = 50
          Width = 198
          Height = 22
          Hint = 'Click to select icon for note'
          Extended = False
          DropDownCount = 10
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object Combo_DefEdLang: TLanguagesCombo
          Left = 76
          Top = 178
          Width = 216
          Height = 22
          Language = 2048
          LanguageType = ltInstalled
          ViewType = lvtLocalized
          ParentShowHint = False
          ShowFlag = False
          ShowHint = True
          TabOrder = 6
        end
      end
    end
    object Tab_Tree: TTab95Sheet
      HelpContext = 252
      Caption = 'Tree settings'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      object GBox_Tree: TGroupBox
        Left = 6
        Top = 0
        Width = 306
        Height = 309
        Caption = ' Default properties for new trees '
        TabOrder = 0
        object Bevel4: TBevel
          Left = 10
          Top = 81
          Width = 286
          Height = 5
          Shape = bsTopLine
        end
        object Label5: TLabel
          Left = 15
          Top = 25
          Width = 164
          Height = 13
          Caption = 'Default &Name for new tree nodes:'
          FocusControl = Edit_NodeName
        end
        object Bevel5: TBevel
          Left = 13
          Top = 228
          Width = 286
          Height = 5
          Shape = bsTopLine
        end
        object Label_TreeFonts: TLabel
          Left = 37
          Top = 221
          Width = 138
          Height = 13
          Caption = ' Font and background color: '
          Enabled = False
          Transparent = False
        end
        object Label_TreeSettings: TLabel
          Left = 35
          Top = 75
          Width = 73
          Height = 13
          Caption = ' Tree settings: '
          Enabled = False
          Transparent = False
        end
        object Label2: TLabel
          Left = 13
          Top = 181
          Width = 95
          Height = 13
          Caption = 'I&mage icons in tree:'
          FocusControl = Combo_TreeImages
        end
        object CB_TreeCheck: TCheckBox
          Left = 20
          Top = 135
          Width = 277
          Height = 17
          Hint = 'Display or hide checkboxes in ALL nodes'
          Caption = '&Show checkboxes in all nodes'
          TabOrder = 4
        end
        object Edit_NodeName: TComboBox
          Left = 15
          Top = 40
          Width = 211
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
          Left = 20
          Top = 95
          Width = 277
          Height = 17
          Hint = 'When adding a node, append sequential number to its name'
          Caption = '&Add sequential number to names of new nodes '
          TabOrder = 2
        end
        object BitBtn_TknHlp: TBitBtn
          Left = 235
          Top = 40
          Width = 25
          Height = 25
          Hint = 'Help for auto-naming tree nodes'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000010000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
            0ADAADADADADADA00DADDADA7777770B07DAADA0000000FF007DDA0FFFBFBFBF
            FF07AD0FBFFF44FFBF07DA0FFFBFFFBFFF07AD0FBFFF47FFBF07DA0FFFBF748F
            FF07AD0FBFFFB747BF07DA0FFF47FF44FF07AD0FBF44B844BF07DA0FFF844448
            FF07AD0FBFFFBFFFBF07DA0FFFBFFFBFFF0AADA00000000000AD}
          TabOrder = 1
        end
        object CB_Vertical: TCheckBox
          Left = 20
          Top = 115
          Width = 277
          Height = 17
          Hint = 'Check to show tree ABOVE the editor'
          Caption = '&Vertical layout (tree on top)'
          TabOrder = 3
        end
        object Combo_TreeImages: TComboBox
          Left = 13
          Top = 196
          Width = 176
          Height = 21
          Style = csDropDownList
          TabOrder = 5
        end
        object CB_HideChecked: TCheckBox
          Left = 20
          Top = 156
          Width = 277
          Height = 17
          Hint = 'Show or hide checked nodes'
          Caption = '&Hide checked nodes'
          TabOrder = 6
        end
      end
    end
    object Tab_Adv: TTab95Sheet
      HelpContext = 253
      Caption = 'Advanced'
      GripAlign = gaLeft
      ImageIndex = -1
      StaticPageIndex = -1
      TabVisible = True
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 3
        Top = 0
        Width = 308
        Height = 309
        Caption = ' Advanced settings '
        TabOrder = 0
        object LB_SaveAsDef: TLabel
          Left = 10
          Top = 20
          Width = 286
          Height = 61
          AutoSize = False
          Caption = 
            'Normally, these properties are used for all new notes you create' +
            '. If you select the option below, they will be used as default o' +
            'nly for the current file.'
          WordWrap = True
        end
        object LB_PlainText: TLabel
          Left = 10
          Top = 134
          Width = 286
          Height = 67
          AutoSize = False
          Caption = 
            'Normally, note contents are saved as Rich Text. If you select th' +
            'e option below, contents of this note will be saved as plain tex' +
            't (all formatting will be rermoved).'
          WordWrap = True
        end
        object CB_SaveAsDef: TCheckBox
          Left = 10
          Top = 81
          Width = 286
          Height = 17
          Caption = '&Save as default'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = AM_SaveAsDefClick
        end
        object CB_PlainText: TCheckBox
          Left = 10
          Top = 202
          Width = 286
          Height = 17
          Caption = '&Plain text only (do not save formatting information)'
          TabOrder = 1
          OnClick = AM_SaveAsDefClick
        end
      end
    end
  end
  object BTN_Font: TBitBtn
    Left = 28
    Top = 273
    Width = 90
    Height = 25
    Hint = 'Change initial font'
    Caption = '&Font'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADAADAD444444444444DADADADADADADADAADAD44444DAD4444DADADA44DADA
      D44AADADADA44DADA44DDADADADA4444444AADADADADA44DA44DD0DAD0DADA44
      D44AA07D70ADADA4444DD70007DADADA444AAD0D0DADADADA44DDA0A0ADADADA
      DADAAD707DADADADADADDAD0DADADADADADAADADADADADADADAD}
    TabOrder = 3
    OnClick = BTN_FontClick
  end
  object BTN_Color: TBitBtn
    Left = 123
    Top = 273
    Width = 90
    Height = 25
    Hint = 'Change background color'
    Caption = 'BG &Color'
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
    TabOrder = 4
    OnClick = BTN_ColorClick
  end
  object BTN_Defaults: TBitBtn
    Left = 218
    Top = 273
    Width = 90
    Height = 25
    Hint = 'Reset factory default fonts and colors'
    Caption = '&Reset'
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
      7777777777777777777777777777777777777777777777777777777777777777
      7777777777777778477777444447777748777744447777777477774447777777
      7477774474777777747777477744777748777777777744448777777777777777
      7777777777777777777777777777777777777777777777777777}
    TabOrder = 5
    OnClick = BTN_DefaultsClick
  end
  object Edit_Sample: TEdit
    Left = 28
    Top = 304
    Width = 281
    Height = 21
    MaxLength = 127
    ReadOnly = True
    TabOrder = 6
  end
  object Button_Help: TButton
    Left = 246
    Top = 360
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 7
    OnClick = Button_HelpClick
  end
  object ColorDlg: TColorDialog
    Options = [cdFullOpen, cdSolidColor, cdAnyColor]
    Left = 124
    Top = 404
  end
  object FontDlg: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdEffects, fdForceFontExist]
    Left = 89
    Top = 399
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 308
    Top = 323
  end
end
