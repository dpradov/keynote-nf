object Form_NoteEntriesOptions: TForm_NoteEntriesOptions
  Left = 379
  Top = 248
  HelpContext = 30
  BorderStyle = bsDialog
  Caption = '%s panel in %s'
  ClientHeight = 534
  ClientWidth = 394
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
  object btn_OK: TButton
    Left = 15
    Top = 503
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btn_OKClick
  end
  object btn_Cancel: TButton
    Left = 103
    Top = 503
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = btn_CancelClick
  end
  object btn_Help: TButton
    Left = 305
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = btn_HelpClick
  end
  object gbDisplay: TGroupBox
    Left = 8
    Top = 6
    Width = 378
    Height = 147
    Caption = ' Display '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 5
    DesignSize = (
      378
      147)
    object lbl10: TLabel
      Left = 13
      Top = 26
      Width = 110
      Height = 13
      Hint = 'Display initially, by default, for each entry'
      Caption = 'Display for each entry:'
    end
    object lbl7: TLabel
      Left = 258
      Top = 26
      Width = 68
      Height = 13
      Caption = 'Entry Header:'
    end
    object lbl3: TLabel
      Left = 13
      Top = 99
      Width = 117
      Height = 13
      AutoSize = False
      Caption = 'Folded text:'
    end
    object cEntryCont: TComboBox
      Left = 17
      Top = 45
      Width = 204
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object cb_CompHd: TCheckBox
      Left = 153
      Top = 73
      Width = 110
      Height = 17
      Hint = 'Show entries with less spacing'
      Caption = 'Compact'
      TabOrder = 2
    end
    object CB_DescOrd: TCheckBox
      Left = 17
      Top = 73
      Width = 136
      Height = 17
      Hint = 
        'Entries are displayed in descending order according to their cre' +
        'ation date (most recent at the top)'
      Caption = 'Descending order'
      TabOrder = 1
    end
    object cb_HTags: TCheckBox
      Left = 266
      Top = 69
      Width = 105
      Height = 17
      Hint = 'Displays entry tags'
      Caption = 'Tags'
      TabOrder = 4
    end
    object cb_HDate: TCheckBox
      Left = 266
      Top = 92
      Width = 105
      Height = 17
      Hint = 'Display the date the entry was created'
      Caption = 'Date'
      TabOrder = 5
      OnClick = cb_HDateClick
    end
    object cb_HLine: TCheckBox
      Left = 266
      Top = 47
      Width = 105
      Height = 17
      Hint = 'Displays a line at the beginning of the header'
      Caption = 'Line'
      TabOrder = 3
    end
    object cbFoldedText: TComboBox
      Left = 21
      Top = 116
      Width = 204
      Height = 21
      HelpType = htKeyword
      HelpKeyword = '57-18'
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 7
      OnChange = cbFoldedTextChange
      Items.Strings = (
        'Keep unchanged'
        'Unfold'
        'Remove All')
    end
    object cb_HTime: TCheckBox
      Left = 266
      Top = 115
      Width = 105
      Height = 17
      Hint = 'Display the time the entry was created'
      Caption = 'Time'
      TabOrder = 6
    end
  end
  object gbFilter: TGroupBox
    Left = 8
    Top = 161
    Width = 377
    Height = 230
    Caption = ' Filter entries / content '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 0
    DesignSize = (
      377
      230)
    object Label3: TLabel
      Left = 14
      Top = 97
      Width = 161
      Height = 13
      AutoSize = False
      Caption = 'With TEXT:'
    end
    object lbl4: TLabel
      Left = 297
      Top = 36
      Width = 72
      Height = 13
      AutoSize = False
      Caption = 'TAGs'
    end
    object lbl9: TLabel
      Left = 194
      Top = 36
      Width = 79
      Height = 13
      Hint = 'Exclude text/entries with ANY of the selected tags'
      AutoSize = False
      Caption = 'Without:'
    end
    object lbl8: TLabel
      Left = 14
      Top = 36
      Width = 68
      Height = 13
      Hint = 'Include text/entries with ALL or ANY of the selected tags'
      AutoSize = False
      Caption = 'With:'
    end
    object cbType: TComboBox
      Left = 194
      Top = 114
      Width = 140
      Height = 21
      Hint = 'Select type of search to perform'
      Style = csDropDownList
      TabOrder = 7
    end
    object txtText: TEdit
      Left = 19
      Top = 116
      Width = 143
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 5
    end
    object chkWholeWords: TCheckBox
      Left = 194
      Top = 141
      Width = 180
      Height = 17
      Hint = 'Consider only complete words'
      Caption = 'Whole words only'
      TabOrder = 9
    end
    object chkCaseSens: TCheckBox
      Left = 19
      Top = 141
      Width = 169
      Height = 17
      Hint = 'Distinguish between lowercase and uppercase letters'
      Caption = 'Match case'
      TabOrder = 8
    end
    object txtTagsExcl: TEdit
      Left = 194
      Top = 56
      Width = 140
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 3
      OnEnter = txtTagsExclEnter
    end
    object cbTagFindMode: TComboBox
      Left = 88
      Top = 31
      Width = 74
      Height = 21
      Style = csDropDownList
      Anchors = [akTop]
      TabOrder = 1
      OnChange = cbTagFindModeChange
      Items.Strings = (
        'ALL'
        'ANY')
    end
    object txtTagsIncl: TEdit
      Left = 19
      Top = 56
      Width = 143
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 2
      OnEnter = txtTagsInclEnter
    end
    object chkExcerpts: TCheckBox
      Left = 194
      Top = 204
      Width = 177
      Height = 17
      Hint = 'Show only excerpts satisfying the filter condition'
      Caption = 'Show Excerpts'
      TabOrder = 11
    end
    object chkTagsText: TCheckBox
      Left = 194
      Top = 81
      Width = 197
      Height = 17
      Hint = 'Tags will be searched for in the entries'#39' text'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Consider text of entries'
      TabOrder = 4
    end
    object chkHidden: TCheckBox
      Left = 194
      Top = 182
      Width = 177
      Height = 17
      Hint = 'Consider hidden entries'
      Caption = 'Consider Hidden'
      TabOrder = 10
    end
    object chkEnabled: TCheckBox
      Left = 287
      Top = -2
      Width = 88
      Height = 19
      Hint = 'Enable filter (Ctrl+Click: Reapply)'
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEnabledClick
    end
    object chkApplyAll: TCheckBox
      Left = 12
      Top = 204
      Width = 177
      Height = 17
      Hint = 'Apply the filter to all the panels in this layout'
      Caption = 'Apply to all panels'
      TabOrder = 6
    end
  end
  object btnRestoreDef: TButton
    Left = 202
    Top = 397
    Width = 178
    Height = 25
    Hint = 
      'Reset the configuration (display, filter) of %s panel in %s to t' +
      'he folder defaults'
    Caption = 'Restore Defaults in Panel'
    TabOrder = 1
    OnClick = btnRestoreDefClick
  end
  object gbLayout: TGroupBox
    Left = 8
    Top = 424
    Width = 378
    Height = 73
    Caption = ' Layout '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 6
    object lTL: TLabel
      Left = 146
      Top = 14
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'TL'
    end
    object lTR: TLabel
      Left = 176
      Top = 14
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'TR'
    end
    object lC: TLabel
      Left = 225
      Top = 14
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'C'
    end
    object lBL: TLabel
      Left = 274
      Top = 14
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'BL'
    end
    object lBR: TLabel
      Left = 305
      Top = 14
      Width = 20
      Height = 13
      AutoSize = False
      Caption = 'BR'
    end
    object chkResetSizes: TCheckBox
      Left = 12
      Top = 51
      Width = 163
      Height = 17
      Hint = 
        'Restore the width/height ratio of the panels in this note to the' +
        ' default for %s of the current folder'
      Caption = 'Reset Panel Sizes'
      TabOrder = 0
    end
    object cb_TL: TCheckBox
      Tag = 1
      Left = 144
      Top = 30
      Width = 18
      Height = 17
      TabOrder = 1
    end
    object cb_TR: TCheckBox
      Tag = 2
      Left = 174
      Top = 30
      Width = 18
      Height = 17
      TabOrder = 2
    end
    object cb_C: TCheckBox
      Tag = 5
      Left = 221
      Top = 30
      Width = 18
      Height = 17
      TabOrder = 3
    end
    object cb_BL: TCheckBox
      Tag = 3
      Left = 272
      Top = 30
      Width = 18
      Height = 17
      TabOrder = 4
    end
    object cb_BR: TCheckBox
      Tag = 4
      Left = 303
      Top = 30
      Width = 18
      Height = 17
      TabOrder = 5
    end
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 376
    Top = 498
  end
end
