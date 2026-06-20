object Form_NoteEntriesOptions: TForm_NoteEntriesOptions
  Left = 379
  Top = 248
  HelpContext = 30
  BorderStyle = bsDialog
  Caption = '%s panel in %s'
  ClientHeight = 469
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
    Top = 436
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
    Top = 436
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
    Top = 436
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 4
    OnClick = btn_HelpClick
  end
  object gbDisplay: TGroupBox
    Left = 8
    Top = 6
    Width = 374
    Height = 118
    Caption = ' Display '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 5
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
    object cEntryCont: TComboBox
      Left = 17
      Top = 45
      Width = 204
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cEntryContChange
    end
    object cb_CompHd: TCheckBox
      Left = 149
      Top = 77
      Width = 110
      Height = 17
      Hint = 'Show entries with less spacing'
      Caption = 'Compact'
      TabOrder = 2
    end
    object CB_DescOrd: TCheckBox
      Left = 13
      Top = 77
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
      Hint = 'Display the date and time the entry was created'
      Caption = 'Date / time'
      TabOrder = 5
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
  end
  object gbFilter: TGroupBox
    Left = 9
    Top = 142
    Width = 377
    Height = 240
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
      240)
    object Label3: TLabel
      Left = 14
      Top = 100
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
      Top = 117
      Width = 140
      Height = 21
      Hint = 'Select type of search to perform'
      Style = csDropDownList
      TabOrder = 7
    end
    object txtText: TEdit
      Left = 19
      Top = 119
      Width = 143
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 5
    end
    object chkWholeWords: TCheckBox
      Left = 194
      Top = 144
      Width = 180
      Height = 17
      Hint = 'Consider only complete words'
      Caption = 'Whole words only'
      TabOrder = 9
    end
    object chkCaseSens: TCheckBox
      Left = 19
      Top = 144
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
      Top = 216
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
      Top = 194
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
      Left = 13
      Top = 216
      Width = 177
      Height = 17
      Hint = 'Apply the filter to all the panels in this layout'
      Caption = 'Apply to all panels'
      TabOrder = 6
    end
  end
  object btnRestoreDef: TButton
    Left = 235
    Top = 403
    Width = 145
    Height = 25
    Hint = 
      'Reset the configuration (display, filter) of %s panel in %s to t' +
      'he folder defaults'
    Caption = 'Restore Defaults'
    TabOrder = 1
    OnClick = btnRestoreDefClick
  end
  object chkResetSizes: TCheckBox
    Left = 14
    Top = 403
    Width = 206
    Height = 17
    Hint = 
      'Restore the width/height ratio of the panels in this note to the' +
      ' default for %s of the current folder'
    Caption = 'Reset Panel Sizes'
    TabOrder = 6
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 376
    Top = 450
  end
end
