object Form_NoteEntriesOptions: TForm_NoteEntriesOptions
  Left = 379
  Top = 248
  HelpContext = 30
  BorderStyle = bsDialog
  Caption = '%s panel in %s'
  ClientHeight = 469
  ClientWidth = 401
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
    TabOrder = 4
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
    TabOrder = 5
    OnClick = btn_CancelClick
  end
  object btn_Help: TButton
    Left = 318
    Top = 436
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 6
    OnClick = btn_HelpClick
  end
  object gbDisplay: TGroupBox
    Left = 8
    Top = 6
    Width = 385
    Height = 118
    Caption = ' Display '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 0
    object lbl10: TLabel
      Left = 13
      Top = 26
      Width = 110
      Height = 13
      Hint = 'Display initially, by default, for each entry'
      Caption = 'Display for each entry:'
    end
    object lbl7: TLabel
      Left = 265
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
      Left = 160
      Top = 77
      Width = 110
      Height = 17
      Hint = 'Show entries with less spacing'
      Caption = 'Compact'
      TabOrder = 1
    end
    object CB_DescOrd: TCheckBox
      Left = 13
      Top = 77
      Width = 147
      Height = 17
      Hint = 
        'Entries are displayed in descending order according to their cre' +
        'ation date (most recent at the top)'
      Caption = 'Descending order'
      TabOrder = 2
    end
    object cb_HTags: TCheckBox
      Left = 273
      Top = 69
      Width = 110
      Height = 17
      Hint = 'Displays entry tags'
      Caption = 'Tags'
      TabOrder = 3
    end
    object cb_HDate: TCheckBox
      Left = 273
      Top = 92
      Width = 110
      Height = 17
      Hint = 'Display the date and time the entry was created'
      Caption = 'Date / time'
      TabOrder = 4
    end
    object cb_HLine: TCheckBox
      Left = 273
      Top = 47
      Width = 110
      Height = 17
      Hint = 'Displays a line at the beginning of the header'
      Caption = 'Line'
      TabOrder = 5
    end
  end
  object gbFilter: TGroupBox
    Left = 8
    Top = 159
    Width = 386
    Height = 234
    Caption = ' Filter entries / content '
    DefaultHeaderFont = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -12
    HeaderFont.Name = 'Tahoma'
    HeaderFont.Style = [fsBold]
    TabOrder = 2
    DesignSize = (
      386
      234)
    object Label3: TLabel
      Left = 14
      Top = 140
      Width = 85
      Height = 13
      AutoSize = False
      Caption = 'TEXT included:'
    end
    object lbl1: TLabel
      Left = 43
      Top = 164
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'Type:'
    end
    object lbl4: TLabel
      Left = 14
      Top = 32
      Width = 48
      Height = 13
      AutoSize = False
      Caption = 'TAGs'
    end
    object lbl9: TLabel
      Left = 24
      Top = 79
      Width = 55
      Height = 13
      Hint = 'Exclude text/entries with ANY of the selected tags'
      AutoSize = False
      Caption = 'Without:'
    end
    object lbl8: TLabel
      Left = 25
      Top = 55
      Width = 55
      Height = 13
      Hint = 'Include text/entries with ALL or ANY of the selected tags'
      AutoSize = False
      Caption = 'With:'
    end
    object cbType: TComboBox
      Left = 101
      Top = 162
      Width = 120
      Height = 21
      Hint = 'Select type of search to perform'
      Style = csDropDownList
      TabOrder = 6
    end
    object txtText: TEdit
      Left = 101
      Top = 138
      Width = 120
      Height = 19
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 5
    end
    object chkWholeWords: TCheckBox
      Left = 43
      Top = 207
      Width = 227
      Height = 17
      Hint = 'Consider only complete words'
      Caption = 'Whole words only'
      TabOrder = 8
    end
    object chkCaseSens: TCheckBox
      Left = 43
      Top = 188
      Width = 227
      Height = 17
      Hint = 'Distinguish between lowercase and uppercase letters'
      Caption = 'Match case'
      TabOrder = 7
    end
    object txtTagsExcl: TEdit
      Left = 81
      Top = 77
      Width = 140
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 3
      OnEnter = txtTagsExclEnter
    end
    object cbTagFindMode: TComboBox
      Left = 150
      Top = 28
      Width = 71
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      TabOrder = 1
      OnChange = cbTagFindModeChange
      Items.Strings = (
        'ALL'
        'ANY')
    end
    object txtTagsIncl: TEdit
      Left = 81
      Top = 53
      Width = 140
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 2
      OnEnter = txtTagsInclEnter
    end
    object chkExcerpts: TCheckBox
      Left = 238
      Top = 120
      Width = 145
      Height = 17
      Hint = 'Show only excerpts satisfying the filter condition'
      Caption = 'Show Excerpts'
      TabOrder = 10
    end
    object chkTagsText: TCheckBox
      Left = 25
      Top = 101
      Width = 206
      Height = 17
      Hint = 'Tags will be searched for in the entries'#39' text'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Consider text of entries'
      TabOrder = 4
    end
    object chkHidden: TCheckBox
      Left = 238
      Top = 97
      Width = 145
      Height = 17
      Hint = 'Consider hidden entries'
      Caption = 'Consider Hidden'
      TabOrder = 9
    end
    object chkEnabled: TCheckBox
      Left = 288
      Top = 13
      Width = 88
      Height = 19
      Hint = 'Enable filter'
      Caption = 'Enabled'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkEnabledClick
    end
  end
  object btnRestoreDef: TButton
    Left = 248
    Top = 399
    Width = 145
    Height = 25
    Hint = 
      'Reset the configuration (display, filter) of %s panel in %s to t' +
      'he folder defaults'
    Caption = 'Restore Defaults'
    TabOrder = 3
    OnClick = btnRestoreDefClick
  end
  object chkResetSizes: TCheckBox
    Left = 21
    Top = 129
    Width = 342
    Height = 17
    Hint = 
      'Restore the width/height ratio of the panels in this note to the' +
      ' default for %s of the current folder'
    Caption = 'Reset Panel Sizes'
    TabOrder = 1
  end
  object FormPlacement: TFormPlacement
    IniSection = 'PropDlg'
    Options = [fpPosition]
    UseRegistry = True
    Left = 384
    Top = 458
  end
end
