object KntNoteEntriesUI: TKntNoteEntriesUI
  Left = 0
  Top = 0
  Width = 659
  Height = 480
  Align = alClient
  BiDiMode = bdLeftToRight
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentBiDiMode = False
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    659
    480)
  object cFocusedFlag: TPaintBox
    Left = 0
    Top = 0
    Width = 1
    Height = 480
    Align = alLeft
    OnPaint = cFocusedFlagPaint
  end
  object pnlEntries: TPanel
    Left = 1
    Top = 0
    Width = 658
    Height = 456
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
  end
  object txtCreationDate: TEdit
    Left = 455
    Top = 458
    Width = 119
    Height = 24
    TabStop = False
    Alignment = taCenter
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    ReadOnly = True
    ShowHint = True
    TabOrder = 0
    OnEnter = txtEnter
    OnMouseEnter = txtCreationDateMouseEnter
  end
  object txtName: TEdit
    Left = 37
    Top = 458
    Width = 416
    Height = 24
    TabStop = False
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = txtNameChange
    OnEnter = txtEnter
    OnExit = txtNameExit
    OnMouseEnter = txtNameMouseEnter
  end
  object txtTags: TEdit
    Left = 2
    Top = 458
    Width = 33
    Height = 24
    HelpContext = 122
    TabStop = False
    Anchors = [akLeft, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnEnter = txtTagsEnter
    OnExit = txtTagsExit
  end
  object pnlButtons: TPanel
    Left = 576
    Top = 458
    Width = 82
    Height = 24
    Anchors = [akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    object btnNextEntry: TToolbarButton97
      Left = 63
      Top = 0
      Width = 19
      Height = 22
      Hint = 'Next entry [Ctrl: Last]'
      ImageIndex = 56
      Images = Form_Main.IMG_Toolbar
      OnClick = btnNextEntryClick
    end
    object btnToggleMulti: TToolbarButton97
      Left = 41
      Top = 0
      Width = 22
      Height = 22
      Caption = '1'
      ImageIndex = 39
      OnClick = btnToggleMultiClick
    end
    object btnPrevEntry: TToolbarButton97
      Left = 22
      Top = 0
      Width = 19
      Height = 22
      Hint = 'Previous entry [Ctrl: First]'
      DropdownArrowWidth = 12
      ImageIndex = 55
      Images = Form_Main.IMG_Toolbar
      OnClick = btnPrevEntryClick
    end
    object btnOptions: TToolbarButton97
      Left = 0
      Top = 0
      Width = 22
      Height = 22
      Caption = '...'
      ImageIndex = 39
      OnClick = btnOptionsClick
    end
  end
end
