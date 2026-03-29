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
  object pnlEntries: TPanel
    Left = 0
    Top = 0
    Width = 659
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
  end
  object pnlIdentif: TPanel
    Left = 0
    Top = 456
    Width = 659
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      659
      24)
    object btnNextEntry: TToolbarButton97
      Left = 639
      Top = 2
      Width = 18
      Height = 22
      Hint = 'Next entry [Ctrl: Last]'
      Anchors = [akTop, akRight]
      ImageIndex = 56
      Images = Form_Main.IMG_Toolbar
      OnClick = btnNextEntryClick
      ExplicitLeft = 642
    end
    object btnPrevEntry: TToolbarButton97
      Left = 599
      Top = 2
      Width = 18
      Height = 22
      Hint = 'Previous entry [Ctrl: First]'
      Anchors = [akTop, akRight]
      DropdownArrowWidth = 12
      ImageIndex = 55
      Images = Form_Main.IMG_Toolbar
      OnClick = btnPrevEntryClick
      ExplicitLeft = 602
    end
    object btnOptions: TToolbarButton97
      Left = 577
      Top = 2
      Width = 20
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '...'
      ImageIndex = 39
      ExplicitLeft = 580
    end
    object btnToggleMulti: TToolbarButton97
      Left = 618
      Top = 2
      Width = 20
      Height = 22
      Anchors = [akTop, akRight]
      Caption = '1'
      ImageIndex = 39
      OnClick = btnToggleMultiClick
      ExplicitLeft = 621
    end
    object txtCreationDate: TEdit
      Left = 455
      Top = 2
      Width = 120
      Height = 22
      TabStop = False
      Alignment = taCenter
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 0
      OnEnter = txtEnter
      OnMouseEnter = txtCreationDateMouseEnter
    end
    object txtName: TEdit
      Left = 35
      Top = 2
      Width = 418
      Height = 22
      TabStop = False
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = txtNameChange
      OnEnter = txtEnter
      OnExit = txtNameExit
      OnMouseEnter = txtNameMouseEnter
    end
    object txtTags: TEdit
      Left = 0
      Top = 2
      Width = 33
      Height = 22
      HelpContext = 122
      TabStop = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnEnter = txtTagsEnter
    end
  end
end
