object KntNoteUI: TKntNoteUI
  Left = 0
  Top = 0
  Width = 578
  Height = 480
  Align = alClient
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object pnlEntries: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
  end
  object pnlIdentif: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      578
      24)
    object txtCreationDate: TEdit
      Left = 458
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
      Left = 1
      Top = 2
      Width = 456
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
  end
end
