object UpdateVersion: TUpdateVersion
  Left = 348
  Top = 228
  HelpContext = 4
  Caption = 'Check for Updates - KeyNote NF'
  ClientHeight = 443
  ClientWidth = 840
  Color = clBtnFace
  Constraints.MinHeight = 278
  Constraints.MinWidth = 597
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  DesignSize = (
    840
    443)
  TextHeight = 16
  object Label1: TLabel
    Left = 20
    Top = 11
    Width = 136
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Your version is :'
  end
  object Label2: TLabel
    Left = 20
    Top = 33
    Width = 136
    Height = 16
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Current version is :'
  end
  object lblDonations: TLabel
    Left = 23
    Top = 406
    Width = 142
    Height = 14
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Support the developer'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    StyleElements = [seClient, seBorder]
    OnClick = lblDonationsClick
    OnMouseEnter = lblVisitWebMouseEnter
    OnMouseLeave = lblVisitWebMouseLeave
    ExplicitTop = 421
  end
  object lblVisitWeb: TLabel
    Left = 536
    Top = 57
    Width = 278
    Height = 16
    Cursor = crHandPoint
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'Click here to go to the website'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    StyleElements = [seClient, seBorder]
    OnClick = lblVisitWebClick
    OnMouseEnter = lblVisitWebMouseEnter
    OnMouseLeave = lblVisitWebMouseLeave
  end
  object lblInstalledVersion: TLabel
    Left = 163
    Top = 11
    Width = 12
    Height = 16
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblCurrentVersion: TLabel
    Left = 163
    Top = 33
    Width = 12
    Height = 16
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblStatus: TLabel
    Left = 273
    Top = 33
    Width = 12
    Height = 16
    Caption = '...'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 23
    Top = 67
    Width = 64
    Height = 16
    Caption = 'Changes :'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object chkCheckUpd: TCheckBox
    Left = 464
    Top = 407
    Width = 251
    Height = 16
    Anchors = [akRight, akBottom]
    Caption = ' &Check for updates on startup'
    TabOrder = 0
  end
  object Button_OK: TButton
    Left = 721
    Top = 406
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Close'
    Default = True
    TabOrder = 1
    OnClick = Button_OKClick
  end
  object txtChanges: TMemo
    Left = 20
    Top = 89
    Width = 794
    Height = 302
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
