object AboutBox: TAboutBox
  Left = 302
  Top = 289
  HelpContext = 2
  BorderIcons = []
  BorderStyle = bsToolWindow
  ClientHeight = 367
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object BTN_Close: TSpeedButton
    Left = 173
    Top = 333
    Width = 91
    Height = 25
    Caption = '&Ok'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    NumGlyphs = 2
    ParentFont = False
    Transparent = False
    OnClick = BTN_CloseClick
  end
  object lblDonations: TLabel
    Left = 283
    Top = 333
    Width = 128
    Height = 13
    Cursor = crHandPoint
    Caption = 'Support the developer'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    PopupMenu = NetMenu
    StyleElements = [seClient, seBorder]
    OnClick = lblDonationsClick
    OnMouseDown = Label_MAILTOMouseDown
    OnMouseUp = Label_MAILTOMouseUp
  end
  object Panel_Main: TPanel
    Left = 8
    Top = 10
    Width = 397
    Height = 317
    BevelOuter = bvSpace
    BorderWidth = 1
    BorderStyle = bsSingle
    Color = 13684944
    TabOrder = 0
    object Label_Name: TLabel
      Left = 69
      Top = 13
      Width = 113
      Height = 23
      Caption = 'KeyNote NF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ShowAccelChar = False
    end
    object Label_Desc: TLabel
      Left = 69
      Top = 43
      Width = 36
      Height = 18
      Caption = 'Desc'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ParentShowHint = False
      ShowAccelChar = False
      ShowHint = False
    end
    object Label_License: TLabel
      Left = 71
      Top = 72
      Width = 44
      Height = 15
      Caption = 'License'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
    end
    object Label9: TLabel
      Left = 83
      Top = 162
      Width = 28
      Height = 13
      Caption = 'email:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 80
      Top = 177
      Width = 31
      Height = 13
      Caption = 'Home:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label_URL: TLabel
      Left = 125
      Top = 177
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      PopupMenu = NetMenu
      OnDblClick = Label_URLDblClick
      OnMouseDown = Label_MAILTOMouseDown
      OnMouseUp = Label_MAILTOMouseUp
    end
    object Label_MAILTO: TLabel
      Left = 125
      Top = 162
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      PopupMenu = NetMenu
      OnDblClick = Label_MAILTODblClick
      OnMouseDown = Label_MAILTOMouseDown
      OnMouseUp = Label_MAILTOMouseUp
    end
    object Label_Dart: TLabel
      Left = 66
      Top = 211
      Width = 29
      Height = 13
      Caption = 'Dart..'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Image1: TImage
      Left = 341
      Top = 259
      Width = 45
      Height = 45
      Cursor = crHandPoint
      Hint = 'Created with Delphi 11 Community Edition'
      ParentShowHint = False
      ShowHint = True
      OnDblClick = Image1DblClick
    end
    object LB_RichEditVer: TLabel
      Left = 66
      Top = 282
      Width = 9
      Height = 13
      Caption = 'rtf'
      ShowAccelChar = False
    end
    object Label_Credit2: TLabel
      Left = 71
      Top = 123
      Width = 55
      Height = 13
      Caption = 'Copyright..'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
    end
    object Label_Credit1: TLabel
      Left = 71
      Top = 104
      Width = 55
      Height = 13
      Caption = 'Copyright..'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ShowAccelChar = False
    end
    object Label_MAILTO2: TLabel
      Left = 125
      Top = 147
      Width = 4
      Height = 13
      Cursor = crHandPoint
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      PopupMenu = NetMenu
      OnDblClick = Label_MAILTODblClick
      OnMouseDown = Label_MAILTOMouseDown
      OnMouseUp = Label_MAILTOMouseUp
    end
    object Label6: TLabel
      Left = 83
      Top = 147
      Width = 28
      Height = 13
      Caption = 'email:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label_Version: TLabel
      Left = 193
      Top = 20
      Width = 44
      Height = 14
      Caption = 'version'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Image_Program: TImage
      Left = 11
      Top = 21
      Width = 43
      Height = 43
      Stretch = True
    end
    object Label_Version_Date: TLabel
      Left = 282
      Top = 21
      Width = 16
      Height = 13
      Caption = '(..)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label_KeynoteNF: TLabel
      Left = 66
      Top = 243
      Width = 43
      Height = 13
      Caption = 'KNT NF..'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object NetMenu: TPopupMenu
    Left = 17
    Top = 334
    object CopyEmailaddress1: TMenuItem
      Caption = 'Copy &E-mail address'
      OnClick = CopyEmailaddress1Click
    end
    object CopyuWebURL1: TMenuItem
      Caption = 'Copy &Web URL'
      OnClick = CopyuWebURL1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cancel1: TMenuItem
      Caption = '&Cancel'
    end
  end
end
