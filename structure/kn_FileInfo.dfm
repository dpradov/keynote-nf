object Form_FileInfo: TForm_FileInfo
  Left = 298
  Top = 325
  HelpContext = 500
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'File properties'
  ClientHeight = 255
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label11: TTntLabel
    Left = 10
    Top = 75
    Width = 43
    Height = 13
    Caption = 'Created:'
  end
  object Label12: TTntLabel
    Left = 70
    Top = 75
    Width = 20
    Height = 13
    Caption = '(...)'
  end
  object Button_OK: TTntButton
    Left = 14
    Top = 225
    Width = 79
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button_OKClick
  end
  object Button_Cancel: TTntButton
    Left = 98
    Top = 225
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = Button_CancelClick
  end
  object Pages: TPageControl
    Left = 5
    Top = 5
    Width = 452
    Height = 211
    HelpContext = 230
    ActivePage = Tab_Main
    TabOrder = 0
    object Tab_Main: TTabSheet
      HelpContext = 231
      Caption = 'Information'
      object GroupBox1: TTntGroupBox
        Left = 5
        Top = 0
        Width = 432
        Height = 176
        TabOrder = 0
        object Label1: TTntLabel
          Left = 10
          Top = 15
          Width = 79
          Height = 13
          AutoSize = False
          Caption = 'Filename:'
        end
        object Label2: TTntLabel
          Left = 10
          Top = 75
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Comment:'
          FocusControl = Edit_Comment
        end
        object Label3: TTntLabel
          Left = 10
          Top = 140
          Width = 66
          Height = 13
          AutoSize = False
          Caption = 'Created:'
        end
        object Label4: TTntLabel
          Left = 10
          Top = 155
          Width = 66
          Height = 13
          AutoSize = False
          Caption = 'Modified:'
        end
        object Label_Created: TTntLabel
          Left = 80
          Top = 140
          Width = 218
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label_Modified: TTntLabel
          Left = 80
          Top = 155
          Width = 218
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label5: TTntLabel
          Left = 321
          Top = 140
          Width = 56
          Height = 13
          AutoSize = False
          Caption = 'Notes:'
        end
        object Label_Count: TTntLabel
          Left = 381
          Top = 140
          Width = 7
          Height = 13
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TTntLabel
          Left = 321
          Top = 155
          Width = 56
          Height = 13
          AutoSize = False
          Caption = 'File size:'
        end
        object Label_FileSize: TTntLabel
          Left = 381
          Top = 155
          Width = 44
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label_FileNotFound: TTntLabel
          Left = 91
          Top = 15
          Width = 148
          Height = 13
          Caption = ' FILE HAS NOT BEEN SAVED '
          Color = clRed
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Visible = False
        end
        object Label6: TTntLabel
          Left = 10
          Top = 45
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Description:'
          FocusControl = Edit_Description
        end
        object Label8: TTntLabel
          Left = 10
          Top = 105
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Format:'
          FocusControl = Combo_Format
        end
        object Bevel2: TBevel
          Left = 10
          Top = 130
          Width = 402
          Height = 2
          Shape = bsTopLine
        end
        object LB_RTF3: TTntLabel
          Left = 303
          Top = 155
          Width = 14
          Height = 13
          Caption = '(3)'
          ShowAccelChar = False
          Visible = False
        end
        object Edit_FileName: TTntEdit
          Left = 88
          Top = 15
          Width = 329
          Height = 21
          HelpContext = 520
          TabStop = False
          BorderStyle = bsNone
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 3
        end
        object Edit_Comment: TTntEdit
          Left = 88
          Top = 70
          Width = 327
          Height = 21
          Hint = 'Enter optional comment for the file'
          HelpContext = 530
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          TabOrder = 1
        end
        object Edit_Description: TTntEdit
          Left = 88
          Top = 40
          Width = 327
          Height = 21
          Hint = 'Enter optional description for the file'
          HelpContext = 525
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          TabOrder = 0
        end
        object Combo_Format: TTntComboBox
          Left = 88
          Top = 100
          Width = 327
          Height = 21
          Hint = 'Choose format used when saving the file'
          HelpContext = 535
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object Tab_Settings: TTabSheet
      HelpContext = 232
      Caption = 'Settings'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TTntGroupBox
        Left = 5
        Top = 0
        Width = 433
        Height = 176
        TabOrder = 0
        object Bevel1: TBevel
          Left = 10
          Top = 65
          Width = 410
          Height = 2
          Shape = bsTopLine
        end
        object Label_IsReadOnly: TTntLabel
          Left = 15
          Top = 40
          Width = 170
          Height = 13
          Caption = 'File was opened as Read-Only.'
          Color = clRed
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object TntLabel1: TTntLabel
          Left = 15
          Top = 129
          Width = 97
          Height = 13
          AutoSize = False
          Caption = '&Compress Level:'
          FocusControl = Combo_CompressLevel
        end
        object Bevel4: TBevel
          Left = 10
          Top = 113
          Width = 410
          Height = 2
          Shape = bsTopLine
        end
        object CB_AsReadOnly: TTntCheckBox
          Left = 15
          Top = 15
          Width = 326
          Height = 17
          Hint = 'In future, open this file as Read-only'
          HelpContext = 540
          Caption = 'Open this file in &Read-Only Mode'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CB_NoMultiBackup: TTntCheckBox
          Left = 15
          Top = 80
          Width = 346
          Height = 17
          Hint = 'Ignore global backup level setting for this file'
          Caption = 'Do not keep &multiple backups of this file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object Combo_CompressLevel: TTntComboBox
          Left = 123
          Top = 126
          Width = 226
          Height = 21
          Hint = 'Choose level of compression used when saving the file'
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ItemHeight = 0
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object Tab_Icons: TTabSheet
      HelpContext = 233
      Caption = 'File Icons'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox4: TTntGroupBox
        Left = 5
        Top = 0
        Width = 433
        Height = 176
        TabOrder = 0
        object Image_TrayIcon: TImage
          Left = 378
          Top = 127
          Width = 38
          Height = 38
          AutoSize = True
        end
        object Bevel3: TBevel
          Left = 10
          Top = 100
          Width = 408
          Height = 5
          Shape = bsTopLine
        end
        object TB_OpenDlgTrayIcon: TToolbarButton97
          Left = 336
          Top = 140
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Enabled = False
          Flat = False
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgTrayIconClick
        end
        object TB_OpenDlgTabImg: TToolbarButton97
          Left = 336
          Top = 60
          Width = 25
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Enabled = False
          Flat = False
          Glyph.Data = {00000000}
          GlyphMask.Data = {00000000}
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgTabImgClick
        end
        object CB_ShowTabIcons: TTntCheckBox
          Left = 15
          Top = 15
          Width = 306
          Height = 17
          Hint = 'Display or hide tab images for this file'
          HelpContext = 545
          Caption = '&Show icon images on tabs'
          TabOrder = 0
        end
        object CB_TrayIcon: TTntCheckBox
          Left = 15
          Top = 115
          Width = 290
          Height = 17
          Caption = 'Use custom &Tray icon for this file'
          TabOrder = 1
        end
        object RB_TabImgDefault: TTntRadioButton
          Left = 15
          Top = 40
          Width = 110
          Height = 17
          Caption = '&Default'
          Checked = True
          TabOrder = 2
          TabStop = True
          OnClick = RB_TabImgOtherClick
        end
        object RB_TabImgBuiltIn: TTntRadioButton
          Left = 131
          Top = 40
          Width = 109
          Height = 17
          Caption = '&Built in'
          TabOrder = 3
          OnClick = RB_TabImgOtherClick
        end
        object RB_TabImgOther: TTntRadioButton
          Left = 247
          Top = 40
          Width = 98
          Height = 17
          Caption = '&Other'
          TabOrder = 4
          OnClick = RB_TabImgOtherClick
        end
        object Edit_TrayIcon: TTntEdit
          Left = 16
          Top = 140
          Width = 322
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
        end
        object Edit_TabImg: TTntEdit
          Left = 16
          Top = 60
          Width = 322
          Height = 21
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
      end
    end
    object Tab_Pass: TTabSheet
      HelpContext = 234
      Caption = 'Security'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox2: TTntGroupBox
        Left = 5
        Top = 0
        Width = 431
        Height = 176
        TabOrder = 0
        object Label_Confirm: TTntLabel
          Left = 10
          Top = 85
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Confirm pass.:'
          FocusControl = Edit_Confirm
        end
        object Label_Pass: TTntLabel
          Left = 10
          Top = 55
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Passphrase:'
          FocusControl = Edit_Pass
        end
        object Label_Method: TTntLabel
          Left = 10
          Top = 150
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Algorithm:'
          FocusControl = Combo_Method
        end
        object Label_EnterPass: TTntLabel
          Left = 200
          Top = 20
          Width = 171
          Height = 13
          Caption = ' Enter new access passphrase '
          Color = clHighlight
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clHighlightText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object Edit_Confirm: TTntEdit
          Left = 96
          Top = 80
          Width = 313
          Height = 21
          Hint = 'Carefully re-enter the access passphrase'
          HelpContext = 560
          MaxLength = 255
          PasswordChar = '*'
          TabOrder = 1
        end
        object Edit_Pass: TTntEdit
          Left = 96
          Top = 50
          Width = 313
          Height = 21
          Hint = 'Enter access passphrase for this file'
          HelpContext = 555
          MaxLength = 255
          PasswordChar = '*'
          TabOrder = 0
        end
        object Combo_Method: TTntComboBox
          Left = 96
          Top = 145
          Width = 313
          Height = 21
          Hint = 'Select which encryption algorithm to use'
          HelpContext = 565
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 2
        end
        object Button_SetPass: TTntButton
          Left = 10
          Top = 15
          Width = 181
          Height = 25
          Hint = 'Click to change access passphrase for the encrypted file'
          HelpContext = 550
          Caption = '&Set Passphrase'
          TabOrder = 3
          OnClick = Button_SetPassClick
        end
        object CB_HidePass: TTntCheckBox
          Left = 96
          Top = 110
          Width = 268
          Height = 17
          Hint = 'Obscure passphrase with asterisks'
          HelpContext = 570
          Caption = '&Hide passphrase while typing'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
    end
  end
  object Button_System: TTntButton
    Left = 369
    Top = 225
    Width = 82
    Height = 25
    Hint = 'Displays system File Properties dialog box'
    Caption = 'S&ystem...'
    TabOrder = 4
    OnClick = Button_SystemClick
  end
  object Button_Help: TTntButton
    Left = 178
    Top = 225
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button_HelpClick
  end
  object FormPlacement: TFormPlacement
    IniSection = 'InfoDlg'
    Options = [fpPosition]
    Left = 261
    Top = 220
  end
end
