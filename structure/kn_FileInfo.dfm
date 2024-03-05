object Form_FileInfo: TForm_FileInfo
  Left = 298
  Top = 325
  HelpContext = 287
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'File properties'
  ClientHeight = 297
  ClientWidth = 530
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  ShowHint = True
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnHelp = FormHelp
  OnKeyDown = FormKeyDown
  TextHeight = 13
  object Label11: TLabel
    Left = 10
    Top = 75
    Width = 43
    Height = 13
    Caption = 'Created:'
  end
  object Label12: TLabel
    Left = 70
    Top = 75
    Width = 20
    Height = 13
    Caption = '(...)'
  end
  object Button_OK: TButton
    Left = 38
    Top = 264
    Width = 79
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button_OKClick
  end
  object Button_Cancel: TButton
    Left = 122
    Top = 264
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
    Width = 522
    Height = 246
    HelpContext = 287
    ActivePage = Tab_Main
    TabOrder = 0
    object Tab_Main: TTabSheet
      HelpContext = 288
      Caption = 'Information'
      object GroupBox1: TGroupBox
        Left = 5
        Top = 0
        Width = 505
        Height = 211
        TabOrder = 0
        object Label1: TLabel
          Left = 23
          Top = 15
          Width = 108
          Height = 13
          AutoSize = False
          Caption = 'Filename:'
        end
        object Label2: TLabel
          Left = 23
          Top = 75
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Comment:'
          FocusControl = Edit_Comment
        end
        object Label3: TLabel
          Left = 35
          Top = 158
          Width = 66
          Height = 13
          AutoSize = False
          Caption = 'Created:'
        end
        object Label4: TLabel
          Left = 35
          Top = 173
          Width = 66
          Height = 13
          AutoSize = False
          Caption = 'Modified:'
        end
        object Label_Created: TLabel
          Left = 101
          Top = 158
          Width = 218
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label_Modified: TLabel
          Left = 101
          Top = 173
          Width = 218
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label5: TLabel
          Left = 350
          Top = 158
          Width = 56
          Height = 13
          AutoSize = False
          Caption = 'Notes:'
        end
        object Label_Count: TLabel
          Left = 410
          Top = 158
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
        object Label7: TLabel
          Left = 350
          Top = 173
          Width = 56
          Height = 13
          AutoSize = False
          Caption = 'File size:'
        end
        object Label_FileSize: TLabel
          Left = 410
          Top = 173
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '(...)'
        end
        object Label_FileNotFound: TLabel
          Left = 104
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
        object Label6: TLabel
          Left = 23
          Top = 45
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Description:'
          FocusControl = Edit_Description
        end
        object Label8: TLabel
          Left = 23
          Top = 105
          Width = 73
          Height = 13
          AutoSize = False
          Caption = '&Format:'
          FocusControl = Combo_Format
        end
        object LB_RTF3: TLabel
          Left = 332
          Top = 173
          Width = 14
          Height = 13
          Caption = '(3)'
          ShowAccelChar = False
          Visible = False
        end
        object Edit_FileName: TEdit
          Left = 101
          Top = 15
          Width = 380
          Height = 17
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
        object Edit_Comment: TEdit
          Left = 101
          Top = 70
          Width = 380
          Height = 21
          Hint = 'Enter optional comment for the file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          TabOrder = 1
        end
        object Edit_Description: TEdit
          Left = 101
          Top = 40
          Width = 380
          Height = 21
          Hint = 'Enter optional description for the file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          MaxLength = 255
          ParentFont = False
          TabOrder = 0
        end
        object Combo_Format: TComboBox
          Left = 101
          Top = 100
          Width = 380
          Height = 21
          Hint = 'Choose format used when saving the file'
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
      end
    end
    object Tab_Settings: TTabSheet
      HelpContext = 289
      Caption = 'Settings'
      object GroupBox3: TGroupBox
        Left = 5
        Top = 1
        Width = 505
        Height = 214
        TabOrder = 0
        object Label9: TLabel
          Left = 297
          Top = 44
          Width = 96
          Height = 13
          AutoSize = False
          Caption = '&Compress Level:'
          FocusControl = Combo_CompressLevel
        end
        object Label_IsReadOnly: TLabel
          Left = 280
          Top = 16
          Width = 211
          Height = 15
          Alignment = taCenter
          AutoSize = False
          Caption = 'File was opened as Read-Only.'
          Color = clRed
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object Label26: TLabel
          Left = 13
          Top = 82
          Width = 99
          Height = 13
          AutoSize = False
          Caption = 'Image storage'
          FocusControl = cbImgStorageMode
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          StyleElements = [seClient, seBorder]
        end
        object lblImgWarning: TLabel
          Left = 300
          Top = 80
          Width = 194
          Height = 26
          AutoSize = False
          FocusControl = cbImgStorageMode
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Visible = False
          WordWrap = True
          StyleElements = [seClient, seBorder]
        end
        object CB_AsReadOnly: TCheckBox
          Left = 9
          Top = 15
          Width = 254
          Height = 17
          Hint = 'In future, open this file as Read-only'
          Caption = 'Open this file in &Read-Only Mode'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CB_NoMultiBackup: TCheckBox
          Left = 9
          Top = 43
          Width = 282
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
        object Combo_CompressLevel: TComboBox
          Left = 399
          Top = 41
          Width = 92
          Height = 21
          Hint = 'Choose level of compression used when saving the file'
          Style = csDropDownList
          Enabled = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object cbImgStorageMode: TComboBox
          Left = 116
          Top = 81
          Width = 173
          Height = 21
          Style = csDropDownList
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnChange = cbImgStorageModeChange
        end
        object gbExternalStorage: TGroupBox
          Left = 15
          Top = 109
          Width = 478
          Height = 94
          Caption = '  External storage  '
          TabOrder = 4
          object Label22: TLabel
            Left = 13
            Top = 38
            Width = 50
            Height = 13
            AutoSize = False
            Caption = 'Type'
            FocusControl = cbImgExtStorageType
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object btnOpenDlgExternalPath: TToolbarButton97
            Left = 443
            Top = 57
            Width = 25
            Height = 21
            AllowAllUp = True
            GroupIndex = 3
            DropdownArrow = False
            Enabled = False
            Flat = False
            ImageIndex = 1
            Images = Form_Main.IMG_Toolbar
            RepeatInterval = 101
            OnClick = btnOpenDlgExternalPathClick
          end
          object Label10: TLabel
            Left = 103
            Top = 37
            Width = 159
            Height = 13
            AutoSize = False
            Caption = 'Location folder or file (zip)'
            FocusControl = Combo_CompressLevel
          end
          object cbImgExtStorageType: TComboBox
            Left = 13
            Top = 57
            Width = 75
            Height = 21
            Style = csDropDownList
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnChange = cbImgExtStorageTypeChange
          end
          object txtExtStorageLocation: TEdit
            Left = 101
            Top = 57
            Width = 336
            Height = 21
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnEnter = txtExtStorageLocationEnter
            OnExit = txtExtStorageLocationExit
            OnKeyDown = txtExtStorageLocationKeyDown
          end
          object rbImagesStRelocate: TRadioButton
            Left = 391
            Top = 19
            Width = 81
            Height = 17
            Caption = 'Relocated'
            TabOrder = 1
            OnClick = rbImagesStRelocateClick
          end
          object rbImagesStChange: TRadioButton
            Left = 300
            Top = 19
            Width = 85
            Height = 17
            Caption = 'Change'
            TabOrder = 0
            OnClick = rbImagesStChangeClick
          end
        end
      end
    end
    object Tab_Icons: TTabSheet
      HelpContext = 290
      Caption = 'File Icons'
      object GroupBox4: TGroupBox
        Left = 5
        Top = 0
        Width = 503
        Height = 213
        TabOrder = 0
        object Image_TrayIcon: TImage
          Left = 434
          Top = 149
          Width = 38
          Height = 38
          AutoSize = True
        end
        object TB_OpenDlgTrayIcon: TToolbarButton97
          Left = 384
          Top = 162
          Width = 29
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Enabled = False
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgTrayIconClick
        end
        object TB_OpenDlgTabImg: TToolbarButton97
          Left = 384
          Top = 78
          Width = 29
          Height = 21
          AllowAllUp = True
          GroupIndex = 3
          Enabled = False
          Flat = False
          ImageIndex = 1
          Images = Form_Main.IMG_Toolbar
          RepeatInterval = 101
          OnClick = TB_OpenDlgTabImgClick
        end
        object CB_ShowTabIcons: TCheckBox
          Left = 24
          Top = 24
          Width = 306
          Height = 17
          Hint = 'Display or hide tab images for this file'
          Caption = '&Show icon images on tabs'
          TabOrder = 0
        end
        object CB_TrayIcon: TCheckBox
          Left = 24
          Top = 134
          Width = 290
          Height = 17
          Caption = 'Use custom &Tray icon for this file'
          TabOrder = 1
        end
        object RB_TabImgDefault: TRadioButton
          Left = 24
          Top = 55
          Width = 110
          Height = 17
          Caption = '&Default'
          Checked = True
          TabOrder = 2
          TabStop = True
          OnClick = RB_TabImgOtherClick
        end
        object RB_TabImgBuiltIn: TRadioButton
          Left = 140
          Top = 55
          Width = 109
          Height = 17
          Caption = '&Built in'
          TabOrder = 3
          OnClick = RB_TabImgOtherClick
        end
        object RB_TabImgOther: TRadioButton
          Left = 256
          Top = 55
          Width = 98
          Height = 17
          Caption = '&Other'
          TabOrder = 4
          OnClick = RB_TabImgOtherClick
        end
        object Edit_TrayIcon: TEdit
          Left = 25
          Top = 162
          Width = 361
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
        object Edit_TabImg: TEdit
          Left = 25
          Top = 78
          Width = 361
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
      HelpContext = 291
      Caption = 'Security'
      object GroupBox2: TGroupBox
        Left = 5
        Top = 3
        Width = 506
        Height = 209
        TabOrder = 0
        object Label_Confirm: TLabel
          Left = 22
          Top = 99
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Confirm pass.:'
          FocusControl = Edit_Confirm
        end
        object Label_Pass: TLabel
          Left = 22
          Top = 69
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Passphrase:'
          FocusControl = Edit_Pass
        end
        object Label_Method: TLabel
          Left = 22
          Top = 164
          Width = 79
          Height = 13
          AutoSize = False
          Caption = '&Algorithm:'
          FocusControl = Combo_Method
        end
        object Label_EnterPass: TLabel
          Left = 212
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
        object Edit_Confirm: TEdit
          Left = 108
          Top = 94
          Width = 341
          Height = 21
          Hint = 'Carefully re-enter the access passphrase'
          MaxLength = 255
          PasswordChar = '*'
          TabOrder = 1
        end
        object Edit_Pass: TEdit
          Left = 108
          Top = 64
          Width = 341
          Height = 21
          Hint = 'Enter access passphrase for this file'
          MaxLength = 255
          PasswordChar = '*'
          TabOrder = 0
        end
        object Combo_Method: TComboBox
          Left = 108
          Top = 159
          Width = 341
          Height = 21
          Hint = 'Select which encryption algorithm to use'
          Style = csDropDownList
          TabOrder = 2
        end
        object Button_SetPass: TButton
          Left = 22
          Top = 15
          Width = 181
          Height = 25
          Hint = 'Click to change access passphrase for the encrypted file'
          Caption = '&Set Passphrase'
          TabOrder = 3
          OnClick = Button_SetPassClick
        end
        object CB_HidePass: TCheckBox
          Left = 108
          Top = 124
          Width = 268
          Height = 17
          Hint = 'Obscure passphrase with asterisks'
          Caption = '&Hide passphrase while typing'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
    end
  end
  object Button_System: TButton
    Left = 393
    Top = 264
    Width = 82
    Height = 25
    Hint = 'Displays system File Properties dialog box'
    Caption = 'S&ystem...'
    TabOrder = 4
    OnClick = Button_SystemClick
  end
  object Button_Help: TButton
    Left = 202
    Top = 264
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button_HelpClick
  end
  object FormPlacement: TFormPlacement
    IniSection = 'InfoDlg'
    Options = [fpPosition]
    Left = 285
    Top = 259
  end
end
