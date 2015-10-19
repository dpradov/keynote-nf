object Form_Mail: TForm_Mail
  Left = 394
  Top = 262
  HelpContext = 510
  BorderStyle = bsDialog
  Caption = 'Send note via E-mail'
  ClientHeight = 316
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label_Status: TTntLabel
    Left = 18
    Top = 261
    Width = 48
    Height = 16
    Caption = 'Ready.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button_OK: TTntButton
    Left = 13
    Top = 281
    Width = 108
    Height = 25
    Hint = 'Accept settings and send note'
    Caption = '&E-mail note'
    Default = True
    TabOrder = 0
    OnClick = Button_OKClick
  end
  object Button_Cancel: TTntButton
    Left = 130
    Top = 281
    Width = 108
    Height = 25
    Hint = 'Close dialog box without sending email'
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button_CancelClick
  end
  object Pages: TPageControl
    Left = 5
    Top = 5
    Width = 496
    Height = 250
    ActivePage = Tab_Send
    TabOrder = 2
    object Tab_Send: TTabSheet
      Caption = 'Message settings'
      object GroupBox_Source: TTntGroupBox
        Left = 5
        Top = 0
        Width = 252
        Height = 109
        Caption = ' What to send? '
        TabOrder = 0
        object RB_Current: TTntRadioButton
          Left = 15
          Top = 20
          Width = 228
          Height = 17
          Hint = 'Email only currently active note'
          Caption = '&Current note'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_All: TTntRadioButton
          Left = 15
          Top = 40
          Width = 228
          Height = 17
          Hint = 'Email ALL notes in currently open file'
          Caption = '&All notes'
          TabOrder = 1
        end
        object RB_File: TTntRadioButton
          Left = 15
          Top = 60
          Width = 228
          Height = 17
          Hint = 'Email the whole KeyNote file'
          Caption = '&Whole file (recipient must have KeyNote)'
          TabOrder = 2
        end
        object CheckBox_ExcludeHiddenNodes: TTntCheckBox
          Left = 15
          Top = 83
          Width = 228
          Height = 17
          Hint = 'Don'#39't export nodes hidden'
          Caption = 'Exclude &hidden nodes'
          TabOrder = 3
        end
      end
      object GroupBox1: TTntGroupBox
        Left = 264
        Top = 0
        Width = 217
        Height = 109
        Caption = ' Format '
        TabOrder = 1
        object Label7: TTntLabel
          Left = 10
          Top = 70
          Width = 63
          Height = 13
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'C&odepage:'
          FocusControl = Combo_Charset
        end
        object RB_PlainText: TTntRadioButton
          Left = 10
          Top = 20
          Width = 191
          Height = 17
          Hint = 'Send note text without RTF formatting'
          Caption = 'As &plain text (inline)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RB_RTF: TTntRadioButton
          Left = 10
          Top = 40
          Width = 191
          Height = 17
          Hint = 'Send formatted text as an RTF file'
          Caption = 'As &RTF (attachment)'
          TabOrder = 1
        end
        object Combo_Charset: TTntComboBox
          Left = 78
          Top = 65
          Width = 126
          Height = 21
          Hint = 'Codepage (KeyNote performs NO conversion)'
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'ISO-8859-1'
            'ISO-8859-2'
            'ISO-8859-5'
            'ISO-8859-9'
            'Windows-1250'
            'Windows-1251'
            'Windows-1253')
        end
      end
      object GroupBox2: TTntGroupBox
        Left = 3
        Top = 115
        Width = 476
        Height = 101
        Caption = ' Message information '
        TabOrder = 2
        object Label1: TTntLabel
          Left = 15
          Top = 25
          Width = 53
          Height = 13
          AutoSize = False
          Caption = '&To:'
          FocusControl = Combo_TO
        end
        object Label2: TTntLabel
          Left = 15
          Top = 75
          Width = 53
          Height = 13
          AutoSize = False
          Caption = '&Subject:'
          FocusControl = Edit_Subject
        end
        object Label3: TTntLabel
          Left = 15
          Top = 50
          Width = 53
          Height = 13
          AutoSize = False
          Caption = '&Cc:'
          FocusControl = Combo_CC
        end
        object Combo_TO: TTntComboBox
          Left = 72
          Top = 20
          Width = 389
          Height = 21
          Hint = 'Enter recipient'#39's address, or select from list'
          ItemHeight = 13
          TabOrder = 0
        end
        object Edit_Subject: TTntEdit
          Left = 72
          Top = 70
          Width = 389
          Height = 21
          Hint = 'Enter the Subject line for the mail message'
          TabOrder = 2
        end
        object Combo_CC: TTntComboBox
          Left = 72
          Top = 45
          Width = 389
          Height = 21
          Hint = 'Enter additional address(es), or select from list'
          ItemHeight = 13
          TabOrder = 1
        end
      end
    end
    object Tab_SMTP: TTabSheet
      Caption = 'SMTP server settings'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TTntGroupBox
        Left = 5
        Top = 0
        Width = 476
        Height = 196
        TabOrder = 0
        object Label4: TTntLabel
          Left = 15
          Top = 20
          Width = 65
          Height = 13
          Caption = '&SMTP server:'
          FocusControl = Edit_SMTPServer
        end
        object Label5: TTntLabel
          Left = 355
          Top = 20
          Width = 22
          Height = 13
          Caption = '&Port:'
          FocusControl = Edit_Port
        end
        object Label6: TTntLabel
          Left = 15
          Top = 70
          Width = 122
          Height = 13
          Caption = '&From (your email address):'
          FocusControl = Edit_From
        end
        object Label8: TTntLabel
          Left = 15
          Top = 115
          Width = 124
          Height = 13
          Caption = '&Text to place before notes'
          FocusControl = Edit_FirstLine
        end
        object Edit_SMTPServer: TTntEdit
          Left = 15
          Top = 35
          Width = 306
          Height = 21
          Hint = 'Name of email server used for dispatching messages'
          MaxLength = 127
          TabOrder = 0
        end
        object Edit_Port: TTntEdit
          Left = 355
          Top = 35
          Width = 51
          Height = 21
          Hint = 'Mail server port number (or type "smtp")'
          MaxLength = 5
          TabOrder = 1
        end
        object Edit_From: TTntEdit
          Left = 15
          Top = 85
          Width = 306
          Height = 21
          Hint = 'Your email address (The "From" field)'
          MaxLength = 127
          TabOrder = 2
        end
        object CheckBox_Log: TTntCheckBox
          Left = 15
          Top = 165
          Width = 306
          Height = 17
          Hint = 'Keep a log of communication with SMTP server'
          Caption = '&Log exchange with SMTP server'
          TabOrder = 4
        end
        object Edit_FirstLine: TTntEdit
          Left = 15
          Top = 130
          Width = 306
          Height = 21
          Hint = 'Optional text to be placed in the message before notes'
          MaxLength = 127
          TabOrder = 3
        end
      end
    end
  end
  object Button_Help: TTntButton
    Left = 391
    Top = 281
    Width = 108
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = Button_HelpClick
  end
  object SmtpCli: TSmtpCli
    Tag = 0
    LocalAddr = '0.0.0.0'
    Port = 'smtp'
    AuthType = smtpAuthNone
    CharSet = 'iso-8859-1'
    ContentType = smtpPlainText
    OwnHeaders = False
    OnDisplay = SmtpCliDisplay
    OnHeaderLine = SmtpCliHeaderLine
    OnRequestDone = SmtpCliRequestDone
    Left = 437
    Top = 260
  end
  object GFLog: TGFLog
    Active = True
    MaxLines = 127
    DateStamp = False
    TimeStamp = True
    UniqueFileName = False
    AppendToFile = True
    ShowErrors = True
    DeactivateOnError = False
    Separator = '----- LOG SESSION ENDS -----'
    Left = 398
    Top = 261
  end
end
