object Form_FavExt: TForm_FavExt
  Left = 270
  Top = 298
  BorderStyle = bsDialog
  Caption = 'Add Favorite document or program'
  ClientHeight = 184
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 13
    Top = 0
    Width = 331
    Height = 146
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 10
      Width = 158
      Height = 13
      Caption = '&Location (document or program):'
    end
    object Label2: TLabel
      Left = 10
      Top = 55
      Width = 108
      Height = 13
      Caption = '&Parameters (optional):'
    end
    object Label3: TLabel
      Left = 10
      Top = 100
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object TB_OpenDlg: TToolbarButton97
      Left = 295
      Top = 25
      Width = 25
      Height = 21
      AllowAllUp = True
      GroupIndex = 3
      Flat = False
      Glyph.Data = {00000000}
      GlyphMask.Data = {00000000}
      ImageIndex = 1
      Images = Form_Main.IMG_Toolbar
      RepeatInterval = 101
      OnClick = TB_OpenDlgClick
    end
    object Edit_FN: TEdit
      Left = 11
      Top = 25
      Width = 286
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = Edit_FNChange
    end
    object Edit_Params: TEdit
      Left = 10
      Top = 70
      Width = 311
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Edit_Name: TEdit
      Left = 10
      Top = 115
      Width = 311
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object Button_OK: TButton
    Left = 20
    Top = 154
    Width = 75
    Height = 25
    Hint = 'Accept changes and close dialog box'
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button_Cancel: TButton
    Left = 100
    Top = 154
    Width = 75
    Height = 25
    Hint = 'Discard changes and close dialog box'
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
