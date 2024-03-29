object AboutBox: TAboutBox
  Left = 302
  Top = 289
  HelpContext = 2
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'About - KeyNote NF'
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
      Width = 225
      Height = 18
      Caption = 'Tabbed notebook for Windows'
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
      Width = 285
      Height = 15
      Caption = 'Free software, Open Source (Mozilla Public License)'
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
      Width = 191
      Height = 13
      Cursor = crHandPoint
      Hint = 'Double-click to visit home page; Right-click to copy'
      Caption = 'https://github.com/dpradov/keynote-nf'
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
      Width = 162
      Height = 13
      Cursor = crHandPoint
      Hint = 
        'Double-click to send email; Right-click to copy'#13#10'(No HTML-format' +
        'ted email, PLEASE!)'
      Caption = 'marekjed@users.sourceforge.net'
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
      Width = 307
      Height = 26
      Caption = 
        'Keynote was inspired by a fantastic freeware prog: DaRT Notes'#13#10'b' +
        'y Andre v.d. Merwe (See "dart.txt" for information)'
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
      Picture.Data = {
        07544269746D6170A6180000424DA61800000000000036000000280000002D00
        00002E0000000100180000000000701800000000000000000000000000000000
        0000FBFBFAFBFBFAFBFBFAFBFAFAFBFAFAFBFAFAFCFAFAFCFAFBFAF9F9F9F7F7
        F9F8F7F9F8F8FCFDF9FFFFFAFCFEF8F9FBF7F9F9F8F9F7F9FCFAFBFFFDFCFBFA
        FAF6F8F7F9FCFAFCFFFDFAFAFBF9F5F8FBF8FAFEFAFCFFFEFEFFFFFFFFFEFEFC
        F9FCFBF8FAF9F8F9FAF9F9FBFCFAFBFBFAFBFBFAFBFBFAFBFAFBFBFBFAFBFBFA
        FBFBFAFBFBFAFBFBFA00FCFBFAFCFBFAFCFAFAFCFAFAFCFBFAFCFBFBFCFBFBFC
        FBFBF8F8F7F5F4F4F6F5F4F6F5F4FDFEF7FFFFFAFEFFF7F9FCF4F8F8F7F8F5FA
        FDFAFDFFFFFFF9FBFBF0F6F5F7FDFCFDFFFFFAFAFDF8F0F8FDF5FCFFF9FFFFFF
        FFFFFFFFFFFFFFFCF7FDFAF5FBF8F4F8FAF7F9FAFCF9FAFBF9FAFBFAFAFAFAFA
        F9FBFBFAFBFBFAFBFBFBFAFBFBFAFBFBFA00FCFBFAFCFAFAFDFAFAFDFAFAFCFB
        FAFCFBFBFBFCFBFBFCFCF8F9F8F5F5F4F8F6F7FAF7F9FCFBF9FEFFF9FBFDF8F9
        FAF7F8FBFDF7FBFFF6FFFFF5FFFCF6FAF8FFFEFFFFF5FFFFF7FCF9F9F8F0FCF4
        F2FAF4F4F9F5FCFFFAFFFFFFFFFFFDFDF9FBFBF8F8FAF8F6FBF9F9FAFDF9FAFC
        F9FAFBFAFAFAFAFAF8FBFAF9FBFBFAFBFBFBFAFBFBFAFBFBFA00FDFAFAFDFAFA
        FEFAFAFEFAFAFDFBFAFCFBFBFBFCFCFAFDFDF8FAF9F6F6F5FAF7F9FDF8FEFBF8
        FBF9F8F8F9F8F9F8F9FBC7C2E9968BD76F64C5483DB226209F2523AD2919C42E
        26AF4B40BC695ACA978EE0C5C2F6DFE2F9F9FFFCFCFEFAFEFBF9FDFBF6FCFBF3
        FDFBF8F9FDF8FAFCF9FAFAFAFAF9FBFAF8FBFAF8FBFAF9FBFBFAFBFBFBFAFBFB
        FA00FDFBFAFDFBFAFDFBFAFEFAFBFAF9F8F7F7F5F7F9F5F7FBF6F9FDFAFBFEFF
        FFFFFBFFFFF8F2F2FFE0E4F58C82C54931A53A209A2C108E22127B1914681207
        501C0A49230E7C1B19721510711F177F271A9A301DB54F3CBE7D6CD7DCDEFFDC
        F1DEFAFFF9F7FEF4F9FAF8F7FCF9FBFCFAFEFBFBFCFAFBF9F9FAFAF9FAFAFAFA
        FBFBFAFBFBFAFBFBFA00FCFBFAFDFBFBFDFBFBFEFBFCF7F6F5F1F2EEF3F5EFF5
        F9F0FAFFFCFFFFFFF5F8FFEBE9FE8976D5271A7F10086B1A17771E1177210C78
        160C5B0A0C3D120D5E1A0E7F211181080B36000046141575181D881C269A0E0A
        90200EA62D23A27A79DED6DDFFF2FFF5F4FAF7F5FAF9FBFBFBFFFCFDFDFBFBF9
        F9F9F9FAF9FAFAF9FAFBFAFBFBFAFBFBFA00FCFBFAFDFBFBFDFBFCFEFBFDF9F8
        F6F4F5EFF7F8F5FAFBFBFFFFEFEBFAF0AFABFF2324A9170F8E0B065B12087229
        199A1E1176130A530C0C4E050E491F16812A1991180B650608220F0A49180C71
        1E0F8E241D951C10862312861201873021B73F37A1AEADEAFFFDFEF3F9FAFBF9
        FDFFFAFFFEFAFBF8FAF7F9FBF8F9FBF9FAFBF9FBFBFAFBFBFA00FCFBFBFCFBFC
        FDFBFDFDFCFEFAFAF7F6F8F1FAFAFBFFFCFFEAF4EE967EF125059C15184E221A
        792F1CA524178A1812700E0D4F04082E1210682019A22C1FA3191C5700012203
        060D4040565C5B7F4438882D2B64292165261667160B75271FA4291CA22B199F
        8073D9F0F7FAFBF7FEFFF8FFFFF9FCF7FBF6F8FBF7F9FBF8FAFBF9FBFBFAFBFB
        FA00FBFAFBFBF9FCFBF9FBFCFAF9FAFAFBF9FAFDFFFFFDE0E8E57F54FF1F107D
        081739211495221290230F8A100C4F0C0D3A11145A171B7A261C95361DB0240F
        85110E430F13236D6D79CBC5E5BAB8CAC1BDD7C9CECDC5CBCDD2D9DE8487C025
        1AA82518A225159C221EAD6E55E4E2FBDBF7FCF9F8FCF8F9FDF6F9FCF7F9FBF7
        F9FBF8FAFBF9FAFBFA00FAFAFBFAF7FCFAF7F9FAF8F5FBFAFFFBFDFFEEEDFF80
        66DE24148309183B1B148C2D27B02214901702700C053920115025208B2A2EC6
        2B239B2C17700B00400A002F202623D7D5E4B7AAD3D8D5D5CEC7CCC5B9C2B1AF
        B2BEC4C19198C12415AD2213A31F11992119952B21B4695ADFE7FFF1F2FFF4FC
        FEF7FAFCF6F9FAF6F9FBF7FAFBF8FAFBF900FBFBFDFCF9FFFCF9FCFCF8F7FFFF
        FCFCFFFF887AD0250E93202E492B1A9627159B231B890C0C4F05033C1A0F742F
        1CAC281CA1221C97170E6C0C004118113E04031A979A9FBAB6CADBD4EDFCFCF9
        FFFCFCFFFCFFFFFCFFACADBF5356B42716A31E0E9124158F221992150E963725
        BD7972D7FFFFFFFCFDF5FAFCF5F8FAF4F8FAF6F9FBF7F9FBF900FCFDFFFFFBFF
        FFFAFFFFF9F9EDF1F0BCB2F32308930A0C5A2C14A62D14A4231183180F620509
        35120D55281AAE1F1EBB1C14911A096713094D0D083300000C7F8685DFDFEABE
        B8CFFFFDFFFFFFFDFFFFFDFFFEFDBDBACDBBB5DC2625B82918991A097F2A1A85
        2318901E1B99251BA52B1BB0A39CE2FCFDF4FAFBF3F8FAF3F8FAF5F8FAF7F8FA
        F800FBFDF9FEFDFAFBFCF9F8FBF7E1D9FF39289D101F48271CA52D19A9231287
        0E0B4D09093A1816752622AF281FB41917921C14751E135900002307081D7474
        88E1DFF2A09EACFFFEFFFFFFFFFEFFF6FFFFFFF6F4F9BAB8D0AEA0ED2819A61F
        0F7C29217C24168321188D1C19962A1CB0291BA43C2C99E0D9FFFFFFFFFFFDFF
        FCFBFEF9FAF7F9FAF800FBFEF4FDFFEFF7FEF2F2FDF5847DCE16157A1D11A424
        23A21F1A851A11690A0A3E1A0D5F2A22B41A2FBC171F93130F690B054A03002A
        121229818887E2E0F4A399C0F2EEFEFFFFFCFDFFF5FAFFEEFFFFFFECEAF4A7A6
        C3816BDE1B008514075F3938791E13801F178A191892301EBB261A98351CB084
        75DCE5F1ECFFFFFFFFFDFFF9FAF7F9FAF800FCFBF8FEFBF6FFFFFFE0E0FF251C
        811914902314A71D1C820C124F0C0E421B09872B0FB73223B20A077D0200520A
        0637000012585C5EBABDC4DDDEEAA09FAEF4F1FFFFFFFFFEFFFAFEFFFBFFFFFD
        F9FDF9C4BCDDD4D4F23425841E16527776909FA0D3170E7C20168E201BA42A1B
        AE2417911E10903829AFE4E3FFFFFFFFFDFDFFFAFAF7FAFAF800FDF8FCFEF6FE
        F6FFF38E84E2251B941D13A62918AA1614620A0F3E1F14683D22CB3B3FB47B86
        AC7A76917D7A8BA09DA5C6C1D1CCC6DEB2ADC6B8B4CDFFFFFFFFFFFFFFFFFDFB
        FBF8FFFFFFFFFFFFFFFFFFBCAEE5A1A2C107004AA1A2B5BBAECE756CB30F0978
        201591271EB62418A022138A2724902C1EC38375E4FBFFF9FBFEF8FBF9F8FBFA
        F800FEFAFFFFFAFFD7DDF24D3EBF281C98221A91151269090A411B16822E22C3
        2D17BC6C6FB2A7ADC2B3B0B8A9A7ADB0ADB2C4C1C9C8C5D0DAD9E3FCFCFFFFFF
        FFFFFFFAFFFFFCFFFFFEFEFFFFFEFFFFD6DCDCBFBBDB55507E2A2561A5A7C1C0
        BED74E469B1D198A2A1DB1221A9F2016901D13811D188A1C12A94D39C1DEE3F6
        F6FDF5FEFBFAFCFBF900FFFDFFFFFFFFC8C7FF2D19BB3A2DAC26207D11114F1C
        0B6D2C1DC61D27D12C27A7DDDFEFF4F4F7EBEADFF6F4F0FFFEFFFFFFFFFFFFFF
        FFFFFFFFFFFDFDFEF7FCF8F2FFFEFCFFFFFFEDF2F0D8E0DB9DA4A6C2C9D0180E
        4B6E6A98A9ACCCA5AEBF1810742B299C2320AB1D17871B147F181277231C932D
        27B0270EAEC1C3F2F1FCF1FFFDFCFEFCF900FAFDFEFAFEFFA09CED271AB8160C
        80150E58281A6F3B268727227A232293372D8AFBFFFFFFFFFFF7F7F1FCFAF8FF
        FEFEFFFFFEFFFFFFFFFFFFFAFDFBF2F0EDEBE2E0D2CADBAAA2C6B1AFC7B9BDC7
        DFE1F755567802002BBFC0D8BAC8D07678B62C17A6321DB92413802116831D15
        7E1A1478281CA3261FA72513A5938FE7F1FEF7FFFBFBFEFCF800F5FDF7F1FDF3
        7971DB211CB6120C74433C739F8FD9DAD9F3FFFFF76A5D9551437EF9FFFAFEFF
        FFFFFFFFFFFFFFFFFEFCFFFFFCFFFFFBEAEAEAB5B6B9B7B1B4BAACAFD6D1D5D2
        D6DBA5A8B7797B93211F4909023F5C5C7BD0D7D8ACC4B54743AC301AB1190789
        180F7B24157F1F167C1B177A2D1BB21E179E22199D665BDCF2FFFEFEF9F9FDFB
        F600F7FDF4F5FDF05450D0201BA6191477827CB7BCB4EBF6F7FFFFFFFF635689
        483772FDFFFEFFFFFAFFFFF7FFFFFBFFFFFFFDFCFCE8E8E9B1B6ADCBD4C1A89E
        D9A693FB604F961A0B300B031C0000080A072138355BC4C5DA9FA4A9C0C3F032
        1BB4261A8F1B196A1C17721E157D1D167D2B1BA42B1AAE1C14931E1C905048D3
        FBFFF7FAF8FAFBFAF800F9FCF3F8FDEC302FC5201B97100C6AA19DDCBAB9DDF3
        F5FFF7F2FF5C4F7C3E2B66FFFFFFFFFDF6FFFCEAFFFFF6FFFFFFFBF9FCD2D1D6
        B9B6C6605B76190B7F13119B1B145A43383970686B9D999DB2B0B9C8C7D69B9D
        A8EDF2FA958ED51D0A8F1D1A6D1D14781F138019157B1A167E3B20CE2A18AB19
        11881A1F833A36CBFFFFF0F5F7FBF9FAFA00FAFBF7FAFAF7302CC71F17901710
        9BABA7E3B0AFD0F5F7FDF1F1FF6D5F962E1B97ECEBFFFFFFFFFFFEF8FFFFFCFF
        FFFFFCFBFDE5E7E89C9BABB3AECE9E9DC2A9B8BFABB2B4BDBBB8B5B3B2ACAAAB
        B1AEB3B5B2BBFDFEFFF4F9FF584E990C006B150F651D14761F147D16167F2319
        992F1CB426189D1D13851D1C932E2AC6FEFEEDF6F9FBFAFAFC00FBF9FCFCF7FF
        3128C81F128A1E15CDB5B2EAA6A6C3F8FAFCFBFFFF9F8FD11F0BC8B7B7EBFEFF
        FFFFFFFFFFFFFFFFFDFFFCFCFDF9FCFAD0CFE0A7A2C5B3A9C1BFB1BDCBC8CAD7
        DFD8E9EDE9FBFCFAFFFCFDFFFDFFFFFFFFDBE1E61B0E5D1B13681C146E1E1474
        1F157B1316822B1CB523189B22178F2015832119A2221DC1F9F9EBF7FBFBFBFB
        FD00FDF9FDFFF8FF3F39D52618A82819C5A5A2E8B5BACCE4E6E6FCFFFFC4C3DF
        301DB7685ACFDBE1FEFEFFFFFEFFFFFEFCFEFFFFFEFFFFFFFFFFFEFFFFFCFFFF
        FBFFF6FBFEFAFEFCFFFFFBFEFDFBFCF9FEFDFEFFFEFFFCFFFF9797B6180E5C19
        106C19137519157E1B1585311CC3281BA61E198922198F251895231AA3312BC0
        FFFBF6F8FCFCFBFDFD00FFF9FEFFF9FF4D4AE22D1DC7311CBD9592E6C3CED5D1
        D2D0FDFFFEEAF6EC413990191386B8C7E4F7FFFFFAFFFFFCFDFCFFFFFFFFFFFF
        FFFFFFF9F9FFFDFAFCFFFBF9FFFDFFFFFFFFFEFEFFFAFDF9FDFEFFFFFEFFFAFF
        FF534D86150D5B170E7016127B1516872719B5301AB7141570191977221B8F2A
        1CA7251BA34039BFFFFDFFF9FEFEFCFEFE00FDF7FAFFF7FB7772EC2B1ED12C20
        B85E5CB9E2E0E9BAB7CCFCFFF9FEFCFD8F8AB410095A212B40D2E2DDFFFFFFFF
        FFFFFFFFFFFFFFFEFEFFFFFCFBFFFEFCFEFFFEFCFFFEFFFFFFFFFEFFFFFCFEFE
        FFFFFFFEFEFF9494B11A0E5C09005517126E2319991F1B9E281AAA2414931814
        751C197D271C9F211A9A21149A7169D4FDF9FFFCFDFDFBFCFB00FBF6F7FAF4F3
        A19AF7281FDB2723B326268CFFF2FDA39BC7FBFAF4FFFFFFFCFBF8473E6F0000
        234D4A78E7E6FFFFFFFFFDFDFFF8F8F8FCFAFCFFFCFFFFFEFFFFFFFFFFFFFCFF
        FFF9FEFFFEFEFFFFEDEEFA9D9DB11E16522010731C136F18166C311FB72A1FB5
        1A167A190E6F1C147A1F19842C1DAF19188E1D0D92A298E9F0F5FCFFFBFCFBFB
        F800FBF7FCFBF6FECABDFA2D23D21F19AA211F92C5B5FACDCFE2CAC6D6FFFDFF
        FFFFFFC7CFC5191C5E1B0FBD443BB7AEB2DAFFFFFFF4F4FFFFFFFFFEFDFDFFFE
        FBFFFFF9FFFFFFF9FBFAEFF4F1E5EEE97D7AA015065801004D1D11722F18A532
        1BB12A1BA312166E1915751A137220168A2719A323199B1E19923124B8D0C8F4
        F7F8FCFEFDFCFBFBF900FBF9FFFCF7FFFFF0FF5146E9261FB01B18978977F7F6
        FFFE9A92B8FEF7FFFFFFFFFFFFFFC4C6DC412D9C2E12B31B0C9D555A8EB1B0CD
        E6E7F3FCFEF9FFFFF6FFFFF4FBFAF8D4D6DC807E9C2D265C0000370D0753241D
        721B12713319B42C17AA23178F1A17741814701A187525189B3019C11A158624
        1997453CDEFEF9FEFEFCFDFDFFFCFAFCFA00FBFBF9FCFAF8FFFBFC968FF22F29
        C93324C22F26C2D7E0F8D7D1D7BABCCDEFEEF7FFFFFFFFFFFDDFE5EE534C8A17
        106000002A04002A12132C3032547177D14E4DA0242A4B0B0C1B08061F151133
        7875956A68871216641A17B81B14A01C1C721D19711F16711D167A2319962219
        9B22199F1D1992291DAA8F8CF0FFFCF9FDFBFAF8FBFBF8FAFA00FBFDF1FCFCE7
        FCFFEBDCD9FB2723D12C10CD251AF37766DFFFF5FC96A1B3EBEDF8FFF9FCFFFF
        F4FEFFEDFFFFFFB4B3C355565E171A1A000024240E9C00009413003300000122
        213B706F83DEDDEBFFFFFFFFFFFCADB0BA331F850F05660C0B6717106A23166D
        2217832B1AB720199A14187E211D9E2D21BED9DCFFFFFFF3FCFBF6F3F7FAF5F8
        FB00FBFBFAFAFAFBFBFAFAFBFBFA8B82EC2B25D63023E2261CC7BEB6ECFAFEFD
        969CBEEEEBF2FFFEFFFFFFFFFFFFFEFDFDFDFEFFFFDFE2E5BEBED0ADAACA9A99
        ABA7A9ABCFCECFF6F3F2FFFFFFFFFFFFFFFFFFFDFDFFFEFEFBDFDFD75753770F
        0756160E701C168B2A1FC32419981B177F23198E321AB8807DE0F9FAF9F5FAF6
        F9FBF9FDFCFCFDFBFC00FFFFFBFFFFFCFBFFFAF3FCF8F6EBFD4037DC3620EA2C
        21CC4035B7D5E0F6E9F7F29C96BCEDE9FAFEFCF8FDFEFDFDFFFFFAFFFCF7FEF6
        FAFFFCFDFFFFFAFFFAF8FDF3FDFFF9FFFFFFFFFFFFFFFFFFFEFEFFF8F8FBFDFD
        F5FFFFEFFFFFFF958FA50C0567241ACA23189C131873221A8F311CAB472CBCDC
        EAF4EBF0FAFAF6FFFBFAFAFDFEF5FDFDF600FFFCFBFFFFFCFCFDFBF7FAFAFEFC
        FFA9ACF62C1CBD2E23D6170BAB5143CFEBE9FFF6FFFF8F988BC7C6CDFFFFFFEF
        F6F7FDFFFFFBFFFEFCFFFFFEFFFFFDFFFCFCFFFAFDFEFCFFFEFEFFFFFFFFFFFF
        FEFEFFFCFCFFFFFFFFFFFFFDE3DFE6B3ADBE9C9FCB25258D18147D1B1B921C08
        A13C20B9C5B5FFEEF6F7F5F7F9FBF9FBFCFAFAFCFBF9FCFBF900FEF8FCFDF9FC
        FCF9FCFCF9FCF7FDF6F3FFF19288FF3125E11E1CB82C1CDC5E4BDDCFD1F2FFFF
        FFB1AFC1999DA6E1ECEBFFFFFFFFFFFFFFFFFFFEFEFCFFFFFEFFFFFFFEFEFEFB
        FCFDFAFBFDFAFAFDFDFDFFFFFFFFF1F0F8C2BFCBA9A5B1F1EBF8BCBDD427197E
        1D1485241FB13522BC867CDBE3EAF4FFFFFAFEFFF9FCFCF7FCFAFAFCF8FDFCF9
        FC00FDF9FBFCF9FBFCFAFBFBFAFBFAFAFDF9FBFFF5FFEE7059F9371DD72E1CCE
        3117DA452EE0C7B9FFFFFFFFEADFF1BAB5D39C9BACCDD1D6E8E9EBFFFFFFFFFA
        FFFFF3FFFFFDFFFFFFFFFDFFFFF9FBFFDCDFE3C0C2C09A9F9AC4CDC5FFFFFF94
        8BC52F29731A0C872B1BB1230BB08079D3DDE7F6FEFFFCFFFFF9FCFDF7FAFAF6
        FBF9F9FCF8FCFCF9FC00FCFAFBFCFAFBFBFAFBFBFAFBFDF7FFFFF4FFF7F7FFEF
        FAF86F54E9301DC1352ADA1A18D3312BC5685FD6DDDAFFF3F6EEF7FAF9BABEC5
        B1B1B5A8A4A4B3A5BCBDA6D4B2ABBDA7B0A6A0A7AB989DB0BCC0C6E0E3DCF2F3
        FFC5C4EB4E478F170B722215912E1FB0291DB6816EE2DAE0FAF3FBFFF8FCFBFD
        FEF7FBFAF6F8F8F5FAF8F9FBF9FCFBFAFB00FBFBFBFBFBFBFBFBFAFAFBFAFCFA
        FFFDF8FFF8F9FEF4FAF8F2FFEC9182F02F21C12D20F22D19E03D22DE3A25C277
        68E6CBBEFFFEF4FFF3F7F1F7FFFFFEFFFFF4EFF4F2F6EFEFFEEAFFFFF8FFFFFF
        EEE1FFB9B7EF6B63B30D006714097C2A21A12510A63F29B59085E4F0EFFFFFFF
        FFF7FAFBFAFBFAFDFCFAFCFBF9FAF9F9FBFAFAFBFAFBFBFAFB00FBFCFAFAFCFA
        FAFBFAFAFBFAFAFCFAFBFDFBFAFCFAF8FBF8F5F1FFF3FEF2AAAFFB413FE5392D
        DF311CD93627E51C12D2231CC62A27BB4851A18683D4A9A2DAABA1C1A19BBE97
        95BA7671B7544EB54331C2130A8204006C1513762A25A31E18B14736C6B0AAEE
        F7F9FCFEFAFBFDFAF9FCFAF7FDFAFAFEFBFDFDFBFDFCFBFCFCFBFBFBFBFBFBFB
        FB00FBFCFAFAFBFAFAFBFAFAFBFAFAFBFAFBFCFBFBFCFAFBFCF9FEF7FFFFFEFF
        FFF7FFEDE1F48B86E12A2CCE3632DE3328DE2118C62018BD1C0FB7290BD72816
        9F1610560D08501405711C0E8624189B2219900F165E1718902E2AD22C27AC8A
        83E6EDE8FFF0F7FAF9F9F8FFFDFBFEFBF9FCFAF8FDFAFAFEFBFDFDFBFCFCFBFC
        FCFBFBFBFBFAFBFBFB00FBFBFAFAFBFAFAFAFAFAFAFAFAFBFAFBFCFAFCFDFAFD
        FEFBFFFEFFFFFDFFFFFFFFFFFFFDEDEFF3DBDFE9938FE94A3FEA3025D5352ADF
        3125E02D20E12720B622208B1916833114C823179F1519752016942C14B3664C
        D4A085F5DBD5FFF6FFEEF3F9F1F0EEF3FBF9F3FFFFFCFFFDFAFCFAF8FDFBFBFE
        FCFDFDFCFCFCFCFBFCFCFBFBFCFAFBFBFB00FBFBFAFAFBFAFAFAFAFAF9FAFAFB
        FAFBFCFAFCFDFAFDFEFAFFFDFEFFFDFFFFFDFEFCFDFAFDFDFFFDFCFFFDF5FFFD
        EDFFC4B8EF9A93EC726FEB4A4CEA3F39E43526DD2F2CC33A36CE594DD37964D8
        A79BECD5D2FFE8EBFCFCFFF7FFFFFEF8FFF4F8FBF7F8F7FAFCFAF7FEFCF9FDFB
        F9FCFAF9FDFBFBFEFDFDFDFCFCFCFCFBFCFCFAFBFCF9FBFBFA00FBFBFAFAFAFA
        FAFAFAFAF9FBFBFAFAFCFBFAFDFDF9FEFEF9FDFDF8FBFCF8FAFBF7F9FBF7FCFA
        FBFFFAFFFCF9F9F8F9F4FBFAF6FEFCF8FFFBF9FFFBF9FFFBFBFFFCFCFEFBFCFC
        FAFBFCFBFEFDFDFFFDFBFEFEF9FAFBF8F7F8F7F3F9F9F7FAFAFAFDFDFDFFFFFF
        FDFBFBFAF8F7FBF9F8FCFAFAFDFCFBFEFDFDFDFDFCFDFDFBFCFCFAFBFCF9FCFB
        FA00FBFBFAFAFAFAFAFAFAFAF9FBFBFAFAFCFBFAFCFCFAFDFDF9FCFCF9FBFCF9
        FAFBF8FAFBF8FCF9FCFEF8FFFCF8FCFBF8F8FCFAF8FEFBF9FEFCF9FFFDF8FEFD
        FAFDFCFCFBFCFAFAFCF8FAFBFBFBFBFFFCFBFCFDFAF9FBFAF6FAFAF4FAFAF7FA
        FAFBFDFCFDFFFEFFFDFAFBFAF8F8FBF9F9FCFBFAFCFBFBFDFDFDFDFCFCFCFCFB
        FCFCFAFBFCF9FCFBFB00FBFAFAFBFAFAFBFAFAFAFAFBFBFAFAFCFBFAFCFCFAFD
        FCFAFCFCFAFCFCFAFBFBF9FBFBF9FBF9FDFCF6FFFDF7FEFEF7FCFEF9FBFDFBFA
        FDFDF8FDFFF7FBFEFAFAFCFCF9FDF9F8FEF5F9FCF9FAF9FCFBFAFAFCFBF8FCFC
        F6FCFEF4FBFCF8FBFBFBFDFCFDFFFDFEFDFBFBFAF9F9FBFAFAFBFBFBFCFBFBFD
        FCFCFCFCFCFCFCFBFBFBFBFBFBFAFBFBFB00}
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
      Width = 180
      Height = 13
      Caption = 'Copyright '#169' 2000-05  Marek Jedlinski'
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
      Width = 278
      Height = 13
      Caption = 'Copyright '#169' 2007-15  Daniel Prado Velasco   (since 1.7.0)'
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
      Width = 134
      Height = 13
      Cursor = crHandPoint
      Hint = 
        'Double-click to send email; Right-click to copy'#13#10'(No HTML-format' +
        'ted email, PLEASE!)'
      Caption = 'dprado.keynote@gmail.com'
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
      Width = 83
      Height = 14
      Caption = 'v1.8.0 Beta 1'
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
      Width = 64
      Height = 13
      Caption = '(01/07/2023)'
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
      Width = 241
      Height = 13
      Caption = 'KeyNote NF is an evolution of KeyNote (by Marek)'
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
