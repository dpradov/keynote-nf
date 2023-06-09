{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{         Copyright (c) 2008 Jaro Benes                 }
{                                                       }
{ Recreated from lang. resources by JB.                 }
{*******************************************************}
unit RxResConst_tur;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Araştır';
  SDefaultFilter = 'Bütün dosyalar (*.*)|*.*';
  SDateDlgTitle = 'Bir tarih seç';
  SNextYear = 'Gelecek Yıl|';
  SNextMonth = 'Gelecel Ay|'; 
  SPrevYear = 'Önceki Yıl|';
  SPrevMonth = 'Önceki Ay|';
  SNotImplemented = 'Fonsiyon daha tamamlanmadı';
  SFileNotExec = 'Belirtilen dosya ne koşabilir, ne dinamik bağlanır ne de bir ikon dosyası';
  SLoadLibError = '%s kütüphanesi yüklenemedi';
  SDetails = 'Detaylar';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Bu operasyon lokal bir veri tabanı üzerinde yapılamaz';
  SRetryLogin = 'Veri tabanına bağlanmayı yeniden denemek ister misiniz?';
  SExprNotBoolean = 'Alan ''%s'' mantıksal veri tipinde değil';
  SExprBadNullTest = '''NULL'' sadece ''='' ve ''<>'' ile kullanılabilir';
  SExprBadField = 'Alan ''%s'' filtreleme ifadesinde kullanılamaz';
  SCaptureFilter = 'Bu operasyon kontroller yakalandığında yapılamaz';
  SNotCaptureFilter = 'Bu operasyon kontroller yakalanmadığında yapılamaz';
  SInactiveData = 'Closed';
  SBrowseData = 'Browse';
  SEditData = 'Edit';
  SInsertData = 'Insert';
  SSetKeyData = 'Search';
  SCalcFieldsData = 'Calculate';
  SRegistration = 'Kullanıcı Girişi'; 
  SAppTitleLabel = 'Aplikasyon *%s*';
  SHintLabel = 'Kullanıcı adı ve şifresini yazınız';
  SUserNameLabel = 'Kullanıcı &adı:';
  SPasswordLabel = 'Ş&ifre:';
  SInvalidUserName = 'Geçersiz kullanıcı adı veya şifresi';
  SChangePassword = 'Şifre değiştir';
  SOldPasswordLabel = '&Eski şifre:';
  SNewPasswordLabel = '&Yeni şifre:';
  SConfirmPasswordLabel = 'Şifreyi &onayla:';
  SPasswordChanged = 'Şifre değiştirildi';
  SPasswordNotChanged = 'Şifre değiştirilmedi';
  SPasswordsMismatch = 'Yeni ve onay şifreleri aynı değil';
  SDBExceptCaption = 'veri Tabanı Motoru Hatası';
  SBDEErrorLabel = 'BDE Hatası';
  SServerErrorLabel = 'Sunucu Hatası';
  SErrorMsgLabel = 'Hata Mesajı';
  SNextButton = '&Sonraki';
  SPrevButton = 'Ö&nceki';
  SExprIncorrect = 'Filtre ifadesi yanlış biçimlendirilmiş';
  SExprTermination = 'Filtre ifadesi yanlış bitmiş';
  SExprNameError = 'Bitmemiş alan adı';
  SExprStringError = 'Bitmemiş sabit harf dizisi';
  SExprInvalidChar = 'Geçersiz filtre ifade harfi : ''%s''';
  SExprNoRParen = ''')'' bekleniyordu fakat %s bulundu';
  SExprExpected = 'İfade bekleniyordu fakat %s bulundu';
  SExprBadCompare = 'İlşkisel operatörler alan adı veya sabit isterler';
  SConfirmSave = 'Veri değişti. Verileri kaydet ?';
  SDatabaseName = 'Veri tabanı adı: %s';
  SUnlockCaption = 'Aplikasyonu serbest bırak';
  SUnlockHint = 'Şifrenizi yazınız';
  SDeleteMultipleRecords = 'Seçilen bütün kayıtları sil?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF İmajı';
  SChangeGIFSize = 'Gif İmajın büyüklüğü değiştirilemiyor';
  SNoGIFData = 'Yazılacak GIF verisi yok';
  SUnrecognizedGIFExt = 'Anlaşılamayan uzatma bloğu: %.2x';
  SWrongGIFColors = 'Yanlış sayıda renk; 2''nin katları olmalı';
  SBadGIFCodeSize = 'GIF kodun büyüklüğü 2 ile 9 arasında değil';
  SGIFDecodeError = 'GIF şifreli veri bozuk';
  SGIFEncodeError = 'GIF imaj şifreleme hatası';
  SGIFVersion = 'Bilinmeyen GIF sürümü';

  {RXTConst}
  SDualListSrcCaption = '&Kaynak';
  SDualListDestCaption = '&Varış Noktası';
  SClipbrdUnknown = 'Gösterilemiyor. ''Clipboard'' ''ta ki veri bilinmeyen bir düzende.';
  SClipbrdEmpty = '''Clipboard'' boş';
  SCustomizeSpeedbar = 'Hız çubuğunu düzenle';
  SAvailButtons = '&Erişilebilir düğmeler';
  SSpeedbarCategories = '&Katagoriler';
  SSpeedbarEditHint = 'Komut düğmelerini eklemek için, düğmeleri hız çubuğunun üzerine sürükle-bırak. Komut düğmelerini çıkarmak için, düğmeleri komut çubuğundan dışarı sürükle.';
  SParseSyntaxError = 'Sentaks hatası';
  SParseNotCramp = 'Geçersiz durum (bağlantı yok)';
  SParseDivideByZero = 'Sıfır ile bölünme';
  SParseSqrError = 'Geçersiz kayan nokta operasyonu';
  SParseLogError = 'Geçersiz kayan nokta operasyonu';
  SParseInvalidFloatOperation = 'Geçersiz kayan nokta operasyonu';

  {RXLConst}
  srRXControls = 'RX Controls';
  srRXDBAware = 'RX DBAware';
  srRXTools = 'RX Tools';

  srStorageDesigner = 'Form Storage Designer...';

  srProxyEditor = 'Edit Proxies...';
  srPageProxies = '%s Page Proxies';
  srProxyName = 'Page Proxy Name';
  srPageName = 'Page Name';

  srSBItemNotCreate = 'Cannot create a new Speedbar button';
  srConfirmSBDelete = 'Are you sure you want to delete current section?';
  srSpeedbarDesigner = 'Speedbar designer...';
  srNewSectionName = 'Untitled (%d)';

  srEventNotCreate = 'Cannot create a new event';
  srTimerDesigner = 'Edit Events...';
  srTimerEvents = '%s.Events';

  srAniCurFilter = 'Animated Cursors (*.ani)|*.ani|Any files (*.*)|*.*';
  srEditPicture = 'Edit picture...';
  srLoadAniCursor = 'Load from ANI...';

  srLoadIcon = 'Load Icon';
  srSaveImageList = 'Save to bitmap...';

  srCaptionDesigner = 'Edit Captions...';
  srGradientCaptions = 'Captions';
  srBorrowStructure = 'Borrow structure...';

  ccTimeEditHours = 'hours';
  ccTimeEditMins = 'min';
  ccTimeEditSecs = 'sec';
  
  {additional color constant names}
  SColorBlack = 'Black';
  SColorMaroon = 'Maroon';
  SColorGreen = 'Green';
  SColorOlive = 'Olive';
  SColorNavy = 'Navy';
  SColorPurple = 'Purple';
  SColorTeal = 'Teal';
  SColorGray = 'Gray';
  SColorSilver = 'Silver';
  SColorRed = 'Red';
  SColorLime = 'Lime';
  SColorYellow = 'Yellow';
  SColorBlue = 'Blue';
  SColorFuchsia = 'Fuchsia';
  SColorAqua = 'Aqua';
  SColorWhite = 'White';
  SColorScrollBar = 'ScrollBar';
  SColorBackground = 'Background';
  SColorActiveCaption = 'Active Caption';
  SColorInactiveCaption = 'Inactive Caption';
  SColorMenu = 'Menu';
  SColorWindow = 'Window';
  SColorWindowFrame = 'Window Frame';
  SColorMenuText = 'Menu Text';
  SColorWindowText = 'Window Text';
  SColorCaptionText = 'Caption Text';
  SColorActiveBorder = 'Active Border';
  SColorInactiveBorder = 'Inactive Border';
  SColorAppWorkSpace = 'Application WorkSpace';
  SColorHighlight = 'Highlight Menu Item';
  SColorHighlightText = 'Highlight Text';
  SColorBtnFace = 'Button Face';
  SColorBtnShadow = 'Button Shadow';
  SColorGrayText = 'Gray Text';
  SColorBtnText = 'Button Text';
  SColorInactiveCaptionText = 'Inactive Caption Text';
  SColorBtnHighlight = 'Button Highlight';
  SColor3DDkShadow = '3D Dark Shadow';
  SColor3DLight = '3D Light';
  SColorInfoText = 'Info Text';
  SColorInfoBk = 'Info Background';
  SColorCream = 'Cream';
  SColorMoneyGreen = 'Money Green';
  SColorSkyBlue = 'Sky Blue';
  SColorNone = 'None';
  SColorDefault = 'Default';
  SColorCustom = 'Other...';
  {DS}
  SMemNoRecords = 'No data found';
  SInvalidFields = 'No fields defined';
  {Speedbar exception}
  SAutoSpeedbarMode = 'Cannot set this property value while Position is bpAuto';

implementation
end.