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
unit RxResConst_slo;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Preišči';
  SDefaultFilter = 'Vse datoteke (*.*)|*.*';
  SDateDlgTitle = 'Izberi datum';
  SNextYear = 'Naslednje leto|';
  SNextMonth = 'Naslednji mesec|';
  SPrevYear = 'Predhodno leto|';
  SPrevMonth = 'Predhodni mesec|';
  SNotImplemented = 'Funkcija še ni implementirana';
  SFileNotExec = 'Izbrana datoteka ni izvršilna datoteka, dinamično povezljiva knjižnica (DLL), ali datoteka z ikono';
  SLoadLibError = 'Ne morem naložiti knjižnice ''%s''';
  SDetails = 'Podrobnosti';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Te operacije ne morem izvesti na lokalni bazi';
  SRetryLogin = 'Ali bi radi ponovili povezavo do baze podatkov?';
  SExprNotBoolean = 'Polje ''%s'' ni logičnega tipa';
  SExprBadNullTest = 'NULL je dovoljeno samo z ''='' in ''<>''';
  SExprBadField = 'Polje ''%s'' ne more biti uporabljeno v izrazu za filtriranje';
  SCaptureFilter = 'Te operacije ne morem izvesti, ko so elementi označeni';
  SNotCaptureFilter = 'Te operacije ne morem izvesti, ko elementi niso označeni';
  SInactiveData = 'zaprto';
  SBrowseData = 'pregledovanje';
  SEditData = 'popravljanje';
  SInsertData = 'dodajanje';
  SSetKeyData = 'določanje ključa';
  SCalcFieldsData = 'izračunavanje polj';
  SRegistration = 'Registracija';
  SAppTitleLabel = 'Aplikacija *%s*';
  SHintLabel = 'Vnesite svoje uporabniško ime in geslo';
  SUserNameLabel = '&Uporabniško ime:';
  SPasswordLabel = '&Geslo:';
  SInvalidUserName = 'Nepravilno uporabniško ime ali geslo';
  SChangePassword = 'Spremeni geslo';
  SOldPasswordLabel = '&Staro geslo:';
  SNewPasswordLabel = '&Novo geslo:';
  SConfirmPasswordLabel = '&Potrdi geslo:';
  SPasswordChanged = 'Geslo je spremenjeno';
  SPasswordNotChanged = 'Geslo ni spremenjeno';
  SPasswordsMismatch = 'Novo in potrjeno geslo se ne ujemata';
  SDBExceptCaption = 'Napaka na podatkovnem stroju (Database Engine Error)';
  SBDEErrorLabel = 'BDE napaka';
  SServerErrorLabel = 'Server napaka';
  SErrorMsgLabel = 'Sporočilo o napaki';
  SNextButton = '&Naslednji';
  SPrevButton = '&Predhodni';
  SExprIncorrect = 'Nepravilno formiran izraz za filtriranje';
  SExprTermination = 'Izraz za filtriranje ni pravilno končan';
  SExprNameError = 'Nekončano ime polja';
  SExprStringError = 'Nekončana znakovna konstanta';
  SExprInvalidChar = 'Nepravilni znak v izrazu za filtriranje: ''%s''';
  SExprNoRParen = 'Pričakovano '')'', toda najdeno %s ';
  SExprExpected = 'Pričakovan izraz, toda najden %s';
  SExprBadCompare = 'Relacijski operatorji zahtevajo polje in konstanto';
  SConfirmSave = 'Podatki so spremenjeni. Jih shranim?';
  SDatabaseName = 'Ime baze podatkov: %s';
  SUnlockCaption = 'Odkleni aplikacijo';
  SUnlockHint = 'Vnesite svoje geslo';
  SDeleteMultipleRecords = 'Zbrišem vse izbrane zapise?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Cannot change the Size of a GIF image';
  SNoGIFData = 'No GIF Data to write';
  SUnrecognizedGIFExt = 'Unrecognized extension block: %.2x';
  SWrongGIFColors = 'Wrong number of colors; must be a power of 2';
  SBadGIFCodeSize = 'GIF code size not in range 2 to 9';
  SGIFDecodeError = 'GIF encoded data is corrupt';
  SGIFEncodeError = 'GIF image encoding error';
  SGIFVersion = 'Unknown GIF version';
  {RXTConst}
  SDualListSrcCaption = '&Izvor';
  SDualListDestCaption = '&Cilj';
  SClipbrdUnknown = 'Ne morem pokazati. Podatki v odložišču so v nepoznanem formatu.';
  SClipbrdEmpty = 'Odložišče je prazno';
  SCustomizeSpeedbar = 'Uredi orodno vrstico';
  SAvailButtons = '&Gumbi na voljo:';
  SSpeedbarCategories = '&Kategorije:';
  SSpeedbarEditHint = 'Ukazne gumbe gumbe dodate tako, da jih povlečete in spustite v orodni vrstici. Ukazne gumbe odstranite tako, da jih odvlečete iz orodne vrstice.';
  SParseSyntaxError = 'Napaka v sintaksi';
  SParseNotCramp = 'Nepravilni pogoji (brez operatorjev)';
  SParseDivideByZero = 'Deljenje z nič';
  SParseSqrError = 'Nepravilna operacija s plavajočo vejico';
  SParseLogError = 'Nepravilna operacija s plavajočo vejico';
  SParseInvalidFloatOperation = 'Nepravilna operacija s plavajočo vejico';

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