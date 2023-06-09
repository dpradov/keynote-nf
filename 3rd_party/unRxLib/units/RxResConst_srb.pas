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
unit RxResConst_srb;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Pregled';
  SDefaultFilter = 'Sve datoteke (*.*)|*.*';
  SDateDlgTitle = 'Izaberi datum';
  SNextYear = 'Sledeca godina|';
  SNextMonth = 'Sledeci mesec|';
  SPrevYear = 'Predhodna godina|';
  SPrevMonth = 'Predhodni mesec|';
  SNotImplemented = 'Funkcija jos nije implementirana';
  SFileNotExec = 'Izabrana datoteka nije izvrsna datoteka, dinamicki povezana biblioteka (DLL), ili datoteka sa ikonom';
  SLoadLibError = 'Ne mogu ucitati biblioteku ''%s''';
  SDetails = 'Detalji';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Te operacije ne mogu izvesti na lokalnoj bazi';
  SRetryLogin = 'Da li bi ste ponovili povezivanje na bazu podataka?';
  SExprNotBoolean = 'Polje ''%s'' nije logickog tipa';
  SExprBadNullTest = 'NULL je dozvoljeno samo sa ''='' i ''<>''';
  SExprBadField = 'Polje ''%s'' ne moze biti upotrebljeno u izrazu za filtriranje';
  SCaptureFilter = 'Te operacije ne mogu izvesti, jer su elementi oznaceni';
  SNotCaptureFilter = 'Te operacije ne mogu izvesti, jer elementi niso oznaceni';
  SInactiveData = 'Zatvoreno';
  SBrowseData = 'Pregledavanje';
  SEditData = 'Popravljanje';
  SInsertData = 'Dodavanje';
  SSetKeyData = 'Nalazenje kljuca';
  SCalcFieldsData = 'Izracunavanje polja';
  SRegistration = 'Registracija';
  SAppTitleLabel = 'Aplikacija *%s*';
  SHintLabel = 'Vnesite svoje korisnicko ime i lozinku';
  SUserNameLabel = '&Korisnicko ime:';
  SPasswordLabel = '&Lozinka:';
  SInvalidUserName = 'Nepravilno korisnicko ime ili lozinka';
  SChangePassword = 'Promeni lozinku';
  SOldPasswordLabel = '&Stara lozinka:';
  SNewPasswordLabel = '&Nova lozinka:';
  SConfirmPasswordLabel = '&Potvrdi lozinku:';
  SPasswordChanged = 'Lozinka je promenjena';
  SPasswordNotChanged = 'Lozinka nije promenjena';
  SPasswordsMismatch = 'Nova i potvrdjena lozinka se ne poklapaju';
  SDBExceptCaption = 'Greska u pristupu podatcima (Database Engine Error)';
  SBDEErrorLabel = 'BDE greska';
  SServerErrorLabel = 'Greska servera';
  SErrorMsgLabel = 'Izvestaj o gresci';
  SNextButton = '&Sledeci';
  SPrevButton = '&Predhodni';
  SExprIncorrect = 'Nepravilno formiran izraz za filtriranje';
  SExprTermination = 'Izraz za filtriranje nije pravilno zavrsen';
  SExprNameError = 'Nezavrseno ime polja';
  SExprStringError = 'Nezavrsena znakovna konstanta';
  SExprInvalidChar = 'Nepravilni znak u izrazu za filtriranje: ''%s''';
  SExprNoRParen = 'Ocekovano '')'', ali najdeno %s ';
  SExprExpected = 'Ocekivan izraz, ali najden %s';
  SExprBadCompare = 'Relacijski operatori zahtevaju polje i konstantu';
  SConfirmSave = 'Podatci su promenjeni. Da ih snimim?';
  SDatabaseName = 'Ime baze podataka: %s';
  SUnlockCaption = 'Odkljucaj aplikaciju';
  SUnlockHint = 'Vnesite svoju lozinku';
  SDeleteMultipleRecords = 'Brisanje svih izbranih zapisa?';

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
  SDualListDestCaption = '&Odrediste';
  SClipbrdUnknown = 'Ne mogu pokazati. Podatci u Clipbordu su u nepoznatom formatu.';
  SClipbrdEmpty = 'Clipboard je prazan';
  SCustomizeSpeedbar = 'Uredi liniju sa precicama';
  SAvailButtons = '&Raspolozivi tasteri:';
  SSpeedbarCategories = '&Kategorije:';
  SSpeedbarEditHint = 'Tastere dodatjete tako sto ih dovucete i spustite u liniju sa precicama. Odabrane tasteree odstranite tako sto ih odvucete iz linije sa precicama.';
  SParseSyntaxError = 'Greska u sintaksi';
  SParseNotCramp = 'Nepravilni izraz (bez operatora)';
  SParseDivideByZero = 'Deljenje sa nulom';
  SParseSqrError = 'Nepravilna operacija s pokretnom tackom';
  SParseLogError = 'Nepravilna operacija s pokretnom tackom';
  SParseInvalidFloatOperation = 'Nepravilna operacija s pokretnom tackom';

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