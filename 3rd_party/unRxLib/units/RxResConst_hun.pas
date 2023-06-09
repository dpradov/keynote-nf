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
unit RxResConst_hun;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Tallóz';
  SDefaultFilter = 'Minden fájl (*.*)|*.*';
  SDateDlgTitle = 'Dátum választás';
  SNextYear = 'Következő év|';
  SNextMonth = 'Következő hónap|';
  SPrevYear = 'Előző év|';
  SPrevMonth = 'Előző Hónap|';
  SNotImplemented = 'Nem implementált funkció';
  SFileNotExec = 'A megadott fájl nem végrehajtható, DLL vagy ikon fájl';
  SLoadLibError = 'A ''%s'' könyvtár nem tölthető be';
  SDetails = 'Részletek';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'A művelet helyi adatbázison nem hajtható végre';
  SRetryLogin = 'Megpróbáljam még egyszer az adatbázishoz kapcsolódást?';
  SExprNotBoolean = 'A ''%s'' mező nem logikai típusú';
  SExprBadNullTest = 'NULL csak ''='' és ''<>'' -vel megengedett';
  SExprBadField = 'A ''%s'' mező nem használható szűrőkifejezésben';
  SCaptureFilter = 'A művelet nem hajtható végre elfogott vezérlőknél';
  SNotCaptureFilter = 'A művelet nem hajtható végre nem elfogott vezérlőknél';
  SInactiveData = 'Zárva';
  SBrowseData = 'Tallóz';
  SEditData = 'Szerkeszt';
  SInsertData = 'Beszúr';
  SSetKeyData = 'Keres';
  SCalcFieldsData = 'Számol';
  SRegistration = 'Regisztrálás';
  SAppTitleLabel = 'Alkalmazás *%s*';
  SHintLabel = 'Írd be a neved és jelszavad';
  SUserNameLabel = 'Felhasználó &neve:';
  SPasswordLabel = '&Jelszó:';
  SInvalidUserName = 'Érvénytelen név vagy jelszó';
  SChangePassword = 'Jelszó változtatás';
  SOldPasswordLabel = '&Régi jelszó:';
  SNewPasswordLabel = 'Ú&j jelszó:';
  SConfirmPasswordLabel = 'Jelszó &megerősítés:';
  SPasswordChanged = 'A jelszó megváltozott';
  SPasswordNotChanged = 'A jelszó nem változott';
  SPasswordsMismatch = 'Az új és a megerősített jelszó nem egyezik';
  SDBExceptCaption = 'Adatbázis motor hiba';
  SBDEErrorLabel = 'BDE hiba';
  SServerErrorLabel = 'Szerver hiba';
  SErrorMsgLabel = 'Hiba üzenet';
  SNextButton = '&Következő';
  SPrevButton = '&Előző';
  SExprIncorrect = 'Rossz szűrőkifejezés';
  SExprTermination = 'Rosszul lezárt szűrőkifejezés';
  SExprNameError = 'Lezáratlan mezőnév';
  SExprStringError = 'Lezáratlan karakterlánc';
  SExprInvalidChar = 'Érvénytelen karakter a szűrőben: ''%s''';
  SExprNoRParen = ''')'' helyett %s található';
  SExprExpected = 'Kifejezés helyett %s található';
  SExprBadCompare = 'Relációs operátorhoz mező vagy konstans kell';
  SConfirmSave = 'Az adatok megváltoztak. Elmentsük?';
  SDatabaseName = 'Adatbázis név: %s';
  SUnlockCaption = 'Alkalmazás feloldása';
  SUnlockHint = 'Írd be a jelszavad';
  SDeleteMultipleRecords = 'Töröljünk minden kiválasztott rekordot?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF Kép';
  SChangeGIFSize = 'A GIF kép mérete nem változtatható';
  SNoGIFData = 'Nincs írható GIF adat';
  SUnrecognizedGIFExt = 'Ismeretlen kiterjesztett blokk: %.2x';
  SWrongGIFColors = 'Rossz színszám; kettõ hatványának kell lennie';
  SBadGIFCodeSize = 'GIF kódméretnek 2 és 9 között kell lennie';
  SGIFDecodeError = 'GIF kódolt adat sérült';
  SGIFEncodeError = 'GIF képkódolási hiba';
  SGIFVersion = 'Ismeretlen GIF verzió';
  {RXTConst}
  SDualListSrcCaption = '&Forrás';
  SDualListDestCaption = '&Cél';
  SClipbrdUnknown = 'Nem jeleníthető meg. A vágólap tartalma ismeretlen formátumú.';
  SClipbrdEmpty = 'A vágólap üres';
  SCustomizeSpeedbar = 'Gyorsítópanel beállítása';
  SAvailButtons = '&Elérhető gombok:';
  SSpeedbarCategories = '&Kategóriák:';
  SSpeedbarEditHint = 'Gombok hozzáadásához fogja és dobja a gombokat a gyorsítópanelra. Eltávolítani pedig a gombok panelről lehúzásával lehet.';
  SParseSyntaxError = 'Szintaktikai hiba';
  SParseNotCramp = 'Rossz feltétel (nincs lezárva)';
  SParseDivideByZero = 'Nullával osztás';
  SParseSqrError = 'Érvénytelen lebegőpontos művelet';
  SParseLogError = 'Érvénytelen lebegőpontos művelet';
  SParseInvalidFloatOperation = 'Érvénytelen lebegőpontos művelet';
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