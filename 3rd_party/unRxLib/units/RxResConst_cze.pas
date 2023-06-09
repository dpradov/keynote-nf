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
unit RxResConst_cze;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Prohlížet';
  SDefaultFilter = 'Všechny soubory (*.*)|*.*';
  SDateDlgTitle = 'Vybrat datum';
  SNextYear = 'Další rok|';
  SNextMonth = 'Další měsíc|';
  SPrevYear = 'Předchozí rok|';
  SPrevMonth = 'Předchozí měsíc|';
  SNotImplemented = 'Funkce ještě není implementovaná';
  SFileNotExec = 'Tento soubor není EXE, DLL ani ICO';
  SLoadLibError = 'Nelze zavést ''%s''';
  SDetails = 'Podrobnosti';
  SDateOutOfRange = '''%s'' - chybný datum. Hodnota musí být mezi ''%s'' a ''%s''';
  SDateOutOfMin = '''%s'' - chybný datum. Hodnota musí být větší než ''%s''';
  SDateOutOfMax = '''%s'' - chybný datum. Hodnota musí být menší než ''%s''';
  SDateMinLimit = 'Spodní mez datumu musí být menší než ''%s''';
  SDateMaxLimit = 'Horní mez datumu musí být větší než ''%s''';
  {RXDConst}
  SLocalDatabase = 'Tuto operaci nelze provést na lokální databázi';
  SRetryLogin = 'Chcete opakovat připojení k databázi?';
  SExprNotBoolean = 'Pole ''%s'' není typu Boolean';
  SExprBadNullTest = 'NULL povolen pouze s ''='' a ''<>''';
  SExprBadField = 'Pole ''%s'' nelze použít ve filtru';
  SCaptureFilter = 'Tuto operaci nelze provést, pole jsou v módu vkládání filtru';
  SNotCaptureFilter = 'Tuto operaci nelze provést, pole nejsou v módu vkládání filtru';
  SInactiveData = 'zavřen';
  SBrowseData = 'prohlížet';
  SEditData = 'editovat';
  SInsertData = 'vkládat';
  SSetKeyData = 'nastavit klíč';
  SCalcFieldsData = 'kalkulovaná pole';
  SRegistration = 'Registrace';
  SAppTitleLabel = 'Aplikace * %s *';
  SHintLabel = 'Vložte vaše uživatelské jméno a heslo';
  SUserNameLabel = '&Uživatelské jméno:';
  SPasswordLabel = '&Heslo:';
  SInvalidUserName = 'Chybné uživatelské jméno nebo heslo';
  SChangePassword = 'Změnit heslo';
  SOldPasswordLabel = '&Staré heslo:';
  SNewPasswordLabel = '&Nové heslo:';
  SConfirmPasswordLabel = '&Potvrdit heslo:';
  SPasswordChanged = 'Heslo bylo změněno';
  SPasswordNotChanged = 'Heslo nebylo změněno';
  SPasswordsMismatch = 'Nové a potvrzené heslo nesouhlasí';
  SDBExceptCaption = 'Chyba Database Engine';
  SBDEErrorLabel = 'BDE chyba';
  SServerErrorLabel = 'Chyba serveru';
  SErrorMsgLabel = 'Chybová zpráva';
  SNextButton = '&Další';
  SPrevButton = '&Předchozí';
  SExprIncorrect = 'Nekorektně vyvořený výraz pro filtr';
  SExprTermination = 'Výraz pro filtr není korektně ukončen';
  SExprNameError = 'Neukončený název pole';
  SExprStringError = 'Neukončená string konstanta';
  SExprInvalidChar = 'Chybný znak ve výrazu pro filtr: ''%s''';
  SExprNoRParen = 'Očekávaná '')'' ale nalezeno %s';
  SExprExpected = 'Očekáván výraz, ale nalezeno %s';
  SExprBadCompare = 'Relační operátory vyžadují pole a konstantu';
  SConfirmSave = 'Data byla změněna. Uložit?';
  SDatabaseName = 'Název databáze: %s';
  SUnlockCaption = 'Odemknutí aplikace';
  SUnlockHint = 'Vložte vaše heslo';
  SDeleteMultipleRecords = 'Vymazat všechny vybrané záznamy?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF obrázek';
  SChangeGIFSize = 'Velikost obrázku nelze změnit';
  SNoGIFData = 'Žádná GIF-data k zápisu';
  SUnrecognizedGIFExt = 'Nerozpoznaný rozšířený blok: %.2x';
  SWrongGIFColors = 'Chybné číslo barvy; musí být násobkem 2';
  SBadGIFCodeSize = 'Velikost GIF-kódu není v rozsahu mezi 2 a 9';
  SGIFDecodeError = 'Dekódovaná data GIF-kódu jsou poškozená';
  SGIFEncodeError = 'Chyba obrazového GIF-kódování';
  SGIFVersion = 'Neznámá verse GIF-kódování';
  {RXTConst}
  SDualListSrcCaption = '&Zdroj';
  SDualListDestCaption = '&Cíl';
  SClipbrdUnknown = 'Nelze zobrazit. Data v clipboardu jsou neznámého formátu.';
  SClipbrdEmpty = 'Clipboard je prázdný';
  SCustomizeSpeedbar = 'Úprava tlačítkové lišty';
  SAvailButtons = '&Dostupná tlačítka:';
  SSpeedbarCategories = '&Kategorie:';
  SSpeedbarEditHint = 'Chcete-li přidat tlačítka příkazů, táhněte a pusťte tlačítka na lištu. Chcete-li je vyjmout, táhněte je pryč z tlačítkové lišty.';
  SParseSyntaxError = 'Chyba syntaxe';
  SParseNotCramp = 'Chybná podmínka';
  SParseDivideByZero = 'Dělení nulou';
  SParseSqrError = 'Chybná operace plovoucí čárky';
  SParseLogError = 'Chybná operace plovoucí čárky';
  SParseInvalidFloatOperation = 'Chybná operace plovoucí čárky';
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

  ccTimeEditHours = 'hod';
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