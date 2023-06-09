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
unit RxResConst_svk;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Prezerať';
  SDefaultFilter = 'Všetky súbory (*.*)|*.*';
  SDateDlgTitle = 'Vybrať dátum';
  SNextYear = 'Nasledujúci rok|';
  SNextMonth = 'Nasledujúci mesiac|';
  SPrevYear = 'Predchádzajúci rok|';
  SPrevMonth = 'Predchádzajúci mesiac|';
  SNotImplemented = 'Funkcia ešte nie je implementovaná';
  SFileNotExec = 'Tento súbor nie je EXE, DLL ani ICO';
  SLoadLibError = 'Nemožno zaviesť ''%s''';
  SDetails = 'Podrobnosti';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Tuto operáciu nemožno uskutočniť na lokálnej databáze';
  SRetryLogin = 'Chcete opakovať pripojenie k databáze?';
  SExprNotBoolean = 'Pole ''%s'' nie je typu Boolean';
  SExprBadNullTest = 'NULL povolené len s ''='' alebo ''<>''';
  SExprBadField = 'Pole ''%s'' nemožno použiť vo filtry';
  SCaptureFilter = 'Tuto operáciu nemožno uskutočniť, polia sú v móde vkladania filtra';
  SNotCaptureFilter = 'Tuto operáciu nemožno uskutočniť, polia nie sú v móde vkladania filtra';
  SInactiveData = 'zavrený';
  SBrowseData = 'prezerať';
  SEditData = 'editovať';
  SInsertData = 'vkladať';
  SSetKeyData = 'nastaviť kľúč';
  SCalcFieldsData = 'kalkulované polia';
  SRegistration = 'Registrácia';
  SAppTitleLabel = 'Aplikácia *%s*';
  SHintLabel = 'Vložte vaše uživateľské meno a heslo';
  SUserNameLabel = '&Uživateľské meno:';
  SPasswordLabel = '&Heslo:';
  SInvalidUserName = 'Chybné uživateľské meno alebo heslo';
  SChangePassword = 'Zmeniť heslo';
  SOldPasswordLabel = '&Staré heslo:';
  SNewPasswordLabel = '&Nové heslo:';
  SConfirmPasswordLabel = '&Potvrdiť heslo:';
  SPasswordChanged = 'Heslo bolo zmenené';
  SPasswordNotChanged = 'Heslo nebolo zmenené';
  SPasswordsMismatch = 'Nové a potvrdené heslo nesúhlasí';
  SDBExceptCaption = 'Chyba Database Engine';
  SBDEErrorLabel = 'BDE chyba';
  SServerErrorLabel = 'Chyba serveru';
  SErrorMsgLabel = 'Chybová správa';
  SNextButton = '&Nasledujúci';
  SPrevButton = '&Predchádzajúci';
  SExprIncorrect = 'Nekorektne vyvorený výraz pre filter';
  SExprTermination = 'Výraz pre filter nie je korektne ukončený';
  SExprNameError = 'Neukončený názov poľa';
  SExprStringError = 'Neukončená reťazcová konštanta';
  SExprInvalidChar = 'Chybný znak vo výraze pre filter: ''%s''';
  SExprNoRParen = 'Očakávaná '')'' ale nájdené %s';
  SExprExpected = 'Očakávaný výraz, ale nájdené %s';
  SExprBadCompare = 'Relačné operátory vyžadujú pole a konštantu';
  SConfirmSave = 'Dáta boly zmenené. Uložiť?';
  SDatabaseName = 'Názov databázy: %s';
  SUnlockCaption = 'Odomknutie aplikácie';
  SUnlockHint = 'Vložte vaše heslo';
  SDeleteMultipleRecords = 'Vymazat všetky vybrané záznamy?';


  {RXGConst}
  SGIFImage = 'CompuServe GIF obrázok';
  SChangeGIFSize = 'Veľkosť obrázku nemožno zmeniť';
  SNoGIFData = 'Žiadne GIF-dáta k zápisu';
  SUnrecognizedGIFExt = 'Nerozpoznaný rozšírený blok: %.2x';
  SWrongGIFColors = 'Chybné číslo barvy; musí býť násobkom 2';
  SBadGIFCodeSize = 'Veľkost GIF-kódu nie je v rozsahu medzi 2 a 9';
  SGIFDecodeError = 'Dekódované dáta GIF-kódu sú poškodené';
  SGIFEncodeError = 'Chyba obrazového GIF-kódovania';
  SGIFVersion = 'Neznáma verzia GIF-kódovania';

  {RXTConst}
  SDualListSrcCaption = '&Zdroj';
  SDualListDestCaption = '&Cieľ';
  SClipbrdUnknown = 'Nemožno zobraziť. Dáta v clipboardu sú neznámeho formátu.';
  SClipbrdEmpty = 'Clipboard je prázdný';
  SCustomizeSpeedbar = 'Úprava tlačítkovej lišty';
  SAvailButtons = '&Dostupné tlačítka:';
  SSpeedbarCategories = '&Kategórie:';
  SSpeedbarEditHint = 'Ak chcete pridať tlačítka príkazov, tiahnite a pusťte tlačítka na lištu. Ak ich chcete vyňat, tiahnite ich preč z tlačítkovej lišty.';
  SParseSyntaxError = 'Chybná syntax';
  SParseNotCramp = 'Chybná podmienka';
  SParseDivideByZero = 'Delenie nulou';
  SParseSqrError = 'Chybná operácia plávajúcej čiarky';
  SParseLogError = 'Chybná operácia plávajúcej čiarky';
  SParseInvalidFloatOperation = 'Chybná operácia plávajúcej čiarky';

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