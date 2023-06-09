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
unit RxResConst_pol;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Przeglądanie';
  SDefaultFilter = 'Wszystkie zbiory (*.*)|*.*';
  SDateDlgTitle = 'Wybierz datę';
  SNextYear = 'Następny rok|';
  SNextMonth = 'Następny miesiąc|';
  SPrevYear = 'Poprzedni rok|';
  SPrevMonth = 'Poprzedni miesiąc|';
  SNotImplemented = 'Funkcja jeszcze nie zaimplementowana';
  SFileNotExec = 'Wybrany zbiór nie jest plikiem wykonywalnym, biblioteką DLL, lub ikoną';
  SLoadLibError = 'Nie można załadować biblioteki ''%s''';
  SDetails = 'Szczegóły';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Nie można wykonać tej operacji na lokalnej bazie';
  SRetryLogin = 'Czy próbować jeszcze raz podłączyć się do bazy?';
  SExprNotBoolean = 'Pole ''%s'' nie jest typu Boolean';
  SExprBadNullTest = 'NULL jest tylko dopuszczalne z ''='' i ''<>''';
  SExprBadField = 'Pole ''%s'' nie może być użyte w wyrażeniu filtrującym';
  SCaptureFilter = 'Nie można wykonać tej operacji kiedy kontrolki są zablokowane';
  SNotCaptureFilter = 'Nie można wykonać tej operacji kiedy kontrolki są zablokowane';
  SInactiveData = 'Zamknięte';
  SBrowseData = 'Przeglądanie';
  SEditData = 'Edycja';
  SInsertData = 'Wstawianie';
  SSetKeyData = 'Szukanie';
  SCalcFieldsData = 'Obliczanie';
  SRegistration = 'Rejestracja';
  SAppTitleLabel = 'Aplikacja *%s*';
  SHintLabel = 'Wpisz swoją nazwę użytkownika i hasło';
  SUserNameLabel = '&Nazwa użytkownika:';
  SPasswordLabel = '&Hasło:';
  SInvalidUserName = 'Błędna nazwa użytkownika lub hasło';
  SChangePassword = 'Zmiana hasła';
  SOldPasswordLabel = '&Stare hasło:';
  SNewPasswordLabel = '&Nowe hasło:';
  SConfirmPasswordLabel = '&Potwierdź hasło:';
  SPasswordChanged = 'Hasło zostało zmienione';
  SPasswordNotChanged = 'Hasło nie zostało zmienione';
  SPasswordsMismatch = 'Hasło i potweirdzenie hasła nie są takie same';
  SDBExceptCaption = 'Błąd Database Engine';
  SBDEErrorLabel = 'Błąd BDE';
  SServerErrorLabel = 'Błąd Serwera';
  SErrorMsgLabel = 'Komunikat o błędzie';
  SNextButton = '&Następny';
  SPrevButton = '&Poprzedni';
  SExprIncorrect = 'Źle zbudowany warunek filtrowania';
  SExprTermination = 'Źle zakończony warunek filtrowania';
  SExprNameError = 'Niezakończona nazwa pola';
  SExprStringError = 'Niezakończona zmienna łańcuchowa';
  SExprInvalidChar = 'Nie prawidłowy znak w wyrażeniu filtrującym: ''%s''';
  SExprNoRParen = ''')'' spodziewany ale %s znaleziony';
  SExprExpected = 'Spodziewane wyrażenie lecz %s znalezione';
  SExprBadCompare = 'Operatory relacji potrzebują pola i stałej';
  SConfirmSave = 'Dane zostały zmienione. Zapisać?';
  SDatabaseName = 'Nazwa bazy: %s';
  SUnlockCaption = 'Odblokowanie aplikacji';
  SUnlockHint = 'Wpisz swoje hasło';
  SDeleteMultipleRecords = 'Skasować wszystkie zaznaczone rekordy?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Nie można zmienić wielkości obrazu GIF';
  SNoGIFData = 'Brak danych GIF do zapisu';
  SUnrecognizedGIFExt = 'Nierozpoznany rozszerzony blok: %.2x';
  SWrongGIFColors = 'Zła liczba kolorów; Liczba musi być potęgą 2';
  SBadGIFCodeSize = 'GIF wielkość kodu poza zakresem [2..9]';
  SGIFDecodeError = 'GIF zakodowane dane są błędne';
  SGIFEncodeError = 'GIF błąd kodowania';
  SGIFVersion = 'Nieznana wersja GIF';

  {RXTConst}
  SDualListSrcCaption = '&Źródło';
  SDualListDestCaption = '&Przeznaczenie';
  SClipbrdUnknown = 'Nie można wyświetlić. Dane w schowku są w nieznanym formacie';
  SClipbrdEmpty = 'Schowek jest pusty';
  SCustomizeSpeedbar = 'Dostosowywanie paska';
  SAvailButtons = '&Dostępne przyciski:';
  SSpeedbarCategories = '&Kategorie:';
  SSpeedbarEditHint = 'Żeby dodać przycisk, przenieś i upuś przycisk na pasek. Żeby usunąć , przeciągnij poza pasek';
  SParseSyntaxError = 'Błąd syntaktyczny';
  SParseNotCramp = 'Błędny warunek (no cramp)';
  SParseDivideByZero = 'Dzielenie przez zero';
  SParseSqrError = 'Błędna operacja zmiennoprzecinkowa';
  SParseLogError = 'Błędna operacja zmiennoprzecinkowa';
  SParseInvalidFloatOperation = 'Błędna operacja zmiennoprzecinkowa';

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