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
unit RxResConst_bul;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Списък';
  SDefaultFilter = 'Всички файлове (*.*)|*.*';
  SDateDlgTitle = 'Избор на дата';
  SNextYear = 'Следваща година|';
  SNextMonth = 'Следващ месец|';
  SPrevYear = 'Предишна година|';
  SPrevMonth = 'Предишен месец|';
  SNotImplemented = 'Функцията не е реализирана';
  SFileNotExec = 'Посоченият файл не е изпълним, динамична библиотека или файл с икона';
  SLoadLibError = 'Bиблиотеката ''%s'' не се зарежда';
  SDetails = 'Детайли';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Невъзможно е извършването на операцията с локална база данни';
  SRetryLogin = 'Искате ли повторен опит за свързване към базата данни?';
  SExprNotBoolean = 'Полето ''%s'' не е булево.';
  SExprBadNullTest = 'NULL е допустимо само за операциите ''='' и ''<>''';
  SExprBadField = 'Полето ''%s'' не може да бъде използвано във филтър.';
  SCaptureFilter = 'Невъзможно е да бъде изпълнена тази операция, когато контролите са прихванати.';
  SNotCaptureFilter = 'Невъзможно е да бъде изпълнена тази операция, когато контролите не са прихванати.';
  SInactiveData = 'Неактивно';
  SBrowseData = 'Списък';
  SEditData = 'Редактиране';
  SInsertData = 'Добавяне';
  SSetKeyData = 'Търсене';
  SCalcFieldsData = 'Изчисления';
  SRegistration = 'Регистрация';
  SAppTitleLabel = 'Приложение *%s*';
  SHintLabel = 'Въведете вашето име и парола';
  SUserNameLabel = '&Потребителско име:';
  SPasswordLabel = '&Парола:';
  SInvalidUserName = 'Невалидни потребителско име или парола';
  SChangePassword = 'Смяна на парола';
  SOldPasswordLabel = '&Стара парола:';
  SNewPasswordLabel = '&Нова парола:';
  SConfirmPasswordLabel = '&Потвърдена парола:';
  SPasswordChanged = 'Паролата е променена';
  SPasswordNotChanged = 'Паролата не е променена';
  SPasswordsMismatch = 'Новата и потвърдената парола не съвпадат';
  SDBExceptCaption = 'Грешка на базата данни';
  SBDEErrorLabel = 'Грешка на BDE-то';
  SServerErrorLabel = 'Грешка на сървъра';
  SErrorMsgLabel = 'Съобщение за грешка';
  SNextButton = '&Следващ';
  SPrevButton = '&Предишен';
  SExprIncorrect = 'Неправилно формулиран израз за филтър';
  SExprTermination = 'Неправилен край на филтър';
  SExprNameError = 'Неправилно име на поле';
  SExprStringError = 'Неправилно завършен низ';
  SExprInvalidChar = 'Символът ''%s'' е неправилно поставен във филтъра';
  SExprNoRParen = 'Трябва да има '')'',а не %s';
  SExprExpected = 'Трабва да има израз, а не %s';
  SExprBadCompare = 'Операторите за сравнение очакват поле и константа';
  SConfirmSave = 'Данните са променени. Желаете ли да бъдат записани?';
  SDatabaseName = 'База данни: %s';
  SUnlockCaption = 'Отключване (отблокиране) на приложението';
  SUnlockHint = 'Въведете Вашата парола';
  SDeleteMultipleRecords = 'Желаете ли да се изтрият всички избрани записи?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Промяната на размерите на GIF изображението е невъзможна';
  SNoGIFData = 'Няма данни за запис във GIF формат';
  SUnrecognizedGIFExt = 'Неопределено разширение на GIF формата: %.2x';
  SWrongGIFColors = 'Грешен брой цветове; трябва да бъде степен на 2';
  SBadGIFCodeSize = 'Размерът на GIF кода не е в интервала 2-9';
  SGIFDecodeError = 'Данните във GIF формат са повредени';
  SGIFEncodeError = 'Грешка при кодиране на GIF изображение';
  SGIFVersion = 'Версията на GIF изображението е необичайна';
  {RXTConst}
  SDualListSrcCaption = '&Източник';
  SDualListDestCaption = '&Цел(Приемник)';
  SClipbrdUnknown = 'Неизвестен формат на данни в Clipboard. Не могат да бъдат показани.';
  SClipbrdEmpty = 'Празен Clipboard';
  SCustomizeSpeedbar = 'Настройка на палитра с инструменти';
  SAvailButtons = '&Достъпни бутони:';
  SSpeedbarCategories = '&Категории:';
  SSpeedbarEditHint = 'За да добавяте бутони, ги влачете върху палитрата с инструменти. За да ги премахвате ги влачете навън.';
  SParseSyntaxError = 'Синтактична грешка';
  SParseNotCramp = 'Липсва скоба';
  SParseDivideByZero = 'Деление на нула';
  SParseSqrError = 'Изразът под корена е по-малък от нула';
  SParseLogError = 'Изразът в логаритъма е по-малък от нула';
  SParseInvalidFloatOperation = 'Грешка при операция с плаваща запетая';
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