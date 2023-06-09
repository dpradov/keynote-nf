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
unit RxResConst_ukr;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Список';
  SDefaultFilter = 'Всі файли (*.*)|*.*';
  SDateDlgTitle = 'Вибір дати';
  SNextYear = 'Наступний рік|';
  SNextMonth = 'Наступний місяць|';
  SPrevYear = 'Попередній рік|';
  SPrevMonth = 'Попередній місяць|';
  SNotImplemented = 'Функція не реалізована';
  SFileNotExec = 'Вказаний файл не є виконуючімся, динамічною бібліотекою чи файлом іконки';
  SLoadLibError = 'Неможливо завантажити бібліотеку ''%s''';
  SDetails = 'Зведення';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Неможливо виконати цю операцію з локальною базою даних';
  SRetryLogin = 'Ви хочете повторити спробу підєднання до бази даних?';
  SExprNotBoolean = 'Поле ''%s'' не є полем логічного типу';
  SExprBadNullTest = 'NULL-значення припустимі тільки в операціях ''='' та ''<>''';
  SExprBadField = 'Поле ''%s'' не може бути використане у вираженні фільтру';
  SCaptureFilter = 'Елементи керування захоплені фільтром';
  SNotCaptureFilter = 'Елементи керування повинні бути захоплені фільтром';
  SInactiveData = 'неактивно';
  SBrowseData = 'перегляд';
  SEditData = 'редагування';
  SInsertData = 'додавання';
  SSetKeyData = 'пошук';
  SCalcFieldsData = 'підрахунок';
  SRegistration = 'Реєстрація';
  SAppTitleLabel = 'Програма *%s*';
  SHintLabel = 'Введіть Ваше ім''я користувача і пароль';
  SUserNameLabel = '&Ім''я користувача:';
  SPasswordLabel = '&Пароль:';
  SInvalidUserName = 'Невірне ім''я користувача чи пароль';
  SChangePassword = 'Зміна пароля';
  SOldPasswordLabel = '&Старий пароль:';
  SNewPasswordLabel = '&Новий пароль:';
  SConfirmPasswordLabel = '&Підтвердження:';
  SPasswordChanged = 'Пароль змінений';
  SPasswordNotChanged = 'Пароль не змінений';
  SPasswordsMismatch = 'Новий пароль і підтвердження не збігаються';
  SDBExceptCaption = 'Помилка процесора БД';
  SBDEErrorLabel = 'Помилка BDE';
  SServerErrorLabel = 'Помилка сервера';
  SErrorMsgLabel = 'Повідомлення про помилку';
  SNextButton = '&Далі';
  SPrevButton = '&Назад';
  SExprIncorrect = 'Некоректно сформульований вираз фільтру';
  SExprTermination = 'Невірне завершення виразу фільтру';
  SExprNameError = 'Неможливо визначити завершення назви поля';
  SExprStringError = 'Неможливо визначити завершення рядкової константи';
  SExprInvalidChar = 'Невірний символ у виразі фільтру: ''%s''';
  SExprNoRParen = 'Очікуалося '')'', а знайдено: %s';
  SExprExpected = 'Очікувався вираз, а знайдено %s';
  SExprBadCompare = 'Операції порівняння вимагають наявності поля і константи';
  SConfirmSave = 'Дані були змінені. Зберегти?';
  SDatabaseName = 'База даних: %s';
  SUnlockCaption = 'Розблокування';
  SUnlockHint = 'Введіть ваш пароль';
  SDeleteMultipleRecords = 'Видалити всі вибрані записи?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Неможливо змінити розмір зображення GIF';
  SNoGIFData = 'Нема даних для збереження в форматі GIF';
  SUnrecognizedGIFExt = 'Невизначен розширення GIF: %.2x';
  SWrongGIFColors = 'Невірне число кольорів; повинно бути ступенем числа 2';
  SBadGIFCodeSize = 'Розмір коду GIF не належить діапазону від 2 до 9';
  SGIFDecodeError = 'Невірні дані в форматі GIF';
  SGIFEncodeError = 'Помилка кодировки даних в форматі GIF';
  SGIFVersion = 'Невідома версія зображення GIF';

  {RXTConst}
  SDualListSrcCaption = '&Джерело';
  SDualListDestCaption = '&Приймач';
  SClipbrdUnknown = 'Невідомий формат даних у бефері обміну';
  SClipbrdEmpty = 'Буфер обміну пустий';
  SCustomizeSpeedbar = 'Настроювання панелі інструментів';
  SAvailButtons = '&Доступні кнопки:';
  SSpeedbarCategories = '&Категорії:';
  SSpeedbarEditHint = 'Виберіть кнопку і перенесіть її за допомогою миші на потрібне місце на панелі інструментів.';
  SParseSyntaxError = 'Синтаксична помилка';
  SParseNotCramp = 'Не вистачає дужки';
  SParseDivideByZero = 'Ділення на нуль';
  SParseSqrError = 'Вираз під коренем менше нуля';
  SParseLogError = 'Вираз під логарифмом менше нуля';
  SParseInvalidFloatOperation = 'Помилка в операції з плаваючою комою';

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