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
unit RxResConst_rus;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Список';
  SDefaultFilter = 'Все файлы (*.*)|*.*';
  SDateDlgTitle = 'Выбор даты';
  SNextYear = 'Следующий год|';
  SNextMonth = 'Следующий месяц|';
  SPrevYear = 'Предыдущий год|';
  SPrevMonth = 'Предыдущий месяц|';
  SNotImplemented = 'Функция не реализована';
  SFileNotExec = 'Указанный файл не является исполнимым файлом, динамической библиотекой или файлом иконки';
  SLoadLibError = 'Невозможно загрузить библиотеку ''%s''';
  SDetails = 'Сведени\377';

  SDateOutOfRange = '''%s'' - неверная дата. Значение должно быть между ''%s'' и ''%s''';
  SDateOutOfMin = '''%s'' - неверная дата. Значение должно быть не меньше ''%s''';
  SDateOutOfMax = '''%s'' - неверна\377 дата. Значение должно быть не больше ''%s''';
  SDateMinLimit = 'Минимальная дата должна быть не больше ''%s''';
  SDateMaxLimit = 'Максимальная дата должна быть не меньше ''%s''';
  {RXDConst}
  SLocalDatabase = 'Невозможно произвести эту операцию с локальной базой данных';
  SRetryLogin = 'Вы хотите повторить попытку соединения с базой данных?';
  SExprNotBoolean = 'Поле ''%s'' не является полем логического типа';
  SExprBadNullTest = 'NULL-значения допустимы только в операциях ''='' и ''<>''';
  SExprBadField = 'Поле ''%s'' не может быть использовано в выражении фильтра';
  SCaptureFilter = 'Элементы управления захвачены фильтром';
  SNotCaptureFilter = 'Элементы управления должны быть захвачены фильтром';
  SInactiveData = 'неактивно';
  SBrowseData = 'просмотр';
  SEditData = 'редактирование';
  SInsertData = 'добавление';
  SSetKeyData = 'поиск';
  SCalcFieldsData = 'подсчет';
  SRegistration = 'Регистрация';
  SAppTitleLabel = 'Программа *%s*';
  SHintLabel = 'Введите Ваше пользовательское имя и пароль';
  SUserNameLabel = '&Имя пользователя:';
  SPasswordLabel = '&Пароль:';
  SInvalidUserName = 'Неверное имя пользователя или пароль';
  SChangePassword = 'Смена пароля';
  SOldPasswordLabel = '&Старый пароль:';
  SNewPasswordLabel = '&Новый пароль:';
  SConfirmPasswordLabel = '&Подтверждение:';
  SPasswordChanged = 'Пароль сменен';
  SPasswordNotChanged = 'Пароль не сменен';
  SPasswordsMismatch = 'Новый пароль и подтверждение не совпадают';
  SDBExceptCaption = 'Ошибка процессора БД';
  SBDEErrorLabel = 'Ошибка BDE';
  SServerErrorLabel = 'Ошибка сервера';
  SErrorMsgLabel = 'Сообщение об ошибке';
  SNextButton = '&Дальше';
  SPrevButton = '&Назад';
  SExprIncorrect = 'Некорректно сформулировано выражение фильтра';
  SExprTermination = 'Неверное завершение выражения фильтра';
  SExprNameError = 'Невозможно определить завершение имени поля';
  SExprStringError = 'Невозможно определить завершение строковой константы';
  SExprInvalidChar = 'Неверный символ в выражении фильтра: ''%s''';
  SExprNoRParen = 'Ожидалось '')'', а встречено: %s';
  SExprExpected = 'Ожидалось выражение, а встречено %s';
  SExprBadCompare = 'Операции сравнения требуют наличия поля и константы';
  SConfirmSave = 'Данные были изменены. Сохранять?';
  SDatabaseName = 'База данных: %s';
  SUnlockCaption = 'Разблокирование';
  SUnlockHint = 'Введите ваш пароль';
  SDeleteMultipleRecords = 'Удалить все выбранные записи?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Невозможно изменить размер изображения GIF';
  SNoGIFData = 'Нет данных для записи в формате GIF';
  SUnrecognizedGIFExt = 'Неопределенное расширение GIF: %.2x';
  SWrongGIFColors = 'Неверное число цветов; должно быть степенью числа 2';
  SBadGIFCodeSize = 'Размер кода GIF не принадлежит диапазону от 2 до 9';
  SGIFDecodeError = 'Неверные данные в формате GIF';
  SGIFEncodeError = 'Ошибка кодировки данных в формат GIF';
  SGIFVersion = 'Неизвестная версия изображения GIF';
  {RXTConst}
  SDualListSrcCaption = '&Источник';
  SDualListDestCaption = '&Приемник';
  SClipbrdUnknown = 'Неизвестный формат данных в буфере обмена';
  SClipbrdEmpty = 'Буфер обмена пуст';
  SCustomizeSpeedbar = 'Настройка панели инструментов';
  SAvailButtons = '&Доступные кнопки:';
  SSpeedbarCategories = '&Категории:';
  SSpeedbarEditHint = 'Выберите кнопку и перенесите ее с помощью мыши на нужное место на панели инструментов.';
  SParseSyntaxError = 'Синтаксическая ошибка';
  SParseNotCramp = 'Не хватает скобки';
  SParseDivideByZero = 'Деление на нуль';
  SParseSqrError = 'Выражение под корнем меньше нуля';
  SParseLogError = 'Выражение под логарифмом меньше нуля';
  SParseInvalidFloatOperation = 'Ошибка в операции с плавающей точкой';

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
  SColorBlack = 'Черный';
  SColorMaroon = 'Бордовый';
  SColorGreen = 'Зеленый';
  SColorOlive = 'Оливковый';
  SColorNavy = 'Темно-синий';
  SColorPurple = 'Фиолетовый';
  SColorTeal = 'Бирюзовый';
  SColorGray = 'Серый';
  SColorSilver = 'Светло-серый';
  SColorRed = 'Красный';
  SColorLime = 'Светло-зеленый';
  SColorYellow = 'Желтый';
  SColorBlue = 'Синий';
  SColorFuchsia = 'Сиреневый';
  SColorAqua = 'Голубой';
  SColorWhite = 'Белый';
  SColorScrollBar = 'Полоса прокрутки';
  SColorBackground = 'Рабочая область приложения';
  SColorActiveCaption = 'Заголовок активного окна';
  SColorInactiveCaption = 'Заголовок неактивного окна';
  SColorMenu = 'Строка меню';
  SColorWindow = 'Окно';
  SColorWindowFrame = 'Область окна';
  SColorMenuText = 'Текст меню';
  SColorWindowText = 'Текст в окне';
  SColorCaptionText = 'Текст заголовка';
  SColorActiveBorder = 'Граница активного окна';
  SColorInactiveBorder = 'Граница неактивного окна';
  SColorAppWorkSpace = 'Рабочая область приложения';
  SColorHighlight = 'Выделенный пункт меню';
  SColorHighlightText = 'Текст выделенного пункта меню';
  SColorBtnFace = 'Кнопки управления окном';
  SColorBtnShadow = 'Тень кнопки управления окном';
  SColorGrayText = 'Невыделенный текст';
  SColorBtnText = 'Текст кнопки управления окном';
  SColorInactiveCaptionText = 'Текст неактивного окна';
  SColorBtnHighlight = 'Выделенная кнопка';
  SColor3DDkShadow = 'Тень рельефного объекта';
  SColor3DLight = 'Рельефные объекты';
  SColorInfoText = 'Текст всплывающей подсказки';
  SColorInfoBk = 'Всплывающая подсказка';
  SColorCream = 'Кремовый';
  SColorMoneyGreen = 'Валюта';
  SColorSkyBlue = 'Небесный';
  SColorNone = 'Нет';
  SColorDefault = 'По умолчанию';
  SColorCustom = 'Другой...';
  {DS}
  SMemNoRecords = 'No data found';
  SInvalidFields = 'No fields defined';
  {Speedbar exception}
  SAutoSpeedbarMode = 'Cannot set this property value while Position is bpAuto';

implementation
end.