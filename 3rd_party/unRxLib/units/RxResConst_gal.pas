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
unit RxResConst_gal;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Examinar';
  SDefaultFilter = 'Tódolos arquivos (*.*)|*.*';
  SDateDlgTitle = 'Seleccione unha data';
  SNextYear = 'Seguinte ano|';
  SNextMonth = 'Seguinte mes|';
  SPrevYear = 'Ano anterior|';
  SPrevMonth = 'Mes anterior|';
  SNotImplemented = 'Función aínda non implementada';
  SFileNotExec = 'O arquivo especificado non é un executable, nin unha biblioteca de vínculo dinámico, nin un icono.';
  SLoadLibError = 'Non se puido carga-la biblioteca ''%s''';
  SDetails = 'Detalles';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Non se pode realizar esa operación nunha base de datos local';
  SRetryLogin = '¿Desexa retenta-la conexión coa base de datos?';
  SExprNotBoolean = 'O campo ''%s'' non é do tipo Boolean';
  SExprBadNullTest = 'NULL só se permite con ''='' e ''<>''';
  SExprBadField = 'O campo ''%s'' non se pode utilizar nunha expresión de filtrado';
  SCaptureFilter = 'Non se pode realizar esa operación cando os controis están capturados';
  SNotCaptureFilter = 'Non se pode realizar esa operación cando os controis non están capturados';
  SInactiveData = 'closed';
  SBrowseData = 'browse';
  SEditData = 'edit';
  SInsertData = 'insert';
  SSetKeyData = 'set key';
  SCalcFieldsData = 'calc fields';
  SRegistration = 'Rexistro';
  SAppTitleLabel = 'Aplicación *%s*';
  SHintLabel = 'Escriba o seu nome de usuario e contrasinal';
  SUserNameLabel = 'Nome de &Usuario:';
  SPasswordLabel = '&Contrasinal:';
  SInvalidUserName = 'Nome de usuario ou contrasinal non válidos';
  SChangePassword = 'Muda-lo contrasinal';
  SOldPasswordLabel = '&Antigo contrasinal:';
  SNewPasswordLabel = '&Novo contrasinal:';
  SConfirmPasswordLabel = '&Confirme o contrasinal:';
  SPasswordChanged = 'Contrasinal mudado';
  SPasswordNotChanged = 'Contrasinal non mudado';
  SPasswordsMismatch = 'Os contrasinais novo e confirmado non concordan';
  SDBExceptCaption = 'Erro da ''Database Engine''';
  SBDEErrorLabel = 'Erro da BDE';
  SServerErrorLabel = 'Erro no servidor';
  SErrorMsgLabel = 'Mensaxe de erro';
  SNextButton = '&Seg';
  SPrevButton = '&Ant';
  SExprIncorrect = 'Expresión de filtrado incorrecta';
  SExprTermination = 'Expresión de filtrado rematada incorrectamente';
  SExprNameError = 'Nome de campo incompleto';
  SExprStringError = 'Constante de tipo cadea incompleta';
  SExprInvalidChar = 'Carácter na expresión de filtrado non válido: ''%s''';
  SExprNoRParen = ''')'' agardado pero achouse %s';
  SExprExpected = 'Expresión agardada pero achouse %s';
  SExprBadCompare = 'Os operadores relacionais requiren un campo e unha constante';
  SConfirmSave = 'Os datos mudaron. ¿Gárdanse?';
  SDatabaseName = 'Nome da base de datos: %s';
  SUnlockCaption = 'Despeche a aplicación';
  SUnlockHint = 'Escriba o seu contrasinal';
  SDeleteMultipleRecords = '¿Borrar tódolos rexistros seleccionados?';

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
  SDualListSrcCaption = '&Fonte';
  SDualListDestCaption = '&Destino';
  SClipbrdUnknown = 'Non se pode amosar. Os datos do cartafol son de formato descoñecido.';
  SClipbrdEmpty = 'O cartafol está baleiro';
  SCustomizeSpeedbar = 'Personaliza-la barra de tarefas';
  SAvailButtons = '&Iconos dispoñibles:';
  SSpeedbarCategories = '&Categorías:';
  SSpeedbarEditHint = 'Para añadir iconos, arrastre e solte os botóns na barra de tarefas. Para sacalos, arrástreos fóra da barra.';
  SParseSyntaxError = 'Erro de sintaxe';
  SParseNotCramp = 'Condición non válida (sen liame)';
  SParseDivideByZero = 'División por cero';
  SParseSqrError = 'Operación de coma flotante non válida';
  SParseLogError = 'Operación de coma flotante non válida';
  SParseInvalidFloatOperation = 'Operación de coma flotante non válida';
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