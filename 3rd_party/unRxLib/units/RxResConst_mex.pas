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
unit RxResConst_mex;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Localiza';
  SDefaultFilter = 'Todos los Archivos (*.*)|*.*';
  SDateDlgTitle = 'Seleccione un Día';
  SNextYear = 'Próximo Año|';
  SNextMonth = 'Próximo Mes|';
  SPrevYear = 'Año Anterior|';
  SPrevMonth = 'Mes Anterior|';
  SNotImplemented = 'Función todavía no implementada';
  SFileNotExec = 'El archivo especificado no es ejecutable, una librería de enlace dinámico, o un archivo de íconos';
  SLoadLibError = 'No se pudo cargar la librería ''%s''';
  SDetails = 'Detalles';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'No se puede realizar esta operación en una base de datos local';
  SRetryLogin = 'Desea reintentar la conexión con la base de datos?';
  SExprNotBoolean = 'El campo ''%s'' no es de tipo Lógico';
  SExprBadNullTest = 'Sólo se permite el valor NULO con ''='' y ''<>''';
  SExprBadField = 'El campo ''%s'' no pude utilizarse en una expresión de filtro';
  SCaptureFilter = 'No se pude realizar esta operación cuando los controles están capturados';
  SNotCaptureFilter = 'No se pude realizar esta operación cuando los controles no están capturados';
  SInactiveData = 'cerrado';
  SBrowseData = 'localiza';
  SEditData = 'editar';
  SInsertData = 'insertar';
  SSetKeyData = 'definir tecla';
  SCalcFieldsData = 'calcular campos';
  SRegistration = 'Registro';
  SAppTitleLabel = 'Aplicación *%s*';
  SHintLabel = 'Proporcione su nombre de usuario y su contraseña';
  SUserNameLabel = '&Nombre de Usuario:';
  SPasswordLabel = '&Contraseña:';
  SInvalidUserName = 'Nombre de usuario o contraseña invalida';
  SChangePassword = 'Cambiar contraseña ';
  SOldPasswordLabel = 'Contraseña &Anterior:';
  SNewPasswordLabel = '&Nueva contraseña:';
  SConfirmPasswordLabel = '&Confirmar contraseña:';
  SPasswordChanged = 'La contraseña ha sido cambiada';
  SPasswordNotChanged = 'La contraseña no ha sido cambiada';
  SPasswordsMismatch = 'La contraseña no coincide con la confirmación';
  SDBExceptCaption = 'Error de la Base de Datos';
  SBDEErrorLabel = 'Error del BDE';
  SServerErrorLabel = 'Error del Servidor';
  SErrorMsgLabel = 'Mensaje de Error';
  SNextButton = '&Próximo';
  SPrevButton = '&Anterior';
  SExprIncorrect = 'Expresión incorrecta del filtro';
  SExprTermination = 'Expresión del filtro terminada incorrectamente';
  SExprNameError = 'Nombre de campo no definido';
  SExprStringError = 'Constante de caracteres no definida';
  SExprInvalidChar = 'Carácter inválido en la expresión de filtro: ''%s''';
  SExprNoRParen = ''')'' esperado pero ''%s'' encontrado';
  SExprExpected = 'Expresión esperada pero ''%s'' encontrada';
  SExprBadCompare = 'El Operador Relacional requiere de un campo y una constante';
  SConfirmSave = 'Los datos han cambiado. ¿Desea guardarlos?';
  SDatabaseName = 'Nombre de la Base de Datos: %s';
  SUnlockCaption = 'Aplicación no bloqueada';
  SUnlockHint = 'Proporcione su contraseña';
  SDeleteMultipleRecords = '¿Desea eliminar todos los registros seleccionados?';

  {RXGConst}
  SGIFImage = 'Imagen GIF de CompuServe';
  SChangeGIFSize = 'No se puede cambiar el tamaño de una imagen GIF';
  SNoGIFData = 'No hay datos para guardar una imagen GIF';
  SUnrecognizedGIFExt = 'Extensión de bloque desconocida: %.2x';
  SWrongGIFColors = 'No. de errores equivocado; debe ser una potencia de 2';
  SBadGIFCodeSize = 'El tamaño del código del GIF no está entre 2 y 9';
  SGIFDecodeError = 'Codificación de datos del GIF corrupta';
  SGIFEncodeError = 'Error de codificación de imagen GIF';
  SGIFVersion = 'Versión del GIF desconocida';

  {RXTConst}
  SDualListSrcCaption = '&Fuente';
  SDualListDestCaption = '&Destino';
  SClipbrdUnknown = 'No se puede mostrar el contenido del Portapapeles. Formato desconocido.';
  SClipbrdEmpty = 'El Portapapeles está vacío';
  SCustomizeSpeedbar = 'Redibujar la Barra de Herramientas';
  SAvailButtons = '&Botones disponibles:';
  SSpeedbarCategories = '&Categorías:';
  SSpeedbarEditHint = 'Para agregar un botón, arrastre y suelte el botón en la Barra de Herramientas. Para eliminar un botón, arrastre y suelte el botón fuera de la Barra de Herramientas.';
  SParseSyntaxError = 'Error de sintaxis';
  SParseNotCramp = 'Condición inválida (sin contracción)';
  SParseDivideByZero = 'División entre cero';
  SParseSqrError = 'Raíz cuadrada inválida';
  SParseLogError = 'Operación logarítmica inválida';
  SParseInvalidFloatOperation = 'Operación de punto flotante inválida';

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