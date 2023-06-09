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
unit RxResConst_spa;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Visualizar';
  SDefaultFilter = 'Todos los ficheros (*.*)|*.*';
  SDateDlgTitle = 'Seleccione un fecha';
  SNextYear = 'Año siguiente|';
  SNextMonth = 'Mes siguiente|';
  SPrevYear = 'Año anterior|';
  SPrevMonth = 'Mes anterior|';
  SNotImplemented = 'Función aún no implementada';
  SFileNotExec = 'El fichero indicado no es un ejecutable, ni una DLL ni un fichero de iconos';
  SLoadLibError = 'No se pudo cargar la librería ''%s'' ';
  SDetails = 'Detalles';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'No puedo ejecutar esta operación sobre una base de datos local';
  SRetryLogin = '¿Seguro que quiere reintentar la conexión con la base de datos?';
  SExprNotBoolean = 'El campo ''%s'' no es de tipo lógico';
  SExprBadNullTest = 'NULL sólo se permite con ''='' y ''<>''';
  SExprBadField = 'El campo ''%s'' no se puede usar en una expresión de filtro';
  SCaptureFilter = 'No se puede ejecutar esta operación mientras los controles están capturados';
  SNotCaptureFilter = 'No se puede ejecutar esta operación cuando los controles no están capturados';
  SInactiveData = 'Cerrado';
  SBrowseData = 'Visualizar';
  SEditData = 'Editar';
  SInsertData = 'Insertar';
  SSetKeyData = 'Buscar';
  SCalcFieldsData = 'Calcular';
  SRegistration = 'Operación de registro';
  SAppTitleLabel = 'Aplicación %s';
  SHintLabel = 'Teclee su nombre y contraseña';
  SUserNameLabel = 'Nombre de &usuario:';
  SPasswordLabel = '&Contraseña:';
  SInvalidUserName = 'Nombre de usuario o contraseña inválidos';
  SChangePassword = 'Cambiar contraseña';
  SOldPasswordLabel = 'Contraseña &Antigua:';
  SNewPasswordLabel = 'Contraseña &Nueva:';
  SConfirmPasswordLabel = '&Confirmar contraseña:';
  SPasswordChanged = 'La contraseña ha sido cambiada';
  SPasswordNotChanged = 'La contraseña no ha sido cambiada';
  SPasswordsMismatch = 'La contraseña nueva y la confirmada no concuerdan';
  SDBExceptCaption = 'Error del motor de datos';
  SBDEErrorLabel = 'Error del BDE';
  SServerErrorLabel = 'Error del Servidor';
  SErrorMsgLabel = 'Mensaje de Error';
  SNextButton = '&Siguiente';
  SPrevButton = '&Anterior';
  SExprIncorrect = 'Expresión del filtro incorrectamente formada';
  SExprTermination = 'Expresión del filtro incorrectamente terminada';
  SExprNameError = 'Nombre del campo sin acabar';
  SExprStringError = 'Literal alfanumérico inacabado';
  SExprInvalidChar = 'El caracter: ''%s'' es una expresión de filtro inválida';
  SExprNoRParen = 'Se esperaba un '')''  pero se encontró %s';
  SExprExpected = 'Se esperaba una expresión pero se encontró  %s';
  SExprBadCompare = 'Los operadores relacionales requieren un campo y una constante';
  SConfirmSave = 'Los datos han cambiado ¿Guardarlos?';
  SDatabaseName = 'Nombre de la base de datos: %s';
  SUnlockCaption = 'Aplicación no bloqueada';
  SUnlockHint = 'Teclee su contraseña';
  SDeleteMultipleRecords = '¿Borrar todos los registros seleccionados?';

  {RXGConst}
  SGIFImage = 'Imagen GIF de CompuServe';
  SChangeGIFSize = 'No se puede cambiar de tamaño a una imagen GIF';
  SNoGIFData = 'No hay datos que escribir en el GIF';
  SUnrecognizedGIFExt = 'Bloque de extensión no reconocido: %.2x';
  SWrongGIFColors = 'Número de colores incorrecto. Ha de ser una potencia de 2';
  SBadGIFCodeSize = 'El tamaño de ódigo del GIF no está comprendido entre 2 y 9';
  SGIFDecodeError = 'Los datos codificados del GIF están corruptos';
  SGIFEncodeError = 'Error de codificación de la imagen GIF';
  SGIFVersion = 'Versión GIF desconocida';

  {RXTConst}
  SDualListSrcCaption = '&Fuente';
  SDualListDestCaption = '&Destino';
  SClipbrdUnknown = 'No puedo mostrar nada. Los datos del portapapeles están en un formato desconocido';
  SClipbrdEmpty = 'El portapapeles está vacío';
  SCustomizeSpeedbar = 'Personalizar la barra de herramientas';
  SAvailButtons = '&Botones disponibles:';
  SSpeedbarCategories = '&Categorías:';
  SSpeedbarEditHint = 'Para añadir botones, arrástrelos y suéltelos sobre la barra de herramientas. Para quitarlos, arrástrelos fuera de la barra de herramientas';
  SParseSyntaxError = 'Error de sintaxis';
  SParseNotCramp = 'Condición inválida (sin soporte)';
  SParseDivideByZero = 'División entre 0';
  SParseSqrError = 'Operación de coma flotante inválida';
  SParseLogError = 'Operación de coma flotante inválida';
  SParseInvalidFloatOperation = 'Operación de coma flotante inválida';

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