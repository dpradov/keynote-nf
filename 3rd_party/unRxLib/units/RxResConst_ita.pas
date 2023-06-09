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
unit RxResConst_ita;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Seleziona file';
  SDefaultFilter = 'Tutti i files (*.*)|*.*';
  SDateDlgTitle = 'Seleziona una data';
  SNextYear = 'Anno successivo|';
  SNextMonth = 'Mese successivo|';
  SPrevYear = 'Anno precedente|';
  SPrevMonth = 'Mese precedente|';
  SNotImplemented = 'Funzione non ancora implementata';
  SFileNotExec = 'Il file specificato non è un eseguibile, una dll, o una icona';
  SLoadLibError = 'Impossibile caricare la libreria: ''%s''';
  SDetails = 'Dettagli';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Impossibile portare a termine questa operazione su un database locale';
  SRetryLogin = 'Vuoi ritentare la connessione al database?';
  SExprNotBoolean = 'Il campo ''%s'' non è di tipo Booleano';
  SExprBadNullTest = 'Solo NULL è consentito con ''='' e ''<>''';
  SExprBadField = 'Il campo ''%s'' non può essere usato in una espressione di filtro';
  SCaptureFilter = 'Impossibile portare a termine questa operazione quando i controlli sono bloccati';
  SNotCaptureFilter = 'Impossibile portare a termine questa operazione quando i controlli non sono bloccati';
  SInactiveData = 'chiuso';
  SBrowseData = 'browse';
  SEditData = 'modifica';
  SInsertData = 'inserimento';
  SSetKeyData = 'setta chiavi ricerca';
  SCalcFieldsData = 'campi calcolati';
  SRegistration = 'Registration';
  SAppTitleLabel = 'Applicazione *%s*';
  SHintLabel = 'Inserire nome utente e password';
  SUserNameLabel = 'Nome &Utente:';
  SPasswordLabel = '&Password:';
  SInvalidUserName = 'Nome utente o password non valide';
  SChangePassword = 'Cambia password';
  SOldPasswordLabel = '&Vecchia password:';
  SNewPasswordLabel = '&Nuova password:';
  SConfirmPasswordLabel = '&Conferma password:';
  SPasswordChanged = 'La Password è stata modificata';
  SPasswordNotChanged = 'La Password non è stata modificata';
  SPasswordsMismatch = 'La nuova passoword non corrisponde alla password di conferma';
  SDBExceptCaption = 'Errore del motore Database';
  SBDEErrorLabel = 'Errore del BDE';
  SServerErrorLabel = 'Errore del server';
  SErrorMsgLabel = 'Messaggio di errore';
  SNextButton = '&Successivo';
  SPrevButton = '&Precedente';
  SExprIncorrect = 'Espressione filtro non corretta';
  SExprTermination = 'Espressione filtro terminata scorrettamente';
  SExprNameError = 'Nome campo non terminato';
  SExprStringError = 'Stringa non terminata';
  SExprInvalidChar = 'Carattere non valido per una espressione filtro: ''%s''';
  SExprNoRParen = ''')'' attesa ma %s trovata';
  SExprExpected = 'Attesa una espressione ma %s trovata';
  SExprBadCompare = 'Gli operatori relazionali richiedono un campo ed una costante';
  SConfirmSave = 'I dati sono combiati. Salvarli?';
  SDatabaseName = 'Nome Database: %s';
  SUnlockCaption = 'Applicazione sbloccata';
  SUnlockHint = 'Digitare password';
  SDeleteMultipleRecords = 'Cancellare tutti i record selezionati?';

  {RXGConst}
  SGIFImage = 'Immagine GIF di CompuServe';
  SChangeGIFSize = 'Impossibile cambiare le dimensioni di una immagine GIF';
  SNoGIFData = 'Nessun dato GIF da scrivere';
  SUnrecognizedGIFExt = 'Estensione blocco non riconosciuta: %.2x';
  SWrongGIFColors = 'Numero di colori errato; deve essere potenza di due 2';
  SBadGIFCodeSize = 'Dimensione codice GIF fuori intervallo: da 2 a 9';
  SGIFDecodeError = 'Dati GIF corrotti, decodifica impossibile';
  SGIFEncodeError = 'Errore durante al codifica GIF';
  SGIFVersion = 'Versione GIF sconosciuta';

  {RXTConst}
  SDualListSrcCaption = '&Dati disponibili';
  SDualListDestCaption = 'Dati &selezionati';
  SClipbrdUnknown = 'Impossibile mostrare. I dati negli appunti sono in un formato sconosciuto';
  SClipbrdEmpty = 'Gli appunti sono vuoti';
  SCustomizeSpeedbar = 'Personalizza barra degli strumenti';
  SAvailButtons = '&Pulsanti:';
  SSpeedbarCategories = '&Gruppi:';
  SSpeedbarEditHint = 'Per aggiungere un pulsante, trascinarlo sulla barra degli strumenti. Per eliminare un pulsante trascinarlo fuori dalla barra degli strumenti.';
  SParseSyntaxError = 'Errore di sintassi';
  SParseNotCramp = 'Condizione non valida (no cramp)';
  SParseDivideByZero = 'Divisione per zero';
  SParseSqrError = 'Operazione in virgola mobile non valida';
  SParseLogError = 'Operazione in virgola mobile non valida';
  SParseInvalidFloatOperation = 'Operazione in virgola mobile non valida';

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