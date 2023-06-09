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
unit RxResConst_rom;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Răsfoire';
  SDefaultFilter = 'Toate fişierele (*.*)|*.*';
  SDateDlgTitle = 'Selectaţi o dată';
  SNextYear = 'Anul următor|';
  SNextMonth = 'Luna următoare|';
  SPrevYear = 'Anul anterior|';
  SPrevMonth = 'Luna anterioară|';
  SNotImplemented = 'Funcţia nu este încă implementată';
  SFileNotExec = 'Fişierul specificat nu este un fişier executabil, o bibliotecă DLL sau o icoană';
  SLoadLibError = 'Nu pot să încarc biblioteca ''%s''';
  SDetails = 'Detalii';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Nu pot să efectuez operaţia cerută pe o bază de date locală';
  SRetryLogin = 'Mai vreţi să încercăm conectarea la baza de date?';
  SExprNotBoolean = 'Câmpul ''%s'' nu este de tip logic.';
  SExprBadNullTest = 'NULL este permis doar cu ''='' şi ''<>''';
  SExprBadField = 'Câmpul ''%s'' nu poate fi folosit într-o expresie de filtrare';
  SCaptureFilter = 'Nu pot să efectuez operaţia cerută atunci când obiectele sunt capturate';
  SNotCaptureFilter = 'Nu pot să efectuez operaţia cerută atunci când obiectele nu sunt capturate';
  SInactiveData = 'Închis';
  SBrowseData = 'Răsfoire';
  SEditData = 'Editare';
  SInsertData = 'Inserare';
  SSetKeyData = 'Căutare';
  SCalcFieldsData = 'Calculare';
  SRegistration = 'Înregistrare';
  SAppTitleLabel = 'Aplicaţia *%s*';
  SHintLabel = 'Introduceţi numele şi parola';
  SUserNameLabel = '&Numele:';
  SPasswordLabel = '&Parola:';
  SInvalidUserName = 'Nume sau parolă greşite.';
  SChangePassword = 'Schimbare parolă';
  SOldPasswordLabel = '&Parola veche:';
  SNewPasswordLabel = '&Parola nouă:';
  SConfirmPasswordLabel = '&Confirmaţi parola:';
  SPasswordChanged = 'Parola a fost schimbată';
  SPasswordNotChanged = 'Parola nu a fost schimbată';
  SPasswordsMismatch = 'Parola nouă şi cea confirmată nu se potrivesc';
  SDBExceptCaption = 'Eroare a motorului de baze de date';
  SBDEErrorLabel = 'Eroare BDE';
  SServerErrorLabel = 'Eroare server';
  SErrorMsgLabel = 'Mesaj de eroare';
  SNextButton = '&Următorul';
  SPrevButton = '&Anteriorul';
  SExprIncorrect = 'Expresie de filtrare formată incorect';
  SExprTermination = 'Expresie de filtrare terminată incorect';
  SExprNameError = 'Nume de câmp neterminat';
  SExprStringError = 'Constantă şir neterminată';
  SExprInvalidChar = 'Expresie de filtrate de tip caracter invalidă: ''%s''';
  SExprNoRParen = 'Aştept '')'' dar am găsit %s';
  SExprExpected = 'Aştept o expresie dar am găsit %s';
  SExprBadCompare = 'Operatorii relaţionali necesită un câmp şi o constantă';
  SConfirmSave = 'Datele au fost schimbate. Le salvăm?';
  SDatabaseName = 'Numele bazei de date: %s';
  SUnlockCaption = 'Deblocare aplicaţie';
  SUnlockHint = 'Introduceţi parola dvs.';
  SDeleteMultipleRecords = 'Ştergem toate înregistrările selectate?';

  {RXGConst}
  SGIFImage = 'Imagine GIF';
  SChangeGIFSize = 'Nu pot să schimb dimensiunea unei imagini GIF';
  SNoGIFData = 'Nu sunt date GIF de scris';
  SUnrecognizedGIFExt = 'Bloc de extensie nerecunoscut: %.2x';
  SWrongGIFColors = 'Număr de culori greşit; trebuie să fie o putere a lui 2';
  SBadGIFCodeSize = 'Dimensiunea codului GIF nu e în intervalul 2 până la 9';
  SGIFDecodeError = 'Datele codate din GIF sunt corupte';
  SGIFEncodeError = 'Eroare de codare a imaginii GIF';
  SGIFVersion = 'Versiune GIF necunoscută';

  {RXTConst}
  SDualListSrcCaption = '&Sursa';
  SDualListDestCaption = '&Destinaţia';
  SClipbrdUnknown = 'Nu pot să afişez. Datele din Clipboard sunt într-un format necunoscut.';
  SClipbrdEmpty = 'Clipboard-ul este gol.';
  SCustomizeSpeedbar = 'Personalizarea barei de butoane';
  SAvailButtons = '&Butoane disponibile:';
  SSpeedbarCategories = '&Categorii:';
  SSpeedbarEditHint = 'Pentru a adăuga butoane, trageţi cu mouse-ul butoanele pe bară. Pentru a le îndepărta, trageţi-le jos de pe aceasta.';
  SParseSyntaxError = 'Eroare de sintaxă';
  SParseNotCramp = 'Condiţie invalidă (nu este tratată)';
  SParseDivideByZero = 'Împărţire cu zero';
  SParseSqrError = 'Operaţiune flotantă invalidă';
  SParseLogError = 'Operaţiune flotantă invalidă';
  SParseInvalidFloatOperation = 'Operaţiune flotantă invalidă';

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