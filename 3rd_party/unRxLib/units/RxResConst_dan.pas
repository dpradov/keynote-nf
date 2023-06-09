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
unit RxResConst_dan;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Gennemse';
  SDefaultFilter = 'Alle filer (*.*)|*.*';
  SDateDlgTitle = 'Vælg en dato';
  SNextYear = 'Næste år|';
  SNextMonth = 'Næste måned|';
  SPrevYear = 'Forrige år|';
  SPrevMonth = 'Forrige måned|';
  SNotImplemented = 'Funktionen er ikke implementeret';
  SFileNotExec = 'Den angivne fil er ikke eksekverbar, et dynamisk hentet bibliotek eller en ikon-fil';
  SLoadLibError = 'Kunne ikke indlæse det dynamiske bibliotek ''%s''';
  SDetails = 'Detaljer';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Kan ikke udføre denne operation på en lokal database';
  SRetryLogin = 'Forsøg etablering af forbindelse til databasen igen?';
  SExprNotBoolean = 'Feltet ''%s'' er ikke af typen Boolean';
  SExprBadNullTest = 'NULL er kun tillad med operatorerne ''='' og ''<>''';
  SExprBadField = 'Feltet ''%s'' kan ikke benyttes i et filter-udtryk';
  SCaptureFilter = 'Kan ikke udføre denne operation når kontrollerne er indfanget';
  SNotCaptureFilter = 'Kan ikke udføre denne operation når kontrollerne ikke er indfanget';
  SInactiveData = 'Lukket';
  SBrowseData = 'Gennemse';
  SEditData = 'Rediger';
  SInsertData = 'Indsæt';
  SSetKeyData = 'Søg';
  SCalcFieldsData = 'Beregn';
  SRegistration = 'Registrering';
  SAppTitleLabel = 'Applikation *%s*';
  SHintLabel = 'Indtast dit brugernavn og adgangskode';
  SUserNameLabel = '&Brugernavn:';
  SPasswordLabel = '&Adgangskode:';
  SInvalidUserName = 'Ugyldigt brugernavn eller forkert adgangskode';
  SChangePassword = 'Skift adgangskode';
  SOldPasswordLabel = '&Gammel adgangskode:';
  SNewPasswordLabel = '&Ny adgangskode:';
  SConfirmPasswordLabel = '&Bekræft adgangskode:';
  SPasswordChanged = 'Adgangskoden er ændret';
  SPasswordNotChanged = 'Adgangskoden er ikke ændret';
  SPasswordsMismatch = 'De angivne adgangskoder er ikke ens';
  SDBExceptCaption = 'Databasefejl';
  SBDEErrorLabel = 'BDE fejl';
  SServerErrorLabel = 'Serverfejl';
  SErrorMsgLabel = 'Fejlmeddelelse';
  SNextButton = '&Næste';
  SPrevButton = '&Forrige';
  SExprIncorrect = 'Ugyldigt filter-udtryk';
  SExprTermination = 'Forkert afsluttet filter-udtryk';
  SExprNameError = 'Uafsluttet feltnavn';
  SExprStringError = 'Uafsluttet streng';
  SExprInvalidChar = 'Ugyldigt tegn i filterudtrykket: ''%s''';
  SExprNoRParen = 'Forventede '')'' men fandt %s';
  SExprExpected = 'Forventede udtryk men fandt %s';
  SExprBadCompare = 'Relationelle operatorer kræver angivelse af et felt og en konstant';
  SConfirmSave = 'Der er sket ændringer. Gem ændringerne?';
  SDatabaseName = 'Databasenavn: %s';
  SUnlockCaption = 'Lås applikationen op';
  SUnlockHint = 'Indtast din adgangskode';
  SDeleteMultipleRecords = 'Slet alle valgte poster?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF-billede';
  SChangeGIFSize = 'Kan ikke ændre størrelsen på et GIF-billede';
  SNoGIFData = 'Ingen GIF-data';
  SUnrecognizedGIFExt = 'Ukendt udvidelsesblok: %.2x';
  SWrongGIFColors = 'Forkert antal farver; skal være en potens af 2';
  SBadGIFCodeSize = 'GIF-koden er ikke mellem 2 og 9';
  SGIFDecodeError = 'Fejl i GIF-data';
  SGIFEncodeError = 'Fejl i GIF-data';
  SGIFVersion = 'Ukendt GIF-version';
  {RXTConst}
  SDualListSrcCaption = '&Kilde';
  SDualListDestCaption = '&Destination';
  SClipbrdUnknown = 'Kan ikke vises. Data i udklipsbrættet er i et ukendt format.';
  SClipbrdEmpty = 'Udklipsbrættet er tomt';
  SCustomizeSpeedbar = 'Tilpas værktøjslinje';
  SAvailButtons = '&Tilgængelige knapper:';
  SSpeedbarCategories = '&Kategorier:';
  SSpeedbarEditHint = 'Træk og slip knapper på værktøjslinjen for at tilføje kommandoer. Træk knapper af værktøjslinjen for at fjerne kommandoer.';
  SParseSyntaxError = 'Syntaksfejl';
  SParseNotCramp = 'Ugyldig tilstand (ingen cramp)';
  SParseDivideByZero = 'Division med nul';
  SParseSqrError = 'Fejl ved beregning af kvadratrod';
  SParseLogError = 'Fejl ved beregning af logaritme';
  SParseInvalidFloatOperation = 'Ugyldig kommatalsoperation';

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