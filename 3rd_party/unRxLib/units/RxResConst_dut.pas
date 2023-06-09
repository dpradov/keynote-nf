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
unit RxResConst_dut;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Bladeren';
  SDefaultFilter = 'Alle bestanden (*.*)|*.*';
  SDateDlgTitle = 'Selecteer een Datum';
  SNextYear = 'Volgende Jaar|';
  SNextMonth = 'Volgende Maand|';
  SPrevYear = 'Vorige Jaar|';
  SPrevMonth = 'Vorige Maand|';
  SNotImplemented = 'Functie is nog niet geïmplementeerd';
  SFileNotExec = 'Opgegeven bestand is geen executable, dynamic-link library of icon bestand';
  SLoadLibError = 'Kon bibliotheek ''%s'' niet laden';
  SDetails = 'Details';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Kan deze handeling niet uitvoeren op een lokale database';
  SRetryLogin = 'Wilt u de verbinding met de database proberen te herstellen?';
  SExprNotBoolean = 'Veld ''%s'' is geen Boolean type';
  SExprBadNullTest = 'NULL alleen toegestaan met ''='' en ''<>''';
  SExprBadField = 'Veld ''%s'' kan niet worden gebruikt in een filter expressie';
  SCaptureFilter = 'Kan deze handeling niet verrichten als besturing ''vast'' zit';
  SNotCaptureFilter = 'Kan deze handeling niet verrichten als besturing niet ''vast'' zit';
  SInactiveData = 'Gesloten';
  SBrowseData = 'Bladeren';
  SEditData = 'Wijzigen';
  SInsertData = 'Toevoegen';
  SSetKeyData = 'Zoeken';
  SCalcFieldsData = 'Berekenen';
  SRegistration = 'Registratie';
  SAppTitleLabel = 'Applicatie *%s*';
  SHintLabel = 'Vul uw gebruikersnaam en wachtwoord in';
  SUserNameLabel = '&Gebruikersnaam:';
  SPasswordLabel = '&Wachtwoord:';
  SInvalidUserName = 'Ongeldige gebruikersnaam of wachtwoord';
  SChangePassword = 'Wijzig wachtwoord';
  SOldPasswordLabel = '&Oud wachtwoord:';
  SNewPasswordLabel = '&Nieuw wachtwoord:';
  SConfirmPasswordLabel = '&Bevestig nieuw wachtwoord:';
  SPasswordChanged = 'Wachtwoord is gewijzigd';
  SPasswordNotChanged = 'Wachtwoord is niet gewijzigd';
  SPasswordsMismatch = 'Bevestiging van nieuw wachtwoord komt niet overeen';
  SDBExceptCaption = 'Database Engine Fout';
  SBDEErrorLabel = 'BDE Fout';
  SServerErrorLabel = 'Server Fout';
  SErrorMsgLabel = 'Fout melding';
  SNextButton = 'Vol&gende';
  SPrevButton = '&Vorige';
  SExprIncorrect = 'Foutief geformuleerde filter expressie';
  SExprTermination = 'Filter expressie foutief beëindigd  ';
  SExprNameError = 'Onvolledige veldnaam';
  SExprStringError = 'Onvolledige string constante';
  SExprInvalidChar = 'Ongeldige filter expressieteken: ''%s''';
  SExprNoRParen = ''')'' verwacht maar %s gevonden';
  SExprExpected = 'Expressie verwacht maar %s gevonden';
  SExprBadCompare = 'Relatie operatoren hebben een veld en een constante nodig';
  SConfirmSave = 'De gegevens zijn gewijzigd. Gegevens opslaan?';
  SDatabaseName = 'Database naam: %s';
  SUnlockCaption = 'Unlock applicatie';
  SUnlockHint = 'Vul uw wachtwoord in';
  SDeleteMultipleRecords = 'Alle geselecteerde records verwijderen?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF Image';
  SChangeGIFSize = 'Kan de grootte van een GIF afbeelding niet wijzigen';
  SNoGIFData = 'Geen GIF gegevens om te schrijven';
  SUnrecognizedGIFExt = 'Onbekend extensie blok: %.2x';
  SWrongGIFColors = 'Foutief aantal kleuren; moet een expressie tot de macht 2 zijn';
  SBadGIFCodeSize = 'GIF kode grootte niet in bereik 2 tot 9';
  SGIFDecodeError = 'GIF gekodeerde gegevens zijn corrupt';
  SGIFEncodeError = 'GIF afbeelding koderingsfout';
  SGIFVersion = 'Onbekend GIF versie';
  {RXTConst}
  SDualListSrcCaption = '&Bron';
  SDualListDestCaption = '&Doel';
  SClipbrdUnknown = 'Kan niet tonen. Gegevens in Kladblok zijn van een onbekend type.';
  SClipbrdEmpty = 'Kladblok is leeg';
  SCustomizeSpeedbar = 'Taakbalk aanpassen';
  SAvailButtons = '&Beschikbare taken:';
  SSpeedbarCategories = '&Categorieën :';
  SSpeedbarEditHint = 'Om taken toe te voegen, sleep deze naar de taakbalk. Om taken te verwijderen, sleep deze van de taakbalk.';
  SParseSyntaxError = 'Syntax fout';
  SParseNotCramp = 'Ongeldige voorwaarde';
  SParseDivideByZero = 'Delen door nul';
  SParseSqrError = 'Ongeldige zwevende komma handeling';
  SParseLogError = 'Ongeldige zwevende komma handeling';
  SParseInvalidFloatOperation = 'Ongeldige zwevende komma handeling';
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