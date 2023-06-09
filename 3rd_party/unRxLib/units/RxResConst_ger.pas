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
unit RxResConst_ger;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Durchsuchen';
  SDefaultFilter = 'Alle Dateien (*.*)|*.*';
  SDateDlgTitle = 'Datum auswählen';
  SNextYear = 'Nächstes Jahr|';
  SNextMonth = 'Nächster Monat|';
  SPrevYear = 'Vorhergehendes Jahr|';
  SPrevMonth = 'Vorhergehender Monat|';
  SNotImplemented = 'Funktion noch nicht implementiert';
  SFileNotExec = 'Die angegebene Datei ist keine EXE-, DLL- oder ICO-Datei';
  SLoadLibError = 'Die Bibliothek ''%s'' kann nicht geladen werden';
  SDetails = 'Details';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Operation bei lokalen Datenbanken nicht möglich';
  SRetryLogin = 'Verbindung zur Datenbank erneut aufbauen?';
  SExprNotBoolean = 'Das Feld ''%s'' ist nicht vom Typ Boolean';
  SExprBadNullTest = 'NULL nur mit ''='' und ''<>'' erlaubt';
  SExprBadField = 'Das Feld ''%s'' kann in einem Filterausdruck nicht verwendet werden';
  SCaptureFilter = 'Operation nicht möglich, Felder sind im Filter-Eingabemodus';
  SNotCaptureFilter = 'Operation nicht möglich, Felder sind nicht im Filter-Eingabemodus';
  SInactiveData = 'Geschlossen';
  SBrowseData = 'Blättern';
  SEditData = 'Bearbeiten';
  SInsertData = 'Einfügen';
  SSetKeyData = 'Suchen';
  SCalcFieldsData = 'Berechnen';
  SRegistration = 'Registrierung';
  SAppTitleLabel = 'Anwendung *%s*';
  SHintLabel = 'Benutzername und Paßwort eingeben';
  SUserNameLabel = '&Benutzername:';
  SPasswordLabel = '&Paßwort';
  SInvalidUserName = 'Falscher Benutzername oder falsches Paßwort';
  SChangePassword = 'Paßwort ändern';
  SOldPasswordLabel = '&Altes Paßwort:';
  SNewPasswordLabel = '&Neues Paßwort:';
  SConfirmPasswordLabel = '&Paßwort bestätigen:';
  SPasswordChanged = 'Das Paßwort wurde geändert';
  SPasswordNotChanged = 'Das Paßwort wurde nicht geändert';
  SPasswordsMismatch = 'Die beiden Paßwörter stimmen nicht überein';
  SDBExceptCaption = 'Datenbankfehler';
  SBDEErrorLabel = 'BDE-Fehler';
  SServerErrorLabel = 'Serverfehler';
  SErrorMsgLabel = 'Fehlermeldung';
  SNextButton = '&Weiter';
  SPrevButton = '&Zurück';
  SExprIncorrect = 'Filterausdruck ist nicht korrekt';
  SExprTermination = 'Filterausdruck wurde nicht korrekt beendet';
  SExprNameError = 'Feldname ist nicht korrekt beendet';
  SExprStringError = 'String Konstante ist nicht korrekt beendet';
  SExprInvalidChar = 'Ungültiges Zeichen im Filterausdruck: ''%s''';
  SExprNoRParen = '")" erwartet, aber %s gefunden';
  SExprExpected = 'Ausdruck erwartet, aber %s gefunden';
  SExprBadCompare = 'Relationale Operatoren benötigen ein Feld und eine Konstante';
  SConfirmSave = 'Die Daten wurden geändert, Änderungen speichern?';
  SDatabaseName = 'Datenbankname: %s';
  SUnlockCaption = 'Anwendung freigeben';
  SUnlockHint = 'Paßwort eingeben';
  SDeleteMultipleRecords = 'Alle markierten Datensätze löschen?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF Bild';
  SChangeGIFSize = 'Die Größe eines GIF Bildes kann nicht geändert werden';
  SNoGIFData = 'Keine GIF Daten zum schreiben';
  SUnrecognizedGIFExt = 'Unerkannter Erweiterungsblock: %.2x';
  SWrongGIFColors = 'Falsche Farbanzahl; muß eine Potenz von 2 sein';
  SBadGIFCodeSize = 'GIF Codegröße nicht im Bereich von 2 bis 9';
  SGIFDecodeError = 'GIF Bild enthält fehlerhafte Daten';
  SGIFEncodeError = 'GIF Bild Kodierungsfehler';
  SGIFVersion = 'Unbekannte GIF Version';

  {RXTConst}
  SDualListSrcCaption = '&Quelle';
  SDualListDestCaption = '&Ziel';
  SClipbrdUnknown = 'Anzeige ist nicht möglich. Unbekanntes Datenformat in der Zwischenablage';
  SClipbrdEmpty = 'Die Zwischenablage ist leer';
  SCustomizeSpeedbar = 'Symbolleisteneditor';
  SAvailButtons = '&Befehle:';
  SSpeedbarCategories = '&Kategorien:';
  SSpeedbarEditHint = 'Um Befehlsschalter hinzuzufügen, ziehen Sie die Befehle auf die Symbolleiste und legen sie dort ab. Um Sie zu entfernen, ziehen Sie diese von der Symbolleiste weg.';
  SParseSyntaxError = 'Syntax Fehler';
  SParseNotCramp = 'Ungültige Bedingung (Klammer fehlt)';
  SParseDivideByZero = 'Division durch 0';
  SParseSqrError = 'Wurzel aus negativer Zahl ist nicht definiert';
  SParseLogError = 'Logarithmen aus Null oder neg. Zahlen sind nicht definiert';
  SParseInvalidFloatOperation = 'Ungültige Fließkommaoperation';

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