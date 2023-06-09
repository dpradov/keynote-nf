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
unit RxResConst_gre;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Επισκόπηση';
  SDefaultFilter = 'Όλα τα αρχεία (*.*)|*.*';
  SDateDlgTitle = 'Επιλέξτε μία Ημερομηνία';
  SNextYear = 'Επόμενος Χρόνος|';
  SNextMonth = 'Επόμενος Μήνας|';
  SPrevYear = 'Προηγούμενος Χρόνος|';
  SPrevMonth = 'Προηγούμενος Μήνας|';
  SNotImplemented = 'Η Λειτουργία δεν έχει υλοποιηθεί ακόμα';
  SFileNotExec = 'Το συγκεκριμένο αρχείο δεν είναι εκτελέσιμο, DLL ή εικονίδιο';
  SLoadLibError = 'Δεν μπόρεσα να φορτώσω την ''%s'' βιβλιοθήκη';
  SDetails = 'Λεπτομέρειες';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Αδυνατώ να εκτελέσω αυτήν την πράξη σε μία τοπική βάση δεδομένων';
  SRetryLogin = 'Επιθυμείτε να ξαναεπιχειρήσω την σύνδεση με την βάση δεδομένων;';
  SExprNotBoolean = 'Το πεδίο ''%s'' δεν είναι τύπου Boolean (Λογικού)';
  SExprBadNullTest = 'Το NULL επιτέπεται μόνο με ''='' και με ''<>''';
  SExprBadField = 'Το πεδίο ''%s'' δεν μπορεί να χρησιμοποιηθεί σε μία έκφραση φίλτρου';
  SCaptureFilter = 'Αδυνατώ να εκτελέσω αυτήν την πράξη όταν τα controls είναι δεσμευμένα';
  SNotCaptureFilter = 'Αδυνατώ να εκτελέσω αυτήν την πράξη όταν τα controls δεν είναι δεσμευμένα';
  SInactiveData = 'Κλειστό';
  SBrowseData = 'Επισκόπηση';
  SEditData = 'Επεξεργασία';
  SInsertData = 'Εισαγωγή';
  SSetKeyData = 'Αναζήτηση';
  SCalcFieldsData = 'Υπολογισμός';
  SRegistration = 'Εγγραφή';
  SAppTitleLabel = 'Αίτηση *%s*';
  SHintLabel = 'Πληκτρολόγησε όνομα χρήστη και το συνθηματικό σου';
  SUserNameLabel = '&Όνομα χρήστη:';
  SPasswordLabel = '&Σύνθημα:';
  SInvalidUserName = 'Άκυρο όνομα χρήστη ή σύνθημα';
  SChangePassword = 'Αλλαγή συνθήματος';
  SOldPasswordLabel = '&Παλαιό σύνθημα:';
  SNewPasswordLabel = '&Νέο σύνθημα:';
  SConfirmPasswordLabel = '&Επιβεβαίωση συνθήματος:';
  SPasswordChanged = 'Το σύνθημα έχει αλλαχθεί';
  SPasswordNotChanged = 'Το σύνθημα δεν έχει αλλαχθεί';
  SPasswordsMismatch = 'Τα συνθήματα ''Νέο'' και ''Επιβεβαίωση'' δεν ταιριάζουν';
  SDBExceptCaption = 'Σφάλμα Μηχανής Βάσης Δεδομένων (Database Engine)';
  SBDEErrorLabel = 'Σφάλμα BDE';
  SServerErrorLabel = 'Σφάλμα Server';
  SErrorMsgLabel = 'Μήνυμα σφάλματος';
  SNextButton = '&Επόμενο';
  SPrevButton = '&Προηγούμενο';
  SExprIncorrect = 'Λανθασμένη διάταξη της έκφρασης φίλτρου';
  SExprTermination = 'Λανθασμένος τερματισμός της έκφρασης φίλτρου';
  SExprNameError = 'Μη τερματισμένο όνομα πεδίου';
  SExprStringError = 'Μη τερματισμένη αλφαριθμητική σταθερά';
  SExprInvalidChar = 'Άκυρος χαρακτήρας έκφρασης φίλτρου: ''%s''';
  SExprNoRParen = ''')'' αναμενόταν αλλά %s βρέθηκε';
  SExprExpected = 'Έκφραση αναμενόταν αλλά %s βρέθηκε';
  SExprBadCompare = 'Σχεσιακοί τελεστές απαιτούν ένα πεδίο και μία σταθερά';
  SConfirmSave = 'Τα δεδομένα έχουν αλλάξει. Να σωθούν;';
  SDatabaseName = 'Όνομα βάσης δεδομένων: %s';
  SUnlockCaption = 'Ξεκλείδωμα εφαρμογής';
  SUnlockHint = 'Πληκτρολόγησε το σύνθημά σου';
  SDeleteMultipleRecords = 'Να διαγράψω όλες τις επιλεγμένες εγγραφές;';

  {RXGConst}
  SGIFImage = 'Εικόνα CompuServe GIF';
  SChangeGIFSize = 'Δεν μπορώ να αλλάξω το μέγεθος μίας εικόνας GIF';
  SNoGIFData = 'Δεν υπάρχουν GIF Δεδομένα για εγγραφή';
  SUnrecognizedGIFExt = 'Μη αναγνωρίσιμο μέρος επέκτασης: %.2x';
  SWrongGIFColors = 'Λανθασμένος αριθμός χρωμάτων; πρέπει να είναι δύναμη του 2';
  SBadGIFCodeSize = 'Ο κωδικός μεγέθους του GIF είναι εκτός του πεδίου τιμών 2 έως 9';
  SGIFDecodeError = 'Τα κωδικοποιημένα δεδομένα του GIF είναι κατεστραμμένα';
  SGIFEncodeError = 'Σφάλμα κατά την κωδικοποίηση της εικόνας GIF';
  SGIFVersion = 'Άγνωστη έκδοση GIF';

  {RXTConst}
  SDualListSrcCaption = '&Πηγή';
  SDualListDestCaption = '&Προορισμός';
  SClipbrdUnknown = 'Αδυνατώ να απεικονίσω. Τα Δεδομένα στο έχουν άγνωστη διάταξη.';
  SClipbrdEmpty = 'Το Clipboard είναι κενό';
  SCustomizeSpeedbar = 'Προσαρμογή Speedbar';
  SAvailButtons = '&Διαθέσιμα κόμβια:';
  SSpeedbarCategories = '&Κατηγορίες:';
  SSpeedbarEditHint = 'Για να προσθέσετε κόμβια εντολών, σύρτε και αφήστε κόμβια επάνω στη SpeedBar. Για να τα αφαιρέσετε, σύρτε τα εκτός της SpeedBar.';
  SParseSyntaxError = 'Συντακτικό σφάλμα';
  SParseNotCramp = 'Άκυρη συνθήκη (no cramp)';
  SParseDivideByZero = 'Διαίρεση με το μηδέν';
  SParseSqrError = 'Άκυρη πράξη κινητής υποδιαστολής';
  SParseLogError = 'Άκυρη πράξη κινητής υποδιαστολής';
  SParseInvalidFloatOperation = 'Άκυρη πράξη κινητής υποδιαστολής';

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