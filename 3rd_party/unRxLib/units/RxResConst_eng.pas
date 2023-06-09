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
unit RxResConst_eng;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Browse';
  SDefaultFilter = 'All files (*.*)|*.*';
  SDateDlgTitle = 'Select a Date';
  SNextYear = 'Next Year|';
  SNextMonth = 'Next Month|';
  SPrevYear = 'Previous Year|';
  SPrevMonth = 'Previous Month|';
  SNotImplemented = 'Function not yet implemented';
  SFileNotExec = 'File specified is not an executable file, dynamic-link library, or icon file';
  SLoadLibError = 'Could not load ''%s'' library';
  SDetails = 'Details';
  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Cannot perform this operation on a local database';
  SRetryLogin = 'Do you wish to retry the connect to database?';
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  SCaptureFilter = 'Cannot perform this operation when controls are captured';
  SNotCaptureFilter = 'Cannot perform this operation when controls are not captured';
  SInactiveData = 'Closed';
  SBrowseData = 'Browse';
  SEditData = 'Edit';
  SInsertData = 'Insert';
  SSetKeyData = 'Search';
  SCalcFieldsData = 'Calculate';
  SRegistration = 'Registration';
  SAppTitleLabel = 'Application *%s*';
  SHintLabel = 'Type your user name and password';
  SUserNameLabel = '&User name:';
  SPasswordLabel = '&Password:';
  SInvalidUserName = 'Invalid user name or password';
  SChangePassword = 'Change password';
  SOldPasswordLabel = '&Old password:';
  SNewPasswordLabel = '&New password:';
  SConfirmPasswordLabel = '&Confirm password:';
  SPasswordChanged = 'Password has been changed';
  SPasswordNotChanged = 'Password has not been changed';
  SPasswordsMismatch = 'The new and confirmed passwords do not match';
  SDBExceptCaption = 'Database Engine Error';
  SBDEErrorLabel = 'BDE Error';
  SServerErrorLabel = 'Server Error';
  SErrorMsgLabel = 'Error message';
  SNextButton = '&Next';
  SPrevButton = '&Prev';
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoRParen = ''')'' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadCompare = 'Relational operators require a field and a constant';
  SConfirmSave = 'The data were changed. Save them?';
  SDatabaseName = 'Database name: %s';
  SUnlockCaption = 'Unlock application';
  SUnlockHint = 'Type your password';
  SDeleteMultipleRecords = 'Delete all selected records?';
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
  SDualListSrcCaption = '&Source';
  SDualListDestCaption = '&Destination';
  SClipbrdUnknown = 'Cannot display. Data in Clipboard is in an unknown format.';
  SClipbrdEmpty = 'Clipboard is empty';
  SCustomizeSpeedbar = 'Customize Speedbar';
  SAvailButtons = '&Available buttons:';
  SSpeedbarCategories = '&Categories:';
  SSpeedbarEditHint = 'To add command buttons, drag and drop buttons onto the SpeedBar. To remove command buttons, drag them off of the SpeedBar.';
  SParseSyntaxError = 'Syntax error';
  SParseNotCramp = 'Invalid condition (no cramp)';
  SParseDivideByZero = 'Divide by zero';
  SParseSqrError = 'Invalid floating point operation';
  SParseLogError = 'Invalid floating point operation';
  SParseInvalidFloatOperation = 'Invalid floating point operation';
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