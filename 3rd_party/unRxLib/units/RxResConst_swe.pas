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
unit RxResConst_swe;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Bläddra';
  SDefaultFilter = 'Alla filer (*.*)|*.*';
  SDateDlgTitle = 'Välj ett datum';
  SNextYear = 'Nästa År|';
  SNextMonth = 'Nästa Månad|';
  SPrevYear = 'Föregående År|';
  SPrevMonth = 'Föregående Månad|';
  SNotImplemented = 'Funktionen ej implementerad';
  SFileNotExec = 'Den angivna filen är inte ett körbart program, DLL eller en ikon';
  SLoadLibError = 'Kunde inte ladda biblioteket ''%s''';
  SDetails = 'Detaljer';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Ogiltig operation för lokalt dataset';
  SRetryLogin = 'Vill du försöka koppla dig till databasen igen?';
  SExprNotBoolean = 'Fält ''%s'' är inte ett boolskt fält';
  SExprBadNullTest = 'NULL är endast giltigt med ''='' och ''<>''';
  SExprBadField = 'Fältet ''%s'' kan inte användas i ett filter uttryck';
  SCaptureFilter = 'Ogiltig operation när kontroller är fångade';
  SNotCaptureFilter = 'Ogiltig operation när kontroller inte är fångade';
  SInactiveData = 'Stängd';
  SBrowseData = 'Bläddra';
  SEditData = 'Redigera';
  SInsertData = 'Infoga';
  SSetKeyData = 'Sök';
  SCalcFieldsData = 'Beräkna';
  SRegistration = 'Registrering';
  SAppTitleLabel = 'Program *%s*';
  SHintLabel = 'Skriv in namn och lösenord';
  SUserNameLabel = '&Användarnamn:';
  SPasswordLabel = '&Lösenord:';
  SInvalidUserName = 'Ogiltigt namn eller lösenord';
  SChangePassword = 'Ändra lösenord';
  SOldPasswordLabel = '&Gammalt lösenord:';
  SNewPasswordLabel = '&Nytt lösenord:';
  SConfirmPasswordLabel = '&Bekräfta lösenord:';
  SPasswordChanged = 'Lösenordet har ändrats';
  SPasswordNotChanged = 'Lösenordet har inte ändrats';
  SPasswordsMismatch = 'Det nya och bekräftade lösenordet är inte identiska';
  SDBExceptCaption = 'Databas fel';
  SBDEErrorLabel = 'BDE fel';
  SServerErrorLabel = 'Server fel';
  SErrorMsgLabel = 'Felmeddelande';
  SNextButton = '&Nästa';
  SPrevButton = '&Föreg';
  SExprIncorrect = 'Felaktigt formaterat filteruttryck';
  SExprTermination = 'Filteruttrycket felaktigt terminerat';
  SExprNameError = 'Oterminerat fältnamn';
  SExprStringError = 'Oterminerad strängkonstant';
  SExprInvalidChar = 'Ogiltigt tecken i filteruttryck: ''%s''';
  SExprNoRParen = 'Förväntade '')'' men fann %s';
  SExprExpected = 'Förväntade uttryck men fann %s';
  SExprBadCompare = 'Relationsoperatorer kräver ett fält och en konstant';
  SConfirmSave = 'Data har ändrats: vill du spara?';
  SDatabaseName = 'Databasnamn: %s';
  SUnlockCaption = 'Lås upp programmet';
  SUnlockHint = 'skriv ditt lösenord';
  SDeleteMultipleRecords = 'Ta bort alla valda poster?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF bild';
  SChangeGIFSize = 'Kan inte ändra storlek på en GIF bild';
  SNoGIFData = 'Finns ingen GIF data att skriva';
  SUnrecognizedGIFExt = 'Okänt tilläggsblock: %.2x';
  SWrongGIFColors = 'Fel antal färger; antalet måste vara en multipel av 2';
  SBadGIFCodeSize = 'GIF bildens kodstorlek måste vara mellan 2 och 9';
  SGIFDecodeError = 'GIF bildens data är skadad';
  SGIFEncodeError = 'GIF kodningsfel';
  SGIFVersion = 'Okänd GIF version';

  {RXTConst}
  SDualListSrcCaption = '&Källa';
  SDualListDestCaption = '&Mål';
  SClipbrdUnknown = 'Kan inte visa innehållet:data i Urklipp är i ett okänt format.';
  SClipbrdEmpty = 'Urklipp är tom';
  SCustomizeSpeedbar = 'Anpassa verktygsfält';
  SAvailButtons = '&Tillgängliga knappar:';
  SSpeedbarCategories = '&Kategorier:';
  SSpeedbarEditHint = 'Lägg till knappar genom att dra och släppa knappen på verktygsfältet. Ta bort knappar genom att dra knappen bort från verktygsfältet.';
  SParseSyntaxError = 'Syntaxfel';
  SParseNotCramp = 'Ogiltigt villkor (ingen cramp)';
  SParseDivideByZero = 'Division med noll';
  SParseSqrError = 'Ogiltig flyttals operation';
  SParseLogError = 'Ogiltig flyttals operation';
  SParseInvalidFloatOperation = 'Ogiltig flyttals operation';

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