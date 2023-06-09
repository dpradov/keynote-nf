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
unit RxResConst_chi;
interface
resourcestring
  {RXCConst}
  SBrowse = '浏览';
  SDefaultFilter = '所有文件 (*.*)|*.*';
  SDateDlgTitle = '选择日期';
  SNextYear = '下一年|';
  SNextMonth = '下一月|';
  SPrevYear = '上一年|';
  SPrevMonth = '上一月|';
  SNotImplemented = '功能尚未实现';
  SFileNotExec = '标识的文件不是一个可执行文件, 动态链接库或图标文件';
  SLoadLibError = '不能载入 ''%s'' 库';
  SDetails = '细目';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = '不能对本地数据库执行这个操作';
  SRetryLogin = '希望再次连接数据库吗?';
  SExprNotBoolean = '字段 ''%s'' 不是逻辑类型';
  SExprBadNullTest = 'NULL 只允许 ''='' 和 ''<>''操作';
  SExprBadField = '字段 ''%s'' 不能用于过滤表达式';
  SCaptureFilter = '当元件被捕获时不能执行此项操作';
  SNotCaptureFilter = '当元件不被捕获时不能执行此项操作';
  SInactiveData = '关闭';
  SBrowseData = '浏览';
  SEditData = '编辑';
  SInsertData = '插入';
  SSetKeyData = '查询';
  SCalcFieldsData = '计算';
  SRegistration = '登录';
  SAppTitleLabel = '应用程序 *%s*';
  SHintLabel = '输入用户姓名和口令';
  SUserNameLabel = '用户姓名[&U]:';
  SPasswordLabel = '用户口令[&P]:';
  SInvalidUserName = '非法的用户姓名和口令';
  SChangePassword = '更换口令';
  SOldPasswordLabel = '旧口令[&O]:';
  SNewPasswordLabel = '新口令[&N]:';
  SConfirmPasswordLabel = '确认口令[&C]:';
  SPasswordChanged = '口令已经被更换';
  SPasswordNotChanged = '口令没有被更换';
  SPasswordsMismatch = '新口令和确认口令不一致';
  SDBExceptCaption = '数据库系统引擎错误';
  SBDEErrorLabel = 'BDE错误';
  SServerErrorLabel = '数据库服务器错误';
  SErrorMsgLabel = '错误信息';
  SNextButton = '下一个[&N]';
  SPrevButton = '上一个[&P]';
  SExprIncorrect = '构造了错误的过滤表达式';
  SExprTermination = '错误的过滤表达式被终止';
  SExprNameError = '错误的字段名';
  SExprStringError = '错误的字符串常量';
  SExprInvalidChar = '非法的过滤表达式字符: ''%s''';
  SExprNoRParen = '需要一个'')'',  但发现一个 %s ';
  SExprExpected = '需要一个表达式, 但发现一个 %s ';
  SExprBadCompare = '关系操作要求一个字段和一个常数';
  SConfirmSave = '数据已经被修改. 是否保存?';
  SDatabaseName = '数据库名称: %s';
  SUnlockCaption = '解锁应用程序';
  SUnlockHint = '输入口令';
  SDeleteMultipleRecords = '删除所有选中记录?';
  {RXGConst}
  SGIFImage = 'CompuServe GIF 图像';
  SChangeGIFSize = '不能改变GIF图像的大小';
  SNoGIFData = '没有GIF图像的数据可以写入';
  SUnrecognizedGIFExt = '不可识别的扩展数据块: %.2x';
  SWrongGIFColors = '错误的颜色数量; 必须是 2 的指数';
  SBadGIFCodeSize = 'GIF图像代码大小不能介于2和9之间';
  SGIFDecodeError = 'GIF图像的解码数据被损坏';
  SGIFEncodeError = 'GIF图像解码出错';
  SGIFVersion = '不可识别的GIF图像版本';
  {RXTConst}
  SDualListSrcCaption = '源[&S]';
  SDualListDestCaption = '目的[&D]';
  SClipbrdUnknown = '不能显示. 剪裁板中的数据格式不可识别.';
  SClipbrdEmpty = '剪裁板为空';
  SCustomizeSpeedbar = '自定义工具栏';
  SAvailButtons = '可选的按钮[&A]:';
  SSpeedbarCategories = '目录[&C]:';
  SSpeedbarEditHint = '要增加按钮, 将按钮拖放到工具栏上. 要删除按钮, 将按钮拖离工具栏.';
  SParseSyntaxError = '语法错误';
  SParseNotCramp = '非法条件(不可辨识)';
  SParseDivideByZero = '被0除';
  SParseSqrError = '非法的浮点操作';
  SParseLogError = '非法的浮点操作';
  SParseInvalidFloatOperation = '非法的浮点操作';
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