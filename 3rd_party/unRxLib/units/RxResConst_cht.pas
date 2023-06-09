{*******************************************************}
{ Traditional Chinese                                   }
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{         Copyright (c) 2008 Jaro Benes                 }
{                                                       }
{ Recreated from lang. resources by JB.                 }
{*******************************************************}
unit RxResConst_cht;
interface
resourcestring
  {RXCConst}
  SBrowse = '瀏覽';
  SDefaultFilter = '所有檔案 (*.*)|*.*';
  SDateDlgTitle = '選擇日期';
  SNextYear = '下一年|';
  SNextMonth = '下一月|';
  SPrevYear = '上一年|';
  SPrevMonth = '上一月|';
  SNotImplemented = '本功能尚未完成';
  SFileNotExec = '指定的檔案不是一個可執行檔、動態連結程式庫、或圖示檔';
  SLoadLibError = '無法載入 ''%s'' 函式庫';
  SDetails = '詳細內容';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = '不能對本地資料庫執行這個操作';
  SRetryLogin = '您想要再嘗試連接資料庫嗎?';
  SExprNotBoolean = '欄位 ''%s'' 不是布林型態';
  SExprBadNullTest = 'NULL 只允許 ''='' and ''<>'' 操作';
  SExprBadField = '欄位 ''%s'' 不能被使用於篩選表示式';
  SCaptureFilter = '當元件被捕獲時不能執行此項操作';
  SNotCaptureFilter = '當元件未被捕獲時不能執行此項操作';
  SInactiveData = '關閉';
  SBrowseData = '瀏覽';
  SEditData = '編輯';
  SInsertData = '新增';
  SSetKeyData = '查詢';
  SCalcFieldsData = '計算';
  SRegistration = '登錄';
  SAppTitleLabel = '應用程式 *%s*';
  SHintLabel = '請輸入使用者名稱及密碼';
  SUserNameLabel = '使用者名稱(&U):';
  SPasswordLabel = '使用者密碼(&P):';
  SInvalidUserName = '無效的使用者名稱或密碼';
  SChangePassword = '變更密碼';
  SOldPasswordLabel = '舊的密碼(&O):';
  SNewPasswordLabel = '新的密碼(&N):';
  SConfirmPasswordLabel = '確認密碼(&C):';
  SPasswordChanged = '密碼已經變更';
  SPasswordNotChanged = '密碼沒有改變';
  SPasswordsMismatch = '新的密碼及確認密碼不一致';
  SDBExceptCaption = '資料庫引擎錯誤';
  SBDEErrorLabel = 'BDE 錯誤';
  SServerErrorLabel = '伺服器錯誤';
  SErrorMsgLabel = '錯誤訊息';
  SNextButton = '下一筆(&N)';
  SPrevButton = '上一筆(&P)';
  SExprIncorrect = '篩選表示式的語法錯誤';
  SExprTermination = '篩選表示式沒有結束字符';
  SExprNameError = '不正確的欄位名稱(沒有終止字符)';
  SExprStringError = '字串常數沒有終止字符';
  SExprInvalidChar = '無效的篩選表示式字元: ''%s''';
  SExprNoRParen = '需要一個 '')'' 但卻找到 %s ';
  SExprExpected = '需要一個表示式但卻找到 %s ';
  SExprBadCompare = '關係運算子必須指定一個欄位及常數';
  SConfirmSave = '資料已經修改過，是否儲存?';
  SDatabaseName = '資料庫名稱: %s';
  SUnlockCaption = '解除應用程式鎖定';
  SUnlockHint = '請輸入您的密碼';
  SDeleteMultipleRecords = '刪除所有選取的記錄?';

  {RXGConst}
  SGIFImage = 'CompuServe GIF 圖形';
  SChangeGIFSize = '無法改變 GIF 圖形的大小';
  SNoGIFData = '沒有 GIF 資料可以寫入';
  SUnrecognizedGIFExt = '無法識別的延伸區塊: %.2x';
  SWrongGIFColors = '錯誤的顏色數量; 必須為 2 的指數';
  SBadGIFCodeSize = 'GIF 代碼長度不能介於範圍 2 到 9 之間';
  SGIFDecodeError = 'GIF 解碼資料已經損壞';
  SGIFEncodeError = 'GIF 圖形編碼錯誤';
  SGIFVersion = '未知的 GIF 版本';

  {RXTConst}
  SDualListSrcCaption = '來源(&S)';
  SDualListDestCaption = '目的(&D)';
  SClipbrdUnknown = '無法顯示。剪貼簿中的資料格式無法識別。';
  SClipbrdEmpty = '剪貼簿是空的';
  SCustomizeSpeedbar = '自訂工具列';
  SAvailButtons = '可用的按鈕(&A):';
  SSpeedbarCategories = '目錄(&C):';
  SSpeedbarEditHint = '若要增加按鈕，請將按鈕拖放至工具列上。要移除按鈕，將它們拖離工具列。';
  SParseSyntaxError = '語法錯誤';
  SParseNotCramp = '無效的條件 (缺少右刮號)';
  SParseDivideByZero = '除零錯誤';
  SParseSqrError = '非法的浮點數運算';
  SParseLogError = '非法的浮點數運算';
  SParseInvalidFloatOperation = '非法的浮點數運算';

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