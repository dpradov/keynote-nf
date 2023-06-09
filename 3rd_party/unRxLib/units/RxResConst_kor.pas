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
unit RxResConst_kor;
interface
resourcestring
  {RXCConst}
  SBrowse = '찾아보기';
  SDefaultFilter = '모든 파일 (*.*)|*.*';
  SDateDlgTitle = '날짜를 선택하세요';
  SNextYear = '다음 해|';
  SNextMonth = '다음 달|';
  SPrevYear = '이전 해|';
  SPrevMonth = '이전 달|';
  SNotImplemented = '아직 구현되지 않은 기능입니다';
  SFileNotExec = '지정한 파일이 실행파일이나 동적링크 라이브러리, 아이콘 파일이 아닙니다';
  SLoadLibError = '''%s'' 라이브러리를 읽어들일 수 없습니다';
  SDetails = 'Details';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = '이 동작은 로컬 데이터베이스에서 수행할 수 없습니다';
  SRetryLogin = '데이터베이스로의 연결을 재시도하겠습니까?';
  SExprNotBoolean = '필드 ''%s''가 불린 타입이 아닙니다';
  SExprBadNullTest = 'NULL은 ''='', ''<>''과 함께일 때만 허용됩니다';
  SExprBadField = '필드 ''%s''은 필터 연산식에서 사용될 수 없습니다';
  SCaptureFilter = '컨트롤이 캡처되어 있을 때는 이 동작을 수행할 수 없습니다';
  SNotCaptureFilter = '컨트롤이 캡처되어 있지 않을 때는 이 동작을 수행할 수 없습니다';
  SInactiveData = '닫김';
  SBrowseData = '찾아보기';
  SEditData = '편집';
  SInsertData = '삽입';
  SSetKeyData = '찾기';
  SCalcFieldsData = '계산';
  SRegistration = '등록';
  SAppTitleLabel = '어플리케이션 *%s*';
  SHintLabel = '유저 네임과 패스워드를 입력하십시오';
  SUserNameLabel = '유저 네임(&U):';
  SPasswordLabel = '패스워드(&P):';
  SInvalidUserName = '유저 네임이나 패스워드가 잘못되었습니다';
  SChangePassword = '패스워드 변경';
  SOldPasswordLabel = '이전 패스워드(&O):';
  SNewPasswordLabel = '새 패스워드(&N):';
  SConfirmPasswordLabel = '패스워드 확인(&C):';
  SPasswordChanged = '패스워드가 변경되었습니다';
  SPasswordNotChanged = '패스워드가 변경되지 않았습니다';
  SPasswordsMismatch = '새 패스워드와 다시 입력한 패스워드가 일치하지 않습니다';
  SDBExceptCaption = '데이터베이스 엔진 에러';
  SBDEErrorLabel = 'BDE 에러';
  SServerErrorLabel = '서버 에러';
  SErrorMsgLabel = '에러 메시지';
  SNextButton = '다음(&N)';
  SPrevButton = '이전(&P)';
  SExprIncorrect = '잘못된 형태의 필터 연산식';
  SExprTermination = '필터 연산식이 비정상적으로 끝났습니다';
  SExprNameError = '끝나지 않은 필드 이름';
  SExprStringError = '끝나지 않은 스트링 상수';
  SExprInvalidChar = '잘못된 필터 연산식 문자: ''%s''';
  SExprNoRParen = ''')''가 필요한 곳에서 %s가 발견되었습니다';
  SExprExpected = '연산식이 필요한 곳에서 %s가 발견되었습니다';
  SExprBadCompare = '관계 연산자는 하나의 필드와 하나의 상수를 필요로 합니다';
  SConfirmSave = '데이터가 변경되었습니다. 저장하시겠습니까?';
  SDatabaseName = '데이터베이스 이름: %s';
  SUnlockCaption = '어플리케이션 잠금 풀기';
  SUnlockHint = '패스워드를 입력하세요';
  SDeleteMultipleRecords = '선택된 모든 레코드를 삭제하시겠습니까?';

  {RXGConst}
  SGIFImage = '컴퓨서브 GIF 이미지';
  SChangeGIFSize = 'GIF 이미지의 사이즈를 변경할 수 없습니다';
  SNoGIFData = '써넣을 GIF 데이터가 없습니다';
  SUnrecognizedGIFExt = '인식되지 않는 확장 블럭: %.2x';
  SWrongGIFColors = '잘못된 컬러 수; 2의 지수승이어야 합니다';
  SBadGIFCodeSize = 'GIF 코드 사이즈가 2에서 9사이가 아닙니다';
  SGIFDecodeError = 'GIF 압축 데이터가 잘못되었습니다';
  SGIFEncodeError = 'GIF 이미지 압축 에러';
  SGIFVersion = '알 수 없는 GIF 버전';

  {RXTConst}
  SDualListSrcCaption = '소스(&S)';
  SDualListDestCaption = '데스티네이션(&D)';
  SClipbrdUnknown = '표시할 수 없습니다. 클립보드의 데이터가 알 수 없는 포맷입니다.';
  SClipbrdEmpty = '클립보드가 비어있습니다';
  SCustomizeSpeedbar = '스피드바 사용자 정의';
  SAvailButtons = '사용가능한 버튼(&A):';
  SSpeedbarCategories = '카테고리(&C):';
  SSpeedbarEditHint = '컴맨드 버튼을 추가하려면, 버튼을 드래그하여 스피드바로 드랍하십시오. 컴맨드 버튼을 제거하려면, 버튼을 스피드바 바깥으로 드래그하십시오.';
  SParseSyntaxError = '문법 오류';
  SParseNotCramp = '잘못된 조건 (괄호가 없슴)';
  SParseDivideByZero = '0으로 나누어짐';
  SParseSqrError = '잘못된 부동소수점 연산';
  SParseLogError = '잘못된 부동소수점 연산';
  SParseInvalidFloatOperation = '잘못된 부동소수점 연산';

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