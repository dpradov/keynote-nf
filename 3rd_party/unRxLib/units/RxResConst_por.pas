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
unit RxResConst_por;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Visualizador';
  SDefaultFilter = 'Todos Arquivos (*.*)|*.*';
  SDateDlgTitle = 'Selecione uma Data';
  SNextYear = 'Próximo Ano|';
  SNextMonth = 'Próximo Mês|';
  SPrevYear = 'Ano Anterior|';
  SPrevMonth = 'Mês Anterior|';
  SNotImplemented = 'Função ainda não implementada';
  SFileNotExec = 'O Arquivo especificado não é um arquivo executável, biblioteca de ligação-dinâmica ou arquivo de ícone';
  SLoadLibError = 'Não foi possível carregar a biblioteca ''%s''';
  SDetails = 'Detalhes';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Não é possível realizar esta operação no Banco de Dados Local';
  SRetryLogin = 'Você deseja tentar novamente a conexão com o banco de dados?';
  SExprNotBoolean = 'O Campo ''%s'' não é do tipo Booleano';
  SExprBadNullTest = 'NULL é apenas permitido com ''='' e ''<>''';
  SExprBadField = 'O Campo ''%s'' não pode ser usado numa expressão de filtro';
  SCaptureFilter = 'Não é possível realizar esta operação quando os controles estão capturados';
  SNotCaptureFilter = 'Não é possível realizar esta operação quando os controles não estão capturados';
  SInactiveData = 'Fechado';
  SBrowseData = 'Visualizar';
  SEditData = 'Editar';
  SInsertData = 'Inserir';
  SSetKeyData = 'Procurar';
  SCalcFieldsData = 'Calcular';
  SRegistration = 'Registro';
  SAppTitleLabel = 'Aplicação *%s*';
  SHintLabel = 'Entre o seu nome e senha';
  SUserNameLabel = 'Nome do &Usuário:';
  SPasswordLabel = '&Senha:';
  SInvalidUserName = 'Nome de usuário ou senha inválido';
  SChangePassword = 'Altere a senha';
  SOldPasswordLabel = 'Senha &Antiga:';
  SNewPasswordLabel = 'Senha &Nova:';
  SConfirmPasswordLabel = '&Confirmar senha:';
  SPasswordChanged = 'A Senha foi alterada';
  SPasswordNotChanged = 'A Senha não foi alterada';
  SPasswordsMismatch = 'A nova senha e a confirmação não conferem';
  SDBExceptCaption = 'Erro no Engine do Banco de Dados';
  SBDEErrorLabel = 'Erro no BDE';
  SServerErrorLabel = 'Erro no Servidor';
  SErrorMsgLabel = 'Mensagem de Erro';
  SNextButton = '&Próximo';
  SPrevButton = '&Anterior';
  SExprIncorrect = 'Expressão de filtro incorretamente formada';
  SExprTermination = 'Expressão de filtro incorretamente terminada';
  SExprNameError = 'Nome de campo indeterminado';
  SExprStringError = 'Constante tipo string não terminada';
  SExprInvalidChar = 'Caracter de filtro de expressão inválido: ''%s''';
  SExprNoRParen = ''')'' esperado, mas %s encontrado';
  SExprExpected = 'Expressão esperada, mas %s encontrado';
  SExprBadCompare = 'Operadores relacionais exigem um campo e uma constante';
  SConfirmSave = 'Os dados foram alterados. Deve-se salvá-los?';
  SDatabaseName = 'Nome do Banco de Dados: %s';
  SUnlockCaption = 'Aplicação liberada';
  SUnlockHint = 'Entre a sua senha';
  SDeleteMultipleRecords = 'Apagar todos os registros selecionados?';

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
  SDualListSrcCaption = '&Fonte';
  SDualListDestCaption = '&Destino';
  SClipbrdUnknown = 'Não é possível exibir. Os dados no Clipboard estão num formato desconhecido.';
  SClipbrdEmpty = 'O Clipboard está vazio';
  SCustomizeSpeedbar = 'Customizar a  Speedbar';
  SAvailButtons = '&Botões disponíveis:';
  SSpeedbarCategories = '&Categorias:';
  SSpeedbarEditHint = 'Para adicionar botões de comando, arraste e solte botões na SpeedBar. Para remover botões de comando, arraste-os para fora da SpeedBar.';
  SParseSyntaxError = 'Erro de syntaxe';
  SParseNotCramp = 'Condição inválida (nenhum gancho)';
  SParseDivideByZero = 'Divisão por zero';
  SParseSqrError = 'Operação de ponto flutuante inválida';
  SParseLogError = 'Operação de ponto flutuante inválida';
  SParseInvalidFloatOperation = 'Operação de ponto flutuante inválida';

  {RXLConst}
  srRXControls = 'RX Controls';
  srRXDBAware = 'RX DBAware';
  srRXTools = 'RX Tools';

  srStorageDesigner = 'Módulo de Desenho de Formulários de Armazenagem...';

  srProxyEditor = 'Editar Proxies...';
  srPageProxies = '%s Páginas de Proxies';
  srProxyName = 'Nome da Página de Proxy';
  srPageName = 'Nome da Página';

  srSBItemNotCreate = 'Não é possível criar um novo botão tipo Speedbar';
  srConfirmSBDelete = 'Você tem certeza que deseja apagar a  secção corrente?';
  srSpeedbarDesigner = 'Módulo de Desenho de Speedbar...';
  srNewSectionName = 'Sem Título (%d)';

  srEventNotCreate = 'Não foi possível criar um novo evento';
  srTimerDesigner = 'Editar Eventos...';
  srTimerEvents = '%s.Eventos';

  srAniCurFilter = 'Cursores Animados (*.ani)|*.ani|Todos Arquivos (*.*)|*.*';
  srEditPicture = 'Editar Figura...';
  srLoadAniCursor = 'Carregar a partir de um ANI...';

  srLoadIcon = 'Carregar Ícone';

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