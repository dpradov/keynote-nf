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
unit RxResConst_fra;
interface
resourcestring
  {RXCConst}
  SBrowse = 'Parcourir';
  SDefaultFilter = 'Tous les fichiers (*.*)|*.*';
  SDateDlgTitle = 'Sélectionner une date';
  SNextYear = 'Année suivante|';
  SNextMonth = 'Mois suivant|';
  SPrevYear = 'Année précédente|';
  SPrevMonth = 'Mois précédent|';
  SNotImplemented = 'Fonction non encore implémentée';
  SFileNotExec = 'Le fichier indiqué n''est pas un exécutable, une bibliothèque dynamique ou une icône';
  SLoadLibError = 'Impossible de charger la bibliothèque ''%s''';
  SDetails = 'Détails';

  SDateOutOfRange = '''%s'' - invalid date. Value must be between ''%s'' and ''%s''';
  SDateOutOfMin = '''%s'' - invalid date. Value must be greater than ''%s''';
  SDateOutOfMax = '''%s'' - invalid date. Value must be less than ''%s''';
  SDateMinLimit = 'Minimum date must be less than ''%s''';
  SDateMaxLimit = 'Maximum date must be greater than ''%s''';
  {RXDConst}
  SLocalDatabase = 'Impossible d''effectuer cette opération sur une base locale';
  SRetryLogin = 'Voulez-vous essayer de vous reconnecter à la base de données?';
  SExprNotBoolean = 'Le champ ''%s'' n''est pas de type Booléen';
  SExprBadNullTest = 'NULL est permis seulement avec ''='' et ''<>''';
  SExprBadField = 'Le champ ''%s'' ne peut pas être utilisé dans une condition de filtre';
  SCaptureFilter = 'Cannot perform this operation when controls are captured';
  SNotCaptureFilter = 'Cannot perform this operation when controls are not captured';
  SInactiveData = 'Fermé';
  SBrowseData = 'Parcourir';
  SEditData = 'Editer';
  SInsertData = 'Insérer';
  SSetKeyData = 'Chercher';
  SCalcFieldsData = 'Calculer';
  SRegistration = 'Registration';
  SAppTitleLabel = 'Application *%s*';
  SHintLabel = 'Entrez votre nom d''utilisateur et votre mot de passe';
  SUserNameLabel = '&Nom d''utilisateur:';
  SPasswordLabel = '&Mot de passe:';
  SInvalidUserName = 'Nom d''utilisateur ou mot de passe invalide';
  SChangePassword = 'Modifier le mot de passe';
  SOldPasswordLabel = '&Ancien mot de passe:';
  SNewPasswordLabel = '&Nouveau mot de passe:';
  SConfirmPasswordLabel = '&Confirmer le mot de passe:';
  SPasswordChanged = 'Le mot de passe a été modifié';
  SPasswordNotChanged = 'Le mot de passe n''a pas été modifié';
  SPasswordsMismatch = 'Le nouveau mot de passe et sa confirmation ne concordent pas';
  SDBExceptCaption = 'Erreur du moteur de la base de données';
  SBDEErrorLabel = 'Erreur BDE';
  SServerErrorLabel = 'Erreur Serveur';
  SErrorMsgLabel = 'Message d''erreur';
  SNextButton = '&Suiv';
  SPrevButton = '&Préc';
  SExprIncorrect = 'Condition de filtre constituée incorrectement';
  SExprTermination = 'Condition de filtre terminée incorrectement';
  SExprNameError = 'Nom de champ non terminé';
  SExprStringError = 'Constante chaîne non terminée';
  SExprInvalidChar = 'Caractère invalide dans la condition de filtre: ''%s''';
  SExprNoRParen = ''')'' attendu mais %s trouvé';
  SExprExpected = 'Expression attendue mais %s trouvé';
  SExprBadCompare = 'Les opérateurs relationnels nécessitent un champ et une constante';
  SConfirmSave = 'Les données ont changé. Les enregistrer?';
  SDatabaseName = 'Nom de la base de données: %s';
  SUnlockCaption = 'Déverrouiller l''application';
  SUnlockHint = 'Entrez votre mot de passe';
  SDeleteMultipleRecords = 'Effacer tous les enregistrements sélectionnés?';
  {RXGConst}
  SGIFImage = 'Image CompuServe GIF';
  SChangeGIFSize = 'Impossible de modifier la taille d''une image GIF';
  SNoGIFData = 'Aucune donnée GIF à écrire';
  SUnrecognizedGIFExt = 'Bloc d''extension non reconnu: %.2x';
  SWrongGIFColors = 'Nombre de couleurs erroné; doit être une puissance de 2';
  SBadGIFCodeSize = 'GIF code size not in range 2 to 9';
  SGIFDecodeError = 'Les données codées GIF sont altérées';
  SGIFEncodeError = 'Erreur de codage de l''image GIF';
  SGIFVersion = 'Version GIF inconnue';
  {RXTConst}
  SDualListSrcCaption = '&Source';
  SDualListDestCaption = '&Destination';
  SClipbrdUnknown = 'Affichage impossible. Le format du contenu du presse-papiers est inconnu.';
  SClipbrdEmpty = 'Le presse-papiers est vide';
  SCustomizeSpeedbar = 'Personnaliser la barre d''icônes';
  SAvailButtons = '&Boutons disponibles:';
  SSpeedbarCategories = '&Catégories:';
  SSpeedbarEditHint = 'Pour ajouter des boutons de commande, tirez et lâchez les boutons sur la barre d''icônes. Pour les supprimer, déplacez-les hors de la barre d''icônes.';
  SParseSyntaxError = 'Erreur de syntaxe';
  SParseNotCramp = 'Condition invalide (no cramp)';
  SParseDivideByZero = 'Division par zéro';
  SParseSqrError = 'Opération en virgule flottante invalide';
  SParseLogError = 'Opération en virgule flottante invalide';
  SParseInvalidFloatOperation = 'Opération en virgule flottante invalide';
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