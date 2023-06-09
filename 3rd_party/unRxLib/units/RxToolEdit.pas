{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxToolEdit;

interface

{$I RX.INC}
{$IFDEF RX_D6}
{$WARN UNIT_PLATFORM OFF}  // Polaris
{$ENDIF}

uses {$IFNDEF VER80} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Classes,
  StdCtrls, Controls, Messages, SysUtils, Forms, Graphics, Menus, Buttons,
  {$IFDEF RX_D7}Themes,{$ENDIF}
  {$IFDEF RX_D6}Types, {$ENDIF}
  {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  {$IFDEF RX_D12}SysConst,{$ENDIF}
  Dialogs, RxCtrls, FileCtrl, Mask, RxDateUtil;

const
  scAltDown = scAlt + vk_Down;
  DefEditBtnWidth = 21;

type
{$IFNDEF VER80}
  TFileExt = type string;
{$ENDIF}

{ TPopupWindow }

  TCloseUpEvent = procedure (Sender: TObject; Accept: Boolean) of object;
  TPopupAlign = (epaRight, epaLeft);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPopupWindow = class(TCustomControl)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
{$IFNDEF VER80}
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
{$ELSE}
    procedure CreateWnd; override;
    function GetValue: string; virtual; abstract;
    procedure SetValue(const Value: string); virtual; abstract;
{$ENDIF}
    procedure InvalidateEditor;
    procedure PopupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetPopupText: string; virtual;
    procedure Hide;
    procedure Show(Origin: TPoint); virtual; // Polaris
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

{ TCustomComboEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TEditButton = class(TRxSpeedButton)
  private
    FNoAction: Boolean;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$IFNDEF VER80}
    procedure Paint; override;
{$ENDIF}
  public
    FStandart: Boolean; // Polaris
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TGlyphKind = (gkCustom, gkDefault, gkDropDown, gkEllipsis);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCustomComboEdit = class(TCustomMaskEdit)
  private
//    FButton: TEditButton; // Polaris
    FBtnControl: TWinControl;
    FOnButtonClick: TNotifyEvent;
    FClickKey: TShortCut;
    FReadOnly: Boolean;
    FDirectInput: Boolean;
    FAlwaysEnable: Boolean;
    FAlignment: TAlignment;
//    FPopupVisible: Boolean; // Polaris
//    FFocused: Boolean; // Polaris
    FPopupAlign: TPopupAlign;
    FGlyphKind: TGlyphKind;
    procedure SetEditRect;
    procedure RecreateGlyph;
    procedure UpdateBtnBounds;
    procedure EditButtonClick(Sender: TObject);
    function GetMinHeight: Integer;
    function GetTextHeight: Integer;
//    procedure SetShowCaret; // Polaris
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetPopupVisible: Boolean;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(Value: Integer);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
    function GetDirectInput: Boolean;
//    procedure SetDirectInput(Value: Boolean);  // Polaris
    procedure SetReadOnly(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    function IsCustomGlyph: Boolean;
    function BtnWidthStored: Boolean;
    procedure SetGlyphKind(Value: TGlyphKind);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnter(var Message: TMessage); message CM_ENTER;
    procedure CNCtlColor(var Message: TMessage); message
      {$IFNDEF VER80} CN_CTLCOLOREDIT {$ELSE} CN_CTLCOLOR {$ENDIF};
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;

{$IFNDEF VER80}
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
{$ENDIF}
{$IFDEF RX_D4}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$ENDIF}
  protected
    FButton: TEditButton; // Polaris
    FPopupVisible: Boolean; // Polaris
    FFocused: Boolean; // Polaris

    FPopup: TCustomControl;
    FDefNumGlyphs: TNumGlyphs;

    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; virtual;
    procedure PopupDropDown(DisableEdit: Boolean); virtual;
    procedure PopupCloseUp(Sender: TObject; Accept: Boolean); virtual; //virtual Polaris
    procedure ShowPopup(Origin: TPoint); virtual;
    procedure HidePopup; virtual;
    procedure UpdatePopupVisible;
    procedure DoChange; virtual; //virtual Polaris
    procedure SetShowCaret; // Polaris
    procedure SetDirectInput(Value: Boolean);  // Polaris
{$IFNDEF VER80}
    function AcceptPopup(var Value: Variant): Boolean; virtual;
    procedure AcceptValue(const Value: Variant); virtual;
    procedure SetPopupValue(const Value: Variant); virtual;
    function GetPopupValue: Variant; virtual;
{$ELSE}
    function AcceptPopup(var Value: string): Boolean; virtual;
    procedure AcceptValue(const Value: string); virtual;
    procedure SetPopupValue(const Value: string); virtual;
    function GetPopupValue: string; virtual;
{$ENDIF}
    procedure Change; override;
    procedure PopupChange; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ButtonClick; dynamic;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AlwaysEnable: Boolean read FAlwaysEnable write FAlwaysEnable default False;
    property Button: TEditButton read FButton;
    property ClickKey: TShortCut read FClickKey write FClickKey
      default scAltDown;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property GlyphKind: TGlyphKind read FGlyphKind write SetGlyphKind default gkCustom;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth
      stored BtnWidthStored;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property DirectInput: Boolean read GetDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property PopupAlign: TPopupAlign read FPopupAlign write FPopupAlign default epaRight;
    property PopupVisible: Boolean read GetPopupVisible;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick;
    procedure SelectAll;
  end;

{ TComboEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TComboEdit = class(TCustomComboEdit)
  public
    property Button;
  published
//Polaris
    property Align;

    property Alignment;
    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is published before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFNDEF VER80}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFNDEF VER80}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TFileDirEdit }
{ The common parent of TFilenameEdit and TDirectoryEdit          }
{ For internal use only; it's not intended to be used separately }

{$IFDEF VER80}
const
  MaxFileLength = SizeOf(TFileName) - 1;
{$ENDIF}

type
  TExecOpenDialogEvent = procedure(Sender: TObject; var AName: string;
    var Action: Boolean) of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFileDirEdit = class(TCustomComboEdit)
  private
    FErrMode: Cardinal;
    FAcceptFiles: Boolean;
    FMultipleDirs: Boolean;
    FOnDropFiles: TNotifyEvent;
    FOnBeforeDialog: TExecOpenDialogEvent;
    FOnAfterDialog: TExecOpenDialogEvent;
    procedure SetDragAccept(Value: Boolean);
    procedure SetAcceptFiles(Value: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
{$IFNDEF VER80}
    function GetLongName: string; virtual; abstract;
    function GetShortName: string; virtual; abstract;
{$ENDIF}
    procedure DoAfterDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure DoBeforeDialog(var FileName: string; var Action: Boolean); dynamic;
    procedure ReceptFileDir(const AFileName: string); virtual; abstract;
    procedure ClearFileList; virtual;
    procedure DisableSysErrors;
    procedure EnableSysErrors;
    property GlyphKind default gkDefault;
    property MaxLength {$IFDEF VER80} default MaxFileLength {$ENDIF};
  public
    constructor Create(AOwner: TComponent); override;
{$IFNDEF VER80}
    property LongName: string read GetLongName;
    property ShortName: string read GetShortName;
{$ENDIF}
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles default False;
    property OnBeforeDialog: TExecOpenDialogEvent read FOnBeforeDialog
      write FOnBeforeDialog;
    property OnAfterDialog: TExecOpenDialogEvent read FOnAfterDialog
      write FOnAfterDialog;
    property OnDropFiles: TNotifyEvent read FOnDropFiles write FOnDropFiles;
    property OnButtonClick;
  end;

{ TFilenameEdit }

  TFileDialogKind = (dkOpen, dkSave {$IFDEF RX_D3}, dkOpenPicture,
    dkSavePicture {$ENDIF});

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFilenameEdit = class(TFileDirEdit)
  private
    FDialog: TOpenDialog;
    FDialogKind: TFileDialogKind;
    procedure CreateEditDialog;
    function GetFileName: string;
    function GetDefaultExt: TFileExt;
    function GetFileEditStyle: TFileEditStyle;
    function GetFilter: string;
    function GetFilterIndex: Integer;
    function GetInitialDir: string;
    function GetHistoryList: TStrings;
    function GetOptions: TOpenOptions;
    function GetDialogTitle: string;
    function GetDialogFiles: TStrings;
    procedure SetDialogKind(Value: TFileDialogKind);
    procedure SetFileName(const Value: string);
    procedure SetDefaultExt(Value: TFileExt);
    procedure SetFileEditStyle(Value: TFileEditStyle);
    procedure SetFilter(const Value: string);
    procedure SetFilterIndex(Value: Integer);
    procedure SetInitialDir(const Value: string);
    procedure SetHistoryList(Value: TStrings);
    procedure SetOptions(Value: TOpenOptions);
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function IsCustomFilter: Boolean;
  protected
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
    procedure ClearFileList; override;
{$IFNDEF VER80}
    function GetLongName: string; override;
    function GetShortName: string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    property Dialog: TOpenDialog read FDialog;
    property DialogFiles: TStrings read GetDialogFiles;
  published
//Polaris
    property Align;

    property DialogKind: TFileDialogKind read FDialogKind write SetDialogKind
      default dkOpen;
    property DefaultExt: TFileExt read GetDefaultExt write SetDefaultExt;
    property FileEditStyle: TFileEditStyle read GetFileEditStyle write SetFileEditStyle
      default fsEdit;
    property FileName: string read GetFileName write SetFileName stored False;
    property Filter: string read GetFilter write SetFilter stored IsCustomFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read GetInitialDir write SetInitialDir;
    property HistoryList: TStrings read GetHistoryList write SetHistoryList;
    property DialogOptions: TOpenOptions read GetOptions write SetOptions
      default [ofHideReadOnly];
    property DialogTitle: string read GetDialogTitle write SetDialogTitle
      stored IsCustomTitle;
    property AutoSelect;
    property ButtonHint;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFNDEF VER80}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFNDEF VER80}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TDirectoryEdit }

{$IFNDEF VER80}
  TDirDialogKind = (dkVCL, dkWin32);
{$ENDIF}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDirectoryEdit = class(TFileDirEdit)
  private
    FOptions: TSelectDirOpts;
    FInitialDir: string;
{$IFNDEF VER80}
    FDialogText: string;
    FDialogKind: TDirDialogKind;
{$ENDIF}
  protected
    FMultipleDirs: Boolean;   // Polaris
    procedure ButtonClick; override;
    procedure ReceptFileDir(const AFileName: string); override;
{$IFNDEF VER80}
    function GetLongName: string; override;
    function GetShortName: string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
//Polaris
    property Align;

{$IFNDEF VER80}
    property DialogKind: TDirDialogKind read FDialogKind write FDialogKind
      default dkVCL;
    property DialogText: string read FDialogText write FDialogText;
{$ENDIF}
    property DialogOptions: TSelectDirOpts read FOptions write FOptions default [];
    property InitialDir: string read FInitialDir write FInitialDir;
    property MultipleDirs: Boolean read FMultipleDirs write FMultipleDirs default False;
    property AutoSelect;
    property ButtonHint;
    property BorderStyle;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EditMask;
    property Enabled;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property NumGlyphs;
    property ButtonWidth;
    property HideSelection;
{$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFNDEF VER80}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFNDEF VER80}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

{ TCustomDateEdit }

  TCalendarStyle = (csPopup, csDialog);
  TYearDigits = (dyDefault, dyFour, dyTwo);

const
{$IFDEF DEFAULT_POPUP_CALENDAR}
  dcsDefault = csPopup;
{$ELSE}
  dcsDefault = csDialog;
{$ENDIF DEFAULT_POPUP_CALENDAR}

type
  TExecDateDialog = procedure(Sender: TObject; var ADate: TDateTime;
    var Action: Boolean) of object;

  TCustomDateEdit = class(TCustomComboEdit)
  private
    FMinDate,
    FMaxDate: TDateTime;  // Polaris
    FErrorRaise: Boolean;
{$IFDEF RX_D4}   // Polaris
    FTitle: String;
{$ELSE}
    FTitle: PString;
{$ENDIF}
    FOnAcceptDate: TExecDateDialog;
    FDefaultToday: Boolean;
    FHooked: Boolean;
    FPopupColor: TColor;
    FCheckOnExit: Boolean;
    FBlanksChar: Char;
    FCalendarHints: TStrings;
    FStartOfWeek: TDayOfWeekName;
    FWeekends: TDaysOfWeek;
    FWeekendColor: TColor;
    FYearDigits: TYearDigits;
    FDateFormat: string{$IFNDEF RX_D12}[10]{$ENDIF};
    FFormatting: Boolean;
// Polaris
    procedure SetMinDate(Value: TDateTime);
    procedure SetMaxDate(Value: TDateTime);
// Polaris
    function GetDate: TDateTime;
//    procedure SetDate(Value: TDateTime);

    procedure SetYearDigits(Value: TYearDigits);
    function GetPopupColor: TColor;
    procedure SetPopupColor(Value: TColor);
    function GetDialogTitle: string;
    procedure SetDialogTitle(const Value: string);
    function IsCustomTitle: Boolean;
    function GetCalendarStyle: TCalendarStyle;
    procedure SetCalendarStyle(Value: TCalendarStyle);
    procedure SetCalendarHints(Value: TStrings);
    procedure CalendarHintsChanged(Sender: TObject);
    procedure SetWeekendColor(Value: TColor);
    procedure SetWeekends(Value: TDaysOfWeek);
    procedure SetStartOfWeek(Value: TDayOfWeekName);
    procedure SetBlanksChar(Value: Char);
    function TextStored: Boolean;

// Polaris
    function StoreMinDate: Boolean;
    function StoreMaxDate: Boolean;
// Polaris

    function FourDigitYear: Boolean;
    function FormatSettingsChange(var Message: TMessage): Boolean;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
// Polaris
    FDateAutoBetween: Boolean;
    procedure SetDate(Value: TDateTime); virtual;
    procedure SetDateAutoBetween(Value: Boolean); virtual;
    procedure TestDateBetween(var Value: TDateTime); virtual;
// Polaris
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;
{$IFNDEF VER80}
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure AcceptValue(const Value: Variant); override;
    procedure SetPopupValue(const Value: Variant); override;
{$ELSE}
    function AcceptPopup(var Value: string): Boolean; override;
{$ENDIF}
    function GetDateFormat: string;
    procedure ApplyDate(Value: TDateTime); virtual;
    function GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap; override;
    procedure UpdateFormat;
    procedure UpdatePopup;
    procedure ButtonClick; override;
    property BlanksChar: Char read FBlanksChar write SetBlanksChar default ' ';
    property CalendarHints: TStrings read FCalendarHints write SetCalendarHints;
    property CheckOnExit: Boolean read FCheckOnExit write FCheckOnExit default False;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;
    property DialogTitle: string read GetDialogTitle write SetDialogTitle
      stored IsCustomTitle;
    property EditMask stored False;
    property ErrorRaise: Boolean read FErrorRaise write FErrorRaise default False;
    property Formatting: Boolean read FFormatting;
    property GlyphKind default gkDefault;
    property PopupColor: TColor read GetPopupColor write SetPopupColor
      default clBtnFace;
    property CalendarStyle: TCalendarStyle read GetCalendarStyle
      write SetCalendarStyle default dcsDefault;
    property StartOfWeek: TDayOfWeekName read FStartOfWeek write SetStartOfWeek default Mon;
    property Weekends: TDaysOfWeek read FWeekends write SetWeekends default [Sun];
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clRed;
    property YearDigits: TYearDigits read FYearDigits write SetYearDigits default dyDefault;
    property OnAcceptDate: TExecDateDialog read FOnAcceptDate write FOnAcceptDate;
    property MaxLength stored False;
    property Text stored TextStored;
  public
// Polaris
    property DateAutoBetween: Boolean read FDateAutoBetween write SetDateAutoBetween default True;
    property MinDate: TDateTime read FMinDate write SetMinDate stored StoreMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate stored StoreMaxDate;
{$IFDEF RX_D4}
    procedure ValidateEdit; override;
{$ENDIF}
// Polaris
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CheckValidDate;
    function GetDateMask: string;
    procedure UpdateMask; virtual;
    property Date: TDateTime read GetDate write SetDate;
    property PopupVisible;
  end;

{ TDateEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDateEdit = class(TCustomDateEdit)
// Polaris
  protected
    procedure SetDate(Value: TDateTime); override;

// Polaris
  public
    constructor Create(AOwner: TComponent); override;
    property EditMask;
  published
// Polaris
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align;
// Polaris
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
    property CalendarHints;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DefaultToday;
    property DialogTitle;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ErrorRaise;
    property Font;
    property GlyphKind;
    { Ensure GlyphKind is declared before Glyph and ButtonWidth }
    property Glyph;
    property ButtonWidth;
    property HideSelection;
{$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
{$ENDIF}
{$IFNDEF VER80}
  {$IFNDEF VER90}
    property ImeMode;
    property ImeName;
  {$ENDIF}
{$ENDIF}
    property MaxLength;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupColor;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property CalendarStyle;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnAcceptDate;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFNDEF VER80}
    property OnStartDrag;
{$ENDIF}
{$IFDEF RX_D5}
    property OnContextPopup;
{$ENDIF}
{$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
{$ENDIF}
  end;

  EComboEditError = class(Exception);

{ Utility routines }

procedure DateFormatChanged;

function EditorTextMargins(Editor: TCustomComboEdit): TPoint; {$IFDEF RX_D9}inline;{$ENDIF}

function PaintComboEdit(Editor: TCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TWMPaint): Boolean;

implementation

uses ShellAPI, Consts, {$IFDEF RX_D3} ExtDlgs, {$ENDIF} RxResConst, RxVCLUtils,
  RxFileUtil, RxPickDate, RxMaxMin,
  {$IFDEF RX_D6} Variants, RTLConsts,{$ENDIF} RxStrUtils; // Polaris

{$R *.RES}

const
  sFileBmp = 'FEDITBMP'; { Filename and directory editor button glyph }
  sDateBmp = 'DEDITBMP'; { Date editor button glyph }

{ Utility routines }

function EditorTextMargins(Editor: TCustomComboEdit): TPoint;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  with Editor do begin
{$IFNDEF VER80}
    if NewStyleControls then begin
      if BorderStyle = Forms.bsNone then I := 0
      else if Ctl3D then I := 1
      else I := 2;
      Result.X := SendMessage(Handle, EM_GETMARGINS, 0, 0) and $0000FFFF + I;
      Result.Y := I;
    end
    else begin
{$ENDIF}
      if BorderStyle = Forms.bsNone then I := 0
      else begin
        DC := GetDC(0);
        GetTextMetrics(DC, SysMetrics);
        SaveFont := SelectObject(DC, Font.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, SaveFont);
        ReleaseDC(0, DC);
        I := SysMetrics.tmHeight;
        if I > Metrics.tmHeight then I := Metrics.tmHeight;
        I := I div 4;
      end;
      Result.X := I;
      Result.Y := I;
{$IFNDEF VER80}
    end;
{$ENDIF}
  end;
end;

function PaintComboEdit(Editor: TCustomComboEdit; const AText: string;
  AAlignment: TAlignment; StandardPaint: Boolean;
  var ACanvas: TControlCanvas; var Message: TWMPaint): Boolean;
var
  AWidth, ALeft: Integer;
  _Margins: TPoint;
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
  S: string;
{$IFDEF RX_D4}
  ExStyle: DWORD;
const
  AlignStyle: array[Boolean, TAlignment] of DWORD =
   ((WS_EX_LEFT, WS_EX_RIGHT, WS_EX_LEFT),
    (WS_EX_RIGHT, WS_EX_LEFT, WS_EX_LEFT));
{$ENDIF}
begin
  Result := True;
  with Editor do
  begin
    {$IFDEF RX_D4}
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    {$ENDIF}
    if StandardPaint {$IFNDEF VER80} and not (csPaintCopy in ControlState) {$ENDIF} then
    begin
      {$IFDEF RX_D4}
      if SysLocale.MiddleEast and HandleAllocated and (IsRightToLeft) then
      begin { This keeps the right aligned text, right aligned }
        ExStyle := DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) and (not WS_EX_RIGHT) and
          (not WS_EX_RTLREADING) and (not WS_EX_LEFTSCROLLBAR);
        if UseRightToLeftReading then
          ExStyle := ExStyle or WS_EX_RTLREADING;
        if UseRightToLeftScrollbar then
          ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
        ExStyle := ExStyle or
          AlignStyle[UseRightToLeftAlignment, AAlignment];
        if DWORD(GetWindowLong(Handle, GWL_EXSTYLE)) <> ExStyle then
          SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);
      end;
      {$ENDIF RX_D4}
      Result := False;
      { return false if we need to use standard paint handler }
      Exit;
    end;
    { Since edit controls do not handle justification unless multi-line (and
      then only poorly) we will draw right and center justify manually unless
      the edit has the focus. }
    if ACanvas = nil then
    begin
      ACanvas := TControlCanvas.Create;
      ACanvas.Control := Editor;
    end;
    DC := Message.DC;
    if DC = 0 then DC := BeginPaint(Handle, PS);
    ACanvas.Handle := DC;
    try
      ACanvas.Font := Font;
      if not Enabled and NewStyleControls and not
        (csDesigning in ComponentState) and
        (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
        ACanvas.Font.Color := clGrayText;
      with ACanvas do begin
        R := ClientRect;
        if {$IFNDEF VER80} not (NewStyleControls and Ctl3D) and {$ENDIF}
          (BorderStyle = bsSingle) then
        begin
          Brush.Color := clWindowFrame;
          FrameRect(R);
          InflateRect(R, -1, -1);
        end;
        Brush.Color := Color;
        S := AText;
        AWidth := TextWidth(S);
        _Margins := EditorTextMargins(Editor);
        {$IFDEF RX_D10}
        if PopupVisible then ALeft := Editor.Margins.Left
        {$ELSE}
        if PopupVisible then ALeft := _Margins.X
        {$ENDIF}
        else
        begin
          if ButtonWidth > 0 then Inc(AWidth);
          case AAlignment of
            taLeftJustify:
              ALeft := _Margins.X;
            taRightJustify:
              ALeft := ClientWidth - ButtonWidth - AWidth - _Margins.X{Polaris - 2};
          else
              ALeft := (ClientWidth - ButtonWidth - AWidth) div 2;
          end;
        end;
        {$IFDEF RX_D4}
        if SysLocale.MiddleEast then UpdateTextFlags;
        {$ENDIF}
        TextRect(R, ALeft, _Margins.Y, S);
      end;
    finally
      ACanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

{ TEditButton }

constructor TEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStandart := True; // Polaris
{$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
{$ELSE}
  Style := bsWin31;
{$ENDIF}
  ParentShowHint := True;
end;

{$IFNDEF VER80}
procedure TEditButton.Paint;
{$IFDEF RX_D7}
{$IFDEF SPECDRAW}
//outer wire around box
var
  OuterRect: TRect;
  Box: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF}
{$ENDIF}
begin
  inherited Paint;
  if (FState <> rbsDown) then
    with Canvas do
    begin
      if NewStyleControls then Pen.Color := clBtnFace
      else Pen.Color := clBtnShadow;
      {$IFDEF RX_D7}
      with {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF} do
        if {$IFDEF RX_D16}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
        begin
          {$IFDEF SPECDRAW}
          OuterRect := Bounds(0, 0, Self.Width, Self.Height);
          if Enabled then
            Box := tbGroupBoxNormal
          else
            Box := tbGroupBoxDisabled;
          Details := {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF}.GetElementDetails(Box);
          {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF}.DrawElement(Handle, Details, OuterRect);
          {$ENDIF}
        end
        else
      {$ENDIF}
        begin
          MoveTo(0, 0);
          LineTo(0, Self.Height - 1);
          Pen.Color := clBtnHighlight;
          MoveTo(1, 1);
          LineTo(1, Self.Height - 2);
        end;
    end;
end;

{$ENDIF}

procedure TEditButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) then
    with TCustomComboEdit(Owner) do
    begin
      FNoAction := (FPopup <> nil) and FPopupVisible;
      if not FPopupVisible then
      begin
        if TabStop and CanFocus and (GetFocus <> Handle) then SetFocus;
      end
//      else PopupCloseUp(FPopup, True);
      else PopupCloseUp(FPopup, FStandart);
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TEditButton.Click;
begin
  if not FNoAction then inherited Click else FNoAction := False;
end;

{ TPopupWindow }

constructor TPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TWinControl(AOwner);
{$IFNDEF VER80}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable,
    csAcceptsControls];
{$ELSE}
  ControlStyle := ControlStyle + [csAcceptsControls];
{$ENDIF}
  Ctl3D := False;
  ParentCtl3D := False;
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TPopupWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
{$IFNDEF VER80}
    ExStyle := WS_EX_TOOLWINDOW;
{$ENDIF}
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

{$IFDEF VER80}
procedure TPopupWindow.CreateWnd;
begin
  inherited CreateWnd;
  if (csDesigning in ComponentState) then SetParent(nil);
end;
{$ENDIF}

procedure TPopupWindow.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

function TPopupWindow.GetPopupText: string;
begin
  Result := '';
end;

procedure TPopupWindow.InvalidateEditor;
var
  R: TRect;
begin
  if (FEditor is TCustomComboEdit) then
  begin
    with TCustomComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - FBtnControl.Width{Polaris - 2}, ClientHeight + 1);
  end
  else R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TPopupWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TPopupWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then FCloseUp(Self, Accept);
end;

procedure TPopupWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TPopupWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

{ TCustomComboEdit }

constructor TCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF RX_D3}
  ControlStyle := ControlStyle + [csCaptureMouse];
{$ENDIF}
//  AutoSize := False;   // Polaris
  FDirectInput := True;
  FClickKey := scAltDown;
  FPopupAlign := epaRight;
  FBtnControl := TWinControl.Create(Self);
{$IFNDEF VER80}
  with FBtnControl do
    ControlStyle := ControlStyle + [csReplicatable];
{$ENDIF}
  FBtnControl.Width := DefEditBtnWidth;
  FBtnControl.Height := 17;
  FBtnControl.Visible := True;
  FBtnControl.Parent := Self;
  FButton := TEditButton.Create(Self);
  FButton.SetBounds(0, 0, FBtnControl.Width, FBtnControl.Height);
  FButton.Visible := True;
  FButton.Parent := FBtnControl;
  TEditButton(FButton).OnClick := EditButtonClick;
  Height := 21;
  FDefNumGlyphs := 1;
  FGlyphKind := gkCustom;
end;

destructor TCustomComboEdit.Destroy;
begin
  FButton.OnClick := nil;
  inherited Destroy;
end;

procedure TCustomComboEdit.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style
    or ES_MULTILINE
    or WS_CLIPCHILDREN
    or Alignments[FAlignment];
end;

procedure TCustomComboEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TCustomComboEdit.HidePopup;
begin
  TPopupWindow(FPopup).Hide;
end;

procedure TCustomComboEdit.ShowPopup(Origin: TPoint);
begin
  TPopupWindow(FPopup).Show(Origin);
end;

procedure TCustomComboEdit.PopupDropDown(DisableEdit: Boolean);
var
  P: TPoint;
  Y: Integer;
begin
  if (FPopup <> nil) and not (ReadOnly or FPopupVisible) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FPopup.Height > Screen.Height then
      Y := P.Y - FPopup.Height;
    case FPopupAlign of
      epaRight:
        begin
          Dec(P.X, FPopup.Width - Width);
          if P.X < 0 then Inc(P.X, FPopup.Width - Width);
        end;
      epaLeft:
        begin
          if P.X + FPopup.Width > Screen.Width then
            Dec(P.X, FPopup.Width - Width);
        end;
    end;
    if P.X < 0 then P.X := 0
    else if P.X + FPopup.Width > Screen.Width then
      P.X := Screen.Width - FPopup.Width;
{$IFNDEF VER80}
    if Text <> '' then SetPopupValue(Text)
    else SetPopupValue(Null);
{$ELSE}
    SetPopupValue(Text);
{$ENDIF}
    if CanFocus then SetFocus;
    ShowPopup(Point(P.X, Y));
    FPopupVisible := True;
    if DisableEdit then
    begin
      inherited ReadOnly := True;
      HideCaret(Handle);
    end;
  end;
end;

procedure TCustomComboEdit.PopupCloseUp(Sender: TObject; Accept: Boolean);
var
{$IFNDEF VER80}
  AValue: Variant;
{$ELSE}
  AValue: string;
{$ENDIF}
begin
  if (FPopup <> nil) and FPopupVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    AValue := GetPopupValue;
    HidePopup;
    try
      try
        if CanFocus then
        begin
          SetFocus;
          if GetFocus = Handle then SetShowCaret;
        end;
      except
        { ignore exceptions }
      end;
      SetDirectInput(DirectInput);
      Invalidate;
      if Accept and AcceptPopup(AValue) and EditCanModify then
      begin
        AcceptValue(AValue);
        if FFocused then inherited SelectAll;
      end;
    finally
      FPopupVisible := False;
    end;
  end;
end;

procedure TCustomComboEdit.DoChange;
begin
  inherited Change;
end;

{$IFNDEF VER80}
function TCustomComboEdit.GetPopupValue: Variant;
{$ELSE}
function TCustomComboEdit.GetPopupValue: string;
{$ENDIF}
begin
  if FPopup <> nil then Result := TPopupWindow(FPopup).GetValue
  else Result := '';
end;

{$IFNDEF VER80}
procedure TCustomComboEdit.SetPopupValue(const Value: Variant);
{$ELSE}
procedure TCustomComboEdit.SetPopupValue(const Value: string);
{$ENDIF}
begin
  if FPopup <> nil then TPopupWindow(FPopup).SetValue(Value);
end;

{$IFNDEF VER80}
procedure TCustomComboEdit.AcceptValue(const Value: Variant);
begin
  if Text <> VarToStr(Value) then
  begin
{$ELSE}
procedure TCustomComboEdit.AcceptValue(const Value: string);
begin
  if Text <> Value then
  begin
{$ENDIF}
    Text := Value;
    Modified := True;
    UpdatePopupVisible;
    DoChange;
  end;
end;

{$IFNDEF VER80}
function TCustomComboEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TCustomComboEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
begin
  Result := True;
end;

function TCustomComboEdit.EditCanModify: Boolean;
begin
  Result := not FReadOnly;
end;

procedure TCustomComboEdit.Change;
begin
  if not PopupVisible then DoChange
  else PopupChange;
end;

procedure TCustomComboEdit.PopupChange;
begin
end;

type
  TCrackWin = class(TWinControl);

procedure TCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
//Polaris
var
  Form: TCustomForm;
begin
//Polaris
  Form := GetParentForm(Self);
  if (ssCtrl in Shift)  then
   case Key of
    VK_RETURN:
     if (Form <> nil) {and Form.KeyPreview} then
     begin
       TCrackWin(Form).KeyDown(Key, Shift);
       Key := 0;
      end;
    VK_TAB:
     if (Form <> nil){ and Form.KeyPreview} then
     begin
       TCrackWin(Form).KeyDown(Key, Shift);
       Key := 0;
     end;
   end;
//Original
  inherited KeyDown(Key, Shift);
  if (FClickKey = ShortCut(Key, Shift)) and (ButtonWidth > 0) then
  begin
    EditButtonClick(Self);
    Key := 0;
  end;
end;

procedure TCustomComboEdit.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);

//Polaris  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) or ((KEY=#10) and PopupVisible) then
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) or ((KEY=#10) and PopupVisible) then
  begin
    if PopupVisible then
    begin
//Polaris      PopupCloseUp(FPopup, Key = Char(VK_RETURN));
      PopupCloseUp(FPopup, Key <> Char(VK_ESCAPE));
      Key := #0;
    end
    else
    begin
      { must catch and remove this, since is actually multi-line }
      GetParentForm(Self).Perform(CM_DIALOGKEY, WPARAM(Key), 0);
      if Key = Char(VK_RETURN) then
      begin
        inherited KeyPress(Key);
        Key := #0;
        Exit;
      end;
    end;
  end;
//Polaris
  if {$IFDEF UNICODE}CharInSet(Key, [#10, #9]){$ELSE}Key in [#10, #9]{$ENDIF} then
  begin
    Key := #0;
    if (Form <> nil) {and Form.KeyPreview} then
      TCrackWin(Form).KeyPress(Key);
  end;
//Polaris
  inherited KeyPress(Key);
end;

procedure TCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FPopup <> nil) and (Button = mbLeft) then
  begin
    if CanFocus then SetFocus;
    if not FFocused then Exit;
    if FPopupVisible then PopupCloseUp(FPopup, False);
    {else if (not ReadOnly or AlwaysEnable) and (not DirectInput) then
      PopupDropDown(True);}
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

function TCustomComboEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

procedure TCustomComboEdit.SetButtonWidth(Value: Integer);
begin
  if ButtonWidth <> Value then
  begin
    FBtnControl.Visible := Value > 1;
    if (csCreating in ControlState) then
    begin
      FBtnControl.Width := Value;
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      RecreateGlyph;
    end
//    else if (Value <> ButtonWidth) and (Value < ClientWidth) then begin
//Polaris
    else if (Value <> ButtonWidth) and
            ((Assigned(Parent) and (Value < ClientWidth)) or
             (not Assigned(Parent) and (Value < Width)))
    then
    begin
      FButton.Width := Value;
      with FButton do
        ControlStyle := ControlStyle - [csFixedWidth];
      if HandleAllocated then RecreateWnd;
      RecreateGlyph;
    end;
  end;
end;

function TCustomComboEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TCustomComboEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

function TCustomComboEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TCustomComboEdit.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FGlyphKind := gkCustom;
end;

function TCustomComboEdit.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TCustomComboEdit.SetNumGlyphs(Value: TNumGlyphs);
begin
  if FGlyphKind in [gkDropDown, gkEllipsis] then FButton.NumGlyphs := 1
  else if FGlyphKind = gkDefault then FButton.NumGlyphs := FDefNumGlyphs
  else FButton.NumGlyphs := Value;
end;

procedure TCustomComboEdit.SetEditRect;
var
  Loc: TRect;
begin
  SetRect(Loc, 0, 0, ClientWidth - FBtnControl.Width{Polaris - 2}, ClientHeight + 1);
  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
//Polaris
//  SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, FBtnControl.Width));
end;

procedure TCustomComboEdit.UpdateBtnBounds;
var
  BtnRect: TRect;
begin
{$IFNDEF VER80}
  if NewStyleControls then
  begin
    if Ctl3D and (BorderStyle = bsSingle) then
      BtnRect := Bounds(Width - FButton.Width - 4, 0,
        FButton.Width, Height - 4)
    else
    begin
      if BorderStyle = bsSingle then
        BtnRect := Bounds(Width - FButton.Width - 2, 2,
          FButton.Width, Height - 4)
      else
        BtnRect := Bounds(Width - FButton.Width, 0,
          FButton.Width, Height);
    end;
  end
  else
    BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
{$ELSE}
  BtnRect := Bounds(Width - FButton.Width, 0, FButton.Width, Height);
{$ENDIF}
  with BtnRect do
    FBtnControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
  FButton.Height := FBtnControl.Height;
  SetEditRect;
end;

{$IFNDEF VER80}
procedure TCustomComboEdit.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  UpdateBtnBounds;
end;
{$ENDIF}

procedure TCustomComboEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    MinHeight := GetMinHeight;
    { text edit bug: if size to less than MinHeight, then edit ctrl does
      not display the text }
    if Height < MinHeight then
    begin
      Height := MinHeight;
      Exit;
    end;
  end
  else
  begin
    if (FPopup <> nil) and (csDesigning in ComponentState) then
      FPopup.SetBounds(0, Height + 1, 10, 10);
  end;
  UpdateBtnBounds;
end;

function TCustomComboEdit.GetTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  try
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
  finally
    ReleaseDC(0, DC);
  end;
//  Result := Min(SysMetrics.tmHeight, Metrics.tmHeight);  // Polaris
  Result := Metrics.tmHeight;                              // Polaris
end;

function TCustomComboEdit.GetMinHeight: Integer;
var
  I: Integer;
begin
  I := GetTextHeight;
  Result := I + GetSystemMetrics(SM_CYBORDER) * 4 +
    1 {$IFDEF VER80} + (I div 4) {$ENDIF};
end;

procedure TCustomComboEdit.UpdatePopupVisible;
begin
  FPopupVisible := (FPopup <> nil) and FPopup.Visible;
end;

function TCustomComboEdit.GetPopupVisible: Boolean;
begin
  Result := (FPopup <> nil) and FPopupVisible;
end;

procedure TCustomComboEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then SetEditRect;
end;

procedure TCustomComboEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FPopup) and
    (Message.Sender <> FButton) and ((FPopup <> nil) and
    not FPopup.ContainsControl(Message.Sender)) then
    PopupCloseUp(FPopup, False);
end;

procedure TCustomComboEdit.CMEnter(var Message: TMessage);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

procedure TCustomComboEdit.CNCtlColor(var Message: TMessage);
var
  TextColor: Longint;
begin
  inherited;
  if NewStyleControls then
  begin
    TextColor := ColorToRGB(Font.Color);
    if not Enabled and (ColorToRGB(Color) <> ColorToRGB(clGrayText)) then
      TextColor := ColorToRGB(clGrayText);
    SetTextColor(Message.WParam, TextColor);
  end;
end;

procedure TCustomComboEdit.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FFocused := False;
  PopupCloseUp(FPopup, False);
end;

procedure TCustomComboEdit.WMSetFocus(var Message: TMessage);
begin
  inherited;
  FFocused := True;
  SetShowCaret;
end;

{$IFDEF RX_D4}
procedure TCustomComboEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FPopup <> nil then FPopup.BiDiMode := BiDiMode;
end;
{$ENDIF}

procedure TCustomComboEdit.SetShowCaret;
const
  CaretWidth: array[Boolean] of Byte = (1, 2);
begin
  CreateCaret(Handle, 0, CaretWidth[fsBold in Font.Style], GetTextHeight);
  ShowCaret(Handle);
end;

procedure TCustomComboEdit.EditButtonClick(Sender: TObject);
begin
  if (not FReadOnly) or AlwaysEnable then ButtonClick;
end;

procedure TCustomComboEdit.DoClick;
begin
  EditButtonClick(Self);
end;

procedure TCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
  if FPopup <> nil then
  begin
    if FPopupVisible then PopupCloseUp(FPopup, True) else PopupDropDown(True);
  end;
end;

procedure TCustomComboEdit.SelectAll;
begin
  if DirectInput then inherited SelectAll;
end;

function TCustomComboEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

procedure TCustomComboEdit.SetDirectInput(Value: Boolean);
begin
  inherited ReadOnly := not Value or FReadOnly;
  FDirectInput := Value;
end;

procedure TCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

procedure TCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if not FDirectInput or ReadOnly then Exit;
  inherited;
end;

function TCustomComboEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

procedure TCustomComboEdit.SetReadOnly(Value: Boolean);
begin
  if Value <> FReadOnly then
  begin
    FReadOnly := Value;
    inherited ReadOnly := Value or not FDirectInput;
  end;
end;

procedure TCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

function TCustomComboEdit.BtnWidthStored: Boolean;
begin
  if (FGlyphKind = gkDefault) and (Glyph <> nil) then
    Result := ButtonWidth <> Max(Glyph.Width div FButton.NumGlyphs + 6,
      DefEditBtnWidth)
  else if FGlyphKind = gkDropDown then
    Result := ButtonWidth <> GetSystemMetrics(SM_CXVSCROLL)
      {$IFDEF VER80} + 1{$ENDIF}
  else Result := ButtonWidth <> DefEditBtnWidth;
end;

function TCustomComboEdit.IsCustomGlyph: Boolean;
begin
  Result := FGlyphKind = gkCustom;
end;

procedure TCustomComboEdit.SetGlyphKind(Value: TGlyphKind);
begin
  if FGlyphKind <> Value then
  begin
    FGlyphKind := Value;
    if (FGlyphKind = gkCustom) and (csReading in ComponentState) then
    begin
      Glyph := nil;
    end;
    RecreateGlyph;
    if (FGlyphKind = gkDefault) and (Glyph <> nil) then
      ButtonWidth := Max(Glyph.Width div FButton.NumGlyphs + 6, FButton.Width)
    else if FGlyphKind = gkDropDown then
    begin
      ButtonWidth := GetSystemMetrics(SM_CXVSCROLL){$IFDEF VER80} + 1{$ENDIF};
      with FButton do
        ControlStyle := ControlStyle + [csFixedWidth];
    end;
  end;
end;

function TCustomComboEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  Result := nil;
end;

procedure TCustomComboEdit.RecreateGlyph;

  function CreateEllipsisGlyph: TBitmap;
  var
    W, G, I: Integer;
  begin
    Result := TBitmap.Create;
    with Result do
    try
      Monochrome := True;
      Width := Max(1, FButton.Width - 6);
      Height := 4;
      W := 2;
      G := (Result.Width - 3 * W) div 2;
      if G <= 0 then G := 1;
      if G > 3 then G := 3;
      I := (Width - 3 * W - 2 * G) div 2;
      PatBlt(Canvas.Handle, I, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + G + W, 1, W, W, BLACKNESS);
      PatBlt(Canvas.Handle, I + 2 * G + 2 * W, 1, W, W, BLACKNESS);
    except
      Free;
      raise;
    end;
  end;

var
  NewGlyph: TBitmap;
  DestroyNeeded: Boolean;
begin
  case FGlyphKind of
    gkDefault:
      begin
        DestroyNeeded := False;
        NewGlyph := GetDefaultBitmap(DestroyNeeded);
        try
          FButton.Glyph.Assign(NewGlyph);
          NumGlyphs := FDefNumGlyphs;
        finally
          if DestroyNeeded then NewGlyph.Destroy;
        end;
      end;
    gkDropDown:
      begin
        FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
        NumGlyphs := 1;
      end;
    gkEllipsis:
      begin
        NewGlyph := CreateEllipsisGlyph;
        try
          FButton.Glyph := NewGlyph;
          NumGlyphs := 1;
        finally
          NewGlyph.Destroy;
        end;
      end;
  end;
end;

const
  FileBitmap: TBitmap = nil;
  DateBitmap: TBitmap = nil;

{ TFileDirEdit }

constructor TFileDirEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OEMConvert := True;
{$IFDEF VER80}
  MaxLength := MaxFileLength;
{$ENDIF}
  ControlState := ControlState + [csCreating];
  try
    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

function TFileDirEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if FileBitmap = nil then
  begin
    FileBitmap := TBitmap.Create;
    FileBitmap.Handle := LoadBitmap(hInstance, sFileBmp);
  end;
  Result := FileBitmap;
end;

procedure TFileDirEdit.DoBeforeDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnBeforeDialog) then FOnBeforeDialog(Self, FileName, Action);
end;

procedure TFileDirEdit.DoAfterDialog(var FileName: string;
  var Action: Boolean);
begin
  if Assigned(FOnAfterDialog) then FOnAfterDialog(Self, FileName, Action);
end;

procedure TFileDirEdit.CreateHandle;
begin
  inherited CreateHandle;
  if FAcceptFiles then SetDragAccept(True);
end;

procedure TFileDirEdit.DestroyWindowHandle;
begin
  SetDragAccept(False);
  inherited DestroyWindowHandle;
end;

procedure TFileDirEdit.SetDragAccept(Value: Boolean);
begin
  if not (csDesigning in ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Value);
end;

procedure TFileDirEdit.SetAcceptFiles(Value: Boolean);
begin
  if FAcceptFiles <> Value then
  begin
    SetDragAccept(Value);
    FAcceptFiles := Value;
  end;
end;

procedure TFileDirEdit.DisableSysErrors;
begin
  FErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
end;

procedure TFileDirEdit.EnableSysErrors;
begin
  SetErrorMode(FErrMode);
  FErrMode := 0;
end;

procedure TFileDirEdit.WMDropFiles(var Msg: TWMDropFiles);
var
  AFileName: array[0..255] of Char;
  I, Num: Cardinal;
begin
  Msg.Result := 0;
  try
{$IFNDEF VER80}
    Num := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
{$ELSE}
    Num := DragQueryFile(Msg.Drop, $FFFF, nil, 0);
{$ENDIF}
    if Num > 0 then
    begin
      ClearFileList;
      for I := 0 to Num - 1 do
      begin
        DragQueryFile(Msg.Drop, I, PChar(@AFileName), Pred(Length(AFileName)));
        ReceptFileDir(StrPas(AFileName));
        if not FMultipleDirs then Break;
      end;
      if Assigned(FOnDropFiles) then FOnDropFiles(Self);
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TFileDirEdit.ClearFileList;
begin
end;

{ TFilenameEdit }

function ClipFilename(const FileName: string): string;
var
  Params: string;
begin
  if FileExists(FileName) then Result := FileName
  else SplitCommandLine(FileName, Result, Params);
end;

function ExtFilename(const FileName: string): string;
begin
  if (Pos(' ', FileName) > 0) and (FileName[1] <> '"') then
    Result := Format('"%s"', [FileName])
  else Result := FileName;
end;

constructor TFilenameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateEditDialog;
end;

procedure TFilenameEdit.CreateEditDialog;
var
  NewDialog: TOpenDialog;
begin
  case FDialogKind of
    dkOpen: NewDialog := TOpenDialog.Create(Self);
{$IFDEF RX_D3}
    dkOpenPicture: NewDialog := TOpenPictureDialog.Create(Self);
    dkSavePicture: NewDialog := TSavePictureDialog.Create(Self);
{$ENDIF}
    else {dkSave} NewDialog := TSaveDialog.Create(Self);
  end;
  try
    if FDialog <> nil then
    begin
      with NewDialog do
      begin
        DefaultExt := FDialog.DefaultExt;
        FileEditStyle := FDialog.FileEditStyle;
        FileName := FDialog.FileName;
        Filter := FDialog.Filter;
        FilterIndex := FDialog.FilterIndex;
        InitialDir := FDialog.InitialDir;
        HistoryList := FDialog.HistoryList;
        Files.Assign(FDialog.Files);
        Options := FDialog.Options;
        Title := FDialog.Title;
      end;
      FDialog.Free;
    end
    else
    begin
      NewDialog.Title := RxLoadStr(SBrowse);
      NewDialog.Filter := RxLoadStr(SDefaultFilter);
      NewDialog.Options := [ofHideReadOnly];
    end;
  finally
    FDialog := NewDialog;
  end;
end;

function TFilenameEdit.IsCustomTitle: Boolean;
begin
  Result := CompareStr(RxLoadStr(SBrowse), FDialog.Title) <> 0;
end;

function TFilenameEdit.IsCustomFilter: Boolean;
begin
  Result := CompareStr(RxLoadStr(SDefaultFilter), FDialog.Filter) <> 0;
end;

procedure TFilenameEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := inherited Text;
  Action := True;
  Temp := ClipFilename(Temp);
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if ValidFileName(Temp) then
    try
      if DirExists(ExtractFilePath(Temp)) then
        SetInitialDir(ExtractFilePath(Temp));
      if (ExtractFileName(Temp) = '') or not ValidFileName(ExtractFileName(Temp)) then
        Temp := '';
      FDialog.FileName := Temp;
    except
      { ignore any exceptions }
    end;
  FDialog.HelpContext := Self.HelpContext;
  DisableSysErrors;
  try
    Action := FDialog.Execute;
  finally
    EnableSysErrors;
  end;
  if Action then Temp := FDialog.FileName;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then
  begin
    inherited Text := ExtFilename(Temp);
    SetInitialDir(ExtractFilePath(FDialog.FileName));
  end;
end;

function TFilenameEdit.GetFileName: string;
begin
  Result := ClipFilename(inherited Text);
end;

procedure TFilenameEdit.SetFileName(const Value: string);
begin
  if (Value = '') or ValidFileName(ClipFilename(Value)) then
  begin
    inherited Text := ExtFilename(Value);
    ClearFileList;
  end
  else raise EComboEditError.CreateFmt(ResStr({$IFDEF RX_D26}SInvalidKnownFilename{$ELSE}SInvalidFilename{$ENDIF}), [Value]);
end;

{$IFNDEF VER80}

function TFilenameEdit.GetLongName: string;
begin
  Result := ShortToLongFileName(FileName);
end;

function TFilenameEdit.GetShortName: string;
begin
  Result := LongToShortFileName(FileName);
end;

{$ENDIF}

procedure TFilenameEdit.ClearFileList;
begin
  FDialog.Files.Clear;
end;

procedure TFilenameEdit.ReceptFileDir(const AFileName: string);
begin
  if FMultipleDirs then
  begin
    if FDialog.Files.Count = 0 then SetFileName(AFileName);
    FDialog.Files.Add(AFileName);
  end
  else SetFileName(AFileName);
end;

function TFilenameEdit.GetDialogFiles: TStrings;
begin
  Result := FDialog.Files;
end;

function TFilenameEdit.GetDefaultExt: TFileExt;
begin
  Result := FDialog.DefaultExt;
end;

function TFilenameEdit.GetFileEditStyle: TFileEditStyle;
begin
  Result := FDialog.FileEditStyle;
end;

function TFilenameEdit.GetFilter: string;
begin
  Result := FDialog.Filter;
end;

function TFilenameEdit.GetFilterIndex: Integer;
begin
  Result := FDialog.FilterIndex;
end;

function TFilenameEdit.GetInitialDir: string;
begin
  Result := FDialog.InitialDir;
end;

function TFilenameEdit.GetHistoryList: TStrings;
begin
  Result := FDialog.HistoryList;
end;

function TFilenameEdit.GetOptions: TOpenOptions;
begin
  Result := FDialog.Options;
end;

function TFilenameEdit.GetDialogTitle: string;
begin
  Result := FDialog.Title;
end;

procedure TFilenameEdit.SetDialogKind(Value: TFileDialogKind);
begin
  if FDialogKind <> Value then
  begin
    FDialogKind := Value;
    CreateEditDialog;
  end;
end;

procedure TFilenameEdit.SetDefaultExt(Value: TFileExt);
begin
  FDialog.DefaultExt := Value;
end;

procedure TFilenameEdit.SetFileEditStyle(Value: TFileEditStyle);
begin
  FDialog.FileEditStyle := Value;
end;

procedure TFilenameEdit.SetFilter(const Value: string);
begin
  FDialog.Filter := Value;
end;

procedure TFilenameEdit.SetFilterIndex(Value: Integer);
begin
  FDialog.FilterIndex := Value;
end;

procedure TFilenameEdit.SetInitialDir(const Value: string);
begin
  FDialog.InitialDir := Value;
end;

procedure TFilenameEdit.SetHistoryList(Value: TStrings);
begin
  FDialog.HistoryList := Value;
end;

procedure TFilenameEdit.SetOptions(Value: TOpenOptions);
begin
  if Value <> FDialog.Options then
  begin
    FDialog.Options := Value;
    FMultipleDirs := ofAllowMultiSelect in FDialog.Options;
    if not FMultipleDirs then ClearFileList;
  end;
end;

procedure TFilenameEdit.SetDialogTitle(const Value: string);
begin
  FDialog.Title := Value;
end;

{ TDirectoryEdit }

constructor TDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
end;

procedure TDirectoryEdit.ButtonClick;
var
  Temp: string;
  Action: Boolean;
begin
  inherited ButtonClick;
  Temp := Text;
  Action := True;
  DoBeforeDialog(Temp, Action);
  if not Action then Exit;
  if (Temp = '') then
  begin
    if (InitialDir <> '') then Temp := InitialDir
    else Temp := {$IFDEF RX_D6}PathDelim{$ELSE}'\'{$ENDIF};
  end;
  if not DirExists(Temp) then Temp := {$IFDEF RX_D6}PathDelim{$ELSE}'\'{$ENDIF};
  DisableSysErrors;
  try
{$IFNDEF VER80}
    if NewStyleControls and (DialogKind = dkWin32) then
      Action := BrowseDirectory(Temp, FDialogText, Self.HelpContext)
    else Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
{$ELSE}
    Action := SelectDirectory(Temp, FOptions, Self.HelpContext);
{$ENDIF}
  finally
    EnableSysErrors;
  end;
  if CanFocus then SetFocus;
  DoAfterDialog(Temp, Action);
  if Action then
  begin
    SelText := '';
    if (Text = '') or not MultipleDirs then Text := Temp
    else Text := Text + ';' + Temp;
    if (Temp <> '') and DirExists(Temp) then InitialDir := Temp;
  end;
end;

procedure TDirectoryEdit.ReceptFileDir(const AFileName: string);
var
  Temp: string;
begin
  if FileExists(AFileName) then Temp := ExtractFilePath(AFileName)
  else Temp := AFileName;
  if (Text = '') or not MultipleDirs then Text := Temp
  else Text := Text + ';' + Temp;
end;

{$IFNDEF VER80}

function TDirectoryEdit.GetLongName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := ShortToLongPath(Text)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do
    begin
      Temp := ShortToLongPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

function TDirectoryEdit.GetShortName: string;
var
  Temp: string;
  Pos: Integer;
begin
  if not MultipleDirs then
    Result := LongToShortPath(Text)
  else
  begin
    Result := '';
    Pos := 1;
    while Pos <= Length(Text) do
    begin
      Temp := LongToShortPath(ExtractSubstr(Text, Pos, [';']));
      if (Result <> '') and (Temp <> '') then Result := Result + ';';
      Result := Result + Temp;
    end;
  end;
end;

{$ENDIF}

{ TCustomDateEdit }

function NvlDate(DateValue, DefaultValue: TDateTime): TDateTime;
begin
  if DateValue = NullDate then Result := DefaultValue
  else Result := DateValue;
end;

constructor TCustomDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
// Polaris
  FDateAutoBetween := True;
  FMinDate := NullDate;
  FMaxDate := NullDate;
  FErrorRaise := False;
  FBlanksChar := ' ';
{$IFDEF RX_D4}   // Polaris
  FTitle := RxLoadStr(SDateDlgTitle);
{$ELSE}
  FTitle := NewStr(RxLoadStr(SDateDlgTitle));
{$ENDIF}
  FPopupColor := clBtnFace;
  FDefNumGlyphs := 2;
  FStartOfWeek := Mon;
  FWeekends := [Sun];
  FWeekendColor := clRed;
  FYearDigits := dyDefault;
  FCalendarHints := TStringList.Create;
  TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  ControlState := ControlState + [csCreating];
  try
    UpdateFormat;
{$IFDEF DEFAULT_POPUP_CALENDAR}
    FPopup := TPopupWindow(CreatePopupCalendar(Self,
      {$IFDEF RX_D4} BiDiMode, {$ENDIF}
// Polaris
      FMinDate, FMaxDate

      ));
    TPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
    TPopupWindow(FPopup).Color := FPopupColor;
{$ENDIF DEFAULT_POPUP_CALENDAR}
    GlyphKind := gkDefault; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TCustomDateEdit.Destroy;
begin
  if FHooked then
  begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  if FPopup <> nil then TPopupWindow(FPopup).OnCloseUp := nil;
  FPopup.Free;
  FPopup := nil;
  TStringList(FCalendarHints).OnChange := nil;
  FCalendarHints.Free;
  FCalendarHints := nil;
{$IFNDEF RX_D4}   // Polaris
  DisposeStr(FTitle);
{$ENDIF}
  inherited Destroy;
end;

procedure TCustomDateEdit.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if Handle <> 0 then
  begin
    UpdateMask;
    if not (csDesigning in ComponentState) and not (IsLibrary or FHooked) then
    begin
      Application.HookMainWindow(FormatSettingsChange);
      FHooked := True;
    end;
  end;
end;

procedure TCustomDateEdit.DestroyWindowHandle;
begin
  if FHooked then
  begin
    Application.UnhookMainWindow(FormatSettingsChange);
    FHooked := False;
  end;
  inherited DestroyWindowHandle;
end;

procedure TCustomDateEdit.UpdateFormat;
begin
  FDateFormat := DefDateFormat(FourDigitYear);
end;

function TCustomDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TCustomDateEdit.TextStored: Boolean;
begin
  Result := not IsEmptyStr(Text, [#0, ' ', {$IFDEF RX_D15}FormatSettings.{$ENDIF}DateSeparator, FBlanksChar]);
end;

// Polaris
function TCustomDateEdit.StoreMinDate: Boolean;
begin
  Result := FMinDate <> NullDate;
end;

function TCustomDateEdit.StoreMaxDate: Boolean;
begin
  Result := FMaxDate <> NullDate;
end;
// Polaris

procedure TCustomDateEdit.CheckValidDate;
begin
  if TextStored then
    try
      FFormatting := True;
      try
        SetDate(StrToDateFmt(FDateFormat, Text));
      finally
        FFormatting := False;
      end;
    except
      if not FErrorRaise then
        ShowMessage(Format({$IFDEF RX_D12}SysConst.{$ENDIF}SInvalidDate, [Text]))
      else
      begin
        Self.Date := 0;
        Invalidate;
      end;
      if CanFocus then SetFocus;
      if FErrorRaise then
        raise;
    end;
end;

procedure TCustomDateEdit.Change;
begin
  if not FFormatting then inherited Change;
end;

procedure TCustomDateEdit.CMExit(var Message: TCMExit);
begin
  if not (csDesigning in ComponentState) and CheckOnExit then
    CheckValidDate;
  inherited;
end;

function TCustomDateEdit.GetDefaultBitmap(var DestroyNeeded: Boolean): TBitmap;
begin
  DestroyNeeded := False;
  if DateBitmap = nil then
  begin
    DateBitmap := TBitmap.Create;
    DateBitmap.Handle := LoadBitmap(hInstance, sDateBmp);
  end;
  Result := DateBitmap;
end;

procedure TCustomDateEdit.SetBlanksChar(Value: Char);
begin
  if Value <> FBlanksChar then
  begin
    if (Value < ' ') then Value := ' ';
    FBlanksChar := Value;
    UpdateMask;
  end;
end;

procedure TCustomDateEdit.UpdateMask;
var
  DateValue: TDateTime;
  OldFormat: string{$IFNDEF RX_D12}[10]{$ENDIF};
begin
  DateValue := GetDate;
  OldFormat := FDateFormat;
  UpdateFormat;
  if (GetDateMask <> EditMask) or (OldFormat <> FDateFormat) then
  begin
    { force update }
    EditMask := '';
    EditMask := GetDateMask;
  end;
  UpdatePopup;
  SetDate(DateValue);
end;

function TCustomDateEdit.FormatSettingsChange(var Message: TMessage): Boolean;
begin
  Result := False;
  if (Message.Msg = WM_WININICHANGE)
    {$IFNDEF VER80} and Application.UpdateFormatSettings {$ENDIF} then
    UpdateMask;
end;

function TCustomDateEdit.FourDigitYear: Boolean;
begin
  Result := (FYearDigits = dyFour) or ((FYearDigits = dyDefault) and
    (RxDateUtil.FourDigitYear));
end;

function TCustomDateEdit.GetDateMask: string;
begin
  Result := DefDateMask(FBlanksChar, FourDigitYear);
end;

// Polaris
procedure TCustomDateEdit.SetMinDate(Value: TDateTime);
begin
  if Value <> FMinDate then
  begin
    if (Value <> NullDate) and (FMaxDate <> NullDate) and (Value > FMaxDate)
    then if FDateAutoBetween then SetMaxDate(Value)
    else raise Exception.CreateFmt(RxLoadStr(SDateMinLimit),[DateToStr(FMaxDate)]);
    FMinDate := Value;
    UpdatePopup;
    if FDateAutoBetween
      then SetDate(Date);
  end;
end;

procedure TCustomDateEdit.SetMaxDate(Value: TDateTime);
begin
  if Value <> FMaxDate then
  begin
    if (Value <> NullDate) and (FMinDate <> NullDate) and (Value < FMinDate)
    then if FDateAutoBetween then SetMinDate(Value)
    else raise Exception.CreateFmt(RxLoadStr(SDateMinLimit),[DateToStr(FMinDate)]);
    FMaxDate := Value;
    UpdatePopup;
    if FDateAutoBetween
      then SetDate(Date);
  end;
end;

function TCustomDateEdit.GetDate: TDateTime;
begin
  if DefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  Result := StrToDateFmtDef(FDateFormat, Text, Result);
end;

{$IFDEF RX_D4}
procedure TCustomDateEdit.ValidateEdit;
begin
  if TextStored
  then CheckValidDate;
end;
{$ENDIF}
// Polaris

procedure TCustomDateEdit.SetDate(Value: TDateTime);
var
  D: TDateTime;
begin
  if not ValidDate(Value) or (Value = NullDate) then
  begin
    if DefaultToday then Value := SysUtils.Date
    else Value := NullDate;
  end;
  D := Date;
  TestDateBetween(Value);
  if Value = NullDate then Text := ''
  else Text := FormatDateTime(FDateFormat, Value);
  Modified := D <> Date;
end;

procedure TCustomDateEdit.SetDateAutoBetween(Value: Boolean);
var
  D: TDateTime;
begin
//
  if Value <> FDateAutoBetween then
  begin
    FDateAutoBetween := Value;
    if Value then
    begin
      D := Date;
      TestDateBetween(D);
      if D <> Date then Date := D;
    end;
    Invalidate;
  end;
end;

procedure TCustomDateEdit.TestDateBetween(var Value: TDateTime);
begin
  if FDateAutoBetween then
  begin
    if (FMinDate <> NullDate) and (Value <> NullDate) and (Value < FMinDate)
    then Value := FMinDate;
    if (FMaxDate <> NullDate) and (Value <> NullDate) and (Value > FMaxDate)
    then Value := FMaxDate;
  end;
end;

procedure TCustomDateEdit.ApplyDate(Value: TDateTime);
begin
  SetDate(Value);
  SelectAll;
end;

function TCustomDateEdit.GetDialogTitle: string;
begin
{$IFDEF RX_D4}   // Polaris
  Result := FTitle;
{$ELSE}
  Result := FTitle^;
{$ENDIF}
end;

procedure TCustomDateEdit.SetDialogTitle(const Value: string);
begin
{$IFDEF RX_D4}   // Polaris
  FTitle := Value;
{$ELSE}
  AssignStr(FTitle, Value);
{$ENDIF}
end;

function TCustomDateEdit.IsCustomTitle: Boolean;
begin
  Result := (CompareStr(RxLoadStr(SDateDlgTitle), DialogTitle) <> 0) and
    (DialogTitle <> EmptyStr);    // Polaris
end;

procedure TCustomDateEdit.UpdatePopup;
begin
  if FPopup <> nil then SetupPopupCalendar(FPopup, FStartOfWeek,
    FWeekends, FWeekendColor, FCalendarHints, FourDigitYear,
// Polaris
    FMinDate,
    FMaxDate
// Polaris
    );
end;

procedure TCustomDateEdit.SetYearDigits(Value: TYearDigits);
begin
  if FYearDigits <> Value then
  begin
    FYearDigits := Value;
    UpdateMask;
  end;
end;

function TCustomDateEdit.GetPopupColor: TColor;
begin
  if FPopup <> nil then Result := TPopupWindow(FPopup).Color
  else Result := FPopupColor;
end;

procedure TCustomDateEdit.SetPopupColor(Value: TColor);
begin
  if Value <> PopupColor then
  begin
    if FPopup <> nil then TPopupWindow(FPopup).Color := Value;
    FPopupColor := Value;
  end;
end;

function TCustomDateEdit.GetCalendarStyle: TCalendarStyle;
begin
  if FPopup <> nil then Result := csPopup
  else Result := csDialog;
end;

procedure TCustomDateEdit.SetCalendarStyle(Value: TCalendarStyle);
begin
  if Value <> CalendarStyle then
  begin
    case Value of
      csPopup:
        begin
          if FPopup = nil then
            FPopup := TPopupWindow(CreatePopupCalendar(Self,
              {$IFDEF RX_D4} BiDiMode, {$ENDIF}
             FMinDate, FMaxDate    // Polaris
              ));
          TPopupWindow(FPopup).OnCloseUp := PopupCloseUp;
          TPopupWindow(FPopup).Color := FPopupColor;
          UpdatePopup;
        end;
      csDialog:
        begin
          FPopup.Free;
          FPopup := nil;
        end;
    end;
  end;
end;

procedure TCustomDateEdit.SetCalendarHints(Value: TStrings);
begin
  FCalendarHints.Assign(Value);
end;

procedure TCustomDateEdit.CalendarHintsChanged(Sender: TObject);
begin
  TStringList(FCalendarHints).OnChange := nil;
  try
    while (FCalendarHints.Count > 4) do
      FCalendarHints.Delete(FCalendarHints.Count - 1);
  finally
    TStringList(FCalendarHints).OnChange := CalendarHintsChanged;
  end;
  if not (csDesigning in ComponentState) then UpdatePopup;
end;

procedure TCustomDateEdit.SetWeekendColor(Value: TColor);
begin
  if Value <> FWeekendColor then
  begin
    FWeekendColor := Value;
    UpdatePopup;
  end;
end;

procedure TCustomDateEdit.SetWeekends(Value: TDaysOfWeek);
begin
  if Value <> FWeekends then
  begin
    FWeekends := Value;
    UpdatePopup;
  end;
end;

procedure TCustomDateEdit.SetStartOfWeek(Value: TDayOfWeekName);
begin
  if Value <> FStartOfWeek then
  begin
    FStartOfWeek := Value;
    UpdatePopup;
  end;
end;

procedure TCustomDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN,
    VK_ADD, VK_SUBTRACT]) and
    PopupVisible then
  begin
    TPopupWindow(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end
  else if (Shift = []) and DirectInput then
  begin
    case Key of
      VK_ADD:
        begin
          ApplyDate(NvlDate(Date, Now) + 1);
          Key := 0;
        end;
      VK_SUBTRACT:
        begin
          ApplyDate(NvlDate(Date, Now) - 1);
          Key := 0;
        end;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCustomDateEdit.KeyPress(var Key: Char);
begin
  if CharInSet(Key, ['T', 't', '+', '-']) and PopupVisible then
  begin
    TPopupWindow(FPopup).KeyPress(Key);
    Key := #0;
  end
  else if DirectInput then
  begin
    case Key of
      'T', 't':
        begin
          ApplyDate(Trunc(Now));
          Key := #0;
        end;
      '+', '-':
        begin
          Key := #0;
        end;
    end;
  end;
  inherited KeyPress(Key);
end;

procedure TCustomDateEdit.ButtonClick;
var
  D: TDateTime;
  Action: Boolean;
begin
  inherited ButtonClick;
  if CalendarStyle = csDialog then
  begin
    D := Self.Date;
    Action := SelectDate(Self, D, DialogTitle, FStartOfWeek, FWeekends, // Polaris (Self added)
      FWeekendColor, FCalendarHints,
      FMinDate, FMaxDate);   // Polaris
    if CanFocus then SetFocus;
    if Action then
    begin
      if Assigned(FOnAcceptDate) then FOnAcceptDate(Self, D, Action);
      if Action then
      begin
        Self.Date := D;
        if FFocused then inherited SelectAll;
      end;
    end;
  end;
end;

{$IFNDEF VER80}
function TCustomDateEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TCustomDateEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
var
  D: TDateTime;
begin
  Result := True;
  if Assigned(FOnAcceptDate) then
  begin
{$IFNDEF VER80}
    if VarIsNull(Value) or VarIsEmpty(Value) then D := NullDate
    else
      try
        D := VarToDateTime(Value);
      except
        if DefaultToday then D := SysUtils.Date else D := NullDate;
      end;
{$ELSE}
    if DefaultToday then D := SysUtils.Date else D := NullDate;
    D := StrToDateDef(Value, D);
{$ENDIF}
    FOnAcceptDate(Self, D, Result);
{$IFNDEF VER80}
    if Result then Value := VarFromDateTime(D);
{$ELSE}
    if Result then Value := FormatDateTime(FDateFormat, D);
{$ENDIF}
  end;
end;

{$IFNDEF VER80}
procedure TCustomDateEdit.SetPopupValue(const Value: Variant);
begin
  inherited SetPopupValue(StrToDateFmtDef(FDateFormat, VarToStr(Value),
    SysUtils.Date));
end;

procedure TCustomDateEdit.AcceptValue(const Value: Variant);
begin
  SetDate(VarToDateTime(Value));
  UpdatePopupVisible;
  if Modified then inherited Change;
end;
{$ENDIF}

{ TDateEdit }

constructor TDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateMask;
end;

// Polaris
procedure TDateEdit.SetDate(Value: TDateTime);
begin
  if not FDateAutoBetween then
    if Value <> NullDate then
    begin
      if ((FMinDate <> NullDate) and (FMaxDate <> NullDate) and
         ((Value < FMinDate) or (Value > FMaxDate))) then
        raise Exception.CreateFmt(RxLoadStr(SDateOutOfRange),[FormatDateTime(FDateFormat, Value),FormatDateTime(FDateFormat, FMinDate), FormatDateTime(FDateFormat, FMaxDate)])
      else if ((FMinDate <> NullDate) and (Value < FMinDate)) then
        raise Exception.CreateFmt(RxLoadStr(SDateOutOfMin),[FormatDateTime(FDateFormat, Value),FormatDateTime(FDateFormat,FMinDate)])
      else if ((FMaxDate <> NullDate) and (Value > FMaxDate)) then
        raise Exception.CreateFmt(RxLoadStr(SDateOutOfMax),[FormatDateTime(FDateFormat, Value),FormatDateTime(FDateFormat,FMaxDate)]);
    end; 
  inherited SetDate(Value);
end;
// Polaris

{ Utility routines }

procedure DateFormatChanged;

  procedure IterateControls(AControl: TWinControl);
  var
    I: Integer;
  begin
    with AControl do
      for I := 0 to ControlCount - 1 do
      begin
        if Controls[I] is TCustomDateEdit then
          TCustomDateEdit(Controls[I]).UpdateMask
        else if Controls[I] is TWinControl then
          IterateControls(TWinControl(Controls[I]));
      end;
  end;

var
  I: Integer;
begin
  if Screen <> nil then
    for I := 0 to Screen.FormCount - 1 do
      IterateControls(Screen.Forms[I]);
end;

procedure DestroyLocals; far;
begin
  FileBitmap.Free;
  FileBitmap := nil;
  DateBitmap.Free;
  DateBitmap := nil;
end;

{$IFNDEF VER80}
initialization
finalization
  DestroyLocals;
{$ELSE}
initialization
  AddExitProc(DestroyLocals);
{$ENDIF}
end.