{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxDBCtrl;

{$I RX.INC}
{$R-}

interface

uses
  {$IFNDEF VER80}
  Windows, Registry,
  {$ELSE}
  WinTypes, WinProcs, DbiTypes, DbiProcs, ObjStr,
  {$ENDIF}
  Messages, Classes, Controls, Forms, Grids, Graphics, Buttons, Menus,
  StdCtrls, Mask, IniFiles, RxToolEdit, DB, DBGrids, ImgList,
  {$IFNDEF RX_D3}DBTables, {$ENDIF}
  {$IFDEF RX_D6}Types, {$ENDIF} {$IFDEF RX_D16}System.UITypes, {$ENDIF}
  RxPlacemnt, RxDateUtil, DBCtrls, RxCtrls, RxCurrEdit;

{ TRxDBGrid }

const
  DefRxGridOptions = [dgEditing, dgTitles, dgIndicator, dgColumnResize,
    dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit];

  {$IFDEF RX_V110}
  {$IFDEF CBUILDER}
  {$NODEFINE DefRxGridOptions}
  {$ENDIF}
  {$ENDIF}

type
  {$IFDEF RX_D12}
  TBookmarkType = TBookmark;
  {$ELSE}
  TBookmarkType = TBookmarkStr;
  {$ENDIF}

  TTitleClickEvent = procedure(Sender: TObject; ACol: LongInt;
    Field: TField) of object;
  TCheckTitleBtnEvent = procedure(Sender: TObject; ACol: LongInt;
    Field: TField; var Enabled: Boolean) of object;
  TGetCellParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; Highlight: Boolean) of object;
  TSortMarker = (smNone, smDown, smUp);
  TGetBtnParamsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor; var SortMarker: TSortMarker;
    IsDown: Boolean) of object;
  TGetCellPropsEvent = procedure(Sender: TObject; Field: TField;
    AFont: TFont; var Background: TColor) of object; { obsolete }
  TDBEditShowEvent = procedure(Sender: TObject; Field: TField;
    var AllowEdit: Boolean) of object;

  {$IFDEF VER80}
  TBookmarkList = class
  private
    FList: THugeList;
    FGrid: TCustomDBGrid;
    FCache: TBookmark;
    FCacheIndex: LongInt;
    FCacheFind: Boolean;
    FLinkActive: Boolean;
    function GetCount: LongInt;
    function GetCurrentRowSelected: Boolean;
    function GetItem(Index: LongInt): TBookmark;
    procedure SetCurrentRowSelected(Value: Boolean);
    procedure ListChanged;
  protected
    function CurrentRow: TBookmark;
    function Compare(const Item1, Item2: TBookmark): LongInt;
    procedure LinkActive(Value: Boolean);
  public
    constructor Create(AGrid: TCustomDBGrid);
    destructor Destroy; override;
    procedure Clear; { free all bookmarks }
    procedure Delete; { delete all selected rows from dataset }
    function Find(const Item: TBookmark; var Index: LongInt): Boolean;
    function IndexOf(const Item: TBookmark): LongInt;
    function Refresh: Boolean; { drop orphaned bookmarks; True = orphans found }
    property Count: LongInt read GetCount;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected
      write SetCurrentRowSelected;
    property Items[Index: LongInt]: TBookmark read GetItem; default;
  end;
  {$ENDIF}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBGrid = class(TDBGrid)
  private
    FAutoAppend: Boolean; // Polaris
    FSizingIndex, FSizingOfs: Integer; // Polaris
    FShowGlyphs: Boolean;
    FDefaultDrawing: Boolean;
    FMultiSelect: Boolean;
    FSelecting: Boolean;
    FClearSelection: Boolean;
    FTitleButtons: Boolean;
    {$IFNDEF VER80}
    FPressedCol: TColumn;
    {$ELSE}
    FPressedCol: LongInt;
    {$ENDIF}
    FPressed: Boolean;
    FTracking: Boolean;
    FSwapButtons: Boolean;
    FIniLink: TIniLink;
    FDisableCount: Integer;
    FFixedCols: Integer;
    FMsIndicators: TImageList;
    FOnCheckButton: TCheckTitleBtnEvent;
    FOnGetCellProps: TGetCellPropsEvent;
    FOnGetCellParams: TGetCellParamsEvent;
    FOnGetBtnParams: TGetBtnParamsEvent;
    FOnEditChange: TNotifyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnTitleBtnClick: TTitleClickEvent;
    FOnShowEditor: TDbEditShowEvent;
    FOnTopLeftChanged: TNotifyEvent;
    {$IFNDEF VER80}
    FSelectionAnchor: TBookmarkType;
    {$ELSE}
    FSelectionAnchor: TBookmark;
    FBookmarks: TBookmarkList;
    FOnColumnMoved: TMovedEvent;
    {$ENDIF}
    FRowColorsUse: Boolean;
    FRowColors: array[0..1] of TColor;
    procedure SetRowColorsUse(Value: Boolean);
    procedure SetRowColor(Index: Integer; Value: TColor);
    function GetImageIndex(Field: TField): Integer;
    procedure SetShowGlyphs(Value: Boolean);
    procedure SetRowsHeight(Value: Integer);
    function GetRowsHeight: Integer;
    function GetStorage: TFormPlacement;
    procedure SetStorage(Value: TFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetTitleButtons(Value: Boolean);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function ActiveRowSelected: Boolean;
    function GetSelCount: LongInt;
    procedure InternalSaveLayout(IniFile: TObject; const Section: string);
    procedure InternalRestoreLayout(IniFile: TObject; const Section: string);
    {$IFNDEF VER80}
    procedure SaveColumnsLayout(IniFile: TObject; const Section: string);
    procedure RestoreColumnsLayout(IniFile: TObject; const Section: string);
    function GetOptions: TDBGridOptions;
    procedure SetOptions(Value: TDBGridOptions);
    function GetMasterColumn(ACol, ARow: LongInt): TColumn;
    {$ELSE}
    function GetFixedColor: TColor;
    procedure SetFixedColor(Value: TColor);
    function GetIndicatorOffset: Byte;
    {$ENDIF}
    function GetTitleOffset: Byte;
    procedure SetFixedCols(Value: Integer);
    function GetFixedCols: Integer;
    {$IFDEF RX_D4}
    function CalcLeftColumn: Integer;
    {$ENDIF}
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    {$IFNDEF VER80}
    procedure WMRButtonUp(var Message: TWMMouse); message WM_RBUTTONUP;
    {$ENDIF}
  protected
    function AcquireFocus: Boolean;
    function CanEditShow: Boolean; override;
    function CreateEditor: TInplaceEdit; override;
    procedure DoTitleClick(ACol: LongInt; AField: TField); dynamic;
    procedure CheckTitleButton(ACol, ARow: LongInt; var Enabled: Boolean); dynamic;
    procedure DrawCell(ACol, ARow: LongInt; ARect: TRect; AState: TGridDrawState); override;
    procedure DrawDataCell(const Rect: TRect; Field: TField; State: TGridDrawState); override; { obsolete from Delphi 2.0 }
    procedure EditChanged(Sender: TObject); dynamic;
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean); dynamic;
    function HighlightCell(DataCol, DataRow: Integer; const Value: string; AState: TGridDrawState): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure SetColumnAttributes; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFDEF RX_D4}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$ENDIF}
    procedure Scroll(Distance: Integer); override;
    procedure LayoutChanged; override;
    procedure TopLeftChanged; override;
    {$IFNDEF VER80}
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer;
      Column: TColumn; State: TGridDrawState); override;
    procedure ColWidthsChanged; override;
    {$ELSE}
    procedure ColumnMoved(FromIndex, ToIndex: LongInt); override;
    procedure LinkActive(Value: Boolean); override;
    {$ENDIF}
    procedure Paint; override;
    {$IFDEF RX_D4} // Polaris
    procedure CalcSizingState(X, Y: Integer; var State: TGridState;
      var Index: LongInt; var SizingPos, SizingOfs: Integer;
      var FixedInfo: TGridDrawInfo); override;
    {$ENDIF} // Polaris
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDataCellDraw(const Rect: TRect; Field: TField; State: TGridDrawState);
    procedure DisableScroll;
    procedure EnableScroll;
    function ScrollDisabled: Boolean;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: LongInt);
    procedure SaveLayout(IniFile: TIniFile);
    procedure RestoreLayout(IniFile: TIniFile);
    procedure SelectAll;
    procedure UnselectAll;
    procedure ToggleRowSelection;
    procedure GotoSelection(Index: LongInt);
    {$IFNDEF VER80}
    procedure SaveLayoutReg(IniFile: TRegIniFile);
    procedure RestoreLayoutReg(IniFile: TRegIniFile);
    procedure SetGridColumnWidthsByContent(const GoToFirst: Boolean = False);
    property SelectedRows;
    {$ELSE}
    property SelectedRows: TBookmarkList read FBookmarks;
    {$ENDIF}
    property SelCount: LongInt read GetSelCount;
    property Canvas;
    property Col;
    property InplaceEditor;
    property LeftCol;
    property Row;
    property VisibleRowCount;
    property VisibleColCount;
    property IndicatorOffset{$IFDEF VER80}: Byte read GetIndicatorOffset{$ENDIF};
    property TitleOffset: Byte read GetTitleOffset;
  published
    property AutoAppend: Boolean read FAutoAppend write FAutoAppend default True; // Polaris
    {$IFNDEF VER80}
    property Options: TDBGridOptions read GetOptions write SetOptions default DefRxGridOptions;
    {$ELSE}
    property FixedColor: TColor read GetFixedColor write SetFixedColor default clBtnFace; { fix Delphi 1.0 bug }
    property Options default DefRxGridOptions;
    {$ENDIF}
    property FixedCols: Integer read GetFixedCols write SetFixedCols default 0;
    property ClearSelection: Boolean read FClearSelection write FClearSelection default True;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property IniStorage: TFormPlacement read GetStorage write SetStorage;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property RowColorsUse: Boolean read FRowColorsUse write SetRowColorsUse default False;
    property RowColor1: TColor index 0 read FRowColors[0] write SetRowColor default clInfoBk;
    property RowColor2: TColor index 1 read FRowColors[1] write SetRowColor default{$IFDEF RX_D6}clSkyBlue{$ELSE}clWhite{$ENDIF};
    property ShowGlyphs: Boolean read FShowGlyphs write SetShowGlyphs default True;
    property TitleButtons: Boolean read FTitleButtons write SetTitleButtons default False;
    property RowsHeight: Integer read GetRowsHeight write SetRowsHeight stored False; { obsolete, for backward compatibility only }
    property OnCheckButton: TCheckTitleBtnEvent read FOnCheckButton write FOnCheckButton;
    property OnGetCellProps: TGetCellPropsEvent read FOnGetCellProps write FOnGetCellProps; { obsolete }
    property OnGetCellParams: TGetCellParamsEvent read FOnGetCellParams write FOnGetCellParams;
    property OnGetBtnParams: TGetBtnParamsEvent read FOnGetBtnParams write FOnGetBtnParams;
    property OnEditChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnShowEditor: TDBEditShowEvent read FOnShowEditor write FOnShowEditor;
    property OnTitleBtnClick: TTitleClickEvent read FOnTitleBtnClick write FOnTitleBtnClick;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    {$IFDEF VER80}
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    {$ENDIF}
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF RX_D4}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
  end;

{ TRxDBComboEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBComboEdit = class(TCustomComboEdit)
  private
    FDataLink: TFieldDataLink;
    {$IFNDEF VER80}
    FCanvas: TControlCanvas;
    {$ENDIF}
    FFocused: Boolean;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    {$IFNDEF VER80}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}
  protected
    procedure Change; override;
    function EditCanModify: Boolean; override;
    function GetReadOnly: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$ENDIF}
    property Button;
    property Field: TField read GetField;
  published
//Polaris
    property Align;

    property AutoSelect;
    property BorderStyle;
    property ButtonHint;
    property CharCase;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DirectInput;
    property DragCursor;
    property DragMode;
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
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
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

{ TDBDateEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBDateEdit = class(TCustomDateEdit)
  private
    FInReset: Boolean; // Polaris
    FDataLink: TFieldDataLink;
    {$IFNDEF VER80}
    FCanvas: TControlCanvas;
    {$ENDIF}
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure AfterPopup(Sender: TObject; var Date: TDateTime; var Action: Boolean);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    {$IFNDEF VER80}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    {$ENDIF}
  protected
    {$IFNDEF VER80}
    procedure AcceptValue(const Value: Variant); override;
    {$ENDIF}
    procedure ApplyDate(Value: TDateTime); override;
    function GetReadOnly: Boolean; override;
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;

// Polaris
    procedure SetDate(Value: TDateTime); override;
    function IsValidDate(Value: TDateTime): Boolean;
// Polaris
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateMask; override;
    {$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$ENDIF}
    property Field: TField read GetField;
  published
// Polaris
    property DateAutoBetween;
    property MinDate;
    property MaxDate;
    property Align;
// Polaris
    property CalendarHints;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property AutoSelect;
    property BlanksChar;
    property BorderStyle;
    property ButtonHint;
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
    property ShowHint;
    property CalendarStyle;
    property TabOrder;
    property TabStop;
    property StartOfWeek;
    property Weekends;
    property WeekendColor;
    property YearDigits;
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

{ TRxDBCalcEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBCalcEdit = class(TRxCustomCalcEdit)
  private
    FDataLink: TFieldDataLink;
    FDefaultParams: Boolean;

    //Polaris
    FLEmptyIsNull: Boolean;
    FEmptyIsNull: Boolean;
    procedure SetEmptyIsNull(Value: Boolean);
    function GetZeroEmpty: Boolean;
    procedure SetZeroEmpty(Value: Boolean);
    function StoreEmptyIsNull: Boolean;
    //Polaris

    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultParams(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateFieldData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
//Polaris procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    {$IFNDEF VER80}
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    {$ENDIF}
  protected
    {$IFNDEF VER80}
    procedure AcceptValue(const Value: Variant); override;
    function GetDisplayText: string; override;
    {$ENDIF}
    function GetReadOnly: Boolean; override;
    procedure Change; override;

    procedure DataChanged; override; //Polaris

    function EditCanModify: Boolean; override;
    function IsValidChar(Key: Char): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure UpdatePopup; override;

//Polaris
    procedure Loaded; override;
//Polaris

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFieldParams;
    {$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
    {$ENDIF}
    property Field: TField read GetField;
    property Value;
  published
//Polaris
    property Align;
    property DecimalPlaceRound;

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultParams: Boolean read FDefaultParams write SetDefaultParams default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Alignment;
    property AutoSelect;
    property BeepOnError;
    property BorderStyle;
    property ButtonHint;
    property CheckOnExit;
    property ClickKey;
    property Color;
    property Ctl3D;
    property DecimalPlaces;
    property DirectInput;
    property DisplayFormat;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FormatOnEditing;
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
    property MaxValue;
    property MinValue;
    property NumGlyphs;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
//Polaris
    property EmptyIsNull: Boolean read FEmptyIsNull write SetEmptyIsNull stored StoreEmptyIsNull;
    property ZeroEmpty: Boolean read GetZeroEmpty write SetZeroEmpty default True;
//Polaris
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

{ TDBStatusLabel }

  TGetStringEvent = function(Sender: TObject): string of object;
  TDataValueEvent = procedure(Sender: TObject; DataSet: TDataSet;
    var Value: LongInt) of object;
  TDBLabelStyle = (lsState, lsRecordNo, lsRecordSize);
  TGlyphAlign = glGlyphLeft..glGlyphRight;
  TDBStatusKind = dsInactive..dsCalcFields;
  TDBLabelOptions = (doCaption, doGlyph, doBoth);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBStatusLabel = class(TRxCustomLabel)
  private
    FDataLink: TDataLink;
    {$IFDEF RX_D4} // Polaris
    FDataSetName: string;
    {$ELSE}
    FDataSetName: PString;
    {$ENDIF}
    FStyle: TDBLabelStyle;
    FEditColor: TColor;
    FCalcCount: Boolean;
    FCaptions: TStrings;
    FGlyph: TBitmap;
    FCell: TBitmap;
    FGlyphAlign: TGlyphAlign;
    FRecordCount: LongInt;
    FRecordNo: LongInt;
    FShowOptions: TDBLabelOptions;
    FOnGetDataName: TGetStringEvent;
    FOnGetRecNo: TDataValueEvent;
    FOnGetRecordCount: TDataValueEvent;
    function GetStatusKind(State: TDataSetState): TDBStatusKind;
    procedure CaptionsChanged(Sender: TObject);
    function GetDataSetName: string;
    procedure SetDataSetName(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDatasetState: TDataSetState;
    procedure SetEditColor(Value: TColor);
    procedure SetStyle(Value: TDBLabelStyle);
    procedure SetShowOptions(Value: TDBLabelOptions);
    procedure SetGlyphAlign(Value: TGlyphAlign);
    procedure SetCaptions(Value: TStrings);
    procedure SetCalcCount(Value: Boolean);
  protected
    procedure Loaded; override;
    function GetDefaultFontColor: TColor; override;
    function GetLabelCaption: string; override;
    function GetCaption(State: TDataSetState): string; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateData; virtual;
    procedure UpdateStatus; virtual;
    property Caption;
    property DatasetState: TDataSetState read GetDatasetState;
  published
    property DatasetName: string read GetDataSetName write SetDataSetName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property EditColor: TColor read FEditColor write SetEditColor default clRed;
    property Captions: TStrings read FCaptions write SetCaptions;
    property Style: TDBLabelStyle read FStyle write SetStyle default lsState;
    property CalcRecCount: Boolean read FCalcCount write SetCalcCount default False;
    property ShowOptions: TDBLabelOptions read FShowOptions write SetShowOptions
      default doCaption;
    property GlyphAlign: TGlyphAlign read FGlyphAlign write SetGlyphAlign
      default glGlyphLeft;
    property Layout default tlCenter;
    property ShadowSize default 0;
    property Align;
    property Alignment;
    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property Font;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowPos;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnGetDataName: TGetStringEvent read FOnGetDataName write FOnGetDataName;
    property OnGetRecordCount: TDataValueEvent read FOnGetRecordCount
      write FOnGetRecordCount;
    property OnGetRecNo: TDataValueEvent read FOnGetRecNo write FOnGetRecNo;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
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

implementation

uses SysUtils, Dialogs, ExtCtrls, DbConsts, {$IFDEF RX_D5}RxAppUtils{$ELSE}AppUtils{$ENDIF}, RxVCLUtils, // Alex
  RxDBUtils, {$IFNDEF RX_D3}BdeUtils, {$ENDIF}RxPickDate, RxCalc, RxMaxMin, RxResConst, // Polaris
  {$IFDEF RX_D7}Themes, {$ENDIF}
  {$IFDEF RX_D6}Variants, {$ENDIF}RxStrUtils; // Polaris

{$R *.RES}

type
  TGridPicture = (gpBlob, gpMemo, gpPicture, gpOle, gpObject, gpData,
    gpNotEmpty, gpMarkDown, gpMarkUp);

const
  GridBmpNames: array[TGridPicture] of PChar =
  ('DBG_BLOB', 'DBG_MEMO', 'DBG_PICT', 'DBG_OLE', 'DBG_OBJECT', 'DBG_DATA',
    'DBG_NOTEMPTY', 'DBG_SMDOWN', 'DBG_SMUP');
  GridBitmaps: array[TGridPicture] of TBitmap =
  (nil, nil, nil, nil, nil, nil, nil, nil, nil);
  bmMultiDot = 'DBG_MSDOT';
  bmMultiArrow = 'DBG_MSARROW';

function GetGridBitmap(BmpType: TGridPicture): TBitmap;
begin
  if GridBitmaps[BmpType] = nil then
  begin
    GridBitmaps[BmpType] := TBitmap.Create;
    GridBitmaps[BmpType].Handle := LoadBitmap(HInstance, GridBmpNames[BmpType]);
  end;
  Result := GridBitmaps[BmpType];
end;

procedure DestroyLocals; far;
var
  I: TGridPicture;
begin
  for I := Low(TGridPicture) to High(TGridPicture) do
    GridBitmaps[I].Free;
end;

procedure GridInvalidateRow(Grid: TRxDBGrid; Row: LongInt);
var
  I: LongInt;
begin
  for I := 0 to Grid.ColCount - 1 do
    Grid.InvalidateCell(I, Row);
end;

{$IFDEF VER80}

{ TBookmarkList }

constructor TBookmarkList.Create(AGrid: TCustomDBGrid);
begin
  inherited Create;
  FList := THugeList.Create;
  FGrid := AGrid;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
var
  I: LongInt;
begin
  if FList.Count = 0 then Exit;
  for I := FList.Count - 1 downto 0 do
    StrDispose(FList[I]);
  FList.Clear;
  ListChanged;
  FGrid.Invalidate;
end;

function TBookmarkList.Compare(const Item1, Item2: TBookmark): LongInt;
begin
  Result := BookmarksCompare(TRxDBGrid(FGrid).Datalink.Dataset,
    Item1, Item2);
end;

function TBookmarkList.CurrentRow: TBookmark;
begin
  if not FLinkActive then _DBError(sDataSetClosed);
  Result := TRxDBGrid(FGrid).Datalink.Dataset.GetBookmark;
end;

function TBookmarkList.GetCurrentRowSelected: Boolean;
var
  Index: LongInt;
  Row: TBookmark;
begin
  Row := CurrentRow;
  try
    Result := Find(Row, Index);
  finally
    StrDispose(Row);
  end;
end;

function TBookmarkList.Find(const Item: TBookmark; var Index: LongInt): Boolean;
var
  L, H, I, C: LongInt;
  P: PChar;
begin
  if (Compare(Item, FCache) = 0) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := False;
  L := 0;
  H := FList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(TBookmark(FList[I]), Item);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
  StrDispose(FCache);
  FCache := nil;
  P := PChar(Item);
  if P <> nil then
  begin
    Dec(P, 2);
    FCache := StrAlloc(Word(Pointer(P)^));
    Move(Item^, FCache^, Word(Pointer(P)^));
  end;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TBookmarkList.GetCount: LongInt;
begin
  Result := FList.Count;
end;

function TBookmarkList.GetItem(Index: LongInt): TBookmark;
begin
  Result := TBookmark(FList[Index]);
end;

function TBookmarkList.IndexOf(const Item: TBookmark): LongInt;
begin
  if not Find(Item, Result) then Result := -1;
end;

procedure TBookmarkList.LinkActive(Value: Boolean);
begin
  Clear;
  FLinkActive := Value;
end;

procedure TBookmarkList.Delete;
var
  I: LongInt;
begin
  with TRxDBGrid(FGrid).Datalink.Dataset do
  begin
    DisableControls;
    try
      for I := FList.Count - 1 downto 0 do
      begin
        if FList[I] <> nil then
        begin
          GotoBookmark(TBookmark(FList[I]));
          Delete;
          StrDispose(FList[I]);
        end;
        FList.Delete(I);
      end;
      ListChanged;
    finally
      EnableControls;
    end;
  end;
end;

function TBookmarkList.Refresh: Boolean;
var
  I: LongInt;
begin
  Result := False;
  with TRxDBGrid(FGrid).DataLink.Dataset do
  try
    CheckBrowseMode;
    for I := FList.Count - 1 downto 0 do
      if DbiSetToBookmark(Handle, Pointer(FList[I])) <> 0 then
      begin
        Result := True;
        StrDispose(FList[I]);
        FList.Delete(I);
      end;
    ListChanged;
  finally
    UpdateCursorPos;
    if Result then FGrid.Invalidate;
  end;
end;

procedure TBookmarkList.SetCurrentRowSelected(Value: Boolean);
var
  Index: LongInt;
  Current: TBookmark;
begin
  Current := CurrentRow;
  Index := 0;
  if (Current = nil) or (Find(Current, Index) = Value) then
  begin
    if Current <> nil then StrDispose(Current);
    Exit;
  end;
  if Value then
  begin
    try
      FList.Insert(Index, Current);
    except
      StrDispose(Current);
      raise;
    end;
  end
  else
  begin
    if (Index < FList.Count) and (Index >= 0) then
    begin
      StrDispose(FList[Index]);
      FList.Delete(Index);
    end;
    StrDispose(Current);
  end;
  ListChanged;
  TRxDBGrid(FGrid).InvalidateRow(TRxDBGrid(FGrid).Row);
  GridInvalidateRow(TRxDBGrid(FGrid), TRxDBGrid(FGrid).Row);
end;

procedure TBookmarkList.ListChanged;
begin
  if FCache <> nil then StrDispose(FCache);
  FCache := nil;
  FCacheIndex := -1;
end;

{$ENDIF}

type
  TBookmarks = class(TBookmarkList);

{ TRxDBGrid }

constructor TRxDBGrid.Create(AOwner: TComponent);
var
  Bmp: TBitmap;
begin
  inherited Create(AOwner);
  inherited DefaultDrawing := False;
  FRowColors[0] := clInfoBk;
  FRowColors[1] := {$IFDEF RX_D6}clSkyBlue{$ELSE}clWhite{$ENDIF};
  FRowColorsUse := False;
  Options := DefRxGridOptions;
  Bmp := TBitmap.Create;
  try
    Bmp.Handle := LoadBitmap(hInstance, bmMultiDot);
    {$IFNDEF VER80}
    FMsIndicators := TImageList.CreateSize(Bmp.Width, Bmp.Height);
    {$ELSE}
    FMsIndicators := TImageList.Create(Bmp.Width, Bmp.Height);
    Bmp.Monochrome := False;
    {$ENDIF}
    FMsIndicators.AddMasked(Bmp, clWhite);
    Bmp.Handle := LoadBitmap(hInstance, bmMultiArrow);
    {$IFDEF VER80}
    Bmp.Monochrome := False;
    {$ENDIF}
    FMsIndicators.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free;
  end;
  FIniLink := TIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FShowGlyphs := True;
  FDefaultDrawing := True;
  FClearSelection := True;
  {$IFDEF VER80}
  FBookmarks := TBookmarkList.Create(Self);
  FPressedCol := -1;
  {$ENDIF}
  FAutoAppend := True; // Polaris
end;

destructor TRxDBGrid.Destroy;
begin
  FIniLink.Free;
  {$IFDEF VER80}
  if FSelectionAnchor <> nil then StrDispose(FSelectionAnchor);
  FSelectionAnchor := nil;
  FBookmarks.Free;
  FBookmarks := nil;
  {$ENDIF}
  FMsIndicators.Free;
  inherited Destroy;
end;

function TRxDBGrid.GetImageIndex(Field: TField): Integer;
var
  AOnGetText: TFieldGetTextEvent;
  AOnSetText: TFieldSetTextEvent;
begin
  Result := -1;
  if FShowGlyphs and Assigned(Field) then
  begin
    if (not ReadOnly) and Field.CanModify then
    begin
      { Allow editing of memo fields if OnSetText and OnGetText
        events are assigned }
      AOnGetText := Field.OnGetText;
      AOnSetText := Field.OnSetText;
      if Assigned(AOnSetText) and Assigned(AOnGetText) then Exit;
    end;
    case Field.DataType of
      ftBytes, ftVarBytes, ftBlob: Result := Ord(gpBlob);
      ftMemo: Result := Ord(gpMemo);
      ftGraphic: Result := Ord(gpPicture);
      {$IFNDEF VER80}
      ftTypedBinary: Result := Ord(gpBlob);
      ftFmtMemo: Result := Ord(gpMemo);
      ftParadoxOle, ftDBaseOle: Result := Ord(gpOle);
      {$ENDIF}
      {$IFDEF RX_D3}
      ftCursor: Result := Ord(gpData);
      {$ENDIF}
      {$IFDEF RX_D4}
      ftReference, ftDataSet: Result := Ord(gpData);
      {$ENDIF}
      {$IFDEF RX_D5}
      ftOraClob: Result := Ord(gpMemo);
      ftOraBlob: Result := Ord(gpBlob);
      {$ENDIF}
    end;
  end;
end;

function TRxDBGrid.ActiveRowSelected: Boolean;
var
  {$IFNDEF VER80}
  Index: Integer;
  {$ELSE}
  Index: LongInt;
  Bookmark: TBookmark;
  {$ENDIF}
begin
  Result := False;
  if MultiSelect and Datalink.Active then
  begin
    {$IFNDEF VER80}
    Result := SelectedRows.Find(Datalink.DataSet.Bookmark, Index);
    {$ELSE}
    Bookmark := Datalink.Dataset.GetBookmark;
    try
      Result := SelectedRows.Find(Bookmark, Index);
    finally
      StrDispose(Bookmark);
    end;
    {$ENDIF}
  end;
end;

function TRxDBGrid.HighlightCell(DataCol, DataRow: Integer;
  const Value: string; AState: TGridDrawState): Boolean;
begin
  Result := ActiveRowSelected;
  if not Result then
    Result := inherited HighlightCell(DataCol, DataRow, Value, AState);
end;

procedure TRxDBGrid.ToggleRowSelection;
begin
  if MultiSelect and Datalink.Active then
    with SelectedRows do
      CurrentRowSelected := not CurrentRowSelected;
end;

function TRxDBGrid.GetSelCount: LongInt;
begin
  if MultiSelect and (Datalink <> nil) and Datalink.Active then
    Result := SelectedRows.Count
  else
    Result := 0;
end;

procedure TRxDBGrid.SelectAll;
var
  ABookmark: TBookmark;
begin
  if MultiSelect and DataLink.Active then
  begin
    with Datalink.Dataset do
    begin
      if (Bof and Eof) then Exit;
      DisableControls;
      try
        ABookmark := GetBookmark;
        try
          First;
          while not Eof do
          begin
            SelectedRows.CurrentRowSelected := True;
            Next;
          end;
        finally
          try
            GotoBookmark(ABookmark);
          except
          end;
          FreeBookmark(ABookmark);
        end;
      finally
        EnableControls;
      end;
    end;
  end;
end;

procedure TRxDBGrid.UnselectAll;
begin
  if MultiSelect then
  begin
    SelectedRows.Clear;
    FSelecting := False;
  end;
end;

procedure TRxDBGrid.GotoSelection(Index: LongInt);
begin
  if MultiSelect and DataLink.Active and (Index < SelectedRows.Count) and
    (Index >= 0) then
    {$IFDEF RX_D25} //Rad Studio 10.2 Tokyo
    Datalink.DataSet.GotoBookmark(SelectedRows[Index]);
    {$ELSE}
    Datalink.DataSet.GotoBookmark(Pointer(SelectedRows[Index]));
    {$ENDIF}
end;

{$IFDEF VER80}
function TRxDBGrid.GetIndicatorOffset: Byte;
begin
  Result := 0;
  if dgIndicator in Options then Inc(Result);
end;
{$ENDIF}

procedure TRxDBGrid.LayoutChanged;
var
  ACol: LongInt;
begin
  ACol := Col;
  inherited LayoutChanged;
  if Datalink.Active and (FixedCols > 0) then
    {$IFDEF RX_D4}
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
  {$ELSE}
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);
  {$ENDIF}
end;

{$IFNDEF VER80}
procedure TRxDBGrid.ColWidthsChanged;
var
  ACol: LongInt;
begin
  ACol := Col;
  inherited ColWidthsChanged;
  if Datalink.Active and (FixedCols > 0) then
    {$IFDEF RX_D4}
    Col := Min(Max(CalcLeftColumn, ACol), ColCount - 1);
  {$ELSE}
    Col := Min(Max(inherited FixedCols, ACol), ColCount - 1);
  {$ENDIF}
end;
{$ENDIF}

function TRxDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  TEdit(Result).OnChange := EditChanged;
end;

function TRxDBGrid.GetTitleOffset: Byte;
{$IFDEF RX_D4}
var
  I, J: Integer;
  {$ENDIF}
begin
  Result := 0;
  if dgTitles in Options then
  begin
    Result := 1;
    {$IFDEF RX_D4}
    if (Datalink <> nil) and (Datalink.Dataset <> nil) and
      Datalink.Dataset.ObjectView then
    begin
      for I := 0 to Columns.Count - 1 do
      begin
        if Columns[I].Showing then
        begin
          J := Columns[I].Depth;
          if J >= Result then Result := J + 1;
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

procedure TRxDBGrid.SetColumnAttributes;
begin
  inherited SetColumnAttributes;
  SetFixedCols(FFixedCols);
end;

procedure TRxDBGrid.SetFixedCols(Value: Integer);
var
  FixCount, I: Integer;
begin
  FixCount := Max(Value, 0) + IndicatorOffset;
  if DataLink.Active and not (csLoading in ComponentState) and
    (ColCount > IndicatorOffset + 1) then
  begin
    FixCount := Min(FixCount, ColCount - 1);
    inherited FixedCols := FixCount;
    for I := 1 to Min(FixedCols, ColCount - 1) do
      TabStops[I] := False;
  end;
  FFixedCols := FixCount - IndicatorOffset;
end;

procedure TRxDBGrid.SetGridColumnWidthsByContent(const GoToFirst: Boolean);
const
  DEFBORDER = 10;
var
  tmp, n: Integer;
  lmax: array [Byte] of Integer;
begin
  DataSource.DataSet.DisableControls;
  try
    Canvas.Font := Font;
    for n := 0 to Columns.Count - 1 do
      //if columns[n].visible then
      lmax[n] := Canvas.TextWidth(Fields[n].FieldName) + DEFBORDER;
    DataSource.DataSet.First;
    while not DataSource.DataSet.EOF do
    begin
      for n := 0 to Columns.Count - 1 do
      begin
        //if columns[n].visible then begin
        tmp := Canvas.TextWidth(Trim(Columns[n].Field.DisplayText)) + DEFBORDER;
        if tmp > lmax[n] then lmax[n] := tmp;
        //end; { if }
      end; {for}
      DataSource.DataSet.Next;
    end; { while }
    if GoToFirst then
      DataSource.DataSet.First;
    for n := 0 to Columns.Count - 1 do
      if lmax[n] > 0 then
        Columns[n].Width := lmax[n];
  finally
    DataSource.DataSet.EnableControls;
  end;
end;

function TRxDBGrid.GetFixedCols: Integer;
begin
  if DataLink.Active then
    Result := inherited FixedCols - IndicatorOffset
  else
    Result := FFixedCols;
end;

{$IFDEF RX_D4}
 function TRxDBGrid.CalcLeftColumn: Integer;
begin
  Result := FixedCols + IndicatorOffset;
  while (Result < ColCount) and (ColWidths[Result] <= 0) do
    Inc(Result);
end;
{$ENDIF}

procedure TRxDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyDownEvent: TKeyEvent;

  procedure ClearSelections;
  begin
    if FMultiSelect then
    begin
      if FClearSelection then SelectedRows.Clear;
      FSelecting := False;
    end;
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
    {$IFDEF VER80}
    CurRow: TBookmark;
    {$ENDIF}
  begin
    AddAfter := False;
    {$IFNDEF VER80}
    BeginUpdate;
    try
      {$ENDIF}
      if MultiSelect and DataLink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            {$IFDEF VER80}
            if FSelectionAnchor <> nil then StrDispose(FSelectionAnchor);
            {$ENDIF}
            FSelectionAnchor := TBookmarks(SelectedRows).CurrentRow;
            SelectedRows.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
            with TBookmarks(SelectedRows) do
            begin
              {$IFNDEF VER80}
              AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
              {$ELSE}
              CurRow := CurrentRow;
              try
                AddAfter := Compare(CurRow, FSelectionAnchor) <> -Direction;
              finally
                StrDispose(CurRow);
              end;
              {$ENDIF}
              if not AddAfter then CurrentRowSelected := False;
            end
        end
        else
          ClearSelections;
      if Direction <> 0 then Datalink.DataSet.MoveBy(Direction);
      if AddAfter then SelectedRows.CurrentRowSelected := True;
      {$IFNDEF VER80}
    finally
      EndUpdate;
    end;
    {$ENDIF}
  end;

  procedure NextRow(Select: Boolean);
  begin
    with Datalink.Dataset do
    begin
      DoSelection(Select, 1);
      if AutoAppend and Eof and CanModify and (not ReadOnly) and (dgEditing in Options) then
        Append;
    end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
    DoSelection(Select, -1);
  end;

  procedure CheckTab(GoForward: Boolean);
  var
    ACol, Original: Integer;
  begin
    ACol := Col;
    Original := ACol;
    if MultiSelect and DataLink.Active then
      while True do
      begin
        if GoForward then
          Inc(ACol)
        else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          ClearSelections;
          ACol := IndicatorOffset;
        end
        else if ACol < IndicatorOffset then
        begin
          ClearSelections;
          ACol := ColCount;
        end;
        if ACol = Original then Exit;
        if TabStops[ACol] then Exit;
      end;
  end;

  function DeletePrompt: Boolean;
  var
    S: string;
  begin
    if (SelectedRows.Count > 1) then
      {$IFNDEF VER80}
      S := {ResStr(} SDeleteMultipleRecordsQuestion {)}
        {$ELSE}
      S := {LoadStr(} SDeleteMultipleRecords {)}
        {$ENDIF}
    else
      S := {ResStr(} SDeleteRecordQuestion {)};
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;

begin
  KeyDownEvent := OnKeyDown;
  if Assigned(KeyDownEvent) then KeyDownEvent(Self, Key, Shift);
  if not Datalink.Active or not CanGridAcceptKey(Key, Shift) then Exit;
  with Datalink.DataSet do
    if ssCtrl in Shift then
    begin
      if (Key in [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END]) then
        ClearSelections;
      case Key of
        VK_LEFT:
          if FixedCols > 0 then
          begin
            {$IFDEF RX_D4}
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            {$ELSE}
            SelectedIndex := FixedCols;
            {$ENDIF}
            Exit;
          end;
        VK_DELETE:
          if not ReadOnly and CanModify and not
            IsDataSetEmpty(Datalink.DataSet) then
          begin
            if DeletePrompt then
            begin
              if SelectedRows.Count > 0 then
                SelectedRows.Delete
              else
                Delete;
            end;
            Exit;
          end;
      end
    end
    else
    begin
      case Key of
        VK_LEFT:
          if (FixedCols > 0) and not (dgRowSelect in Options) then
          begin
            {$IFDEF RX_D4}
            if SelectedIndex <= CalcLeftColumn - IndicatorOffset then
              Exit;
            {$ELSE}
            if SelectedIndex <= FFixedCols then Exit;
            {$ENDIF}
          end;
        VK_HOME:
          if (FixedCols > 0) and (ColCount <> IndicatorOffset + 1) and
            not (dgRowSelect in Options) then
          begin
            {$IFDEF RX_D4}
            SelectedIndex := CalcLeftColumn - IndicatorOffset;
            {$ELSE}
            SelectedIndex := FixedCols;
            {$ENDIF}
            Exit;
          end;
      end;
      if (Datalink.DataSet.State = dsBrowse) then
      begin
        case Key of
          VK_UP:
            begin
              PriorRow(True); Exit;
            end;
          VK_DOWN:
            begin
              NextRow(True); Exit;
            end;
        end;
      end;
      if ((Key in [VK_LEFT, VK_RIGHT]) and (dgRowSelect in Options)) or
        ((Key in [VK_HOME, VK_END]) and ((ColCount = IndicatorOffset + 1)
        or (dgRowSelect in Options))) or (Key in [VK_ESCAPE, VK_NEXT,
        VK_PRIOR]) or ((Key = VK_INSERT) and (CanModify and
        (not ReadOnly) and (dgEditing in Options))) then
        ClearSelections
      else if ((Key = VK_TAB) and not (ssAlt in Shift)) then
        CheckTab(not (ssShift in Shift));
    end;
  OnKeyDown := nil;
  try
    inherited KeyDown(Key, Shift);
  finally
    OnKeyDown := KeyDownEvent;
  end;
  if ((Key = VK_NEXT) and not DataLink.DataSet.Eof)
    or ((Key = VK_PRIOR) and not DataLink.DataSet.Bof) then
    Invalidate;
end;

procedure TRxDBGrid.SetShowGlyphs(Value: Boolean);
begin
  if FShowGlyphs <> Value then
  begin
    FShowGlyphs := Value;
    Invalidate;
  end;
end;

procedure TRxDBGrid.SetRowColor(Index: Integer; Value: TColor);
begin
  if FRowColors[Index] <> Value then
  begin
    FRowColors[Index] := Value;
    if FRowColorsUse then Invalidate;
  end;
end;

procedure TRxDBGrid.SetRowColorsUse(Value: Boolean);
begin
  if FRowColorsUse <> Value then
  begin
    FRowColorsUse := Value;
    Invalidate;
  end;
end;

procedure TRxDBGrid.SetRowsHeight(Value: Integer);
begin
  if not (csDesigning in ComponentState) and (DefaultRowHeight <> Value) then
  begin
    DefaultRowHeight := Value;
    if dgTitles in Options then RowHeights[0] := Value + 2;
    if HandleAllocated then
      Perform(WM_SIZE, SIZE_RESTORED, MakeLParam(ClientWidth, ClientHeight));
  end;
end;

function TRxDBGrid.GetRowsHeight: Integer;
begin
  Result := DefaultRowHeight;
end;

{$IFNDEF VER80}
function TRxDBGrid.GetOptions: TDBGridOptions;
begin
  Result := inherited Options;
  if FMultiSelect then
    Result := Result + [dgMultiSelect]
  else
    Result := Result - [dgMultiSelect];
end;

procedure TRxDBGrid.SetOptions(Value: TDBGridOptions);
var
  NewOptions: TGridOptions;
begin
  inherited Options := Value - [dgMultiSelect];
  NewOptions := TDrawGrid(Self).Options;
  {
  if FTitleButtons then begin
    TDrawGrid(Self).Options := NewOptions + [goFixedHorzLine, goFixedVertLine];
  end else
  }
  begin
    if not (dgColLines in Value) then
      NewOptions := NewOptions - [goFixedVertLine];
    if not (dgRowLines in Value) then
      NewOptions := NewOptions - [goFixedHorzLine];
    TDrawGrid(Self).Options := NewOptions;
  end;
  SetMultiSelect(dgMultiSelect in Value);
end;

{$ELSE}

procedure TRxDBGrid.LinkActive(Value: Boolean);
begin
  SelectedRows.LinkActive(Value);
  inherited LinkActive(Value);
end;

function TRxDBGrid.GetFixedColor: TColor;
begin
  Result := inherited TitleColor;
end;

procedure TRxDBGrid.SetFixedColor(Value: TColor);
begin
  if FixedColor <> Value then
  begin
    inherited TitleColor := Value;
    inherited FixedColor := Value;
    Invalidate;
  end;
end;

procedure TRxDBGrid.ColumnMoved(FromIndex, ToIndex: LongInt);
begin
  inherited ColumnMoved(FromIndex, ToIndex);
  if Assigned(FOnColumnMoved) then FOnColumnMoved(Self, FromIndex, ToIndex);
end;
{$ENDIF}

procedure TRxDBGrid.Paint;
begin
  inherited Paint;
  if not (csDesigning in ComponentState) and
    (dgRowSelect in Options) and DefaultDrawing and Focused then
  begin
    Canvas.Font.Color := clWindowText;
    with Selection do
      DrawFocusRect(Canvas.Handle, BoxRect(Left, Top, Right, Bottom));
  end;
end;

procedure TRxDBGrid.SetTitleButtons(Value: Boolean);
begin
  if FTitleButtons <> Value then
  begin
    FTitleButtons := Value;
    Invalidate;
    {$IFNDEF VER80}
    SetOptions(Options);
    {$ENDIF}
  end;
end;

procedure TRxDBGrid.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not Value then SelectedRows.Clear;
  end;
end;

function TRxDBGrid.GetStorage: TFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TRxDBGrid.SetStorage(Value: TFormPlacement);
begin
  FIniLink.Storage := Value;
end;

function TRxDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not (csDesigning in ComponentState) then
  begin
    SetFocus;
    Result := Focused or (InplaceEditor <> nil) and InplaceEditor.Focused;
  end;
end;

function TRxDBGrid.CanEditShow: Boolean;
var
  F: TField;
begin
  Result := inherited CanEditShow;
  F := nil;
  if Result and (Datalink <> nil) and Datalink.Active and (FieldCount > 0) and
    (SelectedIndex < FieldCount) and (SelectedIndex >= 0) and
    (FieldCount <= DataSource.DataSet.FieldCount) then
  begin
    F := Fields[SelectedIndex];
    if F <> nil then Result := GetImageIndex(F) < 0;
  end;
  if Result and Assigned(FOnShowEditor) then
    FOnShowEditor(Self, F, Result);
end;

procedure TRxDBGrid.GetCellProps(Field: TField; AFont: TFont;
  var Background: TColor; Highlight: Boolean);
var
  AColor, ABack: TColor;
begin
  if FRowColorsUse and not Highlight then
    Background := FRowColors[DataLink.ActiveRecord mod 2]
  else
  begin
    if Highlight and ((dgMultiSelect in Options) or (dgRowSelect in Options) or (SelectedField = Field)) then
    begin
      {$IFDEF RX_D7}
      with {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF} do
      if {$IFDEF RX_D16}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
      begin
        AFont.Color := {$IFDEF RX_D16}GetSystemColor{$ENDIF}(clHighlightText);
        Background := {$IFDEF RX_D16}GetSystemColor{$ENDIF}(clHighlight);
      end
      else
      {$ENDIF}
      begin
        AFont.Color := clHighlightText;
        Background := clHighlight;
      end;
    end
  end;

  if Assigned(FOnGetCellParams) then
    FOnGetCellParams(Self, Field, AFont, Background, Highlight)
  else if Assigned(FOnGetCellProps) then
  begin
    if Highlight then
    begin
      AColor := AFont.Color;
      FOnGetCellProps(Self, Field, AFont, ABack);
      AFont.Color := AColor;
    end
    else
      FOnGetCellProps(Self, Field, AFont, Background);
  end;
end;

procedure TRxDBGrid.DoTitleClick(ACol: LongInt; AField: TField);
begin
  if Assigned(FOnTitleBtnClick) then FOnTitleBtnClick(Self, ACol, AField);
end;

procedure TRxDBGrid.CheckTitleButton(ACol, ARow: LongInt; var Enabled: Boolean);
var
  Field: TField;
begin
  if (ACol >= 0) and (ACol < {$IFNDEF VER80}Columns.Count{$ELSE}
    FieldCount{$ENDIF}) then
  begin
    if Assigned(FOnCheckButton) then
    begin
      {$IFNDEF VER80}
      Field := Columns[ACol].Field;
      {$IFDEF RX_D4}
      if ColumnAtDepth(Columns[ACol], ARow) <> nil then
        Field := ColumnAtDepth(Columns[ACol], ARow).Field;
      {$ENDIF}
      {$ELSE}
      Field := Fields[ACol];
      {$ENDIF}
      FOnCheckButton(Self, ACol, Field, Enabled);
    end;
  end
  else
    Enabled := False;
end;

procedure TRxDBGrid.DisableScroll;
begin
  Inc(FDisableCount);
end;

type
  THackLink = class(TGridDataLink);

procedure TRxDBGrid.EnableScroll;
begin
  if FDisableCount <> 0 then
  begin
    Dec(FDisableCount);
    if FDisableCount = 0 then
      THackLink(DataLink).DataSetScrolled(0);
  end;
end;

function TRxDBGrid.ScrollDisabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TRxDBGrid.Scroll(Distance: Integer);
{$IFNDEF RX_D3}
var
  IndicatorRect: TRect;
  {$ENDIF}
begin
  Invalidate;
  if FDisableCount = 0 then
  begin
    inherited Scroll(Distance);
    {$IFNDEF RX_D3}
    if (dgIndicator in Options) and HandleAllocated and MultiSelect then
    begin
      IndicatorRect := BoxRect(0, 0, 0, RowCount - 1);
      InvalidateRect(Handle, @IndicatorRect, False);
    end;
    {$ENDIF}
  end;
end;

{$IFDEF RX_D4}
function TRxDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(1) <> 0;
    end;
  end;
end;

function TRxDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result then
  begin
    if not AcquireFocus then Exit;
    if Datalink.Active then
    begin
      Result := Datalink.DataSet.MoveBy(-1) <> 0;
    end;
  end;
end;

{$ENDIF RX_D4}

procedure TRxDBGrid.EditChanged(Sender: TObject);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TRxDBGrid.TopLeftChanged;
begin
  if (dgRowSelect in Options) and DefaultDrawing then
    GridInvalidateRow(Self, Self.Row);
  inherited TopLeftChanged;
  if FTracking then StopTracking;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

procedure TRxDBGrid.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRxDBGrid.TrackButton(X, Y: Integer);
var
  Cell: TGridCoord;
  NewPressed: Boolean;
  I, Offset: Integer;
begin
  Cell := MouseCoord(X, Y);
  Offset := TitleOffset;
  NewPressed := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y)) and
    (FPressedCol = {$IFNDEF VER80}GetMasterColumn(Cell.X, Cell.Y){$ELSE}
    Cell.X{$ENDIF}) and (Cell.Y < Offset);
  if FPressed <> NewPressed then
  begin
    FPressed := NewPressed;
    for I := 0 to Offset - 1 do
      GridInvalidateRow(Self, I);
  end;
end;

procedure TRxDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  MouseDownEvent: TMouseEvent;
  EnableClick: Boolean;
begin
  if not AcquireFocus then Exit;
  if (ssDouble in Shift) and (Button = mbLeft) then
  begin
    DblClick;
    Exit;
  end;
  if Sizing(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
  begin
    Cell := MouseCoord(X, Y);
    {$IFDEF RX_D4}
    if (DragKind = dkDock) and (Cell.X < IndicatorOffset) and
      (Cell.Y < TitleOffset) and (not (csDesigning in ComponentState)) then
    begin
      BeginDrag(False);
      Exit;
    end;
    {$ENDIF}
    if FTitleButtons and (Datalink <> nil) and Datalink.Active and
      (Cell.Y < TitleOffset) and (Cell.X >= IndicatorOffset) and
      not (csDesigning in ComponentState) then
    begin
      if (dgColumnResize in Options) and (Button = mbRight) then
      begin
        Button := mbLeft;
        FSwapButtons := True;
        MouseCapture := True;
      end
      else if Button = mbLeft then
      begin
        EnableClick := True;
        CheckTitleButton(Cell.X - IndicatorOffset, Cell.Y, EnableClick);
        if EnableClick then
        begin
          MouseCapture := True;
          FTracking := True;
          {$IFNDEF VER80}
          FPressedCol := GetMasterColumn(Cell.X, Cell.Y);
          {$ELSE}
          FPressedCol := Cell.X;
          {$ENDIF}
          TrackButton(X, Y);
        end
        else
          Beep;
        Exit;
      end;
    end;
    if (Cell.X < FixedCols + IndicatorOffset) and Datalink.Active then
    begin
      if (dgIndicator in Options) then
        inherited MouseDown(Button, Shift, 1, Y)
      else if Cell.Y >= TitleOffset then
        if Cell.Y - Row <> 0 then Datalink.Dataset.MoveBy(Cell.Y - Row);
    end
    else
      inherited MouseDown(Button, Shift, X, Y);
    MouseDownEvent := OnMouseDown;
    if Assigned(MouseDownEvent) then MouseDownEvent(Self, Button, Shift, X, Y);
    if not (((csDesigning in ComponentState) or (dgColumnResize in Options)) and
      (Cell.Y < TitleOffset)) and (Button = mbLeft) then
    begin
      if MultiSelect and Datalink.Active then
        with SelectedRows do
        begin
          Refresh;
          FSelecting := False;
          if ssCtrl in Shift then
            CurrentRowSelected := not CurrentRowSelected
          else
          begin
            Clear;
            if FClearSelection then CurrentRowSelected := True;
          end;
        end;
    end;
  end;
end;

procedure TRxDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then TrackButton(X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TRxDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cell: TGridCoord;
  ACol: LongInt;
  DoClick: Boolean;
begin
  if FTracking and {$IFNDEF VER80}(FPressedCol <> nil){$ELSE}
  (FPressedCol >= 0){$ENDIF} then
  begin
    Cell := MouseCoord(X, Y);
    DoClick := PtInRect(Rect(0, 0, ClientWidth, ClientHeight), Point(X, Y))
      and (Cell.Y < TitleOffset) and
      {$IFNDEF VER80}
    (FPressedCol = GetMasterColumn(Cell.X, Cell.Y));
    {$ELSE}
    (Cell.X = FPressedCol);
    {$ENDIF}
    StopTracking;
    if DoClick then
    begin
      ACol := Cell.X;
      if (dgIndicator in Options) then Dec(ACol);
      if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and
        (ACol < {$IFNDEF VER80}Columns.Count{$ELSE}FieldCount{$ENDIF}) then
      begin
        {$IFNDEF VER80}
        DoTitleClick(FPressedCol.Index, FPressedCol.Field);
        {$ELSE}
        DoTitleClick(ACol, Fields[ACol]);
        {$ENDIF}
      end;
    end;
  end
  else if FSwapButtons then
  begin
    FSwapButtons := False;
    MouseCapture := False;
    if Button = mbRight then Button := mbLeft;
  end;
  // Polaris
  if (Button = mbLeft) and (FGridState = gsColSizing) and
    (FSizingIndex + Byte(not (dgIndicator in Options)) <= FixedCols) then
  begin
    ColWidths[FSizingIndex] := X - FSizingOfs - CellRect(FSizingIndex, 0).Left;
    FGridState := gsNormal;
    Exit
  end;
  // Polaris
  inherited MouseUp(Button, Shift, X, Y);
end;

{$IFNDEF VER80}
procedure TRxDBGrid.WMRButtonUp(var Message: TWMMouse);
begin
  if not (FGridState in [gsColMoving, gsRowMoving]) then
    inherited
  else if not (csNoStdEvents in ControlStyle) then
    with Message do
      MouseUp(mbRight, KeysToShiftState(Keys), XPos, YPos);
end;
{$ENDIF}

procedure TRxDBGrid.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

type
  THack = class(TWinControl);

procedure TRxDBGrid.WMChar(var Msg: TWMChar);

  function DoKeyPress(var Msg: TWMChar): Boolean;
  var
    Form: TCustomForm;
    Ch: Char;
  begin
    Result := True;
    Form := GetParentForm(Self);
    if (Form <> nil) and TForm(Form).KeyPreview and
      THack(Form).DoKeyPress(Msg) then Exit;
    with Msg do
    begin
      if Assigned(FOnKeyPress) then
      begin
        Ch := Char(CharCode);
        FOnKeyPress(Self, Ch);
        CharCode := Word(Ch);
      end;
      if Char(CharCode) = #0 then Exit;
    end;
    Result := False;
  end;

begin

  if EditorMode or not DoKeyPress(Msg) then inherited;
end;

procedure TRxDBGrid.KeyPress(var Key: Char);
begin
  if EditorMode then inherited OnKeyPress := FOnKeyPress;
  try
    inherited KeyPress(Key);
  finally
    inherited OnKeyPress := nil;
  end;
end;

procedure TRxDBGrid.DefaultDataCellDraw(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
  DefaultDrawDataCell(Rect, Field, State);
end;

{$IFNDEF VER80}
function TRxDBGrid.GetMasterColumn(ACol, ARow: LongInt): TColumn;
begin
  if (dgIndicator in Options) then Dec(ACol, IndicatorOffset);
  if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
    (ACol < Columns.Count) then
  begin
    Result := Columns[ACol];
    {$IFDEF RX_D4}
    Result := ColumnAtDepth(Result, ARow);
    {$ENDIF}
  end
  else
    Result := nil;
end;
{$ENDIF}

procedure TRxDBGrid.DrawCell(ACol, ARow: LongInt; ARect: TRect;
  AState: TGridDrawState);

{$IFDEF RX_D4}

function CalcTitleRect(Col: TColumn; ARow: Integer; var MasterCol: TColumn): TRect;
    { copied from Inprise's DbGrids.pas }
  var
    I, J: Integer;
    InBiDiMode: Boolean;
    DrawInfo: TGridDrawInfo;
  begin
    MasterCol := ColumnAtDepth(Col, ARow);
    if MasterCol = nil then Exit;
    I := DataToRawColumn(MasterCol.Index);
    if I >= LeftCol then
      J := MasterCol.Depth
    else
    begin
      if (FixedCols > 0) and (MasterCol.Index < FixedCols) then
      begin
        J := MasterCol.Depth;
      end
      else
      begin
        I := LeftCol;
        if Col.Depth > ARow then
          J := ARow
        else
          J := Col.Depth;
      end;
    end;
    Result := CellRect(I, J);
    InBiDiMode := UseRightToLeftAlignment and (Canvas.CanvasOrientation = coLeftToRight);
    for I := Col.Index to Columns.Count - 1 do
    begin
      if ColumnAtDepth(Columns[I], ARow) <> MasterCol then Break;
      if not InBiDiMode then
      begin
        J := CellRect(DataToRawColumn(I), ARow).Right;
        if J = 0 then Break;
        Result.Right := Max(Result.Right, J);
      end
      else
      begin
        J := CellRect(DataToRawColumn(I), ARow).Left;
        if J >= ClientWidth then Break;
        Result.Left := J;
      end;
    end;
    J := Col.Depth;
    if (J <= ARow) and (J < FixedRows - 1) then
    begin
      CalcFixedInfo(DrawInfo);
      Result.Bottom := DrawInfo.Vert.FixedBoundary -
        DrawInfo.Vert.EffectiveLineWidth;
    end;
  end;

  procedure DrawExpandBtn(var TitleRect, TextRect: TRect; InBiDiMode: Boolean;
    Expanded: Boolean); { copied from Inprise's DbGrids.pas }
  const
    ScrollArrows: array[Boolean, Boolean] of Integer =
    ((DFCS_SCROLLRIGHT, DFCS_SCROLLLEFT), (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  var
    ButtonRect: TRect;
    I: Integer;
  begin
    I := GetSystemMetrics(SM_CXHSCROLL);
    if ((TextRect.Right - TextRect.Left) > I) then
    begin
      Dec(TextRect.Right, I);
      ButtonRect := TitleRect;
      ButtonRect.Left := TextRect.Right;
      I := SaveDC(Canvas.Handle);
      try
        Canvas.FillRect(ButtonRect);
        InflateRect(ButtonRect, -1, -1);
        with ButtonRect do
          IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
        InflateRect(ButtonRect, 1, 1);
        { DrawFrameControl doesn't draw properly when orienatation has changed.
          It draws as ExtTextOut does. }
        if InBiDiMode then { stretch the arrows box }
          Inc(ButtonRect.Right, GetSystemMetrics(SM_CXHSCROLL) + 4);
        DrawFrameControl(Canvas.Handle, ButtonRect, DFC_SCROLL,
          ScrollArrows[InBiDiMode, Expanded] or DFCS_FLAT);
      finally
        RestoreDC(Canvas.Handle, I);
      end;
      TitleRect.Right := ButtonRect.Left;
    end;
  end;
  {$ENDIF RX_D4}

var
  FrameOffs: Byte;
  BackColor: TColor;
  SortMarker: TSortMarker;
  Indicator, ALeft: Integer;
  Down: Boolean;
  Bmp: TBitmap;
  SavePen: TColor;
  OldActive: LongInt;
  MultiSelected: Boolean;
  FixRect: TRect;
  TitleRect, TextRect: TRect;
  AField: TField;
  {$IFDEF RX_D4}
  MasterCol: TColumn;
  InBiDiMode: Boolean;
  {$ENDIF}
  {$IFNDEF VER80}
  DrawColumn: TColumn;
const
  EdgeFlag: array[Boolean] of UINT = (BDR_RAISEDINNER, BDR_SUNKENINNER);
  {$ENDIF}
begin
  if (gdFixed in AState) then Canvas.Brush.Color := FixedColor;
  inherited DrawCell(ACol, ARow, ARect, AState);
  {$IFDEF RX_D4}
  InBiDiMode := Canvas.CanvasOrientation = coRightToLeft;
  {$ENDIF}
  if (dgIndicator in Options) and (ACol = 0) and (ARow - TitleOffset >= 0)
    and MultiSelect and (DataLink <> nil) and DataLink.Active and
    (Datalink.DataSet.State = dsBrowse) then
  begin { draw multiselect indicators if needed }
    FixRect := ARect;
    if ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
    begin
      InflateRect(FixRect, -1, -1);
      FrameOffs := 1;
    end
    else
      FrameOffs := 2;
    OldActive := DataLink.ActiveRecord;
    try
      Datalink.ActiveRecord := ARow - TitleOffset;
      MultiSelected := ActiveRowSelected;
    finally
      Datalink.ActiveRecord := OldActive;
    end;
    if MultiSelected then
    begin
      if (ARow - TitleOffset <> Datalink.ActiveRecord) then
        Indicator := 0
      else
        Indicator := 1; { multiselected and current row }
      {$IFNDEF VER80}
      FMsIndicators.BkColor := FixedColor;
      {$ELSE}
      Canvas.Brush.Color := TitleColor;
      Canvas.FillRect(FixRect);
      {$ENDIF}
      ALeft := FixRect.Right - FMsIndicators.Width - FrameOffs;
      {$IFDEF RX_D4}
      if InBiDiMode then Inc(ALeft);
      {$ENDIF}    {$IFDEF RX_D7}
      with {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF} do
      if {$IFDEF RX_D16}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
      begin
        FMsIndicators.BkColor := clWhite;
        FMsIndicators.DrawingStyle := {$IFDEF RX_D16}ImgList.TDrawingStyle.{$ENDIF}dsTransparent;
      end;
      {$ENDIF}
      FMsIndicators.Draw(Self.Canvas, ALeft, (FixRect.Top +
        FixRect.Bottom - FMsIndicators.Height) shr 1, Indicator);
    end;
  end
  else if not (csLoading in ComponentState) and
    (FTitleButtons{$IFDEF RX_D4} or (FixedCols > 0){$ENDIF}) and
    (gdFixed in AState) and (dgTitles in Options) and (ARow < TitleOffset) then
  begin
    SavePen := Canvas.Pen.Color;
    try
      Canvas.Pen.Color := clWindowFrame;
      if (dgIndicator in Options) then Dec(ACol, IndicatorOffset);
      AField := nil;
      SortMarker := smNone;
      {$IFNDEF VER80}
      if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and (ACol < Columns.Count) then
      begin
        DrawColumn := Columns[ACol];
        AField := DrawColumn.Field;
      end
      else
        DrawColumn := nil;
      {$IFDEF RX_D4}
      if Assigned(DrawColumn) and not DrawColumn.Showing then Exit;
      TitleRect := CalcTitleRect(DrawColumn, ARow, MasterCol);
      if TitleRect.Right < ARect.Right then
        TitleRect.Right := ARect.Right;
      if MasterCol = nil then
        Exit
      else if MasterCol <> DrawColumn then
        AField := MasterCol.Field;
      DrawColumn := MasterCol;
      if ((dgColLines in Options) or FTitleButtons) and (ACol = FixedCols - 1) then
      begin
        if (ACol < Columns.Count - 1) and not (Columns[ACol + 1].Showing) then
        begin
          Canvas.MoveTo(TitleRect.Right, TitleRect.Top);
          Canvas.LineTo(TitleRect.Right, TitleRect.Bottom);
        end;
      end;
      if ((dgRowLines in Options) or FTitleButtons) and not MasterCol.Showing then
      begin
        Canvas.MoveTo(TitleRect.Left, TitleRect.Bottom);
        Canvas.LineTo(TitleRect.Right, TitleRect.Bottom);
      end;
      {$ELSE}
      TitleRect := ARect;
      {$ENDIF RX_D4}
      Down := FPressed and FTitleButtons and (FPressedCol = DrawColumn);
      if FTitleButtons or ([dgRowLines, dgColLines] * Options = [dgRowLines, dgColLines]) then
      begin
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_BOTTOMRIGHT);
        DrawEdge(Canvas.Handle, TitleRect, EdgeFlag[Down], BF_TOPLEFT);
        InflateRect(TitleRect, -1, -1);
      end;
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := FixedColor;
      if (DrawColumn <> nil) then
      begin
        Canvas.Font := DrawColumn.Title.Font;
        Canvas.Brush.Color := DrawColumn.Title.Color;
      end;
      if FTitleButtons and (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, SortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if Down then
      begin
        Inc(TitleRect.Left); Inc(TitleRect.Top);
      end;
      ARect := TitleRect;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(TitleRect)
      else if (DrawColumn <> nil) then
      begin
        case SortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
        else
          Bmp := nil;
        end;
        if Bmp <> nil then
          Indicator := Bmp.Width + 6
        else
          Indicator := 1;
        TextRect := TitleRect;
        {$IFDEF RX_D4}
        if DrawColumn.Expandable then
          DrawExpandBtn(TitleRect, TextRect, InBiDiMode, DrawColumn.Expanded);
        {$ENDIF}
        with DrawColumn.Title do
          DrawCellText(Self, ACol, ARow, MinimizeText(Caption, Canvas,
            WidthOf(TextRect) - Indicator), TextRect, Alignment, vaCenter
            {$IFDEF RX_D4}, IsRightToLeft{$ENDIF});
        if Bmp <> nil then
        begin
          ALeft := TitleRect.Right - Bmp.Width - 3;
          if Down then Inc(ALeft);
          {$IFDEF RX_D4}
          if IsRightToLeft then ALeft := TitleRect.Left + 3;
          {$ENDIF}
          if (ALeft > TitleRect.Left) and (ALeft + Bmp.Width < TitleRect.Right) then
            DrawBitmapTransparent(Canvas, ALeft, (TitleRect.Bottom +
              TitleRect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
        end;
      end
        {$ELSE}
      if not (dgColLines in Options) then
      begin
        Canvas.MoveTo(ARect.Right - 1, ARect.Top);
        Canvas.LineTo(ARect.Right - 1, ARect.Bottom);
        Dec(ARect.Right);
      end;
      if not (dgRowLines in Options) then
      begin
        Canvas.MoveTo(ARect.Left, ARect.Bottom - 1);
        Canvas.LineTo(ARect.Right, ARect.Bottom - 1);
        Dec(ARect.Bottom);
      end;
      Down := FPressed and FTitleButtons and (FPressedCol = ACol);
      if (Datalink <> nil) and Datalink.Active and (ACol >= 0) and
        (ACol < FieldCount) then
      begin
        AField := Fields[ACol];
      end;
      if Down then
      begin
        with ARect do
        begin
          Canvas.Pen.Color := clBtnShadow;
          Canvas.PolyLine([Point(Left, Bottom - 1), Point(Left, Top),
            Point(Right, Top)]);
          Inc(Left, 2); Inc(Top, 2);
        end;
      end
      else
        Frame3D(Canvas, ARect, clBtnHighlight, clBtnShadow, 1);
      Canvas.Font := TitleFont;
      Canvas.Brush.Color := TitleColor;
      if FTitleButtons and (AField <> nil) and Assigned(FOnGetBtnParams) then
      begin
        BackColor := Canvas.Brush.Color;
        FOnGetBtnParams(Self, AField, Canvas.Font, BackColor, SortMarker, Down);
        Canvas.Brush.Color := BackColor;
      end;
      if (DataLink = nil) or not DataLink.Active then
        Canvas.FillRect(ARect)
      else if (AField <> nil) then
      begin
        case SortMarker of
          smDown: Bmp := GetGridBitmap(gpMarkDown);
          smUp: Bmp := GetGridBitmap(gpMarkUp);
        else
          Bmp := nil;
        end;
        if Bmp <> nil then
          Indicator := Bmp.Width + 8
        else
          Indicator := 1;
        DrawCellText(Self, ACol, ARow, MinimizeText(AField.DisplayLabel,
          Canvas, WidthOf(ARect) - Indicator), ARect, taLeftJustify, vaCenter);
        if Bmp <> nil then
        begin
          ALeft := ARect.Right - Bmp.Width - 4;
          if Down then Inc(ALeft);
          DrawBitmapTransparent(Canvas, ALeft,
            (ARect.Bottom + ARect.Top - Bmp.Height) div 2, Bmp, clFuchsia);
        end;
      end
        {$ENDIF}
      else
        DrawCellText(Self, ACol, ARow, '', ARect, taLeftJustify, vaCenter);
    finally
      Canvas.Pen.Color := SavePen;
    end;
  end
  else
  begin
    {$IFDEF RX_D4}
    Canvas.Font := Self.Font;
    if (DataLink <> nil) and DataLink.Active and (ACol >= 0) and (ACol < Columns.Count) then
    begin
      DrawColumn := Columns[ACol];
      if DrawColumn <> nil then Canvas.Font := DrawColumn.Font;
    end;
    {$ENDIF}
  end;
end;

{$IFNDEF VER80}
procedure TRxDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer;
  Column: TColumn; State: TGridDrawState);
{$ELSE}

procedure TRxDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
{$ENDIF}
var
  I: Integer;
  NewBackgrnd: TColor;
  Highlight: Boolean;
  Bmp: TBitmap;
  {$IFNDEF VER80}
  Field: TField;
  {$ENDIF}
begin
  {$IFNDEF VER80}
  Field := Column.Field;
  {$ENDIF}
  NewBackgrnd := Canvas.Brush.Color;
  Highlight := (gdSelected in State) and ((dgAlwaysShowSelection in Options) or
    Focused);
  GetCellProps(Field, Canvas.Font, NewBackgrnd, Highlight or ActiveRowSelected);
  Canvas.Brush.Color := NewBackgrnd;
  if FDefaultDrawing then
  begin
    I := GetImageIndex(Field);
    if I >= 0 then
    begin
      Bmp := GetGridBitmap(TGridPicture(I));
      Canvas.FillRect(Rect);
      DrawBitmapTransparent(Canvas, (Rect.Left + Rect.Right - Bmp.Width) div 2,
        (Rect.Top + Rect.Bottom - Bmp.Height) div 2, Bmp, clOlive);
    end
    else
      {$IFNDEF VER80}
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    {$ELSE}
      DefaultDrawDataCell(Rect, Field, State);
    {$ENDIF}
  end;
  {$IFNDEF VER80}
  if (Columns.State = csDefault) or not DefaultDrawing or (csDesigning in ComponentState) then
    inherited DrawDataCell(Rect, Field, State);
  inherited DrawColumnCell(Rect, DataCol, Column, State);
  {$ELSE}
  inherited DrawDataCell(Rect, Field, State);
  {$ENDIF}
  if FDefaultDrawing and Highlight and not (csDesigning in ComponentState)
    and not (dgRowSelect in Options)
    and (ValidParentForm(Self).ActiveControl = Self) then
    Canvas.DrawFocusRect(Rect);
end;

{$IFNDEF VER80}
procedure TRxDBGrid.DrawDataCell(const Rect: TRect; Field: TField;
  State: TGridDrawState);
begin
end;
{$ENDIF}

procedure TRxDBGrid.MouseToCell(X, Y: Integer; var ACol, ARow: LongInt);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

{$IFNDEF VER80}
procedure TRxDBGrid.SaveColumnsLayout(IniFile: TObject;
  const Section: string);
var
  I: Integer;
  S: string;
begin
  if Section <> '' then
    S := Section
  else
    S := GetDefaultSection(Self);
  IniEraseSection(IniFile, S);
  with Columns do
  begin
    for I := 0 to Count - 1 do
    begin
      IniWriteString(IniFile, S, Format('%s.%s', [Name, Items[I].FieldName]),
        Format('%d,%d', [Items[I].Index, Items[I].Width]));
    end;
  end;
end;

procedure TRxDBGrid.RestoreColumnsLayout(IniFile: TObject;
  const Section: string);
type
  TColumnInfo = record
    Column: TColumn;
    EndIndex: Integer;
  end;
  PColumnArray = ^TColumnArray;
  TColumnArray = array[0..0] of TColumnInfo;
const
  Delims = [' ', ','];
var
  I, J: Integer;
  SectionName, S: string;
  ColumnArray: PColumnArray;
begin
  if Section <> '' then
    SectionName := Section
  else
    SectionName := GetDefaultSection(Self);
  with Columns do
  begin
    ColumnArray := AllocMemo(Count * SizeOf(TColumnInfo));
    try
      for I := 0 to Count - 1 do
      begin
        S := IniReadString(IniFile, SectionName,
          Format('%s.%s', [Name, Items[I].FieldName]), '');
        ColumnArray^[I].Column := Items[I];
        ColumnArray^[I].EndIndex := Items[I].Index;
        if S <> '' then
        begin
          ColumnArray^[I].EndIndex := StrToIntDef(ExtractWord(1, S, Delims),
            ColumnArray^[I].EndIndex);
          Items[I].Width := StrToIntDef(ExtractWord(2, S, Delims),
            Items[I].Width);
        end;
      end;
      for I := 0 to Count - 1 do
      begin
        for J := 0 to Count - 1 do
        begin
          if ColumnArray^[J].EndIndex = I then
          begin
            ColumnArray^[J].Column.Index := ColumnArray^[J].EndIndex;
            Break;
          end;
        end;
      end;
    finally
      FreeMemo(Pointer(ColumnArray));
    end;
  end;
end;

procedure TRxDBGrid.SaveLayoutReg(IniFile: TRegIniFile);
begin
  InternalSaveLayout(IniFile, '');
end;

procedure TRxDBGrid.RestoreLayoutReg(IniFile: TRegIniFile);
begin
  InternalRestoreLayout(IniFile, '');
end;
{$ENDIF}

procedure TRxDBGrid.InternalSaveLayout(IniFile: TObject;
  const Section: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    {$IFNDEF VER80}
    if StoreColumns then
      SaveColumnsLayout(IniFile, Section)
    else
      {$ENDIF}
      InternalSaveFields(DataSource.DataSet, IniFile, Section);
end;

procedure TRxDBGrid.InternalRestoreLayout(IniFile: TObject;
  const Section: string);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  begin
    HandleNeeded;
    {$IFNDEF VER80}
    BeginLayout;
    try
      if StoreColumns then
        RestoreColumnsLayout(IniFile, Section)
      else
        {$ENDIF}
        InternalRestoreFields(DataSource.DataSet, IniFile, Section, False);
      {$IFNDEF VER80}
    finally
      EndLayout;
    end;
    {$ENDIF}
  end;
end;

procedure TRxDBGrid.SaveLayout(IniFile: TIniFile);
begin
  InternalSaveLayout(IniFile, '');
end;

procedure TRxDBGrid.RestoreLayout(IniFile: TIniFile);
begin
  InternalRestoreLayout(IniFile, '');
end;

procedure TRxDBGrid.IniSave(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
  begin
    {$IFNDEF VER80}
    if StoreColumns then
      Section := FIniLink.RootSection + GetDefaultSection(Self)
    else
      {$ENDIF}if (FIniLink.RootSection <> '') and (DataSource <> nil) and
        (DataSource.DataSet <> nil) then
        Section := FIniLink.RootSection + DataSetSectionName(DataSource.DataSet)
      else
        Section := '';
    InternalSaveLayout(FIniLink.IniObject, Section);
  end;
end;

procedure TRxDBGrid.IniLoad(Sender: TObject);
var
  Section: string;
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
  begin
    {$IFNDEF VER80}
    if StoreColumns then
      Section := FIniLink.RootSection + GetDefaultSection(Self)
    else
      {$ENDIF}if (FIniLink.RootSection <> '') and (DataSource <> nil) and (DataSource.DataSet <> nil) then
        Section := FIniLink.RootSection + DataSetSectionName(DataSource.DataSet)
      else
        Section := '';
    InternalRestoreLayout(FIniLink.IniObject, Section);
  end;
end;

{$IFDEF RX_D4} // Polaris
procedure TRxDBGrid.CalcSizingState(X, Y: Integer; var State: TGridState;
  var Index: LongInt; var SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);
var
  Coord: TGridCoord;
begin
  inherited CalcSizingState(X, Y, State, Index, SizingPos, SizingOfs, FixedInfo);
  if (State = gsNormal) and (Y <= RowHeights[0]) then
  begin
    Coord := MouseCoord(X, Y);
    CalcDrawInfo(FixedInfo);
    if (CellRect(Coord.X, 0).Right - 5 < X) then
    begin
      State := gsColSizing;
      Index := Coord.X;
      SizingPos := X;
      SizingOfs := X - CellRect(Coord.X, 0).Right;
    end;
    if (CellRect(Coord.X, 0).Left + 5 > X) then
    begin
      State := gsColSizing;
      Index := Coord.X - 1;
      SizingPos := X;
      SizingOfs := X - CellRect(Coord.X, 0).Left;
    end;
    if Index <= Byte(dgIndicator in Options) - 1 then State := gsNormal;
  end;
  FSizingIndex := Index;
  FSizingOfs := SizingOfs;
end;
{$ENDIF} // Polaris

{ TRxDBComboEdit }

procedure ResetMaxLength(DBEdit: TRxDBComboEdit);
var
  F: TField;
begin
  with DBEdit do
    if (MaxLength > 0) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
    begin
      F := DataSource.DataSet.FindField(DataField);
      if Assigned(F) and (F.DataType = ftString) and (F.Size = MaxLength) then
        MaxLength := 0;
    end;
end;

constructor TRxDBComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  inherited ReadOnly := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  AlwaysEnable := True;
end;

destructor TRxDBComboEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  {$IFNDEF VER80}
  FCanvas.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TRxDBComboEdit.Loaded;
begin
  inherited Loaded;
  ResetMaxLength(Self);
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TRxDBComboEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TRxDBComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
end;

procedure TRxDBComboEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= ' ') and (FDataLink.Field <> nil) and not FDataLink.Field.IsValidChar(Key) then
  begin
    Beep;
    Key := #0;
  end;
  if (Key >= ' ') then FDataLink.Edit
  else
  case Key of
    ^H, ^V, ^X:
      FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TRxDBComboEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TRxDBComboEdit.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TRxDBComboEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (Alignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TRxDBComboEdit.Change;
begin
  FDataLink.Modified;
  inherited Change;
end;

function TRxDBComboEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRxDBComboEdit.SetDataSource(Value: TDataSource);
begin
  {$IFDEF RX_D4}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    {$ENDIF}
    FDataLink.DataSource := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
end;

function TRxDBComboEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRxDBComboEdit.SetDataField(const Value: string);
begin
  if not (csDesigning in ComponentState) then ResetMaxLength(Self);
  FDataLink.FieldName := Value;
end;

function TRxDBComboEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRxDBComboEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TRxDBComboEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRxDBComboEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Alignment <> FDataLink.Field.Alignment then
    begin
      EditText := ''; {forces update}
      Alignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;
    if not (csDesigning in ComponentState) then
    begin
      if (FDataLink.Field.DataType = ftString) and (MaxLength = 0) then
        MaxLength := FDataLink.Field.Size;
    end;
    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      {if FDataLink.Editing then Modified := True;}
    end;
  end
  else
  begin
    Alignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name
    else
      EditText := '';
  end;
end;

procedure TRxDBComboEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TRxDBComboEdit.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TRxDBComboEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBComboEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBComboEdit.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  {$IFDEF RX_D3}
  if SysLocale.FarEast and FDataLink.CanModify then
    inherited ReadOnly := False;
  {$ENDIF}
end;

procedure TRxDBComboEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  SetFocused(False);
  CheckCursor;
  DoExit;
end;

{$IFNDEF VER80}
procedure TRxDBComboEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    S := FDataLink.Field.DisplayText;
    case CharCase of
      ecUpperCase: S := AnsiUpperCase(S);
      ecLowerCase: S := AnsiLowerCase(S);
    end;
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Message) then
    inherited;
end;

procedure TRxDBComboEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;
{$ENDIF}

{$IFDEF RX_D4}
function TRxDBComboEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TRxDBComboEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TRxDBComboEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TDBDateEdit }

constructor TDBDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  inherited ReadOnly := True;
  FInReset := False; // Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  Self.OnAcceptDate := AfterPopup;
  AlwaysEnable := True;
  UpdateMask;
end;

destructor TDBDateEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  {$IFNDEF VER80}
  FCanvas.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TDBDateEdit.AfterPopup(Sender: TObject; var Date: TDateTime;
  var Action: Boolean);
begin
  Action := Action and (DataSource <> nil) and (DataSource.DataSet <> nil) and
    DataSource.DataSet.CanModify;
  if Action then Action := EditCanModify;
end;

procedure TDBDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBDateEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_DELETE) or ((Key = VK_INSERT)
    and (ssShift in Shift))) then
    FDataLink.Edit;
end;

procedure TDBDateEdit.KeyPress(var Key: Char);
begin
{
  if (Key = #27) then begin
    Reset;
    Key := #0;
  end;
}
  inherited KeyPress(Key);
  if (Key >= ' ') and (FDataLink.Field <> nil) and
    not CharInSet(Key, ['0'..'9']) and (Key <> {$IFDEF RX_D15}FormatSettings.{$ENDIF}DateSeparator) then
  begin
    Beep;
    Key := #0;
  end;
  case Key of
    ^H, ^V, ^X, '0'..'9': FDataLink.Edit;
    #27:
      begin
        Reset;
        Key := #0;
      end;
  end;
end;

function TDBDateEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TDBDateEdit.Reset;
begin
  FInReset := True; // Polaris
  FDataLink.Reset;
  SelectAll;
  FInReset := False; // Polaris
end;

// Polaris begin

function TDBDateEdit.IsValidDate(Value: TDateTime): Boolean;
begin
  Result := FDateAutoBetween;
  if not Result then
    if (not FInReset) and FDataLink.Editing then
    try
      if (Value <> NullDate) then
      begin
        if ((MinDate <> NullDate) and (MaxDate <> NullDate) and ((Value < MinDate) or (Value > MaxDate))) then
          raise Exception.CreateFmt(RxLoadStr(SDateOutOfRange), [FormatDateTime(GetDateFormat, Value), FormatDateTime(GetDateFormat, MinDate), FormatDateTime(GetDateFormat, MaxDate)])
        else if ((MinDate <> NullDate) and (Value < MinDate)) then
          raise Exception.CreateFmt(RxLoadStr(SDateOutOfMin), [FormatDateTime(GetDateFormat, Value), FormatDateTime(GetDateFormat, MinDate)])
        else if ((MaxDate <> NullDate) and (Value > MaxDate)) then
          raise Exception.CreateFmt(RxLoadStr(SDateOutOfMax), [FormatDateTime(GetDateFormat, Value), FormatDateTime(GetDateFormat, MaxDate)]);
      end;
      Result := True;
    except
      Reset;
      raise;
    end;
end;

procedure TDBDateEdit.SetDate(Value: TDateTime);
begin
  IsValidDate(Value);
  inherited SetDate(Value);
end;
// Polaris end

procedure TDBDateEdit.Change;
begin
  if not Formatting then FDataLink.Modified;
  inherited Change;
end;

function TDBDateEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBDateEdit.SetDataSource(Value: TDataSource);
begin
  {$IFDEF RX_D4}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    {$ENDIF}
    FDataLink.DataSource := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
end;

function TDBDateEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TDBDateEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TDBDateEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TDBDateEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TDBDateEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TDBDateEdit.UpdateMask;
begin
  UpdateFormat;
  UpdatePopup;
  DataChange(nil);
end;

procedure TDBDateEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    EditMask := GetDateMask;
// Polaris
    inherited SetDate(FDataLink.Field.AsDateTime);
//    Self.Date := FDataLink.Field.AsDateTime;
//    SetDate(FDataLink.Field.AsDateTime);
// Polaris
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      EditMask := '';
      EditText := Name;
    end
    else
    begin
      EditMask := GetDateMask;
      if DefaultToday then
        Date := SysUtils.Date
      else
        Date := NullDate;
    end;
  end;
end;

procedure TDBDateEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
  if FDataLink.Editing and DefaultToday and (FDataLink.Field <> nil) and
    (FDataLink.Field.AsDateTime = NullDate) then
    FDataLink.Field.AsDateTime := SysUtils.Now;
end;

procedure TDBDateEdit.UpdateData(Sender: TObject);
var
  D: TDateTime;
begin
  ValidateEdit;
  D := Self.Date;
  if D <> NullDate then
  begin
    if Int(FDataLink.Field.AsDateTime) <> D then
      FDataLink.Field.AsDateTime := D + Frac(FDataLink.Field.AsDateTime)
  end
  else
    FDataLink.Field.Clear;
end;

{$IFNDEF VER80}
procedure TDBDateEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure TDBDateEdit.WMPaint(var Message: TWMPaint);
var
  S: string;
begin
  if (csPaintCopy in ControlState) and (FDataLink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
    begin
      S := GetDateFormat;
      S := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(S, '/', {$IFDEF RX_D15}FormatSettings.{$ENDIF}DateSeparator),
        'Y', ' '), 'M', ' '), 'D', ' ');
    end
    else
      S := FormatDateTime(GetDateFormat, FDataLink.Field.AsDateTime);
  end
  else
    S := EditText;
  if not PaintComboEdit(Self, S, Alignment, True, FCanvas, Message) then
    inherited;
end;

procedure TDBDateEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.AsDateTime :=
      VarToDateTime(Value) + Frac(FDataLink.Field.AsDateTime);
  DoChange;
end;
{$ENDIF}

procedure TDBDateEdit.ApplyDate(Value: TDateTime);
begin
  FDataLink.Edit;
  inherited ApplyDate(Value);
end;

procedure TDBDateEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBDateEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TDBDateEdit.CMEnter(var Message: TCMEnter);
begin
  inherited;
end;

procedure TDBDateEdit.CMExit(var Message: TCMExit);
begin
  try
    if not (csDesigning in ComponentState) and CheckOnExit then
      CheckValidDate;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  CheckCursor;
  DoExit;
end;

{$IFDEF RX_D4}
function TDBDateEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TDBDateEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TDBDateEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TRxDBCalcEdit }

//Polaris

procedure TRxDBCalcEdit.Loaded;
begin
  inherited Loaded;
  FLEmptyIsNull := True;
end;

procedure TRxDBCalcEdit.SetEmptyIsNull(Value: Boolean);
begin
  if Value <> FEmptyIsNull then
  begin
    FEmptyIsNull := Value;
    if csLoading in ComponentState then FLEmptyIsNull := False;
  end;
end;

function TRxDBCalcEdit.GetZeroEmpty: Boolean;
begin
  Result := inherited ZeroEmpty;
end;

procedure TRxDBCalcEdit.SetZeroEmpty(Value: Boolean);
begin
  inherited ZeroEmpty := Value;
  if FLEmptyIsNull then
    SetEmptyIsNull(ZeroEmpty)
end;

function TRxDBCalcEdit.StoreEmptyIsNull: Boolean;
begin
  Result := FEmptyIsNull <> ZeroEmpty;
end;

//Polaris

constructor TRxDBCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  inherited ReadOnly := True;
//Polaris
  FEmptyIsNull := ZeroEmpty;
  FLEmptyIsNull := True;
//Polaris
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateFieldData;
  AlwaysEnable := True;
end;

destructor TRxDBCalcEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TRxDBCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TRxDBCalcEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if not ReadOnly and ((Key = VK_DELETE) or ((Key = VK_INSERT)
    and (ssShift in Shift))) then FDataLink.Edit;
end;

procedure TRxDBCalcEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key >= ' ') then begin if not PopupVisible then FDataLink.Edit; end
  else
  case Key of
    ^H, ^V, ^X:
      if not PopupVisible then FDataLink.Edit;
    #27:
      begin
        FDataLink.Reset;
        SelectAll;
        Key := #0;
      end;
  end;
end;

function TRxDBCalcEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := inherited IsValidChar(Key);
  if Result and (FDatalink.Field <> nil) then
    Result := FDatalink.Field.IsValidChar(Key);
end;

procedure TRxDBCalcEdit.UpdatePopup;
var
  Precision: Byte;
begin
  Precision := DefCalcPrecision;
  if (FDatalink <> nil) and (FDatalink.Field <> nil) and
    (FDatalink.Field is TFloatField) then
    Precision := TFloatField(FDatalink.Field).Precision;
  if FPopup <> nil then
    SetupPopupCalculator(FPopup, Precision, BeepOnError);
end;

function TRxDBCalcEdit.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

{$IFNDEF VER80}
function TRxDBCalcEdit.GetDisplayText: string;
var
  E: Extended;
begin
  if (csPaintCopy in ControlState) and (FDatalink.Field <> nil) then
  begin
    if FDataLink.Field.IsNull then
      E := 0.0
    else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      E := FDataLink.Field.AsInteger
    else if FDataLink.Field.DataType = ftBoolean then
      E := Ord(FDataLink.Field.AsBoolean)
        {$IFDEF RX_D4}
    else if FDataLink.Field is TLargeintField then
      E := TLargeintField(FDataLink.Field).AsLargeInt
        {$ENDIF}
    else
      E := FDataLink.Field.AsFloat;
    if FDataLink.Field.IsNull then
      Result := ''
    else
      Result := FormatDisplayText(E);
  end
  else
  begin
    if (FDataLink.Field = nil) then
    begin
      if (csDesigning in ComponentState) then
        Result := Format('(%s)', [Name])
      else
        Result := '';
    end
    else //Polaris Result := inherited GetDisplayText;
{//Polaris} if FDataLink.Field.IsNull then
        Result := ''
      else
        Result := inherited GetDisplayText;
//Polaris
  end;
end;
{$ENDIF}

procedure TRxDBCalcEdit.Reset;
begin
  FDataLink.Reset;
  inherited Reset;
end;

procedure TRxDBCalcEdit.Change;
begin
  if not Formatting then FDataLink.Modified;
  inherited Change;
end;

//Polaris

procedure TRxDBCalcEdit.DataChanged;
begin
  inherited;
  if Assigned(FDataLink) and Assigned(FDataLink.Field) and
    DecimalPlaceRound then
  begin
    EditText := DisplayText;
    try
      if (EditText <> '') then
        if (StrToFloat(EditText) = 0) and ZeroEmpty then EditText := '';
    except
    end;
  end;
end;
//Polaris

function TRxDBCalcEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRxDBCalcEdit.SetDataSource(Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    {$IFDEF RX_D4}
    if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
      {$ENDIF}
      FDataLink.DataSource := Value;
    {$IFNDEF VER80}
    if Value <> nil then Value.FreeNotification(Self);
    {$ENDIF}
    UpdateFieldParams;
  end;
end;

function TRxDBCalcEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRxDBCalcEdit.SetDataField(const Value: string);
begin
  if FDataLink.FieldName <> Value then
  begin
    FDataLink.FieldName := Value;
    UpdateFieldParams;
  end;
end;

procedure TRxDBCalcEdit.SetDefaultParams(Value: Boolean);
begin
  if DefaultParams <> Value then
  begin
    FDefaultParams := Value;
    if FDefaultParams then UpdateFieldParams;
  end;
end;

procedure TRxDBCalcEdit.UpdateFieldParams;

begin
  if FDatalink.Field <> nil then
  begin
    if FDatalink.Field is TNumericField then
    begin
      if TNumericField(FDatalink.Field).DisplayFormat <> '' then
        DisplayFormat := TNumericField(FDatalink.Field).DisplayFormat;
      Alignment := TNumericField(FDatalink.Field).Alignment;
    end;
    {$IFDEF RX_D4}
    if FDatalink.Field is TLargeintField then
    begin
      MaxValue := TLargeintField(FDatalink.Field).MaxValue;
      MinValue := TLargeintField(FDatalink.Field).MinValue;
      DecimalPlaces := 0;
      if DisplayFormat = '' then DisplayFormat := ',#';
    end
    else
      {$ENDIF}if FDatalink.Field is TIntegerField then
      begin
        MaxValue := TIntegerField(FDatalink.Field).MaxValue;
        MinValue := TIntegerField(FDatalink.Field).MinValue;
        DecimalPlaces := 0;
        if DisplayFormat = '' then DisplayFormat := ',#';
      end
        {$IFNDEF VER80}
      else if FDatalink.Field is TBCDField then
      begin
        MaxValue := TBCDField(FDatalink.Field).MaxValue;
        MinValue := TBCDField(FDatalink.Field).MinValue;
      end
        {$ENDIF}
      else if FDatalink.Field is TFloatField then
      begin
        MaxValue := TFloatField(FDatalink.Field).MaxValue;
        MinValue := TFloatField(FDatalink.Field).MinValue;
//Polaris      DecimalPlaces := TFloatField(FDatalink.Field).Precision;
        DecimalPlaces := Min(DecimalPlaces, TFloatField(FDatalink.Field).Precision);
      end
      else if FDatalink.Field is TBooleanField then
      begin
        MinValue := 0;
        MaxValue := 1;
        DecimalPlaces := 0;
        if DisplayFormat = '' then DisplayFormat := ',#';
      end;
  end;
  UpdatePopup;
end;

function TRxDBCalcEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRxDBCalcEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TRxDBCalcEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRxDBCalcEdit.DataChange(Sender: TObject);
begin
  if FDefaultParams then UpdateFieldParams;
  if FDataLink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      Self.Value := 0.0;
      EditText := '';
    end
    else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
      Self.AsInteger := FDataLink.Field.AsInteger
    else if FDataLink.Field.DataType = ftBoolean then
      Self.AsInteger := Ord(FDataLink.Field.AsBoolean)
        {$IFDEF RX_D4}
    else if FDataLink.Field is TLargeintField then
      Self.Value := TLargeintField(FDataLink.Field).AsLargeInt
        {$ENDIF}
    else
      Self.Value := FDataLink.Field.AsFloat;
    DataChanged;
  end
  else
  begin
    if csDesigning in ComponentState then
    begin
      Self.Value := 0;
      EditText := Format('(%s)', [Name]);
    end
    else
      Self.Value := 0;
  end;
end;

procedure TRxDBCalcEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TRxDBCalcEdit.UpdateFieldData(Sender: TObject);
begin
  inherited UpdateData;
//Polaris  if (Value = 0) and ZeroEmpty then FDataLink.Field.Clear
  if (Trim(Text) = EmptyStr) and FEmptyIsNull then
    FDataLink.Field.Clear
  else if FDataLink.Field.DataType in [ftSmallint, ftInteger, ftWord] then
    FDataLink.Field.AsInteger := Self.AsInteger
  else if FDataLink.Field.DataType = ftBoolean then
    FDataLink.Field.AsBoolean := Boolean(Self.AsInteger)
  else
    FDataLink.Field.AsFloat := Self.Value;
end;

{$IFNDEF VER80}

procedure TRxDBCalcEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure TRxDBCalcEdit.AcceptValue(const Value: Variant);
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    FDataLink.Field.Clear
  else
    FDataLink.Field.Value := CheckValue(Value, False);
  DoChange;
end;
{$ENDIF}

procedure TRxDBCalcEdit.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBCalcEdit.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

// Polaris
(*
procedure TRxDBCalcEdit.CMEnter(var Message: TCMEnter);
begin
  try
    if (FDatalink <> Nil) and (FDatalink.Field <> Nil) and
       (FDatalink.Field.DataSet.State in [dsEdit,dsInsert]) and
       not FDatalink.Field.IsNull and
       DecimalPlaceRound and (DecimalPlaces > 0)
      then
      begin
        // А здесь отсекаем лишние цифры мантиссы
        FDatalink.Field.AsFloat := CheckValue( Value, false );
        FDatalink.UpdateRecord;
        // -- типа отсекли --
      end;
  finally
    inherited;
  end;
end;
*)
// Polaris

procedure TRxDBCalcEdit.CMExit(var Message: TCMExit);
begin
  try
    CheckRange;
    FDataLink.UpdateRecord;
  except
    SelectAll;
    if CanFocus then SetFocus;
    raise;
  end;
  inherited;
end;

{$IFDEF RX_D4}

function TRxDBCalcEdit.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;

function TRxDBCalcEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TRxDBCalcEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

{ TStatusDataLink }

type
  TStatusDataLink = class(TDataLink)
  private
    FLabel: TDBStatusLabel;
  protected
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure LayoutChanged; override;
  public
    constructor Create(ALabel: TDBStatusLabel);
    destructor Destroy; override;
  end;

constructor TStatusDataLink.Create(ALabel: TDBStatusLabel);
begin
  inherited Create;
  FLabel := ALabel;
end;

destructor TStatusDataLink.Destroy;
begin
  FLabel := nil;
  inherited Destroy;
end;

procedure TStatusDataLink.ActiveChanged;
begin
  DataSetChanged;
end;

procedure TStatusDataLink.DataSetScrolled(Distance: Integer);
begin
  if (FLabel <> nil) and (FLabel.Style = lsRecordNo) then
    FLabel.UpdateStatus;
end;

procedure TStatusDataLink.EditingChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    FLabel.UpdateStatus;
end;

procedure TStatusDataLink.DataSetChanged;
begin
  if (FLabel <> nil) then FLabel.UpdateData;
end;

procedure TStatusDataLink.LayoutChanged;
begin
  if (FLabel <> nil) and (FLabel.Style <> lsRecordSize) then
    DataSetChanged; { ??? }
end;

{ TDBStatusLabel }

const
  GlyphSpacing = 2;
  GlyphColumns = 7;

constructor TDBStatusLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShadowSize := 0;
  Layout := tlCenter;
  ControlStyle := ControlStyle - [csSetCaption{$IFNDEF VER80},
  csReplicatable{$ENDIF}];
  FRecordCount := -1;
  FRecordNo := -1;
  ShowAccelChar := False;
  {$IFDEF RX_D4} // Polaris
  FDataSetName := EmptyStr;
  {$ELSE}
  FDataSetName := NullStr;
  {$ENDIF}
  FDataLink := TStatusDataLink.Create(Self);
  FStyle := lsState;
  GlyphAlign := glGlyphLeft;
  FEditColor := clRed;
  FCaptions := TStringList.Create;
  TStringList(FCaptions).OnChange := CaptionsChanged;
  FGlyph := TBitmap.Create;
  FGlyph.Handle := LoadBitmap(HInstance, 'DS_STATES');
  Caption := '';
end;

destructor TDBStatusLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  {$IFNDEF RX_D4} // Polaris
  DisposeStr(FDataSetName);
  {$ENDIF}
  TStringList(FCaptions).OnChange := nil;
  FCaptions.Free;
  FCaptions := nil;
  FCell.Free;
  FCell := nil;
  FGlyph.Free;
  FGlyph := nil;
  inherited Destroy;
end;

procedure TDBStatusLabel.Loaded;
begin
  inherited Loaded;
  UpdateData;
end;

function TDBStatusLabel.GetDefaultFontColor: TColor;
begin
  if (FStyle = lsState) and (FDatalink <> nil) and
    (GetDatasetState in [dsEdit, dsInsert]) then
    Result := FEditColor
  else
    Result := inherited GetDefaultFontColor;
end;

function TDBStatusLabel.GetLabelCaption: string;
begin
  if (csDesigning in ComponentState) and ((FStyle = lsState) or
    (FDatalink = nil) or not FDatalink.Active) then
    Result := Format('(%s)', [Name])
  else if ((FDatalink = nil) or (DataSource = nil)) then
    Result := ''
  else
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doCaption, doBoth] then
        begin
          if DataSetName = '' then
            Result := GetCaption(DataSource.State)
          else
            Result := Format('%s: %s', [DataSetName, GetCaption(DataSource.State)]);
        end
        else { doGlyph }
          Result := '';
      lsRecordNo:
        if FDataLink.Active then
        begin
          if FRecordNo >= 0 then
          begin
            if FRecordCount >= 0 then
              Result := Format('%d:%d', [FRecordNo, FRecordCount])
            else
              Result := IntToStr(FRecordNo);
          end
          else
          begin
            if FRecordCount >= 0 then
              Result := Format('( %d )', [FRecordCount])
            else
              Result := '';
          end;
        end
        else
          Result := '';
      lsRecordSize:
        if FDatalink.Active then
          Result := IntToStr(FDatalink.DataSet.RecordSize)
        else
          Result := '';
    end;
  end;
end;

function TDBStatusLabel.GetDatasetState: TDataSetState;
begin
  if DataSource <> nil then
    Result := DataSource.State
  else
    Result := dsInactive;
end;

procedure TDBStatusLabel.SetName(const Value: TComponentName);
begin
  inherited SetName(Value);
  if (csDesigning in ComponentState) then Invalidate;
end;

procedure TDBStatusLabel.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

function TDBStatusLabel.GetStatusKind(State: TDataSetState): TDBStatusKind;
begin
  {$IFNDEF VER80}
  if not (State in [Low(TDBStatusKind)..High(TDBStatusKind)]) then
  begin
    case State of
      dsFilter: Result := dsSetKey;
      {$IFDEF RX_D3}
      dsNewValue, dsOldValue, dsCurValue: Result := dsEdit;
      {$ELSE}
      dsUpdateNew, dsUpdateOld: Result := dsEdit;
      {$ENDIF}
    else
      Result := TDBStatusKind(State);
    end;
  end
  else
    {$ENDIF}
    Result := TDBStatusKind(State);
end;

function TDBStatusLabel.GetCaption(State: TDataSetState): string;
const
  StrIds: array[TDBStatusKind] of Integer = (SInactiveData, SBrowseData,
    SEditData, SInsertData, SSetKeyData, SCalcFieldsData);
var
  Kind: TDBStatusKind;
begin
  Kind := GetStatusKind(State);
  if (FCaptions <> nil) and (Ord(Kind) < FCaptions.Count) and
    (FCaptions[Ord(Kind)] <> '') then
    Result := FCaptions[Ord(Kind)]
  else
    Result := RxLoadStr(StrIds[Kind]);
end;

procedure TDBStatusLabel.Paint;
var
  GlyphOrigin: TPoint;
begin
  inherited Paint;
  if (FStyle = lsState) and (FShowOptions in [doGlyph, doBoth]) and
    (FCell <> nil) then
  begin
    if GlyphAlign = glGlyphLeft then
      GlyphOrigin.X := GlyphSpacing
    else {glGlyphRight}
      GlyphOrigin.X := Left + ClientWidth - RightMargin + GlyphSpacing;
    case Layout of
      tlTop: GlyphOrigin.Y := 0;
      tlCenter: GlyphOrigin.Y := (ClientHeight - FCell.Height) div 2;
    else { tlBottom }
      GlyphOrigin.Y := ClientHeight - FCell.Height;
    end;
    DrawBitmapTransparent(Canvas, GlyphOrigin.X, GlyphOrigin.Y,
      FCell, FGlyph.TransparentColor);
  end;
end;

procedure TDBStatusLabel.CaptionsChanged(Sender: TObject);
begin
  TStringList(FCaptions).OnChange := nil;
  try
    while (Pred(FCaptions.Count) > Ord(High(TDBStatusKind))) do
      FCaptions.Delete(FCaptions.Count - 1);
  finally
    TStringList(FCaptions).OnChange := CaptionsChanged;
  end;
  if not (csDesigning in ComponentState) then Invalidate;
end;

procedure TDBStatusLabel.UpdateData;

  function IsSequenced: Boolean;
  begin
    {$IFDEF RX_D3}
    Result := FDatalink.DataSet.IsSequenced;
    {$ELSE}
    Result := not ((FDatalink.DataSet is TDBDataSet) and
      TDBDataSet(FDatalink.DataSet).Database.IsSQLBased);
    {$ENDIF}
  end;

begin
  FRecordCount := -1;
  if (FStyle = lsRecordNo) and FDataLink.Active and
    (DataSource.State in [dsBrowse, dsEdit]) then
  begin
    if Assigned(FOnGetRecordCount) then
      FOnGetRecordCount(Self, FDataLink.DataSet, FRecordCount)
    else if (FCalcCount or IsSequenced) then
      {$IFDEF RX_D3}
      FRecordCount := FDataLink.DataSet.RecordCount;
    {$ELSE}
      FRecordCount := DataSetRecordCount(FDataLink.DataSet)
        {$ENDIF}
  end;
  UpdateStatus;
end;

procedure TDBStatusLabel.UpdateStatus;
begin
  if DataSource <> nil then
  begin
    case FStyle of
      lsState:
        if FShowOptions in [doGlyph, doBoth] then
        begin
          if GlyphAlign = glGlyphLeft then
          begin
            RightMargin := 0;
            LeftMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end
          else {glGlyphRight}
          begin
            LeftMargin := 0;
            RightMargin := (FGlyph.Width div GlyphColumns) + GlyphSpacing * 2;
          end;
          if FCell = nil then FCell := TBitmap.Create;
          AssignBitmapCell(FGlyph, FCell, GlyphColumns, 1,
            Ord(GetStatusKind(DataSource.State)));
        end
        else { doCaption }
        begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
        end;
      lsRecordNo:
        begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
          FRecordNo := -1;
          if FDataLink.Active then
          begin
            if Assigned(FOnGetRecNo) then
              FOnGetRecNo(Self, FDataLink.DataSet, FRecordNo)
            else
            try
              {$IFDEF RX_D3}
              with FDatalink.DataSet do
                if not IsEmpty then FRecordNo := RecNo;
              {$ELSE}
              FRecordNo := DataSetRecNo(FDatalink.DataSet);
              {$ENDIF}
            except
            end;
          end;
        end;
      lsRecordSize:
        begin
          FCell.Free;
          FCell := nil;
          LeftMargin := 0;
          RightMargin := 0;
        end;
    end;
  end
  else
  begin
    FCell.Free;
    FCell := nil;
  end;
  AdjustBounds;
  Invalidate;
end;

procedure TDBStatusLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TDBStatusLabel.GetDataSetName: string;
begin
  {$IFDEF RX_D4} // Polaris
  Result := FDataSetName;
  {$ELSE}
  Result := FDataSetName^;
  {$ENDIF}
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FOnGetDataName) then
      Result := FOnGetDataName(Self)
    else if (Result = '') and (DataSource <> nil) and
      (DataSource.DataSet <> nil) then
      Result := DataSource.DataSet.Name;
  end;
end;

procedure TDBStatusLabel.SetDataSetName(Value: string);
begin
  {$IFDEF RX_D4} // Polaris
  FDataSetName := Value;
  {$ELSE}
  AssignStr(FDataSetName, Value);
  {$ENDIF}
  Invalidate;
end;

function TDBStatusLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBStatusLabel.SetDataSource(Value: TDataSource);
begin
  {$IFDEF RX_D4}
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    {$ENDIF}
    FDataLink.DataSource := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
  if not (csLoading in ComponentState) then UpdateData;
end;

procedure TDBStatusLabel.SetEditColor(Value: TColor);
begin
  if FEditColor <> Value then
  begin
    FEditColor := Value;
    if Style = lsState then Invalidate;
  end;
end;

procedure TDBStatusLabel.SetGlyphAlign(Value: TGlyphAlign);
begin
  if FGlyphAlign <> Value then
  begin
    FGlyphAlign := Value;
    UpdateStatus;
  end;
end;

procedure TDBStatusLabel.SetShowOptions(Value: TDBLabelOptions);
begin
  if FShowOptions <> Value then
  begin
    FShowOptions := Value;
    UpdateStatus;
  end;
end;

procedure TDBStatusLabel.SetCalcCount(Value: Boolean);
begin
  if FCalcCount <> Value then
  begin
    FCalcCount := Value;
    if not (csLoading in ComponentState) then UpdateData;
  end;
end;

procedure TDBStatusLabel.SetStyle(Value: TDBLabelStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if not (csLoading in ComponentState) then UpdateData;
  end;
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