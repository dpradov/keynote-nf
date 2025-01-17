{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998-2000 Polaris Software      }
{                                                       }
{ Adopted from Components library for Delphi and BCB++. }
{*******************************************************}
//==============================================
//      rRecordGrid Component.
//      Copyright 1998-2000 Polaris Software
//      http://polesoft.da.ru
//      mailto: PolarisLib@mail.ru
//==============================================

unit RxRecordGrid;

{$I RX.INC}
{$R-}

interface

uses SysUtils, Messages, Windows, Classes, Graphics, Menus, Controls, Forms,
  StdCtrls, DB, DBCtrls, {$IFDEF RX_D6}Variants, {$ENDIF}
  {$IFDEF RX_D6}Types,{$ENDIF}
  {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  Mask, RxMaxMin;

const
  MaxCustomExtents = {$IFDEF RX_D16}MaxInt div 16{$ELSE}MaxListSize{$ENDIF};
  MaxShortInt = High(ShortInt);

{ TrCustomRecGrid }

type
  EInvalidGridOperation = class(Exception);

  { Internal Grid types }
  TGetExtentsFunc = function(Index: Longint): Integer of object;

  TGridAxisDrawInfo = record
    EffectiveLineWidth: Integer;
    FixedBoundary: Integer;
    GridBoundary: Integer;
    GridExtent: Integer;
    LastFullVisibleCell: Longint;
    FullVisBoundary: Integer;
    FixedCellCount: Integer;
    FirstGridCell: Integer;
    GridCellCount: Integer;
    GetExtent: TGetExtentsFunc;
  end;

  TGridDrawInfo = record
    Horz, Vert: TGridAxisDrawInfo;
  end;

  TGridState = (gsNormal, gsColSizing);

  { TInplaceEdit }
  { The inplace editor is not intended to be used outside the Grid }

  TRxCustomRecGrid = class;

  TInplaceEdit = class(TCustomMaskEdit)
  private
    FGrid: TRxCustomRecGrid;
    FClickTime: Longint;
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure SetGrid(Value: TRxCustomRecGrid);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message); message WM_PASTE;
    procedure WMCut(var Message); message WM_CUT;
    procedure WMClear(var Message); message WM_CLEAR;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; virtual;
    procedure UpdateContents; virtual;
    procedure WndProc(var Message: TMessage); override;
    property Grid: TRxCustomRecGrid read FGrid;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
    procedure Deselect;
    procedure Hide;
    procedure Invalidate; override;
    procedure Move(const Loc: TRect);
    function PosEqual(const Rect: TRect): Boolean;
    procedure UpdateLoc(const Loc: TRect);
    function Visible: Boolean;
  end;

  { TRxCustomRecGrid }

  TGridOption = (goFixedVertLine, goFixedHorzLine, goHorzLine,
    goColSizing, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking);
  TGridOptions = set of TGridOption;
  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);
  TGridScrollDirection = set of (sdLeft, sdRight, sdUp, sdDown);

  TGridCoord = record
    X: Longint;
    Y: Longint;
  end;

  TGridRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Longint);
      1: (TopLeft, BottomRight: TGridCoord);
  end;

  TSelectCellEvent = procedure(Sender: TObject; Col, Row: Longint;
    var CanSelect: Boolean) of object;
  TDrawCellEvent = procedure(Sender: TObject; Col, Row: Longint;
    Rect: TRect; State: TGridDrawState) of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCustomRecGrid = class(TCustomControl)
  private
    FAnchor: TGridCoord;
    FBorderStyle: TBorderStyle;
    FCanEditModify: Boolean;
    FColCount: Longint;
    FColWidths: Pointer;
    FTabStops: Pointer;
    FCurrent: TGridCoord;
    FDefaultColWidth: Integer;
    FDefaultRowHeight: Integer;
    FFixedCols: Integer;
    FFixedRows: Integer;
    FFixedColor: TColor;
    FGridLineWidth: Integer;
    FOptions: TGridOptions;
    FRowCount: Longint;
    FScrollBars: TScrollStyle;
    FTopLeft: TGridCoord;
    FSizingIndex: Longint;
    FSizingPos, FSizingOfs: Integer;
    FMoveIndex, FMovePos: Longint;
    FHitTest: TPoint;
    FInplaceEdit: TInplaceEdit;
    FInplaceCol, FInplaceRow: Longint;
    FColOffset: Integer;
    FDefaultDrawing: Boolean;
    FEditorMode: Boolean;
    function CalcCoordFromPoint(X, Y: Integer; const DrawInfo: TGridDrawInfo): TGridCoord;
    procedure CalcDrawInfo(var DrawInfo: TGridDrawInfo);
    procedure CalcDrawInfoXY(var DrawInfo: TGridDrawInfo; UseWidth, UseHeight: Integer);
    procedure CalcFixedInfo(var DrawInfo: TGridDrawInfo);
    function CalcMaxTopLeft(const Coord: TGridCoord; const DrawInfo: TGridDrawInfo): TGridCoord;
    procedure CalcSizingState(X, Y: Integer; var State: TGridState; var Index: Longint; var SizingPos, SizingOfs: Integer; var FixedInfo: TGridDrawInfo);
    procedure ChangeSize(NewColCount, NewRowCount: Longint);
    procedure ClampInView(const Coord: TGridCoord);
    procedure DrawSizingLine(const DrawInfo: TGridDrawInfo);
    procedure DrawMove;
    procedure GridRectToScreenRect(GridRect: TGridRect; var ScreenRect: TRect; IncludeLine: Boolean);
    procedure HideEdit;
    procedure InvalidateRect(ARect: TGridRect);
    procedure ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal);
    procedure MoveAnchor(const NewAnchor: TGridCoord);
    procedure MoveCurrent(ACol, ARow: Longint; MoveAnchor, Show: Boolean);
    procedure MoveTopLeft(ALeft, ATop: Longint);
    procedure ResizeCol(Index: Longint; OldSize, NewSize: Integer);
    procedure SelectionMoved(const OldSel: TGridRect);
    procedure ScrollDataInfo(DX, DY: Integer; var DrawInfo: TGridDrawInfo);
    procedure TopLeftMoved(const OldTopLeft: TGridCoord);
    procedure UpdateScrollPos;
    procedure UpdateScrollRange;
    function GetColWidths(Index: Longint): Integer;
    function GetRowHeights(Index: Longint): Integer;
    function GetSelection: TGridRect;
    function GetTabStops(Index: Longint): Boolean;
    function GetVisibleColCount: Integer;
    function GetVisibleRowCount: Integer;
    function IsActiveControl: Boolean;
    procedure ReadColWidths(Reader: TReader);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCol(Value: Longint);
    procedure SetColCount(Value: Longint);
    procedure SetColWidths(Index: Longint; Value: Integer);
    procedure SetDefaultColWidth(Value: Integer);
    procedure SetDefaultRowHeight(Value: Integer);
    procedure SetEditorMode(Value: Boolean);
    procedure SetFixedColor(Value: TColor);
    procedure SetFixedCols(Value: Integer);
    procedure SetFixedRows(Value: Integer);
    procedure SetGridLineWidth(Value: Integer);
    procedure SetLeftCol(Value: Longint);
    procedure SetOptions(Value: TGridOptions);
    procedure SetRow(Value: Longint);
    procedure SetRowCount(Value: Longint);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetSelection(Value: TGridRect);
    procedure SetTabStops(Index: Longint; Value: Boolean);
    procedure SetTopRow(Value: Longint);
    procedure UpdateText;
    procedure WriteColWidths(Writer: TWriter);
    procedure CMCancelMode(var Msg: TMessage); message CM_CANCELMODE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure Initialize;
  protected
    FGridState: TGridState;
    FSaveCellExtents: Boolean;
    DesignOptionsBoost: TGridOptions;
    virtualView: Boolean;
    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean); dynamic;
    procedure ColEnter; virtual;
    procedure ColExit; virtual;
    procedure UpdateEdit; virtual;
    procedure Reset; virtual;
    function CreateEditor: TInplaceEdit; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
    procedure DoExit; override;
    function CellRect(ACol, ARow: Longint): TRect;
    function CanEditAcceptKey(Key: Char): Boolean; dynamic;
    function CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean; dynamic;
    function CanEditModify: Boolean; dynamic;
    function CanEditShow: Boolean; virtual;
    function GetEditText(ACol, ARow: Longint): string; dynamic;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); dynamic;
    function GetEditMask(ACol, ARow: Longint): string; dynamic;
    function GetEditLimit: Integer; dynamic;
    function GetGridWidth: Integer;
    function GetGridHeight: Integer;
    procedure HideEditor;
    procedure ShowEditor; dynamic;
    procedure ShowEditorChar(Ch: Char); dynamic;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); virtual; abstract;
    procedure DefineProperties(Filer: TFiler); override;
    function SelectCell(ACol, ARow: Longint): Boolean; virtual;
    procedure SizeChanged(OldColCount, OldRowCount: Longint); dynamic;
    function Sizing(X, Y: Integer): Boolean;
    procedure ScrollData(DX, DY: Integer);
    procedure TopLeftChanged; dynamic;
    procedure TimedScroll(Direction: TGridScrollDirection); dynamic;
    procedure Paint; override;
    procedure ColWidthsChanged; dynamic;
    procedure UpdateDesigner;
    property Col: Longint read FCurrent.X write SetCol;
    property ColWidths[Index: Longint]: Integer read GetColWidths write SetColWidths;
    property DefaultColWidth: Integer read FDefaultColWidth write SetDefaultColWidth default 64;
    property DefaultDrawing: Boolean read FDefaultDrawing write FDefaultDrawing default True;
    property EditorMode: Boolean read FEditorMode write SetEditorMode;
    property FixedCols: Integer read FFixedCols write SetFixedCols default 1;
    property GridHeight: Integer read GetGridHeight;
    property GridLineWidth: Integer read FGridLineWidth write SetGridLineWidth default 1;
    property GridWidth: Integer read GetGridWidth;
    property HitTest: TPoint read FHitTest;
    property InplaceEditor: TInplaceEdit read FInplaceEdit;
    property LeftCol: Longint read FTopLeft.X write SetLeftCol;
    property Row: Longint read FCurrent.Y write SetRow;
    property RowCount: Longint read FRowCount write SetRowCount default 5;
    property Selection: TGridRect read GetSelection write SetSelection;
    property TabStops[Index: Longint]: Boolean read GetTabStops write SetTabStops;
    property TopRow: Longint read FTopLeft.Y write SetTopRow;
    property VisibleColCount: Integer read GetVisibleColCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property ColCount: Longint read FColCount write SetColCount default 2;
    property EditRow: LongInt read FInplaceRow write FInplaceRow;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssVertical;

    property Color default clWindow;
    property FixedRows: Integer read FFixedRows write SetFixedRows default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DefaultRowHeight: Integer read FDefaultRowHeight write SetDefaultRowHeight default 17;
    property FixedColor: TColor read FFixedColor write SetFixedColor default clBtnFace;
    property Options: TGridOptions read FOptions write SetOptions
      default [goFixedVertLine, goFixedHorzLine, goHorzLine, goEditing, goColSizing];
    property ParentColor default False;
    procedure InvalidateRow(ARow: Longint);
  public
    property Editor: TInplaceEdit read FInplaceEdit write FInplaceEdit;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MouseCoord(X, Y: Integer): TGridCoord;
    procedure Invalidate; override;
    procedure InvalidateData; virtual;
  published
    property TabStop default True;
  end;

{ TrRecordGrid }

type

  TValidateEvent = procedure(Sender: TObject; ARow: Longint; PickList: TStrings; var Value: Variant; var Accept: Boolean) of object;
  TGetTextEvent = procedure(Sender: TObject; ACol, ARow: Longint; PickList: TStrings; var Value: Variant) of object;
  TGetMaskEvent = procedure(Sender: TObject; ARow: Longint; var Mask: string) of object;
  TSetTextEvent = procedure(Sender: TObject; ARow: Longint; Modified: Boolean; PickList: TStrings; const Value: Variant) of object;
  TSetEditEvent = procedure(Sender: TObject; ARow: Longint; PickList: TStrings; const Value: Variant) of object;
  TMovedEvent = procedure(Sender: TObject; FromIndex, ToIndex: Longint) of object;
  TEditButtonClick = procedure(Sender: TObject; ARow: Longint; Field: TField) of object;

  TRxDBRecordGrid = class;
  TRow = class;

  { TRowTitle }

  TRowTitle = class(TPersistent)
  private
    FColumn: TRow;
    FCaption: string;
    FFont: TFont;
    FColor: TColor;
    FAlignment: TAlignment;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetCaption: string;
    function GetFont: TFont;
    function IsAlignmentStored: Boolean;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsCaptionStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetCaption(const Value: string); virtual;
  protected
    procedure RefreshDefaultFont;
  public
    constructor Create(Column: TRow);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultAlignment: TAlignment;
    function DefaultColor: TColor;
    function DefaultFont: TFont;
    function DefaultCaption: string;
    procedure RestoreDefaults; virtual;
    property Column: TRow read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment
      stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  { TrGridDataLink }

  TrGridDataLink = class(TDataLink)
  private
    FGrid: TRxDBRecordGrid;
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(AGrid: TRxDBRecordGrid);
    destructor Destroy; override;
  end;

  TColumnButtonStyle = (cbsAuto, cbsDropDown, cbsDropDownList, cbsEllipsis, cbsNone);

  TColumnValue = (cvAlignment, cvReadOnly, cvImeMode, cvImeName, cvFont, cvTitleCaption,
    cvTitleColor, cvTitleAlignment, cvTitleFont);
  TColumnValues = set of TColumnValue;
  TRecGridRows = class;

  { TRow }

  TRow = class(TCollectionItem)
  private
    FObject: TObject;
    FField: TField;
    FFieldName: string;
    FTitle: TRowTitle;
    FFont: TFont;
    FStored: Boolean;
    FValue: Variant;
    FPickList: TStrings;
    FPopupMenu: TPopupMenu;
    FDropDownRows: Cardinal;
    FButtonStyle: TColumnButtonStyle;
    FEditMask: string;
    FOnEditButtonClick: TNotifyEvent;
    FAlignment: TAlignment;
    FReadonly: Boolean;
    FAssignedValues: TColumnValues;
    FImeMode: TImeMode;
    FImeName: TImeName;

    FIndentLevel: Cardinal;
    FExpanded: Boolean;
    FNodes: TRecGridRows;
    FParent: TRow;
    FHasChildren: Boolean;
    FIDCode: string;

    FOnGetEditMask: TGetMaskEvent;
    FOnGetEditText: TGetTextEvent;
    FOnGetText: TGetTextEvent;
    FOnSetEditText: TSetEditEvent;
    FOnSetText: TSetTextEvent;
    FOnValidate: TValidateEvent;

    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    function IsImeModeStored: Boolean;
    function IsImeNameStored: Boolean;
    procedure SetImeMode(Value: TImeMode); virtual;
    procedure SetImeName(Value: TImeName); virtual;

    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment); virtual;
    function GetPickList: TStrings;
    function GetReadOnly: Boolean;
    function IsAlignmentStored: Boolean;
    function IsReadOnlyStored: Boolean;
    procedure SetButtonStyle(Value: TColumnButtonStyle);
    procedure SetPickList(Value: TStrings);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure EditButtonClick;

    function GetField: TField;
    procedure SetField(Value: TField); virtual;
    procedure SetFieldName(const Value: string);
    procedure SetTitle(Value: TRowTitle);
    //
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function IsFontStored: Boolean;
  protected
    function GetDisplayName: string; override;
    function GetEditMask: string;
    function GetText: string;
    procedure SetText(Value: string);
    procedure SetIndentLevel(Value: Cardinal);
    procedure SetExpanded(Value: Boolean);
    procedure SetIndex(Value: Integer); override;
    function SetParentAndLevel: Integer;
    function CreateTitle: TRowTitle; virtual;
    //
    procedure RefreshDefaultFont;
    procedure FontChanged(Sender: TObject);
    property IsStored: Boolean read FStored write FStored default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetGrid: TRxDBRecordGrid;
    function DefaultAlignment: TAlignment;
    function DefaultReadOnly: Boolean;
    function DefaultImeMode: TImeMode;
    function DefaultImeName: TImeName;
    procedure Assign(Source: TPersistent); override;
    procedure Refresh; virtual;
    property Grid: TRxDBRecordGrid read GetGrid;
    property Field: TField read GetField write SetField;
    property AssignedValues: TColumnValues read FAssignedValues;
    property Value: Variant read FValue write FValue;
    property Cargo: TObject read FObject write FObject;
    property Nodes: TRecGridRows read FNodes write FNodes;
    property Parent: TRow read FParent write FParent;
    property HasChildren: Boolean read FHasChildren write FHasChildren;
    //
    function DefaultFont: TFont;
  published
    property PickList: TStrings read GetPickList write SetPickList;
    property FieldName: string read FFieldName write SetFieldName;
    property Title: TRowTitle read FTitle write SetTitle;
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
    property DropDownRows: Cardinal read FDropDownRows write FDropDownRows default 7;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property EditMask: string read GetEditMask write FEditMask;
    property ImeMode: TImeMode read GetImeMode write SetImeMode stored IsImeModeStored;
    property ImeName: TImeName read GetImeName write SetImeName stored IsImeNameStored;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;
    property OnGetEditMask: TGetMaskEvent read FOnGetEditMask write FOnGetEditMask;
    property OnGetEditText: TGetTextEvent read FOnGetEditText write FOnGetEditText;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    property OnSetEditText: TSetEditEvent read FOnSetEditText write FOnSetEditText;
    property OnSetText: TSetTextEvent read FOnSetText write FOnSetText;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property Text: string read GetText write SetText;
    property IndentLevel: Cardinal read FIndentLevel write SetIndentLevel default 0;
    property Expanded: Boolean read FExpanded write SetExpanded default False;
    property IDCode: string read FIDCode write FIDCode;
    //
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  TColumnClass = class of TRow;

  { TRecGridRows }

  TRecGridRows = class(TCollection)
  private
    FGrid: TRxDBRecordGrid;
//    FEmpty: Boolean;
    function GetColByID(Index: Variant): TRow;
    function GetColumn(Index: Variant): TRow;
    procedure SetColumn(Index: Variant; Value: TRow);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Grid: TRxDBRecordGrid; ColumnClass: TColumnClass; AddEmpty: Boolean);
    function Add: TRow;
    procedure Clear;
    procedure Delete(Index: Variant);
    procedure RebuildRows;
    property Grid: TRxDBRecordGrid read FGrid;
    property Items[Index: Variant]: TRow read GetColumn write SetColumn; default;
  end;

  { TrRecordGrid }

  TRecGridState = (gsDefault, gsCustomized);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBRecordGrid = class(TRxCustomRecGrid)
  private
    FDataLink: TrGridDataLink;
    FRows: TRecGridRows;
    FState: TRecGridState;
    FRebuildRowsIfEmpty: Boolean;
    FObject: TObject;
    FOnTopLeftChanged: TNotifyEvent;
    FOnColEnter: TNotifyEvent;
    FOnColExit: TNotifyEvent;
    FReadOnly: Boolean;
    FUpdating: Boolean;
    FOnEditButtonClick: TEditButtonClick;
    FUpdatingEditor: Boolean;
    FOriginalImeName: TImeName;
    FOriginalImeMode: TImeMode;
    procedure SetIme;
    procedure UpdateIme;
    procedure WMIMEStartComp(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetRecGridRows(Value: TRecGridRows);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SetFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure SetState(const Value: TRecGridState);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override; //

    function GetEditLimit: Integer; override;

    procedure FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditMask(ACol, ARow: Longint): string; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const cValue: string); override;
    procedure TopLeftChanged; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property FixedCols;
    function CreateEditor: TInplaceEdit; override;
    function CreateRows: TRecGridRows; dynamic;
    property EditRow;
    procedure ColEnter; override;
    procedure ColExit; override;
    property InUpdating: Boolean read FUpdating write FUpdating;
    procedure DblClick; override;
    procedure Loaded; override;
    procedure ResyncFields;
    function CanEditAcceptKey(Key: Char): Boolean; override;
    procedure ShowEditor; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
{$ENDIF}

    procedure DefaultHandler(var Msg); override;
    function CellRect(ACol, ARow: Longint): TRect;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure Reset; override;
    procedure Refresh; virtual;
    procedure RefreshData; virtual;
    property DataLink: TrGridDataLink read FDataLink;
    property Cargo: TObject read FObject write FObject;
    property Canvas;
    property Row;
    property TabStops;
    property TopRow;
    property State: TRecGridState read FState write SetState;
    property RebuildRowsIfEmpty: Boolean read FRebuildRowsIfEmpty write FRebuildRowsIfEmpty;
  published
    property Rows: TRecGridRows read FRows write SetRecGridRows;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Anchors;
    property Constraints;
    property Color;
    property FixedRows;
    property BorderStyle;
    property DefaultRowHeight;
    property FixedColor;
    property Options;
    property ImeMode;
    property ImeName;
    property Align;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GridLineWidth;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleRowCount;
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
    property OnStartDrag;
    property OnTopLeftChanged: TNotifyEvent read FOnTopLeftChanged write FOnTopLeftChanged;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnEditButtonClick: TEditButtonClick read FOnEditButtonClick write FOnEditButtonClick;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;

const ColumnTitleValues = [cvTitleColor..cvTitleFont];

implementation

uses
  Consts, RxStrUtils;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxCustomExtents] of Integer;

procedure InvalidOp(const ID: string);
begin
  raise EInvalidGridOperation.Create(ID);
end;

function GridRect(Coord1, Coord2: TGridCoord): TGridRect;
begin
  with Result do
  begin
    Left := Coord2.X;
    if Coord1.X < Coord2.X then Left := Coord1.X;
    Right := Coord1.X;
    if Coord1.X < Coord2.X then Right := Coord2.X;
    Top := Coord2.Y;
    if Coord1.Y < Coord2.Y then Top := Coord1.Y;
    Bottom := Coord1.Y;
    if Coord1.Y < Coord2.Y then Bottom := Coord2.Y;
  end;
end;

function PointInGridRect(Col, Row: Longint; const Rect: TGridRect): Boolean;
begin
  Result := (Col >= Rect.Left) and (Col <= Rect.Right) and (Row >= Rect.Top)
    and (Row <= Rect.Bottom);
end;

type
  TXorRects = array[0..3] of TRect;

procedure XorRects(const R1, R2: TRect; var XorRects: TXorRects);
var
  Intersect, Union: TRect;

  function PtInRect(X, Y: Integer; const Rect: TRect): Boolean;
  begin
    with Rect do Result := (X >= Left) and (X <= Right) and (Y >= Top) and
      (Y <= Bottom);
  end;

  function Includes(const P1: TPoint; var P2: TPoint): Boolean;
  begin
    with P1 do
    begin
      Result := PtInRect(X, Y, R1) or PtInRect(X, Y, R2);
      if Result then P2 := P1;
    end;
  end;

  function Build(var R: TRect; const P1, P2, P3: TPoint): Boolean;
  begin
    Build := True;
    with R do
      if Includes(P1, TopLeft) then
      begin
        if not Includes(P3, BottomRight) then BottomRight := P2;
      end
      else if Includes(P2, TopLeft) then BottomRight := P3
      else Build := False;
  end;

begin
  FillChar(XorRects, SizeOf(XorRects), 0);
  if not Bool(IntersectRect(Intersect, R1, R2)) then
  begin
    { Don't intersect so its simple }
    XorRects[0] := R1;
    XorRects[1] := R2;
  end
  else
  begin
    UnionRect(Union, R1, R2);
    if Build(XorRects[0],
      Point(Union.Left, Union.Top),
      Point(Union.Left, Intersect.Top),
      Point(Union.Left, Intersect.Bottom))
    then
      XorRects[0].Right := Intersect.Left;
    if Build(XorRects[1],
      Point(Intersect.Left, Union.Top),
      Point(Intersect.Right, Union.Top),
      Point(Union.Right, Union.Top))
    then
      XorRects[1].Bottom := Intersect.Top;
    if Build(XorRects[2],
      Point(Union.Right, Intersect.Top),
      Point(Union.Right, Intersect.Bottom),
      Point(Union.Right, Union.Bottom))
    then
      XorRects[2].Left := Intersect.Right;
    if Build(XorRects[3],
      Point(Union.Left, Union.Bottom),
      Point(Intersect.Left, Union.Bottom),
      Point(Intersect.Right, Union.Bottom))
    then
      XorRects[3].Top := Intersect.Bottom;
  end;
end;

procedure ModifyExtents(var Extents: Pointer; Index, Amount: Longint; Default: Integer);
var
  LongSize: LongInt;
  NewSize: Cardinal;
  OldSize: Cardinal;
  I: Cardinal;
begin
  if Amount <> 0 then
  begin
    if not Assigned(Extents) then OldSize := 0
    else OldSize := PIntArray(Extents)^[0];
    if (Index < 0) or (Longint(OldSize) < Index) then
      InvalidOp(SIndexOutOfRange);
    LongSize := Longint(OldSize) + Amount;
    if LongSize < 0 then
      InvalidOp(STooManyDeleted)
    else
      if LongSize >= {$IFDEF RX_D16}MaxInt div 16{$ELSE}MaxListSize{$ENDIF} - 1 then
        InvalidOp(SGridTooLarge);
    NewSize := Cardinal(LongSize);
    if NewSize > 0 then Inc(NewSize);
    ReallocMem(Extents, NewSize * SizeOf(Integer));
    if Assigned(Extents) then
    begin
      I := Index;
      while I < NewSize do
      begin
        PIntArray(Extents)^[I] := Default;
        Inc(I);
      end;
      PIntArray(Extents)^[0] := NewSize - 1;
    end;
  end;
end;

procedure UpdateExtents(var Extents: Pointer; NewSize: Longint;
  Default: Integer);
var
  OldSize: Integer;
begin
  OldSize := 0;
  if Assigned(Extents) then OldSize := PIntArray(Extents)^[0];
  ModifyExtents(Extents, OldSize, NewSize - OldSize, Default);
end;

function CompareExtents(E1, E2: Pointer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if E1 <> nil then
  begin
    if E2 <> nil then
    begin
      for I := 0 to PIntArray(E1)^[0] do
        if PIntArray(E1)^[I] <> PIntArray(E2)^[I] then Exit;
      Result := True;
    end
  end
  else Result := E2 = nil;
end;

{ TInplaceEdit }

{ Private. LongMulDiv multiplys the first two arguments and then
divides by the third.  This is used so that real number
(floating point) arithmetic is not necessary.  This routine saves
the possible 64-bit value in a temp before doing the divide.  Does
not do error checking like divide by zero.  Also assumes that the
result is in the 32-bit range (Actually 31-bit, since this algorithm
is for unsigned). }

function LongMulDiv(Mult1, Mult2, Div1: Longint): Longint; stdcall;
  external 'kernel32.dll' name 'MulDiv';

type
  TSelection = record
    StartPos, EndPos: Integer;
  end;

constructor TInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
end;

procedure TInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TInplaceEdit.SetGrid(Value: TRxCustomRecGrid);
begin
  FGrid := Value;
end;

procedure TInplaceEdit.CMShowingChanged(var Message: TMessage);
begin
  { Ignore showing using the Visible property }
end;

procedure TInplaceEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  if goTabs in Grid.Options then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

procedure TInplaceEdit.WMPaste(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEdit.WMClear(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEdit.WMCut(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TInplaceEdit.DblClick;
begin
  Grid.DblClick;
end;

function TInplaceEdit.EditCanModify: Boolean;
begin
  Result := Grid.CanEditModify;
end;

procedure TInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    Grid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := Grid.OnKeyDown;
    if Assigned(GridKeyDown) then
      GridKeyDown(Grid, Key, Shift);
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, WPARAM(@Result.StartPos), LPARAM(@Result.EndPos));
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := ((StartPos = 0) or (endPos = StartPos)) and
        (endPos = GetTextLen);
  end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (StartPos = 0) and ((endPos = 0) or (endPos = GetTextLen));
  end;

begin
  case Key of
    VK_ESCAPE: begin
        Reset;
        SendToParent;
      end;
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      SendToParent;
    VK_INSERT:
      if Shift = [] then SendToParent
      else if (Shift = [ssShift]) and not Grid.CanEditModify then Key := 0;
    VK_F2:
      begin
        ParentEvent;
        if Key = VK_F2 then
        begin
          Deselect;
          Exit;
        end;
      end;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
  end;
  if (Key = VK_DELETE) and not Grid.CanEditModify then Key := 0;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TInplaceEdit.KeyPress(var Key: Char);
var
  Selection: TSelection;
begin
  Grid.KeyPress(Key);
  if (Key >= ' ') and not Grid.CanEditAcceptKey(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  if (Key >= ' ') then begin if not Grid.CanEditModify then Key := #0; end
  else
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        SendMessage(Handle, EM_GETSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.endPos));
        if (Selection.StartPos = 0) and (Selection.endPos = GetTextLen) then
          Deselect
        else
          SelectAll;
        Key := #0;
      end;
    ^H, ^V, ^X:
      if not Grid.CanEditModify then Key := #0;
  end;
  if Key <> #0 then
    inherited KeyPress(Key);
end;

procedure TInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Grid.KeyUp(Key, Shift);
end;

procedure TInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(Grid) then
          Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      begin
        if Cardinal(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TInplaceEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, WPARAM($7FFFFFFF), LPARAM($FFFFFFFF));
end;

procedure TInplaceEdit.Invalidate;
var
  Cur: TRect;
begin
  inherited Invalidate;
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, @Cur);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TInplaceEdit.Hide;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
      SWP_NOREDRAW);
    if Focused then Windows.SetFocus(Grid.Handle);
  end;
end;

function TInplaceEdit.PosEqual(const Rect: TRect): Boolean;
var
  Cur: TRect;
begin
  GetWindowRect(Handle, Cur);
  MapWindowPoints(HWND_DESKTOP, Grid.Handle, Cur, 2);
  Result := EqualRect(Rect, Cur);
end;

procedure TInplaceEdit.InternalMove(const Loc: TRect; Redraw: Boolean);
begin
  if IsRectEmpty(Loc) then
    Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
    with Loc do
      SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top, SWP_SHOWWINDOW or SWP_NOREDRAW);
    BoundsChanged;
    if Redraw then
      Invalidate;
    if Grid.Focused then
      Windows.SetFocus(Handle);
  end;
end;

procedure TInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  R := Rect(2, 2, Width - 2, Height);
  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TInplaceEdit.UpdateLoc(const Loc: TRect);
begin
  InternalMove(Loc, False);
end;

function TInplaceEdit.Visible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TInplaceEdit.Move(const Loc: TRect);
begin
  InternalMove(Loc, True);
end;

procedure TInplaceEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then Windows.SetFocus(Handle);
end;

procedure TInplaceEdit.UpdateContents;
begin
  Text := '';
  EditMask := Grid.GetEditMask(Grid.Col, Grid.Row);
  Text := Grid.GetEditText(Grid.Col, Grid.Row);
  MaxLength := Grid.GetEditLimit;
end;

{ TRxCustomRecGrid }

constructor TRxCustomRecGrid.Create(AOwner: TComponent);
const
  GridStyle = [csCaptureMouse, csOpaque, csDoubleClicks];
begin
  inherited Create(AOwner);
  if NewStyleControls then
    ControlStyle := GridStyle
  else
    ControlStyle := GridStyle + [csFramed];
  FCanEditModify := True;
  FColCount := 2;
  FRowCount := 1;
  FFixedCols := 1;
  FFixedRows := 0;
  FGridLineWidth := 1;
  FInplaceCol := -1;
  FInplaceRow := -1;
  FOptions := [goFixedVertLine, goFixedHorzLine, goHorzLine, goEditing, goColSizing];
  FFixedColor := clBtnFace;
  FScrollBars := ssVertical;
  FDefaultRowHeight := 17;
  Color := clWindow;
  FBorderStyle := bsSingle;
  FDefaultColWidth := 64;
  FDefaultDrawing := True;
  FSaveCellExtents := True;
  FEditorMode := False;
  ParentColor := False;
  TabStop := True;
  SetBounds(Left, Top, FColCount * FDefaultColWidth,
    Max(FRowCount, 5) * FDefaultRowHeight + (FRowCount - 1) * 2);
  Initialize;
end;

procedure TRxCustomRecGrid.Reset;
begin
  FRowCount := 1;
  FInplaceCol := -1;
  FInplaceRow := -1;
  FTopLeft.X := FixedCols;
  FTopLeft.Y := FixedRows;
  FCurrent := FTopLeft;
  FAnchor := FCurrent;
  HideEditor;
  inherited Invalidate;
end;

destructor TRxCustomRecGrid.Destroy;
begin
  FInplaceEdit.Free;
  FreeMem(FColWidths);
  FreeMem(FTabStops);
  inherited Destroy;
end;

function TRxCustomRecGrid.BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
var
  GridRect: TGridRect;
begin
  GridRect.Left := ALeft;
  GridRect.Right := ARight;
  GridRect.Top := ATop;
  GridRect.Bottom := ABottom;
  GridRectToScreenRect(GridRect, Result, False);
end;

procedure TRxCustomRecGrid.DoExit;
begin
  inherited DoExit;
  if not (goAlwaysShowEditor in Options) then HideEditor;
end;

function TRxCustomRecGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := BoxRect(ACol, ARow, ACol, ARow);
end;

function TRxCustomRecGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  Result := True;
end;

function TRxCustomRecGrid.CanGridAcceptKey(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;
end;

function TRxCustomRecGrid.CanEditModify: Boolean;
begin
  Result := FCanEditModify;
end;

function TRxCustomRecGrid.CanEditShow: Boolean;
begin
  Result := (goEditing in Options) and
    FEditorMode and not (csDesigning in ComponentState) and HandleAllocated and
    ((goAlwaysShowEditor in Options) or IsActiveControl);
end;

function TRxCustomRecGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm(Self);
  if Assigned(ParentForm) then
  begin
    if (ParentForm.ActiveControl = Self) then
      Result := True
  end
  else
  begin
    H := GetFocus;
    while IsWindow(H) and (Result = False) do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent(H);
    end;
  end;
end;

function TRxCustomRecGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := '';
end;

function TRxCustomRecGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := '';
end;

procedure TRxCustomRecGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
end;

function TRxCustomRecGrid.GetEditLimit: Integer;
begin
  Result := 0;
end;

procedure TRxCustomRecGrid.HideEditor;
begin
  FEditorMode := False;
  HideEdit;
  //RSV
  FInplaceCol := -1;
  FInplaceRow := -1;
end;

procedure TRxCustomRecGrid.ShowEditor;
begin
  FEditorMode := True;
  UpdateEdit;
end;

procedure TRxCustomRecGrid.ShowEditorChar(Ch: Char);
begin
  ShowEditor;
  if FInplaceEdit <> nil then
    PostMessage(FInplaceEdit.Handle, WM_CHAR, WPARAM(Ch), 0);
end;

procedure TRxCustomRecGrid.ReadColWidths(Reader: TReader);
var
  I: Integer;
begin
  with Reader do
  begin
    ReadListbegin;
    for I := 0 to ColCount - 1 do
      ColWidths[I] := ReadInteger;
    ReadListend;
  end;
end;

procedure TRxCustomRecGrid.WriteColWidths(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListbegin;
    for I := 0 to ColCount - 1 do
      WriteInteger(ColWidths[I]);
    WriteListend;
  end;
end;

procedure TRxCustomRecGrid.DefineProperties(Filer: TFiler);

  function DoColWidths: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not CompareExtents(TRxCustomRecGrid(Filer.Ancestor).FColWidths, FColWidths)
    else
      Result := FColWidths <> nil;
  end;

begin
  inherited DefineProperties(Filer);
  if FSaveCellExtents then
    with Filer do Defineproperty('ColWidths', ReadColWidths, WriteColWidths, DoColWidths);
end;

function TRxCustomRecGrid.MouseCoord(X, Y: Integer): TGridCoord;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := CalcCoordFromPoint(X, Y, DrawInfo);
  if Result.X < 0 then Result.Y := -1
  else if Result.Y < 0 then Result.X := -1;
end;

function TRxCustomRecGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
end;

procedure TRxCustomRecGrid.SizeChanged(OldColCount, OldRowCount: Longint);
begin
end;

function TRxCustomRecGrid.Sizing(X, Y: Integer): Boolean;
var
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Index: Longint;
  Pos, Ofs: Integer;
begin
  State := FGridState;
  if State = gsNormal then
  begin
    CalcDrawInfo(DrawInfo);
    CalcSizingState(X, Y, State, Index, Pos, Ofs, DrawInfo);
  end;
  Result := State <> gsNormal;
end;

procedure TRxCustomRecGrid.TopLeftChanged;
begin
  if FEditorMode and (FInplaceEdit <> nil) then FInplaceEdit.UpdateLoc(CellRect(Col, Row));
end;

{$IFDEF WIN64}
procedure FillDWord(var Dest; Count, Value: Integer); inline;
var
  I: Integer;
  P: ^Integer;
begin
  P := @Dest;
  for I := 0 to Count do
  begin
    Move(Value, P^, SizeOf(Value));
    Inc(P);
  end;
end;
function StackAlloc(Size: Integer): Pointer;
begin
  Result := AllocMem(Size)
end;
procedure StackFree(P: Pointer);
begin
  ReallocMem(P, 0);
end;
{$ELSE}
procedure FillDWord(var Dest; Count, Value: Integer); register;
asm
  XCHG  EDX, ECX
  PUSH  EDI
  MOV   EDI, EAX
  MOV   EAX, EDX
  REP   STOSD
  POP   EDI
end;

function StackAlloc(Size: Integer): Pointer; register;
{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;
procedure StackFree(P: Pointer); register;
{ StackFree pops the memory allocated by StackAlloc off the stack.
- Calling StackFree is optional - SP will be restored when the calling routine
  exits, but it's a good idea to free the stack allocated memory ASAP anyway.
- StackFree must be called in the same stack context as StackAlloc - not in
  a subroutine or finally block.
- Multiple StackFree calls must occur in reverse order of their corresponding
  StackAlloc calls.
- Built-in sanity checks guarantee that an improper call to StackFree will not
  corrupt the stack. Worst case is that the stack block is not released until
  the calling routine exits. }
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;
{$ENDIF}



procedure TRxCustomRecGrid.Paint;
var
  LineColor: TColor;
  FixedLineColor: TColor;
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
  UpdateRect: TRect;
  PointsList: PIntArray;
  StrokeList: PIntArray;
  MaxStroke: Integer;
  FrameFlags1, FrameFlags2: DWORD;

  procedure DrawLines(DoHorz, DoVert: Boolean; Col, Row: Longint;
    const CellBounds: array of Integer; OnColor, OffColor: TColor);

  {Cellbounds is 4 integers: StartX, StartY, StopX, StopY
   Horizontal lines:  MajorIndex = 0
   Vertical lines:    MajorIndex = 1 }

  const
    FlatPenStyle = PS_Geometric or PS_Solid or PS_endCap_Flat or PS_Join_Miter;

    procedure DrawAxisLines(const AxisInfo: TGridAxisDrawInfo;
      Cell, MajorIndex: Integer; UseOnColor: Boolean);
    var
      Line: Integer;
      LogBrush: TLOGBRUSH;
      Index: Integer;
      Points: PIntArray;
      StopMajor, StartMinor, StopMinor: Integer;
    begin
      with Canvas, AxisInfo do
      begin
        if EffectiveLineWidth <> 0 then
        begin
          Pen.Width := GridLineWidth;
          if UseOnColor then
            Pen.Color := OnColor
          else
            Pen.Color := OffColor;
          if Pen.Width > 1 then
          begin
            LogBrush.lbStyle := BS_Solid;
            LogBrush.lbColor := Pen.Color;
            LogBrush.lbHatch := 0;
            Pen.Handle := ExtCreatePen(FlatPenStyle, Pen.Width, LogBrush, 0, nil);
          end;
          Points := PointsList;
          Line := CellBounds[MajorIndex] + EffectiveLineWidth shr 1 +
            GetExtent(Cell);
          StartMinor := CellBounds[MajorIndex xor 1];
          StopMinor := CellBounds[2 + (MajorIndex xor 1)];
          StopMajor := CellBounds[2 + MajorIndex] + EffectiveLineWidth;
          Index := 0;
          repeat
            Points^[Index + MajorIndex] := Line; { MoveTo }
            Points^[Index + (MajorIndex xor 1)] := StartMinor;
            Inc(Index, 2);
            Points^[Index + MajorIndex] := Line; { LineTo }
            Points^[Index + (MajorIndex xor 1)] := StopMinor;
            Inc(Index, 2);
            Inc(Cell);
            Inc(Line, GetExtent(Cell) + EffectiveLineWidth);
          until Line > StopMajor;
          { 2 integers per point, 2 points per line -> Index div 4 }
          PolyPolyLine(Canvas.Handle, Points^, StrokeList^, Index shr 2);
        end;
      end;
    end;

  begin
    if (CellBounds[0] = CellBounds[2]) or (CellBounds[1] = CellBounds[3]) then Exit;
    if not DoHorz then
    begin
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
    end
    else
    begin
      DrawAxisLines(DrawInfo.Horz, Col, 0, DoVert);
      DrawAxisLines(DrawInfo.Vert, Row, 1, DoHorz);
    end;
  end;

  procedure DrawCells(ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer;
    Color: TColor; IncludeDrawState: TGridDrawState);
  var CurCol, CurRow: Longint;
    Where: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while (Where.Top < StopY) and (CurRow < RowCount) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + FDefaultRowHeight;
      while (Where.Left < StopX) and (CurCol < ColCount) do
      begin
        Where.Right := Where.Left + ColWidths[CurCol];
        if RectVisible(Canvas.Handle, Where) then
        begin
          DrawState := IncludeDrawState;
          Focused := IsActiveControl;
          if Focused and (CurRow = Row) and (CurCol = Col) then
            Include(DrawState, gdFocused);
          if PointInGridRect(CurCol, CurRow, Sel) then
            Include(DrawState, gdSelected);
          if not (gdFocused in DrawState) or not (goEditing in Options) or
            not FEditorMode or (csDesigning in ComponentState)
          then
          begin
            if DefaultDrawing or (csDesigning in ComponentState) then
            begin
              with Canvas do
              begin
                Pen.Color := clBtnFace;
                if (gdFocused in DrawState) and (gdSelected in DrawState) then
                begin
                  Brush.Color := clHighlight;
                  Font.Color := clHighlightText;
                end
                else
                begin
                  Brush.Color := Color;
                  Font.Color := self.Font.Color;
                end;
              end;
            end;
            DrawCell(CurCol, CurRow, Where, DrawState);
            if (gdFocused in DrawState) and (gdSelected in DrawState) then
              Canvas.DrawFocusRect(Rect(Where.Left, Where.Top, Where.Right, Where.Bottom));
            if DefaultDrawing
              and (gdFixed in DrawState)
              and ((FrameFlags1 or FrameFlags2) <> 0)
              and (goFixedVertLine in Options)
            then
              DrawEdge(Canvas.Handle, Where, BDR_raiseDINNER, BF_RECT);
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc(CurCol);
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc(CurRow);
    end;
  end;

begin
  Canvas.Font := Self.Font;
  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if (Horz.EffectiveLineWidth > 0) or (Vert.EffectiveLineWidth > 0) then
    begin
      // Draw the Grid line in the four areas (fixed, fixed), (variable, fixed),
      // (fixed, variable) and (variable, variable)
      MaxStroke := Max(Horz.LastFullVisibleCell - LeftCol + FixedCols, Vert.LastFullVisibleCell - TopRow + FixedRows) + 3;
      PointsList := StackAlloc(MaxStroke * sizeof(TPoint) * 2);
      StrokeList := StackAlloc(MaxStroke * sizeof(Integer));
      FillDWord(StrokeList^, MaxStroke, 2);

      LineColor := clBtnFace;
      if Color = clBtnFace then LineColor := clBtnShadow;
      FixedLineColor := clBlack;

      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, 0, [0, 0, Horz.FixedBoundary, Vert.FixedBoundary],
        FixedLineColor, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        LeftCol, 0, [Horz.FixedBoundary, 0, Horz.GridBoundary,
        Vert.FixedBoundary], FixedLineColor, FixedColor);
      DrawLines(goFixedHorzLine in Options, goFixedVertLine in Options,
        0, TopRow, [0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridBoundary],
        FixedLineColor, FixedColor);
      DrawLines(goHorzLine in Options, True, LeftCol,
        TopRow, [Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary],
        LineColor, Color);

      StackFree(StrokeList);
      StackFree(PointsList);
    end;
    // Draw the cells in the four areas
    Sel := Selection;
    FrameFlags1 := 0;
    FrameFlags2 := 0;
    if goFixedVertLine in Options then
    begin
      FrameFlags1 := BF_RIGHT;
      FrameFlags2 := BF_LEFT;
    end;
    if goFixedHorzLine in Options then
    begin
      FrameFlags1 := FrameFlags1 or BF_BOTTOM;
      FrameFlags2 := FrameFlags2 or BF_TOP;
    end;
    DrawCells(0, 0, 0, 0, Horz.FixedBoundary, Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, 0, Horz.FixedBoundary - FColOffset, 0, Horz.GridBoundary, Vert.FixedBoundary, FixedColor, [gdFixed]);
    DrawCells(0, TopRow, 0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridBoundary, FixedColor, [gdFixed]);
    DrawCells(LeftCol, TopRow, Horz.FixedBoundary - FColOffset, Vert.FixedBoundary, Horz.GridBoundary, Vert.GridBoundary, Color, []);

    // Закрашиваем область не занимаемую ячейками
    if Horz.GridBoundary < Horz.GridExtent then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(Horz.GridBoundary, 0, Horz.GridExtent, Vert.GridBoundary));
    end;
    if Vert.GridBoundary < Vert.GridExtent then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(Rect(0, Vert.GridBoundary, Horz.GridExtent, Vert.GridExtent));
    end;
  end;
  if ((Row = FInplaceRow) and CanEditShow) and (goAlwaysShowEditor in Options) and (Self.Focused)
    then FInplaceEdit.SetFocus;
end;

function TRxCustomRecGrid.CalcCoordFromPoint(X, Y: Integer;
  const DrawInfo: TGridDrawInfo): TGridCoord;

  function DoCalc(const AxisInfo: TGridAxisDrawInfo; N: Integer): Integer;
  var
    I, Start, Stop: Longint;
    Line: Integer;
  begin
    with AxisInfo do
    begin
      if N < FixedBoundary then
      begin
        Start := 0;
        Stop := FixedCellCount - 1;
        Line := 0;
      end
      else
      begin
        Start := FirstGridCell;
        Stop := GridCellCount - 1;
        Line := FixedBoundary;
      end;
      Result := -1;
      for I := Start to Stop do
      begin
        Inc(Line, GetExtent(I) + EffectiveLineWidth);
        if N < Line then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
  end;

begin
  Result.X := DoCalc(DrawInfo.Horz, X);
  Result.Y := DoCalc(DrawInfo.Vert, Y);
end;

procedure TRxCustomRecGrid.CalcDrawInfo(var DrawInfo: TGridDrawInfo);
begin
  CalcDrawInfoXY(DrawInfo, ClientWidth, ClientHeight);
end;

procedure TRxCustomRecGrid.CalcDrawInfoXY(var DrawInfo: TGridDrawInfo;
  UseWidth, UseHeight: Integer);

  procedure CalcAxis(var AxisInfo: TGridAxisDrawInfo; UseExtent: Integer);
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      GridExtent := UseExtent;
      GridBoundary := FixedBoundary;
      FullVisBoundary := FixedBoundary;
      LastFullVisibleCell := FirstGridCell;
      for I := FirstGridCell to GridCellCount - 1 do
      begin
        Inc(GridBoundary, GetExtent(I) + EffectiveLineWidth);
        if GridBoundary > GridExtent + EffectiveLineWidth then
        begin
          GridBoundary := GridExtent;
          Break;
        end;
        LastFullVisibleCell := I;
        FullVisBoundary := GridBoundary;
      end;
    end;
  end;

begin
  CalcFixedInfo(DrawInfo);
  CalcAxis(DrawInfo.Horz, UseWidth);
  CalcAxis(DrawInfo.Vert, UseHeight);
end;

procedure TRxCustomRecGrid.CalcFixedInfo(var DrawInfo: TGridDrawInfo);

  procedure CalcFixedAxis(var Axis: TGridAxisDrawInfo; LineOptions: TGridOptions;
    FixedCount, FirstCell, CellCount: Integer; GetExtentFunc: TGetExtentsFunc);
  var
    I: Integer;
  begin
    with Axis do
    begin
      if LineOptions * Options = [] then
        EffectiveLineWidth := 0
      else
        EffectiveLineWidth := GridLineWidth;

      FixedBoundary := 0;
      for I := 0 to FixedCount - 1 do
        Inc(FixedBoundary, GetExtentFunc(I) + EffectiveLineWidth);

      FixedCellCount := FixedCount;
      FirstGridCell := FirstCell;
      GridCellCount := CellCount;
      GetExtent := GetExtentFunc;
    end;
  end;

begin
  CalcFixedAxis(DrawInfo.Horz, [goFixedVertLine], FixedCols,
    LeftCol, ColCount, GetColWidths);
  CalcFixedAxis(DrawInfo.Vert, [goFixedHorzLine, goHorzLine], FixedRows,
    TopRow, RowCount, GetRowHeights);
end;

{ Calculates the TopLeft that will put the given Coord in view }

function TRxCustomRecGrid.CalcMaxTopLeft(const Coord: TGridCoord;
  const DrawInfo: TGridDrawInfo): TGridCoord;

  function CalcMaxCell(const Axis: TGridAxisDrawInfo; Start: Integer): Integer;
  var
    Line: Integer;
    I: Longint;
  begin
    Result := Start;
    with Axis do
    begin
      Line := GridExtent + EffectiveLineWidth;
      for I := Start downto FixedCellCount do
      begin
        Dec(Line, GetExtent(I));
        Dec(Line, EffectiveLineWidth);
        if Line < FixedBoundary then Break;
        Result := I;
      end;
    end;
  end;

begin
  Result.X := CalcMaxCell(DrawInfo.Horz, Coord.X);
  Result.Y := CalcMaxCell(DrawInfo.Vert, Coord.Y);
end;

procedure TRxCustomRecGrid.CalcSizingState(X, Y: Integer; var State: TGridState;
  var Index: Longint; var SizingPos, SizingOfs: Integer;
  var FixedInfo: TGridDrawInfo);

  procedure CalcAxisState(const AxisInfo: TGridAxisDrawInfo; Pos: Integer;
    NewState: TGridState);
  var
    Line, Back, Range: Integer;
  begin
    with AxisInfo do
    begin
      Range := EffectiveLineWidth;
      Back := 0;
      if Range < 7 then
      begin
        Range := 7;
        Back := (Range - EffectiveLineWidth) shr 1;
      end;

      Line := GetExtent(0);

      if (Pos >= Line - Back) and (Pos <= Line - Back + Range) then
      begin
        State := NewState;
        SizingPos := Line;
        SizingOfs := Line - Pos;
        Index := 0;
      end;
    end;
  end;

var
  EffectiveOptions: TGridOptions;
begin
  State := gsNormal;
  Index := -1;
  EffectiveOptions := Options;

  if goColSizing in Options then
    with FixedInfo do
    begin
      Vert.GridExtent := ClientHeight;
      Horz.GridExtent := ClientWidth;
      CalcAxisState(Horz, X, gsColSizing);
    end;
end;

procedure TRxCustomRecGrid.ChangeSize(NewColCount, NewRowCount: Longint);
var
  OldColCount, OldRowCount: Longint;
  OldDrawInfo: TGridDrawInfo;

  procedure MinRedraw(const OldInfo, NewInfo: TGridAxisDrawInfo; Axis: Integer);
  var
    R: TRect;
    First: Integer;
  begin
    if (OldInfo.LastFullVisibleCell = NewInfo.LastFullVisibleCell) then Exit;
    First := Min(OldInfo.LastFullVisibleCell, NewInfo.LastFullVisibleCell);
// Get the rectangle around the leftmost or topmost cell in the target range.
    R := CellRect(First and not Axis, First and Axis);
    R.Bottom := Height;
    R.Right := Width;
    Windows.InvalidateRect(Handle, @R, False);
  end;

  procedure DoChange;
  var
    Coord: TGridCoord;
    NewDrawInfo: TGridDrawInfo;
  begin
    if FColWidths <> nil then
    begin
      UpdateExtents(FColWidths, ColCount, DefaultColWidth);
      UpdateExtents(FTabStops, ColCount, Integer(True));
    end;
    Coord := FCurrent;
    if Row >= RowCount then Coord.Y := RowCount - 1;
    if Col >= ColCount then Coord.X := ColCount - 1;
    if (FCurrent.X <> Coord.X) or (FCurrent.Y <> Coord.Y) then
      MoveCurrent(Coord.X, Coord.Y, True, True);
    if (FAnchor.X <> Coord.X) or (FAnchor.Y <> Coord.Y) then
      MoveAnchor(Coord);
    if virtualView or
      (LeftCol <> OldDrawInfo.Horz.FirstGridCell) or
      (TopRow <> OldDrawInfo.Vert.FirstGridCell)
    then
      Invalidate
    else
      if HandleAllocated then
      begin
        CalcDrawInfo(NewDrawInfo);
        MinRedraw(OldDrawInfo.Horz, NewDrawInfo.Horz, 0);
        MinRedraw(OldDrawInfo.Vert, NewDrawInfo.Vert, -1);
      end;
    UpdateScrollRange;
    SizeChanged(OldColCount, OldRowCount);
  end;

begin
  if HandleAllocated then
    CalcDrawInfo(OldDrawInfo);
  OldColCount := FColCount;
  OldRowCount := FRowCount;
  FColCount := NewColCount;
  FRowCount := NewRowCount;
  if FixedCols > NewColCount then FFixedCols := NewColCount - 1;
  if FixedRows > NewRowCount then FFixedRows := NewRowCount - 1;
  try
    DoChange;
  except
    { Could not change size so try to clean up by setting the size back }
    FColCount := OldColCount;
    FRowCount := OldRowCount;
    DoChange;
    Invalidate;
    raise;
  end;
end;

{ Will move TopLeft so that Coord is in view }

procedure TRxCustomRecGrid.ClampInView(const Coord: TGridCoord);
var
  DrawInfo: TGridDrawInfo;
  MaxTopLeft: TGridCoord;
  OldTopLeft: TGridCoord;
begin
  if not HandleAllocated then Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo, Coord do
  begin
    if (X > Horz.LastFullVisibleCell) or
      (Y > Vert.LastFullVisibleCell) or (X < LeftCol) or (Y < TopRow)
    then
    begin
      OldTopLeft := FTopLeft;
      MaxTopLeft := CalcMaxTopLeft(Coord, DrawInfo);
      Update;
      if X < LeftCol then FTopLeft.X := X
      else
        if X > Horz.LastFullVisibleCell then
          FTopLeft.X := MaxTopLeft.X;
      if Y < TopRow then FTopLeft.Y := Y
      else
        if Y > Vert.LastFullVisibleCell then
          FTopLeft.Y := MaxTopLeft.Y;
      TopLeftMoved(OldTopLeft);
    end;
  end;
end;

procedure TRxCustomRecGrid.DrawSizingLine(const DrawInfo: TGridDrawInfo);
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with Canvas, DrawInfo do
    begin
      OldPen.Assign(Pen);
      Pen.Style := psDot;
      Pen.Mode := pmXor;
      Pen.Width := 1;
      try
        MoveTo(FSizingPos, 0);
        LineTo(FSizingPos, Vert.GridBoundary);
      finally
        Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TRxCustomRecGrid.DrawMove;
var
  OldPen: TPen;
  Pos: Integer;
  R: TRect;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Width := 5;
        R := CellRect(FMovePos, 0);
        if FMovePos > FMoveIndex then
          Pos := R.Right
        else
          Pos := R.Left;
        MoveTo(Pos, 0);
        LineTo(Pos, ClientHeight);
      finally
        Canvas.Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TRxCustomRecGrid.FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
begin
  MoveCurrent(ACol, ARow, MoveAnchor, True);
  UpdateEdit;
  Click;
end;

procedure TRxCustomRecGrid.GridRectToScreenRect(GridRect: TGridRect;
  var ScreenRect: TRect; IncludeLine: Boolean);

  function LinePos(const AxisInfo: TGridAxisDrawInfo; Line: Integer): Integer;
  var Start, I: Longint;
  begin
    with AxisInfo do
    begin
      Result := 0;
      if Line < FixedCellCount then
        Start := 0
      else
      begin
        if Line >= FirstGridCell then
          Result := FixedBoundary;
        Start := FirstGridCell;
      end;
      for I := Start to Line - 1 do
      begin
        Inc(Result, GetExtent(I) + EffectiveLineWidth);
        if Result > GridExtent then
        begin
          Result := 0;
          Exit;
        end;
      end;
    end;
  end;

  function CalcAxis(const AxisInfo: TGridAxisDrawInfo; GridRectMin, GridRectMax: Integer;
    var ScreenRectMin, ScreenRectMax: Integer): Boolean;
  begin
    Result := False;
    with AxisInfo do
    begin
      if (GridRectMin >= FixedCellCount) and (GridRectMin < FirstGridCell) then
        if GridRectMax < FirstGridCell then
        begin
          FillChar(ScreenRect, SizeOf(ScreenRect), 0); { erase partial results }
          Exit;
        end
        else
          GridRectMin := FirstGridCell;
      if GridRectMax > LastFullVisibleCell then
      begin
        GridRectMax := LastFullVisibleCell;
        if GridRectMax < GridCellCount - 1 then Inc(GridRectMax);
        if LinePos(AxisInfo, GridRectMax) = 0 then Dec(GridRectMax);
      end;
      ScreenRectMin := LinePos(AxisInfo, GridRectMin);
      ScreenRectMax := LinePos(AxisInfo, GridRectMax);
      if ScreenRectMax = 0 then
        ScreenRectMax := ScreenRectMin + GetExtent(GridRectMin)
      else Inc(ScreenRectMax, GetExtent(GridRectMax));
      if ScreenRectMax > GridExtent then
        ScreenRectMax := GridExtent;
      if IncludeLine then
        Inc(ScreenRectMax, EffectiveLineWidth);
    end;
    Result := True;
  end;

var
  DrawInfo: TGridDrawInfo;
begin
  FillChar(ScreenRect, SizeOf(ScreenRect), 0);
  if (GridRect.Left > GridRect.Right) or (GridRect.Top > GridRect.Bottom) then
    Exit;
  CalcDrawInfo(DrawInfo);
  with DrawInfo do
  begin
    if GridRect.Left > Horz.LastFullVisibleCell + 1 then Exit;
    if GridRect.Top > Vert.LastFullVisibleCell + 1 then Exit;
    if CalcAxis(Horz, GridRect.Left, GridRect.Right, ScreenRect.Left, ScreenRect.Right) then
      CalcAxis(Vert, GridRect.Top, GridRect.Bottom, ScreenRect.Top, ScreenRect.Bottom);
  end;
end;

procedure TRxCustomRecGrid.Initialize;
begin
  FTopLeft.X := FixedCols;
  FTopLeft.Y := FixedRows;
  ColEnter;
  FCurrent := FTopLeft;
  ColEnter;
  FAnchor := FCurrent;
end;

procedure TRxCustomRecGrid.InvalidateData;
var
  Rect: TGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := 0;
  Rect.Left := 1;
  Rect.Bottom := VisibleRowCount + 1;
  Rect.Right := 1;
  InvalidateRect(Rect);
end;

procedure TRxCustomRecGrid.InvalidateRow(ARow: Longint);
var
  Rect: TGridRect;
begin
  if not HandleAllocated then Exit;
  Rect.Top := ARow;
  Rect.Left := 0;
  Rect.Bottom := ARow;
  Rect.Right := VisibleColCount + 1;
  InvalidateRect(Rect);
end;

procedure TRxCustomRecGrid.Invalidate;
begin
  inherited Invalidate;
end;

procedure TRxCustomRecGrid.InvalidateRect(ARect: TGridRect);
var
  InvalidRect: TRect;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(ARect, InvalidRect, True);
  Windows.InvalidateRect(Handle, @InvalidRect, False);
end;

procedure TRxCustomRecGrid.ModifyScrollBar(ScrollBar, ScrollCode, Pos: Cardinal);
var
  NewTopLeft, MaxTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;

  function Min: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := FixedCols
    else Result := FixedRows;
  end;

  function Max: Longint;
  begin
    if ScrollBar = SB_HORZ then Result := MaxTopLeft.X
    else Result := MaxTopLeft.Y;
  end;

  function PageUp: Longint;
  var
    MaxTopLeft: TGridCoord;
  begin
    MaxTopLeft := CalcMaxTopLeft(FTopLeft, DrawInfo);
    if ScrollBar = SB_HORZ then
      Result := FTopLeft.X - MaxTopLeft.X
    else
      Result := FTopLeft.Y - MaxTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function PageDown: Longint;
  var
    DrawInfo: TGridDrawInfo;
  begin
    CalcDrawInfo(DrawInfo);
    with DrawInfo do
      if ScrollBar = SB_HORZ then
        Result := Horz.LastFullVisibleCell - FTopLeft.X
      else
        Result := Vert.LastFullVisibleCell - FTopLeft.Y;
    if Result < 1 then Result := 1;
  end;

  function CalcScrollBar(Value: Longint): Longint;
  begin
    Result := Value;
    case ScrollCode of
      SB_LINEUP:
        Result := Value - 1;
      SB_LINEDOWN:
        Result := Value + 1;
      SB_PAGEUP:
        Result := Value - PageUp;
      SB_PAGEDOWN:
        Result := Value + PageDown;
      SB_THUMBPOSITION, SB_THUMBTRACK:
        if (goThumbTracking in Options) or (ScrollCode = SB_THUMBPOSITION) then
          Result := Min + LongMulDiv(Pos, Max - Min, MaxShortInt);
      SB_BOTTOM:
        Result := Min;
      SB_TOP:
        Result := Min;
    end;
  end;

  procedure ModifyPixelScrollBar(Code, Pos: Cardinal);
  var
    NewOffset: Integer;
    OldOffset: Integer;
    R: TGridRect;
    GridSpace, ColWidth: Integer;
  begin
    NewOffset := FColOffset;
    ColWidth := ColWidths[DrawInfo.Horz.FirstGridCell];
    GridSpace := ClientWidth - DrawInfo.Horz.FixedBoundary;
    case Code of
      SB_LINEUP: Dec(NewOffset, Canvas.TextWidth('0'));
      SB_LINEDOWN: Inc(NewOffset, Canvas.TextWidth('0'));
      SB_PAGEUP: Dec(NewOffset, GridSpace);
      SB_PAGEDOWN: Inc(NewOffset, GridSpace);
      SB_THUMBPOSITION: NewOffset := Pos;
      SB_THUMBTRACK: if goThumbTracking in Options then NewOffset := Pos;
      SB_BOTTOM: NewOffset := 0;
      SB_TOP: NewOffset := ColWidth - GridSpace;
    end;
    if NewOffset < 0 then
      NewOffset := 0
    else if NewOffset >= ColWidth - GridSpace then
      NewOffset := ColWidth - GridSpace;
    if NewOffset <> FColOffset then
    begin
      OldOffset := FColOffset;
      FColOffset := NewOffset;
      ScrollData(OldOffset - NewOffset, 0);
      FillChar(R, SizeOf(R), 0);
      R.Bottom := FixedRows;
      InvalidateRect(R);
      Update;
      UpdateScrollPos;
    end;
  end;

begin
  if Visible and CanFocus and TabStop and not (csDesigning in ComponentState) then
    SetFocus;
  CalcDrawInfo(DrawInfo);
  if (ScrollBar = SB_HORZ) and (ColCount = 1) then
  begin
    ModifyPixelScrollBar(ScrollCode, Pos);
    Exit;
  end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  NewTopLeft := FTopLeft;
  if ScrollBar = SB_HORZ then NewTopLeft.X := CalcScrollBar(NewTopLeft.X)
  else NewTopLeft.Y := CalcScrollBar(NewTopLeft.Y);
  if NewTopLeft.X < FixedCols then NewTopLeft.X := FixedCols
  else if NewTopLeft.X > MaxTopLeft.X then NewTopLeft.X := MaxTopLeft.X;
  if NewTopLeft.Y < FixedRows then NewTopLeft.Y := FixedRows
  else if NewTopLeft.Y > MaxTopLeft.Y then NewTopLeft.Y := MaxTopLeft.Y;
  if (NewTopLeft.X <> FTopLeft.X) or (NewTopLeft.Y <> FTopLeft.Y) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
end;

procedure TRxCustomRecGrid.MoveAnchor(const NewAnchor: TGridCoord);
var
  OldSel: TGridRect;
begin
  if [goEditing] * Options = [] then
  begin
    OldSel := Selection;
    FAnchor := NewAnchor;
    ClampInView(NewAnchor);
    SelectionMoved(OldSel);
  end
  else MoveCurrent(NewAnchor.X, NewAnchor.Y, True, True);
end;

procedure TRxCustomRecGrid.ColEnter;
begin
end;

procedure TRxCustomRecGrid.ColExit;
begin
end;

procedure TRxCustomRecGrid.MoveCurrent(ACol, ARow: Longint; MoveAnchor,
  Show: Boolean);
var
  OldSel: TGridRect;
  OldCurrent: TGridCoord;
begin
  if (ACol < 0) or (ARow < 0) or (ACol >= ColCount) or (ARow >= RowCount) then
    InvalidOp(SIndexOutOfRange);

  if FCurrent.Y <> ARow then
  begin
    ColExit;
  end;

  if SelectCell(ACol, ARow) then
  begin
    OldSel := Selection;
    OldCurrent := FCurrent;

    FCurrent.X := ACol;
    FCurrent.Y := ARow;

    if OldCurrent.Y <> ARow then ColEnter;

    if not (goAlwaysShowEditor in Options) then HideEditor;
    if MoveAnchor then
    begin
      FAnchor := FCurrent;
    end;
    if Show then ClampInView(FCurrent);
    SelectionMoved(OldSel);
    with OldCurrent do InvalidateRow(Y);
    with FCurrent do InvalidateRow(ARow);
  end;
end;

procedure TRxCustomRecGrid.MoveTopLeft(ALeft, ATop: Longint);
var
  OldTopLeft: TGridCoord;
begin
  if (ALeft = FTopLeft.X) and (ATop = FTopLeft.Y) then Exit;
  Update;
  OldTopLeft := FTopLeft;
  FTopLeft.X := ALeft;
  FTopLeft.Y := ATop;
  TopLeftMoved(OldTopLeft);
end;

procedure TRxCustomRecGrid.ResizeCol(Index: Longint; OldSize, NewSize: Integer);
begin
  Invalidate;
end;

procedure TRxCustomRecGrid.SelectionMoved(const OldSel: TGridRect);
var
  OldRect, NewRect: TRect;
  AXorRects: TXorRects;
  I: Integer;
begin
  if not HandleAllocated then Exit;
  GridRectToScreenRect(OldSel, OldRect, True);
  GridRectToScreenRect(Selection, NewRect, True);
  XorRects(OldRect, NewRect, AXorRects);
  for I := Low(AXorRects) to High(AXorRects) do
    Windows.InvalidateRect(Handle, @AXorRects[I], False);
end;

procedure TRxCustomRecGrid.ScrollDataInfo(DX, DY: Integer;
  var DrawInfo: TGridDrawInfo);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  with DrawInfo do
  begin
    ScrollFlags := SW_INVALIDATE;
    if not DefaultDrawing then
      ScrollFlags := ScrollFlags or SW_ERASE;
    { Scroll the area }
    if DY = 0 then
    begin
      { Scroll both the column titles and data area at the same time }
      ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.GridExtent);
      ScrollWindowEx(Handle, DX, 0, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);
    end
    else
      if DX = 0 then
      begin
        { Scroll both the row titles and data area at the same time }
        ScrollArea := Rect(0, Vert.FixedBoundary, Horz.GridExtent, Vert.GridExtent);
        ScrollWindowEx(Handle, 0, DY, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);
      end
      else
      begin
        { Scroll titles and data area separately }
        { Column titles }
        ScrollArea := Rect(Horz.FixedBoundary, 0, Horz.GridExtent, Vert.FixedBoundary);
        ScrollWindowEx(Handle, DX, 0, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);
        { Row titles }
        ScrollArea := Rect(0, Vert.FixedBoundary, Horz.FixedBoundary, Vert.GridExtent);
        ScrollWindowEx(Handle, 0, DY, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);
        { Data area }
        ScrollArea := Rect(Horz.FixedBoundary, Vert.FixedBoundary, Horz.GridExtent,
          Vert.GridExtent);
        ScrollWindowEx(Handle, DX, DY, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);
      end;
  end;
end;

procedure TRxCustomRecGrid.ScrollData(DX, DY: Integer);
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  ScrollDataInfo(DX, DY, DrawInfo);
end;

procedure TRxCustomRecGrid.TopLeftMoved(const OldTopLeft: TGridCoord);

  function CalcScroll(const AxisInfo: TGridAxisDrawInfo;
    OldPos, CurrentPos: Integer; var Amount: Longint): Boolean;
  var
    Start, Stop: Longint;
    I: Longint;
  begin
    Result := False;
    with AxisInfo do
    begin
      if OldPos < CurrentPos then
      begin
        Start := OldPos;
        Stop := CurrentPos;
      end
      else
      begin
        Start := CurrentPos;
        Stop := OldPos;
      end;
      Amount := 0;
      for I := Start to Stop - 1 do
      begin
        Inc(Amount, GetExtent(I) + EffectiveLineWidth);
        if Amount > (GridBoundary - FixedBoundary) then
        begin
          { Scroll amount too big, redraw the whole thing }
          Invalidate;
          Exit;
        end;
      end;
      if OldPos < CurrentPos then Amount := -Amount;
    end;
    Result := True;
  end;

var
  DrawInfo: TGridDrawInfo;
  Delta: TGridCoord;
begin
  UpdateScrollPos;
  CalcDrawInfo(DrawInfo);
  if CalcScroll(DrawInfo.Horz, OldTopLeft.X, FTopLeft.X, Delta.X) and
    CalcScroll(DrawInfo.Vert, OldTopLeft.Y, FTopLeft.Y, Delta.Y) then
    ScrollDataInfo(Delta.X, Delta.Y, DrawInfo);
  TopLeftChanged;
end;

procedure TRxCustomRecGrid.UpdateScrollPos;
var
  DrawInfo: TGridDrawInfo;
  MaxTopLeft: TGridCoord;

  procedure SetScroll(Code: Word; Value: Integer);
  begin
    if GetScrollPos(Handle, Code) <> Value then
      SetScrollPos(Handle, Code, Value, True);
  end;

begin
  if (not HandleAllocated) or (ScrollBars = ssNone) then Exit;
  CalcDrawInfo(DrawInfo);
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  if ScrollBars in [ssVertical, ssBoth] then
    SetScroll(SB_VERT, LongMulDiv(FTopLeft.Y - FixedRows, MaxShortInt,
      MaxTopLeft.Y - FixedRows));
end;

procedure TRxCustomRecGrid.UpdateScrollRange;
var
  MaxTopLeft, OldTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;
  OldScrollBars: TScrollStyle;
  Updated: Boolean;

  procedure DoUpdate;
  begin
    if not Updated then
    begin
      Update;
      Updated := True;
    end;
  end;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (ScrollBars = ssBoth) or
      ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
    begin
      GetScrollRange(Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

  procedure CalcSizeInfo;
  begin
    CalcDrawInfoXY(DrawInfo, DrawInfo.Horz.GridExtent, DrawInfo.Vert.GridExtent);
    MaxTopLeft.X := ColCount - 1;
    MaxTopLeft.Y := RowCount - 1;
    MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  end;

  procedure SetAxisRange(var Max, Old, Current: Longint; Code: Word;
    Fixeds: Integer);
  begin
    CalcSizeInfo;
    if Fixeds < Max then
      SetScrollRange(Handle, Code, 0, MaxShortInt, True)
    else
      SetScrollRange(Handle, Code, 0, 0, True);
    if Old > Max then
    begin
      DoUpdate;
      Current := Max;
    end;
  end;

  procedure SetVertRange;
  begin
    if OldScrollBars in [ssVertical, ssBoth] then
      SetAxisRange(MaxTopLeft.Y, OldTopLeft.Y, FTopLeft.Y, SB_VERT, FixedRows);
  end;

begin
  if (ScrollBars = ssNone) or not HandleAllocated then Exit;
  with DrawInfo do
  begin
    Horz.GridExtent := ClientWidth;
    Vert.GridExtent := ClientHeight;
    { Ignore scroll bars for initial calculation }
    if ScrollBarVisible(SB_HORZ) then
      Inc(Vert.GridExtent, GetSystemMetrics(SM_CYHSCROLL));
    if ScrollBarVisible(SB_VERT) then
      Inc(Horz.GridExtent, GetSystemMetrics(SM_CXVSCROLL));
  end;

  { Reset the ColumnWidths }

  OldTopLeft := FTopLeft;
  { Temporarily mark us as not having scroll bars to avoid recursion }
  OldScrollBars := FScrollBars;
  FScrollBars := ssNone;
  Updated := False;
  try
    { Update scrollbars }
    DrawInfo.Vert.GridExtent := ClientHeight;
    SetVertRange;
    if DrawInfo.Horz.GridExtent <> ClientWidth then
      DrawInfo.Horz.GridExtent := ClientWidth;
    // Не надо !!!!!!!!!!!
    // if ClientWidth - ColWidths[0] < 4 then ColWidths[0] := ClientWidth Div 2;
    ColWidths[1] := ClientWidth - ColWidths[0] - 1;
  finally
    FScrollBars := OldScrollBars;
  end;
  UpdateScrollPos;
  if (FTopLeft.X <> OldTopLeft.X) or (FTopLeft.Y <> OldTopLeft.Y) then
    TopLeftMoved(OldTopLeft);
end;

function TRxCustomRecGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEdit.Create(Self);
end;

procedure TRxCustomRecGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    if FScrollBars in [ssVertical, ssBoth] then Style := Style or WS_VSCROLL;
    Windowclass.style := CS_DBLCLKS;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TRxCustomRecGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewTopLeft, NewCurrent, MaxTopLeft: TGridCoord;
  DrawInfo: TGridDrawInfo;
  PageWidth, PageHeight: Integer;

  procedure CalcPageExtents;
  begin
    CalcDrawInfo(DrawInfo);
    PageWidth := DrawInfo.Horz.LastFullVisibleCell - LeftCol;
    if PageWidth < 1 then PageWidth := 1;
    PageHeight := DrawInfo.Vert.LastFullVisibleCell - TopRow;
    if PageHeight < 1 then PageHeight := 1;
  end;

  procedure Restrict(var Coord: TGridCoord; MinX, MinY, MaxX, MaxY: Longint);
  begin
    with Coord do
    begin
      if X > MaxX then X := MaxX
      else if X < MinX then X := MinX;
      if Y > MaxY then Y := MaxY
      else if Y < MinY then Y := MinY;
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  if not CanGridAcceptKey(Key, Shift) then Key := 0;
  NewCurrent := FCurrent;
  NewTopLeft := FTopLeft;
  CalcPageExtents;
  if ssCtrl in Shift then
    case Key of
      VK_UP: Dec(NewTopLeft.Y);
      VK_DOWN: Inc(NewTopLeft.Y);
      VK_LEFT:
        begin
          Dec(NewCurrent.X, PageWidth);
          Dec(NewTopLeft.X, PageWidth);
        end;
      VK_RIGHT:
        begin
          Inc(NewCurrent.X, PageWidth);
          Inc(NewTopLeft.X, PageWidth);
        end;
      VK_PRIOR: NewCurrent.Y := TopRow;
      VK_NEXT: NewCurrent.Y := DrawInfo.Vert.LastFullVisibleCell;
      VK_HOME:
        begin
          NewCurrent.X := FixedCols;
          NewCurrent.Y := FixedRows;
        end;
      VK_END:
        begin
          NewCurrent.X := ColCount - 1;
          NewCurrent.Y := RowCount - 1;
        end;
    end
  else
    case Key of
      VK_UP: Dec(NewCurrent.Y);
      VK_DOWN: Inc(NewCurrent.Y);
      VK_LEFT: Dec(NewCurrent.Y);
      VK_RIGHT: Inc(NewCurrent.Y);
      VK_NEXT:
        begin
          Inc(NewCurrent.Y, PageHeight);
          Inc(NewTopLeft.Y, PageHeight);
        end;
      VK_PRIOR:
        begin
          Dec(NewCurrent.Y, PageHeight);
          Dec(NewTopLeft.Y, PageHeight);
        end;
      VK_HOME: NewCurrent.Y := FixedRows;
      VK_END: NewCurrent.Y := RowCount - 1;
      VK_TAB:
        if not (ssAlt in Shift) then
          repeat
            if ssShift in Shift then
            begin
              Dec(NewCurrent.X);
              if NewCurrent.X < FixedCols then
              begin
                NewCurrent.X := ColCount - 1;
                Dec(NewCurrent.Y);
                if NewCurrent.Y < FixedRows then NewCurrent.Y := RowCount - 1;
              end;
              Shift := [];
            end
            else
            begin
              Inc(NewCurrent.X);
              if NewCurrent.X >= ColCount then
              begin
                NewCurrent.X := FixedCols;
                Inc(NewCurrent.Y);
                if NewCurrent.Y >= RowCount then NewCurrent.Y := FixedRows;
              end;
            end;
          until TabStops[NewCurrent.X] or (NewCurrent.X = FCurrent.X);
      VK_F2: EditorMode := True;
    end;
  MaxTopLeft.X := ColCount - 1;
  MaxTopLeft.Y := RowCount - 1;
  MaxTopLeft := CalcMaxTopLeft(MaxTopLeft, DrawInfo);
  Restrict(NewTopLeft, FixedCols, FixedRows, MaxTopLeft.X, MaxTopLeft.Y);
  if (NewTopLeft.X <> LeftCol) or (NewTopLeft.Y <> TopRow) then
    MoveTopLeft(NewTopLeft.X, NewTopLeft.Y);
  Restrict(NewCurrent, FixedCols, FixedRows, ColCount - 1, RowCount - 1);
  if (NewCurrent.X <> Col) or (NewCurrent.Y <> Row) then
    FocusCell(NewCurrent.X, NewCurrent.Y, not (ssShift in Shift));
end;

procedure TRxCustomRecGrid.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if not (goAlwaysShowEditor in Options) and (Key = #13) then
  begin
    if FEditorMode then
      HideEditor
    else
      ShowEditor;
    Key := #0;
  end;
end;

procedure TRxCustomRecGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CellHit: TGridCoord;
  DrawInfo: TGridDrawInfo;
  MoveDrawn: Boolean;
begin
  MoveDrawn := False;
  HideEdit;
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    SetFocus;
    if not IsActiveControl then
    begin
      MouseCapture := False;
      Exit;
    end;
  end;
  if (Button = mbLeft) and (ssDouble in Shift) then
    DblClick
  else if Button = mbLeft then
  begin
    CalcDrawInfo(DrawInfo);
    { Check Grid sizing }
    CalcSizingState(X, Y, FGridState, FSizingIndex, FSizingPos, FSizingOfs,
      DrawInfo);
    if FGridState <> gsNormal then
    begin
      DrawSizingLine(DrawInfo);
      Exit;
    end;
    CellHit := CalcCoordFromPoint(X, Y, DrawInfo);
    if (CellHit.Y >= FixedRows) then
    begin
      if goEditing in Options then
      begin
        if (CellHit.Y = FCurrent.Y) then
          ShowEditor
        else
          FocusCell(1, CellHit.Y, not (ssShift in Shift));
      end;
    end;
  end;
  try
    inherited MouseDown(Button, Shift, X, Y);
  except
    if MoveDrawn then DrawMove;
  end;
end;

procedure TRxCustomRecGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  case FGridState of
    gsColSizing:
      begin
        DrawSizingLine(DrawInfo); { XOR it out }
        FSizingPos := X + FSizingOfs;
        DrawSizingLine(DrawInfo); { XOR it back in }
      end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRxCustomRecGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DrawInfo: TGridDrawInfo;
  NewSize: Integer;

  function ResizeLine(const AxisInfo: TGridAxisDrawInfo): Integer;
  var
    I: Integer;
  begin
    with AxisInfo do
    begin
      Result := FixedBoundary;
      for I := FirstGridCell to FSizingIndex - 1 do
        Inc(Result, GetExtent(I) + EffectiveLineWidth);
      Result := FSizingPos - Result;
    end;
  end;

begin
  try
    if FGridState = gsColSizing then
    begin
      FSizingIndex := 0;
      CalcDrawInfo(DrawInfo);
      DrawSizingLine(DrawInfo);
      if FGridState = gsColSizing then
      begin
        NewSize := FSizingPos;

        if NewSize > 1 then
        begin
          ColWidths[1] := ColWidths[1] + (ColWidths[0] - NewSize);
          ColWidths[0] := FSizingPos;
          UpdateDesigner;
        end;
      end;
    end;

    inherited MouseUp(Button, Shift, X, Y);
  finally
    FGridState := gsNormal;
  end;
end;

function TRxCustomRecGrid.GetColWidths(Index: Longint): Integer;
begin
  if (FColWidths = nil) or (Index >= ColCount) then
    Result := DefaultColWidth
  else
    Result := PIntArray(FColWidths)^[Index + 1];
end;

function TRxCustomRecGrid.GetRowHeights(Index: Longint): Integer;
begin
  Result := FDefaultRowHeight;
end;

function TRxCustomRecGrid.GetGridWidth: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.GridBoundary;
end;

function TRxCustomRecGrid.GetGridHeight: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.GridBoundary;
end;

function TRxCustomRecGrid.GetSelection: TGridRect;
begin
  Result := GridRect(FCurrent, FAnchor);
end;

function TRxCustomRecGrid.GetTabStops(Index: Longint): Boolean;
begin
  if FTabStops = nil then Result := True
  else Result := Boolean(PIntArray(FTabStops)^[Index + 1]);
end;

function TRxCustomRecGrid.GetVisibleColCount: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Horz.LastFullVisibleCell - LeftCol + 1;
end;

function TRxCustomRecGrid.GetVisibleRowCount: Integer;
var
  DrawInfo: TGridDrawInfo;
begin
  CalcDrawInfo(DrawInfo);
  Result := DrawInfo.Vert.LastFullVisibleCell - TopRow + 1;
end;

procedure TRxCustomRecGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomRecGrid.SetCol(Value: Longint);
begin
  if Col <> Value then FocusCell(Value, Row, True);
end;

procedure TRxCustomRecGrid.SetColCount(Value: Longint);
begin
  if FColCount <> Value then
  begin
    if Value < 1 then Value := 1;
    if Value <= FixedCols then FixedCols := Value - 1;
    ChangeSize(Value, RowCount);
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetColWidths(Index: Longint; Value: Integer);
begin
  if FColWidths = nil then
    UpdateExtents(FColWidths, ColCount, DefaultColWidth);
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  if Value <> PIntArray(FColWidths)^[Index + 1] then
  begin
    ResizeCol(Index, PIntArray(FColWidths)^[Index + 1], Value);
    PIntArray(FColWidths)^[Index + 1] := Value;
    ColWidthsChanged;
  end;
end;

procedure TRxCustomRecGrid.SetDefaultColWidth(Value: Integer);
begin
  if FColWidths <> nil then UpdateExtents(FColWidths, 0, 0);
  FDefaultColWidth := Value;
  ColWidthsChanged;
  Invalidate;
end;

procedure TRxCustomRecGrid.SetDefaultRowHeight(Value: Integer);
begin
  FDefaultRowHeight := Value;
  UpdateScrollRange;
  UpdateEdit;
  Invalidate;
end;

procedure TRxCustomRecGrid.SetFixedColor(Value: TColor);
begin
  if FFixedColor <> Value then
  begin
    FFixedColor := Value;
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetFixedCols(Value: Integer);
begin
  if FFixedCols <> Value then
  begin
    if Value < 0 then InvalidOp(SIndexOutOfRange);
    if Value >= ColCount then InvalidOp(SFixedColTooBig);
    FFixedCols := Value;
    Initialize;
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetFixedRows(Value: Integer);
begin
  if FFixedRows <> Value then
  begin
    if Value < 0 then InvalidOp(SIndexOutOfRange);
    if Value >= RowCount then InvalidOp(SFixedRowTooBig);
    FFixedRows := Value;
    Initialize;
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetEditorMode(Value: Boolean);
begin
  if not Value
    then HideEditor
  else begin
    ShowEditor;
    if FInplaceEdit <> nil then FInplaceEdit.Deselect;
  end;
end;

procedure TRxCustomRecGrid.SetGridLineWidth(Value: Integer);
begin
  if FGridLineWidth <> Value then
  begin
    FGridLineWidth := Value;
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetLeftCol(Value: Longint);
begin
  if FTopLeft.X <> Value then MoveTopLeft(Value, TopRow);
end;

procedure TRxCustomRecGrid.SetOptions(Value: TGridOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    if not FEditorMode then
      if goAlwaysShowEditor in Value then
        ShowEditor else
        HideEditor;
    Invalidate;
  end;
end;

procedure TRxCustomRecGrid.SetRow(Value: Longint);
begin
  if Row <> Value then FocusCell(Col, Value, True);
end;

procedure TRxCustomRecGrid.SetRowCount(Value: Longint);
begin
  if FRowCount <> Value then
  begin
    if Value < 1 then Value := 1;
    if Value <= FixedRows then FixedRows := Value - 1;
    ChangeSize(ColCount, Value);
  end;
end;

procedure TRxCustomRecGrid.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomRecGrid.SetSelection(Value: TGridRect);
var
  OldSel: TGridRect;
begin
  OldSel := Selection;
  FAnchor := Value.TopLeft;
  FCurrent := Value.BottomRight;
  SelectionMoved(OldSel);
end;

procedure TRxCustomRecGrid.SetTabStops(Index: Longint; Value: Boolean);
begin
  if FTabStops = nil then
    UpdateExtents(FTabStops, ColCount, Integer(True));
  if Index >= ColCount then InvalidOp(SIndexOutOfRange);
  PIntArray(FTabStops)^[Index + 1] := Integer(Value);
end;

procedure TRxCustomRecGrid.SetTopRow(Value: Longint);
begin
  if FTopLeft.Y <> Value then MoveTopLeft(LeftCol, Value);
end;

procedure TRxCustomRecGrid.HideEdit;
begin
  if FInplaceEdit <> nil then
  try
    UpdateText;
  finally
    InvalidateRow(FinplaceRow);
    FInplaceEdit.Hide;
  end;
end;

procedure TRxCustomRecGrid.UpdateEdit;
var
  FocRect: TRect;

  procedure UpdateEditor;
  begin
    FInplaceCol := Col;
    FInplaceRow := Row;
    FInplaceEdit.UpdateContents;
    if FInplaceEdit.MaxLength = -1
      then FCanEditModify := False
    else FCanEditModify := True;
    FInplaceEdit.SelectAll;
  end;

begin
  if CanEditShow then begin
    if FInplaceEdit = nil then begin
      FInplaceEdit := CreateEditor;
      FInplaceEdit.SetGrid(Self);
      FInplaceEdit.Parent := Self;
      UpdateEditor;
    end else begin
      if (Col <> FInplaceCol) or (Row <> FInplaceRow) then begin
        if (FInplaceRow >= 0) and (FInplaceRow < RowCount)
          then InvalidateRow(FInplaceRow);
        HideEdit;
        UpdateEditor;
      end;
    end;
    UpdateEditor;
    if CanEditShow then begin
      FInplaceEdit.Move(CellRect(Col, Row));
      GridRectToScreenRect(GetSelection, FocRect, False);
//      FocRect.Left := 0;
//      FocRect.Top := FocRect.Top - 1;
//      Canvas.DrawFocusRect(FocRect);
    end;
  end;
end;

procedure TRxCustomRecGrid.UpdateText;
begin
  if (FInplaceRow <> -1) and (FInplaceRow < RowCount) then
    SetEditText(FInplaceCol, FInplaceRow, FInplaceEdit.Text);
end;

procedure TRxCustomRecGrid.WMChar(var Msg: TWMChar);
begin
  if (goEditing in Options) and ((Char(Msg.CharCode) = ^H) or (Char(Msg.CharCode) >= ' ')) then 
    ShowEditorChar(Char(Msg.CharCode))
  else inherited;
end;

procedure TRxCustomRecGrid.WMCommand(var Message: TWMCommand);
begin
  with Message do
  begin
    if (FInplaceEdit <> nil) and (Ctl = FInplaceEdit.Handle) then
      case NotifyCode of
        EN_CHANGE: UpdateText;
      end;
  end;
end;

procedure TRxCustomRecGrid.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
  if goTabs in Options then Msg.Result := Msg.Result or DLGC_WANTTAB;
  if goEditing in Options then Msg.Result := Msg.Result or DLGC_WANTCHARS;
end;

procedure TRxCustomRecGrid.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  InvalidateRect(Selection);
  if (FInplaceEdit <> nil) and (Msg.FocusedWnd <> FInplaceEdit.Handle)
    then HideEdit;
end;

procedure TRxCustomRecGrid.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if (FInplaceEdit = nil) or (Msg.FocusedWnd <> FInplaceEdit.Handle) then begin
    InvalidateRect(Selection);
    UpdateEdit;
  end;
end;

procedure TRxCustomRecGrid.WMLButtonDown(var Message: TMessage);
begin
  inherited;
  if FInplaceEdit <> nil then FInplaceEdit.FClickTime := GetMessageTime;
end;

procedure TRxCustomRecGrid.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  FHitTest := ScreenToClient(SmallPointToPoint(Msg.Pos));
end;

procedure TRxCustomRecGrid.WMSetCursor(var Msg: TWMSetCursor);
var
  DrawInfo: TGridDrawInfo;
  State: TGridState;
  Index: Longint;
  Pos, Ofs: Integer;
  Cur: HCURSOR;
begin
  Cur := 0;
  with Msg do
  begin
    if HitTest = HTCLIENT then
    begin
      if FGridState = gsNormal then
      begin
        CalcDrawInfo(DrawInfo);
        CalcSizingState(FHitTest.X, FHitTest.Y, State, Index, Pos, Ofs,
          DrawInfo);
      end
      else
        State := FGridState;

      if State = gsColSizing then
        Cur := Screen.Cursors[crHSplit]
    end;
  end;
  if Cur <> 0 then SetCursor(Cur)
  else inherited;
end;

procedure TRxCustomRecGrid.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateScrollRange;
end;

procedure TRxCustomRecGrid.WMVScroll(var Msg: TWMVScroll);
begin
  ModifyScrollBar(SB_VERT, Msg.ScrollCode, Msg.Pos);
end;

procedure TRxCustomRecGrid.WMHScroll(var Msg: TWMHScroll);
begin
  ModifyScrollBar(SB_HORZ, Msg.ScrollCode, Msg.Pos);
end;

procedure TRxCustomRecGrid.CMCancelMode(var Msg: TMessage);
begin
  if Assigned(FInplaceEdit) then FInplaceEdit.WndProc(Msg);
  inherited;
end;

procedure TRxCustomRecGrid.CMFontChanged(var Message: TMessage);
begin
  if FInplaceEdit <> nil then FInplaceEdit.Font := Font;
  inherited;
end;

procedure TRxCustomRecGrid.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  RecreateWnd;
end;

procedure TRxCustomRecGrid.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  Msg.Result := Longint(BOOL(Sizing(Msg.Pos.X, Msg.Pos.Y)));
end;

procedure TRxCustomRecGrid.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (goEditing in Options) and (Char(Msg.CharCode) = #13) then Msg.Result := 1;
end;

procedure TRxCustomRecGrid.TimedScroll(Direction: TGridScrollDirection);
var
  MaxAnchor, NewAnchor: TGridCoord;
begin
  NewAnchor := FAnchor;
  MaxAnchor.X := ColCount - 1;
  MaxAnchor.Y := RowCount - 1;
  if (sdLeft in Direction) and (FAnchor.X > FixedCols) then Dec(NewAnchor.X);
  if (sdRight in Direction) and (FAnchor.X < MaxAnchor.X) then Inc(NewAnchor.X);
  if (sdUp in Direction) and (FAnchor.Y > FixedRows) then Dec(NewAnchor.Y);
  if (sdDown in Direction) and (FAnchor.Y < MaxAnchor.Y) then Inc(NewAnchor.Y);
  if (FAnchor.X <> NewAnchor.X) or (FAnchor.Y <> NewAnchor.Y) then
    MoveAnchor(NewAnchor);
end;

procedure TRxCustomRecGrid.ColWidthsChanged;
begin
  UpdateScrollRange;
  UpdateEdit;
end;

procedure TRxCustomRecGrid.UpdateDesigner;
var
  ParentForm: TCustomForm;
begin
  if (csDesigning in ComponentState) and HandleAllocated and
    not (csUpdating in ComponentState) then
  begin
    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
      ParentForm.Designer.Modified;
  end;
end;

type
  TEditStyle = (esSimple, esEllipsis, esPickList, esDropDown, esDataList);
  TPopupListbox = class;

  TRxRecGridInplaceEdit = class(TInplaceEdit)
  private
    FButtonWidth: Integer;
    FDataList: TDBLookupListBox;
    FPickList: TPopupListbox;
    FActiveList: TWinControl;
    FLookupSource: TDatasource;
    FEditStyle: TEditStyle;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FSavedCancel: TDataSetNotifyEvent;
    procedure UpdateField;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetEditStyle(Value: TEditStyle);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    procedure CatchCancel(Dataset: TDataSet);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    procedure BoundsChanged; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
    procedure DropDown;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
    function GetColumn: TRow;
    property EditStyle: TEditStyle read FEditStyle write SetEditStyle;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property DataList: TDBLookupListBox read FDataList;
    property PickList: TPopupListbox read FPickList;
//
//        procedure SetFocus; override;
//
  public
    procedure CloseUp(Accept: Boolean);
    constructor Create(Owner: TComponent); override;
  end;

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

function GetSpecialText(Field: TField): string;
begin
  case Field.Datatype of
    ftBytes,
      fttypedBinary,
      ftvarBytes: Result := '<Binary>';
    ftBlob: Result := '<Blob>';
    ftMemo: Result := Field.AsString; // '<Memo>';
    ftGraphic: Result := '<Bitmap>';
    ftFmtMemo: Result := '<Formatted>';
    ftParadoxOle,
      ftDBaseOle: Result := '<OLE>';
  else
    Result := '<Неизвестный>';
  end;
end;

function IsSpecialField(Field: TField): Boolean;
begin
  Result := Field.Datatype in [ftUnknown, ftBytes, fttypedBinary, ftvarBytes,
    ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle];
end;

{ TrRecordGrid }

procedure TRxDBRecordGrid.ResyncFields;
var
  i: Integer;
begin
  for i := 0 to Rows.Count - 1 do
    if FDatalink.Active
      then Rows[i].FieldName := Rows[i].FieldName
    else Rows[i].FField := nil;
end;

procedure TRxDBRecordGrid.ShowEditor;
begin
  if Rows[Row].Field <> nil
    then inherited ShowEditor;
end;

procedure TRxDBRecordGrid.Refresh;
begin
  UpdateEdit;
  Invalidate;
end;

function TRxDBRecordGrid.CanEditAcceptKey(Key: Char): Boolean;
begin
  //Rabbit (Типа залепил :)
  if (EditRow >= 0) and (EditRow < Rows.Count) then
    with Rows[EditRow] do
      Result := FDatalink.Active and Assigned(Field) and Field.IsValidChar(Key)
  else Result := False;
end;

procedure TRxDBRecordGrid.RefreshData;
begin
  UpdateEdit;
  InvalidateData;
end;

function TRxDBRecordGrid.CellRect(ACol, ARow: Longint): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

procedure TRxDBRecordGrid.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var
  Coord: TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

function TRxDBRecordGrid.GetEditMask(ACol, ARow: Longint): string;
begin
  Result := Rows[ARow].GetEditMask;
end;

function TRxDBRecordGrid.GetEditText(ACol, ARow: Longint): string;
var VResult: Variant;
begin
  Result := '';
  if ARow < Rows.Count then begin
    with Rows[ARow] do begin
      if Assigned(Field) then begin
        if DataSource.DataSet.Active then begin
          if IsSpecialField(Field)
            then Result := GetSpecialText(Field)
          else Result := DataSource.DataSet.FieldByName(Rows[ARow].FieldName).Text;
        end;
      end else
        if not (VarType(Value) = varEmpty)
          then Result := Value;
      if Assigned(OnGetEditText) then begin
        VResult := Result;
        OnGetEditText(Self, ACol, ARow, PickList, VResult);
        Result := VResult;
      end;
    end;
  end;
end;

procedure TRxDBRecordGrid.SetEditText(ACol, ARow: Longint; const cValue: string);
begin
  if ARow < Rows.Count then begin
    with Rows[ARow] do begin
      if {(Rows[ARow] <> nil) and} Assigned(Field) then begin
        if DataSource.DataSet.Active then begin
          if Assigned(Field) and
            (cValue <> Field.Text)
            and not IsSpecialField(Field)
            and (EditRow = ARow) then
            if not Self.ReadOnly
              and not Rows[ARow].ReadOnly
              and not Field.ReadOnly
              and not (DataSource.DataSet.State in [dsEdit, dsInsert]) then begin
              InUpdating := True;
              DataSource.DataSet.Edit;
              Field.Text := cValue;
            end;
//          if (cValue <> Field.Text) and not IsSpecialField(Field) and (EditRow=ARow) then begin
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//          if (cValue <> Field.AsString) and not IsSpecialField(Field) then begin

//            if not Rows[ARow].ReadOnly and not Field.ReadOnly and not (DataSource.DataSet.State In [dsEdit, dsInsert]) then begin
//            if {not Self.ReadOnly and} not Rows[ARow].ReadOnly and not Field.ReadOnly and not (DataSource.DataSet.State In [dsEdit, dsInsert]) then begin
//              InUpdating := True;
//              DataSource.DataSet.Edit;
//            end;
//            Field.Text := cValue;
//          end;
        end;
      end else
        if not (vartype(Value) = varEmpty)
          then Value := cValue;
      if Assigned(OnSetEditText)
        then OnSetEditText(Self, ARow, PickList, cValue);
    end;
  end;
end;

procedure TRxDBRecordGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if Assigned(FOnTopLeftChanged) then FOnTopLeftChanged(Self);
end;

function TRxDBRecordGrid.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRxDBRecordGrid.SetDataSource(Value: TDataSource);
var
  nCnt: Integer;
  nSub: Integer;

  function CheckDataSet(Value: TDataSource): Boolean;
  begin
    Result := (Value <> nil) and (Value.DataSet <> nil);
  end;

begin
  if Value = FDatalink.Datasource then Exit;
//  if FDataLink.DataSource <> nil then begin
//    FDataLink.CheckBrowseMode;
//    if FDataLink.DataSource.DataSet.State in [dsEdit,dsInsert]
//      then FDataLink.DataSource.DataSet.Post;
//  end;
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
  for nCnt := Rows.Count - 1 downto 0 do begin
    with Rows[nCnt].Nodes do
      for nSub := Count - 1 downto 0 do
        if Assigned(TRow(Items[nSub]).Field) then
          if not CheckDataSet(Value)
            and (Value.DataSet.FindField(TRow(Items[nSub]).Field.FieldName) = nil)
            then Items[nSub].Collection := nil
          else TRow(Items[nSub]).Field := Value.DataSet.FindField(TRow(Items[nSub]).Field.FieldName);
    if Assigned(Rows[nCnt].Field) then
      if not CheckDataSet(Value)
        and (Value.DataSet.FindField(Rows[nCnt].Field.FieldName) = nil)
        then Rows[nCnt].Collection := nil
      else Rows[nCnt].Field := Value.DataSet.FindField(Rows[nCnt].Field.FieldName);
  end;
  RowCount := Rows.Count;
  if Row > RowCount - 1 then Row := RowCount - 1;
  Invalidate;
end;

procedure TRxDBRecordGrid.SetRecGridRows(Value: TRecGridRows);
begin
  Rows.Assign(Value);
end;

function TRxDBRecordGrid.CreateRows: TRecGridRows;
begin
  Result := TRecGridRows.Create(Self, TRow, False {True});
end;

procedure TRxDBRecordGrid.SetIme;
var
  Column: TRow;
begin
  if not SysLocale.Fareast then Exit;
  if FUpdatingEditor then
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
  end
  else
  if Assigned(Rows) then
  begin
    Column := Rows[Row];
    ImeName := FOriginalImeName;
    ImeMode := FOriginalImeMode;
    if cvImeMode in Column.FAssignedValues then
    begin
      ImeName := Column.ImeName;
      ImeMode := Column.ImeMode;
    end;
  end;
  if InplaceEditor <> nil then
  begin
    TRxRecGridInplaceEdit(Self).ImeName := ImeName;
    TRxRecGridInplaceEdit(Self).ImeMode := ImeMode;
  end;
end;

procedure TRxDBRecordGrid.UpdateIme;
begin
  if not SysLocale.Fareast then Exit;
  SetIme;
  if InplaceEditor <> nil then
    TRxRecGridInplaceEdit(Self).SetIme;
end;

procedure TRxDBRecordGrid.WMIMEStartComp(var Message: TMessage);
begin
  inherited;
  FUpdatingEditor := True;
  ShowEditor;
  FUpdatingEditor := False;
end;

function TRxDBRecordGrid.CreateEditor: TInplaceEdit;
begin
  Result := TRxRecGridInplaceEdit.Create(Self);
end;

procedure TRxDBRecordGrid.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  nSub: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if (AComponent is TPopupMenu) then begin
      for I := 0 to Rows.Count - 1 do begin
        if Rows[I].PopupMenu = AComponent
          then Rows[I].PopupMenu := nil;
        if Rows[I].Nodes.Count > 0 then
          for nSub := 0 to Rows[I].Nodes.Count - 1 do
            if TRow(Rows[I].Nodes.Items[nSub]).PopupMenu = AComponent
              then TRow(Rows[I].Nodes[nSub]).PopupMenu := nil;
      end;
    end else
      if (FDataLink <> nil) then
        if (AComponent = DataSource)
          then DataSource := nil
        else if (AComponent is TField) then begin
          for I := 0 to Rows.Count - 1 do
            with Rows[I] do
              if Field = AComponent then begin
                Field := nil;
                if Nodes.Count > 0 then
                  for nSub := 0 to Nodes.Count - 1 do
                    if TRow(Nodes.Items[nSub]).Field = AComponent
                      then Field := nil;
              end;
        end;
  end;
end;

procedure TRxDBRecordGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);

  procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer; const Text: string; Alignment: TAlignment);
  const AlignFlags: array[TAlignment] of Integer =
    (DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX);
  var Left: Integer;
  begin
    case Alignment of
      taLeftJustify: Left := ARect.Left + DX;
      taRightJustify: Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else // taCenter
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1 - (ACanvas.TextWidth(Text) shr 1);
    end;
    ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY, ETO_OPAQUE or ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil);
  end;

var
  S: string;
  Alignment: TAlignment;
  Val: Variant;
  DE: Boolean;
  DefaultRow: TRow;
begin
  S := '';
  DE := False;
  Alignment := taLeftJustify;
  if Rows.Count > ARow then begin
    if ACol = 0 then begin
      S := '  ';
      if Rows[ARow].HasChildren then begin
        if Rows[ARow].IndentLevel > 0
          then S := StringOfChar(' ', (Rows[ARow].IndentLevel) * 2)
        else S := '';
        if Rows[ARow].Expanded
          then S := S + '- '
        else S := S + '+';
      end else
        if Rows[ARow].IndentLevel > 0
          then S := StringOfChar(' ', (Rows[ARow].IndentLevel) * 2) + S;
      if Rows[ARow].Title = nil
        then S := S
      else S := S + Rows[ARow].Title.Caption;
      Canvas.Brush.Color := Rows[ARow].Title.Color;
      Canvas.FillRect(ARect);
      Canvas.Font.Assign(Rows[ARow].Title.Font);
      WriteText(Canvas, ARect, 2, 2, S, Rows[ARow].Title.Alignment);
      Exit;

    end else if Assigned(DataSource) and Assigned(DataSource.DataSet) then begin
      DefaultRow := Rows[ARow];
      Alignment := DefaultRow.Alignment;
      Canvas.Font.Assign(DefaultRow.Font);
      if (gdSelected in AState) and (gdFocused in AState) then
        Canvas.Font.Color := clHighlightText;
      with DefaultRow do begin
        if Assigned(Field) then begin
          if IsSpecialField(Field)
            then S := GetSpecialText(Field)
          else
            if DataSource.DataSet.Active
              then S := DataSource.DataSet.FieldByName(DefaultRow.FieldName).DisplayText;
        end
        else begin
          if not (VarType(Value) = varEmpty) then S := Value;
          DE := True
        end;
        if Assigned(OnGetText) then begin
          Val := S;
          OnGetText(Self, ACol, ARow, PickList, Val);
          S := Val;
        end;
      end;
    end;
  end;
  if DE
    then begin
    Canvas.Brush.Color := FixedColor;
    Canvas.FillRect(ARect);
    Canvas.Font.Color := clWindowText;
    WriteText(Canvas, ARect, 2, 2, S, Alignment);
    {
    TR := ARect;
    if gdFocused in AState
    then begin
      TR.Bottom := ARect.Bottom-1;
      TR.Right := ARect.Right-1;
    end
    else begin
      TR.Bottom := ARect.Bottom+1;
      TR.Right := ARect.Right;
    end;
    DrawEdge(Canvas.Handle, TR, DrwEdge[gdFocused in AState], BF_RECT);
    }
  end
  else WriteText(Canvas, ARect, 2, 2, S, Alignment);
end;

procedure TRxDBRecordGrid.ColEnter;
begin
  UpdateIme;
  if Assigned(FOnColEnter) then FOnColEnter(Self);
end;

procedure TRxDBRecordGrid.ColExit;
begin
  if Assigned(FOnColExit) then FOnColExit(Self);
end;

procedure TRxDBRecordGrid.WMSetFocus(var Message: TWMSetFocus);
begin
  SetIme;
  inherited;
end;

procedure TRxDBRecordGrid.WMKillFocus(var Message: TMessage);
begin
  ImeName := Screen.DefaultIme;
  ImeMode := imDontCare;
  inherited;
end;

procedure TRxDBRecordGrid.DefaultHandler(var Msg);
var
  P: TPopupMenu;
  Cell: TGridCoord;
begin
  inherited DefaultHandler(Msg);
  if TMessage(Msg).Msg = wm_RButtonUp then
    with TWMRButtonUp(Msg) do
    begin
      Cell := MouseCoord(XPos, YPos);
      if (Cell.X <> -1) and (Cell.Y <> -1) then
        P := Rows[Cell.Y].PopupMenu
      else
        P := nil;
      if (P <> nil) and P.AutoPopup then
      begin
        SendCancelMode(nil);
        P.PopupComponent := Self;
        with ClientToScreen(SmallPointToPoint(Pos)) do
          P.Popup(X, Y);
        Result := 1;
      end;
    end;
end;

procedure TRxDBRecordGrid.DblClick;
begin
  if Rows[Row].HasChildren then Rows[Row].Expanded := not Rows[Row].Expanded;
  inherited DblClick;
end;

procedure TRxDBRecordGrid.Loaded;
var
  nCnt: Integer;
begin
  inherited Loaded;
  for nCnt := Rows.Count - 1 downto 0 do
  begin
    if not Rows[nCnt].Expanded then
    begin
      Rows[nCnt].FExpanded := True;
      Rows[nCnt].Expanded := False;
    end;
  end;
end;

constructor TRxDBRecordGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Parent := TWinControl(AOwner);
  FRows := CreateRows;
  FDataLink := TrGridDataLink.Create(Self);
  FOriginalImeName := ImeName;
  FOriginalImeMode := ImeMode;
  FObject := nil;
  FRebuildRowsIfEmpty := True;
end;

procedure TRxDBRecordGrid.Reset;
begin
  FRows.Free;
  FRows := nil;
  FRows := CreateRows;
  EditRow := 0;
  RowCount := 0;
  inherited Reset;
  Invalidate;
end;

destructor TRxDBRecordGrid.Destroy;
begin
  FRows.Free;
  FRows := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

{$IFDEF RX_D4}
function TRxDBRecordGrid.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and
    FDataLink.ExecuteAction(Action);
end;

function TRxDBRecordGrid.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and
    FDataLink.UpdateAction(Action);
end;
{$ENDIF}

function TRxDBRecordGrid.GetEditLimit: Integer;
begin
  Result := 0;
  if {(EditRow<>ARow) and }
    Assigned(Rows[EditRow]) and //RSV new
    (Rows[EditRow].Field <> nil) and
    (Rows[EditRow].Field.Datatype = ftString)
    then Result := Rows[EditRow].Field.Size;
end;

procedure TRxDBRecordGrid.FocusCell(ACol, ARow: Longint; MoveAnchor: Boolean);
begin
  inherited FocusCell(ACol, ARow, MoveAnchor);
end;

procedure TRxDBRecordGrid.SetState(const Value: TRecGridState);
begin
  if FState <> Value then FState := Value;
end;

procedure TRxDBRecordGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and Rows[Row].HasChildren then
    case Key of
      VK_ADD: Rows[Row].Expanded := True;
      VK_SUBTRACT: Rows[Row].Expanded := False;
    end;
  if not (ssShift in Shift) and (Key = VK_ESCAPE) then
  begin
    if not (goAlwaysShowEditor in Options) then HideEditor;
    if (Perform(CM_WANTSPECIALKEY, DLGC_WANTALLKEYS, 0) = 0) and
      (Perform(WM_GETDLGCODE, 0, 0) and DLGC_WANTALLKEYS = 0)
    then
      GetParentForm(Self).Perform(CM_DIALOGKEY, VK_ESCAPE, 0);
  end
  else inherited KeyDown(Key, Shift);
end;

{ TRow }

procedure TRow.SetIndentLevel(Value: Cardinal);
var
  nPar: Integer;
begin
  if Index = 0 then
    Value := 0
  else
    if Value > TRow(Collection.Items[Index - 1]).IndentLevel + 1 then
      Value := TRow(Collection.Items[Index - 1]).IndentLevel + 1;
  if FIndentLevel <> Value then
  begin
    FIndentLevel := Value;
    if Value > 0 then
    begin
      nPar := Index;
      repeat
        Dec(nPar)
      until TRow(Collection.Items[nPar]).IndentLevel < IndentLevel;
      Parent := TRow(Collection.Items[nPar]);
      Parent.HasChildren := True;
    end
    else
    begin
      if Index > 0 then
        TRow(Collection.Items[Index - 1]).HasChildren :=
          (TRow(Collection.Items[Index - 1]).IndentLevel < IndentLevel);
      Parent := nil;
      if Index < Collection.Count - 1 then
        HasChildren := (TRow(Collection.Items[Index + 1]).IndentLevel > IndentLevel)
      else
        HasChildren := False;
    end;
    Grid.Invalidate;
  end;
end;

function TRow.SetParentAndLevel: Integer;
var
  Idx: Integer;
  nCnt: Integer;
begin
  Idx := Index;
  Collection.beginUpdate;
  if Idx < Collection.Count then
  begin
    with Collection do
      repeat
        if TRow(Items[Idx]).Nodes.Count > 0 then
        begin
          TRow(Items[Idx]).HasChildren := True;
          for nCnt := 0 to Count - 1 do
          begin
            with TRow(TRow(Items[Idx]).Nodes.Items[nCnt]) do
            begin
              IndentLevel := TRow(Items[Idx]).IndentLevel + 1;
              Parent := TRow(Items[Idx]);
            end;
          end;
        end
        else
          TRow(Items[Idx]).HasChildren := False;
        if (Idx < Count - 1) and TRow(Items[Idx]).HasChildren then
          if TRow(Items[Idx + 1]).IndentLevel > IndentLevel then
          begin
            TRow(Items[Idx]).Expanded := not TRow(Items[Idx]).Expanded;
            TRow(Items[Idx]).Expanded := not TRow(Items[Idx]).Expanded;
          end;
        if TRow(Items[Idx]).IndentLevel > IndentLevel then
        begin
          if TRow(Items[Idx]).IndentLevel > IndentLevel + 1 then
            TRow(Items[Idx]).IndentLevel := IndentLevel + 1;
          TRow(Items[Idx]).Parent := TRow(Items[Idx - 1]);
          TRow(Items[Idx]).Parent.HasChildren := True;
          Idx := TRow(Items[Idx]).SetParentAndLevel;
        end
        else
        begin
          if TRow(Items[Idx]).IndentLevel = IndentLevel then
          begin
            TRow(Items[Idx]).Parent := Parent;
            Inc(Idx);
          end
          else
          begin
            Break;
          end;
        end;
      until Idx = Count;
  end;
  Collection.EndUpdate;
  Result := Idx;
end;

procedure TRow.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  if csDesigning in Grid.ComponentState then
    with Grid do
    begin
      if Value = 0 then
        if IndentLevel > 0 then IndentLevel := 0;
      Rows[0].Parent := nil;
      Rows[0].SetParentAndLevel;
    end;
  Grid.Invalidate;
end;

procedure TRow.SetExpanded(Value: Boolean);
var
  nIdx: Integer;
  nLvl: Cardinal;
begin
  { Always expanded while designing }
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    if Value then // Expand
    begin
      if Nodes.Count > 0 then
      begin
        nIdx := Index;
        Collection.BeginUpdate;
        repeat
          Inc(nIdx);
          Nodes.Items[0].Collection := Self.Collection;
          Collection.Items[Collection.Count - 1].Index := nIdx;
        until Nodes.Count = 0;
//        Grid.ResyncFields;
        Collection.EndUpdate;
      end;
    end
    else // Collapse
    begin
      nIdx := Index;
      nLvl := FIndentLevel + 1;
      if not (csDesigning in Grid.ComponentState) then
      begin
        if Index < Collection.Count - 1 then
        begin
          Inc(nIdx);
          Collection.beginUpdate;
          while nIdx < Collection.Count do
          begin
            if TRow(Collection.Items[nIdx]).IndentLevel < nLvl then
              Break
            else
            begin
              { Forward Collapse }
              if nIdx + 1 < Collection.Count then
                if TRow(Collection.Items[nIdx + 1]).IndentLevel > nLvl then
                  TRow(Collection.Items[nIdx]).Expanded := False;
              { Collapse }
              Collection.Items[nIdx].Collection := Self.Nodes;
            end;
          end;
          Collection.EndUpdate;
        end;
      end;
    end;
    GetGrid.RowCount := Collection.Count;
    Grid.Invalidate;
  end;
end;

function TRow.GetField: TField;
var
  Grid: TRxDBRecordGrid;
begin
  // Returns nil if FieldName can't be found in dataset
  Grid := GetGrid;
  if (FField = nil) and (Length(FFieldName) > 0) and Assigned(Grid)
    and Assigned(Grid.DataLink.DataSet)
  then
    with Grid.Datalink.Dataset do
      if Active then
        SetField(FindField(FieldName));
  Result := FField;
end;

procedure TRow.SetField(Value: TField);
begin
  if FField = Value then Exit;
  FField := Value;
  if Assigned(Value) then
    FFieldName := Value.FieldName;

  if Assigned(Value) and (FieldName <> Value.FieldName) then
  begin
    if (AnsiUpperCase(IDCode) = AnsiUpperCase(FFieldName)) or
      ((Title <> nil) and (AnsiUpperCase(IDCode) = AnsiUpperCase(Title.Caption))) or (IDCode = '')
    then
      FIDCode := Value.FieldName;
    if (Title <> nil) and ((AnsiUpperCase(Title.Caption) = AnsiUpperCase(FFieldName)) or (Title.Caption = ''))
    then
      FTitle.Caption := Value.FieldName;
  end;

  Changed(False);
  GetGrid.InvalidateRow(Index);
end;

procedure TRow.SetFieldName(const Value: string);
var
  AField: TField;
  Grid: TRxDBRecordGrid;
  EmptyVar: Variant;
begin
  AField := nil;
  Grid := GetGrid;
  if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) and
    not (csLoading in Grid.ComponentState) and (Length(Value) > 0)
    then AField := Grid.DataLink.DataSet.FindField(Value); { no exceptions }
  FFieldName := Value;
  SetField(AField);
  if AField <> nil then
  begin
    FTitle.Caption := AField.DisplayLabel;
    Self.Value := EmptyVar;
  end;
  GetGrid.InvalidateRow(Index);
  Changed(False);
end;

function TRow.GetGrid: TRxDBRecordGrid;
begin
  if Assigned(Collection) and (Collection is TRecGridRows)
    then Result := TRecGridRows(Collection).Grid
  else Result := nil;
end;

function TRow.GetDisplayName: string;
begin
  if (Title <> nil) then
  begin
    if (Title.Caption = EmptyStr) then
    begin
      if FFieldName = EmptyStr then
        Result := 'Column'
      else
        Result := FFieldName;
    end
    else
      Result := Title.Caption;
  end;
  Result := StringOfChar(' ', IndentLevel * 2) + Result;
end;

procedure TRow.Assign(Source: TPersistent);
begin
  if Source is TRow then
  begin
    if Assigned(Collection) then Collection.beginUpdate;
    try
      FieldName := TRow(Source).FieldName;

      if cvAlignment in TRow(Source).AssignedValues then
        Alignment := TRow(Source).Alignment;
      if cvReadOnly in TRow(Source).AssignedValues then
        ReadOnly := TRow(Source).ReadOnly;
      if cvImeMode in TRow(Source).AssignedValues then
        ImeMode := TRow(Source).ImeMode;
      if cvImeName in TRow(Source).AssignedValues then
        ImeName := TRow(Source).ImeName;

      if Title <> nil then Title.Caption := TRow(Source).Title.Caption;
      DropDownRows := TRow(Source).DropDownRows;
      ButtonStyle := TRow(Source).ButtonStyle;
      PickList := TRow(Source).PickList;
      PopupMenu := TRow(Source).PopupMenu;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


function TRow.DefaultAlignment: TAlignment;
begin
  if Assigned(Field) then Result := Field.Alignment
  else Result := taLeftJustify;
end;

function TRow.DefaultReadOnly: Boolean;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := GetGrid;
  Result := (Assigned(Grid) and Grid.ReadOnly) or (Assigned(Field) and FField.ReadOnly);
end;

function TRow.GetAlignment: TAlignment;
begin
  if cvAlignment in FAssignedValues
    then Result := FAlignment
  else Result := DefaultAlignment;
end;

function TRow.GetPickList: TStrings;
begin
  if FPickList = nil then
    FPickList := TStringList.Create;
  Result := FPickList;
end;

function TRow.GetReadOnly: Boolean;
begin
  if cvReadOnly in FAssignedValues then
    Result := FReadOnly
  else
    Result := DefaultReadOnly;
end;

function TRow.IsAlignmentStored: Boolean;
begin
  Result := (cvAlignment in FAssignedValues) and (FAlignment <> DefaultAlignment);
end;

function TRow.IsReadOnlyStored: Boolean;
begin
  Result := (cvReadOnly in FAssignedValues) and (FReadOnly <> DefaultReadOnly);
end;

procedure TRow.SetAlignment(Value: TAlignment);
begin
  if (cvAlignment in FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FAssignedValues, cvAlignment);
  GetGrid.InvalidateRow(Index);
  Changed(False);
end;

procedure TRow.SetButtonStyle(Value: TColumnButtonStyle);
begin
  if Value = FButtonStyle then Exit;
  FButtonStyle := Value;
  GetGrid.InvalidateRow(Index);
  Changed(False);
end;

procedure TRow.SetPickList(Value: TStrings);
begin
  if Value = nil then
  begin
    FPickList.Free;
    FPickList := nil;
    Exit;
  end;
  PickList.Assign(Value);
end;

procedure TRow.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if Value <> nil then Value.FreeNotification(GetGrid);
end;

procedure TRow.SetReadOnly(Value: Boolean);
begin
  if (cvReadOnly in FAssignedValues) and (Value = FReadOnly) then Exit;
  FReadOnly := Value;
  Include(FAssignedValues, cvReadOnly);
  Changed(False);
end;

function TRow.GetEditMask: string;
begin
  Result := FEditMask;
  if FEditMask = '' then
  begin
    if Assigned(Field) then
      Result := Field.EditMask;
    if Result = '' then
      if Assigned(FOnGetEditMask) then FOnGetEditMask(Grid, Grid.EditRow, Result);
  end;
end;

procedure TRow.EditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(GetGrid)
  else
    if Assigned(Grid.OnEditButtonClick) then
      Grid.OnEditButtonClick(Grid, Index, Field);
end;

function TRow.DefaultImeMode: TImeMode;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeMode
  else
    Result := FImeMode;
end;

function TRow.DefaultImeName: TImeName;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.ImeName
  else
    Result := FImeName;
end;

function TRow.GetImeMode: TImeMode;
begin
  if cvImeMode in FAssignedValues then
    Result := FImeMode
  else
    Result := DefaultImeMode;
end;

function TRow.GetImeName: TImeName;
begin
  if cvImeName in FAssignedValues then
    Result := FImeName
  else
    Result := DefaultImeName;
end;

function TRow.IsImeModeStored: Boolean;
begin
  Result := (cvImeMode in FAssignedValues) and (FImeMode <> DefaultImeMode);
end;

function TRow.IsImeNameStored: Boolean;
begin
  Result := (cvImeName in FAssignedValues) and (FImeName <> DefaultImeName);
end;

procedure TRow.SetImeMode(Value: TImeMode);
begin
  if (cvImeMode in FAssignedValues) or (Value <> DefaultImeMode) then
  begin
    FImeMode := Value;
    Include(FAssignedValues, cvImeMode);
  end;
  Changed(False);
end;

procedure TRow.SetImeName(Value: TImeName);
begin
  if (cvImeName in FAssignedValues) or (Value <> DefaultImeName) then
  begin
    FImeName := Value;
    Include(FAssignedValues, cvImeName);
  end;
  Changed(False);
end;

function TRow.GetText: string;
begin
  Result := FValue;
end;

procedure TRow.SetText(Value: string);
begin
  FValue := Value;
  GetGrid.InvalidateRow(Index);
  FieldName := '';
end;

procedure TRow.Refresh;
begin
  GetGrid.InvalidateRow(Index);
end;

constructor TRow.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FTitle := CreateTitle;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
  GetGrid.RowCount := Collection.Count;
  FDropDownRows := 7;
  FButtonStyle := cbsAuto;
  FImeMode := imDontCare;
  FImeName := Screen.DefaultIme;
  FObject := nil;
  FParent := nil;
  FNodes := TRecGridRows.Create(Grid, TRow, False);
  FIndentLevel := 0;
  FExpanded := False;
  FHasChildren := False;
  FIDCode := '';
  FStored := True;
  GetGrid.State := gsCustomized;
end;

destructor TRow.Destroy;
begin
  Grid.RowCount := Collection.Count - 1;
  FFont.Free;
  FPickList.Free;
  FNodes.Free;
  FTitle.Free;
  inherited Destroy;
end;

procedure TRow.SetTitle(Value: TRowTitle);
begin
  FTitle.Assign(Value);
end;

function TRow.CreateTitle: TRowTitle;
begin
  Result := TRowTitle.Create(Self);
end;

function TRow.DefaultFont: TFont;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FFont;
end;

function TRow.GetFont: TFont;
var
  Save: TNotifyEvent;
begin
  if not (cvFont in FAssignedValues) and (FFont.Handle <> DefaultFont.Handle) then
  begin
    Save := FFont.OnChange;
    FFont.OnChange := nil;
    FFont.Assign(DefaultFont);
    FFont.OnChange := Save;
  end;
  Result := FFont;
end;

function TRow.IsFontStored: Boolean;
begin
  Result := (cvFont in FAssignedValues);
end;

procedure TRow.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if cvFont in FAssignedValues then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TRow.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Include(FAssignedValues, cvFont);
  Changed(False);
end;

procedure TRow.FontChanged(Sender: TObject);
begin
  Include(FAssignedValues, cvFont);
  Title.RefreshDefaultFont;
  Changed(False);
end;

{ TRecGridRows }

function TRecGridRows.GetColByID(Index: Variant): TRow;
var
  nCnt, nSub: Integer;
begin
  Result := nil;
  Index := AnsiUpperCase(Index);
  for nCnt := 0 to Count - 1 do
  begin
    if AnsiUpperCase(TRow(Items[nCnt]).FIDCode) = Index then
    begin
      Result := TRow(Items[nCnt]);
      Break;
    end
    else
      if TRow(Items[nCnt]).Nodes.Count > 0 then
        for nSub := 0 to TRow(Items[nCnt]).Nodes.Count - 1 do
          if AnsiUpperCase(TRow(TRow(Items[nCnt]).Nodes.Items[nSub]).FIDCode) = Index then
          begin
            Result := TRow(TRow(Items[nCnt]).Nodes.Items[nSub]);
            Break;
          end
  end;
end;

function TRecGridRows.GetColumn(Index: Variant): TRow;
begin
  if Vartype(Index) = VarString then
    Result := GetColByID(Index)
  else
    if Index >= 0 then
      Result := TRow(inherited Items[Index])
    else
      Result := nil;
end;

procedure TRecGridRows.SetColumn(Index: Variant; Value: TRow);
begin
  if vartype(Index) = varString then
    GetColByID(Index).Assign(Value)
  else
    if Index >= 0 then
      Items[Index].Assign(Value);
end;

function TRecGridRows.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TRecGridRows.Add: TRow;
begin
  Result := TRow(inherited Add)
end;

procedure TRecGridRows.Clear;
begin
  inherited Clear;
end;

procedure TRecGridRows.Delete(Index: Variant);
begin
  Items[Index].Free;
  if Count = 0 then Clear;
end;

constructor TRecGridRows.Create(Grid: TRxDBRecordGrid; ColumnClass: TColumnClass; AddEmpty: Boolean);
begin
  inherited Create(ColumnClass);
  FGrid := Grid;
  if AddEmpty then Add;
end;

procedure TRecGridRows.RebuildRows;

  procedure AddFields(Fields: TFields; Depth: Integer);
  var
    I: Integer;
  begin
    Inc(Depth);
    for I := 0 to Fields.Count - 1 do
    begin
      if Fields[i].Visible then
      begin
        Add.FieldName := Fields[I].FullName;
        if Fields[I].DataType in [ftADT, ftArray] then
          AddFields((Fields[I] as TObjectField).Fields, Depth);
      end;
    end;
  end;

begin
  if Assigned(FGrid) and Assigned(FGrid.DataSource) and
    Assigned(FGrid.Datasource.Dataset)
  then
  begin
//    FGrid.BeginUpDate;
    try
      Clear;
      AddFields(FGrid.Datasource.Dataset.Fields, 0);
      FGrid.State := gsDefault;
    finally
//      FGrid.EndUpDate;
    end
  end
  else
    Clear;
end;

{ TrGridDataLink }

procedure TrGridDataLink.CheckBrowseMode;
begin
  if DataSet.State in [dsEdit, dsInsert] then
    if FGrid.Editor <> nil then
      if FGrid.Editor.Modified then
      begin
        FGrid.Editor.Modified := False;
        TRxRecGridInplaceEdit(FGrid.Editor).CloseUp(True);
      end;
end;

procedure TrGridDataLink.ActiveChanged;
begin
  if Active then
  begin
    FGrid.UpdateEdit;
    if (FGrid.Rows.Count = 0) and FGrid.FRebuildRowsIfEmpty then
      FGrid.Rows.RebuildRows
    else
  end
  else
  begin
    if FGrid.State = gsDefault then
      FGrid.Rows.Clear;
    FGrid.ResyncFields;
  end;
  FGrid.Invalidate;
end;

procedure TrGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.InUpdating := False;
  FGrid.UpdateEdit;
  FGrid.Invalidate;
end;

procedure TrGridDataLink.RecordChanged(Field: TField);
var
  nCnt: Integer;
begin
  if DataSet.State = dsBrowse then
    FGrid.InUpdating := False;
  if Field = nil then
  begin
    FGrid.Invalidate;
    if not FGrid.InUpdating then
      FGrid.UpdateEdit;
  end
  else
    with FGrid do
      if not InUpdating then
        for nCnt := 0 to Rows.Count - 1 do
          if Rows[nCnt].Field = Field then
          begin
            InvalidateRow(nCnt);
            if nCnt = EditRow then UpdateEdit;
          end;
end;

constructor TrGridDataLink.Create(AGrid: TRxDBRecordGrid);
begin
  inherited Create;
  FGrid := AGrid;
end;

destructor TrGridDataLink.Destroy;
begin
  inherited Destroy;
end;

{ TPopupListBox }

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
  else
    if Key >= ' ' then
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WPARAM(-1), LPARAM(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TRxRecGridInplaceEdit(Owner).CloseUp((X >= 0) and (Y >= 0) and
    (X < Width) and (Y < Height));
end;

{ TrRecGridInplaceEdit }

constructor TRxRecGridInplaceEdit.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FLookupSource := TDataSource.Create(Self);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  FSavedCancel := nil;
end;

procedure TRxRecGridInplaceEdit.CatchCancel(Dataset: TDataSet);
begin
  Reset;
  if Assigned(FSavedCancel) then FSavedCancel(DataSet);
end;

procedure TRxRecGridInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if FEditStyle <> esSimple then Dec(R.Right, FButtonWidth);
  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.Fareast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TRxRecGridInplaceEdit.CloseUp(Accept: Boolean);
var
  MasterField: TField;
  ListValue: Variant;
  nCnt: Integer;
  nIdx: Integer;
  //RSV
  CRow: TRow;

begin
  if FListVisible then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if FActiveList = FDataList then
      ListValue := FDataList.KeyValue
    else
      if FPickList.ItemIndex <> -1 then
        ListValue := FPickList.Items[FPicklist.ItemIndex];

    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    if Assigned(FDataList) then
      FDataList.ListSource := nil;
    FLookupSource.Dataset := nil;
    Invalidate;
    if Accept then
      if FActiveList = FDataList then
        with TRxDBRecordGrid(Grid), Rows[EditRow].Field do
        begin
          MasterField := DataSet.FieldByName(KeyFields);
          if MasterField.CanModify then
          begin
            if Assigned(GetColumn.OnValidate) then
              GetColumn.OnValidate(Grid, EditRow, Rows[EditRow].PickList, ListValue, Accept);

            if Accept then
            begin
              InUpdating := True;
              //RSV
              if not (DataSet.State in [dsEdit, dsInsert]) then
                DataSet.Edit;
              MasterField.Value := ListValue;
              UpdateContents;
              SelectAll;
              Accept := False;
            end
            else
              Reset;
          end;
        end
      else
        if (not varIsNull(ListValue)) and EditCanModify then
          Text := ListValue;
  end;

  if Accept and EditCanModify then
  begin
    if FEditStyle in [esPickList, esDropDown] then
    begin
      nIdx := PickList.Items.IndexOf(Self.Text);

      if nIdx = -1 then
        for nCnt := 0 to PickList.Items.Count - 1 do
          if AnsiUpperCase(Self.Text) = AnsiUpperCase(PickList.Items.Strings[nCnt]) then
          begin
            Self.Text := PickList.Items.Strings[nCnt];
            nIdx := nCnt;
            Break;
          end;
      if (FEditStyle = esPickList) and (nIdx = -1) then
      begin
        Reset;
        Exit;
      end;
    end;

    ListValue := Text;

    //RSV    if Assigned(GetColumn.OnValidate) then
    CRow := GetColumn;
    if Assigned(CRow) and Assigned(CRow.OnValidate) then
      with TRxDBRecordGrid(Grid) do
        GetColumn.OnValidate(Grid, EditRow, CRow.PickList, ListValue, Accept);

    if Accept then
    begin
      Text := ListValue;
      UpdateField;
    end
    else
      Reset;
  end;
end;

procedure TRxRecGridInplaceEdit.UpdateField;
var
  S: string;
begin
  with TRxDBRecordGrid(Grid), Rows[EditRow] do
    if Assigned(Rows[EditRow]) then //RSV new
//RSV
      if not Assigned(Field) or (Assigned(Field) and not IsSpecialField(Field)) then
      begin
        if Assigned(Field) then
        begin
          if DataSource.DataSet.Active then
          begin
//            if DataSource.DataSet.FieldByName(FieldName).Text <> Self.Text then
            if DataSource.DataSet.FieldByName(FieldName).AsString <> Self.Text then
            begin
              InUpdating := True;
//RSV
              if Assigned(DataSource.DataSet) and not (DataSource.DataSet.State in [dsEdit, dsInsert]) then
                DataSource.DataSet.Edit;
              S := DataSource.DataSet.FieldByName(FieldName).AsString;
              try
                if DataSource.DataSet.FieldByName(FieldName).Datatype = ftMemo then
                  DataSource.DataSet.FieldByName(FieldName).AsString := Self.Text
                else
                  DataSource.DataSet.FieldByName(FieldName).Text := Self.Text;
              finally
                Self.Text := S;
              end;
//                    DataSource.DataSet.FieldByName(FieldName).AsString := Self.Text;
              InUpdating := False;
            end;
          end;
        end
        else
          if not (vartype(Value) = varEmpty) then
            Value := Self.Text;

        if Assigned(OnSetText) then OnSetText(TRxDBRecordGrid(Grid), EditRow, Modified, PickList, Self.Text);
      end;

  UpdateContents;
  SelectAll;
end;

procedure TRxRecGridInplaceEdit.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if FListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if FListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TRxRecGridInplaceEdit.DropDown;
var
  P: TPoint;
  I, J, Y: Integer;
  Column: TRow;
begin
  if not FListVisible and Assigned(FActiveList) then
  begin
    FActiveList.Width := Width;
    with TRxDBRecordGrid(Grid) do
      Column := Rows[Row];
    if FActiveList = FDataList then
      with Column.Field do
      begin
        FDataList.Color := Color;
        FDataList.Font := Font;
        FDataList.RowCount := Column.DropDownRows;
        FLookupSource.DataSet := LookupDataSet;
        FDataList.KeyField := LookupKeyFields;
        FDataList.ListField := LookupResultField;
        FDataList.ListSource := FLookupSource;
        FDataList.KeyValue := DataSet.FieldByName(KeyFields).Value;
      end
    else
    begin
      FPickList.Color := Color;
      FPickList.Font := Font;
      FPickList.Items := Column.Picklist;
      if Cardinal(FPickList.Items.Count) >= Column.DropDownRows then
        FPickList.Height := FPickList.ItemHeight * Integer(Column.DropDownRows) + 4
      else
        FPickList.Height := FPickList.Items.Count * FPickList.ItemHeight + 4;

      if Assigned(Column.Field) then
      begin
        if Column.Field.IsNull then
          FPickList.ItemIndex := -1
        else
          FPickList.ItemIndex := FPickList.Items.IndexOf(Column.Field.Value);
      end
      else
        FPickList.ItemIndex := FPickList.Items.IndexOf(Text);

      J := FPickList.ClientWidth;
      for I := 0 to FPickList.Items.Count - 1 do
      begin
        Y := FPickList.Canvas.TextWidth(FPickList.Items[I]);
        if Y > J then J := Y;
      end;
      FPickList.ClientWidth := J;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FActiveList.Height > Screen.Height then Y := P.Y - FActiveList.Height;
    SetWindowPos(FActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

type
  TWinControlCracker = class(TWinControl) end;

function TRxRecGridInplaceEdit.GetColumn: TRow;
begin
  with TRxDBRecordGrid(Grid) do
    Result := Rows[EditRow];
end;

procedure TRxRecGridInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key in [VK_SUBTRACT, VK_ADD]) and Assigned(Grid)
    then TRxDBRecordGrid(Grid).KeyDown(Key, Shift);

  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    GetColumn.EditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TRxRecGridInplaceEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TRxRecGridInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(X, Y))
  then
  begin
    if FListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(FActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxRecGridInplaceEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if FListVisible then
    begin
      ListPos := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, LPARAM(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRxRecGridInplaceEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := FPressed;
  StopTracking;
  if (Button = mbLeft) and (FEditStyle = esEllipsis) and WasPressed then
    GetColumn.EditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TRxRecGridInplaceEdit.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W: Integer;
begin
  if FEditStyle <> esSimple then
  begin
    SetRect(R, Width - FButtonWidth, 0, Width, Height);
    Flags := 0;
    if FEditStyle in [esDataList, esPickList, esDropDown] then
    begin
      if FActiveList = nil then
        Flags := DFCS_INACTIVE
      else if FPressed then
        Flags := DFCS_FLAT or DFCS_PUSHED;
      DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
    end
    else { esEllipsis }
    begin
      if FPressed then
        Flags := BF_FLAT;
      DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
      Flags := ((R.Right - R.Left) shr 1) - 1 + Ord(FPressed);
      W := Height shr 3;
      if W = 0 then W := 1;
      PatBlt(DC, R.Left + Flags, R.Top + Flags, W, W, BLACKNESS);
      PatBlt(DC, R.Left + Flags - (W * 2), R.Top + Flags, W, W, BLACKNESS);
      PatBlt(DC, R.Left + Flags + (W * 2), R.Top + Flags, W, W, BLACKNESS);
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TRxRecGridInplaceEdit.SetEditStyle(Value: TEditStyle);
begin
  if Value = FEditStyle then Exit;
  FEditStyle := Value;
  case Value of
    esPickList, esDropDown:
      begin
        if FPickList = nil then
        begin
          FPickList := TPopupListbox.Create(Self);
          FPickList.Visible := False;
          FPickList.Parent := Self;
          FPickList.OnMouseUp := ListMouseUp;
          FPickList.IntegralHeight := True;
          FPickList.ItemHeight := 11;
        end;
        FActiveList := FPickList;
      end;
    esDataList:
      begin
        if FDataList = nil then
        begin
          FDataList := TPopupDataList.Create(Self);
          FDataList.Visible := False;
          FDataList.Parent := Self;
          FDataList.OnMouseUp := ListMouseUp;
        end;
        FActiveList := FDataList;
      end;
  else { cbsNone, cbsEllipsis, or read only field }
    FActiveList := nil;
  end;
  with TRxDBRecordGrid(Grid) do
    Self.ReadOnly := Rows[Row].ReadOnly;
  Repaint;
end;

procedure TRxRecGridInplaceEdit.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRxRecGridInplaceEdit.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  SetRect(R, ClientWidth - FButtonWidth, 0, ClientWidth, ClientHeight);
  NewState := PtInRect(R, Point(X, Y));
  if FPressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TRxRecGridInplaceEdit.UpdateContents;
var
  Column: TRow;
  NewStyle: TEditStyle;
  MasterField: TField;
begin
  with TRxDBRecordGrid(Grid) do
    Column := Rows[Row];

  Self.ReadOnly := Column.ReadOnly;
  NewStyle := esSimple;

  case Column.ButtonStyle of
    cbsEllipsis: NewStyle := esEllipsis;
    cbsAuto:
      begin
        if Assigned(Column.Field) then
        begin
          with Column.Field do
          begin
            { Show the dropdown button only if the field is editable }
            if FieldKind = fkLookup then
            begin
              MasterField := Dataset.FieldByName(KeyFields);
              { Column.DefaultReadonly will always be True for a lookup field.
               Test if Column.ReadOnly has been assigned a value of True }
              if Assigned(MasterField) and
                MasterField.CanModify and not
                ((cvReadOnly in Column.AssignedValues) and
                Column.ReadOnly) then
                with TRxDBRecordGrid(Grid) do
                  if not ReadOnly and
                    DataLink.Active and not
                    Datalink.ReadOnly then
                    NewStyle := esDataList
            end
            else
            begin
              if IsSpecialField(Column.Field) then
              begin
                if Assigned(TRxDBRecordGrid(Grid).OnEditButtonClick) then
                  NewStyle := esEllipsis;
              end;
            end;
          end;
        end;

        if Assigned(Column.Picklist) and
          (Column.PickList.Count > 0) and not Column.Readonly
        then
          NewStyle := esPickList;
      end;
    cbsDropDown:
      if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and not Column.Readonly then
        NewStyle := esDropDown;
    cbsDropDownList:
      if Assigned(Column.Picklist) and (Column.PickList.Count > 0) and not Column.Readonly then
        NewStyle := esPickList;
  end;
  EditStyle := NewStyle;
  inherited UpdateContents;
end;

procedure TRxRecGridInplaceEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  //!!!!!!!!!!!!!!!!!!
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then
    CloseUp(False);
end;

procedure TRxRecGridInplaceEdit.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TRxRecGridInplaceEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if (TRxDBRecordGrid(Grid).DataSource <> nil) then
    if (TRxDBRecordGrid(Grid).DataSource.DataSet <> nil) and not Assigned(FSavedCancel) then
    begin
      FSavedCancel := TRxDBRecordGrid(Owner).DataSource.DataSet.BeforeCancel;
      TRxDBRecordGrid(Owner).DataSource.DataSet.BeforeCancel := Self.CatchCancel;
    end;
end;

procedure TRxRecGridInplaceEdit.WMKillFocus(var Message: TMessage);
begin
  if SysLocale.FarEast then
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
  end;
  inherited;
  CloseUp(Modified);
  if TRxDBRecordGrid(Owner).DataSource <> nil then
    if TRxDBRecordGrid(Owner).Datasource.DataSet <> nil then
      TRxDBRecordGrid(Owner).DataSource.DataSet.BeforeCancel := FSavedCancel;
  FSavedCancel := nil;
end;

procedure TRxRecGridInplaceEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  nIdx: Integer;
  oStr: TStrings;
  nCnt: Integer;
begin
  with Message do
    if (FEditStyle <> esSimple) then
    begin
      if PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), Point(XPos, YPos)) then Exit;

      if FEditStyle in [esPickList, esDropDown] then
      begin
        with TRxDBRecordGrid(Grid) do FPickList.Items := Rows[Row].PickList;
        if FPickList.Items.Count > 0 then
          oStr := FPickList.Items
        else
          with TRxDBRecordGrid(Grid) do oStr := Rows[Row].PickList;
        if oStr.Count > 0 then
        begin
          nIdx := oStr.IndexOf(Self.Text);
          if nIdx = -1 then
            for nCnt := 0 to oStr.Count - 1 do
              if AnsiUpperCase(Self.Text) = AnsiUpperCase(oStr.Strings[nCnt]) then
              begin
                Self.Text := oStr.Strings[nCnt];
                nIdx := nCnt;
                Break;
              end;
          if nIdx <> -1 then
          begin
            if nIdx < oStr.Count - 1 then
              Self.Text := oStr.Strings[nIdx + 1]
            else
              Self.Text := oStr.Strings[0];
          end
          else
            Self.Text := oStr.Strings[0];
          Modified := True;
          with GetColumn, TRxDBRecordGrid(Grid) do
          begin
            if Assigned(OnSetEditText) then OnSetEditText(Self, Row, PickList, Self.Text);
          end;
          CloseUp(True);
        end;
      end;
    end;
  inherited;
end;

procedure TRxRecGridInplaceEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TRxRecGridInplaceEdit.WMSetCursor(var Message: TWMSetCursor);
var P: TPoint;
begin
  GetCursorPos(P);
  if (FEditStyle <> esSimple) and
    PtInRect(Rect(Width - FButtonWidth, 0, Width, Height), ScreenToClient(P))
  then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TRxRecGridInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle in [esPickList, esDropDown, esDataList] then
        with TWMKey(Message) do
        begin
          doDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
          if (CharCode <> 0) and FListVisible then
          begin
            with TMessage(Message) do
              SendMessage(FActiveList.Handle, Msg, WParam, LParam);
            Exit;
          end;
        end
  end;
  inherited;
end;

{ TRowTitle }

constructor TRowTitle.Create(Column: TRow);
begin
  inherited Create;
  FColumn := Column;
  FFont := TFont.Create;
  FFont.Assign(DefaultFont);
  FFont.OnChange := FontChanged;
end;

destructor TRowTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TRowTitle.Assign(Source: TPersistent);
begin
  if Source is TRowTitle then
  begin
    if cvTitleAlignment in TRowTitle(Source).FColumn.FAssignedValues then
      Alignment := TRowTitle(Source).Alignment;
    if cvTitleColor in TRowTitle(Source).FColumn.FAssignedValues then
      Color := TRowTitle(Source).Color;
    if cvTitleCaption in TRowTitle(Source).FColumn.FAssignedValues then
      Caption := TRowTitle(Source).Caption;
    if cvTitleFont in TRowTitle(Source).FColumn.FAssignedValues then
      Font := TRowTitle(Source).Font;
  end
  else
    inherited Assign(Source);
end;

function TRowTitle.DefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TRowTitle.DefaultColor: TColor;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.FixedColor
  else
    Result := clBtnFace;
end;

function TRowTitle.DefaultFont: TFont;
var
  Grid: TRxDBRecordGrid;
begin
  Grid := FColumn.GetGrid;
  if Assigned(Grid) then
    Result := Grid.Font
  else
    Result := FColumn.Font;
end;

function TRowTitle.DefaultCaption: string;
var
  Field: TField;
begin
  Field := FColumn.Field;
  if Assigned(Field) then
    Result := Field.DisplayName
  else
    Result := FColumn.FieldName;
end;

procedure TRowTitle.FontChanged(Sender: TObject);
begin
  Include(FColumn.FAssignedValues, cvTitleFont);
  FColumn.Changed(True);
end;

function TRowTitle.GetAlignment: TAlignment;
begin
  if cvTitleAlignment in FColumn.FAssignedValues then
    Result := FAlignment
  else
    Result := DefaultAlignment;
end;

function TRowTitle.GetColor: TColor;
begin
  if cvTitleColor in FColumn.FAssignedValues then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TRowTitle.GetCaption: string;
begin
  if cvTitleCaption in FColumn.FAssignedValues then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TRowTitle.GetFont: TFont;
var
  Save: TNotifyEvent;
  Def: TFont;
begin
  if not (cvTitleFont in FColumn.FAssignedValues) then
  begin
    Def := DefaultFont;
    if (FFont.Handle <> Def.Handle) or (FFont.Color <> Def.Color) then
    begin
      Save := FFont.OnChange;
      FFont.OnChange := nil;
      FFont.Assign(DefaultFont);
      FFont.OnChange := Save;
    end;
  end;
  Result := FFont;
end;

function TRowTitle.IsAlignmentStored: Boolean;
begin
  Result := (cvTitleAlignment in FColumn.FAssignedValues) and
    (FAlignment <> DefaultAlignment);
end;

function TRowTitle.IsColorStored: Boolean;
begin
  Result := (cvTitleColor in FColumn.FAssignedValues) and
    (FColor <> DefaultColor);
end;

function TRowTitle.IsFontStored: Boolean;
begin
  Result := (cvTitleFont in FColumn.FAssignedValues);
end;

function TRowTitle.IsCaptionStored: Boolean;
begin
  Result := (cvTitleCaption in FColumn.FAssignedValues) and
    (FCaption <> DefaultCaption);
end;

procedure TRowTitle.RefreshDefaultFont;
var
  Save: TNotifyEvent;
begin
  if (cvTitleFont in FColumn.FAssignedValues) then Exit;
  Save := FFont.OnChange;
  FFont.OnChange := nil;
  try
    FFont.Assign(DefaultFont);
  finally
    FFont.OnChange := Save;
  end;
end;

procedure TRowTitle.RestoreDefaults;
var
  FontAssigned: Boolean;
begin
  FontAssigned := cvTitleFont in FColumn.FAssignedValues;
  FColumn.FAssignedValues := FColumn.FAssignedValues - ColumnTitleValues;
  FCaption := '';
  RefreshDefaultFont;
  FColumn.Changed(FontAssigned);
end;

procedure TRowTitle.SetAlignment(Value: TAlignment);
begin
  if (cvTitleAlignment in FColumn.FAssignedValues) and (Value = FAlignment) then Exit;
  FAlignment := Value;
  Include(FColumn.FAssignedValues, cvTitleAlignment);
  FColumn.Changed(False);
  with FColumn do GetGrid.InvalidateRow(Index);
end;

procedure TRowTitle.SetColor(Value: TColor);
begin
  if (cvTitleColor in FColumn.FAssignedValues) and (Value = FColor) then Exit;
  FColor := Value;
  Include(FColumn.FAssignedValues, cvTitleColor);
  FColumn.Changed(False);
  with FColumn do GetGrid.InvalidateRow(Index);
end;

procedure TRowTitle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  with FColumn do GetGrid.InvalidateRow(Index);
end;

procedure TRowTitle.SetCaption(const Value: string);
var
  Grid: TRxDBRecordGrid;
begin
  if Column.IsStored then
  begin
    if (cvTitleCaption in FColumn.FAssignedValues) and (Value = FCaption) then Exit;
    FCaption := Value;
    Include(Column.FAssignedValues, cvTitleCaption);
    Column.Changed(False);
  end
  else
  begin
    Grid := Column.GetGrid;
    if Assigned(Grid) and (Grid.Datalink.Active) and Assigned(Column.Field) then
      Column.Field.DisplayLabel := Value;
  end;
  with FColumn do GetGrid.InvalidateRow(Index);
end;

end.