{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and components added by JB.                  }
{ Revised for Delphi6 by JB.                            }
{*******************************************************}

(* ------------------------------------------------------
  + Changes by Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [dpv]
  + Changes by Marek Jedlinski <marek@tranglos.com> (Poland) [mj]
   >> Changes to original source code available in KeyNote NF project.
   >> Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
      in https://github.com/dpradov/keynote-nf
--------------------------------------------------------*)


unit RxCtrls;

{$I RX.INC}
{$W-,T-}

interface

uses
  Windows, Registry, {$IFDEF RX_D7}Themes, {$ENDIF}
  Messages, Classes, Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls, Forms,
  {$IFDEF RX_D16}System.UITypes, {$ENDIF}
  Buttons, Menus, RxTimer, RxConst, IniFiles, RxPlacemnt, RxGraph;

type
  TPositiveInt = 1..MaxInt;

{ TTextListBox }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTextListBox = class(TCustomListBox)
  private
    FMaxWidth: Integer;
    {$IFDEF VER80}
    FTabWidth: Integer;
    procedure SetTabWidth(Value: Integer);
    {$ENDIF}
    procedure ResetHorizontalExtent;
    procedure SetHorizontalExtent;
    function GetItemWidth(Index: Integer): Integer;
  protected
    {$IFDEF VER80}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    {$ENDIF}
    procedure WndProc(var Message: TMessage); override;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFNDEF VER80}
    {$IFNDEF RX_D9}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    {$IFNDEF VER80}
    property TabWidth;
    {$ELSE}
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
    {$ENDIF}
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;           // [mj]
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
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    {$IFDEF RX_D6}
    property Style;
    property AutoComplete;
    {$IFDEF RX_D9}
    property AutoCompleteDelay;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property ScrollWidth;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnMeasureItem;
    {$IFDEF RX_D9}
    property OnMouseActivate;
    {$ENDIF}
    {$ENDIF}
  end;

{ TRxCustomListBox }

  TGetItemWidthEvent = procedure(Control: TWinControl; Index: Integer;
    var Width: Integer) of object;

  {$IFNDEF VER80}
  TGetItemHintEvent = procedure(Control: TWinControl; Index: Integer;
    var Hint: string) of object;

  THPC_HintSource = (hsMainControl, hsMainItems, hsItemsMain, hsItems, hsDefault);
  {$ENDIF}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCustomListBox = class(TWinControl)
  private
    FItems: TStrings;
    FBorderStyle: TBorderStyle;
    FCanvas: TCanvas;
    FColumns: Integer;
    FItemHeight: Integer;
    FStyle: TListBoxStyle;
    FIntegralHeight: Boolean;
    FMultiSelect: Boolean;
    FSorted: Boolean;
    FExtendedSelect: Boolean;
    FTabWidth: Integer;
    FSaveItems: TStringList;
    FSaveTopIndex: Integer;
    FSaveItemIndex: Integer;
    FAutoScroll: Boolean;
    FGraySelection: Boolean;
    FMaxItemWidth: Integer;
    FOnDrawItem: TDrawItemEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnGetItemWidth: TGetItemWidthEvent;
    {$IFNDEF VER80}
    FHintSource: THPC_HintSource;
    FOnGetItemHintEvent: TGetItemHintEvent;
    {$ENDIF}
    procedure ResetHorizontalExtent;
    procedure SetHorizontalExtent;
    function GetAutoScroll: Boolean;
    function GetItemHeight: Integer; virtual;
    function GetItemIndex: Integer;
    function GetSelCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetTopIndex: Integer;
    procedure SetAutoScroll(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColumnWidth;
    procedure SetColumns(Value: Integer);
    procedure SetExtendedSelect(Value: Boolean);
    procedure SetIntegralHeight(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetSelected(Index: Integer; Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetStyle(Value: TListBoxStyle);
    procedure SetTabWidth(Value: Integer);
    procedure SetTopIndex(Value: Integer);
    procedure SetGraySelection(Value: Boolean);
    procedure SetOnDrawItem(Value: TDrawItemEvent);
    procedure SetOnGetItemWidth(Value: TGetItemWidthEvent);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    {$IFNDEF VER80}
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    function GetItemHint(Index: Integer): string;
    procedure SetItemHint(Index: Integer; const Value: string);
    {$ENDIF}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function CreateItemList: TStrings; virtual;
    function GetItemWidth(Index: Integer): Integer; virtual;
    procedure WndProc(var Message: TMessage); override;
    procedure DragCanceled; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); virtual;
    procedure MeasureItem(Index: Integer; var Height: Integer); virtual;
    function GetItemData(Index: Integer): {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF}; dynamic;
    procedure SetItemData(Index: Integer; AData: {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF}); dynamic;
    procedure SetItems(Value: TStrings); virtual;
    procedure ResetContent; dynamic;
    procedure DeleteString(Index: Integer); dynamic;
    {$IFNDEF VER80}
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    property OnGetItemHint: TGetItemHintEvent read FOnGetItemHintEvent write FOnGetItemHintEvent default nil;
    {$ENDIF}
    property AutoScroll: Boolean read GetAutoScroll write SetAutoScroll default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: Integer read FColumns write SetColumns default 0;
    property ExtendedSelect: Boolean read FExtendedSelect write SetExtendedSelect default True;
    property GraySelection: Boolean read FGraySelection write SetGraySelection default False;
    property IntegralHeight: Boolean read FIntegralHeight write SetIntegralHeight default False;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property ParentColor default False;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property Style: TListBoxStyle read FStyle write SetStyle default lbStandard;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 0;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write SetOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property OnGetItemWidth: TGetItemWidthEvent read FOnGetItemWidth write SetOnGetItemWidth;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure DefaultDrawText(X, Y: Integer; const S: string);
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    function ItemRect(Index: Integer): TRect;
    property Canvas: TCanvas read FCanvas;
    property Items: TStrings read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelCount: Integer read GetSelCount;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    {$IFNDEF VER80}
    property ItemHint[Index: Integer]: string read GetItemHint write SetItemHint;
    {$ENDIF}
  published
    property TabStop default True;
    property HintSource: THPC_HintSource read FHintSource write FHintSource default hsDefault;
  end;

{ TRxCheckListBox }

  TCheckKind = (ckCheckBoxes, ckRadioButtons, ckCheckMarks);
  TChangeStateEvent = procedure(Sender: TObject; Index: Integer) of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCheckListBox = class(TRxCustomListBox)
  private
    FAllowGrayed: Boolean;
    FCheckKind: TCheckKind;
    FSaveStates: TList;
    FDrawBitmap: TBitmap;
    FCheckWidth, FCheckHeight: Integer;
    FReserved: Integer;
    FInUpdateStates: Boolean;
    FIniLink: TIniLink;
    FOnClickCheck: TNotifyEvent;
    FOnStateChange: TChangeStateEvent;
    procedure ResetItemHeight;
    function GetItemHeight: Integer; override;
    procedure DrawCheck(R: TRect; AState: TCheckBoxState; Enabled: Boolean);
    procedure SetCheckKind(Value: TCheckKind);
    procedure SetChecked(Index: Integer; AChecked: Boolean);
    function GetChecked(Index: Integer): Boolean;
    procedure SetState(Index: Integer; AState: TCheckBoxState);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    function GetAllowGrayed: Boolean;
    procedure ToggleClickCheck(Index: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure InvalidateItem(Index: Integer);
    function CreateCheckObject(Index: Integer): TObject;
    function FindCheckObject(Index: Integer): TObject;
    function GetCheckObject(Index: Integer): TObject;
    function IsCheckObject(Index: Integer): Boolean;
    procedure ReadVersion(Reader: TReader);
    procedure WriteVersion(Writer: TWriter);
    procedure ReadCheckData(Reader: TReader);
    procedure WriteCheckData(Writer: TWriter);
    procedure InternalSaveStates(IniFile: TObject; const Section: string);
    procedure InternalRestoreStates(IniFile: TObject; const Section: string);
    function GetStorage: TFormPlacement;
    procedure SetStorage(Value: TFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure UpdateCheckStates;
    function GetCheckedIndex: Integer;
    procedure SetCheckedIndex(Value: Integer);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    function CreateItemList: TStrings; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetItemWidth(Index: Integer): Integer; override;
    function GetItemData(Index: Integer): {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF}; override;
    procedure SetItemData(Index: Integer; AData: {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF}); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ResetContent; override;
    procedure DeleteString(Index: Integer); override;
    procedure ClickCheck; dynamic;
    procedure ChangeItemState(Index: Integer); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    function GetCheckWidth: Integer;
    procedure SetItems(Value: TStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFNDEF VER80}
    procedure SaveStatesReg(IniFile: TRegIniFile);
    procedure RestoreStatesReg(IniFile: TRegIniFile);
    {$ENDIF}
    procedure SaveStates(IniFile: TIniFile);
    procedure RestoreStates(IniFile: TIniFile);
    procedure ApplyState(AState: TCheckBoxState; EnabledOnly: Boolean);
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property EnabledItem[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
  published
    property AllowGrayed: Boolean read GetAllowGrayed write FAllowGrayed default False;
    property CheckKind: TCheckKind read FCheckKind write SetCheckKind default ckCheckBoxes;
    property CheckedIndex: Integer read GetCheckedIndex write SetCheckedIndex default -1;
    property IniStorage: TFormPlacement read GetStorage write SetStorage;
    property Align;
    property AutoScroll default True;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property GraySelection;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    {$IFNDEF VER80}
    {$IFNDEF RX_D9}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    {$ENDIF}
    property IntegralHeight;
    property ItemHeight;
    property Items stored False;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabWidth;
    property Visible;
    property OnStateChange: TChangeStateEvent read FOnStateChange write FOnStateChange;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetItemWidth;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFNDEF VER80}
    property OnStartDrag;
    property OnGetItemHint;
    {$ENDIF}
    {$IFDEF RX_D5}
    property OnContextPopup;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    {$IFDEF RX_D6}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property TabStop;
    {$IFDEF RX_D9}
    property OnMouseActivate;
    {$ENDIF}
    {$ENDIF}
  end;

const
  clbDefaultState = cbUnchecked;
  clbDefaultEnabled = True;

{ TRxCustomLabel }

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);
  {$IFNDEF RX_D3}
  TTextLayout = (tlTop, tlCenter, tlBottom);
  {$ENDIF}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCustomLabel = class(TGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FLayout: TTextLayout;
    FShadowColor: TColor;
    FShadowSize: Byte;
    FShadowPos: TShadowPosition;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FShowFocus: Boolean;
    FFocused: Boolean;
    FMouseInControl: Boolean;
    FDragging: Boolean;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure DoDrawText(var Rect: TRect; Flags: Word);
    function GetTransparent: Boolean;
    procedure UpdateTracking;
    procedure SetAlignment(Value: TAlignment);
    {$IFNDEF RX_D6} // Polaris
    procedure SetAutoSize(Value: Boolean);
    {$ENDIF}
    procedure SetFocusControl(Value: TWinControl);
    procedure SetLayout(Value: TTextLayout);
    procedure SetLeftMargin(Value: Integer);
    procedure SetRightMargin(Value: Integer);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowSize(Value: Byte);
    procedure SetShadowPos(Value: TShadowPosition);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure SetShowFocus(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    procedure AdjustBounds;
    {$IFDEF RX_D6} // Polaris
    procedure SetAutoSize(Value: Boolean); override;
    {$ENDIF}
    function GetDefaultFontColor: TColor; virtual;
    function GetLabelCaption: string; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 0;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 0;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnHighlight;
    property ShadowSize: Byte read FShadowSize write SetShadowSize default 1;
    property ShadowPos: TShadowPosition read FShadowPos write SetShadowPos default spLeftTop;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    property MouseInControl: Boolean read FMouseInControl;
  end;

{ TRxLabel }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxLabel = class(TRxCustomLabel)
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    property Layout;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor;
    property ShadowSize;
    property ShadowPos;
    property ShowAccelChar;
    property ShowFocus;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
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
    {$IFDEF RX_D9}
    property OnMouseActivate;
    {$ENDIF}
  end;

{ TSecretPanel }

  TGlyphLayout = (glGlyphLeft, glGlyphRight, glGlyphTop, glGlyphBottom);
  TScrollDirection = (sdVertical, sdHorizontal);
  TPanelDrawEvent = procedure(Sender: TObject; Canvas: TCanvas;
    Rect: TRect) of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSecretPanel = class(TCustomPanel)
  private
    FActive: Boolean;
    FAlignment: TAlignment;
    FLines: TStrings;
    FCycled: Boolean;
    FScrollCnt: Integer;
    FMaxScroll: Integer;
    FTxtDivider: Byte;
    FFirstLine: Integer;
    FTimer: TRxTimer;
    FTxtRect: TRect;
    FPaintRect: TRect;
    FGlyphOrigin: TPoint;
    FMemoryImage: TBitmap;
    FGlyph: TBitmap;
    FHiddenList: TList;
    FTextStyle: TPanelBevel;
    FDirection: TScrollDirection;
    FGlyphLayout: TGlyphLayout;
    FOnPaintClient: TPanelDrawEvent;
    FOnStartPlay: TNotifyEvent;
    FOnStopPlay: TNotifyEvent;
    {$IFDEF RX_D3}
    FAsyncDrawing: Boolean;
    procedure SetAsyncDrawing(Value: Boolean);
    {$ENDIF}
    function GetInflateWidth: Integer;
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure SetGlyph(Value: TBitmap);
    procedure SetLines(Value: TStrings);
    procedure SetActive(Value: Boolean);
    procedure SetAlignment(Value: TAlignment);
    procedure SetGlyphLayout(Value: TGlyphLayout);
    procedure SetTextStyle(Value: TPanelBevel);
    procedure SetDirection(Value: TScrollDirection);
    procedure RecalcDrawRect;
    procedure PaintGlyph;
    procedure PaintText;
    procedure UpdateMemoryImage;
    procedure GlyphChanged(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Paint; override;
    procedure PaintClient(Canvas: TCanvas; Rect: TRect); virtual;
    procedure TimerExpired(Sender: TObject); virtual;
    procedure StartPlay; dynamic;
    procedure StopPlay; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    property Canvas;
  published
    {$IFDEF RX_D3}
    property AsyncDrawing: Boolean read FAsyncDrawing write SetAsyncDrawing default True;
    {$ENDIF}
    property Active: Boolean read FActive write SetActive default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Cycled: Boolean read FCycled write FCycled default False;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphLayout: TGlyphLayout read FGlyphLayout write SetGlyphLayout
      default glGlyphLeft;
    property Interval: Cardinal read GetInterval write SetInterval default 30;
    property Lines: TStrings read FLines write SetLines;
    property ScrollDirection: TScrollDirection read FDirection write SetDirection
      default sdVertical;
    property TextStyle: TPanelBevel read FTextStyle write SetTextStyle default bvNone;
    {$IFDEF RX_D4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    property Align;
    property BevelInner;
    property BevelOuter default bvLowered;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnPaintClient: TPanelDrawEvent read FOnPaintClient write FOnPaintClient;
    property OnStartPlay: TNotifyEvent read FOnStartPlay write FOnStartPlay;
    property OnStopPlay: TNotifyEvent read FOnStopPlay write FOnStopPlay;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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
    property OnResize;
    {$IFDEF RX_D6}
    property AutoSize;
    property BevelEdges;
    property BevelKind;
    property Caption;
    property UseDockManager default True;
    property DockSite;
    property Enabled;
    property FullRepaint;
    property Locked;
    {$IFDEF RX_D7}
    property ParentBackground default False;
    {$ENDIF}
    {$IFDEF RX_D9}
    property VerticalAlignment;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    {$ENDIF}
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    {$IFDEF RX_D9}
    property OnMouseActivate;
    {$ENDIF}
    {$IFDEF RX_10}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnUnDock;
    {$ENDIF}
  end;

{ TRxSpeedButton }

  TRxNumGlyphs = 1..5;
  TRxDropDownMenuPos = (dmpBottom, dmpRight);
  TRxButtonState = (rbsUp, rbsDisabled, rbsDown, rbsExclusive, rbsInactive);

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxSpeedButton = class(TGraphicControl)
  private
    FGroupIndex: {$IFDEF RX_D15}WPARAM{$ELSE}Integer{$ENDIF};
    FStyle: TButtonStyle;
    FGlyph: Pointer;
    FDrawImage: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FModalResult: TModalResult;
    FTransparent: Boolean;
    FMarkDropDown: Boolean;
    FDropDownMenu: TPopupMenu;
    FMenuPosition: TRxDropDownMenuPos;
    FInactiveGrayed: Boolean;
    FMenuTracking: Boolean;
    FRepeatTimer: TTimer;
    FAllowTimer: Boolean;
    FInitRepeatPause: Word;
    FRepeatPause: Word;
    FThemedStyle: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TRxNumGlyphs;
    procedure SetNumGlyphs(Value: TRxNumGlyphs);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    function GetGroupIndex: Integer;
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetDropDownMenu(Value: TPopupMenu);
    procedure SetFlat(Value: Boolean);
    procedure SetStyle(Value: TButtonStyle);
    procedure SetInactiveGrayed(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetMarkDropDown(Value: Boolean);
    procedure TimerExpired(Sender: TObject);
    procedure SetAllowTimer(Value: Boolean);
    function CheckMenuDropDown(const Pos: TSmallPoint;
      Manual: Boolean): Boolean;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CMButtonPressed(var Message: TMessage); message CM_RXBUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TMessage); message WM_MOUSEMOVE;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
  protected
    FState: TRxButtonState;
    FFlatStandard: Boolean;
    procedure SetFlatStandard(Value: Boolean);
    {$IFDEF RX_D4}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    {$ENDIF}
    function GetDropDownMenuPos: TPoint;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure PaintGlyph(Canvas: TCanvas; ARect: TRect; AState: TRxButtonState;
      DrawMark: Boolean); virtual;
    procedure MouseEnter; dynamic;
    procedure MouseLeave; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property ButtonGlyph: Pointer read FGlyph;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick;
    function CheckBtnMenuDropDown: Boolean;
    procedure Click; override;
    procedure UpdateTracking;
    property ThemedStyle: Boolean read FThemedStyle write FThemedStyle default True;
  published
    property FlatStandard: Boolean read FFlatStandard write SetFlatStandard default False;
    {$IFDEF RX_D4}
    property Action;
    property Anchors;
    property Align;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    {$ENDIF}
    property Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property AllowTimer: Boolean read FAllowTimer write SetAllowTimer default False;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    { Ensure group index is declared before Down }
    property Down: Boolean read FDown write SetDown default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property MenuPosition: TRxDropDownMenuPos read FMenuPosition write FMenuPosition
      default dmpBottom;
    property Caption;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GrayedInactive: Boolean read FInactiveGrayed write SetInactiveGrayed
      default True;
    property InitPause: Word read FInitRepeatPause write FInitRepeatPause default 500;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphTop;
    property Margin: Integer read FMargin write SetMargin default -1;
    property MarkDropDown: Boolean read FMarkDropDown write SetMarkDropDown default True;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property NumGlyphs: TRxNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint default False;
    property RepeatInterval: Word read FRepeatPause write FRepeatPause default 100;
    property ShowHint default True;
    property Spacing: Integer read FSpacing write SetSpacing default 1;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property Visible;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFNDEF VER80}
    property OnStartDrag;
    {$ENDIF}
    {$IFDEF RX_D4}
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    {$IFDEF RX_D9}
    property OnMouseActivate;
    {$ENDIF}
  end;

{ TButtonImage }

  TButtonImage = class(TObject)
  private
    FGlyph: TObject;
    FButtonSize: TPoint;
    FCaption: TCaption;
    function GetNumGlyphs: TRxNumGlyphs;
    procedure SetNumGlyphs(Value: TRxNumGlyphs);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    {$IFNDEF VER80}
    procedure DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
      Layout: TButtonLayout; AFont: TFont; Images: TImageList;
      ImageIndex: Integer; Flags: Word); {$IFDEF RX_D9}inline; {$ENDIF}
    {$ENDIF}
    procedure Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
      Layout: TButtonLayout; AFont: TFont; Flags: Word); {$IFDEF RX_D9}inline; {$ENDIF}
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read FCaption write FCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TRxNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property ButtonSize: TPoint read FButtonSize write FButtonSize;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
  end;

{ TRxButtonGlyph }

  TRxButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TImageList;
    FIndexs: array[TRxButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TRxNumGlyphs;
    FWordWrap: Boolean;
    FAlignment: TAlignment;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TRxNumGlyphs);
    function MapColor(Color: TColor): TColor;
  protected
    procedure MinimizeCaption(Canvas: TCanvas; const Caption: string;
      Buffer: PChar; MaxLen, Width: Integer);
    function CreateButtonGlyph(State: TRxButtonState): Integer;
    {$IFNDEF VER80}
    function CreateImageGlyph(State: TRxButtonState; Images: TImageList;
      Index: Integer): Integer;
    {$ENDIF}
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect;
      Flags: Word{$IFNDEF VER80}; Images: TImageList; ImageIndex: Integer
      {$ENDIF});
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    function DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TRxButtonState): TPoint;
    {$IFNDEF VER80}
    function DrawButtonImage(Canvas: TCanvas; X, Y: Integer; Images: TImageList;
      ImageIndex: Integer; State: TRxButtonState): TPoint;
    function DrawEx(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; PopupMark: Boolean;
      Images: TImageList; ImageIndex: Integer; State: TRxButtonState;
      Flags: Word): TRect;
    {$ENDIF}
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TRxButtonState; Flags: Word);
    procedure DrawPopupMark(Canvas: TCanvas; X, Y: Integer;
      State: TRxButtonState);
    function Draw(Canvas: TCanvas; const Client: TRect; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; PopupMark: Boolean;
      State: TRxButtonState; Flags: Word): TRect;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TRxNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TStyle = (vsNormal, vsFlat, vsPushButton);
  {$IFNDEF RX_D9}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$ENDIF}
  {  TRxCheckBox  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCheckBox = class(TCheckBox)
  private
    FHorzAlign: TAlignment;
    FVertAlign: TVerticalAlignment;
    FWordWrap: Boolean;
    FStyle: TStyle;
    procedure SetHorzAlign(Value: TAlignment);
    procedure SetVertAlign(Value: TVerticalAlignment);
    procedure SetWordWrap(Value: Boolean);
    procedure SetStyle(Value: TStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HorizontalAlignment: TAlignment read FHorzAlign write SetHorzAlign;
    property VerticalAlignment: TVerticalAlignment read FVertAlign write SetVertAlign;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Style: TStyle read FStyle write SetStyle;
  end;

  {  TRxRadioButton  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxRadioButton = class(TRadioButton)
  private
    FHorzAlign: TAlignment;
    FVertAlign: TVerticalAlignment;
    FWordWrap: Boolean;
    FStyle: TStyle;
    procedure SetHorzAlign(Value: TAlignment);
    procedure SetVertAlign(Value: TVerticalAlignment);
    procedure SetWordWrap(Value: Boolean);
    procedure SetStyle(Value: TStyle);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HorizontalAlignment: TAlignment read FHorzAlign write SetHorzAlign;
    property VerticalAlignment: TVerticalAlignment read FVertAlign write SetVertAlign;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Style: TStyle read FStyle write SetStyle;
  end;

  {  TAnimOrientation  }

{ image must be oriented like
  +--+--+--+--+
  |  |  |  |  |
  +--+--+--+--+
  or
  +--+
  |  |
  +--+
  |  |
  +--+
  |  |
  +--+
  |  |
  +--+
  but not (cannot be automatically recognized number of subpictures)
  +--+--+
  |  |  |
  +--+--+
  |  |  |
  +--+--+
  NOTE:
    Image of AnimGlyph must be squared and the same proportional size as GLYPH.
}

  TRxAnimGlyphsOrientation = (agoHorizontal, agoVertical);

  {  TRxAnimBitBtn  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxAnimBitBtn = class(TBitBtn)
  private
    { Private declarations }
    FAnimGlyph: TBitmap;
    FTempGlyph: TBitmap;
    FSparkle: TBitmap;
    FTimer: TTimer;
    FAnimCount: Integer;
    FInterval: Integer;
    FAnimated: Boolean;
    FRxAnimGlyphsOrientation: TRxAnimGlyphsOrientation;
    FSubImageIndex: Integer;
    FOnFinishAnim: TNotifyEvent;
    procedure SetAnimGlyph(Value: TBitmap);
    procedure SetAnimated(Value: Boolean);
    procedure SetInterval(Value: Integer);
    procedure SetAnimOrientation(Value: TRxAnimGlyphsOrientation);
    procedure CheckBitmapSizeOf(Value: TBitmap);
  protected
    { Protected declarations }
    dX, dY: Integer;
    procedure TimerEvent(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AnimGlyph: TBitmap read FAnimGlyph write SetAnimGlyph;
    property Animated: Boolean read FAnimated write SetAnimated;
    property Interval: Integer read FInterval write SetInterval;
    property AnimOrientation: TRxAnimGlyphsOrientation read FRxAnimGlyphsOrientation write SetAnimOrientation;
    property OnFinishAnim: TNotifyEvent read FOnFinishAnim write FOnFinishAnim;
  end;

  {  TRxAnimSpeedButton  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxAnimSpeedButton = class(TSpeedButton)
  private
    { Private declarations }
    FAnimGlyph: TBitmap;
    FTempGlyph: TBitmap;
    FSparkle: TBitmap;
    FTimer: TTimer;
    FAnimCount: Integer;
    FInterval: Integer;
    FAnimated: Boolean;
    FRxAnimGlyphsOrientation: TRxAnimGlyphsOrientation;
    FSubImageIndex: Integer;
    FOnFinishAnim: TNotifyEvent;
    procedure SetAnimGlyph(Value: TBitmap);
    procedure SetAnimated(Value: Boolean);
    procedure SetInterval(Value: Integer);
    procedure SetAnimOrientation(Value: TRxAnimGlyphsOrientation);
    procedure CheckBitmapSizeOf(Value: TBitmap);
  protected
    { Protected declarations }
    dX, dY: Integer;
    procedure TimerEvent(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AnimGlyph: TBitmap read FAnimGlyph write SetAnimGlyph;
    property Animated: Boolean read FAnimated write SetAnimated;
    property Interval: Integer read FInterval write SetInterval;
    property AnimOrientation: TRxAnimGlyphsOrientation read FRxAnimGlyphsOrientation write SetAnimOrientation;
    property OnFinishAnim: TNotifyEvent read FOnFinishAnim write FOnFinishAnim;
  end;

  {  TRxStatusPanelBinder  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxStatusPanelBinder = class(TComponent)
  private
    FStatusBar: TStatusBar;
    FControl: TControl;
    FControlParent: TWinControl;
    FControlBoundsRect: TRect;
    FOldOnDrawPanel: TDrawPanelEvent;
    FPanelIndex: Integer;
    FPanelStyle: TStatusPanelStyle;
    FControlVisible: Boolean;
    procedure SetStatusBar(Value: TStatusBar);
    procedure SetControl(Value: TControl);
    procedure FOnDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure SetPanelIndex(Value: Integer);
    procedure SetPanelStyle;
    procedure PushControl;
    procedure PopControl;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StatusBar: TStatusBar read FStatusBar write SetStatusBar;
    property Control: TControl read FControl write SetControl;
    property PanelIndex: Integer read FPanelIndex write SetPanelIndex;
  end;

  {  TRxProgress  }

  TDrawStyle = (dsNormal, dsInvert);
  TTextUAlign = (tuaCenter, tuaLeft, tuaRight, tuaFlat);
  TBorderUStyle = (busNone, busShallow, busDeep);
  TGetTextEvent = procedure(Position, Percent: Integer; var Text: string)
    of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxProgress = class(TCustomControl)
  private
    FPerc: Integer;
    FMin: Integer;
    FMax: Integer;
    FPos: Integer;
    FPosition: Integer;
    FPColor: TColor;
    FTransparent: Boolean;
    FText: string;
    FTextAlign: TTextUAlign;
    FDrawStyle: TDrawStyle;
    FCtl3D: Boolean;
    FIndent: Integer;
    FShowPos: Boolean;
    FShowPer: Boolean;
    FBorderStyle: TBorderUStyle;
    FShowAllFlag: Boolean;
    FProgressGradient: TRxGradient;
    FOnGetText: TGetTextEvent;
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetPColor(Value: TColor);
    procedure SetText(Value: string);
    procedure SetTextAlign(Value: TTextUAlign);
    procedure SetDrawStyle(Value: TDrawStyle);
    procedure SetCtl3D(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetShowPos(Value: Boolean);
    procedure SetShowPer(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderUStyle);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Percent: Integer read FPerc;
  published
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property Position: Integer read FPosition write SetPosition;
    property ProgressColor: TColor read FPColor write SetPColor;
    property Gradient: TRxGradient read FProgressGradient write FProgressGradient;
    property Text: string read FText write SetText;
    property ParentColor default False;
    {$IFDEF RX_D7}
    property ParentBackground default False;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property PopupMenu;
    property BorderStyle: TBorderUStyle read FBorderStyle write SetBorderStyle default busShallow;
    property Color default clBtnFace;
    property Font;
    property Align;
    property TextAlign: TTextUAlign read FTextAlign write SetTextAlign default tuaCenter;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property DrawStyle: TDrawStyle read FDrawStyle write SetDrawStyle default dsInvert;
    property Ctl3D: Boolean read FCtl3D write SetCtl3D;
    property Indent: Integer read FIndent write SetIndent default 0;
    property ShowPosition: Boolean read FShowPos write SetShowPos;
    property ShowPercent: Boolean read FShowPer write SetShowPer;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnEnter;
    property OnExit;
    {$IFDEF RX_D4}
    property Anchors;
    property Constraints;
    property OnResize;
    {$ENDIF}
  end;

  {  TRxColorButton  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxColorButton = class(TButton)
  private
    ShowBackColor  : Boolean;
    FCanvas        : TCanvas;
    IsFocused      : Boolean;
    FBackColor     : TColor;
    FForeColor     : TColor;
    FHoverColor    : TColor;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message : TMessage); override;

    procedure SetButtonStyle(Value: Boolean); override;
    procedure DrawButton(Rect: TRect; State: UINT);

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackColor : TColor read FBackColor  write SetBackColor default clBtnFace;
    property ForeColor : TColor read FForeColor  write SetForeColor default clBtnText;
    property HoverColor: TColor read FHoverColor write SetHoverColor default clBtnFace;
  end;

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer; {$IFDEF RX_D9}inline; {$ENDIF}

function CheckBitmap: TBitmap;

type
  TRxBevelStyle = (bsRxLowered, bsRxRaised);
  TRxBevelShape = (bsRxNone, bsRxBox, bsRxFrame, bsRxTopLine, bsRxBottomLine, bsRxLeftLine,
    bsRxRightLine, bsRxSpacer);

  {  TRxPanel  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxPanel = class(TPanel)
  private
    FBackground: TPicture;
    FGradient: TRxGradient;
    FTileImage: Boolean;
    FBevelStyle: TRxBevelStyle;
    FBevelShape: TRxBevelShape;
    {$IFNDEF RX_D9}
    FVerticalAlignment: TVerticalAlignment;
    {$ENDIF}
    FBlotter: Boolean;
    procedure PictureChanged(ASender: TObject);
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure SetBackground(const Value: TPicture);
    procedure SetBevelShape(const Value: TRxBevelShape);
    procedure SetBevelStyle(const Value: TRxBevelStyle);
    procedure SetBlotter(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure DoChanges(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFNDEF RX_D9}
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write FVerticalAlignment;
    {$ENDIF}
    property BackgroundImage: TPicture read FBackground write SetBackground;
    property BevelStyle: TRxBevelStyle read FBevelStyle write SetBevelStyle default bsRxRaised;
    property BevelShape: TRxBevelShape read FBevelShape write SetBevelShape default bsRxNone;
    property Blotter: Boolean read FBlotter write SetBlotter default False;
    property Gradient: TRxGradient read FGradient write FGradient;
    property TileImage: Boolean read FTileImage write FTileImage;
  end;

implementation

{$R *.RES}

uses
  SysUtils, Dialogs, {$IFNDEF VER80}CommCtrl, {$ELSE}RxStr16, {$ENDIF}
  RxVCLUtils, RxMaxMin, Consts, RxAppUtils{$IFDEF RX_D4}, ImgList, // Polaris
  ActnList{$ENDIF}
  {$IFDEF RX_D6}, RTLConsts, Types{$ENDIF}, RxResConst; // Polaris

{  TRxPanel  }

procedure TRxPanel.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Self.Repaint;
end;

procedure TRxPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Self.Repaint;
end;

constructor TRxPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvNone;
  FTileImage := False;
  FBackground := TPicture.Create;
  FBackground.OnChange := PictureChanged;
  FGradient := TRxGradient.Create;
  FGradient.OnChange := DoChanges;
  FBevelStyle := bsRxRaised;
  FBevelShape := bsRxNone;
  FBlotter := False;
end;

destructor TRxPanel.Destroy;
begin
  FGradient.Visible := False;
  FGradient.Free;
  FBackground.Free;
  inherited Destroy;
end;

procedure TRxPanel.DoChanges(Sender: TObject);
begin
  Self.Invalidate;
end;

procedure TRxPanel.Paint;
const
  XorColor = $00FFD8CE;
var
  Color1, Color2: TColor;
  Temp: TColor;

  procedure BevelRect(const R: TRect);
  begin
    with Canvas do
    begin
      Pen.Color := Color1;
      PolyLine([Point(R.Left, R.Bottom), Point(R.Left, R.Top),
        Point(R.Right, R.Top)]);
      Pen.Color := Color2;
      PolyLine([Point(R.Right, R.Top), Point(R.Right, R.Bottom),
        Point(R.Left, R.Bottom)]);
    end;
  end;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

  procedure DoDrawBevel();
  begin
    //copy of standard tbevel
    if FBevelShape <> bsRxNone then
      with Canvas do
      begin
        Pen.Width := 1;

        if FBevelStyle = bsRxLowered then
        begin
          Color1 := clBtnShadow;
          Color2 := clBtnHighlight;
        end
        else
        begin
          Color1 := clBtnHighlight;
          Color2 := clBtnShadow;
        end;

        case FBevelShape of
          bsRxBox:
            BevelRect(Rect(0, 0, Width - 1, Height - 1));
          bsRxFrame:
            begin
              Temp := Color1;
              Color1 := Color2;
              BevelRect(Rect(1, 1, Width - 1, Height - 1));
              Color2 := Temp;
              Color1 := Temp;
              BevelRect(Rect(0, 0, Width - 2, Height - 2));
            end;
          bsRxTopLine:
            begin
              BevelLine(Color1, 0, 0, Width, 0);
              BevelLine(Color2, 0, 1, Width, 1);
            end;
          bsRxBottomLine:
            begin
              BevelLine(Color1, 0, Height - 2, Width, Height - 2);
              BevelLine(Color2, 0, Height - 1, Width, Height - 1);
            end;
          bsRxLeftLine:
            begin
              BevelLine(Color1, 0, 0, 0, Height);
              BevelLine(Color2, 1, 0, 1, Height);
            end;
          bsRxRightLine:
            begin
              BevelLine(Color1, Width - 2, 0, Width - 2, Height);
              BevelLine(Color2, Width - 1, 0, Width - 1, Height);
            end;
        end;
      end;
  end;

  procedure DoDrawBorderLimit(cBackColor: TColor = clGreen; cLine: TColor = clBlack; cLineShadow: TColor = clSilver;
    cCorner: TColor = clYellow; cCornerShadow: TColor = clOlive);
  var
    CanDrawBackground: Boolean;
  begin
    if not FBlotter then Exit;

    {draw on the canvas directly}
    with Canvas do
    begin
      if (Assigned(FGradient) and FGradient.Visible) or
        (Assigned(FBackground) and (FBackground.Graphic <> nil) and (not FBackground.Graphic.Empty)) then
        CanDrawBackground := False
      else
        CanDrawBackground := True;
      if CanDrawBackground then
      begin
        Brush.Color := cBackColor;
        Rectangle(0, 0, Width, Height);
      end;

    {**************************************************}
      {draw vertical lines on left side of form}
      Pen.Color := cLine;
      Moveto(0, 0); {column,row}
      Lineto(0, Height);

      Pen.Color := cLineShadow;
      Moveto(0 + 1, 0); {column,row}
      Lineto(0 + 1, Height);

      Pen.Color := cLine;
      Moveto(0 + 4, 0); {column,row}
      Lineto(0 + 4, Height);

      {draw vertical line on right side of form}
      Pen.Color := cLineShadow;
      Moveto(Width - 4, 0);
      Lineto(Width - 4, Height);

      Pen.Color := cLine;
      Moveto(Width - 1, 0);
      Lineto(Width - 1, Height);

      {draw horizontal line on top side of form}
      Pen.Color := cLine;
      Moveto(0, 0);
      Lineto(Width, 0);

      Pen.Color := cLineShadow;
      Moveto(0, 0 + 1);
      Lineto(Width, 0 + 1);

      Pen.Color := cLine;
      Moveto(0, 0 + 4);
      Lineto(Width, 0 + 4);

      {draw horizontal line on bottom side of form}
      Pen.Color := cLineShadow;
      Moveto(0, Height - 4);
      Lineto(Width, Height - 4);

      Pen.Color := cLine;
      Moveto(0, Height - 1);
      Lineto(Width, Height - 1);

    {***************************************************}
      {draw blotter outer corners}
      Pen.Color := cCorner;
      {Upper Left vertical and horizontal}
      MoveTo(0 + 1, 0 + 1);
      LineTo(0 + 1, 15);
      Moveto(0 + 1, 0 + 1);
      LineTo(15, 0 + 1);
      {Lower Left vertical only }
      MoveTo(0 + 1, Height - 1);
      LineTo(0 + 1, Height - 16);
      {Lower Right}
      Pen.Color := cLine;
      MoveTo(Width - 2, Height - 1);
      LineTo(Width - 15, Height - 1);
      MoveTo(Width - 1, Height - 1);
      LineTo(Width - 1, Height - 15);
      Pen.Color := cCorner;
      MoveTo(Width - 15, Height - 1);
      LineTo(Width - 16, Height - 1);
      MoveTo(Width - 1, Height - 15);
      LineTo(Width - 1, Height - 16);
      {Upper Right, horizontal only}
      MoveTo(Width - 15, 1);
      LineTo(Width - 1, 1);

    {************************************************}
      {draw blotter inner corners}
      Pen.Color := cLine;
      Brush.Color := cLine;
      {Upper Left}
      MoveTo(0 + 5, 0 + 5);
      LineTo(0 + 5, 6 + 6);
      Moveto(0 + 5, 0 + 5);
      LineTo(6 + 6, 0 + 5);

      {Lower Left}
      MoveTo(0 + 5, Height - 5);
      LineTo(0 + 5, (Height - 5) - 7); {draw vert}
      Moveto(0 + 5, Height - 5);
      LineTo(12, Height - 5); {draw horiz}

      Pen.Color := cCorner;
      MoveTo(0 + 6, Height - 5);
      LineTo(11, Height - 5);
      Pen.Color := cLine;

      {lower right}
      Pen.Color := cCorner;
      MoveTo(Width - 5, Height - 5);
      LineTo(Width - 5, Height - 12);
      MoveTo(Width - 5, Height - 5);
      LineTo(Width - 12, Height - 5);

      {Upper Right}
      Pen.Color := cLine;
      MoveTo(Width - 11, 5);
      LineTo(Width - 5, 5);
      Pen.Color := cCorner;
      MoveTo(Width - 5, 5);
      LineTo(Width - 5, 13);

    {************************************************}
      {draw the staircase pixels}
      Pen.Color := cLine;

      {upper left}
      {lower pixels}
      MoveTo(0 + 1, 15);
      LineTo(0 + 4, 12);

      Moveto(2, Height - 13);
      LineTo(3, Height - 12);
      Moveto(4, Height - 11);
      LineTo(4, Height - 11);

      {upper pixels}
      MoveTo(15, 0 + 1);
      LineTo(12, 0 + 4);


      {lower left}
      {upper pixels}
      Pen.Color := cCorner;
      Moveto(2, Height - 14);
      LineTo(5, Height - 11);

      Pen.Color := cLine;
      MoveTo(11, Height - 5);
      LineTo(15, Height - 1);

      {lower right}
      Pen.Color := cCorner;
      MoveTo(Width - 15, Height - 1);
      LineTo(Width - 10, Height - 6);
      MoveTo(Width - 1, Height - 15);
      LineTo(Width - 6, Height - 10);

      { Upper Right}
      Pen.Color := cLine;
      MoveTo(Width - 1, 16);
      LineTo(Width - 5, 12);

      MoveTo(Width - 14, 2);
      LineTo(Width - 12, 4);

    {****************************************************}
      {fill in "brass" areas for corners}
      Brush.Color := cCornerShadow;
      Pen.Color := cCornerShadow;

      {upper left}
      {fill in large areas}
      Rectangle(2, 2, 5, 12);
      Rectangle(2, 2, 12, 5);

      {fill in upper pixels}
      Moveto(12, 2);
      LineTo(14, 2);
      Moveto(12, 3);
      LineTo(13, 3);
      {fill in lower pixels}
      MoveTo(2, 12);
      LineTo(2, 14);
      MoveTo(3, 12);
      LineTo(3, 13);

    {------------------------}
      {lower left}
      {fill in large areas}
      Rectangle(2, Height - 1, 12, Height - 4);
      Rectangle(2, Height - 2, 5, Height - 11);

      {fill in upper pixels}
      Moveto(2, Height - 13);
      LineTo(3, Height - 12);
      Moveto(2, Height - 12);
      LineTo(4, Height - 12);
      Moveto(4, Height - 11);
      LineTo(4, Height - 11);
      {fill in lower pixels}
      MoveTo(12, Height - 3);
      LineTo(13, Height - 2);
      MoveTo(14, Height - 1);
      LineTo(14, Height - 1);
      MoveTo(12, Height - 2);
      LineTo(14, Height - 2);

    {-----------------------}
      {lower right}

      {fill in large areas}
      Rectangle(Width - 1, Height - 1, Width - 11,
        Height - 4);
      Rectangle(Width - 1, Height - 1, Width - 4, Height - 11);

      {fill in upper pixels}
      MoveTo(Width - 3, Height - 12);
      LineTo(Width - 1, Height - 12);
      MoveTo(Width - 2, Height - 13);
      LineTo(Width - 1, Height - 13);

      {fill in lower pixels}
      MoveTo(Width - 12, Height - 3);
      LineTo(Width - 12, Height - 1);
      MoveTo(Width - 13, Height - 2);
      LineTo(Width - 13, Height - 1);

    {-----------------------}
      {upper right}

      {fill in large areas}
      Rectangle(Width - 11, 2, Width - 1, 5);
      Rectangle(Width - 1, 13, Width - 4, 2);

      {fill in upper pixels}
      MoveTo(Width - 12, 2);
      LineTo(Width - 12, 4);
      MoveTo(Width - 13, 2);
      LineTo(Width - 13, 1);

      {fill in lower pixels}
      MoveTo(Width - 2, 13);
      LineTo(Width - 4, 13);
      MoveTo(Width - 2, 14);
      LineTo(Width - 1, 14);

    {***************************************************}
      {cleanup corner pixels}
      Pen.Color := cLine;
      Moveto(0, 0);
      LineTo(0, 10);

      {Lower Left}
      MoveTo(0, Height - 1);
      LineTo(13, Height - 1);
      MoveTo(0, Height - 1);
      LineTo(0, Height - 14);

      {Upper Right}
      Moveto(Width - 1, 0);
      LineTo(Width - 14, 0);
      Moveto(Width - 1, 0);
      LineTo(Width - 1, 13);

      {Lower Right}
      MoveTo(Width - 1, Height - 1);
      LineTo(Width - 14, Height - 1);
      MoveTo(Width - 1, Height - 1);
      LineTo(Width - 1, Height - 14);

    end;
  end;

  procedure DoDrawText;
  const
    Alignments: array[TAlignment] of LongInt = (DT_LEFT, DT_RIGHT, DT_CENTER);
    ccVerticalAlignment: array[TVerticalAlignment] of LongInt = (DT_TOP, DT_BOTTOM, DT_VCENTER);
  var
    Flags: LongInt;
    R: TRect;
  begin
    if Caption <> '' then
      with Canvas do
      begin
        Canvas.Font.Assign(Self.Font);
        R := GetClientRect;
        Flags := DT_EXPANDTABS or DT_SINGLELINE or
          ccVerticalAlignment[VerticalAlignment] or Alignments[Alignment];
        Flags := DrawTextBiDiModeFlags(Flags);
        Brush.Style := bsClear;
        DrawText(Handle, PChar(Caption), -1, R, Flags);
      end;
  end;
begin
  if Assigned(FGradient) and FGradient.Visible then
  begin
    FGradient.Draw(Self.Canvas, Self.ClientRect);
    DoDrawBevel;
    DoDrawBorderLimit();
    DoDrawText;
  end
  else if Assigned(FBackground) and (FBackground.Graphic <> nil) and (not FBackground.Graphic.Empty) then
  begin
    if FTileImage then
      RxGraph.TileImage(Canvas, ClientRect, FBackground.Graphic)
    else
      Canvas.StretchDraw(ClientRect, FBackground.Graphic);
    DoDrawBevel;
    DoDrawBorderLimit();
    DoDrawText;
  end
  else
  begin
    inherited;
    DoDrawBorderLimit(Color);
  end;

  if (csDesigning in ComponentState) and (BevelInner = bvNone) and (BevelOuter = bvNone) then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TRxPanel.PictureChanged(ASender: TObject);
begin
  Invalidate;
end;

procedure TRxPanel.SetBackground(const Value: TPicture);
begin
  FBackground := Value;
end;

procedure TRxPanel.SetBevelShape(const Value: TRxBevelShape);
begin
  FBevelShape := Value;
  Invalidate;
end;

procedure TRxPanel.SetBevelStyle(const Value: TRxBevelStyle);
begin
  FBevelStyle := Value;
  Invalidate;
end;

procedure TRxPanel.SetBlotter(const Value: Boolean);
begin
  FBlotter := Value;
  Invalidate;
end;

procedure TRxPanel.WMEraseBkGnd(var Message: TMessage);
begin
  if Assigned(FGradient) and FGradient.Visible then
  begin
    Message.Result := 1;
  end
  else if Assigned(FBackground) and (FBackground.Graphic <> nil) and (not FBackground.Graphic.Empty) then
  begin
    Message.Result := 1;
  end
  else
    inherited;
end;

{  TRxProgress  }

constructor TRxProgress.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF RX_D7}
  if not (csParentBackground in ControlStyle) then
    ControlStyle := ControlStyle + [csParentBackground];
  {$ENDIF}
  FProgressGradient := TRxGradient.Create;
  FProgressGradient.Visible := False;
  Width := 150;
  Height := 19;
  ParentColor := False;
  FTransparent := False;
  {$IFDEF RX_D7}
  ParentBackground := False;
  {$ENDIF}
  FPerc := 0;
  FMin := 0;
  FMax := 100;
  FPos := 0;
  FPosition := 0;
  FPColor := clHighlight;
  FTextAlign := tuaCenter;
  FDrawStyle := dsInvert;
  FCtl3D := True;
  FIndent := 0;
  FShowPos := True;
  FShowPer := True;
  FBorderStyle := busShallow;
  Color := clBtnFace;
  FShowAllFlag := True;
  FOnGetText := nil;
end;

procedure TRxProgress.SetText(Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetMax(Value: Integer);
begin
  if Value <> FMax then
  begin
    if Value <= FMin then
      Value := FMin + 1;
    FMax := Value;
    if FPosition > FMax then
      FPosition := FMax;
    FShowAllFlag := False;
    Paint;
  end;
end;

procedure TRxProgress.SetPosition(Value: Integer);
begin
  if Value <> FPosition then
  begin
    if Value < FMin then
      Value := FMin;
    if Value > FMax then
      Value := FMax;
    FPosition := Value;
    FShowAllFlag := False;
    Paint;
  end;
end;

procedure TRxProgress.SetPColor(Value: TColor);
begin
  if Value <> FPColor then
  begin
    FPColor := Value;
    Paint;
  end;
end;

procedure TRxProgress.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

destructor TRxProgress.Destroy;
begin
  FProgressGradient.Free;
  inherited;
end;

procedure TRxProgress.Paint;

function DiameterColor(Color1, Color2: TColor): TColor; {$IFDEF RX_D9}inline; {$ENDIF}
  var //JB.
    r, g, b: Byte;
  begin
    r := (GetRValue(ColorToRGB(Color1)) + GetRValue(ColorToRGB(Color2))) div 2; // R
    g := (GetGValue(ColorToRGB(Color1)) + GetGValue(ColorToRGB(Color2))) div 2; // G
    b := (GetBValue(ColorToRGB(Color1)) + GetBValue(ColorToRGB(Color2))) div 2; // B
    Result := RGB(r, g, b);
  end;

  function GetTextColor: TColor;
  var
    OK: Boolean;
  begin
    OK := False;
    if FMax > 0 then
      OK := (FPosition / FMax) > 0.5;
    if FProgressGradient.Visible then
    begin
      if OK then
        Result := ContrastColor(DiameterColor(FProgressGradient.StartColor, FProgressGradient.EndColor))
      else
        Result := ContrastColor(Color);
    end
    else
    begin
      if OK then
        Result := ContrastColor(ProgressColor)
      else
        Result := ContrastColor(Color);
    end;
  end;

  function GetBorderSize: Integer;
  begin
    case FBorderStyle of
      busNone:
        Result := 0;
      busShallow:
        Result := 1;
      busDeep:
        if FCtl3D then
          Result := 2
        else
          Result := 1;
    else
      Result := 0;
    end;
  end;

  procedure PaintToBmp(lBMP: TBitmap; Bgr: TBitmap = nil);
  var
    w, h, deep: Integer;
    col: TColor;
    i, x, y: Integer;
    bmpText: TBitmap;
    s, s1: string;
  begin
    deep := GetBorderSize;
    w := Width;
    h := Height;
    lBMP.Width := w;
    lBMP.Height := h;
    if Assigned(Bgr) then
      lBMP.Canvas.Draw(0, 0, Bgr)
    else
    begin
      lBMP.Canvas.Brush.Style := bsSolid;
      lBMP.Canvas.Brush.Color := Self.Color;
      lBMP.Canvas.FillRect(Bounds(0, 0, w, h));
    end;
    lBMP.Canvas.Font.Assign(Font);
    //with lBMP.Canvas do
    begin
      if Assigned(FOnGetText) then
        FOnGetText(FPosition, FPerc, s)
      else
      begin
        s1 := '';
        if FShowPos then
        begin
          if FShowPer then
            s1 := Format('%d%%', [FPerc])
          else
            s1 := Format('%d', [FPosition]);
        end;
        if FText <> '' then
          s := Format('%s   %s', [FText, s1])
        else
          s := s1;
      end;
      i := lBMP.Canvas.TextWidth(s);
      case FTextAlign of
        tuaLeft:
          x := 4;
        tuaRight:
          x := w - i - 4;
        tuaCenter:
          x := (w - i) div 2;
        tuaFlat:
          begin
            x := (FPos - i) div 2;
            if x < 4 then
              x := 4;
            x := x + FIndent;
          end;
      else
        x := 4;
      end;
      y := (h - lBMP.Canvas.TextHeight(s)) div 2;
      col := GetTextColor;
      if FDrawStyle = dsInvert then
      begin
        { inverted ProgressBar for better readable }
        bmpText := TBitmap.Create;
        try
          bmpText.Width := lBMP.Width;
          bmpText.Height := lBMP.Height;
          bmpText.Canvas.Font.Assign(Font);
          i := FIndent + deep;
//          col := $80;
//          while (col = ColorToRGB(Color)) or (col = ColorToRGB(FPColor)) do
//            col := col shl 8;
          with bmpText.Canvas do
            if FProgressGradient.Visible and (FProgressGradient.StartColor <> FProgressGradient.EndColor) then
            begin
              col := GetTextColor;
              if Assigned(Bgr) then
                Canvas.Draw(0, 0, Bgr)
              else
              begin
                Brush.Style := bsSolid;
                Brush.Color := Color;
                FillRect(Bounds(0, 0, w, h));
              end;
              if FPos > 0 then
                FProgressGradient.Draw(bmpText.Canvas, Rect(i, i, i + FPos, h - i));
              Brush.Color := Color;
              Brush.Style := bsClear;
              Font.Color := col;
              TextOut(x, y, s);
            end
            else
            begin
              if Assigned(Bgr) then
                Canvas.Draw(0, 0, Bgr)
              else
              begin
                Brush.Style := bsSolid;
                Brush.Color := Color;
                FillRect(Bounds(0, 0, w, h));
              end;
              Brush.Color := FPColor;
              if FPos > 0 then
                FillRect(Rect(i, i, i + FPos, h - i));
              Brush.Style := bsClear;
              Font.Color := col;
              TextOut(x, y, s);
            end;
          lBMP.Canvas.Brush.Color := Color;
          lBMP.Canvas.BrushCopy(Rect(0, 0, i + FPos, h), bmpText, Rect(0, 0, i + FPos, h), col);
          lBMP.Canvas.Brush.Color := Font.Color;
          lBMP.Canvas.BrushCopy(Rect(i + FPos, 0, w, h), bmpText, Rect(i + FPos, 0, w, h), col);
        finally
          bmpText.Free;
        end;
      end
      else
      begin
        { like normal ProgressBar }
        if Assigned(Bgr) then
          lBMP.Canvas.Draw(0, 0, Bgr)
        else
        begin
          lBMP.Canvas.Brush.Style := bsSolid;
          lBMP.Canvas.Brush.Color := Color;
          lBMP.Canvas.FillRect(Rect(0, 0, w, h));
        end;
        i := FIndent + deep;
        if FProgressGradient.Visible and (FProgressGradient.StartColor <> FProgressGradient.EndColor) then
        begin
          col := GetTextColor;
          if FPos > 0 then
            FProgressGradient.Draw(lBMP.Canvas, Rect(i, i, i + FPos, h - i));
        end
        else
        begin
          lBMP.Canvas.Brush.Color := FPColor;
          if FPos > 0 then
            lBMP.Canvas.FillRect(Rect(i, i, i + FPos, h - i));
        end;
        lBMP.Canvas.Brush.Style := bsClear;
        lBMP.Canvas.Font.Color := col;
        lBMP.Canvas.TextOut(x, y, s);
      end;
      { border }
      if FBorderStyle <> busNone then
      begin
        lBMP.Canvas.Pen.Style := psSolid;
        if FCtl3D then
        begin
          lBMP.Canvas.Pen.Color := clBtnShadow;
          lBMP.Canvas.MoveTo(0, h - 1);
          lBMP.Canvas.LineTo(0, 0);
          lBMP.Canvas.LineTo(w - 1, 0);
          lBMP.Canvas.Pen.Color := clBtnHighlight;
          lBMP.Canvas.LineTo(w - 1, h - 1);
          lBMP.Canvas.LineTo(0, h - 1);
          if FBorderStyle = busDeep then
          begin
            lBMP.Canvas.Pen.Color := cl3DDkShadow;
            lBMP.Canvas.MoveTo(1, h - 2);
            lBMP.Canvas.LineTo(1, 1);
            lBMP.Canvas.LineTo(w - 2, 1);
            lBMP.Canvas.Pen.Color := clBtnFace;
            lBMP.Canvas.LineTo(w - 2, h - 2);
            lBMP.Canvas.LineTo(1, h - 2);
          end;
        end
        else
        begin
          lBMP.Canvas.Pen.Color := clBlack;
          lBMP.Canvas.Brush.Style := bsClear;
          lBMP.Canvas.Rectangle(0, 0, w, h);
        end;
      end;
    end;
  end;

  procedure PaintParentBack(lBMP: TBitmap);
  var
    {$IFDEF RX_D7}
    MemDC: HDC;
    OldBMP: HBITMAP;
    {$ENDIF}
    B: TBitmap;
  begin
    B := TBitmap.Create;
    try
      B.Assign(lBMP);
      B.Canvas.Brush.Color := Color;
      B.Canvas.FillRect(B.Canvas.ClipRect);
      {$IFDEF RX_D7}
      if ParentBackground then
        with {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF} do
          if {$IFDEF RX_D16}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
          begin
            MemDC := CreateCompatibleDC(0);
            OldBMP := SelectObject(MemDC, B.Handle);
            DrawParentBackground(Handle, MemDC, nil, False);
            if OldBMP <> 0 then SelectObject(MemDC, OldBMP);
            if MemDC <> 0 then DeleteDC(MemDC);
            Canvas.Draw(0, 0, B); //kresli pozadi
            lBMP.Canvas.Draw(0, 0, B);
            PaintToBmp(lBMP, B);
          end;
      {$ENDIF}
      if {$IFDEF RX_D7}ParentBackground and {$ENDIF}FTransparent then //kresli popredi s pruhlednosti
        DrawTransparentBitmap(Canvas.Handle, lBMP.Handle, 0, 0, Self.Color)
      else
        Self.Canvas.Copyrect(Self.ClientRect, lBMP.Canvas, Bounds(0, 0, lBMP.Width, lBMP.Height));
    finally
      B.Free;
    end;
  end;
var
  lBMP: TBitmap;
  iFPerc, iFPos: Integer;
begin
  iFPerc := Round(((FPosition - FMin) / (FMax - FMin)) * 100);
  iFPos := Round(((FPosition - FMin) / (FMax - FMin)) *
    (Width - FIndent * 2 - GetBorderSize * 2));
  if FShowAllFlag then
  begin
    FPerc := iFPerc;
    FPos := iFPos;
  end
  else
  begin
    FShowAllFlag := True;
    if (iFPerc <> FPerc) or (iFPos <> FPos) then
    begin
      FPerc := iFPerc;
      FPos := iFPos;
    end
    else
      Exit; //no repaint needed
  end;
  //----------------------------------------------------------------------------
  lBMP := TBitmap.Create;
  try
    PaintToBmp(lBMP);
    { show resultat }
    PaintParentBack(lBMP);
  finally
    lBMP.Free;
  end;
end;

procedure TRxProgress.SetTextAlign(Value: TTextUAlign);
begin
  if Value <> FTextAlign then
  begin
    FTextAlign := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TRxProgress.SetDrawStyle(Value: TDrawStyle);
begin
  if Value <> FDrawStyle then
  begin
    FDrawStyle := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetCtl3D(Value: Boolean);
begin
  if Value <> FCtl3D then
  begin
    FCtl3D := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetIndent(Value: Integer);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetShowPos(Value: Boolean);
begin
  if Value <> FShowPos then
  begin
    FShowPos := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetShowPer(Value: Boolean);
begin
  if Value <> FShowPer then
  begin
    FShowPer := Value;
    Paint;
  end;
end;

procedure TRxProgress.SetMin(Value: Integer);
begin
  if Value <> FMin then
  begin
    if Value >= FMax then
      Value := FMax - 1;
    FMin := Value;
    if FPosition < FMin then
      FPosition := FMin;
    FShowAllFlag := False;
    Paint;
  end;
end;

procedure TRxProgress.SetBorderStyle(Value: TBorderUStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Paint;
  end;
end;

{ TRxStatusPanelBinder }

constructor TRxStatusPanelBinder.Create(AOwner: TComponent);
begin
  inherited;
  FPanelIndex := 0;
end;

destructor TRxStatusPanelBinder.Destroy;
begin
  if FControl <> nil then
  begin
    FControl.Parent := FControlParent;
    FControl.BoundsRect := FControlBoundsRect;
  end;
  if FStatusBar <> nil then
    FStatusBar.OnDrawPanel := FOldOnDrawPanel;
  inherited;
end;

procedure TRxStatusPanelBinder.SetControl(Value: TControl);
var
  i: Integer;
begin
  if Value <> FControl then
  begin
    if (Value is TForm) or (Value is TStatusBar) then
      FControl := nil
    else
    begin
      PopControl;
      if Value <> nil then
        for i := 0 to Owner.ComponentCount - 1 do
          if Owner.Components[i] is TRxStatusPanelBinder then
            if (Owner.Components[i] as TRxStatusPanelBinder).Control = Value then
              (Owner.Components[i] as TRxStatusPanelBinder).Control := nil;
      FControl := Value;
      PushControl;
      if FStatusBar <> nil then
        FStatusBar.Refresh;
    end;
  end;
end;

procedure TRxStatusPanelBinder.SetStatusBar(Value: TStatusBar);
begin
  if Value <> FStatusBar then
  begin
    if FStatusBar <> nil then
    begin
      FStatusBar.OnDrawPanel := FOldOnDrawPanel;
      PopControl;
    end;
    FStatusBar := Value;
    if FStatusBar <> nil then
    begin
      FOldOnDrawPanel := FStatusBar.OnDrawPanel;
      FStatusBar.OnDrawPanel := FOnDrawPanel;
      FStatusBar.FreeNotification(Self);
      PushControl;
      FStatusBar.Refresh;
    end;
  end;
end;

procedure TRxStatusPanelBinder.FOnDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  if (FControl <> nil) and (FPanelIndex = Panel.Index) then
  begin
    FControl.Top := Rect.Top;
    FControl.Height := Rect.Bottom - Rect.Top;
    FControl.Left := Rect.Left;
    if (Panel.Index = StatusBar.Panels.Count - 1) and StatusBar.SizeGrip then
      FControl.Width := Rect.Right - Rect.Left - 13
    else
      FControl.Width := Rect.Right - Rect.Left;
  end
  else if Assigned(FOldOnDrawPanel) then
    FOldOnDrawPanel(StatusBar, Panel, Rect);
end;

procedure TRxStatusPanelBinder.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
  begin
    if AComponent = FControl then
      Control := nil
    else if AComponent = FStatusBar then
      StatusBar := nil;
  end;
end;

procedure TRxStatusPanelBinder.SetPanelIndex(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FPanelIndex then
  begin
    FPanelIndex := Value;
    SetPanelStyle;
    if FStatusBar <> nil then
      FStatusBar.Refresh;
  end;
end;

procedure TRxStatusPanelBinder.SetPanelStyle;
begin
  if (FStatusBar <> nil) and (FControl <> nil) then
    if FPanelIndex < FStatusBar.Panels.Count then
    begin
      FPanelStyle := FStatusBar.Panels[FPanelIndex].Style;
      FStatusBar.Panels[FPanelIndex].Style := psOwnerDraw;
    end;
end;

procedure TRxStatusPanelBinder.PushControl;
begin
  if FControl <> nil then
  begin
    FControlVisible := FControl.Visible;
    FControl.FreeNotification(Self);
    FControlParent := FControl.Parent;
    FControlBoundsRect := FControl.BoundsRect;
    if FStatusBar <> nil then
      FControl.Parent := FStatusBar;
    SetPanelStyle;
    FControl.Visible := True;
  end;
end;

procedure TRxStatusPanelBinder.PopControl;
begin
  if FControl <> nil then
  begin
    FControl.Visible := FControlVisible;
    if not (csDestroying in FControl.ComponentState) then
    begin
      FControl.Parent := FControlParent;
      FControl.BoundsRect := FControlBoundsRect;
    end;
  end;
end;

procedure CreateParamsInternal(var Params: TCreateParams; AHorzAlign: TAlignment;
  AVertAlign: TVerticalAlignment; AStyle: TStyle; AWordWrap: Boolean);
begin
  Params.Style := Params.Style and not (BS_LEFT or BS_RIGHT or BS_CENTER or
    BS_TOP or BS_BOTTOM or BS_VCENTER or BS_FLAT or BS_PUSHLIKE or BS_MULTILINE);
  case AHorzAlign of
    taLeftJustify: Params.Style := Params.Style or BS_LEFT;
    taRightJustify: Params.Style := Params.Style or BS_RIGHT;
    taCenter: Params.Style := Params.Style or BS_CENTER;
  end;
  case AVertAlign of
    taAlignTop: Params.Style := Params.Style or BS_TOP;
    taAlignBottom: Params.Style := Params.Style or BS_BOTTOM;
    taVerticalCenter: Params.Style := Params.Style or BS_VCENTER;
  end;
  case AStyle of
    vsFlat: Params.Style := Params.Style or BS_FLAT;
    vsPushButton: Params.Style := Params.Style or BS_PUSHLIKE;
  end;
  if AWordWrap then Params.Style := Params.Style or BS_MULTILINE;
end;

{ TRxCheckBox }

constructor TRxCheckBox.Create(AOwner: TComponent);
begin
  FWordWrap := True;
  inherited;
end;

procedure TRxCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateParamsInternal(Params, FHorzAlign, FVertAlign, FStyle, FWordWrap);
end;

procedure TRxCheckBox.SetHorzAlign(Value: TAlignment);
begin
  if Value <> FHorzAlign then
  begin
    FHorzAlign := Value;
    RecreateWnd;
  end;
end;

procedure TRxCheckBox.SetStyle(Value: TStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRxCheckBox.SetVertAlign(Value: TVerticalAlignment);
begin
  if Value <> FVertAlign then
  begin
    FVertAlign := Value;
    RecreateWnd;
  end;
end;

procedure TRxCheckBox.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

{ TRxRadioButton }

constructor TRxRadioButton.Create(AOwner: TComponent);
begin
  FWordWrap := True;
  inherited;
end;

procedure TRxRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateParamsInternal(Params, FHorzAlign, FVertAlign, FStyle, FWordWrap);
end;

procedure TRxRadioButton.SetHorzAlign(Value: TAlignment);
begin
  if Value <> FHorzAlign then
  begin
    FHorzAlign := Value;
    RecreateWnd;
  end;
end;

procedure TRxRadioButton.SetStyle(Value: TStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRxRadioButton.SetVertAlign(Value: TVerticalAlignment);
begin
  if Value <> FVertAlign then
  begin
    FVertAlign := Value;
    RecreateWnd;
  end;
end;

procedure TRxRadioButton.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

{  TRxAnimBitBtn  }

constructor TRxAnimBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSparkle := TBitmap.Create;
  FSparkle.Handle := LoadBitmap(hInstance, 'SPARKS_IMAGES');
  FSparkle.TransparentColor := clBlack;
  FSparkle.Transparent := True;
  FTimer := TTimer.Create(Self);
  FInterval := 100;
  FAnimated := False;
  FTimer.Enabled := False;
  FTimer.Interval := FInterval;
  FTimer.OnTimer := TimerEvent;
  FAnimCount := 0;
  FSubImageIndex := 1;
  FRxAnimGlyphsOrientation := agoHorizontal;
  FAnimGlyph := TBitmap.Create;
  FTempGlyph := TBitmap.Create;
end;

destructor TRxAnimBitBtn.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FAnimGlyph.Free;
  FTempGlyph.Free;
  FSparkle.Free;
  inherited Destroy;
end;

procedure TRxAnimBitBtn.CheckBitmapSizeOf(Value: TBitmap);
begin
  if Value.Width > Value.Height then
    FRxAnimGlyphsOrientation := agoHorizontal
  else
    FRxAnimGlyphsOrientation := agoVertical;
  case FRxAnimGlyphsOrientation of
    agoHorizontal:
      begin
        FAnimCount := Value.Width div Value.Height;
        dY := Value.Height;
        dX := Value.Width div FAnimCount;
      end;
    agoVertical:
      begin
        FAnimCount := Value.Height div Value.Width;
        dY := Value.Height div FAnimCount;
        dx := Value.Width;
      end;
  end;
end;

procedure TRxAnimBitBtn.SetAnimGlyph(Value: TBitmap);
var
  WasAnim: Boolean;
begin
  if FAnimGlyph <> Value then
  begin
    WasAnim := FAnimated;
    Animated := False; //stop the animation
    try
      FAnimGlyph.Assign(Value);
      if FAnimGlyph <> nil then
      begin
        CheckBitmapSizeOf(Value);
      end;
    finally
      Animated := WasAnim; //return previous value of animation
    end;
  end;
end;

procedure TRxAnimBitBtn.SetAnimated(Value: Boolean);
begin
  if FAnimated <> Value then
  begin
    FAnimated := Value;
    if Value then
    begin
      FTempGlyph.Assign(Glyph);
      FSubImageIndex := 1;
      TimerEvent(Self);
    end
    else
      Glyph.Assign(FTempGlyph);
    FTimer.Enabled := Value;
  end;
end;

procedure TRxAnimBitBtn.SetInterval(Value: Integer);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if Animated then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := Value;
      FTimer.Enabled := True;
    end
    else
    begin
      FTimer.Interval := Value;
    end;
  end;
end;

procedure TRxAnimBitBtn.SetAnimOrientation(Value: TRxAnimGlyphsOrientation);
begin
  if Value <> FRxAnimGlyphsOrientation then
  begin
    if Animated then
    begin
      Animated := False;
      FRxAnimGlyphsOrientation := Value;
      Animated := True;
    end
    else
      FRxAnimGlyphsOrientation := Value;
  end;
end;

procedure TRxAnimBitBtn.TimerEvent(Sender: TObject);
var
  tmpBmp: TBitmap;
  Rsrc, Rdst: TRect;
  vX, i: Integer;
begin
  if csDesigning in ComponentState then Exit;
  tmpBmp := TBitmap.Create;
  try
    if (FAnimGlyph = nil) or (FAnimGlyph.Empty) then
    begin
      //no exist value, must be simulated from sparks
      if FTempGlyph.Empty then FTempGlyph.Assign(Self.Glyph);
      if Self.NumGlyphs = 1 then
        vX := FTempGlyph.Width
      else
        vX := FTempGlyph.Width div Self.NumGlyphs;
      if (vX < 24) or (FTempGlyph.Height < 24) then Exit;
      FAnimCount := 9; //fix of sparks
      tmpBmp.Assign(FTempGlyph);
      tmpBmp.Height := FTempGlyph.Height;
      tmpBmp.Width := FTempGlyph.Width;
      Rsrc := Bounds((FSubImageIndex - 1) * 24, 0, 24, 24); //from sparks
      Rdst := tmpBmp.Canvas.ClipRect;
      tmpBmp.Canvas.CopyMode := cmSrcInvert or cmSrcAnd;
      if Self.NumGlyphs > 1 then
      begin
        for i := 0 to Self.NumGlyphs - 1 do
        begin
          Rdst := Bounds(i * vX, 0, vX, FTempGlyph.Height);
          tmpBmp.Canvas.CopyRect(Rdst, FSparkle.Canvas, Rsrc);
        end;
      end
      else
      begin
        tmpBmp.Canvas.CopyRect(Rdst, FSparkle.Canvas, Rsrc);
      end;
    end
    else
    begin
      if FRxAnimGlyphsOrientation = agoHorizontal then
      begin
        tmpBmp.Height := dY;
        tmpBmp.Width := dX;
        tmpBmp.Canvas.CopyMode := cmSrcCopy;
        tmpBmp.Canvas.CopyRect(Rect(0, 0, tmpBmp.Height, tmpBmp.Height),
          FAnimGlyph.Canvas, Bounds(dX * (FSubImageIndex - 1), 0, dX, dY));
      end
      else
      begin
        tmpBmp.Height := dY;
        tmpBmp.Width := dX;
        tmpBmp.Canvas.CopyMode := cmSrcCopy;
        tmpBmp.Canvas.CopyRect(Rect(0, 0, tmpBmp.Width, tmpBmp.Width),
          FAnimGlyph.Canvas,
          Bounds(0, dY * (FSubImageIndex - 1), dX, dY));
      end;
    end;
    Self.Glyph.Assign(tmpBmp);
    Inc(FSubImageIndex);
    if FSubImageIndex > FAnimCount then
    begin
      FSubImageIndex := 1;
      if Assigned(FOnFinishAnim) then
        FOnFinishAnim(Self)
    end;
  finally
    tmpBmp.Free;
  end;
end;

{  TRxAnimSpeedButton  }

constructor TRxAnimSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSparkle := TBitmap.Create;
  //special sparks image for simple sparking
  FSparkle.Handle := LoadBitmap(hInstance, 'SPARKS_IMAGES');
  FSparkle.TransparentColor := clBlack;
  FSparkle.Transparent := True;
  FTimer := TTimer.Create(Self);
  FInterval := 100;
  FAnimated := False;
  FTimer.Enabled := False;
  FTimer.Interval := FInterval;
  FTimer.OnTimer := TimerEvent;
  FAnimCount := 0;
  FSubImageIndex := 1;
  FRxAnimGlyphsOrientation := agoHorizontal;
  FAnimGlyph := TBitmap.Create;
  FTempGlyph := TBitmap.Create;
end;

destructor TRxAnimSpeedButton.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FAnimGlyph.Free;
  FTempGlyph.Free;
  FSparkle.Free;
  inherited Destroy;
end;

procedure TRxAnimSpeedButton.CheckBitmapSizeOf(Value: TBitmap);
begin
  if Value.Width > Value.Height then
    FRxAnimGlyphsOrientation := agoHorizontal
  else
    FRxAnimGlyphsOrientation := agoVertical;
  case FRxAnimGlyphsOrientation of
    agoHorizontal:
      begin
        FAnimCount := Value.Width div Value.Height;
        dY := Value.Height;
        dX := Value.Width div FAnimCount;
      end;
    agoVertical:
      begin
        FAnimCount := Value.Height div Value.Width;
        dY := Value.Height div FAnimCount;
        dx := Value.Width;
      end;
  end;
end;

procedure TRxAnimSpeedButton.SetAnimGlyph(Value: TBitmap);
var
  WasAnim: Boolean;
begin
  if FAnimGlyph <> Value then
  begin
    WasAnim := FAnimated;
    Animated := False; //stop the animation
    try
      FAnimGlyph.Assign(Value);
      if FAnimGlyph <> nil then
      begin
        CheckBitmapSizeOf(Value);
      end;
    finally
      Animated := WasAnim; //return previous value of animation
    end;
  end;
end;

procedure TRxAnimSpeedButton.SetAnimated(Value: Boolean);
begin
  if FAnimated <> Value then
  begin
    FAnimated := Value;
    if Value then
    begin
      FTempGlyph.Assign(Glyph);
      FSubImageIndex := 1;
      TimerEvent(Self);
    end
    else
      Glyph.Assign(FTempGlyph);
    FTimer.Enabled := Value;
  end;
end;

procedure TRxAnimSpeedButton.SetInterval(Value: Integer);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;
    if Animated then
    begin
      FTimer.Enabled := False;
      FTimer.Interval := Value;
      FTimer.Enabled := True;
    end
    else
    begin
      FTimer.Interval := Value;
    end;
  end;
end;

procedure TRxAnimSpeedButton.SetAnimOrientation(Value: TRxAnimGlyphsOrientation);
begin
  if Value <> FRxAnimGlyphsOrientation then
  begin
    if Animated then
    begin
      Animated := False;
      FRxAnimGlyphsOrientation := Value;
      Animated := True;
    end
    else
      FRxAnimGlyphsOrientation := Value;
  end;
end;

procedure TRxAnimSpeedButton.TimerEvent(Sender: TObject);
var
  tmpBmp: TBitmap;
  Rsrc, Rdst: TRect;
  vX, i: Integer;
begin
  if csDesigning in ComponentState then Exit;
  tmpBmp := TBitmap.Create;
  try
    if (FAnimGlyph = nil) or (FAnimGlyph.Empty) then
    begin
      //no exist value, must be simulated from sparks
      if FTempGlyph.Empty then FTempGlyph.Assign(Self.Glyph);
      if Self.NumGlyphs = 1 then
        vX := FTempGlyph.Width
      else
        vX := FTempGlyph.Width div Self.NumGlyphs;
      if (vX < 24) or (FTempGlyph.Height < 24) then Exit;
      FAnimCount := 9; //fix of sparks
      tmpBmp.Assign(FTempGlyph);
      tmpBmp.Height := FTempGlyph.Height;
      tmpBmp.Width := FTempGlyph.Width;
      Rsrc := Bounds((FSubImageIndex - 1) * 24, 0, 24, 24); //from sparks
      Rdst := tmpBmp.Canvas.ClipRect;
      tmpBmp.Canvas.CopyMode := cmSrcInvert or cmSrcAnd;
      if Self.NumGlyphs > 1 then
      begin
        for i := 0 to Self.NumGlyphs - 1 do
        begin
          Rdst := Bounds(i * vX, 0, vX, FTempGlyph.Height);
          tmpBmp.Canvas.CopyRect(Rdst, FSparkle.Canvas, Rsrc);
        end;
      end
      else
      begin
        tmpBmp.Canvas.CopyRect(Rdst, FSparkle.Canvas, Rsrc);
      end;
    end
    else
    begin
      if FRxAnimGlyphsOrientation = agoHorizontal then
      begin
        tmpBmp.Height := dY;
        tmpBmp.Width := dX;
        tmpBmp.Canvas.CopyMode := cmSrcCopy;
        tmpBmp.Canvas.CopyRect(Rect(0, 0, tmpBmp.Height, tmpBmp.Height),
          FAnimGlyph.Canvas, Bounds(dX * (FSubImageIndex - 1), 0, dX, dY));
      end
      else
      begin
        tmpBmp.Height := dY;
        tmpBmp.Width := dX;
        tmpBmp.Canvas.CopyMode := cmSrcCopy;
        tmpBmp.Canvas.CopyRect(Rect(0, 0, tmpBmp.Width, tmpBmp.Width),
          FAnimGlyph.Canvas,
          Bounds(0, dY * (FSubImageIndex - 1), dX, dY));
      end;
    end;
    Self.Glyph.Assign(tmpBmp);
    Inc(FSubImageIndex);
    if FSubImageIndex > FAnimCount then
    begin
      FSubImageIndex := 1;
      if Assigned(FOnFinishAnim) then
        FOnFinishAnim(Self)
    end;
  finally
    tmpBmp.Free;
  end;
end;

{--- constants as sets ---}
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

{ TTextListBox }

procedure TTextListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

function TTextListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: LongInt;
  S: string;
begin
  S := Items[Index] + 'x';
  if TabWidth > 0 then
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
      1, ATabWidth));
  end
  else
    Result := Canvas.TextWidth(S);
end;

procedure TTextListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxWidth := Max(FMaxWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

{$IFDEF VER80}
procedure TTextListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    RecreateWnd;
  end;
end;

procedure TTextListBox.CreateParams(var Params: TCreateParams);
const
  TabStops: array[Boolean] of LongWord = (0, LBS_USETABSTOPS);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or TabStops[FTabWidth <> 0];
end;

procedure TTextListBox.CreateWnd;
begin
  inherited CreateWnd;
  if FTabWidth <> 0 then
    SendMessage(Handle, LB_SETTABSTOPS, 1, {$IFDEF WIN64}UIntPtr{$ELSE}LongInt{$ENDIF}(@FTabWidth));
end;
{$ENDIF}

procedure TTextListBox.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        inherited WndProc(Message);
        FMaxWidth := Max(FMaxWidth, GetItemWidth(Message.Result));
        SetHorizontalExtent;
      end;
    LB_DELETESTRING:
      begin
        if GetItemWidth(Message.wParam) >= FMaxWidth then
        begin
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Message);
          ResetHorizontalExtent;
        end
        else
          inherited WndProc(Message);
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHorizontalExtent;
        Perform(WM_HSCROLL, SB_TOP, 0);
        inherited WndProc(Message);
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Message);
        Canvas.Font.Assign(Self.Font);
        ResetHorizontalExtent;
        Exit;
      end;
  else
    inherited WndProc(Message);
  end;
end;

{ TRxCustomListBox implementation copied from STDCTRLS.PAS and modified }

{ TRxListBoxStrings }

{$IFNDEF VER80}
{ $ Define RLBS_calc_hints}
//set it to free hints array if the last hint was wiped out
//to me, having an array of nil's is not too hard to novadays PC
const
  MaxStrArrSz = System.MaxInt div SizeOf(PString) - 1;
type
  TStringArr = array[0..MaxStrArrSz] of string;
  PStringArr = ^TStringArr;
{$ENDIF}
type
  TRxListBoxStrings = class(TStrings)
  private
    ListBox: TRxCustomListBox;
    {$IFNDEF VER80}
    FHintCapacity: Integer;
    FHintStrings: pstringarr;
    {$IFDEF RLBS_calc_hints}
    FHintSetQuantity: Integer;
    {$ENDIF}{$ENDIF}
  protected
    {$IFNDEF RX_D3}
    procedure Error(Msg: Word; Data: Integer);
    {$ENDIF}
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
    {$IFNDEF VER80}
    procedure InternalSetHint(Index: Integer; Hint: string);
    procedure AllocHints(dropAll: Boolean = False);
    procedure DropHints;
    procedure InsertHintCell(Index: Integer);
    procedure DeleteHintCell(Index: Integer);
  public
    function GetHint(Index: Integer): string;
    procedure SetHint(Index: Integer; Hint: string);
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$ENDIF}
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

{$IFNDEF RX_D3}
procedure TRxListBoxStrings.Error(Msg: Word; Data: Integer);

{$IFNDEF VER80}

function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;
  {$ELSE}

function ReturnAddr: Pointer; assembler;
  asm
    MOV     AX,[BP].Word[2]
    MOV     DX,[BP].Word[4]
  end;
  {$ENDIF}

begin
  raise EStringListError.CreateFmt('%s: %d', [LoadStr(Msg),
    Data])at ReturnAddr;
end;
{$ENDIF}

function TRxListBoxStrings.GetCount: Integer;
begin
  Result := SendMessage(ListBox.Handle, LB_GETCOUNT, 0, 0);
end;

function TRxListBoxStrings.Get(Index: Integer): string;
var
  Len: Integer;
  {$IFNDEF VER80}
  Text: array[0..4095] of Char;
  {$ENDIF}
begin
  Len := SendMessage(ListBox.Handle, LB_GETTEXT, WPARAM(Index),
    {$IFNDEF VER80}LPARAM(@Text){$ELSE}LongInt(@Result){$ENDIF});
  if Len < 0 then Error(SListIndexError, Index);
  {$IFNDEF VER80}
  SetString(Result, Text, Len);
  {$ELSE}
  System.Move(Result[0], Result[1], Len);
  Result[0] := Char(Len);
  {$ENDIF}
end;

function TRxListBoxStrings.GetObject(Index: Integer): TObject;
begin
  Result := TObject(ListBox.GetItemData(Index));
  if {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF}(Result) = LB_ERR then Error(SListIndexError, Index);
end;

procedure TRxListBoxStrings.PutObject(Index: Integer; AObject: TObject);
begin
  ListBox.SetItemData(Index, {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF}(AObject));
end;

function TRxListBoxStrings.Add(const S: string): Integer;
{$IFDEF VER80}
var
  Text: array[0..255] of Char;
{$ENDIF}
begin
  {$IFNDEF VER80}
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, LPARAM(PChar(S)));
  {$ELSE}
  Result := SendMessage(ListBox.Handle, LB_ADDSTRING, 0, LongInt(StrPCopy(Text, S)));
  {$ENDIF}
  if Result < 0 then raise EOutOfResources.Create(ResStr(SInsertLineError));
  {$IFNDEF VER80}InsertHintCell(Result); {$ENDIF}
end;

procedure TRxListBoxStrings.Insert(Index: Integer; const S: string);
{$IFDEF VER80}
var
  Text: array[0..255] of Char;
{$ENDIF}
begin
  if SendMessage(ListBox.Handle, LB_INSERTSTRING, WPARAM(Index),
    {$IFNDEF VER80}
    LPARAM(PChar(S))) < 0 then
    {$ELSE}
    LongInt(StrPCopy(Text, S))) < 0 then
    {$ENDIF}
    raise EOutOfResources.Create(ResStr(SInsertLineError));
  {$IFNDEF VER80}InsertHintCell(Index); {$ENDIF}
end;

procedure TRxListBoxStrings.Delete(Index: Integer);
begin
  ListBox.DeleteString(Index);
end;

procedure TRxListBoxStrings.Clear;
begin
  ListBox.ResetContent; {$IFNDEF VER80}DropHints; {$ENDIF}
end;

procedure TRxListBoxStrings.SetUpdateState(Updating: Boolean);
begin
  SendMessage(ListBox.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then ListBox.Refresh;
end;

{$IFNDEF VER80}
procedure TRxListBoxStrings.AllocHints(dropAll: Boolean);
var
  sz: Integer; i: Integer; p: PString;
begin
  {$IFDEF RLBS_calc_hints}
  if FHintCapacity = 0 then FHintQuantity := 0;
  {$ENDIF}
  if dropAll then
    sz := 0
  else
    sz := GetCount;
  if sz = FHintCapacity then Exit;

  if sz < FHintCapacity then
    if FHintStrings <> nil then
    begin
      p := @FHintStrings^[sz];
      for i := sz to FHintCapacity - 1 do
      begin
        p^ := ''; Inc(p); // freeing our references to ANSI strings. No use for FillChar
      end;
    end;

  ReallocMem(FHintStrings, sz * SizeOf(FHintStrings^[0]));

  if sz > FHintCapacity then
    FillChar(Pointer(FHintStrings^[FHintCapacity]), (sz - FHintCapacity) * SizeOf(FHintStrings^[0]), 0);

  FHintCapacity := sz;
  {$IFDEF RLBS_calc_hints}
  if FHintCapacity = 0 then FHintQuantity := 0;
  {$ENDIF}
end;

procedure TRxListBoxStrings.DropHints;
begin
  AllocHints(True);
end;

procedure TRxListBoxStrings.DeleteHintCell(Index: Integer);
begin
  if FHintStrings = nil then Exit;
  if (Index < 0) or (Index >= FhintCapacity) then Exit;

  InternalSetHint(Index, ''); //clearing link to ANSIstring
  system.Move(FHintStrings^[1 + Index], FHintStrings^[Index],
    (FHintCapacity - Index - 1) * SizeOf(FHintStrings^[0]));
  Pointer(FHintStrings^[FHintCapacity - 1]) := nil;
  AllocHints;
end;

procedure TRxListBoxStrings.InsertHintCell(Index: Integer);
var
  PrevCap: Integer;
begin
  if FHintStrings = nil then Exit; //will be alocated later on demand
  PrevCap := FHintCapacity;
   {if FHintCapacity<GetCount then}AllocHints;
  if Index >= PrevCap then Exit; // no need in scrolling
  if Index < 0 then Exit;

  system.Move(FHintStrings^[Index], FHintStrings^[1 + Index],
    (FHintCapacity - Index - 1) * SizeOf(FHintStrings^[0]));
  Pointer(FHintStrings^[Index]) := nil;
end;

function TRxListBoxStrings.GetHint(Index: Integer): string;
begin
  Result := '';
  if FHintCapacity = 0 then Exit;
  if (Index < 0) or (Index >= FHintCapacity) then Error(SListIndexError, Index);
  if Assigned(FhintStrings) then Result := FHintStrings^[Index];
end;

procedure TRxListBoxStrings.InternalSetHint(Index: Integer; Hint: string);
begin // No checks, just centralised asignment and so on
  if Assigned(FhintStrings) then
  begin
    {$IFDEF RLBS_calc_hints}
    if FHintStrings^[Index] <> '' then Dec(FHintQuantity);
    if Hint <> '' then Inc(FHintQuantity);
    {$ENDIF}
    FHintStrings^[Index] := Hint;
  end{$IFDEF RLBS_calc_hints}
  else
    FHintQuantity := 0{$ENDIF};
end;

procedure TRxListBoxStrings.SetHint(Index: Integer; Hint: string);
var
  sz: Integer;
begin
  sz := FHintCapacity; if sz = 0 then sz := GetCount;
  if (Index < 0) or (Index >= sz) then
    Error(SListIndexError, Index);
  if Hint <> '' then
    if FHintStrings = nil then AllocHints;
  InternalSetHint(Index, Hint);
end;

constructor TRxListBoxStrings.Create;
begin
  inherited;
  FHintStrings := nil; FHintCapacity := 0;
  {$IFDEF RLBS_calc_hints}FHintSetQuantity := 0; {$ENDIF}
end;

destructor TRxListBoxStrings.Destroy;
begin
  DropHints; // interesting, why TStrings.Destroy doesnt call Clear?
  inherited;
end;

procedure TRxListBoxStrings.Assign(Source: TPersistent);
var
  i: Integer;
begin
  inherited;
  DropHints;
  if Source is TRxListBoxStrings then
    with Source as TRxListBoxStrings do
    begin
      if Assigned(FHintStrings) then
        for i := 0 to GetCount - 1 do
          Self.SetHint(i, GetHint(i));
    end;
end;
{$ENDIF}

{ TRxCustomListBox }

procedure ListIndexError(Index: Integer);

{$IFNDEF VER80}

function ReturnAddr: Pointer;
  asm
    MOV     EAX,[EBP+4]
  end;
  {$ELSE}

function ReturnAddr: Pointer; assembler;
  asm
    MOV     AX,[BP].Word[2]
    MOV     DX,[BP].Word[4]
  end;
  {$ENDIF}

begin
  {$IFDEF RX_D3}
  raise EStringListError.CreateFmt(SListIndexError, [Index])at ReturnAddr;
  {$ELSE}
  raise EStringListError.CreateFmt('%s: %d', [{LoadStr(}SListIndexError {)},
    Index])at ReturnAddr;
  {$ENDIF}
end;

constructor TRxCustomListBox.Create(AOwner: TComponent);
const
  ListBoxStyle = [csSetCaption, csDoubleClicks];
begin
  inherited Create(AOwner);
  {$IFNDEF VER80}
  if NewStyleControls then
    ControlStyle := ListBoxStyle
  else
    ControlStyle := ListBoxStyle + [csFramed];
  {$ELSE}
  ControlStyle := ListBoxStyle + [csFramed];
  {$ENDIF}
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FItems := CreateItemList;
  TRxListBoxStrings(FItems).ListBox := Self;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FItemHeight := 16;
  FBorderStyle := bsSingle;
  FExtendedSelect := True;
  {$IFNDEF VER80}
  FHintSource := hsDefault;
  FOnGetItemHintEvent := nil;
  {$ENDIF}
end;

destructor TRxCustomListBox.Destroy;
begin
  inherited Destroy;
  FCanvas.Free;
  FItems.Free;
  FSaveItems.Free;
end;

function TRxCustomListBox.CreateItemList: TStrings;
begin
  Result := TRxListBoxStrings.Create;
end;

function TRxCustomListBox.GetItemData(Index: Integer): {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF};
begin
  Result := SendMessage(Handle, LB_GETITEMDATA, WPARAM(Index), 0);
end;

procedure TRxCustomListBox.SetItemData(Index: Integer; AData: {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF});
begin
  SendMessage(Handle, LB_SETITEMDATA, WPARAM(Index), AData);
end;

procedure TRxCustomListBox.DeleteString(Index: Integer);
begin
  {$IFNDEF VER80}if {$ENDIF}
  SendMessage(Handle, LB_DELETESTRING, WPARAM(Index), 0) //why not in TRxListBoxstrings as .Add and .Insert?
    {$IFNDEF VER80} <> LB_ERR then
    if Fitems is TRxListBoxstrings then
      (FItems as TRxListBoxstrings).DeleteHintCell(Index){$ENDIF};
end;

procedure TRxCustomListBox.SetHorizontalExtent;
begin
  SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxItemWidth, 0);
end;

function TRxCustomListBox.GetItemWidth(Index: Integer): Integer;
var
  ATabWidth: LongInt;
  S: string;
begin
  if (Style <> lbStandard) and Assigned(FOnGetItemWidth) and
    Assigned(FOnDrawItem) then
  begin
    Result := 0;
    FOnGetItemWidth(Self, Index, Result);
  end
  else
  begin
    S := Items[Index] + 'x';
    if TabWidth > 0 then
    begin
      {if (FTabChar > #0) then
        for I := 1 to Length(S) do
          if S[I] = FTabChar then S[I] := #9;}
      ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
      Result := LoWord(GetTabbedTextExtent(Canvas.Handle, @S[1], Length(S),
        1, ATabWidth));
    end
    else
      Result := Canvas.TextWidth(S);
  end;
end;

procedure TRxCustomListBox.ResetHorizontalExtent;
var
  I: Integer;
begin
  FMaxItemWidth := 0;
  for I := 0 to Items.Count - 1 do
    FMaxItemWidth := Max(FMaxItemWidth, GetItemWidth(I));
  SetHorizontalExtent;
end;

procedure TRxCustomListBox.ResetContent;
begin
  SendMessage(Handle, LB_RESETCONTENT, 0, 0);
end;

procedure TRxCustomListBox.Clear;
begin
  FItems.Clear;
end;

procedure TRxCustomListBox.SetColumnWidth;
begin
  if FColumns > 0 then
    SendMessage(Handle, LB_SETCOLUMNWIDTH, (Width + FColumns - 3) div
      FColumns, 0);
end;

procedure TRxCustomListBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
    if (FColumns = 0) or (Value = 0) then
    begin
      FColumns := Value;
      RecreateWnd;
    end
    else
    begin
      FColumns := Value;
      if HandleAllocated then SetColumnWidth;
    end;
end;

function TRxCustomListBox.GetItemIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETCURSEL, 0, 0);
end;

function TRxCustomListBox.GetSelCount: Integer;
begin
  Result := SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
end;

procedure TRxCustomListBox.SetItemIndex(Value: Integer);
begin
  if GetItemIndex <> Value then
    SendMessage(Handle, LB_SETCURSEL, Value, 0);
end;

procedure TRxCustomListBox.SetExtendedSelect(Value: Boolean);
begin
  if Value <> FExtendedSelect then
  begin
    FExtendedSelect := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomListBox.SetIntegralHeight(Value: Boolean);
begin
  if Value <> FIntegralHeight then
  begin
    FIntegralHeight := Value;
    RecreateWnd;
  end;
end;

function TRxCustomListBox.GetAutoScroll: Boolean;
begin
  Result := FAutoScroll and (Columns = 0);
end;

procedure TRxCustomListBox.SetOnDrawItem(Value: TDrawItemEvent);
begin
  if Assigned(FOnDrawItem) <> Assigned(Value) then
  begin
    FOnDrawItem := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  end
  else
    FOnDrawItem := Value;
end;

procedure TRxCustomListBox.SetOnGetItemWidth(Value: TGetItemWidthEvent);
begin
  if Assigned(FOnGetItemWidth) <> Assigned(Value) then
  begin
    FOnGetItemWidth := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  end
  else
    FOnGetItemWidth := Value;
end;

procedure TRxCustomListBox.SetAutoScroll(Value: Boolean);
begin
  if AutoScroll <> Value then
  begin
    FAutoScroll := Value;
    Perform(WM_HSCROLL, SB_TOP, 0);
    if HandleAllocated then
    begin
      if AutoScroll then
        ResetHorizontalExtent
      else
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
    end;
  end;
end;

function TRxCustomListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and (FStyle = lbStandard) then
  begin
    Perform(LB_GETITEMRECT, 0, LPARAM(@R));
    Result := R.Bottom - R.Top;
  end;
end;

procedure TRxCustomListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
  begin
    FItemHeight := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomListBox.SetTabWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FTabWidth <> Value then
  begin
    FTabWidth := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomListBox.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    RecreateWnd;
  end;
end;

function TRxCustomListBox.GetSelected(Index: Integer): Boolean;
var
  R: LongInt;
begin
  R := SendMessage(Handle, LB_GETSEL, Index, 0);
  if R = LB_ERR then ListIndexError(Index);
  Result := LongBool(R);
end;

procedure TRxCustomListBox.SetSelected(Index: Integer; Value: Boolean);
begin
  if MultiSelect then
  begin
    if SendMessage(Handle, LB_SETSEL, Ord(Value), Index) = LB_ERR then
      ListIndexError(Index);
  end
  else
  begin
    if Value then
      SetItemIndex(Index)
    else if (ItemIndex = Index) then
      SetItemIndex(-1);
  end;
end;

procedure TRxCustomListBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomListBox.SetStyle(Value: TListBoxStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    RecreateWnd;
  end;
end;

function TRxCustomListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle, LB_GETTOPINDEX, 0, 0);
end;

procedure TRxCustomListBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TRxCustomListBox.SetTopIndex(Value: Integer);
begin
  if GetTopIndex <> Value then SendMessage(Handle, LB_SETTOPINDEX, Value, 0);
end;

procedure TRxCustomListBox.SetGraySelection(Value: Boolean);
begin
  if FGraySelection <> Value then
  begin
    FGraySelection := Value;
    if not Focused then Invalidate;
  end;
end;

procedure TRxCustomListBox.SetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TRxCustomListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
  if PtInRect(ClientRect, Pos) then
  begin
    Result := TopIndex;
    Count := Items.Count;
    while Result < Count do
    begin
      Perform(LB_GETITEMRECT, Result, LPARAM(@ItemRect));
      if PtInRect(ItemRect, Pos) then Exit;
      Inc(Result);
    end;
    if not Existing then Exit;
  end;
  Result := -1;
end;

function TRxCustomListBox.ItemRect(Index: Integer): TRect;
var
  Count: Integer;
begin
  Count := Items.Count;
  if (Index = 0) or (Index < Count) then
    Perform(LB_GETITEMRECT, WPARAM(Index), LPARAM(@Result))
  else if Index = Count then
  begin
    Perform(LB_GETITEMRECT, WPARAM(Index - 1), LPARAM(@Result));
    OffsetRect(Result, 0, Result.Bottom - Result.Top);
  end
  else
    FillChar(Result, SizeOf(Result), 0);
end;

procedure TRxCustomListBox.CreateParams(var Params: TCreateParams);
type
  PSelects = ^TSelects;
  TSelects = array[Boolean] of LongWord;
const
  BorderStyles: array[TBorderStyle] of LongWord = (0, WS_BORDER);
  Styles: array[TListBoxStyle] of LongWord =
  (0, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWVARIABLE
    {$IFDEF RX_D6}, LBS_OWNERDRAWFIXED, LBS_OWNERDRAWFIXED{$ENDIF});
  Sorteds: array[Boolean] of LongWord = (0, LBS_SORT);
  MultiSelects: array[Boolean] of LongWord = (0, LBS_MULTIPLESEL);
  ExtendSelects: array[Boolean] of LongWord = (0, LBS_EXTENDEDSEL);
  IntegralHeights: array[Boolean] of LongWord = (LBS_NOINTEGRALHEIGHT, 0);
  MultiColumns: array[Boolean] of LongWord = (0, LBS_MULTICOLUMN);
  TabStops: array[Boolean] of LongWord = (0, LBS_USETABSTOPS);
var
  Selects: PSelects;
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'LISTBOX');
  with Params do
  begin
    {$IFDEF VER80}
    Inc(X); Inc(Y);
    Dec(Width, 2); Dec(Height, 2);
    {$ENDIF}
    Selects := @MultiSelects;
    if FExtendedSelect then Selects := @ExtendSelects;
    Style := Style or (WS_HSCROLL or WS_VSCROLL or LBS_HASSTRINGS or
      LBS_NOTIFY) or Styles[FStyle] or Sorteds[FSorted] or
      Selects^[FMultiSelect] or IntegralHeights[FIntegralHeight] or
      MultiColumns[FColumns <> 0] or BorderStyles[FBorderStyle] or
      TabStops[FTabWidth <> 0];
    {$IFNDEF VER80}
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    {$ENDIF}
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TRxCustomListBox.CreateWnd;
var
  W, H: Integer;
begin
  W := Width;
  H := Height;
  inherited CreateWnd;
  SetWindowPos(Handle, 0, Left, Top, W, H, SWP_NOZORDER or SWP_NOACTIVATE);
  if FTabWidth <> 0 then
    SendMessage(Handle, LB_SETTABSTOPS, 1, LPARAM(@FTabWidth));
  SetColumnWidth;
  if FSaveItems <> nil then
  begin
    FItems.Assign(FSaveItems);
    SetTopIndex(FSaveTopIndex);
    SetItemIndex(FSaveItemIndex);
    FSaveItems.Free;
    FSaveItems := nil;
  end;
end;

procedure TRxCustomListBox.DestroyWnd;
begin
  if FItems.Count > 0 then
  begin
    FSaveItems := TStringList.Create;
    FSaveItems.Assign(FItems);
    FSaveTopIndex := GetTopIndex;
    FSaveItemIndex := GetItemIndex;
  end;
  inherited DestroyWnd;
end;

procedure TRxCustomListBox.WndProc(var Message: TMessage);
begin
  if AutoScroll then
  begin
    case Message.Msg of
      LB_ADDSTRING, LB_INSERTSTRING:
        begin
          inherited WndProc(Message);
          FMaxItemWidth := Max(FMaxItemWidth, GetItemWidth(Message.Result));
          SetHorizontalExtent;
          Exit;
        end;
      LB_DELETESTRING:
        begin
          if GetItemWidth(Message.wParam) >= FMaxItemWidth then
          begin
            Perform(WM_HSCROLL, SB_TOP, 0);
            inherited WndProc(Message);
            ResetHorizontalExtent;
          end
          else
            inherited WndProc(Message);
          Exit;
        end;
      LB_RESETCONTENT:
        begin
          FMaxItemWidth := 0;
          SetHorizontalExtent;
          Perform(WM_HSCROLL, SB_TOP, 0);
          inherited WndProc(Message);
          Exit;
        end;
      WM_SETFONT:
        begin
          inherited WndProc(Message);
          Canvas.Font.Assign(Self.Font);
          ResetHorizontalExtent;
          Exit;
        end;
    end;
  end;
  {for auto drag mode, let listbox handle itself, instead of TControl}
  if not (csDesigning in ComponentState) and ((Message.Msg = WM_LBUTTONDOWN) or
    (Message.Msg = WM_LBUTTONDBLCLK)) and not Dragging then
  begin
    if DragMode = dmAutomatic then
    begin
      if IsControlMouseMsg(TWMMouse(Message)) then Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message); {overrides TControl's BeginDrag}
      Exit;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TRxCustomListBox.WMLButtonDown(var Message: TWMLButtonDown);
var
  ItemNo: Integer;
  ShiftState: TShiftState;
begin
  ShiftState := KeysToShiftState(Message.Keys);
  if (DragMode = dmAutomatic) and FMultiSelect then
  begin
    if not (ssShift in ShiftState) or (ssCtrl in ShiftState) then
    begin
      ItemNo := ItemAtPos(SmallPointToPoint(Message.Pos), True);
      if (ItemNo >= 0) and (Selected[ItemNo]) then
      begin
        BeginDrag(False);
        Exit;
      end;
    end;
  end;
  inherited;
  if (DragMode = dmAutomatic) and not (FMultiSelect and
    ((ssCtrl in ShiftState) or (ssShift in ShiftState))) then
    BeginDrag(False);
end;

procedure TRxCustomListBox.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if csDesigning in ComponentState then
    DefaultHandler(Msg)
  else
    inherited;
end;

procedure TRxCustomListBox.CNCommand(var Message: TWMCommand);
begin
  case Message.NotifyCode of
    LBN_SELCHANGE:
      begin
        {$IFDEF RX_D3}
        inherited Changed;
        {$ENDIF}
        Click;
      end;
    LBN_DBLCLK: DblClick;
  end;
end;

procedure TRxCustomListBox.WMPaint(var Message: TWMPaint);

  procedure PaintListBox;
  var
    DrawItemMsg: TWMDrawItem;
    MeasureItemMsg: TWMMeasureItem;
    DrawItemStruct: TDrawItemStruct;
    MeasureItemStruct: TMeasureItemStruct;
    R: TRect;
    Y, I, H, W: Integer;
  begin
    { Initialize drawing records }
    DrawItemMsg.Msg := CN_DRAWITEM;
    DrawItemMsg.DrawItemStruct := @DrawItemStruct;
    DrawItemMsg.Ctl := Handle;
    DrawItemStruct.CtlType := ODT_LISTBOX;
    DrawItemStruct.itemAction := ODA_DRAWENTIRE;
    DrawItemStruct.itemState := 0;
    DrawItemStruct.hDC := Message.DC;
    DrawItemStruct.CtlID := Handle;
    DrawItemStruct.hwndItem := Handle;
    { Intialize measure records }
    MeasureItemMsg.Msg := CN_MEASUREITEM;
    MeasureItemMsg.IDCtl := Handle;
    MeasureItemMsg.MeasureItemStruct := @MeasureItemStruct;
    MeasureItemStruct.CtlType := ODT_LISTBOX;
    MeasureItemStruct.CtlID := Handle;
    { Draw the listbox }
    Y := 0;
    I := TopIndex;
    GetClipBox(Message.DC, R);
    H := Height;
    W := Width;
    while Y < H do
    begin
      MeasureItemStruct.itemID := I;
      if I < Items.Count then
        MeasureItemStruct.itemData := {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF}(Pointer(Items.Objects[I]));
      MeasureItemStruct.itemWidth := W;
      MeasureItemStruct.itemHeight := FItemHeight;
      DrawItemStruct.itemData := MeasureItemStruct.itemData;
      DrawItemStruct.itemID := I;
      Dispatch(MeasureItemMsg);
      DrawItemStruct.rcItem := Rect(0, Y, MeasureItemStruct.itemWidth,
        Y + Integer(MeasureItemStruct.itemHeight));
      Dispatch(DrawItemMsg);
      Inc(Y, MeasureItemStruct.itemHeight);
      Inc(I);
      if I >= Items.Count then Break;
    end;
  end;

begin
  if Message.DC <> 0 then
    PaintListBox
  else
    inherited;
end;

procedure TRxCustomListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  SetColumnWidth;
end;

procedure TRxCustomListBox.DragCanceled;
var
  M: TWMMouse;
  {$IFNDEF VER80}
  MousePos: TPoint;
  {$ENDIF}
begin
  with M do
  begin
    Msg := WM_LBUTTONDOWN;
    {$IFNDEF VER80}
    GetCursorPos(MousePos);
    Pos := PointToSmallPoint(ScreenToClient(MousePos));
    {$ELSE}
    GetCursorPos(Pos);
    Pos := ScreenToClient(Pos);
    {$ENDIF}
    Keys := 0;
    Result := 0;
  end;
  DefaultHandler(M);
  M.Msg := WM_LBUTTONUP;
  DefaultHandler(M);
end;

procedure TRxCustomListBox.DefaultDrawText(X, Y: Integer; const S: string);
var
  ATabWidth: LongInt;
begin
  {$IFDEF RX_D4}
  TControlCanvas(FCanvas).UpdateTextFlags;
  {$ENDIF}
  if FTabWidth = 0 then
    FCanvas.TextOut(X, Y, S)
  else
  begin
    ATabWidth := Round((TabWidth * Canvas.TextWidth('0')) * 0.25);
    TabbedTextOut(FCanvas.Handle, X, Y, @S[1], Length(S), 1, ATabWidth, X);
  end;
end;

procedure TRxCustomListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Assigned(FOnDrawItem) then
    FOnDrawItem(Self, Index, Rect, State)
  else
  begin
    FCanvas.FillRect(Rect);
    if Index < Items.Count then
    begin
      {$IFDEF RX_D4}
      if not UseRightToLeftAlignment then
        Inc(Rect.Left, 2)
      else
        Dec(Rect.Right, 2);
      {$ELSE}
      Inc(Rect.Left, 2);
      {$ENDIF}
      DefaultDrawText(Rect.Left, Max(Rect.Top, (Rect.Bottom +
        Rect.Top - Canvas.TextHeight('Wy')) div 2), Items[Index]);
    end;
  end;
end;

procedure TRxCustomListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  if Assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Index, Height)
end;

procedure TRxCustomListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    {$IFNDEF VER80}
    {$IFDEF RX_D5}
    State := TOwnerDrawState(LongRec(itemState).Lo);
    {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ENDIF}
    {$ELSE}
    State := TOwnerDrawState(WordRec(itemState).Lo);
    {$ENDIF}
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      with FCanvas do
        if not (csDesigning in ComponentState) and FGraySelection and
          not Focused then
        begin
          Brush.Color := clBtnFace;
          if ColorToRGB(Font.Color) = ColorToRGB(clBtnFace) then
            Font.Color := clBtnText;
        end
        else
        begin
          Brush.Color := clHighlight;
          Font.Color := clHighlightText
        end;
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State)
    else
      FCanvas.FillRect(rcItem);
    if odFocused in State then DrawFocusRect(hDC, rcItem);
    FCanvas.Handle := 0;
  end;
end;

procedure TRxCustomListBox.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemHeight := FItemHeight;
    if FStyle = lbOwnerDrawVariable then
      MeasureItem(itemID, Integer(itemHeight));
  end;
end;

procedure TRxCustomListBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if FGraySelection and MultiSelect and (SelCount > 1) then Invalidate;
end;

procedure TRxCustomListBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if FGraySelection and MultiSelect and (SelCount > 1) then Invalidate;
end;

{$IFNDEF VER80}
procedure TRxCustomListBox.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;
{$ENDIF}

{ TCheckListBoxItem }

type
  TCheckListBoxItem = class
  private
    FData: {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF};
    FState: TCheckBoxState;
    FEnabled: Boolean;
    function GetChecked: Boolean;
  public
    constructor Create;
    property Checked: Boolean read GetChecked;
    property Enabled: Boolean read FEnabled write FEnabled;
    property State: TCheckBoxState read FState write FState;
  end;

constructor TCheckListBoxItem.Create;
begin
  inherited Create;
  FState := clbDefaultState;
  FEnabled := clbDefaultEnabled;
end;

function TCheckListBoxItem.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;

{ TCheckListBoxStrings }

type
  TCheckListBoxStrings = class(TRxListBoxStrings)
  public
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

procedure TCheckListBoxStrings.Exchange(Index1, Index2: Integer);
var
  TempEnabled1, TempEnabled2: Boolean;
  TempState1, TempState2: TCheckBoxState;
  {$IFNDEF VER80}TempHint1, TempHint2: string; {$ENDIF}
begin
  with TRxCheckListBox(ListBox) do
  begin
    TempState1 := State[Index1];
    TempEnabled1 := EnabledItem[Index1];
    TempState2 := State[Index2];
    TempEnabled2 := EnabledItem[Index2];
    {$IFNDEF VER80}
    TempHint1 := ItemHint[Index1];
    TempHint2 := ItemHint[Index2];
    {$ENDIF}
    inherited Exchange(Index1, Index2);
    {$IFNDEF VER80}
    ItemHint[Index2] := TempHint1;
    ItemHint[Index1] := TempHint2;
    {$ENDIF}
    State[Index1] := TempState2;
    EnabledItem[Index1] := TempEnabled2;
    State[Index2] := TempState1;
    EnabledItem[Index2] := TempEnabled1;
  end;
end;

procedure TCheckListBoxStrings.Move(CurIndex, NewIndex: Integer);
var
  TempEnabled: Boolean;
  TempState: TCheckBoxState;
  {$IFNDEF VER80}TempHint: string; {$ENDIF}
begin
  with TRxCheckListBox(ListBox) do
  begin
    {$IFNDEF VER80}TempHint := ItemHint[CurIndex]; {$ENDIF}
    TempState := State[CurIndex];
    TempEnabled := EnabledItem[CurIndex];
    inherited Move(CurIndex, NewIndex);
    State[NewIndex] := TempState;
    EnabledItem[NewIndex] := TempEnabled;
    {$IFNDEF VER80}ItemHint[NewIndex] := TempHint; {$ENDIF}
  end;
end;

{$IFNDEF VER80}
procedure TRxCustomListBox.CMHintShow(var Message: TMessage);
var
  ItemNo, ItCount: Integer;
  hasMain, hasItem, goodItemNo: Boolean;
  strItem, TheHint: string; Choice: (cNo, cM, cI);
begin
  inherited;
  TheHint := TCMHintShow(Message).HintInfo^.HintStr + '|';
  ItemNo := ItemAtPos(TCMHintShow(Message).HintInfo^.CursorPos, False); // X&Y are expected in Client coordinates
  hasItem := False; ItCount := FItems.Count;
  goodItemno := (ItemNo >= 0) and (ItemNo <= ItCount);
  if (ItemNo >= 0) and (ItemNo < ItCount) then
  begin
    strItem := ItemHint[ItemNo];
    hasItem := strItem <> '';
  end;

  if FHintSource <> hsDefault then
  begin
    if TCMHintShow(Message).Result = 1 then Exit; // CanShow = False?

    with TCMHintShow(Message).HintInfo^ do
    begin
      if TControl(Self) <> HintControl then Exit; //strange, hint requested by other component. Why should we deal with it?
      hasMain := Self.Hint <> '';

      Choice := cNo; // which one to use?
      if ((Self.FHintSource = hsItemsMain) and hasMain) then Choice := cM; // auxlary variants
      if ((Self.FHintSource = hsMainItems) and hasItem) then Choice := cI; // followin there are main variants

      if (hasMain and ((Self.FHintSource = hsMainItems) or (Self.FHintSource = hsMainControl))) then
        Choice := cM;
      if (hasItem and ((Self.FHintSource = hsItemsMain) or (Self.FHintSource = hsItems))) then
        Choice := cI;

      if Choice = cM then
        TheHint := Self.Hint
      else if Choice = cI then
        TheHint := strItem;
    end; //with
    if goodItemNo and Assigned(FOnGetItemHintEvent) then
      FOnGetItemhintEvent(Self, ItemNo, TheHint);
    TCMHintShow(Message).HintInfo^.HintStr := GetShortHint(TheHint);
  end; // querying hintSource
end;

function TRxCustomListBox.GetItemHint(Index: Integer): string;
begin
  if (FItems is TRxListboxStrings) and (Index <> -1) then
    Result := (FItems as TRxListboxStrings).getHint(Index)
  else
    Result := '';
end;

procedure TRxCustomListBox.SetItemHint(Index: Integer;
  const Value: string);
begin
  if FItems is TRxListboxStrings then
    (FItems as TRxListboxStrings).SetHint(Index, Value);
end;
{$ENDIF}

{ TRxCheckListBox }

const
  FCheckBitmap: TBitmap = nil;

function CheckBitmap: TBitmap;
begin
  if FCheckBitmap = nil then
  begin
    FCheckBitmap := TBitmap.Create;
    FCheckBitmap.Handle := LoadBitmap(hInstance, 'CHECK_IMAGES');
  end;
  Result := FCheckBitmap;
end;

procedure DestroyLocals; far;
begin
  if FCheckBitmap <> nil then
  begin
    FCheckBitmap.Free;
    FCheckBitmap := nil;
  end;
end;

const
  InternalVersion = 202; { for backward compatibility only }

constructor TRxCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoScroll := True;
  with CheckBitmap do
  begin
    FCheckWidth := Width div 6;
    FCheckHeight := Height div 3;
  end;
  FDrawBitmap := TBitmap.Create;
  with FDrawBitmap do
  begin
    Width := FCheckWidth;
    Height := FCheckHeight;
  end;
  FIniLink := TIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
end;

destructor TRxCheckListBox.Destroy;
begin
  FSaveStates.Free;
  FSaveStates := nil;
  FDrawBitmap.Free;
  FDrawBitmap := nil;
  FIniLink.Free;
  inherited Destroy;
end;

procedure TRxCheckListBox.Loaded;
begin
  inherited Loaded;
  UpdateCheckStates;
end;

function TRxCheckListBox.CreateItemList: TStrings;
begin
  Result := TCheckListBoxStrings.Create;
end;

const
  sCount = 'Count';
  sItem = 'Item';

procedure TRxCheckListBox.InternalSaveStates(IniFile: TObject;
  const Section: string);
var
  I: Integer;
begin
  IniEraseSection(IniFile, Section);
  IniWriteInteger(IniFile, Section, sCount, Items.Count);
  for I := 0 to Items.Count - 1 do
    IniWriteInteger(IniFile, Section, sItem + IntToStr(I), Integer(State[I]));
end;

procedure TRxCheckListBox.InternalRestoreStates(IniFile: TObject;
  const Section: string);
var
  I: Integer;
  ACount: Integer;
begin
  ACount := Min(IniReadInteger(IniFile, Section, sCount, 0), Items.Count);
  for I := 0 to ACount - 1 do
  begin
    State[I] := TCheckBoxState(IniReadInteger(IniFile, Section,
      sItem + IntToStr(I), Integer(clbDefaultState)));
    if (State[I] = cbChecked) and (FCheckKind = ckRadioButtons) then Exit;
  end;
end;

{$IFNDEF VER80}
procedure TRxCheckListBox.SaveStatesReg(IniFile: TRegIniFile);
begin
  InternalSaveStates(IniFile, GetDefaultSection(Self));
end;

procedure TRxCheckListBox.RestoreStatesReg(IniFile: TRegIniFile);
begin
  InternalRestoreStates(IniFile, GetDefaultSection(Self));
end;
{$ENDIF}

procedure TRxCheckListBox.SaveStates(IniFile: TIniFile);
begin
  InternalSaveStates(IniFile, GetDefaultSection(Self));
end;

procedure TRxCheckListBox.RestoreStates(IniFile: TIniFile);
begin
  InternalRestoreStates(IniFile, GetDefaultSection(Self));
end;

function TRxCheckListBox.GetStorage: TFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TRxCheckListBox.SetStorage(Value: TFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TRxCheckListBox.IniSave(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalSaveStates(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TRxCheckListBox.IniLoad(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalRestoreStates(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TRxCheckListBox.ReadCheckData(Reader: TReader);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    Reader.ReadListBegin;
    Clear;
    while not Reader.EndOfList do
    begin
      I := Items.Add(Reader.ReadString);
      if FReserved >= InternalVersion then
      begin
        {$IFNDEF VER80} {my addition}
        if Reader.NextValue in [vaLString, vaString] then ItemHint[i] := Reader.ReadString;
        {$ENDIF}
        State[I] := TCheckBoxState(Reader.ReadInteger);
        EnabledItem[I] := Reader.ReadBoolean;
      end
      else
      begin { for backward compatibility only }
        Checked[I] := Reader.ReadBoolean;
        EnabledItem[I] := Reader.ReadBoolean;
        if FReserved > 0 then
          State[I] := TCheckBoxState(Reader.ReadInteger);
      end;
    end;
    Reader.ReadListEnd;
    UpdateCheckStates;
  finally
    Items.EndUpdate;
  end;
end;

procedure TRxCheckListBox.WriteCheckData(Writer: TWriter);
var
  I: Integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for I := 0 to Items.Count - 1 do
    begin
      WriteString(Items[I]);
      {$IFNDEF VER80} {my addition}
      if ItemHint[I] <> '' then WriteString(ItemHint[I]);
      {$ENDIF}
      WriteInteger(Integer(Self.State[I]));
      WriteBoolean(EnabledItem[I]);
    end;
    WriteListEnd;
  end;
end;

procedure TRxCheckListBox.ReadVersion(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TRxCheckListBox.WriteVersion(Writer: TWriter);
begin
  Writer.WriteInteger(InternalVersion);
end;

procedure TRxCheckListBox.DefineProperties(Filer: TFiler);

{$IFNDEF VER80}

function DoWrite: Boolean;
  var
    I: Integer;
    Ancestor: TRxCheckListBox;
  begin
    Result := False;
    Ancestor := TRxCheckListBox(Filer.Ancestor);
    if (Ancestor <> nil) and (Ancestor.Items.Count = Items.Count) and
      (Ancestor.Items.Count > 0) then
      for I := 1 to Items.Count - 1 do
      begin
        Result := (CompareText(Items[I], Ancestor.Items[I]) <> 0) or
          (State[I] <> Ancestor.State[I]) or
          (EnabledItem[I] <> Ancestor.EnabledItem[I])
          (* my addition *)
        or (CompareText(ItemHint[I], Ancestor.ItemHint[I]) <> 0);
        if Result then Break;
      end
    else
      Result := Items.Count > 0;
  end;
  {$ENDIF}

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('InternalVersion', ReadVersion, WriteVersion,
    {$IFNDEF VER80}Filer.Ancestor = nil{$ELSE}True{$ENDIF});
  Filer.DefineProperty('Strings', ReadCheckData, WriteCheckData,
    {$IFNDEF VER80}DoWrite{$ELSE}Items.Count > 0{$ENDIF});
end;

procedure TRxCheckListBox.CreateWnd;
begin
  inherited CreateWnd;
  if FSaveStates <> nil then
  begin
    FSaveStates.Free;
    FSaveStates := nil;
  end;
  ResetItemHeight;
end;

procedure TRxCheckListBox.DestroyWnd;
begin
  inherited DestroyWnd;
end;

procedure TRxCheckListBox.WMDestroy(var Msg: TWMDestroy);
var
  I: Integer;
begin
  if Items.Count > 0 then
  begin
    if FSaveStates <> nil then
      FSaveStates.Clear
    else
      FSaveStates := TList.Create;
    for I := 0 to Items.Count - 1 do
    begin
      FSaveStates.Add(TObject(MakeLong(Ord(EnabledItem[I]), Word(State[I]))));
      FindCheckObject(I).Free;
    end;
  end;
  inherited;
end;

procedure TRxCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if Style and (LBS_OWNERDRAWFIXED or LBS_OWNERDRAWVARIABLE) = 0 then
      Style := Style or LBS_OWNERDRAWFIXED;
end;

procedure TRxCheckListBox.SetItems(Value: TStrings);
var
  I: Integer;
begin
  Items.BeginUpdate;
  try
    inherited SetItems(Value);
    if (Value <> nil) and (Value is TRxListBoxStrings) and
      (TRxListBoxStrings(Value).ListBox <> nil) and
      (TRxListBoxStrings(Value).ListBox is TRxCheckListBox) then
    begin
      for I := 0 to Items.Count - 1 do
        if I < Value.Count then
        begin
          Self.State[I] := TRxCheckListBox(TRxListBoxStrings(Value).ListBox).State[I];
          EnabledItem[I] := TRxCheckListBox(TRxListBoxStrings(Value).ListBox).EnabledItem[I];
        end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TRxCheckListBox.GetItemWidth(Index: Integer): Integer;
begin
  Result := inherited GetItemWidth(Index) + GetCheckWidth;
end;

function TRxCheckListBox.GetCheckWidth: Integer;
begin
  Result := FCheckWidth + 2;
end;

function TRxCheckListBox.GetAllowGrayed: Boolean;
begin
  Result := FAllowGrayed and (FCheckKind in [ckCheckBoxes, ckCheckMarks]);
end;

procedure TRxCheckListBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResetItemHeight;
end;

function TRxCheckListBox.GetItemHeight: Integer;
var
  R: TRect;
begin
  Result := FItemHeight;
  if HandleAllocated and ((FStyle = lbStandard) or
    ((FStyle = lbOwnerDrawFixed) and not Assigned(FOnDrawItem))) then
  begin
    Perform(LB_GETITEMRECT, 0, LPARAM(@R));
    Result := R.Bottom - R.Top;
  end;
end;

procedure TRxCheckListBox.ResetItemHeight;
var
  H: Integer;
begin
  if (Style = lbStandard) or ((Style = lbOwnerDrawFixed) and
    not Assigned(FOnDrawItem)) then
  begin
    Canvas.Font := Font;
    H := Max(Canvas.TextHeight('Wg'), FCheckHeight);
    if Style = lbOwnerDrawFixed then H := Max(H, FItemHeight);
    Perform(LB_SETITEMHEIGHT, 0, LPARAM(H));
    if (H * Items.Count) <= ClientHeight then
      SetScrollRange(Handle, SB_VERT, 0, 0, True);
  end;
end;

procedure TRxCheckListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  SaveEvent: TDrawItemEvent;
begin
  if Index < Items.Count then
  begin
    R := Rect;
    {$IFDEF RX_D4}
    if not UseRightToLeftAlignment then
    begin
      R.Right := Rect.Left;
      R.Left := R.Right - GetCheckWidth;
    end
    else
    begin
      R.Left := Rect.Right;
      R.Right := R.Left + GetCheckWidth;
    end;
    {$ELSE}
    R.Right := Rect.Left;
    R.Left := R.Right - GetCheckWidth;
    {$ENDIF}
    DrawCheck(R, GetState(Index), EnabledItem[Index]);
    if not EnabledItem[Index] then
      if odSelected in State then
        Canvas.Font.Color := clInactiveCaptionText
      else
        Canvas.Font.Color := clGrayText;
  end;
  if (Style = lbStandard) and Assigned(FOnDrawItem) then
  begin
    SaveEvent := OnDrawItem;
    OnDrawItem := nil;
    try
      inherited DrawItem(Index, Rect, State);
    finally
      OnDrawItem := SaveEvent;
    end;
  end
  else
    inherited DrawItem(Index, Rect, State);
end;

procedure TRxCheckListBox.CNDrawItem(var Message: TWMDrawItem);
begin
  with Message.DrawItemStruct^ do
    {$IFDEF RX_D4}
    if not UseRightToLeftAlignment then
      rcItem.Left := rcItem.Left + GetCheckWidth
    else
      rcItem.Right := rcItem.Right - GetCheckWidth;
  {$ELSE}
    rcItem.Left := rcItem.Left + GetCheckWidth;
  {$ENDIF}
  inherited;
end;

procedure TRxCheckListBox.DrawCheck(R: TRect; AState: TCheckBoxState;
  Enabled: Boolean);
const
  CheckImages: array[TCheckBoxState, TCheckKind, Boolean] of Integer =
  (((3, 0), (9, 6), (15, 12)), { unchecked }
    ((4, 1), (10, 7), (16, 13)), { checked   }
    ((5, 2), (11, 8), (17, 14))); { grayed    }
var
  DrawRect: TRect;
  SaveColor: TColor;
begin
  DrawRect.Left := R.Left + (R.Right - R.Left - FCheckWidth) div 2;
  DrawRect.Top := R.Top + (R.Bottom - R.Top - FCheckHeight) div 2;
  DrawRect.Right := DrawRect.Left + FCheckWidth;
  DrawRect.Bottom := DrawRect.Top + FCheckHeight;
  SaveColor := Canvas.Brush.Color;
  AssignBitmapCell(CheckBitmap, FDrawBitmap, 6, 3,
    CheckImages[AState, FCheckKind, Enabled]);
  Canvas.Brush.Color := Self.Color;
  try
    Canvas.BrushCopy(DrawRect, FDrawBitmap, Bounds(0, 0, FCheckWidth,
      FCheckHeight), CheckBitmap.TransparentColor and not PaletteMask);
  finally
    Canvas.Brush.Color := SaveColor;
  end;
end;

procedure TRxCheckListBox.ApplyState(AState: TCheckBoxState;
  EnabledOnly: Boolean);
var
  I: Integer;
begin
  if FCheckKind in [ckCheckBoxes, ckCheckMarks] then
    for I := 0 to Items.Count - 1 do
      if not EnabledOnly or EnabledItem[I] then
      begin
        State[I] := AState;
      end;
end;

function TRxCheckListBox.GetCheckedIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  if FCheckKind = ckRadioButtons then
    for I := 0 to Items.Count - 1 do
      if State[I] = cbChecked then
      begin
        Result := I;
        Exit;
      end;
end;

procedure TRxCheckListBox.SetCheckedIndex(Value: Integer);
begin
  if (FCheckKind = ckRadioButtons) and (Items.Count > 0) then
    SetState(Max(Value, 0), cbChecked);
end;

procedure TRxCheckListBox.UpdateCheckStates;
begin
  if (FCheckKind = ckRadioButtons) and (Items.Count > 0) then
  begin
    FInUpdateStates := True;
    try
      SetState(Max(GetCheckedIndex, 0), cbChecked);
    finally
      FInUpdateStates := False;
    end;
  end;
end;

procedure TRxCheckListBox.SetCheckKind(Value: TCheckKind);
begin
  if FCheckKind <> Value then
  begin
    FCheckKind := Value;
    UpdateCheckStates;
    Invalidate;
  end;
end;

procedure TRxCheckListBox.SetChecked(Index: Integer; AChecked: Boolean);
const
  CheckStates: array[Boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
begin
  SetState(Index, CheckStates[AChecked]);
end;

procedure TRxCheckListBox.SetState(Index: Integer; AState: TCheckBoxState);
var
  I: Integer;
begin
  if (AState <> GetState(Index)) or FInUpdateStates then
  begin
    if (FCheckKind = ckRadioButtons) and (AState = cbUnchecked) and
      (GetCheckedIndex = Index) then Exit;
    TCheckListBoxItem(GetCheckObject(Index)).State := AState;
    if (FCheckKind = ckRadioButtons) and (AState = cbChecked) then
      for I := Items.Count - 1 downto 0 do
      begin
        if (I <> Index) and (GetState(I) = cbChecked) then
        begin
          TCheckListBoxItem(GetCheckObject(I)).State := cbUnchecked;
          InvalidateCheck(I);
        end;
      end;
    InvalidateCheck(Index);
    if not (csReading in ComponentState) then ChangeItemState(Index);
  end;
end;

procedure TRxCheckListBox.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  if Value <> GetItemEnabled(Index) then
  begin
    TCheckListBoxItem(GetCheckObject(Index)).Enabled := Value;
    InvalidateItem(Index);
  end;
end;

procedure TRxCheckListBox.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  {$IFDEF RX_D4}
  if not UseRightToLeftAlignment then
    R.Right := R.Left + GetCheckWidth
  else
    R.Left := R.Right - GetCheckWidth;
  {$ELSE}
  R.Right := R.Left + GetCheckWidth;
  {$ENDIF}
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  UpdateWindow(Handle);
end;

procedure TRxCheckListBox.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  InvalidateRect(Handle, @R, not (csOpaque in ControlStyle));
  UpdateWindow(Handle);
end;

function TRxCheckListBox.GetChecked(Index: Integer): Boolean;
begin
  if IsCheckObject(Index) then
    Result := TCheckListBoxItem(GetCheckObject(Index)).GetChecked
  else
    Result := False;
end;

function TRxCheckListBox.GetState(Index: Integer): TCheckBoxState;
begin
  if IsCheckObject(Index) then
    Result := TCheckListBoxItem(GetCheckObject(Index)).State
  else
    Result := clbDefaultState;
  if (FCheckKind = ckRadioButtons) and (Result <> cbChecked) then
    Result := cbUnchecked;
end;

function TRxCheckListBox.GetItemEnabled(Index: Integer): Boolean;
begin
  if IsCheckObject(Index) then
    Result := TCheckListBoxItem(GetCheckObject(Index)).Enabled
  else
    Result := clbDefaultEnabled;
end;

procedure TRxCheckListBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ' ':
      begin
        ToggleClickCheck(ItemIndex);
        Key := #0;
      end;
    '+':
      begin
        ApplyState(cbChecked, True);
        ClickCheck;
      end;
    '-':
      begin
        ApplyState(cbUnchecked, True);
        ClickCheck;
      end;
  end;
end;

procedure TRxCheckListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Index: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    Index := ItemAtPos(Point(X, Y), True);
    if (Index <> -1) then
    begin
      {$IFDEF RX_D4}
      if not UseRightToLeftAlignment then
      begin
        if X - ItemRect(Index).Left < GetCheckWidth then
          ToggleClickCheck(Index);
      end
      else
      begin
        Dec(X, ItemRect(Index).Right - GetCheckWidth);
        if (X > 0) and (X < GetCheckWidth) then
          ToggleClickCheck(Index);
      end;
      {$ELSE}
      if X - ItemRect(Index).Left < GetCheckWidth then
        ToggleClickCheck(Index);
      {$ENDIF}
    end;
  end;
end;

procedure TRxCheckListBox.ToggleClickCheck(Index: Integer);
var
  State: TCheckBoxState;
begin
  if (Index >= 0) and (Index < Items.Count) and EnabledItem[Index] then
  begin
    State := Self.State[Index];
    case State of
      cbUnchecked:
        if AllowGrayed then
          State := cbGrayed
        else
          State := cbChecked;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
    end;
    Self.State[Index] := State;
    ClickCheck;
  end;
end;

procedure TRxCheckListBox.ChangeItemState(Index: Integer);
begin
  if Assigned(FOnStateChange) then FOnStateChange(Self, Index);
end;

procedure TRxCheckListBox.ClickCheck;
begin
  if Assigned(FOnClickCheck) then FOnClickCheck(Self);
end;

function TRxCheckListBox.GetItemData(Index: Integer): {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF};
var
  Item: TCheckListBoxItem;
begin
  Result := 0;
  if IsCheckObject(Index) then
  begin
    Item := TCheckListBoxItem(GetCheckObject(Index));
    if Item <> nil then Result := Item.FData;
  end;
end;

function TRxCheckListBox.GetCheckObject(Index: Integer): TObject;
begin
  Result := FindCheckObject(Index);
  if Result = nil then Result := CreateCheckObject(Index);
end;

function TRxCheckListBox.FindCheckObject(Index: Integer): TObject;
var
  ItemData: {$IFDEF WIN64}IntPtr{$ELSE}LongInt{$ENDIF};
begin
  Result := nil;
  ItemData := inherited GetItemData(Index);
  if ItemData = LB_ERR then
    ListIndexError(Index)
  else
  begin
    Result := TCheckListBoxItem(ItemData);
    if not (Result is TCheckListBoxItem) then Result := nil;
  end;
end;

function TRxCheckListBox.CreateCheckObject(Index: Integer): TObject;
begin
  Result := TCheckListBoxItem.Create;
  inherited SetItemData(Index, {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF}(Result));
end;

function TRxCheckListBox.IsCheckObject(Index: Integer): Boolean;
begin
  Result := FindCheckObject(Index) <> nil;
end;

procedure TRxCheckListBox.SetItemData(Index: Integer; AData: {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF});
var
  Item: TCheckListBoxItem;
  L: LongInt;
begin
  Item := TCheckListBoxItem(GetCheckObject(Index));
  Item.FData := AData;
  if (FSaveStates <> nil) and (FSaveStates.Count > 0) then
  begin
    L := LongInt(Pointer(FSaveStates[0]));
    Item.FState := TCheckBoxState(LongRec(L).Hi);
    Item.FEnabled := LongRec(L).Lo <> 0;
    FSaveStates.Delete(0);
  end;
end;

procedure TRxCheckListBox.ResetContent;
var
  I: Integer;
begin
  for I := Items.Count - 1 downto 0 do
  begin
    if IsCheckObject(I) then GetCheckObject(I).Free;
    inherited SetItemData(I, 0);
  end;
  inherited ResetContent;
end;

procedure TRxCheckListBox.DeleteString(Index: Integer);
begin
  if IsCheckObject(Index) then GetCheckObject(Index).Free;
  inherited SetItemData(Index, 0);
  inherited DeleteString(Index);
end;

{ TRxCustomLabel }

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer; {$IFDEF RX_D9}inline; {$ENDIF}
var
  RText, RShadow: TRect;
  Color: TColorRef;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(DC, ShadowColor);
  case ShadowPos of
    spLeftTop: OffsetRect(RShadow, -ShadowSize, -ShadowSize);
    spRightBottom: OffsetRect(RShadow, ShadowSize, ShadowSize);
    spLeftBottom:
      begin
        {OffsetRect(RText, ShadowSize, 0);}
        OffsetRect(RShadow, -ShadowSize, ShadowSize);
      end;
    spRightTop:
      begin
        {OffsetRect(RText, 0, ShadowSize);}
        OffsetRect(RShadow, ShadowSize, -ShadowSize);
      end;
  end; { case }
  Result := DrawText(DC, Str, Count, RShadow, Format);
  if Result > 0 then Inc(Result, ShadowSize);
  SetTextColor(DC, Color);
  DrawText(DC, Str, Count, RText, Format);
  UnionRect(Rect, RText, RShadow);
end;

constructor TRxCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  FShadowColor := clBtnHighlight;
  FShadowSize := 1;
  FShadowPos := spLeftTop;
end;

function TRxCustomLabel.GetLabelCaption: string;
begin
  Result := Caption;
end;

function TRxCustomLabel.GetDefaultFontColor: TColor;
begin
  Result := Font.Color;
end;

procedure TRxCustomLabel.DoDrawText(var Rect: TRect; Flags: Word);
var
  {$IFNDEF VER80}
  Text: string;
  {$ELSE}
  Text: array[0..255] of Char;
  {$ENDIF}
  PosShadow: TShadowPosition;
  SizeShadow: Byte;
  ColorShadow: TColor;
begin
  {$IFNDEF VER80}
  Text := GetLabelCaption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + ' ';
  {$ELSE}
  StrPLCopy(Text, GetLabelCaption, 255);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or FShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then
    StrCopy(Text, ' ');
  {$ENDIF}
  if not FShowAccelChar then Flags := Flags or DT_NOPREFIX;
  {$IFDEF RX_D4}
  Flags := DrawTextBiDiModeFlags(Flags);
  {$ENDIF}
  Canvas.Font := Font;
  Canvas.Font.Color := GetDefaultFontColor;
  PosShadow := FShadowPos;
  SizeShadow := FShadowSize;
  ColorShadow := FShadowColor;
  if not Enabled then
  begin
    if (FShadowSize = 0) and NewStyleControls then
    begin
      PosShadow := spRightBottom;
      SizeShadow := 1;
    end;
    Canvas.Font.Color := clGrayText;
    ColorShadow := clBtnHighlight;
  end;
  {$IFNDEF VER80}
  DrawShadowText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
  {$ELSE}
  DrawShadowText(Canvas.Handle, Text, StrLen(Text), Rect, Flags,
    SizeShadow, ColorToRGB(ColorShadow), PosShadow);
  {$ENDIF}
end;

procedure TRxCustomLabel.Paint;
var
  Rect: TRect;
  DrawStyle: Integer;
begin
  if not Enabled and not (csDesigning in ComponentState) then
    FDragging := False;
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    Inc(Rect.Left, FLeftMargin);
    Dec(Rect.Right, FRightMargin);
    InflateRect(Rect, -1, 0);
    DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    { Calculate vertical layout }
    if FLayout <> tlTop then
    begin
      DoDrawText(Rect, DrawStyle or DT_CALCRECT);
      Rect.Left := ClientRect.Left + FLeftMargin;
      Rect.Right := ClientRect.Right - FRightMargin;
      if FLayout = tlBottom then
        OffsetRect(Rect, 0, Height - Rect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - Rect.Bottom) div 2);
    end;
    DoDrawText(Rect, DrawStyle);
    if FShowFocus and Assigned(FFocusControl) and FFocused and not (csDesigning in ComponentState) then
    begin
      InflateRect(Rect, 1, 0);
      {$IFNDEF VER80}
      Brush.Color := Self.Color;
      {$ENDIF}
      DrawFocusRect(Rect);
    end;
  end;
end;

procedure TRxCustomLabel.AdjustBounds;
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if AutoSize then
  begin
    Rect := ClientRect;
    Inc(Rect.Left, FLeftMargin);
    Dec(Rect.Right, FRightMargin);
    InflateRect(Rect, -1, 0);
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, DT_EXPANDTABS or DT_CALCRECT or WordWraps[FWordWrap]);
    Dec(Rect.Left, FLeftMargin);
    Inc(Rect.Right, FRightMargin);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    InflateRect(Rect, 1, 0);
    X := Left;
    AAlignment := FAlignment;
    {$IFDEF RX_D4}
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    {$ENDIF}
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TRxCustomLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;

procedure TRxCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetLeftMargin(Value: Integer);
begin
  if FLeftMargin <> Value then
  begin
    FLeftMargin := Max(Value, 0);
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetRightMargin(Value: Integer);
begin
  if FRightMargin <> Value then
  begin
    FRightMargin := Max(Value, 0);
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetShadowSize(Value: Byte);
begin
  if Value <> FShadowSize then
  begin
    FShadowSize := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetShadowPos(Value: TShadowPosition);
begin
  if Value <> FShadowPos then
  begin
    FShadowPos := Value;
    Invalidate;
  end;
end;

function TRxCustomLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TRxCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
  if FShowFocus then Invalidate;
end;

procedure TRxCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetShowFocus(Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    Invalidate;
  end;
end;

procedure TRxCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
  end;
end;

procedure TRxCustomLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FocusControl := nil;
end;

procedure TRxCustomLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    FDragging := True;
  end;
end;

procedure TRxCustomLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging and (Button = mbLeft) then FDragging := False;
  UpdateTracking;
end;

procedure TRxCustomLabel.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TRxCustomLabel.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TRxCustomLabel.UpdateTracking;
var
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if (FMouseInControl <> OldValue) then
    if FMouseInControl then
      MouseEnter
    else
      MouseLeave;
end;

procedure TRxCustomLabel.CMFocusChanged(var Message: TCMFocusChanged);
var
  Active: Boolean;
begin
  Active := Assigned(FFocusControl) and (Message.Sender = FFocusControl);
  if FFocused <> Active then
  begin
    FFocused := Active;
    if FShowFocus then Invalidate;
  end;
  inherited;
end;

procedure TRxCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure TRxCustomLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TRxCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and IsAccel(Message.CharCode, GetLabelCaption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure TRxCustomLabel.WMRButtonDown(var Message: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TRxCustomLabel.WMRButtonUp(var Message: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

procedure TRxCustomLabel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateTracking;
end;

procedure TRxCustomLabel.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then UpdateTracking;
end;

procedure TRxCustomLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled and IsForegroundTask then
  begin
    FMouseInControl := True;
    MouseEnter;
  end;
end;

procedure TRxCustomLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    MouseLeave;
  end;
end;

{ TSecretPanel }

constructor TSecretPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollCnt := 0;
  FAlignment := taCenter;
  FActive := False;
  FTxtDivider := 1;
  FGlyphLayout := glGlyphLeft;
  ControlStyle := ControlStyle - [csSetCaption];
  BevelOuter := bvLowered;
  FTextStyle := bvNone;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FHiddenList := TList.Create;
  FTimer := TRxTimer.Create(Self);
  with FTimer do
  begin
    Enabled := False;
    OnTimer := TimerExpired;
    Interval := 30;
    {$IFDEF RX_D3}
    SyncEvent := False;
    FAsyncDrawing := True;
    {$ENDIF}
  end;
  {$IFDEF RX_D7}
  ParentBackground := False;
  {$ENDIF}
end;

destructor TSecretPanel.Destroy;
begin
  SetActive(False);
  FGlyph.OnChange := nil;
  FGlyph.Free;
  TStringList(FLines).OnChange := nil;
  FLines.Free;
  FHiddenList.Free;
  inherited Destroy;
end;

procedure TSecretPanel.GlyphChanged(Sender: TObject);
begin
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TSecretPanel.LinesChanged(Sender: TObject);
begin
  if Active then
  begin
    FScrollCnt := 0;
    UpdateMemoryImage;
    Invalidate;
  end;
end;

procedure TSecretPanel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Active then UpdateMemoryImage;
end;

procedure TSecretPanel.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if Active then UpdateMemoryImage;
end;

procedure TSecretPanel.WMSize(var Message: TMessage);
begin
  inherited;
  if Active then
  begin
    UpdateMemoryImage;
    Invalidate;
  end;
end;

{$IFDEF RX_D3}
procedure TSecretPanel.SetAsyncDrawing(Value: Boolean);
begin
  if FAsyncDrawing <> Value then
  begin
    FTimer.SyncEvent := not Value;
    FAsyncDrawing := Value;
  end;
end;
{$ENDIF RX_D3}

procedure TSecretPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
  if (AControl = nil) and Active then UpdateMemoryImage;
end;

function TSecretPanel.GetInflateWidth: Integer;
begin
  Result := BorderWidth;
  if BevelOuter <> bvNone then Inc(Result, BevelWidth);
  if BevelInner <> bvNone then Inc(Result, BevelWidth);
end;

procedure TSecretPanel.RecalcDrawRect;
const
  MinOffset = 3;
var
  InflateWidth: Integer;
  LastLine: Integer;
begin
  FTxtRect := GetClientRect;
  FPaintRect := FTxtRect;
  InflateWidth := GetInflateWidth;
  InflateRect(FPaintRect, -InflateWidth, -InflateWidth);
  Inc(InflateWidth, MinOffset);
  InflateRect(FTxtRect, -InflateWidth, -InflateWidth);
  with FGlyphOrigin do
  begin
    case FGlyphLayout of
      glGlyphLeft:
        begin
          X := FTxtRect.Left;
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then Y := FTxtRect.Top;
          if Glyph.Width > 0 then
          begin
            Inc(X, MinOffset);
            FTxtRect.Left := X + Glyph.Width + InflateWidth;
          end;
        end;
      glGlyphRight:
        begin
          Y := (FTxtRect.Bottom + FTxtRect.Top - Glyph.Height) div 2;
          if Y < FTxtRect.Top then Y := FTxtRect.Top;
          X := FTxtRect.Right - Glyph.Width;
          if Glyph.Width > 0 then
          begin
            Dec(X, MinOffset);
            if X < FTxtRect.Left then X := FTxtRect.Left;
            FTxtRect.Right := X - InflateWidth;
          end;
        end;
      glGlyphTop:
        begin
          Y := FTxtRect.Top;
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then X := FTxtRect.Left;
          if Glyph.Height > 0 then
          begin
            Inc(Y, MinOffset);
            FTxtRect.Top := Y + Glyph.Height + (InflateWidth + MinOffset);
          end;
        end;
      glGlyphBottom:
        begin
          X := (FTxtRect.Right + FTxtRect.Left - Glyph.Width) div 2;
          if X < FTxtRect.Left then X := FTxtRect.Left;
          Y := FTxtRect.Bottom - Glyph.Height;
          if Glyph.Height > 0 then
          begin
            Dec(Y, MinOffset);
            if Y < FTxtRect.Top then Y := FTxtRect.Top;
            FTxtRect.Bottom := Y - (InflateWidth + MinOffset);
          end;
        end;
    end;
  end;
  if FDirection = sdHorizontal then
  begin
    LastLine := FLines.Count - 1;
    while (LastLine >= 0) and (Trim(FLines[LastLine]) = '') do
      Dec(LastLine);
    InflateWidth := HeightOf(FTxtRect) -
      (LastLine + 1 - FFirstLine) * FTxtDivider;
    if InflateWidth > 0 then
      InflateRect(FTxtRect, 0, -InflateWidth div 2);
  end;
  with FTxtRect do
    if (Left >= Right) or (Top >= Bottom) then FTxtRect := Rect(0, 0, 0, 0);
end;

procedure TSecretPanel.PaintGlyph;
begin
  if not FGlyph.Empty then
  begin
    RecalcDrawRect;
    DrawBitmapTransparent(Canvas, FGlyphOrigin.X, FGlyphOrigin.Y,
      FGlyph, FGlyph.TransparentColor and not PaletteMask);
  end;
end;

procedure TSecretPanel.PaintText;
var
  STmp: array[0..1023] of Char;
  R: TRect;
  I: Integer;
  Flags: LongInt;
begin
  if (FLines.Count = 0) or IsRectEmpty(FTxtRect) or not HandleAllocated then
    Exit;
  {$IFDEF RX_D3}
  FMemoryImage.Canvas.Lock;
  try
    {$ENDIF}
    with FMemoryImage.Canvas do
    begin
      I := SaveDC(Handle);
      try
        with FTxtRect do
          MoveWindowOrg(Handle, -Left, -Top);
        Brush.Color := Self.Color;
        PaintClient(FMemoryImage.Canvas, FPaintRect);
      finally
        RestoreDC(Handle, I);
        SetBkMode(Handle, Transparent);
      end;
    end;
    R := Bounds(0, 0, WidthOf(FTxtRect), HeightOf(FTxtRect));
    if FDirection = sdHorizontal then
    begin
      {$IFDEF RX_D4}
      if IsRightToLeft then
      begin
        R.Right := R.Left + FScrollCnt;
        R.Left := R.Right - (FMaxScroll - WidthOf(FTxtRect));
      end
      else
      begin
        R.Left := R.Right - FScrollCnt;
        R.Right := R.Left + (FMaxScroll - WidthOf(FTxtRect));
      end;
      {$ELSE}
      R.Left := R.Right - FScrollCnt;
      R.Right := R.Left + (FMaxScroll - WidthOf(FTxtRect));
      {$ENDIF}
    end
    else
    begin { sdVertical }
      R.Top := R.Bottom - FScrollCnt;
    end;
    R.Bottom := R.Top + FTxtDivider;
    Flags := DT_EXPANDTABS or Alignments[FAlignment] or DT_SINGLELINE or
      DT_NOCLIP or DT_NOPREFIX;
    {$IFDEF RX_D4}
    Flags := DrawTextBiDiModeFlags(Flags);
    {$ENDIF}
    for I := FFirstLine to FLines.Count do
    begin
      if I = FLines.Count then
        StrCopy(STmp, ' ')
      else
        StrPLCopy(STmp, FLines[I], Length(STmp) - 1);
      if R.Top >= HeightOf(FTxtRect) then
        Break
      else if R.Bottom > 0 then
      begin
        if FTextStyle <> bvNone then
        begin
          FMemoryImage.Canvas.Font.Color := clBtnHighlight;
          case FTextStyle of
            bvLowered:
              begin
                OffsetRect(R, 1, 1);
                DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
                OffsetRect(R, -1, -1);
              end;
            bvRaised:
              begin
                OffsetRect(R, -1, -1);
                DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
                OffsetRect(R, 1, 1);
              end;
          end;
          FMemoryImage.Canvas.Font.Color := Self.Font.Color;
          SetBkMode(FMemoryImage.Canvas.Handle, Transparent);
        end;
        DrawText(FMemoryImage.Canvas.Handle, STmp, -1, R, Flags);
      end;
      OffsetRect(R, 0, FTxtDivider);
    end;
    {$IFDEF RX_D3}
    Canvas.Lock;
    try
      {$ENDIF}
      BitBlt(Canvas.Handle, FTxtRect.Left, FTxtRect.Top, FMemoryImage.Width,
        FMemoryImage.Height, FMemoryImage.Canvas.Handle, 0, 0, SRCCOPY);
      ValidateRect(Handle, @FTxtRect);
      {$IFDEF RX_D3}
    finally
      Canvas.Unlock;
    end;
    {$ENDIF}
    {$IFDEF RX_D3}
  finally
    FMemoryImage.Canvas.Unlock;
  end;
  {$ENDIF}
end;

procedure TSecretPanel.PaintClient(Canvas: TCanvas; Rect: TRect);
begin
  if Assigned(FOnPaintClient) then
    FOnPaintClient(Self, Canvas, Rect)
  else
    Canvas.FillRect(Rect);
end;

procedure TSecretPanel.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
  SaveIndex: Integer;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Self.Color, Self.Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  SaveIndex := SaveDC(Canvas.Handle);
  try
    with Rect do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    Canvas.Brush.Color := Self.Color;
    PaintClient(Canvas, Rect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
  if Active then
  begin
    PaintGlyph;
    {PaintText;}
  end;
end;

procedure TSecretPanel.StartPlay;
begin
  if Assigned(FOnStartPlay) then FOnStartPlay(Self);
end;

procedure TSecretPanel.StopPlay;
begin
  if Assigned(FOnStopPlay) then FOnStopPlay(Self);
end;

procedure TSecretPanel.TimerExpired(Sender: TObject);
begin
  if (FScrollCnt < FMaxScroll) then
  begin
    Inc(FScrollCnt);
    if Assigned(FMemoryImage) then PaintText;
  end
  else if Cycled then
  begin
    FScrollCnt := 0;
    if Assigned(FMemoryImage) then PaintText;
  end
  else
  begin
    {$IFDEF RX_D3}
    FTimer.Synchronize(Stop);
    {$ELSE}
    SetActive(False);
    {$ENDIF}
  end;
end;

procedure TSecretPanel.UpdateMemoryImage;
var
  Metrics: TTextMetric;
  I: Integer;
begin
  if FMemoryImage = nil then FMemoryImage := TBitmap.Create;
  {$IFDEF RX_D3}
  FMemoryImage.Canvas.Lock;
  try
    {$ENDIF}
    FFirstLine := 0;
    while (FFirstLine < FLines.Count) and (Trim(FLines[FFirstLine]) = '') do
      Inc(FFirstLine);
    Canvas.Font.Assign(Self.Font); //Canvas.Font := Self.Font;
    if GetTextMetrics(Canvas.Handle, Metrics) then
    begin
      FTxtDivider := Metrics.tmHeight + Metrics.tmExternalLeading;
      if FTextStyle <> bvNone then Inc(FTxtDivider);
      RecalcDrawRect;
      case FDirection of
        sdHorizontal:
          begin
            FMaxScroll := 0;
            for I := FFirstLine to FLines.Count - 1 do
              FMaxScroll := Max(FMaxScroll, Canvas.TextWidth(FLines[I]));
            Inc(FMaxScroll, WidthOf(FTxtRect));
          end;
        sdVertical:
          begin
            FMaxScroll := ((FLines.Count - FFirstLine) * FTxtDivider) +
              HeightOf(FTxtRect);
          end;
      end;

      FMemoryImage.Width := WidthOf(FTxtRect);
      FMemoryImage.Height := HeightOf(FTxtRect);
      with FMemoryImage.Canvas do
      begin
        Font.Assign(Self.Font);
        Brush.Color := Self.Color;
        SetBkMode(Handle, Transparent);
      end;
    end;
    {$IFDEF RX_D3}
  finally
    FMemoryImage.Canvas.UnLock;
  end;
  {$ENDIF}
end;

function TSecretPanel.GetInterval: Cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TSecretPanel.SetInterval(Value: Cardinal);
begin
  FTimer.Interval := Value;
end;

procedure TSecretPanel.Play;
begin
  SetActive(True);
end;

procedure TSecretPanel.Stop;
begin
  SetActive(False);
end;

procedure TSecretPanel.SetActive(Value: Boolean);
var
  I: Integer;
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      try
        FTimer.Enabled := True;
        StartPlay;
      except
        FActive := False;
        FTimer.Enabled := False;
        raise;
      end;
    end
    else
    begin
      {$IFDEF RX_D3}
      FMemoryImage.Canvas.Lock;
      { ensure that canvas is locked before timer is disabled }
      {$ENDIF}
      FTimer.Enabled := False;
      FScrollCnt := 0;
      FMemoryImage.Free;
      FMemoryImage := nil;
      StopPlay;
      if (csDesigning in ComponentState) and not (csDestroying in ComponentState) then
        ValidParentForm(Self).Designer.Modified;
    end;
    if not (csDestroying in ComponentState) then
      for I := 0 to Pred(ControlCount) do
      begin
        if FActive then
        begin
          if Controls[I].Visible then FHiddenList.Add(Controls[I]);
          if not (csDesigning in ComponentState) then
            Controls[I].Visible := False
        end
        else if FHiddenList.IndexOf(Controls[I]) >= 0 then
        begin
          Controls[I].Visible := True;
          Controls[I].Invalidate;
          if (csDesigning in ComponentState) then
            Controls[I].Update;
        end;
      end;
    if not FActive then FHiddenList.Clear;
    Invalidate;
  end;
end;

procedure TSecretPanel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    if Active then Invalidate;
  end;
end;

procedure TSecretPanel.SetGlyph(Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TSecretPanel.SetDirection(Value: TScrollDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    if FActive then
    begin
      FScrollCnt := 0;
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TSecretPanel.SetTextStyle(Value: TPanelBevel);
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TSecretPanel.SetGlyphLayout(Value: TGlyphLayout);
begin
  if FGlyphLayout <> Value then
  begin
    FGlyphLayout := Value;
    if FActive then
    begin
      UpdateMemoryImage;
      Invalidate;
    end;
  end;
end;

procedure TSecretPanel.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

{ TGlyphList }

type
  TGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): Integer;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    {$IFNDEF VER80}
    {$IFNDEF RX_D3} { Delphi 2.0 bug fix }
    procedure ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
    {$ENDIF}
    {$ENDIF}
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

{ TGlyphCache }

  TGlyphCache = class
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  {$IFNDEF VER80}
  inherited CreateSize(AWidth, AHeight);
  {$ELSE}
  inherited Create(AWidth, AHeight);
  {$ENDIF}
  FUsed := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

{$IFNDEF VER80}
{$IFNDEF RX_D3} { Delphi 2.0 bug fix }
procedure TGlyphList.ReplaceMasked(Index: Integer; NewImage: TBitmap; MaskColor: TColor);
var
  TempIndex: Integer;
  Image, Mask: TBitmap;
begin
  if HandleAllocated then
  begin
    TempIndex := inherited AddMasked(NewImage, MaskColor);
    if TempIndex <> -1 then
    try
      Image := TBitmap.Create;
      Mask := TBitmap.Create;
      try
        with Image do
        begin
          Height := Self.Height;
          Width := Self.Width;
        end;
        with Mask do
        begin
          Monochrome := True; { fix }
          Height := Self.Height;
          Width := Self.Width;
        end;
        ImageList_Draw(Handle, TempIndex, Image.Canvas.Handle, 0, 0, ILD_NORMAL);
        ImageList_Draw(Handle, TempIndex, Mask.Canvas.Handle, 0, 0, ILD_MASK);
        if not ImageList_Replace(Handle, Index, Image.Handle, Mask.Handle) then
          raise EInvalidOperation.Create({LoadStr(}SReplaceImage {)});
      finally
        Image.Free;
        Mask.Free;
      end;
    finally
      inherited Delete(TempIndex);
    end
    else
      raise EInvalidOperation.Create({LoadStr(}SReplaceImage {)});
  end;
  Change;
end;
{$ENDIF}
{$ENDIF}

function TGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

const
  GlyphCache: TGlyphCache = nil;

{ TRxButtonGlyph }

constructor TRxButtonGlyph.Create;
var
  I: TRxButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clFuchsia;
  FAlignment := taCenter;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TRxButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TRxButtonGlyph.Invalidate;
var
  I: TRxButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if Assigned(FGlyphList) then
      if (FIndexs[I] <> -1) then TGlyphList(FGlyphList).Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(TGlyphList(FGlyphList));
  FGlyphList := nil;
end;

procedure TRxButtonGlyph.GlyphChanged(Sender: TObject);
var
  Glyphs: Integer;
begin
  if Sender = FOriginal then
  begin
    Invalidate;
    if (FOriginal <> nil) and (FOriginal.Height > 0) then
    begin
      FTransparentColor := FOriginal.TransparentColor and not PaletteMask;
      if FOriginal.Width mod FOriginal.Height = 0 then
      begin
        Glyphs := FOriginal.Width div FOriginal.Height;
        if Glyphs > (Ord(High(TRxButtonState)) + 1) then Glyphs := 1;
        SetNumGlyphs(Glyphs);
      end;
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TRxButtonGlyph.SetGlyph(Value: TBitmap);
begin
  Invalidate;
  FOriginal.Assign(Value);
end;

procedure TRxButtonGlyph.SetNumGlyphs(Value: TRxNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

function TRxButtonGlyph.MapColor(Color: TColor): TColor;
var
  Index: Byte;
begin
  if (Color = FTransparentColor) or (ColorToRGB(Color) = ColorToRGB(clBtnFace)) then
    Result := Color
  else
  begin
    Color := ColorToRGB(Color);
    Index := Byte(LongInt(Word(GetRValue(Color)) * 77 +
      Word(GetGValue(Color)) * 150 + Word(GetBValue(Color)) * 29) shr 8);
    Result := RGB(Index, Index, Index);
  end;
end;

{$IFNDEF VER80}
function TRxButtonGlyph.CreateImageGlyph(State: TRxButtonState;
  Images: TImageList; Index: Integer): Integer;
var
  TmpImage, Mask: TBitmap;
  IWidth, IHeight, X, Y: Integer;
begin
  if (State = rbsDown) then State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (Images.Width = 0) or (Images.Height = 0) or
    (Images.Count = 0) then Exit;
  IWidth := Images.Width;
  IHeight := Images.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          with TmpImage.Canvas do
          begin
            FillRect(Rect(0, 0, IWidth, IHeight));
            ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_NORMAL);
          end;
          Mask := TBitmap.Create;
          try
            with Mask do
            begin
              Monochrome := True;
              Height := IHeight;
              Width := IWidth;
            end;
            with Mask.Canvas do
            begin
              FillRect(Rect(0, 0, IWidth, IHeight));
              ImageList_Draw(Images.Handle, Index, Handle, 0, 0, ILD_MASK);
            end;
            FIndexs[State] := TGlyphList(FGlyphList).Add(TmpImage, Mask);
          finally
            Mask.Free;
          end;
        end;
      rbsDisabled:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageListDrawDisabled(Images, TmpImage.Canvas, 0, 0, Index,
            clBtnHighlight, clBtnShadow, True);
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
      rbsInactive:
        begin
          TmpImage.Canvas.Brush.Color := clBtnFace;
          TmpImage.Canvas.FillRect(Rect(0, 0, IWidth, IHeight));
          ImageList_Draw(Images.Handle, Index, TmpImage.Canvas.Handle, 0, 0,
            ILD_NORMAL);
          with TmpImage do
          begin
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(Canvas.Pixels[X, Y]);
          end;
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage,
            ColorToRGB(clBtnFace));
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
end;
{$ENDIF}

function TRxButtonGlyph.CreateButtonGlyph(State: TRxButtonState): Integer;
var
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight, X, Y: Integer;
  IRect, ORect: TRect;
  I: TRxButtonState;
begin
  if (State = rbsDown) and (NumGlyphs < 3) then State := rbsUp;
  Result := FIndexs[State];
  if (Result <> -1) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    I := State;
    if Ord(I) >= NumGlyphs then I := rbsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      rbsUp, rbsDown, rbsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor);
        end;
      rbsDisabled:
        if NumGlyphs > 1 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor);
        end
        else
        begin
          MonoBmp := CreateDisabledBitmap(FOriginal, clBlack);
          try
            FIndexs[State] := TGlyphList(FGlyphList).AddMasked(MonoBmp,
              ColorToRGB(clBtnFace));
          finally
            MonoBmp.Free;
          end;
        end;
      rbsInactive:
        if NumGlyphs > 4 then
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor);
        end
        else
        begin
          with TmpImage do
          begin
            for X := 0 to Width - 1 do
              for Y := 0 to Height - 1 do
                Canvas.Pixels[X, Y] := MapColor(FOriginal.Canvas.Pixels[X, Y]);
          end;
          FIndexs[State] := TGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TRxButtonGlyph.DrawPopupMark(Canvas: TCanvas; X, Y: Integer;
  State: TRxButtonState);
var
  AColor: TColor;

  procedure DrawMark;
  var
    I: Integer;
  begin
    with Canvas do
    begin
      for I := 0 to 6 do
      begin
        Pixels[X + I, Y - 1] := AColor;
        if (I > 0) and (I < 6) then
        begin
          Pixels[X + I, Y] := AColor;
          if (I > 1) and (I < 5) then Pixels[X + I, Y + 1] := AColor;
        end;
      end;
      Pixels[X + 3, Y + 2] := AColor;
    end;
  end;

begin
  if State = rbsDisabled then
  begin
    AColor := clBtnHighlight;
    Inc(X, 1); Inc(Y, 1);
    DrawMark;
    Dec(X, 1); Dec(Y, 1);
    AColor := clBtnShadow;
  end
  else
    AColor := clBtnText;
  DrawMark;
end;

function TRxButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TRxButtonState): TPoint;
var
  Index: Integer;
begin
  Result := Point(0, 0);
  if (FOriginal = nil) or (FOriginal.Width = 0) or (FOriginal.Height = 0) or
    FOriginal.Empty then Exit;
  Index := CreateButtonGlyph(State);
  if Index >= 0 then
  begin
    {$IFNDEF VER80}
    ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
    {$ELSE}
    FGlyphList.Draw(Canvas, X, Y, Index);
    {$ENDIF}
    Result := Point(FGlyphList.Width, FGlyphList.Height);
  end;
end;

{$IFNDEF VER80}

function TRxButtonGlyph.DrawButtonImage(Canvas: TCanvas; X, Y: Integer;
  Images: TImageList; ImageIndex: Integer; State: TRxButtonState): TPoint;
var
  Index: Integer;
begin
  Result := Point(0, 0);
  if (Images = nil) or (ImageIndex < 0) or (ImageIndex >= Images.Count) then
    Exit;
  if State = rbsDisabled then
  begin
    ImageListDrawDisabled(Images, Canvas, X, Y, ImageIndex, clBtnHighlight,
      clBtnShadow, True);
  end
  else if State = rbsInactive then
  begin
    Index := CreateImageGlyph(State, Images, ImageIndex);
    if Index >= 0 then
      ImageList_Draw(FGlyphList.Handle, Index, Canvas.Handle, X, Y, ILD_NORMAL);
  end
  else
    ImageList_Draw(Images.Handle, ImageIndex, Canvas.Handle, X, Y, ILD_NORMAL);
  Result := Point(Images.Width, Images.Height);
end;
{$ENDIF}

procedure TRxButtonGlyph.MinimizeCaption(Canvas: TCanvas; const Caption: string;
  Buffer: PChar; MaxLen, Width: Integer);
var
  I: Integer;
  {$IFDEF VER80}
  P: PChar;
  {$ENDIF}
  Lines: TStrings;
begin
  StrPLCopy(Buffer, Caption, MaxLen);
  if FWordWrap then Exit;
  Lines := TStringList.Create;
  try
    {$IFNDEF VER80}
    Lines.Text := Caption;
    for I := 0 to Lines.Count - 1 do
      Lines[I] := MinimizeText(Lines[I], Canvas, Width);
    StrPLCopy(Buffer, TrimRight(Lines.Text), MaxLen);
    {$ELSE}
    Lines.SetText(Buffer);
    for I := 0 to Lines.Count - 1 do
      Lines[I] := MinimizeText(Lines[I], Canvas, Width);
    P := Lines.GetText;
    try
      StrPLCopy(Buffer, TrimRight(StrPas(P)), MaxLen);
    finally
      StrDispose(P);
    end;
    {$ENDIF}
  finally
    Lines.Free;
  end;
end;

procedure TRxButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TRxButtonState; Flags: Word);
begin
  Canvas.Brush.Style := bsClear;
  Flags := DT_VCENTER or WordWraps[FWordWrap] or Flags;
  if State = rbsDisabled then
  begin
    with Canvas do
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
    end;
  end
  else
    DrawText(Canvas.Handle, PChar(Caption), -1, TextBounds, Flags);
end;

procedure TRxButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  var Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; var GlyphPos: TPoint; var TextBounds: TRect; Flags: Word
  {$IFNDEF VER80}; Images: TImageList; ImageIndex: Integer{$ENDIF});
var
  TextPos: TPoint;
  MaxSize, ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  CString: array[0..255] of Char;
begin
  { calculate the item sizes }
  FillChar(CString, SizeOf(CString), 0);
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);
  {$IFNDEF VER80}
  if Assigned(Images) and (Images.Width > 0) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
    GlyphSize := Point(Images.Width, Images.Height)
  else
    {$ENDIF}if FOriginal <> nil then
      GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
    else
      GlyphSize := Point(0, 0);
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    MaxSize.X := ClientSize.X - GlyphSize.X;
    if Margin <> -1 then Dec(MaxSize.X, Margin);
    if Spacing <> -1 then Dec(MaxSize.X, Spacing);
    if PopupMark then Dec(MaxSize.X, 9);
    MaxSize.Y := ClientSize.Y;
  end
  else { blGlyphTop, blGlyphBottom }
  begin
    MaxSize.X := ClientSize.X;
    MaxSize.Y := ClientSize.Y - GlyphSize.Y;
    if Margin <> -1 then Dec(MaxSize.Y, Margin);
    if Spacing <> -1 then Dec(MaxSize.Y, Spacing);
  end;
  MaxSize.X := Max(0, MaxSize.X);
  MaxSize.Y := Max(0, MaxSize.Y);
  MinimizeCaption(Canvas, Caption, CString, Length(CString) - 1, MaxSize.X);
  Caption := StrPas(CString);
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, MaxSize.X, 0);
    DrawText(Canvas.Handle, CString, -1, TextBounds, DT_CALCRECT or DT_CENTER
      or DT_VCENTER or WordWraps[FWordWrap] or Flags);
  end
  else
    TextBounds := Rect(0, 0, 0, 0);
  TextBounds.Bottom := Max(TextBounds.Top, TextBounds.Top +
    Min(MaxSize.Y, HeightOf(TextBounds)));
  TextBounds.Right := Max(TextBounds.Left, TextBounds.Left +
    Min(MaxSize.X, WidthOf(TextBounds)));
  TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
    TextBounds.Top);
  if PopupMark then
    if ((GlyphSize.X = 0) or (GlyphSize.Y = 0)) or (Layout = blGlyphLeft) then
      Inc(TextSize.X, 9)
    else if (GlyphSize.X > 0) then
      Inc(GlyphSize.X, 6);
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y div 2) - (GlyphSize.Y div 2);
    TextPos.Y := (ClientSize.Y div 2) - (TextSize.Y div 2);
  end
  else
  begin
    GlyphPos.X := (ClientSize.X div 2) - (GlyphSize.X div 2);
    TextPos.X := (ClientSize.X div 2) - (TextSize.X div 2);
  end;
  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then Spacing := 0;
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X div 2) - (TotalSize.X div 2)
      else
        Margin := (ClientSize.Y div 2) - (TotalSize.Y div 2);
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X div 2) - (TextSize.X div 2)
      else
        Spacing := (TotalSize.Y div 2) - (TextSize.Y div 2);
    end;
  end;
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;
  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

{$IFNDEF VER80}

function TRxButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; State: TRxButtonState; Flags: Word): TRect;
begin
  Result := DrawEx(Canvas, Client, Caption, Layout, Margin, Spacing,
    PopupMark, nil, -1, State, Flags);
end;
{$ENDIF}

{$IFNDEF VER80}

function TRxButtonGlyph.DrawEx(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; Images: TImageList; ImageIndex: Integer;
  State: TRxButtonState; Flags: Word): TRect;
{$ELSE}

function TRxButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  PopupMark: Boolean; State: TRxButtonState; Flags: Word): TRect;
{$ENDIF}
var
  {$IFNDEF VER80}
  UseImages: Boolean;
  {$ENDIF}
  GlyphPos, PopupPos: TPoint;
  TextBounds: TRect;
  CaptionText: string;
begin
  CaptionText := Caption;
  CalcButtonLayout(Canvas, Client, CaptionText, Layout, Margin, Spacing,
    PopupMark, GlyphPos, TextBounds, Flags{$IFNDEF VER80}, Images,
    ImageIndex{$ENDIF});
  {$IFNDEF VER80}
  UseImages := False;
  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) and (Images.Width > 0) then
  begin
    UseImages := True;
    PopupPos := DrawButtonImage(Canvas, GlyphPos.X, GlyphPos.Y, Images,
      ImageIndex, State);
  end
  else
    {$ENDIF}
    PopupPos := DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State);
  DrawButtonText(Canvas, CaptionText, TextBounds, State, Flags);
  if PopupMark then
    if (Layout <> blGlyphLeft) and (((FOriginal <> nil) and (FOriginal.Width > 0)){$IFNDEF VER80} or UseImages{$ENDIF}) then
    begin
      PopupPos.X := GlyphPos.X + PopupPos.X + 1;
      PopupPos.Y := GlyphPos.Y + PopupPos.Y div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end
    else
    begin
      if CaptionText <> '' then
        PopupPos.X := TextBounds.Right + 3
      else
        PopupPos.X := (Client.Left + Client.Right - 7) div 2;
      PopupPos.Y := TextBounds.Top + HeightOf(TextBounds) div 2;
      DrawPopupMark(Canvas, PopupPos.X, PopupPos.Y, State);
    end;
  Result := TextBounds;
end;

const
  {$IFNDEF RX_D4}
  Pattern: TBitmap = nil;
  {$ENDIF}
  ButtonCount: Integer = 0;

{ DrawButtonFrame - returns the remaining usable area inside the Client rect }

function DrawButtonFrame(Canvas: TCanvas; const Client: TRect;
  IsDown, IsFlat: Boolean; Style: TButtonStyle): TRect;
var
  NewStyle: Boolean;
begin
  Result := Client;
  NewStyle := (Style = bsNew) or (NewStyleControls and (Style = bsAutoDetect));
  if IsDown then
  begin
    if NewStyle then
    begin
//Polaris      Frame3D(Canvas, Result,clBtnShadow{ clWindowFrame}, clBtnHighlight, 1);
//      if not IsFlat then
//        Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      if not IsFlat then
      begin
        Frame3D(Canvas, Result, clWindowFrame, clBtnHighlight, 1);
        Frame3D(Canvas, Result, clBtnShadow, clBtnFace, 1);
      end
      else
        Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1);
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnShadow, clBtnHighlight, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.PolyLine([Point(Result.Left, Result.Bottom - 1),
          Point(Result.Left, Result.Top), Point(Result.Right, Result.Top)]);
      end;
    end;
  end
  else
  begin
    if NewStyle then
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1)
      else
      begin
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnFace, clBtnShadow, 1);
      end;
    end
    else
    begin
      if IsFlat then
        Frame3D(Canvas, Result, clBtnHighlight, clWindowFrame, 1)
      else
      begin
        Frame3D(Canvas, Result, clWindowFrame, clWindowFrame, 1);
        Frame3D(Canvas, Result, clBtnHighlight, clBtnShadow, 1);
      end;
    end;
  end;
  InflateRect(Result, -1, -1);
end;

{ TButtonImage }

constructor TButtonImage.Create;
begin
  FGlyph := TRxButtonGlyph.Create;
  NumGlyphs := 1;
  FButtonSize := Point(24, 23);
end;

destructor TButtonImage.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TButtonImage.Invalidate;
begin
  TRxButtonGlyph(FGlyph).Invalidate;
end;

function TButtonImage.GetNumGlyphs: TRxNumGlyphs;
begin
  Result := TRxButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TButtonImage.SetNumGlyphs(Value: TRxNumGlyphs);
begin
  TRxButtonGlyph(FGlyph).NumGlyphs := Value;
end;

function TButtonImage.GetWordWrap: Boolean;
begin
  Result := TRxButtonGlyph(FGlyph).WordWrap;
end;

procedure TButtonImage.SetWordWrap(Value: Boolean);
begin
  TRxButtonGlyph(FGlyph).WordWrap := Value;
end;

function TButtonImage.GetGlyph: TBitmap;
begin
  Result := TRxButtonGlyph(FGlyph).Glyph;
end;

procedure TButtonImage.SetGlyph(Value: TBitmap);
begin
  TRxButtonGlyph(FGlyph).Glyph := Value;
end;

function TButtonImage.GetAlignment: TAlignment;
begin
  Result := TRxButtonGlyph(FGlyph).Alignment;
end;

procedure TButtonImage.SetAlignment(Value: TAlignment);
begin
  TRxButtonGlyph(FGlyph).Alignment := Value;
end;

{$IFNDEF VER80}

procedure TButtonImage.Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Flags: Word);
begin
  DrawEx(Canvas, X, Y, Margin, Spacing, Layout, AFont, nil, -1, Flags);
end;
{$ENDIF}

{$IFNDEF VER80}

procedure TButtonImage.DrawEx(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Images: TImageList; ImageIndex: Integer;
  Flags: Word);
{$ELSE}

procedure TButtonImage.Draw(Canvas: TCanvas; X, Y, Margin, Spacing: Integer;
  Layout: TButtonLayout; AFont: TFont; Flags: Word);
{$ENDIF}
var
  Target: TRect;
  SaveColor: Integer;
  SaveFont: TFont;
begin
  SaveColor := Canvas.Brush.Color;
  SaveFont := TFont.Create;
  SaveFont.Assign(Canvas.Font);
  try
    Target := Bounds(X, Y, FButtonSize.X, FButtonSize.Y);
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Target);
    Frame3D(Canvas, Target, clBtnShadow, clWindowFrame, 1);
    Frame3D(Canvas, Target, clBtnHighlight, clBtnShadow, 1);
    if AFont <> nil then Canvas.Font := AFont;
    {$IFNDEF VER80}
    TRxButtonGlyph(FGlyph).DrawEx(Canvas, Target, Caption, Layout, Margin,
      Spacing, False, Images, ImageIndex, rbsUp, Flags);
    {$ELSE}
    TRxButtonGlyph(FGlyph).Draw(Canvas, Target, Caption, Layout, Margin,
      Spacing, False, rbsUp, Flags);
    {$ENDIF}
  finally
    Canvas.Font.Assign(SaveFont);
    SaveFont.Free;
    Canvas.Brush.Color := SaveColor;
  end;
end;

{ TRxSpeedButton }

constructor TRxSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFlatStandard := False;
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  {$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable];
  {$ENDIF}
  FInactiveGrayed := True;
  FDrawImage := TBitmap.Create;
  FGlyph := TRxButtonGlyph.Create;
  TRxButtonGlyph(FGlyph).OnChange := GlyphChanged;
  ParentFont := True;
  ParentShowHint := False;
  ShowHint := True;
  FSpacing := 1;
  FMargin := -1;
  FInitRepeatPause := 500;
  FRepeatPause := 100;
  FStyle := bsAutoDetect;
  FLayout := blGlyphTop;
  FMarkDropDown := True;
  Inc(ButtonCount);
  FThemedStyle := True;
end;

destructor TRxSpeedButton.Destroy;
begin
  TRxButtonGlyph(FGlyph).Free;
  Dec(ButtonCount);
  {$IFNDEF RX_D4}
  if ButtonCount = 0 then
  begin
    Pattern.Free;
    Pattern := nil;
  end;
  {$ENDIF}
  FDrawImage.Free;
  FDrawImage := nil;
  if FRepeatTimer <> nil then FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TRxSpeedButton.Loaded;
var
  State: TRxButtonState;
begin
  inherited Loaded;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TRxButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TRxSpeedButton.PaintGlyph(Canvas: TCanvas; ARect: TRect;
  AState: TRxButtonState; DrawMark: Boolean);
begin
  if FFlatStandard and (AState = rbsInactive) then AState := rbsExclusive;
  TRxButtonGlyph(FGlyph).Draw(Canvas, ARect, Caption, FLayout,
    FMargin, FSpacing, DrawMark, AState,
    {$IFDEF RX_D4}DrawTextBiDiModeFlags(Alignments[Alignment]){$ELSE}
    Alignments[Alignment]{$ENDIF});
end;

procedure TRxSpeedButton.Paint;
var
  PaintRect: TRect;
  AState: TRxButtonState;
  {$IFDEF RX_D7}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if not Enabled {and not (csDesigning in ComponentState)} then
  begin
    FState := rbsDisabled;
    FDragging := False;
  end
  else if FState = rbsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := rbsExclusive
    else
      FState := rbsUp;
  AState := FState;
  if FFlat and not FMouseInControl and not (csDesigning in ComponentState) then
    AState := rbsInactive;
  {$IFDEF RX_D7}
  with {$IFDEF RX_D16}StyleServices{$ELSE}ThemeServices{$ENDIF} do
    if FThemedStyle and {$IFDEF RX_D16}Enabled{$ELSE}ThemesEnabled{$ENDIF} then
    begin
      if FTransparent then
        CopyParentImage(Self, FDrawImage.Canvas)
      else
        PerformEraseBackground(Self, Canvas.Handle);

      if not Enabled then
        Button := tbPushButtonDisabled
      else if AState in [rbsDown, rbsExclusive] then
        Button := tbPushButtonPressed
      else if FMouseInControl then
        Button := tbPushButtonHot
      else
        Button := tbPushButtonNormal;

      ToolButton := ttbToolbarDontCare;
      if FFlat then
      begin
        case Button of
          tbPushButtonDisabled: Toolbutton := ttbButtonDisabled;
          tbPushButtonPressed: Toolbutton := ttbButtonPressed;
          tbPushButtonHot: Toolbutton := ttbButtonHot;
          tbPushButtonNormal: Toolbutton := ttbButtonNormal;
        end;
      end;

      PaintRect := ClientRect;
      if ToolButton = ttbToolbarDontCare then
      begin
        InflateRect(PaintRect, 1, 1);
        Details := GetElementDetails(Button);
        DrawElement(Canvas.Handle, Details, PaintRect);
        {$IFDEF RX_D16}
        GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
        {$ELSE}
        PaintRect := ContentRect(Canvas.Handle, Details, PaintRect);
        {$ENDIF}
      end
      else
      begin
        Details := GetElementDetails(ToolButton);
        DrawElement(Canvas.Handle, Details, PaintRect);
        {$IFDEF RX_D16}
        GetElementContentRect(Canvas.Handle, Details, PaintRect, PaintRect);
        {$ELSE}
        PaintRect := ContentRect(Canvas.Handle, Details, PaintRect);
        {$ENDIF}
      end;

      if Button = tbPushButtonPressed then
      begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
        if ToolButton <> ttbToolbarDontCare then
          Canvas.Font.Color := clHighlightText;
        OffsetRect(PaintRect, 1, 0);
      end;
      PaintGlyph({FDrawImage.}Canvas, PaintRect, AState, FMarkDropDown and
        Assigned(FDropDownMenu));
    end
    else
      {$ENDIF}
    begin
      PaintRect := Rect(0, 0, Width, Height);
      FDrawImage.Width := Self.Width;
      FDrawImage.Height := Self.Height;
      with FDrawImage.Canvas do
      begin
        Font := Self.Font;
        Brush.Color := clBtnFace;
        Brush.Style := bsSolid;
        FillRect(PaintRect);
        if FTransparent then CopyParentImage(Self, FDrawImage.Canvas);
        if (AState <> rbsInactive) or (FState = rbsExclusive) then
          PaintRect := DrawButtonFrame(FDrawImage.Canvas, PaintRect,
            FState in [rbsDown, rbsExclusive], FFlat, FStyle)
        else if FFlat then
          InflateRect(PaintRect, -2, -2);
      end;
      if (FState = rbsExclusive) and not Transparent and
        (not FFlat or (AState = rbsInactive)) then
      begin
        {$IFDEF RX_D4}
        FDrawImage.Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        {$ELSE}
        if Pattern = nil then
          Pattern := CreateTwoColorsBrushPattern(clBtnFace, clBtnHighlight);
        FDrawImage.Canvas.Brush.Bitmap := Pattern;
        {$ENDIF}
        InflateRect(PaintRect, 1, 1);
        FDrawImage.Canvas.FillRect(PaintRect);
        InflateRect(PaintRect, -1, -1);
      end;
      if FState in [rbsDown, rbsExclusive] then OffsetRect(PaintRect, 1, 1);
      if (FState = rbsDisabled) or not FInactiveGrayed then AState := FState;
      PaintGlyph(FDrawImage.Canvas, PaintRect, AState, FMarkDropDown and
        Assigned(FDropDownMenu));
      Canvas.Draw(0, 0, FDrawImage);
    end;
end;

procedure TRxSpeedButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = DropDownMenu) and (Operation = opRemove) then
    DropDownMenu := nil;
end;

function TRxSpeedButton.GetDropDownMenuPos: TPoint;
begin
  if Assigned(FDropDownMenu) then
  begin
    if MenuPosition = dmpBottom then
    begin
      case FDropDownMenu.Alignment of
        paLeft: Result := Point(-1, Height);
        paRight: Result := Point(Width + 1, Height);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end
    else { dmpRight }
    begin
      case FDropDownMenu.Alignment of
        paLeft: Result := Point(Width, -1);
        paRight: Result := Point(-1, -1);
      else {paCenter}
        Result := Point(Width div 2, Height);
      end;
    end;
  end
  else
    Result := Point(0, 0);
end;

function TRxSpeedButton.CheckBtnMenuDropDown: Boolean;
begin
  Result := CheckMenuDropDown(
    {$IFNDEF VER80}PointToSmallPoint(GetDropDownMenuPos){$ELSE}
    GetDropDownMenuPos{$ENDIF}, True);
end;

function TRxSpeedButton.CheckMenuDropDown(const Pos: TSmallPoint;
  Manual: Boolean): Boolean;
var
  Form: TCustomForm;
begin
  Result := False;
  if csDesigning in ComponentState then Exit;
  if Assigned(FDropDownMenu) and (DropDownMenu.AutoPopup or Manual) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then Form.SendCancelMode(nil);
    DropDownMenu.PopupComponent := Self;
    with ClientToScreen(SmallPointToPoint(Pos)) do
      DropDownMenu.Popup(X, Y);
    Result := True;
  end;
end;

procedure TRxSpeedButton.MouseEnter;
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TRxSpeedButton.MouseLeave;
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TRxSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Msg: TMsg;
begin
  if FMenuTracking then Exit;
  inherited MouseDown(Button, Shift, X, Y);
  if (not FMouseInControl) and Enabled then
  begin
    FMouseInControl := True;
    Repaint;
  end;
  if (Button = mbLeft) and Enabled {and not (ssDouble in Shift)} then
  begin
    if not FDown then
    begin
      FState := rbsDown;
      Repaint;
    end;
    FDragging := True;
    FMenuTracking := True;
    try
      P := GetDropDownMenuPos;
      if CheckMenuDropDown(PointToSmallPoint(P), False) then
        DoMouseUp(Button, Shift, X, Y);
      if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
      begin
        if (Msg.Message = WM_LBUTTONDOWN) or (Msg.Message = WM_LBUTTONDBLCLK) then
        begin
          P := ScreenToClient(Msg.Pt);
          if (P.X >= 0) and (P.X < ClientWidth) and (P.Y >= 0) and (P.Y <= ClientHeight) then
            KillMessage(0, Msg.Message); {PeekMessage(Msg, 0, 0, 0, PM_REMOVE);}
        end;
      end;
    finally
      FMenuTracking := False;
    end;
    if FAllowTimer then
    begin
      if FRepeatTimer = nil then FRepeatTimer := TTimer.Create(Self);
      FRepeatTimer.Interval := InitPause;
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Enabled := True;
    end;
  end;
end;

procedure TRxSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TRxButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then
      NewState := rbsUp
    else
      NewState := rbsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := rbsExclusive
      else
        NewState := rbsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end;

procedure TRxSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  DoMouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then FRepeatTimer.Enabled := False;
end;

procedure TRxSpeedButton.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  if FDragging and (Button = mbLeft) then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      FState := rbsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [rbsExclusive, rbsDown]) then
        Repaint
      else
        Invalidate;
    end
    else if DoClick then
    begin
      SetDown(not FDown);
      if FDown then Repaint;
    end
    else
    begin
      if FDown then FState := rbsExclusive;
      Repaint;
    end;
    if DoClick and not FMenuTracking then Click;
  end;
  UpdateTracking;
end;

procedure TRxSpeedButton.ButtonClick;
var
  FirstTickCount, Now: LongInt;
begin
  if FMenuTracking or (not Enabled) or (Assigned(FDropDownMenu) and DropDownMenu.AutoPopup) then
    Exit;
  if not FDown then
  begin
    FState := rbsDown;
    Repaint;
  end;
  try
    FirstTickCount := GetTickCount;
    repeat
      Now := GetTickCount;
    until (Now - FirstTickCount >= 20) or (Now < FirstTickCount);
    if FGroupIndex = 0 then Click;
  finally
    FState := rbsUp;
    if FGroupIndex = 0 then
      Repaint
    else
    begin
      SetDown(not FDown);
      Click;
    end;
  end;
end;

procedure TRxSpeedButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.ModalResult := ModalResult;
  inherited Click;
end;

function TRxSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TRxSpeedButton.GetWordWrap: Boolean;
begin
  Result := TRxButtonGlyph(FGlyph).WordWrap;
end;

procedure TRxSpeedButton.SetWordWrap(Value: Boolean);
begin
  if Value <> WordWrap then
  begin
    TRxButtonGlyph(FGlyph).WordWrap := Value;
    Invalidate;
  end;
end;

function TRxSpeedButton.GetAlignment: TAlignment;
begin
  Result := TRxButtonGlyph(FGlyph).Alignment;
end;

procedure TRxSpeedButton.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    TRxButtonGlyph(FGlyph).Alignment := Value;
    Invalidate;
  end;
end;

function TRxSpeedButton.GetGlyph: TBitmap;
begin
  Result := TRxButtonGlyph(FGlyph).Glyph;
end;

function TRxSpeedButton.GetGroupIndex: Integer;
begin
  Result := FGroupIndex;
end;

procedure TRxSpeedButton.SetGlyph(Value: TBitmap);
begin
  TRxButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TRxSpeedButton.GetNumGlyphs: TRxNumGlyphs;
begin
  Result := TRxButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TRxSpeedButton.SetNumGlyphs(Value: TRxNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > Ord(High(TRxButtonState)) + 1 then
    Value := Ord(High(TRxButtonState)) + 1;
  if Value <> TRxButtonGlyph(FGlyph).NumGlyphs then
  begin
    TRxButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TRxSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_RXBUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := {$IFDEF WIN64}LPARAM{$ELSE}LongInt{$ENDIF}(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TRxSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = rbsUp then Invalidate;
      FState := rbsExclusive;
    end
    else
    begin
      FState := rbsUp;
    end;
    Repaint;
    if Value then UpdateExclusive;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetGroupIndex(Value: Integer);
begin
  if GetGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TRxSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TRxSpeedButton.SetAllowTimer(Value: Boolean);
begin
  FAllowTimer := Value;
  if not FAllowTimer and (FRepeatTimer <> nil) then
  begin
    FRepeatTimer.Enabled := False;
    FRepeatTimer.Free;
    FRepeatTimer := nil;
  end;
end;

procedure TRxSpeedButton.SetDropDownMenu(Value: TPopupMenu);
begin
  FDropDownMenu := Value;
  {$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
  {$ENDIF}
  if FMarkDropDown then Invalidate;
end;

procedure TRxSpeedButton.SetInactiveGrayed(Value: Boolean);
begin
  if Value <> FInactiveGrayed then
  begin
    FInactiveGrayed := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetFlatStandard(Value: Boolean); //>Polaris
begin
  if FFlatStandard <> Value then
  begin
    FFlatStandard := Value;
    Invalidate;
  end;
end; //<Polaris

procedure TRxSpeedButton.SetStyle(Value: TButtonStyle);
begin
  if Style <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetMarkDropDown(Value: Boolean);
begin
  if Value <> FMarkDropDown then
  begin
    FMarkDropDown := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TRxSpeedButton.WMRButtonDown(var Message: TWMRButtonDown);
begin
  inherited;
  UpdateTracking;
end;

procedure TRxSpeedButton.WMRButtonUp(var Message: TWMRButtonUp);
begin
  inherited;
  UpdateTracking;
end;

procedure TRxSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  if not FMenuTracking then
  begin
    inherited;
    if FDown then DblClick;
  end;
end;

procedure TRxSpeedButton.CMEnabledChanged(var Message: TMessage);
var
  State: TRxButtonState;
begin
  inherited;
  if Enabled then
  begin
    if Flat then
      State := rbsInactive
    else
      State := rbsUp;
  end
  else
    State := rbsDisabled;
  TRxButtonGlyph(FGlyph).CreateButtonGlyph(State);
  UpdateTracking;
  Repaint;
end;

procedure TRxSpeedButton.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Visible then UpdateTracking;
end;

procedure TRxSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (not FMouseInControl) and Enabled and IsForegroundTask then
  begin
    FMouseInControl := True;
    if FFlat then Repaint;
    MouseEnter;
  end;
end;

procedure TRxSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    if FFlat then Repaint;
    MouseLeave;
  end;
end;

procedure TRxSpeedButton.WMMouseMove(var Message: TMessage);
begin
  inherited;
end;

procedure TRxSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TControl;
begin
  if (Message.WParam = FGroupIndex) and Parent.HandleAllocated then
  begin
    Sender := TControl(Message.LParam);
    if (Sender <> nil) and (Sender is TRxSpeedButton) then
      if Sender <> Self then
      begin
        if TRxSpeedButton(Sender).Down and FDown then
        begin
          FDown := False;
          FState := rbsUp;
          Repaint;
        end;
        FAllowAllUp := TRxSpeedButton(Sender).AllowAllUp;
      end;
  end;
end;

procedure TRxSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TRxSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TRxSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TRxSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  TRxButtonGlyph(FGlyph).Invalidate;
  Invalidate;
end;

procedure TRxSpeedButton.UpdateTracking;
var
  P: TPoint;
  OldValue: Boolean;
begin
  OldValue := FMouseInControl;
  GetCursorPos(P);
  FMouseInControl := Enabled and (FindDragTarget(P, True) = Self) and
    IsForegroundTask;
  if (FMouseInControl <> OldValue) then
    if FMouseInControl then
    begin
      if Flat then Repaint;
      MouseEnter;
    end
    else
    begin
      if Flat then Invalidate;
      MouseLeave;
    end;
end;

procedure TRxSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatInterval;
  if (FState = rbsDown) and MouseCapture then
  try
    Click;
  except
    FRepeatTimer.Enabled := False;
    raise;
  end;
end;

{$IFDEF RX_D4}
procedure TRxSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
      TransparentColor := clFuchsia;
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (not CheckDefaults or (Self.Down = False)) and (FGroupIndex <> 0) then
        Self.Down := Checked;
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;
{$ENDIF RX_D4}

{  TRxColorButton  }

constructor TRxColorButton.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 ShowBackColor := True;
 FCanvas := TCanvas.Create;
 BackColor := clBtnFace;
 ForeColor := clBtnText;
 HoverColor := clBtnFace;
end;

destructor TRxColorButton.Destroy;
begin
 FreeAndNil(FCanvas);
 inherited Destroy;
end;

procedure TRxColorButton.WndProc(var Message : TMessage);
begin
 if (Message.Msg = CM_MOUSELEAVE) then
  begin
   ShowBackColor := True;
   Invalidate;
  end;
 if (Message.Msg = CM_MOUSEENTER) then
  begin
   ShowBackColor := False;
   Invalidate;
  end;
 inherited;
end;

procedure TRxColorButton.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
  with Params do
    Style := Style or BS_OWNERDRAW;
end;

procedure TRxColorButton.SetButtonStyle(Value: Boolean);
begin
 if Value <> IsFocused then
  begin
   IsFocused := Value;
   Invalidate;
  end;
end;

procedure TRxColorButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
 with Message.MeasureItemStruct^ do
  begin
   itemWidth  := Width;
   itemHeight := Height;
  end;
end;

procedure TRxColorButton.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
 with Message.DrawItemStruct^ do
  begin
   SaveIndex := SaveDC(hDC);
   FCanvas.Lock;
   try
    FCanvas.Handle := hDC;
    FCanvas.Font   := Font;
    FCanvas.Brush  := Brush;
    DrawButton(rcItem, itemState);
   finally
    FCanvas.Handle := 0;
    FCanvas.Unlock;
    RestoreDC(hDC, SaveIndex);
   end;
 end;
 Message.Result := 1;
end;

procedure TRxColorButton.CMEnabledChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TRxColorButton.CMFontChanged(var Message: TMessage);
begin
 inherited;
 Invalidate;
end;

procedure TRxColorButton.SetBackColor(const Value: TColor);
begin
 if FBackColor <> Value then
  begin
   FBackColor:= Value;
   Invalidate;
  end;
end;

procedure TRxColorButton.SetForeColor(const Value: TColor);
begin
 if FForeColor <> Value then
  begin
   FForeColor:= Value;
   Invalidate;
  end;
end;

procedure TRxColorButton.SetHoverColor(const Value: TColor);
begin
 if FHoverColor <> Value then
  begin
   FHoverColor:= Value;
   Invalidate;
  end;
end;

procedure TRxColorButton.DrawButton(Rect: TRect; State: UINT);

var Flags, OldMode: Longint;
    IsDown, IsDefault, IsDisabled: Boolean;
    OldColor: TColor;
    OrgRect: TRect;
    NewCaption : string;

begin
 NewCaption := Caption;
 OrgRect := Rect;
 Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
 IsDown := State and ODS_SELECTED <> 0;
 IsDisabled := State and ODS_DISABLED <> 0;
 IsDefault := State and ODS_FOCUS <> 0;

 if IsDown then Flags := Flags or DFCS_PUSHED;
 if IsDisabled then Flags := Flags or DFCS_INACTIVE;

 if (IsFocused or IsDefault) then
  begin
   FCanvas.Pen.Color   := clWindowFrame;
   FCanvas.Pen.Width   := 1;
   FCanvas.Brush.Style := bsClear;
   FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
   InflateRect(Rect, - 1, - 1);
  end;

  if IsDown then
  begin
   FCanvas.Pen.Color   := clBtnShadow;
   FCanvas.Pen.Width   := 1;
   FCanvas.Brush.Color := clBtnFace;
   FCanvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
   InflateRect(Rect, - 1, - 1);
  end
 else
  begin
   DrawFrameControl(FCanvas.Handle, Rect, DFC_BUTTON, Flags);
  end;

  if IsDown then OffsetRect(Rect, 1, 1);

  OldColor := FCanvas.Brush.Color;
  if ShowBackColor then
   FCanvas.Brush.Color := BackColor
  else
   FCanvas.Brush.Color := HoverColor;
  FCanvas.FillRect(Rect);
  FCanvas.Brush.Color := OldColor;
  OldMode := SetBkMode(FCanvas.Handle, TRANSPARENT);
  FCanvas.Font.Color := ForeColor;
  if IsDisabled then
   DrawState(FCanvas.Handle, FCanvas.Brush.Handle, nil, {$IFDEF  WIN64}NativeInt{$ELSE}Integer{$ENDIF}(NewCaption), 0,
             ((Rect.Right - Rect.Left) - FCanvas.TextWidth(NewCaption)) div 2,
             ((Rect.Bottom - Rect.Top) - FCanvas.TextHeight(NewCaption)) div 2,
             0, 0, DST_TEXT or DSS_DISABLED)
  else
   begin
    InflateRect(Rect, -4, -4);
    DrawText(FCanvas.Handle, PChar(NewCaption), - 1, Rect, DT_WORDBREAK or DT_CENTER);
   end;

  SetBkMode(FCanvas.Handle, OldMode);

 if (IsFocused and IsDefault) then
  begin
   Rect := OrgRect;
   InflateRect(Rect, - 4, - 4);
   FCanvas.Pen.Color   := clWindowFrame;
   FCanvas.Brush.Color := clBtnFace;
   DrawFocusRect(FCanvas.Handle, Rect);
  end;
end;

{$IFNDEF VER80}
initialization
  FCheckBitmap := nil;
  RegisterClass(TRxColorButton); // needed for persistence at runtime

finalization
  DestroyLocals;
  {$ELSE}
initialization
  FCheckBitmap := nil;
  AddExitProc(DestroyLocals);
  {$ENDIF}
end.