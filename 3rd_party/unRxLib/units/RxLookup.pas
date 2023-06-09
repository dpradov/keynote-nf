{*******************************************************}
{                                                       }
{       Delphi VCL Extensions (RX)                      }
{                                                       }
{       Copyright (c) 1995,1997 Borland International   }
{       Portions copyright (c) 1995, 1996 AO ROSNO      }
{       Portions copyright (c) 1997, 1998 Master-Bank   }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and component added by JB.                   }
{*******************************************************}

unit RxLookup;

interface

{$I RX.INC}

uses SysUtils, {$IFNDEF VER80} Windows, DBCtrls, {$ELSE} WinTypes, WinProcs,
  {$ENDIF} Messages, Classes, Controls, Forms, Graphics, Menus, DB, Mask,
  {$IFDEF RX_D6}Types, {$ENDIF}
  {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  {$IFDEF RX_D17}System.Generics.Collections,{$ENDIF}
  {$IFNDEF RX_D3} DBTables, {$ENDIF} Buttons, StdCtrls, RxDBUtils, RxToolEdit;

const
  DefFieldsDelim = ',';

type

{ TRxLookupControl }

  TLookupListStyle = (lsFixed, lsDelimited);
  TRxLookupControl = class;
  TGetImageEvent = procedure (Sender: TObject; IsEmpty: Boolean;
    var Graphic: TGraphic; var TextMargin: Integer) of object;

  TDataSourceLink = class(TDataLink)
  private
    FDataControl: TRxLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    procedure RecordChanged(Field: TField); override;
  end;

  TLookupSourceLink = class(TDataLink)
  private
    FDataControl: TRxLookupControl;
  protected
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetChanged; override;
  end;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxLookupControl = class(TCustomControl)
  private
    FLookupSource: TDataSource;
    FDataLink: TDataSourceLink;
    FLookupLink: TLookupSourceLink;
    FDataFieldName: string;
    FLookupFieldName: string;
    FLookupDisplay: string;
    FDisplayIndex: Integer;
    FDataField: TField;
    FMasterField: TField;
    FKeyField: TField;
    FDisplayField: TField;
    FListFields: {$IFDEF RX_D17}TList<TField>{$ELSE}TList{$ENDIF};
    FValue: string;
    FDisplayValue: string;
    FDisplayEmpty: string;
    FSearchText: string;
    FEmptyValue: string;
    FEmptyStrIsNull: Boolean;   // Polaris
    FEmptyItemColor: TColor;
    FListActive: Boolean;
    FPopup: Boolean;
    FFocused: Boolean;
    FLocate: TLocateObject;
    FIndexSwitch: Boolean;
    FIgnoreCase: Boolean;
    FItemHeight: Integer;
    FFieldsDelim: Char;
    FListStyle: TLookupListStyle;
    FOnChange: TNotifyEvent;
    FOnGetImage: TGetImageEvent;
{$IFNDEF VER80}
    FLookupMode: Boolean;
    procedure CheckNotFixed;
    procedure SetLookupMode(Value: Boolean);
    function GetKeyValue: Variant;
    procedure SetKeyValue(const Value: Variant);
{$ENDIF}
    function CanModify: Boolean;
    procedure CheckNotCircular;
    procedure DataLinkActiveChanged;
    procedure CheckDataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    function GetBorderSize: Integer;
    function GetField: TField;
    function GetDataSource: TDataSource;
    function GetLookupField: string;
    function GetLookupSource: TDataSource;
    function GetReadOnly: Boolean;
    function GetTextHeight: Integer;
    function DefaultTextHeight: Integer;
    function GetItemHeight: Integer;
    function LocateKey: Boolean;
    function LocateDisplay: Boolean;
    function ValueIsEmpty(const S: string): Boolean;
    function StoreEmpty: Boolean;
    procedure ProcessSearchKey(Key: Char);
    procedure UpdateKeyValue;
    procedure SelectKeyValue(const Value: string);
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDisplayEmpty(const Value: string);
    procedure SetEmptyValue(const Value: string);
    procedure SetEmptyStrIsNull(const Value: Boolean);  // Polaris
    procedure SetEmptyItemColor(Value: TColor);
    procedure SetLookupField(const Value: string);
    procedure SetValueKey(const Value: string);
    procedure SetValue(const Value: string);
    procedure SetDisplayValue(const Value: string);
    procedure SetListStyle(Value: TLookupListStyle); virtual;
    procedure SetFieldsDelim(Value: Char); virtual;
    procedure SetLookupDisplay(const Value: string);
    procedure SetLookupSource(Value: TDataSource);
    procedure SetReadOnly(Value: Boolean);
    procedure SetItemHeight(Value: Integer);
    function ItemHeightStored: Boolean;
    procedure DrawPicture(Canvas: TCanvas; Rect: TRect; Image: TGraphic);
    procedure UpdateDisplayValue;
    function EmptyRowVisible: Boolean;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
  protected
    procedure Change; dynamic;
    procedure KeyValueChanged; virtual;
    procedure DisplayValueChanged; virtual;
    procedure ListLinkActiveChanged; virtual;
    procedure ListLinkDataChanged; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; virtual;
    procedure UpdateDisplayEmpty(const Value: string); virtual;
    function SearchText(var AValue: string): Boolean;
    function GetWindowWidth: Integer;
    property DataField: string read FDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisplayEmpty: string read FDisplayEmpty write SetDisplayEmpty;
    property EmptyValue: string read FEmptyValue write SetEmptyValue stored StoreEmpty;
    property EmptyStrIsNull: Boolean read FEmptyStrIsNull write SetEmptyStrIsNull default True; // Polaris
    property EmptyItemColor: TColor read FEmptyItemColor write SetEmptyItemColor default clWindow;
    property IgnoreCase: Boolean read FIgnoreCase write FIgnoreCase default True;
    property IndexSwitch: Boolean read FIndexSwitch write FIndexSwitch default True;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight
      stored ItemHeightStored;
    property ListStyle: TLookupListStyle read FListStyle write SetListStyle default lsFixed;
    property FieldsDelimiter: Char read FFieldsDelim write SetFieldsDelim default DefFieldsDelim;
    property LookupDisplay: string read FLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read FDisplayIndex write FDisplayIndex default 0;
    property LookupField: string read GetLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property ParentColor default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property TabStop default True;
    property Value: string read FValue write SetValue stored False;
    property DisplayValue: string read FDisplayValue write SetDisplayValue stored False;
{$IFNDEF VER80}
    property KeyValue: Variant read GetKeyValue write SetKeyValue stored False;
{$ENDIF}
    procedure SetFieldValue(Field: TField; const Value: string); // Polaris
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetImage: TGetImageEvent read FOnGetImage write FOnGetImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearValue;
    function Locate(const SearchField: TField; const AValue: string;
      Exact: Boolean): Boolean;
    procedure ResetField; virtual;
{$IFDEF RX_D4}
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function UseRightToLeftAlignment: Boolean; override;
{$ENDIF}
    property Field: TField read GetField;
  end;

{ TRxDBLookupList }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBLookupList = class(TRxLookupControl)
  private
    FRecordIndex: Integer;
    FRecordCount: Integer;
    FRowCount: Integer;
    FBorderStyle: TBorderStyle;
    FKeySelected: Boolean;
    FTracking: Boolean;
    FTimerActive: Boolean;
    FLockPosition: Boolean;
    FSelectEmpty: Boolean;
    FMousePos: Integer;
    function GetKeyIndex: Integer;
    procedure ListDataChanged;
    procedure SelectCurrent;
    procedure SelectItemAt(X, Y: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetRowCount(Value: Integer);
    procedure StopTimer;
    procedure StopTracking;
    procedure TimerScroll;
    procedure UpdateScrollBar;
    procedure UpdateBufferCount(Rows: Integer);
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMTimer(var Message: TMessage); message WM_TIMER;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyValueChanged; override;
    procedure DisplayValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure UpdateDisplayEmpty(const Value: string); override;
{$IFDEF RX_D4}
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DrawItemText(Canvas: TCanvas; Rect: TRect;
      Selected, IsEmpty: Boolean); virtual;
    property RowCount: Integer read FRowCount write SetRowCount stored False;
    property DisplayValue;
    property Value;
{$IFNDEF VER80}
    property KeyValue;
{$ENDIF}
  published
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Align;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DisplayEmpty;
    property DragCursor;
    property DragMode;
    property EmptyItemColor;
    property EmptyValue;
    property EmptyStrIsNull; // Polaris
    property Enabled;
    property FieldsDelimiter;
    property Font;
    property IgnoreCase;
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
    property IndexSwitch;
    property ItemHeight;
    property ListStyle;
    property LookupField;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImage;
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
  end;

{ TRxDBLookupCombo }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxPopupDataList = class(TRxDBLookupList)
  private
    FCombo: TRxLookupControl;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
{$IFDEF VER80}
    procedure CreateWnd; override;
{$ENDIF}
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{$IFDEF VER80}
  TDropDownAlign = (daLeft, daRight, daCenter);
{$ENDIF}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBLookupCombo = class(TRxLookupControl)
  private
    FDataList: TRxPopupDataList;
    FButtonWidth: Integer;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FDropDownAlign: TDropDownAlign;
    FEscapeClear: Boolean;
    FListVisible: Boolean;
    FPressed: Boolean;
    FTracking: Boolean;
    FAlignment: TAlignment;
    FSelImage: TPicture;
    FSelMargin: Integer;
    FDisplayValues: TStrings;
    FDisplayAll: Boolean;
{$IFDEF VER80}
    FBtnGlyph: TBitmap;
    FBtnDisabled: TBitmap;
{$ENDIF}
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopTracking;
    procedure TrackButton(X, Y: Integer);
    function GetMinHeight: Integer;
    function GetText: string;
    procedure InvalidateText;
    procedure UpdateCurrentImage;
    procedure PaintDisplayValues(Canvas: TCanvas; R: TRect; ALeft: Integer);
    procedure SetFieldsDelim(Value: Char); override;
    procedure SetListStyle(Value: TLookupListStyle); override;
    function GetDisplayAll: Boolean;
    procedure SetDisplayAll(Value: Boolean);
    function GetDisplayValues(Index: Integer): string; 
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
{$IFNDEF VER80}
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
{$ENDIF}
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
{$IFDEF RX_D4}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
{$ENDIF}
  protected
    procedure Click; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; override;
    procedure UpdateFieldText;
    procedure KeyValueChanged; override;
    procedure DisplayValueChanged; override;
    procedure ListLinkActiveChanged; override;
    procedure ListLinkDataChanged; override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateDisplayEmpty(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DropDown; virtual;
    procedure ResetField; override;
    property IsDropDown: Boolean read FListVisible;
    property ListVisible: Boolean read FListVisible;
    property Text: string read GetText;
    property DisplayValue;
    property DisplayValues[Index: Integer]: string read GetDisplayValues;
    property Value;
{$IFNDEF VER80}
    property KeyValue;
{$ENDIF}
  published
    property Align;   // Polaris
    property DropDownAlign: TDropDownAlign read FDropDownAlign write FDropDownAlign default daLeft;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount default 7;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property EscapeClear: Boolean read FEscapeClear write FEscapeClear default True;
    property DisplayAllFields: Boolean read GetDisplayAll write SetDisplayAll default False;
    property Color;
    property Ctl3D;
    property DataField;
    property DataSource;
    property DisplayEmpty;
    property DragCursor;
    property DragMode;
    property EmptyValue;
    property EmptyStrIsNull; // Polaris
    property EmptyItemColor;
    property Enabled;
    property FieldsDelimiter;
    property Font;
    property IgnoreCase;
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
    property IndexSwitch;
    property ItemHeight;
    property ListStyle;
    property LookupField;
    property LookupDisplay;
    property LookupDisplayIndex;
    property LookupSource;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImage;
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

{ TPopupDataWindow }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPopupDataWindow = class(TRxPopupDataList)
  private
    FEditor: TWinControl;
    FCloseUp: TCloseUpEvent;
  protected
    procedure InvalidateEditor;
    procedure Click; override;
    procedure DisplayValueChanged; override;
    function GetPicture(Current, Empty: Boolean; var TextMargin: Integer): TGraphic; override;
    procedure KeyPress(var Key: Char); override;
    procedure PopupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseUp(Accept: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Hide;
    procedure Show(Origin: TPoint);
    property OnCloseUp: TCloseUpEvent read FCloseUp write FCloseUp;
  end;

{ TRxLookupEdit }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxLookupEdit = class(TCustomComboEdit)
  private
    FChanging: Boolean;
    FIgnoreChange: Boolean;
    FDropDownCount: Integer;
    FDropDownWidth: Integer;
    FPopupOnlyLocate: Boolean;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    function GetListStyle: TLookupListStyle;
    procedure SetListStyle(Value: TLookupListStyle);
    function GetFieldsDelim: Char;
    procedure SetFieldsDelim(Value: Char);
    function GetLookupDisplay: string;
    procedure SetLookupDisplay(const Value: string);
    function GetDisplayIndex: Integer;
    procedure SetDisplayIndex(Value: Integer);
    function GetLookupField: string;
    procedure SetLookupField(const Value: string);
    function GetLookupSource: TDataSource;
    procedure SetLookupSource(Value: TDataSource);
    procedure SetDropDownCount(Value: Integer);
    function GetLookupValue: string;
    procedure SetLookupValue(const Value: string);
    function GetOnGetImage: TGetImageEvent;
    procedure SetOnGetImage(Value: TGetImageEvent);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure ShowPopup(Origin: TPoint); override;
    procedure HidePopup; override;
    procedure PopupChange; override;
    procedure PopupDropDown(DisableEdit: Boolean); override;
{$IFNDEF VER80}
    function AcceptPopup(var Value: Variant): Boolean; override;
    procedure SetPopupValue(const Value: Variant); override;
    function GetPopupValue: Variant; override;
{$ELSE}
    function AcceptPopup(var Value: string): Boolean; override;
    procedure SetPopupValue(const Value: string); override;
    function GetPopupValue: string; override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LookupValue: string read GetLookupValue write SetLookupValue;
  published
    property DropDownCount: Integer read FDropDownCount write SetDropDownCount default 8;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property ListStyle: TLookupListStyle read GetListStyle write SetListStyle default lsFixed;
    property FieldsDelimiter: Char read GetFieldsDelim write SetFieldsDelim default DefFieldsDelim;
    property LookupDisplay: string read GetLookupDisplay write SetLookupDisplay;
    property LookupDisplayIndex: Integer read GetDisplayIndex write SetDisplayIndex default 0;
    property LookupField: string read GetLookupField write SetLookupField;
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property PopupOnlyLocate: Boolean read FPopupOnlyLocate write FPopupOnlyLocate default True;
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
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupAlign;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnGetImage: TGetImageEvent read GetOnGetImage write SetOnGetImage;
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
{   Compiled with RxLib 2.60 and better.
    Component - both DBEdit and DBLookup. If anyone knows how to
  make it easier to contact me at vs@balance.dp.ua.
    I understand that it is ideologically wrong - to edit the same
  data in two different places, but sometimes necessary.
    You can distribute, use and modify as you wish. If you
  improve this component (eg, add an event "manually input element
  which is not in Lookup-table and Post ") - please send me a new
  module.
    Today, catching another penultimate serious bug, and decided - the world
  work to do better. Use and enjoy, once again, perfect -
  send ...
    It's still raw, actually. But something might.
 }

{   Conditional DEFINE RIGHT_SPACE_ALIGN can enable property
  "Align the right spaces to a certain width". Since it must not always.
    Property RightSpaceAlign: if False, the alignment is not performed,
    otherwise, any entered value catches up with spaces to the left
    width Field.Size}

{$DEFINE RIGHT_SPACE_ALIGN}

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxDBLookupEditEx = class(TRxLookupEdit)
  private
    FDataLink: TFieldDataLink;
    FAlignment: TAlignment;
    FFocused: Boolean;
{$IFDEF RIGHT_SPACE_ALIGN}
    FRightSpaceAlign: boolean;
{$ENDIF}
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    // here warning - is it need - VS.
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
  {This is the patch. Change is only called before the first CloseUp,
   Since the State of FDataLink changed only once (for Editing).
   Now the rest of the CloseUp checked this box and need. called Change
   _invisible_ * Post * the database.}
    ChangeInvoked: boolean;
    function GetReadOnly: Boolean; override; // to override - VS.
    procedure Change; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Reset; override;
    procedure DropDown;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcceptPopup(var Value: Variant): Boolean; override;

    property Field: TField read GetField;
  published

    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
{$IFDEF RIGHT_SPACE_ALIGN}
    property RightSpaceAlign: boolean read FRightSpaceAlign write FRightSpaceAlign;
{$ENDIF}
  end;

implementation

uses DBConsts, Dialogs, {$IFDEF VER80} RxStr16, {$ENDIF} RxVCLUtils,
  {$IFNDEF RX_D3} BdeUtils, {$ENDIF} RxMaxMin, RxClipIcon,
  {$IFDEF RX_D6} Variants, VDBConsts, RTLConsts,{$ENDIF} RxStrUtils; // Polaris

{ TDataSourceLink }

procedure TDataSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then FDataControl.DataLinkActiveChanged;
end;

procedure TDataSourceLink.LayoutChanged;
begin
  if FDataControl <> nil then FDataControl.CheckDataLinkActiveChanged;
end;

procedure TDataSourceLink.RecordChanged(Field: TField);
begin
  if FDataControl <> nil then FDataControl.DataLinkRecordChanged(Field);
end;

procedure TDataSourceLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (FDataControl <> nil) and
    (Field^ = FDataControl.FDataField) and FDataControl.CanFocus then
  begin
    Field^ := nil;
    FDataControl.SetFocus;
  end;
end;

{ TLookupSourceLink }

procedure TLookupSourceLink.ActiveChanged;
begin
  if FDataControl <> nil then FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.LayoutChanged;
begin
  if FDataControl <> nil then FDataControl.ListLinkActiveChanged;
end;

procedure TLookupSourceLink.DataSetChanged;
begin
  if FDataControl <> nil then FDataControl.ListLinkDataChanged;
end;

{ TRxLookupControl }

const
  SearchTickCount: Longint = 0;

{$IFDEF VER80}
procedure GetFieldList(DataSet: TDataSet; List: TList;
  const FieldNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(FieldNames) do
    List.Add(DataSet.FieldByName(ExtractFieldName(FieldNames, Pos)));
end;
{$ENDIF}

constructor TRxLookupControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if NewStyleControls then ControlStyle := [csOpaque]
  else ControlStyle := [csOpaque, csFramed];
  ParentColor := False;
  TabStop := True;
  FFieldsDelim := DefFieldsDelim;
  FLookupSource := TDataSource.Create(Self);
  FDataLink := TDataSourceLink.Create;
  FDataLink.FDataControl := Self;
  FLookupLink := TLookupSourceLink.Create;
  FLookupLink.FDataControl := Self;
  FListFields := {$IFDEF RX_D17}TList<TField>.Create{$ELSE}TList.Create{$ENDIF};
  FEmptyValue := EmptyStr;
  FEmptyStrIsNull := True; // Polaris
  FEmptyItemColor := clWindow;
  FValue := FEmptyValue;
{$IFDEF RX_D3}
  FLocate := CreateLocate(nil);
{$ELSE}
  FLocate := TDBLocate.Create;
{$ENDIF}
  FIndexSwitch := True;
  FIgnoreCase := True;
end;

destructor TRxLookupControl.Destroy;
begin
  FListFields.Free;
  FListFields := nil;
  FLookupLink.FDataControl := nil;
  FLookupLink.Free;
  FLookupLink := nil;
  FDataLink.FDataControl := nil;
  FDataLink.Free;
  FDataLink := nil;
  FLocate.Free;
  FLocate := nil;
  inherited Destroy;
end;

function TRxLookupControl.CanModify: Boolean;
begin
  Result := FListActive and not ReadOnly and ((FDataLink.DataSource = nil) or
    (FMasterField <> nil) and FMasterField.CanModify);
end;

procedure TRxLookupControl.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TRxLookupControl.ValueIsEmpty(const S: string): Boolean;
begin
  Result := (S = FEmptyValue);
end;

function TRxLookupControl.StoreEmpty: Boolean;
begin
  Result := (FEmptyValue <> EmptyStr);
end;

{$IFNDEF VER80}
procedure TRxLookupControl.CheckNotFixed;
begin
  if FLookupMode then _DBError(SPropDefByLookup);
  if FDataLink.DataSourceFixed then _DBError(SDataSourceFixed);
end;

procedure TRxLookupControl.SetLookupMode(Value: Boolean);
begin
  if FLookupMode <> Value then
    if Value then begin
      FMasterField := FDataField.DataSet.FieldByName(FDataField.KeyFields);
      FLookupSource.DataSet := FDataField.LookupDataSet;
      FLookupFieldName := FDataField.LookupKeyFields;
      FLookupMode := True;
      FLookupLink.DataSource := FLookupSource;
    end else
    begin
      FLookupLink.DataSource := nil;
      FLookupMode := False;
      FLookupFieldName := '';
      FLookupSource.DataSet := nil;
      FMasterField := FDataField;
    end;
end;

function TRxLookupControl.GetKeyValue: Variant;
begin
  if ValueIsEmpty(Value) then 
    if (Value = EmptyStr) and FEmptyStrIsNull then Result := Null // Polaris
    else Result := FEmptyValue // Polaris
  else Result := Value;
end;

procedure TRxLookupControl.SetKeyValue(const Value: Variant);
begin
  if VarIsNull(Value) then Self.Value := FEmptyValue // Polaris
  else Self.Value := Value;
//  Self.Value := Value;
end;
{$ENDIF}

procedure TRxLookupControl.CheckNotCircular;
begin
  {
  if FDataLink.Active and FDataLink.DataSet.IsLinkedTo(LookupSource) then
    _DBError(SCircularDataLink);
  }
  if FDataLink.Active and ((DataSource = LookupSource) or
    (FDataLink.DataSet = FLookupLink.DataSet)) then
    _DBError(SCircularDataLink);
end;

procedure TRxLookupControl.CheckDataLinkActiveChanged;
var
  TestField: TField;
begin
  if FDataLink.Active and (FDataFieldName <> '') then begin
    TestField := FDataLink.DataSet.FieldByName(FDataFieldName);
    if Pointer(FDataField) <> Pointer(TestField) then begin
      FDataField := nil;
      FMasterField := nil;
      CheckNotCircular;
      FDataField := TestField;
      FMasterField := FDataField;
      DataLinkRecordChanged(nil);
    end;
  end;
end;

procedure TRxLookupControl.DataLinkActiveChanged;
begin
  FDataField := nil;
  FMasterField := nil;
  if FDataLink.Active and (FDataFieldName <> '') then begin
    CheckNotCircular;
    FDataField := FDataLink.DataSet.FieldByName(FDataFieldName);
    FMasterField := FDataField;
  end;
{$IFNDEF VER80}
  SetLookupMode((FDataField <> nil) and FDataField.Lookup);
{$ENDIF}
  DataLinkRecordChanged(nil);
end;

procedure TRxLookupControl.DataLinkRecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FMasterField) then begin
    if FMasterField <> nil then begin
      SetValueKey(FMasterField.AsString);
    end else SetValueKey(FEmptyValue);
  end;
end;

{$IFDEF RX_D4}
function TRxLookupControl.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or ((FDataLink <> nil) and
    FDataLink.ExecuteAction(Action));
end;

function TRxLookupControl.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or ((FDataLink <> nil) and
    FDataLink.UpdateAction(Action));
end;

function TRxLookupControl.UseRightToLeftAlignment: Boolean;
begin
  Result := DBUseRightToLeftAlignment(Self, Field);
end;
{$ENDIF}

function TRxLookupControl.GetBorderSize: Integer;
var
  Params: TCreateParams;
  R: TRect;
begin
  CreateParams(Params);
  SetRect(R, 0, 0, 0, 0);
{$IFNDEF VER80}
  AdjustWindowRectEx(R, Params.Style, False, Params.ExStyle);
{$ELSE}
  AdjustWindowRect(R, Params.Style, False);
  if (csFramed in ControlStyle) and Ctl3D and 
    (Params.Style and WS_BORDER <> 0) then Inc(R.Bottom, 2);
{$ENDIF}
  Result := R.Bottom - R.Top;
end;

function TRxLookupControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TRxLookupControl.GetLookupField: string;
begin
{$IFNDEF VER80}
  if FLookupMode then Result := '' else
{$ENDIF}
  Result := FLookupFieldName;
end;

function TRxLookupControl.GetLookupSource: TDataSource;
begin
{$IFNDEF VER80}
  if FLookupMode then Result := nil else
{$ENDIF}
  Result := FLookupLink.DataSource;
end;

function TRxLookupControl.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TRxLookupControl.GetField: TField;
begin
  if Assigned(FDataLink) then Result := FDataField
  else Result := nil;
end;

function TRxLookupControl.DefaultTextHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function TRxLookupControl.GetTextHeight: Integer;
begin
  Result := Max(DefaultTextHeight, FItemHeight);
end;

procedure TRxLookupControl.KeyValueChanged;
begin
end;

procedure TRxLookupControl.DisplayValueChanged;
begin
end;

procedure TRxLookupControl.ListLinkActiveChanged;
var
  DataSet: TDataSet;
{$IFNDEF VER80}
  ResultField: TField;
{$ENDIF}
begin
  FListActive := False;
  FKeyField := nil;
  FDisplayField := nil;
  FListFields.Clear;
  if FLookupLink.Active and (FLookupFieldName <> '') then begin
    CheckNotCircular;
    DataSet := FLookupLink.DataSet;
    FKeyField := DataSet.FieldByName(FLookupFieldName);
{$IFNDEF VER80}
    DataSet.GetFieldList(FListFields, FLookupDisplay);
{$ELSE}
    GetFieldList(DataSet, FListFields, FLookupDisplay);
{$ENDIF}
{$IFNDEF VER80}
    if FLookupMode then begin
      ResultField := DataSet.FieldByName(FDataField.LookupResultField);
      if FListFields.IndexOf(ResultField) < 0 then
        FListFields.Insert(0, ResultField);
      FDisplayField := ResultField;
    end
    else begin
      if FListFields.Count = 0 then FListFields.Add(FKeyField);
      if (FDisplayIndex >= 0) and (FDisplayIndex < FListFields.Count) then
        FDisplayField := FListFields[FDisplayIndex]
      else FDisplayField := FListFields[0];
    end;
{$ELSE}
    if FListFields.Count = 0 then FListFields.Add(FKeyField);
    if (FDisplayIndex >= 0) and (FDisplayIndex < FListFields.Count) then
      FDisplayField := FListFields[FDisplayIndex]
    else FDisplayField := FListFields[0];
{$ENDIF}
    FListActive := True;
  end;
  FLocate.DataSet := FLookupLink.DataSet;
end;

procedure TRxLookupControl.ListLinkDataChanged;
begin
end;

function TRxLookupControl.LocateDisplay: Boolean;
begin
  Result := False;
  try
    Result := Locate(FDisplayField, FDisplayValue, True);
  except
  end;
end;

function TRxLookupControl.LocateKey: Boolean;
begin
  Result := False;
  try
    Result := not ValueIsEmpty(FValue) and Locate(FKeyField, FValue, True);
  except
  end;
end;

procedure TRxLookupControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
    if (FLookupLink <> nil) and (AComponent = LookupSource) then
      LookupSource := nil;
  end;
end;

function TRxLookupControl.SearchText(var AValue: string): Boolean;
begin
  Result := False;
  if (FDisplayField <> nil) then
    if (AValue <> '') and Locate(FDisplayField, AValue, False) then begin
      SelectKeyValue(FKeyField.AsString);
      AValue := Copy(FDisplayField.AsString, 1, Length(AValue));
      Result := True;
    end
    else if AValue = '' then begin
      FLookupLink.DataSet.First;
      SelectKeyValue(FKeyField.AsString);
      AValue := '';
    end;
end;

procedure TRxLookupControl.ProcessSearchKey(Key: Char);
var
  TickCount: Longint;
  S: string;
begin
  S := '';
  if (FDisplayField <> nil) {and (FDisplayField.DataType = ftString)} then
  begin
    if (Key = Char(VK_BACK)) or (Key >=' ') then
    begin
      if CanModify then
      begin
        if not FPopup then
        begin
          TickCount := GetTickCount;
          if TickCount - SearchTickCount > 2000 then FSearchText := '';
          SearchTickCount := TickCount;
        end;
        if (Key = Char(VK_BACK)) then
          S := Copy(FSearchText, 1, Length(FSearchText) - 1)
        else if Length(FSearchText) < 32 then
          S := FSearchText + Key;
        if SearchText(S) or (S = '') then FSearchText := S;
      end;
    end
    else
    case Key of
      #9, #27: FSearchText := '';
    end;
  end;
end;

procedure TRxLookupControl.ResetField;
begin
{  if (FDataLink.DataSource = nil) or
    ((FDataLink.DataSource <> nil) and CanModify) then
  begin
    if (FDataLink.DataSource <> nil) and (FMasterField <> nil) and
      FDataLink.Edit then
    begin
      if (FEmptyValue = EmptyStr) then FMasterField.Clear
      else FMasterField.AsString := FEmptyValue;
    end;}       // Polaris
  if (FDataLink.DataSource = nil) or
     (FMasterField = nil) or FDataLink.Edit then
  begin
    if FDataLink.Edit then
      SetFieldValue(FMasterField,FEmptyValue); // Polaris
    FValue := FEmptyValue;
    FDisplayValue := EmptyStr;
    inherited Text := DisplayEmpty;
    Invalidate;
    Click;
  end;
end;

procedure TRxLookupControl.ClearValue;
begin
  SetValueKey(FEmptyValue);
end;

procedure TRxLookupControl.SelectKeyValue(const Value: string);
begin
  if FMasterField <> nil then begin
    if CanModify and FDataLink.Edit then begin
      if FDataField = FMasterField then FDataField.DataSet.Edit;
//      FMasterField.AsString := Value;
      SetFieldValue(FMasterField,Value); // Polaris
    end
    else Exit;
  end
  else SetValueKey(Value);
  UpdateDisplayValue;
  Repaint;
  Click;
end;

procedure TRxLookupControl.SetDataFieldName(const Value: string);
begin
  if FDataFieldName <> Value then begin
    FDataFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TRxLookupControl.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
{$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TRxLookupControl.SetListStyle(Value: TLookupListStyle);
begin
  if FListStyle <> Value then begin
    FListStyle := Value;
    Invalidate;
  end;
end;

procedure TRxLookupControl.SetFieldsDelim(Value: Char);
begin
  if FFieldsDelim <> Value then begin
    FFieldsDelim := Value;
    if ListStyle = lsDelimited then Invalidate;
  end;
end;

procedure TRxLookupControl.SetLookupField(const Value: string);
begin
{$IFNDEF VER80}
  CheckNotFixed;
{$ENDIF}
  if FLookupFieldName <> Value then begin
    FLookupFieldName := Value;
    ListLinkActiveChanged;
    if FListActive then DataLinkRecordChanged(nil);
  end;
end;

procedure TRxLookupControl.SetDisplayEmpty(const Value: string);
begin
  if FDisplayEmpty <> Value then begin
    UpdateDisplayEmpty(Value);
    FDisplayEmpty := Value;
    if not (csReading in ComponentState) then Invalidate;
  end;
end;

procedure TRxLookupControl.SetEmptyValue(const Value: string);
begin
  if FEmptyValue <> Value then begin
    if ValueIsEmpty(FValue) then FValue := Value;
    FEmptyValue := Value;
  end;
end;

// Polaris begin
procedure TRxLookupControl.SetFieldValue(Field: TField; const Value: string);
begin
  if Value = FEmptyValue then
    if (FEmptyValue = EmptyStr) and FEmptyStrIsNull then Field.Clear
    else Field.AsString := FEmptyValue
  else
    Field.AsString := Value;
end;

procedure TRxLookupControl.SetEmptyStrIsNull(const Value: Boolean);
begin
  if FEmptyStrIsNull <> Value then begin
    FEmptyStrIsNull := Value;
    if CanModify and (FDataLink.DataSource <> nil) and FDataLink.Edit then
      if FMasterField <> nil then SetFieldValue(FMasterField,FValue)
      else SetFieldValue(FDataField,FValue);
  end;
end;
// Polaris end

procedure TRxLookupControl.SetEmptyItemColor(Value: TColor);
begin
  if FEmptyItemColor <> Value then begin
    FEmptyItemColor := Value;
    if not (csReading in ComponentState) and (DisplayEmpty <> '') then
      Invalidate;
  end;
end;

procedure TRxLookupControl.UpdateDisplayEmpty(const Value: string);
begin
end;

procedure TRxLookupControl.SetDisplayValue(const Value: string);
{var
  S: string;}  // Polaris
begin
  if (FDisplayValue <> Value) and CanModify and (FDataLink.DataSource <> nil) and
    Locate(FDisplayField, Value, True) then
  begin
//    S := FValue;  // Polaris
    if FDataLink.Edit then begin
//      if FMasterField <> nil then FMasterField.AsString := S
//      else FDataField.AsString := S;
      if FMasterField <> nil then SetFieldValue(FMasterField,FValue) // Polaris
      else SetFieldValue(FDataField,FValue); // Polaris
    end;
  end
  else if (FDisplayValue <> Value) then begin
    FDisplayValue := Value;
    DisplayValueChanged;
    Change;
  end;
end;

procedure TRxLookupControl.UpdateKeyValue;
begin
  if FMasterField <> nil then FValue := FMasterField.AsString
  else FValue := FEmptyValue;
  KeyValueChanged;
end;

procedure TRxLookupControl.SetValueKey(const Value: string);
begin
  if FValue <> Value then begin
    FValue := Value;
    KeyValueChanged;
  end;
end;

procedure TRxLookupControl.SetValue(const Value: string);
begin
  if (Value <> FValue) then begin // Polaris // begin added
    if CanModify and (FDataLink.DataSource <> nil) and FDataLink.Edit then
    begin
//      if FMasterField <> nil then FMasterField.AsString := Value
//      else FDataField.AsString := Value;
      if FMasterField <> nil then SetFieldValue(FMasterField,Value) // Polaris
      else SetFieldValue(FDataField,Value); // Polaris
    end
    else // begin  // Polaris
      SetValueKey(Value);
      Change;
    end;
end;

procedure TRxLookupControl.SetLookupDisplay(const Value: string);
begin
  if FLookupDisplay <> Value then begin
    FLookupDisplay := Value;
    ListLinkActiveChanged;
    if FListActive then DataLinkRecordChanged(nil);
  end;
end;

procedure TRxLookupControl.SetLookupSource(Value: TDataSource);
begin
{$IFNDEF VER80}
  CheckNotFixed;
{$ENDIF}
  FLookupLink.DataSource := Value;
{$IFNDEF VER80}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
  if Value <> nil then FLocate.DataSet := Value.DataSet
  else FLocate.DataSet := nil;
  if FListActive then DataLinkRecordChanged(nil);
end;

procedure TRxLookupControl.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TRxLookupControl.GetItemHeight: Integer;
begin
  Result := {Max(GetTextHeight, FItemHeight);}GetTextHeight;
end;

procedure TRxLookupControl.SetItemHeight(Value: Integer);
begin
  if not (csReading in ComponentState) then
    FItemHeight := Max(DefaultTextHeight, Value)
  else FItemHeight := Value;
  Perform(CM_FONTCHANGED, 0, 0);
end;

function TRxLookupControl.ItemHeightStored: Boolean;
begin
  Result := FItemHeight > DefaultTextHeight;
end;

procedure TRxLookupControl.DrawPicture(Canvas: TCanvas; Rect: TRect;
  Image: TGraphic);
var
  X, Y, SaveIndex: Integer;
{$IFNDEF VER80}
  Ico: HIcon;
  W, H: Integer;
{$ENDIF}
begin
  if Image <> nil then begin
    X := (Rect.Right + Rect.Left - Image.Width) div 2;
    Y := (Rect.Top + Rect.Bottom - Image.Height) div 2;
    SaveIndex := SaveDC(Canvas.Handle);
    try
      IntersectClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right,
        Rect.Bottom);
      if Image is TBitmap then
        DrawBitmapTransparent(Canvas, X, Y, TBitmap(Image),
          TBitmap(Image).TransparentColor)
{$IFNDEF VER80}
      else if Image is TIcon then begin
        Ico := CreateRealSizeIcon(TIcon(Image));
        try
          GetIconSize(Ico, W, H);
          DrawIconEx(Canvas.Handle, (Rect.Right + Rect.Left - W) div 2,
            (Rect.Top + Rect.Bottom - H) div 2, Ico, W, H, 0, 0, DI_NORMAL);
        finally
          DestroyIcon(Ico);
        end;
      end
{$ENDIF}
      else Canvas.Draw(X, Y, Image);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
  end;
end;

function TRxLookupControl.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  TextMargin := 0;
  Result := nil;
  if Assigned(FOnGetImage) then FOnGetImage(Self, Empty, Result, TextMargin);
end;

procedure TRxLookupControl.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TRxLookupControl.WMKillFocus(var Message: TMessage);
begin
  FFocused := False;
  Invalidate;
end;

procedure TRxLookupControl.WMSetFocus(var Message: TMessage);
begin
  FFocused := True;
  Invalidate;
end;

function TRxLookupControl.Locate(const SearchField: TField;
  const AValue: string; Exact: Boolean): Boolean;
begin
  FLocate.IndexSwitch := FIndexSwitch;
  Result := False;
  try
    if not ValueIsEmpty(AValue) and (SearchField <> nil) then begin
      Result := FLocate.Locate(SearchField.FieldName, AValue, Exact,
        not IgnoreCase);
      if Result then begin
        if SearchField = FDisplayField then FValue := FKeyField.AsString;
        UpdateDisplayValue;
      end;
    end;
  except
  end;
end;

function TRxLookupControl.EmptyRowVisible: Boolean;
begin
  Result := DisplayEmpty <> EmptyStr;
end;

procedure TRxLookupControl.UpdateDisplayValue;
begin
  if not ValueIsEmpty(FValue) then begin
    if FDisplayField <> nil then
      FDisplayValue := FDisplayField.AsString
    else FDisplayValue := '';
  end
  else FDisplayValue := '';
end;

function TRxLookupControl.GetWindowWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FListFields.Count - 1 do
    Inc(Result, TField(FListFields[I]).DisplayWidth);
  Canvas.Font := Font;
  Result := Min(Result * Canvas.TextWidth('M') + FListFields.Count * 4 +
    GetSystemMetrics(SM_CXVSCROLL), Screen.Width);
end;

{ TRxDBLookupList }

constructor TRxDBLookupList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 121;
  Ctl3D := True;
  FBorderStyle := bsSingle;
{$IFNDEF VER80}
  ControlStyle := [csOpaque, csDoubleClicks];
{$ELSE}
  ControlStyle := [csFramed, csOpaque, csDoubleClicks];
{$ENDIF}
  RowCount := 7;
end;

procedure TRxDBLookupList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_VSCROLL;
    if FBorderStyle = bsSingle then
{$IFNDEF VER80}
      if NewStyleControls and Ctl3D then ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else Style := Style or WS_BORDER;
{$ELSE}
      Style := Style or WS_BORDER;
{$ENDIF}
  end;
end;

procedure TRxDBLookupList.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

procedure TRxDBLookupList.Loaded;
begin
  inherited Loaded;
  Height := Height;
end;

function TRxDBLookupList.GetKeyIndex: Integer;
var
  FieldValue: string;
begin
  if not ValueIsEmpty(FValue) then
    for Result := 0 to FRecordCount - 1 do begin
      FLookupLink.ActiveRecord := Result;
      FieldValue := FKeyField.AsString;
      FLookupLink.ActiveRecord := FRecordIndex;
      if FieldValue = FValue then Exit;
    end;
  Result := -1;
end;

procedure TRxDBLookupList.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta, KeyIndex, EmptyRow: Integer;
begin
  inherited KeyDown(Key, Shift);
  FSelectEmpty := False;
  EmptyRow := Ord(EmptyRowVisible);
  if CanModify then begin
    Delta := 0;
    case Key of
      VK_UP, VK_LEFT: Delta := -1;
      VK_DOWN, VK_RIGHT: Delta := 1;
      VK_PRIOR: Delta := 1 - (FRowCount - EmptyRow);
      VK_NEXT: Delta := (FRowCount - EmptyRow) - 1;
      VK_HOME: Delta := -Maxint;
      VK_END: Delta := Maxint;
    end;
    if Delta <> 0 then begin
      if ValueIsEmpty(Value) and (EmptyRow > 0) and (Delta < 0) then
        FSelectEmpty := True;
      FSearchText := '';
      if Delta = -Maxint then FLookupLink.DataSet.First
      else if Delta = Maxint then FLookupLink.DataSet.Last
      else begin
        KeyIndex := GetKeyIndex;
        if KeyIndex >= 0 then begin
          FLookupLink.DataSet.MoveBy(KeyIndex - FRecordIndex);
        end
        else begin
          KeyValueChanged;
          Delta := 0;
        end;
        FLookupLink.DataSet.MoveBy(Delta);
        if FLookupLink.DataSet.BOF and (Delta < 0) and (EmptyRow > 0) then
          FSelectEmpty := True;
      end;
      SelectCurrent;
    end;
  end;
end;

procedure TRxDBLookupList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  ProcessSearchKey(Key);
end;

procedure TRxDBLookupList.KeyValueChanged;
begin
  if FListActive and not FLockPosition then
    if not LocateKey then FLookupLink.DataSet.First;
end;

procedure TRxDBLookupList.DisplayValueChanged;
begin
  if FListActive and not FLockPosition then
    if not LocateDisplay then FLookupLink.DataSet.First;
end;

procedure TRxDBLookupList.ListLinkActiveChanged;
begin
  try
    inherited ListLinkActiveChanged;
  finally
    if FListActive and not FLockPosition then begin
      if Assigned(FMasterField) then UpdateKeyValue
      else KeyValueChanged;
    end
    else ListDataChanged;
  end;
end;

procedure TRxDBLookupList.ListDataChanged;
begin
  if FListActive then begin
    FRecordIndex := FLookupLink.ActiveRecord;
    FRecordCount := FLookupLink.Dataset.RecordCount; // Dataset added by Alexey Kogosov (Scroll bar fix)

    FKeySelected := not ValueIsEmpty(FValue) or not FLookupLink.DataSet.BOF;
  end
  else begin
    FRecordIndex := 0;
    FRecordCount := 0;
    FKeySelected := False;
  end;
  if HandleAllocated then begin
    UpdateScrollBar;
    Invalidate;
  end;
end;

procedure TRxDBLookupList.ListLinkDataChanged;
begin
  ListDataChanged;
end;

procedure TRxDBLookupList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then begin
    FSearchText := '';
    if not FPopup then begin
      if CanFocus then SetFocus;
      if not FFocused then Exit;
    end;
    if CanModify then
      if ssDouble in Shift then begin
        if FRecordIndex = Y div GetTextHeight then DblClick;
      end
      else begin
        MouseCapture := True;
        FTracking := True;
        SelectItemAt(X, Y);
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxDBLookupList.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FTracking then begin
    SelectItemAt(X, Y);
    FMousePos := Y;
    TimerScroll;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRxDBLookupList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FTracking then begin
    StopTracking;
    SelectItemAt(X, Y);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TRxDBLookupList.DrawItemText(Canvas: TCanvas; Rect: TRect;
  Selected, IsEmpty: Boolean);
var
  J, W, X, ATop, TextWidth, LastFieldIndex: Integer;
  S: string;
  Field: TField;
  R: TRect;
  AAlignment: TAlignment;
begin
  TextWidth := Canvas.TextWidth('M');
  LastFieldIndex := FListFields.Count - 1;
  R := Rect;
  R.Right := R.Left;
  S := '';
  ATop := (R.Bottom + R.Top - Canvas.TextHeight('Xy')) div 2;
  for J := 0 to LastFieldIndex do begin
    Field := FListFields[J];
    if FListStyle = lsFixed then begin
      if J < LastFieldIndex then W := Field.DisplayWidth * TextWidth + 4
      else W := ClientWidth - R.Right;
      if IsEmpty then begin
        if J = 0 then begin
          S := DisplayEmpty;
        end
        else S := '';
      end
      else S := Field.DisplayText;
      X := 2;
      AAlignment := Field.Alignment;
{$IFDEF RX_D4}
      if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
{$ENDIF}
      case AAlignment of
        taRightJustify: X := W - Canvas.TextWidth(S) - 3;
        taCenter: X := (W - Canvas.TextWidth(S)) div 2;
      end;
      R.Left := R.Right;
      R.Right := R.Right + W;
{$IFDEF RX_D4}
      if SysLocale.MiddleEast and UseRightToLeftReading then
        Canvas.TextFlags := Canvas.TextFlags or ETO_RTLREADING
      else
        Canvas.TextFlags := Canvas.TextFlags and not ETO_RTLREADING;
{$ENDIF}
      Canvas.TextRect(R, R.Left + X, ATop, S);
      if J < LastFieldIndex then begin
        Canvas.MoveTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom);
        Inc(R.Right);
        if R.Right >= ClientWidth then Break;
      end;
    end
    else {if FListStyle = lsDelimited then} if not IsEmpty then begin
      S := S + Field.DisplayText;
      if J < LastFieldIndex then S := S + FFieldsDelim + ' ';
    end;
  end;
  if (FListStyle = lsDelimited) then begin
    if IsEmpty then
      S := DisplayEmpty;
    R.Left := Rect.Left;
    R.Right := Rect.Right;
{$IFDEF RX_D4}
    if SysLocale.MiddleEast and UseRightToLeftReading then
      Canvas.TextFlags := Canvas.TextFlags or ETO_RTLREADING
    else
      Canvas.TextFlags := Canvas.TextFlags and not ETO_RTLREADING;
{$ENDIF}
    Canvas.TextRect(R, R.Left + 2, ATop, S);
  end;
end;

procedure TRxDBLookupList.Paint;
var
  I, J, TextHeight, TextMargin: Integer;
  Image: TGraphic;
  Bmp: TBitmap;
  R, ImageRect: TRect;
  Selected: Boolean;
begin
  Bmp := TBitmap.Create;
  try
    Canvas.Font := Font;
    TextHeight := GetTextHeight;
    if ColorToRGB(Color) <> ColorToRGB(clBtnFace) then
      Canvas.Pen.Color := clBtnFace
    else Canvas.Pen.Color := clBtnShadow;
    for I := 0 to FRowCount - 1 do begin
      J := I - Ord(EmptyRowVisible);
      Canvas.Font.Color := Font.Color;
      Canvas.Brush.Color := Color;
      Selected := not FKeySelected and (I = 0) and not EmptyRowVisible;
      R.Top := I * TextHeight;
      R.Bottom := R.Top + TextHeight;
      if I  < FRecordCount + Ord(EmptyRowVisible) then begin
        if (I = 0) and (J = -1) then begin
          if ValueIsEmpty(FValue) then begin
            Canvas.Font.Color := clHighlightText;
            Canvas.Brush.Color := clHighlight;
            Selected := True;
          end
          else Canvas.Brush.Color := EmptyItemColor;
          R.Left := 0; R.Right := ClientWidth;
          Image := GetPicture(False, True, TextMargin);
          if TextMargin > 0 then begin
            with Bmp do begin
              Canvas.Font := Self.Canvas.Font;
              Canvas.Brush := Self.Canvas.Brush;
              Canvas.Pen := Self.Canvas.Pen;
              Width := WidthOf(R);
              Height := HeightOf(R);
            end;
            ImageRect := Bounds(0, 0, TextMargin, HeightOf(R));
            Bmp.Canvas.FillRect(ImageRect);
            if Image <> nil then DrawPicture(Bmp.Canvas, ImageRect, Image);
            DrawItemText(Bmp.Canvas, Bounds(TextMargin, 0, WidthOf(R) - TextMargin,
              HeightOf(R)), Selected, True);
            Canvas.Draw(R.Left, R.Top, Bmp);
          end
          else DrawItemText(Canvas, R, Selected, True);
        end
        else begin
          FLookupLink.ActiveRecord := J;
          if not ValueIsEmpty(FValue) and (FKeyField.AsString = FValue) then
          begin
            Canvas.Font.Color := clHighlightText;
            Canvas.Brush.Color := clHighlight;
            Selected := True;
          end;
          R.Left := 0; R.Right := ClientWidth;
          Image := GetPicture(False, False, TextMargin);
          if TextMargin > 0 then begin
            with Bmp do begin
              Canvas.Font := Self.Canvas.Font;
              Canvas.Brush := Self.Canvas.Brush;
              Canvas.Pen := Self.Canvas.Pen;
              Width := WidthOf(R);
              Height := HeightOf(R);
            end;
            ImageRect := Bounds(0, 0, TextMargin, HeightOf(R));
            Bmp.Canvas.FillRect(ImageRect);
            if Image <> nil then DrawPicture(Bmp.Canvas, ImageRect, Image);
            DrawItemText(Bmp.Canvas, Bounds(TextMargin, 0, WidthOf(R) - TextMargin,
              HeightOf(R)), Selected, False);
            Canvas.Draw(R.Left, R.Top, Bmp);
          end
          else DrawItemText(Canvas, R, Selected, False);
        end;
      end;
      R.Left := 0;
      R.Right := ClientWidth;
      if J >= FRecordCount then Canvas.FillRect(R);
      if Selected and (FFocused or FPopup) then Canvas.DrawFocusRect(R);
    end;
  finally
    Bmp.Free;
  end;
  if FRecordCount <> 0 then FLookupLink.ActiveRecord := FRecordIndex;
end;

procedure TRxDBLookupList.SelectCurrent;
begin
  FLockPosition := True;
  try
    if FSelectEmpty then begin
      ResetField;
    end
    else SelectKeyValue(FKeyField.AsString);
  finally
    FSelectEmpty := False;
    FLockPosition := False;
  end;
end;

procedure TRxDBLookupList.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if Y < 0 then Y := 0;
  if Y >= ClientHeight then Y := ClientHeight - 1;
  Delta := Y div GetTextHeight;
  if (Delta = 0) and EmptyRowVisible then begin
    FSelectEmpty := True;
  end
  else begin
    Delta := Delta - FRecordIndex;
    if EmptyRowVisible then Dec(Delta);
    FLookupLink.DataSet.MoveBy(Delta);
  end;
  SelectCurrent;
end;

procedure TRxDBLookupList.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd;
    if not (csReading in ComponentState) then begin
      Height := Height;
      RowCount := RowCount;
    end;
  end;
end;

procedure TRxDBLookupList.UpdateDisplayEmpty(const Value: string);
begin
  UpdateBufferCount(RowCount - Ord(Value <> EmptyStr));
end;

procedure TRxDBLookupList.UpdateBufferCount(Rows: Integer);
begin
  if FLookupLink.BufferCount <> Rows then begin
    FLookupLink.BufferCount := Rows;
    ListLinkDataChanged;
  end;
end;

procedure TRxDBLookupList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  BorderSize, TextHeight, Rows: Integer;
begin
  BorderSize := GetBorderSize;
  TextHeight := GetTextHeight;
  Rows := (AHeight - BorderSize) div TextHeight;
  if Rows < 1 then Rows := 1;
  FRowCount := Rows;
  UpdateBufferCount(Rows - Ord(EmptyRowVisible));
  if not (csReading in ComponentState) then
    AHeight := Rows * TextHeight + BorderSize;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TRxDBLookupList.SetRowCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 50 then Value := 50;
  Height := Value * GetTextHeight + GetBorderSize;
end;

procedure TRxDBLookupList.StopTimer;
begin
  if FTimerActive then begin
    KillTimer(Handle, 1);
    FTimerActive := False;
  end;
end;

procedure TRxDBLookupList.StopTracking;
begin
  if FTracking then begin
    StopTimer;
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRxDBLookupList.TimerScroll;
var
  Delta, Distance, Interval: Integer;
begin
  Delta := 0;
  Distance := 0;
  if FMousePos < 0 then begin
    Delta := -1;
    Distance := -FMousePos;
  end;
  if FMousePos >= ClientHeight then begin
    Delta := 1;
    Distance := FMousePos - ClientHeight + 1;
  end;
  if Delta = 0 then StopTimer
  else begin
    FLookupLink.DataSet.MoveBy(Delta);
    SelectCurrent;
    Interval := 200 - Distance * 15;
    if Interval < 0 then Interval := 0;
    SetTimer(Handle, 1, Interval, nil);
    FTimerActive := True;
  end;
end;

procedure TRxDBLookupList.UpdateScrollBar;
(*
{$IFDEF RX_D3}
var
  SIOld, SINew: TScrollInfo;
begin
  if FLookuplink.Active and HandleAllocated then begin
    with FLookuplink.DataSet do begin
      SIOld.cbSize := sizeof(SIOld);
      SIOld.fMask := SIF_ALL;
      GetScrollInfo(Self.Handle, SB_VERT, SIOld);
      SINew := SIOld;
      if IsSequenced then begin
        SINew.nMin := 1;
        SINew.nPage := Self.FRowCount - Ord(EmptyRowVisible);
        SINew.nMax := RecordCount + SINew.nPage - 1;
        if State in [dsInactive, dsBrowse, dsEdit] then
          SINew.nPos := RecNo;
      end
      else begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        if Self.FRecordCount = (FRowCount - Ord(EmptyRowVisible)) then begin
          SINew.nMax := 4;
          if BOF then SINew.nPos := 0
          else if EOF then SINew.nPos := 4
          else SINew.nPos := 2;
        end
        else begin
          SINew.nMax := 0;
          SINew.nPos := 0;
        end;
      end;
      if (SINew.nMin <> SIOld.nMin) or (SINew.nMax <> SIOld.nMax) or
        (SINew.nPage <> SIOld.nPage) or (SINew.nPos <> SIOld.nPos) then
        SetScrollInfo(Self.Handle, SB_VERT, SINew, True);
    end;
  end
  else begin
    SetScrollRange(Handle, SB_VERT, 0, 0, False);
    SetScrollPos(Handle, SB_VERT, 0, True);
  end;
end;
{$ELSE}
*)
var
  Pos, Max: Integer;
  CurPos, MaxPos: Integer;
begin
  if FLookupLink.Active then begin
    Pos := 0;
    Max := 0;
    if FRecordCount >= (FRowCount - Ord(EmptyRowVisible)) then begin //change "=" to ">="
      Max := 4;
      if not FLookupLink.DataSet.BOF then
        if not FLookupLink.DataSet.EOF then Pos := 2 else Pos := 4;
    end;
    GetScrollRange(Handle, SB_VERT, CurPos, MaxPos);
    if MaxPos = 0 then MaxPos := FRecordCount;
    CurPos := GetScrollPos(Handle, SB_VERT);
    if Max <> MaxPos then SetScrollRange(Handle, SB_VERT, 0, Max, True {False});
    if CurPos <> Pos then SetScrollPos(Handle, SB_VERT, Pos, True);
  end
  else begin
    SetScrollRange(Handle, SB_VERT, 0, 0, False);
    SetScrollPos(Handle, SB_VERT, 0, True);
  end;
end;

procedure TRxDBLookupList.CMCtl3DChanged(var Message: TMessage);
begin
{$IFNDEF VER80}
  if NewStyleControls and (FBorderStyle = bsSingle) then begin
    RecreateWnd;
    if not (csReading in ComponentState) then RowCount := RowCount;
  end;
  inherited;
{$ELSE}
  inherited;
  Invalidate;
  if not (csReading in ComponentState) then RowCount := RowCount;
{$ENDIF}
end;

procedure TRxDBLookupList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not (csReading in ComponentState) then Height := Height;
end;

procedure TRxDBLookupList.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TRxDBLookupList.WMTimer(var Message: TMessage);
begin
  TimerScroll;
end;

procedure TRxDBLookupList.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  if csDesigning in ComponentState then begin
    if FLookupLink.Active then DefaultHandler(Msg)
    else inherited;
  end
  else inherited;
end;

{$IFDEF RX_D4}
function TRxDBLookupList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if not Result then begin
    with FLookupLink.DataSet do
      Result := MoveBy(FRecordCount - FRecordIndex) <> 0;
  end;
end;

function TRxDBLookupList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if not Result then begin
    with FLookupLink.DataSet do
      Result := MoveBy(-FRecordIndex - 1) <> 0;
  end;
end;
{$ENDIF RX_D4}

procedure TRxDBLookupList.WMVScroll(var Message: TWMVScroll);
begin
  FSearchText := '';
  with Message, FLookupLink.DataSet do
    case ScrollCode of
      SB_LINEUP: MoveBy(-FRecordIndex - 1);
      SB_LINEDOWN: //MoveBy(FRecordCount - FRecordIndex);
       begin // improvement by SIDNEI
         if FRecordIndex = 0 then
           MoveBy (+ RowCount)
         else
           MoveBy (+1);
       end;
      SB_PAGEUP: MoveBy(-FRecordIndex - FRecordCount + 1);
      SB_PAGEDOWN: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
      SB_THUMBPOSITION:
        begin
          case Pos of
            0: First;
            1: MoveBy(-FRecordIndex - FRecordCount + 1);
            2: Exit;
            3: MoveBy(FRecordCount - FRecordIndex + FRecordCount - 2);
            4: Last;
          end;
        end;
      SB_BOTTOM: Last;
      SB_TOP: First;
    end;
end;

{ TRxPopupDataList }

constructor TRxPopupDataList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TRxLookupControl then FCombo := TRxLookupControl(AOwner);
{$IFNDEF VER80}
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
{$ELSE}
  ControlStyle := [csOpaque];
{$ENDIF}
  FPopup := True;
  TabStop := False;
  ParentCtl3D := False;
  Ctl3D := False;
end;

procedure TRxPopupDataList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := WS_POPUP or WS_BORDER;
{$IFNDEF VER80}
    ExStyle := WS_EX_TOOLWINDOW;
{$ENDIF}
{$IFDEF RX_D4}
    AddBiDiModeExStyle(ExStyle);
{$ENDIF}
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

{$IFDEF VER80}
procedure TRxPopupDataList.CreateWnd;
begin
  inherited CreateWnd;
  if (csDesigning in ComponentState) then SetParent(nil);
end;
{$ENDIF}

procedure TRxPopupDataList.WMMouseActivate(var Message: TMessage);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TRxPopupDataList.Click;
begin
  inherited Click;
  if Assigned(FCombo) and TRxDBLookupCombo(FCombo).FListVisible then
    TRxDBLookupCombo(FCombo).InvalidateText;
end;

procedure TRxPopupDataList.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Assigned(FCombo) and TRxDBLookupCombo(FCombo).FListVisible then
    TRxDBLookupCombo(FCombo).InvalidateText;
end;

{ TRxDBLookupCombo }

constructor TRxDBLookupCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFNDEF VER80}
  ControlStyle := ControlStyle + [csReplicatable] - [csSetCaption];
{$ELSE}
  ControlStyle := [csFramed, csOpaque];
{$ENDIF}
  Width := 145;
  Height := 0;
  FDataList := TRxPopupDataList.Create(Self);
  FDataList.Visible := False;
  FDataList.Parent := Self;
  FDataList.OnMouseUp := ListMouseUp;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FDropDownCount := 8;
  FDisplayValues := TStringList.Create;
  FSelImage := TPicture.Create;
{$IFDEF VER80}
  FBtnGlyph := TBitmap.Create;
  { Load ComboBox button glyph }
  FBtnGlyph.Handle := LoadBitmap(0, PChar(32738));
  FBtnDisabled := CreateDisabledBitmap(FBtnGlyph, clBlack);
{$ENDIF}
  Height := {GetMinHeight}21;
  FIgnoreCase := True;
  FEscapeClear := True;
end;

destructor TRxDBLookupCombo.Destroy;
begin
{$IFDEF VER80}
  FBtnDisabled.Free;
  FBtnGlyph.Free;
{$ENDIF}
  FSelImage.Free;
  FSelImage := nil;
  FDisplayValues.Free;
  FDisplayValues := nil;
  inherited Destroy;
end;

procedure TRxDBLookupCombo.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
{$IFNDEF VER80}
    if NewStyleControls and Ctl3D then ExStyle := ExStyle or WS_EX_CLIENTEDGE
    else Style := Style or WS_BORDER;
{$ELSE}
    Style := Style or WS_BORDER;
{$ENDIF}
end;

procedure TRxDBLookupCombo.CloseUp(Accept: Boolean);
var
  ListValue: string;
begin
  if FListVisible then begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    ListValue := FDataList.Value;
    SetWindowPos(FDataList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    FDataList.LookupSource := nil;
    Invalidate;
    FSearchText := '';
    FDataList.FSearchText := '';
    if Accept and CanModify and (Value <> ListValue) then
      SelectKeyValue(ListValue);
    if CanFocus then SetFocus;
    if Assigned(FOnCloseUp) then FOnCloseUp(Self);
  end;
end;

procedure TRxDBLookupCombo.DropDown;
var
  P: TPoint;
  I, Y: Integer;
  S: string;
begin
  if not FListVisible and {FListActive} CanModify then begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    FDataList.Color := Color;
    FDataList.Font := Font;
    FDataList.ItemHeight := ItemHeight;
    FDataList.ReadOnly := not CanModify;
    FDataList.EmptyValue := EmptyValue;
    FDataList.DisplayEmpty := DisplayEmpty;
    FDataList.EmptyItemColor := EmptyItemColor;
    FDataList.RowCount := FDropDownCount;
    FDataList.LookupField := FLookupFieldName;
    FDataList.ListStyle := FListStyle;
    FDataList.FieldsDelimiter := FFieldsDelim;
    FDataList.IgnoreCase := FIgnoreCase;
    FDataList.IndexSwitch := FIndexSwitch;
    FDataList.OnGetImage := OnGetImage;
//polaris    if FDisplayField <> nil then FAlignment := FDisplayField.Alignment;
    S := '';
    for I := 0 to FListFields.Count - 1 do
      S := S + TField(FListFields[I]).FieldName + ';';
    FDataList.LookupDisplay := S;
    FDataList.LookupDisplayIndex := FListFields.IndexOf(FDisplayField);
    {FDataList.FLockPosition := True;}
    try
      FDataList.LookupSource := FLookupLink.DataSource;
    finally
      {FDataList.FLockPosition := False;}
    end;
    FDataList.SetValueKey(Value);
    {FDataList.KeyValueChanged;}
    if FDropDownWidth > 0 then
      FDataList.Width := FDropDownWidth
    else if FDropDownWidth < 0 then
      FDataList.Width := Max(Width, FDataList.GetWindowWidth)
    else FDataList.Width := Width;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + FDataList.Height > Screen.Height then
      Y := P.Y - FDataList.Height;
    case FDropDownAlign of
      daRight: Dec(P.X, FDataList.Width - Width);
      daCenter: Dec(P.X, (FDataList.Width - Width) div 2);
    end;
    if P.X + FDataList.Width > Screen.Width then
      P.X := Screen.Width - FDataList.Width;
    SetWindowPos(FDataList.Handle, HWND_TOP, Max(P.X, 0), Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    InvalidateText;
    Repaint;
  end;
end;

function TRxDBLookupCombo.GetMinHeight: Integer;
begin
  Result := DefaultTextHeight + GetBorderSize + 3;
end;

procedure TRxDBLookupCombo.UpdateFieldText;
var
  I: Integer;
  S: string;
begin
  if FDisplayValues <> nil then FDisplayValues.Clear;
  if DisplayAllFields then begin
    S := '';
    for I := 0 to FListFields.Count - 1 do begin
      if S <> '' then S := S + FFieldsDelim + ' ';
      S := S + TField(FListFields[I]).DisplayText;
      if (ListStyle = lsFixed) and Assigned(FDisplayValues) then begin
        with TField(FListFields[I]) do
          FDisplayValues.AddObject(DisplayText,
            TObject(MakeLong(DisplayWidth, Ord(Alignment))));
      end;
    end;
    if S = '' then S := FDisplayField.DisplayText;
    inherited Text := S;
  end
  else inherited Text := FDisplayField.DisplayText;
  FAlignment := FDisplayField.Alignment;
end;

function TRxDBLookupCombo.GetDisplayValues(Index: Integer): string; 
begin
  if Assigned(FDisplayValues) and (FDisplayValues.Count > Index) then
    Result := FDisplayValues[Index]
  else  
    Result := FDisplayValue;
end;

function TRxDBLookupCombo.GetText: string;
begin
  Result := inherited Text;
end;

procedure TRxDBLookupCombo.InvalidateText;
var
  R: TRect;
begin
  SetRect(R, 1, 1, ClientWidth - FButtonWidth - 1, ClientHeight - 1);
{$IFDEF VER80}
  InflateRect(R, -1, -1);
{$ENDIF}
  InvalidateRect(Self.Handle, @R, False);
  UpdateWindow(Self.Handle);
end;

procedure TRxDBLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  if FListActive and ((Key = VK_UP) or (Key = VK_DOWN)) then
    if ssAlt in Shift then begin
      if FListVisible then CloseUp(True) else DropDown;
      Key := 0;
    end
    else if (not FListVisible) and (not ReadOnly) then begin
      if not LocateKey then FLookupLink.DataSet.First
      else begin
        if Key = VK_UP then Delta := -1 else Delta := 1;
        FLookupLink.DataSet.MoveBy(Delta);
      end;
      SelectKeyValue(FKeyField.AsString);
      Key := 0;
    end;
  if (Key <> 0) and FListVisible then FDataList.KeyDown(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TRxDBLookupCombo.KeyPress(var Key: Char);
begin
  if FListVisible then begin
    if CharInSet(Key, [#13, #27]) then begin
      CloseUp(Key = #13);
      Key := #0;
    end
    else FDataList.KeyPress(Key)
  end
  else begin
    if Key >= ' ' then begin
      DropDown;
      if FListVisible then FDataList.KeyPress(Key);
    end
    else if (Key = #27) and FEscapeClear and (not ValueIsEmpty(FValue)) and
      CanModify then
    begin
      ResetField;
//      Key := #0;
      if FValue = FEmptyValue then Key := #0;   // Polaris
    end;
  end;
  inherited KeyPress(Key);
  if CharInSet(Key, [#13, #27]) then
    GetParentForm(Self).Perform(CM_DIALOGKEY, WPARAM(Key), 0);
end;

procedure TRxDBLookupCombo.DisplayValueChanged;
begin
  if FListActive and LocateDisplay then begin
    FValue := FKeyField.AsString;
    UpdateFieldText;
  end
  else begin
    FValue := FEmptyValue;
    inherited Text := DisplayEmpty;
    if FDisplayValues <> nil then FDisplayValues.Clear;
    FAlignment := taLeftJustify;
  end;
  UpdateDisplayValue;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TRxDBLookupCombo.KeyValueChanged;
begin
{$IFNDEF VER80}
  if FLookupMode then begin
    if FDisplayValues <> nil then FDisplayValues.Clear;
    if FDataLink.Active and (FDataField <> nil) then {begin
      inherited Text := FDataField.DisplayText;
      FAlignment := FDataField.Alignment;
    end}
      if ValueIsEmpty(FValue) then begin  // Polaris
        inherited Text := DisplayEmpty;
        FAlignment := taLeftJustify;
      end else begin
        inherited Text := FDataField.DisplayText;
        FAlignment := FDataField.Alignment;
      end
    else inherited Text := '';
  end else
{$ENDIF}
  if FListActive and LocateKey then
    UpdateFieldText
  else if FListActive then begin
    FValue := FEmptyValue;
    inherited Text := DisplayEmpty;
    if FDisplayValues <> nil then FDisplayValues.Clear;
    FAlignment := taLeftJustify;
  end
  else begin
    if (csDesigning in ComponentState) then  // Polaris
      inherited Text := DisplayEmpty         // Polaris
    else                                     // Polaris
    inherited Text := '';
    if FDisplayValues <> nil then FDisplayValues.Clear;
  end;
  UpdateDisplayValue;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TRxDBLookupCombo.SetFieldsDelim(Value: Char);
begin
  if (FFieldsDelim <> Value) then begin
    inherited SetFieldsDelim(Value);
    if (ListStyle = lsDelimited) and DisplayAllFields and
      not (csReading in ComponentState) then KeyValueChanged;
  end;
end;

procedure TRxDBLookupCombo.SetListStyle(Value: TLookupListStyle);
begin
  if (FListStyle <> Value) then begin
    FListStyle := Value;
    if DisplayAllFields and not (csReading in ComponentState) then
      KeyValueChanged;
  end;
end;

function TRxDBLookupCombo.GetDisplayAll: Boolean;
begin
{$IFNDEF VER80}
  if FLookupMode then Result := False else
{$ENDIF}
  Result := FDisplayAll;
end;

procedure TRxDBLookupCombo.SetDisplayAll(Value: Boolean);
begin
  if FDisplayAll <> Value then begin
{$IFNDEF VER80}
    if FLookupMode then FDisplayAll := False else
{$ENDIF}
    FDisplayAll := Value;
    if not (csReading in ComponentState)
      {$IFNDEF VER80} and not FLookupMode {$ENDIF} then
      KeyValueChanged
    else Invalidate;
  end;
end;

procedure TRxDBLookupCombo.ListLinkDataChanged;
begin
  if FDataLink.Active and FDataLink.DataSet.IsLinkedTo(LookupSource) then
    if FListActive then DataLinkRecordChanged(nil);
end;

procedure TRxDBLookupCombo.ListLinkActiveChanged;
begin
  inherited ListLinkActiveChanged;
  if FListActive and Assigned(FMasterField) then UpdateKeyValue
  else KeyValueChanged;
end;

procedure TRxDBLookupCombo.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(FDataList.ClientRect, Point(X, Y)));
end;

procedure TRxDBLookupCombo.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then begin
    if CanFocus then SetFocus;
    if not FFocused then Exit;
    if FListVisible then CloseUp(False)
    else if {FListActive} CanModify then begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TRxDBLookupCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then begin
    TrackButton(X, Y);
    if FListVisible then begin
      ListPos := FDataList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FDataList.ClientRect, ListPos) then begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(FDataList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TRxDBLookupCombo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  StopTracking;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TRxDBLookupCombo.UpdateCurrentImage;
begin
  FSelImage.Assign(nil);
  FSelMargin := 0;
  FSelImage.Graphic := inherited GetPicture(False, ValueIsEmpty(Value),
    FSelMargin);
end;

function TRxDBLookupCombo.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  if Current then begin
    TextMargin := 0;
    Result := nil;
    if (FSelImage <> nil) and (FSelImage.Graphic <> nil) and
      not FSelImage.Graphic.Empty then
    begin
      Result := FSelImage.Graphic;
      TextMargin := FSelMargin;
    end;
  end
  else Result := inherited GetPicture(Current, Empty, TextMargin);
end;

procedure TRxDBLookupCombo.PaintDisplayValues(Canvas: TCanvas; R: TRect;
  ALeft: Integer);
var
  I, LastIndex, TxtWidth: Integer;
  X, W, ATop, ARight: Integer;
  S: string;
begin
  if ColorToRGB(Self.Color) <> ColorToRGB(clBtnFace) then
    Canvas.Pen.Color := clBtnFace
  else Canvas.Pen.Color := clBtnShadow;
  LastIndex := FDisplayValues.Count - 1;
  TxtWidth := Canvas.TextWidth('M');
  ATop := Max(0, (HeightOf(R) - Canvas.TextHeight('Xy')) div 2);
  ARight := R.Right;
  Inc(R.Left, ALeft);
  for I := 0 to LastIndex do begin
    S := FDisplayValues[I];
    W := LoWord(Longint(FDisplayValues.Objects[I]));
    if I < LastIndex then W := W * TxtWidth + 4
    else W := ARight - R.Left;
    X := 2;   
    R.Right := R.Left + W;
    case TAlignment(HiWord(Longint(FDisplayValues.Objects[I]))) of
      taRightJustify: X := W - Canvas.TextWidth(S) - 3;
      taCenter: X := (W - Canvas.TextWidth(S)) div 2;
    end;
    Canvas.TextRect(R, R.Left + Max(0, X), ATop, S);
    Inc(R.Left, W);
    if I < LastIndex then begin
      Canvas.MoveTo(R.Right, R.Top);
      Canvas.LineTo(R.Right, R.Bottom);
      Inc(R.Left);
    end;
    if R.Left >= ARight then Break;
  end;
end;

procedure TRxDBLookupCombo.Paint;
const
  TransColor: array[Boolean] of TColor = (clBtnFace, clWhite);
var
  W, X, Flags, TextMargin: Integer;
  AText: string;
  Selected, DrawList, IsEmpty: Boolean;
  R, ImageRect: TRect;
  Image: TGraphic;
  Bmp: TBitmap;
  Alignment: TAlignment;
{$IFDEF VER80}
  Target: TRect;
{$ENDIF}
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  Selected := FFocused and not FListVisible {$IFNDEF VER80} and
    not (csPaintCopy in ControlState) {$ENDIF};
  if Selected then begin
    Canvas.Font.Color := clHighlightText;
    Canvas.Brush.Color := clHighlight;
  end
  else if not Enabled and NewStyleControls then
    Canvas.Font.Color := clGrayText;
  AText := inherited Text;
  Alignment := FAlignment;
  Image := nil;
  IsEmpty := False;
  DrawList := DisplayAllFields;
{$IFNDEF VER80}
  if (csPaintCopy in ControlState) and (FDataField <> nil) then begin
    DrawList := False;
    AText := FDataField.DisplayText;
    Alignment := FDataField.Alignment;
  end;
{$ENDIF}
  TextMargin := 0;
  if FListVisible then begin
    DrawList := False;
    if FDataList.FSearchText <> '' then begin
      AText := FDataList.FSearchText;
    end
    else begin
      if FDataList.ValueIsEmpty(FDataList.Value) then begin
        AText := DisplayEmpty;
        IsEmpty := True;
        Image := GetPicture(False, True, TextMargin);
      end
      else if (FDataList.FKeyField.AsString = FDataList.Value) then begin
        AText := FDataList.FDisplayField.DisplayText;
        Image := FDataList.GetPicture(False, False, TextMargin);
      end
      else begin
        Image := GetPicture(True, False, TextMargin);
      end;
    end;
  end
  else begin
{$IFNDEF VER80}
    if (csPaintCopy in ControlState) then Image := nil else
{$ENDIF}
    begin
      IsEmpty := ValueIsEmpty(Value);
      Image := GetPicture(True, IsEmpty, TextMargin);
    end;
  end;
{$IFDEF RX_D4}
  if UseRightToLeftAlignment then ChangeBiDiModeAlignment(Alignment);
{$ENDIF}
  W := ClientWidth - FButtonWidth;
  if W > 4 then begin
    SetRect(R, 1, 1, W - 1, ClientHeight - 1);
{$IFDEF VER80}
    InflateRect(R, -1, -1);
{$ENDIF}
    if TextMargin > 0 then Inc(TextMargin);
    X := 0{2} + TextMargin;  // Polaris
    if not (FListVisible and (FDataList.FSearchText <> '')) and not DrawList then
      case Alignment of
        taRightJustify: X := W - Canvas.TextWidth(AText) - 6;
        taCenter: X := (W + TextMargin - Canvas.TextWidth(AText)) div 2;
      end;
    Bmp := TBitmap.Create;
    try
      with Bmp.Canvas do begin
        Font := Self.Canvas.Font;
        Brush := Self.Canvas.Brush;
        Pen := Self.Canvas.Pen;
      end;
{$IFDEF RX_D4}
      if (BiDiMode = bdRightToLeft) then begin
        Inc(X, FButtonWidth);
        Inc(R.Left, FButtonWidth);
        R.Right := ClientWidth;
      end;
      if SysLocale.MiddleEast then begin
        TControlCanvas(Self.Canvas).UpdateTextFlags;
        Bmp.Canvas.TextFlags := Self.Canvas.TextFlags;
      end;
{$ENDIF}
      Bmp.Width := WidthOf(R);
      Bmp.Height := HeightOf(R);
      ImageRect := Rect(0, 0, WidthOf(R), HeightOf(R));
      if DrawList and (ListStyle = lsFixed) and (FDisplayValues <> nil) and
        (FDisplayValues.Count > 0) then
      begin
        if IsEmpty then begin
          AText := DisplayEmpty;
          Bmp.Canvas.TextRect(ImageRect, X, Max(0, (HeightOf(R) -
            Canvas.TextHeight(AText)) div 2), AText);
        end
        else PaintDisplayValues(Bmp.Canvas, ImageRect, TextMargin);
      end
      else begin
        Bmp.Canvas.TextRect(ImageRect, X, Max(0, (HeightOf(R) -
          Canvas.TextHeight(AText)) div 2), AText);
      end;
      if Image <> nil then begin
        ImageRect.Right := ImageRect.Left + TextMargin + 2;
        DrawPicture(Bmp.Canvas, ImageRect, Image);
      end;
      Canvas.Draw(R.Left, R.Top, Bmp);
    finally
      Bmp.Free;
    end;
    if Selected then Canvas.DrawFocusRect(R);
  end;
  SetRect(R, W, 0, ClientWidth, ClientHeight);
{$IFDEF RX_D4}
  if (BiDiMode = bdRightToLeft) then begin
    R.Left := 0;
    R.Right:= FButtonWidth;
  end;
{$ENDIF}
{$IFNDEF VER80}
  if (not FListActive) or (not Enabled) or ReadOnly then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE
  else if FPressed then
    Flags := DFCS_SCROLLCOMBOBOX or DFCS_FLAT or DFCS_PUSHED
  else
    Flags := DFCS_SCROLLCOMBOBOX;
  DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, Flags);
{$ELSE}
  if NewStyleControls then begin
    InflateRect(R, -1, -1); Dec(R.Left);
  end
  else begin
    InflateRect(R, 1, 1); Inc(R.Left);
  end;
  R := DrawButtonFace(Canvas, R, 1, bsWin31, False, FPressed, False);
  { draw button glyph }
  if (not FListActive) or (not Enabled) or ReadOnly then
    Bmp := FBtnDisabled
  else
    Bmp := FBtnGlyph;
  Target := Bounds(R.Left, R.Top, Bmp.Width, Bmp.Height);
  OffsetRect(Target, ((R.Right - R.Left) div 2) - (Bmp.Width div 2),
    ((R.Bottom - R.Top) div 2) - (Bmp.Height div 2));
  { Canvas.Draw(Target.Left, Target.Top, Bmp); }
  DrawBitmapTransparent(Canvas, Target.Left, Target.Top, Bmp,
    TransColor[Bmp = FBtnGlyph]);
{$ENDIF}
end;

procedure TRxDBLookupCombo.ResetField;
begin
  if FListVisible then CloseUp(False);
  inherited ResetField;
  UpdateCurrentImage;
  Invalidate;
end;

procedure TRxDBLookupCombo.StopTracking;
begin
  if FTracking then begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TRxDBLookupCombo.TrackButton(X, Y: Integer);
var
  NewState: Boolean;
begin
  NewState := PtInRect(Rect(ClientWidth - FButtonWidth, 0, ClientWidth,
    ClientHeight), Point(X, Y));
  if FPressed <> NewState then begin
    FPressed := NewState;
    Repaint;
  end;
end;

procedure TRxDBLookupCombo.UpdateDisplayEmpty(const Value: string);
begin
  if Text = FDisplayEmpty then inherited Text := Value;
end;

procedure TRxDBLookupCombo.Click;
begin
  inherited Click;
  Change;
end;

procedure TRxDBLookupCombo.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FDataList) then
    CloseUp(False);
end;

{$IFNDEF VER80}
procedure TRxDBLookupCombo.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then begin
    RecreateWnd;
    if not (csReading in ComponentState) and (Height < GetMinHeight) then
      Height := GetMinHeight;
  end;
  inherited;
end;

procedure TRxDBLookupCombo.CNKeyDown(var Message: TWMKeyDown);
begin
  if not (csDesigning in ComponentState) then
    if (Message.CharCode in [VK_RETURN, VK_ESCAPE]) and FListVisible and
      FLookupMode and FDataLink.DataSourceFixed then
    begin
      CloseUp(Message.CharCode = VK_RETURN);
      Message.Result := 1;
      Exit;
    end;
  inherited;
end;
{$ENDIF}

procedure TRxDBLookupCombo.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not (csReading in ComponentState) then
    Height := Max(Height, GetMinHeight);
end;

procedure TRxDBLookupCombo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

{$IFNDEF VER80}
procedure TRxDBLookupCombo.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;
{$ENDIF}

procedure TRxDBLookupCombo.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TRxDBLookupCombo.WMGetDlgCode(var Message: TMessage);
begin
  inherited;
  Message.Result := DLGC_BUTTON or DLGC_WANTALLKEYS or DLGC_WANTARROWS
    or DLGC_WANTCHARS;
end;

procedure TRxDBLookupCombo.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp(False);
end;

procedure TRxDBLookupCombo.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  with ClientRect do
    if PtInRect(Bounds(Right - FButtonWidth, Top, FButtonWidth, Bottom - Top),
      ScreenToClient(P)) then
{$IFNDEF VER80}
      Windows.SetCursor(LoadCursor(0, IDC_ARROW))
{$ELSE}
      WinProcs.SetCursor(LoadCursor(0, IDC_ARROW))
{$ENDIF}
    else inherited;
end;

procedure TRxDBLookupCombo.WMSize(var Message: TWMSize);
begin
  inherited;
  if not (csReading in ComponentState) and (Height < GetMinHeight) then
    Height := GetMinHeight
  else begin
    if (csDesigning in ComponentState) then
      FDataList.SetBounds(0, Height + 1, 10, 10);
  end;
end;

{$IFDEF RX_D4}
procedure TRxDBLookupCombo.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  FDataList.BiDiMode := BiDiMode;
end;
{$ENDIF}

{ TPopupDataWindow }

constructor TPopupDataWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditor := TWinControl(AOwner);
  Visible := False;
  Parent := FEditor;
  OnMouseUp := PopupMouseUp;
end;

procedure TPopupDataWindow.InvalidateEditor;
var
  R: TRect;
begin
  if (FEditor is TCustomComboEdit) then begin
    with TComboEdit(FEditor) do
      SetRect(R, 0, 0, ClientWidth - Button.Width - 2, ClientHeight + 1);
  end
  else R := FEditor.ClientRect;
  InvalidateRect(FEditor.Handle, @R, False);
  UpdateWindow(FEditor.Handle);
end;

procedure TPopupDataWindow.Click;
begin
  inherited Click;
  if Value <> '' then
    with TRxLookupEdit(FEditor) do begin
      if not (FChanging or ReadOnly) then begin
        FChanging := True;
        try
          Text := Self.DisplayValue;
          if AutoSelect then SelectAll;
        finally
          FChanging := False;
        end;
      end;
    end;
  InvalidateEditor;
end;

procedure TPopupDataWindow.DisplayValueChanged;
begin
  if not FLockPosition then
    if FListActive then begin
      if LocateDisplay then
        FValue := FKeyField.AsString
      else begin
        FLookupLink.DataSet.First;
        FValue := EmptyValue;
      end;
    end
    else FValue := FEmptyValue;
end;

procedure TPopupDataWindow.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  InvalidateEditor;
end;

procedure TPopupDataWindow.PopupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then CloseUp(PtInRect(Self.ClientRect, Point(X, Y)));
end;

procedure TPopupDataWindow.CloseUp(Accept: Boolean);
begin
  if Assigned(FCloseUp) then FCloseUp(Self, Accept);
end;

function TPopupDataWindow.GetPicture(Current, Empty: Boolean;
  var TextMargin: Integer): TGraphic;
begin
  TextMargin := 0;
  Result := nil;
  if Assigned(FOnGetImage) then FOnGetImage(FEditor, Empty, Result, TextMargin);
end;

procedure TPopupDataWindow.Hide;
begin
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
    SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  Visible := False;
end;

procedure TPopupDataWindow.Show(Origin: TPoint);
begin
  SetWindowPos(Handle, HWND_TOP, Origin.X, Origin.Y, 0, 0,
    SWP_NOACTIVATE or SWP_SHOWWINDOW or SWP_NOSIZE);
  Visible := True;
end;

{ TRxLookupEdit }

constructor TRxLookupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount := 8;
  FPopupOnlyLocate := True;
  ControlState := ControlState + [csCreating];
  try
    FPopup := TPopupDataWindow.Create(Self);
    TPopupDataWindow(FPopup).OnCloseUp := PopupCloseUp;
    GlyphKind := gkDropDown; { force update }
  finally
    ControlState := ControlState - [csCreating];
  end;
end;

destructor TRxLookupEdit.Destroy;
begin
  if FPopup <> nil then
    with TPopupDataWindow(FPopup) do begin
      OnCloseUp := nil;
      OnGetImage := nil;
    end;
  FPopup.Free;
  FPopup := nil;
  inherited Destroy;
end;

procedure TRxLookupEdit.SetDropDownCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 50 then Value := 50;
  FDropDownCount := Value;
end;

function TRxLookupEdit.GetListStyle: TLookupListStyle;
begin
  Result := TPopupDataWindow(FPopup).ListStyle;
end;

procedure TRxLookupEdit.SetListStyle(Value: TLookupListStyle);
begin
  TPopupDataWindow(FPopup).ListStyle := Value;
end;

function TRxLookupEdit.GetFieldsDelim: Char;
begin
  Result := TPopupDataWindow(FPopup).FieldsDelimiter;
end;

procedure TRxLookupEdit.SetFieldsDelim(Value: Char);
begin
  TPopupDataWindow(FPopup).FieldsDelimiter := Value;
end;

function TRxLookupEdit.GetLookupDisplay: string;
begin
  Result := TPopupDataWindow(FPopup).LookupDisplay;
end;

procedure TRxLookupEdit.SetLookupDisplay(const Value: string);
begin
  TPopupDataWindow(FPopup).LookupDisplay := Value;
end;

function TRxLookupEdit.GetDisplayIndex: Integer;
begin
  Result := TPopupDataWindow(FPopup).LookupDisplayIndex;
end;

procedure TRxLookupEdit.SetDisplayIndex(Value: Integer);
begin
  TPopupDataWindow(FPopup).LookupDisplayIndex := Value;
end;

function TRxLookupEdit.GetLookupField: string;
begin
  Result := TPopupDataWindow(FPopup).LookupField;
end;

procedure TRxLookupEdit.SetLookupField(const Value: string);
begin
  TPopupDataWindow(FPopup).LookupField := Value;
end;

function TRxLookupEdit.GetLookupSource: TDataSource;
begin
  Result := TPopupDataWindow(FPopup).LookupSource;
end;

procedure TRxLookupEdit.SetLookupSource(Value: TDataSource);
begin
  TPopupDataWindow(FPopup).LookupSource := Value;
end;

function TRxLookupEdit.GetOnGetImage: TGetImageEvent;
begin
  Result := TPopupDataWindow(FPopup).OnGetImage;
end;

procedure TRxLookupEdit.SetOnGetImage(Value: TGetImageEvent);
begin
  TPopupDataWindow(FPopup).OnGetImage := Value;
end;

function TRxLookupEdit.GetLookupValue: string;
begin
  TPopupDataWindow(FPopup).DisplayValue := Text;
  Result := TPopupDataWindow(FPopup).Value;
end;

procedure TRxLookupEdit.SetLookupValue(const Value: string);
begin
  TPopupDataWindow(FPopup).Value := Value;
  Text := TPopupDataWindow(FPopup).DisplayValue;
end;

procedure TRxLookupEdit.ShowPopup(Origin: TPoint);
begin
  TPopupDataWindow(FPopup).Show(Origin);
end;

procedure TRxLookupEdit.HidePopup;
begin
  TPopupDataWindow(FPopup).Hide;
end;

procedure TRxLookupEdit.PopupDropDown(DisableEdit: Boolean);
begin
  if not (ReadOnly or PopupVisible) then begin
    if Assigned(FOnDropDown) then FOnDropDown(Self);
    with TPopupDataWindow(FPopup) do begin
      Color := Self.Color;
      Font := Self.Font;
      if FDropDownWidth > 0 then
        Width := FDropDownWidth
      else if FDropDownWidth < 0 then
        Width := Max(Self.Width, GetWindowWidth)
      else Width := Self.Width;
      ReadOnly := Self.ReadOnly;
      RowCount := FDropDownCount;
    end;
  end;
  inherited PopupDropDown(False);
end;

procedure TRxLookupEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP, VK_DOWN]) and
    PopupVisible then
  begin
    TPopupDataWindow(FPopup).KeyDown(Key, Shift);
    Key := 0;
  end;
  inherited KeyDown(Key, Shift);
  FIgnoreChange := (SelLength > 0) or (Key = VK_BACK);
  if not (PopupVisible or ReadOnly) and (Key in [VK_UP, VK_DOWN]) and
    (Shift = []) then
  begin
    with TPopupDataWindow(FPopup) do begin
      KeyDown(Key, Shift);
      if Value <> EmptyValue then Key := 0;
    end;
  end;
end;

procedure TRxLookupEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  FIgnoreChange := (SelLength > 0) or (Key = Char(VK_BACK));
end;

procedure TRxLookupEdit.Change;
begin
  if PopupOnlyLocate or PopupVisible then
    inherited Change
  else begin
    PopupChange;
    DoChange;
  end;
end;

procedure TRxLookupEdit.PopupChange;
var
  S: string;
  Len: Integer;
begin
  if FChanging or FIgnoreChange or ReadOnly then begin
    FIgnoreChange := False;
    Exit;
  end;
  FChanging := True;
  try
    S := Text;
    if TPopupDataWindow(FPopup).SearchText(S) then begin
      Len := Length(Text);
      Text := TPopupDataWindow(FPopup).DisplayValue;
      SelStart := Len;
      SelLength := Length(Text) - Len;
    end
    else with TPopupDataWindow(FPopup) do Value := EmptyValue;
  finally
    FChanging := False;
  end;
end;

{$IFNDEF VER80}
procedure TRxLookupEdit.SetPopupValue(const Value: Variant);
{$ELSE}
procedure TRxLookupEdit.SetPopupValue(const Value: string);
{$ENDIF}
begin
{$IFNDEF VER80}
  if VarIsNull(Value) or VarIsEmpty(Value) then
    TPopupDataWindow(FPopup).Value := TPopupDataWindow(FPopup).EmptyValue
  else
{$ENDIF}
    TPopupDataWindow(FPopup).DisplayValue := Value;
end;

{$IFNDEF VER80}
function TRxLookupEdit.GetPopupValue: Variant;
{$ELSE}
function TRxLookupEdit.GetPopupValue: string;
{$ENDIF}
begin
  with TPopupDataWindow(FPopup) do
    if Value <> EmptyValue then Result := DisplayValue
    else Result := Self.Text;
end;

{$IFNDEF VER80}
function TRxLookupEdit.AcceptPopup(var Value: Variant): Boolean;
{$ELSE}
function TRxLookupEdit.AcceptPopup(var Value: string): Boolean;
{$ENDIF}
begin
  Result := True;
  if Assigned(FOnCloseUp) then FOnCloseUp(Self);
end;

{  TRxDBLookupEditEx  }

constructor TRxDBLookupEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  inherited ReadOnly := True;
  ControlStyle := ControlStyle + [csReplicatable];
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  //DSS
  PopupOnlyLocate := False;
{$IFDEF RIGHT_SPACE_ALIGN}
  FRightSpaceAlign := False;
{$ENDIF}
end;

destructor TRxDBLookupEditEx.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TRxDBLookupEditEx.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

procedure TRxDBLookupEditEx.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TRxDBLookupEditEx.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) or ((Key = VK_INSERT) and (ssShift in Shift)) then
    FDataLink.Edit;
  inherited;
end;

procedure TRxDBLookupEditEx.KeyUp(var Key: Word; Shift: TShiftState);
var
  SStart, SLength: integer;
begin
  inherited;
  SLength := SelLength;
  SStart := SelStart + FDataLink.Field.Size - Length(Text);
  if Trim(Text) <> '' then
    Text := RightStr(Trim(Text), FDataLink.Field.Size)
  else
    Text := '';
  SelStart := SStart; //after assignment Text restore.
  SelLength := SLength;
  FDataLink.Edit;
//  SelStart := Length(Text);
end;

procedure TRxDBLookupEditEx.KeyPress(var Key: Char);
var
  NewText: string;
//  SStart, SLength: integer;
begin
{$IFDEF RIGHT_SPACE_ALIGN}
  if FRightSpaceAlign and (Key >= ' ')
    and (FDataLink.Field <> nil) and FDataLink.Field.IsValidChar(Key)
    and (SelStart <= MaxLength)
  then begin
    NewText := Copy(Text, 1, SelStart) + Key +
               Copy(Text, SelStart+SelLength, Length(Text) - SelStart - SelLength);
    if Trim(NewText) <> '' then
      Text := RightStr(Trim(NewText), FDataLink.Field.Size)
    else
      Text := '';
    SelStart := Length(Text);
  end;  
{$ENDIF}

  inherited KeyPress(Key);

  if (Key >= ' ') and (FDataLink.Field <> nil) and not FDataLink.Field.IsValidChar(Key) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
  if Key >= ' '  then FDataLink.Edit
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

function TRxDBLookupEditEx.EditCanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;

procedure TRxDBLookupEditEx.Reset;
begin
  FDataLink.Reset;
  SelectAll;
end;

procedure TRxDBLookupEditEx.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if (FAlignment <> taLeftJustify) and not IsMasked then Invalidate;
    FDataLink.Reset;
  end;
end;

procedure TRxDBLookupEditEx.Change;
begin
//  if ChangeInvoked then
//    exit; {If you will come here - should be hung.}
  Application.ProcessMessages;
  ChangeInvoked := True;
  // Added, because this procedure is called before FormCreate, mother-mother.
  if not Showing then exit;

  FDataLink.Modified;
  if Assigned(FDataLink.Dataset)
     and FDataLink.Dataset.Active
                                   //    and Assigned(LookupSource.Dataset) and LookupSource.Dataset.Active
  then begin                       // inherited Change; - replaced by more appropriate treatment options.
    if PopupOnlyLocate then begin  //DSS - PopupOnlyLocate undefined VS - now like a definite.
      if PopupVisible then PopupChange;
    end else begin
      PopupChange;
      DoChange;
    end;
  end;
  ChangeInvoked := False;
end;

function TRxDBLookupEditEx.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRxDBLookupEditEx.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TRxDBLookupEditEx.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRxDBLookupEditEx.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TRxDBLookupEditEx.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRxDBLookupEditEx.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

function TRxDBLookupEditEx.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRxDBLookupEditEx.DataChange(Sender: TObject);
begin
  Enabled := (FDataLink.Field <> nil)
             and Assigned(LookupSource)
             and LookupSource.Dataset.Active
             and FDataLink.Field.Dataset.Active;

  if Enabled then
  begin
    if (FAlignment <> FDataLink.Field.Alignment) then
    begin
      EditText := '';  //  forces update
      FAlignment := FDataLink.Field.Alignment;
    end;
    EditMask := FDataLink.Field.EditMask;

    // my piece of code - VS
    if (FDataLink.Field is TStringField) and
       (MaxLength <> FDataLink.Field.Size)
      then MaxLength := FDataLink.Field.Size;

    if FFocused and FDataLink.CanModify then
      Text := FDataLink.Field.Text
    else
    begin
      EditText := FDataLink.Field.DisplayText;
      if FDataLink.Editing and
        ( {$IFDEF USE_BDE}(FDataLink.Dataset is TBDEDataSet)
          and (FDataLink.Dataset as TBDEDataSet).CachedUpdates
          and {$ENDIF}(FDataLink.Field.OldValue <> FDataLink.Field.NewValue)
          {$IFDEF USE_BDE}or not (FDataLink.Dataset is TBDEDataSet){$ENDIF}
        )
//      FDataLink.FModified
      then
        Modified := True;
    end;
  end else begin
    FAlignment := taLeftJustify;
    EditMask := '';
    if csDesigning in ComponentState then
      EditText := Name else
      EditText := '';
  end;
end;

procedure TRxDBLookupEditEx.EditingChange(Sender: TObject);
begin
//  inherited ReadOnly := not FDataLink.Editing;
end;

procedure TRxDBLookupEditEx.UpdateData(Sender: TObject);
begin
  ValidateEdit;
  FDataLink.Field.Text := Text;
end;

procedure TRxDBLookupEditEx.WMPaste(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBLookupEditEx.WMCut(var Message: TMessage);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TRxDBLookupEditEx.CMEnter(var Message: TCMEnter);
begin
  SetFocused(True);
  inherited;
  if SysLocale.FarEast and FDataLink.CanModify then begin
    inherited ReadOnly := False;
    ReadOnly := False;
  end;
end;

procedure TRxDBLookupEditEx.CMExit(var Message: TCMExit);
begin
  try
    if FDataLink.Field.Dataset.State in [dsEdit, dsInsert] then
      FDataLink.UpdateRecord;
  except
    SelectAll;
    SetFocus;
    raise;
  end;
//  SetFocused(False);
//  CheckCursor;
  DoExit;
end;

procedure TRxDBLookupEditEx.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

function TRxDBLookupEditEx.AcceptPopup(var Value: Variant): Boolean;
begin
  FDataLink.Edit;
  result := inherited AcceptPopup(Value);
  if result then begin
    FDataLink.UpdateRecord;
    FDataLink.Reset;
  end {$IFDEF DEBUG} else MessageDlg('DFFF', mtError, [mbOk], 0){$ENDIF};
end;

procedure TRxDBLookupEditEx.DropDown;
begin
//! Not the fact that RecordCount is always running fast.
// This may have to do in particular cases.
  if DropDownCount > LookupSource.Dataset.RecordCount then
    DropDownCount := LookupSource.Dataset.RecordCount;
  inherited;
end;

end.