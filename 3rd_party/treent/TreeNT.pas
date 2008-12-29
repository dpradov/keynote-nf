unit TreeNT;

// TTreeNT  - An enhanced TreeView control with almost all additional
//            features implemented introduced since IE 3.0
//            (COMCTL32.DLL version 4.70 + : single expand, custom draw, ...) plus
//            many new abilities like multi-select, check boxes and radio
//            buttons (see help file for a complete description).
// Version     - 3.1
// Last Change - 09. November 1999
//
// written by Dipl. Ing. Mike Lischke (public@lischke-online.de)

// Modificaciones por Daniel Prado Velasco - 2007 [dpv]

{$R-,B-}


interface

{$I DFS.inc}
{$LONGSTRINGS ON}

{.$define DarkImages} // include this compiler switch if you want to have darker default check images

{$ifndef DFS_DELPHI_4_UP}
  {$ifndef DFS_CPPB}
    {$define WithCommCrtl98}
  {$endif}
{$endif}

uses
  Windows, Classes, ShellAPI,
  {$ifdef DFS_DELPHI_4_UP} ImgList, {$endif}
  {$ifdef WithCommCrtl98} CommCtrl98, {$else} CommCtrl, {$endif}
  {$IFDEF DFS_DELPHI_6_UP} // *1
  RTLConsts,
  {$ENDIF}
  Controls, ExtCtrls, Forms, Graphics, Messages, SysUtils, Menus, ComCtrls;


const
  _MJ_INVALIDATE_ON_FOCUS_CHANGE : boolean = false; // [mj]

{$ifdef DFS_COMPILER_4_UP}
const
  CDRF_NOTIFYITEMERASE = $00000080;
{$endif}

const
  // indices for check state images used for checking
  ckEmpty         = 0; // an empty image (state index must be > 0)
  ckCheckEmpty    = 1; // an empty checkbox
  ckCheckChecked  = 2; // a checkbox with a cross
  ckCheckDisabled = 3; // a grayed and empty checkbox
  ckCheckGrayed   = 4; // a grayed checkbox with a cross
  ckRadioEmpty    = 5; // an empty radio button (circle)
  ckRadioChecked  = 6; // a radio button with a filled circle
  ckRadioDisabled = 7; // a grayed and empty radio button
  ckRadioGrayed   = 8; // a grayed radio button with a filled circle

type
  TTreeOptions = set of (toAutoExpand, toAutoScroll, toCheckSupport, toNoEraseBkgnd, toEvenHeight, toFullRowSelect,
                         toHideSelection, toHotTrack, toInfoTip, toLevelSelectConstraint, toMultiSelect, toNoScroll, toReadOnly, toRightClickSelect,
                         toToolTips, toShowButtons, toShowLines, toShowRoot, toSingleExpand, toWantReturn);

const
  DefaultOptions = [toAutoExpand, toAutoScroll, toEvenHeight, toHideSelection, toShowButtons,
                    toShowLines, toShowRoot, toRightClickSelect];

type

  TCustomTreeNT = class;
  TTreeNTNodes  = class;
  TTreeNTNode   = class;

  { TTreeNodeList class }

  PNodeList        = ^TNodeList;
  TNodeList        = array[0..MaxListSize-1] of TTreeNTNode;

  TTreeNodeList = class(TObject)
  private
    FList     : PNodeList;
    FCount    : Integer;
    FCapacity : Integer;
    function Get(Index: Integer): TTreeNTNode;
    procedure Put(Index: Integer; Item: TTreeNTNode);
  protected
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetCountNotHidden: Integer;
  public
    destructor Destroy; override;
    function Add(Item: TTreeNTNode): Integer;
    procedure Clear; dynamic;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TTreeNodeList;
    function First: TTreeNTNode;
    function IndexOf(Item: TTreeNTNode): Integer;
    procedure Insert(Index: Integer; Item: TTreeNTNode);
    function Last: TTreeNTNode;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TTreeNTNode): Integer;
    procedure Pack;
    {$ifdef DFS_COMPILER_3_UP}
      procedure Sort(Compare: TTVCompare; Data: Integer);
    {$else}
      procedure Sort(Compare: TTVCompare; Data: Longint);
    {$endif}
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property CountNotHidden: Integer read GetCountNotHidden;                // [dpv]
    property Items[Index: Integer]: TTreeNTNode read Get write Put; default;
    property List: PNodeList read FList;
  end;

  TNodeState = (nsChecked, nsCut, nsDisabled, nsDropHilited, nsExpanded, nsExpandedOnce, nsExpandPartial, nsFocused,
                nsGrayed, nsHot, nsIndeterminate, nsMarked, nsSelected);
  TNodeStates = set of TNodeState;

  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);
  TAddMode = (taAddFirst, taAdd, taInsert);

  TCheckType = (ctNone, ctCheckBox, ctCheckBoxGrayed, ctRadioButton);
  TCheckState = (csUnchecked, csChecked, csGrayed, csCheckedGrayed);

  THiddenState = (hsHidden, hsVisible, hsHiding);      // [dpv]

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    IntegralHeight: Integer;
    CheckState: TCheckState;
    CheckType: TCheckType;
    Enabled: Boolean;
    Expanded: Boolean;
    Selected: Boolean;
    Data: Pointer;
    Count: Integer;
    Color: TColor;
    ParentColor: Boolean;
    ParentFont: Boolean;
    FontData: TFontData;
    FontColor: TColor;
    Hidden: THiddenState;                // [dpv]
    Text: String[255];
  end;


  TTreeNTNode = class(TPersistent)
  private
    FOwner: TTreeNTNodes;
    FText: String;
    FData: Pointer;
    FItemId: HTreeItem;
    FFont: TFont;
    FParent: TTreeNTNode;
    FExpanded,
    FParentColor,
    FParentFont: Boolean;
    FImageIndex,
    FSelectedIndex,
    FOverlayIndex,
    FStateIndex: Integer;
    FDeleting: Boolean;
    FColor: TColor;
    FInTree: Boolean;
    FUpdateCount: Integer;
    FChildList: TTreeNodeList;
    FCheckState: TCheckState;
    FCheckType: TCheckType;
    FEnabled: Boolean;
    FHidden: THiddenState;   // [dpv]
    function DoCanExpand(Expand: Boolean): Boolean;
    procedure DoCheckClick;
    procedure DoExpand(Expand: Boolean);
    procedure ExpandItem(Expand: Boolean; Recurse: Boolean);
    function GetAbsoluteCount: Integer;
    function GetAbsoluteIndex: Integer;
    function GetColor: TColor;
    function GetIndex: Integer;
    function GetExpanded: Boolean;
    function GetExpandedOnce: Boolean;
    function GetExpandedPartial: Boolean;
    function GetLevel: Integer;
    function GetChildren: Boolean;
    function GetCut: Boolean;
    function GetDropTarget: Boolean;
    function GetFocused: Boolean;
    function GetFont: TFont;
    function GetIntegralHeight: Integer;
    function GetItem(Index: Integer): TTreeNTNode;
    function GetParent: TTreeNTNode;
    function GetSelected: Boolean;
    function GetState(NodeState: TNodeState): Boolean;
    function GetCount: Integer;
    function GetTreeNT: TCustomTreeNT;
    procedure InternalMove(ParentNode, Node: TTreeNTNode; HItem: HTreeItem; AddMode: TAddMode);
    function IsEqual(Node: TTreeNTNode): Boolean;
    function IsNodeVisible: Boolean;
    procedure SetCheckState(Value: TCheckState);
    procedure SetCheckType(Value: TCheckType);
    procedure SetChildren(Value: Boolean);
    procedure SetColor(AValue: TColor);
    procedure SetCut(Value: Boolean);
    procedure SetData(Value: Pointer);
    procedure SetDropTarget(Value: Boolean);
    procedure SetEnabled(Value: Boolean);
    procedure SetFont(AFont: TFont);
    procedure SetItem(Index: Integer; Value: TTreeNTNode);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedPartial(Value: Boolean);
    procedure SetFocused(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetIntegralHeight(Value: Integer);
    procedure SetOverlayIndex(Value: Integer);
    procedure SetParentColor(AValue: Boolean);
    procedure SetParentFont(AValue: Boolean);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetSelected(Value: Boolean);
    procedure SetStateIndex(Value: Integer);
    procedure SetText(const S: String);
    procedure SetHidden(Value: Boolean);    // [dpv]
    function GetHidden: Boolean;            // [dpv]
    function GetMarkedHidden: Boolean;      // [dpv]
    procedure Hide(Hide: Boolean; Force: Boolean; RecurseForce: Boolean);    // [dpv]
  protected
    procedure FontChanged(Sender: Tobject);
    procedure ReadData(Stream: TStream; Info: PNodeInfo); virtual;
    function ReadStrings(var S: PChar; CurrentLevel: Integer): Integer;
    procedure WriteData(Stream: TStream; Info: PNodeInfo); virtual;
    procedure WriteStrings(Stream: TStream; Level: Integer);
  public
    constructor Create(AOwner: TTreeNTNodes);
    destructor Destroy; override;
    function AlphaSort: Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Collapse(Recurse: Boolean);
    function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
    procedure Delete;
    procedure DeleteChildren;
    function DisplayRect(TextOnly: Boolean): TRect;
    function EditText: Boolean;
    procedure EndEdit(Cancel: Boolean);
    procedure EndUpdate;
    procedure Expand(Recurse: Boolean);
    function GetFirstChild: TTreeNTNode;
    function GetHandle: HWND;
    function GetLastChild: TTreeNTNode;
    function GetNext: TTreeNTNode;
    function GetNextNotHidden: TTreeNTNode; // [dpv]
    function GetNextChild(Value: TTreeNTNode): TTreeNTNode;
    function GetNextSelected: TTreeNTNode;
    function GetNextSibling: TTreeNTNode;
    function GetNextSiblingNotHidden: TTreeNTNode;  // [dpv]
    function GetNextVisible: TTreeNTNode;
    function GetPrev: TTreeNTNode;
    function GetPrevChild(Value: TTreeNTNode): TTreeNTNode;
    function GetPrevSelected: TTreeNTNode;
    function GetPrevSibling: TTreeNTNode;
    function GetPrevSiblingNotHidden: TTreeNTNode;    // [dpv]
    function GetPrevVisible: TTreeNTNode;
    function HasAsParent(Value: TTreeNTNode): Boolean;
    function IndexOf(Value: TTreeNTNode): Integer;
    function IsUpdating: Boolean;
    procedure MakeVisible;
    procedure MakeVisibilityPosible;       // [dpv]
    procedure MoveTo(Destination: TTreeNTNode; Mode: TNodeAttachMode);

    property AbsoluteCount: Integer read GetAbsoluteCount;
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property CheckState: TCheckState read FCheckState write SetCheckState;
    property CheckType: TCheckType read FCheckType write SetCheckType;
    property Color: TColor read GetColor write SetColor;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    property Data: Pointer read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Focused: Boolean read GetFocused write SetFocused;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property ExpandedOnce: Boolean read GetExpandedOnce;
    property ExpandedPartial: Boolean read GetExpandedPartial write SetExpandedPartial;
    property Font: TFont read GetFont write SetFont;
    property Handle: HWND read GetHandle;
    property HasChildren: Boolean read GetChildren write SetChildren;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property IntegralHeight: Integer read GetIntegralHeight write SetIntegralHeight;
    property IsVisible: Boolean read IsNodeVisible;
    property Item[Index: Integer]: TTreeNTNode read GetItem write SetItem; default;
    property ItemId: HTreeItem read FItemId;
    property Level: Integer read GetLevel;
    property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex;
    property Owner: TTreeNTNodes read FOwner;
    property Parent: TTreeNTNode read GetParent;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property Selected: Boolean read GetSelected write SetSelected;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property StateIndex: Integer read FStateIndex write SetStateIndex;
    property Text: String read FText write SetText;
    property TreeView: TCustomTreeNT read GetTreeNT;
    property Hidden: Boolean read GetHidden write SetHidden;       // [dpv]
    property MarkedHidden: Boolean read GetMarkedHidden;           // [dpv]   Explicit marked as Hidden
  end;

  TTreeNTNodeClass = class of TTreeNTNode;
  TSelectType = (stSet, stReset, stToggle);
  TFindFlags  = set of (ffText, ffData);

  TTreeNTNodes = class(TPersistent)
  private
    FOwner: TCustomTreeNT;
    FUpdateCount: Integer;
    FCoreSelecting,
    FDeleting: Boolean;
    FRoot: TTreeNTNode;
    FItemCache,
    FSelection: TTreeNodeList;
    FSelLockCount: Integer;
    FRootCheckType: TCheckType;
    FLastSelLevel: Integer;
    FCount    : Integer;                      // [dpv]
    procedure AddedNode(ParentNode: TTreeNTNode);
    function GetHandle: HWND;
    procedure ReadData(Stream: TStream);
    procedure Repaint(Node: TTreeNTNode);
    procedure SetCheckType(Value: TCheckType);
    procedure WriteData(Stream: TStream);
  protected
    function AddItem(Parent, Target: HTreeItem; const Item: TTVItem; AddMode: TAddMode): HTreeItem;
    procedure AddToSelection(Node: TTreeNTNode);
    function InternalAddObject(Node: TTreeNTNode; const S: String; Ptr: Pointer; AddMode: TAddMode): TTreeNTNode;
    procedure DefineProperties(Filer: TFiler); override;
    function CreateItem(Node: TTreeNTNode): TTVItem;
    procedure FillCache;
    function GetCountNotHidden: Integer;      // GetCount --> GetCountNotHidden   [dpv]
    function GetCount: Integer;               // [dpv]
    function GetSelectedCount: Integer;
    procedure SelectNode(Node: TTreeNTNode; Value: TSelectType);
    procedure SelectNodes(NodeFrom, NodeTo: TTreeNTNode; AddOnly: Boolean);
    procedure SetItem(Index: Integer; Value: TTreeNTNode);
    procedure SetUpdateState(Updating: Boolean);
    procedure ToggleSelectionRange(NodeFrom, NodeTo: TTreeNTNode);
    procedure ReadStrings(Stream: TStream);
    procedure RemoveFromSelection(Node: TTreeNTNode);
    procedure WriteStrings(Stream: TStream);
  public
    constructor Create(AOwner: TCustomTreeNT);
    destructor Destroy; override;

    function AddChildFirst(Node: TTreeNTNode; const S: String): TTreeNTNode;
    function AddChild(Node: TTreeNTNode; const S: String): TTreeNTNode;
    function AddChildObjectFirst(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;
    function AddChildObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;
    function AddFirst(Node: TTreeNTNode; const S: String): TTreeNTNode;
    function Add(Node: TTreeNTNode; const S: String): TTreeNTNode;
    function AddObjectFirst(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;
    function AddObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Node: TTreeNTNode);
    procedure DeleteSelectedNodes;
    procedure EndUpdate;
    function FindNode(Flags: TFindFlags; AText: String; AData: Pointer): TTreeNTNode;
    function GetFirstNode: TTreeNTNode;
    function GetFirstSelectedNode: TTreeNTNode;
    function GetNode(ItemId: HTreeItem): TTreeNTNode;
    function GetNodeFromIndex(Index: Integer): TTreeNTNode;
    function Insert(Node: TTreeNTNode; const S: String): TTreeNTNode;
    function InsertObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;
    procedure InvalidateItemCache; virtual;
    function IsDeleting: Boolean;
    function IsUpdating: Boolean;
    function LockSelection: Integer;
    procedure SelectAll;   // [dpv] Doesn't include hidden ones
    procedure SelectAllPlusHidden;              // [dpv]
    function UnlockSelection: Integer;

    property CountNotHidden: Integer read GetCountNotHidden;     // [dpv]
    property Count: Integer read GetCount;                       // [dpv]
    property Handle: HWND read GetHandle;
    property Item[Index: Integer]: TTreeNTNode read GetNodeFromIndex; default;
    property SelectedCount: Integer read GetSelectedCount;
    property Owner: TCustomTreeNT read FOwner;
    property TopLevelCheckType: TCheckType read FRootCheckType write SetCheckType;
  end;


  ETreeNTError = class(Exception);

  THitTest     = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon,
                  htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
  THitTests    = set of THitTest;

  TTVChangingEvent   = procedure(Sender: TObject; Node: TTreeNTNode; var AllowChange: Boolean) of object;
  TTVChangedEvent    = procedure(Sender: TObject; Node: TTreeNTNode) of object;
  TTVEditingEvent    = procedure(Sender: TObject; Node: TTreeNTNode; var AllowEdit: Boolean) of object;
  TTVEditedEvent     = procedure(Sender: TObject; Node: TTreeNTNode; var S: String) of object;

  // [MJ] added
  TTVEditCanceledEvent = procedure(Sender: TObject) of object;
  TFileDroppedEvent = procedure( sender : Tobject; FileList : TStringList ) of object;
  // end [MJ] added

  TTVExpandingEvent  = procedure(Sender: TObject; Node: TTreeNTNode; var AllowExpansion: Boolean) of object;
  TTVCollapsingEvent = procedure(Sender: TObject; Node: TTreeNTNode; var AllowCollapse: Boolean) of object;
  TTVExpandedEvent   = procedure(Sender: TObject; Node: TTreeNTNode) of object;
  TTVHintEvent       = procedure(Sender: TObject; Node: TTreeNTNode; var NewText: String) of object;
  {$ifdef DFS_COMPILER_3_UP}
    TTVCompareEvent    = procedure(Sender: TObject; Node1, Node2: TTreeNTNode; Data: Integer; var Compare: Integer) of object;
  {$else}
    TTVCompareEvent    = procedure(Sender: TObject; Node1, Node2: TTreeNTNode; Data: Integer; var Compare: Longint) of object;
  {$endif}
  TTVPaintEvent      = procedure(Sender: TObject) of object;
  TTVBeforeItemPaintEvent  = procedure(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates; var OwnerDraw: Boolean)of object;
  TTVAfterItemPaintEvent  = procedure(Sender: TObject; Node: TTreeNTNode; ItemRect: TRect; NodeStates: TNodeStates)of object;
  TTVSingleExpandingEvent = procedure(Sender: TObject; Node: TTreeNTNode; var AutoCollapse: Boolean) of object;
  TTVCreateNodeEvent = procedure(Sender: TObject; var NodeClass: TTreeNTNodeClass) of object;
  TTVCheckingEvent   = procedure(Sender: TObject; Node: TTreeNTNode; var AllowCheck: Boolean) of object;
  TTVCheckedEvent    = procedure(Sender: TObject; Node: TTreeNTNode) of object;
  TTVPopupEvent      = procedure(Sender: TObject; Node: TTreeNTNode; var AskParent: Boolean; var PopupMenu: TPopupMenu) of object;
  TTVHelpContextEvent  = procedure(Sender: TObject; Node: TTreeNTNode; var HelpContext: Integer) of object;

  TSortType = (stNone, stData, stText, stBoth);

  TScrollDirection = set of (sdLeft, sdUp, sdRight, sdDown);

  // used to track a selection rectangle in multi-selection mode
  TSelectRec = record
    Pending: Boolean;
    StartX,
    StartY: Integer;
    Rect: TRect;
  end;

  TLevelErrorEvent = procedure(Line: Integer; var RaiseException: Boolean) of object;

  TCustomTreeNT = class(TWinControl)
  private
    FBorderStyle: TBorderStyle;
    FImages: TImageList;
    FStateImages: TImageList;
    FImageChangeLink: TChangeLink;
    FStateChangeLink: TChangeLink;
    FExpandTimer: TTimer;
    FScrollTimer: TTimer;
    FScrollDirection: TScrollDirection;
    FDragImage: TImageList;
    FTreeNTNodes: TTreeNTNodes;
    FSortType: TSortType;
    FSaveTopIndex: Integer;
    FSaveIndex: Integer;
    FSaveIndent: Integer;
    FSaveItemHeight: Integer;
    FScrollTime: Integer;
    FMemStream: TMemoryStream;
    FEditInstance: Pointer;
    FDefEditProc: Pointer;
    FEditHandle: HWND;
    FDragged: Boolean;
    FClicked: Boolean;
    FRClicked: Boolean;
    FOptions: TTreeOptions;
    FManualNotify: Boolean;
    FLastDropTarget: TTreeNTNode;
    FDragNode: TTreeNTNode;
    FDragObject: TDragObject;
    FRClickNode: TTreeNTNode;
    FCanvas: TCanvas;
    FLastFont: THandle;
    FStateChanging: Boolean;
    FFirstSelection: TTreeNTNode;
    FSelectRec: TSelectRec;
    FHitList: TTreeNodeList;
    FCheckNode: TTreeNTNode;
    FColorUnfocusedSelected,
    FColorSelected,
    FColorDropSelected: TColor;

    FOnAfterPaint: TTVPaintEvent;
    FOnBeforePaint: TTVPaintEvent;
    FOnAfterItemPaint: TTVAfterItemPaintEvent;
    FOnBeforeItemPaint: TTVBeforeItemPaintEvent;
    FOnEditing: TTVEditingEvent;
    FOnEdited: TTVEditedEvent;
    FOnEditCanceled : TTVEditCanceledEvent; // [MJ]
    FOnFileDropped : TFileDroppedEvent; // [MJ]
    FOnExpanded: TTVExpandedEvent;
    FOnExpanding: TTVExpandingEvent;
    FOnCollapsed: TTVExpandedEvent;
    FOnCollapsing: TTVCollapsingEvent;
    FOnChanging: TTVChangingEvent;
    FOnChange: TTVChangedEvent;
    FOnCompare: TTVCompareEvent;
    FOnDeletion: TTVExpandedEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    FOnGetSelectedIndex: TTVExpandedEvent;
    FOnHint: TTVHintEvent;
    FOnSingleExpanded: TTVSingleExpandingEvent;
    FOnCreateNode: TTVCreateNodeEvent;
    FOnChecking: TTVCheckingEvent;
    FOnChecked: TTVCheckedEvent;
    FOnGetPopupMenu: TTVPopupEvent;
    FOnGetHelpContext: TTVHelpContextEvent;
    FOnLevelError: TLevelErrorEvent;

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DoDragExpand(Sender: TObject);
    procedure DoScroll(Sender: TObject);
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    procedure EditWndProc(var Message: TMessage);
    procedure FontChanged(Sender: TObject);
    function GetChangeDelay: Integer;
    function GetDropTarget: TTreeNTNode;
    procedure GetImageIndex(Node: TTreeNTNode);
    function GetIndent: Integer;
    function GetInsertMarkColor: TColor;
    function GetItemHeight: ShortInt;
    function GetLastVisibleNode: TTreeNTNode;
    function GetNodeFromItem(const Item: TTVItem): TTreeNTNode;
    function GetScrollTime: Integer;
    function GetSearchString: String;
    procedure GetSelectedIndex(Node: TTreeNTNode);
    function GetSelection: TTreeNTNode;
    function GetTopItem: TTreeNTNode;
    function GetTreeHeight: Integer;
    function GetTreeWidth: Integer;
    procedure HandleDrawSelection(Shift: TShiftState; Rect: TRect);
    procedure HandleMultiSelection(LastNode: TTreeNTNode; OldState: TSelectType; NewNode: TTreeNTNode; NewState: TSelectType; Shift: TShiftState);
    procedure ImageListChange(Sender: TObject);
    procedure OnChangeTimer(Sender: TObject);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCheckImage(Node: TTreeNTNode; Image: Integer);
    procedure SetColorDropSelected(Color: TColor);
    procedure SetColorSelected(Color: TColor);
    procedure SetColorUnfocusedSelected(Color: TColor);
    procedure SetChangeDelay(Value: Integer);
    procedure SetDropTarget(Value: TTreeNTNode);
    procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetIndent(Value: Integer);
    procedure SetInsertMarkColor(Value: TColor);
    procedure SetImages(Value: TImageList);
    procedure SetItemHeight(Value: ShortInt);
    procedure SetOptions(Values: TTreeOptions);
    procedure SetScrollTime(Value: Integer);
    procedure SetSelection(Value: TTreeNTNode);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TImageList);
    procedure SetStyle(Value: Integer; UseStyle: Boolean);
    procedure SetTreeNTNodes(Value: TTreeNTNodes);
    procedure SetTopItem(Value: TTreeNTNode);
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; // [MJ]

  protected
    FChangeTimer: TTimer;
    function CanCheck(Node: TTreeNTNode): Boolean; dynamic;
    function CanEdit(Node: TTreeNTNode): Boolean; dynamic;
    function CanChange(Node: TTreeNTNode): Boolean; dynamic;
    function CanCollapse(Node: TTreeNTNode): Boolean; dynamic;
    function CanExpand(Node: TTreeNTNode): Boolean; dynamic;
    procedure Change(Node: TTreeNTNode); dynamic;
    procedure Check(Node: TTreeNTNode); dynamic;
    procedure Collapse(Node: TTreeNTNode); dynamic;
    function CreateNode: TTreeNTNode; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoAutoScroll(X, Y: Integer); virtual;
    procedure DoChange(Node: TTreeNTNode); virtual;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DrawFocusRect(Rect: TRect);
    procedure Edit(const Item: TTVItem); dynamic;
    procedure Expand(Node: TTreeNTNode); dynamic;
    function  GetDragImages: {$ifdef DFS_COMPILER_4_UP} TDragImageList {$else} TCustomImageList {$endif}; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure WndProc(var Message: TMessage); override;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    property ColorDropSelected: TColor read FColorDropSelected write SetColorDropSelected default clBtnFace;
    property ColorSelected: TColor read FColorSelected write SetColorSelected default clHighlight;
    property ColorUnfocusedSelected: TColor read FColorUnfocusedSelected write SetColorUnfocusedSelected default cl3DLight;
    property Images: TImageList read FImages write SetImages;
    property Indent: Integer read GetIndent write SetIndent;
    property InsertMarkColor: TColor read GetInsertMarkColor write SetInsertMarkColor;
    property ItemHeight: ShortInt read GetItemHeight write SetItemHeight;
    property Items: TTreeNTNodes read FTreeNTNodes write SetTreeNTNodes;
    property Options: TTreeOptions read FOptions write SetOptions
             default DefaultOptions;
    property ScrollTime: Integer read GetScrollTime write SetScrollTime default 100;
    property SearchString: String read GetSearchString;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TImageList read FStateImages write SetStateImages;

    property OnAfterPaint: TTVPaintEvent read FOnAfterPaint write FOnAfterPaint;
    property OnAfterItemPaint: TTVAfterItemPaintEvent read FOnAfterItemPaint write FOnAfterItemPaint;
    property OnBeforePaint: TTVPaintEvent read FOnBeforePaint write FOnBeforePaint;
    property OnBeforeItemPaint: TTVBeforeItemPaintEvent read FOnBeforeItemPaint write FOnBeforeItemPaint;
    property OnChecked: TTVCheckedEvent read FOnChecked write FOnChecked;
    property OnChecking: TTVCheckingEvent read FOnChecking write FOnChecking;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnCompare: TTVCompareEvent read FOnCompare write FOnCompare;
    property OnCreateNode: TTVCreateNodeEvent read FOnCreateNode write FOnCreateNode;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnEditCanceled:  TTVEditCanceledEvent read FOnEditCanceled write FOnEditCanceled;
    property OnFileDropped : TFileDroppedEvent read FOnFileDropped write FOnFileDropped;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnGetHelpContext: TTVHelpContextEvent read FOnGetHelpContext write FOnGetHelpContext;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetPopupMenu: TTVPopupEvent read FOnGetPopupMenu write FOnGetPopupMenu;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property OnHint: TTVHintEvent read FOnHint write FOnHint;
    property OnSingleExpanded: TTVSingleExpandingEvent read FOnSingleExpanded write FOnSingleExpanded;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AlphaSort: Boolean;
    procedure ClearSelection;
    function CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;
    procedure DrawTo(ACanvas: TCanvas);
    procedure FullCollapse;
    procedure FullExpand;
    procedure FullNotHidden;                            // [dpv]
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetNodeAt(X, Y: Integer): TTreeNTNode;
    function GetToolTips: HWND;
    function IsEditing: Boolean;
    function IsMouseSelecting: Boolean;
    procedure LoadFromFile(const FileName: String);
    procedure LoadFromStream(Stream: TStream);
    procedure Print(XPos, YPos, Width: Integer);
    procedure SaveToFile(const FileName: String; Complete: Boolean);
    procedure SaveToStream(Stream: TStream; Complete: Boolean);
    function ScreenToClientEx(const Point: TPoint): TPoint;
    function SetToolTips(TTHandle: HWND): HWND;
    procedure ShowInsertMark(Node: TTreeNTNode; After: Boolean);

    property Canvas: TCanvas read FCanvas;
    property DropTarget: TTreeNTNode read GetDropTarget write SetDropTarget;
    property LastVisibleNode: TTreeNTNode read GetLastVisibleNode;
    property Selected: TTreeNTNode read GetSelection write SetSelection;
    property TopItem: TTreeNTNode read GetTopItem write SetTopItem;
    property TreeHeight: Integer read GetTreeHeight;
    property TreeWidth: Integer read GetTreeWidth;

    property OnLevelError: TLevelErrorEvent read FOnLevelError write FOnLevelError;
  end;

  TTreeNT = class(TCustomTreeNT)
  published
    property Align;
    {$ifdef DFS_COMPILER_4_UP}
      property Anchors;
      property BiDiMode;
    {$endif}
    property BorderStyle;
    {$ifdef DFS_COMPILER_4_UP}
      property BorderWidth;
    {$endif}
    property ChangeDelay;
    property Color;
    property ColorDropSelected;
    property ColorSelected;
    property ColorUnfocusedSelected;
    property Ctl3D;
    {$ifdef DFS_COMPILER_4_UP}
      property Constraints;
      property DragKind;
    {$endif}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property Indent;
    property InsertMarkColor;
    property Items;
    property ItemHeight;
    property Options;
    {$ifdef DFS_COMPILER_4_UP}
      property ParentBiDiMode;
    {$endif}
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ScrollTime;
    property SearchString;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Visible;

    property OnAfterPaint;
    property OnAfterItemPaint;
    property OnBeforePaint;
    property OnBeforeItemPaint;
    property OnChange;
    property OnChanging;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnCreateNode;
    property OnDeletion;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditCanceled;
    property OnFileDropped;
    property OnEditing;
    {$ifdef DFS_COMPILER_4_UP}
      property OnEndDock;
    {$endif}
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnEndDrag;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetPopupMenu;
    property OnGetSelectedIndex;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSingleExpanded;
    {$ifdef DFS_COMPILER_4_UP}
      property OnStartDock;
    {$endif}
    property OnStartDrag;
  end;

//------------------------------------------------------------------------------

implementation

{$R TreeNT.res}

uses
  Consts, ComStrs, Dialogs, Printers;

var
  CheckImages: TImageList;
  CurrentLine: Integer;

//----------------- end additonal declariations --------------------------------

procedure LoadAndMapBitmap(var BM: TBitmap; Instance: THandle; ResName: PChar);

// loads the named bitmap resource (which must be a 16 color bitmap) and replaces
// light gray and dark gray by clBtnFace and clBtnShadow, respectively

// In D3 +  there's already a function to do that (CreateGrayMappedRes), but this isn't
// available in D2,  so I had to write my own.

var
  BMData: TMemoryStream;
  Color: TRGBQuad;
  Temp: Byte;

begin
  // load the bitmap
  BM.LoadFromResourceName(Instance, ResName);
  // create a temporary mem stream to manipulate the bitmap's palette
  // (cannot use TResourceStream here, because it cuts out the bitmap file header, which is
  // needed to load the bitmap from it finally)
  BMData := TMemoryStream.Create;
  try
    // write the bitmap's entire content to this stream
    BM.SaveToStream(BMData);
    BM.FreeImage;
    // seek to the seventh palette entry (dark gray in our case)
    BMData.Seek(SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + 7 * SizeOf(TRGBQuad), soFromBeginning);
    // split the current system color for clBtnShadow into its red, green and blue fragments
    Integer(Color) := ColorToRGB(clBtnShadow);
    // exchange red and blue
    Temp := Color.rgbBlue;
    Color.rgbBlue := Color.rgbRed;
    Color.rgbRed := Temp;
    // write it in place of the dark gray palette entry
    BMData.Write(Color, SizeOf(Color));
    // now the same stuff for clBtnFace...
    Integer(Color) := ColorToRGB(clBtnFace);
    Temp := Color.rgbBlue;
    Color.rgbBlue := Color.rgbRed;
    Color.rgbRed := Temp;
    BMData.Write(Color, SizeOf(Color));
    // rewind the stream ...
    BMData.Seek(0, soFromBeginning);
    // ... and finally load the bitmap from it (with the new colors)
    BM.LoadFromStream(BMData);
  finally
    BMData.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure ConvertAndAddImages(IL: TImageList);

// loads the internal state image list for the tree's check images

var Images,
    OneImage  : TBitmap;
    I         : Integer;
    MaskColor : TColor;
    Source,
    Dest      : TRect;

begin
  // Since we want our state images looking in the correct system colors,
  // we have to remap its colors. I really would like to use
  // IL.GetInstRes(HInstance, rtBitmap, Resname, 16, [lrMap3DColors], clWhite) for this task,
  // but could not get it working and don't know why :-(
  Images := TBitmap.Create;
  OneImage := TBitmap.Create;
  {$ifdef DarkImages}
    LoadAndMapBitmap(Images, HINstance, 'CHECK_DARK');
  {$else}
    LoadAndMapBitmap(Images, HINstance, 'CHECK_LIGHT');
  {$endif}
  try
    // it is assumed that the image height determines also the width of
    // one entry in the image list
    IL.Clear;
    IL.Height := Images.Height;
    IL.Width := Images.Height;
    OneImage.Width := IL.Width;
    OneImage.Height := IL.Height;
    MaskColor := Images.Canvas.Pixels[0, 0];
    Dest := Rect(0, 0, IL.Width, IL.Height);
    for I := 0 to (Images.Width div Images.Height) - 1 do
    begin
      Source := Rect(I * IL.Width, 0, (I + 1) * IL.Width, IL.Height);
      OneImage.Canvas.CopyRect(Dest, Images.Canvas, Source);
      IL.AddMasked(OneImage, MaskColor);
    end;
  finally
    Images.Free;
    OneImage.Free;
  end;
end;

//------------------------------------------------------------------------------

{$ifdef DFS_COMPILER_3_UP}

  procedure TreeNTError(Msg: String);

  begin
    raise ETreeNTError.Create(Msg);
  end;

{$else}

  procedure TreeNTError(MsgID: Integer);

  begin
    raise ETreeNTError.CreateRes(MsgID);
  end;

{$endif}

//----------------- TTreeNodeList ----------------------------------------------

destructor TTreeNodeList.Destroy;

begin
  Clear;
end;

//------------------------------------------------------------------------------

function TTreeNodeList.Add(Item: TTreeNTNode): Integer;

begin
  Result := FCount;
  if Result = FCapacity then Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Clear;

begin
  SetCount(0);
  SetCapacity(0);
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Delete(Index: Integer);

begin
  if (Index < 0) or (Index >= FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index);
  {$else}
    Error(LoadStr(SListIndexError), Index);
  {$endif}
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TTreeNTNode));
end;

//------------------------------------------------------------------------------

class procedure TTreeNodeList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
                   MOV EAX, [EBP + 4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Exchange(Index1, Index2: Integer);

var Item: TTreeNTNode;

begin
  if (Index1 < 0) or (Index1 >= FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index1);
  {$else}
    Error(LoadStr(SListIndexError), Index1);
  {$endif}
  if (Index2 < 0) or (Index2 >= FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index2);
  {$else}
    Error(LoadStr(SListIndexError), Index2);
  {$endif}
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

//------------------------------------------------------------------------------

function TTreeNodeList.Expand: TTreeNodeList;

begin
  if FCount = FCapacity then Grow;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TTreeNodeList.First: TTreeNTNode;

begin
  Result := Get(0);
end;

//------------------------------------------------------------------------------

function TTreeNodeList.Get(Index: Integer): TTreeNTNode;

begin
  if (Index < 0) or (Index >= FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index);
  {$else}
    Error(LoadStr(SListIndexError), Index);
  {$endif}
  Result := FList[Index];
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Grow;

var Delta: Integer;

begin
  if FCapacity > 64 then Delta := FCapacity div 4
                    else
    if FCapacity > 8 then Delta := 16
                     else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

//------------------------------------------------------------------------------

function TTreeNodeList.IndexOf(Item: TTreeNTNode): Integer; assembler;

// finds Item's index in the list
// EAX contains the Self reference (address of this instance)
// EDX contains Item

asm
                   PUSH EBX
                   PUSH EDI
                   MOV ECX, EAX.FCount  // number of entries
                   JECXZ @NotFound     // no search, if no entries
                   MOV EBX, ECX         // keep count for index calc.
                   MOV EDI, EAX.FList   // start address
                   MOV EAX, EDX         // search value
                   REPNE SCASD         // search the value
                   JNZ @NotFound       // jump if not found
                   MOV EAX, EBX
                   SUB EAX, ECX         // calculate index
                   DEC EAX             // it's zero-based
                   JMP @Finish

@NotFound:         MOV EAX, -1
@Finish:           POP EDI
                   POP EBX
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Insert(Index: Integer; Item: TTreeNTNode);

begin
  if (Index < 0) or (Index > FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index);
  {$else}
    Error(LoadStr(SListIndexError), Index);
  {$endif}
  if FCount = FCapacity then Grow;
  if Index < FCount then System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TTreeNTNode));
  FList[Index] := Item;
  Inc(FCount);
end;

//------------------------------------------------------------------------------

function TTreeNodeList.Last: TTreeNTNode;

begin
  Result := Get(FCount - 1);
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Move(CurIndex, NewIndex: Integer);

var Item: Pointer;

begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
    {$ifdef DFS_COMPILER_3_UP}
      Error(SListIndexError, NewIndex);
    {$else}
      Error(LoadStr(SListIndexError), NewIndex);
    {$endif}
    Item := Get(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Put(Index: Integer; Item: TTreeNTNode);

begin
  if (Index < 0) or (Index >= FCount) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListIndexError, Index);
  {$else}
    Error(LoadStr(SListIndexError), Index);
  {$endif}
  FList[Index] := Item;
end;

//------------------------------------------------------------------------------

function TTreeNodeList.Remove(Item: TTreeNTNode): Integer;

begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.Pack; assembler;

// removes all nil entries from the list
// EAX contains Self reference

// This routine is in particular effective for very fragmented lists or large holes, but
// looses the battle with TList's Pack if only a few nil entries are there, because
// all non-nil entries are moved, even if they land on the same address as they were
// before.

asm
                   PUSH EDI
                   PUSH ESI
                   MOV ECX, EAX.FCount            // current entries in list
                   JECXZ @Finish                 // nothing to do?
                   MOV ESI, EAX.FList             // source and destination point to
                   MOV EDI, ESI                   // the list memory
                   SUB EDX, EDX                   // remaining entries counter
@Loop:             CMP DWORD PTR [ESI], 0         // entry = nil?
                   JZ @Empty                     // yes, it's empty, so skip it
                   MOVSD                         // else move the entry to the new location
                   INC EDX                       // count the moved entries
                   LOOPNZ @Loop                  // do it until all entries are processed
                   JMP @SetCount

@Empty:            ADD ESI, 4                     // point to the next entry
                   LOOPNZ @Loop                  // do it until all entries are processed
@SetCount:         CMP EAX.FCount, EDX            // needs the count to be adjusted
                   JE @Finish
                   CALL SetCount                 // set the new list count
                                                 // EAX still contains the Self reference
                                                 // and EDX the new count (as needed for the call)
@Finish:           POP ESI
                   POP EDI
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.SetCapacity(NewCapacity: Integer);

begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListCapacityError, NewCapacity);
  {$else}
    Error(LoadStr(SListIndexError), NewCapacity);
  {$endif}

  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TTreeNTNode));
    FCapacity := NewCapacity;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNodeList.SetCount(NewCount: Integer);

begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
  {$ifdef DFS_COMPILER_3_UP}
    Error(SListCountError, NewCount);
  {$else}
    Error(LoadStr(SListIndexError), NewCount);
  {$endif}

  if NewCount > FCapacity then SetCapacity(NewCount);
  if NewCount > FCount then FillChar(FList[FCount], (NewCount - FCount) * SizeOf(TTreeNTNode), 0);
  FCount := NewCount;
end;

//------------------------------------------------------------------------------

function TTreeNodeList.GetCountNotHidden: Integer;           // [dpv]
var
   I: Integer;
begin
    Result:= 0;
    for I := 0 to FCount - 1 do
      if Items[i].FHidden = hsVisible then Inc(Result);
end;

//------------------------------------------------------------------------------

{$ifdef DFS_COMPILER_3_UP}
  procedure QuickSort(SortList: PNodeList; L, R: Integer; SCompare: TTVCompare; Data: Integer);
{$else}
  procedure QuickSort(SortList: PNodeList; L, R: Integer; SCompare: TTVCompare; Data: Longint);
{$endif}

var I, J: Integer;
    P, T: Pointer;

begin
  repeat
    I := L;
    J := R;
    P := SortList[(L + R) shr 1];
    repeat
      while SCompare(Integer(SortList[I]), Integer(P), Data) < 0 do Inc(I);
      while SCompare(Integer(SortList[J]), Integer(P), Data) > 0 do Dec(J);
      if I <= J then
      begin
        T := SortList[I];
        SortList[I] := SortList[J];
        SortList[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(SortList, L, J, SCompare, Data);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------

{$ifdef DFS_COMPILER_3_UP}
  procedure TTreeNodeList.Sort(Compare: TTVCompare; Data: Integer);
{$else}
  procedure TTreeNodeList.Sort(Compare: TTVCompare; Data: Longint);
{$endif}

begin
  if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare, Data);
end;

//----------------- TTreeNTNode ------------------------------------------------

{$ifdef DFS_COMPILER_3_UP}
  function DefaultTreeNTSort(Node1, Node2: TTreeNTNode; lParam: Integer): Integer; stdcall;
{$else}
  function DefaultTreeNTSort(Node1, Node2: TTreeNTNode; lParam: Longint): Longint; stdcall;
{$endif}

begin
  with Node1 do
    if assigned(TreeView.OnCompare)
      then TreeView.OnCompare(TreeView, Node1, Node2, lParam, Result)
      else Result := lstrcmp(PChar(Node1.Text), PChar(Node2.Text));
end;

//------------------------------------------------------------------------------

constructor TTreeNTNode.Create(AOwner: TTreeNTNodes);

begin
  inherited Create;
  // inc( _CNT_CREATE );
  FOverlayIndex := -1;
  FStateIndex := -1;
  FImageIndex := -1;
  FSelectedIndex := -1;
  FOwner := AOwner;
  FParentColor := True;
  FParentFont := True;
  FChildList := TTreeNodeList.Create;
  FUpdateCount := 0;
  FEnabled := True;
  FHidden := hsVisible;   // [dpv]
end;

//------------------------------------------------------------------------------

destructor TTreeNTNode.Destroy;

var Node : TTreeNTNode;

begin
  // inc( _CNT_FREE );
  FDeleting := True;
  FOwner.FSelection.Remove(Self);

  if Owner.Owner.FLastDropTarget = Self then Owner.Owner.FLastDropTarget := nil;
  Node := FParent;
  if assigned(Node) and not Node.Deleting then
  begin
    Node.FChildList.Remove(Self);
    if Node.FChildList.Count = 0 then
    begin
      Node.Expanded := False;
      Node.HasChildren := False;
    end;
  end;

  if ItemID <> nil  then TreeView_DeleteItem(Handle, ItemID);
  Data := nil;
  FFont.Free;
  FChildList.Free;
  Dec(FOwner.FCount);         // [dpv]
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetHandle: HWND;

begin
  Result := FOwner.FOwner.Handle;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetTreeNT: TCustomTreeNT;

begin
  Result := FOwner.FOwner;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.HasAsParent(Value: TTreeNTNode): Boolean;

begin
  if assigned(Value) then
  begin
    if Self = Value then Result := True
                    else
      if (FParent <> FOwner.FRoot) then Result := FParent.HasAsParent(Value)
                                   else Result := False;
  end
  else Result := True;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetHidden: Boolean;    // [dpv]
begin
  // Un nodo, aunque no tenga FHidden = hsHidden, si alguno de sus antecesores
  // si lo tiene, también estará oculto.
  // También, una característica de estar oculto es que ItemID = nil
  Result:= (ItemID = nil);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetMarkedHidden: Boolean;    // [dpv]
begin
    if FHidden= hsVisible then
       Result:= False
    else
       Result:= True;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetHidden(Value: Boolean);    // [dpv]
begin
  if (FHidden = hsHiding) or (Value and (FHidden = hsHidden)) or ((FHidden = hsVisible) and not Value) then
       exit;

  FOwner.BeginUpdate;
  if Value then begin
     Hide(Value, True, False);
     HasChildren := False;

      if assigned(FParent) and (FParent.FChildList.CountNotHidden = 0) then begin
         FParent.HasChildren := False;
      end;

  end
  else begin
     Hide(Value, True, False);
     if assigned(FParent) and (FParent.FChildList.CountNotHidden = 1) then begin
         FParent.HasChildren := True;
     end;
  end;
  FOwner.EndUpdate;

end;

//------------------------------------------------------------------------------
// Force: Forzar este nodo como oculto o no. Si False, sólo se hará visible si FHidden = fsVisible
// RecurseForce: Aplicar Force a todos los hijos

procedure TTreeNTNode.Hide(Hide: Boolean; Force: Boolean; RecurseForce: Boolean);    // [dpv]
var InsertStruct : TTVInsertStruct;
    _Item: TTVITem;
    nodo: TTreeNTNode;
    I: Integer;
    HiddenOld: THiddenState;
begin
  if Hide then begin
  // Ocultar
      for I := 0 to FChildList.Count - 1 do FChildList[I].Hide(Hide, Force and RecurseForce, RecurseForce);

      if Owner.Owner.FLastDropTarget = Self then Owner.Owner.FLastDropTarget := nil;
      HiddenOld:= FHidden;
      FHidden:= hsHiding;      // El nodo no se eliminará cuando se reciba el mensaje TVN_DELETEITEM tras TreeView_DeleteItem
      if assigned(ItemID) then TreeView_DeleteItem(Handle, ItemID);
      if Force then
         FHidden:= hsHidden
      else
         FHidden:= HiddenOld;             // Sólo permanecerán fsHidden los nodos ocultos de manera explícita, no por ser hijos de otros ocultos
      FItemId := nil;
  end
  else begin
      if (FHidden = hsVisible) or Force then begin // Si el nodo sólo estaba oculto por el padre, o bien se fuerza su visibilidad
      // Hacer visible
          if not assigned(ItemID) and
             (assigned(FParent) and (assigned(FParent.ItemId) or (FParent=FOwner.FRoot)) ) then begin
              _Item:= FOwner.CreateItem(Self);
              with InsertStruct do begin
                hParent := FParent.ItemId;
                Item:= _Item;
                nodo:= self.GetPrevSiblingNotHidden;
                if nodo <> nil then
                   hInsertAfter := nodo.FItemId
                else
                   hInsertAfter := TVI_FIRST;
              end;

              FItemId := TreeView_InsertItem(FOwner.Handle, InsertStruct);
              if FItemId=nil then
                {$ifdef DFS_COMPILER_3_UP}
                  raise EOutOfResources.Create(sInsertError)
                {$else}
                  raise EOutOfResources.CreateRes(sInsertError)
                {$endif}
              else begin
                  case FParent.FCheckType of
                    ctNone:
                      FOwner.FOwner.SetCheckImage(Self, ckEmpty);
                    ctCheckBoxGrayed,
                    ctCheckBox:
                      FOwner.FOwner.SetCheckImage(Self, ckCheckEmpty);
                    ctRadioButton:
                      FOwner.FOwner.SetCheckImage(Self, ckRadioEmpty);
                  end;
                  CheckState := FCheckState;
              end;
            end;
          FHidden:= hsVisible;     // Se hará visible si su padre lo es, pero independientemente se marcará como visible
      end;

      Force:= Force and RecurseForce;
      for I := 0 to FChildList.Count - 1 do FChildList[I].Hide(Hide, Force, RecurseForce);
      if FChildList.CountNotHidden > 0 then begin
         HasChildren := True;
         Expanded:= FExpanded;
      end;
  end;

end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetText(const S: String);

var Item: TTVItem;

begin
  FText := S;
  if FItemID <> nil then begin            // [dpv]
      with Item do
      begin
        Mask := TVIF_TEXT;
        hItem := ItemId;
        pszText := LPSTR_TEXTCALLBACK;
      end;
     TreeView_SetItem(Handle, Item);
  end;
  if (TreeView.SortType in [stText, stBoth]) and FInTree then
    if FParent <> FOwner.FRoot then FParent.AlphaSort
                               else TreeView.AlphaSort;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetData(Value: Pointer);

begin
  FData := Value;
  If FInTree and not Deleting then
  begin
    if (TreeView.SortType in [stData, stBoth]) and
       assigned(TreeView.OnCompare)            and
       (not FDeleting) and FInTree             then
    begin
      if FParent <> FOwner.FRoot then FParent.AlphaSort
                                 else TreeView.AlphaSort;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetState(NodeState: TNodeState): Boolean;

var Item: TTVItem;

begin
  Result := False;
  if ItemID = nil then Exit; // special case FRoot of TTreeNTNodes  // [dpv] or nodo is hidden

  with Item do
  begin
    Mask := TVIF_STATE;
    hItem := ItemId;
    if TreeView_GetItem(Handle, Item) then
      case NodeState of
        nsCut:
          Result := (state and TVIS_CUT) <> 0;
        nsFocused:
          Result := (state and TVIS_FOCUSED) <> 0;
        nsSelected:
          Result := (state and TVIS_SELECTED) <> 0;
        nsExpanded:
          Result := (state and TVIS_EXPANDED) <> 0;
        nsDropHilited:
          Result := (state and TVIS_DROPHILITED) <> 0;
        nsExpandedOnce:
          Result := (state and TVIS_EXPANDEDONCE) <> 0;
        nsExpandPartial:
          Result := (state and TVIS_EXPANDPARTIAL) <> 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetImageIndex(Value: Integer);

var Item: TTVItem;

begin
  FImageIndex := Value;
  if FItemID <> nil then begin            // [dpv]
      with Item do
      begin
        Mask := TVIF_IMAGE or TVIF_HANDLE;
        hItem := ItemId;
        iImage := I_IMAGECALLBACK;
      end;
      TreeView_SetItem(Handle, Item);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetParentColor(AValue: Boolean);

begin
  if FParentColor <> AValue then
  begin
    FParentColor := AValue;                               //*****¿?
    if not IsUpdating then FOwner.Repaint(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetParentFont(AValue: Boolean);

begin
  if FParentFont <> AValue then
  begin                                                   //*****¿?
    FParentFont := AValue;
    if FParentFont then
    begin
      FFont.Free;
      FFont := nil;
    end
    else
    begin
      FFont := TFont.Create;
      FFont.Assign(TreeView.Font);
    end;
    if not IsUpdating then FOwner.Repaint(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetSelectedIndex(Value: Integer);

var Item: TTVItem;

begin
  FSelectedIndex := Value;
  if FItemID <> nil then begin            // [dpv]
      with Item do
      begin
        Mask := TVIF_SELECTEDIMAGE or TVIF_HANDLE;
        hItem := ItemId;
        iSelectedImage := I_IMAGECALLBACK;
      end;
      TreeView_SetItem(Handle, Item);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetIntegralHeight(Value: Integer);

var Item: PTVItemEx;

begin
  if FItemID <> nil then begin            // [dpv]
      New(Item);
      try
        with Item^ do
        begin
          Mask := TVIF_INTEGRAL;
          hItem := ItemId;
          iIntegral := Value;
        end;
        TreeView_SetItem(Handle, PTVItem(Item)^);
        TreeView.Invalidate;
      finally
        Dispose(Item);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetOverlayIndex(Value: Integer);

var Item: TTVItem;

begin
  FOverlayIndex := Value;
  if FItemID <> nil then begin            // [dpv]
      with Item do
      begin
        Mask := TVIF_STATE or TVIF_HANDLE;
        stateMask := TVIS_OVERLAYMask;
        hItem := ItemId;
        state := IndexToOverlayMask(Value + 1);
      end;
      TreeView_SetItem(Handle, Item);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetStateIndex(Value: Integer);

var Item: TTVItem;

begin
  FStateIndex := Value;
  if Value >= 0 then Dec(Value);
  if FItemID <> nil then begin            // [dpv]
      with Item do
      begin
        Mask := TVIF_STATE or TVIF_HANDLE;
        stateMask := TVIS_STATEIMAGEMASK;
        hItem := ItemId;
        state := IndexToStateImageMask(Value + 1);
      end;
      TreeView_SetItem(Handle, Item);
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.DoCanExpand(Expand: Boolean): Boolean;

begin
  Result := False;
  if HasChildren then
    if Expand then Result := TreeView.CanExpand(Self)
              else Result := TreeView.CanCollapse(Self);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.DoCheckClick;

begin
  if FEnabled then
    case FParent.FCheckType of
      ctCheckBox:
        if FCheckState = csUnchecked then SetCheckState(csChecked)
                                     else SetCheckState(csUnchecked);
      ctCheckBoxGrayed :
        case FCheckState of
          csUnchecked: SetCheckState(csCheckedGrayed);
          csCheckedGrayed: SetCheckState(csChecked);
          csChecked: SetCheckState(csUnchecked);
        end;
      ctRadioButton:
        SetCheckState(csChecked);
    end
  else
    case FParent.FCheckType of
      ctCheckBox:
        if FCheckState = csUnchecked then TreeView.SetCheckImage(Self, ckCheckEmpty)
                                     else TreeView.SetCheckImage(Self, ckCheckChecked);
      ctCheckBoxGrayed :
        case FCheckState of
          csUnchecked:
            TreeView.SetCheckImage(Self, ckCheckEmpty);
          csChecked:
            TreeView.SetCheckImage(Self, ckCheckChecked);
        end;
      ctRadioButton:
        if FCheckState = csUnchecked then TreeView.SetCheckImage(Self, ckRadioEmpty)
                                     else TreeView.SetCheckImage(Self, ckRadioChecked);
    end
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.DoExpand(Expand: Boolean);

begin
  if HasChildren then
    if Expand then TreeView.Expand(Self)
              else TreeView.Collapse(Self);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.ExpandItem(Expand: Boolean; Recurse: Boolean);

var Flag : Integer;
    I    : Integer;

begin
  TreeView.FManualNotify := True;
  try
    Flag := 0;
    if Expand then
    begin
      if DoCanExpand(True) then
      begin
        Flag := TVE_EXPAND;
        DoExpand(True);
        FExpanded := True;
      end;
    end
    else
    begin
      if DoCanExpand(False) then
      begin
        Flag := TVE_COLLAPSE;
        DoExpand(False);
        FExpanded := False;
      end;
    end;
    if FItemID <> nil then                       // [dpv]
       TreeView_Expand(Handle, ItemId, Flag);
  finally
    TreeView.FManualNotify := False;
  end;

  if Recurse then
    for I := 0 to FChildList.Count - 1 do FChildList[I].ExpandItem(Expand, True);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.Expand(Recurse: Boolean);

begin
  ExpandItem(True, Recurse);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.Collapse(Recurse: Boolean);

begin
  ExpandItem(False, Recurse);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetExpanded: Boolean;

begin
  Result := FExpanded;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetExpandedOnce: Boolean;

begin
  Result := GetState(nsExpandedOnce);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetExpandedPartial: Boolean;

begin
  Result := GetState(nsExpandPartial);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetExpanded(Value: Boolean);

begin
  if Value then Expand(False)
           else Collapse(False);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetExpandedPartial(Value: Boolean);

var Item     : TTVItem;
    Template : Integer;

begin
  if FItemID = nil then exit;            // [dpv]

  if Value then Template := -1
           else Template := 0;
  with Item do
  begin
    Mask := TVIF_STATE;
    hItem := ItemId;
    stateMask := TVIS_EXPANDPARTIAL;
    state := stateMask and Template;
  end;
  TreeView_SetItem(Handle, Item);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetParent: TTreeNTNode;

begin
  if FParent <> FOwner.FRoot then Result := FParent
                             else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetSelected: Boolean;

begin
  Result := FOwner.FSelection.IndexOf(Self) > -1;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetSelected(Value: Boolean);

begin
  {if Value then TreeView_SelectItem(Handle, ItemID)
           else
    if Selected then TreeView_SelectItem(Handle, nil);}
  if Value then FOwner.SelectNode(Self, stSet)
           else FOwner.SelectNode(Self, stReset);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetCut: Boolean;

begin
  Result := GetState(nsCut);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetColor(AValue: TColor);

begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    FParentColor := False;
    if not IsUpdating
       and (FItemID <> nil) then             // [dpv]
          FOwner.Repaint(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetCut(Value: Boolean);

var Item     : TTVItem;
    Template : Integer;

begin
  if FItemID = nil then exit;            // [dpv]

  if Value then Template := -1
           else Template := 0;
  with Item do
  begin
    Mask := TVIF_STATE;
    hItem := ItemId;
    stateMask := TVIS_CUT;
    state := stateMask and Template;
  end;
  TreeView_SetItem(Handle, Item);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetDropTarget: Boolean;

begin
  Result := GetState(nsDropHilited);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetDropTarget(Value: Boolean);

begin
  if FItemID = nil then exit;            // [dpv]

  if Value then TreeView_SelectDropTarget(Handle, ItemId)
           else
    if DropTarget then TreeView_SelectDropTarget(Handle, nil);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetChildren: Boolean;

// The result doesn't reflect the real state, since the user can
// specify that an item has children, even if it hasn't one.
// Hence, not FChildList.Count is queried but what the window control
// believes to know.

var Item: TTVItem;

begin
  Result := False;
  if FItemID = nil then exit;            // [dpv]

  Item.Mask := TVIF_CHILDREN;
  Item.hItem := ItemId;
  if TreeView_GetItem(Handle, Item) then Result := Item.cChildren > 0;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetFocused(Value: Boolean);

var Item     : TTVItem;
    Template : Integer;

begin
  if FItemID = nil then exit;            // [dpv]

  if Value then Template := -1
           else Template := 0;
  with Item do
  begin
    Mask := TVIF_HANDLE or TVIF_STATE;
    hItem := ItemId;
    stateMask := TVIS_FOCUSED;
    state := stateMask and Template;
  end;
  TreeView_SetItem(Handle, Item);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetFocused: Boolean;

begin
  Result := GetState(nsFocused);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextSelected: TTreeNTNode;

var SelIndex : Integer;

begin
  Result := nil;
  with FOwner.FSelection do
    SelIndex := FOwner.FSelection.IndexOf(Self);
  if (SelIndex > -1) and (SelIndex < FOwner.FSelection.Count - 1) then Result := FOwner.FSelection[SelIndex + 1];
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextSibling: TTreeNTNode;

var AIndex : Integer;

begin
  AIndex := Index;
  if AIndex < FParent.Count - 1 then Result := FParent.Item[AIndex + 1]
                                else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetPrevSelected: TTreeNTNode;

var SelIndex : Integer;

begin
  Result := nil;
  SelIndex := FOwner.FSelection.IndexOf(Self);
  if SelIndex > 0 then Result := FOwner.FSelection[SelIndex - 1];
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetPrevSibling: TTreeNTNode;

var AIndex : Integer;

begin
  Result := nil;
  if assigned(FParent) then
  begin
    AIndex := Index;
    if AIndex > 0 then Result := FParent.Item[AIndex - 1];
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetPrevSiblingNotHidden: TTreeNTNode;    // [dpv]

var AIndex : Integer;

begin
  Result := nil;
  if assigned(FParent) then
  begin
    AIndex := Index;
    if AIndex > 0 then begin
      repeat
         AIndex := AIndex -1;
         Result := FParent.Item[AIndex];
      until not Result.Hidden or (AIndex = 0);
      if Result.Hidden then Result:= nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextSiblingNotHidden: TTreeNTNode;    // [dpv]

var AIndex : Integer;

begin
  Result := nil;
  if assigned(FParent) then
  begin
    AIndex := Index;
    if AIndex < FParent.Count - 1 then begin
      repeat
         AIndex := AIndex + 1;
         Result := FParent.Item[AIndex];
      until not Result.Hidden or (AIndex = FParent.Count-1);
      if Result.Hidden then Result:= nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextVisible: TTreeNTNode;

var I, Index : Integer;

begin
  Result := nil;
  with FOwner do
  begin
    FillCache;
    Index := FItemCache.IndexOf(Self) + 1;
    for I := Index to FItemCache.Count - 1 do
      if FItemCache[I].IsVisible then
      begin
        Result := FItemCache[I];
        Break;
      end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetPrevVisible: TTreeNTNode;

var Node : TTreeNTNode;

begin
  Node := GetPrev;
  while assigned(Node) and not Node.IsVisible do Node := Node.GetPrev;
  Result := Node;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextChild(Value: TTreeNTNode): TTreeNTNode;

begin
  if assigned(Value) and (Value.FParent = Self) then Result := Value.GetNextSibling
                                                else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetPrevChild(Value: TTreeNTNode): TTreeNTNode;

begin
  if assigned(Value) and (Value.FParent = Self) then Result := Value.GetPrevSibling
                                                else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetFirstChild: TTreeNTNode;

begin
  if FChildList.Count = 0 then Result := nil
                          else Result := FChildList[0];
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetLastChild: TTreeNTNode;

begin
  if FChildList.Count = 0 then Result := nil
                          else Result := FChildList[FChildList.Count - 1];
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNext: TTreeNTNode;

var Index : Integer;

begin
  FOwner.FillCache;
  Index := FOwner.FItemCache.IndexOf(Self) + 1;
  if Index < FOwner.FItemCache.Count then Result := FOwner.FItemCache[Index]
                                     else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetNextNotHidden: TTreeNTNode;
var
   node: TTreeNTNode;
begin
     node:= Self;
     repeat
        node := node.GetNext;
     until not assigned(node) or (not node.Hidden);
     Result:= node;
end;


//------------------------------------------------------------------------------

function TTreeNTNode.GetPrev: TTreeNTNode;

var Node : TTreeNTNode;

begin
  Result := GetPrevSibling;
  if assigned(Result) then
  begin
    Node := Result;
    repeat
      Result := Node;
      Node := Result.GetLastChild;
    until Node = nil;
  end
  else
    if FParent <> FOwner.FRoot then Result := FParent;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetAbsoluteCount: Integer;

// determine the number of all children plus the number of all of their children

var I : Integer;

begin
  Result := FChildList.Count;
  for I := 0 to FChildList.Count - 1 do Inc(Result, Item[I].AbsoluteCount);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetAbsoluteIndex: Integer;

// determines the absolute position in the entire tree

begin
  FOwner.FillCache;
  Result := FOwner.FItemCache.IndexOf(Self);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetColor: TColor;

begin
  if FParentColor then Result := TreeView.Color
                  else Result := FColor;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetIndex: Integer;

begin
  Result := FParent.FChildList.IndexOf(Self);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetFont: TFont;

begin
  if FFont = nil then
  begin
    FFont := TFont.Create;
    FFont.Assign(TreeView.Font);
    FFont.OnChange := FontChanged;
  end;
  Result := FFont;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetIntegralHeight: Integer;

var Item: PTVItemEx;

begin
  Result := 0;
  New(Item);
  try
    if assigned(ItemID) then  // special case FRoot of TTreeNTNodes
    begin
      Item.Mask := TVIF_INTEGRAL;
      Item.hItem := ItemId;
      if TreeView_GetItem(Handle, PTVItem(Item)^) then Result := Item.iIntegral;
    end;
  finally
    Dispose(Item);
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetItem(Index: Integer): TTreeNTNode;

begin
  if Index > (FChildList.Count - 1) then
  begin
    Result := nil;
    TreeNTError(SListIndexError);
  end
  else Result := FChildList[Index];
end;

//------------------------------------------------------------------------------

procedure CheckParentCheckState(Node: TTreeNTNode);

var I, CheckCount : Integer;

begin
  with Node do
  begin
    CheckCount := 0;
    for I := 0 to Count - 1 do
      if Node[I].FCheckState in [csChecked, csCheckedGrayed] then Inc(CheckCount);
    if CheckCount = 0 then
    begin
      FCheckState := csUnchecked;
      TreeView.SetCheckImage(Node, ckCheckEmpty);
    end
    else
      if CheckCount < Count then
      begin
        FCheckState := csCheckedGrayed;
        TreeView.SetCheckImage(Node, ckCheckGrayed);
      end
      else
      begin
        FCheckState := csChecked;
        TreeView.SetCheckImage(Node, ckCheckChecked);
      end;

    // recursively adjust parent of parent
    if (FParent <> FOwner.FRoot) and
       (FParent.FParent.FCheckType = ctCheckBoxGrayed) then CheckParentCheckState(FParent);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetCheckState(Value: TCheckState);

// sets the check state of the node according to the given value and the
// parent's check type

var I : Integer;

begin
  // nothing happens if the node is either disabled, not allowed to change
  // its state or the state type of its parent is ctNone
  if FEnabled and TreeView.CanCheck(Self) then
  begin
    case FParent.FCheckType of
      // standard checkbox, simply check or uncheck it
      ctCheckBox:
        begin
          if Value in [csChecked, csCheckedGrayed] then
          begin
            FCheckState := csChecked;
            TreeView.SetCheckImage(Self, ckCheckChecked);
          end
          else
          begin
            FCheckState := csUnchecked;
            TreeView.SetCheckImage(Self, ckCheckEmpty);
          end;
          // propagate state up to the parent
          if (FParent <> FOwner.FRoot) and
             (FParent.FParent.FCheckType = ctCheckBoxGrayed) then CheckParentCheckState(FParent);
        end;
      // check state change with additional consequences for check states of the children
      ctCheckBoxGrayed:
        begin
          FCheckState := Value;
          case Value of
            csUnchecked:
              TreeView.SetCheckImage(Self, ckCheckEmpty);
            csChecked:
              TreeView.SetCheckImage(Self, ckCheckChecked);
            csCheckedGrayed:
              TreeView.SetCheckImage(Self, ckCheckGrayed);
          end;
          // propagate state down to the children
          if FCheckType in [ctCheckBox, ctCheckBoxGrayed] then
          case Value of
            csUnchecked:
              for I := 0 to Count - 1 do Item[I].CheckState := csUnchecked;
            csChecked:
              for I := 0 to Count - 1 do Item[I].CheckState := csChecked;
          end;
          // propagate state up to the parent
          if (FParent <> FOwner.FRoot) and
             (FParent.FParent.FCheckType = ctCheckBoxGrayed) then CheckParentCheckState(FParent);
        end;
      // radio button check state change
      ctRadioButton:
        if Value in [csChecked, csCheckedGrayed] then
        begin
          FCheckState := csChecked;
          TreeView.SetCheckImage(Self, ckRadioChecked);
          for I := 0 to FParent.Count - 1 do
            if FParent.Item[I] <> Self then
            begin
              FParent.Item[I].FCheckState := csUnchecked;
              if FParent.Item[I].Enabled then TreeView.SetCheckImage(FParent.Item[I], ckRadioEmpty)
                                         else TreeView.SetCheckImage(FParent.Item[I], ckRadioDisabled);
            end;
        end;
    end;
    TreeView.Check(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetCheckType(Value: TCheckType);

// sets the check type for this node's children and changes the children's check image

var I : Integer;

begin
  if FCheckType <> Value then
  begin
    case Value of
      ctNone:
        for I := 0 to Count - 1 do
        begin
          Item[I].FCheckState := csUnchecked;
          TreeView.SetCheckImage(Item[I], ckEmpty);
        end;
      ctCheckBox:
        begin
          for I := 0 to Count - 1 do
          if Item[I].FCheckState = csChecked then
          begin
            if Item[I].Enabled then TreeView.SetCheckImage(Item[I], ckCheckChecked)
                               else TreeView.SetCheckImage(Item[I], ckCheckGrayed);
          end
          else
          begin
            if Item[I].Enabled then TreeView.SetCheckImage(Item[I], ckCheckEmpty)
                               else TreeView.SetCheckImage(Item[I], ckCheckDisabled);
          end;
        end;
      ctCheckBoxGrayed:
        begin
          FCheckState := csUnchecked;
          for I := 0 to Count - 1 do
            if Item[I].Enabled then TreeView.SetCheckImage(Item[I], ckCheckEmpty)
                               else TreeView.SetCheckImage(Item[I], ckCheckDisabled);
        end;
      ctRadioButton:
        begin
          for I := 0 to Count - 1 do
          begin
            Item[I].FCheckState := csUnchecked;
            if Item[I].Enabled then TreeView.SetCheckImage(Item[I], ckRadioEmpty)
                               else TreeView.SetCheckImage(Item[I], ckRadioDisabled);
          end;
          // find first enabled item to check it
          for I := 0 to Count - 1 do
            if Item[I].Enabled then
            begin
              TreeView.SetCheckImage(Item[I], ckRadioChecked);
              Item[I].FCheckState := csChecked;
              Break;
            end;
        end;
    end;
    FCheckType := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetChildren(Value: Boolean);

var Item : TTVItem;

begin
  if FItemID = nil then exit;            // [dpv]

  with Item do
  begin
    Mask := TVIF_CHILDREN;
    hItem := ItemId;
    cChildren := Ord(Value);
  end;
  TreeView_SetItem(Handle, Item);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetEnabled(Value: Boolean);

begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FParent.FCheckType <> ctNone then
    begin
      if Value then
        case StateIndex of
          ckCheckDisabled:
            TreeView.SetCheckImage(Self, ckCheckEmpty);
          ckCheckGrayed:
            if FParent.FCheckType = ctCheckBox then TreeView.SetCheckImage(Self, ckCheckChecked);
          ckRadioDisabled:
            TreeView.SetCheckImage(Self, ckRadioEmpty);
          ckRadioGrayed:
            TreeView.SetCheckImage(Self, ckRadioChecked);
        end
               else
        case StateIndex of
          ckCheckEmpty:
            TreeView.SetCheckImage(Self, ckCheckDisabled);
          ckCheckChecked:
            TreeView.SetCheckImage(Self, ckCheckGrayed);
          ckRadioEmpty:
            TreeView.SetCheckImage(Self, ckRadioDisabled);
          ckRadioChecked:
            TreeView.SetCheckImage(Self, ckRadioGrayed);
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetFont(AFont: TFont);

begin
  FParentFont := False;
  if FFont = nil then
  begin
    FFont := TFont.Create;
    FFont.OnChange := FontChanged;
  end;
  FFont.Assign(AFont);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.SetItem(Index: Integer; Value: TTreeNTNode);

begin
  Item[Index].Assign(Value);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.IndexOf(Value: TTreeNTNode): Integer;

begin
  if Value = nil then Result := -1
                 else
  Result := FChildlist.IndexOf(Value);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.IsUpdating: Boolean;

begin
  Result := (FUpdateCount > 0) or FDeleting or FOwner.IsUpdating;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.GetCount: Integer;

begin
  Result := FChildList.Count;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.BeginUpdate;

begin
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.EndUpdate;

begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.EndEdit(Cancel: Boolean);

begin
  TreeView_EndEditLabelNow(Handle, Cancel);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.InternalMove(ParentNode, Node: TTreeNTNode; HItem: HTreeItem; AddMode: TAddMode);

var I          : Integer;
    NodeId     : HTreeItem;
    TreeNTItem : TTVItem;
    Children,
    IsExpanded,
    IsSelected : Boolean;

begin
  if (AddMode = taInsert) and (Node <> nil) then NodeId := Node.ItemId
                                            else NodeId := nil;
  Children := HasChildren;
  IsSelected := Selected;
  IsExpanded := Expanded;
  // this node is already removed from its parent's child list
  if FParent.Count = 0 then
  begin
    FParent.Expanded := False;
    FParent.HasChildren := False;
  end;

  if not Hidden then begin         // [dpv]

      with TreeNTItem do
      begin
        Mask := TVIF_PARAM;
        hItem := ItemID;
        lParam := 0;
      end;

      TreeView_SetItem(Handle, TreeNTItem);

      with Owner do HItem := AddItem(HItem, NodeId, CreateItem(Self), AddMode);

      if HItem = nil then
      {$ifdef DFS_COMPILER_3_UP}
        raise EOutOfResources.Create(sInsertError);
      {$else}
        raise EOutOfResources.CreateRes(sInsertError);
      {$endif}

      for I := Count - 1 downto 0 do
          Item[I].InternalMove(Self, nil, HItem, taAddFirst);

      TreeView_DeleteItem(Handle, ItemID);

      FItemId := HItem;
  end;

  FParent := ParentNode;
  if FParent = nil then FParent := FOwner.FRoot;

  Assign(Self);
  HasChildren := Children;
  if IsSelected then FOwner.SelectNode(Self, stSet);
  Expanded := IsExpanded;
  FOwner.InvalidateItemCache;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.MoveTo(Destination: TTreeNTNode; Mode: TNodeAttachMode);

var AddMode       : TAddMode;
    LastFocused,
    Node          : TTreeNTNode;
    HItem         : HTreeItem;
    OldOnChanging : TTVChangingEvent;
    OldOnChange   : TTVChangedEvent;

begin
  if assigned(Destination) and (Destination.TreeView <> TreeView) then Exit;

  OldOnChanging := TreeView.OnChanging;
  OldOnChange := TreeView.OnChange;
  LastFocused := TreeView.Selected;
  TreeView.OnChanging := nil;
  TreeView.OnChange := nil;
  try
    if (Destination = nil) or not Destination.HasAsParent(Self) then
    begin
      AddMode := taAdd;
      if assigned(Destination) and not (Mode in [naAddChild, naAddChildFirst])
        then
        begin
          Node := Destination.FParent;
        end
        else Node := Destination;

      case Mode of
        naAdd,
        naAddChild:
          AddMode := taAdd;
        naAddFirst,
        naAddChildFirst:
          AddMode := taAddFirst;
        naInsert:
          begin
            if Destination = nil then AddMode := taAdd
                                 else AddMode := taInsert;
          end;
      end;
      if assigned(Node) then HItem := Node.ItemId
                        else HItem := nil;

      // keep child list up to date
      FParent.FChildList.Remove(Self);
      if FParent.Count = 0 then FParent.HasChildren := False;

      InternalMove(Node, Destination, HItem, AddMode);

      // keep child list up to date
      with FParent.FChildList do
        case AddMode of
          taAddFirst:
            Insert(0, Self);
          taAdd:
            Add(Self);
          taInsert:
            Insert(Destination.Index + 1, Self);
        end;

      case FParent.FCheckType of
        ctNone:
          TreeView.SetCheckImage(Self, ckEmpty);
        ctCheckBoxGrayed,
        ctCheckBox:
          TreeView.SetCheckImage(Self, ckCheckEmpty);
        ctRadioButton:
          TreeView.SetCheckImage(Self, ckRadioEmpty);
      end;

      SetCheckState(FCheckState);     // [dpv] Respetar la marca que tuviera

      FParent.HasChildren := True;
      FParent.Expanded := True;
    end;
  finally
    TreeView.Selected := LastFocused;
    TreeView.OnChanging := OldOnChanging;
    TreeView.OnChange := OldOnChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.MakeVisible;
begin
  MakeVisibilityPosible;     // [dpv]
  TreeView_EnsureVisible(Handle, ItemID);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.MakeVisibilityPosible;       // [dpv]
var Node : TTreeNTNode;

begin
  if Hidden then begin
     Hidden:= False;
     Node := FParent;
     while (Node <> FOwner.FRoot) and (Node.ItemID =nil) do begin
       Node.Hidden:= False;
       Node := Node.FParent;
     end;
  end;
end;


//------------------------------------------------------------------------------

function TTreeNTNode.GetLevel: Integer;

var Node : TTreeNTNode;

begin
  Result := 0;
  Node := FParent;
  while Node <> FOwner.FRoot do
  begin
    Inc(Result);
    Node := Node.FParent;
  end;
end;

//------------------------------------------------------------------------------
// [dpv] ¿Es visible y seleccionable porque todos sus antecesores están expandidos?

function TTreeNTNode.IsNodeVisible: Boolean;

var AParent : TTreeNTNode;

begin
  if FItemID = nil then        // [dpv]
     Result:= False
  else begin
      AParent := FParent;
      while assigned(AParent) and AParent.FExpanded do AParent := AParent.FParent;
      if assigned(AParent) then Result := False
                           else Result := True;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.EditText: Boolean;

begin
  if FItemID = nil then     // [dpv]
     Result:= False         // [dpv]
  else
     Result := TreeView_EditLabel(Handle, ItemID) <> 0;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.DisplayRect(TextOnly: Boolean): TRect;

begin
  if FItemID = nil then exit;            // [dpv]

  FillChar(Result, SizeOf(Result), 0);
  TreeView_GetItemRect(Handle, ItemID, Result, TextOnly);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.AlphaSort: Boolean;

begin
  Result := CustomSort(nil, 0);
end;

//------------------------------------------------------------------------------

{$ifdef DFS_COMPILER_3_UP}
  function DefaultListSort(Item1, Item2, Data: Integer): Integer; stdcall;
{$else}
  function DefaultListSort(Item1, Item2, Data: Longint): Longint; stdcall;
{$endif}

// Because each node maintains an own child list, it must sort
// it depending on the sort type of the tree.

begin
  if assigned(TTreeNTNode(Item1).TreeView.FOnCompare)
    then TTreeNTNode(Item1).TreeView.FOnCompare(TTreeNTNode(Item1).TreeView, TTreeNTNode(Item1), TTreeNTNode(Item2), Data, Result)
    else
      case TTreeNTNode(Item1).TreeView.SortType of
        stData:
          Result := Integer(TTreeNTNode(Item1).FData) - Integer(TTreeNTNode(Item2).FData);
        stBoth:
          begin
            Result := CompareText(TTreeNTNode(Item1).FText, TTreeNTNode(Item2).FText);
            if Result = 0 then Result := Integer(TTreeNTNode(Item1).FData) - Integer(TTreeNTNode(Item2).FData);
          end;
      else
        // if no sort type is specified (or stText is set) then sort by text
        Result := CompareText(TTreeNTNode(Item1).FText, TTreeNTNode(Item2).FText);
      end;
end;

//------------------------------------------------------------------------------

function TTreeNTNode.CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;

var SortCB : TTVSortCB;

begin
  with SortCB do
  begin
    if assigned(SortProc) then lpfnCompare := SortProc
                          else lpfnCompare := @DefaultTreeNTSort;
    hParent := ItemId;
    lParam := Data;
  end;
  Result := TreeView_SortChildrenCB(Handle, SortCB, 0);

  // since each node maintains its own child list, it must sort
  // it explicitely
  if assigned(SortProc) then FChildList.Sort(SortProc, Data)
                        else FChildList.Sort(DefaultListSort, Data);

  // the item cache is no longer valid
  FOwner.InvalidateItemCache;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.Delete;

begin
  // First we assure that this node and all the children are visible, so the delete mechanism works ok
  if FItemId <> nil then
     Hide(false, true, true);

  if not FDeleting then Free;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.DeleteChildren;

begin
  if (Count > 0)
    and (FItemID <> nil)             // [dpv]
    then TreeView_Expand(TreeView.Handle, ItemID, TVE_COLLAPSE or TVE_COLLAPSERESET);
  //  then TreeView_Expand(Handle, ItemID, TVE_COLLAPSE or TVE_COLLAPSERESET);
  HasChildren := False;
  FChildList.Clear;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.Assign(Source: TPersistent);

var Node : TTreeNTNode;

begin
  if Source is TTreeNTNode then
  begin
    Node := TTreeNTNode(Source);
    Text := Node.Text;
    Data := Node.Data;
    ImageIndex := Node.ImageIndex;
    SelectedIndex := Node.SelectedIndex;
    StateIndex := Node.StateIndex;
    CheckType := Node.CheckType;
    CheckState := Node.CheckState;
    Enabled := Node.Enabled;
    OverlayIndex := Node.OverlayIndex;
    Expanded := Node.Expanded;
    IntegralHeight := Node.IntegralHeight;
    HasChildren := Node.HasChildren;
    FParentColor := Node.FParentColor;
    FHidden := Node.FHidden;                        // [dpv]
    if not FParentColor then FColor := Node.Color;
    FParentFont := Node.FParentFont;
    if not FParentFont then
    begin
      if FFont = nil then FFont := TFont.Create
                     else FFont.OnChange := nil;
      FFont.Assign(Node.FFont);
      FFont.OnChange := FontChanged;
    end
    else
    begin
      FFont.Free;
      FFont := nil;
    end;
    // not assigned properties:
    // Focused
    // Selected
    // DropTarget
    // Cut
  end
  else inherited Assign(Source);
end;

//------------------------------------------------------------------------------

function TTreeNTNode.IsEqual(Node: TTreeNTNode): Boolean;

begin
  Result := (Text = Node.Text) and (Data = Node.Data);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.FontChanged(Sender: TObject);

begin
  FParentFont := False;
  if FItemID <> nil then             // [dpv]
     FOwner.Repaint(Self);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.ReadData(Stream: TStream; Info: PNodeInfo);

var I, Size    : Integer;
    IsExpanded : Boolean;

begin
  Stream.ReadBuffer(Size, SizeOf(Size));
  Stream.ReadBuffer(Info^, Size);
  Text := Info.Text;
  if Info.Hidden = hsHidden then      // [dpv]
    Hidden:= True
  else
    Hidden:= False;
  ImageIndex := Info.ImageIndex;
  SelectedIndex := Info.SelectedIndex;
  StateIndex := Info.StateIndex;
  CheckType := Info.CheckType;
  CheckState := Info.CheckState;
  Enabled := Info.Enabled;
  if Info.Selected then FOwner.SelectNode(Self, stSet);
  IsExpanded := Info.Expanded;
  OverlayIndex := Info.OverlayIndex;
  IntegralHeight := Info.IntegralHeight;
  Data := Info.Data;
  FParentColor := Info.ParentColor;
  if not FParentColor then FColor := Info.Color;
  FParentFont := Info.ParentFont;
  if not FParentFont then
  begin
    FFont := TFont.Create;
    FFont.Height := Info.FontData.Height;
    FFont.Name := Info.FontData.Name;
    FFont.Pitch := Info.FontData.Pitch;
    FFont.Style := Info.FontData.Style;
    FFont.Color := Info.FontColor;
    FFont.OnChange := FontChanged;
  end;
  for I := 0 to Info.Count - 1 do with Owner.AddChild(Self, '') do ReadData(Stream, Info);
  if not (toSingleExpand in TreeView.Options) then Expanded := IsExpanded;
end;


//------------------------------------------------------------------------------

function TTreeNTNode.ReadStrings(var S: PChar; CurrentLevel: Integer): Integer;

// decomposes the provided string and builds new nodes from it

var NewLevel   : Integer;
    NewText    : String;
    NewChild   : TTreeNTNode;
    RaiseException: Boolean;

  function GetString(var S: PChar): String;

  var Start : PChar;

  begin
    Start := S;
    while not (S^ in [#0, #10, #13]) do Inc(S);
    SetString(Result, Start, S - Start);
    while (S^ <> #0) and (S^ in [#13, #10]) do Inc(S);
  end;

begin
  NewChild := nil;
  repeat
    NewLevel := 0;
    NewText := GetString(S);
    Inc(CurrentLine);
    if Length(NewText) > 0 then NewChild := FOwner.AddChild(Self, NewText)
                           else Break;
    while (S^ <> #0) and (S^ in [' ', #9]) do
    begin
      Inc(S);
      Inc(NewLevel);
    end;
    if NewLevel - CurrentLevel > 1 then
    begin
      RaiseException := True;
      if Assigned(Owner.Owner.FOnLevelError) then
        Owner.Owner.FOnLevelError(CurrentLine, RaiseException);
      if RaiseException then
        TreeNTError(sInvalidLevel);
    end;
    if (NewLevel > CurrentLevel) and assigned(NewChild) then NewLevel := NewChild.ReadStrings(S, NewLevel);
  until NewLevel < CurrentLevel;
  Result := NewLevel;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.WriteData(Stream: TStream; Info: PNodeInfo);

var I, Size, L : Integer;

begin
  L := Length(Text);
  if L > 255 then L := 255;
  Size := SizeOf(TNodeInfo) + L - 255;
  Info.Text := Text;
  Info.ImageIndex := ImageIndex;
  Info.SelectedIndex := SelectedIndex;
  Info.OverlayIndex := OverlayIndex;
  Info.StateIndex := StateIndex;
  Info.IntegralHeight := IntegralHeight;
  Info.CheckType := CheckType;
  Info.CheckState := CheckState;
  Info.Enabled := Enabled;
  Info.Expanded := FExpanded;
  Info.Hidden := FHidden;                 // [dpv]
  if csDesigning in (FOwner.FOwner.ComponentState) then Info.Selected := False
                                                   else Info.Selected := FOwner.FSelection.IndexOf(Self) > -1;
  Info.Data := Data;
  Info.Color := FColor;
  Info.ParentFont := FParentFont;
  Info.ParentColor := FParentColor;
  if not FParentFont then
  begin
    Info.FontData.Height := FFont.Height;
    Info.FontData.Name := FFont.Name;
    Info.FontData.Pitch := FFont.Pitch;
    Info.FontData.Style := FFont.Style;
    Info.FontColor := FFont.Color;
  end;
  Info.Count := Count;
  Stream.WriteBuffer(Size, SizeOf(Size));
  Stream.WriteBuffer(Info^, Size);
  for I := 0 to Info.Count - 1 do Item[I].WriteData(Stream, Info);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNode.WriteStrings(Stream: TStream; Level: Integer);

// writes the node's text plus the texts of its children into the provided
// stream (replaces therefore the much slower save-to-disk solution of TTreeView)

var I      : Integer;
    Buffer : String;

begin
  if Hidden then exit;      // [dpv]

  if Level > 0 then
  begin
    Buffer := '';
    for I := 0 to Level - 1 do Buffer := Buffer + #9;
    Stream.Write(PChar(Buffer)^, Length(Buffer));
  end;

  Stream.Write(PChar(FText)^, Length(FText));

  Buffer := #13#10;
  Stream.Write(PChar(Buffer)^, 2);

  for I := 0 to Count - 1 do Item[I].WriteStrings(Stream, Level + 1);
end;

//----------------- TTreeNTNodes -----------------------------------------------

constructor TTreeNTNodes.Create(AOwner: TCustomTreeNT);

begin
  inherited Create;
  FOwner := AOwner;
  FRoot := TTreeNTNode.Create(Self);
  FRoot.FExpanded := True; // expanded state is used for visibility tests
  FSelection := TTreeNodeList.Create;
  FLastSelLevel := -1;
end;

//------------------------------------------------------------------------------

destructor TTreeNTNodes.Destroy;

begin
  Clear;
  FRoot.Free;
  FSelection.Free;
  inherited Destroy;
end;

// [dpv]
//------------------------------------------------------------------------------

function TTreeNTNodes.GetCount: Integer;

begin
  //if FOwner.HandleAllocated then Result := TreeView_GetCount(Handle)
  //                          else Result := 0;
    Result := FCount;
end;


//------------------------------------------------------------------------------

function TTreeNTNodes.GetCountNotHidden: Integer;

begin
  if FOwner.HandleAllocated then Result := TreeView_GetCount(Handle)
                            else Result := 0;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetHandle: HWND;

// returns window handle of the owner tree

begin
  Result := Owner.Handle;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.Delete(Node: TTreeNTNode);

// deletes the given node from the tree

begin
  if (Node.ItemId = nil) and assigned(Owner.FOnDeletion) then FOwner.FOnDeletion(Self, Node);

  // First we assure that this node and all the children are visible, so the delete mechanism works ok
  if Node.ItemId <> nil then
     Node.Hide(false, true, true);
  Node.Delete;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.DeleteSelectedNodes;

// deletes all nodes which are in the current selection

var I    : Integer;
    List : TTreeNodeList;

begin
  List := TTreeNodeList.Create;
  try
    with FOwner do
    if assigned(FFirstSelection) and
       (FSelection.Indexof(FFirstSelection) > -1 ) then FFirstSelection := nil;
    // make a copy of the current selection to avoid interferences
    // with internal selection handling
    List.Count := FSelection.Count;
    Move(FSelection.List^, List.List^, FSelection.Count * SizeOf(Pointer));
    for I := 0 to List.Count - 1 do begin
        List[I].Delete;
    end;
  finally
    // if the tree becomes empty after deletion no change notification is sent,
    // so do it now
    if CountNotHidden = 0 then FOwner.DoChange(nil);
    InvalidateItemCache;
    List.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.Clear;

var I: Integer;

begin
  if (Owner.HandleAllocated) and (Count > 0) then
  try
    FDeleting := True;
    FRoot.FDeleting := True;
    for I := FRoot.FChildList.Count - 1 downto 0 do
    begin
      FRoot.FChildList[I].DeleteChildren;
      FRoot.FChildList[I].Free;
    end;
    FRoot.FChildList.Clear;
    InvalidateItemCache;
  finally
    FRoot.FDeleting := False;
    FDeleting := False;
    FCount:= 0;                    // [dpv]
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddChildFirst(Node: TTreeNTNode; const S: String): TTreeNTNode;

begin
  Result := InternalAddObject(Node, S, nil, taAddFirst);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddChildObjectFirst(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;

begin
  Result := InternalAddObject(Node, S, Ptr, taAddFirst);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddChild(Node: TTreeNTNode; const S: String): TTreeNTNode;

begin
  Result := InternalAddObject(Node, S, nil, taAdd);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddChildObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;

begin
  Result := InternalAddObject(Node, S, Ptr, taAdd);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddFirst(Node: TTreeNTNode; const S: String): TTreeNTNode;

begin
  if Node <> nil then Node := Node.FParent;
  Result := InternalAddObject(Node, S, nil, taAddFirst);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddObjectFirst(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;

begin
  if Node <> nil then Node := Node.FParent;
  Result := InternalAddObject(Node, S, Ptr, taAddFirst);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.Add(Node: TTreeNTNode; const S: String): TTreeNTNode;

begin
  if Node <> nil then Node := Node.FParent;
  Result := InternalAddObject(Node, S, nil, taAdd);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;

begin
  if Node <> nil then Node := Node.FParent;
  Result := InternalAddObject(Node, S, Ptr, taAdd);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.Repaint(Node: TTreeNTNode);

var R : TRect;

begin
  if FUpdateCount = 0 then
  begin
    while assigned(Node) and not Node.IsVisible do Node := Node.FParent;
    if assigned(Node) then
    begin
      R := Node.DisplayRect(False);
      InvalidateRect(Owner.Handle, @R, True);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.Insert(Node: TTreeNTNode; const S: String): TTreeNTNode;

begin
  Result := InsertObject(Node, S, nil);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.AddedNode(ParentNode: TTreeNTNode);

begin
  Inc(FCount);                       // [dpv]
  if ParentNode <> FRoot then
  begin
    ParentNode.HasChildren := True;
    Repaint(ParentNode);
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.InsertObject(Node: TTreeNTNode; const S: String; Ptr: Pointer): TTreeNTNode;

var Item,
    ItemID     : HTreeItem;
    ParentNode : TTreeNTNode;
    AddMode    : TAddMode;

begin
  Result := Owner.CreateNode;
  try
    ParentNode := nil;
    Item := nil;
    ItemId := nil;
    AddMode := taInsert;
    if assigned(Node) then
    begin
      ParentNode := Node.FParent;
      if assigned(ParentNode) then Item := ParentNode.ItemId;
      Node := Node.GetPrevSiblingNotHidden;
      if assigned(Node) then ItemId := Node.ItemId
                        else AddMode := taAddFirst;
    end;

    Result.Data := Ptr;
    Result.Text := S;
    Item := AddItem(Item, ItemID, CreateItem(Result), AddMode);
    if Item = nil then
    {$ifdef DFS_COMPILER_3_UP}
      raise EOutOfResources.Create(sInsertError);
    {$else}
      raise EOutOfResources.CreateRes(sInsertError);
    {$endif}

    Result.FItemId := Item;
    AddedNode(ParentNode);
    Result.FParent := ParentNode;
    if Result.FParent = nil then Result.FParent := FRoot;

    with Result do
    begin
      if ParentNode = nil then ParentNode := FRoot;

      with ParentNode do
        case AddMode of
          taAddFirst:
            FChildList.Insert(0, Result);
          taAdd:
            FChildList.Add(Result);
          taInsert:
            FChildList.Insert(Node.Index + 1, Result);
        end;

    end;
    case ParentNode.FCheckType of
      ctNone:
        FOwner.SetCheckImage(Result, ckEmpty);
      ctCheckBoxGrayed,
      ctCheckBox:
        FOwner.SetCheckImage(Result, ckCheckEmpty);
      ctRadioButton:
        FOwner.SetCheckImage(Result, ckRadioEmpty);
    end;
    InvalidateItemCache;
    FOwner.DoChange(Result);
  except
    Result.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.InvalidateItemCache;

begin
  if assigned(FItemCache) then
  begin
    FItemCache.Free;
    FItemCache := nil;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.IsDeleting: Boolean;

begin
  Result := FDeleting;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.IsUpdating: Boolean;

begin
  Result := FUpdateCount > 0;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.LockSelection: Integer;

begin
  Inc(FSelLockCount);
  Result := FSelLockCount;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.UnlockSelection: Integer;

begin
  if FSelLockCount > 0 then Dec(FSelLockCount);
  Result := FSelLockCount;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.ToggleSelectionRange(NodeFrom, NodeTo: TTreeNTNode);

// adds the given range to the current selection, excluding the given nodes

var GoForward : Boolean;

begin
  // no range to toggle if the limits are the same
  if NodeFrom = NodeTo then Exit;

  FCoreSelecting := True;

  // determine loop direction
  if NodeFrom.AbsoluteIndex < NodeTo.AbsoluteIndex then GoForward := True
                                                   else GoForward := False;

  // leave alone the start node
  if GoForward then NodeFrom := NodeFrom.GetNextVisible
               else NodeFrom := NodeFrom.GetPrevVisible;

  // can be unassigned in case of the very first or last node
  if assigned(NodeFrom) then
  begin
    // go through the (visible) items
    while NodeFrom <> NodeTo do
    begin
      // toggle selection depending on whether item
      // is within the range
      SelectNode(NodeFrom, stToggle);
      if GoForward then NodeFrom := NodeFrom.GetNextVisible
                   else NodeFrom := NodeFrom.GetPrevVisible;
    end;
  end;
  FCoreSelecting := False;

  with FOwner.FChangeTimer do
    if Interval > 0 then
    begin
      Enabled := False;
      Tag := Integer(NodeTo);
      Enabled := True;
    end
    else FOwner.DoChange(NodeTo);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.ReadStrings(Stream: TStream);

var Size   : Integer;
    Start,
    S      : PChar;

begin
  Size := Stream.Size - Stream.Position;
  if Size > 0 then
  begin
    S := AllocMem(Size + 1);
    try
      Stream.Read(S^, Size);
      Start := S;
      CurrentLine := 1;
      FRoot.ReadStrings(Start, 0);
    finally
      FreeMem(S);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.RemoveFromSelection(Node: TTreeNTNode);

begin
  if FSelLockCount = 0 then FSelection.Remove(Node);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.WriteStrings(Stream: TStream);

var I : Integer;

begin
  if FRoot.Count > 0 then
    for I := 0 to FRoot.Count - 1 do
        if not FRoot[I].Hidden  then          // [dpv]
           FRoot[I].WriteStrings(Stream, 0);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.AddToSelection(Node: TTreeNTNode);

// adds the given node to the selection list if it isn't already there

begin
  if (FSelLockCount = 0) and
     (FSelection.IndexOf(Node) = -1) then FSelection.Add(Node)
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.InternalAddObject(Node: TTreeNTNode; const S: String; Ptr: Pointer; AddMode: TAddMode): TTreeNTNode;

var Item       : HTreeItem;
    ParentNode : TTreeNTNode;

begin
  ParentNode := Node;
  if ParentNode = nil then ParentNode := FRoot;

  Result := Owner.CreateNode;
  try
    if assigned(Node) then Item := Node.ItemId
                      else Item := nil;

    if (assigned(ParentNode.ItemId) or (ParentNode=FRoot) ) then begin  // [dpv]
        Result.Data := Ptr;
        Result.Text := S;
        Item := AddItem(Item, nil, CreateItem(Result), AddMode);

        if Item = nil then
        {$ifdef DFS_COMPILER_3_UP}
          raise EOutOfResources.Create(sInsertError);
        {$else}
          raise EOutOfResources.CreateRes(sInsertError);
        {$endif}
    end;

    Result.FItemId := Item;
    Result.FParent := ParentNode;
    AddedNode(ParentNode);

    with ParentNode do
      case AddMode of
        taAddFirst:
          FChildList.Insert(0, Result);
        taAdd:
          FChildList.Add(Result);
        taInsert:
          FChildList.Insert(Node.Index + 1, Result);
      end;
    case ParentNode.FCheckType of
      ctNone:
        FOwner.SetCheckImage(Result, ckEmpty);
      ctCheckBoxGrayed,
      ctCheckBox:
        FOwner.SetCheckImage(Result, ckCheckEmpty);
      ctRadioButton:
        FOwner.SetCheckImage(Result, ckRadioEmpty);
    end;
    InvalidateItemCache;
    FOwner.DoChange(Result);
  except
    Result.Free;
    raise;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.CreateItem(Node: TTreeNTNode): TTVItem;

begin
  with Result do
  begin
    Mask := TVIF_TEXT or TVIF_PARAM or TVIF_IMAGE or
          TVIF_SELECTEDIMAGE or TVIF_CHILDREN;
    lParam := Longint(Node);
    pszText := LPSTR_TEXTCALLBACK;
    iImage := I_IMAGECALLBACK;
    iSelectedImage := I_IMAGECALLBACK;
    cChildren := 0;
  end;
  Node.FInTree := True;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.AddItem(Parent, Target: HTreeItem; const Item: TTVItem; AddMode: TAddMode): HTreeItem;

var InsertStruct : TTVInsertStruct;

begin
  InvalidateItemCache;

  with InsertStruct do
  begin
    hParent := Parent;
    case AddMode of
      taAddFirst:
        hInsertAfter := TVI_FIRST;
      taAdd:
        hInsertAfter := TVI_LAST;
      taInsert:
        hInsertAfter := Target;
    end;
  end;
  InsertStruct.Item := Item;
  FOwner.FChangeTimer.Enabled := False;
  Result := TreeView_InsertItem(Handle, InsertStruct);
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.FindNode(Flags: TFindFlags; AText: String; AData: Pointer): TTreeNTNode;

// searchs for the first node matching one or both AText and AData (depending on the passed flags)

begin
  Result := GetFirstNode;
  if Flags = [ffText] then
  begin
    while assigned(Result) do
    begin
      if CompareText(Result.FText, AText) = 0 then Break;
      Result := Result.GetNext;
    end;
  end
  else
    if Flags = [ffData] then
    begin
      while assigned(Result) do
      begin
        if Result.Data = AData then Break;
        Result := Result.GetNext;
      end;
    end
    else
      if Flags = [ffText, ffData] then
      begin
        while assigned(Result) do
        begin
          if (CompareText(Result.FText, AText) = 0) and
             (Result.Data = AData)                 then Break;
          Result := Result.GetNext;
        end;
      end
      else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetFirstNode: TTreeNTNode;

begin
  if FRoot.Count > 0 then Result := FRoot[0]
                     else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetFirstSelectedNode: TTreeNTNode;

begin
  if FSelection.Count > 0 then Result := FSelection[0]
                          else Result := nil;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetNodeFromIndex(Index: Integer): TTreeNTNode;

// return node at (absolute) index Index

begin
  FillCache;
  Result := FItemCache[Index];
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetNode(ItemID: HTreeItem): TTreeNTNode;

var Item : TTVItem;

begin
  Result := nil;
  if ItemID = nil then Exit; // special case: FRoot      // [dpv] or is hidden

  with Item do
  begin
    hItem := ItemId;
    Mask := TVIF_PARAM;
  end;
  if TreeView_GetItem(Handle, Item) then Result := TTreeNTNode(Item.lParam);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.FillCache;

// fill item cache to speed up retrieving nodes, if not already done

  procedure AddToCache(Node: TTreeNTNode);

  var I : Integer;

  begin
    if assigned(Node) then
      for I := 0 to Node.FChildList.Count - 1 do
      begin
        FItemCache.Add(Node.FChildList[I]);
        AddToCache(Node.FChildList[I]);
      end;
  end;

var I : Integer;

begin
  if FItemCache = nil then
  begin
    FItemCache := TTreeNodeList.Create;
    FItemCache.Capacity := Count;
    if FRoot.Count > 0 then // [MJ]
    for I := 0 to FRoot.Count - 1 do
    begin
      FItemCache.Add(FRoot[I]);
      AddToCache(FRoot[I]);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TTreeNTNodes.GetSelectedCount: Integer;

begin
  Result := FSelection.Count;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SelectAll;

// selects all nodes in the tree (even unvisible ones)
// [dpv] Hidden nodes are not selected

var I : Integer;
    n: Integer;
begin
  FillCache;
  if FItemCache.Count > 0 then
  begin
    n:= 0;
    FCoreSelecting := True;
    for I := 0 to FItemCache.Count - 1 do
       if not FItemCache[I].Hidden then begin          // [dpv]
         SelectNode(FItemCache[I], stSet);
         Inc(n);
       end;
    FCoreSelecting := False;

    with FOwner.FChangeTimer do
      if Interval > 0 then
      begin
        Enabled := False;
        //Tag := Integer(FItemCache[FItemCache.Count - 1]);
        Tag:= Integer(FItemCache[n]);        // [dpv]
        Enabled := True;
      end
      else //FOwner.DoChange(FItemCache[FItemCache.Count - 1]);
            FOwner.DoChange(FItemCache[n]);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SelectAllPlusHidden;              // [dpv]

// selects all nodes in the tree (even unvisible ones). Also, hidden nodes are selected

var I : Integer;

begin
  FillCache;
  if FItemCache.Count > 0 then
  begin
    FCoreSelecting := True;
    for I := 0 to FItemCache.Count - 1 do SelectNode(FItemCache[I], stSet);
    FCoreSelecting := False;

    with FOwner.FChangeTimer do
      if Interval > 0 then
      begin
        Enabled := False;
        Tag := Integer(FItemCache[FItemCache.Count - 1]);
        Enabled := True;
      end
      else FOwner.DoChange(FItemCache[FItemCache.Count - 1]);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SelectNode(Node: TTreeNTNode; Value: TSelectType);

// another procedure to select or deselect an item, but it doesn't change
// the selection state of any other node (used for multi-selection)

// Since this method is the fast brother of 'TreeView_SelectItem', we don't get
// a TVN_SELCHANGING or TVN_SELCHANGED message, so we have to simulate these
// messages from here.

var Item     : TTVItem;
    Template : Integer;

begin
  if not Owner.CanChange(Node) then Exit;
  case Value of
    stSet:
      Template := -1;
    stReset:
      Template := 0;
  else
    // stToggle
    if Node.Selected then Template := 0
                     else Template := -1;
  end;

  if Template = -1 then
  begin
    if (toLevelSelectConstraint in Owner.FOptions) and (Node.Level <> FLastSelLevel) then Exit;
    AddToSelection(Node);
  end
  else RemoveFromSelection(Node);

  if Node.FItemId <> nil then begin       // [dpv]
      with Item do
      begin
        Mask := TVIF_STATE;
        hItem := Node.ItemId;
        stateMask := TVIS_SELECTED;
        state := stateMask and Template;
      end;
      TreeView_SetItem(Handle, Item);
  end;

  if not FCoreSelecting then
    with FOwner.FChangeTimer do
    if Interval > 0 then
    begin
      Enabled := False;
      Tag := Integer(Node);
      Enabled := True;
    end
    else FOwner.DoChange(Node);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SelectNodes(NodeFrom, NodeTo: TTreeNTNode; AddOnly: Boolean);

// selects a range of (visible) nodes; if AddOnly is True then the last selection
// will not be cleared (useful for several ranges)

var Node        : TTreeNTNode;
    TempList,
    NewNodes    : TTreeNodeList;
    I           : Integer;

begin
  FCoreSelecting := True;
  NewNodes := TTreeNodeList.Create;
  TempList := TTreeNodeList.Create;
  try
    if NodeFrom.AbsoluteIndex > NodeTo.AbsoluteIndex then
    begin
      Node := NodeFrom;
      NodeFrom := NodeTo;
      NodeTo := Node;
    end;

    // what we need here is a bit set maths; the task is to find the nodes to unselect
    // by: unselect := selected - new nodes and the nodes newly to select
    // by: new select := new nodes - selected
    // first step is to build the list of new nodes:
    Node := NodeFrom;
    if assigned(Node) then
      repeat
        NewNodes.Add(Node);
        if Node = NodeTo then Break;
        Node := Node.GetNextVisible;
      until Node = nil;

    if NewNodes.Count > 0 then
    begin
      // next: find selected nodes not in new nodes and unselected nodes in new nodes
      if not AddOnly then
        for I := 0 to FSelection.Count - 1 do
          if NewNodes.IndexOf(FSelection[I]) = -1 then TempList.Add(FSelection[I]);

      for I := 0 to NewNodes.Count - 1 do
        if FSelection.IndexOf(NewNodes[I]) > -1 then NewNodes[I] := nil;
      NewNodes.Pack;

      // finally do selection
      if not AddOnly then
        for I := 0 to TempList.Count - 1 do SelectNode(TempList[I], stReset);
      for I := 0 to NewNodes.Count - 1 do SelectNode(NewNodes[I], stSet);
    end;
  finally
    TempList.Free;
    NewNodes.Free;
    FCoreSelecting := False;

    with FOwner.FChangeTimer do
      if Interval > 0 then
      begin
        Enabled := False;
        Tag := Integer(NodeTo);
        Enabled := True;
      end
      else FOwner.DoChange(NodeTo);
  end;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SetItem(Index: Integer; Value: TTreeNTNode);

begin
  GetNodeFromIndex(Index).Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.BeginUpdate;

begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.SetUpdateState(Updating: Boolean);

begin
  SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if Updating then Owner.Refresh;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.EndUpdate;

begin
  if FUpdateCount > 0 then Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.Assign(Source: TPersistent);

var TreeNTNodes : TTreeNTNodes;
    MemStream  : TMemoryStream;

begin
  if Source is TTreeNTNodes then
  begin
    Clear;
    MemStream := TMemoryStream.Create;
    try
      BeginUpdate;
      TreeNTNodes := TTreeNTNodes(Source);
      TreeNTNodes.WriteData(MemStream);
      MemStream.Position := 0;
      ReadData(MemStream);
    finally
      MemStream.Free;
      EndUpdate;
    end;
  end
  else inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.DefineProperties(Filer: TFiler);

  function WriteNodes: Boolean;

  var I     : Integer;
      Nodes : TTreeNTNodes;

  begin
    Result := False;
    Nodes := TTreeNTNodes(Filer.Ancestor);
    if (Nodes <> nil) and (Nodes.Count = Count) then
      for I := 0 to Count - 1 do
      begin
        Result := not Item[I].IsEqual(Nodes[I]);
        if Result then Break;
      end
    else Result := Count > 0;
    Result := Result or (TopLevelCheckType <> ctNone);
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNodes);
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.ReadData(Stream: TStream);

var I,
    Count     : Integer;
    NodeInfo  : TNodeInfo;
    CheckType : TCheckType;

begin
  Clear;
  try
    BeginUpdate;
    Stream.ReadBuffer(CheckType, SizeOf(CheckType));
    SetCheckType(CheckType);
    Stream.ReadBuffer(Count, SizeOf(Count));
    for I := 0 to Count - 1 do
      with Add(nil, '') do ReadData(Stream, @NodeInfo);
  finally
    EndUpdate;
  end;
end;


//------------------------------------------------------------------------------

procedure TTreeNTNodes.SetCheckType(Value: TCheckType);

begin
  FRootCheckType := Value;
  FRoot.CheckType := Value;
end;

//------------------------------------------------------------------------------

procedure TTreeNTNodes.WriteData(Stream: TStream);

var I        : Integer;
    Node     : TTreeNTNode;
    NodeInfo : TNodeInfo;

begin
  Stream.WriteBuffer(FRootCheckType, SizeOf(FRootCheckType));
  I := FRoot.Count;
  Stream.WriteBuffer(I, SizeOf(I));
  Node := GetFirstNode;
  while Node <> nil do
  begin
    Node.WriteData(Stream, @NodeInfo);
    Node := Node.GetNextSibling;
  end;
end;

//----------------- TCustomTreeNT ----------------------------------------------

constructor TCustomTreeNT.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse] + [csDisplayDragImage {$ifdef DFS_COMPILER_3_UP}, csReflector{$endif} ];
  Width := 121;
  Height := 97;
  TabStop := True;
  ParentColor := False;
  FTreeNTNodes := TTreeNTNodes.Create(Self);

  // auto expand collapsed node if it is the drag target for >= 1 seconds
  FExpandTimer := TTimer.Create(Self);
  FExpandTimer.Enabled := False;
  FExpandTimer.Interval := 1000;
  FExpandTimer.OnTimer := DoDragExpand;

  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.OnTimer := DoScroll;
  FScrollTimer.Interval := 300;

  FChangeTimer := TTimer.Create(Self);
  FChangeTimer.Enabled := False;
  FChangeTimer.Interval := 0;
  FChangeTimer.OnTimer := OnChangeTimer;

  FBorderStyle := bsSingle;
  FDragImage := TImageList.CreateSize(32, 32);
  FSaveIndent := -1;
  FSaveItemHeight := -1;
  FOptions := DefaultOptions;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

  FEditInstance := MakeObjectInstance(EditWndProc);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FStateChangeLink := TChangeLink.Create;
  FStateChangeLink.OnChange := ImageListChange;

  FColorSelected := clHighlight;
  FColorDropSelected := clBtnFace;
  FColorUnfocusedSelected := cl3DLight;
end;

//------------------------------------------------------------------------------

destructor TCustomTreeNT.Destroy;

begin
  FCanvas.Free;
  FChangeTimer.Free;
  FExpandTimer.Free;
  FScrollTimer.Free;
  FDragImage.Free;
  FMemStream.Free;
  FreeObjectInstance(FEditInstance);
  FImageChangeLink.Free;
  FStateChangeLink.Free;
  inherited Destroy;
  FTreeNTNodes.Free;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CreateParams(var Params: TCreateParams);

begin
  {$ifdef DFS_COMPILER_3_UP}
    InitCommonControl(ICC_TREEVIEW_CLASSES);
  {$else}
    InitCommonControls;
  {$endif}
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TREEVIEW);
  with Params do
  begin
    if toShowLines in FOptions then Style := Style or TVS_HASLINES;
    if toShowRoot in FOptions then Style := Style or TVS_LINESATROOT;
    if toShowButtons in FOptions then Style := Style or TVS_HASBUTTONS;
    if not (toReadOnly in FOptions) then Style := Style or TVS_EDITLABELS;
    if not (toHideSelection in FOptions) then Style := Style or TVS_SHOWSELALWAYS;
    if DragMode = dmManual then Style := Style or TVS_DISABLEDRAGDROP;
    if toHotTrack in FOptions then Style := Style or TVS_TRACKSELECT;
    if not (toToolTips in FOptions) then Style := Style or TVS_NOTOOLTIPS;
    if toSingleExpand in FOptions then Style := Style or TVS_SINGLEEXPAND;
    if toInfoTip in FOptions then Style := Style or TVS_INFOTIP;
    if toNoScroll in FOptions then Style := Style or TVS_NOSCROLL;
    if toFullRowSelect in FOptions then Style := Style or TVS_FULLROWSELECT;
    if not (toEvenHeight in FOptions) then Style := Style or TVS_NONEVENHEIGHT;

    Style := Style or WS_CLIPCHILDREN;
    if FBorderStyle = bsSingle then Style := Style or WS_BORDER;
    if Ctl3D and (FBorderStyle = bsSingle) then ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;

    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CreateWnd;

begin
  FStateChanging := False;
  inherited CreateWnd;
  if FMemStream <> nil then
  begin
    Items.FSelection.Clear;
    Items.ReadData(FMemStream);
    FMemStream.Destroy;
    FMemStream := nil;
    SetTopItem(Items.GetNodeFromIndex(FSaveTopIndex));
    FSaveTopIndex := 0;
    SetSelection(Items.GetNodeFromIndex(FSaveIndex));
    FSaveIndex := 0;
  end;

  DragAcceptFiles( handle, true ); // [MJ]

  if FSaveIndent <> -1 then Indent := FSaveIndent;
  if FSaveItemHeight <> -1 then ItemHeight := FSaveItemHeight;
  if (Images <> nil) and Images.HandleAllocated then SetImageList(Images.Handle, TVSIL_NORMAL);
  if (StateImages <> nil) and StateImages.HandleAllocated then SetImageList(StateImages.Handle, TVSIL_STATE)
                                                          else SetImageList(CheckImages.Handle, TVSIL_STATE);
  Treeview_SetScrollTime(Handle, FScrollTime);
  GetScrollTime;

  FontChanged(nil);
  Treeview_SetUnicodeFormat(Handle, False);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DestroyWnd;

var Node : TTreeNTNode;

begin
  // [x] [MJ] THIS IS where pointers to items become invalid!!!
  DragAcceptFiles( handle, false ); // [MJ]
  FStateChanging := True;
  if Items.Count > 0 then
  begin
    FMemStream := TMemoryStream.Create;
    Items.WriteData(FMemStream);
    FMemStream.Position := 0;
    Node := GetTopItem;
    if Node <> nil then FSaveTopIndex := Node.AbsoluteIndex;
    Node := Selected;
    if Node <> nil then FSaveIndex := Node.AbsoluteIndex;
  end;
  FSaveIndent := Indent;
  FSaveItemHeight := ItemHeight;
  inherited DestroyWnd;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoScroll(Sender: TObject);

var LastScrollInfo,
    NewScrollInfo  : TScrollInfo;
    Offset         : Integer;
    ScrollPages    : Boolean;

begin
  FScrollTimer.Enabled := False;
  ScrollPages := False;
  if FScrollTimer.Interval = 300 then FScrollTimer.Interval := 50;
  if FScrollTimer.Interval > 1 then FScrollTimer.Interval := 3 * FScrollTimer.Interval div 4
                               else ScrollPages := True;
  if Dragging and assigned(FDragObject) then FDragObject.HideDragImage;

  // remove focus rect if we are in selection mode
  if IsMouseSelecting then DrawFocusRect(FSelectRec.Rect);

  with NewScrollInfo do
  begin
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_POS;
  end;
  // if we are in selection mode we have to adjust the start and end points of
  // our selection rectangle
  with LastScrollInfo do
  begin
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_VERT, LastScrollInfo);
    if sdUp in FScrollDirection then
    begin
      if nPos = nMin then Exclude(FScrollDirection, sdUp)
                     else
      begin
        if ScrollPages then Perform(WM_VSCROLL, SB_PAGEUP, 0)
                       else Perform(WM_VSCROLL, SB_LINEUP, 0);

        if IsMouseSelecting then
        begin
          // determine the absolute position of the client window
          // relatively to the entire canvas
          GetScrollInfo(Handle, SB_VERT, NewScrollInfo);
          Offset := (nPos - NewScrollInfo.nPos) * ItemHeight;
          Inc(FSelectRec.StartY, Offset);
          Inc(FSelectRec.Rect.Bottom, Offset);
        end;
      end;
    end;
    if sdDown in FScrollDirection then
    begin
      if nPos + Integer(nPage) - 1 = nMax then Exclude(FScrollDirection, sdDown)
                                          else
      begin
        if ScrollPages then Perform(WM_VSCROLL, SB_PAGEDOWN, 0)
                       else Perform(WM_VSCROLL, SB_LINEDOWN, 0);
        if IsMouseSelecting then
        begin
          // determine the absolute position of the client window
          // relatively to the entire canvas
          GetScrollInfo(Handle, SB_VERT, NewScrollInfo);
          Offset := (NewScrollInfo.nPos - nPos) * ItemHeight;
          Dec(FSelectRec.StartY, Offset);
          Dec(FSelectRec.Rect.Top, Offset);
        end;
      end;
    end;
  end;

  with LastScrollInfo do
  begin
    GetScrollInfo(Handle, SB_HORZ, LastScrollInfo);
    if sdLeft in FScrollDirection then
    begin
      if nPos = nMin then Exclude(FScrollDirection, sdleft)
                     else
      begin
        Perform(WM_HSCROLL, SB_LINELEFT, 0);
        if IsMouseSelecting then
        begin
          // the window is always scrolled 5 pixels horizontally
          Inc(FSelectRec.StartX, 5);
          Inc(FSelectRec.Rect.Right, 5);
        end;
      end;
    end;
    if sdRight in FScrollDirection then
    begin
      if nPos + Integer(nPage) - 1 = nMax then Exclude(FScrollDirection, sdRight)
                                          else
      begin
        Perform(WM_HSCROLL, SB_LINERIGHT, 0);
        if IsMouseSelecting then
        begin
          // the window is always scrolled 5 pixels horizontally
          Dec(FSelectRec.StartX, 5);
          Dec(FSelectRec.Rect.Left, 5);
        end;
      end;
    end;
  end;

  // redraw focus rect if we are in selection mode
  if IsMouseSelecting then DrawFocusRect(FSelectRec.Rect);
  if Dragging and assigned(FDragObject) then FDragObject.ShowDragImage;
  if FScrollDirection <> [] then FScrollTimer.Enabled := True;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.EditWndProc(var Message: TMessage);

begin
  try
    with Message do
    begin
      case Msg of
        WM_KEYDOWN,
        WM_SYSKEYDOWN:
          if DoKeyDown(TWMKey(Message)) then Exit;
        WM_CHAR:
          if DoKeyPress(TWMKey(Message)) then Exit;
        WM_KEYUP,
        WM_SYSKEYUP:
          if DoKeyUp(TWMKey(Message)) then Exit;
        CN_KEYDOWN,
        CN_CHAR,
        CN_SYSKEYDOWN,
        CN_SYSCHAR:
          begin
            WndProc(Message);
            Exit;
          end;
      end;
      Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.FontChanged(Sender: TObject);

begin
  Perform(WM_SETFONT, Font.Handle, MakeLParam(1, 0));
  TreeView_SetTextColor(Handle, ColorToRGB(Font.Color));
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.ClearSelection;

// deselects all selected nodes
// due to the walk through, probably, many nodes in the tree, this method
// may be time costly with a large selection (and it's called every time
// the user makes a simple focus change)

var Next, Node : TTreeNTNode;

begin
  if Items.FSelection.Count > 0 then
  begin
    Items.LockSelection;
    Items.FCoreSelecting := True;
    Node := Items.GetFirstSelectedNode;
    while assigned(Node) do
    begin
      Next := Node.GetNextSelected;
      Items.SelectNode(Node, stReset);
      Node := Next;
    end;
    Items.FSelection.Clear;
    Items.FCoreSelecting := False;

    with FChangeTimer do
    if Interval > 0 then
    begin
      Enabled := False;
      Tag := 0;
      Enabled := True;
    end
    else Change(nil);
    Items.UnlockSelection;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CMColorChanged(var Message: TMessage);

begin
  inherited;
  RecreateWnd;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CMCtl3DChanged(var Message: TMessage);

begin
  inherited;
  if FBorderStyle = bsSingle then
  begin
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.AlphaSort: Boolean;

begin
  if HandleAllocated then Result := CustomSort(nil, 0)
                     else Result := False;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CustomSort(SortProc: TTVCompare; Data: Longint): Boolean;

var SortCB : TTVSortCB;
    Node   : TTreeNTNode;

begin
  Result := False;
  if HandleAllocated then
  begin
    with SortCB do
    begin
      if assigned(SortProc) then lpfnCompare := SortProc
                            else lpfnCompare := @DefaultTreeNTSort;
      hParent := TVI_ROOT;
      lParam := Data;
      Result := TreeView_SortChildrenCB(Handle, SortCB, 0);
    end;

    if assigned(SortProc) then Items.FRoot.FChildList.Sort(SortProc, Data)
                          else Items.FRoot.FChildList.Sort(DefaultListSort, Data);

    if Items.Count > 0 then
    begin
      Node := Items.GetFirstNode;
      while Node <> nil do
      begin
        if Node.Count > 0 then Node.CustomSort(SortProc, Data);
        Node := Node.GetNext;
      end;
    end;
  end;

  // the item cache is no longer valid
  Items.InvalidateItemCache;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetSortType(Value: TSortType);

begin
  if SortType <> Value then
  begin
    FSortType := Value;
    if FSortType <> stNone then AlphaSort;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetStyle(Value: Integer; UseStyle: Boolean);

var Style : Integer;

begin
  if HandleAllocated then
  begin
    Style := GetWindowLong(Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
                    else Style := Style or Value;
    SetWindowLong(Handle, GWL_STYLE, Style);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetBorderStyle(Value: TBorderStyle);

begin
  if BorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetDragMode(Value: TDragMode);

begin
  if Value <> DragMode then SetStyle(TVS_DISABLEDRAGDROP, Value = dmManual);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetItemHeight(Value: ShortInt);

begin
  if (Value < 1) and (Value <> -1) then Value := -1;
  TreeView_SetItemHeight(Handle, Value);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetOptions(Values: TTreeOptions);

// sets one or more tree options

const NoRecreationStyles = [toAutoExpand, toAutoScroll, toCheckSupport, toLevelSelectConstraint, toNoEraseBkgnd, toEvenHeight, toMultiSelect,
                            toShowLines, toShowRoot, toShowButtons, toHideSelection, toInfoTip, toWantReturn, toRightClickSelect];

var ToBeSet,
    ToBeCleared : TTreeOptions;

begin
  // handle special cases:
  // - full row select can't be used together with ShowLines, the former
  //   gets higher priority here
  if toFullRowSelect in Values then Exclude(Values, toShowLines);

  // determine actually changed options
  ToBeSet := Values - FOptions;
  ToBeCleared := FOptions - Values;

  // now set the options, which can be applied without recreation of the control
  if toEvenHeight in ToBeSet then SetStyle(TVS_NONEVENHEIGHT, False);
  if toHideSelection in ToBeSet then SetStyle(TVS_SHOWSELALWAYS, False);
  if toInfoTip in ToBeSet then SetStyle(TVS_INFOTIP, True);
  if toShowLines in ToBeSet then SetStyle(TVS_HASLINES, True);
  if toShowRoot in ToBeSet then SetStyle(TVS_LINESATROOT, True);
  if toShowButtons in ToBeSet then SetStyle(TVS_HASBUTTONS, True);
  if toSingleExpand in ToBeSet then SetStyle(TVS_SINGLEEXPAND, True);

  // leave only styles in the indicator, which require a window recreation
  ToBeSet := ToBeSet - NoRecreationStyles;

  // reset the options, which can be applied without recreation of the control
  if toEvenHeight in ToBeCleared then SetStyle(TVS_NONEVENHEIGHT, True);
  if toHideSelection in ToBeCleared then SetStyle(TVS_SHOWSELALWAYS, True);
  if toInfoTip in ToBeSet then SetStyle(TVS_INFOTIP, False);
  if toShowLines in ToBeCleared then SetStyle(TVS_HASLINES, False);
  if toShowRoot in ToBeCleared then SetStyle(TVS_LINESATROOT, False);
  if toShowButtons in ToBeCleared then SetStyle(TVS_HASBUTTONS, False);
  if toSingleExpand in ToBeCleared then SetStyle(TVS_SINGLEEXPAND, False);

  // multi-selection special
  if toMultiSelect in ToBeCleared then ClearSelection;

  // leave only styles in the indicator, which require a window recreation
  ToBeCleared := ToBeCleared - NoRecreationStyles;

  FOptions := Values;

  // if one of the styles which require a window recreation has changed
  // initiate recreation now
  if (ToBeSet + ToBeCleared) <> [] then
  begin
    RecreateWnd;
  end;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetNodeAt(X, Y: Integer): TTreeNTNode;

var HitTest : TTVHitTestInfo;

begin
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    if TreeView_HitTest(Handle, HitTest) <> nil then Result := Items.GetNode(HitTest.hItem)
                                               else Result := nil;
  end;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetHitTestInfoAt(X, Y: Integer): THitTests;

var HitTest : TTVHitTestInfo;

begin
  Result := [];
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    TreeView_HitTest(Handle, HitTest);
    if (flags and TVHT_ABOVE) <> 0 then Include(Result, htAbove);
    if (flags and TVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and TVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and TVHT_ONITEM) <> 0 then Include(Result, htOnItem);
    if (flags and TVHT_ONITEMBUTTON) <> 0 then Include(Result, htOnButton);
    if (flags and TVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
    if (flags and TVHT_ONITEMINDENT) <> 0 then Include(Result, htOnIndent);
    if (flags and TVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
    if (flags and TVHT_ONITEMRIGHT) <> 0 then Include(Result, htOnRight);
    if (flags and TVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    if (flags and TVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and TVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetTreeNTNodes(Value: TTreeNTNodes);

begin
  Items.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetIndent(Value: Integer);

begin
  if Value <> Indent then TreeView_SetIndent(Handle, Value);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetIndent: Integer;

begin
  Result := TreeView_GetIndent(Handle)
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.FullExpand;

var Node: TTreeNTNode;

begin
  Node := Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Expand(True);
    Node := Node.GetNextSibling;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.FullNotHidden;
var Node: TTreeNTNode;

begin
  Items.BeginUpdate;
  Node := Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Hide(False,True,True);
    Node := Node.GetNextSibling;
  end;
  Items.EndUpdate;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DrawTo(ACanvas: TCanvas);

// Draws the entire tree to the given canvas. This can be the canvas of the
// current printer as well as any bitmap canvas, to store the image onto the harddisk.
// Make sure the target canvas is large enough to draw to.
// Call TreeWidth and TreeHeight to get the entire tree extent.

var AbsWidth,
    AbsHeight,
    NodeHeight,
    X, Y,
    LastX,
    LastY      : Integer;
    ScrollInfo : TScrollInfo;
    I, J       : Integer;
    Rect       : TRect;

  //---------- local function ----------

  function Ceiling(X: Extended): Integer;

  begin
    Result := Trunc(X);
    if Frac(X) > 0 then Inc(Result);
  end;

  //------------------------------------

begin
  if Items.CountNotHidden > 0 then
  begin
    // determine height of a node
    // can't rely on property ItemHeight, since it might not be available
    // with pre-4.70-versions of ComCtrl32.dll
    NodeHeight := ItemHeight;
    if NodeHeight = 0 then
    begin
      Rect := Items.GetFirstNode.DisplayRect(False);
      NodeHeight := Rect.Bottom - Rect.Top;
    end
    else
      if Items.GetFirstNode.IntegralHeight > 0 then NodeHeight := NodeHeight div Items.GetFirstNode.IntegralHeight;

    // determine last scroll position and absolute size
    with ScrollInfo do
    begin
      // init the scroll info structure
      FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
      cbSize := SizeOf(TScrollInfo);
      fMask := SIF_ALL;
    end;

    GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
    LastX := ScrollInfo.nPos;
    // horizontal dimension is given in pixels
    if ScrollInfo.nPage = 0 then AbsWidth := ClientWidth
                            else AbsWidth := ScrollInfo.nMax;

    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    LastY := ScrollInfo.nPos;
    // vertical dimension is given in number of items
    if ScrollInfo.nPage = 0 then AbsHeight := ClientHeight
                            else AbsHeight := (ScrollInfo.nMax + 1) * NodeHeight;

    // scroll through the entire tree and capture the output;
    // start at top of the window
    Perform(WM_VSCROLL, SB_TOP, 0);
    for J := 0 to Ceiling(AbsHeight/ClientHeight) do
    begin
      GetScrollInfo(Handle, SB_VERT, ScrollInfo);
      if ScrollInfo.nPage > 0 then Y := ScrollInfo.nPos * NodeHeight
                              else Y := 0;

      // scroll to left border
      Perform(WM_HSCROLL, SB_TOP, 0);
      for I := 0 to Ceiling(AbsWidth / ClientWidth) do
      begin
        GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
        X := ScrollInfo.nPos;
        // capture current content...
        BitBlt(ACanvas.Handle, X, Y, ClientWidth, ClientHeight, Canvas.Handle, 0, 0, SRCCOPY);
        // .. and scroll one page to the right
        Perform(WM_HSCROLL, SB_PAGERIGHT, 0);
      end;
      // scroll one page down
      Perform(WM_VSCROLL, SB_PAGEDOWN, 0);
    end;

    // finally scroll to the last position
    Perform(WM_HSCROLL, MakeWParam(SB_THUMBPOSITION, LastX), 0);
    Perform(WM_VSCROLL, MakeWParam(SB_THUMBPOSITION, LastY), 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.FullCollapse;

var Node: TTreeNTNode;

begin
  Node := Items.GetFirstNode;
  while Node <> nil do
  begin
    Node.Collapse(True);
    Node := Node.GetNextSibling;
  end;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetTopItem: TTreeNTNode;

begin
  if HandleAllocated then Result := Items.GetNode(TreeView_GetFirstVisible(Handle))
                     else Result := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetTopItem(Value: TTreeNTNode);

begin
  if HandleAllocated and (Value <> nil)
     and (Value.ItemId <> nil)          // [dpv]
   then TreeView_SelectSetFirstVisible(Handle, Value.ItemId);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetScrollTime: Integer;

begin
  FScrollTime := Treeview_GetScrollTime(Handle);
  Result := FScrollTime;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetSearchString: String;

var Count  : Integer;

begin
  Count := Integer(TreeView_GetISearchString(Handle, nil));
  if Count > 0 then
  begin
    SetString(Result, nil, Count);
    TreeView_GetISearchString(Handle, PChar(Result));
  end
  else Result := '';
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetSelection: TTreeNTNode;

begin
  if HandleAllocated then
  begin
    if (toRightClickSelect in FOptions) and assigned(FRClickNode)
      then Result := FRClickNode
      else Result := Items.GetNode(TreeView_GetSelection(Handle));
  end
  else Result := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetScrollTime(Value: Integer);

begin
  if Value <> FScrollTime then
  begin
    FScrollTime := Value;
    if FScrollTime < 100 then FScrollTime := 100;
    Treeview_SetScrollTime(Handle, FScrollTime);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetSelection(Value: TTreeNTNode);

begin
  if assigned(Value)
     and assigned(Value.ItemId)                     // [dpv]
   then TreeView_SelectItem(Handle, Value.ItemID)
   else TreeView_SelectItem(Handle, nil);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetChangeDelay: Integer;

begin
  Result := FChangeTimer.Interval;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetDropTarget: TTreeNTNode;

begin
  if HandleAllocated then
  begin
    Result := Items.GetNode(TreeView_GetDropHilite(Handle));
    if Result = nil then Result := FLastDropTarget;
  end
  else Result := nil;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetCheckImage(Node: TTreeNTNode; Image: Integer);

begin
  if toCheckSupport in FOptions then Node.StateIndex := Image;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetColorDropSelected(Color: TColor);

begin
  if FColorDropSelected <> Color then
  begin
    FColorDropSelected := Color;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetColorSelected(Color: TColor);

begin
  if FColorSelected <> Color then
  begin
    FColorSelected := Color;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetColorUnfocusedSelected(Color: TColor);

begin
  if FColorUnfocusedSelected <> Color then
  begin
    FColorUnfocusedSelected := Color;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetChangeDelay(Value: Integer);

begin
  FChangeTimer.Interval := Value;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetDropTarget(Value: TTreeNTNode);

begin
  if HandleAllocated then
    if Value <> nil then Value.DropTarget := True
                    else TreeView_SelectDropTarget(Handle, nil);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetItemHeight: ShortInt;

var R : TRect;

begin
  Result := TreeView_GetItemHeight(Handle);
  // older ComCtl32.DLL versions don't have an exported item height, so calculate it
  // from the first item, if there's one
  if (Result = 0) and (Items.GetFirstNode <> nil) then
  begin
    R := Items.GetFirstNode.DisplayRect(False);
    Result := R.Bottom - R.Top;
  end;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetInsertMarkColor: TColor;

begin
  Result := TColor(TreeView_GetInsertMarkColor(Handle));
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetLastVisibleNode: TTreeNTNode;

begin
  Result := Items.GetNode(TreeView_GetLastVisible(Handle));
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetNodeFromItem(const Item: TTVItem): TTreeNTNode;

begin
  with Item do
    if (State and TVIF_PARAM) <> 0 then Result := Pointer(lParam)
                                   else Result := Items.GetNode(hItem);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.IsEditing: Boolean;

var ControlHandle : HWnd;

begin
  ControlHandle := TreeView_GetEditControl(Handle);
  Result := (ControlHandle <> 0) and IsWindowVisible(ControlHandle);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetToolTips: HWND;

begin
  Result := TreeView_GetToolTips(Handle);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CMSysColorChange(var Message: TMessage);

begin
  inherited;
  ConvertAndAddImages(CheckImages);
  Message.Msg := WM_SYSCOLORCHANGE;
  DefaultHandler(Message);
end;

//------------------------------------------------------------------------------

function MakeNodeState(ItemStates: UINT): TNodeStates;

begin
  Result := [];
  if (ItemStates and CDIS_SELECTED) <> 0 then Include(Result, nsSelected);
  if (ItemStates and CDIS_FOCUS) <> 0 then Include(Result, nsFocused);
  if (ItemStates and CDIS_DISABLED) <> 0 then Include(Result, nsDisabled);
  if (ItemStates and CDIS_GRAYED) <> 0 then Include(Result, nsGrayed);
  if (ItemStates and CDIS_HOT) <> 0 then Include(Result, nsHot);
  if (ItemStates and CDIS_INDETERMINATE) <> 0 then Include(Result, nsIndeterminate);
  if (ItemStates and CDIS_MARKED) <> 0 then Include(Result, nsMarked);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CNNotify(var Message: TWMNotify);

var Node       : TTreeNTNode;
    NodeStates : TNodeStates;
    NewText    : String;
    OwnerDraw,
    Allowed    : Boolean;
    MousePos   : TPoint;

begin
  with Message.NMHdr^ do
  case code of
    TVN_BEGINRDRAG,
    TVN_BEGINDRAG:
      begin
        FDragged := True;
        with PNMTreeView(Pointer(Message.NMHdr))^ do FDragNode := GetNodeFromItem(ItemNew);
      end;
    TVN_BEGINLABELEDIT:
      with PTVDispInfo(Pointer(Message.NMHdr))^ do
      begin
        if Dragging or not CanEdit(GetNodeFromItem(Item)) then Message.Result := 1;
        if Message.Result = 0 then
        begin
          FEditHandle := TreeView_GetEditControl(Handle);
          FDefEditProc := Pointer(GetWindowLong(FEditHandle, GWL_WNDPROC));
          SetWindowLong(FEditHandle, GWL_WNDPROC, LongInt(FEditInstance));
          Node := GetNodeFromItem(Item);
          if not Node.ParentFont and assigned(Node.FFont) then
            SendMessage(FEditHandle, WM_SETFONT, Node.Font.Handle, MakeLParam(1, 0));
        end;
      end;
    TVN_ENDLABELEDIT:
      with PTVDispInfo(Pointer(Message.NMHdr))^ do Edit(Item);
    TVN_ITEMEXPANDING:
      if not FManualNotify then
      begin
        with PNMTreeView(Pointer(Message.NMHdr))^ do
        begin
          Node := GetNodeFromItem(ItemNew);
          if (action = TVE_EXPAND) then
            if not CanExpand(Node) then Message.Result := 1;

          if (action = TVE_COLLAPSE) then
            if not CanCollapse(Node) then Message.Result := 1;
        end;
      end;
    TVN_ITEMEXPANDED:
      if not FManualNotify then
      begin
        with PNMTreeView(Pointer(Message.NMHdr))^ do
        begin
          Node := GetNodeFromItem(ItemNew);
          if action = TVE_EXPAND then
          begin
            if assigned(Node) then Node.FExpanded := True;
            Expand(Node);
          end
          else
          if action = TVE_COLLAPSE then
          begin
            if assigned(Node) then Node.FExpanded := False;
            Collapse(Node);
          end;
        end;
      end;
    TVN_SELCHANGING:
      with PNMTreeView(Pointer(Message.NMHdr))^ do
        if not CanChange(GetNodeFromItem(ItemNew)) then Message.Result := 1;
    TVN_SELCHANGED:
      with PNMTreeView(Pointer(Message.NMHdr))^ do
      begin
        Node := GetNodeFromItem(ItemOld);
        if assigned(Node) then Items.RemoveFromSelection(Node);
        Node := GetNodeFromItem(ItemNew);
        if assigned(Node) then
        begin
          Items.AddToSelection(Node);
          if not Items.FCoreSelecting then Items.FLastSelLevel := Node.Level;
        end;
        DoChange(Node);
      end;
    TVN_DELETEITEM:
      begin
        with PNMTreeView(Pointer(Message.NMHdr))^ do Node := GetNodeFromItem(ItemOld);
        if Node <> nil then
        begin
          if Node.FHidden <> hsHiding then begin              // [dpv]
              Node.FItemId := nil;
              FChangeTimer.Enabled := False;
              if FStateChanging then Node.Delete
                                else Items.Delete(Node);
              Items.InvalidateItemCache;
          end;
        end;
      end;
    TVN_SETDISPINFO:
      with PTVDispInfo(Pointer(Message.NMHdr))^ do
      begin
        Node := GetNodeFromItem(Item);
        if (Node <> nil) and ((Item.Mask and TVIF_TEXT) <> 0) then Node.Text := Item.pszText;
      end;
    TVN_GETDISPINFO:
      with PTVDispInfo(Pointer(Message.NMHdr))^ do
      begin
        Node := GetNodeFromItem(Item);
        if Node <> nil then
        begin
//##fe
          if (Item.Mask and TVIF_TEXT) <> 0 then
            StrLCopy(Item.pszText, PChar(Node.Text), Item.cchTextMax);


          if (Item.Mask and TVIF_IMAGE) <> 0 then
          begin
            GetImageIndex(Node);
            Item.iImage := Node.ImageIndex;
          end;
          if (Item.Mask and TVIF_SELECTEDIMAGE) <> 0 then
          begin
            GetSelectedIndex(Node);
            Item.iSelectedImage := Node.SelectedIndex;
          end;
        end;
      end;
    TVN_SINGLEEXPAND:
      with PNMTreeView(Pointer(Message.NMHdr))^ do
      begin
        Node := GetNodeFromItem(ItemNew);
        Allowed := True;
        if assigned(FOnSingleExpanded) then FOnSingleExpanded(Self, Node, Allowed);
        if not Allowed then Message.Result := 1;
      end;
    TVN_GETINFOTIP:
      if assigned(FOnHint) then
      with PNMTVGetInfoTip(Pointer(Message.NMHdr))^ do
      begin
        Node := TTreeNTNode(lParam);
        NewText := '';
        FOnHint(Self, Node, NewText);
        StrPLCopy(pszText, NewText, cchTextMax - 1);
      end;
    NM_CUSTOMDRAW:
      with PNMTVCustomDraw(Pointer(Message.NMHdr))^ do
      case nmcd.dwDrawStage of
        CDDS_PREPAINT:
          begin
            if Items.FCoreSelecting then Message.Result := CDRF_SKIPDEFAULT
                                    else
            begin
              if assigned(FOnBeforePaint) then FOnBeforePaint(Self);
              Message.Result := CDRF_NOTIFYITEMDRAW or
                              CDRF_NOTIFYITEMERASE or
                              CDRF_NOTIFYPOSTERASE or
                              CDRF_NOTIFYPOSTPAINT;
            end;
          end;
        CDDS_ITEMPREPAINT:
          begin
            Message.Result := CDRF_NOTIFYPOSTPAINT;

            Node := TTreeNTNode(nmcd.lItemlParam);

            if Node = nil then
            begin
              Message.Result := CDRF_DODEFAULT;
              Exit;
            end;

            NodeStates := MakeNodeState(nmcd.uItemState);
            if Node.CheckState in [csChecked, csCheckedGrayed] then Include(NodeStates, nsChecked);

            // do custom draw only if not otherwise directed
            OwnerDraw := False;
            // ask user
            if assigned(FOnBeforeItemPaint) then FOnBeforeItemPaint(Self, Node, nmcd.rc, NodeStates, OwnerDraw);

            // if nothing says the user will draw the node then let us
            // do the usual work
            if not OwnerDraw then
            begin
              // the background color is determined by various conditions
              if nsSelected in NodeStates then // node selected?
              begin
                // do we currently drag?
                if Dragging then
                begin
                  // yes, we do, so color the drop target in usual selection shade
                  // if it is not part of the selection, else color it lighter
                  if Node = DropTarget then clrTextBk := ColorToRGB(FColorDropSelected)
                                       else clrTextBk := ColorToRGB(FColorSelected);
                  //clrText := ColorToRGB(clWhite);
                end
                else
                  // no drag'n drop, but has the tree the focus at all?
                  if not Focused then clrTextBk := ColorToRGB(FColorUnfocusedSelected)
                                 else
                    if Node.ParentColor then
                      // tree has the focus, so the user is changing the node focus,
                      // to avoid a wrong background we have to check for the color
                      // the treeview suggests (in this case it always wants to use
                      // clWindow, which might be different from the current
                      // treeview background)
                      if clrTextBk = Cardinal(ColorToRGB(clWindow)) then clrTextBk := ColorToRGB(Color)
                                                                    else clrTextBk := ColorToRGB(FColorSelected)
                                        else clrTextBk := ColorToRGB(FColorSelected);
              end
              else
              begin
                // the node is not selected, but is perhaps the drop target?
                if Node = DropTarget then
                begin
                  clrTextBk := ColorToRGB(FColorSelected);
                  //clrText := ColorToRGB(clWhite);
                end
                else
                  // no, not the drop target and not selected, so draw it
                  // either in its own color or the one of the tree
                  if Node.ParentColor then clrTextBk := ColorToRGB(Color)
                                      else clrTextBk := ColorToRGB(Node.Color);
              end;

              if not Node.ParentFont then
              begin
                FLastFont := SelectObject(nmcd.hdc, Node.Font.Handle);
                // change font color to reflect node font setting (don't change
                // the color for a selected node or there will be no text drawn as
                // drag image)
                if not (nsSelected in NodeStates) then clrText := ColorToRGB(Node.Font.Color);
                Message.Result := Message.Result or CDRF_NEWFONT;
              end
              else
                if FLastFont <> 0 then
                begin
                  SelectObject(nmcd.hdc, FLastFont);
                  Message.Result := Message.Result or CDRF_NEWFONT;
                  FLastFont := 0;
                end;

            end
            else Message.Result := CDRF_SKIPDEFAULT; // note: no item post paint stage is entered
          end;
        CDDS_ITEMPREERASE:
          begin
            Message.Result := CDRF_NOTIFYPOSTERASE; // not yet supported
          end;
        CDDS_ITEMPOSTERASE:
          Message.Result := CDRF_DODEFAULT; // not yet supported
        CDDS_ITEMPOSTPAINT:
          begin
            Node := TTreeNTNode(nmcd.lItemlParam);
            if assigned(FOnAfterItemPaint) then
            begin
              NodeStates := MakeNodeState(nmcd.uItemState);
              FOnAfterItemPaint(Self, Node, nmcd.rc, NodeStates);
            end;
            Message.Result := CDRF_DODEFAULT;
          end;
        CDDS_POSTPAINT:
          begin
            if assigned(FOnAfterPaint) then FOnAfterPaint(Self);
            if FLastFont <> 0 then
            begin
              SelectObject(nmcd.hdc, FLastFont);
              FLastFont := 0;
              Message.Result := CDRF_NEWFONT;
            end;
            Message.Result := CDRF_DODEFAULT;
          end;
        CDDS_PREERASE:
          Message.Result := CDRF_DODEFAULT; // not yet supported
        CDDS_POSTERASE:
          Message.Result := CDRF_DODEFAULT; // not yet supported
      else Message.Result := CDRF_DODEFAULT;
      end;
    NM_CLICK:
      FClicked := True;
    NM_RCLICK:
      begin
        FRClicked := True;
        if toRightClickSelect in FOptions then
        begin
          GetCursorPos(MousePos);
          with PointToSmallPoint(ScreenToClientEx(MousePos)) do
          begin
            FRClickNode := GetNodeAt(X, Y);
            if assigned(FRClickNode) and not FRClickNode.Selected then ClearSelection;
            Perform(WM_RBUTTONUP, 0, MakeLong(X, Y));
          end;
        end
        else FRClickNode := Pointer(1);
      end;
    NM_RETURN:
      if toWantReturn in FOptions then
      begin
        if assigned(Selected) then
        begin
          Node := Selected.GetNext;
          if Node = nil then Node := Items.GetFirstNode;
        end
        else Node := Items.GetFirstNode;
        if assigned(Node) then
        begin
          ClearSelection;
          Node.Selected := True;
        end;
        Message.Result := 1;
      end;
    NM_SETFOCUS:
      begin
        // [mj] calling invalidate here causes annnoying flicker
        // whenever a modal dialog is open, because the tree has to
        // be redrawn at the time when the application is busy
        // initializing the dialog. I's entirely unnecessary, and
        // removing the Invalidate call doesn't seem to affect the
        // tree in any manner.
        if _MJ_INVALIDATE_ON_FOCUS_CHANGE then
          Invalidate;
        if FFirstSelection = nil then FFirstSelection := Selected;
      end;
    NM_KILLFOCUS:
      begin
        // [mj see note for NM_SETFOCUS
        if _MJ_INVALIDATE_ON_FOCUS_CHANGE then
          Invalidate;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CMMouseLeave(var Message: TMessage);

begin
  FScrollTimer.Enabled := False;
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetDragImages: {$ifdef DFS_COMPILER_4_UP} TDragImageList {$else} TCustomImageList {$endif};

begin
  if FDragImage.Count > 0 then Result := FDragImage
                          else Result := nil;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.IsMouseSelecting: Boolean;

begin
  Result := FSelectRec.Pending and not IsRectEmpty(FSelectRec.Rect);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.OnChangeTimer(Sender: TObject);

begin
  FChangeTimer.Enabled := False;
  Change(TTreeNTNode(FChangeTimer.Tag));
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WndProc(var Message: TMessage);

begin
  // for auto drag mode, let tree handle itself, instead of TControl
  if not (csDesigning in ComponentState) and
     ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) then
  begin
    if DragMode = dmAutomatic then
    begin
      if IsControlMouseMsg(TWMMouse(Message)) then Exit;
      ControlState := ControlState + [csLButtonDown];
      Dispatch(Message);  // overrides TControl's BeginDrag
      Exit;
    end;
  end;
  inherited WndProc(Message);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoStartDrag(var DragObject: TDragObject);

var
  ImageHandle: HImageList;
  DragNode: TTreeNTNode;
  P: TPoint;

begin
  inherited DoStartDrag(DragObject);
  DragNode := FDragNode;
  FDragNode := nil;
  FLastDropTarget := nil;

  if DragNode = nil then
  begin
    GetCursorPos(P);
    with ScreenToClient(P) do DragNode := GetNodeAt(X, Y);
  end;

  if DragNode <> nil then
  begin
    ImageHandle := TreeView_CreateDragImage(Handle, DragNode.ItemID);
    if ImageHandle <> 0 then
    begin
      with FDragImage do
      begin
        Handle := ImageHandle;
        SetDragImage(0, 2, 2);
      end;
    end;
    FExpandTimer.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoAutoScroll(X, Y: Integer);

var DefOffset  : Integer;
    VSInfo,
    HSInfo     : TScrollInfo;

begin
  // prepare structures
  FillChar(HSInfo, SizeOf(HSInfo), 0);
  HSInfo.cbSize := SizeOf(TScrollInfo);
  HSInfo.fMask := SIF_ALL;
  VSInfo := HSInfo;

  if ItemHeight > 0 then DefOffset := ItemHeight div 2 // v 4.71 +
                    else DefOffset := Abs(Font.Size); // v 4.70
  FScrollDirection := [];

  GetScrollInfo(Handle, SB_VERT, VSInfo);
  GetScrollInfo(Handle, SB_HORZ, HSInfo);

  with HSInfo do
  begin
    if (X < DefOffset) and (nPos > nMin) then Include(FScrollDirection, sdLeft);
    if (nPage > 0)                        and
       (nPos + Integer(nPage) - 1 < nMax) and
       (X > ClientWidth - DefOffset)      then Include(FScrollDirection, sdRight);
  end;

  with VSInfo do
  begin
    if (Y < DefOffset) and (nPos > nMin) then Include(FScrollDirection, sdUp);
    if (nPage > 0)                        and
       (Y > ClientHeight - DefOffset)     and
       (nPos + Integer(nPage) - 1 < nMax) then Include(FScrollDirection, sdDown);
  end;

  FScrollTimer.Interval := 300;
  if FScrollDirection <> [] then FScrollTimer.Enabled := True
                            else FScrollTimer.Enabled := False;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoChange(Node: TTreeNTNode);

begin
  if FChangeTimer.Interval > 0 then
  with FChangeTimer do
  begin
   Enabled := False;
   Tag := Integer(Node);
   Enabled := True;
  end
  else Change(Node);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoEndDrag(Target: TObject; X, Y: Integer);

begin
  FExpandTimer.Enabled := False;
  inherited DoEndDrag(Target, X, Y);
  FLastDropTarget := nil;
  Items.Repaint(DropTarget);
  Items.Repaint(Selected);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoDragExpand;

begin
  FExpandTimer.Enabled := False;
  if (toAutoExpand in FOptions) and
     assigned(DropTarget)       and
     DropTarget.HasChildren     and
     not DropTarget.Expanded    then
  begin
    if assigned(FDragObject) then FDragObject.HideDragImage;
    DropTarget.Expand(False);
    UpdateWindow(Handle);
    if assigned(FDragObject) then FDragObject.ShowDragImage;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.CMDrag(var Message: TCMDrag);

begin
  inherited;
  with Message, DragRec^ do
    case DragMessage of
      dmDragMove:
        with ScreenToClientEx(Pos) do DoDragOver(Source, X, Y, Message.Result <> 0);
      dmDragLeave:
        begin
          TDragObject(Source).HideDragImage;
          if assigned(DropTarget) and FDragImage.Dragging then DropTarget.DropTarget := False;
          DropTarget := nil;
          TDragObject(Source).ShowDragImage;
        end;
      dmDragDrop:
        begin
          FDragObject := nil;
          FDragged := False;
          FExpandTimer.Enabled := False;
        end;
    end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);

var Node : TTreeNTNode;

begin
  FDragObject := Source;
  if (toAutoScroll in FOptions) then DoAutoScroll(X, Y);
  Node := GetNodeAt(X, Y);
  if (Node <> nil) and
     ((Node <> DropTarget) or (Node = FLastDropTarget)) then
  begin
    FLastDropTarget := nil;
    FExpandTimer.Enabled := False;
    TDragObject(Source).HideDragImage;
    Node.DropTarget := True;
    TDragObject(Source).ShowDragImage;
    FExpandTimer.Enabled := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.GetImageIndex(Node: TTreeNTNode);

begin
  if assigned(FOnGetImageIndex) then FOnGetImageIndex(Self, Node);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.GetSelectedIndex(Node: TTreeNTNode);

begin
  if assigned(FOnGetSelectedIndex) then FOnGetSelectedIndex(Self, Node);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CanChange(Node: TTreeNTNode): Boolean;

begin
  Result := True;
  if assigned(FOnChanging) then FOnChanging(Self, Node, Result);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Change(Node: TTreeNTNode);

begin
  if assigned(FOnChange) then FOnChange(Self, Node);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Expand(Node: TTreeNTNode);

begin
  if assigned(FOnExpanded) then FOnExpanded(Self, Node);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CanExpand(Node: TTreeNTNode): Boolean;

begin
  Result := True;
  if assigned(FOnExpanding) then FOnExpanding(Self, Node, Result);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Check(Node: TTreeNTNode);

begin
  if assigned(FOnChecked) then FOnChecked(Self, Node);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Collapse(Node: TTreeNTNode);

begin
  if assigned(FOnCollapsed) then FOnCollapsed(Self, Node);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CanCollapse(Node: TTreeNTNode): Boolean;

begin
  Result := True;
  if assigned(FOnCollapsing) then FOnCollapsing(Self, Node, Result);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CanCheck(Node: TTreeNTNode): Boolean;

begin
  Result := True;
  if assigned(FOnChecking) then FOnChecking(Self, Node, Result);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CanEdit(Node: TTreeNTNode): Boolean;

begin
  Result := True;
  if assigned(FOnEditing) then FOnEditing(Self, Node, Result);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.DrawFocusRect(Rect: TRect);

begin
  FCanvas.Pen.Color := clBlack;
  FCanvas.DrawFocusRect(Rect);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Edit(const Item: TTVItem);

var S    : String;
    Node : TTreeNTNode;

begin
  with Item do
    if pszText <> nil then
    begin
      S := pszText;
      Node := GetNodeFromItem(Item);
      if assigned(FOnEdited) then FOnEdited(Self, Node, S);
      if assigned(Node) then Node.Text := S;
    end
    else { [MJ] added to tell main program that editing was canceled }
    begin
      if assigned(FOnEditCanceled) then FOnEditCanceled(Self);
    end;

end;

//------------------------------------------------------------------------------

function TCustomTreeNT.CreateNode: TTreeNTNode;

var AClass : TTreeNTNodeClass;

begin
  AClass := TTreeNTNode;
  if assigned(FOnCreateNode) then FOnCreateNode(Self, AClass);
  Result := AClass.Create(Items);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetImageList(Value: HImageList; Flags: Integer);

begin
  if HandleAllocated then TreeView_SetImageList(Handle, Value, Flags);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetTreeHeight;

// determines absolute height of the tree canvas

var ScrollInfo : TScrollInfo;
    NodeHeight : Integer;
    Rect       : TRect;

begin
  // determine height of a node
  // can't rely on property ItemHeight, since it might not be available
  // with pre-4.70-versions of ComCtrl32.dll
  NodeHeight := ItemHeight;
  if NodeHeight = 0 then
  begin
    Rect := Items.GetFirstNode.DisplayRect(False);
    NodeHeight := Rect.Bottom - Rect.Top;
  end
  else
    if Items.GetFirstNode.IntegralHeight > 0 then NodeHeight := NodeHeight div Items.GetFirstNode.IntegralHeight;

  // init the scroll info structure
  with ScrollInfo do
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
  end;

  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  // vertical dimension is given in number of items
  if ScrollInfo.nPage = 0 then Result := ClientHeight
                          else Result := (ScrollInfo.nMax + 1) * NodeHeight;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.GetTreeWidth;

// determines absolute width of the tree canvas

var ScrollInfo : TScrollInfo;

begin
  // init the scroll info structure
  with ScrollInfo do
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
  end;

  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  // horizontal dimension is given in pixels
  if ScrollInfo.nPage = 0 then Result := ClientWidth
                          else Result := ScrollInfo.nMax + 1;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.HandleDrawSelection(Shift: TShiftState; Rect: TRect);

// handles multi-selection with a focus rectangle, it is called every time
// the mouse is moved during selection mode

var FirstNode,
    LastNode      : TTreeNTNode;
    Dummy         : TRect;
    I             : Integer;
    HasChanged    : Boolean;
    LastIndex     : Integer;

begin
  Items.FCoreSelecting := True;
  HasChanged := False;
  FirstNode := GetNodeAt(0, Rect.Top);
  if FirstNode = nil then FirstNode := Items.GetFirstNode;
  LastNode := GetNodeAt(ClientWidth, Rect.Bottom);
  FTreeNTNodes.FillCache;

  // the last node is not to be selected
  with FTreeNTNodes do
    if assigned(LastNode) then
    begin
      LastIndex := FItemCache.IndexOf(LastNode);
      if LastIndex < FItemCache.Count - 1 then Inc(LastIndex);
    end
    else LastIndex := FItemCache.Count - 1;

  // selection whithout Ctrl key (Shift key is the same as without any key
  // except that the previous selection will not be cleared)
  if not (ssCtrl in Shift) then
  begin
    // deselect nodes which were, but are no longer in the focus rect
    for I := FHitList.Count - 1 downto 0 do
      if not IntersectRect(Dummy, FHitList[I].DisplayRect(True), Rect) then
      begin
        Items.SelectNode(FHitList[I], stReset);
        FHitList[I] := nil;
        HasChanged := True;
      end;

    // do some housekeeping with the list
    FHitList.Pack;

    // add selection
    with FTreeNTNodes do
      for I := FItemCache.IndexOf(FirstNode) to LastIndex do
        if FItemCache[I].IsVisible and IntersectRect(Dummy, FItemCache[I].DisplayRect(True), Rect) and
           (FHitList.IndexOf(FItemCache[I]) = -1) then
        begin
          FHitList.Add(FItemCache[I]);
          Items.SelectNode(FItemCache[I], stSet);
          HasChanged := True;
        end;
  end
  else
  begin
    // toggle selection

    with FTreeNTNodes do
      for I := FItemCache.IndexOf(FirstNode) to LastIndex do
        if (FHitList.IndexOf(FItemCache[I]) = -1) and
           IntersectRect(Dummy, FItemCache[I].DisplayRect(True), Rect) then
        begin
          FHitList.Add(FItemCache[I]);
          Items.SelectNode(FItemCache[I], stToggle);
          HasChanged := True;
        end;

    // now restore nodes which were, but are no longer in the focus rect
    for I := 0 to FHitList.Count - 1 do
      if not IntersectRect(Dummy, FHitList[I].DisplayRect(True), Rect) then
      begin
        Items.SelectNode(FHitList[I], stToggle);
        FHitList[I] := nil;
        HasChanged := True;
      end;
    // do some housekeeping with the list
    FHitList.Pack;
  end;

  Items.FCoreSelecting := False;
  if HasChanged then
    with FChangeTimer do
    if Interval > 0 then
    begin
      Enabled := False;
      Tag := Integer(LastNode);
      Enabled := True;
    end
    else Change(LastNode);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.HandleMultiSelection(LastNode: TTreeNTNode; OldState: TSelectType; NewNode: TTreeNTNode; NewState: TSelectType; Shift: TShiftState);

// handles multi-selection with mouse click

begin
  // Ctrl key down
  if ssCtrl in Shift then
  begin
    if ssShift in Shift then Items.SelectNodes(LastNode, NewNode, True)
                        else
    begin
      if assigned(LastNode) then Items.SelectNode(LastNode, OldState);
      if assigned(NewNode) then Items.SelectNode(NewNode, NewState);
    end;
  end
  else
    // Shift key down
    if ssShift in Shift then
    begin
      if FFirstSelection = nil then FFirstSelection := NewNode;
      // select node range
      if assigned(FFirstSelection) then Items.SelectNodes(FFirstSelection, NewNode, False);
    end
    else
      // any other case
      if assigned(NewNode) then
      begin
        ClearSelection;
        Items.SelectNode(NewNode, stSet);
        // assign new reference item
        FFirstSelection := NewNode;
        Items.FLastSelLevel := NewNode.Level;
      end;
  FTreeNTNodes.Repaint(NewNode);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.ImageListChange(Sender: TObject);

var ImageHandle : HImageList;

begin
  if HandleAllocated then
  begin
    ImageHandle := TImageList(Sender).Handle;
    if Sender = Images then SetImageList(ImageHandle, TVSIL_NORMAL)
                       else
      if Sender = StateImages then SetImageList(ImageHandle, TVSIL_STATE);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.MouseMove(Shift: TShiftState; X, Y: Integer);

begin
  if not FSelectRec.Pending then inherited MouseMove(Shift, X, Y);

  // handle auto scrolling if required
  if (toAutoScroll in FOptions) then DoAutoScroll(X, Y);

  // handle draw selection if reqired
  with FSelectRec do
    if Pending then
    begin
      // remove focus rect if we are in selection mode
      if IsMouseSelecting then DrawFocusRect(FSelectRec.Rect);

      // now draw new focus rectangle, if required
      // recalculate coordinates
      if X < StartX then begin Rect.Left := X; Rect.Right := StartX; end
                    else begin Rect.Left := StartX; Rect.Right := X; end;

      if Y < StartY then begin Rect.Top := Y; Rect.Bottom := StartY; end
                    else begin Rect.Top := StartY; Rect.Bottom := Y; end;

      if Items.CountNotHidden > 0 then HandleDrawSelection(Shift, Rect);
      // show the new selection before the focus rect is redrawn
      UpdateWindow(Handle);

      if not IsRectEmpty(Rect) then DrawFocusRect(Rect);
    end;

end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Notification(AComponent: TComponent; Operation: TOperation);

begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Images then Images := nil;
    if AComponent = StateImages then StateImages := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetImages(Value: TImageList);

begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    SetImageList(Images.Handle, TVSIL_NORMAL)
  end
  else SetImageList(0, TVSIL_NORMAL);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetInsertMarkColor(Value: TColor);

begin
  TreeView_SetInsertMarkColor(Handle, ColorToRGB(Value));
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SetStateImages(Value: TImageList);

// sets the given state image list for the tree or (if no image list is
// passed) the predefined check image list

begin
  if FStateImages <> nil then StateImages.UnRegisterChanges(FStateChangeLink);
  FStateImages := Value;
  if FStateImages <> nil then
  begin
    StateImages.RegisterChanges(FStateChangeLink);
    SetImageList(StateImages.Handle, TVSIL_STATE)
  end
  else SetImageList(CheckImages.Handle, TVSIL_STATE);
end;

//------------------------------------------------------------------------------

// this is the ID used to identify a 'native' TreeNT file or just the usual
// text file as produced by TTreeView

type TMagicID = array[0..4] of Char;

const MagicID : TMagicID = (#13, 'T', 'N', 'T', #13);


procedure TCustomTreeNT.LoadFromFile(const FileName: String);

var Stream : TStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.LoadFromStream(Stream: TStream);

// tries to load a tree structure from the given stream
// can load both types: text only and complete files

var ThisMagicID: TMagicID;
    LastPosition: Integer;

begin
  LastPosition := Stream.Position;
  Stream.Read(ThisMagicID, SizeOf(ThisMagicID));
  with Items do
  begin
    BeginUpdate;
    Clear;

    if ThisMagicID <> MagicID then
    begin
      Stream.Position := LastPosition;
      ReadStrings(Stream);
    end
    else ReadData(Stream);

    EndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure StretchBitmap(DestDc: HDC; X, Y, Width, Height: Word; BM: TBitmap);

// Stretches the given bitmap to the given destination DC, at position (X, Y)
// and the given size.

// This procedure is an adapted sample from Borland Q&A WWW site.

type // for palette re-construction
     PPalEntriesArray = ^TPalEntriesArray;
     TPalEntriesArray = array[0..0] of TPaletteEntry;

var OriginalWidth : Word;              // width of BM
    DC            : hdc;               // screen dc
    IsPaletteDevice     : Boolean;     // if the device uses palettes
    IsDestPaletteDevice : Boolean;     // if the device uses palettes
    BISize        : integer;           // sizeof the bitmapinfoheader
    lpBitmapInfo  : PBitmapInfo;       // the bitmap info header
    hBm           : HBitmap;           // handle to the bitmap
    hPal          : HPalette;          // handle to the palette
    OldPal        : HPalette;          // temp palette
    hBits         : THandle;           // handle to the DIB bits
    pBits         : Pointer;           // pointer to the DIB bits
    PalArray      : PPalEntriesArray;  // palette entry array
    NumPalEntries : integer;           // number of palette entries
    I             : integer;           // looping variable

begin
  {$ifopt R+} {$define RangeCheck} {$R-} {$endif}

  // save the original width of the bitmap
  OriginalWidth := BM.Width;

  // get the screen's dc to use since memory dc's are not reliable
  DC := GetDC(0);
  // Are we a palette device?
  IsPaletteDevice := GetDeviceCaps(dc, RASTERCAPS) and RC_PALETTE = RC_PALETTE;
  // give back the screen dc
  ReleaseDC(0, DC);

  // allocate the BitmapInfo structure
  if IsPaletteDevice then BISize := SizeOf(TBitmapInfo) + (SizeOf(TRGBQUAD) * 255)
                     else BISize := SizeOf(TBitmapInfo);
  GetMem(lpBitmapInfo, BISize);

  // zero out the BitmapInfo structure
  FillChar(lpBitmapInfo^, BISize, 0);
  OldPal := 0;

  // fill in the BitmapInfo structure
  with lpBitmapInfo^ do
  begin
    bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    bmiHeader.biWidth := OriginalWidth;
    bmiHeader.biHeight := bm.Height;
    bmiHeader.biPlanes := 1;

    if IsPaletteDevice then bmiHeader.biBitCount := 8
                       else bmiHeader.biBitCount := 24;
    bmiHeader.biCompression := BI_RGB;
    bmiHeader.biSizeImage := ((bmiHeader.biWidth * Integer(bmiHeader.biBitCount)) div 8) * bmiHeader.biHeight;
    bmiHeader.biXPelsPerMeter := 0;
    bmiHeader.biYPelsPerMeter := 0;
    if IsPaletteDevice then
    begin
      bmiHeader.biClrUsed := 256;
      bmiHeader.biClrImportant := 256;
    end
    else
    begin
      bmiHeader.biClrUsed := 0;
      bmiHeader.biClrImportant := 0;
    end;
  end;

  // take ownership of the bitmap handle and palette
  hBM := BM.ReleaseHandle;
  hPal := BM.ReleasePalette;

  // get the screen's dc to use since memory dc's are not reliable
  DC := GetDC(0);

  if IsPaletteDevice then
  begin
    // If we are using a palette, it must be selected into the dc during the conversion
    OldPal := SelectPalette(DC, hPal, True);
    // realize the palette
    RealizePalette(DC);
  end;

  // tell GetDIBits to fill in the rest of the bitmap info structure
  GetDIBits(DC, hBM, 0, lpBitmapInfo^.bmiHeader.biHeight, nil, TBitmapInfo(lpBitmapInfo^), DIB_RGB_COLORS);

  // allocate memory for the Bits
  HBits := GlobalAlloc(GMEM_MOVEABLE, lpBitmapInfo^.bmiHeader.biSizeImage);
  pBits := GlobalLock(hBits);

  // get the bits
  GetDIBits(DC, hBM, 0, lpBitmapInfo^.bmiHeader.biHeight, pBits, TBitmapInfo(lpBitmapInfo^), DIB_RGB_COLORS);

  if IsPaletteDevice then
  begin
    // let's fix up the color table for buggy video drivers
    GetMem(PalArray, SizeOf(TPaletteEntry) * 256);
    {$ifdef DFS_COMPILER_3_UP}
      NumPalEntries := GetPaletteEntries(hPal, 0, 256, PalArray^);
    {$else}
      NumPalEntries := GetSystemPaletteEntries(DC, 0, 256, PalArray^);
    {$endif}
    for I := 0 to NumPalEntries - 1 do
    begin
      lpBitmapInfo^.bmiColors[I].rgbRed := PalArray^[I].peRed;
      lpBitmapInfo^.bmiColors[I].rgbGreen := PalArray^[I].peGreen;
      lpBitmapInfo^.bmiColors[I].rgbBlue := PalArray^[I].peBlue;
    end;
    FreeMem(PalArray, SizeOf(TPaletteEntry) * 256);

    // select the old palette back in
    SelectPalette(dc, OldPal, True);
    // realize the old palette
    RealizePalette(DC);
  end;

  // give back the screen dc
  ReleaseDc(0, DC);

  // Is the Dest dc a palette device?
  IsDestPaletteDevice := GetDeviceCaps(DestDc, RASTERCAPS) and RC_PALETTE = RC_PALETTE;

  if IsPaletteDevice then
  begin
    // if we are using a palette, it must be selected into the DC during the conversion
    OldPal := SelectPalette(DestDC, hPal, True);
    // realize the palette
    RealizePalette(DestDC);
  end;

  // Do the blt
  StretchDIBits(DestDC, X, Y, Width, Height, 0, 0, OriginalWidth, lpBitmapInfo^.bmiHeader.biHeight,
                pBits, lpBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);

  if IsDestPaletteDevice then
  begin
    // select the old palette back in
    SelectPalette(DestDC, OldPal, True);
    // realize the old palette
    RealizePalette(DestDC);
  end;

  // de-allocate the DIB Bits
  GlobalUnLock(hBits);
  GlobalFree(hBits);

  // de-allocate the BitmapInfo
  FreeMem(lpBitmapInfo, BISize);

  // set the ownership of the bimap handles back to the bitmap
  bm.Handle := hBM;
  bm.Palette := hPal;

  {$ifdef RangeCheck} {$undef RangeCheck} {$R + } {$endif}
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.Print(XPos, YPos, Width: Integer);

// prints the entire tree at the given position to the current printer
// Note: units are millimeters!
//
// Since I want to preserve the aspect ratio so there's no need to pass a height
// here.

var PrinterResX,
    PrinterResY  : Single;
    BM           : TBitmap;
    PrnWidth,
    PrnHeight    : Integer;

begin
  BM := TBitmap.Create;
  try
    Printer.Title := Format('tree printing of %s', [Application.Title]);
    Printer.BeginDoc;
    try
      // capture screen output
      {$ifdef DFS_COMPILER_3_UP}
        BM.PixelFormat := pf24Bit;
      {$endif}
      BM.Width := TreeWidth;
      BM.Height := TreeHeight;

      // draw the tree to our bitmap
      DrawTo(BM.Canvas);

      // read printer resolutions and transform them in pixels per mm
      PrinterResX := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSX)/25.4;
      PrinterResY := GetDeviceCaps(Printer.Canvas.Handle, LOGPIXELSY)/25.4;

      // calculate actual size (in pixels)
      XPos := Round(XPos * PrinterResX);
      YPos := Round(YPos * PrinterResY);
      PrnWidth := Round(Width * PrinterresX);
      PrnHeight := Round(PrinterResY * BM.Height * PrnWidth/BM.Width/PrinterResX);

      // finally blit the bitmap to the printer canvas
      StretchBitmap(Printer.Canvas.Handle, XPos, YPos, PrnWidth, PrnHeight, BM);

      Printer.EndDoc;
    except
      Printer.Abort;
      raise;
    end;
  finally
    BM.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SaveToFile(const FileName: String; Complete: Boolean);

var Stream : TStream;

begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Complete);
  finally
    Stream.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.SaveToStream(Stream: TStream; Complete: Boolean);

begin
  if Complete then
  begin
    Stream.Write(MagicID, SizeOf(MagicID));
    Items.WriteData(Stream);
  end
  else Items.WriteStrings(Stream);
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.ScreenToClientEx(const Point: TPoint): TPoint;

var LocalCopy : TPoint;

begin
  LocalCopy := Point;
  MapWindowPoints(0, Handle, LocalCopy, 1);
  Result := LocalCopy;
end;

//------------------------------------------------------------------------------

function TCustomTreeNT.SetToolTips(TTHandle: HWND): HWND;

begin
  Result := TreeView_SetToolTips(Handle, TTHandle);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.ShowInsertMark(Node: TTreeNTNode; After: Boolean);

begin
  if assigned(Node) then TreeView_SetInsertmark(Handle, Integer(Node.ItemID), After)
                    else TreeView_SetInsertmark(Handle, 0, After);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMRButtonDown(var Message: TWMRButtonDown);

var MousePos : TPoint;

begin
  // check if we are in selection mode
  if FSelectRec.Pending then
    with FSelectRec do
    begin
      // yes we are, so finish it ...
      Pending := False;
      FHitList.Free;
      // ... and remove the last focus rectangle if there was one
      if not IsRectEmpty(Rect) then DrawFocusRect(Rect);
    end;

  FRClickNode := nil;
  try
    if not (toRightClickSelect in FOptions) then
    begin
      inherited;
      if FRClickNode <> nil then
      begin
        GetCursorPos(MousePos);
        with PointToSmallPoint(ScreenToClientEx(MousePos)) do Perform(WM_RBUTTONUP, 0, MakeLong(X, Y));
      end;
    end
    else DefaultHandler(Message);
  finally
    FRClickNode := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMRButtonUp(var Message: TWMRButtonUp);

  procedure DoMouseDown(var Message: TWMMouse; Button: TMouseButton; Shift: TShiftState);

  begin
    if not (csNoStdEvents in ControlStyle) then
      with Message do MouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
  end;

var Menu        : TPopupMenu;
    CurrentNode : TTreeNTNode;
    AskParent   : Boolean;

begin
  // support for node dependend popup menus
  if assigned(FOnGetPopupMenu) then
  begin
    CurrentNode := Selected;
    if assigned(CurrentNode) then
    begin
      Menu := nil;
      AskParent := True;
      repeat
        FOnGetPopupMenu(Self, CurrentNode, AskParent, Menu);
        CurrentNode := CurrentNode.FParent;
      until (CurrentNode = Items.FRoot) or assigned(Menu) or not AskParent;
      if assigned(Menu) then
      begin
        Menu.PopupComponent := Self;
        with ClientToScreen(Point(Message.XPos, Message.YPos)) do Menu.Popup(X, Y);
        Exit;
      end;
    end;
  end;
  if toRightClickSelect in FOptions then DoMouseDown(Message, mbRight, []);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMDestroy(var Message: TWMDestroy);

begin
  FullCollapse;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMKeyDown(var Message: TWMKeyDown);

// handle all key presses if they concern selection

var Shift         : TShiftState;
    WasSelected   : Boolean;
    CurrentNode,
    NextNode      : TTreeNTNode;
    Context,
    LastCount     : Integer;
    ParentControl : TWinControl;

begin
  if not (toMultiselect in FOptions) then
  begin
    FFirstSelection := Selected;
    inherited;
  end
  else
  with Message do
  begin
    Shift := KeyDataToShiftState(KeyData);

    // Ctrl key has special meaning
    if (ssCtrl in Shift) or (Selected = nil) then
    begin
      inherited;
      Exit;
    end;

    // line up/line down movement?
    if CharCode in [VK_UP, VK_DOWN] then
    begin
      // determine the node, which will be focused after the function
      // returns
      CurrentNode := Selected;
      if CharCode = VK_UP then NextNode := CurrentNode.GetPrevVisible
                          else NextNode := CurrentNode.GetNextVisible;
      // shift pressed (for multi-selection)?
      if ([ssShift] = Shift) then
      begin
        if FFirstSelection = nil then
        begin
          // initialize the start point if needed
          FFirstSelection := NextNode;
          ClearSelection;
        end;
        if assigned(NextNode) then
        begin
          WasSelected := NextNode.Selected;
          inherited;
          if not WasSelected then Items.SelectNode(CurrentNode, stSet);
        end
        else inherited;
      end
      else
      begin
        if assigned(NextNode) then ClearSelection;
        inherited;
        FFirstSelection := Selected;
      end;
    end
    else
    begin
      CurrentNode := Selected;
      // page up/page down movement?
      if CharCode in [VK_PRIOR, VK_NEXT, VK_HOME, VK_END] then
      begin
        // shift pressed (for multi-selection)?
        if ([ssShift] = Shift) then
        begin
          if FFirstSelection = nil then
          begin
            // initialize the start point if needed
            FFirstSelection := Selected;
            ClearSelection;
          end;
          inherited;
          NextNode := Selected;
          LastCount := Items.SelectedCount;
          Items.ToggleSelectionRange(CurrentNode, NextNode);
          // take out the effect of automatic selection if necessary
          if LastCount < Items.SelectedCount then Items.SelectNode(CurrentNode, stSet);
        end
        else
        begin
          if Items.SelectedCount > 1 then ClearSelection;
          inherited;
          FFirstSelection := Selected;
        end;
      end
      else
        case CharCode of
          VK_SPACE:
            begin // space-key, simulate check click
              if assigned(CurrentNode) then CurrentNode.DoCheckClick;
              Message.Result := 0;
            end;
          VK_F1:
            if assigned(FOnGetHelpContext) then
            begin // do a bit help support
              Context := 0;
              // traverse the tree structure up to the root
              repeat
                FOnGetHelpContext(Self, CurrentNode, Context);
                CurrentNode := CurrentNode.FParent;
              until (CurrentNode = Items.FRoot) or (Context <> 0);
              // if no help context could be found try the tree's one
              // or its parent's contexts
              ParentControl := Self;
              while assigned(ParentControl) and (Context = 0) do
              begin
                Context := ParentControl.HelpContext;
                ParentControl := ParentControl.Parent;
              end;
              if Context <> 0 then Application.HelpContext(Context);
            end;
        else
          // default handling (mostly used for incremental search)
          if CharCode > Word('0') then
          begin
            ClearSelection;
            inherited;
            FFirstSelection := CurrentNode;
            Exit;
          end
          else inherited; // all other keys we don't handle
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMLButtonDown(var Message: TWMLButtonDown);

// handles all mouse button clicks (left button)

var
  LastFocused,
  Node: TTreeNTNode;
  OldSelState,
  NewSelState: TSelectType;
  ShiftState: TShiftState;
  HitInfo: THitTests;

begin
  if IsEditing then
  begin
    inherited;
    Exit;
  end;

  SetFocus;

  // get more information about the hit
  HitInfo := GetHitTestInfoAt(Message.XPos, Message.YPos);
  // translate keys and filter out shift and ctrl key
  ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl];

  // a click on an item button is handled here (also special case check boxes)
  if (not (htOnItem in HitInfo) and not (toFullRowSelect in FOptions)) or
     (htOnStateIcon in HitInfo) then
  begin
    // user clicked on free space or a part not directly belonging to an item
    if ([htNoWhere, htOnRight, htOnIndent] * HitInfo <> []) and (toMultiSelect in FOptions)
    then
      with FSelectRec do
      begin
        // user starts a selection with a selection rectangle
        Pending := True;
        StartX := Message.XPos;
        StartY := Message.YPos;
        Rect.Left := 0;
        Rect.Top := 0;
        Rect.Right := 0;
        Rect.Bottom := 0;
        FHitList := TTreeNodeList.Create;
        FHitList.Capacity := Items.CountNotHidden;
        if ShiftState = [] then ClearSelection;
      end
    else
      // user clicked a state icon, try to handle this as check event
      if htOnStateIcon in HitInfo then
      begin
        SetCapture(Handle);
        FCheckNode := GetNodeAt(Message.XPos, Message.YPos);
        case FCheckNode.FStateIndex of
          ckCheckEmpty:
            SetCheckImage(FCheckNode, ckCheckDisabled);
          ckCheckChecked:
            SetCheckImage(FCheckNode, ckCheckGrayed);
          ckRadioEmpty:
            SetCheckImage(FCheckNode, ckRadioDisabled);
          ckRadioChecked:
            SetCheckImage(FCheckNode, ckRadioGrayed);
        end;
        Exit;
      end;
    // now do default..
    inherited;
    // ... and get out of here
    Exit;
  end;

  // here starts the handling for all cases with an item involved
  FDragged := False;
  FDragNode := nil;
  FClicked := False;
  // get the currently focused node
  LastFocused := Selected;

  // determine the node the user clicked on
  Node := GetNodeAt(Message.XPos, Message.YPos);

  // save style of newly clicked node
  NewSelState := stSet;
  if assigned(Node) and Node.Selected then NewSelState := stReset;

  if (Node = nil) or (not Node.Selected and (ShiftState = [])) then ClearSelection;

  // save selection state of the last focused node
  OldSelState := stReset;
  if assigned(LastFocused) and LastFocused.Selected then OldSelState := stSet;

  if ShiftState <> [] then
  begin
    // avoid node edit while selecting
    Selected := nil;
    // avoid changing last selection level (in CNNotify)
    Items.FCoreSelecting := True;
  end;
  inherited;
  // Support for constrained selection. I cannot prevent Windows to select the node
  // internally. Hence I have to remove the selection again (which will probably later again be set).
  if toLevelSelectConstraint in FOptions then Items.SelectNode(Node, stReset);
  Items.FCoreSelecting := False;

  if (DragMode = dmAutomatic)
     {$ifdef DFS_COMPILER_4_UP} and (DragKind = dkDrag) {$endif} then
  begin
    if FDragged          and
       assigned(Node)    then
    begin
      if (toMultiSelect in FOptions) then Items.SelectNode(Node, stSet)
                                     else SetSelection(Node);
      Node.Focused := True;
      BeginDrag(False);
    end
    else
    begin
      Perform(WM_LBUTTONUP, 0, MakeLong(Message.XPos, Message.YPos));
      if toMultiSelect in FOptions then HandleMultiSelection(LastFocused, OldSelState, Node, NewSelState, ShiftState);
    end;
  end
  else
    if toMultiSelect in FOptions then HandleMultiSelection(LastFocused, OldSelState, Node, NewSelState, ShiftState);
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMLButtonDblClk(var Message: TWMLButtonDblClk);

// in FullRowSelect mode a node should also expand and collapse when the user
// clicked on another place in the node's row than the text

var Node : TTreeNTNode;

begin
  inherited;
  if (FOptions * [toFullRowSelect, toSingleExpand]) = [toFullRowSelect]  then
  begin
    Node := GetNodeAt(Message.XPos, Message.YPos);
    if htOnRight in GetHitTestInfoAt(Message.XPos, Message.YPos) then
      if Node.Expanded then Node.Collapse(False)
                       else Node.Expand(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMLButtonUp(var Message: TWMLButtonUp);

begin
  // check if we are in selection mode
  if FSelectRec.Pending then
    with FSelectRec do
    begin
      // yes we are, so finish it ...
      Pending := False;
      FHitList.Free;
      // ... and remove the last focus rectangle if there was one
      if not IsRectEmpty(Rect) then DrawFocusRect(Rect);
    end;

  // an assigned check node shows an check event
  if assigned(FCheckNode) then
  begin
    FCheckNode.DoCheckClick;
    FCheckNode := nil;
    ReleaseCapture;
  end;

  // now do the standard behaviour
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMNCHitTest(var Message: TWMNCHitTest);

// pass non-client-area messages to the default handler to be able to
// scroll at design time (see also TScrollBox.WMNCHitTest)
// also stop the scroll timer in case the client area wasn't hit

begin
  DefaultHandler(Message);
  if Message.Result <> HTCLIENT then FScrollTimer.enabled := False;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMNotify(var Message: TWMNotify);

var Node       : TTreeNTNode;
    MaxTextLen : Integer;
    Pt         : TPoint;
    TextCopy   : String;

begin
  with Message do
    if (NMHdr^.code = TTN_NEEDTEXTW) and not (toInfoTip in Options) then
    begin
      // Work around NT COMCTL32 problem with tool tips >= 80 characters
      GetCursorPos(Pt);
      Pt := ScreenToClient(Pt);
      Node := GetNodeAt(Pt.X, Pt.Y);
      if (Node = nil) or (Node.Text = '') or
         (PToolTipTextW(NMHdr)^.uFlags and TTF_IDISHWND = 0) then Exit;

      // if {$ifdef DFS_COMPILER_3_UP} (GetComCtlVersion >= ComCtlVersionIE4) and {$endif}
      if  (Length(Node.Text) < 80) then
      begin
        inherited;
        Exit;
      end;

      TextCopy := Node.Text;
      MaxTextLen := SizeOf(PToolTipTextW(NMHdr)^.szText) div SizeOf(WideChar);
      if Length(TextCopy) >= MaxTextLen then
      begin
        SetLength(TextCopy, MaxTextLen - 4);
        TextCopy := TextCopy + '...';
      end;
      FillChar(PToolTipTextW(NMHdr)^.szText, MaxTextLen, 0);
      StringToWideChar(TextCopy, PToolTipTextW(NMHdr)^.szText, Length(TextCopy) * SizeOf(WideChar));
      PToolTipTextW(NMHdr)^.hInst := 0;
      SetWindowPos(NMHdr^.hwndFrom, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);
      Result := 1;
    end
    else inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if toNoEraseBkgnd in FOptions then Message.Result := 1
                                else inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTreeNT.WMDropFiles(var Msg: TWMDropFiles);  // [MJ]
var
  CFileName : array[0..MAX_PATH] of Char;
  FileList : TStringList;
  i, count : integer;
begin
  FileList := TStringList.Create;

  try
    count := DragQueryFile( Msg.Drop, $FFFFFFFF, CFileName, MAX_PATH );

    if ( count > 0 ) then
    begin
      for i := 0 to count-1 do
      begin
        DragQueryFile( Msg.Drop, i, CFileName, MAX_PATH );
        FileList.Add( CFileName );
      end;
    end;

    if assigned( FOnFileDropped ) then FOnFileDropped( self, FileList );

  finally
    FileList.Free;
    DragFinish(Msg.Drop);
  end;


end; // WMDropFiles

//------------------------------------------------------------------------------

initialization
  // create default image list for checkboxes and radio buttons
  CheckImages := TImageList.Create(nil);
  // we do share our image list but not after it is destroyed
  CheckImages.ShareImages := False;
  ConvertAndAddImages(CheckImages);
finalization
  CheckImages.Free;
end.
