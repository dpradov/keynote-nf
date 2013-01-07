{$I DFS.INC}

unit ComCtrls95;

{Version 2.94, Released 09/21/99}

interface
{.$DEFINE MJ_MODS}
{$D+}

{$ifdef ver90}      //remove this line for explict D2 usage
  {$Define delphi2} //Create .DCU for D2
{$endif}            //remove this line for explict D2 usage

{$ifdef ver110}     //This is for BCB3  Do not remove!
   {$Define delphi4}
   {$Define BCB3}
{$endif}

{$ifdef ver120}     //remove this line for explict D4 usage
  {$Define delphi4} //Create .DCU for D4
{$endif}            //remove this line for explict D4 usage

{$ifdef ver130}
  {$define Delphi4}
{$endif}

uses Messages, Windows, SysUtils, ComCtrls, CommCtrl, Classes, controls,
     Forms, Graphics, typinfo, dialogs, wideStrings, TntClasses,
  {$IFDEF DFS_DELPHI_6_UP} // *1
  RTLConsts
  {$ENDIF}
//{$IFDEF DFS_NO_DSGNINTF}     // [dpv]
//    DesignIntf,
//    DesignEditors
//{$ELSE}
//    DsgnIntf
//{$ENDIF}
     {$IFDEF MJ_MODS}
     ,gf_floatform
     {$ENDIF}
     //{$ifdef delphi4}
     {$ifdef DFS_DELPHI_4_UP}
        {$ifNDef BCB3}
        ,imglist
        {$endif}
     {$endif};

const
     CM_ComCtrls95Base      = wm_user+1;
     CM_TabSheetDraggedON   = CM_ComCtrls95Base+1;
     CM_TabSheetDraggedOFF  = CM_ComCtrls95Base+2;
     CM_ResetImages         = CM_ComCtrls95Base+3;


const
  {$IFDEF MJ_MODS}
    _TABS_ARE_DRAGGABLE = true;
  {$ELSE}
    _TABS_ARE_DRAGGABLE = false;
  {$ENDIF}

type
  TCustomTab95Control = Class;
  TTab95Sheet = class;

  TGripAlign = (gaLeft, gaRight, gaTop, gaBottom);
  TTextRotation = (trHorizontal, trVertical);
  TFloatState = (fsFloating, fsDocked);

  TDrawTab95Event = procedure(Page95Control: TCustomTab95Control; Caption: WideString;
    PageIndex:Integer; const Rect: TRect) of object;

  TTab95FloatEvent = procedure(Sender: TObject; FloatState: TFloatState) of object;
  TTab95TabTrackEvent = procedure(sender:TObject; TabIndex: integer) of object;

  TTab95Position = (tpTopLeft, tpBottomRight);
  TRotationAngle = (raUp, raLeft, raBottom, raRight);

  TCustomTab95Control = class(TWinControl)
  private
    FOnDblClick : TNotifyEvent;
    FMarkedPage: TTab95Sheet;
    FCanvas: TCanvas;
    FTabs: TTntStrings;
    FAnsiTabs: TStrings;
    FImages: TImageList;
    FSaveTabs: TTntStrings;
    FSaveTabIndex: Integer;
    FTabSize: TSmallPoint;
    FMultiLine: Boolean;
    fMultiSelect: boolean;
    FHint : tHintWindow;
    FUpdating: Boolean;
    FFlatButtons: Boolean;
    FFlatSeperators:Boolean;
    ffocusbutton:boolean;
    fHottrack:boolean;
    FScrollOpposite: Boolean;
    FTabPosition: TTab95Position;
    FVerticalTabs:Boolean;
    FTextRotation:TTextRotation;
    FOnChange, FOnTabShift: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TDrawTab95Event;
    FButtonStyle: boolean;
    FImageChangeLink: TChangeLink;
    fmouseovertab, fMouseDragTab : integer;
    fTabShifting:boolean;
    FTabInactiveColor: TColor;
    FTabInactiveFont: TFont;
    fOnTabTrack: TTab95TabTrackEvent;
    fMSDraw : boolean;

    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);  message WM_LBUTTONDBLCLK;
    function GetDisplayRect: TRect;
    function GetTabIndex: Integer;
    procedure SetMultiLine(Value: Boolean);
    procedure SetScrollOpposite(Value: Boolean);
    procedure SetTabHeight(Value: Smallint);
    procedure SetTabIndex(Value: Integer);
    procedure SetTabPosition(Value: TTab95Position);
    procedure SetTabsVertical(value:boolean);
    procedure SetTextRotation(value:tTextRotation);
    procedure SetButtonStyle(value:boolean);
    procedure SetTabs(Value: TTntStrings);
    procedure SetTabWidth(Value: Smallint);
    procedure SetImageList(value:TImageList);
    procedure SetFlatButtons(value:boolean);
    procedure SetFlatSeperators(value:boolean);
    function GetFlatSeperators:boolean;
    procedure SetMultiSelect(value:boolean);
    procedure SetMSDraw(value:Boolean);
    procedure Setfocusbutton(value:boolean);
    procedure ImageListChange(Sender: TObject);
    procedure TabsChanged;
    procedure UpdateTabSize;
    procedure RotateText(Text:wideString; TabPosition:TTab95Position; WorkCanvas:Tcanvas; R:TRect; xadj, yadj:integer; UseGrayString : boolean);
    procedure RotateImage(RotatedBMP, OriginalBMP:tbitmap; angle:integer);
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMNotifyFormat(var Message: TMessage); message WM_NOTIFYFORMAT;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure wmmouseleave(var message:tmessage); message cm_mouseleave;
    procedure wmnchittest(var message:TWMNCHitTest); message wm_nchittest;
    procedure TabFontChanged(Sender: TObject); virtual;
    procedure SetTabInactiveColor(Value: TColor);
    procedure SetTabInactiveFont(Value: TFont);
  protected
    procedure MyRecreateWND;
    procedure loaded; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CanChange: Boolean; dynamic;
    procedure Change; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DisplayTabHint(TabIndex:integer); virtual; abstract;
    procedure DrawTab(Caption: WideString; PageIndex:integer; const wRect: TRect);
    property DisplayRect: TRect read GetDisplayRect;
    property FocusButtons:boolean read ffocusbutton write setfocusbutton default false;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property MultiSelect:boolean read fMultiselect write SetMultiselect default false;
    property ScrollOpposite: Boolean read FScrollOpposite write SetScrollOpposite default False;
    property TabHeight: Smallint read FTabSize.Y write SetTabHeight default 0;
    property ButtonStyle:boolean read fbuttonstyle write SetButtonStyle default false;
    property FlatButtons:boolean read fflatbuttons write SetFlatButtons default false;
    property FlatSeperators: boolean read GetFlatSeperators write setflatseperators default true;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTab95Position read FTabPosition write SetTabPosition default tpTopLeft;
    property Tabs: TTntStrings read FTabs write SetTabs;
    property TabWidth: Smallint read FTabSize.X write SetTabWidth default 0;
    property TextRotation:TTextRotation read FTextRotation write SetTextRotation default trHorizontal;
    property UseMSDrawing:boolean read fmsdraw write setmsdraw default false;
    property VerticalTabs:boolean read FVerticalTabs write SetTabsVertical default false;
    property Images:TImageList read FImages write SetImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTab95Event read FOnDrawTab write FOnDrawTab;
    property OnTabTrack:TTab95TabTrackEvent read fontabtrack write fontabtrack;
    property OnTabShift:TNotifyEvent read FOnTabShift write FOnTabShift;
    property canvas: TCanvas read FCanvas;
    property HotTrack:boolean read fhottrack write fhottrack;
    property TabInactiveColor: TColor read FTabInactiveColor write SetTabInactiveColor default $00A4A0A0;
    property TabInactiveFont: TFont read FTabInactiveFont write SetTabInactiveFont;
    property AllowTabShifting:boolean read fTabShifting write fTabShifting default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTabAt(x,y:integer):integer;
    property TabStop default True;
  published
    {$ifdef delphi4}
       {$ifNDef BCB3}
    property Anchors;
    property Constraints;
       {$endif}
    {$endif}
  end;

  TTab95Control = class(TCustomTab95Control)
  private
    FImageIndexList: TStrings;
    FImageIndexSave: TStrings;
    ftabHints:tStrings;
    procedure SetTabHints(value:TStrings);
    procedure SetImageIndexList(value:TStrings);
    function GetImageIndexList:TStrings;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure DisplayTabHint(TabIndex:integer); override;
    procedure loaded; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    property DisplayRect;
  published
    property AllowTabShifting;
    property ButtonStyle;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FlatButtons;
    property FlatSeperators;
    property FocusButtons;
    property Font;
    property HotTrack;
    property Images;
    property TabInactiveColor;
    property TabInactiveFont;
    property MultiLine;
    property MultiSelect;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollOpposite;
    property ShowHint;
    property TabHeight;
    property TabHints:tstrings read ftabHints write SetTabHints;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property Tabs;
    property TabStop;
    property TabWidth;
    property TextRotation;
    property UseMSDrawing;
    property VerticalTabs;
    property Visible;
    property Color nodefault;
    property ImageIndexes:TStrings read GetImageIndexList write SetImageIndexList stored TRUE;
    property OnChange;
    property OnChanging;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnDrawTab;
    property OnTabTrack;
    property OnTabShift;
  end;

  TPage95Control = class;

  {$IFDEF MJ_MODS}
  TFloatingForm = class(TFloatForm)
  {$ELSE}
  TFloatingForm = class(TForm)
  {$ENDIF}
  private
    { Private declarations }
    FSheet : TTab95Sheet;
    procedure DoCloseWindow(Sender: TObject; var Action: TCloseAction);
    procedure DoDestroyWindow(Sender: TObject);

  public
    { Public declarations }
    constructor create(AOwner:Tcomponent); override;
    property TabSheet:TTab95Sheet read FSheet write FSheet;
  end;

  TTab95Sheet = class(TWinControl)
  private
//Floating Vars...
    fOldMousePos                 : TPoint;    { previous mouse position                      }
    fMouseOffset                 : TPoint;    { Mouse coordinates in relation to client rect }
    fDragStart                   : boolean;
    fDragging                    : boolean;   { true when dragging                           }
    fDragRect                    : TRect;     { position of rectangle while dragging         }
    fDragable                    : boolean;   { true to make this dragable                   }
    fWidth,
    fHeight                      : integer;   { width and height of client area at dragstart }
    fOldPageControl              : TPage95Control; { saves the page control so that it can be reset }
    ffloating                    : boolean;

    FOnFloatChange: TTab95FloatEvent;

    { fields for the form }
    fFloatingForm                : TFloatingForm; { form to use when floating           }
    fFloatOnTop                  : Boolean;
    fcanvas : tcanvas;
    fGripAlign : TGripAlign;
//Normal

    FPageControl: TPage95Control;
    FPageIndex : integer;
    FTabVisible: Boolean;
    FTabShowing: Boolean;
    fimageindex : integer;
    fTabHint : string;

    // [MJ] 12-Oct-1999
    fPrimaryControl : TWinControl;
    fPrimaryObject : TObject;

    function GetPageIndex: Integer;
    function GetTabIndex: Integer;
    procedure SeTPage95Control(APageControl: TPage95Control);
    procedure setImageIndex(value:integer);
    function GetImageIndex:integer;
    procedure SetPageIndex(Value: Integer);
    procedure SetTabShowing(Value: Boolean);
    procedure SetTabVisible(Value: Boolean);
    procedure SetFloatOnTop(value:boolean);
    procedure UpdateTabShowing;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
//Floating
    procedure WMLButtonDown(var Msg : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Msg : TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Msg : TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMPaint(var msg:TWMPaint); message WM_Paint;

    procedure DrawDraggingRect(MousePos : TPoint); { Draws the focus rectangle at appropriate place}
    procedure setDragOption(value:boolean);
    procedure setGripAlign(value:TGripAlign);
    function GetGripRect:TRect;
    function GetGripperRect:TRect;
    function GetCaption: wideString;
    procedure SetCaption(const value: wideString);

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ReadState(Reader: TReader); override;
    procedure SetEnabled(Value: boolean);
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FloatTabSheet;
    procedure DockTabSheet;
    function GetClientRect:TRect; override;
    property PageControl: TPage95Control read FPageControl write SeTPage95Control;
    property TabIndex: Integer read GetTabIndex;
    property FloatingForm: TFloatingForm read fFloatingForm;
    property GripRect: TRect read GetGripperRect;
    property Floating: Boolean read fFloating;

    // [MJ] 12-Oct-1999
    // NOTES: The Tabsheet does not create or free these objects.
    // They may be assigned by an "external" procedure to easily keep
    // track of the main control the tabsheet holds (e.g. a TMemo,
    // especially if it's the ONLY control on the tabsheet), and of
    // a user object the tabsheet (or the memo in the example above)
    // are logically bound to.
    {
    property PrimaryControl : TWinControl read fPrimaryControl write fPrimaryControl;
    }
    property PrimaryObject : TObject read fPrimaryObject write fPrimaryObject;

  published
    {$ifdef delphi4}
       {$ifNDef BCB3}
    property Constraints;
       {$endif}
    {$endif}
    property Caption: wideString read GetCaption write SetCaption;
    property FloatOnTop : boolean read ffloatontop write SetFloatOnTop default false;
    property Dragable : boolean read fDragable write SetDragOption default false; //Floating
    property Enabled;
    property Font;
    property Color;
    property GripAlign : TGripAlign read FGripAlign write SetGripAlign;
    property Height stored False;
    property ImageIndex : integer read getimageindex write setimageindex;
    property Left stored False;
    property StaticPageIndex : integer read fpageindex write fpageindex;
    property TabHint:string read ftabhint write ftabhint;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible stored true;
    property Tag;
    property Top stored False;
    property Visible stored False;
    property Width stored False;

    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnFloatChange: TTab95FloatEvent read FOnFloatChange write FOnFloatChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TPage95Control = class(TCustomTab95Control)
  private
    FPages: TList;
    FActivePage: TTab95Sheet;
    FFloatingPageCount : Integer;
    FRemoveLastTab: Boolean;
    FOnFloatChange: TTab95FloatEvent;
    procedure ChangeActivePage(Page: TTab95Sheet);
    procedure DeleteTab(Page: TTab95Sheet);
    function GetPage(Index: Integer): TTab95Sheet;
    function GetPageCount: Integer;
    procedure InsertPage(Page: TTab95Sheet);
    procedure InsertTab(Page: TTab95Sheet);
    procedure MoveTab(CurIndex, NewIndex: Integer);
    procedure RemovePage(Page: TTab95Sheet);
    procedure SetActivePage(Page: TTab95Sheet);
    procedure UpdateTab(Page: TTab95Sheet);
    procedure UpdateActivePage;
    function GetAllowPageToFloat:Boolean;
    procedure CMTabDraggedOff(var Message:TMessage); message CM_TabSheetDraggedOFF;
    procedure CMTabDraggedOn(var Message:TMessage); message CM_TabSheetDraggedON;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMResetImages(var Msg:TMessage); message CM_ResetImages;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure ResetImageInfo;
  protected
    procedure Change; override;
    {$IFDEF delphi2}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);
    {$ELSE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    {$ENDIF}
    procedure DisplayTabHint(TabIndex:integer); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure ShowControl(AControl: TControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TTab95Sheet;
      GoForward, CheckTabVisible: Boolean): TTab95Sheet;
    procedure SelectNextPage(GoForward: Boolean);
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TTab95Sheet read GetPage;
    property AllowPageToFloat:Boolean read GetAllowPageToFloat;
  published
    property ActivePage: TTab95Sheet read FActivePage write SetActivePage;
    property MarkedPage : TTab95Sheet read FMarkedPage write FMarkedPage;
    property AllowTabShifting;
    property Images;
    property ButtonStyle;
    property Color nodefault;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FlatButtons;
    property FlatSeperators;
    property FocusButtons;
    property Font;
    property HotTrack;
    property TabInactiveColor;
    property TabInactiveFont;
    property MultiLine;
    property MultiSelect;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RemoveLastTab: Boolean read FRemoveLastTab write FRemoveLastTab default False;
    property ScrollOpposite;
    property ShowHint;
    property TabHeight;
    property TabOrder;
    property TabPosition;
    property TabStop;
    property TabWidth;
    property TextRotation;
    property UseMSDrawing;
    property VerticalTabs;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnDblClick read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFloatChange: TTab95FloatEvent read FOnFloatChange write FOnFloatChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnDrawTab;
    property OnTabTrack;
    property OnTabShift;
  end;

implementation

{$IFDEF delphi2}
{$R FloatingFormD2.DFM}
{$ELSE}
{$R FloatingForm.DFM}
{$ENDIF}

uses Consts, ComStrs, TntControls, TntSysUtils, gf_miscvcl;

const
     SIllegalButtonPosition = 'Buttons are only supported when aligned along the top';

     {$ifdef delphi4}
     sTabAccessError = 'Tab control access error';
     {$endif}

//*** Start of IE4 ONLY Properties ***
     TCS_MULTISELECT       = $0004;
     TCS_FLATBUTTONS       = $0008;
     TCS_EX_FLATSEPARATORS = $00000001;
     TCS_EX_REGISTERDROP   = $00000002;

     TCIS_HIGHLIGHTED      = $0002;

     TCM_HIGHLIGHTITEM     = (TCM_FIRST + 51);
     TCM_SETEXTENDEDSTYLE  = (TCM_FIRST + 52);  // optional wParam == mask
     TCM_GETEXTENDEDSTYLE  = (TCM_FIRST + 53);

     TCN_GETOBJECT         = (TCN_FIRST - 3);   // WM_Notify code
//*** End of IE4 ONLY Properties ***

     GripSize = 7;

{$IFDEF delphi2}

//Beginning of the D2 declairations

     TCS_SCROLLOPPOSITE    = $0001;
     TCS_BOTTOM            = $0002;
     TCS_RIGHT             = $0002;
     TCS_HOTTRACK          = $0040;
     TCS_VERTICAL          = $0080;
     TCS_BUTTONS           = $0100;

     ICC_TAB_CLASSES      = $00000008; // tab, tooltips

     cctrl = 'comctl32.dll';
     sInvalidComCtl32 = 'This control requires version 4.70 or greater of COMCTL32.DLL';
     sPageIndexError = '%d is an invalid PageIndex value.  PageIndex must be ' +
                       'between 0 and %d';

type
  PInitCommonControlsEx = ^TInitCommonControlsEx;
  TInitCommonControlsEx = packed record
    dwSize: DWORD;             // size of this structure
    dwICC: DWORD;              // flags indicating which classes to be initialized
  end;

var
  ComCtl32DLL: THandle;
  _InitCommonControlsEx: function(var ICC: TInitCommonControlsEx): Bool stdcall;

procedure InitCommonControls; stdcall; forward;
procedure InitCommonControls; external cctrl name 'InitCommonControls';

procedure InitComCtl;
begin
  if ComCtl32DLL = 0 then begin
    ComCtl32DLL := GetModuleHandle(cctrl);
    if (ComCtl32DLL >= 0) and (ComCtl32DLL < 32) then
      ComCtl32DLL := 0
    else
      @_InitCommonControlsEx := GetProcAddress(ComCtl32DLL, 'InitCommonControlsEx');
  end;
end;

function InitCommonControlsEx(var ICC: TInitCommonControlsEx): Bool;
begin
  if ComCtl32DLL = 0 then InitComCtl;
  Result := Assigned(_InitCommonControlsEx) and _InitCommonControlsEx(ICC);
end;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;

procedure CheckCommonControl(CC: Integer);
begin
  if not InitCommonControl(CC) then
    raise EComponentError.Create(SInvalidComCtl32);
end;

//End of the D2 Declarations.

{$ENDIF}

{ TTabStrings }

  type
  TTntTabStrings = class(TTntStrings)
  private
    FTabControl: TCustomTab95Control;
    FAnsiTabs: TStrings{TNT-ALLOW TStrings};
  protected
    function Get(Index: Integer): WideString; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: WideString); override;
  end;

type
TTabStrings = TTntTabStrings;

  TAnsiTabStrings = class(TStrings)
  private
    FTabControl: TCustomTab95Control;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

procedure TabControlError(msg:string); overload;
begin
(*  {$ifdef delphi2}
  Raise EListError.createres(sTabAccessError);
  {$Else}
  raise EListError.Create(sTabAccessError+#13#10+'Violation occured in '+msg);
  {$endif}
*)
    {$IFDEF DFS_COMPILER_3_UP}
    raise EListError.Create('<sTabAccessError> '+#13#10+'Violation occured in '+msg);
    {$ELSE}
    raise EListError.CreateRes('<sTabAccessError> '+#13#10+'Violation occured in '+msg);
    {$ENDIF}

end;


procedure TAnsiTabStrings.Clear;
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEALLITEMS, 0, 0) = 0 then
    TabControlError('TTabStrings.Clear');
  FTabControl.TabsChanged;
end;

procedure TAnsiTabStrings.Delete(Index: Integer);
begin
  if SendMessage(FTabControl.Handle, TCM_DELETEITEM, Index, 0) = 0 then
    TabControlError('TTabStrings.Delete');
  FTabControl.TabsChanged;
end;

function TAnsiTabStrings.Get(Index: Integer): String;
var
  TCItem: TTCItem;
  Buffer: array[0..4095] of Char;
begin
  TCItem.mask := TCIF_TEXT;
  TCItem.pszText := Buffer;
  TCItem.cchTextMax := SizeOf(Buffer);
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then TabControlError('TTabStrings.Get');
  Result := Buffer;
end;

function TAnsiTabStrings.GetCount: Integer;
begin
  Result := SendMessage(FTabControl.Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TAnsiTabStrings.GetObject(Index: Integer): TObject;
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index,
    Longint(@TCItem)) = 0 then TabControlError('TTabStrings.GetObject');
  Result := TObject(TCItem.lParam);
end;

procedure TAnsiTabStrings.Put(Index: Integer; const S: String);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT;
  TCItem.pszText := PChar(S);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then TabControlError('TTabStrings.Put');
  FTabControl.TabsChanged;
end;

procedure TAnsiTabStrings.PutObject(Index: Integer; AObject: TObject);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_PARAM;
  TCItem.lParam := Longint(AObject);
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then TabControlError('TTabStrings.PutObject');
end;

procedure TAnsiTabStrings.Insert(Index: Integer; const S: String);
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_TEXT;
  TCItem.pszText := PChar(S);
  if SendMessage(FTabControl.Handle, TCM_INSERTITEM, Index,
    Longint(@TCItem)) < 0 then TabControlError('TTabStrings.Insert');
  FTabControl.TabsChanged;
end;

procedure TAnsiTabStrings.SetUpdateState(Updating: Boolean);
begin
  FTabControl.FUpdating := Updating;
  SendMessage(FTabControl.Handle, WM_SETREDRAW, Ord(not Updating), 0);
  if not Updating then
  begin
    FTabControl.Invalidate;
    FTabControl.TabsChanged;
  end;
end;

{ TImageIndexStrings }

type
  TImageIndexStrings = class(TStrings)
  private
    FTabControl: TCustomTab95Control;
    indexcounter:integer;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: string): Integer; override;
    procedure Assign(Source : TPersistent); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

function TImageIndexStrings.Add(const S: string): Integer;
begin
     //Stub Function
     put(indexcounter,s);
     result := indexcounter;
     inc(indexcounter);
end;

procedure TImageIndexStrings.Assign(Source : TPersistent);
var
   loop : integer;
begin
     if not (Source is TStrings) then
       raise Exception.Create('Can''t assign '+source.classname+' to '+self.classname+'.');

     loop := 0;
     while (loop < TStrings(Source).count) and (loop < self.count) do
     begin
          self.strings[loop] := TStrings(Source).Strings[loop];
          inc(loop);
     end;
end;

procedure TImageIndexStrings.Clear;
begin
     //Stub Function
     indexcounter := 0;
end;

procedure TImageIndexStrings.Delete(Index: Integer);
begin
     //Stub Function
end;

function TImageIndexStrings.Get(Index: Integer): string;
var
  TCItem: TTCItem;
begin
  TCItem.mask := TCIF_IMAGE;
  if SendMessage(FTabControl.Handle, TCM_GETITEM, Index, Longint(@TCItem)) = 0 then TabControlError('TImageIndexStrings.Get');
  Result := inttostr(TCItem.iImage);
end;

function TImageIndexStrings.GetCount: Integer;
begin
  Result := SendMessage(FTabControl.Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TImageIndexStrings.GetObject(Index: Integer): TObject;
begin
     result := nil; //Stub Function
end;

procedure TImageIndexStrings.Put(Index: Integer; const S: string);
var
  TCItem: TTCItem;
begin
  if index >= count then exit;
  TCItem.mask := TCIF_IMAGE;
  try
     if strtoint(s) < 0 then
        TCItem.iImage := -1
     else
        TCItem.iImage := strtoint(S);
  except
        TCItem.iImage := -1;
  end;
  if SendMessage(FTabControl.Handle, TCM_SETITEM, Index,
    Longint(@TCItem)) = 0 then TabControlError('TImageIndexStrings.Put');
  FTabControl.TabsChanged;
end;

procedure TImageIndexStrings.PutObject(Index: Integer; AObject: TObject);
begin
     //Stub Function
end;

procedure TImageIndexStrings.Insert(Index: Integer; const S: string);
begin
     //Stub Function
end;

procedure TImageIndexStrings.SetUpdateState(Updating: Boolean);
begin
     // Stub Function
end;

{ TCustomTab95Control }

procedure TCustomTab95Control.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  if assigned( FOnDblClick ) then
    FOnDblClick( self );
  // inherited;
end;


constructor TCustomTab95Control.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 289;
  Height := 193;
  TabStop := True;
  fTabShifting := false;
  ControlStyle := [csAcceptsControls, csDoubleClicks];
  fhint := thintwindow.create(self);
  FTabs := TTabStrings.Create;
  FAnsiTabs:= TAnsiTabStrings.Create;
  TAnsiTabStrings(FAnsiTabs).FTabControl := Self;
  TTabStrings(FTabs).FTabControl := Self;
  TTabStrings(FTabs).FAnsiTabs := FAnsiTabs;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  fmouseovertab := -1;
  fflatbuttons := false;
  fFlatSeperators := true;
  fmultiselect := false;
  fmsdraw := false;
  hottrack := false;
  FTabInactiveColor := $00A4A0A0;
  FTabInactiveFont := TFont.Create;
  FTabInactiveFont.OnChange := TabFontChanged;
end;

procedure TCustomTab95Control.loaded;
begin
     inherited;
     updatetabsize;
end;

destructor TCustomTab95Control.Destroy;
begin
  TTntTabStrings(FTabs).FTabControl := nil;
  TTntTabStrings(FTabs).FAnsiTabs := nil;
  FreeAndNil(FTabs);
  FreeAndNil(FSaveTabs);

  fhint.free;
  FCanvas.Free;
  FImageChangeLink.Free;
  FTabInactiveFont.Free;
  inherited Destroy;
end;

function TCustomTab95Control.CanChange: Boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then FOnChanging(Self, Result);
end;

procedure TCustomTab95Control.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomTab95Control.CreateParams(var Params: TCreateParams);
const
  {$ifdef delphi4}
  AlignStyles: array[TTab95Position] of uint = (0, TCS_BOTTOM);
  {$else}
  AlignStyles: array[TTab95Position] of Integer = (0, TCS_BOTTOM);
  {$endif}
begin
  InitCommonControl(ICC_TAB_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TABCONTROL);
  with Params do
  begin
//Buttons or Tabs
    if (FButtonStyle) and (Style and TCS_Buttons = 0) then Style := Style or TCS_BUTTONS
    else if (not (FButtonStyle)) and (Style and TCS_Buttons <> 0) then Style := Style xor TCS_Buttons;
//Normal or Vertical
    if (FVerticalTabs) and (Style and TCS_Vertical = 0) then Style := Style or TCS_VERTICAL
    else if (not (FVerticalTabs)) and (Style and TCS_Vertical <> 0) then Style := Style xor TCS_VERTICAL;
//Tab positioning
    Style := Style or WS_CLIPCHILDREN or AlignStyles[FTabPosition];
//Tab movement and arangment
    if FMultiLine then Style := Style or TCS_MULTILINE;
    if FScrollOpposite then Style := Style or TCS_SCROLLOPPOSITE;
//Misc. Tab settings
    if not TabStop then Style := Style or TCS_FOCUSNEVER;
    if FTabSize.X <> 0 then Style := Style or TCS_FIXEDWIDTH;
    if fflatbuttons then Style := style or TCS_FLATBUTTONS;
    if fMultiSelect then Style := style or TCS_MultiSelect;
    if not fmsdraw then style := style or TCS_OWNERDRAWFIXED;
    if ffocusbutton then style := style or TCS_FOCUSONBUTTONDOWN;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW) or CS_DBLCLKS;
  end;
end;

procedure TCustomTab95Control.CreateWnd;
begin
  inherited CreateWnd;
  if Integer(FTabSize) <> 0 then UpdateTabSize;
  if FSaveTabs <> nil then
  begin
    FTabs.Assign(FSaveTabs);
    SetTabIndex(FSaveTabIndex);
    FSaveTabs.Free;
    FSaveTabs := nil;
  end;
end;

procedure TCustomTab95Control.DestroyWnd;
begin
  if FTabs.Count > 0 then
  begin
    FSaveTabs := TTntStringList.Create;
    FSaveTabs.Assign(FTabs);
    FSaveTabIndex := GetTabIndex;
  end;
  inherited DestroyWnd;
end;

procedure TCustomTab95Control.AlignControls(AControl: TControl;
  var Rect: TRect);
begin
  Rect := DisplayRect;
  inherited AlignControls(AControl, Rect);
end;

function TCustomTab95Control.GetDisplayRect: TRect;
begin
  Result := ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, Integer(@Result));
  Inc(Result.Top, 2);
end;

function TCustomTab95Control.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

procedure TCustomTab95Control.SetMultiLine(Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    if not Value then FScrollOpposite := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetMSDraw(value:Boolean);
begin
  if FMSDraw <> Value then
  begin
    FMSDraw := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.Setfocusbutton(value:boolean);
begin
  if Ffocusbutton <> Value then
  begin
    Ffocusbutton := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetScrollOpposite(Value: Boolean);
begin
  if FScrollOpposite <> Value then
  begin
    FScrollOpposite := Value;
    if Value then FMultiLine := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetTabInactiveColor(Value: TColor);
begin
   if Value <> FTabInactiveColor then
   begin
      FTabInactiveColor := Value;
      Repaint;
   end;
end;

procedure TCustomTab95Control.TabFontChanged(Sender: TObject);
begin
  if HandleAllocated then
    Repaint;
end;

procedure TCustomTab95Control.SetTabInactiveFont(Value: TFont);
begin
   FTabInactiveFont.Assign(Value);
end;

procedure TCustomTab95Control.SetTabHeight(Value: Smallint);
begin
  if FTabSize.Y <> Value then
  begin
    if Value < 0 then
      {$ifdef delphi2}
      raise EInvalidOperation.CreateResFmt(SPropertyOutOfRange, [Self.Classname]);
      {$Else}
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
      {$EndIF}

    FTabSize.Y := Value;
    UpdateTabSize;
  end;
end;

procedure TCustomTab95Control.SetTabIndex(Value: Integer);
begin
  SendMessage(Handle, TCM_SETCURSEL, Value, 0);
end;

procedure TCustomTab95Control.SetTabPosition(Value: TTab95Position);
begin
  if ButtonStyle then
  begin
       raise EInvalidOperation.CreateFmt(SIllegalButtonPosition, [nil]);
       exit;
  end;
  if FTabPosition <> Value then
  begin
    FTabPosition := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetTabsVertical(Value:boolean);
begin
  if ButtonStyle then
  begin
       raise EInvalidOperation.CreateFmt(SIllegalButtonPosition, [nil]);
       exit;
  end;
  if FVerticalTabs <> Value then
  begin
    FVerticalTabs := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetTextRotation(Value:TTextRotation);
begin
  if FTextRotation <> Value then
  begin
    FTextRotation := Value;
    MyRecreateWND;
  end;
end;

procedure TCustomTab95Control.SetTabs(Value: TTntStrings);
begin
  FTabs.Assign(Value);
end;

procedure TCustomTab95Control.SetTabWidth(Value: Smallint);
var
  OldValue: Smallint;
begin
  if FTabSize.X <> Value then
  begin
    if Value < 0 then
      {$IFDef delphi2}
      raise EInvalidOperation.CreateResFmt(SPropertyOutOfRange, [Self.Classname]);
      {$Else}
      raise EInvalidOperation.CreateFmt(SPropertyOutOfRange, [Self.Classname]);
      {$ENDIF}
    OldValue := FTabSize.X;
    FTabSize.X := Value;
    if (OldValue = 0) or (Value = 0) then
      MyRecreateWND else
      UpdateTabSize;
  end;
end;

procedure TCustomTab95Control.TabsChanged;
begin
  if not FUpdating then
  begin
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, SIZE_RESTORED,
        Word(Width) or Word(Height) shl 16);
    Realign;
  end;
end;

procedure TCustomTab95Control.SetImageList(value:TImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    sendmessage(Handle, TCM_SetImageList, 0, images.Handle)
  end
  else sendmessage(Handle, TCM_SetImageList, 0, 0);
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
end;

procedure TCustomTab95Control.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
  begin
       sendmessage(Handle, TCM_SetImageList, 0, fimages.Handle);
       RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
  end;
end;

procedure TCustomTab95Control.UpdateTabSize;
begin
  SendMessage(Handle, TCM_SETITEMSIZE, 0, Integer(FTabSize));
  TabsChanged;
end;

procedure TCustomTab95Control.WMDestroy(var Message: TWMDestroy);
var
  FocusHandle: HWnd;
begin
  FocusHandle := GetFocus;
  if (FocusHandle <> 0) and ((FocusHandle = Handle) or
    IsChild(Handle, FocusHandle)) then
    Windows.SetFocus(0);
  inherited;
end;

procedure TCustomTab95Control.WMEraseBkgnd(var Message: TMessage);
begin
  {$IFNDEF VER90}
  if FDoubleBuffered and (Message.wParam <> Message.lParam) then Message.Result := 1
  else
  {$ENDIF}
  inherited;
end;

procedure TCustomTab95Control.WMNotifyFormat(var Message: TMessage);
begin
  with Message do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TCustomTab95Control.WMSize(var Message: TMessage);
begin
  inherited;
  RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE);
end;

procedure TCustomTab95Control.CMFontChanged(var Message);
begin
  inherited;
  if HandleAllocated then Perform(WM_SIZE, 0, 0);
end;

procedure TCustomTab95Control.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    Message.Msg := WM_SYSCOLORCHANGE;
    DefaultHandler(Message);
  end;
end;

procedure TCustomTab95Control.CMTabStopChanged(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then MyRecreateWND;
end;

procedure TCustomTab95Control.CNNotify(var Message: TWMNotify);
begin
  with Message.NMHdr^ do
    case code of
      TCN_SELCHANGE:
        Change;
      TCN_SELCHANGING:
        begin
          Message.Result := 1;
          if CanChange then Message.Result := 0;
        end;
    end;
end;

procedure TCustomTab95Control.DrawTab(Caption: wideString; PageIndex:integer; const wRect: TRect);
var
  TCItem: TTCItem;
  w1rect : trect;
  x,y : integer;
  xadj, yadj : integer;
  UseGrayString : boolean;
begin
  w1rect := wrect;
  if Assigned(FOnDrawTab) then
     FOnDrawTab(Self, Caption, PageIndex, w1Rect) else
  begin

    if (( FMarkedPage <> nil ) and
       ( FMarkedPage.TabIndex = PageIndex )) then
    begin
      fcanvas.brush.color := clInfoBK;
      fcanvas.font := Font;
      xadj := 1;
      yadj := 0;
    end
    else
    begin
       if pageindex = TabIndex then
       begin
          fcanvas.brush.color := Color;
          fcanvas.font := Font;
          xadj := 1;
          yadj := 0;
       end
       else
       begin
          fcanvas.brush.color := FTabInactiveColor;
          fcanvas.font := FTabInactiveFont;
          xadj := 0;
          yadj := 0;
       end;
     end;

     if fhottrack and (pageindex = fmouseovertab) then
      fcanvas.font.color := clhighlight;
     with fcanvas do
     begin
          fillrect(w1rect);
          caption := trim(caption);
          if (images <> nil) then
          begin
               tcitem.mask := TCIF_Image;
               if sendmessage(self.handle,TCM_GetItem,pageindex,LongInt(@tcitem)) = 0 then
                  TabControlError('TCustomTab95Control.DrawTab');
               if (tcitem.iImage <> -1) then
               begin
                    if not (verticaltabs) then
                    begin
                         if PageIndex = TabIndex then
                         begin
                              xadj := 1;
                              yadj := -1;
                         end
                         else
                         begin
                              xadj := 0;
                              yadj := 1;
                         end;
                         x := w1rect.left + 5 + xadj;
                         y := w1rect.top + (((w1rect.bottom-w1rect.top) div 2) - (images.height div 2)) + yadj;
                         images.draw(fcanvas,x,y,tcitem.iImage);
                         w1rect.left := x + 5 + images.Width;
                    end
                    else
                    begin
                         if PageIndex = TabIndex then
                         begin
                              xadj := -1;
                              yadj := 1;
                         end
                         else
                         begin
                              xadj := 1;
                              yadj := 0;
                         end;
                         x := w1rect.left + (((w1rect.right-w1rect.left) div 2) - (images.width div 2)) + xadj;
                         y := w1rect.top + 5 + yadj;
                         images.draw(fcanvas,x,y,tcitem.iImage);
                         w1rect.top := y + 5 + images.height;
                    end;
               end
               else
               begin
                    if not (verticaltabs) then
                         w1rect.left := w1rect.left + 5 + xadj
                    else
                         w1rect.top := w1rect.top + 5 + yadj;
               end;
          end
          else
          begin
               if not (verticaltabs) then
                    w1rect.left := w1rect.left + 5 + xadj
               else
                    w1rect.top := w1rect.top + 5 + yadj;
          end;

          UseGrayString := false;

          if (self is TPage95Control) then
               UseGrayString := Not TPage95Control(self).Pages[PageIndex].Enabled;

          if TextRotation = trVertical then
             RotateText(caption, TabPosition, fcanvas, w1rect, xadj, 0, UseGrayString)
          else
          begin
               w1Rect.top := w1Rect.top + yadj;
               w1Rect.bottom := w1Rect.bottom + yadj;

               if UseGrayString then
               begin
                    OffsetRect(w1Rect, 1, 1);
                    fCanvas.Font.Color := clBtnHighlight;
                    DrawTextW(fCanvas.Handle, PWideChar(caption), Length(caption), w1Rect, DT_EXPANDTABS or {DT_CENTER or} DT_VCENTER or DT_SINGLELINE);
//                    textrect(w1rect,w1rect.left + ((w1rect.right-w1rect.left)div 2)-(textwidth(caption)div 2),w1rect.top + ((w1rect.bottom-w1rect.top)div 2)-(textheight(caption)div 2)+yadj,caption);
                    OffsetRect(w1Rect, -1, -1);
                    fCanvas.Font.Color := clBtnShadow;
                    fCanvas.Brush.style := bsClear;
                    DrawTextW(fCanvas.Handle, PWideChar(caption), Length(caption), w1Rect, DT_EXPANDTABS or {DT_CENTER or} DT_VCENTER or DT_SINGLELINE);
//                    textrect(w1rect,w1rect.left + ((w1rect.right-w1rect.left)div 2)-(textwidth(caption)div 2),w1rect.top + ((w1rect.bottom-w1rect.top)div 2)-(textheight(caption)div 2)+yadj,caption);
                    fCanvas.Brush.style := bsSolid;
               end
               else
               begin
                   DrawTextW(fCanvas.Handle, PWideChar(caption), Length(caption), w1Rect, DT_EXPANDTABS or {DT_CENTER or} DT_VCENTER or DT_SINGLELINE);
               end;
          end;
     end;
  end;
end;

procedure TCustomTab95Control.RotateImage(RotatedBMP, OriginalBMP:tbitmap; angle:integer);
TYPE
    pTRGBArray  = ^TRGBArray;
    TRGBArray   = packed ARRAY[0..0] OF byte;  {This syntax is as bad as C}

VAR
   cosTheta       :  DOUBLE;
   i              :  INTEGER;
   iRotationAxis  :  INTEGER;
   iOriginal      :  INTEGER;
   iPrime         :  INTEGER;
   iPrimeRotated  :  INTEGER;
   j              :  INTEGER;
   jRotationAxis  :  INTEGER;
   jOriginal      :  INTEGER;
   jPrime         :  INTEGER;
   jPrimeRotated  :  INTEGER;
   RowOriginal    :  pTRGBArray;
   RowRotated     :  pTRGBArray;
   sinTheta       :  DOUBLE;
   Theta          :  DOUBLE;       {radians}
   bpp            :  integer;
   dc             :  THandle;
   loop           :  integer;
   Mirror         :  integer;
   BytesPP        :  integer;

begin
  RotatedBMP.canvas.fillrect(rect(0,0,RotatedBMP.width,RotatedBMP.height));
  {$IFDEF delphi2}
  WITH OriginalBMP DO
  BEGIN
    if angle = 90 then
    begin
         FOR i := 0 TO width-1 DO
             FOR j := 0 TO height-1 DO
                 RotatedBMP.Canvas.Pixels[j,(width-1)-i-1] := Canvas.Pixels[i,j];
    end
    else
    begin
         FOR i := 0 TO width-1 DO
             FOR j := 0 TO height-1 DO
                 RotatedBMP.Canvas.Pixels[(height-1)-j-1,i] := Canvas.Pixels[i,j];
    END;
  END;
  {$ELSE}
  dc:=GetDC(0);
  bpp:=GetDeviceCaps(dc,BITSPIXEL)*GetDeviceCaps(dc,PLANES);
  ReleaseDC(0,dc);

  if bpp < 8 then
  begin
       if bpp<8 then bpp:=8;

       iRotationAxis := RotatedBMP.Width  DIV 2;
       jRotationAxis := RotatedBMP.Height DIV 2;
       Theta := angle * PI / 180;
       sinTheta := SIN(Theta);
       cosTheta := COS(Theta);
       FOR j := RotatedBMP.Height-1 DOWNTO 0 DO
       BEGIN
         RowRotated  := pTRGBArray(RotatedBMP.Scanline[j]);
         jPrime := 2*(j - jRotationAxis) + 1;
         FOR i := RotatedBMP.Width-1 DOWNTO 0 DO
         BEGIN
           iPrime := 2*(i - iRotationAxis) + 1;
           iPrimeRotated := ROUND(iPrime * CosTheta - jPrime * sinTheta);
           jPrimeRotated := ROUND(iPrime * sinTheta + jPrime * cosTheta);
           iOriginal := (iPrimeRotated - 1) DIV 2 + iRotationAxis;
           jOriginal := (jPrimeRotated - 1) DIV 2 + jRotationAxis;
           {$R-}
           IF   (iOriginal >= 0) AND (iOriginal <= OriginalBMP.Width-1) AND
                (jOriginal >= 0) AND (jOriginal <= OriginalBMP.Height-1)
           THEN BEGIN
             RowOriginal := pTRGBArray(OriginalBMP.Scanline[jOriginal]);

             for loop:=0 to (bpp div 8)-1 do
               RowRotated[(i*bpp div 8)+loop] := RowOriginal[(iOriginal*bpp div 8)+loop];
           END
           ELSE
           BEGIN
             for loop:=0 to (bpp div 8)-1 do
               RowRotated[(i*bpp div 8)+loop] := 192;  {assign "corner" color}
           END
           {$R+}
         END
       END;
  end
  else
  begin
       if bpp<8 then bpp:=8;

       { Caution : this will not work if the bit depth is lower than 1 byte, i.e. when 16 colors
         are selected }
       BytesPP:= bpp shr 3;
       { For 90 and 270 degrees, use special code rather than those bulky routines }
       case Angle of    //
         90 : begin
           { Ensure the bitmaps are of required size }
           RotatedBMP.Width:= OriginalBMP.Height;
           RotatedBMP.Height:= OriginalBMP.Width;
           {$R-}
           for J:= 0 to OriginalBMP.Height - 1 do begin
             RowOriginal := pTRGBArray(OriginalBMP.Scanline[J]);
             Mirror:= OriginalBMP.Width - 1;
             for I:= 0 to OriginalBMP.Width - 1 do begin
               RowRotated:= pTRGBArray(RotatedBMP.Scanline[Mirror]);
               Dec(Mirror);
               Move(RowOriginal[I * BytesPP], RowRotated[J * BytesPP], BytesPP);
             end;
           end;
           {$R+}
         end;
         270 : begin
           { Ensure the bitmaps are of required size }
           RotatedBMP.Width:= OriginalBMP.Height;
           RotatedBMP.Height:= OriginalBMP.Width;
           {$R-}
           Mirror:= 0;
           for J:= OriginalBMP.Height - 1 downto 0 do begin
             RowOriginal := pTRGBArray(OriginalBMP.Scanline[Mirror]);
             Inc(Mirror);
             for I:= OriginalBMP.Width - 1 downto 0 do begin
               RowRotated:= pTRGBArray(RotatedBMP.Scanline[I]);
               Move(RowOriginal[I * BytesPP], RowRotated[J * BytesPP], BytesPP);
             end;
           end;
           {$R+}
         end;
         else begin
           iRotationAxis := RotatedBMP.Width  DIV 2;
           jRotationAxis := RotatedBMP.Height DIV 2;
           Theta := angle * PI / 180;
           sinTheta := SIN(Theta);
           cosTheta := COS(Theta);
           FOR j := RotatedBMP.Height-1 DOWNTO 0 DO BEGIN
             RowRotated  := pTRGBArray(RotatedBMP.Scanline[j]);
             jPrime := 2*(j - jRotationAxis) + 1;
             FOR i := RotatedBMP.Width-1 DOWNTO 0 DO BEGIN
               iPrime := 2*(i - iRotationAxis) + 1;
               iPrimeRotated := ROUND(iPrime * CosTheta - jPrime * sinTheta);
               jPrimeRotated := ROUND(iPrime * sinTheta + jPrime * cosTheta);
               iOriginal := ((iPrimeRotated - 1) div 2) + iRotationAxis;
               jOriginal := ((jPrimeRotated - 1) div 2) + jRotationAxis;
               {$R-}
               IF   (iOriginal >= 0) AND (iOriginal < OriginalBMP.Width) AND
                    (jOriginal >= 0) AND (jOriginal < OriginalBMP.Height)
               THEN BEGIN
                 RowOriginal := pTRGBArray(OriginalBMP.Scanline[jOriginal]);
                 Move(RowOriginal[iOriginal * BytesPP], RowRotated[i * BytesPP], BytesPP);
               END ELSE FillChar(RowRotated[i * BytesPP], BytesPP, 192);
               {$R+}
             END
           END;
         end;
       end;   // case
  end;
  {$ENDIF}
end;


procedure TCustomTab95Control.RotateText(Text:wideString; TabPosition:TTab95Position; WorkCanvas:Tcanvas; R:TRect; xadj, yadj:integer; UseGrayString : boolean);
const
  RotationStyles: array[TTab95Position] of Integer = (90, 270);
var
   tm : TTextMetric;
   LogFont: TLogFont;
   NewFont, OldFont: TFont;
   X, Y, yout, bigger : integer;
   bmp1, bmp2 : tbitmap;
   tw, th : integer;
   wr : trect;
   angle : integer;
   bpp            :  integer;
   dc             :  THandle;
begin
     angle := RotationStyles[TabPosition];
     GetTextMetrics(WorkCanvas.handle, tm);
     if (tm.tmPitchAndFamily and tmpf_TrueType <> 0) then
     begin

          x := 0;
          y := 0;
          case angle of
               90 : begin
                         x := r.left + (((r.right-r.left)div 2) - (TCanvasW(workcanvas).textheightW(text) div 2))+xadj;
                         y := (r.Bottom - (r.bottom-r.top))+ TCanvasW(workcanvas).textwidthW(text);
                    end;
               270 : begin
                          x := r.left + (((r.right-r.left)div 2) + (TCanvasW(workcanvas).textheightW(text) div 2))+xadj;
                          y := r.top;
                     end;
          end;

          OldFont := tFont.create;
          OldFont.assign(Workcanvas.Font);
          NewFont := tfont.create;
          NewFont.assign(Workcanvas.Font);
          windows.GetObject(NewFont.Handle, SizeOf(TLogFont), @LogFont);
          LogFont.lfEscapement:= Angle * 10;
          LogFont.lfOrientation := Angle * 10;
          NewFont.handle := CreateFontIndirect(LogFont);
          WorkCanvas.Font.Assign(NewFont);

          //Using Drawtext doens't work here.  It doesn't do rotated text with accel
          //characters properly.  This is a bug with certain versions of the Windows Functions.
          //The latest version of IE works correctly but not every one has the IE4 update. So.....

          if UseGrayString then
          begin
               WorkCanvas.Brush.style := bsClear;
               OffsetRect(R, 1, 1);
               inc(x);
               inc(y);
               WorkCanvas.Font.Color := clBtnHighlight;
               TCanvasW(WorkCanvas).TextRectW(r, x, y, text);
               //DrawTextW(WorkCanvas.Handle, PWideChar(text), Length(text), wr, DT_EXPANDTABS {or DT_CENTER} or DT_VCENTER or DT_SINGLELINE);

               dec(x);
               dec(y);
               OffsetRect(R, -1, -1);
               WorkCanvas.Font.Color := clBtnShadow;
               TCanvasW(WorkCanvas).TextRectW(r, x, y, text);
//               DrawTextW(WorkCanvas.Handle, PWideChar(text), Length(text), wr, DT_EXPANDTABS {or DT_CENTER} or DT_VCENTER or DT_SINGLELINE);
               WorkCanvas.Brush.style := bsSolid;
          end
          else
          begin
//               DrawTextW(WorkCanvas.Handle, PWideChar(text), Length(text), r, DT_EXPANDTABS {or DT_CENTER} or DT_VCENTER or DT_SINGLELINE);

               WorkCanvas.Brush.Style := bsClear;
               TCanvasW(WorkCanvas).TextRectW(r, x, y+1, text);
               WorkCanvas.Brush.Style := bsSolid;
          end;


          WorkCanvas.Font.Assign(OldFont);
          windows.DeleteObject(NewFont.handle);
          NewFont.free;
          OldFont.free;
     end
     else
     begin
          bmp1 := tbitmap.create;
          try
             {$IFNDEF VER90}
             dc:=GetDC(0);
             bpp:=GetDeviceCaps(dc,BITSPIXEL);
             ReleaseDC(0,dc);

             case bpp of
               1..8 : bmp1.pixelformat := pf8Bit;
               15 : bmp1.pixelformat := pf15Bit;
               16 : bmp1.pixelformat := pf16Bit;
               24 : bmp1.pixelformat := pf24Bit;
               32 : bmp1.pixelformat := pf32Bit;
             else
               bmp1.pixelformat := pf24Bit;
             end;
             {$ENDIF}
             bmp1.canvas.brush.assign(workcanvas.brush);
             bmp1.canvas.font.assign(workcanvas.font);
             th := bmp1.canvas.TextHeight(text);
             tw := bmp1.canvas.textwidth(text);
             if th > tw then
                bigger := th
             else
                bigger := tw;
             if odd(bigger) then inc(bigger);
             bmp1.width := bigger;
             bmp1.Height := bigger;
             yout := (bmp1.height div 2) - (th div 2);

             wr := Rect(0,yout,tw,yout+th);

             if UseGrayString then
             begin
                  OffsetRect(WR, 1, 1);
                  Bmp1.Canvas.Font.Color := clBtnHighlight;
                  DrawTextW(BMP1.Canvas.Handle, PWideChar(text), Length(text), wr, DT_EXPANDTABS or {DT_CENTER or }DT_VCENTER or DT_SINGLELINE);

                  Bmp1.Canvas.Brush.style := bsClear;
                  OffsetRect(WR, -1, -1);
                  Bmp1.Canvas.Font.Color := clBtnShadow;
                  DrawTextW(BMP1.Canvas.Handle, PWideChar(text), Length(text), wr, DT_EXPANDTABS or {DT_CENTER or }DT_VCENTER or DT_SINGLELINE);
                  Bmp1.Canvas.Brush.style := bsSolid;
             end
             else
                 DrawTextW(BMP1.Canvas.Handle, PWideChar(text), Length(text), wr, DT_EXPANDTABS or {DT_CENTER or }DT_VCENTER or DT_SINGLELINE);

             bmp2 := tbitmap.create;
             try
                bmp2.assign(bmp1);
                rotateimage(bmp2, bmp1, angle);
                if angle = 90 then
                begin
                     wr := rect(yout, bmp2.height-tw, th+yout, bmp2.height);
                     x := r.left + ((r.right-r.left)div 2) - (th div 2)+xadj;
                     y := r.top;
                end
                else
                begin
                    wr := rect(yout-1, 0, th+yout-1, tw);
                    x := r.left + ((r.right-r.left)div 2) - (th div 2)+xadj;
                    y := r.top;
                end;
                workcanvas.BrushCopy(rect(x,y,x+th,y+tw), bmp2, wr,workcanvas.brush.color);
             finally
                bmp2.free;
             end;
          finally
             bmp1.free;
          end;
     end;
end;

procedure TCustomTab95Control.CNDrawItem(var Message: TWMDrawItem);
var
  SaveIndex: Integer;
begin
  with Message.DrawItemStruct^ do
  begin
       SaveIndex := SaveDC(hDC);
       FCanvas.Handle := hDC;
       FCanvas.Font := Font;
       FCanvas.Brush.Color := clBtnFace;
       FCanvas.Brush.Style := bsSolid;

       {$ifdef delphi4}
       if itemID <> UINT(TabIndex) then
       {$else}
       if itemID <> TabIndex then
       {$endif}
       begin                                  
          if not ButtonStyle then
          begin
          if not VerticalTabs then
             begin
             case TabPosition of
                tpTopLeft: OffsetRect(rcItem, 0, 1);
                tpBottomRight: OffsetRect(rcItem, 0, -2);
                end;
             end
          else
             begin
             case TabPosition of
                tpTopLeft: OffsetRect(rcItem, 1, 0);
                tpBottomRight: OffsetRect(rcItem, -2, 0);
             end;
       end;
          end;
       end;

       DrawTab(Tabs.strings[itemID], itemID, rcItem);
       FCanvas.Handle := 0;
       RestoreDC(hDC, SaveIndex);
  end;
  Message.Result := 1;
end;

procedure TCustomTab95Control.SetButtonStyle(value:boolean);
begin
     if value <> fbuttonstyle then
     begin
          FVerticalTabs := false;
          FTabPosition := tpTopLeft;
          fbuttonstyle := value;
          MyRecreateWND;
     end;
end;

procedure TCustomTab95Control.SetFlatButtons(value:boolean);
begin
     if value <> fflatbuttons then
     begin
          fflatbuttons := value;
          MyRecreateWND;
          flatseperators := fflatseperators;
     end;
end;

procedure TCustomTab95Control.SetMultiSelect(value:boolean);
begin
     if value <> fmultiselect then
     begin
          fmultiselect := value;
          MyRecreateWND;
     end;
end;

procedure TCustomTab95Control.SetFlatSeperators(value:boolean);
begin
     fflatseperators := value;
     sendmessage(handle, TCM_SETEXTENDEDSTYLE, TCS_EX_FLATSEPARATORS, integer(value));
end;

function TCustomTab95Control.GetFlatSeperators:boolean;
begin
     result := false;
     sendmessage(handle, TCM_GETEXTENDEDSTYLE, TCS_EX_FLATSEPARATORS, integer(result));
     if result <> fflatseperators then
     begin
          SetFlatSeperators(fflatseperators);
          result := fflatseperators;
     end;
end;

procedure TCustomTab95Control.MyRecreateWND;
begin
     recreatewnd;
     if (parent<>nil) and parent.HandleAllocated and (handle<>0) then
     begin
          FlatSeperators;
          postmessage(handle,cm_resetimages,0,0);
     end;
end;

procedure TCustomTab95Control.wmmouseleave(var message:tmessage);
var
   oldtab:trect;
begin
     inherited;
     if (hottrack) and not (csdesigning in componentstate) then
     begin
          sendmessage(handle,TCM_GetItemRect, fmouseovertab, longint(@OldTab));
          InvalidateRect(handle,@OldTab,false);
          fmouseovertab := -1;
     end;
end;

function TCustomTab95Control.GetTabAt(x,y:integer):integer;
var
   HitTest : TTCHitTestInfo;
begin
     HitTest.pt := point(x,y);
     HitTest.flags := TCHT_ONITEM;
     result :=  sendmessage(handle,TCM_HITTEST,0,longint(@HitTest));
end;

procedure TCustomTab95Control.wmnchittest(var message:TWMNCHitTest);
var
   HitTest : TTCHitTestInfo;
   result : integer;
   OldTab, NewTab : trect;
begin
     inherited;
     if not (csdesigning in componentstate) then
     begin
          HitTest.pt := screentoclient(point(message.XPos,message.ypos));
          HitTest.flags := TCHT_ONITEM;
          result :=  sendmessage(handle,TCM_HITTEST,0,longint(@HitTest));
          if (result <> fmouseovertab) then
          begin
               if assigned(fontabtrack) then
                  fontabtrack(self,result);
               DisplayTabHint(result);
               if (hottrack) then
               begin
                    sendmessage(handle,TCM_GetItemRect, fmouseovertab, longint(@OldTab));
                    sendmessage(handle,TCM_GetItemRect, result, longint(@NewTab));
                    InvalidateRect(handle,@OldTab,false);
                    InvalidateRect(handle,@NewTab,false);
               end;
               fmouseovertab := result;
          end;
     end
     else
     fmouseovertab := -1;
end;

{ TFloatingForm }

constructor TFloatingForm.create(AOwner: TComponent);
begin
     Inherited create(AOwner);
     OnClose := DoCloseWindow;
     OnDestroy := DoDestroyWindow;
end;

procedure TFloatingForm.DoCloseWindow(Sender: TObject; var Action: TCloseAction);
begin
     if assigned(fsheet) then fsheet.docktabsheet;
     action := cafree;
end;

procedure TFloatingForm.DoDestroyWindow(Sender: TObject);
begin
     fsheet.ffloatingform := nil;
end;


{ TTab95Control }

constructor TTab95Control.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ftabhints := tstringlist.create;
  FImageIndexList := TImageIndexStrings.create;
  TImageIndexStrings(FImageIndexList).FTabControl := Self;
  FimageIndexSave := TStringList.create;
end;

destructor TTab95Control.destroy;
begin
  FImageIndexList.free;
  FImageIndexSave.free;
  ftabhints.free;
  inherited;
end;

procedure TTab95Control.loaded;
begin
     inherited;
     imageindexes.assign(fimageindexsave);
end;

procedure TTab95Control.CreateWnd;
begin
  inherited CreateWnd;
  if tabs.count > 0 then
  begin
       images := images;
       imageindexes.assign(FImageIndexSave);
       TabsChanged;
  end;
end;

procedure TTab95Control.DestroyWnd;
begin
  if Tabs.Count > 0 then
    FImageIndexSave.Assign(ImageIndexes);
  inherited;
end;

procedure TTab95Control.SetImageIndexList(value:TStrings);
var
   loop : integer;
begin
     loop := 0;
     while (loop < value.count) and (loop < ftabs.Count) do
     begin
          fImageIndexList.strings[loop] := value.Strings[loop];
          inc(loop);
     end;
end;

function TTab95Control.GetImageIndexList:TStrings;
begin
     result := fImageIndexList;
end;

procedure TTab95Control.DisplayTabHint(TabIndex:integer);
begin
     application.CancelHint;
     if (tabindex > -1) and (tabindex < tabhints.Count)  then
        hint := trim(tabhints.strings[tabindex]);
end;

procedure TTab95Control.SetTabHints(value:TStrings);
begin
     ftabhints.assign(value);
end;

procedure TTab95Control.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseOverTab = TabIndex) then
        fMouseDragTab := fMouseOverTab;
     Inherited;
end;

procedure TTab95Control.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseDragTab <> fMouseOverTab) and (fMouseOverTab <> -1) then
     begin
          Tabs.Move(fMouseDragTab,fMouseOverTab);
          Cursor := crDefault;
          if assigned(fOnTabShift) then fOnTabShift(self);
     end;
     inherited;
end;

procedure TTab95Control.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) then
     begin
          if (ssLeft in Shift) then
          begin
               if (fMouseOverTab = -1) then
                  Cursor := crNo
               else
               if (fMouseDragTab <> fMouseOverTab) then
                  Cursor := crDrag
               else
                  Cursor := crDefault;
          end
          else
          Cursor := crDefault;
     end;
     inherited;
end;


procedure TTab95Control.CMDialogChar(var Message: TCMDialogChar);
var
   loop : integer;
begin
     for loop := 0 to Tabs.count-1 do
     begin
       if IsAccel(Message.CharCode, Tabs.strings[loop]) then
       begin
         if (enabled) and (CanFocus) and (TabIndex <> loop) then
         begin
              TabIndex := loop;
              if assigned(fonchange) then
                 FOnChange(self);
         end;
         Message.Result := 1;
         break;
       end
       else
       Message.Result := 0;
     end;
     If Message.Result <> 1 Then Inherited;
end;

{ TTab95Sheet }

constructor TTab95Sheet.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     Align := alClient;
     ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
     Visible := False;
     fimageindex := -1;
     height := 0;
     width := 0;
     FCanvas := TControlCanvas.Create;
     TControlCanvas(FCanvas).Control := Self;
     fpageindex := -1;
     fdragstart := false;
     fDragging := false;
     GripAlign := gaLeft;
     TabVisible := true;
     FloatOnTop := false;

     // [MJ] 12-Oct-1999
     fPrimaryControl := nil;
     fPrimaryObject := nil;
end;

destructor TTab95Sheet.Destroy;
begin
     fcanvas.free;
     if FPageControl <> nil then FPageControl.RemovePage(Self);
     inherited Destroy;
end;

procedure TTab95Sheet.SetFloatOnTop(value:boolean);
begin
     if fFloatOnTop <> value then
     begin
          fFloatOnTop := value;
          if floating then
          begin
               if value then
               if (Application.mainform <> nil) and (Application.mainform is tform) then
                  setwindowlong(ffloatingform.handle,gwl_hwndparent,Application.mainform.handle)
               else
                  setwindowlong(ffloatingform.handle,gwl_hwndparent,0);
          end
     end;
end;

function TTab95Sheet.GetGripRect:TRect;
begin
     result := Rect(0,0,0,0);
     if (dragable) and (not fFloating) then
     case GripAlign of
          gaLeft: result := rect(0,0,GripSize,height);
          gaRight: result := rect(width-GripSize,0,width,height);
          gaTop: result := rect(0,0,width,GripSize);
          gaBottom: result := rect(0,height-gripsize,width,height);
     end;
end;

function TTab95Sheet.GetGripperRect:TRect;
begin
     result := Rect(0,0,0,0);
     if (dragable) and (not fFloating) then
     case GripAlign of
          gaLeft: result := rect(0,0,GripSize,20);
          gaRight: result := rect(width-GripSize,height-20,width,height);
          gaTop: result := rect(width-20,0,width,GripSize);
          gaBottom: result := rect(0,height-gripsize,20,height);
     end;
end;

function TTab95Sheet.GetPageIndex: Integer;
begin
  if FPageControl <> nil then
     Result := FPageControl.FPages.IndexOf(Self) else
    Result := -1;
end;

function TTab95Sheet.GetTabIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not FTabShowing then Dec(Result) else
    for I := 0 to PageIndex - 1 do
      if TTab95Sheet(FPageControl.FPages[I]).FTabShowing then
        Inc(Result);
end;

procedure TTab95Sheet.CreateWindowHandle(const Params: TCreateParams);
begin
    //inherited CreateWindowHandle(Params);
    CreateUnicodeHandle(Self, Params, '');
end;

procedure TTab95Sheet.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TTab95Sheet.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TPage95Control then
    PageControl := TPage95Control(Reader.Parent);
end;

procedure TTab95Sheet.SeTPage95Control(APageControl: TPage95Control);
begin
  if FPageControl <> APageControl then
  begin
    if FPageControl <> nil then FPageControl.RemovePage(Self);
    Parent := APageControl;
    if APageControl <> nil then APageControl.InsertPage(Self);
  end;
end;

procedure TTab95Sheet.setImageIndex(value:integer);
var
  TCItem: TTCItem;
  WorkingIndex : integer;
  loop : integer;
begin
     if ftabshowing then
     begin
          TCItem.mask := TCIF_Image;
          TCItem.iImage := value;
          workingindex := Pageindex;
          loop := pageindex;
          while loop >= 0 do
          begin
               if TTab95Sheet(fpagecontrol.FPages[loop]).TabVisible = false then
                  dec(workingindex);
               dec(loop);
          end;
          if SendMessage(FPageControl.Handle, TCM_SETITEM, workingIndex, Longint(@TCItem)) = 0 then
             TabControlError('TTab95Sheet.SetImageIndex')
          else
              fimageindex := value;
          FPageControl.TabsChanged;
     end
     else
     fimageindex := value;
end;

function TTab95Sheet.GetImageIndex:integer;
var
  TCItem: TTCItem;
  WorkingIndex : integer;
  loop : integer;
begin
     result := fimageindex;
     if TabVisible and not Floating then
     begin
          TCItem.mask := TCIF_Image;
          workingindex := Pageindex;
          loop := pageindex;
          while loop >= 0 do
          begin
               if TTab95Sheet(fpagecontrol.FPages[loop]).TabVisible = false then
                  dec(workingindex);
               dec(loop);
          end;
          if SendMessage(FPageControl.Handle, TCM_GETITEM, WorkingIndex, Longint(@TCItem)) = 0 then TabControlError('TTab95Sheet.GetImageIndex');
          result := TCItem.iImage;
          if result <> fimageindex then result := fimageindex;
     end;
end;

procedure TTab95Sheet.SetPageIndex(Value: Integer);
var
  I, MaxPageIndex: Integer;
begin
  if FPageControl <> nil then
  begin
    MaxPageIndex := FPageControl.FPages.Count - 1;
    if Value > MaxPageIndex then
      raise EListError.CreateFmt(SPageIndexError, [Value, MaxPageIndex]);
    I := TabIndex;
    FPageControl.FPages.Move(PageIndex, Value);
    if I >= 0 then FPageControl.MoveTab(I, TabIndex);
  end;
end;

procedure TTab95Sheet.SetTabShowing(Value: Boolean);
begin
  if fFloating = true then exit;
  if FTabShowing <> Value then
    if Value then
    begin
      FTabShowing := True;
      FPageControl.InsertTab(Self);
      Imageindex := fimageindex;
    end
    else
    begin
      FPageControl.DeleteTab(Self);
      FTabShowing := False;
    end;
end;

procedure TTab95Sheet.SetTabVisible(Value: Boolean);
begin
  if FTabVisible <> Value then
  begin
    FTabVisible := Value;
    UpdateTabShowing;
  end;
end;

procedure TTab95Sheet.UpdateTabShowing;
begin
  SetTabShowing((FPageControl <> nil) and FTabVisible);
end;

procedure TTab95Sheet.CMTextChanged(var Message: TMessage);
begin
  if FTabShowing then FPageControl.UpdateTab(Self);
end;

procedure TTab95Sheet.DrawDraggingRect(MousePos : TPoint);
var
  DC             : hDC;      { device context for the window       }
  Canvas         : TCanvas;  { canvas to draw dragging rect        }
  AdjustedRect   : TRect;    { fDragRect adjusted for MousePos     }
  ScreenPos      : TPoint;   { screen-relative version of MousePos }

begin
  DC := GetWindowDc(GetDesktopWindow);
  if DC <> 0 then begin
    ScreenPos := ClientToScreen(MousePos);
    with AdjustedRect do begin
      Left := ScreenPos.X-fMouseOffset.X;
      Top := ScreenPos.Y-fMouseOffset.Y;
      Right := Left+fWidth;
      Bottom := Top+fHeight;
    end; { with AdjustedRect do }
    fDragRect := AdjustedRect;
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    Canvas.DrawFocusRect(AdjustedRect);
    Canvas.Free;
  end else
    ShowMessage('Could not retreive Desktop Window device context.');
end;

procedure TTab95Sheet.WMLBUTTONDOWN(var Msg : TWMLButtonDown);
begin
     inherited;
     if (Parent is TPage95Control) then
        if not PageControl.AllowPageToFloat then exit;

     if ffloating then exit;

     if (fDragable) and (ptinrect(GripRect, point(msg.pos.x,msg.pos.y))) then
     begin
          fDragStart := TRUE;

          fWidth := width;
          fHeight := height;

          fOldMousePos := Point(Msg.Pos.x,Msg.Pos.y);
          fMouseOffset := fOldMousePos;
     end;
end;

procedure TTab95Sheet.WMMOUSEMOVE(var Msg : TWMMouseMove);
begin
     inherited;
     if (fDragStart) and ((abs(foldmousepos.x - msg.pos.x) > 3) or (abs(foldmousepos.y - msg.pos.y) > 3)) then
     begin
          fdragging := true;
          fDragStart := false;
          DrawDraggingRect(fOldMousePos);
     end;
     if fDragging then
     begin
          DrawDraggingRect(fOldMousePos);
          fOldMousePos := Point(Msg.Pos.x,Msg.Pos.y);
          DrawDraggingRect(fOldMousePos);
     end;
end;

procedure TTab95Sheet.WMLBUTTONUP(var Msg : TWMLButtonDown);
begin
     inherited;
     if fDragging then
     begin
          DrawDraggingRect(fOldMousePos);
          FloatTabSheet;
          fDragging := FALSE;
     end; { if dragging }
     if fDragStart then
     begin
          fDragStart := false;
          invalidate;
     end;
end;

procedure TTab95Sheet.FloatTabSheet;
var
   ScreenPos: TPoint;
begin
     if (not FFloating) and (PageControl.AllowPageToFloat) then
     begin
        if not fDragging then
        begin
           ScreenPos := ClientToScreen(Point(Left, Top));
           with fDragRect do begin
              Left := ScreenPos.X;
              Top := ScreenPos.Y;
              fWidth := ClientRect.Right;
              fHeight := ClientRect.Bottom;
              Right := Left + fWidth;
              Bottom := Top + fHeight;
           end;
        end;

        fOldPageControl := PageControl;
        PageControl := nil;

        FFloating := true;

        fOldPageControl.SelectNextPage(True);
        fOldPageControl.Perform(CM_TabSheetDraggedOFF,0,0);

        if not assigned(fFloatingForm) then
        begin
           {$IFDEF MJ_MODS}
           fFloatingForm := TFloatingForm.Create( nil );
           {$ELSE}
           fFloatingForm := TFloatingForm.Create(self.owner);
           {$ENDIF}
           if (fFloatOnTop) and (Application.mainform <> nil) and (Application.mainform is tform) then
              setwindowlong(ffloatingform.handle,gwl_hwndparent,Application.mainform.handle);
           if assigned(fOldPageControl.images) and (imageindex <> -1) then
              fOldPageControl.Images.GetIcon(imageindex,ffloatingform.Icon);
           fFloatingForm.Caption := Caption;
           fFloatingForm.ClientWidth := fWidth;
           fFloatingForm.ClientHeight := fHeight;
           fFloatingForm.TabSheet := self;
        end;

        fFloatingForm.Left := fDragRect.Left;
        fFloatingForm.Top := fDragRect.Top;

        if assigned(FOnFloatChange) then
           FOnFloatChange(Self, fsFloating);

        Parent := fFloatingForm;
        fFloatingForm.Show;
        Show;
   end;                  
end;

procedure TTab95Sheet.DockTabSheet;
begin
   if FFloating then
   begin
      fFloatingForm.Hide;
      FFloating := false;

      PageControl:= fOldPageControl;
      PageControl.Perform(CM_TabSheetDraggedON,0,0);
      PageControl.ActivePage := Self;
      foldPageControl := nil;

      if assigned(FOnFloatChange) then
         FOnFloatChange(Self, fsDocked);
   end;
end;

function TTab95Sheet.GetClientRect:TRect;
var
   clientrect : trect;
begin
     clientrect := inherited GetClientRect;
     if (not dragable) or (ffloating) then
        result := clientrect
     else
     begin
          case gripalign of
               gaLeft: clientrect.Left := clientrect.Left + GripSize;
               gaRight: clientrect.right := clientrect.right - GripSize;
               gaTop: clientrect.Top := clientrect.top + GripSize;
               gaBottom: clientrect.bottom := clientrect.bottom - GripSize;
          end;
          result := clientrect;
     end;
end;

procedure TTab95Sheet.setDragOption(value:boolean);
begin
     if value <> fdragable then
     begin
          fDragable := value;
          realign;
          invalidate;
     end;
end;

procedure TTab95Sheet.setGripAlign(value:TGripAlign);
begin
     fGripAlign := value;
     realign;
     invalidate;
end;

procedure TTab95Sheet.WMPaint(var msg:TWMPaint);
const
     xpos = 4;
var
   loop : integer;
   workcolor : tcolor;
   position : integer;
   ypos : integer;
begin
     inherited;
     if (dragable) and not (FFloating) then
     with fcanvas do
     begin
          brush.color := clbtnface;
          brush.style := bsSolid;
          fillrect(GetGripRect);
          if enabled then
            workcolor := clactivecaption
          else
            workcolor := clinactivecaption;

          ypos := 0;
          if gripalign = gabottom then ypos := (height-gripsize)+2;
          if gripalign = garight then ypos := (width-gripsize)+2;
          if (gripalign in [gabottom, gaTop]) then
          for loop := 0 to 4 do
          begin
               if gripalign = gaTop then position := (width - (xpos * loop))-4
               else
               position := xpos * loop;

               pixels[position,ypos] := clbtnhighlight;
               pixels[1+position,ypos] := workcolor;
               pixels[position,ypos+1] := workcolor;
               pixels[1+position,ypos+1] := workcolor;

               pixels[2+position,ypos+3] := clbtnhighlight;
               pixels[3+position,ypos+3] := workcolor;
               pixels[2+position,ypos+4] := workcolor;
               pixels[3+position,ypos+4] := workcolor;
          end;

          if (gripalign = gaLeft) or (gripalign = gaRight)  then
          for loop := 0 to 4 do
          begin
               if gripalign = gaRight then position := (height - (xpos * loop))-4
               else
               position := xpos*loop;

               pixels[ypos,position] := clbtnhighlight;
               pixels[ypos,1+position] := workcolor;
               pixels[ypos+1,position] := workcolor;
               pixels[ypos+1,1+position] := workcolor;

               pixels[ypos+3,2+position] := clbtnhighlight;
               pixels[ypos+3,3+position] := workcolor;
               pixels[ypos+4,2+position] := workcolor;
               pixels[ypos+4,3+position] := workcolor;
          end;
     end;
end;

procedure TTab95Sheet.SetEnabled(Value: boolean);
begin
     // [MJ] "SetEnabled not found in base class"!
     // inherited SetEnabled(value);
     if ( Enabled = Value ) then exit;
     Enabled := Value;
     if assigned(PageControl) then PageControl.Repaint;
end;

function TTab95Sheet.GetCaption: WideString;
begin
  Result := TntControl_GetText(Self)
end;
procedure TTab95Sheet.SetCaption(const Value: WideString);
begin
  TntControl_SetText(Self, Value);
end;


{ TPage95Control }

constructor TPage95Control.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csDoubleClicks, csOpaque];
  FPages := TList.Create;
end;

destructor TPage95Control.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do TTab95Sheet(FPages[I]).FPageControl := nil;
  FPages.Free;
  inherited Destroy;
end;

procedure TPage95Control.Change;
var
{$ifdef delphi2}
  Form: TForm;
{$Else}
  Form: TCustomForm;
{$Endif}
begin
  UpdateActivePage;
  if csDesigning in ComponentState then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil) then Form.Designer.Modified;
  end;
  inherited Change;
end;

procedure TPage95Control.ChangeActivePage(Page: TTab95Sheet);
var
{$ifdef delphi2}
  ParentForm: TForm;
{$Else}
  ParentForm: TCustomForm;
{$EndIF}
begin
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        TabIndex := FActivePage.TabIndex;
        Exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and
        (ParentForm.ActiveControl = FActivePage) then
        if Page.CanFocus then
          ParentForm.ActiveControl := Page else
          ParentForm.ActiveControl := Self;
    end;
    if FActivePage <> nil then FActivePage.Visible := False;
    FActivePage := Page;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
      FActivePage.SelectFirst;
  end;
end;

procedure TPage95Control.DeleteTab(Page: TTab95Sheet);
begin
  Tabs.Delete(Page.TabIndex);
  UpdateActivePage;
end;

function TPage95Control.FindNextPage(CurPage: TTab95Sheet;
  GoForward, CheckTabVisible: Boolean): TTab95Sheet;
var
  I, StartIndex: Integer;
begin
  if FPages.Count <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex = -1 then
      if GoForward then StartIndex := FPages.Count - 1 else StartIndex := 0;
    I := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(I);
        if I = FPages.Count then I := 0;
      end else
      begin
        if I = 0 then I := FPages.Count;
        Dec(I);
      end;
      Result := FPages[I];
      if not CheckTabVisible or Result.TabVisible then Exit;
    until I = StartIndex;
  end;
  Result := nil;
end;

procedure TPage95Control.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do Proc(TComponent(FPages[I]));
end;

function TPage95Control.GetPage(Index: Integer): TTab95Sheet;
begin
  Result := FPages[Index];
end;

function TPage95Control.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TPage95Control.InsertPage(Page: TTab95Sheet);
begin
  FPages.Add(Page);
  Page.FPageControl := Self;
  Page.UpdateTabShowing;
end;

procedure TPage95Control.InsertTab(Page: TTab95Sheet);
begin
  Tabs.InsertObject(Page.TabIndex, Page.Caption, Page);
  UpdateActivePage;
end;

procedure TPage95Control.MoveTab(CurIndex, NewIndex: Integer);
begin
  Tabs.Move(CurIndex, NewIndex);
end;

procedure TPage95Control.RemovePage(Page: TTab95Sheet);
begin
  if FActivePage = Page then SetActivePage(nil);
  if FMarkedPage = Page then FMarkedPage := nil;
  Page.SetTabShowing(False);
  Page.FPageControl := nil;
  FPages.Remove(Page);
end;

procedure TPage95Control.SelectNextPage(GoForward: Boolean);
var
  Page: TTab95Sheet;
begin
  Page := FindNextPage(ActivePage, GoForward, True);
  if (Page <> nil) and (Page <> ActivePage) and CanChange then
  begin
    TabIndex := Page.TabIndex;
    Change;
  end;
end;

procedure TPage95Control.SetActivePage(Page: TTab95Sheet);
begin
  if (Page <> nil) and (Page.PageControl <> Self) then Exit;
  ChangeActivePage(Page);
  if Page = nil then
    TabIndex := -1
  else if Page = FActivePage then
    TabIndex := Page.TabIndex;
end;

procedure TPage95Control.SetChildOrder(Child: TComponent; Order: Integer);
begin
  TTab95Sheet(Child).PageIndex := Order;
end;

procedure TPage95Control.ShowControl(AControl: TControl);
begin
  if (AControl is TTab95Sheet) and (TTab95Sheet(AControl).PageControl = Self) then
    SetActivePage(TTab95Sheet(AControl));
  inherited ShowControl(AControl);
end;

procedure TPage95Control.UpdateTab(Page: TTab95Sheet);
begin
  Tabs[Page.TabIndex] := Page.Caption;
end;

procedure TPage95Control.UpdateActivePage;
begin
  if TabIndex >= 0 then SetActivePage(TTab95Sheet(Tabs.Objects[TabIndex]));
end;

procedure TPage95Control.CMDesignHitTest(var Message: TCMDesignHitTest);
var
  HitIndex: Integer;
  HitTestInfo: TTCHitTestInfo;
begin
  HitTestInfo.pt := SmallPointToPoint(Message.Pos);
  HitIndex := SendMessage(Handle, TCM_HITTEST, 0, Longint(@HitTestInfo));
  if (HitIndex >= 0) and (HitIndex <> TabIndex) then Message.Result := 1;
end;

procedure TPage95Control.CMDialogKey(var Message: TCMDialogKey);
begin
  if (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end else
    inherited;
end;

function TPage95Control.GetAllowPageToFloat:Boolean;
begin
     if (GetPageCount = 1) then
        result := FRemoveLastTab
     else
        result := true;
end;

procedure TPage95Control.CMTabDraggedOff(var Message:TMessage);
begin
     inc(FFloatingPageCount);
     if assigned(FOnFloatChange) then
        FOnFloatChange(Self, fsFloating);
end;

procedure TPage95Control.CMTabDraggedOn(var Message:TMessage);
var
   loop, loop1 : integer;
   worksheet : ttab95sheet;
begin
     Dec(FFloatingPageCount);
     for loop :=  0 to fpages.count-1 do
     begin
          worksheet := ttab95sheet(fpages[loop]);
          loop1 := 0;
          while (loop1 < fpages.count - 1) and (worksheet.StaticPageIndex > ttab95sheet(fpages[loop1]).staticpageindex) do
                inc(loop1);
          worksheet.pageindex := loop1;
          if worksheet.TabVisible then
             worksheet.imageindex := worksheet.imageindex;
     end;
     if assigned(FOnFloatChange) then
        FOnFloatChange(Self, fsDocked);
end;

procedure TPage95Control.ResetImageInfo;
var
   loop : integer;
   worksheet : TTab95Sheet;
begin
     images := images;
     for loop := 0 to fpages.count-1 do
     begin
          worksheet := ttab95sheet(fpages[loop]);
          if worksheet.tabvisible then
             worksheet.imageindex := worksheet.imageindex;
     end;
end;

procedure TPage95Control.CMResetImages(var Msg:TMessage);
begin
     try
        sendmessage(handle,wm_setredraw,0,0);
        ResetImageInfo;
     finally
        sendmessage(handle,wm_setredraw,1,0);
     end;
end;

procedure TPage95Control.CMDialogChar(var Message: TCMDialogChar);
var
   loop : integer;
begin
     for loop := 0 to fpages.count-1 do
     begin
       if IsAccel(Message.CharCode, TTab95Sheet(fpages[loop]).Caption) then
       begin
         if (enabled) and (CanFocus) and (ActivePage <> TTab95Sheet(fpages[loop])) then
         begin
           ActivePage := TTab95Sheet(fpages[loop]);
           if assigned(FOnChange) then
              FOnChange(self);
         end;
         Message.Result := 1;
         break;
       end
       else
       Message.Result := 0;
     end;
     If Message.Result <> 1 Then Inherited;
end;

procedure TPage95Control.DisplayTabHint(TabIndex:integer);
begin
     application.CancelHint;
     if not assigned(activepage) then exit; // [MJ]
     if tabindex <> -1 then
        hint := trim(ttab95sheet(fpages.items[tabindex]).tabhint)
     else
        hint := trim(ttab95sheet(activepage).hint);
end;

procedure TPage95Control.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) and (button = mbleft) and (fMouseOverTab = ActivePage.PageIndex) then
        fMouseDragTab := fMouseOverTab;
     Inherited;
end;

procedure TPage95Control.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   tabsheet1 : TTab95Sheet;
begin
     if (ftabshifting) and (button = mbleft) and (fMouseDragTab <> fMouseOverTab) and (fMouseOverTab <> -1) then
     begin
          TabSheet1 := Pages[fMouseDragTab];
          TabSheet1.PageIndex := fMouseOverTab;
          Cursor := crDefault;
          if assigned(fOnTabShift) then fOnTabShift(self);
     end;
     inherited;
end;

procedure TPage95Control.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
     if (ftabshifting) then
     begin
          if (ssLeft in Shift) then
          begin
               if (fMouseOverTab = -1) then
                  Cursor := crNo
               else
               if (fMouseDragTab <> fMouseOverTab) then
                  Cursor := crDrag
               else
                  Cursor := crDefault;
          end
          else
          Cursor := crDefault;
     end;
     inherited;
end;


type TAccessStrings = class(TStrings{TNT-ALLOW TStrings});

procedure TabControlError(const S: WideString);    overload;
begin
  raise EListError.Create(S);
end;

procedure TTntTabStrings.Clear;
begin
  FAnsiTabs.Clear;
end;

procedure TTntTabStrings.Delete(Index: Integer);
begin
  FAnsiTabs.Delete(Index);
end;

function TTntTabStrings.GetCount: Integer;
begin
  Result := FAnsiTabs.Count;
end;

function TTntTabStrings.GetObject(Index: Integer): TObject;
begin
  Result := FAnsiTabs.Objects[Index];
end;

procedure TTntTabStrings.PutObject(Index: Integer; AObject: TObject);
begin
  FAnsiTabs.Objects[Index] := AObject;
end;

procedure TTntTabStrings.SetUpdateState(Updating: Boolean);
begin
  inherited;
  TAccessStrings(FAnsiTabs).SetUpdateState(Updating);
end;

function TTntTabStrings.Get(Index: Integer): WideString;
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItemW;
  Buffer: array[0..4095] of WideChar;
begin
  if (not Win32PlatformIsUnicode) then
    Result := FAnsiTabs[Index]
  else begin
    TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading];
    TCItem.pszText := Buffer;
    TCItem.cchTextMax := SizeOf(Buffer);
    if SendMessageW(FTabControl.Handle, TCM_GETITEMW, Index, Longint(@TCItem)) = 0 then
      TabControlError(WideFormat(sTabFailRetrieve, [Index]));
    Result := Buffer;
  end;
end;

type TAccessCustomTabControl = class(TCustomTabControl{TNT-ALLOW TCustomTabControl});

function GetTabControlImageIndex(Self: TCustomTabControl{TNT-ALLOW TCustomTabControl}; TabIndex: Integer): Integer;
begin
  Result := TabIndex;
  with TAccessCustomTabControl(Self) do
    if Assigned(OnGetImageIndex) then OnGetImageIndex(Self, TabIndex, Result);
end;

procedure TTntTabStrings.Put(Index: Integer; const S: WideString);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItemW;
begin
  if (not Win32PlatformIsUnicode) then
    FAnsiTabs[Index] := S
  else begin
    TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading]; // or TCIF_IMAGE;
    TCItem.pszText := PWideChar(S);
    if SendMessageW(FTabControl.Handle, TCM_SETITEMW, Index, Longint(@TCItem)) = 0 then
       TabControlError(WideFormat(sTabFailSet, [S, Index]));
  end;
end;

procedure TTntTabStrings.Insert(Index: Integer; const S: WideString);
const
  RTL: array[Boolean] of LongInt = (0, TCIF_RTLREADING);
var
  TCItem: TTCItemW;
begin
  if (not Win32PlatformIsUnicode) then
    FAnsiTabs.Insert(Index, S)
  else begin
    TCItem.mask := TCIF_TEXT or RTL[FTabControl.UseRightToLeftReading]; // or TCIF_IMAGE;
    TCItem.pszText := PWideChar(S);
    if SendMessageW(FTabControl.Handle, TCM_INSERTITEMW, Index, Longint(@TCItem)) < 0 then
       TabControlError(WideFormat(sTabFailSet, [S, Index]));
  end;
end;


end.
