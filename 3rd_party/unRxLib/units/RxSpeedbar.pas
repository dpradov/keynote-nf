{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and patched by JB.                           }
{*******************************************************}

unit RxSpeedBar;

{$I RX.INC}
{$S-,W-,R-}

interface

uses {$IFNDEF VER80} Windows, Registry, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  SysUtils, Classes, Messages, Menus, Buttons, Controls, Graphics, Forms,
  {$IFDEF RX_D4} ImgList, ActnList, {$ENDIF} ExtCtrls, Grids, IniFiles,
  {$IFDEF RX_D6}Types, {$ENDIF} {$IFDEF RX_D16}System.UITypes,{$ENDIF}
  RxCtrls, RxPlacemnt;

const
  DefButtonWidth = 24;
  DefButtonHeight = 23;

type
  TSpeedItem = class;
  TSpeedbarSection = class;
  ESpeedbarError = class(Exception);

{ TSpeedBar }

  TBarOrientation = (boHorizontal, boVertical);
  TBarPosition = (bpAuto, bpCustom);
  TSpeedbarOption = (sbAllowDrag, sbAllowResize, sbFlatBtns, sbGrayedBtns,
    sbTransparentBtns, sbStretchBitmap);
  TSpeedbarOptions = set of TSpeedbarOption;
  TBoundLine = (blTop, blBottom, blLeft, blRight);
  TBoundLines = set of TBoundLine;
  TSbScaleFlags = set of (sfOffsetX, sfOffsetY, sfBtnSizeX, sfBtnSizeY);
  TForEachItem = procedure (Item: TSpeedItem; Data: Longint) of object;
  TApplyAlignEvent = procedure (Sender: TObject; Align: TAlign;
    var Apply: Boolean) of object;

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSpeedBar = class(TCustomPanel)
  private
    FSections: TList;
    FPosition: TBarPosition;
    FOrientation: TBarOrientation;
    FAlign: TAlign;
    FButtonSize: TPoint;
    FButtonStyle: TButtonStyle;
    FGridSize: TPoint;
    FOffset: TPoint;
    FEditWin: HWnd;
    FRowCount: Integer;
    FPrevRect: TRect;
    FPrevAlign: TAlign;
    FOptions: TSpeedbarOptions;
    FLocked: Boolean;
    FVersion: Integer;
    FDrag: Boolean;
    FResizing: Boolean;
    FStartDrag: TPoint;
    FWallpaper: TPicture;
    FBoundLines: TBoundLines;
    FIniLink: TIniLink;
    FReserved: Integer;
    FFix: Boolean;
    FDesignStyle: Boolean;
    FScaleFlags: TSbScaleFlags;
    FOnAddItem: TNotifyEvent;
    FOnApplyAlign: TApplyAlignEvent;
    FOnPosChanged: TNotifyEvent;
    FOnVisibleChanged: TNotifyEvent;
    FOnCustomize: TNotifyEvent;
{$IFNDEF RX_D10}
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;
{$ENDIF}
{$IFNDEF VER80}
    FImages: TImageList;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TImageList);
    procedure InvalidateItem(Item: TSpeedItem; Data: Longint);
{$ENDIF}
    function GetOrientation: TBarOrientation;
    procedure SetOrientation(Value: TBarOrientation);
    procedure ApplyOrientation(Value: TBarOrientation);
    procedure ApplyButtonSize;
    procedure UpdateGridSize;
    procedure ClearSections;
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetButtonSize(Index: Integer): Integer;
    procedure SetButtonSize(Index, Value: Integer);
    function GetButtonsOffset(Index: Integer): Integer;
    procedure SetButtonsOffset(Index: Integer; Value: Integer);
    procedure SetOptions(Value: TSpeedbarOptions);
    procedure SetBoundLines(Value: TBoundLines);
    function MinButtonsOffset: Integer;
    procedure WallpaperChanged(Sender: TObject);
    procedure SetWallpaper(Value: TPicture);
    procedure SetItemParams(Item: TSpeedItem; InitBounds: Boolean);
    procedure SetItemVisible(Item: TSpeedItem; Data: Longint);
    procedure SetItemEnabled(Item: TSpeedItem; Data: Longint);
    procedure SetItemButtonSize(Item: TSpeedItem; Data: Longint);
    procedure OffsetItem(Item: TSpeedItem; Data: Longint);
    procedure ApplyItemSize(Item: TSpeedItem; Data: Longint);
    procedure AlignItemToGrid(Item: TSpeedItem; Data: Longint);
    procedure SwapItemBounds(Item: TSpeedItem; Data: Longint);
    procedure SetItemEditing(Item: TSpeedItem; Data: Longint);
    procedure HideItem(Item: TSpeedItem; Data: Longint);
    procedure WriteItemLayout(Item: TSpeedItem; Data: Longint);
    procedure FlatItem(Item: TSpeedItem; Data: Longint);
    procedure TransparentItem(Item: TSpeedItem; Data: Longint);
    function GetSection(Index: Integer): TSpeedbarSection;
    function GetSectionCount: Integer;
    procedure GrayedItem(Item: TSpeedItem; Data: Longint);
    function GetFramePos(X, Y: Integer; var Apply: Boolean): Integer;
    function GetFrameRect(X, Y: Integer): TRect;
    procedure StartDragFrame;
    procedure DragFrame(X, Y: Integer);
    procedure StopDragFrame(X, Y: Integer);
    function CheckResize(Shift: TShiftState; X, Y: Integer): Boolean;
    procedure ReadSections(Reader: TReader);
    procedure WriteSections(Writer: TWriter);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    procedure ReadDesignStyle(Reader: TReader);
    procedure ReadAllowDrag(Reader: TReader);
    procedure WriteDesignStyle(Writer: TWriter);
    function GetStorage: TFormPlacement;
    procedure SetStorage(Value: TFormPlacement);
    procedure IniSave(Sender: TObject);
    procedure IniLoad(Sender: TObject);
    procedure InternalSaveLayout(IniFile: TObject; const Section: string);
    procedure InternalRestoreLayout(IniFile: TObject; const Section: string);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
{$IFNDEF RX_D10}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
{$ENDIF}
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function AppendSection(Value: TSpeedbarSection): Integer; virtual;
    procedure AlignItemsToGrid;
    procedure ChangeScale(M, D: Integer); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
{$IFNDEF VER80}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
{$ELSE}
    procedure WriteComponents(Writer: TWriter); override;
{$ENDIF}
    procedure ForEachItem(Proc: TForEachItem; Data: Longint); virtual;
    procedure PosChanged; dynamic;
    procedure AfterCustomize; dynamic;
    property ScaleFlags: TSbScaleFlags read FScaleFlags write FScaleFlags;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFontDefault; virtual;
    procedure RemoveItem(Item: TSpeedItem);
    procedure RemoveSection(Section: Integer); { delete and free section and items }
    procedure DeleteSection(Section: Integer); { delete section }
    function AddSection(const ACaption: string): Integer;
    procedure AddItem(Section: Integer; Item: TSpeedItem);
    function NewItem(AOwner: TComponent; Section: Integer;
      const AName: string): TSpeedItem;
    function AcceptDropItem(Item: TSpeedItem; X, Y: Integer): Boolean;
    procedure SetEditing(Win: HWnd);
    function GetEditing: Boolean;
    function SearchItem(const ItemName: string): TSpeedItem;
    function FindItem(Item: TSpeedItem; var Section, Index: Integer): Boolean;
    function SearchSection(const ACaption: string): Integer;
    procedure Customize(HelpCtx: THelpContext);
{$IFNDEF VER80}
    procedure GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
      Root: TComponent {$ENDIF}); override;
    procedure SaveLayoutReg(IniFile: TRegIniFile);
    procedure RestoreLayoutReg(IniFile: TRegIniFile);
{$ENDIF}
    procedure SaveLayout(IniFile: TIniFile);
    procedure RestoreLayout(IniFile: TIniFile);
    Procedure ReArrangeButtons(ByList:TStringList); //added 12.12.2001 by JB.
    function ItemsCount(Section: Integer): Integer;
    function Items(Section, Index: Integer): TSpeedItem;
    property EditMode: Boolean read GetEditing;
    property SectionCount: Integer read GetSectionCount;
    property Sections[Index: Integer]: TSpeedbarSection read GetSection;
    property Orientation: TBarOrientation read GetOrientation write SetOrientation
      default boHorizontal;
    property OnAddItem: TNotifyEvent read FOnAddItem write FOnAddItem; { for internal use only }
  published
    property Font;
    property ParentFont default False;
    property BoundLines: TBoundLines read FBoundLines write SetBoundLines default [];
    property Position: TBarPosition read FPosition write FPosition default bpAuto;
    { ensure Position is declared before Align }
    property Align: TAlign read GetAlign write SetAlign default alTop;
    { ensure Options is declared before BtnOffset... }
    property Options: TSpeedbarOptions read FOptions write SetOptions
      default [sbAllowDrag, sbGrayedBtns];
    property BtnOffsetHorz: Integer index 0 read GetButtonsOffset write SetButtonsOffset
      stored True;
    property BtnOffsetVert: Integer index 1 read GetButtonsOffset write SetButtonsOffset
      stored True;
    property BtnWidth: Integer index 0 read GetButtonSize write SetButtonSize;
    property BtnHeight: Integer index 1 read GetButtonSize write SetButtonSize;
    property IniStorage: TFormPlacement read GetStorage write SetStorage;
    property Version: Integer read FVersion write FVersion default 0;
    property Wallpaper: TPicture read FWallpaper write SetWallpaper;
{$IFNDEF VER80}
    property Images: TImageList read FImages write SetImages;
{$ENDIF}
{$IFDEF RX_D4}
    property BiDiMode;
    property Constraints;
    property ParentBiDiMode;
{$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Locked;
{$IFDEF RX_D7}
    property ParentBackground default False;
{$ENDIF}
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint default False;
    property PopupMenu;
    property ShowHint default True;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnApplyAlign: TApplyAlignEvent read FOnApplyAlign write FOnApplyAlign;
    property OnCustomize: TNotifyEvent read FOnCustomize write FOnCustomize;
    property OnPosChanged: TNotifyEvent read FOnPosChanged write FOnPosChanged;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write FOnVisibleChanged;
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
{$IFDEF RX_D7}
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelKind;
    property Caption;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property FullRepaint;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
{$ENDIF}
{$IFDEF RX_D9}
    property VerticalAlignment;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnMouseActivate;
{$ENDIF}
{$IFDEF RX_D10}
    property Padding;
    property OnMouseEnter;
    property OnMouseLeave;
{$ELSE}
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF}
  end;

{ TSpeedItem }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSpeedItem = class(TComponent)
  private
{$IFDEF RX_D4}   // Polaris
    FCaption: String;
{$ELSE}
    FCaption: PString;
{$ENDIF}
    FEditing: Boolean;
    FEnabled: Boolean;
    FButton: TRxSpeedButton;
    FVisible: Boolean;
    FStored: Boolean;
    FParent: TSpeedBar;
    FSection: Integer;
    FSectionName: string;
    FThemedStyle: Boolean;
{$IFNDEF VER80}
    FImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
{$ENDIF}
{$IFDEF RX_D4}
    function GetAction: TBasicAction;
    procedure SetAction(Value: TBasicAction);
{$ENDIF}
    function GetAllowAllUp: Boolean;
    procedure SetAllowAllUp(Value: Boolean);
    function GetAllowTimer: Boolean;
    procedure SetAllowTimer(Value: Boolean);
    function GetBtnCaption: TCaption;
    procedure SetBtnCaption(const Value: TCaption);
    function GetGroupIndex: Integer;
    procedure SetGroupIndex(Value: Integer);
    function GetDown: Boolean;
    procedure SetDown(Value: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetLayout: TButtonLayout;
    procedure SetLayout(Value: TButtonLayout);
    function GetMargin: Integer;
    procedure SetMargin(Value: Integer);
    function GetNumGlyphs: TRxNumGlyphs;
    procedure SetNumGlyphs(Value: TRxNumGlyphs);
    function GetParentShowHint: Boolean;
    procedure SetParentShowHint(Value: Boolean);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetParentFont: Boolean;
    procedure SetParentFont(Value: Boolean);
    function IsFontStored: Boolean;
    function GetShowHint: Boolean;
    procedure SetShowHint(Value: Boolean);
    function IsShowHintStored: Boolean;
    function GetSpacing: Integer;
    procedure SetSpacing(Value: Integer);
    function GetCursor: TCursor;
    procedure SetCursor(Value: TCursor);
    function GetHint: string;
    procedure SetHint(const Value: string);
    function GetTag: Longint;
    procedure SetTag(Value: Longint);
    function GetDropDownMenu: TPopupMenu;
    procedure SetDropDownMenu(Value: TPopupMenu);
    function GetMarkDropDown: Boolean;
    procedure SetMarkDropDown(Value: Boolean);
    function GetWordWrap: Boolean;
    procedure SetWordWrap(Value: Boolean);
    function GetOnClick: TNotifyEvent;
    procedure SetOnClick(Value: TNotifyEvent);
    function GetOnDblClick: TNotifyEvent;
    procedure SetOnDblClick(Value: TNotifyEvent);
    function GetOnMouseDown: TMouseEvent;
    procedure SetOnMouseDown(Value: TMouseEvent);
    function GetOnMouseMove: TMouseMoveEvent;
    procedure SetOnMouseMove(Value: TMouseMoveEvent);
    function GetOnMouseUp: TMouseEvent;
    procedure SetOnMouseUp(Value: TMouseEvent);
    function GetOnMouseEnter: TNotifyEvent;
    procedure SetOnMouseEnter(Value: TNotifyEvent);
    function GetOnMouseLeave: TNotifyEvent;
    procedure SetOnMouseLeave(Value: TNotifyEvent);
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetEditing(Value: Boolean);
    function GetLeft: Integer;
    function GetTop: Integer;
    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    function GetSection: Integer;
    procedure SetSection(Value: Integer);
    function GetSectionName: string;
    {procedure SetSectionName(const Value: string);}
    procedure ReadSection(Reader: TReader);
    procedure WriteSection(Writer: TWriter);
    procedure ReadSectionName(Reader: TReader);
    procedure WriteSectionName(Writer: TWriter);
    procedure SetThemedStyle(const Value: Boolean);
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
{$IFNDEF VER80}
    function GetParentComponent: TComponent; override;
    procedure SetParentComponent(Value: TComponent); override;
{$ENDIF}
    procedure ButtonClick;
    function CheckBtnMenuDropDown: Boolean;
    procedure Click; virtual;
    procedure UpdateSection;
    procedure InvalidateItem;
    property ASection: Integer read GetSection write SetSection;
    property SpeedBar: TSpeedBar read FParent;
    property Button: TRxSpeedButton read FButton;
  published
{$IFDEF RX_D4}
    property Action: TBasicAction read GetAction write SetAction;
{$ENDIF}
    property AllowAllUp: Boolean read GetAllowAllUp write SetAllowAllUp default False;
    property AllowTimer: Boolean read GetAllowTimer write SetAllowTimer default False;
    property BtnCaption: TCaption read GetBtnCaption write SetBtnCaption;
    property Caption: TCaption read GetCaption write SetCaption;
    property GroupIndex: Integer read GetGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read GetDown write SetDown default False;
    property DropDownMenu: TPopupMenu read GetDropDownMenu write SetDropDownMenu;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Hint: string read GetHint write SetHint;
{$IFNDEF VER80}
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
{$ENDIF}
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphTop;
    property Margin: Integer read GetMargin write SetMargin default -1;
    property MarkDropDown: Boolean read GetMarkDropDown write SetMarkDropDown default True;
    property NumGlyphs: TRxNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentShowHint: Boolean read GetParentShowHint write SetParentShowHint default True;
    property ParentFont: Boolean read GetParentFont write SetParentFont default True;
    property ShowHint: Boolean read GetShowHint write SetShowHint stored IsShowHintStored;
    property Spacing: Integer read GetSpacing write SetSpacing default 4;
    property Stored: Boolean read FStored write FStored default True;
    property Tag: Longint read GetTag write SetTag default 0;
    property ThemedStyle: Boolean read FThemedStyle write SetThemedStyle default False;
    property Left: Integer read GetLeft write SetLeft default 0;
    property Top: Integer read GetTop write SetTop default 0;
    property Visible: Boolean read FVisible write SetVisible default False;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap default False;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnMouseEnter: TNotifyEvent read GetOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read GetOnMouseLeave write SetOnMouseLeave;
  end;

{ TSpeedbarSection }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSpeedbarSection = class(TComponent)
  private
    FList: TList;
{$IFDEF RX_D4}   // Polaris
    FTitle: String;
{$ELSE}
    FTitle: PString;
{$ENDIF}
    FParent: TSpeedBar;
    function Get(Index: Integer): TSpeedItem;
    procedure Put(Index: Integer; Item: TSpeedItem);
    function GetCount: Integer;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetSpeedbar(Value: TSpeedBar);
    procedure ValidateCaption(const NewCaption: string);
  protected
{$IFNDEF VER80}
    procedure SetParentComponent(Value: TComponent); override;
{$ELSE}
    procedure ReadState(Reader: TReader); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HasParent: Boolean; override;
{$IFNDEF VER80}
    function GetParentComponent: TComponent; override;
{$ENDIF}
    procedure Clear;
    procedure RemoveItem(Item: TSpeedItem);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSpeedItem read Get write Put; default;
    property List: TList read FList; { for internal use only }
    property SpeedBar: TSpeedBar read FParent write SetSpeedbar stored False;
  published
    property Caption: string read GetTitle write SetTitle;
    property Index: Integer read GetIndex write SetIndex stored False;
  end;

{ TBtnControl }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TBtnControl = class(TCustomControl)
  private
    FImage: TButtonImage;
    FSpacing, FMargin: Integer;
    FLayout: TButtonLayout;
{$IFNDEF VER80}
    FImageIndex: Integer;
    FImages: TImageList;
{$ENDIF}
    function GetCaption: TCaption;
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TRxNumGlyphs;
    function GetWordWrap: Boolean;
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: TCaption);
    procedure SetNumGlyphs(Value: TRxNumGlyphs);
    procedure SetGlyph(Value: TBitmap);
    procedure SetWordWrap(Value: Boolean);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignSpeedItem(Item: TSpeedItem);
    procedure Activate(Rect: TRect);
    procedure ReleaseHandle;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Caption: TCaption read GetCaption write SetCaption;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs: TRxNumGlyphs read GetNumGlyphs write SetNumGlyphs;
    property Spacing: Integer read FSpacing write FSpacing;
{$IFNDEF VER80}
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Images: TImageList read FImages write FImages;
{$ENDIF}
    property Margin: Integer read FMargin write FMargin;
    property Layout: TButtonLayout read FLayout write FLayout;
    property WordWrap: Boolean read GetWordWrap write SetWordWrap;
    property Font;
  end;

const
{ Values for WParam for CM_SPEEDBARCHANGED message }
  SBR_CHANGED        = 0; { change buttons properties  }
  SBR_DESTROYED      = 1; { destroy speedbar           }
  SBR_BTNSELECT      = 2; { select button in speedbar  }
  SBR_BTNSIZECHANGED = 3; { button size changed        }

{ Utility routines for Speedbar Editors }

function FindSpeedBar(const Pos: TPoint): TSpeedBar; {$IFDEF RX_D9}inline;{$ENDIF}
procedure DrawCellButton(Grid: TDrawGrid; R: TRect; Item: TSpeedItem;
  Image: TButtonImage {$IFDEF RX_D4}; ARightToLeft: Boolean = False {$ENDIF});
function NewSpeedSection(ASpeedbar: TSpeedBar; const ACaption: string): Integer; {$IFDEF RX_D9}inline;{$ENDIF}
function NewSpeedItem(AOwner: TComponent; ASpeedbar: TSpeedBar; Section: Integer;
  const AName: string): TSpeedItem; {$IFDEF RX_D9}inline;{$ENDIF}

implementation

uses
  Dialogs, RxMaxMin, RxVCLUtils, RxAppUtils, Consts, RxConst, RxSbSetup, // Polaris
  RxResConst,
  {$IFDEF RX_D6} RTLConsts,{$ENDIF} RxStrUtils; // Polaris

const
  DefaultButtonSize: TPoint = (X: DefButtonWidth; Y: DefButtonHeight);
  DragFrameWidth = 3;
  StartDragOffset = 4;
  Registered: Boolean = False;

const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

{ TSpeedbarSection }

constructor TSpeedbarSection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
{$IFDEF RX_D4}   // Polaris
  FTitle := EmptyStr;
{$ELSE}
  FTitle := NullStr;
{$ENDIF}
end;

destructor TSpeedbarSection.Destroy;
begin
  Clear;
  if FParent <> nil then FParent.DeleteSection(Index);
{$IFNDEF RX_D4}   // Polaris
  DisposeStr(FTitle);
  FTitle := NullStr;
{$ENDIF}
  FList.Free;
  inherited Destroy;
end;

procedure TSpeedbarSection.Clear;
begin
  while FList.Count > 0 do
  begin
    TSpeedItem(FList[0]).Free;
    FList.Delete(0);
  end;
end;

function TSpeedbarSection.Get(Index: Integer): TSpeedItem;
begin
  Result := TSpeedItem(FList[Index]);
end;

procedure TSpeedbarSection.Put(Index: Integer; Item: TSpeedItem);
begin
  FList[Index] := Item;
end;

function TSpeedbarSection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSpeedbarSection.GetIndex: Integer;
begin
  if FParent <> nil then Result := FParent.FSections.IndexOf(Self)
  else Result := -1;
end;

procedure TSpeedbarSection.SetIndex(Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := GetIndex;
  if CurIndex >= 0 then
  begin
    Count := FParent.FSections.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      FParent.FSections.Delete(CurIndex);
      FParent.FSections.Insert(Value, Self);
    end;
  end;
end;

function TSpeedbarSection.HasParent: Boolean;
begin
  Result := True;
end;

procedure TSpeedbarSection.SetSpeedbar(Value: TSpeedBar);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if FParent <> nil then FParent.DeleteSection(Index);
  if Value <> nil then Value.AppendSection(Self);
  if CurIndex >= 0 then Index := CurIndex;
end;

{$IFNDEF VER80}

function TSpeedbarSection.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TSpeedbarSection.SetParentComponent(Value: TComponent);
begin
  SpeedBar := Value as TSpeedBar;
end;

{$ELSE}

procedure TSpeedbarSection.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TSpeedBar then SpeedBar := TSpeedBar(Reader.Parent);
end;

{$ENDIF}

procedure TSpeedbarSection.RemoveItem(Item: TSpeedItem);
var
  I: Integer;
begin
  I := FList.IndexOf(Item);
  if I >= 0 then
  begin
    Item.FButton.Parent := nil;
    Item.FParent := nil;
    Item.FSection := -1;
    FList.Delete(I);
  end;
end;

procedure TSpeedbarSection.ValidateCaption(const NewCaption: string);
var
  I: Integer;
begin
  if FParent <> nil then
  begin
    I := FParent.SearchSection(NewCaption);
    if (I <> Index) and (I >= 0) then
      raise ESpeedbarError.Create(ResStr(SDuplicateString));
  end;
end;

procedure TSpeedbarSection.SetTitle(const Value: string);
begin
  if not (csLoading in ComponentState) then ValidateCaption(Value);
{$IFDEF RX_D4}   // Polaris
  FTitle := Value;
{$ELSE}
  AssignStr(FTitle, Value);
{$ENDIF}
end;

function TSpeedbarSection.GetTitle: string;
begin
{$IFDEF RX_D4}   // Polaris
  Result := FTitle;
{$ELSE}
  Result := FTitle^;
{$ENDIF}
end;

{ TSpeedbarButton }

type
  TSpeedbarButton = class(TRxSpeedButton)
  private
    FItem: TSpeedItem;
    FBtn: TBtnControl;
    procedure InvalidateGlyph;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintGlyph(Canvas: TCanvas; ARect: TRect; AState: TRxButtonState;
      DrawMark: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

constructor TSpeedbarButton.Create(AOwner: TComponent);
begin
  FItem := TSpeedItem(AOwner);
  { Ensure FItem is assigned before inherited Create }
  inherited Create(AOwner);
  Visible := False;
  Style := bsNew;
  ParentShowHint := True;
  ParentFont := True;
end;

destructor TSpeedbarButton.Destroy;
begin
  FBtn.Free;
  inherited Destroy;
end;

procedure TSpeedbarButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FItem.Speedbar <> nil) then
  begin
    case FItem.Speedbar.Orientation of
      boHorizontal: ATop := Max(FItem.Speedbar.FOffset.Y, ATop);
      boVertical: ALeft := Max(FItem.Speedbar.FOffset.X, ALeft);
    end;
  end;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TSpeedbarButton.CMVisibleChanged(var Message: TMessage);
begin
  if Visible then ControlStyle := ControlStyle + [csOpaque]
  else ControlStyle := ControlStyle - [csOpaque];
  inherited;
end;

procedure TSpeedbarButton.WndProc(var Message: TMessage);
begin
  if FItem.FEditing and (csDesigning in ComponentState) and
    (Message.Msg >= WM_MOUSEFIRST) and (Message.Msg <= WM_MOUSELAST) then
  begin
    if (Message.Msg = WM_LBUTTONDOWN) and not Visible then
      inherited WndProc(Message)
    else Dispatch(Message);
  end
  else inherited WndProc(Message);
end;

procedure TSpeedbarButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if FItem.FEditing and Visible and (Button = mbLeft) and (FItem.Speedbar <> nil) then
  begin
    P := ClientToScreen(Point(FItem.Speedbar.BtnWidth {div 2},
      FItem.Speedbar.BtnHeight {div 2}));
    X := P.X; Y := P.Y;
    if FBtn = nil then
    begin
      SetCursorPos(X, Y);
      FBtn := TBtnControl.Create(Self);
      FBtn.AssignSpeedItem(FItem);
    end;
    BringToFront;
  end
  else inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSpeedbarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  R: TRect;
begin
  if FItem.FEditing and (FBtn <> nil) then
  begin
    P := ClientToScreen(Point(X - (FBtn.Width {div 2}),
      Y - (FBtn.Height {div 2})));
    X := P.X; Y := P.Y;
    if FItem.SpeedBar <> nil then
    begin
      Visible := False;
      if (csDesigning in ComponentState) then
      begin
        R := BoundsRect;
        InvalidateRect(FItem.Speedbar.Handle, @R, True);
      end;
      P := FItem.SpeedBar.ScreenToClient(P);
      if PtInRect(FItem.SpeedBar.ClientRect, P) then
      begin
        FBtn.Activate(Bounds(X, Y, FBtn.Width, FBtn.Height));
      end
      else FBtn.ReleaseHandle;
    end;
  end
  else inherited MouseMove(Shift, X, Y);
end;

procedure TSpeedbarButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if FItem.FEditing and (FBtn <> nil) then
  begin
    X := X - (FBtn.Width {div 2});
    Y := Y - (FBtn.Height {div 2});
    FBtn.Free;
    FBtn := nil;
    P := ClientToScreen(Point(X, Y));
    if FItem.SpeedBar <> nil then
    begin
      P := FItem.SpeedBar.ScreenToClient(P);
      if PtInRect(FItem.SpeedBar.ClientRect, P) then
      begin
        if not FItem.SpeedBar.AcceptDropItem(FItem, P.X, P.Y) then
        begin
          SendMessage(FItem.Speedbar.FEditWin, CM_SPEEDBARCHANGED, SBR_CHANGED,
            LPARAM(FItem.Speedbar));
        end
        else
        begin
          SendMessage(FItem.Speedbar.FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSELECT,
            LPARAM(FItem));
          Invalidate;
        end;
      end
      else
      begin
        SendToBack;
        FItem.Visible := False;
        SendMessage(FItem.Speedbar.FEditWin, CM_SPEEDBARCHANGED, SBR_CHANGED,
          LPARAM(FItem.Speedbar));
      end;
    end;
  end
  else inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSpeedbarButton.InvalidateGlyph;
begin
  TRxButtonGlyph(ButtonGlyph).Invalidate;
end;

procedure TSpeedbarButton.PaintGlyph(Canvas: TCanvas; ARect: TRect;
  AState: TRxButtonState; DrawMark: Boolean);
begin
{$IFNDEF VER80}
  if (FItem.Speedbar <> nil) then
  begin
    TRxButtonGlyph(ButtonGlyph).DrawEx(Canvas, ARect, Caption, Layout,
      Margin, Spacing, DrawMark, FItem.Speedbar.Images, FItem.FImageIndex,
      AState, {$IFDEF RX_D4} DrawTextBiDiModeFlags(Alignments[Alignment])
      {$ELSE} Alignments[Alignment] {$ENDIF});
  end
  else
{$ENDIF}
    inherited PaintGlyph(Canvas, ARect, AState, DrawMark);
end;

procedure TSpeedbarButton.Paint;
begin
  if Visible then inherited Paint;
end;

{ TSpeedItem }

constructor TSpeedItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThemedStyle := False;
  FButton := TSpeedbarButton.Create(Self);
  FButton.ThemedStyle := Self.ThemedStyle;
  FButton.Visible := False;
  FButton.SetBounds(0, 0, DefaultButtonSize.X, DefaultButtonSize.Y);
{$IFDEF RX_D4}   // Polaris
  FCaption := EmptyStr;
{$ELSE}
  FCaption := NullStr;
{$ENDIF}
  ShowHint := True;
  ParentShowHint := True;
  FVisible := False;
  FStored := True;
  FEnabled := True;
  FEditing := False;
  FParent := nil;
{$IFNDEF VER80}
  FImageIndex := -1;
{$ENDIF}
end;

destructor TSpeedItem.Destroy;
begin
  FVisible := False;
  if FParent <> nil then FParent.RemoveItem(Self);
  FButton.Free;
{$IFNDEF RX_D4}   // Polaris
  DisposeStr(FCaption);
  FCaption := NullStr;
{$ENDIF}
  inherited Destroy;
end;

function TSpeedItem.GetCaption: TCaption;
begin
{$IFDEF RX_D4}   // Polaris
  Result := TCaption(FCaption);
{$ELSE}
  Result := TCaption(FCaption^);
{$ENDIF}
end;

procedure TSpeedItem.SetCaption(const Value: TCaption);
var
  ChangeHint: Boolean;
begin
  ChangeHint := (Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState) and
    (Caption = GetShortHint(Hint));
{$IFDEF RX_D4}   // Polaris
  FCaption := Value;
{$ELSE}
  AssignStr(FCaption, Value);
{$ENDIF}
  if ChangeHint then
  begin
    if Pos('|', Value) = 0 then
    begin
      if Pos('|', Hint) = 0 then Hint := Value + '|'
      else Hint := Value + '|' + GetLongHint(Hint);
    end
    else
    begin
      if GetLongHint(Value) = '' then
        Hint := GetShortHint(Value) + '|' + GetLongHint(Hint)
      else Hint := Value;
    end;
  end;
end;

procedure TSpeedItem.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (Name = Caption) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then Caption := Value;
end;

procedure TSpeedItem.SetEditing(Value: Boolean);
begin
  FEditing := Value;
  if FEditing then
  begin
    FButton.Enabled := True;
    FButton.Flat := False;
  end
  else
  begin
    SetEnabled(FEnabled);
    if SpeedBar <> nil then
      FButton.Flat := (sbFlatBtns in SpeedBar.Options);
  end;
end;

function TSpeedItem.HasParent: Boolean;
begin
  Result := True;
end;

procedure TSpeedItem.DefineProperties(Filer: TFiler);

{$IFNDEF VER80}
  function DoWrite: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := GetSectionName <> TSpeedItem(Filer.Ancestor).GetSectionName
    else Result := True;
  end;
{$ENDIF}

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Section', ReadSection, WriteSection, False);
  Filer.DefineProperty('SectionName', ReadSectionName, WriteSectionName,
    {$IFNDEF VER80} DoWrite {$ELSE} True {$ENDIF});
end;

procedure TSpeedItem.ReadSectionName(Reader: TReader);
begin
  FSectionName := Reader.ReadString;
end;

procedure TSpeedItem.WriteSectionName(Writer: TWriter);
begin
  Writer.WriteString(GetSectionName);
end;

procedure TSpeedItem.ReadSection(Reader: TReader);
begin
  FSection := Reader.ReadInteger;
end;

procedure TSpeedItem.WriteSection(Writer: TWriter);
begin
  UpdateSection;
  Writer.WriteInteger(FSection);
end;

{$IFNDEF VER80}

function TSpeedItem.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TSpeedItem.SetParentComponent(Value: TComponent);
var
  I: Integer;
begin
  if not (csLoading in ComponentState) then
  begin
    if FParent <> nil then FParent.RemoveItem(Self);
    if (Value <> nil) and (Value is TSpeedBar) then
    begin
      I := TSpeedBar(Value).SearchSection(FSectionName);
      if I >= 0 then FSection := I;
      TSpeedBar(Value).AddItem(FSection, Self);
    end;
  end;
end;

procedure TSpeedItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    TSpeedbarButton(FButton).InvalidateGlyph;
    FButton.Invalidate;
  end;
end;

{$ENDIF}

procedure TSpeedItem.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TSpeedBar then
  begin
    if FSectionName <> '' then
      FSection := TSpeedBar(Reader.Parent).SearchSection(FSectionName);
    TSpeedBar(Reader.Parent).AddItem(Max(FSection, 0), Self);
  end;
end;

function TSpeedItem.GetSection: Integer;
begin
  UpdateSection;
  Result := FSection;
end;

procedure TSpeedItem.SetSection(Value: Integer);
begin
  if Speedbar = nil then FSection := Value;
end;

function TSpeedItem.GetSectionName: string;
begin
  UpdateSection;
  if FSection >= 0 then Result := FParent.Sections[FSection].Caption
  else Result := FSectionName;
end;

{
procedure TSpeedItem.SetSectionName(const Value: string);
begin
  if FParent <> nil then FSection := FParent.SearchSection(Value)
  else FSection := -1;
  FSectionName := Value;
end;
}

procedure TSpeedItem.InvalidateItem;
begin
  FSection := -1;
end;

procedure TSpeedItem.UpdateSection;
var
  I: Integer;
begin
  if FParent <> nil then FParent.FindItem(Self, FSection, I)
  else FSection := -1;
end;

procedure TSpeedItem.SetEnabled(Value: Boolean);
begin
  if ((FButton.Enabled <> Value) or (FEnabled <> Value)) then
  begin
    FEnabled := Value;
    if not FEditing then begin
      if (SpeedBar <> nil) and Value then
        FButton.Enabled := (Value and SpeedBar.Enabled)
      else FButton.Enabled := Value;
    end;
  end;
end;

procedure TSpeedItem.SetVisible(Value: Boolean);
begin
  if (FButton.Visible <> Value) or (FVisible <> Value) or (Value and (FButton.Parent = nil)) then
  begin
    FVisible := Value;
    if (SpeedBar <> nil) and Value then
      FButton.Visible := Value and SpeedBar.Visible
    else FButton.Visible := Value;
    if Value then FButton.Parent := Speedbar;
  end;
end;

function TSpeedItem.GetAllowAllUp: Boolean;
begin
  Result := FButton.AllowAllUp;
end;

procedure TSpeedItem.SetAllowAllUp(Value: Boolean);
begin
  FButton.AllowAllUp := Value;
end;

function TSpeedItem.GetAllowTimer: Boolean;
begin
  Result := FButton.AllowTimer;
end;

procedure TSpeedItem.SetAllowTimer(Value: Boolean);
begin
  FButton.AllowTimer := Value;
end;

function TSpeedItem.GetBtnCaption: TCaption;
begin
  Result := FButton.Caption;
end;

procedure TSpeedItem.SetBtnCaption(const Value: TCaption);
begin
  FButton.Caption := Value;
end;

function TSpeedItem.GetGroupIndex: Integer;
begin
  Result := FButton.GroupIndex;
end;

procedure TSpeedItem.SetGroupIndex(Value: Integer);
begin
  FButton.GroupIndex := Value;
end;

function TSpeedItem.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;

procedure TSpeedItem.SetOnClick(Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;

function TSpeedItem.GetOnDblClick: TNotifyEvent;
begin
  Result := FButton.OnDblClick;
end;

procedure TSpeedItem.SetOnDblClick(Value: TNotifyEvent);
begin
  FButton.OnDblClick := Value;
end;

function TSpeedItem.GetOnMouseDown: TMouseEvent;
begin
  Result := FButton.OnMouseDown;
end;

procedure TSpeedItem.SetOnMouseDown(Value: TMouseEvent);
begin
  FButton.OnMouseDown := Value;
end;

function TSpeedItem.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := FButton.OnMouseMove;
end;

procedure TSpeedItem.SetOnMouseMove(Value: TMouseMoveEvent);
begin
  FButton.OnMouseMove := Value;
end;

function TSpeedItem.GetOnMouseUp: TMouseEvent;
begin
  Result := FButton.OnMouseUp;
end;

procedure TSpeedItem.SetOnMouseUp(Value: TMouseEvent);
begin
  FButton.OnMouseUp := Value;
end;

function TSpeedItem.GetOnMouseEnter: TNotifyEvent;
begin
  Result := FButton.OnMouseEnter;
end;

procedure TSpeedItem.SetOnMouseEnter(Value: TNotifyEvent);
begin
  FButton.OnMouseEnter := Value;
end;

function TSpeedItem.GetOnMouseLeave: TNotifyEvent;
begin
  Result := FButton.OnMouseLeave;
end;

procedure TSpeedItem.SetOnMouseLeave(Value: TNotifyEvent);
begin
  FButton.OnMouseLeave := Value;
end;

function TSpeedItem.GetDown: Boolean;
begin
  Result := FButton.Down;
end;

procedure TSpeedItem.SetDown(Value: Boolean);
begin
  FButton.Down := Value;
end;

function TSpeedItem.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TSpeedItem.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

function TSpeedItem.GetLayout: TButtonLayout;
begin
  Result := FButton.Layout;
end;

procedure TSpeedItem.SetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

function TSpeedItem.GetMargin: Integer;
begin
  Result := FButton.Margin;
end;

procedure TSpeedItem.SetMargin(Value: Integer);
begin
  FButton.Margin := Value;
end;

function TSpeedItem.GetNumGlyphs: TRxNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

procedure TSpeedItem.SetNumGlyphs(Value: TRxNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

function TSpeedItem.GetParentShowHint: Boolean;
begin
  Result := FButton.ParentShowHint;
end;

procedure TSpeedItem.SetParentShowHint(Value: Boolean);
begin
  FButton.ParentShowHint := Value;
end;

function TSpeedItem.GetShowHint: Boolean;
begin
  Result := FButton.ShowHint;
end;

procedure TSpeedItem.SetShowHint(Value: Boolean);
begin
  FButton.ShowHint := Value;
end;

function TSpeedItem.GetFont: TFont;
begin
  Result := FButton.Font;
end;

procedure TSpeedItem.SetFont(Value: TFont);
begin
  FButton.Font := Value;
end;

function TSpeedItem.GetParentFont: Boolean;
begin
  Result := FButton.ParentFont;
end;

procedure TSpeedItem.SetParentFont(Value: Boolean);
begin
  FButton.ParentFont := Value;
end;

function TSpeedItem.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

function TSpeedItem.IsShowHintStored: Boolean;
begin
  Result := not ParentShowHint;
end;

function TSpeedItem.GetSpacing: Integer;
begin
  Result := FButton.Spacing;
end;

procedure TSpeedItem.SetSpacing(Value: Integer);
begin
  FButton.Spacing := Value;
end;

function TSpeedItem.GetCursor: TCursor;
begin
  Result := FButton.Cursor;
end;

procedure TSpeedItem.SetCursor(Value: TCursor);
begin
  FButton.Cursor := Value;
end;

function TSpeedItem.GetHint: string;
begin
  Result := FButton.Hint;
end;

procedure TSpeedItem.SetHint(const Value: string);
begin
  FButton.Hint := Value;
end;

{$IFDEF RX_D4}
function TSpeedItem.GetAction: TBasicAction;
begin
  Result := FButton.Action;
end;

procedure TSpeedItem.SetAction(Value: TBasicAction);
begin
  FButton.Action := Value;
end;
{$ENDIF}

procedure TSpeedItem.ButtonClick;
begin
  FButton.ButtonClick;
end;

function TSpeedItem.CheckBtnMenuDropDown: Boolean;
begin
  Result := FButton.CheckBtnMenuDropDown;
end;

procedure TSpeedItem.Click;
begin
  FButton.Click;
end;

function TSpeedItem.GetTag: Longint;
begin
  Result := inherited Tag;
end;

procedure TSpeedItem.SetTag(Value: Longint);
begin
  inherited Tag := Value;
  FButton.Tag := Value;
end;

procedure TSpeedItem.SetThemedStyle(const Value: Boolean);
begin
  FThemedStyle := Value;
  FButton.ThemedStyle := Value;
end;

function TSpeedItem.GetDropDownMenu: TPopupMenu;
begin
  Result := FButton.DropDownMenu;
end;

procedure TSpeedItem.SetDropDownMenu(Value: TPopupMenu);
begin
  FButton.DropDownMenu := Value;
end;

function TSpeedItem.GetMarkDropDown: Boolean;
begin
  Result := FButton.MarkDropDown;
end;

procedure TSpeedItem.SetMarkDropDown(Value: Boolean);
begin
  FButton.MarkDropDown := Value;
end;

function TSpeedItem.GetWordWrap: Boolean;
begin
  Result := FButton.WordWrap;
end;

procedure TSpeedItem.SetWordWrap(Value: Boolean);
begin
  FButton.WordWrap := Value;
end;

function TSpeedItem.GetLeft: Integer;
begin
  Result := FButton.Left;
end;

function TSpeedItem.GetTop: Integer;
begin
  Result := FButton.Top;
end;

procedure TSpeedItem.SetLeft(Value: Integer);
begin
  FButton.Left := Value;
end;

procedure TSpeedItem.SetTop(Value: Integer);
begin
  FButton.Top := Value;
end;

{ TSpeedBar }

const
  InternalVer = 1;

constructor TSpeedBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSections := TList.Create;
  FButtonSize := DefaultButtonSize;
  FButtonStyle := bsNew;
  FWallpaper := TPicture.Create;
  FWallpaper.OnChange := WallpaperChanged;
  FIniLink := TIniLink.Create;
  FIniLink.OnSave := IniSave;
  FIniLink.OnLoad := IniLoad;
  FOffset.X := MinButtonsOffset;
  FOffset.Y := FOffset.X;
  Height := 2 * FOffset.Y + DefaultButtonSize.Y;
  FRowCount := 1;
  FEditWin := 0;
  FOptions := [sbAllowDrag, sbGrayedBtns];
  ControlStyle := ControlStyle - [csSetCaption {$IFNDEF VER80}, csReplicatable {$ENDIF}];
  ParentShowHint := False;
  ShowHint := True;
  SetFontDefault;
  inherited Align := alTop;
  FAlign := alTop;
  UpdateGridSize;
{$IFNDEF VER80}
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
{$ENDIF}
{$IFDEF RX_D7}
  ParentBackground := False;
{$ENDIF}
  if not Registered then
  begin
    RegisterClasses([TSpeedItem, TSpeedbarSection, TSpeedbarButton]);
    Registered := True;
  end;
end;

destructor TSpeedBar.Destroy;
begin
  FOnVisibleChanged := nil;
  FOnApplyAlign := nil;
  FOnPosChanged := nil;
  FIniLink.Free;
  FWallpaper.OnChange := nil;
  FWallpaper.Free;
  FWallpaper := nil;
  if FEditWin <> 0 then
  begin
    SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_DESTROYED, LPARAM(Self));
    FEditWin := 0;
  end;
  ClearSections;
  FSections.Free;
{$IFNDEF VER80}
  FImageChangeLink.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TSpeedBar.Loaded;
begin
  inherited Loaded;
  if (FReserved = 0) and FFix then
  begin { fix previous version error }
    inherited Align := alTop;
    FAlign := alTop;
  end;
  UpdateGridSize;
  ForEachItem(SetItemButtonSize, 0);
end;

procedure TSpeedBar.ReadData(Reader: TReader);
begin
  FReserved := Reader.ReadInteger;
end;

procedure TSpeedBar.WriteData(Writer: TWriter);
begin
  Writer.WriteInteger(InternalVer);
end;

procedure TSpeedBar.ReadAllowDrag(Reader: TReader);
begin
  if Reader.ReadBoolean then Options := Options + [sbAllowDrag]
  else Options := Options - [sbAllowDrag];
end;

procedure TSpeedBar.ReadDesignStyle(Reader: TReader);
begin
  FDesignStyle := Reader.ReadBoolean;
end;

procedure TSpeedBar.WriteDesignStyle(Writer: TWriter);
begin
  Writer.WriteBoolean(NewStyleControls);
end;

procedure TSpeedBar.ReadSections(Reader: TReader);
var
{$IFNDEF VER80}
  TmpList: TStrings;
  I: Integer;
{$ELSE}
  S: string;
{$ENDIF}
begin
{$IFNDEF VER80}
  TmpList := TStringList.Create;
  try
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      TmpList.AddObject(Reader.ReadString, nil);
    Reader.ReadListEnd;
    if (Reader.Ancestor = nil) or (TmpList.Count > 0) then
    begin
      for I := 0 to TmpList.Count - 1 do
      begin
        if SearchSection(TmpList[I]) < 0 then AddSection(TmpList[I]);
      end;
    end;
  finally
    TmpList.Free;
  end;
{$ELSE}
  Reader.ReadListBegin;
  FSections.Clear;
  while not Reader.EndOfList do
  begin
    S := Reader.ReadString;
    if SearchSection(S) < 0 then AddSection(S);
  end;
  Reader.ReadListEnd;
{$ENDIF}
end;

procedure TSpeedBar.WriteSections(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FSections.Count - 1 do
    Writer.WriteString(Sections[I].Caption);
  Writer.WriteListEnd;
end;

procedure TSpeedBar.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Sections', ReadSections, WriteSections, False);
  Filer.DefineProperty('NewStyle', ReadDesignStyle, WriteDesignStyle, False);
  Filer.DefineProperty('InternalVer', ReadData, WriteData,
    {$IFNDEF VER80} Filer.Ancestor = nil {$ELSE} True {$ENDIF});
  { AllowDrag reading for backward compatibility only }
  Filer.DefineProperty('AllowDrag', ReadAllowDrag, nil, False);
end;

function TSpeedBar.GetSection(Index: Integer): TSpeedbarSection;
begin
  Result := TSpeedbarSection(FSections[Index]);
end;

function TSpeedBar.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

procedure TSpeedBar.ForEachItem(Proc: TForEachItem; Data: Longint);
var
  I, Idx: Integer;
  Sect: TSpeedbarSection;
begin
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Sect := TSpeedbarSection(FSections[I]);
      for Idx := 0 to Sect.Count - 1 do
      begin
        if (Sect[Idx] <> nil) and Assigned(Proc) then
          Proc(TSpeedItem(Sect[Idx]), Data);
      end;
    end;
end;

function TSpeedBar.MinButtonsOffset: Integer;
begin
  Result := BorderWidth + 2 * Ord(not (sbFlatBtns in Options));
  if BevelOuter <> bvNone then Inc(Result, BevelWidth);
  if BevelInner <> bvNone then Inc(Result, BevelWidth);
end;

procedure TSpeedBar.SetItemVisible(Item: TSpeedItem; Data: Longint);
var
  ItemVisible: Boolean;
begin
  ItemVisible := Item.Visible and Self.Visible;
  Item.FButton.Visible := ItemVisible;
  if (Item.FButton.Parent <> Self) and ItemVisible then
    Item.FButton.Parent := Self;
end;

procedure TSpeedBar.SetItemEnabled(Item: TSpeedItem; Data: Longint);
begin
  Item.FButton.Enabled := Item.Enabled and Self.Enabled;
end;

procedure TSpeedBar.SetItemButtonSize(Item: TSpeedItem; Data: Longint);
begin
  ApplyItemSize(Item, Data);
  Item.Visible := Item.Visible; { update visible and parent after loading }
end;

procedure TSpeedBar.SwapItemBounds(Item: TSpeedItem; Data: Longint);
begin
  Item.FButton.SetBounds(Item.Top, Item.Left, FButtonSize.X, FButtonSize.Y);
end;

procedure TSpeedBar.SetFontDefault;
{$IFNDEF VER80}
var
  NCMetrics: TNonClientMetrics;
{$ENDIF}
begin
  ParentFont := False;
  with Font do
  begin
{$IFNDEF VER80}
    NCMetrics.cbSize := SizeOf(TNonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCMetrics, 0) then
    begin
      Handle := CreateFontIndirect(NCMetrics.lfMenuFont);
  {$IFNDEF VER90}
      Charset := DEFAULT_CHARSET;
  {$ENDIF}
    end
    else
    begin
{$ENDIF}
      Name := {$IFDEF RX_D6}'Tahoma'{$ELSE}'MS Sans Serif'{$ENDIF};
      Size := 8;
      Style := [];
      Color := clBtnText;
{$IFNDEF VER80}
    end;
{$ENDIF}
  end;
end;

procedure TSpeedBar.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then ForEachItem(SetItemVisible, 0);
  if Assigned(FOnVisibleChanged) then FOnVisibleChanged(Self);
end;

procedure TSpeedBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not ((csLoading in ComponentState) or (csDesigning in ComponentState)) then
    ForEachItem(SetItemEnabled, 0);
end;

{$IFNDEF RX_D10}
procedure TSpeedBar.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
  inherited;
end;

procedure TSpeedBar.CMMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
  inherited;
end;
{$ENDIF}

procedure TSpeedBar.WallpaperChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TSpeedBar.SetWallpaper(Value: TPicture);
begin
  FWallpaper.Assign(Value);
{$IFDEF RX_D7}
  if Assigned(Value) then
    ParentBackground := False;
{$ENDIF}
end;

procedure TSpeedBar.ClearSections;
begin
  while FSections.Count > 0 do RemoveSection(FSections.Count - 1);
  FSections.Clear;
end;

function TSpeedBar.Items(Section, Index: Integer): TSpeedItem;
var
  List: TSpeedbarSection;
begin
  Result := nil;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    List := Sections[Section];
    if List <> nil then
      if (Index >= 0) and (Index < List.Count) then
        Result := List[Index];
  end;
end;

function TSpeedBar.ItemsCount(Section: Integer): Integer;
begin
  Result := 0;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    if FSections[Section] <> nil then
      Result := Sections[Section].Count;
  end;
end;

procedure TSpeedBar.RemoveSection(Section: Integer);
var
  Sect: TSpeedbarSection;
  Item: TSpeedItem;
begin
  Sect := Sections[Section];
  if Sect <> nil then
  begin
    while Sect.Count > 0 do
    begin
      Item := Sect[0];
      Item.Free;
    end;
    Sect.FParent := nil;
    Sect.Free;
    FSections[Section] := nil;
  end;
  FSections.Delete(Section);
end;

procedure TSpeedBar.DeleteSection(Section: Integer);
var
  Sect: TSpeedbarSection;
  I: Integer;
begin
  Sect := Sections[Section];
  if Sect <> nil then
  begin
    for I := Sect.Count - 1 downto 0 do RemoveItem(TSpeedItem(Sect[I]));
    Sect.FParent := nil;
    FSections[Section] := nil;
  end;
  FSections.Delete(Section);
end;

procedure TSpeedBar.RemoveItem(Item: TSpeedItem);
var
  I, Index: Integer;
begin
  if FindItem(Item, I, Index) then
  begin
    Item.FButton.Parent := nil;
    Item.FParent := nil;
    Item.FSection := -1;
    Sections[I].FList.Delete(Index);
  end;
end;

function TSpeedBar.SearchSection(const ACaption: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FSections.Count - 1 do
    if Sections[I].Caption = ACaption then
    begin
      Result := I;
      Exit;
    end;
end;

function TSpeedBar.AppendSection(Value: TSpeedbarSection): Integer;
var
  UniqueName: string;
  I: Integer;
begin
  I := 0;
  UniqueName := Value.Caption;
  while SearchSection(UniqueName) >= 0 do
  begin
    Inc(I);
    UniqueName := Value.Caption + Format(' (%d)', [I]);
  end;
  Value.Caption := UniqueName;
  Result := FSections.Add(Value);
  if Result >= 0 then
  begin
    Value.FParent := Self;
    for I := 0 to Value.Count - 1 do
    begin
      Value[I].FSection := Result;
      SetItemParams(Value[I], not (csLoading in ComponentState));
    end;
  end;
end;

function TSpeedBar.AddSection(const ACaption: string): Integer;
var
  Section: TSpeedbarSection;
begin
  if Owner <> nil then Section := TSpeedbarSection.Create(Owner)
  else Section := TSpeedbarSection.Create(Self);
  Section.Caption := ACaption;
  Result := AppendSection(Section);
end;

procedure TSpeedBar.SetItemParams(Item: TSpeedItem; InitBounds: Boolean);
begin
  with Item do
  begin
    FParent := Self;
    with FButton do
    begin
      if InitBounds then SetBounds(0, 0, BtnWidth, BtnHeight);
      Style := FButtonStyle;
      Flat := (sbFlatBtns in Options);
      Transparent := (sbTransparentBtns in Options);
      GrayedInactive := (sbGrayedBtns in Options);
    end;
    SetEditing(FEditWin <> 0);
  end;
end;

function TSpeedBar.NewItem(AOwner: TComponent; Section: Integer;
  const AName: string): TSpeedItem;
begin
  Result := nil;
  if (Section >= 0) and (Section < FSections.Count) then
  begin
    Result := TSpeedItem.Create(AOwner);
    try
      Sections[Section].FList.Add(Result);
      Result.FSection := Section;
      SetItemParams(Result, True);
      if AName <> '' then
        with Result do
        begin
          Name := AName;
          Caption := AName;
          FButton.Visible := False;
          FButton.Parent := Self;
        end;
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TSpeedBar.AddItem(Section: Integer; Item: TSpeedItem);
var
  I, Index: Integer;
begin
  if FindItem(Item, I, Index) then
  begin
    Sections[I].FList.Delete(Index);
    if Section >= FSections.Count then Section := FSections.Count - 1;
    Sections[Section].FList.Add(Item);
    Item.FSection := Section;
    Exit;
  end;
  if (Section >= 0) and (Item <> nil) then
  begin
    if Assigned(FOnAddItem) then
    begin
      FOnAddItem(Item);
      Section := Item.FSection;
    end;
    if FSections.Count = 0 then Section := AddSection('')
    else if Section >= FSections.Count then Section := FSections.Count - 1;
    Sections[Section].FList.Add(Item);
    Item.FSection := Section;
    SetItemParams(Item, not (csLoading in ComponentState));
    Item.FButton.Visible := False;
    Item.FButton.Parent := Self;
  end;
end;

function TSpeedBar.FindItem(Item: TSpeedItem; var Section,
  Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  Section := -1;
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Index := Sections[I].FList.IndexOf(Item);
      if Index >= 0 then
      begin
        Section := I;
        Result := True;
        Exit;
      end;
    end;
end;

procedure TSpeedBar.AlignItemsToGrid;
begin
  ForEachItem(AlignItemToGrid, 0);
end;

procedure TSpeedBar.AlignItemToGrid(Item: TSpeedItem; Data: Longint);
begin
  if Item.Visible then
  begin
    if GetOrientation = boVertical then
    begin
      Item.Left := Trunc((Item.Left - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Item.Top := Round((Item.Top - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end
    else
    begin
      Item.Left := Round((Item.Left - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Item.Top := Trunc((Item.Top - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end;
  end;
end;

function TSpeedBar.AcceptDropItem(Item: TSpeedItem; X, Y: Integer): Boolean;
var
  I, Sect: Integer;
begin
  Result := False;
  if FindItem(Item, Sect, I) then
  begin
    if GetOrientation = boVertical then
    begin
      X := Trunc((X - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Y := Round((Y - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end
    else
    begin
      X := Round((X - FOffset.X) / FGridSize.X) * FGridSize.X + FOffset.X;
      Y := Trunc((Y - FOffset.Y) / FGridSize.Y) * FGridSize.Y + FOffset.Y;
    end;
    Item.Left := X;
    Item.Top := Y;
    Result := PtInRect(ClientRect, Point(X, Y));
    if Result then Item.FButton.BringToFront
    else Item.FButton.SendToBack;
    Item.Visible := Result;
  end;
end;

procedure TSpeedBar.SetItemEditing(Item: TSpeedItem; Data: Longint);
begin
  Item.SetEditing(FEditWin <> 0);
end;

function TSpeedBar.GetEditing: Boolean;
begin
  Result := (FEditWin <> 0);
end;

procedure TSpeedBar.SetEditing(Win: HWnd);
begin
  FEditWin := Win;
  ForEachItem(SetItemEditing, 0);
  if (FEditWin = 0) and not (csDesigning in ComponentState) then
    AfterCustomize;
end;

procedure TSpeedBar.Paint;
var
  XCnt, YCnt, X, Y: Integer;
  BevelSize, SaveIndex: Integer;
  Rect: TRect;
  C1, C2: TColor;

  procedure BevelLine(C: TColor; X1, Y1, X2, Y2: Integer);
  begin
    with Canvas do
    begin
      Pen.Color := C;
      MoveTo(X1, Y1);
      LineTo(X2, Y2);
    end;
  end;

begin
  if not FLocked then
  begin
    Rect := ClientRect;
    BevelSize := BorderWidth;
    if BevelOuter <> bvNone then Inc(BevelSize, BevelWidth);
    if BevelInner <> bvNone then Inc(BevelSize, BevelWidth);
    InflateRect(Rect, -BevelSize, -BevelSize);
    inherited Paint;
    {-->added 12.9.2003}
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Rect);
    {<--added 12.9.2003}
    if (FWallpaper.Graphic <> nil) and (FWallpaper.Width > 0) and
      (FWallpaper.Height > 0) then
    begin
      SaveIndex := SaveDC(Canvas.Handle);
      try
        with Rect do
          IntersectClipRect(Canvas.Handle, Left, Top, Right - Left +
            BevelSize, Bottom - Top + BevelSize);
        if sbStretchBitmap in Options then
          Canvas.StretchDraw(Rect, FWallpaper.Graphic)
        else
        begin
          XCnt := (ClientWidth - 2 * BevelSize) div FWallpaper.Width;
          YCnt := (ClientHeight - 2 * BevelSize) div FWallpaper.Height;
          for X := 0 to XCnt do
            for Y := 0 to YCnt do
              Canvas.Draw(Rect.Left + X * FWallpaper.Width,
                Rect.Top + Y * FWallpaper.Height, FWallpaper.Graphic);
        end;
      finally
        RestoreDC(Canvas.Handle, SaveIndex);
      end;
    end;
    if FBoundLines <> [] then
    begin
      C1 := clBtnShadow;
      C2 := clBtnHighlight;
      if blTop in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Top, Rect.Right, Rect.Top);
        BevelLine(C2, Rect.Left, Rect.Top + 1, Rect.Right, Rect.Top + 1);
      end;
      if blLeft in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom);
        BevelLine(C2, Rect.Left + 1, Rect.Top + Integer(blTop in FBoundLines), Rect.Left + 1, Rect.Bottom);
      end;
      if blBottom in FBoundLines then
      begin
        BevelLine(C1, Rect.Left, Rect.Bottom - 2, Rect.Right, Rect.Bottom - 2);
        BevelLine(C2, Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
      end;
      if blRight in FBoundLines then
      begin
        BevelLine(C1, Rect.Right - 2, Rect.Top, Rect.Right - 2, Rect.Bottom - Integer(blBottom in FBoundLines));
        BevelLine(C2, Rect.Right - 1, Rect.Top, Rect.Right - 1, Rect.Bottom);
      end;
    end;
  end;
end;

procedure TSpeedBar.ApplyOrientation(Value: TBarOrientation);
begin
  if (GetOrientation <> Value) and not (csReading in ComponentState) then
  begin
    FLocked := True;
    try
      FOrientation := Value;
      SwapInt(Integer(FButtonSize.X), Integer(FButtonSize.Y));
      SwapInt(Integer(FGridSize.X), Integer(FGridSize.Y));
      SwapInt(Integer(FOffset.X), Integer(FOffset.Y));
      ForEachItem(SwapItemBounds, 0);
    finally
      FLocked := False;
      Invalidate;
    end;
    if FEditWin <> 0 then
      SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSIZECHANGED, LPARAM(Self));
  end;
end;

procedure TSpeedBar.SetOrientation(Value: TBarOrientation);
begin
  if GetOrientation <> Value then
  begin
    if (FPosition = bpAuto) then
      raise ESpeedbarError.Create(RxLoadStr(SAutoSpeedbarMode));
    ApplyOrientation(Value);
  end;
end;

function TSpeedBar.GetOrientation: TBarOrientation;
begin
  if FPosition = bpCustom then Result := FOrientation
  else
    case Align of
      alLeft, alRight: Result := boVertical;
      alTop, alBottom: Result := boHorizontal;
    else
      Result := FOrientation;
    end;
end;

function TSpeedBar.GetAlign: TAlign;
begin
  Result := FAlign;
end;

procedure TSpeedBar.SetAlign(Value: TAlign);
var
  X, Y: Integer;
begin
  { fix previous version error }
  if (csLoading in ComponentState) and (Value = alNone) and
    (Position = bpAuto) then FFix := True;
  if Align <> Value then
  begin
    X := Width; Y := Height;
    if (FPosition = bpAuto) and (Value in [alClient, alNone]) then
      raise ESpeedbarError.Create(RxLoadStr(SAutoSpeedbarMode));
    inherited Align := Value;
    if (csLoading in ComponentState) then
    begin
      Width := X; Height := Y;
    end;
    if FPosition = bpAuto then
      case Value of
        alLeft, alRight: ApplyOrientation(boVertical);
        alTop, alBottom: ApplyOrientation(boHorizontal);
      else
        if not (csLoading in ComponentState) then
          raise ESpeedbarError.Create(RxLoadStr(SAutoSpeedbarMode));
      end;
    FAlign := inherited Align;
  end;
end;

procedure TSpeedBar.ChangeScale(M, D: Integer);
var
  Flags: TSbScaleFlags;
begin
  DisableAlign;
  try
    if csLoading in ComponentState then Flags := ScaleFlags
    else Flags := [sfOffsetX, sfOffsetY, sfBtnSizeX, sfBtnSizeY];
    if (sfBtnSizeX in Flags) and not (csFixedWidth in ControlStyle) then
      FButtonSize.X := MulDiv(FButtonSize.X, M, D);
    if (sfBtnSizeY in Flags) and not (csFixedHeight in ControlStyle) then
      FButtonSize.Y := MulDiv(FButtonSize.Y, M, D);
    if (sfOffsetX in Flags) then
      FOffset.X := MulDiv(FOffset.X, M, D);
    if (sfOffsetY in Flags) then
      FOffset.Y := MulDiv(FOffset.Y, M, D);
    UpdateGridSize;
    inherited ChangeScale(M, D);
    ApplyButtonSize;
    AlignItemsToGrid;
    FScaleFlags := [];
  finally
    EnableAlign;
  end;
end;

procedure TSpeedBar.AlignControls(AControl: TControl; var Rect: TRect);
var
  P: TPoint;
  Min: Integer;
begin
  if FBoundLines <> [] then
  begin
    if blTop in FBoundLines then Inc(Rect.Top, 2);
    if blBottom in FBoundLines then Dec(Rect.Bottom, 2);
    if blLeft in FBoundLines then Inc(Rect.Left, 2);
    if blRight in FBoundLines then Dec(Rect.Right, 2);
  end;
  inherited AlignControls(AControl, Rect);
  Min := MinButtonsOffset;
  if FOffset.X < Min then
  begin
    P.X := Min - FOffset.X;
    FOffset.X := Min;
  end
  else
    P.X := 0;
  if FOffset.Y < Min then
  begin
    P.Y := Min - FOffset.Y;
    FOffset.Y := Min;
  end
  else
    P.Y := 0;
  if not (csLoading in ComponentState) and ((P.X <> 0) or (P.Y <> 0)) then
    ForEachItem(OffsetItem, Longint(@P));
end;

procedure TSpeedBar.FlatItem(Item: TSpeedItem; Data: Longint);
begin
  Item.FButton.Flat := Boolean(Data);
end;

procedure TSpeedBar.GrayedItem(Item: TSpeedItem; Data: Longint);
begin
  Item.FButton.GrayedInactive := Boolean(Data);
end;

procedure TSpeedBar.TransparentItem(Item: TSpeedItem; Data: Longint);
begin
  Item.FButton.Transparent := Boolean(Data);
end;

procedure TSpeedBar.SetBoundLines(Value: TBoundLines);
begin
  if FBoundLines <> Value then
  begin
    FBoundLines := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TSpeedBar.SetOptions(Value: TSpeedbarOptions);
var
  FlatChanged: Boolean;
begin
  if FOptions <> Value then
  begin
    FlatChanged := (sbFlatBtns in FOptions) <> (sbFlatBtns in Value);
    FOptions := Value;
    ForEachItem(FlatItem, Longint(sbFlatBtns in Options));
    ForEachItem(TransparentItem, Longint(sbTransparentBtns in Options));
    ForEachItem(GrayedItem, Longint(sbGrayedBtns in Options));
    UpdateGridSize;
    if FlatChanged then Realign;
    Invalidate;
  end;
end;

procedure TSpeedBar.OffsetItem(Item: TSpeedItem; Data: Longint);
var
  P: TPoint;
begin
  P := PPoint(Data)^;
  Item.FButton.SetBounds(Item.Left + P.X, Item.Top + P.Y, FButtonSize.X,
    FButtonSize.Y);
end;

function TSpeedBar.GetButtonsOffset(Index: Integer): Integer;
begin
  if Index = 0 then Result := FOffset.X
  else if Index = 1 then Result := FOffset.Y
  else Result := 0;
end;

procedure TSpeedBar.SetButtonsOffset(Index: Integer; Value: Integer);
var
  P: TPoint;
begin
  if Value < MinButtonsOffset then Value := MinButtonsOffset;
  P.X := 0; P.Y := 0;
  if Index = 0 then
  begin
    P.X := Value - FOffset.X;
    FOffset.X := Value;
    Include(FScaleFlags, sfOffsetX);
  end
  else if Index = 1 then
  begin
    P.Y := Value - FOffset.Y;
    FOffset.Y := Value;
    Include(FScaleFlags, sfOffsetY);
  end;
  if (P.X <> 0) or (P.Y <> 0) then
    ForEachItem(OffsetItem, Longint(@P));
end;

procedure TSpeedBar.UpdateGridSize;
var
  Base: Integer;
begin
  case Orientation of
    boHorizontal: Base := FButtonSize.X;
  else
    {boVertical:} Base := FButtonSize.Y;
  end;
  case Orientation of
    boHorizontal:
      begin
        FGridSize.X := Max(1, Min(8, Base div 3));
        while (Base mod FGridSize.X <> 0) do Inc(FGridSize.X);
        if (FGridSize.X = Base) and (Base > 1) then
        begin
          Dec(FGridSize.X);
          while (FGridSize.X > 1) and (Base mod FGridSize.X <> 0) do
            Dec(FGridSize.X);
        end;
        FGridSize.Y := FButtonSize.Y;
      end;
    boVertical:
      begin
        FGridSize.Y := Max(1, Min(8, Base div 3));
        while (Base mod FGridSize.Y <> 0) do Inc(FGridSize.Y);
        if (FGridSize.Y = Base) and (Base > 1) then
        begin
          Dec(FGridSize.Y);
          while (FGridSize.Y > 1) and (Base mod FGridSize.Y <> 0) do
            Dec(FGridSize.Y);
        end;
        FGridSize.X := FButtonSize.X;
      end;
  end;
end;

procedure TSpeedBar.ApplyItemSize(Item: TSpeedItem; Data: Longint);
begin
  with Item do
    FButton.SetBounds(FButton.Left, FButton.Top, FButtonSize.X, FButtonSize.Y);
end;

procedure TSpeedBar.ApplyButtonSize;
begin
  ForEachItem(ApplyItemSize, 0);
  if FEditWin <> 0 then { update speedbar editor }
    SendMessage(FEditWin, CM_SPEEDBARCHANGED, SBR_BTNSIZECHANGED, LPARAM(Self));
end;

function TSpeedBar.GetButtonSize(Index: Integer): Integer;
begin
  if Index = 0 then Result := FButtonSize.X
  else if Index = 1 then Result := FButtonSize.Y
  else Result := 0;
end;

procedure TSpeedBar.SetButtonSize(Index, Value: Integer);
var
  NewSize: TPoint;
begin
  NewSize.X := FButtonSize.X;
  NewSize.Y := FButtonSize.Y;
  if Index = 0 then
  begin
    NewSize.X := Value;
    Include(FScaleFlags, sfBtnSizeX);
  end
  else if Index = 1 then
  begin
    NewSize.Y := Value;
    Include(FScaleFlags, sfBtnSizeY);
  end
  else Exit;
  FButtonSize := NewSize;
  UpdateGridSize;
  if not (csReading in ComponentState) then
    case Orientation of
      boHorizontal:
        ClientHeight := Max(ClientHeight, 2 * FOffset.Y + FButtonSize.Y);
      boVertical:
        ClientWidth := Max(ClientWidth, 2 * FOffset.X + FButtonSize.X);
    end;
  ApplyButtonSize;
end;

{$IFNDEF VER80}

procedure TSpeedBar.GetChildren(Proc: TGetChildProc {$IFDEF RX_D3};
  Root: TComponent {$ENDIF});
var
  I, Idx: Integer;
  Sect: TSpeedbarSection;
  Item: TSpeedItem;
begin
  inherited GetChildren(Proc {$IFDEF RX_D3}, Root {$ENDIF});
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := Sections[I];
    if Sect <> nil then Proc(Sect);
  end;
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := Sections[I];
    if Sect <> nil then
      for Idx := 0 to Sect.Count - 1 do
      begin
        Item := Sect[Idx];
        if (Item <> nil) and (Item.Owner <> Self) then Proc(Item);
      end;
  end;
end;

procedure TSpeedBar.SetChildOrder(Component: TComponent; Order: Integer);
begin
  if FSections.IndexOf(Component) >= 0 then
    (Component as TSpeedbarSection).Index := Order;
end;

procedure TSpeedBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then SetImages(nil);
end;

procedure TSpeedBar.InvalidateItem(Item: TSpeedItem; Data: Longint);
begin
  with Item do
    if (Button <> nil) then
    begin
      TSpeedbarButton(Button).InvalidateGlyph;
      if FImageIndex >= 0 then Button.Invalidate;
    end;
end;

procedure TSpeedBar.ImageListChange(Sender: TObject);
begin
  ForEachItem(InvalidateItem, 0);
end;

procedure TSpeedBar.SetImages(Value: TImageList);
begin
  if Images <> nil then Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  ImageListChange(FImages);
end;

{$ELSE}

procedure TSpeedBar.WriteComponents(Writer: TWriter);
var
  I, Idx: Integer;
  Sect: TSpeedbarSection;
  Item: TSpeedItem;
begin
  inherited WriteComponents(Writer);
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := TSpeedbarSection(FSections[I]);
    if (Sect <> nil) and (Sect.Owner = Writer.Root) then
      Writer.WriteComponent(Sect);
  end;
  for I := 0 to FSections.Count - 1 do
  begin
    Sect := TSpeedbarSection(FSections[I]);
    if Sect <> nil then
      for Idx := 0 to Sect.Count - 1 do
      begin
        Item := TSpeedItem(Sect[Idx]);
        if (Item <> nil) and (Item.Owner = Writer.Root) then
          Writer.WriteComponent(Item);
      end;
  end;
end;

{$ENDIF}

function TSpeedBar.SearchItem(const ItemName: string): TSpeedItem;
var
  I, Idx: Integer;
  Sect: TSpeedbarSection;
  Item: TSpeedItem;
begin
  Result := nil;
  for I := 0 to FSections.Count - 1 do
    if FSections[I] <> nil then
    begin
      Sect := TSpeedbarSection(FSections[I]);
      for Idx := 0 to Sect.Count - 1 do
        if (Sect[Idx] <> nil) then
        begin
          Item := TSpeedItem(Sect[Idx]);
          if AnsiCompareText(Item.Name, ItemName) = 0 then
          begin
            Result := Item;
            Exit;
          end;
        end;
    end;
end;

type
  TSpeedbarPos = (bpTop, bpBottom, bpLeft, bpRight);
const
  PosToAlign: array[TSpeedbarPos] of TAlign = (alTop, alBottom, alLeft, alRight);

function TSpeedBar.GetFramePos(X, Y: Integer; var Apply: Boolean): Integer;
var
  P: TPoint;
  W, H: Double;
begin
  P := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));
  W := Parent.ClientWidth;
  H := Parent.ClientHeight;
  if P.Y <= P.X * (H / W) then
  begin { top or right }
    if P.Y >= H * (1 - P.X / W) then Result := Integer(bpRight)
    else Result := Integer(bpTop);
  end
  else
  begin { left or bottom }
    if P.Y >= H * (1 - P.X / W) then Result := Integer(bpBottom)
    else Result := Integer(bpLeft);
  end;
  if Assigned(FOnApplyAlign) then
    FOnApplyAlign(Self, PosToAlign[TSpeedbarPos(Result)], Apply);
end;

function TSpeedBar.GetFrameRect(X, Y: Integer): TRect;
var
  Pos: TSpeedbarPos;
  W: Integer;
  Apply: Boolean;

  function InsertBefore(C1, C2: TControl; AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop: Result := C1.Top < C2.Top;
      alBottom: Result := (C1.Top + C1.Height) > (C2.Top + C2.Height);
      alLeft: Result := C1.Left < C2.Left;
      alRight: Result := (C1.Left + C1.Width) > (C2.Left + C2.Width);
    end;
  end;

  function MaxRect: TRect;
  var
    I: Integer;
    Control: TControl;
  begin
    Result := Parent.ClientRect;
    for I := 0 to Parent.ControlCount - 1 do
    begin
      Control := Parent.Controls[I];
      if (Control.Visible) and (Control <> Self) and not (Control.Align in [alNone, alClient]) then
      begin
        if (Control.Align > PosToAlign[Pos]) or ((Control.Align = PosToAlign[Pos])
          and not InsertBefore(Control, Self, Control.Align)) then Continue;
        case Control.Align of
          alTop: Inc(Result.Top, Control.Height);
          alBottom: Dec(Result.Bottom, Control.Height);
          alLeft: Inc(Result.Left, Control.Width);
          alRight: Dec(Result.Right, Control.Width);
        end;
      end;
    end;
  end;

begin
  Apply := True;
  Pos := TSpeedbarPos(GetFramePos(X, Y, Apply));
  if Apply then
  begin
    Result := MaxRect;
    FPrevAlign := PosToAlign[Pos];
  end
  else begin
    Result := FPrevRect;
    Exit;
  end;
  with Result do
  begin
    TopLeft := Parent.ClientToScreen(TopLeft);
    BottomRight := Parent.ClientToScreen(BottomRight);
  end;
  case GetOrientation of
    boHorizontal: W := Height;
    boVertical: W := Width;
  else
    W := 0;
  end;
  case Pos of
    bpTop: Result.Bottom := Result.Top + W;
    bpBottom: Result.Top := Result.Bottom - W;
    bpLeft: Result.Right := Result.Left + W;
    bpRight: Result.Left := Result.Right - W;
  end;
end;

procedure TSpeedBar.StartDragFrame;
var
  Rect: TRect;
begin
  with Rect do
  begin
    TopLeft := ClientToScreen(Point(0, 0));
    BottomRight := ClientToScreen(Point(Width, Height));
  end;
  FPrevRect := Rect;
  FPrevAlign := Align;
  DrawInvertFrame(FPrevRect, DragFrameWidth);
  SetCursor(Screen.Cursors[crDragHand]);
  FDrag := True;
end;

procedure TSpeedBar.DragFrame(X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := GetFrameRect(X, Y);
  if not EqualRect(Rect, FPrevRect) then
  begin
    DrawInvertFrame(FPrevRect, DragFrameWidth);
    SetCursor(Screen.Cursors[crDragHand]);
    FPrevRect := Rect;
    DrawInvertFrame(FPrevRect, DragFrameWidth);
  end;
end;

procedure TSpeedBar.StopDragFrame(X, Y: Integer);
var
  Pos: TSpeedbarPos;
  Apply: Boolean;
begin
  DrawInvertFrame(FPrevRect, DragFrameWidth);
  SetCursor(Screen.Cursors[Cursor]);
  FDrag := False;
  if Align in [alLeft, alTop, alRight, alBottom] then
  begin
    Apply := True;
    Pos := TSpeedbarPos(GetFramePos(X, Y, Apply));
    Parent.DisableAlign;
    try
      if Apply then Align := PosToAlign[Pos]
      else Align := FPrevAlign;
    finally
      Parent.EnableAlign;
    end;
    PosChanged;
  end;
end;

function TSpeedBar.CheckResize(Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := False;
  if (FEditWin <> 0) and (sbAllowResize in Options) and not FDrag then
  begin
    if (Align in [alTop, alBottom]) and (X > 0) and (X <= ClientWidth) then
    begin
      case Align of
        alTop:
          Result :=  (Y > ClientHeight - StartDragOffset) and
            (Y <= ClientHeight + StartDragOffset);
        alBottom:
          Result :=  (Y > - StartDragOffset) and (Y <= StartDragOffset);
      end;
      if Result then SetCursor(Screen.Cursors[crSizeNS]);
    end;
    if (Align in [alLeft, alRight]) and (Y > 0) and (Y <= ClientHeight) then
    begin
      case Align of
        alLeft:
          Result :=  (X > ClientWidth - StartDragOffset) and
            (X <= ClientWidth + StartDragOffset);
        alRight:
          Result :=  (X > - StartDragOffset) and (X <= StartDragOffset);
      end;
      if Result then SetCursor(Screen.Cursors[crSizeWE]);
    end;
  end;
end;

procedure TSpeedBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (Parent <> nil) and CheckResize(Shift, X, Y) then
  begin
    FResizing := True;
    MouseCapture := True;
    Exit;
  end;
  if (Button = mbLeft) and (Parent <> nil) and (sbAllowDrag in Options) and
    (Align in [alLeft, alTop, alRight, alBottom]) then
  begin
    MouseCapture := True;
    FStartDrag := Point(X, Y);
  end;
end;

procedure TSpeedBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Cnt: Integer;
  P: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  CheckResize(Shift, X, Y);
  Cnt := 0;
  if (GetCapture = Handle) and (csLButtonDown in ControlState) then
    if FResizing then
    begin
      P := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));
      if not PointInRect(P, Parent.ClientRect) then Exit;
      case Align of
        alTop: Cnt := Abs(Y - (2 * FOffset.Y)) div BtnHeight;
        alLeft: Cnt := Abs(X - (2 * FOffset.X)) div BtnWidth;
        alBottom: Cnt := Abs(ClientHeight - (2 * FOffset.Y) - Y) div BtnHeight;
        alRight: Cnt := Abs(ClientWidth - (2 * FOffset.X) - X) div BtnWidth;
      end;
      Cnt := Max(1, Cnt);
      case Align of
        alTop, alBottom:
          begin
            SetCursor(Screen.Cursors[crSizeNS]);
            Height := Min(BtnHeight * Cnt + (2 * FOffset.Y), Parent.ClientHeight);
          end;
        alLeft, alRight:
          begin
            SetCursor(Screen.Cursors[crSizeWE]);
            Width := Min(BtnWidth * Cnt + (2 * FOffset.X), Parent.ClientWidth);
          end;
      end;
    end
    else if (sbAllowDrag in Options) then
    begin
      if FDrag then DragFrame(X, Y)
      else
      begin
        if (Abs(X - FStartDrag.X) > StartDragOffset) or
          (Abs(Y - FStartDrag.Y) > StartDragOffset) then StartDragFrame;
      end;
    end;
end;

procedure TSpeedBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) then
  begin
    if FResizing then
    begin
      FResizing := False;
      SetCursor(Screen.Cursors[Cursor]);
    end;
    if FDrag then StopDragFrame(X, Y);
    MouseCapture := False;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSpeedBar.PosChanged;
begin
  if Assigned(FOnPosChanged) then FOnPosChanged(Self);
end;

procedure TSpeedBar.AfterCustomize;
begin
  if Assigned(FOnCustomize) then FOnCustomize(Self);
end;

function TSpeedBar.GetStorage: TFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TSpeedBar.SetStorage(Value: TFormPlacement);
begin
  FIniLink.Storage := Value;
end;

procedure TSpeedBar.Customize(HelpCtx: THelpContext);
begin
  ShowSpeedbarSetupWindow(Self, HelpCtx);
end;

procedure TSpeedBar.IniSave(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalSaveLayout(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

procedure TSpeedBar.IniLoad(Sender: TObject);
begin
  if (Name <> '') and (FIniLink.IniObject <> nil) then
    InternalRestoreLayout(FIniLink.IniObject, FIniLink.RootSection +
      GetDefaultSection(Self));
end;

const
  { The following strings should not be localized }
  sPosition = 'Position';
  sCount = 'Count';
  sBtn = 'Button';
  sVer = 'Version';
  sPixelsPerInch = 'PixelsPerInch';
  sBtnWidth = 'BtnWidth';
  sBtnHeight = 'BtnHeight';
  sBarWidth = 'Width';

type
  PIniData = ^TIniData;
  TIniData = record
    IniFile: TObject;
    I: Integer;
    Sect: string;
  end;

procedure TSpeedBar.HideItem(Item: TSpeedItem; Data: Longint);
begin
  Item.Visible := False;
end;

procedure TSpeedBar.WriteItemLayout(Item: TSpeedItem; Data: Longint);
begin
  if Item.Visible and Item.Stored then
  begin
    Inc(PIniData(Data)^.I);
    IniWriteString(PIniData(Data)^.IniFile, PIniData(Data)^.Sect,
      sBtn + IntToStr(PIniData(Data)^.I),
      Format('%s,%d,%d', [Item.Name, Item.Left, Item.Top]));
  end;
end;

procedure TSpeedBar.InternalSaveLayout(IniFile: TObject;
  const Section: string);
var
  Data: TIniData;
begin
  Data.Sect := Section;
  Data.IniFile := IniFile;
  Data.I := 0;
  IniEraseSection(IniFile, Data.Sect);
  IniWriteInteger(IniFile, Data.Sect, sPosition, Integer(Align));
  if Align in [alTop, alBottom] then
    IniWriteInteger(IniFile, Data.Sect, sBarWidth, Height)
  else if Align in [alLeft, alRight] then
    IniWriteInteger(IniFile, Data.Sect, sBarWidth, Width);
  IniWriteInteger(IniFile, Data.Sect, sVer, FVersion);
  IniWriteInteger(IniFile, Data.Sect, sPixelsPerInch, Screen.PixelsPerInch);
  IniWriteInteger(IniFile, Data.Sect, sBtnWidth, FButtonSize.X);
  IniWriteInteger(IniFile, Data.Sect, sBtnHeight, FButtonSize.Y);
  ForEachItem(WriteItemLayout, Longint(@Data));
  IniWriteInteger(IniFile, Data.Sect, sCount, Data.I);
end;

procedure TSpeedBar.InternalRestoreLayout(IniFile: TObject;
  const Section: string);
const
  Delims = [' ',','];
var
  Item: TSpeedItem;
  Count, I: Integer;
  Sect, S: string;
begin
  Sect := Section;
  FPrevAlign := Align;
  if IniReadInteger(IniFile, Sect, sVer, FVersion) < FVersion then Exit;
  if sbAllowDrag in Options then
    try
      Align := TAlign(IniReadInteger(IniFile, Sect, sPosition, Integer(Align)));
    except
      Align := alTop;
    end;
  if Owner is TCustomForm then I := TForm(Owner).PixelsPerInch
  else I := 0;
  if Screen.PixelsPerInch <> IniReadInteger(IniFile, Sect, sPixelsPerInch, I) then
  begin
    if FPrevAlign <> Align then PosChanged;
    Exit;
  end;
  if sbAllowResize in Options then
  begin
    if Align in [alTop, alBottom] then
      Height := IniReadInteger(IniFile, Sect, sBarWidth, Height)
    else if Align in [alLeft, alRight] then
      Width := IniReadInteger(IniFile, Sect, sBarWidth, Width);
  end;
  if FPrevAlign <> Align then PosChanged;
  {if (IniReadInteger(IniFile, Sect, sBtnWidth, FButtonSize.X) >
    FButtonSize.X) or (IniReadInteger(IniFile, Sect, sBtnHeight,
    FButtonSize.Y) > FButtonSize.Y) then Exit;}
  Count := IniReadInteger(IniFile, Sect, sCount, 0);
  if Count > 0 then
  begin
    ForEachItem(HideItem, 0);
    for I := 1 to Count do
    begin
      S := IniReadString(IniFile, Sect, sBtn + IntToStr(I), '');
      if S <> '' then
      begin
        Item := SearchItem(ExtractWord(1, S, Delims));
        if (Item <> nil) then
        begin
          Item.Left := Max(StrToIntDef(ExtractWord(2, S, Delims), Item.Left),
            FOffset.X);
          Item.Top := Max(StrToIntDef(ExtractWord(3, S, Delims), Item.Top),
            FOffset.Y);
          Item.Visible := True;
        end;
      end;
    end;
  end;
  Repaint;
end;

Procedure TSpeedBar.ReArrangeButtons(ByList:TStringList); // by JB.
{each lines of list have a form "<name>,<left-coord>,<top-coord>"}
Var
  I:Integer;
  S:String;
  Item: TSpeedItem;
Begin
  if ByList.Count > 0 then
  begin
    ForEachItem(HideItem, 0);
    for I := 0 to ByList.Count-1 do
    begin
      S := ByList.Strings[I];
      if S <> '' then
      begin
        Item := SearchItem(ExtractWord(1, S, [',']));
        if (Item <> nil) then
        begin
          Item.Left := Max(StrToIntDef(ExtractWord(2, S, [',']), Item.Left),
            FOffset.X);
          Item.Top := Max(StrToIntDef(ExtractWord(3, S, [',']), Item.Top),
            FOffset.Y);
          Item.Visible := True;
        end;
      end;
    end;
  end;
  Repaint;
End;

procedure TSpeedBar.SaveLayout(IniFile: TIniFile);
begin
  InternalSaveLayout(IniFile, GetDefaultSection(Self));
end;

procedure TSpeedBar.RestoreLayout(IniFile: TIniFile);
begin
  InternalRestoreLayout(IniFile, GetDefaultSection(Self));
end;

{$IFNDEF VER80}
procedure TSpeedBar.SaveLayoutReg(IniFile: TRegIniFile);
begin
  InternalSaveLayout(IniFile, GetDefaultSection(Self));
end;

procedure TSpeedBar.RestoreLayoutReg(IniFile: TRegIniFile);
begin
  InternalRestoreLayout(IniFile, GetDefaultSection(Self));
end;
{$ENDIF}

{ TBtnControl }

constructor TBtnControl.Create(AOwner: TComponent);
begin
  FImage := TButtonImage.Create;
  inherited Create(AOwner);
  Cursor := crDragHand;
  FSpacing := 1;
  FMargin := -1;
  FLayout := blGlyphTop;
{$IFNDEF VER80}
  FImageIndex := -1;
{$ENDIF}
end;

destructor TBtnControl.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TBtnControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_DISABLED;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
{$IFNDEF VER80}
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
{$ENDIF}
  end;
end;

procedure TBtnControl.AssignSpeedItem(Item: TSpeedItem);
begin
  Alignment := Item.FButton.Alignment;
  Glyph := Item.Glyph;
  NumGlyphs := Item.NumGlyphs;
  Spacing := Item.Spacing;
  Margin := Item.Margin;
  Layout := Item.Layout;
  Caption := Item.BtnCaption;
  WordWrap := Item.WordWrap;
{$IFNDEF VER80}
  ImageIndex := Item.ImageIndex;
  if Item.Speedbar <> nil then Images := Item.Speedbar.Images
  else Images := nil;
{$ENDIF}
  Font := Item.Font;
{$IFDEF RX_D4}
  BiDiMode := Item.FButton.BiDiMode;
{$ENDIF}
  SetBounds(0, 0, Item.Speedbar.BtnWidth, Item.Speedbar.BtnHeight);
end;

function TBtnControl.GetGlyph: TBitmap;
begin
  Result := FImage.Glyph;
end;

function TBtnControl.GetNumGlyphs: TRxNumGlyphs;
begin
  Result := FImage.NumGlyphs;
end;

function TBtnControl.GetCaption: TCaption;
begin
  Result := FImage.Caption;
end;

procedure TBtnControl.SetCaption(const Value: TCaption);
begin
  FImage.Caption := Value;
end;

procedure TBtnControl.SetNumGlyphs(Value: TRxNumGlyphs);
begin
  FImage.NumGlyphs := Value;
end;

procedure TBtnControl.SetGlyph(Value: TBitmap);
begin
  FImage.Glyph := Value;
end;

function TBtnControl.GetWordWrap: Boolean;
begin
  Result := FImage.WordWrap;
end;

procedure TBtnControl.SetWordWrap(Value: Boolean);
begin
  FImage.WordWrap := Value;
end;

function TBtnControl.GetAlignment: TAlignment;
begin
  Result := FImage.Alignment;
end;

procedure TBtnControl.SetAlignment(Value: TAlignment);
begin
  FImage.Alignment := Value;
end;

procedure TBtnControl.WMSize(var Message: TWMSize);
begin
  FImage.ButtonSize := Point(ClientWidth, ClientHeight);
end;

procedure TBtnControl.Paint;
begin
{$IFNDEF VER80}
  FImage.DrawEx(Canvas, 0, 0, Margin, Spacing, Layout, Font, Images,
    ImageIndex, {$IFDEF RX_D4} DrawTextBiDiModeFlags(Alignments[Alignment])
    {$ELSE} Alignments[Alignment] {$ENDIF});
{$ELSE}
  FImage.Draw(Canvas, 0, 0, Margin, Spacing, Layout, Font,
    Alignments[Alignment]);
{$ENDIF}
end;

procedure TBtnControl.Activate(Rect: TRect);
begin
  if IsRectEmpty(BoundsRect) then BoundsRect := Rect;
  SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, 0,
    0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
  SetCursor(Screen.Cursors[Cursor]);
end;

procedure TBtnControl.ReleaseHandle;
begin
  DestroyHandle;
end;

{ Utility routines }

function NewSpeedSection(ASpeedbar: TSpeedBar; const ACaption: string): Integer;
begin
  Result := ASpeedbar.AddSection(ACaption);
end;

function NewSpeedItem(AOwner: TComponent; ASpeedbar: TSpeedBar; Section: Integer;
  const AName: string): TSpeedItem;
begin
  Result := ASpeedBar.NewItem(AOwner, Section, AName);
end;

function FindSpeedBar(const Pos: TPoint): TSpeedBar;
var
  Window: TWinControl;
  Handle: HWnd;
begin
  Result := nil;
  Handle := WindowFromPoint(Pos);
  Window := nil;
  while (Handle <> 0) and (Window = nil) do
  begin
    Window := FindControl(Handle);
    if Window = nil then Handle := GetParent(Handle);
  end;
  if Window <> nil then
  begin
    if Window is TSpeedBar then Result := Window as TSpeedBar;
  end;
end;

procedure DrawCellButton(Grid: TDrawGrid; R: TRect; Item: TSpeedItem;
  Image: TButtonImage {$IFDEF RX_D4}; ARightToLeft: Boolean = False {$ENDIF});
var
  FBar: TSpeedBar;
  AFont: TFont;
{$IFNDEF VER80}
  ImageList: TImageList;
{$ENDIF}
begin
  if Item <> nil then
  begin
    FBar := Item.Speedbar;
    AFont := nil;
{$IFNDEF VER80}
    ImageList := nil;
    if FBar <> nil then
    begin
      AFont := FBar.Font;
      if Item.ImageIndex >= 0 then ImageList := FBar.Images;
    end;
    if ImageList = nil then Image.Glyph := Item.Glyph
    else Image.Glyph := nil;
{$ELSE}
    Image.Glyph := Item.Glyph;
    if FBar <> nil then AFont := FBar.Font;
{$ENDIF}
    with Image do
    begin
      Alignment := Item.FButton.Alignment;
      NumGlyphs := Item.NumGlyphs;
      Caption := Item.BtnCaption;
      WordWrap := Item.WordWrap;
      if FBar <> nil then
        ButtonSize := Point(FBar.BtnWidth, FBar.BtnHeight);
    end;
{$IFNDEF VER80}
    Image.DrawEx(Grid.Canvas, R.Left + 1, R.Top + 1, Item.Margin,
      Item.Spacing, Item.Layout, AFont, ImageList, Item.ImageIndex,
      {$IFDEF RX_D4} Item.FButton.DrawTextBiDiModeFlags(Alignments[Image.Alignment])
      {$ELSE} Alignments[Image.Alignment] {$ENDIF});
{$ELSE}
    Image.Draw(Grid.Canvas, R.Left + 1, R.Top + 1, Item.Margin,
      Item.Spacing, Item.Layout, AFont, Alignments[Image.Alignment]);
{$ENDIF}
    Inc(R.Left, Image.ButtonSize.X + 3);
    DrawCellText(Grid, 0, 0, Item.Caption, R, taLeftJustify, vaCenter
      {$IFDEF RX_D4}, ARightToLeft {$ENDIF});
  end;
end;

end.