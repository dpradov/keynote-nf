{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

// Delphi 2 and C++B 1 have incorrectly declared InsertItem as private.
{$IFDEF DFS_COMPILER_3_UP}
  {$DEFINE DFS_FIXED_LIST_VIEW}
{$ENDIF}

{.$DEFINE DFS_DEBUG}


{------------------------------------------------------------------------------}
{ TdfsEnhListView v3.70                                                        }
{------------------------------------------------------------------------------}
{ A list view control that provides enhanced functionality beyond the          }
{ standard list view.  For example, automatic sorting of simple data types,    }
{ owner draw event for vsReport mode, and more.  This does NOT require any     }
{ special version of COMCTL32.DLL.                                             }
{                                                                              }
{ Copyright 2000, Brad Stowers.  All Rights Reserved.                          }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TDFSColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ Support is provided via the DFS Support Forum, which is a web-based message  }
{ system.  You can find it at http://www.delphifreestuff.com/discus/           }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{ See ELV.txt for notes, known issues, and revision history.                   }
{------------------------------------------------------------------------------}
{ Date last modified:  April 30, 2000                                          }
{------------------------------------------------------------------------------}


unit EnhListView;

interface

{$IFNDEF DFS_WIN32}
  ERROR!  This unit only available for Delphi 2.0 or higher!!!
{$ENDIF}

uses
  Windows, Messages, Classes, Controls, ComCtrls, CommCtrl, SysUtils, Graphics,
  {$IFDEF DFS_COMPILER_4_UP} ImgList, {$ENDIF} StdCtrls, Menus;


const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsEnhListView v3.70';

  DRAWTEXTEX_FLAGS = DT_NOPREFIX or DT_SINGLELINE or DT_VCENTER or
     DT_END_ELLIPSIS;
  DRAWTEXTEX_ALIGNMENT: array[TAlignment] of UINT = (DT_LEFT, DT_RIGHT,
     DT_CENTER);
  WM_OWNERDRAWCOLUMNS = WM_USER + 143;

type
  TIntArray = array[0..(MaxInt div SizeOf(Integer)-1)] of Integer;
  PIntArray = ^TIntArray;

  TResizeMethod = (rmFitText, rmFitHeader);
  TAutoColumnSort = (acsNoSort, acsSort, acsSortToggle);
  TAutoSortStyle = (assSmart, assDefault);
  TSortAs = (saNone, saString, saNumeric, saDateTime);
  TLVStyle = (lvStandard, lvOwnerDrawFixed);
  TLVHDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; var ARect: TRect; Selected: boolean;
     var DefaultDrawing: boolean) of object;
  TLVMeasureItemEvent = procedure(Control: TWinControl;
     var AHeight: UINT) of object;
  TLVDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; ARect: TRect; State: TOwnerDrawState;
     var DefaultDrawing, FullRowSelect: boolean) of object;
  TLVDrawSubItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index, SubItem: Integer; ARect: TRect; State: TOwnerDrawState;
     var DefaultDrawing: boolean) of object;
  TLVAfterDrawItemEvent = procedure(Control: TWinControl; var ACanvas: TCanvas;
     Index: Integer; ARect: TRect; State: TOwnerDrawState) of object;
  TLVSortItemsEvent = procedure(Sender: TObject; Item1, Item2: TListItem;
     SortColumn: integer; var SortAs: TSortAs; var CompResult: integer) of object;
  TLVSortStatusEvent = procedure(Sender: TObject; SortColumn: integer;
     Ascending: boolean) of object;
  TLVEditCanceled = procedure(Sender: TObject; Item: TListItem) of object;
  {$IFNDEF DFS_COMPILER_4_UP}
  TLVNotifyEvent = procedure(Sender: TObject; Item: TListItem) of object;
  {$ENDIF}

  // Class for saved settings
  TdfsEnhLVSaveSettings = class(TPersistent)
  private
    FAutoSave: boolean;
    FRegistryKey: string;
    FSaveColumnSizes: boolean;
    FSaveCurrentSort: boolean;
    FSaveViewStyle: boolean;
  public
    constructor Create; virtual;
    procedure StoreColumnSizes(ColCount: integer;
       const IntArray: array of integer);
    procedure ReadColumnSizes(ColCount: integer;
       var IntArray: array of integer);
    procedure StoreCurrentSort(Ascending: boolean; SortCol: integer);
    procedure ReadCurrentSort(var Ascending: boolean; var SortCol: integer);
    procedure StoreViewStyle(Style: TViewStyle);
    function ReadViewStyle(Default: TViewStyle): TViewStyle;
  published
    property AutoSave: boolean read FAutoSave write FAutoSave default FALSE;
    property RegistryKey: string read FRegistryKey write FRegistryKey;
    property SaveColumnSizes: boolean
       read FSaveColumnSizes
       write FSaveColumnSizes
       default TRUE;
    property SaveCurrentSort: boolean
       read FSaveCurrentSort
       write FSaveCurrentSort
       default TRUE;
    property SaveViewStyle: boolean
       read FSaveViewStyle
       write FSaveViewStyle
       default TRUE;
  end;

  { The new class }
  TCustomEnhListView = class(TCustomListView)
  private
    FSortDirty: boolean;
    FUpdateCount: integer;
    FStyle: TLVStyle;
    FAutoColumnSort: TAutoColumnSort;
    FAutoSortStyle: TAutoSortStyle;
    FAutoResort: boolean;
    FAutoSortAscending: boolean;
    FTmpAutoSortAscending: boolean;
    FLastColumnClicked: Integer;
    FSaveSettings: TdfsEnhLVSaveSettings;
    FShowSortArrows: boolean;
    FReverseSortArrows: boolean;
    FSortUpBmp,
    FSortDownBmp: TBitmap;
    FCreatingWindowHandle: boolean;
{$IFDEF BACKGROUND_FIXED}
    FBackgroundImage: TBitmap;
{$ENDIF}
    FNoColumnResize: boolean;
    FOldHeaderWndProc: pointer;
    FHeaderInstance: pointer;
    FSearchStr: string;
    FSearchTickCount: Double;
    FColumnSearch: boolean;

    FOnSortBegin: TLVSortStatusEvent;
    FOnSortFinished: TLVSortStatusEvent;
    FOnMeasureItem: TLVMeasureItemEvent;
    FOnDrawItem: TLVDrawItemEvent;
    FOnDrawSubItem: TLVDrawSubItemEvent;
    FOnAfterDefaultDrawItem: TLVAfterDrawItemEvent;
    FOnDrawHeader: TLVHDrawItemEvent;
    FOnSortItems: TLVSortItemsEvent;
    FOnEditCanceled: TLVEditCanceled;
{$IFNDEF DFS_COMPILER_4_UP}
    FOnGetImageIndex: TLVNotifyEvent;
{$ENDIF}

    procedure HeaderWndProc(var Message: TMessage);
    { Message handlers }
    procedure CMSysColorChange(var Message: TWMSysColorChange);
       message CM_SYSCOLORCHANGE;
    procedure CMFontChanged(var Messsage: TMessage); message CM_FONTCHANGED;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMDrawHeader(var Message: TWMDrawItem); message WM_DRAWITEM;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure WMOwnerDrawColumns(var Message: TMessage);
       message WM_OWNERDRAWCOLUMNS;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
  protected
    { USE WITH CARE.  This can be NIL }
    FCanvas: TCanvas;
    FHeaderHandle: HWND;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$IFNDEF DFS_COMPILER_4_UP}
    function GetItem(Value: TLVItem): TListItem;
{$ENDIF}
    procedure ResetOwnerDrawHeight;
    procedure InvalidateColumnHeader(Index: integer); virtual;
    procedure DoSort(ColumnIndex:integer; Descending: boolean); virtual;
    procedure SortBegin(ColumnIndex: integer; Ascending: boolean); virtual;
    procedure SortFinished(ColumnIndex: integer; Ascending: boolean); virtual;
    procedure SortItems(const Item1, Item2: TListItem; SortColumn: integer;
       var CompResult: integer); virtual;
    procedure MeasureItem(var Height: UINT); virtual;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
       State: TOwnerDrawState; FullRowSelect: boolean); virtual;
    procedure DefaultDrawSubItem(Index, SubItem: integer; Rect: TRect;
       State: TOwnerDrawState); virtual;
    procedure ProcessDrawItemMsg(Index: Integer;
       Rect: TRect; State: TOwnerDrawState; var DefaultDrawing,
       FullRowSelect: boolean); virtual;
    function ActualColumnIndex(Index: integer): integer; virtual;
    function GetActualColumn(Index: integer): TListColumn; virtual;
    function GetSubItemText(Index, SubItem: integer): string; virtual;
    procedure DrawSubItem(Index, SubItem: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing: boolean); virtual;
    procedure DrawItem(var Canvas: TCanvas; Index: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing,
       FullRowSelect: boolean);
       {$IFDEF DFS_COMPILER_4_UP} reintroduce; overload; {$ENDIF} virtual;
    procedure AfterDrawItem(var Canvas: TCanvas; Index: Integer;
       Rect: TRect; State: TOwnerDrawState); virtual;
    procedure Edit(const Item: TLVItem); override;
    procedure EditCanceled(const Item: TLVItem); virtual;
    { Overriden ancestor methods }
    procedure ColClick(Column: TListColumn); override;
{$IFDEF DFS_FIXED_LIST_VIEW}
    procedure InsertItem(Item: TListItem); override;
{$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure ProcessDrawHeaderMsg(Index: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing: boolean); virtual;
    procedure DrawHeader(var Canvas: TCanvas; Index: Integer; var Rect: TRect;
       Selected: boolean; var DefaultDrawing: boolean); virtual;
    procedure DefaultDrawHeader(var Canvas: TCanvas; Index: Integer;
       var Rect: TRect; Selected: boolean); virtual;
    procedure SetOnDrawHeader(Value: TLVHDrawItemEvent); virtual;
    procedure SetColumnsOwnerDrawFlag(OwnerDrawn: boolean); virtual;
    procedure CreateSortBmps(var UpBmp, DownBmp: TBitmap); virtual;
{$IFNDEF DFS_COMPILER_4_UP}
    procedure GetImageIndex(Item: TListItem); virtual;
{$ENDIF}
{$IFDEF BACKGROUND_FIXED}
    procedure BackgroundImageChanged(Sender: TObject); virtual;
{$ENDIF}

    { Property methods }
    procedure SetAutoColumnSort(Value: TAutoColumnSort);
    procedure SetAutoSortStyle(Value: TAutoSortStyle);
    procedure SetCurrentSortAscending(Value: boolean);
    procedure SetAutoSortAscending(Value: boolean);
    procedure SetStyle(Value: TLVStyle);
    procedure SetShowSortArrows(Value: boolean);
    procedure SetReverseSortArrows(Value: boolean);
    procedure SetLastColumnClicked(Value: integer);
    procedure SetAutoResort(Value: boolean);
{$IFDEF BACKGROUND_FIXED}
    procedure SetBackgroundImage(const Value: TBitmap);
{$ENDIF}
    function GetSmallImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE}
       TImageList {$ENDIF};
    procedure SetSmallImages(Val: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList
       {$ELSE} TImageList {$ENDIF});
    function GetVersion: string; virtual;
    procedure SetVersion(const Val: string);
    function GetCurrentColumnWidth(Index: integer): integer;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;

    { Should probably remain protected }
    property SortUpBmp: TBitmap
      read FSortUpBmp;
    property SortDownBmp: TBitmap
      read FSortDownBmp;

    { Should be made public by descendants as needed }
    property LastColumnClicked: Integer
      read FLastColumnClicked
      write SetLastColumnClicked;

    { Should be published by descendants as needed }
    property HeaderHandle: HWnd
       read FHeaderHandle;
    property AutoColumnSort: TAutoColumnSort
       read FAutoColumnSort
       write SetAutoColumnSort
       default acsNoSort;
    property AutoSortStyle: TAutoSortStyle
       read FAutoSortStyle
       write SetAutoSortStyle
       default assSmart;
    property AutoResort: boolean
       read FAutoResort
       write SetAutoResort
       default TRUE;
    property AutoSortAscending: boolean
       read FAutoSortAscending
       write SetAutoSortAscending
       default TRUE;
    property ColumnSearch: boolean
       read FColumnSearch
       write FColumnSearch
       default FALSE;
    property ShowSortArrows: boolean
       read FShowSortArrows
       write SetShowSortArrows
       default FALSE;
    property ReverseSortArrows: boolean
       read FReverseSortArrows
       write SetReverseSortArrows
       default FALSE;
    property CurrentSortAscending: boolean
       read FTmpAutoSortAscending
       write SetCurrentSortAscending;
    property SaveSettings: TdfsEnhLVSaveSettings
       read FSaveSettings
       write FSaveSettings;
    property Style: TLVStyle
       read FStyle
       write SetStyle
       default lvStandard;
    property CurrentColumnWidth[Index: integer]: integer
       read GetCurrentColumnWidth;
{$IFDEF BACKGROUND_FIXED}
    property BackgroundImage: TBitmap
       read FBackgroundImage
       write SetBackgroundImage;
{$ENDIF}       
    property NoColumnResize: boolean
       read FNoColumnResize
       write FNoColumnResize;
    // We have to redeclare this so we can hook into the read/write methods.
    property SmallImages:
       {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF}
       read GetSmallImages
       write SetSmallImages;

    { Events }
    property OnDrawHeader: TLVHDrawItemEvent
       read FOnDrawHeader
       write SetOnDrawHeader;
    property OnMeasureItem: TLVMeasureItemEvent
       read FOnMeasureItem
       write FOnMeasureItem;
    property OnDrawItem: TLVDrawItemEvent
       read FOnDrawItem
       write FOnDrawItem;
    property OnDrawSubItem: TLVDrawSubItemEvent
       read FOnDrawSubItem
       write FOnDrawSubItem;
    property OnAfterDefaultDrawItem: TLVAfterDrawItemEvent
       read FOnAfterDefaultDrawItem
       write FOnAfterDefaultDrawItem;
    property OnSortItems: TLVSortItemsEvent
       read FOnSortItems
       write FOnSortItems;
    property OnSortBegin: TLVSortStatusEvent
       read FOnSortBegin
       write FOnSortBegin;
    property OnSortFinished: TLVSortStatusEvent
       read FOnSortFinished
       write FOnSortFinished;
    property OnEditCanceled: TLVEditCanceled
       read FOnEditCanceled
       write FOnEditCanceled;
{$IFNDEF DFS_COMPILER_4_UP}
    property OnGetImageIndex: TLVNotifyEvent
       read FOnGetImageIndex
       write FOnGetImageIndex;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function StoreSettings: boolean; virtual;
    function WriteSettings: boolean; virtual;
    function LoadSettings: boolean; virtual;
    function ReadSettings: boolean; virtual;
    procedure DefaultSort(ColumnIndex:integer; Descending: boolean); virtual;
    procedure Resort; virtual;
    // Use these as replacements for Items.BeginUpdate and EndUpdate.  They
    // call those methods, but they also inhibit autosorting until after the
    // last EndUpdate.
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    // Resize all columns.
    procedure ResizeColumns(ResizeMethod: TResizeMethod); virtual;

    // Move list item to new position.
    procedure MoveItem(OriginalIndex, NewIndex: Integer); virtual;

    function StringSelect(FindStr: string; ColumnIndex: Integer): boolean; virtual;
    function SubStringSelect(FindStr: string; ColumnIndex: Integer): boolean; virtual;

    // Accounts for re-ordered columns
    property ActualColumn[Index: integer]: TListColumn
       read GetActualColumn;
  published
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;
  end;


  TdfsEnhListView = class(TCustomEnhListView)
  public
    property HeaderHandle;
    property CurrentSortAscending;
    property LastColumnClicked;
    property CurrentColumnWidth;
  published
    property AutoColumnSort;
    property AutoSortStyle;
    property AutoResort;
    property AutoSortAscending;
{$IFDEF BACKGROUND_FIXED}
    property BackgroundImage;
{$ENDIF}
    property ColumnSearch;
    property NoColumnResize;
    property ReverseSortArrows;
    property ShowSortArrows;
    property SaveSettings;
    property Style;

    property OnMeasureItem;
    property OnDrawItem;
    property OnDrawSubItem;
    property OnAfterDefaultDrawItem;
    property OnDrawHeader;
    property OnSortItems;
    property OnSortBegin;
    property OnSortFinished;
    property OnEditCanceled;

    { Publish TCustomListView inherited protected properties }
    property Align;
{$IFDEF DFS_COMPILER_4_UP}
    property Anchors;
    property BiDiMode;
{$ENDIF}
    property BorderStyle;
{$IFDEF DFS_COMPILER_4_UP}
    property BorderWidth;
{$ENDIF}
    property Color;
    property ColumnClick;
    property OnClick;
    property OnDblClick;
    property Columns;
{$IFDEF DFS_COMPILER_4_UP}
    property Constraints;
{$ENDIF}
    property Ctl3D;
{$IFDEF DFS_COMPILER_4_UP}
    property DragKind;
{$ENDIF}
    property DragMode;
    property ReadOnly
       default False;
    property Enabled;
    property Font;
    property HideSelection;
    property IconOptions;
    property Items;
    property AllocBy;
    property MultiSelect;
    property OnChange;
    property OnChanging;
    property OnColumnClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
{$IFDEF DFS_COMPILER_4_UP}
    property OnEndDock;
{$ENDIF}
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnDragDrop;
    property OnDragOver;
    property DragCursor;
    property OnStartDrag;
    property OnEndDrag;
    property OnGetImageIndex;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
{$IFDEF DFS_COMPILER_4_UP}
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
{$ENDIF}
    property ParentColor
       default False;
    property ParentFont;
    property ParentShowHint;
{$IFDEF DFS_COMPILER_4_UP}
    property ParentBiDiMode;
{$ENDIF}
    property ShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop
       default True;
    property ViewStyle;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property LargeImages;
    property SmallImages;
    property StateImages;
  end;

var
  { Default drawing variables }
  DefDraw_TextOffset: integer; // Offset for the text -- 5
  DefDraw_ImageOffset: integer; // Offset for image -- 2


implementation

uses
  Registry, Forms, ExtListView;


var
  FDirection,
  FSortColNum: integer;

{$IFNDEF DFS_COMPILER_4_UP}
type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

function StringReplace(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := {$IFDEF DFS_COMPILER_2} Pos( {$ELSE} AnsiPos( {$ENDIF} Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;
{$ENDIF}

function IsValidNumber(S: string; var V: extended): boolean;
var
  NumCode: integer;
  FirstSpace: integer;
begin
  FirstSpace := Pos(' ', S);
  if FirstSpace > 0 then
    S := Copy(S, 1, FirstSpace - 1);
  Val(S, V, NumCode);
  Result := (NumCode = 0);
  if not Result then
  begin
    // Remove all thousands seperators
    S := StringReplace(S, ThousandSeparator, '', [rfReplaceAll]);
    // change DecimalSeperator to '.' because Val only recognizes that, not
    // the locale specific decimal char.  Stupid Val.
    S := StringReplace(S, DecimalSeparator, '.', [rfReplaceAll]);
    // and try again
    Val(S, V, NumCode);
    Result := (NumCode = 0);
  End;
end;

// date conversion will fail if using long format, e.g. '1 January 1994'
function IsValidDateTime(const S: string; var D: TDateTime): boolean;
var
  i: integer;
  HasDate: boolean;
  HasTime: boolean;
begin
  // Check for two date seperators.  This is because some regions use a "-"
  //  to seperate dates, so if we just checked for one we would flag negative
  //  numbers as being dates.
  i := Pos(DateSeparator, S);
  HasDate := i > 0;
  if HasDate and (i <> Length(S)) then
    HasDate := Pos(DateSeparator, Copy(S, i+1, Length(S)-i)) > 0;
  HasTime := Pos(TimeSeparator, S) > 0;
  Result := HasDate or HasTime;
  if Result then
  begin
    try
      if HasDate and HasTime then
        D := StrToDateTime(S)
      else if HasDate then
        D := StrToDate(S)
      else if HasTime then
        D := StrToTime(S);
    except
      // Something failed to convert...
      D := 0;
      Result := FALSE;
    end;
  end;
end; { IsValidDateTime }

function __CustomSortProc1__(Item1, Item2: TListItem; Data: integer): integer;
   stdcall;
var
  Str1, Str2: string;
  Val1, Val2: extended;
  Date1, Date2: TDateTime;
  Diff: TDateTime;
begin
  if (Item1 = NIL) or (Item2 = NIL) then
  begin
    // something bad happening, I'm outta here
    Result := 0;
    exit;
  end;

  try
    if FSortColNum = -1 then
    begin
      Str1 := Item1.Caption;
      Str2 := Item2.Caption;
    end else begin
      if FSortColNum < Item1.SubItems.Count then
        Str1 := Item1.SubItems[FSortColNum]
      else
        Str1 := '';
      if FSortColNum < Item2.SubItems.Count then
        Str2 := Item2.SubItems[FSortColNum]
      else
        Str2 := '';
    end;

    if TCustomEnhListView(Data).AutoSortStyle = assSmart then
    begin
      if IsValidDateTime(Str1, Date1) and IsValidDateTime(Str2, Date2) then
      begin
        Diff := Date1 - Date2;
        if Diff < 0.0 then Result := -1
        else if Diff > 0.0 then Result := 1
        else Result := 0
      end else if IsValidNumber(Str1, Val1) and IsValidNumber(Str2, Val2) then
      begin
        if Val1 < Val2 then Result := -1
        else if Val1 > Val2 then Result := 1
        else Result := 0
      end else
        Result := AnsiCompareStr(Str1, Str2);
    end else
      Result := AnsiCompareStr(Str1, Str2);

    Result := FDirection * Result; // Set direction flag.
  except
    Result := 0;  // Something went bad in the comparison.  Say they are equal.
  end;
end;

function __CustomSortProc2__(Item1, Item2: TListItem; Data: integer): integer;
   stdcall;
var
  EvRes: integer;
begin
  EvRes := 0;
  TCustomEnhListView(Data).SortItems(Item1, Item2, FSortColNum, EvRes);
  Result := EvRes * FDirection;
end;



{ TdfsEnhLVSaveSettings }

constructor TdfsEnhLVSaveSettings.Create;
begin
  inherited Create;

  FAutoSave := FALSE;
  FRegistryKey := '';
  FSaveViewStyle := TRUE;
  FSaveColumnSizes := TRUE;
  SaveCurrentSort := TRUE;
end;

procedure TdfsEnhLVSaveSettings.StoreColumnSizes(ColCount: integer;
   const IntArray: array of integer);
var
  Reg: TRegIniFile;
  x: integer;
  s: string;
begin
  if ColCount < 1 then exit;
  s := '';
  for x := 0 to ColCount-1 do
    s := s + IntToStr(IntArray[x]) + ',';
  SetLength(s, Length(s)-1);
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    Reg.WriteString('Columns', 'Sizes', s);
  finally
    Reg.Free;
  end;
end;

procedure TdfsEnhLVSaveSettings.ReadColumnSizes(ColCount: integer;
   var IntArray: array of integer);
var
  Reg: TRegIniFile;
  x,y: integer;
  s: string;
begin
  if ColCount < 1 then exit;
  s := '';
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    s := Reg.ReadString('Columns', 'Sizes', '');
  finally
    Reg.Free;
  end;
  if s = '' then
  begin
    IntArray[0] := -1;
    exit;
  end;
  y := 0;
  for x := 0 to ColCount-1 do
  begin
    try
      y := Pos(',', s);
      if y = 0 then
        y := Length(s)+1;
      IntArray[x] := StrToInt(Copy(s, 1, y-1));
    except
      { Nothing, just eat the exception };
    end;
    s := copy(s, y+1, length(s));
    if s = '' then break;
  end;
end;

procedure TdfsEnhLVSaveSettings.StoreCurrentSort(Ascending: boolean;
   SortCol: integer);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    Reg.WriteBool('Sort', 'Ascending', Ascending);
    Reg.WriteInteger('Sort', 'SortCol', SortCol);
  finally
    Reg.Free;
  end;
end;

procedure TdfsEnhLVSaveSettings.ReadCurrentSort(var Ascending: boolean;
   var SortCol: integer);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    Ascending := Reg.ReadBool('Sort', 'Ascending', TRUE);
    SortCol := Reg.ReadInteger('Sort', 'SortCol', 0);
  finally
    Reg.Free;
  end;
end;

procedure TdfsEnhLVSaveSettings.StoreViewStyle(Style: TViewStyle);
const
  STYLE_VAL: array[TViewStyle] of integer = (0, 1, 2, 3);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    Reg.WriteInteger('ViewStyle', 'ViewStyle', STYLE_VAL[Style]);
  finally
    Reg.Free;
  end;
end;

function TdfsEnhLVSaveSettings.ReadViewStyle(Default: TViewStyle): TViewStyle;
const
  STYLES: array[0..3] of TViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport);
var
  Reg: TRegIniFile;
  i: integer;
begin
  Reg := TRegIniFile.Create(FRegistryKey);
  try
    i := Reg.ReadInteger('ViewStyle', 'ViewStyle', -1);
    if (i >= Low(STYLES)) and (i <= High(STYLES)) then
      Result := STYLES[i]
    else
      Result := Default;
  finally
    Reg.Free;
  end;
end;


// Override constructor to "zero out" our internal variable.
constructor TCustomEnhListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSearchStr := '';
  FSearchTickCount := 0;
  FHeaderHandle := 0;
  FSortDirty := FALSE;
  FUpdateCount := 1; // inhibit sorting until finished creating.
  FSaveSettings := TdfsEnhLVSaveSettings.Create;
  FAutoColumnSort := acsNoSort;
  FAutoResort := TRUE;
  FAutoSortStyle := assSmart;
  FAutoSortAscending := TRUE;
  FTmpAutoSortAscending := FAutoSortAscending;
  FLastColumnClicked := -1;
  FCanvas := NIL;
  FStyle  := lvStandard;
  FSortUpBmp := NIL;
  FSortDownBmp := NIL;
  FShowSortArrows := FALSE;
  FReverseSortArrows := FALSE;
{$IFDEF BACKGROUND_FIXED}
  FBackgroundImage := TBitmap.Create;
{$ENDIF}
  FHeaderInstance := MakeObjectInstance(HeaderWndProc);
end;

destructor TCustomEnhListView.Destroy;
begin
{$IFDEF BACKGROUND_FIXED}
  FBackgroundImage.Free;
{$ENDIF}
  FSortUpBmp.Free;
  FSortDownBmp.Free;
  FCanvas.Free;
  if FHeaderHandle <> 0 then
    SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FOldHeaderWndProc));
  FreeObjectInstance(FHeaderInstance);

  inherited Destroy;

  FSaveSettings.Free;
end;

procedure TCustomEnhListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if (FStyle = lvOwnerDrawFixed) then
  begin
    Params.Style := Params.Style or LVS_OWNERDRAWFIXED;
    if FCanvas = NIL then
      FCanvas := TCanvas.Create;
  end else begin
    if (not assigned(FOnDrawHeader)) and (not FShowSortArrows) then
    begin
      FCanvas.Free;
      FCanvas := NIL;
    end;
  end;
end;

procedure TCustomEnhListView.CreateWnd;
begin
//  if FCreatingWindowHandle then exit;

  FCreatingWindowHandle := TRUE;
  try
    inherited CreateWnd;
    // If we are loading object from stream (form file), we have to wait until
    // everything is loaded before populating the list.  If we are not loading,
    // i.e. the component was created dynamically or was just dropped on a form,
    // we need to reset the flag now.
    if not (csLoading in ComponentState) then
      FUpdateCount := 0;

    // Something very bizarre happens in either TCustomListView or in the
    // list view code itself in COMCTL32.DLL:  The first WM_MEASUREITEM value
    // is not honored if the listview has small images assigned to it.  Instead
    // the value is ignored and the height of the images are used.  I found that
    // by forcing Windows to ask for the item height a second time, it would
    // honor the value then.
    if Style = lvOwnerDrawFixed then
      ResetOwnerDrawHeight;
  finally
    FCreatingWindowHandle := FALSE;
  end;
end;

procedure TCustomEnhListView.Loaded;
begin
  inherited Loaded;

{$IFDEF BACKGROUND_FIXED}
  BackgroundImageChanged(Self);
{$ENDIF}

  if not FCreatingWindowHandle then
    HandleNeeded;

  FUpdateCount := 0;

  if (not LoadSettings) or (not SaveSettings.SaveCurrentSort) then
  begin
    if Columns.Count > 0 then
      FLastColumnClicked := 0;
    Resort;
  end;

  // Something flaky going on.  Hard to explain, but this clears it up.
  PostMessage(Handle, WM_OWNERDRAWCOLUMNS, 0, 0);
end;

procedure TCustomEnhListView.WMDestroy(var Message: TWMDestroy);
begin
  StoreSettings;

  inherited;
end;


function TCustomEnhListView.StoreSettings: boolean;
begin
  if FSaveSettings.AutoSave and
     (([csDesigning, csLoading, csReading] * ComponentState) = []) then
    Result := WriteSettings
  else
    Result := FALSE;
end;

function TCustomEnhListView.WriteSettings: boolean;
var
  ColCount: integer;
  ColArray: PIntArray;
  x: integer;
begin
  Result := TRUE;
  ColCount := Columns.Count;
  if ColCount > 0 then
  begin
    GetMem(ColArray, SizeOf(Integer)*ColCount);
    try
      if FSaveSettings.SaveColumnSizes then
      begin
        for x := 0 to ColCount-1 do
          ColArray[x] := ActualColumn[x].Width;
        FSaveSettings.StoreColumnSizes(ColCount, ColArray^);
      end;
      if FSaveSettings.SaveCurrentSort then
        FSaveSettings.StoreCurrentSort(CurrentSortAscending, LastColumnClicked);
      if FSaveSettings.SaveViewStyle then
        FSaveSettings.StoreViewStyle(ViewStyle);
    finally
      FreeMem(ColArray);
    end;
  end;
end;

function TCustomEnhListView.LoadSettings: boolean;
begin
  if FSaveSettings.AutoSave and (not(csDesigning in ComponentState)) then
    Result := ReadSettings
  else
    Result := FALSE;
end;

function TCustomEnhListView.ReadSettings: boolean;
var
  ColCount: integer;
  ColArray: PIntArray;
  x: integer;
  SortCol: integer;
  SortAscending: boolean;
begin
  Result := TRUE;
  ColCount := Columns.Count;
  if ColCount > 0 then
  begin
    GetMem(ColArray, SizeOf(Integer)*ColCount);
    try
      if FSaveSettings.SaveColumnSizes then
      begin
        for x := 0 to ColCount-1 do
          ColArray[x] := ActualColumn[x].Width;
        FSaveSettings.ReadColumnSizes(ColCount, ColArray^);
        if ColArray[0] <> -1 then
          for x := 0 to ColCount-1 do
            ActualColumn[x].Width := ColArray[x];
      end;
    finally
      FreeMem(ColArray);
    end;
  end;

  if FSaveSettings.SaveCurrentSort then
  begin
    FSaveSettings.ReadCurrentSort(SortAscending, SortCol);
    if SortCol >= Columns.Count then
      SortCol := Columns.Count-1;
    if SortCol < 0 then
      SortCol := 0;
    BeginUpdate;
    try
      CurrentSortAscending := SortAscending;
      LastColumnClicked := SortCol;
      Resort;
    finally
      EndUpdate;
    end;
  end;

  if FSaveSettings.SaveViewStyle then
    ViewStyle := FSaveSettings.ReadViewStyle(ViewStyle);
end;

procedure TCustomEnhListView.DoSort(ColumnIndex:integer; Descending: boolean);
begin
  FSortDirty := FALSE;
  LastColumnClicked := ColumnIndex;
  SortBegin(ColumnIndex, not Descending);
  if Descending then
    FDirection := 1
  else
    FDirection := -1;
  FSortColNum := ColumnIndex - 1;
  if assigned(FOnSortItems) then
    CustomSort(@__CustomSortProc2__, integer(Self))
  else
    CustomSort(@__CustomSortProc1__, integer(Self));
  SortFinished(ColumnIndex, not Descending);
end;

procedure TCustomEnhListView.DefaultSort(ColumnIndex: integer;
   Descending: boolean);
begin
  // Check if the sort order should be toggled
  if FAutoColumnSort = acsSortToggle then
    if LastColumnClicked = ColumnIndex then
      FTmpAutoSortAscending := not Descending
    else
      FTmpAutoSortAscending := Descending;

  InvalidateColumnHeader(ColumnIndex);
  DoSort(ColumnIndex, Descending);
end;

procedure TCustomEnhListView.SortItems(const Item1, Item2: TListItem;
   SortColumn: integer; var CompResult: integer);
var
  SortAs: TSortAs;
  Str1, Str2: string;
  F1, F2: extended;
  Date1, Date2, Diff: TDateTime;
begin
  // The only way to get in here is if FOnSortItems is assigned, so don't bother
  //  checking for NIL
  SortAs := saNone;
  FonSortItems(Self, Item1, Item2, SortColumn, SortAs, CompResult);
  // Do they want us to sort it?
  if SortAs <> saNone then
  begin
    if SortColumn = -1 then
    begin
      Str1 := Item1.Caption;
      Str2 := Item2.Caption;
    end else begin
      if SortColumn < Item1.SubItems.Count then
        Str1 := Item1.SubItems[SortColumn]
      else
        Str1 := '';
      if SortColumn < Item2.SubItems.Count then
        Str2 := Item2.SubItems[SortColumn]
      else
        Str2 := '';
    end;

    case SortAs of
      saString: CompResult := AnsiCompareStr(Str1, Str2);
      saNumeric:
        begin
          if not IsValidNumber(Str1, F1) then
            F1 := 0;
          if not IsValidNumber(Str2, F2) then
            F2 := 0;
          if F1 < F2 then CompResult := -1
          else if F1 > F2 then CompResult := 1
          else CompResult := 0;
        end;
      saDateTime:
        begin
          if not IsValidDateTime(Str1, Date1) then
            Date1 := 0;
          if not IsValidDateTime(Str2, Date2) then
            Date1 := 0;
          Diff := Date1 - Date2;
          if Diff < 0.0 then CompResult := -1
          else if Diff > 0.0 then CompResult := 1
          else CompResult := 0
        end;
    end;
  end;
end;

procedure TCustomEnhListView.SortBegin(ColumnIndex: integer;
   Ascending: boolean);
begin
  if assigned(FOnSortBegin) then
    FOnSortBegin(Self, ColumnIndex, Ascending);
end;

procedure TCustomEnhListView.SortFinished(ColumnIndex: integer;
   Ascending: boolean);
begin
  if assigned(FOnSortFinished) then
    FOnSortFinished(Self, ColumnIndex, Ascending);
end;

procedure TCustomEnhListView.ColClick(Column: TListColumn);
begin
  // Check if the sort order should be toggled
  if FAutoColumnSort = acsSortToggle then
    if LastColumnClicked = Column.Index then
      FTmpAutoSortAscending := not FTmpAutoSortAscending
    else
      FTmpAutoSortAscending := FAutoSortAscending;

  inherited ColClick(Column);

  if (FAutoColumnSort <> acsNoSort) and (Column.Index < Columns.Count) then
    DoSort(Column.Index, FTmpAutoSortAscending);

  LastColumnClicked := Column.Index;
end;

{$IFDEF DFS_FIXED_LIST_VIEW}
procedure TCustomEnhListView.InsertItem(Item: TListItem);
begin
  inherited InsertItem(Item);
  if FAutoResort then
    Resort;
end;
{$ENDIF}


procedure TCustomEnhListView.Edit(const Item: TLVItem);
begin
  inherited Edit(Item);
  if FAutoResort then
    Resort;
end;

type
  THackListItems = class(TListItems)
  end;

procedure TCustomEnhListView.EditCanceled(const Item: TLVItem);
begin
  if assigned(FOnEditCanceled) then
    with Item do
      FOnEditCanceled(Self, THackListItems(Items).GetItem(iItem));
end;

{$IFNDEF DFS_COMPILER_4_UP}
function TCustomEnhListView.GetItem(Value: TLVItem): TListItem;
begin
  with Value do
    if (mask and LVIF_PARAM) <> 0 then Result := TListItem(lParam)
    else Result := Items[IItem];
end;
{$ENDIF}


{$IFNDEF DFS_COMPILER_4_UP}
type
  THackdfsExtListView = class(TdfsExtListView);
{$ENDIF}
procedure TCustomEnhListView.CNNotify(var Message: TWMNotify);
{$IFNDEF DFS_COMPILER_4_UP}
var
  Item: TListItem;
{$ENDIF}
begin
  inherited;

  with Message.NMHdr^ do
    case code of
{$IFNDEF DFS_FIXED_LIST_VIEW}
      LVN_INSERTITEM:
        if FAutoResort then
          Resort;
{$ENDIF}
      LVN_ENDLABELEDIT:
        with PLVDispInfo(Pointer(Message.NMHdr))^ do
          if (item.pszText = NIL) and (item.IItem <> -1) then
            EditCanceled(item);
{$IFNDEF DFS_COMPILER_4_UP}
      LVN_GETDISPINFO:
        begin
          Item := GetItem(PLVDispInfo(Message.NMHdr)^.item);
          if Item <> NIL then
            with PLVDispInfo(Message.NMHdr)^.item do
            begin
              if (mask and LVIF_IMAGE) <> 0 then
              begin
                if iSubItem = 0 then
                begin
                  GetImageIndex(Item);
                  iImage := Item.ImageIndex;
                  if Assigned(StateImages) then
                  begin
                    state := IndexToStateImageMask(Item.StateIndex + 1);
                    stateMask := $F000;
                    mask := mask or LVIF_STATE;
                  end;
                end;
              end;
            end;
        end;
{$ENDIF}
    end;
end;

procedure TCustomEnhListView.SetAutoColumnSort(Value: TAutoColumnSort);
begin
  if FAutoColumnSort <> Value then
  begin
    FAutoColumnSort := Value;
    if FAutoColumnSort <> acsNoSort then
      Resort;
  end;
end;

procedure TCustomEnhListView.SetAutoSortStyle(Value: TAutoSortStyle);
begin
  if FAutoSortStyle <> Value then
  begin
    FAutoSortStyle := Value;
    Resort;
  end;
end;

procedure TCustomEnhListView.SetAutoResort(Value: boolean);
begin
  if FAutoResort <> Value then
    FAutoResort := Value;
end;

procedure TCustomEnhListView.SetCurrentSortAscending(Value: boolean);
begin
  if FTmpAutoSortAscending <> Value then
  begin
    FTmpAutoSortAscending := Value;
    InvalidateColumnHeader(FLastColumnClicked);
  end;
end;

procedure TCustomEnhListView.SetAutoSortAscending(Value: Boolean);
begin
  if FAutoSortAscending <> Value then
  begin
    FAutoSortAscending := Value;
    FTmpAutoSortAscending := Value;
  end;
end;

procedure TCustomEnhListView.Resort;
begin
  FSortDirty := TRUE;
  if ((FAutoColumnSort <> acsNoSort) and (LastColumnClicked >= 0) and
     (LastColumnClicked < Columns.Count)) or (assigned(FOnSortItems)) then
  begin
    if FUpdateCount < 1 then
      DoSort(LastColumnClicked, FTmpAutoSortAscending);
  end;
end;

procedure TCustomEnhListView.BeginUpdate;
begin
  Items.BeginUpdate;
  inc(FUpdateCount);
end;


procedure TCustomEnhListView.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0; // In case someone gets overly happy with EndUpdate calls
  if FUpdateCount = 0 then
  begin
    // Need to resort?
    if FSortDirty then
      Resort;
  end;

  // Call this last so resort happens before screen redraw is re-enabled.
  Items.EndUpdate;
end;


procedure TCustomEnhListView.DrawItem(var Canvas: TCanvas; Index: Integer;
   Rect: TRect; State: TOwnerDrawState; var DefaultDrawing,
   FullRowSelect: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawItem);
  if assigned(FOnDrawItem) then
    FOnDrawItem(Self, Canvas, Index, Rect, State, DefaultDrawing,FullRowSelect);
end;

procedure TCustomEnhListView.AfterDrawItem(var Canvas: TCanvas; Index: Integer;
   Rect: TRect; State: TOwnerDrawState);
begin
  if assigned(FOnAfterDefaultDrawItem) then
    FOnAfterDefaultDrawItem(Self, Canvas, Index, Rect, State);
end;

procedure TCustomEnhListView.CMSysColorChange(var Message: TWMSysColorChange);
begin
  // Need to recreate the sort arrow bmps to use the new system colors
  if ShowSortArrows then
    CreateSortBmps(FSortUpBmp, FSortDownBmp);
  inherited;
end;

procedure TCustomEnhListView.CMFontChanged(var Messsage: TMessage);
begin
  if HandleAllocated and (Style = lvOwnerDrawFixed) then
    RecreateWnd
  else
    inherited;
end;

procedure TCustomEnhListView.CNMeasureItem(var Message: TWMMeasureItem);
var
  DC: HDC;
  OldFont: HFONT;
  Size: TSize;
begin
  inherited;

  DC := CreateCompatibleDC(0);
  OldFont := SelectObject(DC, Font.Handle);
  try
    GetTextExtentPoint32(DC, 'Wy', 2, Size);
    // Owner drawing only happens in vsReport mode, so no need to check anything
    // besides that.
    // I'm checking SmallImages.Height here, but I don't think it'll do any
    // good.  From what I can tell, if you have SmallImages assigned, this
    // handler will get called but the value you give it is ignored and the
    // list uses it's normal item height.  Strange....
    if assigned(SmallImages) and (SmallImages.Height > Size.cy) then
      Message.MeasureItemStruct.itemHeight := SmallImages.Height
    else
      Message.MeasureItemStruct.itemHeight := Size.cy + 1;
  finally
    SelectObject(DC, OldFont);
    DeleteDC(DC);
  end;
  MeasureItem(Message.MeasureItemStruct.itemHeight);
  Message.Result := 1;
end;

procedure TCustomEnhListView.MeasureItem(var Height: UINT);
begin
  if assigned(FOnMeasureItem) then
    FOnMeasureItem(Self, Height);
end;


procedure TCustomEnhListView.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  DoDefaultDrawing: boolean;
  FullRowSelect: boolean;
  SavedDC: integer;
begin { CNDrawItem }
  if FCanvas = NIL then exit;

  with Message.DrawItemStruct^ do
  begin
    {$IFDEF DFS_COMPILER_5_UP}
    State := TOwnerDrawState(LongRec(itemState).Lo);
    {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ENDIF}
    SavedDC := SaveDC(hDC);
    FCanvas.Handle := hDC;
    try
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      DoDefaultDrawing := FALSE;
      FullRowSelect := FALSE;
      ProcessDrawItemMsg(itemID, rcItem, State, DoDefaultDrawing, FullRowSelect);
    finally
      FCanvas.Handle := 0;
      RestoreDC(hDC, SavedDC);
    end;
  end;

  Message.Result := 1;
end;

function TCustomEnhListView.GetActualColumn(Index: integer): TListColumn;
begin
  // Delphi 2 and C++B 1 have a bug in TListColumn.GetWidth.  It returns zero
  // for the width if the handle hasn't been allocated yet instead of returning
  // the value of the internal storage variable like Delphi 3 does.  I've also
  // had some problems similar under Delphi 3, so I'm just always requiring the
  // handle to be valid.
  HandleNeeded;

  if Index >= Columns.Count then
    Result := NIL
  else
    Result := Columns[Index];
end;

function TCustomEnhListView.GetSubItemText(Index, SubItem: integer): string;
begin
  if SubItem < 0 then
    Result := Items[Index].Caption
  else
    Result := Items[Index].SubItems[SubItem];
end;

// SubItem is -1 for Caption item
procedure TCustomEnhListView.DrawSubItem(Index, SubItem: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawSubItem);
  if assigned(FOnDrawSubItem) then
    FOnDrawSubItem(Self, FCanvas, Index, SubItem, Rect, State, DefaultDrawing);
end;

procedure TCustomEnhListView.DefaultDrawSubItem(Index, SubItem: Integer;
   Rect: TRect; State: TOwnerDrawState);
var
  DoDefaultDrawing: boolean;
  SavedDC: integer;
begin
  DoDefaultDrawing := csDesigning in ComponentState;
  SavedDC := SaveDC(FCanvas.Handle);
  try
    if not (csDesigning in ComponentState) then
      DrawSubItem(Index, SubItem, Rect, State, DoDefaultDrawing);

    if DoDefaultDrawing then
    begin
      if SubItem >= 0 then
        InflateRect(Rect, -4, 0);
      if ActualColumn[SubItem+1].Alignment = taLeftJustify then
        Inc(Rect.Left, DefDraw_TextOffset);
      DrawTextEx(FCanvas.Handle, PChar(GetSubItemText(Index, SubItem)), -1, Rect,
         DRAWTEXTEX_FLAGS or
         DRAWTEXTEX_ALIGNMENT[ActualColumn[SubItem+1].Alignment], NIL);
    end;
  finally
    RestoreDC(FCanvas.Handle, SavedDC);
  end;
end;

{$IFDEF DFS_COMPILER_4_UP}
type
  THackImageList = class(TCustomImageList);
{$ENDIF}

procedure TCustomEnhListView.DefaultDrawItem(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; FullRowSelect: boolean);
{$IFDEF DFS_COMPILER_4_UP}
const
  DrawingStyles: array[TDrawingStyle] of Longint = (ILD_FOCUS, ILD_SELECTED,
    ILD_NORMAL, ILD_TRANSPARENT);
  Images: array[TImageType] of Longint = (0, ILD_MASK);
{$ENDIF}
var
{$IFDEF DFS_COMPILER_4_UP}
  DS: TDrawingStyle;
  x: integer;
{$ELSE}
  OldStyle: TDrawingStyle;
{$ENDIF}
  OldBlend: TColor;
  Count: Integer;
  SubRect: TRect;
  ImgTop: integer;
begin
  if Items[Index] = NIL then
    // something bad happening, I'm outta here
    exit;

  if Columns.Count > 0 then
  begin
    if (odSelected in State) then
    begin
      if Focused then
      begin
        FCanvas.Brush.Color := clHighlight;
        FCanvas.Font.Color := clHighlightText;
      end else begin
        if not HideSelection then
        begin
          FCanvas.Brush.Color := clBtnFace;
          FCanvas.Font.Color := clBtnText;
        end;
      end;
    end;
    SubRect := Rect;
    SubRect.Right := Rect.Left + CurrentColumnWidth[0]{ - 2};

    if assigned(StateImages) then
    begin
      StateImages.Draw(FCanvas, SubRect.Left + DefDraw_ImageOffSet,
         SubRect.Top + (SubRect.Bottom - SubRect.Top - StateImages.Height) div 2,
         Items[Index].StateIndex);
      Inc(SubRect.Left, StateImages.Width);
    end;

    if assigned(SmallImages) then
    begin
      OldBlend := SmallImages.BlendColor;
      SmallImages.BlendColor := clHighlight;
      ImgTop := SubRect.Top + (SubRect.Bottom - SubRect.Top -
        SmallImages.Height) div 2;
      {$IFDEF DFS_COMPILER_4_UP}
      { Changing DrawStyle causes an invalidate, which is very nasty since we
        are in the process of repainting here.  Continuous flickering.... }
      if Focused and ((odSelected in State) or Items[Index].Focused) then
        DS := dsSelected
      else
        DS := dsTransparent;
      // Draw OverlayImage
      if (Items[Index].OverlayIndex >= 0) and
         (Items[Index].OverlayIndex <= 3) then // vadid overlay index?
      begin
        x := IndexToOverlayMask(Items[Index].OverlayIndex+1);
        THackImageList(SmallImages).DoDraw(Items[Index].ImageIndex, FCanvas,
           SubRect.Left + DefDraw_ImageOffSet, ImgTop, DrawingStyles[DS] or
           Images[SmallImages.ImageType] or ILD_OVERLAYMASK and x, Enabled);
      end else
        THackImageList(SmallImages).DoDraw(Items[Index].ImageIndex, FCanvas,
           SubRect.Left + DefDraw_ImageOffSet, ImgTop,
           DrawingStyles[DS] or Images[SmallImages.ImageType], Enabled);


      {$ELSE}
      OldStyle := SmallImages.DrawingStyle;
      if Focused and ((odSelected in State) or Items[Index].Focused) then
        SmallImages.DrawingStyle := dsSelected
      else
        SmallImages.DrawingStyle := dsTransparent;

      SmallImages.Draw(FCanvas, SubRect.Left + DefDraw_ImageOffSet, ImgTop,
         Items[Index].ImageIndex);

      // Draw OverlayImage
      if (Items[Index].OverlayIndex >= 0) and
         (Items[Index].OverlayIndex <= 3) then // vadid overlay index?
        SmallImages.DrawOverlay(FCanvas, SubRect.Left + DefDraw_ImageOffSet,
           ImgTop, Items[Index].ImageIndex, Items[Index].OverlayIndex);

      SmallImages.DrawingStyle := OldStyle;
      {$ENDIF}

      SmallImages.BlendColor := OldBlend;
      if ActualColumn[0].Alignment = taLeftJustify then
        Inc(SubRect.Left, {DefDraw_TextOffset + }SmallImages.Width);
{    end else begin
      if ActualColumn[0].Alignment = taLeftJustify then
        Inc(SubRect.Left, DefDraw_TextOffset);}
    end;

    DefaultDrawSubItem(Index, -1, SubRect, State);

    // Already done column 0, start at 1.
    for Count := 1 to Columns.Count-1 do
    begin
      { Restore this through each iteration since they may screw with it in
        the OnDrawSubItem event. }
      if not FullRowSelect then
      begin
        FCanvas.Brush.Color := clWindow;
        FCanvas.Font.Color := clWindowText;
      end;

      if Count > Items[Index].SubItems.Count then
        continue; // Hidden item
      if ActualColumn[Count].Alignment = taLeftJustify then
      begin
        SubRect.Left := SubRect.Right;
        SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
//        Inc(SubRect.Left, DefDraw_TextOffset)
      end else begin
        SubRect.Left := SubRect.Right;// + DefDraw_TextOffset;
        SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
//        Dec(SubRect.Right, DefDraw_TextOffset);
      end;
      DefaultDrawSubItem(Index, Count-1, SubRect, State);
    end;
  end;
end;


procedure TCustomEnhListView.ProcessDrawItemMsg(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing, FullRowSelect: boolean);
var
  SubRect: TRect;
begin
  DefaultDrawing := csDesigning in ComponentState;
  if not (csDesigning in ComponentState) then
    DrawItem(FCanvas, Index, Rect, State, DefaultDrawing, FullRowSelect);

  if DefaultDrawing then
  begin
    FCanvas.FillRect(Rect);

    if (Index >= 0) then
    begin
      if (odSelected in State) then
      begin
        if (not HideSelection) or Focused then
        begin
          if Focused then
            FCanvas.Brush.Color := clHighlight
          else
            FCanvas.Brush.Color := clBtnFace;

          SubRect := Rect;
//          Inc(SubRect.Left, DefDraw_TextOffset - 2);
//          Dec(SubRect.Left, 2);
          if (not FullRowSelect) then
          begin
            if assigned(Items[Index]) then
              SubRect.Right := SubRect.Left +
                 FCanvas.TextWidth(Items[Index].Caption) + 8;
            if assigned(StateImages) then
              OffsetRect(SubRect, StateImages.Width, 0);
            if assigned(SmallImages) then
              OffsetRect(SubRect, SmallImages.Width, 0);
            // Don't let it go past first column width
            if (Columns.Count > 0) and
               (CurrentColumnWidth[0] < SubRect.Right) then
              SubRect.Right := CurrentColumnWidth[0];
          end else begin
            if assigned(StateImages) then
              Inc(SubRect.Left, StateImages.Width);
            if assigned(SmallImages) then
              Inc(SubRect.Left, SmallImages.Width);
          end;
          FCanvas.FillRect(SubRect);
        end;
      end;
      DefaultDrawItem(Index, Rect, State, FullRowSelect);
      if (odFocused in State) and Focused then
      begin
        SubRect := Rect;
//        Inc(SubRect.Left, DefDraw_TextOffset - 2);
//        Dec(SubRect.Left, 2);
        if (not FullRowSelect) then
        begin
          if assigned(Items[Index]) then
            SubRect.Right := SubRect.Left +
               FCanvas.TextWidth(Items[Index].Caption) + 8;
            if assigned(SmallImages) then
              OffsetRect(SubRect, SmallImages.Width, 0);
            if assigned(StateImages) then
              OffsetRect(SubRect, StateImages.Width, 0);
            // Don't let it go past first column width
            if (Columns.Count > 0) and
               (CurrentColumnWidth[0] < SubRect.Right) then
              SubRect.Right := CurrentColumnWidth[0];
        end else begin
          if assigned(StateImages) then
            Inc(SubRect.Left, StateImages.Width);
          if assigned(SmallImages) then
            Inc(SubRect.Left, SmallImages.Width);
        end;
        FCanvas.DrawFocusRect(SubRect);
      end;
    end else
      FCanvas.FillRect(Rect);

    if (not (csDesigning in ComponentState)) then
      AfterDrawItem(FCanvas, Index, Rect, State);
  end;
end;


procedure TCustomEnhListView.SetStyle(Value: TLVStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomEnhListView.SetReverseSortArrows(Value: boolean);
begin
  if Value <> FReverseSortArrows then
  begin
    FReverseSortArrows := Value;
    if ShowSortArrows then
    begin
      CreateSortBmps(FSortUpBmp, FSortDownBmp);
      InvalidateColumnHeader(FLastColumnClicked);
    end;
  end;
end;

procedure TCustomEnhListView.SetShowSortArrows(Value: boolean);
begin
  if Value <> FShowSortArrows then
    FShowSortArrows := Value;
  FSortUpBmp.Free;
  FSortDownBmp.Free;
  if FShowSortArrows then
  begin
    FSortUpBmp := TBitmap.Create;
    FSortDownBmp := TBitmap.Create;
    CreateSortBmps(FSortUpBmp, FSortDownBmp);
    if not (csReading in ComponentState) then
      SetColumnsOwnerDrawFlag(TRUE);
  end else begin
    FSortUpBmp := NIL;
    FSortDownBmp := NIL;

    if not (csReading in ComponentState) then
      SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader))
  end;
  if HandleAllocated then
    Invalidate;
end;

procedure TCustomEnhListView.CreateSortBmps(var UpBmp, DownBmp: TBitmap);
var
  HeaderHeight: integer;
  MidPoint: integer;
  Bmp: TBitmap;
begin
  if UpBmp = NIL then
    UpBmp := TBitmap.Create;
  if DownBmp = NIL then
    DownBmp := TBitmap.Create;

  UpBmp.Canvas.Font.Assign(Font);
  HeaderHeight := UpBmp.Canvas.TextHeight('Wy') - 6;
  if HeaderHeight > 0 then
  begin
    if Odd(HeaderHeight) then
      Inc(HeaderHeight);
    UpBmp.Width := HeaderHeight;
    UpBmp.Height := HeaderHeight;
    DownBmp.Width := HeaderHeight;
    DownBmp.Height := HeaderHeight;
    MidPoint := HeaderHeight div 2;

    { Don't ask about the drawing.  I just fooled around until I got
      something I liked. }
    if FReverseSortArrows then
      Bmp := UpBmp
    else
      Bmp := DownBmp;
    with Bmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, HeaderHeight-2);
      LineTo(HeaderHeight-1, 0);
      Pixels[HeaderHeight-1, 0] := Pen.Color;
      Pen.Color := clBtnHighlight;
      MoveTo(HeaderHeight-2, 0);
      LineTo(0, 0);
      LineTo(MidPoint-1, HeaderHeight-2);
      Pixels[MidPoint-1, HeaderHeight-2] := Pen.Color;
    end;

    if FReverseSortArrows then
      Bmp := DownBmp
    else
      Bmp := UpBmp;
    with Bmp.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(Rect(0, 0, HeaderHeight, HeaderHeight));
      Pen.Color := clBtnHighlight;
      MoveTo(0, HeaderHeight-1);
      LineTo(MidPoint-1, 0);
      Pen.Color := clBtnShadow;
      MoveTo(MidPoint, 0);
      LineTo(HeaderHeight-1, HeaderHeight-1);
      LineTo(-1, HeaderHeight-1);
      Pixels[MidPoint, 0] := clBtnFace;
    end;
  end;
end;

procedure TCustomEnhListView.DestroyWnd;
begin
  if not FCreatingWindowHandle then
  begin
    inherited DestroyWnd;

    FHeaderHandle := 0;
  end;
end;

procedure TCustomEnhListView.DrawHeader(var Canvas: TCanvas; Index: Integer;
   var Rect: TRect; Selected: boolean; var DefaultDrawing: boolean);
begin
  DefaultDrawing := not assigned(FOnDrawHeader);
  if assigned(FOnDrawHeader) then
    FOnDrawHeader(Self, Canvas, Index, Rect, Selected, DefaultDrawing);
end;

procedure TCustomEnhListView.WMNotify(var Message: TWMNotify);
//const      // [dpv]
var
  RECURSE_FLAG: boolean;
begin
  RECURSE_FLAG:= FALSE;
  if NoColumnResize then
    case Message.NMHdr.code of
      HDN_BEGINTRACK, HDN_TRACK, HDN_BEGINTRACKW, HDN_TRACKW:
      begin
        Message.Result := 1;
        exit;
      end;
    end;

  inherited;
  // Note the recursion flag.  This is needed since the SetColumnsOwnerDrawFlag
  // call below will cause some HDN_xxx notification messages.
  if RECURSE_FLAG then
    exit;

  // For some reason, the SECOND time you drag a header width, it toasts the
  // column index in the draw item message.  Also seems to reset owner draw
  // info at times, too.  Anyway, the best fix I could come up with was to
  // always reset the owner draw flag.
  case Message.NMHdr.code of
    HDN_BEGINTRACK, HDN_ITEMCHANGED, HDN_BEGINTRACKW, HDN_ITEMCHANGEDW:
      begin
        if Message.NMHdr.code <> HDN_TRACK then
        begin
          RECURSE_FLAG := TRUE;
          try
            SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
          finally
            RECURSE_FLAG := FALSE;
          end;
        end;
      end;
    HDN_DIVIDERDBLCLICK, HDN_DIVIDERDBLCLICKW:
      { D4 (and others probably) don't update column width when this happens. }
      begin
        with PHDNotify(Pointer(Message.NMHdr))^ do
          if Item < Columns.Count then
            {$IFDEF DFS_COMPILER_4_UP}
            Column[Item].Width :=
            {$ELSE}
            ActualColumn[Item].Width :=
            {$ENDIF}
              ListView_GetColumnWidth(Handle, Item);
      end;
  end;

(*  old way.  had some performance problems when used in conjunction with
    TToolbar97 component.  No idea why that would cause it, though.
  // For some reason, the SECOND time you drag a header width, it toasts the
  // column index in the draw item message.  Also seems to reset owner draw
  // info at times, too.  Anyway, the best fix I could come up with was to
  // always watch for a change in the header handle, and always reset the owner
  // draw flag.  Note the recursion flag.  This is needed since the
  // SetColumnsOwnerDrawFlag will cause some HDN_xxx notification messages.

  // Best way that I can find to snag the real header handle.  Kludgy at best,
  // but what else are you gonna do?
  case Message.NMHdr.code of
    HDN_LAST..HDN_FIRST:
      begin
        if Message.NMHdr.hwndFrom <> FHeaderHandle then
          FHeaderHandle := Message.NMHdr^.hwndFrom;

        if RECURSE_FLAG or (FUpdateCount > 0) then exit;

        RECURSE_FLAG := TRUE;
        try
          SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
        finally
          RECURSE_FLAG := FALSE;
        end;
      end;
  end;
*)
end;

procedure TCustomEnhListView.WMDrawHeader(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
  DoDefaultDrawing: boolean;
  SavedDC: integer;
begin { CNDrawItem }
  if FCanvas = NIL then exit;

  with Message.DrawItemStruct^ do
  begin
    Message.Result := 1;
    {$IFDEF DFS_COMPILER_5_UP}
    State := TOwnerDrawState(LongRec(itemState).Lo);
    {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
    {$ENDIF}
    SavedDC := SaveDC(hDC);
    FCanvas.Handle := hDC;
    try
      FCanvas.Font := Font;
      FCanvas.Brush := Brush;
      DoDefaultDrawing := FALSE;
      ProcessDrawHeaderMsg(itemID, rcItem, State, DoDefaultDrawing);
    finally
      FCanvas.Handle := 0;
      RestoreDC(hDC, SavedDC);
    end;
  end;
end;

procedure TCustomEnhListView.ProcessDrawHeaderMsg(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing: boolean);
begin
  FCanvas.Font.Assign(Font);
  FCanvas.Brush.Assign(Brush);
  FCanvas.Brush.Style := bsClear;
  FCanvas.Brush.Color := clBtnFace;

  DefaultDrawing := csDesigning in ComponentState;
  if not (csDesigning in ComponentState) then
    DrawHeader(FCanvas, Index, Rect, odSelected in State, DefaultDrawing);

  if DefaultDrawing then
    DefaultDrawHeader(FCanvas, Index, Rect, odSelected in State);
end;

procedure TCustomEnhListView.DefaultDrawHeader(var Canvas: TCanvas;
   Index: Integer; var Rect: TRect; Selected: boolean);
var
  TheColumn: TListColumn;
  Offset: integer;
  R, CR: TRect;
  Bmp: TBitmap;
begin

(******************************************************************************)
(* NOTE:  This method is overriden and replaced in TExtListView.  That means  *)
(*   that if changes are made here, they will also need to be made in         *)
(*   ExtListView.pas' DefaultDrawHeader method.                               *)
(******************************************************************************)

  if not Selected then
    InflateRect(Rect, -2, -2);
  Canvas.FillRect(Rect);
  if Selected then
    InflateRect(Rect, -2, -2);

  if (Index >= 0) and (Index < Columns.Count) then
  begin
    // Don't use ActualColumn[] here!  That's for SubItem foolery, not header.
    TheColumn := Columns[Index];

    if Selected then
    begin
      inc(Rect.Top);
      inc(Rect.Left);
    end;

    R := Rect;

    case TheColumn.Alignment of
      taRightJustify:
        Dec(R.Right, 4);
      taLeftJustify:
        Inc(R.Left, 4);
      // taCenter needs no modification
    end;

    if FShowSortArrows and (LastColumnClicked = Index) and
       (AutoColumnSort <> acsNoSort) then
    begin
      if CurrentSortAscending then
        Bmp := FSortUpBmp
      else
        Bmp := FSortDownBmp;

      if TheColumn.Alignment = taRightJustify then
        Inc(R.Left, Bmp.Width + 8)
      else
        Dec(R.Right, Bmp.Width + 8);

      { How big of a rectangle do we have to work with for the text? }
      CR := R;
      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, CR,
         DRAWTEXTEX_FLAGS or DT_CALCRECT or
         DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], NIL);
      { Note that DT_CALCRECT does not adjust for alignment. We must do that }
      case TheColumn.Alignment of
        taRightJustify:
          R.Left := R.Right - (CR.Right - CR.Left);
        taCenter:
          begin
            R.Left := R.Left + (((R.Right - R.Left) - (CR.Right - CR.Left)) div
               2);
            R.Right := R.Left + (CR.Right - CR.Left);
          end;
      else // taLeftJustify: doesn't matter, that is what DT_CALCRECT returns
        R := CR;
      end;
      if R.Left < Rect.Left then
        R.Left := Rect.Left;
      if R.Right > Rect.Right then
        R.Right := Rect.Right;

      if Selected then
        OffsetRect(R, 1, 1);
      // Draw the caption in the rect available
      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], NIL);

      // Draw the sort arrow bitmap
      Offset := (Rect.Bottom - Rect.Top - Bmp.Height) div 2;
      case TheColumn.Alignment of
        taRightJustify:
          // Only draw if we have enough room
          if (R.Left - Bmp.Width - 8) >= Rect.Left then
            Canvas.Draw(R.Left - Bmp.Width - 8, R.Top + Offset, Bmp);
      else // taLeftJustify, taCenter
        // Only draw if we have enough room
        if (R.Right + Bmp.Width + 8) <= Rect.Right then
          Canvas.Draw(R.Right + 8, R.Top + Offset, Bmp);
      end;
    end else begin
      if Selected then
        OffsetRect(R, 1, 1);
      DrawTextEx(FCanvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], NIL);
    end;
  end;
end;

procedure TCustomEnhListView.SetOnDrawHeader(Value: TLVHDrawItemEvent);
begin
  FOnDrawHeader := Value;
  SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
end;

procedure TCustomEnhListView.SetColumnsOwnerDrawFlag(OwnerDrawn: boolean);
var
  Item: THDItem;
  x: integer;
begin
  if not HandleAllocated then exit;

  for x := 0 to Columns.Count-1 do
  begin
    Item.Mask := HDI_FORMAT;
    if Header_GetItem(HeaderHandle, x, Item) then
    begin
      if OwnerDrawn then
        Item.Fmt := Item.Fmt or HDF_OWNERDRAW
      else
        Item.Fmt := Item.Fmt and not HDF_OWNERDRAW;
      Header_SetItem(HeaderHandle, x, Item);
    end;
  end;

  if OwnerDrawn then
  begin
    if (FCanvas = NIL) then
      FCanvas := TCanvas.Create;
  end else begin
    if (Style = lvStandard) and (FCanvas <> NIL) then
    begin
      FCanvas.Free;
      FCanvas := NIL;
    end;
  end;
end;

procedure TCustomEnhListView.SetLastColumnClicked(Value: integer);
var
  OldValue: integer;
begin
  if Value <> FLastColumnClicked then
  begin
    OldValue := FLastColumnClicked;
    FLastColumnClicked := Value;
    // If showing arrows and clicked column changes, we have to get rid of the
    // old sorting arrow by causing the header to be repainted.
    if FShowSortArrows then
      // Can't do this above because FLastColumnClicked is used to paint the
      // arrow
      InvalidateColumnHeader(OldValue);
  end;
end;

function TCustomEnhListView.ActualColumnIndex(Index: integer): integer;
begin
  Result := Index;
end;

procedure TCustomEnhListView.InvalidateColumnHeader(Index: integer);
  function RealColWidth(i: integer): integer;
  {$IFDEF DFS_COMPILER_4_UP}
  var
    Column: TLVColumn;
  {$ENDIF}
  begin
    {$IFDEF DFS_COMPILER_4_UP}
    Column.mask := LVCF_WIDTH;
    ListView_GetColumn(Handle, i, Column);
    Result := Column.cx;
    {$ELSE}
    Result := Columns[i].Width;
    {$ENDIF}
  end;
var
  R: TRect;
  x: integer;
  w: integer;
begin
  if (Index < 0) or (Index >= Columns.Count) or (HeaderHandle = 0) then
    exit;

  w := RealColWidth(Index);
  // We have to turn this into the actual column index if drag-drop headers have
  // re-arranged stuff in the TExtListView descendant component.
  Index := ActualColumnIndex(Index);

  Windows.GetClientRect(HeaderHandle, R);
  for x := 0 to Columns.Count - 1 do
    if ActualColumnIndex(x) < Index then
      inc(R.Left, RealColWidth(x));
  R.Right := R.Left + w;

  // Adjust for shadow
  InflateRect(R, -2, -2);
  InvalidateRect(HeaderHandle, @R, FALSE);
end;

procedure TCustomEnhListView.WMOwnerDrawColumns(var Message: TMessage);
begin
  SetColumnsOwnerDrawFlag(assigned(FOnDrawHeader) or FShowSortArrows);
  Update;
end;

function TCustomEnhListView.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TCustomEnhListView.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;

procedure TCustomEnhListView.ResizeColumns(ResizeMethod: TResizeMethod);
var
  i: integer;
begin
  BeginUpdate;
  Columns.BeginUpdate;
  try
    for i := 0 to Columns.Count - 1 do
      if ResizeMethod = rmFitText then
        Columns[i].Width := -1
      else
        Columns[i].Width := -2;
  finally
    EndUpdate;
    Columns.EndUpdate;
  end;
end;


function TCustomEnhListView.GetCurrentColumnWidth(Index: integer): integer;
{$IFDEF DFS_COMPILER_4_UP}
var
  Column: TLVColumn;
{$ENDIF}
begin
{$IFDEF DFS_COMPILER_4_UP}
  if HandleAllocated then
  begin
    Column.mask := LVCF_WIDTH;
    ListView_GetColumn(Handle, ActualColumnIndex(Index), Column);
    Result := Column.cx;
  end else
    Result := ActualColumn[Index].Width;
{$ELSE}
  Result := ActualColumn[Index].Width;
{$ENDIF}
end;


{$IFDEF BACKGROUND_FIXED}
procedure TCustomEnhListView.SetBackgroundImage(
   const Value: TBitmap);
begin
  FBackgroundImage.Assign(Value);
  BackgroundImageChanged(Self);
end;
{$ENDIF}

{$IFDEF BACKGROUND_FIXED}
procedure TCustomEnhListView.BackgroundImageChanged(Sender: TObject);
begin
  Brush.Bitmap := NIL;
  if (FBackgroundImage <> NIL) and (not FBackgroundImage.Empty) then
  begin
    // Transparent text
    ListView_SetTextBkColor(Handle, $FFFFFFFF);
    Brush.Bitmap := FBackgroundImage;
  end else begin
    ListView_SetTextBkColor(Handle, ColorToRGB(Color));
    Brush.Color := Color;
  end;
  Invalidate;
end;
{$ENDIF}

function TCustomEnhListView.GetSmallImages:
   {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF};
begin
  Result := inherited SmallImages;
end;

procedure TCustomEnhListView.SetSmallImages(Val:
   {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF});
begin
  inherited SmallImages := Val;

  // If owner drawn, we have to recreate the window so that the WM_MEASUREITEM
  // will get sent to us again, and we can handle it to account for image list
  // size.
  if HandleAllocated and (Style = lvOwnerDrawFixed) and (not (csLoading in
    ComponentState)) then
    ResetOwnerDrawHeight;
end;

procedure TCustomEnhListView.HeaderWndProc(var Message: TMessage);
  function DisallowColumnResize: boolean;
  var
    HTI: THDHitTestInfo;
    pt: TPoint;
  begin
    Result := NoColumnResize;
    if (not Result) and (Self is TCustomExtListView) then
    begin
      // get cursor position
      GetCursorPos(pt);
      // convert to coordinates on header control of the listview
      Windows.ScreentoClient(HeaderHandle, pt);
      // fill in hittest structure
      HTI.flags := HHT_ONHEADER Or HHT_ONDIVIDER;
      HTI.point.x := pt.x;
      HTI.point.y := pt.y;
      //  get the header's hit-test info
      SendMessage(HeaderHandle, HDM_HITTEST, LongInt(0),LongInt(@HTI));
      if (HTI.Item >=0) and (HTI.Item <
        TdfsExtListView(Self).ColumnsFormat.Count) then
        Result := not TdfsExtListView(Self).ColumnsFormat[HTI.Item].AllowResize;
    end;
  end;
var
  HTI: THDHitTestInfo;
  Icon: HICON;
begin
  try
    with Message do
    begin
      case Msg of
        WM_SETCURSOR:
          begin
            if DisallowColumnResize then
//            if NoColumnResize then
            begin
              Icon := GetClassLong(FHeaderHandle, GCL_HICON);
              if Icon = 0 then
                Icon := LoadCursor(0, IDC_ARROW);
              SetCursor(Icon);
              exit;
            end;
          end;
        WM_NCHITTEST:
          begin
            with TWMNCHitTest(Message) do
              if csDesigning in ComponentState then
              begin
                Result := Windows.HTTRANSPARENT;
                exit;
              end
              else if DisallowColumnResize then
              begin
                HTI.Point := Point(LoWord(Message.LParam), HiWord(Message.LParam));
                Windows.ScreenToClient(FHeaderHandle, HTI.Point);
                SendMessage(FHeaderHandle, HDM_HITTEST, 0, integer(@HTI));
                if ((HTI.Flags and HHT_ONHeader) = 0) then
                begin
                  Result := Windows.HTNOWHERE;
                  exit;
                end;
              end;
          end;
        WM_NCDESTROY:
          begin
            Result := CallWindowProc(FOldHeaderWndProc, FHeaderHandle, Msg, WParam, LParam);
            FHeaderHandle := 0;
            FOldHeaderWndProc := nil;
            Exit;
          end;
      end;
      Result := CallWindowProc(FOldHeaderWndProc, FHeaderHandle, Msg, WParam, LParam);
    end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomEnhListView.WMParentNotify(var Message: TWMParentNotify);
begin
  with Message do
    if (Event = WM_CREATE) and (FHeaderHandle = 0) then
    begin
      FHeaderHandle := ChildWnd;
      FOldHeaderWndProc := Pointer(GetWindowLong(FHeaderHandle, GWL_WNDPROC));
      SetWindowLong(FHeaderHandle, GWL_WNDPROC, LongInt(FHeaderInstance));
    end;
  inherited;
end;

procedure TCustomEnhListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  // Ctrl + causes all columns to change size as if their dividers had been
  // double-clicked.  Can't have that.
  if NoColumnResize and (Key = VK_ADD) and (Shift = [ssCtrl]) then
    Key := VK_SUBTRACT;
end;

procedure TCustomEnhListView.ResetOwnerDrawHeight;
var
  r: TRect;
  wp: TWindowPos;
begin
  // Found this code on www.codeguru.com in an article talking about how to get
  // an owner draw listview to ask for the item height (WM_MEASUREITEM) again.  
	GetWindowRect(Handle, r);
	wp.hwnd := Handle;
	wp.cx := Width;
	wp.cy := Height;
	wp.flags := SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER;
	SendMessage(Handle, WM_WINDOWPOSCHANGED, 0, LPARAM(@wp));
end;

procedure TCustomEnhListView.MoveItem(OriginalIndex, NewIndex: Integer);
var
  Selected, Focused: boolean;
  ListItem:  TListItem;
begin
  if ((OriginalIndex < 0) or (OriginalIndex > Items.Count)) or
    ((NewIndex < 0) or (NewIndex > Items.Count)) then
    Exit;
    
  BeginUpdate;
  try
    Selected := Items[OriginalIndex].Selected;
    Focused := Items[OriginalIndex].Focused;
    if NewIndex < OriginalIndex then
      inc(OriginalIndex);
    if (NewIndex > OriginalIndex) then
      ListItem := Items.Insert(NewIndex + 1)
    else
      ListItem := Items.Insert(NewIndex);
    ListItem.Assign(Items[OriginalIndex]);
    Items.Delete(OriginalIndex);
    ListItem.Selected := Selected;
    ListItem.Focused := Focused;
  finally
    EndUpdate;
  end;
end;

procedure TCustomEnhListView.KeyUp(var Key: Word; Shift: TShiftState);
var
  PrevSearch: string;
  Ascii: array[0..1] of char;
  KBState: TKeyboardState;
begin
  inherited;
  if ColumnSearch then
  begin
    GetKeyboardState(KBState);
    if (ToAscii(Key, 0, KBState, Ascii, 0) = 1) and (Ascii[0] in [#32..#127]) then
    begin
      PrevSearch := FSearchStr;                      // remember searchstring
      if GetTickCount > FSearchTickCount + 1000 then // last search over one second ago?
        PrevSearch := '';                            // reset searchstring
      FSearchStr := PrevSearch + Ascii[0];           // Append searchstring
      FSearchTickCount := GetTickCount;              // remember last search time
      Key := 0;                                      // prevent automatic search on first column
      if not StringSelect(FSearchStr, LastColumnClicked) then
      begin
        MessageBeep(MB_ICONSTOP);
        FSearchStr := PrevSearch;
      end;
    end;
  end;
end;

function TCustomEnhListView.StringSelect(FindStr: string; ColumnIndex: Integer): boolean;
var
  SearchLen,
  SearchIndex,
  SearchStart: Integer;
begin
  Result := FALSE;
  SearchLen := Length(FindStr);
  if Assigned(Selected) then   // determine starting item
    SearchStart := Selected.Index + 1
  else
    SearchStart := 1;

  // Searches from currently selected item to last item
  // and from first item to currently selected item until result(found)

  SearchIndex := 0;
  while (SearchIndex < Items.Count) and not Result do
  begin
    if ColumnIndex = 0 then                                // find main or subitem?
      Result := AnsiCompareText(Copy(Items[(SearchStart + SearchIndex) mod
        Items.Count].Caption, 0, SearchLen), FindStr) = 0
    else
      Result := AnsiCompareText(Copy(Items[(SearchStart + SearchIndex) mod
        Items.Count].SubItems[ColumnIndex - 1], 0, SearchLen), FindStr) = 0;
    Inc(SearchIndex);
  end;
  if Result then
  begin
    Selected := Items[(SearchStart + SearchIndex - 1) mod Items.Count];
    ItemFocused := Selected;
  end;
end;

function TCustomEnhListView.SubStringSelect(FindStr: string;
  ColumnIndex: Integer): boolean;
var
  SearchIndex,
  SearchStart: Integer;
begin
  Result := FALSE;
  if Assigned(Selected) then  // determine starting item
    SearchStart := Selected.Index + 1
  else
    SearchStart := 1;

  // Searches from currently selected item to last item
  // and from first item to currently selected item until result(found)

  SearchIndex := 0;
  while (SearchIndex < Items.Count) and not Result do
  begin
    if ColumnIndex = 0 then                                // find main or subitem?
      Result := Pos(FindStr, Items[(SearchStart + SearchIndex) mod
        Items.Count].Caption) > 0
    else
      Result := Pos(FindStr, Items[(SearchStart + SearchIndex) mod
        Items.Count].SubItems[ColumnIndex - 1]) > 0;
    Inc(SearchIndex);
  end;
  if Result then
  begin
    Selected := Items[(SearchStart + SearchIndex - 1) mod Items.Count];
    ItemFocused := Selected;
  end;
end;

{$IFNDEF DFS_COMPILER_4_UP}
procedure TCustomEnhListView.GetImageIndex(Item: TListItem);
begin
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Self, Item);
end;
{$ENDIF}

procedure TCustomEnhListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = SmallImages) then
    SmallImages := NIL;
  inherited Notification(AComponent, Operation);
end;

initialization
  DefDraw_TextOffset := 4;
  DefDraw_ImageOffset := 2;
end.

