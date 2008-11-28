{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{.$DEFINE DFS_DEBUG}
{.$DEFINE DFS_TRY_BACKGROUND_IMAGE}

{------------------------------------------------------------------------------}
{ TdfsExtListView v3.70                                                        }
{------------------------------------------------------------------------------}
{ A list view control that enables access to the new style types provieded     }
{ by the updated list view control.  The updated list view is provided in      }
{ the COMCTL32.DLL file that comes with Microsoft's new internet software.     }
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

(*  // [dpv]
The following items from CommCtrl.h are not declared in CommCtrl.pas for Delphi 2005 up1 (both ):
#if (_WIN32_IE >= 0x0500)
#define LVS_EX_LABELTIP         0x00004000
#define LVS_EX_BORDERSELECT     0x00008000
#endif  // End (_WIN32_IE >= 0x0500)
#if (_WIN32_WINNT >= 0x501)
#define LVS_EX_DOUBLEBUFFER     0x00010000
#define LVS_EX_HIDELABELS       0x00020000
#define LVS_EX_SINGLEROW        0x00040000
#define LVS_EX_SNAPTOGRID       0x00080000
#define LVS_EX_SIMPLESELECT     0x00100000
#endif

So could we have the following added in the appropriate location:

  {$EXTERNALSYM LVS_EX_LABELTIP}
  LVS_EX_LABELTIP        = $00004000;
  {$EXTERNALSYM LVS_EX_BORDERSELECT}
  LVS_EX_BORDERSELECT    = $00008000;
  {$EXTERNALSYM LVS_EX_DOUBLEBUFFER}
  LVS_EX_DOUBLEBUFFER    = $00010000;
  {$EXTERNALSYM LVS_EX_HIDELABELS}
  LVS_EX_HIDELABELS      = $00020000;
  {$EXTERNALSYM LVS_EX_SINGLEROW}
  LVS_EX_SINGLEROW       = $00040000;
  {$EXTERNALSYM LVS_EX_SNAPTOGRID}
  LVS_EX_SNAPTOGRID      = $00080000;
  {$EXTERNALSYM LVS_EX_SIMPLESELECT}
  LVS_EX_SIMPLESELECT    = $00100000;
*)


unit ExtListView;

interface

{$IFNDEF DFS_WIN32}
  ERROR!  This unit only available for Delphi 2.0 or higher!!!
{$ENDIF}

uses
  Windows, Messages, Classes, Controls, ComCtrls, CommCtrl, SysUtils, Graphics,
{$IFDEF DFS_COMPILER_4_UP}
  ImgList,
{$ENDIF}
  StdCtrls, Menus, EnhListView;

const
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsExtListView v3.70';

// Setting a subitem image (lvxSubItemImages ExtendStyle) to -1 does not
// properly clear the image for the subitem.  The current COMCTL32.DLL
// implementation does not seem to store this value and instead it gets a
// random value assigned to it.  The work-around that I have found is to set
// the index to a value that does not exist in the image list.  To make this
// a bit easier, I have declared this constant.  Assigning this value to
// SubItem_ImageIndex[itemindex] will clear the image from the subitem as long
// as your image list does not have more than 2,147,483,646 images in it. :)
const
  ELV_NO_SUBITEM_IMAGE    = MAXINT - 1;


// C3 and D4 CommCtrl.pas have almost everything we need
{$IFDEF DFS_CPPB_3_UP}
  {$DEFINE DFS_C3D4COMMCTRL}
{$ELSE} {$IFDEF DFS_DELPHI_4_UP}
  {$DEFINE DFS_C3D4COMMCTRL}
{$ENDIF} {$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
type
  TLVDispInfo = TLVDispInfoA; // Borland forgot this one in D2, D3 & C1s
{$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVIF_INDENT             = $0010;
  LVIF_NORECOMPUTE        = $0800;

{.$IFDEF DFS_DELPHI_2}
{ These are in COMMCTRL unit
  LVCF_FMT                = $0001;
  LVCF_WIDTH              = $0002;
  LVCF_TEXT               = $0004;
  LVCF_SUBITEM            = $0008;
}
{.$ENDIF}
  LVCF_IMAGE              = $0010;
  LVCF_ORDER              = $0020;

{.$IFDEF DFS_DELPHI_2}
{ These are in COMMCTRL unit
  LVCFMT_LEFT             = $0000;
  LVCFMT_RIGHT            = $0001;
  LVCFMT_CENTER           = $0002;
  LVCFMT_JUSTIFYMASK      = $0003;
}
{.$ENDIF}
  LVCFMT_IMAGE            = $0800; // Item displays an image from an image list.
  LVCFMT_BITMAP_ON_RIGHT  = $1000; // Image appears to right of Text.
  LVCFMT_COL_HAS_IMAGES   = $8000; // Undocumented.

  LVIS_ACTIVATING         = $0020;
{$ENDIF}

type
  PLVItemEx = ^TLVItemEx;
  TLVItemEx = packed record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: integer;
  end;

  PLVDispInfoEx = ^TLVDispInfoEx;
  TLVDispInfoEx = packed record
    hdr:   TNMHDR;
    item:  TLVItemEx;
  end;

  TLVColumnEx = packed record
    mask: UINT;
    fmt: Integer;
    cx: Integer;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: integer; // New
    iOrder: integer; // New
  end;


// These functions already exist, and there is no way to override them, so I'll
// just rename them and you can use them as best you can.
function ListView_GetColumnEx(LVWnd: HWND; iCol: Integer;
   var pcol: TLVColumnEx): Bool;
function ListView_SetColumnEx(LVWnd: HWnd; iCol: Integer;
   const pcol: TLVColumnEx): Bool;
function ListView_InsertColumnEx(LVWnd: HWND; iCol: Integer;
   const pcol: TLVColumnEx): Integer;


{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVM_GETHEADER           = LVM_FIRST + 31;

function ListView_GetHeader(LVWnd: HWnd): HWnd;
{$ENDIF}


{$IFNDEF DFS_COMPILER_3_UP}
const
  LVM_SETICONSPACING      = LVM_FIRST + 53;


// -1 for cx and cy means we'll use the default (system settings)
// 0 for cx or cy means use the current setting (allows you to change just one
// param)
function ListView_SetIconSpacing(LVWnd: HWnd; cx, cy: integer): DWORD;

const
  LVS_EX_GRIDLINES             = $00000001;  // Report mode only.
  LVS_EX_SUBITEMIMAGES         = $00000002;  // Report mode only.
  LVS_EX_CHECKBOXES            = $00000004;
  LVS_EX_TRACKSELECT           = $00000008;
  LVS_EX_HEADERDRAGDROP        = $00000010;  // Report mode only.
  LVS_EX_FULLROWSELECT         = $00000020;  // Report mode only.
  LVS_EX_ONECLICKACTIVATE      = $00000040;
  LVS_EX_TWOCLICKACTIVATE      = $00000080;

  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54; // optional wParam = mask

function ListView_SetExtendedListViewStyle(LVWnd: HWnd; ExStyle: LPARAM): DWORD;

const
  LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55;

function ListView_GetExtendedListViewStyle(LVWnd: HWnd): DWORD;

{$ENDIF}

(* These were already defined in everything...
const
  LVIR_BOUNDS             = 0;
  LVIR_ICON               = 1;
  LVIR_LABEL              = 2;
  LVIR_SELECTBOUNDS       = 3;
*)

{$IFDEF DFS_COMPILER_2}
const
  LVM_GETSUBITEMRECT      = LVM_FIRST + 56;

function ListView_GetSubItemRect(LVWnd: HWnd; ParentItem, SubItem,
   Code: integer; var Rect: TRect): boolean;

const
  LVM_SUBITEMHITTEST      = LVM_FIRST + 57;
{$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVS_EX_FLATSB           = $00000100;
  LVS_EX_REGIONAL         = $00000200;
  LVS_EX_INFOTIP          = $00000400;
  LVS_EX_UNDERLINEHOT     = $00000800;
  LVS_EX_UNDERLINECOLD    = $00001000;
  LVS_EX_MULTIWORKAREAS   = $00002000;

// Pass the LVS_EX_* styles you want to modify in Mask and ExStyle will apply
// only to those.  Others will be left in current state.  For example, if you
// pass LVS_EX_FULLROWSELECT for Mask and 0 for ExStyle, the
// LVS_EX_FULLROWSELECT style will be cleared, but all other styles will remain
// the same.
function ListView_SetExtendedListViewStyleEx(LVWnd: HWnd; Mask: DWord;
   ExStyle: LPARAM): DWORD;
{$ENDIF}

{$IFNDEF DFS_COMPILER_6}
const
  LVS_EX_LABELTIP         = $00004000; // Requires ComCtl32.DLL v5.80
{$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
  // C3 & D4 users don't need this because their COMMCTRL.PAS file has it right
  // and they can simply use the existing TLVHitTestInfo and
  // ListView_SubItemHitTest()
type
  PLVHitTestInfoEx = ^TLVHitTestInfoEx;
  TLVHitTestInfoEx = packed record
    pt: TPoint;
    flags: UINT;
    iItem: integer;
    iSubItem: integer;
  end;

function ListView_SubItemHitTestEx(LVWnd: HWnd;
   var HitTestInfo: TLVHitTestInfoEx): integer;
{$ENDIF}

{$IFNDEF DFS_COMPILER_3_UP}
const
  LVM_SETCOLUMNORDERARRAY = LVM_FIRST + 58;

function ListView_SetColumnOrderArray(LVWnd: HWnd; Count: integer;
   IntArray: PIntArray): boolean;

const
  LVM_GETCOLUMNORDERARRAY = LVM_FIRST + 59;

function ListView_GetColumnOrderArray(LVWnd: HWnd; Count: integer;
   IntArray: PIntArray): boolean;

const
  LVM_SETHOTITEM  = LVM_FIRST + 60;

function ListView_SetHotItem(LVWnd: HWnd; Item: integer): integer;

const
  LVM_GETHOTITEM  = LVM_FIRST + 61;

function ListView_GetHotItem(LVWnd: HWnd): integer;

const
  LVM_SETHOTCURSOR  = LVM_FIRST + 62;

function ListView_SetHotCursor(LVWnd: HWnd; Cursor: HCursor): HCursor;

const
  LVM_GETHOTCURSOR  = LVM_FIRST + 63;

function ListView_GetHotCursor(LVWnd: HWnd): HCursor;

const
  LVM_APPROXIMATEVIEWRECT = LVM_FIRST + 64;

function ListView_ApproximateViewRect(LVWnd: HWnd; Width, Height,
   Count: integer): DWORD;

const
  LVM_SETWORKAREA         = LVM_FIRST + 65;

function ListView_SetWorkArea(LVWnd: HWnd; const Rect: TRect): boolean;

function ListView_GetCheckState(LVWnd: HWnd; Index: UINT): boolean;

procedure ListView_SetCheckState(LVWnd: HWnd; Index: UINT; Checked: boolean);
{$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVSICF_NOINVALIDATEALL  = $00000001;
  LVSICF_NOSCROLL         = $00000002;

procedure ListView_SetItemCountEx(LVWnd: HWnd; Items: integer; Flags: DWORD);
{$ENDIF}

{$IFNDEF DFS_COMPILER_3_UP}
const
  // New list view style flags.
  LVS_OWNERDATA                = $1000; // Specifies a "virtual" control.

  // New notification messages.
  LVN_ODCACHEHINT              = LVN_FIRST-13;
  LVN_ODFINDITEMA              = LVN_FIRST-52;
  LVN_ODFINDITEMW              = LVN_FIRST-79;
  LVN_ODFINDITEM               = LVN_ODFINDITEMA;
{$ENDIF}

{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVN_ITEMACTIVATE             = LVN_FIRST-14;
  LVN_ODSTATECHANGED           = LVN_FIRST-15;
  LVN_MARQUEEBEGIN             = LVN_FIRST-56;
{$ENDIF}

{$IFNDEF DFS_COMPILER_3_UP}
type
  PNMCacheHint = ^TNMCacheHint;
  TNMCacheHint = packed record
    hdr:       TNMHDR;
    iFrom:     integer;
    iTo:       integer;
  end;

  PNMFindItem = ^TNMFindItem;
  TNMFindItem = packed record
    hdr:       TNMHDR;
    iStart:    integer;
    lvif:      TLVFindInfo;
  end;
{$ENDIF}

type
  PNMODStateChange = ^TNMODStateChange;
  TNMODStateChange = packed record
    hdr:       TNMHDR;
    iFrom:     integer;
    iTo:       integer;
    uNewState: UINT;
    uOldState: UINT;
  end;


{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVM_GETSELECTIONMARK = (LVM_FIRST + 66);

function ListView_GetSelectionMark(LVWnd: HWnd): integer;

const
  LVM_SETSELECTIONMARK = (LVM_FIRST + 67);

function ListView_SetSelectionMark(LVWnd: HWnd; iIndex: integer): integer;

const
  LVM_SETHOVERTIME = (LVM_FIRST + 71);

function ListView_SetHoverTime(LVWnd: HWnd; dwHoverTimeMS: DWORD): DWORD;

const
  LVM_GETHOVERTIME = (LVM_FIRST + 72);

function ListView_GetHoverTime(LVWnd: HWnd): DWORD;

const
  LVM_SETTOOLTIPS = (LVM_FIRST + 74);

function ListView_SetToolTips(LVWnd, NewWnd: HWnd): HWnd;

const
  LVM_GETTOOLTIPS = (LVM_FIRST + 78);

function ListView_GetToolTips(LVWnd: HWnd): HWnd;
{$ENDIF}

type
  PLVBkImageA = ^TLVBkImageA;
  TLVBkImageA = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PChar;
    cchImageMax: UINT;
    xOffsetPercent: integer;
    yOffsetPercent: integer;
  end;

  PLVBkImageW = ^TLVBkImageW;
  TLVBkImageW = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PWideChar;
    cchImageMax: UINT;
    xOffsetPercent: integer;
    yOffsetPercent: integer;
  end;

  PLVBkImage = PLVBkImageA;
  TLVBkImage = TLVBkImageA;

{$IFNDEF DFS_C3D4COMMCTRL}
const
  LVBKIF_SOURCE_NONE      = $00000000;
  LVBKIF_SOURCE_HBITMAP   = $00000001;
  LVBKIF_SOURCE_URL       = $00000002;
  LVBKIF_SOURCE_MASK      = $00000003;
  LVBKIF_STYLE_NORMAL     = $00000000;
  LVBKIF_STYLE_TILE       = $00000010;
  LVBKIF_STYLE_MASK       = $00000010;

  LVM_SETBKIMAGEA         = (LVM_FIRST + 68);
  LVM_SETBKIMAGEW         = (LVM_FIRST + 138);
  LVM_GETBKIMAGEA         = (LVM_FIRST + 69);
  LVM_GETBKIMAGEW         = (LVM_FIRST + 139);

const
  LVM_SETBKIMAGE          = LVM_SETBKIMAGEA;

function ListView_SetBkImage(LVWnd: HWnd; plvbki: PLVBkImage): BOOL;

const
  LVM_GETBKIMAGE          = LVM_GETBKIMAGEA;

function ListView_GetBkImage(LVWnd: HWnd; plvbki: PLVBkImage): BOOL;

const
  LVN_HOTTRACK            = (LVN_FIRST-21);
{$ENDIF}

type
  PNMLVGetInfoTipA = ^TNMLVGetInfoTipA;
  TNMLVGetInfoTipA = packed record
    hdr: TNMHDR;
    dwFlags: DWORD;
    pszText: PChar;
    cchTextMax: integer;
    iItem: integer;
    iSubItem: integer;
    lParam: LPARAM;
  end;

  PNMLVGetInfoTipW = ^TNMLVGetInfoTipW;
  TNMLVGetInfoTipW = packed record
    hdr: TNMHDR;
    dwFlags: DWORD;
    pszText: PWideChar;
    cchTextMax: integer;
    iItem: integer;
    iSubItem: integer;
    lParam: LPARAM;
  end;

  PNMLVGetInfoTip = PNMLVGetInfoTipA;
  TNMLVGetInfoTip = TNMLVGetInfoTipA;

{$IFNDEF DFS_C3D4COMMCTRL}
// NMLVGETINFOTIPA.dwFlag values
const
  LVGIT_UNFOLDED          = $0001;

  LVN_GETINFOTIPA         = (LVN_FIRST-57);
  LVN_GETINFOTIPW         = (LVN_FIRST-58);
  LVN_GETINFOTIP          = LVN_GETINFOTIPA;
{$ENDIF}

type
  EELVException = class(Exception);
  EELVOldComCtl = class(EELVException);

  // New extended style flags converted to set format (RPM = Report Mode Only).
  //  lvxGridlines: Adds grid lines to seperate items and columns. RPM
  //  lvxSubItemImages: Allows images to be displayed for subitems. RPM
  //  lvxCheckboxes: Adds checkboxes to items.  Checked items are stored
  //      internally as selected items.
  //  lvxTrackSelect: Tracks the mouse and highlights the item it currently
  //      positioned over by changing it's color.  If mouse is left over an
  //      item for a brief period of time, it will be automatically selected.
  //  lvxHeaderDragDrop: Allows headers to be dragged to new positions and
  //      dropped, allowing users to reorder column information.
  //  lvxFullRowSelect: Allows user to click anywhere on an item to select it,
  //      highlighting the entire length of the item.  Without this style,
  //      users must click inside the Text of column 0.  It is only useful in
  //      vsReport view style.
  //  lvxOneClickActivate: Sends an LVN_ITEMACTIVATE notification message to
  //      the parent when the user clicks an item.
  //  lvxTwoClickActivate: Sends an LVN_ITEMACTIVATE notification message to
  //      the parent when the user double clicks an item.
  //  lvxFlatScrollBar: Enables flat scroll bars in the list view.
  //  lvxInfoTip: Enables the OnInfoTip event that allows notification and/or
  //      modification of a tooltip before it is displayed.  Only allowed for
  //      vsIcon ViewStyle.
  //  lvxUnderlineHot: Causes hot items to be displayed with underlined Text.
  //      This style is ignored if lvxOneClickActivate or lvxTwoClickActivate
  //      is not set.
  //  lvxUnderlineCold: Causes nonhot items to be displayed with underlined
  //      Text. This style is ignored if lvxOneClickActivate is not set.
  //  lvxLabelTip: If a partially hidden label in any list view mode lacks
  //      tooltip text, the list view control will unfold the label. If this
  //      style is not set, the list view control will unfold partly hidden
  //      labels only for the large icon mode.  Requires ComCtl32.dll v5.80. 
  TLVExtendedStyle = (lvxGridLines, lvxSubItemImages, lvxCheckboxes,
     lvxTrackSelect, lvxHeaderDragDrop, lvxFullRowSelect, lvxOneClickActivate,
     lvxTwoClickActivate, lvxFlatScrollBar, lvxInfoTip, lvxUnderlineHot,
     lvxUnderlineCold, lvxLabelTip);

  // A set of the new style bits.
  TLVExtendedStyles = set of TLVExtendedStyle;

  TLVItemCountFlag = (lvsicfNoInvalidateAll, lvsicfNoScroll);
  TLVItemCountFlags = set of TLVItemCountFlag;

  TLVVMMaskItem = (lvifText, lvifImage, lvifParam, lvifState, lvifIndent);
  TLVVMMaskItems = set of TLVVMMaskItem;
  TLVVMStateMaskItem = (lvisCut, lvisDropHighlighted, lvisFocused, lvisSelected,
     lvisOverlayMask);
  TLVVMStateMaskItems = set of TLVVMStateMaskItem;

  TColumnImageAlign = (ciaLeftOfText, ciaRightOfText);

  TLVItemCheckedEvent = procedure (Sender: TObject; ItemIndex: integer;
    Checked: boolean) of object;
  TLVMarqueeBeginEvent = procedure(Sender: TObject;
     var CanBegin: boolean) of object;
  TLVItemActivateEvent = TNotifyEvent;
  TLVInfoTipEvent = procedure(Sender: TObject; ItemIndex, SubItemIndex: integer;
     Current: string; var Additional: string) of object;
  TLVHotTrackEvent = procedure(Sender: TObject; var ItemIndex: integer;
     SubItemIndex: integer; Location: TPoint;
     var AllowSelect: boolean) of object;
  TLVVMGetItemInfoEvent = procedure(Sender: TObject; Item, SubItem: integer;
     var Mask: TLVVMMaskItems; var Image: integer; var OverlayImage,
     StateImage: word; var Param: LPARAM; var State: UINT; var StateMask: UINT;
     var Indent: integer; var Text: string) of object;
  TLVVMCacheHintEvent = procedure(Sender: TObject;
     var HintInfo: TNMCacheHint) of object;
  TLVVMFindItemEvent = procedure(Sender: TObject; var FindInfo: TNMFindItem;
     var Found: integer) of object;
  TLVVMStateChangedEvent = procedure(Sender: TObject;
     var StateInfo: TNMODStateChange) of object;
  TLVVMCaptionEditedEvent = procedure (Sender: TObject; Item: integer;
    Canceled: boolean; const Text: string) of object;
  {$IFNDEF DFS_COMPILER_4_UP}
  TLVSubItemImageEvent = procedure(Sender: TObject; Item: TListItem; SubItem: Integer;
    var ImageIndex: Integer) of object;
  {$ENDIF}


  TCustomExtListView = class; { forward declaration }


{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
  // Class for BackgroundImage property
  TELVBackgroundImage = class(TPersistent)
  private
    FOwningListView: TCustomExtListView;
    FFilename: string;
    FBrushBmp: TBitmap;
    FTile: boolean;
    FXOffsetPercent: integer;
    FYOffsetPercent: integer;
  protected
    procedure SetFilename(const Val: string);
    procedure SetTile(Val: boolean);
    procedure SetXOffsetPercent(Val: integer);
    procedure SetYOffsetPercent(Val: integer);

    procedure ApplyToListView; virtual;
  public
    constructor Create(AOwner: TCustomExtListView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Filename: string
       read FFilename
       write SetFilename;
    property Tile: boolean
       read FTile
       write SetTile
       default FALSE;
    property XOffsetPercent: integer
       read FXOffsetPercent
       write SetXOffsetPercent
       default 0;
    property YOffsetPercent: integer
       read FYOffsetPercent
       write SetYOffsetPercent
       default 0;
  end;
{$ENDIF}


  // Class for saved settings
  TdfsExtLVSaveSettings = class(TdfsEnhLVSaveSettings)
  private
    FSaveColumnOrder: boolean;
  public
    constructor Create; override;
    procedure StoreColumnOrder(ColCount: integer;
       const IntArray: array of integer);
    procedure ReadColumnOrder(ColCount: integer;
       var IntArray: array of integer);
  published
    property SaveColumnOrder: boolean
       read FSaveColumnOrder
       write FSaveColumnOrder
       default TRUE;
  end;


  TdfsExtListColumn = class(TCollectionItem)
  private
    FSmallImageIndex: Integer;
    FImageAlignment : TColumnImageAlign;
    FAllowResize: boolean;
    procedure DoChange;
    procedure SetSmallImageIndex(Value: Integer);
    procedure SetImageAlignment(Value: TColumnImageAlign);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: integer
       read FSmallImageIndex
       write SetSmallImageIndex
       default -1;
    property ImageAlignment: TColumnImageAlign
       read FImageAlignment
       write SetImageAlignment
       default ciaRightOfText;
    property AllowResize: boolean
       read FAllowResize
       write FAllowResize
       default TRUE;
  end;

  TdfsExtListColumns = class(TCollection)
  private
    FOwner: TCustomExtListView;
    function GetItem(Index: Integer): TdfsExtListColumn;
    procedure SetItem(Index: Integer; Value: TdfsExtListColumn);
  protected
    function GetOwner: TPersistent; {$IFDEF DFS_COMPILER_3_UP} override; {$ENDIF}
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomExtListView);
    procedure Assign(Source: TPersistent); override;
    function Add: TdfsExtListColumn;
    procedure Refresh;
    property Owner: TCustomExtListView
       read FOwner;
    property Items[Index: Integer]: TdfsExtListColumn
       read GetItem
       write SetItem;
       default;
  end;

  // The new class.
  TCustomExtListView = class(TCustomEnhListView)
  private
    FExtendedStyles: TLVExtendedStyles;
    FColumnOrder: PIntArray;
    FColumnOrderCount: integer;
    FColumnsFormat: TdfsExtListColumns;
    FVirtualMode: boolean;
    FSaveSettings: TdfsExtLVSaveSettings;
    FColumnsFormatChangeLink: TChangeLink;
    FSelectionMark: integer;
    FHoverTime: Longint;
    FRequireComCtlUpdate: boolean;
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
    FBackgroundImage: TELVBackgroundImage;
{$ENDIF}
    FItemCountEx: integer;
    FItemCountExFlags: TLVItemCountFlags;
    FRecreateStream: TMemoryStream;
{$IFDEF DFS_COMPILER_4_UP}
    FInhibitFeedData: boolean;
{$ENDIF}
    FChecked: boolean;
    FCheckedListItemIndex: integer;

    FOnItemChecked: TLVItemCheckedEvent;
    FOnMarqueeBegin: TLVMarqueeBeginEvent;
    FOnItemActivate: TLVItemActivateEvent;
    FOnHotTrack: TLVHotTrackEvent;
    FOnInfoTip: TLVInfoTipEvent;
    FOnVMGetItemInfo: TLVVMGetItemInfoEvent;
    FOnVMCacheHint: TLVVMCacheHintEvent;
    FOnVMFindItem: TLVVMFindItemEvent;
    FOnVMStateChanged: TLVVMStateChangedEvent;
    FOnVMCaptionEdited: TLVVMCaptionEditedEvent;

    // Function to convert from our set type to expected API value.
    function SetValueToAPIValue(Styles: TLVExtendedStyles): LPARAM;
    function SetValueFromAPIValue(Styles: DWORD): TLVExtendedStyles;

    procedure ColumnHeaderImagesChange(Sender: TObject);

    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    function GetItemIndent(Index: integer): Integer;
    procedure SetItemIndent(Index: integer; Value: Integer);
{$IFDEF DFS_COMPILER_4_UP}
    procedure FeedOwnerDataMode(Sender: TObject; Item: TListItem);
{$ENDIF}
  protected
    // Property method for setting styles.
    procedure SetExtendedStyles(Val: TLVExtendedStyles);
    function GetExtendedStyles: TLVExtendedStyles;
    function GetHeaderHandle: HWnd;
    function GetSubItemRect(Item, SubItem: integer; Index: integer): TRect;
    procedure SetHotItem(Val: integer);
    function GetHotItem: integer;
    procedure SetHotCursor(const Val: HCursor);
    function GetHotCursor: HCursor;
    procedure SetWorkArea(Rect: TRect);
    procedure SetCheckState(Index: integer; Checked: boolean);
    function GetCheckState(Index: integer): boolean;
    procedure SetVirtualMode(Val: boolean);
    procedure SetColumnsFormat(Value: TdfsExtListColumns);
    function GetSubItemImageIndex(Item, SubItem: integer): integer;
    procedure SetSubItemImageIndex(Item, SubItem, Value: integer);
    function GetSelectionMark: integer;
    procedure SetSelectionMark(Val: integer);
    function GetHoverTime: Longint;
    procedure SetHoverTime(Val: Longint);
    procedure SetRequireComCtlUpdate(Value: boolean);
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
    procedure SetBackgroundImage(Value: TELVBackgroundImage);
{$ENDIF}
    function GetStateImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList; {$ELSE} TImageList; {$ENDIF}
    procedure SetStateImages(Value: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF});
    function GetSmallImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList; {$ELSE} TImageList; {$ENDIF}
    procedure SetSmallImages(Value: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF});
    function GetShowSortArrows: boolean;
    procedure SetShowSortArrows(Value: boolean);
    function GetVersion: string; override;
    function GetSubItemText(Index, SubItem: integer): string; override;
    function ActualColumnIndex(Index: integer): integer; override;
    function GetActualColumn(Index: integer): TListColumn; override;
    procedure DestroyWnd; override;
    procedure RestoreChecks;
    procedure SaveChecks;
    procedure MeasureItem(var Height: UINT); override;
    procedure DrawItem(var Canvas: TCanvas; Index: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing,
       FullRowSelect: boolean); override;
    procedure DrawSubItem(Index, SubItem: Integer; Rect: TRect;
       State: TOwnerDrawState; var DefaultDrawing: boolean); override;
    procedure DefaultDrawHeader(var Canvas: TCanvas; Index: Integer;
       var Rect: TRect; Selected: boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DefaultDrawItem(Index: Integer; Rect: TRect;
       State: TOwnerDrawState; FullRowSelect: boolean); override;
    procedure DefaultDrawSubItem(Index, SubItem: integer; Rect: TRect;
       State: TOwnerDrawState); override;
    // Event method handlers -- fire the events if they exist.
    procedure ItemChecked(ItemIndex: integer; Checked: boolean); virtual;
    function MarqueeBegin: boolean; virtual;
    procedure ItemActivate; virtual;
    function HotTrack(var Item: TNMListView): boolean; virtual;
    procedure GetInfoTip(InfoTip: PNMLVGetInfoTip); virtual;

    procedure VMGetDispInfo(var ItemInfo: TLVItemEx); virtual;
    procedure VMCacheHint(var HintInfo: TNMCacheHint); virtual;
    function VMFindItem(var FindInfo: TNMFindItem): integer; virtual;
    procedure VMStateChanged(var StateInfo: TNMODStateChange); virtual;
    procedure VMCaptionEdited(Item: integer; Canceled: boolean; const Text: string);

    // These should be redeclared as PUBLIC as needed.
    property HeaderHandle: HWnd
       read GetHeaderHandle;
    property SubItem_BoundsRect[Item: integer; SubItem: integer]: TRect
       index LVIR_BOUNDS
       read GetSubItemRect;
    property SubItem_IconRect[Item: integer; SubItem: integer]: TRect
       index LVIR_ICON
       read GetSubItemRect;
    property SubItem_LabelRect[Item: integer; SubItem: integer]: TRect
       index LVIR_LABEL
       read GetSubItemRect;
    property SubItem_SelectBoundsRect[Item: integer; SubItem: integer]: TRect
       index LVIR_SELECTBOUNDS
       read GetSubItemRect;
    property HotItem: integer
       read GetHotItem
       write SetHotItem;
    property HotCursor: HCursor
       read GetHotCursor
       write SetHotCursor;
    property WorkArea: TRect
       write SetWorkArea;
    property IsChecked[Index: integer]: boolean
       read GetCheckState
       write SetCheckState;
    property SubItem_ImageIndex[Item: integer; SubItem: integer]: integer
       read GetSubItemImageIndex
       write SetSubItemImageIndex;
    property SelectionMark: integer
       read GetSelectionMark
       write SetSelectionMark;
    property ItemIndent[Index: integer]: integer
       read GetItemIndent
       write SetItemIndent;

    // These should be redeclared as PUBLIC or PUBLISHED as needed
    property ExtendedStyles: TLVExtendedStyles
       read GetExtendedStyles
       write SetExtendedStyles
       default [lvxInfoTip];
    property VirtualMode: boolean
       read FVirtualMode
       write SetVirtualMode
       default FALSE;
    property HoverTime: Longint
       read GetHoverTime
       write SetHoverTime
       default -1;
    property RequireComCtlUpdate: boolean
       read FRequireComCtlUpdate
       write SetRequireComCtlUpdate
       default FALSE;
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
    property BackgroundImage: TELVBackgroundImage
       read FBackgroundImage
       write SetBackgroundImage;
{$ENDIF}

    // Autosave settings property.
    property SaveSettings: TdfsExtLVSaveSettings
       read FSaveSettings
       write FSaveSettings;

    property ColumnsFormat: TdfsExtListColumns
       read FColumnsFormat
       write SetColumnsFormat;

    // Events
    property OnItemChecked: TLVItemCheckedEvent
       read FOnItemChecked
       write FOnItemChecked;
    property OnMarqueeBegin: TLVMarqueeBeginEvent
       read FOnMarqueeBegin
       write FOnMarqueeBegin;
    property OnItemActivate: TLVItemActivateEvent
       read FOnItemActivate
       write FOnItemActivate;
    property OnHotTrack: TLVHotTrackEvent
       read FOnHotTrack
       write FOnHotTrack;
    property OnInfoTip: TLVInfoTipEvent
       read FOnInfoTip
       write FOnInfoTip;
    property OnVMGetItemInfo: TLVVMGetItemInfoEvent
       read FOnVMGetItemInfo
       write FOnVMGetItemInfo;
    property OnVMCacheHint: TLVVMCacheHintEvent
       read FOnVMCacheHint
       write FOnVMCacheHint;
    property OnVMFindItem: TLVVMFindItemEvent
       read FOnVMFindItem
       write FOnVMFindItem;
    property OnVMStateChanged: TLVVMStateChangedEvent
       read FOnVMStateChanged
       write FOnVMStateChanged;
    property OnVMCaptionEdited: TLVVMCaptionEditedEvent
       read FOnVMCaptionEdited
       write FOnVMCaptionEdited;
    property ShowSortArrows: boolean
       read GetShowSortArrows
       write SetShowSortArrows
       stored TRUE
       default FALSE;
    // Redeclare so we can reset checkboxes.
    property StateImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF}
       read GetStateImages
       write SetStateImages;
    // Redeclare so we can know when it changes and hook into it.
    property SmallImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF}
       read GetSmallImages
       write SetSmallImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Force reset of column image information
    procedure UpdateColumnsImages;
    procedure UpdateColumnImage(Index: integer);

    procedure SetIconSpacing(X, Y: integer);
    function GetSubItemAt(X, Y: integer): string;
    procedure SetColumnOrder(Count: integer; const IntArray: array of integer);
    function GetColumnOrder(Count: integer;
       var IntArray: array of integer): boolean;
    function ApproximateViewRect(Count: integer;
       const Proposed: TPoint): TPoint;
    procedure SetItemCountEx(Count: integer; Flags: TLVItemCountFlags);
    function StoreSettings: boolean; override;
    function WriteSettings: boolean; override;
    function LoadSettings: boolean; override;
    function ReadSettings: boolean; override;
    function CheckComCtlVersion(MajorHi, MajorLo,
       MinorHi, MinorLo: word): boolean;

    procedure ELV_EditCaption(Item: integer);
    function ELV_GetNextItem(StartItem: integer; Direction: TSearchDirection;
       States: TItemStates): integer;
    procedure ELV_SetItemState(Index: integer; States: TItemStates;
       Setting: boolean);
  end;

  TdfsExtListView = class(TCustomExtListView)
  public
    property LastColumnClicked;
    property CurrentColumnWidth;
    property HeaderHandle;
    property SubItem_BoundsRect;
    property SubItem_IconRect;
    property SubItem_LabelRect;
    property SubItem_SelectBoundsRect;
    property HotItem;
    property HotCursor;
    property WorkArea;
    property IsChecked;
    property SubItem_ImageIndex;
    property SelectionMark;
    property ItemIndent;
    property CurrentSortAscending;
  published
    property Columns;
    property ColumnSearch;
    property HideSelection;
    property ExtendedStyles;
    property VirtualMode;
    property HoverTime;
    property RequireComCtlUpdate;
{$IFDEF BACKGROUND_FIXED}
    property BackgroundImage;
{$ENDIF}
    property NoColumnResize;
    property SaveSettings;
    property ColumnsFormat;
    // New Events
    property OnItemChecked;
    property OnMarqueeBegin;
    property OnItemActivate;
    property OnHotTrack;
    property OnInfoTip;
    property OnVMGetItemInfo;
    property OnVMCacheHint;
    property OnVMFindItem;
    property OnVMStateChanged;
    property OnVMCaptionEdited;
    property ShowSortArrows;


    // Publish inherited protected properties
    property AutoColumnSort;
    property AutoSortStyle;
    property AutoResort;
    property AutoSortAscending;
    property ReverseSortArrows;
    property Style;

    property OnDrawHeader;
    property OnMeasureItem;
    property OnDrawItem;
    property OnDrawSubItem;
    property OnAfterDefaultDrawItem;
    property OnSortItems;
    property OnSortBegin;
    property OnSortFinished;
    property OnEditCanceled;


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
{$IFDEF DFS_COMPILER_4_UP}
    property Constraints;
{$ENDIF}
    property OnClick;
    property OnDblClick;
    property Ctl3D;
    property DragMode;
{$IFDEF DFS_COMPILER_4_UP}
    property DragKind;
{$ENDIF}
    property ReadOnly
       default False;
    property Enabled;
    property Font;
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
{$IFDEF DFS_COMPILER_5_UP}
    property OnGetSubItemImage;
{$ENDIF}
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


{ You may find this function useful in install programs and such.  Example of
  usage is:
     if not CheckDLLVersion('COMCTL32.DLL', 4, 70, 0, 0) then ....
  which returns TRUE if COMCTL32.DLL is version 4.70.0.0 or higher. }
function CheckDLLVersion(const DLLName: string; MajorHi, MajorLo,
   MinorHi, MinorLo: word): boolean;


implementation

uses
{$IFDEF DFS_COMPILER_3_UP}
  ActiveX,
{$ELSE}
  OLE2, ExtColEd,
{$ENDIF}
  Registry;

function ListView_GetColumnEx(LVWnd: HWND; iCol: Integer;
   var pcol: TLVColumnEx): bool;
begin
  Result := bool(SendMessage(LVWnd, LVM_GETCOLUMN, iCol, LPARAM(@pcol)));
end;

function ListView_SetColumnEx(LVWnd: HWnd; iCol: Integer;
   const pcol: TLVColumnEx): Bool;
begin
  Result := bool(SendMessage(LVWnd, LVM_SETCOLUMN, iCol, Longint(@pcol)));
end;

function ListView_InsertColumnEx(LVWnd: HWND; iCol: Integer;
                                 const pcol: TLVColumnEx): Integer;
begin
  Result := SendMessage(LVWnd, LVM_INSERTCOLUMN, iCol, Longint(@pcol));
end;

function ListView_GetHeader(LVWnd: HWnd): HWnd;
begin
  Result := HWnd(SendMessage(LVWnd, LVM_GETHEADER, 0, 0));
end;

function ListView_SetIconSpacing(LVWnd: HWnd; cx, cy: integer): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_SETICONSPACING, 0, MAKELONG(cx,cy));
end;

function ListView_SetExtendedListViewStyle(LVWnd: HWnd; ExStyle: LPARAM): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, ExStyle);
end;

function ListView_GetExtendedListViewStyle(LVWnd: HWnd): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
end;

function ListView_GetSubItemRect(LVWnd: HWnd; ParentItem, SubItem,
   Code: integer; var Rect: TRect): boolean;
begin
  Rect.Top := SubItem;
  Rect.Left := Code;
  Result := (SendMessage(LVWnd, LVM_GETSUBITEMRECT, ParentItem,
     LPARAM(@Rect)) <> 0);
end;

{$IFNDEF DFS_C3D4COMMCTRL}
  // C3 & D4 users don't need this because their COMMCTRL.PAS file has it right
  // and they can simply use the existing TLVHitTestInfo and
  // ListView_SubItemHitTest
function ListView_SubItemHitTestEx(LVWnd: HWnd;
   var HitTestInfo: TLVHitTestInfoEx): integer;
begin
  Result := SendMessage(LVWnd, LVM_SUBITEMHITTEST, 0, LPARAM(@HitTestInfo));
end;
{$ENDIF}

function ListView_SetColumnOrderArray(LVWnd: HWnd; Count: integer;
                                      IntArray: PIntArray): boolean;
begin
  Result := (SendMessage(LVWnd, LVM_SETCOLUMNORDERARRAY, Count,
     LPARAM(IntArray)) <> 0);
end;

function ListView_GetColumnOrderArray(LVWnd: HWnd; Count: integer;
                                      IntArray: PIntArray): boolean;
begin
  Result := (SendMessage(LVWnd, LVM_GETCOLUMNORDERARRAY, Count,
     LPARAM(IntArray)) <> 0);
end;

function ListView_SetHotItem(LVWnd: HWnd; Item: integer): integer;
begin
  Result := SendMessage(LVWnd, LVM_SETHOTITEM, Item, 0);
end;

function ListView_GetHotItem(LVWnd: HWnd): integer;
begin
  Result := SendMessage(LVWnd, LVM_GETHOTITEM, 0, 0);
end;

function ListView_SetHotCursor(LVWnd: HWnd; Cursor: HCursor): HCursor;
begin
  Result := HCursor(SendMessage(LVWnd, LVM_SETHOTCURSOR, 0, LPARAM(Cursor)));
end;

function ListView_GetHotCursor(LVWnd: HWnd): HCursor;
begin
  Result := HCursor(SendMessage(LVWnd, LVM_GETHOTCURSOR, 0, 0));
end;

function ListView_ApproximateViewRect(LVWnd: HWnd; Width, Height,
   Count: integer): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_APPROXIMATEVIEWRECT, Count,
     MAKELPARAM(Width, Height));
end;

function ListView_SetWorkArea(LVWnd: HWnd; const Rect: TRect): boolean;
begin
  Result := (SendMessage(LVWnd, LVM_SETWORKAREA, 0, LPARAM(@Rect)) <> 0);
end;

function ListView_GetCheckState(LVWnd: HWnd; Index: UINT): boolean;
begin
  Result := (SendMessage(LVWnd, LVM_GETITEMSTATE, Index,
     LVIS_STATEIMAGEMASK) SHR 12)-1 <> 0;
end;

procedure ListView_SetCheckState(LVWnd: HWnd; Index: UINT; Checked: boolean);
const
  LVIS_UNCHECKED = $1000;
  LVIS_CHECKED = $2000;
var
  Data: integer;
begin
  if Checked then Data := LVIS_CHECKED
  else Data := LVIS_UNCHECKED;
  ListView_SetItemState(LVWnd, Index, Data, LVIS_STATEIMAGEMASK);
end;

procedure ListView_SetItemCountEx(LVWnd: HWnd; Items: integer; Flags: DWORD);
begin
  SendMessage(LVWnd, LVM_SETITEMCOUNT, Items, Flags);
end;

{$IFNDEF DFS_C3D4COMMCTRL}
function ListView_SetExtendedListViewStyleEx(LVWnd: HWnd; Mask: DWord;
   ExStyle: LPARAM): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_SETEXTENDEDLISTVIEWSTYLE, Mask, ExStyle);
end;
{$ENDIF}

function ListView_GetSelectionMark(LVWnd: HWnd): integer;
begin
  Result := SendMessage(LVWnd, LVM_GETSELECTIONMARK, 0, 0);
end;

function ListView_SetSelectionMark(LVWnd: HWnd; iIndex: integer): integer;
begin
  Result := SendMessage(LVWnd, LVM_SETSELECTIONMARK, 0, iIndex);
end;

{$IFNDEF DFS_C3D4COMMCTRL}
function ListView_SetHoverTime(LVWnd: HWnd; dwHoverTimeMS: DWORD): DWORD;
begin
  Result := SendMessage(LVWnd, LVM_SETHOVERTIME, 0, dwHoverTimeMs);
end;

function ListView_GetHoverTime(LVWnd: HWnd): DWORD;
begin
  Result := DWORD(SendMessage(LVWnd, LVM_GETHOVERTIME, 0, 0));
end;

function ListView_SetToolTips(LVWnd, NewWnd: HWnd): HWnd;
begin
  Result := SendMessage(LVWnd, LVM_SETTOOLTIPS, NewWnd, 0);
end;

function ListView_GetToolTips(LVWnd: HWnd): HWnd;
begin
  Result := SendMessage(LVWnd, LVM_GETTOOLTIPS, 0, 0);
end;

function ListView_SetBkImage(LVWnd: HWnd; plvbki: PLVBkImage): BOOL;
begin
  Result := (SendMessage(LVWnd, LVM_SETBKIMAGE, 0, LPARAM(plvbki)) <> 0);
end;

function ListView_GetBkImage(LVWnd: HWnd; plvbki: PLVBkImage): BOOL;
begin
  Result := (SendMessage(LVWnd, LVM_GETBKIMAGE, 0, LPARAM(plvbki)) <> 0);
end;
{$ENDIF}



{$IFDEF DFS_TRY_BACKGROUND_IMAGE}

constructor TELVBackgroundImage.Create(AOwner: TCustomExtListView);
begin
  inherited Create;
  FBrushBmp := TBitmap.Create;
  FOwningListView := AOwner;
end;

destructor TELVBackgroundImage.Destroy;
begin
  FBrushBmp.Free;
  inherited Destroy;
end;

procedure TELVBackgroundImage.Assign(Source: TPersistent);
begin
  if Source is TELVBackgroundImage then
  begin
    FFilename := TELVBackgroundImage(Source).Filename;
    FTile := TELVBackgroundImage(Source).Tile;
    FXOffsetPercent := TELVBackgroundImage(Source).XOffsetPercent;
    FYOffsetPercent := TELVBackgroundImage(Source).YOffsetPercent;
    ApplyToListView;
  end;
end;

procedure TELVBackgroundImage.SetFilename(const Val: string);
begin
  if FFilename <> Val then
    FFilename := Val;
  ApplyToListView;
end;

procedure TELVBackgroundImage.SetTile(Val: boolean);
begin
  if FTile <> Val then
    FTile := Val;
  ApplyToListView;
end;

procedure TELVBackgroundImage.SetXOffsetPercent(Val: integer);
begin
  if FXOffsetPercent <> Val then
    FXOffsetPercent := Val;
  ApplyToListView;
end;

procedure TELVBackgroundImage.SetYOffsetPercent(Val: integer);
begin
  if FYOffsetPercent <> Val then
    FYOffsetPercent := Val;
  ApplyToListView;
end;

procedure TELVBackgroundImage.ApplyToListView;
var
  LVBkImg: TLVBkImage;
begin
  if assigned(FOwningListView) and FOwningListView.HandleAllocated then
  begin
    if FFilename <> '' then
      LVBkImg.ulFlags := LVBKIF_SOURCE_URL
    else
      LVBkImg.ulFlags := LVBKIF_SOURCE_NONE;
    if FTile then
      LVBkImg.ulFlags := LVBkImg.ulFlags or LVBKIF_STYLE_TILE
    else
      LVBkImg.ulFlags := LVBkImg.ulFlags or LVBKIF_STYLE_NORMAL;
    LVBkImg.hbm := 0;
    LVBkImg.pszImage := PChar(FFilename);
    LVBkImg.cchImageMax := Length(FFilename);
    LVBkImg.xOffsetPercent := FXOffsetPercent;
    LVBkImg.yOffsetPercent := FYOffsetPercent;
    // Transparent
    ListView_SettExtBkColor(FOwningListView.Handle, $FFFFFFFF);
    ListView_SetBkImage(FOwningListView.Handle, @LVBkImg);
  end;
end;
{$ENDIF}


constructor TdfsExtLVSaveSettings.Create;
begin
  inherited Create;
  FSaveColumnOrder := TRUE;
end;

procedure TdfsExtLVSaveSettings.StoreColumnOrder(ColCount: integer;
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
  Reg := TRegIniFile.Create(RegistryKey);
  try
    Reg.WriteString('Columns', 'Order', s);
  finally
    Reg.Free;
  end;
end;

procedure TdfsExtLVSaveSettings.ReadColumnOrder(ColCount: integer;
   var IntArray: array of integer);
var
  Reg: TRegIniFile;
  x,y: integer;
  s: string;
begin
  if ColCount < 1 then exit;
  s := '';
  Reg := TRegIniFile.Create(RegistryKey);
  try
    s := Reg.ReadString('Columns', 'Order', '');
  finally
    Reg.Free;
  end;
  if s = '' then
  begin
    for x := 0 to ColCount-1 do
      IntArray[x] := x;
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
      IntArray[x] := 0;
    end;
    s := copy(s, y+1, length(s));
    if s = '' then break;
  end;
end;



// Override constructor to "zero out" our internal variable.
constructor TCustomExtListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItemCountEx := 0;
  FItemCountExFlags := [];
  FSelectionMark := -1;
  FHoverTime := -1;
  FExtendedStyles := [lvxInfoTip];
  FColumnOrder := NIL;
  FColumnOrderCount := 0;
  FRequireComCtlUpdate := FALSE;
  FSaveSettings := TdfsExtLVSaveSettings.Create;
  FColumnsFormatChangeLink := TChangeLink.Create;
  FColumnsFormatChangeLink.OnChange := ColumnHeaderImagesChange;
  FVirtualMode := FALSE;
  FColumnsFormat := TdfsExtListColumns.Create(Self);
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
  FBackgroundImage := TELVBackgroundImage.Create(Self);
{$ENDIF}
{$IFDEF DFS_COMPILER_4_UP}
  OnData := FeedOwnerDataMode;
{$ENDIF}
end;

destructor TCustomExtListView.Destroy;
begin
  FColumnsFormat.Free; { don't think i need this, it has an Owner property }
  FColumnsFormatChangeLink.Free;

  if FColumnOrder <> NIL then
    FreeMem(FColumnOrder, FColumnOrderCount * SizeOf(Integer));
  FRecreateStream.Free;
  FRecreateStream := NIL;

  inherited Destroy;

  FSaveSettings.Free;
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
  { Free after inherited because inherited calls DestroyWnd and it is needed
    until after that...}
  FBackgroundImage.Free;
{$ENDIF}
end;

procedure TCustomExtListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if FVirtualMode then
    Params.Style := Params.Style or LVS_OWNERDATA;
end;

procedure TCustomExtListView.CreateWnd;
begin
  inherited CreateWnd;

//  RestoreChecks;
  SetSelectionMark(FSelectionMark);
  SetHoverTime(FHoverTime);
  SetExtendedStyles(FExtendedStyles);
  if VirtualMode and (FItemCountEx > 0) then
    SetItemCountEx(FItemCountEx, FItemCountExFlags);

  if FColumnOrder <> NIL then
  begin
    SendMessage(Handle, LVM_SETCOLUMNORDERARRAY, FColumnOrderCount,
       LongInt(FColumnOrder));
    Refresh;
  end;
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
  FBackgroundImage.ApplyToListView;
{$ENDIF}
  if not (csLoading in ComponentState) then
  begin
    if (StateImages <> NIL) then
      ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) or
        LVIS_STATEIMAGEMASK);
    ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) or
      LVIS_OVERLAYMASK);
  end;
  RestoreChecks;
end;

procedure TCustomExtListView.Loaded;
begin
  inherited Loaded;

  HandleNeeded;
  UpdateColumnsImages;
  if StateImages <> NIL then
    ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) or
      LVIS_STATEIMAGEMASK);
  ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) or
    LVIS_OVERLAYMASK);
end;

// Subitem set to -1 means Caption Text
function TCustomExtListView.GetSubItemText(Index, SubItem: integer): string;
var
  x,
  ColCount: integer;
  ColArray: PIntArray;
begin
  // needs to account for modified column order
  Result := '';
  if Items[Index] = NIL then
    exit;

  ColCount := Columns.Count;
  if (SubItem + 2 > ColCount) then
  begin
    if SubItem < Items[Index].SubItems.Count then
      Result := Items[Index].SubItems[SubItem];
  end else begin
    GetMem(ColArray, SizeOf(Integer)*ColCount);
    try
      GetColumnOrder(ColCount, ColArray^);
      x := ColArray[SubItem+1];
      if x = 0 then
        Result := Items[Index].Caption
      else
        Result := Items[Index].SubItems[x-1];
    finally
      FreeMem(ColArray);
    end;
  end;
end;

function TCustomExtListView.GetActualColumn(Index: integer): TListColumn;
var
//  x,
  ColCount: integer;
  ColArray: PIntArray;
begin
  // account for modified column order

  // Delphi 2 and C++B 1 have a bug in TListColumn.GetWidth.  It returns zero
  // for the width if the handle hasn't been allocated yet instead of returning
  // the value of the internal storage variable like Delphi 3 does.  I've also
  // had some problems similar under Delphi 3, so I'm just always requiring the
  // handle to be valid.
  HandleNeeded;

  Result := NIL;
  ColCount := Columns.Count;
  if Index >= ColCount then
    exit;

  GetMem(ColArray, SizeOf(Integer)*ColCount);
  try
    GetColumnOrder(ColCount, ColArray^);
    Result := Columns[ColArray[Index]];
(* I must have been high
    for x := 0 to ColCount-1 do
      if ColArray[x] = Index then
      begin
        Result := Columns[ColArray[x]];
        exit;
      end;
*)
  finally
    FreeMem(ColArray);
  end;
end;

procedure TCustomExtListView.MeasureItem(var Height: UINT);
begin
  inherited MeasureItem(Height);
end;

procedure TCustomExtListView.DrawItem(var Canvas: TCanvas; Index: Integer;
   Rect: TRect; State: TOwnerDrawState; var DefaultDrawing,
   FullRowSelect: boolean);
begin
  { Default to whatever is in ExtendedStyles settings }
  FullRowSelect := lvxFullRowSelect in ExtendedStyles;
  inherited DrawItem(Canvas, Index, Rect, State, DefaultDrawing,
     FullRowSelect);
end;

procedure TCustomExtListView.DrawSubItem(Index, SubItem: Integer; Rect: TRect;
   State: TOwnerDrawState; var DefaultDrawing: boolean);
begin
  inherited DrawSubItem(Index, SubItem, Rect, State, DefaultDrawing);
end;

procedure TCustomExtListView.DefaultDrawHeader(var Canvas: TCanvas;
   Index: Integer; var Rect: TRect; Selected: boolean);
var
  TheColumn: TListColumn;
  ExtColumn: TdfsExtListColumn;
  ImageOffset: integer;
  Offset: integer;
  R, CR: TRect;
  Bmp: TBitmap;
begin

(******************************************************************************)
(* NOTE:  This method is overriden and replaced from the one in TdfsEnhListView. *)
(*   That means that if changes are made here, they will also need to be made *)
(*   in EnhListView.pas' DefaultDrawHeader method.                            *)
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
//    TheColumn := ActualColumn[Index];

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

    if (Index < ColumnsFormat.Count) and assigned(SmallImages) and
       ((ColumnsFormat[Index].ImageIndex >= 0) and
       (ColumnsFormat[Index].ImageIndex < SmallImages.Count)) then
      ExtColumn := ColumnsFormat[Index]
    else
      ExtColumn := NIL;

    if assigned(ExtColumn) then
    begin
      case ExtColumn.ImageAlignment of
        ciaLeftOfText:
          Inc(R.Left, SmallImages.Width + 4);
        ciaRightOfText:
          Dec(R.Right, SmallImages.Width + 4);
      end;
    end;

    if ShowSortArrows and (LastColumnClicked = Index) and
       ((AutoColumnSort <> acsNoSort) or (assigned(OnSortItems))) then
    begin
      if CurrentSortAscending then
        Bmp := SortUpBmp
      else
        Bmp := SortDownBmp;

      Dec(R.Right, Bmp.Width + 8);
      if R.Right < R.Left then
        R.Right := R.Left;

      { How big of a rectangle do we have to work with for the Text? }
      CR := R;
      DrawTextEx(Canvas.Handle, PChar(TheColumn.Caption), -1, CR,
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
      DrawTextEx(Canvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], NIL);

      // Draw column image if we have one
      if assigned(ExtColumn) then
      begin
        ImageOffset := (Rect.Bottom - Rect.Top - SmallImages.Height) div 2;
        case ExtColumn.ImageAlignment of
          ciaLeftOfText:
            SmallImages.Draw(Canvas, R.Left - (SmallImages.Width + 4),
               R.Top + ImageOffset, ExtColumn.ImageIndex);
          ciaRightOfText:
            begin
              SmallImages.Draw(Canvas, R.Right + 4, R.Top + ImageOffset,
                 ExtColumn.ImageIndex);
              inc(R.Right, SmallImages.Width);
              if R.Right > Rect.Right then
                R.Right := Rect.Right;
            end;
        end;
      end;

      // Draw the sort arrow bitmap
      Offset := (Rect.Bottom - Rect.Top - Bmp.Height) div 2;
      // Only draw if we have enough room
      if (R.Right + Bmp.Width + 8) <= Rect.Right then
        Canvas.Draw(R.Right + 8, R.Top + Offset, Bmp);
    end else begin
      if Selected then
        OffsetRect(R, 1, 1);
      CR := R;

      DrawTextEx(Canvas.Handle, PChar(TheColumn.Caption), -1, CR,
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

      DrawTextEx(Canvas.Handle, PChar(TheColumn.Caption), -1, R,
         DRAWTEXTEX_FLAGS or DRAWTEXTEX_ALIGNMENT[TheColumn.Alignment], NIL);

      // Draw column image if we have one
      if assigned(ExtColumn) then
      begin
        ImageOffset := (Rect.Bottom - Rect.Top - SmallImages.Height) div 2;
        case ExtColumn.ImageAlignment of
          ciaLeftOfText:
            // Only draw if we have enough room
            if (R.Left - (SmallImages.Width + 4)) >= Rect.Left then
              SmallImages.Draw(Canvas, R.Left - (SmallImages.Width + 4),
                 R.Top + ImageOffset, ExtColumn.ImageIndex);
          ciaRightOfText:
            // Only draw if we have enough room
            if (R.Right + SmallImages.Width + 4) <= Rect.Right then
              SmallImages.Draw(Canvas, R.Right + 4, R.Top + ImageOffset,
                 ExtColumn.ImageIndex);
        end;
      end;
    end;
  end;
end;


const
  {$EXTERNALSYM LVS_EX_LABELTIP}        // [dpv]
  LVS_EX_LABELTIP        = $00004000;

  API_STYLES: array[Low(TLVExtendedStyle)..High(TLVExtendedStyle)] of LPARAM = (
     LVS_EX_GRIDLINES, LVS_EX_SUBITEMIMAGES, LVS_EX_CHECKBOXES,
     LVS_EX_TRACKSELECT, LVS_EX_HEADERDRAGDROP, LVS_EX_FULLROWSELECT,
     LVS_EX_ONECLICKACTIVATE, LVS_EX_TWOCLICKACTIVATE, LVS_EX_FLATSB,
     LVS_EX_INFOTIP, LVS_EX_UNDERLINEHOT, LVS_EX_UNDERLINECOLD, LVS_EX_LABELTIP);
     // LVS_EX_REGIONAL, LVS_EX_MULTIWORKAREAS - not implemented

// Function to convert our style set type into the value expected by the API.
function TCustomExtListView.SetValueToAPIValue(Styles: TLVExtendedStyles): LPARAM;
var
  x: TLVExtendedStyle;
begin
  Result := 0;
  { Check for each possible style. }
  for x := Low(TLVExtendedStyle) to High(TLVExtendedStyle) do
    { If the style is set... }
    if x in Styles then
      { OR the appropriate value into the result. }
      Result := Result OR API_STYLES[x];
end;

// Function to convert from the API values to our style set type.
function TCustomExtListView.SetValueFromAPIValue(Styles: DWORD): TLVExtendedStyles;
var
  x: TLVExtendedStyle;
begin
  Result := [];
  { Check for each possible style. }
  for x := Low(TLVExtendedStyle) to High(TLVExtendedStyle) do
    { If the style is set... }
    if (API_STYLES[x] and Styles) <> 0 then
      { OR the appropriate value into the result. }
      Result := Result + [x];
end;

// Property method to get the extended style bits.
function TCustomExtListView.GetExtendedStyles: TLVExtendedStyles;
begin
  if HandleAllocated then
    FExtendedStyles :=
       SetValueFromAPIValue(ListView_GetExtendedListViewStyle(Handle));
  Result := FExtendedStyles;
end;

// Property method to set new style bits.
procedure TCustomExtListView.SetExtendedStyles(Val: TLVExtendedStyles);
begin
  { Update the window with the new styles. }
  if (Val * [lvxUnderlineHot, lvxUnderlineCold] <> []) then
  begin
    // lvxUnderlineHot and lvxUnderlineCold require lvxOneClickActivate and/or
    // lvxTwoClickActivate
    if (lvxUnderlineCold in Val) and (not (lvxOneClickActivate in Val)) then
      Include(Val, lvxOneClickActivate);
    if (lvxUnderlineHot in Val) and
       (Val * [lvxOneClickActivate, lvxTwoClickActivate] = []) then
      Include(Val, lvxOneClickActivate);
  end;

  // A real world use of XOR!!!  We need to invalidate if subitem images is in
  // new value and not in old, or in old value and not in new, but NOT if it is
  // set or cleared in both.
  if ((lvxSubItemImages in Val) xor (lvxSubItemImages in FExtendedStyles)) and
    (HandleAllocated) then
    Invalidate;
  FExtendedStyles := Val;
  if HandleAllocated then
    ListView_SetExtendedListViewStyle(Handle, SetValueToAPIValue(Val));
end;

function TCustomExtListView.GetHeaderHandle: HWnd;
begin
  if FHeaderHandle <> 0 then
    Result := FHeaderHandle
  else begin
    if HandleAllocated then
      Result := ListView_GetHeader(Handle)
    else
      Result := 0;
  end;
end;

procedure TCustomExtListView.SetIconSpacing(X, Y: integer);
begin
// Not sure about how to update the view after changing this.  Refresh doesn't
// do the job.  Seems the best way to do it is in client code, something like:
(*
  SetIconSpacing(X, Y);
  // Does strange things if ViewStyle is not set to vsIcon!
  if ViewStyle = vsIcon then
  begin
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    try
      ViewStyle := vsSmallIcon;
      ViewStyle := vsIcon;
    finally
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  end;
*)

  if HandleAllocated then
    if ViewStyle = vsIcon then
      ListView_SetIconSpacing(Handle, X, Y);
end;

function TCustomExtListView.GetSubItemRect(Item, SubItem: integer;
   Index: integer): TRect;
begin
  if HandleAllocated then
    ListView_GetSubItemRect(Handle, Item, SubItem, Index, Result);
end;

function TCustomExtListView.GetSubItemAt(X, Y: integer): string;
var
{$IFNDEF DFS_C3D4COMMCTRL}
  Info: TLVHitTestInfoEx;
{$ELSE}
  Info: TLVHitTestInfo;
{$ENDIF}
begin
  Result := '';
  if HandleAllocated then
  begin
    Info.pt := Point(X, Y);
{$IFNDEF DFS_C3D4COMMCTRL}
    if ListView_SubItemHitTestEx(Handle, Info) <> -1 then
{$ELSE}
    if ListView_SubItemHitTest(Handle, @Info) <> -1 then
{$ENDIF}
    begin
      if (Info.iItem > -1) and (Items[Info.iItem] <> NIL) then
      begin
        if Info.iSubItem = 0 then
          Result := Items[Info.iItem].Caption
        else
          Result := Items[Info.iItem].SubItems[Info.iSubItem-1];
      end;
    end;
  end;
end;

procedure TCustomExtListView.SetColumnOrder(Count: integer; const IntArray:
   array of integer);
begin
  if FColumnOrder <> NIL then
    FreeMem(FColumnOrder, FColumnOrderCount * SizeOf(Integer));
  FColumnOrderCount := Count;
  GetMem(FColumnOrder, FColumnOrderCount * SizeOf(Integer));
  Move(IntArray, FColumnOrder^, FColumnOrderCount * SizeOf(Integer));
  if HandleAllocated then
  begin
    ListView_SetColumnOrderArray(Handle, Count, @IntArray);
    Refresh;
  end;
end;

function TCustomExtListView.GetColumnOrder(Count: integer;
                                     var IntArray: array of integer): boolean;
begin
  if HandleAllocated then
  begin
    if Count <> FColumnOrderCount then
    begin
      FColumnOrderCount := Count;
      if FColumnOrder <> NIL then
        FreeMem(FColumnOrder, FColumnOrderCount * SizeOf(Integer));
      GetMem(FColumnOrder, FColumnOrderCount * SizeOf(Integer));
    end;
    Result := ListView_GetColumnOrderArray(Handle, FColumnOrderCount,
       @IntArray);
    Move(IntArray, FColumnOrder^, FColumnOrderCount * SizeOf(Integer));
  end else begin
    if FColumnOrder <> NIL then
    begin
      Move(FColumnOrder^, IntArray, Count * SizeOf(Integer));
      Result := TRUE;
    end else
      Result := FALSE;
  end;
end;

procedure TCustomExtListView.SetHotItem(Val: integer);
begin
  if HandleAllocated then
    ListView_SetHotItem(Handle, Val);
end;

function TCustomExtListView.GetHotItem: integer;
begin
  if HandleAllocated then
    Result := ListView_GetHotItem(Handle)
  else
    Result := -1;
end;

procedure TCustomExtListView.SetHotCursor(const Val: HCursor);
begin
  if HandleAllocated then
    ListView_SetHotCursor(Handle, Val);
end;

function TCustomExtListView.GetHotCursor: HCursor;
begin
  if HandleAllocated then
    Result := ListView_GetHotCursor(Handle)
  else
    Result := 0;
end;

function TCustomExtListView.ApproximateViewRect(Count: integer;
   const Proposed: TPoint): TPoint;
var
  Res: DWORD;
begin
  if HandleAllocated then
  begin
    Res := ListView_ApproximateViewRect(Handle, Proposed.X, Proposed.Y, Count);
    Result := Point(LoWord(Res), HiWord(Res));
  end else
    Result := Point(-1, -1);
end;

procedure TCustomExtListView.SetWorkArea(Rect: TRect);
begin
  if HandleAllocated then
    ListView_SetWorkArea(Handle, Rect);
end;

procedure TCustomExtListView.SetCheckState(Index: integer; Checked: boolean);
begin
  if HandleAllocated then
    ListView_SetCheckState(Handle, Index, Checked);
end;

function TCustomExtListView.GetCheckState(Index: integer): boolean;
begin
  if HandleAllocated then
    Result := ListView_GetCheckState(Handle, Index)
  else
    Result := FALSE;
end;

procedure TCustomExtListView.SetItemCountEx(Count: integer; Flags: TLVItemCountFlags);
var
  APIFlags: DWORD;
begin
  FItemCountEx := Count;
  FItemCountExFlags := Flags;
  if HandleAllocated then
  begin
    APIFlags := 0;
    if lvsicfNoInvalidateAll in Flags then
      APIFlags := LVSICF_NOINVALIDATEALL;
    if lvsicfNoScroll in Flags then
      APIFlags := APIFlags or LVSICF_NOSCROLL;
    ListView_SetItemCountEx(Handle, Count, APIFlags);
  end;
end;

procedure TCustomExtListView.SetVirtualMode(Val: boolean);
begin
  if Val = FVirtualMode then exit;
  FVirtualMode := Val;
  if Items <> NIL then
    Items.Clear;
  {$IFDEF DFS_COMPILER_4_UP}
  OwnerData := Val;
  {$ELSE}
  if HandleAllocated then
  begin
    RecreateWnd;
    HandleNeeded;
  end;
  {$ENDIF}
end;

function TCustomExtListView.GetItemIndent(Index: integer): Integer;
var
  APIItem: TLVItemEx;
begin
  HandleNeeded;
  { Which item do they want? }
  APIItem.iItem := Index;
  { Indenting is only supported for items, not subitems, by COMCTL32.DLL }
  APIItem.iSubItem := 0;
  { Tell it that only the iIndent value is to be set for the item so it }
  { leaves the rest of the stuff alone }
  APIItem.mask := LVIF_INDENT;
  { Get it. }
  if SendMessage(Handle, LVM_GETITEM, 0, LPARAM(@APIItem)) <> 0 then
    Result := APIItem.iIndent
  else
    Result := -1;
end;


procedure TCustomExtListView.SetItemIndent(Index: integer; Value: Integer);
var
  APIItem: TLVItemEx;
begin
  HandleNeeded;
  { Which item do they want? }
  APIItem.iItem := Index;
  { Indenting is only supported for items, not subitems, by COMCTL32.DLL }
  APIItem.iSubItem := 0;
  { Tell it that only the iIndent value is set for the item so it }
  { leaves the rest of the stuff alone }
  APIItem.mask := LVIF_INDENT;
  APIItem.iIndent := Value;
  { Set it. }
  if SendMessage(Handle, LVM_SETITEM, 0, LPARAM(@APIItem)) = 0 then
    messagebeep(1);
end;

procedure TCustomExtListView.ItemChecked(ItemIndex: integer; Checked: boolean);
begin
  if assigned(FOnItemChecked) then
    FOnItemChecked(Self, ItemIndex, Checked);
end;

procedure TCustomExtListView.CNNotify(var Message: TWMNotify);
var
  CallInherited: boolean;
  RenameText: string;
  RenameCanceled: boolean;
begin
  if Message.NMHdr = NIL then
  begin
    inherited;
    exit;
  end;

  with Message.NMHdr^ do
  begin
    Message.Result := 0;
    CallInherited := FALSE;
    case code of
      LVN_ITEMCHANGING:
        begin
          inherited;
          // make sure we're going to change, and the change involves the state
          with PNMListView(Pointer(Message.NMHdr))^ do
            if ((uChanged and LVIF_STATE) <> 0) and (Message.Result = 0) then
            begin
              FCheckedListItemIndex := iItem;
              FChecked := IsChecked[iItem];
            end;
        end;
      LVN_ITEMCHANGED:
        begin
          inherited;
          // See if an item's checked state changed
          with PNMListView(Pointer(Message.NMHdr))^ do
            if ((uChanged and LVIF_STATE) <> 0) and (iItem =
              FCheckedListItemIndex) and (FChecked <> IsChecked[iItem]) then
              ItemChecked(FCheckedListItemIndex, not FChecked);
        end;
      // We only want to handle LVN_ENDLABELEDIT when in virtual mode
      LVN_ENDLABELEDIT:
        if FVirtualMode then
        begin
          RenameCanceled  := PLVDispInfo(pointer(Message.NMHdr))^.item.pszText =
            NIL;
          if RenameCanceled then
            RenameText := ''
          else
            RenameText := StrPas(
              PLVDispInfo(pointer(Message.NMHdr))^.item.pszText);
          VMCaptionEdited(PLVDispInfo(pointer(Message.NMHdr))^.item.iItem,
            RenameCanceled, RenameText);
        end
        else
          CallInherited := TRUE;
      // We only want to handle LVN_GETDISPINFO when in virtual mode
      LVN_GETDISPINFO:
        if FVirtualMode then
          VMGetDispInfo(PLVDispInfoEx(pointer(Message.NMHdr))^.item)
        else
          CallInherited := TRUE;
      LVN_ODCACHEHINT:
        VMCacheHint(PNMCacheHint(pointer(Message.NMHdr))^);
      LVN_ODSTATECHANGED:
        VMStateChanged(PNMODStateChange(pointer(Message.NMHdr))^);
      LVN_ODFINDITEM:
        Message.Result := VMFindItem(PNMFindItem(pointer(Message.NMHdr))^);

      LVN_ITEMACTIVATE:
        begin
          ItemActivate;
          Message.Result := 0;
        end;
      LVN_MARQUEEBEGIN:
        begin
          if MarqueeBegin then
            Message.Result := 0
          else
            Message.Result := 1;
        end;
      LVN_HOTTRACK:
        begin
          if HotTrack(PNMListView(Message.NMHdr)^) then
            Message.Result := 0
          else
            Message.Result := 1;
        end;
      LVN_GETINFOTIP:
        begin
          GetInfoTip(PNMLVGetInfoTip(Message.NMHdr));
          SetWindowPos(Handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or
          SWP_NOSIZE or SWP_NOMOVE);
          Message.Result := 1;
        end;
    else
      CallInherited := TRUE;
    end;
  end;
  if CallInherited then
    inherited;
end;


procedure TCustomExtListView.WMNotify(var Message: TWMNotify);
var
  CallInherited: boolean;
  HTI: THDHitTestInfo;
  pt: TPoint;
  ExtColumn: TdfsExtListColumn;
begin
  CallInherited := TRUE;

  case Message.NMHdr.code of
    // For some reason, the SECOND time you drag a header width, it toasts the
    // bitmap.  I think it has to do with the TListView class somehow resetting
    // the column information, overwriting our information (image info).  Anyway,
    // catching the condition (begin column header drag) and resetting all the
    // extended information for that column fixes it.
    HDN_BEGINTRACK, HDN_BEGINTRACKW, // the ...W messages were necessary, why? I do not know.
    HDN_DIVIDERDBLCLICK, HDN_DIVIDERDBLCLICKW: // doubleclick also
    begin
      UpdateColumnImage(PHDNotify(Message.NMHdr).Item); // from above
      if HeaderHandle <> 0 then
      begin
        // get cursor position
        GetCursorPos(pt);
        // convert to coordinates on header control of the listview
        Windows.ScreentoClient(HeaderHandle,pt);
        // fill in hittest structure
        HTI.flags := HHT_ONHEADER Or HHT_ONDIVIDER;
        HTI.point.x := pt.x;
        HTI.point.y := pt.y;
        //  get the header's hit-test info
        SendMessage(HeaderHandle, HDM_HITTEST, LongInt(0),LongInt(@HTI));
        if (HTI.Item >=0) and (HTI.Item < ColumnsFormat.Count) then
          ExtColumn := ColumnsFormat[HTI.Item] // get the extended format
        else
          ExtColumn := NIL;
        if Assigned(ExtColumn) then // found one?
          if not ExtColumn.AllowResize then
          begin
            CallInherited := FALSE; // Do not pass this msg
            Message.Result := 1;  // Message stopped, do not handle the resize event
          end;
      end;
    end;

{$IFDEF DFS_COMPILER_4_UP}
    // Delphi 4 re-orders column information on this notification to adjust for
    // column headers that have been moved to new locations.  That breaks this
    // component since it already adjusts for that, but in a different way.
    // Since this is a notification message, we don't need to pass it on to
    // inherited unless it needs it for something specific.  In this case, the
    // only thing this notification is used for is to kick off the order
    // adjustment, so we'll just eat the message.
    HDN_ENDDRAG:
      begin
        CallInherited := FALSE; // Don't pass it on!
        // Since we aren't letting the default handler have it, we must
        // invalidate the area our self so the items get redrawn with the new
        // column order applied.
        Invalidate;
      end;
{$ENDIF}
  end;

  if CallInherited then
    inherited;
end;


function TCustomExtListView.MarqueeBegin: boolean;
begin
  Result := TRUE;
  if assigned(FOnMarqueeBegin) then
    FOnMarqueeBegin(Self, Result);
end;

procedure TCustomExtListView.ItemActivate;
begin
  if assigned(FOnItemActivate) then
    FOnItemActivate(Self);
end;

function TCustomExtListView.HotTrack(var Item: TNMListView): boolean;
begin
  Result := TRUE;
  if assigned(FOnHotTrack) then
    FOnHotTrack(Self, Item.iItem, Item.iSubItem, Item.ptAction, Result);
end;

procedure TCustomExtListView.GetInfoTip(InfoTip: PNMLVGetInfoTip);
var
  Current,
  Additional: string;
begin
  if assigned(InfoTip) then
  begin
    if assigned(FOnInfoTip) then
    begin
      if InfoTip^.dwFlags = LVGIT_UNFOLDED then
        Current := string(InfoTip^.pszText)
      else
        Current := '';
      FOnInfoTip(Self, InfoTip^.iItem, InfoTip^.iSubItem, Current, Additional);
      if Additional <> '' then
      begin
        if InfoTip^.dwFlags = LVGIT_UNFOLDED then
          StrLCat(InfoTip^.pszText, PChar(Additional), InfoTip^.cchTextMax)
        else
          StrLCopy(InfoTip^.pszText, PChar(Additional), InfoTip^.cchTextMax);
      end;
    end
    else if not VirtualMode then
    begin
      if InfoTip^.dwFlags = LVGIT_UNFOLDED then
        StrLCat(InfoTip^.pszText, PChar(Items[InfoTip^.iItem].Caption),
          InfoTip^.cchTextMax)
      else
        StrLCopy(InfoTip^.pszText, PChar(Items[InfoTip^.iItem].Caption),
          InfoTip^.cchTextMax);
    end;
  end;
end;

procedure TCustomExtListView.VMGetDispInfo(var ItemInfo: TLVItemEx);
  function MaskFlagsToSet(Mask: UINT): TLVVMMaskItems;
  begin
    Result := [];
    if (Mask and LVIF_TEXT) = LVIF_TEXT then
      Include(Result, lvifText);
    if (Mask and LVIF_IMAGE) = LVIF_IMAGE then
      Include(Result, lvifImage);
    if (Mask and LVIF_PARAM) = LVIF_PARAM then
      Include(Result, lvifParam);
    if (Mask and LVIF_STATE) = LVIF_STATE then
      Include(Result, lvifState);
    if (Mask and LVIF_INDENT) = LVIF_INDENT then
      Include(Result, lvifIndent);
  end;
  function SetToMaskFlags(Mask: TLVVMMaskItems): UINT;
  begin
    Result := 0;
    if lvifText in Mask then
      Result := Result or LVIF_TEXT;
    if lvifImage in Mask then
      Result := Result or LVIF_IMAGE;
    if lvifParam in Mask then
      Result := Result or LVIF_PARAM;
    if lvifState in Mask then
      Result := Result or LVIF_STATE;
    if lvifIndent in Mask then
      Result := Result or LVIF_INDENT;
  end;
(*  function StateMaskFlagsToSet(Mask: UINT): TLVVMStateMaskItems;
  begin
    Result := [];
    if (Mask and LVIS_CUT) = LVIS_CUT then
      Include(Result, lvisCut);
    if (Mask and LVIS_DROPHILITED) = LVIS_DROPHILITED then
      Include(Result, lvisDropHighlighted);
    if (Mask and LVIS_FOCUSED) = LVIS_FOCUSED then
      Include(Result, lvisFocused);
    if (Mask and LVIS_SELECTED) = LVIS_SELECTED then
      Include(Result, lvisSelected);
    if (Mask and LVIS_OVERLAYMASK) = LVIS_OVERLAYMASK then
      Include(Result, lvisOverlayMask);
  end;*)
var
  Text: string;
  NewState: UINT;
  NewStateMask: UINT;
  GetMask: TLVVMMaskItems;
  OverlayImage, StateImage: word;
begin
  if ItemInfo.iItem = -1 then exit;  // No way.
  Text := '';
  NewState := ItemInfo.State;
  NewStateMask := ItemInfo.StateMask;
  GetMask := MaskFlagsToSet(ItemInfo.Mask);

  if assigned(FOnVMGetItemInfo) then
  begin
    OverlayImage := 0;
    StateImage := 0;
    with ItemInfo do
      FOnVMGetItemInfo(Self, iItem, iSubItem, GetMask,
       iImage, OverlayImage, StateImage, lParam, NewState, NewStateMask,
       iIndent, Text);
    if (ItemInfo.mask and LVIF_TEXT) = LVIF_TEXT then
      StrLCopy(ItemInfo.pszText, PChar(Text), ItemInfo.cchTextMax);
    ItemInfo.State := NewState;
    ItemInfo.State := ItemInfo.State and not UINT(IndexToOverlayMask($FFFF));
    ItemInfo.State := ItemInfo.State or UINT(IndexToOverlayMask(OverlayImage));
    ItemInfo.State := ItemInfo.State and not UINT(IndexToStateImageMask($FFFF));
    ItemInfo.State := ItemInfo.State or UINT(IndexToStateImageMask(StateImage));
    ItemInfo.Mask := SetToMaskFlags(GetMask);
    ItemInfo.StateMask := NewStateMask;
  end;
end;

procedure TCustomExtListView.VMCacheHint(var HintInfo: TNMCacheHint);
begin
  if assigned(FOnVMCacheHint) then
    FOnVMCacheHint(Self, HintInfo);
end;

function TCustomExtListView.VMFindItem(var FindInfo: TNMFindItem): integer;
begin
  Result := -1;
  if assigned(FOnVMFindItem) then
    FOnVMFindItem(Self, FindInfo, Result);
end;

procedure TCustomExtListView.VMStateChanged(var StateInfo: TNMODStateChange);
begin
  if assigned(FOnVMStateChanged) then
    FOnVMStateChanged(Self, StateInfo);
end;

procedure TCustomExtListView.VMCaptionEdited(Item: integer; Canceled: boolean;
  const Text: string);
begin
  if assigned(FOnVMCaptionEdited) then
    FOnVMCaptionEdited(Self, Item, Canceled, Text);
end;

function TCustomExtListView.WriteSettings: boolean;
var
  x,
  ColCount: integer;
  ColArray: PIntArray;
begin
  Result := TRUE;
  ColCount := Columns.Count;
  if (FSaveSettings.SaveColumnOrder or FSaveSettings.SaveColumnSizes) and
     (ColCount > 0) then
  begin
    GetMem(ColArray, SizeOf(Integer)*ColCount);
    try
      if FSaveSettings.SaveColumnOrder then
      begin
        GetColumnOrder(ColCount, ColArray^);
        FSaveSettings.StoreColumnOrder(ColCount, ColArray^);
      end;
      if FSaveSettings.SaveColumnSizes then
      begin
        for x := 0 to ColCount-1 do
          ColArray[x] := ActualColumn[x].Width;
        FSaveSettings.StoreColumnSizes(ColCount, ColArray^);
      end;
    finally
      FreeMem(ColArray);
    end;
  end;
  if FSaveSettings.SaveCurrentSort then
    FSaveSettings.StoreCurrentSort(CurrentSortAscending, LastColumnClicked);
  if FSaveSettings.SaveViewStyle then
    FSaveSettings.StoreViewStyle(ViewStyle);
end;

function TCustomExtListView.StoreSettings: boolean;
begin
  // DON'T CALL INHERITED!!!!  It has caused me no end of trouble, so I
  // just resave the width stuff if I need to rather than call inherited.

  if FSaveSettings.AutoSave and
     ((([csDesigning, csLoading, csReading] * ComponentState) = []) or
     (csDestroying in ComponentState)) then
    Result := WriteSettings
  else
    Result := FALSE;
end;

function TCustomExtListView.ReadSettings: boolean;
var
  x,
  ColCount: integer;
  ColArray: PIntArray;
  SortCol: integer;
  SortAscending: boolean;
begin
  Result := TRUE;
  ColCount := Columns.Count;
  if (FSaveSettings.SaveColumnOrder or FSaveSettings.SaveColumnSizes) and
     (ColCount > 0) then
  begin
    GetMem(ColArray, SizeOf(Integer)*ColCount);
    try
      if FSaveSettings.SaveColumnOrder then
      begin
        FSaveSettings.ReadColumnOrder(ColCount, ColArray^);
        SetColumnOrder(ColCount, ColArray^);
      end;

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

function TCustomExtListView.LoadSettings: boolean;
begin
  if FSaveSettings.AutoSave and (not(csDesigning in ComponentState)) then
    Result := ReadSettings
  else
    Result := FALSE;
end;


function TCustomExtListView.ELV_GetNextItem(StartItem: integer;
   Direction: TSearchDirection; States: TItemStates): integer;
var
  Flags: Integer;
begin
  Result := -1;
  if HandleAllocated then
  begin
    Flags := 0;
    case Direction of
      sdAbove: Flags := LVNI_ABOVE;
      sdBelow: Flags := LVNI_BELOW;
      sdLeft: Flags := LVNI_TOLEFT;
      sdRight: Flags := LVNI_TORIGHT;
      sdAll: Flags := LVNI_ALL;
    end;
    if isCut in States then Flags := Flags or LVNI_CUT;
    if isDropHilited in States then Flags := Flags or LVNI_DROPHILITED;
    if isFocused in States then Flags := Flags or LVNI_FOCUSED;
    if isSelected in States then Flags := Flags or LVNI_SELECTED;
    Result := ListView_GetNextItem(Handle, StartItem, Flags);
  end;
end;

procedure TCustomExtListView.ELV_SetItemState(Index: integer;
   States: TItemStates; Setting: boolean);
var
  NewState,
  Flags: UINT;
begin
  if HandleAllocated then
  begin
    Flags := 0;
    if isCut in States then Flags := Flags or LVIS_CUT;
    if isDropHilited in States then Flags := Flags or LVIS_DROPHILITED;
    if isFocused in States then Flags := Flags or LVIS_FOCUSED;
    if isSelected in States then Flags := Flags or LVIS_SELECTED;
    {$IFDEF DFS_COMPILER_4}
    if isActivating in States then Flags := Flags or LVIS_ACTIVATING;
    {$ENDIF}
    if Setting then
      NewState := Flags
    else
      NewState := 0;
    ListView_SetItemState(Handle, Index, NewState, Flags);
  end;
end;

procedure TCustomExtListView.ELV_EditCaption(Item: integer);
begin
  if HandleAllocated then
    ListView_EditLabel(Handle, Item);
end;

procedure TCustomExtListView.ColumnHeaderImagesChange(Sender: TObject);
begin
  UpdateColumnsImages; { Images changed }
end;


procedure TCustomExtListView.SetColumnsFormat(Value: TdfsExtListColumns);
begin
  FColumnsFormat.Assign(Value);
end;

{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
procedure TCustomExtListView.SetBackgroundImage(Value: TELVBackgroundImage);
begin
  FBackgroundImage.Assign(Value);
end;
{$ENDIF}

function TCustomExtListView.GetSubItemImageIndex(Item, SubItem: integer): integer;
var
  APIItem: TLVItem;
begin
  HandleNeeded;

  Result := -1; // Assume the worst

  { Which item do they want? }
  APIItem.iItem := Item;
  { Subitem index, this is one-based, zero is the caption item }
  APIItem.iSubItem := SubItem+1;
  { Tell it that only the iImage value is to be set for the item so it }
  { leaves the rest of the stuff alone }
  APIItem.mask := LVIF_IMAGE;
  { Get it. }
  if ListView_GetItem(Handle, APIItem) then
    Result := APIItem.iImage;
end;

procedure TCustomExtListView.SetSubItemImageIndex(Item, SubItem, Value: integer);
var
  APIItem: TLVItem;
begin
  HandleNeeded;

  { Which item is it? }
  APIItem.iItem := Item;
  { Subitem index, this is one-based, zero is the caption item }
  APIItem.iSubItem := SubItem+1;
  { Tell it what image list index to use }
  APIItem.iImage := Value;
  { Tell it that only the iImage value is to be set for the item so it }
  { leaves the rest of the stuff in the item alone }
  APIItem.mask := LVIF_IMAGE;
  { Set it. }
  ListView_SetItem(Handle, APIItem);
end;

function TCustomExtListView.GetStateImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList; {$ELSE} TImageList; {$ENDIF}
begin
  // Nothing, just get it
  Result := inherited StateImages;
end;

procedure TCustomExtListView.SetStateImages(Value: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF});
begin
  SaveChecks;
  inherited StateImages := Value;
  if StateImages <> NIL then
    ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) or
      LVIS_STATEIMAGEMASK)
  else
    ListView_SetCallbackMask(Handle, ListView_GetCallbackMask(Handle) and not
      LVIS_STATEIMAGEMASK);
  RestoreChecks;
end;

function TCustomExtListView.GetSmallImages: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList; {$ELSE} TImageList; {$ENDIF}
begin
  // Nothing, just get it
  Result := inherited SmallImages;
end;

procedure TCustomExtListView.SetSmallImages(Value: {$IFDEF DFS_COMPILER_4_UP} TCustomImageList {$ELSE} TImageList {$ENDIF});
begin
  // Unlink ourself from old value
  if SmallImages <> NIL then
    SmallImages.UnregisterChanges(FColumnsFormatChangeLink);

  inherited SmallImages := Value;

  // Re-link to the new value.
  if SmallImages <> NIL then
    SmallImages.RegisterChanges(FColumnsFormatChangeLink);

  // Force the header to redraw
  SetColumnsOwnerDrawFlag(assigned(OnDrawHeader) or ShowSortArrows);
end;

function TCustomExtListView.GetSelectionMark: integer;
begin
  if HandleAllocated then
    FSelectionMark := ListView_GetSelectionMark(Handle);
  Result := FSelectionMark;
end;

procedure TCustomExtListView.SetSelectionMark(Val: integer);
begin
  if Val <> FSelectionMark then
    FSelectionMark := Val;
  // Call even if not changed because handle may not have been allocated
  if HandleAllocated then
    ListView_SetSelectionMark(Handle, Val);
end;

function TCustomExtListView.GetHoverTime: Longint;
begin
  if HandleAllocated then
    FHoverTime := Longint(ListView_GetHoverTime(Handle));
  Result := FHoverTime;
end;

procedure TCustomExtListView.SetHoverTime(Val: Longint);
begin
  if Val <> FHoverTime then
    FHoverTime := Val;
  // Call even if not changed because handle may not have been allocated last time
  if HandleAllocated then
    ListView_SetHoverTime(Handle, DWORD(Val));
end;

procedure TCustomExtListView.SetRequireComCtlUpdate(Value: boolean);
begin
  FRequireComCtlUpdate := Value;
  if FRequireComCtlUpdate and (not (csDesigning in ComponentState)) then
  begin
    if not CheckComCtlVersion(4, 70, 0, 0) then
      raise EELVOldComCtl.Create('COMCTL32.DLL is older than required version');
  end;
end;

procedure TCustomExtListView.UpdateColumnsImages;
var
  i: Integer;
begin
  if not (assigned(OnDrawHeader) or ShowSortArrows) then
    if HandleAllocated then
      for i := 0 to Columns.Count - 1 do UpdateColumnImage(i);
end;

procedure TCustomExtListView.UpdateColumnImage(Index: integer);
  function ValidImages: boolean;
  begin
    Result := assigned(SmallImages) and (SmallImages.Count > 0);
  end;
var
  Column: TLVColumnEx;
begin { UpdateColumnImage }
  if assigned(OnDrawHeader) or ShowSortArrows then
    exit;

  if HandleAllocated and (Index > -1) and (Index < FColumnsFormat.Count) and
     ValidImages then
  begin
    FillChar(Column, SizeOf(Column), #0);
    ListView_GetColumnEx(Handle, Index, Column);
    with Column, FColumnsFormat[Index] do
    begin
      if (ImageIndex <> -1) then
      begin
        iImage := ImageIndex;
        // Add LVCF_FMT Just to make sure...
        mask := mask or LVCF_IMAGE or LVCF_FMT;
        fmt  := fmt or LVCFMT_IMAGE;
        if ImageAlignment = ciaRightOfText then
          fmt := fmt or LVCFMT_BITMAP_ON_RIGHT;
      end else begin
        mask := LVCF_FMT;
        fmt  := fmt and not LVCFMT_IMAGE and not LVCFMT_BITMAP_ON_RIGHT;
      end;
      case Columns.Items[Index].Alignment of
        taLeftJustify: fmt := fmt or LVCFMT_LEFT;
        taCenter: fmt := fmt or LVCFMT_CENTER;
        taRightJustify: fmt := fmt or LVCFMT_RIGHT;
      end;
    end;
    ListView_SetColumnEx(Handle, Index, Column);
  end;
end;

function TCustomExtListView.CheckComCtlVersion(MajorHi, MajorLo,
   MinorHi, MinorLo: word): boolean;
begin
  Result := CheckDLLVersion('COMCTL32.DLL', MajorHi, MajorLo, MinorHi, MinorLo);
end;

function TCustomExtListView.GetShowSortArrows: boolean;
begin
  Result := inherited ShowSortArrows;
end;

procedure TCustomExtListView.SetShowSortArrows(Value: boolean);
begin
  inherited ShowSortArrows := Value;
  UpdateColumnsImages;
end;

function TCustomExtListView.ActualColumnIndex(Index: integer): integer;
var
  x,
  ColCount: integer;
  ColArray: PIntArray;
begin
  // account for modified column order

  // Delphi 2 and C++B 1 have a bug in TListColumn.GetWidth.  It returns zero
  // for the width if the handle hasn't been allocated yet instead of returning
  // the value of the internal storage variable like Delphi 3 does.  I've also
  // had some problems similar under Delphi 3, so I'm just always requiring the
  // handle to be valid.
  HandleNeeded;

  Result := 0;
  ColCount := Columns.Count;
  if Index >= ColCount then
    exit;

  GetMem(ColArray, SizeOf(Integer)*ColCount);
  try
    GetColumnOrder(ColCount, ColArray^);
    for x := 0 to ColCount-1 do
      if ColArray[x] = Index then
      begin
        Result := x;
        exit;
      end;
  finally
    FreeMem(ColArray);
  end;
end;

function TCustomExtListView.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TCustomExtListView.DefaultDrawSubItem(Index, SubItem: Integer;
   Rect: TRect; State: TOwnerDrawState);
var
  Dummy: boolean;
  SavedDC: integer;
begin
  if VirtualMode then
  begin
    SavedDC := SaveDC(FCanvas.Handle);
    try
      if not (csDesigning in ComponentState) then
        DrawSubItem(Index, SubItem, Rect, State, Dummy)
      else if Index = -1 then
        DrawTextEx(FCanvas.Handle, 'Virtual mode', -1, Rect, DRAWTEXTEX_FLAGS or
           DRAWTEXTEX_ALIGNMENT[ActualColumn[SubItem+1].Alignment], NIL);
    finally
      RestoreDC(FCanvas.Handle, SavedDC);
    end;
  end else
    inherited DefaultDrawSubItem(Index, SubItem, Rect, State);
end;

procedure TCustomExtListView.DefaultDrawItem(Index: Integer; Rect: TRect;
   State: TOwnerDrawState; FullRowSelect: boolean);
var
  Count: Integer;
  SubRect: TRect;
begin
  if VirtualMode then
  begin
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
      SubRect.Right := Rect.Left + CurrentColumnWidth[0] - 2;
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

        if ActualColumn[Count].Alignment = taLeftJustify then
        begin
          SubRect.Left := SubRect.Right;
          SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
          Inc(SubRect.Left, DefDraw_TextOffset)
        end else begin
          SubRect.Left := SubRect.Right + DefDraw_TextOffset;
          SubRect.Right := SubRect.Left + CurrentColumnWidth[Count];
          Dec(SubRect.Right, DefDraw_TextOffset);
        end;
        DefaultDrawSubItem(Index, Count-1, SubRect, State);
      end;
    end;
  end else
    inherited DefaultDrawItem(Index, Rect, State, FullRowSelect);
end;

procedure TCustomExtListView.RestoreChecks;
var
  i, q: Integer;
  Value: Boolean;
  SubItem: integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if FRecreateStream <> nil then
    begin
      FRecreateStream.Read(Value, SizeOf(Value));
      IsChecked[i] := Value;
      for q := 0 to Columns.Count - 2 do
      begin
        FRecreateStream.Read(SubItem, SizeOf(SubItem));
        SubItem_ImageIndex[i, q] := SubItem;
      end;
    end
    else
    begin
      IsChecked[i] := False;
      for q := 0 to Columns.Count - 2 do
        SubItem_ImageIndex[i, q] := ELV_NO_SUBITEM_IMAGE;
    end;
  end;
  FRecreateStream.Free;
  FRecreateStream := nil;
end;

procedure TCustomExtListView.SaveChecks;
var
  i, q: Integer;
  Value: Boolean;
  SubItem: integer;
begin
  if FRecreateStream = nil then
    FRecreateStream := TMemoryStream.Create
  else
    {$IFDEF DFS_COMPILER_2}
    FRecreateStream.Clear;
    {$ELSE}
    FRecreateStream.Size := 0;
    {$ENDIF}
  for i := 0 to Items.Count - 1 do
  begin
    Value := IsChecked[i];
    FRecreateStream.Write(Value, SizeOf(Value));
    for q := 0 to Columns.Count - 2 do
    begin
      SubItem := SubItem_ImageIndex[i, q];
      FRecreateStream.Write(SubItem, SizeOf(SubItem));
    end;
  end;
  FRecreateStream.Position := 0;
end;

procedure TCustomExtListView.DestroyWnd;
begin
  SaveChecks;
  inherited;
end;

{$IFDEF DFS_COMPILER_4_UP}
procedure TCustomExtListView.FeedOwnerDataMode(Sender: TObject; Item: TListItem);
var
  ItemData: TLVItemEx;
  x: integer;
begin
  if not FInhibitFeedData then // Try to prevent unnecessary calls
  begin
    ItemData.iItem := Item.Index;
    ItemData.iSubItem := 0;
    ItemData.mask := LVIF_TEXT or LVIF_IMAGE or LVIF_PARAM or LVIF_STATE or
      LVIF_INDENT;
    GetMem(ItemData.pszText, 1024);
    try
      VMGetDispInfo(ItemData);
      Item.Caption := ItemData.pszText;
      Item.ImageIndex := ItemData.iImage;
      Item.Data := pointer(ItemData.lParam);
      Item.Indent := ItemData.iIndent;
      ItemData.mask := LVIF_TEXT;
      for x := 1 to Columns.Count - 1 do
      begin
        VMGetDispInfo(ItemData);
        Item.SubItems.Add(ItemData.pszText);
      end;
    finally
      FreeMem(ItemData.pszText);
    end;
  end;
end;
{$ENDIF}


{ TdfsExtListColumn }

constructor TdfsExtListColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSmallImageIndex := -1;
  FImageAlignment := ciaRightOfText;
  FAllowResize := TRUE;
end;

destructor TdfsExtListColumn.Destroy;
begin
  FSmallImageIndex := -1;
  FImageAlignment := ciaRightOfText;

  inherited Destroy;
end;

procedure TdfsExtListColumn.DoChange;
var
  i: Integer;
begin
  for i := 0 to Collection.Count-1 do
    Changed(i <> Collection.Count);
end;

procedure TdfsExtListColumn.SetSmallImageIndex(Value: Integer);
begin
  if FSmallImageIndex <> Value then
  begin
    FSmallImageIndex := Value;
    DoChange;
  end;
end;

procedure TdfsExtListColumn.SetImageAlignment(Value: TColumnImageAlign);
begin
  if FImageAlignment <> Value then
  begin
    FImageAlignment := Value;
    DoChange;
  end;
end;

procedure TdfsExtListColumn.Assign(Source: TPersistent);
var
  Column: TdfsExtListColumn;
begin
  if Source is TdfsExtListColumn then
  begin
    Column := TdfsExtListColumn(Source);
    ImageIndex := Column.ImageIndex;
    ImageAlignment  := Column.ImageAlignment;
  end else
    inherited Assign(Source);
end;

constructor TdfsExtListColumns.Create(AOwner: TCustomExtListView);
begin
  inherited Create(TdfsExtListColumn);
  FOwner := AOwner;
end;

function TdfsExtListColumns.GetItem(Index: Integer): TdfsExtListColumn;
begin
  Result := TdfsExtListColumn(inherited GetItem(Index));
end;

procedure TdfsExtListColumns.SetItem(Index: Integer; Value: TdfsExtListColumn);
begin
  inherited SetItem(Index, Value);
end;

function TdfsExtListColumns.Add: TdfsExtListColumn;
begin
  Result := TdfsExtListColumn(inherited Add);
end;

function TdfsExtListColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TdfsExtListColumns.Update(Item: TCollectionItem);
begin
  if Owner <> NIL then
  begin
    if Item <> NIL then
      Owner.UpdateColumnImage(Item.Index)
    else
      Owner.UpdateColumnsImages;
  end;
end;

procedure TdfsExtListColumns.Refresh;
begin
  if Owner <> NIL then
    Owner.UpdateColumnsImages;
end;

procedure TdfsExtListColumns.Assign(Source: TPersistent);
begin
  Clear;
  inherited Assign(Source);
end;

function CheckDLLVersion(const DLLName: string; MajorHi, MajorLo,
   MinorHi, MinorLo: word): boolean;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  V1, V2, V3, V4: word;
begin
  Result := FALSE;
  VerInfoSize := GetFileVersionInfoSize(PChar(DLLName), Dummy);
  if VerInfoSize = 0 then
    exit;
  GetMem(VerInfo, VerInfoSize);
  if not assigned(VerInfo) then
    exit;
  try
    if GetFileVersionInfo(PChar(DLLName), 0, VerInfoSize, VerInfo) then
    begin
      if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then
      begin
        with VerValue^ do
        begin
          V1 := dwFileVersionMS shr 16;
          V2 := dwFileVersionMS and $FFFF;
          V3 := dwFileVersionLS shr 16;
          V4 := dwFileVersionLS and $FFFF;
        end;
        { This would be SO much easier with D4's int64 type... }
        if V1 < MajorHi then
          Result := FALSE
        else if V1 > MajorHi then
          Result := TRUE
        else begin
          if V2 < MajorLo then
            Result := FALSE
          else if V2 > MajorLo then
            Result := TRUE
          else begin
            if V3 < MinorHi then
              Result := FALSE
            else if V3 > MinorHi then
              Result := TRUE
            else begin
              if V4 < MinorLo then
                Result := FALSE
              else if V4 > MinorLo then
                Result := TRUE;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(VerInfo, VerInfoSize);
  end;
end;


initialization
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
//  OleInitialize(NIL);
  CoInitialize(NIL);
{$ENDIF}
finalization
{$IFDEF DFS_TRY_BACKGROUND_IMAGE}
//  OleUninitialize;
  CoUninitialize;
{$ENDIF}
end.

