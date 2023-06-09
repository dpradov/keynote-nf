
{*******************************************************}
{                                                       }
{       Delphi Run-time Library                         }
{       Windows 32bit API Interface Unit                }
{                                                       }
{       Copyright (c) 1996,97 Borland International     }
{                                                       }
{*******************************************************}


unit CommCtrl98;

// derived from Delphi 4's CommCtrl.pas unit

{$I DFS.inc}

{$ifdef DFS_COMPILER_3_UP}
  {$WEAKPACKAGEUNIT}
{$endif}

interface

uses
   Winapi.Messages,
   Winapi.Windows,
   {$ifdef DFS_COMPILER_3_UP}
   Winapi.ActiveX;
   {$else}
   OLE2;
   {$endif}

{ From prsht.h -- Interface for the Windows Property Sheet Pages }

const
  MAXPROPPAGES = 100;

  PSP_DEFAULT             = $00000000;
  PSP_DLGINDIRECT         = $00000001;
  PSP_USEHICON            = $00000002;
  PSP_USEICONID           = $00000004;
  PSP_USETITLE            = $00000008;
  PSP_RTLREADING          = $00000010;
  PSP_HASHELP             = $00000020;
  PSP_USEREFPARENT        = $00000040;
  PSP_USECALLBACK         = $00000080;
  PSP_PREMATURE           = $00000400;
  PSP_HIDEHEADER          = $00000800;
  PSP_USEHEADERTITLE      = $00001000;
  PSP_USEHEADERSUBTITLE   = $00002000;

  PSPCB_RELEASE           = 1;
  PSPCB_CREATE            = 2;

  PSH_DEFAULT             = $00000000;
  PSH_PROPTITLE           = $00000001;
  PSH_USEHICON            = $00000002;
  PSH_USEICONID           = $00000004;
  PSH_PROPSHEETPAGE       = $00000008;
  PSH_WIZARDHASFINISH     = $00000010;
  PSH_MULTILINETABS       = $00000010;
  PSH_WIZARD              = $00000020;
  PSH_USEPSTARTPAGE       = $00000040;
  PSH_NOAPPLYNOW          = $00000080;
  PSH_USECALLBACK         = $00000100;
  PSH_HASHELP             = $00000200;
  PSH_MODELESS            = $00000400;
  PSH_RTLREADING          = $00000800;
  PSH_WIZARDCONTEXTHELP   = $00001000;
  PSH_WIZARD97            = $00002000;
  PSH_WATERMARK           = $00008000;
  PSH_USEHBMWATERMARK     = $00010000;  // user pass in a hbmWatermark instead of pszbmWatermark
  PSH_USEHPLWATERMARK     = $00020000;  //
  PSH_STRETCHWATERMARK    = $00040000;  // stretchwatermark also applies for the header
  PSH_HEADER              = $00080000;
  PSH_USEHBMHEADER        = $00100000;
  PSH_USEPAGELANG         = $00200000;  // use frame dialog template matched to page

  PSCB_INITIALIZED  = 1;
  PSCB_PRECREATE    = 2;

  PSN_FIRST               = -200;
  PSN_LAST                = -299;

  PSN_SETACTIVE           = PSN_FIRST - 0;
  PSN_KILLACTIVE          = PSN_FIRST - 1;
  PSN_APPLY               = PSN_FIRST - 2;
  PSN_RESET               = PSN_FIRST - 3;
  PSN_HELP                = PSN_FIRST - 5;
  PSN_WIZBACK             = PSN_FIRST - 6;
  PSN_WIZNEXT             = PSN_FIRST - 7;
  PSN_WIZFINISH           = PSN_FIRST - 8;
  PSN_QUERYCANCEL         = PSN_FIRST - 9;
  PSN_GETOBJECT           = PSN_FIRST - 10;

  PSNRET_NOERROR              = 0;
  PSNRET_INVALID              = 1;
  PSNRET_INVALID_NOCHANGEPAGE = 2;

  PSM_SETCURSEL           = WM_USER + 101;
  PSM_REMOVEPAGE          = WM_USER + 102;
  PSM_ADDPAGE             = WM_USER + 103;
  PSM_CHANGED             = WM_USER + 104;
  PSM_RESTARTWINDOWS      = WM_USER + 105;
  PSM_REBOOTSYSTEM        = WM_USER + 106;
  PSM_CANCELTOCLOSE       = WM_USER + 107;
  PSM_QUERYSIBLINGS       = WM_USER + 108;
  PSM_UNCHANGED           = WM_USER + 109;
  PSM_APPLY               = WM_USER + 110;
  PSM_SETTITLE            = WM_USER + 111;
  PSM_SETTITLEW           = WM_USER + 120;
  PSM_SETWIZBUTTONS       = WM_USER + 112;
  PSM_PRESSBUTTON         = WM_USER + 113;
  PSM_SETCURSELID         = WM_USER + 114;
  PSM_SETFINISHTEXT       = WM_USER + 115;
  PSM_SETFINISHTEXTW      = WM_USER + 121;
  PSM_GETTABCONTROL       = WM_USER + 116;
  PSM_ISDIALOGMESSAGE     = WM_USER + 117;

  PSWIZB_BACK             = $00000001;
  PSWIZB_NEXT             = $00000002;
  PSWIZB_FINISH           = $00000004;
  PSWIZB_DISABLEDFINISH   = $00000008;

  PSBTN_BACK              = 0;
  PSBTN_NEXT              = 1;
  PSBTN_FINISH            = 2;
  PSBTN_OK                = 3;
  PSBTN_APPLYNOW          = 4;
  PSBTN_CANCEL            = 5;
  PSBTN_HELP              = 6;
  PSBTN_MAX               = 6;

  ID_PSRESTARTWINDOWS     = 2;
  ID_PSREBOOTSYSTEM       = ID_PSRESTARTWINDOWS or 1;

  WIZ_CXDLG               = 276;
  WIZ_CYDLG               = 140;

  WIZ_CXBMP               = 80;

  WIZ_BODYX               = 92;
  WIZ_BODYCX              = 184;

  PROP_SM_CXDLG           = 212;
  PROP_SM_CYDLG           = 188;

  PROP_MED_CXDLG          = 227;
  PROP_MED_CYDLG          = 215;

  PROP_LG_CXDLG           = 252;
  PROP_LG_CYDLG           = 218;

type
  HPropSheetPage = Pointer;

  PPropSheetPageA = ^TPropSheetPageA;
  PPropSheetPageW = ^TPropSheetPageW;
  PPropSheetPage = PPropSheetPageA;

  LPFNPSPCALLBACKA = function(Wnd: HWnd; Msg: Integer;
    PPSP: PPropSheetPageA): Integer stdcall;
  LPFNPSPCALLBACKW = function(Wnd: HWnd; Msg: Integer;
    PPSP: PPropSheetPageW): Integer stdcall;
  LPFNPSPCALLBACK = LPFNPSPCALLBACKA;
  TFNPSPCallbackA = LPFNPSPCALLBACKA;
  TFNPSPCallbackW = LPFNPSPCALLBACKW;
  TFNPSPCallback = TFNPSPCallbackA;

  _PROPSHEETPAGEA = record
    dwSize: Longint;
    dwFlags: Longint;
    hInstance: THandle;
    case Integer of
      0: (
        pszTemplate: PAnsiChar);
      1: (
        pResource: Pointer;
        case Integer of
          0: (
            hIcon: THandle);
          1: (
            pszIcon: PAnsiChar;
            pszTitle: PAnsiChar;
            pfnDlgProc: Pointer;
            lParam: Longint;
            pfnCallback: TFNPSPCallbackA;
            pcRefParent: PInteger;
            pszHeaderTitle: PAnsiChar;      // this is displayed in the header
            pszHeaderSubTitle: PAnsiChar)); //
  end;

  _PROPSHEETPAGEW = record
    dwSize: Longint;
    dwFlags: Longint;
    hInstance: THandle;
    case Integer of
      0: (
        pszTemplate: PWideChar);
      1: (
        pResource: Pointer;
        case Integer of
          0: (
            hIcon: THandle);
          1: (
            pszIcon: PWideChar;
            pszTitle: PWideChar;
            pfnDlgProc: Pointer;
            lParam: Longint;
            pfnCallback: TFNPSPCallbackW;
            pcRefParent: PInteger;
            pszHeaderTitle: PWideChar;      // this is displayed in the header
            pszHeaderSubTitle: PWideChar)); //
  end;
  _PROPSHEETPAGE = _PROPSHEETPAGEA;
  TPropSheetPageA = _PROPSHEETPAGEA;
  TPropSheetPageW = _PROPSHEETPAGEW;
  TPropSheetPage = TPropSheetPageA;
  PROPSHEETPAGEA = _PROPSHEETPAGEA;
  PROPSHEETPAGEW = _PROPSHEETPAGEW;
  PROPSHEETPAGE = PROPSHEETPAGEA;

  PFNPROPSHEETCALLBACK = function(Wnd: HWnd; Msg: Integer;
    LParam: Integer): Integer stdcall;
  TFNPropSheetCallback = PFNPROPSHEETCALLBACK;

  PPropSheetHeaderA = ^TPropSheetHeaderA;
  PPropSheetHeaderW = ^TPropSheetHeaderW;
  PPropSheetHeader = PPropSheetHeaderA;

  _PROPSHEETHEADERA = record
    dwSize: Longint;
    dwFlags: Longint;
    hwndParent: HWnd;
    hInstance: THandle;
    case Integer of
      0: (
	hIcon: THandle);
      1: (
	pszIcon: PAnsiChar;
	pszCaption: PAnsiChar;
	nPages: Integer;
	case Integer of
	  0: (
	    nStartPage: Integer);
	  1: (
	    pStartPage: PAnsiChar;
	    case Integer of
	      0: (
		ppsp: PPropSheetPageA);
	      1: (
		phpage: Pointer;
		pfnCallback: TFNPropSheetCallback;
                case Integer of
                  0: (
                    hbmWatermark: HBITMAP);
                  1: (
                    pszbmWatermark: PAnsiChar;
                    hplWatermark: HPALETTE;
                    // Header bitmap shares the palette with watermark
                    case Integer of
                      0: (
                        hbmHeader: HBITMAP);
                      1: (
                        pszbmHeader: PAnsiChar)))));
  end;

  _PROPSHEETHEADERW = record
    dwSize: Longint;
    dwFlags: Longint;
    hwndParent: HWnd;
    hInstance: THandle;
    case Integer of
      0: (
	hIcon: THandle);
      1: (
	pszIcon: PWideChar;
	pszCaption: PWideChar;
	nPages: Integer;
	case Integer of
	  0: (
	    nStartPage: Integer);
	  1: (
	    pStartPage: PWideChar;
	    case Integer of
	      0: (
		ppsp: PPropSheetPageW);
	      1: (
		phpage: Pointer;
		pfnCallback: TFNPropSheetCallback;
                case Integer of
                  0: (
                    hbmWatermark: HBITMAP);
                  1: (
                    pszbmWatermark: PWideChar;
                    hplWatermark: HPALETTE;
                    // Header bitmap shares the palette with watermark
                    case Integer of
                      0: (
                        hbmHeader: HBITMAP);
                      1: (
                        pszbmHeader: PWideChar)))));
  end;
  _PROPSHEETHEADER = _PROPSHEETHEADERA;
  TPropSheetHeaderA = _PROPSHEETHEADERA;
  TPropSheetHeaderW = _PROPSHEETHEADERW;
  TPropSheetHeader = TPropSheetHeaderA;

  LPFNADDPROPSHEETPAGE = function(hPSP: HPropSheetPage;
    lParam: Longint): BOOL stdcall;
  TFNAddPropSheetPage = LPFNADDPROPSHEETPAGE;

  LPFNADDPROPSHEETPAGES = function(lpvoid: Pointer; pfn: TFNAddPropSheetPage;
    lParam: Longint): BOOL stdcall;
  TFNAddPropSheetPages = LPFNADDPROPSHEETPAGES;

function CreatePropertySheetPageA(var PSP: TPropSheetPageA): HPropSheetPage; stdcall;
function CreatePropertySheetPageW(var PSP: TPropSheetPageW): HPropSheetPage; stdcall;
function CreatePropertySheetPage(var PSP: TPropSheetPage): HPropSheetPage; stdcall;
function DestroyPropertySheetPage(hPSP: HPropSheetPage): BOOL; stdcall;
function PropertySheetA(var PSH: TPropSheetHeaderA): Integer; stdcall;
function PropertySheetW(var PSH: TPropSheetHeaderW): Integer; stdcall;
function PropertySheet(var PSH: TPropSheetHeader): Integer; stdcall;

{ From commctrl.h }

type
  tagINITCOMMONCONTROLSEX = packed record
    dwSize: DWORD;             // size of this structure
    dwICC: DWORD;              // flags indicating which classes to be initialized
  end;
  PInitCommonControlsEx = ^TInitCommonControlsEx;
  TInitCommonControlsEx = tagINITCOMMONCONTROLSEX;

const
  ICC_LISTVIEW_CLASSES   = $00000001; // listview, header
  ICC_TREEVIEW_CLASSES   = $00000002; // treeview, tooltips
  ICC_BAR_CLASSES        = $00000004; // toolbar, statusbar, trackbar, tooltips
  ICC_TAB_CLASSES        = $00000008; // tab, tooltips
  ICC_UPDOWN_CLASS       = $00000010; // updown
  ICC_PROGRESS_CLASS     = $00000020; // progress
  ICC_HOTKEY_CLASS       = $00000040; // hotkey
  ICC_ANIMATE_CLASS      = $00000080; // animate
  ICC_WIN95_CLASSES      = $000000FF;
  ICC_DATE_CLASSES       = $00000100; // month picker, date picker, time picker, updown
  ICC_USEREX_CLASSES     = $00000200; // comboex
  ICC_COOL_CLASSES       = $00000400; // rebar (coolbar) control
  ICC_INTERNET_CLASSES   = $00000800;
  ICC_PAGESCROLLER_CLASS = $00001000; // page scroller
  ICC_NATIVEFNTCTL_CLASS = $00002000; // native font control

procedure InitCommonControls; stdcall;
function InitCommonControlsEx(var ICC: TInitCommonControlsEx): Bool; { Re-defined below }

const
  IMAGE_BITMAP = 0;

const
  ODT_HEADER              = 100;
  ODT_TAB                 = 101;
  ODT_LISTVIEW            = 102;


{ ====== Ranges for control message IDs ======================= }

const
  LVM_FIRST               = $1000;      { ListView messages }
  TV_FIRST                = $1100;      { TreeView messages }
  HDM_FIRST               = $1200;      { Header messages }
  TCM_FIRST               = $1300;      { Tab control messages }
  PGM_FIRST               = $1400;      { Pager control messages }
  CCM_FIRST               = $2000;      { Common control shared messages }

  CCM_SETBKCOLOR          = CCM_FIRST + 1; // lParam is bkColor

type
  tagCOLORSCHEME = packed record
    dwSize: DWORD;
    clrBtnHighlight: COLORREF;    // highlight color
    clrBtnShadow: COLORREF;       // shadow color
  end;
  PColorScheme = ^TColorScheme;
  TColorScheme = tagCOLORSCHEME;

const
  CCM_SETCOLORSCHEME      = CCM_FIRST + 2; // lParam is color scheme
  CCM_GETCOLORSCHEME      = CCM_FIRST + 3; // fills in COLORSCHEME pointed to by lParam
  CCM_GETDROPTARGET       = CCM_FIRST + 4;
  CCM_SETUNICODEFORMAT    = CCM_FIRST + 5;
  CCM_GETUNICODEFORMAT    = CCM_FIRST + 6;

  INFOTIPSIZE = 1024;  // for tooltips

{ ====== WM_NOTIFY codes (NMHDR.code values) ================== }

const
  NM_FIRST                 = 0-  0;       { generic to all controls }
  NM_LAST                  = 0- 99;

  LVN_FIRST                = 0-100;       { listview }
  LVN_LAST                 = 0-199;

  HDN_FIRST                = 0-300;       { header }
  HDN_LAST                 = 0-399;

  TVN_FIRST                = 0-400;       { treeview }
  TVN_LAST                 = 0-499;

  TTN_FIRST                = 0-520;       { tooltips }
  TTN_LAST                 = 0-549;

  TCN_FIRST                = 0-550;       { tab control }
  TCN_LAST                 = 0-580;

{ Shell reserved           (0-580) -  (0-589) }

  CDN_FIRST                = 0-601;       { common dialog (new) }
  CDN_LAST                 = 0-699;

  TBN_FIRST                = 0-700;       { toolbar }
  TBN_LAST                 = 0-720;

  UDN_FIRST                = 0-721;       { updown }
  UDN_LAST                 = 0-740;

  MCN_FIRST                = 0-750;       { monthcal }
  MCN_LAST                 = 0-759;

  DTN_FIRST                = 0-760;       { datetimepick }
  DTN_LAST                 = 0-799;

  CBEN_FIRST               = 0-800;       { combo box ex }
  CBEN_LAST                = 0-830;

  RBN_FIRST                = 0-831;       { coolbar }
  RBN_LAST                 = 0-859;

  IPN_FIRST               = 0-860;       { internet address }
  IPN_LAST                = 0-879;       { internet address }

  SBN_FIRST               = 0-880;       { status bar }
  SBN_LAST                = 0-899;

  PGN_FIRST               = 0-900;       { Pager Control }
  PGN_LAST                = 0-950;

  MSGF_COMMCTRL_BEGINDRAG     = $4200;
  MSGF_COMMCTRL_SIZEHEADER    = $4201;
  MSGF_COMMCTRL_DRAGSELECT    = $4202;
  MSGF_COMMCTRL_TOOLBARCUST   = $4203;


{ ====== Generic WM_NOTIFY notification codes ================= }

const
  NM_OUTOFMEMORY           = NM_FIRST-1;
  NM_CLICK                 = NM_FIRST-2;
  NM_DBLCLK                = NM_FIRST-3;
  NM_RETURN                = NM_FIRST-4;
  NM_RCLICK                = NM_FIRST-5;
  NM_RDBLCLK               = NM_FIRST-6;
  NM_SETFOCUS              = NM_FIRST-7;
  NM_KILLFOCUS             = NM_FIRST-8;
  NM_CUSTOMDRAW            = NM_FIRST-12;
  NM_HOVER                 = NM_FIRST-13;
  NM_NCHITTEST             = NM_FIRST-14;   // uses NMMOUSE struct
  NM_KEYDOWN               = NM_FIRST-15;   // uses NMKEY struct
  NM_RELEASEDCAPTURE       = NM_FIRST-16;
  NM_SETCURSOR             = NM_FIRST-17;   // uses NMMOUSE struct
  NM_CHAR                  = NM_FIRST-18;   // uses NMCHAR struct

type
  tagNMMOUSE = packed record
    hdr: TNMHdr;
    dwItemSpec: DWORD;
    dwItemData: DWORD;
    pt: TPoint;
    dwHitInfo: DWORD; // any specifics about where on the item or control the mouse is
  end;
  PNMMouse = ^TNMMouse;
  TNMMouse = tagNMMOUSE;

  PNMClick = ^TNMClick;
  TNMClick = tagNMMOUSE;

  // Generic structure to request an object of a specific type.
  tagNMOBJECTNOTIFY = packed record
    hdr: TNMHdr;
    iItem: Integer;
    piid: PGUID;
    pObject: Pointer;
    hResult: HRESULT;
    dwFlags: DWORD;    // control specific flags (hints as to where in iItem it hit)
  end;
  PNMObjectNotify = ^TNMObjectNotify;
  TNMObjectNotify = tagNMOBJECTNOTIFY;

  // Generic structure for a key
  tagNMKEY = packed record
    hdr: TNMHdr;
    nVKey: UINT;
    uFlags: UINT;
  end;
  PNMKey = ^TNMKey;
  TNMKey = tagNMKEY;

  // Generic structure for a character
  tagNMCHAR = packed record
    hdr: TNMHdr;
    ch: UINT;
    dwItemPrev: DWORD;     // Item previously selected
    dwItemNext: DWORD;     // Item to be selected
  end;
  PNMChar = ^TNMChar;
  TNMChar = tagNMCHAR;

{ ==================== CUSTOM DRAW ========================================== }

const
  // custom draw return flags
  // values under 0x00010000 are reserved for global custom draw values.
  // above that are for specific controls
  CDRF_DODEFAULT          = $00000000;
  CDRF_NEWFONT            = $00000002;
  CDRF_SKIPDEFAULT        = $00000004;

  CDRF_NOTIFYPOSTPAINT    = $00000010;
  CDRF_NOTIFYITEMDRAW     = $00000020;
  CDRF_NOTIFYSUBITEMDRAW  = $00000020;  // flags are the same, we can distinguish by context
  CDRF_NOTIFYPOSTERASE    = $00000040;
  CDRF_NOTIFYITEMERASE    = $00000080;

  // drawstage flags
  // values under = $00010000 are reserved for global custom draw values.
  // above that are for specific controls
  CDDS_PREPAINT           = $00000001;
  CDDS_POSTPAINT          = $00000002;
  CDDS_PREERASE           = $00000003;
  CDDS_POSTERASE          = $00000004;
  // the = $000010000 bit means it's individual item specific
  CDDS_ITEM               = $00010000;
  CDDS_ITEMPREPAINT       = CDDS_ITEM or CDDS_PREPAINT;
  CDDS_ITEMPOSTPAINT      = CDDS_ITEM or CDDS_POSTPAINT;
  CDDS_ITEMPREERASE       = CDDS_ITEM or CDDS_PREERASE;
  CDDS_ITEMPOSTERASE      = CDDS_ITEM or CDDS_POSTERASE;
  CDDS_SUBITEM            = $00020000;

  // itemState flags
  CDIS_SELECTED       = $0001;
  CDIS_GRAYED         = $0002;
  CDIS_DISABLED       = $0004;
  CDIS_CHECKED        = $0008;
  CDIS_FOCUS          = $0010;
  CDIS_DEFAULT        = $0020;
  CDIS_HOT            = $0040;
  CDIS_MARKED         = $0080;
  CDIS_INDETERMINATE  = $0100;

type
  tagNMCUSTOMDRAWINFO = packed record
    hdr: TNMHdr;
    dwDrawStage: DWORD;
    hdc: HDC;
    rc: TRect;
    dwItemSpec: DWORD;  // this is control specific, but it's how to specify an item.  valid only with CDDS_ITEM bit set
    uItemState: UINT;
    lItemlParam: LPARAM;
  end;
  PNMCustomDraw = ^TNMCustomDraw;
  TNMCustomDraw = tagNMCUSTOMDRAWINFO;

  tagNMTTCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    uDrawFlags: UINT;
  end;
  PNMTTCustomDraw = ^TNMTTCustomDraw;
  TNMTTCustomDraw = tagNMTTCUSTOMDRAW;

{ ====== IMAGE LIST =========================================== }

const
  CLR_NONE                = $FFFFFFFF;
  CLR_DEFAULT             = $FF000000;

type
  HIMAGELIST = THandle;

  _IMAGELISTDRAWPARAMS = packed record
    cbSize: DWORD;
    himl: HIMAGELIST;
    i: Integer;
    hdcDst: HDC;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    xBitmap: Integer;        // x offest from the upperleft of bitmap
    yBitmap: Integer;        // y offset from the upperleft of bitmap
    rgbBk: COLORREF;
    rgbFg: COLORREF;
    fStyle: UINT;
    dwRop: DWORD;
  end;
  PImageListDrawParams = ^TImageListDrawParams;
  TImageListDrawParams = _IMAGELISTDRAWPARAMS;

const
  
  ILC_MASK                = $0001;
  
  ILC_COLOR               = $00FE;
  
  ILC_COLORDDB            = $00FE;
  
  ILC_COLOR4              = $0004;
  
  ILC_COLOR8              = $0008;
  
  ILC_COLOR16             = $0010;
  
  ILC_COLOR24             = $0018;
  
  ILC_COLOR32             = $0020;
  
  ILC_PALETTE             = $0800;


function ImageList_Create(CX, CY: Integer; Flags: UINT;
  Initial, Grow: Integer): HIMAGELIST; stdcall;

function ImageList_Destroy(ImageList: HIMAGELIST): Bool; stdcall;

function ImageList_GetImageCount(ImageList: HIMAGELIST): Integer; stdcall;

function ImageList_SetImageCount(himl: HIMAGELIST; uNewCount: UINT): Integer; stdcall;

function ImageList_Add(ImageList: HIMAGELIST; Image, Mask: HBitmap): Integer; stdcall;

function ImageList_ReplaceIcon(ImageList: HIMAGELIST; Index: Integer;
  Icon: HIcon): Integer; stdcall;

function ImageList_SetBkColor(ImageList: HIMAGELIST; ClrBk: TColorRef): TColorRef; stdcall;

function ImageList_GetBkColor(ImageList: HIMAGELIST): TColorRef; stdcall;

function ImageList_SetOverlayImage(ImageList: HIMAGELIST; Image: Integer;
  Overlay: Integer): Bool; stdcall;


function ImageList_AddIcon(ImageList: HIMAGELIST; Icon: HIcon): Integer;

const
  
  ILD_NORMAL              = $0000;
  
  ILD_TRANSPARENT         = $0001;
  
  ILD_MASK                = $0010;
  
  ILD_IMAGE               = $0020;
  
  ILD_ROP                 = $0040;
  
  ILD_BLEND25             = $0002;
  
  ILD_BLEND50             = $0004;
  
  ILD_OVERLAYMASK         = $0F00;


function IndexToOverlayMask(Index: Integer): Integer;

const
  
  ILD_SELECTED            = ILD_BLEND50;
  
  ILD_FOCUS               = ILD_BLEND25;
  
  ILD_BLEND               = ILD_BLEND50;
  
  CLR_HILIGHT             = CLR_DEFAULT;


function ImageList_Draw(ImageList: HIMAGELIST; Index: Integer;
  Dest: HDC; X, Y: Integer; Style: UINT): Bool; stdcall;


function ImageList_Replace(ImageList: HIMAGELIST; Index: Integer;
  Image, Mask: HBitmap): Bool; stdcall;

function ImageList_AddMasked(ImageList: HIMAGELIST; Image: HBitmap;
  Mask: TColorRef): Integer; stdcall;

function ImageList_DrawEx(ImageList: HIMAGELIST; Index: Integer;
  Dest: HDC; X, Y, DX, DY: Integer; Bk, Fg: TColorRef; Style: Cardinal): Bool; stdcall;

function ImageList_DrawIndirect(pimldp: PImageListDrawParams): Integer; stdcall;

function ImageList_Remove(ImageList: HIMAGELIST; Index: Integer): Bool; stdcall;

function ImageList_GetIcon(ImageList: HIMAGELIST; Index: Integer;
  Flags: Cardinal): HIcon; stdcall;

function ImageList_LoadImageA(Instance: THandle; Bmp: PAnsiChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;

function ImageList_LoadImageW(Instance: THandle; Bmp: PWideChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;

function ImageList_LoadImage(Instance: THandle; Bmp: PChar; CX, Grow: Integer;
  Mask: TColorRef; pType, Flags: Cardinal): HIMAGELIST; stdcall;

const
  
  ILCF_MOVE   = $00000000;
  
  ILCF_SWAP   = $00000001;


function ImageList_Copy(himlDst: HIMAGELIST; iDst: Integer; himlSrc: HIMAGELIST;
  Src: Integer; uFlags: UINT): Integer; stdcall;


function ImageList_BeginDrag(ImageList: HIMAGELIST; Track: Integer;
  XHotSpot, YHotSpot: Integer): Bool; stdcall;

function ImageList_EndDrag: Bool; stdcall;

function ImageList_DragEnter(LockWnd: HWnd; X, Y: Integer): Bool; stdcall;

function ImageList_DragLeave(LockWnd: HWnd): Bool; stdcall;

function ImageList_DragMove(X, Y: Integer): Bool; stdcall;

function ImageList_SetDragCursorImage(ImageList: HIMAGELIST; Drag: Integer;
  XHotSpot, YHotSpot: Integer): Bool; stdcall;

function ImageList_DragShowNolock(Show: Bool): Bool; stdcall;

function ImageList_GetDragImage(Point, HotSpot: PPoint): HIMAGELIST; stdcall;

{ macros }

procedure ImageList_RemoveAll(ImageList: HIMAGELIST);

function ImageList_ExtractIcon(Instance: THandle; ImageList: HIMAGELIST;
  Image: Integer): HIcon;

function ImageList_LoadBitmap(Instance: THandle; Bmp: PChar;
  CX, Grow: Integer; MasK: TColorRef): HIMAGELIST;


function ImageList_Read(Stream: IStream): HIMAGELIST; stdcall;

function ImageList_Write(ImageList: HIMAGELIST; Stream: IStream): BOOL; stdcall;

type
  PImageInfo = ^TImageInfo;
  
  _IMAGEINFO = packed record
    hbmImage: HBitmap;
    hbmMask: HBitmap;
    Unused1: Integer;
    Unused2: Integer;
    rcImage: TRect;
  end;
  TImageInfo = _IMAGEINFO;
  
  IMAGEINFO = _IMAGEINFO;


function ImageList_GetIconSize(ImageList: HIMAGELIST; var CX, CY: Integer): Bool; stdcall;

function ImageList_SetIconSize(ImageList: HIMAGELIST; CX, CY: Integer): Bool; stdcall;

function ImageList_GetImageInfo(ImageList: HIMAGELIST; Index: Integer;
  var ImageInfo: TImageInfo): Bool; stdcall;

function ImageList_Merge(ImageList1: HIMAGELIST; Index1: Integer;
  ImageList2: HIMAGELIST; Index2: Integer; DX, DY: Integer): Bool; stdcall;

function ImageList_Duplicate(himl: HIMAGELIST): HIMAGELIST; stdcall;

{ ====== HEADER CONTROL ========================== }

const
  
  WC_HEADER = 'SysHeader32';

  
  HDS_HORZ                = $00000000;
  
  HDS_BUTTONS             = $00000002;
  
  HDS_HOTTRACK            = $00000004;
  
  HDS_HIDDEN              = $00000008;
  
  HDS_DRAGDROP            = $00000040;
  
  HDS_FULLDRAG            = $00000080;

type
  PHDItemA = ^THDItemA;
  PHDItemW = ^THDItemW;
  PHDItem = PHDItemA;
  
  _HD_ITEMA = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PAnsiChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;        // index of bitmap in ImageList
    iOrder: Integer;        // where to draw this item
  end;
  
  _HD_ITEMW = packed record
    Mask: Cardinal;
    cxy: Integer;
    pszText: PWideChar;
    hbm: HBITMAP;
    cchTextMax: Integer;
    fmt: Integer;
    lParam: LPARAM;
    iImage: Integer;        // index of bitmap in ImageList
    iOrder: Integer;        // where to draw this item
  end;
  
  _HD_ITEM = _HD_ITEMA;
  THDItemA = _HD_ITEMA;
  THDItemW = _HD_ITEMW;
  THDItem = THDItemA;
  
  HD_ITEMA = _HD_ITEMA;
  
  HD_ITEMW = _HD_ITEMW;
  
  HD_ITEM = HD_ITEMA;

const
  
  HDI_WIDTH               = $0001;
  
  HDI_HEIGHT              = HDI_WIDTH;
  
  HDI_TEXT                = $0002;
  
  HDI_FORMAT              = $0004;
  
  HDI_LPARAM              = $0008;
  
  HDI_BITMAP              = $0010;
  
  HDI_IMAGE               = $0020;
  
  HDI_DI_SETITEM          = $0040;
  
  HDI_ORDER               = $0080;

  
  HDF_LEFT                = 0;
  
  HDF_RIGHT               = 1;
  
  HDF_CENTER              = 2;
  
  HDF_JUSTIFYMASK         = $0003;
  
  HDF_RTLREADING          = 4; 

  
  HDF_OWNERDRAW           = $8000;
  
  HDF_STRING              = $4000;
  
  HDF_BITMAP              = $2000;
  
  HDF_BITMAP_ON_RIGHT     = $1000;
  
  HDF_IMAGE               = $0800;

  
  HDM_GETITEMCOUNT        = HDM_FIRST + 0;


function Header_GetItemCount(Header: HWnd): Integer;

const
  
  HDM_INSERTITEMW          = HDM_FIRST + 10;
  
  HDM_INSERTITEMA          = HDM_FIRST + 1;
  
  HDM_INSERTITEM           = HDM_INSERTITEMA;


function Header_InsertItem(Header: HWnd; Index: Integer;
  const Item: THDItem): Integer;

const
  
  HDM_DELETEITEM          = HDM_FIRST + 2;


function Header_DeleteItem(Header: HWnd; Index: Integer): Bool;

const
  
  HDM_GETITEMW             = HDM_FIRST + 11;
  
  HDM_GETITEMA             = HDM_FIRST + 3;
  
  HDM_GETITEM              = HDM_GETITEMA;


function Header_GetItem(Header: HWnd; Index: Integer;
  var Item: THDItem): Bool;

const
  
  HDM_SETITEMA            = HDM_FIRST + 4;
  
  HDM_SETITEMW            = HDM_FIRST + 12;
  
  HDM_SETITEM             = HDM_SETITEMA;


function Header_SetItem(Header: HWnd; Index: Integer; const Item: THDItem): Bool;

type
  PHDLayout = ^THDLayout;
  
  _HD_LAYOUT = packed record
    Rect: ^TRect;
    WindowPos: PWindowPos;
  end;
  THDLayout = _HD_LAYOUT;
  
  HD_LAYOUT = _HD_LAYOUT;

const
  
  HDM_LAYOUT              = HDM_FIRST + 5;


function Header_Layout(Header: HWnd; Layout: PHDLayout): Bool;

const
  
  HHT_NOWHERE             = $0001;
  
  HHT_ONHEADER            = $0002;
  
  HHT_ONDIVIDER           = $0004;
  
  HHT_ONDIVOPEN           = $0008;
  
  HHT_ABOVE               = $0100;
  
  HHT_BELOW               = $0200;
  
  HHT_TORIGHT             = $0400;
  
  HHT_TOLEFT              = $0800;

type
  PHDHitTestInfo = ^THDHitTestInfo;
  
  _HD_HITTESTINFO = packed record
    Point: TPoint;
    Flags: Cardinal;
    Item: Integer;
  end;
  THDHitTestInfo = _HD_HITTESTINFO;
  
  HD_HITTESTINFO = _HD_HITTESTINFO;

const
  
  HDM_HITTEST             = HDM_FIRST + 6;
  
  HDM_GETITEMRECT         = HDM_FIRST + 7;
  
  HDM_SETIMAGELIST        = HDM_FIRST + 8;
  
  HDM_GETIMAGELIST        = HDM_FIRST + 9;
  
  HDM_ORDERTOINDEX        = HDM_FIRST + 15;
  
  HDM_CREATEDRAGIMAGE     = HDM_FIRST + 16;  // wparam = which item = by index;
  
  HDM_GETORDERARRAY       = HDM_FIRST + 17;
  
  HDM_SETORDERARRAY       = HDM_FIRST + 18;
  
  HDM_SETHOTDIVIDER       = HDM_FIRST + 19;
  
  HDM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  
  HDM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;


function Header_GetItemRect(hwnd: HWND; iItem: Integer; lprc: PRect): Integer;

function Header_SetImageList(hwnd: HWND; himl: HIMAGELIST): HIMAGELIST;

function Header_GetImageList(hwnd: HWND): HIMAGELIST;

function Header_OrderToIndex(hwnd: HWND; i: Integer): Integer;

function Header_CreateDragImage(hwnd: HWND; i: Integer): HIMAGELIST;

function Header_GetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;

function Header_SetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;

// lparam = int array of size HDM_GETITEMCOUNT
// the array specifies the order that all items should be displayed.
// e.g.  { 2, 0, 1}
// says the index 2 item should be shown in the 0ths position
//      index 0 should be shown in the 1st position
//      index 1 should be shown in the 2nd position


function Header_SetHotDivider(hwnd: HWND; fPos: BOOL; dw: DWORD): Integer;

// convenience message for external dragdrop
// wParam = BOOL  specifying whether the lParam is a dwPos of the cursor
//              position or the index of which divider to hotlight
// lParam = depends on wParam  (-1 and wParm = FALSE turns off hotlight)


function Header_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer;

function Header_GetUnicodeFormat(hwnd: HWND): Integer;

const
  
  HDN_ITEMCHANGINGA        = HDN_FIRST-0;
  
  HDN_ITEMCHANGEDA         = HDN_FIRST-1;
  
  HDN_ITEMCLICKA           = HDN_FIRST-2;
  
  HDN_ITEMDBLCLICKA        = HDN_FIRST-3;
  
  HDN_DIVIDERDBLCLICKA     = HDN_FIRST-5;
  
  HDN_BEGINTRACKA          = HDN_FIRST-6;
  
  HDN_ENDTRACKA            = HDN_FIRST-7;
  
  HDN_TRACKA               = HDN_FIRST-8;
  
  HDN_GETDISPINFOA         = HDN_FIRST-9;
  
  HDN_BEGINDRAG            = HDN_FIRST-10;
  
  HDN_ENDDRAG              = HDN_FIRST-11;

  
  HDN_ITEMCHANGINGW        = HDN_FIRST-20;
  
  HDN_ITEMCHANGEDW         = HDN_FIRST-21;
  
  HDN_ITEMCLICKW           = HDN_FIRST-22;
  
  HDN_ITEMDBLCLICKW        = HDN_FIRST-23;
  
  HDN_DIVIDERDBLCLICKW     = HDN_FIRST-25;
  
  HDN_BEGINTRACKW          = HDN_FIRST-26;
  
  HDN_ENDTRACKW            = HDN_FIRST-27;
  
  HDN_TRACKW               = HDN_FIRST-28;
  
  HDN_GETDISPINFOW         = HDN_FIRST-29;

  
  HDN_ITEMCHANGING        = HDN_ITEMCHANGINGA;
  
  HDN_ITEMCHANGED         = HDN_ITEMCHANGEDA;
  
  HDN_ITEMCLICK           = HDN_ITEMCLICKA;
  
  HDN_ITEMDBLCLICK        = HDN_ITEMDBLCLICKA;
  
  HDN_DIVIDERDBLCLICK     = HDN_DIVIDERDBLCLICKA;
  
  HDN_BEGINTRACK          = HDN_BEGINTRACKA;
  
  HDN_ENDTRACK            = HDN_ENDTRACKA;
  
  HDN_TRACK               = HDN_TRACKA;
  
  HDN_GETDISPINFO         = HDN_GETDISPINFOA;

type
  
  tagNMHEADERA = packed record
    Hdr: TNMHdr;
    Item: Integer;
    Button: Integer;
    PItem: PHDItemA;
  end;
  
  tagNMHEADERW = packed record
    Hdr: TNMHdr;
    Item: Integer;
    Button: Integer;
    PItem: PHDItemW;
  end;
  
  tagNMHEADER = tagNMHEADERA;
  
  HD_NOTIFYA = tagNMHEADERA;
  
  HD_NOTIFYW = tagNMHEADERW;
  
  HD_NOTIFY = HD_NOTIFYA;
  PHDNotifyA = ^THDNotifyA;
  PHDNotifyW = ^THDNotifyW;
  PHDNotify = PHDNotifyA;
  THDNotifyA = tagNMHEADERA;
  THDNotifyW = tagNMHEADERW;
  THDNotify = THDNotifyA;

  
  tagNMHDDISPINFOA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    mask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  
  tagNMHDDISPINFOW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    mask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;
  
  tagNMHDDISPINFO = tagNMHDDISPINFOA;
  PNMHDispInfoA = ^TNMHDispInfoA;
  PNMHDispInfoW = ^TNMHDispInfoW;
  PNMHDispInfo = PNMHDispInfoA;
  TNMHDispInfoA = tagNMHDDISPINFOA;
  TNMHDispInfoW = tagNMHDDISPINFOW;
  TNMHDispInfo = TNMHDispInfoA;


{ ====== TOOLBAR CONTROL =================== }

const
  
  TOOLBARCLASSNAME = 'ToolbarWindow32';

type
  PTBButton = ^TTBButton;
  
  _TBBUTTON = packed record
    iBitmap: Integer;
    idCommand: Integer;
    fsState: Byte;
    fsStyle: Byte;
    bReserved: array[1..2] of Byte;
    dwData: Longint;
    iString: Integer;
  end;
  TTBButton = _TBBUTTON;

  PColorMap = ^TColorMap;
  
  _COLORMAP = packed record
    cFrom: TColorRef;
    cTo: TColorRef;
  end;
  TColorMap = _COLORMAP;
  
  COLORMAP = _COLORMAP;


function CreateToolBarEx(Wnd: HWnd; ws: Longint; ID: UINT;
  Bitmaps: Integer; BMInst: THandle; BMID: Cardinal; Buttons: PTBButton;
  NumButtons: Integer; dxButton, dyButton: Integer;
  dxBitmap, dyBitmap: Integer; StructSize: UINT): HWnd; stdcall;


function CreateMappedBitmap(Instance: THandle; Bitmap: Integer;
  Flags: UINT; ColorMap: PColorMap; NumMaps: Integer): HBitmap; stdcall;

const

  
  CMB_MASKED              = $02;

  
  TBSTATE_CHECKED         = $01;
  
  TBSTATE_PRESSED         = $02;
  
  TBSTATE_ENABLED         = $04;
  
  TBSTATE_HIDDEN          = $08;
  
  TBSTATE_INDETERMINATE   = $10;
  
  TBSTATE_WRAP            = $20;
  
  TBSTATE_ELLIPSES        = $40;
  
  TBSTATE_MARKED          = $80;

  
  TBSTYLE_BUTTON          = $00;
  
  TBSTYLE_SEP             = $01;
  
  TBSTYLE_CHECK           = $02;
  
  TBSTYLE_GROUP           = $04;
  
  TBSTYLE_CHECKGROUP      = TBSTYLE_GROUP or TBSTYLE_CHECK;
  
  TBSTYLE_DROPDOWN        = $08;
  
  TBSTYLE_AUTOSIZE        = $0010; // automatically calculate the cx of the button
  
  TBSTYLE_NOPREFIX        = $0020; // if this button should not have accel prefix

  
  TBSTYLE_TOOLTIPS        = $0100;
  
  TBSTYLE_WRAPABLE        = $0200;
  
  TBSTYLE_ALTDRAG         = $0400;
  
  TBSTYLE_FLAT            = $0800;
  
  TBSTYLE_LIST            = $1000;
  
  TBSTYLE_CUSTOMERASE     = $2000;
  
  TBSTYLE_REGISTERDROP    = $4000;
  
  TBSTYLE_TRANSPARENT     = $8000;
  
  TBSTYLE_EX_DRAWDDARROWS = $00000001;

type
  // Custom Draw Structure
  
  _NMTBCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    hbrMonoDither: HBRUSH;
    hbrLines: HBRUSH;                // For drawing lines on buttons
    hpenLines: HPEN;                 // For drawing lines on buttons
    clrText: COLORREF;               // Color of text
    clrMark: COLORREF;               // Color of text bk when marked. (only if TBSTATE_MARKED)
    clrTextHighlight: COLORREF;      // Color of text when highlighted
    clrBtnFace: COLORREF;            // Background of the button
    clrBtnHighlight: COLORREF;       // 3D highlight
    clrHighlightHotTrack: COLORREF;  // In conjunction with fHighlightHotTrack
                                     // will cause button to highlight like a menu
    rcText: TRect;                   // Rect for text
    nStringBkMode: Integer;
    nHLStringBkMode: Integer;
  end;
  PNMTBCustomDraw = ^TNMTBCustomDraw;
  TNMTBCustomDraw = _NMTBCUSTOMDRAW;

const
  // Toolbar custom draw return flags
  
  TBCDRF_NOEDGES              = $00010000;  // Don't draw button edges
  
  TBCDRF_HILITEHOTTRACK       = $00020000;  // Use color of the button bk when hottracked
  
  TBCDRF_NOOFFSET             = $00040000;  // Don't offset button if pressed
  
  TBCDRF_NOMARK               = $00080000;  // Don't draw default highlight of image/text for TBSTATE_MARKED
  
  TBCDRF_NOETCHEDEFFECT       = $00100000;  // Don't draw etched effect for disabled items

  
  TB_ENABLEBUTTON         = WM_USER + 1;
  
  TB_CHECKBUTTON          = WM_USER + 2;
  
  TB_PRESSBUTTON          = WM_USER + 3;
  
  TB_HIDEBUTTON           = WM_USER + 4;
  
  TB_INDETERMINATE        = WM_USER + 5;
  
  TB_MARKBUTTON           = WM_USER + 6;
  
  TB_ISBUTTONENABLED      = WM_USER + 9;
  
  TB_ISBUTTONCHECKED      = WM_USER + 10;
  
  TB_ISBUTTONPRESSED      = WM_USER + 11;
  
  TB_ISBUTTONHIDDEN       = WM_USER + 12;
  
  TB_ISBUTTONINDETERMINATE = WM_USER + 13;
  
  TB_ISBUTTONHIGHLIGHTED   = WM_USER + 14;
  
  TB_SETSTATE             = WM_USER + 17;
  
  TB_GETSTATE             = WM_USER + 18;
  
  TB_ADDBITMAP            = WM_USER + 19;

type
  PTBAddBitmap = ^TTBAddBitmap;
  
  tagTBADDBITMAP = packed record
    hInst: THandle;
    nID: UINT;
  end;
  TTBAddBitmap = tagTBADDBITMAP;
  
  TBADDBITMAP = tagTBADDBITMAP;

const
  
  HINST_COMMCTRL = THandle(-1);

  
  IDB_STD_SMALL_COLOR     = 0;
  
  IDB_STD_LARGE_COLOR     = 1;
  
  IDB_VIEW_SMALL_COLOR    = 4;
  
  IDB_VIEW_LARGE_COLOR    = 5;
  
  IDB_HIST_SMALL_COLOR    = 8;
  
  IDB_HIST_LARGE_COLOR    = 9;

{ icon indexes for standard bitmap }
  
  STD_CUT                 = 0;
  
  STD_COPY                = 1;
  
  STD_PASTE               = 2;
  
  STD_UNDO                = 3;
  
  STD_REDOW               = 4;
  
  STD_DELETE              = 5;
  
  STD_FILENEW             = 6;
  
  STD_FILEOPEN            = 7;
  
  STD_FILESAVE            = 8;
  
  STD_PRINTPRE            = 9;
  
  STD_PROPERTIES          = 10;
  
  STD_HELP                = 11;
  
  STD_FIND                = 12;
  
  STD_REPLACE             = 13;
  
  STD_PRINT               = 14;

{ icon indexes for standard view bitmap }

  
  VIEW_LARGEICONS         = 0;
  
  VIEW_SMALLICONS         = 1;
  
  VIEW_LIST               = 2;
  
  VIEW_DETAILS            = 3;
  
  VIEW_SORTNAME           = 4;
  
  VIEW_SORTSIZE           = 5;
  
  VIEW_SORTDATE           = 6;
  
  VIEW_SORTTYPE           = 7;
  
  VIEW_PARENTFOLDER       = 8;
  
  VIEW_NETCONNECT         = 9;
  
  VIEW_NETDISCONNECT      = 10;
  
  VIEW_NEWFOLDER          = 11;
  
  VIEW_VIEWMENU           = 12;

{ icon indexes for history bitmap }

  
  HIST_BACK               = 0;
  
  HIST_FORWARD            = 1;
  
  HIST_FAVORITES          = 2;
  
  HIST_ADDTOFAVORITES     = 3;
  
  HIST_VIEWTREE           = 4;

  
  TB_ADDBUTTONSA          = WM_USER + 20;
  
  TB_INSERTBUTTONA        = WM_USER + 21;
  
  TB_DELETEBUTTON         = WM_USER + 22;
  
  TB_GETBUTTON            = WM_USER + 23;
  
  TB_BUTTONCOUNT          = WM_USER + 24;
  
  TB_COMMANDTOINDEX       = WM_USER + 25;

type
  PTBSaveParamsA = ^TTBSaveParamsA;
  PTBSaveParamsW = ^TTBSaveParamsW;
  PTBSaveParams = PTBSaveParamsA;
  
  tagTBSAVEPARAMSA = packed record
    hkr: THandle;
    pszSubKey: PAnsiChar;
    pszValueName: PAnsiChar;
  end;
  
  tagTBSAVEPARAMSW = packed record
    hkr: THandle;
    pszSubKey: PWideChar;
    pszValueName: PWideChar;
  end;
  
  tagTBSAVEPARAMS = tagTBSAVEPARAMSA;
  TTBSaveParamsA = tagTBSAVEPARAMSA;
  TTBSaveParamsW = tagTBSAVEPARAMSW;
  TTBSaveParams = TTBSaveParamsA;
  
  TBSAVEPARAMSA = tagTBSAVEPARAMSA;
  
  TBSAVEPARAMSW = tagTBSAVEPARAMSW;
  
  TBSAVEPARAMS = TBSAVEPARAMSA;

const
  
  TB_SAVERESTOREA          = WM_USER + 26;
  
  TB_ADDSTRINGA            = WM_USER + 28;
  
  TB_GETBUTTONTEXTA        = WM_USER + 45;
  
  TBN_GETBUTTONINFOA       = TBN_FIRST-0;

  
  TB_SAVERESTOREW          = WM_USER + 76;
  
  TB_ADDSTRINGW            = WM_USER + 77;
  
  TB_GETBUTTONTEXTW        = WM_USER + 75;
  
  TBN_GETBUTTONINFOW       = TBN_FIRST-20;

  
  TB_SAVERESTORE          = TB_SAVERESTOREA;
  
  TB_ADDSTRING            = TB_ADDSTRINGA;
  
  TB_GETBUTTONTEXT        = TB_GETBUTTONTEXTA;
  
  TBN_GETBUTTONINFO       = TBN_GETBUTTONINFOA;

  
  TB_CUSTOMIZE            = WM_USER + 27;
  
  TB_GETITEMRECT          = WM_USER + 29;
  
  TB_BUTTONSTRUCTSIZE     = WM_USER + 30;
  
  TB_SETBUTTONSIZE        = WM_USER + 31;
  
  TB_SETBITMAPSIZE        = WM_USER + 32;
  
  TB_AUTOSIZE             = WM_USER + 33;
  
  TB_GETTOOLTIPS          = WM_USER + 35;
  
  TB_SETTOOLTIPS          = WM_USER + 36;
  
  TB_SETPARENT            = WM_USER + 37;
  
  TB_SETROWS              = WM_USER + 39;
  
  TB_GETROWS              = WM_USER + 40;
  
  TB_SETCMDID             = WM_USER + 42;
  
  TB_CHANGEBITMAP         = WM_USER + 43;
  
  TB_GETBITMAP            = WM_USER + 44;
  
  TB_REPLACEBITMAP        = WM_USER + 46;
  
  TB_SETINDENT            = WM_USER + 47;
  
  TB_SETIMAGELIST         = WM_USER + 48;
  
  TB_GETIMAGELIST         = WM_USER + 49;
  
  TB_LOADIMAGES           = WM_USER + 50;
  
  TB_GETRECT              = WM_USER + 51; { wParam is the Cmd instead of index }
  
  TB_SETHOTIMAGELIST      = WM_USER + 52;
  
  TB_GETHOTIMAGELIST      = WM_USER + 53;
  
  TB_SETDISABLEDIMAGELIST = WM_USER + 54;
  
  TB_GETDISABLEDIMAGELIST = WM_USER + 55;
  
  TB_SETSTYLE             = WM_USER + 56;
  
  TB_GETSTYLE             = WM_USER + 57;
  
  TB_GETBUTTONSIZE        = WM_USER + 58;
  
  TB_SETBUTTONWIDTH       = WM_USER + 59;
  
  TB_SETMAXTEXTROWS       = WM_USER + 60;
  
  TB_GETTEXTROWS          = WM_USER + 61;

  
  TB_GETOBJECT            = WM_USER + 62;  // wParam == IID, lParam void **ppv
  
  TB_GETHOTITEM           = WM_USER + 71;
  
  TB_SETHOTITEM           = WM_USER + 72;  // wParam == iHotItem
  
  TB_SETANCHORHIGHLIGHT   = WM_USER + 73;  // wParam == TRUE/FALSE
  
  TB_GETANCHORHIGHLIGHT   = WM_USER + 74;
  
  TB_MAPACCELERATORA      = WM_USER + 78;  // wParam == ch, lParam int * pidBtn

type
  
  TBINSERTMARK = packed record
    iButton: Integer;
    dwFlags: DWORD;
  end;
  PTBInsertMark = ^TTBInsertMark;
  TTBInsertMark = TBINSERTMARK;

const
  
  TBIMHT_AFTER      = $00000001; // TRUE = insert After iButton, otherwise before
  
  TBIMHT_BACKGROUND = $00000002; // TRUE iff missed buttons completely

  
  TB_GETINSERTMARK        = WM_USER + 79;  // lParam == LPTBINSERTMARK
  
  TB_SETINSERTMARK        = WM_USER + 80;  // lParam == LPTBINSERTMARK
  
  TB_INSERTMARKHITTEST    = WM_USER + 81;  // wParam == LPPOINT lParam == LPTBINSERTMARK
  
  TB_MOVEBUTTON           = WM_USER + 82;
  
  TB_GETMAXSIZE           = WM_USER + 83;  // lParam == LPSIZE
  
  TB_SETEXTENDEDSTYLE     = WM_USER + 84;  // For TBSTYLE_EX_*
  
  TB_GETEXTENDEDSTYLE     = WM_USER + 85;  // For TBSTYLE_EX_*
  
  TB_GETPADDING           = WM_USER + 86;
  
  TB_SETPADDING           = WM_USER + 87;
  
  TB_SETINSERTMARKCOLOR   = WM_USER + 88;
  
  TB_GETINSERTMARKCOLOR   = WM_USER + 89;

  
  TB_SETCOLORSCHEME       = CCM_SETCOLORSCHEME;  // lParam is color scheme
  
  TB_GETCOLORSCHEME       = CCM_GETCOLORSCHEME;	// fills in COLORSCHEME pointed to by lParam

  
  TB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  
  TB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

  
  TB_MAPACCELERATORW      = WM_USER + 90;  // wParam == ch, lParam int * pidBtn
  
  TB_MAPACCELERATOR       = TB_MAPACCELERATORA;

type
  
  TBREPLACEBITMAP = packed record
    hInstOld: THandle;
    nIDOld: Cardinal;
    hInstNew: THandle;
    nIDNew: Cardinal;
    nButtons: Integer;
  end;
  PTBReplaceBitmap = ^TTBReplaceBitmap;
  TTBReplaceBitmap = TBREPLACEBITMAP;

const  
  
  TBBF_LARGE              = $0001;

  
  TB_GETBITMAPFLAGS       = WM_USER + 41;

  
  TBIF_IMAGE              = $00000001;
  
  TBIF_TEXT               = $00000002;
  
  TBIF_STATE              = $00000004;
  
  TBIF_STYLE              = $00000008;
  
  TBIF_LPARAM             = $00000010;
  
  TBIF_COMMAND            = $00000020;
  
  TBIF_SIZE               = $00000040;

type
  
  TBBUTTONINFOA = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PAnsiChar;
    cchText: Integer;
  end;
  
  TBBUTTONINFOW = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PWideChar;
    cchText: Integer;
  end;
  
  TBBUTTONINFO = TBBUTTONINFOA;
  PTBButtonInfoA = ^TTBButtonInfoA;
  PTBButtonInfoW = ^TTBButtonInfoW;
  PTBButtonInfo = PTBButtonInfoA;
  TTBButtonInfoA = TBBUTTONINFOA;
  TTBButtonInfoW = TBBUTTONINFOW;
  TTBButtonInfo = TTBButtonInfoA;

const
  // BUTTONINFO APIs do NOT support the string pool.
  
  TB_GETBUTTONINFOW        = WM_USER + 63;
  
  TB_SETBUTTONINFOW        = WM_USER + 64;
  
  TB_GETBUTTONINFOA        = WM_USER + 65;
  
  TB_SETBUTTONINFOA        = WM_USER + 66;
  
  TB_GETBUTTONINFO         = TB_GETBUTTONINFOA;
  
  TB_SETBUTTONINFO         = TB_SETBUTTONINFOA;

  
  TB_INSERTBUTTONW        = WM_USER + 67;
  
  TB_ADDBUTTONSW          = WM_USER + 68;

  
  TB_HITTEST              = WM_USER + 69;

  // New post Win95/NT4 for InsertButton and AddButton.  if iString member
  // is a pointer to a string, it will be handled as a string like listview
  // = although LPSTR_TEXTCALLBACK is not supported;.
  
  TB_INSERTBUTTON         = TB_INSERTBUTTONA;
  
  TB_ADDBUTTONS           = TB_ADDBUTTONSA;

  
  TB_SETDRAWTEXTFLAGS     = WM_USER + 70;  // wParam == mask lParam == bit values

  
  TBN_BEGINDRAG           = TBN_FIRST-1;
  
  TBN_ENDDRAG             = TBN_FIRST-2;
  
  TBN_BEGINADJUST         = TBN_FIRST-3;
  
  TBN_ENDADJUST           = TBN_FIRST-4;
  
  TBN_RESET               = TBN_FIRST-5;
  
  TBN_QUERYINSERT         = TBN_FIRST-6;
  
  TBN_QUERYDELETE         = TBN_FIRST-7;
  
  TBN_TOOLBARCHANGE       = TBN_FIRST-8;
  
  TBN_CUSTHELP            = TBN_FIRST-9;
  
  TBN_DROPDOWN            = TBN_FIRST-10;
  
  TBN_CLOSEUP             = TBN_FIRST-11;
  
  TBN_GETOBJECT           = TBN_FIRST-12;

type
  // Structure for TBN_HOTITEMCHANGE notification
  
  tagNMTBHOTITEM = packed record
    hdr: TNMHdr;
    idOld: Integer;
    idNew: Integer;
    dwFlags: DWORD;           // HICF_*
  end;
  PNMTBHotItem = ^TNMTBHotItem;
  TNMTBHotItem = tagNMTBHOTITEM;

const
  // Hot item change flags
  
  HICF_OTHER          = $00000000;
  
  HICF_MOUSE          = $00000001;          // Triggered by mouse
  
  HICF_ARROWKEYS      = $00000002;          // Triggered by arrow keys
  
  HICF_ACCELERATOR    = $00000004;          // Triggered by accelerator
  
  HICF_DUPACCEL       = $00000008;          // This accelerator is not unique
  
  HICF_ENTERING       = $00000010;          // idOld is invalid
  
  HICF_LEAVING        = $00000020;          // idNew is invalid
  
  HICF_RESELECT       = $00000040;          // hot item reselected

  
  TBN_HOTITEMCHANGE       = TBN_FIRST - 13;
  
  TBN_DRAGOUT             = TBN_FIRST - 14; // this is sent when the user clicks down on a button then drags off the button
  
  TBN_DELETINGBUTTON      = TBN_FIRST - 15; // uses TBNOTIFY
  
  TBN_GETDISPINFOA        = TBN_FIRST - 16; // This is sent when the  toolbar needs  some display information
  
  TBN_GETDISPINFOW        = TBN_FIRST - 17; // This is sent when the  toolbar needs  some display information
  
  TBN_GETINFOTIPA         = TBN_FIRST - 18;
  
  TBN_GETINFOTIPW         = TBN_FIRST - 19;

type
  
  tagNMTBGETINFOTIPA = packed record
    hdr: TNMHdr;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: LPARAM;
  end;
  
  tagNMTBGETINFOTIPW = packed record
    hdr: TNMHdr;
    pszText: PWideChar;
    cchTextMax: Integer;
    iItem: Integer;
    lParam: LPARAM;
  end;
  
  tagNMTBGETINFOTIP = tagNMTBGETINFOTIPA;
  PNMTBGetInfoTipA = ^TNMTBGetInfoTipA;
  PNMTBGetInfoTipW = ^TNMTBGetInfoTipW;
  PNMTBGetInfoTip = PNMTBGetInfoTipA;
  TNMTBGetInfoTipA = tagNMTBGETINFOTIPA;
  TNMTBGetInfoTipW = tagNMTBGETINFOTIPW;
  TNMTBGetInfoTip = TNMTBGetInfoTipA;

const
  
  TBNF_IMAGE              = $00000001;
  
  TBNF_TEXT               = $00000002;
  
  TBNF_DI_SETITEM         = $10000000;

type
  
  NMTBDISPINFOA = packed record
    hdr: TNMHdr;
    dwMask: DWORD;      // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer; // [in] id of button we're requesting info for
    lParam: DWORD;      // [in] lParam of button
    iImage: Integer;    // [out] image index
    pszText: PAnsiChar;    // [out] new text for item
    cchText: Integer;   // [in] size of buffer pointed to by pszText
  end;
  
  NMTBDISPINFOW = packed record
    hdr: TNMHdr;
    dwMask: DWORD;      // [in] Specifies the values requested .[out] Client ask the data to be set for future use
    idCommand: Integer; // [in] id of button we're requesting info for
    lParam: DWORD;      // [in] lParam of button
    iImage: Integer;    // [out] image index
    pszText: PWideChar;    // [out] new text for item
    cchText: Integer;   // [in] size of buffer pointed to by pszText
  end;
  
  NMTBDISPINFO = NMTBDISPINFOA;
  PNMTBDispInfoA = ^TNMTBDispInfoA;
  PNMTBDispInfoW = ^TNMTBDispInfoW;
  PNMTBDispInfo = PNMTBDispInfoA;
  TNMTBDispInfoA = NMTBDISPINFOA;
  TNMTBDispInfoW = NMTBDISPINFOW;
  TNMTBDispInfo = TNMTBDispInfoA;

const
  // Return codes for TBN_DROPDOWN
  
  TBDDRET_DEFAULT         = 0;
  
  TBDDRET_NODEFAULT       = 1;
  
  TBDDRET_TREATPRESSED    = 2;       // Treat as a standard press button

type
  
  tagNMTOOLBARA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PAnsiChar;
  end;
  
  tagNMTOOLBARW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PWideChar;
  end;
  
  tagNMTOOLBAR = tagNMTOOLBARA;
  PNMToolBarA = ^TNMToolBarA;
  PNMToolBarW = ^TNMToolBarW;
  PNMToolBar = PNMToolBarA;
  TNMToolBarA = tagNMTOOLBARA;
  TNMToolBarW = tagNMTOOLBARW;
  TNMToolBar = TNMToolBarA;

{ ====== REBAR CONTROL =================== }

const
  
  REBARCLASSNAME = 'ReBarWindow32';

type
  
  tagREBARINFO = packed record
    cbSize: UINT;
    fMask: UINT;
    himl: HIMAGELIST;
  end;
  PReBarInfo = ^TReBarInfo;
  TReBarInfo = tagREBARINFO;

const
  
  RBIM_IMAGELIST    = $00000001;

  
  RBS_TOOLTIPS      = $00000100;
  
  RBS_VARHEIGHT     = $00000200;
  
  RBS_BANDBORDERS   = $00000400;
  
  RBS_FIXEDORDER    = $00000800;

  
  RBS_REGISTERDROP  = $00001000;
  
  RBS_AUTOSIZE      = $00002000;
  
  RBS_VERTICALGRIPPER = $00004000;  // this always has the vertical gripper (default for horizontal mode)
  
  RBS_DBLCLKTOGGLE  = $00008000;

  
  RBBS_BREAK        = $00000001;  // break to new line
  
  RBBS_FIXEDSIZE    = $00000002;  // band can't be sized
  
  RBBS_CHILDEDGE    = $00000004;  // edge around top and bottom of child window
  
  RBBS_HIDDEN       = $00000008;  // don't show
  
  RBBS_NOVERT       = $00000010;  // don't show when vertical
  
  RBBS_FIXEDBMP     = $00000020;  // bitmap doesn't move during band resize
  
  RBBS_VARIABLEHEIGHT = $00000040;  // allow autosizing of this child vertically
  
  RBBS_GRIPPERALWAYS  = $00000080;  // always show the gripper
  
  RBBS_NOGRIPPER      = $00000100;  // never show the gripper

  
  RBBIM_STYLE       = $00000001;
  
  RBBIM_COLORS      = $00000002;
  
  RBBIM_TEXT        = $00000004;
  
  RBBIM_IMAGE       = $00000008;
  
  RBBIM_CHILD       = $00000010;
  
  RBBIM_CHILDSIZE   = $00000020;
  
  RBBIM_SIZE        = $00000040;
  
  RBBIM_BACKGROUND  = $00000080;
  
  RBBIM_ID          = $00000100;
  
  RBBIM_IDEALSIZE     = $00000200;
  
  RBBIM_LPARAM        = $00000400;
  
  RBBIM_HEADERSIZE    = $00000800;  // control the size of the header

type
  
  tagREBARBANDINFOA = packed record
    cbSize: UINT;
    fMask: UINT;
    fStyle: UINT;
    clrFore: TColorRef;
    clrBack: TColorRef;
    lpText: PAnsiChar;
    cch: UINT;
    iImage: Integer;
    hwndChild: HWnd;
    cxMinChild: UINT;
    cyMinChild: UINT;
    cx: UINT;
    hbmBack: HBitmap;
    wID: UINT;
    cyChild: UINT;
    cyMaxChild: UINT;
    cyIntegral: UINT;
    cxIdeal: UINT;
    lParam: LPARAM;
    cxHeader: UINT;
  end;
  
  tagREBARBANDINFOW = packed record
    cbSize: UINT;
    fMask: UINT;
    fStyle: UINT;
    clrFore: TColorRef;
    clrBack: TColorRef;
    lpText: PWideChar;
    cch: UINT;
    iImage: Integer;
    hwndChild: HWnd;
    cxMinChild: UINT;
    cyMinChild: UINT;
    cx: UINT;
    hbmBack: HBitmap;
    wID: UINT;
    cyChild: UINT;
    cyMaxChild: UINT;
    cyIntegral: UINT;
    cxIdeal: UINT;
    lParam: LPARAM;
    cxHeader: UINT;
  end;
  
  tagREBARBANDINFO = tagREBARBANDINFOA;
  PReBarBandInfoA = ^TReBarBandInfoA;
  PReBarBandInfoW = ^TReBarBandInfoW;
  PReBarBandInfo = PReBarBandInfoA;
  TReBarBandInfoA = tagREBARBANDINFOA;
  TReBarBandInfoW = tagREBARBANDINFOW;
  TReBarBandInfo = TReBarBandInfoA;

const
  
  RB_INSERTBANDA     = WM_USER +  1;
  
  RB_DELETEBAND      = WM_USER +  2;
  
  RB_GETBARINFO      = WM_USER +  3;
  
  RB_SETBARINFO      = WM_USER +  4;
  RB_GETBANDINFO_PRE_IE4     = WM_USER +  5;
  
  RB_SETBANDINFOA    = WM_USER +  6;
  
  RB_SETPARENT       = WM_USER +  7;
  
  RB_HITTEST         = WM_USER +  8;
  
  RB_GETRECT         = WM_USER +  9;
  
  RB_INSERTBANDW     = WM_USER +  10;
  
  RB_SETBANDINFOW    = WM_USER +  11;
  
  RB_GETBANDCOUNT    = WM_USER +  12;
  
  RB_GETROWCOUNT     = WM_USER +  13;
  
  RB_GETROWHEIGHT    = WM_USER +  14;
  
  RB_IDTOINDEX       = WM_USER +  16; // wParam == id
  
  RB_GETTOOLTIPS     = WM_USER +  17;
  
  RB_SETTOOLTIPS     = WM_USER +  18;
  
  RB_SETBKCOLOR      = WM_USER +  19; // sets the default BK color
  
  RB_GETBKCOLOR      = WM_USER +  20; // defaults to CLR_NONE
  
  RB_SETTEXTCOLOR    = WM_USER +  21;
  
  RB_GETTEXTCOLOR    = WM_USER +  22; // defaults to 0x00000000
  
  RB_SIZETORECT      = WM_USER +  23; // resize the rebar/break bands and such to this rect (lparam;

  // for manual drag control
  // lparam == cursor pos
        // -1 means do it yourself.
        // -2 means use what you had saved before
  
  RB_BEGINDRAG    = WM_USER + 24;
  
  RB_ENDDRAG      = WM_USER + 25;
  
  RB_DRAGMOVE     = WM_USER + 26;
  
  RB_GETBARHEIGHT = WM_USER + 27;
  
  RB_GETBANDINFOW = WM_USER + 28;
  
  RB_GETBANDINFOA = WM_USER + 29;

  
  RB_MINIMIZEBAND = WM_USER + 30;
  
  RB_MAXIMIZEBAND = WM_USER + 31;

  
  RB_GETDROPTARGET = CCM_GETDROPTARGET;

  
  RB_GETBANDBORDERS = WM_USER + 34;  // returns in lparam = lprc the amount of edges added to band wparam

  
  RB_SHOWBAND     = WM_USER + 35;      // show/hide band
  
  RB_SETPALETTE   = WM_USER + 37;
  
  RB_GETPALETTE   = WM_USER + 38;
  
  RB_MOVEBAND     = WM_USER + 39;

  
  RB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  
  RB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

  
  RB_INSERTBAND      = RB_INSERTBANDA;
  
  RB_SETBANDINFO     = RB_SETBANDINFOA;
  
  RB_GETBANDINFO     = RB_GETBANDINFOA;

  
  RBN_HEIGHTCHANGE   = RBN_FIRST - 0;

  
  RBN_GETOBJECT       = RBN_FIRST - 1;
  
  RBN_LAYOUTCHANGED   = RBN_FIRST - 2;
  
  RBN_AUTOSIZE        = RBN_FIRST - 3;
  
  RBN_BEGINDRAG       = RBN_FIRST - 4;
  
  RBN_ENDDRAG         = RBN_FIRST - 5;
  
  RBN_DELETINGBAND    = RBN_FIRST - 6;     // Uses NMREBAR
  
  RBN_DELETEDBAND     = RBN_FIRST - 7;     // Uses NMREBAR
  
  RBN_CHILDSIZE       = RBN_FIRST - 8;

type
  
  tagNMREBARCHILDSIZE = packed record
    hdr: TNMHdr;
    uBand: UINT;
    wID: UINT;
    rcChild: TRect;
    rcBand: TRect;
  end;
  PNMReBarChildSize = ^TNMReBarChildSize;
  TNMReBarChildSize = tagNMREBARCHILDSIZE;

  
  tagNMREBAR = packed record
    hdr: TNMHdr;
    dwMask: DWORD;           // RBNM_*
    uBand: UINT;
    fStyle: UINT;
    wID: UINT;
    lParam: LPARAM;
  end;
  PNMReBar = ^TNMReBar;
  TNMReBar = tagNMREBAR;

const
  // Mask flags for NMREBAR
  
  RBNM_ID         = $00000001;
  
  RBNM_STYLE      = $00000002;
  
  RBNM_LPARAM     = $00000004;

type
  
  tagNMRBAUTOSIZE = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    rcTarget: TRect;
    rcActual: TRect;
  end;
  PNMRBAutoSize = ^TNMRBAutoSize;
  TNMRBAutoSize = tagNMRBAUTOSIZE;

const
  
  RBHT_NOWHERE    = $0001;
  
  RBHT_CAPTION    = $0002;
  
  RBHT_CLIENT     = $0003;
  
  RBHT_GRABBER    = $0004;

type
  
  _RB_HITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    iBand: Integer;
  end;
  PRBHitTestInfo = ^TRBHitTestInfo;
  TRBHitTestInfo = _RB_HITTESTINFO;

{ ====== TOOLTIPS CONTROL ========================== }

const
  
  TOOLTIPS_CLASS = 'tooltips_class32';

type
  PToolInfoA = ^TToolInfoA;
  PToolInfoW = ^TToolInfoW;
  PToolInfo = PToolInfoA;
  
  tagTOOLINFOA = packed record
    cbSize: UINT;
    uFlags: UINT;
    hwnd: HWND;
    uId: UINT;
    Rect: TRect;
    hInst: THandle;
    lpszText: PAnsiChar;
    lParam: LPARAM;
  end;
  
  tagTOOLINFOW = packed record
    cbSize: UINT;
    uFlags: UINT;
    hwnd: HWND;
    uId: UINT;
    Rect: TRect;
    hInst: THandle;
    lpszText: PWideChar;
    lParam: LPARAM;
  end;
  
  tagTOOLINFO = tagTOOLINFOA;
  TToolInfoA = tagTOOLINFOA;
  TToolInfoW = tagTOOLINFOW;
  TToolInfo = TToolInfoA;
  
  TOOLINFOA = tagTOOLINFOA;
  
  TOOLINFOW = tagTOOLINFOW;
  
  TOOLINFO = TOOLINFOA;

const
  
  TTS_ALWAYSTIP           = $01;
  
  TTS_NOPREFIX            = $02;

  
  TTF_IDISHWND            = $0001;

  // Use this to center around trackpoint in trackmode
  // -OR- to center around tool in normal mode.
  // Use TTF_ABSOLUTE to place the tip exactly at the track coords when
  // in tracking mode.  TTF_ABSOLUTE can be used in conjunction with TTF_CENTERTIP
  // to center the tip absolutely about the track point.

  
  TTF_CENTERTIP           = $0002;
  
  TTF_RTLREADING          = $0004;
  
  TTF_SUBCLASS            = $0010;
  
  TTF_TRACK               = $0020;
  
  TTF_ABSOLUTE            = $0080;
  
  TTF_TRANSPARENT         = $0100;
  
  TTF_DI_SETITEM          = $8000;       // valid only on the TTN_NEEDTEXT callback

  
  TTDT_AUTOMATIC          = 0;
  
  TTDT_RESHOW             = 1;
  
  TTDT_AUTOPOP            = 2;
  
  TTDT_INITIAL            = 3;

  
  TTM_ACTIVATE            = WM_USER + 1;
  
  TTM_SETDELAYTIME        = WM_USER + 3;

  
  TTM_ADDTOOLA             = WM_USER + 4;
  
  TTM_DELTOOLA             = WM_USER + 5;
  
  TTM_NEWTOOLRECTA         = WM_USER + 6;
  
  TTM_GETTOOLINFOA         = WM_USER + 8;
  
  TTM_SETTOOLINFOA         = WM_USER + 9;
  
  TTM_HITTESTA             = WM_USER + 10;
  
  TTM_GETTEXTA             = WM_USER + 11;
  
  TTM_UPDATETIPTEXTA       = WM_USER + 12;
  
  TTM_ENUMTOOLSA           = WM_USER + 14;
  
  TTM_GETCURRENTTOOLA      = WM_USER + 15;

  
  TTM_ADDTOOLW             = WM_USER + 50;
  
  TTM_DELTOOLW             = WM_USER + 51;
  
  TTM_NEWTOOLRECTW         = WM_USER + 52;
  
  TTM_GETTOOLINFOW         = WM_USER + 53;
  
  TTM_SETTOOLINFOW         = WM_USER + 54;
  
  TTM_HITTESTW             = WM_USER + 55;
  
  TTM_GETTEXTW             = WM_USER + 56;
  
  TTM_UPDATETIPTEXTW       = WM_USER + 57;
  
  TTM_ENUMTOOLSW           = WM_USER + 58;
  
  TTM_GETCURRENTTOOLW      = WM_USER + 59;
  
  TTM_WINDOWFROMPOINT      = WM_USER + 16;
  
  TTM_TRACKACTIVATE        = WM_USER + 17;  // wParam = TRUE/FALSE start end  lparam = LPTOOLINFO
  
  TTM_TRACKPOSITION        = WM_USER + 18;  // lParam = dwPos
  
  TTM_SETTIPBKCOLOR        = WM_USER + 19;
  
  TTM_SETTIPTEXTCOLOR      = WM_USER + 20;
  
  TTM_GETDELAYTIME         = WM_USER + 21;
  
  TTM_GETTIPBKCOLOR        = WM_USER + 22;
  
  TTM_GETTIPTEXTCOLOR      = WM_USER + 23;
  
  TTM_SETMAXTIPWIDTH       = WM_USER + 24;
  
  TTM_GETMAXTIPWIDTH       = WM_USER + 25;
  
  TTM_SETMARGIN            = WM_USER + 26;  // lParam = lprc
  
  TTM_GETMARGIN            = WM_USER + 27;  // lParam = lprc
  
  TTM_POP                  = WM_USER + 28;
  
  TTM_UPDATE               = WM_USER + 29;

  
  TTM_ADDTOOL             = TTM_ADDTOOLA;
  
  TTM_DELTOOL             = TTM_DELTOOLA;
  
  TTM_NEWTOOLRECT         = TTM_NEWTOOLRECTA;
  
  TTM_GETTOOLINFO         = TTM_GETTOOLINFOA;
  
  TTM_SETTOOLINFO         = TTM_SETTOOLINFOA;
  
  TTM_HITTEST             = TTM_HITTESTA;
  
  TTM_GETTEXT             = TTM_GETTEXTA;
  
  TTM_UPDATETIPTEXT       = TTM_UPDATETIPTEXTA;
  
  TTM_ENUMTOOLS           = TTM_ENUMTOOLSA;
  
  TTM_GETCURRENTTOOL      = TTM_GETCURRENTTOOLA;

  
  TTM_RELAYEVENT          = WM_USER + 7;
  
  TTM_GETTOOLCOUNT        = WM_USER +13;


type
  PTTHitTestInfoA = ^TTTHitTestInfoA;
  PTTHitTestInfoW = ^TTTHitTestInfoW;
  PTTHitTestInfo = PTTHitTestInfoA;
  
  _TT_HITTESTINFOA = packed record
    hwnd: HWND;
    pt: TPoint;
    ti: TToolInfoA;
  end;
  
  _TT_HITTESTINFOW = packed record
    hwnd: HWND;
    pt: TPoint;
    ti: TToolInfoW;
  end;
  
  _TT_HITTESTINFO = _TT_HITTESTINFOA;
  TTTHitTestInfoA = _TT_HITTESTINFOA;
  TTTHitTestInfoW = _TT_HITTESTINFOW;
  TTTHitTestInfo = TTTHitTestInfoA;
  
  TTHITTESTINFOA = _TT_HITTESTINFOA;
  
  TTHITTESTINFOW = _TT_HITTESTINFOW;
  
  TTHITTESTINFO = TTHITTESTINFOA;


const
  
  TTN_NEEDTEXTA            = TTN_FIRST - 0;
  
  TTN_NEEDTEXTW            = TTN_FIRST - 10;

  
  TTN_NEEDTEXT            = TTN_NEEDTEXTA;

  
  TTN_SHOW                = TTN_FIRST - 1;
  
  TTN_POP                 = TTN_FIRST - 2;

type
  
  tagNMTTDISPINFOA = packed record
    hdr: TNMHdr;
    lpszText: PAnsiChar;
    szText: array[0..79] of AnsiChar;
    hinst: HINST;
    uFlags: UINT;
    lParam: LPARAM;
  end;
  
  tagNMTTDISPINFOW = packed record
    hdr: TNMHdr;
    lpszText: PWideChar;
    szText: array[0..79] of WideChar;
    hinst: HINST;
    uFlags: UINT;
    lParam: LPARAM;
  end;
  
  tagNMTTDISPINFO = tagNMTTDISPINFOA;
  PNMTTDispInfoA = ^TNMTTDispInfoA;
  PNMTTDispInfoW = ^TNMTTDispInfoW;
  PNMTTDispInfo = PNMTTDispInfoA;
  TNMTTDispInfoA = tagNMTTDISPINFOA;
  TNMTTDispInfoW = tagNMTTDISPINFOW;
  TNMTTDispInfo = TNMTTDispInfoA;

  
  tagTOOLTIPTEXTA = tagNMTTDISPINFOA;
  
  tagTOOLTIPTEXTW = tagNMTTDISPINFOW;
  
  tagTOOLTIPTEXT = tagTOOLTIPTEXTA;
  
  TOOLTIPTEXTA = tagNMTTDISPINFOA;
  
  TOOLTIPTEXTW = tagNMTTDISPINFOW;
  
  TOOLTIPTEXT = TOOLTIPTEXTA;
  TToolTipTextA = tagNMTTDISPINFOA;
  TToolTipTextW = tagNMTTDISPINFOW;
  TToolTipText = TToolTipTextA;
  PToolTipTextA = ^TToolTipTextA;
  PToolTipTextW = ^TToolTipTextW;
  PToolTipText = PToolTipTextA;
{ ====== STATUS BAR CONTROL ================= }

const
  
  SBARS_SIZEGRIP          = $0100;


procedure DrawStatusTextA(hDC: HDC; lprc: PRect; pzsText: PAnsiChar;
  uFlags: UINT); stdcall;

procedure DrawStatusTextW(hDC: HDC; lprc: PRect; pzsText: PWideChar;
  uFlags: UINT); stdcall;

procedure DrawStatusText(hDC: HDC; lprc: PRect; pzsText: PChar;
  uFlags: UINT); stdcall;

function CreateStatusWindowA(Style: Longint; lpszText: PAnsiChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;

function CreateStatusWindowW(Style: Longint; lpszText: PWideChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;

function CreateStatusWindow(Style: Longint; lpszText: PChar;
  hwndParent: HWND; wID: UINT): HWND; stdcall;

const
  
  STATUSCLASSNAME = 'msctls_statusbar32';

const
  
  SB_SETTEXTA             = WM_USER+1;
  
  SB_GETTEXTA             = WM_USER+2;
  
  SB_GETTEXTLENGTHA       = WM_USER+3;
  
  SB_SETTIPTEXTA          = WM_USER+16;
  
  SB_GETTIPTEXTA          = WM_USER+18;

  
  SB_SETTEXTW             = WM_USER+11;
  
  SB_GETTEXTW             = WM_USER+13;
  
  SB_GETTEXTLENGTHW       = WM_USER+12;
  
  SB_SETTIPTEXTW          = WM_USER+17;
  
  SB_GETTIPTEXTW          = WM_USER+19;

  
  SB_SETTEXT             = SB_SETTEXTA;
  
  SB_GETTEXT             = SB_GETTEXTA;
  
  SB_GETTEXTLENGTH       = SB_GETTEXTLENGTHA;
  
  SB_SETTIPTEXT          = SB_SETTIPTEXTA;
  
  SB_GETTIPTEXT          = SB_GETTIPTEXTA;

  
  SB_SETPARTS             = WM_USER+4;
  
  SB_GETPARTS             = WM_USER+6;
  
  SB_GETBORDERS           = WM_USER+7;
  
  SB_SETMINHEIGHT         = WM_USER+8;
  
  SB_SIMPLE               = WM_USER+9;
  
  SB_GETRECT              = WM_USER + 10;
  
  SB_ISSIMPLE             = WM_USER+14;
  
  SB_SETICON              = WM_USER+15;
  
  SB_GETICON              = WM_USER+20;
  
  SB_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;
  
  SB_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;

  
  SBT_OWNERDRAW            = $1000;
  
  SBT_NOBORDERS            = $0100;
  
  SBT_POPOUT               = $0200;
  
  SBT_RTLREADING           = $0400;
  
  SBT_TOOLTIPS             = $0800;

  
  SB_SETBKCOLOR            = CCM_SETBKCOLOR;      // lParam = bkColor

  // status bar notifications
  
  SBN_SIMPLEMODECHANGE     = SBN_FIRST - 0;

{ ====== MENU HELP ========================== }


procedure MenuHelp(Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  hMainMenu: HMENU; hInst: THandle; hwndStatus: HWND; lpwIDs: PUINT); stdcall;

function ShowHideMenuCtl(hWnd: HWND; uFlags: UINT; lpInfo: PINT): Bool; stdcall;

procedure GetEffectiveClientRect(hWnd: HWND; lprc: PRect; lpInfo: PINT); stdcall;

const
  
  MINSYSCOMMAND   = SC_SIZE;


{ ====== TRACKBAR CONTROL =================== }

  
  TRACKBAR_CLASS = 'msctls_trackbar32';

const
  
  TBS_AUTOTICKS           = $0001;
  
  TBS_VERT                = $0002;
  
  TBS_HORZ                = $0000;
  
  TBS_TOP                 = $0004;
  
  TBS_BOTTOM              = $0000;
  
  TBS_LEFT                = $0004;
  
  TBS_RIGHT               = $0000;
  
  TBS_BOTH                = $0008;
  
  TBS_NOTICKS             = $0010;
  
  TBS_ENABLESELRANGE      = $0020;
  
  TBS_FIXEDLENGTH         = $0040;
  
  TBS_NOTHUMB             = $0080;
  
  TBS_TOOLTIPS            = $0100;

  
  TBM_GETPOS              = WM_USER;
  
  TBM_GETRANGEMIN         = WM_USER+1;
  
  TBM_GETRANGEMAX         = WM_USER+2;
  
  TBM_GETTIC              = WM_USER+3;
  
  TBM_SETTIC              = WM_USER+4;
  
  TBM_SETPOS              = WM_USER+5;
  
  TBM_SETRANGE            = WM_USER+6;
  
  TBM_SETRANGEMIN         = WM_USER+7;
  
  TBM_SETRANGEMAX         = WM_USER+8;
  
  TBM_CLEARTICS           = WM_USER+9;
  
  TBM_SETSEL              = WM_USER+10;
  
  TBM_SETSELSTART         = WM_USER+11;
  
  TBM_SETSELEND           = WM_USER+12;
  
  TBM_GETPTICS            = WM_USER+14;
  
  TBM_GETTICPOS           = WM_USER+15;
  
  TBM_GETNUMTICS          = WM_USER+16;
  
  TBM_GETSELSTART         = WM_USER+17;
  
  TBM_GETSELEND           = WM_USER+18;
  
  TBM_CLEARSEL            = WM_USER+19;
  
  TBM_SETTICFREQ          = WM_USER+20;
  
  TBM_SETPAGESIZE         = WM_USER+21;
  
  TBM_GETPAGESIZE         = WM_USER+22;
  
  TBM_SETLINESIZE         = WM_USER+23;
  
  TBM_GETLINESIZE         = WM_USER+24;
  
  TBM_GETTHUMBRECT        = WM_USER+25;
  
  TBM_GETCHANNELRECT      = WM_USER+26;
  
  TBM_SETTHUMBLENGTH      = WM_USER+27;
  
  TBM_GETTHUMBLENGTH      = WM_USER+28;
  
  TBM_SETTOOLTIPS         = WM_USER+29;
  
  TBM_GETTOOLTIPS         = WM_USER+30;
  
  TBM_SETTIPSIDE          = WM_USER+31;

  // TrackBar Tip Side flags
  
  TBTS_TOP                = 0;
  
  TBTS_LEFT               = 1;
  
  TBTS_BOTTOM             = 2;
  
  TBTS_RIGHT              = 3;

  
  TBM_SETBUDDY            = WM_USER+32; // wparam = BOOL fLeft; (or right)
  
  TBM_GETBUDDY            = WM_USER+33; // wparam = BOOL fLeft; (or right)
  
  TBM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  
  TBM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;

  
  TB_LINEUP               = 0;
  
  TB_LINEDOWN             = 1;
  
  TB_PAGEUP               = 2;
  
  TB_PAGEDOWN             = 3;
  
  TB_THUMBPOSITION        = 4;
  
  TB_THUMBTRACK           = 5;
  
  TB_TOP                  = 6;
  
  TB_BOTTOM               = 7;
  
  TB_ENDTRACK             = 8;

  // custom draw item specs
  
  TBCD_TICS    = $0001;
  
  TBCD_THUMB   = $0002;
  
  TBCD_CHANNEL = $0003;

{ ====== DRAG LIST CONTROL ================== }

type
  PDragListInfo = ^TDragListInfo;
  
  tagDRAGLISTINFO = packed record
    uNotification: UINT;
    hWnd: HWND;
    ptCursor: TPoint;
  end;
  TDragListInfo = tagDRAGLISTINFO;
  
  DRAGLISTINFO = tagDRAGLISTINFO;

const
  
  DL_BEGINDRAG            = WM_USER+133;
  
  DL_DRAGGING             = WM_USER+134;
  
  DL_DROPPED              = WM_USER+135;
  
  DL_CANCELDRAG           = WM_USER+136;

  
  DL_CURSORSET            = 0;
  
  DL_STOPCURSOR           = 1;
  
  DL_COPYCURSOR           = 2;
  
  DL_MOVECURSOR           = 3;

const
  
  DRAGLISTMSGSTRING = 'commctrl_DragListMsg';


procedure MakeDragList(hLB: HWND); stdcall;

procedure DrawInsert(hwndParent: HWND; hLB: HWND; nItem: Integer); stdcall;

function LBItemFromPt(hLB: HWND; pt: TPoint; bAutoScroll: Bool): Integer; stdcall;


{ ====== UPDOWN CONTROL ========================== }

const
  
  UPDOWN_CLASS = 'msctls_updown32';

type
  PUDAccel = ^TUDAccel;
  
  _UDACCEL = packed record
    nSec: UINT;
    nInc: UINT;
  end;
  TUDAccel = _UDACCEL;
  
  UDACCEL = _UDACCEL;

const
  
  UD_MAXVAL               = $7fff;
  
  UD_MINVAL               = -UD_MAXVAL;

  
  UDS_WRAP                = $0001;
  
  UDS_SETBUDDYINT         = $0002;
  
  UDS_ALIGNRIGHT          = $0004;
  
  UDS_ALIGNLEFT           = $0008;
  
  UDS_AUTOBUDDY           = $0010;
  
  UDS_ARROWKEYS           = $0020;
  
  UDS_HORZ                = $0040;
  
  UDS_NOTHOUSANDS         = $0080;
  
  UDS_HOTTRACK            = $0100;


  
  UDM_SETRANGE            = WM_USER+101;
  
  UDM_GETRANGE            = WM_USER+102;
  
  UDM_SETPOS              = WM_USER+103;
  
  UDM_GETPOS              = WM_USER+104;
  
  UDM_SETBUDDY            = WM_USER+105;
  
  UDM_GETBUDDY            = WM_USER+106;
  
  UDM_SETACCEL            = WM_USER+107;
  
  UDM_GETACCEL            = WM_USER+108;
  
  UDM_SETBASE             = WM_USER+109;
  
  UDM_GETBASE             = WM_USER+110;
  
  UDM_SETRANGE32          = WM_USER+111;
  
  UDM_GETRANGE32          = WM_USER+112; // wParam & lParam are LPINT
  
  UDM_SETUNICODEFORMAT    = CCM_SETUNICODEFORMAT;
  
  UDM_GETUNICODEFORMAT    = CCM_GETUNICODEFORMAT;


function CreateUpDownControl(dwStyle: Longint; X, Y, CX, CY: Integer;
  hParent: HWND;  nID: Integer; hInst: THandle; hBuddy: HWND;
  nUpper, nLower, nPos: Integer): HWND; stdcall;

type
  PNMUpDown = ^TNMUpDown;
  
  _NM_UPDOWN = packed record
    hdr: TNMHDR;
    iPos: Integer;
    iDelta: Integer;
  end;
  TNMUpDown = _NM_UPDOWN;
  
  NM_UPDOWN = _NM_UPDOWN;

const
  
  UDN_DELTAPOS = UDN_FIRST - 1;


{ ====== PROGRESS CONTROL ========================= }

const
  
  PROGRESS_CLASS = 'msctls_progress32';

type
  
  PBRANGE = record
    iLow: Integer;
    iHigh: Integer;
  end;
  PPBRange = ^TPBRange;
  TPBRange = PBRANGE;

const
  
  PBS_SMOOTH              = 01;
  
  PBS_VERTICAL            = 04;
  
  
  PBM_SETRANGE            = WM_USER+1;
  
  PBM_SETPOS              = WM_USER+2;
  
  PBM_DELTAPOS            = WM_USER+3;
  
  PBM_SETSTEP             = WM_USER+4;
  
  PBM_STEPIT              = WM_USER+5;
  
  PBM_SETRANGE32          = WM_USER+6;   // lParam = high, wParam = low
  
  PBM_GETRANGE            = WM_USER+7;   // lParam = PPBRange or Nil
					 // wParam = False: Result = high
					 // wParam = True: Result = low
  
  PBM_GETPOS              = WM_USER+8;
  
  PBM_SETBARCOLOR         = WM_USER+9;		// lParam = bar color
  
  PBM_SETBKCOLOR          = CCM_SETBKCOLOR;  // lParam = bkColor


{  ====== HOTKEY CONTROL ========================== }

const
  
  HOTKEYF_SHIFT           = $01;
  
  HOTKEYF_CONTROL         = $02;
  
  HOTKEYF_ALT             = $04;
  
  HOTKEYF_EXT             = $08;

  
  HKCOMB_NONE             = $0001;
  
  HKCOMB_S                = $0002;
  
  HKCOMB_C                = $0004;
  
  HKCOMB_A                = $0008;
  
  HKCOMB_SC               = $0010;
  
  HKCOMB_SA               = $0020;
  
  HKCOMB_CA               = $0040;
  
  HKCOMB_SCA              = $0080;


  
  HKM_SETHOTKEY           = WM_USER+1;
  
  HKM_GETHOTKEY           = WM_USER+2;
  
  HKM_SETRULES            = WM_USER+3;

const
  HOTKEYCLASS = 'msctls_hotkey32';


{ ====== COMMON CONTROL STYLES ================ }

const
  
  CCS_TOP                 = $00000001;
  
  CCS_NOMOVEY             = $00000002;
  
  CCS_BOTTOM              = $00000003;
  
  CCS_NORESIZE            = $00000004;
  
  CCS_NOPARENTALIGN       = $00000008;
  
  CCS_ADJUSTABLE          = $00000020;
  
  CCS_NODIVIDER           = $00000040;
  
  CCS_VERT                = $00000080;
  
  CCS_LEFT                = (CCS_VERT or CCS_TOP);
  
  CCS_RIGHT               = (CCS_VERT or CCS_BOTTOM);
  
  CCS_NOMOVEX             = (CCS_VERT or CCS_NOMOVEY);


{ ====== LISTVIEW CONTROL ====================== }

const
  
  WC_LISTVIEW = 'SysListView32';

const

  { List View Styles }
  
  LVS_ICON                = $0000;
  
  LVS_REPORT              = $0001;
  
  LVS_SMALLICON           = $0002;
  
  LVS_LIST                = $0003;
  
  LVS_TYPEMASK            = $0003;
  
  LVS_SINGLESEL           = $0004;
  
  LVS_SHOWSELALWAYS       = $0008;
  
  LVS_SORTASCENDING       = $0010;
  
  LVS_SORTDESCENDING      = $0020;
  
  LVS_SHAREIMAGELISTS     = $0040;
  
  LVS_NOLABELWRAP         = $0080;
  
  LVS_AUTOARRANGE         = $0100;
  
  LVS_EDITLABELS          = $0200;
  
  LVS_OWNERDATA           = $1000; 
  
  LVS_NOSCROLL            = $2000;

  
  LVS_TYPESTYLEMASK       = $FC00;

  
  LVS_ALIGNTOP            = $0000;
  
  LVS_ALIGNLEFT           = $0800;
  
  LVS_ALIGNMASK           = $0c00;

  
  LVS_OWNERDRAWFIXED      = $0400;
  
  LVS_NOCOLUMNHEADER      = $4000;
  
  LVS_NOSORTHEADER        = $8000;

  { List View Extended Styles }
  
  LVS_EX_GRIDLINES        = $00000001;
  
  LVS_EX_SUBITEMIMAGES    = $00000002;
  
  LVS_EX_CHECKBOXES       = $00000004;
  
  LVS_EX_TRACKSELECT      = $00000008;
  
  LVS_EX_HEADERDRAGDROP   = $00000010;
  
  LVS_EX_FULLROWSELECT    = $00000020; // applies to report mode only
  
  LVS_EX_ONECLICKACTIVATE = $00000040;
  
  LVS_EX_TWOCLICKACTIVATE = $00000080;
  
  LVS_EX_FLATSB           = $00000100;
  
  LVS_EX_REGIONAL         = $00000200;
  
  LVS_EX_INFOTIP          = $00000400; // listview does InfoTips for you
  
  LVS_EX_UNDERLINEHOT     = $00000800;
  
  LVS_EX_UNDERLINECOLD    = $00001000;
  
  LVS_EX_MULTIWORKAREAS   = $00002000;

const
  
  LVM_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;


function ListView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer;

const
  
  LVM_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;


function ListView_GetUnicodeFormat(hwnd: HWND): BOOL;

const
  
  LVM_GETBKCOLOR          = LVM_FIRST + 0;


function ListView_GetBkColor(hWnd: HWND): TColorRef;

const
  
  LVM_SETBKCOLOR          = LVM_FIRST + 1;


function ListView_SetBkColor(hWnd: HWND; clrBk: TColorRef): Bool;

const
  
  LVM_GETIMAGELIST        = LVM_FIRST + 2;


function ListView_GetImageList(hWnd: HWND; iImageList: Integer): HIMAGELIST;

const
  
  LVSIL_NORMAL            = 0;
  
  LVSIL_SMALL             = 1;
  
  LVSIL_STATE             = 2;

const
  
  LVM_SETIMAGELIST        = LVM_FIRST + 3;


function ListView_SetImageList(hWnd: HWND; himl: HIMAGELIST;
  iImageList: Integer): HIMAGELIST;

const
  
  LVM_GETITEMCOUNT        = LVM_FIRST + 4;


function ListView_GetItemCount(hWnd: HWND): Integer;

const
  
  LVIF_TEXT               = $0001;
  
  LVIF_IMAGE              = $0002;
  
  LVIF_PARAM              = $0004;
  
  LVIF_STATE              = $0008;
  
  LVIF_INDENT             = $0010;
  
  LVIF_NORECOMPUTE        = $0800;

  
  LVIS_FOCUSED            = $0001;
  
  LVIS_SELECTED           = $0002;
  
  LVIS_CUT                = $0004;
  
  LVIS_DROPHILITED        = $0008;
  
  LVIS_ACTIVATING         = $0020;

  
  LVIS_OVERLAYMASK        = $0F00;
  
  LVIS_STATEIMAGEMASK     = $F000;


function IndexToStateImageMask(I: Longint): Longint;

const
  
  I_INDENTCALLBACK        = -1;

type
  PLVItemA = ^TLVItemA;
  PLVItemW = ^TLVItemW;
  PLVItem = PLVItemW;

  tagLVITEMA = packed record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;
  end;

  tagLVITEMW = packed record
    mask: UINT;
    iItem: Integer;
    iSubItem: Integer;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
    iIndent: Integer;
  end;

  tagLVITEM = tagLVITEMW;

  _LV_ITEMA = tagLVITEMA;

  _LV_ITEMW = tagLVITEMW;

  _LV_ITEM = _LV_ITEMW;
  TLVItemA = tagLVITEMA;
  TLVItemW = tagLVITEMW;
  TLVItem = TLVItemW;

  LV_ITEMA = tagLVITEMA;

  LV_ITEMW = tagLVITEMW;

  LV_ITEM = LV_ITEMW;

const

  LPSTR_TEXTCALLBACKA = LPSTR(-1);

  LPSTR_TEXTCALLBACKW = LPWSTR(-1);


  LPSTR_TEXTCALLBACK = LPSTR_TEXTCALLBACKW;


  I_IMAGECALLBACK         = -1;

const

  LVM_GETITEMA            = LVM_FIRST + 5;

  LVM_SETITEMA            = LVM_FIRST + 6;

  LVM_INSERTITEMA         = LVM_FIRST + 7;

  
  LVM_GETITEMW            = LVM_FIRST + 75;
  
  LVM_SETITEMW            = LVM_FIRST + 76;
  
  LVM_INSERTITEMW         = LVM_FIRST + 77;

  
  LVM_GETITEM            = LVM_GETITEMW;

  LVM_SETITEM            = LVM_SETITEMW;

  LVM_INSERTITEM         = LVM_INSERTITEMW;


  LVM_DELETEITEM          = LVM_FIRST + 8;

  LVM_DELETEALLITEMS      = LVM_FIRST + 9;

  LVM_GETCALLBACKMASK     = LVM_FIRST + 10;

  LVM_SETCALLBACKMASK     = LVM_FIRST + 11;


function ListView_GetItemA(hWnd: HWND; var pItem: TLVItemA): Bool;

function ListView_GetItemW(hWnd: HWND; var pItem: TLVItemW): Bool;

function ListView_GetItem(hWnd: HWND; var pItem: TLVItem): Bool;

function ListView_SetItemA(hWnd: HWND; const pItem: TLVItemA): Bool;

function ListView_SetItemW(hWnd: HWND; const pItem: TLVItemW): Bool;

function ListView_SetItem(hWnd: HWND; const pItem: TLVItem): Bool;

function ListView_InsertItemA(hWnd: HWND; const pItem: TLVItemA): Integer;

function ListView_InsertItemW(hWnd: HWND; const pItem: TLVItemW): Integer;

function ListView_InsertItem(hWnd: HWND; const pItem: TLVItem): Integer;

function ListView_DeleteItem(hWnd: HWND; i: Integer): Bool;

function ListView_DeleteAllItems(hWnd: HWND): Bool;

function ListView_GetCallbackMask(hWnd: HWND): UINT;

function ListView_SetCallbackMask(hWnd: HWND; mask: UINT): Bool;

const
  
  LVNI_ALL                = $0000;
  
  LVNI_FOCUSED            = $0001;
  
  LVNI_SELECTED           = $0002;
  
  LVNI_CUT                = $0004;
  
  LVNI_DROPHILITED        = $0008;

  
  LVNI_ABOVE              = $0100;
  
  LVNI_BELOW              = $0200;
  
  LVNI_TOLEFT             = $0400;
  
  LVNI_TORIGHT            = $0800;


const
  
  LVM_GETNEXTITEM         = LVM_FIRST + 12;


function ListView_GetNextItem(hWnd: HWND; iStart: Integer; Flags: UINT): Integer;

const
  
  LVFI_PARAM              = $0001;
  
  LVFI_STRING             = $0002;
  
  LVFI_PARTIAL            = $0008;
  
  LVFI_WRAP               = $0020;
  
  LVFI_NEARESTXY          = $0040;


type
  PLVFindInfoA = ^TLVFindInfoA;
  PLVFindInfoW = ^TLVFindInfoW;
  PLVFindInfo = PLVFindInfoW;

  tagLVFINDINFOA = packed record
    flags: UINT;
    psz: PAnsiChar;
    lParam: LPARAM;
    pt: TPoint;
    vkDirection: UINT;
  end;

  tagLVFINDINFOW = packed record
    flags: UINT;
    psz: PWideChar;
    lParam: LPARAM;
    pt: TPoint;
    vkDirection: UINT;
  end;

  tagLVFINDINFO = tagLVFINDINFOW;

  _LV_FINDINFOA = tagLVFINDINFOA;

  _LV_FINDINFOW = tagLVFINDINFOW;

  _LV_FINDINFO = _LV_FINDINFOW;
  TLVFindInfoA = tagLVFINDINFOA;
  TLVFindInfoW = tagLVFINDINFOW;
  TLVFindInfo = TLVFindInfoW;

  LV_FINDINFOA = tagLVFINDINFOA;

  LV_FINDINFOW = tagLVFINDINFOW;

  LV_FINDINFO = LV_FINDINFOW;

const

  LVM_FINDITEMA            = LVM_FIRST + 13;

  LVM_FINDITEMW            = LVM_FIRST + 83;

  LVM_FINDITEM            = LVM_FINDITEMW;


function ListView_FindItemA(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoA): Integer;

function ListView_FindItemW(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoW): Integer;

function ListView_FindItem(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfo): Integer;

const

  LVIR_BOUNDS             = 0;

  LVIR_ICON               = 1;

  LVIR_LABEL              = 2;

  LVIR_SELECTBOUNDS       = 3;


const

  LVM_GETITEMRECT         = LVM_FIRST + 14;


function ListView_GetItemRect(hWnd: HWND; i: Integer; var prc: TRect;
  Code: Integer): Bool;

const

  LVM_SETITEMPOSITION     = LVM_FIRST + 15;


function ListView_SetItemPosition(hWnd: HWND; i, x, y: Integer): Bool;

const
  
  LVM_GETITEMPOSITION     = LVM_FIRST + 16;


function ListView_GetItemPosition(hwndLV: HWND; i: Integer; var ppt: TPoint): Bool;

const
  
  LVM_GETSTRINGWIDTHA      = LVM_FIRST + 17;
  
  LVM_GETSTRINGWIDTHW      = LVM_FIRST + 87;
  
  LVM_GETSTRINGWIDTH      = LVM_GETSTRINGWIDTHW;


function ListView_GetStringWidthA(hwndLV: HWND; psz: PAnsiChar): Integer;

function ListView_GetStringWidthW(hwndLV: HWND; psz: PWideChar): Integer;

function ListView_GetStringWidth(hwndLV: HWND; psz: PChar): Integer;

const

  LVHT_NOWHERE            = $0001;

  LVHT_ONITEMICON         = $0002;

  LVHT_ONITEMLABEL        = $0004;

  LVHT_ONITEMSTATEICON    = $0008;

  LVHT_ONITEM             = LVHT_ONITEMICON or LVHT_ONITEMLABEL or
			    LVHT_ONITEMSTATEICON;

  LVHT_ABOVE              = $0008;

  LVHT_BELOW              = $0010;

  LVHT_TORIGHT            = $0020;

  LVHT_TOLEFT             = $0040;

type
  PLVHitTestInfo = ^TLVHitTestInfo;

  tagLVHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    iItem: Integer;
    iSubItem: Integer;    // this is was NOT in win95.  valid only for LVM_SUBITEMHITTEST
  end;
  TLVHitTestInfo = tagLVHITTESTINFO;

  LV_HITTESTINFO = tagLVHITTESTINFO;

  _LV_HITTESTINFO = tagLVHITTESTINFO;

const

  LVM_HITTEST             = LVM_FIRST + 18;


function ListView_HitTest(hwndLV: HWND; var pinfo: TLVHitTestInfo): Integer;

const

  LVM_ENSUREVISIBLE       = LVM_FIRST + 19;


function ListView_EnsureVisible(hwndLV: HWND; i: Integer; fPartialOK: Bool): Bool;

const

  LVM_SCROLL              = LVM_FIRST + 20;


function ListView_Scroll(hwndLV: HWnd; DX, DY: Integer): Bool;

const

  LVM_REDRAWITEMS         = LVM_FIRST + 21;


function ListView_RedrawItems(hwndLV: HWND; iFirst, iLast: Integer): Bool;

const

  LVA_DEFAULT             = $0000;

  LVA_ALIGNLEFT           = $0001;

  LVA_ALIGNTOP            = $0002;
  LVA_ALIGNRIGHT          = $0003;
  LVA_ALIGNBOTTOM         = $0004;

  LVA_SNAPTOGRID          = $0005;

  LVA_SORTASCENDING       = $0100;
  LVA_SORTDESCENDING      = $0200;


  LVM_ARRANGE             = LVM_FIRST + 22;


function ListView_Arrange(hwndLV: HWND; Code: UINT): Bool;


const

  LVM_EDITLABELA           = LVM_FIRST + 23;

  LVM_EDITLABELW           = LVM_FIRST + 118;

  LVM_EDITLABEL           = LVM_EDITLABELW;


function ListView_EditLabelA(hwndLV: HWND; i: Integer): HWND;

function ListView_EditLabelW(hwndLV: HWND; i: Integer): HWND;

function ListView_EditLabel(hwndLV: HWND; i: Integer): HWND;

const

  LVM_GETEDITCONTROL      = LVM_FIRST + 24;


function ListView_GetEditControl(hwndLV: HWND): HWND;

type
  PLVColumnA = ^TLVColumnA;
  PLVColumnW = ^TLVColumnW;
  PLVColumn = PLVColumnW;

  tagLVCOLUMNA = packed record
    mask: UINT;
    fmt: Integer;
    cx: Integer;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: Integer;
    iOrder: Integer;
  end;

  tagLVCOLUMNW = packed record
    mask: UINT;
    fmt: Integer;
    cx: Integer;
    pszText: PWideChar;
    cchTextMax: Integer;
    iSubItem: Integer;
    iImage: Integer;
    iOrder: Integer;
  end;

  tagLVCOLUMN = tagLVCOLUMNW;

  _LV_COLUMNA = tagLVCOLUMNA;

  _LV_COLUMNW = tagLVCOLUMNW;

  _LV_COLUMN = _LV_COLUMNW;
  TLVColumnA = tagLVCOLUMNA;
  TLVColumnW = tagLVCOLUMNW;
  TLVColumn = TLVColumnW;

  LV_COLUMNA = tagLVCOLUMNA;

  LV_COLUMNW = tagLVCOLUMNW;

  LV_COLUMN = LV_COLUMNW;

const

  LVCF_FMT                = $0001;

  LVCF_WIDTH              = $0002;

  LVCF_TEXT               = $0004;

  LVCF_SUBITEM            = $0008;

  LVCF_IMAGE              = $0010;

  LVCF_ORDER              = $0020;


  LVCFMT_LEFT             = $0000;

  LVCFMT_RIGHT            = $0001;

  LVCFMT_CENTER           = $0002;

  LVCFMT_JUSTIFYMASK      = $0003;

  LVCFMT_IMAGE            = $0800;

  LVCFMT_BITMAP_ON_RIGHT  = $1000;

  LVCFMT_COL_HAS_IMAGES   = $8000;


  LVM_GETCOLUMNA          = LVM_FIRST + 25;

  LVM_GETCOLUMNW          = LVM_FIRST + 95;

  LVM_GETCOLUMN           = LVM_GETCOLUMNW;


function ListView_GetColumnA(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumnA): Bool;

function ListView_GetColumnW(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumnW): Bool;

function ListView_GetColumn(hwnd: HWND; iCol: Integer;
  var pcol: TLVColumn): Bool;

const

  LVM_SETCOLUMNA           = LVM_FIRST + 26;

  LVM_SETCOLUMNW           = LVM_FIRST + 96;

  LVM_SETCOLUMN           = LVM_SETCOLUMNW;


function ListView_SetColumnA(hwnd: HWnd; iCol: Integer; const pcol: TLVColumnA): Bool;

function ListView_SetColumnW(hwnd: HWnd; iCol: Integer; const pcol: TLVColumnW): Bool;

function ListView_SetColumn(hwnd: HWnd; iCol: Integer; const pcol: TLVColumn): Bool;

const

  LVM_INSERTCOLUMNA        = LVM_FIRST + 27;

  LVM_INSERTCOLUMNW        = LVM_FIRST + 97;

  LVM_INSERTCOLUMN        = LVM_INSERTCOLUMNW;


function ListView_InsertColumnA(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumnA): Integer;

function ListView_InsertColumnW(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumnW): Integer;

function ListView_InsertColumn(hwnd: HWND; iCol: Integer;
  const pcol: TLVColumn): Integer;

const

  LVM_DELETECOLUMN        = LVM_FIRST + 28;


function ListView_DeleteColumn(hwnd: HWND; iCol: Integer): Bool;

const

  LVM_GETCOLUMNWIDTH      = LVM_FIRST + 29;


function ListView_GetColumnWidth(hwnd: HWND; iCol: Integer): Integer;

const

  LVSCW_AUTOSIZE              = -1;

  LVSCW_AUTOSIZE_USEHEADER    = -2;

  LVM_SETCOLUMNWIDTH          = LVM_FIRST + 30;


function ListView_SetColumnWidth(hwnd: HWnd; iCol: Integer; cx: Integer): Bool;

const

  LVM_GETHEADER               = LVM_FIRST + 31;


function ListView_GetHeader(hwnd: HWND): HWND;

const

  LVM_CREATEDRAGIMAGE     = LVM_FIRST + 33;


function ListView_CreateDragImage(hwnd: HWND; i: Integer;
  const lpptUpLeft: TPoint): HIMAGELIST;

const

  LVM_GETVIEWRECT         = LVM_FIRST + 34;


function ListView_GetViewRect(hwnd: HWND; var prc: TRect): Bool;

const

  LVM_GETTEXTCOLOR        = LVM_FIRST + 35;


function ListView_GetTextColor(hwnd: HWND): TColorRef;

const

  LVM_SETTEXTCOLOR        = LVM_FIRST + 36;


function ListView_SetTextColor(hwnd: HWND; clrText: TColorRef): Bool;

const

  LVM_GETTEXTBKCOLOR      = LVM_FIRST + 37;


function ListView_GetTextBkColor(hwnd: HWND): TColorRef;

const

  LVM_SETTEXTBKCOLOR      = LVM_FIRST + 38;


function ListView_SetTextBkColor(hwnd: HWND; clrTextBk: TColorRef): Bool;

const

  LVM_GETTOPINDEX         = LVM_FIRST + 39;


function ListView_GetTopIndex(hwndLV: HWND): Integer;

const

  LVM_GETCOUNTPERPAGE     = LVM_FIRST + 40;


function ListView_GetCountPerPage(hwndLV: HWND): Integer;

const

  LVM_GETORIGIN           = LVM_FIRST + 41;


function ListView_GetOrigin(hwndLV: HWND; var ppt: TPoint): Bool;

const

  LVM_UPDATE              = LVM_FIRST + 42;


function ListView_Update(hwndLV: HWND; i: Integer): Bool;

const

  LVM_SETITEMSTATE        = LVM_FIRST + 43;


function ListView_SetItemState(hwndLV: HWND; i: Integer; data, mask: UINT): Bool;

const

  LVM_GETITEMSTATE        = LVM_FIRST + 44;


function ListView_GetItemState(hwndLV: HWND; i, mask: Integer): Integer;


function ListView_GetCheckState(hwndLV: HWND; i: Integer): UINT;
procedure ListView_SetCheckState(hwndLV: HWND; i: Integer; Checked: Boolean);

const

  LVM_GETITEMTEXTA         = LVM_FIRST + 45;

  LVM_GETITEMTEXTW         = LVM_FIRST + 115;

  LVM_GETITEMTEXT         = LVM_GETITEMTEXTW;


function ListView_GetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar; cchTextMax: Integer): Integer;

function ListView_GetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar; cchTextMax: Integer): Integer;

function ListView_GetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar; cchTextMax: Integer): Integer;

const

  LVM_SETITEMTEXTA         = LVM_FIRST + 46;

  LVM_SETITEMTEXTW         = LVM_FIRST + 116;

  LVM_SETITEMTEXT         = LVM_SETITEMTEXTW;


function ListView_SetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar): Bool;

function ListView_SetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar): Bool;

function ListView_SetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar): Bool;

const
  // these flags only apply to LVS_OWNERDATA listviews in report or list mode

  LVSICF_NOINVALIDATEALL  = $00000001;

  LVSICF_NOSCROLL         = $00000002;


  LVM_SETITEMCOUNT        = LVM_FIRST + 47;


procedure ListView_SetItemCount(hwndLV: HWND; cItems: Integer);


procedure ListView_SetItemCountEx(hwndLV: HWND; cItems: Integer; dwFlags: DWORD);

type

  PFNLVCOMPARE = function(lParam1, lParam2, lParamSort: Integer): Integer stdcall;
  TLVCompare = PFNLVCOMPARE;

const

  LVM_SORTITEMS           = LVM_FIRST + 48;


function ListView_SortItems(hwndLV: HWND; pfnCompare: TLVCompare;
  lPrm: Longint): Bool;

const

  LVM_SETITEMPOSITION32   = LVM_FIRST + 49;


procedure ListView_SetItemPosition32(hwndLV: HWND; i, x, y: Integer);

const

  LVM_GETSELECTEDCOUNT    = LVM_FIRST + 50;


function ListView_GetSelectedCount(hwndLV: HWND): UINT;

const
  
  LVM_GETITEMSPACING      = LVM_FIRST + 51;


function ListView_GetItemSpacing(hwndLV: HWND; fSmall: Integer): Longint;

const
  
  LVM_GETISEARCHSTRINGA    = LVM_FIRST + 52;
  
  LVM_GETISEARCHSTRINGW    = LVM_FIRST + 117;
  
  LVM_GETISEARCHSTRING    = LVM_GETISEARCHSTRINGW;


function ListView_GetISearchStringA(hwndLV: HWND; lpsz: PAnsiChar): Bool;

function ListView_GetISearchStringW(hwndLV: HWND; lpsz: PWideChar): Bool;

function ListView_GetISearchString(hwndLV: HWND; lpsz: PChar): Bool;

const

  LVM_SETICONSPACING      = LVM_FIRST + 53;

// -1 for cx and cy means we'll use the default (system settings)
// 0 for cx or cy means use the current setting (allows you to change just one param)

function ListView_SetIconSpacing(hwndLV: HWND; cx, cy: Word): DWORD;

const

  LVM_SETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 54;


function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL;

const

  LVM_GETEXTENDEDLISTVIEWSTYLE = LVM_FIRST + 55;


function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD;

const
  
  LVM_GETSUBITEMRECT      = LVM_FIRST + 56;


function ListView_GetSubItemRect(hwndLV: HWND; iItem, iSubItem: Integer;
  code: DWORD; prc: PRect): BOOL;

const
  
  LVM_SUBITEMHITTEST      = LVM_FIRST + 57;


function ListView_SubItemHitTest(hwndLV: HWND; plvhti: PLVHitTestInfo): Integer;

const
  
  LVM_SETCOLUMNORDERARRAY = LVM_FIRST + 58;


function ListView_SetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL;

const
  
  LVM_GETCOLUMNORDERARRAY = LVM_FIRST + 59;


function ListView_GetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL;

const
  
  LVM_SETHOTITEM  = LVM_FIRST + 60;


function ListView_SetHotItem(hwndLV: HWND; i: Integer): Integer;

const
  
  LVM_GETHOTITEM  = LVM_FIRST + 61;


function ListView_GetHotItem(hwndLV: HWND): Integer;

const
  
  LVM_SETHOTCURSOR  = LVM_FIRST + 62;


function ListView_SetHotCursor(hwndLV: HWND; hcur: HCURSOR): HCURSOR;

const
  
  LVM_GETHOTCURSOR  = LVM_FIRST + 63;


function ListView_GetHotCursor(hwndLV: HWND): HCURSOR;

const
  
  LVM_APPROXIMATEVIEWRECT = LVM_FIRST + 64;


function ListView_ApproximateViewRect(hwndLV: HWND; iWidth, iHeight: Word;
  iCount: Integer): DWORD;

const
  
  LV_MAX_WORKAREAS        = 16;
  
  LVM_SETWORKAREA         = LVM_FIRST + 65;


function ListView_SetWorkAreas(hwndLV: HWND; prc: PRect): BOOL;

const
  
  LVM_GETSELECTIONMARK    = LVM_FIRST + 66;


function ListView_GetSelectionMark(hwnd: HWND): Integer;

const
  
  LVM_SETSELECTIONMARK    = LVM_FIRST + 67;


function ListView_SetSelectionMark(hwnd: HWND; i: Integer): Integer;

const
  
  LVM_GETWORKAREAS        = LVM_FIRST + 70;


function ListView_GetWorkAreas(hwnd: HWND; nWorkAreas: Integer; prc: PRect): BOOL;

const
  
  LVM_SETHOVERTIME        = LVM_FIRST + 71;


function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD;

const
  
  LVM_GETHOVERTIME        = LVM_FIRST + 72;


function ListView_GetHoverTime(hwndLV: HWND): Integer;

const
  
  LVM_GETNUMBEROFWORKAREAS  = LVM_FIRST + 73;


function ListView_GetNumberOfWorkAreas(hwnd: HWND; pnWorkAreas: PInteger): Integer;

const
  
  LVM_SETTOOLTIPS       = LVM_FIRST + 74;


function ListView_SetToolTips(hwndLV: HWND; hwndNewHwnd: HWND): HWND;

const
  
  LVM_GETTOOLTIPS       = LVM_FIRST + 78;


function ListView_GetToolTips(hwndLV: HWND): HWND;

type

  tagLVBKIMAGEA = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PAnsiChar;
    cchImageMax: UINT;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;

  tagLVBKIMAGEW = packed record
    ulFlags: ULONG;              // LVBKIF_*
    hbm: HBITMAP;
    pszImage: PWideChar;
    cchImageMax: UINT;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;
  
  tagLVBKIMAGE = tagLVBKIMAGEA;
  PLVBKImageA = ^TLVBKImageA;
  PLVBKImageW = ^TLVBKImageW;
  PLVBKImage = PLVBKImageW;
  TLVBKImageA = tagLVBKIMAGEA;
  TLVBKImageW = tagLVBKIMAGEW;
  TLVBKImage = TLVBKImageW;

const

  LVBKIF_SOURCE_NONE      = $00000000;

  LVBKIF_SOURCE_HBITMAP   = $00000001;

  LVBKIF_SOURCE_URL       = $00000002;

  LVBKIF_SOURCE_MASK      = $00000003;

  LVBKIF_STYLE_NORMAL     = $00000000;

  LVBKIF_STYLE_TILE       = $00000010;

  LVBKIF_STYLE_MASK       = $00000010;


  LVM_SETBKIMAGEA         = LVM_FIRST + 68;

  LVM_SETBKIMAGEW         = LVM_FIRST + 138;

  LVM_GETBKIMAGEA         = LVM_FIRST + 69;

  LVM_GETBKIMAGEW         = LVM_FIRST + 139;


  LVM_SETBKIMAGE = LVM_SETBKIMAGEW;

  LVM_GETBKIMAGE = LVM_GETBKIMAGEW;


function ListView_SetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL;


function ListView_GetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL;

type

  tagNMLISTVIEW = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
  end;

  _NM_LISTVIEW = tagNMLISTVIEW;

  NM_LISTVIEW = tagNMLISTVIEW;
  PNMListView = ^TNMListView;
  TNMListView = tagNMLISTVIEW;

  // NMITEMACTIVATE is used instead of NMLISTVIEW in IE >= 0x400
  // therefore all the fields are the same except for extra uKeyFlags
  // they are used to store key flags at the time of the single click with
  // delayed activation - because by the time the timer goes off a user may
  // not hold the keys (shift, ctrl) any more

  tagNMITEMACTIVATE = packed record
    hdr: TNMHdr;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
    uKeyFlags: UINT;
  end;
  PNMItemActivate = ^TNMItemActivate;
  TNMItemActivate = tagNMITEMACTIVATE;

const
  // key flags stored in uKeyFlags

  LVKF_ALT       = $0001;

  LVKF_CONTROL   = $0002;

  LVKF_SHIFT     = $0004;

type

  tagNMLVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iSubItem: Integer;
  end;
  PNMLVCustomDraw = ^TNMLVCustomDraw;
  TNMLVCustomDraw = tagNMLVCUSTOMDRAW;


  tagNMLVCACHEHINT = packed record
    hdr: TNMHDR;
    iFrom: Integer;
    iTo: Integer;
  end;
  PNMLVCacheHint = ^TNMLVCacheHint;
  TNMLVCacheHint = tagNMLVCACHEHINT;
  PNMCacheHint = ^TNMCacheHint;
  TNMCacheHint = tagNMLVCACHEHINT;


  tagNMLVFINDITEM = packed record
    hdr: TNMHdr;
    iStart: Integer;
    lvfi: TLVFindInfo;
  end;
  PNMLVFinditem = ^TNMLVFinditem;
  TNMLVFinditem = tagNMLVFINDITEM;
  PNMFinditem = ^TNMFinditem;
  TNMFinditem = tagNMLVFINDITEM;


  tagNMLVODSTATECHANGE = packed record
    hdr: TNMHdr;
    iFrom: Integer;
    iTo: Integer;
    uNewState: UINT;
    uOldState: UINT;
  end;
  PNMLVODStateChange = ^TNMLVODStateChange;
  TNMLVODStateChange = tagNMLVODSTATECHANGE;

const

  LVN_ITEMCHANGING        = LVN_FIRST-0;

  LVN_ITEMCHANGED         = LVN_FIRST-1;

  LVN_INSERTITEM          = LVN_FIRST-2;

  LVN_DELETEITEM          = LVN_FIRST-3;

  LVN_DELETEALLITEMS      = LVN_FIRST-4;

  LVN_COLUMNCLICK         = LVN_FIRST-8;

  LVN_BEGINDRAG           = LVN_FIRST-9;

  LVN_BEGINRDRAG          = LVN_FIRST-11;


  LVN_ODCACHEHINT         = LVN_FIRST-13;

  LVN_ODFINDITEMA         = LVN_FIRST-52;

  LVN_ODFINDITEMW         = LVN_FIRST-79;


  LVN_ITEMACTIVATE        = LVN_FIRST-14;

  LVN_ODSTATECHANGED      = LVN_FIRST-15;


  LVN_ODFINDITEM          = LVN_ODFINDITEMW;


  LVN_BEGINLABELEDITA      = LVN_FIRST-5;

  LVN_ENDLABELEDITA        = LVN_FIRST-6;

  LVN_BEGINLABELEDITW      = LVN_FIRST-75;

  LVN_ENDLABELEDITW        = LVN_FIRST-76;

  LVN_BEGINLABELEDIT      = LVN_BEGINLABELEDITW;

  LVN_ENDLABELEDIT        = LVN_ENDLABELEDITW;


  LVN_HOTTRACK            = LVN_FIRST-21;


  LVN_GETDISPINFOA        = LVN_FIRST-50;

  LVN_SETDISPINFOA        = LVN_FIRST-51;

  LVN_GETDISPINFOW        = LVN_FIRST-77;

  LVN_SETDISPINFOW        = LVN_FIRST-78;

  LVN_GETDISPINFO        = LVN_GETDISPINFOW;

  LVN_SETDISPINFO        = LVN_SETDISPINFOW;


  LVIF_DI_SETITEM         = $1000;

type
  PLVDispInfoA = ^TLVDispInfoA;
  PLVDispInfoW = ^TLVDispInfoW;
  PLVDispInfo = PLVDispInfoW;

  tagLVDISPINFO = packed record
    hdr: TNMHDR;
    item: TLVItemA;
  end;

  _LV_DISPINFO = tagLVDISPINFO;

  tagLVDISPINFOW = packed record
    hdr: TNMHDR;
    item: TLVItemW;
  end;

  _LV_DISPINFOW = tagLVDISPINFOW;
  TLVDispInfoA = tagLVDISPINFO;
  TLVDispInfoW = tagLVDISPINFOW;
  TLVDispInfo = TLVDispInfoW;

  LV_DISPINFOA = tagLVDISPINFO;

  LV_DISPINFOW = tagLVDISPINFOW;

  LV_DISPINFO = LV_DISPINFOW;

const

  LVN_KEYDOWN             = LVN_FIRST-55;

type
  PLVKeyDown = ^TLVKeyDown;

  tagLVKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;

  _LV_KEYDOWN = tagLVKEYDOWN;
  TLVKeyDown = tagLVKEYDOWN;

  LV_KEYDOWN = tagLVKEYDOWN;

const

  LVN_MARQUEEBEGIN        = LVN_FIRST-56;

type

  tagNMLVGETINFOTIPA = packed record
    hdr: TNMHdr;
    dwFlags: DWORD;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: LPARAM;
  end;

  tagNMLVGETINFOTIPW = packed record
    hdr: TNMHdr;
    dwFlags: DWORD;
    pszText: PWideChar;
    cchTextMax: Integer;
    iItem: Integer;
    iSubItem: Integer;
    lParam: LPARAM;
  end;

  tagNMLVGETINFOTIP = tagNMLVGETINFOTIPA;
  PNMLVGetInfoTipA = ^TNMLVGetInfoTipA;
  PNMLVGetInfoTipW = ^TNMLVGetInfoTipW;
  PNMLVGetInfoTip = PNMLVGetInfoTipW;
  TNMLVGetInfoTipA = tagNMLVGETINFOTIPA;
  TNMLVGetInfoTipW = tagNMLVGETINFOTIPW;
  TNMLVGetInfoTip = TNMLVGetInfoTipW;

const
  // NMLVGETINFOTIPA.dwFlag values

  LVGIT_UNFOLDED  = $0001;


  LVN_GETINFOTIPA          = LVN_FIRST-57;

  LVN_GETINFOTIPW          = LVN_FIRST-58;


  LVN_GETINFOTIP          = LVN_GETINFOTIPA;

{ ====== TREEVIEW CONTROL =================== }

const

  WC_TREEVIEW = 'SysTreeView32';

const

  TVS_HASBUTTONS          = $0001;

  TVS_HASLINES            = $0002;

  TVS_LINESATROOT         = $0004;

  TVS_EDITLABELS          = $0008;

  TVS_DISABLEDRAGDROP     = $0010;

  TVS_SHOWSELALWAYS       = $0020;

  TVS_RTLREADING          = $0040;

  TVS_NOTOOLTIPS          = $0080;

  TVS_CHECKBOXES          = $0100;

  TVS_TRACKSELECT         = $0200;

  TVS_SINGLEEXPAND        = $0400;

  TVS_INFOTIP             = $0800;

  TVS_FULLROWSELECT       = $1000;

  TVS_NOSCROLL            = $2000;
  
  TVS_NONEVENHEIGHT       = $4000;

type
  
  HTREEITEM = ^_TREEITEM;
  
  _TREEITEM = packed record
  end;

const
  
  TVIF_TEXT               = $0001;
  
  TVIF_IMAGE              = $0002;
  
  TVIF_PARAM              = $0004;
  
  TVIF_STATE              = $0008;
  
  TVIF_HANDLE             = $0010;
  
  TVIF_SELECTEDIMAGE      = $0020;
  
  TVIF_CHILDREN           = $0040;
  
  TVIF_INTEGRAL           = $0080;

  
  TVIS_FOCUSED            = $0001;
  
  TVIS_SELECTED           = $0002;
  
  TVIS_CUT                = $0004;
  
  TVIS_DROPHILITED        = $0008;
  
  TVIS_BOLD               = $0010;

  TVIS_EXPANDED           = $0020;
  
  TVIS_EXPANDEDONCE       = $0040;
  
  TVIS_EXPANDPARTIAL      = $0080;

  
  TVIS_OVERLAYMASK        = $0F00;
  
  TVIS_STATEIMAGEMASK     = $F000;
  
  TVIS_USERMASK           = $F000;


const
  
  I_CHILDRENCALLBACK  = -1;

type
  PTVItemA = ^TTVItemA;
  PTVItemW = ^TTVItemW;
  PTVItem = PTVItemW;

  tagTVITEMA = packed record
    mask: UINT;
    hItem: HTreeItem;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
  end;

  tagTVITEMW = packed record
    mask: UINT;
    hItem: HTreeItem;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
  end;

  tagTVITEM = tagTVITEMA;

  _TV_ITEMA = tagTVITEMA;

  _TV_ITEMW = tagTVITEMW;

  _TV_ITEM = _TV_ITEMW;
  TTVItemA = tagTVITEMA;
  TTVItemW = tagTVITEMW;
  TTVItem = TTVItemW;

  TV_ITEMA = tagTVITEMA;

  TV_ITEMW = tagTVITEMW;

  TV_ITEM = TV_ITEMw;

  // only used for Get and Set messages.  no notifies

  tagTVITEMEXA = packed record
    mask: UINT;
    hItem: HTREEITEM;
    state: UINT;
    stateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
    iIntegral: Integer;
  end;

  tagTVITEMEXW = packed record
    mask: UINT;
    hItem: HTREEITEM;
    state: UINT;
    stateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    cChildren: Integer;
    lParam: LPARAM;
    iIntegral: Integer;
  end;

  tagTVITEMEX = tagTVITEMEXA;
  PTVItemExA = ^TTVItemExA;
  PTVItemExW = ^TTVItemExW;
  PTVItemEx = PTVItemExW;
  TTVItemExA = tagTVITEMEXA;
  TTVItemExW = tagTVITEMEXW;
  TTVItemEx = TTVItemExW;

const

  TVI_ROOT                = HTreeItem($FFFF0000);

  TVI_FIRST               = HTreeItem($FFFF0001);

  TVI_LAST                = HTreeItem($FFFF0002);

  TVI_SORT                = HTreeItem($FFFF0003);

type
  PTVInsertStructA = ^TTVInsertStructA;
  PTVInsertStructW = ^TTVInsertStructW;
  PTVInsertStruct = PTVInsertStructW;

  tagTVINSERTSTRUCTA = packed record
    hParent: HTreeItem;
    hInsertAfter: HTreeItem;
    case Integer of
      0: (itemex: TTVItemExA);
      1: (item: TTVItemA);
  end;

  tagTVINSERTSTRUCTW = packed record
    hParent: HTreeItem;
    hInsertAfter: HTreeItem;
    case Integer of
      0: (itemex: TTVItemExW);
      1: (item: TTVItemW);
  end;

  tagTVINSERTSTRUCT = tagTVINSERTSTRUCTW;

  _TV_INSERTSTRUCTA = tagTVINSERTSTRUCTA;

  _TV_INSERTSTRUCTW = tagTVINSERTSTRUCTW;

  _TV_INSERTSTRUCT = _TV_INSERTSTRUCTW;
  TTVInsertStructA = tagTVINSERTSTRUCTA;
  TTVInsertStructW = tagTVINSERTSTRUCTW;
  TTVInsertStruct = TTVInsertStructW;

  TV_INSERTSTRUCTA = tagTVINSERTSTRUCTA;

  TV_INSERTSTRUCTW = tagTVINSERTSTRUCTW;

  TV_INSERTSTRUCT = TV_INSERTSTRUCTW;

const

  TVM_INSERTITEMA          = TV_FIRST + 0;

  TVM_INSERTITEMW          = TV_FIRST + 50;

  TVM_INSERTITEM          = TVM_INSERTITEMW;


function TreeView_InsertItem(hwnd: HWND; const lpis: TTVInsertStruct): HTreeItem;

const

  TVM_DELETEITEM          = TV_FIRST + 1;


function TreeView_DeleteItem(hwnd: HWND; hitem: HTreeItem): Bool;


function TreeView_DeleteAllItems(hwnd: HWND): Bool;

const

  TVM_EXPAND              = TV_FIRST + 2;


function TreeView_Expand(hwnd: HWND; hitem: HTreeItem; code: Integer): Bool;

const

  TVE_COLLAPSE            = $0001;

  TVE_EXPAND              = $0002;

  TVE_TOGGLE              = $0003;
  
  TVE_EXPANDPARTIAL       = $4000;
  
  TVE_COLLAPSERESET       = $8000;

const
  
  TVM_GETITEMRECT         = TV_FIRST + 4;


function TreeView_GetItemRect(hwnd: HWND; hitem: HTreeItem;
  var prc: TRect; code: Bool): Bool;

const
  
  TVM_GETCOUNT            = TV_FIRST + 5;


function TreeView_GetCount(hwnd: HWND): UINT;

const
  
  TVM_GETINDENT           = TV_FIRST + 6;


function TreeView_GetIndent(hwnd: HWND): UINT;

const
  
  TVM_SETINDENT           = TV_FIRST + 7;


function TreeView_SetIndent(hwnd: HWND; indent: Integer): Bool;

const
  
  TVM_GETIMAGELIST        = TV_FIRST + 8;


function TreeView_GetImageList(hwnd: HWND; iImage: Integer): HIMAGELIST;

const
  
  TVSIL_NORMAL            = 0;
  
  TVSIL_STATE             = 2;


const
  
  TVM_SETIMAGELIST        = TV_FIRST + 9;


function TreeView_SetImageList(hwnd: HWND; himl: HIMAGELIST;
  iImage: Integer): HIMAGELIST;

const
  
  TVM_GETNEXTITEM         = TV_FIRST + 10;


function TreeView_GetNextItem(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem;

const
  
  TVGN_ROOT               = $0000;
  
  TVGN_NEXT               = $0001;
  
  TVGN_PREVIOUS           = $0002;
  
  TVGN_PARENT             = $0003;
  
  TVGN_CHILD              = $0004;
  
  TVGN_FIRSTVISIBLE       = $0005;
  
  TVGN_NEXTVISIBLE        = $0006;
  
  TVGN_PREVIOUSVISIBLE    = $0007;
  
  TVGN_DROPHILITE         = $0008;
  
  TVGN_CARET              = $0009;
  
  TVGN_LASTVISIBLE        = $000A;


function TreeView_GetChild(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetNextSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetPrevSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetParent(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetFirstVisible(hwnd: HWND): HTreeItem;

function TreeView_GetNextVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetPrevVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_GetSelection(hwnd: HWND): HTreeItem;

function TreeView_GetDropHilite(hwnd: HWND): HTreeItem;

function TreeView_GetRoot(hwnd: HWND): HTreeItem;

function TreeView_GetLastVisible(hwnd: HWND): HTreeItem;

const
  
  TVM_SELECTITEM          = TV_FIRST + 11;


function TreeView_Select(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem;


function TreeView_SelectItem(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_SelectDropTarget(hwnd: HWND; hitem: HTreeItem): HTreeItem;

function TreeView_SelectSetFirstVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;

const
  
  TVM_GETITEMA             = TV_FIRST + 12;
  
  TVM_GETITEMW             = TV_FIRST + 62;
  
  TVM_GETITEM             = TVM_GETITEMW;


function TreeView_GetItemA(hwnd: HWND; var pitem: TTVItemA): Bool;

function TreeView_GetItemW(hwnd: HWND; var pitem: TTVItemW): Bool;

function TreeView_GetItem(hwnd: HWND; var pitem: TTVItem): Bool;

const

  TVM_SETITEMA             = TV_FIRST + 13;

  TVM_SETITEMW             = TV_FIRST + 63;

  TVM_SETITEM             = TVM_SETITEMW;


function TreeView_SetItemA(hwnd: HWND; const pitem: TTVItemA): Bool;

function TreeView_SetItemW(hwnd: HWND; const pitem: TTVItemW): Bool;

function TreeView_SetItem(hwnd: HWND; const pitem: TTVItem): Bool;

const
  
  TVM_EDITLABELA           = TV_FIRST + 14;
  
  TVM_EDITLABELW           = TV_FIRST + 65;
  
  TVM_EDITLABEL           = TVM_EDITLABELW;


function TreeView_EditLabelA(hwnd: HWND; hitem: HTreeItem): HWND;

function TreeView_EditLabelW(hwnd: HWND; hitem: HTreeItem): HWND;

function TreeView_EditLabel(hwnd: HWND; hitem: HTreeItem): HWND;

const

  TVM_GETEDITCONTROL      = TV_FIRST + 15;


function TreeView_GetEditControl(hwnd: HWND): HWND;


const

  TVM_GETVISIBLECOUNT     = TV_FIRST + 16;


function TreeView_GetVisibleCount(hwnd: HWND): UINT;

const

  TVM_HITTEST             = TV_FIRST + 17;

type
  PTVHitTestInfo = ^TTVHitTestInfo;

  tagTVHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
    hItem: HTreeItem;
  end;

  _TV_HITTESTINFO = tagTVHITTESTINFO;
  TTVHitTestInfo = tagTVHITTESTINFO;

  TV_HITTESTINFO = tagTVHITTESTINFO;


function TreeView_HitTest(hwnd: HWND; var lpht: TTVHitTestInfo): HTreeItem;

const

  TVHT_NOWHERE            = $0001;

  TVHT_ONITEMICON         = $0002;

  TVHT_ONITEMLABEL        = $0004;

  TVHT_ONITEMINDENT       = $0008;

  TVHT_ONITEMBUTTON       = $0010;
  
  TVHT_ONITEMRIGHT        = $0020;
  
  TVHT_ONITEMSTATEICON    = $0040;

  
  TVHT_ONITEM             = TVHT_ONITEMICON or TVHT_ONITEMLABEL or
			      TVHT_ONITEMSTATEICON;

  
  TVHT_ABOVE              = $0100;
  
  TVHT_BELOW              = $0200;
  
  TVHT_TORIGHT            = $0400;
  
  TVHT_TOLEFT             = $0800;

const
  
  TVM_CREATEDRAGIMAGE     = TV_FIRST + 18;


function TreeView_CreateDragImage(hwnd: HWND; hitem: HTreeItem): HIMAGELIST;

const
  
  TVM_SORTCHILDREN        = TV_FIRST + 19;


function TreeView_SortChildren(hwnd: HWND; hitem: HTreeItem;
  recurse: Integer): Bool;

const
  
  TVM_ENSUREVISIBLE       = TV_FIRST + 20;


function TreeView_EnsureVisible(hwnd: HWND; hitem: HTreeItem): Bool;

const
  
  TVM_SORTCHILDRENCB      = TV_FIRST + 21;

type

  {$ifdef DFS_COMPILER_3_UP}
    PFNTVCOMPARE = function(lParam1, lParam2, lParamSort: Longint): Integer stdcall;
  {$else}
    PFNTVCOMPARE = function(lParam1, lParam2, lParamSort: Longint): Longint stdcall;
  {$endif}
  TTVCompare = PFNTVCOMPARE;

type
  
  tagTVSORTCB = packed record
    hParent: HTreeItem;
    lpfnCompare: TTVCompare;
    lParam: LPARAM;
  end;
  
  _TV_SORTCB = tagTVSORTCB;
  TTVSortCB = tagTVSORTCB;
  
  TV_SORTCB = tagTVSORTCB;


function TreeView_SortChildrenCB(hwnd: HWND; const psort: TTVSortCB;
  recurse: Integer): Bool;

const
  
  TVM_ENDEDITLABELNOW     = TV_FIRST + 22;


function TreeView_EndEditLabelNow(hwnd: HWND; fCancel: Bool): Bool;

const
  
  TVM_GETISEARCHSTRINGA    = TV_FIRST + 23;
  
  TVM_GETISEARCHSTRINGW    = TV_FIRST + 64;
  
  TVM_GETISEARCHSTRING    = TVM_GETISEARCHSTRINGW;


function TreeView_GetISearchStringA(hwndTV: HWND; lpsz: PAnsiChar): Bool;

function TreeView_GetISearchStringW(hwndTV: HWND; lpsz: PWideChar): Bool;

function TreeView_GetISearchString(hwndTV: HWND; lpsz: PChar): Bool;

const

  TVM_SETTOOLTIPS         = TV_FIRST + 24;


function TreeView_SetToolTips(wnd: HWND; hwndTT: HWND): HWND;

const

  TVM_GETTOOLTIPS         = TV_FIRST + 25;


function TreeView_GetToolTips(wnd: HWND): HWND;

const

  TVM_SETINSERTMARK       = TV_FIRST + 26;


function TreeView_SetInsertMark(hwnd: HWND; hItem: Integer; fAfter: BOOL): BOOL;

const

  TVM_SETUNICODEFORMAT     = CCM_SETUNICODEFORMAT;


function TreeView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;

const
  
  TVM_GETUNICODEFORMAT     = CCM_GETUNICODEFORMAT;


function TreeView_GetUnicodeFormat(hwnd: HWND): BOOL;

const
  
  TVM_SETITEMHEIGHT         = TV_FIRST + 27;


function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;

const
  
  TVM_GETITEMHEIGHT         = TV_FIRST + 28;


function TreeView_GetItemHeight(hwnd: HWND): Integer;

const
  
  TVM_SETBKCOLOR              = TV_FIRST + 29;


function TreeView_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF;

const
  
  TVM_SETTEXTCOLOR              = TV_FIRST + 30;


function TreeView_SetTextColor(hwnd: HWND; clr: COLORREF): COLORREF;

const
  
  TVM_GETBKCOLOR              = TV_FIRST + 31;


function TreeView_GetBkColor(hwnd: HWND): COLORREF;

const
  
  TVM_GETTEXTCOLOR              = TV_FIRST + 32;


function TreeView_GetTextColor(hwnd: HWND): COLORREF;

const
  
  TVM_SETSCROLLTIME              = TV_FIRST + 33;


function TreeView_SetScrollTime(hwnd: HWND; uTime: UINT): UINT;

const
  
  TVM_GETSCROLLTIME              = TV_FIRST + 34;


function TreeView_GetScrollTime(hwnd: HWND): UINT;

const
  
  TVM_SETINSERTMARKCOLOR         = TV_FIRST + 37;


function TreeView_SetInsertMarkColor(hwnd: HWND; clr: COLORREF): COLORREF;

const

  TVM_GETINSERTMARKCOLOR         = TV_FIRST + 38;


function TreeView_GetInsertMarkColor(hwnd: HWND): COLORREF;

type
  PNMTreeViewA = ^TNMTreeViewA;
  PNMTreeViewW = ^TNMTreeViewW;
  PNMTreeView = PNMTreeViewW;

  tagNMTREEVIEWA = packed record
    hdr: TNMHDR;
    action: UINT;
    itemOld: TTVItemA;
    itemNew: TTVItemA;
    ptDrag: TPoint;
  end;

  tagNMTREEVIEWW = packed record
    hdr: TNMHDR;
    action: UINT;
    itemOld: TTVItemW;
    itemNew: TTVItemW;
    ptDrag: TPoint;
  end;

  tagNMTREEVIEW = tagNMTREEVIEWA;

  _NM_TREEVIEWA = tagNMTREEVIEWA;

  _NM_TREEVIEWW = tagNMTREEVIEWW;

  _NM_TREEVIEW = _NM_TREEVIEWW;
  TNMTreeViewA  = tagNMTREEVIEWA;
  TNMTreeViewW  = tagNMTREEVIEWW;
  TNMTreeView = TNMTreeViewW;

  NM_TREEVIEWA  = tagNMTREEVIEWA;

  NM_TREEVIEWW  = tagNMTREEVIEWW;

  NM_TREEVIEW = NM_TREEVIEWW;

const

  TVN_SELCHANGINGA         = TVN_FIRST-1;

  TVN_SELCHANGEDA          = TVN_FIRST-2;

  TVN_SELCHANGINGW         = TVN_FIRST-50;

  TVN_SELCHANGEDW          = TVN_FIRST-51;

  TVN_SELCHANGING         = TVN_SELCHANGINGW;

  TVN_SELCHANGED          = TVN_SELCHANGEDW;

const

  TVC_UNKNOWN             = $0000;

  TVC_BYMOUSE             = $0001;

  TVC_BYKEYBOARD          = $0002;

const

  TVN_GETDISPINFOA         = TVN_FIRST-3;

  TVN_SETDISPINFOA         = TVN_FIRST-4;

  TVN_GETDISPINFOW         = TVN_FIRST-52;

  TVN_SETDISPINFOW         = TVN_FIRST-53;

  TVN_GETDISPINFO         = TVN_GETDISPINFOW;

  TVN_SETDISPINFO         = TVN_SETDISPINFOW;


  TVIF_DI_SETITEM         = $1000;

type
  PTVDispInfoA = ^TTVDispInfoA;
  PTVDispInfoW = ^TTVDispInfoW;
  PTVDispInfo = PTVDispInfoW;

  tagTVDISPINFOA = packed record
    hdr: TNMHDR;
    item: TTVItemA;
  end;

  tagTVDISPINFOW = packed record
    hdr: TNMHDR;
    item: TTVItemW;
  end;

  tagTVDISPINFO = tagTVDISPINFOW;

  _TV_DISPINFOA = tagTVDISPINFOA;

  _TV_DISPINFOW = tagTVDISPINFOW;

  _TV_DISPINFO = _TV_DISPINFOW;
  TTVDispInfoA = tagTVDISPINFOA;
  TTVDispInfoW = tagTVDISPINFOW;
  TTVDispInfo = TTVDispInfoW;

  TV_DISPINFOA = tagTVDISPINFOA;

  TV_DISPINFOW = tagTVDISPINFOW;

  TV_DISPINFO = TV_DISPINFOW;

const

  TVN_ITEMEXPANDINGA       = TVN_FIRST-5;

  TVN_ITEMEXPANDEDA        = TVN_FIRST-6;

  TVN_BEGINDRAGA           = TVN_FIRST-7;

  TVN_BEGINRDRAGA          = TVN_FIRST-8;

  TVN_DELETEITEMA          = TVN_FIRST-9;

  TVN_BEGINLABELEDITA      = TVN_FIRST-10;

  TVN_ENDLABELEDITA        = TVN_FIRST-11;

  TVN_GETINFOTIPA          = TVN_FIRST-13;

  TVN_ITEMEXPANDINGW       = TVN_FIRST-54;

  TVN_ITEMEXPANDEDW        = TVN_FIRST-55;

  TVN_BEGINDRAGW           = TVN_FIRST-56;

  TVN_BEGINRDRAGW          = TVN_FIRST-57;

  TVN_DELETEITEMW          = TVN_FIRST-58;

  TVN_BEGINLABELEDITW      = TVN_FIRST-59;

  TVN_ENDLABELEDITW        = TVN_FIRST-60;

  TVN_GETINFOTIPW          = TVN_FIRST-14;

  TVN_ITEMEXPANDING       = TVN_ITEMEXPANDINGW;

  TVN_ITEMEXPANDED        = TVN_ITEMEXPANDEDW;

  TVN_BEGINDRAG           = TVN_BEGINDRAGW;

  TVN_BEGINRDRAG          = TVN_BEGINRDRAGW;

  TVN_DELETEITEM          = TVN_DELETEITEMW;

  TVN_BEGINLABELEDIT      = TVN_BEGINLABELEDITW;

  TVN_ENDLABELEDIT        = TVN_ENDLABELEDITW;

  TVN_GETINFOTIP         = TVN_GETINFOTIPW;

const

  TVN_KEYDOWN             = TVN_FIRST-12;

  TVN_SINGLEEXPAND        = TVN_FIRST-15;

type

  tagTVKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;

  _TV_KEYDOWN = tagTVKEYDOWN;
  TTVKeyDown = tagTVKEYDOWN;

  TV_KEYDOWN = tagTVKEYDOWN;


  tagNMTVCUSTOMDRAW = packed record
    nmcd: TNMCustomDraw;
    clrText: COLORREF;
    clrTextBk: COLORREF;
    iLevel: Integer;
  end;
  PNMTVCustomDraw = ^TNMTVCustomDraw;
  TNMTVCustomDraw = tagNMTVCUSTOMDRAW;

  // for tooltips

  tagNMTVGETINFOTIPA = packed record
    hdr: TNMHdr;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    hItem: HTREEITEM;
    lParam: LPARAM;
  end;

  tagNMTVGETINFOTIPW = packed record
    hdr: TNMHdr;
    pszText: PWideChar;
    cchTextMax: Integer;
    hItem: HTREEITEM;
    lParam: LPARAM;
  end;

  tagNMTVGETINFOTIP = tagNMTVGETINFOTIPW;
  PNMTVGetInfoTipA = ^TNMTVGetInfoTipA;
  PNMTVGetInfoTipW = ^TNMTVGetInfoTipW;
  PNMTVGetInfoTip = PNMTVGetInfoTipW;
  TNMTVGetInfoTipA = tagNMTVGETINFOTIPA;
  TNMTVGetInfoTipW = tagNMTVGETINFOTIPW;
  TNMTVGetInfoTip = TNMTVGetInfoTipW;

const
  // treeview's customdraw return meaning don't draw images.  valid on CDRF_NOTIFYITEMPREPAINT

  TVCDRF_NOIMAGES         = $00010000;

{ ====== ComboBoxEx ======================== }

const

  WC_COMBOBOXEX = 'ComboBoxEx32';

  CBEIF_TEXT              = $00000001;
  CBEIF_IMAGE             = $00000002;
  CBEIF_SELECTEDIMAGE     = $00000004;
  CBEIF_OVERLAY           = $00000008;
  CBEIF_INDENT            = $00000010;
  CBEIF_LPARAM            = $00000020;

  CBEIF_DI_SETITEM        = $10000000;

type

  tagCOMBOBOXEXITEMA = packed record
    mask: UINT;
    iItem: Integer;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    iOverlay: Integer;
    iIndent: Integer;
    lParam: LPARAM;
  end;

  tagCOMBOBOXEXITEMW = packed record
    mask: UINT;
    iItem: Integer;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    iSelectedImage: Integer;
    iOverlay: Integer;
    iIndent: Integer;
    lParam: LPARAM;
  end;

  tagCOMBOBOXEXITEM = tagCOMBOBOXEXITEMW;
  PComboBoxExItemA = ^TComboBoxExItemA;
  PComboBoxExItemW = ^TComboBoxExItemW;
  PComboBoxExItem = PComboBoxExItemW;
  TComboBoxExItemA = tagCOMBOBOXEXITEMA;
  TComboBoxExItemW = tagCOMBOBOXEXITEMW;
  TComboBoxExItem = TComboBoxExItemW;

const

  CBEM_INSERTITEMA        = WM_USER + 1;
  CBEM_SETIMAGELIST       = WM_USER + 2;
  CBEM_GETIMAGELIST       = WM_USER + 3;
  CBEM_GETITEMA           = WM_USER + 4;
  CBEM_SETITEMA           = WM_USER + 5;
  CBEM_DELETEITEM         = CB_DELETESTRING;
  CBEM_GETCOMBOCONTROL    = WM_USER + 6;
  CBEM_GETEDITCONTROL     = WM_USER + 7;
  CBEM_SETEXSTYLE         = WM_USER + 8;  // use SETEXTENDEDSTYLE instead
  CBEM_GETEXSTYLE         = WM_USER + 9;  // use GETEXTENDEDSTYLE instead
  CBEM_GETEXTENDEDSTYLE   = WM_USER + 9;
  CBEM_HASEDITCHANGED     = WM_USER + 10;
  CBEM_INSERTITEMW        = WM_USER + 11;
  CBEM_SETITEMW           = WM_USER + 12;
  CBEM_GETITEMW           = WM_USER + 13;
  CBEM_SETEXTENDEDSTYLE   = WM_USER + 14; // lparam == new style, wParam (optional) == mask
  CBEM_SETUNICODEFORMAT   = CCM_SETUNICODEFORMAT;
  CBEM_GETUNICODEFORMAT   = CCM_GETUNICODEFORMAT;

  CBEM_INSERTITEM         = CBEM_INSERTITEMW;
  CBEM_SETITEM            = CBEM_SETITEMW;
  CBEM_GETITEM            = CBEM_GETITEMW;

  CBES_EX_NOEDITIMAGE          = $00000001;
  CBES_EX_NOEDITIMAGEINDENT    = $00000002;
  CBES_EX_PATHWORDBREAKPROC    = $00000004;
  CBES_EX_NOSIZELIMIT          = $00000008;
  CBES_EX_CASESENSITIVE        = $00000010;

type

  NMCOMBOBOXEXA = packed record
    hdr: TNMHdr;
    ceItem: TComboBoxExItemA;
  end;

  NMCOMBOBOXEXW = packed record
    hdr: TNMHdr;
    ceItem: TComboBoxExItemW;
  end;

  NMCOMBOBOXEX = NMCOMBOBOXEXW;
  PNMComboBoxExA = ^TNMComboBoxExA;
  PNMComboBoxExW = ^TNMComboBoxExW;
  PNMComboBoxEx = PNMComboBoxExW;
  TNMComboBoxExA = NMCOMBOBOXEXA;
  TNMComboBoxExW = NMCOMBOBOXEXW;
  TNMComboBoxEx = TNMComboBoxExW;

const

  CBEN_GETDISPINFOA       = CBEN_FIRST - 0;
  CBEN_INSERTITEM         = CBEN_FIRST - 1;
  CBEN_DELETEITEM         = CBEN_FIRST - 2;
  CBEN_BEGINEDIT          = CBEN_FIRST - 4;
  CBEN_ENDEDITA           = CBEN_FIRST - 5; // lParam specifies why the endedit is happening
  CBEN_ENDEDITW           = CBEN_FIRST - 6;
  CBEN_GETDISPINFOW       = CBEN_FIRST - 7;
  CBEN_DRAGBEGINA			    = CBEN_FIRST - 8;
  CBEN_DRAGBEGINW			    = CBEN_FIRST - 9;

  CBEN_ENDEDIT            = CBEN_ENDEDITW;
  CBEN_GETDISPINFO        = CBEN_GETDISPINFOW;
  CBEN_DRAGBEGIN          = CBEN_DRAGBEGINW;

  CBENF_KILLFOCUS         = 1;
  CBENF_RETURN            = 2;
  CBENF_ESCAPE            = 3;
  CBENF_DROPDOWN          = 4;

  CBEMAXSTRLEN            = 260;

type
  // CBEN_DRAGBEGIN sends this information ...

  NMCBEDRAGBEGINA = packed record
    hdr: TNMHdr;
    iItemid: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of AnsiChar;
  end;

  NMCBEDRAGBEGINW = packed record
    hdr: TNMHdr;
    iItemid: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of WideChar;
  end;

  NMCBEDRAGBEGIN = NMCBEDRAGBEGINW;
  PNMCBEDragBeginA = ^TNMCBEDragBeginA;
  PNMCBEDragBeginW = ^TNMCBEDragBeginW;
  PNMCBEDragBegin = PNMCBEDragBeginW;
  TNMCBEDragBeginA = NMCBEDRAGBEGINA;
  TNMCBEDragBeginW = NMCBEDRAGBEGINW;
  TNMCBEDragBegin = TNMCBEDragBeginW;

  // CBEN_ENDEDIT sends this information...
  // fChanged if the user actually did anything
  // iNewSelection gives what would be the new selection unless the notify is failed
  //                      iNewSelection may be CB_ERR if there's no match

  NMCBEENDEDITA = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    iNewSelection: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of AnsiChar;
    iWhy: Integer;
  end;

  NMCBEENDEDITW = packed record
    hdr: TNMHdr;
    fChanged: BOOL;
    iNewSelection: Integer;
    szText: array[0..CBEMAXSTRLEN - 1] of WideChar;
    iWhy: Integer;
  end;

  NMCBEENDEDIT = NMCBEENDEDITW;
  PNMCBEEndEditA = ^TNMCBEEndEditA;
  PNMCBEEndEditW = ^TNMCBEEndEditW;
  PNMCBEEndEdit = PNMCBEEndEditW;
  TNMCBEEndEditA = NMCBEENDEDITA;
  TNMCBEEndEditW = NMCBEENDEDITW;
  TNMCBEEndEdit = TNMCBEEndEditW;

{ ====== TAB CONTROL ======================== }

const

  WC_TABCONTROL = 'SysTabControl32';

const

  TCS_SCROLLOPPOSITE    = $0001;  // assumes multiline tab

  TCS_BOTTOM            = $0002;

  TCS_RIGHT             = $0002;

  TCS_MULTISELECT       = $0004;  // allow multi-select in button mode

  TCS_FLATBUTTONS       = $0008;

  TCS_FORCEICONLEFT     = $0010;

  TCS_FORCELABELLEFT    = $0020;

  TCS_HOTTRACK          = $0040;

  TCS_VERTICAL          = $0080;

  TCS_TABS              = $0000;

  TCS_BUTTONS           = $0100;

  TCS_SINGLELINE        = $0000;

  TCS_MULTILINE         = $0200;

  TCS_RIGHTJUSTIFY      = $0000;

  TCS_FIXEDWIDTH        = $0400;

  TCS_RAGGEDRIGHT       = $0800;

  TCS_FOCUSONBUTTONDOWN = $1000;

  TCS_OWNERDRAWFIXED    = $2000;

  TCS_TOOLTIPS          = $4000;

  TCS_FOCUSNEVER        = $8000;


  TCS_EX_FLATSEPARATORS = $00000001;

  TCS_EX_REGISTERDROP   = $00000002;


  TCM_GETIMAGELIST       = TCM_FIRST + 2;

  TCM_SETIMAGELIST       = TCM_FIRST + 3;

  TCM_GETITEMCOUNT       = TCM_FIRST + 4;

  TCM_DELETEITEM         = TCM_FIRST + 8;

  TCM_DELETEALLITEMS     = TCM_FIRST + 9;

  TCM_GETITEMRECT        = TCM_FIRST + 10;

  TCM_GETCURSEL          = TCM_FIRST + 11;

  TCM_SETCURSEL          = TCM_FIRST + 12;

  TCM_HITTEST            = TCM_FIRST + 13;

  TCM_SETITEMEXTRA       = TCM_FIRST + 14;

  TCM_ADJUSTRECT         = TCM_FIRST + 40;

  TCM_SETITEMSIZE        = TCM_FIRST + 41;

  TCM_REMOVEIMAGE        = TCM_FIRST + 42;

  TCM_SETPADDING         = TCM_FIRST + 43;

  TCM_GETROWCOUNT        = TCM_FIRST + 44;

  TCM_GETTOOLTIPS        = TCM_FIRST + 45;

  TCM_SETTOOLTIPS        = TCM_FIRST + 46;
  
  TCM_GETCURFOCUS        = TCM_FIRST + 47;
  
  TCM_SETCURFOCUS        = TCM_FIRST + 48;
  
  TCM_SETMINTABWIDTH     = TCM_FIRST + 49;
  
  TCM_DESELECTALL        = TCM_FIRST + 50;
  
  TCM_HIGHLIGHTITEM      = TCM_FIRST + 51;
  
  TCM_SETEXTENDEDSTYLE   = TCM_FIRST + 52;  // optional wParam == mask
  
  TCM_GETEXTENDEDSTYLE   = TCM_FIRST + 53;

  TCM_SETUNICODEFORMAT   = CCM_SETUNICODEFORMAT;

  TCM_GETUNICODEFORMAT   = CCM_GETUNICODEFORMAT;


  TCIF_TEXT       = $0001;

  TCIF_IMAGE      = $0002;

  TCIF_RTLREADING = $0004;

  TCIF_PARAM      = $0008;

  TCIF_STATE      = $0010;


  TCIS_BUTTONPRESSED      = $0001;

  TCIS_HIGHLIGHTED        = $0002;

type
  PTCItemHeaderA = ^TTCItemHeaderA;
  PTCItemHeaderW = ^TTCItemHeaderW;
  PTCItemHeader = PTCItemHeaderW;

  tagTCITEMHEADERA = packed record
    mask: UINT;
    lpReserved1: UINT;
    lpReserved2: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
  end;

  tagTCITEMHEADERW = packed record
    mask: UINT;
    lpReserved1: UINT;
    lpReserved2: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
  end;

  tagTCITEMHEADER = tagTCITEMHEADERW;

  _TC_ITEMHEADERA = tagTCITEMHEADERA;

  _TC_ITEMHEADERW = tagTCITEMHEADERW;

  _TC_ITEMHEADER = _TC_ITEMHEADERW;
  TTCItemHeaderA = tagTCITEMHEADERA;
  TTCItemHeaderW = tagTCITEMHEADERW;
  TTCItemHeader = TTCItemHeaderW;

  TC_ITEMHEADERA = tagTCITEMHEADERA;

  TC_ITEMHEADERW = tagTCITEMHEADERW;

  TC_ITEMHEADER = TC_ITEMHEADERW;

  PTCItemA = ^TTCItemA;
  PTCItemW = ^TTCItemW;
  PTCItem = PTCItemW;

  tagTCITEMA = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PAnsiChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

  tagTCITEMW = packed record
    mask: UINT;
    dwState: UINT;
    dwStateMask: UINT;
    pszText: PWideChar;
    cchTextMax: Integer;
    iImage: Integer;
    lParam: LPARAM;
  end;

  tagTCITEM = tagTCITEMW;

  _TC_ITEMA = tagTCITEMA;

  _TC_ITEMW = tagTCITEMW;

  _TC_ITEM = _TC_ITEMW;
  TTCItemA = tagTCITEMA;
  TTCItemW = tagTCITEMW;
  TTCItem = TTCItemW;

  TC_ITEMA = tagTCITEMA;

  TC_ITEMW = tagTCITEMW;

  TC_ITEM = TC_ITEMW;

const

  TCM_GETITEMA             = TCM_FIRST + 5;

  TCM_SETITEMA             = TCM_FIRST + 6;

  TCM_INSERTITEMA          = TCM_FIRST + 7;

  TCM_GETITEMW             = TCM_FIRST + 60;

  TCM_SETITEMW             = TCM_FIRST + 61;

  TCM_INSERTITEMW          = TCM_FIRST + 62;

  TCM_GETITEM             = TCM_GETITEMW;

  TCM_SETITEM             = TCM_SETITEMW;

  TCM_INSERTITEM          = TCM_INSERTITEMW;

const

  TCHT_NOWHERE     = $0001;

  TCHT_ONITEMICON  = $0002;

  TCHT_ONITEMLABEL = $0004;

  TCHT_ONITEM      = TCHT_ONITEMICON or TCHT_ONITEMLABEL;

type
  PTCHitTestInfo = ^TTCHitTestInfo;

  tagTCHITTESTINFO = packed record
    pt: TPoint;
    flags: UINT;
  end;

  _TC_HITTESTINFO = tagTCHITTESTINFO;
  TTCHitTestInfo = tagTCHITTESTINFO;

  TC_HITTESTINFO = tagTCHITTESTINFO;


  tagTCKEYDOWN = packed record
    hdr: TNMHDR;
    wVKey: Word;
    flags: UINT;
  end;

  _TC_KEYDOWN = tagTCKEYDOWN;
  TTCKeyDown = tagTCKEYDOWN;

  TC_KEYDOWN = tagTCKEYDOWN;

const

  TCN_KEYDOWN             = TCN_FIRST - 0;

  TCN_SELCHANGE           = TCN_FIRST - 1;

  TCN_SELCHANGING         = TCN_FIRST - 2;

  TCN_GETOBJECT           = TCN_FIRST - 3;


function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer;

function TabCtrl_SetItemExtra(hwndTC: HWND; cb: Integer): BOOL;

function TabCtrl_AdjustRect(hwnd: HWND; bLarger: BOOL; prc: PRect): Integer;

function TabCtrl_SetItemSize(hwnd: HWND; x, y: Integer): DWORD;

procedure TabCtrl_RemoveImage(hwnd: HWND; i: Integer);

procedure TabCtrl_SetPadding(hwnd: HWND; cx, cy: Integer);

function TabCtrl_GetRowCount(hwnd: HWND): Integer;

function TabCtrl_GetToolTips(wnd: HWND): HWND;

procedure TabCtrl_SetToolTips(hwnd: HWND; hwndTT: HWND);

function TabCtrl_GetCurFocus(hwnd: HWND): Integer;

procedure TabCtrl_SetCurFocus(hwnd: HWND; i: Integer);

function TabCtrl_SetMinTabWidth(hwnd: HWND; x: Integer): Integer;

procedure TabCtrl_DeselectAll(hwnd: HWND; fExcludeFocus: BOOL);

function TabCtrl_HighlightItem(hwnd: HWND; i: Integer; fHighlight: WordBool): BOOL;

function TabCtrl_SetExtendedStyle(hwnd: HWND; dw: DWORD): DWORD;

function TabCtrl_GetExtendedStyle(hwnd: HWND): DWORD;

function TabCtrl_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;

function TabCtrl_GetUnicodeFormat(hwnd: HWND): BOOL;

{ ====== ANIMATE CONTROL ================= }

const
  
  ANIMATE_CLASS = 'SysAnimate32';

const
  
  ACS_CENTER              = $0001;
  
  ACS_TRANSPARENT         = $0002;
  
  ACS_AUTOPLAY            = $0004;
  
  ACS_TIMER               = $0008;  { don't use threads... use timers }

  
  ACM_OPENA                = WM_USER + 100;
  
  ACM_OPENW                = WM_USER + 103;
  
  ACM_OPEN                = ACM_OPENA;

  
  ACM_PLAY                = WM_USER + 101;
  
  ACM_STOP                = WM_USER + 102;

  
  ACN_START               = 1;
  
  ACN_STOP                = 2;


function Animate_Create(hwndP: HWND; id: HMENU; dwStyle: DWORD; hInstance: HINST): HWND;

function Animate_Open(hwnd: HWND; szName: PChar): BOOL;

function Animate_OpenEx(hwnd: HWND; hInst: HINST; szName: PChar): BOOL;

function Animate_Play(hwnd: HWND; from, _to: Word; rep: UINT): BOOL;

function Animate_Stop(hwnd: HWND): BOOL;

function Animate_Close(hwnd: HWND): BOOL;

function Animate_Seek(hwnd: HWND; frame: Word): BOOL;

{ ====== MONTHCAL CONTROL ========= }

const
  
  MONTHCAL_CLASS          = 'SysMonthCal32';

const  
  // Message constants
  
  MCM_FIRST             = $1000;
  
  MCM_GETCURSEL         = MCM_FIRST + 1;
  
  MCM_SETCURSEL         = MCM_FIRST + 2;
  
  MCM_GETMAXSELCOUNT    = MCM_FIRST + 3;
  
  MCM_SETMAXSELCOUNT    = MCM_FIRST + 4;
  
  MCM_GETSELRANGE       = MCM_FIRST + 5;
  
  MCM_SETSELRANGE       = MCM_FIRST + 6;
  
  MCM_GETMONTHRANGE     = MCM_FIRST + 7;
  
  MCM_SETDAYSTATE       = MCM_FIRST + 8;
  
  MCM_GETMINREQRECT     = MCM_FIRST + 9;
  
  MCM_SETCOLOR          = MCM_FIRST + 10;
  
  MCM_GETCOLOR          = MCM_FIRST + 11;
  
  MCM_SETTODAY          = MCM_FIRST + 12;
  
  MCM_GETTODAY          = MCM_FIRST + 13;
  
  MCM_HITTEST           = MCM_FIRST + 14;
  
  MCM_SETFIRSTDAYOFWEEK = MCM_FIRST + 15;
  
  MCM_GETFIRSTDAYOFWEEK = MCM_FIRST + 16;
  
  MCM_GETRANGE          = MCM_FIRST + 17;
  
  MCM_SETRANGE          = MCM_FIRST + 18;
  
  MCM_GETMONTHDELTA     = MCM_FIRST + 19;
  
  MCM_SETMONTHDELTA     = MCM_FIRST + 20;
  
  MCM_GETMAXTODAYWIDTH  = MCM_FIRST + 21;
  
  MCM_SETUNICODEFORMAT  = CCM_SETUNICODEFORMAT;
  
  MCM_GETUNICODEFORMAT  = CCM_GETUNICODEFORMAT;

  // Hit test flags
  
  MCHT_TITLE            = $00010000;
  
  MCHT_CALENDAR         = $00020000;
  
  MCHT_TODAYLINK        = $00030000;
  
  MCHT_NEXT             = $01000000;  // these indicate that hitting
  
  MCHT_PREV             = $02000000;  // here will go to the next/prev month
  
  MCHT_NOWHERE          = $00000000;
  
  MCHT_TITLEBK          = MCHT_TITLE;
  
  MCHT_TITLEMONTH       = MCHT_TITLE or $0001;
  
  MCHT_TITLEYEAR        = MCHT_TITLE or $0002;
  
  MCHT_TITLEBTNNEXT     = MCHT_TITLE or MCHT_NEXT or $0003;
  
  MCHT_TITLEBTNPREV     = MCHT_TITLE or MCHT_PREV or $0003;
  
  MCHT_CALENDARBK       = MCHT_CALENDAR;
  
  MCHT_CALENDARDATE     = MCHT_CALENDAR or $0001;
  
  MCHT_CALENDARDATENEXT = MCHT_CALENDARDATE or MCHT_NEXT;
  
  MCHT_CALENDARDATEPREV = MCHT_CALENDARDATE or MCHT_PREV;
  
  MCHT_CALENDARDAY      = MCHT_CALENDAR or $0002;
  
  MCHT_CALENDARWEEKNUM  = MCHT_CALENDAR or $0003;

  // Color codes
  
  MCSC_BACKGROUND       = 0;   // the background color (between months)
  
  MCSC_TEXT             = 1;   // the dates
  
  MCSC_TITLEBK          = 2;   // background of the title
  
  MCSC_TITLETEXT        = 3;
  
  MCSC_MONTHBK          = 4;   // background within the month cal
  
  MCSC_TRAILINGTEXT     = 5;   // the text color of header & trailing days

  // Notification codes
  
  MCN_SELCHANGE         = MCN_FIRST + 1;
  
  MCN_GETDAYSTATE       = MCN_FIRST + 3;
  
  MCN_SELECT            = MCN_FIRST + 4;

  // Style flags
  
  MCS_DAYSTATE          = $0001;
  
  MCS_MULTISELECT       = $0002;
  
  MCS_WEEKNUMBERS       = $0004;
  MCS_NOTODAY_PRE_IE4   = $0008;
  
  MCS_NOTODAYCIRCLE     = $0008;
  
  MCS_NOTODAY           = $0010;

  
  GMR_VISIBLE           = 0;       // visible portion of display
  
  GMR_DAYSTATE          = 1;       // above plus the grayed out parts of
                                   // partially displayed months
                                   
type
  // bit-packed array of "bold" info for a month
  // if a bit is on, that day is drawn bold
  
  MONTHDAYSTATE = DWORD;
  PMonthDayState = ^TMonthDayState;
  TMonthDayState = MONTHDAYSTATE;

  
  MCHITTESTINFO = packed record
    cbSize: UINT;
    pt: TPoint;
    uHit: UINT;      // out param
    st: TSystemTime;
  end;
  PMCHitTestInfo = ^TMCHitTestInfo;
  TMCHitTestInfo = MCHITTESTINFO;

  // MCN_SELCHANGE is sent whenever the currently displayed date changes
  // via month change, year change, keyboard navigation, prev/next button
  
  tagNMSELCHANGE = packed record
    nmhdr: TNmHdr;  // this must be first, so we don't break WM_NOTIFY
    stSelStart: TSystemTime;
    stSelEnd: TSystemTime;
  end;
  PNMSelChange = ^TNMSelChange;
  TNMSelChange = tagNMSELCHANGE;

  // MCN_GETDAYSTATE is sent for MCS_DAYSTATE controls whenever new daystate
  // information is needed (month or year scroll) to draw bolding information.
  // The app must fill in cDayState months worth of information starting from
  // stStart date. The app may fill in the array at prgDayState or change
  // prgDayState to point to a different array out of which the information
  // will be copied. (similar to tooltips)
  
  tagNMDAYSTATE = packed record
    nmhdr: TNmHdr;  // this must be first, so we don't break WM_NOTIFY
    stStart: TSystemTime;
    cDayState: Integer;
    prgDayState: PMonthDayState; // points to cDayState TMONTHDAYSTATEs
  end;
  PNMDayState = ^TNMDayState;
  TNMDayState = tagNMDAYSTATE;

  // MCN_SELECT is sent whenever a selection has occured (via mouse or keyboard)
  
  NMSELECT = tagNMSELCHANGE;
  PNMSelect = ^TNMSelect;
  TNMSelect = NMSELECT;

//   returns FALSE if MCS_MULTISELECT
//   returns TRUE and sets *pst to the currently selected date otherwise

function MonthCal_GetCurSel(hmc: HWND; var pst: TSystemTime): BOOL;

//   returns FALSE if MCS_MULTISELECT
//   returns TURE and sets the currently selected date to *pst otherwise

function MonthCal_SetCurSel(hmc: HWND; const pst: TSystemTime): BOOL;

//   returns the maximum number of selectable days allowed

function MonthCal_GetMaxSelCount(hmc: HWND): DWORD;

//   sets the max number days that can be selected iff MCS_MULTISELECT

function MonthCal_SetMaxSelCount(hmc: HWND; n: UINT): BOOL;

//   sets rgst[0] to the first day of the selection range
//   sets rgst[1] to the last day of the selection range

function MonthCal_GetSelRange(hmc: HWND; rgst: PSystemTime): BOOL;

//   selects the range of days from rgst[0] to rgst[1]

function MonthCal_SetSelRange(hmc: HWND; rgst: PSystemTime): BOOL;

//   if rgst specified, sets rgst[0] to the starting date and
//      and rgst[1] to the ending date of the the selectable (non-grayed)
//      days if GMR_VISIBLE or all the displayed days (including grayed)
//      if GMR_DAYSTATE.
//   returns the number of months spanned by the above range.

function MonthCal_GetMonthRange(hmc: HWND; gmr: DWORD; rgst: PSystemTime): DWORD;

//   cbds is the count of DAYSTATE items in rgds and it must be equal
//   to the value returned from MonthCal_GetMonthRange(hmc, GMR_DAYSTATE, NULL)
//   This sets the DAYSTATE bits for each month (grayed and non-grayed
//   days) displayed in the calendar. The first bit in a month's DAYSTATE
//   corresponts to bolding day 1, the second bit affects day 2, etc.

function MonthCal_SetDayState(hmc: HWND; cbds: Integer; const rgds: TNMDayState): BOOL;

//   sets prc the minimal size needed to display one month

function MonthCal_GetMinReqRect(hmc: HWND; var prc: TRect): BOOL;

// set what day is "today"   send NULL to revert back to real date

function MonthCal_SetToday(hmc: HWND; const pst: TSystemTime): BOOL;

// get what day is "today"
// returns BOOL for success/failure

function MonthCal_GetToday(hmc: HWND; var pst: TSystemTime): BOOL;

// determine what pinfo->pt is over

function MonthCal_HitTest(hmc: HWND; var info: TMCHitTestInfo): DWORD;

// set colors to draw control with -- see MCSC_ bits below

function MonthCal_SetColor(hmc: HWND; iColor: Integer; clr: TColorRef): TColorRef;


function MonthCal_GetColor(hmc: HWND; iColor: Integer): TColorRef;

// set first day of week to iDay:
// 0 for Monday, 1 for Tuesday, ..., 6 for Sunday
// -1 for means use locale info

function MonthCal_SetFirstDayOfWeek(hmc: HWND; iDay: Integer): Integer;

// DWORD result...  low word has the day.  high word is bool if this is app set
// or not (FALSE == using locale info)

function MonthCal_GetFirstDayOfWeek(hmc: HWND): Integer;

//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
//   returns GDTR_MIN|GDTR_MAX if there is a minimum|maximum limit

function MonthCal_GetRange(hmc: HWND; rgst: PSystemTime): DWORD;

//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
//   returns TRUE on success, FALSE on error (such as invalid parameters)

function Monthcal_SetRange(hmc: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL;

//   returns the number of months one click on a next/prev button moves by

function MonthCal_GetMonthDelta(hmc: HWND): Integer;

//   sets the month delta to n. n = 0 reverts to moving by a page of months
//   returns the previous value of n.

function MonthCal_SetMonthDelta(hmc: HWND; n: Integer): Integer;

//   sets *psz to the maximum width/height of the "Today" string displayed
//   at the bottom of the calendar (as long as MCS_NOTODAY is not specified)

function MonthCal_GetMaxTodayWidth(hmc: HWND): DWORD;


function MonthCal_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;


function MonthCal_GetUnicodeFormat(hwnd: HWND): BOOL;

{ ====== DATETIMEPICK CONTROL =============== }

const
  
  DATETIMEPICK_CLASS = 'SysDateTimePick32';

  // Message constants
  
  DTM_FIRST         = $1000;
  
  DTM_GETSYSTEMTIME = DTM_FIRST + 1;
  
  DTM_SETSYSTEMTIME = DTM_FIRST + 2;
  
  DTM_GETRANGE      = DTM_FIRST + 3;
  
  DTM_SETRANGE      = DTM_FIRST + 4;
  
  DTM_SETFORMATA    = DTM_FIRST + 5;
  
  DTM_SETMCCOLOR    = DTM_FIRST + 6;
  
  DTM_GETMCCOLOR    = DTM_FIRST + 7;
  
  DTM_GETMONTHCAL   = DTM_FIRST + 8;
  
  DTM_SETMCFONT     = DTM_FIRST + 9;
  
  DTM_GETMCFONT     = DTM_FIRST + 10;
  
  DTM_SETFORMATW    = DTM_FIRST + 50;
  
  DTM_SETFORMAT     = DTM_SETFORMATA;

  // Style Flags
  
  DTS_UPDOWN          = $0001;  // use UPDOWN instead of MONTHCAL
  
  DTS_SHOWNONE        = $0002;  // allow a NONE selection
  
  DTS_SHORTDATEFORMAT = $0000;  // use the short date format
                                // (app must forward WM_WININICHANGE messages)
  
  DTS_LONGDATEFORMAT  = $0004;  // use the long date format
                                // (app must forward WM_WININICHANGE messages)
  
  DTS_TIMEFORMAT      = $0009;  // use the time format
                                // (app must forward WM_WININICHANGE messages)
  
  DTS_APPCANPARSE     = $0010;  // allow user entered strings
                                // (app MUST respond to DTN_USERSTRING)
  
  DTS_RIGHTALIGN      = $0020;  // right-align popup instead of left-align it

  // Notification codes
  
  DTN_DATETIMECHANGE = DTN_FIRST + 1;  // the systemtime has changed
  
  DTN_USERSTRINGA    = DTN_FIRST + 2;  // the user has entered a string
  
  DTN_USERSTRINGW    = DTN_FIRST + 15;
  
  DTN_WMKEYDOWNA     = DTN_FIRST + 3;  // modify keydown on app format field (X)
  
  DTN_WMKEYDOWNW     = DTN_FIRST + 16;
  
  DTN_FORMATA        = DTN_FIRST + 4;  // query display for app format field (X)
  
  DTN_FORMATW        = DTN_FIRST + 17;
  
  DTN_FORMATQUERYA   = DTN_FIRST + 5;  // query formatting info for app format field (X)
  
  DTN_FORMATQUERYW   = DTN_FIRST + 18;
  
  DTN_DROPDOWN       = DTN_FIRST + 6;  // MonthCal has dropped down
  
  DTN_CLOSEUP        = DTN_FIRST + 7;  // MonthCal is popping up
  
  DTN_USERSTRING     = DTN_USERSTRINGW;

  DTN_WMKEYDOWN      = DTN_WMKEYDOWNW;

  DTN_FORMAT         = DTN_FORMATW;

  DTN_FORMATQUERY    = DTN_FORMATQUERYW;

  // Ranges

  GDTR_MIN = $0001;

  GDTR_MAX = $0002;

  // Return Values

  GDT_ERROR = -1;

  GDT_VALID = 0;

  GDT_NONE  = 1;

type

  tagNMDATETIMECHANGE = packed record
    nmhdr: TNmHdr;
    dwFlags: DWORD;         // GDT_VALID or GDT_NONE
    st: TSystemTime;        // valid iff dwFlags = GDT_VALID
  end;
  PNMDateTimeChange = ^TNMDateTimeChange;
  TNMDateTimeChange = tagNMDATETIMECHANGE;


  tagNMDATETIMESTRINGA = packed record
    nmhdr: TNmHdr;
    pszUserString: PAnsiChar;     // string user entered
    st: TSystemTime;           // app fills this in
    dwFlags: DWORD;            // GDT_VALID or GDT_NONE
  end;

  tagNMDATETIMESTRINGW = packed record
    nmhdr: TNmHdr;
    pszUserString: PWideChar;     // string user entered
    st: TSystemTime;           // app fills this in
    dwFlags: DWORD;            // GDT_VALID or GDT_NONE
  end;

  tagNMDATETIMESTRING = tagNMDATETIMESTRINGW;
  PNMDateTimeStringA = ^TNMDateTimeStringA;
  PNMDateTimeStringW = ^TNMDateTimeStringW;
  PNMDateTimeString = PNMDateTimeStringW;
  TNMDateTimeStringA = tagNMDATETIMESTRINGA;
  TNMDateTimeStringW = tagNMDATETIMESTRINGW;
  TNMDateTimeString = TNMDateTimeStringW;


  tagNMDATETIMEWMKEYDOWNA = packed record
    nmhdr: TNmHdr;
    nVirtKey: Integer; // virtual key code of WM_KEYDOWN which MODIFIES an X field
    pszFormat: PAnsiChar; // format substring
    st: TSystemTime;   // current systemtime, app should modify based on key
  end;

  tagNMDATETIMEWMKEYDOWNW = packed record
    nmhdr: TNmHdr;
    nVirtKey: Integer; // virtual key code of WM_KEYDOWN which MODIFIES an X field
    pszFormat: PWideChar; // format substring
    st: TSystemTime;   // current systemtime, app should modify based on key
  end;

  tagNMDATETIMEWMKEYDOWN = tagNMDATETIMEWMKEYDOWNW;
  PNMDateTimeWMKeyDownA = ^TNMDateTimeWMKeyDownA;
  PNMDateTimeWMKeyDownW = ^TNMDateTimeWMKeyDownW;
  PNMDateTimeWMKeyDown = PNMDateTimeWMKeyDownW;
  TNMDateTimeWMKeyDownA = tagNMDATETIMEWMKEYDOWNA;
  TNMDateTimeWMKeyDownW = tagNMDATETIMEWMKEYDOWNW;
  TNMDateTimeWMKeyDown = TNMDateTimeWMKeyDownW;


  tagNMDATETIMEFORMATA = packed record
    nmhdr: TNmHdr;
    pszFormat: PAnsiChar;                // format substring
    st: TSystemTime;                  // current systemtime
    pszDisplay: PAnsiChar;               // string to display
    szDisplay: array[0..63] of AnsiChar; // buffer pszDisplay originally points at
  end;

  tagNMDATETIMEFORMATW = packed record
    nmhdr: TNmHdr;
    pszFormat: PWideChar;                // format substring
    st: TSystemTime;                  // current systemtime
    pszDisplay: PWideChar;               // string to display
    szDisplay: array[0..63] of WideChar; // buffer pszDisplay originally points at
  end;

  tagNMDATETIMEFORMAT = tagNMDATETIMEFORMATW;
  PNMDateTimeFormatA = ^TNMDateTimeFormatA;
  PNMDateTimeFormatW = ^TNMDateTimeFormatW;
  PNMDateTimeFormat = PNMDateTimeFormatW;
  TNMDateTimeFormatA = tagNMDATETIMEFORMATA;
  TNMDateTimeFormatW = tagNMDATETIMEFORMATW;
  TNMDateTimeFormat = TNMDateTimeFormatW;


  tagNMDATETIMEFORMATQUERYA = packed record
    nmhdr: TNmHdr;
    pszFormat: PAnsiChar; // format substring
    szMax: TSize;      // max bounding rectangle app will use for this format string
  end;

  tagNMDATETIMEFORMATQUERYW = packed record
    nmhdr: TNmHdr;
    pszFormat: PWideChar; // format substring
    szMax: TSize;      // max bounding rectangle app will use for this format string
  end;

  tagNMDATETIMEFORMATQUERY = tagNMDATETIMEFORMATQUERYW;
  PNMDateTimeFormatQueryA = ^TNMDateTimeFormatQueryA;
  PNMDateTimeFormatQueryW = ^TNMDateTimeFormatQueryW;
  PNMDateTimeFormatQuery = PNMDateTimeFormatQueryW;
  TNMDateTimeFormatQueryA = tagNMDATETIMEFORMATQUERYA;
  TNMDateTimeFormatQueryW = tagNMDATETIMEFORMATQUERYW;
  TNMDateTimeFormatQuery = TNMDateTimeFormatQueryW;

//   returns GDT_NONE if "none" is selected (DTS_SHOWNONE only)
//   returns GDT_VALID and modifies pst to be the currently selected value

function DateTime_GetSystemTime(hdp: HWND; var pst: TSystemTime): DWORD;

//   if gd = GDT_NONE, sets datetimepick to None (DTS_SHOWNONE only)
//   if gd = GDT_VALID, sets datetimepick to pst
//   returns TRUE on success, FALSE on error (such as bad params)

function DateTime_SetSystemTime(hdp: HWND; gd: DWORD; const pst: TSystemTime): BOOL;

//   modifies rgst[0] to be the minimum ALLOWABLE systemtime (or 0 if no minimum)
//   modifies rgst[1] to be the maximum ALLOWABLE systemtime (or 0 if no maximum)
//   returns GDTR_MIN or GDTR_MAX if there is a minimum or maximum limit

function DateTime_GetRange(hdp: HWND; rgst: PSystemTime): DWORD;

//   if GDTR_MIN, sets the minimum ALLOWABLE systemtime to rgst[0], otherwise removes minimum
//   if GDTR_MAX, sets the maximum ALLOWABLE systemtime to rgst[1], otherwise removes maximum
//   returns TRUE on success, FALSE on error (such as invalid parameters)

function DateTime_SetRange(hdp: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL;

//   sets the display formatting string to sz (see GetDateFormat and GetTimeFormat for valid formatting chars)
//   NOTE: 'X' is a valid formatting character which indicates that the application
//   will determine how to display information. Such apps must support DTN_WMKEYDOWN,
//   DTN_FORMAT, and DTN_FORMATQUERY.

function DateTime_SetFormatA(hdp: HWND; sz: PAnsiChar): BOOL;

function DateTime_SetFormatW(hdp: HWND; sz: PWideChar): BOOL;

function DateTime_SetFormat(hdp: HWND; sz: PChar): BOOL;


function DateTime_SetMonthCalColor(hdp: HWND; iColor: DWORD; clr: TColorRef): TColorRef;


function DateTime_GetMonthCalColor(hdp: HWND; iColor: DWORD): TColorRef;

// returns the HWND of the MonthCal popup window. Only valid
// between DTN_DROPDOWN and DTN_CLOSEUP notifications.

function DateTime_GetMonthCal(hdp: HWND): HWND;


procedure DateTime_SetMonthCalFont(hdp: HWND; hfont: HFONT; fRedraw: BOOL);


function DateTime_GetMonthCalFont(hdp: HWND): HFONT;

{  ====================== IP Address edit control ============================= }

const
  
  WC_IPADDRESS         = 'SysIPAddress32';

  // Messages sent to IPAddress controls
  
  IPM_CLEARADDRESS     = WM_USER+100;  { no parameters }
  
  IPM_SETADDRESS       = WM_USER+101;  { lparam = TCP/IP address }
  
  IPM_GETADDRESS       = WM_USER+102;  { lresult = 
  
  IPM_SETRANGE         = WM_USER+103;  { wparam = field, lparam = range }
  
  IPM_SETFOCUS         = WM_USER+104;  { wparam = field }
  
  IPM_ISBLANK          = WM_USER+105;  { no parameters }

  
  IPN_FIELDCHANGED     = IPN_FIRST - 0;

type
  
  tagNMIPADDRESS = packed record
    hdr: TNMHDR;
    iField: Integer;
    iValue: Integer;
  end;
  PNMIPAddress = ^TNMIPAddress;
  TNMIPAddress = tagNMIPADDRESS;

{ The following is a useful macro for passing the range values in the }
{ IPM_SETRANGE message. }

function MAKEIPRANGE(low, high: Byte): LPARAM;

{ And this is a useful macro for making the IP Address to be passed }
{ as a LPARAM. }

function MAKEIPADDRESS(b1, b2, b3, b4: DWORD): LPARAM;

{ Get individual number }

function FIRST_IPADDRESS(x: DWORD): DWORD;


function SECOND_IPADDRESS(x: DWORD): DWORD;


function THIRD_IPADDRESS(x: DWORD): DWORD;


function FOURTH_IPADDRESS(x: DWORD): DWORD;

{  ====================== Pager Control ============================= }

const
  { Pager Class Name }
  
  WC_PAGESCROLLER               = 'SysPager';

  { Pager Control Styles }
  
  PGS_VERT                    = $00000000;
  
  PGS_HORZ                    = $00000001;
  
  PGS_AUTOSCROLL              = $00000002;
  
  PGS_DRAGNDROP               = $00000004;

  { Pager Button State }
  { The scroll can be in one of the following control State }
  
  PGF_INVISIBLE        = 0;     { Scroll button is not visible }
  
  PGF_NORMAL           = 1;     { Scroll button is in normal state }
  
  PGF_GRAYED           = 2;     { Scroll button is in grayed state }
  
  PGF_DEPRESSED        = 4;     { Scroll button is in depressed state }
  
  PGF_HOT              = 8;     { Scroll button is in hot state }

  { The following identifiers specifies the button control }
  
  PGB_TOPORLEFT           = 0;
  
  PGB_BOTTOMORRIGHT       = 1;

  { Pager Control  Messages }
  
  PGM_SETCHILD                = PGM_FIRST + 1;   { lParam == hwnd }
  
  PGM_RECALCSIZE              = PGM_FIRST + 2;
  
  PGM_FORWARDMOUSE            = PGM_FIRST + 3;
  
  PGM_SETBKCOLOR              = PGM_FIRST + 4;
  
  PGM_GETBKCOLOR              = PGM_FIRST + 5;
  
  PGM_SETBORDER              = PGM_FIRST + 6;
  
  PGM_GETBORDER              = PGM_FIRST + 7;
  
  PGM_SETPOS                  = PGM_FIRST + 8;
  
  PGM_GETPOS                  = PGM_FIRST + 9;
  
  PGM_SETBUTTONSIZE           = PGM_FIRST + 10;
  
  PGM_GETBUTTONSIZE           = PGM_FIRST + 11;
  
  PGM_GETBUTTONSTATE          = PGM_FIRST + 12;
  
  PGM_GETDROPTARGET           = CCM_GETDROPTARGET;


procedure Pager_SetChild(hwnd: HWND; hwndChild: HWND);

procedure Pager_RecalcSize(hwnd: HWND);

procedure Pager_ForwardMouse(hwnd: HWND; bForward: BOOL);

function Pager_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF;

function Pager_GetBkColor(hwnd: HWND): COLORREF;

function Pager_SetBorder(hwnd: HWND; iBorder: Integer): Integer;

function Pager_GetBorder(hwnd: HWND): Integer;

function Pager_SetPos(hwnd: HWND; iPos: Integer): Integer;

function Pager_GetPos(hwnd: HWND): Integer;

function Pager_SetButtonSize(hwnd: HWND; iSize: Integer): Integer;

function Pager_GetButtonSize(hwnd: HWND): Integer;

function Pager_GetButtonState(hwnd: HWND; iButton: Integer): DWORD;

procedure Pager_GetDropTarget(hwnd: HWND; ppdt: Pointer{!!});

const
  { Pager Control Notification Messages }

  { PGN_SCROLL Notification Message }
  
  PGN_SCROLL              = PGN_FIRST-1;

  
  PGF_SCROLLUP            = 1;
  
  PGF_SCROLLDOWN          = 2;
  
  PGF_SCROLLLEFT          = 4;
  
  PGF_SCROLLRIGHT         = 8;

  { Keys down }
  
  PGK_SHIFT               = 1;
  
  PGK_CONTROL             = 2;
  
  PGK_MENU                = 4;

type
  { This structure is sent along with PGN_SCROLL notifications }
  
  NMPGSCROLL = packed record
    hdr: TNMHDR;
    fwKeys: Word;           { Specifies which keys are down when this notification is send }
    rcParent: TRect;        { Contains Parent Window Rect }
    iDir: Integer;          { Scrolling Direction }
    iXpos: Integer;         { Horizontal scroll position }
    iYpos: Integer;         { Vertical scroll position }
    iScroll: Integer;       { [in/out] Amount to scroll }
  end;
  PNMPGScroll = ^TNMPGScroll;
  TNMPGScroll = NMPGSCROLL;

const
  { PGN_CALCSIZE Notification Message }
  
  PGN_CALCSIZE            = PGN_FIRST-2;

  
  PGF_CALCWIDTH           = 1;
  
  PGF_CALCHEIGHT          = 2;

type
  
  NMPGCALCSIZE = packed record
    hdr: TNMHDR;
    dwFlag: DWORD;
    iWidth: Integer;
    iHeight: Integer;
  end;
  PNMPGCalcSize = ^TNMPGCalcSize;
  TNMPGCalcSize = NMPGCALCSIZE;

{ ======================  Native Font Control ============================== }

const
  
  WC_NATIVEFONTCTL            = 'NativeFontCtl';

  { style definition }
  
  NFS_EDIT                    = $0001;
  
  NFS_STATIC                  = $0002;
  
  NFS_LISTCOMBO               = $0004;
  
  NFS_BUTTON                  = $0008;
  
  NFS_ALL                     = $0010;

{ ====== TrackMouseEvent  ================================================== }

const
  
  WM_MOUSEHOVER                       = $02A1;
  
  WM_MOUSELEAVE                       = $02A3;

  
  TME_HOVER           = $00000001;
  
  TME_LEAVE           = $00000002;
  
  TME_QUERY           = $40000000;
  
  TME_CANCEL          = $80000000;


  HOVER_DEFAULT       = $FFFFFFFF;

type
  
  tagTRACKMOUSEEVENT = packed record
    cbSize: DWORD;
    dwFlags: DWORD;
    hwndTrack: HWND;
    dwHoverTime: DWORD;
  end;
  PTrackMouseEvent = ^TTrackMouseEvent;
  TTrackMouseEvent = tagTRACKMOUSEEVENT;

{ Declare _TrackMouseEvent.  This API tries to use the window manager's }
{ implementation of TrackMouseEvent if it is present, otherwise it emulates. }

function _TrackMouseEvent(lpEventTrack: PTrackMouseEvent): BOOL; stdcall;

{ ====== Flat Scrollbar APIs========================================= }

const
  
  WSB_PROP_CYVSCROLL      = $00000001;
  
  WSB_PROP_CXHSCROLL      = $00000002;
  
  WSB_PROP_CYHSCROLL      = $00000004;
  
  WSB_PROP_CXVSCROLL      = $00000008;
  
  WSB_PROP_CXHTHUMB       = $00000010;
  
  WSB_PROP_CYVTHUMB       = $00000020;
  
  WSB_PROP_VBKGCOLOR      = $00000040;
  
  WSB_PROP_HBKGCOLOR      = $00000080;
  
  WSB_PROP_VSTYLE         = $00000100;

  WSB_PROP_HSTYLE         = $00000200;
  
  WSB_PROP_WINSTYLE       = $00000400;
  
  WSB_PROP_PALETTE        = $00000800;
  
  WSB_PROP_MASK           = $00000FFF;

  
  FSB_FLAT_MODE               = 2;
  
  FSB_ENCARTA_MODE            = 1;
  
  FSB_REGULAR_MODE            = 0;


function FlatSB_EnableScrollBar(hWnd: HWND; wSBflags, wArrows: UINT): BOOL; stdcall;

function FlatSB_ShowScrollBar(hWnd: HWND; wBar: Integer; bShow: BOOL): BOOL; stdcall;


function FlatSB_GetScrollRange(hWnd: HWND; nBar: Integer; var lpMinPos,
  lpMaxPos: Integer): BOOL; stdcall;

function FlatSB_GetScrollInfo(hWnd: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): BOOL; stdcall;

function FlatSB_GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;

function FlatSB_GetScrollProp(p1: HWND; propIndex: Integer;
  p3: PInteger): Bool; stdcall;


function FlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer;
  bRedraw: BOOL): Integer; stdcall;

function FlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer;
  const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;

function FlatSB_SetScrollRange(hWnd: HWND; nBar, nMinPos, nMaxPos: Integer;
  bRedraw: BOOL): BOOL; stdcall;

function FlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer;
  p4: Bool): Bool; stdcall;


function InitializeFlatSB(hWnd: HWND): Bool; stdcall;

procedure UninitializeFlatSB(hWnd: HWND); stdcall;

implementation

const
  cctrl = 'comctl32.dll';

var
  ComCtl32DLL: THandle;
  _InitCommonControlsEx: function(var ICC: TInitCommonControlsEx): Bool stdcall;


procedure InitCommonControls; external cctrl name 'InitCommonControls';

procedure InitComCtl;
begin
  if ComCtl32DLL = 0 then begin
    ComCtl32DLL := GetModuleHandle(cctrl);
    if ComCtl32DLL > 0 then
      @_InitCommonControlsEx := GetProcAddress(ComCtl32DLL, 'InitCommonControlsEx');
  end;
end;

function InitCommonControlsEx(var ICC: TInitCommonControlsEx): Bool;
begin
  if ComCtl32DLL = 0 then InitComCtl;
  Result := Assigned(_InitCommonControlsEx) and _InitCommonControlsEx(ICC);
end;

{ Property Sheets }

function CreatePropertySheetPageA; external cctrl name 'CreatePropertySheetPageA';

function CreatePropertySheetPageW; external cctrl name 'CreatePropertySheetPageW';

function CreatePropertySheetPage; external cctrl name 'CreatePropertySheetPageA';

function DestroyPropertySheetPage; external cctrl name 'DestroyPropertySheetPage';

function PropertySheetA; external cctrl name 'PropertySheetA';

function PropertySheetW; external cctrl name 'PropertySheetW';

function PropertySheet; external cctrl name 'PropertySheetA';

{ Image List }

function ImageList_Create; external cctrl name 'ImageList_Create';

function ImageList_Destroy; external cctrl name 'ImageList_Destroy';

function ImageList_GetImageCount; external cctrl name 'ImageList_GetImageCount';

function ImageList_SetImageCount; external cctrl name 'ImageList_SetImageCount';

function ImageList_Add; external cctrl name 'ImageList_Add';

function ImageList_ReplaceIcon; external cctrl name 'ImageList_ReplaceIcon';

function ImageList_SetBkColor; external cctrl name 'ImageList_SetBkColor';

function ImageList_GetBkColor; external cctrl name 'ImageList_GetBkColor';

function ImageList_SetOverlayImage; external cctrl name 'ImageList_SetOverlayImage';


function ImageList_AddIcon(ImageList: HIMAGELIST; Icon: HIcon): Integer;
begin
  Result := ImageList_ReplaceIcon(ImageList, -1, Icon);
end;


function IndexToOverlayMask(Index: Integer): Integer;
begin
  Result := Index shl 8;
end;


function ImageList_Draw; external cctrl name 'ImageList_Draw';


function ImageList_Replace; external cctrl name 'ImageList_Replace';

function ImageList_AddMasked; external cctrl name 'ImageList_AddMasked';

function ImageList_DrawEx; external cctrl name 'ImageList_DrawEx';

function ImageList_DrawIndirect; external cctrl name 'ImageList_DrawIndirect';

function ImageList_Remove; external cctrl name 'ImageList_Remove';

function ImageList_GetIcon; external cctrl name 'ImageList_GetIcon';

function ImageList_LoadImageA; external cctrl name 'ImageList_LoadImageA';

function ImageList_LoadImageW; external cctrl name 'ImageList_LoadImageW';

function ImageList_LoadImage; external cctrl name 'ImageList_LoadImageA';

function ImageList_Copy; external cctrl name 'ImageList_Copy';

function ImageList_BeginDrag; external cctrl name 'ImageList_BeginDrag';

function ImageList_EndDrag; external cctrl name 'ImageList_EndDrag';

function ImageList_DragEnter; external cctrl name 'ImageList_DragEnter';

function ImageList_DragLeave; external cctrl name 'ImageList_DragLeave';

function ImageList_DragMove; external cctrl name 'ImageList_DragMove';

function ImageList_SetDragCursorImage; external cctrl name 'ImageList_SetDragCursorImage';

function ImageList_DragShowNolock; external cctrl name 'ImageList_DragShowNolock';

function ImageList_GetDragImage; external cctrl name 'ImageList_GetDragImage';

{ macros }

procedure ImageList_RemoveAll(ImageList: HIMAGELIST);
begin
  ImageList_Remove(ImageList, -1);
end;


function ImageList_ExtractIcon(Instance: THandle; ImageList: HIMAGELIST;
  Image: Integer): HIcon;
begin
  Result := ImageList_GetIcon(ImageList, Image, 0);
end;


function ImageList_LoadBitmap(Instance: THandle; Bmp: PChar;
  CX, Grow: Integer; Mask: TColorRef): HIMAGELIST;
begin
  Result := ImageList_LoadImage(Instance, Bmp, CX, Grow, Mask,
    IMAGE_BITMAP, 0);
end;

function ImageList_Read; external cctrl name 'ImageList_Read';
function ImageList_Write; external cctrl name 'ImageList_Write';


function ImageList_GetIconSize; external cctrl name 'ImageList_GetIconSize';

function ImageList_SetIconSize; external cctrl name 'ImageList_SetIconSize';

function ImageList_GetImageInfo; external cctrl name 'ImageList_GetImageInfo';

function ImageList_Merge; external cctrl name 'ImageList_Merge';

function ImageList_Duplicate(himl: HIMAGELIST): HIMAGELIST; stdcall; external cctrl name 'ImageList_Duplicate';

{ Headers }


function Header_GetItemCount(Header: HWnd): Integer;
begin
  Result := SendMessage(Header, HDM_GETITEMCOUNT, 0, 0);
end;


function Header_InsertItem(Header: HWnd; Index: Integer;
  const Item: THDItem): Integer;
begin
  Result := SendMessage(Header, HDM_INSERTITEM, Index, Longint(@Item));
end;


function Header_DeleteItem(Header: HWnd; Index: Integer): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_DELETEITEM, Index, 0) );
end;


function Header_GetItem(Header: HWnd; Index: Integer; var Item: THDItem): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_GETITEM, Index, Longint(@Item)) );
end;


function Header_SetItem(Header: HWnd; Index: Integer; const Item: THDItem): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_SETITEM, Index, Longint(@Item)) );
end;


function Header_Layout(Header: HWnd; Layout: PHDLayout): Bool;
begin
  Result := Bool( SendMessage(Header, HDM_LAYOUT, 0, Longint(Layout)) );
end;


function Header_GetItemRect(hwnd: HWND; iItem: Integer; lprc: PRect): Integer;
begin
  Result := SendMessage(hwnd, HDM_GETITEMRECT, iItem, LPARAM(lprc));
end;


function Header_SetImageList(hwnd: HWND; himl: HIMAGELIST): HIMAGELIST;
begin
  Result := SendMessage(hwnd, HDM_SETIMAGELIST, 0, LPARAM(himl));
end;


function Header_GetImageList(hwnd: HWND): HIMAGELIST;
begin
  Result := SendMessage(hwnd, HDM_GETIMAGELIST, 0, 0);
end;


function Header_OrderToIndex(hwnd: HWND; i: Integer): Integer;
begin
  Result := SendMessage(hwnd, HDM_ORDERTOINDEX, i, 0);
end;


function Header_CreateDragImage(hwnd: HWND; i: Integer): HIMAGELIST;
begin
  Result := SendMessage(hwnd, HDM_CREATEDRAGIMAGE, i, 0);
end;


function Header_GetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
begin
  Result := SendMessage(hwnd, HDM_GETORDERARRAY, iCount, LPARAM(lpi));
end;


function Header_SetOrderArray(hwnd: HWND; iCount: Integer; lpi: PInteger): Integer;
begin
  Result := SendMessage(hwnd, HDM_SETORDERARRAY, iCount, LPARAM(lpi));
end;


function Header_SetHotDivider(hwnd: HWND; fPos: BOOL; dw: DWORD): Integer;
begin
  Result := SendMessage(hwnd, HDM_SETHOTDIVIDER, Integer(fPos), dw);
end;


function Header_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer;
begin
  Result := SendMessage(hwnd, HDM_SETUNICODEFORMAT, Integer(fUnicode), 0);
end;


function Header_GetUnicodeFormat(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, HDM_GETUNICODEFORMAT, 0, 0);
end;

{ Toolbar }


function CreateToolBarEx; external cctrl name 'CreateToolbarEx';

function CreateMappedBitmap; external cctrl name 'CreateMappedBitmap';

{ Status bar }

procedure DrawStatusTextA; external cctrl name 'DrawStatusTextA';

procedure DrawStatusTextW; external cctrl name 'DrawStatusTextW';

procedure DrawStatusText; external cctrl name 'DrawStatusTextA';

function CreateStatusWindowA; external cctrl name 'CreateStatusWindowA';

function CreateStatusWindowW; external cctrl name 'CreateStatusWindowW';

function CreateStatusWindow; external cctrl name 'CreateStatusWindowA';

{ Menu Help }

procedure MenuHelp; external cctrl name 'MenuHelp';

function ShowHideMenuCtl; external cctrl name 'ShowHideMenuCtl';

procedure GetEffectiveClientRect; external cctrl name 'GetEffectiveClientRect';


{ Drag List Box }

procedure MakeDragList; external cctrl name 'MakeDragList';

procedure DrawInsert; external cctrl name 'DrawInsert';

function LBItemFromPt; external cctrl name 'LBItemFromPt';

{ UpDown control }

function CreateUpDownControl; external cctrl name 'CreateUpDownControl';


{ List View }

function ListView_GetUnicodeFormat(hwnd: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, LVM_GETUNICODEFORMAT, 0, 0));
end;


function ListView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): Integer;
begin
  Result := SendMessage(hwnd, LVM_SETUNICODEFORMAT, Integer(fUnicode), 0);
end;


function ListView_GetBkColor(hWnd: HWND): TColorRef;
begin
  Result := SendMessage(hWnd, LVM_GETBKCOLOR, 0, 0);
end;


function ListView_SetBkColor(hWnd: HWND; clrBk: TColorRef): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETBKCOLOR, 0, clrBk) );
end;


function ListView_GetImageList(hWnd: HWND; iImageList: Integer): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hWnd, LVM_GETIMAGELIST, iImageList, 0) );
end;


function ListView_SetImageList(hWnd: HWND; himl: HIMAGELIST; iImageList: Integer): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hWnd, LVM_SETIMAGELIST, iImageList, Longint(himl)) );
end;


function ListView_GetItemCount(hWnd: HWND): Integer;
begin
  Result := SendMessage(hWnd, LVM_GETITEMCOUNT, 0, 0);
end;


function IndexToStateImageMask(I: Longint): Longint;
begin
  Result := I shl 12;
end;


function ListView_GetItemA(hWnd: HWND; var pItem: TLVItemA): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_GETITEM, 0, Longint(@pItem)) );
end;

function ListView_GetItemW(hWnd: HWND; var pItem: TLVItemW): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_GETITEM, 0, Longint(@pItem)) );
end;

function ListView_GetItem(hWnd: HWND; var pItem: TLVItem): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_GETITEM, 0, Longint(@pItem)) );
end;


function ListView_SetItemA(hWnd: HWND; const pItem: TLVItemA): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETITEM, 0, Longint(@pItem)) );
end;

function ListView_SetItemW(hWnd: HWND; const pItem: TLVItemW): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETITEM, 0, Longint(@pItem)) );
end;

function ListView_SetItem(hWnd: HWND; const pItem: TLVItem): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETITEM, 0, Longint(@pItem)) );
end;


function ListView_InsertItemA(hWnd: HWND; const pItem: TLVItemA): Integer;
begin
  Result := Integer( SendMessage(hWnd, LVM_INSERTITEM, 0, Longint(@pItem)) );
end;

function ListView_InsertItemW(hWnd: HWND; const pItem: TLVItemW): Integer;
begin
  Result := Integer( SendMessage(hWnd, LVM_INSERTITEM, 0, Longint(@pItem)) );
end;

function ListView_InsertItem(hWnd: HWND; const pItem: TLVItem): Integer;
begin
  Result := Integer( SendMessage(hWnd, LVM_INSERTITEM, 0, Longint(@pItem)) );
end;


function ListView_DeleteItem(hWnd: HWND; i: Integer): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_DELETEITEM, i, 0) );
end;


function ListView_DeleteAllItems(hWnd: HWND): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_DELETEALLITEMS, 0, 0) );
end;


function ListView_GetCallbackMask(hWnd: HWND): UINT;
begin
  Result := SendMessage(hWnd, LVM_GETCALLBACKMASK, 0, 0);
end;


function ListView_SetCallbackMask(hWnd: HWND; mask: UINT): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETCALLBACKMASK, mask, 0) );
end;


function ListView_GetNextItem(hWnd: HWND; iStart: Integer; Flags: UINT): Integer;
begin
  Result := SendMessage(hWnd, LVM_GETNEXTITEM, iStart, MakeLong(Flags, 0));
end;


function ListView_FindItemA(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoA): Integer;
begin
  Result := SendMessage(hWnd, LVM_FINDITEM, iStart, Longint(@plvfi));
end;

function ListView_FindItemW(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfoW): Integer;
begin
  Result := SendMessage(hWnd, LVM_FINDITEM, iStart, Longint(@plvfi));
end;

function ListView_FindItem(hWnd: HWND; iStart: Integer;
  const plvfi: TLVFindInfo): Integer;
begin
  Result := SendMessage(hWnd, LVM_FINDITEM, iStart, Longint(@plvfi));
end;


function ListView_GetItemRect(hWnd: HWND; i: Integer; var prc: TRect;
  Code: Integer): Bool;
begin
  if @prc <> nil then
  begin
    prc.left := Code;
    Result := Bool( SendMessage(hWnd, LVM_GETITEMRECT, i, Longint(@prc)) );
  end
  else
    Result := Bool( SendMessage(hWnd, LVM_GETITEMRECT, i, 0) );
end;


function ListView_SetItemPosition(hWnd: HWND; i, x, y: Integer): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_SETITEMPOSITION, i, MakeLong(x, y)) );
end;


function ListView_GetItemPosition(hwndLV: HWND; i: Integer;
  var ppt: TPoint): Bool;
begin
  Result := Bool( SendMessage(hWndLV, LVM_GETITEMPOSITION, i, Longint(@ppt)) );
end;


function ListView_GetStringWidthA(hwndLV: HWND; psz: PAnsiChar): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETSTRINGWIDTH, 0, Longint(psz));
end;

function ListView_GetStringWidthW(hwndLV: HWND; psz: PWideChar): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETSTRINGWIDTH, 0, Longint(psz));
end;

function ListView_GetStringWidth(hwndLV: HWND; psz: PChar): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETSTRINGWIDTH, 0, Longint(psz));
end;


function ListView_HitTest(hwndLV: HWND; var pinfo: TLVHitTestInfo): Integer;
begin
  Result := SendMessage(hwndLV, LVM_HITTEST, 0, Longint(@pinfo));
end;


function ListView_EnsureVisible(hwndLV: HWND; i: Integer; fPartialOK: Bool): Bool;
begin
  Result := SendMessage(hwndLV, LVM_ENSUREVISIBLE, i,
    MakeLong(Integer(fPartialOK), 0)) <> 0;
end;


function ListView_Scroll(hwndLV: HWnd; DX, DY: Integer): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_SCROLL, DX, DY) );
end;


function ListView_RedrawItems(hwndLV: HWND; iFirst, iLast: Integer): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_REDRAWITEMS, iFirst, iLast) );
end;


function ListView_Arrange(hwndLV: HWND; Code: UINT): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_ARRANGE, Code, 0) );
end;


function ListView_EditLabelA(hwndLV: HWND; i: Integer): HWND;
begin
  Result := HWND( SendMessage(hwndLV, LVM_EDITLABEL, i, 0) );
end;

function ListView_EditLabelW(hwndLV: HWND; i: Integer): HWND;
begin
  Result := HWND( SendMessage(hwndLV, LVM_EDITLABEL, i, 0) );
end;

function ListView_EditLabel(hwndLV: HWND; i: Integer): HWND;
begin
  Result := HWND( SendMessage(hwndLV, LVM_EDITLABEL, i, 0) );
end;


function ListView_GetEditControl(hwndLV: HWND): HWND;
begin
  Result := HWND( SendMessage(hwndLV, LVM_GETEDITCONTROL, 0, 0) );
end;


function ListView_GetColumnA(hwnd: HWND; iCol: Integer; var pcol: TLVColumnA): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_GETCOLUMN, iCol, Longint(@pcol)) );
end;

function ListView_GetColumnW(hwnd: HWND; iCol: Integer; var pcol: TLVColumnW): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_GETCOLUMN, iCol, Longint(@pcol)) );
end;

function ListView_GetColumn(hwnd: HWND; iCol: Integer; var pcol: TLVColumn): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_GETCOLUMN, iCol, Longint(@pcol)) );
end;


function ListView_SetColumnA(hwnd: HWND; iCol: Integer; const pcol: TLVColumnA): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETCOLUMN, iCol, Longint(@pcol)) );
end;

function ListView_SetColumnW(hwnd: HWND; iCol: Integer; const pcol: TLVColumnW): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETCOLUMN, iCol, Longint(@pcol)) );
end;

function ListView_SetColumn(hwnd: HWND; iCol: Integer; const pcol: TLVColumn): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETCOLUMN, iCol, Longint(@pcol)) );
end;


function ListView_InsertColumnA(hwnd: HWND; iCol: Integer; const pcol: TLVColumnA): Integer;
begin
  Result := SendMessage(hWnd, LVM_INSERTCOLUMN, iCol, Longint(@pcol));
end;

function ListView_InsertColumnW(hwnd: HWND; iCol: Integer; const pcol: TLVColumnW): Integer;
begin
  Result := SendMessage(hWnd, LVM_INSERTCOLUMN, iCol, Longint(@pcol));
end;

function ListView_InsertColumn(hwnd: HWND; iCol: Integer; const pcol: TLVColumn): Integer;
begin
  Result := SendMessage(hWnd, LVM_INSERTCOLUMN, iCol, Longint(@pcol));
end;


function ListView_DeleteColumn(hwnd: HWND; iCol: Integer): Bool;
begin
  Result := Bool( SendMessage(hWnd, LVM_DELETECOLUMN, iCol, 0) );
end;


function ListView_GetColumnWidth(hwnd: HWND; iCol: Integer): Integer;
begin
  Result := Integer( SendMessage(hwnd, LVM_GETCOLUMNWIDTH, iCol, 0) );
end;


function ListView_SetColumnWidth(hwnd: HWnd; iCol: Integer; cx: Integer): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETCOLUMNWIDTH, iCol,
    MakeLong((cx), 0)) );
end;


function ListView_GetHeader(hwnd: HWND): HWND;
begin
  Result := SendMessage(hwnd, LVM_GETHEADER, 0, 0);
end;


function ListView_CreateDragImage(hwnd: HWND; i: Integer;
  const lpptUpLeft: TPoint): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hwnd, LVM_CREATEDRAGIMAGE, i,
    Longint(@lpptUpLeft)));
end;


function ListView_GetViewRect(hwnd: HWND; var prc: TRect): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_GETVIEWRECT, 0, Longint(@prc)) );
end;


function ListView_GetTextColor(hwnd: HWND): TColorRef;
begin
  Result := SendMessage(hwnd, LVM_GETTEXTCOLOR, 0, 0);
end;


function ListView_SetTextColor(hwnd: HWND; clrText: TColorRef): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETTEXTCOLOR, 0, clrText) );
end;


function ListView_GetTextBkColor(hwnd: HWND): TColorRef;
begin
  Result := SendMessage(hwnd, LVM_GETTEXTBKCOLOR, 0, 0);
end;


function ListView_SetTextBkColor(hwnd: HWND; clrTextBk: TColorRef): Bool;
begin
  Result := Bool( SendMessage(hwnd, LVM_SETTEXTBKCOLOR, 0, clrTextBk) );
end;


function ListView_GetTopIndex(hwndLV: HWND): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETTOPINDEX, 0, 0);
end;


function ListView_GetCountPerPage(hwndLV: HWND): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETCOUNTPERPAGE, 0, 0);
end;


function ListView_GetOrigin(hwndLV: HWND; var ppt: TPoint): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_GETORIGIN, 0, Longint(@ppt)) );
end;


function ListView_Update(hwndLV: HWND; i: Integer): Bool;
begin
  Result := SendMessage(hwndLV, LVM_UPDATE, i, 0) <> 0;
end;


function ListView_SetItemState(hwndLV: HWND; i: Integer; data, mask: UINT): Bool;
var
  Item: TLVItem;
begin
  Item.stateMask := mask;
  Item.state := data;
  Result := Bool( SendMessage(hwndLV, LVM_SETITEMSTATE, i, Longint(@Item)) );
end;


function ListView_GetItemState(hwndLV: HWND; i, mask: Integer): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETITEMSTATE, i, mask);
end;

function ListView_GetCheckState(hwndLV: HWND; i: Integer): UINT;
begin
  Result := (SendMessage(hwndLV, LVM_GETITEMSTATE, i, LVIS_STATEIMAGEMASK) shr 12) - 1 ;
end;

procedure ListView_SetCheckState(hwndLV: HWND; i: Integer; Checked: Boolean);
var
  Item: TLVItem;
begin
  Item.statemask := LVIS_STATEIMAGEMASK;
  Item.State := ((Integer(Checked) and 1) + 1) shl 12;
  SendMessage(hwndLV, LVM_SETITEMSTATE, i, Integer(@Item));
end;


function ListView_GetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar; cchTextMax: Integer): Integer;
var
  Item: TLVItemA;
begin
  Item.iSubItem := iSubItem;
  Item.cchTextMax := cchTextMax;
  Item.pszText := pszText;
  Result := SendMessage(hwndLV, LVM_GETITEMTEXT, i, Longint(@Item));
end;

function ListView_GetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar; cchTextMax: Integer): Integer;
var
  Item: TLVItemW;
begin
  Item.iSubItem := iSubItem;
  Item.cchTextMax := cchTextMax;
  Item.pszText := pszText;
  Result := SendMessage(hwndLV, LVM_GETITEMTEXT, i, Longint(@Item));
end;

function ListView_GetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar; cchTextMax: Integer): Integer;
var
  Item: TLVItem;
begin
  Item.iSubItem := iSubItem;
  Item.cchTextMax := cchTextMax;
  Item.pszText := pszText;
  Result := SendMessage(hwndLV, LVM_GETITEMTEXT, i, Longint(@Item));
end;


function ListView_SetItemTextA(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PAnsiChar): Bool;
var
  Item: TLVItemA;
begin
  Item.iSubItem := iSubItem;
  Item.pszText := pszText;
  Result := Bool( SendMessage(hwndLV, LVM_SETITEMTEXT, i, Longint(@Item)) );
end;

function ListView_SetItemTextW(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PWideChar): Bool;
var
  Item: TLVItemW;
begin
  Item.iSubItem := iSubItem;
  Item.pszText := pszText;
  Result := Bool( SendMessage(hwndLV, LVM_SETITEMTEXT, i, Longint(@Item)) );
end;

function ListView_SetItemText(hwndLV: HWND; i, iSubItem: Integer;
  pszText: PChar): Bool;
var
  Item: TLVItem;
begin
  Item.iSubItem := iSubItem;
  Item.pszText := pszText;
  Result := Bool( SendMessage(hwndLV, LVM_SETITEMTEXT, i, Longint(@Item)) );
end;


procedure ListView_SetItemCount(hwndLV: HWND; cItems: Integer);
begin
  SendMessage(hwndLV, LVM_SETITEMCOUNT, cItems, 0);
end;


procedure ListView_SetItemCountEx(hwndLV: HWND; cItems: Integer; dwFlags: DWORD);
begin
  SendMessage(hwndLV, LVM_SETITEMCOUNT, cItems, dwFlags);
end;


function ListView_SortItems(hwndLV: HWND; pfnCompare: TLVCompare;
  lPrm: Longint): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_SORTITEMS, lPrm,
    Longint(@pfnCompare)) );
end;


procedure ListView_SetItemPosition32(hwndLV: HWND; i, x, y: Integer);
var
  ptNewPos: TPoint;
begin
  ptNewPos.x := x;
  ptNewPos.y := y;
  SendMessage(hwndLV, LVM_SETITEMPOSITION32, i, Longint(@ptNewPos));
end;


function ListView_GetSelectedCount(hwndLV: HWND): UINT;
begin
  Result := SendMessage(hwndLV, LVM_GETSELECTEDCOUNT, 0, 0);
end;


function ListView_GetItemSpacing(hwndLV: HWND; fSmall: Integer): Longint;
begin
  Result := SendMessage(hwndLV, LVM_GETITEMSPACING, fSmall, 0);
end;


function ListView_GetISearchStringA(hwndLV: HWND; lpsz: PAnsiChar): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;

function ListView_GetISearchStringW(hwndLV: HWND; lpsz: PWideChar): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;

function ListView_GetISearchString(hwndLV: HWND; lpsz: PChar): Bool;
begin
  Result := Bool( SendMessage(hwndLV, LVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;

function ListView_SetIconSpacing(hwndLV: HWND; cx, cy: Word): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_SETICONSPACING, 0, MakeLong(cx, cy));
end;

function ListView_SetExtendedListViewStyle(hwndLV: HWND; dw: DWORD): BOOL;
begin
  Result := BOOL(SendMessage(hwndLV, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, dw));
end;

function ListView_GetExtendedListViewStyle(hwndLV: HWND): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0);
end;

function ListView_GetSubItemRect(hwndLV: HWND; iItem, iSubItem: Integer;
  code: DWORD; prc: PRect): BOOL;
begin
  if prc <> nil then
  begin
    prc^.Top := iSubItem;
    prc^.Left := code;
  end;
  Result := BOOL(SendMessage(hwndLV, LVM_GETSUBITEMRECT, iItem, Longint(prc)));
end;

function ListView_SubItemHitTest(hwndLV: HWND; plvhti: PLVHitTestInfo): Integer;
begin
  Result := SendMessage(hwndLV, LVM_SUBITEMHITTEST, 0, Longint(plvhti));
end;

function ListView_SetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL;
begin
  Result := BOOL(SendMessage(hwndLV, LVM_SETCOLUMNORDERARRAY, iCount,
    Longint(pi)));
end;

function ListView_GetColumnOrderArray(hwndLV: HWND; iCount: Integer;
  pi: PInteger): BOOL;
begin
  Result := BOOL(SendMessage(hwndLV, LVM_GETCOLUMNORDERARRAY, iCount,
    Longint(pi)));
end;

function ListView_SetHotItem(hwndLV: HWND; i: Integer): Integer;
begin
  Result := SendMessage(hwndLV, LVM_SETHOTITEM, i, 0);
end;

function ListView_GetHotItem(hwndLV: HWND): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETHOTITEM, 0, 0);
end;

function ListView_SetHotCursor(hwndLV: HWND; hcur: HCURSOR): HCURSOR;
begin
  Result := SendMessage(hwndLV, LVM_SETHOTCURSOR, 0, hcur);
end;

function ListView_GetHotCursor(hwndLV: HWND): HCURSOR;
begin
  Result := SendMessage(hwndLV, LVM_GETHOTCURSOR, 0, 0);
end;

function ListView_ApproximateViewRect(hwndLV: HWND; iWidth, iHeight: Word;
  iCount: Integer): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_APPROXIMATEVIEWRECT, iCount,
    MakeLParam(iWidth, iHeight));
end;

function ListView_SetWorkAreas(hwndLV: HWND; prc: PRect): BOOL;
begin
  Result := BOOL(SendMessage(hwndLV, LVM_SETWORKAREA, 0, Longint(prc)));
end;


function ListView_GetSelectionMark(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, LVM_GETSELECTIONMARK, 0, 0);
end;


function ListView_SetSelectionMark(hwnd: HWND; i: Integer): Integer;
begin
  Result := SendMessage(hwnd, LVM_SETSELECTIONMARK, 0, i);
end;


function ListView_GetWorkAreas(hwnd: HWND; nWorkAreas: Integer; prc: PRect): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, LVM_GETWORKAREAS, nWorkAreas, Integer(prc)));
end;


function ListView_SetHoverTime(hwndLV: HWND; dwHoverTimeMs: DWORD): DWORD;
begin
  Result := SendMessage(hwndLV, LVM_SETHOVERTIME, 0, dwHoverTimeMs);
end;


function ListView_GetHoverTime(hwndLV: HWND): Integer;
begin
  Result := SendMessage(hwndLV, LVM_GETHOVERTIME, 0, 0);
end;


function ListView_GetNumberOfWorkAreas(hwnd: HWND; pnWorkAreas: PInteger): Integer;
begin
  Result := SendMessage(hwnd, LVM_GETNUMBEROFWORKAREAS, 0, Integer(pnWorkAreas));
end;


function ListView_SetToolTips(hwndLV: HWND; hwndNewHwnd: HWND): HWND;
begin
  Result := HWND(SendMessage(hwndLV, LVM_SETTOOLTIPS, WPARAM(hwndNewHwnd), 0));
end;


function ListView_GetToolTips(hwndLV: HWND): HWND;
begin
  Result := HWND(SendMessage(hwndLV, LVM_GETTOOLTIPS, 0, 0));
end;


function ListView_SetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, LVM_SETBKIMAGE, 0, LPARAM(plvbki)));
end;

function ListView_GetBkImage(hwnd: HWND; plvbki: PLVBKImage): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, LVM_GETBKIMAGE, 0, LPARAM(plvbki)));
end;

{ Tree View }


function TreeView_InsertItem(hwnd: HWND; const lpis: TTVInsertStruct): HTreeItem;
begin
  Result := HTreeItem( SendMessage(hwnd, TVM_INSERTITEM, 0, Longint(@lpis)) );
end;


function TreeView_DeleteItem(hwnd: HWND; hitem: HTreeItem): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_DELETEITEM, 0, Longint(hitem)) );
end;


function TreeView_DeleteAllItems(hwnd: HWND): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_DELETEITEM, 0, Longint(TVI_ROOT)) );
end;


function TreeView_Expand(hwnd: HWND; hitem: HTreeItem; code: Integer): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_EXPAND, code, Longint(hitem)) );
end;


function TreeView_GetItemRect(hwnd: HWND; hitem: HTreeItem;
  var prc: TRect; code: Bool): Bool;
begin
  HTreeItem(Pointer(@prc)^) := hitem;
  Result := Bool( SendMessage(hwnd, TVM_GETITEMRECT, Integer(code), Longint(@prc)) );
end;


function TreeView_GetCount(hwnd: HWND): UINT;
begin
  Result := SendMessage(hwnd, TVM_GETCOUNT, 0, 0);
end;


function TreeView_GetIndent(hwnd: HWND): UINT;
begin
  Result := SendMessage(hwnd, TVM_GETINDENT, 0, 0);
end;


function TreeView_SetIndent(hwnd: HWND; indent: Integer): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SETINDENT, indent, 0) );
end;


function TreeView_GetImageList(hwnd: HWND; iImage: Integer): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hwnd, TVM_GETIMAGELIST, iImage, 0) );
end;


function TreeView_SetImageList(hwnd: HWND; himl: HIMAGELIST;
  iImage: Integer): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hwnd, TVM_SETIMAGELIST, iImage,
    Longint(himl)) );
end;


function TreeView_GetNextItem(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem;
begin
  Result := HTreeItem( SendMessage(hwnd, TVM_GETNEXTITEM, code,
    Longint(hitem)) );
end;


function TreeView_GetChild(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_CHILD);
end;


function TreeView_GetNextSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_NEXT);
end;


function TreeView_GetPrevSibling(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUS);
end;


function TreeView_GetParent(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_PARENT);
end;


function TreeView_GetFirstVisible(hwnd: HWND): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, nil,  TVGN_FIRSTVISIBLE);
end;


function TreeView_GetNextVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_NEXTVISIBLE);
end;


function TreeView_GetPrevVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, hitem, TVGN_PREVIOUSVISIBLE);
end;


function TreeView_GetSelection(hwnd: HWND): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, nil, TVGN_CARET);
end;

function TreeView_GetDropHilite(hwnd: HWND): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, nil, TVGN_DROPHILITE);
end;


function TreeView_GetRoot(hwnd: HWND): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, nil, TVGN_ROOT);
end;


function TreeView_GetLastVisible(hwnd: HWND): HTreeItem;
begin
  Result := TreeView_GetNextItem(hwnd, nil,  TVGN_LASTVISIBLE);
end;


function TreeView_Select(hwnd: HWND; hitem: HTreeItem;
  code: Integer): HTreeItem;
begin
  Result := HTreeItem( SendMessage(hwnd, TVM_SELECTITEM, code,
    Longint(hitem)) );
end;


function TreeView_SelectItem(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_Select(hwnd, hitem, TVGN_CARET);
end;


function TreeView_SelectDropTarget(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_Select(hwnd, hitem, TVGN_DROPHILITE);
end;


function TreeView_SelectSetFirstVisible(hwnd: HWND; hitem: HTreeItem): HTreeItem;
begin
  Result := TreeView_Select(hwnd, hitem, TVGN_FIRSTVISIBLE);
end;


function TreeView_GetItemA(hwnd: HWND; var pitem: TTVItemA): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_GETITEM, 0, Longint(@pitem)) );
end;

function TreeView_GetItemW(hwnd: HWND; var pitem: TTVItemW): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_GETITEM, 0, Longint(@pitem)) );
end;

function TreeView_GetItem(hwnd: HWND; var pitem: TTVItem): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_GETITEM, 0, Longint(@pitem)) );
end;


function TreeView_SetItemA(hwnd: HWND; const pitem: TTVItemA): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SETITEM, 0, Longint(@pitem)) );
end;

function TreeView_SetItemW(hwnd: HWND; const pitem: TTVItemW): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SETITEM, 0, Longint(@pitem)) );
end;

function TreeView_SetItem(hwnd: HWND; const pitem: TTVItem): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SETITEM, 0, Longint(@pitem)) );
end;


function TreeView_EditLabelA(hwnd: HWND; hitem: HTreeItem): HWND;
begin
  Result := Winapi.Windows.HWND( SendMessage(hwnd, TVM_EDITLABEL, 0, Longint(hitem)) );
end;

function TreeView_EditLabelW(hwnd: HWND; hitem: HTreeItem): HWND;
begin
  Result := Winapi.Windows.HWND( SendMessage(hwnd, TVM_EDITLABEL, 0, Longint(hitem)) );
end;

function TreeView_EditLabel(hwnd: HWND; hitem: HTreeItem): HWND;
begin
  Result := Winapi.Windows.HWND( SendMessage(hwnd, TVM_EDITLABEL, 0, Longint(hitem)) );
end;


function TreeView_GetEditControl(hwnd: HWND): HWND;
begin
  Result := Winapi.Windows.HWND( SendMessage(hwnd, TVM_GETEDITCONTROL, 0, 0) );
end;


function TreeView_GetVisibleCount(hwnd: HWND): UINT;
begin
  Result := SendMessage(hwnd, TVM_GETVISIBLECOUNT, 0, 0);
end;


function TreeView_HitTest(hwnd: HWND; var lpht: TTVHitTestInfo): HTreeItem;
begin
  Result := HTreeItem( SendMessage(hwnd, TVM_HITTEST, 0, Longint(@lpht)) );
end;


function TreeView_CreateDragImage(hwnd: HWND; hitem: HTreeItem): HIMAGELIST;
begin
  Result := HIMAGELIST( SendMessage(hwnd, TVM_CREATEDRAGIMAGE, 0,
    Longint(hitem)) );
end;


function TreeView_SortChildren(hwnd: HWND; hitem: HTreeItem;
  recurse: Integer): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SORTCHILDREN, recurse,
    Longint(hitem)) );
end;


function TreeView_EnsureVisible(hwnd: HWND; hitem: HTreeItem): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_ENSUREVISIBLE, 0, Longint(hitem)) );
end;


function TreeView_SortChildrenCB(hwnd: HWND; const psort: TTVSortCB;
  recurse: Integer): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_SORTCHILDRENCB, recurse,
    Longint(@psort)) );
end;


function TreeView_EndEditLabelNow(hwnd: HWND; fCancel: Bool): Bool;
begin
  Result := Bool( SendMessage(hwnd, TVM_ENDEDITLABELNOW, Integer(fCancel),
    0) );
end;


function TreeView_GetISearchStringA(hwndTV: HWND; lpsz: PAnsiChar): Bool;
begin
  Result := Bool( SendMessage(hwndTV, TVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;

function TreeView_GetISearchStringW(hwndTV: HWND; lpsz: PWideChar): Bool;
begin
  Result := Bool( SendMessage(hwndTV, TVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;

function TreeView_GetISearchString(hwndTV: HWND; lpsz: PChar): Bool;
begin
  Result := Bool( SendMessage(hwndTV, TVM_GETISEARCHSTRING, 0,
    Longint(lpsz)) );
end;


function TreeView_SetToolTips(wnd: HWND; hwndTT: HWND): HWND;
begin
  Result := HWND(SendMessage(wnd, TVM_SETTOOLTIPS, WPARAM(hwndTT), 0));
end;


function TreeView_GetToolTips(wnd: HWND): HWND;
begin
  Result := HWND(SendMessage(wnd, TVM_GETTOOLTIPS, 0, 0));
end;


function TreeView_SetInsertMark(hwnd: HWND; hItem: Integer; fAfter: BOOL): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, TVM_SETINSERTMARK, WPARAM(fAfter), LPARAM(hItem)));
end;


function TreeView_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, TVM_SETUNICODEFORMAT, WPARAM(fUnicode), 0));
end;


function TreeView_GetUnicodeFormat(hwnd: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, TVM_GETUNICODEFORMAT, 0, 0));
end;


function TreeView_SetItemHeight(hwnd: HWND; iHeight: Integer): Integer;
begin
  Result := SendMessage(hwnd, TVM_SETITEMHEIGHT, iHeight, 0);
end;


function TreeView_GetItemHeight(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TVM_GETITEMHEIGHT, 0, 0);
end;


function TreeView_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_SETBKCOLOR, 0, LPARAM(clr)));
end;


function TreeView_SetTextColor(hwnd: HWND; clr: COLORREF): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_SETTEXTCOLOR, 0, LPARAM(clr)));
end;


function TreeView_GetBkColor(hwnd: HWND): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_GETBKCOLOR, 0, 0));
end;


function TreeView_GetTextColor(hwnd: HWND): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_GETTEXTCOLOR, 0, 0));
end;


function TreeView_SetScrollTime(hwnd: HWND; uTime: UINT): UINT;
begin
  Result := SendMessage(hwnd, TVM_SETSCROLLTIME, uTime, 0);
end;


function TreeView_GetScrollTime(hwnd: HWND): UINT;
begin
  Result := SendMessage(hwnd, TVM_GETSCROLLTIME, 0, 0);
end;


function TreeView_SetInsertMarkColor(hwnd: HWND; clr: COLORREF): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_SETINSERTMARKCOLOR, 0, LPARAM(clr)));
end;


function TreeView_GetInsertMarkColor(hwnd: HWND): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, TVM_GETINSERTMARKCOLOR, 0, 0));
end;

{ Tab control }


function TabCtrl_HitTest(hwndTC: HWND; pinfo: PTCHitTestInfo): Integer;
begin
  Result := SendMessage(hwndTC, TCM_HITTEST, 0, LPARAM(pinfo));
end;


function TabCtrl_SetItemExtra(hwndTC: HWND; cb: Integer): BOOL;
begin
  Result := BOOL(SendMessage(hwndTC, TCM_SETITEMEXTRA, cb, 0));
end;


function TabCtrl_AdjustRect(hwnd: HWND; bLarger: BOOL; prc: PRect): Integer;
begin
  Result := SendMessage(hwnd, TCM_ADJUSTRECT, WPARAM(bLarger), LPARAM(prc));
end;


function TabCtrl_SetItemSize(hwnd: HWND; x, y: Integer): DWORD;
begin
  Result := SendMessage(hwnd, TCM_SETITEMSIZE, 0, MAKELPARAM(x, y));
end;


procedure TabCtrl_RemoveImage(hwnd: HWND; i: Integer);
begin
  SendMessage(hwnd, TCM_REMOVEIMAGE, i, 0);
end;


procedure TabCtrl_SetPadding(hwnd: HWND; cx, cy: Integer);
begin
  SendMessage(hwnd, TCM_SETPADDING, 0, MAKELPARAM(cx, cy));
end;


function TabCtrl_GetRowCount(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TCM_GETROWCOUNT, 0, 0);
end;


function TabCtrl_GetToolTips(wnd: HWND): HWND;
begin
  Result := HWND(SendMessage(wnd, TCM_GETTOOLTIPS, 0, 0));
end;


procedure TabCtrl_SetToolTips(hwnd: HWND; hwndTT: HWND);
begin
  SendMessage(hwnd, TCM_SETTOOLTIPS, WPARAM(hwndTT), 0);
end;


function TabCtrl_GetCurFocus(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, TCM_GETCURFOCUS, 0, 0);
end;


procedure TabCtrl_SetCurFocus(hwnd: HWND; i: Integer);
begin
  SendMessage(hwnd,TCM_SETCURFOCUS, i, 0);
end;


function TabCtrl_SetMinTabWidth(hwnd: HWND; x: Integer): Integer;
begin
  Result := SendMessage(hwnd, TCM_SETMINTABWIDTH, 0, x);
end;


procedure TabCtrl_DeselectAll(hwnd: HWND; fExcludeFocus: BOOL);
begin
  SendMessage(hwnd, TCM_DESELECTALL, WPARAM(fExcludeFocus), 0)
end;


function TabCtrl_HighlightItem(hwnd: HWND; i: Integer; fHighlight: WordBool): BOOL;
begin
  Result :=  BOOL(SendMessage(hwnd, TCM_HIGHLIGHTITEM, i, MAKELONG(Word(fHighlight), 0)));
end;


function TabCtrl_SetExtendedStyle(hwnd: HWND; dw: DWORD): DWORD;
begin
  Result := SendMessage(hwnd, TCM_SETEXTENDEDSTYLE, 0, dw);
end;


function TabCtrl_GetExtendedStyle(hwnd: HWND): DWORD;
begin
  Result := SendMessage(hwnd, TCM_GETEXTENDEDSTYLE, 0, 0);
end;


function TabCtrl_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, TCM_SETUNICODEFORMAT, WPARAM(fUnicode), 0));
end;


function TabCtrl_GetUnicodeFormat(hwnd: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, TCM_GETUNICODEFORMAT, 0, 0));
end;

{ Animate control }


function Animate_Create(hwndP: HWND; id: HMENU; dwStyle: DWORD; hInstance: HINST): HWND;
begin
  Result := CreateWindow(ANIMATE_CLASS, nil, dwStyle, 0, 0, 0, 0, hwndP, id,
    hInstance, nil);
end;


function Animate_Open(hwnd: HWND; szName: PChar): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, ACM_OPEN, 0, LPARAM(szName)));
end;


function Animate_OpenEx(hwnd: HWND; hInst: HINST; szName: PChar): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, ACM_OPEN, WPARAM(hInst), LPARAM(szName)));
end;


function Animate_Play(hwnd: HWND; from, _to: Word; rep: UINT): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, ACM_PLAY, rep, MAKELONG(from, _to)));
end;


function Animate_Stop(hwnd: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, ACM_STOP, 0, 0));
end;


function Animate_Close(hwnd: HWND): BOOL;
begin
  Result := Animate_Open(hwnd, nil);
end;


function Animate_Seek(hwnd: HWND; frame: Word): BOOL;
begin
  Result := Animate_Play(hwnd, frame, frame, 1);
end;

{ MonthCal control }


function MonthCal_GetCurSel(hmc: HWND; var pst: TSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_GETCURSEL, 0, Longint(@pst)));
end;


function MonthCal_SetCurSel(hmc: HWND; const pst: TSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETCURSEL, 0, Longint(@pst)));
end;


function MonthCal_GetMaxSelCount(hmc: HWND): DWORD;
begin
  Result := SendMessage(hmc, MCM_GETMAXSELCOUNT, 0, 0);
end;


function MonthCal_SetMaxSelCount(hmc: HWND; n: UINT): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETMAXSELCOUNT, n, 0));
end;


function MonthCal_GetSelRange(hmc: HWND; rgst: PSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_GETSELRANGE, 0, Longint(rgst)));
end;


function MonthCal_SetSelRange(hmc: HWND; rgst: PSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETSELRANGE, 0, Longint(rgst)));
end;


function MonthCal_GetMonthRange(hmc: HWND; gmr: DWORD; rgst: PSystemTime): DWORD;
begin
  Result := SendMessage(hmc, MCM_GETMONTHRANGE, gmr, Longint(rgst));
end;


function MonthCal_SetDayState(hmc: HWND; cbds: Integer; const rgds: TNMDayState): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETDAYSTATE, cbds, Longint(@rgds)));
end;


function MonthCal_GetMinReqRect(hmc: HWND; var prc: TRect): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_GETMINREQRECT, 0, Longint(@prc)));
end;


function MonthCal_SetToday(hmc: HWND; const pst: TSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETTODAY, 0, Longint(@pst)));
end;


function MonthCal_GetToday(hmc: HWND; var pst: TSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_GETTODAY, 0, Longint(@pst)));
end;


function MonthCal_HitTest(hmc: HWND; var info: TMCHitTestInfo): DWORD;
begin
  Result := SendMessage(hmc, MCM_HITTEST, 0, Longint(@info));
end;


function MonthCal_SetColor(hmc: HWND; iColor: Integer; clr: TColorRef): TColorRef;
begin
  Result := TColorRef(SendMessage(hmc, MCM_SETCOLOR, iColor, clr));
end;


function MonthCal_GetColor(hmc: HWND; iColor: Integer): TColorRef;
begin
  Result := TColorRef(SendMessage(hmc, MCM_SETCOLOR, iColor, 0));
end;


function MonthCal_SetFirstDayOfWeek(hmc: HWND; iDay: Integer): Integer;
begin
  Result := SendMessage(hmc, MCM_SETFIRSTDAYOFWEEK, 0, iDay);
end;


function MonthCal_GetFirstDayOfWeek(hmc: HWND): Integer;
begin
  Result := SendMessage(hmc, MCM_GETFIRSTDAYOFWEEK, 0, 0);
end;


function MonthCal_GetRange(hmc: HWND; rgst: PSystemTime): DWORD;
begin
  Result := SendMessage(hmc, MCM_GETRANGE, 0, Longint(rgst));
end;


function Monthcal_SetRange(hmc: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hmc, MCM_SETRANGE, gdtr, Longint(rgst)));
end;


function MonthCal_GetMonthDelta(hmc: HWND): Integer;
begin
  Result := SendMessage(hmc, MCM_GETMONTHDELTA, 0, 0);
end;


function MonthCal_SetMonthDelta(hmc: HWND; n: Integer): Integer;
begin
  Result := SendMessage(hmc, MCM_SETMONTHDELTA, n, 0);
end;


function MonthCal_GetMaxTodayWidth(hmc: HWND): DWORD;
begin
  Result := SendMessage(hmc, MCM_GETMAXTODAYWIDTH, 0, 0);
end;


function MonthCal_SetUnicodeFormat(hwnd: HWND; fUnicode: BOOL): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, MCM_SETUNICODEFORMAT, WPARAM(fUnicode), 0));
end;


function MonthCal_GetUnicodeFormat(hwnd: HWND): BOOL;
begin
  Result := BOOL(SendMessage(hwnd, MCM_GETUNICODEFORMAT, 0, 0));
end;

{ Date/Time Picker }


function DateTime_GetSystemTime(hdp: HWND; var pst: TSystemTime): DWORD;
begin
  Result := SendMessage(hdp, DTM_GETSYSTEMTIME, 0, Longint(@pst));
end;


function DateTime_SetSystemTime(hdp: HWND; gd: DWORD; const pst: TSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hdp, DTM_SETSYSTEMTIME, gd, Longint(@pst)));
end;


function DateTime_GetRange(hdp: HWND; rgst: PSystemTime): DWORD;
begin
  Result := SendMessage(hdp, DTM_GETRANGE, 0, Longint(rgst));
end;


function DateTime_SetRange(hdp: HWND; gdtr: DWORD; rgst: PSystemTime): BOOL;
begin
  Result := BOOL(SendMessage(hdp, DTM_SETRANGE, gdtr, Longint(rgst)));
end;


function DateTime_SetFormatA(hdp: HWND; sz: PAnsiChar): BOOL;
begin
  Result := BOOL(SendMessage(hdp, DTM_SETFORMATA, 0, Longint(sz)));
end;

function DateTime_SetFormatW(hdp: HWND; sz: PWideChar): BOOL;
begin
  Result := BOOL(SendMessage(hdp, DTM_SETFORMATW, 0, Longint(sz)));
end;

function DateTime_SetFormat(hdp: HWND; sz: PChar): BOOL;
begin
  Result := BOOL(SendMessage(hdp, DTM_SETFORMAT, 0, Longint(sz)));
end;


function DateTime_SetMonthCalColor(hdp: HWND; iColor: DWORD; clr: TColorRef): TColorRef;
begin
  Result := TColorRef(SendMessage(hdp, DTM_SETMCCOLOR, iColor, clr));
end;


function DateTime_GetMonthCalColor(hdp: HWND; iColor: DWORD): TColorRef;
begin
  Result := SendMessage(hdp, DTM_GETMCCOLOR, iColor, 0);
end;


function DateTime_GetMonthCal(hdp: HWND): HWND;
begin
  Result := SendMessage(hdp, DTM_GETMONTHCAL, 0, 0);
end;


procedure DateTime_SetMonthCalFont(hdp: HWND; hfont: HFONT; fRedraw: BOOL);
begin
  SendMessage(hdp, DTM_SETMCFONT, WPARAM(hfont), LPARAM(fRedraw));
end;


function DateTime_GetMonthCalFont(hdp: HWND): HFONT;
begin
  Result := HFONT(SendMessage(hdp, DTM_GETMCFONT, 0, 0));
end;

{ IP Address edit control }


function MAKEIPRANGE(low, high: Byte): LPARAM;
begin
  Result := high;
  Result := (Result shl 8) + low;
end;

function MAKEIPADDRESS(b1, b2, b3, b4: DWORD): LPARAM;
begin
  Result := (b1 shl 24) + (b2 shl 16) + (b3 shl 8) + b4;
end;

function FIRST_IPADDRESS(x: DWORD): DWORD;
begin
  Result := (x shr 24) and $FF;
end;

function SECOND_IPADDRESS(x: DWORD): DWORD;
begin
  Result := (x shr 16) and $FF;
end;

function THIRD_IPADDRESS(x: DWORD): DWORD;
begin
  Result := (x shr 8) and $FF;
end;

function FOURTH_IPADDRESS(x: DWORD): DWORD;
begin
  Result := x and $FF;
end;

{ Pager control }


procedure Pager_SetChild(hwnd: HWND; hwndChild: HWND);
begin
  SendMessage(hwnd, PGM_SETCHILD, 0, LPARAM(hwndChild));
end;


procedure Pager_RecalcSize(hwnd: HWND);
begin
  SendMessage(hwnd, PGM_RECALCSIZE, 0, 0);
end;


procedure Pager_ForwardMouse(hwnd: HWND; bForward: BOOL);
begin
  SendMessage(hwnd, PGM_FORWARDMOUSE, WPARAM(bForward), 0);
end;


function Pager_SetBkColor(hwnd: HWND; clr: COLORREF): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, PGM_SETBKCOLOR, 0, LPARAM(clr)));
end;


function Pager_GetBkColor(hwnd: HWND): COLORREF;
begin
  Result := COLORREF(SendMessage(hwnd, PGM_GETBKCOLOR, 0, 0));
end;


function Pager_SetBorder(hwnd: HWND; iBorder: Integer): Integer;
begin
  Result := SendMessage(hwnd, PGM_SETBORDER, 0, iBorder);
end;


function Pager_GetBorder(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, PGM_GETBORDER, 0, 0);
end;


function Pager_SetPos(hwnd: HWND; iPos: Integer): Integer;
begin
  Result := SendMessage(hwnd, PGM_SETPOS, 0, iPos);
end;


function Pager_GetPos(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, PGM_GETPOS, 0, 0);
end;


function Pager_SetButtonSize(hwnd: HWND; iSize: Integer): Integer;
begin
  Result := SendMessage(hwnd, PGM_SETBUTTONSIZE, 0, iSize);
end;


function Pager_GetButtonSize(hwnd: HWND): Integer;
begin
  Result := SendMessage(hwnd, PGM_GETBUTTONSIZE, 0,0);
end;


function Pager_GetButtonState(hwnd: HWND; iButton: Integer): DWORD;
begin
  Result := SendMessage(hwnd, PGM_GETBUTTONSTATE, 0, iButton);
end;


procedure Pager_GetDropTarget(hwnd: HWND; ppdt: Pointer{!!});
begin
  SendMessage(hwnd, PGM_GETDROPTARGET, 0, LPARAM(ppdt));
end;

{ TrackMouseEvent }

function _TrackMouseEvent;              external cctrl name '_TrackMouseEvent';

{ Flat Scrollbar APIs }


function FlatSB_EnableScrollBar;        external cctrl name 'FlatSB_EnableScrollBar';
function FlatSB_GetScrollInfo;          external cctrl name 'FlatSB_GetScrollInfo';
function FlatSB_GetScrollPos;           external cctrl name 'FlatSB_GetScrollPos';
function FlatSB_GetScrollProp;          external cctrl name 'FlatSB_GetScrollProp';
function FlatSB_GetScrollRange;         external cctrl name 'FlatSB_GetScrollRange';
function FlatSB_SetScrollInfo;          external cctrl name 'FlatSB_SetScrollInfo';
function FlatSB_SetScrollPos;           external cctrl name 'FlatSB_SetScrollPos';
function FlatSB_SetScrollProp;          external cctrl name 'FlatSB_SetScrollProp';
function FlatSB_SetScrollRange;         external cctrl name 'FlatSB_SetScrollRange';
function FlatSB_ShowScrollBar;          external cctrl name 'FlatSB_ShowScrollBar';
function InitializeFlatSB;              external cctrl name 'InitializeFlatSB';
procedure UninitializeFlatSB;           external cctrl name 'UninitializeFlatSB';

end.
