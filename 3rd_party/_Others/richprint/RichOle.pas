Unit RichOLE;

{*
 *	RICHOLE.H
 *
 *	Purpose:
 *		OLE Extensions to the Rich Text Editor
 *
 *	Copyright (c) 1985-1996, Microsoft Corporation
 *}

{WEAKPACKAGEUNIT}
{$MINENUMSIZE 4}

interface

uses
   Winapi.Windows,
   Winapi.ActiveX,
   Winapi.RichEdit,
   Vcl.OleCtnrs,
   ComCtrls {ComObj,};

// Structure passed to GetObject and InsertObject
type
  TREOBJECT = packed record
    cbStruct: DWORD;			// Size of structure
    cp: longint;					// Character position of object
    clsid: TCLSID;				// Class ID of object
    oleobj: IOleObject;			// OLE object interface
    stg: IStorage;				// Associated storage interface
    olesite: IOLEClientSite;			// Associated client site interface
    sizel: TSize;				// Size of object (may be 0,0)
    dvaspect: DWORD;			// Display aspect to use
    dwFlags: DWORD;			// Object status flags
    dwUser: DWORD;				// Dword for user's use
  end;

const               
// Flags to specify which interfaces should be returned in the structure above
  REO_GETOBJ_NO_INTERFACES	= $00000000;
  REO_GETOBJ_POLEOBJ			= $00000001;
  REO_GETOBJ_PSTG				= $00000002;
  REO_GETOBJ_POLESITE			= $00000004;
  REO_GETOBJ_ALL_INTERFACES	= $00000007;

// Place object at selection
  REO_CP_SELECTION = $FFFFFFFF;

// Use character position to specify object instead of index
  REO_IOB_SELECTION = $FFFFFFFF;
  REO_IOB_USE_CP  = $FFFFFFFE;

// Object flags
  REO_NULL			= $00000000;	// No flags
  REO_READWRITEMASK	= $0000003F;	// Mask out RO bits
  REO_DONTNEEDPALETTE	= $00000020;	// Object doesn't need palette
  REO_BLANK			= $00000010;	// Object is blank
  REO_DYNAMICSIZE		= $00000008;	// Object defines size always
  REO_INVERTEDSELECT	= $00000004;	// Object drawn all inverted if sel
  REO_BELOWBASELINE	= $00000002;	// Object sits below the baseline
  REO_RESIZABLE		= $00000001;	// Object may be resized
  REO_LINK			= $80000000;	// Object is a link (RO)
  REO_STATIC			= $40000000;	// Object is static (RO)
  REO_SELECTED		= $08000000;	// Object selected (RO)
  REO_OPEN			= $04000000;	// Object open in its server (RO)
  REO_INPLACEACTIVE	= $02000000;	// Object in place active (RO)
  REO_HILITED			= $01000000;	// Object is to be hilited (RO)
  REO_LINKAVAILABLE	= $00800000;	// Link believed available (RO)
  REO_GETMETAFILE		= $00400000;	// Object requires metafile (RO)

// flags for IRichEditOle::GetClipboardData(),
// IRichEditOleCallback::GetClipboardData() and
// IRichEditOleCallback::QueryAcceptData()
  RECO_PASTE			= $00000000;	// paste from clipboard
  RECO_DROP			= $00000001;	// drop
  RECO_COPY			= $00000002;	// copy to the clipboard
  RECO_CUT			= $00000003;	// cut to the clipboard
  RECO_DRAG			= $00000004;	// drag

{*
 *	IRichEditOle
 *
 *	Purpose:
 *		Interface used by the client of RichEdit to perform OLE-related
 *		operations.
 *
 *	//$ REVIEW:
 *		The methods herein may just want to be regular Windows messages.
 *}

type
  IRichEditOle = interface(IUnknown)
    ['{00020D00-0000-0000-C000-000000000046}']
    function GetClientSite(out lplpolesite: IOLECLIENTSITE): HResult; stdcall;
    function GetObjectCount: longint; stdcall;
    function GetLinkCount: longint; stdcall;
    function GetObject(iob: longint; out reobject: TREOBJECT; dwFlags: DWORD): HRESULT; stdcall;
    function InsertObject(const reobject: TREOBJECT): HResult; stdcall;
    function ConvertObject(iob: longint; const clsidNew: TCLSID;
       lpStrUserTypeNew: POleStr): HRESULT; stdcall;
    function ActivateAs(const clsid, clsidAs: TCLSID): HRESULT; stdcall;
    function SetHostNames(lpstrContainerApp, lpstrContainerObj: POleStr): HRESULT; stdcall;
    function SetLinkAvailable(iob: longint; fAvailable: BOOL): HRESULT; stdcall;
    function SetDvaspect(iob: longint; dvaspect: DWORD): HRESULT; stdcall;
    function HandsOffStorage(iob: longint): HRESULT; stdcall;
    function SaveCompleted(iob: longint; stg: IStorage): HRESULT; stdcall;
    function InPlaceDeactivate: HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function ImportDataObject(dataobj: IDataObject; cf: TClipFormat;
         hMetaPict: HGLOBAL): HRESULT; stdcall;
  end;

{*
 *	IRichEditOleCallback
 *
 *	Purpose:
 *		Interface used by the RichEdit to get OLE-related stuff from the
 *		application using RichEdit.
 *}
  IRichEditOleCallback = interface(IUnknown)
    ['{00020D03-0000-0000-C000-000000000046}']
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
         out Doc: IOleInPlaceUIWindow; var FrameInfo: TOleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; stg: IStorage; cp: longint): HRESULT; stdcall;
    function DeleteObject(oleobj: IOLEObject): HRESULT; stdcall;
    function QueryAcceptData(dataobj: IDataObject; var cfFormat: TClipFormat;
         reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD;
         out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
         var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
         const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;



function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;

implementation

function RichEdit_SetOleCallback(RichEdit: HWnd; OleInterface: IRichEditOleCallback): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_SETOLECALLBACK, 0, longint(OleInterface)));
end;

function RichEdit_GetOleInterface(RichEdit: HWnd; out OleInterface: IRichEditOle): BOOL;
begin
  Result:= BOOL(SendMessage(RichEdit, EM_GETOLEINTERFACE, 0, longint(@OleInterface)));
end;



end.

