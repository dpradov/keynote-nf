unit TB97Cmn;

{
  Toolbar97
  Copyright (C) 1998-2001 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  Internal common functions

  $Id: TB97Cmn.pas,v 1.2 2001/01/04 04:17:14 jr Exp $
}

interface

{$I TB97Ver.inc}

uses
  Windows, Classes, Messages, Controls;

type
  THookProcCode = (hpSendActivateApp, hpSendWindowPosChanged, hpPreDestroy,
    hpPostMouseMove);
  THookProcCodes = set of THookProcCode;
  THookProc = procedure(Code: THookProcCode; Wnd: HWND; WParam: WPARAM; LParam: LPARAM);
  TListSortExCompare = function(const Item1, Item2, ExtraData: Pointer): Integer;
  THandleWMPrintNCPaintProc = procedure(Wnd: HWND; DC: HDC; AppData: Longint);
  TGetToolbarDockPosType = (gtpTop, gtpBottom, gtpLeft, gtpRight, gtpNone);

var
  GetToolbarDockPosProc: function(Ctl: TControl): TGetToolbarDockPosType = nil;
  TB97CALLCOUNT : longint;


function ApplicationIsActive: Boolean;
procedure InstallHookProc (AProc: THookProc; ACodes: THookProcCodes;
  OnlyIncrementCount: Boolean);
procedure UninstallHookProc (AProc: THookProc);
procedure ListSortEx (const List: TList; const Compare: TListSortExCompare;
  const ExtraData: Pointer);
procedure SelectNCUpdateRgn (Wnd: HWND; DC: HDC; Rgn: HRGN);
procedure HandleWMPrint (const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: Longint);
procedure HandleWMPrintClient (const Control: TWinControl;
  var Message: TMessage);

{$IFNDEF TB97D3}
type
  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[Byte] of TPaletteEntry;
  end;
function CopyPalette (Palette: HPALETTE): HPALETTE;
{$ENDIF}

implementation

uses
  Forms;

type
  PHookProcData = ^THookProcData;
  THookProcData = record
    Proc: THookProc;
    RefCount: Longint;
    Codes: THookProcCodes;
  end;
  THookType = (htCallWndProc, htCBT, htGetMessage);
  THookTypes = set of THookType;

var
  HookHandles: array[THookType] of HHOOK;
  HookProcList: TList = nil;
  HookCounts: array[THookType] of Longint;


function CallWndProcHook (Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
type
  THookProcCodeMsgs = hpSendActivateApp..hpSendWindowPosChanged;
const
  MsgMap: array[THookProcCodeMsgs] of UINT =
    (WM_ACTIVATEAPP, WM_WINDOWPOSCHANGED);
var
  J: THookProcCodeMsgs;
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) then
    with PCWPStruct(LParam)^ do begin
      for J := Low(J) to High(J) do
        if Message = MsgMap[J] then begin
          for I := 0 to HookProcList.Count-1 do
            try
              with PHookProcData(HookProcList.List[I])^ do
                if J in Codes then
                  Proc (J, hwnd, WParam, LParam);
            except
            end;
          Break;
        end;
    end;
  Result := CallNextHookEx(HookHandles[htCallWndProc], Code, WParam, LParam);
  { // MJ
  inc( TB97CALLCOUNT );
  if ( TB97CALLCOUNT > 225 ) and ( TB97CALLCOUNT < 500 ) then
  begin
    TB97CALLCOUNT := 0;
  end;
  }
end;

function CBTHook (Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HCBT_DESTROYWND) then
    for I := 0 to HookProcList.Count-1 do
      try
        with PHookProcData(HookProcList.List[I])^ do
          if hpPreDestroy in Codes then
            Proc (hpPreDestroy, HWND(WParam), 0, 0);
      except
      end;
  Result := CallNextHookEx(HookHandles[htCBT], Code, WParam, LParam);
end;

function GetMessageHook (Code: Integer; WParam: WPARAM; LParam: LPARAM): LRESULT;
stdcall;
var
  I: Integer;
begin
  if Assigned(HookProcList) and (Code = HC_ACTION) and
     (PMsg(LParam).message = WM_MOUSEMOVE) then
    for I := 0 to HookProcList.Count-1 do
      try
        with PHookProcData(HookProcList.List[I])^, PMsg(LParam)^ do
          if hpPostMouseMove in Codes then
            Proc (hpPostMouseMove, hwnd, wParam, lParam);
      except
      end;
  Result := CallNextHookEx(HookHandles[htGetMessage], Code, WParam, LParam);
end;

function HookCodesToTypes (Codes: THookProcCodes): THookTypes;
const
  HookCodeToType: array[THookProcCode] of THookType =
    (htCallWndProc, htCallWndProc, htCBT, htGetMessage);
var
  J: THookProcCode;
begin
  Result := [];
  for J := Low(J) to High(J) do
    if J in Codes then
      Include (Result, HookCodeToType[J]);
end;

const
  HookProcs: array[THookType] of TFNHookProc =
    (CallWndProcHook, CBTHook, GetMessageHook);
  HookIDs: array[THookType] of Integer =
    (WH_CALLWNDPROC, WH_CBT, WH_GETMESSAGE);

procedure InstallHooks (ATypes: THookTypes);
var
  T: THookType;
begin
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      Inc (HookCounts[T]);
      if HookHandles[T] = 0 then
        HookHandles[T] := SetWindowsHookEx(HookIDs[T], HookProcs[T],
          0, GetCurrentThreadId);
    end;
end;

procedure UninstallHooks (const ATypes: THookTypes; const Force: Boolean);
var
  T: THookType;
begin
  for T := Low(T) to High(T) do
    if T in ATypes then begin
      if HookCounts[T] > 0 then
        Dec (HookCounts[T]);
      if (Force or (HookCounts[T] = 0)) and (HookHandles[T] <> 0) then begin
        UnhookWindowsHookEx (HookHandles[T]);
        HookHandles[T] := 0;
      end;
    end;
end;

procedure InstallHookProc (AProc: THookProc; ACodes: THookProcCodes;
  OnlyIncrementCount: Boolean);
var
  Found: Boolean;
  I: Integer;
  Data: PHookProcData;
begin
  if HookProcList = nil then
    HookProcList := TList.Create;
  Found := False;
  for I := 0 to HookProcList.Count-1 do
    with PHookProcData(HookProcList[I])^ do
      if @Proc = @AProc then begin
        Inc (RefCount);
        Found := True;
        Break;
      end;
  if not Found then begin
    New (Data);
    with Data^ do begin
      Proc := AProc;
      RefCount := 1;
      Codes := ACodes;
    end;
    HookProcList.Add (Data);
  end;
  if not OnlyIncrementCount then
    InstallHooks (HookCodesToTypes(ACodes));
end;

procedure UninstallHookProc (AProc: THookProc);
var
  I: Integer;
  Data: PHookProcData;
  T: THookTypes;
begin
  if HookProcList = nil then Exit;
  for I := 0 to HookProcList.Count-1 do begin
    Data := PHookProcData(HookProcList[I]);
    if @Data.Proc = @AProc then begin
      T := HookCodesToTypes(Data.Codes);
      Dec (Data.RefCount);
      if Data.RefCount = 0 then begin
        HookProcList.Delete (I);
        Dispose (Data);
      end;
      UninstallHooks (T, False);
      Break;
    end;
  end;
  if HookProcList.Count = 0 then begin
    HookProcList.Free;
    HookProcList := nil;
  end;
end;

function ApplicationIsActive: Boolean;
{ Returns True if the application is in the foreground }
begin
  Result := GetActiveWindow <> 0;
end;

{$IFNDEF TB97D3}
function CopyPalette (Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries (Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;
{$ENDIF}

procedure ListSortEx (const List: TList; const Compare: TListSortExCompare;
  const ExtraData: Pointer);
{ Similar to TList.Sort, but lets you pass a user-defined ExtraData pointer }
  procedure QuickSortEx (L: Integer; const R: Integer);
  var
    I, J: Integer;
    P: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := List[(L + R) shr 1];
      repeat
        while Compare(List[I], P, ExtraData) < 0 do Inc(I);
        while Compare(List[J], P, ExtraData) > 0 do Dec(J);
        if I <= J then
        begin
          List.Exchange (I, J);
          Inc (I);
          Dec (J);
        end;
      until I > J;
      if L < J then QuickSortEx (L, J);
      L := I;
    until I >= R;
  end;
begin
  if List.Count > 1 then
    QuickSortEx (0, List.Count-1);
end;

procedure SelectNCUpdateRgn (Wnd: HWND; DC: HDC; Rgn: HRGN);
var
  R: TRect;
  NewClipRgn: HRGN;
begin
  if (Rgn <> 0) and (Rgn <> 1) then begin
    GetWindowRect (Wnd, R);
    if SelectClipRgn(DC, Rgn) = ERROR then begin
      NewClipRgn := CreateRectRgnIndirect(R);
      SelectClipRgn (DC, NewClipRgn);
      DeleteObject (NewClipRgn);
    end;
    OffsetClipRgn (DC, -R.Left, -R.Top);
  end;
end;

type
  PPrintEnumProcData = ^TPrintEnumProcData;
  TPrintEnumProcData = record
    PrintChildren: Boolean;
    ParentWnd: HWND;
    DC: HDC;
    PrintFlags: LPARAM;
  end;

function PrintEnumProc (Wnd: HWND; LParam: LPARAM): BOOL; stdcall;
var
  R: TRect;
  SaveIndex: Integer;
begin
  Result := True;  { continue enumerating }
  with PPrintEnumProcData(LParam)^ do begin
    { Skip window if it isn't a child/owned window of ParentWnd or isn't visible }
    if (HWND(GetWindowLong(Wnd, GWL_HWNDPARENT)) <> ParentWnd) or
       (GetWindowLong(Wnd, GWL_STYLE) and WS_VISIBLE = 0) then
         { ^ don't use IsWindowVisible since it returns False if the window's
           parent window is not visible }
      Exit;
    GetWindowRect (Wnd, R);
    MapWindowPoints (0, ParentWnd, R, 2);
    SaveIndex := SaveDC(DC);
    { Like Windows, offset the window origin to the top-left coordinates of
      the child/owned window }
    MoveWindowOrg (DC, R.Left, R.Top);
    { Like Windows, intersect the clipping region with the entire rectangle of
      the child/owned window }
    OffsetRect (R, -R.Left, -R.Top);
    IntersectClipRect (DC, R.Left, R.Top, R.Right, R.Bottom);
    { Send a WM_PRINT message to the child/owned window }
    SendMessage (Wnd, WM_PRINT, WPARAM(DC), PrintFlags);
    { Restore the DC's state, in case the WM_PRINT handler didn't put things
      back the way it found them }
    RestoreDC (DC, SaveIndex);
  end;
end;

procedure HandleWMPrint (const Wnd: HWND; var Message: TMessage;
  const NCPaintFunc: THandleWMPrintNCPaintProc; const AppData: Longint);
{ note: AppData is an application-defined value which is passed to NCPaintFunc }
var
  DC: HDC;
  SaveIndex, SaveIndex2: Integer;
  R: TRect;
  P: TPoint;
  Data: TPrintEnumProcData;
begin
  if (Message.LParam and PRF_CHECKVISIBLE = 0) or IsWindowVisible(Wnd) then begin
    DC := HDC(Message.WParam);
    SaveIndex2 := SaveDC(DC);
    try
      if Message.LParam and PRF_NONCLIENT <> 0 then begin
        SaveIndex := SaveDC(DC);
        if Assigned(NCPaintFunc) then
          NCPaintFunc (Wnd, DC, AppData);
        RestoreDC (DC, SaveIndex);
      end;
      { Calculate the difference between the top-left corner of the window
        and the top-left corner of its client area }
      GetWindowRect (Wnd, R);
      P.X := 0;  P.Y := 0;
      ClientToScreen (Wnd, P);
      Dec (P.X, R.Left);  Dec (P.Y, R.Top);
      if Message.LParam and PRF_CLIENT <> 0 then begin
        { Like Windows, the flags PRF_ERASEBKGND, PRF_CHILDREN, and PRF_OWNED
          are ignored if PRF_CLIENT isn't also specified }
        if Message.LParam and PRF_ERASEBKGND <> 0 then begin
          { Send WM_ERASEBKGND }
          SaveIndex := SaveDC(DC);
          if Message.LParam and PRF_NONCLIENT <> 0 then
            MoveWindowOrg (DC, P.X, P.Y);
          SendMessage (Wnd, WM_ERASEBKGND, Message.WParam, 0);
          RestoreDC (DC, SaveIndex);
        end;
        { Send WM_PRINTCLIENT }
        SaveIndex := SaveDC(DC);
        if Message.LParam and PRF_NONCLIENT <> 0 then
          MoveWindowOrg (DC, P.X, P.Y);
        SendMessage (Wnd, WM_PRINTCLIENT, Message.WParam, 0);
        RestoreDC (DC, SaveIndex);
        { Like Windows, always offset child/owned windows by the size of the
          client area even if PRF_NONCLIENT isn't specified (a bug?) }
        MoveWindowOrg (DC, P.X, P.Y);
        Data.ParentWnd := Wnd;
        Data.DC := DC;
        { Send WM_PRINT to child/owned windows }
        if Message.LParam and PRF_CHILDREN <> 0 then begin
          Data.PrintChildren := True;
          Data.PrintFlags := PRF_NONCLIENT or PRF_CLIENT or PRF_ERASEBKGND or
            PRF_CHILDREN;  { same flags as Windows passes to children }
          EnumChildWindows (Wnd, @PrintEnumProc, LPARAM(@Data));
        end;
        if Message.LParam and PRF_OWNED <> 0 then begin
          Data.PrintChildren := False;
          Data.PrintFlags := Message.LParam;
          EnumWindows (@PrintEnumProc, LPARAM(@Data));
        end;
      end;
    finally
      RestoreDC (DC, SaveIndex2);
    end;
  end;
  { Windows' WM_PRINT returns 1. I'm not sure why. }
  Message.Result := 1;
end;

type
  TWinControlAccess = class(TWinControl);

procedure HandleWMPrintClient (const Control: TWinControl; var Message: TMessage);
var
  Msg: TWMPaint;
  SaveIndex: Integer;
begin
  Msg.Msg := WM_PAINT;
  Msg.DC := HDC(Message.WParam);
  Msg.Unused := 0;
  Msg.Result := 0;
  SaveIndex := SaveDC(HDC(Message.WParam));
  try
    TWinControlAccess(Control).PaintHandler (Msg);
  finally
    RestoreDC (HDC(Message.WParam), SaveIndex);
  end;
end;


initialization
  // TB97CALLCOUNT := 500;
  
finalization
  UninstallHooks ([Low(THookType)..High(THookType)], True);
  HookProcList.Free;
  { Following line needed because, under certain circumstances, HookProcList
    may be referenced after the 'finalization' section is processed. (This
    can happen if a 'Halt' call is placed in the main form's OnCreate
    handler, for example.) }
  HookProcList := nil;

  inc( TB97CALLCOUNT );

end.
