unit knt.ui.selector;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls
  ;


type
  TSelector = class(TForm)
  private
    FOwnerHandle: HWND;
    FThreadAttached: Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowSelector(X, Y: Integer; OwnerHandle: HWND);
    procedure HideSelector;
    destructor Destroy; override;
  end;



implementation

uses gf_miscvcl;


constructor TSelector.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 0);

  BorderStyle := bsSingle;
  BorderWidth := 1;
  FormStyle := fsStayOnTop;

  DoubleBuffered := True;
  Color := clWhite;
  Ctl3D := False;
  Visible := False;
  FThreadAttached := False;
end;


procedure TSelector.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP or WS_VISIBLE or WS_BORDER;
  Params.Style := Params.Style and not (WS_SYSMENU or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);
  Params.ExStyle := WS_EX_NOACTIVATE or WS_EX_TOPMOST or WS_EX_TOOLWINDOW;
  Params.WndParent := GetDesktopWindow();
end;


procedure TSelector.WMActivate(var Message: TWMActivate);
begin
  if Message.Active <> WA_INACTIVE then begin
    // If this window is attempted to be activated, return focus to the main window
    SetForegroundWindow(FOwnerHandle);
    PostMessage(FOwnerHandle, WM_NCACTIVATE, 1, 0);
  end;

  // Do not call inherited to avoid default behavior
  Message.Result := 0;
end;

procedure TSelector.WMMouseActivate(var Message: TWMMouseActivate);
begin
  // Prevent activation on click
  Message.Result := MA_NOACTIVATE;
end;

procedure TSelector.WMNCHitTest(var Message: TWMNCHitTest);
begin
  // This message is crucial for handling clicks without activating the window
  inherited;
  if Message.Result = HTCLIENT then
     Message.Result := HTTRANSPARENT;
end;

procedure TSelector.ShowSelector(X, Y: Integer; OwnerHandle: HWND);
var
  OwnerThreadID, MyThreadID: DWORD;
begin
  FOwnerHandle := OwnerHandle;
  Left := X;
  Top := Y;

  // Set as secondary window without activation
  SetWindowLong(Handle, GWL_HWNDPARENT, FOwnerHandle);

  MyThreadID := GetCurrentThreadId();
  OwnerThreadID := GetWindowThreadProcessId(FOwnerHandle, nil);

  if MyThreadID <> OwnerThreadID then begin
    AttachThreadInput(MyThreadID, OwnerThreadID, True);
    FThreadAttached := True;
  end;

  Show;
  EnableShadow(Self.Handle);
  
  SetForegroundWindow(FOwnerHandle);                    // Ensure the main window has focus
  PostMessage(FOwnerHandle, WM_NCACTIVATE, 1, 0);
end;

procedure TSelector.HideSelector;
var
  MyThreadID, OwnerThreadID: DWORD;
begin
  // Disconnect threads if they are connected
  if FThreadAttached then begin
    MyThreadID := GetCurrentThreadId();
    OwnerThreadID := GetWindowThreadProcessId(FOwnerHandle, nil);

    if MyThreadID <> OwnerThreadID then begin
      AttachThreadInput(MyThreadID, OwnerThreadID, False);
      FThreadAttached := False;
    end;
  end;

  Hide();
  SetForegroundWindow(FOwnerHandle);       // Ensure the main window has focus
end;

destructor TSelector.Destroy;
begin
  // Ensure threads are disconnected
  if FThreadAttached then
    HideSelector;

  inherited;
end;

end.