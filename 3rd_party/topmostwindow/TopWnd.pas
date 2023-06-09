
{**********************************************************
  Component »TopMostWindow«

  Drop this component on a form and it's possible, to make
  a top-level- or a not-top-level-window.

  Copyright © 1997 MSD & Consulting. All rights reserved.
  Contact by EMail: sstephan@donau.de
**********************************************************}

unit TopWnd;

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs;

type

{ TTopMostWindow class }

  TTopMostWindow = class(TComponent)
  private
    FHWindow: HWnd;
    FAlwaysOnTop: Boolean;
    FOnBeforeChange: TNotifyEvent;
    FOnAfterChange: TNotifyEvent;
    procedure SetAlwaysOnTop(Value: Boolean);
  protected
    procedure DoOnBeforeChange;
    procedure DoOnAfterChange;
    procedure ChangeState; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HWindow: HWnd read FHWindow write FHWindow;
  published
  { properties }
    property AlwaysOnTop: Boolean read FAlwaysOnTop write SetAlwaysOnTop;
  { events }
    property OnBeforeChange: TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property OnAfterChange: TNotifyEvent read FOnAfterChange write FOnAfterChange;
  end;

{ Register procedure }

procedure Register;

implementation

{ TTopMostWindow }

constructor TTopMostWindow.Create(AOwner: TComponent);
begin
  inherited;
  FHWindow := (AOwner as TForm).Handle;
  FAlwaysOnTop := False;
end;

destructor TTopMostWindow.Destroy;
begin
  inherited;
end;

procedure TTopMostWindow.ChangeState;
var
  x, y, cx, cy: Integer;
  Flags: UINT;
begin
  x := (Owner as TForm).Left;
  y := (Owner as TForm).Top;
  cx := (Owner as TForm).Width;
  cy := (Owner as TForm).Height;
  Flags := SWP_DRAWFRAME or SWP_NOMOVE or SWP_NOSIZE;
  if FAlwaysOnTop then SetWindowPos(FHWindow, HWND_TOPMOST, x, y, cx, cy, Flags)
  else SetWindowPos(FHWindow, HWND_NOTOPMOST, x, y, cx, cy, Flags);
{ Note: The flag SWP_NOMOVE ignores the x- und y-parameter and
        the flag SWP_NOSIZE ignores the cx- und cy-parameter. In this case they
        are absolutly obsolet.
        The second parameter of SetWindowPos 'hwndInsertAfter' is also ignored
        if HWND_TOPMOST or HWND_NOTOPMOST is set. }
end;

procedure TTopMostWindow.DoOnAfterChange;
begin
  if Assigned(FOnAfterChange) then FOnAfterChange(Self);
end;

procedure TTopMostWindow.DoOnBeforeChange;
begin
  if Assigned(FOnBeforeChange) then FOnBeforeChange(Self);
end;

procedure TTopMostWindow.SetAlwaysOnTop(Value: Boolean);
begin
  if FAlwaysOnTop <> Value then
  begin
    if not (csDesigning in ComponentState) then DoOnBeforeChange;
    FAlwaysOnTop := Value;
    if not (csDesigning in ComponentState) then ChangeState;
    if not (csDesigning in ComponentState) then DoOnAfterChange;
  end;
end;

{ Register }

procedure Register;
begin
  RegisterComponents('MSD', [TTopMostWindow]);
end;

end.
