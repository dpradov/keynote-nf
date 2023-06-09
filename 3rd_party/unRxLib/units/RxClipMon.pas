{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit RxClipMon;

interface

{$I RX.INC}
{$IFDEF RX_D6}
{$WARN SYMBOL_PLATFORM OFF} // Polaris
{$ENDIF}

uses
  Messages, {$IFNDEF VER80}Windows, {$ELSE}WinTypes, WinProcs, {$ENDIF}
  SysUtils, Classes;

type
  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TClipboardMonitor = class(TComponent)
  private
    FWindowHandle: HWnd;
    FNextWindow: HWnd;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure ForwardMessage(var Msg: TMessage);
    procedure SetEnabled(Value: Boolean);
    procedure WndProc(var AMsg: TMessage);
    procedure ClipboardChanged;
  protected
    procedure Change; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure SaveClipboardToStream(Format: Word; Stream: TStream);
procedure LoadClipboardFromStream(Format: Word; Stream: TStream; Size: LongInt);

implementation

uses Forms, Clipbrd;

{ Stream routines }

procedure SaveClipboardToStream(Format: Word; Stream: TStream);
var
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then Exit;
    Buffer := GlobalLock(Data);
    try
      Stream.Write(Buffer^, GlobalSize(Data));
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure LoadClipboardFromStream(Format: Word; Stream: TStream; Size: LongInt);
var
  Len: LongInt;
  Buffer: Pointer;
  Data: THandle;
begin
  Clipboard.Open;
  try
    Len := Stream.Size - Stream.Position;
    if Len > Size then Len := Size;
    Data := GlobalAlloc(HeapAllocFlags, Len);
    try
      if Data <> 0 then
      begin
        Buffer := GlobalLock(Data);
        try
          Stream.Read(Buffer^, Len);
          SetClipboardData(Format, Data);
        finally
          GlobalUnlock(Data);
        end;
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

{ TClipboardMonitor }

constructor TClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF RX_D6}
  FWindowHandle := Classes.AllocateHWnd(WndProc);
  {$ELSE}
  FWindowHandle := AllocateHWnd(WndProc);
  {$ENDIF}
  SetEnabled(True);
end;

destructor TClipboardMonitor.Destroy;
begin
  FOnChange := nil;
  SetEnabled(False);
  {$IFDEF RX_D6}
  Classes.DeallocateHWnd(FWindowHandle);
  {$ELSE}
  DeallocateHWnd(FWindowHandle);
  {$ENDIF}
  inherited Destroy;
end;

procedure TClipboardMonitor.ForwardMessage(var Msg: TMessage);
begin
  if FNextWindow <> 0 then
    with Msg do
      SendMessage(FNextWindow, Msg, WParam, LParam);
end;

procedure TClipboardMonitor.WndProc(var AMsg: TMessage);
begin
  with AMsg do
  begin
    Result := 0;
    case Msg of
      WM_DESTROYCLIPBOARD:
        ClipboardChanged;
      WM_CHANGECBCHAIN:
        if HWnd(WParam) = FNextWindow then
          FNextWindow := HWnd(LParam)
        else
          ForwardMessage(AMsg);
      WM_DRAWCLIPBOARD:
        begin
          ForwardMessage(AMsg);
          ClipboardChanged;
        end;
      WM_DESTROY:
        SetEnabled(False);
    else
      Result := DefWindowProc(FWindowHandle, Msg, WParam, LParam);
    end;
  end;
end;

procedure TClipboardMonitor.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    if Value then
    begin
      FNextWindow := SetClipboardViewer(FWindowHandle);
      FEnabled := True;
    end
    else
    begin
      ChangeClipboardChain(FWindowHandle, FNextWindow);
      FEnabled := False;
      FNextWindow := 0;
    end;
  end;
end;

procedure TClipboardMonitor.ClipboardChanged;
begin
  try
    Change;
  except
    Application.HandleException(Self);
  end;
end;

procedure TClipboardMonitor.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

end.
