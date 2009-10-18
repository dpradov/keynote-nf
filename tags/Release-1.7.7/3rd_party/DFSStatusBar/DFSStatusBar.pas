{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ TdfsStatusBar v1.23                                                          }
{------------------------------------------------------------------------------}
{ A status bar that provides many common specialized panels and owning of      }
{ other components by the status bar.                                          }
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
{ example, if you create a descendant of TdfsColorButton, you must include in  }
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
{ See DFSStatusBar.txt for notes, known issues, and revision history.          }
{------------------------------------------------------------------------------}
{ Date last modified:  June 22, 2000                                           }
{------------------------------------------------------------------------------}


unit dfsStatusBar;

interface

uses
  {$IFDEF DFS_DEBUG}
  DFSDebug,
  {$ENDIF}
  {$IFDEF DFS_DELPHI_6_UP} // *1
  RTLConsts,
  {$ENDIF}
  Windows, Classes, Messages, Controls, ComCtrls, Graphics, Forms,
  ExtCtrls;

const
  WM_REFRESHLOCKINDICATORS = WM_APP + 230;
  { This shuts up C++Builder 3 about the redefiniton being different. There
    seems to be no equivalent in C1.  Sorry. }
  {$IFDEF DFS_CPPB_3_UP}
  {$EXTERNALSYM DFS_COMPONENT_VERSION}
  {$ENDIF}
  DFS_COMPONENT_VERSION = 'TdfsStatusBar v1.23';

type
  TdfsStatusPanelType = (
     sptNormal,            // Nothing special, same as a regular TStatusPanel
     sptCapsLock,          // Caps lock indicator.  Normal color if on, gray if
                           //   off
     sptNumLock,           // Num lock indicator.  Normal color if on, gray if
                           //   off
     sptScrollLock,        // Scroll lock indicator.  Normal color if on, gray
                           //   if off
     sptDate,              // Current date.  Uses DateFormat property for format
     sptTime,              // Current time.  Uses TimeFormat property for format
     sptDateTime,          // Current date and time.  Uses DateFormat and
                           //   TimeFormat properties for format
     sptTimeDate,          // Current time and date.  Uses DateFormat and
                           //   TimeFormat properties for format
     sptEllipsisText,      // Shorten text at the end with '...' when won't fit.
     sptEllipsisPath,      // Shorten by removing path info with '...' when
                           //   won't fit.
     sptGlyph,             // Displays a TPicture object in the panel.
     sptGauge,             // A progress meter.  Use GaugeAttrs to customize it.
     sptOwnerDraw          // Same as the old TStatusPanel.Style = psOwnerDraw.
    );

  TPercent = 0..100;

  TdfsGaugeStyle = (
     gsPercent,        // Your basic progress meeter.
     gsIndeterminate,  // A progress indicator where the min/max are not
                       //   known.  That is, you want to show something
                       //   going on, but don't know how long it will take.
                       //   It's a little ball that "bounces" back and forth.
     gsIndeterminate2  // Same as above, but looks more Netscape-ish.
    );
  TdfsGaugeStyles = set of TdfsGaugeStyle;

  TdfsStatusBar = class; // forward declaration
  TdfsStatusPanel = class; // forward declaration


  TdfsDrawPanelEvent = procedure(StatusBar: TdfsStatusBar;
     Panel: TdfsStatusPanel; const Rect: TRect) of object;
  TdfsPanelHintTextEvent = procedure (StatusBar: TdfsStatusBar;
     Panel: TdfsStatusPanel; var Hint: wideString) of object;


  TdfsGaugeAttrs = class(TPersistent)
  private
    FStyle: TdfsGaugeStyle;
    FOwner: TdfsStatusPanel;
    FPosition: TPercent;
    FSpeed: integer;
    FColor: TColor;
    FTextColor: TColor;
    procedure SetPosition(const Value: TPercent);
    procedure SetStyle(const Value: TdfsGaugeStyle);
    procedure SetSpeed(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  public
    constructor Create(AOwner: TdfsStatusPanel);
    procedure Assign(Source: TPersistent); override;

    property Owner: TdfsStatusPanel
       read FOwner;
  published
    property Style: TdfsGaugeStyle
       read FStyle
       write SetStyle
       default gsPercent;
    property Position: TPercent
       read FPosition
       write SetPosition
       default 0;
    property Speed: integer
       read FSpeed
       write SetSpeed
       default 4;
    property Color: TColor
       read FColor
       write SetColor
       default clHighlight;
    property TextColor: TColor
       read FTextColor
       write SetTextColor
       default clHighlightText;
  end;

  TdfsStatusPanel = class(TCollectionItem)
  private
    FKeyOn: boolean;
    FPanelType: TdfsStatusPanelType;
    FAutoFit: boolean;
    FEnabled: boolean;
    FTimeFormat: string;
    FDateFormat: string;
    FText: wideString;
    FGlyph: TPicture;
    FGaugeLastPos: integer;
    FGaugeDirection: integer;
    FOnDrawPanel: TdfsDrawPanelEvent;
    FHint: wideString;
    FOnHintText: TdfsPanelHintTextEvent;
    FOnClick: TNotifyEvent;
    FGaugeAttrs: TdfsGaugeAttrs;
    FGaugeBitmap: TBitmap;
    FBorderWidth: TBorderWidth;

    procedure SetPanelType(const Val: TdfsStatusPanelType);
    function GetAlignment: TAlignment;
    function GetBevel: TStatusPanelBevel;
{$IFDEF DFS_COMPILER_4_UP}
    function IsBiDiModeStored: Boolean;
    function GetBiDiMode: TBiDiMode;
    function GetParentBiDiMode: Boolean;
{$ENDIF}
    function GetWidth: Integer;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetBevel(const Value: TStatusPanelBevel);
{$IFDEF DFS_COMPILER_4_UP}
    procedure SetBiDiMode(const Value: TBiDiMode);
    procedure SetParentBiDiMode(const Value: Boolean);
{$ENDIF}
    procedure SetText(const Value: wideString);
    procedure SetWidth(const Value: Integer);
    procedure SetAutoFit(const Value: boolean);
    procedure SetDateFormat(const Value: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetGlyph(const Value: TPicture);
    procedure SetTimeFormat(const Value: string);
    function GetStatusBar: TdfsStatusBar;
    function GetEnabled: boolean;
    function GetHint: wideString;
    procedure SetGaugeAttrs(const Value: TdfsGaugeAttrs);
    function GetLinkedPanel: TStatusPanel;
    function GetGaugeBitmap: TBitmap;
    procedure SetBorderWidth(const Value: TBorderWidth);
    function IsTextStored: Boolean;
  protected
    procedure SetIndex(Value: integer); override;
    function GetDisplayName: string; override;
    procedure TimerNotification;
    procedure UpdateAutoFitWidth; dynamic;
    procedure UpdateDateTime; dynamic;
    procedure GlyphChanged(Sender: TObject); dynamic;
    procedure DrawPanel(Rect: TRect); dynamic;
    procedure EnabledChanged; dynamic;
    procedure DoHintText(var HintText: wideString); dynamic;
    procedure Redraw(Canvas: TCanvas; Dest: TRect); dynamic;
    procedure DrawKeyLock(Canvas: TCanvas; R: TRect); dynamic;
    procedure DrawTextBased(Canvas: TCanvas; R: TRect); dynamic;
    procedure DrawGlyph(Canvas: TCanvas; R: TRect); dynamic;
    procedure DrawGauge(Canvas: TCanvas; R: TRect); dynamic;
    procedure DrawIndeterminateGauge(Canvas: TCanvas; R: TRect); dynamic;
    function InitGaugeBitmap: TBitmap; dynamic;
    procedure Click; dynamic;
    procedure UpdateKeyboardHook;
    property LinkedPanel: TStatusPanel
       read GetLinkedPanel;
    property GaugeBitmap: TBitmap
       read GetGaugeBitmap;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Invalidate;

    property StatusBar: TdfsStatusBar
       read GetStatusBar;
  published
    property GaugeAttrs: TdfsGaugeAttrs
       read FGaugeAttrs
       write SetGaugeAttrs;
    property Alignment: TAlignment
       read GetAlignment
       write SetAlignment
       default taLeftJustify;
    property Bevel: TStatusPanelBevel
       read GetBevel
       write SetBevel
       default pbLowered;
    property BorderWidth: TBorderWidth
      read FBorderWidth
      write SetBorderWidth
      default 0;
{$IFDEF DFS_COMPILER_4_UP}
    property BiDiMode: TBiDiMode
       read GetBiDiMode
       write SetBiDiMode
       stored IsBiDiModeStored;
    property ParentBiDiMode: Boolean
       read GetParentBiDiMode
       write SetParentBiDiMode
       default True;
{$ENDIF}
    // PanelType must come before most of the other properties because it would
    //   stomp on some of their values as they are streamed.  Some of the other
    //   properties have to be ordered a certain way, too, so don't mess with
    //   the declaration order.
    property PanelType: TdfsStatusPanelType
       read FPanelType
       write SetPanelType
       default sptNormal;
    property Glyph: TPicture
       read FGlyph
       write SetGlyph;
    property Text: wideString
       read FText
       write SetText
       stored IsTextStored;
    property DateFormat: string
       read FDateFormat
       write SetDateFormat;
    property TimeFormat: string
       read FTimeFormat
       write SetTimeFormat;
    property Enabled: boolean
       read GetEnabled
       write SetEnabled;
    property Width: Integer
       read GetWidth
       write SetWidth;
    property AutoFit: boolean
       read FAutoFit
       write SetAutoFit;
    property Hint: wideString
       read GetHint
       write FHint;

    property OnDrawPanel: TdfsDrawPanelEvent
       read FOnDrawPanel
       write FOnDrawPanel;
    property OnHintText: TdfsPanelHintTextEvent
       read FOnHintText
       write FOnHintText;
    property OnClick: TNotifyEvent
       read FOnClick
       write FOnClick;
  end;

  TdfsStatusPanels = class(TCollection)
  private
    FTimer: TTimer;
    FTimerClients: TList;
    FLastDate: TDateTime;
    FStatusBar: TdfsStatusBar;
    FLinkedPanels: TStatusPanels;
    function GetItem(Index: Integer): TdfsStatusPanel;
    procedure SetItem(Index: Integer; Value: TdfsStatusPanel);
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    procedure RegisterTimer(Client: TdfsStatusPanel);
    procedure DeregisterTimer(Client: TdfsStatusPanel);
    procedure TimerEvent(Sender: TObject);
  public
    constructor Create(StatusBar: TdfsStatusBar; LinkedPanels: TStatusPanels);
    destructor Destroy; override;
    function Add: TdfsStatusPanel;
    property Items[Index: Integer]: TdfsStatusPanel
       read GetItem
       write SetItem;
       default;
  end;

  TdfsStatusBar = class(TStatusBar)
  private
    FPanels: TdfsStatusPanels;
    FMainWinHookClients: TList;
    FExtentCanvas: HDC;
    FExtentFont: HFONT;
    FExtentFontOld: HFONT;
    FUseMonitorDLL: boolean;
    FDLLClientCount: integer;
    FKeyHookMsg: UINT;
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
    procedure SetPanels(const Value: TdfsStatusPanels);
    function AppWinHook(var Message: TMessage): boolean;
    procedure WMRefreshLockIndicators(var Msg: TMessage);
       message WM_REFRESHLOCKINDICATORS;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetOnDrawPanel(const Value: TdfsDrawPanelEvent);
    function GetOnDrawPanel: TdfsDrawPanelEvent;
    function GetVersion: string;
    procedure SetVersion(const Val: string);
    procedure UpdateExtentFont;
    procedure SetUseMonitorDLL(const Value: boolean);
    procedure UpdateKeyboardHooks;
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  protected
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect); override;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure WndProc(var Msg: TMessage); override;
    function GetPanelRect(Index: integer): TRect;
    function FindLinkedPanel(Panel: TStatusPanel): TdfsStatusPanel;
    procedure RegisterMainWinHook(Client: TdfsStatusPanel);
    procedure DeregisterMainWinHook(Client: TdfsStatusPanel);
    procedure RegisterSystemHook;
    procedure DeregisterSystemHook;
    function TextExtent(const Text: wideString): TSize;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidatePanel(Index: integer);
  published
    property UseMonitorDLL: boolean
       read FUseMonitorDLL
       write SetUseMonitorDLL
       default FALSE;
    property Panels: TdfsStatusPanels
       read FPanels
       write SetPanels;
    property Version: string
       read GetVersion
       write SetVersion
       stored FALSE;

    property OnDrawPanel: TdfsDrawPanelEvent
       read GetOnDrawPanel
       write SetOnDrawPanel;

    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;


// You may want to change this value if you don't like the speed of the
// indeterminate gauge
const
  INDETERMINATE_GAUGE_UPDATE_INTERVAL: integer = 50; // in milliseconds

{$IFDEF DFS_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SCapsLock   = ' CAPS ';
  SNumLock    = ' NUM ';
  SScrollLock = ' SCROLL ';

const
  IndeterminateGuages: TdfsGaugeStyles = [gsIndeterminate, gsIndeterminate2];

implementation

uses
  Consts, CommCtrl, TypInfo, SysUtils, DFSKb,
  TntStdCtrls, TntControls, TntSysUtils;

const
  KEY_CODE: array[sptCapsLock..sptScrollLock] of integer = (
     VK_CAPITAL, VK_NUMLOCK, VK_SCROLL
    );

var
  KeyboardHookHandle: HHOOK;
  KeyHookClients: TList;
  RegisteredTimers: integer;
  MayNeedRefresh: boolean;

// Keyboard hook callback
function KeyboardHookCallBack(Code: integer; KeyCode: WPARAM;
   KeyInfo: LPARAM): LRESULT; stdcall;
var
  x: integer;
begin
  if Code >= 0 then
  begin
    if MayNeedRefresh then
    begin
      for x := 0 to KeyHookClients.Count-1 do
        TdfsStatusPanel(KeyHookClients[x]).Invalidate;
      MayNeedRefresh := FALSE;
    end else
    // Is it one of the indicator keys, and is it not a repeat
    if ((KeyCode = VK_CAPITAL) or (KeyCode = VK_NUMLOCK) or
       (KeyCode = VK_SCROLL)) and
       // This checks to see if the key is being pressed (bit 31) and if it was
       // up before (bit 30).  We don't care about key releases or keys that
       // were already down.  That just makes us flicker...
       (((KeyInfo SHR 31) and 1) = 0) and (((KeyInfo SHR 30) and 1) = 0) then
    begin
      for x := 0 to KeyHookClients.Count-1 do
      begin
        case TdfsStatusPanel(KeyHookClients[x]).PanelType of
          sptCapsLock:
            begin
              if KeyCode = VK_CAPITAL then
                TdfsStatusPanel(KeyHookClients[x]).Invalidate;
            end;
          sptNumLock:
            begin
              if KeyCode = VK_NUMLOCK then
                TdfsStatusPanel(KeyHookClients[x]).Invalidate;
            end;
          sptScrollLock:
            begin
              if KeyCode = VK_SCROLL then
                TdfsStatusPanel(KeyHookClients[x]).Invalidate;
            end;
        end;
      end;
    end;
  end;
  Result := CallNextHookEx(KeyboardHookHandle, Code, KeyCode, KeyInfo);
end;

// Utility routins for installing the windows hook for keypresses
procedure RegisterTaskKeyboardHook(Client: TdfsStatusPanel);
begin
  if KeyboardHookHandle = 0 then
    KeyboardHookHandle := SetWindowsHookEx(WH_KEYBOARD, KeyboardHookCallBack,
       0, GetCurrentThreadID);

  KeyHookClients.Add(Client);
end;

procedure DeregisterTaskKeyboardHook(Client: TdfsStatusPanel);
begin
  KeyHookClients.Remove(Client);
  if KeyHookClients.Count < 1 then
  begin
    UnhookWindowsHookEx(KeyboardHookHandle);
    KeyboardHookHandle := 0;
  end;
end;

// Utility function for making a copy of a font handle
function CopyHFont(Font: HFONT): HFONT;
var
  LF: TLogFont;
begin
  if Font <> 0 then
  begin
    GetObject(Font, SizeOf(LF), @LF);
    Result := CreateFontIndirect(LF);
  end else
    Result := 0;
end;


{ TdfsGaugeAttrs }

procedure TdfsGaugeAttrs.Assign(Source: TPersistent);
var
  SrcAttrs: TdfsGaugeAttrs absolute Source;
begin
  if Source is TdfsGaugeAttrs then
  begin
    FOwner := SrcAttrs.Owner;
    Position := SrcAttrs.Position;
    Style := SrcAttrs.Style;
  end else
    inherited Assign(Source);
end;

constructor TdfsGaugeAttrs.Create(AOwner: TdfsStatusPanel);
begin
  inherited Create;
  FOwner := AOwner;
  FStyle := gsPercent;
  FPosition := 0;
  FSpeed := 4;
  FColor := clHighlight;
  FTextColor := clHighlightText;
end;

procedure TdfsGaugeAttrs.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FOwner.FGaugeBitmap.Free;
    FOwner.FGaugeBitmap := NIL;
    FOwner.Invalidate;
  end;
end;

procedure TdfsGaugeAttrs.SetPosition(const Value: TPercent);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    FOwner.Invalidate;
  end;
end;

procedure TdfsGaugeAttrs.SetSpeed(const Value: integer);
begin
  if (FSpeed <> Value) and (FSpeed > 0) then
    FSpeed := Value;

  if Owner.FGaugeDirection < 0 then
    Owner.FGaugeDirection := -FSpeed
  else
    Owner.FGaugeDirection := FSpeed;
end;

procedure TdfsGaugeAttrs.SetStyle(const Value: TdfsGaugeStyle);
begin
  if FStyle <> Value then
  begin
    if (Owner.PanelType = sptGauge) and (FStyle in IndeterminateGuages) and
       Owner.Enabled then
      TdfsStatusPanels(Owner.Collection).DeregisterTimer(Owner);
    FStyle := Value;
    FOwner.Invalidate;
    if (Owner.PanelType = sptGauge) and (FStyle in IndeterminateGuages) and
       Owner.Enabled then
      TdfsStatusPanels(Owner.Collection).RegisterTimer(Owner);
  end;
end;


procedure TdfsGaugeAttrs.SetTextColor(const Value: TColor);
begin
  if Value <> FTextColor then
  begin
    FTextColor := Value;
    Owner.Invalidate;
  end;
end;

{ TdfsStatusPanel }

procedure TdfsStatusPanel.Assign(Source: TPersistent);
var
  SrcPanel: TdfsStatusPanel absolute Source;
begin
  if Source is TdfsStatusPanel then
  begin
{    if LinkedPanel <> NIL then
      LinkedPanel.Free;
    LinkedPanel := SrcPanel.FLinkedPanel;}

    GaugeAttrs.Assign(SrcPanel.GaugeAttrs);
    Alignment := SrcPanel.Alignment;
    Bevel := SrcPanel.Bevel;
{$IFDEF DFS_COMPILER_4_UP}
    BiDiMode := SrcPanel.BiDiMode;
    ParentBiDiMode := SrcPanel.ParentBiDiMode;
{$ENDIF}
    Glyph.Assign(SrcPanel.Glyph);
    Text := SrcPanel.Text;
    DateFormat := SrcPanel.DateFormat;
    TimeFormat := SrcPanel.TimeFormat;
    Enabled := SrcPanel.Enabled;
    Width := SrcPanel.Width;
    AutoFit := SrcPanel.AutoFit;
    Hint := SrcPanel.Hint;

    OnDrawPanel := SrcPanel.OnDrawPanel;
    OnHintText := SrcPanel.OnHintText;

    // Do last!
    PanelType := SrcPanel.PanelType;
  end else
    inherited Assign(Source);
end;

constructor TdfsStatusPanel.Create(AOwner: TCollection);
begin
  inherited Create(AOwner);

  if AOwner is TdfsStatusPanels then
  begin
    TdfsStatusPanels(AOwner).FLinkedPanels.Add;
    LinkedPanel.Style := psOwnerDraw;
  end else
    raise Exception.Create('TdfsStatusPanel owner must be TdfsStatusPanesls');
  FKeyOn := FALSE;
  FGaugeLastPos := 0;
  FGaugeDirection := 1;
  FPanelType := sptNormal;
  FAutoFit := FALSE;
  FEnabled := TRUE;
  FTimeFormat := '';
  FDateFormat := '';
  FGaugeAttrs := TdfsGaugeAttrs.Create(Self);
  FGlyph := TPicture.Create;
  FGlyph.OnChange := GlyphChanged;
end;


destructor TdfsStatusPanel.Destroy;
begin
  if Enabled then
    case FPanelType of
      sptCapsLock, sptNumLock, sptScrollLock:
        begin
          if StatusBar.UseMonitorDLL then
            StatusBar.DeregisterSystemHook
          else begin
            DeregisterTaskKeyboardHook(Self);
            StatusBar.DeregisterMainWinHook(Self);
          end;
        end;
      sptDate, sptTime, sptDateTime, sptTimeDate:
        TdfsStatusPanels(Collection).DeregisterTimer(Self);
      sptGauge:
        if GaugeAttrs.Style in IndeterminateGuages then
          TdfsStatusPanels(Collection).DeregisterTimer(Self);
    end;

  FGlyph.Free;
  FGaugeAttrs.Free;
  FGaugeBitmap.Free;
  TdfsStatusPanels(Collection).FLinkedPanels[Index].Free;

  inherited Destroy;
end;


function TdfsStatusPanel.GetAlignment: TAlignment;
begin
  Result := LinkedPanel.Alignment
end;

function TdfsStatusPanel.GetBevel: TStatusPanelBevel;
begin
  Result := LinkedPanel.Bevel
end;

{$IFDEF DFS_COMPILER_4_UP}
function TdfsStatusPanel.GetBiDiMode: TBiDiMode;
begin
  Result := LinkedPanel.BiDiMode
end;

function TdfsStatusPanel.GetParentBiDiMode: Boolean;
begin
  Result := LinkedPanel.ParentBiDiMode
end;
{$ENDIF}

function TdfsStatusPanel.GetStatusBar: TdfsStatusBar;
begin
  Result := TdfsStatusPanels(Collection).FStatusBar;
end;

function TdfsStatusPanel.GetWidth: Integer;
begin
  Result := LinkedPanel.Width
end;

procedure TdfsStatusPanel.Invalidate;
begin
  if StatusBar <> NIL then
    StatusBar.InvalidatePanel(Index);
end;

{$IFDEF DFS_COMPILER_4_UP}
function TdfsStatusPanel.IsBiDiModeStored: Boolean;
begin
  Result := not ParentBiDiMode;
end;
{$ENDIF}

procedure TdfsStatusPanel.Redraw(Canvas: TCanvas; Dest: TRect);
var
  Buffer: TBitmap;
  R: TRect;
begin
  if (not StatusBar.HandleAllocated) or (IsRectEmpty(Dest))then
    exit;

  InflateRect(Dest, -1, -1); // Don't paint over the shadows.

  R := Dest;
  OffsetRect(R, -Dest.Left, -Dest.Top);
  Buffer := TBitmap.Create;
  try
    Buffer.Width := R.Right;
    Buffer.Height := R.Bottom;

    Buffer.Canvas.Font.Handle := CopyHFont(Canvas.Font.Handle);
    Buffer.Canvas.Brush.Color := StatusBar.Color;
    Buffer.Canvas.FillRect(R);

    if BorderWidth > 0 then
      InflateRect(R, -BorderWidth, -BorderWidth);

    if Enabled then
    begin
      case PanelType of
        sptCapsLock, sptNumLock, sptScrollLock:
          DrawKeyLock(Buffer.Canvas, R);

        sptNormal, sptDate, sptTime, sptDateTime, sptTimeDate, sptEllipsisText,
        sptEllipsisPath, sptOwnerDraw:
          begin
            if (PanelType = sptOwnerDraw) and
               not (csDesigning in StatusBar.ComponentState) then
              exit;
            DrawTextBased(Buffer.Canvas, R);
          end;

          sptGlyph:
            DrawGlyph(Buffer.Canvas, R);

          sptGauge:
            if GaugeAttrs.Style in IndeterminateGuages then
              DrawIndeterminateGauge(Buffer.Canvas, R)
            else
              DrawGauge(Buffer.Canvas, R);
      end;
    end;

    Canvas.Draw(Dest.Left, Dest.Top, Buffer);
  finally
    Buffer.Free;
  end;
end;

procedure TdfsStatusPanel.DrawGauge(Canvas: TCanvas; R: TRect);
var
  R1, R2: TRect;
  R1Rgn, R2Rgn, OldRgn: HRGN;
  Pct: string;
  OldColor: TColorRef;
  DTFlags: UINT;
begin
  R1 := R;
  R2 := R;
  R1.Right := R1.Left + MulDiv(R.Right-R.Left, FGaugeAttrs.Position, 100);
  with Canvas do
  begin
    Brush.Color := GaugeAttrs.Color;
    FillRect(R1);
    R2.Left := R1.Right;
    Brush.Color := StatusBar.Color;
    FillRect(R2);

    { This could probably be simplified with ExtTextOut and SetTextAlign now
      things are being properly buffered off-screen.  But, this is working and
      doesn't seem slow, so....  "If it ain't broke, don't fix it."  :)        }
    if Text = '' then
      Pct := IntToStr(FGaugeAttrs.Position) + '%'
    else
      Pct := Text; // Use what's in the panel's text property.
    // don't change background color behind text!
    Brush.Style := bsClear;
    OldColor := GetTextColor(Handle);

    R1Rgn := CreateRectRgnIndirect(R1);
    R2Rgn := CreateRectRgnIndirect(R2);
    OldRgn := CreateRectRgn(0, 0, 1, 1);
    try
      GetClipRgn(Handle, OldRgn);

      DTFlags := DT_VCENTER or DT_NOPREFIX or DT_SINGLELINE;
      case Alignment of
        taCenter:       DTFlags := DTFlags or DT_CENTER;
        taRightJustify: DTFlags := DTFlags or DT_RIGHT;
      end;
      // Draw the text in the "filled" area with text color
      if (R1Rgn<>0) and (SelectClipRgn(Handle, R1Rgn) <> ERROR) then
        try
          SetTextColor(Handle, ColorToRGB(GaugeAttrs.TextColor));
          DrawText(Handle, PChar(Pct), -1, R, DTFlags);
        finally
          SelectClipRgn(Handle, OldRgn);
        end;

      // Draw the text in the "empty" area with normal color
      if (R2Rgn<>0) and (SelectClipRgn(Handle, R2Rgn) <> ERROR) then
        try
//          SetTextColor(Handle, OldColor);
          SetTextColor(Handle, ColorToRGB(StatusBar.Font.Color));
          DrawText(Handle, PChar(Pct), -1, R, DTFlags);
        finally
          SelectClipRgn(Handle, OldRgn);
        end;
    finally
      SetTextColor(Handle, OldColor);
      DeleteObject(R1Rgn);
      DeleteObject(R2Rgn);
      DeleteObject(OldRgn);
    end;
  end;
end;

procedure TdfsStatusPanel.DrawGlyph(Canvas: TCanvas; R: TRect);
const
  TEXT_SPACE = 2;
var
  TW: integer;
  GR: TRect;
begin
  GR := R;
  if Text <> '' then
    TW := Canvas.TextWidth(Text) + TEXT_SPACE
  else
    TW := 0;
  if (Alignment = taCenter) or AutoFit then
     with GR do
       Left := Left + ((Right - Left - Glyph.Width - TW) div 2)
  else if Alignment = taRightJustify then
    GR.Left := GR.Right - Glyph.Width;

  GR.Top := GR.Top + (GR.Bottom - GR.Top - Glyph.Height) div 2;

  if Glyph.Graphic is TBitmap then
  begin
    // Draw it transparently
    Canvas.BrushCopy(Bounds(GR.Left, GR.Top, Glyph.Width,
       Glyph.Height), Glyph.Bitmap, Rect(0, 0, Glyph.Width,
       Glyph.Height), Glyph.Bitmap.Canvas.Pixels[0, Glyph.Height-1]);
  end else
    Canvas.Draw(GR.Left, GR.Top, Glyph.Graphic);
  if Text <> '' then
  begin
    SetTextColor(Canvas.Handle, ColorToRGB(StatusBar.Font.Color));
    case Alignment of
      taLeftJustify,
      taCenter:
        begin
          GR.Left := GR.Left + Glyph.Width + TEXT_SPACE;
          GR.Top := R.Top;
          GR.Bottom := R.Bottom;
          DrawTextW(Canvas.Handle, PWideChar(Text), -1, GR, DT_SINGLELINE or
            DT_NOPREFIX or DT_VCENTER);
        end;
      taRightJustify:
        begin
          GR.Left := GR.Left - TW - TEXT_SPACE;
          GR.Top := R.Top;
          GR.Bottom := R.Bottom;
          DrawTextW(Canvas.Handle, PWideChar(Text), -1, GR, DT_SINGLELINE or
            DT_NOPREFIX or DT_VCENTER);
        end;
    end;
  end;
end;

function TdfsStatusPanel.InitGaugeBitmap: TBitmap;
var
	r1, b1, g1, r2, b2, g2: byte;
	c1, c2: Longint;
  i: integer;
	divi: integer;
	mul: extended;
begin
	c1 := ColorToRGB(StatusBar.Color);
	c2 := ColorToRGB(GaugeAttrs.Color);
	r1 := GetRValue(c1);
  b1 := GetBValue(c1);
  g1 := GetGValue(c1);
	r2 := GetRValue(c2);
  b2 := GetBValue(c2);
  g2 := GetGValue(c2);
	Result := TBitmap.Create;
	with Result do
	begin
		Height := StatusBar.Height;
		Width := 100;
		divi := Width-1;
		Canvas.Brush.Color := clRed;
		Canvas.FillRect(Rect(0, 0, Width, Height));
		for i := 0 to divi do
		begin
			mul := (i/divi);
			Canvas.Pen.Color := RGB(trunc(r1 + (r2 - r1) * mul),
         trunc(g1 + (g2 - g1) *mul), trunc(b1 + (b2 - b1) * mul));
			Canvas.MoveTo(i, 0);
			Canvas.LineTo(i, Height);
		end;
	end;
end;

procedure TdfsStatusPanel.DrawIndeterminateGauge(Canvas: TCanvas; R: TRect);
var
	gb:TBitmap;
	gbr:TRect;
  x: integer;
begin
  inc(FGaugeLastPos, FGaugeDirection);
  case GaugeAttrs.Style of
    gsIndeterminate:
      begin
        with Canvas do
        begin
          Brush.Color := GaugeAttrs.Color;
          Pen.Color := GaugeAttrs.Color;
          gbr := R;
          InflateRect(R, 0, -((R.Bottom - R.Top) div 3));
          x := R.Bottom - R.Top;
          if (FGaugeDirection > 0) and ((FGaugeLastPos + X + 1) >=
             (R.Right - R.Left)) then
          begin
            FGaugeDirection := -GaugeAttrs.Speed;
          end else if (FGaugeDirection < 0) and (FGaugeLastPos <= 1) then
          begin
            FGaugeDirection := GaugeAttrs.Speed;
          end;
          Inc(R.Left, FGaugeLastPos);
          R.Right := R.Left + X;
          // Make it a wee bit bigger
          InflateRect(R, 1, 1);

          with R do
            Ellipse(Left, Top, Right, Bottom);
        end;
      end;

    gsIndeterminate2:
      begin
        with Canvas do
        begin
          gb := GaugeBitmap;
          if (FGaugeDirection > 0) and
             ((FGaugeLastPos+ 1) >= (R.Right - R.Left)) then
            FGaugeDirection := -FGaugeAttrs.Speed
          else if (FGaugeDirection < 0) and (FGaugeLastPos <= -gb.Width) then
            FGaugeDirection := FGaugeAttrs.Speed;
          Inc(R.Left, FGaugeLastPos);
          gbr := Rect(0, 0, gb.Width, gb.Height);
          if (r.right - r.left) > gb.width then
            r.right := r.left + gb.Width
          else
            if (r.right - r.left) < gb.width then
            begin
              if FGaugeDirection > 0 then
                gbr.Right := r.right - r.Left
              else
                gbr.Left := gbr.right - (r.right - r.left);
            end;

          if FGaugeDirection > 0 then
            CopyRect(r, gb.Canvas, gbr)
          else
            CopyRect(r, gb.Canvas,
               Rect(gbr.right-1, gbr.Bottom-1, gbr.left-1, gbr.top-1))
        end;
      end;
  end;
end;

procedure TdfsStatusPanel.DrawKeyLock(Canvas: TCanvas; R: TRect);
var
  DTFlags: UINT;
  OldColor: TColorRef;
begin
  OldColor := GetTextColor(Canvas.Handle);
  if StatusBar.UseMonitorDLL then
  begin
    if not FKeyOn then
      SetTextColor(Canvas.Handle, ColorToRGB(clGrayText)) // might need to be a property
    else
      SetTextColor(Canvas.Handle, ColorToRGB(StatusBar.Font.Color));
  end else begin
    if not Odd(GetKeyState(KEY_CODE[FPanelType])) then
      SetTextColor(Canvas.Handle, ColorToRGB(clGrayText)) // might need to be a property
    else
      SetTextColor(Canvas.Handle, ColorToRGB(StatusBar.Font.Color));
  end;
  DTFlags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER;
  if AutoFit then
    DTFLags := DTFlags or DT_CENTER
  else
    case Alignment of
      taCenter:       DTFlags := DTFlags or DT_CENTER;
      taRightJustify: DTFlags := DTFlags or DT_RIGHT;
    end;
  DrawTextW(Canvas.Handle, PWideChar(Text), -1, R, DTFlags);
  SetTextColor(Canvas.Handle, OldColor);
end;

procedure TdfsStatusPanel.DrawTextBased(Canvas: TCanvas; R: TRect);
var
  DTFlags: UINT;
begin
  DTFlags := DT_SINGLELINE or DT_NOPREFIX or DT_VCENTER;
  if AutoFit then
    DTFLags := DTFlags or DT_CENTER
  else
    case Alignment of
      taCenter:       DTFlags := DTFlags or DT_CENTER;
      taRightJustify:
        begin
          dec(R.Right);
          DTFlags := DTFlags or DT_RIGHT;
        end;
    end;
  case PanelType of
    sptEllipsisPath: DTFlags := DTFlags or DT_PATH_ELLIPSIS;
    sptEllipsisText: DTFlags := DTFlags or DT_END_ELLIPSIS;
  end;
  SetTextColor(Canvas.Handle, ColorToRGB(StatusBar.Font.Color));
  if PanelType = sptOwnerDraw then
    // This only happens when in design mode, see Redraw method.
    DrawText(Canvas.Handle, ' *OD* ', -1, R, DTFlags)
  else
    DrawTextW(Canvas.Handle, PWideChar(Text), -1, R, DTFlags);
end;

procedure TdfsStatusPanel.SetAlignment(const Value: TAlignment);
begin
  if LinkedPanel.Alignment <> Value then
  begin
    LinkedPanel.Alignment := Value;
    Invalidate;
  end;
end;

procedure TdfsStatusPanel.SetAutoFit(const Value: boolean);
begin
  if FAutoFit <> Value then
  begin
    FAutoFit := Value;
    UpdateAutoFitWidth;
  end;
end;

procedure TdfsStatusPanel.SetBevel(const Value: TStatusPanelBevel);
begin
  if LinkedPanel.Bevel <> Value then
    LinkedPanel.Bevel := Value;
end;

{$IFDEF DFS_COMPILER_4_UP}
procedure TdfsStatusPanel.SetBiDiMode(const Value: TBiDiMode);
begin
  if LinkedPanel.BiDiMode <> Value then
    LinkedPanel.BiDiMode := Value;
end;

procedure TdfsStatusPanel.SetParentBiDiMode(const Value: Boolean);
begin
  if LinkedPanel.ParentBiDiMode <> Value then
    LinkedPanel.ParentBiDiMode := Value;
end;

{$ENDIF}

procedure TdfsStatusPanel.SetDateFormat(const Value: string);
begin
  if FDateFormat <> Value then
  begin
    FDateFormat := Value;
    UpdateDateTime;
  end;
end;

procedure TdfsStatusPanel.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    EnabledChanged;
  end;
end;

procedure TdfsStatusPanel.SetGlyph(const Value: TPicture);
begin
  FGlyph.Assign(Value);
  // GlyphChanged method will take care of updating display.
end;

procedure TdfsStatusPanel.SetPanelType(const Val: TdfsStatusPanelType);
const
  LOCK_TEXT: array[sptCapsLock..sptScrollLock] of string = (
     SCapsLock, SNumLock, SScrollLock
    );
begin
  if Val <> FPanelType then
  begin
    if Enabled then
      case FPanelType of
        sptCapsLock, sptNumLock, sptScrollLock:
          begin
            if StatusBar.UseMonitorDLL then
              StatusBar.DeregisterSystemHook
            else begin
              DeregisterTaskKeyboardHook(Self);
              StatusBar.DeregisterMainWinHook(Self);
            end;
          end;
        sptDate, sptTime, sptDateTime, sptTimeDate:
          TdfsStatusPanels(Collection).DeregisterTimer(Self);
        sptGauge:
          if GaugeAttrs.Style in IndeterminateGuages then
            TdfsStatusPanels(Collection).DeregisterTimer(Self);
      end;

    FPanelType := Val;
    case FPanelType of
      sptCapsLock, sptNumLock, sptScrollLock:
        begin
          Text := LOCK_TEXT[FPanelType];
          AutoFit := TRUE;
          if Enabled then
          begin
            if StatusBar.UseMonitorDLL then
            begin
              StatusBar.RegisterSystemHook;
              FKeyOn := Odd(GetKeyState(KEY_CODE[FPanelType]));
            end else begin
              RegisterTaskKeyboardHook(Self);
              StatusBar.RegisterMainWinHook(Self);
            end;
          end;
        end;
      sptDate, sptTime, sptDateTime, sptTimeDate:
        begin
          AutoFit := FALSE;
          if Enabled then
            TdfsStatusPanels(Collection).RegisterTimer(Self);
          UpdateDateTime;
        end;
      sptEllipsisText, sptEllipsisPath:
        begin
          AutoFit := FALSE;
          if Hint = '' then
            Hint := '...'; 
        end;
      sptGlyph:
        begin
          AutoFit := TRUE;
        end;
      sptGauge:
        begin
          AutoFit := FALSE;
          Alignment := taCenter;
          if GaugeAttrs.Style in IndeterminateGuages then
          begin
            Enabled := FALSE; // Enabled is false, so don't need to register
            FGaugeLastPos := 0;
            FGaugeDirection := GaugeAttrs.Speed;
          end;
        end;
    else
      AutoFit := FALSE;
    end;
    
    Invalidate;
  end;
end;


procedure TdfsStatusPanel.SetText(const Value: wideString);
begin
  if FText <> Value then
  begin
    FText := Value;
    Invalidate;
    UpdateAutoFitWidth;
  end;
end;

procedure TdfsStatusPanel.SetTimeFormat(const Value: string);
begin
  if FTimeFormat <> Value then
  begin
    FTimeFormat := Value;
    UpdateDateTime;
  end;
end;

procedure TdfsStatusPanel.SetWidth(const Value: Integer);
begin
  if ((not FAutoFit) or (csLoading in StatusBar.ComponentState)) and
     (LinkedPanel.Width <> Value) then
    LinkedPanel.Width := Value;
  if (PanelType = sptGauge) and (GaugeAttrs.Style in IndeterminateGuages) then
  begin
    FGaugeLastPos := 0;
    FGaugeDirection := GaugeAttrs.Speed;
    Invalidate;
  end;
end;

procedure TdfsStatusPanel.TimerNotification;
begin
  if PanelType in [sptDate, sptTime, sptDateTime, sptTimeDate] then
    UpdateDateTime
  else if (PanelType = sptGauge) and (GaugeAttrs.Style in IndeterminateGuages) then
    // Call Redraw directly. It will take care of erasing the old part.  If we
    // used Invalidate, the background would get erased, too, and it would
    // flicker a lot.
    Redraw(StatusBar.Canvas, StatusBar.GetPanelRect(Index));
end;

procedure TdfsStatusPanel.UpdateAutoFitWidth;
begin
  if FAutoFit and (StatusBar <> NIL) and (StatusBar.HandleAllocated) then
  begin
    if PanelType = sptGlyph then
    begin
      if Text = '' then
        LinkedPanel.Width := BorderWidth + Glyph.Width + 4
      else
        LinkedPanel.Width := StatusBar.TextExtent(Text).cx + 2 +
          (BorderWidth * 2) + Glyph.Width + 4;
    end
    else
      LinkedPanel.Width := StatusBar.TextExtent(Text).cx + 6 + BorderWidth;
  end;
  Invalidate;
end;

procedure TdfsStatusPanel.UpdateDateTime;
var
  Fmt: string;
  Txt: string;
begin
  case PanelType of
    sptDate:
      if DateFormat = '' then
        Fmt := ShortDateFormat
      else
        Fmt := DateFormat;
    sptTime:
      if TimeFormat = '' then
        Fmt := LongTimeFormat
      else
        Fmt := TimeFormat;
    sptDateTime:
      begin
        if DateFormat = '' then
          Fmt := ShortDateFormat
        else
          Fmt := DateFormat;
        if TimeFormat = '' then
          Fmt := Fmt + ' ' + LongTimeFormat
        else
          Fmt := Fmt + ' ' + TimeFormat;
      end;
    sptTimeDate:
      begin
        if TimeFormat = '' then
          Fmt := LongTimeFormat
        else
          Fmt := TimeFormat;
        if DateFormat = '' then
          Fmt := Fmt + ' ' + ShortDateFormat
        else
          Fmt := Fmt + ' ' + DateFormat;
      end;
  end;
  Txt := FormatDateTime(Fmt, Now);
  if Txt <> Text then
  begin
    Text := Txt;
    //    Invalidate(TRUE);
    Redraw(Statusbar.Canvas, StatusBar.GetPanelRect(Index));
  end;
end;

procedure TdfsStatusPanel.GlyphChanged(Sender: TObject);
begin
  if PanelType = sptGlyph then
  begin
    Invalidate;
    UpdateAutoFitWidth;
  end;
end;


procedure TdfsStatusPanel.DrawPanel(Rect: TRect);
begin
  if (csDesigning in StatusBar.ComponentState) or (Addr(OnDrawPanel) = NIL) or
     (PanelType <> sptOwnerDraw) then
    Redraw(StatusBar.Canvas, StatusBar.GetPanelRect(Index))
  else if assigned(FOnDrawPanel) then
    FOnDrawPanel(StatusBar, Self, Rect);
end;


function TdfsStatusPanel.GetEnabled: boolean;
begin
  if csWriting in StatusBar.ComponentState then
    Result := FEnabled
  else
    Result := FEnabled and StatusBar.Enabled;
end;

procedure TdfsStatusPanel.EnabledChanged;
begin
  // Enabled property (self or parent) changed, update register/deregister calls
  if Enabled then
  begin
    case FPanelType of
      sptCapsLock, sptNumLock, sptScrollLock:
        begin
          if StatusBar.UseMonitorDLL then
          begin
            StatusBar.RegisterSystemHook;
            FKeyOn := Odd(GetKeyState(KEY_CODE[FPanelType]));
          end else begin
            RegisterTaskKeyboardHook(Self);
            StatusBar.RegisterMainWinHook(Self);
          end;
        end;
      sptDate, sptTime, sptDateTime, sptTimeDate:
        TdfsStatusPanels(Collection).RegisterTimer(Self);
      sptGauge:
        if GaugeAttrs.Style in IndeterminateGuages then
          TdfsStatusPanels(Collection).RegisterTimer(Self);
    end;
  end else begin
    case FPanelType of
      sptCapsLock, sptNumLock, sptScrollLock:
        begin
          if StatusBar.UseMonitorDLL then
            StatusBar.DeregisterSystemHook
          else begin
            DeregisterTaskKeyboardHook(Self);
            StatusBar.DeregisterMainWinHook(Self);
          end;
        end;
      sptDate, sptTime, sptDateTime, sptTimeDate:
        TdfsStatusPanels(Collection).DeregisterTimer(Self);
      sptGauge:
        if GaugeAttrs.Style in IndeterminateGuages then
          TdfsStatusPanels(Collection).DeregisterTimer(Self);
    end;
  end;

  Invalidate;
  if not Enabled then
  begin
    FGaugeLastPos := 0;
    FGaugeDirection := GaugeAttrs.Speed;
  end;
end;


function TdfsStatusPanel.GetHint: wideString;
begin
  if (not (csDesigning in StatusBar.ComponentState)) and
     (PanelType in [sptEllipsisText, sptEllipsisPath]) and (FHint = '...') then
    Result := Text
  else
    Result := FHint;
  DoHintText(Result);
end;

procedure TdfsStatusPanel.DoHintText(var HintText: wideString);
begin
  if assigned(FOnHintText) then
    FOnHintText(StatusBar, Self, HintText);
end;

procedure TdfsStatusPanel.SetGaugeAttrs(const Value: TdfsGaugeAttrs);
begin
  FGaugeAttrs := Value;
end;

function TdfsStatusPanel.GetDisplayName: string;
begin
  case PanelType of
    sptNormal, sptEllipsisText, sptEllipsisPath:
      Result := Text;
  else
    Result := GetEnumName(TypeInfo(TdfsStatusPanelType), ord(PanelType));
  end;

  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TdfsStatusPanel.SetIndex(Value: integer);
var
  CurIndex: Integer;
begin
  CurIndex := Index;
  if (CurIndex >= 0) and (CurIndex <> Value) then
  begin
    TdfsStatusPanels(Collection).FLinkedPanels[CurIndex].Index := Value;
    inherited SetIndex(Value);
  end;
end;

function TdfsStatusPanel.GetLinkedPanel: TStatusPanel;
begin
  Result := TdfsStatusPanels(Collection).FLinkedPanels[Index];
end;


procedure TdfsStatusPanel.UpdateKeyboardHook;
begin
  if PanelType in [sptCapsLock, sptNumLock, sptScrollLock] then
  begin
    if StatusBar.UseMonitorDLL and Enabled then
    begin
      DeregisterTaskKeyboardHook(Self);
      StatusBar.DeregisterMainWinHook(Self);
      StatusBar.RegisterSystemHook;
      FKeyOn := Odd(GetKeyState(KEY_CODE[FPanelType]));
    end else if (not StatusBar.UseMonitorDLL) and Enabled then
    begin
      StatusBar.DeregisterSystemHook;
      RegisterTaskKeyboardHook(Self);
      StatusBar.RegisterMainWinHook(Self);
    end;
  end;
end;

procedure TdfsStatusPanel.Click;
begin
  if assigned(FOnClick) then
    FOnClick(Self);
end;

function TdfsStatusPanel.GetGaugeBitmap: TBitmap;
begin
  if FGaugeBitmap = NIL then
    FGaugeBitmap := InitGaugeBitmap;
  Result := FGaugeBitmap;
end;

procedure TdfsStatusPanel.SetBorderWidth(const Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    UpdateAutoFitWidth;
    Invalidate;
  end;
end;

function TdfsStatusPanel.IsTextStored: Boolean;
begin
  Result := not (PanelType in [sptDate, sptTime, sptDateTime, sptTimeDate]);
end;

{ TdfsStatusPanels }

function TdfsStatusPanels.Add: TdfsStatusPanel;
begin
  Result := TdfsStatusPanel(inherited Add);
end;

constructor TdfsStatusPanels.Create(StatusBar: TdfsStatusBar;
   LinkedPanels: TStatusPanels);
begin
  FStatusBar := StatusBar;
  FLinkedPanels := LinkedPanels;
  FTimer := NIL;
  FTimerClients := TList.Create;

  inherited Create(TdfsStatusPanel);
end;

procedure TdfsStatusPanels.DeregisterTimer(Client: TdfsStatusPanel);
var
  x: integer;
  NewTimerRes: integer;
begin
  if FTimerClients.Remove(Client) <> -1 then
    dec(RegisteredTimers);
  if FTimerClients.Count < 1 then
  begin
    FTimer.Free;
    FTimer := NIL;
  end else begin
    NewTimerRes := 60000; // Least impact we can manage easily
    for x := 0 to FTimerClients.Count-1 do
      case TdfsStatusPanel(FTimerClients[x]).PanelType of
        sptTime, sptDateTime, sptTimeDate:
          NewTimerRes := 1000;
        sptGauge:
          if TdfsStatusPanel(FTimerClients[x]).GaugeAttrs.Style in
             IndeterminateGuages then begin
            NewTimerRes := INDETERMINATE_GAUGE_UPDATE_INTERVAL;
            break;
          end;
      end;

    FTimer.Interval := NewTimerRes;
  end;
end;

destructor TdfsStatusPanels.Destroy;
begin
  // Call inherited first because it causes children to be destroyed, and that
  // might cause FTimerClients to be needed.
  inherited Destroy;

  FTimer.Free;
  FTimer := NIL;
  FTimerClients.Free;
  FTimerClients := NIL; // Yes, there is a reason for this!
end;

function TdfsStatusPanels.GetItem(Index: Integer): TdfsStatusPanel;
begin
  Result := TdfsStatusPanel(inherited GetItem(Index));
end;

function TdfsStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure TdfsStatusPanels.RegisterTimer(Client: TdfsStatusPanel);
var
  FirstClient: boolean;
begin
  if FTimer = NIL then
  begin
    FTimer := TTimer.Create(FStatusBar);
    FLastDate := Date;
    FTimer.OnTimer := TimerEvent;
  end;
  if FTimerClients.IndexOf(Client) >= 0 then
    exit;  // We're already in the list!
    
  FTimerClients.Add(Client);
  inc(RegisteredTimers);
  FirstClient := FTimerClients.Count = 1;
  case Client.PanelType of
    sptDate:
      if FirstClient then
        FTimer.Interval := 60000; // Least impact we can manage easily
    sptTime, sptDateTime, sptTimeDate:
      if FirstClient or (FTimer.Interval > 1000) then
        FTimer.Interval := 1000;
    sptGauge:
      if Client.GaugeAttrs.Style in IndeterminateGuages then
        FTimer.Interval := INDETERMINATE_GAUGE_UPDATE_INTERVAL;
  end;
  FTimer.Enabled := TRUE;
end;

procedure TdfsStatusPanels.SetItem(Index: Integer; Value: TdfsStatusPanel);
begin
  // I have no idea if this will work or not....
  inherited SetItem(Index, Value);
  FLinkedPanels[Index] := Value.LinkedPanel;
end;

procedure TdfsStatusPanels.TimerEvent(Sender: TObject);
var
  x: integer;
  DateUpdate: boolean;
  Panel: TdfsStatusPanel;
begin
  if FLastDate <> Date then
  begin
    DateUpdate := TRUE;
    FLastDate := Date;
  end else
    DateUpdate := FALSE;

  for x := 0 to FTimerClients.Count-1 do
  begin
    Panel := TdfsStatusPanel(FTimerClients[x]); // shorthand
    if (Panel.PanelType in [sptTime, sptDateTime, sptTimeDate]) or
       (DateUpdate and (Panel.PanelType = sptDate)) or
       ((Panel.PanelType = sptGauge) and
       (Panel.GaugeAttrs.Style in IndeterminateGuages)) then
      TdfsStatusPanel(FTimerClients[x]).TimerNotification;
  end;
end;

procedure TdfsStatusPanels.Update(Item: TCollectionItem);
begin
  if Item is TdfsStatusPanel then
    TdfsStatusPanel(Item).Invalidate
  else
    FStatusBar.Invalidate;
end;

{ TdfsStatusBar }

constructor TdfsStatusBar.Create(AOwner: TComponent);
begin
  FExtentCanvas := CreateCompatibleDC(0);
  FExtentFont := 0;
  FExtentFontOld := 0;
  FUseMonitorDLL := FALSE;
  FDLLClientCount := 0;
  FMainWinHookClients := TList.Create;

  inherited Create(AOwner);

  // Allow it to accept controls dropped onto it.
  ControlStyle:= ControlStyle + [csAcceptsControls];

  FPanels := TdfsStatusPanels.Create(Self, inherited Panels);
end;

procedure TdfsStatusBar.InvalidatePanel(Index: integer);
var
  PanelRect: TRect;
begin
  if (Index >= 0) and (Index < Panels.Count) then
  begin
    PanelRect := GetPanelRect(Index);
    if not IsRectEmpty(PanelRect) then
      Panels[Index].Redraw(Canvas, PanelRect)
  end else begin
    {$IFDEF DFS_COMPILER_3_UP}
    raise EListError.Create(SListIndexError);
    {$ELSE}
    raise EListError.CreateRes(SListIndexError);
    {$ENDIF}
  end;
end;

function TdfsStatusBar.GetPanelRect(Index: integer): TRect;
begin
  SetRectEmpty(Result);
  if HandleAllocated then
    if Perform(SB_GETRECT, Index, LPARAM(@Result)) = 0 then
      SetRectEmpty(Result); // SB_GETRECT failed, probably not visible
end;

procedure TdfsStatusBar.SetPanels(const Value: TdfsStatusPanels);
begin
  FPanels.Assign(Value);
// what about linked panels????
end;

destructor TdfsStatusBar.Destroy;
begin
  FPanels.Free;
  SelectObject(FExtentCanvas, FExtentFontOld);
  if FExtentFont <> 0 then
  begin
    DeleteObject(FExtentFont);
    FExtentFont := 0;
  end;
  if FExtentCanvas <> 0 then
  begin
    DeleteDC(FExtentCanvas);
    FExtentCanvas := 0;
  end;

  Assert(FMainWinHookClients.Count = 0, 'Unbalanced MainWinHook registrations');

  inherited Destroy;
  FMainWinHookClients.Free;
end;


procedure TdfsStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
var
  DFSPanel: TdfsStatusPanel;
  OldFont: HFONT;
begin
  // Panel is the REAL TStatusPanel, we need to find our special one.
  DFSPanel := FindLinkedPanel(Panel);
  Assert(DFSPanel <> NIL, 'Panel links corrupted');

  // Stupid VCL status bar doesn't always have the right font in Canvas.
  OldFont := SelectObject(Canvas.Handle, FExtentFont);
  try
    if Addr(OnDrawPanel) <> NIL then
      inherited DrawPanel(TStatusPanel(DFSPanel), Rect);
    DFSPanel.DrawPanel(Rect);
  finally
    SelectObject(Canvas.Handle, OldFont);
  end;
end;

function TdfsStatusBar.FindLinkedPanel(Panel: TStatusPanel): TdfsStatusPanel;
var
  x: integer;
begin
  Result := NIL;
  for x := 0 to Panels.Count-1 do
    if Panels[x].LinkedPanel = Panel then
    begin
      Result := Panels[x];
      break;
    end;
end;

function TdfsStatusBar.AppWinHook(var Message: TMessage): boolean;
begin
  if Message.Msg = WM_ACTIVATEAPP then
  begin
    if UseMonitorDLL then
    begin
{      if Message.wParam = 1 then
        PostMessage(Handle, WM_REFRESHLOCKINDICATORS, 0, 0);}
    end else begin
      // We're being deactivated, someone may change an indicator and that will
      // screw up the GetKeyState API call.
      if Message.wParam = 0 then
        MayNeedRefresh := TRUE;
      // Won't work in some situations if we call it directly.
      PostMessage(Handle, WM_REFRESHLOCKINDICATORS, 0, 0);
    end;
  end;
  Result := FALSE;
end;

procedure TdfsStatusBar.WMRefreshLockIndicators(var Msg: TMessage);
var
  x: integer;
begin
  Panels.BeginUpdate;
  try
    for x := 0 to Panels.Count-1 do
      if Panels[x].PanelType in [sptCapsLock, sptNumLock, sptScrollLock] then
        InvalidatePanel(Panels[x].Index);
  finally
    Panels.EndUpdate;
  end;
end;

procedure TdfsStatusBar.CMFontChanged(var Msg: TMessage);
var
  x: integer;
begin
  inherited;

  UpdateExtentFont;

  if Panels = NIL then exit;

  Panels.BeginUpdate;
  try
    for x := 0 to Panels.Count-1 do
      if Panels[x].AutoFit then
        Panels[x].UpdateAutoFitWidth;
  finally
    Panels.EndUpdate;
  end;
end;


procedure TdfsStatusBar.SetOnDrawPanel(const Value: TdfsDrawPanelEvent);
begin
  inherited OnDrawPanel := TDrawPanelEvent(Value);
end;

function TdfsStatusBar.GetOnDrawPanel: TdfsDrawPanelEvent;
begin
  TDrawPanelEvent(Result) := inherited OnDrawPanel;
end;

function TdfsStatusBar.GetVersion: string;
begin
  Result := DFS_COMPONENT_VERSION;
end;

procedure TdfsStatusBar.SetVersion(const Val: string);
begin
  { empty write method, just needed to get it to show up in Object Inspector }
end;


procedure TdfsStatusBar.CMEnabledChanged(var Msg: TMessage);
var
  x: integer;
begin
  inherited;
  Invalidate;
  for x := 0 to Panels.Count-1 do
    Panels[x].EnabledChanged;
end;

procedure TdfsStatusBar.CMHintShow(var Msg: TMessage);
  function FindClosestBefore(x: integer): TdfsStatusPanel;
  var
    y: integer;
  begin
    Result := NIL;
    for y := 0 to Panels.Count-1 do
    begin
      if GetPanelRect(y).Left < x then
        Result := Panels[y]
      else
        break;
    end;
(*  If I do it this way, it screws up.  Optimizaer bug, maybe?
    for y := Panels.Count-1 downto 0 do
    begin
      if GetPanelRect(y).Left < x then
      begin
        Result := Panels[y];
        break;
      end;
    end;*)
  end;

  function FindClosestAfter(x: integer): TdfsStatusPanel;
  var
    y: integer;
  begin
    Result := NIL;
    for y := 0 to Panels.Count-1 do
    begin
      if GetPanelRect(y).Right > x then
      begin
        Result := Panels[y];
        break;
      end;
    end;
  end;
var
  x: integer;
  Panel: TdfsStatusPanel;
  R: TRect;
begin
  inherited;

  with TCMHintShow(Msg) do
  begin
    begin
      Panel := NIL;
      for x := 0 to Panels.Count-1 do
      begin
        if PtInRect(GetPanelRect(x), HintInfo.CursorPos) then
        begin
          Panel := Panels[x];
          break;
        end;
      end;

      if (Panel = NIL) or (Panel.Hint = '') then
      begin
        // Hit a border, or a panel without a hint.  What we have to do here is
        // tell the hint info how big of a rectangle the hint applies to.  So,
        // we must find the first panel before this point with a hint, and the
        // first panel after this point with a hint and set CursorRect equal to
        // the area between those two panels.  CursorRect already has the area
        // of the status bar, so if we don't find a panel, it's ok.

        // Find first valid panel before hint position and set CursorRect.Left
        Panel := FindClosestBefore(HintInfo.CursorPos.x);
        while (Panel <> NIL) do
        begin
          R := GetPanelRect(Panel.Index);
          if Panel.Hint <> '' then
          begin
            HintInfo.CursorRect.Left := R.Right;
            Panel := NIL;
          end else
            Panel := FindClosestBefore(R.Left);
        end;

        // Find first valid panel after hint position and set CursorRect.Right
        Panel := FindClosestAfter(HintInfo.CursorPos.x);
        while (Panel <> NIL) do
        begin
          R := GetPanelRect(Panel.Index);
          if Panel.Hint <> '' then
          begin
            HintInfo.CursorRect.Right := R.Left;
            Panel := NIL;
          end else
            Panel := FindClosestAfter(R.Right);
        end;
      end else begin
        // Give it the hint of the panel
        HintInfo.HintStr := Panel.Hint;
        // Tell the hint mechanism that it needs to check the hint when the
        // cursor leaves the panel rectangle.
        HintInfo.CursorRect := GetPanelRect(Panel.Index);
      end;
    end;
  end;
end;

procedure TdfsStatusBar.DeregisterMainWinHook(Client: TdfsStatusPanel);
begin
  FMainWinHookClients.Remove(Client);
  Assert(FMainWinHookClients.Count >= 0, 'Unbalanced MainWinHook registrations');
  if FMainWinHookClients.Count < 1 then
    Application.UnhookMainWindow(AppWinHook);
end;

procedure TdfsStatusBar.RegisterMainWinHook(Client: TdfsStatusPanel);
begin
  FMainWinHookClients.Add(Client);
  if FMainWinHookClients.Count = 1 then
    Application.HookMainWindow(AppWinHook);
end;



procedure TdfsStatusBar.Loaded;
var
  x: integer;
begin
  inherited Loaded;

  UpdateExtentFont;

  for x := 0 to Panels.Count-1 do
    if Panels[x].AutoFit then
      Panels[x].UpdateAutoFitWidth;
end;

procedure TdfsStatusBar.CreateWnd;
var
  x: integer;
begin
  inherited CreateWnd;

  if not (csLoading in ComponentState) then
  begin
    UpdateExtentFont;

    for x := 0 to Panels.Count-1 do
      if Panels[x].AutoFit then
        Panels[x].UpdateAutoFitWidth;
  end;

  if FDLLClientCount > 0 then
    FKeyHookMsg := DLLRegisterKeyboardHook(Handle);
end;

procedure TdfsStatusBar.WMDestroy(var Msg: TWMDestroy);
begin
  if FUseMonitorDLL and (FDLLClientCount > 0) then
    DLLDeregisterKeyboardHook(Handle);

  inherited;
end;


function TdfsStatusBar.TextExtent(const Text: wideString): TSize;
begin
  if not GetTextExtentPoint32W(FExtentCanvas, PWideChar(Text), Length(Text),
     Result) then
  begin
    Result.cx := -1;
    Result.cy := -1;
  end;
end;

procedure TdfsStatusBar.UpdateExtentFont;
begin
  if FExtentFont <> 0 then
  begin
    SelectObject(FExtentCanvas, FExtentFontOld);
    DeleteObject(FExtentFont);
  end;

  // In D4, the font handle might be different than what TFont describes!
  FExtentFont := CopyHFont(Font.Handle);
  FExtentFontOld := SelectObject(FExtentCanvas, FExtentFont);
end;

procedure TdfsStatusBar.SetUseMonitorDLL(const Value: boolean);
begin
  if FUseMonitorDLL <> Value then
  begin
    FUseMonitorDLL := Value;
    UpdateKeyboardHooks;
    if FUseMonitorDLL and (not DFSKbDLL_Loaded) {and
       not (csDesigning in ComponentState)} then
    begin
      UseMonitorDLL := FALSE;
      if csDesigning in ComponentState then
        raise Exception.Create('Could not load ' + DFSKbDLLName);
    end;    
  end;
end;

procedure TdfsStatusBar.UpdateKeyboardHooks;
var
  x: integer;
begin
  for x := 0 to Panels.Count-1 do
    Panels[x].UpdateKeyboardHook;
end;


procedure TdfsStatusBar.DeregisterSystemHook;
begin
  dec(FDLLClientCount);
  if FDLLClientCount < 1 then
  begin
    if DFSKbDLL_Loaded and HandleAllocated then
      DLLDeregisterKeyboardHook(Handle);
    FDLLClientCount := 0;
    if DFSKbDLL_Loaded then
      UnloadDFSKbDLL;
  end;
end;

procedure TdfsStatusBar.RegisterSystemHook;
begin
  inc(FDLLClientCount);
  if (FDLLClientCount = 1) {and not (csDesigning in ComponentState)} then
  begin
    if not DFSKbDLL_Loaded then
      IniTdfsKbDLL;
    if HandleAllocated and DFSKbDLL_Loaded then
      FKeyHookMsg := DLLRegisterKeyboardHook(Handle);
  end;
end;

procedure TdfsStatusBar.WndProc(var Msg: TMessage);
  function VKToPanelType(VKCode: byte): TdfsStatusPanelType;
  begin
    case VKCode of
      VK_NUMLOCK: Result := sptNumLock;
      VK_SCROLL:  Result := sptScrollLock;
    else
      Result := sptCapsLock;
    end;
  end;
var
  x: integer;
begin
  if Msg.Msg = FKeyHookMsg then
  begin
    for x := 0 to Panels.Count-1 do
      if VKToPanelType(Msg.wParam) = Panels[x].PanelType then
      begin
        Panels[x].FKeyOn := Odd(Msg.lParam);
        Panels[x].Invalidate;
      end;
  end else
    inherited WndProc(Msg);
end;

procedure TdfsStatusBar.Click;
var
  x: integer;
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  CursorPos := ScreenToClient(CursorPos);
  for x := 0 to Panels.Count-1 do
  begin
    if PtInRect(GetPanelRect(x), CursorPos) then
    begin
      Panels[x].Click;
      break;
    end;
  end;

  inherited Click;
end;

procedure TdfsStatusBar.WMPaint(var Msg: TWMPaint);
  procedure DrawSizeGrip(R: TRect);
  begin
    OffsetRect(R, -1, -1);
    with Canvas do
    begin
      Brush.Color := Color;
      Pen.Width := 1;
      FillRect(R);
      Pen.Color := clBtnHighlight;
      MoveTo(R.Right - 2, R.Bottom);
      LineTo(R.Right, R.Bottom - 2);
      MoveTo(R.Right - 13, R.Bottom);
      LineTo(R.Right, R.Bottom - 13);
      MoveTo(R.Right - 9, R.Bottom);
      LineTo(R.Right, R.Bottom - 9);
      MoveTo(R.Right - 5, R.Bottom);
      LineTo(R.Right, R.Bottom - 5);
      MoveTo(R.Right - 1, R.Bottom);
      LineTo(R.Right, R.Bottom);

      Pen.Color := clBtnShadow;
      MoveTo(R.Right - 11, R.Bottom);
      LineTo(R.Right, R.Bottom - 11);
      MoveTo(R.Right - 7, R.Bottom);
      LineTo(R.Right, R.Bottom - 7);
      MoveTo(R.Right - 3, R.Bottom);
      LineTo(R.Right, R.Bottom - 3);

      Brush.Color := clBtnFace;
      Pen.Color := clBtnShadow;
      MoveTo(R.Left, R.Top);
      LineTo(R.Right, R.Top);
    end;
  end;
var
  R: TRect;
begin
  inherited;
  if Color <> clBtnFace then
  begin
    R := ClientRect;
    R.Left := R.Right - 15;
    Inc(R.Top, 3);
    dec(R.Bottom);
    DrawSizeGrip(R);
  end;
end;

function TdfsStatusBar.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self);
end;

function TdfsStatusBar.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;

procedure TdfsStatusBar.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;


initialization
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('dfsStatusBar: init begin', TRUE);
  {$ENDIF}
  MayNeedRefresh := FALSE;
  KeyboardHookHandle := 0;
  KeyHookClients := TList.Create;
  RegisteredTimers := 0;
  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('dfsStatusBar: init end.', TRUE);
  {$ENDIF}

finalization

  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('dfsStatusBar: finalization begin.', TRUE);
  {$ENDIF}
  // remove hook just in case it somehow got left installed
  if KeyboardHookHandle <> 0 then
  begin
    UnhookWindowsHookEx(KeyboardHookHandle);
    KeyboardHookHandle := 0;
    Assert(FALSE, 'TdfsStatusBar: Keyboard hook still installed');
  end;

  Assert(RegisteredTimers = 0, 'TdfsStatusBar: Unbalanced timer registrations');

  KeyHookClients.Free;
  KeyHookClients := NIL;

  if DFSKb.DFSKbDLL_Loaded then
    UnloadDFSKbDLL;

  {$IFDEF DFS_DEBUG}
  DFSDebug.Log('dfsStatusBar: finalization end.', TRUE);
  {$ENDIF}
end.



