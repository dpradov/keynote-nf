unit gf_miscvcl;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

{$I gf_base.inc}

interface
uses
   Winapi.Windows,
   Winapi.ShellAPI,
   Winapi.Messages,
   Winapi.DWMAPI,  // DWMAPI is required for modern shadows
   System.Classes,
   System.SysUtils,
   System.DateUtils,
   System.Math,
   Vcl.Forms,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.ExtCtrls,
   Vcl.Dialogs,
   Vcl.ImgList,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   SynGdiPlus;


const
  SmallFontsPixelsPerInch = 96;          // 100% scaling for Windows

function GetSystemPixelsPerInch: integer;   // See comments below

{
type
  TFontDialog = class(Vcl.Dialogs.TFontDialog)
  protected
    function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool; override;
  end;
}

Procedure PostKeyEx32( key: Word; Const shift: TShiftState;
            specialkey: Boolean );

Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift:
            TShiftState; specialkey: Boolean );

function CheckDir( const aDir : string; const AutoCreate : boolean ) : boolean;

procedure SleepWell( const TenthsOfSecond : cardinal );
function VerdanaInstalled : boolean;
function TahomaInstalled : boolean;

type
  TMsgDlgDefBtn = (def1, def2, def3, def4);


function PopUpMessage(const mStr : string; const caption: string; const mType : TMsgDlgType;
                      const mButtons : TMsgDlgButtons; const DefButton: TMsgDlgDefBtn = def1;
                      const mHelpCtx : integer= 0) : word; overload;
{
Function DefMessageDlg(const aCaption: String;
                       const Msg: string;
                       DlgType: TMsgDlgType;
                       Buttons: TMsgDlgButtons;
                       DefButton: Integer;
                       HelpCtx: Longint): Integer;
}

function DoMessageBox (const text: string; caption: string;
                       DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgDefBtn= def1;
                       HelpCtx: Longint = 0; hWnd: HWND= 0): integer; overload;
function DoMessageBox (const text: string; caption: string; uType: UINT= 0; hWnd: HWND= 0): integer; overload;


function ScaleFromSmallFontsDimension(const X: Integer): Integer;
function DotsToTwips(dots: Integer): integer; inline;
function PixelsToTwips(Pixels, DPI: Integer): Integer; inline;
function TwipsToPixels(Twips, DPI: Integer): Integer; inline;

procedure LoadBitmapFromResource(ImageList: TImageList; const ResourceName: string; TransparentColor: TColor);
procedure LoadGifFromResource(ImageList: TImageList; const ResName: string); overload;
procedure LoadGifFromResource(Image: TImage; const ResName: string); overload;
function DarkenColor(Color: TColor; Amount: Byte; BInc: byte= 0): TColor;
function LightenColor(Color: TColor; Amount: Byte; BInc: byte= 0): TColor;
function GetHotColorFor(BackColor, FontColor: TColor): TColor;
function InvertColor(Color: TColor): TColor;
//function ColorInformation(Color: TColor): string;      // Included in ColorPicker

function CheckCalendarInTDateTimePicker(AOwner: TWinControl): boolean;
function PrepareTDateTimePicker(Control: TDateTimePicker): boolean;

procedure PauseFormUpdates(Form: TForm);
procedure ResumeFormUpdates(Form: TForm);

procedure AdjustRTLControls(Parent: TWinControl);

function GetEditCaretPos(Edit: TEdit; CharIndex: integer): TPoint;
procedure EnableShadow(FormHandle: HWND);
procedure EnableTopMost(hWND: HWND; Enable: boolean);


var
  _TahomaFontInstalled : boolean = false;

implementation

procedure SleepWell( const TenthsOfSecond : cardinal );
var
  TargetTicks : cardinal;
begin
  TargetTicks := GetTickCount+(100*TenthsOfSecond);
  while (TargetTicks-GetTickCount) >  0 do begin
    sleep(100); // Check for updates 10 times a second.
    Application.ProcessMessages;
  end;
end; // SleepWell


function VerdanaInstalled : boolean;
var
   i : integer;
begin
  { look BACKWARDS, because the string 'Verdana'
  will be closer to the end of the font list, so
  we will find it faster by looking from the end }
  result := false;
  for i := pred( screen.fonts.count ) downto 0 do
    if ( screen.fonts[i] = 'Verdana' ) then
    begin
      result := true;
      break;
    end;
end; // VerdanaInstalled

function TahomaInstalled : boolean;
var
   i : integer;
begin
  { look BACKWARDS, because the string 'Verdana'
  will be closer to the end of the font list, so
  we will find it faster by looking from the end }
  result := false;
  for i := pred( screen.fonts.count ) downto 0 do
    if ( screen.fonts[i] = 'Tahoma' ) then
    begin
      result := true;
      break;
    end;
end; // TahomaInstalled


{************************************************************
 * Procedure PostKeyEx32
 *
 * Parameters:
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 * Description:
 *  Uses keybd_event to manufacture a series of key events matching
 *  the passed parameters. The events go to the control with focus.
 *  Note that for characters key is always the upper-case version of
 *  the character. Sending without any modifier keys will result in
 *  a lower-case character, sending it with [ssShift] will result
 *  in an upper-case character!
 *Created: 17.7.98 by P. Below
 ************************************************************}
Procedure PostKeyEx32( key: Word; Const shift: TShiftState;
                     specialkey: Boolean );
  Type
    TShiftKeyInfo = Record
                      shift: Byte;
                      vkey : Byte;
                    End;
    byteset = Set of 0..7;
  Const
    shiftkeys: Array [1..3] of TShiftKeyInfo =
      ((shift: Ord(ssCtrl); vkey: VK_CONTROL ),
       (shift: Ord(ssShift); vkey: VK_SHIFT ),
       (shift: Ord(ssAlt); vkey: VK_MENU ));
  Var
    flag: DWORD;
    bShift: ByteSet absolute shift;
    i: Integer;
  Begin
    For i := 1 To 3 Do Begin
      If shiftkeys[i].shift In bShift Then
        keybd_event( shiftkeys[i].vkey,
                     MapVirtualKey(shiftkeys[i].vkey, 0),
                     0, 0);
    End; { For }
    If specialkey Then
      flag := KEYEVENTF_EXTENDEDKEY
    Else
      flag := 0;

    keybd_event( key, MapvirtualKey( key, 0 ), flag, 0 );
    flag := flag or KEYEVENTF_KEYUP;
    keybd_event( key, MapvirtualKey( key, 0 ), flag, 0 );

    For i := 3 DownTo 1 Do Begin
      If shiftkeys[i].shift In bShift Then
        keybd_event( shiftkeys[i].vkey,
                     MapVirtualKey(shiftkeys[i].vkey, 0),
                     KEYEVENTF_KEYUP, 0);
    End; { For }
  End; { PostKeyEx32 }


{************************************************************
 * Procedure PostKeyEx
 *
 * Parameters:
 *  hWindow: target window to be send the keystroke
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *           For letters alsways use the upper-case version!
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 *           If this parameter is true, bit 24 of the lparam for
 *           the posted WM_KEY* messages will be set.
 * Description:
 *  This procedure sets up Windows key state array to correctly
 *  reflect the requested pattern of modifier keys and then posts
 *  a WM_KEYDOWN/WM_KEYUP message pair to the target window. Then
 *  Application.ProcessMessages is called to process the messages
 *  before the keyboard state is restored.
 * Error Conditions:
 *  May fail due to lack of memory for the two key state buffers.
 *  Will raise an exception in this case.
 * NOTE:
 *  Setting the keyboard state will not work across applications
 *  running in different memory spaces on Win32.
 *Created: 02/21/96 16:39:00 by P. Below
 ************************************************************}
Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift:
    TShiftState; specialkey: Boolean );
Type
  TBuffers = Array [0..1] of TKeyboardState;
Var
  pKeyBuffers : ^TBuffers;
  LP: LPARAM;
Begin
  (* check if the target window exists *)
  If IsWindow(hWindow) Then Begin
    (* set local variables to default values *)
    pKeyBuffers := Nil;
    LP := MakeLParam(0, MapVirtualKey(key, 0));

    (* modify lparam if special key requested *)
    If specialkey Then
      LP := LP or $1000000;

    (* allocate space for the key state buffers *)
    New(pKeyBuffers);
    try
      (* Fill buffer 1 with current state so we can later restore it.
         Null out buffer 0 to get a "no key pressed" state. *)
      GetKeyboardState( pKeyBuffers^[1] );
      FillChar(pKeyBuffers^[0], Sizeof(TKeyboardState), 0);

      (* set the requested modifier keys to "down" state in the buffer *)
      If ssShift In shift Then
        pKeyBuffers^[0][VK_SHIFT] := $80;
      If ssAlt In shift Then Begin
        (* Alt needs special treatment since a bit in lparam needs also be set *)
        pKeyBuffers^[0][VK_MENU] := $80;
        LP := LP or $20000000;
      End;
      If ssCtrl In shift Then
        pKeyBuffers^[0][VK_CONTROL] := $80;
      If ssLeft In shift Then
        pKeyBuffers^[0][VK_LBUTTON] := $80;
      If ssRight In shift Then
        pKeyBuffers^[0][VK_RBUTTON] := $80;
      If ssMiddle In shift Then
        pKeyBuffers^[0][VK_MBUTTON] := $80;

      (* make out new key state array the active key state map *)
      SetKeyboardState( pKeyBuffers^[0] );

      (* post the key messages *)
      If ssAlt In Shift Then Begin
        PostMessage( hWindow, WM_SYSKEYDOWN, key, LP);
        PostMessage( hWindow, WM_SYSKEYUP, key, LP or $C0000000);
      End
      Else Begin
        PostMessage( hWindow, WM_KEYDOWN, key, LP);
        PostMessage( hWindow, WM_KEYUP, key, LP or $C0000000);
      End;
      (* process the messages *)
      Application.ProcessMessages;

      (* restore the old key state map *)
      SetKeyboardState( pKeyBuffers^[1] );
    finally
      (* free the memory for the key state buffers *)
      If pKeyBuffers <> Nil Then
        Dispose( pKeyBuffers );
    End; { If }
  End;
End; { PostKeyEx }

function CheckDir( const aDir : string; const AutoCreate : boolean ) : boolean;
var
  io : integer;
begin
  result := DirectoryExists( aDir );
  if (( not result ) and AutoCreate )then
  begin
    if ( messagedlg( 'Directory does not exist:' +#13+
      aDir +#13+ 'Attempt to create directory?', mtError, [mbYes,mbNo], 0 ) = mrYes ) then
      begin
        {$I-}
        mkdir( aDir );
        io := IOResult;
        {$I+}
        result := ( io = 0 );
        if result then
          messagedlg( 'Directory successfully created.', mtInformation, [mbOK], 0 )
        else
          messagedlg( 'Failed to create directory. Error code: ' + inttostr( io ), mtError, [mbOK], 0 );
      end;
  end;
end; // CheckDir



function ScaleFromSmallFontsDimension(const X: Integer): Integer;
begin
  Result := MulDiv(X, Screen.PixelsPerInch, SmallFontsPixelsPerInch);
end;


function GetSystemPixelsPerInch: integer;
var
   previousDpiContext: DPI_AWARENESS_CONTEXT;
   DC: HDC;
begin
   { Since DPI Awareness is set to "GDI Scaling", when using Screen.PixelsPerInch we always get 96,
     even though temporarily we changed the DPI Awareness.
     With this method we make sure to obtain the real DPI
   }

   try
      previousDpiContext := SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
      DC := GetDC(0);
      try
        Result:= GetDeviceCaps(DC, LOGPIXELSX);
      finally
        ReleaseDC(0, DC);
        SetThreadDpiAwarenessContext(previousDpiContext);
      end;

   except
      Result:= Screen.PixelsPerInch;
   end;
end;

function PixelsToTwips(Pixels, DPI: Integer): Integer; inline;
begin
   Result := MulDiv(Pixels, 1440, DPI);
end;

function TwipsToPixels(Twips, DPI: Integer): Integer; inline;
begin
   Result := MulDiv(Twips, DPI, 1440);
end;


function DotsToTwips(dots: Integer ): integer; inline;
var
  DC: HDC;
  dpi: Integer;
begin

{ A point is 1/72 inch. With that formula you can convert between inches and points.
 What() 's missing is the number of pixels in an inch. You can retrieve that by calling
 GetDeviceCaps( hdc, LOGPIXELSY ) which returns the number of pixels in 1 logical inch of your screen.
 A logical inch isn't exactly 1 inch if you measure it, but that isn't a problem as the point size
 only relates to logical inches on your screen.
 GetDeviceCaps( hdc, LOGPIXELSX )--> Graphics.DpiX
 A twip is a twentieth of a point, i.e., a 1440th of an inch (72*20)
}
{
   if dpi = 0 then begin
      DC := GetDC(0);
      try
        DPI:= GetDeviceCaps(DC, LOGPIXELSX);
      finally
        ReleaseDC(0, DC);
      end;
   end;
}
  dpi:= Screen.PixelsPerInch;
  Result := MulDiv( dots, 1440, dpi );
end;

{
There is a known issue with TFontDialog in Delphi when using scaling settings other than 100%.
This issue can cause the selected font to appear larger than expected.
The problem lies in how TFontDialog (and, in fact, the underlying Win32 ChooseFont API) handles DPI awareness.
In particular, TFontDialog does not fully support “Per Monitor V2” DPI awareness.
As a workaround, it seems that we can temporarily switch to “System aware” DPI awareness while the dialog
is displayed and switch back afterwards.

https://stackoverflow.com/questions/59679860/delphi-tfontdialog-how-to-scale-for-high-dpi

However, in Alexandria 11.3, it is working ok with “Per Monitor V2”, but not with "GDI Scaling"
and the solution indicated it is not working

We will use the real DPI calling to GetSystemPixelsPerInch (above), to get the correct font size
}

{
function TFontDialog.TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
var
  previousDpiContext: DPI_AWARENESS_CONTEXT;
begin
  previousDpiContext := SetThreadDpiAwarenessContext(DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
  try
    Result := inherited TaskModalDialog(DialogFunc, DialogData);
  finally
    SetThreadDpiAwarenessContext(previousDpiContext);
  end;
end;
}


{  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp);

  MB_ICONHAND = $00000010;
  MB_ICONQUESTION = $00000020;
  MB_ICONEXCLAMATION = $00000030;
  MB_ICONASTERISK = $00000040;
  MB_USERICON = $00000080;
  MB_ICONWARNING                 = MB_ICONEXCLAMATION;
  MB_ICONERROR                   = MB_ICONHAND;
  MB_ICONINFORMATION             = MB_ICONASTERISK;
  MB_ICONSTOP                    = MB_ICONHAND;

  MB_OK = $00000000;
  MB_OKCANCEL = $00000001;
  MB_ABORTRETRYIGNORE = $00000002;
  MB_YESNOCANCEL = $00000003;
  MB_YESNO = $00000004;
  MB_RETRYCANCEL = $00000005;

}


//http://msdn.microsoft.com/en-us/library/ms645505%28VS.85%29.aspx
function DoMessageBox (const text: string; caption: string;
                       DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefButton: TMsgDlgDefBtn = def1;
                       HelpCtx: Longint = 0; hWnd: HWND= 0): integer;
var
   uType: UINT;
begin
    uType:= 0;
    case DlgType of
       mtWarning: uType:= MB_ICONWARNING;
       mtError: uType:= MB_ICONERROR;
       mtInformation: uType:= MB_ICONINFORMATION;
       mtConfirmation: uType:= MB_ICONQUESTION;
    end;
    if (mbYes in Buttons) and (mbNo in Buttons) and (mbCancel in Buttons) then
       uType:= uType or MB_YESNOCANCEL
    else if (mbYes in Buttons) and (mbNo in Buttons) then
       uType:= uType or MB_YESNO
    else if (mbOk in Buttons) and (mbCancel in Buttons) then
       uType:= uType or MB_OKCANCEL
    else if (mbAbort in Buttons) and (mbRetry in Buttons) and (mbIgnore in Buttons) then
       uType:= uType or MB_ABORTRETRYIGNORE
    else if (mbRetry in Buttons) and(mbCancel in Buttons) then
       uType:= uType or MB_RETRYCANCEL
    else
       uType:= uType or MB_OK;

    case DefButton of
       def2: uType:= uType or MB_DEFBUTTON2;
       def3: uType:= uType or MB_DEFBUTTON3;
       def4: uType:= uType or MB_DEFBUTTON4;
    end;

    if hWnd = 0 then
       hWnd:= Application.MainFormHandle;
    Result:= MessageBox(hWnd, PChar(text), PChar(caption), uType);
end;

function DoMessageBox (const text: string; caption: string; uType: UINT= 0; hWnd: HWND= 0): integer;
begin
    if hWnd = 0 then
       hWnd:= Application.MainFormHandle;
    Result:= MessageBox(hWnd, PChar(text), PChar(caption), uType);
end;


function PopUpMessage(const mStr: string; const caption: string; const mType: TMsgDlgType;
                      const mButtons: TMsgDlgButtons;
                      const DefButton: TMsgDlgDefBtn = def1;
                      const mHelpCtx: integer = 0) : word;

// Like MessageDlg, but brings application window to front before
// displaying the message, and minimizes application if it was
// minimized before the message was shown.
var
  WasIconic : boolean;
begin
  WasIconic := ( IsIconic(Application.Handle) = TRUE );
  if WasIconic then
    Application.Restore;
  Application.BringToFront;
  result := DoMessageBox( mStr, caption, mType, mButtons, DefButton, mHelpCtx );
  if WasIconic then
    Application.Minimize;
end; // PopUpMessage

(*
Function DefMessageDlg(const aCaption: String; const Msg: string;
                       DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
                       DefButton: Integer;   HelpCtx: Longint): Integer;
// code by Peter Below (TeamB)
Var
  i: Integer;
  btn: TButton;
Begin
  With CreateMessageDialog(Msg, DlgType, Buttons) Do
  try
    Caption := aCaption;
    HelpContext := HelpCtx;
    For i := 0 To ComponentCount-1 Do Begin
      If Components[i] Is TButton Then Begin
        btn := TButton(Components[i]);
        btn.Default := btn.ModalResult = DefButton;
        If btn.Default Then
          ActiveControl := Btn;
      End;
    End; { For }
    Result := ShowModal;
  finally
    Free;
  end;
End; // DefMessageDlg
*)

procedure LoadBitmapFromResource(ImageList: TImageList; const ResourceName: string; TransparentColor: TColor);
var
  Bitmap: TBitmap;
begin
  //IMG_Toolbar.ResourceLoad(rtBitmap, 'TOOLBAR_MAIN',  clFuchsia);                    // Doesn't seem to be using 24 bit color
  //IMG_Format.ResInstLoad( HInstance, rtBitmap, 'TOOLBAR_FORMAT',  clFuchsia );       // Idem

  Bitmap := TBitmap.Create;
  Bitmap.LoadFromResourceName(HInstance, ResourceName);
  ImageList.AddMasked(Bitmap, TransparentColor);
  Bitmap.Free;
end;


procedure LoadGifFromResource(Image: TImage; const ResName: string);
var
  ResStream: TResourceStream;
  GIFImage: TGifImage;
begin
  ResStream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  GIFImage := TGifImage.Create;
  GIFImage.LoadFromStream(ResStream);
  Image.Picture.Assign(GifImage);
  GIFImage.Free;
  ResStream.Free;
end;

procedure LoadGifFromResource(ImageList: TImageList; const ResName: string);
var
  ResStream: TResourceStream;
  GIFImage: TGifImage;
  Bitmap: TBitmap;
  TransparentColor: TColor;
begin
  ResStream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  GIFImage := TGifImage.Create;
  GIFImage.LoadFromStream(ResStream);
  Bitmap:= GifImage.ToBitmap;

  // When converting to GIF from BMP, the background color that was set to clFuchsia (RGB=255,0,255)
  // may no longer be so. I notice that when converting to GIF it becomes (RGB=252,2,252).
  // In case it might change for some other reason, instead of setting it as a parameter I will use the color
  // of the lower left corner of the image.
  TransparentColor:= Bitmap.Canvas.Pixels[0, Bitmap.Height - 1];
  ImageList.AddMasked(Bitmap, TransparentColor);
  GIFImage.Free;
  ResStream.Free;
  Bitmap.Free;
end;


function DarkenColor(Color: TColor; Amount: Byte; BInc: byte= 0): TColor;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  RGBColor:= GetRGBFromColor(Color);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);
  R := R - Amount;
  G := G - Amount;
  B := B - Amount + BInc;

  if R < 0 then R:= 0;
  if G < 0 then G:= 0;
  if B < 0 then B:= 0;
  if B > 255 then B:= 255;

  Result := RGB(R,G,B);
end;


function LightenColor(Color: TColor; Amount: Byte; BInc: byte= 0): TColor;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  RGBColor:= GetRGBFromColor(Color);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);
  R := R + Amount;
  G := G + Amount;
  B := B + Amount + BInc;

  if R > 255 then R:= 255;
  if G > 255 then G:= 255;
  if B > 255 then B:= 255;

  Result := RGB(R,G,B);
end;


function InvertColor(Color: TColor): TColor;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  RGBColor:= GetRGBFromColor(Color);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);
  R := 255 - R;
  G := 255 - G;
  B := 255 - B;

  Result := RGB(R,G,B);
end;


function BlueishColor(Color: TColor): Boolean;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  Result:= false;

  RGBColor:= GetRGBFromColor(Color);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);

  if (B>= $FB) and ( ((G=$FF) and (R < $B3)) or ((G < $D8) and (R<$D8))) then
     Result:= true;
end;


function YellowishColor(Color: TColor): Boolean;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  Result:= false;

  RGBColor:= GetRGBFromColor(Color);
  R := GetRValue(RGBColor);
  G := GetGValue(RGBColor);
  B := GetBValue(RGBColor);

  if (R = $FF) and (G = $FF) and (B < $D8) then
     Result:= true;
end;


function GetColorLuminosity(Color: TColor): Double;
var
  Red, Green, Blue: Byte;
  RGBColor: Longint;
begin
  RGBColor := ColorToRGB(Color);
  Red   := GetRValue(RGBColor);
  Green := GetGValue(RGBColor);
  Blue  := GetBValue(RGBColor);

  Result := 0.299 * Red + 0.587 * Green + 0.114 * Blue;
end;


function GetRelativeLuminosity(Color: TColor): Double;
var
  R, G, B: Double;
begin
  // Extract normalized RGB components
  Color := ColorToRGB(Color);
  R := GetRValue(Color) / 255;
  G := GetGValue(Color) / 255;
  B := GetBValue(Color) / 255;

  // Adjust values ​​according to relative luminance formula
  if R <= 0.03928 then
    R := R / 12.92
  else
    R := Power((R + 0.055) / 1.055, 2.4);

  if G <= 0.03928 then
    G := G / 12.92
  else
    G := Power((G + 0.055) / 1.055, 2.4);

  if B <= 0.03928 then
    B := B / 12.92
  else
    B := Power((B + 0.055) / 1.055, 2.4);

  // Calculate relative luminosity
  Result := 0.2126 * R + 0.7152 * G + 0.0722 * B;
end;


function GetHotColorFor(BackColor, FontColor: TColor): TColor;
var
   RelLumBG: Double;
begin
   RelLumBG:= GetRelativeLuminosity(BackColor);

   if RelLumBG > 0.45 then begin
      Result:= clBlue;
      if BlueishColor(BackColor) or BlueishColor(FontColor) then
         Result:= $0000CD;   // Rojo oscuro
   end
   else begin
       Result:= clYellow;
       if YellowishColor(BackColor) or YellowishColor(FontColor) then
          Result:= clWhite;
   end;

end;

{      // Included in ColorPicker

function ColorInformation(Color: TColor): string;
var
  RGBColor: Cardinal;
  R, G, B: Integer;
begin
  Result:= ColorToString(Color);
  if Copy(Result,1,2) = 'cl' then
     Result:= Copy(Result,3)
  else begin
     RGBColor:= GetRGBFromColor(Color);
     R := GetRValue(RGBColor);
     G := GetGValue(RGBColor);
     B := GetBValue(RGBColor);
     Result:= Format('RGB=%d,%d,%d', [R,G,B]);
  end;

end;
}

// There is a bug in Delphi, and it does not handle certain calendars correctly, such as the Um-Al-Qura Calendar
// used in Saudi Arabia.
// This error appears in previous versions of KeyNote, such as 1.9.5.2, when opening the alarms window,
// but since TDateTimePicker controls have been incorporated into the search panel, the error can directly
// prevent the application from opening.
// I find that I cannot even insert the control at design time or open it if it is already inserted.
// The exception ECommonCalendarError: Failed to set calendar min/max range is generated.
// I have tested that by setting certain controlled values MinDate and MaxDate the control seems to work
// Therefore, to avoid the error, I will initially set these fields to Visible=False and will only make them
// visible in a controlled manner, with the help of this method.

function CheckCalendarInTDateTimePicker(AOwner: TWinControl): boolean;
var
  Control: TDateTimePicker;
begin
   Result:= False;
   Control:= TDateTimePicker.Create(AOwner);
   try
      try
        Control.Parent:= AOwner;
        Result:= True;
      except
      end;
   finally
      Control.Free;
   end;
end;


function PrepareTDateTimePicker(Control: TDateTimePicker): boolean;
begin
   Result:= False;
   try
     Control.MinDate:= EncodeDate(2000, 1, 1);
     Control.MaxDate:= EncodeDate(2075, 12, 31);
     Control.Date:= Today();
     Control.Visible := True;
     Result:= true;
   except
   end;
end;

procedure PauseFormUpdates(Form: TForm);
begin
  SendMessage(Form.Handle, WM_SETREDRAW, WPARAM(False), 0);
end;

procedure ResumeFormUpdates(Form: TForm);
begin
  SendMessage(Form.Handle, WM_SETREDRAW, WPARAM(True), 0);
  RedrawWindow(Form.Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
end;


procedure AdjustRTLControls(Parent: TWinControl);
var
  i: Integer;
  LabelControl: TLabel;
  RelatedControl: TWinControl;
  TempLeft: Integer;
begin

  for i := 0 to Parent.ControlCount - 1 do
     if Parent.Controls[i] is TLabel then begin
       LabelControl := TLabel(Parent.Controls[i]);

       if Assigned(LabelControl.FocusControl) and (LabelControl.FocusControl is TWinControl) then begin
         RelatedControl := TWinControl(LabelControl.FocusControl);

         if RelatedControl.Top < (LabelControl.Top + LabelControl.Height) then begin
            TempLeft := LabelControl.Left;
            LabelControl.Left := RelatedControl.Left;
            RelatedControl.Left := TempLeft;
            LabelControl.Left := LabelControl.Left + (RelatedControl.Width - LabelControl.Width);
         end;

         if LabelControl.Alignment = taRightJustify then
            LabelControl.Alignment := taLeftJustify;
       end;
     end
     else
     if Parent.Controls[i] is TWinControl then
        AdjustRTLControls(TWinControl(Parent.Controls[i]));
end;


function GetEditCaretPos(Edit: TEdit; CharIndex: integer): TPoint;
var
  Res: Longint;
begin
  FillChar(Result, SizeOf(Result), 0);
  if CharIndex > 0 then begin
     if CharIndex > Edit.GetTextLen then
        dec(CharIndex);
     Res := SendMessage(Edit.Handle, EM_POSFROMCHAR, WPARAM(CharIndex), 0);
     if Res >= 0 then begin
        Result.X := LoWord(Res);
        Result.Y := HiWord(Res);
     end;
  end;
  Result := Edit.ClientToScreen(Result);
end;


var
  DWMShadowAvailable: boolean;

procedure EnableShadow(FormHandle: HWND);
var
  AttrValue: Integer;
begin
  if not DWMShadowAvailable then Exit;
  try
    AttrValue := DWMNCRP_ENABLED;    // Enable non-client rendering
    DwmSetWindowAttribute(FormHandle, DWMWA_NCRENDERING_POLICY, @AttrValue, SizeOf(AttrValue));
  except
    DWMShadowAvailable := False;
  end;
end;

procedure EnableTopMost(hWND: HWND; Enable: boolean);
begin
   if Enable then
      SetWindowPos(hWND, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
   else
      SetWindowPos(hWND, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
end;



initialization

DWMShadowAvailable:= True;

end.




