unit knt.ui.SecureEdit;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2026 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Classes,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Clipbrd,
   Vcl.StdCtrls,
   Vcl.Forms,
   Vcl.Dialogs;

type
  TSecureEdit = class(TEdit)
  private
    FSecureMode: boolean;
    FSecureText: string;
    FObfuscationEnabled: Boolean;
    FAllowPaste: Boolean;
    FIgnoringFakeKeys: Boolean;
    //FTestKeys: string;
    procedure WMGetText(var Message: TMessage); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TMessage); message WM_GETTEXTLENGTH;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;

  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetSecureMode(Value: boolean);
    procedure SendFakeKeystrokes;

  public
    property SecureMode: boolean read FSecureMode write SetSecureMode;
    property ObfuscationEnabled: Boolean read FObfuscationEnabled write FObfuscationEnabled;
    property AllowPaste: Boolean read FAllowPaste write FAllowPaste;
    function GetSecureText: string;
    procedure ClearSecure;
    constructor Create(AOwner: TComponent); override;
  end;

{$IFDEF INCLUDE_POSTKEYEX}
  Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift: TShiftState; specialkey: Boolean );
{$ENDIF}


procedure Register;

implementation
{$IFNDEF INCLUDE_POSTKEYEX}
uses
  gf_miscvcl;
{$ENDIF}


constructor TSecureEdit.Create(AOwner: TComponent);
begin
  inherited;
  FSecureText := '';
  FSecureMode:= True;
  PasswordChar := '*';
  FIgnoringFakeKeys := False;
  FObfuscationEnabled:= True;
  FAllowPaste := True;
  Randomize;
end;


procedure TSecureEdit.SetSecureMode(Value: boolean);
begin
   if Value = FSecureMode then exit;

   if Value then begin
      FSecureText:= Text;
      PasswordChar := '*';
      Text := StringOfChar('*', Length(FSecureText));
      FSecureMode:= True;
   end
   else begin
      PasswordChar := #0;
      FSecureMode:= False;
      Text := FSecureText;
   end;

end;


procedure TSecureEdit.WMContextMenu(var Message: TWMContextMenu);
begin
  Message.Result := 1;       // Do not call inherited = do not show contextual menu
end;

procedure TSecureEdit.WMPaste(var Message: TMessage);
begin
  Message.Result := 0;       // Block the system's WM_PASTE message. We already handle this manually in KeyDown.
end;

procedure TSecureEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  if FIgnoringFakeKeys then begin
    Message.Result := 0;
    Exit;
  end;

  inherited;
end;

procedure TSecureEdit.KeyPress(var Key: Char);
var
  CursorPos: Integer;
begin
  if not FSecureMode then begin
     inherited;
     exit;
  end
  else
  if FIgnoringFakeKeys then begin
    Key := #0;
    Exit;
  end;


  CursorPos := SelStart;

  if Key = #8 then begin            // Backspace
     if SelLength > 0 then
       Delete(FSecureText, CursorPos + 1, SelLength)
     else if CursorPos > 0 then
       Delete(FSecureText, CursorPos, 1);

     inherited KeyPress(Key);

     SendFakeKeystrokes;            // Send fake keys after backspace
  end
  else if Key >= #32 then begin    // Normal character
     if SelLength > 0 then
        Delete(FSecureText, CursorPos + 1, SelLength);
     Insert(Key, FSecureText, CursorPos + 1);
     Key := '*';
     inherited KeyPress(Key);

     // THEN we send fake characters
     SendFakeKeystrokes;
  end
  else
     inherited KeyPress(Key);
end;


procedure TSecureEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  CursorPos: Integer;
  ClpStr: string;
begin
  if not FSecureMode then begin
     inherited;
     exit;
  end
  else
  if FIgnoringFakeKeys then begin
    Key := 0;
    Exit;
  end;

  CursorPos := SelStart;

  if Key = VK_DELETE then begin
     if SelLength > 0 then
       Delete(FSecureText, CursorPos + 1, SelLength)
     else if CursorPos < Length(FSecureText) then
       Delete(FSecureText, CursorPos + 1, 1);

     inherited;

     SendFakeKeystrokes;
     Exit;
  end
  else
  if FAllowPaste and
     ( ((ssCtrl in Shift) and (Key = Ord('V'))) or
      ((Key = VK_INSERT) and (shift = [ssShift])) ) then begin
     if Clipboard.HasFormat(CF_TEXT) then begin
        ClpStr := Clipboard.AsText;

        if SelLength > 0 then
           Delete(FSecureText, CursorPos + 1, SelLength);
        Insert(ClpStr, FSecureText, CursorPos + 1);

        Text := StringOfChar('*', Length(FSecureText));
        SelStart := CursorPos + Length(ClpStr);

        SendFakeKeystrokes;
        if Length(ClpStr) > 1 then
           SendFakeKeystrokes;
     end;
     Key := 0;
     exit;
  end;

  // Block Ctrl+A, Ctrl+C, Ctrl+X
  if (ssCtrl in Shift) and (Key in [Ord('A'), Ord('C'), Ord('X')]) then begin
     Key := 0;
     exit;
  end;


  inherited;
end;


procedure TSecureEdit.WMGetText(var Message: TMessage);
begin
  if FSecureMode then
     Message.Result := 0
  else
     inherited;
end;

procedure TSecureEdit.WMGetTextLength(var Message: TMessage);
begin
  if FSecureMode then
     Message.Result := 0
  else
     inherited;
end;


function TSecureEdit.GetSecureText: string;
begin
  if not FSecureMode then
     Result:= Text
  else
     Result := FSecureText;
end;

procedure TSecureEdit.ClearSecure;
begin
  if Length(FSecureText) > 0 then
     FillChar(FSecureText[1], Length(FSecureText) * SizeOf(Char), 0);
  FSecureText := '';
  Text := '';
end;


procedure TSecureEdit.SendFakeKeystrokes;
var
  i, NumFakes: Integer;
  FakeKey: Word;
begin
  if not FObfuscationEnabled then Exit;

  //FTestKeys:= FTestKeys + '|';

  if Random(10) < 2 then Exit;  // 20% of the time, send nothing.

  NumFakes := Random(4);        // 0-3 characters
  FIgnoringFakeKeys := True;

  try
      for i := 1 to NumFakes do  begin
        case Random(3) of
          0: FakeKey := Ord('A') + Random(26);   // A-Z
          1: FakeKey := Ord('0') + Random(10);   // 0-9
          2: FakeKey := VK_SPACE + Random(15);   // Various symbols
        end;

        PostKeyEx(Handle, FakeKey, [], False);
        //FTestKeys:= FTestKeys + Char(FakeKey);
      end;
      Application.ProcessMessages;

  finally
    FIgnoringFakeKeys := False;
  end;

end;


{$IFDEF INCLUDE_POSTKEYEX}

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
Procedure PostKeyEx( hWindow: HWnd; key: Word; Const shift: TShiftState; specialkey: Boolean );
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

{$ENDIF}


procedure Register;
begin
  RegisterComponents('KeyNoteNF', [TSecureEdit]);
end;


end.