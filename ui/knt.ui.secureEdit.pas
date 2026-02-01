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
   Vcl.StdCtrls,
   Vcl.Forms,
   Vcl.Dialogs;

type
  TSecureEdit = class(TEdit)
  private
    FSecureMode: boolean;
    FSecureText: string;
    procedure WMGetText(var Message: TMessage); message WM_GETTEXT;
    procedure WMGetTextLength(var Message: TMessage); message WM_GETTEXTLENGTH;
    //procedure WMPaste(var Message: TMessage); message WM_PASTE;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetSecureMode(Value: boolean);

  public
    property SecureMode: boolean read FSecureMode write SetSecureMode;
    function GetSecureText: string;
    procedure ClearSecure;
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation


constructor TSecureEdit.Create(AOwner: TComponent);
begin
  inherited;
  FSecureText := '';
  FSecureMode:= True;
  PasswordChar := '*';
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


procedure TSecureEdit.KeyPress(var Key: Char);
var
  CursorPos: Integer;
begin
  if not FSecureMode then begin
     inherited;
     exit;
  end;

  CursorPos := SelStart;

  if Key = #8 then begin            // Backspace
     if SelLength > 0 then
       Delete(FSecureText, CursorPos + 1, SelLength)
     else if CursorPos > 0 then
       Delete(FSecureText, CursorPos, 1);
     Key := #0;
     inherited KeyPress(Key);
  end
  else if Key >= #32 then begin    // Carácter normal
     if SelLength > 0 then begin
       Delete(FSecureText, CursorPos + 1, SelLength);
       Insert(Key, FSecureText, CursorPos + 1);
     end
     else
       Insert(Key, FSecureText, CursorPos + 1);
     Key := '*';
     inherited KeyPress(Key);
  end
  else
     inherited KeyPress(Key);
end;


procedure TSecureEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  CursorPos: Integer;
begin
  if not FSecureMode then begin
     inherited;
     exit;
  end;

  CursorPos := SelStart;

  if Key = VK_DELETE then begin
     if SelLength > 0 then
       Delete(FSecureText, CursorPos + 1, SelLength)
     else if CursorPos < Length(FSecureText) then
       Delete(FSecureText, CursorPos + 1, 1);

     inherited;
     Exit;
  end;

  // Block clipboard paste?
  {
  if (ssCtrl in Shift) and (Key in [Ord('C'), Ord('V'), Ord('X')]) then begin
     Key := 0;
     Exit;
  end;
  }

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

// Block clipboard paste?
{
procedure TSecureEdit.WMPaste(var Message: TMessage);
begin
  if FSecureMode then
     Message.Result := 0
  else
     inherited;
end;
}

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




procedure Register;
begin
  RegisterComponents('KeyNoteNF', [TSecureEdit]);
end;


end.