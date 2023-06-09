(* Run-time redirection of a function calls inside of the executable module.
  Attention: this unit is full of dirty hacks. :)
  Copyright (C) 2006 - 2018 Kryvich, Belarusian Linguistic Software team.
*)

unit KDL.Detours;

{$IF NOT DEFINED(MSWINDOWS)}
  {$Message Fatal 'Only Windows platform is supported,'
    + ' If you can do it on other platforms - tell me how.'}
{$IFEND}

interface

type
   // Dump of original func, pointers to Original & New funcs
  TFuncReplacement = class
  private
    var OrigDump: packed array[0..4] of Byte;
    OrigFunc, MyFunc: Pointer;
    fReplaced: Boolean; // Is func replaced now

    procedure SetReplaced(aReplaced: Boolean);
  public
    constructor Create(aOrigFunc, aMyFunc: Pointer);
    destructor Destroy; override;
    property Replaced: Boolean read fReplaced write SetReplaced;
  end;


implementation

uses
  SysUtils, Windows;

type
//  Used for Windows 95
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean; // copied from JclPeImage.pas
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@Exit
@@NoThunk:
  XOR EAX, EAX
@@Exit:
end;

type
  // Used for packages
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    // FF25B8A9BD02     jmp dword ptr [$02bda9b8]                      -- x86
    // 0000000000E31380 FF2522D00000     jmp qword ptr [rel $0000d022] -- x64
    OpCode: Word;
    Addr: UInt32;
  end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then begin
    if (Win32Platform <> VER_PLATFORM_WIN32_NT)
      and IsWin9xDebugThunk(Proc)
    then
      Proc := PWin9xDebugThunk(Proc).Addr; // !! not tested for x64
    if PAbsoluteIndirectJmp(Proc).OpCode = $25FF then // JMP mem32
      // It's possible in packages
      Result := PPointer(
        {$IFDEF CPUX64} NativeInt(Proc) + 6 + {$ENDIF} // FF /4 jmp r/m64
        PAbsoluteIndirectJmp(Proc).Addr)^
    else
      Result := Proc;
  end else
    Result := nil;
end;

{ TFuncReplacement }

constructor TFuncReplacement.Create(aOrigFunc, aMyFunc: Pointer);
var
  OldProtect: DWORD;
begin
  OrigFunc := GetActualAddr(aOrigFunc);
  MyFunc := aMyFunc;
  Move(OrigFunc^, OrigDump[0], 5);
  VirtualProtect(OrigFunc, 5, PAGE_EXECUTE_READWRITE,
    @OldProtect);
end;

destructor TFuncReplacement.Destroy;
begin
  SetReplaced(False);
  inherited;
end;

procedure TFuncReplacement.SetReplaced(aReplaced: Boolean);
var
  Offset: Int32;
begin
  if aReplaced = fReplaced then
    Exit;
  if aReplaced then begin // Set MyFunc
    Offset := NativeInt(MyFunc) - NativeInt(OrigFunc) - 5;
    Byte(OrigFunc^) := $E9;
    Move(Offset, Pointer(NativeInt(OrigFunc)+1)^, 4);
  end else // Set OrigFunc
    Move(OrigDump[0], OrigFunc^, 5);
  fReplaced := aReplaced;
end;

end.
