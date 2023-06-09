{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Franois PIETTE
Description:  TBuffer is an object wich buffers data in a single dynamically
              allocated memory block. It is a kind of FIFO wich manages
              characters in bloc of various sizes.
EMail:        http://users.swing.be/francois.piette  francois.piette@swing.be
              http://www.rtfm.be/fpiette             francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Creation:     April 1996
Version:      2.01
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997-2000 by Franois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

              This software is provided 'as-is', without any express or
  	      implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment 
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.
                 
              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Mar 06, 1998  V2.00 Added a property and a parameter for the create method
              to select the buffer size. Using a 0 value will make the object
              use the default 1514 bytes (the largest size for an ethernet
              packet).
Jul 08, 1998  V2.01 Adadpted for Delphi 4


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit WSockBuf;

interface

uses
  SysUtils;

const
  WSockBufVersion = 201;

type
  TBuffer = class(TObject)
    Buf      : Pointer;
    FBufSize : Integer;
    WrCount  : Integer;
    RdCount  : Integer;
  public
    constructor Create(nSize : Integer); virtual;
    destructor  Destroy; override;
    function    Write(Data : Pointer; Len : Integer) : Integer;
    function    Read(Data : Pointer; Len : Integer) : Integer;
    function    Peek(var Len : Integer) : Pointer;
    function    Remove(Len : Integer) : Integer;
    procedure   SetBufSize(newSize : Integer);
    property    BufSize : Integer read FBufSize write SetBufSize;
  end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBuffer.Create(nSize : Integer);
begin
    inherited Create;
    WrCount  := 0;
    RdCount  := 0;
    BufSize := nSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TBuffer.Destroy;
begin
    if Assigned(Buf) then
        FreeMem(Buf, FBufSize);

    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBuffer.SetBufSize(newSize : Integer);
var
    newBuf : Pointer;
begin
    if newSize <= 0 then
        newSize := 1514;

    if newSize = FBufSize then
        Exit;

    if WrCount = RdCount then begin
        { Buffer is empty }
        if Assigned(Buf) then
            FreeMem(Buf, FBufSize);
        FBufSize := newSize;
        GetMem(Buf, FBufSize);
    end
    else begin
        { Buffer contains data }
        GetMem(newBuf, newSize);
        Move(Buf^, newBuf^, WrCount);
        if Assigned(Buf) then
            FreeMem(Buf, FBufSize);
        FBufSize := newSize;
        Buf      := newBuf;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Write(Data : Pointer; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
begin
    Remaining := FBufSize - WrCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
        Move(Data^, (PChar(Buf) + WrCount)^, Copied);
        WrCount := WrCount + Copied;
        Result  := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Read(Data : Pointer; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
        Move((PChar(Buf) + RdCount)^, Data^, Copied);
        RdCount := RdCount + Copied;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Peek(var Len : Integer) : Pointer;
var
    Remaining : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        Len    := 0;
        Result := nil;
    end
    else begin
        Len    := Remaining;
        Result := PChar(Buf) + RdCount;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Remove(Len : Integer) : Integer;
var
    Remaining : Integer;
    Removed   : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len < Remaining then
            Removed := Len
        else
            Removed := Remaining;
        RdCount := RdCount + Removed;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Removed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.