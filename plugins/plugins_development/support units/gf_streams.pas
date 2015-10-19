unit gf_streams;

(* *********************************************************
 gf_streams - Streams handling routines
 --------------------------------------------------------
  Adapted from a set of routines by
  Ran Biron <Biron01@IBM.NET>
 --------------------------------------------------------
 Author : Marek Jedlinski
 E-mail : eristic@lodz.pdi.net
 URL    : http://www.lodz.pdi.net/~eristic/free/index.html
          http://go.to/generalfrenetics/
 --------------------------------------------------------
 This program (binary and source code) is freeware.
 It may be used, modified and distributed freely
 for non-comercial purposes. See LICENSE.TXT for details.

 The origin of this software must not be misrepresented.
 This notice must not be altered and must accompany all
 distributions of this source code.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. In no event
 shall the author of the softare, Marek Jedlinski, be held
 accountable for any damages or losses that may occur from use
 or misuse of the software.
 --------------------------------------------------------
 Note: This source code is poorly documented right now.
 Feel free to ask questions. See also "readme.txt" and
 "compile.txt"
 --------------------------------------------------------
 Version  : 1.0
 Released : 18 Oct 1999
********************************************************** *)


interface
uses Windows, Classes, Sysutils;

{$ALIGN OFF}          

procedure SaveStringToStream( S : string; Stream : TStream );
function LoadStringFromStream( Stream : TStream ) : string;
procedure SaveIntegerToStream( i : integer; Stream : TStream );
function LoadIntegerFromStream( Stream : TStream ) : Integer;
procedure SaveLongintToStream( i : integer; Stream : TStream );
function LoadLongintFromStream( Stream : TStream ) : longint;
procedure SaveFloatToStream( F : Real; Stream : TStream );
function LoadFloatFromStream( Stream : TStream ) : Real;
procedure SaveDateTimeToStream( TD : TDateTime; Stream : TStream );
function LoadDateTimeFromStream( Stream : TStream ) : TDateTime;
procedure SaveBLOBToStream( Dest, Source : TStream );
procedure LoadBLOBFromStream( Dest, Source : TStream );
procedure SaveBooleanToStream( B : Boolean; Stream : TStream );
function LoadBooleanFromStream( Stream : TStream ) : Boolean;


implementation

procedure SaveStringToStream( S : string; Stream : TStream );
var
 i : integer;
Begin
 i := Length( S );
 Stream.WriteBuffer( i, SizeOf( Integer ));
 if ( i > 0 ) then Stream.WriteBuffer( S[1], i ); 
End; 

function LoadStringFromStream( Stream : TStream ) : string; 
var
 i : integer; 
begin
 Stream.ReadBuffer( i, SizeOf( Integer )); 
 if ( i > 0 ) then
 begin
   SetLength( Result, i );
   Stream.ReadBuffer( Result[1], i );
 end
 else
   result := '';
End;

procedure SaveIntegerToStream( i : integer; Stream : TStream );
begin
 Stream.WriteBuffer( i, SizeOf( Integer ));
end;

function LoadIntegerFromStream( Stream : TStream ) : Integer;
Begin
 Stream.ReadBuffer( Result, SizeOf( integer ));
End;

procedure SaveLongintToStream( i : integer; Stream : TStream );
begin
  Stream.WriteBuffer( i, SizeOf( longint ));
end;

function LoadLongintFromStream( Stream : TStream ) : longint;
begin
  Stream.ReadBuffer( Result, SizeOf( longint ));
end;

procedure SaveFloatToStream( F : Real; Stream : TStream );
begin
 Stream.WriteBuffer( F, SizeOf( Real ));
End;

function LoadFloatFromStream( Stream : TStream ) : Real;
begin
 Stream.ReadBuffer( Result, SizeOf( Real ));
End;

procedure SaveDateTimeToStream( TD : TDateTime; Stream : TStream );
begin
 SaveFloatToStream( TD, Stream );
End;

function LoadDateTimeFromStream( Stream : TStream ) : TDateTime;
begin
 Result := LoadFloatFromStream( Stream );
End;

procedure SaveBLOBToStream( Dest, Source : TStream );
var
 i : integer;
begin
 Source.Position := 0;
 i := Source.Size;
 Dest.write( i, SizeOf( Longint ));
 Dest.CopyFrom( Source, i );
end;

procedure LoadBLOBFromStream( Dest, Source : TStream );
var
 i : integer;
begin
 Source.read( i, SizeOf( Longint ));
 Dest.Size := i;
 Dest.CopyFrom( Source, i );
 Dest.Position := 0;
end; 

procedure SaveBooleanToStream( B : Boolean; Stream : TStream ); 
begin
 Stream.WriteBuffer( B, SizeOf( Boolean )); 
end; 

function LoadBooleanFromStream( Stream : TStream ) : Boolean; 
begin
 Stream.ReadBuffer( Result, SizeOf( Boolean )); 
end;


end.
