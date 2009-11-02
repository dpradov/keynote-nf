unit gf_streams;
{$I gf_base.inc}
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

function NodeStreamIsRTF ( Stream : TMemoryStream ): boolean;
function NodeStreamIsUTF8_WithoutBOM (Stream : TMemoryStream; var NodeText: string): boolean; overload;
function NodeStreamIsUTF8_WithoutBOM ( Stream : TMemoryStream ): boolean; overload;
function AddUTF8_BOM ( Stream : TMemoryStream ): boolean;


type
  TWTextFile = class
  private
    F: TStream;
    fileName: wideString;
    buffer: PChar;
    bufSize: integer;
    posI, posF: integer;
  public
    constructor Create;
    destructor Destroy; Override;
    procedure AssignFile(const aFileName: wideString);
    procedure AssignStream(const stream: TStream);
    procedure Reset;
    procedure Rewrite;
    procedure CloseFile;
    function Readln: string;
    procedure WriteLn (const Args: array of const);
    function Eof: boolean;
  end;


implementation
uses TntSystem, TntClasses;

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

function LoadDateTimeFromStream( Stream : TStream ) : TDateTime;
var
  r : real;
begin
  r := LoadFloatFromStream( Stream );
  result := r;
End;

procedure SaveDateTimeToStream( TD : TDateTime; Stream : TStream );
begin
  SaveFloatToStream( TD, Stream );
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


function NodeStreamIsRTF ( Stream : TMemoryStream ): boolean;
var
    NodeText : string;
begin
    // From TForm_ExportNew.PerformExport:
    // <<now for some treachery. In KeyNote, a user can mark a note
    // as "plain text only". In such a node, all nodes are stored as
    // plain text, not RTF. However, the change from RTF to text (or
    // back) occurs only when a node is DISPLAYED. So, it is possible
    // that user enabled "plain text only", but some tree nodes have
    // not been viewed, hence are still in RTF. So, at this point
    // we cannot know if the node data we're about to export is RTF
    // or plain text data. Yet we must pass the correct information
    // to PutRichText. Which is why we must check manually, like so:

    Result:= False;
    if Stream.Size > 6 then begin
      // transfer stream contents to temp string
      SetLength( NodeText, 6 );
      move( Stream.Memory^, NodeText[1], 6 );
      Result := ( copy( NodeText, 1, 6 ) = '{\rtf1' );
    end;
end;

function NodeStreamIsUTF8_WithoutBOM (Stream : TMemoryStream; var NodeText: string): boolean;
var
    BOM: string[4];
    TextSize: integer;
begin
    // Node is assumed to be plain text
    Result:= False;

    TextSize:= Stream.Size;
    if TextSize >= 2 then begin
       SetLength( NodeText, TextSize );
       move( Stream.Memory^, NodeText[1], 3 );
       if Copy(NodeText,1,3) <> UTF8_BOM then begin
          move( Stream.Memory^, NodeText[1], TextSize );
          if Utf8ToAnsi(NodeText) <> NodeText then
             Result:= True;
          end;
    end;
end;

function NodeStreamIsUTF8_WithoutBOM ( Stream : TMemoryStream ): boolean;
var
    NodeText : string;
begin
    Result:= NodeStreamIsUTF8_WithoutBOM(Stream, NodeText);
end;

function AddUTF8_BOM ( Stream : TMemoryStream ): boolean;
var
    NodeText : string;
begin
    Result:= False;

    if NodeStreamIsUTF8_WithoutBOM(Stream, NodeText) then begin
       NodeText:= UTF8_BOM + NodeText;
       Stream.Position:= 0;
       Stream.Write(NodeText[1], length(NodeText));
       Stream.Position:= 0;
       Result:= True;
    end;
end;


//===== TWTextFile

constructor TWTextFile.Create;
begin
  posF:= 0;
end;

destructor TWTextFile.Destroy;
begin
    if assigned(buffer) then begin
       FreeMem(buffer);
       buffer:= nil;
    end;
end;

procedure TWTextFile.AssignFile(const aFileName: wideString);
begin
    if afileName = '' then
       raise Exception.CreateFmt( 'Error: Filename not specified', [''] );

    fileName:= aFileName;
    if assigned(F) then
       FreeAndNil(F);

    if assigned(buffer) then
       FreeMem(buffer);

    bufSize:= 1048576;  //1 MB
    GetMem(buffer, bufSize);
end;

procedure TWTextFile.AssignStream(const stream: TStream);
begin
    if (fileName <> '') and assigned(F) then
       FreeAndNil(F);

    F:= stream;
    fileName := '';

    if assigned(buffer) then
       FreeMem(buffer);

    bufSize:= 4096;     //4Kb
    GetMem(buffer, bufSize);
end;

procedure TWTextFile.Rewrite;
begin
    if assigned(F) then begin   //F will be a generic Stream, probably a TMemoryStream
       F.Position:= 0;
    end
    else begin
        if fileName = '' then
           raise Exception.CreateFmt( 'Error: Filename not specified', [''] )
        else
           F:= TTntFileStream.Create( fileName, ( fmCreate or fmShareExclusive ));
    end;

    posF:= 0;
end;

procedure TWTextFile.Reset;
begin
    if assigned(F) then begin  //F will be a generic Stream, probably a TMemoryStream
       F.Position:= 0;
    end
    else begin
        if fileName = '' then
           raise Exception.CreateFmt( 'Error: Filename not specified', [''] )
        else
           F:= TTntFileStream.Create( fileName, ( fmOpenRead ));
    end;

    if F.Size = 0 then
       posF:= 0
    else begin
       posI:= bufSize;
       posF:= bufSize-1;
    end;
end;

procedure TWTextFile.CloseFile;
begin
    if (fileName <> '') and assigned(F) then
       FreeAndNil(F);
end;

function TWTextFile.Readln: string;
var
   i: integer;
   cad: string;
   lineReaden: boolean;
   nCrLf: integer;
begin

   Result:= '';
   if not assigned (F) or (posF=0) then exit;

   lineReaden:= false;
   nCrLf:= 0;
   repeat
     i:= posI;
     if i <= posF then begin
       while (not lineReaden) and (i <= posF) do begin
           if (buffer[i] = #13) or (buffer[i] = #10) then begin
              nCrLf:= nCrLf + 1;
              if buffer[i] = #10 then
                 lineReaden:= True;
           end;
           i:= i + 1;
       end;
       SetString(cad, PChar(@buffer[posI]), i-posI - nCrLf);
       Result:= Result + cad;
     end;

     if i <= posF then begin
        posI:= i;
     end
     else
        if F.Position < F.Size then begin
           posF:= F.Read(buffer[0], bufSize) -1;
           posI:= 0;
        end
        else begin
           lineReaden:= true;    // We have reached final of the file
           posF:= 0;
           end;

   until lineReaden;
end;

function TWTextFile.Eof: boolean;
begin
   Result:= (posF = 0);
end;


procedure TWTextFile.WriteLn (const Args: array of const);
var
  I: Integer;
  line: string;
  wline: WideString;
  lastParamWide: boolean;
  x: PShortString;
  ws: wideString;
  s: string;

  procedure checkToWrite (force: boolean = false);
  var
     S: AnsiString;
  begin
    if (wline <> '') and (not lastParamWide or force) then begin
        S:= string(wline);                   // To make it more compatible with older versions. Only use UTF8 if it's necessary
        if wline <> WideString(S) then
           S:= WideStringToUTF8(wline);

        F.WriteBuffer(PChar(S)^, length(S));
        wline:= '';
    end;
    if (line <> '') and (lastParamWide or force) then begin
        F.WriteBuffer(PChar(line)^, length(line));
        line:= '';
    end;
  end;

begin
  if not assigned (F) then exit;

  for I := Low(Args) to High (Args) do begin
    lastParamWide:= False;
    case Args [I].VType of
      vtInteger:  line := line + intToStr(Args [I].VInteger);
      vtChar:     line := line + Args [I].VChar;
      vtBoolean:  line := line + 'TRUE';   //Args [I].VBoolean;

      vtExtended:   line := line + FloatToStr(Args [I].VExtended^);
      vtInt64:      line := line + FloatToStr(Args [I].VInt64^);
      vtPChar:      line := line + PChar(Args [I].VPChar)^;
      vtString:     line := line + PShortString(Args [I].VString)^;
      vtAnsiString: line := line + string(Args [I].VAnsiString);
      vtWideChar:   begin
                    wline:= wline + Args [I].VWideChar;
                    lastParamWide:= true;
                    end;
      vtPWideChar,
      vtWideString: begin
                    ws:= WideString(Args [I].VPWideChar);
                    s:= string(ws);
                    if s = ws then
                       line := line + s
                    else begin
                        wline:= wline + ws;
                        lastParamWide:= true;
                        end;
                    end;
      vtCurrency:   line := line + CurrToStr(Args [I].VCurrency^);
    end; // case
    checkToWrite;
  end;
  line:= line + #13#10;
  checkToWrite (true);
end;

end.
