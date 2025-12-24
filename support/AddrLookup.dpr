(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2025 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 
 Based on JCL: JEDI Code Library (https://github.com/project-jedi/jcl)
******************************************************************************

  AddrLookup - Post-mortem address analyzer
  Usage: AddrLookup <mapfile> <baseaddr> <erroraddr>

    <mapfile>   : Path to .MAP file (generated with Detailed mode)
    <baseaddr>  : Module base address (hex, without $)
    <erroraddr> : Exception address (hex, without $)

  Example: AddrLookup MyApp.map 880000 D35D1E

------------------------------------------------------------------------------
*)

program AddrLookup;

{$R *.res}
{$APPTYPE CONSOLE}

uses
  Windows,
  System.SysUtils,
  System.Classes,
  JclDebug;

type
  TMapSymbol = record
    Segment: Integer;
    Offset: DWORD;
    FullName: string;
    UnitName: string;
    ProcName: string;
  end;

var
  MapFile: string;
  BaseAddr: DWORD;
  AbsAddr: DWORD;
  ModuleCodeOffset: DWORD;
  Symbols: array of TMapSymbol;

function ParseHexValue(const S: string): DWORD;
begin
  Result := StrToInt('$' + S);
end;

procedure ParseMapFile;
var
  Lines: TStringList;
  i: Integer;
  Line, SegStr, OffStr, Name: string;
  InSymbols: Boolean;
  SpacePos, ColonPos, DotPos: Integer;
  SegVal: Integer;
  OffVal: DWORD;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(MapFile);
    InSymbols := False;
    ModuleCodeOffset := 0;
    SetLength(Symbols, 0);

    for i := 0 to Lines.Count - 1 do begin
      Line := Trim(Lines[i]);

      // Look for the CODE segment offset
      if (ModuleCodeOffset = 0) and (Pos('.text', Line) > 0) and (Pos('CODE', Line) > 0) then begin
        // Format: " 0001:00401000 00501030H .text                   CODE"
        // The offset is 00401000, but the actual ModuleCodeOffset is only the last 4 hex digits
        // because 00400000 is the segment base
        SpacePos := Pos(':', Line);
        if SpacePos > 0 then begin
          Delete(Line, 1, SpacePos);
          Line := Trim(Line);
          SpacePos := Pos(' ', Line);
          if SpacePos > 0 then begin
            OffStr := Copy(Line, 1, SpacePos - 1);
            try
              ModuleCodeOffset := ParseHexValue(OffStr) and $FFFF;
            except
              // Ignore errors
            end;
          end;
        end;
      end;

      // Detect start of symbols section
      if (Length(Line) > 4) and (Copy(Line, 1, 4) = '0001') and (Pos(':', Line) > 0) then
        InSymbols := True;

      // Parse symbols
      if InSymbols and (Length(Line) > 0) and (Line[1] in ['0'..'9']) then begin
        ColonPos := Pos(':', Line);
        if ColonPos > 0 then begin
          SegStr := Copy(Line, 1, ColonPos - 1);
          Delete(Line, 1, ColonPos);
          Line := Trim(Line);

          SpacePos := Pos(' ', Line);
          if SpacePos > 0 then begin
            OffStr := Copy(Line, 1, SpacePos - 1);
            Name := Trim(Copy(Line, SpacePos + 1, MaxInt));

            if (Name <> '') and (Pos('.', Name) > 0) then begin
              try
                SegVal := StrToInt('$' + SegStr);
                OffVal := ParseHexValue(OffStr);

                SetLength(Symbols, Length(Symbols) + 1);
                Symbols[High(Symbols)].Segment := SegVal;
                Symbols[High(Symbols)].Offset := OffVal;
                Symbols[High(Symbols)].FullName := Name;

                // Extract UnitName and ProcName
                DotPos := LastDelimiter('.', Name);
                if DotPos > 0 then begin
                  Symbols[High(Symbols)].UnitName := Copy(Name, 1, DotPos - 1);
                  Symbols[High(Symbols)].ProcName := Copy(Name, DotPos + 1, MaxInt);
                end
                else begin
                  Symbols[High(Symbols)].UnitName := '';
                  Symbols[High(Symbols)].ProcName := Name;
                end;
              except
                // Ignore lines that cannot be parsed
              end;
            end;
          end;
        end;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

function FindSymbolForRVA(RVA: DWORD): Integer;
var
  i, BestIdx: Integer;
  BestDist: Int64;
  Dist: Int64;
begin
  Result := -1;
  BestIdx := -1;
  BestDist := High(Int64);

  for i := 0 to High(Symbols) do begin
    if (Symbols[i].Segment = 1) and (Symbols[i].Offset <= RVA) then begin
      Dist := RVA - Symbols[i].Offset;
      if Dist < BestDist then begin
        BestDist := Dist;
        BestIdx := i;
      end;
    end;
  end;

  Result := BestIdx;
end;

procedure LookupAddress;
var
  VA, RVA: DWORD;
  SymIdx: Integer;
  LocationInfo: TJclLocationInfo;
  MapScanner: TJclMapScanner;
  Offset: DWORD;
begin
  // Calculate VA and RVA
  VA := AbsAddr - BaseAddr;
  RVA := VA - ModuleCodeOffset;

  WriteLn('=== Address Lookup ===');
  WriteLn('Map File:      ', MapFile);
  WriteLn('Base Address:  $', IntToHex(BaseAddr, 8));
  WriteLn('Error Address: $', IntToHex(AbsAddr, 8));
  WriteLn('VA:            $', IntToHex(VA, 8));
  WriteLn('Code Offset:   $', IntToHex(ModuleCodeOffset, 8));
  WriteLn('RVA:           $', IntToHex(RVA, 8));
  WriteLn;

  // Method 1: Manual lookup in parsed symbols
  WriteLn('--- Manual Symbol Lookup ---');
  SymIdx := FindSymbolForRVA(RVA);
  if SymIdx >= 0 then begin
    Offset := RVA - Symbols[SymIdx].Offset;
    WriteLn('Procedure: ', Symbols[SymIdx].FullName);
    WriteLn('Offset:    +$', IntToHex(Offset, 1), ' (', Offset, ' bytes)');
  end
  else
    WriteLn('Symbol not found');

  WriteLn;

  // Method 2: Use JCL
  WriteLn('--- JCL Debug Info ---');
  try
    MapScanner := TJclMapScanner.Create(MapFile, 0);
    try
      FillChar(LocationInfo, SizeOf(LocationInfo), 0);

      if MapScanner.LineNumberFromAddr(RVA) > 0 then begin
        LocationInfo.UnitName := MapScanner.ModuleNameFromAddr(RVA);
        LocationInfo.ProcedureName := MapScanner.ProcNameFromAddr(RVA, LocationInfo.OffsetFromProcName);
        LocationInfo.LineNumber := MapScanner.LineNumberFromAddr(RVA, LocationInfo.OffsetFromLineNumber);
        LocationInfo.SourceName := MapScanner.SourceNameFromAddr(RVA);

        WriteLn('Unit:      ', LocationInfo.UnitName);
        WriteLn('Procedure: ', LocationInfo.ProcedureName);
        WriteLn('Source:    ', LocationInfo.SourceName);
        WriteLn('Line:      ', LocationInfo.LineNumber, ' +', LocationInfo.OffsetFromLineNumber);
        WriteLn('Offset:    +$', IntToHex(LocationInfo.OffsetFromProcName, 1));
        WriteLn;
        WriteLn('Full location:');
        WriteLn(Format('%s.%s (Line %d, "%s" + %d)',
          [LocationInfo.UnitName, LocationInfo.ProcedureName,
           LocationInfo.LineNumber, LocationInfo.SourceName,
           LocationInfo.OffsetFromLineNumber]));
      end
      else begin
        WriteLn('Line number information not available in MAP file.');
        WriteLn('(MAP file must be generated with Detailed debug info)');
      end;

    finally
      MapScanner.Free;
    end;

  except
    on E: Exception do
      WriteLn('Error using JCL: ', E.Message);
  end;
end;

procedure ShowUsage;
begin
  WriteLn('AddrLookup - Post-mortem address analyzer');
  WriteLn('Usage: AddrLookup <mapfile> <baseaddr> <erroraddr>');
  WriteLn;
  WriteLn('  <mapfile>   : Path to .MAP file (generated with Detailed mode)');
  WriteLn('  <baseaddr>  : Module base address (hex, without $)');
  WriteLn('  <erroraddr> : Exception address (hex, without $)');
  WriteLn;
  WriteLn('Example: AddrLookup MyApp.map 880000 D35D1E');
  Halt(1);
end;

begin
  try
    WriteLn;
    if ParamCount <> 3 then
      ShowUsage;

    MapFile := ParamStr(1);
    if not FileExists(MapFile) then begin
      WriteLn('ERROR: Map file not found: ', MapFile);
      Halt(1);
    end;

    try
      BaseAddr := ParseHexValue(ParamStr(2));
      AbsAddr := ParseHexValue(ParamStr(3));
    except
      WriteLn('ERROR: Invalid hex address format');
      ShowUsage;
    end;

    ParseMapFile;

    if ModuleCodeOffset = 0 then begin
      WriteLn('WARNING: Could not determine ModuleCodeOffset from MAP file.');
      WriteLn('Assuming default offset of $1000');
      ModuleCodeOffset := $1000;
      WriteLn;
    end;

    LookupAddress;

  except
    on E: Exception do begin
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
