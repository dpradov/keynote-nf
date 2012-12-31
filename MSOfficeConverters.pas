unit MSOfficeConverters;

{ -----------------------------------------------------------------------------}
{ Modified by Daniel Prado <dprado.keynote@gmail.com> on Dec 2012              }
{ -----------------------------------------------------------------------------}
{ Modified by Marek Jedlinski <eristic@lodz.pdi.net> on Jan 8, 2002            }
{ -----------------------------------------------------------------------------}
{ Unit: Converters                                                             }
{ Purpose: import and export to Office known formats                           }
{ Required: the TRichEdit98. See enclosed zip. This demo could also use        }
{           an imported RICHTX32.OCX, but you'll have to modify the form for   }
{           this to work.                                                      }
{ Authors: Fred Jansma, Paul te Bokkel                                         }
{          Incontrol Business Engineers                                        }
{ -----------------------------------------------------------------------------}

{ Copyright (c) 1998 Paul te Bokkel & Fred Jansma, Incontrol Business Engineers}

{ This program is free software; you can redistribute it and/or                }
{ modify it under the terms of the GNU General Public License                  }
{ as published by the Free Software Foundation; either version 2               }
{ of the License, or (at your option) any later version.                       }

{ This program is distributed in the hope that it will be useful,              }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of               }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                }
{ GNU General Public License for more details.                                 }

{ You should have received a copy of the GNU General Public License            }
{ along with this program; if not, write to the Free Software                  }
{ Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.  }


interface

uses
  Registry, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

// -----------------------------------------------------------------
// Functions to call when you want to convert something
// -----------------------------------------------------------------

// Function to import a file to a WideString.
function TextConvImportAsRTF(FileName: String; Converter: String; aStream : TStream; ConverterLocation : string ): Boolean;

// Function to export RTF to a file.
function TextConvExportRTF(FileName: String; Converter: String; aRTFText : PChar; ConverterLocation : string ): Boolean;

// -----------------------------------------------------------------
// Functions to call if you want to know what can be converted
// -----------------------------------------------------------------

// Function to build a filter list, with available import and export converters.
function BuildFilterList(ForImport: Boolean): string;

// Function to build a list with available conveters.
function BuildConverterList(ForImport: Boolean; StrLst: TStringList): Boolean;

// -----------------------------------------------------------------
// Supportive functions - not to be called direct, unless you
// know what you are doing
// -----------------------------------------------------------------

// Initialize the selected converter.
function LoadConverter(Name: string; Import: boolean; ConverterLocation : string): HWND;

// Check if current file is of right format.
function IsKnownFormat(FileName: string): Boolean;

// Convert string to HGLOBAL.
function StringToHGLOBAL(const str: string): HGLOBAL;

// Procedure to free the converters.
procedure DoFreeConverters;

// Function to obtain the directory of WordPad. Not needed to be public, but maybe
// someone is looking for it.
function WordPadDir: string;

// Callback functions for reading and writing.
function Reading(CCH, nPercentComplete: integer): Integer; stdcall;
function Writing(flags, nPercentComplete: integer): Integer; stdcall;


implementation

  // Return Codes from the converter, Defined by the interface.
const                                   // File Conversion Errors.
      fceTrue           =  1;   // IsFormatCorrect32 recognized the input file
      fceNoErr    	=  0;   // success
      fceOpenInFileErr	= -1;	// could not open input file
      fceReadErr	= -2;	// error during read
      fceOpenConvErr	= -3;	// error opening conversion file
      fceWriteErr	= -4;	// error during write
      fceInvalidFile	= -5;	// invalid data in conversion file
      fceOpenExceptErr	= -6;   // error opening exception file
      fceWriteExceptErr	= -7;	// error writing exception file
      fceNoMemory	= -8;	// out of memory
      fceInvalidDoc	= -9;	// invalid document
      fceDiskFull	= -10;	// out of space on output
      fceDocTooLarge	= -11;	// conversion document too large for target
      fceOpenOutFileErr	= -12;	// could not open output file
      fceUserCancel	= -13;	// conversion cancelled by user
      fceWrongFileType	= -14;	// wrong file type for this converter

type
  // Our functions to convert the RTF-format to a foreign format, or a foreign format to the RTF-format.
  // These functions are hidden in the converters.
  RTF_CALLBACK = function(CCH, nPercentComplete: integer): Integer; stdcall;
  TInitConverter = function(ParentWin: THandle; ParentAppName: LPCSTR): integer; stdcall;
  TUninitConverter = procedure(); stdcall;
  TIsFormatCorrect = function(FileName, Desc: HGLOBAL): integer; stdcall;
  TForeignToRtf = function(FileName: HGLOBAL; void: pointer{LPSTORAGE}; Buf, Desc, Subset: HGLOBAL; Callback: RTF_CALLBACK): integer; stdcall;
  TRtfToForeign = function(FileName: HGLOBAL; void: pointer{LPSTORAGE}; Buf, Desc: HGLOBAL; Callback: RTF_CALLBACK): integer; stdcall;

var
  CurrentConverter: HWND;
  InitConverter: TInitConverter = nil;
  UninitConverter: TUninitConverter = nil;
  IsFormatCorrect: TIsFormatCorrect = nil;
  ForeignToRtf: TForeignToRtf = nil;
  RtfToForeign: TRtfToForeign = nil;
  hBuf: HGLOBAL;
  bytesRead: integer = 0;
  WritePos: integer = 0; // in Char, not WChar
  WriteMax: integer = 0; // in Char, not WChar
  RTFToWrite: string;
  mstream: TMemorystream = nil;

const
  nBufSize: integer = 4096;
  MSTextConvKey = 'SOFTWARE\Microsoft\Shared Tools\Text Converters\';


function TextConvImportAsRTF(FileName: String; Converter: String; aStream : TStream; ConverterLocation : string): Boolean;
var
  // Variables used for the actual conversion.
  hSubset,
  hFileName,
  hDesc: HGLOBAL;
  res: integer;
begin
  Result := False; // We are very pessimistic.
  try
    if LoadConverter(Converter, True, ConverterLocation ) <> 0 then begin

        if not IsKnownFormat(FileName) then        // Check selected file format.
            ShowMessage('Incorrect file format.')

        else begin
            // prepare parameters
            hSubset := StringToHGLOBAL('');
            hDesc := StringToHGLOBAL('');

            hFileName := StringToHGLOBAL(FileName);

            hBuf := GlobalAlloc(GHND, nBufSize + 1);

            try
              Screen.Cursor := crHourGlass;

              mstream := TMemoryStream.Create;
              mstream.SetSize(512 * 1024); // initial: 512 kB, seems reasonable.

              if Assigned(ForeignToRtf) then
                 res := ForeignToRtf(hFileName, nil, hBuf, hDesc, hSubset, Reading)
              else
                 res := -1; // no valid entry point for DLL

              if res = fceNoErr then begin
                  mstream.SetSize(mstream.Position); // shrink to right size
                  mstream.Position := 0;

                  aStream.Position := 0;
                  mstream.SaveToStream( aStream );

                  // rtbApp.Lines.LoadFromStream(mStream);
                  mStream.Free;

                  Result := True;
                  end
              else
                  Result := False;

            finally
              GlobalFree(hBuf);
              GlobalFree(hFileName);
              GlobalFree(hDesc);
              GlobalFree(hSubset);
              Screen.Cursor := crDefault;

              UninitConverter();
            end;
          end;
      end;

  except
    Result := False;
  end;
end;


function TextConvExportRTF(FileName: String; Converter: String; aRTFText : PChar; ConverterLocation : string ): Boolean;
var
  hSubset,
  hFileName,
  hDesc: HGLOBAL;
  res: integer;
begin
  if LoadConverter(Converter, False, ConverterLocation ) <> 0 then
    begin
      if not (Assigned(InitConverter)
         and LongBool(InitConverter(Application.Handle, PChar(Uppercase(Application.ExeName))))) then
        begin
          ShowMessage('InitConverter failed');
          Result := False;
          Exit;
        end;

      hSubset := StringToHGLOBAL('');
      hDesc := StringToHGLOBAL('');
      hFileName := StringToHGLOBAL(FileName);
      hBuf := GlobalAlloc(GHND, nBufSize + 1);

      try
        Screen.Cursor := crHourGlass;
        WritePos := 0;

        if Assigned(RtfToForeign) then
          begin

            RTFToWrite := aRTFText;
            WriteMax := length(RTFToWrite);

            res := RtfToForeign(hFileName, nil, hBuf, hDesc, Writing);

            RTFToWrite := '';

            Result:= (res = fceNoErr);
          end
        else
          begin
            ShowMessage('Coult not export document');
            Result := False;
          end;

      finally
        GlobalFree(hBuf);
        GlobalFree(hFileName);
        GlobalFree(hDesc);
        GlobalFree(hSubset);
        Screen.Cursor := crDefault;

        UninitConverter();
      end;
    end
  else
    Result := False;
end;


// Try to obtain the WordPad directory.
function WordPadDir: string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;

  with reg do
    try
      RootKey := HKEY_LOCAL_MACHINE;

      Result := '';

      if OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\WORDPAD.EXE', False) then
        Result := ReadString('');

      Result := ExtractFilePath(Result); // leaves the trailing backslash intact
    finally
      Free;
    end;
end;


// The function "BuildFilterList" creates a string, that we can use as a filterlist
// in the dialog.
function BuildFilterList(ForImport: Boolean): string;
const
  saImEx: array[False..True] of string = ('Export', 'Import');
var
  regTxtConv: TRegistry;
  regConvEntry: TRegistry;
  slEntries: TStringList;
  x,
  i: integer;
  extensions: string;
begin
  regTxtConv := TRegistry.Create;
  regConvEntry := TRegistry.Create;
  slEntries := TStringList.Create;
  Result := '';

  with regTxtConv do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(MSTextConvKey + saImEx[ForImport], False) then
        GetKeyNames(slEntries);
    finally
      CloseKey;
      Free;
    end;

  regConvEntry.RootKey := HKEY_LOCAL_MACHINE;
  try
    for i := 0 to slEntries.Count-1 do
      begin
        if regConvEntry.OpenKey(MSTextConvKey + saImEx[ForImport] + '\' + slEntries[i], false) then
          try
            extensions := '*.' + regConvEntry.ReadString('Extensions');
            x := pos(' ', extensions);
            while x > 0 do
              begin
                delete(extensions, x, 1);
                insert(';*.', extensions, x);
                x := pos(' ', extensions);
              end;
            Result := Result + regConvEntry.ReadString('Name') + '|' + extensions + '|';
          except
            // catch a faulty key mismatch
          end;
        regConvEntry.CloseKey;
      end;
  finally
    regConvEntry.Free;
    slEntries.Free;
  end;

  // We can always im/export RTF, but there is no (trivial) converter for,
  // but we do want to show it...
  Result := 'Rich Text Format|*.rtf|' + Result;

  // Since the WordPad converters are identically to the Office converters,
  // we test if WordPad is installed and if so, if we did find Word and/or
  // Write format. If not, add them.
  // NB: these are valid for both im- and export.
  if WordPadDir <> '' then
  begin
    if (pos('Windows Write', Result) = 0) and
       (FileExists(WordPadDir + 'write32.wpc')) then
      Result := 'Windows Write|*.wri|' + Result;
    if FileExists(WordPadDir + 'mswd6_32.wpc') then // do we have W6 converter?
      if (pos('Word 6.0/95', Result) = 0) then
        // no office converter for W6, add Wordpad as default converter
        Result := 'Word 6.0/95|*.doc|' + Result
      else
        // Since Office converter seems buggy, add Wordpad converter as default
        Result := 'Word 95 via WordPad|*.doc|' + Result;
  end;

  if (Result <> '') and (Result[Length(Result)] = '|') then
    Delete(Result, Length(Result), 1); // strip trailing |
end;


// The function "BuildConverterList" creates a stringlist, in which all available
// converters are stored.
function BuildConverterList(ForImport: Boolean; StrLst: TStringList): Boolean;
const
  saImEx: array[False..True] of string = ('Export', 'Import');
var
  regTxtConv: TRegistry;
  regConvEntry: TRegistry;
  slEntries: TStringList;
  i: integer;
begin
  if not Assigned(StrLst) then
    begin
      Result := False;
      Exit; // StrLst must be initialized.
    end;

  regTxtConv := TRegistry.Create;
  regConvEntry := TRegistry.Create;
  slEntries := TStringList.Create;

  try
    with regTxtConv do
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey(MSTextConvKey + saImEx[ForImport], False) then
          GetKeyNames(slEntries);
      finally
        CloseKey;
        Free;
      end;

    regConvEntry.RootKey := HKEY_LOCAL_MACHINE;

    try
      for i := 0 to slEntries.Count-1 do
        begin
          if regConvEntry.OpenKey(MSTextConvKey + saImEx[ForImport] + '\' + slEntries[i], False) then
            try
              StrLst.Add(regConvEntry.ReadString('Name'));
            except
              // catch a faulty key mismatch
            end;
          regConvEntry.CloseKey;
        end;
    finally
      regConvEntry.Free;
      slEntries.Free;
    end;

    // We can always im/export RTF, but there is no (trivial) converter for,
    // but we do want to show it...
    StrLst.Insert(0, 'Rich Text Format');

    // Since the WordPad converters are identically to the Office converters,
    // we test if WordPad is installed and if so, if we did find Word and/or
    // Write format. If not, add them.
    // NB: these are valid for both im- and export.
    if WordPadDir <> '' then
      begin
        if (pos('Windows Write', StrLst.Text) = 0) and
           (FileExists(WordPadDir + 'write32.wpc')) then
           StrLst.Insert(0, 'Windows Write');
        if FileExists(WordPadDir + 'mswd6_32.wpc') then // do we have W6 converter?
          if (pos('Word 6.0/95', StrLst.Text) = 0) then
            // no office converter for W6, add Wordpad as default converter
            StrLst.Insert(0, 'Word 6.0/95')
          else
            // Since Office converter seems buggy, add Wordpad converter as default
            StrLst.Insert(0, 'Word 95 via WordPad');
      end;

    Result := True;
  except
    Result := False;
  end;
end;


// The function "LoadConverter" loads a specific converter.
// We set the converter-functions as well.
function LoadConverter(Name: string; Import: boolean; ConverterLocation : string): HWND;
const
  saImEx: array[false..true] of string = ('Export', 'Import');
var
  regConvEntry: TRegistry;
  i: integer;
  ConverterDLL: string;
begin
  regConvEntry := TRegistry.Create;

  Result := 0;
  ConverterDLL := '';

  if ( ConverterLocation <> '' ) then
    ConverterDLL := ConverterLocation

  else begin
      try
          regConvEntry.RootKey := HKEY_LOCAL_MACHINE;
          if regConvEntry.OpenKey(MSTextConvKey + saImEx[Import] + '\' + Name, False) then begin
             ConverterDLL := regConvEntry.ReadString('Path'); // get dll-location & name
             regConvEntry.CloseKey;
          end;
      finally
        regConvEntry.Free;
      end;


      if ConverterDLL = '' then // It could be a Wordpad provided converter
      begin
        if pos('Word 6.0/95', Name) > 0 then
          ConverterDLL := WordPadDir + 'mswd6_32.wpc'
        else if pos('Windows Write', Name) > 0 then
          ConverterDLL := WordPadDir + 'write32.wpc'
        else if pos('WordPad', Name) > 0 then
          ConverterDLL := WordPadDir + 'mswd6_32.wpc';
      end;

  end;

  if ConverterDLL <> '' then
    begin
      if CurrentConverter <> 0 then // Unload the current loaded converter.
        FreeLibrary(CurrentConverter);

      Result := LoadLibrary(PChar(ConverterDLL)); // Load the new converter.

      if Result <> 0 then
        begin
          CurrentConverter := Result; // Try to initialize our functions.
          @InitConverter := GetProcAddress(Result, 'InitConverter32');
          @UninitConverter := GetProcAddress(Result, 'UninitConverter');
          @IsFormatCorrect := GetProcAddress(Result, 'IsFormatCorrect32');
          @ForeignToRtf := GetProcAddress(Result, 'ForeignToRtf32');
          @RtfToForeign := GetProcAddress(Result, 'RtfToForeign32');
        end;
    end;

  if Result = 0 then
    begin // On failure, reset...
      @InitConverter := nil;
      @UninitConverter := nil;
      @IsFormatCorrect := nil;
      @ForeignToRtf := nil;
      @RtfToForeign := nil;
      raise Exception.CreateFmt( 'Could not load converter for %s.', [Name] );
    end;
end;


// The function "StringToHGlobal" converts a string in HGLOBAL.
function StringToHGLOBAL(const str: string): HGLOBAL;
var
  new: PChar;
begin
  Result := GlobalAlloc(GHND, Length(str) * 2 + 1);

  new := GlobalLock(Result);

  if new <> nil then
    strcopy(new, PChar(str));

  GlobalUnlock(Result);
end;


// The function "IsKnownFormat" is used to check if a selected file
// matches a converter-type.
function IsKnownFormat(FileName: string): Boolean;
var
  hFileName,
  hDesc: HGLOBAL;
begin
  Result := False;

  if not (Assigned(InitConverter)
     and LongBool(InitConverter(Application.Handle, PChar(Uppercase(Application.ExeName))))) then
    ShowMessage('InitConverter failed') // Question only is: report to who?
  else
    begin
      hFileName := StringToHGLOBAL(FileName);
      hDesc := StringToHGLOBAL('');
      try
        if Assigned(IsFormatCorrect) then
           Result:= (IsFormatCorrect(hFileName, hDesc) = fceTrue); // hDesc gets like 'MSWord6'
      finally
        GlobalFree(hDesc);
        GlobalFree(hFileName);
      end;
    end;
end;


// The function "Reading" is used by the converter-DLL's.
// It is used to import a foreign format to the RTF-format.
function Reading(CCH, nPercentComplete: integer): Integer; stdcall;
var
  tempBuf: PChar;
begin
  tempBuf := GlobalLock(hBuf);

  if CCH > 0 then
    begin
      if mstream.Position + CCH >= mstream.Size then // enlarge stream
        mstream.SetSize(mstream.size + (mstream.size div 4)); // by .25
      mstream.Write(tempBuf^, CCH);
    end;

  GlobalUnlock(hBuf);

  inc(BytesRead, CCH);

  Result := 0; // everything OK
end;


// The function "Writing" is used by the converter-DLL's.
// It is used to export the RTF-format to a foreign format.
function Writing(flags, nPercentComplete: integer): Integer; stdcall;
var
  tempBuf: PChar;
begin
  tempBuf := GlobalLock(hBuf);

  if tempBuf = nil then
    begin
      Result := -8; // out of memory?
      Exit;
    end;

  if writePos < writeMax then
    begin
      if (writeMax - writePos) < nBufSize then
        bytesRead := writeMax - writePos
      else
        bytesRead := nBufSize;

      move(RTFToWrite[WritePos+1], tempBuf^, bytesRead);

      inc(writePos, bytesRead);
    end
  else
    bytesRead := 0;

  GlobalUnlock(hBuf);
  Result := bytesRead;
end;


procedure DoFreeConverters;
begin
  if CurrentConverter <> 0 then
    begin
      FreeLibrary(CurrentConverter);
      CurrentConverter := 0;
    end;
end;

initialization

finalization
  DoFreeConverters;

end.
