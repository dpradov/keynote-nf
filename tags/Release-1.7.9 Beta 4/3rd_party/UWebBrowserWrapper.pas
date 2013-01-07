{
 * =========================================================================== *
 * Modifications from Daniel Prado 2012
 * =========================================================================== *
 * The code of this unit accompanies the article "How to load and save documents
 * in TWebBrowser in a Delphi-like way" which can be found at
 * http://www.delphidabbler.com/articles?article=14.
 *
 * IMPORTANT NOTE
 * --------------
 *
 * The code is presented for demonstration purposes only. It should not be used
 * in a production environment without thoroughly testing it first. No guarantee
 * or warranty as to the suitability of this code is provided and it is used at
 * your own risk.
 *
 * $Rev: 99 $
 * $Date: 2011-09-10 23:12:46 +0100 (Sat, 10 Sep 2011) $
 *
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UWebBrowserWrapper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2004-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   Mauricio Julio
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or the
 * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
 * case the provisions of the GPL or the LGPL are applicable instead of those
 * above. If you wish to allow use of your version of this file only under the
 * terms of either the GPL or the LGPL, and not to allow others to use your
 * version of this file under the terms of the MPL, indicate your decision by
 * deleting the provisions above and replace them with the notice and other
 * provisions required by the LGPL or the GPL. If you do not delete the
 * provisions above, a recipient may use your version of this file under the
 * terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****
}

{$BOOLEVAL OFF}

unit UWebBrowserWrapper;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0} // >= Delphi 7
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}

uses
  // Delphi
  SysUtils, Classes, SHDocVw, Controls;

type
  // Wrapper class
  TWebBrowserWrapper = class(TObject)
  private
    fWebBrowser: TWebBrowser; // wrapped control
    fFreeOnDestroy: boolean;

  protected // "Helper" methods
    procedure InternalLoadDocumentFromStream(const Stream: TStream);
    procedure InternalSaveDocumentToStream(const Stream: TStream);
    {$IFDEF UNICODE}
    function GetDocumentEncoding: TEncoding;
    {$ENDIF}
  public
    constructor Create(const Parent: TWinControl); overload;
    constructor Create(const WebBrowser: TWebBrowser); overload;
    destructor Destroy(); override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromString(const HTML: string); overload;
    {$IFDEF UNICODE}
    procedure LoadFromString(const HTML: string;
      const Encoding: TEncoding); overload;
    {$ENDIF}
    function NavigateToLocalFile(const FileName: WideString): Boolean;
    procedure NavigateToResource(const Module: HMODULE; const ResName: PChar;
      const ResType: PChar = nil); overload;
    procedure NavigateToResource(const ModuleName: string; const ResName: PChar;
      const ResType: PChar = nil); overload;
    procedure NavigateToURL(const URL: WideString);
    procedure NavigateToBlank();
    function SaveToString: string;
    procedure SaveToStream(const Stm: TStream); overload;
    {$IFDEF UNICODE}
    procedure SaveToStream(const Stm: TStream; const Encoding: TEncoding);
      overload;
    {$ENDIF}
    procedure SaveToFile(const FileName: String); overload;
    {$IFDEF UNICODE}
    procedure SaveToFile(const FileName: string; const Encoding: TEncoding);
      overload;
    {$ENDIF}
    property WebBrowser: TWebBrowser read fWebBrowser;
    {$IFDEF UNICODE}
    property Encoding: TEncoding read GetDocumentEncoding;
    {$ENDIF}

    procedure CopyAll;
  end;

implementation

uses
  // Delphi
  StrUtils, Windows, ActiveX, Forms, MSHTML, TntSysUtils;


{ Helper routines }

{
  Bug fixed version of function by Mauricio Julio from the cyIEUtils unit in his
  cyComponents package.
}
{$IFDEF UNICODE}
function GetStreamEncoding(const Stream: TStream): TEncoding;
var
  Bytes: TBytes;
  Size: Int64;
begin
  // could optimise this to read only sufficient bytes to find largest preamble
  // (this is 3 for current standard encodings)
  Stream.Seek(0, soFromBeginning);
  Size := Stream.Size;
  SetLength(Bytes, Size);
  Stream.ReadBuffer(Pointer(Bytes)^, Size);
  Result := nil;  //!fix: must initialise Result to pass as var param below
  TEncoding.GetBufferEncoding(Bytes, Result);
end;
{$ENDIF}

{
  Copies a string to a stream, encoded as required, with any byte order mark
}
{$IFDEF UNICODE}
procedure StringToStreamBOM(const S: string; const Stm: TStream;
  const Encoding: TEncoding);
var
  Bytes: TBytes;
  Preamble: TBytes;
begin
  Assert(Assigned(Encoding));
  Bytes := Encoding.GetBytes(S);
  Preamble := Encoding.GetPreamble;
  if Length(Preamble) > 0 then
    Stm.WriteBuffer(Preamble[0], Length(Preamble));
  Stm.WriteBuffer(Bytes[0], Length(Bytes));
end;
{$ENDIF}

{
  The following 3 functions are taken from the DelphiDabbler article "How to
  create and use HTML resource files" at
  http://www.delphidabbler.com/articlesarticle=10
}

{
  Do not use this function to URL encode query strings: see the more flexible
  version in the Code Snippets Database at
  http://www.delphidabbler.com/codesnip?action=named&routines=URLEncode
}
function URLEncode(const S: string): string;
var
  Idx: Integer; // loops thru characters in string
begin
  Result := '';
  for Idx := 1 to Length(S) do
  begin
    {$IFDEF UNICODE}
    if CharInSet(S[Idx], ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
    {$ELSE}
    if S[Idx] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.'] then
    {$ENDIF}
      Result := Result + S[Idx]
    else
      Result := Result + '%' + IntToHex(Ord(S[Idx]), 2);
  end;
end;

function MakeResourceURL(const ModuleName: string; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
  // ---------------------------------------------------------------------------
  function ResNameOrTypeToString(R: PChar): string;
  begin
    if HiWord(LongWord(R)) = 0 then
      // high word = 0 => numeric resource id
      // numeric value is stored in low word
      Result := Format('#%d', [LoWord(LongWord(R))])
    else
      // high word <> 0 => string value
      // PChar is implicitly converted to string
      Result := R;
  end;
  // ---------------------------------------------------------------------------
begin
  Assert(ModuleName <> '');
  Assert(Assigned(ResName));
  Result := 'res://' + URLEncode(ModuleName);
  if Assigned(ResType) then
    Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResType));
  Result := Result + '/' + URLEncode(ResNameOrTypeToString(ResName));
end;

function MakeResourceURL(const Module: HMODULE; const ResName: PChar;
  const ResType: PChar = nil): string; overload;
begin
  Result := MakeResourceURL(GetModuleName(Module), ResName, ResType);
end;

{ TWebBrowserWrapper }

constructor TWebBrowserWrapper.Create(const WebBrowser: TWebBrowser);
begin
  inherited Create;
  fWebBrowser := WebBrowser;
  fFreeOnDestroy:= false;
end;

constructor TWebBrowserWrapper.Create(const Parent: TWinControl);
begin
  inherited Create;

  fWebBrowser := TWebBrowser.Create(nil);
  fWebBrowser.Width := 0;
  TControl(fWebBrowser).Parent := Parent;
  TControl(fWebBrowser).Visible:= False;
  fFreeOnDestroy:= true;
end;

procedure Pause(const ADelay: Cardinal);
var
  StartTC: Cardinal;  // tick count when routine called
begin
  StartTC := Windows.GetTickCount;
  repeat
    Application.ProcessMessages;
  until Int64(Windows.GetTickCount) - Int64(StartTC) >= ADelay;
end;

destructor TWebBrowserWrapper.Destroy();
begin
  if fFreeOnDestroy and assigned(fWebBrowser) then begin
     NavigateToURL('about:blank');
     FreeAndNil(fWebBrowser);
  end;

  inherited Destroy;
end;


{$IFDEF UNICODE}
function TWebBrowserWrapper.GetDocumentEncoding: TEncoding;
var
  Doc: IHTMLDocument2;
  DocStm: TStream;
begin
  Assert(Assigned(WebBrowser.Document));
  Result := TEncoding.Default;
  if WebBrowser.Document.QueryInterface(IHTMLDocument2, Doc) = S_OK then
  begin
    DocStm := TMemoryStream.Create;
    try
      InternalSaveDocumentToStream(DocStm);
      Result := GetStreamEncoding(DocStm);
    finally
      DocStm.Free;
    end;
  end;
end;
{$ENDIF}

procedure TWebBrowserWrapper.InternalLoadDocumentFromStream(
  const Stream: TStream);
var
  PersistStreamInit: IPersistStreamInit;
  StreamAdapter: IStream;
begin
  if not Assigned(WebBrowser.Document) then
    Exit;

  WebBrowser.Silent:= True;        // DPV: Para que no de mensajes de error, p.ej por Javascript

  // Get IPersistStreamInit interface on document object
  if WebBrowser.Document.QueryInterface(
    IPersistStreamInit, PersistStreamInit
  ) = S_OK then
  begin
    // Clear document
    if PersistStreamInit.InitNew = S_OK then
    begin
      // Get IStream interface on stream
      StreamAdapter:= TStreamAdapter.Create(Stream);
      // Load data from Stream into WebBrowser
      PersistStreamInit.Load(StreamAdapter);
    end;
  end;
end;

procedure TWebBrowserWrapper.InternalSaveDocumentToStream(
  const Stream: TStream);
var
  StreamAdapter: IStream;
  PersistStreamInit: IPersistStreamInit;
begin
  if not Assigned(WebBrowser.Document) then
    Exit;
  if WebBrowser.Document.QueryInterface(
    IPersistStreamInit, PersistStreamInit
  ) = S_OK then
  begin
    StreamAdapter := TStreamAdapter.Create(Stream);
    PersistStreamInit.Save(StreamAdapter, True);
  end;
end;

procedure TWebBrowserWrapper.LoadFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TWebBrowserWrapper.LoadFromStream(const Stream: TStream);
begin
  NavigateToURL('about:blank');
  InternalLoadDocumentFromStream(Stream);
  while WebBrowser.ReadyState <> READYSTATE_COMPLETE do
    Pause(10);
end;

{$IFDEF UNICODE}
procedure TWebBrowserWrapper.LoadFromString(const HTML: string;
  const Encoding: TEncoding);
var
  HTMLStm: TMemoryStream;
begin
  Assert(Assigned(Encoding));
  HTMLStm := TMemoryStream.Create;
  try
    StringToStreamBOM(HTML, HTMLStm, Encoding);
    HTMLStm.Position := 0;
    LoadFromStream(HTMLStm);
  finally
    HTMLStm.Free;
  end;
end;
{$ENDIF}

procedure TWebBrowserWrapper.LoadFromString(const HTML: string);
{$IFDEF UNICODE}
begin
  LoadFromString(HTML, TEncoding.Default);
end;
{$ELSE}
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(HTML);
  try
     LoadFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;
{$ENDIF}

function TWebBrowserWrapper.NavigateToLocalFile(
  const FileName: wideString): Boolean;
begin
  Result := WideFileExists(FileName);
  if Result then
     NavigateToURL('file://' + FileName)
end;

procedure TWebBrowserWrapper.NavigateToResource(const Module: HMODULE;
  const ResName, ResType: PChar);
begin
  NavigateToURL(MakeResourceURL(Module, ResName, ResType));
end;

procedure TWebBrowserWrapper.NavigateToResource(const ModuleName: string;
  const ResName, ResType: PChar);
begin
  NavigateToURL(MakeResourceURL(ModuleName, ResName, ResType));
end;

procedure TWebBrowserWrapper.NavigateToBlank();
begin
  NavigateToURL('about:blank');
end;


procedure TWebBrowserWrapper.NavigateToURL(const URL: WideString);
var
  Flags: OleVariant;  // flags that determine action
begin
  Flags := navNoHistory;           // Don't record in history
  if AnsiStartsText('res://', URL) or AnsiStartsText('file://', URL)
    or AnsiStartsText('about:', URL) or AnsiStartsText('javascript:', URL)
    or AnsiStartsText('mailto:', URL) then
        Flags := Flags or navNoReadFromCache or navNoWriteToCache;  // don't use cache for local files

  WebBrowser.Silent:= True;        // No error messages, caused by Javascript, ..

  // Do the navigation and wait for it to complete
  WebBrowser.Navigate(URL, Flags);
  while WebBrowser.ReadyState <> READYSTATE_COMPLETE do
    Pause(10);
end;

procedure TWebBrowserWrapper.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

{$IFDEF UNICODE}
procedure TWebBrowserWrapper.SaveToFile(const FileName: string;
  const Encoding: TEncoding);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream, Encoding);
  finally
    FileStream.Free;
  end;
end;
{$ENDIF}

procedure TWebBrowserWrapper.SaveToStream(const Stm: TStream);
begin
  InternalSaveDocumentToStream(Stm);
end;

{$IFDEF UNICODE}
procedure TWebBrowserWrapper.SaveToStream(const Stm: TStream;
  const Encoding: TEncoding);
var
  HTML: string;
begin
  HTML := SaveToString;
  StringToStreamBOM(HTML, Stm, Encoding);
end;
{$ENDIF}

function TWebBrowserWrapper.SaveToString: string;
{$IFDEF UNICODE}
var
  MS: TMemoryStream;
  Encoding: TEncoding;
  Bytes: TBytes;
begin
  MS := TMemoryStream.Create;
  try
    SaveToStream(MS);
    // This stream may have a pre-amble indicating encoding
    Encoding := GetStreamEncoding(MS);
    MS.Position := Length(Encoding.GetPreamble);
    SetLength(Bytes, MS.Size - MS.Position);
    MS.ReadBuffer(Bytes[0], Length(Bytes));
    Result := Encoding.GetString(Bytes);
  finally
    MS.Free;
  end;
{$ELSE}
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    SaveToStream(StringStream);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
{$ENDIF}
end;


procedure TWebBrowserWrapper.CopyAll;
begin
    WebBrowser.ExecWB( OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT );
    WebBrowser.ExecWB( OLECMDID_COPY,      OLECMDEXECOPT_DODEFAULT );
end;



initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.

