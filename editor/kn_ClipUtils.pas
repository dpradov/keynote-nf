unit kn_ClipUtils;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   Winapi.Windows,
   Winapi.RichEdit,
   Winapi.ShellAPI,
   System.Classes,
   System.StrUtils,
   System.SysUtils,
   Vcl.Dialogs,
   Vcl.Clipbrd ,
   RxRichEd;

var
  CFHtml : Integer; // = 0;
  CFRtf: Integer;
  LastCopiedIDImage:   integer;
  LastCopyFromScratchpad: boolean;


type
   TClipboardHelper = class helper for TClipboard
     private
       function RetryGetClipboardData(const ClipbrdContent: integer): THandle;
     protected
       function GetAsRTF: AnsiString;
       procedure SetAsRTF(const Value: AnsiString);
       function GetAsHTML: AnsiString;
     public
       function TryAsText: string;
       property AsRTF: AnsiString read GetAsRTF write SetAsRTF;
       property AsHTML: AnsiString read GetAsHTML;
       function AddOrReplaceRTF(const NewRTF: AnsiString; const NewHTML: AnsiString = ''): boolean;
       // function GetTitleFromHTML (const HTMLClipboard: AnsiString): string;
       function GetURLFromHTML (const HTMLClipboard: AnsiString): string;
       procedure TrimMetadataFromHTML (var HTMLClipboard: AnsiString);
       procedure PrepareHTMLFormatTag (tag: AnsiString; var HTMLClipboard: AnsiString);
       function GetNextImageInformation(const HTMLText: AnsiString; Offset: integer;
                                        var SourceImg: AnsiString; var TextAlt: string;
                                        var W, H: integer;
                                        var PosTagOpening: integer;
                                        var PosTagClosing: integer): boolean;
       function ToStream (Fmt : Word; Stm : TStream ) : boolean;
       function HasRTFformat: boolean;
       function HasHTMLformat: boolean;
       function TryGetFirstLine(const MaxLen : integer): string;
       function TryOfferRTF (const HTMLText: AnsiString=''; TextAttrib: TRxTextAttributes = nil; PlainText: boolean = false): AnsiString;
       function TryGetAsHandle(Format: Word): THandle;
   end;


  function TestCRCForDuplicates(ClpStr: string; UpdateLastCalculated: boolean = true): boolean;

  procedure LogRTFHandleInClipboard();
  function ClipboardContentWasCopiedByKNT: boolean;
  function GetDropFiles(hDrop: THANDLE): TStringList;

  procedure CopyToClipboard (Editor: TRxRichEdit);
  procedure CutToClipboard (Editor: TRxRichEdit);


implementation
uses
   WinApi.WinInet,
   System.Math,
   Vcl.Controls,
   Vcl.Forms,
   CRC32,
   gf_strings,
   kn_global,
   kn_ExportImport,
   kn_RTFUtils,
   knt.App,
   knt.RS
   ;


type
  TClipboardContent = (
    ccRTF, ccUNICODE, ccHtml
  );


var
   LastCopiedRTFHandle: HGLOBAL;
   LastCopiedRTFAsWebPlain: boolean;      // It was used Ctrl+Shift+W (or equivalent)
   //LastCopiedRTFPtr:    Pointer;


procedure LogRTFHandleInClipboard();
begin
   LastCopiedRTFHandle := Clipboard.TryGetAsHandle(CFRtf);
   LastCopiedRTFAsWebPlain:= false;
end;

{
procedure LogRTFHandleInClipboard();
var
  RTFHandle: HGLOBAL;
  RTFPtr: Pointer;
  RTFSize: DWORD;
  str: AnsiString;
begin
    Clipboard.Open;
    try
      RTFHandle := Clipboard.GetAsHandle(CFRtf);
      RTFPtr := GlobalLock(RTFHandle);
      RTFSize := GlobalSize(RTFHandle);
      Str := Ansistring(PAnsiChar(RTFPtr));
      LastCopiedRTFHandle :=RTFHandle;
      LastCopiedRTFPtr:= RTFPtr;

    finally
      GlobalUnlock(RTFHandle);
      Clipboard.Close;
    end;
end;
}


function ClipboardContentWasCopiedByKNT: boolean;
var
  RTFHandle: HGLOBAL;
begin
   Result:= False;
   RTFHandle := Clipboard.TryGetAsHandle(CFRtf);
   if LastCopiedRTFHandle = RTFHandle then
      Result:= True
   else
      LastCopiedIDImage:= 0;
end;


procedure CopyToClipboard (Editor: TRxRichEdit);
begin
  _IS_COPYING_TO_CLIPBOARD:= true;
  try
     Editor.CopyToClipboard;
  finally
    _IS_COPYING_TO_CLIPBOARD:= false;
  end;
end;

procedure CutToClipboard (Editor: TRxRichEdit);
begin
  _IS_COPYING_TO_CLIPBOARD:= true;
  try
     Editor.CutToClipboard;
  finally
    _IS_COPYING_TO_CLIPBOARD:= false;
  end;
end;


function GetDropFiles(hDrop: THANDLE): TStringList;
var
  CFileName : array[0..MAX_PATH] of Char;
  FileList : TStringList;
  i, count : integer;
begin
  FileList := TStringList.Create;

  try
    count := DragQueryFile( hDrop, $FFFFFFFF, CFileName, MAX_PATH );

    if ( count > 0 ) then begin
      for i := 0 to count-1 do begin
        DragQueryFile( hDrop, i, CFileName, MAX_PATH );
        FileList.Add( CFileName );
      end;
    end;

    Result:= FileList;

  finally
    DragFinish(hDrop);
  end;

end;

function TClipboardHelper.HasHTMLformat: boolean;
begin
   result := (CFHtml <> 0) and Clipboard.HasFormat( CFHtml );
end;

function TClipboardHelper.HasRTFformat: boolean;
begin
   result := (CFRtf <> 0) and Clipboard.HasFormat( CFRtf );
end;



function TClipboardHelper.RetryGetClipboardData(const ClipbrdContent: integer): THandle;
var
  RetryCount: integer;
begin
  Result:= 0;
  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Clipboard.Open;
      try
        Result := GetClipboardData(ClipbrdContent);
        RetryCount:= 99;   // Ok, salimos
      finally
        Clipboard.Close;
      end;

    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;  // RetryGetClipboardData



function TClipboardHelper.ToStream( Fmt : Word; Stm : TStream ) : boolean;
// code by Peter Below (modified DPV)
var
  hMem: THandle;
  pMem: Pointer;
begin
  result := false;
  hMem := RetryGetClipboardData(fmt);  //[DPV]
  if hMem <> 0 then begin
    pMem := GlobalLock( hMem );
    if pMem <> Nil Then Begin
      try
        Stm.Write( pMem^, GlobalSize( hMem ));
        result := true;
      finally
        GlobalUnlock( hMem );
      end;
    end
  end;
end; // ClipboardToStream


function TClipboardHelper.GetURLFromHTML (const HTMLClipboard: AnsiString): string;
const
  HTML_FMT_SRCURL  = 'SourceURL:';
  l = length(HTML_FMT_SRCURL);
var
  p, pF: integer;
begin
   Result := '';

   p:= pos( HTML_FMT_SRCURL, HTMLClipboard);
   if p >= 1 then begin
     pF:= p;
     while HTMLClipboard[pF] <> #13 do
         pF := pF + 1;
     Result:= Copy( HTMLClipboard, p+l, pF-p-l);
   end;
   if Result = 'about:blank' then
      Result:= '';
end; // GetURLFromHTMLClipboard

{  At the beginning some browsers included title on the clipboard, but currently it does not.
function TClipboardHelper.GetTitleFromHTML (const HTMLClipboard: AnsiString): string;
const
  TITLE  = '<TITLE>';
  l = length(TITLE);
var
   p, pF: integer;
begin
   Result := '';

   p:= pos( TITLE, HTMLClipboard);
   if p >= 1 then begin
     pF:= PosEx( '</TITLE>', HTMLClipboard, p+l);
     if pF > p  then
        Result:= Copy( HTMLClipboard, p+l, pF-p-l);
   end;
end; // GetURLFromHTMLClipboard
}

procedure TClipboardHelper.TrimMetadataFromHTML (var HTMLClipboard: AnsiString);
var
  p : integer;
begin
   p:= pos( '<html>', HTMLClipboard);
   if p <= 0 then p:= pos( '<HTML>', HTMLClipboard);
   if p <= 0 then p:= pos( '<HTML ', HTMLClipboard);
   if p <= 0 then p:= pos( '<html ', HTMLClipboard);

   if p >= 1 then
     delete( HTMLClipboard, 1, p-1);
end;

{
When copying HTML text to the clipboard, Firefox simplifies some tag styles and the resulting text can be
correctly converted to RTF, but other browsers such as Edge or Chrome do not do this and therefore the
resulting RTF loses some formatting style.

We need to simplify some tags like:
  <STRONG style="BOX-SIZING: border-box; FONT-WEIGHT: bolder">Text....</STRONG>   -->   <STRONG>Text</STRONG>
}
procedure TClipboardHelper.PrepareHTMLFormatTag (tag: AnsiString; var HTMLClipboard: AnsiString);
var
  pI, pF, i, L : integer;
  tokenSearched, s: string;
begin
   tokenSearched:= '<' + tag + ' style';
   L:= Length(Tag);
   i:= 1;
   repeat
      pI:= pos(tokenSearched, HTMLClipboard, i);
      if pI >= 1 then begin
        pF:= pos( '>', HTMLClipboard, pI + L+5);
        if pF > 0 then begin
           s:= Copy( HTMLClipboard, pI+(L+1), pF-pI-(L+1));
           delete( HTMLClipboard, pI+(L+1), pF-pI-(L+1));
        end;
        i:= pI + L;
      end;
   until pI <= 0;
end;


function TClipboardHelper.GetNextImageInformation(const HTMLText: AnsiString; Offset: integer;
                                                   var SourceImg: AnsiString; var TextAlt: string;
                                                   var W, H: integer;
                                                   var PosTagOpening: integer;
                                                   var PosTagClosing: integer): boolean;
var
  p1, p2, p3: integer;
  TextAlt_Ansi: AnsiString;
  //s: string;

begin
{
 Also consider cases such as:

<picture>
  <source type="image/webp" media="(max-width: 578px)" srcset="https://.....webp">
  <source type="image/jpg" media="(max-width: 1099px)" srcset="https://....jpg">
   ...
  <img src="data:image/svg+xml,..." alt="..." ... >
</picture>
}


   Result:= false;
   TextAlt_Ansi:= '';
   SourceImg:= '';
   p1:= Pos('<img ', HTMLText, Offset);
   if p1 > 0 then begin
        p2:= Pos(' src="', HTMLText, p1);
        p3:= Pos('"', HTMLText, p2+6);
        SourceImg:= Copy(HTMLText, p2+6, p3-p2-6);
        p2:= Pos('>', HTMLText, p3);
        PosTagOpening:= p1;
        PosTagClosing:= p2;

        if PosTagClosing > 0 then begin
           p2:= Pos('alt="', HTMLText, p1);
           if (p2 > 0) and (p2 < PosTagClosing) then begin
              p3:= Pos('"', HTMLText, p2+5);
              TextAlt_Ansi:= Copy(HTMLText, p2+5, p3-p2-5);
           end;

           //s:= Copy(HTMLText, p1, p2-p1+1);
           Result:= true;
           TextAlt:= '';
           if TextAlt_Ansi <> '' then begin
              TextAlt:= TryUTF8ToUnicodeString(TextAlt_Ansi);
              TextAlt:= ConvertHTMLAsciiCharacters(TextAlt);
           end;

           W:= 0;
           H:= 0;
           p2:= Pos(' width="', HTMLText, p1);
           if (p2 > 0) and (p2 < PosTagClosing) then begin
              p3:= Pos('"', HTMLText, p2+8);
              W:= StrToIntDef(Copy(HTMLText, p2+8, p3-p2-8), 0);

              p2:= Pos('height="', HTMLText, p2);
              if (p2 > 0) and (p2 < PosTagClosing) then begin
                 p3:= Pos('"', HTMLText, p2+8);
                 H:= StrToIntDef(Copy(HTMLText, p2+8, p3-p2-8), 0);
              end;
           end
           else begin
              p2:= Pos(' width: ', HTMLText, p1);
              if (p2 > 0) and (p2 < PosTagClosing) then begin
                 p3:= Pos('px;', HTMLText, p2+8);
                 W:= StrToIntDef(Copy(HTMLText, p2+8, p3-p2-8), 0);

                 p2:= Pos('height: ', HTMLText, p2);
                 if (p2 > 0) and (p2 < PosTagClosing) then begin
                    p3:= Pos('px;', HTMLText, p2+8);
                    H:= StrToIntDef(Copy(HTMLText, p2+8, p3-p2-8), 0);
                 end;
              end
           end;


           if Copy(SourceImg,1,5) = 'data:' then begin
              p1:= Pos('<source type="image/jpg"', HTMLText, Offset);
              if (p1 > 0) and (p1 < PosTagOpening) then begin
                 p2:= Pos('srcset="', HTMLText, p1);
                 p3:= Pos('"', HTMLText, p2+8);
                 SourceImg:= Copy(HTMLText, p2+8, p3-p2-8);
              end;
           end;
        end;
   end;
end;


procedure DownloadImage(const URL: string; const TextAlt: string; W, H: integer; var StrRTF: AnsiString);
var
  hInet, hConnect: HINTERNET;
  HeaderBuffer: array[0..255] of Char;
  HeaderBufferSize, Index: DWORD;
  Buffer: array of Byte;
  Stream: TMemoryStream;
  BufferSize: integer;
  BytesRead, TotalBytesRead, ContentLength: DWORD;
begin
  StrRTF:= '';
  if not URL.StartsWith('http') then exit;


  hInet := InternetOpen('KeyNote_ImageDownloader', INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
  if Assigned(hInet) then begin
    screen.Cursor := crHourGlass;
    try
      hConnect := InternetOpenUrl(hInet, PChar(URL), nil, 0, INTERNET_FLAG_RELOAD, 0);
      if Assigned(hConnect) then
         try
           ContentLength := 0;
           HeaderBufferSize := SizeOf(HeaderBuffer);
           Index := 0;
           if HttpQueryInfo(hConnect, HTTP_QUERY_CONTENT_LENGTH, @HeaderBuffer, HeaderBufferSize, Index) then
             ContentLength := StrToIntDef(string(HeaderBuffer), 0);

           Stream:= TMemoryStream.Create;

           if ContentLength = 0 then begin
               ContentLength:= Integer.MaxValue;
               BufferSize:= 200*1024;
               SetLength(Buffer, BufferSize);
           end
           else begin
              BufferSize:= ContentLength;
              SetLength(Buffer, BufferSize);
              Stream.SetSize(ContentLength);
           end;

           TotalBytesRead:= 0;
           while (ContentLength > TotalBytesRead)
                 and InternetReadFile(hConnect, @Buffer[0], BufferSize, BytesRead)
                 and (BytesRead > 0) do begin

              Stream.WriteBuffer(Buffer[0], BytesRead);
              inc(TotalBytesRead, BytesRead);
           end;

           ImageMng.RegisterAndInsertImage('', ActiveEditor, true, StrRTF, TextAlt, Stream, False, W,H);

         finally
           InternetCloseHandle(hConnect);
         end;
    finally
      InternetCloseHandle(hInet);
      screen.Cursor := crDefault;
    end;
  end;
end;


{
Calculates CRC on ClpStr and compare it with last calculated CRC, returning true if it is equal
Last calculated CRC (ClipCapCRC32) is then updated (if 'UpdateLastCalculated' = true)
}
function TestCRCForDuplicates(ClpStr: String; UpdateLastCalculated: boolean = true): boolean;
var
   thisClipCRC32: DWORD;
begin
   Result:= false;

   if ( ClpStr <> '' ) then begin
      try
        CalculateCRC32( addr(ClpStr[1]), length(ClpStr) * SizeOf(Char), thisClipCRC32 );
      except
        on E : Exception do begin
          messagedlg( sEdt50 + E.Message, mtError, [mbOK], 0 );
          ClipOptions.TestDupClips := false;
          exit;
        end;
      end;
      if ( thisClipCRC32 = ClipCapCRC32 ) then
          Result:= true;

      if UpdateLastCalculated then
         ClipCapCRC32 := thisClipCRC32; // set value for next test
   end;

end;



procedure TClipboardHelper.SetAsRTF(const Value: AnsiString);
begin
  SetBuffer(CFRtf, PAnsiChar(Value)^, Length(Value) + 1);
end;

function TClipboardHelper.GetAsRTF: AnsiString;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFRtf);
   if Data <> 0 then begin
      Result := PAnsiChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;

function TClipboardHelper.GetAsHTML: AnsiString;
var
   Data: THandle;
begin
   Data:= RetryGetClipboardData(CFHtml);
   if Data <> 0 then begin
      Result := PAnsiChar(GlobalLock(Data));
      GlobalUnLock(Data)
   end
   else
      Result:= '';
end;


function TClipboardHelper.TryGetFirstLine(const MaxLen : integer): string;
begin
    Result:= FirstLineFromString(TrimLeft(TryAsText), MaxLen);
end;

function TClipboardHelper.TryAsText: string;
var
  RetryCount: integer;
begin
  Result:= '';
  if not Clipboard.HasFormat (CF_TEXT) then exit;

  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Result:= Clipboard.AsText;
      break;
    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


function TClipboardHelper.TryGetAsHandle(Format: Word): THandle;
var
  RetryCount: integer;
begin
  Result:= 0;
  RetryCount:= 0;
  while RetryCount < 6 do
    try
      Clipboard.Open;
      try
        Result:= Clipboard.GetAsHandle(Format);
        break;
      finally
        Clipboard.Close;
      end;

    except
      Inc(RetryCount);
      if RetryCount < 6 then Sleep(RetryCount * 100);
    end;
end;


function TClipboardHelper.AddOrReplaceRTF(const NewRTF: AnsiString; const NewHTML: AnsiString = ''): boolean;
var
  TextData, HTMLData: THandle;
  RTFHandle: THandle;
  RTFPointer: Pointer;
  function CopyContent (var Data: THandle): boolean;
  var
     DataPointer: Pointer;
  begin
    Result:= true;
    if Data <> 0 then begin
      DataPointer := GlobalLock(Data);
      try
        Data := GlobalAlloc(GMEM_MOVEABLE, GlobalSize(Data));
        if Data = 0 then
           Result:= false;
        Move(DataPointer^, GlobalLock(Data)^, GlobalSize(Data));
      finally
        if DataPointer <> nil then
          GlobalUnlock(Data);
      end;
    end;

  end;
  function CopyText(Format: integer; Data: AnsiString): boolean;
  var
    DataHandle: THandle;
    DataPointer: Pointer;
  begin
    if Data <> '' then begin
      DataHandle:= GlobalAlloc(GMEM_MOVEABLE, Length(Data) + 1);
      if DataHandle = 0 then
         exit;
      try
        DataPointer := GlobalLock(DataHandle);
        try
          Move(PAnsiChar(Data)^, DataPointer^, Length(Data) + 1);
        finally
          GlobalUnlock(DataHandle);
        end;
        SetClipboardData(Format, DataHandle);
        Result:= true;
      except
        GlobalFree(DataHandle);
      end;
    end;

  end;
begin
  Result:= false;
  if not OpenClipboard(0) then exit;
  try
    TextData := GetClipboardData(CF_TEXT);
    if not CopyContent(TextData) then exit;
    if NewHTML = '' then begin
      HTMLData := GetClipboardData(CFHtml);
      if not CopyContent(HTMLData) then exit;
    end;
    EmptyClipboard;
    if TextData <> 0 then                            // Restore the contents of CF_TEXT
      SetClipboardData(CF_TEXT, TextData);
    if NewHTML = '' then begin
       if HTMLData <> 0 then                          // Restore the contents of CF_HTML
         SetClipboardData(CFHtml, HTMLData);
    end
    else
       if not CopyText(CFHtml, NewHTML) then exit;
    if not CopyText(CFRtf, NewRTF) then exit;
  finally
    CloseClipboard;
  end;
end;


{ Try to convert the HTML received (or available in clipboard) to RTF
 If conversion is possible, it will return RTF text, and also it will be available as RTF format in the clipboard }

function TClipboardHelper.TryOfferRTF (const HTMLText: AnsiString=''; TextAttrib: TRxTextAttributes = nil; PlainText: boolean = false): AnsiString;
var
   HTMLTextToConvert, HTMLTextBAK: AnsiString;
   p, len: integer;

   posOpening, posClosing, W, H, i: integer;
   SourceImg, StrRTF: AnsiString;
   imgInfo: AnsiString;
   TextAlt: string;
   FoundImg: boolean;

   NImg: integer;
   ImagesRTF: array of AnsiString;

begin
   { *1  The RTF available in the clipboard, if any, probably has been converted by us (with the help of TWebBrowser)
         We have been reusing directly this RTF format, but now, since the introduction of changes in ConvertHTMLToRTF to allow
         defining the font and size by default, it is convenient not to do it, because these changes are no reintroduced
         in the clipboard.
         It is not necessary to do the conversion again, but simply apply the changes to set the default font and size.
   }

    Result:= '';
    if _ConvertHTMLClipboardToRTF {and (not Clipboard.HasRTFformat) *1} and Clipboard.HasHTMLformat  then begin

       HTMLTextBAK:= HTMLText;
       if Clipboard.HasRTFformat and ((not ClipboardContentWasCopiedByKNT) or (LastCopiedRTFAsWebPlain=PlainText)) then begin
          Result:= Clipboard.AsRTF;
          //SetDefaultFontAndSizeInRTF(Result, TextAttrib)  // Commented: unnecessary because of call to AddOrReplaceRTF
       end
       else begin
          if HTMLText = '' then begin
             HTMLTextToConvert:= Clipboard.AsHTML;
             HTMLTextBAK:= HTMLTextToConvert;
          end
          else
             HTMLTextToConvert:= HTMLText;

          Clipboard.TrimMetadataFromHTML(HTMLTextToConvert);

          // We must ensure that this format is recovered correctly because 'Plain text mode' can include font style (bold, cursive, ..)
          Clipboard.PrepareHTMLFormatTag('strong', HTMLTextToConvert);
          Clipboard.PrepareHTMLFormatTag('b', HTMLTextToConvert);
          Clipboard.PrepareHTMLFormatTag('em', HTMLTextToConvert);
          Clipboard.PrepareHTMLFormatTag('i', HTMLTextToConvert);

          // -- Insert images -----------------------------
          NImg:= 0;
          p:= 1;
          repeat
             FoundImg:= Clipboard.GetNextImageInformation(HTMLTextToConvert, p, SourceImg, TextAlt, W, H, posOpening, posClosing);
             if FoundImg then begin
                p:= posClosing + 1;
                StrRTF:= '';
                if not PlainText then
                   DownloadImage(SourceImg, TextAlt, W, H, StrRTF);
                Inc(NImg);
                SetLength(ImagesRTF, NImg);
                ImagesRTF[NImg-1]:= StrRTF;
                if StrRTF <> '' then
                   imgInfo:= Format(' Img:</a> _IMG_%d </p>', [NImg])
                else
                   imgInfo:= Format(' [IMG]</a></p>', [NImg]);

                Move(imgInfo[1], HTMLTextToConvert[posOpening], Length(ImgInfo));
                for i := posOpening+ Length(ImgInfo) to posClosing do
                    HTMLTextToConvert[i]:= ' ';
             end;
          until not FoundImg;


          ConvertHTMLToRTF(HTMLTextToConvert, Result);

          if not PlainText then begin
             for i:= 0 to High(ImagesRTF) do begin
                 StrRTF:= ImagesRTF[i];
                 imgInfo:= Format('_IMG_%d', [i+1]);
                 Result:= StringReplace(Result, imgInfo, '\sb0' + StrRTF, []);
             end;

             // If the image is not included in a hyperlink we will not display "Img:". Otherwise,
             // it is necessary or otherwise the image URL will be displayed directly,
             // moving the image much further.
             Result:= StringReplace(Result, 'Img: \sb0\v\', '\sb0\v\', [rfReplaceAll]);
          end;
          // -- Insert images -----------------------------


          len:= Length(Result);
          p:= Pos('\line\par\pard}', Result, len - 16);
          if p = 0 then
             p:= Pos(' \par\pard}', Result, len - 12);
          if p > 0 then begin
             delete(Result, p, len-p+1);
             insert('}', Result, p);
          end;
          SetDefaultFontAndSizeInRTF(Result, TextAttrib);

          AddOrReplaceRTF(Result, HTMLTextBAK);

          Application.ProcessMessages;
          sleep(10);
          Application.ProcessMessages;
          LogRTFHandleInClipboard();
          LastCopiedRTFAsWebPlain:= PlainText;
       end;
    end;
end;



Initialization

  CFHtml := RegisterClipboardFormat('HTML Format');
  CFRtf  := RegisterClipboardFormat(CF_RTF);

end.
