unit kn_ImagesUtils;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain)

  Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
  in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
   Winapi.Windows,
   Winapi.Messages,
   System.SysUtils,
   System.Math,
   System.Classes,
   System.Contnrs,
   System.IOUtils,
   System.AnsiStrings,
   Vcl.Graphics,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.ComCtrls,
   Vcl.Clipbrd,
   SynGdiPlus,
   RxRichEd,
   gf_streams,
   gf_miscvcl,
   kn_Const,
   kn_NodeList,
   kn_NoteObj,
   kn_LocationObj;





  // ----------------
  function  String2Hex (const Buffer: AnsiString): AnsiString;
  
  function  BinToHexString (const Stream: TMemoryStream): AnsiString; overload;
  procedure BinToHexString (const Stream: TMemoryStream; 
                            var StringToAppend: AnsiString; 
                            ExtraLength: Integer= 0); overload;

  procedure HexStringToBin (const HexStr: AnsiString; 
                            const outStream: TMemoryStream;
                            TextOffset: Integer= 0; 
                            MaxSize: Integer= integer.MaxValue); overload;
  procedure HexStringToBin (const HexStr: AnsiString; 
                            const FileName: String;
                            TextOffset: Integer= 0; 
                            MaxSize: Integer= integer.MaxValue); overload;


  // ----------------
  function GetImageFormat (Stream: TMemoryStream): TImageFormat;
  function GetGDIPPictureType (Format: TImageFormat): TGDIPPictureType;
  
  // ----------------
  function PixelsToHundredthMM (Pixels: integer): integer; inline;
  function HundredthMMToPixels (HundredthMM: integer): integer; inline;
    
  function  ImgStreamToMetafile (Stream: TMemoryStream; PixelFormat: TPixelFormat= pfCustom): TMetaFile;
  procedure MetafileToWMFStream (MF: TMetaFile; Stream: TMemoryStream);
  
  // ----------------
  function ImgStreamToRTF (ImgID: integer; 
                           const Stream: TMemoryStream; 
                           StreamImgFormat: TImageFormat;
                           Width, Height, WidthGoal, HeightGoal: integer;
                           AddRTFHeader: boolean;
                           WMFStreamIncludesMetaFileHeader: boolean = true
                           ): AnsiString;
  
  function MetaFileToRTF (ImgID: integer; MF: TMetaFile; WidthGoal, HeightGoal: integer): AnsiString;

  function GetRTFforImageInsertion (ImgID: integer;
                                    Stream: TMemoryStream;
                                    var StreamImgFormat: TImageFormat;
                                    var Width, Height, WidthGoal, HeightGoal: integer;
                                    AddRTFHeader: boolean;
                                    PreserveOriginalStreamContent: boolean;
                                    MaintainWMF_EMF: boolean = false
                                    ): AnsiString;

  procedure CheckDimensionGoals(Width, Heigh: integer; var WidthGoal, HeightGoal: integer);

  // ----------------
  function RTFPictToImage (Buffer: Pointer;
                           PictOffset: integer;
                           Stream: TMemoryStream;
                           var imgFormat: TImageFormat;
                           var Width, Height, WidthGoal, HeightGoal: integer;
                           var PosRTFImageEnd: integer;
                           GetStream: boolean= true
                           ): boolean;
  
  function RTFLinkToImage (Buffer: Pointer;
                           PictOffset: integer;
                           var WidthGoal, HeightGoal: integer;
                           var PosRTFImageEnd: integer
                           ): boolean;

  function GetImageIDinPlaintext (Str: String): integer;
  function GetImageIDinURLstr (Str: String): integer;
  function CountRTFLinkChars (RTF: PAnsiChar; LinkOffset: integer): integer;


  // ----------------
  procedure ConvertStreamToImgFormatDest (Stream: TMemoryStream;
                                         const ImgFormat: TImageFormat;
                                         var ImgFormatDest: TImageFormat );



implementation

uses  System.DateUtils,
      WinApi.MMSystem,
      ComCtrls95,
      CRC32,
      gf_misc,
      gf_strings,
      kn_ClipUtils,
      kn_info,
      kn_Main,
      kn_Global,
      kn_TreeNoteMng,
      kn_RTFUtils,
      kn_LinksMng,
      kn_VCLControlsMng
      ;

resourcestring
  STR_01 = 'Error creating RTF for image insertion on editor: ';
  STR_02 = 'Error processing RTF visible image (\pict) : ';
  STR_03 = 'Error processing RTF hidden image (hyperlink) : ';
  STR_04 = 'Error converting image format: ';




function String2Hex(const Buffer: Ansistring): AnsiString;
begin
  SetLength(result, 2*Length(Buffer));
  BinToHex(@Buffer[1], PAnsiChar(@result[1]), Length(Buffer));
end;

function BinToHexString(const Stream: TMemoryStream): AnsiString; overload;
var
  Pos, LenBin: Integer;
begin
  Pos:= Stream.Position;
  LenBin:= Stream.Size - Pos;
  SetLength(result, 2* LenBin);
  BinToHex(PAnsiChar(Stream.Memory)+Pos, PAnsiChar(@result[1]), Stream.Size);
end;

procedure BinToHexString(const Stream: TMemoryStream; var StringToAppend: AnsiString; ExtraLength: Integer= 0); overload;
var
  Pos, LenStr, LenBin: Integer;
begin
  LenStr:= Length(StringToAppend);
  Pos:= Stream.Position;
  LenBin:= Stream.Size - Pos;
  SetLength(StringToAppend, LenStr + 2* LenBin + ExtraLength);
  BinToHex(PAnsiChar(Stream.Memory)+Pos, PAnsiChar(@StringToAppend[LenStr+1]), LenBin);
end;


{ Equivalent to Classes.HexToBin, but ignoring invalid hexadecimal characters (<> '0'..'f')
  Intended to ignore possible #D#A characters }

function HexOnlyToBin(Text, Buffer: PAnsiChar; BufSize: Integer; TextOffset: Integer= 0): Integer;
const
  Convert: array['0'..'f'] of Byte = (
     $0,  $1,  $2,  $3,  $4,  $5,  $6,  $7,  $8,  $9, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF,  $A,  $B,  $C,  $D,  $E,  $F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
    $FF,  $A,  $B,  $C,  $D,  $E,  $F);

var
  I, N: Integer;
  b1,b2: Byte;
begin
  Inc(Text, TextOffset);
  I:= BufSize;
  N:= 0;
  while I > 0 do begin
    if (Text[0] in ['0'..'f']) and (Text[1] in ['0'..'f']) then begin
       b1:= Convert[Text[0]];
       b2:= Convert[Text[1]];
       if (b1 <> $FF) and (b2 <> $FF ) then begin
          Buffer[0] := AnsiChar((b1 shl 4) + b2);
          Inc(Buffer);
          Inc(N);
       end;
    end;
    Inc(Text, 2);
    Dec(I);
  end;
  Result := N;
end;


procedure HexStringToBin(const HexStr: AnsiString; const outStream: TMemoryStream;
                         TextOffset: Integer= 0; MaxSize: Integer= integer.MaxValue); overload;
var
   BufSize, Pos, N: Integer;
begin
  BufSize:= Min(MaxSize, Length(HexStr)-TextOffset) div 2;

  if BufSize > 0 then begin
     Pos:= outStream.Position;
     outStream.Size := Pos + BufSize;
     N:= HexOnlyToBin(PAnsiChar(HexStr), PAnsiChar(outStream.Memory)+Pos, BufSize, TextOffset);
     outStream.Size:= Pos + N;        // Non-hexadecimal characters (e.g. $D$A) are ignored
  end;
end;

procedure HexStringToBin(const HexStr: AnsiString; const FileName: string;
                         TextOffset: Integer= 0; MaxSize: Integer= integer.MaxValue); overload;
var
  BinaryStream: TMemoryStream;
begin
  BinaryStream := TMemoryStream.Create;
  try
    HexStringToBin(HexStr, BinaryStream, TextOffset, MaxSize);
    BinaryStream.SaveToFile(FileName);

  finally
    BinaryStream.Free;
  end;
end;


function GetImageFormat(Stream: TMemoryStream): TImageFormat; overload;
var
  Pict: TSynPicture;
begin
  Pict:= TSynPicture.Create();
  try
     Pict.LoadFromStream(Stream);
     Result:= Pict.GetNativeImageFormat;
  finally
     Pict.Free;
  end;

end;


function GetGDIPPictureType(Format: TImageFormat): TGDIPPictureType;
begin
   case Format of
     imgGIF: Result:= gptGIF;
     imgPNG: Result:= gptPNG;
     imgJPG: Result:= gptJPG;
     imgBMP: Result:= gptBMP;
     imgTIF: Result:= gptTIF;
     imgWMF, imgEMF:
             Result:= gptPNG;
   end;
end;




//==========================================================================================
//                                         WMF Images
//==========================================================================================

{ References:

   [MS-WMF]: Windows Metafile Format     : https://msdn.microsoft.com/en-us/library/cc250370.aspx
   Windows-Format Metafiles (WMF) [MSDN] : https://msdn.microsoft.com/es-es/library/windows/desktop/dd145202(v=vs.85).aspx
}


{
 META_PLACEABLE Record

- Key (4 bytes): Identification value that indicates the presence of a placeable metafile header. This value MUST be 0x9AC6CDD7.
- HWmf (2 bytes): The resource handle to the metafile, when the metafile is in memory. When the metafile is on disk, this field
  MUST contain 0x0000. This attribute of the metafile is specified in the Type field of the META_HEADER record.
- BoundingBox (8 bytes): The rectangle in the playback context (or simply the destination rectangle), measured in logical units,
  for displaying the metafile. The size of a logical unit is specified by the Inch field.
- Inch (2 bytes): The number of logical units per inch used to represent the image. This value can be used to scale an image.
  By convention, an image is considered to be recorded at 1440 logical units (twips) per inch. Thus, a value of 720 specifies
  that the image SHOULD be rendered at twice its normal size, and a value of 2880 specifies that the image SHOULD be rendered
  at half its normal size.
- Reserved (4 bytes): A field that is not used and MUST be set to 0x00000000.
- Checksum (2 bytes): A checksum for the previous 10 16-bit values in the header. This value can be used to determine whether
  the metafile has become corrupted.
}

type
  TMetafileHeader = packed record
    Key: Longint;
    Handle: SmallInt;
    Box: TSmallRect;
    Inch: Word;
    Reserved: Longint;
    CheckSum: Word;
  end;

  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault            = $0,
    EmfToWmfBitsFlagsEmbedEmf           = $1,
    EmfToWmfBitsFlagsIncludePlaceable   = $2,
    EmfToWmfBitsFlagsNoXORClip          = $4
  );

function GdipEmfToWmfBits (hemf: HENHMETAFILE; cbData16: UINT; pData16: PByte;
                           iMapMode: Integer; eFlags: EmfToWmfBitsFlags): UINT; stdcall; external 'gdiplus.dll';


const
  WMFKey = Integer($9AC6CDD7);
  WMFWord = $CDD7;
  HundredthMMPerInch = 2540;
  ScreenLogPixels = 96;


function PixelsToHundredthMM(Pixels: integer): integer; inline;
begin
  Result:= MulDiv(Pixels, HundredthMMPerInch, ScreenLogPixels);
end;

function HundredthMMToPixels(HundredthMM: integer): integer; inline;
begin
  Result:= MulDiv(HundredthMM, ScreenLogPixels, HundredthMMPerInch);
end;


{
NOTE: Based on the documentation on the WMF format specification ([MS-WMF].pdf, included within
Windows_Server_Protocols.zip, downloadable from https://msdn.microsoft.com/en-us/library/cc215212.aspx)
I can verify that the beginning of the WMFs that appear in RichEdit correspond to META_HEADER Record (not META_PLACEABLE Record)
That is, it appears that the RichEdit control expects original, 'device-specific' metafiles.

<<
Original WMF metafiles were device-specific; that is, the graphical images they contained would only be rendered
correctly if played back on the output device for which they were recorded. To overcome this limitation,
"placeable" WMF metafiles were developed, which contain an extension to the standard header with information about
the placement and scaling of the image.
>>

The META_PLACEABLE header occupies 22 bytes, and begins with 4 bytes of the Key field: 0x9AC6CDD7
(saved as D7 CD C6 9A --Little-endian)
}


function ComputeWMFChecksum(var WMF: TMetafileHeader): Word;
var
  i: Integer;
  pW: ^Word;
begin
  Result:= 0;
  pW:= @WMF;
  for i:= 0 to 9 do begin
     Result := Result xor pW^;
     Inc(pW);
  end;
end;


procedure AddWMFHeader(Stream: TMemoryStream; W_Pixels, H_Pixels: Integer);
var
  WMF: TMetafileHeader;
begin
   with WMF do begin
      Key:= WMFKEY;
      Handle:= 0;
      Reserved:= 0;
      Box.Left:= 0;
      Box.Top:= 0;
      Box.Right:=  W_Pixels;
      Box.Bottom:= H_Pixels;
      Inch:= ScreenLogPixels;
      Checksum:= ComputeWMFChecksum(WMF);
   end;
   Stream.Write(WMF, SizeOf(TMetafileHeader));
end;

// ------------------------------------------------------------------



function ImgStreamToMetafile (Stream: TMemoryStream; PixelFormat: TPixelFormat= pfCustom): TMetaFile;
var
  MF: TMetaFile;
  Pic: TSynPicture;
  bmp: TBitMap;
  hdcMem, emfdc, RefDC: HDC;
  hMF: HENHMETAFILE;
  NumBytes: Integer;

begin
   Pic := TSynPicture.Create;
   MF:= TMetaFile.Create;
   try
       Pic.LoadFromStream(Stream);               // will load bmp/gif/tiff/jpeg/png content (and also wmf/emf)
       bmp:= Pic.ToBitmap;                       // It uses pf24bit
       if PixelFormat <> pfCustom then
          bmp.PixelFormat:= PixelFormat;         // TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);

       RefDC:= GetDC(0);
       hdcMem:= CreateCompatibleDC(RefDC);
       SelectObject(hdcMem, bmp.Handle);
       emfdc := CreateEnhMetaFile(hdcMem, 0, nil, nil);
       BitBlt(emfdc, 0,0, bmp.Width, bmp.Height, hdcMem, 0,0, SRCCOPY);
       hMF:= CloseEnhMetaFile(emfdc);
       MF.Handle:= hMF;

       Result:= MF;

   finally
      Pic.Free;
      if assigned(bmp) then
         bmp.Free;
   end;
end;


procedure MetafileToWMFStream (MF: TMetaFile; Stream: TMemoryStream);
var
  NumBytes: Integer;
begin
  // We could also use MF.SveToStream(), with MF.Enhanced:= False, but we would obtain a stream that would take up twice as much...
  // ToDO: Is it possible to make a single call to GdipEmfToWmfBits? In all the examples I see on the internet they make two calls, like here

   NumBytes:= GdipEmfToWmfBits (MF.Handle, 0, 0, MM_ANISOTROPIC, EmfToWmfBitsFlagsIncludePlaceable);
   Stream.Clear;
   Stream.Size:= NumBytes;
   GdipEmfToWmfBits (MF.Handle, NumBytes, Stream.Memory, MM_ANISOTROPIC, EmfToWmfBitsFlagsIncludePlaceable);
end;


//==================================================================================================
// IMAGES  ==> EDITOR (RTF)
//======================================

(* Pictures in RTF
 ----------------------
  References:
     Rich Text Format (RTF) Specification,version 1.6:     http://latex2rtf.sourceforge.net/rtfspec.html
     Pictures:  http://latex2rtf.sourceforge.net/rtfspec_7.html#rtfspec_24

     [MS-WMF]: Windows Metafile Format     : https://msdn.microsoft.com/en-us/library/cc250370.aspx
     Windows-Format Metafiles (WMF) [MSDN] : https://msdn.microsoft.com/es-es/library/windows/desktop/dd145202(v=vs.85).aspx


   {\pict{\*\picprop}\wmetafile8\picw5820\pich3889\picwgoal3300\pichgoal2205 0100090...}
   {\pict{\*\picprop}\emfblip\picw434\pich270\picwgoal26250\pichgoal16320 010000006...}
   {\pict{\*\picprop{\sp{\sn wzDescription}{\sv Image}}{\sp{\sn posv}{\sv 1}}}\pngblip\picw9999\pich6426\picwgoal5669\pichgoal3643 89504e470d00...}
   {\pict{\*\picprop{\sp{\sn wzDescription}{\sv Image}}{\sp{\sn posv}{\sv 1}}}\jpegblip\picw7811\pich6414\picwgoal4428\pichgoal3636 ffd8ffe000...}


 <<
  \picwN 	xExt field if the picture is a Windows metafile; picture width in pixels if the picture is a bitmap or from QuickDraw.
   The N argument is a long integer.
  \pichN 	yExt field if the picture is a Windows metafile; picture height in pixels if the picture is a bitmap or from QuickDraw.
   The N argument is a long integer.
 >>
   From what I observe, N is expressed in hundredths of mm (HundredthMM) if it is a Windows metafile
   In Windows images metafile N must be expressed in hundredths of mm (HundredthMM)
   WMF images assume a resolution of 96 pixels per inch.

   [ NOTE *1 ]
   It is not clear how the dimensions should be expressed in the case of other formats such as jpg (jpegblip) or png (pngblip),
   but from the tests it is observed that it is treated the same as the windows metafile, in hundreds of mm. But I also observe
   that an image in these formats is automatically modified by the RichEdit control, equating the values of picwN and pichN to
   those corresponding to the visible dimensions (picwgoalN and pichgoalN).
   Thus, a jpeg image with a resolution of 1400x957 and displayed as 410x281, and therefore with a header built by KNT in this way:
      \jpegblip\picw37042\pich25321\picwgoal6150\pichgoal4215 .....
   is converted by the control to:
      \jpegblip\picw10848\pich7435\picwgoal6150\pichgoal4215 ...

   This prevents determining the real dimensions of the image from what is reflected in the RTF chain.
   It is better to calculate from its Stream.


  HundredthMM (N) <-> pixel (P)

  PixelsPERInch = ScreenLogPixels = 96
  HundredthMMPerInch = 2540             (1 inch = 2,54 cm = 25,4 mm = 2540  0,01 mm

  P = N HundredthMM/HundredthMMPerInch * PixelsPERInch        (0,01 mm)/[(0,01 mm)/inch]  *  (p/inch) =  inch * (p/inch) = p
    = MulDiv(N, PixelsPERInch, HundredthMMPerInch);

  N = P/ScreenLogPixels * HundredthMMPerInch
    = MulDiv(P, HundredthMMPerInch, ScreenLogPixels);


  \picwgoalN	Desired width of the picture in twips.  The N argument is a long integer.
  \pichgoalN	Desired height of the picture in twips. The N argument is a long integer.

  1 dot = 1/72 inch      (1 inch = 72 dots)
  1 twip = 1/1440 inch -> 1 twip = 72/1440 dots =  1/20 dots -> 1 dot = 20 twips

  In MS.Windows: Default display DPI= 96 PPI    (Macintosh: 72 PPI)
  1 pixel = 1/96 inch   (1 inch = 96 pixels)
  96 pixels = 72 dots -> 1 dot = 96/72 pixels =  1,3333 pixels
  1 pixel = 1/1,3333 dots = 20/1,3333 twips ~= 15 twips


  The above is correct if we want to show the image in its normal size and with the same proportions.
  But it can be used (and RichEdit does) to manage the visible height and width, as a result of
  user settings. In fact, it converts some word controls that it recognizes (like \picscalexN or \picscaleyN)
  in changes to \picwgoalN and/or \pichgoalN
  Note: Crop control words (ex: \piccroptN) do not seem to be recognized by RichEdit
*)



function ImgStreamToRTF(ImgID: integer;
                        const Stream: TMemoryStream; StreamImgFormat: TImageFormat;
                        Width, Height, WidthGoal, HeightGoal: integer;
                        AddRTFHeader: boolean;
                        WMFStreamIncludesMetaFileHeader: boolean = true): AnsiString;
var
  RTFformat: TRTFImageFormat;
  twipsX, twipsY: integer;
  LenRTF: integer;
  StrRTF, RTFHeader, RTFHiddenMark, RTFClose: AnsiString;
  StreamRTF: TMemoryStream;
  LengthHeaderToDiscard: integer;
  ExtraLen: integer;

  MaxWidth: integer;
  RatioRed: Single;
const
   MaxGoalTwips = 32760;

begin
  // StreamImgFormat: Format of the image contained in the stream
  // Width, Height: in pixels
  // WidthGoal, HeightGoal: in pixels

  // Depending on the Stream has been created from a MF.Enhanced = True or False, this will be the size of it and the RTF obtained...

  (*
   From the tests I have done, it seems that the RichEdit control supports a maximum visible value, in any of the dimensions (x or y)
   of approx. 32760 twips (2184 pixels) (1 pixel=15 twips). I have clearly verified it with the image uploaded by Ulfff here:
    https://github.com/dpradov/keynote-nf/issues/623#issuecomment-1824370437
   but also with other images playing with the dimensions.
   When trying to insert that image with its actual size (100%) (=> 16200 x 36000 twips) the control simply ignores it.
   KeyNote successfully processes the image, registers it, generates the RTF string for display, but the RichEdit control ignores the image.
   As a consequence, the only thing that is inserted is the hidden mark with the registered ID, which remains isolated. And right now, post
   processing (by switching to another node, or changing visibility mode) of the note including that isolated hidden mark next to another
   correct image (with its own hidden mark) causes KNT to ignore the second hidden mark and associate the dimensions of the second image 
   to the first hidden mark (image not visible). This is the same thing that happened in Beta 2 when deleting an image via the Delete
   option in the context menu (which I had not controlled)

   Depending on when you saved, the second image (which was being viewed ok) ends up being saved to disk or not.


   Regarding the maximum size, I have verified it with the following tests.
   The example image provided in that link has the following dimensions: 1080 x 2400.
   When trying to represent it at 100%, the string to be generated includes the following header:
    {\rtf1\ansi {\pict{\*\picprop}\jpegblip\picw28575\pich63500\picwgoal16200\pichgoal36000
     100%: 16200/36000

   As I indicated, that image was ignored. If I drag the same image to WordPad I can see that it indicates that it adds it with a display
   of 53%. And it points out that the size can be changed with a zoom between 1 and 91%
   When dragged and dropped: 53%: {\pict{\*\picprop}\wmetafile8\picw28575\pich63500\picwgoal8640\pichgoal19200

      Tam. maximo: 1 - 100 / 1- 91
      1%  -> 162 - 360
      91% -> 14742 - 32760

   I verify that increasing the Goal dimensions above these values causes the image to not be displayed.
   Although there are combinations that without knowing very well why they are shown. For example,
      30400 - 42000: Does not display. Normal
      40400 - 42000: Yes, it is supported. It shows it as 100% but it still says that the maximum is between 1 and 91

   I verify it with other smaller starting images. Ex:

   250 x 204    Tamaño valido: 2 - 874%
   Al arrastrar: 100% {\pict{\*\picprop}\wmetafile8\picw6615\pich5398\picwgoal3750\pichgoal3060
   2%   -> 75 - 61
   874% -> 32759 - 26747  [*]
    [*]: In this case, if we ask WordPad to set the maximum it allows (874%) it saves it incorrectly (picwgoal-32759\pichgoal26747)? 
   When you open the file it doesn't show it... But if we remove the minus sign, it does show it

   120 x 171   Tamaño valido: 3 - 1278%
   Al arrastrar: 100%: {\pict{\*\picprop}\wmetafile8\picw3175\pich4524\picwgoal1800\pichgoal2565
   3%     -> 54 - 77
   1278% -> 23004  - 32781
  *)

  LengthHeaderToDiscard:= 0;

  case StreamImgFormat of
    ImgPNG: RTFFormat:= rtfPngblip;
    imgJPG: RTFFormat:= rtfJpegblip;
    imgWMF: begin
            RTFFormat:= rtfWmetafile8;
            if WMFStreamIncludesMetaFileHeader then
               LengthHeaderToDiscard:= SizeOf(TMetafileHeader);        // We must discard the META_PLACEABLE header
            end;
    imgEMF: RTFFormat:= rtfEmfblip;
    else
        exit('Incorrect stream format');
  end;

  Width:=  PixelsToHundredthMM (Width);
  Height:= PixelsToHundredthMM (Height);

  StreamRTF:= TMemoryStream.Create();
  try
     twipsX:= WidthGoal * 15;
     twipsY:= HeightGoal * 15;

     if (twipsX > MaxGoalTwips) then begin
         RatioRed:= MaxGoalTwips / twipsX;
         twipsX:=  MaxGoalTwips;
         twipsY:= Round(twipsY * RatioRed);
     end;
     if (twipsY > MaxGoalTwips) then begin
         RatioRed:= MaxGoalTwips / twipsY;
         twipsY:=  MaxGoalTwips;
         twipsX:= Round(twipsX * RatioRed);
     end;


     if AddRTFHeader then begin
        RTFHeader:= '{\rtf1\ansi ';
        ExtraLen:= 4;
        RTFClose:= #13#10 + '}}';
     end
     else begin
        RTFHeader:= '';
        ExtraLen:= 3;
        RTFClose:= #13#10 + '}';
     end;

     RTFHiddenMark:= '';
     if ImgID <> 0 then
        RTFHiddenMark:= Format(KNT_RTF_IMG_HIDDEN_MARK, [ImgID]);

     StrRTF:= Format('%s%s{\pict{\*\picprop}\%s\picw%d\pich%d\picwgoal%d\pichgoal%d ' + #13#10,
                      [RTFHeader, RTFHiddenMark, RTF_IMAGE_FORMATS[RTFFormat], Width, Height, twipsX, twipsY]);

     Stream.Position:= LengthHeaderToDiscard;

     BinToHexString(Stream, StrRTF, ExtraLen);            // ExtraLen= 4:  #13#10}}
     LenRTF:= Length(StrRTF);
     Move(RTFClose[1], StrRTF[LenRTF-ExtraLen +1], ExtraLen);

     Result:= StrRTF;

  finally
     StreamRTF.Free;
  end;

end;



function MetaFileToRTF(ImgID: integer; MF: TMetaFile; WidthGoal, HeightGoal: integer): AnsiString;
var
  Stream: TMemoryStream;
  Imgformat: TImageFormat;

begin

  Stream:= TMemoryStream.Create;
  try

    if MF.Enhanced = True then begin
       ImgFormat:= imgEMF;
       MF.SaveToStream(Stream);
    end
    else begin
       ImgFormat:= imgWMF;
       MetafileToWMFStream (MF, Stream);
    end;

    Result:= ImgStreamToRTF(ImgID, Stream, ImgFormat, MF.Width, MF.Height, WidthGoal, HeightGoal, true);

  finally
     Stream.Free;
  end;

end;


procedure  CheckDimensionGoals(Width, Heigh: integer; var WidthGoal, HeightGoal: integer);
var
   MaxWidth: integer;
   Ratio: Single;

begin
   if (KeyOptions.ImgMaxAutoWidthGoal <> 0) then begin

      if (KeyOptions.ImgMaxAutoWidthGoal > 0) then
          MaxWidth:= KeyOptions.ImgMaxAutoWidthGoal
      else begin
          MaxWidth:= ActiveNote.Editor.Width;
          if Form_Main.MMAlternativeMargins.Checked then
             Dec(MaxWidth, KeyOptions.MarginAltLeft + KeyOptions.MarginAltRight + 20)
          else
             Dec(MaxWidth, 35);
      end;

      if (WidthGoal > MaxWidth) then begin
          Ratio:= MaxWidth / WidthGoal;
          WidthGoal:=  MaxWidth;
          HeightGoal:= Round(HeightGoal * Ratio);
      end;
   end;

end;


procedure  GetDimensionsFromStream(Stream: TMemoryStream;
                                   Pic: TSynPicture;
                                   MF: TMetaFile;
                                   var StreamImgFormat: TImageFormat;
                                   var Width, Height, WidthGoal, HeightGoal: integer;
                                   var MFLoaded: boolean);
var
  FreePic, FreeMF: boolean;

begin
   FreePic:= false;
   FreeMF:= false;

   if Pic = nil then begin
      Pic := TSynPicture.Create;
      FreePic:= true;
   end;
   if MF = nil then begin
      MF:= TMetaFile.Create;
      FreeMF:= true;
   end;

   try
      Pic.LoadFromStream(Stream);                        // will load bmp/gif/tiff/jpeg/png content (and also wmf/emf)
      Stream.Position:= 0;
      MFLoaded:= false;

      if (StreamImgFormat = imgUndefined) then
         StreamImgFormat:= Pic.GetNativeImageFormat();

      if (Width = -1) then begin
         if StreamImgFormat in [imgWMF, imgEMF] then begin
            MF.LoadFromStream(Stream);
            MFLoaded:= true;
            Width:=  MF.Width;
            Height:= MF.Height;
         end
         else begin
            Width:=  Pic.Width;
            Height:= Pic.Height;
         end;
      end;
      if (WidthGoal = -1) then begin
         // If WidthGoal and HeightGoal are not forced, set them equal to Width and Heigth
         // and take into account the KeyOptions.ImgMaxAutoWidthGoal option
         WidthGoal:= Width;
         HeightGoal:= Height;
         CheckDimensionGoals (Width, Height, WidthGoal, HeightGoal);
      end;

   finally
      if FreeMF then
         MF.Free;
      if FreePic then
         Pic.Free;
   end;
end;


{ Obtains RTF adapted to the format required by configuration, converting the stream if necessary
 (This converted stream is the one required for the Editor, but not necessarily the one we want the image to be saved in)
}

function GetRTFforImageInsertion(ImgID: integer;
                                 Stream: TMemoryStream;
                                 var StreamImgFormat: TImageFormat;
                                 var Width, Height, WidthGoal, HeightGoal: integer;
                                 AddRTFHeader: boolean;
                                 PreserveOriginalStreamContent: boolean;
                                 MaintainWMF_EMF: boolean = false
                                ): AnsiString;
var
  Pic: TSynPicture;
  MF: TMetaFile;
  StrRTF: AnsiString;
  StreamImgFormatOutput: TImageFormat;
  MFLoaded: boolean;
  FreeStream: boolean;

begin
  FreeStream:= false;

  try
      Pic := TSynPicture.Create;
      MF:= TMetaFile.Create;
      try
         GetDimensionsFromStream(Stream, Pic, MF, StreamImgFormat, Width, Height, WidthGoal, HeightGoal, MFLoaded);

         if KeyOptions.ImgFormatInsideRTF = ifWmetafile8 then begin
            StreamImgFormatOutput:= imgWMF;   // We will have to convert to WMF, unless the stream is already in that format

            if StreamImgFormat = imgWMF then begin
               { If the Stream corresponds to the WMF format we can use it directly.
                 It would be possible to ensure its "optimization" with the help of MetafileToWMFStream, but that would only be necessary if the
                 file has been created in an "improper" way, and I have no reason to assume so.  }
            end
            else begin
                if StreamImgFormat = imgEMF then begin
                   if not MFLoaded then
                      MF.LoadFromStream(Stream)
                end
                else
                   MF:= ImgStreamToMetafile(Stream);

                if PreserveOriginalStreamContent then begin
                   Stream:= TMemoryStream.Create;
                   FreeStream:= true;
                end;

                MetafileToWMFStream (MF, Stream);
            end;
            StrRTF:= ImgStreamToRTF(ImgID, Stream, imgWMF, Width, Height, WidthGoal, HeightGoal, AddRTFHeader);
         end
         else begin     // ImageFormatToRTF = ifAccordingImage
            if MaintainWMF_EMF and (StreamImgFormat in [imgWMF, imgEMF]) then begin
                { If the source file is in those formats, we will insert it like this into the RTF
                  With WMF this will only happen if we are opening a WMF file.
                  The images whose Stream we find as WMF processing the RichEdit control, perhaps pasting from the clipboard,
                  we will convert them to the format that has been configured in ImgDefaultFormatFromClipb. This will happen because before calling this method, it will have been
                  converted the Stream to the desired format (ImgDefaultFormatFromClipb). See ProcessImagesInClipboard
                 }
                StreamImgFormatOutput:= StreamImgFormat;            // We can directly use the Stream without having to go through the MetaFile
                //if not MFLoaded then
                //   MF.LoadFromStream(Stream);
                //StrRTF:=  MetaFileToRTF(MF, WidthGoal, HeightGoal);
            end
            else begin
                case StreamImgFormat of
                  ImgPNG, imgJPG: StreamImgFormatOutput:= StreamImgFormat;             // Nothing to do. The stream is worth it as it comes
                  else begin   // imgGIF, ImgBMP, ImgTIF:
                       if PreserveOriginalStreamContent then begin
                          Stream:= TMemoryStream.Create;
                          FreeStream:= true;
                       end;
                       { *1 Here we just need to use a format recognized by the control. We are interested in using PNG or JPG, and not WMF,
                         but although we could use the option indicated in KeyOptions.ImgDefaultFormatFromClipb, we can actually use PNG directly,
                         which is a lossless option. We are not going to save this image like this (not necessarily), it is just to generate
                         the RTF code. }
                       //StreamImgFormatOutput:= KeyOptions.ImgDefaultFormatFromClipb;    // *1
                       StreamImgFormatOutput:= imgPNG;                                    // *1
                       Stream.Clear;
                       Pic.SaveAs(Stream, GetGDIPPictureType(StreamImgFormatOutput), KeyOptions.ImgCompressionQuality);
                  end;
                end;
            end;
            StrRTF:= ImgStreamToRTF(ImgID, Stream, StreamImgFormatOutput, Width, Height, WidthGoal, HeightGoal, AddRTFHeader);
         end;

         //TFile.WriteAllText('E:\_Pruebas\imagenes\InsertIMAGEInRTF_' + IMAGE_FORMATS[StreamImgFormatOutput] + '.rtf', StrRTF, TEncoding.ANSI);
         Result:= StrRTF;

      finally
         MF.Free;
         Pic.Free;
         if FreeStream then
            Stream.Free;
      end;

  except
     on E : Exception do begin
        Messagedlg( STR_01 + E.Message, mtError, [mbOK], 0 );
        Result:= '';
        exit;
     end;

  end;
end;



//==================================================================================================
// EDITOR (RTF)  ==>  IMAGES
//======================================


function RTFPictToImage(Buffer: Pointer;
                    PictOffset: integer;
                    Stream: TMemoryStream;
                    var imgFormat: TImageFormat;
                    var Width, Height, WidthGoal, HeightGoal: integer;
                    var PosRTFImageEnd: integer;
                    GetStream: boolean= true): boolean;
var
  DefImgRTF: AnsiString;
  RtfImgFormat: TRTFImageFormat;
  FormatRecognized: boolean;
  p1,p2,p3,p4,p5,p6: integer;
  RTF: PAnsiChar;

begin
  Result:= true;

  try
    if GetStream then
       Stream.Clear;

   RTF:= PAnsiChar(Buffer);           // RTF (+ PictOffset): Text that starts with {\pict ...

  (*
     {\pict{\*\picprop}\wmetafile8\picw5820\pich3889\picwgoal3300\pichgoal2205 0100090...}
     {\pict{\*\picprop}\emfblip\picw434\pich270\picwgoal26250\pichgoal16320 010000006...}
     {\pict{\*\picprop{\sp{\sn wzDescription}{\sv Image}}{\sp{\sn posv}{\sv 1}}}\pngblip\picw9999\pich6426\picwgoal5669\pichgoal3643 89504e470d00...}
     {\pict{\*\picprop{\sp{\sn wzDescription}{\sv Image}}{\sp{\sn posv}{\sv 1}}}\jpegblip\picw7811\pich6414\picwgoal4428\pichgoal3636 ffd8ffe000...}
  *)

   SetString(DefImgRTF, PAnsiChar(@RTF[1 + PictOffset]), 300);      // Ensuring to include up to pichgoalN
   p1:= pos(AnsiString('\*\nonshppict'), DefImgRTF);
   if p1 > 0 then
      delete(DefImgRTF, p1, 200);

   // {\*\shppict{\pict{.... }}{\*\nonshppict{\pict{.... }}     Normally in emfblip

   FormatRecognized:= False;
   for RtfImgFormat := Low(RtfImgFormat) to High(RtfImgFormat) do begin
      if pos(RTF_IMAGE_FORMATS[RtfImgFormat], DefImgRTF) > 0 then begin
          FormatRecognized:= true;
          break;
      end;
   end;

   if not FormatRecognized then
      exit(False);

   case RtfImgFormat of
     rtfwmetafile8: imgFormat:= imgWMF;
     rtfEmfblip:    imgFormat:= imgEMF;
     rtfPngblip:    imgFormat:= imgPNG;
     rtfJpegblip:   imgFormat:= imgJPG;
   end;


   // Determine the WidthGoal and HeightGoal values
   // Ex: \picw49\pich49\picwgoal28\pichgoal28
   // Note: it can only come \picgoal...
   //----------
   p1:= pos(AnsiString('\picw'), DefImgRTF);
   p2:= pos(AnsiString('\pich'), DefImgRTF);
   p3:= pos(AnsiString('\picwgoal'), DefImgRTF);
   p4:= pos(AnsiString('\pichgoal'), DefImgRTF);
   p5:= pos(AnsiString(' '), DefImgRTF, p4);

   try
     const L1 = Length('\picw');              // 5
     const L2 = Length('\picwgoal');          // 9

     WidthGoal  := StrToInt(Copy(DefImgRTF, p3 + L2, p4-(p3+L2))) div 15;  // On RTF it is expressed in twips
     HeightGoal := StrToInt(Copy(DefImgRTF, p4 + L2, p5-(p4+L2))) div 15;

     if (p1 < p3) and (p2 < p4) then begin
       Width      := StrToInt(Copy(DefImgRTF, p1 + L1, p2-(p1+L1)));
       Height     := StrToInt(Copy(DefImgRTF, p2 + L1, p3-(p2+L1)));
       //if RtfImgFormat in [rtfwmetafile8, rtfEmfblip] then begin
       Width:=  HundredthMMToPixels (Width);
       Height:= HundredthMMToPixels (Height);
       //end;
     end
     else begin
        Width:=  WidthGoal;
        Height:= HeightGoal;
     end;


   except
     Width      := -1;
     Height     := -1;
     WidthGoal  := -1;
     HeightGoal := -1;
   end;

   // Get a stream with the content of the image in binary
   //----------
   p1:= PosPAnsiChar(' ', RTF, p5 + PictOffset);
   p2:= PosPAnsiChar('}', RTF, p1);

   PosRTFImageEnd:= p2 -1;       // => RTF[PosRTFImageEnd] = '}'

   if GetStream then begin
      if RtfImgFormat = rtfwmetafile8 then
         AddWMFHeader (Stream, Width, Height);           //  META_PLACEABLE header

      HexStringToBin(RTF, Stream, p1, p2-p1-1);
      Stream.Position:= 0;
      //Stream.SaveToFile('E:\_Pruebas\imagenes\desdeClipboard.bin');

      // See [ NOTE *1 ] before function 'ImgStreamToRTF'
      var MFLoaded: boolean;
      Width      := -1;
      GetDimensionsFromStream(Stream, nil, nil, imgFormat, Width, Height, WidthGoal, HeightGoal, MFLoaded);
   end;

 except
     on E : Exception do begin
        Messagedlg( STR_02 + E.Message, mtError, [mbOK], 0 );
        Result:= false;
     end;
 end;

end;



function RTFLinkToImage(Buffer: Pointer;
                    PictOffset: integer;
                    var WidthGoal, HeightGoal: integer;
                    var PosRTFImageEnd: integer
                    ): boolean;
var
  p1,p2,p3: integer;
  pIni: integer;
  RTF: PAnsiChar;

begin
  Result:= true;

  try
   // RTF (+ PictOffset): Text that starts with {\field{\*\fldinst{HYPERLINK "img: ...
   RTF:= PAnsiChar(Buffer);


  (*
   KNT_IMG_LINK = '{\field{\*\fldinst{HYPERLINK "img:%d,%d,%d"}}{\fldrslt{\ul\cf1 %s}}}';
                   {\field{\*\fldinst{HYPERLINK "img:ImgID,WGoal,HGoal"}}{\fldrslt{\ul\cf1 textOfHyperlink}}}

                   {\field{\*\fldinst{HYPERLINK "img:999999,2000,2000"}}{\fldrslt{\ul\cf1 textOfHyperlink}}}

   KNT_IMG_LINK_PREFIX = '{\field{\*\fldinst{HYPERLINK "img:'
  *)

   pIni:= 1 + PictOffset + Length(KNT_IMG_LINK_PREFIX);

   p1:= PosPAnsiChar(',', RTF, pIni);         // [pIni, p1] -> ID
   p2:= PosPAnsiChar(',', RTF, p1+1);         // [p1, p2]   -> WidthGoal
   p3:= PosPAnsiChar('"', RTF, p2);           // [p2, p3]   -> HeightGoal

   try
     WidthGoal  := StrToInt(CopyPAnsiChar(RTF, p1 + 1, p2- p1-1));
     HeightGoal := StrToInt(CopyPAnsiChar(RTF, p2 + 1, p3- p2-1));

   except
     WidthGoal  := -1;
     HeightGoal := -1;
   end;

   p1:= PosPAnsiChar('}}}', RTF, p3);

   PosRTFImageEnd:= p1+2  -1;      // Point to the last character => RTF[PosRTFImageEnd] = '}'

 except
     on E : Exception do begin
        Messagedlg( STR_03 + E.Message, mtError, [mbOK], 0 );
        Result:= false;
     end;
 end;

end;


function GetImageIDinPlaintext (Str: String): integer;
var
  L, R: integer;
begin
  Result:= 0;              // 0 is not a valid ID
  // <L>I999999<R>
   L:= pos(KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_IMAGE, Str);
   R:= pos(KNT_RTF_HIDDEN_MARK_R_CHAR, Str);
   Result:= StrToIntDef(Copy(Str, L+2, R-L-2), 0);
end;

function GetImageIDinURLstr (Str: String): integer;
var
    p: integer;
begin
   // img:ID,W.H
   if not Str.StartsWith('img:') then exit;
   p:= Pos(',', Str, 5);
   Result:= StrToIntDef(Copy(Str, 5, p-5), 0);
end;



function CountRTFLinkChars(RTF: PAnsiChar; LinkOffset: integer): integer;
var
  p1,p2,p3,p4: integer;
  pIni: integer;
  strAux: AnsiString;
  CharsHidden, CharsVisible: integer;

begin
  Result:= 0;

  try
   // RTF (+ LinkOffset): Text that starts with {\field{\*\fldinst{HYPERLINK "img: ...

   //  {\field{\*\fldinst{HYPERLINK "img:ImgID,WGoal,HGoal"}}{\fldrslt{\ul\cf1 textOfHyperlink}}}
   //  {\field{\*\fldinst{HYPERLINK "img:1,255,142"}}{\fldrslt {NOTE1\\1_Image_15nov.png}}}


   p1:= PosPAnsiChar('HYPERLINK', RTF, LinkOffset);
   p2:= PosPAnsiChar('"', RTF, p1 + Length('HYPERLINK')+2);
   p3:= PosPAnsiChar(' ', RTF, p2+1);
   if RTF[p3] = '{' then
      inc(p3);
   p4:= PosPAnsiChar('}', RTF, p3+1);

   // Los caracteres \ los hemos debido escapar duplicándolos ->
   StrAux:= CopyPAnsiChar(RTF, p3+1,p4-p3-1);
   StrAux:= StringReplace(StrAux,'\\','*', [rfReplaceAll]);

   CharsHidden:= (p2-p1)+1;
   CharsVisible:= Length(StrAux);
   Result:= CharsHidden + CharsVisible;

 except
     on E : Exception do begin
        Result:= 0;
     end;
 end;

end;



// ---------------------------------------------------------------------------

procedure ConvertStreamToImgFormatDest (Stream: TMemoryStream;
                                       const ImgFormat: TImageFormat;
                                       var ImgFormatDest: TImageFormat);
var
  NeedConvert: boolean;
  Pic: TSynPicture;
  MF: TMetaFile;
  NumBytes: Integer;
  StreamAux: TMemoryStream;

begin
  // NOTE. When inserting into RTF with the emfblip format, the control will automatically convert it to {\*\shppict{\pict{.... }}{\*\nonshppict{\pict{.... }}
  // But this is not how we will save it (unless the new functionality is not activated...)}

  StreamAux:= nil;
  try
     NeedConvert:= false;

     if (ImgFormatDest = imgUndefined) then
        if (ImgFormat in [imgPNG, imgJPG, imgEMF]) then begin
            ImgFormatDest:= ImgFormat;
           exit;                                 // No need to convert
        end
        else begin
           // To use when image is is BMP, WMF, TIF, GIF  (PNG, JPG o EMF => same formats)
           ImgFormatDest:= KeyOptions.ImgDefaultFormatFromClipb;
        end;


     case ImgFormatDest of
       imgGIF, ImgBMP, ImgTIF: NeedConvert:= true;
       else
          NeedConvert:= (ImgFormat <> ImgFormatDest);
     end;

     if not NeedConvert then
        exit;


     // The Stream needs to be converted to the indicated output format
     // -------------------
     Pic := TSynPicture.Create;
     MF:= nil;
     try

        if (ImgFormatDest <> ImgWMF) and (ImgFormatDest <> ImgEMF) then begin
        {
          When the content of a screenshot is directly pasted from the clipboard (for example with Greesnhot), an image (CF_BITMAP) is detected
          in the clipboard, and in that case the conversion to PNG is identical and the result takes up much less space.
           - Images are available on the clipboard on many other occasions, for example when selecting an image in a browser and indicating Copy Image
          In these cases, the conversion to PNG probably takes up less space than its conversion to JPEG.
          As an example, an image captured from an Explorer window can take up the following (in binary, viewing Stream.Size), approx.:
          - BMP: 775.000   - WMF: 582.000   - PNG: 30700   - JPG: 39.000    (JPG occupies 1.3 times more than the PNG version here)

          If that same image is pasted into WordPad and from there it is copied back to the clipboard, at the time of pasting we will no longer
          have a bitmap, but a Picture (Metafile). In this case, an image (CF_BITMAP) is not detected in the clipboard, but rather an embedded object,
          which gives rise to an image and which we can detect through the generated RTF. We can convert the image extracted from the RTF.
          But that conversion, even if it is good, is not as perfect as the one that it occurs when we directly detect the image, and they take up slightly more.
          (Note: A Picture (Device independent bitmap) is offered as CF_BITMAP)


          When what is pasted is the content of a photograph, in that case it is clearly more favorable to use the JPEG format over PNG.
          As an example, a relatively small image, of a photograph with much of the image in a soft gradient, without much detail,
          it can occupy 538,499 in PNG and 34,067 in JPEG (15.8 times more in PNG)

          Option: ImgRatioSizePngVsJPG: // Related with ImgDefaultFormatFromClipb. Value > 0 => A conversion will be made to both formats
           (Png and Jpg). If the default format has a size larger than the alternative, in a proportion >= than indicated, the alternative format will be used
         }

            Pic.LoadFromStream(Stream);

            var ImgFormatAlt: TImageFormat;

            if KeyOptions.ImgRatioSizePngVsJPG > 0 then begin
              if ImgFormatDest <> imgJPG then
                 ImgFormatAlt:= imgJPG
              else
                 ImgFormatAlt:= imgPNG;

              StreamAux:= TMemoryStream.Create;
              Pic.SaveAs(StreamAux, GetGDIPPictureType(ImgFormatAlt), KeyOptions.ImgCompressionQuality);
              StreamAux.Position:= 0;
            end;

            Stream.Clear;
            Pic.SaveAs(Stream, GetGDIPPictureType(ImgFormatDest), KeyOptions.ImgCompressionQuality);

            if (KeyOptions.ImgRatioSizePngVsJPG > 0) and (StreamAux.Size <> 0) and (Stream.Size / StreamAux.Size > KeyOptions.ImgRatioSizePngVsJPG) then begin
               Stream.Clear;
               StreamAux.SaveToStream(Stream);
               ImgFormatDest:= ImgFormatAlt;
            end;
            Stream.Position:= 0;
        end
        else begin
            if (ImgFormat <> imgWMF) and (ImgFormat <> imgEMF) then
               MF:= ImgStreamToMetafile(Stream, KeyOptions.ImgBmpPixelFormat)
            else begin
               MF:= TMetaFile.Create;
               MF.LoadFromStream(Stream);
            end;

            case ImgFormatDest of
              imgWMF:
                 MetafileToWMFStream (MF, Stream);
              imgEMF:
                 begin
                   MF.Enhanced:= True;
                   MF.SaveToStream(Stream);
                 end;
            end;
        end;


     finally
        Pic.Free;
        if MF <> nil then
           MF.Free;
        if StreamAux <> nil then
           StreamAux.Free;
     end;


  except
     on E : Exception do begin
        Messagedlg( STR_04 + E.Message, mtError, [mbOK], 0 );
     end;
  end;


end;


initialization


end.
