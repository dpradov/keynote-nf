unit RichPrint;

(*******************************************************************************

TRichPrinter Version 3.3
30/07/99
Copyright (c) 1998-1999 Gerrit Wolsink

This is a non-visual VCL component that encapsulates the Printer object.

It requires long strings which means it can't be used with Delphi 1.0.
It works fine with Delphi 2.0, 3.0 and 4.0 I haven't tested it yet in C++ Builder,
but it should also be functioning there.
It supports the normal RichEdit and the RichEdEx component:
    procedure PrintRichEdit(const PRichEditorTrans: TRichEdit; StartPage: Integer);
    procedure PrintRichEditPreview(const PRichEditorTrans: TRichEdit);
In fact it supports all components which are decendants of TCustomRichEdit

For more information see the included Doc file.

*******************************************************************************)

{ $LONGSTRINGS ON }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Printers, StdCtrls, ExtCtrls,ComCtrls,CommDlg,Registry,
  ComStrs, RichEdit;


const
     TokenSeparator = '|';

     {In Pixels}
     DefaultDPI = 600;
     DefaultBorderWidth = 2;
     {In Inches}
     DefaultPhysicalPageHeightIn = 11.0;
     DefaultPhysicalPageWidthIn = 8.5;
     DefaultAvailablePageHeightIn = 10.5;
     DefaultAvailablePageWidthIn = 8.0;
     DefaultGutterLeftIn = 0.25;
     DefaultGutterTopIn = 0.25;
     {In Millimeters}
     DefaultPhysicalPageHeightMm = 297.0;
     DefaultPhysicalPageWidthMm = 210.0;
     DefaultAvailablePageHeightMm = 284.0;
     DefaultAvailablePageWidthMm = 198.0;
     DefaultGutterLeftMm = 6.0;
     DefaultGutterTopMm = 6.0;

     {These are expanded only in
     Headers and Footers.}
     LineField = '{$LINE}';
     PageField = '{$PAGE}';
     DateField = '{$DATE}';
     TimeField = '{$TIME}';
     TitleField = '{$TITLE}';

var
  MetafileRichPrint: TMetafile;
  MetaFileCanvasRichPrint: TMetafileCanvas;
  RatioPage: Double;

type
  ELinePrinter = class(EPrinter);

  {These are declared so you can tell at a glance what
  a property or function's return value is used for.}
  TMeasurement = Double;
  TPixels = Cardinal;

  TRichPrntProgDlg = class(TForm)
    Bevel: TBevel;
    lblStatus: TLabel;
    lblTitle: TLabel;
    lblPageNumDesc: TLabel;
    lblPageNumber: TLabel;
    lblLineNumDesc: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


  TPageSetupFlags = (poDisableMargins,poMargins,poDisableOrientation,poDisablePaper,poDisablePrinter);
  TPageOptions    = Set Of TPageSetupFlags;

  TMeasureUnit = (muMillimeters,muInches);
  TPageBorderStyle = (pbTop, pbBottom, pbLeft, pbRight);
  TPageBorders = set of TPageBorderStyle;

 TRichPrinter = class(TComponent)
  private
    { Private declarations }
    fCanvas: TCanvas;
    fPrntProgDlg: TRichPrntProgDlg;
    StartPagePaper: integer;

    fLineNumber: Cardinal;
    fMarginTop, fMarginBottom, fMarginLeft, fMarginRight: TMeasurement;
    fLogoTop,fLogoLeft,fLogoWidth,fLogoHeight: TMeasurement;
    fMeasureUnit: TMeasureUnit;
    fHeader, fFooter: String;
    fPageBorders: TPageBorders;
    fTextMetrics: TTextMetric;
    HeaderFontCopy: TFont;
    fShowProgress: Boolean;
    fStillCreating: Boolean;
    fBorderOffset: TMeasurement;
    fHeaderFormat, fFooterFormat: String;
    fMaxHeightChar: Integer;
    TableFormat: String;
    RichPrintPreview: Boolean;

    PRichEditor: TCustomRichEdit;
    {These X,Y are relative to the printable space.
    They should normally be bounded by the Margins.
    So 0,0 is the left,top corner of the printable space.
    fCurrentY is negative only when printing the header.}
    fCurrentX, fCurrentY: Integer;
    fLineSpace: TPixels;

    fOnBeginDoc: TNotifyEvent;
    fOnEndDoc: TNotifyEvent;
    fOnAbortDoc: TNotifyEvent;
    fOnNewPage: TNotifyEvent;

    FOptions_Dialog : TPageOptions;
    FFlags   : Longint;
    FPaperLength,FPaperWidth: Short;
    FBitmapLogo: TBitmap;
    fSaveMargins: Boolean;
    fCompressPreview: Boolean;

    DefaultColWidth: TMeasurement;

    procedure SetOptions(Value : TPageOptions);
    procedure FillingOnePage(LaatstePagina: Boolean);

    function GetAborted: Boolean;
    function GetFont: TFont;
    function GetOrientation: TPrinterOrientation;
    function GetAvailablePageHeight: TMeasurement;
    function GetAvailablePageWidth: TMeasurement;
    function GetPageNumber: Cardinal;
    function GetTitle: String;
    function GetGutterTop: TMeasurement;
    function GetGutterBottom: TMeasurement;
    function GetGutterLeft: TMeasurement;
    function GetGutterRight: TMeasurement;

    procedure SetMarginTop(Value: TMeasurement);
    function  GetMarginTop:TMeasurement;
    procedure SetMarginBottom(Value: TMeasurement);
    function  GetMarginBottom:TMeasurement;
    procedure SetMarginLeft(Value: TMeasurement);
    function  GetMarginLeft:TMeasurement;
    procedure SetMarginRight(Value: TMeasurement);
    function  GetMarginRight:TMeasurement;
    procedure SetMeasureUnit(Value: TMeasureUnit);
    procedure SetLineSpacing;
    procedure SetHeader(Value: String);
    procedure SetFooter(Value: String);
    procedure SetPageBorders(Value: TPageBorders);
    procedure SetLogoTop(Value: TMeasurement);
    procedure SetLogoLeft(Value: TMeasurement);
    procedure SetLogoWidth(Value: TMeasurement);
    procedure SetLogoHeight(Value: TMeasurement);

    procedure SetFont(Value: TFont);
    procedure SetOrientation(Value: TPrinterOrientation);
    procedure SetTitle(Value: String);
    procedure SetShowProgress(Value: Boolean);
    procedure SetBorderOffset(Value: TMeasurement);
    procedure SetHeaderFormat(Value: String);
    procedure SetFooterFormat(Value: String);
    procedure SetBitmapLogo(Value: TBitmap);
    procedure SetSaveRegistry(Value: Boolean);
    procedure SetCompressPreview(Value: Boolean);

    function GetPhysicalPageHeight: TMeasurement;
    function GetPhysicalPageWidth: TMeasurement;
    function PixelPrintWidth: TPixels;
    function PixelPrintHeight: TPixels;
    function StartingLeft: TPixels;
    function StartingRight: TPixels;
    function StartingTop: TPixels;
    function StartingBottom: TPixels;
    procedure WriteIniFileCompress;

  protected
    { Protected declarations }
    procedure SplitLineAndPrint(const Line: String);
    procedure DoNewPageProcessing;
    procedure UpdateProgressDlg(const Status: String);
    function GetClippedLine(const Line: String; const Width: TPixels): String;
    function MeasureUnitsToPixelsH(const M: TMeasurement): TPixels;
    function MeasureUnitsToPixelsV(const M: TMeasurement): TPixels;
    function PixelsToMeasureUnitsH(const P: TPixels): TMeasurement;
    function PixelsToMeasureUnitsV(const P: TPixels): TMeasurement;
    procedure SetPixelsPerInch;
    function ExpandLogicalFields(S: String): String;

  public
    fPageNumber: Cardinal;
    { Public declarations }
    property Aborted: Boolean read GetAborted;
{    property Canvas: TCanvas read fCanvas write fCanvas;}
    property LineNumber: Cardinal read fLineNumber;
    //This is the Printer.PageHeight/Width property converted to TMeasurement.
    //It's the largest available printable space per page.
    property AvailablePageHeight: TMeasurement read GetAvailablePageHeight;
    property AvailablePageWidth: TMeasurement read GetAvailablePageWidth;
    //This is how large the piece of paper physically is.
    property PhysicalPageHeight: TMeasurement read GetPhysicalPageHeight;
    property PhysicalPageWidth: TMeasurement read GetPhysicalPageWidth;
    //This is the printable area determined by the margins.
    property PageNumber: Cardinal read GetPageNumber;

    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure AbortDoc;
    procedure BeginDoc;
    procedure EndDoc;
    function NewPage: Cardinal;
    function NewLine: Cardinal;
    procedure WriteLine(const Line: String);
    procedure WriteTableLine(const Line: String);
    procedure PrintRichEdit(const PRichEditorTrans: TCustomRichEdit; StartPage: Integer);
    procedure PrintRichEditPreview(const PRichEditorTrans: TCustomRichEdit);

    function PageDialog: Boolean;
    procedure Refresh;

    procedure ReadIniFile;
    procedure WriteIniFile;
    procedure MakePreviewMap;

  published
    { Published declarations }
    {The MeasureUnit should be the first in the published part; otherwise not all things work well}
    property MeasureUnit: TMeasureUnit read fMeasureUnit write SetMeasureUnit default muMillimeters;
    property CompressPreview: Boolean read fCompressPreview write SetCompressPreview default true;
    property SaveMargins: Boolean read fSaveMargins write SetSaveRegistry default false;
    property BitmapLogo: TBitmap read FBitmapLogo write SetBitmapLogo;
    property MarginTop: TMeasurement read GetMarginTop write SetMarginTop;
    property MarginBottom: TMeasurement read GetMarginBottom write SetMarginBottom;
    property MarginLeft: TMeasurement read GetMarginLeft write SetMarginLeft;
    property MarginRight: TMeasurement read GetMarginRight write SetMarginRight;
    property LogoTop: TMeasurement read fLogoTop write SetLogoTop;
    property LogoLeft: TMeasurement read fLogoLeft write SetLogoLeft;
    property LogoWidth: TMeasurement read fLogoWidth write SetLogoWidth;
    property LogoHeight: TMeasurement read fLogoHeight write SetLogoHeight;

    property Header: String read fHeader write SetHeader nodefault;
    property HeaderFormat: String read fHeaderFormat write SetHeaderFormat;
    property Footer: String read fFooter write SetFooter nodefault;
    property FooterFormat: String read fFooterFormat write SetFooterFormat;
    property PageBorders: TPageBorders read fPageBorders write SetPageBorders default [];
    property ShowProgress: Boolean read fShowProgress write SetShowProgress default true;
    property Font_HeaderFooter: TFont read GetFont write SetFont;
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation default poPortrait;
    property Title: String read GetTitle write SetTitle nodefault;
    property BorderOffset: TMeasurement read fBorderOffset write SetBorderOffset;
    property GutterTop: TMeasurement read GetGutterTop;
    property GutterBottom: TMeasurement read GetGutterBottom;
    property GutterLeft: TMeasurement read GetGutterLeft;
    property GutterRight: TMeasurement read GetGutterRight;

    property OnBeginDoc: TNotifyEvent read fOnBeginDoc write fOnBeginDoc;
    property OnEndDoc: TNotifyEvent read fOnEndDoc write fOnEndDoc;
    property OnAbortDoc: TNotifyEvent read fOnAbortDoc write fOnAbortDoc;
    property OnNewPage: TNotifyEvent read fOnNewPage write fOnNewPage;

    Property Options_Dialog : TPageOptions Read FOptions_Dialog Write SetOptions;

    Property PaperLength     : Short    Read FPaperLength;
    Property PaperWidth      : Short    Read FPaperWidth;

  end;



procedure Register;
function ReplaceSubString(OldSubStr, NewSubStr, S: String): String;
procedure ParseFormatToken(var CurToken: String; var CurAlignment: TAlignment; var CurWidth: TMeasurement; const DefaultColWidth: TMeasurement);
procedure TokenizeString(const S: String; Tokens: TStringList);
function StripBackToWhiteSpace(const S: String): String;

{$R RichPrntProg.dfm}


implementation

{=============================================================================}
{ Non-methods that may prove useful elsewhere.                                }
{=============================================================================}

uses RichPreview;

var FloatType: Set of Char=['-','+','.','0','1','2','3','4','5','6','7','8','9','E','e'];


function ReplaceSubString(OldSubStr, NewSubStr, S: String): String;
var
 P: Cardinal;
begin
 {First, make sure old isn't contained in new.
 This gets around the infinite loop situation.
 If old is in new, we just return S unmodified.}
 P:=Pos(OldSubStr, NewSubStr);
 if P = 0 then
 begin
  P:=Pos(OldSubStr, S);
  while P > 0 do
  begin
   S:=Copy(S, 1, P-1)+NewSubStr+Copy(S, P+Length(OldSubStr), Length(S));
   P:=Pos(OldSubStr, S);
  end;
 end;
 Result:=S;
end;

procedure ParseFormatToken(var CurToken: String; var CurAlignment: TAlignment; var CurWidth: TMeasurement; const DefaultColWidth: TMeasurement);
begin
 if CurToken = '' then CurToken:='<'+FloatToStr(DefaultColWidth);
 if Length(CurToken) = 1 then
   if (CurToken[1] in ['<', '^', '>']) then CurToken:=CurToken+FloatToStr(DefaultColWidth);

     {Alignment}
  case CurToken[1] of
      '<': begin
            CurAlignment:=taLeftJustify;
            Delete(CurToken, 1, 1);
           end;
      '^': begin
            CurAlignment:=taCenter;
            Delete(CurToken, 1, 1);
           end;
      '>': begin
            CurAlignment:=taRightJustify;
            Delete(CurToken, 1, 1);
           end;
       else
         CurAlignment:=taLeftJustify;
   end;

     {Width}
   try
    CurWidth:=StrToFloat(CurToken);
   except
    on EConvertError do CurWidth:=DefaultColWidth;
   end;
end;

procedure TokenizeString(const S: String; Tokens: TStringList);
var
 i, Len: Cardinal;
 CurToken: String;
begin
 Tokens.Clear;
 CurToken:='';
 Len:=Length(S);
 for i:=1 to Len do
 begin
  if S[i] = TokenSeparator then
  begin
   Tokens.Add(CurToken);
   CurToken:='';
  end
 else CurToken:=CurToken+S[i];
 end;
 Tokens.Add(CurToken);
end;

function StripBackToWhiteSpace(const S: String): String;
var
 i, Len, Mark: Cardinal;
begin
 Mark:=0;
 Len:=Length(S);
 for i:=Len downto 1 do
 begin
  if S[i] in [#0..#32] then
  begin
   Mark:=i;
   Break;
  end;
 end;
  if Mark > 0 then Result:=Copy(S, 1, Mark)
  {If there is nowhere to break, just return the whole line.}
 else Result:=S;
end;


{=============================================================================}
{ Public stuff for TRichPrinter.                                              }
{=============================================================================}

constructor TRichPrinter.Create(Owner: TComponent);
begin
 fStillCreating:=True;
 inherited Create(Owner);
 {Make sure things don't blow up if there is no printer.}
 try
  fCanvas:=Printer.Canvas;
 except
  on EPrinter do
  begin
   if csDesigning in ComponentState then
      MessageDlg('There have to be at least one printer installed will printing functioning.',
                  mtWarning, [mbOk], 0);
   end;
  end;
  {Note: This is created as a TStringList
  but declared as a TStrings. This is to
  maintain a consistent look with other
  VCL components.  TStrings is used as a
  visible outer layer while TStringList
  is used internally for storage.}

  {Make this explicitly nil so UpdateProgressDlg
  can tell if it needs to Create or Free itself.}
  fPrntProgDlg := nil;
  fCurrentX:=0;
  fCurrentY:=0;
  fLineNumber:=0;
  fPageNumber:=0;
  Font_HeaderFooter.Name := 'Courier New';
  Font_HeaderFooter.Size := 10;
  Font_HeaderFooter.Style:= [];
  PageBorders:=[];
  Orientation:=poPortrait;
  ShowProgress:=true;
  HeaderFormat:='>100';
  Header:='{$TITLE}';
  FooterFormat:='>100';
  Footer:='Page {$PAGE}';
  TableFormat:='';

  Title:='';
  BorderOffset:=2;
  DefaultColWidth:=0;
  fMarginTop:=30.0;
  fMarginBottom:=25.0;
  fMarginLeft:=25.0;
  fMarginRight:=15.0;
  LogoTop:=0.0;
  LogoLeft:=0.0;
  LogoWidth:=25.0;
  LogoHeight:=20.0;
  MeasureUnit:=muMillimeters;
  FOptions_Dialog := [poMargins];
  FBitmapLogo := TBitmap.Create;
  fSaveMargins:=false;
  fCompressPreview:=true;
  RichPrintPreview:=false;
  if fSaveMargins then ReadIniFile;
  fStillCreating:=False;
end;

destructor TRichPrinter.Destroy;
begin
 FBitmapLogo.Free;
 inherited Destroy;
end;

procedure TRichPrinter.AbortDoc;
begin
 try
  UpdateProgressDlg('Aborting Printing');
  try
   if Not RichPrintPreview then Printer.Abort;
    except
     on EInOutError do raise ELinePrinter.Create('Not be able to end the printing to a file.');
     on EPrinter do raise ELinePrinter.Create('Not be able to end the printing.');
    end;
  finally
   UpdateProgressDlg('');
  end;
 {Fire the event handler if it exists.}
 if Assigned(fOnAbortDoc) then fOnAbortDoc(Self);
end;

procedure TRichPrinter.BeginDoc;
begin
 if fSaveMargins then ReadIniFile;
 {Fire the event handler if it exists.}
 if Assigned(fOnBeginDoc) then fOnBeginDoc(Self);
 try
  if Not RichPrintPreview then Printer.BeginDoc;
  //Make sure the font gets sized correctly for the page.
  SetFont(Font_HeaderFooter);
  SetPixelsPerInch;
  GetTextMetrics(fCanvas.Handle, fTextMetrics);
 except
   on E: EInOutError do raise ELinePrinter.Create(E.Message);
   on EPrinter do raise ELinePrinter.Create('Not be able to start the printing.');
 end;
 if Not RichPrintPreview then UpdateProgressDlg('Preparing to Print') else UpdateProgressDlg('Preparing to Print Preview');
 fLineNumber:=0;
 fPageNumber:=StartPagePaper;
 {Make sure the new page processing fires on BeginDoc.}
 DoNewPageProcessing;
end;

procedure TRichPrinter.EndDoc;
begin
 try
  if Not RichPrintPreview then UpdateProgressDlg('Finished Printing') else UpdateProgressDlg('Finished Preparing Preview');
  try
   if Not RichPrintPreview then Printer.EndDoc;
  except
   on EInOutError do raise ELinePrinter.Create('Not be able to print to a file.');
   on EPrinter do raise ELinePrinter.Create('Not be able to end the printing.');
  end;
 finally
   if RichPrintPreview then
   begin
    FillingOnePage(True);
    RichPrintPreview:=false;
   end;
   UpdateProgressDlg('');
  {Fire the event handler if it exists.}
  if Assigned(fOnEndDoc) then fOnEndDoc(Self);
 end;
end;

function TRichPrinter.NewPage: Cardinal;
begin
 try
  if not RichPrintPreview then Printer.NewPage;
  Inc(fPageNumber);
 except
  on EInOutError do raise ELinePrinter.Create('Not be able to print a new page.');
  on EPrinter do raise ELinePrinter.Create('Not be able to print a new page.');
 end;
 if RichPrintPreview then FillingOnePage(False);
 DoNewPageProcessing;
 Result:=fPageNumber;
{ fCanvas.Font.Assign(PRichEditor.SelAttributes);}
end;

function TRichPrinter.NewLine: Cardinal;
begin
 fCurrentX:=0;
 fCurrentY:=fCurrentY+fLineSpace;
 Inc(fLineNumber);

 {See if the entire next line will fit.}
 if (fCurrentY+fLineSpace) >= PixelPrintHeight then NewPage;
 {Fire the event handler if it exists.}
 Result:=LineNumber;
end;

procedure TRichPrinter.WriteLine(const Line: String);
var
 LineWidth: TPixels;
 Buffer: String;
begin
 Buffer:=Line;
 try
  LineWidth:=fCanvas.TextWidth(Buffer);
 except
  on EPrinter do LineWidth:=0;
 end;
 if LineWidth > PixelPrintWidth then
 begin
  SplitLineAndPrint(Buffer);
 end
else
 begin
  fCurrentX:=0;
  {Make sure we don't write off the end of the page.}
  if (fCurrentY+fLineSpace) >= PixelPrintHeight then NewPage;
  {Now print the line.}
  try
   fCanvas.TextOut(StartingLeft+fCurrentX, StartingTop+fCurrentY, Buffer);
  except  on EPrinter do ;
          on EInOutError do ;
  end;
  NewLine;
 end;
end; 

procedure TRichPrinter.WriteTableLine(const Line: String);
var
 FormatTokens, LineTokens: TStringList;
 i, CurWidth, LeftPos, offset: Integer;
 FloatCurWidth: TMeasurement;
 CurAlignment: TAlignment;
 CurToken: String;
begin
 FormatTokens:=TStringList.Create;
 LineTokens:=TStringList.Create;
 try
  TokenizeString(TableFormat, FormatTokens);
  TokenizeString(Line, LineTokens);
  fCurrentX:=StartingLeft;
  for i:=0 to FormatTokens.Count-1 do
  begin
   {Get the Width and Alignment from the current column format.}
   CurToken:=FormatTokens[i];
   ParseFormatToken(CurToken, CurAlignment, FloatCurWidth, DefaultColWidth);
   CurWidth:=MeasureUnitsToPixelsH(FloatCurWidth);
   {Now get a line token even if it's blank.}
   if i < LineTokens.Count then CurToken:=LineTokens[i] else CurToken:='';

   //Expand logical field names (e.g. {$LINE}).
   {The '{$' check is just to speed things up.}
   if Pos('{$', CurToken) > 0 then CurToken:=ExpandLogicalFields(CurToken);

   {Get just what will fit in the current column.}
   CurToken:=GetClippedLine(CurToken, CurWidth);

   try
    {Figure out where the X position will be in the current column.}
    case CurAlignment of
      taCenter: LeftPos:=(CurWidth-fCanvas.TextWidth(CurToken)) shr 1;
      taRightJustify: LeftPos:=CurWidth-fCanvas.TextWidth(CurToken);
     else
      LeftPos:=0;
    end;
   {We try to offset the text so it's not right on a border.}
   Offset:=3*DefaultBorderWidth*(GetDeviceCaps(Printer.Handle, LOGPIXELSX) div DefaultDPI);
   if fCanvas.TextWidth(CurToken) < (CurWidth-Offset) then
   if CurAlignment = taRightJustify then LeftPos:=LeftPos - Offset
      else if CurAlignment = taLeftJustify then LeftPos:=LeftPos + Offset;

   {Print out the current token.}
    fCanvas.TextOut(fCurrentX+LeftPos, fCurrentY+StartingTop, CurToken);
    except
     on EPrinter do ;
    end;

   {Increase fCurrentX by the COLUMN width.}
   fCurrentX:=fCurrentX+CurWidth;
  end;
  finally
   FormatTokens.Free;
   LineTokens.Free;
  end;

  {If we're not printing the Header or Footer, go to a new line.}
  if ((fCurrentY >= 0) and (fCurrentY < PixelPrintHeight)) then NewLine;
end;


procedure TRichPrinter.PrintRichEdit(const PRichEditorTrans: TCustomRichEdit;StartPage: Integer);

var
  Range: TFormatRange;
  LastChar, MaxLen, LogX, LogY, OldMap: Integer;

procedure SetupPageSize;
begin
 with Range do
 begin
  LogX := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  LogY := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  rc.left:=Trunc(StartingLeft*1440/LogX);
  rc.top:=Trunc(StartingTop*1440/LogY);
  rc.right:=Trunc((StartingLeft+PixelPrintWidth)* 1440/LogX);
  rc.Bottom:=Trunc((StartingTop+PixelPrintHeight)* 1440/LogY);
  rcPage := rc;
  hdc := fCanvas.Handle;
  hdcTarget := hdc;
 end;
end;

begin  {body PrintRichEdit}
 PRichEditor:=PRichEditorTrans;
 StartPagePaper:=StartPage;
 FillChar(Range, SizeOf(TFormatRange), 0);
 if RichPrintPreview then
 begin
  with Range do
  begin
   Self.BeginDoc;
   SetupPageSize;
   LastChar := 0;
   MaxLen := PRichEditor.GetTextLen;
   chrg.cpMax := -1;
    // ensure printer DC is in text map mode
   OldMap := SetMapMode(hdc, MM_TEXT);
   SendMessage(fCanvas.Handle, EM_FORMATRANGE, 0, 0);    // flush buffer
   try
    repeat
     chrg.cpMin := LastChar;
     LastChar := SendMessage(PRichEditor.Handle, EM_FORMATRANGE, 1, Longint(@Range));
     if (LastChar < MaxLen) and (LastChar <> -1) then
     begin
      Self.NewPage;
      SetupPageSize;
     end;
    until (LastChar >= MaxLen) or (LastChar = -1) or (LastChar<=chrg.cpMin);
    SendMessage(fCanvas.Handle, EM_FORMATRANGE, 0, 0);  // flush buffer
    Self.EndDoc;
   finally
    SetMapMode(hdc, OldMap);       // restore previous map mode
   end;
  end;
 end
else
 begin
{  fCanvas:=Printer.canvas;}
  with Printer, Range do
  begin
   Self.BeginDoc;
   SetupPageSize;
   LastChar := 0;
   MaxLen := PRichEditor.GetTextLen;
   chrg.cpMax := -1;
   // ensure printer DC is in text map mode
   OldMap := SetMapMode(hdc, MM_TEXT);
   SendMessage(Handle, EM_FORMATRANGE, 0, 0);    // flush buffer
   try
    repeat
     chrg.cpMin := LastChar;
     LastChar := SendMessage(PRichEditor.Handle, EM_FORMATRANGE, 1, Longint(@Range));
     if (LastChar < MaxLen) and (LastChar <> -1) then
     begin
      Self.NewPage;
      SetupPageSize;
     end;
    until (LastChar >= MaxLen) or (LastChar = -1) or (LastChar<=chrg.cpMin);
    SendMessage(Handle, EM_FORMATRANGE, 0, 0);  // flush buffer
    Self.EndDoc;
   finally
    SetMapMode(hdc, OldMap);       // restore previous map mode
   end;
  end;
 end;
end;



procedure TRichPrinter.FillingOnePage(LaatstePagina: Boolean);
begin
 RichPreviewForm.FillImagelist;
 RichPreviewForm.Refresh;
 if Not LaatstePagina then MakePreviewMap;
end;

procedure TRichPrinter.MakePreviewMap;
begin
 RichPrintPreview:=true;
 MetafileRichPrint:=TMetafile.Create;
 MetafileRichPrint.Width:=MeasureUnitsToPixelsH(GetPhysicalPageWidth);
 MetafileRichPrint.Height:=MeasureUnitsToPixelsV(GetPhysicalPageHeight);
 MetaFileCanvasRichPrint:=TMetafileCanvas.Create(MetafileRichPrint,Printer.handle);
 MetaFileCanvasRichPrint.Font.Assign(HeaderFontCopy);
 fCanvas:=MetaFileCanvasRichPrint;
end;

procedure TRichPrinter.PrintRichEditPreview(const PRichEditorTrans: TCustomRichEdit);
var ImageIndex: integer;
    Stop: Boolean;
    FileNameMetafile,FileNameMetafileExt: String;
    FileTyp: file of TMetafile;
begin
 try
  Screen.Cursor:=crHourglass;
  WriteIniFileCompress;
  HeaderFontCopy:=Font_HeaderFooter;
  MakePreviewMap;
  RatioPage:=GetPhysicalPageHeight/GetPhysicalPageWidth;
  RichPreviewForm:=TRichPreviewForm.Create(Application);
  RichPreviewForm.PreviewPaperWidth:=MeasureUnitsToPixelsH(GetPhysicalPageWidth);
  RichPreviewForm.PreviewPaperHeight:=MeasureUnitsToPixelsV(GetPhysicalPageHeight);
  RichPreviewForm.Refresh;
  RichPreviewForm.MeasureUnits:=fMeasureUnit;
  RichPreviewForm.PaperWidthUnits:=GetPhysicalPageWidth;
  RichPreviewForm.PaperHeightUnits:=GetPhysicalPageHeight;
  PrintRichEdit(PRichEditorTrans,1);
  fCanvas:=Printer.Canvas;
 except
  Stop:=false;
  ImageIndex:=0;
  repeat
   str(ImageIndex,FileNameMetafileExt);
   FileNameMetafile:='PreviewMetafile'+FileNameMetafileExt;
   AssignFile(FileTyp,FileNameMetafile);
   {$I-}
   RESET(FileTyp);
   {$I+}
   IF IOResult=0 then
   begin
    closeFile(FileTyp);
    Erase(FileTyp);
   end else Stop:=true;
   ImageIndex:=ImageIndex+1;
  until Stop;
 end;
 Screen.Cursor:=crDefault;
end;

procedure TRichPrinter.Refresh;
begin
 if not Printer.Printing then
 begin
  SetMarginTop(MarginTop);
  SetMarginBottom(MarginBottom);
  SetMarginLeft(MarginLeft);
  SetMarginRight(MarginRight);
  SetBorderOffSet(BorderOffSet);
  SetLogoLeft(LogoLeft);
  SetLogoTop(LogoTop);
  SetLogoWidth(LogoWidth);
  SetLogoHeight(LogoHeight);
  SetHeaderFormat(HeaderFormat);
  SetFooterFormat(FooterFormat);
  SetSaveRegistry(SaveMargins);
 end;
end;

{=============================================================================}
{ Private and Protected stuff for TRichPrinter.                               }
{=============================================================================}

procedure TRichPrinter.SetPixelsPerInch;
var
 FontSize: Integer;
begin
 {This routine gets us around the Delphi tiny font bug.}
 FontSize := fCanvas.Font.Size;
 fCanvas.Font.PixelsPerInch := GetDeviceCaps (fcanvas.Handle, LOGPIXELSY );
 fCanvas.Font.Size := FontSize;
end;

procedure TRichPrinter.DoNewPageProcessing;
var
 PixelBorderOffset: TPixels;
 OldTableFormat: String;
 LogoRect: TRect;
procedure ScaleLogo;
begin
 fCurrentY:=((StartingTop-fLineSpace) shr 1)+fLineSpace;
 fCurrentX:=StartingLeft;
 LogoRect.Top:= fCurrentY+round(LogoTop/GetPhysicalPageHeight*PixelPrintHeight);
 LogoRect.Left:=fCurrentX+round(LogoLeft/GetPhysicalPageWidth*PixelPrintWidth);
 LogoRect.Right:=LogoRect.Left+round(LogoWidth/GetPhysicalPageWidth*PixelPrintWidth);
 LogoRect.Bottom:=LogoRect.Top+round(LogoHeight/GetPhysicalPageHeight*PixelPrintHeight);
end;

begin {body  DoNewPageProcessing}
 UpdateProgressDlg('Currently Printing');

 {Keep TableFormat because we temporarily
 change it for the Header and Footer.}
 OldTableFormat:=TableFormat;
 try

  {Print the header.}
  if Header <> '' then
  begin
   SetFont(Font_HeaderFooter);
   SetPixelsPerInch;
   SetLineSpacing;
   fCurrentY:=(round((StartingTop-fLineSpace)/2))-StartingTop;
   TableFormat:=HeaderFormat;
   WriteTableLine(Header);
   if not BitmapLogo.Empty  then
   begin
    ScaleLogo;
    try
     fCanvas.DrawFocusRect(LogoRect);
     fcanvas.stretchdraw(LogoRect,BitmapLogo);
    except
     on EPrinter do;
    end;
   end;
  end;

 {Print the footer.}
  if Footer <> '' then
  begin
   SetFont(Font_HeaderFooter);
   SetPixelsPerInch;
   SetLineSpacing;
   fCurrentY:=PixelPrintHeight+((StartingBottom-fLineSpace) shr 1);
   TableFormat:=FooterFormat;
   WriteTableLine(Footer);
  end;

  finally
   {Restore the original values.}
   TableFormat:=OldTableFormat;
  end;

  {Reset the fields and fire new page and line events.}
  fCurrentX:=0;
  fCurrentY:=0;
  fLineNumber:=0;
  if Assigned(fOnNewPage) then fOnNewPage(Self);

  {Print the PageBorders.}
  try
   with fCanvas do
   begin
    Pen.Width:=DefaultBorderWidth*(GetDeviceCaps(Printer.Handle, LOGPIXELSY) div DefaultDPI);
    PixelBorderOffset:=MeasureUnitsToPixelsV(BorderOffset);
    if pbTop in PageBorders then
    begin
     MoveTo(StartingLeft-PixelBorderOffset,StartingTop-PixelBorderOffset);
     LineTo(StartingLeft+PixelPrintWidth+PixelBorderOffset, StartingTop-PixelBorderOffset);
    end;
    if pbBottom in PageBorders then
    begin
     MoveTo(StartingLeft-PixelBorderOffset, StartingTop+PixelPrintHeight+2*PixelBorderOffset);
     LineTo(StartingLeft+PixelPrintWidth+PixelBorderOffset, StartingTop+PixelPrintHeight+2*PixelBorderOffset);
    end;
    Pen.Width:=DefaultBorderWidth*(GetDeviceCaps(Printer.Handle, LOGPIXELSX) div DefaultDPI);
    PixelBorderOffset:=MeasureUnitsToPixelsH(BorderOffset);
    if pbLeft in PageBorders then
    begin
     MoveTo(StartingLeft-PixelBorderOffset, StartingTop-PixelBorderOffset);
     LineTo(StartingLeft-PixelBorderOffset, StartingTop+PixelPrintHeight+2*PixelBorderOffset);
    end;
    if pbRight in PageBorders then
    begin
     MoveTo(StartingLeft+PixelPrintWidth+PixelBorderOffset, StartingTop-PixelBorderOffset);
     LineTo(StartingLeft+PixelPrintWidth+PixelBorderOffset, StartingTop+PixelPrintHeight+2*PixelBorderOffset);
    end;
   end;
  except
   on EPrinter do ;
  end;
end;

procedure TRichPrinter.SplitLineAndPrint(const Line: String);
var
 Buffer, CurLine: String;
 Len: Cardinal;
begin
 Buffer:=Line;
 repeat
  CurLine:=GetClippedLine(Buffer, PixelPrintWidth);
  Len:=Length(CurLine);
  {If the next character isn't whitespace, slide back to the nearest.
  Also, like most word processors do, I'm going to delete the
  first leading whitespace character left in the next-line buffer after
  the delete/newline (if one exists).}
  if Len<Length(Buffer) then
  begin
   if not (Buffer[Len+1] in [#0..#32]) then
   begin
    CurLine:=StripBackToWhiteSpace(CurLine);
    Len:=Length(CurLine);
   end
  else Inc(Len);
  end;
  WriteLine(CurLine);
  Delete(Buffer, 1, Len);
 until Buffer='';
end;

function TRichPrinter.GetClippedLine(const Line: String; const Width: TPixels): String;
var
 PixelLen: TPixels;
 StartPos, EndPos, Mid, PreviousMid: Cardinal;
begin
 try
  PixelLen:=fCanvas.TextWidth(Line);
  if PixelLen > Width then
  begin
   EndPos:=Length(Line);
   StartPos:=1;
   Mid:=0;
   repeat
    PreviousMid:=Mid;
    Mid:=(StartPos+EndPos) shr 1;
    PixelLen:=fCanvas.TextWidth(Copy(Line,1,Mid));
    if PixelLen > Width then EndPos:=Mid
      else if PixelLen < Width then StartPos:=Mid
     else StartPos:=EndPos;
   until (Mid=PreviousMid) or (StartPos>=EndPos);
   Result:=Copy(Line, 1, Mid);
  end
 else
  Result:=Line;
  except
   on EPrinter do Result:=Line;
  end;
end;

function TRichPrinter.PixelPrintWidth: TPixels;
begin
 try
  Result:=MeasureUnitsToPixelsH(AvailablePageWidth)-StartingLeft-StartingRight;
 except
  on ERangeError do Result:=0;
 end;
end;

function TRichPrinter.PixelPrintHeight: TPixels;
begin
 try
  Result:=MeasureUnitsToPixelsV(AvailablePageHeight)-StartingTop-StartingBottom;
 except
 on ERangeError do Result:=0;
 end;
end;

function TRichPrinter.GetFont: TFont;
var
 Font: TFont;
begin
 Font:=TFont.Create;
 Font.Name:='Courier New';
 Font.Size:=10;
 Font.Style:=[];

 try
  Result:=fCanvas.Font;
 except
  on EPrinter do
  begin
   Result:=Font;
  end;
 end;
 Font.Free;
end;

procedure TRichPrinter.SetFont(Value: TFont);
begin
 try
  fCanvas.Font.Assign(Value);
  if fCanvas.Font.Color=clWhite then fCanvas.Font.Color:=clBlack;
 except
   on EPrinter do
   begin
    if csDesigning in ComponentState then
      MessageDlg('You must have at least one printer installed to set the Font property for TRichPrinter.',
      mtWarning, [mbOk], 0);
   end;
  end;
  {Force fLineSpace to be updated.}
  SetLineSpacing;
end;

function TRichPrinter.GetTitle: String;
begin
 try
  Result:=Printer.Title;
 except
  on EPrinter do Result:='<Unknown>';
 end;
end;

procedure TRichPrinter.SetTitle(Value: String);
begin
 if not Printer.Printing then
 try
  Printer.Title:=Value
 except
  on EPrinter do
  begin
   if csDesigning in ComponentState then
      MessageDlg('You must have at least one printer installed to set the Title property for TRichPrinter.',
      mtWarning, [mbOk], 0);
  end;
 end
 else raise ELinePrinter.Create('Unable to change title while printing');
end;

function TRichPrinter.GetOrientation: TPrinterOrientation;
begin
 try
  Result:=Printer.Orientation;
 except
  on EPrinter do Result:=poPortrait;
 end;
end;

procedure TRichPrinter.SetOrientation(Value: TPrinterOrientation);
begin
 if not Printer.Printing then
 try
  Printer.Orientation:=Value
 except
 on EPrinter do
 begin
  if csDesigning in ComponentState then
           MessageDlg('You must have at least one printer installed to set the Orientation property for TRichPrinter.',
           mtWarning, [mbOk], 0);
  end;
 end
 else raise ELinePrinter.Create('Unable to change orientation while printing');
end;

function TRichPrinter.GetAborted: Boolean;
begin
 try
  Result:=Printer.Aborted;
 except
  on EPrinter do Result:=True;
 end;
end;

function TRichPrinter.GetAvailablePageHeight: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsV(Printer.PageHeight);
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultAvailablePageHeightIn
  else Result:=DefaultAvailablePageHeightMm;
 end;
end;

function TRichPrinter.GetPageNumber: Cardinal;
begin
 try
  Result:=fPageNumber;
 except
  on EPrinter do Result:=0;
 end;
end;


function TRichPrinter.GetAvailablePageWidth: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsH(Printer.PageWidth);
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultAvailablePageWidthIn
  else Result:=DefaultAvailablePageWidthMm;
 end;
end;

function TRichPrinter.GetMarginTop:TMeasurement;
begin
 Result:=fMarginTop;
end;

procedure TRichPrinter.SetMarginTop(Value: TMeasurement);
begin
 if not Printer.Printing then
 begin
  if Value >= GutterTop then
  begin
   if Value <= (PhysicalPageHeight-GutterBottom) then fMarginTop:=Value
  else
   begin
    fMarginTop:=PhysicalPageHeight-GutterBottom;
   end;
  end
  else fMarginTop:=GutterTop;
 end
 else raise ELinePrinter.Create('Unable to change top margin while printing');
end;

function TRichPrinter.GetMarginBottom:TMeasurement;
begin
 Result:=fMarginBottom;
end;

procedure TRichPrinter.SetMarginBottom(Value: TMeasurement);
begin
 if not Printer.Printing then
 begin
  if Value >= GutterBottom then
  begin
   if Value < (PhysicalPageHeight-GutterTop) then fMarginBottom:=Value
  else
   begin
    fMarginBottom:=PhysicalPageHeight-GutterTop;
   end;
  end
  else fMarginBottom:=GutterBottom;
 end
 else raise ELinePrinter.Create('Unable to change bottom margin while printing');
end;

function TRichPrinter.GetMarginLeft:TMeasurement;
begin
 Result:=fMarginLeft;
end;

procedure TRichPrinter.SetMarginLeft(Value: TMeasurement);
begin
 if not Printer.Printing then
 begin
  if Value >= GutterLeft then
  begin
   if Value <= (PhysicalPageWidth-GutterRight) then fMarginLeft:=Value
  else
   begin
    fMarginLeft:=PhysicalPageWidth-GutterRight;
   end;
  end
 else fMarginLeft:=GutterLeft;
 end
 else raise ELinePrinter.Create('Unable to change left margin while printing');
end;

function TRichPrinter.GetMarginRight:TMeasurement;
begin
 Result:=fMarginRight;
end;

procedure TRichPrinter.SetMarginRight(Value: TMeasurement);
begin
 if not Printer.Printing then
 begin
  if Value >= GutterRight then
  begin
   if Value < (PhysicalPageWidth-GutterLeft) then fMarginRight:=Value
  else
   begin
    fMarginRight:=PhysicalPageWidth-GutterLeft;
   end;
  end
 else fMarginRight:=GutterRight;
 end
else raise ELinePrinter.Create('Unable to change right margin while printing');
end;

procedure TRichPrinter.SetLogoTop(Value: TMeasurement);
begin
 fLogoTop:=Value;
end;

procedure TRichPrinter.SetLogoLeft(Value: TMeasurement);
begin
 fLogoLeft:=Value;
end;

procedure TRichPrinter.SetLogoWidth(Value: TMeasurement);
begin
 fLogoWidth:=Value;
end;

procedure TRichPrinter.SetLogoHeight(Value: TMeasurement);
begin
 fLogoHeight:=Value;
end;

procedure TRichPrinter.SetMeasureUnit(Value: TMeasureUnit);
var FormatEdit: string;
    Formatvalue: TMeasurement;
    I,Code: Integer;
begin
 if Value <> fMeasureUnit then
 begin
  fMeasureUnit:=Value;
  if not fStillCreating then
  begin
   // Update the margins if the units have changed.
   if MeasureUnit = muInches then
   begin
    MarginTop:=MarginTop/25.4;
    MarginBottom:=MarginBottom/25.4;
    MarginLeft:=MarginLeft/25.4;
    MarginRight:=MarginRight/25.4;
    BorderOffset:=BorderOffset/25.4;
    DefaultColWidth:=DefaultColWidth/25.4;
    LogoHeight:=LogoHeight/25.4;
    LogoLeft:=LogoLeft/25.4;
    LogoTop:=LogoTop/25.4;
    LogoWidth:=LogoWidth/25.4;
    FormatEdit:=FooterFormat;
    delete(FormatEdit,1,1);
    val(FormatEdit,Formatvalue,code);
    Formatvalue:=Formatvalue/25.4;
    str(Formatvalue:8:3,FormatEdit);
    FooterFormat:='>'+FormatEdit;
    FormatEdit:=HeaderFormat;
    delete(FormatEdit,1,1);
    val(FormatEdit,Formatvalue,code);
    Formatvalue:=Formatvalue/25.4;
    str(Formatvalue:8:3,FormatEdit);
    HeaderFormat:='>'+FormatEdit;
   end
  else
   begin
    MarginTop:=MarginTop*25.4;
    MarginBottom:=MarginBottom*25.4;
    MarginLeft:=MarginLeft*25.4;
    MarginRight:=MarginRight*25.4;
    BorderOffset:=BorderOffset*25.4;
    DefaultColWidth:=DefaultColWidth*25.4;
    LogoHeight:=LogoHeight*25.4;
    LogoLeft:=LogoLeft*25.4;
    LogoTop:=LogoTop*25.4;
    LogoWidth:=LogoWidth*25.4;
    FormatEdit:=FooterFormat;
    delete(FormatEdit,1,1);
    val(FormatEdit,Formatvalue,code);
    Formatvalue:=Formatvalue*25.4;
    str(Formatvalue:8:3,FormatEdit);
    FooterFormat:='>'+FormatEdit;
    FormatEdit:=HeaderFormat;
    delete(FormatEdit,1,1);
    val(FormatEdit,Formatvalue,code);
    Formatvalue:=Formatvalue*25.4;
    str(Formatvalue:8:3,FormatEdit);
    HeaderFormat:='>'+FormatEdit;
   end;
   Refresh;
  end;
 end;
end;

procedure TRichPrinter.SetLineSpacing;
var
 H: TPixels;
begin
 if Printer.Printing or RichPrintPreview then
 begin
  GetTextMetrics(fCanvas.Handle, fTextMetrics);
  H:=fTextMetrics.tmHeight+fTextMetrics.tmExternalLeading;
  fMaxHeightChar:=fTextMetrics.tmAscent;
 end
else
 try
  H:=fCanvas.TextHeight('M');
  fMaxHeightChar:=H;
 except
  on EPrinter do H:=10;
 end;
 fLineSpace:=H;
end;

procedure TRichPrinter.SetHeader(Value: String);
begin
 fHeader:=Value;
end;

procedure TRichPrinter.SetFooter(Value: String);
begin
 fFooter:=Value;
end;

procedure TRichPrinter.SetPageBorders(Value: TPageBorders);
begin
 fPageBorders:=Value;
end;

procedure TRichPrinter.SetShowProgress(Value: Boolean);
begin
 fShowProgress:=Value;
end;

procedure TRichPrinter.SetHeaderFormat(Value: String);
begin
 fHeaderFormat:=Value;
end;

procedure TRichPrinter.SetFooterFormat(Value: String);
begin
 fFooterFormat:=Value;
end;

procedure TRichPrinter.UpdateProgressDlg(const Status: String);
begin
(* if (ShowProgress and Printing} or (ShowProgress and RichPrintPreview) then*)
 if (ShowProgress and Printer.Printing) or (ShowProgress and RichPrintPreview) then
 begin
  {Create it if is doesn't already exist.}
  if fPrntProgDlg = nil then fPrntProgDlg:=TRichPrntProgDlg.Create(Application);
  {Show it if it isn't visible.}
  if not fPrntProgDlg.Visible then fPrntProgDlg.Show;
  {Update it as necessary.}
  with fPrntProgDlg do
  begin
   if Status <> '' then lblStatus.Caption:=Status;
   lblTitle.Caption:=Title;
   lblPageNumber.Caption:=IntToStr(fPageNumber);
   if Visible then Update;
  end;
 end
else
 begin
 {If it exists, get rid of it.}
  if fPrntProgDlg <> nil then
  begin
   {If it is visible, close it.}
   if fPrntProgDlg.Visible then fPrntProgDlg.Close;
   fPrntProgDlg:= nil;
   fPrntProgDlg.Free;
  end;
 end;
end;

function TRichPrinter.GetGutterTop: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsV(GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY));
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultGutterTopIn
  else Result:=DefaultGutterTopMm;
 end;
end;

function TRichPrinter.GetGutterBottom: TMeasurement;
begin
 Result:=PhysicalPageHeight-AvailablePageHeight-GutterTop;
end;

function TRichPrinter.GetGutterLeft: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsH(GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX));
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultGutterLeftIn
  else Result:=DefaultGutterLeftMm;
 end;
end;

function TRichPrinter.GetGutterRight: TMeasurement;
begin
 Result:=PhysicalPageWidth-AvailablePageWidth-GutterLeft;
end;  

function TRichPrinter.StartingLeft: TPixels;
begin
 Result:=MeasureUnitsToPixelsH(MarginLeft-GutterLeft);
end;

function TRichPrinter.StartingRight: TPixels;
begin
 Result:=MeasureUnitsToPixelsH(MarginRight-GutterRight);
end;

function TRichPrinter.StartingTop: TPixels;
begin
 Result:=MeasureUnitsToPixelsV(MarginTop-GutterTop);
end;

function TRichPrinter.StartingBottom: TPixels;
begin
 Result:=MeasureUnitsToPixelsV(MarginBottom-GutterBottom);
 if Result < fLineSpace then Result:=fLineSpace;
end;

function TRichPrinter.MeasureUnitsToPixelsH(const M: TMeasurement): TPixels;
var
 Temp: TMeasurement;
begin
 Temp := M;
 try
  if MeasureUnit = muMillimeters then Temp := M / 25.4;
  Result:=Round((Temp*GetDeviceCaps(Printer.Handle, LOGPIXELSX)));
 except
  on EPrinter do Result:=Round(Temp*DefaultDPI);
 end;
end;

function TRichPrinter.MeasureUnitsToPixelsV(const M: TMeasurement): TPixels;
var
 Temp: TMeasurement;
begin
 Temp := M;
 try
  if MeasureUnit = muMillimeters then Temp := M / 25.4;
  Result:=Round((Temp*GetDeviceCaps(Printer.Handle, LOGPIXELSY)));
 except
  on EPrinter do Result:=Round(Temp*DefaultDPI);
 end;
end;

function TRichPrinter.PixelsToMeasureUnitsH(const P: TPixels): TMeasurement;
begin
 try
  Result:=(P / GetDeviceCaps(Printer.Handle, LOGPIXELSX));
 except
  on EZeroDivide do Result:=P/DefaultDPI;
  on EPrinter do Result:=P/DefaultDPI;
 end;
 if MeasureUnit = muMillimeters then Result:=Result*25.4;
end;

function TRichPrinter.PixelsToMeasureUnitsV(const P: TPixels): TMeasurement;
begin
 try
  Result:=(P / GetDeviceCaps(Printer.Handle, LOGPIXELSY));
 except
  on EZeroDivide do Result:=P/DefaultDPI;
  on EPrinter do Result:=P/DefaultDPI;
 end;
 if MeasureUnit = muMillimeters then Result:=Result*25.4;
end;

function TRichPrinter.ExpandLogicalFields(S: String): String;
begin
 S:=ReplaceSubString(LineField, IntToStr(LineNumber), S);
 S:=ReplaceSubString(PageField, IntToStr(PageNumber), S);
 S:=ReplaceSubString(DateField, FormatDateTime('ddddd',Date), S);
 S:=ReplaceSubString(TimeField, FormatDateTime('tt',Time), S);
 S:=ReplaceSubString(TitleField, Title, S);
 Result:=S;
end;

procedure TRichPrinter.SetBorderOffset(Value: TMeasurement);
begin
 fBorderOffset:=Value;
end;

function TRichPrinter.GetPhysicalPageHeight: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsV(GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT));
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultPhysicalPageHeightIn
  else Result:=DefaultPhysicalPageHeightMm;
 end;
end;

function TRichPrinter.GetPhysicalPageWidth: TMeasurement;
begin
 try
  Result:=PixelsToMeasureUnitsH(GetDeviceCaps(Printer.Handle, PHYSICALWIDTH));
 except
  on EPrinter do
   if MeasureUnit = muInches then Result:=DefaultPhysicalPageWidthIn
  else Result:=DefaultPhysicalPageWidthMm;
 end;
end;

procedure TRichPrinter.SetOptions(Value : TPageOptions);
Begin
 FOptions_Dialog := Value;
 FFlags := 0;
 If poDisableMargins in FOptions_Dialog then
 begin
  FFlags := FFlags or PSD_DISABLEMARGINS;
  FOptions_Dialog:=FOptions_Dialog-[poMargins];
 end;
 If poMargins in FOptions_Dialog then
 begin
  FFlags := FFlags or PSD_MARGINS;
  FOptions_Dialog:=FOptions_Dialog-[poDisableMargins];
 end;
 If poDisableOrientation in FOptions_Dialog then  FFlags := FFlags or PSD_DISABLEORIENTATION;
 If poDisablePaper in FOptions_Dialog then        FFlags := FFlags or PSD_DISABLEPAPER;
 If poDisablePrinter in FOptions_Dialog then      FFlags := FFlags or PSD_DISABLEPRINTER;
 If MeasureUnit=muMillimeters then                FFlags := FFlags or PSD_INHUNDREDTHSOFMILLIMETERS;
 If MeasureUnit=muInches then                     FFlags := FFlags or PSD_INTHOUSANDTHSOFINCHES;
End;

procedure GetPrinter(var DeviceMode, DeviceNames: THandle);
var
  Device, Driver, Port: array[0..79] of char;
  DevNames: PDevNames;
  Offset: PChar;
begin
  Printer.GetPrinter(Device, Driver, Port, DeviceMode);
  if DeviceMode <> 0 then
  begin
    DeviceNames := GlobalAlloc(GHND, SizeOf(TDevNames) +
     StrLen(Device) + StrLen(Driver) + StrLen(Port) + 3);
    DevNames := PDevNames(GlobalLock(DeviceNames));
    try
      Offset := PChar(DevNames) + SizeOf(TDevnames);
      with DevNames^ do
      begin
        wDriverOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Driver) + 1;
        wDeviceOffset := Longint(Offset) - Longint(DevNames);
        Offset := StrECopy(Offset, Device) + 1;
        wOutputOffset := Longint(Offset) - Longint(DevNames);;
        StrCopy(Offset, Port);
      end;
    finally
      GlobalUnlock(DeviceNames);
    end;
  end;
end;

function CopyData(Handle: THandle): THandle;
var
  Src, Dest: PChar;
  Size: Integer;
begin
  if Handle <> 0 then
  begin
    Size := GlobalSize(Handle);
    Result := GlobalAlloc(GHND, Size);
    if Result <> 0 then
      try
        Src := GlobalLock(Handle);
        Dest := GlobalLock(Result);
        if (Src <> nil) and (Dest <> nil) then Move(Src^, Dest^, Size);
      finally
        GlobalUnlock(Handle);
        GlobalUnlock(Result);
      end
  end
  else Result := 0;
end;

procedure CenterWindow(Wnd: HWnd);
var
  Rect: TRect;
begin
  GetWindowRect(Wnd, Rect);
  SetWindowPos(Wnd, 0,
    (GetSystemMetrics(SM_CXSCREEN) - Rect.Right + Rect.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - Rect.Bottom + Rect.Top) div 3,
    0, 0, SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;


function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := 0;
  case Msg of
    WM_INITDIALOG:
      begin
        CenterWindow(Wnd);
        Result := 1;
      end;
  end;
end;

function TaskModalDialog(DialogFunc: Pointer; var DialogData): Bool;
type
    TDialogFunc = function(var DialogData): Bool stdcall;
var
    ActiveWindow: HWnd;
    WindowList: Pointer;
begin
    ActiveWindow := GetActiveWindow;
    WindowList := DisableTaskWindows(0);
    try
        Result := TDialogFunc(DialogFunc)(DialogData);
    finally
        EnableTaskWindows(WindowList);
        SetActiveWindow(ActiveWindow);
    end;
end;

procedure SetPrinter(DeviceMode, DeviceNames: THandle);
var
  DevNames: PDevNames;
begin
  DevNames := PDevNames(GlobalLock(DeviceNames));
  try
    with DevNames^ do
      Printer.SetPrinter(PChar(DevNames) + wDeviceOffset,
        PChar(DevNames) + wDriverOffset,
        PChar(DevNames) + wOutputOffset, DeviceMode);
  finally
    GlobalUnlock(DeviceNames);
    GlobalFree(DeviceNames);
  end;
end;

procedure TRichPrinter.ReadIniFile;
var  CodeMeasurement,DefaultInt,CodeDirection: Integer;
     TRegIniFile1: TRegIniFile;
begin
 DefaultInt:=1;
 TRegIniFile1:=TRegIniFile.Create('RichPrint.Settings');;
 with TRegIniFile1 do
 begin
  if OpenKey('Marges',false) then
  begin
   MarginLeft:=ReadFloat('MarginLeft');
   MarginTop:=ReadFloat('MarginTop');
   MarginBottom:=ReadFloat('MarginBottom');
   MarginRight:=ReadFloat('MarginRight');
   CodeMeasurement:=ReadInteger('','CodeMeasurement',DefaultInt);
   if CodeMeasurement=1 then MeasureUnit:=muMillimeters else MeasureUnit:=muInches;
   CodeDirection:=ReadInteger('','CodeDirection',DefaultInt);
   if CodeDirection=1 then Orientation:=poPortrait else Orientation:=poLandscape;
  end
 else
  begin
   MarginLeft:=fMarginLeft;
   MarginTop:=fMarginTop;
   MarginBottom:=fMarginBottom;
   MarginRight:=fMarginRight;
   MeasureUnit:=fMeasureUnit{muMillimeters};
   Orientation:=GetOrientation;{poPortrait;}
  end;
 end;
 TRegIniFile1.Free;
end;

procedure TRichPrinter.WriteIniFile;
var  CodeMeasurement,CodeDirection: Integer;
     TRegIniFile1: TRegIniFile;
begin
 TRegIniFile1:=TRegIniFile.Create('RichPrint.Settings');;
 with TRegIniFile1 do
 begin
  create('Marges');
  WriteFloat('MarginLeft',MarginLeft);
  WriteFloat('MarginTop',MarginTop);
  WriteFloat('MarginBottom',MarginBottom);
  WriteFloat('MarginRight',MarginRight);
  if MeasureUnit=muMillimeters then CodeMeasurement:=1 else CodeMeasurement:=0;
  WriteInteger('','CodeMeasurement',CodeMeasurement);
  if Orientation=poPortrait then CodeDirection:=1 else CodeDirection:=0;
  WriteInteger('','CodeDirection',CodeDirection);
 end;
 TRegIniFile1.Free;
end;

procedure TRichPrinter.WriteIniFileCompress;
var TRegIniFile1: TRegIniFile;
begin
 TRegIniFile1:=TRegIniFile.Create('RichPrint.Settings');;
 with TRegIniFile1 do
 begin
  create('Compress');
  WriteBool('','CompressPreview',fCompressPreview);
 end;
 TRegIniFile1.Free;
end;

procedure TRichPrinter.SetCompressPreview(Value: Boolean);
begin
 fCompressPreview:=Value;
end;


procedure TRichPrinter.SetSaveRegistry(Value: Boolean);
begin
 fSaveMargins:=Value;
end;

procedure TRichPrinter.SetBitmapLogo(Value: TBitmap);
begin
 FBitmapLogo.Assign(Value);
end;

function TRichPrinter.PageDialog: Boolean;
var
  PageDlgRec: TPageSetupDlg;
  DevHandle: THandle;
begin
  FillChar(PageDlgRec, SizeOf(PageDlgRec), 0);
  with PageDlgRec do
  begin
   lStructSize := SizeOf(PageDlgRec);
   GetPrinter(DevHandle,hDevNames);
   hDevMode    := CopyData(DevHandle);
   if fSaveMargins then ReadIniFile;
   Printer.Orientation:=Orientation;
   if MeasureUnit=muMillimeters then
    rtMargin    := Rect(Round(MarginLeft*100),Round(MarginTop*100),Round(MarginRight*100),Round(MarginBottom*100))
   else
    rtMargin    := Rect(Round(MarginLeft*1000),Round(MarginTop*1000),Round(MarginRight*1000),Round(MarginBottom*1000));
   Flags       := PSD_ENABLEPAGESETUPHOOK or FFlags;
   hWndOwner   := Application.Handle;
   lpfnPageSetupHook := DialogHook;
  End;
  if TaskModalDialog(@PageSetupDlg, PageDlgRec) then Result:=true else Result:=false;
  with PageDlgRec do
  begin
   if MeasureUnit=muMillimeters then
   begin
    MarginLeft   := rtMargin.Left/100.0;
    MarginTop    := rtMargin.Top/100.0;
    MarginRight  := rtMargin.Right/100.0;
    MarginBottom := rtMargin.Bottom/100.0;
   end
  else
   begin
    MarginLeft   := rtMargin.Left/1000.0;
    MarginTop    := rtMargin.Top/1000.0;
    MarginRight  := rtMargin.Right/1000.0;
    MarginBottom := rtMargin.Bottom/1000.0;
   end;
   Refresh;
   SetPrinter(hDevMode, hDevNames);
   Orientation:=Printer.Orientation;
   if fSaveMargins then WriteIniFile;
  End;
end;

procedure Register;
begin
 RegisterComponents('Win32', [TRichPrinter]);
end;

end.
