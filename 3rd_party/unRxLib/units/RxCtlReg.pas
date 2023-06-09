{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{ Revision and component added by JB.                   }
{*******************************************************}

{ Note:
  - in Delphi 4.0 you must add DCLSTD40 and DCLSMP40 to the requires
    page of the package you install this components into.
  - in Delphi 3.0 you must add DCLSTD30 and DCLSMP30 to the requires
    page of the package you install this components into.
  - in C++Builder 3.0 you must add DCLSTD35 to the requires page of the
    package you install this components into. }

unit RxCtlReg;

{$I RX.INC}
{$D-,L-,S-}

interface

{ Register custom useful controls }

procedure Register;

implementation

{$R *.dcr}

uses
  {$IFNDEF VER80}Windows, {$ELSE}WinTypes, {$ENDIF}Classes, SysUtils,
  TypInfo, Controls, Graphics, ExtCtrls, Tabs, Dialogs, Forms,
  {$IFDEF RX_D3}DsnConst, ExtDlgs, {$ELSE}LibConst, {$ENDIF}
  {$IFDEF DCS}
  {$IFDEF RX_D4}ImgEdit, {$ENDIF}{$IFNDEF VER80}ImgList, {$ENDIF}
  {$ENDIF DCS}
  {$IFDEF RX_D6}DesignIntf, DesignEditors, VCLEditors, Registry{$ELSE}DsgnIntf{$ENDIF}, // Polaris
  {$IFNDEF VER80}RxRichEd, {$ENDIF}Menus, FiltEdit, StdCtrls, Buttons,
  RxResConst, RxCtrls, RxGrids, RxCurrEdit, RxToolEdit, RxHintProp, RxDateUtil,
  RxPickDate, RxSplit, RxSlider, RxClock, RxAnimate, RxCombos, RxSpin, Consts,
  RxDice, RxSwitch, RxCheckItm, RxVCLUtils, RxColors, RxAniFile, RxGraph,
  {$IFDEF USE_RX_GIF}RxGIF, RxGIFCtrl, {$ENDIF}RxHints, RxExcptDlg, RxTimer,
  RxFileUtil, RxDsgn, RxExtenders;

{$IFNDEF RX_D3}

{ TDateProperty }

type
  TDateProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TDateProperty.GetValue: string;
begin
  if GetFloatValue = NullDate then
    Result := ''
  else
    Result := FormatDateTime(ShortDateFormat, GetFloatValue);
end;

procedure TDateProperty.SetValue(const Value: string);
begin
  if Value = '' then
    SetFloatValue(NullDate)
  else
    SetFloatValue(StrToDateFmt(ShortDateFormat, Value));
end;

{ TRxModalResultProperty }

type
  TRxModalResultProperty = class(TModalResultProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

const
  ModalResults: array[mrAll..mrYesToAll] of string = (
    'mrAll',
    'mrNoToAll',
    'mrYesToAll');

function TRxModalResultProperty.GetValue: string;
var
  CurValue: LongInt;
begin
  CurValue := GetOrdValue;
  case CurValue of
    Low(ModalResults)..High(ModalResults):
      Result := ModalResults[CurValue];
  else
    Result := inherited GetValue;
  end;
end;

procedure TRxModalResultProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := Low(ModalResults) to High(ModalResults) do
    Proc(ModalResults[I]);
end;

procedure TRxModalResultProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if (Value <> '') then
    for I := Low(ModalResults) to High(ModalResults) do
      if CompareText(ModalResults[I], Value) = 0 then
      begin
        SetOrdValue(I);
        Exit;
      end;
  inherited SetValue(Value);
end;

{$ENDIF RX_D3}

function ValueName(E: Extended): string;
begin
  if E = High(Integer) then
    Result := 'MaxInt'
  else if E = Low(Integer) then
    Result := 'MinInt'
  else if E = High(LongInt) then
    Result := 'MaxLong'
  else if E = Low(LongInt) then
    Result := 'MinLong'
  else if E = High(ShortInt) then
    Result := 'MaxShort'
  else if E = Low(ShortInt) then
    Result := 'MinShort'
  else if E = High(Word) then
    Result := 'MaxWord'
  else
    Result := '';
end;

function StrToValue(const S: string): LongInt;
begin
  if CompareText(S, 'MaxLong') = 0 then
    Result := High(LongInt)
  else if CompareText(S, 'MinLong') = 0 then
    Result := Low(LongInt)
  else if CompareText(S, 'MaxInt') = 0 then
    Result := High(Integer)
  else if CompareText(S, 'MinInt') = 0 then
    Result := Low(Integer)
  else if CompareText(S, 'MaxShort') = 0 then
    Result := High(ShortInt)
  else if CompareText(S, 'MinShort') = 0 then
    Result := Low(ShortInt)
  else if CompareText(S, 'MaxWord') = 0 then
    Result := High(Word)
  else
    Result := 0;
end;

{ TRxIntegerProperty }

type
  TRxIntegerProperty = class(TIntegerProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TRxIntegerProperty.GetValue: string;
begin
  Result := ValueName(GetOrdValue);
  if Result = '' then Result := IntToStr(GetOrdValue);
end;

procedure TRxIntegerProperty.SetValue(const Value: string);
var
  L: LongInt;
begin
  L := StrToValue(Value);
  if L = 0 then L := StrToInt(Value);
  inherited SetValue(IntToStr(L));
end;

{ TRxFloatProperty }

type
  TRxFloatProperty = class(TFloatProperty)
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

function TRxFloatProperty.GetValue: string;
const
  {$IFNDEF VER80}
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 18);
  {$ELSE}
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18);
  {$ENDIF}
begin
  Result := ValueName(GetFloatValue);
  if Result = '' then
    Result := FloatToStrF(GetFloatValue, ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
end;

procedure TRxFloatProperty.SetValue(const Value: string);
var
  L: LongInt;
begin
  L := StrToValue(Value);
  if L <> 0 then
    SetFloatValue(L)
  else
    SetFloatValue(StrToFloat(Value));
end;

{ TPaintBoxEditor }

type
  TPaintBoxEditor = class(TDefaultEditor)
  public
    {$IFDEF RX_D6} // Polaris
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
    {$ENDIF}
  end;

  {$IFDEF RX_D6} // Polaris

procedure TPaintBoxEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
{$ELSE}

procedure TPaintBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
begin
  if CompareText(PropertyEditor.GetName, 'OnPaint') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end
    {$IFDEF RX_D6} // Polaris
  else
    inherited EditProperty(PropertyEditor, Continue);
  {$ELSE}
  else
    inherited EditProperty(PropertyEditor, Continue, FreeEditor);
  {$ENDIF}
end;

{ TAnimatedEditor }

type
  TAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean;
    {$IFDEF RX_D6} // Polaris
    procedure CheckEdit(const PropertyEditor: IProperty);
    {$ELSE}
    procedure CheckEdit(PropertyEditor: TPropertyEditor);
    {$ENDIF}
    procedure EditImage(Image: TAnimatedImage);
    procedure LoadAniFile(Image: TAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {$IFDEF RX_D6} // Polaris

procedure TAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
{$ELSE}

procedure TAnimatedEditor.CheckEdit(PropertyEditor: TPropertyEditor);
{$ENDIF}
begin
  try
    if FContinue and (CompareText(PropertyEditor.GetName, 'GLYPH') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  finally
    {$IFNDEF RX_D6} // Polaris
    PropertyEditor.Free;
    {$ENDIF}
  end;
end;

procedure TAnimatedEditor.EditImage(Image: TAnimatedImage);
var
  {$IFDEF RX_D6} // Polaris
  Components: IDesignerSelections;
  {$ELSE}
  Components: TDesignerSelectionList;
  {$ENDIF}
begin
  {$IFDEF RX_D6} // Polaris
  Components := CreateSelectionlist;
  {$ELSE}
  Components := TDesignerSelectionList.Create;
  {$ENDIF}
  try
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit);
  finally
    {$IFNDEF RX_D6} // Polaris
    Components.Free;
    {$ENDIF}
  end;
end;

procedure TAnimatedEditor.LoadAniFile(Image: TAnimatedImage);
var
  Dialog: TOpenDialog;
  AniCursor: TAnimatedCursorImage;
  CurDir: string;
begin
  CurDir := GetCurrentDir;
  Dialog := TOpenDialog.Create(Application);
  try
    with Dialog do
    begin
      Options := [ofHideReadOnly, ofFileMustExist];
      DefaultExt := 'ani';
      Filter := RxLoadStr(srAniCurFilter);
      if Execute then
      begin
        AniCursor := TAnimatedCursorImage.Create;
        try
          AniCursor.LoadFromFile(FileName);
          AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
            Image.Orientation = goVertical);
          Image.Interval := AniCursor.DefaultRate;
          Image.TransparentColor := clFuchsia;
          Designer.Modified;
        finally
          AniCursor.Free;
        end;
      end;
    end;
  finally
    Dialog.Free;
    SetCurrentDir(CurDir);
  end;
end;

procedure TAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if (Index = GetVerbCount - 1) then
    LoadAniFile(TAnimatedImage(Component))
  else if (Index = GetVerbCount - 2) then
    EditImage(TAnimatedImage(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if (Index = GetVerbCount - 1) then
    Result := RxLoadStr(srLoadAniCursor)
  else if (Index = GetVerbCount - 2) then
    Result := RxLoadStr(srEditPicture)
  else
    Result := inherited GetVerb(Index);
end;

function TAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

{$IFDEF DCS}
{$IFNDEF VER80}

type
  TRxImageListEditor = class(TComponentEditor)
  private
    procedure SaveAsBitmap(ImageList: TImageList);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TRxImageListEditor.SaveAsBitmap(ImageList: TImageList);
var
  Bitmap: TBitmap;
  SaveDlg: TOpenDialog;
  I: Integer;
begin
  if ImageList.Count > 0 then
  begin
    {$IFDEF RX_D3}
    SaveDlg := TSavePictureDialog.Create(Application);
    {$ELSE}
    SaveDlg := TSaveDialog.Create(Application);
    {$ENDIF}
    with SaveDlg do
    try
      Options := [ofHideReadOnly, ofOverwritePrompt];
      DefaultExt := GraphicExtension(TBitmap);
      Filter := GraphicFilter(TBitmap);
      if Execute then
      begin
        Bitmap := TBitmap.Create;
        try
          with Bitmap do
          begin
            Width := ImageList.Width * ImageList.Count;
            Height := ImageList.Height;
            if ImageList.BkColor <> clNone then
              Canvas.Brush.Color := ImageList.BkColor
            else
              Canvas.Brush.Color := clWindow;
            Canvas.FillRect(Bounds(0, 0, Width, Height));
            for I := 0 to ImageList.Count - 1 do
              ImageList.Draw(Canvas, ImageList.Width * I, 0, I);
            {$IFDEF RX_D3}
            HandleType := bmDIB;
            if PixelFormat in [pf15bit, pf16bit] then
            try
              PixelFormat := pf24bit;
            except
            {silent}
            end;
            {$ENDIF}
          end;
          Bitmap.SaveToFile(FileName);
        finally
          Bitmap.Free;
        end;
      end;
    finally
      Free;
    end;
  end
  else
    Beep;
end;

procedure TRxImageListEditor.ExecuteVerb(Index: Integer);
begin
  if Designer <> nil then
    case Index of
      0: if EditImageList(Component as TImageList) then Designer.Modified;
      1: SaveAsBitmap(TImageList(Component));
    end;
end;

function TRxImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    {$IFDEF RX_D3}
    0: Result := SImageListEditor;
    {$ELSE}
    0: Result := RxLoadStr(SImageEditor);
    {$ENDIF}
    1: Result := RxLoadStr(srSaveImageList);
  else
    Result := '';
  end;
end;

function TRxImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$ENDIF}
{$ENDIF DCS}

{ TWeekDayProperty }

type
  TWeekDayProperty = class(TEnumProperty)
    function GetAttributes: TPropertyAttributes; override;
  end;

function TWeekDayProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList];
end;

{ TRxColorProperty }

//NOTE (JB):
//  Big thanks to Remy Lebeau for invaluable assistance and help in correct
//  malfunction of original TColorProperty (standard no works fine in D6..XE2)

type
  TRxColorProperty =
    {$IFDEF RX_D6}
    { must be completelly replaced, not be used "TColorProperty", it no works properly }
  class(TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing
      {$IFDEF DX_D9}, ICustomPropertyDrawing80{$ENDIF})
    {$ELSE}
  class(TColorProperty)
    {$ENDIF}
  protected
    function PaintColorBox(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean): TRect; virtual;
  public
    {$IFDEF RX_D6}
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    {$ENDIF}
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    {$IFDEF RX_D6}
    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    {$ENDIF}
    {$IFDEF RX_D5}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF RX_D6} override; {$ENDIF} // Polaris
    {$ENDIF}
    {$IFDEF RX_D6}
    { CustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    {$IFDEF DX_D9}
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
    {$ENDIF}
    {$ENDIF}
  end;

function TRxColorProperty.GetValue: string;
var
  Color: TColor;
begin
  Color := TColor(GetOrdValue);
  {$IFNDEF VER80}
  if Color = clNone16 then
    Color := clNone
  else if Color = clInfoBk16 then
    Color := clInfoBk;
  {$ENDIF}
  Result := RxColorToString(Color);
end;

procedure TRxColorProperty.GetValues(Proc: TGetStrProc);
begin
  RxGetColorValues(Proc);
end;

procedure TRxColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(RxStringToColor(Value));
end;

{$IFDEF RX_D5}

procedure TRxColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red, Green, Blue, Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or
      (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  with ACanvas do
  begin
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    try
      Right := (ARect.Bottom - ARect.Top) + ARect.Left;
      Brush.Color := clWindow;
      FillRect(ARect);
      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);
      Brush.Color := RxStringToColor(Value);
      Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
      Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);
    finally
      Brush.Color := OldBrushColor;
      Pen.Color := OldPenColor;
    end;
    ACanvas.TextRect(Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1, ARect.Top + 1, Value);
  end;
end;
{$ENDIF}

function TRxColorProperty.PaintColorBox(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean): TRect;

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
        Green,
        Blue,
        Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
      (TColorQuad(AColor).Green > 192) or
      (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  Right: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
  with ACanvas do
  begin
    // save off things
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    try
      Brush.Color := clWindow;
      FillRect(ARect);
      // frame things
      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

      // set things up and do the work
      Brush.Color := RxStringToColor(Value);

      Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
      Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    finally
      // restore the things we twiddled with
      Brush.Color := OldBrushColor;
      Pen.Color := OldPenColor;
    end;
    ACanvas.TextRect(Rect(Right, ARect.Top, ARect.Right, ARect.Bottom),
      Right + 1, ARect.Top + 1, Value);

    Result := Rect(Right, ARect.Top, ARect.Right, ARect.Bottom);
  end;
end;

{$IFDEF RX_D6}

procedure TRxColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
  begin
    PaintColorBox(GetVisualValue, ACanvas, ARect, ASelected)
    //stDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  end
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

{$IFDEF DX_D9}

function TRxColorProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TRxColorProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;
{$ENDIF}

procedure TRxColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // No implemenation necessary
end;

procedure TRxColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TRxColorProperty.Edit; //original source from Borland Delphi 2005
const
  hcDColorEditor = 25010;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S,
              CustomColors.Values[S]);
          end;
        end;
  end;

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
    GetCustomColors;
    ColorDialog.Color := GetOrdValue;
    ColorDialog.HelpContext := hcDColorEditor;
    ColorDialog.Options := [cdShowHelp];
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
    SaveCustomColors;
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TRxColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

procedure TRxColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;
{$ENDIF}

{$IFDEF RX_D3}
resourcestring
  srSamples = 'Samples';
  {$ENDIF}

procedure Register;
const
  srRXControls = 'RX Controls';
  BaseClass: TClass = {$IFDEF RX_D3}TPersistent{$ELSE}TComponent{$ENDIF};

begin
  RegisterComponents(srRXControls, [TComboEdit, TFilenameEdit,
    TDirectoryEdit, TDateEdit, TRxCalcEdit, TCurrencyEdit, TTextListBox,
      TRxCheckListBox, TFontComboBox, TColorComboBox, TRxSplitter, TRxSlider,
      TRxLabel, {$IFNDEF VER80}TRxRichEdit, {$ENDIF}{$IFNDEF UNICODE}TRxThread, {$ENDIF}
    TRxCheckBox, TRxRadioButton, TRxStatusPanelBinder, TRxWizardHeader,
      TRxFlexHelpPanel, TRxHoleShape, TRxAnimBitBtn, TRxAnimSpeedButton,
      TRxClock, TAnimatedImage, TRxDrawGrid, TRxSpeedButton, TRxProgress,
      {$IFDEF USE_RX_GIF}TRxGIFAnimator, {$ENDIF}TRxSpinButton, TRxSpinEdit,
    TRxTimeEdit, TRxSwitch, TRxDice, TRxPanel, TRxColorButton]);
  {$IFDEF CBUILDER}
  {$IFNDEF RX_V110} { C++Builder 1.0 }
  RegisterComponents(srSamples, [TScroller]);
  {$ELSE}
  RegisterComponents(srSamples, [TScroller]);
  {$ENDIF}
  {$ELSE}
  RegisterComponents(srSamples, [TScroller]);
  {$ENDIF}

  {$IFDEF RX_D3}
  RegisterNonActiveX([TCustomComboEdit, TCustomDateEdit, TCustomNumEdit,
    TFileDirEdit, TRxCustomListBox, TRxRichEdit], axrComponentOnly);
  RegisterNonActiveX([TScroller], axrComponentOnly);
  {$ENDIF RX_D3}
  RegisterPropertyEditor(TypeInfo(TDateTime), TRxTimeEdit, 'Time', TTimeProperty);
  RegisterPropertyEditor(TypeInfo(TDayOfWeekName), nil, '', TWeekDayProperty);
  {$IFDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(string), TCustomNumEdit, 'Text', nil);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(string), TCustomNumEdit, 'Text', TStringProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(string), TFileDirEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomDateEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TFileNameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TFileNameEdit, 'FileName', TFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDirectoryEdit, 'Text', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'FolderName', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'DirectoryName', TDirnameProperty);
  RegisterPropertyEditor(TypeInfo(string), BaseClass, 'Hint', THintProperty);
  RegisterPropertyEditor(TypeInfo(string), TMenuItem, 'Hint', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TCustomComboEdit, 'ButtonHint', THintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TRxCheckListBox, 'Items', TCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TProgressControlProperty);
  {$IFDEF RX_D3}
  RegisterPropertyEditor(TypeInfo(Boolean), TFontComboBox, 'TrueTypeOnly', nil);
  RegisterPropertyEditor(TypeInfo(TCursor), TRxSplitter, 'Cursor', nil);
  {$ELSE}
  RegisterPropertyEditor(TypeInfo(TDateTime), TPersistent, '', TDateProperty);
  RegisterPropertyEditor(TypeInfo(TModalResult), TPersistent, '', TRxModalResultProperty);
  {$ENDIF}

  RegisterPropertyEditor(TypeInfo(TColor), {$IFNDEF RX_D6}TPersistent{$ELSE}nil{$ENDIF}, '', TRxColorProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TRxLabel, 'Caption', THintProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TRxSpeedButton, 'Caption', THintProperty);

  RegisterPropertyEditor(TypeInfo(Integer), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(ShortInt), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(SmallInt), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(LongInt), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Word), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Byte), BaseClass, '', TRxIntegerProperty);
  RegisterPropertyEditor(TypeInfo(Cardinal), BaseClass, '', TRxIntegerProperty);

  RegisterPropertyEditor(TypeInfo(Single), BaseClass, '', TRxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Double), BaseClass, '', TRxFloatProperty);
  RegisterPropertyEditor(TypeInfo(Extended), BaseClass, '', TRxFloatProperty);
  {$IFNDEF VER80}
  RegisterPropertyEditor(TypeInfo(Currency), BaseClass, '', TRxFloatProperty);
  {$ENDIF}

  RegisterComponentEditor(TPaintBox, TPaintBoxEditor);
  RegisterComponentEditor(TAnimatedImage, TAnimatedEditor);
  {$IFNDEF VER80}
  {$IFDEF DCS}
  RegisterComponentEditor(TCustomImageList, TRxImageListEditor);
  RegisterComponentEditor(TImageList, TRxImageListEditor);
  {$ENDIF}
  {$ENDIF}
end;

end.

