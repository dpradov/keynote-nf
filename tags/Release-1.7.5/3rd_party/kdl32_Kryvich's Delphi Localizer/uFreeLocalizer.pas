{ Kryvich's Delphi Localizer Class
  Copyright (C) 2006, 2007, 2008 Kryvich, Belarusian Linguistic Software team.
}

{$include version.inc}

unit uFreeLocalizer;

interface

uses
  Classes;

type
  // Method of error processing
  TErrorProcessing = (
    epSilent,    // Just skip errors (default) - use for public releases
    epMessage,   // Show message to a user - use for a beta testing
    epException, // Raise exception - use while develop and debug
    epDebug,     // Use DebugOutputString
    epErrors     // Append all messages to a string list
  );

  // Translated form properties
  TResForm = class
    Name: string;         // Form name
    Props: TStringList;   // Property names
    Values: TStringList;  // Translated property values
  end;

   // Dump of original func, pointers to Original & New funcs
  TFuncReplacement = class
  private
    OrigDump: packed array[0..4] of byte;
    OrigFunc, MyFunc: pointer;
    fReplaced: boolean; // Is func replaced now

    procedure SetReplaced(aReplaced: boolean);
  public
    constructor Create(aOrigFunc, aMyFunc: pointer);
    destructor Destroy; override;
    property Replaced: boolean read fReplaced write SetReplaced;
  end;

  // Events of Localizer
  TBeforeLanguageLoadEvent = procedure(Sender: TObject; const OldLanguageFile, NewLanguageFile: string) of object;
  TAfterLanguageLoadEvent = procedure(Sender: TObject; const LanguageFile: string) of object;

  TFreeLocalizer = class
  private
    fLanguageFile: string; // Loaded language file
    ResForms: array of TResForm; // List of all localized forms
    fAutoTranslate: boolean;
    InitInheritedRepl: TFuncReplacement; // InitInheritedComponent replacement
    fBeforeLanguageLoadEvent: TBeforeLanguageLoadEvent;
    fAfterLanguageLoadEvent: TAfterLanguageLoadEvent;
    fErrors: TStrings;

    // Check status of UTF8VCL library
    class function CheckUtf8vcl: boolean;
    // Get Humanize settings of a language file
    procedure GetEncoding(sl: TStringList; var Humanize: boolean;
      var HumanizedCR, HumanizedCRLF: WideString);
    // Delete old translations in ResForms
    procedure ClearResForms;
    // Load translations from file
    procedure LoadLanguageFile(const aLanguageFile: string);
    // Set value PropValue for property PropName in Component RootComp
    procedure TranslateProp(RootComp: TComponent; const PropName, PropValue: string);
    // Enable/disable translation of resource strings
    procedure SetTranslateResourceStrings(aTranslate: boolean);
    // Enable/Disable autotranslation feature
    procedure SetAutoTranslate(aAutoTranslate: boolean);
    // Called when error encountered
    procedure Error(const Mess: string);
    // Translate component (form) as component of class CompClassType
    procedure TranslateAs(Comp: TComponent; const CompClassType: TClass);
    // Get error messages from fErrors
    function GetErrors: string;
  public
    LanguageDir: string; // Directory with language files (optional)
    ErrorProcessing: TErrorProcessing;

    constructor Create;
    destructor Destroy; override;
    // Translate component (form)
    procedure Translate(Comp: TComponent);
    // Translate all forms on Screen
    procedure TranslateScreen;
    // Clear error messages in fErrors
    procedure ClearErrors; 
    // Error messages (set ErrorProcessing to epErrors)
    property Errors: string read GetErrors;
    // Language file name. Set it to load a new translation
    property LanguageFile: string read fLanguageFile write LoadLanguageFile;
    // Enable/disable translation of resource strings
    property TranslateResourceStrings: boolean write SetTranslateResourceStrings;
    // Auto translate a form after creating
    property AutoTranslate: boolean read fAutoTranslate write SetAutoTranslate;
    // Occurs exactly before loading new language file
    // Call silent exception (Abort) to abort the operation
    property BeforeLanguageLoad: TBeforeLanguageLoadEvent read fBeforeLanguageLoadEvent write fBeforeLanguageLoadEvent;
    // Occurs exactly after loading new language
    // Do here necessary operations such as calling TranslateScreen (if AutoTranslate disabled)
    // and updating of controls state
    property AfterLanguageLoad: TAfterLanguageLoadEvent read fAfterLanguageLoadEvent write fAfterLanguageLoadEvent;
  end;

var
  FreeLocalizer: TFreeLocalizer;

implementation

uses
  Windows, SysUtils, TypInfo, Forms, uStringUtils
  {$ifdef D7} ,uLegacyCode {$endif}
  {$ifndef D5} ,StrUtils {$endif};

const
  LngHeader = '; Kryvich''s Delphi Localizer Language File.';

  sNewMark = '(!)';
  sDelMark = '(x)';

var
  AnsiCP: LongWord; // current ANSI code-page identifier for the system

{$ifndef D7}{$region 'EKdlError'}{$endif}
type
  EKdlError = class (Exception)
    constructor Create(AMessage: string);
  end;
  EKdlSilentError = class (EKdlError)
    constructor Create;
  end;

constructor EKdlError.Create(AMessage: string);
begin
  inherited Create(AMessage);
end;

constructor EKdlSilentError.Create;
begin
  inherited Create('');
end;
{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'TResStringer'}{$endif}
type
  TResStringer = class
  private
    LoadResRepl: TFuncReplacement; // LoadResString replacement
    ResStrings: TStringList; // Translated resource strings
    fTranslate: boolean; // Do translations of resource strings

    // Get resource string
    function GetString(Id: integer; var s: AnsiString): boolean;
    // Set translation status
    procedure SetTranslate(aTranslate: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    // Read resource strings from sl into ResStrings
    procedure LoadResStrings(sl: TStringList; var i: integer;
      Humanize: boolean; const HumanizedCR, HumanizedCRLF: WideString;
      Utf8vclActive: boolean);
    property Translate: boolean read fTranslate write SetTranslate;
  end;

var
  ResStringer: TResStringer;

function MyLoadResString(ResStringRec: PResStringRec): AnsiString;
begin
  if ResStringRec = nil then exit;
  if assigned(ResStringer) and ResStringer.Translate then begin
    if ResStringRec.Identifier >= 64*1024 then
      Result := PChar(ResStringRec.Identifier)
    else if not ResStringer.GetString(ResStringRec.Identifier, result) then begin
      ResStringer.Translate := false;
      result := System.LoadResString(ResStringRec);
      ResStringer.Translate := true;
    end;
  end else
    result := System.LoadResString(ResStringRec);
end;

{ TResStringer }

constructor TResStringer.Create;
begin
  LoadResRepl := TFuncReplacement.Create(@System.LoadResString, @MyLoadResString);
  Translate := true;
end;

destructor TResStringer.Destroy;
begin
  Translate := false;
  FreeAndNil(ResStrings);
  LoadResRepl.Free;
  inherited;
end;

procedure TResStringer.SetTranslate(aTranslate: boolean);
begin
  LoadResRepl.Replaced := aTranslate;
  fTranslate := aTranslate;
end;

procedure TResStringer.LoadResStrings(sl: TStringList; var i: integer;
  Humanize: boolean; const HumanizedCR, HumanizedCRLF: WideString; 
  Utf8vclActive: boolean);
var
  s: string;
  ws, el: WideString;
  id: integer;
  oTranslate: boolean;
begin
  oTranslate := Translate;
  Translate := false;
  if ResStrings <> nil then
    ResStrings.Clear
  else
    ResStrings := TStringList.Create;
  while i < sl.Count do begin
    s := sl[i];
    if (s <> '') and (s[1] <> ';') then begin
      ws := UTF8Decode(s);
      if ws = '' then
        FreeLocalizer.Error('Bad UTF-8 line in language file:'#13#10'"' + sl[i] +
          '"'#13#10'Check file encoding!')
      else begin
        if ws[1] = '(' then begin
          if copy(ws, 1, length(sDelMark)) = sDelMark then
            FreeLocalizer.Error('Obsolete line in language file:'#13#10'"' +
              sl[i] + '"'#13#10'You have to delete it!')
          else if copy(ws, 1, length(sNewMark)) = sNewMark then
            FreeLocalizer.Error('Untranslated line in language file:'#13#10'"' +
              sl[i] + '"'#13#10'You have to translate it!');
        end else begin
          if ws[1] = '[' then break;
          // 65167_ComConst_SOleError='OLE error %.8x'
          WideSplitBy(ws, '_', el);
          if not TryStrToInt(el, id) then
            FreeLocalizer.Error('Bad resource ID in language file: "' + el + '"');
          WideSplitBy(ws, '=', el);
          ws := LngToString(ws, Humanize, HumanizedCR, HumanizedCRLF, sLineBreak);

          if Utf8vclActive then
            s := ws // Wide --> UTF-8
          else
            WideToAnsiString(ws, s, GetACP);

          ResStrings.Add(s);
          ResStrings.Objects[ResStrings.Count-1] := pointer(id);
        end;
      end;
    end;
    inc(i);
  end;
  Translate := oTranslate and (ResStrings.Count > 0);
end;

function TResStringer.GetString(Id: integer; var s: AnsiString): boolean;
var
  i0, i1, i2: integer;
begin
  if ResStrings = nil then
    result := false
  else begin
    i0 := 0;
    i2 := ResStrings.Count-1;
    while i0 < i2 do begin
      i1 := (i0+i2) div 2;
      if Id > integer(ResStrings.Objects[i1]) then
        i0 := i1+1
      else
        i2 := i1;
    end;
    result := (Id = integer(ResStrings.Objects[i0]));
    if result then
      s := ResStrings[i0];
  end;
end;

{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'TFreeLocalizer'}{$endif}
{$ifndef D7}{$region 'TFreeLocalizer - Form translation'}{$endif}

{ TFreeLocalizer }

class function TFreeLocalizer.CheckUtf8vcl: boolean;
begin
  Result := GetACP = CP_UTF8;
end;

procedure TFreeLocalizer.ClearErrors;
begin
  fErrors.Clear;
end;

procedure TFreeLocalizer.ClearResForms;
var
  i: integer;
begin
  for i := 0 to length(ResForms) - 1 do begin
    ResForms[i].Props.Free;
    ResForms[i].Values.Free;
    ResForms[i].Free;
  end;
  SetLength(ResForms, 0);
end;

constructor TFreeLocalizer.Create;
begin
  fErrors := TStringList.Create;
  ResStringer := TResStringer.Create;
end;

destructor TFreeLocalizer.Destroy;
begin
  SetAutoTranslate(False);
  ResStringer.Free;
  ClearResForms;
  fErrors.Free;
  inherited;
end;

procedure TFreeLocalizer.Error(const Mess: string);
begin
  case ErrorProcessing of
    epMessage: Application.MessageBox(pChar(Mess), 'K.D.L. Error', MB_ICONERROR+MB_OK+MB_DEFBUTTON1+MB_APPLMODAL);
    epException: raise EKdlError.Create(Mess);
    epDebug: OutputDebugString(pChar(Mess));
    epErrors: fErrors.Append(Mess);
  end;
end;

procedure TFreeLocalizer.GetEncoding(sl: TStringList; var Humanize: boolean;
  var HumanizedCR, HumanizedCRLF: WideString);
var
  i: integer;
  s: string;
begin
  Humanize := false;
  HumanizedCR := defHumanizeDivider;
  HumanizedCRLF := defHumanizeDivider;

  i := sl.IndexOfName('Humanize');
  if i >= 0 then begin
{$ifdef D6}
    Humanize := (TStringsGetValueFromIndex(sl, i) = '1');
{$else}
    Humanize := (sl.ValueFromIndex[i] = '1');
{$endif}

    i := sl.IndexOfName('HumanizeDivider');
    if i >= 0 then begin // For backward compatibility
{$ifdef D6}
      s := TStringsGetValueFromIndex(sl, i);
{$else}
      s := sl.ValueFromIndex[i];
{$endif}
      HumanizedCR := UTF8Decode(s);
      HumanizedCRLF := HumanizedCR;
    end;

    i := sl.IndexOfName('HumanizedCR');
    if i >= 0 then begin
{$ifdef D6}
      s := TStringsGetValueFromIndex(sl, i);
{$else}
      s := sl.ValueFromIndex[i];
{$endif}
      HumanizedCR := UTF8Decode(s);
    end;

    i := sl.IndexOfName('HumanizedCRLF');
    if i >= 0 then begin
{$ifdef D6}
      s := TStringsGetValueFromIndex(sl, i);
{$else}
      s := sl.ValueFromIndex[i];
{$endif}
      HumanizedCRLF := UTF8Decode(s);
    end;
  end;
end;

function TFreeLocalizer.GetErrors: string;
begin
  Result := fErrors.Text;
end;

procedure TFreeLocalizer.LoadLanguageFile(const aLanguageFile: string);
const
  LngExt = '.lng';
var
  FullLangFile: string;
  sl: TStringList;
  i, iResForms: integer;
  s: string;
  ws, el: WideString;
  Utf8vclActive: boolean; // Is the UTF8VCL library linked and active?
  Humanize: boolean;
  HumanizedCR, HumanizedCRLF: WideString;
begin
  if AnsiCP = 0 then
    AnsiCP := 1250;
  if AnsiCP = 65001 then
    raise EKdlError.Create('K.D.L. initialization error.'#13#10+
      'Please put uFreeLocalizer before UTF8VCL in an uses clause of Application');

  if Assigned(fBeforeLanguageLoadEvent) then
    fBeforeLanguageLoadEvent(Self, LanguageFile, aLanguageFile);

  try
    ClearResForms;
    Utf8vclActive := CheckUtf8vcl;

    // Build Full name of language file
    FullLangFile := LanguageDir;
    if (FullLangFile <> '') and not (FullLangFile[length(FullLangFile)] in ['/', '\']) then
      FullLangFile := FullLangFile + '\';
    FullLangFile := FullLangFile + aLanguageFile;
    if not AnsiEndsText(LngExt, FullLangFile) then
      FullLangFile := FullLangFile + LngExt;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(FullLangFile);
      if (sl.Count <= 0)
        or ((sl[0] <> LngHeader)
        and (sl[0] <> UnicodeLabel + LngHeader))
      then begin
        Error('Bad signature in language file "' + FullLangFile + '"');
        exit;
      end;
      GetEncoding(sl, Humanize, HumanizedCR, HumanizedCRLF);
      iResForms := -1;
      i := 1;
      while i < sl.Count do begin
        s := sl[i];
        if (s <> '') and (s[1] <> ';') then begin
          if s[1] = '[' then begin
            if UpperCase(s) = '[RESOURCESTRINGS]' then begin
              inc(i);
              ResStringer.LoadResStrings(sl, i, Humanize, HumanizedCR,
                HumanizedCRLF, Utf8vclActive);
              continue;
            end else begin
              if copy(s, 2, length(sDelMark)) = sDelMark then begin
                Error('Deleted component in language file:'#13#10'"' + sl[i] +
                  '"'#13#10'You have to remove it!');
                exit;
              end;
              inc(iResForms);
              SetLength(ResForms, iResForms+1);
              ResForms[iResForms] := TResForm.Create;
              ResForms[iResForms].Name := copy(s, 2, length(s)-2);
              ResForms[iResForms].Props := TStringList.Create;
              ResForms[iResForms].Values := TStringList.Create;
            end;
          end else if iResForms >= 0 then begin
            ws := UTF8Decode(s);
            if ws = '' then
              Error('Bad UTF-8 line in language file:'#13#10'"' + sl[i] +
                '"'#13#10'Check file encoding!')
            else begin
              if ws[1] = '(' then begin
                if copy(ws, 1, length(sDelMark)) = sDelMark then
                  Error('Obsolete line in language file:'#13#10'"' + sl[i] +
                    '"'#13#10'You have to remove it!')
                else if copy(ws, 1, length(sNewMark)) = sNewMark then
                  Error('Untranslated line in language file:'#13#10'"' + sl[i] +
                    '"'#13#10'You have to translate it!');
              end else begin
                WideSplitBy(ws, '=', el);

                ws := LngToString(ws, Humanize, HumanizedCR, HumanizedCRLF, #13);
                if Utf8vclActive then
                  s := ws // Wide --> UTF-8
                else
                  WideToAnsiString(ws, s, GetACP);
                ResForms[iResForms].Values.Add(s);

                // btnNewForm.Caption{1}  -> drop version #
                WideSplitBy(el, '{', ws);
                if ws = '' then begin
                  Error('Bad line in language file: "' + sl[i] + '"');
                  exit;
                end;
                ResForms[iResForms].Props.Add(ws);
              end;
            end;
          end;
        end;
        inc(i);
      end;
    finally
      sl.Free;
    end;
    fLanguageFile := aLanguageFile;
    if AutoTranslate then TranslateScreen;

    if Assigned(fAfterLanguageLoadEvent) then
      fAfterLanguageLoadEvent(Self, fLanguageFile);
  except
    on E: Exception do
      Error('Error while loading language file "' + FullLangFile + '"'#13#10 + E.Message);
  end;
end;

procedure TFreeLocalizer.TranslateAs(Comp: TComponent; const CompClassType: TClass);
var
  ResForm: TResForm;
  ParentClassType: TClass;
  i: integer;
begin
  // Whether the component's ancestor can contain localizable controls?
  ParentClassType := CompClassType.ClassParent;
  if (ParentClassType <> TForm)
    and (ParentClassType <> TDataModule)
    and (ParentClassType <> TObject)
  then
    TranslateAs(Comp, ParentClassType)
  else begin
    // Translate nested frames
    for i := 0 to Comp.ComponentCount - 1 do
      if Comp.Components[i] is TFrame then
        FreeLocalizer.Translate(Comp.Components[i]);
  end;

  ResForm := Nil;
  for i := 0 to length(ResForms)-1 do
    if CompClassType.ClassName = ResForms[i].Name then begin
      ResForm := ResForms[i];
      break;
    end;
  if ResForm = Nil then exit; // This component not translated
  for i := 0 to ResForm.Props.Count - 1 do
    TranslateProp(Comp, ResForm.Props[i], ResForm.Values[i]);
end;

procedure TFreeLocalizer.Translate(Comp: TComponent);
begin
  TranslateAs(Comp, Comp.ClassType);
end;

procedure TFreeLocalizer.TranslateProp(RootComp: TComponent; const PropName,
  PropValue: string);

  procedure SetStringsProp(st: TStrings);
  var
    i: integer;
    s, el: string;
  begin
    s := PropValue;
    i := 0;
    st.BeginUpdate;
    try
      while s <> '' do begin
        SplitBy(s, ListDivider, el);
        if i < st.Count
        then st[i] := el
        else st.Add(el);
        inc(i);
      end;
      while st.Count > i do
        st.Delete(st.Count-1);
    finally
      st.EndUpdate;
    end;
  end;

  procedure SetProp(Obj: TObject; const pName: string);
  var
    PropInfo: PPropInfo;
  begin
    if Obj is TStrings
    then SetStringsProp(Obj as TStrings)
    else begin
      PropInfo := GetPropInfo(Obj.ClassInfo, pName);
      if PropInfo <> Nil // Property exists
      then
        {$ifdef D7}
          SetPropValue(Obj, pName, PropValue)
        {$else}
          SetPropValue(Obj, PropInfo, PropValue)
        {$endif}
      else raise EKdlSilentError.Create;
    end;
  end;

label
  CheckComp, CheckClass;
var
  s, el: string;
  Comp, cmp, OwnerComp: TComponent;
  obj: TObject;
  PropInfo: PPropInfo;
  i: integer;
begin
  try
    OwnerComp := RootComp;
    Comp := RootComp;
    s := PropName;
    repeat
      SplitBy(s, '.', el);
      CheckComp:
      if s = '' then begin // el is property name
        SetProp(Comp, el);
        exit;
      end;
      cmp := Comp.FindComponent(el);
      if cmp = Nil then break;
      Comp := cmp;
      if Comp is TFrame then
        OwnerComp := Comp;
    until false;

    // Check for nested classes
    obj := Comp;
    while Obj is TPersistent do begin
      PropInfo := GetPropInfo(obj.ClassInfo, el);
      if (PropInfo = Nil) or (PropInfo.PropType^.Kind <> tkClass)
      then break; // Such class property not exists
      {$ifdef D7}
        obj := pointer(longint(GetPropValue(Obj, el)));
      {$else}
        obj := pointer(longint(GetPropValue(Obj, PropInfo)));
      {$endif}
      CheckClass:
      SplitBy(s, '.', el);
      if s = '' then begin // el is property name
        SetProp(obj, el);
        exit;
      end;
      if Obj is TCollection then
        break;
    end;

    // Check for nested TCollection
    if (obj is TCollection)
      and (length(el) >= 3)
      and (el[1] = '(')
      and (el[length(el)] = ')')
      and TryStrToInt(copy(el, 2, length(el)-2), i)
    then begin
      // el = '(0)'   s = ...rest of nested classes and properties
      obj := (obj as TCollection).Items[i];
      goto CheckClass;
    end;

    // Try to find out el among components of OwnerComp
    if Comp <> OwnerComp then begin
      Comp := OwnerComp;
      goto CheckComp;
    end;

    // yet untranslated...
    raise EKdlSilentError.Create;

  except
    on E: EKdlSilentError do begin
      s := 'Unknown property "%s" found in component "%s".'#13#10 +
        'Remove it from language file';
      Error(Format(s, [PropName, RootComp.Name]));
    end;
    on E: Exception do begin
      s := 'Translation error of property "%s" in component "%s"'#13#10 + E.Message;
      Error(Format(s, [PropName, RootComp.Name]));
    end;
  end;
end;

procedure TFreeLocalizer.TranslateScreen;
var
  i: integer;
begin
  for i := 0 to Screen.FormCount - 1 do Translate(Screen.Forms[i]);
end;

{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'TFreeLocalizer - Auto translation feature'}{$endif}

function MyInitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
begin
  FreeLocalizer.InitInheritedRepl.Replaced := false;
  try
    result := InitInheritedComponent(Instance, RootAncestor);
    FreeLocalizer.Translate(Instance);
  finally
    FreeLocalizer.InitInheritedRepl.Replaced := true;
  end;
end;

procedure TFreeLocalizer.SetAutoTranslate(aAutoTranslate: boolean);
begin
  if aAutoTranslate = fAutoTranslate then exit;
  if aAutoTranslate then begin
    InitInheritedRepl := TFuncReplacement.Create(
      @Classes.InitInheritedComponent,
      @MyInitInheritedComponent);
    InitInheritedRepl.Replaced := true;
  end else begin
    InitInheritedRepl.Free;
  end;
  fAutoTranslate := aAutoTranslate;
end;

procedure TFreeLocalizer.SetTranslateResourceStrings(aTranslate: boolean);
begin
  ResStringer.Translate := aTranslate;
end;

{$ifndef D7}{$endregion}{$endif}
{$ifndef D7}{$endregion}{$endif}

{$ifndef D7}{$region 'TFuncReplacement'}{$endif}

{ TFuncReplacement }

type
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;

function IsWin9xDebugThunk(AnAddr: Pointer): Boolean; // extract from JclPeImage.pas
{ -> EAX: AnAddr }
asm
  TEST EAX, EAX
  JZ  @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.PUSH, $68
  JNE @@NoThunk
  CMP BYTE PTR [EAX].TWin9xDebugThunk.JMP, $E9
  JNE @@NoThunk
  XOR EAX, EAX
  MOV AL, 1
  JMP @@exit
@@NoThunk:
  XOR EAX, EAX
@@exit:
end;

function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then // JMP mem32
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end else
    Result := nil;
end;

constructor TFuncReplacement.Create(aOrigFunc, aMyFunc: pointer);
var
  OldProtect: cardinal;
begin
  OrigFunc := GetActualAddr(aOrigFunc);
  MyFunc := aMyFunc;
  move(OrigFunc^, OrigDump[0], 5);
  VirtualProtect(OrigFunc, 5, PAGE_EXECUTE_READWRITE, @OldProtect);
end;

destructor TFuncReplacement.Destroy;
begin
  SetReplaced(false);
  inherited;
end;

procedure TFuncReplacement.SetReplaced(aReplaced: boolean);
var
  Offset: integer;
begin
  if aReplaced = fReplaced then exit;
  if aReplaced then begin // Set MyFunc
    Offset := integer(MyFunc) - integer(OrigFunc) - 5;
    byte(OrigFunc^) := $E9;
    move(Offset, pointer(cardinal(OrigFunc)+1)^, 4);
  end else // Set OrigFunc
    move(OrigDump[0], OrigFunc^, 5);
  fReplaced := aReplaced;
end;

{$ifndef D7}{$endregion}{$endif}

Initialization
  AnsiCP := GetACP;
  SetMultiByteConversionCodePage(CP_THREAD_ACP);

  FreeLocalizer := TFreeLocalizer.Create;
Finalization
  FreeLocalizer.Free;
end.
