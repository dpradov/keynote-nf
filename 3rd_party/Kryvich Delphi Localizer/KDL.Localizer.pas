{ Kryvich's Delphi Localizer Class
  Copyright (C) 2006 - 2018 Kryvich, Belarusian Linguistic Software team.
}

unit KDL.Localizer;
{$I NoRTTI}

interface

uses
  Classes, KDL.Detours;

const
  KDL_PLATFORM =
{$IF DEFINED(VCL)} 'VCL'
{$ELSEIF DEFINED(FMX)} 'FMX'
{$ELSEIF DEFINED(NOGUI)} 'NOGUI'
{$ELSE}
  {$Message Fatal 'One of the following symbols must be defined: VCL, FMX or NOGUI'}
{$IFEND};

type
  // Method of error processing
  TErrorProcessing = (
    epSilent,    // Just skip errors (default) - use for public releases
    epMessage,   // Show message to an user - use for beta testing
    epException, // Raise exception - use while develop and debug
    epDebug,     // Use DebugOutputString
    epErrors     // Append all messages to a string list
  );

  // Translated form properties
  TResForm = class
  public
    Name: string;         // Form name
    Props: TStringList;   // Property names
    Values: TStringList;  // Translated property values
  end;

  // Events of Localizer
  TBeforeLanguageLoadEvent = procedure(Sender: TObject; const OldLanguageFile,
    NewLanguageFile: string) of object;
  TAfterLanguageLoadEvent = procedure(Sender: TObject;
    const LanguageFile: string) of object;

  TFreeLocalizer = class
  private
    fLanguageFile: string; // Loaded language file
    ResForms: array of TResForm; // List of all localized forms
    fAutoTranslate: Boolean;
    fBeforeLanguageLoadEvent: TBeforeLanguageLoadEvent;
    fAfterLanguageLoadEvent: TAfterLanguageLoadEvent;
    fErrors: TStrings;
{$IF NOT DEFINED(NOGUI)}
    InitInheritedRepl: TFuncReplacement; // InitInheritedComponent replacement
{$IFEND}
    // Get Humanize settings of a language file
    procedure GetEncoding(sl: TStringList; var Humanize: Boolean;
      var HumanizedCR, HumanizedCRLF, HumanizedLF: string);
    // Delete old translations in ResForms
    procedure ClearResForms;
    // Load translations from file
    procedure LoadLanguageFile(const aLanguageFile: string);
{$IF NOT DEFINED(NOGUI)}
    // Set value PropValue for property PropName in Component RootComp
    procedure TranslateProp(RootComp: TComponent; const PropName, PropValue: string);
    // Translate component (form) as component of class CompClassType
    procedure TranslateAs(Comp: TComponent; const CompClassType: TClass);
{$IFEND}
    // Enable/disable autotranslation feature
    procedure SetAutoTranslate(aAutoTranslate: Boolean);
    // Enable/disable translation of resource strings
    procedure EnableResStringer(DoEnable: Boolean);
    // Called when error encountered
    procedure Error(const Mess: string);
    // Get error messages from fErrors
    function GetErrors: string;
  public
    LanguageDir: string; // Directory with language files (optional)
    ErrorProcessing: TErrorProcessing;

    constructor Create;
    destructor Destroy; override;
{$IF NOT DEFINED(NOGUI)}
    // Translate component (form)
    procedure Translate(Comp: TComponent);
    // Translate all forms on Screen
    procedure TranslateScreen;
{$IFEND}
    // Clear error messages in fErrors
    procedure ClearErrors;
    // Error messages (set ErrorProcessing to epErrors)
    property Errors: string read GetErrors;
    // Language file name. Set it to load a new translation
    property LanguageFile: string read fLanguageFile write LoadLanguageFile;
    // Enable/disable translation of resource strings
    property TranslateResourceStrings: Boolean write EnableResStringer;
    // Auto translate a form after creating
    property AutoTranslate: Boolean read fAutoTranslate write SetAutoTranslate;
    // Occurs exactly before loading new language file.
    // You can call the silent exception (Abort) to abort the operation
    property BeforeLanguageLoad: TBeforeLanguageLoadEvent
      read fBeforeLanguageLoadEvent write fBeforeLanguageLoadEvent;
    // Occurs exactly after a new language was loaded.
    // Do here necessary operations such as calling TranslateScreen
    // (if AutoTranslate is disabled) and updating of controls state
    property AfterLanguageLoad: TAfterLanguageLoadEvent
      read fAfterLanguageLoadEvent write fAfterLanguageLoadEvent;
  end;

var
  FreeLocalizer: TFreeLocalizer;

implementation

uses
  Windows, SysUtils, TypInfo, KDL.StringUtils, StrUtils
{$IF DEFINED(VCL)}
  , Vcl.Forms
{$ELSEIF DEFINED(FMX)}
  , FMX.Forms, System.UITypes, FMX.DialogService.Sync
{$ELSEIF DEFINED(NOGUI)}
  // No GUI framework used
{$IFEND};

resourcestring
  rsKdlMark = '*KDL*Mark*';

const
  LngHeader = '; Kryvich''s Delphi Localizer Language File.';
  sNewMark = '(!)';
  sDelMark = '(x)';

{$region 'EKdlError'}
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
{$endregion}

{$region 'TResStringer'}
type
  TResStringer = class
  private
    LoadResRepl: TFuncReplacement; // LoadResString replacement
    ResStrings: TStringList; // Translated resource strings
    fEnabled: Boolean; // Do translations of resource strings
    fSelfTestMode: Boolean;

    // Get resource string
    function GetString(Id: Integer; var s: string): Boolean;
    // Set translation status
    procedure SetEnabled(aEnabled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    // Read resource strings from sl into ResStrings
    procedure LoadResStrings(sl: TStringList; var i: Integer;
      Humanize: Boolean; const HumanizedCR, HumanizedCRLF, HumanizedLF: string);
    property Enabled: Boolean read fEnabled write SetEnabled;
  end;

var
  ResStringer: TResStringer;

function MyLoadResString(ResStringRec: PResStringRec): string;

  function GetNotTranslated: string;
  begin
    ResStringer.Enabled := False;
    try
      Result := System.LoadResString(ResStringRec);
    finally
      ResStringer.Enabled := True;
    end;
  end;

begin
  if ResStringRec = nil then
    Exit;
  if Assigned(ResStringer) and ResStringer.Enabled then begin
    if ResStringRec.Identifier >= 64*1024 then
      Result := PChar(ResStringRec.Identifier)
    else if not ResStringer.GetString(ResStringRec.Identifier, Result) then
      if ResStringer.fSelfTestMode then
        Result := ''
      else
        Result := GetNotTranslated;
  end else
    Result := System.LoadResString(ResStringRec);
end;

{ TResStringer }

constructor TResStringer.Create;
begin
  LoadResRepl := TFuncReplacement.Create(@System.LoadResString, @MyLoadResString);
end;

destructor TResStringer.Destroy;
begin
  Enabled := False;
  FreeAndNil(ResStrings);
  LoadResRepl.Free;
  inherited;
end;

procedure TResStringer.SetEnabled(aEnabled: Boolean);
begin
  LoadResRepl.Replaced := aEnabled;
  fEnabled := aEnabled;
end;

procedure TResStringer.LoadResStrings(sl: TStringList; var i: Integer;
  Humanize: Boolean; const HumanizedCR, HumanizedCRLF, HumanizedLF: string);
const
  KdlMarkStringName = 'KDL_Localizer_rsKdlMark';
var
  s, el: string;
  id: Integer;
  oEnabled: Boolean;
  kdlMarkFound: Boolean;
begin
  oEnabled := Enabled;
  Enabled := False;
  if ResStrings <> nil then
    ResStrings.Clear
  else
    ResStrings := TStringList.Create;
  kdlMarkFound := False;
  while i < sl.Count do begin
    s := sl[i];
    if (s <> '') and (s[1] <> ';') then begin
      if s[1] = '(' then begin
        if Copy(s, 1, Length(sDelMark)) = sDelMark then
          FreeLocalizer.Error('Obsolete line in language file:'#13#10'"' +
            sl[i] + '"'#13#10'You have to delete it!')
        else if Copy(s, 1, Length(sNewMark)) = sNewMark then
          FreeLocalizer.Error('Untranslated line in language file:'#13#10'"' +
            sl[i] + '"'#13#10'You have to translate it!');
      end else begin
        if s[1] = '[' then
          Break;
        // 65167_ComConst_SOleError='OLE error %.8x'
        SplitBy(s, '_', el);
        if not TryStrToInt(el, id) then
          FreeLocalizer.Error('Bad resource ID in language file: "' + el + '"');
        SplitBy(s, '=', el);
        kdlMarkFound := kdlMarkFound or (el = KdlMarkStringName);
        s := LngToString(s, Humanize, HumanizedCR, HumanizedCRLF, HumanizedLF,
          sLineBreak);
        ResStrings.Add(s);
        ResStrings.Objects[ResStrings.Count-1] := Pointer(id);
      end;
    end;
    Inc(i);
  end;
  if not kdlMarkFound then begin
    FreeLocalizer.Error('Can''t find the special string ' + KdlMarkStringName
      + ' in the loaded language file. This language file is corrupted.');
    ResStrings.Clear;
  end else begin
    try
      fSelfTestMode := True;
      Enabled := True;
      if rsKdlMark <> '*KDL*Mark*' then begin
        ResStrings.Clear;
        FreeLocalizer.Error(
          'Strings section in the loaded language file is outdated.'#13#10
          + 'Messages of this application will not be translated.');
      end;
    finally
      Enabled := False;
      fSelfTestMode := False;
    end;
  end;
  Enabled := oEnabled and (ResStrings.Count > 0);
end;

function TResStringer.GetString(Id: Integer; var s: string): Boolean;
var
  i0, i1, i2: Integer;
begin
  if ResStrings = nil then
    Result := False
  else begin
    i0 := 0;
    i2 := ResStrings.Count-1;
    while i0 < i2 do begin
      i1 := (i0+i2) div 2;
      if Id > Integer(ResStrings.Objects[i1]) then
        i0 := i1+1
      else
        i2 := i1;
    end;
    Result := (Id = Integer(ResStrings.Objects[i0]));
    if Result then
      s := ResStrings[i0];
  end;
end;
{$endregion}

{$region 'TFreeLocalizer'}
procedure TFreeLocalizer.ClearErrors;
begin
  fErrors.Clear;
end;

procedure TFreeLocalizer.ClearResForms;
var
  i: Integer;
begin
  for i := 0 to Length(ResForms) - 1 do begin
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
  ResStringer.Enabled := True;
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
    epMessage:
{$IF DEFINED(VCL)}
      Application.MessageBox(pChar(Mess), 'K.D.L. Error',
        MB_ICONERROR+MB_OK+MB_DEFBUTTON1+MB_APPLMODAL);
{$ELSEIF DEFINED(FMX)}
      TDialogServiceSync.MessageDialog(Mess, TMsgDlgType.mtError,
        [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0);
{$ELSE}
      Writeln('K.D.L. Error: ', Mess);
{$IFEND}
    epException: raise EKdlError.Create(Mess);
    epDebug: OutputDebugString(pChar(Mess));
    epErrors: fErrors.Append(Mess);
  end;
end;

procedure TFreeLocalizer.GetEncoding(sl: TStringList; var Humanize: Boolean;
  var HumanizedCR, HumanizedCRLF, HumanizedLF: string);
var
  i: Integer;
  s: string;
begin
  Humanize := False;
  HumanizedCR := defHumanizeDivider;
  HumanizedCRLF := defHumanizeDivider;
  HumanizedLF := defHumanizeDivider;

  i := sl.IndexOfName('Humanize');
  if i >= 0 then begin
    Humanize := (sl.ValueFromIndex[i] = '1');

    i := sl.IndexOfName('HumanizeDivider');
    if i >= 0 then begin // For backward compatibility
      s := sl.ValueFromIndex[i];
      HumanizedCR := s;
      HumanizedCRLF := HumanizedCR;
    end;

    i := sl.IndexOfName('HumanizedCR');
    if i >= 0 then begin
      s := sl.ValueFromIndex[i];
      HumanizedCR := s;
    end;

    i := sl.IndexOfName('HumanizedCRLF');
    if i >= 0 then begin
      s := sl.ValueFromIndex[i];
      HumanizedCRLF := s;
    end;

    i := sl.IndexOfName('HumanizedLF');
    if i >= 0 then begin
      s := sl.ValueFromIndex[i];
      HumanizedLF := s;
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
  i, iResForms: Integer;
  s, el: string;
  Humanize: Boolean;
  HumanizedCR, HumanizedCRLF, HumanizedLF: string;
begin
  if Assigned(fBeforeLanguageLoadEvent) then
    fBeforeLanguageLoadEvent(Self, LanguageFile, aLanguageFile);

  try
    ClearResForms;

    // Build Full name of language file
    FullLangFile := LanguageDir;
    if (FullLangFile <> '')
      and not CharInSet(FullLangFile[Length(FullLangFile)], ['/', '\'])
    then
      FullLangFile := FullLangFile + '\';
    FullLangFile := FullLangFile + aLanguageFile;
    if not AnsiEndsText(LngExt, FullLangFile) then
      FullLangFile := FullLangFile + LngExt;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(FullLangFile, TEncoding.UTF8);
      if (sl.Count <= 0)
        or (sl[0] <> LngHeader)
      then begin
        Error('Bad signature in language file "' + FullLangFile + '"');
        Exit;
      end;
      GetEncoding(sl, Humanize, HumanizedCR, HumanizedCRLF, HumanizedLF);
      iResForms := -1;
      i := 1;
      while i < sl.Count do begin
        s := sl[i];
        if (s <> '') and (s[1] <> ';') then begin
          if s[1] = '[' then begin
            if UpperCase(s) = '[RESOURCESTRINGS]' then begin
              Inc(i);
              ResStringer.LoadResStrings(sl, i, Humanize, HumanizedCR,
                HumanizedCRLF, HumanizedLF);
              Continue;
            end else begin
              if Copy(s, 2, Length(sDelMark)) = sDelMark then begin
                Error('Deleted component in language file:'#13#10'"' + sl[i] +
                  '"'#13#10'You have to remove it!');
                Exit;
              end;
              Inc(iResForms);
              SetLength(ResForms, iResForms+1);
              ResForms[iResForms] := TResForm.Create;
              ResForms[iResForms].Name := Copy(s, 2, Length(s)-2);
              ResForms[iResForms].Props := TStringList.Create;
              ResForms[iResForms].Values := TStringList.Create;
            end;
          end else if iResForms >= 0 then begin
            if s[1] = '(' then begin
              if Copy(s, 1, Length(sDelMark)) = sDelMark then
                Error('Obsolete line in language file:'#13#10'"' + sl[i] +
                  '"'#13#10'You have to remove it!')
              else if Copy(s, 1, Length(sNewMark)) = sNewMark then
                Error('Untranslated line in language file:'#13#10'"' + sl[i] +
                  '"'#13#10'You have to translate it!');
            end else begin
              SplitBy(s, '=', el);
              s := LngToString(s, Humanize, HumanizedCR, HumanizedCRLF,
                HumanizedLF, #13);
              ResForms[iResForms].Values.Add(s);

              // btnNewForm.Caption{1}  -> drop version #
              SplitBy(el, '{', s);
              if s = '' then begin
                Error('Bad line in language file: "' + sl[i] + '"');
                Exit;
              end;
              ResForms[iResForms].Props.Add(s);
            end;
          end;
        end;
        Inc(i);
      end;
    finally
      sl.Free;
    end;
    fLanguageFile := aLanguageFile;
{$IF NOT DEFINED(NOGUI)}
    if AutoTranslate then
      TranslateScreen;
{$IFEND}
    if Assigned(fAfterLanguageLoadEvent) then
      fAfterLanguageLoadEvent(Self, fLanguageFile);
  except
    on E: Exception do
      Error('Error while loading language file "' + FullLangFile + '"'#13#10
        + E.Message);
  end;
end;

{$IF NOT DEFINED(NOGUI)}
procedure TFreeLocalizer.TranslateAs(Comp: TComponent;
  const CompClassType: TClass);
var
  ResForm: TResForm;
  ParentClassType: TClass;
  i: Integer;
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
  for i := 0 to Length(ResForms)-1 do
    if CompClassType.ClassName = ResForms[i].Name then begin
      ResForm := ResForms[i];
      Break;
    end;
  if ResForm = Nil then Exit; // This component not translated
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
    i: Integer;
    s, el: string;
  begin
    s := PropValue;
    i := 0;
    st.BeginUpdate;
    try
      while s <> '' do begin
        SplitBy(s, ListDivider, el);
        if i < st.Count then
          st[i] := el
        else
          st.Add(el);
        Inc(i);
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
    if Obj is TStrings then
      SetStringsProp(Obj as TStrings)
    else begin
      PropInfo := GetPropInfo(Obj.ClassInfo, pName);
      if PropInfo <> Nil then // Property exists
        SetPropValue(Obj, PropInfo, PropValue)
      else
        raise EKdlSilentError.Create;
    end;
  end;

label
  CheckComp, CheckClass;
var
  s, el: string;
  Comp, cmp, OwnerComp: TComponent;
  obj: TObject;
  PropInfo: PPropInfo;
  i: Integer;
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
        Exit;
      end;
      cmp := Comp.FindComponent(el);
      if cmp = Nil then
        Break;
      Comp := cmp;
      if Comp is TFrame then
        OwnerComp := Comp;
    until False;

    // Check for nested classes
    obj := Comp;
    while Obj is TPersistent do begin
      PropInfo := GetPropInfo(obj.ClassInfo, el);
      if (PropInfo = Nil) or (PropInfo.PropType^.Kind <> tkClass) then
        Break; // Such class property not exists
      obj := Pointer(longint(GetPropValue(Obj, PropInfo)));
      CheckClass:
      SplitBy(s, '.', el);
      if s = '' then begin // el is property name
        SetProp(obj, el);
        Exit;
      end;
      if Obj is TCollection then
        Break;
    end;

    // Check for nested TCollection
    if (obj is TCollection)
      and (Length(el) >= 3)
      and (el[1] = '(')
      and (el[Length(el)] = ')')
      and TryStrToInt(Copy(el, 2, Length(el)-2), i)
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
      s := 'Unknown property "%s" found in component "%s".'#13#10
        + 'Remove it from language file';
      Error(Format(s, [PropName, RootComp.Name]));
    end;
    on E: Exception do begin
      s := 'Translation error of property "%s" in component "%s"'#13#10
        + E.Message;
      Error(Format(s, [PropName, RootComp.Name]));
    end;
  end;
end;

procedure TFreeLocalizer.TranslateScreen;
var
  i: Integer;
begin
  for i := 0 to Screen.FormCount - 1 do Translate(Screen.Forms[i]);
end;

function MyInitInheritedComponent(Instance: TComponent;
  RootAncestor: TClass): Boolean;
begin
  FreeLocalizer.InitInheritedRepl.Replaced := False;
  try
    Result := InitInheritedComponent(Instance, RootAncestor);
    FreeLocalizer.Translate(Instance);
  finally
    FreeLocalizer.InitInheritedRepl.Replaced := True;
  end;
end;

procedure TFreeLocalizer.SetAutoTranslate(aAutoTranslate: Boolean);
begin
  if aAutoTranslate = fAutoTranslate then
    Exit;
  if aAutoTranslate then begin
    InitInheritedRepl := TFuncReplacement.Create(
      @Classes.InitInheritedComponent,
      @MyInitInheritedComponent);
    InitInheritedRepl.Replaced := True;
  end else begin
    InitInheritedRepl.Free;
  end;
  fAutoTranslate := aAutoTranslate;
end;
{$ELSE}
procedure TFreeLocalizer.SetAutoTranslate(aAutoTranslate: Boolean);
begin
  fAutoTranslate := False;  // Auto translation is for GUI only
end;
{$IFEND}

procedure TFreeLocalizer.EnableResStringer(DoEnable: Boolean);
begin
  ResStringer.Enabled := DoEnable;
end;
{$endregion}

Initialization
  FreeLocalizer := TFreeLocalizer.Create;
Finalization
  FreeLocalizer.Free;
end.
