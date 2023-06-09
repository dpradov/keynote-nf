{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997, 2000 Vladimir Gaitanoff   }
{                                                       }
{ Adopted from Delphi VCL Library (vgLib)               }
{ National Language Support                             }
{ Unicode revision (utf-8) by JB.                       }
{*******************************************************}

unit RxTranslate;

{$I RX.INC }

interface

uses
  Windows, SysUtils, Classes, IniFiles,
  {$IFDEF UNICODE}
  Generics.Collections,
  {$ENDIF}
  RxPlacemnt;

type
  TTranslateMsgEvent = procedure(Sender: TObject; const OldMsg: string;
    var NewMsg: string) of object;

  TTranslatePropEvent = procedure(Sender: TObject; const PropPath, OldPropValue: string;
    var NewPropValue: string) of object;

  TTranslatePropStringEvent = procedure(Sender: TObject; Instance: TObject; const PropName: string;
    const OldPropValue: string; var NewPropValue: string) of object;

  TTranslatePropInstanceEvent = procedure(Sender: TObject; Instance: TObject) of object;

  {$IFNDEF RX_D4}
  TCustomIniFile = TIniFile;
  {$ENDIF}

  {  TRxCustomTranslator  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxCustomTranslator = class(TComponent)
  private
    FActive, FStreamedActive: Boolean;
    FProps: TStrings;
    FRoot: TObject;
    FTransList: TList{$IFDEF UNICODE}<Pointer>{$ENDIF};
    FTransStack: string;
    FOnTranslateMsg: TTranslateMsgEvent;
    FOnTranslateProp: TTranslatePropEvent;
    FOnTranslatePropString: TTranslatePropStringEvent;
    FOnTranslatePropInstance: TTranslatePropInstanceEvent;
    procedure SetActive(Value: Boolean);
    procedure SetProps(Value: TStrings);
  protected
    procedure DoActivate; virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    function DoTranslateMsg(const Msg: string): string; virtual;
    function DoTranslateProp(const PropPath, PropValue: string): string; virtual;
    procedure DoTranslatePropString(Instance: TObject; const PropName: string;
      const OldPropValue: string; var NewPropValue: string); virtual;
    procedure DoTranslatePropInstance(Instance: TObject); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TranslatePath: string;
    function TranslatePathProp(const PropName: string): string;
    function TranslateProp(const PropPath, PropValue: string): string;
    procedure TranslatePropClass(Instance: TObject; const PropName: string);
    procedure TranslatePropString(Instance: TObject; const PropName: string);
    procedure TranslateProps(Instance: TObject);
    function TranslateMessage(const Msg: string): string;
    function TranslateUserMessage(const Msg: string): string; { For TLanguage compability }
    function TMsg(const Msg: string): string; { TranslateMessage macro }
    procedure Translate;
    property Root: TObject read FRoot;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Properties: TStrings read FProps write SetProps;
    property OnTranslateMsg: TTranslateMsgEvent read FOnTranslateMsg write FOnTranslateMsg;
    property OnTranslateProp: TTranslatePropEvent read FOnTranslateProp write FOnTranslateProp;
    property OnTranslatePropString: TTranslatePropStringEvent read FOnTranslatePropString write FOnTranslatePropString;
    property OnTranslatePropInstance: TTranslatePropInstanceEvent read FOnTranslatePropInstance write FOnTranslatePropInstance;
  end;

  TComponentCallback = procedure(Instance: TComponent; Data: Pointer);

  {  TRxTranslator  }

  {$IFDEF RX_D16}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRxTranslator = class(TRxCustomTranslator)
  private
    FInCreateFile: Boolean;
    FLanguageFileName: TFileName;
    FLanguageFile: TCustomIniFile;
    {$IFDEF RX_D4}
    FFormPlacementFile: Boolean;
    FIniFileLink: TIniLink;
    {$ENDIF}
    procedure SetIniFileName(const Value: TFileName);
    {$IFDEF RX_D4}
    function GetFormPlacement: TFormPlacement;
    procedure SetFormPlacement(Value: TFormPlacement);
    procedure IniDestroy(Sender: TObject);
    {$ENDIF}
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
  protected
    procedure DoActivate; override;
    procedure DoDeactivate; override;
    function DoTranslateMsg(const Msg: string): string; override;
    function DoTranslateProp(const PropPath, PropValue: string): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateLanguageFile(const FileName: TFileName; SaveOld: Boolean);
    property IniFile: TCustomIniFile read FLanguageFile;
  published
    property Active: Boolean read GetActive write SetActive default False;
    {$IFDEF RX_D4}
    property FormPlacement: TFormPlacement read GetFormPlacement write SetFormPlacement;
    {$ENDIF}
    property LanguageFileName: TFileName read FLanguageFileName write SetIniFileName;
  end;

const {Do Not Translate}
  SectionTranslate = 'Translations';
  SectionMessages = 'Messages';

{ Utility routines }

function TranslateStringsToString(Strings: TStrings): string; {$IFDEF RX_D9}inline; {$ENDIF}
procedure TranslateStringToStrings(Msg: string; Strings: TStrings); {$IFDEF RX_D9}inline; {$ENDIF}

implementation

uses
  TypInfo, RxVCLUtils, Forms;

const
  StringProps = [tkWString, {$IFDEF UNICODE}tkUString, {$ENDIF}tkString, tkLString];
  ClassProps = [tkClass];
  DefDelimeter = '.';

function TranslateStringsToString(Strings: TStrings): string;
{$IFNDEF RX_D6}
var
  S: string;
  I, J: Integer;
  {$ENDIF}
begin
  {$IFDEF RX_D6}
  Result := Strings.CommaText;
  {$ELSE}
  Result := '';
  for I := 0 to Strings.Count - 1 do
  begin
    S := Strings[I];

    for J := 1 to Length(S) do
      if S[J] = ',' then
        Result := Result + ',,'
      else
        Result := Result + S[J];

    Result := Result + ',';
  end;
  System.Delete(Result, Length(Result), 1);
  {$ENDIF}
end;

procedure TranslateStringToStrings(Msg: string; Strings: TStrings);
{$IFNDEF RX_D6}
var
  P: PChar;
  Tmp: string;
  {$ENDIF}
begin
  {$IFDEF RX_D6}
  Strings.CommaText := Msg;
  {$ELSE}
  Strings.BeginUpdate;
  try
    Strings.Clear;

    Tmp := '';
    P := PChar(Msg);
    while P^ <> #0 do
    begin
      if P^ = ',' then
      begin
        Inc(P);
        if (P^ <> ',') then
        begin
          Strings.Add(Tmp);
          Tmp := '';
          Continue;
        end;
      end;
      Tmp := Tmp + P^;
      Inc(P);
      if P^ = #0 then Strings.Add(Tmp);
    end;
  finally
    Strings.EndUpdate;
  end;
  {$ENDIF}
end;

{ TRxCustomTranslator }

constructor TRxCustomTranslator.Create(AOwner: TComponent);
begin
  inherited;
  FProps := TStringList.Create;
end;

destructor TRxCustomTranslator.Destroy;
begin
  SetActive(False);
  FProps.Free;
  inherited;
end;

procedure TRxCustomTranslator.Loaded;
begin
  inherited;
  try
    SetActive(FStreamedActive);
    if not (csDesigning in ComponentState) and Active then
      Translate;
  except
    if (csDesigning in ComponentState) then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TRxCustomTranslator.SetActive(Value: Boolean);
begin
  if csLoading in ComponentState then
    FStreamedActive := Value
  else if FActive <> Value then
  begin
    if Value then
      DoActivate
    else
      DoDeactivate;
    FActive := Value;
  end;
end;

procedure TRxCustomTranslator.SetProps(Value: TStrings);
begin
  FProps.Assign(Value);
end;

function TRxCustomTranslator.DoTranslateMsg(const Msg: string): string;
begin
  Result := Msg;
  if Assigned(FOnTranslateMsg) then
    FOnTranslateMsg(Self, Msg, Result);
end;

function TRxCustomTranslator.DoTranslateProp(const PropPath, PropValue: string): string;
begin
  Result := PropValue;
  if Assigned(FOnTranslateProp) then
    FOnTranslateProp(Self, PropPath, PropValue, Result);
end;

procedure TRxCustomTranslator.DoTranslatePropString(Instance: TObject; const PropName: string;
  const OldPropValue: string; var NewPropValue: string);
var
  PropPath: string;
begin
  PropPath := TranslatePathProp(PropName);

  if (FProps.Count = 0) or (FProps.IndexOf(PropPath) >= 0) then
  begin
    if Assigned(FOnTranslatePropString) then
      FOnTranslatePropString(Self, Instance, PropName, OldPropValue, NewPropValue)
    else
      NewPropValue := TranslateProp(PropPath, OldPropValue);
  end;
end;

procedure TRxCustomTranslator.DoTranslatePropInstance(Instance: TObject);
var
  I: Integer;
  SaveStack: string;
  NewPropValue, OldPropValue: string;
begin
  if Instance is TStrings then
  begin
    OldPropValue := TranslateStringsToString(TStrings(Instance));
    NewPropValue := OldPropValue;
    DoTranslatePropString(Instance, '', OldPropValue, NewPropValue);
    if OldPropValue <> NewPropValue then
      TranslateStringToStrings(NewPropValue, TStrings(Instance));
  end
  else if Instance is TCollection then
  begin
    SaveStack := FTransStack;
    try
      for I := 0 to TCollection(Instance).Count - 1 do
      begin
        FTransStack := SaveStack + DefDelimeter + IntToStr(I);
        TranslateProps(TCollection(Instance).Items[I]);
      end;
    finally
      FTransStack := SaveStack;
    end;
  end;

  if Assigned(FOnTranslatePropInstance) then
    FOnTranslatePropInstance(Self, Instance);
end;

function TRxCustomTranslator.TranslatePath: string;
begin
  Result := FTransStack;
end;

function TRxCustomTranslator.TranslatePathProp(const PropName: string): string;
begin
  if PropName <> '' then
    Result := TranslatePath + DefDelimeter + PropName
  else
    Result := TranslatePath;
end;

function TRxCustomTranslator.TranslateProp(const PropPath, PropValue: string): string;
begin
  SetActive(True);
  Result := DoTranslateProp(PropPath, PropValue);
end;

procedure TRxCustomTranslator.TranslatePropClass(Instance: TObject; const PropName: string);
var
  PropValue: TObject;
  SaveStack: string;
begin
  PropValue := TObject(GetOrdProp(Instance, GetPropInfo(Instance.ClassInfo, PropName)));
  if Assigned(PropValue) and not (PropValue is TComponent) then
  begin
    SaveStack := FTransStack;
    FTransStack := FTransStack + DefDelimeter + PropName;
    try
      TranslateProps(PropValue);
    finally
      FTransStack := SaveStack;
    end;
  end;
end;

procedure TRxCustomTranslator.TranslatePropString(Instance: TObject; const PropName: string);
var
  OldPropValue, NewPropValue: string;
begin
  if (AnsiCompareText(PropName, 'Name') = 0) then Exit;
  OldPropValue := GetStrProp(Instance, GetPropInfo(Instance.ClassInfo, PropName));
  NewPropValue := OldPropValue;
  DoTranslatePropString(Instance, PropName, OldPropValue, NewPropValue);
  if OldPropValue <> NewPropValue then
    SetStrProp(Instance, GetPropInfo(Instance.ClassInfo, PropName), NewPropValue);
end;

procedure DoTranslate(Instance: TComponent; Data: Pointer);
begin
  if Instance.Name <> '' then
    TRxCustomTranslator(Data).TranslateProps(Instance);
end;

procedure TRxCustomTranslator.TranslateProps(Instance: TObject);

{$IFNDEF UNICODE}
function FindInteger(Value: Integer; const Buff; Count: Integer): Integer; assembler;
  asm
          XCHG    EDI,EDX
          PUSH    ECX
          REPNE   SCASD
          MOV     EDI,EDX
          POP     EAX
          JE      @@1
          XOR     EAX,EAX
    @@1:  SUB     EAX,ECX
          DEC     EAX
          MOV     EDI,EDX
  end;

  function ListIndexOf(List: TList; Item: Pointer): Integer;
  begin
    Result := -1;
    if Assigned(List) then
      with List do
        Result := FindInteger(Integer(Item), List{$IFNDEF RX_D16}^{$ENDIF}, Count)
  end;

  function ListAdd(var List: TList; Item: Pointer): Pointer;
  begin
    if List = nil then
      List := TList.Create;
    List.Add(Item);
    Result := Item;
  end;

  procedure FreeObject(var Obj); assembler;
  asm
          MOV     ECX, [EAX]
          TEST    ECX, ECX
          JE      @@exit
          PUSH    EAX
          MOV     EAX, ECX
          MOV     ECX, [EAX]
          MOV     DL,1
          CALL    dword ptr [ECX - 4] { vtDestroy }
          POP     EAX
          XOR     ECX, ECX
          MOV     [EAX], ECX
  @@exit:
  end;
  {$ENDIF}

  procedure GetPropInfoList(List: TList; Instance: TObject; Filter: TTypeKinds);
  var
    Count: Integer;
  begin
    List.Clear;
    try
      Count := GetPropList(Instance.ClassInfo, Filter, nil);
      List.Count := Count;
      GetPropList(Instance.ClassInfo, Filter, PPropList(List.List));
    except
      List.Clear;
      raise;
    end;
  end;

  procedure ForEachComponent(Instance: TComponent;
    ComponentClass: TComponentClass; Callback: TComponentCallback;
    Data: Pointer; Children: Boolean);
  var
    I: Integer;
    C: TComponent;
  begin
    for I := 0 to Instance.ComponentCount - 1 do
    begin
      C := Instance.Components[I];
      if C is ComponentClass then
        Callback(C, Data);
      if Children then
        ForEachComponent(C, ComponentClass, Callback, Data, Children);
    end;
  end;

var
  I: Integer;
  StackDelta: string;
  List: TList;
  First: Boolean;
begin
  try
    Active := True;

    { Check for possible cross-references }
    First := not Assigned(FTransList);
    if First then FRoot := Instance;
    {$IFDEF UNICODE}
    if Assigned(FTransList) then
      if (FTransList.IndexOf(Instance) >= 0) or (Instance is TComponent)
        and (Instance <> FRoot)
        and (FTransList.IndexOf(TComponent(Instance).Owner) < 0) then Exit;

    if not Assigned(FTransList) then FTransList := TList < Pointer > .Create;
    FTransList.Add(Instance);
    {$ELSE}
    if FTransList <> nil then
      if (ListIndexOf(FTransList, Instance) >= 0) or (Instance is TComponent)
        and (Instance <> FRoot)
        and (ListIndexOf(FTransList, TComponent(Instance).Owner) < 0) then Exit;

    ListAdd(FTransList, Instance);
    {$ENDIF}
    try
      StackDelta := '';
      if Instance is TComponent then
        StackDelta := TComponent(Instance).Name;

      if StackDelta <> '' then
      begin
        if FTransStack <> '' then FTransStack := FTransStack + DefDelimeter;
        FTransStack := FTransStack + StackDelta;
      end;

      List := TList.Create;
      try
        DoTranslatePropInstance(Instance);

        { String properties }
        GetPropInfoList(List, Instance, StringProps);
        for I := 0 to List.Count - 1 do
          TranslatePropString(Instance, string(PPropInfo(List[I]).Name));

        { Class properties }
        GetPropInfoList(List, Instance, ClassProps);
        for I := 0 to List.Count - 1 do
          TranslatePropClass(Instance, string(PPropInfo(List[I]).Name));

        { Nested components }
        if Instance is TComponent then
          ForEachComponent(TComponent(Instance), TComponent, @DoTranslate, Self, False);
      finally
        List.Free;
      end;
    finally
      if First then
      begin
        {$IFDEF UNICODE}
        FreeAndNil(FTransList);
        {$ELSE}
        FreeObject(FTransList);
        FTransList := nil;
        {$ENDIF}
        FRoot := nil;
        FTransStack := '';
      end
      else if StackDelta <> '' then
        FTransStack := Copy(FTransStack, 1, Length(FTransStack) - Length(StackDelta) - 1);
    end;
  except
    //sillent exception for non owned components
  end;
end;

function TRxCustomTranslator.TranslateMessage(const Msg: string): string;
begin
  SetActive(True);
  Result := DoTranslateMsg(Msg)
end;

function TRxCustomTranslator.TranslateUserMessage(const Msg: string): string;
begin
  Result := TranslateMessage(Msg);
end;

function TRxCustomTranslator.TMsg(const Msg: string): string;
begin
  Result := TranslateMessage(Msg);
end;

procedure TRxCustomTranslator.Translate;
begin
  if Assigned(Owner) then TranslateProps(Owner);
end;

{ TRxTranslator }

constructor TRxTranslator.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF RX_D4}
  FIniFileLink := TIniLink.Create;
  FIniFileLink.OnIniLinkDestroy := IniDestroy;
  {$ENDIF}
end;

destructor TRxTranslator.Destroy;
begin
  {$IFDEF RX_D4}
  FIniFileLink.Free;
  {$ENDIF}
  inherited;
end;

{$IFDEF RX_D4}

function TRxTranslator.GetFormPlacement: TFormPlacement;
begin
  Result := FIniFileLink.Storage;
end;

procedure TRxTranslator.SetFormPlacement(Value: TFormPlacement);
var
  SaveActive: Boolean;
begin
  if FormPlacement <> Value then
  begin
    SaveActive := Active;
    Active := False;
    SetIniFileName('');
    FIniFileLink.Storage := Value;
    if Assigned(FormPlacement) then
      Active := SaveActive;
  end;
end;

procedure TRxTranslator.IniDestroy(Sender: TObject);
begin
  Active := False;
end;
{$ENDIF}

procedure TRxTranslator.DoActivate;
{$IFDEF UNICODE}
  function UTF8FileBOM(const FileName: string): boolean;
  var
    txt: file;
    bytes: array[0..2] of byte;
    amt: integer;
  begin
    FileMode := fmOpenRead;
    AssignFile(txt, FileName);
    Reset(txt, 1);
    try
      BlockRead(txt, bytes, 3, amt);
      result := (amt=3) and (bytes[0] = $EF) and (bytes[1] = $BB) and (bytes[2] = $BF);
    finally
      CloseFile(txt);
    end;
  end;
{$ENDIF}
begin
  {$IFDEF RX_D4}
  {exists form placement?}
  if Assigned(FormPlacement) then
  begin
    {is readable?}
    if not FInCreateFile then
    begin
      {lang file will be from placement file}
      FLanguageFile := FormPlacement.IniFile;
      FFormPlacementFile := True;
      Exit;
    end;
  end
  else
  {$ENDIF}
  begin
    {$IFDEF UNICODE}
    if UTF8FileBOM(FLanguageFileName) then
      FLanguageFile := TMemIniFile.Create(FLanguageFileName, TEncoding.UTF8)
    else
      FLanguageFile := TMemIniFile.Create(FLanguageFileName);
    {$ELSE}
    FLanguageFile := TIniFile.Create(FLanguageFileName);
    {$ENDIF} {open extra file}
  end;
end;

procedure TRxTranslator.DoDeactivate;
begin
  {$IFDEF RX_D4}
  if FFormPlacementFile then
  begin
    FFormPlacementFile := False;
    FLanguageFile := nil;
  end
  else
    {$ENDIF}
  begin
    FLanguageFile.Free;
    FLanguageFile := nil;
  end;
end;

function TRxTranslator.DoTranslateMsg(const Msg: string): string;
begin
  Result := Msg;
  if Assigned(OnTranslateMsg) then
    OnTranslateMsg(Self, Msg, Result)
  else
    Result := FLanguageFile.ReadString(SectionMessages, Msg, Result);
end;

function TRxTranslator.DoTranslateProp(const PropPath, PropValue: string): string;
begin
  Result := PropValue;
  if FInCreateFile then
  begin
    if PropValue <> '' then
      FLanguageFile.WriteString(SectionTranslate, PropPath, PropValue)
  end
  else if Assigned(OnTranslateProp) then
    OnTranslateProp(Self, PropPath, PropValue, Result)
  else
    Result := FLanguageFile.ReadString(SectionTranslate, PropPath, Result);
end;

function TRxTranslator.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TRxTranslator.SetActive(Value: Boolean);
begin
  inherited SetActive(Value);
end;

procedure TRxTranslator.SetIniFileName(const Value: TFileName);
var
  SaveActive: Boolean;
begin
  if csLoading in ComponentState then
    FLanguageFileName := Value
  else if FLanguageFileName <> Value then
  begin
    SaveActive := Active;
    Active := False;
    FLanguageFileName := Value;
    {$IFDEF RX_D4}
    SetFormPlacement(nil);
    {$ENDIF}
    if FLanguageFileName <> '' then Active := SaveActive;
  end;
end;

procedure TRxTranslator.CreateLanguageFile(const FileName: TFileName; SaveOld: Boolean);
var
  SaveActive: Boolean;
  SaveFileName: TFileName;
  bak: string;
begin
  if SaveOld then
  begin
    if FileExists(FileName) then
    begin
      bak := ChangeFileExt(FileName, '.bak');
      if FileExists(bak) then
        DeleteFile(bak);
      RenameFile(FileName, bak);
    end
  end
  else
    DeleteFile(FileName);

  SaveActive := Active;
  try
    Active := False;

    try
      SaveFileName := LanguageFileName;
      LanguageFileName := FileName;
      FInCreateFile := True;
      try
        Translate;{$IFDEF UNICODE}FLanguageFile.UpdateFile;{$ENDIF}
      finally
        FInCreateFile := False;
      end;
    finally
      LanguageFileName := SaveFileName;
    end;
  finally
    Active := SaveActive;
  end;
end;

end.
