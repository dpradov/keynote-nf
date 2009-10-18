{***************************************************************
 *
 * Unit Name: AJBSpeller
 * Purpose  : Interface with Word spellcheck
 * Author   : Andrew Baylis
 * Date     : 4/07/2001
 * History  : Version 1 - bug in Word speller when options clicked
              Version 2 - Uses Filemapping to provide shared memory
                          This, in conjunction with a message hook
                          allows the pop-up spell check dialog to be
                          detected and the caption changed.
 *
 ****************************************************************}

unit AJBSpeller;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComObj, ActiveX, registry;

type
  TAJBSpell = class(TComponent)
  private
    FChangedText: string;
    FConnected: Boolean;
    FHandle: HWND;
    FHookCaption: Boolean;
    FNumChanges: Integer;
    FOleOn: Boolean;
    FSpellCaption: string;
    FWordApp, FRange, FADoc, FCustDics: OLEVariant;
    FWordVersion: string;
    hMapObject: Cardinal;
    pMem: Pointer;
    function GetCheckGWS: Boolean;
    function GetGrammarErrors: Integer;
    function GetSpellCaption: string;
    function GetSpellChecked: Boolean;
    function GetSpellErrors: Integer;
    function GetVersion: string;
    procedure SetVersion(const Value:string);
    function GetWordVersion: string;
    procedure SetCheckGWS(const Value: Boolean);
    procedure SetHookCaption(Value: Boolean);
    procedure SetSpellCaption(const Value: string);
  protected
    function Internal_checkGrammar: Boolean;
    function Internal_checkSpelling: Boolean;
    procedure SetHook;
    procedure UnSetHook;
  public
    function AddCustomDic(const FileName: string): Integer;
    function CheckClipboardGrammar: Boolean;
    function CheckClipboardSpell: Boolean;
    function CheckGrammar(const Text: string): Boolean;
    function CheckSpelling(const Text: string): Boolean;
    procedure ClearText;
    procedure Connect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Disconnect;
    procedure RemoveCustomDic(const Index: Integer); 
    procedure RemoveCustomDicName(const Name: string);
    procedure ResetIgnoreAll;
    procedure SpellingOptions;
    property ChangedText: string read FChangedText;
    property CheckGrammarWithSpelling: Boolean read GetCheckGWS write SetCheckGWS;
    property Connected: Boolean read FConnected;
    property GrammarErrorCount: Integer read GetGrammarErrors;
    property NumChanges: Integer read FNumChanges;
    property SpellChecked: Boolean read GetSpellChecked;
    property SpellErrorCount: Integer read GetSpellErrors;
    property WordVersion: string read GetWordVersion;
  published
    property HookCaption: Boolean read FHookCaption write SetHookCaption;
    property SpellCaption: string read GetSpellCaption write SetSpellCaption;
    property Version: string read GetVersion Write SetVersion;
  end;

  function IsWordPresent:Boolean;

implementation

// these constants used to implement the shared memory space
// using a FileMapping to hold two strings (up to 255 char each)
const
  AJBShareSize = 512;
  AJBCaptionStart = 0;
  AJBClassStart = 256;
  //memspace looks like this: |..Caption...|...WindowsClassID..|

  //Constants for MS Word
  MSDialogWndClass2000 = 'bosa_sdm_Microsoft Word 9.0';
  MSDialogWndClass97 = 'bosa_sdm_Microsoft Word 8.0';
  MSWordWndClass = 'OpusApp';

  //Component Version
  AJBSpellerVersion = 'Ver 2.0';

type
    pTCWPRetStruct = ^TCWPRetStruct;

var
  nHook: HHOOK;

function IsWordPresent:Boolean;
var reg: TRegistry;
begin
     reg:=TRegistry.Create;
     try
        reg.RootKey:=HKEY_CLASSES_ROOT;
        Result:=reg.KeyExists('Word.Application');
     finally
     reg.Free;
     end;
end;


function MessWatch(nCode, wParam, lParam: Integer): Integer; stdcall;
var
  p: pTCWPRetStruct;
  h: HWND;
  pMem: Pointer;
  q: PChar;
  hMapObject: Cardinal;

begin
  Result := 0;
  if nCode < 0 then
    Result := CallNextHookEx(nHook, nCode, wParam, lParam)
  else
  begin
    p := pTCWPRetStruct(lParam);
    if (p.message = WM_NCPAINT) then begin //wait for NC area to be drawn
      //open shared memory
      hMapObject := OpenFileMapping(FILE_MAP_READ, False, 'ajb_spell_share');
      pMem := MapViewOfFile(hMapObject, FILE_MAP_READ, 0, 0, 0);
      if (pMem <> nil) then begin
        q := pMem;
        h := FindWindow(q + AJBClassStart, nil);
        if (h <> 0) then
          SetWindowText(h, q + AJBCaptionStart);
      end;
      CloseHandle(hMapObject); //close shared memory
    end;
  end;
end;

{ TAJBSpeller }

function TAJBSpell.GetCheckGWS: Boolean;
begin
  Result := False;
  if FConnected then Result := FWordApp.Options.CheckGrammarWithSpelling;
end;

function TAJBSpell.GetGrammarErrors: Integer;
begin
  if FConnected then
    Result := FRange.GrammaticalErrors.Count
  else
    Result := 0;
end;

function TAJBSpell.GetSpellCaption: string;
begin
  Result := FSpellCaption;
end;

function TAJBSpell.GetSpellChecked: Boolean;
// returns false if spelling has yet to be checked
begin
  Result := True;
  if FConnected then Result := not FRange.SpellingChecked;
end;

function TAJBSpell.GetSpellErrors: Integer;
begin
  if FConnected then
    Result := FRange.SpellingErrors.Count
  else
    Result := 0;
end;

function TAJBSpell.GetVersion: string;
begin
  Result := AJBSpellerVersion;
end;

function TAJBSpell.GetWordVersion: string;
begin
  Result := FWordVersion;
end;

procedure TAJBSpell.SetCheckGWS(const Value: Boolean);
begin
  if FConnected then FWordApp.Options.CheckGrammarWithSpelling := Value;
end;

procedure TAJBSpell.SetHookCaption(Value: Boolean);
begin
  FHookCaption := Value;
  if not (csDesigning in ComponentState) then begin
    if Value then SetHook
    else UnSetHook;
  end;
end;

procedure TAJBSpell.SetSpellCaption(const Value: string);
var
  p: PChar;
begin
  FSpellCaption := Value;
  if FConnected then begin
     // copy to shared memory
    p := pMem;
    if (p <> nil) then
      StrCopy(p + AJBCaptionStart, PChar(FSpellCaption));
  end;
end;

function TAJBSpell.Internal_checkGrammar: Boolean;
begin
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOACTIVATE + SWP_HIDEWINDOW); // ensures dialogs appear in front
  FADoc.TrackRevisions := True; // note if changes are made
  FNumChanges := 0;
  OleCheck(FRange.CheckGrammar);
  FWordApp.Visible := False; // need to stop ActiveDocument appearing
  FNumChanges := FRange.Revisions.Count div 2; // seems revisions counts the old word and the new one separately
  Result := (FRange.Revisions.Count > 0);
  if Result then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions := False; // don't track future changes
end;

function TAJBSpell.Internal_checkSpelling: Boolean;
begin
  SetWindowPos(FHandle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NOACTIVATE + SWP_HIDEWINDOW); // ensures dialogs appear in front }

  FADoc.TrackRevisions := True; // note if changes are made
  FNumChanges := 0;

  // OleCheck(FADoc.CheckGrammar);
  OleCheck(FADoc.CheckSpelling); // [mj] replaced the above line

  FWordApp.Visible := False; // need to stop ActiveDocument appearing
  FNumChanges := FRange.Revisions.Count div 2; // seems revisions counts the old word and the new one separately
  Result := (FRange.Revisions.Count > 0);
  if Result then FRange.Revisions.AcceptAll; // accept all changes
  FADoc.TrackRevisions := False; // don't track future changes
end;

procedure TAJBSpell.SetHook;
begin
  if (nHook = 0) then
    nHook := SetWindowsHookEx(WH_CALLWNDPROCRET, MessWatch, HInstance, 0);
end;

procedure TAJBSpell.UnSetHook;
begin
  if (nHook <> 0) then UnHookWindowsHookEx(nHook);
  nHook := 0;
end;

function TAJBSpell.AddCustomDic(const FileName: string): Integer;
begin
  FCustDics.Add(FileName);
  Result := FCustDics.Count;
end;

function TAJBSpell.CheckClipboardGrammar: Boolean;
// returns true if changes were made. Corrected text is on
// the clipboard
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!

  if FConnected then
  begin
    FRange.Paste; // replace with new text to check
    Result := Internal_CheckGrammar;
    if Result then FRange.Copy;
  end;
end;

function TAJBSpell.CheckClipboardSpell: Boolean;
// returns true if changes were made. Corrected text is on
// the clipboard
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FRange.Paste; // replace with new text to check
    Result := Internal_checkSpelling;
    if Result then FRange.Copy; // put onto clipboard
  end;
end;

function TAJBSpell.CheckGrammar(const Text: string): Boolean;
// returns true if changes were made and the corrected text is
// placed in the Text string
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FChangedText := '';
    FRange.Text := Text; // replace with new text to check
    Result := Internal_CheckGrammar;
    if Result then FChangedText := FRange.Text;
  end;
end;

function TAJBSpell.CheckSpelling(const Text: string): Boolean;
// returns true if changes were made and the corrected text is
// placed in the Text string
begin
  Result := False;
  if not FConnected then Connect;
  if not FConnected then Exit; // if still not connected then no MS Word!
  if FConnected then
  begin
    FChangedText := '';
    FRange.Text := Text; // replace with new text to check
    Result := Internal_CheckSpelling;
    if Result then FChangedText := FRange.Text;
  end
  else
    Result := False;
end;

procedure TAJBSpell.ClearText;
begin
  if FConnected then FRange.Text := '';
end;

procedure TAJBSpell.Connect;
var
  s: string;
  p: PChar;
  // EmptyParam : OleVariant;
  prog : integer;
begin
  prog := 0;
  if FConnected then Exit; // don't create two instances
  try
    FWordApp := CreateOleObject('Word.Application');
    prog := 1;
    FConnected := True;
    prog := 2;
    FWordApp.Visible := False; // hides the application
    prog := 3;
    FWordApp.ScreenUpdating := False; // speed up winword's processing
    prog := 4;
    FWordApp.WindowState := $00000002; // minimise
    prog := 5;
    FADoc := FWordApp.Documents.Add; // [mj] ( EmptyParam, EmptyParam, EmptyParam, False); // this will hold the text to be checked
    prog := 6;
    FRange := FADoc.Range;
    prog := 7;
    FRange.WholeStory; // makes FRange point to all text in document
    prog := 8;
    FCustDics := FWordApp.CustomDictionaries;
    prog := 9;
    FWordVersion := FWordApp.Version;
    prog := 10;
    s := FADoc.Name + ' - ' + FWordApp.Name;
    prog := 11;
    FHandle := FindWindow(MSWordWndClass, PChar(s)); // winword
    prog := 12;
    if FWordVersion[1] = '9' then
      s := MSDialogWndClass2000
    else
      s := MSDialogWndClass97;
    prog := 13;
    //set up shared memory space
    hMapObject := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, AJBShareSize,
      'ajb_spell_share');
    prog := 14;
    pMem := MapViewOfFile(hMapObject, FILE_MAP_WRITE, 0, 0, 0);
    prog := 15;
    if pMem <> nil then begin
      FillChar(pMem^, AJBShareSize, 0);
      prog := 16;
      p := pMem;
      StrCopy(p + AJBClassStart, PChar(s));
      StrCopy(p + AJBCaptionStart, PChar(FSpellCaption));
    end;
    //memory share set up

  except
    On E : Exception do
    begin
      FWordApp := Unassigned;
      FConnected := False;
      MessageDlg(Format(
        'Unable to initialise MS Word (%d): %s',
        [prog, E.Message] ), mtError, [mbOK], 0);
    end;
  end;
end;

constructor TAJBSpell.Create(AOwner: TComponent);
var
  init: Integer;
begin
  inherited;
  FConnected := False;
  FChangedText := '';
  init := CoInitialize(nil);
  FHookCaption := False;
  if (init = S_OK) or (init = S_FALSE) then FOleOn := True
  else raise EOleSysError.Create('Error initialising COM library', 0, 0 );
end;

destructor TAJBSpell.Destroy;
begin
  Disconnect;
  UnSetHook;
  if FOleOn then CoUninitialize;
  inherited;
end;

procedure TAJBSpell.Disconnect;
var
  savechanges: OleVariant;

begin
  if not VarIsEmpty(FWordApp) then
  begin
    savechanges := False;
    FWordApp.Quit(savechanges); // don't save changes
    FRange := Unassigned;
    FADoc := Unassigned;
    FWordApp := Unassigned;
    FCustDics := Unassigned;
    FConnected := False;
    CloseHandle(hMapObject);
  end;
end;

procedure TAJBSpell.RemoveCustomDic(const Index: Integer);
var
  dic: OleVariant;
begin
  dic := FCustDics.Item(Index);
  if not VarIsEmpty(dic) then
    dic.Delete;
  dic := Unassigned;
end;

procedure TAJBSpell.RemoveCustomDicName(const Name: string);
var
  dic: OleVariant;
begin
  dic := FCustDics.Item(Name);
  if not VarIsEmpty(dic) then
    dic.Delete;
  dic := Unassigned;
end;

procedure TAJBSpell.ResetIgnoreAll;
begin
  if FConnected then
  begin
    FRange.Text := ''; // ResetIgnoreAll performs an automatic spell check
    FWordApp.ResetIgnoreAll;
  end;
end;

procedure TAJBSpell.SpellingOptions;
begin
  BringWindowToTop(FHandle); // ensures that dialog opens on top
  FWordApp.dialogs.item($000000D3).show;
  FWordApp.Visible := False;
end;

procedure TAJBSpell.SetVersion(const Value:string);
begin
     //FVersion:=AJBSpellerVersion;
end;

end.

