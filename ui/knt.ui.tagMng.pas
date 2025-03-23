unit knt.ui.tagMng;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls,

  knt.ui.tagSelector,
  knt.model.note
  ;



type
  TTagsMode = (tmEdit, tmSearch);

  TOnEndEditTagsIntrod = procedure(PressedReturn: boolean) of object;
  TOnEndFindTagsIntrod = procedure(PressedReturn: boolean; FindTags: TFindTags) of object;
  TOnChangeFindTagsIntrod = procedure(FindTags: TFindTags) of object;


  TTagMng = class
  private
    txtTags: TEdit;
    FTagsMode: TTagsMode;
    FOnEndEditTagsIntrod: TOnEndEditTagsIntrod;
    FOnEndFindTagsIntrod: TOnEndFindTagsIntrod;
    FOnChangeFindTagsIntrod: TOnChangeFindTagsIntrod;
    FOldTxtTagsWndProc: TWndMethod;

    FNote: TNote;
    FFolder: TObject;

    FCanvasAux : TControlCanvas;

    fChangingInCode: boolean;
    fFindTagsInformed: string;

    constructor Create;

  protected
    procedure txtTagsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure txtTagsChange(Sender: TObject);
    procedure txtTagsExit(Sender: TObject);
    procedure txtTagsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtTagsKeyPress(Sender: TObject; var Key: Char);
    procedure NewTxtTagsWndProc(var Msg: TMessage);

    procedure CheckBeginOfTag;
    function CommitIntroducedTags(Ending: boolean): TFindTags;
    function GetCaretPosTag: integer;
    function GetEndOfWord: integer;

  protected
    procedure StartTxtTagIntrod(TagEdit: TEdit);

  public
    destructor Destroy; override;

    procedure StartTxtEditTagIntrod(TagEdit: TEdit; OnEndEditTagsIntrod: TOnEndEditTagsIntrod; Note: TNote; Folder: TObject);
    procedure StartTxtFindTagIntrod(TagEdit: TEdit; OnEndFindTagsIntrod: TOnEndFindTagsIntrod; OnChangeFindTagsIntrod: TOnChangeFindTagsIntrod);
    procedure EndedTxtTagIntrod(PressedReturn: boolean);
    procedure UpdateTxtTagsHint(TagEdit: TEdit = nil);
    procedure UpdateTxtFindTagsHint(txtEdit: TEdit; const ConsideredWords: string; FindTags: TFindTags);

    procedure CreateTagSelector;
    procedure UpdateTagSelector;
    procedure GetTagsMaxWidth(NTags: TNoteTagList; var MaxNameWidth: integer; var MaxDescWidth: integer);
    procedure CheckEndTagIntroduction(OnTypedHash: boolean= false);
    procedure EndTagIntroduction;
    function IsValidTagName (var Name: string): boolean;
    function GetTagSubstr(Txt: string): string;

    function GetTextWidth(Str: string; txtEdit: TEdit): integer;
  end;


var
  TagMng: TTagMng;



implementation

uses VirtualTrees,
     gf_miscvcl,
     kn_KntFolder,
     knt.ui.editor,
     kn_KntFile,
     kn_const,
     knt.App,
     knt.RS
     ;



// Common methods =========================================

{$REGION Common methods }

function TTagMng.GetTextWidth(Str: string; txtEdit: TEdit): integer;
begin
   if txtEdit = nil then exit;
   if FCanvasAux.Control <> txtEdit then begin
      FCanvasAux.Control := txtEdit;
      FCanvasAux.Font := txtEdit.Font;
   end;
   Result:= FCanvasAux.TextWidth(Str);
end;


function TTagMng.GetTagSubstr(Txt: string): string;
var
   i: integer;
begin
   Result:= '';
   for i:= 2 to Length(Txt) do begin
      if Txt[i] in TagCharsDelimiters then begin
         Result:= Copy(Txt, 2, i-2);
         exit;
      end;
   end;
end;


procedure TTagMng.CreateTagSelector;
begin
  if cTagSelector <> nil then exit;

  cTagSelector:= TTagSelector.Create(Form_Main);
end;


procedure TTagMng.UpdateTagSelector;
var
  MaxNameWidth, MaxDescWidth: integer;
  Node: PVirtualNode;
begin
   if (IgnoreSelectorForTagSubsr <> '') and (AnsiCompareText(TagSubstr, IgnoreSelectorForTagSubsr) = 0) then begin
      cTagSelector.CloseTagSelector(false);
      exit;
   end;

   IgnoreSelectorForTagSubsr:= '';
   if TagSubstr <> '' then
      ActiveFile.UpdateNTagsMatching(TagSubstr, PotentialNTags);

   if (TagSubstr <> '') and (PotentialNTags.Count > 0) then begin
      GetTagsMaxWidth(PotentialNTags, MaxNameWidth, MaxDescWidth);
      cTagSelector.ShowTagSelector(MaxNameWidth, MaxDescWidth, Form_Main.Handle);
      with cTagSelector do begin
         TV.RootNodeCount := PotentialNTags.Count;
         Node:= TV.GetFirst;
         TV.FocusedNode:= Node;
         TV.ClearSelection;
         TV.Selected[Node] := True;
         TV.Invalidate;
      end;
   end
   else
     cTagSelector.CloseTagSelector(false);
end;


procedure TTagMng.CheckEndTagIntroduction(OnTypedHash: boolean= false);
var
   SS, EndRange: integer;
   Editor: TKntRichEdit;
   CEdit: TEdit;
   TxtFromCaretPos: string;
   Txt: string;
   PosTag: integer;     // including initial #

begin
   Editor:= nil;
   if cTagSelector.EditorCtrlUI is TKntRichEdit then begin
      FTagsMode := tmEdit;
      Editor:= TKntRichEdit(cTagSelector.EditorCtrlUI);
      SS:= Editor.SelStart;
      if (IntroducingTagsState = itHashTyped) and (SS = CaretPosTag) or
         (SS = CaretPosTag-1) and (Editor.GetTextRange(SS, SS+1) = '#') then
         exit;
      TxtFromCaretPos:= Editor.GetTextRange(CaretPosTag-1, CaretPosTag + TAG_MAX_LENGTH);
      PosTag:= CaretPosTag;
   end
   else begin
      CEdit:= TEdit(cTagSelector.EditorCtrlUI);
      SS:= CEdit.SelStart;
      TxtFromCaretPos:= Copy(CEdit.Text, CaretPosTag);
      if (SS = CaretPosTag-1) and (TxtFromCaretPos <> '') then
         exit;
      if (TxtFromCaretPos <> '') and not (TxtFromCaretPos[1] in ['#',' ']) then
         TxtFromCaretPos:= '#' + TxtFromCaretPos;
      PosTag:= CaretPosTag-1;
   end;

   if (TxtFromCaretPos = '') or (TxtFromCaretPos[1] <> '#') then begin
      cTagSelector.CloseTagSelector(true);
      exit;
   end;

   if (cTagSelector.Active or cTagSelector.EditorCtrlUI.Focused) and (SS >= CaretPosTag) and (SS <= CaretPosTag + TAG_MAX_LENGTH) then
      Txt:= Copy(TxtFromCaretPos, 1, SS-PosTag+1)
   else
      Txt:= TxtFromCaretPos;

   if (Txt <> '') then begin
      if (SS < CaretPosTag) or OnTypedHash then
         Txt:= Txt + ' ';    // If the label is written right at the end and the cursor is moved to the left of # -> we will add it
      Txt:= GetTagSubstr(Txt);
      if not ( (Txt = '') and (SS < CaretPosTag) ) then begin
         if (Txt <> '') then begin
            TagSubstr:= Txt;
            EndTagIntroduction;
         end
         else begin
            Txt:= TxtFromCaretPos + ' ';
            Txt:= GetTagSubstr(Txt);
            TagSubstr:= Txt;
            UpdateTagSelector;
         end;
         exit;
      end;
   end;

   cTagSelector.CloseTagSelector(true);
end;


procedure TTagMng.EndTagIntroduction;
var
  NTag: TNoteTag;
begin
   cTagSelector.CloseTagSelector (true);
   if (TagSubstr <> '') and (FTagsMode = tmEdit) then begin
      if TagSubstr[Length(TagSubstr)] = '.' then
         delete(TagSubstr, Length(TagSubstr), 1);

      if ( AnsiCompareText( TagSubstr, IgnoreTagSubstr ) <> 0 ) then begin
         NTag:= ActiveFile.AddNTag(TagSubstr, '');
         if NTag <> nil then
            ActiveFile.NoteTagsTemporalAdded.Add(NTag)
         else begin
            NTag:= ActiveFile.GetNTagByName(TagSubstr, ActiveFile.NoteTagsTemporalAdded);
            if NTag <> nil then begin
               NTag.Name:= TagSubstr;      // If you are adding it temporarily and you change its spelling, we assume that you want to correct it. Eg: "TOdo" and then "ToDO"
               if App.TagsState = tsVisible then
                  Form_Main.TVTags.Invalidate;
            end;
         end;
      end;
   end
   else begin
      if (FTagsMode = tmSearch) and assigned(FOnChangeFindTagsIntrod) then begin
         fFindTagsInformed:= txtTags.Text;
         FOnChangeFindTagsIntrod(CommitIntroducedTags(false));
      end;
   end;
   TagSubstr:= '';
   IgnoreTagSubstr:= '';
   IgnoreSelectorForTagSubsr:= '';
end;


function TTagMng.IsValidTagName (var Name: string): boolean;
var
   i: integer;
begin
   if Name = '' then exit(False);

   Result:= True;
   for i:= 1 to Length(Name) do begin
      if Name[i] in TagCharsDelimiters then
         exit (False);
   end;

   if Name[Length(Name)] = '.' then
      delete(Name, Length(Name), 1);
end;


procedure TTagMng.GetTagsMaxWidth(NTags: TNoteTagList; var MaxNameWidth: integer; var MaxDescWidth: integer);
var
  i: integer;
  NTag: TNoteTag;
  W, iName, iDesc: integer;
begin
  // -> Approximated widths
  MaxNameWidth:= 0;             // We will initially store the maximum length of the strings, not the width...
  MaxDescWidth:= 0;
  iDesc:= -1;
  if NTags <> nil then begin
     for i := 0 to NTags.Count-1 do begin
        NTag:= NTags[i];
        W:= Length(NTag.Name);
        if W > MaxNameWidth then begin
           MaxNameWidth:= W;
           iName:= i;
        end;

        if NTag.Description <> '' then begin
           W:= Length(NTag.Description);
           if W > MaxDescWidth then begin
              MaxDescWidth:= W;
              iDesc:= i;
           end;
        end;
     end;
  end;
  MaxNameWidth:= cTagSelector.TV.Canvas.TextWidth(NTags[iName].Name) + 20;
  if iDesc >= 0 then
     MaxDescWidth:= cTagSelector.TV.Canvas.TextWidth(NTags[iDesc].Description) + 20;
end;


{$ENDREGION}


// TxtTags methods =========================================

{$REGION TxtTags methods }

constructor  TTagMng.Create;
begin
   inherited Create;

   txtTags:= nil;
   FOnEndEditTagsIntrod:= nil;
   FOnEndFindTagsIntrod:= nil;
   FOnChangeFindTagsIntrod:= nil;

   FCanvasAux := TControlCanvas.Create;
end;

destructor  TTagMng.Destroy;
begin
   if txtTags <> nil then
      EndedTxtTagIntrod(false);

   if FCanvasAux <> nil then
      FCanvasAux.Free;
   inherited Destroy;
end;


procedure TTagMng.StartTxtTagIntrod(TagEdit: TEdit);
begin
   txtTags:= TagEdit;
   ActiveFile.NoteTagsTemporalAdded.Clear;

   IgnoreTagSubstr:= '';
   if txtTags.Text = EMPTY_TAGS then
      txtTags.Text:= '';
   txtTags.Color:= clWindow;
   txtTags.Font.Color:= clMaroon;
   txtTags.Hint:= '';

   with txtTags do begin
      OnKeyDown:=  txtTagsKeyDown;
      OnKeyUp:=  txtTagsKeyUp;
      OnKeyPress:=  txtTagsKeyPress;
      OnMouseDown:= txtTagsMouseDown;
      OnChange:=  txtTagsChange;
      OnExit:=  txtTagsExit;

      FOldTxtTagsWndProc := txtTags.WindowProc;
      txtTags.WindowProc := NewTxtTagsWndProc;
   end;
end;

procedure TTagMng.StartTxtEditTagIntrod(TagEdit: TEdit; OnEndEditTagsIntrod: TOnEndEditTagsIntrod; Note: TNote; Folder: TObject);
begin
   if TagEdit = txtTags then exit;
   StartTxtTagIntrod(TagEdit);

   FTagsMode:= tmEdit;
   FOnEndEditTagsIntrod:= OnEndEditTagsIntrod;
   FNote:= Note;
   FFolder:= Folder;
end;


procedure TTagMng.StartTxtFindTagIntrod(TagEdit: TEdit; OnEndFindTagsIntrod: TOnEndFindTagsIntrod; OnChangeFindTagsIntrod: TOnChangeFindTagsIntrod);
begin
   if TagEdit = txtTags then exit;

   StartTxtTagIntrod(TagEdit);

   FTagsMode:= tmSearch;
   FOnEndFindTagsIntrod:= OnEndFindTagsIntrod;
   FOnChangeFindTagsIntrod:= OnChangeFindTagsIntrod;
end;



procedure TTagMng.EndedTxtTagIntrod(PressedReturn: boolean);
var
  Color: TColor;
  FindTags: TFindTags;

begin
   if txtTags <> nil then begin
      txtTags.SelStart:= 0;
      CheckBeginOfTag;

      cTagSelector.CloseTagSelector(true);

      FindTags:= CommitIntroducedTags(true);

      if FTagsMode = tmEdit then
         UpdateTxtTagsHint;

      Color:= clWindowText;
      if txtTags.Text = '' then begin
         fChangingInCode:= True;
         txtTags.Text := EMPTY_TAGS;
         fChangingInCode:= False;
         Color:= clGray;
      end;
      txtTags.Font.Color:= Color;

      with txtTags do begin
         OnKeyDown:=  nil;
         OnKeyUp:=  nil;
         OnKeyPress:=  nil;
         OnMouseDown:= nil;
         OnChange:=  nil;
         OnExit:=  nil;
         txtTags.WindowProc := FOldTxtTagsWndProc;
      end;

      if FTagsMode = tmEdit then begin
         if assigned(FOnEndEditTagsIntrod) then
             FOnEndEditTagsIntrod(PressedReturn);
      end
      else begin
         if assigned(FOnEndFindTagsIntrod) then
            FOnEndFindTagsIntrod(PressedReturn, FindTags);
      end;

      txtTags:= nil;
      FOnEndEditTagsIntrod:= nil;
      FOnEndFindTagsIntrod:= nil;
      FOnChangeFindTagsIntrod:= nil;
      FOldTxtTagsWndProc:= nil;
      fFindTagsInformed:= '';
   end;

end;


procedure TTagMng.NewTxtTagsWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_GETDLGCODE) then begin
    FOldTxtTagsWndProc(Msg);
    Msg.Result := Msg.Result or DLGC_WANTTAB;
    {
    if FWantsTab then
       Msg.Result := Msg.Result or DLGC_WANTTAB
    else
       Msg.Result := Msg.Result and not DLGC_WANTTAB;
    }
    Exit;
  end;
  FOldTxtTagsWndProc(Msg);
end;



function TTagMng.GetCaretPosTag: integer;
var
   i, SS: integer;
   txt: string;
begin
   Result:= 1;
   if txtTags.GetTextLen = 0 then exit;

   SS:= txtTags.SelStart;
   txt:= txtTags.Text;

   for i:= SS downto 1 do
      if Txt[i] in TagCharsDelimiters then
         break;
   Result:= i+1;
end;

function TTagMng.GetEndOfWord: integer;
var
   i, SS: integer;
   txt: string;
begin
   SS:= txtTags.SelStart;
   txt:= txtTags.Text;

   for i:= SS to txtTags.GetTextLen do
      if Txt[i] in TagCharsDelimiters then
         break;
   Result:= i;
end;


procedure TTagMng.CheckBeginOfTag;
begin
  if (IntroducingTagsState = itNoTags) then begin
     if (FTagsMode = tmSearch) and assigned(FOnChangeFindTagsIntrod) and (Trim(txtTags.Text) <> Trim(fFindTagsInformed)) then begin
         fFindTagsInformed:= txtTags.Text;
         FOnChangeFindTagsIntrod(CommitIntroducedTags(false));
     end;

     CaretPosTag:= GetCaretPosTag;
     cTagSelector.EditorCtrlUI:= txtTags;
     IntroducingTagsState := itWithoutTagSelector;
  end;
  if IntroducingTagsState <> itNoTags then
     CheckEndTagIntroduction;
end;



procedure TTagMng.txtTagsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // The first CheckBeginOfTag could have caused a tag to be added
  // The second tries to check if we are located in another tag, to show the selector
  CheckBeginOfTag;
  CheckBeginOfTag;
end;

procedure TTagMng.txtTagsChange(Sender: TObject);
begin
  if not ActiveFileIsBusy and not fChangingInCode then
     CheckBeginOfTag;
end;


procedure TTagMng.txtTagsExit(Sender: TObject);
begin
   EndedTxtTagIntrod(false);
end;



procedure TTagMng.txtTagsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (IntroducingTagsState = itWithTagSelector) then
     if key in [VK_RETURN, VK_TAB] then
        Key:= 0
     else
     if (key in [VK_UP, VK_DOWN]) then begin
        if (PotentialNTags.Count > 1) then
           cTagSelector.SelectTag(Key)
        else
           cTagSelector.CloseTagSelector(false);

        Key:= 0;
     end;
end;


procedure TTagMng.txtTagsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if key in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_BACK] then begin
      CheckBeginOfTag;
      CheckBeginOfTag;
   end;
end;


procedure TTagMng.txtTagsKeyPress(Sender: TObject; var Key: Char);
var
 txt: string;
 ch: Char;
 StartTag: boolean;
 SS: integer;
begin
  if Key in [',',':'] then Key:= ' ';

  case IntroducingTagsState of
     itNoTags:
         begin
           CaretPosTag:= GetCaretPosTag;
           cTagSelector.EditorCtrlUI:= txtTags;
           IntroducingTagsState := itWithoutTagSelector;
         end;

     itWithTagSelector:
         if (Key in TagCharsDelimiters) or (Key = '.' ) then
         begin
            if txtTags.ReadOnly then exit;

            TagSubstr:= cTagSelector.SelectedTagName;
            fChangingInCode:= true;
            if Key = #9   then Key:= #0
            else
            if Key <> '.' then Key:= ' ';
            SS:= GetEndOfWord;
            txtTags.SelStart:= CaretPosTag-1;
            txtTags.SelLength:= SS - CaretPosTag;
            txtTags.SelText:= TagSubstr;
            if Key <> #0 then
               txtTags.SelText:= Key;
            fChangingInCode:= false;

            case key of
              #9: ;
              '.': begin
                 TagSubstr:= TagSubstr + '.';
                 UpdateTagSelector;
              end;
              else                           // ' ' #13 ':' ',' '#'
                EndTagIntroduction;
            end;

            key:= #0;
            exit;
         end;
  end;
  if (Key in [#13, #9]) and (IntroducingTagsState = itWithoutTagSelector) then
     EndedTxtTagIntrod(true);

end;


function TTagMng.CommitIntroducedTags(Ending: boolean): TFindTags;
var
   i, pI, pF: integer;
   Txt, tagName: string;
   NTag: TNoteTag;
   TagsAssigned: TNoteTagArray;
   TagsNames: array of string;
   N: integer;
   TagsStateBAK: TTagsState;
   NEntry: TNoteEntry;
   FindTags: TFindTags;
   NTagsOR: TNoteTagList;
   ConsideredWords: string;

   function DuplicateTag(aName: string): boolean;
   var
     i: integer;
   begin
      Result:= false;
      for i := 0 to High(TagsNames) do begin
          if ( AnsiCompareText( TagsNames[i], aName ) = 0 ) then
               exit(true);
      end;
   end;

   procedure CheckTag;
   var
      TagsOR: TTagsOR;
      M, i: integer;
      tagNameOR: string;
   begin
      if pI = pF then exit;
      tagName:= Copy(txt, pI, pF-pI+1);
      if Trim(tagName) = '' then exit;
      if tagName[Length(tagName)] = '.' then
         delete(tagName, Length(tagName), 1);
      if DuplicateTag(tagName) then exit;


      if FTagsMode = tmEdit then begin
         inc(N);
         SetLength(TagsAssigned, N);
         SetLength(TagsNames, N);
         TagsNames[N-1]:= tagName;
         NTag:= ActiveFile.GetNTagByName(tagName, ActiveFile.NoteTagsTemporalAdded);
         if NTag <> nil then
            NTag:= nil
         else
            NTag:= ActiveFile.GetNTagByName(tagName);  // If return NTag is nil must have been added by pasting, not typing.
         TagsAssigned[N-1]:= NTag;                     // NTag = nil => New tag that will be created  (if FTagsMode = tmEdit)
      end
      else begin
                                                      // FTagsMode = tmSearch
         TagsOR:= nil;
         NTag:= ActiveFile.GetNTagByName(tagName);
         if NTag <> nil then begin
            SetLength(TagsOR, 1);
            TagsOR[0]:= NTag;
         end
         else begin
            { If it ends in * and has not been identified as a registered tag, it will be assumed that you want to 
              indicate the search for tags that contain the entered substring (not just the one that ends with it).
              In this way, if we have for example the tags: "Error", "ErrorHigh", "ErrorLow" and we want to select only
              the first one, we will write "Error" (or simply select it from the selector, in the many possible ways).
              If we want to consider the three tags (an error, whatever the severity or if it has not been defined) we
              will write "Error*". When there is no match with any individual tag it is not necessary to write * to 
              select several. E.g.: If we have "PryA.ModuleA" and "PryA.ModuleB" we could write "PryA" to select the two
             }
            tagNameOR:= tagName;
            if tagNameOR[Length(tagNameOR)] = '*' then
               delete(tagNameOR, Length(tagNameOR), 1);

            NTagsOR.Clear;
            ActiveFile.UpdateNTagsMatching(tagNameOR, NTagsOR);
            M:= NTagsOR.Count;
            if M > 0 then begin
               SetLength(TagsOR, M);
               for i:= 0 to M - 1 do
                  TagsOR[i]:= NTagsOR[i];
            end;
         end;
         if TagsOR <> nil then begin
            ConsideredWords:= ConsideredWords + TagName + ' ';
            inc(N);
            SetLength(FindTags, N);
            SetLength(TagsNames, N);        // To control duplicates
            TagsNames[N-1]:= tagName;
            FindTags[N-1]:= TagsOR;
         end;
      end;
   end;

begin
   Result:= nil;
   NTagsOR:= nil;

   try

      if FTagsMode = tmSearch then begin
         NTagsOR:= TNoteTagList.Create;
      end;

      N:= 0;
      txt:= txtTags.Text;
      if trim(txt) <> '' then begin
         pI:= 1;
         for i:= 1 to txt.Length do begin
            if Txt[i] in TagCharsDelimiters then begin
               pF:= i-1;
               CheckTag;
               pI:= i+1;
            end;
         end;
         pF:= i;
         CheckTag;
      end;


      if FTagsMode = tmEdit then begin
        // Delete all temporarily added tags, and then add the ones identified in the text.
        // This will cause the ones added by mistake to be discarded, while also not unnecessarily increasing the value of the Tag IDs

        for i := 0 to ActiveFile.NoteTagsTemporalAdded.Count-1 do
           ActiveFile.DeleteNTag(ActiveFile.NoteTagsTemporalAdded[i]);
        ActiveFile.NoteTagsTemporalAdded.Clear;

        TagsStateBAK:= App.TagsState;
        App.TagsState := tsHidden;

        for i := 0 to High(TagsAssigned) do begin
            if TagsAssigned[i] = nil then begin
               NTag:= ActiveFile.AddNTag(TagsNames[i], '');
               TagsAssigned[i]:= NTag;
            end;
        end;
        App.TagsState := TagsStateBAK;
        App.TagsUpdated;                            // Perform the sorting only once

        NEntry:= FNote.Entries[0];                               // %%%%
        if not NEntry.HaveSameTags(TagsAssigned) then begin
           NEntry.Tags:= TagsAssigned;
           App.NEntryModified(NEntry, FNote, TKntFolder(FFolder));
           if TKntFolder(FFolder).TreeUI.ShowUseOfTags then
              Form_Main.RefreshFilterOnTags;
        end;
        txtTags.Text:= NEntry.TagsNames;

      end
      else begin     // FTagsMode = tmSearch
         { We allow the introduction of both individual tags and parts of them, which allow the selection of several at a time.
           In the latter case, they are treated as OR conditions.
           Example: Suppose that there are tags such as PryA.Module1 PryA.Module2 ... Bug Bug.High Bugh.Low
           If you enter: "PryA Bug.Low" -> (PryA.Module1 OR PryA.Module2 .. ) AND (Bug.Low) will be considered.
         }
         Result:= FindTags;
         if Ending then begin
            txtTags.Text:=  ConsideredWords;
            UpdateTxtFindTagsHint(txtTags, ConsideredWords, FindTags);
         end;
      end;

   finally
      if NTagsOR <> nil then
         NTagsOR.Free;
   end;

end;


procedure TTagMng.UpdateTxtTagsHint(TagEdit: TEdit = nil);
var
  Hint: string;
  txtEdit: TEdit;
begin
    txtEdit:= TagEdit;
    if txtEdit = nil then
       txtEdit:= txtTags;
    if txtEdit = nil then exit;


    Hint:= txtEdit.Text;
    if Hint = EMPTY_TAGS then
       Hint:= '';
    txtEdit.Hint:= Hint;
    if (Hint = '') then
        Hint:= GetRS(sTag7);
    Form_Main.RTFMTags.Hint:= Hint;
end;


procedure TTagMng.UpdateTxtFindTagsHint(txtEdit: TEdit; const ConsideredWords: string; FindTags: TFindTags);
var
  Hint: string;
  i, j: integer;
  TagsOR: TTagsOR;
  SepOR, SepAND: string;
  IncludesORs: boolean;
begin
    IncludesORs:= false;

    if FindTags <> nil then begin
      SepAND:= '';
      for i:= 0 to High(FindTags) do begin
         TagsOR:= FindTags[i];
         Hint:= Hint + SepAND;
         if Length(TagsOR) = 1 then
            Hint:= Hint + TagsOR[0].Name
         else begin
            IncludesORs:= True;
            Hint:= Hint + ' (';
            SepOR:= '';
            for j:= 0 to High(TagsOR) do begin
               Hint:= Hint + SepOR + TagsOR[j].Name;
               SepOR:= '│';
            end;
            Hint:= Hint + ' )';
         end;
         SepAND:= ' & ';
      end;
    end;
    if IncludesORs and (ConsideredWords <> '') then
       Hint:= '"' + ConsideredWords + '": ' + #13 + Hint;
    txtEdit.Hint:= Hint;
end;

{$ENDREGION}


initialization
   TagMng:= TTagMng.Create;
   TagMng.CreateTagSelector;

end.