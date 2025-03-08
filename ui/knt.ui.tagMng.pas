unit knt.ui.tagMng;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes, System.StrUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.StdCtrls,

  knt.ui.tagSelector,
  knt.model.note,
  kn_KntFolder
  ;



 type
    TOnEndTagsIntroduction = procedure(PressedReturn: boolean) of object;
    TTagsMode = (tmEdit, tmSearch);


  TTagMng = class
  private
    fChangingInCode: boolean;

    txtTags: TEdit;
    FTagsMode: TTagsMode;
    FOnEndTagsIntroduction: TOnEndTagsIntroduction;
    FOldTxtTagsWndProc: TWndMethod;

    FNote: TNote;
    FFolder: TKntFolder;

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
    procedure CommitAddedTags;
    function GetCaretPosTag: integer;
    function GetEndOfWord: integer;

  public
    destructor Destroy; override;

    procedure StartTxtTagProcessing(TagEdit: TEdit; TagsMode: TTagsMode; OnEndTagsIntroduction: TOnEndTagsIntroduction;
                                 Note: TNote = nil; Folder: TKntFolder = nil);
    procedure EndTxtTagProcessing(PressedReturn: boolean);
    procedure UpdateTxtTagsHint(TagEdit: TEdit = nil);

    procedure CreateTagSelector;
    procedure UpdateTagSelector;
    procedure GetTagsMaxWidth(NTags: TNoteTagList; var MaxNameWidth: integer; var MaxDescWidth: integer);
    procedure CheckEndTagIntroduction(OnTypedHash: boolean= false);
    procedure EndTagIntroduction;
    function IsValidTagName (var Name: string): boolean;
    function GetTagSubstr(Txt: string): string;
  end;


var
  TagMng: TTagMng;



implementation

uses VirtualTrees,
     gf_miscvcl,
     knt.ui.editor,
     kn_KntFile,
     knt.App,
     knt.RS
     ;



// Common methods =========================================

{$REGION Common methods }


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
   if TagSubstr <> '' then begin
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
   FOnEndTagsIntroduction:= nil;
end;

destructor  TTagMng.Destroy;
begin
   if txtTags <> nil then
      EndTxtTagProcessing(false);

   inherited Destroy;
end;


procedure TTagMng.StartTxtTagProcessing(TagEdit: TEdit; TagsMode: TTagsMode; OnEndTagsIntroduction: TOnEndTagsIntroduction;
                                     Note: TNote = nil; Folder: TKntFolder = nil);
begin
   txtTags:= TagEdit;
   FTagsMode:= TagsMode;
   FOnEndTagsIntroduction:= OnEndTagsIntroduction;
   if FTagsMode = tmEdit then begin
      FNote:= Note;
      FFolder:= Folder;
      ActiveFile.NoteTagsTemporalAdded.Clear;
   end;

   IgnoreTagSubstr:= '';
   if txtTags.Text = ' #' then
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


procedure TTagMng.EndTxtTagProcessing(PressedReturn: boolean);
var
  Color: TColor;

begin
   if txtTags <> nil then begin
      txtTags.SelStart:= 0;
      CheckBeginOfTag;

      cTagSelector.CloseTagSelector(true);

      if FTagsMode = tmEdit then begin
         CommitAddedTags;
         UpdateTxtTagsHint;
      end;
      Color:= clWindowText;
      if txtTags.Text = '' then begin
         txtTags.Text := ' #';
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

      if assigned(FOnEndTagsIntroduction) then
          FOnEndTagsIntroduction(PressedReturn);

      txtTags:= nil;
      FOnEndTagsIntroduction:= nil;
      FOldTxtTagsWndProc:= nil;
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
   EndTxtTagProcessing(false);
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
  if (Key = #13) and (IntroducingTagsState = itWithoutTagSelector) then
     EndTxtTagProcessing(true);

end;


procedure TTagMng.CommitAddedTags;
var
   i, pI, pF: integer;
   Txt, tagName: string;
   NTag: TNoteTag;
   TagsAssigned: TNoteTagArray;
   TagsNames: array of string;
   N: integer;
   TagsStateBAK: TTagsState;
   NEntry: TNoteEntry;

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
   begin
      if pI = pF then exit;
      tagName:= Copy(txt, pI, pF-pI+1);
      if Trim(tagName) = '' then exit;
      if tagName[Length(tagName)] = '.' then
         delete(tagName, Length(tagName), 1);
      if DuplicateTag(tagName) then exit;

      inc(N);
      SetLength(TagsAssigned, N);
      SetLength(TagsNames, N);
      TagsNames[N-1]:= tagName;
      NTag:= ActiveFile.GetNTagByName(tagName, ActiveFile.NoteTagsTemporalAdded);
      if NTag <> nil then
         NTag:= nil
      else
         NTag:= ActiveFile.GetNTagByName(tagName);  // If return NTag is nil must have been added by pasting, not typing.
      TagsAssigned[N-1]:= NTag;                     // NTag = nil => New tag that will be created
   end;

begin
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
     App.NEntryModified(NEntry, FNote, FFolder);
  end;
  txtTags.Text:= NEntry.TagsNames;
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
    if Hint = ' #' then
       Hint:= '';
    txtEdit.Hint:= Hint;
    if (Hint = '') then
        Hint:= GetRS(sTag7);
    Form_Main.RTFMTags.Hint:= Hint;
end;

{$ENDREGION}


initialization
   TagMng:= TTagMng.Create;

end.