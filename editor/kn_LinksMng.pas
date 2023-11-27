unit kn_LinksMng;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 (c) 2007-2015 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)

{.$DEFINE DEBUG_HISTORY}

interface
uses
   Winapi.Windows,
   Winapi.Messages,
   Winapi.ShellAPI,
   Winapi.RichEdit,
   System.Classes,
   System.SysUtils,
   System.StrUtils,
   System.IOUtils,
   System.Math,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Clipbrd,
   RxRichEd,
   TreeNT,
   gf_misc,
   gf_miscvcl,
   gf_files,
   kn_Const,
   kn_Info,
   kn_Ini,
   kn_RTFUtils,
   kn_NoteObj,
   kn_NoteFileMng,
   kn_NodeList,
   kn_TreeNoteMng,
   kn_LocationObj,
   kn_URL,
   kn_History
   ;


   // Links related routines
    procedure GetKNTLocation (var Location: TLocation; Simplified: Boolean= false);
    procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean );
    procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean ; TextURL: string);
    function BuildKNTLocationText( const aLocation : TLocation; IgnoreActiveNotePlainText: Boolean= false) : string;
    procedure JumpToKNTLocation( LocationStr : string );
    function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true): boolean;
    function SearchCaretPos (myNote : TTabNote; myTreeNode: TTreeNTNode; CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean): integer;
    function PositionInImLinkTextPlain (myNote: TTabNote; myTreeNode: TTreeNTNode; CaretPosition: integer): integer;

    procedure ClickOnURL(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false);
    procedure InsertURL(URLStr : string; TextURL : string; Note: TTabNote);

    function PathOfKNTLink (myTreeNode: TTreeNTNode; myNote : TTabNote; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
                            forUseInFindResults: boolean = false): string;
    procedure GetTreeNodeFromLocation (const Location: TLocation; var Note: TTabNote; var myTreeNode: TTreeNTNode);

    procedure NavigateToTreeNode(myTreeNode: TTreeNTNode);

    // Navigation history
    procedure AddHistoryLocation( const aNote : TTabNote; const AddLocalMaintainingIndex: boolean;
                                  aLocation: TLocation= nil; const AddToGlobalHistory: boolean= true);
    procedure NavigateInHistory( const Direction: THistoryDirection);
    procedure UpdateHistoryCommands;

    function TypeURL (var URLText: string; var KNTlocation: boolean): TKntURL;
    function URLFileExists (var URL: string): boolean;


var
   _Executing_History_Jump : boolean;
   _LastMoveWasHistory : boolean;


implementation
uses
   kn_Global,
   kn_Main,
   kn_EditorUtils,
   kn_FindReplaceMng,
   kn_ImagesMng,
   kn_ImageForm;


resourcestring
  STR_01 = 'Note ID not found: %d';
  STR_02 = 'Note name not found: %s';
  STR_03 = 'Node ID not found: %d';
  STR_04 = 'Node name not found: %s';
  STR_05 = 'Select file to link to';
  STR_06 = 'Select file to insert';
  STR_07 = 'The file you selected is not a plain-text or RTF file and cannot be inserted.';
  STR_08 = 'Cannot insert link to a KeyNote location, because no location has been marked. First, mark a location to which you want to link.';
  STR_09 = ' Location inserted';
  STR_10 = ' Current location marked';
  STR_11 = ' Failed to open location';
  STR_12 = 'Location does not exist or file cannot be opened: "%s"';
  STR_13 = 'Invalid location string: %s';
  STR_14 = ' Invalid location';
  STR_15 = 'Error executing hyperlink: %s';
  // STR_16 = ' Hold down SHIFT while clicking the URL:  ';
  STR_17 = ' URL modified';
  STR_18 = ' URL action canceled';
  STR_19 = ' URL copied to clipboard';
  STR_20 = 'Error %d executing hyperlink "%s": "%s"';
  STR_21 = ' History error';
  STR_22 = ' Cannot navigate to history location';
  STR_23 = ' History navigation error';

  STR_24 = 'Navigate backwards in history';
  STR_25 = 'Navigate backwards in note (''local'') history';
  STR_26 = 'Navigate backwards in global history';
  STR_27 = 'Navigate forward in history';
  STR_28 = 'Navigate forward in note (''local'') history';
  STR_29 = 'Navigate forward in global history';
  STR_30 = ' (Ctrl+click: only in note history)';
  STR_31 = ' [Mark: %d]';
  STR_32 = '   (Undo to remove new hidden markers)';

type
  EInvalidLocation = Exception;

var
   INVALID_CHARS_FN : array[0..8] of string = (
    '*', '?', '"', '<', '>', '|',
    '=', ';', ',');   // this ones are not invalid but very unusual..


const
   IMAGE_GOBACK_IN_NOTE:    integer = 53;
   IMAGE_GOFORWARD_IN_NOTE: integer = 54;
   IMAGE_GOBACK_OTHER_NOTE:    integer = 38;
   IMAGE_GOFORWARD_OTHER_NOTE: integer = 39;


procedure ClickOnURLImage(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false); forward;

//=========================================
// PathOfKNTLink
//=========================================
function PathOfKNTLink (myTreeNode: TTreeNTNode; myNote : TTabNote; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
                        forUseInFindResults: boolean = false): string;
var
  path, pathInsertionPoint : string;
  i, j, n, m: integer;
  pDelim: integer;
  ShowFullPath: boolean;
  PathTopToBottom: boolean;

begin
  PathTopToBottom:= TreeOptions.PathTopToBottom;
  ShowFullPath:= TreeOptions.ShowFullPath;
  if forUseInFindResults then begin
     ShowFullPath:= TreeOptions.ShowFullPathSearch;
     PathTopToBottom:= true;
  end;

  if assigned(myTreeNode) then begin
     if ShowFullPath then
        path:= GetNodePath( myTreeNode, TreeOptions.NodeDelimiter, PathTopToBottom ) // {N}
     else
        path:= myTreeNode.Text; // {N}

     if PathTopToBottom then
        path:= myNote.Name + TreeOptions.NodeDelimiter + path
     else
        path:= path + TreeOptions.NodeDelimiter + myNote.Name;
  end
  else
     path := myNote.Name;


  // Hide common part of the path (common ancestors), if RelativeKNTLink=True
  if RelativeKNTLink then begin
     pathInsertionPoint:= PathOfKNTLink(GetCurrentTreeNode, ActiveNote, -1, false, false);

     if path = pathInsertionPoint then
        path:= ''
     else begin
         i:= 1;
         n:= Length(path);
         m:= Length(pathInsertionPoint);

         if TreeOptions.PathTopToBottom then begin
            pDelim:= 0;
            while (i < Min(n, m)) and (path[i] = pathInsertionPoint[i]) do begin
               if path[i] = TreeOptions.NodeDelimiter then
                  pDelim:= i;
               i:= i + 1;
            end;
            if (i = m + 1) and (path[i] = TreeOptions.NodeDelimiter) then
               pDelim:= i;
            path:= Copy(path, pDelim+1, Length(path));
         end
         else begin
            i:= n;
            j:= m;
            pDelim:= n+1;
            while (i >= 1) and (j >= 1) and (path[i] = pathInsertionPoint[j]) do begin
               if path[i] = TreeOptions.NodeDelimiter then
                  pDelim:= i;
               i:= i - 1;
               j:= j - 1;
            end;
            if (j = 0) and (path[i] = TreeOptions.NodeDelimiter) then
               pDelim:= i;
            path:= Copy(path, 1, pDelim-1);
         end;

     end;
  end;


  if (position >= 0) and (ForceShowPosition or TreeOptions.CaretInKNTLinks or (path = '')) then begin
     if path = '' then
        path:= 'Pos.'
     else
        path := path + ' - ';

     path := path + IntToStr(position);
  end;

  Result:= path;
end; // PathOfKNTLink


//=========================================
// GetTreeNode
//=========================================
procedure GetTreeNodeFromLocation (const Location: TLocation; var Note: TTabNote; var myTreeNode: TTreeNTNode);
begin
   with Location do begin
     // obtain NOTE
      Note := nil;
      if (NoteID <> 0) then // new format
      begin
         Note := notefile.GetNoteByID( NoteID );
         if (Note = nil ) then
            raise EInvalidLocation.Create(Format( STR_01, [NoteID] ));
      end
      else begin
         Note := notefile.GetNoteByName( NoteName );
         if (Note = nil) then
            raise EInvalidLocation.Create(Format( STR_02, [NoteName] ));
      end;


      // obtain NODE
      myTreeNode := nil;
      if ( Note.Kind = ntTree ) and (NodeID >= 0) then begin   // If NodeID < 0 -> Node will be ignored
         if ( NodeID <> 0 ) then begin // new format
            myTreeNode := TTreeNote( Note ).GetTreeNodeByID( NodeID );
            if (myTreeNode = nil) then
               raise EInvalidLocation.Create(Format( STR_03, [NodeID] ));
         end
         else begin
            myTreeNode := TTreeNote( Note ).TV.Items.FindNode( [ffText], NodeName, nil );
            if (myTreeNode = nil) then
               raise EInvalidLocation.Create(Format( STR_04, [NodeName] ));
         end;
       end;
   end;
end;


//----------------------------------------
// InsertHyperlink
//----------------------------------------
procedure InsertHyperlink(URLStr: string; TextURL : string; KNTLink: boolean; Note: TTabNote);
var
  SelL: integer;
  SelR: integer;
  sepL, sepR: string;
  UseHyperlink: boolean;
  cad: string;
begin
    UseHyperlink:= False;
    if (RichEditVersion >= 4) and (not Note.PlainText) then
       UseHyperlink:= True;

    if ImagesManager.StorageMode <> smEmbRTF then begin
       if Note.Editor.SelLength > 0 then
          CheckToSelectLeftImageHiddenMark (Note.Editor);
    end;

    with Note.Editor do begin
      SelL := SelStart;
      SelR := SelL + SelLength;
      sepL:= '';
      SetSelection(SelL-1, SelL, false);
      cad:= Trim(SelText);
      if SelAttributes.Link or (not UseHyperlink and (cad <> '') and not (AnsiChar(cad[1]) in ['<','(','[','{']) ) then
          sepL:= ' ';
      SetSelection(SelR, SelR+1, false);
      cad:= Trim(SelText);
      if SelAttributes.Link or (not UseHyperlink and (cad <> '') and not (AnsiChar(cad[1]) in ['>',')',']','}']) ) then
         sepR:= ' ';
      SetSelection(SelL, SelR, false);

      {$IFDEF KNT_DEBUG}Log.Add('Insert HyperLink',  4 ); {$ENDIF}

      if UseHyperlink then begin
           Note.Editor.PutRtfText(Format('{\rtf1\ansi{\colortbl ;\red0\green0\blue255;}{\fonttbl}%s{\field{\*\fldinst{HYPERLINK "'
                                           + '%s"}}{\fldrslt{\cf1\ul %s}}}%s\cf0\ulnone}',
                                           [sepL, URLToRTF(URLStr, false ), URLToRTF(TextURL, true), sepR]), true);
      end
      else begin
          if not KNTLink then
             URLStr := FileNameToURL( URLStr );
          if TextURL <> '' then
             SelText := sepL + '''' + TextURL + '''' + ' (' + URLStr + ') '
          else
             SelText := sepL + URLStr + #32;

          if sepR = ' ' then
             SelStart:= SelStart + SelLength
          else begin
             SelStart:= SelStart + SelLength-1;
             SelLength:= 1;
             SelText:= '';
          end;
      end;

      SelLength := 0;
    end;
end;

//===============================================================
// InsertFileOrLink
//===============================================================
procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean );
var
  FN : string;
  oldFilter : string;
  ImportFileType : TImportFileType;
  ext : string;
  RTFAux: TRxRichEdit;

begin
   if ( not ( Form_Main.HaveNotes( true, true ) and assigned( ActiveNote ))) then exit;
   if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

   if ( aFileName = '' ) then begin
      with Form_Main.OpenDlg do begin
         oldFilter := Filter;
         if AsLink then
           Filter := FILTER_FILELINK
         else
           Filter := FILTER_RTFFILES + '|' +
                     FILTER_TEXTFILES + '|' +
                     FILTER_ALLFILES;
         FilterIndex := 1;
         if AsLink then
           Title := STR_05
         else
           Title := STR_06;
         Options := Options - [ofAllowMultiSelect];
         Form_Main.OpenDlg.FileName := '';
         if ( KeyOptions.LastImportPath <> '' ) then
           InitialDir := KeyOptions.LastImportPath
         else
           InitialDir := GetFolderPath( fpPersonal );
      end;

      try
         if ( not Form_Main.OpenDlg.Execute ) then exit;
         FN := Form_Main.OpenDlg.FileName;
         KeyOptions.LastImportPath := properfoldername( extractfilepath( FN ));
      finally
         Form_Main.OpenDlg.Filter := oldFilter;
         Form_Main.OpenDlg.FilterIndex := 1;
      end;

   end
   else
      FN := aFileName;


   if AsLink then begin
      if pos( 'FILE:', AnsiUpperCase(FN) ) = 0 then
         FN := 'file:///' + FN;

      InsertHyperlink(FN, StripFileURLPrefix(FN), false, ActiveNote);
   end

   else begin
       ext := extractfileext( FN );
       ImportFileType := itText;
       if ( ext = ext_RTF ) then
          ImportFileType := itRTF
       else
       if ExtIsHTML( ext ) then
          ImportFileType := itHTML
       else
       if ExtIsText( ext ) then
          ImportFileType := itText

       else begin
          DoMessageBox( STR_07, mtError, [mbOK]);
          exit;
       end;

       ActiveNote.Editor.Lines.BeginUpdate;

       try
         try
           if ImagesManager.StorageMode <> smEmbRTF then begin
              if ActiveNote.Editor.SelLength > 0 then
                 CheckToSelectLeftImageHiddenMark (ActiveNote.Editor);
           end;

           case ImportFileType of
             itText, itHTML : begin
               ActiveNote.Editor.SelText :=  TFile.ReadAllText(FN);
               ActiveNote.Editor.SelLength := 0;
             end;

             itRTF:
               ActiveNote.Editor.PutRtfText(TFile.ReadAllText(FN), true);
           end;

         except
           on E : Exception do begin
              DoMessageBox( E.Message, mtError, [mbOK] );
              exit;
           end;
         end;

       finally
          ActiveNote.Editor.Lines.EndUpdate;
       end;

   end;

   NoteFile.Modified := true;
   UpdateNoteFileState( [fscModified] );

end; // InsertFileOrLink


//===============================================================
// GetKNTLocation
//===============================================================
procedure GetKNTLocation (var Location: TLocation; Simplified: Boolean= false);
var
  tNote: TTreeNote;
begin
{$IFDEF DEBUG_HISTORY}
   Simplified:= false;
{$ENDIF}

   if not assigned(Location) then
      Location:= TLocation.Create;

    with Location do begin
      if not Simplified then begin
        FileName := normalFN( NoteFile.FileName );
        NoteName := ActiveNote.Name;
      end;
      NoteID := ActiveNote.ID;
      NodeName := '';
      NodeID := 0;
      if (ActiveNote.Kind = ntTree) then begin
         tNote:= TTreeNote(ActiveNote);
         if TTreeNote(ActiveNote).SelectedNode <> nil then begin
            NodeID := TTreeNote(ActiveNote).SelectedNode.ID;
            if not Simplified then
               NodeName := TTreeNote(ActiveNote).SelectedNode.Name;
         end;
      end;
      CaretPos := ActiveNote.Editor.SelStart;
      SelLength := ActiveNote.Editor.SelLength;
      Mark := 0;
    end;

end;


//===============================================================
// GetLastTargetMarker
//===============================================================
function GetLastTargetMarker(const Str: string): integer;
var
  p, pF, N, Num: integer;
  TargetMarker: String;
begin
   TargetMarker:= KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_BOOKMARK;          // \v\'11B999\'12\v0      HB999H
   Num:= 0;

   try
     p:= 1;
     repeat
       p:= pos(TargetMarker, Str, p);
       if (p > 0) then begin
          pF:= pos(KNT_RTF_HIDDEN_MARK_R_CHAR, Str, p+1);
          if (pF > 0 ) and ((pF - p) <= 5) then begin
             N:= StrToInt(Copy(Str, P + 2, (pF - p)-2));
             if N > Num then
                Num:= N;
          end;
          p:= pF + 1;
       end;
     until (p = 0);

   except
   end;

   Result:= Num;
end;



//===============================================================
// InsertOrMarkKNTLink
//===============================================================

(*
  New KNT Links, vinculated to markers, not only to caret position

	Until now, KNT links only included, in addition to the note and node, the caret position. If the note
	was edited, the absolute position we were pointing to, would have changed and therefore our link would no longer point
	to the intended position.

	When marking a KNT location (Ctr+F6) the app will now insert a small hidden bookmark next to the text
	identified as target. When inserting the link (Shift+F6) the new hyperlink will refer to the created bookmark,
	and will locate it even though all the text could have been relocated (within the same note/node). If the marker
	were eliminated, the caret position would be used, as before.

	Note: The bookmark will only be inserted if the destination is a note or RTF node, other than virtual node.

	The format used internally to register these markers is of the form. Ex:

   	{\rtf1\ansi \v\'11B5\'12\v0 Target of a KNT link}
	   Once retrieved as plain text: '#$11'B5'#$12'Target of a KNT link
     B: Bookmark   "5" 5º bookmark in the note / node.

	To achieve this, it has been necessary for KNT to be able to handle RTF texts with this type of hidden text from any
	functionality.
	Among other things, it has been necessary to replace the mechanism that has been used to carry out the searches, where
	the rich control (RxRichEdit.FindText) had been called directly iteratively. All the hidden texts are considered
	in the search by RichEdit control, and would prevent finding the patterns that include them inside.

	Now the search is done directly in Delphi, on a text string previously retrieved from the control.
	A new method has been added to the RichEdit control, TextPlain, which is supported by the EM_GETTEXTEX message,
	making use of the option GT_RAWTEXT:
	<<Text is retrieved exactly as it appears in memory. This includes special structure characters for table row and cell delimiters
	 (see Remarks for EM_INSERTTABLE) as well as math object delimiters (start delimiter U+FDD0, argument delimiter U+FDEE, and end
	 delimiter U+FDDF) and object markers (U+FFFC). This maintains character-position alignment between the retrieved text and the
	 text in memory.>>

	This way, position identified looking for a pattern text in a string returned by this function can be used to move caret to that position.

	As an advantage, the new search mechanism is much faster. Once initialized [*] new plain text variables (NoteTextPlain / NodeTextPlain),
  searches will now be almost instantaneous,	whereas currently each search incurs the same amount of time, which on a large file can easily
  be 5 or 6 seconds.
    [*] With TNoteFile.UpdateTextPlainVariables, called by TForm_Main.TimerTimer, or on demand by RunFindAllEx (in kn_FindReplaceMng)

	Subsequently, a modification will be included to try to ensure that these initial data are obtained at times when
	the application is idle, so even the first search is instantaneous.

	It has been necessary to revise many other points in the application to ensure that these hidden characters are 'transparent'.
	For example, when copying (cutting or pasting is not necessary) to the clipboard, we must remove our possible hidden characters
	(not others like hyperlinks). It is also needed to discard those hidden characters when exporting to other files, or when creating
	templates. It has also been necessary to keep them in mind when performing operations such as changing to uppercase, lowercase, etc.

	Once these changes have been made, the management of this new hidden text format will allow the implementation of other functionalities
  in which it is necessary to mark or tag any text within a note.

*)

procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean; TextURL: string);
var
   Note: TTabNote;
   TreeNode: TTreeNTNode;
   TargetMarker: integer;
   str, strTargetMarker: string;

   function GetActualTargetMarker (Editor: TRxRichEdit): integer;
   var
      SS, L, Len: integer;
      Str: String;
   begin
      // *1 By the check made from TForm_Main.RxRTFKeyDown, when the Left cursor is pressed, if it is already next to
      //    an existing marker, we'll be just to the left
      // *2 But if we insert a marker (CTR+F6) several times without moving the position, there would be a mark just in the left
      Result:= 0;
      Str:= '';
      try
        with Editor do begin
           SS:= SelStart;

           SelStart:= SS + 1;
           L:= SelStart;                    // *1
           if L <> SS + 1 then              // It will have been placed to the left of the first non-hidden character, to the right (L > SS )
              Str:= GetTextRange(SS, L)     // HB999H
           else begin
              SelStart:= SS - 1;
              L:= SelStart;                  // *2
              if L <> SS - 1 then            // It will have been placed to the left of the first non-hidden character, to the left (L < SS)
                 Str:= GetTextRange(L, SS);
           end;


           if Str <> '' then begin
              Len:= Length(Str);             // HB999H
              if (Str[1]=KNT_RTF_HIDDEN_MARK_L_CHAR) and (Str[2]=KNT_RTF_HIDDEN_BOOKMARK) and (Str[Len]=KNT_RTF_HIDDEN_MARK_R_CHAR) then
                 Result:= StrToInt(Copy(Str, 3, (Len)-3));
           end;

           SelStart:= SS;
        end;
      except
      end;
   end;

begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( aLocation = nil ) then
     aLocation := _KNTLocation;

  if AsInsert then begin
    // insert link to previously marked location
    if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
    if ( aLocation.NoteName = '') and (aLocation.NoteID = 0) then begin
      showmessage( STR_08 );
      exit;
    end;

    if TextURL = '' then
       if NoteFile.FileName = aLocation.FileName then begin
           GetTreeNodeFromLocation (aLocation, Note, TreeNode);
           TextURL:= PathOfKNTLink(TreeNode, Note, aLocation.CaretPos, false, TreeOptions.RelativeKNTLinks);
       end
       else
          TextURL:= Format('%s: %s/%s %d', [ExtractFileName(aLocation.FileName),
                                                aLocation.NoteName, aLocation.NodeName,
                                                aLocation.CaretPos]);

    InsertHyperlink(BuildKNTLocationText(aLocation),  TextURL, true, ActiveNote);
    if aLocation.Mark <> 0 then
       strTargetMarker:= Format(STR_31, [aLocation.Mark]);
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_09 + strTargetMarker;
  end
  else begin
    // mark caret position as TLocation
    GetKNTLocation (aLocation);

    // If we are pointing to the start of a node or a note (CaretPos = 0), we will not create any new markers. We will always aim for that position 0.
    strTargetMarker:= '';

    if (aLocation.CaretPos <> 0) and (not ActiveNote.PlainText)
       and ((ActiveNote.Kind <> ntTree) or (TTreeNote(ActiveNote).SelectedNode.VirtualMode in [vmNone, vmRTF, vmKNTNode]) ) then begin        // Allow the mark (hidden) although Note is ReadOnly
      TargetMarker:= GetActualTargetMarker(ActiveNote.Editor);             // If a marker already exists at that position, we will use it
      if TargetMarker = 0 then begin
         {$IFDEF KNT_DEBUG}Log.Add('Insert Marker for HyperLink',  4 ); {$ENDIF}
         TargetMarker:= 1 + GetLastTargetMarker(ActiveNote.Editor.TextPlain);
         //  {\rtf1\ansi {\v\'11B5\'12}};    => {\rtf1\ansi \v\'11B5\'12\v0};  // Finally they will be inserted directly with \v...\v0 (see comment *2 next to KNT_RTF_BMK_HIDDEN_MARK in kn_const.pas)
         ActiveNote.Editor.PutRtfText(Format('{\rtf1\ansi' + KNT_RTF_BMK_HIDDEN_MARK + '}', [TargetMarker]),  true);
      end;
      strTargetMarker:= Format(STR_31 + STR_32, [TargetMarker]);
      aLocation.Mark:= TargetMarker;
    end;

    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_10 + strTargetMarker;
  end;

end; // InsertOrMarkKNTLink


//===============================================================
// BuildKNTLocationText
//===============================================================
function BuildKNTLocationText( const aLocation : TLocation; IgnoreActiveNotePlainText: Boolean= false) : string;
var
  LocationString : string;
  NoteId, NodeId, LocationMark: string;
begin
  if ( aLocation.FileName = normalFN( NoteFile.FileName )) then
    LocationString := ''
  else
    LocationString := FileNameToURL( aLocation.FileName );

    // [x] this does not handle files on another computer, i.e.
    // we cannot do file://computername/pathname/file.knt

    if (RichEditVersion >= 4) and (IgnoreActiveNotePlainText or not ActiveNote.PlainText) then begin
        LocationMark:= KNTLOCATION_MARK_NEW;
        NoteId:= inttostr( aLocation.NoteID );
        NodeId:= inttostr( aLocation.NodeID );
    end
    else begin
        LocationMark:= KNTLOCATION_MARK_OLD;
        NoteId:= FileNameToURL( aLocation.NoteName );
        NodeId:= FileNameToURL( aLocation.NodeName );
    end;

    LocationString := 'file:///' + LocationString + LocationMark +
      NoteId + KNTLINK_SEPARATOR +
      NodeId + KNTLINK_SEPARATOR +
      inttostr( aLocation.CaretPos ) + KNTLINK_SEPARATOR +
      inttostr( aLocation.SelLength );

    if aLocation.Mark > 0 then
       LocationString := LocationString + KNTLINK_SEPARATOR + inttostr( aLocation.Mark);

  result := LocationString;
end; // BuildKNTLocationText



//---------------------------------------------------------------
// BuildKNTLocationFromString
//---------------------------------------------------------------
function BuildKNTLocationFromString( LocationStr : string ): TLocation;
var
  p, pold, pnew : integer;
  Location : TLocation;
  NewFormatURL : boolean;
  origLocationStr : string;
  Note: TTabNote;
  myTreeNode: TTreeNTNode;
begin

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive two types of links:
  // the old style link: file:///?filename.knt...
  // the new style link: file:///*filename.knt...

    p := 0;
    origLocationStr := LocationStr;

    Location := TLocation.Create;

    LocationStr := StripFileURLPrefix( LocationStr );

    pold := pos( KNTLOCATION_MARK_OLD, LocationStr );
    pnew := pos( KNTLOCATION_MARK_NEW, LocationStr );
    if (( pold = 0 ) and ( pnew = 0 )) then
      raise EInvalidLocation.Create( origLocationStr );
    // see which marker occurs FIRST
    // (both markers may occur, because '?' and '*' may occur within note or node names
    if ( pnew < pold ) then begin
      if ( pnew > 0 ) then begin
        NewFormatURL := true;
        p := pnew;
      end
      else begin
        NewFormatURL := false;
        p := pold;
      end;
    end
    else begin
      if ( pold > 0 ) then begin
        NewFormatURL := false;
        p := pold;
      end
      else begin
        NewFormatURL := true;
        p := pnew;
      end;
    end;

    // extract filename
    case p of
      0 : raise EInvalidLocation.Create( origLocationStr );
      1 : Location.FileName := ''; // same file as current
      else begin
        Location.FileName := HTTPDecode( copy( LocationStr, 1, pred( p )));
        if ( Location.FileName = NoteFile.FileName ) then
          Location.FileName := '';
      end;
    end;
    delete( LocationStr, 1, p ); // delete filename and ? or * marker

    // extract note name or ID
    p := pos( KNTLINK_SEPARATOR, LocationStr );
    case p of
      0 : begin
        if NewFormatURL then
          Location.NoteID := strtoint( LocationStr ) // get ID
        else
          Location.NoteName := HTTPDecode( LocationStr ); // get name
        LocationStr := '';
      end;
      1 : raise EInvalidLocation.Create( origLocationStr );
      else
      begin
        if NewFormatURL then
          Location.NoteID := strtoint( copy( LocationStr, 1, pred( p )))
        else
          Location.NoteName := HTTPDecode( copy( LocationStr, 1, pred( p )));
        delete( LocationStr, 1, p );
      end;
    end;

    if Location.NoteID <> 0 then
       Note := notefile.GetNoteByID( Location.NoteID )
    else
       Note := notefile.GetNoteByName( Location.NoteName );

    if  assigned(Note) then begin
        Location.NoteID:= Note.ID;
        Location.NoteName:= Note.Name;
    end;


    p := pos( KNTLINK_SEPARATOR, LocationStr );
    case p of
      0 : begin
        if NewFormatURL then
          Location.NodeID := strtoint( LocationStr )
        else
          Location.NodeName := HTTPDecode( LocationStr );
        LocationStr := '';
      end;
      1 : begin
        Location.NodeName := '';
        Location.NodeID := -1;
      end;
      else
      begin
        if NewFormatURL then
          Location.NodeID := strtoint( copy( LocationStr, 1, pred( p )))
        else
          Location.NodeName := HTTPDecode( copy( LocationStr, 1, pred( p )));
      end;
    end;
    delete( LocationStr, 1, p );

    if assigned(Note) then
      if ( Note.Kind = ntTree ) and (Location.NodeID >= 0) then begin
        if ( Location.NodeID <> 0 ) then
          myTreeNode := TTreeNote( Note ).GetTreeNodeByID( Location.NodeID )
        else
          myTreeNode := TTreeNote( Note ).TV.Items.FindNode( [ffText], Location.NodeName, nil );

        if assigned(myTreeNode) then
            if assigned( myTreeNode.Data ) then begin
               Location.NodeID:= TNoteNode( myTreeNode.Data ).ID;
               Location.NoteName:= TNoteNode( myTreeNode.Data ).Name;
            end;
      end;


    if ( LocationStr <> '' ) then begin
      p := pos( KNTLINK_SEPARATOR, LocationStr );
      if ( p > 0 ) then begin
          try
            Location.CaretPos := strtoint( copy( LocationStr, 1, pred( p )));
          except
            Location.CaretPos := 0;
          end;
          delete( LocationStr, 1, p );
      end;
    end;

    if ( LocationStr <> '' ) then begin
      p := pos( KNTLINK_SEPARATOR, LocationStr );
      Location.SelLength := 0;
      if ( p > 0 ) then begin                              // selLenght|mark
          try
            Location.SelLength := strtoint(copy(LocationStr, 1, pred(p)));
          except
          end;
          delete( LocationStr, 1, p );
          if ( LocationStr <> '' ) then
              try
                Location.Mark := strToInt( LocationStr );
              except
                Location.Mark := 0;
              end;
      end
      else                                               // selLenght
          try
            Location.SelLength := strtoint(LocationStr);
          except
          end;
    end;


    Result:= Location;

end; // BuildKNTLocationFromString


//===============================================================
// NavigateToTreeNode
//===============================================================
procedure NavigateToTreeNode(myTreeNode: TTreeNTNode);
var
  myNote: TTabNote;
begin
    if assigned(myTreeNode) then begin
        myNote:= NoteFile.GetNoteByTreeNode(myTreeNode);
        if ( myNote <> ActiveNote ) then begin
          Form_Main.Pages.ActivePage := myNote.TabSheet;
          Form_Main.PagesChange( Form_Main.Pages );
        end;

        if assigned( myTreeNode ) then begin
           myTreeNode.MakeVisible;
           TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
        end;
    end;
end;


function GetPositionOffset (myNote : TTabNote; myTreeNode: TTreeNTNode; Pos_ImLinkTextPlain: integer; CaretPosition: integer): integer;
var
  Stream: TMemoryStream;
  imLinkTextPlain: String;
  Offset: integer;
  RtfModified: boolean;
begin
   Offset:= 0;
   if (CaretPosition < 0) and (Pos_ImLinkTextPlain < 0) then exit;


   if (ImagesManager.StorageMode <> smEmbRTF) and NoteSupportsRegisteredImages then begin
     imLinkTextPlain:= '';
      if assigned(myTreeNode) then begin
          if assigned( myTreeNode.Data ) then begin
             Stream:= TNoteNode( myTreeNode.Data ).Stream;
             imLinkTextPlain := TNoteNode( myTreeNode.Data ).NodeTextPlain;
             RtfModified := TNoteNode( myTreeNode.Data ).RTFModified;
          end;
      end
      else begin
         Stream:= myNote.DataStream;
         imLinkTextPlain := myNote.NoteTextPlain;
         RtfModified := myNote.Modified;
      end;

   end;

   // See notes in ImagesManager.GetPositionOffset

   if CaretPosition >= 0 then
      Offset:= ImagesManager.GetPositionOffset_FromEditorTP (Stream, CaretPosition, imLinkTextPlain, RTFModified)
   else
      Offset:= ImagesManager.GetPositionOffset_FromImLinkTP (Stream, Pos_ImLinkTextPlain, imLinkTextPlain, RtfModified);


   Result:= Offset;
end;


function SearchCaretPos (myNote : TTabNote; myTreeNode: TTreeNTNode; CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean): integer;
var
  Offset: integer;
  Pos_ImLinkTextPlain: integer;
begin
  Pos_ImLinkTextPlain:= CaretPosition;            // What we receive we must treat as a position in imLinkTextPlain
  Offset:= GetPositionOffset(myNote, myTreeNode, Pos_ImLinkTextPlain, -1);

  if PlaceCaret then
     with myNote.Editor do begin
       SelStart := CaretPosition - Offset;
       SelLength := SelectionLength;
       Perform( EM_SCROLLCARET, 0, 0 );
     end;

  Result:= Offset;
end;

function PositionInImLinkTextPlain (myNote: TTabNote; myTreeNode: TTreeNTNode; CaretPosition: integer): integer;
var
   Offset: integer;
begin
   Offset:= GetPositionOffset(myNote, myTreeNode, -1, CaretPosition);
   Result:= CaretPosition + Offset;
end;



//===============================================================
// JumpToLocation
//===============================================================
function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true): boolean;
var
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  origLocationStr : string;
  LocBeforeJump: TLocation;

  function SearchTargetMark: boolean;
  var
     p, selLen: integer;
     TargetMark: string;
  begin
      Result:= false;
      if Location.Mark > 0 then begin
        TargetMark:=  KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_BOOKMARK + IntToStr(Location.Mark) + KNT_RTF_HIDDEN_MARK_R_CHAR;
        with myNote.Editor do begin
          p:= FindText(TargetMark, 0, -1, []);
          if p > 0 then begin
            SelStart := p;
            selLen:= 0;
            if Location.SelLength > 0 then
               selLen:= Location.SelLength + Length(TargetMark);
            SelLength := selLen;
            Perform( EM_SCROLLCARET, 0, 0 );
            Result:= true;
          end;
        end;
      end;
  end;

begin

  result := false;
  if IgnoreOtherFiles and ( not Form_Main.HaveNotes( false, true )) then exit;

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive two types of links:
  // the old style link: file:///?filename.knt...
  // the new style link: file:///*filename.knt...

  try
      LocBeforeJump:= nil;
      GetKntLocation (LocBeforeJump, true);

      (*
      showmessage(
        'file: ' + Location.FileName + #13 +
        'note: ' + Location.NoteName + #13 +
        'note id: ' + inttostr( Location.NoteID ) + #13 +
        'node: ' + Location.NodeName + #13 +
        'node id: ' + inttostr( Location.NodeID ) + #13 +
        inttostr( Location.CaretPos ) + ' / ' + inttostr( Location.SelLength )
      );
      *)

      // open file, if necessary
      if ( Location.FileName <> '' ) and ( Location.FileName <> NoteFile.FileName ) then
      begin
        if IgnoreOtherFiles then
           exit;
        if (( not Fileexists( Location.FileName )) or
         ( NoteFileOpen( Location.FileName ) <> 0 )) then
        begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_11;
          raise EInvalidLocation.Create(Format( STR_12, [origLocationStr] ));
        end;
      end;

      GetTreeNodeFromLocation(Location, myNote, myTreeNode);
      // if not current note, switch to it
      if ( myNote <> ActiveNote ) then
      begin
        Form_Main.Pages.ActivePage := myNote.TabSheet;
        Form_Main.PagesChange( Form_Main.Pages );
      end;

      if assigned( myTreeNode ) then begin
         // select the node
         if TTreeNote( ActiveNote ).TV.Selected <> myTreeNode then begin
            myTreeNode.MakeVisible;     // It could be hidden
            TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
         end;
      end;

      result := true;

      if not SearchTargetMark then
         SearchCaretPos(myNote, myTreeNode, Location.CaretPos, Location.SelLength, true);

      myNote.Editor.SetFocus;


      if _Executing_History_Jump then begin
         if (LocBeforeJump.NoteID <> Location.NoteID) then
            ActiveNote.History.SyncWithLocation (Location, hdBack, true);
      end
      else
      if (LocBeforeJump <> nil) and
         (LocBeforeJump.NoteID = ActiveNote.ID) and ((ActiveNote.Kind = ntRTF) or (TTreeNote(ActiveNote).SelectedNode.ID = LocBeforeJump.NodeID)) then begin
          AddHistoryLocation (ActiveNote, false, LocBeforeJump);
          _LastMoveWasHistory:= false;
          UpdateHistoryCommands;
      end;


    except
      on E : EInvalidLocation do
        if not _Executing_History_Jump then
          DoMessageBox( Format( STR_13, [E.Message] ), mtWarning, [mbOK]);
      on E : Exception do
        begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_14;
        DoMessageBox( Format( STR_15, [E.Message] ), mtWarning, [mbOK]);
        end;
  end;

end; // JumpToLocation

//===============================================================
// JumpToKNTLocation
//===============================================================
procedure JumpToKNTLocation( LocationStr : string );
var
  Location : TLocation;
begin
  try
    Location:= BuildKNTLocationFromString(LocationStr);
    try
       JumpToLocation(Location, false);
    finally
       Location.Free;
    end;

  except
    on E : EInvalidLocation do
      DoMessageBox( Format( STR_13, [E.Message] ), mtWarning, [mbOK]);

    on E : Exception do
      begin
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_14;
      DoMessageBox( Format( STR_15, [E.Message]  ), mtError, [mbOK] );
      end;
  end;

end; // JumpToKNTLocation



function URLFileExists (var URL: string): boolean;
var
   AbsolutePath: string;
begin
   AbsolutePath:= GetAbsolutePath(ExtractFilePath(NoteFile.FileName), URL);
   Result:= FileExists( AbsolutePath) or DirectoryExists( AbsolutePath );
end;


//--------------------------------------------------
// TypeURL
//--------------------------------------------------
function TypeURL (var URLText: string; var KNTlocation: boolean): TKNTURL;
var
   URLType, KntURL: TKNTURL;
   URLPos : integer; // position at which the actual URL starts in URLText
   URLTextLower: string;
   URLaux, URLaux2: string;
begin
  // determine where URL address starts in URLText
  URLType := urlUndefined;
  if URLText = '' then begin
     Result:= urlUndefined;
     exit;
  end;



  URLTextLower:= AnsiLowerCase(URLText);
  for KntURL := low( KntURL ) to high( KntURL ) do
  begin
    if KntURL = urlUndefined then continue;
    URLPos := pos( KNT_URLS[KntURL], URLTextLower );
    if ( URLPos > 0 ) then
    begin
      URLType := KntURL;
      break;
    end;
  end;

  if ( URLType  <> urlUndefined ) then
    URLText := copy( URLText, URLPos, length( URLText ))

  else
      if ( pos( '@', URLText ) > 0 ) then begin
          URLText := 'mailto:' + trim(URLText);
          URLType := urlMailto;
          end
      else if ( pos( 'WWW.', AnsiUpperCase(URLText) ) > 0 ) then begin
          URLText := 'http://' + trim(URLText);
          URLType := urlHttp;
          end;

  KNTlocation:= False;   // By default

  if (URLType = urlUndefined) then
      if pos( ':', URLText ) <= 2 then
         URLType := urlFile;


  if (URLType = urlFile) then begin
      if (( pos( KNTLOCATION_MARK_NEW, URLText ) > 0 ) or ( pos( KNTLOCATION_MARK_OLD, URLText ) > 0 )) then
          KNTlocation:= True

      else begin
          URLaux:= URLText;
          // various fixes, mostly with XP in mind:
          {1}
          if KeyOptions.URLFileNoPrefix then
             URLaux := StripFileURLPrefix( URLaux );

          {2}
          if KeyOptions.URLFileDecodeSpaces then begin
             URLaux2 := HTTPDecode(URLaux);
             if URLFileExists(URLaux2) then begin
               URLaux:= URLaux2;
               {3}
               if ( KeyOptions.URLFileQuoteSpaces and ( pos( #32, URLaux ) > 0 )) then
                  URLaux := '"' + URLaux + '"';
            end;

          URLText:= URLaux;
          end;

      end;
  end
  else
      if (URLType = urlHTTP) or ((URLType = urlHTTPS)) then begin
          URLText:= ReplaceStr(URLText, #32, '%20');
      end;


  if (URLType = urlUndefined) then
      URLType := urlOther;

  result:= URLType;
end;


//------------------------------------------------------
// TextOfLink
//------------------------------------------------------
(*
''' <summary>
''' Returns text associated (shown) to hyperlink, and start and end position of that text.
''' </summary>
''' <param name="endPosURL">Final position of the hyperlink (URL) [in]</param>
''' <param name="startPos">Initial position of the text shown associated to the hyperlink finished in 'endPosURL' [out]</param>
''' <param name="endPos">Final position of the text shown associated to the hyperlink finished in 'endPosURL'</param>
''' <returns>Text associated to hyperlink (because it uses {\field{\*\fldinst{HYPERLINK ... ). "" in other cases</returns>' +
'*)
Function TextOfLink(endPosURL: Integer; var startPos: Integer; var endPos: Integer): string;
var
    pos: Integer;
    esLink: Boolean;
    lastPosLink: Integer;
    _selectionLenght:Integer;
    _selectStart: Integer;
    TextLen: Integer;
 begin
        _selectStart := ActiveNote.Editor.SelStart;
        _selectionLenght := ActiveNote.Editor.SelLength;

        Try
            // If uses {\field{\*\fldinst{HYPERLINK "hyperlink" ... ) then next char will be "", hidden
            ActiveNote.Editor.SetSelection(endPosURL+1, endPosURL+1, false);

            If (ActiveNote.Editor.SelText = '')
               and (ActiveNote.Editor.GetTextRange(endPosURL+1, endPosURL+2) = '"') Then begin     // " character doesn't have Hidden mark but is treated as such
                lastPosLink := endPosURL + 1;
                pos := lastPosLink;
                TextLen:= ActiveNote.Editor.TextLength;
                repeat
                    pos := pos + 1;
                    ActiveNote.Editor.SetSelection(pos, pos, false);
                    esLink:= (ActiveNote.Editor.SelAttributes.LinkStyle = lsLink);
                    If esLink Then
                        lastPosLink := pos;
                Until Not esLink or (pos > TextLen);

                If lastPosLink >= (endPosURL + 2) Then begin
                    startPos := endPosURL + 2;
                    endPos := lastPosLink;
                    Result := ActiveNote.Editor.GetTextRange(startPos, endPos+1);
                End;

            End;

        Finally
            ActiveNote.Editor.SelStart:= _selectStart;
            ActiveNote.Editor.SelLength:= _selectionLenght;
        End;

End;



//===============================================================
// ClickOnURL
//===============================================================
procedure ClickOnURL(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false);
var
  ShellExecResult : integer;
  Form_URLAction: TForm_URLAction;
  //myURLAction : TURLAction;
  browser : string;
  URLType : TKNTURL;
  myURL : string; // the actual URL
  TextURL : string; // the text shown for actual URL
  textURLposIni, textURLposFin: Integer;
  //ShiftWasDown, AltWasDown, CtrlWasDown : boolean;
  usesHyperlinkCmd: boolean;
  path: string;
  Location: TLocation;
  KNTlocation: boolean;
  FileName, Parameters: String;


  function GetHTTPClient : string;
  begin
    result := '';
    if ( not KeyOptions.URLSystemBrowser ) then
       result := NormalFN( KeyOptions.URLAltBrowserPath );

    if ( result = '' ) then
       result := GetDefaultBrowserPath();
  end;

  function KNTPathFromString (url: string): string;
  var
    Location: TLocation;
    note: TTabNote;
    treeNode: TTreeNTNode;
  begin
     Location:= BuildKNTLocationFromString(URL);
     try
       if (NoteFile.FileName = Location.FileName) or (Location.FileName = '') then begin
           GetTreeNodeFromLocation (Location, note, treeNode);
           Result:= PathOfKNTLink(treeNode, note, Location.CaretPos, false, false);
       end
       else
          if Location.NodeName <> '' then
             Result:= Format('%s: %s/%s|%d|%d', [ExtractFileName(Location.FileName),
                                                Location.NoteName, Location.NodeName,
                                                Location.CaretPos, Location.SelLength])
          else
             Result:= Format('%s: %d|%d|%d|%d', [ExtractFileName(Location.FileName),
                                                Location.NoteID, Location.NodeID,
                                                Location.CaretPos, Location.SelLength]);
     finally
       Location.Free;
     end;
  end;

begin

  // this procedure must now support two methods of handling URLstr
  // that is passed to it. If the link was added with richedit v. 3
  // loaded, the link text will have a different format then when
  // created with earlier versions of richedit20.dll. See
  // TForm_Main.InsertHyperlink for detailed comments on this.

  //ShiftWasDown := ShiftDown and ( not _IS_FAKING_MOUSECLICK );
  //CtrlWasDown := CtrlDown and ( not _IS_FAKING_MOUSECLICK );
  //AltWasDown := AltDown and ( not _IS_FAKING_MOUSECLICK );
  _GLOBAL_URLText := '';

  // Determine type of URL. Parameter of TypeURL can also be modified
  myURL := URLstr;
  URLType := TypeURL( myURL , KNTlocation);

  if (URLType = urlKNTImage) and
        (not (myURLAction in [urlNothing, urlCopy, urlCreateOrModify])
          or (KeyOptions.ImgHotTrackViewer and (ImgViewerInstance <> nil) ))  then begin

      if not KeyOptions.ImgHotTrackViewer then
         ClickOnURLImage (URLstr, chrgURL, myURLAction, EnsureAsk)

      else begin
         if ShowingImageOnTrack then
            ShowingImageOnTrack:= false
         else begin
           ClickOnURLImage (URLstr, chrgURL, myURLAction, EnsureAsk);
           ShowingImageOnTrack:= true;
         end;
      end;

      exit;
  end;


  ShellExecResult := maxint; // dummy
  usesHyperlinkCmd:= false;

  try
    try

    (*
      myURLAction := KeyOptions.URLAction; // assume default action

      if AltWasDown then
         myURLAction := urlCopy
      else
         if CtrlWasDown and ( KNTLocation or (URLType <> urlFile) or (URLFileExists(myURL)) ) then begin
            {
            if ( myURLAction <> urlOpenNew ) then
               myURLAction := urlOpenNew // always open in new window if Ctrl pressed
            else
            }
            myURLAction := urlOpen;
         end;
      {                    Shift + Click doesn't raise now the event Editor.URLClick
       else begin
        if (( not _IS_FAKING_MOUSECLICK ) and KeyOptions.URLClickShift and ( not ShiftWasDown )) then begin
            if KNTLocation then
               myURL:= '(KNT) ' + KNTPathFromString(URLstr)
            else
               myURL:= URLstr;
            Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_16 + myURL;
            exit;
        end;
      end;
      }
    *)

      //-------------------------------------
      if (not EnsureAsk) and ( URLType = urlFile ) and ( myURLAction in [urlAsk] ) and KeyOptions.URLFileAuto then
           if URLFileExists(myURL) then
              myURLAction := urlOpen;


      //-------------------------------------
      if ( myURLAction = urlAsk ) then
      begin
        ActiveNote.Editor.SelLength:= 0;
        Form_URLAction := TForm_URLAction.Create( Form_Main );
        try
           if KNTlocation then begin
              path:= KNTPathFromString(myURL);
              Form_URLAction.AllowURLModification:= false;
              Form_URLAction.Edit_URL.Text := path;
           end
           else
              Form_URLAction.Edit_URL.Text := myURL;

          // Seleccionar el texto correspondiente al hipervinculo
          usesHyperlinkCmd:= true;
          TextURL:= TextOfLink(chrgURL.cpMax-1, textURLposIni, textURLposFin);
          if TextURL = '' then begin
             if length(URLstr) < Length(myURL) then
                Form_URLAction.Edit_TextURL.Text := URLstr
             else
                Form_URLAction.Edit_TextURL.Text := Form_URLAction.Edit_URL.Text;
             usesHyperlinkCmd:= false;
             end
          else
             Form_URLAction.Edit_TextURL.Text := TextURL;

          Form_URLAction.URLAction:= urlOpen;   // Default action
          Form_URLAction.Button_OpenNew.Enabled := ( URLType in [urlHTTP, urlHTTPS] );
          if ( Form_URLAction.ShowModal = mrOK ) then begin
              myURLAction := Form_URLAction.URLAction;
              TextURL:= trim(Form_URLAction.Edit_TextURL.Text);
              if not KNTlocation then begin                  // If it was a KNT Location then URL will not be modified
                 myURL := trim( Form_URLAction.Edit_URL.Text );
                 URLType := TypeURL( myURL, KNTlocation );    // The type could have been modified
              end;
          end
          else
             myURLAction := urlNothing;
        finally
          Form_URLAction.Free;
        end;
      end;

      //-------------------------------------
      if ( myURLAction = urlCreateOrModify ) then begin
          if TextURL = '' then TextURL := myURL;
          if usesHyperlinkCmd then
             ActiveNote.Editor.SetSelection(chrgURL.cpMin -11, textURLposFin +1, false)    // -11: HYPERLINK "
          else
             ActiveNote.Editor.SetSelection(chrgURL.cpMin, chrgURL.cpMax, false);

          ActiveNote.Editor.SelText:= '';
          if KNTLocation then begin
             Location:= BuildKNTLocationFromString(myURL);
             InsertOrMarkKNTLink(Location, true, TextURL);
          end
          else
             InsertURL(myURL, TextURL, ActiveNote);

          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_17;
          exit;
      end;

      //-------------------------------------
      if ( myURLAction = urlNothing ) then begin
         Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_18;
         exit;
      end;

      if ( myURLAction in [urlCopy, urlBoth] ) then begin
          if KNTLocation then
             Clipboard.AsText:= URLstr      // includes file prefix
          else
             Clipboard.AsText:= myURL;

          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_19;
      end;

      //-------------------------------------
      // urlOpenNew is only for HTTP and HTTPS protocols
      if ( not ( URLType in [urlHTTP, urlHTTPS] )) and ( myURLAction = urlOpenNew ) then
           myURLAction := urlOpen;


      //-------------------------------------
      if ( myURLAction in [urlOpen, urlOpenNew, urlBoth] ) then begin

          Parameters:= '';

          case URLType of
            urlFILE : begin // it may be a KNT location or a normal file URL.
              if KNTlocation then begin
                // KNT location!
                _GLOBAL_URLText := myURL;
                  { Why "postmessage" and not a regular procedure?
                  Because we are, here, inside an event that belongs to the TTabRichEdit control. When a link is clicked,
                  it may cause KeyNote to close this file and open a different .KNT file. In the process, this TTabRichEdit
                  will be destroyed. If we called a normal procedure from here, we would then RETURN HERE: to an event handler
                  belonging to a control that NO LONGER EXISTS. Which results in a nice little crash. By posting a message,
                  we change the sequence, so that the file will be closed and a new file opened after we have already
                  returned from this here event handler. }
                postmessage( Form_Main.Handle, WM_JumpToKNTLink, 0, 0 );
                exit;
              end
              else begin
                myURL:= GetAbsolutePath(ExtractFilePath(NoteFile.FileName), myURL);
                FileName:= myURL;
              end;
            end;
            else begin                              // all other URL types
                FileName:= myURL;                                                             // and by default: Parameters:= ''
                if URLType in [urlHTTP, urlHTTPS] then begin
                    browser := GetHTTPClient();
                    if browser <> '' then begin
                       FileName:= browser;
                       Parameters:= myURL;
                       if ( myURLAction = urlOpenNew ) then
                          Parameters:= '--new-window ' + Parameters;
                    end;
                end;
            end;
          end;


          screen.Cursor := crAppStart;
          try
              ShellExecResult := ShellExecute( 0, 'open', PChar(FileName), PChar(Parameters), nil, SW_NORMAL );
          finally
              screen.Cursor := crDefault;
          end;


          if ( ShellExecResult <= 32 ) then begin
            if (( ShellExecResult > 2 ) or KeyOptions.ShellExecuteShowAllErrors ) then
              PopupMessage( Format(
                STR_20,
                [ShellExecResult, myURL, TranslateShellExecuteError(ShellExecResult)] ), mtError, [mbOK], 0 );
          end
          else begin
            if KeyOptions.MinimizeOnURL then
               Application.Minimize;
          end;
      end;

    except
      on E : Exception do
        DoMessageBox( E.Message, mtWarning, [mbOK] );
    end;

  finally
    _IS_FAKING_MOUSECLICK := false;
  end;

end; // ClickOnURL


procedure ClickOnURLImage(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false);
var
  p1, ImgID: integer;

begin
   p1:= pos(',', URLstr, 5);
   ImgID  := StrToIntDef(Copy(URLstr, 5, p1- 5), 0);

   if ImgID <> 0 then
      ImagesManager.OpenImageViewer(ImgID, myURLAction=urlOpenNew, true);
end;


//--------------------------------------------------
// PathFileOK
//--------------------------------------------------
function PathFileOK (const FN: string): boolean;
var
   charPos : integer; // position at which the actual URL starts in URLText
   i : integer;
begin
  result:= true;
  for i := low( INVALID_CHARS_FN ) to high( INVALID_CHARS_FN ) do
  begin
    charPos := pos( INVALID_CHARS_FN[i], FN );
    if ( charPos > 0 ) then
    begin
      result:= false;
      break;
    end;
  end;
end;


//===============================================================
// InsertURL
//===============================================================
procedure InsertURL(URLStr: string; TextURL : string; Note: TTabNote);
var
  URLType : TKNTURL;
  Form_URLAction: TForm_URLAction;
  askUser: Boolean;
  KNTLocation: boolean;
  SS, SL: integer;

  procedure RemoveAngleBrackets(var Cad : string);
  var
    l, r, n: integer;
  begin
    n:= Length(Cad);
    l:= 1;
    while (l <= n) and ( Cad[l] = '<' ) do
        l:= l + 1;

    r:= n;
    while (r >= 1) and ( Cad[r] = '>' ) do
        r:= r - 1;

    Cad:= Copy(Cad, l, r-l+1);
  end;


  procedure SelectTextToUse();
  var
      UrlSel, TxtSel: string;
      p: integer;
      L, R: integer;
  begin
        if Note.Editor.SelLength > 0 then begin
           TxtSel:= Trim(Note.Editor.SelVisibleText);
           UrlSel:= Trim(Note.Editor.SelText);
           RemoveAngleBrackets(TxtSel);
           RemoveAngleBrackets(UrlSel);

           p:= Pos('HYPERLINK "', UrlSel);
           if p > 0 then begin
              URLStr:= Copy(UrlSel, p+11, Length(UrlSel) -Length(TxtSel) -12);
              URLType:= TypeURL( UrlStr, KNTlocation);
              if TextURL = '' then TextURL:= TxtSel;
              end
           else begin
              UrlSel:= TxtSel;
              URLType:= TypeURL( UrlSel, KNTlocation);
              if (URLType <> urlFile) or ( (pos('FILE:', AnsiUpperCase(TxtSel))=1) or (pos(':', UrlSel)=2) or URLFileExists(UrlSel)) then
                 URLStr:= UrlSel
              else begin
                 URLType:= urlUndefined;
                 if TextURL = '' then TextURL:= TxtSel;  // URLStr will remain ""
              end;
           end;
        end
        else begin
           Note.Editor.GetLinkAtCursor(URLStr, TxtSel, L, R);
           URLType:= TypeURL( UrlStr, KNTlocation);
           if TextURL = '' then TextURL:= TxtSel;
        end;

        if (TextURL = '') and (URLType = urlFile) then
           TextURL:= StripFileURLPrefix(URLStr);
  end;

begin
  if ( not ( Form_Main.HaveNotes( true, true ) and assigned( Note ))) then exit;
  if Form_Main.NoteIsReadOnly( Note, true ) then exit;
  askUser:= (URLStr = '');


  if askUser then begin
      SS:= ActiveNote.Editor.SelStart;
      SL:= ActiveNote.Editor.SelLength;
      SelectTextToUse;
      if URLType = urlKNTImage then begin
         ActiveNote.Editor.SetSelection(SS, SS+SL, true);
         exit;
      end;

      Form_URLAction := TForm_URLAction.Create( Form_Main );
      try
        Form_URLAction.Edit_URL.Text := URLStr;
        Form_URLAction.Edit_TextURL.Text := TextURL;
        Form_URLAction.URLAction:= urlCreateOrModify;   // Mode: Create. Only will show buttons Ok and Cancel

        if ( Form_URLAction.ShowModal = mrOK ) then
        begin
            URLStr := trim( Form_URLAction.Edit_URL.Text );
            TextURL:= trim( Form_URLAction.Edit_TextURL.Text );
        end
        else
            URLStr := '';
      finally
        Form_URLAction.Free;
      end;
  end;

  if URLStr <> '' then
  begin
    // Determine type of URL. Parameter of TypeURL can also be modified
      URLType := TypeURL( URLStr, KNTLocation );
      if (URLType = urlFile) and ( pos( 'FILE:', AnsiUpperCase(URLStr) ) = 0 ) then
         URLStr := 'file:///' + URLStr;

      if (TextURL = '') and (not Note.PlainText) then TextURL:= StripFileURLPrefix(URLStr);
      InsertHyperlink(URLStr, TextURL, false, Note);
  end;

end; // Insert URL


//=========================================
// AddHistoryLocation
//=========================================
procedure AddHistoryLocation( const aNote : TTabNote; const AddLocalMaintainingIndex: boolean;
                              aLocation: TLocation= nil;
                              const AddToGlobalHistory: boolean= true);
var
  gLocation: TLocation;

begin
  if not assigned(aNote) then exit;

  try
    if aLocation = nil then
       GetKNTLocation (aLocation, true);                       // true: simplified (register only IDs)
    aLocation.SelLength := 0;

    if AddToGlobalHistory then begin
      gLocation:= aLocation.Clone();
      History.AddLocation( gLocation );                        // History: global navigation history
    end;

    if AddLocalMaintainingIndex  then
       aNote.History.AddLocationMaintainingIndex (aLocation)
    else
       aNote.History.AddLocation(aLocation);


  except
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_21;
    aNote.History.Clear;
    History.Clear;
    if assigned(aLocation) then
        aLocation.Free;
  end;

end; // AddHistoryLocation



//=========================================
// NavigateInHistory
//=========================================

{
	Redesigned navigation history mechanism              [dpv] (jul.2023)

    - Default navigation is now global and so we can move between the nodes of a note but also between notes
    - Local navigation (traveling only active note's history) is also possible, clicking on the toolbar button with Ctrl.
    - Every jump from an internal keynote link will generate history, not only those that go to another node in the same
      note.
    - Bookmark jumps will also generate history
    - Standard RTF notes (not multi-lvel tree notes) will have its own history navigation
    - Related buttons have been moved to main toolbar.
      their color and hint indicates if global and/or local history is available in that direction


    When we are travelling the global history, the local history in the active note will be navigated accordingly,
    in a synchronized way, if possible, looking for an equal or equivalent (same node) item. The same will occur if the
    local history is leading the history navigation: global history will be synchronized, but in this case in a limited
    way, only if can go to the same item in the same direction and in one step.

    As usual, if we navigate backwards in history and then create new history (we are jumping or selecting another location,
    not navigating forward again) the history from the point forward will be truncated, replaced by the new history.
    This will happen always	in the global history, but in the local (note) history only if we jump to another location in
    the same note. If we have navigated backwards in a local history and then select another note, global history will be
    truncated accordingly but the history in the starting note will be kept intact. When we are back again in that note
    (because of history navigation or not) we can move only in that local history if we want.
    We can move inside a note (creating new history), select another note and move there, creating also new history.
    If we navigate backwards from that point we will end up in the starting note, but we can also select directly that initial
    note and navigate its history (with ctrl+click) directly.

    The color of the arrows in the toolbar buttons will indicate if the forward o backwards navigation (in global history
    by default) will end up in another note (intense blue) or in the same note (light blue). Even if color is intense blue,
    we could navigate in that direction to an item in the same note, when clicking with Ctrl key pressed (if local history allows it).
    If we reach one end of global history navigation but there is local history in that direction, the color of the button will be
    light blue and clicking in that button will be managed as if we pressed Ctrl+Click.
    The hint in the buttons will indicate what kind of history navigation is available in each direction (global, local or both)
}

procedure NavigateInHistory( const Direction: THistoryDirection);
var
  myLocation, LocBeforeNavigation : TLocation;
  masterHistory, slaveHistory : TKNTHistory;
  IterateAllOnSync, MaintainIndexInLocalHist: boolean;

  procedure UpdateIfNil(var LocToUpdate: TLocation; const newLocation: TLocation);
  begin
      if LocToUpdate <> nil then exit;
      LocToUpdate:= newLocation;
  end;

begin
  if not assigned(ActiveNote) then exit;

{$IFDEF DEBUG_HISTORY}
  if NoteFile <> nil then begin
    if not _LastMoveWasHistory then
        Form_Main.Res_RTF.Text:= '--------     ' + #13 + 'Last move was history: NO' + #13#13 + Form_Main.Res_RTF.Text;
  end;
{$ENDIF}

  LocBeforeNavigation:= nil;
  GetKntLocation (LocBeforeNavigation, true);


  if CtrlDown and ( not _IS_FAKING_MOUSECLICK ) then begin
     masterHistory:= ActiveNote.History;
     slaveHistory := History;
     IterateAllOnSync:= false;
  end
  else begin
     masterHistory:= History;
     slaveHistory := ActiveNote.History;
     IterateAllOnSync:= true;
  end;

  { *1
    myLocation could be nil => The arrow would be showing light blue because it is possible to go back in the local history,
    but without global history in that direction. In that case it is not necessary to press CTRl+Click.
    If we do not get any element with the 'master' history (depending on whether or not CTRL has been pressed), we will get it in the 'slave' history
    In some of them we must get a location value, or else the button would have been disabled from UpdateHistoryCommands

   *2:
    If myLocation=nil because we have not been able to advance having pressed CTRL (therefore in the local history), we will not do anything. 
    We'll go out and that's it.  If you've pressed CTRL you don't want to got to another note.
  }

  try
    if Direction = hdForward then begin
       myLocation := masterHistory.GoForward;
       if (myLocation = nil) then
          if (masterHistory <> History)  then   // *2
             Exit
          else begin
             AddHistoryLocation(ActiveNote, True);
             History.GoBack;
          end;
       UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdForward, IterateAllOnSync));
    end
    else begin
       if ( not _LastMoveWasHistory ) then begin
         MaintainIndexInLocalHist:= false;
         myLocation:= masterHistory.PickBack;
         if (myLocation = nil) or (myLocation.NoteID <> ActiveNote.ID) then
            MaintainIndexInLocalHist:= True;
         AddHistoryLocation(ActiveNote, MaintainIndexInLocalHist);
         myLocation := masterHistory.GoBack;
         if not MaintainIndexInLocalHist and (slaveHistory <> nil) then
            slaveHistory.GoBack;
       end;
       myLocation := masterHistory.GoBack;
       if (myLocation = nil) and (masterHistory<>History)  then          // *2   *1
           exit;
       UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdBack, IterateAllOnSync));   // *1
    end;

    try
      _Executing_History_Jump := true;
      if not ( assigned( myLocation ) and (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation(myLocation) ) then begin
        if Direction = hdForward then
            while masterHistory.CanGoForward do begin
              myLocation := masterHistory.GoForward;
              UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdForward, IterateAllOnSync));
              if (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation( myLocation ) then
                 break;
            end
        else
            while masterHistory.CanGoBack do begin
              myLocation := masterHistory.GoBack;
              UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdBack, IterateAllOnSync));
              if (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation( myLocation ) then
                 break;
            end;
      end
      else
         Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_22;

    finally
      _Executing_History_Jump := false;
      _LastMoveWasHistory := true;
      UpdateHistoryCommands;
    end;
  except
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_23;
    masterHistory.Clear;
    if slaveHistory <> nil then
       slaveHistory.Clear;
  end;

end; // NavigateInHistory


//=========================================
// UpdateHistoryCommands
//=========================================
procedure UpdateHistoryCommands;
var
  lHistory: TkntHistory;
  Loc: TLocation;
  GoBackEnabled, GoForwardEnabled: Boolean;
  strHint: string;

{$IFDEF DEBUG_HISTORY}
  i: integer;
  str: string;
{$ENDIF}
begin
  lHistory:= nil;
  if assigned(activenote) then
     lHistory:= ActiveNote.History;

  with Form_Main do begin
    GoBackEnabled :=        assigned(ActiveNote) and ( History.CanGoBack or ((lHistory <> nil) and lHistory.CanGoBack) );
    MMTreeGoBack.Enabled := GoBackEnabled;
    TB_GoBack.Enabled := GoBackEnabled;
    strHint:= STR_24;
    if GoBackEnabled then begin
       Loc:= History.PickBack;
       if (Loc <> nil) and (Loc.NoteID <> ActiveNote.ID ) then
          TB_GoBack.ImageIndex:= IMAGE_GOBACK_OTHER_NOTE
       else
          TB_GoBack.ImageIndex:= IMAGE_GOBACK_IN_NOTE;

       if not History.CanGoBack then
          strHint:= STR_25
       else begin
          strHint:= STR_26; //'Navigate backwards in global history';
          if (lHistory <> nil) and lHistory.CanGoBack and (TB_GoBack.ImageIndex= IMAGE_GOBACK_OTHER_NOTE) then
             strHint:= strHint + STR_30;
       end;
    end;
    TB_GoBack.Hint:= strHint;

    GoForwardEnabled:= assigned(ActiveNote) and (History.CanGoForward or ((lHistory <> nil) and lHistory.CanGoForward) );
    MMTreeGoForward.Enabled := GoForwardEnabled;
    TB_GoForward.Enabled := GoForwardEnabled;
    strHint:= STR_27;
    if GoForwardEnabled then begin
       Loc:= History.PickForward;
       if (Loc <> nil) and (Loc.NoteID <> ActiveNote.ID ) then
          TB_GoForward.ImageIndex:= IMAGE_GOFORWARD_OTHER_NOTE
       else
          TB_GoForward.ImageIndex:= IMAGE_GOFORWARD_IN_NOTE;

       if not History.CanGoForward then
          strHint:= STR_28
       else begin
          strHint:= STR_29;
          if (lHistory <> nil) and lHistory.CanGoForward and (TB_GoForward.ImageIndex= IMAGE_GOFORWARD_OTHER_NOTE) then
             strHint:= strHint + STR_30;
       end;
    end;
    TB_GoForward.Hint:= strHint;

  end;

{$IFDEF DEBUG_HISTORY}
  if NoteFile <> nil then begin
    str:= '--------     ' + #13;
    str:= str + 'Global: ' + History.Summary + #13;
    for i := 0 to NoteFile.Notes.Count-1 do begin
       str:= str + NoteFile.Notes[i].Name + ': ' + NoteFile.Notes[i].History.Summary + #13;
    end;
    Form_Main.Res_RTF.Text:= str + #13 + Form_Main.Res_RTF.Text;
  end;
{$ENDIF}

end; // UpdateHistoryCommands


(*
procedure TForm_Main.InsertHyperlink(
  const aLinkType : TLinkType;
  aLinkText, aLinkTarget : string;
  const aLocation : TLocation );
var
  InitialSelStart : integer;
  TextLen, TargetLen : integer;
  Location : TLocation;
begin
  { Inserts a hyperlink in active note.
    Only RichEdit v.3 supports .Link and .Hidden properties.
    For RichEdit 2, we can only display full link address,
    and cannot use not or node IDs
  }

  { Syntax of the "KeyNote location" link:
    1) OLD STYLE (used in KeyNote versions earlier than 1.1
       and still used when RichEditVersion < 2)

       (a)
       file:///filename.knt?NoteName|NodeName|CaretPos|SelLength

       Filename.knt may be blank if links points to current file.
       Only NoteName is required.
       The '?' character is invalid in filenames, so it tells us
       this is a hyperlink to a KeyNote location, not a normal
       link to local file.

       The problem with this scheme is that it fails when there
       is more than one note (or tree node) by the same name.

       (b)
       Begining with version 1.1, we use note and node IDs instead:
       file:///filename.knt|NoteID|NodeID|CaretPos|SelLength
       The '|' character has the same function as '?' in (a),
       (also invalid in filenames), but it also tells us this
       is the new type of link, using Note IDs rather than names.
       However, we can only use this methof with RichEdit v. 3
       (see below) because it allows us to have an arbitrary description
       assigned to an URL. In RichEdit v. 2 we can only display the
       URL itself, so we must use note and node names rather than IDs,
       so as to have meaningful links (otherwise, we'd have links
       such as "file:///filename.knt?23|45" which are meaningless).

    2) NEW STYLE, used only if RichEditVersion >= 3

       file:///filename.knt*NoteID|NodeID|CaretPos|SelLength

       (the '*' replaces the '?' and indicates new format)

       This link format uses unique note and node IDs.
       The URL is actually hidden and the displayed link
       is any user-defined text.

       If no link text is specified, it is generated automatically.

       The address part is formatted using hidden text and follows
       the link description:

       Yahoo websitehttp://www.yahoo.com
       +--------------------------------+ has .Link property
                    +-------------------+ has .Hidden property
       This way, when the link is clicked, the full text of the link
       will be passed to OnURLClick handler. Note that there is no
       separator character that divides the link text from link URL,
       because in the editor only the text link is displayed, and
       the user may type in any character she wishes to. (We could use
       an nprintable character, though?)

    Notes: all strings are URL-encoded: filename.knt,
    as well as note name and node name.

  }
  {
    LocationString := 'file:///' + LocationString + '?' +
      FileNameToURL( _KNTLocation.NoteName ) + KNTLINK_SEPARATOR +
      FileNameToURL( _KNTLocation.NodeName ) + KNTLINK_SEPARATOR +
      inttostr( _KNTLocation.CaretPos ) + KNTLINK_SEPARATOR +
      inttostr( _KNTLocation.SelLength );
  }

  if ( not assigned( ActiveNote )) then exit;

  InitialSelStart := ActiveNote.Editor.SelStart;

  ActiveNote.Editor.Lines.BeginUpdate;
  try
    if (( _LoadedRichEditVersion > 2 ) and KeyOptions.UseNewStyleURL ) then
    begin
      // use new URL syntax
      case aLinkType of
        lnkURL : begin
          if ( aLinkText = '' ) then
            aLinkText := aLinkTarget;
          aLinkTarget := FileNameToURL( aLinkTarget );
        end;
        lnkEmail : begin
          if ( aLinkText = '' ) then
          begin
            aLinkText := aLinkTarget;
            if ( pos( 'mailto:', aLinkText ) = 1 ) then
              delete( aLinktext, 1, 7 );
          end;
          aLinkTarget := FileNameToURL( aLinkTarget );
        end;
        lnkFile : begin
          if ( aLinkText = '' ) then
            aLinkText := ExtractFilename( aLinkTarget );
          aLinkTarget := FileNameToURL( aLinkTarget );
        end;
        lnkKNT : begin

        end;
      end; // case

      // format the hyperlink text using .Link and .Hidden properties
      TextLen := length( aLinkText );
      TargetLen := length( aLinkTarget );

      ActiveNote.Editor.SelText := aLinkText+aLinkTarget+#32;
      ActiveNote.Editor.SelAttributes.Link := false; // in case we were in link already
      ActiveNote.Editor.SelAttributes.Hidden := false; // in case we were in hidden font already

      // select whole thing and mark as link, excluding the final space
      ActiveNote.Editor.SelLength := TextLen+TargetLen;
      ActiveNote.Editor.SelAttributes.Link := true;

      // now select the LinkTarget part and mark it as hidden
      ActiveNote.Editor.SelStart := InitialSelStart + TextLen;
      ActiveNote.Editor.SelLength := TargetLen;
      ActiveNote.Editor.SelAttributes.Hidden := true;

      // clear any selection
      ActiveNote.Editor.SelStart := InitialSelStart;
      ActiveNote.Editor.SelLength := 0;

    end
    else
    begin
      // use old URL syntax
      case aLinkType of
        lnkKNT : begin
        end
        else
        begin
          ActiveNote.Editor.SelText := FileNameToURL( aLinkTarget ) + #32;
          ActiveNote.Editor.SelLength := 0;
        end;
      end;
    end;

  finally
    ActiveNote.Editor.Lines.EndUpdate;
    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );
  end;


end; // InsertHyperlink
*)

(*

procedure TForm_Main.CreateHyperlink;
var
  Form_Hyperlink : TForm_Hyperlink;
  s : string;
begin
  if ( not assigned( ActiveNote )) then exit;
  if NoteIsReadOnly( ActiveNote, true ) then exit;

  Form_Hyperlink := TForm_Hyperlink.Create( self );
  try

    Form_Hyperlink.LinkText := ActiveNote.Editor.SelText;
    Form_Hyperlink.Edit_Text.Enabled := (( _LoadedRichEditVersion > 2 ) and KeyOptions.UseNewStyleURL );
    Form_Hyperlink.LB_Text.Enabled := Form_Hyperlink.Edit_Text.Enabled;

    if ( Form_Hyperlink.ShowModal = mrOK ) then
    begin
      { New syntax for hyperlinks - requires RichEdit v. 3
      hyperlinks in RTF text are formatted as follows:
      <LINK>Link title<HIDDEN>target address</HIDDEN></LINK>
      "Link" and "Hidden" are properties of RxRichEdit.SelAttributes,
      i.e. ActiveNote.Editor.SelAttributes.
      That way, when the link is clicked, the OnURLClick event handler
      gives us the complete text of the link. We'll then have to search
      for the protocol identifier, e.g. http://, mailto:, file:///, etc.
      }

      with Form_Hyperlink do
      begin

        s := lowercase( LinkTarget );
        case LinkType of
          lnkURL : begin
            // test the URL, esp. see if it has a scheme prefix
            if ( pos( ':/', LinkTarget ) = 0 ) then
            begin
              if ( pos( 'ftp', LinkTarget ) = 1 ) then
                LinkTarget := 'ftp://' + LinkTarget
              else
                LinkTarget := 'http://' + LinkTarget; // [x] very simplistic
            end;
          end;
          lnkEmail : begin
            if ( pos( 'mailto:', s ) <> 1 ) then
              LinkTarget := 'mailto:' + LinkTarget;
          end;
          lnkFile : begin
            // may be a file or a folder
            if fileexists( LinkTarget ) then
            begin
              LinkTarget := NormalFN( LinkTarget );
            end
            else
            if DirectoryExists( Linktarget ) then
            begin
              LinkTarget := ProperFolderName( LinkTarget );
            end
            else
            begin
              // not a file and not a folder, must be an error
              MessageDlg( 'No file or folder by the specified name exists: ' + LinkTarget, mtError, [mbOK], 0 );
              exit;
            end;
            LinkTarget := 'file:///' + LinkTarget;
          end;
          lnkKNT : begin
            // we do not use LinkTarget here. Instead, we use the
            // location that was last marked and stored in _KntLocation.
          end;
        end;

        InsertHyperlink( LinkType, LinkText, LinkTarget, _KNTLocation );

      end;
    end;
  finally
    Form_Hyperlink.Free;
  end;
end; // CreateHyperlink
*)

Initialization
   _Executing_History_Jump := false;
   _LastMoveWasHistory := false;


end.
