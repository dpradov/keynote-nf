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

   TreeNT,

   kn_Info,
   kn_Const,
   kn_KntFolder,
   kn_History,
   kn_LocationObj,
   knt.ui.editor
   ;


   // Links related routines
    procedure GetKNTLocation (const aFolder : TKntFolder; var Location: TLocation; Simplified: Boolean= false);
    procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean; Relative: boolean= false );
    procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean ; TextURL: string; NumBookmark09: integer= 0);
    function BuildKNTLocationText( const aLocation : TLocation) : string;
    function BuildKNTLocationFromString( LocationStr : string ): TLocation;
    function ConvertKNTLinksToNewFormat(const Buffer: Pointer; BufSize: integer): AnsiString;
    function BuildBookmark09FromString( LocationStr : AnsiString ): TLocation;
    procedure JumpToKNTLocation( LocationStr : string; myURLAction: TURLAction = urlOpen; OpenInCurrentFile: boolean= false);
    function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true; AdjustVisiblePosition: boolean = true;
                              myURLAction: TURLAction = urlOpen;
                              OpenInCurrentFile: boolean= false;
                              ConsiderOffset: boolean = false ): boolean;
    procedure OpenLocationInOtherInstance( aLocation : TLocation );
    function SearchCaretPos (Editor: TKntRichEdit; myTreeNode: TTreeNTNode;
                             CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean;
                             ScrollPosInEditor: TPoint;
                             AdjustVisiblePosition: boolean = true;
                             ContainsRegImages: boolean = true;
                             ConsiderOffset: boolean = false): integer;
    function PositionInImLinkTextPlain (myFolder: TKntFolder; myTreeNode: TTreeNTNode; CaretPosition: integer; ForceCalc: boolean = false): integer;

    procedure ClickOnURL(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false);
    procedure InsertURL(URLStr : string; TextURL : string; Editor: TKntRichEdit);

    function PathOfKNTLink (myTreeNode: TTreeNTNode; myFolder : TKntFolder; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
                            forUseInFindResults: boolean = false): string;
    procedure GetTreeNodeFromLocation (const Location: TLocation; var Folder: TKntFolder; var myTreeNode: TTreeNTNode);

    procedure NavigateToTreeNode(myTreeNode: TTreeNTNode);

    // Navigation history
    procedure AddHistoryLocation( const aFolder : TKntFolder; const AddLocalMaintainingIndex: boolean;
                                  aLocation: TLocation= nil; const AddToGlobalHistory: boolean= true);
    procedure NavigateInHistory( const Direction: THistoryDirection);
    procedure UpdateHistoryCommands;

    function TypeURL (var URLText: string; var KNTlocation: boolean): TKntURL;
    function URLFileExists (var URL: string): boolean;

    function DeleteBookmark09 (Location: TLocation): boolean;

var
   _Executing_History_Jump : boolean;
   _Executing_JumpToKNTLocation_ToOtherNote : boolean;
   _LastMoveWasHistory : boolean;


implementation
uses
   RxRichEd,

   gf_misc,
   gf_files,
   gf_strings,
   gf_streams,
   kn_Global,
   kn_Main,
   kn_URL,
   kn_KntNote,
   kn_EditorUtils,
   kn_RTFUtils,
   kn_FindReplaceMng,
   kn_ImagesMng,
   kn_ImageForm,
   kn_NoteFileMng,
   kn_TreeNoteMng,
   kn_KntFile,
   knt.App
  ;


resourcestring
  STR_01 = 'Folder ID not found: %d';
  STR_02 = 'Folder name not found: %s';
  STR_03 = 'Node ID not found: %d';
  STR_03b = 'Note GID not found: %d';
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
  STR_25 = 'Navigate backwards in folder (''local'') history';
  STR_26 = 'Navigate backwards in global history';
  STR_27 = 'Navigate forward in history';
  STR_28 = 'Navigate forward in folder (''local'') history';
  STR_29 = 'Navigate forward in global history';
  STR_30 = ' (Ctrl+click: only in folder history)';
  STR_31 = ' [Mark: %d]';
  STR_32 = '   (Undo to remove new hidden markers)';
  STR_33 = 'Action canceled';

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
function PathOfKNTLink (myTreeNode: TTreeNTNode; myFolder : TKntFolder; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
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
        path:= myFolder.Name + TreeOptions.NodeDelimiter + path
     else
        path:= path + TreeOptions.NodeDelimiter + myFolder.Name;
  end
  else
     path := myFolder.Name;


  // Hide common part of the path (common ancestors), if RelativeKNTLink=True
  if RelativeKNTLink then begin
     pathInsertionPoint:= PathOfKNTLink(GetCurrentTreeNode, ActiveFolder, -1, false, false);

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
procedure GetTreeNodeFromLocation (const Location: TLocation; var Folder: TKntFolder; var myTreeNode: TTreeNTNode);
var
   Note: TKntNote;

begin
   with Location do begin
     // obtain FOLDER
      Folder := nil;

      if NoteGID <> 0 then begin  // lfNew2 format
         ActiveFile.GetNoteByGID(NoteGID, Note, Folder);
         if Note = nil then
            raise EInvalidLocation.Create(Format(STR_03b, [NoteGID]))
      end
      else
      if (FolderID <> 0) then begin // lfNew format
         Folder := KntFile.GetFolderByID(FolderID);
         if (Folder = nil ) then
            raise EInvalidLocation.Create(Format(STR_01, [FolderID]));
      end
      else begin                    // lfOld format
         Folder := KntFile.GetFolderByName( FolderName );
         if (Folder = nil) then
            raise EInvalidLocation.Create(Format( STR_02, [FolderName] ));
      end;


      // obtain NODE
      myTreeNode := nil;

      if NoteGID <> 0 then begin
         myTreeNode := Folder.GetTreeNodeByGID(NoteGID);
         if (myTreeNode = nil) then
            raise EInvalidLocation.Create(Format( STR_03b, [NoteGID] ));
      end
      else
      if (NoteID > 0) or (NoteName <> '') then begin   // If NodeID <= 0 and NodeName = '' -> Node will be ignored
         if (NoteID <> 0) then begin    // lfNew2 or lfNew format
            myTreeNode := Folder.GetTreeNodeByID(NoteID);
            if (myTreeNode = nil) then
               raise EInvalidLocation.Create(Format( STR_03, [NoteID] ));
         end
         else begin
            myTreeNode := Folder.TV.Items.FindNode( [ffText], NoteName, nil );
            if (myTreeNode = nil) then
               raise EInvalidLocation.Create(Format( STR_04, [NoteName] ));
         end;
       end;
   end;
end;


//----------------------------------------
// Insertlink
//----------------------------------------
procedure InsertLink(URLStr: string; TextURL : string; FileLink: Boolean; Editor: TKntRichEdit; FromInsertURL: Boolean= false);
var
  SelL: integer;
  SelR: integer;
  sepL, sepR: string;
  UseHyperlink, ShowTextAndURL: boolean;
  cad: string;
begin
    UseHyperlink:= False;

    if (RichEditVersion >= 4) and (not Editor.PlainText) then
       UseHyperlink:= True;

    if Editor.SupportsRegisteredImages then begin
       if Editor.SelLength > 0 then
          Editor.CheckToSelectLeftImageHiddenMark;
    end;

    // URLFileEncodeName: Default=False   URLFilePrefNoHyp: Default=False
    if UseHyperlink and (not FromInsertURL) and FileLink and KeyOptions.URLFilePrefNoHyp then
      if ((not KeyOptions.URLFileEncodeName) and (TextURL = StripFileURLPrefix(URLStr))) or
             ((KeyOptions.URLFileEncodeName) and (TextURL = StripFileURLPrefix(FileNameToURL(URLStr)))) then
         UseHyperlink:= false;

    with Editor do begin
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
           Editor.PutRtfText(Format('{\rtf1\ansi{\colortbl ;\red0\green0\blue255;}{\fonttbl}%s{\field{\*\fldinst{HYPERLINK "'
                                           + '%s"}}{\fldrslt{\cf1\ul %s}}}%s\cf0\ulnone}',
                                           [sepL, URLToRTF(URLStr, false ), URLToRTF(TextURL, true), sepR]), true);
      end
      else begin
          if FileLink and KeyOptions.URLFileEncodeName then
             URLStr := FileNameToURL( URLStr );

          ShowTextAndURL:= (TextURL <> '') and (TextURL <> StripFileURLPrefix(URLStr));
          if pos(' ', URLStr) > 0 then
             URLStr:= '<' + URLStr + '>';
             
          if ShowTextAndURL then
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
procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean; Relative: boolean= false );
var
  FN : string;
  oldFilter : string;
  ImportFileType : TImportFileType;
  ext : string;
  RTFAux: TRxRichEdit;
  Editor: TKntRichEdit;

begin
   if not App.CheckActiveEditorNotReadOnly then exit;

   Editor:= ActiveEditor;


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
         KeyOptions.LastImportPath := ProperFolderName( ExtractFilePath( FN ));
         Relative:= AltDown;
      finally
         Form_Main.OpenDlg.Filter := oldFilter;
         Form_Main.OpenDlg.FilterIndex := 1;
      end;

   end
   else
      FN := aFileName;

   if Relative then
      FN:= ExtractRelativePath(ActiveFile.File_Path, FN);


   if AsLink then begin
      if pos( 'FILE:', AnsiUpperCase(FN) ) = 0 then
         FN := 'file:///' + FN;

      InsertLink(FN, StripFileURLPrefix(FN), true, Editor);
   end

   else begin
       ext := ExtractFileExt( FN );
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

       Editor.BeginUpdate;

       try
         try
           if Editor.SupportsRegisteredImages then begin
              if Editor.SelLength > 0 then
                 Editor.CheckToSelectLeftImageHiddenMark;
           end;

           case ImportFileType of
             itText, itHTML : begin
               Editor.SelText :=  ReadAllText(FN);       // gf_streams
               Editor.SelLength := 0;
             end;

             itRTF:
               Editor.PutRtfText(ReadAllText(FN), true);
           end;

         except
           on E : Exception do begin
              DoMessageBox( E.Message, mtError, [mbOK] );
              exit;
           end;
         end;

       finally
          Editor.EndUpdate;
       end;

   end;

end; // InsertFileOrLink


//===============================================================
// GetKNTLocation
//===============================================================
procedure GetKNTLocation (const aFolder : TKntFolder; var Location: TLocation; Simplified: Boolean= false);
var
  Note: TKntNote;
begin
{$IFDEF DEBUG_HISTORY}
   Simplified:= false;
{$ENDIF}

   if not assigned(Location) then
      Location:= TLocation.Create;

   if not assigned(aFolder) then exit;

   Note:= aFolder.SelectedNote;
   with Location do begin
      if not Simplified then begin
         FileName := normalFN( TKntFile(aFolder.KntFile).FileName );
         FolderName := aFolder.Name;
      end;
      FolderID := aFolder.ID;
      NoteName := '';
      NoteID := 0;
      NoteGID:= 0;
      if Note <> nil then begin
         NoteID := Note.ID;
         NoteGID:= Note.GID;
         if not Simplified then
            NoteName := Note.Name;
      end;
      CaretPos := aFolder.Editor.SelStart;
      SelLength := aFolder.Editor.SelLength;
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

	Until now, KNT links only included, in addition to the folder and node, the caret position. If the folder
	was edited, the absolute position we were pointing to, would have changed and therefore our link would no longer point
	to the intended position.

	When marking a KNT location (Ctr+F6) the app will now insert a small hidden bookmark next to the text
	identified as target. When inserting the link (Shift+F6) the new hyperlink will refer to the created bookmark,
	and will locate it even though all the text could have been relocated (within the same folder/node). If the marker
	were eliminated, the caret position would be used, as before.

	Note: The bookmark will only be inserted if the destination is a RTF node, other than virtual node.

	The format used internally to register these markers is of the form. Ex:

   	{\rtf1\ansi \v\'11B5\'12\v0 Target of a KNT link}
	   Once retrieved as plain text: '#$11'B5'#$12'Target of a KNT link
     B: Bookmark   "5" 5º bookmark in the folder / node.

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

	As an advantage, the new search mechanism is much faster. Once initialized [*] new plain text variables (NoteTextPlain),
  searches will now be almost instantaneous,	whereas currently each search incurs the same amount of time, which on a large file can easily
  be 5 or 6 seconds.
    [*] With TKntFile.UpdateTextPlainVariables, called by TForm_Main.TimerTimer, or on demand by RunFindAllEx (in kn_FindReplaceMng)

	Subsequently, a modification will be included to try to ensure that these initial data are obtained at times when
	the application is idle, so even the first search is instantaneous.

	It has been necessary to revise many other points in the application to ensure that these hidden characters are 'transparent'.
	For example, when copying (cutting or pasting is not necessary) to the clipboard, we must remove our possible hidden characters
	(not others like hyperlinks). It is also needed to discard those hidden characters when exporting to other files, or when creating
	templates. It has also been necessary to keep them in mind when performing operations such as changing to uppercase, lowercase, etc.

	Once these changes have been made, the management of this new hidden text format will allow the implementation of other functionalities
  in which it is necessary to mark or tag any text within a folder.

*)

procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean; TextURL: string; NumBookmark09: integer= 0);
var
   Folder: TKntFolder;
   TreeNode: TTreeNTNode;
   TargetMarker: integer;
   str, strTargetMarker: string;
   RTFMarker: AnsiString;
   KEY_Marker: Char;
   Editor: TKntRichEdit;

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
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;
  if ( aLocation = nil ) then
     aLocation := _KNTLocation;

  Editor:= ActiveEditor;

  if AsInsert then begin
    // insert link to previously marked location
    if not App.CheckActiveEditorNotReadOnly then exit;

    if aLocation.Bookmark09 then exit;
    if (aLocation.NoteGID = 0) and (aLocation.FolderID = 0) and (aLocation.FolderName = '') then begin
      showmessage( STR_08 );
      exit;
    end;

    if TextURL = '' then
       if KntFile.FileName = aLocation.FileName then begin
           GetTreeNodeFromLocation (aLocation, Folder, TreeNode);
           TextURL:= PathOfKNTLink(TreeNode, Folder, aLocation.CaretPos, false, assigned(Editor.NoteObj) and TreeOptions.RelativeKNTLinks);
       end
       else
          TextURL:= Format('%s: %s/%s %d', [ExtractFileName(aLocation.FileName),
                                                aLocation.FolderName, aLocation.NoteName,
                                                aLocation.CaretPos]);

    InsertLink(BuildKNTLocationText(aLocation),  TextURL, false, Editor);
    if aLocation.Mark <> 0 then
       strTargetMarker:= Format(STR_31, [aLocation.Mark]);
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_09 + strTargetMarker;
  end
  else begin
    // mark caret position as TLocation
    GetKNTLocation (ActiveFolder, aLocation);

    // If we are pointing to the start of a node or a folder (CaretPos = 0), we will not create any new markers. We will always aim for that position 0.
    strTargetMarker:= '';

    if (aLocation.CaretPos <> 0) and assigned(Editor) and (not Editor.PlainText) and (assigned(Editor.NoteObj)) then begin
        // Allow the mark (hidden) although Folder is ReadOnly
      if NumBookmark09 <= 0 then
         TargetMarker:= GetActualTargetMarker(Editor);             // If a marker already exists at that position, we will use it

      if TargetMarker = 0 then begin
         {$IFDEF KNT_DEBUG}Log.Add('Insert Marker for HyperLink',  4 ); {$ENDIF}
         if NumBookmark09 >= 1 then begin              // Bookmark0-9 -> [1-10]
            TargetMarker:= NumBookmark09;
            aLocation.Bookmark09:= true;
            KEY_Marker:= KNT_RTF_HIDDEN_Bookmark09;
         end
         else begin
            TargetMarker:= 1 + GetLastTargetMarker(Editor.TextPlain);
            KEY_Marker:= KNT_RTF_HIDDEN_BOOKMARK;
         end;

         //  {\rtf1\ansi {\v\'11B5\'12}};    => {\rtf1\ansi \v\'11B5\'12\v0};  // Finally they will be inserted directly with \v...\v0 (see comment *2 next to KNT_RTF_BMK_HIDDEN_MARK in kn_const.pas)
         RTFMarker:= Format('\v' + KNT_RTF_HIDDEN_MARK_L + KEY_Marker + '%d'+ KNT_RTF_HIDDEN_MARK_R + '\v0', [TargetMarker]);
         Editor.PutRtfText('{\rtf1\ansi' + RTFMarker + '}',  true);
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
function BuildKNTLocationText( const aLocation : TLocation) : string;
var
  LocationString : string;
  FolderID_WithSEP, NodeId, LocationMark: string;
begin
  if ( aLocation.FileName = normalFN( KntFile.FileName )) then
     LocationString := ''
  else
     LocationString := FileNameToURL( aLocation.FileName );

    // [x] this does not handle files on another computer, i.e.
    // we cannot do file://computername/pathname/file.knt

    if (RichEditVersion >= 4) then begin
        if aLocation.NoteGID <> 0 then begin           // New created links will use newer format, with GIDs: file:///"NoteGID|...
           LocationMark:= KNTLOCATION_MARK_NEW2;
           NodeId:= IntToStr(aLocation.NoteGID);
           FolderID_WithSEP:= '';
        end
        else begin
           LocationMark:= KNTLOCATION_MARK_NEW;
           FolderID_WithSEP:= IntToStr( aLocation.FolderID ) + KNTLINK_SEPARATOR;
           NodeId:= IntToStr( aLocation.NoteID );
        end;
    end
    else begin
        LocationMark:= KNTLOCATION_MARK_OLD;
        FolderID_WithSEP:= FileNameToURL( aLocation.FolderName )  + KNTLINK_SEPARATOR;
        NodeId:= FileNameToURL( aLocation.NoteName );
    end;

    LocationString := 'file:///' + LocationString + LocationMark +
      FolderID_WithSEP +
      NodeId + KNTLINK_SEPARATOR +
      IntToStr( aLocation.CaretPos ) + KNTLINK_SEPARATOR +
      IntToStr( aLocation.SelLength );

    if aLocation.Mark > 0 then
       LocationString := LocationString + KNTLINK_SEPARATOR + IntToStr( aLocation.Mark);

  result := LocationString;
end; // BuildKNTLocationText



//---------------------------------------------------------------
// BuildKNTLocationFromString
//---------------------------------------------------------------
function BuildKNTLocationFromString( LocationStr : string ): TLocation;
var
  p, pold, pnew, pnew2, pMin : integer;
  Location : TLocation;
  FormatLink: TKntLinkFormat;
  origLocationStr : string;
  Folder: TKntFolder;
  Note: TKntNote;
  myTreeNode: TTreeNTNode;
  str: string;
begin

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive three types of links:
  // the old style link:          file:///?filename.knt...
  // the last recent style link:  file:///*filename.knt...
  // the newer style link:        file:///<filename.knt...

    p := 0;
    origLocationStr := LocationStr;

    Location := TLocation.Create;

    LocationStr := StripFileURLPrefix( LocationStr );

    // see which marker occurs FIRST
    // (both markers may occur, because '?', '*' and '<' may occur within folder or node names

    FormatLink:= lfNone;
    pMin:= 99999;
    p:= pos( KNTLOCATION_MARK_OLD, LocationStr );
    if p > 0 then begin
       pMin:= p;
       FormatLink:= lfOld;
    end;
    p:= pos( KNTLOCATION_MARK_NEW, LocationStr );
    if (p > 0) and (p < pMin) then begin
       pMin:= p;
       FormatLink:= lfNew;
    end;
    p:= pos( KNTLOCATION_MARK_NEW2, LocationStr );
    if (p > 0) and (p < pMin) then begin
       pMin:= p;
       FormatLink:= lfNew2;
    end;

    // extract filename
    case pMin of
      0 : raise EInvalidLocation.Create( origLocationStr );
      1 : Location.FileName := ''; // same file as current
      else begin
        Location.FileName := HTTPDecode( copy( LocationStr, 1, pMin-1));
        if ( Location.FileName = KntFile.FileName ) then
          Location.FileName := '';
      end;
    end;
    delete( LocationStr, 1, pMin ); // delete filename and ?, * or < marker


    if FormatLink <> lfNew2 then begin
       // extract folder name or ID
       p := pos( KNTLINK_SEPARATOR, LocationStr );
       if p= 1 then
          raise EInvalidLocation.Create( origLocationStr );
       if p= 0 then
          p:= 999;

       str:= copy(LocationStr, 1, p-1);
       if FormatLink = lfNew then
          Location.FolderID := StrToUInt(str)
       else
          Location.FolderName := HTTPDecode(str);
       delete( LocationStr, 1, p );

       if Location.FolderID <> 0 then
          Folder := ActiveFile.GetFolderByID (Location.FolderID)
       else
          Folder := ActiveFile.GetFolderByName (Location.FolderName);

       if assigned(Folder) then begin
          Location.FolderID:= Folder.ID;
          Location.FolderName:= Folder.Name;
       end;
    end;

    p := pos( KNTLINK_SEPARATOR, LocationStr );
    if p= 1 then begin
       Location.NoteName := '';
       Location.NoteID := 0;
       Location.NoteGID := 0;
    end
    else begin
       if p= 0 then
          p:= 999;
       str:= copy(LocationStr, 1, p-1);
       case FormatLink of
          lfOld:  Location.NoteName := HTTPDecode(str);
          lfNew:  Location.NoteID:=  StrToUIntDef(str, 0);
          lfNew2: Location.NoteGID:= StrToUIntDef(str, 0);
       end;
    end;
    delete(LocationStr, 1, p);

    myTreeNode:= nil;
    if assigned(Folder) then begin
       if (Location.NoteID > 0) then
          myTreeNode := Folder.GetTreeNodeByID( Location.NoteID )
       else if Location.NoteName <> '' then
          myTreeNode:= Folder.TV.Items.FindNode( [ffText], Location.NoteName, nil );

       if assigned(myTreeNode) then begin
          Note:= TKntNote(myTreeNode.Data);
          if assigned(Note) then begin
             Location.NoteID:= Note.ID;
             Location.NoteGID:= Note.GID;
             Location.FolderName:= Note.Name;
          end;
       end;
    end
    else
    if (Location.NoteGID > 0) then begin
       ActiveFile.GetNoteByGID(Location.NoteGID, Note, Folder);
       if Note <> nil then begin
          Location.NoteID:= Note.ID;
          Location.FolderName:= Folder.Name;
       end;
    end;


    if (LocationStr <> '') then begin
      p := pos( KNTLINK_SEPARATOR, LocationStr );
      if p <= 0 then
         p:= 99;

      Location.CaretPos := StrToIntDef(copy(LocationStr, 1, p-1), 0);
      delete( LocationStr, 1, p );
    end;

    if (LocationStr <> '') then begin
      p := pos( KNTLINK_SEPARATOR, LocationStr );
      Location.SelLength := 0;
      if (p > 0) then begin                              // selLenght|mark
         try
           Location.SelLength := StrToInt(copy(LocationStr, 1, pred(p)));
         except
         end;
         delete(LocationStr, 1, p);
         if (LocationStr <> '') then
            Location.Mark := StrToIntDef(LocationStr, 0);
      end
      else                                               // selLength
         Location.SelLength := StrToIntDef(LocationStr, 0);
    end;


    Result:= Location;

end; // BuildKNTLocationFromString


function BuildBookmark09FromString( LocationStr : AnsiString ): TLocation;
var
  Strs: TStrings;
  s: AnsiString;
  Format: TKntLinkFormat;
  L, i: integer;
begin
   // file:///*FolderID|NodeID|CursorPosition|SelectionLength|MarkID
   // Ex: file:///*8|2|4|0|1
   // or:
   // file:///<NoteGID|CursorPosition|SelectionLength|MarkID
   // Ex: file:///<23|4|0|1

   Result:= nil;
   L:= Length('file:///');
   if (Length(LocationStr) <= L + 2) or (Pos('file:///', LocationStr) <> 1) then exit;

   Format:= lfNone;

   if LocationStr[L+1] = KNTLOCATION_MARK_NEW2 then
      Format:= lfNew2
   else
   if LocationStr[L+1] = KNTLOCATION_MARK_NEW then
      Format:= lfNew
   else
      exit;

   Strs:= TStringList.Create;
   Result:= TLocation.Create;
   try
      try
         s:= Copy(LocationStr, L+2);
         SplitString(Strs, s, '|', false);
         i:= 0;
         if Format = lfNew then begin
            Result.FolderID:= StrToUInt(Strs[0]);
            Result.NoteID:= StrToUInt(Strs[1]);
         end
         else begin
            Result.NoteGID:= StrToUInt(Strs[0]);
            i:= 1;
         end;

         with Result do begin
             Bookmark09:= true;
             CaretPos:= StrToInt(Strs[2-i]);
             SelLength:= StrToInt(Strs[3-i]);
             Mark:= StrToInt(Strs[4-i]);
         end;

      except
      end;

   finally
      Strs.Free;
   end;

end;

//===============================================================
// NavigateToTreeNode
//===============================================================
procedure NavigateToTreeNode(myTreeNode: TTreeNTNode);
var
  myFolder: TKntFolder;
begin
    if assigned(myTreeNode) then begin
        myFolder:= ActiveFile.GetFolderByTreeNode(myTreeNode);
        if (myFolder <> ActiveFolder) then
           App.ActivateFolder(myFolder);

        if assigned( myTreeNode ) then begin
           myTreeNode.MakeVisible;
           myFolder.TV.Selected := myTreeNode;
        end;
    end;
end;


function GetPositionOffset (myFolder: TKntFolder; myTreeNode: TTreeNTNode; Pos_ImLinkTextPlain: integer; CaretPosition: integer; ForceCalc: boolean = false): integer;
var
  Stream: TMemoryStream;
  imLinkTextPlain: String;
  Offset: integer;
  RtfModified: boolean;
begin
   Offset:= 0;
   if (CaretPosition < 0) and (Pos_ImLinkTextPlain < 0) then exit(0);

   Stream:= nil;

   if (ImageMng.StorageMode <> smEmbRTF) and NoteSupportsRegisteredImages then begin
     imLinkTextPlain:= '';
      if assigned(myTreeNode) then begin
          if assigned( myTreeNode.Data ) then begin
             Stream:= TKntNote( myTreeNode.Data ).Stream;
             imLinkTextPlain := TKntNote( myTreeNode.Data ).NoteTextPlain;
             RtfModified := TKntNote( myTreeNode.Data ).RTFModified;
          end;
      end;

   end;

   if Stream = nil then exit(0);

   if imLinkTextPlain = '' then begin
      var RTFAux: TAuxRichEdit;
      RTFAux:= CreateAuxRichEdit;
      try
         imLinkTextPlain:= myFolder.PrepareTextPlain(myTreeNode, RTFAux);
      finally
         RTFAux.Free;
      end;
   end;



   // See notes in ImagesManager.GetPositionOffset

   if CaretPosition >= 0 then
      Offset:= ImageMng.GetPositionOffset_FromEditorTP (Stream, CaretPosition, imLinkTextPlain, RTFModified, ForceCalc)
   else
      Offset:= ImageMng.GetPositionOffset_FromImLinkTP (Stream, Pos_ImLinkTextPlain, imLinkTextPlain, RtfModified, ForceCalc);


   Result:= Offset;
end;


function SearchCaretPos (Editor: TKntRichEdit; myTreeNode: TTreeNTNode;
                         CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean;
                         ScrollPosInEditor: TPoint;
                         AdjustVisiblePosition: boolean = true;
                         ContainsRegImages: boolean = true;
                         ConsiderOffset: boolean = false): integer;
var
  Offset: integer;
  Pos_ImLinkTextPlain: integer;
  myFolder : TKntFolder;
begin
  // ContainsRegImages = True => We have not verified that there are no registered images
  // ConsiderOffset = True    => What we receive in CaretPosition is a position in imLinkTextPlain

  Offset:= 0;
  if ConsiderOffset and Editor.SupportsRegisteredImages and ContainsRegImages then begin
     Pos_ImLinkTextPlain:= CaretPosition;
     myFolder:= TKntFolder(Editor.FolderObj);
     if (myFolder <> nil) then
        Offset:= GetPositionOffset(myFolder, myTreeNode, Pos_ImLinkTextPlain, -1);
  end;

  if PlaceCaret then
     with Editor do begin
       BeginUpdate;
       try
          if CaretPosition >= 0 then
             SelStart := CaretPosition - Offset;
          if SelectionLength >= 0 then
             SelLength := SelectionLength;
          if AdjustVisiblePosition then begin
             ScrollLinesBy(80);
             Perform( EM_SCROLLCARET, 0, 0 );
          end
          else begin
             if ScrollPosInEditor.Y >= 0 then
                SetScrollPosInEditor(ScrollPosInEditor);
          end;

       finally
          EndUpdate;
       end;
     end;

  Result:= Offset;
end;

function PositionInImLinkTextPlain (myFolder: TKntFolder; myTreeNode: TTreeNTNode; CaretPosition: integer; ForceCalc: boolean = false): integer;
var
   Offset: integer;
begin
   Offset:= GetPositionOffset(myFolder, myTreeNode, -1, CaretPosition, ForceCalc);
   Result:= CaretPosition + Offset;
end;


//===============================================================
// DeleteBookmark9
//===============================================================

function DeleteBookmark09 (Location: TLocation): boolean;
var
  myFolder : TKntFolder;
  myTreeNode : TTreeNTNode;
  myNote: TKntNote;
  p, SS, SL, selLen: integer;
  TargetMark: string;
  EditorVisible: boolean;
  Stream: TMemoryStream;
  RTFText: PAnsiChar;
  RTFBookmark: AnsiString;
  NBytes: integer;
  Str: AnsiString;

begin
   Result:= false;
   if not Location.Bookmark09 or (Location.Mark < 1) or (Location.Mark > 10) then exit;

   GetTreeNodeFromLocation(Location, myFolder, myTreeNode);

   if not assigned( myFolder ) then exit;

   // If not included in some of the visible Editors -> make the change directly in the folder or node stream
   // See comment to kn_EditorUtils.RemoveKNTHiddenCharactersInRTF

  EditorVisible:= false;

  if not assigned( myTreeNode ) then exit;
  if ActiveFolder.TV.Selected = myTreeNode then
     EditorVisible:= true;

  if EditorVisible then begin
     SS:= myFolder.Editor.SelStart;
     SL:= myFolder.Editor.SelLength;

     TargetMark:=  KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_Bookmark09 + IntToStr(Location.Mark) + KNT_RTF_HIDDEN_MARK_R_CHAR;
     with myFolder.Editor do begin
        p:= FindText(TargetMark, 0, -1, [stMatchCase]);
        if p > 0 then begin
          SelStart := p;
          SelLength := Length(TargetMark);
          if SelLength > Length(TargetMark) then begin
             // If our hidden mark is next to others, it will select everything hidden. And if it is next to a HYPERLINK it will select it in its entirety
             Str:= RTFSelText;
             Str:= StringReplace(Str, Format(KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_Bookmark09 + '%d'+ KNT_RTF_HIDDEN_MARK_R, [Location.Mark]), '', []);
             SelText:= Str;
          end
          else
             SelText := '';

          Result:= true;
          if SS < p then
             myFolder.Editor.SelStart:= SS
          else
             myFolder.Editor.SelStart:= SS - Length(TargetMark);

          myFolder.Editor.SelLength:= SL;
        end;
     end;

  end
  else begin
     myNote:= TKntNote(myTreeNode.Data);
     Stream:= myNote.Stream;

     RTFText:= PAnsiChar(Stream.Memory);
     RTFBookmark:= Format(KNT_RTF_Bmk09_HIDDEN_MARK, [Location.Mark]);

     p:= PosPAnsiChar(PAnsiChar(RTFBookmark), RTFText, 1) -1;                        // See comment *2 in ImagesManager.ProcessImagesInRTF
     if p < 0 then begin
        RTFBookmark:= Format(KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_Bookmark09 + '%d'+ KNT_RTF_HIDDEN_MARK_R, [Location.Mark]);
        p:= PosPAnsiChar(PAnsiChar(RTFBookmark), RTFText, 1) -1;
     end;

     if p >= 0 then begin
        selLen:= Length(RTFBookmark);
        NBytes:= Stream.Size - p - selLen;
        Move(RTFText[p+selLen], RTFText[p], NBytes);
        Stream.Size:= Stream.Size-selLen;
        myNote.NoteTextPlain:= '';
     end;

  end;



 end;



//===============================================================
 // JumpToLocation
 //===============================================================
 function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true; AdjustVisiblePosition: boolean = true;
                          myURLAction: TURLAction = urlOpen;
                          OpenInCurrentFile: boolean= false;
                          ConsiderOffset: boolean = false): boolean;
 var
   myFolder : TKntFolder;
   myTreeNode : TTreeNTNode;
   origLocationStr : string;
   LocBeforeJump: TLocation;
   FN: string;
   ResultOpen: integer;

   function SearchTargetMark (SearchBookmark09: boolean = false): boolean;
  var
      p, selLen: integer;
      TargetMark: string;
      KeyBookmark: char;
   begin
       if SearchBookmark09 then
         KeyBookmark:= KNT_RTF_HIDDEN_Bookmark09
      else
         KeyBookmark:= KNT_RTF_HIDDEN_BOOKMARK;

      Result:= false;
      if Location.Mark > 0 then begin
        TargetMark:=  KNT_RTF_HIDDEN_MARK_L_CHAR + KeyBookmark + IntToStr(Location.Mark) + KNT_RTF_HIDDEN_MARK_R_CHAR;
        with myFolder.Editor do begin
          p:= FindText(TargetMark, 0, -1, [stMatchCase]);
          if p > 0 then begin
            BeginUpdate;
            try
               SelStart := p;
               selLen:= 0;
               if Location.SelLength > 0 then
                  selLen:= Location.SelLength + Length(TargetMark);
               SelLength := selLen;

               myFolder.Editor.ScrollLinesBy(80);
               Perform( EM_SCROLLCARET, 0, 0 );
               Result:= true;
            finally
               EndUpdate;
            end;
          end;
        end;
      end;
  end;

begin

  result := false;
  if IgnoreOtherFiles and ( not Form_Main.HaveKntFolders( false, true )) then exit;

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive thre types of links:
  // the old style link:          file:///?filename.knt...
  // the last recent style link:  file:///*filename.knt...
  // the newer style link:        file:///<filename.knt...

  try
      LocBeforeJump:= nil;
      GetKntLocation (ActiveFolder, LocBeforeJump, true);
      LocBeforeJump.ScrollPosInEditor:= ActiveFolder.Editor.GetScrollPosInEditor;

      (*
      showmessage(
        'file: ' + Location.FileName + #13 +
        'folder: ' + Location.FolderName + #13 +
        'folder id: ' + inttostr( Location.FolderID ) + #13 +
        'node: ' + Location.NodeName + #13 +
        'node id: ' + inttostr( Location.NodeID ) + #13 +
        'node gid: ' + inttostr( Location.NodeGID ) + #13 +
        inttostr( Location.CaretPos ) + ' / ' + inttostr( Location.SelLength )
      );
      *)


      // open file, if necessary
      if ( Location.FileName <> '' ) and ( Location.FileName <> KntFile.FileName ) then begin
        if IgnoreOtherFiles then
           exit;

        FN:= GetAbsolutePath(ExtractFilePath(Application.ExeName), Location.FileName);

        if FN.ToUpper <> KntFile.FileName.ToUpper then begin
           if not Initializing and not OpenInCurrentFile and (KeyOptions.ExtKNTLnkInNewInst or (myURLAction = urlOpenNew)) then begin
              OpenLocationInOtherInstance (Location);
              exit;
           end
           else begin
              ResultOpen:= 0;
              if not FileExists( FN ) then
                 ResultOpen:= -1;
              if ResultOpen = 0 then
                 ResultOpen:= KntFileOpen( FN );

              if ResultOpen <> 0 then begin
                 if ResultOpen <> -2 then begin
                    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_11;
                    raise EInvalidLocation.Create(Format( STR_12, [origLocationStr] ));
                 end
                 else begin
                    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_33;
                    exit;
                 end;
              end;

           end;
        end;

      end;

      GetTreeNodeFromLocation(Location, myFolder, myTreeNode);

      _Executing_JumpToKNTLocation_ToOtherNote:= false;
      try
         // if not current folder, switch to it
         if ( myFolder <> ActiveFolder ) then begin
           _Executing_JumpToKNTLocation_ToOtherNote:= true;
           App.ActivateFolder(myFolder);
         end;

         // myFolder = ActiveFolder

         if assigned( myTreeNode ) then begin
            // select the node
            if myFolder.TV.Selected <> myTreeNode then begin
               myTreeNode.MakeVisible;     // It could be hidden
               myFolder.TV.Selected := myTreeNode;
            end;
         end
         else
           myTreeNode:= myFolder.TV.Selected;


         result := true;

         if not SearchTargetMark (Location.Bookmark09) then
            SearchCaretPos(myFolder.Editor, myTreeNode, Location.CaretPos, Location.SelLength, True, Location.ScrollPosInEditor,
                          AdjustVisiblePosition, true, ConsiderOffset);

         myFolder.Editor.SetFocus;

      finally
         _Executing_JumpToKNTLocation_ToOtherNote:= false;
      end;


      if _Executing_History_Jump then begin
         if (LocBeforeJump.FolderID <> Location.FolderID) then
            myFolder.History.SyncWithLocation (Location, hdBack, true);
      end
      else
      if (LocBeforeJump <> nil) and
         (LocBeforeJump.FolderID = myFolder.ID) and (myFolder.SelectedNote.ID = LocBeforeJump.NoteID) then begin
          AddHistoryLocation (myFolder, false, LocBeforeJump);
          _LastMoveWasHistory:= false;
          UpdateHistoryCommands;
      end;


    except
      on E : EInvalidLocation do
        if not _Executing_History_Jump and not Location.Bookmark09 then
          DoMessageBox( Format( STR_13, [E.Message] ), mtWarning, [mbOK]);
      on E : Exception do begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_14;
        DoMessageBox( Format( STR_15, [E.Message] ), mtWarning, [mbOK]);
      end;
  end;

end; // JumpToLocation


procedure OpenLocationInOtherInstance( aLocation : TLocation );
var
  Args: string;
  exresult: integer;
begin
   //-ignSI Ignore single instance option for this call (-> Ensure the file will open in another instance and not finally in this one..)
   Args:= Format('-ignSI -jmp"%s "', [BuildKNTLocationText(aLocation)]);
   exresult := ShellExecute( 0, 'open', PChar( Application.ExeName ), PChar( Args ), nil, SW_NORMAL );
   if ( exresult <= 32 ) then
      messagedlg( TranslateShellExecuteError( exresult ), mtError, [mbOK], 0 );
end;


//===============================================================
// JumpToKNTLocation
//===============================================================
procedure JumpToKNTLocation( LocationStr : string; myURLAction: TURLAction = urlOpen; OpenInCurrentFile: boolean= false );
var
  Location : TLocation;
begin
  try
    Location:= BuildKNTLocationFromString(LocationStr);
    try
       JumpToLocation(Location, false, true, myURLAction, OpenInCurrentFile);
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
   AbsolutePath:= GetAbsolutePath(ExtractFilePath(KntFile.FileName), StripFileURLPrefix(URL));
   Result:= FileExists( AbsolutePath) or DirectoryExists( AbsolutePath );
end;


//--------------------------------------------------
// TypeURL
//--------------------------------------------------
function TypeURL (var URLText: string; var KNTlocation: boolean): TKNTURL;
var
   URLType, KntURL: TKNTURL;
   URLPos, MinURLPos : integer; // position at which the actual URL starts in URLText
   URLTextLower: string;
   URLaux, URLaux2: string;
begin
  // determine where URL address starts in URLText
  URLType := urlUndefined;
  if URLText = '' then begin
     Result:= urlUndefined;
     exit;
  end;


  // *1  Consider the following case: "https://web.archive.org/web/20171103002946/http://www.s3graphics.com/en/"

  MinURLPos:= Integer.MaxValue;
  URLTextLower:= AnsiLowerCase(URLText);
  for KntURL := low( KntURL ) to high( KntURL ) do begin
    if KntURL = urlUndefined then continue;
    URLPos := pos( KNT_URLS[KntURL], URLTextLower );
    if ( URLPos > 0 ) then begin
      if URLPos < MinURLPos then begin       // *1
         URLType := KntURL;
         MinURLPos:= URLPos;
      end;
      if URLPos = 1 then
         break;
    end;
  end;

  if ( URLType  <> urlUndefined ) then
    URLText := copy( URLText, MinURLPos, length( URLText ))

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
      if (pos(KNTLOCATION_MARK_NEW2, URLText) > 0) or (pos(KNTLOCATION_MARK_NEW, URLText) > 0) or
         (pos(KNTLOCATION_MARK_OLD,  URLText) > 0) then
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
  Editor: TKntRichEdit;
begin
   Editor:= ActiveEditor;
   if not assigned(Editor) then exit;

    _selectStart := Editor.SelStart;
    _selectionLenght := Editor.SelLength;

    Try
        // If uses {\field{\*\fldinst{HYPERLINK "hyperlink" ... ) then next char will be "", hidden
        Editor.SetSelection(endPosURL+1, endPosURL+1, false);

        If (Editor.SelText = '')
           and (Editor.GetTextRange(endPosURL+1, endPosURL+2) = '"') Then begin     // " character doesn't have Hidden mark but is treated as such
            lastPosLink := endPosURL + 1;
            pos := lastPosLink;
            TextLen:= Editor.TextLength;
            repeat
                pos := pos + 1;
                Editor.SetSelection(pos, pos, false);
                esLink:= (Editor.SelAttributes.LinkStyle = lsLink);
                If esLink Then
                    lastPosLink := pos;
            Until Not esLink or (pos > TextLen);

            If lastPosLink >= (endPosURL + 2) Then begin
                startPos := endPosURL + 2;
                endPos := lastPosLink;
                Result := Editor.GetTextRange(startPos, endPos+1);
            End;

        End;

    Finally
        Editor.SelStart:= _selectStart;
        Editor.SelLength:= _selectionLenght;
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
  SS: integer;
  Editor: TKntRichEdit;


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
    folder: TKntFolder;
    treeNode: TTreeNTNode;
    FN: string;
  begin
     Location:= BuildKNTLocationFromString(URL);
     try
       if (ActiveFile.FileName = Location.FileName) or (Location.FileName = '') then begin
           GetTreeNodeFromLocation (Location, folder, treeNode);
           Result:= PathOfKNTLink(treeNode, folder, Location.CaretPos, false, false);
       end
       else begin
          FN:= ExtractFileName(Location.FileName);
          if Location.NoteName <> '' then
             Result:= Format('%s: ?%s/%s', [FN, Location.FolderName, Location.NoteName])
          else begin
             if Location.NoteGID > 0 then
                Result:= Format('%s: <%d', [FN, Location.NoteGID])
             else
                Result:= Format('%s: *%d|%d', [FN, Location.FolderID, Location.NoteID]);
          end;

          Result:= Result + Format('|%d|%d|%d', [Location.CaretPos, Location.SelLength, Location.Mark]);
       end;
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

  Editor:= ActiveEditor;
  if not assigned(Editor) then exit;


  _GLOBAL_URLText := '';

  // Determine type of URL. Parameter of TypeURL can also be modified
  myURL := URLstr;
  URLType := TypeURL( myURL , KNTlocation);

  if (URLType = urlKNTImage) and
        (not (myURLAction in [urlNothing, urlCopy, urlCreateOrModify])
          or (KeyOptions.ImgHotTrackViewer and ImageMng.ImgViewerIsOpen ))  then begin

      if not KeyOptions.ImgHotTrackViewer then
         ClickOnURLImage (URLstr, chrgURL, myURLAction, EnsureAsk)

      else begin
         if App.ShowingImageOnTrack then
            App.ShowingImageOnTrack:= false
         else begin
           ClickOnURLImage (URLstr, chrgURL, myURLAction, EnsureAsk);
           App.ShowingImageOnTrack:= true;
         end;
      end;

      exit;
  end;


  ShellExecResult := maxint; // dummy
  usesHyperlinkCmd:= false;

  try

      //-------------------------------------
      if (not EnsureAsk) and ( URLType = urlFile ) and ( myURLAction in [urlAsk] ) and KeyOptions.URLFileAuto then
           if URLFileExists(myURL) then
              myURLAction := urlOpen;


      //-------------------------------------
      if ( myURLAction = urlAsk ) then begin
        Editor.SelLength:= 0;
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

          if KNTlocation then
             Form_URLAction.Edit_URL.Text := Format('[ %s ]  %s', [myURL, path]);

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
             Editor.SetSelection(chrgURL.cpMin -11, textURLposFin +1, false)    // -11: HYPERLINK "
          else
             Editor.SetSelection(chrgURL.cpMin, chrgURL.cpMax, false);

          Editor.SelText:= '';
          SS:= Editor.SelStart;
          if Editor.GetTextRange(SS-1, SS+1) = '<>' then begin
             Editor.SetSelection(SS-1, SS+1, false);
             Editor.SelText:= '';
          end;


          if KNTLocation then begin
             Location:= BuildKNTLocationFromString(myURL);
             InsertOrMarkKNTLink(Location, true, TextURL);
          end
          else
             InsertURL(myURL, TextURL, Editor);

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
      // urlOpenNew is only for HTTP and HTTPS protocols .... No [dpv]
      {if ( not ( URLType in [urlHTTP, urlHTTPS] )) and ( myURLAction = urlOpenNew ) then
           myURLAction := urlOpen;
      }

      //-------------------------------------
      if ( myURLAction in [urlOpen, urlOpenNew, urlBoth] ) then begin

          Parameters:= '';

          case URLType of
            urlFILE : begin // it may be a KNT location or a normal file URL.
              if KNTlocation then begin
                // KNT location!
                _GLOBAL_URLText := myURL;
                  { Why "postmessage" and not a regular procedure?
                  Because we are, here, inside an event that belongs to the TKntRichEdit control. When a link is clicked,
                  it may cause KeyNote to close this file and open a different .KNT file. In the process, this TKntRichEdit
                  will be destroyed. If we called a normal procedure from here, we would then RETURN HERE: to an event handler
                  belonging to a control that NO LONGER EXISTS. Which results in a nice little crash. By posting a message,
                  we change the sequence, so that the file will be closed and a new file opened after we have already
                  returned from this here event handler. }
                postmessage( Form_Main.Handle, WM_JumpToKNTLink, NativeInt(myURLAction), 0 );
                exit;
              end
              else begin
                UnquoteString(myURL);                      // In case URLFileQuoteSpaces=1
                myURL:= StripFileURLPrefix(myURL);         // In case URLFileNoPrefix=0
                if pos( 'file:', myURL) = 0 then begin
                   if pos(LINK_RELATIVE_SETUP, myURL) = 1 then begin
                      var i: integer:= 1;
                      if myURL[Length(LINK_RELATIVE_SETUP)+1] = '\' then i:= 2;
                      myURL:= GetAbsolutePath(ExtractFilePath(Application.ExeName), Copy(myURL, Length(LINK_RELATIVE_SETUP) + i, 999));
                   end
                   else
                      myURL:= GetAbsolutePath(ExtractFilePath(ActiveFile.FileName), myURL);
                end;
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

end; // ClickOnURL


procedure ClickOnURLImage(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false);
var
  p1, ImgID: integer;

begin
   p1:= pos(',', URLstr, 5);
   ImgID  := StrToIntDef(Copy(URLstr, 5, p1- 5), 0);

   if ImgID <> 0 then
      ImageMng.OpenImageViewer(ImgID, myURLAction=urlOpenNew, true);
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
procedure InsertURL(URLStr: string; TextURL : string; Editor: TKntRichEdit);
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
        if Editor.SelLength > 0 then begin
           TxtSel:= Trim(Editor.SelVisibleText);
           UrlSel:= Trim(Editor.SelText);
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
           Editor.GetLinkAtCursor(URLStr, TxtSel, L, R);
           URLType:= TypeURL( UrlStr, KNTlocation);
           if TextURL = '' then TextURL:= TxtSel;
        end;

        if (TextURL = '') and (URLType = urlFile) then
           TextURL:= StripFileURLPrefix(URLStr);
  end;

begin
  if not assigned(Editor) then exit;
  if Editor.ReadOnly then begin
     App.WarnEditorIsReadOnly
  end;

  askUser:= (URLStr = '');


  if askUser then begin
      SS:= Editor.SelStart;
      SL:= Editor.SelLength;
      SelectTextToUse;
      if URLType = urlKNTImage then begin
         Editor.SetSelection(SS, SS+SL, true);
         exit;
      end;

      Form_URLAction := TForm_URLAction.Create( Form_Main );
      try
        Form_URLAction.Edit_URL.Text := URLStr;
        Form_URLAction.Edit_TextURL.Text := TextURL;
        Form_URLAction.URLAction:= urlCreateOrModify;   // Mode: Create. Only will show buttons Ok and Cancel

        if ( Form_URLAction.ShowModal = mrOK ) then begin
            URLStr := trim( Form_URLAction.Edit_URL.Text );
            TextURL:= trim( Form_URLAction.Edit_TextURL.Text );
        end
        else
            URLStr := '';
      finally
        Form_URLAction.Free;
      end;
  end;

  if URLStr <> '' then begin
    // Determine type of URL. Parameter of TypeURL can also be modified
      URLType := TypeURL( URLStr, KNTLocation );
      if (URLType = urlFile) and ( pos( 'FILE:', AnsiUpperCase(URLStr) ) = 0 ) then
         URLStr := 'file:///' + URLStr;

      if (TextURL = '') and (not Editor.PlainText) then TextURL:= StripFileURLPrefix(URLStr);
      InsertLink(URLStr, TextURL, (URLType = urlFile), Editor, True);
  end;

end; // Insert URL



function ConvertKNTLinksToNewFormat(const Buffer: Pointer; BufSize: integer): AnsiString;
const
   LINK_PREFIX = '{\field{\*\fldinst{HYPERLINK ';
   KntLINK_PREFIX_1 = LINK_PREFIX + '"file:///*';
   KntLINK_PREFIX_2 = LINK_PREFIX + 'file:///*';

var
  pIn, pOut,  pLink, pLinkEnd : integer;
  RTFTextOut, NewFormat: AnsiString;
  NBytes: integer;
  LinksConverted: boolean;
  RTFText: PAnsiChar;
  i, L: integer;


  function RTFLinkToNewFormat(Buffer: Pointer; LinkOffset: integer; var PosLinkEnd: integer): string;
  var
    pf, pIni, i: integer;
    RTF: PAnsiChar;
    Location: TLocation;
    Link: AnsiString;

  begin
    Result:= '';

    try
        // RTF (+ LinkOffset): Text that starts with {\field{\*\fldinst{HYPERLINK "file:///*...   (or ...HYPERLINK file:///*...  )
        (*
             ...{\field{\*\fldinst{HYPERLINK "file:///* ..."}}{...
             ...{\field{\*\fldinst{HYPERLINK file:///*  ... }}{...
        *)

        RTF:= PAnsiChar(Buffer);

        pIni:= 1 + LinkOffset + Length(LINK_PREFIX);
        pf:= PosPAnsiChar('}}', RTF, pIni);

        PosLinkEnd:= pf+1  -1;      // Point to the last character => RTF[PosLinkEnd] = '}'
        i:= 0;
        if RTF[pIni-1] = '"' then
           i:= 1;

        Link:= Copy(RTF, pIni +i, PosLinkEnd-pIni-1 -i);
        Location:= BuildKNTLocationFromString(Link);
        try
           if Location <> nil then begin
              Result:= BuildKNTLocationText(Location) + ' }}';
              Result:= Copy(RTF, 1+LinkOffset, pIni - LinkOffset-1) + Result;
           end;
        finally
           if Location <> nil then
              Location.Free;
        end;

    except
      Result:= '';
    end;
  end;


begin
{
  Using PosPAnsiChar() instead of Pos():  See coment *2 in kn_ImagesMng.ProcessImagesInRTF
}

   pLinkEnd:= -1;
   pIn:= 1;
   pOut:= 1;
   pLink:= -99;


   Result:= '';
   LinksConverted:= false;
   RTFText:= PAnsiChar(Buffer);

   if (Length(RTFText) > BufSize) then begin
      assert(Length(RTFText) <= BufSize );
      exit;
   end;

   try
      RTFTextOut:= '';

      repeat
         if pLink = -99 then begin
            pLink:= PosPAnsiChar(KntLINK_PREFIX_1, RTFText, pIn)-1;
            L:= 1;
         end;

         if pLink = -1 then begin
            pLink:= PosPAnsiChar(KntLINK_PREFIX_2, RTFText, pIn)-1;
            L:= 2;
         end;

         if pLink >= BufSize then
            pLink := -1;

         if (pLink = -1) then
            break;

         NBytes:= pLink - pLinkEnd-1;                // Previous bytes to copy

         if RTFTextOut = '' then begin
            SetLength(RTFTextOut, BufSize);
            {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
            for var k: integer := 1 to BufSize do
               RTFTextOut[k]:= ' ';  //ZeroMemory(@RTFTextOut[1], BufSize);
            {$ENDIF}
         end;

         // Copy bytes previous to the link found
         Move(RTFText[pLinkEnd+1], RTFTextOut[pOut], NBytes);
         Inc(pOut, NBytes);

         NewFormat:= RTFLinkToNewFormat(RTFText, pLink, pLinkEnd );

         if NewFormat = '' then  begin
            if L= 1 then
               NewFormat:= KntLINK_PREFIX_1
            else
               NewFormat:= KntLINK_PREFIX_2;
            pLinkEnd:= pLink + Length(NewFormat) -1;
         end
         else begin
            SetLength(RTFTextOut, Length(RTFTextOut) + 5);     // The new format will normally take up less, but just in case we increase the length. In the end we will adjust
            if pos(KNTLOCATION_MARK_NEW2, NewFormat) > 0 then
               LinksConverted:= true;
         end;

         NBytes:= Length(NewFormat);
         Move(NewFormat[1], RTFTextOut[pOut], NBytes);
         Inc(pOut, NBytes);

         pLink:= -99;                  // We will go back to look for another one. We have already 'consumed' this one
         pIn:= pLinkEnd + 1;

      until (pLink = -1);

      if not LinksConverted then exit;

      if (RTFTextOut <> '')  then begin
         if pIn < BufSize then begin
            NBytes:= BufSize-pIn +1;
            Move(RTFText[pIn], RTFTextOut[pOut], NBytes);
            Inc(pOut, NBytes);
         end;
         SetLength(RTFTextOut, pOut-2);
      end;

      Result:= RTFTextOut;

   except
     on E: Exception do
        MessageDlg( STR_20 + E.Message, mtError, [mbOK], 0 );
   end;

end;



//=========================================
// AddHistoryLocation
//=========================================
procedure AddHistoryLocation( const aFolder : TKntFolder; const AddLocalMaintainingIndex: boolean;
                              aLocation: TLocation= nil;
                              const AddToGlobalHistory: boolean= true);
var
  gLocation: TLocation;

begin
  if not assigned(aFolder) then exit;

  try
    if aLocation = nil then begin
       GetKNTLocation (aFolder, aLocation, true);                       // true: simplified (register only IDs)
       aLocation.ScrollPosInEditor:= aFolder.Editor.GetScrollPosInEditor;
    end;
    aLocation.SelLength := 0;

    if AddToGlobalHistory then begin
      gLocation:= aLocation.Clone();
      History.AddLocation( gLocation );                        // History: global navigation history
    end;

    if AddLocalMaintainingIndex  then
       aFolder.History.AddLocationMaintainingIndex (aLocation)
    else
       aFolder.History.AddLocation(aLocation);


  except
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_21;
    aFolder.History.Clear;
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

    - Default navigation is now global and so we can move between the nodes of a folder but also between notes
    - Local navigation (traveling only active folder's history) is also possible, clicking on the toolbar button with Ctrl.
    - Every jump from an internal keynote link will generate history, not only those that go to another node in the same
      folder.
    - Bookmark jumps will also generate history
    - Standard RTF notes (not multi-lvel tree notes) will have its own history navigation
    - Related buttons have been moved to main toolbar.
      their color and hint indicates if global and/or local history is available in that direction


    When we are travelling the global history, the local history in the active folder will be navigated accordingly,
    in a synchronized way, if possible, looking for an equal or equivalent (same node) item. The same will occur if the
    local history is leading the history navigation: global history will be synchronized, but in this case in a limited
    way, only if can go to the same item in the same direction and in one step.

    As usual, if we navigate backwards in history and then create new history (we are jumping or selecting another location,
    not navigating forward again) the history from the point forward will be truncated, replaced by the new history.
    This will happen always	in the global history, but in the local (folder) history only if we jump to another location in
    the same folder. If we have navigated backwards in a local history and then select another folder, global history will be
    truncated accordingly but the history in the starting folder will be kept intact. When we are back again in that folder
    (because of history navigation or not) we can move only in that local history if we want.
    We can move inside a folder (creating new history), select another folder and move there, creating also new history.
    If we navigate backwards from that point we will end up in the starting folder, but we can also select directly that initial
    folder and navigate its history (with ctrl+click) directly.

    The color of the arrows in the toolbar buttons will indicate if the forward o backwards navigation (in global history
    by default) will end up in another folder (intense blue) or in the same folder (light blue). Even if color is intense blue,
    we could navigate in that direction to an item in the same folder, when clicking with Ctrl key pressed (if local history allows it).
    If we reach one end of global history navigation but there is local history in that direction, the color of the button will be
    light blue and clicking in that button will be managed as if we pressed Ctrl+Click.
    The hint in the buttons will indicate what kind of history navigation is available in each direction (global, local or both)
}

procedure NavigateInHistory( const Direction: THistoryDirection);
var
  myLocation, LocBeforeNavigation : TLocation;
  masterHistory, slaveHistory : TKNTHistory;
  IterateAllOnSync, MaintainIndexInLocalHist: boolean;
  Done: boolean;
  AdjustVisiblePosition: boolean;

  procedure UpdateIfNil(var LocToUpdate: TLocation; const newLocation: TLocation);
  begin
      if LocToUpdate <> nil then exit;
      LocToUpdate:= newLocation;
  end;

begin
  if not assigned(ActiveFolder) then exit;

{$IFDEF DEBUG_HISTORY}
  if KntFile <> nil then begin
    if not _LastMoveWasHistory then
        Form_Main.Res_RTF.Text:= '--------     ' + #13 + 'Last move was history: NO' + #13#13 + Form_Main.Res_RTF.Text;
  end;
{$ENDIF}

  LocBeforeNavigation:= nil;
  GetKntLocation (ActiveFolder, LocBeforeNavigation, true);


  if CtrlDown then begin
     masterHistory:= ActiveFolder.History;
     slaveHistory := History;
     IterateAllOnSync:= false;
  end
  else begin
     masterHistory:= History;
     slaveHistory := ActiveFolder.History;
     IterateAllOnSync:= true;
  end;

  { *1
    myLocation could be nil => The arrow would be showing light blue because it is possible to go back in the local history,
    but without global history in that direction. In that case it is not necessary to press CTRl+Click.
    If we do not get any element with the 'master' history (depending on whether or not CTRL has been pressed), we will get it in the 'slave' history
    In some of them we must get a location value, or else the button would have been disabled from UpdateHistoryCommands

   *2:
    If myLocation=nil because we have not been able to advance having pressed CTRL (therefore in the local history), we will not do anything. 
    We'll go out and that's it.  If you've pressed CTRL you don't want to got to another folder.
  }

  try
    if Direction = hdForward then begin
       myLocation := masterHistory.GoForward;
       if (myLocation = nil) then
          if (masterHistory <> History)  then   // *2
             Exit
          else begin
             AddHistoryLocation(ActiveFolder, True);
             History.GoBack;
          end;
       UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdForward, IterateAllOnSync));
    end
    else begin
       if ( not _LastMoveWasHistory ) then begin
         MaintainIndexInLocalHist:= false;
         myLocation:= masterHistory.PickBack;
         if (myLocation = nil) or (myLocation.FolderID <> ActiveFolder.ID) then
            MaintainIndexInLocalHist:= True;
         AddHistoryLocation(ActiveFolder, MaintainIndexInLocalHist);
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
      AdjustVisiblePosition:= (Direction = hdForward);  // hdBack -> False by default (except when navigating back to another Editor (node or folder))
      if not ( assigned( myLocation ) and (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation(myLocation, true, AdjustVisiblePosition) ) then begin
        Done:= False;
        if Direction = hdForward then
            while masterHistory.CanGoForward do begin
              myLocation := masterHistory.GoForward;
              UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdForward, IterateAllOnSync));
              if (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation( myLocation ) then begin
                 Done:= True;
                 break;
              end;
            end
        else
            while masterHistory.CanGoBack do begin
              myLocation := masterHistory.GoBack;
              UpdateIfNil(myLocation, slaveHistory.SyncWithLocation (myLocation, hdBack, IterateAllOnSync));
              if (not LocBeforeNavigation.Equal(myLocation)) and JumpToLocation( myLocation, true, false ) then begin
                 Done:= True;
                 break;
              end;
            end;

        if (not Done) then
           Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_22;
      end;

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
  if Initializing then exit;

  lHistory:= nil;
  if assigned(ActiveFolder) then
     lHistory:= ActiveFolder.History;

  with Form_Main do begin
    GoBackEnabled :=        assigned(ActiveFolder) and ( History.CanGoBack or ((lHistory <> nil) and lHistory.CanGoBack) );
    MMHistoryGoBack.Enabled := GoBackEnabled;
    TB_GoBack.Enabled := GoBackEnabled;
    strHint:= STR_24;
    if GoBackEnabled then begin
       Loc:= History.PickBack;
       if (Loc <> nil) and (Loc.FolderID <> ActiveFolder.ID ) then
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

    GoForwardEnabled:= assigned(ActiveFolder) and (History.CanGoForward or ((lHistory <> nil) and lHistory.CanGoForward) );
    MMHistoryGoForward.Enabled := GoForwardEnabled;
    TB_GoForward.Enabled := GoForwardEnabled;
    strHint:= STR_27;
    if GoForwardEnabled then begin
       Loc:= History.PickForward;
       if (Loc <> nil) and (Loc.FolderID <> ActiveFolder.ID ) then
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
  if ActiveFile <> nil then begin
    str:= '--------     ' + #13;
    str:= str + 'Global: ' + History.Summary + #13;
    for i := 0 to ActiveFile.Folders.Count-1 do begin
       str:= str + ActiveFile.Folders[i].Name + ': ' + ActiveFile.Folders[i].History.Summary + #13;
    end;
    Form_Main.Res_RTF.Text:= str + #13 + Form_Main.Res_RTF.Text;
  end;
{$ENDIF}

end; // UpdateHistoryCommands



Initialization
   _Executing_History_Jump := false;
   _Executing_JumpToKNTLocation_ToOtherNote := false;
   _LastMoveWasHistory := false;


end.
