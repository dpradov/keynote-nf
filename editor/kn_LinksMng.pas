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
   System.DateUtils,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.StdCtrls,
   Vcl.Clipbrd,

   VirtualTrees,
   RxRichEd,

   gf_misc,
   kn_Info,
   kn_Const,
   kn_KntFolder,
   knt.model.note,
   kn_History,
   kn_LocationObj,
   kn_FindReplaceMng,
   knt.ui.editor
   ;

type
   TInfoExportedNoteInRTF = record
      NNodeGID: Cardinal;
      PosIni: Integer;
      PosInKNTLinks: TIntegerList;     // Cursor positions referenced by internal KNT Links
   end;
   TInfoExportedNotesInRTF = Array of TInfoExportedNoteInRTF;


   // Links related routines
    procedure GetKNTLocation (const aFolder : TKntFolder; var Location: TLocation; GetNames: Boolean= false; aNNode: TNoteNode = nil);
    procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean; Relative: boolean= false );
    procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean ; TextURL: string; NumBookmark09: integer= 0);
    procedure InsertMarker(Editor: TRxRichEdit; KEY_Marker: Char; TargetMarker: integer);
    procedure InsertHiddenKntCommand(Editor: TRxRichEdit; HiddenCommand: Char; Param: String);
    procedure InsertRtfHyperlink(const URLStr: string; const TextURL: string;
                                 Editor: TRxRichEdit;
                                 const sepL: string = '';
                                 const sepR: string = '');

    function BuildKntURL( const aLocation : TLocation) : string;
    function BuildLocationFromKntURL( KntURL : string): TLocation;
    function GetTextURLFromKntLocation (Loc : TLocation; RelativePath: boolean = false): string;
    function ConvertKNTLinksToNewFormat(const Buffer: Pointer; BufSize: integer; NoteGIDs: TMergedNotes; FolderIDs: array of TMergeFolders;
                                         var GIDsNotConverted: integer): AnsiString;
    function BuildBookmark09FromString( KntURL : AnsiString ): TLocation;
    procedure JumpToKNTLocation( LocationStr : string; myURLAction: TURLAction = urlOpen; OpenInCurrentFile: boolean= false);
    function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true; AdjustVisiblePosition: boolean = true;
                              myURLAction: TURLAction = urlOpen;
                              OpenInCurrentFile: boolean= false;
                              ConsiderOffset: boolean = false;
                              WordInRS: TWordInResultSearch = nil ): boolean;
    procedure OpenLocationInOtherInstance( aLocation : TLocation );
    function SearchCaretPos (Editor: TKntRichEdit;
                             CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean;
                             ScrollPosInEditor: TPoint;
                             AdjustVisiblePosition: boolean = true;
                             ContainsRegImages: boolean = true;
                             ConsiderOffset: boolean = false): integer;
    function PositionInImLinkTextPlain (myFolder: TKntFolder; NNode: TNoteNode; CaretPosition: integer; ForceCalc: boolean = false): integer;

    procedure ClickOnURL(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false; Button: TMouseButton = mbLeft);
    procedure InsertURL(URLStr : string; TextURL : string; Editor: TKntRichEdit);

    function PathOfKntLink (myTreeNode: PVirtualNode; myFolder : TKntFolder; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
                            forUseInFindResults: boolean = false): string;
    function UpdateLocation (Loc: TLocation; RaiseExcept: boolean= true): boolean;
    function UpdateLocationTarget (Loc: TLocation; RaiseExcept: boolean= true): boolean;

    // Navigation history
    procedure AddHistoryLocation( const aFolder : TKntFolder; const AddLocalMaintainingIndex: boolean;
                                  aLocation: TLocation= nil; const AddToGlobalHistory: boolean= true);
    procedure NavigateInHistory( const Direction: THistoryDirection);
    procedure UpdateHistoryCommands;

    function TypeURL (var URLText: string; var KNTlocation: boolean): TKntURL;
    function URLFileExists (var URL: string): boolean;

    function DeleteBookmark09 (Loc: TLocation): boolean;

    function ReplaceHyperlinksWithStandardBookmarks(const S: AnsiString): AnsiString;
    function ConvertToStandardBookmarks(const S: AnsiString; InfoExportedNotes: TInfoExportedNotesInRTF;
                                        RemoveAllHiddenCharacters: Boolean;
                                        ExportOptions: TExportOptions): AnsiString;
    procedure GetInfoKNTLinksWithoutMarker(const S: AnsiString; InfoExportedNotes: TInfoExportedNotesInRTF);
    procedure InsertKNTLinksWithoutMarker(const RTF : TAuxRichEdit; InfoExportedNotes: TInfoExportedNotesInRTF);

var
   _Executing_History_Jump : boolean;
   _Executing_JumpToKNTLocation_ToOtherNote : boolean;
   _LastMoveWasHistory : boolean;


implementation
uses
   gf_files,
   gf_strings,
   gf_streams,
   kn_Global,
   kn_Main,
   kn_URL,
   kn_EditorUtils,
   kn_RTFUtils,
   kn_ImagesMng,
   kn_ImageForm,
   kn_NoteFileMng,
   kn_KntFile,
   knt.App,
   knt.RS
  ;


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
function PathOfKntLink (myTreeNode: PVirtualNode; myFolder : TKntFolder; position: Integer; ForceShowPosition: boolean; RelativeKNTLink: boolean;
                        forUseInFindResults: boolean = false): string;
var
  path, pathInsertionPoint : string;
  i, j, n, m: integer;
  pDelim: integer;
  ShowFullPath: boolean;
  PathTopToBottom: boolean;

begin
  PathTopToBottom:= KntTreeOptions.PathTopToBottom;
  ShowFullPath:= KntTreeOptions.ShowFullPath;
  if forUseInFindResults then begin
     ShowFullPath:= KntTreeOptions.ShowFullPathSearch;
     PathTopToBottom:= true;
  end;

  if assigned(myTreeNode) and not (myTreeNode = myFolder.TV.RootNode) then begin
     if ShowFullPath then
        path:= myFolder.TreeUI.GetNodePath( myTreeNode, KntTreeOptions.NodeDelimiter, PathTopToBottom ) // {N}
     else
        path:= myFolder.TreeUI.GetNNode(myTreeNode).NoteName; // {N}

     if PathTopToBottom then
        path:= myFolder.Name + KntTreeOptions.NodeDelimiter + path
     else
        path:= path + KntTreeOptions.NodeDelimiter + myFolder.Name;
  end
  else
     path := myFolder.Name;


  // Hide common part of the path (common ancestors), if RelativeKNTLink=True
  if RelativeKNTLink then begin
     pathInsertionPoint:= PathOfKntLink(GetCurrentTreeNode, ActiveFolder, -1, false, false);

     if path = pathInsertionPoint then
        path:= ''
     else begin
         i:= 1;
         n:= Length(path);
         m:= Length(pathInsertionPoint);

         if KntTreeOptions.PathTopToBottom then begin
            pDelim:= 0;
            while (i < Min(n, m)) and (path[i] = pathInsertionPoint[i]) do begin
               if path[i] = KntTreeOptions.NodeDelimiter then
                  pDelim:= i;
               i:= i + 1;
            end;
            if (i = m + 1) and (path[i] = KntTreeOptions.NodeDelimiter) then
               pDelim:= i;
            path:= Copy(path, pDelim+1, Length(path));
         end
         else begin
            i:= n;
            j:= m;
            pDelim:= n+1;
            while (i >= 1) and (j >= 1) and (path[i] = pathInsertionPoint[j]) do begin
               if path[i] = KntTreeOptions.NodeDelimiter then
                  pDelim:= i;
               i:= i - 1;
               j:= j - 1;
            end;
            if (j = 0) and (path[i] = KntTreeOptions.NodeDelimiter) then
               pDelim:= i;
            path:= Copy(path, 1, pDelim-1);
         end;

     end;
  end;


  if (position >= 0) and (ForceShowPosition or KntTreeOptions.CaretInKNTLinks or (path = '')) then begin
     if path = '' then
        path:= 'Pos.'
     else
        path := path + ' - ';

     path := path + IntToStr(position);
  end;

  Result:= path;
end; // PathOfKNTLink


//=========================================
// GetNNode
//=========================================

function UpdateLocation (Loc: TLocation; RaiseExcept: boolean= true): boolean;
var
  Ok: boolean;
begin
   if not Loc.Calculated then
      Ok:= UpdateLocationTarget(Loc, RaiseExcept)

   else begin
      Ok:= false;

      try
         if assigned(Loc.NNode) then begin
            if not ActiveFile.CheckNNode(Loc.NNode, Loc.Folder) then exit;
            if not Loc.NNode.Note.IsValid (Loc.NEntry) then exit;
         end
         else begin
            if not assigned(Loc.Folder) or not ActiveFile.IsValid(Loc.Folder) then exit;
         end;
         Ok:= True;

      finally
         if not Ok then
            raise EInvalidLocation.Create(GetRS(sLnk11));
      end;

   end;
   Result:= Ok;

end;


function UpdateLocationTarget (Loc: TLocation; RaiseExcept: boolean= true): boolean;
begin
   with Loc do begin

      Folder := nil;
      NNode:= nil;
      NEntry:= nil;
      Calculated:= true;
      Result:= false;


      if NNodeGID <> 0 then begin  // lfNew2 format
         ActiveFile.GetNNodeByGID(NNodeGID, NNode, Folder);
         if (NNode = nil) then begin
             if RaiseExcept then
                raise EInvalidLocation.Create(Format(GetRS(sLnk03b), [NNodeGID]))
         end
         else begin
            NEntry:= NNode.Note.Entries[0];        // %%%
            exit (True)
         end;
      end;


      if (FolderID <> 0) then begin // lfNew format
         Folder := ActiveFile.GetFolderByID(FolderID);
         if (Folder = nil) and RaiseExcept then
            raise EInvalidLocation.Create(Format(GetRS(sLnk01), [FolderID]));
      end
      else begin                    // lfOld format
         Folder := ActiveFile.GetFolderByName( FolderName );
         if (Folder = nil) and RaiseExcept then
            raise EInvalidLocation.Create(Format( GetRS(sLnk02), [FolderName] ));
      end;

      if (Folder = nil) then exit;      // False

      // obtain NNODE

      if (NNodeID > 0) or (NoteName <> '') then begin   // If NodeID <= 0 and NodeName = '' -> Node will be ignored
         if (NNodeID <> 0) then begin    // lfNew2 or lfNew format
            NNode:= Folder.GetNNodeByID(NNodeID);
            if (NNode = nil) and RaiseExcept then
               raise EInvalidLocation.Create(Format( GetRS(sLnk03), [NNodeID] ));
         end
         else begin
            NNode:= Folder.GetNNodeByNoteName(NoteName);
            if (NNode = nil) and RaiseExcept  then
               raise EInvalidLocation.Create(Format( GetRS(sLnk04), [NoteName] ));
         end;
         if NNode <> nil then
            NEntry:= NNode.Note.Entries[0];        // %%%
      end;

      if NEntry <> nil then begin
         NNodeGID:= NNode.GID;
         Result:= true;
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
         InsertRtfHyperlink(URLStr, TextURL, Editor, sepL, sepR);
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


procedure InsertRtfHyperlink(const URLStr: string; const TextURL: string;
                             Editor: TRxRichEdit;
                             const sepL: string = '';
                             const sepR: string = '');
begin
     Editor.PutRtfText(Format('{\rtf1\ansi{\colortbl ;\red0\green0\blue255;}{\fonttbl}%s' + LINK_RTF + '%s\cf0\ulnone}',
                                     [sepL, URLToRTF(URLStr, false ), URLToRTF(TextURL, true), sepR]), true);
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
                     GetRS(FILTER_ALLFILES);
         FilterIndex := 1;
         if AsLink then
           Title := GetRS(sLnk05)
         else
           Title := GetRS(sLnk06);
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
          App.ErrorPopup( GetRS(sLnk07));
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
              App.ErrorPopup( E);
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
procedure GetKNTLocation (const aFolder : TKntFolder; var Location: TLocation; GetNames: Boolean= false; aNNode: TNoteNode = nil);
begin
{$IFDEF DEBUG_HISTORY}
   Simplified:= false;
{$ENDIF}

   if not assigned(Location) then
      Location:= TLocation.Create;

   if not assigned(aFolder) then exit;

   if aNNode = nil then
      aNNode:= aFolder.FocusedNNode;

   Location.NNode:= aNNode;
   Location.Folder:= aFolder;

   with Location do begin
      FolderID := aFolder.ID;
      NoteName := '';
      NNodeID := 0;
      NNodeGID:= 0;
      if aNNode <> nil then begin
         NNodeID := NNode.ID;
         NNodeGID:= NNode.GID;
         NEntry:= aNNode.Note.Entries[0];       // %%%
      end;
      CaretPos := aFolder.Editor.SelStart;
      SelLength := aFolder.Editor.SelLength;
      Mark := 0;

      if GetNames then begin
         FileName := normalFN( TKntFile(aFolder.KntFile).FileName );
         FolderName := aFolder.Name;
         if NNode <> nil then
            NoteName := NNode.NoteName;
      end;
      Calculated:= true;
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

procedure InsertMarker(Editor: TRxRichEdit; KEY_Marker: Char; TargetMarker: integer);
begin
   //  {\rtf1\ansi {\v\'11B5\'12}};    => {\rtf1\ansi \v\'11B5\'12\v0};  // Finally they will be inserted directly with \v...\v0 (see comment *2 next to KNT_RTF_BMK_HIDDEN_MARK in kn_const.pas)
   InsertHiddenKntCommand(Editor, KEY_Marker, TargetMarker.ToString);
end;

procedure InsertHiddenKntCommand(Editor: TRxRichEdit; HiddenCommand: Char; Param: String);
var
   RTFHiddenCommand: AnsiString;
begin
   RTFHiddenCommand:= '\v' + KNT_RTF_HIDDEN_MARK_L + HiddenCommand + Param + KNT_RTF_HIDDEN_MARK_R + '\v0';
   Editor.PutRtfText('{\rtf1\ansi' + RTFHiddenCommand + '}',  True);
end;


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
   TreeNode: PVirtualNode;
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
    if (aLocation.NNodeGID = 0) and (aLocation.FolderID = 0) then begin
      App.InfoPopup(GetRS(sLnk08));
      exit;
    end;

    if TextURL = '' then
       TextURL:= GetTextURLFromKntLocation (aLocation, assigned(Editor.NNodeObj) and KntTreeOptions.RelativeKNTLinks);

    InsertLink(BuildKntURL(aLocation),  TextURL, false, Editor);
    if aLocation.Mark <> 0 then
       strTargetMarker:= Format(GetRS(sLnk31), [aLocation.Mark]);
    App.ShowInfoInStatusBar(GetRS(sLnk09) + strTargetMarker);
  end
  else begin
    // mark caret position as TLocation
    GetKNTLocation (ActiveFolder, aLocation, true);

    // If we are pointing to the start of a node or a folder (CaretPos = 0), we will not create any new markers. We will always aim for that position 0.
    strTargetMarker:= '';

    if (aLocation.CaretPos <> 0) and assigned(Editor) and (not Editor.PlainText) and (assigned(Editor.NNodeObj)) then begin
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

         InsertMarker(Editor, KEY_Marker, TargetMarker);
      end;
      strTargetMarker:= Format(GetRS(sLnk31) + GetRS(sLnk32), [TargetMarker]);
      aLocation.Mark:= TargetMarker;
    end;

    App.ShowInfoInStatusBar(GetRS(sLnk10) + strTargetMarker);
  end;

end; // InsertOrMarkKNTLink



function GetURLLinkStandard(Loc: TLocation): AnsiString;
var
   Bk: AnsiString;
begin
  if (Loc.Mark > 0) then
     Bk:= 'B' + Loc.Mark.ToString
  else
  if (Loc.CaretPos = 0) then
     Bk:= 'B0'
  else
     Bk:= 'P' + Loc.CaretPos.ToString;

  Result:= Format('\\l "%d_%s"', [Loc.NNodeGID, Bk])
end;


const
   sHYPERLINK = '\*\fldinst{HYPERLINK "file:';  // -> ", inclusive: 22
   PrefixData = KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_DATA;

(*
   {{\field{\*\fldinst{HYPERLINK "file:///<41|663|0|11"}}{\fldrslt{\ul\cf2\cf2\ul Introduction}}}}\f0\fs20\par
     ->
   {{\field{\*\fldinst{HYPERLINK \\l "41_B11"}}{\fldrslt{\ul\cf2\cf2\ul Introduction}}}}\f0\fs20\par
*)

function ReplaceHyperlinksWithStandardBookmarks(const S: AnsiString): AnsiString;
var
   pIn, pOut, pI, pF, NBytes: integer;
   URL, NewURL: AnsiString;
   Loc: TLocation;

begin
  if S = '' then Exit('');

  SetLength(Result, Length(S));
{$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
  ZeroMemory(@Result[1], Length(S));
{$ENDIF}

  pIn:= 1;
  pOut:= 1;
  pI:= 0;

  repeat
     pI:= Pos(AnsiString(sHYPERLINK), S, pI+1);

     if pI > 0 then begin
        pF:= Pos('"}', S, pI + length(sHYPERLINK));
        if (pF > 0) then begin
            URL:= Copy(S, pI+22, pF-pI-22);
            try
              try
                 Loc:= BuildLocationFromKntURL(URL);
                 if UpdateLocation(Loc, false) or (Loc.NNodeGID <> 0) then begin
                    NewURL:= GetURLLinkStandard(Loc);

                    NBytes:= pI+21 - pIn;                 // 21 -> Do not include the first "
                    Move(S[pIn], Result[pOut], NBytes);
                    inc(pOut, NBytes);
                    Move(NewURL[1], Result[pOut], Length(NewURL));
                    inc(pOut, Length(NewURL));
                    pIn:= pF+1;
                 end;

              finally
                 Loc.Free;
              end;

            except
            end;

            pI:= pF;
        end;
     end;
    until pI = 0;

  if pOut = 1 then
     Result:= S

  else begin
     NBytes:= Length(S) - pIn;
     Move(S[pIn], Result[pOut], NBytes);
     inc(pOut, NBytes);

     SetLength(Result, pOut);
  end;
end;


(*
   If current NNodeGID=45
    \pard\ltrpar\sa100\cf3\b\v\'11B13\'12\v0 Changing the storage mode\b0\par
      ->
    \pard\ltrpar\sa100\cf3\b{\*\bkmkstart 45_B13}{\*\bkmkend 45_B13} Changing the storage mode\b0\par
*)

function ConvertToStandardBookmarks(const S: AnsiString; InfoExportedNotes: TInfoExportedNotesInRTF;
                                    RemoveAllHiddenCharacters: Boolean;
                                    ExportOptions: TExportOptions): AnsiString;
var
   i, pI, pF, pOut: integer;
   HiddenMarks: THiddenMarks;
   InfoExpNote: TInfoExportedNoteInRTF;
   GID: Cardinal;
   NNode: TNoteNode;

begin
  if S='' then Exit('');

  try
     HiddenMarks:= hmAll;
     if not RemoveAllHiddenCharacters then
        HiddenMarks:= hmOnlyBookmarks;

     if Length(InfoExportedNotes) = 1 then begin
        Result:= RemoveKNTHiddenCharactersInRTF(S, HiddenMarks, true, InfoExportedNotes[0].NNodeGID);
        exit;
     end;

     pI:= 0;

     //  \v\'11D321\'12\v0
     //  \v\f1\fs20\'11D1\'12\cf1\v0
     //  ...

     repeat
        pI:= Pos(AnsiString(PrefixData), S, pI+1);
        if pI <= 0 then break;
        pF:= Pos(AnsiString(KNT_RTF_HIDDEN_MARK_R), S, pI + Length(PrefixData));
        if pF <= 0 then break;

        GID:= StrToIntDef(Copy(S,pI+Length(PrefixData), pF-pI - Length(PrefixData)), 0);
        for i:= 0 to High(InfoExportedNotes) do begin
           if InfoExportedNotes[i].NNodeGID= GID then begin
              InfoExportedNotes[i].PosIni:= LastPos('\v\', S, pI);     // The normal will be: PosIni= pI-10
              break;
           end;
        end;
     until (pI = 0);

     InfoExportedNotes[0].PosIni:= 1;
     Result:= '';

     for i:= 0 to High(InfoExportedNotes) do begin
         InfoExpNote:= InfoExportedNotes[i];

         pI:= InfoExpNote.PosIni;
         if i = High(InfoExportedNotes) then
            pF:= -1
         else
            pF:= InfoExportedNotes[i+1].PosIni;

         if (pI <= 0) or ((i < High(InfoExportedNotes)) and (pF <= pI)) then
            raise Exception.Create('Invalid Range');

         RemoveKNTHiddenCharactersInRTF(S, HiddenMarks, Result, pOut, true, InfoExpNote.NNodeGID, pI, pF);
     end;

  except
     On E: Exception do begin
        App.ErrorPopup(E, 'Converting to Std Bkmrks');
        Result:= S;
     end;
  end;

end;


procedure GetInfoKNTLinksWithoutMarker(const S: AnsiString; InfoExportedNotes: TInfoExportedNotesInRTF);
var
   pI, pF: integer;
   URL: AnsiString;
   Loc: TLocation;

   procedure AddPosition;
   var
     i: integer;
     InfoExpNote: TInfoExportedNoteInRTF;
   begin
     for i:= 0 to High(InfoExportedNotes) do begin
        with InfoExportedNotes[i] do begin
           if NNodeGID= Loc.NNodeGID then begin
              if PosInKNTLinks = nil then
                 PosInKNTLinks:= TIntegerList.Create;
              if PosInKNTLinks.IndexOf(Loc.CaretPos) < 0 then
                 PosInKNTLinks.Add(Loc.CaretPos);
              break;
           end;
        end;
     end;
   end;

begin
  if S = '' then Exit;

  pI:= 0;
  repeat
     pI:= Pos(AnsiString(sHYPERLINK), S, pI+1);
     if pI > 0 then begin
        pF:= Pos('"}', S, pI + length(sHYPERLINK));
        if (pF > 0) then begin
           URL:= Copy(S, pI+22, pF-pI-22);
           try
             try
                Loc:= BuildLocationFromKntURL(URL);
                if UpdateLocation(Loc, false) or (Loc.NNodeGID <> 0) then begin
                   if (Loc.Mark = 0) and (Loc.CaretPos <> 0) then
                      AddPosition;        // It will be necessary to create a marker for the indicated 'position'
                end;

             finally
                Loc.Free;
             end;
           except
           end;
           pI:= pF;
        end;
     end;
  until pI = 0;
end;


procedure InsertKNTLinksWithoutMarker(const RTF : TAuxRichEdit; InfoExportedNotes: TInfoExportedNotesInRTF);
var
  InfoExpNote: TInfoExportedNoteInRTF;
  i, j: integer;
  pIN, pI, pF: integer;
  GID: Integer;
  Offset, SS, SSr: integer;
  S: String;
  Ch: Char;

begin
  // Note: The elements in InfoExportedNotes are added in the same order as they appear in the RTF file.

  //  \v\'11D321\'12\v0
  //  \v\f1\fs20\'11D1\'12\cf1\v0

  pIN:= -1;

  for i:= 0 to High(InfoExportedNotes) do begin
     with InfoExportedNotes[i], RTF do begin
        if PosInKNTLinks = nil then continue;          // No KNT Links without markers

        repeat
           Inc(pIN);
           pI:= FindText(KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_DATA, pIN, -1, []);
           pF:= FindText(KNT_RTF_HIDDEN_MARK_R_CHAR, pI, pI+20, []);
           S:=  GetTextRange(pI+2, pF);
           pIN:= pF;
           GID:= StrToIntDef(S, 0);
        until GID = NNodeGID;

        Offset:= pF + 1;
        PosInKNTLinks.Sort(CompareIntegers);

        for j:= 0 to PosInKNTLinks.Count-1 do begin
           pI:= PosInKNTLinks[j];

           SS:= pI + Offset;
           RTF.SelStart:= SS;
           SSr:= RTF.SelStart;
           if SSr >= 1 then begin                  // See comment to VK_LEFT in TKntRichEdit.KeyDown (in  knt.ui.editor)
              S:= GetTextRange(SSr-1, SSr);
              if Length(S) > 0 then begin
                 ch:= S[1];
                 if (ch = KNT_RTF_HIDDEN_MARK_R_CHAR) or (ch = '"') then begin
                    SelStart:= SSr-1;
                    if SelStart <> SSr-1 then      // It will have been placed to the left of the first hidden character
                    else
                      SelStart:= SSr;              // It was not hidden. We leave it where it was.
                 end;
              end;
           end;
           InsertMarker(RTF, KNT_RTF_HIDDEN_BMK_POSITION, pI);
           inc(Offset, Length(IntToStr(pI)) + 3);       // pI=123 ->  HP123H

           pIN:= pI + 2;
        end;
     end;
  end;

end;



//===============================================================
// BuildKNTURL
//===============================================================
function BuildKntURL( const aLocation : TLocation) : string;
var
  LocationString : string;
  FolderID_WithSEP, NodeId, LocationMark: string;
begin
  if ( aLocation.FileName = normalFN( ActiveFile.FileName )) then
     LocationString := ''
  else
     LocationString := FileNameToURL( aLocation.FileName );

     if aLocation.NNodeGID <> 0 then begin           // New created links will use newer format, with GIDs: file:///"NoteGID|...
        LocationMark:= KNTLOCATION_MARK_NEW2;
        NodeId:= IntToStr(aLocation.NNodeGID);
        FolderID_WithSEP:= '';
     end
     else begin
        LocationMark:= KNTLOCATION_MARK_NEW;
        FolderID_WithSEP:= IntToStr( aLocation.FolderID ) + KNTLINK_SEPARATOR;
        NodeId:= IntToStr( aLocation.NNodeID );
     end;

    LocationString := 'file:///' + LocationString + LocationMark +
      FolderID_WithSEP +
      NodeId + KNTLINK_SEPARATOR +
      IntToStr( aLocation.CaretPos ) + KNTLINK_SEPARATOR +
      IntToStr( aLocation.SelLength );

    if aLocation.Mark > 0 then
       LocationString := LocationString + KNTLINK_SEPARATOR + IntToStr( aLocation.Mark);

  result := LocationString;
end;


//---------------------------------------------------------------
// BuildLocationFromKntURL
//---------------------------------------------------------------

function BuildLocationFromKntURL( KntURL : string): TLocation;
var
  p, pMin: integer;
  FormatLink: TKntLinkFormat;
  Bak_KntURL : string;
  str: string;

begin

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive three types of links:
  // the old style link:          file:///?filename.knt...
  // the last recent style link:  file:///*filename.knt...
  // the newer style link:        file:///<filename.knt...

  Result:= TLocation.Create;
  with Result do begin

    Calculated:= false;

    p := 0;
    Bak_KntURL := KntURL;

    KntURL := StripFileURLPrefix( KntURL );

     // see which marker occurs FIRST
    // (both markers may occur, because '?', '*' and '<' may occur within folder or node names

     FormatLink:= lfNone;
     pMin:= 99999;
     p:= pos( KNTLOCATION_MARK_OLD, KntURL );
     if p > 0 then begin
        pMin:= p;
        FormatLink:= lfOld;
     end;
     p:= pos( KNTLOCATION_MARK_NEW, KntURL );
     if (p > 0) and (p < pMin) then begin
        pMin:= p;
        FormatLink:= lfNew;
     end;
     p:= pos( KNTLOCATION_MARK_NEW2, KntURL );
     if (p > 0) and (p < pMin) then begin
        pMin:= p;
        FormatLink:= lfNew2;
     end;

     // extract filename
     case pMin of
       0 : raise EInvalidLocation.Create( Bak_KntURL );
       1 : FileName := ''; // same file as current
       else begin
         FileName := HTTPDecode( copy( KntURL, 1, pMin-1));
         if (ActiveFile <> nil) and (FileName = ActiveFile.FileName ) then
           FileName := '';
       end;
     end;
     delete( KntURL, 1, pMin ); // delete filename and ?, * or < marker


     if FormatLink <> lfNew2 then begin
        // extract folder name or ID
        p := pos( KNTLINK_SEPARATOR, KntURL );
        if p= 1 then
           raise EInvalidLocation.Create( Bak_KntURL );
        if p= 0 then
          p:= 999;

        str:= copy(KntURL, 1, p-1);
        if FormatLink = lfNew then
           FolderID := StrToUInt(str)
        else
           FolderName := HTTPDecode(str);
       delete( KntURL, 1, p );

        if ActiveFile <>  nil then
           if FolderID <> 0 then
              Folder := ActiveFile.GetFolderByID (FolderID)
           else begin
              Folder := ActiveFile.GetFolderByName (FolderName);
              if assigned(Folder) then
                 FolderID:= Folder.ID;
           end;
     end;

     p := pos( KNTLINK_SEPARATOR, KntURL );
     if p= 1 then begin
        NNodeID := 0;
        NNodeGID := 0;
     end
     else begin
        if p= 0 then
           p:= 999;
        str:= copy(KntURL, 1, p-1);
        case FormatLink of
           lfOld:  NoteName := HTTPDecode(str);
           lfNew:  NNodeID:=  StrToUIntDef(str, 0);
           lfNew2: NNodeGID:= StrToUIntDef(str, 0);
        end;
     end;
     delete(KntURL, 1, p);

     if (KntURL <> '') then begin
       p := pos( KNTLINK_SEPARATOR, KntURL );
       if p <= 0 then
         p:= 99;

       CaretPos := StrToIntDef(copy(KntURL, 1, p-1), 0);
       delete( KntURL, 1, p );
     end;

     if (KntURL <> '') then begin
       p := pos( KNTLINK_SEPARATOR, KntURL );
       SelLength := 0;
       if (p > 0) then begin                              // selLenght|mark
          try
            SelLength := StrToInt(copy(KntURL, 1, pred(p)));
          except
          end;
          delete(KntURL, 1, p);
          if (KntURL <> '') then
             Mark := StrToIntDef(KntURL, 0);
       end
       else                                               // selLength
          SelLength := StrToIntDef(KntURL, 0);
     end;
  end;

end;


function BuildBookmark09FromString( KntURL : AnsiString ): TLocation;
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
   if (Length(KntURL) <= L + 2) or (Pos('file:///', KntURL) <> 1) then exit;

   Format:= lfNone;

   if KntURL[L+1] = KNTLOCATION_MARK_NEW2 then
      Format:= lfNew2
   else
   if KntURL[L+1] = KNTLOCATION_MARK_NEW then
      Format:= lfNew
   else
      exit;

   Strs:= TStringList.Create;
   Result:= TLocation.Create;
   try
     try
        s:= Copy(KntURL, L+2);
         SplitString(Strs, s, '|', false);
         i:= 0;
         if Format = lfNew then begin
            Result.FolderID:= StrToUInt(Strs[0]);
            Result.NNodeID:= StrToUInt(Strs[1]);
         end
         else begin
           Result.NNodeGID:= StrToUInt(Strs[0]);
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


function GetTextURLFromKntURL (KntURL : string; RelativePath: boolean = false): string;
var
  Loc: TLocation;

begin
  Loc:= BuildLocationFromKntURL(KntURL);
  try
    Result:= GetTextURLFromKntLocation (Loc, RelativePath);

  finally
    if Loc <> nil then
       Loc.Free;
  end;
end;


function GetTextURLFromKntLocation (Loc : TLocation; RelativePath: boolean = false): string;
var
  FN: string;

begin
   with Loc do begin

       if (ActiveFile.FileName = FileName) or (FileName = '') then begin
          UpdateLocation(Loc, false);
          if NNode = nil then
             Result:= UpperCase(GetRS(sLnk14))
          else
             Result:= PathOfKNTLink(NNode.TVNode, Folder, CaretPos, false, RelativePath);
      end
      else begin
         FN:= ExtractFileName(FileName);
         if NoteName <> '' then
            Result:= Format('%s: ?%s/%s', [FN, FolderName, NoteName])
         else begin
            if NNodeGID > 0 then
               Result:= Format('%s: <%d', [FN, NNodeGID])
            else
               Result:= Format('%s: *%d|%d', [FN, FolderID, NNodeID]);
         end;

         Result:= Result + Format('|%d|%d|%d', [CaretPos, SelLength, Mark]);
      end;

   end;

end;


function GetPositionOffset (myFolder: TKntFolder; NNode: TNoteNode; Pos_ImLinkTextPlain: integer; CaretPosition: integer; ForceCalc: boolean = false): integer;
var
  Stream: TMemoryStream;
  imLinkTextPlain: String;
  Offset: integer;
  Modified: boolean;
  NEntry: TNoteEntry;
begin
   Offset:= 0;
   if (CaretPosition < 0) and (Pos_ImLinkTextPlain < 0) then exit(0);

   Stream:= nil;

   if myFolder.Editor.SupportsRegisteredImages then begin
     imLinkTextPlain:= '';
      if assigned(NNode) then begin
          NEntry:= NNode.Note.Entries[0];                              // %%%
          Stream:= NEntry.Stream;
          imLinkTextPlain := NEntry.TextPlain;
          Modified := NEntry.Modified;
      end;
   end;

   if Stream = nil then exit(0);

   if imLinkTextPlain = '' then begin
      var RTFAux: TAuxRichEdit;
      RTFAux:= CreateAuxRichEdit;
      try
         imLinkTextPlain:= myFolder.PrepareTextPlain(NNode, RTFAux);
      finally
         RTFAux.Free;
      end;
   end;



   // See notes in ImagesManager.GetPositionOffset

   if CaretPosition >= 0 then
      Offset:= ImageMng.GetPositionOffset_FromEditorTP (Stream, CaretPosition, imLinkTextPlain, Modified, ForceCalc)
   else
      Offset:= ImageMng.GetPositionOffset_FromImLinkTP (Stream, Pos_ImLinkTextPlain, imLinkTextPlain, Modified, ForceCalc);


   Result:= Offset;
end;


function SearchCaretPos (Editor: TKntRichEdit;
                         CaretPosition: integer; SelectionLength: integer; PlaceCaret: boolean;
                         ScrollPosInEditor: TPoint;
                         AdjustVisiblePosition: boolean = true;
                         ContainsRegImages: boolean = true;
                         ConsiderOffset: boolean = false): integer;
var
  Offset: integer;
  Pos_ImLinkTextPlain: integer;
  myFolder : TKntFolder;
  NNode: TNoteNode;
  InFoldedBlock: boolean;
begin
  // ContainsRegImages = True => We have not verified that there are no registered images
  // ConsiderOffset = True    => What we receive in CaretPosition is a position in imLinkTextPlain

  Offset:= 0;
  if ConsiderOffset and Editor.SupportsRegisteredImages and ContainsRegImages then begin
     Pos_ImLinkTextPlain:= CaretPosition;
     myFolder:= TKntFolder(Editor.FolderObj);
     NNode:= TNoteNode(Editor.NNodeObj);
     if (myFolder <> nil) then
        Offset:= GetPositionOffset(myFolder, NNode, Pos_ImLinkTextPlain, -1);
  end;

  if PlaceCaret then
     InFoldedBlock:= False;
     with Editor do begin
       BeginUpdate;
       try
          if CaretPosition >= 0 then
             SelStart := CaretPosition - Offset;

          if SelAttributes.Protected then begin
             var pI, pF: integer;
             if PositionInFoldedBlock(TextPlain, SelStart, Editor, pI, pF) then begin
                InFoldedBlock:= True;
                SelStart:= pI;
                SelLength:= 1;
             end;
          end;
          if not InFoldedBlock and (SelectionLength >= 0) then
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

function PositionInImLinkTextPlain (myFolder: TKntFolder; NNode: TNoteNode; CaretPosition: integer; ForceCalc: boolean = false): integer;
var
   Offset: integer;
begin
   Offset:= GetPositionOffset(myFolder, NNode, -1, CaretPosition, ForceCalc);
   Result:= CaretPosition + Offset;
end;


//===============================================================
// DeleteBookmark9
//===============================================================

function DeleteBookmark09 (Loc: TLocation): boolean;
var
  p, SS, SL, selLen: integer;
  TargetMark: string;
  EditorVisible: boolean;
  Stream: TMemoryStream;
  RTFText: PAnsiChar;
  RTFBookmark: AnsiString;
  NBytes: integer;
  Str: AnsiString;
  Editor: TKntRichEdit;

begin
   Result:= false;
   if not Loc.Bookmark09 or (Loc.Mark < 1) or (Loc.Mark > 10) then exit;

   UpdateLocation(Loc);

   if not assigned(Loc.NNode) then exit;

   // If not included in some of the visible Editors -> make the change directly in the folder or node stream
   // See comment to kn_EditorUtils.RemoveKNTHiddenCharactersInRTF

  EditorVisible:= false;

  if ActiveFolder.TreeUI.FocusedNode = Loc.NNode.TVNode then
     EditorVisible:= true;

  if EditorVisible then begin
     Editor:= Loc.Folder.Editor;
     SS:= Editor.SelStart;
     SL:= Editor.SelLength;

     TargetMark:=  KNT_RTF_HIDDEN_MARK_L_CHAR + KNT_RTF_HIDDEN_Bookmark09 + IntToStr(Loc.Mark) + KNT_RTF_HIDDEN_MARK_R_CHAR;
     with Editor do begin
        p:= FindText(TargetMark, 0, -1, [stMatchCase]);
        if p > 0 then begin
          SelStart := p;
          SelLength := Length(TargetMark);
          if SelLength > Length(TargetMark) then begin
             // If our hidden mark is next to others, it will select everything hidden. And if it is next to a HYPERLINK it will select it in its entirety
             Str:= RTFSelText;
             Str:= StringReplace(Str, Format(KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_Bookmark09 + '%d'+ KNT_RTF_HIDDEN_MARK_R, [Loc.Mark]), '', []);
             SelText:= Str;
          end
          else
             SelText := '';

          Result:= true;
          if SS < p then
             SelStart:= SS
          else
             SelStart:= SS - Length(TargetMark);

          SelLength:= SL;
        end;
     end;

  end
  else begin
     Stream:= Loc.NEntry.Stream;
     if Stream.Size = 0 then exit;

     RTFText:= PAnsiChar(Stream.Memory);
     RTFBookmark:= Format(KNT_RTF_Bmk09_HIDDEN_MARK, [Loc.Mark]);

     p:= PosPAnsiChar(PAnsiChar(RTFBookmark), RTFText, 1) -1;                        // See comment *2 in ImagesManager.ProcessImagesInRTF
     if p < 0 then begin
        RTFBookmark:= Format(KNT_RTF_HIDDEN_MARK_L + KNT_RTF_HIDDEN_Bookmark09 + '%d'+ KNT_RTF_HIDDEN_MARK_R, [Loc.Mark]);
        p:= PosPAnsiChar(PAnsiChar(RTFBookmark), RTFText, 1) -1;
     end;

     if p >= 0 then begin
        selLen:= Length(RTFBookmark);
        NBytes:= Stream.Size - p - selLen;
        Move(RTFText[p+selLen], RTFText[p], NBytes);
        Stream.Size:= Stream.Size-selLen;
        Loc.NEntry.TextPlain:= '';
     end;

  end;



 end;



//===============================================================
 // JumpToLocation
 //===============================================================
 function JumpToLocation( Location: TLocation; IgnoreOtherFiles: boolean = true; AdjustVisiblePosition: boolean = true;
                          myURLAction: TURLAction = urlOpen;
                          OpenInCurrentFile: boolean= false;
                          ConsiderOffset: boolean = false;
                          WordInRS: TWordInResultSearch = nil): boolean;
 var
   myFolder : TKntFolder;
   myTreeNode : PVirtualNode;
   origLocationStr : string;
   LocBeforeJump: TLocation;
   FN, FN_ActiveFile: string;
   ResultOpen: integer;

   function SearchTargetMark (SearchBookmark09: boolean = false): boolean;
   var
      p, selLen: integer;
      TargetMark: string;
      KeyBookmark: char;
      InFoldedBlock: boolean;
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

               if SelAttributes.Protected then begin
                  var pI, pF: integer;
                  if PositionInFoldedBlock(TextPlain, SelStart, myFolder.Editor, pI, pF) then begin
                     InFoldedBlock:= True;
                     SelStart:= pI;
                     selLen:= 1;
                  end;
               end;
               if not InFoldedBlock and (Location.SelLength > 0) then
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
      if ActiveFolder <> nil then begin
         GetKntLocation (ActiveFolder, LocBeforeJump, false);
         LocBeforeJump.ScrollPosInEditor:= ActiveFolder.Editor.GetScrollPosInEditor;
      end;

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
      FN_ActiveFile:= '?';
      if ActiveFile <> nil then
         FN_ActiveFile:= ActiveFile.FileName;

      if ( Location.FileName <> '' ) and ( Location.FileName <> FN_ActiveFile ) then begin
        if IgnoreOtherFiles then
           exit;

        if Copy(Location.FileName,1,Length(LINK_RELATIVE_SETUP)) = LINK_RELATIVE_SETUP then     // KNT Links are interpreted by default relative to the installation folder (see #838)
           delete(Location.FileName,1,Length(LINK_RELATIVE_SETUP));

        FN:= GetAbsolutePath(ExtractFilePath(Application.ExeName), Location.FileName);

        if FN.ToUpper <> FN_ActiveFile.ToUpper then begin
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
                    App.ShowInfoInStatusBar(GetRS(sLnk11));
                    raise EInvalidLocation.Create(Format( GetRS(sLnk12), [origLocationStr] ));
                 end
                 else begin
                    App.ShowInfoInStatusBar(GetRS(sLnk33));
                    exit;
                 end;
              end;

           end;
        end;

      end;

      UpdateLocation(Location);
      myFolder:= Location.Folder;
      myTreeNode:= Location.Node;

      _Executing_JumpToKNTLocation_ToOtherNote:= false;
      try
         // if not current folder, switch to it
         if ( myFolder <> ActiveFolder ) then begin
           _Executing_JumpToKNTLocation_ToOtherNote:= true;
           App.ActivateFolder(myFolder);
         end;

         if assigned( myTreeNode ) then begin
            // select the node
            with myFolder.TreeUI do begin
               if (FocusedNode <> myTreeNode) or not TV.IsEffectivelyVisible[myTreeNode] then begin
                  if FocusedNode = myTreeNode then
                     myFolder.NodeSelected(myTreeNode, nil);
                  MakePathVisible(myTreeNode);       // It could be hidden
                  MakePathNonFiltered(myTreeNode);   // It also could be filtered
                  SelectAlone(myTreeNode);
               end;
            end;
         end
         else
           myTreeNode:= myFolder.TV.FocusedNode;


         result := true;

         if not SearchTargetMark (Location.Bookmark09) then begin
            if WordInRS <> nil then begin                             // Jump to a word in a serch result
               if WordInRS.BeginOfParagraph >= 0 then
                  SearchCaretPos(myFolder.Editor, WordInRS.BeginOfParagraph, 0, True, Location.ScrollPosInEditor, true, true, true);
               SearchCaretPos(myFolder.Editor, WordInRS.WordPos, WordInRS.WordSel, True, Location.ScrollPosInEditor, false, true, true);
            end
            else
               SearchCaretPos(myFolder.Editor, Location.CaretPos, Location.SelLength, True, Location.ScrollPosInEditor,
                             AdjustVisiblePosition, true, ConsiderOffset);
         end;

         myFolder.Editor.SetFocus;

      finally
         _Executing_JumpToKNTLocation_ToOtherNote:= false;
      end;


      if _Executing_History_Jump then begin
         if (LocBeforeJump.FolderID <> Location.FolderID) then
            TKntHistory(myFolder.History).SyncWithLocation (Location, hdBack, true);
      end
      else
      if (LocBeforeJump <> nil) and
         (LocBeforeJump.FolderID = myFolder.ID) and (myFolder.FocusedNNode.GID = LocBeforeJump.NNodeGID) then begin
          AddHistoryLocation (myFolder, false, LocBeforeJump);
          _LastMoveWasHistory:= false;
          UpdateHistoryCommands;
      end;


    except
      on E : EInvalidLocation do
        if not _Executing_History_Jump and not Location.Bookmark09 then
          App.WarningPopup( Format( GetRS(sLnk13), [E.Message] ));
      on E : Exception do begin
        App.ShowInfoInStatusBar(GetRS(sLnk14));
        App.WarningPopup( Format( GetRS(sLnk15), [E.Message] ));
      end;
  end;

end; // JumpToLocation


procedure OpenLocationInOtherInstance( aLocation : TLocation );
var
  Args: string;
  exresult: integer;
begin
   //-ignSI Ignore single instance option for this call (-> Ensure the file will open in another instance and not finally in this one..)
   Args:= Format('-ignSI -jmp"%s"', [BuildKNTURL(aLocation)]);
   exresult := ShellExecute( 0, 'open', PChar( Application.ExeName ), PChar( Args ), nil, SW_NORMAL );
   if ( exresult <= 32 ) then
      App.ErrorPopup( TranslateShellExecuteError( exresult ));
end;


//===============================================================
// JumpToKNTLocation
//===============================================================
procedure JumpToKNTLocation( LocationStr : string; myURLAction: TURLAction = urlOpen; OpenInCurrentFile: boolean= false );
var
  Location : TLocation;
begin
  try
    App.HideNestedFloatingEditors;
    if FloatingEditorCannotBeSaved then exit;

    Location:= nil;
    Location:= BuildLocationFromKntURL(LocationStr);
    try
       JumpToLocation(Location, false, true, myURLAction, OpenInCurrentFile);
    finally
       Location.Free;
    end;

  except
    on E : EInvalidLocation do begin
      if Location <> nil then
         Location.Free;
      App.WarningPopup( Format( GetRS(sLnk13), [E.Message] ));
    end;

    on E : Exception do begin
      App.ShowInfoInStatusBar(GetRS(sLnk14));
      App.ErrorPopup( Format( GetRS(sLnk15), [E.Message]  ));
    end;
  end;

end; // JumpToKNTLocation



function URLFileExists (var URL: string): boolean;
var
   AbsolutePath: string;
begin
   AbsolutePath:= GetAbsolutePath(ExtractFilePath(ActiveFile.FileName), StripFileURLPrefix(URL));
   Result:= FileExists( AbsolutePath) or DirectoryExists( AbsolutePath );
end;


//--------------------------------------------------
// TypeURL
//--------------------------------------------------
function TypeURL (var URLText: string; var KNTlocation: boolean): TKNTURL;
var
   URLType, KntURL: TKNTURL;
   URLPos, MinURLPos: integer; // position at which the actual URL starts in URLText
   URLTextLower: string;
   URLaux, URLaux2: string;
   pParams, p, p1, p2, p3: integer;
begin
  // determine where URL address starts in URLText
  URLType := urlUndefined;
  if URLText = '' then begin
     Result:= urlUndefined;
     exit;
  end;


  // *1  Consider the following case: "https://web.archive.org/web/20171103002946/http://www.s3graphics.com/en/"
  // *2  Consider this case: "D:\@Developers\Path\file.txt"

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
      if ( pos( 'WWW.', AnsiUpperCase(URLText) ) > 0 ) then begin
          URLText := 'http://' + trim(URLText);
          URLType := urlHttp;
      end;

  KNTlocation:= False;   // By default

  if (URLType = urlUndefined) then begin
      p:= pos( ':', URLText );
      if ( pos( '@', URLText ) > 0 ) and (p <= 0) then begin       // *2
          URLText := 'mailto:' + trim(URLText);
          URLType := urlMailto;
      end
      else
      if p <= 2 then
         URLType := urlFile;
  end;


  if (URLType = urlFile) then begin
      pParams:= pos(KeyOptions.URLFileSepParams, URLText);
      if pParams = 0 then
         pParams:= integer.MaxValue;
      p1:= pos(KNTLOCATION_MARK_NEW2, URLText);
      p2:= pos(KNTLOCATION_MARK_NEW, URLText);
      p3:= pos(KNTLOCATION_MARK_OLD,  URLText);

      if ((p1 > 0) and (p1 < pParams)) or
         ((p2 > 0) and (p2 < pParams)) or
         ((p3 > 0) and (p3 < pParams))  then
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


procedure AdaptURLFileWithParams (var myURL: string; FromUser: boolean);
var
  p: integer;
  Parameters: string;
begin
  if myURL = '' then exit;

  if FromUser then
     myURL:= StringReplace(myURL, '"', '''''', [rfReplaceAll])
  else
     myURL := StringReplace(myURL, '''''', '"', [rfReplaceAll]);   // For the user, show and use ", but internally use two '

  if ActiveEditor.PlainText and
     (KeyOptions.URLFileSepParams <> '') and (KeyOptions.URLFileSpaceInParams <> '') then begin
     p:= pos(KeyOptions.URLFileSepParams, myURL);
     if p > 0 then begin
        Parameters:= Copy(myURL, p);
        if FromUser then
           Parameters:= StringReplace(Parameters, ' ', KeyOptions.URLFileSpaceInParams, [rfReplaceAll])
        else
           Parameters:= StringReplace(Parameters, KeyOptions.URLFileSpaceInParams, ' ', [rfReplaceAll]);
        myURL:= Copy(myURL, 1, p-1) + Parameters;
     end;
  end;
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

    Editor.BeginUpdate;
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
        Editor.EndUpdate;
    End;

End;



//===============================================================
// ClickOnURL
//===============================================================
procedure ClickOnURL(const URLstr: string; chrgURL: TCharRange; myURLAction: TURLAction; EnsureAsk: boolean = false; Button: TMouseButton = mbLeft);
var
  ShellExecResult : integer;
  Form_URLAction: TForm_URLAction;
  //myURLAction : TURLAction;
  browser : string;
  URLType : TKNTURL;
  myURL : string; // the actual URL
  TextURL : string; // the text shown for actual URL
  textURLposIni, textURLposFin, p: Integer;
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
         if App.ShowingImageOnTrack and ImageMng.ImgViewerIsOpen then
            App.ShowingImageOnTrack:= false
         else begin
           ClickOnURLImage (URLstr, chrgURL, myURLAction, EnsureAsk);
           App.ShowingImageOnTrack:= true;
         end;
      end;

      exit;
  end
  else
  if (URLType = urlKNTFold) then begin
     if CtrlDown or AltDown then begin
        if SecondsBetween(Now, TKntRichEdit.LastFoldingTime) > 1 then
           Editor.Unfold(CtrlDown);
     end
     else
     if Button <> mbRight then
        Editor.PreviewFoldedBlock(Editor.SelStart);
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
              path:= GetTextURLFromKntURL(myURL);
              Form_URLAction.AllowURLModification:= false;
              Form_URLAction.Edit_URL.Text := path;
           end
           else begin
              if URLType = urlFILE then
                 AdaptURLFileWithParams(myURL, false);
              Form_URLAction.Edit_URL.Text := myURL;
           end;

          // Seleccionar el texto correspondiente al hipervinculo
          usesHyperlinkCmd:= true;
          TextURL:= TextOfLink(chrgURL.cpMax-1, textURLposIni, textURLposFin);
          if TextURL = '' then begin
             usesHyperlinkCmd:= false;
             if not ActiveEditor.PlainText then
                if length(URLstr) < Length(myURL) then
                   Form_URLAction.Edit_TextURL.Text := URLstr
                else
                   Form_URLAction.Edit_TextURL.Text := Form_URLAction.Edit_URL.Text;
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
                 if URLType = urlFILE then
                    AdaptURLFileWithParams(myURL, true);
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
             Location:= BuildLocationFromKntURL(myURL);
             InsertOrMarkKNTLink(Location, true, TextURL);
          end
          else
             InsertURL(myURL, TextURL, Editor);

          App.ShowInfoInStatusBar(GetRS(sLnk17));
          exit;
      end;

      //-------------------------------------
      if ( myURLAction = urlNothing ) then begin
         App.ShowInfoInStatusBar(GetRS(sLnk18));
         exit;
      end;

      if ( myURLAction in [urlCopy, urlBoth] ) then begin
          if KNTLocation then
             Clipboard.AsText:= URLstr      // includes file prefix
          else
             Clipboard.AsText:= myURL;

          App.ShowInfoInStatusBar(GetRS(sLnk19));
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

                // Process optional params
                p:= pos(KeyOptions.URLFileSepParams, myURL);
                if p = 0 then
                   FileName:= myURL
                else begin
                   FileName:= Copy(myURL,1, p-1);
                   Parameters:= Copy(myURL, p + Length(KeyOptions.URLFileSepParams));
                   Parameters:= StringReplace(Parameters, '''''', '"', [rfReplaceAll]);
                   if (KeyOptions.URLFileSpaceInParams <> '') and ActiveEditor.PlainText then
                      Parameters:= StringReplace(Parameters, KeyOptions.URLFileSpaceInParams, ' ', [rfReplaceAll]);
                end;

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
              App.ErrorPopup(Format(GetRS(sLnk20), [ShellExecResult, myURL, TranslateShellExecuteError(ShellExecResult)] ));
          end
          else begin
            if KeyOptions.MinimizeOnURL then
               Application_Minimize;
          end;
      end;

  except
     on E : Exception do
       App.WarningPopup(E.Message);
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
        if URLType = urlFILE then
           AdaptURLFileWithParams(URLStr, false);
        Form_URLAction.Edit_URL.Text := URLStr;
        Form_URLAction.Edit_TextURL.Text := TextURL;
        Form_URLAction.URLAction:= urlCreateOrModify;   // Mode: Create. Only will show buttons Ok and Cancel

        if ( Form_URLAction.ShowModal = mrOK ) then begin
            URLStr := trim( Form_URLAction.Edit_URL.Text );
            TextURL:= trim( Form_URLAction.Edit_TextURL.Text );
            URLType := TypeURL( URLStr, KNTLocation );
            if URLType = urlFILE then
               AdaptURLFileWithParams(URLStr, true);
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



function ConvertKNTLinksToNewFormat(const Buffer: Pointer; BufSize: integer; NoteGIDs: TMergedNotes; FolderIDs: array of TMergeFolders;
                                    var GIDsNotConverted: integer): AnsiString;
const
   KntLINK_PREFIX_2 = 'file:///*';
   KntLINK_PREFIX_1 = '"' + KntLINK_PREFIX_2;
   KntLINK_PREFIX_4 = 'file:///<';
   KntLINK_PREFIX_3 = '"' + KntLINK_PREFIX_4;

var
  pIn, pOut,  pLink, pLinkEnd, k : integer;
  RTFTextOut, NewFormat: AnsiString;
  NBytes: integer;
  LinksConverted: boolean;
  RTFText: PAnsiChar;
  StrAux: AnsiString;
  i, L: integer;


  function GetNewFolderID(FolderID: Cardinal): Cardinal;
   var
     i: integer;
  begin
    Result:= 0;
    for i := 0 to High(FolderIDs) do
       if FolderID = FolderIDs[i].oldID then begin
          Result := FolderIDs[i].newID;
          exit;
       end;
  end;

  function RTFLinkToNewFormat(Buffer: Pointer; LinkOffset: integer; var PosLinkEnd: integer): string;
  var
    pf, pIni, i: integer;
    RTF: PAnsiChar;
    Location: TLocation;
    Link: AnsiString;
    NNode: TNoteNode;
    NewFolderID: Cardinal;

  begin
    Result:= '';

    try
        // RTF (+ LinkOffset): Text that starts with {\field{\*\fldinst{HYPERLINK "file:///*...   (or ...HYPERLINK file:///*...  )
        (*
            We are only going to automatically convert the usual links, within the same file::

             ...{\field{\*\fldinst{HYPERLINK "file:///* ..."}}{...
             ...{\field{\*\fldinst{HYPERLINK file:///*  ... }}{...
             ...{\field{\*\fldinst{HYPERLINK "file:///<  ... }}{...
        *)

        RTF:= PAnsiChar(Buffer);

        pIni:= 1 + LinkOffset + Length(LINK_PREFIX);
        pf:= PosPAnsiChar('}}', RTF, pIni);

        PosLinkEnd:= pf+1  -1;      // Point to the last character => RTF[PosLinkEnd] = '}'
        i:= 0;
        if RTF[pIni-1] = '"' then
           i:= 1;

        Link:= Copy(RTF, pIni +i, PosLinkEnd-pIni-1 -i);
        Location:= BuildLocationFromKntURL(Link);
        try
          if Location = nil then exit;

          if assigned(NoteGIDs) then begin
             if (Location.NNodeGID <> 0 ) then begin
                 Location.NNodeGID:= NoteGIDs.GetNewGID(Location.NNodeGID);
                 if Location.NNodeGID = NoteGID_NotConverted then
                    inc(GIDsNotConverted);
             end
             else begin
                 NewFolderID := GetNewFolderID(Location.FolderID);
                 if NewFolderID > 0 then begin
                    Location.FolderID:= NewFolderID;
                    Location.Folder:= ActiveFile.GetFolderByID (NewFolderID);
                 end;
             end;
          end;

          if (Location.NNodeGID = 0) and (Location.Folder <> nil) then begin
             NNode:= Location.Folder.GetNNodeByID(Location.NNodeID);
             if NNode <> nil then
                Location.NNodeGID:= NNode.GID;
          end;

          Result:= '"' + BuildKNTURL(Location) + '"}}';
          Result:= Copy(RTF, 1+LinkOffset, pIni - LinkOffset-1) + Result;

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
         k:= 0;
         repeat
            L:= 0;
            pLink:= PosPAnsiChar(LINK_PREFIX, RTFText, pIn + k)-1;
            if (pLink = -1) or (pLink >= BufSize) then
               break;

            StrAux:= CopyPAnsiChar(RTFText, pLink+ Length(LINK_PREFIX)+1, Length(KntLINK_PREFIX_1));
            if Pos(KntLINK_PREFIX_1, StrAux) = 1 then
               L:= 1
            else
            if Pos(KntLINK_PREFIX_2, StrAux) = 1 then
               L:= 2
            else
            if assigned(NoteGIDs) and (Pos(KntLINK_PREFIX_3, StrAux) = 1) then
               L:= 3
            else
            if assigned(NoteGIDs) and (Pos(KntLINK_PREFIX_4, StrAux) = 1) then
               L:= 4;

            if L = 0 then
               k:=  pLink-pIn +  Length(LINK_PREFIX) + Length(KntLINK_PREFIX_1);

         until L <> 0;

         if L= 0 then break;


         NBytes:= pLink - pLinkEnd-1;                // Previous bytes to copy

         if RTFTextOut = '' then begin
            SetLength(RTFTextOut, BufSize);
            {$IF Defined(DEBUG) AND Defined(KNT_DEBUG)}
            for k := 1 to BufSize do
               RTFTextOut[k]:= ' ';  //ZeroMemory(@RTFTextOut[1], BufSize);
            {$ENDIF}
         end;

         // Copy bytes previous to the link found
         Move(RTFText[pLinkEnd+1], RTFTextOut[pOut], NBytes);
         Inc(pOut, NBytes);

         NewFormat:= RTFLinkToNewFormat(RTFText, pLink, pLinkEnd );

         if NewFormat = '' then  begin
            NewFormat:= LINK_PREFIX;
            case L of
               1: NewFormat:= NewFormat + KntLINK_PREFIX_1;
               2: NewFormat:= NewFormat + KntLINK_PREFIX_2;
               3: NewFormat:= NewFormat + KntLINK_PREFIX_3;
               4: NewFormat:= NewFormat + KntLINK_PREFIX_4;
            end;
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
        App.ErrorPopup( GetRS(sLnk20) + E.Message);
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
       GetKNTLocation (aFolder, aLocation, false);                       // false: simplified (register only IDs)
       aLocation.ScrollPosInEditor:= aFolder.Editor.GetScrollPosInEditor;
    end;
    aLocation.SelLength := 0;

    if AddToGlobalHistory then begin
      gLocation:= aLocation.Clone();
      History.AddLocation( gLocation );                        // History: global navigation history
    end;

    if AddLocalMaintainingIndex  then
       TKntHistory(aFolder.History).AddLocationMaintainingIndex (aLocation)
    else
       TKntHistory(aFolder.History).AddLocation(aLocation);


  except
    App.ShowInfoInStatusBar(GetRS(sLnk21));
    TKntHistory(aFolder.History).Clear;
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

  App.HideNestedFloatingEditors;
  if FloatingEditorCannotBeSaved then exit;


{$IFDEF DEBUG_HISTORY}
  if KntFile <> nil then begin
    if not _LastMoveWasHistory then
        Form_Main.Res_RTF.Text:= '--------     ' + #13 + 'Last move was history: NO' + #13#13 + Form_Main.Res_RTF.Text;
  end;
{$ENDIF}

  LocBeforeNavigation:= nil;
  GetKntLocation (ActiveFolder, LocBeforeNavigation, false);


  if CtrlDown then begin
     masterHistory:= TKntHistory(ActiveFolder.History);
     slaveHistory := History;
     IterateAllOnSync:= false;
  end
  else begin
     masterHistory:= History;
     slaveHistory := TKntHistory(ActiveFolder.History);
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
           App.ShowInfoInStatusBar(GetRS(sLnk22));
      end;

    finally
      _Executing_History_Jump := false;
      _LastMoveWasHistory := true;
      UpdateHistoryCommands;
    end;

  except
    App.ShowInfoInStatusBar(GetRS(sLnk23));
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
     lHistory:= TKntHistory(ActiveFolder.History);

  with Form_Main do begin
    GoBackEnabled :=        assigned(ActiveFolder) and ( History.CanGoBack or ((lHistory <> nil) and lHistory.CanGoBack) );
    MMHistoryGoBack.Enabled := GoBackEnabled;
    TB_GoBack.Enabled := GoBackEnabled;
    strHint:= GetRS(sLnk24);
    if GoBackEnabled then begin
       Loc:= History.PickBack;
       if (Loc <> nil) and (Loc.FolderID <> ActiveFolder.ID ) then
          TB_GoBack.ImageIndex:= IMAGE_GOBACK_OTHER_NOTE
       else
          TB_GoBack.ImageIndex:= IMAGE_GOBACK_IN_NOTE;

       if not History.CanGoBack then
          strHint:= GetRS(sLnk25)
       else begin
          strHint:= GetRS(sLnk26); //'Navigate backwards in global history';
          if (lHistory <> nil) and lHistory.CanGoBack and (TB_GoBack.ImageIndex= IMAGE_GOBACK_OTHER_NOTE) then
             strHint:= strHint + GetRS(sLnk30);
       end;
    end;
    TB_GoBack.Hint:= strHint;

    GoForwardEnabled:= assigned(ActiveFolder) and (History.CanGoForward or ((lHistory <> nil) and lHistory.CanGoForward) );
    MMHistoryGoForward.Enabled := GoForwardEnabled;
    TB_GoForward.Enabled := GoForwardEnabled;
    strHint:= GetRS(sLnk27);
    if GoForwardEnabled then begin
       Loc:= History.PickForward;
       if (Loc <> nil) and (Loc.FolderID <> ActiveFolder.ID ) then
          TB_GoForward.ImageIndex:= IMAGE_GOFORWARD_OTHER_NOTE
       else
          TB_GoForward.ImageIndex:= IMAGE_GOFORWARD_IN_NOTE;

       if not History.CanGoForward then
          strHint:= GetRS(sLnk28)
       else begin
          strHint:= GetRS(sLnk29);
          if (lHistory <> nil) and lHistory.CanGoForward and (TB_GoForward.ImageIndex= IMAGE_GOFORWARD_OTHER_NOTE) then
             strHint:= strHint + GetRS(sLnk30);
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
