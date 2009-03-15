unit kn_LinksMng;

interface
uses
  Controls, kn_LocationObj, RichEdit, TreeNT, kn_NoteObj;

   // Links related routines
    procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean );
    procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean ; TextURL: string);
    function BuildKNTLocationText( const aLocation : TLocation ) : string;
    procedure JumpToKNTLocation( LocationStr : string );
    procedure ClickOnURL(const URLstr: String; chrgURL: TCharRange);
    procedure InsertURL(URLStr : string; TextURL : string);

    function PathOfKNTLink (myTreeNode: TTreeNTNode; myNote : TTabNote; position: Integer): string;
    procedure GetTreeNodeFromLocation (const Location: TLocation; var Note: TTabNote; var myTreeNode: TTreeNTNode);

    procedure NavigateToTreeNode(myTreeNode: TTreeNTNode);

    procedure CleanHyperlinks();

    // Navigation history
    procedure AddHistoryLocation( const aNote : TTreeNote );
    procedure NavigateInHistory( const GoForward : boolean );
    procedure UpdateHistoryCommands;

implementation
uses
    Windows, Classes, Forms, SysUtils, Dialogs, StdCtrls, Clipbrd, ShellApi, StrUtils,
    gf_misc, gf_miscvcl, RxRichEd, kn_TreeNoteMng, kn_History, kn_FindReplaceMng,
    kn_Global, kn_Main, kn_Info, kn_Const, kn_URL, kn_RTFUtils, kn_NoteFileMng, kn_NodeList;

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
  STR_16 = ' Hold down SHIFT while clicking the URL:  ';
  STR_17 = ' URL modified';
  STR_18 = ' URL action canceled';
  STR_19 = ' URL copied to clipboard';
  STR_20 = 'Error %d executing hyperlink %s: "%s"';
  STR_21 = ' History error';
  STR_22 = ' Cannot navigate to history location';
  STR_23 = ' History navigation error';

var
   INVALID_CHARS_FN : array[0..8] of string = (
    '*', '?', '"', '<', '>', '|',
    '=', ';', ',');   // this ones are not invalid but very unusual..

//=========================================
// PathOfKNTLink
//=========================================
function PathOfKNTLink (myTreeNode: TTreeNTNode; myNote : TTabNote; position: Integer): string;
var
  path : string;
begin
  if assigned( myTreeNode ) then
  begin
    if TreeOptions.ShowFullPath then
      path := GetNodePath( myTreeNode, TreeOptions.NodeDelimiter, TreeOptions.PathTopToBottom ) // {N}
    else
      path := myTreeNode.Text; // {N}

    if TreeOptions.PathTopToBottom then
      path := Format( '%s%s%s', [myNote.Name, TreeOptions.NodeDelimiter, path] )
    else
      path := Format( '%s%s%s', [path, TreeOptions.NodeDelimiter, myNote.Name] );
  end
  else
     path := myNote.Name;

  if position >= 0 then                  
     path := Format( '%s %d', [path, position] );
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
      if ( NoteID <> 0 ) then // new format
      begin
        Note := notefile.GetNoteByID( NoteID );
        if ( Note = nil ) then
          raise Exception.CreateFmt( STR_01, [NoteID] );
      end
      else begin
        Note := notefile.GetNoteByName( NoteName );
        if ( Note = nil ) then
          raise Exception.CreateFmt( STR_02, [NoteName] );
      end;


      // obtain NODE
      myTreeNode := nil;
      if ( Note.Kind = ntTree ) then
      begin
        if ( NodeID <> 0 ) then begin // new format
          myTreeNode := TTreeNote( Note ).GetTreeNodeByID( NodeID );
          if ( myTreeNode = nil ) then
            raise Exception.CreateFmt( STR_03, [NodeID] );
        end
        else begin
          myTreeNode := TTreeNote( Note ).TV.Items.FindNode( [ffText], NodeName, nil );
          if ( myTreeNode = nil ) then
            raise Exception.CreateFmt( STR_04, [NodeName] );
        end;
      end;
   end;
end;


//----------------------------------------
// InsertHyperlink
//----------------------------------------
procedure InsertHyperlink(URLStr: string; TextURL : string; KNTLink: boolean);
var
  _selectStart: integer;
  _selectionLenght: integer;
  sep: string;
begin
      if (RichEditVersion >= 4) and (not ActiveNote.PlainText) then begin
         _selectStart := ActiveNote.Editor.SelStart;
         _selectionLenght := ActiveNote.Editor.SelLength;
         sep:= '';
         ActiveNote.Editor.SetSelection(_selectStart-1, _selectStart-1, false);
         if ActiveNote.Editor.SelAttributes.Link then
            sep:= ' ';
         ActiveNote.Editor.SelStart:= _selectStart;
         ActiveNote.Editor.SelLength:= _selectionLenght;

         PutRichText('{\rtf1\ansi{\colortbl ;\red0\green0\blue255;}{\fonttbl}' + sep + '\field{\*\fldinst{HYPERLINK "'
            + URLToRTF(URLStr, false ) + '"}}{\fldrslt{\cf1\ul '
            + URLToRTF(TextURL, true) + '}}\cf0\ulnone}',
            ActiveNote.Editor, true, true );
         end
      else begin
          if not KNTLink then
             URLStr := FileNameToURL( URLStr );
          ActiveNote.Editor.SelText := URLStr + #32;
      end;

      ActiveNote.Editor.SelLength := 0;
end;

//===============================================================
// InsertFileOrLink
//===============================================================
procedure InsertFileOrLink( const aFileName : string; const AsLink : boolean );
var
  fn : string;
  oldFilter : string;
  ImportFileType : TImportFileType;
  ext : string;
  RTFAux: TRxRichEdit;

begin
  if ( not ( Form_Main.HaveNotes( true, true ) and assigned( ActiveNote ))) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;

  if ( aFileName = '' ) then
  begin
    with Form_Main.OpenDlg do
    begin
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
  begin
    FN := aFileName;
  end;

    if AsLink then
    begin
      if pos( 'FILE:', AnsiUpperCase(FN) ) = 0 then
         FN := 'file:///' + FN;

      InsertHyperlink(FN, StripFileURLPrefix(FN), false);
    end

    else
    begin
      ext := extractfileext( FN );
      ImportFileType := itText;
      if ( ext = ext_RTF ) then
        ImportFileType := itRTF
      else
      if Form_Main.ExtIsHTML( ext ) then
        ImportFileType := itHTML
      else
      if Form_Main.ExtIsText( ext ) then
        ImportFileType := itText
      else
      begin
        messagedlg( STR_07,
          mtError, [mbOK], 0 );
        exit;
      end;

      ActiveNote.Editor.Lines.BeginUpdate;

      RTFAux := TRxRichEdit.Create( ActiveNote.TabSheet);
      RTFAux.Visible:= False;
      RTFAux.Parent:=ActiveNote.TabSheet ;

      try
        try

        case ImportFileType of
          itText, itHTML : begin
            RTFAux.PlainText := true;
            RTFAux.Lines.LoadFromFile( FN );
            ActiveNote.Editor.SelText := RTFAux.Lines.Text;
            ActiveNote.Editor.SelLength := 0;
          end;

          itRTF : begin
            RTFAux.Lines.LoadFromFile( FN );

            PutRichText(
              GetRichText( RTFAux, true, false ),
              ActiveNote.Editor,
              true, true );

          end;
        end;

        except
          on E : Exception do
          begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            exit;
          end;
        end;

      finally
        ActiveNote.Editor.Lines.EndUpdate;
        RTFAux.Free;
      end;

    end;

    NoteFile.Modified := true;
    UpdateNoteFileState( [fscModified] );

end; // InsertFileOrLink

//===============================================================
// InsertOrMarkKNTLink
//===============================================================
procedure InsertOrMarkKNTLink( aLocation : TLocation; const AsInsert : boolean; TextURL: string);
var
   Note: TTabNote;
   TreeNode: TTreeNTNode;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;
  if ( aLocation = nil ) then
    aLocation := _KNTLocation;

  if AsInsert then
  begin
    // insert link to previously marked location
    if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
    if ( aLocation.NoteName = '') and (aLocation.NoteID = 0) then
    begin
      showmessage( STR_08 );
      exit;
    end;

    if TextURL = '' then begin
       GetTreeNodeFromLocation (aLocation, Note, TreeNode);
       TextURL:= PathOfKNTLink(TreeNode, Note, aLocation.CaretPos);
    end;
    InsertHyperlink(BuildKNTLocationText(aLocation),  TextURL, true);
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_09;

  end
  else
  begin
    // mark caret position as TLocation
    with aLocation do
    begin
      // [x] we should use IDs instead, but the
      // links would then be meaningless to user!
      FileName := normalFN( NoteFile.FileName );
      NoteName := ActiveNote.Name;
      NoteID := ActiveNote.ID;
      if ( ActiveNote.Kind = ntTree ) then
      begin
        NodeName := TTreeNote( ActiveNote ).SelectedNode.Name;
        NodeID := TTreeNote( ActiveNote ).SelectedNode.ID;
      end
      else
      begin
        NodeName := '';
        NodeID := 0;
      end;
      CaretPos := ActiveNote.Editor.SelStart;
      SelLength := ActiveNote.Editor.SelLength;
    end;

    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_10;

  end;

end; // InsertOrMarkKNTLink


//===============================================================
// BuildKNTLocationText
//===============================================================
function BuildKNTLocationText( const aLocation : TLocation ) : string;
var
  LocationString : string;
begin
  if ( aLocation.FileName = normalFN( NoteFile.FileName )) then
    LocationString := ''
  else
    LocationString := FileNameToURL( aLocation.FileName );

    // [x] this does not handle files on another computer, i.e.
    // we cannot do file://computername/pathname/file.knt

    if (RichEditVersion >= 4) and (not ActiveNote.PlainText) then begin
      LocationString := 'file:///' + LocationString + KNTLOCATION_MARK_NEW +
        inttostr( aLocation.NoteID ) + KNTLINK_SEPARATOR +
        inttostr( aLocation.NodeID ) + KNTLINK_SEPARATOR +
        inttostr( aLocation.CaretPos ) + KNTLINK_SEPARATOR +
        inttostr( aLocation.SelLength );
       end
    else begin
      LocationString := 'file:///' + LocationString + KNTLOCATION_MARK_OLD +
        FileNameToURL( aLocation.NoteName ) + KNTLINK_SEPARATOR +
        FileNameToURL( aLocation.NodeName ) + KNTLINK_SEPARATOR +
        inttostr( aLocation.CaretPos ) + KNTLINK_SEPARATOR +
        inttostr( aLocation.SelLength );
    end;

  result := LocationString;
end; // BuildKNTLocationText



//---------------------------------------------------------------
// BuildKNTLocationFromString
//---------------------------------------------------------------
function BuildKNTLocationFromString( LocationStr : string ): TLocation;
type
  EInvalidLocation = Exception;
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
    if ( pnew < pold ) then
    begin
      if ( pnew > 0 ) then
      begin
        NewFormatURL := true;
        p := pnew;
      end
      else
      begin
        NewFormatURL := false;
        p := pold;
      end;
    end
    else
    begin
      if ( pold > 0 ) then
      begin
        NewFormatURL := false;
        p := pold;
      end
      else
      begin
        NewFormatURL := true;
        p := pnew;
      end;
    end;

    // extract filename
    case p of
      0 : raise EInvalidLocation.Create( origLocationStr );
      1 : Location.FileName := ''; // same file as current
      else
      begin
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
        Location.NodeID := 0;
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
      if ( Note.Kind = ntTree ) then
      begin
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


    if ( LocationStr <> '' ) then
    begin
      p := pos( KNTLINK_SEPARATOR, LocationStr );
      if ( p > 0 ) then
      begin
        try
          Location.CaretPos := strtoint( copy( LocationStr, 1, pred( p )));
        except
          Location.CaretPos := 0;
        end;
        delete( LocationStr, 1, p );
        if ( LocationStr <> '' ) then
        begin
          try
            Location.SelLength := strtoint( LocationStr );
          except
            Location.SelLength := 0;
          end;
        end;
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

//===============================================================
// JumpToKNTLocation
//===============================================================
procedure JumpToKNTLocation( LocationStr : string );
type
  EInvalidLocation = Exception;
var
  Location : TLocation;
  myNote : TTabNote;
  myTreeNode : TTreeNTNode;
  origLocationStr : string;
begin

  // Handles links that point to a "KNT location" rather than normal file:// URLs.
  // We may receive two types of links:
  // the old style link: file:///?filename.knt...
  // the new style link: file:///*filename.knt...


  try
    Location:= BuildKNTLocationFromString(LocationStr);
    try
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
      if ( Location.FileName <> '' ) then
      begin
        if (( not fileexists( Location.FileName )) or
         ( NoteFileOpen( Location.FileName ) <> 0 )) then
        begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_11;
          raise Exception.CreateFmt( STR_12, [origLocationStr] );
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
         myTreeNode.MakeVisible;     // It could be hidden
         TTreeNote( ActiveNote ).TV.Selected := myTreeNode;
      end;



      // place caret
      with myNote.Editor do
      begin
        SelStart := Location.CaretPos;
        SelLength := Location.SelLength;
        Perform( EM_SCROLLCARET, 0, 0 );
      end;
      myNote.Editor.SetFocus;
      // StatusBar.Panels[PANEL_HINT].Text := ' Jump to location executed';

    finally
      Location.Free;
    end;

    except
      on E : EInvalidLocation do
      begin
        messagedlg( Format( STR_13, [E.Message] ), mtError, [mbOK], 0 );
        exit;
      end;
      on E : Exception do
      begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_14;
        messagedlg( Format( STR_15, [E.Message] ), mtError, [mbOK], 0 );
        exit;
      end;
  end;

end; // JumpToKNTLocation



//--------------------------------------------------
// TypeURL
//--------------------------------------------------
function TypeURL (var URLText: String; var KNTlocation: boolean): TKNTURL;
var
   URLType, KntURL: TKNTURL;
   URLPos : integer; // position at which the actual URL starts in URLText
begin
  // determine where URL address starts in URLText
  URLType := urlFile;
  for KntURL := low( KntURL ) to high( KntURL ) do
  begin
    URLPos := pos( KNT_URLS[KntURL], URLText );
    if ( URLPos > 0 ) then
    begin
      URLType := KntURL;
      break;
    end;
  end;

  if ( URLPos > 0 ) then
    URLText := copy( URLText, URLPos, length( URLText ))

  else
      if ( pos( '@', URLText ) > 0 ) then begin
          URLText := 'mailto:' + trim(URLText);
          URLType := urlMailto;
          end
      else if ( pos( 'WWW.', AnsiUpperCase(URLText) ) > 0 ) then begin
          URLText := 'http://' + trim(URLText);
          URLType := urlHttp;
          end
      else
          URLType := urlFile;


  if (URLType = urlFile) and (( pos( KNTLOCATION_MARK_NEW, URLText ) > 0 ) or ( pos( KNTLOCATION_MARK_OLD, URLText ) > 0 )) then
      KNTlocation:= True
  else
      KNTlocation:= False;

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
                    esLink:= (ActiveNote.Editor.SelAttributes.Link2 = 1);
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

//------------------------------------------------------
// CleanHyperlinks
//------------------------------------------------------

(*
This function resolve the problem described in issue #59:
   http://code.google.com/p/keynote-nf/issues/detail?id=59

Problematic hyperlink:

{\field{\*\fldinst{HYPERLINK "hyperlink"
\\\\\\\\t "_blank" }}{\fldrslt{\cf2\lang255\ul textOfHyperlink}}}

Correct, "clean" hyperlink:
{\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink}}}

The "_blank" (can be any string) is inoffensive. But the frament \\\\\\\\x
gives too much problem with RichText control. Each time that RTF is readen by
the a RichText control that string is duplicated:
the \\\\\\\\t  is converted to \\\\\\\\\\\\\\\\t, then to
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\t,  etc).

Note that each time you move from a node/note with that problematic hyperlinks to
another node/note the content of the editor is readen and written to the
node/note's stream...
If you don't save the file doesn't matter but if you save the file then
the duplicated strings are saved, and the file can blow out.
*)

procedure CleanHyperlinks();
var
    p, k, lastPosHyperlink : Integer;
    p1, p2: integer;
    link, hidden: Boolean;
    TextLen: Integer;
    startCmdHyperlink, endCmdHyperlink: integer;
    Hyperlink, TextHyperlink: string;
 begin
        TextLen:= ActiveNote.Editor.TextLength;

        ActiveNote.Editor.BeginUpdate;

        p:= 0;
        repeat
            p:= ActiveNote.Editor.FindText('HYPERLINK', p, TextLen, [stWholeWord, stMatchCase]);
            if p < 0 then continue;

            startCmdHyperlink:= p;
            ActiveNote.Editor.SetSelection(p, p + 8, false);
            If not (ActiveNote.Editor.SelText = '') then        // HYPERLINK string doesn't have Hidden mark (nor Link mark) but is treated as such
               endCmdHyperlink:= startCmdHyperlink + 8

            else begin
                p1:= ActiveNote.Editor.FindText('"', p + 9, TextLen-p, []);
                p2:= ActiveNote.Editor.FindText('"', p1 + 1, TextLen-(p1+1), []);
                Hyperlink := ActiveNote.Editor.GetTextRange(p1+1, p2);

                k:= ActiveNote.Editor.FindText('\\\\\\\\\\\\\\\\\\', p2 + 1, TextLen, []);
                if k = p2 +2 then begin
                   k:= ActiveNote.Editor.FindText(' ', k + 1, TextLen, []);
                   if k >= 0 then   // p shouldn't be < 0
                      p2:= k;
                end;

                p := p2 + 1;
                p1:= -1;
                repeat
                    ActiveNote.Editor.SetSelection(p, p+1, false);
                    link:= (ActiveNote.Editor.SelAttributes.Link2 = 1);
                    hidden:= (ActiveNote.Editor.SelText = '');
                    if (p1<0) and (not hidden) then
                        p1:= p;
                    If link or hidden Then
                       lastPosHyperlink := p;
                    p := p + 1;
                Until Not (link or hidden) or (p > TextLen);
                endCmdHyperlink:= lastPosHyperlink;
                TextHyperlink := ActiveNote.Editor.GetTextRange(p1, endCmdHyperlink+1);

                if (endCmdHyperlink-startCmdHyperlink+1) > length(Hyperlink)+Length(TextHyperlink) + 12 then begin // 12=10(>>HYPERLINK <<)+ 2("")
                   ActiveNote.Editor.SetSelection(startCmdHyperlink, endCmdHyperlink +1, false);
                   ActiveNote.Editor.SelText:= '';
                   InsertURL(Hyperlink, TextHyperlink);
                   TextLen:= ActiveNote.Editor.TextLength;
                   endCmdHyperlink:= ActiveNote.Editor.SelStart-1;
                   NoteFile.Modified := True;
                   UpdateNoteFileState( [fscModified] );
                end;
            end;

            p:= endCmdHyperlink +1;
        until (p < 0) or (p >= TextLen);

        ActiveNote.Editor.EndUpdate;

End;


//===============================================================
// ClickOnURL
//===============================================================
procedure ClickOnURL(const URLstr: String; chrgURL: TCharRange);
var
  ShellExecResult : integer;
  Form_URLAction: TForm_URLAction;
  myURLAction : TURLAction;
  browser : string;
  URLType : TKNTURL;
  myURL : string; // the actual URL
  TextURL : string; // the text shown for actual URL
  textURLposIni, textURLposFin: Integer;
  ShiftWasDown, AltWasDown, CtrlWasDown : boolean;
  usesHyperlinkCmd: boolean;

  path: string;
  Location: TLocation;
  KNTlocation: boolean;

  function GetHTTPClient : string;
  begin
    result := '';
    if ( not KeyOptions.URLSystemBrowser ) then
      result := NormalFN( KeyOptions.URLAltBrowserPath );
    if ( result = '' ) then
     result := GetAppFromExt( ext_HTML, true );
  end; // GetHTTPClient

  function KNTPathFromString (url: string): string;
  var
    Location: TLocation;
    note: TTabNote;
    treeNode: TTreeNTNode;
  begin
     Location:= BuildKNTLocationFromString(URL);
     try
       GetTreeNodeFromLocation (Location, note, treeNode);
       Result:= PathOfKNTLink(treeNode, note, Location.CaretPos);
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

  ShiftWasDown := ShiftDown and ( not _IS_FAKING_MOUSECLICK );
  CtrlWasDown := CtrlDown and ( not _IS_FAKING_MOUSECLICK );
  AltWasDown := AltDown and ( not _IS_FAKING_MOUSECLICK );
  _GLOBAL_URLText := '';

  // Determine type of URL. Parameter of TypeURL can also be modified
  myURL := URLstr;
  URLType := TypeURL( myURL , KNTlocation);

  ShellExecResult := maxint; // dummy

  try
    try

      myURLAction := KeyOptions.URLAction; // assume default action

      if AltWasDown then
      begin
        myURLAction := urlCopy;
      end
      else
      if CtrlWasDown then
      begin
        if ( myURLAction <> urlOpenNew ) then
          myURLAction := urlOpenNew // always open in new window if Ctrl pressed
        else
          myURLAction := urlOpen;
      end
      else
      begin
        if (( not _IS_FAKING_MOUSECLICK ) and KeyOptions.URLClickShift and ( not ShiftWasDown )) then
        begin
          if KNTLocation then
             myURL:= '(KNT) ' + KNTPathFromString(URLstr)
          else
             myURL:= URLstr;
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_16 + myURL;
          exit;
        end;
      end;

      if ( URLType = urlFile ) then
      begin
        // various fixes, mostly with XP in mind:

        {1}
        if KeyOptions.URLFileNoPrefix then
          myURL := StripFileURLPrefix( myURL );

        {2}
        if KeyOptions.URLFileDecodeSpaces then
        begin
          myURL := HTTPDecode( myURL );

          {3}
          if ( KeyOptions.URLFileQuoteSpaces and ( pos( #32, myURL ) > 0 )) then
            myURL := '"' + myURL + '"';
        end;

        if ( myURLAction in [urlAsk] ) then
        begin
          if KeyOptions.URLFileAuto then
            myURLAction := urlOpen;
        end;
      end;

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
           else begin
              Form_URLAction.Edit_URL.Text := myURL;
           end;

          // Seleccionar el texto correspondiente al hipervinculo
          usesHyperlinkCmd:= true;
          TextURL:= TextOfLink(chrgURL.cpMax-1, textURLposIni, textURLposFin);
          if TextURL = '' then begin
             Form_URLAction.Edit_TextURL.Text := Form_URLAction.Edit_URL.Text;
             usesHyperlinkCmd:= false;
             end
          else
             Form_URLAction.Edit_TextURL.Text := TextURL;

          Form_URLAction.URLAction:= urlOpen;   // Default action
          Form_URLAction.Button_OpenNew.Enabled := ( URLType in [urlHTTP, urlHTTPS] );
          if ( Form_URLAction.ShowModal = mrOK ) then
          begin
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

      if ( myURLAction = urlCreateOrModify ) then
      begin
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
           InsertURL(myURL, TextURL);

        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_17;
        exit;
      end;

      if ( myURLAction = urlNothing ) then
      begin
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_18;
        exit;
      end;

      if ( myURLAction in [urlCopy, urlBoth] ) then
      begin
        Clipboard.SetTextBuf( PChar( myURL ));
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_19;
      end;

      // urlOpenNew is only for HTTP and HTTPS protocols
      if ( not ( URLType in [urlHTTP, urlHTTPS] )) then
      begin
        if ( myURLAction = urlOpenNew ) then
          myURLAction := urlOpen;
      end;

      if ( myURLAction in [urlOpen, urlOpenNew, urlBoth] ) then
      begin
        case URLType of
          urlFILE : begin // it may be a KNT location or a normal file URL.
            if KNTlocation then
            begin
              // KNT location!
              _GLOBAL_URLText := myURL;
                { Why "postmessage" and not a regular procedure?
                Because we are, here, inside an event that belongs
                to the TTabRichEdit control. When a link is clicked,
                it may cause KeyNote to close this file and open
                a different .KNT file. In the process, this TTabRichEdit
                will be destroyed. If we called a normal procedure
                from here, we would then RETURN HERE: to an event handler
                belonging to a control that NO LONGER EXISTS. Which
                results in a nice little crash. By posting a message,
                we change the sequence, so that the file will be
                closed and a new file opened after we have already
                returned from this here event handler. }
              postmessage( Form_Main.Handle, WM_JumpToKNTLink, 0, 0 );
              exit;
            end
            else
            begin
              ShellExecResult := ShellExecute( 0, 'open', PChar( myURL ), nil, nil, SW_NORMAL );
            end;
          end;
          else // all other URL types
          begin
            myURL := FileNameToURL( myURL );  // We can paste hyperlinks from other programs
            screen.Cursor := crAppStart;
            try
              if ( myURLAction = urlOpenNew ) then
              begin
                ShellExecResult := ShellExecute( 0, 'open', PChar( GetHTTPClient ), PChar( myURL ), nil, SW_NORMAL );
              end
              else
              begin
                if ( URLType in [urlHTTP, urlHTTPS] ) then
                begin
                  if KeyOptions.URLSystemBrowser then
                    ShellExecResult := ShellExecute( 0, 'open', PChar( myURL ), nil, nil, SW_NORMAL )
                  else
                    ShellExecResult := ShellExecute( 0, 'open', PChar( GetHTTPClient ), PChar( myURL ), nil, SW_NORMAL );
                end
                else
                  ShellExecResult := ShellExecute( 0, 'open', PChar( myURL ), nil, nil, SW_NORMAL );
              end;
            finally
              screen.Cursor := crDefault;
            end;
          end;
        end;

        if ( ShellExecResult <= 32 ) then
        begin
          if (( ShellExecResult > 2 ) or KeyOptions.ShellExecuteShowAllErrors ) then
          PopupMessage( Format(
            STR_20,
            [ShellExecResult, myURL, TranslateShellExecuteError(ShellExecResult)] ), mtError, [mbOK], 0 );
        end
        else
        begin
          if KeyOptions.MinimizeOnURL then
            Application.Minimize;
        end;
      end;

    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtWarning, [mbOK], 0 );
      end;
    end;
  finally
    _IS_FAKING_MOUSECLICK := false;
  end;

end; // ClickOnURL

//--------------------------------------------------
// PathFileOK
//--------------------------------------------------
function PathFileOK (const FN: String): boolean;
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
procedure InsertURL(URLStr: string; TextURL : string);
var
  URLType : TKNTURL;
  Form_URLAction: TForm_URLAction;
  askUser: Boolean;
  KNTLocation: boolean;
begin
  if ( not ( Form_Main.HaveNotes( true, true ) and assigned( ActiveNote ))) then exit;
  if Form_Main.NoteIsReadOnly( ActiveNote, true ) then exit;
  askUser:= (URLStr = '');

  if askUser then
     URLStr := trim(ClipboardAsString);         // offer clipboard first

  URLType := TypeURL( URLStr, KNTLocation );
  if (URLType = urlFile) and (not PathFileOK(URLStr)) then begin
      URLStr := '';
      askUser:= true;
  end;

  if askUser then begin
      Form_URLAction := TForm_URLAction.Create( Form_Main );
      try

        Form_URLAction.Edit_URL.Text := URLStr;
        Form_URLAction.Edit_TextURL.Text := '';
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

      if TextURL = '' then TextURL:= StripFileURLPrefix(URLStr);
      InsertHyperlink(URLStr, TextURL, false);
    end;

end; // Insert URL


//=========================================
// AddHistoryLocation
//=========================================
procedure AddHistoryLocation( const aNote : TTreeNote );
var
  myLocation : TLocation;
begin
  if (( not assigned( aNote )) or ( not assigned( aNote.SelectedNode ))) then
    exit;
  myLocation := TLocation.Create;

  try

    myLocation.FileName := notefile.FileName;
    myLocation.NoteName := aNote.Name;
    myLocation.NodeName := aNote.SelectedNode.Name;
    myLocation.CaretPos := aNote.Editor.SelStart;
    myLocation.SelLength := 0;
    myLocation.NoteID := aNote.ID;
    myLocation.NodeID := aNote.SelectedNode.ID;
    aNote.History.AddLocation( myLocation );

  except
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_21;
    aNote.History.Clear;
    myLocation.Free;
  end;

end; // AddHistoryLocation


//=========================================
// NavigateInHistory
//=========================================
procedure NavigateInHistory( const GoForward : boolean );
var
  myLocation : TLocation;
  myHistory : TKNTHistory;
begin
  if ( assigned( ActiveNote ) and ( ActiveNote.Kind = ntTree )) then
  begin
    myHistory := TTreeNote( ActiveNote ).History;
    try
      if GoForward then
      begin
        myLocation := myHistory.GoForward;
      end
      else
      begin
        if ( not _LastMoveWasHistory ) then
        begin
          AddHistoryLocation( TTreeNote( ActiveNote ));
          myHistory.GoBack;
        end;
        myLocation := myHistory.GoBack;
      end;
      try
        _Executing_History_Jump := true;
        if ( not ( assigned( myLocation ) and JumpToLocation( myLocation ))) then
        begin
          if GoForward then
          begin
            while myHistory.CanGoForward do
            begin
              myLocation := myHistory.GoForward;
              if JumpToLocation( myLocation ) then
                break;
            end;
          end
          else
          begin
            while myHistory.CanGoBack do
            begin
              myLocation := myHistory.GoBack;
              if JumpToLocation( myLocation ) then
                break;
            end;
          end;
        end
        else
        begin
          Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_22;
        end;
      finally
        _Executing_History_Jump := false;
        _LastMoveWasHistory := true;
      end;
    except
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_23;
      myHistory.Clear;
    end;
  end;
end; // NavigateInHistory


//=========================================
// UpdateHistoryCommands
//=========================================
procedure UpdateHistoryCommands;
begin
  with Form_Main do begin
      if ( assigned( activenote ) and ( ActiveNote.Kind = ntTree )) then
      begin
        MMTreeGoBack.Enabled := TTreeNote( ActiveNote ).History.CanGoBack;
        MMTreeGoForward.Enabled := TTreeNote( ActiveNote ).History.CanGoForward;
        TB_GoBack.Enabled := MMTreeGoBack.Enabled;
        TB_GoForward.Enabled := MMTreeGoForward.Enabled;
      end
      else
      begin
        TB_GoBack.Enabled := false;
        TB_GoForward.Enabled := false;
        MMTreeGoBack.Enabled := false;
        MMTreeGoForward.Enabled := false;
      end;
  end;
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
            aLinkText := extractfilename( aLinkTarget );
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


end.
