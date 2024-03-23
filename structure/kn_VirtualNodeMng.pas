unit kn_VirtualNodeMng;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)

 [^]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface
uses
   System.SysUtils,
   Vcl.Dialogs,
   Vcl.Controls,
   TreeNT,
   kn_Const,
   kn_NodeList
   ;


    // virtual nodes
    procedure VirtualNodeProc( VMode : TVirtualMode; myTreeNode : TTreeNTNode; VirtFN : string );
    procedure VirtualNodeRefresh( const DoPrompt : boolean );
    procedure VirtualNodeUnlink;
    function GetCurrentVirtualNode : TKntNote;
    procedure VirtualNodeUpdateMenu( const IsVirtual : boolean; const IsKNTVirtual: boolean );
    {$IFDEF WITH_IE}
    function VirtualNodeGetMode( const aNode : TKntNote; var newMode : TVirtualMode; var newFN : string ) : boolean;
    {$ENDIF}

var
   Virtual_UnEncrypt_Warning_Done : boolean;

implementation
uses
   gf_files,
   gf_misc,
   kn_Info,
   kn_Global,
   kn_NoteObj,
   kn_TreeNoteMng,
   kn_NoteFileMng,
   kn_Main;


resourcestring
  STR_01 = 'Virtual node "%s" is currently linked to file "%s". Do you want to link the node to a different file?';
  STR_02 = 'Node "%s" contains text. Do you want to flush this text to a file and make the node virtual?';
  STR_03 = 'This KeyNote file is encrypted, but ' +
           'disk files linked to virtual nodes ' +
           'will NOT be encrypted.' + #13#13 + 'Continue?';
  STR_04 = 'Select file for virtual node';
  STR_05 = 'Only RTF, Text and HTML files can be linked to virtual nodes.';
  STR_06 = 'Cannot link virtual node to a file on removable drive %s:\ ';
  STR_07 = 'You are creating a virtual node linked to file on removable drive %s\. The file may not be available at a later time. Continue anyway?';
  STR_08 = 'Selected file is already linked to a virtual node.';
  STR_09 = 'Virtual node error: ';
  STR_10 = 'Node "%s" represents an Internet Explorer node and cannot be unlinked. Nodes of this type can only be deleted.';
  STR_11 = 'Unlink virtual node "%s"? The contents of the node will be retained, but the link with the file on disk (%s) will be removed.';
  STR_12 = 'Virtual node %s HAS BEEN modified within KeyNote. ' +
           'If the node is refreshed, the changes will be lost. ' +
           'OK to reload the node from file %s?';
  STR_13 = 'Virtual node %s has NOT been modified within KeyNote. ' +
           'OK to reload the node from file %s?';
  STR_14 = 'Error refreshing virtual node: ';
  STR_15 = ' Virtual node refreshed.';
  STR_16 = ' Error refreshing node';
  STR_17 = 'Selected node "%s" is not a virtual node.';
  STR_18 = 'Unlink mirror node "%s"? The contents of the node will be retained but the link with the non virtual node will be removed.';

{$IFDEF WITH_IE}
function VirtualNodeGetMode( const aNode : TKntNote; var newMode : TVirtualMode; var newFN : string ) : boolean;
var
  Form_VNode : TForm_VNode;
begin
  result := false;
  if ( not assigned( aNode )) then exit;
  Form_VNode := TForm_VNode.Create( self );
  try
    Form_VNode.myVirtualMode := aNode.VirtualMode;
    Form_VNode.myVirtualFN := aNode.VirtualFN;
    Form_VNode.myNodeName := aNode.Name;
    if ( Form_VNode.ShowModal = mrOK ) then
    begin
      newMode := Form_VNode.myVirtualMode;
      newFN := Form_VNode.myVirtualFN;
      result := ( newFN <> '' );
    end;
  finally
    Form_VNode.Free;
  end;
end; // VirtualNodeGetMode
{$ENDIF}

procedure VirtualNodeProc( VMode : TVirtualMode; myTreeNode : TTreeNTNode; VirtFN : string );
var
  myNote : TKntNote;
  oldDlgFilter : string;
  ext : string;
  IsVNError, IsFlushingData, IsChangingFile : boolean;
begin
  myNote := nil;
  if ( myTreeNode = nil ) then
    myTreeNode := GetCurrentTreeNode;
  if ( assigned( myTreeNode )) then
    myNote := TKntNote( myTreeNode.Data );

  if ( not assigned( myNote )) then exit;
  IsFlushingData := false;
  IsChangingFile := false;
  IsVNError := false;

  if ( myNote.VirtualMode <> vmNone ) then
  begin
    // Already a virtual node. Ask if user wants
    // to change the file with which the node is linked.
    // Do not prompt if there was an error loading the node
    // (in that case, assume the user DOES want to relink the node)

    {$IFDEF WITH_IE}
    IsChangingFile := true;
    {$ELSE}
    if myNote.HasVNodeError then
    begin
      IsChangingFile := true;
      IsVNError := true;
    end
    else
    begin
      if ( DoMessageBox( Format(STR_01, [myNote.Name, myNote.VirtualFN] ),
      mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
        IsChangingFile := true;
    end;

    {$ENDIF}

    if ( not IsChangingFile ) then
    begin
      exit;
    end;

  end
  else
  begin
    // not a virtual node. If it has text, we have to have an additional prompt
    if ( ActiveKntFolder.Editor.Lines.Count > 0 ) then
    begin
      if ( DoMessageBox( Format(STR_02, [myNote.Name] ),
        mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then
      exit;
      IsFlushingData := true; // needs a SaveDlg, not an OpenDlg
    end;

  end;

  with Form_Main do begin
      if (( KntFile.FileFormat = nffEncrypted ) and ( not Virtual_UnEncrypt_Warning_Done )) then
      begin
        if ( messagedlg(STR_03, mtWarning, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
        Virtual_UnEncrypt_Warning_Done := true;
      end;

      if ( VirtFN = '' ) then
      begin

        if IsFlushingData then
        begin
          // use SaveDlg
          // never true for vmIELocal or vmIERemote
          oldDlgFilter := SaveDlg.Filter;
          SaveDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + FILTER_ALLFILES;
          SaveDlg.Title := STR_04;
          SaveDlg.Filename := myNote.Name;

          try
            if ( not SaveDlg.Execute ) then exit;
          finally
            SaveDlg.Filter := oldDlgFilter;
          end;
          VirtFN := SaveDlg.FileName;
          if ( extractfileext( VirtFN ) = '' ) then
            VirtFN := VirtFN + ext_RTF;
        end
        else
        begin
          {$IFDEF WITH_IE}
          if ( not VirtualNodeGetMode( myNote, VMode, VirtFN )) then exit;
          {$ELSE}
          // use OpenDlg
          oldDlgFilter := OpenDlg.Filter;
          OpenDlg.Filter := FILTER_RTFFILES + '|' + FILTER_TEXTFILES + '|' + FILTER_HTMLFILES + '|' + FILTER_ALLFILES;
          OpenDlg.Title := STR_04;
          if IsVNError then
            OpenDlg.Filename := copy( myNote.VirtualFN, 2, length( myNote.VirtualFN ))
          else
            OpenDlg.Filename := myNote.VirtualFN;

          try
            if ( not OpenDlg.Execute ) then exit;
          finally
            OpenDlg.Filter := oldDlgFilter;
          end;
          VirtFN := OpenDlg.FileName;
          {$ENDIF}
        end; // if IsFlushingData
      end; // if ( VirtFN = '' );

      if ( VMode <> vmIERemote ) then // do not smash case in URLs
      begin
        VirtFN := normalFN( VirtFN );

        if directoryexists( VirtFN ) then
        begin
          // not a file, but a directory - cannot import
          // (user could have drag-dropped a directory, so we must check)
          exit;
        end;

        // these following tests do not apply to IERemote nodes, either
        ext := ExtractFileExt( VirtFN );
        if ( not ( ExtIsRTF( ext ) or ExtIsText( ext ) or ExtIsHTML( ext ))) then
        begin
          messagedlg( STR_05, mtError, [mbOK], 0 );
          exit;
        end;

        // It is not reccommended to link files on virtual media (floppies,
        // CD-ROMs, ZIP drives, etc. So we check.
        if IsDriveRemovable( VirtFN ) then
        begin
          case TreeOptions.RemovableMediaVNodes of
            _REMOVABLE_MEDIA_VNODES_DENY : begin
              MessageDlg( Format(STR_06,[Extractfiledrive( VirtFN )] ), mtError, [mbOK], 0 );
              exit;
            end;
            _REMOVABLE_MEDIA_VNODES_WARN : begin
              if ( messagedlg( Format(STR_07,
                [Extractfiledrive( VirtFN )] ), mtWarning, [mbOK,mbCancel], 0 ) <> mrOK ) then
                  exit;
            end;
            { _REMOVABLE_MEDIA_VNODES_ALLOW or any other value: allow }
          end;
        end;


        // any given file can be linked to a virtual node only once
        // per KNT file. So we must check if the selected file already
        // exists as a virtual node in the currently open KNT file.
        if KntFile.HasVirtualNodeByFileName( myNote, VirtFN ) then
        begin
          messagedlg( STR_08, mtError, [mbOK], 0 );
          exit;
        end;

      end;

      try
        try

          ActiveKntFolder.Editor.OnChange := nil;

          if ( IsChangingFile and ( not ( myNote.VirtualMode in [vmIELocal, vmIERemote] ))) then
          begin
            // Node must save its existing data first:
            if ( not IsVNError ) then
            begin
              ActiveKntFolder.EditorToDataStream;
              myNote.SaveVirtualFile;
            end;
            // now clear the editor
            ActiveKntFolder.Editor.Clear;
            ActiveKntFolder.Editor.ClearUndo;
          end;

          {$IFDEF WITH_IE}
          myNote.VirtualMode := VMode;
          myNote.VirtualFN := VirtFN;
          {$ELSE}
          if ( myNote.VirtualMode in [vmNone, vmText, vmRTF, vmHTML] ) then
            myNote.VirtualMode := VMode; // so that setting new filename will adjust the vm type
          myNote.VirtualFN := VirtFN;
          {$ENDIF}

          // myNote.Stream.LoadFromFile( myNote.VirtualFN );
          if IsFlushingData then
          begin
            // never true for vmIELocal or vmIERemote
            ActiveKntFolder.EditorToDataStream;
            myNote.SaveVirtualFile;
          end
          else
          begin
            myNote.LoadVirtualFile;
            ActiveKntFolder.DataStreamToEditor;
          end;
          VirtualNodeUpdateMenu( true, false );
          myTreeNode := GetCurrentTreeNode;
          SelectIconForNode( myTreeNode, ActiveKntFolder.IconKind );
          if ( TreeOptions.AutoNameVNodes and ( not IsFlushingData )) then
          begin
            myNote.Name := ExtractFilename( myNote.VirtualFN ); // {N}
            (* [x] ImportFileNamesWithExt ignored for virtual nodes, because it is useful to have extension visible
            if KeyOptions.ImportFileNamesWithExt then
              myNote.Name := ExtractFilename( myNote.VirtualFN ) // {N}
            else
              myNote.Name := ExtractFilenameNoExt( myNote.VirtualFN );
            *)
            myTreeNode.Text := myNote.Name;
          end;

        except
          on E : Exception do
          begin
            myNote.VirtualFN := '';
            messagedlg( STR_09 + E.Message,
              mtError, [mbOK], 0 );
          end;
        end;

      finally

        KntFile.Modified := true;
        UpdateKntFileState( [fscModified] );
        ActiveKntFolder.Editor.Modified := false;
        ActiveKntFolder.Editor.OnChange := RxRTFChange;

      end;
  end;
end; // VirtualNodeProc


procedure VirtualNodeUnlink;
var
  myNote : TKntNote;
  myTreeNode, originalTreeNode : TTreeNTNode;
begin
  myNote := GetCurrentVirtualNode;
  if ( not assigned( myNote )) then exit;
  myTreeNode := GetCurrentTreeNode;
  if ( not assigned( myTreeNode )) then exit;

  // cannot unlink vmIERemote virtual nodes,
  // because there's no local file

  if ( myNote.VirtualMode in [vmIELocal, vmIERemote] ) then
  begin
    DoMessageBox( Format(STR_10, [myNote.Name] ), mtError, [mbOK], 0 );
    exit;
  end;

  if (myNote.VirtualMode= vmKNTNode) then begin
       originalTreeNode:= myNote.MirrorNode;
       if assigned(originalTreeNode) and assigned(TKntNote(originalTreeNode.Data)) then
          if ( DoMessageBox( Format(STR_18, [myNote.Name, myNote.VirtualFN] ),
            mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
          begin
            try
              RemoveMirrorNode(originalTreeNode, myTreeNode);
              myNote.MirrorNode:= nil;
              TKntNote(originalTreeNode.Data).Stream.SaveToStream(myNote.Stream);
              ActiveKntFolder.Modified := true;
              VirtualNodeUpdateMenu( false, false );
              SelectIconForNode( myTreeNode, ActiveKntFolder.IconKind );
            finally
              KntFile.Modified := true;
              UpdateKntFileState( [fscModified] );
            end;
          end;

  end
  else
      if ( DoMessageBox( Format(STR_11, [myNote.Name, myNote.VirtualFN] ),
        mtConfirmation, [mbOK, mbCancel], 0 ) = mrOK ) then
      begin
        try
          myNote.VirtualMode := vmNone;
          myNote.VirtualFN := '';
          ActiveKntFolder.Modified := true;
          VirtualNodeUpdateMenu( false, false );
          SelectIconForNode( myTreeNode, ActiveKntFolder.IconKind );
        finally
          KntFile.Modified := true;
          UpdateKntFileState( [fscModified] );
        end;
      end;

end; // VirtualNodeUnlink

procedure VirtualNodeRefresh( const DoPrompt : boolean );
var
  myNote : TKntNote;
begin
  myNote := GetCurrentVirtualNode;
  if ( not assigned( myNote )) then exit;

  // if ( ActiveKntFolder.FocusMemory <> focTree ) then exit;

  if myNote.RTFModified then
  begin
    if ( DoMessageBox( Format(STR_12,
      [myNote.Name, ExtractFilename( myNote.VirtualFN )] ),
      mtWarning, [mbOK,mbCancel], 0 ) <> mrOK ) then
    exit;
  end
  else
  if DoPrompt then
  begin
    if ( DoMessageBox( Format(STR_13,
      [myNote.Name, ExtractFilename( myNote.VirtualFN )] ),
      mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK ) then
    exit;
  end;

  with Form_Main do begin
      ActiveKntFolder.Editor.Lines.BeginUpdate;
      ActiveKntFolder.Editor.OnChange := nil;
      try
        try
          myNote.LoadVirtualFile;
        except
          on E : Exception do
          begin
            messagedlg( STR_14 + E.Message, mtError, [mbOK] , 0 );
            exit;
          end;
        end;

        try
          ActiveKntFolder.Editor.Clear;
          ActiveKntFolder.Editor.ClearUndo;
          ActiveKntFolder.DataStreamToEditor;
          StatusBar.Panels[PANEL_HINT].Text := STR_15;
        except
          StatusBar.Panels[PANEL_HINT].Text := STR_16;
        end;

      finally
        ActiveKntFolder.Editor.Lines.EndUpdate;
        KntFile.Modified := true;
        UpdateKntFileState( [fscModified] );
        ActiveKntFolder.Editor.OnChange := RxRTFChange;
      end;
  end;

end; // VirtualNodeRefresh

procedure VirtualNodeUpdateMenu( const IsVirtual : boolean; const IsKNTVirtual: boolean );
begin
  with Form_Main do begin
      TVVirtualNode.Checked := IsVirtual and not IsKNTVirtual;
      TVRefreshVirtualNode.Enabled := IsVirtual and not IsKNTVirtual;
      TVUnlinkVirtualNode.Enabled := IsVirtual;

      TVNavigateNonVirtualNode.Enabled := IsKNTVirtual;
      //TVInsertMirrorNode.Enabled := true;
  end;
end; // VirtualNodeUpdateMenu

function GetCurrentVirtualNode : TKntNote;
begin
  result := GetCurrentNoteNode;
  if ( result = nil ) then exit;
  if ( result.VirtualMode = vmNone ) then
  begin
    DoMessageBox( Format(
      STR_17,
      [result.Name] ), mtError, [mbOK], 0 );
    result := nil;
  end;
end; // GetCurrentVirtualNode

initialization
  Virtual_UnEncrypt_Warning_Done := false;
end.
