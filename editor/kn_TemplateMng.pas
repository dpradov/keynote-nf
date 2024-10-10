unit kn_TemplateMng;

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
   System.Classes,
   System.SysUtils,
   System.IOUtils,
   Vcl.Forms,
   Vcl.Controls,
   Vcl.Dialogs,
   Vcl.StdCtrls
   ;



var
    Template_Folder : string;
    Template_LastWasFormatted : boolean;
    LastTemplateUsed : string;

    // template functions
    procedure CreateTemplate;
    procedure InsertTemplate( tplFN : string );
    procedure LoadTemplateList;
    procedure RemoveTemplate;
    function GetTemplateIconIndex( const fn : string ) : integer;


implementation
uses
   RxRichEd,
   gf_misc,
   gf_files,
   gf_streams,
   kn_Global,
   kn_Const,
   kn_Info,
   kn_Main,
   kn_NewTemplate,
   kn_NoteFileMng,
   knt.ui.editor,
   kn_EditorUtils,
   knt.App
   ;


resourcestring
  STR_01 = 'Template "%s" already exists. Overwrite existing template?';
  STR_02 = 'Template "%s" created.';
  STR_03 = 'Select template to insert';
  STR_04 = 'OK to delete selected template "%s"?';


procedure LoadTemplateList;
var
  i : integer;
  list : TStringList;
begin
  with Form_Main do begin
      ListBox_ResTpl.Items.BeginUpdate;
      list := TStringList.Create;
      try
        try
          ListBox_ResTpl.Items.Clear;

          // we must run GetFilesInFolder twice:
          // once for *.rtf, and once for *.txt files
          // This RELIES on the fact that GetFilesInFolder
          // does NOT clear the TStrings passed to it.
          GetFilesInFolder(
            Template_Folder,
            '*'+Ext_RTF,
            false,
            false,
            list
          );
          GetFilesInFolder(
            Template_Folder,
            '*'+Ext_TXT,
            false,
            false,
            list
          );

          for i := 1 to list.Count do
          begin
            ListBox_ResTpl.AddItem(
              list[pred( i )],
              cbUnchecked,
              GetTemplateIconIndex( list[pred( i )] )
            );
          end;

        except
        end;
      finally
        ListBox_ResTpl.Items.EndUpdate;
        if ( ListBox_ResTpl.Items.Count > 0 ) then
          ListBox_ResTpl.ItemIndex := 0;
        List.Free;
      end;
  end;
end; // LoadTemplateList


function GetTemplateIconIndex( const fn : string ) : integer;
begin
  result := TEMPLATE_IMAGE_BASE;
  if ( ExtractFileExt( lowercase( fn )) <> ext_RTF ) then
    inc( result );
end;


procedure CreateTemplate;
var
  Form_Template : TForm_Template;
  UseSelection, ReplaceExisting : boolean;
  fn: string;
  F: TFileStream;
  i : integer;
  Editor: TRxRichEdit;
  Encoding: TEncoding;

begin
  Encoding:= nil;

  with Form_Main do begin
      if ( not HaveKntFolders( true, true )) then exit;
      if ( not assigned( ActiveFolder )) then exit;

      Editor:= ActiveFolder.Editor;

      UseSelection := ( Editor.SelLength > 0 );
      ReplaceExisting := false;

      if ( not checkfolder( 'Template', Template_Folder, true, true )) then
         exit;

      Form_Template := TForm_Template.Create( Form_Main );

      try
        with Form_Template do begin
          if UseSelection then begin
             RG_Source.ItemIndex := 0;
             Edit_Name.Text := MakeValidFilename( Editor.SelText, [' '], MAX_FILENAME_LENGTH );
          end
          else begin
             RG_Source.ItemIndex := 1;
             RG_Source.Enabled := false;
             Edit_Name.Text := MakeValidFilename( ActiveFolder.Name, [' '], MAX_FILENAME_LENGTH );
          end;
          CB_Formatted.Checked := Template_LastWasFormatted;
        end;

        if ( Form_Template.ShowModal = mrOK ) then begin
          with Form_Template do begin
             fn := trim( Edit_Name.Text );
             Template_LastWasFormatted := CB_Formatted.Checked;
             UseSelection := (( RG_Source.ItemIndex = 0 ) and ( RG_Source.Enabled ));
          end;

          if Template_LastWasFormatted then
             fn := Template_Folder + fn + ext_RTF
          else
             fn := Template_Folder + fn + ext_TXT;

          if FileExists(fn) then begin
             if (App.DoMessageBox(Format(STR_01, [ExtractFilename(fn)]), mtConfirmation, [mbOK,mbCancel] ) <> mrOK) then
                exit
             else
                ReplaceExisting := true;
          end;

          if ReplaceExisting then begin
             i := ListBox_ResTpl.Items.IndexOf( ExtractFilename(fn));
             if ( i >= 0 ) then
                ListBox_ResTpl.Items.Delete(i);
          end;

          if Template_LastWasFormatted then
             Editor:= ActiveFolder.Editor.GetRichEditorWithNoKNTHiddenCharacters(hmAll, useSelection);       // If editor returned <> ActiveFolder.Editor -> free

          if UseSelection then
             Editor.StreamMode := [smSelection];
          if Template_LastWasFormatted then
             Editor.StreamFormat := sfRichText
          else begin
             Editor.StreamFormat := sfPlainText;
             Encoding:= TEncoding.UTF8;
          end;


          try
             F:= TFileStream.Create(fn, (fmCreate or fmShareExclusive));

          except
            on E : Exception do begin
               messagedlg( E.Message, mtError, [mbOK], 0 );
               exit;
            end;
          end;

          try
            try
              Editor.Lines.SaveToStream( F, Encoding);

              if KeyOptions.ResPanelShow then begin
                 i := ListBox_ResTpl.AddItem(ExtractFilename(fn), cbUnchecked, GetTemplateIconIndex(fn) );
                 ListBox_ResTpl.ItemIndex := i;
              end;
              StatusBar.Panels[PANEL_HINT].Text := Format(STR_02, [ExtractFilename( fn )]  );

            except
              on E : Exception do begin
                messagedlg( E.Message, mtError, [mbOK], 0 );
                exit;
              end;
            end;

          finally
            F.Free;
          end;

        end;
      finally
        if Editor <> ActiveFolder.Editor then
           Editor.Free;          // RTFAux...

        Form_Template.Free;
      end;
  end;

end; // CreateTemplate

procedure InsertTemplate( tplFN : string );
var
  oldFilter : string;
  tplText: string;
  IsRTF: boolean;
  RTFText: AnsiString;
  Editor: TKntRichEdit;
begin
  with Form_Main do begin
      if not assigned(ActiveFolder) and not assigned(ActiveEditor) then exit;

      try
        if not ActiveEditor.Focused then          // Can be Scratchpad editor
           ActiveFolder.Editor.SetFocus;
      except
      end;

      Editor:= ActiveEditor;
      if not App.CheckActiveEditorNotReadOnly then exit;



      if ( not checkfolder( 'Template', Template_Folder, true, false )) then
        exit;

      if ( tplFN <> '' ) then begin
        if ( pos( '\', tplFN ) = 0 ) then
          tplFN := Template_Folder + tplFN;

      end
      else begin
          with OpenDlg do begin
            oldFilter := Filter;
            Filter := FILTER_TEMPLATES;
            FilterIndex := 1;
            Title := STR_03;
            Options := Options - [ofAllowMultiSelect];
            InitialDir := Template_Folder;
            FileName := LastTemplateUsed;
          end;

          try
            if OpenDlg.Execute then
              tplFN := normalFN( OpenDlg.FileName )
            else
              exit;
          finally
            OpenDlg.Filter := oldFilter;
          end;

      end;

      if (not FileExists( tplFN )) then exit;

      LastTemplateUsed := ExtractFilename( tplFN );


      try
        try
          if (ImageMng.StorageMode <> smEmbRTF) and Editor.SupportsRegisteredImages then begin
             if Editor.SelLength > 0 then
                TKntRichEdit(Editor).CheckToSelectLeftImageHiddenMark;
          end;

          tplText:= ReadAllText(tplFN);      // gf_streams
          IsRTF:= tplText.StartsWith('{\rtf1');

          RTFText:= '';
          if IsRTF and Editor.SupportsRegisteredImages then
             RTFText:= ImageMng.ProcessImagesInRTF(tplText, ActiveFolder.Name, ImageMng.ImagesMode, 'Template');

          if RTFText <> '' then
             Editor.PutRtfText(RTFText, True, True)
          else
             Editor.PutRtfText(tplText, true);

        except
          on E : Exception do
          begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            exit;
          end;
        end;
      finally
        App.FileSetModified;
      end;
  end;

end; // InsertTemplate

procedure RemoveTemplate;
var
  i : integer;
  fn : string;
begin
  with Form_Main do begin
      i := ListBox_ResTpl.ItemIndex;
      if (i < 0 ) then exit;
      fn := Template_Folder + ListBox_ResTpl.Items[i];
      if (App.DoMessageBox( Format(STR_04, [ListBox_ResTpl.Items[i]]), mtConfirmation, [mbOK, mbCancel] ) <> mrOK ) then
          exit;

      ListBox_ResTpl.Items.Delete( i );
      DeleteFile( fn );

      if ( ListBox_ResTpl.Items.Count > 0 ) then
         ListBox_ResTpl.ItemIndex := 0;
  end;

end; // RemoveTemplate


Initialization
    Template_Folder := ProperFolderName(ExtractFilePath( application.exename ) + _TEMPLATE_FOLDER );
    Template_LastWasFormatted := true;
    LastTemplateUsed := '';


end.
