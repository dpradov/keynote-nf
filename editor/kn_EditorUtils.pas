unit kn_EditorUtils;

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
   Winapi.Windows,
   Winapi.Messages,
   Winapi.RichEdit,
   Winapi.MMSystem,
   System.SysUtils,
   System.StrUtils,
   System.Classes,
   Vcl.Controls,
   Vcl.Forms,
   Vcl.Dialogs,
   Vcl.Clipbrd,
   Vcl.ComCtrls,
   Vcl.Graphics,
   ExtCtrls,

   RxRichEd,
   kn_KntFolder,
   kn_KntNote,
   knt.ui.editor
   ;


procedure PasteIntoNew (const AsNewFolder: boolean);
procedure PrintRTFNote;
procedure EnableOrDisableUAS;
procedure ConfigureUAS;


type
   TClipCapMng = class
   private
     _IS_CAPTURING_CLIPBOARD : boolean;
     _IS_CHAINING_CLIPBOARD : boolean;

     FLastURLPasted: string;
     FCacheURLs:   TStringList;
     FCacheTitles: TStringList;

   const
      CACHE_URLs_MAX: Integer = 20;
      CACHE_URLs_REDUCE_TO: Integer = 10;
      MAX_TIME_TO_GET_TITLE: Integer = 2000;   // milliseconds
      URL_YOUTUBE: string = 'https://www.youtube.com';

   protected
      FClipCapEditor : TKntRichEdit;
      FClipCapNote: TKntNote;
      FClipCapFolder: TKntFolder;

      procedure SetClipCapState( const IsOn : boolean );
      function GetClipCapActive: boolean;
      procedure SetClipCapEditor(value: TKntRichEdit);

      function GetTitleFromURL (const URL: String; TryForLimitedTime: boolean): String;

      procedure PasteOnClipCap (const ClpStr: string);

   public
      ClipCapNextInChain : HWnd;

      procedure CheckPasteOnClipCap;
      procedure PasteAsWebClip (const pPasteAsText: boolean);

      property ClipCapActive: boolean read GetClipCapActive;
      property ClipCapEditor : TKntRichEdit read FClipCapEditor write SetClipCapEditor;
      property ClipCapFolder : TKntFolder read FClipCapFolder;
      property ClipCapNote : TKntNote read FClipCapNote;

      constructor Create;
      destructor Destroy;
      procedure ToggleOn (const Editor : TKntRichEdit );
      procedure ToggleOff();
      procedure Toggle ( const TurnOn : boolean; const Editor : TKntRichEdit = nil);

      function IsBusy: boolean;
      procedure ShowState(Initial: boolean = false);

      procedure CleanCacheURLs (OnlyWithoutTitle: boolean);
   end;



implementation

uses
   UWebBrowserWrapper,
   UAS,
   RichPrint,
   TreeNT,

   gf_misc,
   gf_strings,

   Kn_Global,
   kn_Main,
   kn_const,
   kn_Cmd,
   kn_Info,
   kn_ClipUtils,
   kn_LinksMng,
   kn_TreeNoteMng,
   kn_VCLControlsMng,
   kn_MacroMng,
   knt.App
   ;



resourcestring
  STR_UAS_01 = 'UAS path';
  STR_UAS_02 = 'Please specify full path to uas.exe';
  STR_UAS_03 = 'KeyNote cannot find the location of uas.exe. UltimaShell Autocompletion Server will not be loaded.';
  STR_UAS_04 = ' UltimaShell Autocompletion Server loaded.';
  STR_UAS_05 = 'Cannot load UltimaShell Autocompletion Server. It may not be installed. Would you like to go to the UAS website and download the application?';
  STR_UAS_06 = ' UltimaShell Autocompletion Server unloaded.';
  STR_UAS_07 = ' UltimaShell Autocompletion Server is not loaded.';
  STR_ClipCap_01 = 'A Read-Only folder cannot be used for clipboard capture.';
  STR_ClipCap_02 = 'a new node';
  STR_ClipCap_03 = 'whichever node is currently selected';
  STR_ClipCap_04 = 'Each copied item will be pasted into %s in the tree. Continue?';
  STR_ClipCap_05 = ' Clipboard capture is now ';
  STR_ClipCap_06 = ' Capturing text from clipboard';
  STR_ClipCap_09 = ' Clipboard capture done';
  STR_Print_01 = 'Current folder contains more than one node. Do you want to print all nodes? Answer No to only print the selected node.';




//=================================================================
//                           TClipCapMng
//=================================================================


constructor TClipCapMng.Create;
begin
   inherited Create;

   FClipCapEditor:= nil;
   FClipCapNote:= nil;
   FClipCapFolder:= nil;
   ClipCapNextInChain := 0;
   _IS_CAPTURING_CLIPBOARD := false;
   _IS_CHAINING_CLIPBOARD := false;

   FCacheURLs:= nil;
   FCacheTitles:= nil;
   FLastURLPasted:= '';
end;

destructor TClipCapMng.Destroy;
begin
   if ClipCapNextInChain <> 0 then
      SetClipCapState(false);

   inherited Destroy;
end;

procedure TClipCapMng.Toggle ( const TurnOn : boolean; const Editor : TKntRichEdit = nil);
begin
   if TurnOn then
      ToggleOn(Editor)
   else
      ToggleOff;
end;

procedure TClipCapMng.ToggleOn (const Editor : TKntRichEdit );
var
  nodeMode : string;
  Folder: TKntFolder;
begin
  with Form_Main do begin
      if not assigned(Editor) then exit;

      ClipCapCRC32 := 0; // reset test value
      FLastURLPasted:= '';

      try
        try
            // turn ON clipboard capture for active folder

            Folder:= TKntFolder(Editor.FolderObj);

            if Editor.ReadOnly then begin
               TB_ClipCap.Down := false;
               PopupMessage( STR_ClipCap_01, mtInformation, [mbOK], 0 );
               exit;
            end;

            if not ( Initializing or ( not ClipOptions.TreeClipConfirm ) or not assigned(Folder) or Folder.TreeHidden ) then begin
              if ClipOptions.PasteAsNewNode then
                nodeMode := STR_ClipCap_02
              else
                nodeMode := STR_ClipCap_03;

              if MessageDlg( Format(STR_ClipCap_04, [nodeMode] ), mtConfirmation, [mbOK,mbCancel], 0 ) <> mrOK then begin
                TB_ClipCap.Down := false;
                exit;
              end;
            end;

            if ClipCapActive then begin     // some other folder was clipcap before
               ClipCapEditor:= nil;
               SetClipCapState( false );
            end;

            if assigned(Folder) then begin
               Pages_Res.MarkedPage:= nil;
               Pages.MarkedPage := Folder.TabSheet;
            end
            else begin
               Pages.MarkedPage:= nil;
               Pages_Res.MarkedPage:= ResTab_RTF;
            end;

            ClipCapEditor:= Editor;
            SetClipCapState( true );

        except
          on e : exception do begin
            messagedlg( E.Message, mtError, [mbOK], 0 );
            ToggleOff;
          end;
        end;

      finally
        ShowState (true);
      end;
  end;

end; // ToggleOn


procedure TClipCapMng.ToggleOff;
begin
  with Form_Main do begin
      ClipCapCRC32 := 0; // reset test value
      FLastURLPasted:= '';
      try
        try
            Pages.MarkedPage := nil;
            Pages_Res.MarkedPage:= nil;
            ClipCapEditor:= nil;
            SetClipCapState( false );
        except
          on e : exception do
            messagedlg( E.Message, mtError, [mbOK], 0 );
        end;

      finally
        ShowState (true);
      end;
  end;

end; // ToggleOff


procedure TClipCapMng.ShowState(Initial: boolean=false);
var
   CCActive: boolean;
begin
  with Form_Main do begin
     if Initial then
        CCActive:= ClipCapActive
     else
        CCActive:= ClipCapActive and ((ClipCapEditor = ActiveEditor) or (ClipCapEditor.FolderObj = nil));

     MMNoteClipCapture.Checked := CCActive;
     TMClipCap.Checked := CCActive;
     TB_ClipCap.Down := CCActive;

     if Initial then begin
        Pages.Invalidate; // force redraw to update "MarkedPage" tab color
        Pages_Res.Invalidate;
        App.ShowInfoInStatusBar(STR_ClipCap_05 + TOGGLEARRAY[ClipCapActive]);
     end;
  end;

end;


function TClipCapMng.IsBusy: boolean;
begin
   Result:= _IS_CAPTURING_CLIPBOARD or _IS_CHAINING_CLIPBOARD;
end;


procedure TClipCapMng.CheckPasteOnClipCap;
var
  ClpStr : string;

begin
  if (IsBusy) then exit;

  {*1 We need to calc CRC32 even if ClipOptions.TestDupClips=False, because it will be used for several other things,
     but here, depending on that configuration (TestDupClips="Ignore duplicate clips") we will consider or not to paste the text }

  {*2 We must register LogRTFHandleInClipboard, since the ecCut or ecCopy operations have been interrupted just after sending content to the
     clipboard, before we can register the handle that we are using to identify if the Clipboard contains content copied by the application itself }


  _IS_CAPTURING_CLIPBOARD:= True;
  try
   try
      if _IS_COPYING_TO_CLIPBOARD then        // *2
         LogRTFHandleInClipboard();

      // only when inactive (*A) or explicitly configured to capture from self... (*B)
      // but never capture text copied from the note that is being used for capture (*C)

      if ClipCapActive then begin
         if (GetActiveWindow <> Form_Main.Handle ) or
              (not ClipOptions.IgnoreSelf and
                (ClipCapEditor <> ActiveEditor) and
                  ((ActiveEditor.NoteObj=nil) or (ActiveFolder=nil) or (ClipCapNote <> ActiveFolder.SelectedNote))
              ) then begin

               ClpStr := Clipboard.TryAsText;
               if ((ClpStr <> '') or not ClipCapEditor.PlainText ) and (not TestCRCForDuplicates(ClpStr) or (not ClipOptions.TestDupClips)) then     // *1
                  PasteOnClipCap(ClpStr);
         end;
      end;

   except
     On E : Exception do
        App.WarnUnexpectedError(E);
        //Log_StoreTick( 'WMDrawClipboard- Exception: '+ E.Message, 1);
   end;

  finally
     _IS_CAPTURING_CLIPBOARD:= False;
  end;
end; // CheckPasteOnClipCap



procedure TClipCapMng.SetClipCapState( const IsOn : boolean );
begin
  with Form_Main do begin
      _IS_CHAINING_CLIPBOARD := true;
      try
        try
          case IsOn of
            true : begin
              ClipCapNextInChain := SetClipboardViewer( Handle );
              LoadTrayIcon( ClipOptions.SwitchIcon );
            end;
            false : begin
              ChangeClipboardChain(Handle, ClipCapNextInChain );
              ClipCapNextInChain := 0;
              LoadTrayIcon( false );
            end;
          end;
        except
          ClipCapNextInChain := 0;
          ClipCapEditor := nil;
          Pages.MarkedPage := nil;
          TB_ClipCap.Down := false;
          LoadTrayIcon( false );
        end;
      finally
        _IS_CHAINING_CLIPBOARD := false;
      end;
  end;

end; // SetClipCapState


function TClipCapMng.GetClipCapActive: boolean;
begin
  Result:= FClipCapEditor <> nil;
end;


procedure TClipCapMng.SetClipCapEditor(value: TKntRichEdit);
begin
    FClipCapEditor:= value;
    if assigned(value) then begin
       FClipCapNote:= TKntNote(FClipCapEditor.NoteObj);
       FClipCapFolder:= TKntFolder(FClipCapEditor.FolderObj);
    end
    else begin
       FClipCapNote:= nil;
       FClipCapFolder:= nil;
    end;

end;



procedure TClipCapMng.CleanCacheURLs (OnlyWithoutTitle: boolean);
var
   I: Integer;
begin
   if not assigned(FCacheURLs) then exit;

   if not OnlyWithoutTitle then begin
     FCacheURLs.Clear;
     FCacheTitles.Clear;
   end
   else begin
      for I := FCacheURLs.Count - 1 downto 0  do
          if FCacheTitles[i] = '' then begin
             FCacheURLs.Delete(i);
             FCacheTitles.Delete(i);
          end;
   end;
end;


function TClipCapMng.GetTitleFromURL (const URL: String; TryForLimitedTime: boolean): String;
var
  I: Integer;
  MaxTime: Integer;
begin
   if URL = '' then begin
      Result:= '';
      exit;
   end;

   if assigned(FCacheURLs) then begin
      I:= FCacheURLs.IndexOf(URL);
      if I >= 0 then begin
         Result:= FCacheTitles[i];
         exit;
      end;
   end
   else begin
      FCacheURLs:= TStringList.Create;
      FCacheTitles:= TStringList.Create;
   end;

   if not assigned(_IE) then
     _IE:= TWebBrowserWrapper.Create(Form_Main);

   MaxTime:= 0;
   if TryForLimitedTime then
      MaxTime := MAX_TIME_TO_GET_TITLE;
   Result:= _IE.GetTitleFromURL(URL, MaxTime);

   FCacheURLs.Add(URL);
   FCacheTitles.Add(Result);

   if FCacheURLs.Count >= CACHE_URLs_MAX then       // If we reach MAX
      for I := 1 to CACHE_URLs_REDUCE_TO do begin  // remove the older ones
         FCacheURLs.Delete(0);
         FCacheTitles.Delete(0);
      end;

end;


procedure TClipCapMng.PasteOnClipCap (const ClpStr: string);
var
  DividerString: string;
  i, j, len : integer;
  wavfn: string;
  myNodeName : string;
  myTreeNode, myParentNode : TTreeNTNode;
  PasteOK, PasteOnlyURL : boolean;
  SourceURLStr : string;
  TitleURL : string;
  AuxStr : string;
  HTMLClipboard: string;
  Folder: TKntFolder;
  Editor: TKntRichEdit;

  GetTitle: boolean;
  _S, _SL, _U: integer;
  TryForLimitedTime: boolean;
  CLIPSOURCE_Token: string;
  IgnoreCopiedText: boolean;

  Using2ndDivider: boolean;
  PasteAsNewNode: boolean;
  FolderName: string;


begin
  HTMLClipboard:= '';

  with Form_Main do begin
        myTreeNode := nil;
        SourceURLStr:= '';

        Editor:= ClipCapEditor;
        Folder:= ClipCapFolder;

        FolderName:= '';
        if assigned(Folder) then
           FolderName:= Folder.Name;

        // TrayIcon.Icon := TrayIcon.Icons[1];
        LoadTrayIcon( not ClipOptions.SwitchIcon ); // flash tray icon

        DividerString := ClipOptions.Divider;
        Using2ndDivider:= False;


        i:= pos(CLIPSECONDDIV, DividerString);
        if (i > 0) then begin
           HTMLClipboard:= Clipboard.AsHTML;
           SourceURLStr := Clipboard.GetURLFromHTML (HTMLClipboard);
           if (SourceURLStr <> '') and (SourceURLStr = FLastURLPasted) then begin
              delete(DividerString, 1, i + length(CLIPSECONDDIV)-1);
              Using2ndDivider:= true;
           end
           else begin
              delete(DividerString, i, 9999);
              FLastURLPasted:= SourceURLStr;
           end;
        end;


        { *1
          From now it seems not possible to obtain the title of a page of YouTube, because the html returned by TWebBrowser
          in this case does'nt contain the final html page
          To avoid loosing time and to avoid wasting we URLs cache, will not try to return title if URL begins with https://www.youtube.com
          Now it is easier to click three times in one word of the video title, to select it

          If the URL begins with 'https://www.youtube.com' then
          - Will not try to look for the title using WebBroser
          - If the text selected includes only 1 line and it hasn't more that 100 characters, then it wil be assumed as the title

          (https://www.smperth.com/resources/youtube/youtube-character-limit/)
          }

         IgnoreCopiedText:= false;       // *1


        _S  := pos(CLIPSOURCE, DividerString);
        _SL := pos(CLIPSOURCE_LIMITED, DividerString);
        _U  := pos(CLIPSOURCE_ONLY_URL, DividerString);

        if ClipOptions.InsertSourceURL then begin
           if HTMLClipboard ='' then begin
              HTMLClipboard:= Clipboard.AsHTML;
              SourceURLStr := Clipboard.GetURLFromHTML (HTMLClipboard);
              //TitleURL:= Clipboard.GetTitleFromHTML (HTMLClipboard);       // This data is not currently copied to the clipboard
           end;

           if (_S=0) and (_SL=0) and (_U > 0) then begin
               GetTitle:= False;
               CLIPSOURCE_Token:= CLIPSOURCE_ONLY_URL;
           end
           else
           if (_SL > 0) then begin
              GetTitle:= True;
              TryForLimitedTime:= True;
              CLIPSOURCE_Token:= CLIPSOURCE_LIMITED;
           end
           else begin
              GetTitle:= True;
              TryForLimitedTime:= false;
              CLIPSOURCE_Token:= CLIPSOURCE;
           end;

           if GetTitle then begin
              if pos(URL_YOUTUBE, SourceURLStr) = 1 then begin  // *1
                 TitleURL:= Clipboard.TryAsText;
                 i:= pos(#13, TitleURL);
                 j:= length(TitleURL);
                 if (j > 100) or ((i > 0) and (i <= j-2)) then   // If select caption clicking 3 times, #13#10 will be added..
                    TitleURL:= ''
                 else
                    IgnoreCopiedText:= True;
              end
              else
                 TitleURL:= GetTitleFromURL (SourceURLStr, TryForLimitedTime);
           end
           else
              TitleURL:= '';
           end
        else begin
            SourceURLStr := '';
            if _S > 0 then
               delete( DividerString, _S, length(CLIPSOURCE));
            if _SL > 0 then
               delete( DividerString, _SL, length(CLIPSOURCE_LIMITED));
            if _U > 0 then
               delete( DividerString, _U, length(CLIPSOURCE_ONLY_URL));
        end;

        PasteOK := true;

        try
          StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_06;
          PasteOnlyURL:= false;
          if ClipOptions.URLOnly and (SourceURLStr <> '') then begin
             AuxStr:= copy(ClpStr,1,30);
             if (GetWordCount(AuxStr)=1) and (not HasNonAlphaNumericOrWordDelimiter(AuxStr)) and (not Using2ndDivider) then begin
               PasteOnlyURL:= true;
               if IgnoreCopiedText then            // YouTube...  See *1
                  TitleURL:= '';
             end;
          end;

          PasteAsNewNode:= ClipOptions.PasteAsNewNode and assigned(Folder) and not Folder.TreeHidden;

          if not PasteOnlyURL then begin
              // ClipCapNode := nil;
              if PasteAsNewNode  then begin
                 if (pos(CLIPDATECHAR, DividerString)=0) and (pos(CLIPTIMECHAR, DividerString)=0) and ((SourceURLStr = '') or (pos(CLIPSOURCE_Token, DividerString)=0)) then
                    DividerString:= '';   // Si no hay que separar de nada y el propia cadena de separación no incluye fecha, ni hora ni se va a mostrar el origen, ignorarla

                 if ( ClipCapMng.ClipCapNote <> nil ) then
                    myParentNode := Folder.TV.Items.FindNode( [ffData], '', ClipCapMng.ClipCapNote )
                 else
                    myParentNode := nil;

                 case ClipOptions.ClipNodeNaming of
                   clnDefault : myNodeName := '';
                   clnClipboard : myNodeName:= Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH_CAPTURE);
                   clnDateTime :  myNodeName:= FormatDateTime(FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, now );
                 end;

                 if assigned( myParentNode ) then
                    myTreeNode := TreeNewNode( Folder, tnAddChild, myParentNode, myNodeName, true )
                 else
                    myTreeNode := TreeNewNode( Folder, tnAddLast, nil, myNodeName, true );

              end
          end;


          try
              Editor.BeginUpdate;

              if not PasteOnlyURL then begin

                for i := 1 to length( DividerString ) do begin
                   if ( DividerString[i] = CLIPDIVCHAR ) then
                      DividerString[i] := #13;
                end;

                i:= 1;
                repeat
                    i := posEx( CLIPSOURCEDELIMITER, DividerString, i );
                    if ( i > 0 ) then begin
                       len:= length(CLIPSOURCEDELIMITER);
                       if SourceURLStr = '' then begin
                          j := PosEx( CLIPSOURCEDELIMITER, DividerString, i + 2);
                          if j > 0 then
                             len:= len + j - i;
                       end;
                       delete( DividerString, i, len);
                    end;
                until i = 0;


                i := pos( CLIPDATECHAR, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPDATECHAR ));
                  if ( length( DividerString ) > 0 ) then
                    insert( FormatDateTime( KeyOptions.DateFmt, now ), DividerString, i )
                  else
                    DividerString := FormatDateTime( KeyOptions.DateFmt, now );
                end;

                i := pos( CLIPTIMECHAR, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPTIMECHAR ));
                  if ( length( DividerString ) > 0 ) then
                    insert( FormatDateTime( KeyOptions.TimeFmt, now ), DividerString, i )
                  else
                    DividerString := FormatDateTime( KeyOptions.TimeFmt, now );
                end;

                  // do not add leading blank lines if pasting in a new tree node
                if PasteAsNewNode then
                   DividerString := trimleft( DividerString );

                i := pos( CLIPSOURCE_DOMAIN, DividerString );
                if ( i > 0 ) then begin
                  delete( DividerString, i, length( CLIPSOURCE_DOMAIN ));
                  if not Editor.PlainText then begin
                     AuxStr:= DomainFromHttpURL(SourceURLStr, TitleURL);        // If TitleURL = '' will always return '', because URL will be shown
                     if ( length( DividerString ) > 0 ) then
                        insert(AuxStr , DividerString, i )
                     else
                        DividerString := AuxStr;
                  end;
                end;

                i := pos( CLIPSOURCE_Token, DividerString );
                if ( i > 0 ) then begin
                    delete( DividerString, i, length( CLIPSOURCE_Token ));
                    if ( SourceURLStr <> '' ) then begin
                       Editor.SelText := Copy(DividerString,1, i-1);
                       delete(DividerString, 1, i-1);   // Remaining divider will be pasted after source url
                    end;
                end
                else begin
                   Editor.SelText := DividerString;
                   DividerString:= '';
                end;
                Editor.SelStart :=  Editor.SelStart + Editor.SelLength;
              end;

              if ((SourceURLStr <> '' ) or PasteOnlyURL) and (not Using2ndDivider) then begin
                   InsertURL(SourceURLStr, TitleURL, Editor);
                   if PasteOnlyURL then begin
                     Editor.SelText := #13;
                     Editor.SelStart :=  Editor.SelStart + 1;
                   end
                   else
                   if (DividerString = '') then  // Si no se ha indicado dónde colocar el origen o se ha puesto justo al final del separador añadir un salto de línea
                       DividerString:= #13;
              end;

              if not PasteOnlyURL then begin
                 if (DividerString <> '') then begin
                     Editor.SelText := DividerString;
                     Editor.SelStart :=  Editor.SelStart + Editor.SelLength;
                 end;

                if not IgnoreCopiedText then
                   if not ClipOptions.PasteAsText and not Editor.PlainText then
                      Editor.TryPasteRTF(HTMLClipboard, FolderName)
                   else
                      Editor.PastePlain(ClpStr, HTMLClipboard, false, ClipOptions.MaxSize);

              end;

           finally
              Editor.EndUpdate;
              Editor.Change;
           end;

          if PasteAsNewNode then
             Folder.EditorToDataStream;


        finally
          if PasteOK then begin
            StatusBar.Panels[PANEL_HINT].Text := STR_ClipCap_09;
            wavfn := ExtractFilePath( application.exename ) + 'clip.wav';
            if ( ClipOptions.PlaySound and FileExists( wavfn )) then
               sndplaysound( PChar( wavfn ), SND_FILENAME or SND_ASYNC or SND_NOWAIT );

            Application.ProcessMessages;
            sleep( ClipOptions.SleepTime * 100 ); // in tenths of a second; default: 5 = half a second
            LoadTrayIcon( ClipOptions.SwitchIcon ); // unflash tray icon
          end;
        end;
  end;

end; // PasteOnClipCap



procedure TClipCapMng.PasteAsWebClip (const pPasteAsText: boolean);
var
  oldClipCapEditor : TKntRichEdit;
  oldDividerString : string;
  oldAsText, oldTreeClipConfirm, oldInsertSourceURL, oldClipPlaySound, oldPasteAsNewNode : boolean;
  oldMaxSize, oldSleepTime : integer;
begin
  if (IsBusy) then exit;

  if not App.CheckActiveEditorNotReadOnly then exit;


  with ClipOptions do begin

     oldClipPlaySound:= PlaySound;
     oldDividerString := Divider;
     oldInsertSourceURL := InsertSourceURL;
     oldMaxSize := MaxSize;
     oldSleepTime := SleepTime;
     oldTreeClipConfirm := TreeClipConfirm;
     oldAsText := PasteAsText;
     oldPasteAsNewNode:= PasteAsNewNode;

     oldClipCapEditor := ClipCapEditor;

     try
       PlaySound:= false;
       PasteAsNewNode:= false;
       MaxSize := 0;
       SleepTime := 0;
       TreeClipConfirm := false;
       InsertSourceURL := true;
       if WCDivider <> '' then                // Let use Divider also for Web Clip if WCDivider = ''
          Divider := WCDivider;
       PasteAsText := pPasteAsText;
       ClipCapEditor := ActiveEditor;

       PasteOnClipCap(Clipboard.TryAsText);

     finally
       PlaySound:= oldClipPlaySound;
       Divider := oldDividerString;
       InsertSourceURL := oldInsertSourceURL;
       MaxSize := oldMaxSize;
       SleepTime := oldSleepTime;
       TreeClipConfirm := oldTreeClipConfirm;
       PasteAsText := oldAsText;
       PasteAsNewNode:= oldPasteAsNewNode;
       ClipCapEditor := oldClipCapEditor;
     end;

  end;

end; // PasteAsWebClip




//=================================================================

procedure PasteIntoNew( const AsNewFolder : boolean );
var
  oldCNT : integer;
  CanPaste : boolean;
  myNodeName : string;
  myTreeNode : TTreeNTNode;
begin
  if ( not Form_Main.HaveKntFolders( true, false )) then exit;

  oldCNT := ActiveFile.Folders.Count;
  CanPaste := false;

  try
    if AsNewFolder then begin
      TKntFolder.NewKntFolder( true, true );
      CanPaste := ( OldCNT < ActiveFile.Folders.Count );
    end
    else begin
      if assigned(ActiveFolder) then begin
        case ClipOptions.ClipNodeNaming of
           clnDefault :   myNodeName := '';
           clnClipboard : myNodeName:= Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH_CAPTURE);
           clnDateTime :  myNodeName:= FormatDateTime(FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, now );
        end;

        myTreeNode := TreeNewNode(ActiveFolder, tnAddAfter, GetCurrentTreeNode, myNodeName, false );
        CanPaste := assigned( myTreeNode );
      end;
    end;

  except
    exit;
  end;

  if CanPaste then begin
     if ShiftDown then
        PerformCmd( ecPastePlain )
     else
        PerformCmd( ecPaste );
  end;

end; // PasteIntoNew



//=================================================================

procedure PrintRTFNote;
var
  PrintRE : TRichEdit;
  MS : TMemoryStream;
  PrintAllNodes : boolean;
  myTreeNode : TTreeNTNode;
  myNote : TKntNote;
begin
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;
  if ( not assigned( Form_Main.RichPrinter )) then    // [dpv]
  begin
      try                                     // [DPV]
         Form_Main.RichPrinter := TRichPrinter.Create(Form_Main);
      except
        On E : Exception do
        begin
          showmessage( E.Message );
          exit;
        end;
      end;
  end;
  PrintAllNodes := false;

  if (ActiveFolder.TV.Items.Count > 1 ) then
  case messagedlg(STR_Print_01,
      mtConfirmation, [mbYes,mbNo,mbCancel], 0 ) of
    mrYes : PrintAllNodes := true;
    mrNo : PrintAllNodes := false;
    else
      exit;
  end;

  if Form_Main.PrintDlg.Execute then begin
    Form_Main.RichPrinter.Title := RemoveAccelChar( ActiveFolder.Name );

    PrintRe := TRichEdit.Create( nil );
    MS := TMemoryStream.Create;

    try

      screen.Cursor := crHourGlass;

      with PrintRe do begin
        parent := Form_Main;
        visible := false;
        WordWrap := false;
      end;

      if (not PrintAllNodes ) then begin

        if KeyOptions.SafePrint then begin
          ActiveFolder.Editor.Print( RemoveAccelChar( ActiveFolder.Name ));
          (*
          ActiveFolder.Editor.Lines.SaveToStream( MS );
          MS.Position := 0;
          PrintRE.Lines.LoadFromStream( MS );
          if ( ActiveFolder.Editor.SelLength > 0 ) then
          begin
            PrintRE.SelStart := ActiveFolder.Editor.SelStart;
            PrintRE.SelLength := ActiveFolder.Editor.SelLength;
          end;
          RichPrinter.PrintRichEdit( TCustomRichEdit( PrintRE ), 1 );
          *)
        end
        else
          Form_Main.RichPrinter.PrintRichEdit( TCustomRichEdit( ActiveFolder.Editor ), 1 );
      end
      else begin
        myTreeNode := ActiveFolder.TV.Items.GetFirstNode;
        if myTreeNode.Hidden then myTreeNode := myTreeNode.GetNextNotHidden;   // [dpv]
        while assigned( myTreeNode ) do begin
          myNote := TKntNote( myTreeNode.Data );
          if assigned( myNote ) then begin
            myNote.Stream.Position := 0;
            PrintRE.Lines.LoadFromStream( myNote.Stream );
            if KeyOptions.SafePrint then
              PrintRE.Print( RemoveAccelChar( ActiveFolder.Name ))
            else
              Form_Main.RichPrinter.PrintRichEdit( TCustomRichEdit( PrintRE ), 1 );
          end;
          //myTreeNode := myTreeNode.GetNext;          // [dpv]
          myTreeNode := myTreeNode.GetNextNotHidden;   // [dpv]
        end;
      end;

    finally
      screen.Cursor := crDefault;
      if assigned( PrintRE ) then PrintRE.Free;
      if assigned( MS ) then MS.Free;
    end;

  end;
end; // PrintRTFNote



//=================================================================
//    UAS
//=================================================================

procedure EnableOrDisableUAS;
var
  UASPath : string;
begin
  try
    if KeyOptions.UASEnable then begin
      UASPath := GetUASPath; // let UAS find itself

      if ( not fileexists( UASPath )) then begin
        UASPath := KeyOptions.UASPath; // maybe we already have it configured

        if ( not fileexists( UASPath )) then begin
          // ...we don't so ask user and check answer
          if ( InputQuery( STR_UAS_01, STR_UAS_02, UASPath ) and
               fileexists( UASPath )) then
            KeyOptions.UASPath := UASPath // found it, so store it for later
          else begin
            // user canceled or entered invalid path, so bail out
            messagedlg( STR_UAS_03, mtError, [mbOK], 0 );
            KeyOptions.UASEnable := false;
            exit;
          end;
        end;
      end;

      if LoadUAS( UASPath ) then begin
        UAS_Window_Handle := GetUASWnd;
        // check if really loaded
        KeyOptions.UASEnable := ( UAS_Window_Handle <> 0 );
      end
      else
        KeyOptions.UASEnable := false;

      if KeyOptions.UASEnable then
        // success
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_04
      else begin
        // something went wrong
        KeyOptions.UASEnable := false;
        if ( messagedlg( STR_UAS_05, mtWarning, [mbOK,mbCancel], 0 ) = mrOK ) then
          GoDownloadUAS;
      end;
    end
    else begin
      if ( UAS_Window_Handle <> 0 ) then begin
        SendMessage(GetUASWnd,WM_CLOSE,0,0);
        Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_06;
      end;
    end;

  finally
    if ( not KeyOptions.UASEnable ) then
      UAS_Window_Handle := 0;
    Form_Main.MMToolsUAS.Checked := KeyOptions.UASEnable;
    Form_Main.MMToolsUASConfig.Enabled := KeyOptions.UASEnable;
    Form_Main.MMToolsUASConfig.Visible := KeyOptions.UASEnable;
  end;

end; // EnableOrDisableUAS


procedure ConfigureUAS;
var
  ptCursor : TPoint;
begin
  if ( UAS_Window_Handle = 0 ) then begin
    Form_Main.StatusBar.Panels[PANEL_HINT].Text := STR_UAS_07;
    exit;
  end;

  GetCursorPos( ptCursor );
  SetForegroundWindow( UAS_Window_Handle );
  PostMessage( UAS_Window_Handle, WM_APP+$2001, ptCursor.x, ptCursor.y );

end; // ConfigureUAS




initialization

end.
