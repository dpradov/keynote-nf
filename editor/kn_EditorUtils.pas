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
   VirtualTrees,
   kn_info,
   kn_KntFolder,
   knt.model.note,
   knt.ui.editor
   ;


procedure PasteIntoNew (const AsNewFolder: boolean);
function GetPageDimensionesInRTF: AnsiString;
function GetHeaderInRTF(const Header: AnsiString): AnsiString;
function GetFooterInRTF: AnsiString;
function GetPrintAreaInPixels (ScreenDPI: Double = -1; IgnorePrinterOffset: Boolean = False): TRect;
function GetRealMonitorDPI: TPoint;
function GetRealMonitorDPIx: Double;
procedure GetPrintDimensionsInPixels (var mLeft, mTop, mRight, mBottom: Integer;
                                     var PageWidth, PageHeight: Integer;
                                     var OffsetX, OffsetY: Integer;
                                     var DPIx, DPIy: Double;
                                     ScreenDPI: Double = -1);
procedure PrintRtfFolder (PrintPreview: boolean = false);
procedure ShowPrintPreview (PrnPreviews: TList; DPI: Integer = 96);
procedure EnableOrDisableUAS;
procedure ConfigureUAS;
procedure ConvertStreamContent(Stream: TMemoryStream; FromFormat, ToFormat: TRichStreamFormat; RTFAux : TRxRichEdit; KntFolder: TKntFolder);
procedure UpdateEditor (AEditor: TRxRichEdit; KntFolder: TKntFolder; SetWordWrap: boolean; KeepNotVisible: boolean = false);
function GetColor(Color: TColor; ColorIfNone: TColor): TColor; inline;

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
      FClipCapNNode: TNoteNode;
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
      property ClipCapNNode : TNoteNode read FClipCapNNode;

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
   Vcl.Printers,
   UWebBrowserWrapper,
   UAS,
   SynGdiPlus,

   gf_misc,
   gf_miscvcl,
   gf_strings,

   Kn_Global,
   kn_Main,
   kn_const,
   kn_Cmd,
   kn_ClipUtils,
   kn_ImagesMng,
   kn_ImageForm,
   kn_LinksMng,
   knt.ui.tree,
   kn_VCLControlsMng,
   kn_MacroMng,
   kn_RTFUtils,
   knt.App,
   knt.RS
   ;



//=================================================================
//                           TClipCapMng
//=================================================================


constructor TClipCapMng.Create;
begin
   inherited Create;

   FClipCapEditor:= nil;
   FClipCapNNode:= nil;
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
      Folder:= TKntFolder(Editor.FolderObj);
      if (Folder = nil) and Editor.DoRegisterNewImages then exit;


      ClipCapCRC32 := 0; // reset test value
      FLastURLPasted:= '';

      try
        try
            // turn ON clipboard capture for active folder

            if Editor.ReadOnly then begin
               TB_ClipCap.Down := false;
               App.InfoPopup(GetRS(sEdt42));
               exit;
            end;

            if not ( Initializing or ( not ClipOptions.TreeClipConfirm ) or not assigned(Folder) or Folder.TreeHidden ) then begin
              if ClipOptions.PasteAsNewNode then
                nodeMode := GetRS(sEdt43)
              else
                nodeMode := GetRS(sEdt44);

              if App.DoMessageBox(Format(GetRS(sEdt45), [nodeMode] ), mtConfirmation, [mbOK,mbCancel], Def2 ) <> mrOK then begin
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
            App.ErrorPopup(E.Message);
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
			App.ErrorPopup(E.Message);
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
        App.ShowInfoInStatusBar(GetRS(sEdt46) + TOGGLEARRAY[ClipCapActive]);
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
                  ((ActiveEditor.NNodeObj=nil) or (ActiveFolder=nil) or (ClipCapNNode <> ActiveFolder.FocusedNNode))
              ) then begin

               ClpStr := Clipboard.TryAsText;
               if ((ClpStr <> '') or not ClipCapEditor.PlainText ) and (not TestCRCForDuplicates(ClpStr) or (not ClipOptions.TestDupClips)) then     // *1
                  PasteOnClipCap(ClpStr);
         end;
      end;

   except
     On E : Exception do
        App.ErrorPopup(E);
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
       FClipCapNNode:= TNoteNode(FClipCapEditor.NNodeObj);
       FClipCapFolder:= TKntFolder(FClipCapEditor.FolderObj);
    end
    else begin
       FClipCapNNode:= nil;
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
  myTreeNode, myParentNode : PVirtualNode;
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
           if KeyOptions.URLWebDecode then
              SourceURLStr:= DecodeURLWebUTF8Characters(SourceURLStr);

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
          StatusBar.Panels[PANEL_HINT].Text := GetRS(sEdt47);
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

                 if ( ClipCapMng.ClipCapNNode <> nil ) then
                    myParentNode:= ClipCapMng.ClipCapNNode.TVNode
                 else
                    myParentNode := nil;

                 case ClipOptions.ClipNodeNaming of
                   clnDefault : myNodeName := '';
                   clnClipboard : myNodeName:= Clipboard.TryGetFirstLine(TREENODE_NAME_LENGTH_CAPTURE);
                   clnDateTime :  myNodeName:= FormatDateTime(FormatSettings.ShortDateFormat + #32 + FormatSettings.ShortTimeFormat, now );
                 end;

                 if assigned( myParentNode ) then
                    Folder.TreeUI.NewNode(tnAddChild, myParentNode, myNodeName, true )
                 else
                    Folder.TreeUI.NewNode(tnAddLast, nil, myNodeName, true );

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
                      Editor.PasteBestAvailableFormat(FolderName, true, true)
                   else
                      Editor.PastePlain(ClpStr, HTMLClipboard, false, ClipOptions.MaxSize);

              end;

           finally
              Editor.EndUpdate;
              Editor.Change;
           end;

          if PasteAsNewNode then
             Folder.SaveEditorToDataModel;


        finally
          if PasteOK then begin
            StatusBar.Panels[PANEL_HINT].Text := GetRS(sEdt48);
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
  oldAsText, oldTreeClipConfirm, oldInsertSourceURL, oldClipPlaySound, oldPasteAsNewNode, oldSwitchIcon : boolean;
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
     oldSwitchIcon:= SwitchIcon;

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
       SwitchIcon:= false;
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
       SwitchIcon:= oldSwitchIcon;
       ClipCapEditor := oldClipCapEditor;
       if ClipCapActive then
          LoadTrayIcon(SwitchIcon); // restaure tray icon
     end;

  end;

end; // PasteAsWebClip




//=================================================================

procedure PasteIntoNew( const AsNewFolder : boolean );
var
  oldCNT : integer;
  CanPaste : boolean;
  myNodeName : string;
  NNode : TNoteNode;
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

        NNode := ActiveFolder.TreeUI.NewNode(tnAddBelow, GetCurrentTreeNode, myNodeName, false );
        CanPaste := assigned(NNode);
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

function GetRealMonitorDPI: TPoint;
var
  ScreenDC: HDC;
  HorzSizeMM, VertSizeMM: Integer;
  HorzResPixels, VertResPixels: Integer;
  DPI_X, DPI_Y: Double;
begin
  ScreenDC := GetDC(0);
  try
    HorzResPixels := GetDeviceCaps(ScreenDC, HORZRES);
    VertResPixels := GetDeviceCaps(ScreenDC, VERTRES);
    HorzSizeMM := GetDeviceCaps(ScreenDC, HORZSIZE);          // millimeters
    VertSizeMM := GetDeviceCaps(ScreenDC, VERTSIZE);

    if (HorzSizeMM > 0) and (VertSizeMM > 0) then begin
      DPI_X := HorzResPixels / (HorzSizeMM / 25.4);           // millimeters -> inches
      DPI_Y := VertResPixels / (VertSizeMM / 25.4);
    end
    else begin
      DPI_X := 96;                      // If physical size is not available, we assume 96 DPI as fallback
      DPI_Y := 96;
    end;
    Result := TPoint.Create(Round(DPI_X), Round(DPI_Y));

  finally
    ReleaseDC(0, ScreenDC);
  end;
end;


function GetRealMonitorDPIx: Double;
var
  ScreenDC: HDC;
  HorzSizeMM, HorzResPixels: Integer;
  DPI_X: Double;
begin
  ScreenDC := GetDC(0);
  try
    HorzResPixels := GetDeviceCaps(ScreenDC, HORZRES);
    HorzSizeMM := GetDeviceCaps(ScreenDC, HORZSIZE);          // millimeters

    if (HorzSizeMM > 0) then
      DPI_X := HorzResPixels / (HorzSizeMM / 25.4)           // millimeters -> inches
    else
      DPI_X := 96;                      // If physical size is not available, we assume 96 DPI as fallback

    Result := DPI_X;

  finally
    ReleaseDC(0, ScreenDC);
  end;
end;


procedure GetPrintDimensionsInPixels (var mLeft, mTop, mRight, mBottom: Integer;
                                      var PageWidth, PageHeight: Integer;
                                      var OffsetX, OffsetY: Integer;
                                      var DPIx, DPIy: Double;
                                      ScreenDPI: Double = -1);
   function IsMetricSystem: Boolean;
   var
     Measure: array[0..1] of Char;
   begin
     GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, Measure, SizeOf(Measure));
     Result := StrToIntDef(Measure, 0) = 0;           // 0 = metric, 1 = imperial
   end;

begin
    {
    https://learn.microsoft.com/en-us/windows/win32/api/richedit/ns-richedit-formatrange
    https://learn.microsoft.com/en-us/windows/win32/api/wingdi/nf-wingdi-getdevicecaps


    LOGPIXELSX:
  	Number of pixels per logical inch along the screen width. In a system with multiple display monitors, this value
    is the same for all monitors.

    LOGPIXELSY:
    Number of pixels per logical inch along the screen height.

    HORZRES:
    Width, in pixels, of the screen; or for printers, the width, in pixels, of the printable area of the page.

    VERTRES:
    Height, in raster lines, of the screen; or for printers, the height, in pixels, of the printable area of the page.

    PHYSICALWIDTH:
	  For printing devices: the width of the physical page, in device units. For example, a printer set to print
    at 600 dpi on 8.5-x11-inch paper has a physical width value of 5100 device units. Note that the physical page
    is almost always greater than the printable area of the page, and never smaller.

    PHYSICALHEIGHT:
	  For printing devices: the height of the physical page, in device units. For example, a printer set to print
    at 600 dpi on 8.5-by-11-inch paper has a physical height value of 6600 device units.

    PHYSICALOFFSETX:
	  For printing devices: the distance from the left edge of the physical page to the left edge of the printable area,
    in device units. For example, a printer set to print at 600 dpi on 8.5-by-11-inch paper, that cannot print on the
    leftmost 0.25-inch of paper, has a horizontal physical offset of 150 device units.

    PHYSICALOFFSETY:
	  For printing devices: the distance from the top edge of the physical page to the top edge of the printable area,
    in device units. For example, a printer set to print at 600 dpi on 8.5-by-11-inch paper, that cannot print on the
    topmost 0.5-inch of paper, has a vertical physical offset of 300 device units.
    }

   // Printer resolution in pixels per inch (PPI)
    DPIx := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    DPIy := GetDeviceCaps(Printer.Handle, LOGPIXELSY);

    PageWidth  := Printer.PageWidth;          // = PHYSICALWIDTH
    PageHeight := Printer.PageHeight;         // = PHYSICALHEIGHT

    OffsetX := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    OffsetY := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);

    if ScreenDPI > 0 then begin
       PageWidth  := Round(PageWidth  * ScreenDPI/DPIx);
       PageHeight := Round(PageHeight * ScreenDPI/DPIy);
       OffsetX :=    Round(OffsetX * ScreenDPI/DPIx);
       OffsetY :=    Round(OffsetY * ScreenDPI/DPIx);
       DPIx:= ScreenDPI;
       DPIy:= ScreenDPI;
    end;


    // PageSetupDlg.Margins:
    // If the user is entering values in inches, MarginBottom expresses the margin in thousandths of an inch.
    // If the user is entering values in millimeters, MarginBottom expresses the margin in hundredths of a millimeter.

    // Converting margins from millimeters (or inches) to pixels
   if Form_Main.PageSetupDlg = nil then
      Form_Main.PageSetupDlg := TPageSetupDialog.Create(Form_Main);

    with Form_Main.PageSetupDlg do begin
       if (Units = pmMillimeters ) or ((Units = pmDefault) and IsMetricSystem) then begin
          mLeft   := Round(MarginLeft  /100 / 25.4 * DPIx);
          mTop    := Round(MarginTop   /100 / 25.4 * DPIy);
          mRight  := Round(MarginRight /100 / 25.4 * DPIx);
          mBottom := Round(MarginBottom/100 / 25.4 * DPIy);
       end
       else begin
          mLeft   := Round(MarginLeft  /1000 * DPIx);
          mTop    := Round(MarginTop   /1000 * DPIy);
          mRight  := Round(MarginRight /1000 * DPIx);
          mBottom := Round(MarginBottom/1000 * DPIy);
       end;
    end;

end;


function GetPrintAreaInPixels (ScreenDPI: Double = -1; IgnorePrinterOffset: Boolean = False): TRect;
var
  mLeft, mTop, mRight, mBottom: Integer;
  DPIx, DPIy: Double;
  PageWidth, PageHeight: Integer;
  OffsetX, OffsetY: Integer;

begin
    GetPrintDimensionsInPixels(mLeft, mTop, mRight, mBottom, PageWidth, PageHeight, OffsetX, OffsetY, DPIx, DPIy, ScreenDPI);

    if not IgnorePrinterOffset then begin
       if mLeft < OffsetX then
          mLeft:= OffsetX;
       if mTop < OffsetY then
          mTop:= OffsetY;
    end;

    // Defining the printable area using margins
    Result.Left   := mLeft;
    Result.Top    := mTop;
    Result.Right  := PageWidth - mRight;
    Result.Bottom := PageHeight - mBottom;
end;


function GetPageDimensionesInRTF: AnsiString;
var
  mLeft, mTop, mRight, mBottom: Integer;
  PageWidth, PageHeight: Integer;
  OffsetX, OffsetY: Integer;
  DPIx, DPIy: Double;
  DPI: integer;
const
  FontHeight = 19-5;
  // I'm going to assume, as a rough approximation to try to center (albeit partially) the header at the top margin, that the font
  // used for this is Tahoma 12, which is most likely => 19 pixels --> Approx. -> 14
  // When previewing and printing I will actually use Tahoma 12

begin
   DPI:= Screen.PixelsPerInch;
   GetPrintDimensionsInPixels(mLeft, mTop, mRight, mBottom, PageWidth, PageHeight, OffsetX, OffsetY, DPIx, DPIy, DPI);
   Result:= Format('\paperw%d\paperh%d\margl%d\margt%d\margr%d\margb%d \footery%d\headery%d ',
     [PixelsToTwips(PageWidth, DPI),
      PixelsToTwips(PageHeight, DPI),
      PixelsToTwips(mLeft, DPI),
      PixelsToTwips(mTop, DPI),
      PixelsToTwips(mRight, DPI),
      PixelsToTwips(mBottom, DPI),
      PixelsToTwips(mBottom - FontHeight, DPI) div 2,
      PixelsToTwips(mTop - FontHeight, DPI) div 2]);

{
   s: string;
   h,w: integer;
   Sz: TSize;

   Form_Main.Canvas.Font.Name:= 'Tahoma';
   Form_Main.Canvas.Font.Size:= 12;
   s:= 'Header text...';
   w:= Form_Main.Canvas.TextWidth(s);
   h:= Form_Main.Canvas.TextHeight(s);
   Sz:= Form_Main.Canvas.TextExtent(s);
}

end;


function GetFooterInRTF: AnsiString;
begin
   //Result:= '{\footer \pard\qc\sa0 ' + TextToUseInRTF('Page') + ' {\field{\*\fldinst PAGE}}\par}';
   Result:= '{\footer \pard\qc {\field{\*\fldinst PAGE}}\par}';
end;


function GetHeaderInRTF(const Header: AnsiString): AnsiString;
begin
   Result:= '{\header\pard\qr ' + Header + '\par}';
end;


procedure PrintRtfFolder(PrintPreview: boolean = false);
var
  operation: string;
  RTFAux: TAuxRichEdit;
  PrintAllNodes : boolean;
  Node : PVirtualNode;
  NNode: TNoteNode;
  NEntry: TNoteEntry;
  TV: TVTree;
  TreeUI: TKntTreeUI;
  PrintMode: string;
  Caption: string;
  NEntryTextSize: integer;
  NEntryText, RTFwithImages: AnsiString;
  FirstPrint, NodeInNewPage: boolean;
  FirstPageMarginTop, LastPagePrintedHeight: integer;
  PageRect: TRect;
  DPI: Integer;
  PrnPreviews: TList;

begin
  if ( not Form_Main.HaveKntFolders( true, true )) then exit;
  if ( not assigned( ActiveFolder )) then exit;

  PrintAllNodes := false;

  TreeUI:= ActiveFolder.TreeUI;
  TV:= TreeUI.TV;

  if PrintPreview then
     Operation:= GetRS(sMain97)
  else
     Operation:= GetRS(sMain29);

  if (TV.TotalCount > 1 ) then begin
    PrintMode:= '0';
    if InputQuery( Operation, GetRS(sEdt49), PrintMode ) then
       if PrintMode = '0' then
          PrintAllNodes := false
       else begin
          PrintAllNodes := true;
          if PrintMode = '1' then
             NodeInNewPage := false
          else
          if PrintMode = '2' then
             NodeInNewPage := true
          else
             exit;
       end
    else
       exit;

  end;


  if Form_Main.PrintDlg.Execute then begin

    Caption:= RemoveAccelChar( ActiveFolder.Name );
    RTFAux:= nil;

    try
      App.ShowInfoInStatusBar(Operation);
      screen.Cursor := crHourGlass;

      if PrintPreview then begin
         DPI:= Screen.PixelsPerInch;
         PageRect:= GetPrintAreaInPixels(DPI);
      end
      else
         PageRect:= GetPrintAreaInPixels;

      if (not PrintAllNodes ) then begin
          ActiveEditor.PageRect:= PageRect;
          PrnPreviews:= ActiveEditor.PrnPreviews;
          if PrintPreview then begin
             ActiveEditor.PrnPreviews.Clear;
             ActiveEditor.CreatePrnPrew(ActiveFolder.Name, true, -1, DPI);
          end
          else
             ActiveFolder.Editor.Print(Caption, true, true);
      end
      else begin
        RTFAux:= CreateAuxRichEdit();
        RTFAux.PrepareEditorforPlainText(ActiveFolder.EditorChrome);
        RTFAux.PageRect:= PageRect;
        PrnPreviews:= RTFAux.PrnPreviews;
        FirstPrint:= True;

        if not PrintPreview then
           Printer.BeginDoc;

        Node := TV.GetFirst;
        if not TV.IsVisible[Node] then
           Node := TV.GetNextNotHidden(Node);

        while assigned( Node ) do begin
           RTFAux.Clear;
           NNode:= TreeUI.GetNNode(Node);
           NEntry:= NNode.Note.Entries[0];            // %%%
           NEntry.Stream.Position := 0;
           RTFwithImages:= '';
           NEntryText:= '';
           NEntryTextSize := NEntry.Stream.Size;
           if NEntry.Stream.Size > 0 then begin
               SetLength( NEntryText, NEntryTextSize );
               move( NEntry.Stream.Memory^, NEntryText[1], NEntryTextSize );
           end;

           if NEntry.IsRTF then
              RTFwithImages:= ImageMng.ProcessImagesInRTF(NEntryText, '', imImage, '', 0, false);

           if RTFwithImages <> '' then
              RTFAux.PutRtfText(RTFwithImages, false)
           else
              RTFAux.PutRtfText(NEntryText, false);

           if RTFAux.TextLength <> 0 then begin
              FirstPageMarginTop:= -1;
              if not FirstPrint then begin
                 if NodeInNewPage then begin
                    if not PrintPreview then
                       Printer.NewPage;
                 end
                 else
                    FirstPageMarginTop:= LastPagePrintedHeight;     // Adjust vertical position for next RichEdit
              end;
              if PrintPreview then
                 LastPagePrintedHeight:= RTFAux.CreatePrnPrew(Caption, true, FirstPageMarginTop, DPI)
              else
                 LastPagePrintedHeight:= RTFAux.Print(Caption, true, FirstPrint, false, FirstPageMarginTop);

              FirstPrint:= False;
           end;

           Node := TV.GetNextNotHidden(Node);
        end;
        if not PrintPreview and Printer.Printing then
           Printer.EndDoc;
      end;

      App.ShowInfoInStatusBar(GetRS(sMain30));
      screen.Cursor := crDefault;

      if PrintPreview then
         ShowPrintPreview(PrnPreviews);

    finally
      screen.Cursor := crDefault;
      if Printer.Printing then
         Printer.EndDoc;
      if assigned( RTFAux ) then RTFAux.Free;
    end;


  end;
end; // PrintRtfFolder


procedure ShowPrintPreview (PrnPreviews: TList; DPI: Integer = 96);
var
  Img: TKntImage;
  PrnPreview:  TMetafile;
  PageWidth, i: integer;
  Stream: TMemoryStream;
  Images: TList;
  Form_Image : TForm_Image;

begin
  if PrnPreviews.Count = 0 then exit;

  Images:= TList.Create;
  for i:= 0 to PrnPreviews.Count-1 do begin
     PrnPreview:= PrnPreviews[i];
     Stream:= TMemoryStream.Create;
     PrnPreview.SaveToStream(Stream);
     Img:= TKntImage.Create(0, '', true, imgWMF, PrnPreview.Width, PrnPreview.Height, 0, 0, Stream);
     Img.Caption:= String.Format(GetRS(sMain98), [i+1, PrnPreviews.Count]);
     Img.ForceName(String.Format(GetRS(sMain99), [i+1, IMAGE_FORMATS[imgWMF]]));
     Images.Add(Img);
     PrnPreview.Free;
  end;
  PrnPreviews.Clear;

  Form_Image := TForm_Image.Create( Form_Main );
  Form_Image.CorrectionRatio:= GetRealMonitorDPIx / DPI;
  Form_Image.Images:= Images;
  Form_Image.Show;
end;


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
          if ( InputQuery( GetRS(sEdt35), GetRS(sEdt36), UASPath ) and
               fileexists( UASPath )) then
            KeyOptions.UASPath := UASPath // found it, so store it for later
          else begin
            // user canceled or entered invalid path, so bail out
            App.ErrorPopup( GetRS(sEdt37) );
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
        App.ShowInfoInStatusBar(GetRS(sEdt38))
      else begin
        // something went wrong
        KeyOptions.UASEnable := false;
        if ( App.DoMessageBox( GetRS(sEdt39), mtWarning, [mbOK,mbCancel], Def2 ) = mrOK ) then
          GoDownloadUAS;
      end;
    end
    else begin
      if ( UAS_Window_Handle <> 0 ) then begin
        SendMessage(GetUASWnd,WM_CLOSE,0,0);
        App.ShowInfoInStatusBar(GetRS(sEdt40));
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
    App.ShowInfoInStatusBar(GetRS(sEdt41));
    exit;
  end;

  GetCursorPos( ptCursor );
  SetForegroundWindow( UAS_Window_Handle );
  PostMessage( UAS_Window_Handle, WM_APP+$2001, ptCursor.x, ptCursor.y );

end; // ConfigureUAS


//=======================================================

procedure ConvertStreamContent(Stream: TMemoryStream; FromFormat, ToFormat: TRichStreamFormat; RTFAux : TRxRichEdit; KntFolder: TKntFolder);
var
  Encoding: TEncoding;
begin
   Encoding:= nil;
   RTFAux.Clear;
   RTFAux.StreamMode := [];

   UpdateEditor(RTFAux, KntFolder, True, True);

   RTFAux.StreamFormat:= FromFormat;
   RTFAux.Lines.LoadFromStream(Stream);
   RTFAux.StreamFormat := ToFormat;

   if (ToFormat = sfPlainText) and (not CanSaveAsANSI(RTFAux.Text)) then
      Encoding:= TEncoding.UTF8;

   Stream.Clear;
   RTFAux.Lines.SaveToStream(Stream, Encoding);
end;


procedure UpdateEditor (AEditor: TRxRichEdit; KntFolder: TKntFolder; SetWordWrap: boolean; KeepNotVisible: boolean = false);
var
//  tabstopcnt : integer;
  TextLen: integer;
  SS, SL: integer;
  FocNNode: TNoteNode;

begin
  if not assigned(AEditor) then exit;

  // Note: Currently WordWrap is set in Editor in CreateVCLControlsForFolder and EditKntFolderProperties (calling here with SetWordWrap=true)
  //       and in TreeNodeSelected

  AEditor.BeginUpdate;
  try
    with KntFolder do begin
       if SetWordWrap then                         // Setting AEditor.WordWrap => calling CMRecreateWnd
          AEditor.WordWrap := WordWrap;

       AEditor.AutoURLDetect := URLDetect;

       AEditor.Color := EditorChrome.BGColor;
       TextLen:= AEditor.TextLength;
       if (TextLen = 0) or AEditor.PlainText then begin          // Solves the problem indicated in EditProperties...*1
          with AEditor.DefAttributes do begin
            Charset := EditorChrome.Font.Charset;
            Name := EditorChrome.Font.Name;
            Size := EditorChrome.Font.Size;
            Style := EditorChrome.Font.Style;
            Color := EditorChrome.Font.Color;
            Language := EditorChrome.Language;
          end;

          if (TextLen = 0) and RTL then
             AEditor.BiDiMode:= bdRightToLeft;
       end;

       if AEditor.PlainText and (TextLen > 0) then begin       // Related to *1. If PlainText then we do want it to always change the font format
          SS:= AEditor.SelStart;
          SL:= AEditor.SelLength;
          AEditor.SelectAll;
          AEditor.SelAttributes.Assign( AEditor.DefAttributes );
         {
         AEditor.Paragraph.TabCount := 8; // max is 32, but what the hell
         for tabstopcnt := 0 to 7 do
           AEditor.Paragraph.Tab[tabstopcnt] := (tabstopcnt+1) * (2*FTabSize); // [x] very rough!
         }
         AEditor.SetSelection(SS, SS+SL, True);
       end;

       if AEditor is TKntRichEdit then begin
          TKntRichEdit(AEditor).TabSize := TabSize;
          TKntRichEdit(AEditor).UseTabChar := UseTabChar;
       end;

       FocNNode:= FocusedNNode;

       if assigned(FocNNode) then begin
          if SetWordWrap then
             case FocNNode.WordWrap of
              wwYes: AEditor.WordWrap := true;
              wwNo:  AEditor.WordWrap := false;
              else   AEditor.WordWrap := WordWrap;     // As Folder
             end;

          AEditor.Color:= GetColor(FocNNode.EditorBGColor, EditorChrome.BGColor);
       end;
    end;

  finally
     if not KeepNotVisible then           // We are working with a RTFAux... EndUpdate would make it visible, although AEditor.Visible=False
        AEditor.EndUpdate;
  end;
end;

function GetColor(Color: TColor; ColorIfNone: TColor): TColor; inline;
begin
   if Color <> clNone then
      Result:= Color
   else
      Result:= ColorIfNone;
end;



initialization

end.
