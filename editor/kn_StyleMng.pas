unit kn_StyleMng;

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
   Vcl.Graphics,
   Vcl.Dialogs,
   Vcl.Controls,
   kn_Info,
   kn_StyleObj
   ;



    // style management
    procedure StyleCreate( aRange : TStyleRange; ExistingStyle : TStyle );
    procedure StyleApply( aName : string );
    procedure StyleRename( aName : string );
    procedure StyleDelete( aName : string );
    procedure StyleManagerToCombo;
    procedure StyleDescribe( const FromEditor, LongDesc : boolean );
    procedure StyleRedefine;

    procedure CheckSaveStyleManagerInfo;

var
    StylesModified : boolean; // styles are only saved if this flag is TRUE
    LastStyleRange : TStyleRange; // obsolete


implementation
uses
   RxRichEd,
   kn_Cmd,
   kn_Global,
   kn_Main,
   kn_NoteFileMng,
   kn_MacroMng,
   knt.ui.editor,
   knt.App,
   knt.RS
   ;


procedure StyleDescribe( const FromEditor, LongDesc : boolean );
var
  s : string;
  style : TStyle;
  Editor: TKntRichEdit;
begin

  with Form_Main do begin
      s := '';

      if FromEditor then begin
        if not App.CheckActiveEditor then exit;
        Editor:= ActiveEditor;
        s := GetRS(sStyM01) + #13#13 + GetRS(sStyM02) + Editor.FontInfoString + #13#13 + GetRS(sStyM03) + Editor.ParaInfoString;
      end
      else begin
        if ( Combo_Style.ItemIndex < 0 ) or
        ( Combo_Style.ItemIndex >= StyleManager.Count ) then exit;

        style := TStyle( StyleManager.Objects[Combo_Style.ItemIndex] );
        case style.range of
          srFont : s := Style.FontInfoToStr( false );
          srParagraph : s := Style.ParaInfoToStr( false );
          srBoth : s := Style.FontInfoToStr( false ) + #13#13 + Style.ParaInfoToStr( false );
        end;
        s := GetRS(sStyM04) + '"' + Style.Name + '"' + #13 +
             GetRS(sStyM05) + STYLE_RANGES[Style.Range] + #13#13 + s;
      end;
      App.DoMessageBox( s, mtInformation, [mbOK] );
  end;
end; // StyleDescribe


procedure StyleCreate( aRange : TStyleRange; ExistingStyle : TStyle );
var
  name : string;
  Style : TStyle;
  idx : integer;
  Editor: TKntRichEdit;
begin
  if not App.CheckActiveEditor then exit;

  if ( not assigned( StyleManager )) then begin
    showmessage( GetRS(sStyM06) );
    exit;
  end;

  if ( ExistingStyle = nil ) then begin
    if ( not InputQuery(Format( GetRS(sStyM07), [STYLE_RANGES[aRange]] ), GetRS(sStyM08), name )) then exit;
    if ( name = '' ) then exit;
    idx := StyleManager.IndexOf( name );
    if ( idx >= 0 ) then
      ExistingStyle := TStyle( StyleManager.Objects[idx] );
  end
  else
    name := ExistingStyle.Name;

  if ( ExistingStyle <> nil ) then begin
    if (App.DoMessageBox(Format(GetRS(sStyM09), [STYLE_RANGES[ExistingStyle.Range],ExistingStyle.Name]),
           mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then exit;
  end;

  if ( ExistingStyle = nil ) then begin
    Style := TStyle.Create;
    Style.Name := name;
    Style.Range := aRange;
  end
  else
    Style := ExistingStyle;

  Editor:= ActiveEditor;

  with Style do begin

    if ( Range in [srFont, srBoth] ) then
       with Editor.SelAttributes do begin
         Font.Charset := Charset;
         Font.Color := Color;
         Font.Name := Name;
         Font.Size := Size;
         Font.Style := Style;
         Text.Disabled := Disabled;
         Text.SubscriptStyle := SubscriptStyle;
         // [l] Text.Language := Language;

         Text.HasHighlight := ( BackColor <> clWindow );
         if Text.HasHighlight then
           Text.Highlight := BackColor;
       end;

    if ( Range in [srParagraph, srBoth] ) then
       with Editor.Paragraph do begin

         case LineSpacing of
           0 : Para.SpacingRule := lsSingle;
           1 : Para.SpacingRule := lsOneAndHalf;
           else
             Para.SpacingRule := lsDouble;
         end;

         Para.Numbering := Numbering;
         Para.Alignment := Alignment;
         Para.LIndent := LeftIndent;
         Para.RIndent := RightIndent;
         Para.FIndent := FirstIndent;
         Para.SpaceBefore := SpaceBefore;
         Para.SpaceAfter := SpaceAfter;
       end;

    Editor.SetFocus;

    try
      StylesModified := true;
      idx := AddToStyleManager( Style );
      StyleManagerToCombo;
      Form_Main.Combo_Style.ItemIndex := idx;
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( GetRS(sStyM10), [Name,STYLE_RANGES[aRange]] );

      CheckSaveStyleManagerInfo;

    except
      on E : Exception do begin
        messagedlg( GetRS(sStyM11) + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  end;
end; // StyleCreate

procedure StyleApply( aName : string );
var
  myStyle : TStyle;
  Editor: TKntRichEdit;
begin
  with Form_Main do begin
      if not App.CheckActiveEditorNotReadOnly then exit;

      try
        if ( aName = '' ) then
          myStyle := TStyle( StyleManager.Objects[Combo_Style.ItemIndex] )
        else
          myStyle := TStyle( StyleManager.Objects[StyleManager.IndexOf( aName )] );
      except
        App.DoMessageBox( Format(GetRS(sStyM12), [aName]), mtError, [mbOK] );
        exit;
      end;

      if ( not assigned( myStyle )) then exit;

      CommandRecall.StyleName := MyStyle.Name;
      UpdateLastCommand( ecStyleApply );
      if IsRecordingMacro then
        AddMacroEditCommand( ecStyleApply );

      Editor:= ActiveEditor;

      Editor.BeginUpdate;
      try
        with myStyle do begin
            if ( Range in [srFont, srBoth] ) then
            with Editor.SelAttributes do begin
              Charset := Font.Charset;
              Editor.SuspendUndo;

              Color := Font.Color;
              Name := Font.Name;
              Size := Font.Size;
              Style := Font.Style;
              Disabled := Text.Disabled;
              SubscriptStyle := Text.SubscriptStyle;
              // [l] Language := Text.Language;
              if Text.HasHighlight then
                BackColor := Text.Highlight;

              Editor.ResumeUndo;
            end;

            if ( Range in [srParagraph, srBoth] ) then
               with Editor.Paragraph do begin
                 LineSpacingRule := Para.SpacingRule;
                 Editor.SuspendUndo;

                 case LineSpacingRule of
                   lsSingle : begin
                     Editor.Paragraph.LineSpacingRule := lsSingle;
                     Editor.Paragraph.LineSpacing := 0;
                   end;
                   lsOneAndHalf : begin
                     Editor.Paragraph.LineSpacingRule := lsOneAndHalf;
                     Editor.Paragraph.LineSpacing := 1; // EditorOptions.LineSpcInc;
                   end;
                   lsDouble : begin
                     Editor.Paragraph.LineSpacingRule := lsDouble;
                     Editor.Paragraph.LineSpacing := 2; // 2*EditorOptions.LineSpcInc;
                   end;
                 end;

                 Numbering := Para.Numbering;
                 Alignment := Para.Alignment;
                 LeftIndent := Para.LIndent;
                 RightIndent := Para.RIndent;
                 FirstIndent := Para.FIndent;
                 SpaceBefore := Para.SpaceBefore;
                 SpaceAfter := Para.SpaceAfter;

                 Editor.ResumeUndo
               end;

        end;

        try
          Editor.SetFocus;
        except
        end;

      finally
        Editor.EndUpdate;

        Editor.Change;
        Editor.ChangedSelection;
      end;
  end;

end; // StyleApply

procedure StyleRename( aName : string );
var
  i, idx : integer;
  name : string;
begin
  idx := Form_Main.Combo_Style.ItemIndex;
  name := Form_Main.Combo_Style.Items[Form_Main.Combo_Style.ItemIndex];

  if ( not InputQuery(
    GetRS(sStyM13), GetRS(sStyM14), name )) then exit;
  if ( name = '' ) then exit;

  if ( name = Form_Main.Combo_Style.Items[Form_Main.Combo_Style.ItemIndex] ) then exit;

  try
    for i := 0 to pred( StyleManager.Count ) do
      if (( i <> idx ) and ( StyleManager[i] = name )) then
      begin
        showmessage( GetRS(sStyM15) );
        exit;
      end;

    StyleManager.Sorted := false;
    try
      StyleManager[idx] := name;
      TStyle( StyleManager.Objects[idx] ).Name := name;
    finally
      StylesModified := true;
      StyleManager.Sorted := true;
      StyleManagerToCombo;
    end;

    Form_Main.Combo_Style.ItemIndex := Form_Main.Combo_Style.Items.IndexOf( name );

    CheckSaveStyleManagerInfo;

  except
    showmessage( GetRS(sStyM16) );
  end;

end; // StyleRename

procedure StyleDelete( aName : string );
var
  idx : integer;
  name : string;
begin
      idx := Form_Main.Combo_Style.ItemIndex;
      name := Form_Main.Combo_Style.Items[idx];

      if (App.DoMessageBox( Format(GetRS(sStyM17),  [STYLE_RANGES[TStyle( StyleManager.Objects[idx] ).Range],name] ),
                            mtConfirmation, [mbYes,mbNo] ) <> mrYes ) then exit;

      try

        Form_Main.Combo_Style.Items.Delete( idx );
        StyleManager.Objects[idx].Free;
        StyleManager.Delete( idx );
        StylesModified := true;

        if ( Form_Main.Combo_Style.Items.Count > 0 ) then
          Form_Main.Combo_Style.ItemIndex := 0;

        CheckSaveStyleManagerInfo;

      except
        showmessage( GetRS(sStyM18) );
      end;

end; // StyleDelete

procedure StyleManagerToCombo;
var
  i : integer;
begin
  with Form_Main do begin
      Combo_Style.Items.BeginUpdate;
      Combo_Style.Items.Clear;
      try
        if ( StyleManager.Count > 0 ) then
        begin
          for i := 0 to pred( StyleManager.Count ) do
          begin
            // [style]
            // Combo_Style.AddItem( StyleManager[i], STYLE_IMAGE_BASE + ord( TStyle( StyleManager.Objects[i] ).Range ));
            Combo_Style.Items.Add( StyleManager[i] );
          end;
        end;
      finally
        Combo_Style.Items.EndUpdate;
      end;
  end;
end; // StyleManagerToCombo



procedure StyleRedefine;
begin
  StyleCreate( LastStyleRange, TStyle( StyleManager.Objects[Form_Main.Combo_Style.ItemIndex] ));
end;

procedure CheckSaveStyleManagerInfo;
begin
  if StylesModified and (not App.opt_NoSaveOpt) then begin
     if SaveStyleManagerInfo( Style_FN ) then
        StylesModified := false;
  end;
end;

initialization
  StylesModified := false;

end.
