unit kn_StyleMng;

interface
uses
  kn_Info, kn_StyleObj;

    // style management
    procedure StyleCreate( aRange : TStyleRange; ExistingStyle : TStyle );
    procedure StyleApply( aName : string );
    procedure StyleRename( aName : string );
    procedure StyleDelete( aName : string );
    procedure StyleManagerToCombo;
    procedure StyleDescribe( const FromEditor, LongDesc : boolean );
    procedure StyleRedefine;

var
    StylesModified : boolean; // styles are only saved if this flag is TRUE
    LastStyleRange : TStyleRange; // obsolete


implementation
uses
  Graphics, Dialogs, Controls, Menus, SysUtils,
  RxRichEd,
  kn_Cmd, kn_Global,kn_Main,kn_NoteFileMng,
  kn_MacroMng;

resourcestring
  STR_01 = 'Style in active note: ';
  STR_02 = 'Font: ';
  STR_03 = 'Paragraph: ';
  STR_04 = 'Named style: ';
  STR_05 = 'Range: ';
  STR_06 = 'Error: StyleManager does not exist.';
  STR_07 = 'Create %s style';
  STR_08 = 'Enter name for new style:';
  STR_09 = '%s style "%s" already exists. ' + #13 + 'Redefine the existing style with new properties?';
  STR_10 = ' Style %s created (%s)';
  STR_11 = 'Error creating style: ';
  STR_12 = 'Error: Cannot access style information for "%s"';
  STR_13 = 'Rename style';
  STR_14 = 'Enter new name for style:';
  STR_15 = 'Cannot rename: a style by that name already exists.';
  STR_16 = 'Error renaming style';
  STR_17 = 'OK to delete %s style "%s"?';
  STR_18 = 'Error deleting style';

procedure StyleDescribe( const FromEditor, LongDesc : boolean );
var
  s : string;
  style : TStyle;
begin
  with Form_Main do begin
      s := '';
      if FromEditor then
      begin
        if ((( not HaveNotes( true, true )) or ( not assigned( ActiveNote )))) then exit;
        s := STR_01 + #13#13 + STR_02 + ActiveNote.Editor.FontInfoString + #13#13 + STR_03 + ActiveNote.Editor.ParaInfoString;
      end
      else
      begin
        if ( Combo_Style.ItemIndex < 0 ) or
        ( Combo_Style.ItemIndex >= StyleManager.Count ) then exit;

        style := TStyle( StyleManager.Objects[Combo_Style.ItemIndex] );
        case style.range of
          srFont : s := Style.FontInfoToStr( false );
          srParagraph : s := Style.ParaInfoToStr( false );
          srBoth : s := Style.FontInfoToStr( false ) + #13#13 + Style.ParaInfoToStr( false );
        end;
        s := STR_04 + Style.Name +  #13 +
             STR_05 + STYLE_RANGES[Style.Range] + #13#13 + s;
      end;
      messagedlg( s, mtInformation, [mbOK], 0 );
  end;
end; // StyleDescribe

procedure StyleCreate( aRange : TStyleRange; ExistingStyle : TStyle );
var
  name : string;
  Style : TStyle;
  idx : integer;
begin
  if ( not Form_Main.HaveNotes( true, true )) then exit;
  if ( not assigned( ActiveNote )) then exit;

  if ( not assigned( StyleManager )) then
  begin
    showmessage( STR_06 );
    exit;
  end;


  if ( ExistingStyle = nil ) then
  begin
    if ( not InputQuery(
      Format( STR_07, [STYLE_RANGES[aRange]] ),
      STR_08, name )) then exit;
    if ( name = '' ) then exit;
    idx := StyleManager.IndexOf( name );
    if ( idx >= 0 ) then
      ExistingStyle := TStyle( StyleManager.Objects[idx] );
  end
  else
  begin
    name := ExistingStyle.Name;
  end;

  if ( ExistingStyle <> nil ) then
  begin
    if ( messagedlg( Format(
      STR_09,
      [STYLE_RANGES[ExistingStyle.Range],ExistingStyle.Name] ),
      mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;
  end;

  if ( ExistingStyle = nil ) then
  begin
    Style := TStyle.Create;
    Style.Name := name;
    Style.Range := aRange;
  end
  else
  begin
    Style := ExistingStyle;
  end;


  with Style do
  begin

    if ( Range in [srFont, srBoth] ) then
    with ActiveNote.Editor.SelAttributes do
    begin
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
    with ActiveNote.Editor.Paragraph do
    begin

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

    ActiveNote.Editor.SetFocus;

    try
      StylesModified := true;
      idx := AddToStyleManager( Style );
      StyleManagerToCombo;
      Form_Main.Combo_Style.ItemIndex := idx;
      Form_Main.StatusBar.Panels[PANEL_HINT].Text := Format( STR_10, [Name,STYLE_RANGES[aRange]] );

    except
      on E : Exception do
      begin
        messagedlg( STR_11 + E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  end;
end; // StyleCreate

procedure StyleApply( aName : string );
var
  myStyle : TStyle;
begin
  with Form_Main do begin
      if ( not HaveNotes( true, true )) then exit;
      if ( not assigned( ActiveNote )) then exit;
      if NoteIsReadOnly( ActiveNote, true ) then exit;

      try
        if ( aName = '' ) then
          myStyle := TStyle( StyleManager.Objects[Combo_Style.ItemIndex] )
        else
          myStyle := TStyle( StyleManager.Objects[StyleManager.IndexOf( aName )] );
      except
        messagedlg( Format(
          STR_12,
          [aName]
        ), mtError, [mbOK], 0 );
        exit;
      end;

      if ( not assigned( myStyle )) then exit;

      CommandRecall.StyleName := MyStyle.Name;
      UpdateLastCommand( ecStyleApply );
      if IsRecordingMacro then
        AddMacroEditCommand( ecStyleApply );

      try
        with myStyle do
        begin
            if ( Range in [srFont, srBoth] ) then
            with ActiveNote.Editor.SelAttributes do
            begin
              Charset := Font.Charset;
              Color := Font.Color;
              Name := Font.Name;
              Size := Font.Size;
              Style := Font.Style;
              Disabled := Text.Disabled;
              SubscriptStyle := Text.SubscriptStyle;
              // [l] Language := Text.Language;
              if Text.HasHighlight then
                BackColor := Text.Highlight;
            end;

            if ( Range in [srParagraph, srBoth] ) then
            with ActiveNote.Editor.Paragraph do
            begin
              LineSpacingRule := Para.SpacingRule;
              case LineSpacingRule of
                lsSingle : begin
                  ActiveNote.Editor.Paragraph.LineSpacingRule := lsSingle;
                  ActiveNote.Editor.Paragraph.LineSpacing := 0;
                end;
                lsOneAndHalf : begin
                  ActiveNote.Editor.Paragraph.LineSpacingRule := lsOneAndHalf;
                  ActiveNote.Editor.Paragraph.LineSpacing := 1; // EditorOptions.LineSpcInc;
                end;
                lsDouble : begin
                  ActiveNote.Editor.Paragraph.LineSpacingRule := lsDouble;
                  ActiveNote.Editor.Paragraph.LineSpacing := 2; // 2*EditorOptions.LineSpcInc;
                end;
              end;

              Numbering := Para.Numbering;
              Alignment := Para.Alignment;
              LeftIndent := Para.LIndent;
              RightIndent := Para.RIndent;
              FirstIndent := Para.FIndent;
              SpaceBefore := Para.SpaceBefore;
              SpaceAfter := Para.SpaceAfter;
            end;

        end;

        try
          ActiveNote.Editor.SetFocus;
        except
        end;

      finally
        ActiveNote.Modified := true;
        NoteFile.Modified := true;
        RxRTFSelectionChange( ActiveNote.Editor );
        UpdateNoteFileState( [fscModified] );
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
    STR_13, STR_14, name )) then exit;
  if ( name = '' ) then exit;

  if ( name = Form_Main.Combo_Style.Items[Form_Main.Combo_Style.ItemIndex] ) then exit;

  try
    for i := 0 to pred( StyleManager.Count ) do
      if (( i <> idx ) and ( StyleManager[i] = name )) then
      begin
        showmessage( STR_15 );
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

  except
    showmessage( STR_16 );
  end;

end; // StyleRename

procedure StyleDelete( aName : string );
var
  idx : integer;
  name : string;
begin
      idx := Form_Main.Combo_Style.ItemIndex;
      name := Form_Main.Combo_Style.Items[idx];

      if ( messagedlg( Format(
        STR_17,
        [STYLE_RANGES[TStyle( StyleManager.Objects[idx] ).Range],name] ),
        mtConfirmation, [mbYes,mbNo], 0 ) <> mrYes ) then exit;

      try

        Form_Main.Combo_Style.Items.Delete( idx );
        StyleManager.Objects[idx].Free;
        StyleManager.Delete( idx );
        StylesModified := true;

        if ( Form_Main.Combo_Style.Items.Count > 0 ) then
          Form_Main.Combo_Style.ItemIndex := 0;

      except
        showmessage( STR_18 );
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

initialization
  StylesModified := false;

end.
