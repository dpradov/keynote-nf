
(* ************************************************************
 KEYNOTE: MOZILLA PUBLIC LICENSE STATEMENT.
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is KeyNote 1.0.

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 Released: 30 June 2001
 -----------------------------------------------------------
 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

unit kn_StyleObj;

interface
uses Classes, Forms, RxRichEd,
  Graphics, SysUtils, IniFiles, WideStrings,
  Dialogs, gf_misc, gf_files, kn_INI,
  kn_Const, kn_Info;


type
  TStyle = class( TObject )
    Name : TNoteNameStr;
    Range : TStyleRange;
    Font : TFontInfo;
    Para : TParaInfo;
    Text : TTextInfo;

    function FontInfoToStr( const short : boolean ) : string;
    function ParaInfoToStr( const short : boolean ) : string;

  end;

var
  StyleManager : TWideStringList;

function SaveStyleManagerInfo( FN : string ) : boolean;
function LoadStyleManagerInfo( FN : string ) : boolean;
function AddToStyleManager( const Style : TStyle ) : integer;
procedure ClearStyleManager;

function AlignmentToStr( const al : TParaAlignment ) : string;
function NumberingToStr( const num : TRxNumbering ) : string;
function LineSpacingRuleToStr( const lsr : TLineSpacingRule ) : string;
function LineSpacingToStr( const spc : integer ) : string;
function SubscriptStyleToStr( const ssStyle : TSubscriptStyle ) : string;
function TextInfoToStr( const ti : TTextInfo ) : string;

implementation

resourcestring
  STR_01 = 'Face: %s' + #13 +
            'Size: %s'  + #13 +
            'Style: %s' + #13 +
            'Color: %s' + #13 +
            'Other: %s';
  STR_02 = ' %s, %s space, %s, L:%d F:%d R:%d, Bef:%d Aft:%d';
  STR_03 = 'Alignment: %s'    + #13 +
            'Line spacing: %s' + #13 +
            'Numbering: %s'    + #13 +
            'Left indent: %s'  + #13 +
            'First indent: %s' + #13 +
            'Right indent: %s' + #13 +
            'Space before: %s' + #13 +
            'Space after: %s';
  STR_04_LineSpac = 'Single';
  STR_05_LineSpac = 'Double';
  STR_06 = 'other';
  STR_07_Numb = 'Bullets';
  STR_08_Alig = 'Left';
  STR_09_Alig = 'Right';
  STR_10_Alig = 'Center';
  STR_11_Alig = 'Justify';
  STR_12_Subsc = 'superscript';
  STR_13_Subsc = 'subscript';
  STR_14_TextInfo = 'Subscript ';
  STR_15_TextInfo = 'Supercript ';
  STR_16_TextInfo = 'Disabled ';
  STR_17_TextInfo = 'Highlighted ';


function TStyle.FontInfoToStr( const short : boolean ) : string;
begin
  if short then
  begin
    result := Format(
      ' %s %d pt %s %s %s',
      [Font.Name,
       Font.Size,
       FontStyleToStr( Font.Style ),
       ColorToString( Font.Color ),
       TextInfoToStr( Text )]
    );          

  end
  else
  begin
    result:= format(STR_01,
                [Font.Name, inttostr( Font.Size ), FontStyleToStr( Font.Style ),
                 ColorToString( Font.Color ), TextInfoToStr( Text )]);
  end;
end; // FontInfoToStr

function TStyle.ParaInfoToStr( const short : boolean ) : string;
begin
  if short then
  begin
  result := Format(
  STR_02,
  [
    AlignmentToStr( Para.Alignment ),
    LineSpacingRuleToStr( Para.SpacingRule ),
    NumberingToStr( Para.Numbering ),
    Para.LIndent,
    Para.FIndent,
    Para.RIndent,
    Para.SpaceBefore,
    Para.SpaceAfter
  ]);
  end
  else
  begin
    result:= format(STR_03,
                        [AlignmentToStr( Para.Alignment ), LineSpacingRuleToStr( Para.SpacingRule ),
                         NumberingToStr( Para.Numbering ), inttostr( Para.LIndent ),
                         inttostr( Para.FIndent ), inttostr( Para.RIndent ),
                         inttostr( Para.SpaceBefore ), inttostr( Para.SpaceAfter )]);
  end;
end; // ParaInfoToStr

procedure ClearStyleManager;
var
  i : integer;
begin
  if ( not assigned( StyleManager )) then exit;
  if ( StyleManager.Count = 0 ) then exit;

  for i := 0 to pred( StyleManager.Count ) do
    StyleManager.Objects[i].Free;
  StyleManager.Clear; 
end; // ClearStyleManager

function SaveStyleManagerInfo( FN : string ) : boolean;
var
  style : TStyle;
  IniFile : TWMemIniFile;
  i, cnt : integer;
  section : string;
begin
  result := false;
  if (( not assigned( StyleManager )) or ( StyleManager.Count = 0 )) then exit;

  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_Style );
  FN := normalFN( FN );

  deletefile( FN ); // better this than removing sections one at a time

  IniFile := TWMemIniFile.Create( fn );
  cnt := 0;

  try
    try
      with IniFile do
      begin
        for i := 0 to pred( StyleManager.Count ) do
        begin
          Style := TStyle( StyleManager.Objects[i] );
          if assigned( Style ) then
          begin
            inc( cnt );
            section := inttostr( cnt );
            writestringW( section, 'Name', Style.Name );
            writeinteger( section, 'Range', ord( Style.Range ));

            if ( Style.Range in [srFont, srBoth] ) then
            begin
              writestring( section, ChromeIniStr.FontName, Style.Font.Name );
              writeinteger( section, ChromeIniStr.FontCharset, Style.Font.Charset );
              writestring( section, ChromeIniStr.FontColor, ColorToString( Style.Font.Color ));
              writeinteger( section, ChromeIniStr.FontSize, Style.Font.Size );
              writestring( section, ChromeIniStr.FontStyle, FontStyleToStr( Style.Font.Style ));
              writebool( section, 'Disabled', Style.Text.Disabled );
              writeinteger( section, 'SubStyle', ord( Style.Text.SubscriptStyle ));

              if Style.Text.HasHighlight then
                writestring( section, 'Hilight', ColorToString( Style.Text.Highlight ));

            end;

            if ( Style.Range in [srParagraph, srBoth] ) then
            begin
              writeinteger( section, 'SpcRule', ord( Style.Para.SpacingRule ));
              writeinteger( section, 'LIndent', Style.Para.LIndent );
              writeinteger( section, 'RIndent', Style.Para.RIndent );
              writeinteger( section, 'FIndent', Style.Para.FIndent );
              writeinteger( section, 'SpcBef', Style.Para.SpaceBefore );
              writeinteger( section, 'SpcAft', Style.Para.SpaceAfter );
              writeinteger( section, 'Number', Ord( Style.Para.Numbering ));
              writeinteger( section, 'Align', ord( Style.Para.Alignment ));
            end;

          end;
        end;
      end;
      IniFile.UpdateFile;

    except
      on E : Exception do
      begin
        messagedlg( E.Message, mtError, [mbOK], 0 );
        exit;
      end;
    end;
  finally
    IniFile.Free;
  end;


  result := true;

end; // SaveStyleManagerInfo

function LoadStyleManagerInfo( FN : string ) : boolean;
var
  IniFile : TWMemIniFile;
  i, r : integer;
  Style : TStyle;
  s, section : WideString;
  sections : TStringList;
  DropStyle : boolean;
begin
  result := false;

  if ( not assigned( StyleManager )) then exit;


  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_Style );
  FN := normalFN( FN );
  if ( not fileexists( FN )) then exit;

  ClearStyleManager;   

  IniFile := TWMemIniFile.Create( fn );
  sections := TStringList.Create;

  StyleManager.BeginUpdate;
  with IniFile do
  try
      ReadSections( sections );
      if ( sections.Count > 0 ) then
      begin
        for i := 0 to pred( sections.count ) do
        begin
          try
            DropStyle := false;
            section := sections[i];
            s := readstringW( section, 'Name', '' );
            if ( s = '' ) then continue; // no style name, ignore

            Style := TStyle.Create;
            Style.Name := s;

            r := readinteger( section, 'Range', ord( srBoth ));
            if (( r < ord( low( TStyleRange ))) or ( r > ord( high( TStyleRange )))) then
              r := ord( srBoth );
            Style.Range := TStyleRange( r );

            if ( Style.Range in [srFont, srBoth] ) then
            begin
              Style.Text.Disabled := readbool( section, 'Disabled', false );
              Style.Text.SubscriptStyle := TSubscriptStyle( readinteger( section, 'SubStyle', ord( ssNone )));
              Style.Font.Name := readstring( section, ChromeIniStr.FontName, Style.Font.Name );
              Style.Font.Charset := readinteger( section, ChromeIniStr.FontCharset, Style.Font.Charset );
              Style.Font.Color := StringToColor( readstring( section, ChromeIniStr.FontColor, 'clBlack' ));
              Style.Font.Size := readinteger( section, ChromeIniStr.FontSize, Style.Font.Size );
              Style.Font.Style := StrToFontStyle( readstring( section, ChromeIniStr.FontStyle, '[]' ));
              
              try
                Style.Text.Highlight := StringToColor( readstring( section, 'Hilight', ColorToString( -1 )));
              except
                Style.Text.Highlight := -1;
              end;
              Style.Text.HasHighlight := ( Style.Text.Highlight <> -1 );

            end;

            if ( Style.Range in [srParagraph, srBoth] ) then
            begin
              Style.Para.SpacingRule := TLineSpacingRule( readinteger( section, 'SpcRule', ord( Style.Para.SpacingRule )));
              Style.Para.LIndent := readinteger( section, 'LIndent', 0 );
              Style.Para.RIndent := readinteger( section, 'RIndent', 0 );
              Style.Para.FIndent := readinteger( section, 'FIndent', 0 );
              Style.Para.SpaceBefore := readinteger( section, 'SpcBef', 0 );
              Style.Para.SpaceAfter := readinteger( section, 'SpcAft', 0 );
              Style.Para.Numbering := TRxNumbering( readinteger( section, 'Number', Ord( Style.Para.Numbering )));
              Style.Para.Alignment := TParaAlignment( readinteger( section, 'Align', ord( Style.Para.Alignment )));
            end;

            if DropStyle then
              Style.Free
            else
              StyleManager.AddObject( Style.Name, Style );

          except
            DropStyle := true;
          end;
        end;
      end;
  finally
    StyleManager.EndUpdate;
    IniFile.Free;
    sections.Free;
  end;

end; // LoadStyleManagerInfo


procedure FreeStyleManager;
var
  i : integer;
begin
  if ( StyleManager.Count > 0 ) then
    for i := 0 to pred( StyleManager.Count ) do
      StyleManager.Objects[i].Free; 
  StyleManager.Free;
end; // FreeStyleManager

function AddToStyleManager( const Style : TStyle ) : integer;
var
  idx : integer;
begin
  result := -1;
  if ( not ( assigned( StyleManager ) and assigned( Style ))) then exit;

  // style names are Case-Sensitive!
  idx := StyleManager.IndexOf( Style.Name );

  try
    if ( idx < 0 ) then
    begin
      // new style, add it
      result := StyleManager.AddObject( Style.Name, Style );
    end
    else
    begin
      // style exists, update

      with TStyle( StyleManager.Objects[idx] ) do
      begin
        range := Style.Range;
        Font := Style.Font;
        Para := Style.Para;
        Text := Style.Text;
      end;

      result := idx;
    end;
  except
    // drop exceptions
  end;

end; // AddToStyleManager

function LineSpacingToStr( const spc : integer ) : string;
begin
  case spc of
    0 : result := STR_04_LineSpac;
    1 : result := '1.5';
    2 : result := STR_05_LineSpac;
    else
      result := STR_06;
  end;
end; // LineSpacingToStr


function LineSpacingRuleToStr( const lsr : TLineSpacingRule ) : string;
begin
  result := LineSpacingToStr( ord( lsr ));
  {
  case lsr of
    lsSingle : result := 'Single';
    lsOneAndHalf : result := '1.5';
    lsDouble : result := 'Double';
    else
      result := 'other';
  end;
  }
end; // LineSpacingRuleToStr

function NumberingToStr( const num : TRxNumbering ) : string;
begin
  case num of
    nsBullet : result := STR_07_Numb;
    nsArabicNumbers,
    nsLoCaseLetter,
    nsUpCaseLetter,
    nsLoCaseRoman,
    nsUpCaseRoman : result := STR_06;
    else
      result := '';
  end;
end; // NumberingToStr

function AlignmentToStr( const al : TParaAlignment ) : string;
begin
  case al of
    paLeftJustify : result := STR_08_Alig;
    paRightJustify : result := STR_09_Alig;
    paCenter : result := STR_10_Alig;
    paJustify : result := STR_11_Alig;
    else
      result := STR_06;
  end;
end; // AlignmentToStr

function SubscriptStyleToStr( const ssStyle : TSubscriptStyle ) : string;
begin
  case ssStyle of
    ssSuperscript : result := STR_12_Subsc;
    ssSubscript : result := STR_13_Subsc;
    else
      result := '';
  end;
end; // SubscriptStyleToStr

function TextInfoToStr( const ti : TTextInfo ) : string;
begin
  result := '';
  with ti do
  begin
    case SubscriptStyle of
      ssSubscript : result := STR_14_TextInfo;
      ssSuperscript : result := STR_15_TextInfo;
    end;

    if Disabled then
      result := result + STR_16_TextInfo;
    if HasHighlight then
      result := result + STR_17_TextInfo;

  end;

end;

Initialization
  StyleManager := TWideStringList.Create;
  StyleManager.Sorted := true;
  StyleManager.Duplicates := dupIgnore;

Finalization
  FreeStyleManager;


end.
