unit kn_StyleObj;

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
   System.IniFiles,
   Vcl.Forms,
   Vcl.Graphics,
   Vcl.Dialogs,
   RxRichEd,
   kn_Info,
   kn_Const
   ;


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
  StyleManager : TStringList;


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

uses
   gf_misc,
   kn_INI,
   knt.RS
   ;


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
    result:= format(GetRS(sSty01),
                [Font.Name, inttostr( Font.Size ), FontStyleToStr( Font.Style ),
                 ColorToString( Font.Color ), TextInfoToStr( Text )]);
  end;
end; // FontInfoToStr

function TStyle.ParaInfoToStr( const short : boolean ) : string;
begin
  if short then
  begin
  result := Format(
  GetRS(sSty02),
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
    result:= format(GetRS(sSty03),
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
  IniFile : TMemIniFile;
  i, cnt : integer;
  section : string;
begin
  result := false;
  if (( not assigned( StyleManager )) or ( StyleManager.Count = 0 )) then exit;

  if ( FN = '' ) then
    FN := changefileext( application.exename, ext_Style );
  FN := normalFN( FN );

  deletefile( FN ); // better this than removing sections one at a time

  IniFile := TMemIniFile.Create( fn );
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
            writestring( section, 'Name', Style.Name );
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
  IniFile : TMemIniFile;
  i, r : integer;
  Style : TStyle;
  s, section : string;
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

  IniFile := TMemIniFile.Create( fn );
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
            s := readstring( section, 'Name', '' );
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
    0 : result := GetRS(sSty04);
    1 : result := '1.5';
    2 : result := GetRS(sSty05);
    else
      result := GetRS(sSty06);
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
    nsBullet : result := GetRS(sSty07);
    nsArabicNumbers,
    nsLoCaseLetter,
    nsUpCaseLetter,
    nsLoCaseRoman,
    nsUpCaseRoman : result := GetRS(sSty06);
    else
      result := '';
  end;
end; // NumberingToStr

function AlignmentToStr( const al : TParaAlignment ) : string;
begin
  case al of
    paLeftJustify : result := GetRS(sSty08);
    paRightJustify : result := GetRS(sSty09);
    paCenter : result := GetRS(sSty10);
    paJustify : result := GetRS(sSty11);
    else
      result := GetRS(sSty06);
  end;
end; // AlignmentToStr

function SubscriptStyleToStr( const ssStyle : TSubscriptStyle ) : string;
begin
  case ssStyle of
    ssSuperscript : result := GetRS(sSty12);
    ssSubscript : result := GetRS(sSty13);
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
      ssSubscript : result := GetRS(sSty14);
      ssSuperscript : result := GetRS(sSty15);
    end;

    if Disabled then
      result := result + GetRS(sSty16);
    if HasHighlight then
      result := result + GetRS(sSty17);

  end;

end;

Initialization
  StyleManager := TStringList.Create;
  StyleManager.Sorted := true;
  StyleManager.Duplicates := dupIgnore;

Finalization
  FreeStyleManager;


end.
