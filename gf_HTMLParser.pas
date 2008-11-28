(* ************************************************************
 MOZILLA PUBLIC LICENSE STATEMENT
 -----------------------------------------------------------
 The contents of this file are subject to the Mozilla Public
 License Version 1.1 (the "License"); you may not use this file
 except in compliance with the License. You may obtain a copy of
 the License at http://www.mozilla.org/MPL/

 Software distributed under the License is distributed on an "AS
 IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

 The Original Code is "gf_HTMLParser.pas".

 The Initial Developer of the Original Code is Marek Jedlinski
 <eristic@lodz.pdi.net> (Poland).
 Portions created by Marek Jedlinski are
 Copyright (C) 2000, 2001. All Rights Reserved.
 -----------------------------------------------------------
 Contributor(s):
 -----------------------------------------------------------
 History:
 -----------------------------------------------------------
 To do:
 -----------------------------------------------------------
 Released: 20 August 2001
 -----------------------------------------------------------
 URLs:

 - original author's software site:
 http://www.lodz.pdi.net/~eristic/free/index.html
 http://go.to/generalfrenetics

 Email addresses (at least one should be valid)
 <eristic@lodz.pdi.net>
 <cicho@polbox.com>
 <cicho@tenbit.pl>

************************************************************ *)

unit gf_HTMLParser;
{$I gf_base.inc}

interface
uses Windows, SysUtils, Classes,
  gf_HTMLTokens, gf_HTMLtags, gf_HTMLBase;

type
  TCaseChange = (
    caseAsIs, caseUpper, caseLower
  );

const
  CASECHANGE_NAMES : array[TCaseChange] of string = (
    'As is', 'Uppercase', 'Lowercase'
  );

type
  TSyntaxWarningEvent = procedure(
    sender : TObject; const s : string; const i : integer; var Abort : boolean
  ) of object;

type
  TTokenEvent = procedure(
    sender : TObject; Token : TToken; var Add : boolean
  ) of object;


type
  TGFHTMLParser = class( TComponent )
  private
    FIsParsing : boolean;
    FCaseChange : TCaseChange;
    FTokenList : TTokenList;
    FProgressIncrement : integer;
    FExpandEntities : boolean;
    FLineBreak : string;

    FOnSyntaxWarning : TSyntaxWarningEvent;
    FOnProgress : TNotifyEvent;
    FOnToken : TTokenEvent;

    function Peek( const S : TStream; const Count : byte ) : string;
    function SyntaxWarning( const s : string; const i : integer ) : boolean; // abort if returns TRUE

  public
    property IsParsing : boolean read FIsParsing;
    property TokenList : TTokenList read FTokenList;

    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;

    function Parse( Stream : TStream ) : integer;
    function ParseFile( const FN : string ) : integer;

    procedure SaveTokens( const FN : string );
    procedure RebuildFile( const FN : string );

  published
    property CaseChange : TCaseChange read FCaseChange write FCaseChange default caseAsIs; // [x] not implemented
    property ProgressIncrement : integer read FProgressIncrement write FProgressIncrement default 1024;
    property ExpandEntities : boolean read FExpandEntities write FExpandEntities default false; // [x] not implemented
    property LineBreak : string read FLineBreak write FLineBreak;
    property OnSyntaxWarning : TSyntaxWarningEvent read FOnSyntaxWarning write FOnSyntaxWarning;
    property OnProgress : TNotifyEvent read FOnProgress write FOnProgress;
    property OnToken : TTokenEvent read FOnToken write FOnToken;

  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents( 'Custom', [TGFHTMLParser] );
end; // Register


{ TGFHTMLParser }

constructor TGFHTMLParser.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );
  FIsParsing := false;
  FCaseChange := caseAsIs;
  FProgressIncrement := 1024;
  FExpandEntities := false;
  FOnSyntaxWarning := nil;
  FOnProgress := nil;
  FOnToken := nil;
  FLineBreak := #13#10;
  FTokenList := TTokenList.Create;
end; // CREATE

destructor TGFHTMLParser.Destroy;
begin
  FTokenList.Free;
  inherited Destroy;
end; // DESTROY

function TGFHTMLParser.Parse(Stream: TStream): integer;
var
  InTag, InQuotes, InStyle, InScript, InComment : boolean;
  ch, prevch, prevquote : char;
  text, tmpstr : string;
  Last3 : string[3];
  i, len : integer;
  LineNum, SizeRead, SizeTotal : longint;
  TokenIsEOL : boolean;

          { inline procedure }
          procedure FlushToken;
          var
            Token : TToken;
            len : integer;
            OKtoAdd : boolean;
          begin
            if ( text = '' ) then exit;
            Token := TToken.Create;
            Token.LineNumber := LineNum;

            if InScript then
            begin
              Token.TokenClass := tcScript;
            end
            else
            if InStyle then
            begin
              Token.TokenClass := tcStyle;
            end
            else
            if InComment then
            begin
              Token.TokenClass := tcComment;
            end
            else
            if InTag then
            begin
              // this may be a regular HTML tag, but it might also be
              // a DTD or ASP construct. We should NOT arrive here if
              // text represents part of a comment
              len := length( text );
              if ( len > 2 ) then
              begin
                case text[2] of
                  '!', '?' : begin
                    Token.TokenClass := tcDTD;
                  end;
                  '%' : begin
                    Token.TokenClass := tcASP;
                  end;
                  else
                  begin
                    Token.TokenClass := tcTag;
                  end;
                end;
              end
              else
              begin
                // has length of 1 or 2 characters
                // so this cannot be a valid <TAG>
                Token.TokenClass := tcUndef; // flag it as a problem
                SyntaxWarning( 'Illegal construct: incomplete tag', LineNum );
              end;
            end
            else
            begin
              if TokenIsEOL then
              begin
                Token.TokenClass := tcEOL;
              end
              else
              begin
                Token.TokenClass := tcText;
              end;
            end;

            Token.Text := Text;
            text := '';

            if ( Token.TokenClass = tcTag ) then
            begin
              case token.tag.Element of
                elemScript : InScript := token.tag.IsStartTag;
                elemStyle : InStyle := token.tag.IsStartTag;
              end;
            end;

            // this allows user to filter or replace tokens
            // as they are added. It also allows the component
            // to be used for parsing without having to store
            // the entire data in memory (a parsed token can be
            // processed in FOnToken event and then discarded)
            OKtoAdd := true;
            if assigned( FOnToken ) then
              FOnToken( self, Token, OKtoAdd );
            if OKtoAdd then
              FTokenList.Add( Token )
            else
              Token.Free;

          end;// FlushToken

begin
  result := 0;

  SizeRead := 0;
  SizeTotal := Stream.Size;
  LineNum := 1;

  InTag := false;
  InQuotes := false;
  InStyle := false;
  InScript := false;
  InComment := false;

  FTokenList.Clear;

  TokenIsEOL := false;

  prevch := #0;
  prevquote := #0;

  Text := '';
  Last3 := '   ';

  FIsParsing := false;

  try

    repeat
      if ( Stream.Read( ch, 1 ) = 0 ) then break;
      inc( SizeRead );

      if ( assigned( FOnProgress ) and (( SizeRead MOD FProgressIncrement ) = 0 )) then
      begin
        FOnProgress( self );
      end;

      case ch of

        '<' : begin
          If ( InQuotes or InComment ) then
          begin
            text := text + ch;
          end
          else
          begin
            // not in quotes, not in comment.
            // could be the start of a new TAG, but it could also be
            // some random stuff inside <SCRIPT>, for example
            if ( InStyle or InScript ) then
            begin
              text := text + ch;
              // if this is a tag CLOSING the SCRIPT or STYLE block,
              // we will resolve it later
            end
            else
            begin
              if InTag then
              begin
                // this should NOT happen!
                if SyntaxWarning( 'Illegal construct: Unquoted opening bracket inside a tag', LineNum ) then break;
                // this will often happen when a tag is not closed.
                // we recover by closing it.
                text := text + '>';
                FlushToken;
                text := '<';
              end
              else
              begin
                FlushToken;
                InTag := true;
                text := '<';
              end;
            end;
          end;
        end; // opening bracker <

        '>' : begin
          If InQuotes then //
          begin
            text := text + ch;
          end
          else
          begin
            if InComment then
            begin
              // check if comment is being closed
              if ( copy( Last3, 2, 2 ) = '--' ) then
              begin
                // yes, comment is closing
                // do not append the > character. Instead, strip the comment marker.
                (*
                // Comments are stored without the <!-- --> delimiters,
                // unless they occur inside STYLE or SCRIPT blocks
                if ( not ( InStyle or InScript )) then
                  delete( text, ( length( text ) - 1 ), 2 )
                else
                *)
                text := text + ch;
                FlushToken;
                InComment := false;
              end
              else
              begin
                // not closing comment, just add it.
                text := text + ch;
              end;
            end
            else
            begin
              // not in comment. A tag can be closing, but we could also be
              // inside a STYLE or SCRIPT block
              if InTag then // InTag is NOT set while within STYLE and SCRIPT blocks
              begin
                text := text + ch;
                FlushToken;
                InTag := false;
              end
              else
              begin
                // not inside a tag
                if ( InScript or InStyle ) then
                begin
                  // check if it is a </STYLE> or </SCRIPT> tag
                  // but note that the trailing > may be preceded by white space
                  len := length( text );
                  tmpstr := '';
                  i := len;
                  // skip any trailing whitespace
                  while (( i > 0 ) and ( text[i] in _WhiteSpace )) do
                    dec( i );
                  while ( i > 0 ) do
                  begin
                    tmpstr := text[i] + tmpstr;
                    if ( text[i] = '<' ) then break;
                    dec( i );
                  end;
                  if ( i = 0 ) then
                  begin
                    // we looked back, but did not encounter a starting <,
                    // so this cannot be a closing tag
                    text := text + ch;
                  end
                  else
                  begin
                    tmpstr := uppercase( tmpstr ) + '>';
                    if (( InScript and ( tmpstr = '</' + ELEMENT_NAMES[elemSCRIPT] + '>' )) or
                       ( InStyle and ( tmpstr = '</' + ELEMENT_NAMES[elemSTYLE] + '>' ))) then
                    begin
                      // found closing </SCRIPT>
                      // first, flush anything that precedes the closing tag
                      delete( text, i, len );
                      FlushToken;
                      // then, process the closing tag
                      text := tmpstr;
                      InTag := true;
                      if InScript then
                        InScript := false
                      else
                        InStyle := false;
                      FlushToken;
                      InTag := false;
                    end
                    else
                    begin
                      // it was some sort of a <TAG>, but not a closing </STYLE> or </SCRIPT>
                      // so just continue
                      text := text + ch;
                    end;
                  end;
                end
                else
                begin
                  // orphaned '>' - nowhere to place it!
                  if SyntaxWarning( 'Illegal construct: orphaned closing bracket', LineNum ) then break;
                  text := text + ch;
                end;
              end;
            end;
          end;
        end; // closing bracket >

        '-' : begin
          if InQuotes then
          begin
            text := text + ch;
          end
          else
          begin
            // check if we are starting a comment, rather than a tag
            if ( InTag or InScript or InStyle ) then
            begin
              if ( Last3 = '<!-' ) then
              begin
                // YES, a comment is starting
                InTag := false;
                InComment := true;
                (*
                // Comments are stored without the <!-- --> delimiters
                // unless they occur inside STYLE or SCRIPT blocks
                if ( not ( InStyle or InScript )) then
                  delete( text, ( length( text ) - 2 ), 3 )
                else
                *)
                  text := text + ch;
              end
              else
              begin
                // not starting a comment
                text := text + ch;
              end;
            end
            else
            begin
              text := text + ch;
            end;
          end;
        end; // dash


        #10, {#13,}
        #9, #32 : begin
          if ( InTag and ( not InQuotes )) then
          begin
            if ( prevch <> '<' ) then
            begin
              // within tags, we do collapse white space (spaces, tabs and linebreaks)
              if ( not ( prevch in _WhiteSpace )) then
              begin
                text := text + #32;
              end;
            end
            else
            begin
              // special case: if whitespace follows a <, this cannot be a valid
              // tag, so we demote it to plain text. This is not legal, but browsers
              // do interpret such orphaned <'s as plain text characters.
              if SyntaxWarning( 'Illegal construct: whitespace follows unquoted opening bracket inside a tag', LineNum ) then break;
              InTag := false;
              text := text + ch;
            end;
          end
          else
          begin
            // Spaces are NOT collapsed anywhere outside of real tags.
            // However, we do have special processing for linebreaks.
            if ( ch in [#10{,#13}] ) then
            begin
              if InQuotes then
              begin
                // should not really happen: linebreak inside a quoted text
                // would terminate the quoted text
                if SyntaxWarning( 'Illegal construct: line break inside quoted text', LineNum ) then break;
                // we need this to recover:
                if ( InScript or InStyle ) then
                  InQuotes := false;
              end;
              FlushToken;          // store what we already have
              TokenIsEOL := true;
              text := FLineBreak;
              FlushToken;          // store the linebreak
              TokenIsEOL := false;
              inc( LineNum );
            end
            else
            begin
              // space or tab, not a line break
              text := text + ch;
            end;
          end;
        end; // Space or Tab

        #13 : begin
          // ignore
          // [x] this will break on Mac files
          ch := prevch;
        end;

        '"', '''' : begin
          Text := Text + ch;
          // interpret quotes ONLY in tags, not in normal text
          if (( InTag or InScript or InStyle ) and ( not InComment )) then
          begin
            if InQuotes then
            begin
              if ( prevQuote = ch ) then
              begin
                // end quoted text
                InQuotes := false;
              end;
            end
            else
            begin
              // begin quoted text
              InQuotes := true;
              prevQuote := ch;
            end;
          end;
        end; // single or double quote

        else
        begin
          text := text + ch;
        end; // any other character


      end; // case ch

      prevch := ch;
      delete( Last3, 1, 1 );
      Last3 := Last3 + ch;

    until false;

    FlushToken; // if any text remains

  finally
    FIsParsing := false;
  end;

end; // Parse


function TGFHTMLParser.Peek(const S: TStream;
  const Count: byte): string;
{ peek Count chars forward into the stream }
var
  oldPos : longint;
  sizeread : integer;
  buf : array[0..255] of char;
begin
  oldPos := S.Position;
  sizeread := S.Read( buf, Count );
  setlength( result, sizeread );
  result := buf;
  S.Position := oldPos;
end; // Peek


function TGFHTMLParser.ParseFile(const FN: string): integer;
var
  Stream : TFileStream;
begin
  result := -1;

  Stream := TFileStream.Create( FN, ( fmOpenRead or fmShareDenyWrite ));

  try
    result := Parse( Stream );
  finally
    Stream.Free;
  end;

end; // ParseFile


function TGFHTMLParser.SyntaxWarning(const s: string; const i: integer) : boolean;
begin
  if assigned( FOnSyntaxWarning ) then
  begin
    result := false;
    FOnSyntaxWarning( self, s, i, result );
  end
  else
  begin
    result := false;
  end;
end; // SyntaxWarning


procedure TGFHTMLParser.SaveTokens(const FN: string);
var
  f : textfile;
  i, a : integer;
  token : TToken;
  attr : TTagAttribute;
begin

  assignfile( f, FN );
  rewrite( f );

  try
    for i := 1 to FTokenList.Count do
    begin
      token := FTokenList[pred( i )];
      writeln( f );
      writeln( f, Format(
        'token %d : %s  (line %d)',
        [i,TOKEN_CLASSES[Token.TokenClass],Token.LineNumber]
      ));
      case Token.TokenClass of
        tcTag : begin
          if token.tag.IsStartTag then
          begin
            writeln( f, Format(
              ' Element: %s  (%d attributes)',
              [ELEMENT_NAMES[token.Tag.Element],token.Tag.AttributeList.Count]
            ));
          end
          else
          begin
            writeln( f, Format(
              ' Element: /%s  (%d attributes)',
              [ELEMENT_NAMES[token.Tag.Element],token.Tag.AttributeList.Count]
            ));
          end;

          if ( token.Tag.AttributeList.Count > 0 ) then
          begin
            for a := 1 to token.Tag.AttributeList.Count do
            begin
              attr := token.Tag.AttributeList[pred( a )];
              writeln( f, Format(
                '  attr: %s  value: %s',
                [attr.Name, attr.Value]
              ));
            end;
          end;
        end;
        tcEOL : begin
          // nothing
        end;
        else
        begin
          writeln( f, Token.Text );
        end;
      end;
    end;
  finally
    closefile( f );
  end;

end; // SaveTokens


procedure TGFHTMLParser.RebuildFile(const FN: string);
var
  f : textfile;
  i : integer;
  token : TToken;
begin

  if ( FTokenList.Count = 0 ) then exit;

  assignfile( f, FN );
  rewrite( f );

  try
    for i := 1 to FTokenList.Count do
    begin
      token := FTokenList[pred( i )];
      case token.TokenClass of
        tcTag : begin
          write( f, token.tag.text );
        end;
        else
          write( f, token.Text );
      end;
    end;
  finally
    closefile( f );
  end;

end; // RebuildFile

end.
