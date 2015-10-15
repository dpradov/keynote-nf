unit gf_HTMLTokens;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 

{$I gf_base.inc}

interface
uses Windows, SysUtils, Classes, contnrs,
  gf_HTMLtags, gf_HTMLBase;

type
  TCharSet = set of char;

const
  _WhiteSpace : TCharset = [#9,#10,#13,#32];

type
  THTMLLocation = (
    locHTML, locHEAD, locBODY, locMETA
  );

const
  HTML_LOCATIONS : array[THTMLLocation] of string = (
    'HTML', 'Head', 'Body', 'Meta'
  );

type
  TTokenClass = (
    tcTag,        // normal HTML tag
    tcText,       // text (PCData)
    tcComment,    // comment
    tcEOL,        // end of line
    tcStyle,      // style def
    tcScript,     // script
    tcDTD,        // DTD declaration
    tcSSI,         { server-side includes, e.g.:
                     <!--#exec cgi="foo.cgi"-->
                     <!--#include file="foo.html"-->
                     <!--#echo var="DOCUMENT_NAME"-->
                   }
    tcASP,        // <%foo%>
    tcUndef       // unknown
  );

const
  TOKEN_CLASSES : array[TTokenClass] of string = (
    'Tag',
    'Text',
    'Comment',
    'Eol',
    'Style',
    'Script',
    'DTD',
    'SSI',
    'ASP',
    'Undefined'
  );

type
  TToken = class( TObject )
  private
    FText : string; //raw text as read from stream or file; null string if EOL
    FTokenClass : TTokenClass;
    FLineNumber : longint;
    // FLocation : TLocation;
    FTag : TTag;
    procedure SetText(const Value: string);

  public
    property Text : string read FText write SetText;
    property TokenClass : TTokenClass read FTokenClass write FTokenClass;
    property LineNumber : longint read FLineNumber write FLineNumber;
    // property Location : TLocation read FLocation write FLocation;
    property Tag : TTag read FTag;

    constructor Create;
    destructor Destroy; override;

  end;

type
  TTokenList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TToken;
    procedure SetItems(Index: Integer; AToken : TToken );
  public

    function Add(AToken: TToken): Integer;
    function Remove(AToken: TToken): Integer;
    function IndexOf(AToken: TToken): Integer;
    procedure Insert(Index: Integer; AToken: TToken);
    property Items[Index: Integer]: TToken read GetItems write SetItems; default;
  end;


implementation

{ TToken }

constructor TToken.Create;
begin
  inherited Create;
  FTag := TTag.Create( true ); // [x] set AutoParse to true?
  FText := '';
  // FLocation := locMETA;
  FTokenClass := tcUndef;
  FLineNumber := 0;
end; // Create

destructor TToken.Destroy;
begin
  FTag.Free;
  inherited Destroy;
end; // Destroy


procedure TToken.SetText(const Value: string);
begin
  FText := Value;
  if ( FTokenClass = tcTag ) then
  begin
    FTag.Text := FText; // tag will parse itself
  end;
end;

{ TTokenList }

function TTokenList.Add(AToken: TToken): Integer;
begin
  Result := inherited Add(AToken);
end;

function TTokenList.GetItems(Index: Integer): TToken;
begin
  Result := TToken(inherited Items[Index]);
end;

function TTokenList.IndexOf(AToken: TToken): Integer;
begin
  Result := inherited IndexOf(AToken);
end;

procedure TTokenList.Insert(Index: Integer; AToken: TToken);
begin
  inherited Insert(Index, AToken );
end;

function TTokenList.Remove(AToken: TToken): Integer;
begin
  Result := inherited Remove(AToken);
end;

procedure TTokenList.SetItems(Index: Integer; AToken: TToken);
begin
  inherited Items[Index] := AToken;
end;

end.
