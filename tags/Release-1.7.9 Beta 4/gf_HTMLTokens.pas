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

 The Original Code is "gf_HTMLTokens.pas".

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

unit gf_HTMLTokens;
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
