unit gf_HTMLTags;

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
uses Classes, SysUtils, gf_HTMLBase, contnrs;

type
  TElementSet = set of TElement;
  TAttributeSet = set of TAttribute;

const
  _UnpairedElements : TElementSet = [
    elemBASE, elemBASEFONT, elemBGSOUND, elemBR,
    elemCOLGROUP, elemFRAME, elemHR,
    elemIMG, elemINPUT, elemISINDEX, elemKEYGEN,
    elemLIMITTEXT, elemLINK, elemMETA, elemPARAM,
    elemSPACER, elemWBR
  ];

type
  { Basic ATTRIBUTE="value" pair }
  TTagAttribute = class( TObject )
  private
    FText : string;
    FName : string;
    FValue : string;
    FAttribute : TAttribute;
    FAutoParse : boolean;

    procedure Parse;
    procedure SetName(const Value: string);
    procedure SetValue(const Value: string);
    procedure SetText(const Value: string);
    procedure SetAttribute(const Value: TAttribute);
    function GetText: string;

  public
    property Text : string read GetText write SetText;
    property Name : string read FName write SetName;
    property Value : string read FValue write SetValue;
    property Attribute : TAttribute read FAttribute write SetAttribute;
    property AutoParse : boolean read FAutoParse write FAutoParse;

    constructor Create( const aAutoParse : boolean );
    constructor CreateFrom( const aAutoParse : boolean; const aText : string );

  end;

type
  TAttributeList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TTagAttribute;
    procedure SetItems(Index: Integer; ATagAttribute : TTagAttribute );
  public

    function Add(ATagAttribute: TTagAttribute): Integer;
    function Remove(ATagAttribute: TTagAttribute): Integer;
    function IndexOf(ATagAttribute: TTagAttribute): Integer;
    procedure Insert(Index: Integer; ATagAttribute: TTagAttribute);
    property Items[Index: Integer]: TTagAttribute read GetItems write SetItems; default;
  end;


type
  TTag = class( TObject )
  private
    FText : string;
    FElement : TElement;
    FElementName : string;
    FAttributeList : TAttributeList;
    FIsStartTag : boolean;
    FAutoParse : boolean;

    procedure Parse;
    procedure SetElement(const Value: TElement);
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetElementName(const Value: string);
    { need procedures:
      - for adding an attribute pair
        (or update attribute with new value, if attribute already exists)
      - for checking if an attribute exists
    }

  public
    property Text : string read GetText write SetText;
    property Element : TElement read FElement write SetElement;
    property AttributeList : TAttributeList read FAttributeList;
    property IsStartTag : boolean read FIsStartTag;
    property AutoParse : boolean read FAutoParse write FAutoParse;
    property ElementName : string read FElementName write SetElementName;

    constructor Create( const aAutoParse : boolean );
    constructor CreateFrom( const aAutoParse : boolean; const aText : string );
    destructor Destroy; override;

    function IndexOfAttr( const A : TAttribute ) : integer;
    function GetAttrValue( const A : TAttribute ) : string;
    procedure SetAttrValue( const A : TAttribute; const aValue : string );

  end;


implementation



{ TTagAttribute }

constructor TTagAttribute.Create(const aAutoParse: boolean);
begin
  inherited Create;
  FAutoParse := aAutoParse;
end; // CREATE;

constructor TTagAttribute.CreateFrom( const aAutoParse : boolean; const aText: string );
begin
  inherited Create;
  FAutoParse := aAutoParse;
  SetText( aText );
end; // CreateFrom

function TTagAttribute.GetText: string;
begin
  if ( FValue = '' ) then
    result := FName
  else
    // [x] value does not HAVE to be always quoted,
    // but it doesn't hurt if it is.
    result := Format(
      '%s="%s"',
      [FName,FValue]
    );
end; // GetText

procedure TTagAttribute.Parse;
var
  p, l : integer;
begin
  p := pos( '=', FText );
  if ( p = 0 ) then
  begin
    FName := FText;
    FValue := '';
  end
  else
  begin
    FName := copy( FText, 1, pred( p ));
    delete( FText, 1, p );
    l := length( FText );
    if ( l > 1 ) then
    begin
      if (( FText[1] = '"' ) and ( FText[l] = '"' )) or
         (( FText[1] = '''' ) and ( FText[l] = '''' )) then
      begin
        delete( FText, l, 1 );
        delete( FText, 1, 1 );
      end;
    end;
    FValue := FText;
  end;
  FAttribute := AttributeFromName( FName );
end; // Parse

procedure TTagAttribute.SetAttribute(const Value: TAttribute);
begin
  FAttribute := Value;
  FName := ATTRIBUTE_NAMES[FAttribute];
end; // SetAttribute

procedure TTagAttribute.SetName(const Value: string);
begin
  FName := Value;
  FAttribute := AttributeFromName( FName );
end; // SetName

procedure TTagAttribute.SetText(const Value: string);
begin
  FText := trim( Value );
  if FAutoParse then
    Parse;
end; // SetText

procedure TTagAttribute.SetValue(const Value: string);
begin
  FValue := Value;
end; // SetValue

{ TAttributeList }

function TAttributeList.Add(ATagAttribute: TTagAttribute): Integer;
begin
  Result := inherited Add(ATagAttribute);
end;

function TAttributeList.GetItems(Index: Integer): TTagAttribute;
begin
  Result := TTagAttribute(inherited Items[Index]);
end;

function TAttributeList.IndexOf(ATagAttribute: TTagAttribute): Integer;
begin
  Result := inherited IndexOf(ATagAttribute);
end;

procedure TAttributeList.Insert(Index: Integer;
  ATagAttribute: TTagAttribute);
begin
  inherited Insert(Index, ATagAttribute);
end;

function TAttributeList.Remove(ATagAttribute: TTagAttribute): Integer;
begin
  Result := inherited Remove(ATagAttribute);
end;

procedure TAttributeList.SetItems(Index: Integer;
  ATagAttribute: TTagAttribute);
begin
  inherited Items[Index] := ATagAttribute;
end;

{ TTag }

constructor TTag.Create( const aAutoParse : boolean );
begin
  inherited Create;
  FAutoParse := aAutoParse;
  FElementName := '';
  FElement := elemUNKNOWN;
  FAttributeList := TAttributeList.Create;
end; // Create

constructor TTag.CreateFrom(const aAutoParse : boolean; const aText: string);
begin
  inherited Create;
  FAttributeList := TAttributeList.Create;
  FAutoParse := aAutoParse;
  FElementName := '';
  FElement := elemUNKNOWN;
  SetText( aText );
end; // CreateFrom

destructor TTag.Destroy;
begin
  FAttributeList.Free;
  inherited Destroy;
end; // Create

function TTag.GetAttrValue(const A: TAttribute): string;
var
  index : integer;
begin
  result := '';
  index := IndexOfAttr( A );
  if ( index >= 0 ) then
    result := FAttributeList[index].Value;
end; // GetAttrValue

procedure TTag.SetAttrValue(const A : TAttribute; const aValue : string);
var
  index : integer;
begin
  index := IndexOfAttr( A );
  if ( index < 0 ) then
  begin
    index := FAttributeList.Add( TTagAttribute.Create( false ));
  end;
  with FAttributeList[index] do
  begin
    Attribute := A;
    Value := aValue;
  end;
end; // SetAttrValue

function TTag.IndexOfAttr(const A: TAttribute): integer;
var
  i : integer;
begin
  result := -1;
  for i := 1 to FAttributeList.Count do
  begin
    if ( FAttributeList[pred( i )].Attribute = A ) then
    begin
      result := pred( i );
      break;
    end;
  end;
end; // IndexOfAttr

function TTag.GetText: string;
var
  i, count : integer;
  name : string;
begin
  if ( FElement <> elemUNKNOWN ) then
    name := ELEMENT_NAMES[FElement]
  else
    name := FElementName;

  if FIsStartTag then
  begin
    count := FAttributeList.Count;
    if ( count = 0 ) then
    begin
      result := Format(
        '<%s>',
        [name]
      );
    end
    else
    begin
      result := Format(
        '<%s ',
        [name]
      );
      for i := 1 to count do
        result := result + FAttributeList[pred( i )].Text;
      result := result + '>';
    end;
  end
  else
  begin
    // closing tag
    result := Format(
      '</%s>',
      [name]
    );
  end;
end; // GetText

procedure TTag.Parse;
var
  p, i, len : integer;
  ch, QuoteChar, PrevChar : char;
  InQuotes, ValueFollows : boolean;
  Attr : TTagAttribute;
  t, s, astr : string;
begin
{
    [x] BUGS:

1.  <FONT FACE="times new roman"color="white">
    this is NOT legal, but parser should still be able to
    figure out where the first attribute ends
2.  Nested quotes; not sure if this is legal:
    <a href="one 'two "three" four' five">
    yields:
    attr: href value: one 'two three four' five
    (internal double quotes lost, attribute value could now be invalid)

}

  FElement := elemUNKNOWN;
  len := length( FText );

  if( len < 3 ) then
  begin
    // [x]
    exit;
  end;

  FIsStartTag := ( FText[2] <> '/' );

  t := FText;

  // delete leading '<' and possibly the '/' marker,
  // but eave the trailing '>', because it will help
  // us when parsing attributes
  delete( t, 1, 1 );
  if ( not FIsStartTag ) then
    delete( t, 1, 1 );

  p := pos( #32, t );
  if ( p = 0 ) then
  begin
    // no attributes, just the element
    delete( t, length( t ), 1 );
    FElementName := t;
    FElement := ElementFromName( FElementName );
    exit;
  end;

  FElementName := copy( t, 1, pred( p ));
  FElement := ElementFromName( FElementName );
  delete( t, 1, p );
  len := length( t );
  s := '';
  astr := '';
  QuoteChar := #0;
  PrevChar := #0;
  InQuotes := false;
  ValueFollows := false;

  // if we arrive here, the tag must have attributes

  for i := 1 to len do
  begin
    ch := t[i];
    case ch of

      #9, #32, '>' : begin
        if InQuotes then
        begin
          s := s + ch;
          ValueFollows := false;
        end
        else
        begin
          if ( PrevChar = '=' ) then
          begin
            ch := '=';
          end
          else
          begin
            if ( ch <> '>' ) then
            begin
              // look ahead for '=' sign
              p := succ( i );
              while ( p < len ) do
              begin
                case t[p] of
                  '=' : begin
                    // will have Value string
                    ValueFollows := true;
                    break;
                  end;
                  #9, #32 : begin
                    // skip
                  end;
                  else
                  begin
                    // will NOT have Value string
                    break;
                  end;
                end;
                inc( p );
              end;
            end;
          end;

          if ( not ValueFollows ) then
          begin
            if ( aStr <> '' ) then
            begin
              Attr := TTagAttribute.Create( false );
              Attr.Name := trim( aStr );
              Attr.Value := s;
              FAttributeList.Add( Attr );
            end
            else
            begin
              if ( s <> '' ) then
              begin
                Attr := TTagAttribute.Create( false );
                Attr.Name := trim( s );
                FAttributeList.Add( Attr );
              end;
            end;
            s := '';
            astr := '';
          end;
        end;
      end;

      '"', '''' : begin
        ValueFollows := false;
        if InQuotes then
        begin
          if ( QuoteChar = ch ) then
            InQuotes := false
          else
            s := s + ch;
        end
        else
        begin
          InQuotes := true;
          QuoteChar := ch;
        end;
      end;

      '=' : begin
        if InQuotes then
        begin
          s := s + ch;
        end
        else
        begin
          aStr := s;
          s := '';
          ValueFollows := true;
        end;
      end;

      else
      begin
        s := s + ch;
        ValueFollows := false;
      end;

    end; // case

    PrevChar := ch;

  end;

end; // Parse


procedure TTag.SetElement(const Value: TElement);
begin
  FElement := Value;
  FElementName := ELEMENT_NAMES[FElement];
end; // SetElement

procedure TTag.SetElementName(const Value: string);
begin
  FElementName := Value;
  FElement := ElementFromName( FElementName );
end;

procedure TTag.SetText(const Value: string);
begin
  FText := Value;
  FIsStartTag := ( pos( '/', FText ) <> 2 );
  if FAutoParse then
    Parse;
end; // SetText;


end.
