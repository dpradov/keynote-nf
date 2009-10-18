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

 The Original Code is "gf_HTMLBase.pas".

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

unit gf_HTMLBase;
{$I gf_base.inc}

interface
uses Sysutils;

const
  UNKNOWN_ELEMENT  = '';
  UNKNOWN_ATTRIBUTE = '';

type
  TElement = (
    elemA, // [-]
    elemABBR, // [?]
    elemACRONYM, // [?]
    elemADDRESS, // [?]
    elemAPPLET,
    elemAREA,
    elemB, // [+]
    elemBASE, // [?]
    elemBASEFONT, // [-]
    elemBDO,
    elemBGSOUND,
    elemBIG, // [+]
    elemBLACKFACE, // [-]
    elemBLINK, // [?]
    elemBLOCKQUOTE, // [?]
    elemBODY, // [-]
    elemBQ, // [-]
    elemBR, // [+]
    elemBUTTON, // [?]
    elemCAPTION, // [-]
    elemCENTER, // [-]
    elemCITE, // [-]
    elemCODE, // [-]
    elemCOL,
    elemCOLGROUP,
    elemCOMMENT, // [-]
    elemDD, // [-]
    elemDEL, // [-] same as strikethrough
    elemDFN, // [-]
    elemDIR, // [-]
    elemDIV, // [-]
    elemDL, // [-]
    elemDT, // [-]
    elemEM, // [-] same as italics
    elemEMBED,
    elemFIELDSET,
    elemFN, // [?]
    elemFONT, // [-]
    elemFORM,
    elemFRAME,
    elemFRAMESET,
    elemH1, // [-]
    elemH2, // [-]
    elemH3, // [-]
    elemH4, // [-]
    elemH5, // [-]
    elemH6, // [-]
    elemHEAD,
    elemHR, // [-]
    elemHTML,
    elemI, // [+]
    elemIFRAME,
    elemILAYER,
    elemIMG, // [-]
    elemINPUT, // [?]
    elemINS, // [-]
    elemISINDEX,
    elemKBD, // [-]
    elemKEYGEN,
    elemLABEL,
    elemLAYER,
    elemLEGEND,
    elemLI, // [-]
    elemLIMITTEXT,
    elemLINK, // [?]
    elemLISTING, // [-]
    elemMAP, // [-]
    elemMARQUEE,
    elemMENU,
    elemMETA, // [-]
    elemMULTICOL,
    elemNOBR, // [-]
    elemNOEMBED,
    elemNOFRAMES,
    elemNOLAYER,
    elemNOSCRIPT,
    elemNOSMARTQUOTES,
    elemOBJECT,
    elemOL, // [-]
    elemOPTGROUP,
    elemOPTION,
    elemP, // [+]
    elemPARAM,
    elemPLAINTEXT, // [-]
    elemPRE, // [-]
    elemQ, // [-]
    elemS, // [-]
    elemSAMP, // [-]
    elemSCRIPT, // [-]
    elemSELECT,
    elemSERVER,
    elemSHADOW,
    elemSIDEBAR,
    elemSMALL, // [+]
    elemSPACER, // [-]
    elemSPAN,
    elemSTRIKE, // [+]
    elemSTRONG, // [-] same as bold
    elemSTYLE,
    elemSUB, // [+]
    elemSUP, // [+]
    elemTABLE, // [-]
    elemTBODY,
    elemTD, // [-]
    elemTEXTAREA,
    elemTFOOT,
    elemTH, // [-]
    elemTHEAD, // [-]
    elemTITLE,
    elemTR, // [-]
    elemTT, // [-]
    elemU, // [+]
    elemUL, // [-]
    elemVAR,
    elemWBR,
    elemXMP, // [-]
    elemUNKNOWN
  );

type
  TAttribute = (
    attrABBR,
    attrACCEPTCHARSET,
    attrACCEPT,
    attrACCESSKEY,
    attrACTION,
    attrADDDATE,
    attrALIGN,
    attrALINK,
    attrALT,
    attrARCHIVE,
    attrAXIS,
    attrBACKGROUND,
    attrBGCOLOR,
    attrBORDER,
    attrCELLPADDING,
    attrCELLSPACING,
    attrCHAROFF,
    attrCHARSET,
    attrCHECKED,
    attrCITE,
    attrCLASS,
    attrCLASSID,
    attrCLEAR,
    attrCODE,
    attrCODEBASE,
    attrCODETYPE,
    attrCOLOR,
    attrCOLS,
    attrCOLSPAN,
    attrCOMPACT,
    attrCONTENT,
    attrCOORDS,
    attrDATA,
    attrDATETIME,
    attrDECLARE,
    attrDEFER,
    attrDIR,
    attrDISABLED,
    attrENCTYPE,
    attrFACE,
    attrFOR,
    attrFRAME,
    attrFRAMEBORDER,
    attrHEADERS,
    attrHEIGHT,
    attrHREF,
    attrHREFLANG,
    attrHSPACE,
    attrHTTPEQUIV,
    attrID,
    attrISMAP,
    attrLABEL,
    attrLANG,
    attrLANGUAGE,
    attrLASTMODIFIED,
    attrLASTVISIT,
    attrLEFTMARGIN,
    attrLINK,
    attrLONGDESC,
    attrMARGINHEIGHT,
    attrMARGINWIDTH,
    attrMAXLENGTH,
    attrMEDIA,
    attrMETHOD,
    attrMULTIPLE,
    attrNAME,
    attrNOHREF,
    attrNORESIZE,
    attrNOSHADE,
    attrNOWRAP,
    attrOBJECT,
    attrONBLUR,
    attrONCHANGE,
    attrONCLICK,
    attrONDBLCLICK,
    attrONKEYDOWN,
    attrONKEYPRESS,
    attrONKEYUP,
    attrONLOAD,
    attrONMOUSEDOWN,
    attrONMOUSEMOVE,
    attrONMOUSEOUT,
    attrONMOUSEOVER,
    attrONMOUSEUP,
    attrONRESET,
    attrONSELECT,
    attrONUNLOAD,
    attrPROFILE,
    attrPROMPT,
    attrREADONLY,
    attrREL,
    attrREV,
    attrROWS,
    attrROWSPAN,
    attrRULES,
    attrSCHEME,
    attrSCOPE,
    attrSCROLLING,
    attrSELECTED,
    attrSHAPE,
    attrSIZE,
    attrSPAN,
    attrSRC,
    attrSTANDBY,
    attrSTART,
    attrSTYLE,
    attrSUMMARY,
    attrTABINDEX,
    attrTARGET,
    attrTEXT,
    attrTITLE,
    attrTOPMARGIN,
    attrTYPE,
    attrURL,
    attrUSEMAP,
    attrVALIGN,
    attrVALUE,
    attrVALUETYPE,
    attrVERSION,
    attrVLINK,
    attrVSPACE,
    attrWIDTH,
    attrXMLLANG,
    attrXMLNS,
    attrUNKNOWN
  );


type
  // [x] list is not complete!
  TEntity = (
    entLT, entGT, entNBSP, entAMP, entQUOT, entTILDE,
    entIEXCL, entCENT, entPOUND, entCURREN, entYEN,
    entBRVBAR, entSECT, entUML, entCOPY, entORDF,
    entLAQUO, entNOT, entSHY, entREG, entMACR,
    entDEG, entPLUSMN, entSUP2, entSUP3, entACUTE,
    entMICRO, entPARA, entMIDDOT, entCCEDIL, entSUP1,
    entORDM, entRAQUO, entFRAC14, entFRAC12, entFRAC34,
    entTIMES, entUnknown
  );

const
  ENTITY_NAMES : array[TEntity] of string = (
    'lt', 'gt', 'nbsp', 'amp', 'quot', 'tilde',
    'iexcl', 'cent', 'pound', 'curren', 'yen',
    'brvbar', 'sect', 'uml', 'copy', 'ordf',
    'laquo', 'not', 'shy', 'reg', 'macr',
    'deg', 'plusmn', 'sup2', 'sup3', 'acute',
    'micro', 'para', 'middot', 'ccedil', 'sup1',
    'ordm', 'raquo', 'frac14', 'frac12', 'frac34',
    'times', ''
  );

  ENTITY_CHARS : array[TEntity] of char = (
    '<', '>', ' ', '&', '"', '~',
     #161, #162, #163, #164, #165,
     #166, #167, #168, #169, #170,
     #171, #172, #173, #174, #175,
     #176, #177, #178, #179, #180,
     #181, #182, #183, #184, #185,
     #186, #187, #188, #189, #190,
     #215, #0
  );


const
  ELEMENT_NAMES : array[TElement] of string = (
    'A',
    'ABBR',
    'ACRONYM',
    'ADDRESS',
    'APPLET',
    'AREA',
    'B',
    'BASE',
    'BASEFONT',
    'BDO',
    'BGSOUND',
    'BIG',
    'BLACKFACE',
    'BLINK',
    'BLOCKQUOTE',
    'BODY',
    'BQ',
    'BR',
    'BUTTON',
    'CAPTION',
    'CENTER',
    'CITE',
    'CODE',
    'COL',
    'COLGROUP',
    'COMMENT',
    'DD',
    'DEL',
    'DFN',
    'DIR',
    'DIV',
    'DL',
    'DT',
    'EM',
    'EMBED',
    'FIELDSET',
    'FN',
    'FONT',
    'FORM',
    'FRAME',
    'FRAMESET',
    'H1',
    'H2',
    'H3',
    'H4',
    'H5',
    'H6',
    'HEAD',
    'HR',
    'HTML',
    'I',
    'IFRAME',
    'ILAYER',
    'IMG',
    'INPUT',
    'INS',
    'ISINDEX',
    'KBD',
    'KEYGEN',
    'LABEL',
    'LAYER',
    'LEGEND',
    'LI',
    'LIMITTEXT',
    'LINK',
    'LISTING',
    'MAP',
    'MARQUEE',
    'MENU',
    'META',
    'MULTICOL',
    'NOBR',
    'NOEMBED',
    'NOFRAMES',
    'NOLAYER',
    'NOSCRIPT',
    'NOSMARTQUOTES',
    'OBJECT',
    'OL',
    'OPTGROUP',
    'OPTION',
    'P',
    'PARAM',
    'PLAINTEXT',
    'PRE',
    'Q',
    'S',
    'SAMP',
    'SCRIPT',
    'SELECT',
    'SERVER',
    'SHADOW',
    'SIDEBAR',
    'SMALL',
    'SPACER',
    'SPAN',
    'STRIKE',
    'STRONG',
    'STYLE',
    'SUB',
    'SUP',
    'TABLE',
    'TBODY',
    'TD',
    'TEXTAREA',
    'TFOOT',
    'TH',
    'THEAD',
    'TITLE',
    'TR',
    'TT',
    'U',
    'UL',
    'VAR',
    'WBR',
    'XMP',
    UNKNOWN_ELEMENT
  );

const
  ATTRIBUTE_NAMES : array[TAttribute] of string = (
    'ABBR',
    'ACCEPT-CHARSET',
    'ACCEPT',
    'ACCESSKEY',
    'ACTION',
    'ADDDATE',
    'ALIGN',
    'ALINK',
    'ALT',
    'ARCHIVE',
    'AXIS',
    'BACKGROUND',
    'BGCOLOR',
    'BORDER',
    'CELLPADDING',
    'CELLSPACING',
    'CHAROFF',
    'CHARSET',
    'CHECKED',
    'CITE',
    'CLASS',
    'CLASSID',
    'CLEAR',
    'CODE',
    'CODEBASE',
    'CODETYPE',
    'COLOR',
    'COLS',
    'COLSPAN',
    'COMPACT',
    'CONTENT',
    'COORDS',
    'DATA',
    'DATETIME',
    'DECLARE',
    'DEFER',
    'DIR',
    'DISABLED',
    'ENCTYPE',
    'FACE',
    'FOR',
    'FRAME',
    'FRAMEBORDER',
    'HEADERS',
    'HEIGHT',
    'HREF',
    'HREFLANG',
    'HSPACE',
    'HTTP-EQUIV',
    'ID',
    'ISMAP',
    'LABEL',
    'LANG',
    'LANGUAGE',
    'LASTMODIFIED',
    'LASTVISIT',
    'LEFTMARGIN',
    'LINK',
    'LONGDESC',
    'MARGINHEIGHT',
    'MARGINWIDTH',
    'MAXLENGTH',
    'MEDIA',
    'METHOD',
    'MULTIPLE',
    'NAME',
    'NOHREF',
    'NORESIZE',
    'NOSHADE',
    'NOWRAP',
    'OBJECT',
    'ONBLUR',
    'ONCHANGE',
    'ONCLICK',
    'ONDBLCLICK',
    'ONKEYDOWN',
    'ONKEYPRESS',
    'ONKEYUP',
    'ONLOAD',
    'ONMOUSEDOWN',
    'ONMOUSEMOVE',
    'ONMOUSEOUT',
    'ONMOUSEOVER',
    'ONMOUSEUP',
    'ONRESET',
    'ONSELECT',
    'ONUNLOAD',
    'PROFILE',
    'PROMPT',
    'READONLY',
    'REL',
    'REV',
    'ROWS',
    'ROWSPAN',
    'RULES',
    'SCHEME',
    'SCOPE',
    'SCROLLING',
    'SELECTED',
    'SHAPE',
    'SIZE',
    'SPAN',
    'SRC',
    'STANDBY',
    'START',
    'STYLE',
    'SUMMARY',
    'TABINDEX',
    'TARGET',
    'TEXT',
    'TITLE',
    'TOPMARGIN',
    'TYPE',
    'URL',
    'USEMAP',
    'VALIGN',
    'VALUE',
    'VALUETYPE',
    'VERSION',
    'VLINK',
    'VSPACE',
    'WIDTH',
    'XML-LANG',
    'XMLNS',
    UNKNOWN_ATTRIBUTE
  );


function AttributeFromName( aName : string ) : TAttribute;
function ElementFromName( aName : string ) : TElement;
function EntityFromName( aName : string ) : TEntity;
function DecodeEntity( ent : string ) : string;
function DecodeHTMLEntities( const S : string ) : string;
function EscapeHTMLMetaCharacters( const S : string ) : string;


implementation

function AttributeFromName( aName : string ) : TAttribute;
var
  a : TAttribute;
begin
  result := attrUnknown;
  aName := uppercase( aName );
  for a := low( TAttribute ) to high( TAttribute ) do
  begin
    if ( ATTRIBUTE_NAMES[a] = aName ) then
    begin
      result := a;
      break;
    end;
  end;
end; // AttributeFromName

function ElementFromName( aName : string ) : TElement;
var
  e : TElement;
begin
  result := elemUnknown;
  aName := uppercase( aName );
  for e := low( TElement ) to high( TElement ) do
  begin
    if ( ELEMENT_NAMES[e] = aName ) then
    begin
      result := e;
      break;
    end;
  end;
end; // ElementFromName

function EntityFromName( aName : string ) : TEntity;
var
  e : TEntity;
begin
  result := entUnknown;
  for e := low( TEntity ) to high( TEntity ) do
  begin
    if ( ENTITY_NAMES[e] = aName ) then
    begin
      result := e;
      break;
    end;
  end;
end; // EntityFromName

function DecodeEntity( ent : string ) : string;
var
  code : integer;
  e : TEntity;
begin
  result := '';
  if ( ent <> '' ) then
  begin
    if ( ent[1] = '#' ) then // character code
    begin
      delete( ent, 1, 1 );
      try
        code := strtoint( ent );
        result := chr( code );
      except
        result := '#' + ent; // cannot decode, return original
      end;
    end
    else // named entity
    begin
      e := EntityFromName( ent );
      if ( e <> entUnknown ) then
        result := ENTITY_CHARS[e]
      else
        result := '#' + ent; // cannot decode, return original
    end;
  end;
end; // DecodeEntity


function DecodeHTMLEntities( const S : string ) : string;
var
  InEntity : boolean;
  i, len : integer;
  ch, prevch : char;
  ent : string; // entity


begin
  result := '';
  InEntity := false;
  prevch := #0;
  len := length( S );
  ent := '';

  for i := 1 to len do
  begin
    ch := S[i];
    case ch of
      '&' : begin
        If InEntity then
        begin
          result := result + ch + ent; // flush previous, must not have been en entity
        end;
        InEntity := true; // start new entity
        ent := '';
      end;
      '#' : begin
        If InEntity then
        begin
          if ( prevch = '&' ) then
          begin
            // check for correct syntax
            ent := ent + ch;
          end
          else
          begin
            // not an entity!
            InEntity := false;
            result := result + '&#';
          end;
        end
        else
        begin
          result := result + ch;
        end;
      end;
      ';' : begin
        If InEntity then
        begin
          InEntity := false;
          result := result + DecodeEntity( ent );
          ent := '';
        end
        else
        begin
          result := result + ch;
        end;
      end;
      '0'..'9', 'a'..'z', 'A'..'Z' : begin
        If InEntity then
          ent := ent + ch
        else
          result := result + ch;
      end;
      else
      begin
        result := result + ch;
      end;
    end;

    prevch := ch;
  end;
  result := result + DecodeEntity( ent );

end; // DecodeHTMLEntities

function EscapeHTMLMetaCharacters( const S : string ) : string;
var
  i, len : integer;
  ch : char;
begin
  result := '';
  len := length( S );
  for i := 1 to len do
  begin
    ch := S[i];
    case ch of
      '<' : result := result + '&lt;';
      '>' : result := result + '&gt;';
      '&' : result := result + '&amp;';
      '"' : result := result + '&#34;';
      else
        result := result + ch;
    end;
  end;
end; // EscapeHTMLMetaCharacters


end.
