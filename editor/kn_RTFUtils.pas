unit kn_RTFUtils;

(****** LICENSE INFORMATION **************************************************

 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
 (c) 2007-2023 Daniel Prado Velasco <dprado.keynote@gmail.com> (Spain) [^2]
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland) [^1]

 [^1]: Based on "CR_UORichEdit.pas";
       Enhanced TRichEdit support for DelphiDrop by UnitOOPS SOftware.
       Based on code examples by Peter Below and Ralph Friedman (TeamB).

 [^2]: Changes since v. 1.7.0. Fore more information, please see 'README.md'
     and 'doc/README_SourceCode.txt' in https://github.com/dpradov/keynote-nf

 *****************************************************************************)


interface

uses
   Winapi.Windows,
   System.Classes,
   System.SysUtils,
   System.StrUtils,
   Vcl.Graphics,
   RxRichEd,
   kn_NoteObj;

{
 Removed the following procedures and function. Moved (adapted) as methods of TRxRichEdit
procedure PutRichText  (const aRTFString: string;const aRichEdit: TRxRichEdit;const isRTF, DoInsert: boolean);
  -> Replaced by: aRichEdit.PutRtfText (aRTFString, DoInsert, SelectionOnly, KeepSelected)    where param aRTFString: string

procedure PutRichTextW (const aRTFString: WideString;const aRichEdit: TRxRichEdit;const isRTF, DoInsert: boolean);
  -> Replaced by: aRichEdit.PutRtfText (aRTFString, DoInsert, SelectionOnly, KeepSelected)    where param aRTFString: RawByteString

procedure AnsiPutRichText (const aRTFString: string; const aRichEdit: TRxRichEdit; const isRTF, DoInsert: boolean);
  -> Replaced by: 
        aRichEdit.PutRtfText (aRTFString, DoInsert)

function GetRichText   (const aRichEdit : TRxRichEdit; const asRTF, selectionOnly : boolean) : string;
  -> Replaced by: 
       aRichEdit.GetRtfText
       aRichEdit.GetRtfSelText


Also available:
       aRichEdit.RtfText    -> GetRtfText / SetRtfText
       aRichEdit.SelRtfText -> GetRtfSelText / SetRtfSelText

Note: If RichEditVersion < 3, then aRichEdit.PutRtfText will do RtfSelText:= aRTFString (taking into account also the other params )



Removed their auxiliar methods:

function EMStreamInCallback(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall; // Callback function for EM_STREAMIN handling.

function EMStreamOutCallback(dwCookie: Longint; pbBuff: PByte; cb: Longint; var pcb: Longint): Longint; stdcall; // Callback function for EM_STREAMOUT handling.
}
function URLToRTF (fn : string; enTextoURL: boolean ) : string;
function URLFromRTF (fn : string ) : string;

function TextToUseInRTF (const UnicodeText : string ): string;
function ScapeSpecialRTFCharacters (const str : string ): string;
function GetRTFColor (Color: TColor): string;

function CleanRTF(const RTF: string; var nRTF: string): boolean;
procedure SetDefaultFontAndSizeInRTF(var RTFText: AnsiString; TextAttrib: TRxTextAttributes = nil);

function GetAuxEditorControl (EditorToLoadFrom: TTabRichEdit= nil; FromSelection: Boolean= True): TTabRichEdit;
procedure FreeAuxEditorControl (FlushToEditor: TTabRichEdit= nil; DoInsert: boolean= true; FreeAndNilControl: boolean= false);


implementation
uses
   kn_Main;

var
   RTFAux : TTabRichEdit;



// To use the filename in {\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink}}}
// we must convert each '\' to four '\' or to '/'. Example: D:\kk.txt -> D:\\\\kk.txt or D:/kk.txt
// In "hyperlink" \\192.168.0.1\folder\leeme.txt -> "\\\\\\\\192.168.0.1/folder/leeme.txt" or "\\\\\\\\192.168.0.1\\folder\\leeme.txt"
// In textOfHyperlink \\192.168.0.1\folder\leeme.txt -> "\\\\192.168.0.1\\folder\\leeme.txt"
function URLToRTF (fn: string; enTextoURL: boolean ): string;
begin

  if enTextoURL then begin
     result:= StringReplace(fn,'\','\\', [rfReplaceAll]);
  end
  else begin
     result:= StringReplace(fn,'\\',chr(1), [rfReplaceAll]);
     result:= StringReplace(result,'\','\\', [rfReplaceAll]);
     result:= StringReplace(result,chr(1),'\\\\\\\\', [rfReplaceAll]);
  end;

end;


function URLFromRTF (fn: string ): string;
begin
   result:= StringReplace(fn,'\\\\',chr(1), [rfReplaceAll]);
   result:= StringReplace(result,'\\','\', [rfReplaceAll]);
   result:= StringReplace(result, chr(1), '\\\\', [rfReplaceAll]);
end;

//-----------------------



// Returns a text in ASCII with characters with 8 bits escaped with \' and unicode characters
// escaped with \u. Tab character is also converted to \tab
//  Examples: "có" --> "c\'f3"  "Việt" --> "Vi\u7879?t"
//
//  References: Special characters in RTF: http://www.biblioscape.com/rtf15_spec.htm#Heading46
//             Unicode characters in RTF: http://www.biblioscape.com/rtf15_spec.htm#Heading9
//  Delphi: http://www.delphibasics.co.uk/RTL.asp?Name=Char
//          http://www.delphibasics.co.uk/ByFunction.asp?Main=Strings

function TextToUseInRTF (const UnicodeText : string ): string;
var
  i, val: Integer;
  str: string;
begin
  Result:= '';

  str:= ScapeSpecialRTFCharacters(UnicodeText);

  for i := 1 to length(str) do begin
    val:= Ord(str[i]);
    if val = 9 then
       Result := Result + '\tab '

    else if (val = 13) then
       Result := Result + '\par '

    else if (val = 10) then
        // do nothing

    else if val >= 127 then
       if val <= 255 then
           Result := Result + '\''' + IntToHex(val, 2)
       else
           Result := Result + '\u' + IntToStr(val) + '?'
    else
       Result := Result + Char(str[i]);
  end;
end;

function ScapeSpecialRTFCharacters (const str : string ): string;
var
  p1, p2, p3: integer;
begin
  p1:= Pos('\', str, 1);
  p2:= Pos('{', str, 1);
  p3:= Pos('}', str, 1);
  Result:= str;

  if (p1 > 0) or (p2 > 0) or (p3 > 0) then begin
    if p1 > 0 then
       Result:= StringReplace(Result,'\','\\', [rfReplaceAll]);
    if p2 > 0 then
       Result:= StringReplace(Result,'{','\{', [rfReplaceAll]);
    if p3 > 0 then
       Result:= StringReplace(Result,'}','\}', [rfReplaceAll]);
  end;
end;




function GetRTFColor (Color: TColor): string;
var
  R, G, B: byte;
begin
  R := GetRValue (Color); {red}
  G := GetGValue (Color); {green}
  B := GetBValue (Color); {blue}
  Result:= '\red' + IntToStr(R) + '\green' + IntToStr(G) + '\blue' + IntToStr(B) + ';';
end;



(*
This function resolve the problem described in issues #59 and #524:
   https://github.com/dpradov/keynote-nf/issues/59

#59: Certain hyperlinks may cause knt file to grow in size suddenly (geometrical increase..)
----------
{\field{\*\fldinst{HYPERLINK "hyperlink"
\\\\\\\\t "_blank" }}{\fldrslt{\cf2\lang255\ul textOfHyperlink}}}

Correct, "clean" hyperlink:
{\field{\*\fldinst{HYPERLINK "hyperlink"}}{\fldrslt{\cf1\ul textOfHyperlink}}}

The "_blank" (can be any string) is inoffensive. But the frament \\\\\\\\x
gives too much problem with RichText control. Each time that RTF is readen by
the a RichText control that string is duplicated:
the \\\\\\\\t  is converted to \\\\\\\\\\\\\\\\t, then to
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\t,  etc).

If you don't save the file doesn't matter but if you save the file then
the duplicated strings are saved, and the file can blow out.

#524 : Problem with MS Sans Serif in Windows 10 in certain situations
----
In some ocassions that the generated code is not properly managed. Example:
Bad: {\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang3082{\fonttbl{\f0\fnil MS Sans Serif;}} .....
Ok: {\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang3082{\fonttbl{\f0\fnil\fcharset0 MS Sans Serif;}} .....
In Windows XP and in Windows 7 (at least) this is not a problem, because the
RichEdit control manage both rtf code correctly.
For new nodes the problem is avoided using a default font different to MS Sans Serif.
But for existing nodes with that problem (not occurs in all) the solution is
to replace "\f0\fnil " with "\f0\fnil\fcharset0 ".
*)

function CleanRTF (const RTF: string; var nRTF: string): boolean;
var
  p, p2, pHR : integer;
begin
    Result:= false;

    if ( pos('\f0\fnil ', RTF) > 0 ) then begin
        nRTF:= ReplaceStr(RTF, '\f0\fnil ', '\f0\fnil\fcharset0 ');
        Result:= true;
    end;

    if ( pos('\\\\', RTF) <= 0 ) then exit;

    if not Result then
       nRTF:= RTF;

    p:= 1;
    repeat
       p:= posEx('\fldinst{HYPERLINK', nRTF, p);  // (length "\fldinst{HYPERLINK": 18)
       if ( p > 0 ) then begin
          Inc(p, 19);
          if (nRTF[p] <> '"') then begin
             // RichEdit versions >=5 allow only one space between HYPERLINK and "
             p2:= p;
             while (p <= Length(nRTF)) and (AnsiChar(nRTF[p]) In [' ',#13,#10]) do
                Inc(p);
             if p > Length(nRTF) then exit;
             if (nRTF[p] = '"') then begin
                nRTF[p2-1]:= ' ';
                delete(nRTF, p2, p-p2);
                Result:= true;
             end
             else
                continue;
          end;
          pHR:= posEx('"', nRTF, p+1);
          if pHR <= 0 then continue;
          if nRTF[pHR+1] <> '}' then begin
             p2:= posEx('}', nRTF, pHR+1);
             if p2 > 0 then begin
                delete(nRTF, pHR+1, p2-pHR-1);
                Result:= true;
             end;
             p:= pHR+1;
          end;
       end;

    until p <= 0;

end;

procedure SetDefaultFontAndSizeInRTF(var RTFText: AnsiString; TextAttrib: TRxTextAttributes = nil);
var
  Flags: TReplaceFlags;
  fSize: string;
begin
   if (RTFText = '') or (TextAttrib  = nil) then
      exit;

  { If received TRxTextAttributes, it will use the font and size (normally it will correspond to the current selection
    in active Editor) to define the default font and size once the conversion from HTML to RTF has finisthed.
    This values seems to be set always to Times New Roman 12 during the conversion

    Notes:
     - The command \fsN changes the font size to N half-points—not points, half-points! So \fs24 means 12-point type,
     - We are setting the font size once defined the table of fonts with \fonttbl,and set the zeroeth font as the default
       with \deff0, just before {\colortbl\
    - \plain resets the character formatting: it turns off all characteristics—italics, bold, smallcaps, superscript,
       and so on. Things that can’t meaningfully be turned off, like point-size, font number, or language are reset to their
       default values.
       The RTF specification seems to say that \plain should reset the current font size to 12-point, but some versions of
       MSWord reset it to 10-point. To be sure the point size resets to what you intend, explicitly set it after every \plain,
  }

  fSize:= 'fs'+(TextAttrib.Size*2).ToString;
  RTFText:= StringReplace(RTFText, '{\fonttbl{\f0\froman\fcharset0 Times New Roman;}',
                                   '{\fonttbl{\f0\froman\fcharset0 ' + TextAttrib.Name + ';}', Flags);

  RTFText:= StringReplace(RTFText, '{\colortbl\',  '\f0\' + fSize + ' {\colortbl\', Flags);
  RTFText:= StringReplace(RTFText, '\plain\fs24',  '\plain\'+ fSize, [rfReplaceAll]);

end;


function GetAuxEditorControl(EditorToLoadFrom: TTabRichEdit= nil; FromSelection: Boolean= True): TTabRichEdit;
var
  Stream: TStream;
  Str: String;
begin

   if not assigned(RTFAux) then begin
      RTFAux := TTabRichEdit.Create(Form_Main);
      RTFAux.Visible:= False;
      RTFAux.OnProtectChangeEx:= Form_Main.RxRTFAuxiliarProtectChangeEx;
      RTFAux.Parent:= Form_Main;
   end;

   RTFAux.Clear;
   //RTFAux.WordWrap:= false;          // It's not necessary and its causing TTabRichEdit.CMRecreateWnd to be called
   RTFAux.StreamMode := [];
   RTFAux.StreamFormat := sfRichText;

   if assigned(EditorToLoadFrom) then begin
      if FromSelection then
         Str:= EditorToLoadFrom.RtfSelText
      else
         Str:= EditorToLoadFrom.RtfText;

      Stream:= TStringStream.Create(Str);
      try
         RTFAux.Lines.LoadFromStream(Stream);
      finally
         Stream.Free;
      end;
   end;
   Result:= RTFAux;
end;

procedure FreeAuxEditorControl(FlushToEditor: TTabRichEdit= nil;
                               DoInsert: boolean= true;
                               FreeAndNilControl: boolean= false);
var
  Str: String;
begin
   if assigned(RTFAux) then begin
      if assigned(FlushToEditor) then begin
         Str:= RTFAux.RtfText;
         FlushToEditor.PutRtfText(Str, DoInsert);
      end;

      RTFAux.Clear;
      if FreeAndNilControl then
         FreeAndNil(RTFAux);
   end;
end;

end.
