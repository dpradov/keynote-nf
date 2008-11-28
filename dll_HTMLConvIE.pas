unit dll_HTMLConvIE;
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
 <marekjed@users.sourceforge.net>

************************************************************ *)

interface
uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  OleCtrls,
  //SHDocVw_TLB, EmbeddedWB,      //*1
  SHDocVw,
  ActiveX, StdCtrls, RxRichEd, ExtCtrls,
  Clipbrd;


type
  TForm_HtmlConvIE = class(TForm)
    RTF: TRxRichEdit;
    IE: TWebBrowser;
    Panel1: TPanel;
    Button_ConvertRTF: TButton;
    Splitter1: TSplitter;
    Button1: TButton;
    Button_ConvertText: TButton;
    procedure FormActivate(Sender: TObject);
    procedure Button_ConvertRTFClick(Sender: TObject);
    procedure Button_ConvertTextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    inFN, outFN : string;
    procedure DoConvert( const AsRTF : boolean );
  end;

implementation

{$R *.DFM}

procedure TForm_HtmlConvIE.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  IE.Navigate( inFN ); // [x] what happens when this fails?
end;

procedure TForm_HtmlConvIE.Button_ConvertRTFClick(Sender: TObject);
begin
  DoConvert( true );
end;

procedure TForm_HtmlConvIE.DoConvert( const AsRTF : boolean );
var
  myList : TStringList;
begin
  IE.ExecWB( OLECMDID_SELECTALL, OLECMDEXECOPT_DODEFAULT );
  IE.ExecWB( OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT );
  if AsRTF then
  begin
    RTF.PasteFromClipboard;
    RTF.Lines.SaveToFile( outFN );
  end
  else
  begin
    myList := TStringList.Create;
    try
      myList.Text := Clipboard.AsText;
      myList.SaveToFile( outFN );
    finally
      myList.Free;
    end;
  end;

  ModalResult := mrOK;
end; // DoConvert

procedure TForm_HtmlConvIE.Button_ConvertTextClick(Sender: TObject);
begin
  DoConvert( false );
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;

end.
