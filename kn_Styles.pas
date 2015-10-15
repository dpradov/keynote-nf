unit kn_Styles;

(****** LICENSE INFORMATION **************************************************
 
 - This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.           
 
------------------------------------------------------------------------------
 (c) 2000-2005 Marek Jedlinski <marek@tranglos.com> (Poland)
 
 Fore more information, please see 'README.md' and 'doc/README_SourceCode.txt'
 in https://github.com/dpradov/keynote-nf      
   
 *****************************************************************************) 


interface

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls,
  Forms, Dialogs, RxRichEd,
  kn_Const, kn_Info,
  kn_StyleObj;



type
  TForm_Style = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;



var
  Form_Style: TForm_Style;

  
implementation

{$R *.DFM}

end.
