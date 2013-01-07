unit gf_floatform;
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TFloatForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure CreateParams( var Params : TCreateParams ); override;

  public
    { Public declarations }
  end;

const
  SC_MyMenuItem = WM_USER + 1;

implementation

{$R *.DFM}

procedure TFloatForm.CreateParams( var Params : TCreateParams );
begin
  inherited CreateParams( Params );
  // Params.ExStyle := Params.exstyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;

  params.exstyle := params.exstyle or WS_EX_TOOLWINDOW;
  params.style := params.style or WS_POPUP;
end; // CreateParams

procedure TFloatForm.WMSysCommand(var Msg: TWMSysCommand);
begin
  if Msg.CmdType = SC_MyMenuItem then
    ShowMessage('Got the message') else
    inherited;
end; // WMSysCommand

procedure TFloatForm.FormCreate(Sender: TObject);
begin
  ClientWidth := 100;
  ClientHeight := 50;
end; // CREATE

procedure TFloatForm.FormActivate(Sender: TObject);
begin
  OnActivate := nil;

  {
  AppendMenu(GetSystemMenu(Handle, FALSE), MF_SEPARATOR, 0, '');
  AppendMenu(GetSystemMenu(Handle, FALSE),
             MF_STRING,
             SC_MyMenuItem,
             'New Menu Item');
  }

  {
  ShowWindow( handle, SW_HIDE );
  SetWindowLong( handle,
                 GWL_EXSTYLE,
                 GetWindowLong( handle, GWL_EXSTYLE ));
  }
  // ShowWindow( GetDesktopWindow, SW_HIDE );



end; // ACTIVATE

end.
