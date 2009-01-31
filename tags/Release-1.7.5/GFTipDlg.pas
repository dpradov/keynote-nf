(*
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
<eristic@lodz.pdi.net>, <cicho@polbox.com> (Poland).
Portions created by Marek Jedlinski are
Copyright (C) 2000, 2001. All Rights Reserved.
-----------------------------------------------------------
Contributor(s):
-----------------------------------------------------------
History:
-----------------------------------------------------------
Released: 30 June 2001
-----------------------------------------------------------
*)
unit GFTipDlg;
{$I gf_base.inc}

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs;

type
  TDlgShowPos = ( posScreenCenter, posFormCenter );

type
  TGFTipDlg = class(TComponent)
  private
  FDlgCaption   : string;
  FTipTitle   : string;
  FTips     : TStringList;
  FTipTitleFont : TFont;
  FTipFont    : TFont;
  FTipFile    : string;
  FPanelColor   : TColor;
  FRandomTips   : boolean;
  FShowAtStartup: boolean;
  FPosition   : TDlgShowPos;
  FSelectedTip  : integer;
  FHandle     : THandle;
  protected
  function ReadFromFile : boolean;
  public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure SetTipValues(Value: TStringList);
  procedure SetTitleFont(Value: TFont);
  procedure SetTipFont(Value: TFont);
  function Execute : boolean;
  published
  property DlgCaption: string read FDlgCaption write FDlgCaption;
  property TipTitle: string read FTipTitle write FTipTitle;
  property Tips: TStringList read FTips write SetTipValues;
  property TipTitleFont: TFont read FTipTitleFont write SetTitleFont;
  property TipFont: TFont read FTipFont write SetTipFont;
  property TipFile : string read fTipFile write fTipFile;
  property PanelColor: TColor read FPanelColor write FPanelColor default clInfoBk;
  property RandomTips: boolean read FRandomTips write FRandomTips default False;
  property ShowAtStartup: boolean read FShowAtStartup write FShowAtStartup default True;
  property Position: TDlgShowPos read FPosition write FPosition;
  property SelectedTip : integer read fSelectedTip write fSelectedTip;
  property Handle : THandle read fHAndle write fHandle;
  end;

procedure Register;

implementation

uses GFTipDlgForm;

resourcestring
  STR_01  = 'Tip of the day';
  STR_02  ='Did you know...';
  STR_03  = '(Tips not found.)';

constructor TGFTipDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTips := TStringList.Create;
  FTipFont := TFont.Create;
  FTipTitleFont := TFont.Create;
  FPanelColor := clInfoBk;
  FRandomTips := False;
  FShowAtStartup := True;
  FSelectedTip := -1;
  Handle := 0;
end;

destructor TGFTipDlg.Destroy;
begin
  FTips.Free;
  inherited Destroy;
end;

function TGFTipDlg.Execute : boolean;
var
  ADlg : TGFTipForm;
begin
  result := false;
  ADlg := TGFTipForm.Create( Application );
  try
    try
      ADlg.TipPanel.Color := FPanelColor;
      if FDlgCaption <> '' then
        ADlg.OriginalCaptionText := FDlgCaption
      else
        ADlg.OriginalCaptionText := STR_01;
      ADlg.Caption := ADlg.OriginalCaptionText;
      ReadFromFile;

      if FTipTitle <> '' then
        ADlg.TipTitleLbl.Caption := FTipTitle
      else
        ADlg.TipTitleLbl.Caption := STR_02;
      ADlg.ShowChk.Checked := FShowAtStartup;
      ADlg.TipLbl.Font.Assign(FTipFont);
      ADlg.TipTitleLbl.Font.Assign(FTipTitleFont);
      ADlg.Tips.Assign(FTips);
      ADlg.IsRandom := FRandomTips;
      ADlg.Button_Prev.Visible := not FRandomTips;
      if FTips.Count > 0 then begin
       if FRandomTips then begin
        Randomize;
        FSelectedTip := Integer( Random( pred( FTips.Count )));
       end;
       ADlg.CurrentTip := FSelectedTip;
       ADlg.Button_NextClick( ADlg.Button_Next );
      end
      else
      begin
       ADlg.TipLbl.Caption := STR_03;
       ADlg.Button_Next.Enabled := False;
       ADlg.Button_Prev.Enabled := False;
      end;

      case FPosition of
         posScreenCenter : ADlg.Position := poScreenCenter;
         posFormCenter : begin
          if csDesigning in ComponentState then
           ADlg.Position := poScreenCenter
          else begin
           ADlg.Top := Application.MainForm.Top +
                 (Application.MainForm.Height div 2) -
                 (ADlg.Height div 2);
           ADlg.Left := Application.MainForm.Left +
                 (Application.MainForm.Width div 2) -
                 (ADlg.Width div 2);
          end;
         end;
      end;

      ADlg.ShowModal;
      ShowAtStartup := ADlg.ShowChk.Checked;
      FSelectedTip := ADlg.CurrentTip;
      Result := ShowAtStartup;

    except
    end;
  finally
    ADlg.Free;
  end;
end;


function TGFTipDlg.ReadFromFile : boolean;
begin
  FTips.Clear;
  try
    FTips.LoadFromFile( fTipFile );
    result := ( FTips.COunt > 0 );
  except
    result := false;
  end;
end; // ReadFromFile

procedure TGFTipDlg.SetTipFont(Value: TFont);
begin
  FTipFont.Assign(value);
end;

procedure TGFTipDlg.SetTipValues(Value: TStringList);
begin
  Tips.Assign(Value);
end;

procedure TGFTipDlg.SetTitleFont(Value: TFont);
begin
  FTipTitleFont.Assign(value);
end;

procedure Register;
begin
  RegisterComponents( 'Dialogs', [TGFTipDlg] );
end;

end.
