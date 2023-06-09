{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 2000 UIL                        }
{                                                       }
{ Adopted Plug-ins tool by UIL                          }
{*******************************************************}
unit RxPluginParamsForm;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ExtCtrls, Buttons;

type
  TfrmPluginParams = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btOK: TBitBtn;
    btCancel: TBitBtn;
    edtPluginName: TEdit;
    mmoDescripton: TMemo;
    edtPluginHostProject: TEdit;
    edtPluginUID: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RadioGroup1: TRadioGroup;
    rbPackage: TRadioButton;
    rbDLL: TRadioButton;
    lblCreateInfo: TLabel;
    procedure FormShow(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
  private

  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

resourcestring
  RsPluginParamsFormInfoText =
    'The settings above will create the following project:' +
    sLineBreak + sLineBreak +
    '* A project called Plg%0:s.%1:s' + sLineBreak +
    '* A unit called Plugin%0:s, containing the data module T%0:s.';

{ TfrmPluginParams }

procedure TfrmPluginParams.SettingsChanged(Sender: TObject);
  function RbToPrjExt: string;
  begin
    Result := 'dpk';
    if rbDLL.Checked then
      Result := 'dpr';
  end;
begin
  lblCreateInfo.Caption := Format(RsPluginParamsFormInfoText, [edtPluginName.Text, RbToPrjExt]);
  btOK.Enabled := Trim(edtPluginName.Text) <> '';
  edtPluginUID.Text := 'RX.' + edtPluginHostProject.Text + '.Plg' + edtPluginName.Text;
end;

procedure TfrmPluginParams.FormShow(Sender: TObject);
begin
  SettingsChanged(Sender);
end;

end.