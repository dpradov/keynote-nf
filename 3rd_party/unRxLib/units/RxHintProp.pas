{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995 AO ROSNO                   }
{                                                       }
{ Patched by Jaro.Benes                                 }
{*******************************************************}

unit RxHintProp;

interface

{$I RX.INC}

uses
  {$IFDEF RX_D6} DesignIntf, DesignEditors, VCLEditors {$ELSE} DsgnIntf {$ENDIF};

type

{ THintProperty }

  THintProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    {$IFNDEF VER80}
    function GetEditLimit: Integer; override;
    {$ENDIF}
    procedure Edit; override;
  end;

implementation

{$IFNDEF VER80}
 {$D-}
{$ENDIF}

uses SysUtils, Classes, {$IFDEF RX_D3} RxStrLEdit, {$ELSE} StrEdit, {$ENDIF}
  TypInfo, Forms, Controls, RxStrUtils;

function THintProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{$IFNDEF VER80}
function THintProperty.GetEditLimit: Integer;
begin
  //XE2 corrected malfunction by JB.
  case GetPropType^.Kind of
    tkString, tkLString:
      Result := GetTypeData(GetPropType)^.MaxLength; //only for shortstring
  else
    Result := $FFFF div 2;
  end;
end;
{$ENDIF}

procedure THintProperty.Edit;
var
  Temp: string;
  Comp: TPersistent;
  {$IFNDEF RX_D6}
  I, Cnt: Integer;
  {$ENDIF}
begin
  with TStrEditDlg.Create(nil) do
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;
    Temp := GetStrValue;
    {$IFDEF RX_D6}
    Memo.Lines.Text := Temp;
    {$ELSE}
    Cnt := WordCount(Temp, [#13, #10]);
    for I := 1 to Cnt do
      Memo.Lines.Add(ExtractWord(I, Temp, [#13, #10]));
    Memo.MaxLength := GetEditLimit;
    {$ENDIF}
    UpdateStatus(nil);
    if ShowModal = mrOk then
    begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

end.