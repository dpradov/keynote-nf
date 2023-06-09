{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{                                                       }
{*******************************************************}

unit RXTConst;

{ RX tools components constants }
{
  Reserved diapasone
  from MaxExtStrID - 136
  to   MaxExtStrID - 184
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ DualList }

  SDualListSrcCaption         = MaxExtStrID - 136;
  SDualListDestCaption        = MaxExtStrID - 137;

{ ClipView }

  SClipbrdUnknown             = MaxExtStrID - 138;
  SClipbrdEmpty               = MaxExtStrID - 139;

{ SpeedBar }

  SCustomizeSpeedbar          = MaxExtStrID - 140;
  SAvailButtons               = MaxExtStrID - 141;
  SSpeedbarCategories         = MaxExtStrID - 142;
  SSpeedbarEditHint           = MaxExtStrID - 143;

{ MathParser }

  SParseSyntaxError           = MaxExtStrID - 145;
  SParseNotCramp              = MaxExtStrID - 146;
  SParseDivideByZero          = MaxExtStrID - 147;
  SParseSqrError              = MaxExtStrID - 148;
  SParseLogError              = MaxExtStrID - 149;
  SParseInvalidFloatOperation = MaxExtStrID - 150;

implementation

{$IFDEF WIN32}
 {$R *.R32}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.