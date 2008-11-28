{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxGConst;

{ RX graphic support constants }
{
  Reserved diapasone
  from MaxExtStrID - 200
  to   MaxExtStrID - 230
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

{ RxGIF }

const
  SGIFImage            = MaxExtStrID - 200;
  SChangeGIFSize       = MaxExtStrID - 201;
  SNoGIFData           = MaxExtStrID - 202;
  SUnrecognizedGIFExt  = MaxExtStrID - 203;
  SWrongGIFColors      = MaxExtStrID - 204;
  SBadGIFCodeSize      = MaxExtStrID - 205;
  SGIFDecodeError      = MaxExtStrID - 206;
  SGIFEncodeError      = MaxExtStrID - 207;
  SGIFVersion          = MaxExtStrID - 208;

implementation

{$IFDEF WIN32}
 {$R *.R32}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.