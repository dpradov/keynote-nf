unit gf_Bits;
{$I gf_base.inc}

(*
Author: Damian Gorski
http://www.swissdelphicenter.ch/torry/showcode.php?id=1341    (TORRY's Delphi)
*)

interface
  function IsBitSet(const val: longint; const TheBit: byte): boolean;
  function BitOn(const val: longint; const TheBit: byte): LongInt;
  function BitOff(const val: longint; const TheBit: byte): LongInt;
  function BitToggle(const val: longint; const TheBit: byte): LongInt;



implementation

function IsBitSet(const val: longint; const TheBit: byte): boolean;
begin
  result := (val and (1 shl TheBit)) <> 0;
end;

function BitOn(const val: longint; const TheBit: byte): LongInt;
begin
  result := val or (1 shl TheBit);
end;

function BitOff(const val: longint; const TheBit: byte): LongInt;
begin
  result := val and ((1 shl TheBit) xor $FFFFFFFF);
end;

function BitToggle(const val: longint; const TheBit: byte): LongInt;
begin
  result := val xor (1 shl TheBit);
end;

end.

