// !! {$if CompilerVersion < 18.0}{$define D7}{$ifend}
{$ifdef VER130} {$define D5} {$define D6} {$define D7} {$endif} // Delphi 5
{$ifdef VER140} {$define D6} {$define D7} {$endif} // Delphi 6
{$ifdef VER150} {$define D7} {$endif} // Delphi 7
