{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{ Patched by Polaris Software                           }
{ Patched by Jouni Airaksinen                           }
{*******************************************************}

unit RxColors;

{$C PRELOAD}
{$I RX.INC}
{$DEFINE RX_COLOR_APPENDED}
interface

uses Classes, Controls, Graphics, Forms, RxVCLUtils;

function RxIdentToColor(const Ident: string; var Color: TColor): Boolean;
function RxColorToString(Color: TColor): string;
function RxStringToColor(S: string): TColor;
procedure RxGetColorValues(Proc: TGetStrProc);

const
  clInfoBk16 = TColor($02E1FFFF);
  clNone16 = TColor($02FFFFFF);

  { Added colors }

  clWavePale =          TColor($00D0D0D0); { selected tab }
  clWaveDarkGray =      TColor($00505050); { borders }
  clWaveGray =          TColor($00A0A0A0); { menus, unselected tabs }
  clWaveLightGray =     TColor($00BCBCBC); { gray button }

  clWaveBeige =         TColor($00B4C4C4); { button face }
  clWaveBrightBeige =   TColor($00BFFFFF); { selected text }
  clWaveLightBeige =    TColor($00D8E8E8); { hotkey }

  clWaveCyan =          TColor($00C4B4C4); { button face }
  clWaveBrightCyan =    TColor($00FFBFFF); { selected text }
  clWaveLightCyan =     TColor($00E8D8E8); { hotkey }

  clWaveGreen =         TColor($00B8B8BA); { button face }
  clWaveBrightGreen =   TColor($00CFCFFF); { selected text }
  clWaveLightGreen =    TColor($00DCDCE8); { hotkey }

  clWaveViolet =        TColor($02C4B8C2); { button face }
  clWaveBrightViolet =  TColor($02FFCFCF); { selected text }
  clWaveLightViolet =   TColor($02E8DCDC); { hotkey }

  { Standard Encarta & FlatStyle Color Constants }

  clRxDarkBlue = TColor($00996633);
  clRxBlue = TColor($00CF9030);
  clRxLightBlue = TColor($00CFB78F);

  clRxDarkRed = TColor($00302794);
  clRxRed = TColor($005F58B0);
  clRxLightRed = TColor($006963B6);

  clRxDarkGreen = TColor($00385937);
  clRxGreen = TColor($00518150);
  clRxLightGreen = TColor($0093CAB1);

  clRxDarkYellow = TColor($004EB6CF);
  clRxYellow = TColor($0057D1FF);
  clRxLightYellow = TColor($00B3F8FF);

  clRxDarkBrown = TColor($00394D4D);
  clRxBrown = TColor($00555E66);
  clRxLightBrown = TColor($00829AA2);

  clRxDarkKhaki = TColor($00D3D3D3);
  clRxKhaki = TColor($00C8D7D7);
  clRxLightKhaki = TColor($00E0E9EF);

  { added standard named html colors }
  clHtmBlack =  TColor($00000000);
  clHtmGray0 =  TColor($00170515);
  clHtmGray18 = TColor($00170525);
  clHtmGray21 = TColor($00171B2B);
  clHtmGray23 = TColor($00172230);
  clHtmGray24 = TColor($00262230);
  clHtmGray25 = TColor($00262834);
  clHtmGray26 = TColor($002C2834);
  clHtmGray27 = TColor($002C2D38);
  clHtmGray28 = TColor($0031313B);
  clHtmGray29 = TColor($0035353E);
  clHtmGray30 = TColor($00393841);
  clHtmGray31 = TColor($003C3841);
  clHtmGray32 = TColor($003F3E46);
  clHtmGray34 = TColor($0044434A);
  clHtmGray35 = TColor($0046464C);
  clHtmGray36 = TColor($0048484E);
  clHtmGray37 = TColor($004B4A50);
  clHtmGray38 = TColor($004F4E54);
  clHtmGray39 = TColor($00515056);
  clHtmGray40 = TColor($00545459);
  clHtmGray41 = TColor($0058585C);
  clHtmGray42 = TColor($00595A5F);
  clHtmGray43 = TColor($005D5D62);
  clHtmGray44 = TColor($00606064);
  clHtmGray45 = TColor($00626366);
  clHtmGray46 = TColor($00656569);
  clHtmGray47 = TColor($0068696D);
  clHtmGray48 = TColor($006B6A6E);
  clHtmGray49 = TColor($006D6E72);
  clHtmGray50 = TColor($00707174);
  clHtmGray =            TColor($006E6F73);
  clHtmSlateGray4 =      TColor($007E6D61);
  clHtmSlateGray =       TColor($00837365);
  clHtmLightSteelBlue4 = TColor($007E6D64);
  clHtmLightSlateGray =  TColor($008D7B6D);
  clHtmCadetBlue4 =      TColor($007E784C);
  clHtmDarkSlateGray4 =  TColor($007E7D4C);
  clHtmThistle4 =        TColor($007E6D80);
  clHtmMediumSlateBlue = TColor($00805A5E);
  clHtmMediumPurple4 =   TColor($007E384E);
  clHtmMidnightBlue =    TColor($00541B15);
  clHtmDarkSlateBlue =   TColor($0056382B);
  clHtmDarkSlateGray =   TColor($003C3825);
  clHtmDimGray =         TColor($00413E46);
  clHtmCornflowerBlue =  TColor($008D1B15);
  clHtmRoyalBlue4 =      TColor($007E3115);
  clHtmSlateBlue4 =      TColor($007E2D34);
  clHtmRoyalBlue =       TColor($00DE602B);
  clHtmRoyalBlue1 =      TColor($00FF6E30);
  clHtmRoyalBlue2 =      TColor($00EC652B);
  clHtmRoyalBlue3 =      TColor($00C75425);
  clHtmDeepSkyBlue =     TColor($00FFB93B);
  clHtmDeepSkyBlue2 =    TColor($00ECAC38);
  clHtmSlateBlue =       TColor($00C77E35);
  clHtmDeepSkyBlue3 =    TColor($00C79030);
  clHtmDeepSkyBlue4 =    TColor($007E5825);
  clHtmDodgerBlue =      TColor($00FF8915);
  clHtmDodgerBlue2 =     TColor($00EC7D15);
  clHtmDodgerBlue3 =     TColor($00C76915);
  clHtmDodgerBlue4 =     TColor($007E3E15);
  clHtmSteelBlue4 =      TColor($007E542B);
  clHtmSteelBlue =       TColor($00A06348);
  clHtmSlateBlue2 =      TColor($00EC6069);
  clHtmViolet =          TColor($00C9388D);
  clHtmMediumPurple3 =   TColor($00C75D7A);
  clHtmMediumPurple =    TColor($00D76784);
  clHtmMediumPurple2 =   TColor($00EC7291);
  clHtmMediumPurple1 =   TColor($00FF7B9E);
  clHtmLightSteelBlue =  TColor($00CE8F72);
  clHtmSteelBlue3 =      TColor($00C78A48);
  clHtmSteelBlue2 =      TColor($00ECA556);
  clHtmSteelBlue1 =      TColor($00FFB35C);
  clHtmSkyBlue3 =        TColor($00C79E65);
  clHtmSkyBlue4 =        TColor($007E6241);
  clHtmSlateBlue3 =      TColor($00A17C73);
  clHtmSlateGray3 =      TColor($00C7AF98);
  clHtmVioletRed =       TColor($008A35F6);
  clHtmVioletRed1 =      TColor($008A35F6);
  clHtmVioletRed2 =      TColor($007F31E4);
  clHtmDeepPink =        TColor($008728F5);
  clHtmDeepPink2 =       TColor($007C28E4);
  clHtmDeepPink3 =       TColor($006722C1);
  clHtmDeepPink4 =       TColor($003F057D);
  clHtmMediumVioletRed = TColor($006B22CA);
  clHtmVioletRed3 =      TColor($006928C1);
  clHtmFirebrick =       TColor($00170580);
  clHtmVioletRed4 =      TColor($0041057D);
  clHtmMaroon4 =         TColor($0052057D);
  clHtmMaroon =          TColor($00410581);
  clHtmMaroon3 =         TColor($008322C1);
  clHtmMaroon2 =         TColor($009D31E3);
  clHtmMaroon1 =         TColor($00AA35F5);
  clHtmMagenta =         TColor($00FF00FF);
  clHtmMagenta1 =        TColor($00FF33F4);
  clHtmMagenta2 =        TColor($00EC38E2);
  clHtmMagenta3 =        TColor($00C731C0);
  clHtmMediumOrchid =    TColor($00B548B0);
  clHtmMediumOrchid1 =   TColor($00FF62D4);
  clHtmMediumOrchid2 =   TColor($00EC5AC4);
  clHtmMediumOrchid3 =   TColor($00C74AA7);
  clHtmMediumOrchid4 =   TColor($007E286A);
  clHtmPurple =          TColor($00EF358E);
  clHtmPurple1 =         TColor($00FF3B89);
  clHtmPurple2 =         TColor($00EC387F);
  clHtmPurple3 =         TColor($00C72D6C);
  clHtmPurple4 =         TColor($007E1B46);
  clHtmDarkOrchid4 =     TColor($007E1B57);
  clHtmDarkOrchid =      TColor($007E1B7D);
  clHtmDarkViolet =      TColor($00CE2D84);
  clHtmDarkOrchid3 =     TColor($00C7318B);
  clHtmDarkOrchid2 =     TColor($00EC3BA2);
  clHtmDarkOrchid1 =     TColor($00FF41B0);
  clHtmPlum4 =           TColor($007E587E);
  clHtmPaleVioletRed =   TColor($008765D1);
  clHtmPaleVioletRed1 =  TColor($00A178F7);
  clHtmPaleVioletRed2 =  TColor($00946EE5);
  clHtmPaleVioletRed3 =  TColor($007C5AC2);
  clHtmPaleVioletRed4 =  TColor($004D357E);
  clHtmPlum =            TColor($008F3BB9);
  clHtmPlum1 =           TColor($00FFB7F9);
  clHtmPlum2 =           TColor($00ECA9E6);
  clHtmPlum3 =           TColor($00C78EC3);
  clHtmThistle =         TColor($00D3B9D2);
  clHtmThistle3 =        TColor($00C7AEC6);
  clHtmLavenderBlush2 =  TColor($00E2DDEB);
  clHtmLavenderBlush3 =  TColor($00BEBBC8);
  clHtmThistle2 =        TColor($00ECCFE9);
  clHtmThistle1 =        TColor($00FFDFFC);
  clHtmLavender =        TColor($00FAE4E3);
  clHtmLavenderBlush =   TColor($00F4EEFD);
  clHtmLightSteelBlue1 = TColor($00FFDEC6);
  clHtmLightBlue =       TColor($00FFDFAD);
  clHtmLightBlue1 =      TColor($00FFEDBD);
  clHtmLightCyan =       TColor($00FFFFE0);
  clHtmSlateGray1 =      TColor($00FFDFC2);
  clHtmSlateGray2 =      TColor($00ECCFB4);
  clHtmLightSteelBlue2 = TColor($00ECCEB7);
  clHtmTurquoise1 =      TColor($00FFF352);
  clHtmCyan =            TColor($00FFFF00);
  clHtmCyan1 =           TColor($00FFFE57);
  clHtmCyan2 =           TColor($00ECEB50);
  clHtmTurquoise2 =      TColor($00ECE24E);
  clHtmMediumTurquoise = TColor($00CDCC48);
  clHtmTurquoise =       TColor($00DBC643);
  clHtmDarkSlateGray1 =  TColor($00FFFE9A);
  clHtmDarkSlateGray2 =  TColor($00ECEB8E);
  clHtmDarkSlateGray3 =  TColor($00C7C778);
  clHtmCyan3 =           TColor($00C7C746);
  clHtmTurquoise3 =      TColor($00C7BF43);
  clHtmCadetBlue3 =      TColor($00C7BF77);
  clHtmPaleTurquoise3 =  TColor($00C7C792);
  clHtmLightBlue2 =      TColor($00ECDCAF);
  clHtmDarkTurquoise =   TColor($009C9C3B);
  clHtmCyan4 =           TColor($007E7D30);
  clHtmLightSeaGreen =   TColor($009FA93E);
  clHtmLightSkyBlue =    TColor($00FACA82);
  clHtmLightSkyBlue2 =   TColor($00ECCFA0);
  clHtmLightSkyBlue3 =   TColor($00C7AF87);
  clHtmSkyBlue =         TColor($00FFCA82);
  clHtmSkyBlue2 =        TColor($00ECBA79);
  clHtmLightSkyBlue4 =   TColor($007E6D56);
  clHtmSkyBlue5 =        TColor($00FF9866);
  clHtmLightSlateBlue =  TColor($00FF6A73);
  clHtmLightCyan2 =      TColor($00ECECCF);
  clHtmLightCyan3 =      TColor($00C7C7AF);
  clHtmLightCyan4 =      TColor($007D7D71);
  clHtmLightBlue3 =      TColor($00C7B995);
  clHtmLightBlue4 =      TColor($007E765E);
  clHtmPaleTurquoise4 =  TColor($007E7D5E);
  clHtmDarkSeaGreen4 =   TColor($00587C61);
  clHtmMediumAquamarine =TColor($00818734);

  clHtmMediumSeaGreen =    TColor($00546730);
  clHtmSeaGreen =          TColor($0075894E);
  clHtmDarkGreen =         TColor($00174125);
  clHtmSeaGreen4 =         TColor($00447C38);
  clHtmForestGreen =       TColor($0058924E);
  clHtmMediumForestGreen = TColor($00357234);
  clHtmSpringGreen4 =      TColor($002C7C34);
  clHtmDarkOliveGreen4 =   TColor($00267C66);
  clHtmChartreuse4 =       TColor($00177C43);
  clHtmGreen4 =            TColor($00177C34);
  clHtmMediumSpringGreen = TColor($00178034);
  clHtmSpringGreen =       TColor($002CA04A);
  clHtmLimeGreen =         TColor($0017A341);
  clHtmDarkSeaGreen =      TColor($0081B38B);
  clHtmDarkSeaGreen3 =     TColor($008EC699);
  clHtmGreen3 =            TColor($0017C44C);
  clHtmChartreuse3 =       TColor($0017C46C);
  clHtmYellowGreen =       TColor($0017D052);
  clHtmSpringGreen3 =      TColor($0052C54C);
  clHtmSeaGreen3 =         TColor($0071C554);
  clHtmSpringGreen2 =      TColor($0064E957);
  clHtmSpringGreen1 =      TColor($006EFB5E);
  clHtmSeaGreen2 =         TColor($0086E964);
  clHtmSeaGreen1 =         TColor($0092FB6A);
  clHtmDarkSeaGreen2 =     TColor($00AAEAB5);
  clHtmDarkSeaGreen1 =     TColor($00B8FDC3);
  clHtmGreen =             TColor($0000FF00);
  clHtmLawnGreen =         TColor($0017F787);
  clHtmGreen1 =            TColor($0017FB5F);
  clHtmGreen2 =            TColor($0017E859);
  clHtmChartreuse2 =       TColor($0017E87F);
  clHtmChartreuse =        TColor($0017FB8A);
  clHtmGreenYellow =       TColor($0017FBB1);
  clHtmDarkOliveGreen1 =   TColor($005DFBCC);
  clHtmDarkOliveGreen2 =   TColor($0054E9BC);
  clHtmDarkOliveGreen3 =   TColor($0044C5A0);
  clHtmYellow =            TColor($0000FFFF);
  clHtmYellow1 =           TColor($0017FCFF);
  clHtmKhaki1 =            TColor($0080F3FF);
  clHtmKhaki2 =            TColor($0075E2ED);
  clHtmGoldenrod =         TColor($0074DAED);
  clHtmGold2 =             TColor($0017C1EA);
  clHtmGold1 =             TColor($0017D0FD);
  clHtmGoldenrod1 =        TColor($0017B9FB);
  clHtmGoldenrod2 =        TColor($0017ABE9);
  clHtmGold =              TColor($0017A0D4);
  clHtmGold3 =             TColor($0017A3C7);
  clHtmGoldenrod3 =        TColor($00178EC6);
  clHtmDarkGoldenrod =     TColor($001778AF);
  clHtmKhaki =             TColor($006EA9AD);
  clHtmKhaki3 =            TColor($0062BEC9);
  clHtmKhaki4 =            TColor($00397882);
  clHtmDarkGoldenrod1 =    TColor($0017B1FB);
  clHtmDarkGoldenrod2 =    TColor($0017A3E8);
  clHtmDarkGoldenrod3 =    TColor($001789C5);
  clHtmSienna1 =           TColor($003174F8);
  clHtmSienna2 =           TColor($002C6CE6);
  clHtmDarkOrange =        TColor($001780F8);
  clHtmDarkOrange1 =       TColor($001772F8);
  clHtmDarkOrange2 =       TColor($001767E5);
  clHtmDarkOrange3 =       TColor($001756C3);
  clHtmSienna3 =           TColor($001758C3);
  clHtmSienna =            TColor($0017418A);
  clHtmSienna4 =           TColor($0017357E);
  clHtmIndianRed4 =        TColor($0017227E);
  clHtmDarkOrange4 =       TColor($0017317E);
  clHtmSalmon4 =           TColor($0017387E);
  clHtmDarkGoldenrod4 =    TColor($0017527F);
  clHtmGold4 =             TColor($00176580);
  clHtmGoldenrod4 =        TColor($00175880);
  clHtmLightSalmon4 =      TColor($002C467F);
  clHtmChocolate =         TColor($00175AC8);
  clHtmCoral3 =            TColor($002C4AC3);
  clHtmCoral2 =            TColor($003C5BE5);
  clHtmCoral =             TColor($004165F7);
  clHtmDarkSalmon =        TColor($006B8BE1);
  clHtmSalmon1 =           TColor($005881F8);
  clHtmSalmon2 =           TColor($005174E6);
  clHtmSalmon3 =           TColor($004162C3);
  clHtmLightSalmon3 =      TColor($005174C4);
  clHtmLightSalmon2 =      TColor($00618AE7);



  clHtmLightSalmon =    TColor($006B96F9);
  clHtmSandyBrown =     TColor($004D9AEE);
  clHtmHotPink =        TColor($00AB60F6);
  clHtmHotPink1 =       TColor($00AB65F6);
  clHtmHotPink2 =       TColor($009D5EE4);
  clHtmHotPink3 =       TColor($008352C2);
  clHtmHotPink4 =       TColor($0052227D);
  clHtmLightCoral =     TColor($007174E7);
  clHtmIndianRed1 =     TColor($00595DF7);
  clHtmIndianRed2 =     TColor($005154E5);
  clHtmIndianRed3 =     TColor($004146C2);
  clHtmRed =            TColor($000000FF);
  clHtmRed1 =           TColor($001722F6);
  clHtmRed2 =           TColor($00171BE4);
  clHtmFirebrick1 =     TColor($001728F6);
  clHtmFirebrick2 =     TColor($001722E4);
  clHtmFirebrick3 =     TColor($00171BC1);
  clHtmPink =           TColor($00BEAFFA);
  clHtmRosyBrown1 =     TColor($00B9BBFB);
  clHtmRosyBrown2 =     TColor($00AAADE8);
  clHtmPink2 =          TColor($00B0A1E7);
  clHtmLightPink =      TColor($00BAAFFA);
  clHtmLightPink1 =     TColor($00B0A7F9);
  clHtmLightPink2 =     TColor($00A399E7);
  clHtmPink3 =          TColor($009387C4);
  clHtmRosyBrown3 =     TColor($008E90C5);
  clHtmRosyBrown =      TColor($008184B3);
  clHtmLightPink3 =     TColor($008981C4);
  clHtmRosyBrown4 =     TColor($00585A7F);
  clHtmLightPink4 =     TColor($00524E7F);
  clHtmPink4 =          TColor($005D527F);
  clHtmLavenderBlush4 = TColor($00797681);
  clHtmLightGoldenrod4 =TColor($00397381);
  clHtmLemonChiffon4 =  TColor($00607B82);
  clHtmLemonChiffon3 =  TColor($0099C2C9);
  clHtmLightGoldenrod3 =TColor($0060B5C8);
  clHtmLightGolden2 =   TColor($0072D6EC);
  clHtmLightGoldenrod = TColor($0072D8EC);
  clHtmLightGoldenrod1 =TColor($007CE8FF);
  clHtmLemonChiffon2 =  TColor($00B6E5EC);
  clHtmLemonChiffon =   TColor($00C6F8FF);
  clHtmLightGoldenrodYellow = TColor($00CCF8FA);

  clCosmicLatte = TColor($00E7F8FF);

implementation

uses
  {$IFDEF RX_D5}Windows, {$ENDIF}SysUtils; // Polaris

type
  TColorEntry = record
    Value: TColor;
    Name: PChar;
  end;

const
  ColorCount = 3{$IFDEF RX_COLOR_APPENDED} + 34 + 295 + 1{$ENDIF};

  Colors: array[0..ColorCount - 1] of TColorEntry = (

    (Value: clCream; Name: 'clCream'),
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue')
    {$IFDEF RX_COLOR_APPENDED},
    { added colors }

    (Value: clWavePale; Name: 'clWavePale'),
    (Value: clWaveDarkGray; Name: 'clWaveDarkGray'),
    (Value: clWaveGray; Name: 'clWaveGray'),
    (Value: clWaveLightGray; Name: 'clWaveLightGray'),

    (Value: clWaveBeige; Name: 'clWaveBeige'),
    (Value: clWaveBrightBeige; Name: 'clWaveBrightBeige'),
    (Value: clWaveLightBeige; Name: 'clWaveLightBeige'),

    (Value: clWaveCyan; Name: 'clWaveCyan'),
    (Value: clWaveBrightCyan; Name: 'clWaveBrightCyan'),
    (Value: clWaveLightCyan; Name: 'clWaveLightCyan'),

    (Value: clWaveGreen; Name: 'clWaveGreen'),
    (Value: clWaveBrightGreen; Name: 'clWaveBrightGreen'),
    (Value: clWaveLightGreen; Name: 'clWaveLightGreen'),

    (Value: clWaveViolet; Name: 'clWaveViolet'),
    (Value: clWaveBrightViolet; Name: 'clWaveBrightViolet'),
    (Value: clWaveLightViolet; Name: 'clWaveLightViolet'),

    { Standard Encarta & FlatStyle Color Constants }

    (Value: clRxDarkBlue; Name: 'clRxDarkBlue'),
    (Value: clRxBlue; Name: 'clRxBlue'),
    (Value: clRxLightBlue; Name: 'clRxLightBlue'),

    (Value: clRxDarkRed; Name: 'clRxDarkRed'),
    (Value: clRxRed; Name: 'clRxRed'),
    (Value: clRxLightRed; Name: 'clRxLightRed'),

    (Value: clRxDarkGreen; Name: 'clRxDarkGreen'),
    (Value: clRxGreen; Name: 'clRxGreen'),
    (Value: clRxLightGreen; Name: 'clRxLightGreen'),

    (Value: clRxDarkYellow; Name: 'clRxDarkYellow'),
    (Value: clRxYellow; Name: 'clRxYellow'),
    (Value: clRxLightYellow; Name: 'clRxLightYellow'),

    (Value: clRxDarkBrown; Name: 'clRxDarkBrown'),
    (Value: clRxBrown; Name: 'clRxBrown'),
    (Value: clRxLightBrown; Name: 'clRxLightBrown'),

    (Value: clRxDarkKhaki; Name: 'clRxDarkKhaki'),
    (Value: clRxKhaki; Name: 'clRxKhaki'),
    (Value: clRxLightKhaki; Name: 'clRxLightKhaki'),

    { added standard named html colors }
    (Value: clHtmBlack; Name: 'clHtmBlack'),
    (Value: clHtmGray0; Name: 'clHtmGray0'),
    (Value: clHtmGray18; Name: 'clHtmGray18'),
    (Value: clHtmGray21; Name: 'clHtmGray21'),
    (Value: clHtmGray23; Name: 'clHtmGray23'),
    (Value: clHtmGray24; Name: 'clHtmGray24'),
    (Value: clHtmGray25; Name: 'clHtmGray25'),
    (Value: clHtmGray26; Name: 'clHtmGray26'),
    (Value: clHtmGray27; Name: 'clHtmGray27'),
    (Value: clHtmGray28; Name: 'clHtmGray28'),
    (Value: clHtmGray29; Name: 'clHtmGray29'),
    (Value: clHtmGray30; Name: 'clHtmGray30'),
    (Value: clHtmGray31; Name: 'clHtmGray31'),
    (Value: clHtmGray32; Name: 'clHtmGray32'),
    (Value: clHtmGray34; Name: 'clHtmGray34'),
    (Value: clHtmGray35; Name: 'clHtmGray35'),
    (Value: clHtmGray36; Name: 'clHtmGray36'),
    (Value: clHtmGray37; Name: 'clHtmGray37'),
    (Value: clHtmGray38; Name: 'clHtmGray38'),
    (Value: clHtmGray39; Name: 'clHtmGray39'),
    (Value: clHtmGray40; Name: 'clHtmGray40'),
    (Value: clHtmGray41; Name: 'clHtmGray41'),
    (Value: clHtmGray42; Name: 'clHtmGray42'),
    (Value: clHtmGray43; Name: 'clHtmGray43'),
    (Value: clHtmGray44; Name: 'clHtmGray44'),
    (Value: clHtmGray45; Name: 'clHtmGray45'),
    (Value: clHtmGray46; Name: 'clHtmGray46'),
    (Value: clHtmGray47; Name: 'clHtmGray47'),
    (Value: clHtmGray48; Name: 'clHtmGray48'),
    (Value: clHtmGray49; Name: 'clHtmGray49'),
    (Value: clHtmGray50; Name: 'clHtmGray50'),
    (Value: clHtmGray; Name: 'clHtmGray'),
    (Value: clHtmSlateGray4; Name: 'clHtmSlateGray4'),
    (Value: clHtmSlateGray; Name: 'clHtmSlateGray'),
    (Value: clHtmLightSteelBlue4; Name: 'clHtmLightSteelBlue4'),
    (Value: clHtmLightSlateGray; Name: 'clHtmLightSlateGray'),
    (Value: clHtmCadetBlue4; Name: 'clHtmCadetBlue4'),
    (Value: clHtmDarkSlateGray4; Name: 'clHtmDarkSlateGray4'),
    (Value: clHtmThistle4; Name: 'clHtmThistle4'),
    (Value: clHtmMediumSlateBlue; Name: 'clHtmMediumSlateBlue'),
    (Value: clHtmMediumPurple4; Name: 'clHtmMediumPurple4'),
    (Value: clHtmMidnightBlue; Name: 'clHtmMidnightBlue'),
    (Value: clHtmDarkSlateBlue; Name: 'clHtmDarkSlateBlue'),
    (Value: clHtmDarkSlateGray; Name: 'clHtmDarkSlateGray'),
    (Value: clHtmDimGray; Name: 'clHtmDimGray'),
    (Value: clHtmCornflowerBlue; Name: 'clHtmCornflowerBlue'),
    (Value: clHtmRoyalBlue4; Name: 'clHtmRoyalBlue4'),
    (Value: clHtmSlateBlue4; Name: 'clHtmSlateBlue4'),
    (Value: clHtmRoyalBlue; Name: 'clHtmRoyalBlue'),
    (Value: clHtmRoyalBlue1; Name: 'clHtmRoyalBlue1'),
    (Value: clHtmRoyalBlue2; Name: 'clHtmRoyalBlue2'),
    (Value: clHtmRoyalBlue3; Name: 'clHtmRoyalBlue3'),
    (Value: clHtmDeepSkyBlue; Name: 'clHtmDeepSkyBlue'),
    (Value: clHtmDeepSkyBlue2; Name: 'clHtmDeepSkyBlue2'),
    (Value: clHtmSlateBlue; Name: 'clHtmSlateBlue'),
    (Value: clHtmDeepSkyBlue3; Name: 'clHtmDeepSkyBlue3'),
    (Value: clHtmDeepSkyBlue4; Name: 'clHtmDeepSkyBlue4'),
    (Value: clHtmDodgerBlue; Name: 'clHtmDodgerBlue'),
    (Value: clHtmDodgerBlue2; Name: 'clHtmDodgerBlue2'),
    (Value: clHtmDodgerBlue3; Name: 'clHtmDodgerBlue3'),
    (Value: clHtmDodgerBlue4; Name: 'clHtmDodgerBlue4'),
    (Value: clHtmSteelBlue4; Name: 'clHtmSteelBlue4'),
    (Value: clHtmSteelBlue; Name: 'clHtmSteelBlue'),
    (Value: clHtmSlateBlue2; Name: 'clHtmSlateBlue2'),
    (Value: clHtmViolet; Name: 'clHtmViolet'),
    (Value: clHtmMediumPurple3; Name: 'clHtmMediumPurple3'),
    (Value: clHtmMediumPurple; Name: 'clHtmMediumPurple'),
    (Value: clHtmMediumPurple2; Name: 'clHtmMediumPurple2'),
    (Value: clHtmMediumPurple1; Name: 'clHtmMediumPurple1'),
    (Value: clHtmLightSteelBlue; Name: 'clHtmLightSteelBlue'),
    (Value: clHtmSteelBlue3; Name: 'clHtmSteelBlue3'),
    (Value: clHtmSteelBlue2; Name: 'clHtmSteelBlue2'),
    (Value: clHtmSteelBlue1; Name: 'clHtmSteelBlue1'),
    (Value: clHtmSkyBlue3; Name: 'clHtmSkyBlue3'),
    (Value: clHtmSkyBlue4; Name: 'clHtmSkyBlue4'),
    (Value: clHtmSlateBlue3; Name: 'clHtmSlateBlue3'),
    (Value: clHtmSlateGray3; Name: 'clHtmSlateGray3'),
    (Value: clHtmVioletRed; Name: 'clHtmVioletRed'),
    (Value: clHtmVioletRed1; Name: 'clHtmVioletRed1'),
    (Value: clHtmVioletRed2; Name: 'clHtmVioletRed2'),
    (Value: clHtmDeepPink; Name: 'clHtmDeepPink'),
    (Value: clHtmDeepPink2; Name: 'clHtmDeepPink2'),
    (Value: clHtmDeepPink3; Name: 'clHtmDeepPink3'),
    (Value: clHtmDeepPink4; Name: 'clHtmDeepPink4'),
    (Value: clHtmMediumVioletRed; Name: 'clHtmMediumVioletRed'),
    (Value: clHtmVioletRed3; Name: 'clHtmVioletRed3'),
    (Value: clHtmFirebrick; Name: 'clHtmFirebrick'),
    (Value: clHtmVioletRed4; Name: 'clHtmVioletRed4'),
    (Value: clHtmMaroon4; Name: 'clHtmMaroon4'),
    (Value: clHtmMaroon; Name: 'clHtmMaroon'),
    (Value: clHtmMaroon3; Name: 'clHtmMaroon3'),
    (Value: clHtmMaroon2; Name: 'clHtmMaroon2'),
    (Value: clHtmMaroon1; Name: 'clHtmMaroon1'),
    (Value: clHtmMagenta; Name: 'clHtmMagenta'),
    (Value: clHtmMagenta1; Name: 'clHtmMagenta1'),
    (Value: clHtmMagenta2; Name: 'clHtmMagenta2'),
    (Value: clHtmMagenta3; Name: 'clHtmMagenta3'),
    (Value: clHtmMediumOrchid; Name: 'clHtmMediumOrchid'),
    (Value: clHtmMediumOrchid1; Name: 'clHtmMediumOrchid1'),
    (Value: clHtmMediumOrchid2; Name: 'clHtmMediumOrchid2'),
    (Value: clHtmMediumOrchid3; Name: 'clHtmMediumOrchid3'),
    (Value: clHtmMediumOrchid4; Name: 'clHtmMediumOrchid4'),
    (Value: clHtmPurple; Name: 'clHtmPurple'),
    (Value: clHtmPurple1; Name: 'clHtmPurple1'),
    (Value: clHtmPurple2; Name: 'clHtmPurple2'),
    (Value: clHtmPurple3; Name: 'clHtmPurple3'),
    (Value: clHtmPurple4; Name: 'clHtmPurple4'),
    (Value: clHtmDarkOrchid4; Name: 'clHtmDarkOrchid4'),
    (Value: clHtmDarkOrchid; Name: 'clHtmDarkOrchid'),
    (Value: clHtmDarkViolet; Name: 'clHtmDarkViolet'),
    (Value: clHtmDarkOrchid3; Name: 'clHtmDarkOrchid3'),
    (Value: clHtmDarkOrchid2; Name: 'clHtmDarkOrchid2'),
    (Value: clHtmDarkOrchid1; Name: 'clHtmDarkOrchid1'),
    (Value: clHtmPlum4; Name: 'clHtmPlum4'),
    (Value: clHtmPaleVioletRed; Name: 'clHtmPaleVioletRed'),
    (Value: clHtmPaleVioletRed1; Name: 'clHtmPaleVioletRed1'),
    (Value: clHtmPaleVioletRed2; Name: 'clHtmPaleVioletRed2'),
    (Value: clHtmPaleVioletRed3; Name: 'clHtmPaleVioletRed3'),
    (Value: clHtmPaleVioletRed4; Name: 'clHtmPaleVioletRed4'),
    (Value: clHtmPlum; Name: 'clHtmPlum'),
    (Value: clHtmPlum1; Name: 'clHtmPlum1'),
    (Value: clHtmPlum2; Name: 'clHtmPlum2'),
    (Value: clHtmPlum3; Name: 'clHtmPlum3'),
    (Value: clHtmThistle; Name: 'clHtmThistle'),
    (Value: clHtmThistle3; Name: 'clHtmThistle3'),
    (Value: clHtmLavenderBlush2; Name: 'clHtmLavenderBlush2'),
    (Value: clHtmLavenderBlush3; Name: 'clHtmLavenderBlush3'),
    (Value: clHtmThistle2; Name: 'clHtmThistle2'),
    (Value: clHtmThistle1; Name: 'clHtmThistle1'),
    (Value: clHtmLavender; Name: 'clHtmLavender'),
    (Value: clHtmLavenderBlush; Name: 'clHtmLavenderBlush'),
    (Value: clHtmLightSteelBlue1; Name: 'clHtmLightSteelBlue1'),
    (Value: clHtmLightBlue; Name: 'clHtmLightBlue'),
    (Value: clHtmLightBlue1; Name: 'clHtmLightBlue1'),
    (Value: clHtmLightCyan; Name: 'clHtmLightCyan'),
    (Value: clHtmSlateGray1; Name: 'clHtmSlateGray1'),
    (Value: clHtmSlateGray2; Name: 'clHtmSlateGray2'),
    (Value: clHtmLightSteelBlue2; Name: 'clHtmLightSteelBlue2'),
    (Value: clHtmTurquoise1; Name: 'clHtmTurquoise1'),
    (Value: clHtmCyan; Name: 'clHtmCyan'),
    (Value: clHtmCyan1; Name: 'clHtmCyan1'),
    (Value: clHtmCyan2; Name: 'clHtmCyan2'),
    (Value: clHtmTurquoise2; Name: 'clHtmTurquoise2'),
    (Value: clHtmMediumTurquoise; Name: 'clHtmMediumTurquoise'),
    (Value: clHtmTurquoise; Name: 'clHtmTurquoise'),
    (Value: clHtmDarkSlateGray1; Name: 'clHtmDarkSlateGray1'),
    (Value: clHtmDarkSlateGray2; Name: 'clHtmDarkSlateGray2'),
    (Value: clHtmDarkSlateGray3; Name: 'clHtmDarkSlateGray3'),
    (Value: clHtmCyan3; Name: 'clHtmCyan3'),
    (Value: clHtmTurquoise3; Name: 'clHtmTurquoise3'),
    (Value: clHtmCadetBlue3; Name: 'clHtmCadetBlue3'),
    (Value: clHtmPaleTurquoise3; Name: 'clHtmPaleTurquoise3'),
    (Value: clHtmLightBlue2; Name: 'clHtmLightBlue2'),
    (Value: clHtmDarkTurquoise; Name: 'clHtmDarkTurquoise'),
    (Value: clHtmCyan4; Name: 'clHtmCyan4'),
    (Value: clHtmLightSeaGreen; Name: 'clHtmLightSeaGreen'),
    (Value: clHtmLightSkyBlue; Name: 'clHtmLightSkyBlue'),
    (Value: clHtmLightSkyBlue2; Name: 'clHtmLightSkyBlue2'),
    (Value: clHtmLightSkyBlue3; Name: 'clHtmLightSkyBlue3'),
    (Value: clHtmSkyBlue; Name: 'clHtmSkyBlue'),
    (Value: clHtmSkyBlue2; Name: 'clHtmSkyBlue2'),
    (Value: clHtmLightSkyBlue4; Name: 'clHtmLightSkyBlue4'),
    (Value: clHtmSkyBlue5; Name: 'clHtmSkyBlue5'),
    (Value: clHtmLightSlateBlue; Name: 'clHtmLightSlateBlue'),
    (Value: clHtmLightCyan2; Name: 'clHtmLightCyan2'),
    (Value: clHtmLightCyan3; Name: 'clHtmLightCyan3'),
    (Value: clHtmLightCyan4; Name: 'clHtmLightCyan4'),
    (Value: clHtmLightBlue3; Name: 'clHtmLightBlue3'),
    (Value: clHtmLightBlue4; Name: 'clHtmLightBlue4'),
    (Value: clHtmPaleTurquoise4; Name: 'clHtmPaleTurquoise4'),
    (Value: clHtmDarkSeaGreen4; Name: 'clHtmDarkSeaGreen4'),
    (Value: clHtmMediumAquamarine; Name: 'clHtmMediumAquamarine'),
    (Value: clHtmMediumSeaGreen; Name: 'clHtmMediumSeaGreen'),
    (Value: clHtmSeaGreen; Name: 'clHtmSeaGreen'),
    (Value: clHtmDarkGreen; Name: 'clHtmDarkGreen'),
    (Value: clHtmSeaGreen4; Name: 'clHtmSeaGreen4'),
    (Value: clHtmForestGreen; Name: 'clHtmForestGreen'),
    (Value: clHtmMediumForestGreen; Name: 'clHtmMediumForestGreen'),
    (Value: clHtmSpringGreen4; Name: 'clHtmSpringGreen4'),
    (Value: clHtmDarkOliveGreen4; Name: 'clHtmDarkOliveGreen4'),
    (Value: clHtmChartreuse4; Name: 'clHtmChartreuse4'),
    (Value: clHtmGreen4; Name: 'clHtmGreen4'),
    (Value: clHtmMediumSpringGreen; Name: 'clHtmMediumSpringGreen'),
    (Value: clHtmSpringGreen; Name: 'clHtmSpringGreen'),
    (Value: clHtmLimeGreen; Name: 'clHtmLimeGreen'),
    (Value: clHtmDarkSeaGreen; Name: 'clHtmDarkSeaGreen'),
    (Value: clHtmDarkSeaGreen3; Name: 'clHtmDarkSeaGreen3'),
    (Value: clHtmGreen3; Name: 'clHtmGreen3'),
    (Value: clHtmChartreuse3; Name: 'clHtmChartreuse3'),
    (Value: clHtmYellowGreen; Name: 'clHtmYellowGreen'),
    (Value: clHtmSpringGreen3; Name: 'clHtmSpringGreen3'),
    (Value: clHtmSeaGreen3; Name: 'clHtmSeaGreen3'),
    (Value: clHtmSpringGreen2; Name: 'clHtmSpringGreen2'),
    (Value: clHtmSpringGreen1; Name: 'clHtmSpringGreen1'),
    (Value: clHtmSeaGreen2; Name: 'clHtmSeaGreen2'),
    (Value: clHtmSeaGreen1; Name: 'clHtmSeaGreen1'),
    (Value: clHtmDarkSeaGreen2; Name: 'clHtmDarkSeaGreen2'),
    (Value: clHtmDarkSeaGreen1; Name: 'clHtmDarkSeaGreen1'),
    (Value: clHtmGreen; Name: 'clHtmGreen'),
    (Value: clHtmLawnGreen; Name: 'clHtmLawnGreen'),
    (Value: clHtmGreen1; Name: 'clHtmGreen1'),
    (Value: clHtmGreen2; Name: 'clHtmGreen2'),
    (Value: clHtmChartreuse2; Name: 'clHtmChartreuse2'),
    (Value: clHtmChartreuse; Name: 'clHtmChartreuse'),
    (Value: clHtmGreenYellow; Name: 'clHtmGreenYellow'),
    (Value: clHtmDarkOliveGreen1; Name: 'clHtmDarkOliveGreen1'),
    (Value: clHtmDarkOliveGreen2; Name: 'clHtmDarkOliveGreen2'),
    (Value: clHtmDarkOliveGreen3; Name: 'clHtmDarkOliveGreen3'),
    (Value: clHtmYellow; Name: 'clHtmYellow'),
    (Value: clHtmYellow1; Name: 'clHtmYellow1'),
    (Value: clHtmKhaki1; Name: 'clHtmKhaki1'),
    (Value: clHtmKhaki2; Name: 'clHtmKhaki2'),
    (Value: clHtmGoldenrod; Name: 'clHtmGoldenrod'),
    (Value: clHtmGold2; Name: 'clHtmGold2'),
    (Value: clHtmGold1; Name: 'clHtmGold1'),
    (Value: clHtmGoldenrod1; Name: 'clHtmGoldenrod1'),
    (Value: clHtmGoldenrod2; Name: 'clHtmGoldenrod2'),
    (Value: clHtmGold; Name: 'clHtmGold'),
    (Value: clHtmGold3; Name: 'clHtmGold3'),
    (Value: clHtmGoldenrod3; Name: 'clHtmGoldenrod3'),
    (Value: clHtmDarkGoldenrod; Name: 'clHtmDarkGoldenrod'),
    (Value: clHtmKhaki; Name: 'clHtmKhaki'),
    (Value: clHtmKhaki3; Name: 'clHtmKhaki3'),
    (Value: clHtmKhaki4; Name: 'clHtmKhaki4'),
    (Value: clHtmDarkGoldenrod1; Name: 'clHtmDarkGoldenrod1'),
    (Value: clHtmDarkGoldenrod2; Name: 'clHtmDarkGoldenrod2'),
    (Value: clHtmDarkGoldenrod3; Name: 'clHtmDarkGoldenrod3'),
    (Value: clHtmSienna1; Name: 'clHtmSienna1'),
    (Value: clHtmSienna2; Name: 'clHtmSienna2'),
    (Value: clHtmDarkOrange; Name: 'clHtmDarkOrange'),
    (Value: clHtmDarkOrange1; Name: 'clHtmDarkOrange1'),
    (Value: clHtmDarkOrange2; Name: 'clHtmDarkOrange2'),
    (Value: clHtmDarkOrange3; Name: 'clHtmDarkOrange3'),
    (Value: clHtmSienna3; Name: 'clHtmSienna3'),
    (Value: clHtmSienna; Name: 'clHtmSienna'),
    (Value: clHtmSienna4; Name: 'clHtmSienna4'),
    (Value: clHtmIndianRed4; Name: 'clHtmIndianRed4'),
    (Value: clHtmDarkOrange4; Name: 'clHtmDarkOrange4'),
    (Value: clHtmSalmon4; Name: 'clHtmSalmon4'),
    (Value: clHtmDarkGoldenrod4; Name: 'clHtmDarkGoldenrod4'),
    (Value: clHtmGold4; Name: 'clHtmGold4'),
    (Value: clHtmGoldenrod4; Name: 'clHtmGoldenrod4'),
    (Value: clHtmLightSalmon4; Name: 'clHtmLightSalmon4'),
    (Value: clHtmChocolate; Name: 'clHtmChocolate'),
    (Value: clHtmCoral3; Name: 'clHtmCoral3'),
    (Value: clHtmCoral2; Name: 'clHtmCoral2'),
    (Value: clHtmCoral; Name: 'clHtmCoral'),
    (Value: clHtmDarkSalmon; Name: 'clHtmDarkSalmon'),
    (Value: clHtmSalmon1; Name: 'clHtmSalmon1'),
    (Value: clHtmSalmon2; Name: 'clHtmSalmon2'),
    (Value: clHtmSalmon3; Name: 'clHtmSalmon3'),
    (Value: clHtmLightSalmon3; Name: 'clHtmLightSalmon3'),
    (Value: clHtmLightSalmon2; Name: 'clHtmLightSalmon2'),
    (Value: clHtmLightSalmon; Name: 'clHtmLightSalmon'),
    (Value: clHtmSandyBrown; Name: 'clHtmSandyBrown'),
    (Value: clHtmHotPink; Name: 'clHtmHotPink'),
    (Value: clHtmHotPink1; Name: 'clHtmHotPink1'),
    (Value: clHtmHotPink2; Name: 'clHtmHotPink2'),
    (Value: clHtmHotPink3; Name: 'clHtmHotPink3'),
    (Value: clHtmHotPink4; Name: 'clHtmHotPink4'),
    (Value: clHtmLightCoral; Name: 'clHtmLightCoral'),
    (Value: clHtmIndianRed1; Name: 'clHtmIndianRed1'),
    (Value: clHtmIndianRed2; Name: 'clHtmIndianRed2'),
    (Value: clHtmIndianRed3; Name: 'clHtmIndianRed3'),
    (Value: clHtmRed; Name: 'clHtmRed'),
    (Value: clHtmRed1; Name: 'clHtmRed1'),
    (Value: clHtmRed2; Name: 'clHtmRed2'),
    (Value: clHtmFirebrick1; Name: 'clHtmFirebrick1'),
    (Value: clHtmFirebrick2; Name: 'clHtmFirebrick2'),
    (Value: clHtmFirebrick3; Name: 'clHtmFirebrick3'),
    (Value: clHtmPink; Name: 'clHtmPink'),
    (Value: clHtmRosyBrown1; Name: 'clHtmRosyBrown1'),
    (Value: clHtmRosyBrown2; Name: 'clHtmRosyBrown2'),
    (Value: clHtmPink2; Name: 'clHtmPink2'),
    (Value: clHtmLightPink; Name: 'clHtmLightPink'),
    (Value: clHtmLightPink1; Name: 'clHtmLightPink1'),
    (Value: clHtmLightPink2; Name: 'clHtmLightPink2'),
    (Value: clHtmPink3; Name: 'clHtmPink3'),
    (Value: clHtmRosyBrown3; Name: 'clHtmRosyBrown3'),
    (Value: clHtmRosyBrown; Name: 'clHtmRosyBrown'),
    (Value: clHtmLightPink3; Name: 'clHtmLightPink3'),
    (Value: clHtmRosyBrown4; Name: 'clHtmRosyBrown4'),
    (Value: clHtmLightPink4; Name: 'clHtmLightPink4'),
    (Value: clHtmPink4; Name: 'clHtmPink4'),
    (Value: clHtmLavenderBlush4; Name: 'clHtmLavenderBlush4'),
    (Value: clHtmLightGoldenrod4; Name: 'clHtmLightGoldenrod4'),
    (Value: clHtmLemonChiffon4; Name: 'clHtmLemonChiffon4'),
    (Value: clHtmLemonChiffon3; Name: 'clHtmLemonChiffon3'),
    (Value: clHtmLightGoldenrod3; Name: 'clHtmLightGoldenrod3'),
    (Value: clHtmLightGolden2; Name: 'clHtmLightGolden2'),
    (Value: clHtmLightGoldenrod; Name: 'clHtmLightGoldenrod'),
    (Value: clHtmLightGoldenrod1; Name: 'clHtmLightGoldenrod1'),
    (Value: clHtmLemonChiffon2; Name: 'clHtmLemonChiffon2'),
    (Value: clHtmLemonChiffon; Name: 'clHtmLemonChiffon'),
    (Value: clHtmLightGoldenrodYellow; Name: 'clHtmLightGoldenrodYellow'),
    (Value: clCosmicLatte; Name: 'clCosmicLatte')
    {$ENDIF}
    );

function RxColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then
  begin
    for I := Low(Colors) to High(Colors) do
      if Colors[I].Value = Color then
      begin
        Result := Colors[I].Name;
        Exit;
      end;
    Result := Format('$%.8x', [Color]);
  end;
end;

function RxIdentToColor(const Ident: string; var Color: TColor): Boolean;
var
  I: Integer;
begin
  {own colors}
  for I := Low(Colors) to High(Colors) do
    if AnsiCompareText(Colors[I].Name, Ident) = 0 then
    begin
      Color := Colors[I].Value;
      Result := True;
      Exit;
    end;
  {systems colors}
  Result := IdentToColor(Ident, Integer(Color));
end;

function RxStringToColor(S: string): TColor;
begin
  if not RxIdentToColor(S, Result) then
  try
    Result := StringToColor(S);
  except
    Result := clNone;
  end;
end;

procedure RxGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(Colors) to High(Colors) do
    Proc(StrPas(Colors[I].Name));
end;

end.

