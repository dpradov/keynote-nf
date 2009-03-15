{$I DFS.INC}  { Standard defines for all Delphi Free Stuff components }

{------------------------------------------------------------------------------}
{ dfsAbout unit v1.03                                                          }
{------------------------------------------------------------------------------}
{ This unit provides a property editor that I use for the version property in  }
{ all of my components.                                                        }
{                                                                              }
{ Copyright 1999-2001, Brad Stowers.  All Rights Reserved.                     }
{                                                                              }
{ Copyright:                                                                   }
{ All Delphi Free Stuff (hereafter "DFS") source code is copyrighted by        }
{ Bradley D. Stowers (hereafter "author"), and shall remain the exclusive      }
{ property of the author.                                                      }
{                                                                              }
{ Distribution Rights:                                                         }
{ You are granted a non-exlusive, royalty-free right to produce and distribute }
{ compiled binary files (executables, DLLs, etc.) that are built with any of   }
{ the DFS source code unless specifically stated otherwise.                    }
{ You are further granted permission to redistribute any of the DFS source     }
{ code in source code form, provided that the original archive as found on the }
{ DFS web site (http://www.delphifreestuff.com) is distributed unmodified. For }
{ example, if you create a descendant of TdfsColorButton, you must include in  }
{ the distribution package the colorbtn.zip file in the exact form that you    }
{ downloaded it from http://www.delphifreestuff.com/mine/files/colorbtn.zip.   }
{                                                                              }
{ Restrictions:                                                                }
{ Without the express written consent of the author, you may not:              }
{   * Distribute modified versions of any DFS source code by itself. You must  }
{     include the original archive as you found it at the DFS site.            }
{   * Sell or lease any portion of DFS source code. You are, of course, free   }
{     to sell any of your own original code that works with, enhances, etc.    }
{     DFS source code.                                                         }
{   * Distribute DFS source code for profit.                                   }
{                                                                              }
{ Warranty:                                                                    }
{ There is absolutely no warranty of any kind whatsoever with any of the DFS   }
{ source code (hereafter "software"). The software is provided to you "AS-IS", }
{ and all risks and losses associated with it's use are assumed by you. In no  }
{ event shall the author of the softare, Bradley D. Stowers, be held           }
{ accountable for any damages or losses that may occur from use or misuse of   }
{ the software.                                                                }
{                                                                              }
{ Support:                                                                     }
{ All DFS source code is provided free of charge. As such, I can not guarantee }
{ any support whatsoever. While I do try to answer all questions that I        }
{ receive, and address all problems that are reported to me, you must          }
{ understand that I simply can not guarantee that this will always be so.      }
{                                                                              }
{ Clarifications:                                                              }
{ If you need any further information, please feel free to contact me directly.}
{ This agreement can be found online at my site in the "Miscellaneous" section.}
{------------------------------------------------------------------------------}
{ Feel free to contact me if you have any questions, comments or suggestions   }
{ at bstowers@pobox.com.                                                       }
{ The lateset version of my components are always available on the web at:     }
{   http://www.delphifreestuff.com/                                            }
{------------------------------------------------------------------------------}
{ Date last modified:  June 27, 2001                                           }
{------------------------------------------------------------------------------}

{$IFDEF DFS_COMPILER_3_UP}
{$WEAKPACKAGEUNIT ON} { Allow unit to exist in multiple packages }
{$ENDIF}

unit dfsAbout;

interface

uses
  {$IFDEF DFS_NO_DSGNINTF}
  DesignIntf,
  DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TdfsVersionProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

uses
  Dialogs, SysUtils;

procedure TdfsVersionProperty.Edit;
const
  ABOUT_TEXT = '%s'#13#13 +
     'Copyright 1999, Brad Stowers, All Rights Reserved.'#13 +
     'This component is distributed as freeware.'#13#13 +
     'The latest version of this component can be found on'#13 +
     'my web site, Delphi Free Stuff, at:'#13 +
     '  http://www.delphifreestuff.com/'#13;
begin
  MessageDlg(Format(ABOUT_TEXT, [GetStrValue]), mtInformation, [mbOK], 0);
end;

function TdfsVersionProperty.GetValue: string;
var
  i: integer;
begin
  i := Pos(' v', GetStrValue);
  Result := Copy(GetStrValue, i + 2, Length(GetStrValue)-i);
end;

function TdfsVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

end.
