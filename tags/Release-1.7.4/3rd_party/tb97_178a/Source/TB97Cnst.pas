unit TB97Cnst;

{
  Toolbar97
  Copyright (C) 1998-2001 by Jordan Russell
  For conditions of distribution and use, see LICENSE.TXT.

  String constants

  $Id: TB97Cnst.pas,v 1.2 2001/01/04 04:17:14 jr Exp $
}

interface

{$I TB97Ver.inc}

{$IFDEF TB97D3} resourcestring {$ELSE} const {$ENDIF}
  { TDock97 exception messages }
  STB97DockParentNotAllowed = 'A TDock97 control cannot be placed inside a tool window or another TDock97';
  STB97DockCannotChangePosition = 'Cannot change Position of a TDock97 if it already contains controls';

  { TCustomToolWindow97 exception messages }
  STB97ToolwinNameNotSet = 'Cannot save tool window''s position because Name property is not set';
  STB97ToolwinDockedToNameNotSet = 'Cannot save tool window''s position because DockedTo''s Name property not set';
  STB97ToolwinParentNotAllowed = 'A tool window can only be placed on a TDock97 or directly on the form';

  { TCustomToolbar97 exception messages }
  STB97ToolbarControlNotChildOfToolbar = 'Control ''%s'' is not a child of the toolbar';

  { TToolbarSep97 exception messages }
  STB97SepParentNotAllowed = 'TToolbarSep97 can only be placed on a TToolbar97';

implementation

end.
 
