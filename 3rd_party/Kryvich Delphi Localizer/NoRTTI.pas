// As of XE5 and newer, this needs to be in each individual unit
// for which you want to disable RTTI
{$IFDEF UNICODE}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
