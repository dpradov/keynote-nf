WordWeb thesaurus/dictionary component for Delphi and C++ Builder
Version 1.62, freeware

The component uses the free WordWeb thesaurus/dictionay available from

   http://wordweb.co.uk/free

You (and anyone using your program) should install WordWeb before using this component.
If it is not installed the component raises an exception when you look up a word.
It will also work with WordWeb Pro, available from http://www.wordweb.co.uk/

For more flexible and customizable dictionary/thesaurus components see
WordWeb Developer at http://www.x-word.com/wwdev/

-----------------------------------------------------------------------------------
INSTALLATION
-----------------------------------------------------------------------------------

The component will be installed onto a tab called "Freeware" on the component pallette:

 1)  	From the Delphi/C++ Builder Component menu select "Install Component"
 2)  	In Delphi 2 click Add
 3)	Type in the full path and name of the FreeWordWeb.pas file

The components will then be compiled.

-----------------------------------------------------------------------------------
USAGE
-----------------------------------------------------------------------------------

Add the component to a form. Use:

 Execute:          Function shows the WordWeb window
 LookupWord:       Determines which word is looked up
 CloseOnCopy:	   Determines whether the window should return when the user 
                   selects a synonym and presses Copy. If true the return value of
                   Execute can be used to tell if Copy or Close was pressed
 ReturnWord:	   The word selected when the user pressed Copy when CloseOnCopy 
                   is true

For example, if you have set the "CloseOnCopy" property to "true", the following 
code could be used to look  up a word and allow the user to choose a replacement:

...
  FreeWordWeb1.LookupWord:='xxxxxx';
  if FreeWordWeb1.Execute then
   begin
	ReplacementWord:=FreeWordWeb1.ReturnWord;	
	...
   end;
...


If WordWeb is not installed an exception will be raised. You could handle this, 
for example

 try 
 FreeWordWeb1.Execute; 
 except
  on EFreeWordWebError do
      Application.MessageBox('You don''t have WordWeb installed. Get it from http://wordweb.co.uk/free',
       'Couldn''t load WordWeb',mb_OK or mb_IconStop);
 end;

-----------------------------------------------------------------------------------
LICENCE
-----------------------------------------------------------------------------------

This component is freeware, do what you like with it.

WordWeb is also freeware. You can include it with your applications as long as
it is distributed in the form of the original self-extracting exe file. Alternatively
you can give people the link http://wordweb.co.uk/free to download it when they like.


-----------------------------------------------------------------------------------
COPYRIGHT AND DISCLAIMER
-----------------------------------------------------------------------------------

TFreeWordWeb Copyright © 2000 by Antony Lewis. All rights reserved.

This code is provided "AS IS" and Antony Lewis accepts no responsibility for anything that
happens as a result of use of it.