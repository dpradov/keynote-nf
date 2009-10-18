Using MS Word as a Spelling and Grammar Checker
ver 2.0

***** NEW in Version 2
In Version 2, provision is made to change the Caption of the spelling correction dialog that MS Word pops up. This uses a hook function to monitor messages and so can have a slight impact on performance.

A module function IsWordPresent: Boolean is provided to check the registry for the 'Word.Application' key.

Work around provided for irritating bug in Word - when CheckSpelling was used and their dialog pops up with an error flagged, clicking on the Options button, then OK would throw a OleServerError. Now I use the CheckGrammar function at the Document level which actually does a spell check first and avoids the error.
*****

The COM interface exposed by MS Word gives a number of mechanisms for the use of the spelling engine. 

i) The Word.Application object can call the engine for a single word or string. The return value is a Boolean indicating True (no errors) or False (errors). Further work must be done to provide spelling suggestions / correction.

ii) The Document object contained by the Word.Application object exposes all the functionality of MS Word, in particular, the MS Word dialog boxes for spelling and grammar checking can be used.

The technique used in this component is the second method. Text is copied to the (hidden) word document, then the appropriate function (spelling or grammar) is called. To discover if the user made any changes to the text, MS Word's ability to track changes is utilised.
RTF text can be checked easily by using the CheckClipBoardSpell / Grammar functions.

Custom Dictionaries: Using custom dictionaries is a powerful feature of Word's spelling checker but there appears to be an error in the documentation (at least I can't get it to work as written in Delphi or VBA within Word itself!). Custom dictionaries are text files (usually with a '.dic' extension) with a sorted list of words - one word on each line. In this version of the component, I have included the ability to "install" a custom dictionary(s) which will be used in all spell checks. The down side is that the dictionary will now be seen by Word all the time, so it's good probably a good idea to remove any such dictionary before exiting your program.

Checking if the user wants to cancel spell/grammar checking: In your own program you could check for the press of a "cancel" button or key. This isn't available when using Word's built in dialogs, so I tend to use the SpellErrorCount and GrammarErrorCount properties to check if there are still errors in the text after a spell check has been completed. If the user has decided to quit, these numbers will be non-zero.

Installation:
i) extract the files to a suitable directory. 
ii) Open the AJBSpell.dpk (Delphi 5) or Create a new package and add the pas & dcr files to the package.
iii) if all goes well, the TAJBSpell component will appear on a new tab [AJB].

Use:
i) Drop the component onto a form.
ii) Call the appropriate function (the first time will take a bit longer as Word needs to be loaded).

Examples:

CheckSpelling(text) - uses the MS Word interactive spelling check dialog. Returns true if the errors have been corrected - the changed text is in the ChangedText property and the number of changes can be obtained from the NumChanges property.

CheckGrammar(text) - similar to CheckSpelling, except it checks the grammar.
eg:
   if speller1.CheckGrammar(memo1.lines.text) then 
	memo1.lines.text := speller1.ChangedText;

CheckClipBoardSpell / Grammar - checks the text on the clipboard and puts any changed text back on the clipboard. Returns true if changes were made and NumChanges indicates the number of corrections made. This function will allow RTF text to be checked simply.
eg: 
    RichEdit1.SelectAll;
    Richedit1.CopyToClipboard;
    if Speller1.CheckClipboardSpell then RichEdit1.PastefromClipboard;

Properties:

Connected: Boolean - read only - True if there is an active Word document
ChangedText: string - read only - contains the corrected text from a call to CheckSpelling / Grammar
NumChanges: Integer - read only - contains the number of changes made to the text SpellChecked: Boolean - read only - returns true if the text has been checked by Word
SpellErrorCount: Integer - read only - returns the number of misspellings that are STILL in the text
GrammarErrorCount: Integer - read only - returns the number of Grammatical errors that are STILL in the text.
CheckGrammarWithSpelling: Boolean - read/write - if True then grammar and spelling are checked simultaneously. This property can also be set on the Spelling & Grammar tab of Word's Options dialog.

***** NEW in Version 2
WordVersion: String - returns the current version of Word
SpellCaption: String - required caption for Word's spelling dialog.
HookCaption: Boolean - If true then the Caption of Word's spelling error dialog will be changed to the contents of the SpellCaption property.

Methods:
    
constructor Create(AOwner: TComponent); override; - does not automatically connect to Word
destructor Destroy; override; - automatically calls Disconnect

procedure Connect; - connects to a Word Application - Connected is false if there was an error
procedure Disconnect; - closes the hidden Word Application
function AddCustomDic(const FileName: string): Integer; - adds a custom dictionary to the list that is used for spelling checks. NB: If the filename does not exist, Word creates one at that location. The return value is the "index" of the Custom Dictionary within Word's Dictionary Collection.

procedure RemoveCustomDic(const Index: Integer); overload;
procedure RemoveCustomDic(const Name: string); overload; - Use RemoveCustomDic to remove a dictionary from Word's internal list. The index returned by AddCustomDic should be passed as the parameter. NB: The file is not deleted, just no longer registered by Word.

function CheckSpelling(const Text: string): Boolean;
function CheckGrammar(const Text: string): Boolean; - return True if any changes were made. The changed text is placed in the ChangedText property.

function CheckClipboardSpell: Boolean;
function CheckClipboardGrammar: Boolean; - return True if any changes were made. The changed text is placed on the clipboard. This is useful for checking RTF text as all formatting is preserved.

procedure ResetIgnoreAll; - resets the list of words that are ignored by the spell check
procedure SpellingOptions; - pops up Word's Spelling & Grammar options dialog

Copyright:
This component is offered as FREEWARE and so I accept no liability. I have checked it with MS Word 97 and 2000. I suspect that it won't work with Word 95 due to the different programming model (WordBasic) but it would be good if someone could check it out for me.
I've had to resort to some "messy" programming techniques to ensure that Word stays hidden and the dialogs appear on top of other programs. If any better programmers have simpler ideas as to how this can be done, I'd love to hear them.
Please let me know of any improvements / features that you think would be worthwhile.

Andrew Baylis (ajbaylis@melbpc.org.au)