-----------------------------------------------------------
KEYNOTE PLUGINS - DEVELOPER KIT

by Marek Jedlinski
<marekjed@pobox.com>
http://keynote.prv.pl
http://www.tranglos.com
22 June 2001

-----------------------------------------------------------

This package contains information for developers who want to create new plugins for use with KeyNote.

First, please see the "Plugins" topic KeyNote's main Help. It contains all the basic information which you will need. Also, try using the existing plugins to see how they work in practice. (Plugin commands are located under the "Tools" menu and on the Resource panel.)

This package does NOT contain binary (compiled) plugins. A few samples are installed with KeyNote. Others may be downloaded from KeyNote's homepage ("Download" section).

The package DOES contain Borland Delphi source code for several plugins. Feel free to use these as examples or templates for your own plugins.

The "plugintest.dpr" project is the simplest possible plugin, and is the best place to begin. The source code contains detailed comments that will help you understand what's going on.

KeyNote is written in Borland Delphi 3.0. You should be able to create plugins with any compiler which can create standard Windows DLL files. (As far as I know, this EXCLUDES Visual Basic.)

-----------------------------------------------------------

EXAMPLES OF PLUGIN FUNCTIONALITY

Plugins are most useful for performing features which:

* are not used very frequently, or appeal only to a selection of users

* can be performed outside of KeyNote's main process

* require allocating considerable amounts of memory or system resources. With plugins, the resources are consumed only while the plugin is running.


For example, plugins could provide the following features:

* converting RTF text of a note to HTML format

* converting an HTML document to RTF stream, which KeyNote will import as a new note

* other format conversions between RTF or KNT and third-party file formats

* implementing a spell checker for RTF text

* calculating advanced text statistics (e.g. a readability index)

* providing access to a dictionary, thesaurus or other reference work

* a calculator, a calendar, an alarm/reminder utility

* a HTML viewer

* a phone dialer

* all sorts of Internet-related functionality. A plugin could check for existence of a new version of KeyNote. Or, a plugin could reimplement KeyNote's "Send Note via Email" function (I am considering rewriting that function as a plugin, to reduce the size of keynote.exe).

-----------------------------------------------------------

QUICK START:

* KeyNote plugins require KeyNote version 0.999 or higher.

* Keynote plugins are DLL files with ".KNL" extension. (This is done so that it will be possible to execute a plugin by double-clicking it in Windows Explorer.)

* Plugins MUST be installed in the "\plugins" subdirectory, which is automatically created by the setup program.

* A plugin cannot be executed without KeyNote running. When user exits KeyNote, all running plugins are automatically shut down.

* A plugin can receive text from KeyNote's active note, and it can request a specific format (plain text, RTF).

* A plugin can return text to KeyNote in several ways.

* A plugin does not HAVE to receive or return text.

* Typically, a plugin runs as a modal dialog box. KeyNote is not available to the user while the plugin is running.

* A plugin can also be "resident", which means that it will run non-modally, alongside KeyNote. See the "keyrepeat.dpr" and "scratchpad.dpr" projects for examples on how to do this.

* Resident plugins may receive text from KeyNote, but they CANNOT return text in the standard way. However, a resident plugin can use Windows messages (WM_COPYDATA in particular) to send keypresses or text to KeyNote.

* There is no limit on the number of plugins that may run simultaneously, but only one copy of each resident plugin is allowed at a time.

* Resident plugins are tricky.

* You should be familiar with the basic concepts of creating Windows DLLs before you try to dig deeper here. I may be able to help you with Delphi-related questions, but note that my time is limited, and I already answer a lot of support mail from users. You should be able to create KeyNote plugins using a suitable C compiler, but I will not be able to help you there at all, I don't know the first thing about C. However, please do write if you realize that I screwed something up and your C dll cannot communicate with KeyNote properly.

* Do contact me if your plugin needs some additional functionality from KeyNote which is not supported at present.

-----------------------------------------------------------

HOW DOES A PLUGIN WORK?

A plugin must export a number of functions with precisely defined names, arguments and return values. KeyNote will assume the plugin supports these functions, and will call them as necessary.

KeyNote loads the plugin DLLs dynamically, and unloads them when no longer needed.

After initializing the DLL, KeyNote will do several things which are described in detail below. However, the three most important things that KeyNote will do are:

	1. KeyNote will check the plugin version, to see if it can handle the plugin. Currently (KeyNote 1.03), plugin must return 1 as version number. If the plugin logic changes sufficiently so as to no longer be compatible with current implementation, the version number will be increased, to tell KeyNote which implementation to use for the particular plugin. KeyNote will refuse to run a plugin if the version number is higher than what KeyNote can handle (currently: 1)

	2. If the version number check succeeds, KeyNote will poll the plugin for its options. To do so, KeyNote will call the "KNTGetPluginFeatures" function. This gives the plugin a chance to tell KeyNote about how the plugin behaves, what it expects to receive and what it will return after it has been executed. For instance, the plugin can tell KeyNote that it wants to receive RTF-formatted text from active note, and that it will return text which KeyNote should display in a dialog box. At this stage, the plugin can also tell KeyNote that it DOES NOT WANT to be executed. (For instance, the plugin can check the computer configuration, or look for missing files, and decide that it cannot run in the present environment. If the plugin does so, KeyNote will display a suitable message to the user and will not execute the plugin.)

	3. If the plugin can be executed, KeyNote will do so. To execute a plugin, KeyNote will call the plugin's "KNTPluginExecute" function, and pass any parameters that the plugin requested in step 2.

After the plugin was executed, KeyNote will:

	4. Perform the operation that the plugin requested, e.g. will display a dialog box with the message the plugin returned, or will insert the text it has received from the plugin in active note. (This does NOT apply to resident plugins.)

	5. Call the "KNTPluginCleanup" procedure, which allows the plugin to perform any cleanup required, e.g. deallocate memory that was allocated in "KNTPluginExecute".

	6. Unload the plugin DLL. (Unless the plugin is resident, in which case the DLL will be unloaded when the plugin shuts down. The resident plugin MUST notify KeyNote about the fact that it's about to quit, by sending an appropriate message to KeyNote. Consult "keycodes.dpr" or "scratchpad.dpr" plugin projects to see how this is achieved.)


There are a few other functions that each plugin must support. They provide additional information about the plugin, such as the internal name and description of the plugin, or allow the plugin to be configured.

-----------------------------------------------------------

SOME CONVENTIONS

Please see the suppoprt file "kn_PluginBase.pas" for a model list of function types that each plugin must export (these are the functions that KeyNote will call to operate the plugin).

The names of the exported functions MUST correspond in name, spelling, letter case, argument list and calling convention to the function declarations in your plugin code.

All exported functions use only the basic, common data types that are also used throughout Windows API. For instance, Pascal strings (long or short) are NOT used. Strings are passed via untyped buffers instead. Other data types used are POINTER, LONGINT and BOOL (or LONGBOOL).

NOTE ABOUT THE CALLING CONVETION:
All exported functions use the "stdcall" calling convention, which MUST be explicitly specified, because it is not the default for Delphi. This allows KeyNote to use plugins written in languages other than Delphi.

Most functions return a LONGINT value. This should be used to notify KeyNote about any errors that may have occurred. A returned value other than 0 will be interpreted as an error code.
EXCEPTIONS:
- "KNTPluginExecute" returns the size of the data (text) it is returning to KeyNote. The function should return 0 if it is not passing back any text. Error codes should be returned as negative integers.
- "KNTGetPluginFeatures" returns a longint value that describes the plugin features. See below (and source code) for details.

-----------------------------------------------------------

BASIC DLL STRUCTURE

This is the core of a KeyNote plugin (Borland Delphi ".dpr" source code).

Each plugin MUST export all these functions. In some cases, the function is not required to actually do anything, but the functions must be present; otherwise KeyNote will not execute the plugin.



======= BEGIN =======

library plugintest;

uses
  // actual code is contained here:
  plugintestunit in 'plugintestunit.pas';

exports
  // the names of these functions MUST be spelled
  // exactly as they are here; otherwise KeyNote
  // will not find them. Remember that DLL function
  // names are Case-Sensitive.
  KNTGetPluginName index 1,
  KNTGetPluginVersion index 2,
  KNTGetPluginDescription index 3,
  KNTConfigurePlugin index 4,
  KNTGetPluginFeatures index 5,
  KNTPluginExecute index 6,
  KNTPluginCleanup index 7;

// This tells the compiler to create a file
// with the .KNL extension, rather than the
// standard .DLL extension
{$E .knl}

begin
end.

======= END =======

-----------------------------------------------------------

BRIEF OVERVIEW OF EXPORTED FUNCTIONS

For detailed descriptions of the functions, their arguments and return values, please see the source code in "plugintestunit.pas".


KNTGetPluginName
  Returns the internal name of the plugin.
  Example: "KeyNote test plugin"
  Note: This name will be displayed by KeyNote in its "Plugins" dialog box. Therefore, it should be short enough to fit in the listbox control of that dialog box.


KNTGetPluginVersion
  Returns the plugin "technology" version number, so that KeyNote can decide if it is capable of operating this plugin. Currently, plugins MUST return the value of 1; otherwise KeyNote will refuse to execute the plugin. The number may be incremented when the plugin logic or the set of exported functions changes and is no longer compatible with present version. (Note that this is NOT the internal version number of the plugin itself; that might be passed in KNTGetPluginName.)


KNTGetPluginDescription
  Returns a free-form description of the plugin.
  Example: "This is only a test plugin. See KeyNote website for more information."
  Note: This description string will be displayed by KeyNote in its "Plugins" dialog box. A resizeable label is used to show the description, but it still should not be exceedingly long.


KNTConfigurePlugin
  KeyNote calls this function when user clicks the "Configure" button in the "Plugins" dialog box. This is a chance for the plugin to show some configuration interface, to allow the user to change any settings before running the plugin. However, plugins are not required to support any configuration whatsoever. For instance, the test plugin displays a dialog box with its name and description, instead. (See the "scratchpad.dpr" or "erisdate.dpr" plugin projects to see how this is done.)


KNTGetPluginFeatures
  Returns a longint value which KeyNote will interpret as a set of 32 booleans (each of the 32 bits in the longint can be set or unset, and thus interpreted as TRUE or FALSE). This function is very important, because it tells KeyNote what data, if any, to pass to the plugin, and what data, if any, the plugin will return when it is executed. If a plugin is resident, the information about it is also passed via the return value of this function. See "plugintestunit.pas" for implementation, and "kn_PluginBase.pas" for the option flags that you have to use.


KNTPluginExecute
  This is the function which performs the actual JOB of the plugin. KeyNote passes a number of arguments to this function, which you will find useful inside the plugin. (E.g. the Application handle, the main form handle, the name of the currently open file and the name of the active note). The function returns a longint value representing the size of data (length of text) the plugin returns to KeyNote. A value of 0 tells KeyNote that no text is being returned. A negative value indicates an error condition.


KNTPluginCleanup
  If the "KNTPluginExecute" function was executed, then this function is executed immediately afterwards. The function does not return any value. The plugin should use it to free any memory allocated in "KNTPluginExecute". The function will be called REGARDLESS of the value returned by the "KNTPluginExecute" function, so you must check for any error condition yourself. This function is ALSO called for resident plugins (even though you probably will not want to deallocate anything there, because the plugin will still be running. Resident plugins have to clean up their own mess in other ways; see source code for details.)


-----------------------------------------------------------
