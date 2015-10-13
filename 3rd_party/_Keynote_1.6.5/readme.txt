KEYNOTE: SOURCE CODE README
Released: 30 June 2001
Updated: 12 Nov 2003
Version: 1.6
-----------------------------------------------------------

This is the full (or as full as possible) source code
for the KeyNote program. It is distributed under the
conditions of Mozilla Public License (MPL). See file
MPL.TXT for details. The license, of course, applies
only to units written by the author, Marek Jedlinski.
Third-party units may be regulated by their own
licenses.

-----------------------------------------------------------

DISCLAIMERS and such.

KeyNote is a work in progress. The source does contain
"dead" code and paths that lead nowhere. There are not
many, but they are there.

I am not a professional programmer. I larn things as I go.
In the process, I have re-invented the wheel many times
over, and occasionally the wheel is somewhat, well, angular.
Imperfections abound.

-----------------------------------------------------------

IF YOU NEED ASSISTANCE in compliing KeyNote, contact me.
See URls and addresses at the end of this file.


-----------------------------------------------------------

KeyNote would never have come to existence if not for
many great libraries written by other Delphi programmers.
KeyNote makes a MASSIVE use of third-party units and
components. The file "compile.txt" has a complete list
of third-party libraries required to compile KeyNote.

Important notes about third-party code:

1. In developing KeyNote, I have ONLY used third-party
units and components which are "freeware" and distributed
with full source code. All these libraries can be downloaded
for free (see Torry's Delphi Pages, the Delphi Super Page,
and Delphi Free Stuff).

2. While the third-party libraries are "free" in the common
usage of the word, they are not necessarily licensed as
"free software" or "open source software". Sometimes their
licenses may be even more open (even public domain), and
at other times, the licensing conditions may be more restrictive.
I have yet to investigate which of these third-party libraries
I can legitimately distribute with KeyNote; but you can just
download them all yourself.

3. In a few (very few) cases, I have found it necessary to
modify third-party source code. This, unfortunately, makes
it even harder to distribute those libraries. I will try to
resolve the issue as soon as possible.

-----------------------------------------------------------

CODING PRACTICES

Fair warnings. I am utterly convinced that all variables
should be initialized. (This generates tons of "value not
used" compiler hints.) I lay out code with plenty of space.
I prefer clarity over compactness. My eyesight isn't the
best in the world, and I find constructs such as
	foo:=(vara-varb)*CalcSomething(varc);
unreadable. In my source, they'll always be written	as
	foo := (( varA - varB ) * CalcSomething( varC ));
I do not write
	if not Verified then exit;
Instead, I write
	if ( not Verified ) then
		exit;
I find this much more readable, easier to follow, and
I can have a breakpoint on the 'exit;' line.


-----------------------------------------------------------

SPECIAL REQUEST

If you are interested in compiling or modifying KeyNote
source code, I would very much appreciate if you started
by taking a look at the "bugs.txt" file (installed with
KeyNote) and see if you have any ideas on how some of
the problems described there can be fixed. I have no clue
as to how these issues should be handled. Thank you.

-----------------------------------------------------------

 URLs:

 - for OpenSource development:
 http://keynote.sourceforge.net
THIS SITE IS UNUSED. It could provide a priceless resource
for the development of KeyNote, but I don't have enough time
to work on it. I'll appreciate any help with this.

 - original author's software site:
 http://www.tranglos.com

 Email addresses (at least one should be valid)
 <marekjed@pobox.com>
 <marekjed@users.sourceforge.net>
 <cicho@polbox.com>
-----------------------------------------------------------
