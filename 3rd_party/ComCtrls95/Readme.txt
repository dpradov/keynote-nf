Unit: ComCtrls95

Components: TPage95Control, TTab95Control, TTab95Sheet

Author: Ryan J. Mills

Author Email: rmills@freenet.edmonton.ab.ca

Code Status: Freeware

Additional Codeing: Daniel Parnell, 
                    Patrick O'Keeffe, 
                    Dave Lively (Floating Tabs),
                    Earl F. Glynn (Rotation Routines),
                    Mark Timmings,
                    Andrea Sessa,
                    Walter Sciancalepore,
                    Flemming Gorzelak,
                    Richard Chang,
                    Juan Pedro Laencina.

Disclaimer: THIS SOFTWARE AND THE ACCOMPANYING FILES ARE DISTRIBUTED 
"AS IS" AND WITHOUT WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR 
ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.  Because of the 
various software environments into which this code may be used, NO 
WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE IS OFFERED.  Good data 
processing procedure dictates that any program based on this code be 
thoroughly tested with non-critical data before relying on it. 
THE USER MUST ASSUME THE ENTIRE RISK OF USING THE ACCOMPANYING CODE.

Comments: This code is being released as freeware.  The author asks
that if you modify the code that you email a copy of the new source 
back to him.

Appreciation:  Many thanks to Mark Timmings for his many emails 
reporting bugs in the controls and for adding his personal touch
to the control interface.  I appreciate all your help Mark!

History: (mm/dd/yy)
04/01/98 - Started on the initial coding of the controls cause 
I got bored one day and because I found that Borland hadn't
finished implementing the common tab controls from M$.

04/10/98 - Was asked to look at implementing the Floating Tab
sheet idea.  Updated and then implemented parts of the code from
Dave Lively's FloatingTabSheet component.

04/14/98 - Determind to implement non-TrueType fonts on the Vertical 
Tab positions, I found the BMP rotation code from Earl F. Glynn 
on a DejaNews post and modified it for internal use.

04/19/98 - Modified the BMP rotation code to overcome the 24-bit
limitation that existed with in it.  Now is able to handle BMP bit
levels from 16 color to 24 bit color.  Currently unable to test 
32 bit color, but the new code should be able to handle that as well.

04/21/98 - Began implementation of adding Images to tabs.  Modifying 
the base class to try and use inheritance just screwed up the two 
control types.  So I've implemented the Imgage handling in the two
descendants of TCustomTab95Control.

04/23/98 - First release into public domain. (v1.0)

04/27/98 - Revised TPage95Control and TTab95Sheet to include the 
following changes.  TPage95Control added two events, OnFloatTabSheet 
and OnDockTabSheet.  TTab95Sheet added the same two events and also 
added a read only property called FloatingForm.  FloatingForm allows
you to change properties of the floating form.  This property is only 
valid after the TTab95Sheet.OnFloatTabSheet event has been fired and 
while the tab is floating.  Thanks to Mark Timmings for these changes.
TPage95Contols OnFloatTabSheet and OnDockTabSheet events fire before
TTab95Sheets events do.

04/28/98 - Fixed a bug with images displaying properly after a floating
tab was returned to its TPage95Control.  Thanks to Rick Beerendonk for
finding that one.

04/29/98 - Rewrote the Grip portion of the TTab95Sheet control.  It is
no longer a seperate control but embedded in to the TTab95Sheet.  
Corrected a display bug found by Mark, when manually floating the tabs
via the FloatTabSheet method.  As well added TextRotation to the 
control to allow vertical or horizontal text when the tabs are at any
position, thanks to Andrea Sessa and Walter Sciancalepore.  
Publishing next release of the components today. (v2.0)

04/30/98 - Mark added the TabInactiveColor property. (Started v2.1)

05/03/98 - Fixed about half a dozen problems with the TTab95Sheet and
TPage95Control.  Included in the Tab Access Violation message a string
to determine from which method the access occured.  This has turned
out to be a god send for debugging purposes.  (v 2.2)

05/04/98 - Added Color and InactiveTabFonts to the controls.  Thanks to
Troy Penke for the Color stuff.  I didn't think it would be that easy!
Oh well I guess that's why I release this stuff as PD.

05/21/98 - After a break from these controls I've added a touch more
sophisication to the controls.  I've cleaned up the code a little and
added some property logic to the way buttons and tabs work together.
The code clean up needs to be way more substancial but I don't
have the time right now. 

05/27/98 - After a quick bug demo I've corrected a big problem in the
logic of what happens when a user tries to add a tab to a page/tab
control and there is a tab hidden within the control. (v 2.3)

06/05/98 - After a long lull Mark has provided me with the the last
of his changes.  This includes the Floating property of the TTab95Sheet
component.  (v 2.3.1)

07/06/98 - Here it is at last!  I've finally implemented the D2 stuff
and it wasn't as hard as I thought it was going to be.  This version 
has the D3 consts declared in it that D2 are missing and I've added a
sucky image rotation for non-TTF's.  I've tested it a little on both
systems and I think it's ready....Thanks to Flemming Gorzelak for
sending me a bug fix for the components that he discovered while using
them in C++ Builder.  So now I've decided to declare these things 
ready for D2, D3 and C++ Builder (v3).  I've also added tab hints!
I've also added a method to do tabtracking for people who want to 
do Drag'n'Drop stuff(?) Hey, it was asked for and I could see a use 
for it!  (v2.5)

07/12/98 - I got my hands on a beta copy of D4 and tried to install 
the controls.  BIG Mistake!!  It seems that Borland (Inprise) has 
decided to do away with a couple of D3's published (and documented) 
components and method declarations in D4.  I wasn't impressed.  I've 
gone through and changed the code and added in conditional defines for 
D2 and D4.  With these changes I've been able to successfully compile 
the controls (and run them) under D2, D3 and D4.  I've also been told 
that they run under BCB as well.  The default defines SHOULD work in 
all cases. As usual use at your own risk.

07/28/98 - I've finally tested the controls under a full release 
version of D4 and everything seems to work fine.  I've also added a 
GetTabAt function from a suggestion of Richard Chang.  I've noted a 
couple of discrepencies in the way the TabHints work so I've started 
looking at correcting them.  Nobody has complained yet so I think I'll
leave them until the next release.  (v2.7)

08/04/98 - After receiving an email from Stefan Marte I've discovered and
fixed a bug in trying to retrieve the imageindex of a TTab95Sheet when the
page is floating.

08/13/98 - After arguing with a co-worker about how a floating form
should behave after the pagecontrol was freed up, I've fixed a problem
arising from that situation.  I've also added in the ability to lock 
a forms position to always floatontop of the applications mainform.
Thanks to Juan Pedro Laencina for this enhancement.  I've received a
number of requests for the floating forms icon to match the Tab images on
the pagecontrol and so I've made that the default action.  To see the 
floating forms icon you need to set the borderstyle of the floatingform
during the OnFloatChange event to a border style that supports the visible
icon.  **NOTE**  The floatingform property is only available after a
OnFloatChange event.  You should test it for nil before using it!!! (v2.72)

08/30/98 - After recieving a note from Magnus Gittins, I've discovered a bug
in the controls.  I haven't quite got a handle on this one yet but here is 
what happens:  Under WinNT, with MS San Serif (8pt), if you set the tabs 
vertical and the textrotation to trvertical when you type a word who's 
textwidth is 64 pixels long you get an access violation.  It's not just 64 
pixels I've also determined that 178 (and others before and after) cause the
same problem.  Under Win95 this error doesn't occur so I'm assuming that it's
a WinNT problem with TBitmaps but I'm not positive.  If you can shed any light
on this problem please let me know.  Thanks.

08/31/98 - Arrgh!!  I hate it when I do something really dumb and it comes
back to kick me in the rump.  I found what was causing the AVE with the font
rotation stuff and have fixed it.  It was one errant line of code where I was
setting a brush but not with an assign statement.  As soon as I fixed it there
was no longer any problem.  Releasing the fix today. (v2.8)

02/06/99 - After a long break I've been alerted to a problem with v2.8 of the
controls.  It seems that some people like to use images larger than 16x16.  I 
don't personally understand why <grin> but I've now fixed the problem with 
images being larger that 16x16.  It seems that a normal 32x32 image icon didn't
map correctly with the text that was being displayed on the tab.  So I've now
corrected this and at the same time I've corrected a nagging problem with the
text and images on the veritcal tabs as well.  I've also replaced a portion of
the image rotation code and text drawing code.  These changes were done to speed 
up the drawing of static fonts and to draw the accelerator key text properly.
(Sombody sent me most of the code that did this and I've included it but I've
in this version of the control.  Unfortunatly I've lost the email, with his name.
I appoligize for that.  I'm attempting to recover the lost message but....)
Accelerator keys now work for both TPage95Control and TTab95Control.  One problem 
that has been discovered and no fix found yet is the fact that DrawText doesn't 
draw the accelerator key properly if the TrueType font is rotated (vertical).  
This is an anoying problem. (v2.9)

02/10/99 - Thanks to Tom Lisjac I've discovered that I've got a rather nasty
D2 problem with the existing ComCtrl95 code.  I've fixed the code have have
started releasing it. (v2.91)

02/22/99 - Thanks to Eric Grange, Michael Powers and João Lira for pointing
out some bugs that really shouldn't have been there in the first place! <g>
I've got more support in the controls now for D4, better support for BCB3
(I hope) and fewer problems with D2.  Thanks to Michael for running most
of the BCB3 testing.  We're still working on a few minor issues but I 
hoppe to have those out of the way with the next public release. 
Thanks goes to Bob Daneshfar for pointing out the Shortcut shortfall
I left when I implemented the shortcut keys on the controls.(v2.92)