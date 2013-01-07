 --------------------------------------------------------
 Expression Evaluator v1.4 for Delphi
 (16 & 32 bits)

 Copyright © 1997 by BitSoft Development, L.L.C.
 All rights reserved

 Web:     http://www.bitsoft.com
 E-mail:  info@bitsoft.com
 Support: tech-support@bitsoft.com
 --------------------------------------------------------
 Portions Copyright © 1992 by Borland International, Inc.
 All rights reserved
 --------------------------------------------------------

 CONTENTS
 
 1. General
 2. Installation
 3. Using this component
   A. Calculating the value of a function
   B. Using user-defined variables
   C. Handling errors
 4. Support, feedback and bugs
 5. Other BitSoft products
 6. Contacting BitSoft Development
 7. Joining the BitSoft Delphi News List
 8. Revision History


 1. General
 ----------

 This component is a mathematical function parser for
 Delphi.  It allows you to calculate a value given an
 expression in string form.  It supports the following
 operators and functions:

    +    :   Addition
    -    :   Substraction
    *    :   Multiplication
    /    :   Division
    ^    :   Exponential (only positive numbers 
               for the base)
    (    :   Open parenthesis
    )    :   Close parenthesis
   MOD   :   MOD operator.  Integers only (if used with floating
             point numbers, it will round the values to integers)

   ABS      (Range: -1e4932..1e4932)
   ATAN     (Range: -1e4932..1e4932)
   COS      (Range: -1e18..1e18)
   EXP      (Range: -11356..11356)
   LN       (Range: 0..1e4932)
   ROUND    (Range: -1e9..1e9)
   SIN      (Range: -1e18..1e18)
   SQRT     (Range: 0..1e4932)
   SQR      (Range: -1e2446..1e2446)
   TRUNC    (Range: -1e9..1e9)


 It also supports an unlimited number of user-defined
 variables and complete error handling support,
 including returning the position in the string where
 an error ocurred.

 This file is distributed as freeware and without 
 warranties of any kind.  Use of this software is
 at your own risk.  See the License Agreement for more
 information.


 2. Installation
 ---------------

 To use this unit in your applications:

 A. Copy the following files into a directory of your
    choice:
 
     1) Parser.pas 

     2) Parser.dcr          (if you are using Delphi2/3)
        \16bits\Parser.dcr  (if you are using Delphi1)

 B. Add Parser.pas to the VCL library using OPTIONS |
     INSTALL COMPONENTS in Delphi 1, or COMPONENTS |
     INSTALL in Delphi2.


 3. Using this unit
 ------------------

 To use this component in your applications, add the
 TMathParser component (located in the Third-Party page
 of the Component Palette) to a form.  Then follow the
 instructions below for performing any of the following
 tasks: calculating the value of a function, using
 user-defined variables and handling errors.


 A. Calculating the value of a function
    -----------------------------------

 To calculate the value of a function, set the 
 ParseString property of your TMathParser component to
 the function that you wish to evaluate.  Then, call
 the component's Parse method and check for errors
 afterwards.  Finally, access the ParseValue field to
 read the numeric result of the function.  For 
 example:

 with MathParser1 do
 begin
   ParseString := 'Round(15.884 / 4.3333)'
   Parse;
   if not ParseError then
     MyResultEdit.Text := FloatToStrF(ParseValue, 
       ffGeneral, 15, 2)
   else
     MyResultEdit.Text := '#Error';
 end; 


 B. Using user-defined variables
    ----------------------------

 If you wish to use user-defined variables, create
 a handler for the TMathParser component's 
 OnGetVar event.  You will receive as a parameter
 the name of the variable being currently evaluated
 and you must return two values: if the variable
 exists and the value of the variable.  For 
 example:

  procedure TfrmMain.MathParserGetVar(Sender:
    TObject; VarName: String; var Value: Extended;
    var Found: Boolean);
  begin
    Found := True;
    VarName := Uppercase(VarName);
    if (VarName = 'DAYSPERWEEK') then
      Value := 7
    else if (VarName = 'SECONDSPERHOUR') then
      Value := 3600
    else if (VarName = 'AVGWEEKSPERMONTH') then
      Value := 4.333
    else
      Found := False;
  end;

 Notice that you can have case-sensitive or case-
 insensitive names for variables, depending on how
 you implement your event handler.


 C. Handling errors
    ---------------

 The TMathParser component does not display any
 error messages by default.  However, you can easily
 implement that functionality by creating a handler
 for the OnParseError event.  The following is an
 example of such an event handler, and the meaning
 of the values provided to you in the ParseError
 parameter.  Notice the use of the Position field of
 the component to tell the user where in the string
 the error ocurred.
 
  procedure TfrmMain.MathParserParseError(Sender: 
    TObject; ParseError: Integer);
  var
    Msg : string;
  begin
    case ParseError of
      1 : Msg := 'Parser stack overflow.';
      2 : Msg := 'Bad cell range.';
      3 : Msg := 'Expected expression.';
      4 : Msg := 'Expected operator.';
      5 : Msg := 'Expected opening parenthesis.';
      6 : Msg := 'Expected operator or closing '+
                 'parenthesis.';
    end; { case }
    Msg := Msg + ' Position in string: ' + 
      IntToStr(MathParser.Position);
    MessageDlg(Msg, mtError, [mbOk], 0);

    { Put the cursor at the position in the string
      where the error ocurred. }

    edtFunction.SelStart := 
      Pred(MathParser.Position);
    edtFunction.SelLength := 0;
  end;


 That's all there is to it!  By using the TMathParser
 component, you will be able to quickly and easily 
 add expression evaluation features to your 
 applications.  Take a look at the included
 demo application to see the math parser at work.

 Enjoy!


 4. Support, feedback and bugs
 -----------------------------
 
 We do not "oficially" provide support for this
 software.  However, if you have any questions about 
 it, please feel free to write to us at:

   tech-support@bitsoft.com

 We will try to answer your questions promptly.  If you
 have any suggestions for improvement, please let us 
 know also, as we will be more than happy to hear your
 opinions!

 If you find a bug in this component, please let us
 know by sending a description of the bug to:

   bugs@bitsoft.com

 We will correct all reported bugs as soon as possible.


 5. Other BitSoft products
 -------------------------

 We would really appreciate it if you visit our web site
 at:

   http://www.bitsoft.com

 and take a look at other Delphi products available in
 our site.  These currently include:

  - QrRotateLabel for QuickReports (16/32 bits)
    Add rotated text to your reports.  Supports
    any version of QuickReports.  $10.

 You will also find other freeware packages for Delphi
 in the "Delphi Free Stuff!" section.  So don't wait and
 take a look around our site now!


 6. Contacting BitSoft Development
 ---------------------------------

 You can contact BitSoft Development by mail at:

  BitSoft Development, L.L.C.
  P.O. Box 8483
  Moscow, Idaho  83843
  U.S.A.

 or by email at:

  info@bitsoft.com

 To report a bug, please send a complete description of 
 the bug and the circumstances under which it appears, 
 to:

  bugs@bitsoft.com

 You can also visit us on the web at:

  http://www.bitsoft.com

 We would also love to hear your opinions about our 
 products. If you have any comments or suggestions about 
 any of our products or services, please feel free to 
 write to us at:

  feedback@bitsoft.com


 7. Joining the BitSoft Delphi News List
 ---------------------------------------

 If you wish to receive information about updates to
 this product or about other Delphi related products
 from BitSoft, we invite you to join the BitSoft Delphi
 News list.  To join, simply send an e-mail message to:

   listserv@bitsoft.com

 with the following text in the body of the message:

   subscribe bsd-delphi-news

 The Subject line is ignored.

 Note: the list is a read-only list and for your
 privacy, your address in the list will not be visible
 to other users.


 8. Revision History
 -------------------

 10/15/97: Version 1.4

    -- Added MOD operator

 7/12/97: Version 1.3
 
    -- Renamed *.dcr files to Parser.d16 and Parser.d32
       and changed Parser.pas to use these files instead
    -- Updated the documentation
  
 7/07/97: Version 1.2

    -- Fixed bug in parser that returned a math error 
       instead of a token error when an invalid numeric
       expression was entered.

 7/04/97: Version 1.1

    -- Fixed some errors in the documentation

 6/25/97: Version 1.0

    -- Initial Release.
