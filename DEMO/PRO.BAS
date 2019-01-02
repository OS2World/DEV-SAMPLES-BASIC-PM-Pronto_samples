' PRONTO! PM demonstration program
'
' This source code is provided for documentation scope only
' use it as a reference for the demo program included
'
' (C) 1990 Artel Informatica Corp.
'
DECLARE SUB StringAssign(BYVAL SrcAdd&, BYVAL SrcLen%, BYVAL DstSeg%, BYVAL DstOff%, BYVAL DstLen%)
DECLARE FUNCTION StrMake$ (stradd&, strlen%)
DECLARE FUNCTION BasWndProc% (msg%, mp1&, mp2&)
DECLARE FUNCTION Dialog00& (hdlg&, msg%, mp1&, mp2&)
DECLARE FUNCTION Dialog01& (hdlg&, msg%, mp1&, mp2&)
DECLARE FUNCTION Dialog02& (hdlg&, msg%, mp1&, mp2&)
DECLARE SUB TestMenus ()
DECLARE SUB TestFile ()
DECLARE SUB TestXqprint ()
DECLARE SUB TestXqline ()
DECLARE SUB TextBox ()
DECLARE SUB ProcessCommand (c%)

'
' this file contains functions declaration
' for PRONTO! PM
' Also include are the Basic translations for
' various PM constant as in OS/2 PM SDK
'
REM $INCLUDE: 'PRONTO.INC'

DEFINT A-Z
'
' Initializations
'
DIM mallocbuf%(8192)

COMMON SHARED /NMALLOC/ mallocbuf%()
COMMON SHARED CursState%, ViewCoord%, Panel1%, Panel2%, vClock%, CurPointer
b% = 0
CursState% = 0
ViewCoord% = 0
Panel1 = 0
Panel2 = 0
vClock = 0
CurPointer = 0
x& = 0!
'
b% = ProntoPM
' Initialize work with Presentation Manager
b% = BStartPMWork
' Lets the dance begin
q% = BCreateWindow
' Terminate work with Presentation Manager
b% = BStopPMWork%
' That'all folks!
END

'
' Window Procedure
'
FUNCTION BasWndProc% (msg%, mp1&, mp2&) STATIC
BasWndProc% = 0
  SELECT CASE msg%
    CASE WMCREATE
      b% = BSetAccelTable(701)
      b% = BSetAppIcon(801)
      ' creation: set window title
      Titolo$ = "Artel PRONTO! PM Demo Program" + CHR$(0)
      b% = BSetWindowTitle%(SSEGADD(Titolo$))
      eb& = BMenu&(501, SSEGADD(Titolo$))
      b% = BCheckMenuItem(509, vClock)

    CASE WMMOUSEMOVE
    ' intercept mouse movements
      IF ViewCoord THEN
	CALL BreakLong(mp1&, x%, y%)
	text$ = "[" + STR$(x%) + " - " + STR$(y%) + "]" + CHR$(0)
	b = BXQprint(SSEGADD(text$), 3, 23, 0)
      END IF

    CASE MCENDWP
       ' this message is received when
       ' the user exit from the memo editor
       ' to read the content of the memo buffer you should
       ' use:
       b% = BGetWpBuffer%(StAdd&, StLen%)
       text$ = StrMake$(StAdd&, StLen%)

    CASE MCENDSINPT
      ' this message is received when the user
      ' exit from an input field operation
      b% = BGetSinpt%(StAdd&, StLen%)
      text$ = StrMake$(StAdd&, StLen%)
      text$ = text$ + CHR$(0)
      b = BXQprint(SSEGADD(text$), 10, 15, 0)
      CALL BreakLong(mp1&, hi%, lo%)
      ' loword contains the exit code as specified
      ' in pmbtool.cfg
      text$ = "1Exit code from input field: "+str$(lo%)+chr$(0)
      b% = BMessage(0, SSEGADD(text$), 1)

    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      Call ProcessCommand(loword%)

    CASE WMBUTTON1DOWN
      CALL BreakLong(mp1&, hiword%, loword%)

    CASE ELSE
      ' Default processing
      BasWndProc% = 0
  END SELECT
END FUNCTION


FUNCTION Dialog00& (hdlg&, msg%, mp1&, mp2&) STATIC
Dialog00& = 0
  SELECT CASE msg%
    CASE WMINITDLG
      text$ = "Jones & Jones inc." + CHR$(0)
      b = BWriteEditControl(hdlg&, 257, SSEGADD(text$))
      ' check box set
      b = BCheckCBox(hdlg&, 264)
      ' set focus on address field
      b = BSetFocusOnItem%(hdlg&, 258)
      Dialog00& = 1
    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      SELECT CASE loword%
	CASE 1
	  ' Ok pressed, closing dialog
	  b% = BReadEditControl(hdlg&, 257, StAdd&, StLen%)
	  text$ = StrMake$(StAdd&, StLen%)
	  ' read which radio button is selected
	  text$ = ""
	  IF BGetButtonState(hdlg&, 265) = 1 THEN
	    text$ = "3 months ?"
	  END IF
	  IF BGetButtonState(hdlg&, 266) = 1 THEN
	    text$ = "6 months ?"
	  END IF
	  IF BGetButtonState(hdlg&, 267) = 1 THEN
	    text$ = "12 months ?"
	  END IF
	  IF LEN(text$) = 0 THEN
	    text$ = "You don't select any contract"
	  END IF
	  text$ = "1" + text$ + CHR$(0)
	  b = BMessage(0, SSEGADD(text$), 5)
	  b = BWinBeep(2)
	  b = BEndDialog%(hdlg&, 1)
	  Dialog00& = 1
	CASE 2
	  ' Cancel dialog
	  b = BEndDialog%(hdlg&, 1)
	  Dialog00& = 1
	CASE 275
	  ' Pushed the "Next" Button
	  b = BDialog(279, 1)
	  Dialog00& = 1
	CASE ELSE
	  Dialog00& = 1
	END SELECT
    CASE ELSE
      ' Dont't bother about messages
      Dialog00& = 0
  END SELECT
END FUNCTION

FUNCTION Dialog01& (hdlg&, msg%, mp1&, mp2&) STATIC
Dialog01& = 0
  SELECT CASE msg%
    CASE WMINITDLG
      ' load list box with elements name
      a$ = "Hydrogen\Carbon\Oxygen\Nitrogen\Phosphor\Calcium\Uranium\Tecnetium"
      DO
	q = INSTR(a$, "\"): IF q = 0 THEN q = LEN(a$) + 1
	text$ = LEFT$(a$, q - 1) + CHR$(0): a$ = MID$(a$, q + 1)
	b = BAddListBoxEntry(hdlg&, 280, -1, SSEGADD(text$))
      LOOP UNTIL a$ = ""
      b = BSetFocusOnItem%(hdlg&, 280)
      Dialog01& = 1
    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      SELECT CASE loword%
	CASE 1
	  ' pushed on Ok
	  b = BWinBeep(2)
	  b = BEndDialog%(hdlg&, 1)
     Dialog01& = 1
      CASE ELSE
     Dialog01& = 1
      END SELECT
  CASE ELSE
    ' Dont't bother about messages
    Dialog01& = 0
  END SELECT
END FUNCTION

FUNCTION Dialog02& (hdlg&, msg%, mp1&, mp2&) STATIC
Dialog02& = 0
  SELECT CASE msg%
    CASE WMINITDLG
      Dialog02& = 1
      b = BSetFocusOnItem%(hdlg&, 1)
    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      SELECT CASE loword%
	CASE 1
	  b = BWinBeep(3)
	  b = BEndDialog%(hdlg&, 1)
	  Dialog02& = 1
	CASE ELSE
	  Dialog02& = 1
      END SELECT
  CASE ELSE
    Dialog02& = 0
  END SELECT
END FUNCTION


SUB TestFile
' This SUB test the directory function
'
' N.B. the double backslash
' in the path$ string
' is necessary because
' the single backslash is the
' start character of an escape sequence in C
path$ = "C:\\" + CHR$(0)
extext$ = "*.*" + CHR$(0)
b% = BGetDir(0, SSEGADD(path$), SSEGADD(extext$), StAdd&, StLen%)
szFile$ = StrMake$(StAdd&, StLen%)
IF LEN(szFile$) > 0 THEN
  Conf$ = "1File selected\" + szFile$ + CHR$(0)
ELSE
  Conf$ = "1No file selected" + CHR$(0)
END IF
b% = BMessage%(0, SSEGADD(Conf$), 4)
END SUB

SUB TestMenus
'
' This SUB loads a user menu with the string text$
' then waits for a selection and display the result
'
text$ = "One\Two\Three\Four\Five\Six\Seven\Eight\Nine\Ten\Eleven\Twelve" + CHR$(0)
q = BMenuSelect(SSEGADD(text$), 10, 10)
text$ = "1Choosed: " + STR$(q) + CHR$(0)
b% = BMessage%(0, SSEGADD(text$), 2)
END SUB

SUB TestXqline
'
' This SUB draw a series of lines using the first six colors
' from pmbtool.cfg (see)
'
b = BCls
b = BChangePointer(1)
for i = 1 to 39
   b = BXLine(2*i,  2, 40, 12, 1, (i MOD 6)+1, 0)
   b = BXLine(2*i, 23, 40, 12, 1, (i MOD 6)+1, 0)
next
b = BXLine(2, 2, 78, 23, 128, 1, 0)
text$ = "PRONTO! PM"+CHR$(0)
b = BXQprint(SSEGADD(text$), 35, 12, 1)
text$ = "is the faster way to PM for Basic..."+CHR$(0)
b = BXQprint(SSEGADD(text$), 35, 13, 3)
b = BChangePointer(1)
END SUB

SUB TestXqprint
'
' This SUB reads the first 22 lines of text from the file
' PRONTO.TXT and writes them to the screen
' file format is as follows:
' "text", column, row, color index
'
b = BCls
f = freefile
j = 0
open "PRONTO.TXT" for input as f
do while not eof(f) and j < 22
    input #f, text$, x, y, c
    j = j+1
    text$ = text$+chr$(0)
    b = BXqprint(SSEGADD(text$), x, y, c)
loop
close f
END SUB

SUB TestBox
'
' This SUB draws some box on the screen using colors 1 - 6
' then after a delay, clears them and beeps.
' Note the pointer change during the delay phase.
'
b = BCls
for i = 1 to 19
   b = BXLine(2*i,  i, (2*i)+12, i+3, 1, (i MOD 6)+1, 1)
next
text$ = "PRONTO! PM"+CHR$(0)
b = BXQprint(SSEGADD(text$), 39, 20, 1)
Call Delay(500)
for i = 1 to 19
   b = BWinBeep(2)
   b = BXqClear(2*i, i)
   Call Delay(50)
next
b = BXQClear(39, 20)
b = BWinBeep(3)
END SUB

SUB Delay(HowMuch)
'
' This SUB creates a delay proportional to the
' "HowMuch" value
' During delay phase, the pointer is changed to hourglass
' and the restored as an arrow
'
b = BChangePointer(1)
for j = 0 to HowMuch
   q# = sin(j)+cos(j)
next j
b = BChangePointer(0)
END SUB

SUB ProcessCommand(cmd%)
'
' This SUB processes commands received
' from the user's menu selection
SELECT CASE cmd%
   CASE 503
      ' File...
      CALL TestFile
   CASE 504
      ' User menu
      CALL TestMenus
   CASE 505
      ' Other menu
      e& = BMenu&(601, SSEGADD(Titolo$))
   CASE 506
      ' Print Test
      CALL TestXqprint
   CASE 507
      ' Line test
      CALL TestXqline
   CASE 527
      'Box Test
      CALL TestBox
   CASE 508
      ' Clear screen
      b% = BCls
   CASE 509
      ' Clock
      IF vClock = 0 THEN
	 vClock = 1
      ELSE
	 vClock = 0
      END IF
      b% = BShowTime(vClock)
      b% = BCheckMenuItem(509, vClock)
   CASE 512
      ' First dialog box
      b% = BDialog(256, 0)
   CASE 513
      ' Second dialog box
      b% = BDialog(279, 1)
   CASE 517
      ' third dialog box
      b% = BDialog(400, 2)
   CASE 514
      ' Cursor
      b% = BLocate(10, 10)
      IF CursState = 0 THEN
	 CursState = 1
      ELSE
	 CursState = 0
      END IF
      b% = BSetCursor(CursState)
      b% = BCheckMenuItem(514, CursState)
   CASE 528
      ' Input field
      text$ = "Input Test" + CHR$(0)
      b% = BSinpt(SSEGADD(text$), 10, 15, 10, 20)
   CASE 515
      ' Memo editor
      text$ = "To invoke the memo editor use the bCreateWp call."+chr$(13)
      text$ = text$+"You can easily set and read the contents of the "
      text$ = text$+"memo buffer with bSetWpBuffer and bGetWpBuffer "
      text$ = text$+"functions."+chr$(13)
      text$ = text$+"Remember that input field and memo functions allows "
      text$ = text$+"your application to exchange data to and from other "
      text$ = text$+"applications using the Clipboard!"+chr$(0)
      b% = BSetWpBuffer(SSEGADD(text$))
      b% = BCreateWp
   CASE 516
      ' Change pointer
      IF CurPointer = 0 THEN
	 CurPointer = 1
      ELSE
	 CurPointer = 0
      END IF
      b% = BChangePointer(CurPointer)
      b% = BCheckMenuItem(516, CurPointer)
   CASE 522
      ' Coordinates
      IF ViewCoord = 0 THEN
	 ViewCoord = 1
      ELSE
	 ViewCoord = 0
      END IF
      b = BCheckMenuItem(515, ViewCoord)
   CASE 523
      ' Text output
      b% = BCls%
      text$ = "Hello" + CHR$(0)
      FOR i = 4 TO 22
	b% = BXLine(50, i, 55, i + 1, 129, 0, 0)
	b% = BXQprint(SSEGADD(text$), 51, i, 1)
      NEXT i
      b% = BXLine(30, 10, 36, 12, 129, 0, 0)
      b% = BXQprint(SSEGADD(text$), 31, 11, 1)
   CASE 524
      ' Torture test
      text$ = "Start Torture Test" + CHR$(0)
      b% = BXQprint(SSEGADD(text$), 0, 1, 1)
      FOR i% = 1 TO 1000
	 text$ = "Torture #. " + STR$(i%) + CHR$(0)
	 xcol% = (i% MOD 6) + 1
	 b% = BXQprint(SSEGADD(text$), 60, (i% MOD 15) + 1, xcol%)
      NEXT i%
      text$ = "End Torture Test" + CHR$(0)
      b% = BXQprint(SSEGADD(text$), 60, 23, 0)
      b% = BWinBeep(1)
   CASE 525
      ' Panel 1
      IF Panel1% = 0 THEN
	 b% = BChildBox(10, 10, 50, 15, 0, 1)
	 text$ = "This is a panel" + CHR$(0)
	 b% = BXQprint(SSEGADD(text$), 11, 11, 0)
	 Panel1% = 1
      ELSE
	 b% = BPopDList
	 Panel1% = 0
      END IF
      b% = BCheckMenuItem(525, Panel1%)
   CASE 526
      ' Panel 2
      IF Panel2% = 0 THEN
	 b% = BChildBox(14, 12, 60, 18, 7, 1)
	 text$ = "This is a panel" + CHR$(0)
	 b = BXQprint(SSEGADD(text$), 15, 13, 0)
	 Panel2% = 1
      ELSE
	 b% = BPopDList
	 Panel2% = 0
     END IF
     b% = BCheckMenuItem(526, Panel2)
   '
   ' second menu
   '
   CASE 603
      ' Confirm
      Conf$ = "3There are 3 buttons." + CHR$(0)
      b% = BMessage%(0, SSEGADD(Conf$), 3)
   CASE 604
      ' Error
      text$ = "Musk presence in drive C:" + CHR$(0)
      b% = BError(SSEGADD(text$), 4242, 1)
   CASE 605
      ' Back to first menu
      e& = BMenu&(501, SSEGADD(text$))
END SELECT
END SUB

FUNCTION StrMake$(stradd&, strlen%)
    Call StringAssign(stradd&, strlen%, VARSEG(S$), VARPTR(S$), 0)
    StrMake$ = S$
END FUNCTION
