REM $INCLUDE: 'PRONTO.INC'

DEFINT A-Z

CONST EOFTYPE = 0, FILETYPE = 1, DIRTYPE = 2, ROOT = "TWH"

DECLARE SUB StringAssign(BYVAL SrcAdd&, BYVAL SrcLen%, BYVAL DstSeg%, BYVAL DstOff%, BYVAL DstLen%)
DECLARE FUNCTION StrMake$ (stradd&, strlen%)
DECLARE FUNCTION BasWndProc% (msg%, mp1&, mp2&)
DECLARE FUNCTION Dialog00& (hdlg&, msg%, mp1&, mp2&)
DECLARE FUNCTION Dialog01& (hdlg&, msg%, mp1&, mp2&)
DECLARE SUB ScanDir (PathSpec$, Level, FileSpec$, Row, hdlg&)
DECLARE FUNCTION MakeFileName$ (Num)
DECLARE FUNCTION GetEntry$ (FileNum, EntryType)
DECLARE FUNCTION IsLeapYear% (N%)
DECLARE FUNCTION GetInput% (Prompt$, Row%, LowVal%, HighVal%)
DECLARE SUB PrintCalendar (Year%, Month%)
DECLARE SUB ComputeMonth (Year%, Month%, StartDay%, TotalDays%)
DECLARE SUB myPrint (aStr$, aCol%, aRow%, aClr%)

' Define a data type for the names of the months and the
' number of days in each:
TYPE MonthType
	Number AS INTEGER  ' Number of days in the month
	MName AS STRING * 9   ' Name  of the month
END TYPE


DIM MonthData(1 TO 12)   AS MonthType
'
' Initializations
'
DIM mallocbuf%(8192)

COMMON SHARED /NMALLOC/ mallocbuf%()
COMMON SHARED pathspec$, year$, month$

' Initialize month definitions from DATA statements below:
FOR I = 1 TO 12
	READ MonthData(I).MName, MonthData(I).Number
NEXT

'
b% = ProntoPM
b% = BStartPMWork
q% = BCreateWindow
b% = BStopPMWork%
END

'
' Window Procedure
'
FUNCTION BasWndProc% (msg%, mp1&, mp2&) STATIC
BasWndProc% = 0
  SELECT CASE msg%
    CASE WMCREATE
      b% = BSetAccelTable%(701)
      eb& = BMenu&(501, SSEGADD(Titolo$))

    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      Call ProcessCommand(loword%)

    CASE ELSE
      ' Default processing
      BasWndProc% = 0
  END SELECT
END FUNCTION

SUB ProcessCommand(Item%)
  SELECT CASE Item%
    CASE 503
      b% = BDialog(256, 0)
    CASE 504
      b% = BDialog(263, 1)
    CASE 505
      b% = BDialog(262, 2)
  END SELECT
END SUB

'IDD_SRCFILE   260
'IDD_SRCPATH   259
'IDD_SEARCH    258
'IDD_MATCHES   257
'SEARCH_FILE   256
'IDD_SEARCHING 261

FUNCTION Dialog00& (hdlg&, msg%, mp1&, mp2&) STATIC
Dialog00& = 0
  SELECT CASE msg%
    CASE WMINITDLG
      PathSpec$ = Curdir$ + chr$(0)
      FileSpec$ = "" + chr$(0)
      b = BWriteEditControl(hdlg&, 259, SSEGADD(PathSpec$))
      b = BWriteEditControl(hdlg&, 260, SSEGADD(FileSpec$))
      b = BSetFocusOnItem%(hdlg&, 259)
      Dialog00& = 1
    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      SELECT CASE loword%
	CASE 2
	  b = BEndDialog%(hdlg&, 1)
	  Dialog00& = 1
	CASE 258
	  ' Pushed the "Search" Button
	  b = BChangePointer(1)
	  b = BReadEditControl(hdlg&, 259, StAdd&, StLen%)
	  PathSpec$ = StrMake$(StAdd&, StLen%)

	  b = BReadEditControl(hdlg&, 260, StAdd&, StLen%)
	  FileSpec$ = StrMake$(StAdd&, StLen%)

	  RightCh$ = RIGHT$(PathSpec$, 1)

	  IF PathSpec$ = "" OR RightCh$ = ":" OR RightCh$ <> "\" THEN
	    PathSpec$ = PathSpec$ + "\"
	  END IF

	  FileSpec$ = UCASE$(FileSpec$)
	  PathSpec$ = UCASE$(PathSpec$)
	  Level = 1
	  Row = 3

	  ' Make the top level call (level 1) to begin the search:
	  ScanDir PathSpec$, Level, FileSpec$, Row, hdlg&

	  KILL ROOT + ".*"  ' Delete all temporary files created
			    ' by the program.

	  b = BChangePointer(0)
	  Dialog00& = 1
	CASE ELSE
	  Dialog00& = 1
	END SELECT
    CASE ELSE
      ' Dont't bother about messages
      Dialog00& = 0
  END SELECT
END FUNCTION


' IDD_CALMONTH	265
' IDD_CALYEAR   264
' CALENDAR      263
FUNCTION Dialog01& (hdlg&, msg%, mp1&, mp2&) STATIC
Dialog01& = 0
  SELECT CASE msg%
    CASE WMINITDLG
      year$ = right$(date$, 4) + chr$(0)
      month$ = left$(date$, 2) + chr$(0)
      b = BWriteEditControl(hdlg&, 264, SSEGADD(year$))
      b = BWriteEditControl(hdlg&, 265, SSEGADD(month$))
      Dialog01& = 1
      b = BSetFocusOnItem%(hdlg&, 1)
    CASE WMCOMMAND
      CALL BreakLong(mp1&, hiword%, loword%)
      SELECT CASE loword%
	CASE 1
	  PrintCalendar Val(year$), Val(month$)
	  b = BEndDialog%(hdlg&, 1)
	  Dialog01& = 1
	CASE 2
	  b = BEndDialog%(hdlg&, 0)
	  Dialog01& = 1
	CASE ELSE
	  Dialog01& = 1
      END SELECT
  CASE ELSE
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

' ======================= GETENTRY ========================
'    This procedure processes entry lines in a DIR listing
'    saved to a file.
'    This procedure returns the following values:
'  GetEntry$   A valid file or directory name
'  EntryType   If equal to 1, then GetEntry$
'        is a file.
'        If equal to 2, then GetEntry$
'        is a directory.
' =========================================================
FUNCTION GetEntry$ (FileNum, EntryType) STATIC
' Loop until a valid entry or end-of-file (EOF) is read:
DO UNTIL EOF(FileNum)
  LINE INPUT #FileNum, EntryLine$
  IF EntryLine$ <> "" THEN
    ' Get first character from the line for test:
    TestCh$ = LEFT$(EntryLine$, 1)
    IF TestCh$ <> " " AND TestCh$ <> "." THEN EXIT DO
  END IF
LOOP

' Entry or EOF found, decide which:
IF EOF(FileNum) THEN	' EOF, so return EOFTYPE
  EntryType = EOFTYPE  ' in EntryType.
  GetEntry$ = ""
ELSE ' Not EOF, so it must be a
     ' file or a directory.
     ' Build and return the entry name:
  EntryName$ = RTRIM$(LEFT$(EntryLine$, 8))
  ' Test for extension and add to name if there is one:
  EntryExt$ = RTRIM$(MID$(EntryLine$, 10, 3))
  IF EntryExt$ <> "" THEN
    GetEntry$ = EntryName$ + "." + EntryExt$
  ELSE
    GetEntry$ = EntryName$
  END IF
  ' Determine the entry type, and return that value
  ' to the point where GetEntry$ was called:
  IF MID$(EntryLine$, 15, 3) = "DIR" THEN
    EntryType = DIRTYPE   ' Directory
  ELSE
    EntryType = FILETYPE  ' File
  END IF
END IF
END FUNCTION

' ===================== MAKEFILENAME$ =====================
'    This procedure makes a file name from a root string
'    ("TWH," defined as a symbolic constant at the module
'    level) and a number passed to it as an argument (Num).
' =========================================================
FUNCTION MakeFileName$ (Num) STATIC
  MakeFileName$ = ROOT + "." + LTRIM$(STR$(Num))
END FUNCTION

' ======================= SCANDIR =========================
'   This procedure recursively scans a directory for the
'   file name entered by the user.
'   NOTE: The SUB header doesn't use the STATIC keyword
'         since this procedure needs a new set of variables
'         each time it is invoked.
' =========================================================
SUB ScanDir (PathSpec$, Level, FileSpec$, Row, hdlg&)

  text$ = PathSpec$+chr$(0)
  b = BWriteEditControl(hdlg&, 261, SSEGADD(text$))

  ' Make a file specification for the temporary file:
  TempSpec$ = MakeFileName$(Level)

  ' Get a directory listing of the current directory,
  ' and save it in the temporary file:
  SHELL "DIR " + PathSpec$ + " > " + TempSpec$

  ' Get the next available file number:
  FileNum = FREEFILE

  ' Open the DIR listing file and scan it:
  OPEN TempSpec$ FOR INPUT AS #FileNum
  ' Process the file, one line at a time:
  DO
    ' Input an entry from the DIR listing file:
    DirEntry$ = GetEntry$(FileNum, EntryType)
    ' If entry is a file:
    IF EntryType = FILETYPE THEN
      ' If the FileSpec$ string matches,
      ' print entry and exit this loop:
      IF DirEntry$ = FileSpec$ THEN
	'add list box entry
	text$ = PathSpec$+DirEntry$+chr$(0)
	b% = BAddListBoxEntry(hdlg&, 257, -1, SSEGADD(text$))
	EntryType = EOFTYPE
      END IF
      ' If the entry is a directory, then make a recursive
      ' call to ScanDir with the new directory:
      ELSEIF EntryType = DIRTYPE THEN
	NewPath$ = PathSpec$ + DirEntry$ + "\"
	  ScanDir NewPath$, Level + 1, FileSpec$, Row, hdlg&
      END IF
  LOOP UNTIL EntryType = EOFTYPE
  ' Scan on this DIR listing file is finished, so close it:
  CLOSE FileNum
END SUB

FUNCTION StrMake$(stradd&, strlen%)
    Call StringAssign(stradd&, strlen%, VARSEG(S$), VARPTR(S$), 0)
    StrMake$ = S$
END FUNCTION

' ====================== COMPUTEMONTH =====================
'  Computes the first day and the total days in a month
' =========================================================
'
SUB ComputeMonth (Year, Month, StartDay, TotalDays) STATIC
	SHARED MonthData() AS MonthType

	CONST LEAP = 366 MOD 7
	CONST NORMAL = 365 MOD 7

	' Calculate total number of days (NumDays) since 1/1/1899:

	' Start with whole years:
	NumDays = 0
	FOR I = 1899 TO Year - 1
		IF IsLeapYear(I) THEN              ' If leap year,
			NumDays = NumDays + LEAP   ' add 366 MOD 7.
		ELSE                               ' If normal year,
			NumDays = NumDays + NORMAL ' add 365 MOD 7.
		END IF
	NEXT

	' Next, add in days from whole months:
	FOR I = 1 TO Month - 1
		NumDays = NumDays + MonthData(I).Number
	NEXT

	' Set the number of days in the requested month:
	TotalDays = MonthData(Month).Number

	' Compensate if requested year is a leap year:
	IF IsLeapYear(Year) THEN

		' If after February, add one to total days:
		IF Month > 2 THEN
			NumDays = NumDays + 1

		' If February, add one to the month's days:
		ELSEIF Month = 2 THEN
			TotalDays = TotalDays + 1
		END IF
	END IF

	' 1/1/1899 was a Sunday, so calculating "NumDays MOD 7"
	' gives the day of week (Sunday = 0, Monday = 1, Tuesday
	' = 2, and so on) for the first day of the input month:
	StartDay = NumDays MOD 7
END SUB

' ====================== ISLEAPYEAR =======================
'   Determines if a year is a leap year or not
' =========================================================
'
FUNCTION IsLeapYear (N) STATIC

	' If the year is evenly divisible by 4 and not divisible
	' by 100, or if the year is evenly divisible by 400,
	' then it's a leap year:
	IsLeapYear = (N MOD 4 = 0 AND N MOD 100 <> 0) OR (N MOD 400 = 0)
END FUNCTION

' ===================== PRINTCALENDAR =====================
'   Prints a formatted calendar given the year and month
' =========================================================
'
SUB PrintCalendar (Year, Month) STATIC
SHARED MonthData() AS MonthType

	CurRow = 1
	LeftMargin = 1

	' Compute starting day (Su M Tu ...)
	' and total days for the month:
	ComputeMonth Year, Month, StartDay, TotalDays
	b% = Bcls
	Header$ = RTRIM$(MonthData(Month).MName) + "," + STR$(Year)

	' Calculate location for centering month and year:
	LeftMargin = (35 - LEN(Header$)) \ 2
' Print header:
	text$ = space$(LeftMargin)+Header$+chr$(0)
	myPrint text$, LeftMargin, CurRow, 1
	LeftMargin = 1
	CurRow = 3
	myPRINT "Su", 1, CurRow, 1
	myPRINT "M",  6, CurRow, 1
	myPRINT "Tu", 11, CurRow, 1
	myPRINT "W",  16, CurRow, 1
	myPRINT "Th", 21, CurRow, 1
	myPRINT "F",  26, CurRow, 1
	myPRINT "Sa", 31, CurRow, 1
	' Recalculate and print tab
	' to the first day of the month (Su M Tu ...):
	LeftMargin = 5 * StartDay + 1
	CurRow = CurRow + 1

	' Print out the days of the month:
	FOR I = 1 TO TotalDays
		myPRINT right$("  "+str$(i),2), LeftMargin, CurRow, 1
		LeftMargin = LeftMargin+5
		IF LeftMargin > 32 THEN
		  CurRow = CurRow + 1
		  LeftMargin = 1
		end if
	NEXT

END SUB

SUB myPrint(aStr$, aCol%, aRow%, aClr%)
    text$ = aStr$ + chr$(0)
    b% = Bxqprint(SSEGADD(text$), aCol%, aRow%, aClr)
END SUB

' Data for the months of a year:
DATA January, 31, February, 28,  March, 31
DATA April, 30,   May, 31, June, 30, July, 31, August, 31
DATA September,   30, October, 31, November, 30, December, 31
