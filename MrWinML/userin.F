CCCCCCCCCCCCCCCCCCCCCCCC LUXDATIM CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

*     PROGRAM TESTDATE
*     CHARACTER YEAR*4,MONTH*9,WEEKD*9,DAYIM*4,TIME*8
*     INTEGER*4 IDATE(6)
*
*     CALL LDATIM(IDATE,YEAR,MONTH,WEEKD,DAYIM,TIME)
*     WRITE(6,61) WEEKD,DAYIM,MONTH,YEAR,TIME
*
*  61 FORMAT('0',9X,60('*')/10X,'*',58X,'*'/
*    -       10X,'*',15X,' P R O G R A M   M A G R E S ',14X,'*'/
*    -       10X,'*',58X,'*'/10X,60('*')/'0'/'0'/'0'/
*    -       '0',15X,'DATE:  ',A,',  ',A,' OF ',A,',  ',A/
*    -       '0',15X,'TIME:  ',A)
*     END
*

*-----------------------------------------------------------------------
      SUBROUTINE SDATIM(IDATUM)
*-----------------------------------------------------------------------
*
*     A SUBROUTINE WITH TIMING BELLS AND WHISTLES
*
*     ENTRY                  SHORT DESCRIPTION
*     -----                  -----------------
*
*     SDATIM (ITSELF)        SHORT INFO OF DATE AND TIME
*     LDATIM                 EXTENDED DATE AND TIME INFO
*     SETCPT                 CREATE A CPU INTERVAL TIMER
*     GETCPT                 USE A CPU INTERVAL TIMER
*     SETLAP                 CREATE AN INTERVAL TIMER FOR ELAPSED REAL
*     GETLAP                 USE    ,,    ,,     ,,  ,,  ,, ,, ,,(TIME
*
*
*     USAGE NOTES:
*     ------------
*
*     SDATIM HAS ONE PARAMETER: 6 INTEGERS*4 RECEIVING DATE AND TIME AS
*                                     YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
*
*
*     LDATIM HAS THE SAME BUT ALSO PARAMETERS FOR RECEIVING CHARACTERS:
*
*       LDATIM(IDATUM,YEAR,MONTH,WEEKDAY,NUMDAY,TIME)
*        IDATUM:       6  INTEGERS*4
*        YEAR,NUMDAY:    4 CHARS    (E.G. "1982" ; " 3RD")
*        MONTH,WEEKDAY:  9 CHARS    (E.G. "MAY      ";"WEDNESDAY")
*        TIME:           8 CHARS    (E.G. " 2:15:03" ; "23:01:30")
*          (THE MAXIMUM NUMBER OF SIGNIFICANT BYTES IS INDICATED,
*           THE CHARACTER ARRAYS ARE RECEIVED AS CHARACTER*(*)
*           RIGHT-PADDING WITH BLANKS IS PERFORMED IF NECESSARY)
*
*     SETCPT AND SETLAP HAVE NO PARAMETERS.
*     GETCPT HAS ONE REAL*8 PARAMETER: THE USED CPU TIME IN SECONDS
*         (2 DIGITS AFTER THE DECIMAL POINT ARE SIGNIFICANT)
*     GETLAP HAS THREE INTEGER*4 PARAMETERS: HOURS,MINUTES,SECONDS
*
*     THE INTERVAL TIME IS THE TIME BETWEEN THE CALL TO SET...
*       AND THE MATCHING GET...   (CF. BRACKETS).
*     LET IT BE NOTED: AN OUTER PAIR OF SET... & GET... ALWAYS
*       GETS A VALUE GREATER OR EQUAL THE VALUE OF AN INNER PAIR.
*
*     FOR BOTH CLASSES THE PAIRS CAN BE NESTED IN UPTO STKLIM LEVELS;
*       NESTING PAIRS TOO DEEP OR CALLING GETCPT OR GETLAP WITHOUT
*       MATCHING PRIOR CALL TO SETCPT OR SETLAP WILL RESULT IN AN ERROR
*       MESSAGE ON UNIT 6 AND DISABLING THE CLASS OF INTERVAL TIMERS.
*
*
*
*     ************************************
      IMPLICIT INTEGER*4(A-P,R-Z),LOGICAL*4(Q)
*     ************************************

      PARAMETER (STKLIM=20)

      EQUIVALENCE (BUFF,BUFF4,BUFF8)
      CHARACTER*1 BUFF(8),DIGIT(0:9)
      CHARACTER*4 BUFF4,DAY(31)
      CHARACTER*8 BUFF8
      CHARACTER*9 WEEKDA(0:6),MONTH(12)
*     INTEGER*2 IYR,IMON,IDY,IHR,IMIN,ISEC,I100TH

*     ARGUMENTS:
      INTEGER*4 IDATUM(6)
      INTEGER*4 ITIM(3)
      INTEGER*4 IDAT(3)
      CHARACTER*(*) IWKD,MONT,YEAR,TIME,IDAY
      REAL*8 SECS

      DIMENSION CPUTIM(STKLIM),DAYTIM(STKLIM)
*       (STACKS FOR CPU TIMERS AND FOR REAL TIMERS)
      SAVE CPUTIM,DAYTIM,SPCPUT,SPLAPS
      DATA SPCPUT /0/, SPLAPS /0/
*       (STACK POINTERS)

      DATA Q_ERRC/.FALSE./, Q_ERRL/.FALSE./

      DATA DIGIT /'0','1','2','3','4','5','6','7','8','9'/
      DATA MONTH /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     1            'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     2            'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAY /' 1ST',' 2ND',' 3RD',' 4TH',' 5TH',
     1          ' 6TH',' 7TH',' 8TH',' 9TH','10TH',
     2          '11TH','12TH','13TH','14TH','15TH',
     3          '16TH','17TH','18TH','19TH','20TH',
     4          '21ST','22ND','23RD','24TH','25TH',
     5          '26TH','27TH','28TH','29TH','30TH','31ST'/
      DATA WEEKDA /'SUNDAY   ','MONDAY   ','TUESDAY  ','WEDNESDAY',
     2             'THURSDAY ','FRIDAY   ','SATURDAY '/

*     CALL GETDAT(IYR,IMON,IDY)
      CALL IDATE(IDAT)
*     CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      CALL ITIME(ITIM)

      IDATUM(1) = IDAT(3)
      IDATUM(2) = IDAT(2)
      IDATUM(3) = IDAT(1)
      IDATUM(4) = ITIM(1)
      IDATUM(5) = ITIM(2)
      IDATUM(6) = ITIM(3)

*     CALL DATIM(IDATE(1),IDATE(2),IDATE(3),IDATE(4),IDATE(5),IDATE(6),
*    1           IDUMMY)
      RETURN

*****
      ENTRY LDATIM(IDATUM,YEAR,MONT,IWKD,IDAY,TIME)
*****
*     CALL DATIM(YE,MO,DA,HO,MI,SE,SINC71)
*     CALL GETDAT(IYR,IMON,IDY)
      CALL IDATE(IDAT)
*     CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      CALL ITIME(ITIM)

      IDATUM(1) = IDAT(3)
      IYR       = IDAT(3)
      IDATUM(2) = IDAT(2)
      IMON      = IDAT(2)
      IDATUM(3) = IDAT(1)
      IDY       = IDAT(1)
      IDATUM(4) = ITIM(1)
      IHR       = ITIM(1)
      IDATUM(5) = ITIM(2)
      IMIN      = ITIM(2)
      IDATUM(6) = ITIM(3)
      ISEC      = ITIM(3)

      BREAK = IYR
      DO 1983 J = 4,1,-1
      P10 = BREAK / 10
      R10 = BREAK - 10*P10
      BUFF(J) = DIGIT(R10)
      BREAK = P10
 1983 CONTINUE
      YEAR = BUFF4
      MONT = MONTH(IMON)
      IDAY = DAY(IDY)

      P10 = IHR/ 10
      R10 = IHR- 10*P10
      BUFF(1) =  DIGIT(P10)
      IF(P10.EQ.0) BUFF(1) = ' '
      BUFF(2) = DIGIT(R10)
      BUFF(3) = ':'
      P10 = IMIN/10
      R10 = IMIN-10*P10
      BUFF(4) = DIGIT(P10)
      BUFF(5) = DIGIT(R10)
      BUFF(6) = ':'
      P10 = ISEC/10
      R10 = ISEC-10*P10
      BUFF(7) = DIGIT(P10)
      BUFF(8) = DIGIT(R10)
      TIME = BUFF8

*
*  The following formula calculates which day of the week corresponds
*  to January 1 of the given year
*
      CENTURY = IYR/100
      YEARS = IYR - CENTURY*100 - 1
      J1 = 29 + YEARS + YEARS/4 + CENTURY/4 - CENTURY*2
      J1 = J1 - ( J1 / 7 ) * 7 + 1
      IF( J1 .LE. 0 ) J1 = J1 + 7
*
      IWKD = WEEKDA(J1)
      RETURN
*
*****
      ENTRY SETCPT

      IF(Q_ERRC) RETURN
      SPCPUT = SPCPUT + 1
      IF(SPCPUT.GT.STKLIM) GOTO 11
CCCCC CALL CPUSED(CPUTIM(SPCPUT))
      CALL CLOCK (CPUTIM(SPCPUT))
      RETURN
   11 CONTINUE
      Q_ERRC = .TRUE.
      !write(6,61) STKLIM
*   61 ! FORMAT(//' ****** ERROR: TOO MANY CALLS TO SETCPT'/
      !1 8X,'(STACK OVERFLOW; STACK LENGTH = ',I2,' )'/
      !2 8X,'INTERVAL CPU TIMERS DISABLED, EXECUTION CONTINUING.'//)
      RETURN


      ENTRY GETCPT (SECS)

      SECS = 0D0
      IF(Q_ERRC) RETURN
      IF(SPCPUT.LT.1) GOTO 22
CCCCC CALL CPUSED(CPUNOW)
      CALL CLOCK(CPUNOW)
      SECS = 0.01D0 * DFLOAT(CPUNOW-CPUTIM(SPCPUT))
      SPCPUT = SPCPUT - 1
      RETURN
   22 CONTINUE
      Q_ERRC = .TRUE.
      !write(6,62)
*   62 !FORMAT(//' ****** ERROR: MORE CALLS MADE TO GETCPT THAN TO ',
      !1'SETCPT'/8X,'(STACK UNDERFLOW)'/8X,
      !2'INTERVAL CPU TIMERS DISABLED, EXECUTION CONTINUING.'//)
      RETURN

*****
      ENTRY SETLAP
*****
      IF(Q_ERRL) RETURN
      SPLAPS = SPLAPS + 1
      IF(SPLAPS.GT.STKLIM) GOTO 44
*...      CALL CLOCK(COUNTS)
      CALL ITIME(ITIM)
*...      DAYTIM(SPLAPS) = COUNTS / 100
      DAYTIM(SPLAPS) = ITIM(1)*3600 + ITIM(2)*60 + ITIM(1)
      RETURN
   44 CONTINUE
      Q_ERRL = .TRUE.
      !write(6,64) STKLIM
*   64 !FORMAT(//' ****** ERROR: TOO MANY CALLS TO SETLAP'/
      !1 8X,'(STACK OVERFLOW; STACK LENGTH = ',I2,' )'/
      !2 8X,'INTERVAL REAL TIMERS DISABLED, EXECUTION CONTINUING.'//)
      RETURN

*****
      ENTRY GETLAP (H,M,S)
*****
      H = 0
      M = 0
      S = 0
      IF(Q_ERRL) RETURN
      IF(SPLAPS.LT.1) GOTO 55
*...      CALL CLOCK(TIMNOW)
      CALL ITIME(ITIM)
*...      ELAPSD = TIMNOW/100 - DAYTIM(SPLAPS)
      ELAPSD = ITIM(1)*3600 + ITIM(2)*60 + ITIM(3)
      ELAPSD = ELAPSD - DAYTIM(SPLAPS)
      MM = ELAPSD / 60
      S = ELAPSD - 60*MM
      H = MM / 60
      M = MM - 60*H
      SPLAPS = SPLAPS - 1
      RETURN
   55 CONTINUE
      Q_ERRL = .TRUE.
      !write(6,65)
*   65 !FORMAT(//' ****** ERROR: MORE CALLS MADE TO GETLAP THAN TO ',
      !1'SETLAP'/8X,'(STACK UNDERFLOW)'/
      !2 8X,'INTERVAL REAL TIMERS DISABLED, EXECUTION CONTINUING.'//)
      RETURN

      END

      SUBROUTINE CLOCK(COUNTS)
      INTEGER COUNTS
      DIMENSION TARRAY(2)
      TOTAL = ETIME(TARRAY)
      COUNTS = TARRAY(1) * 100
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC USRINPUT.FOR CCCCCCCCCCCCCCCCCCCCCCCCCCC

* THIS FILE CONTAINS THE SUBROUTINES USERIN,READIR AND DICTIO
*-----------------------------------------------------------------------
      SUBROUTINE USERIN (NWORDS)
*-----------------------------------------------------------------------
*
*     THIS GENERAL PURPOSE SUBROUTINE INTERFACES TO READIR, SUPPORTING
*     USER-FRIENDLY INPUT. (REFER TO READIR FOR DETAILS.)
*
*     CALLS: (LIBRARY) ROUTINES READIR(+ENTRIES) , FTXFYY AND CMD2
*
*     ALL INPUT LINES ARE ECHOED AS THEY ARE ON UNIT 6.
*
*     IT CAUSES BLANK LINES AND LINES CONSISTING OF COMMENT ONLY
*     TO BE SKIPPED.
*
*     A LINE CONSISTING OF THE WORD "STOP" CAUSES A STOP OF EXECUTION.
*
*     WHEN A LINE (CONTAINING AT LEAST 2 WORDS) STARTS WITH "SYS",
*     THE REST OF IT IS TRANSMITTED TO THE SYSTEM ENVIRONMENT, ALONG
*     WITH ANY "COMMENT" (I.E. TEXT STARTING WITH ASTERISK OR WHATEVER
*     OTHER CHARACTER AS DEFINED BY A CALL TO READIR'S ENTRY SETCOM.
*     NOTE THAT IN THIS CASE THE COMMENT MUST START AFTER THE 2ND WORD).
*       (MAXIMUM LENGTH ALSO DETERMINED BY READIR'S MAXCHR PARAMETER)
*
*     IT IMPLEMENTS THE INCLUDE/RETURN MECHANISM SUPPORTING THE USE
*     OF MORE THAN ONE INPUT FILE.
*
*     AN INPUT LINE "INCLUDE N" CAUSES THE NEXT INPUT TO BE TAKEN
*     FROM LOGICAL UNIT N. (N IS A POSITIVE INTEGER)
*     THE LINE "RETURN","RETURN STAY" OR AN END-OF-FILE CONDITION CAUSES
*     THE CURRENT INPUT FILE TO BE QUITTED, RETURNING TO THE PREVIOUS
*     INPUT FILE; "RETURN STAY" SUPPRESSES A REWIND OF THE FILE,
*     SO THE REST OF ITS CONTENTS CAN BE READ LATER ON; OTHERWISE
*     THE FILE CAN BE READ AGAIN FROM THE BEGINNING.
*     "RETURN"("STAY") IS NOT RECOGNIZED WHEN ISSUED FROM THE FIRST FILE
*     (I.E. WITH NO "INCLUDE N" IN EFFECT).
*     WE START WITH UNIT FIVE.
*
*     ABBREVIATIONS OF THE KEYWORDS MENTIONED ABOVE ARE NOT RECOGNIZED.
*
*     NOTE THAT A CALL MADE TO READIR'S ENTRY SPECHR MAY AFFECT THE
*     INTERPRETATION OF INPUT LINES IN THIS SUBROUTINE.
*
*     THIS SUBROUTINE CONTINUES READING LINES (UNLESS THE "STOP" COMMAND
*     IS ISSUED) UNTIL A LINE WITH A FORMAT NOT CONFORMING TO ONE OF
*     THOSE MENTIONED ABOVE IS ENCOUNTERED. THE CALLING SIDE MAY THEN
*     INTERPRET THIS LINE WITH THE AID OF THE PARAMETER NWORDS AND THE
*     ENTRIES OF READIR.
*

      IMPLICIT INTEGER*4(A-P,R-Z),LOGICAL*4(Q)
      PARAMETER (MAXSTK=20,FIVE=5,SIX=6)

      DIMENSION FILE(MAXSTK)
      CHARACTER*81 TOSYS
      CHARACTER*44 FNAME
      CHARACTER*8 CWORD
      CHARACTER*1 FIRSTC
      SAVE FILE,STKPTR

      DATA STKPTR/1/, FILE(1)/FIVE/

  601 FORMAT(' END OF FILE ON UNIT',I3)
  602 FORMAT('0BACK TO UNIT',I3)
  603 FORMAT(' RETURN CODE =',I6)
  604 FORMAT('0INPUT NOW OBTAINED FROM UNIT',I3,', DATASET: ',A)

  100 CALL READIR(FILE(STKPTR),NWORDS)

      !IF(NWORDS.EQ.-2) GOTO 1301
      IF (NWORDS.EQ.-1) THEN
        !write(SIX,601) FILE(STKPTR)
        REWIND (UNIT=FILE(STKPTR))
        STKPTR = STKPTR - 1
        !IF(STKPTR.LE.0) GOTO 1302
        !write(SIX,602) FILE(STKPTR)
        GOTO 100
      ENDIF

      CALL WRIDIR(SIX)

      IF(NWORDS.EQ.0) GOTO 100

      CALL GTWORD(1,CWORD,NR)
      IF(NWORDS.EQ.1.AND.CWORD.EQ.'STOP') STOP

*     IF (CWORD.EQ.'SYS' .AND. NWORDS.GT.1) THEN
*       CALL WATPOS(2,IC,NC)
*       CALL GTPART(IC,TOSYS)
*       TOSYS(81:81) = ';'
*       CALL SYSTEM(TOSYS,IRC)
*       IF(IRC.NE.0) WRITE(6,603) IRC
*       GOTO 100
*     ENDIF

      IF(NWORDS.GT.3) RETURN
      IF (CWORD.EQ.'RETURN' .AND. STKPTR.GT.1) THEN
        CALL GTWORD(2,CWORD,NR)
        IF(NR.NE.0 .AND. CWORD.NE.'STAY') RETURN
*          I.E. IF THERE IS A SECOND WORD, NOT "STAY": BACK TO CALLER
        IF(NR.EQ.0) REWIND (UNIT=FILE(STKPTR))
        STKPTR = STKPTR - 1
        GOTO 100
      ENDIF
      IF(CWORD.NE.'INCLUDE'.OR.NWORDS.LT.3) RETURN
      CALL GETINT(2,NEWFIL,Q_OK)
      IF(.NOT.Q_OK) RETURN
      IF(NEWFIL.LE.0) RETURN
      STKPTR = STKPTR + 1
      !IF(STKPTR.GT.MAXSTK) GOTO 1303
      CALL GTWORD(3,FNAME,NR)
      FILE(STKPTR) = NEWFIL
C***C CALL FTXFYY(NEWFIL,1,DSN,FLAG)
      INQUIRE(FILE=FNAME, EXIST=Q_EXISTS)
      !IF(.NOT.Q_EXISTS) GOTO 1304
      OPEN(UNIT=NEWFIL,FILE=FNAME,IOSTAT=IFLAG)
      !IF(IFLAG.NE.0) GOTO 1305
      !write(SIX,604) NEWFIL,FNAME
      GOTO 100

* 1301 !write(SIX,6301) FILE(STKPTR)
* 6301 !FORMAT(//' FILE ERROR ON INPUT UNIT',I3)
      STOP 'INPUT'
* 1302 !write(SIX,6302)
 6302 FORMAT(//' ERROR: UNEXPECTED END OF USER INPUT')
      STOP 'INPUT'
* 1303 !write(SIX,6303) MAXSTK
 6303 FORMAT(//' ERROR: TOO MANY INCLUDES, MAXIMUM NUMBER OF',
     -         ' FILES IS',I3)
      STOP 'INPUT'
* 1304 !write(SIX,6304) FNAME
 6304 FORMAT(//' ERROR: FILE NOT FOUND: ',A)
      STOP 'INPUT'
* 1305 !write(SIX,6305) IFLAG, NEWFIL, FNAME
 6305 FORMAT(//' IOERROR ',I4,' ON UNIT ',I4,' FILE ',A)
      STOP 'INPUT'
      END


*-----------------------------------------------------------------------
      SUBROUTINE READIR (JUNIT,NWORDS)
*-----------------------------------------------------------------------
*
*   THIS SUBROUTINE SUPPORTS FLEXIBLE INPUT (DIRECTIVES) .
*
*   THE CURRENT IMPLEMENTATION USES:

      IMPLICIT REAL*8(A-H,O-P,R-Z),INTEGER*4(I-N),LOGICAL*4(Q)
      PARAMETER (MAXCHR=80,MAXWRD=30,MAXSPC=32)
      CHARACTER*1 DEFCOM
      PARAMETER (DEFCOM='*')

*     EXTERNAL NAMES:
*                     READIR,SIMULR,SPECHR,SETCOM,WRIDIR,GETINT,GTREAL,
*                     GTWORD,RECOG,WATPOS,GTPART

*     GENERAL IDEA:
*       EVERY TIME, A COMPLETE "CARD"/"LINE" IS READ. PART OR ALL OF IT
*       MAY CONTAIN COMMENT, WHICH IS IGNORED IN MOST CASES.
*       THE "MEANINGFULL" PART OF THE INPUT IS SPLIT INTO "WORDS"
*       USING BLANKS AND PERHAPS "SPECIAL CHARACTERS" AS SEPARATORS.
*       EVERY WORD MAY NOW BE EXAMINED INDIVIDUALLY AND IN ANY ORDER,
*       E.G. ONE MAY EXPLORE IF A WORD IS AN INTEGER AND ITS VALUE AS
*       SUCH OR WHETHER IS IS RECOGNIZABLE AS (AN ABBREVIATION OF) A
*       KEYWORD FROM A CERTAIN VOCABULARY.

******    ENTRY + PARAMETER EXPLANATION    ******

*   READIR ITSELF READS A "CARD" FROM STREAM JUNIT AND ISOLATES THE
*     ITEMS IN IT TO PREPARE CALLS TO THE ENTRIES. IT RETURNS THE NUMBER
*     OF ITEMS IN THE INPUT LINE BY MEANS OF NWORDS, -1 ON FILE END,
*     -2 ON FILE ERROR.
*
*   THE ENTRY SIMULR SIMULATES A READIR, USING THE CHARACTERS FROM THE
*     CHARACTER VARIABLE LINE.
*     NWORDS RETURNS THE NUMBER OF ITEMS.
*
*   THE MAXIMAL INPUT IS MAXCHR CHARACTERS.
*   AT MOST MAXWRD WORDS CAN BE ISOLATED.
*
*   THE ENTRY SPECHR DEFINES SPECIAL CHARACTERS (MAXSPC AT MOST).
*     A SPECIAL CHARACTER DEFINES A WORD OF LENGTH ONE AND NEED NOT
*     BE PRECEDED OR FOLLOWED BY BLANKS. THE PARAMETER NISPEC DEFINES
*     THE NUMBER OF THESE CHARACTERS (IT WILL RESET IT TO THE DEFAULT
*     VALUE OF ZERO WHEN <= 0) .
*     THE CHARACTERS MUST BE PRESENTED IN CISPEC. ANY CHARACTERS IN
*     EXCESS OF MAXSPC ARE IGNORED.
*
*   THE ENTRY SETCOM DEFINES A NEW COMMENT SYMBOL (DEFAULT IS "DEFCOM").
*     WHEN THE COMMENT SYMBOL CHARACTER IS ENCOUNTERED IN THE INPUT,
*     THE ISOLATION OF WORDS IS TERMINATED, THE PART OF THE LINE
*     STARTING WITH THE COMMENT SYMBOL NOT BEING CONSIDERED.
*
*   THE ENTRY WRIDIR PRINTS (ECHOES) THE LAST INPUT LINE ON UNIT JUNIT,
*     INCLUDING ANY COMMENT.
*
*   THE ENTRIES GETINT,GTREAL AND GTWORD OBTAIN ONE ISOLATED ITEM.
*     THE PARAMETER IWORD IS THE ORDINAL OF THE ITEM IN THE INPUT.
*     THE PARAMETERS IVAL,VALUE AND WORD RETURN THE ITEM AS AN
*     INTEGER*4,REAL*8 OR CHARACTER*(*) VALUE (RESPECTIVELY).
*     THE PARAMETER Q_NUMER OF GETINT AND GTREAL RETURNS WHETHER
*     CONVERSION COULD BE MADE.(SYNTAX ERROR & OVERFLOW CAUSE Q_NUMER=F)
*     IF(.NOT.Q_NUMER) IVAL OR VALUE ARE LEFT UNAFFECTED.
*     THE PARAMETER NREAD OF GTWORD RETURNS THE NUMBER OF CHARACTERS
*     IN THE ITEM THAT WERE ACTUALLY PRESENT.
*     TRUNCATION OR (RIGHT) PADDING WITH BLANKS IS DONE AS APPROPRIATE.
*     WHENEVER IWORD IS OUT OF RANGE, 0, 0D0 OR BLANKS ARE RETURNED AND
*     WE HAVE: Q_NUMER=T ; NREAD=0  .
*
*   THE ENTRY RECOG USES SUBROUTINE DICTIO (SEE THERE);
*     IT TRIES TO RECOGNIZE WORD NUMBER IWORD IN THE VOCABULARY;
*     WHEN IWORD IS OUT OF RANGE, NOT-FOUND IS SIMULATED.
*
*   THE ENTRY WATPOS RETURNS IN PARAMETER IPO THE STARTING POSITION
*     ON THE "CARD" , IN NPO THE LENGTH OF THE ITEM WITH ORDINAL IWORD.
*     (ZEROES WHEN IWORD IS OUT OF RANGE.)
*
*   THE ENTRY GTPART OBTAINS A PART(SLICE) OF THE "CARD" AND RETURNS IT
*     BY PARAMETER TEXT. THE PART STARTS AT POSITION IC OF THE "CARD"
*     AND MAY INCLUDE COMMENT. BEFORE POSITION 1 AND AFTER POSITION
*     MAXCHR, AN INFINITE AMOUNT OF BLANKS IS SIMULATED.
*
*   SUBROUTINES USED: TOINT,TOREAL
*

*   INTERNAL

      CHARACTER*1 SPECH(MAXSPC)
      CHARACTER*(MAXCHR) CARD
      INTEGER*4 ISTART(MAXWRD), LWORD(MAXWRD)
      INTEGER*4 NITEMS /0/ , NSPECH /0/
      CHARACTER*1 SYMCOM /DEFCOM/
      SAVE CARD,SPECH,ISTART,LWORD,NITEMS,NSPECH,SYMCOM

*   PARAMETERS

      REAL*8 VALUE
      INTEGER*4 JUNIT,NWORDS,LENGTH,NISPEC,IWORD,IVAL,NCHAR,NREAD,
     1            IPO,NPO,IC,NKEY
      LOGICAL*4 Q_NUMER
      CHARACTER*1 NEWCOM
      CHARACTER*(*) LINE,WORD,TEXT,VOCAB(*),CISPEC

*   START OF READIR

      CARD = ' '
      READ(JUNIT,105,END=2,ERR=3) CARD
  105 FORMAT(A)
      GOTO 1
    2 NWORDS = -1
      NITEMS = 0
      RETURN
    3 NWORDS = -2
      NITEMS = 0
      RETURN
*-----
      ENTRY SIMULR (LINE,NWORDS)
*-----
      CARD = LINE
      GOTO 1
*-----
      ENTRY SPECHR (CISPEC,NISPEC)
*-----
      NSPECH = 0
      IF(NISPEC.LE.0) RETURN
      NSPECH = NISPEC
      DO 4 I = 1,NSPECH
    4 SPECH(I) = CISPEC(I:I)
      RETURN
*-----
      ENTRY SETCOM (NEWCOM)
*-----
      SYMCOM = NEWCOM
      RETURN

    1 CONTINUE

*   SEPARATE ITEMS

      NITEMS = 0
      Q_INWRD = .FALSE.
*       STANDS FOR "IN WORD" (IF(Q_INWRD) A WORD IS BEING DISCOVERED)
      ICHAR = 0
   10 CONTINUE
*    NEXT CHARACTER
      ICHAR = ICHAR + 1
      IF(ICHAR.GT.MAXCHR) GOTO 100
      IF(CARD(ICHAR:ICHAR).EQ.SYMCOM) GOTO 100
      IF(CARD(ICHAR:ICHAR).NE.' ') GOTO 20
      Q_INWRD = .FALSE.
      GOTO 10
   20 CONTINUE
*    NOT A BLANK
      DO 25 ISP = 1,NSPECH
      IF(CARD(ICHAR:ICHAR).EQ.SPECH(ISP)) GOTO 40
   25 CONTINUE
*    NOT A SEPARATOR
      IF(Q_INWRD) GOTO 30
      IF(NITEMS.GE.MAXWRD) GOTO 100
*    NEW WORD
      Q_INWRD = .TRUE.
      NITEMS = NITEMS + 1
      ISTART(NITEMS) = ICHAR
      LWORD (NITEMS) = 1
      GOTO 10
   30 CONTINUE
*    OLD WORD
      LWORD(NITEMS) = LWORD(NITEMS) + 1
      GOTO 10
   40 CONTINUE
*    SPECIAL CHARACTER: BOTH A WORD IN ITSELF AND A SEPARATOR
      Q_INWRD = .FALSE.
      IF(NITEMS.GE.MAXWRD) GOTO 100
      NITEMS = NITEMS + 1
      ISTART(NITEMS) = ICHAR
      LWORD (NITEMS) = 1
      GOTO 10
  100 CONTINUE
*    END OF LINE  /  COMMENTS STARTS  /  MAXWRD REACHED

      NWORDS = NITEMS
      RETURN

*-----
      ENTRY WRIDIR (JUNIT)
*-----
*     WE LIKE TO KEEP THE OUTPUT SHORTER THAN 80 CHARACTERS IF ALLOWED.
*     THE CODE BELOW ALSO COMPILES OK WHEN MAXCHR < 71

      MODRAT = 71
      IF (MAXCHR.GE.MODRAT.AND.CARD(MODRAT:MAXCHR).EQ.' ') THEN
        MODRAT = MODRAT - 1
        !write(JUNIT,106) CARD(1:MODRAT)
      ELSE
        !write(JUNIT,106) CARD
      ENDIF
  106 FORMAT('        ',A)
      RETURN

*-----
      ENTRY GETINT (IWORD,IVAL,Q_NUMER)
*-----
      IF (IWORD.LT.1.OR.IWORD.GT.NITEMS) THEN
        IVAL = 0
        Q_NUMER = .TRUE.
      ELSE
        CALL TOINT(CARD(ISTART(IWORD):ISTART(IWORD)+LWORD(IWORD)-1),
     1             IVAL,Q_NUMER)
      ENDIF
      RETURN

*-----
      ENTRY GTREAL (IWORD,VALUE,Q_NUMER)
*-----
      IF (IWORD.LT.1.OR.IWORD.GT.NITEMS) THEN
        VALUE = 0D0
        Q_NUMER = .TRUE.
      ELSE
        CALL TOREAL(CARD(ISTART(IWORD):ISTART(IWORD)+LWORD(IWORD)-1),
     1              VALUE,Q_NUMER)
      ENDIF
      RETURN

*-----
      ENTRY GTWORD (IWORD,WORD,NREAD)
*-----
      IF (IWORD.LT.1.OR.IWORD.GT.NITEMS) THEN
        WORD = ' '
        NREAD = 0
      ELSE
        NREAD = LWORD(IWORD)
        WORD  = CARD(ISTART(IWORD):ISTART(IWORD)+LWORD(IWORD)-1)
      ENDIF
      RETURN

*-----
      ENTRY RECOG (IWORD,VOCAB,NKEY,INDEX1,NOCCUR)
*-----
      IF (IWORD.LT.1.OR.IWORD.GT.NITEMS) THEN
        INDEX1 = 0
        NOCCUR = 0
      ELSE
        CALL DICTIO(CARD(ISTART(IWORD):),LWORD(IWORD),VOCAB,NKEY,
     1                                                INDEX1,NOCCUR)
      ENDIF
      RETURN

*-----
      ENTRY WATPOS (IWORD,IPO,NPO)
*-----
      IF (IWORD.LT.1.OR.IWORD.GT.NITEMS) THEN
        IPO = 0
        NPO = 0
      ELSE
        IPO = ISTART(IWORD)
        NPO = LWORD (IWORD)
      ENDIF
      RETURN

*-----
      ENTRY GTPART (IC,TEXT)
*-----
      IF (IC.GT.MAXCHR) THEN
        TEXT = ' '
      ELSE IF (IC.GE.1) THEN
        TEXT = CARD(IC:MAXCHR)
      ELSE
        TEXT = ' '
        IT = 2 - IC
        IF(IT.LE.LEN(TEXT)) TEXT(IT:) = CARD
      ENDIF
      RETURN

      END


*-----------------------------------------------------------------------
      SUBROUTINE DICTIO (WORD,NSIG,VOCAB,NKEY,INDEX1,NOCCUR)
*-----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-P,R-Z),INTEGER*4(I-N),LOGICAL*4(Q)

*     FINDS WORD IN THE "DICTIONARY" VOCAB.
*     THE PART OF WORD THAT IS CONSIDERED IS 1:MIN(NSIG,LEN(WORD)).
*     THE PART THAT EXTENDS BEYOND THE ITEMS OF VOCAB SHOULD BE BLANKS
*     IF ANY MATCH IS TO BE FOUND. ANY PART OF ANY ITEM THAT EXTENDS
*     BEYOND WORD IS NOT CONSIDERED, ALLOWING ABBREVIATIONS.
*     INDEX1 RETURNS THE INDEX OF THE FIRST OCCURRENCE OF WORD (OR 0);
*     NOCCUR THE NUMBER OF OCCURRENCES.

        PARAMETER (MAXCHR=80)
      CHARACTER*(*) WORD,VOCAB(*)
        CHARACTER*(MAXCHR) WORDW, WORDV

      INDEX1 = 0
      NOCCUR = 0
      IF(NKEY.LE.0) RETURN
      IF (NSIG.LE.0) THEN
        INDEX1 = 1
        NOCCUR = NKEY
        RETURN
      ENDIF
      LW = MIN0(NSIG,LEN(WORD))
      LV = MIN0(LW,LEN(VOCAB(1)))
        WORDW = WORD(1:LW)
      DO 100 I=NKEY,1,-1
        WORDV = VOCAB(I)(1:LV)
***** IF(WORD(1:LW).NE.VOCAB(I)(1:LV)) GOTO 100 *** FORTVS BUG !!!!!!
      IF(WORDW.NE.WORDV) GOTO 100
      INDEX1 = I
      NOCCUR = NOCCUR + 1
  100 CONTINUE
      END

*-----------------------------------------------------------------------
      SUBROUTINE TOINT (WORD,ITIS,Q_NUMER)
*-----------------------------------------------------------------------
*
*     THIS SUBROUTINE CONVERTS THE CHARACTERS OF WORD TO AN INTEGER
*
*     Q_NUMER RETURNS THE SUCCESSFULLNESS OF THE CONVERSION
*
*     SYNTAX ERRORS, BLANKS AND OVERFLOW CAUSE Q_NUMER = F
*
*     IF(Q_NUMER) ITIS RETURNS THE INTEGER

      IMPLICIT REAL*8(A-H,O-P,R-Z),INTEGER*4(I-N),LOGICAL*4(Q)
      PARAMETER (MAXINT=2147483647)
      CHARACTER*(*) WORD
      CHARACTER*1 C0TO9(0:9) /'0','1','2','3','4','5','6','7','8','9'/

      Q_NUMER = .FALSE.
      LENGTH = LEN(WORD)
      Q_NEG  = WORD(1:1).EQ.'-'
      Q_PLUS = WORD(1:1).EQ.'+'
      I1 = 1
      IF(Q_NEG.OR.Q_PLUS) I1 = 2
      IF(LENGTH.LT.I1) RETURN

*     HERE COMPUTATION STARTS (FROM LEFT TO RIGHT)
*     BEWARE OF OVERFLOW: BIGGEST INTEGER*4 IS PARAMETER MAXINT

      IF(LENGTH-I1.GT.9) RETURN
      Q_LONG = LENGTH-I1 .EQ. 9
      INT = 0

      DO 100 I = I1,LENGTH
      Q_RISK = Q_LONG .AND. I.EQ.LENGTH
      IF(Q_RISK .AND. INT.GT.MAXINT/10) RETURN
      DO 200 JDIGIT = 0,9
      IF(WORD(I:I).EQ.C0TO9(JDIGIT)) GOTO 150
  200 CONTINUE
      RETURN
  150 CONTINUE
      INT10 = 10 * INT
      IF(Q_RISK .AND. INT10.GT.MAXINT-JDIGIT) RETURN
      INT = INT10 + JDIGIT
  100 CONTINUE

      IF(Q_NEG) INT = -INT
      ITIS = INT
      Q_NUMER = .TRUE.
      END


*-----------------------------------------------------------------------
      SUBROUTINE TOREAL (WORD,WEVGOT,Q_NUMER)
*-----------------------------------------------------------------------
*
*     THIS SUBROUTINE CONVERTS THE CHARACTERS OF WORD TO A REAL*8
*
*     WORD MAY CONTAIN AN EXPONENT PART (WITH D , d , E , e , Q or q )
*
*     Q_NUMER RETURNS THE SUCCESSFULLNESS OF THE CONVERSION
*
*     SYNTAX ERRORS, BLANKS AND OVERFLOW CAUSE Q_NUMER = F
*     VALID INTEGERS ARE ALLOWED
*         (UNDERFLOW MEANS 0D0; OVERFLOW AND UNDERFLOW ARE ASSUMED IN
*          MORE CASES THAN THE IBM REAL*8 HARDWARE IMPELLS)
*
*     IF(Q_NUMER) WEVGOT RETURNS THE REAL*8
*

      IMPLICIT REAL*8(A-H,O-P,R-Z),INTEGER*4(I-N),LOGICAL*4(Q)
      PARAMETER (OFLOW=1D70,UFLOW=1D-74,BIG=1D35,SMALL=1D-35)
      PARAMETER (LOFLOW=73,LUFLOW=-76,LARGE=35)
      CHARACTER*6 EXPSYM /'DEQdeq'/
*        (EXPSYM IS SUBSTRINGED, SO IT CAN'T BE A CONSTANT)

      CHARACTER*(*) WORD
      CHARACTER*1 C0TO9(0:9) /'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER*1 CHAR
      Q_NUMER = .FALSE.
      Q_MANT  = .FALSE.
*        (WE MUST REJECT STRINGS LIKE '.' OR '.D+10' OR 'D3' )
      LENGTH = LEN(WORD)
      Q_NEG  = WORD(1:1).EQ.'-'
      Q_PLUS = WORD(1:1).EQ.'+'
      INDEX = 0
      IF(Q_NEG.OR.Q_PLUS) INDEX = 1
      IF(LENGTH.LE.INDEX) RETURN
      REAL8 = 0D0

*   PART BEFORE DECIMAL POINT

      DO 100 I = INDEX+1 , LENGTH
      INDEX = INDEX + 1
      CHAR = WORD(INDEX:INDEX)
      DO 200 J = 0,9
      IF(CHAR.NE.C0TO9(J)) GOTO 200
      Q_MANT = .TRUE.
      IF(REAL8.GT.OFLOW) RETURN
      REAL8 = 10D0 * REAL8 + J
      GOTO 100
  200 CONTINUE
      IF(CHAR.EQ.'.') GOTO 1000
      DO 60 K=1,LEN(EXPSYM)
      IF(CHAR.EQ.EXPSYM(K:K)) GOTO 2000
   60 CONTINUE
      RETURN
  100 CONTINUE
*       (Q_MANT IS .TRUE.)
      GOTO 3000

 1000 CONTINUE

*   PART AFTER DECIMAL POINT

      IF(INDEX.EQ.LENGTH.AND..NOT.Q_MANT) RETURN
      IF(INDEX.EQ.LENGTH) GOTO 3000
      DIVIDE = 1D0
      DO 1100 I = INDEX+1 , LENGTH
      INDEX = INDEX + 1
      CHAR = WORD(INDEX:INDEX)
      DO 1200 J = 0,9
      IF(CHAR.NE.C0TO9(J)) GOTO 1200
      Q_MANT = .TRUE.
      IF(DIVIDE.LT.UFLOW) DIVIDE = 0D0
      DIVIDE = DIVIDE / 10D0
      REAL8 = REAL8 + J * DIVIDE
      GOTO 1100
 1200 CONTINUE
      DO 61 K=1,LEN(EXPSYM)
      IF(CHAR.EQ.EXPSYM(K:K)) GOTO 2000
   61 CONTINUE
      RETURN
 1100 CONTINUE
*       (Q_MANT IS .TRUE.)
      GOTO 3000

 2000 CONTINUE

*   EXPONENT PART

      IF(INDEX.GE.LENGTH.OR..NOT.Q_MANT) RETURN
      CALL TOINT(WORD(INDEX+1:LENGTH),IEXP,Q_NUM)
      IF(.NOT.Q_NUM) RETURN
      IF(REAL8.EQ.0D0) GOTO 3000

*   REAL OVERFLOW CHECK

      IF(IABS(IEXP).LT.LARGE.AND.
     #   REAL8.LT.BIG.AND.REAL8.GT.SMALL) GOTO 2500
*     ELSE A DLOG10 IS NEEDED

*     NORMALISATION:
*     MANTISSA IS SOMETHING LIKE NDDDDDDDDDD.DDN OR .000000NDDDDDDN
*     WE MAKE IT N.DDDDDDDDDDDDN   (D IS DIGIT; N A NON-ZERO DIGIT)
*     NOTE THAT HERE THE MANTISSA (REAL8) IS A POSITIVE NUMBER .

      SHIFT  =  DLOG10(REAL8)
      ISHIFT =  IDINT(SHIFT)
      IF(DFLOAT(ISHIFT).GT.SHIFT) ISHIFT = ISHIFT - 1
*         (WE WANT, FOR EXAMPLE,  -1.5D0  ->  -2   )
      IEXP = IEXP + ISHIFT
      REAL8 = REAL8 /(10D0**ISHIFT)
      IF(IEXP.GE.LOFLOW) RETURN
      IF(IEXP.GT.LUFLOW) GOTO 2500
      REAL8 = 0D0
      GOTO 3000
*
 2500 CONTINUE
      REAL8 = REAL8 * 10D0 ** IEXP
*
 3000 CONTINUE

*   READY

      IF(Q_NEG) REAL8 = -REAL8
      WEVGOT = REAL8
      Q_NUMER = .TRUE.
      RETURN
      END
