   10 REM Text Viewport Example using VDU 28
   20 :
   30 CLS
   40 :
   50 cols%=FN_getScreenCols - 1
   60 rows%=FN_getScreenRows - 1
   70 :
   80 REM Create a border around the screen
   90 :
  100 REM Top line
  110 PROC_fill(0,0,cols%,0)
  120 REM Left side
  130 PROC_fill(0,rows%-1,0,1)
  140 REM Right side
  150 PROC_fill(cols%,rows%-1,cols%,1)
  160 REM Bottom line
  170 PROC_fill(0,rows%,cols%,rows%)
  180 :
  190 REM Display some text in various sizes of viewport
  200 t% = 2
  210 b% = 2
  220 l% = 2
  230 r% = cols% - 2
  240 size% = 1
  250 REPEAT
  260   PROC_stars(l%,b%,r%,t%)
  270   t% = b% + 2
  280   size% = size% + 1
  290   r% = r% - 1
  300   b% = t% + size%
  310 UNTIL b% > rows% - 1 OR r% < l%
  320 :
  330 k%=GET
  480 END
  490 :
  500 DEF PROC_fill(L%,B%,R%,T%)
  510 VDU 28,L%,B%,R%,T%
  520 VDU 17,191,17,0
  530 VDU 12
  540 VDU 17,128,17,63
  550 VDU 26
  560 ENDPROC
  570 :
  600 DEF FN_getScreenCols
  610 REM A% is the OSBYTE command to run
  620 A%=&A0
  630 REM L% is the sysvar to fetch
  640 L%=&13
  650 =USR(&FFF4)
  660 :
  700 DEF FN_getScreenRows
  710 REM A% is the OSBYTE command to run
  720 A%=&A0
  730 REM L% is the sysvar to fetch
  740 L%=&14
  750 =USR(&FFF4)
  760 :
  800 DEF PROC_stars(L%,B%,R%,T%)
  810 VDU 28,L%,B%,R%,T%
  820 C% = (R% -L% + 1) * (B% - T% + 1) * 2 - 1
  830 PATTERN$ = "/-|\*"
  840 J% = 1
  850 FOR I% = 1 TO C%
  860   PRINT MID$(PATTERN$, J%, 1);
  870   J% = J% + 1
  880   IF J% = 6 THEN J% = 1
  890 NEXT I%
  900 VDU 26
  910 ENDPROC
