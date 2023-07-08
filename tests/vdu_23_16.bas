   10 REM MODE 1 on the BBC: 40x32 characters (X=40, Y=32)
   20 REM MODE 2 on the Agon: 40x25 characters (X=40, Y=25)
   30 REM Set appropriate mode below and X,Y values
   40 :
   50 MODE 2
   60 X=40
   70 Y=25
   80 :
   90 REM Disable screen scrolling:
  100 REM BBC Master only!
  110 VDU 23,16,1,255,0,0,0,0,0,0
  130 PROCdrawscreen
  140 :
  150 A=GET
  170 END
  180 :
  200 DEF PROCdrawscreen
  220    REM Define border characters
  240    REM Horizontal Bar
  250    VDU 23,224,0,0,255,255,255,255,0,0
  260    REM Vertical Bar
  270    VDU 23,225,60,60,60,60,60,60,60,60
  280    REM Top Left Corner
  290    VDU 23,226,0,0,63,63,63,63,60,60
  300    REM Top Right Corner
  310    VDU 23,227,0,0,252,252,252,252,60,60
  320    REM Bottom Right Corner
  330    VDU 23,228,60,60,252,252,252,252,0,0
  340    REM Bottom Left Corner
  350    VDU 23,229,60,60,63,63,63,63,0,0
  370    REM Draw the frame
  390    REM *** Draw Line 0 ***
  410    PRINT TAB(0,0) CHR$(226);
  430    FOR J%=1 TO X-2
  440        PRINT TAB(J%,0) CHR$(224);
  450    NEXT J%
  470    PRINT TAB(X-1,0) CHR$(227);
  490    REM *** Draw middle lines ***
  510    FOR J%=1 TO Y-2
  520        PRINT TAB(0,J%) CHR$(225);
  530        PRINT TAB(X-1,J%) CHR$(225);
  540    NEXT J%
  560    REM *** Draw bottom line ***
  580    PRINT TAB(0,Y-1) CHR$(229);
  600    FOR J%=1 TO X-2
  610        PRINT TAB(J%,Y-1) CHR$(224);
  620    NEXT J%
  640    PRINT TAB(X-1,Y-1) CHR$(228);
  660 ENDPROC