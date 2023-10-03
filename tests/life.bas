   10 REM Conway's Game of Life
   20 :
   30 ON ERROR GOTO 510
   40 W%=38
   50 H%=26
   60 DIM L%(W%,H%), N%(W%,H%)
   70 MODE 8
   80 VDU 23,1,0
   90 FOR I%=1 TO W%
  100   FOR J%=1 TO H%
  110     IF RND(1)>=.7 N%(I%,J%)=1 ELSE N%(I%,J%)=0
  120   NEXT
  130 NEXT
  140 :
  150 COLOUR 13
  160 FOR I%=0 TO W%+1
  170   PRINT TAB(I%,0);"+"
  180   PRINT TAB(I%,H%+1);"+"
  190 NEXT
  200 FOR I%=0 TO H%+1
  210   PRINT TAB(0,I%);"+"
  220   PRINT TAB(W%+1,I%);"+"
  230 NEXT
  240 G%=0
  250 COLOUR 14
  260 PRINT TAB(0,H%+3);"Generation: ";G%;
  270 COLOUR 15
  280 FOR J%=1 TO H%
  290   PRINT TAB(1,J%);
  300   FOR I%=1 TO W%
  310     C%=N%(I%,J%): L%(I%,J%)=C%
  320     IF C% VDU 42 ELSE VDU 32
  330   NEXT
  340 NEXT
  350 FOR I%=1 TO W%
  360   FOR J%=1 TO H%
  370     C%=0
  380     FOR K%=I%-1 TO I%+1
  390       IF K%=0 OR K%>W% THEN 440
  400       FOR M%=J%-1 TO J%+1
  410         IF M%=0 OR M%>H% OR (K%=I% AND M%=J%) GOTO 430
  420         C%=C%+L%(K%,M%)
  430       NEXT
  440     NEXT
  450     IF C%=2 THEN N%(I%,J%)=L%(I%,J%) ELSE N%(I%,J%)=-(C%=3)
  460   NEXT
  470 NEXT
  480 G%=G%+1
  490 GOTO 250
  500 :
  510 ON ERROR OFF
  520 VDU 23,1,1
  530 PRINT TAB(0,0);
