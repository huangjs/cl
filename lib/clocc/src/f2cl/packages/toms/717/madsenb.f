C  ***  SIMPLE TEST PROGRAM FOR DGLGB AND DGLFB  ***
C
      program madsenb
      INTEGER IV(92), LIV, LV, NOUT, UI(1)
      DOUBLE PRECISION B(2,2), V(200), X(2), UR(1)
      EXTERNAL I7MDCN, MADRJ, RHOLS
      INTEGER I7MDCN
C
C I7MDCN... RETURNS OUTPUT UNIT NUMBER.
C
      INTEGER LASTIV, LASTV, LMAX0
      PARAMETER (LASTIV=44, LASTV=45, LMAX0=35)
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      NOUT = I7MDCN(1)
      LV = 200
      LIV = 92
C
C  ***  SPECIFY INITIAL X AND BOUNDS ON X  ***
C
      X(1) = 3.D+0
      X(2) = 1.D+0
C     *** BOUNDS ON X(1)...
      B(1,1) = -.1D+0
      B(2,1) = 10.D+0
C     *** BOUNDS ON X(2)...
      B(1,2) =  0.D+0
      B(2,2) =  2.D+0
C
C  ***  SET IV(1) TO 0 TO FORCE ALL DEFAULT INPUT COMPONENTS TO BE USED.
C
       IV(1) = 0
C
       WRITE(NOUT,10)
 10    FORMAT(' DGLGB ON PROBLEM MADSEN...')
C
C  ***  CALL DGLG, PASSING UI FOR RHOI, UR FOR RHOR, AND MADRJ FOR
C  ***  UFPARM (ALL UNUSED IN THIS EXAMPLE).
C
      CALL DGLGB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
C  ***  SEE HOW MUCH STORAGE DGLGB USED...
C
      WRITE(NOUT,20) IV(LASTIV), IV(LASTV)
 20   FORMAT(' DGLGB NEEDED LIV .GE. ',I3,' AND LV .GE.',I4)
C
C  ***  SOLVE THE SAME PROBLEM USING DGLFB...
C
      WRITE(NOUT,30)
 30   FORMAT(/' DGLFB ON PROBLEM MADSEN...')
      X(1) = 3.D+0
      X(2) = 1.D+0
      IV(1) = 0
      CALL DGLFB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
C  ***  REPEAT THE LAST RUN, BUT WITH A DIFFERENT INITIAL STEP BOUND
C
C  ***  FIRST CALL DIVSET TO GET DEFAULT IV AND V INPUT VALUES...
C
      CALL DIVSET(1, IV, LIV, LV, V)
C
C  ***  NOW ASSIGN THE NONDEFAULT VALUES.
C
      V(LMAX0) = 0.1D+0
      X(1) = 3.D+0
      X(2) = 1.D+0
C
      WRITE(NOUT,40)
 40   FORMAT(/' DGLFB ON PROBLEM MADSEN AGAIN...')
C
      CALL DGLFB(3, 2, 2, X, B, RHOLS, UI, UR, IV, LIV, LV, V, MADRJ,
     1           UI,UR, MADRJ)
C
      STOP
      END
