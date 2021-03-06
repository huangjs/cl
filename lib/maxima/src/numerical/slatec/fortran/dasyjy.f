*DECK DASYJY
      SUBROUTINE DASYJY (FUNJY, X, FNU, FLGJY, IN, Y, WK, IFLW)
C***BEGIN PROLOGUE  DASYJY
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DBESJ and DBESY
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ASYJY-S, DASYJY-D)
C***AUTHOR  Amos, D. E., (SNLA)
C***DESCRIPTION
C
C                 DASYJY computes Bessel functions J and Y
C               for arguments X.GT.0.0 and orders FNU .GE. 35.0
C               on FLGJY = 1 and FLGJY = -1 respectively
C
C                                  INPUT
C
C      FUNJY - External subroutine JAIRY or YAIRY
C          X - Argument, X.GT.0.0D0
C        FNU - Order of the first Bessel function
C      FLGJY - Selection flag
C              FLGJY =  1.0D0 gives the J function
C              FLGJY = -1.0D0 gives the Y function
C         IN - Number of functions desired, IN = 1 or 2
C
C                                  OUTPUT
C
C         Y  - A vector whose first IN components contain the sequence
C       IFLW - A flag indicating underflow or overflow
C                    return variables for BESJ only
C      WK(1) = 1 - (X/FNU)**2 = W**2
C      WK(2) = SQRT(ABS(WK(1)))
C      WK(3) = ABS(WK(2) - ATAN(WK(2)))  or
C              ABS(LN((1 + WK(2))/(X/FNU)) - WK(2))
C            = ABS((2/3)*ZETA**(3/2))
C      WK(4) = FNU*WK(3)
C      WK(5) = (1.5*WK(3)*FNU)**(1/3) = SQRT(ZETA)*FNU**(1/3)
C      WK(6) = SIGN(1.,W**2)*WK(5)**2 = SIGN(1.,W**2)*ZETA*FNU**(2/3)
C      WK(7) = FNU**(1/3)
C
C     Abstract   **** A Double Precision Routine ****
C         DASYJY implements the uniform asymptotic expansion of
C         the J and Y Bessel functions for FNU.GE.35 and real
C         X.GT.0.0D0. The forms are identical except for a change
C         in sign of some of the terms. This change in sign is
C         accomplished by means of the flag FLGJY = 1 or -1. On
C         FLGJY = 1 the Airy functions AI(X) and DAI(X) are
C         supplied by the external function JAIRY, and on
C         FLGJY = -1 the Airy functions BI(X) and DBI(X) are
C         supplied by the external function YAIRY.
C
C***SEE ALSO  DBESJ, DBESY
C***ROUTINES CALLED  D1MACH, I1MACH
C***REVISION HISTORY  (YYMMDD)
C   750101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   891004  Correction computation of ELIM.  (WRB)
C   891009  Removed unreferenced variable.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated the AUTHOR section.  (WRB)
C***END PROLOGUE  DASYJY
      INTEGER I, IFLW, IN, J, JN,JR,JU,K, KB,KLAST,KMAX,KP1, KS, KSP1,
     * KSTEMP, L, LR, LRP1, ISETA, ISETB
      INTEGER I1MACH
      DOUBLE PRECISION ABW2, AKM, ALFA, AP, AR, ASUM, AZ,
     * BETA, BR, BSUM, C, CON1, CON2,
     * CON548,CR,CRZ32, DFI,ELIM, DR,FI, FLGJY, FN, FNU,
     * FN2, GAMA, PHI,  RCZ, RDEN, RELB, RFN2,  RTZ, RZDEN,
     * SA, SB, SUMA, SUMB, S1, TA, TAU, TB, TFN, TOL, TOLS, T2, UPOL,
     *  WK, X, XX, Y, Z, Z32
      DOUBLE PRECISION D1MACH
      DIMENSION Y(*), WK(*), C(65)
      DIMENSION ALFA(26,4), BETA(26,5)
      DIMENSION GAMA(26), KMAX(5), AR(8), BR(10), UPOL(10)
      DIMENSION CR(10), DR(10)
      SAVE TOLS, CON1, CON2, CON548, AR, BR, C,
     1 GAMA
      DATA TOLS            /-6.90775527898214D+00/
      DATA CON1,CON2,CON548/
     1 6.66666666666667D-01, 3.33333333333333D-01, 1.04166666666667D-01/
      DATA  AR(1),  AR(2),  AR(3),  AR(4),  AR(5),  AR(6),  AR(7),
     A      AR(8)          / 8.35503472222222D-02, 1.28226574556327D-01,
     1 2.91849026464140D-01, 8.81627267443758D-01, 3.32140828186277D+00,
     2 1.49957629868626D+01, 7.89230130115865D+01, 4.74451538868264D+02/
      DATA  BR(1), BR(2), BR(3), BR(4), BR(5), BR(6), BR(7), BR(8),
     A      BR(9), BR(10)  /-1.45833333333333D-01,-9.87413194444444D-02,
     1-1.43312053915895D-01,-3.17227202678414D-01,-9.42429147957120D-01,
     2-3.51120304082635D+00,-1.57272636203680D+01,-8.22814390971859D+01,
     3-4.92355370523671D+02,-3.31621856854797D+03/
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3       -2.08333333333333D-01,        1.25000000000000D-01,
     4        3.34201388888889D-01,       -4.01041666666667D-01,
     5        7.03125000000000D-02,       -1.02581259645062D+00,
     6        1.84646267361111D+00,       -8.91210937500000D-01,
     7        7.32421875000000D-02,        4.66958442342625D+00,
     8       -1.12070026162230D+01,        8.78912353515625D+00,
     9       -2.36408691406250D+00,        1.12152099609375D-01,
     A       -2.82120725582002D+01,        8.46362176746007D+01,
     B       -9.18182415432400D+01,        4.25349987453885D+01,
     C       -7.36879435947963D+00,        2.27108001708984D-01,
     D        2.12570130039217D+02,       -7.65252468141182D+02,
     E        1.05999045252800D+03,       -6.99579627376133D+02/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3        2.18190511744212D+02,       -2.64914304869516D+01,
     4        5.72501420974731D-01,       -1.91945766231841D+03,
     5        8.06172218173731D+03,       -1.35865500064341D+04,
     6        1.16553933368645D+04,       -5.30564697861340D+03,
     7        1.20090291321635D+03,       -1.08090919788395D+02,
     8        1.72772750258446D+00,        2.02042913309661D+04,
     9       -9.69805983886375D+04,        1.92547001232532D+05,
     A       -2.03400177280416D+05,        1.22200464983017D+05,
     B       -4.11926549688976D+04,        7.10951430248936D+03,
     C       -4.93915304773088D+02,        6.07404200127348D+00,
     D       -2.42919187900551D+05,        1.31176361466298D+06,
     E       -2.99801591853811D+06,        3.76327129765640D+06/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65)/
     3       -2.81356322658653D+06,        1.26836527332162D+06,
     4       -3.31645172484564D+05,        4.52187689813627D+04,
     5       -2.49983048181121D+03,        2.43805296995561D+01,
     6        3.28446985307204D+06,       -1.97068191184322D+07,
     7        5.09526024926646D+07,       -7.41051482115327D+07,
     8        6.63445122747290D+07,       -3.75671766607634D+07,
     9        1.32887671664218D+07,       -2.78561812808645D+06,
     A        3.08186404612662D+05,       -1.38860897537170D+04,
     B        1.10017140269247D+02/
      DATA ALFA(1,1), ALFA(2,1), ALFA(3,1), ALFA(4,1), ALFA(5,1),
     1     ALFA(6,1), ALFA(7,1), ALFA(8,1), ALFA(9,1), ALFA(10,1),
     2     ALFA(11,1),ALFA(12,1),ALFA(13,1),ALFA(14,1),ALFA(15,1),
     3     ALFA(16,1),ALFA(17,1),ALFA(18,1),ALFA(19,1),ALFA(20,1),
     4     ALFA(21,1),ALFA(22,1),ALFA(23,1),ALFA(24,1),ALFA(25,1),
     5     ALFA(26,1)     /-4.44444444444444D-03,-9.22077922077922D-04,
     6-8.84892884892885D-05, 1.65927687832450D-04, 2.46691372741793D-04,
     7 2.65995589346255D-04, 2.61824297061501D-04, 2.48730437344656D-04,
     8 2.32721040083232D-04, 2.16362485712365D-04, 2.00738858762752D-04,
     9 1.86267636637545D-04, 1.73060775917876D-04, 1.61091705929016D-04,
     1 1.50274774160908D-04, 1.40503497391270D-04, 1.31668816545923D-04,
     2 1.23667445598253D-04, 1.16405271474738D-04, 1.09798298372713D-04,
     3 1.03772410422993D-04, 9.82626078369363D-05, 9.32120517249503D-05,
     4 8.85710852478712D-05, 8.42963105715700D-05, 8.03497548407791D-05/
      DATA ALFA(1,2), ALFA(2,2), ALFA(3,2), ALFA(4,2), ALFA(5,2),
     1     ALFA(6,2), ALFA(7,2), ALFA(8,2), ALFA(9,2), ALFA(10,2),
     2     ALFA(11,2),ALFA(12,2),ALFA(13,2),ALFA(14,2),ALFA(15,2),
     3     ALFA(16,2),ALFA(17,2),ALFA(18,2),ALFA(19,2),ALFA(20,2),
     4     ALFA(21,2),ALFA(22,2),ALFA(23,2),ALFA(24,2),ALFA(25,2),
     5     ALFA(26,2)     / 6.93735541354589D-04, 2.32241745182922D-04,
     6-1.41986273556691D-05,-1.16444931672049D-04,-1.50803558053049D-04,
     7-1.55121924918096D-04,-1.46809756646466D-04,-1.33815503867491D-04,
     8-1.19744975684254D-04,-1.06184319207974D-04,-9.37699549891194D-05,
     9-8.26923045588193D-05,-7.29374348155221D-05,-6.44042357721016D-05,
     1-5.69611566009369D-05,-5.04731044303562D-05,-4.48134868008883D-05,
     2-3.98688727717599D-05,-3.55400532972042D-05,-3.17414256609022D-05,
     3-2.83996793904175D-05,-2.54522720634871D-05,-2.28459297164725D-05,
     4-2.05352753106481D-05,-1.84816217627666D-05,-1.66519330021394D-05/
      DATA ALFA(1,3), ALFA(2,3), ALFA(3,3), ALFA(4,3), ALFA(5,3),
     1     ALFA(6,3), ALFA(7,3), ALFA(8,3), ALFA(9,3), ALFA(10,3),
     2     ALFA(11,3),ALFA(12,3),ALFA(13,3),ALFA(14,3),ALFA(15,3),
     3     ALFA(16,3),ALFA(17,3),ALFA(18,3),ALFA(19,3),ALFA(20,3),
     4     ALFA(21,3),ALFA(22,3),ALFA(23,3),ALFA(24,3),ALFA(25,3),
     5     ALFA(26,3)     /-3.54211971457744D-04,-1.56161263945159D-04,
     6 3.04465503594936D-05, 1.30198655773243D-04, 1.67471106699712D-04,
     7 1.70222587683593D-04, 1.56501427608595D-04, 1.36339170977445D-04,
     8 1.14886692029825D-04, 9.45869093034688D-05, 7.64498419250898D-05,
     9 6.07570334965197D-05, 4.74394299290509D-05, 3.62757512005344D-05,
     1 2.69939714979225D-05, 1.93210938247939D-05, 1.30056674793963D-05,
     2 7.82620866744497D-06, 3.59257485819352D-06, 1.44040049814252D-07,
     3-2.65396769697939D-06,-4.91346867098486D-06,-6.72739296091248D-06,
     4-8.17269379678658D-06,-9.31304715093561D-06,-1.02011418798016D-05/
      DATA ALFA(1,4), ALFA(2,4), ALFA(3,4), ALFA(4,4), ALFA(5,4),
     1     ALFA(6,4), ALFA(7,4), ALFA(8,4), ALFA(9,4), ALFA(10,4),
     2     ALFA(11,4),ALFA(12,4),ALFA(13,4),ALFA(14,4),ALFA(15,4),
     3     ALFA(16,4),ALFA(17,4),ALFA(18,4),ALFA(19,4),ALFA(20,4),
     4     ALFA(21,4),ALFA(22,4),ALFA(23,4),ALFA(24,4),ALFA(25,4),
     5     ALFA(26,4)     / 3.78194199201773D-04, 2.02471952761816D-04,
     6-6.37938506318862D-05,-2.38598230603006D-04,-3.10916256027362D-04,
     7-3.13680115247576D-04,-2.78950273791323D-04,-2.28564082619141D-04,
     8-1.75245280340847D-04,-1.25544063060690D-04,-8.22982872820208D-05,
     9-4.62860730588116D-05,-1.72334302366962D-05, 5.60690482304602D-06,
     1 2.31395443148287D-05, 3.62642745856794D-05, 4.58006124490189D-05,
     2 5.24595294959114D-05, 5.68396208545815D-05, 5.94349820393104D-05,
     3 6.06478527578422D-05, 6.08023907788436D-05, 6.01577894539460D-05,
     4 5.89199657344698D-05, 5.72515823777593D-05, 5.52804375585853D-05/
      DATA BETA(1,1), BETA(2,1), BETA(3,1), BETA(4,1), BETA(5,1),
     1     BETA(6,1), BETA(7,1), BETA(8,1), BETA(9,1), BETA(10,1),
     2     BETA(11,1),BETA(12,1),BETA(13,1),BETA(14,1),BETA(15,1),
     3     BETA(16,1),BETA(17,1),BETA(18,1),BETA(19,1),BETA(20,1),
     4     BETA(21,1),BETA(22,1),BETA(23,1),BETA(24,1),BETA(25,1),
     5     BETA(26,1)     / 1.79988721413553D-02, 5.59964911064388D-03,
     6 2.88501402231133D-03, 1.80096606761054D-03, 1.24753110589199D-03,
     7 9.22878876572938D-04, 7.14430421727287D-04, 5.71787281789705D-04,
     8 4.69431007606482D-04, 3.93232835462917D-04, 3.34818889318298D-04,
     9 2.88952148495752D-04, 2.52211615549573D-04, 2.22280580798883D-04,
     1 1.97541838033063D-04, 1.76836855019718D-04, 1.59316899661821D-04,
     2 1.44347930197334D-04, 1.31448068119965D-04, 1.20245444949303D-04,
     3 1.10449144504599D-04, 1.01828770740567D-04, 9.41998224204238D-05,
     4 8.74130545753834D-05, 8.13466262162801D-05, 7.59002269646219D-05/
      DATA BETA(1,2), BETA(2,2), BETA(3,2), BETA(4,2), BETA(5,2),
     1     BETA(6,2), BETA(7,2), BETA(8,2), BETA(9,2), BETA(10,2),
     2     BETA(11,2),BETA(12,2),BETA(13,2),BETA(14,2),BETA(15,2),
     3     BETA(16,2),BETA(17,2),BETA(18,2),BETA(19,2),BETA(20,2),
     4     BETA(21,2),BETA(22,2),BETA(23,2),BETA(24,2),BETA(25,2),
     5     BETA(26,2)     /-1.49282953213429D-03,-8.78204709546389D-04,
     6-5.02916549572035D-04,-2.94822138512746D-04,-1.75463996970783D-04,
     7-1.04008550460816D-04,-5.96141953046458D-05,-3.12038929076098D-05,
     8-1.26089735980230D-05,-2.42892608575730D-07, 8.05996165414274D-06,
     9 1.36507009262147D-05, 1.73964125472926D-05, 1.98672978842134D-05,
     1 2.14463263790823D-05, 2.23954659232457D-05, 2.28967783814713D-05,
     2 2.30785389811178D-05, 2.30321976080909D-05, 2.28236073720349D-05,
     3 2.25005881105292D-05, 2.20981015361991D-05, 2.16418427448104D-05,
     4 2.11507649256221D-05, 2.06388749782171D-05, 2.01165241997082D-05/
      DATA BETA(1,3), BETA(2,3), BETA(3,3), BETA(4,3), BETA(5,3),
     1     BETA(6,3), BETA(7,3), BETA(8,3), BETA(9,3), BETA(10,3),
     2     BETA(11,3),BETA(12,3),BETA(13,3),BETA(14,3),BETA(15,3),
     3     BETA(16,3),BETA(17,3),BETA(18,3),BETA(19,3),BETA(20,3),
     4     BETA(21,3),BETA(22,3),BETA(23,3),BETA(24,3),BETA(25,3),
     5     BETA(26,3)     / 5.52213076721293D-04, 4.47932581552385D-04,
     6 2.79520653992021D-04, 1.52468156198447D-04, 6.93271105657044D-05,
     7 1.76258683069991D-05,-1.35744996343269D-05,-3.17972413350427D-05,
     8-4.18861861696693D-05,-4.69004889379141D-05,-4.87665447413787D-05,
     9-4.87010031186735D-05,-4.74755620890087D-05,-4.55813058138628D-05,
     1-4.33309644511266D-05,-4.09230193157750D-05,-3.84822638603221D-05,
     2-3.60857167535411D-05,-3.37793306123367D-05,-3.15888560772110D-05,
     3-2.95269561750807D-05,-2.75978914828336D-05,-2.58006174666884D-05,
     4-2.41308356761280D-05,-2.25823509518346D-05,-2.11479656768913D-05/
      DATA BETA(1,4), BETA(2,4), BETA(3,4), BETA(4,4), BETA(5,4),
     1     BETA(6,4), BETA(7,4), BETA(8,4), BETA(9,4), BETA(10,4),
     2     BETA(11,4),BETA(12,4),BETA(13,4),BETA(14,4),BETA(15,4),
     3     BETA(16,4),BETA(17,4),BETA(18,4),BETA(19,4),BETA(20,4),
     4     BETA(21,4),BETA(22,4),BETA(23,4),BETA(24,4),BETA(25,4),
     5     BETA(26,4)     /-4.74617796559960D-04,-4.77864567147321D-04,
     6-3.20390228067038D-04,-1.61105016119962D-04,-4.25778101285435D-05,
     7 3.44571294294968D-05, 7.97092684075675D-05, 1.03138236708272D-04,
     8 1.12466775262204D-04, 1.13103642108481D-04, 1.08651634848774D-04,
     9 1.01437951597662D-04, 9.29298396593364D-05, 8.40293133016090D-05,
     1 7.52727991349134D-05, 6.69632521975731D-05, 5.92564547323195D-05,
     2 5.22169308826976D-05, 4.58539485165361D-05, 4.01445513891487D-05,
     3 3.50481730031328D-05, 3.05157995034347D-05, 2.64956119950516D-05,
     4 2.29363633690998D-05, 1.97893056664022D-05, 1.70091984636413D-05/
      DATA BETA(1,5), BETA(2,5), BETA(3,5), BETA(4,5), BETA(5,5),
     1     BETA(6,5), BETA(7,5), BETA(8,5), BETA(9,5), BETA(10,5),
     2     BETA(11,5),BETA(12,5),BETA(13,5),BETA(14,5),BETA(15,5),
     3     BETA(16,5),BETA(17,5),BETA(18,5),BETA(19,5),BETA(20,5),
     4     BETA(21,5),BETA(22,5),BETA(23,5),BETA(24,5),BETA(25,5),
     5     BETA(26,5)     / 7.36465810572578D-04, 8.72790805146194D-04,
     6 6.22614862573135D-04, 2.85998154194304D-04, 3.84737672879366D-06,
     7-1.87906003636972D-04,-2.97603646594555D-04,-3.45998126832656D-04,
     8-3.53382470916038D-04,-3.35715635775049D-04,-3.04321124789040D-04,
     9-2.66722723047613D-04,-2.27654214122820D-04,-1.89922611854562D-04,
     1-1.55058918599094D-04,-1.23778240761874D-04,-9.62926147717644D-05,
     2-7.25178327714425D-05,-5.22070028895634D-05,-3.50347750511901D-05,
     3-2.06489761035552D-05,-8.70106096849767D-06, 1.13698686675100D-06,
     4 9.16426474122779D-06, 1.56477785428873D-05, 2.08223629482467D-05/
      DATA GAMA(1),   GAMA(2),   GAMA(3),   GAMA(4),   GAMA(5),
     1     GAMA(6),   GAMA(7),   GAMA(8),   GAMA(9),   GAMA(10),
     2     GAMA(11),  GAMA(12),  GAMA(13),  GAMA(14),  GAMA(15),
     3     GAMA(16),  GAMA(17),  GAMA(18),  GAMA(19),  GAMA(20),
     4     GAMA(21),  GAMA(22),  GAMA(23),  GAMA(24),  GAMA(25),
     5     GAMA(26)        / 6.29960524947437D-01, 2.51984209978975D-01,
     6 1.54790300415656D-01, 1.10713062416159D-01, 8.57309395527395D-02,
     7 6.97161316958684D-02, 5.86085671893714D-02, 5.04698873536311D-02,
     8 4.42600580689155D-02, 3.93720661543510D-02, 3.54283195924455D-02,
     9 3.21818857502098D-02, 2.94646240791158D-02, 2.71581677112934D-02,
     1 2.51768272973862D-02, 2.34570755306079D-02, 2.19508390134907D-02,
     2 2.06210828235646D-02, 1.94388240897881D-02, 1.83810633800683D-02,
     3 1.74293213231963D-02, 1.65685837786612D-02, 1.57865285987918D-02,
     4 1.50729501494096D-02, 1.44193250839955D-02, 1.38184805735342D-02/
C***FIRST EXECUTABLE STATEMENT  DASYJY
      TA = D1MACH(3)
      TOL = MAX(TA,1.0D-15)
      TB = D1MACH(5)
      JU = I1MACH(15)
      IF(FLGJY.EQ.1.0D0) GO TO 6
      JR = I1MACH(14)
      ELIM = -2.303D0*TB*(JU+JR)
      GO TO 7
    6 CONTINUE
      ELIM = -2.303D0*(TB*JU+3.0D0)
    7 CONTINUE
      FN = FNU
      IFLW = 0
      DO 170 JN=1,IN
        XX = X/FN
        WK(1) = 1.0D0 - XX*XX
        ABW2 = ABS(WK(1))
        WK(2) = SQRT(ABW2)
        WK(7) = FN**CON2
        IF (ABW2.GT.0.27750D0) GO TO 80
C
C     ASYMPTOTIC EXPANSION
C     CASES NEAR X=FN, ABS(1.-(X/FN)**2).LE.0.2775
C     COEFFICIENTS OF ASYMPTOTIC EXPANSION BY SERIES
C
C     ZETA AND TRUNCATION FOR A(ZETA) AND B(ZETA) SERIES
C
C     KMAX IS TRUNCATION INDEX FOR A(ZETA) AND B(ZETA) SERIES=MAX(2,SA)
C
        SA = 0.0D0
        IF (ABW2.EQ.0.0D0) GO TO 10
        SA = TOLS/LOG(ABW2)
   10   SB = SA
        DO 20 I=1,5
          AKM = MAX(SA,2.0D0)
          KMAX(I) = INT(AKM)
          SA = SA + SB
   20   CONTINUE
        KB = KMAX(5)
        KLAST = KB - 1
        SA = GAMA(KB)
        DO 30 K=1,KLAST
          KB = KB - 1
          SA = SA*WK(1) + GAMA(KB)
   30   CONTINUE
        Z = WK(1)*SA
        AZ = ABS(Z)
        RTZ = SQRT(AZ)
        WK(3) = CON1*AZ*RTZ
        WK(4) = WK(3)*FN
        WK(5) = RTZ*WK(7)
        WK(6) = -WK(5)*WK(5)
        IF(Z.LE.0.0D0) GO TO 35
        IF(WK(4).GT.ELIM) GO TO 75
        WK(6) = -WK(6)
   35   CONTINUE
        PHI = SQRT(SQRT(SA+SA+SA+SA))
C
C     B(ZETA) FOR S=0
C
        KB = KMAX(5)
        KLAST = KB - 1
        SB = BETA(KB,1)
        DO 40 K=1,KLAST
          KB = KB - 1
          SB = SB*WK(1) + BETA(KB,1)
   40   CONTINUE
        KSP1 = 1
        FN2 = FN*FN
        RFN2 = 1.0D0/FN2
        RDEN = 1.0D0
        ASUM = 1.0D0
        RELB = TOL*ABS(SB)
        BSUM = SB
        DO 60 KS=1,4
          KSP1 = KSP1 + 1
          RDEN = RDEN*RFN2
C
C     A(ZETA) AND B(ZETA) FOR S=1,2,3,4
C
          KSTEMP = 5 - KS
          KB = KMAX(KSTEMP)
          KLAST = KB - 1
          SA = ALFA(KB,KS)
          SB = BETA(KB,KSP1)
          DO 50 K=1,KLAST
            KB = KB - 1
            SA = SA*WK(1) + ALFA(KB,KS)
            SB = SB*WK(1) + BETA(KB,KSP1)
   50     CONTINUE
          TA = SA*RDEN
          TB = SB*RDEN
          ASUM = ASUM + TA
          BSUM = BSUM + TB
          IF (ABS(TA).LE.TOL .AND. ABS(TB).LE.RELB) GO TO 70
   60   CONTINUE
   70   CONTINUE
        BSUM = BSUM/(FN*WK(7))
        GO TO 160
C
   75   CONTINUE
        IFLW = 1
        RETURN
C
   80   CONTINUE
        UPOL(1) = 1.0D0
        TAU = 1.0D0/WK(2)
        T2 = 1.0D0/WK(1)
        IF (WK(1).GE.0.0D0) GO TO 90
C
C     CASES FOR (X/FN).GT.SQRT(1.2775)
C
        WK(3) = ABS(WK(2)-ATAN(WK(2)))
        WK(4) = WK(3)*FN
        RCZ = -CON1/WK(4)
        Z32 = 1.5D0*WK(3)
        RTZ = Z32**CON2
        WK(5) = RTZ*WK(7)
        WK(6) = -WK(5)*WK(5)
        GO TO 100
   90   CONTINUE
C
C     CASES FOR (X/FN).LT.SQRT(0.7225)
C
        WK(3) = ABS(LOG((1.0D0+WK(2))/XX)-WK(2))
        WK(4) = WK(3)*FN
        RCZ = CON1/WK(4)
        IF(WK(4).GT.ELIM) GO TO 75
        Z32 = 1.5D0*WK(3)
        RTZ = Z32**CON2
        WK(7) = FN**CON2
        WK(5) = RTZ*WK(7)
        WK(6) = WK(5)*WK(5)
  100   CONTINUE
        PHI = SQRT((RTZ+RTZ)*TAU)
        TB = 1.0D0
        ASUM = 1.0D0
        TFN = TAU/FN
        RDEN=1.0D0/FN
        RFN2=RDEN*RDEN
        RDEN=1.0D0
        UPOL(2) = (C(1)*T2+C(2))*TFN
        CRZ32 = CON548*RCZ
        BSUM = UPOL(2) + CRZ32
        RELB = TOL*ABS(BSUM)
        AP = TFN
        KS = 0
        KP1 = 2
        RZDEN = RCZ
        L = 2
        ISETA=0
        ISETB=0
        DO 140 LR=2,8,2
C
C     COMPUTE TWO U POLYNOMIALS FOR NEXT A(ZETA) AND B(ZETA)
C
          LRP1 = LR + 1
          DO 120 K=LR,LRP1
            KS = KS + 1
            KP1 = KP1 + 1
            L = L + 1
            S1 = C(L)
            DO 110 J=2,KP1
              L = L + 1
              S1 = S1*T2 + C(L)
  110       CONTINUE
            AP = AP*TFN
            UPOL(KP1) = AP*S1
            CR(KS) = BR(KS)*RZDEN
            RZDEN = RZDEN*RCZ
            DR(KS) = AR(KS)*RZDEN
  120     CONTINUE
          SUMA = UPOL(LRP1)
          SUMB = UPOL(LR+2) + UPOL(LRP1)*CRZ32
          JU = LRP1
          DO 130 JR=1,LR
            JU = JU - 1
            SUMA = SUMA + CR(JR)*UPOL(JU)
            SUMB = SUMB + DR(JR)*UPOL(JU)
  130     CONTINUE
          RDEN=RDEN*RFN2
          TB = -TB
          IF (WK(1).GT.0.0D0) TB = ABS(TB)
          IF(RDEN.LT.TOL) GO TO 131
          ASUM = ASUM + SUMA*TB
          BSUM = BSUM + SUMB*TB
          GO TO 140
  131     IF(ISETA.EQ.1) GO TO 132
          IF(ABS(SUMA).LT.TOL) ISETA=1
          ASUM=ASUM+SUMA*TB
  132     IF(ISETB.EQ.1) GO TO 133
          IF(ABS(SUMB).LT.RELB) ISETB=1
          BSUM=BSUM+SUMB*TB
  133     IF(ISETA.EQ.1 .AND. ISETB.EQ.1) GO TO 150
  140   CONTINUE
  150   TB = WK(5)
        IF (WK(1).GT.0.0D0) TB = -TB
        BSUM = BSUM/TB
C
  160   CONTINUE
        CALL FUNJY(WK(6), WK(5), WK(4), FI, DFI)
        TA=1.0D0/TOL
        TB=D1MACH(1)*TA*1.0D+3
        IF(ABS(FI).GT.TB) GO TO 165
        FI=FI*TA
        DFI=DFI*TA
        PHI=PHI*TOL
  165   CONTINUE
        Y(JN) = FLGJY*PHI*(FI*ASUM+DFI*BSUM)/WK(7)
        FN = FN - FLGJY
  170 CONTINUE
      RETURN
      END
