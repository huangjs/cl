      SUBROUTINE SETUP
      RETURN
      END
C     THIS IS EXAMPLE HIMMELBLAU 13, BOX PARAMETER IDENTIFICATION PROBLEM
      BLOCK DATA
      INCLUDE 'O8BLOC.INC'
      INTEGER I,J
      DATA NAME/'BOXPARID'/
      DATA (X(I),I=1,12)/2.52D0,2.D0,37.5D0,9.25D0,6.8D0,7*0.0D0/
      DATA N/12/ , NH/7/ , NG/16/
      DATA DEL0/0.1D0/ ,TAU0/1.0D0/ ,TAU/.1D0/
      DATA (GUNIT(1,J),J=8,23)/16*1/,(GUNIT(3,I),I=8,23)/8*1,8*-1/,
     F     (GUNIT(2,I),I=8,23)/1,2,3,4,5,6,7,8,
     F     1,2,3,4,5,6,7,8/
     F     ,((GUNIT(I,J),I=1,3),J=0,7)/-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0/
      END
      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      SILENT=.FALSE.
      ANALYT=.TRUE.
      EPSDIF=0.D0
      PROU=10
      MEU=20
      NRESET=N
      CALL STARTX(X)
      RETURN
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION FX,X(*),XK(5)
      SAVE XK
      DATA XK/925.548252D3,-61.9688432D3,23.3088196D0,-27.097648D3,
     1        -50.843766D3/
      ICF=ICF+1
      FX=-(50.D0*X(9)+9.583D0*X(10)+20.D0*X(11)+15.D0*X(12)-
     1     852.96D3-38.1D3*(X(2)+0.01D0*X(3)) +XK(1)+XK(2)*X(2)+
     2     XK(3)*X(3)+XK(4)*X(4)+XK(5)*X(5))*X(1)-15.D0*X(6)
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*),XK(5)
      SAVE XK
      DATA XK/925.548252D3,-61.9688432D3,23.3088196D0,-27.097648D3,
     1        -50.843766D3/
      ICGF=ICGF+1
      GRADF(1)=-(50.D0*X(9)+9.583D0*X(10)+20.D0*X(11)+15.D0*X(12)-
     1     852.96D3-38.1D3*(X(2)+0.01D0*X(3)) +XK(1)+XK(2)*X(2)+
     2     XK(3)*X(3)+XK(4)*X(4)+XK(5)*X(5))
      GRADF(2)=38.1D3*X(1)-XK(2)*X(1)
      GRADF(3)=-XK(3)*X(1) +.381D3*X(1)
      GRADF(4)=-XK(4)*X(1)
      GRADF(5)=-XK(5)*X(1)
      GRADF(6)=-15.D0
      GRADF(7)= 0.0D0
      GRADF(8)= 0.0D0
      GRADF(9)=-X(1)*50.D0
      GRADF(10)=-X(1)*9.583D0
      GRADF(11)=-X(1)*20.D0
      GRADF(12)=-X(1)*15.D0
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,K,II,IIK
      DOUBLE PRECISION HXI,X(*),XK(30)
      SAVE XK
      DATA XK/-145.421402D3,2.9311506D3,-40.427932D0,5.106192D3,
     1         15.711360D3,-161.622577D3,4.17615328D3,2.8260078D0,
     2          9.200476D3,  13.160295D3,-21.6869194D3,123.56928D0,
     3        -21.1188894D0,706.834000D0,  2.8985730D3, 28.298388D3,
     4         60.8109600D0, 31.242116D0,329.5740000D0, -2.882082D3,
     5         74.0953845D3,-30.6262544D1,16.2436490D0, -3.094252D3,
     6        - 5.5662628D3,-26.237000D3, 99.D0,-0.42D0,1.3D3,2.1D3/
      CRES(I)=CRES(I)+1
      IF(I .EQ. 7)      GOTO 700
      II=(I-1)*5
      HXI=XK(II+1)
      DO      10      K=2,5
      IIK=II+K
      HXI=HXI+XK(IIK)*X(K)
   10 CONTINUE
      GOTO(100,200,200,200,200,600),I
 100  CONTINUE
      HXI=X(6)-HXI*X(1)
      GOTO 1000
 200  CONTINUE
      HXI=X(I+7)-HXI
      GOTO 1000
 600  CONTINUE
      HXI=X(8)-(HXI*X(1)+X(6)+X(7))
      GOTO 1000
 700  CONTINUE
      HXI=X(7)-(X(9)+X(10)+X(11))*X(1)
1000  CONTINUE
      RETURN
      END
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,K,II,IIK
      DOUBLE PRECISION X(*),GRADHI(*),XK(30)
      SAVE XK
      DATA XK/-145.421402D3,2.9311506D3,-40.427932D0,5.106192D3,
     1         15.711360D3,-161.622577D3,4.17615328D3,2.8260078D0,
     2          9.200476D3,  13.160295D3,-21.6869194D3,123.56928D0,
     3        -21.1188894D0,706.834000D0,  2.8985730D3, 28.298388D3,
     4         60.8109600D0, 31.242116D0,329.5740000D0, -2.882082D3,
     5         74.0953845D3,-30.6262544D1,16.2436490D0, -3.094252D3,
     6        - 5.5662628D3,-26.237000D3, 99.D0,-0.42D0,1.3D3,2.1D3/
      CGRES(I)=CGRES(I)+1
      DO      10   K=1,12
      GRADHI(K)=0.D0
  10  CONTINUE
      GOTO(100,200,200,200,200,100,700),I
  100 CONTINUE
      II=(I-1)*5
      GRADHI(1)=-(XK(II+1)+XK(II+2)*X(2)+XK(II+3)*X(3)+XK(II+4)*X(4)
     1         +XK(II+5)*X(5))
      DO      110      K=2,5
      IIK=II+K
      GRADHI(K)=-XK(IIK)*X(1)
  110 CONTINUE
      IF(I .EQ. 6)      GOTO 600
      GRADHI(6)=1.D0
      GOTO 1000
  200 CONTINUE
      II=(I-1)*5
      DO      210      K=2,5
      IIK=II+K
      GRADHI(K)=-XK(IIK)
  210 CONTINUE
      GRADHI(I+7)=1.D0
      GOTO 1000
  600 CONTINUE
      GRADHI(6)=-1.D0
      GRADHI(7)=-1.D0
      GRADHI(8)= 1.D0
      GOTO 1000
  700 CONTINUE
      GRADHI( 1)=-(X(9)+X(10) +X(11))
      GRADHI( 7)= 1.D0
      GRADHI( 9)=-X(1)
      GRADHI(10)=-X(1)
      GRADHI(11)=-X(1)
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION GXI,X(*),XK(16)
      SAVE XK
      DATA XK/0.D0,1.2D0,20.0D0,9.D0, 6.5D0,0.D0,0.D0,0.D0,
     1        5.D0,2.4D0,6.D1,  9.3D0,7.0D0,294.D3,294.D3,277.2D3/
      IF(I .GT. 8)      GOTO 100
      GXI=X(I)-XK(I)
      RETURN
  100 CONTINUE
      GXI=XK(I)-X(I-8)
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION X(*) ,GRADGI(*)
      DO      100      J=1,12
      GRADGI(J)=0.D0
  100 CONTINUE
      IF(I .GT.  8)      GOTO 200
      GRADGI(I)=1.0D0
      RETURN
  200 CONTINUE
      GRADGI(I-8)=-1.D0
      RETURN
      END
      SUBROUTINE STARTX(X)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(*) ,XK(30)
      INTEGER I
      SAVE XK
      DATA XK/-145.421402D3,2.9311506D3,-40.427932D0,5.106192D3,
     1         15.711360D3,-161.622577D3,4.17615328D3,2.8260078D0,
     2          9.200476D3,  13.160295D3,-21.6869194D3,123.56928D0,
     3        -21.1188894D0,706.834000D0,  2.8985730D3, 28.298388D3,
     4         60.8109600D0, 31.242116D0,329.5740000D0, -2.882082D3,
     5         74.0953845D3,-30.6262544D1,16.2436490D0, -3.094252D3,
     6        - 5.5662628D3,-26.237000D3, 99.D0,-0.42D0,1.3D3,2.1D3/
      X(6)=XK(1)
      DO      10     I=2,5
      X(6)=X(6)+XK(I)*X(I)
   10 CONTINUE
      X(6)=X(6)*X(1)
      X(9)=XK(6)
      DO      11   I=2,5
      X(9)=X(9)+XK(I+5)*X(I)
   11 CONTINUE
      X(10)=XK(11)
      DO      12   I=2,5
      X(10)=X(10)+XK(I+10)*X(I)
   12 CONTINUE
      X(11)=XK(16)
      DO      13   I=2,5
      X(11)=X(11)+XK(I+15)*X(I)
   13 CONTINUE
      X(12)=XK(21)
      DO      14   I=2,5
      X(12)=X(12)+XK(I+20)*X(I)
   14 CONTINUE
      X(7)=(X(9)+X(10)+X(11))*X(1)
      X( 8)=XK(26)
      DO      15   I=2,5
      X(8)= X(8) +XK(I+25)*X(I)
   15 CONTINUE
      X(8)= X(8)*X(1)+X(6) +X(7)
      RETURN
      END
