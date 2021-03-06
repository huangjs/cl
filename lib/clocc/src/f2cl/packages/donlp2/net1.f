      SUBROUTINE SETUP
C   NO SPECIAL USER DATA
      RETURN
      END
      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8FINT.INC'
      INTEGER I,J
      DOUBLE PRECISION XST0(9)
      DATA (XST0(I),I=1,9)
     F     /9*10.D0/
C   NAME IS IDENT OF THE EXAMPLE/USER AND CAN BE SET AT USERS WILL
C   THE FIRST CHARACTER MUST BE ALPHABETIC.  40 CHARACTERS MAXIMUM
       NAME='NET1AMPL'
C   X IS INITIAL GUESS AND ALSO HOLDS THE CURRENT SOLUTION
C   PROBLEM DIMENSION N=DIM(X), NH=DIM(H), NG=DIM(G)
      N=9
      NH=8
      NG=18
      ANALYT=.FALSE.
      EPSDIF=1.d-16
      EPSFCN=1.D-16
      TAUBND=1.D0
      BLOC=.FALSE.
C****** ANYTHING IS LINEAR
      DIFFTYPE=1
      PROU=10
      MEU=20
      NRESET=N
C   DEL0 AND TAU0: SEE BELOW
      DEL0=0.2D0
      TAU0=1.D0
      TAU=0.1D0
      DO I=1,N
      X(I)=XST0(I)
      ENDDO
C    GUNIT-ARRAY, SEE BELOW
      DO J=0,8
      GCONST(J)=.TRUE.
      GUNIT(1,J)=-1
      GUNIT(2,J)=0
      GUNIT(3,J)=0
      ENDDO
      DO J=9,26
      GCONST(J)=.TRUE.
      GUNIT(1,J)=1
      IF ( J .LE. 17 ) THEN
        GUNIT(2,J)=J-8
        GUNIT(3,J)=1
      ELSE
        GUNIT(2,J)=J-17
        GUNIT(3,J)=-1
      ENDIF
      ENDDO
      RETURN
      END

C     OBJECTIVE FUNCTION
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION FX,X(*)
      DOUBLE PRECISION COST(9)
      INTEGER I
      DATA COST/2.5D0,2.5D0,1.7D0,0.7D0,1.3D0,
     F   1.3D0,0.8D0,0.2D0,2.1D0/
      SAVE COST
      ICF=ICF+1
      FX=0.D0
      DO I=1,9
        FX=FX+COST(I)*X(I)
      ENDDO
      RETURN
      END
C     GRADIENT OF OBJECTIVE FUNCTION
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*)
      DOUBLE PRECISION COST(9)
      INTEGER J
      DATA COST/2.5D0,2.5D0,1.7D0,0.7D0,1.3D0,
     F   1.3D0,0.8D0,0.2D0,2.1D0/
      SAVE COST
      ICGF=ICGF+1
      DO       J=1,9
      GRADF(J)=COST(J)
      ENDDO
      RETURN
      END

C  COMPUTE THE I-TH EQUALITY CONSTAINT, VALUE IS HXI
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),HXI
      INTEGER I
      CRES(I)=CRES(I)+1
      GOTO (100,200,300,400,500,600,700,800),I
  100 CONTINUE
      HXI=450.D0-X(1)-X(2)
      RETURN
  200 CONTINUE
      HXI=X(1)-X(3)-X(4)-X(5)
      RETURN
  300 CONTINUE
      HXI=X(2)-X(6)-X(7)-X(8)-X(9)
      RETURN
  400 CONTINUE
      HXI=X(3)-90.D0
      RETURN
  500 CONTINUE
      HXI=X(4)+X(6)-120.D0
      RETURN
  600 CONTINUE
      HXI=X(5)+X(7)-120.D0
      RETURN
  700 CONTINUE
      HXI=X(8)-70.D0
      RETURN
  800 CONTINUE
      HXI=X(9)-50.D0
      RETURN
      END
C  COMPUTE THE GRADIENT OF THE I-TH EQUALITY CONSTRAINT
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),GRADHI(*)
C****** WE DO IT BY DIFFERENCING
      RETURN
      END

C COMPUTE THE I-TH INEQUALITY CONSTAINT, BOUNDS INCLUDED

      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION GXI
      DOUBLE PRECISION X(NX),OG(9),UG(9)
      SAVE UG,OG
      DATA OG/2*250.D0,7*100.D0/
      DATA UG/9*0.D0/
      IF ( I .LE. 9 ) THEN
        GXI=X(I)-UG(I)
      ELSE
        GXI=OG(I-9)-X(I-9)
      ENDIF
      RETURN
      END
C COMPUTE THE GRADIENT OF THE I-TH INEQUALITY CONSTAINT
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(NX) ,GRADGI(NX)
C***** ONLY BOUNDS
      RETURN
      END
