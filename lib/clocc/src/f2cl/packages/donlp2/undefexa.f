      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INTEGER I,J,NUSER
      PARAMETER (NUSER=2)
      DOUBLE PRECISION XST0(NUSER)
      DATA (XST0(I),I=1,NUSER) /
     F  1.D0,1.D0
     F       /
C   NAME IS IDENT OF THE EXAMPLE/USER AND CAN BE SET AT USERS WILL
C   THE FIRST CHARACTER MUST BE ALPHABETIC.  40 CHARACTERS MAXIMUM
       NAME='UNDEFEXA'
C   X IS INITIAL GUESS AND ALSO HOLDS THE CURRENT SOLUTION
C   PROBLEM DIMENSION N=DIM(X), NH=DIM(H), NG=DIM(G)
      N=NUSER
      NH=0
      NG=4
      ANALYT=.TRUE.
      COLD=.TRUE.
      SILENT=.FALSE.
      EPSDIF=1.D-8
      PROU=10
      MEU=20
C   DEL0 AND TAU0: SEE BELOW
      DEL0=1.D0
      TAU0=1.D0
      DO I=1,N
	X(I)=XST0(I)
      ENDDO
C    GUNIT-ARRAY, SEE DONLP2DOC.TXT
      DO J=0,2
	GUNIT(1,J)=-1
	GUNIT(2,J)=0
	GUNIT(3,J)=0
      ENDDO
      GUNIT(1,3)=1
      GUNIT(2,3)=1
      GUNIT(3,3)=1
      GUNIT(1,4)=1
      GUNIT(2,4)=2
      GUNIT(3,4)=1
C  GCONST-ARRAY:
      DO J=0,2
        GCONST(J)=.FALSE.
C  IF THE J-TH FUNCTION IS AFFINE LINEAR
      ENDDO
      GCONST(3)=.TRUE.
      GCONST(4)=.TRUE.
      RETURN
      END

C     OBJECTIVE FUNCTION
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),FX
      ICF=ICF+1
      IF ( X(1)*X(2) .LE. .5D0 ) THEN
        FFUERR=.TRUE.
        RETURN
      ENDIF
      FX=-EXP(X(1)*X(2)-1.D0)/(X(1)*X(2)-0.5D0)
      RETURN
      END
C     GRADIENT OF OBJECTIVE FUNCTION
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*)
C     USER DECLARATIONS, IF ANY ,FOLLOW
      INTEGER J
      ICGF=ICGF+1
      	GRADF(1)=-EXP(X(1)*X(2)-1.D0)*(1.D0-1.D0/(X(1)*X(2)-0.5D0))
     F   *X(2)/(X(1)*X(2)-0.5D0)
        GRADF(2)=-EXP(X(1)*X(2)-1.D0)*(1.D0-1.D0/(X(1)*X(2)-0.5D0))
     F   *X(1)/(X(1)*X(2)-0.5D0)
      RETURN
      END
C  COMPUTE THE I-TH EQUALITY CONSTAINT, VALUE IS HXI
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),HXI
      INTEGER I
      RETURN
      END
C  COMPUTE THE GRADIENT OF THE I-TH EQUALITY CONSTRAINT
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADHI(*)
      INTEGER I,J
      IF ( GUNIT(1,I) .NE. 1 ) CGRES(I)=CGRES(I)+1
      RETURN
      END

C COMPUTE THE I-TH INEQUALITY CONSTAINT, BOUNDS INCLUDED

      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GXI
      INTEGER I
      IF ( GUNIT(1,I+NH) .EQ. -1 ) CRES(I+NH)=CRES(I+NH)+1
      GOTO (100,200,300,400), I
  100 CONTINUE
      GXI=X(1)*X(2)-0.75D0
      RETURN
  200 CONTINUE
      GXI=-(X(2))**2+2.D0-X(1)
      RETURN
  300 CONTINUE
      GXI=X(1)
      RETURN
  400 CONTINUE
      GXI=X(2)
      RETURN
      END
C COMPUTE THE GRADIENT OF THE I-TH INEQUALITY CONSTRAINT
C NOT NECESSARY FOR BOUNDS, BUT CONSTANT GRADIENTS MUST BE SET
C HERE E.G. USING DCOPY FROM A DATA-FIELD
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*) ,GRADGI(*)
      INTEGER I,J
      IF ( GUNIT(1,I+NH) .NE. 1 ) CGRES(I+NH)=CGRES(I+NH)+1
      DO  J=1,NX
	GRADGI(J)=0.D0
      ENDDO
      GOTO(100,200),I
  100 CONTINUE
      GRADGI(1)=X(2)
      GRADGI(2)=X(1)
      RETURN
  200 CONTINUE
      GRADGI(1)=-1.D0
      GRADGI(2)=-2.D0*X(2)
      RETURN
      END
      SUBROUTINE SETUP
      INCLUDE 'O8COMM.INC'
C     CHANGE TERMINATION CRITERION
      EPSX=1.D-10
C     CHANGE I/O-CONTROL
      TE2=.TRUE.
C*** NOW YOU GET A DETAILED OUTPUT, BUT NO MATRICES
C    PLUS A LONG OUTPUT IN *.PRO
      RETURN
      END
