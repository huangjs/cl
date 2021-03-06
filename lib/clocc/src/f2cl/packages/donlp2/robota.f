C  CODED BY: HANS D. MITTELMANN <BECK@PLATO.LA.ASU.EDU>
C
      BLOCK DATA
      DOUBLE PRECISION C11,C12,C13,C21,C22,C23,C31,C32,C33
      COMMON/COEFF/C11,C12,C13,C21,C22,C23,C31,C32,C33
      DATA C11,C12,C13,C21,C22,C23,C31,C32,C33/2.D0,8.D0,250.D0,
     &3.D0,18.D0,650.D0,4.D0,50.D0,1000.D0/
      END
      SUBROUTINE SETUP
      INCLUDE 'O8COMM.INC'
      EPSX=1.D-7
      RETURN
      END
      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INCLUDE 'ROBOTDIM.INC'
      INTEGER I,J,M
      DOUBLE PRECISION XST0(XDIM),BB(XDIM),PHIL,PHIL1,PHIL2,
     &VI(BDM,XDIM),VI1(BDM,XDIM),VI2(BDM,XDIM),V33(BDM),
     &V11(BDM),V12(BDM),V13(BDM),V21(BDM),V22(BDM),V23(BDM),
     &V31(BDM),V32(BDM),C11,C12,C13,C21,C22,C23,C31,C32,C33,H,TP,
     &TH11,TH12,TH13,TH21,TH22,TH23,TH31,TH32,TH33
      COMMON/COEFF/C11,C12,C13,C21,C22,C23,C31,C32,C33
      COMMON/VEC/VI,VI1,VI2,V11,V12,V13,V21,V22,V23,V31,V32,V33
      COMMON/BB/BB
C************* OTHER DATA DECLARATION FOR CASES A AND B
C*** FOR REASONS OF COMPATIBILITY WITH F77STD INITIALIZATION DONE IN
C*** BLOCK DATA AND BELOW
CA     DATA (XST0(I),I=1,XDIM)/XDIM*1.491400623321533D0/
CB     DATA (XST0(I),I=1,XDIM)/XDIM*4.377146244049071D0/
CC     DATA (XST0(I),I=1,XDIM)/XDIM*1.920426464080810D0/
CA     DATA C11,C12,C13,C21,C22,C23,C31,C32,C33/2.D0,8.D0,250.D0,
CA    &3.D0,18.D0,650.D0,4.D0,50.D0,1000.D0/
CB     DATA C11,C12,C13,C21,C22,C23,C31,C32,C33/1.D0,3.D0,100.D0,
CB    &1.D0,3.D0,100.D0,1.D0,3.D0,100.D0/
CC      DATA C11,C12,C13,C21,C22,C23,C31,C32,C33/2.D0,8.D0,25.D0,
CC     &3.D0,18.D0,65.D0,4.D0,50.D0,100.D0/
CA     NAME='ROBOTA'
CB     NAME='ROBOTB'
      N=XDIM
      M=BDM
      DO I=1,XDIM
        XST0(I)=1.491400623321533D0
      ENDDO
      NAME='ROBOTA'
      NH=0
      NG=M*18
      H=1.D0/DBLE(N-1)
      SILENT=.FALSE.
      ANALYT=.TRUE.
      PROU=10
      MEU=20
      EPSDIF=0.D0
      DEL0=1.D0
      TAU0=1.D4
      DO J=1,M
C       TP=DBLE(J)/DBLE(M+1)
        TP=DBLE(J-1)/DBLE(M-1)
        V11(J)=TH11(TP)
        V12(J)=TH12(TP)
        V13(J)=TH13(TP)
        V21(J)=TH21(TP)
        V22(J)=TH22(TP)
        V23(J)=TH23(TP)
        V31(J)=TH31(TP)
        V32(J)=TH32(TP)
        V33(J)=TH33(TP)
        DO I=1,N
          VI(J,I)=PHIL(I,TP)
          VI1(J,I)=PHIL1(I,TP)
          VI2(J,I)=PHIL2(I,TP)
        ENDDO
      ENDDO
      DO I=1,N
        X(I)=XST0(I)
        BB(I)=H
      ENDDO
      BB(1)=.5D0*H
      BB(N)=BB(1)
      BB(2)=23.D0*H/24.D0
      BB(N-1)=BB(2)
      DO J=0,NH+NG
        GUNIT(1,J)=-1
        GUNIT(2,J)=0
        GUNIT(3,J)=0
      ENDDO
      RETURN
      END

      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'ROBOTDIM.INC'
      DOUBLE PRECISION X(*),FX,BB(XDIM)
      COMMON/BB/BB
      INTEGER I
      ICF=ICF+1
      FX=0.D0
      DO I=1,N
      FX=FX+BB(I)*X(I)
      ENDDO
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'ROBOTDIM.INC'
      DOUBLE PRECISION X(*),BB(XDIM),GRADF(*)
      COMMON/BB/BB
      INTEGER I
      ICGF=ICGF+1
      DO I=1,N
        GRADF(I)=BB(I)
      ENDDO
      RETURN
      END

      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),HXI
      INTEGER I
      CRES(I)=CRES(I)+1
      RETURN
      END

      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'ROBOTDIM.INC'
      INTEGER I,IP,K,J
      DOUBLE PRECISION X(*),GXI,C11,C12,C13,C21,C22,C23,C31,C32,C33,
     &VI(BDM,XDIM),
     &VI1(BDM,XDIM),VI2(BDM,XDIM),SUM,SUM1,SUM2,V11(BDM),
     &V12(BDM),V13(BDM),
     &V21(BDM),V22(BDM),V23(BDM),V31(BDM),V32(BDM),V33(BDM)
      COMMON/COEFF/C11,C12,C13,C21,C22,C23,C31,C32,C33
      COMMON/VEC/VI,VI1,VI2,V11,V12,V13,V21,V22,V23,V31,V32,V33
      IF ( GUNIT(1,I+NH) .EQ. -1 ) CRES(I+NH)=CRES(I+NH)+1
      IP=(I-1)/18+1
      K=I-18*(IP-1)
      SUM=0.D0
      SUM1=0.D0
      SUM2=0.D0
      DO J=1,N
      SUM=SUM+VI(IP,J)*X(J)
      SUM1=SUM1+VI1(IP,J)*X(J)
      SUM2=SUM2+VI2(IP,J)*X(J)
      ENDDO
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), K
   1  CONTINUE
      GXI=C11*SUM-V11(IP)
      RETURN
   2  CONTINUE
      GXI=C11*SUM+V11(IP)
      RETURN
   3  CONTINUE
      GXI=C12*SUM**3-(V12(IP)*SUM-V11(IP)*SUM1)
      RETURN
   4  CONTINUE
      GXI=C12*SUM**3+(V12(IP)*SUM-V11(IP)*SUM1)
      RETURN
   5  CONTINUE
      GXI=C13*SUM**5-(V13(IP)*SUM**2-3.D0*V12(IP)*SUM*SUM1
     &+3.D0*V11(IP)*SUM1**2+V11(IP)*SUM*SUM2)
      RETURN
   6  CONTINUE
      GXI=C13*SUM**5+(V13(IP)*SUM**2-3.D0*V12(IP)*SUM*SUM1
     &+3.D0*V11(IP)*SUM1**2+V11(IP)*SUM*SUM2)
      RETURN
   7  CONTINUE
      GXI=C21*SUM-V21(IP)
      RETURN
   8  CONTINUE
      GXI=C21*SUM+V21(IP)
      RETURN
   9  CONTINUE
      GXI=C22*SUM**3-(V22(IP)*SUM-V21(IP)*SUM1)
      RETURN
  10  CONTINUE
      GXI=C22*SUM**3+(V22(IP)*SUM-V21(IP)*SUM1)
      RETURN
  11  CONTINUE
      GXI=C23*SUM**5-(V23(IP)*SUM**2-3.D0*V22(IP)*SUM*SUM1
     &+3.D0*V21(IP)*SUM1**2+V21(IP)*SUM*SUM2)
      RETURN
  12  CONTINUE
      GXI=C23*SUM**5+(V23(IP)*SUM**2-3.D0*V22(IP)*SUM*SUM1
     &+3.D0*V21(IP)*SUM1**2+V21(IP)*SUM*SUM2)
      RETURN
  13  CONTINUE
      GXI=C31*SUM-V31(IP)
      RETURN
  14  CONTINUE
      GXI=C31*SUM+V31(IP)
      RETURN
  15  CONTINUE
      GXI=C32*SUM**3-(V32(IP)*SUM-V31(IP)*SUM1)
      RETURN
  16  CONTINUE
      GXI=C32*SUM**3+(V32(IP)*SUM-V31(IP)*SUM1)
      RETURN
  17  CONTINUE
      GXI=C33*SUM**5-(V33(IP)*SUM**2-3.D0*V32(IP)*SUM*SUM1
     &+3.D0*V31(IP)*SUM1**2+V31(IP)*SUM*SUM2)
      RETURN
  18  CONTINUE
      GXI=C33*SUM**5+(V33(IP)*SUM**2-3.D0*V32(IP)*SUM*SUM1
     &+3.D0*V31(IP)*SUM1**2+V31(IP)*SUM*SUM2)
      RETURN
      END

      SUBROUTINE EGRADH(I,XL,GRADXL)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION XL(NX),GRADXL(NX)
      IF ( GUNIT(1,I) .NE. 1 ) CGRES(I)=CGRES(I)+1
      RETURN
      END
      SUBROUTINE EGRADG(I,X,GRADX)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'ROBOTDIM.INC'
      INTEGER I,IP,K,J
      DOUBLE PRECISION X(NX),GRADX(NX)
      DOUBLE PRECISION C11,C12,C13,C21,C22,C23,C31,C32,C33,VI(BDM,XDIM),
     &VI1(BDM,XDIM),VI2(BDM,XDIM),SUM,SUM1,SUM2,V11(BDM),
     &V12(BDM),V13(BDM),
     &V21(BDM),V22(BDM),V23(BDM),V31(BDM),V32(BDM),V33(BDM)
      COMMON/COEFF/C11,C12,C13,C21,C22,C23,C31,C32,C33
      COMMON/VEC/VI,VI1,VI2,V11,V12,V13,V21,V22,V23,V31,V32,V33
      IF ( GUNIT(1,I+NH) .NE. 1 ) CGRES(I+NH)=CGRES(I+NH)+1
      IP=(I-1)/18+1
      K=I-18*(IP-1)
      SUM=0.D0
      SUM1=0.D0
      SUM2=0.D0
      DO J=1,N
      SUM=SUM+VI(IP,J)*X(J)
      SUM1=SUM1+VI1(IP,J)*X(J)
      SUM2=SUM2+VI2(IP,J)*X(J)
      ENDDO
      GOTO (1,1,3,4,5,6,7,7,9,10,11,12,13,13,15,16,17,18), K
   1  CONTINUE
      DO J=1,N
      GRADX(J)=C11*VI(IP,J)
      ENDDO
      RETURN
   3  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C12*SUM**2*VI(IP,J)-(V12(IP)*VI(IP,J)
     &-V11(IP)*VI1(IP,J))
      ENDDO
      RETURN
   4  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C12*SUM**2*VI(IP,J)+(V12(IP)*VI(IP,J)
     &-V11(IP)*VI1(IP,J))
      ENDDO
      RETURN
   5  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C13*SUM**4*VI(IP,J)-(2.D0*V13(IP)*SUM*
     &VI(IP,J)-3.D0*V12(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V11(IP)*SUM1*VI1(IP,J)+V11(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
   6  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C13*SUM**4*VI(IP,J)+(2.D0*V13(IP)*SUM*
     &VI(IP,J)-3.D0*V12(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V11(IP)*SUM1*VI1(IP,J)+V11(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
   7  CONTINUE
      DO J=1,N
      GRADX(J)=C21*VI(IP,J)
      ENDDO
      RETURN
   9  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C22*SUM**2*VI(IP,J)-(V22(IP)*VI(IP,J)
     &-V21(IP)*VI1(IP,J))
      ENDDO
      RETURN
  10  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C22*SUM**2*VI(IP,J)+(V22(IP)*VI(IP,J)
     &-V21(IP)*VI1(IP,J))
      ENDDO
      RETURN
  11  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C23*SUM**4*VI(IP,J)-(2.D0*V23(IP)*SUM*
     &VI(IP,J)-3.D0*V22(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V21(IP)*SUM1*VI1(IP,J)+V21(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
  12  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C23*SUM**4*VI(IP,J)+(2.D0*V23(IP)*SUM*
     &VI(IP,J)-3.D0*V22(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V21(IP)*SUM1*VI1(IP,J)+V21(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
  13  CONTINUE
      DO J=1,N
      GRADX(J)=C31*VI(IP,J)
      ENDDO
      RETURN
  15  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C32*SUM**2*VI(IP,J)-(V32(IP)*VI(IP,J)
     &-V31(IP)*VI1(IP,J))
      ENDDO
      RETURN
  16  CONTINUE
      DO J=1,N
      GRADX(J)=3.D0*C32*SUM**2*VI(IP,J)+(V32(IP)*VI(IP,J)
     &-V31(IP)*VI1(IP,J))
      ENDDO
      RETURN
  17  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C33*SUM**4*VI(IP,J)-(2.D0*V33(IP)*SUM*
     &VI(IP,J)-3.D0*V32(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V31(IP)*SUM1*VI1(IP,J)+V31(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
  18  CONTINUE
      DO J=1,N
      GRADX(J)=5.D0*C33*SUM**4*VI(IP,J)+(2.D0*V33(IP)*SUM*
     &VI(IP,J)-3.D0*V32(IP)*(SUM1*VI(IP,J)+SUM*VI1(IP,J))
     &+6.D0*V31(IP)*SUM1*VI1(IP,J)+V31(IP)*
     &(SUM2*VI(IP,J)+SUM*VI2(IP,J)))
      ENDDO
      RETURN
      END

      FUNCTION F(T)
      DOUBLE PRECISION F,T
      F=T**3*((6.D0*T-15.D0)*T+10.D0)
      RETURN
      END
      FUNCTION F1(T)
      DOUBLE PRECISION F1,T
      F1=30.D0*T*T*((T-2.D0)*T+1.D0)
      RETURN
      END
      FUNCTION F2(T)
      DOUBLE PRECISION F2,T
      F2=60.D0*T*((2.D0*T-3.D0)*T+1.D0)
      RETURN
      END
      FUNCTION F3(T)
      DOUBLE PRECISION F3,T
      F3=(360.D0*T-360.D0)*T+60.D0
      RETURN
      END
      FUNCTION TH11(T)
      DOUBLE PRECISION TH11,T,F1
      TH11=1.5D0*F1(T)
      RETURN
      END
      FUNCTION TH12(T)
      DOUBLE PRECISION TH12,T,F2
      TH12=1.5D0*F2(T)
      RETURN
      END
      FUNCTION TH13(T)
      DOUBLE PRECISION TH13,T,F3
      TH13=1.5D0*F3(T)
      RETURN
      END
      FUNCTION TH21(T)
      DOUBLE PRECISION TH21,T,F,F1
      TH21=-.5D0*(COS(4.7D0*F(T))*4.7D0*F1(T))
      RETURN
      END
      FUNCTION TH22(T)
      DOUBLE PRECISION TH22,T,F,F1,F2
      TH22=-.5D0*(-SIN(4.7D0*F(T))*(4.7D0*F1(T))**2+COS(4.7D0*F(T))*
     &4.7D0*F2(T))
      RETURN
      END
      FUNCTION TH23(T)
      DOUBLE PRECISION TH23,T,F,F1,F2,F3
      TH23=-.5D0*(-COS(4.7D0*F(T))*(4.7D0*F1(T))**3-SIN(4.7D0*F(T))*
     &3.D0*4.7D0**2*F1(T)*F2(T)+COS(4.7D0*F(T))*F3(T)*4.7D0)
      RETURN
      END
      FUNCTION TH31(T)
      DOUBLE PRECISION TH31,T,F1
      TH31=-1.3D0*F1(T)
      RETURN
      END
      FUNCTION TH32(T)
      DOUBLE PRECISION TH32,T,F2
      TH32=-1.3D0*F2(T)
      RETURN
      END
      FUNCTION TH33(T)
      DOUBLE PRECISION TH33,T,F3
      TH33=-1.3D0*F3(T)
      RETURN
      END
      FUNCTION PHIL(I,T)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION PHIL,T,H
      INTEGER I,K
      H=1.D0/DBLE(N-1)
      K=T/H+1.D-6+1
      IF(I.LE.K-2.OR.I.GE.K+3) THEN
      PHIL=0.D0
      ELSE
      GOTO (10,20,30,40), K-I+3
  10  PHIL=(T-DBLE(K-1)*H)**3/6.D0/H**3
      RETURN
  20  PHIL=1.D0/6.D0+(T-DBLE(K-1)*H)*.5D0/H+(T-DBLE(K-1)*H)**2*.5D0/H**2
     &    -(T-DBLE(K-1)*H)**3*.5D0/H**3
      RETURN
  30  PHIL=1.D0/6.D0+(DBLE(K)*H-T)*.5D0/H+(DBLE(K)*H-T)**2*.5D0/H**2
     &    -(DBLE(K)*H-T)**3*.5D0/H**3
      RETURN
  40  PHIL=(DBLE(K)*H-T)**3/6.D0/H**3
      ENDIF
      RETURN
      END
      FUNCTION PHIL1(I,T)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,K
      DOUBLE PRECISION PHIL1,T,H
      H=1.D0/DBLE(N-1)
      K=T/H+1.D-6+1
      IF(I.LE.K-2.OR.I.GE.K+3) THEN
      PHIL1=0.D0
      ELSE
      GOTO (10,20,30,40), K-I+3
  10  PHIL1=(T-DBLE(K-1)*H)**2/2.D0/H**3
      RETURN
  20  PHIL1=.5D0/H+(T-DBLE(K-1)*H)/H**2-(T-DBLE(K-1)*H)**2*1.5D0/H**3
      RETURN
  30  PHIL1=-.5D0/H-(DBLE(K)*H-T)/H**2+(DBLE(K)*H-T)**2*1.5D0/H**3
      RETURN
  40  PHIL1=-(DBLE(K)*H-T)**2/2.D0/H**3
      ENDIF
      RETURN
      END
      FUNCTION PHIL2(I,T)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,K
      DOUBLE PRECISION PHIL2,T,H
      H=1.D0/DBLE(N-1)
      K=T/H+1.D-6+1
      IF(I.LE.K-2.OR.I.GE.K+3) THEN
      PHIL2=0.D0
      ELSE
      GOTO (10,20,30,40), K-I+3
  10  PHIL2=(T-DBLE(K-1)*H)/H**3
      RETURN
  20  PHIL2=1.D0/H**2-(T-DBLE(K-1)*H)*3.D0/H**3
      RETURN
  30  PHIL2=1.D0/H**2-(DBLE(K)*H-T)*3.D0/H**3
      RETURN
  40  PHIL2=(DBLE(K)*H-T)/H**3
      ENDIF
      RETURN
      END

