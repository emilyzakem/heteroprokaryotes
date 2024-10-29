#ifdef ALLOW_DARWIN

CBOP
C    !ROUTINE: DARWIN_DIAGS.h
C    !INTERFACE:
C #include DARWIN_DIAGS.h

C    !DESCRIPTION:
C Contains indices into diagnostics array

      integer iPP
      integer iNfix
      integer iDenit
      integer iDenitN
      integer iPPplank
      integer iGRplank
      integer iConsDIN
      integer iConsPO4
      integer iConsSi
      integer iConsFe
Cejz add:
      integer iprodPOC
      integer iprodPOC2
      integer iprodPOC3
      integer iprodDOC
      integer iprodDOC2
      integer iprodDOC3
      integer iprodDOC4
      integer iprodDOC5
      integer iconsPOC
      integer iconsPOC2
      integer iconsPOC3
      integer iconsDOC
      integer iconsDOC2
      integer iconsDOC3
      integer iconsDOC4
      integer iconsDOC5
      integer iconsPON
      integer iconsPON2
      integer iconsPON3
      integer iconsDON
      integer iconsDON2
      integer iconsDON3
      integer iconsDON4
      integer iconsDON5
      integer iPupNH4
      integer iPupNO2
      integer iPupNO3
      integer iPupPO4
      integer iBupNH4
      integer iBupPO4
      integer itest
      integer darwin_nDiag
      PARAMETER(iPP=     1)
      PARAMETER(iNfix=   2)
      PARAMETER(iDenit=  3)
      PARAMETER(iDenitN= 4)
      PARAMETER(iConsPO4=5)
      PARAMETER(iConsSi= 6)
      PARAMETER(iConsFe= 7)
      PARAMETER(iConsDIN=8)
      PARAMETER(iprodPOC= 9)
      PARAMETER(iprodPOC2=10)
      PARAMETER(iprodPOC3=11)
      PARAMETER(iprodDOC= 12)
      PARAMETER(iprodDOC2=13)
      PARAMETER(iprodDOC3=14)
      PARAMETER(iprodDOC4=15)
      PARAMETER(iprodDOC5=16)
      PARAMETER(iconsPOC= 17)
      PARAMETER(iconsPOC2=18)
      PARAMETER(iconsPOC3=19)
      PARAMETER(iconsDOC= 20)
      PARAMETER(iconsDOC2=21)
      PARAMETER(iconsDOC3=22)
      PARAMETER(iconsDOC4=23)
      PARAMETER(iconsDOC5=24)
      PARAMETER(iconsPON= 25)
      PARAMETER(iconsPON2=26)
      PARAMETER(iconsPON3=27)
      PARAMETER(iconsDON= 28)
      PARAMETER(iconsDON2=29)
      PARAMETER(iconsDON3=30)
      PARAMETER(iconsDON4=31)
      PARAMETER(iconsDON5=32)
      PARAMETER(iPupNH4=  33)
      PARAMETER(iPupNO2=  34)
      PARAMETER(iPupNO3=  35)
      PARAMETER(iPupPO4=  36)
      PARAMETER(iBupNH4=  37)
      PARAMETER(iBupPO4=  38)
      PARAMETER(itest=  39)
      PARAMETER(iPPplank=40)
      PARAMETER(iGRplank=iPPplank+nPPplank)
      PARAMETER(darwin_nDiag=iGRplank+nGRplank-1)

CEOP
#endif /* ALLOW_DARWIN */
