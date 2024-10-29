#include "DARWIN_OPTIONS.h"
Cz: add alpha_hydrol terms 
Cz add pom3: 2/15/22
Cz: NOTE: NOT including solubilPOM and hydrolPOM-- NEED TO REVAMP if so
C(first by working on conservation -- last I checked it wasn't working right with
CnewCNP. Then by incorporating solubilPOC2, solubilPOC3, etc.)
Cz add dom5: 12/14/21
Cz add dom2: second DOM starting 1/12/21
Cz no dom2 for type3 bact yet (eating both POM and DOM)
Cz no dom2 for implicit remin (with K rates set in data.darwin) 
Cz no dom2 for CDOM 
Cz: modify 10/15/21

CBOP
C !ROUTINE: DARWIN_PLANKTON
C !INTERFACE: ==========================================================
      SUBROUTINE DARWIN_PLANKTON(
     I     Ptr,
     U     gTr,
     O     chlout, diags,
     I     PAR, photoTempFunc, grazTempFunc, reminTempFunc,
     I     mortTempFunc, mort2TempFunc, uptakeTempFunc,
#ifdef DARWIN_DEBUG
     I     iG, jG, k, dT,
#endif
     I     myTime,myIter,myThid)

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE
#ifdef ALLOW_RADTRANS
#include "RADTRANS_SIZE.h"
#endif
#include "DARWIN_SIZE.h"
#include "DARWIN_INDICES.h"
#include "DARWIN_DIAGS.h"
#include "DARWIN_RADTRANS.h"
#include "DARWIN_PARAMS.h"
#include "DARWIN_TRAITS.h"

C !INPUT PARAMETERS: ===================================================
C  Ptr    :: darwin model tracers
C  PAR    :: PAR in uEin/s/m2
C         :: either non-spectral (tlam=1) or waveband total
C  myTime :: current time
C  myIter :: current iteration number
C  myThid :: thread number
      _RL Ptr(nDarwin)
      _RL PAR(nlam)
      _RL photoTempFunc(nplank)
      _RL reminTempFunc
      _RL uptakeTempFunc
      _RL grazTempFunc(nplank)
      _RL mortTempFunc
      _RL mort2TempFunc
      INTEGER myThid, myIter
      _RL myTime
#ifdef DARWIN_DEBUG
      INTEGER iG, jG, k
      _RL dT
#endif

C !INPUT/OUTPUT PARAMETERS: ============================================
C  gTr    :: accumulates computed tendencies
      _RL gTr(nDarwin)

C !OUTPUT PARAMETERS: ==================================================
C  chlout :: computed acclimated chlorophyll if not dynamic
      _RL chlout(nPhoto)
      _RL diags(darwin_nDiag)
CEOP

#ifdef ALLOW_DARWIN

c !LOCAL VARIABLES: ====================================================
      INTEGER j, l
      INTEGER jz, jp

      _RL DIC
      _RL NH4
      _RL NO2
      _RL NO3
      _RL PO4
      _RL SiO2
      _RL FeT
      _RL DOC
      _RL DON
      _RL DOP
      _RL DOFe
      _RL DOC2
      _RL DON2
      _RL DOP2
      _RL DOFe2
      _RL DOC3
      _RL DON3
      _RL DOP3
      _RL DOFe3
      _RL DOC4
      _RL DON4
      _RL DOP4
      _RL DOFe4
      _RL DOC5
      _RL DON5
      _RL DOP5
      _RL DOFe5
      _RL POC
      _RL PON
      _RL POP
      _RL POFe
      _RL POC2
      _RL PON2
      _RL POP2
      _RL POFe2
      _RL POC3
      _RL PON3
      _RL POP3
      _RL POFe3
      _RL POSi
      _RL pIC
      _RL O2

      _RL X(nplank)
      _RL Qc(nplank)
      _RL Qn(nplank)
      _RL Qp(nplank)
      _RL Qsi(nplank)
      _RL Qfe(nplank)
#ifdef DARWIN_ALLOW_CHLQUOTA
      _RL QChl(nPhoto)
#endif

      _RL regQ

      _RL limitpCO2
      _RL limitNH4
      _RL limitNO2
      _RL limitNO3
      _RL fracNH4
      _RL fracNO2
      _RL fracNO3
      _RL limitn
      _RL limitp
      _RL limitsi
      _RL limitfe
      _RL limitnut
      _RL limitI
      _RL ngrow

      _RL muPON
      _RL muPOC
      _RL muPOP
      _RL muPOFe
      _RL muDON
      _RL muDOC
      _RL muDOP
      _RL muDOFe
      _RL muO
      _RL mu

      _RL uptakeDIC
      _RL uptakeNH4
      _RL uptakeNO2
      _RL uptakeNO3
      _RL uptakeN
      _RL uptakePO4
      _RL uptakeSiO2
      _RL uptakeFeT
      _RL consumDIC
      _RL consumDIC_PIC
      _RL consumNH4
      _RL consumNO2
      _RL consumNO3
      _RL consumPO4
      _RL consumSiO2
      _RL consumFeT

      _RL uptakePON
      _RL uptakePOP
      _RL uptakePOC
      _RL uptakePOFe
      _RL uptakePON2
      _RL uptakePOP2
      _RL uptakePOC2
      _RL uptakePOFe2
      _RL uptakePON3
      _RL uptakePOP3
      _RL uptakePOC3
      _RL uptakePOFe3
      _RL uptakeDON
      _RL uptakeDOP
      _RL uptakeDOC
      _RL uptakeDOFe
Cz add dom
      _RL uptakeDON2
      _RL uptakeDOP2
      _RL uptakeDOC2
      _RL uptakeDOFe2
      _RL uptakeDON3
      _RL uptakeDOP3
      _RL uptakeDOC3
      _RL uptakeDOFe3
      _RL uptakeDON4
      _RL uptakeDOP4
      _RL uptakeDOC4
      _RL uptakeDOFe4
      _RL uptakeDON5
      _RL uptakeDOP5
      _RL uptakeDOC5
      _RL uptakeDOFe5
      _RL uptakeO2

      _RL respPON
      _RL respPOP
      _RL respPOC
      _RL respPOFe
      _RL respPON2
      _RL respPOP2
      _RL respPOC2
      _RL respPOFe2
      _RL respPON3
      _RL respPOP3
      _RL respPOC3
      _RL respPOFe3
      _RL respPOSi
      _RL respDON
      _RL respDOP
      _RL respDOC
      _RL respDOFe
Cz add dom
      _RL respDON2
      _RL respDOP2
      _RL respDOC2
      _RL respDOFe2
      _RL respDON3
      _RL respDOP3
      _RL respDOC3
      _RL respDOFe3
      _RL respDON4
      _RL respDOP4
      _RL respDOC4
      _RL respDOFe4
      _RL respDON5
      _RL respDOP5
      _RL respDOC5
      _RL respDOFe5

      _RL hydrolPON
      _RL hydrolPOP
      _RL hydrolPOC
      _RL hydrolPOFe
      _RL solubilPON
      _RL solubilPOP
      _RL solubilPOC
      _RL solubilPOFe
      _RL hydrolPON2
      _RL hydrolPOP2
      _RL hydrolPOC2
      _RL hydrolPOFe2
      _RL solubilPON2
      _RL solubilPOP2
      _RL solubilPOC2
      _RL solubilPOFe2
      _RL hydrolPON3
      _RL hydrolPOP3
      _RL hydrolPOC3
      _RL hydrolPOFe3
      _RL solubilPON3
      _RL solubilPOP3
      _RL solubilPOC3
      _RL solubilPOFe3

      _RL consumPON
      _RL consumPOP
      _RL consumPOC
      _RL consumPOFe
      _RL consumPON2
      _RL consumPOP2
      _RL consumPOC2
      _RL consumPOFe2
      _RL consumPON3
      _RL consumPOP3
      _RL consumPOC3
      _RL consumPOFe3
      _RL consumPOSi
      _RL consumDON
      _RL consumDOP
      _RL consumDOC
      _RL consumDOFe
Cz add dom      
      _RL consumDON2
      _RL consumDOP2
      _RL consumDOC2
      _RL consumDOFe2
      _RL consumDON3
      _RL consumDOP3
      _RL consumDOC3
      _RL consumDOFe3
      _RL consumDON4
      _RL consumDOP4
      _RL consumDOC4
      _RL consumDOFe4
      _RL consumDON5
      _RL consumDOP5
      _RL consumDOC5
      _RL consumDOFe5
      _RL consumO2

      _RL inhibNH4

      _RL alpha_I
      _RL alpha_I_growth
      _RL PCm
      _RL PC
      _RL acclim
      _RL chl2c
      _RL growth
      _RL rhochl
      _RL Ek
      _RL EkoverE

      _RL synthChl

      _RL reminDOC
      _RL reminDON
      _RL reminDOP
      _RL reminDOFe
Cz add dom2      
      _RL reminDOC2
      _RL reminDON2
      _RL reminDOP2
      _RL reminDOFe2
      _RL reminDOC3
      _RL reminDON3
      _RL reminDOP3
      _RL reminDOFe3
      _RL reminDOC4
      _RL reminDON4
      _RL reminDOP4
      _RL reminDOFe4
      _RL reminDOC5
      _RL reminDON5
      _RL reminDOP5
      _RL reminDOFe5
      _RL reminPOC
      _RL reminPON
      _RL reminPOP
      _RL reminPOFe
      _RL reminPOC2
      _RL reminPON2
      _RL reminPOP2
      _RL reminPOFe2
      _RL reminPOC3
      _RL reminPON3
      _RL reminPOP3
      _RL reminPOFe3
      _RL reminPOSi
      _RL disscPIC

      _RL prodNO2
      _RL prodNO3

      _RL PARtot

      _RL tmp

Cejz add nitrifier params
      _RL muNH4 
      _RL muNO2 
      _RL resptoNO2
      _RL resptoNO3

Cejz add 
      _RL EPS
      _RL NOfac 

Cz add for dom2
      _RL wdom1 
      _RL wdom2 
      _RL wdom3 
      _RL wdom4 
      _RL wdom5 
      _RL domcons(5,nplank) !5 DOM
Cz add for pom3
      _RL wpom1
      _RL wpom2
      _RL wpom3
      _RL pomcons(3,nplank) !3 POM
      _RL wpomtodom1
      _RL wpomtodom2
      _RL wpomtodom3

Cejz add for newCNP
      _RL mu_Norg 
      _RL mu_Porg 
      _RL mu_Feorg 
      _RL muPO4 
      _RL muFeT 
      _RL Rup_NC 
      _RL Rup_PC 
      _RL Rup_FeC 
      _RL growth_Norg 
      _RL growth_Porg 
      _RL growth_Feorg 
      _RL pmaxDIP 
      _RL pmaxFeT 
      _RL ksatDIP 
      _RL ksatFe 

#ifdef DARWIN_ALLOW_CDOM
      _RL CDOM
      _RL reminPOC_CDOM
      _RL reminPON_CDOM
      _RL reminPOP_CDOM
      _RL reminPOFe_CDOM
      _RL degrCDOM_DOC
      _RL degrCDOM_DON
      _RL degrCDOM_DOP
      _RL degrCDOM_DOFe
#endif

#ifdef DARWIN_ALLOW_DENIT
      _RL denit, denitNH4
#endif

C for grazing

      _RL regQc, regQn, regQp, regQfe
      _RL sumprey, sumpref, grazphy

      _RL preygraz   (nplank)
      _RL preygrazexp(nplank)
      _RL predgrazc  (nplank)
#ifdef DARWIN_ALLOW_NQUOTA
      _RL predgrazn  (nplank)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
      _RL predgrazp  (nplank)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
      _RL predgrazfe (nplank)
#endif

      _RL predexpc, predexpn, predexpp, predexpfe
      _RL graz2OC, graz2ON, graz2OP, graz2OFe
      _RL graz2POC, graz2PON, graz2POP, graz2POSi, graz2POFe
      _RL graz2PIC

      _RL expfrac

      _RL Xe
      _RL mortX
      _RL mortX2

      _RL exude_DOC
      _RL exude_DON
      _RL exude_DOP
      _RL exude_DOFe

      _RL exude_PIC
      _RL exude_POC
      _RL exude_PON
      _RL exude_POP
      _RL exude_POSi
      _RL exude_POFe

      _RL mort_c(nplank)

      _RL respir
      _RL respir_c

#ifdef DARWIN_ALLOW_CDOM
      _RL graz2CDOM, exude_CDOM
#endif

C==== precompute a few things ==========================================

      PARtot = SUM(PAR)

Cejz add
      EPS = 1D-38
Cejz      #NOfac = 1.414 !10^(1/3) factor for NO uptake kinetics
      NOfac = 1. _d 0

Cejz add for newCNP:
      pmaxDIP = 0.139*80/86400 _d 0 !.077*Vcell^-0.27 mmolP/mmolC/s*CP 
      pmaxFeT = 0.139*80/86400 _d 0 !mol/mol 
      ksatDIP = 0.0125D-01 
      ksatFe = 0.0125D-04 

Cz: From: /Users/zakem/Documents/PriME/Bhet/LognormalDists/
Cz: MakeVmax_for_v10.jl:

Cz n = 3
Cz mu = 1.0
Cz sigma = 1
Cz Vmax = [0.1, 1.0, 10.0]
Cz Weights = [0.06185256604964818, 0.8762948679007038,
C0.06185256604964809]
Cz n = 5
Cz mu = 1.0
Cz sigma = 1
Cz Vmax = [0.03162277660168379, 0.1778279410038923, 1.0,
C5.623413251903491, 31.622776601683793]
Cz Weights = [0.0017644996041189446, 0.15467771133320474,
C0.6871155781253527, 0.15467771133320468, 0.0017644996041189446]

Cz: wdom1 and wpom1 have the highest vmax (order matters)

      wdom1 = 0.0017644996041189446 _d 0
      wdom2 = 0.15467771133320474 _d 0
      wdom3 = 0.6871155781253527 _d 0
      wdom4 = 0.15467771133320474 _d 0
      wdom5 = 1. - wdom1 - wdom2 - wdom3 - wdom4 

Cz for pom 
Cz Here, have same ratio of DOM3:DOM2 for POM2:POM1 (0.687/0.155 = 4.4)
Cz Calc: w2 = [ 0.15467771133320474, 0.6871155781253527, 0.15467771133320474] --> w2 = w2./sum(w2) which is very close to orig 
      wpom1 = 0.15522550200688534 _d 0 
      wpom2 = 0.6895489959862293 _d 0
      wpom3 = 1. - wpom1 - wpom2 !0.2789832149654164
Cz hydrolization: 
Cz for hydrolyzation of POM to DOM (each POM goes to 3
Cclasses of DOM): (also need to add to 1)
      wpomtodom1 = 0.06185256604964818 _d 0
      wpomtodom2 = 0.8762948679007038 _d 0 
      wpomtodom3 = 1. - wpomtodom1 - wpomtodom2 !0.2789832149654164

Cz assign domcons size = (3,nplank) for ALL heterotrophic bact (aerobic and anaerobic)
Cz: need to align with actual assignment (see data.ptracers/data.traits)
      domcons(:,:) = 0
      domcons(1,10) = 1 !DOM1 aerobic copiotrophs
      domcons(2,11) = 1 !DOM2
      domcons(3,12) = 1 !DOM3
      domcons(4,13) = 1 !DOM4
      domcons(5,14) = 1 !DOM5
      domcons(1,15) = 1 !DOM1 aerobic oligotrophs 
      domcons(2,16) = 1 !DOM2
      domcons(3,17) = 1 !DOM3
      domcons(4,18) = 1 !DOM4
      domcons(5,19) = 1 !DOM5
      domcons(1,23) = 1 !DOM1 anaerobic
      domcons(2,24) = 1 !DOM2
      domcons(3,25) = 1 !DOM3
      domcons(4,26) = 1 !DOM4
      domcons(5,27) = 1 !DOM5

Cz assign pomcons size = (3,nplank) for ALL heterotrophic bact (aerobic and anaerobic)
      pomcons(:,:) = 0
      pomcons(1,7) = 1 !POM1 aerobic 
      pomcons(2,8) = 1 !POM2
      pomcons(3,9) = 1 !POM3
      pomcons(1,20) = 1 !POM1 anaerobic
      pomcons(2,21) = 1 !POM2
      pomcons(3,22) = 1 !POM3

C==== make all bio fields non-negative and compute quotas ==============

      DIC  = MAX(0., Ptr(iDIC))
      NH4  = MAX(0., Ptr(iNH4))
      NO2  = MAX(0., Ptr(iNO2))
      NO3  = MAX(0., Ptr(iNO3))
      PO4  = MAX(0., Ptr(iPO4))
      SiO2 = MAX(0., Ptr(iSiO2))
      FeT  = MAX(0., Ptr(iFeT))

      DOC  = MAX(0., Ptr(iDOC))
      DON  = MAX(0., Ptr(iDON))
      DOP  = MAX(0., Ptr(iDOP))
      DOFe = MAX(0., Ptr(iDOFe))

      DOC2  = MAX(0., Ptr(iDOC2))
      DON2  = MAX(0., Ptr(iDON2))
      DOP2  = MAX(0., Ptr(iDOP2))
      DOFe2 = MAX(0., Ptr(iDOFe2))

      DOC3  = MAX(0., Ptr(iDOC3))
      DON3  = MAX(0., Ptr(iDON3))
      DOP3  = MAX(0., Ptr(iDOP3))
      DOFe3 = MAX(0., Ptr(iDOFe3))

      DOC4  = MAX(0., Ptr(iDOC4))
      DON4  = MAX(0., Ptr(iDON4))
      DOP4  = MAX(0., Ptr(iDOP4))
      DOFe4 = MAX(0., Ptr(iDOFe4))

      DOC5  = MAX(0., Ptr(iDOC5))
      DON5  = MAX(0., Ptr(iDON5))
      DOP5  = MAX(0., Ptr(iDOP5))
      DOFe5 = MAX(0., Ptr(iDOFe5))

      PIC  = MAX(0., Ptr(iPIC))
      POC  = MAX(0., Ptr(iPOC))
      PON  = MAX(0., Ptr(iPON))
      POP  = MAX(0., Ptr(iPOP))
      POFe = MAX(0., Ptr(iPOFe))
      POC2  = MAX(0., Ptr(iPOC2))
      PON2  = MAX(0., Ptr(iPON2))
      POP2  = MAX(0., Ptr(iPOP2))
      POFe2 = MAX(0., Ptr(iPOFe2))
      POC3  = MAX(0., Ptr(iPOC3))
      PON3  = MAX(0., Ptr(iPON3))
      POP3  = MAX(0., Ptr(iPOP3))
      POFe3 = MAX(0., Ptr(iPOFe3))
      POSi = MAX(0., Ptr(iPOSi))
#ifdef DARWIN_ALLOW_CARBON
      O2   = MAX(0., Ptr(iO2))
#endif
#ifdef DARWIN_ALLOW_CDOM
      CDOM = MAX(0., Ptr(iCDOM))
#endif

C==== make all bio fields non-negative and compute quotas ==============

      DO j = 1, nplank
C fixed carbon quota, for now 1.0 (may change later)
        Qc(j) = 1.0
        X(j) = MAX(0., Ptr(ic+j-1))/Qc(j)
C other elementsj get quota from corresponding ptracer or set to fixed
c ratio if not variable.
#ifdef DARWIN_ALLOW_NQUOTA
        IF (X(j) .GT. 0 _d 0) THEN
          Qn(j) = MAX(0 _d 0, Ptr(in+j-1))/X(j)
        ELSE
          Qn(j) = Qnmax(j)
        ENDIF
#else
        Qn(j) = R_NC(j)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
        IF (X(j) .GT. 0 _d 0) THEN
          Qp(j) = MAX(0 _d 0, Ptr(ip+j-1))/X(j)
        ELSE
          Qp(j) = Qpmax(j)
        ENDIF
#else
        Qp(j) = R_PC(j)
#endif
#ifdef DARWIN_ALLOW_SIQUOTA
        IF (X(j) .GT. 0 _d 0) THEN
          Qsi(j) = MAX(0 _d 0, Ptr(isi+j-1))/X(j)
        ELSE
          Qsi(j) = Qsimax(j)
        ENDIF
#else
        Qsi(j) = R_SiC(j)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
        IF (X(j) .GT. 0 _d 0) THEN
          Qfe(j) = MAX(0 _d 0, Ptr(ife+j-1))/X(j)
        ELSE
          Qfe(j) = Qfemax(j)
        ENDIF
#else
        Qfe(j) = R_FeC(j)
#endif
      ENDDO
#ifdef DARWIN_ALLOW_CHLQUOTA
      DO j = 1, nPhoto
        IF (X(j) .GT. 0 _d 0) THEN
          QChl(j) = MAX(0 _d 0, Ptr(iChl+j-1))/X(j)
        ELSE
          QChl(j) = chl2cmax(j)
        ENDIF
      ENDDO
#endif

      consumDIC  = 0.0
      consumDIC_PIC = 0.0
      consumNH4  = 0.0
      consumNO2  = 0.0
      consumNO3  = 0.0
      consumPO4  = 0.0
      consumSiO2 = 0.0
      consumFeT  = 0.0
      consumPON  = 0.0
      consumPOP  = 0.0
      consumPOC = 0.0
      consumPOFe  = 0.0
      consumPON2  = 0.0
      consumPOP2  = 0.0
      consumPOC2 = 0.0
      consumPOFe2  = 0.0
      consumPON3  = 0.0
      consumPOP3  = 0.0
      consumPOC3 = 0.0
      consumPOFe3  = 0.0
      consumPOSi  = 0.0
      consumDON  = 0.0
      consumDOP  = 0.0
      consumDOC = 0.0
      consumDOFe  = 0.0
Cz add      
      consumDON2  = 0.0
      consumDOP2  = 0.0
      consumDOC2 = 0.0
      consumDOFe2  = 0.0
      consumDON3  = 0.0
      consumDOP3  = 0.0
      consumDOC3 = 0.0
      consumDOFe3  = 0.0
      consumDON4  = 0.0
      consumDOP4  = 0.0
      consumDOC4 = 0.0
      consumDOFe4  = 0.0
      consumDON5  = 0.0
      consumDOP5  = 0.0
      consumDOC5 = 0.0
      consumDOFe5  = 0.0
      consumO2 = 0.0
      reminPON  = 0.0
      reminPOP  = 0.0
      reminPOC = 0.0
      reminPOFe  = 0.0
      reminPON2  = 0.0
      reminPOP2  = 0.0
      reminPOC2 = 0.0
      reminPOFe2  = 0.0
      reminPON3  = 0.0
      reminPOP3  = 0.0
      reminPOC3 = 0.0
      reminPOFe3  = 0.0
      reminPOSi  = 0.0
      reminDON  = 0.0
      reminDOP  = 0.0
      reminDOC = 0.0
      reminDOFe  = 0.0
      reminDON2  = 0.0
      reminDOP2  = 0.0
      reminDOC2 = 0.0
      reminDOFe2  = 0.0
      reminDON3  = 0.0
      reminDOP3  = 0.0
      reminDOC3 = 0.0
      reminDOFe3  = 0.0
      reminDON4  = 0.0
      reminDOP4  = 0.0
      reminDOC4 = 0.0
      reminDOFe4  = 0.0
      reminDON5  = 0.0
      reminDOP5  = 0.0
      reminDOC5 = 0.0
      reminDOFe5  = 0.0
      solubilPON  = 0.0
      solubilPOP  = 0.0
      solubilPOC = 0.0
      solubilPOFe  = 0.0
      solubilPON2  = 0.0
      solubilPOP2  = 0.0
      solubilPOC2 = 0.0
      solubilPOFe2  = 0.0
      solubilPON3  = 0.0
      solubilPOP3  = 0.0
      solubilPOC3 = 0.0
      solubilPOFe3  = 0.0
      prodNO2 = 0.0
      prodNO3 = 0.0

      DO j = 1, nPhoto
        chlout(j) = 0.0 _d 0
      ENDDO

      DO l=1,darwin_nDiag
        diags(l) = 0.0
      ENDDO

C=======================================================================
C==== phytoplankton ====================================================

      DO j = 1, nPhoto
       IF (isPhoto(j) .NE. 0) THEN
C fixed carbon quota, for now 1.0 (may change later)
C other elements: get quota from corresponding ptracer or set to fixed
c ratio if not variable.

C==== uptake and nutrient limitation ===================================
C       for quota elements, growth is limiteed by available quota,
C       for non-quota elements, by available nutrients in medium

C       to not use PO4, ..., set ksatPO4=0 and vmaxPO4=0 (if DARWIN_ALLOW_PQUOTA)
C       or R_PC=0 (if not)
C       the result will be limitp = 1, uptakePO4 = 0

c phosphorus
        IF (ksatPO4(j) .GT. 0 _d 0) THEN
          limitp = PO4/(PO4 + ksatPO4(j))
        ELSE
          limitp = 1 _d 0
        ENDIF
#ifdef DARWIN_ALLOW_PQUOTA
        regQ = MAX(0., MIN(1., (Qpmax(j)-Qp(j))/
     &                         (Qpmax(j)-Qpmin(j)) ))
        regQ = regQ**hillnumUptake
        uptakePO4 = vmaxPO4(j) * limitp * regQ *
     &                       uptakeTempFunc * X(j)
c       normalized Droop limitation
        limitp = MIN(1., (1.0-Qpmin(j)/MAX(Qpmin(j), Qp(j)))/
     &                   (1.0-Qpmin(j)/Qpmax(j)))
#endif

c silica
        IF (ksatSiO2(j) .GT. 0 _d 0) THEN
          limitsi = SiO2/(SiO2 + ksatSiO2(j))
        ELSE
          limitsi = 1 _d 0
        ENDIF
#ifdef DARWIN_ALLOW_SIQUOTA
        regQ = MAX(0., MIN(1., (Qsimax(j) - Qsi(j))/
     &                         (Qsimax(j) - Qsimin(j)) ))
        regQ = regQ**hillnumUptake
        uptakeSiO2 = vmaxSiO2(j) * limitsi * regQ *
     &                                 uptakeTempFunc * X(j)

c       linear limitation
        limitsi = MAX(0., MIN(1., (Qsi(j) - Qsimin(j))/
     &                         (Qsimax(j) - Qsimin(j)) ))
#endif

c iron
        IF (ksatFeT(j) .GT. 0 _d 0) THEN
          limitfe = FeT/(FeT + ksatFeT(j))
        ELSE
          limitfe = 1 _d 0
        ENDIF
#ifdef DARWIN_ALLOW_FEQUOTA
        regQ = MAX(0., MIN(1., (Qfemax(j)-Qfe(j))/
     &                         (Qfemax(j)-Qfemin(j)) ))
        regQ = regQ**hillnumUptake
        uptakeFeT = vmaxFeT(j) * limitfe * regQ *
     &                       uptakeTempFunc * X(j)

c       normalized Droop limitation
        limitfe = MIN(1., (1.0-Qfemin(j)/MAX(Qfemin(j), Qfe(j)))/
     &                    (1.0-Qfemin(j)/Qfemax(j)))
#endif

c nitrogen
#ifdef DARWIN_ALLOW_NQUOTA
c       have nitrogen quota
        inhibNH4 = EXP(-amminhib(j)*NH4)
        limitNH4 = NH4/(NH4 + ksatNH4(j))
        limitNO2 = NO2/(NO2 + ksatNO2(j))*inhibNH4
        limitNO3 = NO3/(NO3 + ksatNO3(j))*inhibNH4
        regQ = MAX(0., MIN(1., (Qnmax(j)-Qn(j))/
     &                         (Qnmax(j)-Qnmin(j)) ))
        regQ = regQ**hillnumUptake
        uptakeNH4 = vmaxNH4(j)*limitNH4*regQ*uptakeTempFunc*X(j)
        uptakeNO2 = vmaxNO2(j)*limitNO2*regQ*uptakeTempFunc*X(j)
        uptakeNO3 = vmaxNO3(j)*limitNO3*regQ*uptakeTempFunc*X(j)
#ifdef DARWIN_ALLOW_FEQUOTA
#ifdef DARWIN_NITRATE_FELIMIT
        uptakeNO3 = uptakeNO3 * limitfe
#endif
#endif
        uptakeN = MAX(uptakeNH4 + uptakeNO2 + uptakeNO3,
     &                vmaxN(j)*regQ*uptakeTempFunc*X(j)*diazo(j))

c       linear limitation
        limitn = MAX(0., MIN(1., (Qn(j) - Qnmin(j))/
     &                        (Qnmax(j) - Qnmin(j)) ))
#else /* not DARWIN_ALLOW_NQUOTA */
        inhibNH4 = EXP(-amminhib(j)*NH4)
        limitNH4 = useNH4(j)*NH4/(NH4 + ksatNH4(j))
        limitNO2 = useNO2(j)*NO2/
     &   (NO2 + combNO(j)*(NO3 + ksatNO3(j) - ksatNO2(j)) + ksatNO2(j))*
     &   inhibNH4
        limitNO3 = useNO3(j)*NO3/
     &   (combNO(j)*NO2 + NO3 + ksatNO3(j))*inhibNH4
        limitn = limitNH4 + limitNO2 + limitNO3
C       normalize to sum 1
        IF (limitn .GT. 0 _d 0) THEN
          fracNH4 = limitNH4/limitn
          fracNO2 = limitNO2/limitn
          fracNO3 = limitNO3/limitn
        ELSE
          fracNH4 = 0 _d 0
          fracNO2 = 0 _d 0
          fracNO3 = 0 _d 0
        ENDIF
C if diazo, all fracN* == 0 but want no N limitation
        limitn = MIN(1.0, limitn + diazo(j))
#endif /* DARWIN_ALLOW_NQUOTA */
c        IF (limitn .GT. 0.0) THEN
c          ngrow = ((10*4+2)/(10*4 + 2*limitNH4/limitn +
c     &                       8*limitNO2/limitn + 10*limitNO3/limitn))
c        ELSE
        ngrow = 1.0
c        ENDIF

        limitnut = MIN(limitn, limitp, limitsi)
#ifndef DARWIN_ALLOW_FEQUOTA
        limitnut = MIN(limitnut, limitfe)
#endif

        limitpCO2 = 1.

C==== growth ===========================================================
#ifdef DARWIN_ALLOW_GEIDER

        alpha_I = 0 _d 0
        DO l = 1, nlam
          alpha_I = alpha_I + alphachl(j,l)*PAR(l)
        ENDDO
C       NB: for quota, PCmax(j) = Vmax_c(j)
        PCm = PCmax(j)*limitnut*photoTempFunc(j)*limitpCO2

        IF (PCm .GT. 0.0) THEN
          acclim = MAX(chl2cmin(j), MIN(chl2cmax(j),
     &             chl2cmax(j)/(1+(chl2cmax(j)*alpha_I)/(2*PCm)) ))
        ELSE
          acclim = chl2cmin(j)
        ENDIF

#ifdef DARWIN_ALLOW_CHLQUOTA
C       quotas are already relative to carbon
        chl2c = QChl(j)
#else
        chl2c = acclim
#endif

        alpha_I_growth = alpha_I
C a la quota
#ifdef DARWIN_ALLOW_FEQUOTA
        alpha_I_growth = alpha_I_growth*limitfe
#endif

C       carbon-specific growth rate
C       PC = PCm*(1-EXP(-alpha_I_growth*chl2c/MAX(EPS, PCm)))
        IF (PCm .GT. 0.0 .AND. PARtot .GT. PARmin) THEN
          PC = PCm*(1-EXP(-alpha_I_growth*chl2c/PCm))
        ELSE
          PC = 0.0 _d 0
        ENDIF

        IF (inhibGeider(j) .GT. 0.0) THEN
C         "total" PAR:
          tmp = alpha_I/alpha_mean(j)
          Ek = PCm/(chl2c*alpha_mean(j))
          EkoverE = Ek / tmp
          IF (tmp .GE. Ek) THEN
            PC = PC*EkoverE*inhibGeider(j)
          ENDIF
        ENDIF

#else /* not DARWIN_ALLOW_GEIDER */

        IF (PARtot .GT. PARmin) THEN
C         only 1 waveband without DARWIN_ALLOW_GEIDER
          limitI = (1.0 _d 0 - EXP(-PARtot*ksatPAR(j)))*
     &             EXP(-PARtot*kinhPAR(j)) * normI(j)
          PC = PCmax(j)*limitnut*limitI*photoTempFunc(j)*limitpCO2
        ELSE
          PC = 0.0 _d 0
        ENDIF
        synthChl = 0.0

#endif /* DARWIN_ALLOW_GEIDER */

        growth = PC*ngrow*X(j)

        uptakeDIC = growth

C non-quota elements are taken up with fixed stoichiometry
#ifndef DARWIN_ALLOW_NQUOTA
        uptakeN = growth*R_NC(j)
        uptakeNH4 = uptakeN*fracNH4
        uptakeNO2 = uptakeN*fracNO2
        uptakeNO3 = uptakeN*fracNO3
#endif
#ifndef DARWIN_ALLOW_PQUOTA
        uptakePO4 = growth*R_PC(j)
#endif
#ifndef DARWIN_ALLOW_SIQUOTA
        uptakeSiO2 = growth*R_SiC(j)
#endif
#ifndef DARWIN_ALLOW_FEQUOTA
        uptakeFeT = growth*R_FeC(j)
#endif

C==== chlorophyll ======================================================
#ifdef DARWIN_ALLOW_GEIDER
# ifdef DARWIN_ALLOW_CHLQUOTA
#  ifdef DARWIN_ALLOW_NQUOTA
C       Geider 1998
        IF (PARtot .GT. PARmin) THEN
         IF (alpha_I*chl2c .GT. 0.0 _d 0) THEN
C          rhochl = Chl2Nmax/(alpha_I*chl2c)*ngrow ???
          rhochl = Chl2Nmax*PC*ngrow/(alpha_I*chl2c)
         ELSE
          rhochl = Chl2Nmax
         ENDIF
         synthChl = rhochl*uptakeN
        ELSE
         synthChl = 0 _d 0
        ENDIF
CTODO: this seems wrong!  Should only happen when PARtot.GT.PARmin
        uptakeDIC = uptakeDIC - synthcost*uptakeN
#  else /* not DARWIN_ALLOW_NQUOTA */
#   ifdef DARWIN_GEIDER_RHO_SYNTH
        IF (alpha_I .GT. 0.0 _d 0 .AND. acclim .GT. 0.0 _d 0) THEN
          rhochl = Chl2Cmax(j)*PC*ngrow/(alpha_I*acclim)
        ELSE
          rhochl = 0.0 _d 0    ! should be Chl2Cmax(j) ?????
        ENDIF
        synthChl = rhochl*growth +
     &                      acclimtimescl(j)*(acclim-chl2c)*X(j)
#   else
        synthChl = acclim*growth +
     &                      acclimtimescl(j)*(acclim-chl2c)*X(j)
#   endif
#  endif /* not DARWIN_ALLOW_NQUOTA */
# else /* not DARWIN_ALLOW_CHLQUOTA */
        chlout(j) = X(j)*Qc(j)*chl2c
        synthChl = 0.0
# endif /* DARWIN_ALLOW_CHLQUOTA */
#endif /* DARWIN_ALLOW_GEIDER */
C=======================================================================
        consumDIC_PIC = consumDIC_PIC + uptakeDIC*R_PICPOC(j)
        consumDIC  = consumDIC  + uptakeDIC
        consumNH4  = consumNH4  + uptakeNH4
        consumNO2  = consumNO2  + uptakeNO2
        consumNO3  = consumNO3  + uptakeNO3
        consumPO4  = consumPO4  + uptakePO4
        consumSiO2 = consumSiO2 + uptakeSiO2
        consumFeT  = consumFeT  + uptakeFeT

        diags(iPP) = diags(iPP) + growth
        IF (j .LE. nPPplank) THEN
Cejz          diags(iPPplank+j-1) = diags(iPPplank+j-1) + growth
Cejz: specific growth rate:
              diags(iPPplank+j-1) = diags(iPPplank+j-1) + PC*ngrow
        ENDIF
        IF (diazo(j) .GT. 0.0 _d 0) THEN
         diags(iNfix)=diags(iNfix)+uptakeN-uptakeNH4-uptakeNO2-uptakeNO3
        ENDIF

        diags(iPupNH4) = diags(iPupNH4) + uptakeNH4
        diags(iPupNO2) = diags(iPupNO2) + uptakeNO2
        diags(iPupNO3) = diags(iPupNO3) + uptakeNO3
        diags(iPupPO4) = diags(iPupNH4) + uptakePO4
C=======================================================================

        gTr(ic+j-1)=gTr(ic+j-1)  + uptakeDIC
#ifdef DARWIN_ALLOW_NQUOTA
        gTr(in+j-1)=gTr(in+j-1)  + uptakeN
#endif
#ifdef DARWIN_ALLOW_PQUOTA
        gTr(ip+j-1)=gTr(ip+j-1)  + uptakePO4
#endif
#ifdef DARWIN_ALLOW_SIQUOTA
        gTr(isi+j-1)=gTr(isi+j-1) + uptakeSiO2
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
        gTr(ife+j-1)=gTr(ife+j-1) + uptakeFeT
#endif
#ifdef DARWIN_ALLOW_CHLQUOTA
        gTr(iChl+j-1)=gTr(iChl+j-1) + synthChl
#endif

#ifdef DARWIN_DEBUG
        IF (iG.eq.iDEBUG.and.jG.eq.jDEBUG) THEN
         print*,'uptake',myiter,k,j,
     &     uptakeDIC,
     &     uptakeNH4,
     &     uptakeNO2,
     &     uptakeNO3,
     &     uptakeN,
     &     uptakePO4,
     &     uptakeSiO2,
     &     uptakeFeT
        ENDIF
#endif

C      isPhoto(j)
       ENDIF
C     j
      ENDDO

C=======================================================================
C==== bacteria =========================================================

      DO j = 1, nplank
       IF (bactType(j) .NE. 0) THEN

        uptakeO2  = 0. _d 0
        uptakeNO3 = 0. _d 0
        uptakePOC = 0. _d 0
        uptakePON = 0. _d 0
        uptakePOP = 0. _d 0
        uptakePOFe = 0. _d 0
        uptakePOC2 = 0. _d 0
        uptakePON2 = 0. _d 0
        uptakePOP2 = 0. _d 0
        uptakePOFe2 = 0. _d 0
        uptakePOC3 = 0. _d 0
        uptakePON3 = 0. _d 0
        uptakePOP3 = 0. _d 0
        uptakePOFe3 = 0. _d 0
        uptakeDOC = 0. _d 0
        uptakeDON = 0. _d 0
        uptakeDOP = 0. _d 0
        uptakeDOFe = 0. _d 0
Cz add dom
        uptakeDOC2 = 0. _d 0
        uptakeDON2 = 0. _d 0
        uptakeDOP2 = 0. _d 0
        uptakeDOFe2 = 0. _d 0
        uptakeDOC3 = 0. _d 0
        uptakeDON3 = 0. _d 0
        uptakeDOP3 = 0. _d 0
        uptakeDOFe3 = 0. _d 0
        uptakeDOC4 = 0. _d 0
        uptakeDON4 = 0. _d 0
        uptakeDOP4 = 0. _d 0
        uptakeDOFe4 = 0. _d 0
        uptakeDOC5 = 0. _d 0
        uptakeDON5 = 0. _d 0
        uptakeDOP5 = 0. _d 0
        uptakeDOFe5 = 0. _d 0
        hydrolPOC = 0. _d 0
        hydrolPON = 0. _d 0
        hydrolPOP = 0. _d 0
        hydrolPOFe = 0. _d 0
        hydrolPOC2 = 0. _d 0
        hydrolPON2 = 0. _d 0
        hydrolPOP2 = 0. _d 0
        hydrolPOFe2 = 0. _d 0
        hydrolPOC3 = 0. _d 0
        hydrolPON3 = 0. _d 0
        hydrolPOP3 = 0. _d 0
        hydrolPOFe3 = 0. _d 0
        respPOC = 0. _d 0
        respPON = 0. _d 0
        respPOP = 0. _d 0
        respPOFe = 0. _d 0
        respPOC2 = 0. _d 0
        respPON2 = 0. _d 0
        respPOP2 = 0. _d 0
        respPOFe2 = 0. _d 0
        respPOC3 = 0. _d 0
        respPON3 = 0. _d 0
        respPOP3 = 0. _d 0
        respPOFe3 = 0. _d 0
        respDOC = 0. _d 0
        respDON = 0. _d 0
        respDOP = 0. _d 0
        respDOFe = 0. _d 0
Cz add dom2
        respDOC2 = 0. _d 0
        respDON2 = 0. _d 0
        respDOP2 = 0. _d 0
        respDOFe2 = 0. _d 0
        respDOC3 = 0. _d 0
        respDON3 = 0. _d 0
        respDOP3 = 0. _d 0
        respDOFe3 = 0. _d 0
        respDOC4 = 0. _d 0
        respDON4 = 0. _d 0
        respDOP4 = 0. _d 0
        respDOFe4 = 0. _d 0
        respDOC5 = 0. _d 0
        respDON5 = 0. _d 0
        respDOP5 = 0. _d 0
        respDOFe5 = 0. _d 0
        growth = 0. _d 0
Cejz add nitrifier uptake and resp:
        uptakeDIC = 0. _d 0
        uptakeNH4 = 0. _d 0
        uptakeNO2 = 0. _d 0
        uptakePO4 = 0. _d 0
        uptakeFeT = 0. _d 0
        resptoNO2 = 0. _d 0
        resptoNO3 = 0. _d 0

        IF (isAerobic(j) .NE. 0) THEN
          muO = yieldO2(j)*pcoefO2*O2
        ELSEIF (isDenit(j) .NE. 0) THEN
          muO = yieldNO3(j)*pmaxDIN*NO3/(NO3 + ksatDIN)*reminTempFunc
        ENDIF

C       POM-consuming (particle-associated)
Cejz    Feb 2022: add three POM, hardwired for specialists. COULD do
Cjust one generalist for all POM... we know there's transfer from one
Cclass to the next.
Cejz    Dec 2021: make into newCNP! 
        IF (bactType(j) .EQ. 1) THEN
                  
Cejz Topt            
Cejz            PCm = PCmax(j)*reminTempFunc
            PCm = PCmax(j)*photoTempFunc(j)

Cejz: Uptake rates (not growth rates): 
               IF (pomcons(1,j) .EQ. 1) THEN
                  muPOC  = PCm*POC/(POC + ksatPOC(j))
                  muPON  = PCm*PON/(PON + ksatPON(j))
                  muPOP  = PCm*POP/(POP + ksatPOP(j))
                  muPOFe = PCm*POFe/(POFe + ksatPOFe(j))
               ENDIF
               IF (pomcons(2,j) .EQ. 1) THEN
                  muPOC  = PCm*POC2/(POC2 + ksatPOC(j))
                  muPON  = PCm*PON2/(PON2 + ksatPON(j))
                  muPOP  = PCm*POP2/(POP2 + ksatPOP(j))
                  muPOFe = PCm*POFe2/(POFe2 + ksatPOFe(j))
               ENDIF
               IF (pomcons(3,j) .EQ. 1) THEN
                  muPOC  = PCm*POC3/(POC3 + ksatPOC(j))
                  muPON  = PCm*PON3/(PON3 + ksatPON(j))
                  muPOP  = PCm*POP3/(POP3 + ksatPOP(j))
                  muPOFe = PCm*POFe3/(POFe3 + ksatPOFe(j))
               ENDIF
Cejz: inorganic uptake potential:
            muNH4 = pmaxDIN*NH4/(NH4 + ksatDIN)*reminTempFunc
            muPO4 = pmaxDIP*PO4/(PO4 + ksatDIP)*reminTempFunc
            muFeT = pmaxFeT*FeT/(FeT + ksatFe)*reminTempFunc
            mu = MIN(PCm, yield(j)*muPOC, muPON + muNH4, muPOP + 
     &               muPO4, muPOFe + muFeT, muO)
            growth = mu*X(j)
Cejz: org-limited growth determines actual inorganic uptake:
Cejz 3/31/22: add in POP+PO4 to mu_Norg? and vv?
            mu_Norg = MIN(PCm, yield(j)*muPOC, muPON, muO) 
            mu_Porg = MIN(PCm, yield(j)*muPOC, muPOP, muO) 
            mu_Feorg = MIN(PCm, yield(j)*muPOC, muPOFe, muO) 

            uptakeNH4 = MIN(muNH4, MAX((mu - mu_Norg), 0.))*
     &                  X(j)*R_NC(j)
            uptakePO4 = MIN(muPO4, MAX((mu - mu_Porg), 0.))*
     &                  X(j)*R_PC(j)
            uptakeFeT = MIN(muFeT, MAX((mu - mu_Feorg), 0.))*
     &                  X(j)*R_FeC(j)
                  
            diags(iBupNH4) = diags(iBupNH4) + uptakeNH4
            diags(iBupPO4) = diags(iBupNH4) + uptakePO4
Cejz: org-limited growth needed for uptake calc
            growth_Norg = mu_Norg*X(j)*R_NC(j)
            growth_Porg = mu_Porg*X(j)*R_PC(j)
            growth_Feorg = mu_Feorg*X(j)*R_FeC(j)
Cejz: calculate the ratios of uptake, which are preserved even for O2 limitation:
            Rup_NC  = MIN(R_NC(j), muPON/(muPOC + EPS))
            Rup_PC  = MIN(R_PC(j), muPOP/(muPOC + EPS))
            Rup_FeC  = MIN(R_FeC(j), muPOFe/(muPOC + EPS))

Cejz: uptake and respiration:
            uptakeO2 = isAerobic(j)*growth/yieldO2(j)
            uptakeNO3 = isDenit(j)*growth/yieldNO3(j)
               
                IF (pomcons(1,j) .EQ. 1) THEN
                  uptakePOC = alpha_hydrol*growth/yield(j)
                  uptakePON  = uptakePOC*Rup_NC
                  uptakePOP  = uptakePOC*Rup_PC
                  uptakePOFe  = uptakePOC*Rup_FeC
Cejz: with hydrol, O2/NO3 is only used for the part of POC that is metabolized:
                  respPOC = growth/yield(j) - growth 
                  respPON = growth/yield(j)*Rup_NC - growth_Norg
                  respPOP = growth/yield(j)*Rup_PC - growth_Porg
                  respPOFe = growth/yield(j)*Rup_FeC - growth_Feorg
C This is the part of POM that is hydrolized into DOM:
                  hydrolPOC = (alpha_hydrol-1.)*growth/yield(j)
                  hydrolPON  = hydrolPOC*Rup_NC
                  hydrolPOP  = hydrolPOC*Rup_PC
                  hydrolPOFe = hydrolPOC*Rup_FeC
               ENDIF
               IF (pomcons(2,j) .EQ. 1) THEN
                  uptakePOC2 = alpha_hydrol*growth/yield(j)
                  uptakePON2  = uptakePOC2*Rup_NC
                  uptakePOP2  = uptakePOC2*Rup_PC
                  uptakePOFe2  = uptakePOC2*Rup_FeC
Cejz: with hydrol, O2/NO3 is only used for the part of POC that is metabolized:
                  respPOC2 = growth/yield(j) - growth 
                  respPON2 = growth/yield(j)*Rup_NC - growth_Norg
                  respPOP2 = growth/yield(j)*Rup_PC - growth_Porg
                  respPOFe2 = growth/yield(j)*Rup_FeC - growth_Feorg
C This is the part of POM that is hydrolized into DOM:
                  hydrolPOC2 = (alpha_hydrol-1.)*growth/yield(j)
                  hydrolPON2  = hydrolPOC2*Rup_NC
                  hydrolPOP2  = hydrolPOC2*Rup_PC
                  hydrolPOFe2 = hydrolPOC2*Rup_FeC
               ENDIF
               IF (pomcons(3,j) .EQ. 1) THEN
                  uptakePOC3 = alpha_hydrol*growth/yield(j)
                  uptakePON3  = uptakePOC3*Rup_NC
                  uptakePOP3  = uptakePOC3*Rup_PC
                  uptakePOFe3  = uptakePOC3*Rup_FeC
Cejz: with hydrol, O2/NO3 is only used for the part of POC that is metabolized:
                  respPOC3 = growth/yield(j) - growth 
                  respPON3 = growth/yield(j)*Rup_NC - growth_Norg
                  respPOP3 = growth/yield(j)*Rup_PC - growth_Porg
                  respPOFe3 = growth/yield(j)*Rup_FeC - growth_Feorg
C This is the part of POM that is hydrolized into DOM:
                  hydrolPOC3 = (alpha_hydrol-1.)*growth/yield(j)
                  hydrolPON3  = hydrolPOC3*Rup_NC
                  hydrolPOP3  = hydrolPOC3*Rup_PC
                  hydrolPOFe3 = hydrolPOC3*Rup_FeC
               ENDIF


C       DOM-consuming (free-living):
Cz Dec 2021: newCNP parameterization!
Cz add DOM2 and DOM3 here ONLY (not in type 3 yet)
        ELSEIF (bactType(j) .EQ. 2) THEN

Cejz Topt            
Cejz            PCm = PCmax(j)*reminTempFunc
            PCm = PCmax(j)*photoTempFunc(j)

Cz add (hardwired domcons for now for SPECIALISTS only! (else need to
Csum growth, uptake, and resp for generalist j) 
               IF (domcons(1,j) .EQ. 1) THEN
                  muDON  = PCm*DON/(DON + ksatDON(j))
                  muDOC  = PCm*DOC/(DOC + ksatDOC(j))
                  muDOP  = PCm*DOP/(DOP + ksatDOP(j))
                  muDOFe = PCm*DOFe/(DOFe + ksatDOFe(j))
               ENDIF
               IF (domcons(2,j) .EQ. 1) THEN
                  muDON  = PCm*DON2/(DON2 + ksatDON(j))
                  muDOC  = PCm*DOC2/(DOC2 + ksatDOC(j))
                  muDOP  = PCm*DOP2/(DOP2 + ksatDOP(j))
                  muDOFe = PCm*DOFe2/(DOFe2 + ksatDOFe(j))
               ENDIF
               IF (domcons(3,j) .EQ. 1) THEN
                  muDON  = PCm*DON3/(DON3 + ksatDON(j))
                  muDOC  = PCm*DOC3/(DOC3 + ksatDOC(j))
                  muDOP  = PCm*DOP3/(DOP3 + ksatDOP(j))
                  muDOFe = PCm*DOFe3/(DOFe3 + ksatDOFe(j))
               ENDIF
               IF (domcons(4,j) .EQ. 1) THEN
                  muDON  = PCm*DON4/(DON4 + ksatDON(j))
                  muDOC  = PCm*DOC4/(DOC4 + ksatDOC(j))
                  muDOP  = PCm*DOP4/(DOP4 + ksatDOP(j))
                  muDOFe = PCm*DOFe4/(DOFe4 + ksatDOFe(j))
               ENDIF
               IF (domcons(5,j) .EQ. 1) THEN
                  muDON  = PCm*DON5/(DON5 + ksatDON(j))
                  muDOC  = PCm*DOC5/(DOC5 + ksatDOC(j))
                  muDOP  = PCm*DOP5/(DOP5 + ksatDOP(j))
                  muDOFe = PCm*DOFe5/(DOFe5 + ksatDOFe(j))
               ENDIF
Cejz: inorganic uptake potential:
                  muNH4 = pmaxDIN*NH4/(NH4 + ksatDIN)*reminTempFunc
                  muPO4 = pmaxDIP*PO4/(PO4 + ksatDIP)*reminTempFunc
                  muFeT = pmaxFeT*FeT/(FeT + ksatFe)*reminTempFunc
                  mu = MIN(PCm, yield(j)*muDOC, muDON + muNH4, 
     &                     muDOP + muPO4, muDOFe + muFeT, muO)
                  growth = mu*X(j)
Cejz: org-limited growth determines actual inorganic uptake:
                  mu_Norg = MIN(PCm, yield(j)*muDOC, muDON, muO)
                  mu_Porg = MIN(PCm, yield(j)*muDOC, muDOP, muO)
                  mu_Feorg = MIN(PCm, yield(j)*muDOC, muDOFe, muO)

                  uptakeNH4 = MIN(muNH4, MAX((mu - mu_Norg),0.))*
     &                        X(j)*R_NC(j)
                  uptakePO4 = MIN(muPO4, MAX((mu - mu_Porg),0.))*
     &                        X(j)*R_PC(j)
                  uptakeFeT = MIN(muFeT, MAX((mu - mu_Feorg),0.))*
     &                        X(j)*R_FeC(j)
                  
                  diags(iBupNH4) = diags(iBupNH4) + uptakeNH4
                  diags(iBupPO4) = diags(iBupNH4) + uptakePO4
Cejz: org-limited growth needed for uptake calc
                  growth_Norg = mu_Norg*X(j)*R_NC(j)
                  growth_Porg = mu_Porg*X(j)*R_PC(j)
                  growth_Feorg = mu_Feorg*X(j)*R_FeC(j)
Cejz: calculate the ratios of uptake
                  Rup_NC  = MIN(R_NC(j), muDON/(muDOC + EPS))
                  Rup_PC  = MIN(R_PC(j), muDOP/(muDOC + EPS))
                  Rup_FeC  = MIN(R_FeC(j), muDOFe/(muDOC + EPS))
Cejz: uptake and respiration
                
                  uptakeO2 = isAerobic(j)*growth/yieldO2(j)
                  uptakeNO3 = isDenit(j)*growth/yieldNO3(j)
               
                IF (domcons(1,j) .EQ. 1) THEN
                  uptakeDOC = growth/yield(j)
                  uptakeDON = uptakeDOC*Rup_NC
                  uptakeDOP = uptakeDOC*Rup_PC
                  uptakeDOFe = uptakeDOC*Rup_FeC
                  respDOC = growth*(1./yield(j)-1.)
                  respDON = uptakeDON - growth_Norg
                  respDOP = uptakeDOP - growth_Porg
                  respDOFe = uptakeDOFe - growth_Feorg
               ENDIF
               IF (domcons(2,j) .EQ. 1) THEN
                  uptakeDOC2 = growth/yield(j)
                  uptakeDON2 = uptakeDOC2*Rup_NC
                  uptakeDOP2 = uptakeDOC2*Rup_PC
                  uptakeDOFe2 = uptakeDOC2*Rup_FeC
                  respDOC2 = growth*(1./yield(j)-1.)
                  respDON2 = uptakeDON2 - growth_Norg
                  respDOP2 = uptakeDOP2 - growth_Porg
                  respDOFe2 = uptakeDOFe2 - growth_Feorg
               ENDIF
               IF (domcons(3,j) .EQ. 1) THEN
                  uptakeDOC3 = growth/yield(j)
                  uptakeDON3 = uptakeDOC3*Rup_NC
                  uptakeDOP3 = uptakeDOC3*Rup_PC
                  uptakeDOFe3 = uptakeDOC3*Rup_FeC
                  respDOC3 = growth*(1./yield(j)-1.)
                  respDON3 = uptakeDON3 - growth_Norg
                  respDOP3 = uptakeDOP3 - growth_Porg
                  respDOFe3 = uptakeDOFe3 - growth_Feorg
               ENDIF
               IF (domcons(4,j) .EQ. 1) THEN
                  uptakeDOC4 = growth/yield(j)
                  uptakeDON4 = uptakeDOC4*Rup_NC
                  uptakeDOP4 = uptakeDOC4*Rup_PC
                  uptakeDOFe4 = uptakeDOC4*Rup_FeC
                  respDOC4 = growth*(1./yield(j)-1.)
                  respDON4 = uptakeDON4 - growth_Norg
                  respDOP4 = uptakeDOP4 - growth_Porg
                  respDOFe4 = uptakeDOFe4 - growth_Feorg
               ENDIF
               IF (domcons(5,j) .EQ. 1) THEN
                  uptakeDOC5 = growth/yield(j)
                  uptakeDON5 = uptakeDOC5*Rup_NC
                  uptakeDOP5 = uptakeDOC5*Rup_PC
                  uptakeDOFe5 = uptakeDOC5*Rup_FeC
                  respDOC5 = growth*(1./yield(j)-1.)
                  respDON5 = uptakeDON5 - growth_Norg
                  respDOP5 = uptakeDOP5 - growth_Porg
                  respDOFe5 = uptakeDOFe5 - growth_Feorg
               ENDIF

Cejz start add bactType 3 - 5 

Cz: don't incorporate dom2 into type 3 for now (1/12/21) 
Cejz  redo bactType 3 (both POM and DOM) with flex CNP
Cejz  "Model C" in Zakem and Levine 2019 
        ELSEIF (bactType(j) .EQ. 3) THEN

Cejz these are really uptake rates now, not growth rates
Cejz remember that ksatPOC = ksatPON*R_CN(j) etc: reset
Cejz Topt            
Cejz            PCm = PCmax(j)*reminTempFunc
          PCm = PCmax(j)*photoTempFunc(j)
          muPOC  = PCm*POC/(POC + ksatPOC(j))
          muPON  = PCm*PON/(PON + ksatPON(j))
          muPOP  = PCm*POP/(POP + ksatPOP(j))
          muPOFe = PCm*POFe/(POFe + ksatPOFe(j))
          muDOC  = PCm*DOC/(DOC + ksatDOC(j))
          muDON  = PCm*DON/(DON + ksatDON(j))
          muDOP  = PCm*DOP/(DOP + ksatDOP(j))
          muDOFe = PCm*DOFe/(DOFe + ksatDOFe(j))
Cejz: inorganic uptake potential: 
          muNH4  = pmaxDIN*NH4/(NH4+ksatDIN)*reminTempFunc
          muPO4  = pmaxDIP*PO4/(PO4+ksatDIP)*reminTempFunc
          muFeT  = pmaxFeT*FeT/(FeT+ksatFe)*reminTempFunc

          mu = MIN(PCm, yield(j)*(muPOC + muDOC), muPON + muDON + muNH4,
     &             muPOP + muDOP + muPO4, muPOFe + muDOFe + muFeT, muO)

          growth = mu*X(j)

Cejz: org-limited growth determines actual inorganic uptake:
          mu_Norg = MIN(PCm, yield(j)*(muPOC + muDOC), 
     &             muPON + muDON, muO)
          mu_Porg = MIN(PCm, yield(j)*(muPOC + muDOC), 
Cejz: BUG, RIGHT?     &             muPOP + muDOP, muPOFe + muDOFe, muO)
     &             muPOP + muDOP, muO)
          mu_Feorg = MIN(PCm, yield(j)*(muPOC + muDOC), 
     &             muPOFe + muDOFe, muO)

          uptakeNH4 = MIN(muNH4, MAX((mu - mu_Norg), 0. _d 0))*
     &                         X(j)*R_NC(j)
          uptakePO4 = MIN(muPO4, MAX((mu - mu_Porg), 0. _d 0))*
     &                         X(j)*R_PC(j)
          uptakeFeT = MIN(muFeT, MAX((mu - mu_Feorg), 0. _d 0))*
     &                         X(j)*R_FeC(j)

          diags(iBupNH4) = diags(iBupNH4) + uptakeNH4
          diags(iBupPO4) = diags(iBupNH4) + uptakePO4

Cejz: org-limited growth needed for uptake calc
          growth_Norg = mu_Norg*X(j)*R_NC(j)
          growth_Porg = mu_Porg*X(j)*R_PC(j)
          growth_Feorg = mu_Feorg*X(j)*R_FeC(j)


Cejz: calculate the ratios of uptake, which are preserved even if growth
Cis lower due to O2 limitation:
Cejz: NOTE: better to split up POM and DOM here? 
          Rup_NC  = MIN(R_NC(j), (muPON + muDON)/(muPOC + muDOC + EPS)) 
          Rup_PC  = MIN(R_PC(j), (muPOP + muDOP)/(muPOC + muDOC + EPS)) 
          Rup_FeC = MIN(R_FeC(j), (muPOFe + muDOFe)/(muPOC+muDOC+EPS)) 
          
Cejz: uptake and respiration:
Cejz remineralization: POM and DOM respired to inorganic
Cejz note that this needs to be split into POC vs DOC here bc reminPOC and
CreminDOC are split below (i.e. added as sources to DIC in two diff
Cplaces)
          uptakePOC = growth/yield(j)*muPOC/(muPOC + muDOC + EPS)
          uptakeDOC = growth/yield(j)*muDOC/(muPOC + muDOC + EPS)
          respPOC = growth*(1/yield(j)-1)*
     &              muPOC/(muPOC + muDOC + EPS)
          respDOC = growth*(1/yield(j)-1)*
     &              muDOC/(muPOC + muDOC + EPS)

          uptakePON = growth/yield(j)*Rup_NC*muPON/(muPON + muDON +EPS)
          uptakeDON = growth/yield(j)*Rup_NC*muDON/(muPON + muDON +EPS)
Cejz: total growth with N = growth_Norg + uptakeNH4, so here's just the
Corg remin part. now, if uptakeNH4 >0, respPON and respDON = 0 (all used)          
          respPON = uptakePON - growth_Norg*muPON/(muPON + muDON +EPS)
          respDON = uptakeDON - growth_Norg*muDON/(muPON + muDON +EPS)
       
          uptakePOP = growth/yield(j)*Rup_PC*muPOP/(muPOP + muDOP +EPS)
          uptakeDOP = growth/yield(j)*Rup_PC*muDOP/(muPOP + muDOP +EPS)
          respPOP = uptakePOP - growth_Porg*muPOP/(muPOP + muDOP + EPS)
          respDOP = uptakeDOP - growth_Porg*muDOP/(muPOP + muDOP + EPS)
          
          uptakePOFe = growth/yield(j)*Rup_FeC*
     &              muPOFe/(muPOFe + muDOFe + EPS)
          uptakeDOFe = growth/yield(j)*Rup_FeC*
     &              muDOFe/(muPOFe + muDOFe + EPS)
          respPOFe = uptakePOFe - growth_Feorg*
     &              muPOFe/(muPOFe + muDOFe + EPS)
          respDOFe = uptakeDOFe - growth_Feorg*
     &              muDOFe/(muPOFe + muDOFe + EPS)

          uptakeO2 = isAerobic(j)*growth/yieldO2(j)
          uptakeNO3 = isDenit(j)*growth/yieldNO3(j)

C        IF (isAerobic(j) .NE. 0) THEN
C                IF (j .LE. nPPplank) THEN
CCejz                  diags(iPPplank+j-1) = diags(iPPplank+j-1) + growth
CCejz                  diags(iPPplank+j-1) = diags(iPPplank+j-1) + mu
C                  diags(iPPplank+j-1) = mu 
C                  diags(iPPplank+j-1+1) = muPOFe + muDOFe 
C                  diags(iPPplank+j-1+2) = mu_Feorg
C                  diags(iPPplank+j-1+3) = Rup_FeC 
C                  diags(iPPplank+j-1+4) = uptakePOFe 
C                  diags(iPPplank+j-1+5) = respPOFe
C                ENDIF
C        ENDIF


Cez     NH4-oxidizing chemoautotrophs:
        ELSEIF (bactType(j) .EQ. 4) THEN

Cez       Max growth rate is based on max uptake of NH4
          PCm = yield(j)*pmaxDIN*reminTempFunc
          muNH4 = PCm*NH4/(NH4 + ksatDIN)
          mu = MIN(muNH4, muO)

          growth = mu*X(j)

          uptakeNH4 = growth/yield(j)
          uptakeDIC = growth
          uptakePO4 = uptakeDIC*R_PC(j)
          uptakeFeT = uptakeDIC*R_FeC(j)
          uptakeO2 = isAerobic(j)*growth/yieldO2(j)
          resptoNO2 = growth*(1/yield(j) - R_NC(j)) 

Cez     NO2-oxidizing chemoautotrophs:
        ELSEIF (bactType(j) .EQ. 5) THEN

Cez       Max growth rate is based on max uptake of NO2
          PCm = yield(j)*pmaxDIN*NOfac*reminTempFunc
          muNO2 = PCm*NO2/(NO2 + ksatDIN*NOfac)
          mu = MIN(muNO2, muO)

          growth = mu*X(j)

          uptakeNO2 = growth/yield(j)
          uptakeDIC = growth
          uptakePO4 = uptakeDIC*R_PC(j)
          uptakeFeT = uptakeDIC*R_FeC(j)
          uptakeO2 = isAerobic(j)*growth/yieldO2(j)
Cez simplification as in Zakem 2018: really biomass N is from NH4 or DON
Cez but makes no quantitative diff since 1/yield = 333
          resptoNO3 = growth*(1/yield(j) - R_NC(j))  
       
Cejz end add bactType 3 - 5
        ENDIF

        IF (j .LE. nPPplank) THEN
Cejz          diags(iPPplank+j-1) = diags(iPPplank+j-1) + growth
Cejz          diags(iPPplank+j-1) = diags(iPPplank+j-1) + mu
              diags(iPPplank+j-1) = mu
        ENDIF

        gTr(ic+j-1)=gTr(ic+j-1) + growth

C==== Cumulative consum, remin, and prod ===============================
        consumNO3  = consumNO3  + uptakeNO3

C       add B consum and accumulating remin, and prod:
        consumO2 = consumO2 + uptakeO2

        consumDOC = consumDOC + uptakeDOC
        consumDON = consumDON + uptakeDON
        consumDOP = consumDOP + uptakeDOP
        consumDOFe = consumDOFe + uptakeDOFe
Cz add
        consumDOC2 = consumDOC2 + uptakeDOC2
        consumDON2 = consumDON2 + uptakeDON2
        consumDOP2 = consumDOP2 + uptakeDOP2
        consumDOFe2 = consumDOFe2 + uptakeDOFe2

        consumDOC3 = consumDOC3 + uptakeDOC3
        consumDON3 = consumDON3 + uptakeDON3
        consumDOP3 = consumDOP3 + uptakeDOP3
        consumDOFe3 = consumDOFe3 + uptakeDOFe3

        consumDOC4 = consumDOC4 + uptakeDOC4
        consumDON4 = consumDON4 + uptakeDON4
        consumDOP4 = consumDOP4 + uptakeDOP4
        consumDOFe4 = consumDOFe4 + uptakeDOFe4

        consumDOC5 = consumDOC5 + uptakeDOC5
        consumDON5 = consumDON5 + uptakeDON5
        consumDOP5 = consumDOP5 + uptakeDOP5
        consumDOFe5 = consumDOFe5 + uptakeDOFe5

        consumPOC = consumPOC + uptakePOC
        consumPON = consumPON + uptakePON
        consumPOP = consumPOP + uptakePOP
        consumPOFe = consumPOFe + uptakePOFe

        consumPOC2 = consumPOC2 + uptakePOC2
        consumPON2 = consumPON2 + uptakePON2
        consumPOP2 = consumPOP2 + uptakePOP2
        consumPOFe2 = consumPOFe2 + uptakePOFe2

        consumPOC3 = consumPOC3 + uptakePOC3
        consumPON3 = consumPON3 + uptakePON3
        consumPOP3 = consumPOP3 + uptakePOP3
        consumPOFe3 = consumPOFe3 + uptakePOFe3

        reminPOC = reminPOC + respPOC
        reminPON = reminPON + respPON
        reminPOP = reminPOP + respPOP
        reminPOFe = reminPOFe + respPOFe

        solubilPOC = solubilPOC + hydrolPOC
        solubilPON = solubilPON + hydrolPON
        solubilPOP = solubilPOP + hydrolPOP
        solubilPOFe = solubilPOFe + hydrolPOFe

        reminPOC2 = reminPOC2 + respPOC2
        reminPON2 = reminPON2 + respPON2
        reminPOP2 = reminPOP2 + respPOP2
        reminPOFe2 = reminPOFe2 + respPOFe2

        solubilPOC2 = solubilPOC2 + hydrolPOC2
        solubilPON2 = solubilPON2 + hydrolPON2
        solubilPOP2 = solubilPOP2 + hydrolPOP2
        solubilPOFe2 = solubilPOFe2 + hydrolPOFe2

        reminPOC3 = reminPOC3 + respPOC3
        reminPON3 = reminPON3 + respPON3
        reminPOP3 = reminPOP3 + respPOP3
        reminPOFe3 = reminPOFe3 + respPOFe3

        solubilPOC3 = solubilPOC3 + hydrolPOC3
        solubilPON3 = solubilPON3 + hydrolPON3
        solubilPOP3 = solubilPOP3 + hydrolPOP3
        solubilPOFe3 = solubilPOFe3 + hydrolPOFe3

        reminDOC = reminDOC + respDOC 
        reminDON = reminDON + respDON
        reminDOP = reminDOP + respDOP
        reminDOFe = reminDOFe + respDOFe

        reminDOC2 = reminDOC2 + respDOC2
        reminDON2 = reminDON2 + respDON2
        reminDOP2 = reminDOP2 + respDOP2
        reminDOFe2 = reminDOFe2 + respDOFe2

        reminDOC3 = reminDOC3 + respDOC3
        reminDON3 = reminDON3 + respDON3
        reminDOP3 = reminDOP3 + respDOP3
        reminDOFe3 = reminDOFe3 + respDOFe3

        reminDOC4 = reminDOC4 + respDOC4
        reminDON4 = reminDON4 + respDON4
        reminDOP4 = reminDOP4 + respDOP4
        reminDOFe4 = reminDOFe4 + respDOFe4

        reminDOC5 = reminDOC5 + respDOC5
        reminDON5 = reminDON5 + respDON5
        reminDOP5 = reminDOP5 + respDOP5
        reminDOFe5 = reminDOFe5 + respDOFe5

Cejz add nitrifier consumption and remineralization: 
        consumDIC  = consumDIC  + uptakeDIC
        consumNH4  = consumNH4  + uptakeNH4
        consumNO2  = consumNO2  + uptakeNO2
        consumPO4  = consumPO4  + uptakePO4
        consumFeT  = consumFeT  + uptakeFeT
        prodNO2 = prodNO2 + resptoNO2
        prodNO3 = prodNO3 + resptoNO3

C     end bacttype conditional
       ENDIF
C     j loop end
      ENDDO

C=======================================================================
C=======================================================================

      gTr(iDIC )=gTr(iDIC ) - consumDIC - consumDIC_PIC
Cejz add nitrifier prodNO2 and prodNO3 here, so that it can be zeroed
Cout below
Cejz  gTr(iNH4 )=gTr(iNH4 ) - consumNH4
      gTr(iNH4 )=gTr(iNH4 ) - consumNH4
      gTr(iNO2 )=gTr(iNO2 ) - consumNO2 + prodNO2
      gTr(iNO3 )=gTr(iNO3 ) - consumNO3 + prodNO3
      gTr(iPO4 )=gTr(iPO4 ) - consumPO4
      gTr(iSiO2)=gTr(iSiO2) - consumSiO2
      gTr(iFeT )=gTr(iFeT ) - consumFeT

Cz: NO dom2 into implicit remin -- definitely set all K to zero
C parameterized remineralization; want to set all K except KPOSi to zero
C if running with bacteria
      respDOC  = reminTempFunc*KDOC *DOC
      respDON  = reminTempFunc*KDON *DON
      respDOP  = reminTempFunc*KDOP *DOP
      respDOFe = reminTempFunc*KDOFe*DOFe
      respPOC  = reminTempFunc*KPOC *POC
      respPON  = reminTempFunc*KPON *PON
      respPOP  = reminTempFunc*KPOP *POP
      respPOSi = reminTempFunc*KPOSi*POSi
      respPOFe = reminTempFunc*KPOFe*POFe

      consumDOC  = consumDOC  + respDOC
      consumDON  = consumDON  + respDON
      consumDOP  = consumDOP  + respDOP
      consumDOFe = consumDOFe + respDOFe
      consumPOC  = consumPOC  + respPOC
      consumPON  = consumPON  + respPON
      consumPOP  = consumPOP  + respPOP
      consumPOSi = consumPOSi + respPOSi
      consumPOFe = consumPOFe + respPOFe

      reminDOC  = reminDOC  + respDOC
      reminDON  = reminDON  + respDON
      reminDOP  = reminDOP  + respDOP
      reminDOFe = reminDOFe + respDOFe
      reminPOC  = reminPOC  + respPOC
      reminPON  = reminPON  + respPON
      reminPOP  = reminPOP  + respPOP
      reminPOSi = reminPOSi + respPOSi
      reminPOFe = reminPOFe + respPOFe

#ifdef DARWIN_ALLOW_CARBON
      consumO2  = consumO2  + respDOP*R_OP
#ifndef DARWIN_ALLOW_CDOM
      consumO2  = consumO2  + respPOP*R_OP
#endif
#endif

      disscPIC = Kdissc*PIC

c nitrogen chemistry
c NH4 -> NO2 -> NO3 by bacterial action, parameterized
Cejz: since prodNO2 and prodNO3 by nitrifiers are already accounted,
Cthis works like the implicit remin terms: will add to nitrification
CUNLESS knita and knitb are set to 0
Cejz: to clarify, prodNO2 and prodNO3 are RESET here (they are filled
Cabove with explicit consumption and added to tracers above)
      prodNO2 = knita*NH4
      prodNO3 = knitb*NO2
      IF (PAR_oxi .NE. 0.0 _d 0) THEN
        prodNO2 = prodNO2*MAX(0.0, 1.0 - PARtot/PAR_oxi)
        prodNO3 = prodNO3*MAX(0.0, 1.0 - PARtot/PAR_oxi)
      ENDIF

#ifdef DARWIN_ALLOW_CDOM
      reminPOP_CDOM = fracCDOM*reminPOP
      reminPOC_CDOM = R_CP_CDOM*reminPOP_CDOM
      reminPON_CDOM = R_NP_CDOM*reminPOP_CDOM
      reminPOFe_CDOM = R_FeP_CDOM*reminPOP_CDOM
c degradation of  CDOM - high when bleached by light
      degrCDOM_DOP = reminTempFunc*CDOM*
     &     (CDOMdegrd+CDOMbleach*MIN(1.0 _d 0, PARtot/PARCDOM))
      degrCDOM_DOC  = R_CP_CDOM  * degrCDOM_DOP
      degrCDOM_DON  = R_NP_CDOM  * degrCDOM_DOP
      degrCDOM_DOFe = R_FeP_CDOM * degrCDOM_DOP
#endif

#ifdef DARWIN_ALLOW_DENIT
      IF (O2 .LT. O2crit .AND. NO3 .LT. NO3crit) THEN
        consumDOC = 0. _d 0
        consumDOP  = 0. _d 0
        consumDON  = 0. _d 0
        consumDOFe = 0. _d 0
Cz        
        consumDOC2 = 0. _d 0
        consumDOP2  = 0. _d 0
        consumDON2  = 0. _d 0
        consumDOFe2 = 0. _d 0
        consumDOC3 = 0. _d 0
        consumDOP3  = 0. _d 0
        consumDON3  = 0. _d 0
        consumDOFe3 = 0. _d 0
        consumDOC4 = 0. _d 0
        consumDOP4  = 0. _d 0
        consumDON4  = 0. _d 0
        consumDOFe4 = 0. _d 0
        consumDOC5 = 0. _d 0
        consumDOP5  = 0. _d 0
        consumDON5  = 0. _d 0
        consumDOFe5 = 0. _d 0
        consumPOC = 0. _d 0
        consumPOP  = 0. _d 0
        consumPON  = 0. _d 0
        consumPOFe = 0. _d 0
        consumPOC2 = 0. _d 0
        consumPOP2  = 0. _d 0
        consumPON2  = 0. _d 0
        consumPOFe2 = 0. _d 0
        consumPOC3 = 0. _d 0
        consumPOP3  = 0. _d 0
        consumPON3  = 0. _d 0
        consumPOFe3 = 0. _d 0
        reminDOC = 0. _d 0
        reminDOP  = 0. _d 0
        reminDON  = 0. _d 0
        reminDOFe = 0. _d 0
        reminDOC2 = 0. _d 0
        reminDOP2  = 0. _d 0
        reminDON2  = 0. _d 0
        reminDOFe2 = 0. _d 0
        reminDOC3 = 0. _d 0
        reminDOP3  = 0. _d 0
        reminDON3  = 0. _d 0
        reminDOFe3 = 0. _d 0
        reminDOC4 = 0. _d 0
        reminDOP4  = 0. _d 0
        reminDON4  = 0. _d 0
        reminDOFe4 = 0. _d 0
        reminDOC5 = 0. _d 0
        reminDOP5  = 0. _d 0
        reminDON5  = 0. _d 0
        reminDOFe5 = 0. _d 0
        reminPOC = 0. _d 0
        reminPOP  = 0. _d 0
        reminPON  = 0. _d 0
        reminPOFe = 0. _d 0
        reminPOC2 = 0. _d 0
        reminPOP2  = 0. _d 0
        reminPON2  = 0. _d 0
        reminPOFe2 = 0. _d 0
        reminPOC3 = 0. _d 0
        reminPOP3  = 0. _d 0
        reminPON3  = 0. _d 0
        reminPOFe3 = 0. _d 0
#ifdef DARWIN_ALLOW_CDOM
        reminPOC_cdom = 0. _d 0
        reminPOP_cdom  = 0. _d 0
        reminPON_cdom  = 0. _d 0
        reminPOFe_cdom = 0. _d 0
        degrCDOM_DOP = reminTempFunc*CDOM*
     &                 CDOMbleach*MIN(1.0 _d 0, PARtot/PARCDOM)
        degrCDOM_DON  = R_NP_CDOM  * degrCDOM_DOP
        degrCDOM_DOFe = R_FeP_CDOM * degrCDOM_DOP
        degrCDOM_DOC  = R_CP_CDOM  * degrCDOM_DOP
#endif
      ENDIF
#endif /* DARWIN_ALLOW_DENIT */

C==== apply tendencies =================================================

CEZ STOP HERE 2/15/22 (but also I didn't do the bacterial consumption
Cabove yet)

#ifdef DARWIN_ALLOW_CARBON
c production of O2 by photosynthesis
      gTr(iO2  )=gTr(iO2  ) + R_OP*consumPO4
c loss of O2 by remineralization
      IF (O2 .GT. O2crit) THEN
        gTr(iO2)=gTr(iO2) - consumO2
      ENDIF

      gTr(iALK)=gTr(iALK) - (prodNO3 - consumNO3)
     &                    - 2.0 _d 0*(consumDIC_PIC - disscPIC)
#endif /* DARWIN_ALLOW_CARBON */

      gTr(iDIC )=gTr(iDIC ) + reminDOC + reminDOC2 + reminDOC3 +
     &           reminDOC4 + reminDOC5 +disscPIC
Cejz with nitrifiers, consumption of NO2 and NH4 is accounted for as uptake
Cejz: explicit prodNO2 and prodNO3 are added to tracers above, THEN refilled with
Cimplicit with knita and knitb
Cejz if knita and knitb are set to 0, then this will not add anything
      gTr(iNH4 )=gTr(iNH4 ) + reminDON + reminDON2 + reminDON3 + 
     &           reminDON4 + reminDON5 - prodNO2
      gTr(iNO2 )=gTr(iNO2 ) + prodNO2 - prodNO3
      gTr(iNO3 )=gTr(iNO3 ) + prodNO3
      diags(iDenitN) = 0.0
#ifdef DARWIN_ALLOW_DENIT
Cejz NOT changing this yet (i see i changed ONLY reminDOP -- missed
CreminDON and am now not doing reminPOM)
      IF (O2 .LT. O2crit) THEN
        denitNH4 = reminDON
        denit = denit_NP*(reminDOP + reminDOP2 + reminDOP3 + 
     &                    reminDOP4 + reminDOP5)
#ifndef DARWIN_ALLOW_CDOM
        denitNH4 = denitNH4 + reminPON 
        denit = denit + denit_NP*reminPOP
#endif
        diags(iDenit) = denit
        gTr(iNH4)=gTr(iNH4) - denitNH4
        gTr(iNO3)=gTr(iNO3) - denit_NO3/denit_np*denit
        gTr(iALK)=gTr(iALK) + denit_NO3/denit_np*denit
        diags(iDenitN) = denitNH4 + denit_NO3/denit_np*denit
      ENDIF
#endif /* DARWIN_ALLOW_DENIT */

      gTr(iPO4 )=gTr(iPO4 ) + reminDOP + reminDOP2 + reminDOP3 +
     &           reminDOP4 + reminDOP5
      gTr(iFeT )=gTr(iFeT ) + reminDOFe + reminDOFe2 + reminDOFe3 +
     &           reminDOFe4 + reminDOFe5
      gTr(iSiO2)=gTr(iSiO2)             + reminPOSi

C     DOC is created by #4 PA-assoc solubilization and consumed by #5
Cejz NOT including solubilPOM at ALL yet! (NOT just for POM1, even,
Cyet.) 
C a bit arbitrary, but putting POC into 3 of 5 DOC classes, and so using
C wpom for this (even though it really is more like wdom)
      gTr(iDOC )=gTr(iDOC ) + solubilPOC*wpomtodom1 - consumDOC
      gTr(iDON )=gTr(iDON ) + solubilPON*wpomtodom1 - consumDON
      gTr(iDOP )=gTr(iDOP ) + solubilPOP*wpomtodom1 - consumDOP
      gTr(iDOFe)=gTr(iDOFe) + solubilPOFe*wpomtodom1 - consumDOFe

Cz add dom2 BUT need to add pom2, etc. 
Cspread over 3 DOC classes.
      gTr(iDOC2 )=gTr(iDOC2 ) + solubilPOC*wpomtodom2  + 
     &            solubilPOC2*wpomtodom1 - 
     &            consumDOC2
      gTr(iDON2 )=gTr(iDON2 ) + solubilPON*wpomtodom2  + 
     &            solubilPON2*wpomtodom1 - 
     &            consumDON2
      gTr(iDOP2 )=gTr(iDOP2 ) + solubilPOP*wpomtodom2  + 
     &            solubilPOP2*wpomtodom1 - 
     &            consumDOP2
      gTr(iDOFe2)=gTr(iDOFe2) + solubilPOFe*wpomtodom2 + 
     &            solubilPOFe2*wpomtodom1- 
     &            consumDOFe2

      gTr(iDOC3 )=gTr(iDOC3 ) + solubilPOC*wpomtodom3  + 
     &            solubilPOC2*wpomtodom2  +
     &            solubilPOC3*wpomtodom1 - consumDOC3
      gTr(iDON3 )=gTr(iDON3 ) + solubilPON*wpomtodom3  + 
     &            solubilPON2*wpomtodom2  +
     &            solubilPON3*wpomtodom1 - consumDON3
      gTr(iDOP3 )=gTr(iDOP3 ) + solubilPOP*wpomtodom3  + 
     &            solubilPOP2*wpomtodom2  +
     &            solubilPOP3*wpomtodom1 - consumDOP3
      gTr(iDOFe3)=gTr(iDOFe3) + solubilPOFe*wpomtodom3 + 
     &            solubilPOFe2*wpomtodom2 +
     &            solubilPOFe3*wpomtodom1 - consumDOFe3

      gTr(iDOC4 )=gTr(iDOC4 ) + solubilPOC2*wpomtodom3  + 
     &            solubilPOC3*wpomtodom2 -
     &            consumDOC4
      gTr(iDON4 )=gTr(iDON4 ) + solubilPON2*wpomtodom3  + 
     &            solubilPON3*wpomtodom2 -
     &            consumDON4
      gTr(iDOP4 )=gTr(iDOP4 ) + solubilPOP2*wpomtodom3  + 
     &            solubilPOP3*wpomtodom2 -
     &            consumDOP4
      gTr(iDOFe4)=gTr(iDOFe4) + solubilPOFe2*wpomtodom3 + 
     &            solubilPOFe3*wpomtodom2 -
     &            consumDOFe4

      gTr(iDOC5 )=gTr(iDOC5 ) + solubilPOC3*wpomtodom3 - consumDOC5
      gTr(iDON5 )=gTr(iDON5 ) + solubilPON3*wpomtodom3 - consumDON5
      gTr(iDOP5 )=gTr(iDOP5 ) + solubilPOP3*wpomtodom3 - consumDOP5
      gTr(iDOFe5)=gTr(iDOFe5) + solubilPOFe3*wpomtodom3 - consumDOFe5

      gTr(iPIC )=gTr(iPIC ) - disscPIC
      gTr(iPOSi)=gTr(iPOSi) - consumPOSi
      gTr(iPOC )=gTr(iPOC ) - consumPOC
      gTr(iPON )=gTr(iPON ) - consumPON
      gTr(iPOP )=gTr(iPOP ) - consumPOP
      gTr(iPOFe)=gTr(iPOFe) - consumPOFe
      gTr(iPOC2 )=gTr(iPOC2 ) - consumPOC2
      gTr(iPON2 )=gTr(iPON2 ) - consumPON2
      gTr(iPOP2 )=gTr(iPOP2 ) - consumPOP2
      gTr(iPOFe2)=gTr(iPOFe2) - consumPOFe2
      gTr(iPOC3 )=gTr(iPOC3 ) - consumPOC3
      gTr(iPON3 )=gTr(iPON3 ) - consumPON3
      gTr(iPOP3 )=gTr(iPOP3 ) - consumPOP3
      gTr(iPOFe3)=gTr(iPOFe3) - consumPOFe3

#ifdef DARWIN_ALLOW_CDOM
      gTr(iDOC )=gTr(iDOC ) + reminPOC  - reminPOC_CDOM  + degrCDOM_DOC
      gTr(iDON )=gTr(iDON ) + reminPON  - reminPON_CDOM  + degrCDOM_DON
      gTr(iDOP )=gTr(iDOP ) + reminPOP  - reminPOP_CDOM  + degrCDOM_DOP
      gTr(iDOFe)=gTr(iDOFe) + reminPOFe - reminPOFe_CDOM + degrCDOM_DOFe

      gTr(iCDOM)=gTr(iCDOM) + reminPOP_CDOM - degrCDOM_DOP
#else
Cejz: add pom2 and 3      
Cejz      gTr(iDIC )=gTr(iDIC ) + reminPOC
Cejz      gTr(iNH4 )=gTr(iNH4 ) + reminPON
Cejz      gTr(iPO4 )=gTr(iPO4 ) + reminPOP
Cejz      gTr(iFeT )=gTr(iFeT ) + reminPOFe
      gTr(iDIC )=gTr(iDIC ) + reminPOC + reminPOC2 + reminPOC3
      gTr(iNH4 )=gTr(iNH4 ) + reminPON + reminPON2 + reminPON3
      gTr(iPO4 )=gTr(iPO4 ) + reminPOP + reminPOP2 + reminPOP3
      gTr(iFeT )=gTr(iFeT ) + reminPOFe + reminPOFe2 + reminPOFe3
#endif /* DARWIN_ALLOW_CDOM */

      diags(iConsDIN) = consumNH4 + consumNO2 + consumNO3
      diags(iConsPO4) = consumPO4
      diags(iConsSi)  = consumSiO2
      diags(iConsFe)  = consumFeT

C==== grazing ==========================================================

      DO j=1,nplank
       preygraz(j)   = 0.0
       preygrazexp(j) = 0.0
       predgrazc(j)  = 0.0
#ifdef DARWIN_ALLOW_NQUOTA
       predgrazn(j)  = 0.0
#endif
#ifdef DARWIN_ALLOW_PQUOTA
       predgrazp(j)  = 0.0
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
       predgrazfe(j) = 0.0
#endif
      ENDDO
      graz2POC  = 0.0
      graz2PON  = 0.0
      graz2POP  = 0.0
      graz2POSI = 0.0
      graz2POFE = 0.0
      graz2OC   = 0.0
      graz2ON   = 0.0
      graz2OP   = 0.0
      graz2OFE  = 0.0
      graz2PIC  = 0.0

      regQn  = 1.0
      regQp  = 1.0
      regQfe = 1.0
      regQc  = 1.0

C=======================================================================
      DO jz = 1, nplank
       IF (isPred(jz).NE.0) THEN

C       regulate grazing near full quota
        regQc = 1.0 _d 0
#ifdef DARWIN_ALLOW_NQUOTA
        regQn = MAX(0., MIN(1., (Qnmax(jz)-Qn(jz))/
     &                          (Qnmax(jz)-Qnmin(jz)) ))
        regQc = MIN(regQc, 1.0 _d 0 - regQn)
        regQn = regQn**hillnumGraz
#endif
#ifdef DARWIN_ALLOW_PQUOTA
        regQp = MAX(0., MIN(1., (Qpmax(jz)-Qp(jz))/
     &                          (Qpmax(jz)-Qpmin(jz)) ))
        regQc = MIN(regQc, 1.0 _d 0 - regQp)
        regQp = regQp**hillnumGraz
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
        regQfe= MAX(0., MIN(1., (Qfemax(jz)-Qfe(jz))/
     &                          (Qfemax(jz)-Qfemin(jz)) ))
        regQc = MIN(regQc, 1.0 _d 0 - regQfe)
        regQfe=regQfe**hillnumGraz
#endif
        regQc = regQc**hillnumGraz

        sumprey = 0.0
        sumpref = 0.0
        DO jp = 1, nplank
        IF (palat(jp,jz).NE.0 _d 0) THEN
         sumprey = sumprey + palat(jp,jz)*X(jp)
#ifdef DARWIN_GRAZING_SWITCH
         sumpref = sumpref + palat(jp,jz)*palat(jp,jz)*X(jp)*X(jp)
#else
         sumpref = sumpref + palat(jp,jz)*X(jp)
#endif
        ENDIF
        ENDDO
        sumprey = MAX(0.0, sumprey - phygrazmin)
        sumpref = MAX(phygrazmin, sumpref)
        tmp = grazemax(jz)*grazTempFunc(jz)**tempGraz(jz)*X(jz)*
     &    (sumprey**hollexp/(sumprey**hollexp+kgrazesat(jz)**hollexp))*
     &    (1.0 - EXP(-inhib_graz*sumprey))**inhib_graz_exp

        predexpc  = 0.0 _d 0
        predexpn  = 0.0 _d 0
        predexpp  = 0.0 _d 0
        predexpfe = 0.0 _d 0
        DO jp = 1, nplank
         IF (palat(jp,jz).NE.0 _d 0) THEN
#ifdef DARWIN_GRAZING_SWITCH
          grazphy = tmp*palat(jp,jz)*palat(jp,jz)*X(jp)*X(jp)/sumpref
#else
          grazphy = tmp*palat(jp,jz)*X(jp)/sumpref
#endif

          expFrac = ExportFracPreyPred(jp,jz)

          preygraz(jp) = preygraz(jp) + grazphy
          preygrazexp(jp) = preygrazexp(jp) + expFrac*grazphy

          predgrazc(jz) = predgrazc(jz) + grazphy*asseff(jp,jz)*regQc
          predexpc = predexpc + expFrac*grazphy*asseff(jp,jz)*regQc
#ifdef DARWIN_ALLOW_NQUOTA
          predgrazn(jz) = predgrazn(jz) + grazphy*asseff(jp,jz)*
     &                                    regQn*Qn(jp)
          predexpn = predexpn + expFrac*grazphy*asseff(jp,jz)*
     &                                    regQn*Qn(jp)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
          predgrazp(jz) = predgrazp(jz) + grazphy*asseff(jp,jz)*
     &                                    regQp*Qp(jp)
          predexpp = predexpp + expFrac*grazphy*asseff(jp,jz)*
     &                                  regQp*Qp(jp)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
          predgrazfe(jz) = predgrazfe(jz) + grazphy*asseff(jp,jz)*
     &                                      regQfe*Qfe(jp)
          predexpfe = predexpfe + expFrac*grazphy*asseff(jp,jz)*
     &                                    regQfe*Qfe(jp)
#endif
         ENDIF
        ENDDO

C organic-matter gain will be total preygraz - predgraz
        graz2OC   = graz2OC  - predgrazc(jz)
        graz2POC  = graz2POC - predexpc

#ifdef DARWIN_ALLOW_NQUOTA
        graz2ON   = graz2ON  - predgrazn(jz)
        graz2PON  = graz2PON - predexpn
#else
        graz2ON   = graz2ON  - predgrazc(jz)*Qn(jz)
        graz2PON  = graz2PON - predexpc     *Qn(jz)
#endif

#ifdef DARWIN_ALLOW_PQUOTA
        graz2OP   = graz2OP  - predgrazp(jz)
        graz2POP  = graz2POP - predexpp
#else
        graz2OP   = graz2OP  - predgrazc(jz)*Qp(jz)
        graz2POP  = graz2POP - predexpc     *Qp(jz)
#endif

#ifdef DARWIN_ALLOW_FEQUOTA
        graz2OFe  = graz2OFe  - predgrazfe(jz)
        graz2POFe = graz2POFe - predexpfe
#else
        graz2OFe  = graz2OFe  - predgrazc(jz)*Qfe(jz)
        graz2POFe = graz2POFe - predexpc     *Qfe(jz)
#endif

       ENDIF
C     end predator loop
      ENDDO

      DO jp = 1, nplank
       IF (isPrey(jp).NE.0) THEN
          graz2OC  = graz2OC  + preygraz(jp)
          graz2ON  = graz2ON  + preygraz(jp)*Qn (jp)
          graz2OP  = graz2OP  + preygraz(jp)*Qp (jp)
          graz2POSi = graz2POSi + preygraz(jp)*Qsi(jp)
          graz2OFe = graz2OFe + preygraz(jp)*Qfe(jp)
#ifdef DARWIN_ALLOW_CARBON
          graz2PIC = graz2PIC + preygraz(jp)*R_PICPOC(jp)
#endif

          graz2POC  = graz2POC  + preygrazexp(jp)
          graz2PON  = graz2PON  + preygrazexp(jp)*Qn (jp)
          graz2POP  = graz2POP  + preygrazexp(jp)*Qp (jp)
          graz2POFe = graz2POFe + preygrazexp(jp)*Qfe(jp)
       ENDIF
      ENDDO

C==== tendencies =======================================================

Cz      gTr(iDOC )=gTr(iDOC ) + graz2OC  - graz2POC
Cz      gTr(iDON )=gTr(iDON ) + graz2ON  - graz2PON
Cz      gTr(iDOP )=gTr(iDOP ) + graz2OP  - graz2POP
Cz      gTr(iDOFe)=gTr(iDOFe) + graz2OFe - graz2POFe
Cz add dom2      
      gTr(iDOC )=gTr(iDOC ) + (graz2OC  - graz2POC)*wdom1
      gTr(iDON )=gTr(iDON ) + (graz2ON  - graz2PON)*wdom1
      gTr(iDOP )=gTr(iDOP ) + (graz2OP  - graz2POP)*wdom1
      gTr(iDOFe)=gTr(iDOFe) + (graz2OFe - graz2POFe)*wdom1
      gTr(iDOC2 )=gTr(iDOC2 ) + (graz2OC  - graz2POC)*wdom2
      gTr(iDON2 )=gTr(iDON2 ) + (graz2ON  - graz2PON)*wdom2
      gTr(iDOP2 )=gTr(iDOP2 ) + (graz2OP  - graz2POP)*wdom2
      gTr(iDOFe2)=gTr(iDOFe2) + (graz2OFe - graz2POFe)*wdom2
      gTr(iDOC3 )=gTr(iDOC3 ) + (graz2OC  - graz2POC)*wdom3
      gTr(iDON3 )=gTr(iDON3 ) + (graz2ON  - graz2PON)*wdom3
      gTr(iDOP3 )=gTr(iDOP3 ) + (graz2OP  - graz2POP)*wdom3
      gTr(iDOFe3)=gTr(iDOFe3) + (graz2OFe - graz2POFe)*wdom3
      gTr(iDOC4 )=gTr(iDOC4 ) + (graz2OC  - graz2POC)*wdom4
      gTr(iDON4 )=gTr(iDON4 ) + (graz2ON  - graz2PON)*wdom4
      gTr(iDOP4 )=gTr(iDOP4 ) + (graz2OP  - graz2POP)*wdom4
      gTr(iDOFe4)=gTr(iDOFe4) + (graz2OFe - graz2POFe)*wdom4
      gTr(iDOC5 )=gTr(iDOC5 ) + (graz2OC  - graz2POC)*wdom5
      gTr(iDON5 )=gTr(iDON5 ) + (graz2ON  - graz2PON)*wdom5
      gTr(iDOP5 )=gTr(iDOP5 ) + (graz2OP  - graz2POP)*wdom5
      gTr(iDOFe5)=gTr(iDOFe5) + (graz2OFe - graz2POFe)*wdom5
Cz add pom2 and pom3
Cz      gTr(iPOC )=gTr(iPOC ) + graz2POC
Cz      gTr(iPON )=gTr(iPON ) + graz2PON
Cz      gTr(iPOP )=gTr(iPOP ) + graz2POP
Cz      gTr(iPOFe)=gTr(iPOFe) + graz2POFe
      gTr(iPOC )=gTr(iPOC ) + graz2POC*wpom1
      gTr(iPON )=gTr(iPON ) + graz2PON*wpom1
      gTr(iPOP )=gTr(iPOP ) + graz2POP*wpom1
      gTr(iPOFe)=gTr(iPOFe) + graz2POFe*wpom1
      gTr(iPOC2 )=gTr(iPOC2 ) + graz2POC*wpom2
      gTr(iPON2 )=gTr(iPON2 ) + graz2PON*wpom2
      gTr(iPOP2 )=gTr(iPOP2 ) + graz2POP*wpom2
      gTr(iPOFe2)=gTr(iPOFe2) + graz2POFe*wpom2
      gTr(iPOC3 )=gTr(iPOC3 ) + graz2POC*wpom3
      gTr(iPON3 )=gTr(iPON3 ) + graz2PON*wpom3
      gTr(iPOP3 )=gTr(iPOP3 ) + graz2POP*wpom3
      gTr(iPOFe3)=gTr(iPOFe3) + graz2POFe*wpom3
      gTr(iPOSi)=gTr(iPOSi) + graz2POSi
#ifdef DARWIN_ALLOW_CARBON
      gTr(iPIC )=gTr(iPIC ) + graz2PIC
#endif
#ifdef DARWIN_ALLOW_CDOM
      graz2CDOM = fracCDOM*(graz2OP - graz2POP)
      gTr(iCDOM)=gTr(iCDOM) + graz2CDOM
      gTr(iDOC )=gTr(iDOC )             - R_CP_CDOM*graz2CDOM
      gTr(iDON )=gTr(iDON )             - R_NP_CDOM*graz2CDOM
      gTr(iDOP )=gTr(iDOP ) - graz2CDOM
      gTr(iDOFe)=gTr(iDOFe)             - R_FeP_CDOM*graz2CDOM
#endif

      DO jp = 1, nplank
      IF (isPrey(jp).NE.0) THEN
       gTr(ic+jp-1)= gTr(ic+jp-1) - preygraz(jp)
#ifdef DARWIN_ALLOW_NQUOTA
       gTr(in+jp-1)=gTr(in+jp-1) - preygraz(jp)*Qn(jp)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
       gTr(ip+jp-1)=gTr(ip+jp-1) - preygraz(jp)*Qp(jp)
#endif
#ifdef DARWIN_ALLOW_SIQUOTA
       gTr(isi+jp-1)=gTr(isi+jp-1) - preygraz(jp)*Qsi(jp)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
       gTr(ife+jp-1)=gTr(ife+jp-1) - preygraz(jp)*Qfe(jp)
#endif
      ENDIF
      ENDDO
#ifdef DARWIN_ALLOW_CHLQUOTA
      DO jp = 1, nPhoto
      IF (isPrey(jp).NE.0) THEN
       gTr(iChl+jp-1)=gTr(iChl+jp-1) - preygraz(jp)*QChl(jp)
      ENDIF
      ENDDO
#endif

      DO jz = 1, nplank
      IF (isPred(jz).NE.0) THEN
       gTr(ic+jz-1)=gTr(ic+jz-1) + predgrazc(jz)
#ifdef DARWIN_ALLOW_NQUOTA
       gTr(in+jz-1)=gTr(in+jz-1) + predgrazn(jz)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
       gTr(ip+jz-1)=gTr(ip+jz-1) + predgrazp(jz)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
       gTr(ife+jz-1)=gTr(ife+jz-1) + predgrazfe(jz)
#endif
      ENDIF
      ENDDO

Cejz: move to below and add mort_c to get full loss rate
C      DO jp = 1, nGRplank
C        diags(iGRplank+jp-1) = preygraz(jp)
C      ENDDO

C==== mortality ========================================================
      exude_DOC  = 0.0 _d 0
      exude_POC  = 0.0 _d 0
      exude_DON  = 0.0 _d 0
      exude_PON  = 0.0 _d 0
      exude_DOFe = 0.0 _d 0
      exude_POFe = 0.0 _d 0
      exude_DOP  = 0.0 _d 0
      exude_POP  = 0.0 _d 0
      exude_POSi = 0.0 _d 0
      exude_PIC  = 0.0 _d 0
      respir     = 0.0 _d 0

      DO jp = 1, nplank
        Xe = MAX(0 _d 0, X(jp) - Xmin(jp))
        mortX = mort(jp)*Xe*mortTempFunc**tempMort(jp)
        mortX2= mort2(jp)*Xe*Xe*mort2TempFunc**tempMort2(jp)

        mort_c(jp) = mortX + mortX2

        exude_DOC = exude_DOC + (1.-ExportFracMort(jp)) *mortX
     &                        + (1.-ExportFracMort2(jp))*mortX2
        exude_POC = exude_POC +     ExportFracMort(jp)  *mortX
     &                        +     ExportFracMort2(jp) *mortX2

        exude_DON = exude_DON + (1.-ExportFracMort(jp)) *mortX *Qn(jp)
     &                        + (1.-ExportFracMort2(jp))*mortX2*Qn(jp)
        exude_PON = exude_PON +     ExportFracMort(jp)  *mortX *Qn(jp)
     &                        +     ExportFracMort2(jp) *mortX2*Qn(jp)

        exude_DOP = exude_DOP + (1.-ExportFracMort(jp)) *mortX *Qp(jp)
     &                        + (1.-ExportFracMort2(jp))*mortX2*Qp(jp)
        exude_POP = exude_POP +     ExportFracMort(jp)  *mortX *Qp(jp)
     &                        +     ExportFracMort2(jp) *mortX2*Qp(jp)

        exude_DOFe= exude_DOFe+ (1.-ExportFracMort(jp)) *mortX *Qfe(jp)
     &                        + (1.-ExportFracMort2(jp))*mortX2*Qfe(jp)
        exude_POFe= exude_POFe+     ExportFracMort(jp)  *mortX *Qfe(jp)
     &                        +     ExportFracMort2(jp) *mortX2*Qfe(jp)

        exude_POSi = exude_POSi + mort_c(jp)*Qsi(jp)

        exude_PIC = exude_PIC + mort_c(jp)*R_PICPOC(jp)

        respir_c = respRate(jp)*Xe*reminTempFunc
        respir = respir + respir_c

        gTr(ic+jp-1)=gTr(ic+jp-1)  - mort_c(jp) - respir_c
#ifdef DARWIN_ALLOW_NQUOTA
        gTr(in+jp-1)=gTr(in+jp-1)  - mort_c(jp)*Qn(jp)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
        gTr(ip+jp-1)=gTr(ip+jp-1)  - mort_c(jp)*Qp(jp)
#endif
#ifdef DARWIN_ALLOW_SIQUOTA
        gTr(isi+jp-1)=gTr(isi+jp-1) - mort_c(jp)*Qsi(jp)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
        gTr(ife+jp-1)=gTr(ife+jp-1) - mort_c(jp)*Qfe(jp)
#endif

#ifdef DARWIN_ALLOW_EXUDE
        exude_DOC =
     &  exude_DOC + (1.-ExportFracExude(jp))*kexcc(jp)*Xe
        exude_POC =
     &  exude_POC +     ExportFracExude(jp) *kexcc(jp)*Xe
        exude_DON =
     &  exude_DON + (1.-ExportFracExude(jp))*kexcn(jp)*Xe*Qn(jp)
        exude_PON =
     &  exude_PON +     ExportFracExude(jp) *kexcn(jp)*Xe*Qn(jp)
        exude_DOP =
     &  exude_DOP + (1.-ExportFracExude(jp))*kexcp(jp)*Xe*Qp(jp)
        exude_POP =
     &  exude_POP +     ExportFracExude(jp) *kexcp(jp)*Xe*Qp(jp)
        exude_DOFe =
     &  exude_DOFe + (1.-ExportFracExude(jp))*kexcfe(jp)*Xe*Qfe(jp)
        exude_POFe =
     &  exude_POFe +     ExportFracExude(jp) *kexcfe(jp)*Xe*Qfe(jp)
        exude_POSi =
     &  exude_POSi + kexcsi(jp)*Xe*Qsi(jp)
        gTr(ic+jp-1)=gTr(ic+jp-1)   - kexcc(jp)*Xe
#ifdef DARWIN_ALLOW_NQUOTA
        gTr(in+jp-1)=gTr(in+jp-1)   - kexcn(jp)*Xe*Qn(jp)
#endif
#ifdef DARWIN_ALLOW_PQUOTA
        gTr(ip+jp-1)=gTr(ip+jp-1)   - kexcp(jp)*Xe*Qp(jp)
#endif
#ifdef DARWIN_ALLOW_SIQUOTA
        gTr(isi+jp-1)=gTr(isi+jp-1) - kexcsi(jp)*Xe*Qsi(jp)
#endif
#ifdef DARWIN_ALLOW_FEQUOTA
        gTr(ife+jp-1)=gTr(ife+jp-1) - kexcfe(jp)*Xe*Qfe(jp)
#endif
#endif
      ENDDO

#ifdef DARWIN_ALLOW_CHLQUOTA
      DO jp = 1, nPhoto
        gTr(iChl+jp-1)=gTr(iChl+jp-1) - mort_c(jp)*QChl(jp)
      ENDDO
#endif

cez: both grazing and mortality:
      DO jp = 1, nGRplank
        diags(iGRplank+jp-1) = preygraz(jp) + mort_c(jp)
      ENDDO

      gTr(iDIC )=gTr(iDIC ) + respir

Cz      gTr(iDOC )=gTr(iDOC ) + exude_DOC
Cz      gTr(iDON )=gTr(iDON ) + exude_DON
Cz      gTr(iDOP )=gTr(iDOP ) + exude_DOP
Cz      gTr(iDOFe)=gTr(iDOFe) + exude_DOFe
Cz add dom2        
      gTr(iDOC )=gTr(iDOC ) + exude_DOC*wdom1
      gTr(iDON )=gTr(iDON ) + exude_DON*wdom1
      gTr(iDOP )=gTr(iDOP ) + exude_DOP*wdom1
      gTr(iDOFe)=gTr(iDOFe) + exude_DOFe*wdom1
      gTr(iDOC2 )=gTr(iDOC2 ) + exude_DOC*wdom2
      gTr(iDON2 )=gTr(iDON2 ) + exude_DON*wdom2
      gTr(iDOP2 )=gTr(iDOP2 ) + exude_DOP*wdom2
      gTr(iDOFe2)=gTr(iDOFe2) + exude_DOFe*wdom2
      gTr(iDOC3 )=gTr(iDOC3 ) + exude_DOC*wdom3
      gTr(iDON3 )=gTr(iDON3 ) + exude_DON*wdom3
      gTr(iDOP3 )=gTr(iDOP3 ) + exude_DOP*wdom3
      gTr(iDOFe3)=gTr(iDOFe3) + exude_DOFe*wdom3
      gTr(iDOC4 )=gTr(iDOC4 ) + exude_DOC*wdom4
      gTr(iDON4 )=gTr(iDON4 ) + exude_DON*wdom4
      gTr(iDOP4 )=gTr(iDOP4 ) + exude_DOP*wdom4
      gTr(iDOFe4)=gTr(iDOFe4) + exude_DOFe*wdom4
      gTr(iDOC5 )=gTr(iDOC5 ) + exude_DOC*wdom5
      gTr(iDON5 )=gTr(iDON5 ) + exude_DON*wdom5
      gTr(iDOP5 )=gTr(iDOP5 ) + exude_DOP*wdom5
      gTr(iDOFe5)=gTr(iDOFe5) + exude_DOFe*wdom5

      gTr(iPIC )=gTr(iPIC ) + exude_PIC
      gTr(iPOSi)=gTr(iPOSi) + exude_POSi
Cejz: add pom2 and pom3
Cez      gTr(iPOC )=gTr(iPOC ) + exude_POC
Cez      gTr(iPON )=gTr(iPON ) + exude_PON
Cez      gTr(iPOP )=gTr(iPOP ) + exude_POP
Cez      gTr(iPOFe)=gTr(iPOFe) + exude_POFe
      gTr(iPOC )=gTr(iPOC ) + exude_POC*wpom1
      gTr(iPON )=gTr(iPON ) + exude_PON*wpom1
      gTr(iPOP )=gTr(iPOP ) + exude_POP*wpom1
      gTr(iPOFe)=gTr(iPOFe) + exude_POFe*wpom1
      gTr(iPOC2 )=gTr(iPOC2 ) + exude_POC*wpom2
      gTr(iPON2 )=gTr(iPON2 ) + exude_PON*wpom2
      gTr(iPOP2 )=gTr(iPOP2 ) + exude_POP*wpom2
      gTr(iPOFe2)=gTr(iPOFe2) + exude_POFe*wpom2
      gTr(iPOC3 )=gTr(iPOC3 ) + exude_POC*wpom3
      gTr(iPON3 )=gTr(iPON3 ) + exude_PON*wpom3
      gTr(iPOP3 )=gTr(iPOP3 ) + exude_POP*wpom3
      gTr(iPOFe3)=gTr(iPOFe3) + exude_POFe*wpom3
#ifdef DARWIN_ALLOW_CDOM
      exude_CDOM = fracCDOM*exude_DOP
      gTr(iCDOM)=gTr(iCDOM) + exude_CDOM
      gTr(iDOC )=gTr(iDOC )              - R_CP_CDOM*exude_CDOM
      gTr(iDON )=gTr(iDON )              - R_NP_CDOM*exude_CDOM
      gTr(iDOP )=gTr(iDOP ) - exude_CDOM
      gTr(iDOFe)=gTr(iDOFe)              - R_FeP_CDOM*exude_CDOM
#endif

Cejz: total bact uptake and consumption diagnostics:
Cejz: this should always be filled with the same value, 0.2 
Cejz      diags(itest) = yield(7) 
      diags(itest) = diags(itest) + yield(7)
Cejz: assuming no CDOM 
      diags(iconsPOC)  = consumPOC 
      diags(iconsPOC2) = consumPOC2 
      diags(iconsPOC3) = consumPOC3 
      diags(iconsDOC)  = consumDOC 
      diags(iconsDOC2) = consumDOC2 
      diags(iconsDOC3) = consumDOC3 
      diags(iconsDOC4) = consumDOC4 
      diags(iconsDOC5) = consumDOC5 
      
      diags(iconsPON)  = consumPON 
      diags(iconsPON2) = consumPON2 
      diags(iconsPON3) = consumPON3 
      diags(iconsDON)  = consumDON 
      diags(iconsDON2) = consumDON2 
      diags(iconsDON3) = consumDON3 
      diags(iconsDON4) = consumDON4 
      diags(iconsDON5) = consumDON5 
      
      diags(iprodPOC)  = graz2POC*wpom1 + exude_POC*wpom1 
      diags(iprodPOC2) = graz2POC*wpom2 + exude_POC*wpom2 
      diags(iprodPOC3) = graz2POC*wpom3 + exude_POC*wpom3 
      diags(iprodDOC)  = (graz2OC  - graz2POC)*wdom1 + exude_DOC*wdom1 +
     &                   solubilPOC*wpomtodom1 
      diags(iprodDOC2) = (graz2OC  - graz2POC)*wdom2 + exude_DOC*wdom2 +
     &                   solubilPOC*wpomtodom2 + solubilPOC2*wpomtodom1 
      diags(iprodDOC3) = (graz2OC  - graz2POC)*wdom3 + exude_DOC*wdom3 +
     &                   solubilPOC*wpomtodom3 + solubilPOC2*wpomtodom2+
     &                   solubilPOC3*wpomtodom1 
      diags(iprodDOC4) = (graz2OC  - graz2POC)*wdom4 + exude_DOC*wdom4 +
     &                   solubilPOC2*wpomtodom3 + solubilPOC3*wpomtodom2
      diags(iprodDOC5) = (graz2OC  - graz2POC)*wdom5 + exude_DOC*wdom5 +
     &                   solubilPOC3*wpomtodom3 

#endif /* ALLOW_DARWIN */

      RETURN
      END SUBROUTINE

