C**********************************************************************
C Subroutines for BEWDY program
C
C	ZEROHR - Zeroes hourly arrays
C	OUTPUTDYU - Outputs daily data
C	OUTPUTHRU - Outputs hourly data
C	OPENBEWDYINPUTF - Opens input files
C	OPENBEWDYOUTPUTF - Opens output files
C	INPUTBEWDYPARS - reads in data from Bewdy input file
C	INPUTBEWDYARRAYS - Reads in LAI and N data
C	INTERP - Interpolates values of leaf N and LAI
C	BEWDYPARMS - Calculates Bewdy parameters from inputs and met data
C	BEWDY - Bewdy calculations
C	INPUTBEWDYGROW - Read in parameters of growth model
C	GROW - Grow the plant
C	OUTPUTDYGROW - Daily output from growth version
C**********************************************************************

C**********************************************************************
      SUBROUTINE ZEROHR(
     &THRAB, FCO2, FRESPF, FRESPFr, FRESPCR, FH2OT, GSCAN
     &)
C This is subroutine to set the initial values of hourly total variables
C to zero.
C**********************************************************************

      INCLUDE 'maestcom'
      REAL THRAB(KHRS)
      REAL FCO2(KHRS),FRESPF(KHRS), FRESPFr(KHRS), FRESPCr(KHRS)
      REAL GSCAN(KHRS),FH2OT(KHRS)

      DO 10 I = 1,KHRS
          FCO2(I) = 0.0
          FRESPF(I) = 0.0
          FH2OT(I) = 0.0
          GSCAN(I) = 0.0
          THRAB(I) = 0.0
          FRESPFr(I) = 0.0
          FRESPCr(I) = 0.0

10    CONTINUE

      RETURN
      END !ZeroHr


C**********************************************************************
      SUBROUTINE OUTPUTDYU(IDAY,THRAB,FCO2,FH2OT,FRESPF,FRESPFr,
     &FRESPCr,LAIa)
C Output the DAILY totals
C**********************************************************************

      INCLUDE 'MAESTCOM'
      REAL THRAB(KHRS),FCO2(KHRS),FH2OT(KHRS),FRESPF(KHRS),
     &FRESPFr(KHRS), FRESPCr(KHRS)



      PART = 0.0
      CO2T = 0.0
      H2OT = 0.0
      RDT = 0.0


      DO 10 IHR = 1,KHRS
          PART = PART+THRAB(IHR)
          CO2T = CO2T+FCO2(IHR)
          H2OT = H2OT+FH2OT(IHR)
          RDT = RDT+FRESPF(IHR)+FRESPFr(IHR)+ FRESPCr(IHR)


10    CONTINUE

      PART = PART*SPERHR/UMOLPERJ*1E-6	!umol s-1 to MJ d-1
      CO2T = CO2T*SPERHR*1E-6*GCPERMOL	!umol s-1 to gC d-1
      H2OT = H2OT*SPERHR*1E-3				!mmol s-1 to mol d-1
      RDT = RDT*SPERHR*1E-6*GCPERMOL		!umol s-1 to gC d-1
      WRITE (UDAILY,500) IDAY,PART,CO2T,H2OT,RDT,LAIa
500   FORMAT (I7,1X,1(F12.5,1X),4(F12.7,1X))


      RETURN
      END !OutputDyU


C**********************************************************************
      SUBROUTINE OPENBEWDYINPUTF(VTITLE,BTITLE)
C Open BEWDY input files.
C**********************************************************************

      INCLUDE 'maestcom'
      CHARACTER*80 VTITLE, BTITLE

C Open error file
      OPEN (UERROR, FILE = 'Maeserr.dat', STATUS = 'UNKNOWN')

C Open input file
      OPEN (USTOREYI, FILE = 'BEWDYIN.dat', STATUS = 'OLD',
     &IOSTAT=IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('ERROR: BEWDY INPUT FILE DOES NOT EXIST',
     &IFATAL,IOERROR)

C Read title from input file
990   FORMAT (A60)     ! For reading titles in input files.
      READ (USTOREYI, 990) BTITLE

      RETURN
      END !OpenBewdyInputF


C**********************************************************************
      SUBROUTINE OPENBEWDYOUTPUTF(MTITLE,VTITLE,BTITLE)
C Open BEWDY OUTPUT files.
C**********************************************************************

      INCLUDE 'maestcom'
      CHARACTER*80 MTITLE, VTITLE, BTITLE

C Open output files       
      OPEN (UDAILY, FILE = 'BEWDYDY.dat', STATUS = 'UNKNOWN')
      OPEN (UHRLY, FILE = 'BEWDYHR.dat', STATUS = 'UNKNOWN')
      OPEN (UYRLY, FILE = 'BEWDYYR.dat', STATUS = 'UNKNOWN')

C Write headings to output file
991   FORMAT (A12,A60) ! For writing comments to output files.
      WRITE (UHRLY, 991) 'Program:    ', VTITLE
      WRITE (UHRLY, 991) 'BEWDY:      ', BTITLE
      WRITE (UHRLY, 991) 'Met data:   ', MTITLE
      WRITE (UHRLY, *)
      WRITE (UHRLY, 101)
      WRITE (UHRLY, 102)
      WRITE (UHRLY, 103)
      WRITE (UHRLY, 104)
      WRITE (UHRLY, 105)
      WRITE (UHRLY, 106)
      WRITE (UHRLY, *)
      WRITE (UHRLY, 107)
      WRITE (UHRLY, 108)
      WRITE (UHRLY, 109)


 101  FORMAT('DAY')
 102  FORMAT('HOUR')
 103  FORMAT('APAR: Absorbed PAR 	umol m-2 s-1')
 104  FORMAT('PS: Photosynthesis 	umol m-2 s-1')
 105  FORMAT('ET: Transpiration 	mmol m-2 s-1')
 106  FORMAT('RD: Foliage Maintenance Respiration 	umol m-2 s-1')
 107  FORMAT('RDFR: fine root Maintenance Respiration 	umol m-2 s-1')
 108  FORMAT('RDCR: coarse root Maintenance Respiration 	umol m-2 s-1')
 109  FORMAT('Columns: DAY HOUR APAR PS ET RD RDFR RDCR')

      WRITE (UDAILY, 991) 'Program:    ', VTITLE
      WRITE (UDAILY, 991) 'BEWDY:      ', BTITLE
      WRITE (UDAILY, 991) 'Met data:   ', MTITLE
      WRITE (UDAILY, *)
      WRITE (UDAILY, 201)
      WRITE (UDAILY, 202)
      WRITE (UDAILY, 203)
      WRITE (UDAILY, 204)
      WRITE (UDAILY, 205)
      WRITE (UDAILY, 206)
      WRITE (UDAILY, 207)
      WRITE (UDAILY, 208)
      WRITE (UDAILY, 209)
      WRITE (UDAILY, 210)
      WRITE (UDAILY, 211)

201   FORMAT('DAY')
202   FORMAT('APAR: Absorbed PAR 	MJ m-2 d-1')
203   FORMAT('PS: Photosynthesis 	g C m-2 d-1')
204   FORMAT('ET: Transpiration 	mol H2O m-2 d-1')
205   FORMAT('Shoot: Kg DM m-2 d-1')
206   FORMAT('Fine root: Kg DM m-2 d-1')
207   FORMAT('Coarse root: Kg DM m-2 d-1')
208   FORMAT('Dead Foliage: ')
209   FORMAT('LAI: m2 leaf m-2 ground')
210   FORMAT('NEWDM')
211   FORMAT('Columns:  DAY  APAR  PS  ET  Shoot  Root FineRoot
     & CoarseRoot DeadFol  LAI NEWDM')

      WRITE (UYRLY, 991) 'Program:    ', VTITLE
      WRITE (UYRLY, 991) 'BEWDY:      ', BTITLE
      WRITE (UYRLY, 991) 'Met data:   ', MTITLE
      WRITE (UYRLY, *)
      WRITE (UYRLY, 301)      
      WRITE (UYRLY, 302)
      WRITE (UYRLY, 303)
      WRITE (UYRLY, 304)
      WRITE (UYRLY, 305)
      WRITE (UYRLY, 306)
      WRITE (UYRLY, 307)
      WRITE (UYRLY, 308)
      WRITE (UYRLY, 309)
      WRITE (UYRLY, 310)
      WRITE (UYRLY, 311)

301   FORMAT('YEAR')
302   FORMAT('NPPTHISYRa')
303   FORMAT('SHOOTNEXTYRa')
304   FORMAT('ROOTNEXTYRa')
305   FORMAT('LAIMAXNEWa')
306   FORMAT('NUPTAKEa')
307   FORMAT('YEARLYSHOOTNITa')
308   FORMAT('YEARLYFINEROOTNITa')
309   FORMAT('YRLYPARa')
310   FORMAT('LUEa')

 311   FORMAT('Columns: YEAR  NPPTHISYRa  SHOOTNEXTYRa
     & ROOTNEXTYRa LAIMAXNEWa NUPTAKEa
     & YEARLYSHOOTNITa  YEARLYFINEROOTNITa
     & YRLYPARa LUEa')


      RETURN
      END ! OPENBEWDYOUTPUTF


C**********************************************************************
      SUBROUTINE INPUTBEWDYPARS(
     &JMAXN25aI,IECOUI,EAVJI,EDVJI,DELSJI,TVJUPI,TVJDNI,
     &VCMAXN25aI,EAVCI,EDVCI,DELSCI,NMINJ,NMINV,
     &AJQI,ABSRPI,GSBG0U,GSBG1U,CICARATI,RD0I,RDFrI,RDCrI,RDKI,RDTI,
     &SLAaI,EFFYI,EXTKUS)
C Get BEWDY  parameters.
C 16/8/00 Change to take Ball-Berry params for gs only.
C 22/12/03 Add option to specify Ci:Ca ratio - helpful for moss
C**********************************************************************

      INCLUDE 'maestcom'

      INTEGER UFILE
      REAL JMAXN25a,JMAXN25aI,VCMAXN25a,VCMAXN25aI
      REAL JMAXINT,NMINJ,VCMAXINT,NMINV
      REAL KEXT

      NAMELIST /JMAXPARS/ IECO,EAVJ,EDVJ,DELSJ,AJQ
      NAMELIST /VCMAXPARS/ EAVC,EDVC,DELSC,TVJUP,TVJDN
      NAMELIST /BEWDYPARS/ JMAXN25a,VCMAXN25a,JMAXINT,VCMAXINT,
     &ABSRP,KEXT
      NAMELIST /CICA/ CICARAT
      NAMELIST /BBGS/ G0, G1a
      NAMELIST /RDPARS/ RD0,RDK,RDT,SLAa,EFFY,RDFr,RDCr

      UFILE = USTOREYI

      KEXT = 0.0
      JMAXN25a = 0.0
      VCMAXN25a = 0.0
      ABSRP = 0.0

C Read in BEWDY-specific params
      REWIND (UFILE)
      READ (UFILE,BEWDYPARS,IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('INPUT ERROR: MISSING BEWDY PARAMS',
     &IFATAL,IOERROR)
      JMAXN25aI = JMAXN25a
      VCMAXN25aI = VCMAXN25a
      NMINJ = -JMAXINT/JMAXN25a
      NMINV = -VCMAXINT/VCMAXN25a
      ABSRPI = ABSRP
      EXTKUS = KEXT
      IF (KEXT*JMAXN25a*VCMAXN25a*ABSRP.EQ.0.0)
     &CALL SUBERROR('ERROR IN BEWDY PARAMS',IFATAL,0)


C Read in T-response parameters
      REWIND (UFILE)
      AJQ = ALPHAQ !Default values
      IECO = 1 ! Ecocraft formulation of T-deps of Km and Gamma. For Montpied formulation, put 0.
      EDVC = 0.0
      DELSC = 0.0
      TVJUP = -100.0
      TVJDN = -100.0

      READ (UFILE,JMAXPARS,IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('INPUT ERROR: MISSING UNDERSTOREY JMAXPARS',
     &IFATAL,IOERROR)
      REWIND (UFILE)
      READ (UFILE,VCMAXPARS,IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('INPUT ERROR: MISSING UNDERSTOREY VCMAXPARS',
     &IFATAL,IOERROR)

      EAVCI = EAVC
      EDVCI = EDVC
      DELSCI = DELSC
      EAVJI = EAVJ
      EDVJI = EDVJ
      DELSJI = DELSJ
      AJQI = AJQ
      IECOUI = IECO
      TVJUPI = TVJUP
      TVJDNI = TVJDN

C Read in stomatal conductance params (Ball-Berry model only or Ci:Ca ratio)
      REWIND (UFILE)
      CICARATI = 0.0
      READ (UFILE, CICA,IOSTAT = IOERROR)
      IF (IOERROR.EQ.0) THEN

          CICARATI = CICARAT
          GSBG0U = 0.0
          GSBG1U = 0.0

      ELSE

          REWIND (UFILE)
          READ (UFILE, BBGS,IOSTAT = IOERROR)
          IF (IOERROR.NE.0)
     &    CALL SUBERROR('INPUT ERROR: MISSING UNDERSTOREY GS PARS',
     &    IFATAL,IOERROR)
          GSBG0U = G0
          GSBG1U = G1a
          IF (G1a.LT.0.0)
     &    CALL SUBERROR('ERROR IN GS PARAMETERS: G1a MUST BE > 0',
     &    IFATAL,0)


      END IF


C Read in respiration parameters
      REWIND(UFILE)
      RD0 = 0.0
      RDFr = 0.0
      RDCr = 0.0
      RDK = 0.0
      RDT = 0.0
      SLAa = -1.0
      EFFY = 0.0
      READ (UFILE, RDPARS, IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('WARNING: MISSING UNDERSTOREY RD PARS',
     &IWARN,IOERROR)
      RD0I = RD0
      RDFrI = RDFr
      RDCrI = RDCr
      RDKI = RDK
      RDTI = RDT
      SLAaI = SLAa
      EFFYI = EFFY

C Calculate foliar extinction coefficient - is in vertical direction only NO READ IN DIRECTLY
C      CALL READLIA(USTOREYI, NALPHA, ALPHA, FALPHA)
C      CALL EXBEAM(NALPHA,ALPHA,FALPHA,1.0,0.0,EXTKUS,BEXTANG)

      RETURN
      END ! InputBEWDYPars

C**********************************************************************
      SUBROUTINE INPUTBEWDYARRAYS(
     &NOLDATES,DATESLAI,VALUESLAI,NONDATES,DATESN,VALUESN)
C Get values of LAI and leaf N
C**********************************************************************

      INCLUDE 'maestcom'

      INTEGER UFILE
      REAL VALUESLAI(MAXTDATE,MAXT),VALUESN(MAXTDATE,MAXT)
      INTEGER DATESLAI(MAXTDATE),DATESN(MAXTDATE)

      UFILE = USTOREYI

C Read in LAI and foliar N arrays
      CALL READTREEARRAY(UFILE,5,1,NOLDATES,DATESLAI,VALUESLAI)
      CALL READTREEARRAY(UFILE,7,1,NONDATES,DATESN,VALUESN)

      RETURN
      END ! InputBEWDYArrays


C**********************************************************************
      SUBROUTINE INTERP(IDAY,ISTART,
     &NOLDATES,DATESLAI,VALUESLAI,NONDATES,DATESN,VALUESN,
     &TODAYSLAI,TODAYSN)
C Interpolate values of LEAF N AND LAI
C**********************************************************************

      INCLUDE 'maestcom'

C Dates for dimensions
      INTEGER DATESLAI(MAXTDATE),DATESN(MAXTDATE)
C Values of leaf N and LAI
      REAL VALUESLAI(MAXTDATE,MAXT),VALUESN(MAXTDATE,MAXT)
      INTEGER NOTREES
      REAL PARAMS(MAXTT)

      NOTREES=1
      CALL TREEINTERP(IDAY,ISTART,NOLDATES,DATESLAI,VALUESLAI,NOTREES,
     &PARAMS)
      TODAYSLAI = PARAMS(1)
      CALL TREEINTERP(IDAY,ISTART,NONDATES,DATESN,VALUESN,NOTREES,
     &PARAMS)
      TODAYSN = PARAMS(1)

      RETURN
      END !InterpUS


C**********************************************************************
      SUBROUTINE BEWDYPARMSA(TAIR,RH,VPD, CA,
     &JMAXN25a,IECOU,EAVJ,EDVJ,DELSJ,TVJUP,TVJDN,
     &VCMAXN25a,EAVC,EDVC,DELSC,
     &AJQ,G0,G1a,CICARAT,
     &BALPHAa,BLAMBDAJa,BLAMBDAVa)
C Calculates parameters for BEWDY model (Alpha, Lambda) from the Farquhar/Leuning models.
C**********************************************************************

      REAL TAIR,CA,RH, VPD
      REAL JMAXN25a,EAVJ,EDVJ,DELSJ,TVJUP,TVJDN
      REAL VCMAXN25a,EAVC,EDVC,DELSC
      REAL AJQ,G0,G1a
      REAL BALPHAa,BLAMBDAa
      REAL GAMMASTAR,KM,JMAXN,VCMAXN,CI,AJMAX,ACMAX
      REAL KMFN, JMAXTFN, VCMAXTFN

C Calculate photosynthetic p3arameters from leaf temperature - here assumed same as air T.
      GAMMASTAR = GAMMAFN(TAIR,IECOU)                   ! CO2 compensation point, umol mol-1
      KM = KMFN(TAIR,IECOU)                             ! Michaelis-Menten for Rubisco, umol mol-1
      JMAXN = JMAXTFN(JMAXN25a,TAIR,EAVJ,EDVJ,DELSJ,TVJUP,TVJDN)      ! Potential electron transport rate, umol m-2 s-1
      VCMAXN = VCMAXTFN(VCMAXN25a,TAIR,EAVC,EDVC,DELSC,TVJUP,TVJDN)   ! Maximum Rubisco activity, umol m-2 s-1

c      write(77,*) TAIR, GAMMASTAR
C Calculate ACMAX
c      GSDIVA = G1 /1.6 * RH / (CS - GAMMASTAR)
c      A = G0 + GSDIVA * VCMAXN
c      B = (1. - CS*GSDIVA) * VCMAXN + G0 * (KM - CS)
c     +      - GSDIVA * VCMAXN*GAMMASTAR 
c      C = -(1. - CS*GSDIVA) * VCMAXN*GAMMASTAR - G0*KM*CS
c      CIC = QUADP(A,B,C,IQERROR)

c      IF ((IQERROR.EQ.1).OR.(CIC.LE.0.0).OR.(CIC.GT.CS)) THEN
c        ACMAX = 0.0
c      ELSE
c        ACMAX = VCMAXN * (CIC - GAMMASTAR) / (CIC + KM)
c      END IF

C Calculate AJMAX
c      A = G0 + GSDIVA * JMAXN
c      B = (1. - CS*GSDIVA) * JMAXN + G0 * (2.*GAMMASTAR - CS)
c     +      - GSDIVA * JMAXN*GAMMASTAR
c      C = -(1. - CS*GSDIVA) * GAMMASTAR * JMAXN 
c     +      - G0*2.*GAMMASTAR*CS
c      CIJ = QUADP(A,B,C,IQERROR)

c      IF ((IQERROR.EQ.1).OR.(CIJ.LE.0.0).OR.(CIJ.GT.CS)) THEN
c        AJMAX = 0.0
c      ELSE
c        AJMAX = JMAXN/4. * (CIJ - GAMMASTAR) / (CIJ + 2*GAMMASTAR)
c      END IF

C Calculate BALPHA
c      A = G0 + GSDIVA * AJQ
c      B = (1. - CS*GSDIVA) * AJQ + G0 * (2.*GAMMASTAR - CS)
c     +      - GSDIVA * AJQ*GAMMASTAR
c      C = -(1. - CS*GSDIVA) * GAMMASTAR * AJQ 
c     +      - G0*2.*GAMMASTAR*CS
c      CIJ = QUADP(A,B,C,IQERROR)

c      IF ((IQERROR.EQ.1).OR.(CIJ.LE.0.0).OR.(CIJ.GT.CS)) THEN
c        BALPHA = 0.0
c      ELSE
c        BALPHA = AJQ/4. * (CIJ - GAMMASTAR) / (CIJ + 2*GAMMASTAR)
c      END IF

C Calculate Ci from Ball-Berry function
C      CI = CA - (CA-GAMMASTAR)*(1+VPD/GSLD0)*1.6/GSLG1
!	IF (CICARAT.GT.0.0) THEN
!	  CI = CA*CICARAT
!	ELSE 
!        CI = CA - (CA-GAMMASTAR)/RH*1.6/G1a
!	END IF

      IF (CICARAT.GT.0.0) THEN
          CI = CA*CICARAT
      ELSE
          CI = G1a/(sqrt(VPD*0.001)+G1a)*CA
c           CI = CA - (CA-GAMMASTAR)/RH*1.6/G1
      END IF

C Calculate Bewdy parameters
      BAlphaa = AJQ/4.0*(CA-gammastar)/(CA+2.*gammastar)  ! Changed from Ci - according to McM & Wang
      BLAMBDAJa = jmaxn/4.0*(ci-gammastar)/(ci+2.*gammastar)
      BLAMBDAVa = vcmaxn*(ci-gammastar)/(ci+km)

      RETURN
      END !BewdyParmsA




C**********************************************************************
      SUBROUTINE READSOLVEPARS(SCRI,SFRI,CRFRATIOI,FRFRATIOI,
     &DAYONEaI,DAYTWOaI,DAYTHREEaI,DAYFOURaI)
C Read in BEWDY SOLVE PARAMETERS.
C**********************************************************************

      INCLUDE 'MAESTCOM'
      NAMELIST /BEWDYSOLVE/SCR,SFR,CRFRATIO,FRFRATIO,
     &DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa
      INTEGER UFILE
      INTEGER DAYONEaI,DAYTWOaI,DAYTHREEaI,DAYFOURaI
      INTEGER DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa
      REAl SCRI,SFRI,CRFRATIOI,FRFRATIOI
      REAL SCR,SFR,CRFRATIO,FRFRATIO


      UFILE = USTOREYI

      SCR = 0.0
      SFR = 0.0
      CRFRATIO = 0.0
      FRFRATIO = 0.0
      DAYONEa = 0
      DAYTWOa = 0
      DAYTHREEa = 0
      DAYFOURa = 0

      REWIND (UFILE)
      READ (UFILE,BEWDYSOLVE,IOSTAT = IOERROR)
      IF(IOERROR.NE.0)
     &CALL SUBERROR('COULD NOT FIND BEWDY SOLVE PARS',IFATAL,IOERROR)

      SCRI = SCR
      SFRI = SFR
      CRFRATIOI = CRFRATIO
      FRFRATIOI = FRFRATIO
      DAYONEaI = DAYONEa
      DAYTWOaI = DAYTWOa
      DAYTHREEaI = DAYTHREEa
      DAYFOURaI = DAYFOURa

      RETURN

      END !ReadSOLVEPARS

C**********************************************************************
      SUBROUTINE READNITROGENUPTAKEPARS(KRaI, NMINERALaI,RHOaI, KONE,
     &KTWO,FNRETaI)
C Read in NITROGEN UPTAKE PARAMETERS.
C**********************************************************************

      INCLUDE 'MAESTCOM'
      NAMELIST /NITROGENUPTAKE/KRa,NMINERALa,RHOa, KONE, KTWO,FNRETa
      INTEGER UFILE
      REAl KRaI,NMINERALaI,RHOaI, KONEI, KTWOI,FNRETaI
      REAL KRa,NMINERALa,RHOa, KONE, KTWO,FNRETa


      UFILE = USTOREYI

      KRa = 0.0
      NMINERALa= 0.0
      RHOa= 0.0
      KONE=0.0
      KTWO=0.0
      FNRETa = 0.0



      REWIND (UFILE)
      READ (UFILE,NITROGENUPTAKE,IOSTAT = IOERROR)
      IF(IOERROR.NE.0)
     &CALL SUBERROR('COULD NOT FIND NITROGEN UPTAKE PARS',
     &IFATAL,IOERROR)

      KRaI = KRa
      NMINERALaI= NMINERALa
      RHOaI= RHOa
      KONEI = KONE
      KTWOI = KTWO
      FNRETaI = FNRETa



      RETURN

      END !ReadNUPTAKEPARS

C**********************************************************************
      SUBROUTINE BEWDYa(PARa,FBEAM,BALPHAa,BLAMBDAJa,BLAMBDAVa,LEAFNAVa,
     &NMINJ,NMINV,KEXT,ABSRP,LAIa,APARa,PSSa)
C Calculates C assimilation using the BEWDY model. 
C**********************************************************************

      REAL L,LJ,LV,LAIa,KEXT,LEAFNAVa,NMINJ,NMINV,N0J,N0V
      REAL PSa, PSb, PSc, PSd, PSe, PSf, PSg, PSh, PSi, PSj
      REAL PSk, PSl, PSm, PSn, PSo, PSp, PSq, PSr
      REAL theto
      theto = 0.7

c      write(77,*) LEAFNAV,NMINJ, NMINV, NOJ,NOV 
      APARa = PARa*ABSRP*(1.-EXP(-KEXT*LAIa))
c     APARa = PARa*(1.-EXP(-KEXT*LAIa))
      BEAM = FBEAM*PARa
      DIFF = (1.-FBEAM)*PARa
      N0J = (LEAFNAVa-NMINJ)*LAIa*KEXT/(1.-EXP(-KEXT*LAIa))+NMINJ
      N0V = (LEAFNAVa-NMINV)*LAIa*KEXT/(1.-EXP(-KEXT*LAIa))+NMINV
      b = BAlphaa * Kext * BEAM * absrp
      d = BAlphaa * Kext * DIFF * absrp
c      b = BAlphaa * Kext * BEAM
c      d = BAlphaa * Kext * DIFF
      lJ = BLambdaJa * (N0J-NMINJ)
      lv = BLambdaVa * (N0V-NMINV)
      IF (LJ.LT.LV) THEN
          L = LJ
      ELSE
          L = LV
      END IF


C1	PS = 1./Kext*(1.-exp(-Kext*lai))*(l*d*(l+d)+b*l*l)/((l+d)**2)
C1      PS = PS + 1./Kext*(b*b*l*l)/((l+d)**3)*
C1     &  log(((l+d)*exp(-Kext*lai)+b)/(l+d+b))
c2	PS = SUNLA*PS + (1-SUNLA)*1./Kext*(1.-exp(-Kext*lai))*(l*d)/(l+d)
C3       PS = exp(-2*kext*lai)*((-1+ exp(kext*lai))**2)*l*d/(2*kext*(l+d))
C3       PS = PS + 1/(2*Kext*(l+d)**3)*(exp(-2*kext*lai)*l*((l+
C3     &  d)*(2*b*exp(kext*lai)*(-1+ exp(kext*lai))*l + 
C3     &  (-1+ exp(2*kext*lai))*d*(l+d)) - 
C3     &  2*b*b*exp(2*kext*lai)*l*(log(b+l+d) - 
C3     &  log(b+ exp(-kext*lai)*(l+d)))))
C4      PS =  1/(4*kext*theta)*(n - Exp(-2*kext*lai)*n + 
C4     &   s -Exp(-2*kext*lai)*s - 
C4     &   Sqrt(n**2 + s**2 + n*s*(2 - 4*theta)) + 
C4     &   Exp(-kext*lai)* Sqrt(Exp(- 2*kext*lai)*(n**2 + 
C4     &   s**2 + n*s*(2 - 4*theta))) +
C4     &   1/(4*kext*theta)*(2*(b-b*exp(-kext*lai))+n-exp(-2*kext*lai)*n+
C4     &   s-exp(-2*kext*lai)*s - 1/(n**2 + s**2 + n*s*(2 - 
c4     &   4*theta))* (n**2 + 2*n*s + s**2 - 4*n*s*theta + 
c4     &   b*(n + s - 2*n*theta))*(sqrt(b**2 + n**2+ 2*n*s + s**2 - 
c4     &   4*n*s*theta + 2*b*(n + s - 2*n*theta))) +
c4     &   4*b**2*n**2*(-1 + theta)*theta * 
c4     &   log(2)/((n**2 + s**2 + n*s*(2 - 4*theta))**1.5))+
c4     &   (exp(kext*lai)*(exp(-2*kext*lai)*((b*exp(kext*lai)+
c4     &   n + s)**2 - 4*n*(b*exp(kext*lai)+
c4     &   s)*theta))**1.5 * ((1/(n**2+s**2+n*s*(2-4*theta)))*sqrt((n +
c4     &   s + b*exp(kext*lai))**2 - 4*n*(b*exp(kext*lai)+s)*theta)*(n**2+
c4     &   2*n*s + s**2 - 4*n*s*theta - b*exp(kext*lai)*(-n - s + 
c4     &   2*n*theta)) - 4*b**2 * exp(2*kext*lai)*n**2*(-1 + 
c4     &   theta)*theta *log(2)/((n**2 + s**2 + n*s*(2 - 4*theta))**1.5))-
c4     &   (4*b**2*exp(2*kext*lai)*n**2*(-1+theta)*theta* log((exp(-
c4     &   kext*lai)*(b*exp(kext*lai)*n + n**2 + b*exp(kext*lai)*s + s**2-
c4     &   2*b*exp(kext*lai)*n*theta - 4*n*s*theta + sqrt(n**2 + s**2 +
c4     &   n*s*(2 - 4*theta))* sqrt((b*exp(kext*lai) + n + s)**2 - 
c4     &   4*n*(b*exp(kext*lai) + s)*theta)))/ (sqrt(n**2 + s**2 + 
c4     &   n*s*(2 - 4*theta)))))/ ((n**2 + s**2 +
c4     &   n*s*(2 - 4*theta))**1.5)))/(n**2 + s**2 + n*s*(2 - 
c4     &   4*theta))**1.5))/ ((b*exp(kext*lai) + n + s)**2 -
c4     &   4*n*(b*exp(kext*lai) + s)*theta)**1.5 + 
c4     &   1/((n**2 + s**2 + n*s*(2-4*theta))**1.5) * 4*b**2*n**2*(-1+
c4     &   theta)*theta*log((1/sqrt(n**2 + s**2 - n*s*(2-4*theta))*((n**2+
c4     &   2*n*s + s**2 - 4*n*s*theta + b*(n + s- 2*n*theta)) + sqrt(n**2+
c4     &   s**2 + n*s*(2-4*theta)*sqrt(b**2 + n**2 + 2*n*s + s**2 -
c4     &   4*n*s*theta + 2*b*(n+s-2*n*theta))))
      PSa = 1./(4*kext*theto)
      PSb = 2*(b-b*exp(-kext*laia))+ l - exp(-2*kext*laia)*l +
     &d-exp(-2*kext*laia)*d
      PSc = 1./(l**2 + d**2 + l*d*(2 - 4*theto))*(l**2 +
     &2*l*d + d**2 - 4*l*d*theto +
     &b*(l + d - 2*l*theto))*sqrt(b**2 + l**2+ 2*l*d + d**2 -
     &4*l*d*theto + 2*b*(l + d - 2*l*theto))
      PSd = 4*b**2*l**2*(-1 + theto)*theto * log(2.0)/sqrt((l**2 +
     &d**2 + l*d*(2 - 4*theto))**3)
      PSe = exp(kext*laia)
      PSf = sqrt((exp(-2*kext*laia)*((b*exp(kext*laia)+
     &l + d)**2 - 4*l*(b*exp(kext*laia)+ d)*theto))**3)
      PSg = 1./(l**2+d**2+l*d*(2-4*theto))*sqrt((l +
     &d + b*exp(kext*laia))**2 - 4*l*(b*exp(kext*laia)+d)*theto)*
     &(l**2+
     &2*l*d + d**2 - 4*l*d*theto - b*exp(kext*laia)*(-l - d +
     &2*l*theto))
      PSh = 4*b**2 * exp(2*kext*laia)*l**2*(-1 +
     &theto)*theto*log(2.0)/sqrt((l**2+d**2 + l*d*(2 - 4*theto))**3)
      PSi = 4*b**2*exp(2*kext*laia)*l**2*(-1+theto)*theto
      PSj = exp(-kext*laia)
      PSk = b*exp(kext*laia)*l +l**2+ b*exp(kext*laia)*d + 2*l*d +
     &d**2 -
     &2*b*exp(kext*laia)*l*theto - 4*l*d*theto + sqrt(l**2 + d**2 +
     &l*d*(2 - 4*theto))* sqrt((b*exp(kext*laia) + l + d)**2 -
     &4*l*(b*exp(kext*laia) + d)*theto)
      PSl = sqrt(l**2 + d**2 + l*d*(2 - 4*theto))
      PSm = sqrt((l**2 + d**2 + l*d*(2 - 4*theto))**3)
      PSn = sqrt(((b*exp(kext*laia) + l + d)**2 -
     &4*l*(b*exp(kext*laia) + d)*theto)**3)
      PSo = 4*b**2*l**2*(-1+ theto)*theto
      PSp = 1./(sqrt(l**2 + d**2 + l*d*(2-4*theto)))*((l**2+
     &2*l*d + d**2 - 4*l*d*theto + b*(l + d- 2*l*theto)) + sqrt(l**2+
     &d**2 + l*d*(2-4*theto))*sqrt(b**2 + l**2 + 2*l*d + d**2 -
     &4*l*d*theto + 2*b*(l+d-2*l*theto)))
      PSq = sqrt((l**2 + d**2 + l*d*(2-4*theto))**3)
      PSr = l + d + Exp(-2*kext*laia)*(-1 + 2*EXP(kext*laia))* (-l -
     &d + Exp(kext*laia)* Sqrt(Exp(-2*kext*laia)*(l**2 +
     &d**2 + l*d*(2 - 4*theto)))) - Sqrt(l**2 + d**2 + l*d*(2 -
     &4*theto))
      PSSa = PSa*(PSb - PSc + PSd + 1./PSn*(PSe*PSf*(PSg - PSh -
     &1./PSm*(PSi*log(PSj*PSk/PSl)))) +
     &1./PSq*PSo*log(PSp)+ PSr)
      RETURN
      END ! Bewdy


C**********************************************************************
      SUBROUTINE INPUTBEWDYGROW(ROOTa,ROOTFr, ROOTCr, SHOOT,TODAYSNa,
     &LAIMAXFIRSTa,SLAaI,CFRACI,FDECAYI,RDECAYaI,ALLEAFaI,ALROOTaI,
     &ALROOTFrI,ALROOTCrI, RESPFRACI,RESPFRACGI)
C Input parameters needed for growth model
C**********************************************************************

      INCLUDE 'maestcom'

      INTEGER UFILE
      REAL ROOTa,ROOTFr, ROOTCr, SHOOT,INITROOTa, TOTLEAFNa, 
     & LAIMAXFIRSTa
      REAL INITROOTFr, INITROOTCr, INITSHOOT, INITTOTLEAFNa, 
     & INITLAIMAXFIRSTa
      REAL SLAaI,CFRACI,FDECAYI,RDECAYaI,ALLEAFaI,ALROOTaI,ALROOTFrI,
     &RESPFRACI,ALROOTCrI, RESPFRACGI
      REAL SLAa,CFRAC,FDECAY,RDECAYa,ALLEAFa,ALROOTa,ALROOTFr, ALROOTCr
     &,RESPFRAC, RESPFRACG

      NAMELIST /PLANTDATA/ INITROOTa,INITROOTFr, INITROOTCr, INITSHOOT,
     &INITTOTLEAFNa,INITLAIMAXFIRSTa
      NAMELIST /GROWPARS/ SLAa,CFRAC,FDECAY,RDECAYa,ALLEAFa,
     &ALROOTa,ALROOTFr, ALROOTCr, RESPFRAC,RESPFRACG

      UFILE = USTOREYI

C Read in BEWDY-specific params
      REWIND (UFILE)
      READ (UFILE,PLANTDATA,IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('INPUT ERROR: MISSING INITIAL PLANT DATA',
     &IFATAL,IOERROR)
      ROOTa = INITROOTa		! Initial Root DM
      ROOTFr = INITROOTFr   ! Initial FineRoot DM
      ROOTCr = INITROOTCr   ! Initial CoarseRoot DM
      SHOOT  = INITSHOOT    ! Initial Shoot DM
      TOTLEAFNa = INITTOTLEAFNa ! CONSTLEAFN = shoot N:C, held constant
      LAIMAXFIRSTa = INITLAIMAXFIRSTa


      REWIND (UFILE)
      READ (UFILE,GROWPARS,IOSTAT = IOERROR)
      IF (IOERROR.NE.0)
     &CALL SUBERROR('INPUT ERROR: MISSING GROWTH PARAMETERS',
     &IFATAL,IOERROR)
      SLAaI = SLAa
      CFRACI = CFRAC
      FDECAYI = FDECAY
      RDECAYaI = RDECAYa
      ALLEAFaI = ALLEAFa
      ALROOTaI = ALROOTa
      ALROOTFrI = ALROOTFr
      ALROOTCrI = ALROOTCr
      RESPFRACI = RESPFRAC
      RESPFRACGI = RESPFRACG



C	TODAYSN = CONSTLEAFN*CFRAC/SLA*1000. !Shoot N in g N m-2
      TODAYSNa = TOTLEAFNa


      RETURN
      END ! InputBEWDYGrow

C**********************************************************************
      SUBROUTINE OUTPUTHRU(IDAY,IHOUR,
     &THRAB,FCO2,FH2OT,FRESPF, FRESPFr, FRESPCr)
C Output the hourly totals
C**********************************************************************

      INCLUDE 'MAESTCOM'
      REAL THRAB(KHRS),FCO2(KHRS),FH2OT(KHRS),FRESPF(KHRS)
     &,FRESPFr(KHRS), FRESPCr(KHRS)

      WRITE (UHRLY,500) IDAY,IHOUR,
     &THRAB(IHOUR),FCO2(IHOUR),FH2OT(IHOUR),FRESPF(IHOUR),
     &FRESPFr(IHOUR), FRESPCr(IHOUR)
500   FORMAT (I7,1X,1(I4,1X),1(F12.5,1X),5(F12.7,1X))

      RETURN
      END !OutputHrU


C**********************************************************************
      SUBROUTINE GROW(SHOOT,ROOT,ROOTFr, ROOTCr, DEADFOL,FCO2,
     &ALLEAF,ALROOT,ALROOTFr, ALROOTCr, FDECAY,RDECAY,CFRAC,
     &RESPFRAC,RESPFRACG,FRESPF,FRESPFR,FRESPCR,NEWDM)
C First attempt to implement very simple growth model
C**********************************************************************

      INCLUDE 'BEWDYCOM'
      include 'maestcom'

      REAL FCO2(KHRS),NEWDM
      REAL FRESPF(KHRS),FRESPFR(KHRS),FRESPCR(KHRS)


      CO2T = 0.0
      RDT  = 0.0
      FRESPFT = 0.0
      FRESPFRT = 0.0
      FRESPCRT = 0.0


      DO 10 IHR = 1,KHRS
          CO2T = CO2T+FCO2(IHR)
          FRESPFT = FRESPFT+FRESPF(IHR)
          FRESPFRT = FRESPFRT+FRESPFR(IHR)
          FRESPCRT = FRESPCRT+FRESPCR(IHR)
          RDT = RDT+FRESPF(IHR)+FRESPFr(IHR)+ FRESPCr(IHR)


10    CONTINUE
      CO2T = CO2T*SPERHR*1E-6*GCPERMOL*X_KG_AS_T	!umol s-1 to kgC d-1
      FRESPFT = FRESPFT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      FRESPFRT = FRESPFRT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      FRESPCRT = FRESPCRT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      RDT = RDT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T



C     Root = rootfr + rootcr
C      ALLEAF = 1 - ALROOTFr - ALROOTCr
C      SHOOTchange = 0.0
C  SHOOTchange = NEWDM*ALLEAF - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
C SHOOTchange = - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
C    LAIchange = SHOOTchange*SLA
C      NEWDM =  CO2T/CFRAC*RESPFRACG - RDT/CFRAC
C     NEWDM = CO2T/CFRAC*RESPFRAC	! kg DM m-2 d-1
      NEWDM = CO2T/CFRAC*RESPFRACG - FRESPFT/CFRAC
     &- FRESPFRT/CFRAC - FRESPCRT/CFRAC	! kg DM m-2 d-1


      IF (NEWDM.GT.0.0) THEN
          SHOOT = SHOOT + NEWDM*ALLEAF - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
          ROOTFr = ROOTFr + NEWDM*ALROOTFr - RDECAY*ROOTFr*X_DAYS_AS_YRS		!kg DM m-2
          ROOTCr = ROOTCr + NEWDM*ALROOTCr - RDECAY*ROOTCr*X_DAYS_AS_YRS		!kg DM m-2

      ELSE
          SHOOT = SHOOT - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
          ROOTFr = ROOTFr + NEWDM*ALROOTFr/(ALROOTFR+ALROOTCR)
     &    - RDECAY*ROOTFr*X_DAYS_AS_YRS		!kg DM m-2
          ROOTCr = ROOTCr + NEWDM*ALROOTCr/(ALROOTFR+ALROOTCR)
     &    - RDECAY*ROOTCr*X_DAYS_AS_YRS		!kg DM m-2

      END IF

      RETURN
      END ! GrowFirst


C**********************************************************************
      SUBROUTINE GROWmontaldo(SHOOT,ROOT,DEADFOL,FCO2,
     &ALLEAF,ALROOT,FDECAY,RDECAY,CFRAC,RESPFRAC,RESPFRACG)
C Simple growth model from Montaldo (2005) Water Res Res
C**********************************************************************

      INCLUDE 'BEWDYCOM'
      include 'maestcom'

      REAL FCO2(KHRS),DA,DR,RX,KA,TR,PG,TAR,TRANS

C Additional paramaters
      DA = 0.0064
      DR = 0.005
      RX = 1.5
      KA = 0.1
      TR = 0.05

C Calculate daily photosynthetic uptake
      CO2T = 0.0
      DO 10 IHR = 1,KHRS
          CO2T = CO2T+FCO2(IHR)
10    CONTINUE
      PG = CO2T*SPERHR*1E-6*GCPERMOL*X_KG_AS_T/CFRAC	!umol s-1 to kg DM d-1

C Calculate allocation to root
      TAR = (RX*SHOOT - ROOT)/(1+RX)
      IF (TAR.LE.PG) THEN
          ALROOT = TAR/PG
      ELSE
          ALROOT = 1.0
      END IF
      ALLEAF = 1 - ALROOT

C Calculate transfer from root to shoot
      TRANS = TR*ROOT

C Update biomass pools
      SHOOT = SHOOT + ALLEAF*PG*RESPFRAC + TRANS - DA*SHOOT
      ROOT = ROOT + ALROOT*PG*RESPFRAC - TRANS - DR*ROOT
      DEADFOL = DEADFOL + DA*SHOOT - KA*DEADFOL

      RETURN
      END ! GrowMontaldo

C**********************************************************************
      SUBROUTINE OUTPUTDYGROW(IDAY,THRAB,FCO2,FH2OT,
     &SHOOT,ROOT,ROOTFr, ROOTCr, DEADFOL,TODAYSLAI,NEWDM)
C Output the DAILY totals FROM GROWTH MODEL
C**********************************************************************

      INCLUDE 'MAESTCOM'
      INCLUDE 'Bewdycom'
      REAL THRAB(KHRS),FCO2(KHRS),FH2OT(KHRS)
      REAL NEWDM, todayslai(365)


      PART = 0.0
      CO2T = 0.0
      H2OT = 0.0
      DO 10 IHR = 1,KHRS
          PART = PART+THRAB(IHR)
          CO2T = CO2T+FCO2(IHR)
          H2OT = H2OT+FH2OT(IHR)
10    CONTINUE

      PART = PART*SPERHR/UMOLPERJ*1E-6	!umol s-1 to MJ d-1
      CO2T = CO2T*SPERHR*1E-6*GCPERMOL*X_KG_AS_T	!umol s-1 to kgC d-1
      H2OT = H2OT*SPERHR*1E-3				!mmol s-1 to mol d-1
      WRITE (UDAILY,500) IDAY,PART,CO2T,H2OT,SHOOT,ROOT,ROOTFr,
     &ROOTCr, DEADFOL,TODAYSLAI,NEWDM
500   FORMAT (I7,1X,1(F12.5,1X),9(F12.7,1X))


      RETURN
      END !OutputDyGrow

C**********************************************************************
      SUBROUTINE GROWA(SHOOT,ROOT,ROOTFr, ROOTCr, DEADFOL,FCO2,
     &ALLEAF,ALROOT,ALROOTFr, ALROOTCr, FDECAY,RDECAY,CFRAC,
     &RESPFRAC,RESPFRACG,FRESPF,FRESPFR,FRESPCR,NEWDM)
C First attempt to implement very simple growth model
C**********************************************************************

      INCLUDE 'BEWDYCOM'
      include 'maestcom'

      REAL FCO2(KHRS),NEWDM
      REAL FRESPF(KHRS),FRESPFR(KHRS),FRESPCR(KHRS)


      CO2T = 0.0
      RDT  = 0.0
      FRESPFT = 0.0
      FRESPFRT = 0.0
      FRESPCRT = 0.0


      DO 10 IHR = 1,KHRS
          CO2T = CO2T+FCO2(IHR)
          FRESPFT = FRESPFT+FRESPF(IHR)
          FRESPFRT = FRESPFRT+FRESPFR(IHR)
          FRESPCRT = FRESPCRT+FRESPCR(IHR)
          RDT = RDT+FRESPF(IHR)+FRESPFr(IHR)+ FRESPCr(IHR)


10    CONTINUE
      CO2T = CO2T*SPERHR*1E-6*GCPERMOL*X_KG_AS_T	!umol s-1 to kgC d-1
      FRESPFT = FRESPFT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      FRESPFRT = FRESPFRT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      FRESPCRT = FRESPCRT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
      RDT = RDT*SPERHR*1E-6*GCPERMOL*X_KG_AS_T



C     Root = rootfr + rootcr
C      ALLEAF = 1 - ALROOTFr - ALROOTCr
C      SHOOTchange = 0.0
C  SHOOTchange = NEWDM*ALLEAF - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
C SHOOTchange = - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
C    LAIchange = SHOOTchange*SLA
C      NEWDM =  CO2T/CFRAC*RESPFRACG - RDT/CFRAC
C     NEWDM = CO2T/CFRAC*RESPFRAC	! kg DM m-2 d-1
      NEWDM = CO2T/CFRAC*RESPFRACG - FRESPFT/CFRAC
     &- FRESPFRT/CFRAC - FRESPCRT/CFRAC	! kg DM m-2 d-1


      IF (NEWDM.GT.0.0) THEN
          SHOOT = SHOOT + NEWDM*ALLEAF - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
          ROOTFr = ROOTFr + NEWDM*ALROOTFr - RDECAY*ROOTFr*X_DAYS_AS_YRS		!kg DM m-2
          ROOTCr = ROOTCr + NEWDM*ALROOTCr - RDECAY*ROOTCr*X_DAYS_AS_YRS		!kg DM m-2

      ELSE
          SHOOT = SHOOT - FDECAY*SHOOT*X_DAYS_AS_YRS	!kg DM m-2
          ROOTFr = ROOTFr + NEWDM*ALROOTFr/(ALROOTFR+ALROOTCR)
     &    - RDECAY*ROOTFr*X_DAYS_AS_YRS		!kg DM m-2
          ROOTCr = ROOTCr + NEWDM*ALROOTCr/(ALROOTFR+ALROOTCR)
     &    - RDECAY*ROOTCr*X_DAYS_AS_YRS		!kg DM m-2

      END IF

      RETURN
      END ! GrowFirst
C**********************************************************************
      SUBROUTINE OUTPUTDYGROWA(dailypart,dailyps,dailyH2OT,
     &SHOOT,ROOT,ROOTFr, ROOTCr, DEADFOL,TODAYSLAI,NEWDM)
C Output the DAILY totals FROM GROWTH MODEL
C**********************************************************************

      INCLUDE 'MAESTCOM'
      INCLUDE 'Bewdycom'
      REAL dailypart(365), dailyps(365), dailyH2OT(365)
      REAL NEWDM(365), todayslai(365)


      DO 10 IDAY = 1,365

          WRITE (UDAILY,500) IDAY,dailypart(iday),dailyps(iday),
     &    dailyH2OT(iday),SHOOT,ROOT,ROOTFr,
     &    ROOTCr, DEADFOL,TODAYSLAI(iday),NEWDM(iday)
500       FORMAT (I7,1X,1(F12.5,1X),9(F12.7,1X))
 10   continue

      RETURN
      END !OutputDyGrow

C**********************************************************************
      SUBROUTINE Outputdyphotoa(dailypsa, todayslaia)
C Output the DAILY totals FROM GROWTH MODEL
C**********************************************************************


      INCLUDE 'MAESTCOM'
      INCLUDE 'Bewdycom'
      REAL  dailypsa(365), Todayslaia(365)

C      do 11 iyear = 1, 8
      do 12 iday = 1,365

          WRITE (UDAILY,500) Iday, dailypsa(iday),todayslaia(iday)
500       FORMAT (I7,1X,2(F12.7,1X))

12    continue
C11    continue
      RETURN
      END !OutputDyGrow

C**********************************************************************     
      SUBROUTINE CALCULLAIa(LAIMAXa,DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa,
     &TODAYSLAIa)
C Calculates leaf area Index
C**********************************************************************
      INCLUDE 'MAESTCOM'

      INTEGER DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa
      REAL LAIMAXa, TODAYSLAIa(365)


      DO 11 IDAY = 1,365

          IF ((IDAY.GE.DAYONEa).AND.(IDAY.LE.DAYTWOa)) THEN
              TODAYSLAIa(IDAY)  = (IDAY - DAYONEa)*LAIMAXa
     &        /(DAYTWOa - DAYONEa)

          ELSE  IF ((IDAY.GE.DAYTWOa).AND.(IDAY.LE.DAYTHREEa
     &    )) THEN
              TODAYSLAIa(IDAY)  = LAIMAXa

          ELSE  IF ((IDAY.GE.DAYTHREEa).AND.(IDAY.LE.DAYFOURa
     &    )) THEN
              TODAYSLAIa(IDAY)  = -(IDAY - DAYFOURa)*LAIMAXa
     &        /(DAYFOURa- DAYTHREEa)

          ELSE
              TODAYSLAIa(IDAY) = 0.0

          END IF



  11  CONTINUE

      RETURN

      END   ! Finished calculating LAI


C**********************************************************************
      SUBROUTINE BEWDYAa(PARa,FBEAM,BALPHAa,BLAMBDAJa,BLAMBDAVa,
     &LEAFNAVa, NMINJ,NMINV,KEXT,ABSRP,LAIa,APARa,PSSa)
C Calculates C assimilation using the BEWDY model. 
C**********************************************************************

      REAL L,LJ,LV,LAIa,KEXT,LEAFNAVa,NMINJ,NMINV,N0J,N0V,Psa

c     APAR = PAR*ABSRP*(1.-EXP(-KEXT*LAI))
      APARa = PARa*(1.-EXP(-KEXT*LAIa))
      BEAM = FBEAM*PARa
      DIFF = (1.-FBEAM)*PARa
      N0J = (LEAFNAVa-NMINJ)*LAIa*KEXT/(1.-EXP(-KEXT*LAIa))+NMINJ
      N0V = (LEAFNAVa-NMINV)*LAIa*KEXT/(1.-EXP(-KEXT*LAIa))+NMINV
c      b = BAlpha * Kext * BEAM * absrp
c      d = BAlpha * Kext * DIFF * absrp
      b = BAlphaa * Kext * BEAM
      d = BAlphaa * Kext * DIFF
      lJ = BLambdaJa * (N0J-NMINJ)
      lv = BLambdaVa * (N0V-NMINV)
      IF (LJ.LT.LV) THEN
          L = LJ
      ELSE
          L = LV
      END IF
      PSa = 1./Kext*(1.-exp(-Kext*laia))*(l*d*(l+d)+b*l*l)/((l+d)**2)
      PSSa = PSa + 1./Kext*(b*b*l*l)/((l+d)**3)*
     &log(((l+d)*exp(-Kext*laia)+b)/(l+d+b))

      RETURN
      END ! Bewdy


C**********************************************************************	
      SUBROUTINE ROOTFINDER(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
     &RDECAYa, SHOOTNEXTYRa, yearlyshootnita,NDIFF)  ! DO THIS LATER !!

C Calculates approximate root for Annual Carbon Balance. 
C**********************************************************************
c      INCLUDE 'maestcom'
      REAL Nitupa
      REAL FNRETa,rhoa,ROOTNEXTYRa,RDECAYa,SHOOTNEXTYRa
      REAL yearlyshootnita
c     REAL NDIFF


      REAL A, B, MID, NDIFFA, NDIFFB, NDIFFMID, INTLEN, EPSIL
      LOGICAL ZERO


      A=1.0
      B=4.0
      EPSIL=1E-4

      CALL ANNUALNBALANCE(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
     &RDECAYa, SHOOTNEXTYRa, A,NDIFFA)

      CALL ANNUALNBALANCE(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
     &RDECAYa, SHOOTNEXTYRa, B,NDIFFB)

      IF (NDIFFA.EQ.0) THEN
          PRINT *, 'ROOT = ', A
      ELSE IF(NDIFFB.EQ.0)THEN
          PRINT *, 'ROOT = ',B
      ELSE IF (NDIFFA * NDIFFB.GT.0) THEN
          PRINT *, 'NDIFF(A) AND NDIFF(B) HAVE SAME SIGN.'
          PRINT *, 'INTERVAL MAY NOT CONTAIN A ROOT.'
      ELSE
          ZERO = .FALSE.
          INTLEN = B - A

C       While no exact zero found and length of interval is greater than EPSIL,
C       bisect the interval and search one of the halves
10        IF (.NOT. ZERO . AND. (INTLEN / 2.0 .GT. EPSIL)) THEN
              MID = (A + B) /2.0

              CALL ANNUALNBALANCE(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
     &        RDECAYa, SHOOTNEXTYRa, MID,NDIFFMID)

              IF (NDIFFMID.EQ.0) THEN
                  ZERO = .TRUE.
              ELSE IF (NDIFFA * NDIFFMID . LT. 0) THEN
C       Root in left half ---search it
                  B = MID
                  NDIFFB = NDIFFMID
              ELSE

C     Root in right half ---search it
                  A = MID
                  NDIFFA = NDIFFMID
              END IF
              INTLEN = INTLEN / 2

              GO TO 10
          END IF
          IF (ZERO) THEN
              PRINT *, 'ROOT =  ', MID

          ELSE
              PRINT *, 'APPROX. ROOT =  ', MID
              PRINT *, 'FUNCTION VALUE = ', NDIFFMID

          END IF

      END IF

      yearlyshootnita = MID
      NDIFF = NDIFFMID

      RETURN
      END


C**********************************************************************
      SUBROUTINE ANNUALNBALANCE(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
     &RDECAYa, SHOOTNEXTYRa, yearlyshootnita,NDIFF)
C Calculates C UPTAKE and C used AND OVERALL C BALANCE FOR YEAR. 
C**********************************************************************
c      INCLUDE 'maestcom'

      REAL Nitupa
      REAL FNRETa,rhoa,ROOTNEXTYRa,RDECAYa,SHOOTNEXTYRa
      REAL yearlyshootnita
      REAL NDIFF

      NDIFF = Nitupa - yearlyshootnita*((1.0 - FNRETa)+
     &rhoa*ROOTNEXTYRa*RDECAYa/SHOOTNEXTYRa)

c       NDIFF = Nitupa - folnitcratio*((1.0 - 0.5)+ 
c     &  0.7*ROOTNEXTYRa*0.75/SHOOTNEXTYRa) 

      Write (77,*) Nitupa,NDIFF,yearlyshootnita

      RETURN
      END
