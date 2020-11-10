
C =======================================================
C | BEWDY: September 2005 Version					      |
C | Run the Bewdy photosynthesis program				  |
C | Coupled to simple growth routine
C | - implementing nitrogen uptake process and its relations
C =======================================================

      PROGRAM RUNBEWDY

      INCLUDE 'maestcom'
	INCLUDE 'bewdycom'
      
C Array declarations.
C Met data
      INTEGER METCOLS(MAXMET)
      REAL windah(KHRS),tsoil(KHRS),tair(KHRS),radabv(KHRS,3)
      REAL FBEAM(KHRS,3),RH(KHRS),VPD(KHRS),VMFD(KHRS)
      REAL CA(KHRS),PRESS(KHRS),PPT(KHRS),SOILMD(KHRS)
	REAL DELTAT(12)
	INTEGER SOILDATA
C Physiology inputs by layer
      REAL ABSRP(MAXLAY,3),ARHO(MAXLAY,3),ATAU(MAXLAY,3)
      REAL RHOSOL(3)
C Values of LAI and N - not needed here
	REAL VALUESLAI(MAXTDATE,MAXT),VALUESN(MAXTDATE,MAXT)
	INTEGER DATESLAI(MAXTDATE),DATESN(MAXTDATE), IYEAR
	real jmaxn25a,vcmaxn25a, NMINJ, NMINV, jmaxn25b,vcmaxn25b
	real jmaxint,vcmaxint
C Inputs required for Growth model
	REAL SHOOT, ROOTa, DEADFOL, ROOTFr, ROOTCr, shootdaily, finerootdaily
	REAL CONSTLEAFNa, LAIMAXFIRSTa
	REAL SLAa, CFRAC, FDECAY, RDECAYa
	REAL ALLEAFa, ALROOTa, ALROOTFr, ALROOTCr, RESPFRAC, RESPFRACG
C Intermediate calculations
      REAL DIFZEN(MAXANG),DEXT(MAXANG),BEXTANG(MAXANG)
      REAL ZEN(KHRS),AZ(KHRS)
      REAL TU(MAXP),TD(MAXP),RELDF(MAXP)
      REAL DIFDN(MAXP,3),DIFUP(MAXP,3),SCLOST(MAXP,3)
      REAL BFLUX(MAXP,3),DFLUX(MAXP,3),SCATFX(MAXP,3)
C Titles of input files
      CHARACTER*80 MTITLE,VTITLE,BTITLE
C Hourly outputs
      REAL THRAB(KHRS),FCO2(KHRS),FRESPF(KHRS),FRESPFr(KHRS)
      REAL FRESPCR(KHRS),FH2OT(KHRS),GSCAN(KHRS)
      REAL ANNUALPStwo(9), THRABa(KHRS), FCO2a(KHRS)
 
      Integer year
C      REAL DAILYPS(2846)
      REAL PSa, PSb, PSc, PSd, PSe, PSf, PSg, PSh, PSi, PSj
	REAL PSk, PSl, PSm, PSn, PSo, PSp, PSq, PSr
	REAL theto
      REAL DAILYPSa(365), Todayslaia(365) 
      REAL DAILYRDT(365), DAILYFRESPFT(365)
      REAL DAILYFRESPFRT(365),DAILYFRESPCRT(365),NEWDMa(365) 
      REAL DAILYPARTa(365), DAILYH2OT(365) 
      REAL CRFRATIO,SCR,FRFRATIO,SFR, SFOL, nppstaronea, nppstartwoa
      REAL ANNUALPSa(23), NPPa, yearlypsa, newfoliage,folnitcratio, 
     &     dmnewa, ANNUALPARa(23)
      REAL dailySHOOT, dailyshootnew, dailyfineroot, dailyfinerootnew, 
     & dailycoarseroot(365)
     
      REAL alleafnew, sfolnew, alrootfrnew, sfrnew, resptot, resptotconv
      
      INTEGER DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa
      REAL LAIMAXa, laimaxnewa, nppstara
      REAL Nminerala, Kra, Nuptakea, yearlyfineroota
      REAL rhoa, Ncanopy, KONE, KTWO, FNRETa
      REAL yrlyfinerootnita, yrlyshootnita  
      REAL Nitupa, Nitfract, yrlyshtnita
      REAL NC_foliage, NC_fineroot
c      REAL laimaxnewb
      REAL SHOOTNEXTYRa, ROOTNEXTYRa, NPPTHISYRa, LUEa,NDIFF
      REAL SFRyrly, yrlyroota, leafnava, yearlyshootnita, yearlyrootnita

      REAL A, B, MID, NDIFFA, NDIFFB, NDIFFMID, INTLEN, EPSIL
      LOGICAL ZERO


C Set program flag
	VTITLE = 'BEWDY: Version Sep 2005'

C Open parameter file 
	CALL OPENBEWDYINPUTF(VTITLE,BTITLE)

C Open met data file (must be done after ISTART & IEND read)
      CALL READDATES(USTOREYI, ISTART, IEND, NSTEP)

      CALL OPENMETF(ISTART,IEND,CAK,PRESSK,SWMIN,SWMAX,
     &  DIFSKY,ALAT,TTIMD,DELTAT,
     &  MFLAG,METCOLS,NOMETCOLS,MTITLE,MSTART)

C Read in parameters
	CALL INPUTBEWDYPARS(JMAXN25a,IECOU,EAVJU,EDVJU,DELSJU,TVJUPU,TVJDNU,
     &  VCMAXN25a,EAVCU,EDVCU,DELSCU,NMINJ,NMINV,
     &  AJQU,ABSRPU,GSBG0U,GSBG1U,CICARAT,RD0,RDFR,RDCR,RDK,RDT,SLAa,
     &   EFFY,EXTKUS)
       
C Shoot and root are in kg DM m-2; SLA is in m2 kg-1 DM
	CALL INPUTBEWDYGROW(ROOTa,ROOTFr,ROOTCr,SHOOT,TODAYSNa,
     &	LAIMAXFIRSTa, SLAa,CFRAC,FDECAY,RDECAYa,ALLEAFa,ALROOTa,ALROOTFr,
     &  ALROOTCr,RESPFRAC,RESPFRACG)
	DEADFOL = 0.0

C Start met file
      CALL RESTARTMETF(ISTART,MSTART,MFLAG)

C Open output file 
	CALL OPENBEWDYOUTPUTF(MTITLE,VTITLE,BTITLE)
	      

C**********************************************************************
       
      CALL READSOLVEPARS(SCR,SFR,CRFRATIO,FRFRATIO,
     &  DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa)
     
      CALL READNITROGENUPTAKEPARS(KRa, NMINERALa,RHOa, KONE, KTWO,
     &  FNRETa)
       
C  Begin yearly loop
	 NPPTHISYRa = 0.0
	 SHOOTNEXTYRa = 0.0 
       ROOTNEXTYRa = 0.0
	 LAIMAXa = 0.0
	 NUPTAKEa = 0.0  
       yrlyshootnita = 0.0
	 yrlyrootnita = 0.0
       yearlypara = 0.0
	 LUEa = 0.0
	 NPPa = 0.0
	 yearlypsa = 0.0
	 IYEAR= 0

      DO 7 IYEAR= 1, 23
       ANNUALPSa(iyear) = 0.0
       ANNUALPARa(iyear) = 0.0
       
          IF(IYEAR.EQ.1) THEN 
          
          LAIMAXa= LAIMAXFIRSTa
          LEAFNAVa = TODAYSNa/LAIMAXa
          yrlyroota = ROOTa * 1000.
                 
          ELSE
           LAIMAXa = laimaxnewa
           LEAFNAVa = shootnita
           yrlyroota = rootnextyra

          END IF

c         write(77,*) leafnava, laimaxa
C Calculate leaf area (in m2 leaf m-2 grd) 
      CALL CALCULLAIa(LAIMAXa,DAYONEa,DAYTWOa,DAYTHREEa,DAYFOURa,
     & TODAYSLAIa)
      
      IF(IYEAR.EQ.1) THEN 
          write(71,'(a5)') 'LAI'          
          write(71,'(F12.7)') todayslaia
      end if
                        
       DO 110  IDAY = 1, 365
      
         DAILYPSa (IDAY) = 0.0
         DAILYRDT(IDAY)  = 0.0
         DAILYFRESPFT(IDAY) = 0.0
         DAILYFRESPFRT(IDAY) = 0.0
         DAILYFRESPCRT(IDAY) = 0.0
         DAILYPARTa(IDAY) = 0.0
         DAILYH2OT(IDAY)= 0.0

                
c        write(77,*) leafnav
C Calculate zenith angle of sun
      CALL SUN(IDAY+ISTART-1,ALAT,TTIMD,DEC,EQNTIM,DAYL,SUNSET)
      CALL ZENAZ(ALAT,TTIMD,BEAR,DEC,EQNTIM,ZEN,AZ)

C Get meteorological data
      CALL GETMET(IDAY+ISTART-1,MFLAG,ZEN,METCOLS,NOMETCOLS,
     &  CAK,PRESSK,SWMIN,SWMAX,DELTAT,ALAT,DEC,DAYL,
     &  windah,tsoil,tair,radabv,fbeam,RH,VPD,VMFD,CA,PRESS,
     &  PPT,SOILMD,SOILDATA)

C Zero hourly fluxes
      CALL ZEROHR(THRAB,FCO2,FRESPF,FRESPFr,FRESPCr,FH2OT,GSCAN)

C**********************************************************************
C Begin hourly loop
      DO 100 IHOUR = 1,KHRS

         THRABa(IHOUR)=0.0
	   FCO2a(IHOUR)=0.0
C Test to see if daylight hours or if any foliage
        IF ((ABS(ZEN(IHOUR)).LE.1.57).AND.
     &    (RADABV(IHOUR,1).GT.0.1).AND.(TODAYSLAIa(iday).GT.0.0)) THEN

          PARa = RADABV(IHOUR,1)*UMOLPERJ
          
        
C Get parameters of BEWDY model
		CALL BEWDYPARMSA(TAIR(IHOUR),RH(IHOUR),VPD(IHOUR), CA(IHOUR),
     &	  JMAXN25a,IECOU,EAVJU,EDVJU,DELSJU,TVJUPU,TVJDNU,
     &	  VCMAXN25a,EAVCU,EDVCU,DELSCU,AJQU,GSBG0U,GSBG1U,
     &	  CICARAT,BALPHAa,BLAMBDAJa,BLAMBDAVa)
c       write(77,*) TAIR(IHOUR), RH(IHOUR), CA(IHOUR), 
c    &    JMAXN25, VCMAXN25
       
C Skip photosynthesis calculations if parameters are < 0
		IF ((BALPHAa.GT.0.0).AND.(BLAMBDAJa.GT.0.0).AND.
     &    (BLAMBDAVa.GT.0.0)) THEN

C Call BEWDY model to calculate understorey photosynthesis
        CALL BEWDYa(PARa,FBEAM(IHOUR,1),BALPHAa,BLAMBDAJa,BLAMBDAVa,
     &  LEAFNAVa,NMINJ, NMINV,EXTKUS,ABSRPU,TODAYSLAIa(iday)
     &  ,APARUSa,PSUSa)
     
 
     
	  THRAB(IHOUR)=APARUSa
	  FCO2(IHOUR)=PSUSa
	  THRABa(IHOUR)=APARUSa
	  FCO2a(IHOUR)=PSUSa
	  
c        write (77,*) todayslaia
	  
       
C Stomatal conductance and transpiration - estimated at whole-clump level
        GSIPT = GSBG0U + GSBG1U*PSUS*RH(IHOUR)/CA(IHOUR)	! mol CO2 m-2 s-1
	  ETUS = GSIPT*1.6*VPD(IHOUR)/PRESS(IHOUR)*1E3		! mmol H2O m-2 s-1
        FH2OT(IHOUR) = ETUS

          END IF ! If parameters > 0
         
        END IF ! If day or night
C  Finished with the 1st species   

     

C Calculate respiration
       FRESPF(IHOUR) = RESP(RD0,TAIR(IHOUR),RDK,RDT,1.0,-100.0)  ! If resp s in units of per g mass
     &    * SHOOT 
       FRESPFr(IHOUR) = RESP(RDFR,TAIR(IHOUR),RDK,RDT,1.0,-100.0)  ! If resp s in units of per g mass
     &    * ROOTFr 
       FRESPCr(IHOUR) = RESP(RDCR,TAIR(IHOUR),RDK,RDT,1.0,-100.0)  ! If resp s in units of per g mass
     &    * ROOTCr 
     
      DAILYPSa(IDAY) = DAILYPSa(IDAY) + FCO2a(IHOUR)
      DAILYFRESPFT(IDAY) = DAILYFRESPFT(IDAY)+FRESPF(IHOUR)
	DAILYFRESPFRT(IDAY) = DAILYFRESPFRT(IDAY)+FRESPFR(IHOUR)
	DAILYFRESPCRT(IDAY) = DAILYFRESPCRT(IDAY)+FRESPCR(IHOUR)
	DAILYRDT(IDAY) = DAILYRDT(IDAY)+FRESPF(IHOUR)+FRESPFr(IHOUR)
     & + FRESPCr(IHOUR)
      DAILYPARTa(IDAY) = DAILYPARTa(IDAY) + THRABa(IHOUR)
      DAILYH2OT(IDAY) = DAILYH2OT(IDAY) + FH2OT(IHOUR)
      
C Output hourly totals
      CALL OUTPUTHRU(IDAY,IHOUR,THRAB,FCO2,FH2OT,FRESPF,
     & FRESPFr,FRESPCR)

C**********************************************************************
100   CONTINUE
c     End hourly loop

      DAILYPSa(IDAY) = DAILYPSa(IDAY)*SPERHR*1E-6*GCPERMOL*X_KG_AS_T	!umol s-1 to kgC d-1
      DAILYFRESPFT(IDAY) = DAILYFRESPFT(IDAY)*SPERHR*1E-6*
     & GCPERMOL*X_KG_AS_T
	DAILYFRESPFRT(IDAY) = DAILYFRESPFRT(IDAY)*SPERHR*1E-6*GCPERMOL*
     & X_KG_AS_T
	DAILYFRESPCRT(IDAY) = DAILYFRESPCRT(IDAY)*SPERHR*1E-6*GCPERMOL*
     & X_KG_AS_T
	DAILYRDT(IDAY) = DAILYRDT(IDAY)*SPERHR*1E-6*GCPERMOL*X_KG_AS_T
	DAILYPARTa(IDAY) = DAILYPARTa(IDAY)*SPERHR/UMOLPERJ*1E-6	!umol s-1 to MJ m-2 d-1
	DAILYH2OT(IDAY) = DAILYH2OT(IDAY)*SPERHR*1E-3				!mmol s-1 to mol d-1

     
       NEWDMa(IDAY) = (DAILYPSa(IDAY)/CFRAC)*RESPFRACG !kg DM m-2 d-1
       
       Psdaya = dailypsa(iday)
             
       DMnewa = psdaya/CFRAC*RESPFRACG   ! KG dm m-2 
       Shootdailya = dailyshoota*0.44
       finerootdailya = dailyfineroota*0.44
       
       

       ANNUALPSa(iyear) = ANNUALPSa(iyear) + DAILYPSa(IDAY)
       ANNUALPARa(iyear)= Annualpara(iyear) + dailyparta(iday)   ! MJ m-2 yr-1
c      write(77,*) iday, todayslai(iday), dailyps(iday)      
      
       
 110   CONTINUE
C      End daily loop


        call outputdyphotoa(dailypsa, todayslaia)
          yearlypsa = annualpsa(iyear)           ! kg C m-2 grnd yr-1
          yearlypara = annualpara(iyear)         ! MJ m-2 grd yr-1         
      
      
         NPPa = RESPFRACG*yearlypsa         ! kg C m-2 grnd yr-1  
         LUEa = NPPa*1000/yearlypara ! g C MJ-1

 

         yearlyshoota = ALLEAFa*NPPa/cfrac *1000.  ! New shoot biomass  g DM m-2 grnd
        yearlyfineroota  = yrlyroota + ALROOTa*NPPa/cfrac *1000. - 
     &    RDECAYa*yrlyroota      ! New root biomass  g DM m-2 grnd 

     
           
         Nuptakea = Nminerala*
     &  (1.0 - exp(-kra*yrlyroota*cfrac))! g N m-2 grnd yr-1
          
          SHOOTNEXTYRa = yearlyshoota ! next year shoot in g DM m-2 grnd
          ROOTNEXTYRa = yearlyfineroota ! next year root in g DM m-2 grnd
          NPPTHISYRa = NPPa * 1000. ! NPP in g C m-2 grnd at the end of the current yr
   
     
        
        Nitupa = Nuptakea
         yearlyshootnita = Nitupa /((1.0 - FNRETa)+ 
     &   rhoa*ROOTNEXTYRa*RDECAYa/SHOOTNEXTYRa)    !  Nitrogen in new shoot in the following year (g N m-2 grnd)

c        CALL ROOTFINDER(Nitupa,FNRETa,rhoa,ROOTNEXTYRa,
c     &  RDECAYa, SHOOTNEXTYRa, yearlyshootnita,NDIFF) 
              
c        yearlyshootnita = folnitcratio
        
        yrlyshootnita = yearlyshootnita/SHOOTNEXTYRa*1/cfrac   ! foliage N concentration ---g N g-1 C
        yrlyrootnita = rhoa*yrlyshootnita !  root N concentration ---g N g-1 C
        yearlyfinerootnita = yrlyrootnita*cfrac !  Nitrogen in new root in the following year (g N g-1 DM)  
        
               

        laimaxnewa = yearlyshoota/1000. * slaa
        shootnita = yearlyshootnita/laimaxnewa  ! average leaf nitrogen g N m-2 leaf
        
        
        NPPstaronea = Nminerala/(ALLEAFa+rhoa*ALROOTa)
     &   *1./yearlyshootnita
        NPPstartwoa = Kra*RDECAYa/ALROOTa
        NPPSTARa = (NPPstaronea - NPPstartwoa)*cfrac
        
        SHOOTTHISYRa = LAIMAXa/SLAa * 1000.
        FOLNCONTHISYRa = LEAFNAVa*SLAa/1000.*1./cfrac ! g N g-1 C
       

c         write (77,*) yrlyroota, nitupa, nuptakea
        write(77,*) yrlyshootnita,  yrlyrootnita, yearlypara,LUEa 
    
c      write (UYRLY, 600) IYEAR,  NPPTHISYRa, SHOOTNEXTYRa,      
        
      IF(IYEAR.EQ.1) THEN           
       write (78, '(a5,1X,a5,1X,a5,1X,a5,1X,a5,1X,a5,1X,a5,1X,a5, 
     &     1X,a5,1X,a5)') 'IYEAR',  'NPP', 
     & 'SHOOT', 
     &  'ROOT' , 'LAI' , 'NUP',  
     &  'YSHNIT',  'YRTNIT',
     &  'YPAR' , 'LUE'
        end if
      
       write (78, '(I7,1X,9(F12.7,1X))') IYEAR,  NPPTHISYRa, 
     &  SHOOTNEXTYRa, 
     &  ROOTNEXTYRa , LAIMAXa, NUPTAKEa,  
     &  yrlyshootnita,  yrlyrootnita,
     &  yearlypara,LUEa  
       
       OPEN (UYRLY, FILE = 'yearly.dat', STATUS = 'UNKNOWN')
       write (UYRLY, '(I7,1X,9(F12.7,1X))') IYEAR,  NPPTHISYRa, 
     &  SHOOTNEXTYRa, 
     &  ROOTNEXTYRa , LAIMAXa, NUPTAKEa,  
     &  yrlyshootnita,  yrlyrootnita,
     &  yearlypara,LUEa
 
  7    CONTINUE
       

       
C End yearly loop
     
      CLOSE(UMET)
	CLOSE(USTOREYI)
	CLOSE(UHRLY)
	close(udaily)
      close(uyrly)
      STOP
      END !MAIN
C**********************************************************************

