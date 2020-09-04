! $Header: m:/default/source/RCS/resd.f,v 1.572 2016/11/16 20:36:31 kja Exp $
! -------------------------------------------------------------------
!  NEMS Residential Demand Module (RDM)                             *
!                                                                   *
!  A component of the U.S. Energy Information Administration of the *
!  Department of Energy's National Energy Modeling System (NEMS)    *
!                                                                   *
!  LANGUAGE:      FORTRAN                                           *
!  CALLED BY:     PROGRAM NEMS (Integrating Module)                 *
!                                                                   *
!  ANALYSIS:      AEO2017                                           *
!  CASE:          Reference                                         *
!  DATE:          November 16, 2016                                 *
!  FILE NAME:     L:\mid\kja\AEO2017\Resd.f\resd_ref2017.f          *
!                                                                   *
!********************************************************************
!    AEO2017 CHANGES                                                *
!    -Keep lighting subsidies available regardless of CPP switch in *
!      order to accommodate historical energy efficiency rebates    *
!      (!lgtsubhist)                                                    *
!    -Updated call to calculate EE Costs to zero out costs in cost  *
!      calc rather than subroutine call so can call subroutine      *
!      in all cases, also read bldbase in all cases(!EEcosts)       *
!    -Add MEL modeling of wine coolers, including input from RSMELS,*
!      RSSTK, and RSUEC files  (!winecool)                          *
!    -Initialize variables read in from RGENTK for PV ZIP code model*
!    -Move initialization of DG accumulation variables above test   *
!      to use PV ZIP code model                                     *
!    -Adjust electricity bench factor by 1.1 in 2017 onward for     *
!      September STEO benchmarking (!STEObenchEL)                   *
!    -Adjust shares of PV generation deducted from space cooling    *
!      and other appliances (!PVshare)                              *
!    -Modify PV ZIP-code model to correct sales to grid rather than *
!      assume that all generation is for own use (!PVownuse)        *
!    -Calibrate ZIP-code PV penetration model to historical         *
!      exogenous PV capacity from RSGENTK (!PVzipcalib)             *
!    -Changed year for RSHTRSHR Census data read-in (!SOC update)   *
!    -Average bnchfct for post-STEO years based on last 5 historical*
!      data years (or less depending on RECS year) (! STEOread-avg) *
!    -Change HSHELL and CSHELL write-out placeholders for CDIV and  *
!      BLDG from 0 and 0 to 11 and 1 (respectively) for RESDBOUT    *
!    -Correct various spelling errors (xdegrAd) and in comments     *
!    -Remove 1.45 STEO benchmarking factor for 2016 on(!STEObenchNG)*
!********************************************************************
!  AEO2016 CHANGES                                                  *
!    -For February 2016 STEO benchmarking, NG back to MER factor    *
!      times 1.45 for 2016 on (!STEObenchNG)                        *
!    -Read STEO values from new steoblock include (! STEOread-avg)  *
!    -Changed year for RSHTRSHR Census data read-in   (!SOC update) *
!    -Add variables for end-use consumption including PV            *
!      self-gen for uldsm to build load shapes (!PVdispatch)        *
!    -Add initialization for iteration control for                  *
!                                           renewable DG (!111dren) *
!    -Add subsidy capability for renewable DG   (!111dren)          *
!    -Generalize lighting menu dollar costs instead of hard-code    *
!      requires dollar year at beginning of lighting menu           *
!      allows all tech menus to have different dollar year          *
!        (!lightmenu)                                               *
!    -Add read of dollar year for technology file (!rtek$)          *
!    -Set 80% limit on share of households that can have PV in      *
!      each zip code                                                *
!    -Write HSHELL and CSHELL factors to RESDBOUT.txt (not RESOUT)  *
!    -Remove AEO2015 natural gas 1.1 BNCHFCT tweak                  *
!    -Change SEDS benchmark code temporarily until defaulted;       *
!      commented out for STEO benchmarking 1/19/2016    (!bnchSEDS) *
!    -Updated calculation of solar PV generation based on           *
!      on PVWatts 5                                       (!PVgen)  *
!    -Removed RECS average solar PV generation constraint           *
!    -Updated output of end uses to four characters       (!eu)     *
!    -Added econometric PV penetration model option, coefficients,  *
!      zip code data and control switch are in rgentk.txt (!PVPen)  *
!    -Dynamically allocate several large arrays to cut common       *
!      block NEMS overhead                                (!DYN)    *
!********************************************************************
!  AEO2015 CHANGES                                                  *
!    -111(d) adjust lighting capital cost calc                      *
!    -111(d) test version, search 111(d)                            *
!       -Changed indirect cost multiplier to 1.5                    *
!    -Finalized September STEO Benchmarking                         *
!       -NG back to MER factor *1.1 for 2015 on                     *
!       -EL 2014 STEO factor throughout                             *
!    -Finalized August STEO Benchmarking                            *
!       -NG back to MER bench in 2015; EL STEO throughout           *
!    -Major end-use equipment tech update:                          *
!       -Refrigerator and freezer Shares                            *
!       -Tech Dollar Year = 2013$ per Navigant                      *
!    -Adjust new fan efficiency to implement furnace fan standard   *
!      effective 2019                                               *
!    -Adjust furnace fan consumption calculations to use new versus *
!      average UEC for fans replaced in the current model year      *
!    -Moved furnace fan weather adjustment from UEC calculation to  *
!      consumption calculation                                      *
!    -Adjusted furnace fan equipment count calculation to include   *
!      EQCREP(matches heating equipment count now)                  *
!    -Added light bulb type to database output file                 *
!    -Moved diagnostic print for PV technical potential inside of   *
!      Census division loop (no effect on any NEMS variables        *
!    -Changed lighting initialization to maintain foresight ability *
!    -Added residential housing starts (back) to the residential    *
!      database output file (no effect on any NEMS variables        *
!    -Eliminated several arrays that are no longer used             *
!********************************************************************
!  AEO2014 CHANGES                                                  *
!    -Added equipment and shell subsidy values to input files       *
!    -TECHG revised                                                 *
!       -KDEGDAY read-in                                            *
!       -ELOTPEN dimensions                                         *
!       -PCPEN, other RMELS dimensions                              *
!    -HVEQWTN avoided since it's only 2006-2009                     *
!    -BNCHFCT changed                                               *
!    -RSSQFT updated, read-in by BT then DIV                        *
!    -Merged to capture SW2's 1.506 lighting changes                *
!    -Reorganized the read-in of RSSTK and RSUEC files              *
!    -Removed saturation of TVs, PCs, etc in RSMELS                 *
!    -Changed read-in of RSSTEO housing starts                      *
!    -Now uses MAM starts throughout                                *
!    -Reorganized the read-in of RSMELS                             *
!    -Added network equipment, pool heaters and pumps               *
!    -Removed external power supplies                               *
!    -Changed year for RSHTRSHR read-in (C25 update)                *
!    -Finalized September STEO Benchmarking                         *
!    -Replaced old fossil fuel heat rate for renewable consumption  *
!      with parameter in resdrep STEO section                       *
!    -Increased the CRI-related penalty in lighting                 *
!    -Revised MELs variables to three-letter codes                  *
!    -Removed subroutines for TV and PC consumption                 *
!    -Moved TV and PC-related end uses to APCNS                     *
!    -Cleanup of residential database-related concepts              *
!    -Removed unusual benchmarking treatment                        *
!    -Revised stock of 2009 Electric Other Appliances               *
!    -Increased weather elasticity factors                          *
!    -Removed coal from consumption totals and BNCHFCT              *
!    -Revised benchmarking of kerosene                              *
!    -Added income effect to more MELs                              *
!    -Adjusted benchmarking for Table 31 only                       *
!    -Fixed benchmarking treatment for electric other               *
!    -Removed last vestiges of coal consumption                     *
!    -Added subsidy for shell and equipment for residential database*
!    -Added benchmarking bb factors to database heating             *
!********************************************************************

      module R_
      include'parametr'
      include'ncntrl'
      include'apq'
      include'rtek'     ! residential module common
      include'bldglrn'
      include'emmparm'
      include'emission'
      include'eusprc'   ! elastruns
      include'resdrep'
      include'emablk'
      include'macout'
      include'rscon'
      include'rseff'
      include'qsblk'
      include'cogen'
      include'uefpout'  ! electricity price for grid sales
      include'uecpout'  ! contains RPS credit price in 1987 mills/kWh - EPRPSPR(CURIYR)
      include'e111d'
      include'steoblock' ! common STEO inputs                         ! STEOread-avg
      !REAL*4 SAVE111RES(MNUMCR,MNUMYR)                ! Residential sector savings in billions of kilowatthours
      !REAL*4 COST111RES(MNUMCR,MNUMYR)                ! Residential costs 1987$ billions

! Local Parameters Set Here
      INTEGER NHeatClasses         ! Number of heating classes in RSCLASS
      PARAMETER (NHeatClasses=11)
      INTEGER NHeatTypes           ! Number of heating types in RSMEQP
      PARAMETER (NHeatTypes=31)
      INTEGER NCoolClasses         ! Number of cooling classes in RSCLASS
      PARAMETER (NCoolClasses=5)
      INTEGER NCoolTypes           ! Number of cooling classes in RSMEQP
      PARAMETER (NCoolTypes=14)
      INTEGER NShellTypes          ! Number of shells in RSMSHL
      PARAMETER (NShellTypes=5)
      INTEGER NationalPtr          ! Array location for aggregate of census divisions
      PARAMETER (NationalPtr=11)
! Parameters for Price-Induced Technical Change
!   These parameters allow first years of availability to be advanced when energy price
!   increases are large.  The idea is that the menu years are based on business as usual
!   and would not account for R&D in the event of large energy price increases.
!   Setting IFMAX = turns this feature off
      INTEGER   IFMAX         !   MAXIMUM FORWARD EFFECT
      PARAMETER (IFMAX=0)     !
! Parameters for Lighting Model Database and Arrays
      INTEGER NLRec,MaxApps, MaxTypes, MaxBins
      PARAMETER (NLRec=100)  !Number of lighting records in the technology database
      Parameter (MaxApps=4)  !Maximum number of applications
      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
      Parameter (MaxBins=6)  !Maximum number of hours per day usage bins per applications

!##########################################################################################
! 111(d)
      COMMON/BASE111D/BASELINEBKWH(MNUMCR,MNUMYR)
      COMMON/EFFDRIVER/DRIVER(MNUMYR,8,MNUMCR-2,MNUMBLDG),DRIVER2(MNUMYR,MNUMCR-2,MNUMBLDG)
      COMMON/RTEKDOLLARYR/RTEKDOLLARYR,RSHLDOLLARYR
      COMMON/EQCES/EQCESE(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/EQCRP/EQCRP90(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/EQCSR/EQCSR90(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/EQADD/EQCADD(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/EQREP/EQCREP(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/EQSUR/EQCSUR(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/RFCON/NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL, &
       FHTRCON(10),FCLCON(10),FWHCON(10),FSTVCON(10), &
       FDRYCON(10),FREFCON(10),FFRZCON(10), &
       FCSWCON(10),FDSWCON(10),NCSWFL,NDSWFL
      COMMON/LFE/  HDR(MNUMBLDG)
      COMMON/EXHS/ EH(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      COMMON/SAT/  RACSAT(MNUMBLDG,MNUMCR), &
                   RACUNTS(MNUMBLDG,MNUMCR), &
                   CACSAT(MNUMBLDG,MNUMCR), &
                   CACPR(MNUMCR), &
                   FRZSAT(MNUMBLDG,MNUMCR), &
                   ELDRYPR(MNUMBLDG,MNUMCR),&
                   REFSAT(MNUMBLDG,MNUMCR)
      COMMON/RENSH/RENSHR(MNUMCR)
      COMMON/SHTR/ SHTSHR(MNUMBLDG,MNUMCR,8),NSHTSHR(MNUMBLDG,MNUMCR,8)
      COMMON/SHELL/ &
       EHSHELL(RECSYEAR:ENDYR+1,MNUMFUEL,MNUMCR,MNUMBLDG), &
       ECSHELL(RECSYEAR:ENDYR+1,MNUMCR,MNUMBLDG), &
       NHSHELL(RECSYEAR:ENDYR+1,MNUMFUEL,MNUMCR,MNUMBLDG), &
       NCSHELL(RECSYEAR:ENDYR+1,MNUMCR,MNUMBLDG), &
       AHSHELL(RECSYEAR:ENDYR+1,MNUMFUEL,MNUMCR,MNUMBLDG), &
       ACSHELL(RECSYEAR:ENDYR+1,MNUMCR,MNUMBLDG), &
       TECHG(RECSYEAR:ENDYR+1,MNUMCR-2,MNUMBLDG),LIMIT
      COMMON/WTHRADJ/HDDYEAR(RECSYEAR:NUMHDDYR),CDDYEAR(RECSYEAR:NUMCDDYR)
      COMMON/NWDRYSAT/NEWDRYSAT(RECSYEAR+1:ENDYR,2,MNUMBLDG,MNUMCR-2)
      COMMON/ALLHOUSE/OLDHSES(RECSYEAR:ENDYR),NEWHSES(RECSYEAR:ENDYR)
      COMMON/INSCOST/RPINSCOST(MNUMRTCL,MNUMRTCL)
      COMMON/NEWMISC/DISHNEW(RECSYEAR+1:ENDYR,MNUMBLDG,MNUMCR-2),WASHNEW(RECSYEAR+1:ENDYR,MNUMBLDG,MNUMCR-2),&
       TVSPEN(RECSYEAR:ENDYR),TVSEFF(RECSYEAR:ENDYR),STBPEN(RECSYEAR:ENDYR),STBEFF(RECSYEAR:ENDYR),&
       HTSPEN(RECSYEAR:ENDYR),HTSEFF(RECSYEAR:ENDYR),DVDPEN(RECSYEAR:ENDYR),DVDEFF(RECSYEAR:ENDYR),&
       VGCPEN(RECSYEAR:ENDYR),VGCEFF(RECSYEAR:ENDYR),&
       DPCPEN(RECSYEAR:ENDYR),DPCEFF(RECSYEAR:ENDYR),LPCPEN(RECSYEAR:ENDYR),LPCEFF(RECSYEAR:ENDYR),&
       MONPEN(RECSYEAR:ENDYR),MONEFF(RECSYEAR:ENDYR),NETPEN(RECSYEAR:ENDYR),NETEFF(RECSYEAR:ENDYR),&
       BATPEN(RECSYEAR:ENDYR),BATEFF(RECSYEAR:ENDYR),CFNPEN(RECSYEAR:ENDYR),CFNEFF(RECSYEAR:ENDYR),&
       COFPEN(RECSYEAR:ENDYR),COFEFF(RECSYEAR:ENDYR),DEHPEN(RECSYEAR:ENDYR),DEHEFF(RECSYEAR:ENDYR),&
       MCOPEN(RECSYEAR:ENDYR),MCOEFF(RECSYEAR:ENDYR),PHPPEN(RECSYEAR:ENDYR),PHPEFF(RECSYEAR:ENDYR),&
       SECPEN(RECSYEAR:ENDYR),SECEFF(RECSYEAR:ENDYR),SPAPEN(RECSYEAR:ENDYR),SPAEFF(RECSYEAR:ENDYR),&
       WCLPEN(RECSYEAR:ENDYR),WCLEFF(RECSYEAR:ENDYR)    !winecool
      COMMON/APLSHARES/NEWHEATUEC(NHeatClasses,MNUMBLDG,MNUMCR-2),NEWFRIDGEUEC(MNUMBLDG,MNUMCR-2),NEWCOOLUEC(MNUMBLDG,MNUMCR-2),BASELOAD(16)
      COMMON/RETIRE/EQCRET(RECSYEAR:ENDYR,MNUMRTCL)
      COMMON/SQRFOOT/SQNEW(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),EXSQFTADJ(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,5)
      COMMON/SQRFLTS/ELASTIC(5,MNUMCR-2)
      COMMON/SQFTDATA/SQRFOOT(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),EXSQRFOOT(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),STOCKSQRFOOT(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      COMMON/PRI/PRICES(MNUMFUEL,MNUMCR,1990:ENDYR)
      COMMON/DRYER/DRYSHR(4,MNUMBLDG,MNUMCR)
      COMMON/DRYSA/DRYSAT(MNUMBLDG,MNUMCR)
      COMMON/HOTWATER/HOTWATQ(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2) &
        ,CWLOAD(RECSYEAR:ENDYR) &
        ,NCWLOAD(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),DWPR &
        ,ECWLOAD(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      COMMON/EFFIC/EQCEFF(RECSYEAR:ENDYR,MNUMRTCL)
      COMMON/STEFFIC/STKEFF(RECSYEAR:ENDYR,MNUMRTCL)
      COMMON/EUECS/EQCUEC(MNUMCR,MNUMRTCL,MNUMBLDG) &
        ,FANUEC(MNUMCR,MNUMBLDG) &
        ,TVSUEC(MNUMCR,MNUMBLDG),STBUEC(MNUMCR,MNUMBLDG),HTSUEC(MNUMCR,MNUMBLDG) &
        ,DVDUEC(MNUMCR,MNUMBLDG),VGCUEC(MNUMCR,MNUMBLDG),DPCUEC(MNUMCR,MNUMBLDG) &
        ,LPCUEC(MNUMCR,MNUMBLDG),MONUEC(MNUMCR,MNUMBLDG),NETUEC(MNUMCR,MNUMBLDG) &
        ,BATUEC(MNUMCR,MNUMBLDG),CFNUEC(MNUMCR,MNUMBLDG),COFUEC(MNUMCR,MNUMBLDG) &
        ,DEHUEC(MNUMCR,MNUMBLDG),MCOUEC(MNUMCR,MNUMBLDG),PHPUEC(MNUMCR,MNUMBLDG) &
        ,SECUEC(MNUMCR,MNUMBLDG),SPAUEC(MNUMCR,MNUMBLDG),WCLUEC(MNUMCR,MNUMBLDG) &    !winecool
        ,EAUEC(MNUMCR,MNUMBLDG),FANIUEC(MNUMCR,MNUMBLDG),SHTUEC(MNUMCR,7,MNUMBLDG),APPUEC(MNUMCR,3,MNUMBLDG)
      COMMON/EQCUEC/EQCNUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCAUEC(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCAHVUEC(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCRUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCSUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCHVUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCNIUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCRIUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCSIUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,EQCHVIUEC(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/SLC/SLCON(RECSYEAR-BASEYR+1:MNUMYR+1,MNUMCR),SLUEC(MNUMCR)
      COMMON/APLC/APLCON(RECSYEAR-BASEYR+1:MNUMYR,3,MNUMCR-2) !3 Fuels
      COMMON/RSFC/RSFLCN(RECSYEAR-BASEYR+1:MNUMYR,8,MNUMCR-1)
      COMMON/NHOUSES/NH(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2) &
        ,HSEADD(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2) &
        ,HHSTOCKBYDIV(RECSYEAR:ENDYR,MNUMCR-2)
      COMMON/ALLNEW/ALLNEW(RECSYEAR:ENDYR,MNUMCR-2)
      COMMON/LIFE/LFCY(MNUMRTTY,MNUMBLDG,MNUMCR,3)
      COMMON/EQCRP90/EQCRP90RP(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/OEQCRP/OEQCRP90(RECSYEAR:ENDYR,MNUMRTCL,1,MNUMCR)
      COMMON/OEQCRPR/OEQCRP90R(RECSYEAR:ENDYR,MNUMRTCL,1,MNUMCR)
      COMMON/OEQREP/OEQCREP(RECSYEAR:ENDYR,MNUMRTCL,1,MNUMCR)
      COMMON/SWITCH/EQCSW90(RECSYEAR:ENDYR,MNUMRTCL,MNUMRTCL,1,MNUMCR) &
        ,EQCSW90R(RECSYEAR:ENDYR,MNUMRTCL,MNUMRTCL,1,MNUMCR)
      COMMON/SW/SWITCHES(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,SWITCHESR(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,SWITCHTO(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,SWITCHTOR(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)&
        ,SWTOTAL(RECSYEAR:ENDYR,MNUMRTCL,MNUMCR-2) &
        ,SWFTOTAL(RECSYEAR:ENDYR,MNUMRTCL,MNUMCR-2)
      COMMON/NWHTR/HSYSSHR(RECSYEAR:ENDYR+1,NHeatClasses,MNUMBLDG,MNUMCR)
     !  Dynamically allocate these large arrays:                          !DYN
     ! COMMON/EQTSH/NEQTSHR(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR) &  !DYN
     !   ,REQTSHR(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR)              !DYN
          REAL*4,allocatable::NEQTSHR(:,:,:,:)                            !DYN
      REAL*4,allocatable::REQTSHR(:,:,:,:)                                !DYN
      COMMON/WEQCEF/ WTEQCEFFN(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,WTEQCEFFR(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,WTEQCEFFA(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,WTEQCEFFHV(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR) &
        ,WTEQCSQFHV(RECSYEAR:ENDYR+1,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/HURDLE/HRDRATE,ELIGBLE,ALPHA1,HRDADJ
      COMMON/DISCRATE/BETA1DR(MNUMRTTY)
      COMMON/EQCND/EQCND90(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)       !LOCAL
      COMMON/HEATOT/HEATOT(RECSYEAR:ENDYR+1,NHeatClasses,MNUMBLDG,MNUMCR)
!  Dynamically allocate these large arrays:                                         !DYN
!      COMMON/EQPFUT/ &                                                             !DYN
!        EQR90FUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2) &     !DYN
!       ,EQREPFUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2) &     !DYN
!       ,EQADDFUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2) &     !DYN
!       ,EQR90RPFUT(RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2) &     !DYN
!       ,EQCESEFUT (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2)       !DYN
      REAL*4,allocatable::EQR90FUT  (:,:,:,:,:)                                     !DYN
      REAL*4,allocatable::EQREPFUT  (:,:,:,:,:)                                     !DYN
      REAL*4,allocatable::EQADDFUT  (:,:,:,:,:)                                     !DYN
      REAL*4,allocatable::EQR90RPFUT(:,:,:,:,:)                                     !DYN
      REAL*4,allocatable::EQCESEFUT (:,:,:,:,:)                                     !DYN
      COMMON/EQCEQ/EQCEQCN(RECSYEAR-BASEYR:MNUMYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      COMMON/GOEQ/GEEQCN(RECSYEAR-BASEYR:MNUMYR,4,MNUMBLDG,MNUMCR),SLEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR)
      COMMON/FANEQ/FANEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2)
      COMMON/NH2O/NH2OSH(RECSYEAR:ENDYR+1,5,MNUMBLDG,MNUMCR)
      COMMON/NWCK/NCKSH(RECSYEAR:ENDYR+1,3,MNUMBLDG,MNUMCR)
      COMMON/LTEQ/LTEQCN(RECSYEAR-BASEYR:MNUMYR,4,MNUMBLDG,MNUMCR-2)
      COMMON/EAEQ/EAEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2)
      COMMON/OTUSES/TVSEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2)&
        ,STBEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),HTSEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,DVDEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),VGCEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,DPCEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),LPCEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,MONEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),NETEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,BATEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),CFNEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,COFEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),DEHEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,MCOEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),PHPEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,SECEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2),SPAEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2) &
        ,WCLEQCN(RECSYEAR-BASEYR:MNUMYR,1,MNUMBLDG,MNUMCR-2)                                                       !winecool
      COMMON/SHEQ/SHEQCN(RECSYEAR-BASEYR:MNUMYR,7,MNUMBLDG,MNUMCR-2)
      COMMON/APEQ/APEQCN(RECSYEAR-BASEYR:MNUMYR,3,MNUMBLDG,MNUMCR-2),APLEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,3)
      COMMON/HSESHRS/HSESHR(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),HSETOT(RECSYEAR:ENDYR)
      COMMON/RBENCH/BNCHFCT(RECSYEAR-BASEYR+1:MNUMYR,6,MNUMCR-2), BNCHFCTAVG(6,MNUMCR-2)    ! STEOread-avg
      COMMON/RSDgOut/Units(mnumyr,mnumcr,ntek), Cap(mnumyr,mnumcr,ntek), Trills(mnumyr,mnumcr,ntek) &
        ,TrillsOwnUse(mnumyr,mnumcr,ntek), GasUsage(mnumyr,mnumcr,ntek) &
        ,HwBtu(mnumyr,mnumcr,ntek), Invest(mnumyr,mnumcr,ntek)   &
        ,x111drensub(mnumyr,mnumcr,ntek),igencapcostyr   !111dren
      COMMON/COOLVAC/ACICOST(MNUMRTTY,RECSYEAR:ENDYR,MNUMCR-2) &
        ,ACEFF(MNUMRTTY,RECSYEAR:ENDYR,MNUMCR-2) &
        ,HTRCOST(RECSYEAR:ENDYR,MNUMCR-2)
      !  Dynamically allocate these large arrays:                                   !DYN
          ! COMMON/DBEFFOUT/RSNEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2),            !DYN
          !                 RSEEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2)             !DYN
        Real*4,allocatable::RSNEFDB1(:,:,:,:)                                       !DYN
        Real*4,allocatable::RSEEFDB1(:,:,:,:)                                       !DYN
      !  Dynamically allocate these large arrays:                                   !DYN
      !COMMON/SHELLEFF/ &                                                           !DYN
      !   HTSHELLEFFWT(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR-2) &     !DYN
      !  ,HTSHELLWT(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR-2) &        !DYN
      !  ,HSHELL(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR-2) &                     !DYN
      !  ,CSHELL(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR-2) &                     !DYN
      !  ,SHELLBUILDS(RECSYEAR:ENDYR,NHEATTYPES,NShellTypes,MNUMBLDG,MNUMCR-2) &      !DYN
      !  ,SHELLINVEST (RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR) &     !DYN
      !  ,SHELLSUBSIDY(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR) &     !DYN
      !  ,SHELLSUBSIDY111D(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR) & !DYN
      !  ,CLSHELLWT(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR-2) &                    !DYN
      !  ,SHLEVELH(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR-2)         !DYN

        REAL*4,allocatable::HTSHELLEFFWT(:,:,:,:,:)     !DYN
        REAL*4,allocatable::HTSHELLWT(:,:,:,:,:)        !DYN
        REAL*4,allocatable::HSHELL(:,:,:,:)             !DYN
        REAL*4,allocatable::CSHELL(:,:,:,:)             !DYN
        REAL*4,allocatable::SHELLBUILDS(:,:,:,:,:)      !DYN
        REAL*4,allocatable::SHELLINVEST (:,:,:,:,:)     !DYN
        REAL*4,allocatable::SHELLSUBSIDY(:,:,:,:,:)     !DYN
        REAL*4,allocatable::SHELLSUBSIDY111D(:,:,:,:,:) !DYN
        REAL*4,allocatable::CLSHELLWT(:,:,:,:)
        REAL*4,allocatable::SHLEVELH(:,:,:,:,:)

      COMMON/OTHEREQP/FANEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        EAEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),APPEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,3),&
        SHTEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,7),&
        TVSEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),STBEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        HTSEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),DVDEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        VGCEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        DPCEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),LPCEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        MONEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),NETEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        BATEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),CFNEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        COFEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),DEHEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        MCOEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),PHPEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        SECEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),SPAEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2),&
        WCLEQP(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)                                            !winecool
      COMMON/DISPINC/INCOME(MNUMCR-2,RECSYEAR:ENDYR) ! DISPOSABLE INCOME VARIABLE
      !  Dynamically allocate these large arrays:                                               !DYN
     ! COMMON/HVACEQPSHARE/HVEQSHR(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR-2),                !DYN
         ! HEATINGTYPEPURCH(RECSYEAR:ENDYR,MNUMRTTY,MNUMBLDG,MNUMCR-2,2),&                      !DYN
         ! NEQTSHRC(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR),LEARNFACT(MNUMBLDG,MNUMCR-2)     !DYN
        REAL*4,allocatable::HVEQSHR(:,:,:,:)             !DYN
        REAL*4,allocatable::HEATINGTYPEPURCH(:,:,:,:,:)  !DYN
        REAL*4,allocatable::NEQTSHRC(:,:,:,:)            !DYN
        REAL*4,allocatable::LEARNFACT(:,:)               !DYN

      COMMON/LTUEC/LTUEC(MaxApps,MNUMCR-2,MNUMBLDG),LTEQP(MaxApps,RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2), &
        LTNUEC(MaxApps,RECSYEAR:ENDYR,mnumcr-2,MNUMBLDG), LTNUECly(MaxApps,RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG), &
        LTCONWTly(MNUMYR,MNUMCR-2,MNUMBLDG),LTCONINly(MNUMYR,MNUMCR-2,MNUMBLDG), &  !these use year indexes
        LTCONly(RECSYEAR:ENDYR,MNUMCR-2)
      COMMON/STIMULUS/WTHRZTN(RECSYEAR:ENDYR,2,MNUMCR-2)
      !Common block for lighting variables !NLRec is the maximum number of lighting records in the rsmlgt.txt cost
      !   and performance section (see above for parameter setting)     !111(D) adding subsidy with division dimension and division to capital cost
      Common/NewLightingVars/BulbCost(NLRec),BulbSub(NLRec,mnumcr-2),LPW(NLRec),BulbWatts(NLRec),LifeHours(NLRec),BulbCRI(NLRec), &
         BaseWattsBulbs(MaxApps,MaxTypes),BaseWattBins(MaxApps,MaxBins),AnnualBulbCost(MaxApps,MaxTypes,MaxBins), &
         WattsCY(MaxTypes),Beta1,Beta2,AppBinHours(MaxApps,MaxBins), BulbBinLife(MaxApps,MaxBins), &
         BulbsPerHH(MaxApps,MNUMBLDG),BulbBinShares(MaxApps,MaxTypes,MaxBins),BinShares(MaxApps,MaxBins), &
         BulbBinEnergy(MaxApps,MaxTypes,MaxBins),CRIBulb(MaxApps),LTlbeta1, LTlbeta2, watts(MaxTypes), &
         LTlcap(MaxTypes,mnumcr-2,MaxBins),LTLsub(MaxTypes,mnumcr-2),LTlcapInvest(MaxTypes),LTLIFE(MaxTypes,MaxBins),LTBinShare(MaxApps,MaxBins), &
         NumApps,NumTypes(MaxApps),AppIndex(MaxApps),NumAppBins(MaxApps),FirstYear(NLRec),LastYear(NLRec),BulbDiv(nlrec),&
         AppID(MaxApps),LightingApp(NLRec),BulbType(NLRec),rlgtdollaryr
      COMMON/LTDATABASE/  LTinvest(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,2),  &
                          LTsubsidy(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,2), &  !111(d) add subsidy for reporting
                          LTREPbyAPP(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2),&
                          LTNEEDEDbyAPP(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2), &
                          WTLEFFbyAPP(MaxApps,RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2), &
                          appbulbname(MaxApps,MaxTypes),LTSTOCK(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins)
      Real*4 LTinvest,LTsubsidy,LTREPbyAPP,LTNEEDEDbyAPP,WTLEFFbyAPP,LTSTOCK
      Character*3 appbulbname

      Real*4 BulbCost, BulbSub, LPW, BulbWatts, LifeHours, BulbCRI,BaseWattsBulbs,BaseWattBins,AnnualBulbCost, &
           WattsCY,Beta1,Beta2,AppBinHours,BulbBinLife,BulbsPerHH,BulbBinShares,BinShares,BulbBinEnergy,CRIBulb, &
           LTlbeta1, LTlbeta2,watts, LTlcap, LTLsub, LTlife, LTBinShare
      Integer NumApps, NumTypes, appindex, NumAppBins, FirstYear, LastYear, BulbDiv
      Integer rlgtdollaryr         !lightmenu
      Character*3 LightingApp, BulbType, AppID
      REAL*4 LTUEC, LTNUEC, LTEQP, LTCONly, LTCONWTly, LTCONINly, LTNUECly
      REAL*4 BASELINEBKWH
      REAL*4 driver,driver2
      REAL*4 WTHRZTN
!      REAL*4 HVEQSHR,HEATINGTYPEPURCH,NEQTSHRC,LEARNFACT !DYN
      REAL*4 INCOME
      REAL*4 FANEQP,EAEQP,APPEQP,SHTEQP
      REAL*4 TVSEQP,STBEQP,HTSEQP,DVDEQP,VGCEQP
      REAL*4 DPCEQP,LPCEQP,MONEQP,NETEQP
      REAL*4 BATEQP,CFNEQP,COFEQP,DEHEQP,MCOEQP,PHPEQP,SECEQP,SPAEQP,WCLEQP    !winecool
      REAL*4 FANPEN,EAPEN,APPPEN,SHTPEN
      REAL*4 TVSPEN,STBPEN,HTSPEN,DVDPEN,VGCPEN
      REAL*4 DPCPEN,LPCPEN,MONPEN,NETPEN
      REAL*4 BATPEN,CFNPEN,COFPEN,DEHPEN,MCOPEN,PHPPEN,SECPEN,SPAPEN,WCLPEN    !winecool
      REAL*4 ACICOST,ACEFF,HTRCOST
      REAL*4 Units, Cap, Trills, TrillsOwnUse, GasUsage, HwBtu, Invest
      REAL*4 x111drensub             !111dren
      INTEGER igencapcostyr          !111dren
      REAL*4 BNCHFCT, BNCHFCTAVG    ! STEOread-avg
      REAL*4 HSESHR,HSETOT
      REAL*4 APEQCN,APLEQP
      REAL*4 SHEQCN
      REAL*4 TVSEQCN,STBEQCN,HTSEQCN,DVDEQCN,VGCEQCN
      REAL*4 DPCEQCN,LPCEQCN,MONEQCN,NETEQCN
      REAL*4 BATEQCN,CFNEQCN,COFEQCN,DEHEQCN,MCOEQCN,PHPEQCN,SECEQCN,SPAEQCN,WCLEQCN    !winecool
      REAL*4 LTEQCN
      REAL*4 NCKSH
      REAL*4 NH2OSH
      REAL*4 EQCEQCN
      REAL*4 GEEQCN,SLEQCN
      REAL*4 FANEQCN
!      REAL*4 EQR90FUT,EQREPFUT,EQADDFUT,EQR90RPFUT,EQCESEFUT  !DYN
      REAL*4 EQCRP90RP,EQCRET
      REAL*4 BETA1DR
      REAL*4 EQCND90
      REAL*4 HEATOT
      REAL*4 HRDRATE,ELIGBLE,ALPHA1,HRDADJ
      REAL*4 HSYSSHR  !,REQTSHR,NEQTSHR  !DYN
      REAL*4 WTEQCEFFN,WTEQCEFFR,WTEQCEFFA,WTEQCEFFHV,WTEQCSQFHV
      REAL*4 SWITCHES,SWITCHESR,SWITCHTO,SWITCHTOR,SWTOTAL,SWFTOTAL
      REAL*4 EQCSW90,EQCSW90R
      REAL*4 LFCY,OEQCRP90,OEQCREP,OEQCRP90R
      REAL*4 ALLNEW
      REAL*4 NH,HSEADD,HHSTOCKBYDIV
      REAL*4 SLCON, SLUEC,SHTSHR,NSHTSHR,APLCON,RSFLCN
      REAL*4 EQCUEC,EAUEC,FANUEC,FANIUEC,SHTUEC,APPUEC
      REAL*4 TVSUEC,STBUEC,HTSUEC,DVDUEC,VGCUEC
      REAL*4 DPCUEC,LPCUEC,MONUEC,NETUEC
      REAL*4 BATUEC,CFNUEC,COFUEC,DEHUEC,MCOUEC,PHPUEC,SECUEC,SPAUEC,WCLUEC    !winecool
      REAL*4 EQCNUEC,EQCAUEC,EQCRUEC,EQCSUEC,EQCHVUEC,EQCAHVUEC
      REAL*4 EQCNIUEC,EQCRIUEC,EQCSIUEC,EQCHVIUEC
      REAL*4 STKEFF
      REAL*4 EQCEFF
      REAL*4 HOTWATQ,CWLOAD,NCWLOAD,ECWLOAD,DWPR
      REAL*4 DRYSAT,DRYSHR
      REAL*4 PRICES !      PRICES 1=DIS 2=LPG 3=NG 4=EL 5=Ker 6=WOOD 7=COAL
      REAL*4 SQFTADJ,SQRFOOT,SQNEW,STOCKSQRFOOT,EXSQRFOOT,EXSQFTADJ,ELASTIC
      REAL*4 NEWHEATUEC,NEWFRIDGEUEC,NEWCOOLUEC,BASELOAD
!      REAL*4 HTSHELLEFFWT,HTSHELLWT,HSHELL,CSHELL,CLSHELLWT,SHLEVELH,SHELLBUILDS,shellinvest,shellsubsidy,SHELLSUBSIDY111D  !DYN
      REAL*4 TVSEFF,STBEFF,HTSEFF,DVDEFF,VGCEFF
      REAL*4 DPCEFF,LPCEFF,MONEFF,NETEFF
      REAL*4 BATEFF,CFNEFF,COFEFF,DEHEFF,MCOEFF,PHPEFF,SECEFF,SPAEFF,WCLEFF    !winecool
      REAL*4 DISHNEW,WASHNEW
      REAL*4 OLDHSES,NEWHSES
      INTEGER HDDYEAR,CDDYEAR,RPINSCOST
      REAL*4 HDR
      REAL*4 EH
      REAL*4 RACSAT, RACUNTS, CACSAT, CACPR, FRZSAT, ELDRYPR,REFSAT
      REAL*4 RENSHR
      REAL*4 NEWDRYSAT
      REAL*4 EHSHELL, ECSHELL, NHSHELL, NCSHELL, AHSHELL, ACSHELL, &
             TECHG, LIMIT,SHELLCH,CUMSHWTNUM
      INTEGER NHTRFL,NCLFL,NWHFL,NSTVFL,NDRYFL,NREFFL,NFRZFL, &
       FHTRCON,FCLCON,FWHCON,FSTVCON,FDRYCON,FREFCON,FFRZCON, &
       FCSWCON,FDSWCON,NCSWFL,NDSWFL
      REAL*4 EQCESE,EQCRP90,EQCSR90,EQCADD,EQCREP,EQCSUR
      REAL*4 DISRT,HORIZON,LEAPYR
      CHARACTER*30 TITLE
      INTEGER RSYR, PREVYR, EU, RTOVALUE,STEOBM,NRGBILL,NRGBILL07,STIMULUS,EPA111D
      external RTOVALUE
     ! INTEGER R(6)
      INTEGER RTEKDOLLARYR,rshldollaryr   !rtek$
      end module R_

!*******************************************************************
!     RESD SUBROUTINE
!*******************************************************************
      SUBROUTINE RESD
      use R_
      IMPLICIT NONE

      !This whole block is added to dynamically assign array dimensions once in a NEMS run !DYN
       IF(CURCALYR.NE.RECSYEAR.OR.CURITR.NE.1) GOTO 2 !First time through allocate arrays

       allocate(NEQTSHR(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR))
       allocate(REQTSHR(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR))
       allocate(EQR90FUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2))
       allocate(EQREPFUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2))
       allocate(EQADDFUT  (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2))
       allocate(EQR90RPFUT(RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2))
       allocate(EQCESEFUT (RECSYEAR:ENDYR,RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR-2))
       allocate(RSNEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2))
       allocate(RSEEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2))                        !DYN
       allocate(HTSHELLEFFWT(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR-2))
       allocate(HTSHELLWT(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR-2))
       allocate(HSHELL(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR-2))
       allocate(CSHELL(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR-2))
       allocate(SHELLBUILDS(RECSYEAR:ENDYR,NHEATTYPES,NShellTypes,MNUMBLDG,MNUMCR-2))
       allocate(SHELLINVEST (RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR))
       allocate(SHELLSUBSIDY(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR))
       allocate(SHELLSUBSIDY111D(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR))
       allocate(CLSHELLWT(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR-2))
       allocate(SHLEVELH(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR-2))
       allocate(HVEQSHR(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR-2))
       allocate(HEATINGTYPEPURCH(RECSYEAR:ENDYR,MNUMRTTY,MNUMBLDG,MNUMCR-2,2))
       allocate(NEQTSHRC(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR))
       allocate(LEARNFACT(MNUMBLDG,MNUMCR-2))

 ! initialize allocated arrays to zero; otherwise, you get what is in the assigned memory !DYN
       NEQTSHR=0.0
       REQTSHR=0.0
       EQR90FUT=0.0
       EQREPFUT=0.0
       EQADDFUT=0.0
       EQR90RPFUT=0.0
       EQCESEFUT=0.0
       RSNEFDB1=0.0
       RSEEFDB1=0.0
       HTSHELLEFFWT=0.0
       HTSHELLWT=0.0
       HSHELL=0.0
       CSHELL=0.0
       SHELLBUILDS=0.0
       SHELLINVEST=0.0
       SHELLSUBSIDY=0.0
       SHELLSUBSIDY111D=0.0
       CLSHELLWT=0.0
       SHLEVELH=0.0
       HVEQSHR=0.0
       HEATINGTYPEPURCH=0.0
       NEQTSHRC=0.0
       LEARNFACT=0.0

2     Continue
      ! End of dynamic array dimension assignments                                  !DYN

!      RTEKDOLLARYR=2013  !rtek$
      RSYR=CURIYR+(baseyr-1)
      PREVYR=CURIYR-1
      STEOBM = RTOVALUE("STEOBM  ",0)
      NRGBILL = RTOVALUE("NRGBILL  ",1)
      NRGBILL07 = RTOVALUE("NRG2007  ",1)
      STIMULUS = RTOVALUE("STIMULUS  ",1)
      EPA111D=RTOVALUE("EPA111D ",0)
!*******************************************************************
!     READ DATA THE FIRST YEAR AND FIRST ITERATION
!*******************************************************************
      IF  (CURCALYR.LT.RECSYEAR) RETURN
      IF  (CURCALYR.EQ.RECSYEAR.AND.CURITR.EQ.1) THEN
        CALL RTEKREAD
        CALL RTEKREAD1
        If(IFMAX.ne.0) CALL PITCINIT
        CALL RDSQFOOT
        CALL DEGDAYREAD
!        IF(EPA111D.EQ.1) CALL BLDBASEREAD - read in all cases to allow for incremental difference calculations
        CALL BLDBASEREAD
        CALL RSUECSHLREAD
        CALL RSMELSREAD
        CALL RSSWITCHREAD
        CALL RSMISCREAD
        CALL RSMLGTREAD

        CALL RDRET
        CALL INTEQT
        CALL RDHTREQC
        CALL RDEFF
        CALL RDSTEFF
        CALL RDESTARHOMES
        CALL RDUECS
        CALL RCONSFL
        CALL RDISTGEN
      END IF

!*******************************************************************
!     STORE PRICES FOR EACH YEAR
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR) CALL RDPR

!*******************************************************************
!     BEGIN CALLING OTHER SUBROUTINES
!*******************************************************************
      IF (CURCALYR.EQ.RECSYEAR) THEN
         CALL EXCONS
         CALL RSBENCH
         CALL NEMSCN
         CALL RESDRP
      ELSE
        IF (MOD(CURCALYR,4).EQ.0.AND.CURCALYR.LT.2015) THEN
          LEAPYR= 366.0/365.0
         ELSE
          LEAPYR= 365.0/365.0
        END IF
         IF(IFMAX.ne.0) CALL RSPITC(IFMAX, LASTSTEOYR)
         CALL NEWHSE

!*******************************************************************
!     HEATING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL EPACTWD          ! EPACT WINDOW LABELING
         CALL SQFTCALC         ! AVG SQFT OF HOUSING, AND SQFT ADJUSTS for EUs
         CALL RSHVAC
         CALL RHTRTEC
         CALL RHTRADD

!*******************************************************************
!    DISTRIBUTED GENERATION SUBROUTINE
!*******************************************************************
         CALL RDISTGEN

!*******************************************************************
!     COOLING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RCLTEC
         CALL RCLADD

!*******************************************************************
!    CLOTHES WASHER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RCWTEC
         CALL RCWADD
!
!*******************************************************************
!     DISH WASHER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RDWTEC
         CALL RDWADD
!
!*******************************************************************
!     WATER HEATING EQUIPMENT SUBROUTINES
!*******************************************************************
         EU = 5              ! EU = 5 IS WATER HEATERS
         CALL RWHTEC
         CALL REUADD
!
!*******************************************************************
!     COOKING EQUIPMENT SUBROUTINES
!*******************************************************************
         EU = 6              ! EU = 6 IS STOVES (COOK)
         CALL RSTVTEC
         CALL REUADD
!
!*******************************************************************
!     DRYING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RDRYTEC
         CALL RDRYADD
!
!*******************************************************************
!     REFRIGERATING EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RREFTEC
         CALL RREFADD
!
!*******************************************************************
!     FREEZER EQUIPMENT SUBROUTINES
!*******************************************************************
         CALL RFRZTEC
         CALL RFRZADD
!*******************************************************************
!     CONSUMPTION SUBROUTINES
!*******************************************************************
         CALL RHTRCON
         CALL RCLCON
         CALL RCWCON
         CALL RDWCON
         CALL RWHCON
         CALL RSTOVCON
         CALL RDRYCON
         CALL RREFCON
         CALL RFRZCON

!*******************************************************************
!     LIGHTING, SECONDARY HEATING, & SMALL APPLIANCE SUBROUTINES
!*******************************************************************
         CALL LTCNS
         CALL APCNS
         CALL SHTCNS
         CALL APPCNS
!*******************************************************************
!     NEMS CONSUMPTION SUBROUTINE
!*******************************************************************
         CALL FUELCN
         CALL RSBENCH
         CALL NEMSCN
         CALL RESDRP
      !IF(EPA111D.EQ.1) CALL CALC111D      !111(D)
         CALL CALC111D      !EEcosts
       ENDIF  !check curcalyr >= recsyear
!*******************************************************************
!     REPORTING SUBROUTINES
!*******************************************************************
      IF ((CURCALYR-(baseyr-1).EQ.LASTYR).AND.(FCRL.EQ.1)) THEN
         CALL NHTSHR
         CALL RESDRP2
         CALL RESDBOUT
       ENDIF
      CONTAINS


!****************************************************************
!                                                               *
!   RTEKREAD READS THE RESIDENTIAL MODULE TECHNOLOGY DATABASES  *
!     RSCLASS                                                   *
!     RSMEQP                                                    *
!                                                               *
!****************************************************************
      SUBROUTINE RTEKREAD
      IMPLICIT NONE
!
      INTEGER FILE_MGR       ! FILE MANAGER
      INTEGER*4  INFILE,      & ! FILE HANDLE
       IOS,                   & ! READ ERR NUMBER
       I, J, K, L,Type,Y,D,   & ! GENERAL INDICES
       LASTEU, LASTCLAS, LASTTYPE
!
      DO I=1,MNUMENDU
         RTCLEUPT(I) =0
         RTTYEUPT(I) =0
         RTTYPECT(I) =0
      ENDDO
!
      LASTEU  =-1
      LASTCLAS=-1
      LASTTYPE=-1
      RTCLCNT = 0
      RTTYCNT = 0
      RTEUCNT = 0
!
!   OPEN AND READ THE RESD TECHNOLOGY FILE WITH EQUIPMENT CLASS DATA
!
      INFILE=FILE_MGR('O','RSCLASS',.FALSE.)!OPEN THE RSCLASS DATA SET
      READ(INFILE,'(19(/))')               !SKIP 20 LINE HEADER
!
      DO 40 I=1,MNUMRTCL
        READ(INFILE,*,ERR=50,END=55,IOSTAT=IOS) &
             RTCLENDU(I), RTCLEQCL(I), RTCLTYPT(I), RTCLPNTR(I), &
             RTCLREPL(I), RTFUEL(I)  , &
             RTMAJORF(I), RTFFAN(I), RTBASEFF(RECSYEAR,I), RTALPHA(I), &
             RTMINLIF(I), RTMAXLIF(I), RTK(I), RTLAMBDA(I), RTFCBETA(I), &
             RTSWFACT(I), RTSWBETA(I), RTSWBIAS(I), RTCLNAME(I)
!        rtswfact(i)=0. !this turns off switching for a quick test of how much actually goes on.

!
        RTCLCNT=RTCLCNT+1
        J=RTCLENDU(I)
        IF(J.NE.LASTEU) RTEUCNT=RTEUCNT+1
!
!   COLLECT THE RAW DATA TO COMPUTE THE RSCLASS END USE POINTERS
!
        RTCLEUPT(J+1)=RTCLEUPT(J+1)+1
        LASTEU=J
 40    CONTINUE
!
!   PRINT ERR MESSAGE: VALUE OF MNUMRTCL NEEDS TO BE INCREASED
!
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD ERR: Increase value of MNUMRTCL in ', &
         '&PRJ.NEMS.COMMON.PDS.Dmmddyyv:RTEK so that arrays from ', &
         'file RSCLASS will be completely read.'
      RETURN
!
!   PRINT READ ERR MESSAGE AND RETURN
!
 50    CONTINUE
      INFILE=FILE_MGR('C','RSCLASS',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD file RSCLASS read error number ',IOS
      RETURN
!
!   IF THE READ WAS SUCESSFUL, PRINT SUMMARY INFORMATION
!
 55    CONTINUE
!
!   FIRST, CLOSE THE FILE AND ASSIGN THE POINTERS
!
      INFILE=FILE_MGR('C','RSCLASS',.FALSE.)
!
!   USE RAW DATA TO ASSIGN POINTERS
!
      DO J=1,RTEUCNT
        RTCLEUPT(J+1)=RTCLEUPT(J+1)+RTCLEUPT(J)
      ENDDO
!
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSCLASS EOF REACHED OK; COUNT = ',RTCLCNT
!
!   OPEN AND READ THE RESD TECHNOLOGY FILE WITH EQUIPMENT TYPE DATA
!
      INFILE=FILE_MGR('O','RSMEQP',.FALSE.)!OPEN THE RSMEQP DATA SET
      READ(INFILE,'(19(/))')               !SKIP 20 LINE HEADER
      READ(INFILE,*,ERR=100,END=105,IOSTAT=IOS) rtekdollaryr      !rtek$
      READ(INFILE,'(3(/))')               !SKIP 4 LINE HEADER

      LASTEU=-1

      DO 90 I=1,MNUMRTTY
        READ(INFILE,*,ERR=100,END=105,IOSTAT=IOS) &
                   RTTYENDU(I), RTTYEQCL(I), RTEQTYPE(I), RTINITYR(I), &
                   RTLASTYR(I), RTCENDIV(I), HVACPNTR(I), RTTYPNTR(I), CWMEF(I), &
                   LOADADJ(I),  RTEQEFF(I),  RTEQCOST(I), RTRECOST(I), &
                   RTEQSUB(I),  RTRESUB(I),  RTEQSUB111D(I), RTRESUB111D(I),&  !Subsidies for replacement equip, new equip, repl equip 111d programs, new equip 111d programs
                   RTMATURE(I), RTCOSTP1(I), RTCOSTP2(I), &
                   RTCOSTP3(I), RTECBTA1(I), RTECBTA2(I), &
                   RTECBTA3(I), RTECBIAS(I), RTTYNAME(I)

!     IMMEDIATELY SUBTRACT SUBSIDY VALUE FROM EQUIPMENT COSTS
        RTEQCOST(I)=RTEQCOST(I)-RTEQSUB(I)-FLOAT(EPA111D)*RTEQSUB111D(I)
        RTRECOST(I)=RTRECOST(I)-RTRESUB(I)-FLOAT(EPA111D)*RTRESUB111D(I)

!
!     CREATE VARIALBLES FOR HVAC SUBROUTINE
!
!
        DO Y=RECSYEAR,ENDYR
!        HVACPNTR counts equipment types across heating and cooling.
!        Note the first cooling type is numbered 32
!          for heating HVACPNTR == RTEQTYPE
!          for cooling HVACPNTR == RTEQTYPE + 31 current number of heating system types)
         IF (HVACPNTR(I).GT.NHeatTypes) THEN
          Type=HVACPNTR(I)-NHeatTypes
          D=RTCENDIV(I)
          IF (Y.GE.RTINITYR(I).AND.Y.LE.RTLASTYR(I)) THEN
            ACEFF(Type,Y,D)=RTEQEFF(I)
            ACICOST(Type,Y,D)=RTEQCOST(I)
          END IF
         END IF
        END DO
!
        RTTYCNT=RTTYCNT+1
        J=RTTYENDU(I)
!
!   COLLECT THE RAW DATA TO COMPUTE THE RSMEQP END USE POINTERS
!
        RTTYEUPT(J+1)=RTTYEUPT(J+1)+1
        K=RTEQTYPE(I)
        IF(J.NE.LASTEU.OR.K.NE.LASTTYPE)RTTYPECT(J+1)=RTTYPECT(J+1)+1
        LASTEU=J
        LASTTYPE=K
!
 90    CONTINUE
!
!   PRINT ERR MESSAGE: VALUE OF MNUMRTTY NEEDS TO BE INCREASED
!
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD ERR: Increase value of MNUMRTTY in ', &
         '&PRJ.NEMS.COMMON.PDS.Dmmddyyv:RTEK so that arrays from ', &
         'file RSMEQP will be completely read.'
      RETURN
!
!   PRINT READ ERR MESSAGE AND RETURN
!
 100   CONTINUE
      INFILE=FILE_MGR('C','RSMEQP',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSMEQP read error number ',IOS
      RETURN
!
!   IF THE READ WAS SUCESSFUL, PRINT SUMMARY INFORMATION
!
 105   CONTINUE
!
!   FIRST, CLOSE FILE AND USE RAW DATA TO ASSIGN POINTERS
!
      INFILE=FILE_MGR('C','RSMEQP',.FALSE.)
      DO J=1,RTEUCNT
        RTTYEUPT(J+1)=RTTYEUPT(J+1)+RTTYEUPT(J)
        RTTYPECT(J+1)=RTTYPECT(J+1)+RTTYPECT(J)
      ENDDO

      WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSMEQP EOF REACHED OK; COUNT = ',RTTYCNT
!
      RETURN
      END SUBROUTINE RTEKREAD
!
!************************************************************************
!                                                                       *
!   RTEKREAD1 READS THE RESIDENTIAL MODULE SHELL TECHNOLOGY DATABASE    *
!                                                                       *
!************************************************************************

      SUBROUTINE RTEKREAD1
      IMPLICIT NONE
!      REAL*4 SHELLINSTALLCOST(NHeatClasses,9,3,NShellTypes)
      INTEGER FILE_MGR       ! FILE MANAGER
      INTEGER*4  INFILE,      & ! FILE HANDLE
       IOS,                   & ! READ ERR NUMBER
       I,R,T,B,HVTYCNT          ! GENERAL INDICES


!   OPEN AND READ THE RESD HVAC TECHNOLOGY FILE WITH EQUIPMENT TYPE DATA

      INFILE=FILE_MGR('O','RSMSHL',.FALSE.)!OPEN THE RSMSHL DATA SET
      READ(INFILE,'(19(/))')               !SKIP 20 LINE HEADER
      READ(INFILE,*,ERR=100,END=105,IOSTAT=IOS) rshldollaryr      !rtek$
      READ(INFILE,'(3(/))')               !SKIP 4 LINE HEADER

      HVACCNT = 0
      DO 90 I=1,MNUMHVAC
        READ(INFILE,*,ERR=100,END=105,IOSTAT=IOS) &
         RSCENDIV(I),RSBTYPE(I),HVHTEQCL(I),&
         HVHTEQTY(I),HVCLEQCL(I),&
         HVCLEQTY(I),HVFYEAR(I),&
         HVLYEAR(I),HVHEATFACTOR(I),&
         HVCOOLFACTOR(I),HTSHEFF(I),  &
         CLSHEFF(I),HTSHBASE(I),CLSHBASE(I), &
         SHELCOST(I),SHELSUB(I),SHELSUB111D(I),  &
         HVBETA1(I),HVBETA2(I) , &
         HVPACKG(I),HVPGNAME(I)
        HVACCNT=HVACCNT+1

!   CONVERT TO RTEKDOLLARYR (IF DIFFERENT)
        SHELCOST(I)    =SHELCOST(I)    *MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(rshldollaryr-BASEYR+1) !rtek$
        SHELSUB(I)     =SHELSUB(I)     *MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(rshldollaryr-BASEYR+1) !rtek$
        SHELSUB111D(I) =SHELSUB111D(I) *MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(rshldollaryr-BASEYR+1) !rtek$
!   IMMEDIATELY SUBTRACT SUBSIDY AMOUNT FROM SHELL COST
        SHELCOST(I)=(SHELCOST(I)-SHELSUB(I)-SHELSUB111D(I)*FLOAT(EPA111D))

!!!PH: adjust the heating and cooling shell efficiencies
        HTSHEFF(I) = HTSHEFF(I) * 0.4
        CLSHEFF(I) = CLSHEFF(I) * 0.4

 90    CONTINUE

!   PRINT ERR MESSAGE: VALUE OF MNUMRTTY NEEDS TO BE INCREASED

      WRITE(6,*) 'RESDMSG SUB_RTEKREAD ERR: Increase value of MNUMHVAC in ', &
         '&PRJ.NEMS.COMMON.PDS.Dmmddyyv:RTEK so that arrays from ', &
         'file RSHVAC will be completely read.'
      RETURN

!   PRINT READ ERR MESSAGE AND RETURN
 100   CONTINUE
      INFILE=FILE_MGR('C','RSMSHL',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RTEKREAD RSMSHL read error number ',IOS
      RETURN
!
!   IF THE READ WAS SUCESSFUL, PRINT SUMMARY INFORMATION
!
 105   CONTINUE
!
!   FIRST, CLOSE FILE AND USE RAW DATA TO ASSIGN POINTERS
!
      INFILE=FILE_MGR('C','RSMSHL',.FALSE.)

      WRITE(6,*) 'RESDMSG SUB_RTEKREAD1 RSMSHL EOF REACHED OK; COUNT = ',HVACCNT
!
      RETURN
      END SUBROUTINE RTEKREAD1

!********************************************************************
!   READ DEGREE DAY DATA
!     KDEGDAY
!********************************************************************
      SUBROUTINE DEGDAYREAD
      IMPLICIT NONE
!
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          YEAR,DIV,D,Y              ! GENERAL INDICES

!   OPEN AND READ THE DATA FILE
      INFILE=FILE_MGR('O','KDEGDAY',.FALSE.) ! OPEN THE DEGDAY DATA SET


      READ(INFILE,'(137(/))')                ! SKIP 100 LINE HEADER PER CDM CONVENTION + SKIP YEARS BEFORE RECS YEAR

      DO YEAR=RECSYEAR,IJUMPCALYR

        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) HDDYEAR(YEAR), (HDDADJ(YEAR,D),D=1,MNUMCR-2)
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) CDDYEAR(YEAR), (CDDADJ(YEAR,D),D=1,MNUMCR-2)

        ! create national population-weighted degree days
        HDDADJ(YEAR,NationalPtr) = 0.0
        CDDADJ(YEAR,NationalPtr) = 0.0
        DO D=1,MNUMCR-2
           HDDADJ(YEAR,NationalPtr) = HDDADJ(YEAR,NationalPtr) + HDDADJ(YEAR,D) * MC_NP(D,YEAR-(baseyr-1))/MC_NP(NationalPtr,YEAR-(baseyr-1))
           CDDADJ(YEAR,NationalPtr) = CDDADJ(YEAR,NationalPtr) + CDDADJ(YEAR,D) * MC_NP(D,YEAR-(baseyr-1))/MC_NP(NationalPtr,YEAR-(baseyr-1))
        ENDDO
      END DO  ! YEAR

!   File successfully read, close file and return
      WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD: KDEGDAY data set read successfully'
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      RETURN !successful

!********************************************************************
!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
10    CONTINUE
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_DEGDAYREAD read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
 95   CONTINUE
      INFILE=FILE_MGR('C','KDEGDAY',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_DEGDAYREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_DEGDAYREAD: Correct KDEGDAY and resubmit job.'
      RETURN
      END SUBROUTINE DEGDAYREAD

!********************************************************************
!   READ BASELINEKWH FOR EPA111D ANALYSIS
!********************************************************************
      SUBROUTINE BLDBASEREAD
      IMPLICIT NONE
!
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          YEAR,DIV,D,Y              ! GENERAL INDICES
      REAL TEMP                     !PLACEHOLDER TO PULL IN QELCM AND DISCARD


!   OPEN AND READ THE DATA FILE
      INFILE=FILE_MGR('O','BLDBASE',.FALSE.) ! OPEN THE DEGDAY DATA SET

      READ(INFILE,'(99(/))')                 ! SKIP 100 LINE HEADER PER CDM CONVENTION + SKIP YEARS BEFORE RECS YEAR

      WRITE (9,*) 'division, divcheck, year, yearcheck, baseline trills converted to bkWh'

          DO D=1,mnumcr-2
           DO y=1,mnumyr
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) Div, Year, baselinebkwh(d,y), TEMP ! upon read, these data re in trills
            BASELINEBKWH(d,y)=(BASELINEBKWH(d,y)/3412.)*10**3  !convert trills to bkWh
            WRITE(9,5) D,Div,Y,Year,BASELINEBKWH(D,Y)
           ENDDO
          ENDDO
 5    format(' BASELINE CHECK',4I5,F12.5)


!   File successfully read, close file and return
      WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD: BASELINEKWH data set read successfully'
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      RETURN !successful

!********************************************************************
!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
10    CONTINUE
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_BLDBASEREAD read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
 95   CONTINUE
      INFILE=FILE_MGR('C','BLDBASE',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_BLDBASEREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_BLDBASEREAD: Correct BLDBASE and resubmit job.'
      RETURN
      END SUBROUTINE BLDBASEREAD

!********************************************************************
!   READ FUEL SWITCHING DATA
!     RSSWITCH
!********************************************************************
      SUBROUTINE RSSWITCHREAD
      IMPLICIT NONE


      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          B,D,F,E,                & ! GENERAL INDICES
          NUMCL,RECCL,RECCLSW

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSSWITCH',.FALSE.) ! OPEN THE RSSWITCH DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

      DO B=1,MNUMBLDG
        DO E=1,NHeatClasses
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(RTFCBIAS(E,B,D),D=1,MNUMCR-2)
        ENDDO
      ENDDO

!   READ IN MATRIX OF COSTS INVOLVED WITH SWITCHING TECHNOLOGIES ON REPLACEMENT
!     READ RTCLCNT-2 RECORDS BECAUSE NO SWITCHING FOR FOOD REFRIGERTION OR FREEZING

      READ(INFILE,'(3(/))')                ! SKIP 4 LINE HEADER

      DO RECCL = 1,RTCLCNT-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) &
       (RPINSCOST(RECCLSW,RECCL),RECCLSW=1,RTCLCNT-2)
      ENDDO

      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSSWITCHREAD: RSSWITCH data set read successfully'
      RETURN

!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
 10   CONTINUE
      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSSWITCHREAD: RSSWITCH read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
 95   CONTINUE
      INFILE=FILE_MGR('C','RSSWITCH',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSSWITCHREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSSWITCHREAD: Correct RSSWITCH and resubmit job.'
      RETURN

      END SUBROUTINE RSSWITCHREAD


!********************************************************************
!   READ NEW AND EXISTING HOME SHELL UEC DATA
!     RSUECSHL
!********************************************************************
      SUBROUTINE RSUECSHLREAD
      IMPLICIT NONE


      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          D,E,B,F                   ! GENERAL INDICES

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSUECSHL',.FALSE.) ! OPEN THE RSUECSHL DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER


      DO D=1,MNUMCR-2
        DO E=1,NHeatClasses
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWHEATUEC(E,B,D),B=1,MNUMBLDG)
        ENDDO
      ENDDO

      DO D=1,MNUMCR-2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWFRIDGEUEC(B,D),B=1,MNUMBLDG)
      ENDDO

      DO D=1,MNUMCR-2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWCOOLUEC(B,D),B=1,MNUMBLDG)
      ENDDO

      DO E=1,16
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) BASELOAD(E)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LEARNFACT(B,D),B=1,MNUMBLDG)
      ENDDO

      DO B=1,MNUMBLDG
       DO D=1,MNUMCR-2
        DO F=1,MNUMFUEL-2
         EHSHELL(RECSYEAR,F,D,B)=1.0
        ENDDO
       ENDDO
      ENDDO

      DO D=1,MNUMCR-2
       DO B=1,MNUMBLDG
        ECSHELL(RECSYEAR,D,B)=1.0
       ENDDO
      ENDDO

      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL data set read successfully'
      RETURN

!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
 10   CONTINUE
      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSUECSHLREAD: RSUECSHL read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
 95   CONTINUE
      INFILE=FILE_MGR('C','RSUECSHL',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSUECSHLREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSUECSHLREAD: Correct RSUECSHL and resubmit job.'
      RETURN

      END SUBROUTINE RSUECSHLREAD

!********************************************************************
!   READ MISCELLANEOUS ELECTRIC LOAD DATA
!     RSMELS
!********************************************************************
      SUBROUTINE RSMELSREAD
      IMPLICIT NONE


      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L    ! GENERAL INDICES

!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSMELS',.FALSE.) ! OPEN THE RMELS DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER


!      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PCPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
!      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WTPCEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
!   Read in "Televisions and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DVDPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
!   Read in "Computers and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
!   Read in other specified miscellaneous electric loads
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHPPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAPEN(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLPEN(Y), Y=RECSYEAR+1,ijumpcalyr)    !winecool

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

!   Read in "Televisions and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(TVSEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(STBEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HTSEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DVDEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(VGCEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
!   Read in "Computers and related equipment"
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DPCEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(LPCEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MONEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NETEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
!   Read in other specified miscellaneous electric loads
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(BATEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CFNEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(COFEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(DEHEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(MCOEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(PHPEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SECEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(SPAEFF(Y), Y=RECSYEAR+1,ijumpcalyr)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(WCLEFF(Y), Y=RECSYEAR+1,ijumpcalyr)    !winecool

      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS data set read successfully'
      RETURN

!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
 10   CONTINUE
      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMELSREAD: RSMELS read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
 95   CONTINUE
      INFILE=FILE_MGR('C','RSMELS',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMELSREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMELSREAD: Correct RSMELS and resubmit job.'
      RETURN

      END SUBROUTINE RSMELSREAD

!********************************************************************
!   READ MISC RESIDENTIAL DATA
!     RSMISC
!********************************************************************
      SUBROUTINE RSMISCREAD
      IMPLICIT NONE
!
!
!   DECLARE LOCAL VARIABLES
!
      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          B,D,F,Y,YEAR,EQC,E,V,L, & ! GENERAL INDICES
          DUMMY,NUMCL,EU,RECCL,RECCLSW,BIN,Y1
      REAL*4 ELfactor, NGfactor, DSfactor, LGfactor       ! elastruns
      Integer*2 modyear, endmodyear, s                    ! elastruns

!
!   OPEN AND READ THE DATA FILE

      INFILE=FILE_MGR('O','RSMISC',.FALSE.) ! OPEN THE RSMISC DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(HDR(B),B=1,MNUMBLDG)

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(EH(RECSYEAR,B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(RACSAT(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(RACUNTS(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(CACSAT(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(1(/))')                ! SKIP 2 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(CACPR(D),D=1,MNUMCR-2)

      READ(INFILE,'(1(/))')                ! SKIP 2 LINE HEADER

      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DWPR

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(FRZSAT(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(REFSAT(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(ELDRYPR(B,D),B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO B=1,MNUMBLDG
       DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(SHTSHR(B,D,F),F=1,7)
       ENDDO
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO B=1,MNUMBLDG
       DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(NSHTSHR(B,D,F),F=1,7)
       ENDDO
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)DUMMY,(ELASTIC(S,D), &
             S=1,5) !HEATING FOSSIL, HEATING ELEC, CAC, HP AC, FURN FAN
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
       DO E=1,2
         READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)(NEWDRYSAT(RECSYEAR+1,E,B,D),B=1,MNUMBLDG)
       ENDDO
      ENDDO


      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) &
       (DISHNEW(RECSYEAR+1,B,D), B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

      DO D=1,MNUMCR-2
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) &
       (WASHNEW(RECSYEAR+1,B,D), B=1,MNUMBLDG)
      ENDDO

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER

        do f=1,2
        do d=1,9
        READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)  (WTHRZTN(Y,F,D),Y=RECSYEAR,2050)
        end do
        end do

!    Overwrite prices in the common block for elasticity runs. ! elastruns
                                                               ! elastruns
                                                               ! elastruns
      READ(INFILE,'(2(/))')       ! SKIP 3 LINE HEADER         ! elastruns
                                                               ! elastruns
                                                               ! elastruns
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)                & ! elastruns
          MODYEAR, EndModYear, ELFACTOR, NGFACTOR, DSFACTOR, & ! elastruns
            LGFACTOR                                           ! elastruns
                                                               ! elastruns
      DO y=ModYear,EndModYear                                  ! elastruns
        DO d= 1, MNUMCR-2                                      ! elastruns
          PELRS (d,y) =  PELRS (d,y) * ELfactor                ! elastruns
           do s=1,10                                           ! elastruns
             pelrsout(d,y,s)=pelrsout(d,y,s)*Elfactor          ! elastruns
           Enddo ! s                                           ! elastruns
            PNGRS (d,y) =  PNGRS (d,y) * NGfactor              ! elastruns
            PDSRS (d,y) =  PDSRS (d,y) * DSfactor              ! elastruns
                PLGRS (d,y) =  PLGRS (d,y) * LGfactor          ! elastruns
        ENDDO  ! divisions                                     ! elastruns
       ENDDO    ! years                                        ! elastruns
!
!   READ SUCESSFUL.  CLOSE THE FILE.
!
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMISCREAD: RSMISC data set read successfully'
!
!   ZERO OUT AND INITIALIZE VARIABLES AS APPROPRIATE AND RETURN
!
      DO 1 D=1,MNUMCR-2
        CACPR(D)=1.0+CACPR(D)               ! COMMON/SAT/
        DO 1 B=1,MNUMBLDG
          ELDRYPR(B,D)=ELDRYPR(B,D)/25. ! COMMON/SAT/
          DO 1 Y=RECSYEAR+1,ENDYR
            EH(Y,B,D)=((EH(Y-1,B,D)*HDR(B)))    ! COMMON/EXHS/
1     CONTINUE

      OLDHSES(RECSYEAR)=0.0
      DO 300 D=1,MNUMCR-2
        DO 300 B=1,MNUMBLDG
           OLDHSES(RECSYEAR)=OLDHSES(RECSYEAR)+EH(RECSYEAR,B,D)  ! COMMON/ALLHOUSES/
 300  CONTINUE

!!!PH: loosen the shell efficiency improvement limit (0.3 -> 0.2)
       LIMIT=0.2  !Limits shell improvements to 70% over base year, applies to existing, new, heating, cooling

!!!PH: increase the TECHG parameter
!!! TECHG is a parameter that represents the annual increase in existing shell integrity due to technology improvements
      DO Y1=RECSYEAR+1,LASTYR+baseyr-1
        DO B=1,MNUMBLDG
         TECHG(Y1,1,B)=TECHG(Y1-1,1,B)+0.005 + 0.02
         TECHG(Y1,2,B)=TECHG(Y1-1,2,B)+0.003 + 0.02
         TECHG(Y1,3,B)=TECHG(Y1-1,3,B)+0.006 + 0.02
         TECHG(Y1,4,B)=TECHG(Y1-1,4,B)+0.003 + 0.02
         TECHG(Y1,5,B)=TECHG(Y1-1,5,B)+0.006 + 0.02
         TECHG(Y1,6,B)=TECHG(Y1-1,6,B)+0.016 + 0.02
         TECHG(Y1,7,B)=TECHG(Y1-1,7,B)+0.003 + 0.02
         TECHG(Y1,8,B)=TECHG(Y1-1,8,B)+0.003 + 0.02
         TECHG(Y1,9,B)=TECHG(Y1-1,9,B)+0.011 + 0.02
        END DO
      END DO



      RETURN  !return if successful

!********************************************************************
!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
10    CONTINUE
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMISCREAD file RSMISC read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
95    CONTINUE
      INFILE=FILE_MGR('C','RSMISC',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMISCREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMISCREAD: Correct RSMISC and resubmit job.'
      return
      END SUBROUTINE RSMISCREAD


!********************************************************************
!   READ RESIDENTIAL Lighting DATA
!     RSMLGT.TXT
!********************************************************************
      SUBROUTINE RSMLGTREAD
      IMPLICIT NONE


!   DECLARE LOCAL VARIABLES

      INTEGER FILE_MGR              ! FILE MANAGER
      INTEGER*4 INFILE,           & ! FILE HANDLE
          IOS,                    & ! READ ERR NUMBER
          D,B,E,BIN,Y1,I,app
      Integer lightdiag

       ! reverse these statements to switch diagnostics on or off
       lightdiag=1 !print diagnostics
       lightdiag=0 !don't print diagnostics
       lightdiag=1 !print diagnostics
! READ RSMLGT.TXT -- residential lighting technology and usage data file

      INFILE=FILE_MGR('O','RSMLGT',.FALSE.) ! OPEN THE RMISC DATA SET

      READ(INFILE,'(19(/))')                ! SKIP 20 LINE HEADER

! Control Variables
!   Dollar year for lighting technology costs
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) rlgtdollaryr    !lightmenu
      if(lightdiag .ne. 0) write(9,*) 'Read of Lighting data and Test Print'
      if(lightdiag .ne. 0) write(9,*) RLGTDOLLARYR

!   Number of lighting application types (currently 4 types: general service,
!     reflectors, linear fluorescent and outdoor)
      READ(INFILE,'((/))')                ! SKIP 2 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) NumApps
      if(lightdiag .ne. 0) write(9,*) 'Number of Lighting Apps'
      if(lightdiag .ne. 0) write(9,*) numapps

! Application IDs - 3-character codes and order of index which map to the lighting technology data later
      READ(INFILE,'((/))')                ! SKIP 2 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppID(i),i=1,NumApps)
      if(lightdiag .ne. 0) write(9,'(5a5)') (appid(i),i=1,NumApps)
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppIndex(i),i=1,NumApps)
      if(lightdiag .ne. 0) write(9,'(5i5)') (appindex(i),i=1,NumApps)

! Number of bulbs per application types
      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumTypes(i),i=1,NumApps)
      if(lightdiag .ne. 0) write(9,'(5i5)') (numtypes(i),i=1,NumApps)

! Number of bins (i.e., hours used) by application
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (NumAppBins(i),i=1,NumApps)
      if(lightdiag .ne. 0) write(9,'(5i5)') (numappbins(i),i=1,NumApps)

      READ(INFILE,'(3(/))')                ! SKIP 4 LINE HEADER
! Technology Data - read until a first year of 9999 is found
      DO I=1,NLRec
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)FirstYear(i),LastYear(i),BulbCost(i),(BulbSub(i,d),d=1,mnumcr-2),LPW(i), &
           BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i)
      if(lightdiag .ne. 0) write(9,3)FirstYear(i),LastYear(i),BulbCost(i),(BulbSub(i,d),d=1,mnumcr-2),LPW(i), &
           BulbWatts(i),LifeHours(i),BulbCRI(i),LightingApp(i),BulbType(i)
          ! Convert Lighting Equipment Costs to RTEKDOLLARYR (consistent with prices)
            BulbCost(i)=BulbCost(i)*MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(rlgtdollaryr-BASEYR+1)  !lightmenu
      IF(FirstYear(i).eq.9999) goto 5
      END DO
 3    format(2i6,10f8.2,4f7.1,2a5)
 5    Continue

      READ(INFILE,'(2(/))')                ! SKIP 3 LINE HEADER
      Do app=1,NumApps !maximum 5 lighting applications, currently 4
       READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbsPerHH(app,B),B=1,3)
       if(lightdiag .ne. 0) write(9,'("bulbs per hh",i5,3f8.2)') app,(BulbsPerHH(app,B),B=1,3)
       READ(INFILE,'(1x)')
      Enddo

      Do app=1,NumApps !maximum 5 lighting applications, currently 4

      if(lightdiag .ne. 0) write(9,*) 'APPBINHOURS'
      READ(INFILE,'((/))')                ! SKIP 2 MORE LINES
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (AppBinHours(app,BIN),BIN=1,NumAppBins(app))
      if(lightdiag .ne. 0) write(9,'(6f10.0)') (AppBinHours(app,BIN),BIN=1,NumAppBins(app))

      if(lightdiag .ne. 0) write(9,*) 'LTBINSHARES'
! General service bin shares each of 3 lighting types by 6 bins
      READ(INFILE,'(1x)')
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BinShares(app,BIN),BIN=1,NumAppBins(app))
      if(lightdiag .ne. 0) write(9,'(i4,6f10.2)') app,(BinShares(app,BIN),BIN=1,NumAppBins(app))
      READ(INFILE,'(1x)')
          if(lightdiag .ne. 0) write(9,*) 'app, type, BULBBINSHARES (by type)'
      do e=1,NumTypes(app)
       READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BulbBinShares(app,e,BIN),BIN=1,NumAppBins(app))
       if(lightdiag .ne. 0) write(9,'(2I4,6f10.1)') app,e,(BulbBinSHAREs(app,e,BIN),BIN=1,NumAppBins(app))
      enddo !e
      READ(INFILE,'(1x)')
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS) (BaseWattsBulbs(app,e),e=1,numtypes(app))
      if(lightdiag .ne. 0) write(9,'(i4,6f10.1)') app,(BaseWattsBulbs(app,e),e=1,numtypes(app))
      READ(INFILE,'(1x)')

      Enddo

!----------

!compute RECS-year base watts per bulb weight averaged across bulb types
      do app=1,numapps
       do bin=1,numappbins(app)
        basewattbins(app,bin)=0.
       enddo
      enddo
      do app=1,numapps
       do bin=1,numappbins(app)
        do e=1,numtypes(app)
         basewattbins(app,bin)=basewattbins(app,bin) + bulbbinshares(app,e,bin)*basewattsbulbs(app,e)
        enddo
       enddo
      enddo
      do app=1,numapps
       if(lightdiag .ne. 0) write(9,9) AppID(app),(basewattbins(app,bin),BIN=1,numappbins(app))
      enddo
 9    format(' weighted base watts by bin for app: ',a5, 6f10.2)
! Lighting choice parameters
      READ(INFILE,'((/))')                ! SKIP 2 LINE HEADER
      READ(INFILE,*,ERR=10,END=95,IOSTAT=IOS)  LTLBeta1,LTLBeta2
      if(lightdiag .ne. 0) write(9,'("choice parms",2f7.3)') LTLBeta1,LTLBeta2

      WRITE(6,*) 'RESDMSG SUB_RSMLGT: RSMLGT data set read successfully'

      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      RETURN !successful

!********************************************************************
!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
10    CONTINUE
      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RSMLGTREAD file RSMLGT read error number ',IOS
      if(lightdiag .ne. 0) write(9,*) 'RESDMSG SUB_RSMLGTREAD file RSMLGT read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
95    CONTINUE
      INFILE=FILE_MGR('C','RSMLGT',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RSMLGTREAD: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RSMLGTREAD: Correct RSMLGT and resubmit job.'
      if(lightdiag .ne. 0) write(9,*)'RESDMSG SUB_RSMLGTREAD: EOF reached before all data read in.'
      if(lightdiag .ne. 0) write(9,*)'RESDMSG SUB_RSMLGTREAD: Correct RSMLGT and resubmit job.'
      return

      END SUBROUTINE Rsmlgtread

!*******************************************************************
!     READ IN RETIRING PERCENTAGES FOR DECAY OF 2005 EQUIPMENT
!*******************************************************************
      SUBROUTINE RDRET
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      INTEGER*4 IOS
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSRET01'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 50 RECCL=1,RTCLCNT
      READ(IUNIT1,*,ERR=10,END=95,IOSTAT=IOS) &
        (EQCRET(Y,RECCL), Y=RECSYEAR+1,ijumpcalyr) !
 50   CONTINUE

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      WRITE(6,*) 'RESDMSG SUB_RDRET: RSRET01 data set read successfully'
      RETURN
!********************************************************************
!   READ Error OCCURRED
!   CLOSE THE FILE, PRINT READ ERR MESSAGE, AND RETURN
!
10    CONTINUE
      IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
      WRITE(6,*) 'RESDMSG SUB_RDRET file RSRET01 read error number ',IOS
      RETURN
!
!   EOF REACHED BEFORE DATA COMPLETELY READ IN
!   CLOSE THE FILE, PRINT ERR MESSAGE, AND RETURN
!
95    CONTINUE
      IUNIT1=FILE_MGR('C','FNAME',.FALSE.)
      WRITE(6,*)'RESDMSG SUB_RDRET: EOF reached before all data read in.'
      WRITE(6,*)'RESDMSG SUB_RDRET: Correct RDRET01 and resubmit job.'
      RETURN
      END SUBROUTINE RDRET

!********************************************************************
!     FUEL NUMBERING SYSTEMS MAPPING SUBROUTINE
!********************************************************************
      SUBROUTINE RCONSFL
      IMPLICIT NONE

! NOTE FOR ELECTRICITY, PRICES ARE BY END USE AS FOLLOWS
!    PELRSOUT(...,EU)      rtek EU #
!    1 = Space Heating     1
!    2 = Space Cooling     2
!    3 = Water Heating     5
!    4 = Cooking           6
!    5 = Clothes Dryers    7
!    6 = Refrigeration     8
!    7 = Freezers          9
!    8 = Lighting          not in rtek
!    9 = Appliances/Other  4 dishwashers, other appliances
!    10= Secondary Heating not in rtek


!    MAP RTEK FUEL NUMBERS INTO HTRCON FUEL NUMBERS
!                   HTRCON    RTEK
!                   FUEL #   FUEL #
!    FUEL NAME       FCON       F
!    NATURAL GAS       1        3
!    ELECTRICITY       2        4
!    DISTILLATE        3        1
!    LPG               4        2
!    KEROSENE          5        5
!    WOOD              6        1 (PRICED TO DISTILLATE)
!    GEOTHERMAL        7        4 (PRICED TO ELECTRICITY)
!    SOLAR             8        4

      NHTRFL=7
      FHTRCON(1)=3 !gas
      FHTRCON(2)=4 !elec
      FHTRCON(3)=1 !dist
      FHTRCON(4)=2 !lpg
      FHTRCON(5)=5 !kero
      FHTRCON(6)=6 !wood
      FHTRCON(7)=7 !geo

!    MAP RTEK FUEL NUMBERS INTO CLCON FUEL NUMBERS
!                   CLCON     RTEK
!                   FUEL #    FUEL #
!    FUEL NAME       FCON       F
!    ELECTRICITY       1        4
!    GEOTHERMAL        2        4 (PRICED TO ELEC; USE 7)
!    NATURAL GAS       3        3
      NCLFL=3
      FCLCON(3)=3 !gas
      FCLCON(4)=1 !elec
      FCLCON(7)=2 !geo

!   Water Heaters
      NWHFL=5
      FWHCON(1)=3 !gas
      FWHCON(2)=4 !elec
      FWHCON(3)=1
      FWHCON(4)=2
      FWHCON(8)=5 ! SOLAR
!   Stoves
      NSTVFL=3
      FSTVCON(2)=2
      FSTVCON(3)=1
      FSTVCON(4)=3
!   Clothes Dryers
      NDRYFL=2
      FDRYCON(3)=1 !gas
      FDRYCON(4)=2 !elec
!
      NREFFL=1
!
      NFRZFL=1
!
      NCSWFL=1
!
      NDSWFL=1
!
      RETURN
      END SUBROUTINE RCONSFL


!*******************************************************************
!     READ SQUARE FEET IN NEW AND EXISTING HOUSES
!*******************************************************************
      SUBROUTINE RDSQFOOT
      IMPLICIT NONE
      INTEGER D, Y, B,S
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSSQFT'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO B=1,MNUMBLDG
         DO D=1,MNUMCR-2
           READ(IUNIT1,*) (SQRFOOT(Y,B,D),Y=RECSYEAR,ijumpcalyr)
         END DO
      END DO
      IUNIT1=FILE_MGR('C',FNAME,NEW)
      FNAME='RESOUT.TXT'
      OPEN(9,FILE=FNAME,FORM='FORMATTED')
! 100  FORMAT(1X,3(F6.4))
      END SUBROUTINE RDSQFOOT


!*******************************************************************
!     INITIALIZE PRICES FROM NEMS AND INFLATE
!*******************************************************************
      SUBROUTINE RDPR
      IMPLICIT NONE
      INTEGER D, Y, Y1

      IF(CURCALYR.EQ.RECSYEAR) THEN
        !MAP ALL PRICES FOR YEARS PRIOR TO RECSYEAR AND ALL FUTURE YEARS FROM RESTART FILE
        !  NOT IMPLEMENTED, BUT COULD BE USED FOR EXPECTATIONS
        DO 5 D=1,MNUMCR-2
          DO 5 Y=1,MNUMYR
            Y1=Y+(baseyr-1)
            PRICES(1,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(2,D,Y1)=PLGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(3,D,Y1)=PNGRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(4,D,Y1)=PELRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(5,D,Y1)=PKSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(6,D,Y1)=PDSRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
            PRICES(7,D,Y1)=PCLRS(D,Y)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
 5      CONTINUE
      ELSE
        !OVERWRITE PREVIOUS PRICES WITH CURRENT PRICES
        DO 10 D=1,MNUMCR-2
          PRICES(1,D,CURCALYR)=PDSRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
          PRICES(2,D,CURCALYR)=PLGRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
          PRICES(3,D,CURCALYR)=PNGRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
          PRICES(4,D,CURCALYR)=PELRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
          PRICES(5,D,CURCALYR)=PKSRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
          ! WOOD PRICE IS LINKED TO GAS PRICE LESS ANY CARBON TAX

!bookmark  !!!NOTE IN REVIEWING RESOUT.TXT, THE WOOD PRICE IS THE DISTALLATE PRICE IN RECS YEAR BUT NG PRICE LESS CARBON TAX IN OTHER YEARS!
          PRICES(6,D,CURCALYR)=(PNGRS(D,CURIYR)-JNGRS(CURIYR))*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))

          PRICES(7,D,CURCALYR)=PCLRS(D,CURIYR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))

!  Check for zero prices
         IF (PRICES(1,D,CURCALYR) .LE. 0.0) THEN
           PRICES(1,D,CURCALYR)=PRICES(1,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD DIST REG=',D
         END IF
         IF (PRICES(2,D,CURCALYR).LE. 0.0) THEN
           PRICES(2,D,CURCALYR)=PRICES(2,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD LPG REG=',D
         END IF
         IF (PRICES(3,D,CURCALYR) .LE. 0.0) THEN
           PRICES(3,D,CURCALYR)=PRICES(3,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD NGAS REG=',D
         END IF
         IF (PRICES(4,D,CURCALYR) .LE. 0.0) THEN
           PRICES(4,D,CURCALYR)=PRICES(4,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD ELEC REGION=',D
         END IF
         IF (PRICES(5,D,CURCALYR) .LE. 0.0) THEN
           PRICES(5,D,CURCALYR)=PRICES(5,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD KERO REGION=',D
         END IF
         IF (PRICES(6,D,CURCALYR) .LE. 0.0) THEN
           PRICES(6,D,CURCALYR)=PRICES(6,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD WOOD REGION=',D
         END IF
         IF (PRICES(7,D,CURCALYR) .LE. 0.0) THEN
           PRICES(7,D,CURCALYR)=PRICES(7,D,CURCALYR-1)
           WRITE(6,*) 'RESDMSG Non-pos PRICE FOR RESD COAL REGION=',D
         END IF
 10     CONTINUE
      ENDIF
      END SUBROUTINE RDPR


!********************************************************************
! READ RECS 2005 VINTAGE EQUIPMENT SUBROUTINE
!  1=EF 2=EHP 3=GF 4=GO 5=K 6=L 7=DF 8=DO 9=WS 10=GEO 11=GASHP
!********************************************************************
      SUBROUTINE RDHTREQC
      IMPLICIT NONE
      LOGICAL NEW
      CHARACTER*18 FNAME
      CHARACTER*18 FN
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  B, D, Y, EU, RECCL, IUNIT1,NEQC,EQC,RECCLHP,E

!   READ IN All EQUIPMENT FOR 2009
      FNAME='RSSTK'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')

!
        EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D),B=1,MNUMBLDG)
         END DO
        END DO

        EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 3             ! CLOTHES WASH SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 4             ! DISH WASHER SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 5             ! WATER HEATING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 6             ! STOVE (COOK) SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 7             ! CLOTHES DRY SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 8             ! FOOD REFRIG SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*)   (EQCESE(RECSYEAR,RECCL,B,D),B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 9             ! FOOD FREEZING SECTION OF THE DATA
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCESE(RECSYEAR,RECCL,B,D),B=1,MNUMBLDG)
         END DO
        END DO

! REMOVED LIGHTING FROM RSSTK FILE
!        DO 33 E=1,4
! 33        READ(IUNIT1,*) (LTEQP(RECSYEAR,E,B,D),     B=1,MNUMBLDG)

           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (FANEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (TVSEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (STBEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (HTSEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DVDEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (VGCEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DPCEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (LPCEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (MONEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (NETEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (BATEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (CFNEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (COFEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DEHEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (MCOEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (PHPEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (SECEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (SPAEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2                                              !winecool
            READ(IUNIT1,*) (WCLEQP(RECSYEAR,B,D),      B=1,MNUMBLDG)    !winecool
           END DO                                                       !winecool
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (EAEQP(RECSYEAR,B,D),       B=1,MNUMBLDG)
           END DO

!
        DO E=1,7
          DO D=1,MNUMCR-2
           READ(IUNIT1,*) (SHTEQP(RECSYEAR,B,D,E), B=1,MNUMBLDG)
          END DO
        END DO

        DO E=1,3
          DO D=1,MNUMCR-2
           READ(IUNIT1,*) (APPEQP(RECSYEAR,B,D,E), B=1,MNUMBLDG)
          END DO
        END DO

!
!********************************************************************
!  CALCULATE TOTAL WATER HEATERS TO SHARE OUT CLOTHES AND DISWASHERS
!********************************************************************
      EU=5
      DO 51 D=1,MNUMCR-2
       DO 51 B=1,MNUMBLDG
        HOTWATQ(RECSYEAR,B,D)=0.0
         DO 51 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          HOTWATQ(RECSYEAR,B,D)=HOTWATQ(RECSYEAR,B,D)+EQCESE(RECSYEAR,RECCL,B,D)
 51   CONTINUE
!
!********************************************************************
! PROJECT EXISTING HEATING EQUIPMENT BY APPLYING DEATH RATE
!********************************************************************
      EU=1                     ! SPACE HEATING
      DO 55 D=1,MNUMCR-2
        DO 55 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 55 B=1,MNUMBLDG
            DO 55 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)=((EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-EQCRET(Y,RECCL))))
 55   CONTINUE

          EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
!********************************************************************
!  PROJECT EXISTING COOLING EQUIPMENT SUBROUTINE
!  SET HEAT PUMP EQUAL TO PREVIOUS CALCULATED
!  APPLY DEATH RATE TO EQUIPMENT STOCK
!********************************************************************
      DO 65 D=1,MNUMCR-2
       DO 65 B=1,MNUMBLDG
        DO 65 Y=RECSYEAR+1,ENDYR
          DO 65 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).EQ.0) THEN
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(Y,RECCL)))
            ELSE
              RECCLHP=RTCLPNTR(RECCL)
              EQCESE(Y,RECCL,B,D)=EQCESE(Y,RECCLHP,B,D)
            ENDIF
 65   CONTINUE

      EU = 3                 ! clothes washers
!********************************************************************
!     PROJECT EXISTING CLOTHES WASHER EQUIPMENT
!********************************************************************
      DO 68 D=1,MNUMCR-2
        DO 68 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 68 B=1,MNUMBLDG
             DO 68 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 68   CONTINUE

       EU = 4                ! DISH WASHERS
!********************************************************************
!     PROJECT EXISTING DISH WASHER EQUIPMENT
!********************************************************************
      DO 70 D=1,MNUMCR-2
        DO 70 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 70 B=1,MNUMBLDG
             DO 70 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 70   CONTINUE

      EU = 5
!
!   READ IN WATER HEATING EQUIPMENT SHARE FOR CURRENT RECS YEAR
!********************************************************************
!     PROJECT EXISTING WATER HEATING EQUIPMENT
!********************************************************************
      DO 77 D=1,MNUMCR-2
        DO 77 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 77 B=1,MNUMBLDG
            DO 77 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 77   CONTINUE

      EU = 6
!
!   READ IN STOVE SHARES FOR CURRENT RECS YEAR
!********************************************************************
!********************************************************************
!     PROJECT EXISTING COOKING EQUIPMENT
!********************************************************************
      DO 80 D=1,MNUMCR-2
        DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 80 B=1,MNUMBLDG
             DO 80 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 80   CONTINUE

      EU   = 7
      NEQC = RTCLEUPT(EU+1)-RTCLEUPT(EU)
!
!********************************************************************
!     PROJECT EXISTING DRYER EQUIPMENT
!********************************************************************
      DO 92 D=1,MNUMCR-2
        DO 92 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 92 B=1,MNUMBLDG
              DO 92 Y=RECSYEAR+1,ENDYR
               EQCESE(Y,RECCL,B,D)= ((EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL))))
 92   CONTINUE

      EU = 8                 ! REFRIGERATORS
!********************************************************************
!     PROJECT EXISTING REFRIGERATION EQUIPMENT
!********************************************************************
      DO 94 D=1,MNUMCR-2
        DO 94 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 94 B=1,MNUMBLDG
             DO 94 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 94   CONTINUE

       EU = 9                 ! FREEZERS
!********************************************************************
!     PROJECT EXISTING FREEZER EQUIPMENT
!********************************************************************
      DO 96 D=1,MNUMCR-2
        DO 96 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          DO 96 B=1,MNUMBLDG
             DO 96 Y=RECSYEAR+1,ENDYR
              EQCESE(Y,RECCL,B,D)= (EQCESE(RECSYEAR,RECCL,B,D)* &
                (HDR(B)**(Y-RECSYEAR))*(1.0-eqcret(y,RECCL)))
 96   CONTINUE
      END SUBROUTINE RDHTREQC

!*******************************************************************
!     READ IN RETIRING EFFICIENCIES
!*******************************************************************
      SUBROUTINE RDEFF
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSEFF01'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 50 RECCL=1,RTCLCNT
      READ(IUNIT1,*) &
        (EQCEFF(Y,RECCL), Y=RECSYEAR+1,ijumpcalyr)
 50   CONTINUE

! freeze retiring efficiency at the level 8 years out to approximate
!      do Y=RECSYEAR+1,ijumpcalyr
!        do reccl=1,rtclcnt
!         eqceff(y,reccl)=eqceff(recsyear+8,reccl)
!        enddo
!      enddo

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDEFF
!*******************************************************************
!     READ IN 2005 STOCK EFFICIENCIES
!*******************************************************************
      SUBROUTINE RDSTEFF
      INTEGER  Y, RECCL
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
!********************************************************************
      FNAME='RSSTKEFF'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 50 RECCL=1,RTCLCNT
      READ(IUNIT1,*) &
        (STKEFF(Y,RECCL), Y=RECSYEAR,ijumpcalyr)
 50   CONTINUE

      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDSTEFF

!*******************************************************************
!     READ IN HITORICAL ENERGY STAR HOME SHARES
!*******************************************************************
      SUBROUTINE RDESTARHOMES
      IMPLICIT NONE
      COMMON/ESTARHOMES/HVEQWTN(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR)
      REAL*4 HVEQWTN
      INTEGER  B, D, E, S, Y
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1

      FNAME='RSESTAR'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
      DO 40 D=1,MNUMCR-2
        DO 40 E=1,NHeatTypes
          DO 40 S=1,NShellTypes
 40       READ(IUNIT1,*) (HVEQWTN(Y,E,S,1,D), Y=2047,2050)
      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE RDESTARHOMES

!*******************************************************************
!     READ IN INITIAL UECS
!*******************************************************************
      SUBROUTINE RDUECS
      IMPLICIT NONE
! SECONDARY HEATING FUEL=7=G,E,D,L,K,C,W
! APPLIANCE FUEL=2=Gas, LPG
      INTEGER  B, D, E, F, EU, EQC, RECCL, RECCL1
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1

      FNAME='RSUEC'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')

!
        EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 3             ! CLOTHES WASHER SECTION OF THE DATA
!
        RECCL1=RTCLEUPT(EU)+1
        DO D=1,MNUMCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,MNUMBLDG)
        END DO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          END DO
         END DO
        END DO
!
        EU = 4             ! DISH WASHER SECTION OF THE DATA
!
        RECCL1=RTCLEUPT(EU)+1
        DO D=1,MNUMCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,MNUMBLDG)
        END DO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          END DO
         END DO
        END DO
!
        EU = 5             ! WATER HEATING SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,MNUMBLDG)
         END DO
        END DO
!
!
        EU = 6             ! STOVE (COOK) SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 7             ! CLOTHES DRY SECTION OF THE DATA
!
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (EQCUEC(D,RECCL,B), B=1,MNUMBLDG)
         END DO
        END DO
!
        EU = 8             ! FOOD REFRIG SECTION OF THE DATA
!
        RECCL1=RTCLEUPT(EU)+1
        DO D=1,MNUMCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,MNUMBLDG)
        END DO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          END DO
         END DO
        END DO
!
        EU = 9             ! FOOD FREEZING SECTION OF THE DATA
!
        RECCL1=RTCLEUPT(EU)+1
        DO D=1,MNUMCR-2
         READ(IUNIT1,*)(EQCUEC(D,RECCL1,B),B=1,MNUMBLDG)
        END DO
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            EQCUEC(D,RECCL,B)=EQCUEC(D,RECCL1,B)
          END DO
         END DO
        END DO

! REMOVED LIGHTING FROM UEC INPUT FILE.
!        DO E=1,4
!         READ(IUNIT1,*) (LTUEC(E,D,B),B=1,MNUMBLDG)
!        END DO

           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (FANUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (TVSUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (STBUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (HTSUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DVDUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (VGCUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DPCUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (LPCUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (MONUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (NETUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (BATUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (CFNUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (COFUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (DEHUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (MCOUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (PHPUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (SECUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (SPAUEC(D,B),      B=1,MNUMBLDG)
           END DO
           DO D=1,MNUMCR-2                                     !winecool
            READ(IUNIT1,*) (WCLUEC(D,B),      B=1,MNUMBLDG)    !winecool
           END DO                                              !winecool
           DO D=1,MNUMCR-2
            READ(IUNIT1,*) (EAUEC(D,B),       B=1,MNUMBLDG)
           END DO

!
        DO E=1,7
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (SHTUEC(D,E,B), B=1,MNUMBLDG)
         END DO
        END DO

        DO E=1,3
         DO D=1,MNUMCR-2
          READ(IUNIT1,*) (APPUEC(D,E,B), B=1,MNUMBLDG)
         END DO
        END DO


      IUNIT1=FILE_MGR('C',FNAME,NEW)
100   FORMAT(2X,3(F9.4,3X))
      END SUBROUTINE RDUECS

!********************************************************************
!     CALCULATE CONSUMPTION IN RECS YEAR
!       CALLED ONLY FOR CURIYR = RECSYEAR
!*******************************************************************
      SUBROUTINE EXCONS
      IMPLICIT NONE
      REAL*4  TVSCON(MNUMYR,MNUMCR-2),STBCON(MNUMYR,MNUMCR-2),HTSCON(MNUMYR,MNUMCR-2),DVDCON(MNUMYR,MNUMCR-2),&
              VGCCON(MNUMYR,MNUMCR-2)
      REAL*4  DPCCON(MNUMYR,MNUMCR-2),LPCCON(MNUMYR,MNUMCR-2),MONCON(MNUMYR,MNUMCR-2),NETCON(MNUMYR,MNUMCR-2)
      REAL*4  BATCON(MNUMYR,MNUMCR-2),CFNCON(MNUMYR,MNUMCR-2),COFCON(MNUMYR,MNUMCR-2),DEHCON(MNUMYR,MNUMCR-2),&
              MCOCON(MNUMYR,MNUMCR-2),PHPCON(MNUMYR,MNUMCR-2),SPACON(MNUMYR,MNUMCR-2),WCLCON(MNUMYR,MNUMCR-2),&    !winecool
              SECCON(MNUMYR,MNUMCR-2),EACON(MNUMYR,MNUMCR-2)

      REAL*4  TVSCONUS(MNUMYR),STBCONUS(MNUMYR),HTSCONUS(MNUMYR),DVDCONUS(MNUMYR),VGCCONUS(MNUMYR)
      REAL*4  DPCCONUS(MNUMYR),LPCCONUS(MNUMYR),MONCONUS(MNUMYR),NETCONUS(MNUMYR)
      REAL*4  BATCONUS(MNUMYR),CFNCONUS(MNUMYR),COFCONUS(MNUMYR),DEHCONUS(MNUMYR),&
              MCOCONUS(MNUMYR),PHPCONUS(MNUMYR),SPACONUS(MNUMYR),WCLCONUS(MNUMYR),&    !winecool
              SECCONUS(MNUMYR),EACONUS(MNUMYR)

      INTEGER D,E,F,B,FCON,EU,EQC,RECCL,EQCGHP,EQCEHP,EQCEWH,EQCSWH,y
      INTEGER RECCLGHP,RECCLEHP,RECCLEWH,RECCLSWH

          ! 111(d) - initialize sales based on input restart file, against which to track savings (written to RESOUT.txt)
          Write(9,*) 'RESTART FILE baseline electricity data (trills) d, y, qelrs(d,y), qelcm(d,y)'
          DO D=1,mnumcr-2
           DO y=1,mnumyr
           ! QELRS,QELCM in trills
           WRITE(9,5) D,Y,qelrs(d,y),qelcm(d,y)
           ENDDO
          ENDDO
 5    FORMAT(2i5,2F12.5)

!  SET HOT WATER LOAD ADDITIONS FOR CLOTHESWASHERS AND DISHWASHERS
      CWLOAD(RECSYEAR)=0.2047

!CALCULATE HEATING CONSUMPTION RECSYEAR,F,D,B
      DO D=1,MNUMCR-2
        DO F=1,NHTRFL
          HTRCON(CURIYR,F,D)=0.0
          SHTCON(CURIYR,F,D)=0.0
        ENDDO
      ENDDO

      ! SET EU = 1 FOR SPACE HEATING IN RSCLASS
      EU = 1
      ! CALCULATE HEATING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
      ! AT THE SAME TIME IDENTIFY HPS AND GEOTHERMAL HPS FOR LATER USE
      ! LOOP OVER ALL HEATING EQUIPMNENT TYPES
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
        EQC=RTCLEQCL(RECCL)
        F  =RTFUEL(RECCL)
        ! MAP RSCLASS FUEL NUMBERS INTO HTRCON FUEL NUMBERS
        FCON=FHTRCON(F)
            ! ALSO FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEATPUMPS
            ! THESE ARE USED TO COMPUTE CONSUMPTION FOR GEOTHERMAL
            !  AND IDENTIFY ELECTRIC VS GEOTHERMAL HP EQUIPMENT.
            ! GEOTHERMAL USE (UNDER FUEL 7) IS IN ADDITION TO ELECTRICITY,
            !  ITS DESIGNATED FUEL UNDER RSCLASS.
            ! SINCE FUEL 7 IS NOT ENCOUNTERED IN THIS LOOP THROUGH RSCLASS,
            !  CALCULATE BELOW AFTER THIS LOOP.
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
              EQCEHP=RTCLEQCL(RECCL)
              RECCLEHP=EQCEHP
             ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
              EQCGHP=RTCLEQCL(RECCL)
              RECCLGHP=EQCGHP
            ENDIF
        ! NOW THAT FUEL IS MAPPED ROLL UP BY D AND B
        DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            HTRCON(CURIYR,FCON,D)=HTRCON(CURIYR,FCON,D)+ &
              (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
          ENDDO !BUILDING TYPES, B
        ENDDO !CENSUS DIVISIONS, D
      ENDDO !HEATING EQUIPMENT TYPES, RECCL

      ! FINALLY ROLL UP GEOTHERMAL NOW THAT RSCLASS HAS BEEN PROCESSED
      FCON = FHTRCON(7)  !7 FOR HEATING
      DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
         ! NOW THAT ELECTRIC AND GEOTHERMAL HP ARE IDENTIFIED, CALCULATE CONSUMPTION.
         ! FOR GEOTHERMAL (FUEL 7) AS THE DIFFERENCE BETWEEN THE HP AND GHP UEC
         !  DIVIDIED BY .33 TO CONVERT ELECTRICITY INTO PRIMARY GEOTHERMAL USE.
         HTRCON(CURIYR,FCON,D)=HTRCON(CURIYR,FCON,D)+ &
          EQCESE(RECSYEAR,RECCLGHP,B,D)*(EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*FossilHR/3412.
        ENDDO !BUILDING TYPES, B
      ENDDO !CENSUS DIVISIONS, D

!CALCULATE COOLING CONSUMPTION
      DO D=1,MNUMCR-2
        DO F=1,NCLFL
          COOLCN(CURIYR,F,D)=0.0
        ENDDO
      ENDDO

      ! SET EU = 2 FOR SPACE COOLING IN RSCLASS
      EU = 2
      ! CALCULATE COOLING CONSUMPTION FOR THE IDENTIFIED FUELS IN RSCLASS
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !ALL RECORDS IN RSCLASS
        EQC=RTCLEQCL(RECCL)
        F  =RTFUEL(RECCL)
        ! MAP RSCLASS FUEL NUMBERS INTO CLCON FUEL NUMBERS
        FCON=FCLCON(F)
        ! ALSO FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEATPUMPS AS FOR HEATING
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          ! NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCEHP BY HEATERS
          RECCLEHP=EQCEHP+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          ! NOTE START NUMBERING AFTER HEATERS -- NEED TO INCREMENT EQCGHP BY HEATERS
          RECCLGHP=EQCGHP+RTCLEUPT(EU)
        ENDIF

        ! NOW THAT FUEL IS MAPPED ROLL UP BY D AND B
        DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
            COOLCN(CURIYR,FCON,D)=COOLCN(CURIYR,FCON,D)+ &
              (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
          ENDDO !BUILDING TYPES, B
        ENDDO !CENSUS DIVISIONS, D
      ENDDO !COOLING EQUIPMENT TYPES, RECCL

      ! FINALLY ROLL UP GEOTHERMAL NOW THAT RSCLASS HAS BEEN PROCESSED
      FCON = FCLCON(7) !=2 FOR GEOTHERMAL COOLING
      DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
        COOLCN(CURIYR,FCON,D)=COOLCN(CURIYR,FCON,D)+ &
         EQCESE(RECSYEAR,RECCLGHP,B,D)*(EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*FossilHR/3412.
        ENDDO !BUILDING TYPES, B
      ENDDO !CENSUS DIVISIONS, D

!CALCULATE CLOTHES WASHER CONSUMPTION
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
      EU = 3
      DO 160 D=1,MNUMCR-2
        DO 160 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          CSWCON(CURIYR,D)=0.0
          DO 160 B=1,MNUMBLDG
            CSWCON(CURIYR,D)=CSWCON(CURIYR,D)+ &
             (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
 160  CONTINUE

!CALCULATE DISHWASHER CONSUMPTION
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
      EU = 4
      DO 170 D=1,MNUMCR-2
        DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DSWCON(CURIYR,D)=0.0
          DO 170 B=1,MNUMBLDG
            DSWCON(CURIYR,D)=DSWCON(CURIYR,D)+ &
             (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
 170  CONTINUE

!CALCULATE WATER HEATING CONSUMPTION - GEDL
!ALSO CALCULATE SOLAR CONSUMPTION - USES EL UEC (55 PERCENT)
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
      EU = 5

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

!  FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS
!    USED TO COMPUTE H20CON FOR SOLAR FUEL (FCON=5)
!
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH')THEN
          EQCEWH=RTCLEQCL(RECCL)
          !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
          RECCLEWH=EQCEWH+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'SOLAR_WH')THEN
          EQCSWH=RTCLEQCL(RECCL)
          !AS FOR COOLING, INCREMENT THE EQUIPMENT CLASS BY THE SUM OF CLASSES BEFORE IT
          RECCLSWH=EQCSWH+RTCLEUPT(EU)
        ENDIF
      ENDDO

      DO D=1,MNUMCR-2
       SLCON(CURIYR,D)=0.0
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC  = RTCLEQCL(RECCL)
          F  =RTFUEL(RECCL)
          FCON = FWHCON(F)
           H2OCON(CURIYR,FCON,D)=0.0
        ENDDO
      ENDDO
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC  = RTCLEQCL(RECCL)
          F  =RTFUEL(RECCL)
          FCON = FWHCON(F)
          H2OCON(CURIYR,FCON,D)=H2OCON(CURIYR,FCON,D)+(EQCUEC(D,RECCL,B)*EQCESE(RECSYEAR,RECCL,B,D))
        ENDDO

!
!    SOLAR IS COMPUTED DIFFERENTLY
!
!        FCON = FWHCON(8)
!
        H2OCON(CURIYR,5,D)=H2OCON(CURIYR,5,D)+ &
         (EQCESE(RECSYEAR,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412.)
        SLCON(CURIYR,D)=SLCON(CURIYR,D)+ &
         (EQCESE(RECSYEAR,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412.)
        SLEQCN(CURIYR,1,B,D)=(EQCESE(RECSYEAR,RECCLSWH,B,D)* &
         (EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412.)

        ENDDO
       ENDDO


!*******************************************************************
!  CALCULATE COOKING CONSUMPTION - GLE
!
!   SET EU = 6 TO SEARCH THE STOVE (COOK) SECTION OF THE DATA
!
      EU = 6
!
!*******************************************************************
      DO 250 D=1,MNUMCR-2
        DO 250 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          CKCON(CURIYR,EQC,D)=0.0
          DO 250 B=1,MNUMBLDG
            CKCON(CURIYR,EQC,D)=CKCON(CURIYR,EQC,D)+ &
              (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
 250  CONTINUE
!*******************************************************************
!  CALCULATE DRYER CONSUMPTION - GE
!
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!
      EU = 7
!
!*******************************************************************
      DO 270 D=1,MNUMCR-2
        DO 270 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DRYCON(CURIYR,EQC,D)=0.0
          DO 270 B=1,MNUMBLDG
            DRYCON(CURIYR,EQC,D)=DRYCON(CURIYR,EQC,D)+ &
              (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
 270  CONTINUE
!*******************************************************************
!  CALCULATE REFRIGERATOR CONSUMPTION
!
!   SET EU = 8 TO SEARCH THE FOOD REFRIG SECTION OF THE DATA
!
      EU = 8
!
!*******************************************************************
      DO 300 D=1,MNUMCR-2
        DO 300 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          REFCON(CURIYR,D)=0.0
          DO 300 B=1,MNUMBLDG
            REFCON(CURIYR,D)=REFCON(CURIYR,D)+ &
             (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
 300  CONTINUE
!*******************************************************************
!  CALCULATE FREEZER CONSUMPTION
!
!   SET EU = 9 TO SEARCH THE FOOD FREEZING SECTION OF THE DATA
!
      EU = 9
!
!*******************************************************************
      DO 400 D=1,MNUMCR-2
        DO 400 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          FRZCON(CURIYR,D)=0.0
          DO 400 B=1,MNUMBLDG
            FRZCON(CURIYR,D)=FRZCON(CURIYR,D)+ &
             (EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B))
  400  CONTINUE
!*******************************************************************
! moved to Lighting Subroutine  CALCULATE LIGHTING CONSUMPTION
!********************************************************************
!     DO 500 D=1,MNUMCR-2
!        LTCON(CURIYR,D)=0.0
!        DO 500 B=1,MNUMBLDG
!         DO 500 E=1,4
!          LTCON(CURIYR,D)=LTCON(CURIYR,D)+(LTEQP(RECSYEAR,E,B,D)*LTUEC(E,D,B))
!  500   CONTINUE
!
!*******************************************************************
!  CALCULATE PERSONAL COMPUTER CONSUMPTION
!********************************************************************
!     DO 510 D=1,MNUMCR-2
!        PCCON(CURIYR,D)=0.0
!        DO 510 B=1,MNUMBLDG
!          PCCON(CURIYR,D)=PCCON(CURIYR,D)+(PCEQP(RECSYEAR,B,D)*PCUEC(D,B))
! 510   CONTINUE
!
!*******************************************************************
!  CALCULATE COLOR TV CONSUMPTION
!********************************************************************
!      DO 520 D=1,MNUMCR-2
!        TVCON(CURIYR,D)=0.0
!        DO 520 B=1,MNUMBLDG
!          TVCON(CURIYR,D)=TVCON(CURIYR,D)+(TVEQP(RECSYEAR,B,D)*TVUEC(D,B))+ &
!          STBCON(CURIYR,D)+(STBEQP(RECSYEAR,B,D)*STBUEC(D,B))+ &
!          VGCON(CURIYR,D)+(VGEQP(RECSYEAR,B,D)*VGUEC(D,B))
! 520   CONTINUE
!
!*******************************************************************
!  CALCULATE FURNACE FAN
!********************************************************************
      DO 521 D=1,MNUMCR-2
         FANCON(CURIYR,D)=0.0
         DO 521 B=1,MNUMBLDG
         FANEQCN(CURIYR,1,b,d)=(FANEQP(RECSYEAR,B,D)*FANUEC(D,B))
         FANCON(CURIYR,D)=FANCON(CURIYR,D)+(FANEQP(RECSYEAR,B,D)*FANUEC(D,B))
521   CONTINUE
!
!*******************************************************************
!  CALCULATE CONSUMPTION FOR MISCELLANEOUS ELECTRIC LOADS (MELs)
!   TELEVISIONS, SET-TOP BOXES, HOME THEATER SYSTEMS, DVD PLAYERS, VIDEO GAME CONSOLES,
!   DESKTOP PCs, LAPTOP PCs, MONITORS, NETWORKING EQUIPMENT, RECHARGEABLES,CEILING FANS,
!   COFFEE MACHINES, DEHUMIDIFIERS, MICROWAVES, POOL HEATERS AND PUMPS, SECURITY SYSTEMS, SPAS
!********************************************************************

      DO 523 D=1,MNUMCR-2
        TVSCON(CURIYR,D)=0.0
        STBCON(CURIYR,D)=0.0
        HTSCON(CURIYR,D)=0.0
        DVDCON(CURIYR,D)=0.0
        VGCCON(CURIYR,D)=0.0
        DPCCON(CURIYR,D)=0.0
        LPCCON(CURIYR,D)=0.0
        MONCON(CURIYR,D)=0.0
        NETCON(CURIYR,D)=0.0
        BATCON(CURIYR,D)=0.0
        CFNCON(CURIYR,D)=0.0
        COFCON(CURIYR,D)=0.0
        DEHCON(CURIYR,D)=0.0
        MCOCON(CURIYR,D)=0.0
        PHPCON(CURIYR,D)=0.0
        SECCON(CURIYR,D)=0.0
        SPACON(CURIYR,D)=0.0
        WCLCON(CURIYR,D)=0.0    !winecool
        EACON(CURIYR,D)=0.0
        DO 523 B=1,MNUMBLDG
          TVSCON(CURIYR,D)=TVSCON(CURIYR,D)+(TVSEQP(RECSYEAR,B,D)*TVSUEC(D,B))
          STBCON(CURIYR,D)=STBCON(CURIYR,D)+(STBEQP(RECSYEAR,B,D)*STBUEC(D,B))
          HTSCON(CURIYR,D)=HTSCON(CURIYR,D)+(HTSEQP(RECSYEAR,B,D)*HTSUEC(D,B))
          DVDCON(CURIYR,D)=DVDCON(CURIYR,D)+(DVDEQP(RECSYEAR,B,D)*DVDUEC(D,B))
          VGCCON(CURIYR,D)=VGCCON(CURIYR,D)+(VGCEQP(RECSYEAR,B,D)*VGCUEC(D,B))
          DPCCON(CURIYR,D)=DPCCON(CURIYR,D)+(DPCEQP(RECSYEAR,B,D)*DPCUEC(D,B))
          LPCCON(CURIYR,D)=LPCCON(CURIYR,D)+(LPCEQP(RECSYEAR,B,D)*LPCUEC(D,B))
          MONCON(CURIYR,D)=MONCON(CURIYR,D)+(MONEQP(RECSYEAR,B,D)*MONUEC(D,B))
          NETCON(CURIYR,D)=NETCON(CURIYR,D)+(NETEQP(RECSYEAR,B,D)*NETUEC(D,B))
          BATCON(CURIYR,D)=BATCON(CURIYR,D)+(BATEQP(RECSYEAR,B,D)*BATUEC(D,B))
          CFNCON(CURIYR,D)=CFNCON(CURIYR,D)+(CFNEQP(RECSYEAR,B,D)*CFNUEC(D,B))
          COFCON(CURIYR,D)=COFCON(CURIYR,D)+(COFEQP(RECSYEAR,B,D)*COFUEC(D,B))
          DEHCON(CURIYR,D)=DEHCON(CURIYR,D)+(DEHEQP(RECSYEAR,B,D)*DEHUEC(D,B))
          MCOCON(CURIYR,D)=MCOCON(CURIYR,D)+(MCOEQP(RECSYEAR,B,D)*MCOUEC(D,B))
          PHPCON(CURIYR,D)=PHPCON(CURIYR,D)+(PHPEQP(RECSYEAR,B,D)*PHPUEC(D,B))
          SECCON(CURIYR,D)=SECCON(CURIYR,D)+(SECEQP(RECSYEAR,B,D)*SECUEC(D,B))
          SPACON(CURIYR,D)=SPACON(CURIYR,D)+(SPAEQP(RECSYEAR,B,D)*SPAUEC(D,B))
          WCLCON(CURIYR,D)=WCLCON(CURIYR,D)+(WCLEQP(RECSYEAR,B,D)*WCLUEC(D,B))    !winecool
          EACON(CURIYR,D)=EACON(CURIYR,D)+(EH(RECSYEAR,B,D)*EAUEC(D,B))
          EAEQCN(CURIYR,1,B,D)=EH(RECSYEAR,B,D)*EAUEC(D,B)
 523   CONTINUE
        TVSCONUS(CURIYR)=0.0
        STBCONUS(CURIYR)=0.0
        HTSCONUS(CURIYR)=0.0
        DVDCONUS(CURIYR)=0.0
        VGCCONUS(CURIYR)=0.0
        DPCCONUS(CURIYR)=0.0
        LPCCONUS(CURIYR)=0.0
        MONCONUS(CURIYR)=0.0
        NETCONUS(CURIYR)=0.0
        BATCONUS(CURIYR)=0.0
        CFNCONUS(CURIYR)=0.0
        COFCONUS(CURIYR)=0.0
        DEHCONUS(CURIYR)=0.0
        MCOCONUS(CURIYR)=0.0
        PHPCONUS(CURIYR)=0.0
        SPACONUS(CURIYR)=0.0
        WCLCONUS(CURIYR)=0.0    !winecool
        SECCONUS(CURIYR)=0.0
        EACONUS(CURIYR)=0.0
      DO D=1,9
        TVSCONUS(CURIYR)=TVSCONUS(CURIYR)+TVSCON(CURIYR,D)
        STBCONUS(CURIYR)=STBCONUS(CURIYR)+STBCON(CURIYR,D)
        HTSCONUS(CURIYR)=HTSCONUS(CURIYR)+HTSCON(CURIYR,D)
        DVDCONUS(CURIYR)=DVDCONUS(CURIYR)+DVDCON(CURIYR,D)
        VGCCONUS(CURIYR)=VGCCONUS(CURIYR)+VGCCON(CURIYR,D)
        DPCCONUS(CURIYR)=DPCCONUS(CURIYR)+DPCCON(CURIYR,D)
        LPCCONUS(CURIYR)=LPCCONUS(CURIYR)+LPCCON(CURIYR,D)
        MONCONUS(CURIYR)=MONCONUS(CURIYR)+MONCON(CURIYR,D)
        NETCONUS(CURIYR)=NETCONUS(CURIYR)+NETCON(CURIYR,D)
        BATCONUS(CURIYR)=BATCONUS(CURIYR)+BATCON(CURIYR,D)
        CFNCONUS(CURIYR)=CFNCONUS(CURIYR)+CFNCON(CURIYR,D)
        COFCONUS(CURIYR)=COFCONUS(CURIYR)+COFCON(CURIYR,D)
        DEHCONUS(CURIYR)=DEHCONUS(CURIYR)+DEHCON(CURIYR,D)
        MCOCONUS(CURIYR)=MCOCONUS(CURIYR)+MCOCON(CURIYR,D)
        PHPCONUS(CURIYR)=PHPCONUS(CURIYR)+PHPCON(CURIYR,D)
        SECCONUS(CURIYR)=SECCONUS(CURIYR)+SECCON(CURIYR,D)
        SPACONUS(CURIYR)=SPACONUS(CURIYR)+SPACON(CURIYR,D)
        WCLCONUS(CURIYR)=WCLCONUS(CURIYR)+WCLCON(CURIYR,D)    !winecool
        EACONUS(CURIYR)=EACONUS(CURIYR)+EACON(CURIYR,D)
      END DO

!*******************************************************************
!  CALCULATE TELEVISIONS AND RELATED EQUIPMENT TOTAL
!********************************************************************
      DO 530 D=1,MNUMCR-2
        TVRCON(CURIYR,D)=0.0
        DO 530 B=1,MNUMBLDG
          TVRCON(CURIYR,D)=TVSCON(CURIYR,D)+STBCON(CURIYR,D)+HTSCON(CURIYR,D)+DVDCON(CURIYR,D)+VGCCON(CURIYR,D)
 530   CONTINUE

!*******************************************************************
!  CALCULATE PERSONAL COMPUTER AND RELATED EQUIPMENT TOTAL
!********************************************************************
      DO 531 D=1,MNUMCR-2
        PCRCON(CURIYR,D)=0.0
        DO 531 B=1,MNUMBLDG
          PCRCON(CURIYR,D)=DPCCON(CURIYR,D)+LPCCON(CURIYR,D)+MONCON(CURIYR,D)+NETCON(CURIYR,D)
 531   CONTINUE


!********************************************************************
! ELECTRIC APPLIANCES
!********************************************************************
      DO 550 D=1,MNUMCR-2
        APCON(CURIYR,D)=0.0
        DO 550 B=1,MNUMBLDG
          APCON(CURIYR,D)=BATCON(CURIYR,D)+CFNCON(CURIYR,D)+&
            COFCON(CURIYR,D)+DEHCON(CURIYR,D)+MCOCON(CURIYR,D)+&
            PHPCON(CURIYR,D)+SECCON(CURIYR,D)+SPACON(CURIYR,D)+&
            WCLCON(CURIYR,D)+EACON(CURIYR,D)                      !winecool
 550   CONTINUE
!*******************************************************************
!  NON-ELECTRIC APPLIANCES CONSUMPTION  G,L,D
!********************************************************************
      DO 700 D=1,MNUMCR-2
        DO 700 F=1,3
          APLCON(CURIYR,F,D)=0.0
          DO 700 B=1,MNUMBLDG
            APLCON(CURIYR,F,D)=APLCON(CURIYR,F,D)+(APPUEC(D,F,B) &
                                                  *APPEQP(RECSYEAR,B,D,F))
 700   CONTINUE
!*******************************************************************
!  CALCULATE SECONDARY HEATING CONSUMPTION
!********************************************************************
      DO 800 D=1,MNUMCR-2
        DO 800 F=1,7
          SHTCON(CURIYR,F,D)=0.0
         DO 800 B=1,MNUMBLDG
          SHEQCN(CURIYR,F,B,D)=SHTEQP(RECSYEAR,B,D,F)*SHTUEC(D,F,B)
          SHTCON(CURIYR,F,D)=SHTCON(CURIYR,F,D)+(SHTEQP(RECSYEAR,B,D,F)&
            *SHTUEC(D,F,B))
 800  CONTINUE
!********************************************************************
!     INITIALIZE 2009 NEMS DATA
!********************************************************************
         QBMRS(NATIONALPTR,CURIYR)=0.0
         QGERS(NATIONALPTR,CURIYR)=0.0
         QSTRS(NATIONALPTR,CURIYR)=0.0
         QPVRS(NATIONALPTR,CURIYR)=0.0
!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 750 D=1,MNUMCR-2
! SOLAR ENERGY - ADDED SOLAR WATER HEATERS (SLCON)
         SLCON(CURIYR,10)=SLCON(CURIYR,10)+H2OCON(CURIYR,5,D)
         QSTRS(D,CURIYR)=H2OCON(CURIYR,5,D)/1000000.
         QSTRS(NATIONALPTR,CURIYR)=QSTRS(NATIONALPTR,CURIYR)+H2OCON(CURIYR,5,D)/1000000.
         QPVRS(D,CURIYR)=QPVRS(NATIONALPTR,CURIYR)*RENSHR(D)
! GAS
         RSFLCN(CURIYR,1,D)= &
         (HTRCON(CURIYR,1,D)+H2OCON(CURIYR,1,D)+APLCON(CURIYR,1,D)+COOLCN(CURIYR,3,D)+ &
         CKCON(CURIYR,1,D)+DRYCON(CURIYR,1,D)+SHTCON(CURIYR,1,D))/1000000.
         QNGRS(D,CURIYR)=RSFLCN(CURIYR,1,D)
         QGFRS(D,CURIYR)=RSFLCN(CURIYR,1,D)*1.0
         QGIRS(D,CURIYR)=RSFLCN(CURIYR,1,D)*0.0
! ELECTRIC
         RSFLCN(CURIYR,2,D)= &
         (HTRCON(CURIYR,2,D)+COOLCN(CURIYR,1,D)+H2OCON(CURIYR,2,D)+REFCON(CURIYR,D)+ &
          FRZCON(CURIYR,D)+LTCON(CURIYR,D)+APCON(CURIYR,D)+CKCON(CURIYR,3,D)+ &
          DRYCON(CURIYR,2,D)+SHTCON(CURIYR,2,D)+PCRCON(CURIYR,D)+TVRCON(CURIYR,D)+ &
          CSWCON(CURIYR,D)+DSWCON(CURIYR,D)+FANCON(CURIYR,D))/1000000.
         QELRS(D,CURIYR)=RSFLCN(CURIYR,2,D)
! DISTILLATE
         RSFLCN(CURIYR,3,D)= &
         (HTRCON(CURIYR,3,D)+H2OCON(CURIYR,3,D)+APLCON(CURIYR,3,D)+SHTCON(CURIYR,3,D)) &
         /1000000.
         QDSRS(D,CURIYR)=RSFLCN(CURIYR,3,D)
! LPG
         RSFLCN(CURIYR,4,D)= &
         (HTRCON(CURIYR,4,D)+H2OCON(CURIYR,4,D)+APLCON(CURIYR,2,D)+ &
         CKCON(CURIYR,2,D)+SHTCON(CURIYR,4,D))/1000000.
         QLGRS(D,CURIYR)=RSFLCN(CURIYR,4,D)
         QPRRS(D,CURIYR)=QLGRS(D,CURIYR)
! KEROSENE
         RSFLCN(CURIYR,5,D)=(HTRCON(CURIYR,5,D)+SHTCON(CURIYR,5,D))/1000000.
         QKSRS(D,CURIYR)=RSFLCN(CURIYR,5,D)
! COAL
         RSFLCN(CURIYR,6,D)=SHTCON(CURIYR,6,D)/1000000.
!         QCLRS(D,CURIYR)=RSFLCN(CURIYR,6,D)  !removing coal from consumption totals
         QCLRS(D,CURIYR)=0.0
! BIOMASS (WOOD)
         RSFLCN(CURIYR,7,D)=(HTRCON(CURIYR,6,D)+SHTCON(CURIYR,7,D))/1000000.
         QBMRS(D,CURIYR)=RSFLCN(CURIYR,7,D)
! GEOTHERMAL
         RSFLCN(CURIYR,8,D)=(HTRCON(CURIYR,7,D)+COOLCN(CURIYR,2,D))/1000000.
         QGERS(D,CURIYR)=RSFLCN(CURIYR,8,D)
 750   CONTINUE
!********************************************************************
!     CALCULATE US (DIVISION 10) FUEL CONSUMPTION
!********************************************************************
      DO 775 F=1,8
        RSFLCN(CURIYR,F,10)=0.0
        DO 775 D=1,MNUMCR-2
          RSFLCN(CURIYR,F,10)=RSFLCN(CURIYR,F,10)+RSFLCN(CURIYR,F,D)
 775  CONTINUE
      QELRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,2,10)
      QNGRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,1,10)
      QGFRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,1,10)*1.0  ! FIRM GAS
      QGIRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,1,10)*0.0  ! INTERRUPTABLE GAS
      QDSRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,3,10)
      QLGRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,4,10)
      QPRRS(NATIONALPTR,CURIYR)=QLGRS(NATIONALPTR,CURIYR)
      QKSRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,5,10)
!      QCLRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,6,10)
      QCLRS(NATIONALPTR,CURIYR)=0.0
      QBMRS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,7,10)
      QGERS(NATIONALPTR,CURIYR)=RSFLCN(CURIYR,8,10)
      END SUBROUTINE EXCONS

!******************************************************************
!     CALCULATE EPACT WINDOW LABELING IMPACT
!*******************************************************************
      SUBROUTINE EPACTWD
      IMPLICIT NONE
      REAL*4 OLDSHR
      INTEGER Y,D,B
!*******************************************************************
!   COMPUTE EXISTING HOUSING SHARE
!*******************************************************************
      OLDHSES(CURCALYR)=0.0
      NEWHSES(CURCALYR)=0.0
      DO 5 B=1,MNUMBLDG
        DO 5 D=1,MNUMCR-2
          OLDHSES(CURCALYR)=OLDHSES(CURCALYR)+EH(CURCALYR,B,D)
          NEWHSES(CURCALYR)=NEWHSES(CURCALYR)+NH(CURCALYR,B,D)
 5    CONTINUE
      OLDSHR=OLDHSES(CURCALYR)/(OLDHSES(CURCALYR)+NEWHSES(CURCALYR))
!
      END SUBROUTINE EPACTWD


!*******************************************************************
!     CALCULATE NEW HOUSING FOR RSYR = 2006-2011
!*******************************************************************
      SUBROUTINE NEWHSE
      IMPLICIT NONE
      COMMON/STEO/STEOCN(RECSYEAR+1:LASTSTEOYRAVAIL,9,MNUMCR-2)
      REAL*4 STEOCN
      INTEGER  Y, B, D, E, F, IUNIT1,Y1

      Y=CURCALYR
      Y1=CURIYR

      DO 10 D=1,MNUMCR-2
         IF (Y.EQ.RECSYEAR+1) NH(RECSYEAR,1,D)=0.0
         IF (Y.EQ.RECSYEAR+1) NH(RECSYEAR,2,D)=0.0
         IF (Y.EQ.RECSYEAR+1) NH(RECSYEAR,3,D)=0.0

!       IF (CURCALYR.LE.LASTSTEOYRAVAIL-2) THEN     !BENCH HOUSING
!          HSEADD(Y,1,D)=STEOCN(CURCALYR,7,D)
!          NH(Y,1,D) =   STEOCN(CURCALYR,7,D) + &
!                        (NH(Y-1,1,D)*HDR(1))
!
!          HSEADD(Y,2,D)=STEOCN(CURCALYR,8,D)
!          NH(Y,2,D) =   STEOCN(CURCALYR,8,D) + &
!                        (NH(Y-1,2,D)*HDR(2))
!
!          HSEADD(Y,3,D)=STEOCN(CURCALYR,9,D)
!          NH(Y,3,D) =   STEOCN(CURCALYR,9,D) + &
!                        (NH(Y-1,3,D)*HDR(3))
!        ELSE             !BEYOND LAST AVAILABLE STEO YEAR USE NEMS MACRO PROJECTIONS
          HSEADD(Y,1,D)=1000000.0*MC_HUSPS1(D,Y1)
          NH(Y,1,D) =   1000000.0*MC_HUSPS1(D,Y1) + &
                        (NH(Y-1,1,D)*HDR(1))
          HSEADD(Y,2,D)=1000000.0*MC_HUSPS2A(D,Y1)
          NH(Y,2,D) =   1000000.0*MC_HUSPS2A(D,Y1) + &
                        (NH(Y-1,2,D)*HDR(2))
          HSEADD(Y,3,D)=1000000.0*MC_HUSMFG(D,Y1)
          NH(Y,3,D) =   1000000.0*MC_HUSMFG(D,Y1) + &
                        (NH(Y-1,3,D)*HDR(3))
!       END IF

 10   CONTINUE

! DEVELOP SUBTOTALS FOR REPORTING
       DO D=1,MNUMCR-2
        ALLNEW(Y,D)=0.0
        DO B=1,MNUMBLDG
         ALLNEW(Y,D)=NH(Y,B,D)
        ENDDO
       ENDDO
       DO D=1,MNUMCR-2
        HHSTOCKBYDIV(CURCALYR,D)=0.0
        DO B=1,MNUMBLDG
         HHSTOCKBYDIV(CURCALYR,D)=(HHSTOCKBYDIV(CURCALYR,D)+NH(CURCALYR,B,D)+EH(CURCALYR,B,D))/1000000.
        ENDDO
       ENDDO

      END SUBROUTINE NEWHSE


!*******************************************************************
!     CALCULATE AVERAGE SQUARE FOOT OF HOUSING FOR RSYR = 2005-2020
!      Note, not called in the RECS year, so if this is the first year after
!       RECS, some initial RECS year calculations are also done.
!*******************************************************************
      SUBROUTINE SQFTCALC
      IMPLICIT NONE
      REAL*4 SQFTTOT(RECSYEAR:ENDYR),RENOVATE
      INTEGER  Y, B, D, E, F, IUNIT1,T,Y1,Y2,V,S

!     STOCSQRFOOT is output to the database only, not used in other calculations

      Y=CURCALYR  !calendar year
      Y1=CURIYR   !NEMS index for calendar year
      RENOVATE=7.18 ! THIS IS DERIVED FROM THE % OF HOMES ADDING A ROOM X THE SIZE ADDED (1.2% X 1/3 FLOOR AREA).


! Initialize RECS Year Values
!     Process RECS year calculations if this is the first call to this routine
      IF (Y.EQ.RECSYEAR+1) THEN
        SQFTTOT(RECSYEAR)=0.0
        DO D=1,MNUMCR-2
          DO B=1,MNUMBLDG
           ! calculate total square footage in the RECS stock
           SQFTTOT(RECSYEAR)=SQFTTOT(RECSYEAR)+ SQRFOOT(RECSYEAR,B,D)*EH(RECSYEAR,B,D)
           EXSQRFOOT(RECSYEAR,B,D)=SQRFOOT(RECSYEAR,B,D)
          ENDDO !B
        ENDDO !D
        SQFTAVG(Y1-1)=SQFTTOT(RECSYEAR)/OLDHSES(RECSYEAR)
      END IF !End of RECS year calculations



       ! Calculate SQFTADJ for 5 SQFT sensitive end uses "S" loop
       !  s=1 fossil heating, s=2 electric heating, s=3 CAC, s=4 HP AC, s=5 furnace fans
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
         !For projected renovation activity, allow the square footage of existing (RECS year) houses
         ! to increase over time
         EXSQRFOOT(Y,B,D)=SQRFOOT(RECSYEAR,B,D)+(RENOVATE*(Y-RECSYEAR))
! Eliminate this code, since exsqftadj replaces sqftadj for all but new furnace fans...
!         DO S=1,5
!          ! Elastic(s,d) is the responsiveness of end uses to changes in squarefootage
!          !  See RMISC "New and Existing Elasticities for additions to floor space for heating and cooling"
!          SQFTADJ(Y,B,D,S)=(ELASTIC(S,D)*((SQRFOOT(Y,B,D)-SQRFOOT(RECSYEAR,B,D))/ &
!            SQRFOOT(RECSYEAR,B,D)))+1
!         END DO !S - SQFT sensitive end uses
        ENDDO ! B
       ENDDO ! D


!*******************************************************************
!     CALCULATE AVERAGE SQUARE FOOT IN EACH YEAR
!*******************************************************************
!
     DO B=1,MNUMBLDG
      DO D=1,MNUMCR-2
        IF (Y.EQ.RECSYEAR+1) THEN
         SQNEW(RECSYEAR,B,D)=0.
        ELSE
         SQNEW(Y-1,B,D)=0.0
         DO T=RECSYEAR+1,Y-1
          SQNEW(Y-1,B,D)= SQNEW(Y-1,B,D) + &
           ((HSEADD(T,B,D)*HDR(B)**(Y-1-T))*SQRFOOT(T,B,D))/NH(Y-1,B,D)
         ENDDO
        END IF
      ENDDO
     ENDDO
!
!*******************************************************************
!     CALCULATE STOCK AVERAGE SQUARE FOOTAGE FOR DATABASE
!*******************************************************************
! STOCKSQRFOOT for database only
     DO B=1,MNUMBLDG
      DO D=1,MNUMCR-2
        IF (Y.EQ.RECSYEAR+1) THEN
         STOCKSQRFOOT(CURCALYR-1,B,D)=EXSQRFOOT(CURCALYR-1,B,D)
         STOCKSQRFOOT(CURCALYR,B,D)=(EH(Y,B,D)*EXSQRFOOT(CURCALYR,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D)) &
                                    /(EH(Y,B,D)+HSEADD(Y,B,D))
        ELSE
         STOCKSQRFOOT(CURCALYR,B,D)=(EH(Y,B,D)*EXSQRFOOT(CURCALYR,B,D)+HSEADD(Y,B,D)*SQRFOOT(Y,B,D)+ &
                                    SQNEW(Y-1,B,D)*NH(Y-1,B,D))/(EH(Y,B,D)+NH(Y,B,D))
        END IF
      ENDDO
     ENDDO

     SQFTTOT(Y)=0.0
     DO D=1,MNUMCR-2
       DO B=1,MNUMBLDG
        SQFTTOT(Y)=SQFTTOT(Y)+ ( SQRFOOT(Y,B,D)*HSEADD(Y,B,D)  + &
          SQNEW(Y-1,B,D)*NH(Y-1,B,D)+ EXSQRFOOT(CURCALYR,B,D)*EH(Y,B,D) )
       ENDDO
     ENDDO
     SQFTAVG(Y1)= SQFTTOT(Y) / ( OLDHSES(Y)+  NEWHSES(Y) )

     END SUBROUTINE SQFTCALC



!********************************************************************
!     CALCULATE REPLACEMANT EQUIPMENT TYPE SUBROUTINE
!********************************************************************
      SUBROUTINE REPLACE(EU,R,B,RECCL,FLAG)
      IMPLICIT NONE
      REAL*4 EQCOST,CAPITAL,RETAIL,RPSHARE(MNUMRTCL)
      REAL*4 TOTSH, RETIRED,RETIREDR,EQC
      INTEGER EU,EQCSW,RECCL,RECCLSW,EQTSW,RECTYSW,B,R
      INTEGER I,Y,FLAG

      TOTSH = 0.0
      IF(FLAG.EQ.1) THEN
        !  OEQCREP is old value of EQCREP with no technology switching
        RETIRED = OEQCREP(CURCALYR,RECCL,1,R)
       ELSE
        !  OEQCRP90 is old value of EQCRP90 with only switching to FA_NG
        RETIRED = OEQCRP90(CURCALYR,RECCL,1,R)
        RETIREDR= OEQCRP90R(CURCALYR,RECCL,1,R)
      ENDIF

      DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes this enduse
        EQCSW = RTCLEQCL(RECCLSW)
        EQTSW = RTCLTYPT(RECCLSW)
        RECTYSW = 0

        DO I = RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
          IF(RTEQTYPE(I).EQ.EQTSW.AND. &
            (CURCALYR.GE.RTINITYR(I).AND.CURCALYR.LE.RTLASTYR(I))) THEN
            RECTYSW = I
            GO TO 10 ! Once RECTYSW found, get out of loop
          ENDIF
        ENDDO                           ! End I loop
        IF(RECTYSW.EQ.0) THEN
          WRITE(6,*) 'RESDMSG SUB_REPLACE: No representative equipment type ', &
           'in RSCLASS for end use = ',EU,' eq class = ',EQCSW &
          ,' eq type = ',EQTSW,' CURCALYR = ',CURCALYR
          RETURN
        ENDIF

 10   CONTINUE  ! RECTYSW found, continue

        !  If COSTTRSW = 1, use function EQCOST to compute capital and retail
        !     cost of new equipment.
        !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
        !     and retail cost of new equipment.
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTYSW,CURCALYR,"CAP")
          RETAIL = EQCOST(RECTYSW,CURCALYR,"RET")
        ELSE
          CAPITAL = RTEQCOST(RECTYSW)
          RETAIL = RTRECOST(RECTYSW)
        ENDIF

        ! Compute shares for this equipment class
        RPSHARE(EQCSW) = EXP(RTSWBIAS(RECCLSW)+ &
          RTSWBETA(RECCLSW) * &
          (LFCY(EQTSW,B,R,1)+RPINSCOST(RECCL,RECCLSW)))

        ! TOTSH = TOTAL SHARES FOR ALL EQCSW's FOR THIS EQUIPMENT CLASS
        TOTSH = TOTSH+ RPSHARE(EQCSW)
      ENDDO                             ! End RECCLSW loop


      ! NORMALIZE SHARES FOR THOSE WHO SWITCH TECHNOLOGIES
      DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQCSW = RTCLEQCL(RECCLSW)
        IF(TOTSH.GT.0.0)THEN
          RPSHARE(EQCSW)=RPSHARE(EQCSW)/TOTSH
        ELSE
          RPSHARE(EQCSW) = 0.0
        ENDIF

        ! SHARE OUT REPLACEMENTS FOR THOSE WHO SWITCH TECHNOLOGIES
        !  RTSWFACT(RECCL)=SWITCHING FACTOR
        IF(FLAG.EQ.1) THEN
        !  Flag = 1 calculate replacements for post-2005 homes
            EQCREP(CURCALYR,RECCLSW,B,R) = (EQCREP(CURCALYR,RECCLSW,B,R) &
               + (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL)))
          ELSE
          ! Else calculate replacements pre-2006 homes
            EQCSW90(CURCALYR,RECCL,RECCLSW,B,R) =  &
             (RETIRED * RPSHARE(EQCSW) * RTSWFACT(RECCL))
            EQCSW90R(CURCALYR,RECCL,RECCLSW,B,R) =  &
             (RETIREDR * RPSHARE(EQCSW) * RTSWFACT(RECCL))
        ENDIF
      ENDDO

      !   SUM OVER ALL TYPES FOR TOTAL SWITCHES FROM EACH TECH.
      IF (FLAG.NE.1) THEN
          SWITCHES(CURCALYR,RECCL,B,R)=0.0
          SWITCHESR(CURCALYR,RECCL,B,R)=0.0
        DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         IF (RECCLSW.NE.RECCL) THEN
          SWITCHES(CURCALYR,RECCL,B,R)=SWITCHES(CURCALYR,RECCL,B,R)+ &
             EQCSW90(CURCALYR,RECCL,RECCLSW,B,R)
          SWITCHESR(CURCALYR,RECCL,B,R)=SWITCHESR(CURCALYR,RECCL,B,R)+ &
             EQCSW90R(CURCALYR,RECCL,RECCLSW,B,R)
         END IF
        END DO
      ENDIF

      ! REPLACEMENTS FOR THOSE WHO DON'T SWITCH TECHNOLOGIES
      IF(FLAG.EQ.1) THEN
      ! Flag = 1 calculate replacements for post-2005 homes
      EQCREP(CURCALYR,RECCL,B,R) = (EQCREP(CURCALYR,RECCL,B,R)+ &
         (RETIRED*(1-RTSWFACT(RECCL))) )
       ENDIF

!      RETURN
      END SUBROUTINE REPLACE


!*******************************************************************
!     HVAC CHOICE SUBROUTINE
!       CALLED FOR CURIYR = 5,6,7,...
!*******************************************************************
      SUBROUTINE RSHVAC
      IMPLICIT NONE
!bookmark      COMMON/SHINV/SHELLINVEST(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR), &
!bookmark                   SHELLSUBSIDY(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR)
      COMMON/ESTARHOMES/HVEQWTN(RECSYEAR:ENDYR,NHeatTypes,NShellTypes,MNUMBLDG,MNUMCR)
      COMMON/SQFTSHELL/HTSQRFOOTFAC(RECSYEAR:ENDYR,NHeatTypes,MNUMCR-2,MNUMBLDG),CLSQRFOOTFAC(RECSYEAR:ENDYR,NHeatTypes,MNUMCR-2,MNUMBLDG)
      ! LOCAL VARIABLES
      REAL*4 HTSQRFOOTFAC,CLSQRFOOTFAC
!bookmark      REAL*4 SHELLINVEST,SHELLSUBSIDY
      REAL*4 DISRT,HORIZON,ACRECOST,rlearncost
      REAL*4 EQCOST,CAPITAL,RETAIL,CAPITALX
      REAL*4 HDDFACT(MNUMCR),CDDFACT(MNUMCR),HVOPCOST,HVLFCY
      REAL*4 RTEFFAC(3),DECAY,ECTEMP,DENOM,SUM,SUM1,E
      REAL*4 HTSHELLFAC(RECSYEAR:ENDYR,MNUMCENDIV,MNUMBLDG),&
             CLSHELLFAC(RECSYEAR:ENDYR,MNUMCENDIV,MNUMBLDG),SQFTWEIGHTC(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR),&
             EFFWEIGHTC(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR),EQWTNCA(RECSYEAR:ENDYR,NCoolTypes,MNUMBLDG,MNUMCR)
      REAL*4 COOLSHWT(NCoolTypes,MNUMBLDG,MNUMCR)
      REAL*4 TOTEWTN(NHeatClasses,MNUMBLDG,MNUMCR),HVEQWTN,HVBETA2A(MNUMHVAC)
      REAL*4 WTDEFF(NHeatClasses),EFFWEIGHT(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR),SQFTWEIGHT(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR)
      REAL*4 TOTEWTNC(RECSYEAR:ENDYR,5,MNUMBLDG,MNUMCR),EQWTNC(NCoolTypes,5,MNUMBLDG,MNUMCR)
      REAL*4 EQFSHRNC(NCoolTypes),SHLLEARN(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      REAL*4 EQFSHRN(31),effwt(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR),EQPEFF(NHeatTypes)
      REAL*4 weighttot(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR),TOTEFFWT(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR)
      REAL*4 EPRICE(MNUMCR-2,RECSYEAR:ENDYR),ESHR(RECSYEAR:ENDYR)
      INTEGER EU,EUPR,RECTY,RECCL,R,B,F,T,EQT,EQC,TYPE,COUNT,L,HCNT,Y,Y1,HC,EV,RECCL1
      INTEGER RECAR(7),eqtar(7),S,HVRCTY,HVC,HVT,FS,HVCC,HVCT,HVTYCNT,HE,HS,CS

     !   THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
     !     SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
     EU     = 1
     EUPR=1
     ALPHA1 = -0.50
     DO R=1,MNUMCR-2
      DO B=1,MNUMBLDG
       IF (CURCALYR.GT.RECSYEAR+1) THEN
        SHLLEARN(CURCALYR,B,R)=SHLLEARN(CURCALYR-1,B,R)*(LEARNFACT(B,R)**(CURCALYR-(RECSYEAR+1)))
       ELSE
        SHLLEARN(CURCALYR,B,R)=1.0
       END IF
      END DO
     END DO


     !   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
     DO R=1,MNUMCR-2
        !Heating price (EU=1,EUPR=1)
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
        !AC Price (EU=2,EUPR=2)
        EPRICE(R,CURCALYR)=PELRSOUT(R,CURIYR,2)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
     ENDDO

      ! COMPUTE WEATHER ADJUSTMENT FACTORS
      DO R=1,MNUMCR-2
          HDDFACT(R)=(HDDADJ(CURCALYR,R)/HDDADJ(RECSYEAR,R))**2.00
          CDDFACT(R)=(CDDADJ(CURCALYR,R)/CDDADJ(RECSYEAR,R))**1.50
      ENDDO

!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
      DISRT=0.20
      HORIZON=7.0
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF


      DO 90 R=1,MNUMCR-2   !Major loops to end of share calculations
        DO 90 B=1,MNUMBLDG
         EU=1

          ! INITIALIZE ARRAYS
          DO HVC=1,NHeatClasses
            TOTEWTN(HVC,B,R)=0.0
                WTDEFF(HVC)=0.0
            HSHELL(CURCALYR,HVC,B,R)=0.0
          ENDDO

          DO HVCC=1,5
            TOTEWTNC(CURCALYR,HVCC,B,R)=0.0
                CSHELL(CURCALYR,HVCC,B,R)=0.0
          ENDDO

            DO HVCT=1,NCoolTypes
                EQWTNCA(CURCALYR,HVCT,B,R)=0.0
            COOLSHWT(HVCT,B,R)=0.0
                EQFSHRNC(HVCT)=0.0
          END DO

          ! RSMEQP and RSCLASS Variables
          ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
          ! RTTYEUPT(EU+1) = LAST RECORD # IN END USE 1 (SPACE HEATING)
          ! RECTY          = RECORD # FROM RSMEQP FILE
          ! EQT            = EQUIPMENT TYPE NUMBER
          ! EQC            = EQUIPMENT CLASS NUMBER
          ! RECCL          = RECORD # FROM RSCLASS FILE
          ! F              = FUEL #
          DO HVRCTY=1,MNUMHVAC   ! for all records in the shell file
          IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
              ! Heating and cooling types and classes assigned by the shell input file RSMSHL
              HVT=HVHTEQTY(HVRCTY)   ! 31 heating types -- defines efficiency of equipment
              HVC=HVHTEQCL(HVRCTY)   ! 11 heating classes
              HVCT=HVCLEQTY(HVRCTY)  ! 14 cooling types -- defines efficiency of equipment
              HVCC=HVCLEQCL(HVRCTY)  ! 5 cooling classes
              S= HVPACKG(HVRCTY)     ! S = 1 to 5, 1=NoCode, 2=Code, 3=EStar, 4=Forty%, 5=Path; E* Qualified =3 + 4 + 5
              HS=HTSHEFF(HVRCTY)     ! Heating shell efficiency
                  CS=CLSHEFF(HVRCTY)     ! Cooling shell efficiency
          ! Filter Shell File for Calendar Year Availability
          IF (CURCALYR.GE.HVFYEAR(HVRCTY).AND. &
                       CURCALYR.LE.HVLYEAR(HVRCTY) ) THEN
           DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            ! Filter RSMEQP for equipment availability
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
                CURCALYR.LE.RTLASTYR(RECTY).AND.RTCENDIV(RECTY).EQ.R) THEN
              EQT=RTEQTYPE(RECTY) !31 heating types
              EQC=RTTYEQCL(RECTY) !11 classes
              RECCL=RTCLEUPT(EU)+EQC  !points to record number of heating equipment class in RSCLASS
              F    =RTFUEL(RECCL) !fuel for equipment class
              FS   =RTFUEL(HVC)   !fuel for shell class

              ! RECCL1 maps the heating equipment class to the appropriate cooling class
              !  e.g., RECCL=2,(HP) maps to RECCL1=14(HP in cooling section
              !  adding no. of heating classes(11) to the cooling class for HPs(3),etc.
              !  RECCL1 points to the appropriate cooling class record number in RSCLASS
               IF (RECCL.EQ.2) THEN
                  RECCL1=14  !HP    11 + 3 = 14
                 ELSE IF (RECCL.EQ.10) THEN
                  RECCL1=15   !GSHP 11 + 4 = 15
                 ELSE IF (RECCL.EQ.11) THEN
                  RECCL1=16   !NGHP 11 + 5 = 16
                 ELSE
                  RECCL1=13   !CAC for all other -- RECCL1=12 room air, not considered here
               END IF  !RECCL


             IF (EQT.EQ.HVT) THEN
              !  COMPUTE EFFICIENCY FACTORS USED IN COMPUTING OPERATING COST
              !   RTEFFAC(2) is used for the heating component of the shell package
              !   RTEFFAC(3) is used for the cooling component of the shell package
              SHLEVELH(CURCALYR,HVC,S,B,R)=0.0
              EQPEFF(HVT)=RTEQEFF(RECTY)
              IF(RTEQEFF(RECTY) .NE. 0.0) THEN
                rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
               ELSE
                RTEFFAC(2)=RTBASEFF(RECSYEAR,reccl)
              ENDIF
              IF (ACEFF(HVCT,CURCALYR,R) .NE. 0.0) THEN
                rteffac(3)=RTBASEFF(RECSYEAR,HVCC)/ACEFF(HVCT,CURCALYR,R)
               ELSE
                RTEFFAC(3)=RTBASEFF(RECSYEAR,HVCC)
              ENDIF

              ! COMPUTE EFFICIENCY SHELL EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              HTSHELLFAC(CURCALYR,R,B)=HTSHEFF(HVRCTY)/HTSHBASE(HVRCTY)
              CLSHELLFAC(CURCALYR,R,B)=CLSHEFF(HVRCTY)/CLSHBASE(HVRCTY)

              ! COMPUTE SQUARE FOOTAGE EFFECT FOR NEW CONSTRUCTION (CHANGES FROM INITIAL NEW CONTRUCTION VALUE)
              ! These factors are estimated from building simulations for 10% increases in floor area.
              !  The inputs are total effects and thus must be divided by 1.10 to produce a percentage
              !  change in HVAC use per percentage change in floor area.
              HTSQRFOOTFAC(CURCALYR,HVT,R,B)=1.+(((SQRFOOT(CURCALYR,B,R)/SQRFOOT(RECSYEAR+1,B,R))-1.) &
                *(HVHEATFACTOR(HVRCTY)/1.10))
              CLSQRFOOTFAC(CURCALYR,HVCT,R,B)=1.+(((SQRFOOT(CURCALYR,B,R)/SQRFOOT(RECSYEAR+1,B,R))-1.)&
                *(HVCOOLFACTOR(HVRCTY)/1.10))

             !COST TREND CALCULATIONS
             ! If COSTTRSW = 1, use function EQCOST to compute capital cost of new equipment.
             ! If COSTTRSW = 0, use constant value from RSMEQP file for capital cost of new equipment.
             IF (COSTTRSW.EQ.1) THEN
               IF (EQT.EQ.HVT) THEN
                CAPITAL=(EQCOST(RECTY,CURCALYR,"CAP")+SHELCOST(HVRCTY)+ACICOST(HVCT,CURCALYR,R))
                SHELLSUBSIDY(CURCALYR,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                SHELLSUBSIDY111D(CURCALYR,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
               END IF
              ELSE
               IF (EQT.EQ.HVT) THEN
                CAPITAL=(RTEQCOST(RECTY)+SHELCOST(HVRCTY)+ACICOST(HVCT,CURCALYR,R))
                SHELLSUBSIDY(CURCALYR,RECCL,S,B,R)=SHELSUB(HVRCTY)+SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
                SHELLSUBSIDY111D(CURCALYR,RECCL,S,B,R)=SHELSUB111D(HVRCTY)*FLOAT(EPA111D)
               ENDIF
             ENDIF !Cost Trend Calculations

              ! COMPUTE THE PART OF THE EQUIMENT CHOICE CALC NOT DEPENDENT ON REGION OR BUILDING TYPE
              SHELLINVEST(CURCALYR,RECCL,S,B,R)=CAPITAL-RTEQCOST(RECTY)-ACICOST(HVCT,CURCALYR,R)

              ! CALCULATE OPERATING COST
              HVOPCOST=PRICES(F,R,CURCALYR)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)* &
                RTEFFAC(2)*HDDFACT(R)*HTSQRFOOTFAC(CURCALYR,HVT,R,B)*HTSHELLFAC(CURCALYR,R,B)+  &
                EPRICE(R,CURCALYR)*NEWCOOLUEC(B,R)/BASELOAD(RECCL1)*              &
                RTEFFAC(3)*CDDFACT(R)*CLSQRFOOTFAC(CURCALYR,HVT,R,B)*CLSHELLFAC(CURCALYR,R,B)

              ! CALCULATE LIFE CYCLE COSTS
              HVLFCY=CAPITAL+HVOPCOST*DECAY

              ! Shell Learning for Energy Start Qualified Shells (s>2)
              IF (S.GT.2) THEN
                 ! EStar qualified (EStar, Forty & PATH)
                 ! Allow further shell improvements
                 HVBETA2A(HVRCTY)=HVBETA2(HVRCTY)*SHLLEARN(CURCALYR,B,R)
               ELSE
                ! Code homes, non-EStar
                ! Code homes do not get further learned shell improvements
                hvbeta2a(hvrcty)=hvbeta2(hvrcty)
              END IF  !S>2

              ! Benchmark Energy Star Shares for Each of the 31 HVT
              IF (B.NE.1) THEN
                 ! For multi-family and mobile homes, compute shares here
                 HVEQWTN(CURCALYR,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(HVRCTY)*HVLFCY) )
               ELSE IF ((B.EQ.1).AND.(CURCALYR.GT.2009)) THEN            ! HVAC Historical
                 ! Add 2008 to the benchmarking period for AEO 2011        HVAC Historical
                 ! If beyond the benchmarking period also compute shares here
                 HVEQWTN(CURCALYR,HVT,S,B,R)=EXP(HVBETA2A(HVRCTY)+(HVBETA1(HVRCTY)*HVLFCY) )
              END IF !B<>1

              TOTEWTN(HVC,B,R)=TOTEWTN(HVC,B,R)+HVEQWTN(CURCALYR,HVT,S,B,R)
              EQWTNCA(CURCALYR,HVCT,B,R)=EQWTNCA(CURCALYR,HVCT,B,R)+HVEQWTN(CURCALYR,HVT,S,B,R)
              COOLSHWT(HVCT,B,R)=COOLSHWT(HVCT,B,R)+HVEQWTN(CURCALYR,HVT,S,B,R)*CLSHEFF(HVRCTY)
              TOTEWTNC(CURCALYR,HVCC,B,R)=TOTEWTNC(CURCALYR,HVCC,B,R)+HVEQWTN(CURCALYR,HVT,s,B,R)

             END IF  !Calculations for EQT = HVT


            END IF  !Filter all RSMEQP records (RECTY) for Current Year and Division

           ENDDO   !Do all RSMEQP records

           ENDIF  !Filter Shells for Year
           ENDIF  !Filter Shells for Census Division and Building Type

          ENDDO  !For all shell file records


          ! Now that the first pass through the data has been made, raw accumulations
          !  of logit exponents are available for share calculations
          DO HE=1,NHeatTypes      !For all specific heating equipment types

            EQFSHRN(HE)=0.0
            HVEQSHR(CURCALYR,HE,B,R)=0.0

           DO HVRCTY=1,MNUMHVAC  !All shell file records (max 5000)

            IF (RSCENDIV(HVRCTY).EQ.R .AND. RSBTYPE(HVRCTY).EQ.B) THEN
                HVT=HVHTEQTY(HVRCTY) !31 specific types
                HVC=HVHTEQCL(HVRCTY) !11 classes
                HVCC=HVCLEQCL(HVRCTY) !5 cooling classes
                HVCT=HVCLEQTY(HVRCTY) !14 specific cooling types
                S= HVPACKG(HVRCTY)    !5 shell options
               ! Filter for year availability
               IF (CURCALYR.GE.HVFYEAR(HVRCTY).AND. &
                 CURCALYR.LE.HVLYEAR(HVRCTY) ) THEN

               !Filter for equipment match
               IF (HE.EQ.HVT) THEN  !Ignore records for other specific equipment types  !moved from below
               ! SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
               IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                 HTSHELLWT(CURCALYR,HVT,S,B,R)=HVEQWTN(CURCALYR,HVT,S,B,R)/TOTEWTN(HVC,B,R)
                 !Weighted heating shell efficiency for this specific equipment type
                 HTSHELLEFFWT(CURCALYR,HVT,S,B,R)= HTSHELLWT(CURCALYR,HVT,S,B,R)*HTSHEFF(HVRCTY)
                ELSE
                 HTSHELLEFFWT(CURCALYR,HVT,S,B,R)=0.0
                 HTSHELLWT(CURCALYR,HVT,S,B,R)=0.0
               ENDIF

               ! Calculate shell efficiency by heating class and accumulate shares
               HSHELL(CURCALYR,HVC,B,R)=HSHELL(CURCALYR,HVC,B,R)+HTSHELLEFFWT(CURCALYR,HVT,S,B,R)
               SHLEVELH(CURCALYR,HVC,S,B,R)=SHLEVELH(CURCALYR,HVC,S,B,R)+HVEQWTN(CURCALYR,HVT,S,B,R)/TOTEWTN(HVC,B,R)

               !For equipment classes with non-zero shares accumulate shares for specific equipment used by shell
                IF (TOTEWTN(HVC,B,R).GT.0.0) THEN
                  EQFSHRN(HE)=EQFSHRN(HE)+HVEQWTN(CURCALYR,HVT,S,B,R)/TOTEWTN(HVC,B,R)
                     ELSE
                  EQFSHRN(HE)=0.0
                ENDIF

                HVEQSHR(CURCALYR,HE,B,R)=EQFSHRN(HE) !Map specific equipment share into the HV array
               END IF !Filter for specific equipment match (HVT = HE)


              ENDIF !Filter for current year validity
             ENDIF  !Filter for Census Division and Building Type
          ENDDO   !All shell file records

         ENDDO    !HE, All 31 specific heating equipment types


         ! CALCULATE WEIGHTED EFFICIENCY AND WEIGHTED SQUARE FOOTAGE FACTOR FOR EACH HEATING EQUIPMENT CLASS FOR USE BELOW
         DO HVT=1,NHeatTypes
           IF(EQPEFF(HVT).gt.0.) EFFWEIGHT(CURCALYR,HVT,B,R)=HVEQSHR(CURCALYR,HVT,B,R)/EQPEFF(HVT)
! 5-21          EFFWEIGHT(CURCALYR,HVT,B,R)=HVEQSHR(CURCALYR,HVT,B,R)*EQPEFF(HVT)
           SQFTWEIGHT(CURCALYR,HVT,B,R)=HVEQSHR(CURCALYR,HVT,B,R)*HTSQRFOOTFAC(CURCALYR,HVT,R,B)

!              write(9,'("efficiency calc",4i5,2f12.4)') curcalyr,hvt, b, r, hveqshr(CURCALYR,HVT,B,R), EQPEFF(HVT)

           END DO

 90   CONTINUE  ! End Census division and Building Type Loop for Share Calculations

      ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT HEATING EQUIP
      DO 91 R=1,MNUMCR-2
       DO 91 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            COUNT =0
            ! TYPE = INDEX FOR ARRAYS NEQTSHR AND REQTSHR
            !  INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
            !  AND THEN COUNT VALID TYPES IN CURRENT END USE
            !Loop through all equipment records for this end use
                DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  !Count valid efficiency levels for this type
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)  !31 specific types for heating
                  RECAR(COUNT)=RECTY   !RSMEQP record number
                  EQTAR(COUNT)=EQT     !specific type
                  DENOM=DENOM+HVEQSHR(CURCALYR,EQT,B,R)
                ENDIF
              END IF
             END IF
            ENDDO

            ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFHV(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
              WTEQCSQFHV(CURCALYR,RECCL,B,R)=1.0
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+EFFWEIGHT(CURCALYR,TYPE,B,R)
                SUM1=SUM1+SQFTWEIGHT(CURCALYR,TYPE,B,R)
              ENDDO
              !SYSTEM-WEIGHTED SQUARE FOOTAGE UEC ADJUSTMENT FACTORS
              !  (DERIVED FROM RSMSHL HEATING ADJUSTMENT FACTORS)
              WTEQCSQFHV(CURCALYR,RECCL,B,R)=SUM1/DENOM
              !SYSTEM-WEIGHTED CLASS EFFICIENCY
! 5-21 don't invert here it is used as an inverse  WTEQCEFFHV(CURCALYR,RECCL,B,R)=(1/SUM)/DENOM
              WTEQCEFFHV(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF

!              write(9,'("final efficiency calc",4i5,f12.4)') curcalyr,reccl, b, r, WTEQCEFFHV(CURCALYR,RECCL,B,R)

          ENDDO
 91     CONTINUE


      DO 190 B=1,MNUMBLDG
        DO 190 R=1,MNUMCR-2
         DO HC=4,NCoolTypes !skips over room air conditioners
          EQFSHRNC(HC)=0.0
          NEQTSHRC(CURCALYR,HC,B,R)=0.0
          IF (HC.LT.8)               HVCC=2 !central AC
          IF (HC.GT.7.AND.HC.LT.12)  HVCC=3 !HP
          IF (HC.GT.11.AND.HC.LT.14) HVCC=4 !GSHP
          IF (HC.EQ.14)              HVCC=5 !natural gas HP

          IF (TOTEWTNC(CURCALYR,HVCC,B,R).GT.0.0) THEN
             CLSHELLWT(CURCALYR,HC,B,R)=COOLSHWT(HC,B,R)/TOTEWTNC(CURCALYR,HVCC,B,R)
           ELSE
             CLSHELLWT(CURCALYR,HC,B,R)=0.0
          ENDIF
          CSHELL(CURCALYR,HVCC,B,R)=CSHELL(CURCALYR,HVCC,B,R)+CLSHELLWT(CURCALYR,HC,B,R)

          IF (TOTEWTNC(CURCALYR,HVCC,B,R).GT.0.0) THEN
            EQFSHRNC(HC)=EQWTNCA(CURCALYR,HC,B,R)/TOTEWTNC(CURCALYR,HVCC,B,R)
           ELSE
            EQFSHRNC(HC)=0.0
          ENDIF
          NEQTSHRC(CURCALYR,HC,B,R)=EQFSHRNC(HC)
         ENDDO !HC=4,14

         DO HVCT=4,NCoolTypes
           IF(ACEFF(HVCT,CURCALYR,R).gt.0.) EFFWEIGHTC(CURCALYR,HVCT,B,R)=NEQTSHRC(CURCALYR,HVCT,B,R)/ACEFF(HVCT,CURCALYR,R)
! 5-21          EFFWEIGHTC(CURCALYR,HVCT,B,R)=NEQTSHRC(CURCALYR,HVCT,B,R)*ACEFF(HVCT,CURCALYR,R)
          SQFTWEIGHTC(CURCALYR,HVCT,B,R)=NEQTSHRC(CURCALYR,HVCT,B,R)*CLSQRFOOTFAC(CURCALYR,HVCT,R,B)
         END DO

        ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT COOLING EQUIP
        EV=2
        DO RECCL=RTCLEUPT(EV)+2,RTCLEUPT(EV+1) !process cooling classes, skipping room AC
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            COUNT =0

            !     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
            !            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
            !              AND THEN COUNT VALID TYPES IN CURRENT END USE
            TYPE = RTTYPECT(EV)
            ! All records for cooling in RSMEQP, skipping the first 63 for room air
            DO RECTY=RTTYEUPT(EV),RTTYEUPT(EV+1) !all records in RSMEQP
            IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=EQT
                  DENOM=DENOM+NEQTSHRC(CURCALYR,EQT,B,R)
                ENDIF !equipment is a member of this class
             ENDIF !census division filter
            END IF !Year availability filter
            ENDDO !All records from RSMEQP for this end use

            ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFHV(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
              WTEQCSQFHV(CURCALYR,RECCL,B,R)=1.0
             ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+EFFWEIGHTC(CURCALYR,TYPE,B,R)
                SUM1=SUM1+SQFTWEIGHTC(CURCALYR,TYPE,B,R)
              ENDDO
              WTEQCSQFHV(CURCALYR,RECCL,B,R)=SUM1/DENOM
              ! 5-21 since used as an inverse, don't invert numerator
              WTEQCEFFHV(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF

            IF (WTEQCEFFHV(CURCALYR,RECCL,B,R).EQ.0.0) THEN
              ! SHOULDN'T BE HERE!
              write(9,'("NOTE: should not see this msg!",3i5)') curcalyr,reccl,eqt
              WTEQCEFFHV(CURCALYR,RECCL,B,R)= 1/RTBASEFF(RECSYEAR,RECCL)
            END IF
        ENDDO !all classes of equipment for this end use

 190  CONTINUE

      END SUBROUTINE RSHVAC


!*******************************************************************
!     HEATING CHOICE SUBROUTINE
!       CALLED FOR CURIYR = 5,6,7,...
!*******************************************************************
      SUBROUTINE RHTRTEC
      IMPLICIT NONE
      COMMON/TESTHT/HTYSSHR(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR)
      ! LOCAL VARIABLES
      REAL*4 TOTEWTN(NHeatClasses,MNUMBLDG,MNUMCR)
      REAL*4 TOTEWTR(NHeatClasses,MNUMBLDG,MNUMCR)
      REAL*4 EQWTN(NHeatTypes,MNUMBLDG,MNUMCR),EQWTR(NHeatTypes,MNUMBLDG,MNUMCR)
      REAL*4 HEATSYS(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR-2),SYSTOT
      REAL*4 DISRT,HORIZON
      REAL*4 EQCOST,CAPITAL,RETAIL,CAPITAL1
      REAL*4 HDDFACT(MNUMCR)
      REAL*4 EQFSHRR,EQFSHRN,OPCOST(3),BLDRWT
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,e
      REAL*4 HTYSSHR,OTSHRT,HSYSTOT,LAGFACTOR,tmplogit
     ! These variables are involved in the efficiency choice calculation
     !  RECAR and EQTAR are dimensioned for the number of choices
     !  across efficiency types in any single year (current max=5).
     INTEGER EU,EUPR,RECTY,RECCL,R,B,F,EQT,EQC,TYPE,COUNT,L
     INTEGER RECAR(7),eqtar(7)

     ! THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
      EU     = 1 !space heating end use
      EUPR=1
      ALPHA1 = -0.50
      BLDRWT=6.0 ! FACTOR TO DISCOUNT FUEL PRICE IMPACT IN BUILDERS FUEL CHOICE DECISION

      ! MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO


      ! COMPUTE HDDFACT
      DO R=1,MNUMCR-2
          HDDFACT(R)=(HDDADJ(CURCALYR,R)/HDDADJ(RECSYEAR,R))**2.00
      ENDDO

      ! COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
      !  (FIRST ITERATION ONLY)
      IF(CURITR.EQ.1) THEN
        DISRT=0.2
        HORIZON=7.0
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF

      !  OUTTER LOOPS ARE CENSUS DIVISION AND BUILDING TYPE
      DO 90 R=1,MNUMCR-2
        DO 90 B=1,MNUMBLDG

          ! INITIALIZE ARRAYS
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            TOTEWTR(RECCL,B,R)=0.0
            TOTEWTN(RECCL,B,R)=0.0
          ENDDO
          ! VARIABLES USED THIS SECTION:
          ! RSMEQP and RSCLASS Variables
          ! RTTYEUPT(EU)   = 0 FOR EU=1 (BEFORE BEGINNING OF FILE)
          ! RTTYEUPT(EU+1) = LAST RECORD # IN END USE 1 (SPACE HEATING)
          ! RECTY          = RECORD # FROM RSMEQP.TXT FILE
          ! EQT            = EQUIPMENT TYPE NUMBER FROM RSMEQP.TXT FILE
          ! EQC            = EQUIPMENT CLASS NUMBER FROM RSMEQP.TXT FILE
          ! RECCL          = RECORD # FROM RSCLASS.TXT FILE
          ! F              = FUEL # FROM RSCLASS.TXT FILE
          ! RTEQEFF(RECTY) = SPECIFIC EQUIPMENT EFFICIENCY FROM RSMEQP.TXT FILE
          ! EQCEFF(Y,RECCL)= FORECAST RETIRING EFFICIENCY FROM RSEFF01.TXT (computed in vintaging workbook)
          ! RTBASEFF(RECSYEAR,RECCL) = AVERAGE STOCK EFFICIENCY FROM RSCLASS.TXT FILE
          ! BASELOAD (RECCL) = STANDARD LEVEL EFFICIENCY FOR HVAC (THROUGH RECCL=16) FROM RMISC.TXT

          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1) !for all RSMEQP records this enduse
            ! Filter for year availability
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             ! Filter for census division
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)

              ! COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                ! rteffac(1) is used to adjust UECs for replacements from the original stock of equipment from RECSYEAR
                rteffac(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)    !eqceff is retiring stock efficiency
                ! rteffact(2) is used to adjust RECSYEAR UECs for new construction decisions
                rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)  !rtbaseff is stock efficiency at RECSYEAR
               ELSE
                write(9,'("shouldnt see",3i5,3e15.4)') curcalyr,reccl,eqt,eqceff(curcalyr,reccl),rteqeff(recty),rtbaseff(recsyear,reccl)
                RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
              ENDIF

              ! SET CAPITAL COSTS
              !  If COSTTRSW = 1, use function EQCOST to compute capital
              !     cost of new equipment.
              !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
              !     cost of new equipment.
              IF (COSTTRSW.EQ.1) THEN
                CAPITAL =  EQCOST(RECTY,CURCALYR,"CAP")
                IF ((EQC.NE.2).AND.(EQC.NE.10).AND.(EQC.NE.11)) THEN
                   ! If not a HP technology, then add typical
                   !  cost for central air conditioning
                   CAPITAL1= RTRECOST(RECTY)+1700.  !in 2013 dollars, updated for AEO2015 tech updates
                  ELSE
                   CAPITAL1= RTRECOST(RECTY)
                END IF
               ELSE
                CAPITAL =  RTEQCOST(RECTY)
                IF ((EQC.NE.2).AND.(EQC.NE.10).AND.(EQC.NE.11)) THEN
                  ! If not a HP technology, then add typical
                  !  cost for central air conditioning
                  CAPITAL1= RTRECOST(RECTY)+1700.  !in 2013 dollars, updated for AEO2015 tech updates
                 ELSE
                  CAPITAL1= RTRECOST(RECTY)
                END IF
              ENDIF

              ! CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
              !  i.e., reduce implicit discount rates as real prices increase
              IF ((CURCALYR.GT.2008).AND. &
                (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
                HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
                ELIGBLE=HRDRATE - 0.07
                IF (ELIGBLE.GT.0.0) THEN
                  HRDADJ= ELIGBLE * &
                    ((PRICES(F,R,CURCALYR)/PRICES(F,R,RECSYEAR))**ALPHA1 )
                  BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
                 ELSE
                  BETA1DR(RECTY)=RTECBTA1(RECTY)
                END IF
               ELSE
               BETA1DR(RECTY)=RTECBTA1(RECTY)
              END IF

              ! COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT
              !       ON REGION AND BUILDING TYPE
              ECTEMP = RTECBIAS(RECTY) + (BETA1DR(RECTY)*CAPITAL)

             ! CALCULATE OPERATING COST FOR 3 DECISION TYPES
             ! UECS: EQCUEC = RECSYEAR UEC FROM RSUEC.TXT
             ! NEWHEATUEC = NEW UECS READ IN FROM RMISC.TXT NOT YEAR DEPENDENT, SO ADJUST BELOW
             ! EQCAHVUEC =

             IF (CURCALYR.EQ.RECSYEAR+1) THEN
                !prices x original RECS uec x efficiency adjustment x hddadj
                OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)

                ! FOR BUILDER CHOICE IN NEW CONSTRUCTION: DILUTE OPCOST USING BLDRWT
                !prices * new construction uec from rmisc / standard efficiency * basestock efficiency / specific equip efficiency * hddadj / adj for builder
               OPCOST(2)=PRICES(F,R,CURCALYR)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
               OPCOST(3)=PRICES(F,R,CURCALYR)*NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL)*RTEFFAC(2)*HDDFACT(R)
               ELSE
               OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B)*RTEFFAC(1)*HDDFACT(R)*(EHSHELL(CURCALYR-1,F,R,B)/EHSHELL(RECSYEAR,F,R,B))
               OPCOST(2)=PRICES(F,R,CURCALYR)*(NEWHEATUEC(RECCL,B,R)/BASELOAD(RECCL))*RTEFFAC(2)*HDDFACT(R)/BLDRWT
               OPCOST(3)=PRICES(F,R,CURCALYR)*eqcahvuec(CURCALYR-1,reccl,b,r)*(ahshell(CURCALYR-1,f,r,b)/EHSHELL(RECSYEAR,F,R,B))*RTEFFAC(2)*HDDFACT(R)
             END IF

             ! CALCULATE LIFE CYCLE COSTS
             LFCY(EQT,B,R,1)=CAPITAL +  (OPCOST(1)*DECAY)  !replacement choice (eqwtr) for homeowner of house existing in RECS year (equipment costs + installation costs)
             LFCY(EQT,B,R,2)=CAPITAL1 + (OPCOST(2)*DECAY)  !new construction choice for builder, counting only a fraction of operating costs in the decision,
                                                           !  thus favoring a choice toward equipment with lower first costs (also builder does incur certain
                                                           !  installation costs that the homeowner does
             LFCY(EQT,B,R,3)=CAPITAL1 + (OPCOST(3)*DECAY)  !replacement choice (eqwtn) for homeowner in post-RECS added house

             ! COMPUTE WEIGHTS FOR REPLACEMENT EQUIPMENT TYPES
             EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + &
               (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)) )
             TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
             EQWTN(EQT,B,R)= EXP(ECTEMP+ (RTECBTA2(RECTY)*OPCOST(3)) + &
               (RTECBTA3(RECTY)*LFCY(EQT,B,R,3)) )
             TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)

             ENDIF !filter census division
            ENDIF  !filter year availability
          ENDDO    !for all RSMEQP records this enduse


          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            ! Filter for year availability
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
                CURCALYR.LE.RTLASTYR(RECTY)) THEN
             ! Filter for census division
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              ! SET EQUIPMENT CLASS (EQC) & EQUIPMENT TYPE (EQT)
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              ! SET NEW EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                  EQFSHRN=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
                ELSE
                  EQFSHRN=0.0
              ENDIF
              NEQTSHR(CURCALYR,TYPE,B,R)=EQFSHRN

              ! SET REPLACEMENT EQUIPMENT FUEL SHARES (AND NEQTSHR FOR WATER HEATING)
              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                  EQFSHRR=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
                ELSE
                  EQFSHRR=0.0
              ENDIF

              REQTSHR(CURCALYR,TYPE,B,R)=EQFSHRR

            ENDIF
           ENDIF
          ENDDO


          ! CALCULATE WEIGHTED EFFICIENCY FOR NEW AND REPLACEMENT HEATING EQUIP
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM=0
            DENOM2=0
            COUNT =0
            TYPE = RTTYPECT(EU) !initialize to last equip record, prev enduse
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
               TYPE=TYPE+1      !counting valid types
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO


           ! COMPLETE CALCULATION FOR NEW EQUIPMENT
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
            !based on if statement above should never be here!
            IF (WTEQCEFFN(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF

            ! COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF

          ENDDO

          ! COMPUTE LOGIT VALUES FOR EACH HEATING SYSTEM AND SUM OVER TYPE
          OTSHRT=0.0
          SYSTOT=0.0
          LAGFACTOR=0.9
          ! COMPUTE PERCENT ELIGIBLE FOR FUEL CHOICE SIMULATION
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              EQT=RTCLTYPT(RECCL)
              TYPE = RTTYPECT(EU) + EQT  ! not used...
              tmplogit=EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) )
              HEATSYS(CURCALYR,EQT,B,R)=LAGFACTOR*HEATSYS(CURCALYR-1,EQT,B,R) &
                          +(1-LAGFACTOR)*EXP( RTFCBIAS(RECCL,B,R)+ RTFCBETA(RECCL)*LFCY(EQT,B,R,2) )
              SYSTOT=SYSTOT+HEATSYS(CURCALYR,EQT,B,R)


          !Diagnostics only:
          IF ((CURCALYR.gt.RECSYEAR) .and. (b.eq.1) .and. (r.eq.5)) then
!             write(9,'("parm checks",3i5,3e15.4)') curcalyr,reccl,eqt,heatsys(curcalyr-1,eqt,b,r),tmplogit,heatsys(curcalyr,eqt,b,r)
          end if

          ENDDO


          ! COMPUTE NORMALIZED SHARES FOR EACH FUEL SYSTEM CHOICE
          HSYSTOT=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
              EQT=RTCLTYPT(RECCL)
              IF(SYSTOT.NE.0.0) THEN
                HTYSSHR(CURCALYR,EQC,B,R)=HEATSYS(CURCALYR,EQT,B,R)/SYSTOT
              ELSE
                HTYSSHR(CURCALYR,EQC,B,R)=0.0
              ENDIF

          ! Change this line each time SOC (formerly C25) updates  !SOC update
          IF (CURCALYR.GT.2015) HSYSSHR(CURCALYR,EQC,B,R)=HTYSSHR(CURCALYR,EQC,B,R)

          !Diagnostics only:
!          IF ((CURCALYR.gt.2006) .and. (b.eq.1) .and. (r.eq.5)) then
!              write(19,'("system shares",2i5,3e15.4)') curcalyr,eqc,htysshr(CURCALYR,EQC,B,R), &
!                   htysshr(CURCALYR-1,EQC,B,R), htysshr(CURCALYR-2,EQC,B,R)
!          end if

          END DO

90    CONTINUE

      END SUBROUTINE RHTRTEC



!*******************************************************************
!     HEATING ADDED/REPLACED SUBROUTINE
!*******************************************************************
      SUBROUTINE RHTRADD
      IMPLICIT NONE
!bookmark      COMMON/SHINV/SHELLINVEST(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR), &
!bookmark                   SHELLSUBSIDY(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR)
      REAL*4 SHARESN(NHeatTypes,MNUMBLDG,MNUMCR) !bookmark ,SHARESR(NHeatTypes,MNUMBLDG,MNUMCR) !bookmark,SHELLINVEST,SHELLSUBSIDY
      REAL*4 SWT(RECSYEAR:ENDYR),SWF(RECSYEAR:ENDYR),SA, HSR, ESR, SVRTE
      REAL*4 NEWSHELLWT(RECSYEAR:ENDYR,6,MNUMBLDG,MNUMCR),NEWADDWT(RECSYEAR:ENDYR,6,MNUMBLDG,MNUMCR)
             !newshellwt & newaddwt are dimensioned by the number of fuels reflected in RSCLASS
! bookmark      REAL*4 SHWTNUM(RECSYEAR:ENDYR,NShellTypes)
! bookmark      REAL*4 SHLEVELWT(RECSYEAR:ENDYR,NHeatClasses,NShellTypes,MNUMBLDG,MNUMCR-2)
! bookmark      REAL*4 SHWTNUMB(RECSYEAR:ENDYR,NShellTypes,MNUMBLDG)  !bookmark ,SHELLTOTALINVEST(RECSYEAR:ENDYR)
      INTEGER EQT,RECTY,TYPE,COUNT,L,EQCAR(10),RECCLSW,V
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,F,FSW,NUMEQT,T,S,Y1,E,D

      EU = 1 !Space Heating End Use Number
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            SHARESN(EQC,B,R)=0.0
!bookmark            SHARESR(EQC,B,R)=0.0
            EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
            EQCSR90(CURCALYR,RECCL,B,R)=0.0
            EQCSUR(CURCALYR,RECCL,B,R)=0.0
           IF(B.EQ.1) EQCREP(CURCALYR,RECCL,B,R) = 0.0
          ENDDO

          ! AGGREGATE HEAT EQUIP SHARES BY THE 11 GENERAL HEATER CLASSES
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              SHARESN(EQC,B,R)=HSYSSHR(CURCALYR,EQC,B,R)
!bookmark              SHARESR(EQC,B,R)=SHARESR(EQC,B,R)+ &
!bookmark                               REQTSHR(CURCALYR,TYPE,B,R)
            ENDIF
           ENDIF
          ENDDO


          ! CALCULATE HEATERS ADDED IN CURIYR-1
          !  CUMULATE SURVIVING EQUIPMENT REPLACED FOR RECS YEAR VINTAGE PRIOR TO PREVYR
          !  CUMULATE SURVIVING NEW HEATERS ADDED PRIOR TO PREVYR TO ESTIMATE NH
          !  SA REPRESENTS NH at PREVYR-1
          !  CUMULATE SURVIVING NEW HEATERS ADDED & REPLACED PRIOR TO PREVYR
          !  REPLACE EQUIP = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1) !process all classes this enduse
            EQC=RTCLEQCL(RECCL)
            EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)* &
                                    SHARESN(EQC,B,R))
            SA=0.0

            ! Calculate replacement equipment from original base year stock
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
               *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-RECSYEAR)))
             ELSE
              EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
              EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-RECSYEAR)))
            ENDIF


            ! COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
            IF(CURCALYR.GT.RECSYEAR+1) THEN
             DO Y=RECSYEAR+1,CURCALYR-1
              TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
              EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
                ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
                  EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
             ENDDO
            ENDIF


           ! CALCULATE SURVIVING REPLACEMENTS
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1 !loop previous years
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
                 ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
                   EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
                 ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                    (HSR*ESR)) ))
              ENDDO !loop previous years
            ENDIF

            ! CALCULATE REPLACEMENT HEATERS FOR NEW VINTAGE IN CURIYR-1
            !  NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
            !  NOTE: FOR NEW HOUSES (NH) - CURIYR-1 IS THE LAGGED VALUE
            !  SUBROUTINE REPLACE DISTRIBUTES REPLACEMENTS IN POST-2005
            !    SINGLE FAMILY HOMES WHEN LAST ARGUEMENT = 1
            IF(B.EQ.1) THEN
               ! First, store what replacements would have been if no switching allowed.
                 OEQCREP(CURCALYR,RECCL,1,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
               ! Call REPLACE to distribute replacements.
                 CALL REPLACE(EU,R,B,RECCL,1)
              ELSE
               !  No switching allowed in multi-family or mobile homes.
                 EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
            ENDIF

         ENDDO ! process all classes this enduse

5     CONTINUE ! Census Regions and Building Types


      ! The following call to REPLACE with final arguement = 2  distributes
      !   replacements in Existing Single Family Homes.
      B = 1
      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        OEQCRP90(CURCALYR,RECCL,B,R) = EQCRP90(CURCALYR,RECCL,1,R)
        OEQCRP90R(CURCALYR,RECCL,B,R) = EQCRP90RP(CURCALYR,RECCL,1,R)

          CALL REPLACE(EU,R,B,RECCL,2)
        ENDDO
      ENDDO

      DO  R=1,MNUMCR-2
       DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         SWITCHTO(CURCALYR,RECCL,B,R)=0.0
         SWITCHTOR(CURCALYR,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           IF (RECCLSW.NE.RECCL) THEN
             SWITCHTO(CURCALYR,RECCL,B,R)=SWITCHTO(CURCALYR,RECCL,B,R)+ &
                                        EQCSW90(CURCALYR,RECCLSW,RECCL,B,R)
             SWITCHTOR(CURCALYR,RECCL,B,R)=SWITCHTOR(CURCALYR,RECCL,B,R)+ &
                                         EQCSW90R(CURCALYR,RECCLSW,RECCL,B,R)
           ENDIF
          ENDDO
       ENDDO
      ENDDO

      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

          EQCRP90(CURCALYR,RECCL,B,R)= EQCRP90(CURCALYR,RECCL,B,R)- &
                                     SWITCHES(CURCALYR,RECCL,B,R)
          EQCRP90RP(CURCALYR,RECCL,B,R)= EQCRP90RP(CURCALYR,RECCL,B,R)- &
                                       SWITCHESR(CURCALYR,RECCL,B,R)+ SWITCHTOR(CURCALYR,RECCL,B,R) &
                                     + SWITCHTO(CURCALYR,RECCL,B,R)
        ENDDO
      ENDDO

      SWF(CURCALYR)=0.0
      SWT(CURCALYR)=0.0

      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO  R=1,MNUMCR-2
          SWT(CURCALYR)=SWT(CURCALYR)+SWITCHTO(CURCALYR,RECCL,B,R)+ &
                      SWITCHTOR(CURCALYR,RECCL,B,R)
          SWF(CURCALYR)=SWF(CURCALYR)+SWITCHES(CURCALYR,RECCL,B,R)+ &
                      SWITCHESR(CURCALYR,RECCL,B,R)
          SWTOTAL(CURCALYR,RECCL,R)= SWITCHTO(CURCALYR,RECCL,B,R)+SWITCHTOR(CURCALYR,RECCL,B,R)
          SWFTOTAL(CURCALYR,RECCL,R)=SWITCHES(CURCALYR,RECCL,B,R)+SWITCHESR(CURCALYR,RECCL,B,R)
       ENDDO
      ENDDO


       DO  R=1,MNUMCR-2
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQCND90(CURCALYR,RECCL,B,R) = EQCRP90(CURCALYR,RECCL,B,R) + &
             EQCESE(CURCALYR,RECCL,B,R) + EQCSR90(CURCALYR,RECCL,B,R)+ &
              EQCRP90RP(CURCALYR,RECCL,B,R)
         ENDDO
       ENDDO


       !   ADD TOTAL HEATERS AND COMPUTE NEW SHELL EFFICIENCY
       DO R=1,MNUMCR-2
           DO B=1,MNUMBLDG
                 DO F=1,6
              NHSHELL(CURCALYR,F,R,B)=0.0
              NEWSHELLWT(CURCALYR,F,B,R)=0.0
              NEWADDWT(CURCALYR,F,B,R)=0.0
             END DO
         END DO
       ENDDO


      DO RECCL=1,NHeatClasses
         DO S=1,NShellTypes
          DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
! bookmark              SHLEVELWT(CURCALYR,EQC,S,B,R)=0.0
             END DO
            END DO
           END DO
      END DO


      ! Compute Shell Investment
! bookmark      SHELLTOTALINVEST(CURCALYR)=0.0
      DO 32 R=1,MNUMCR-2
        DO 32 B=1,MNUMBLDG
          DO 32 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            IF (RECCL.LE.2.OR.RECCL.EQ.10) F=4
                IF (RECCL.EQ.3.OR.RECCL.EQ.4.OR.RECCL.EQ.11) F=3
                IF (RECCL.EQ.5) F=5
                IF (RECCL.EQ.6) F=2
            IF (RECCL.EQ.7.OR.RECCL.EQ.8) F=1
            IF (RECCL.EQ.9) F=6
            EQC=RTCLEQCL(RECCL)
            HEATOT(CURCALYR,EQC,B,R)=  EQCESE(CURCALYR,RECCL,B,R)+ &
              EQCRP90(CURCALYR,RECCL,B,R)+EQCSR90(CURCALYR,RECCL,B,R)+ &
              EQCADD(CURCALYR,RECCL,B,R) +EQCREP(CURCALYR,RECCL,B,R) + &
              EQCSUR(CURCALYR,RECCL,B,R)+EQCRP90RP(CURCALYR,RECCL,B,R)

            NEWSHELLWT(CURCALYR,F,B,R)=NEWSHELLWT(CURCALYR,F,B,R)+ &
              EQCADD(CURCALYR,RECCL,B,R)*HSHELL(CURCALYR,RECCL,B,R)
              NEWADDWT(CURCALYR,F,B,R)=NEWADDWT(CURCALYR,F,B,R)+EQCADD(CURCALYR,RECCL,B,R)
              DO 32 S=1,NShellTypes
! bookmark               SHLEVELWT(CURCALYR,EQC,S,B,R)= &
! bookmark                 EQCADD(CURCALYR,EQC,B,R)*SHLEVELH(CURCALYR,EQC,S,B,R)
! bookmark               SHELLTOTALINVEST(CURCALYR)=SHELLTOTALINVEST(CURCALYR) + &
! bookmark                 SHLEVELWT(CURCALYR,EQC,S,B,R)*SHELLINVEST(CURCALYR,EQC,S,B,R)
 32   CONTINUE ! End loop compute shell investment


      DO S=1,NShellTypes
! bookmark        SHWTNUM(CURCALYR,S)=0.0
          DO B=1,MNUMBLDG
! bookmark           SHWTNUMB(CURCALYR,S,B)=0.0
             DO R=1,MNUMCR-2
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
! bookmark             SHWTNUM(CURCALYR,S)=SHWTNUM(CURCALYR,S)+SHLEVELWT(CURCALYR,RECCL,S,B,R)
! bookmark             SHWTNUMB(CURCALYR,S,B)=SHWTNUMB(CURCALYR,S,B)+SHLEVELWT(CURCALYR,RECCL,S,B,R)
            END DO !reccl
           END DO !r
            END DO !b
        END DO !s

      ! Initialize Energy Star Shells (by assumption for higher efficiencies)
! bookmark      CUMSHWTNUM(RECSYEAR,3)=57515.
! bookmark      CUMSHWTNUM(RECSYEAR,4)=5752.
! bookmark      CUMSHWTNUM(RECSYEAR,5)=575.
! bookmark      SHWTNUM(RECSYEAR,3)=27359.
! bookmark      SHWTNUM(RECSYEAR,4)=2736.
! bookmark      SHWTNUM(RECSYEAR,5)=274.

      DO S=3,5
!bookmark       CUMSHWTNUM(CURCALYR,S)=CUMSHWTNUM(CURCALYR-1,S)+SHWTNUM(CURCALYR,S)
      END DO


      ! COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE
      DO R=1,MNUMCR-2
           DO B=1,MNUMBLDG
                  DO F=1,6
               IF (NEWADDWT(CURCALYR,F,B,R).GT.0.0) THEN
                   NHSHELL(CURCALYR,F,R,B)=NEWSHELLWT(CURCALYR,F,B,R)/NEWADDWT(CURCALYR,F,B,R)
               ELSE
                     NHSHELL(CURCALYR,F,R,B)=1.0
                 END IF
                   NHSHELL(RECSYEAR,F,R,B)=NHSHELL(RECSYEAR+1,F,R,B)
             END DO
         END DO
        ENDDO


      DO D=1,MNUMCR-2
           DO B=1,MNUMBLDG
          ! Adjustments changes to house size (cooling adjustments in positions 3 & 4 done in RCLADD)
          !  fossil heating
          EXSQFTADJ(CURCALYR,B,D,1)=(ELASTIC(1,D)*((EXSQRFOOT(CURCALYR,B,D)-EXSQRFOOT(RECSYEAR,B,D))/ &
                        EXSQRFOOT(RECSYEAR,B,D))*NHSHELL(CURCALYR,3,D,B))+1
          !  electric heating
          EXSQFTADJ(CURCALYR,B,D,2)=(ELASTIC(2,D)*((EXSQRFOOT(CURCALYR,B,D)-EXSQRFOOT(RECSYEAR,B,D))/ &
                        EXSQRFOOT(RECSYEAR,B,D))*NHSHELL(CURCALYR,4,D,B))+1
          !  furnace fans
          EXSQFTADJ(CURCALYR,B,D,5)=(ELASTIC(5,D)*((EXSQRFOOT(CURCALYR,B,D)-EXSQRFOOT(RECSYEAR,B,D))/ &
                        EXSQRFOOT(RECSYEAR,B,D))*NHSHELL(CURCALYR,3,D,B))+1
         END DO
      END DO

        DO B=1,MNUMBLDG
        DO R=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
              DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R)*ESR*HSR)
                EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R)*ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R)*ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R)*ESR*HSR)
              ENDDO
          ENDDO
        ENDDO
      ENDDO

      ! AGGREGATE HEATING SYSTEMS FOR INVESTMENT ANALYSIS
      T=CURCALYR
      Y=CURIYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

        DO B=1,MNUMBLDG
         DO r=1,MNUMCR-2
          TYPE = RTTYPECT(EU)
           DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)

           ! CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR
           IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN

            TYPE=TYPE+1    ! INDEX to count the 'TYPE' records in RSMEQP
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(T,EQT,B,R,1)=(HVEQSHR(T,EQT,B,r)*EQCADD(T,RECCL,B,r))
                HEATINGTYPEPURCH(T,EQT,B,R,2)=(NEQTSHR(T,EQT,B,r)*(EQCREP(T,RECCL,B,r) + &
                   EQCRP90RP(T,RECCL,B,r)) + REQTSHR(T,EQT,B,r)*EQCRP90(T,RECCL,B,r) )
               DO S=1,NShellTypes
                SHELLBUILDS(T,EQT,S,B,R)=HTSHELLWT(T,EQT,S,B,R)*EQCADD(T,RECCL,B,r)
!bookmark                shellinvdb(T,EQT,S,B,R)=shellinvest(t,reccl,s,b,r)*ifix(shellbuilds(T,EQT,S,B,R))
!bookmark                shellsubdb(T,EQT,S,B,R)=shellsubsidy(t,reccl,s,b,r)*ifix(shellbuilds(T,EQT,S,B,R))
               ENDDO
           ENDIF
           endif
           ENDDO
         ENDDO
        ENDDO

         ! RSGASCUST tracks the number of gas customers by looking across end uses.
         !  Note: it is not a constraint on hookups...
         DO R=1,MNUMCR-2
           RSGASCUST(CURCALYR,R)=0.0
         END DO

        ! Proxy for gas customers is gas heating for CDs 3-6 and 8
        !  special treatment for these CDs, check when updating to RECS2009
        IF ((CURCALYR.EQ.RECSYEAR+1).and.(CURITR.EQ.1)) THEN
         DO R=1,MNUMCR-2
          RSGASCUST(RECSYEAR,R)=0.0
          DO B=1,MNUMBLDG
           RSGASCUST(RECSYEAR,R)=RSGASCUST(RECSYEAR,R)+EQCESE(RECSYEAR,3,B,R)+EQCESE(RECSYEAR,4,B,R)+EQCESE(RECSYEAR,11,B,R)
          END DO
         END DO
        ELSE
         DO R=1,MNUMCR-2
          DO B=1,MNUMBLDG
           RSGASCUST(CURCALYR,R)=RSGASCUST(CURCALYR,R)+HEATOT(CURCALYR,3,B,R)+HEATOT(CURCALYR,4,B,R)+HEATOT(CURCALYR,11,B,R)
          END DO
         END DO
        END IF

       END SUBROUTINE RHTRADD

!*******************************************************************
!     HEATING CONSUMPTION SUBROUTINE
!*******************************************************************
      SUBROUTINE RHTRCON
      IMPLICIT NONE
      REAL*4 NFANUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),EFANUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 ENUMER(MNUMCR,MNUMBLDG), EDENOM(MNUMCR,MNUMBLDG), &
       NDENOM(MNUMCR,MNUMBLDG),ANUM(MNUMFUEL,MNUMCR,MNUMBLDG), &
       ADEN(MNUMFUEL,MNUMCR,MNUMBLDG), NNUMER(MNUMCR,MNUMBLDG)
      REAL*4 HDDFACT(MNUMCR),TEMP,TEMP1,TEMP2, NFANIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 FANEFF(RECSYEAR:ENDYR),NFANEFF(RECSYEAR:ENDYR),ALPHA,ef1,ef2,ef3,alpha2,rbn,rbr,rba,AFANUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      INTEGER F, F2, FCON, Q, V, YEAR,FAN,E,T,y1
      INTEGER EU,EUPR,EQC,RECCL,Y,R,B,EQCEHP,EQCGHP,D,S
      INTEGER RECCLEHP, RECCLGHP
! PRICES 1=DIS 2=LPG 3=NG 4=EL 5=Ker 6=Coal
! SHELL 1=DIS/WOOD 2=LPG 3=NG 4=EL/GEOTHERMAL 5=KER
!*******************************************************************
!  F    = FUEL NUMBER FROM RSCLASS FILE
!  FCON = FUEL NUMBER FOR CONSUMPTION AND DIAMONDS (AS FOLLOWS)
!         1=NG 2=El 3=DIS 4=LPG 5=Ker 6=Wood 7=GEOTHERMAL
!*******************************************************************
!
!  SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!
      EU    =  1
      EUPR=1
      ALPHA=-.15; ef1=.5;ef2=.35;ef3=.15 !own-price elasticity and distribution
      ALPHA2=-.05  !heating shell adjustment

!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EU)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
      DO R=1,MNUMCR-2
        IF(CURCALYR.LE.IJUMPCALYR)THEN
          HDDFACT(R)=(HDDADJ(CURCALYR,R)/HDDADJ(RECSYEAR,R))**2.0
        ENDIF
      ENDDO
!
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
       DO 1 Y=RECSYEAR,ENDYR
        IF (Y.LE.2018) THEN                     ! Furnace fan standard effective 2019
         NFANEFF(Y)=1.0
         FANEFF(Y)=1.0
        ELSE
         NFANEFF(Y)=0.75
         FANEFF(Y)=1.0
        ENDIF
 1    CONTINUE
      ENDIF

       IF (CURITR.EQ.1) THEN                     !ebe - moved HDD adjustment to consumption equations instead of here
       DO 2 R=1,MNUMCR-2
         DO 2 B=1,MNUMBLDG
          EFANUEC(CURCALYR,R,B)=FANUEC(R,B)*FANEFF(CURCALYR)*EXSQFTADJ(CURCALYR,B,R,5)
            FANIUEC(R,B)=FANUEC(R,B)*FANEFF(CURCALYR)
 2     CONTINUE
      END IF

       DO 3 R=1,MNUMCR-2
         DO 3 B=1,MNUMBLDG
          NFANUEC(CURCALYR,R,B)=FANUEC(R,B)*NFANEFF(CURCALYR)* &
                               exSQFTADJ(CURCALYR,B,R,5)
          NFANIUEC(CURCALYR,R,B)=FANUEC(R,B)*NFANEFF(CURCALYR)  ! INTENSITY UEC FOR INDEX
 3     CONTINUE
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*****************************************************************************
!  CALCULATE EXISTING HEATING & COOLING SHELL INDICES - before weatherization
!*****************************************************************************
        DO 5 R=1,MNUMCR-2
          DO 5 B=1,MNUMBLDG
            DO F=1,MNUMFUEL-2
              EHSHELL(CURCALYR,F,R,B)=0.0
              IF (F.NE.6) THEN  ! NO PRICE RESPONSE FOR WOOD
                EHSHELL(CURCALYR,F,R,B)=EHSHELL(RECSYEAR,F,R,B)*RSELAST(F,R,ALPHA2,EF1,EF2,EF3,RECSYEAR,EUPR)&
                 -TECHG(CURCALYR,R,B)
               ELSE  ! wood heated
                EHSHELL(CURCALYR,F,R,B)=EHSHELL(RECSYEAR,F,R,B)-TECHG(CURCALYR,R,B)
              END IF
            Enddo

            ! Compute composite cooling shell by R & B based on 3 fuels
            ECSHELL(CURCALYR,R,B)=ECSHELL(RECSYEAR,R,B)- &
            ((EHSHELL(RECSYEAR,1,R,B)-EHSHELL(CURCALYR,1,R,B))*0.1 + &
             (EHSHELL(RECSYEAR,3,R,B)-EHSHELL(CURCALYR,3,R,B))*0.6 + &
             (EHSHELL(RECSYEAR,4,R,B)-EHSHELL(CURCALYR,4,R,B))*0.3 )*0.38

 5    CONTINUE

!*******************************************************************
!  Add weatherization (read in from rmisc)
!*******************************************************************
        DO 10 R=1,MNUMCR-2
          DO 10 B=1,MNUMBLDG
            DO 9 F=1,MNUMFUEL-2
!   ZERO OUT ARRAYS
!
         ANUM(F,R,B)=0.0
         ADEN(F,R,B)=0.0
         EHSHELL(CURCALYR,F,R,B)=EHSHELL(CURCALYR,F,R,B)+(WTHRZTN(CURCALYR,1,R))
         IF (EHSHELL(CURCALYR,F,R,B) .GT. EHSHELL(CURCALYR-1,F,R,B)) &
          EHSHELL(CURCALYR,F,R,B)=EHSHELL(CURCALYR-1,F,R,B)
         IF(EHSHELL(CURCALYR,F,R,B).LT.LIMIT) EHSHELL(CURCALYR,F,R,B)=LIMIT
  9   CONTINUE
         ECSHELL(CURCALYR,R,B)=ECSHELL(CURCALYR,R,B)+(WTHRZTN(CURCALYR,2,R))
          IF (ECSHELL(CURCALYR,R,B) .GT. ECSHELL(CURCALYR-1,R,B)) &
            ECSHELL(CURCALYR,R,B)=ECSHELL(CURCALYR-1,R,B)
          IF(ECSHELL(CURCALYR,R,B).LT.LIMIT) ECSHELL(CURCALYR,R,B)=LIMIT
 10   CONTINUE

!*******************************************************************
!  CALCULATE AVERAGE NEW HEATING SHELL INDEX
!*******************************************************************
        DO 50 R=1,MNUMCR-2
         DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            IF (CURCALYR.GT.RECSYEAR+1) THEN
            DO Y=RECSYEAR+1,CURCALYR-1
            ANUM(F,R,B)=ANUM(F,R,B)+(NHSHELL(Y,F,R,B)* &
              EQCADD(Y,RECCL,B,R))
            ADEN(F,R,B)=ADEN(F,R,B)+(EQCADD(Y,RECCL,B,R))
             END DO
            END IF
 50   CONTINUE
!*******************************************************************
        DO 55 R=1,MNUMCR-2  ! NO SHELL FOR COAL (7)
         DO 55 B=1,MNUMBLDG
          DO 55 F=1,MNUMFUEL-2  ! NO SHELL FOR COAL (7)
          IF (ADEN(F,R,B).LE.0) THEN
             AHSHELL(CURCALYR,F,R,B)=AHSHELL(CURCALYR-1,F,R,B)
          ELSE
             AHSHELL(CURCALYR,F,R,B)=ANUM(F,R,B)/ADEN(F,R,B)
          END IF
!         IF(AHSHELL(CURCALYR,F,R,B).GT.AHSHELL(CURCALYR-1,F,R,B)) &
!             AHSHELL(CURCALYR,F,R,B)=AHSHELL(CURCALYR-1,F,R,B)
         IF(AHSHELL(CURCALYR,F,R,B).LT.LIMIT) AHSHELL(CURCALYR,F,R,B)=LIMIT
         IF(CURCALYR.LE.(RECSYEAR+1)) THEN
          AHSHELL(CURCALYR,F,R,B)=NHSHELL(CURCALYR,F,R,B)
         ENDIF
 55   CONTINUE
!*******************************************************************
!  CALCULATE NEW, REPLACEMENT AND AVERAGE UECS
!*******************************************************************
      DO 80 R=1,MNUMCR-2
        DO 80 B=1,MNUMBLDG
          DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            FAN=RTFFAN(RECCL)
            F  =RTFUEL(RECCL)
            !S IS USED IN THE EXSQFTADJ CALCULATION AND DIFFERS DEPENDING ON
            !  WHETHER THE FUEL FOR HEATING IS ELECTRIC (2) OR FOSSIL (1)
            !  SEE RMISC FOR "ELASTIC" INPUTS THAT MODIFY THE SQFT ADJUSTMENTS
            !  BY TYPE OF FUEL (THERE ARE ALSO SEPERATE ADJUSTMENTS FOR
            !  CENTRAL AC (3), HP COOLING (4) AND FURNACE FANS (5)
            IF (F.EQ.4) S=2
            IF (F.NE.4) S=1
            ! RTBASEFF(CURCALYR,RECCL) IS PROJECTED STOCK EFFICIENCY OF THE SURVIVING RECSYEAR STOCK
            ! READ FROM RSSTKEFF.TXT WHICH WAS DEVELOPED BY VINTAGING PROGRAM

            !  THERE ARE 2 SETS OF UECS DEVELOPED BELOW:
            !   THE "I" IN MIDDLE OF THE UECS NAMES DENOTE INTENSITIES FOR THE RESIDENTIAL EFFICIENCY INDEX IN FTAB,
            !   LEAVING OFF SQUARE FOOTAGE ADJUSTMENT (INCREASE) TO RECOGNIZE THAT GREATER USEAGE FOR EXPANDED
            !   LIVING SPCE IS NOT AN EFFICIENCY LOSS, BUT INSTEAD SERVES INCREASED SERVICE DEMAND

            ! EQCSUEC APPLIES TO THE SURVIVING RECSYEAR STOCK
            EQCSUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)* RTBASEFF(RECSYEAR,RECCL)/RTBASEFF(CURCALYR,RECCL) &
              *EXSQFTADJ(CURCALYR,B,R,S)
            EQCSIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)* RTBASEFF(RECSYEAR,RECCL)/RTBASEFF(CURCALYR,RECCL)

            ! EQCNUEC APPLIES TO NEW REPLACEMENT EQUIPMENT IN NEW CONSTRUCTION (I.E. ADDED AFTER THE RECSYEAR)
            IF (WTEQCEFFN(CURCALYR,RECCL,B,R).GT.0.0) THEN
             EQCNUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)* WTEQCEFFN(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL)
             EQCNIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)* WTEQCEFFN(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL)
             ELSE
              EQCNUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)
              EQCNIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)
            END IF

            ! EQCHVUEC APPLIES TO NEW CONSTRUCTION THIS YEAR
            IF (WTEQCEFFHV(CURCALYR,RECCL,B,R).GT.0.0) THEN
              EQCHVUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)* WTEQCEFFHV(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL) &
                *WTEQCSQFHV(CURCALYR,RECCL,B,R)
              EQCHVIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)* WTEQCEFFHV(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL)
             ELSE
              EQCHVUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)* &
               WTEQCSQFHV(CURCALYR,RECCL,B,R)
              EQCHVIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)    ! INTENSITY UEC FOR INDEX
            END IF

            ! EQCRUEC APPLIES TO REPLACEMENTS OF EQUIPMENT THIS YEAR FROM HOUSING EXISTING IN RECSYEAR
            IF (WTEQCEFFR(CURCALYR,RECCL,B,R) .GT. 0.0) THEN
              EQCRUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)* WTEQCEFFR(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL) &
                *EXSQFTADJ(CURCALYR,B,R,S)
              EQCRIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)* WTEQCEFFR(CURCALYR,RECCL,B,R)*RTBASEFF(RECSYEAR,RECCL)
             ELSE
              EQCRUEC(CURCALYR,RECCL,B,R) =EQCUEC(R,RECCL,B)*EXSQFTADJ(CURCALYR,B,R,S)
              EQCRIUEC(CURCALYR,RECCL,B,R)=EQCUEC(R,RECCL,B)
            END IF

            ! EQCAUEC IS THE AVERAGE UEC FOR EQUIPMENT IN RECSYEAR HOUSING STOCK THAT HAS BEEN REPLACED (ONCE)
            !   AND THAT BOTH THE EQUIPMENT AND HOUSE HAVE SURVIVED TO THIS YEAR
            ! EQCAHVUEC IS THE AVERAGE UEC FOR EQUIPMENT IN POST-RECSYEAR HOUSINGS STOCK
            IF (CURCALYR-1.EQ.RECSYEAR)THEN
                 EQCAUEC(CURCALYR,RECCL,B,R)=EQCNUEC(CURCALYR,RECCL,B,R)
                 EQCAHVUEC(CURCALYR,RECCL,B,R)=EQCHVUEC(CURCALYR,RECCL,B,R)
                 AFANUEC(RECSYEAR+1,R,B)=NFANUEC(RECSYEAR+1,R,B)
                ELSE
                 ! SUM ALL OF THE SURVIVING / VINTAGED EQUIPMENT FROM RECSYEAR TO YEAR PRIOR TO THIS YEAR
                 !   (EQUIPMENT STOCK NAMES WITH "FUT" APPENDED, FOR "SURVIVING IN A FUTURE YEAR")
                 TEMP=0.0
                 TEMP1=0.0
                 TEMP2=0.0
                 DO Y=RECSYEAR,CURCALYR-1
                  TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,R)+ &
                   EQR90RPFUT(Y,CURCALYR,RECCL,B,R)
                  TEMP1=TEMP1+(EQR90FUT(Y,CURCALYR,RECCL,B,R)+ &
                   EQADDFUT(Y,CURCALYR,RECCL,B,R)+EQREPFUT(Y,CURCALYR,RECCL,B,R)+ &
                   EQR90RPFUT(Y,CURCALYR,RECCL,B,R))*FAN
                  TEMP2=TEMP2+ &
                   EQADDFUT(Y,CURCALYR,RECCL,B,R)+EQREPFUT(Y,CURCALYR,RECCL,B,R)
                 ENDDO

                IF(TEMP.LE.0.0) THEN
                  EQCAUEC(CURCALYR,RECCL,B,R)=EQCNUEC(CURCALYR,RECCL,B,R)
                 ELSE
                  EQCAUEC(CURCALYR,RECCL,B,R)=0.0
                  DO Y=RECSYEAR,CURCALYR-1
                   EQCAUEC(CURCALYR,RECCL,B,R)=EQCAUEC(CURCALYR,RECCL,B,R)+( &
                    EQR90FUT(Y,CURCALYR,RECCL,B,R)*EQCRUEC(Y,RECCL,B,R)+ &
                    EQR90RPFUT(Y,CURCALYR,RECCL,B,R)*EQCNIUEC(Y,RECCL,B,R)) &
                    /TEMP
                  ENDDO
                END IF

             IF(TEMP1.LE.0.0) THEN
               AFANUEC(CURCALYR,R,B)=NFANUEC(CURCALYR,R,B)
              ELSE
               AFANUEC(CURCALYR,R,B)=0.0
               DO Y=RECSYEAR,CURCALYR-1
                AFANUEC(CURCALYR,R,B)=AFANUEC(CURCALYR,R,B)+( &
                  EQR90FUT(Y,CURCALYR,RECCL,B,R)*EFANUEC(Y,R,B)*FAN+ &
                  EQADDFUT(Y,CURCALYR,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+&
                  EQREPFUT(Y,CURCALYR,RECCL,B,R)*NFANUEC(Y,R,B)*FAN+ &
                  EQR90RPFUT(Y,CURCALYR,RECCL,B,R)*NFANUEC(Y,R,B)*FAN) &   !ebe- change from EUECto NUEC in fan calc
                  /TEMP1
               ENDDO
              END IF !TEMP1 <=0.

              IF(TEMP2.LE.0.0) THEN
                EQCAHVUEC(CURCALYR,RECCL,B,R)=EQCHVUEC(CURCALYR,RECCL,B,R)
               ELSE
               EQCAHVUEC(CURCALYR,RECCL,B,R)=0.0
               DO Y=RECSYEAR,CURCALYR-1
                 EQCAHVUEC(CURCALYR,RECCL,B,R)=EQCAHVUEC(CURCALYR,RECCL,B,R)+( &
                   EQADDFUT(Y,CURCALYR,RECCL,B,R)*EQCHVUEC(Y,RECCL,B,R)+ &
                   EQREPFUT(Y,CURCALYR,RECCL,B,R)*EQCNUEC(Y,RECCL,B,R)) &
                   /TEMP2
               ENDDO
              END IF !TEMP2 <=0.
            END IF  !CURCALYR = RECSYEAR

 80     CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 85 R=1,MNUMCR-2
        DO 85 B=1,MNUMBLDG
          DO 85 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            FAN=RTFFAN(RECCL)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
!
              WTEQCEFFA(RECSYEAR+1,RECCL,B,R)=WTEQCEFFN(RECSYEAR+1,RECCL,B,R)
            ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,R)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,R)+EQREPFUT(Y,CURCALYR,RECCL,B,R)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,R)
           ENDDO
             IF(TEMP.GT. 0.0) THEN
              WTEQCEFFA(CURCALYR,RECCL,B,R)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,R)=WTEQCEFFA(CURCALYR,RECCL,B,R)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,R)*WTEQCEFFR(Y,RECCL,B,R))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,R)*WTEQCEFFHV(Y,RECCL,B,R))+&
          (EQREPFUT(Y,CURCALYR,RECCL,B,R)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,R))*WTEQCEFFN(Y,RECCL,B,R))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,R)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,R)
              END IF
            END IF
 85   CONTINUE

!*******************************************************************
!  INITIALIZE HEATING CONSUMPTION
!*******************************************************************
      DO 90 R=1,MNUMCR-2
        DO 90 FCON=1,NHTRFL
          !FOR NEMS REPORTING
          HTRCON(CURIYR,FCON,R)=0.0
          FANCON(CURIYR,R)=0.0
         DO 90 B=1,MNUMBLDG
          !FOR FTAB EFFICIENCY CALCULATION
          HTRCONWT(CURIYR,FCON,R,B)=0.
          HTRCONIN(CURIYR,FCON,R,B)=0.
          DRIVER(CURIYR,FCON,R,B)=0.
          FANCONWT(CURIYR,R,B)=0.
          FANCONIN(CURIYR,R,B)=0.
          DRIVER2(CURIYR,R,B)=0.
 90   CONTINUE

!*******************************************************************
!  CALCULATE HEATING CONSUMPTION
!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

      ! FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEATPUMPS
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          RECCLEHP=EQCEHP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          RECCLGHP=EQCGHP
        ENDIF
      ENDDO

      DO 100 R=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
!bookmark MELS
!  this statement may be causing a problem as it zeros out RECS year faneqcn!
!         IF (CURCALYR.EQ.(RECSYEAR+1)) FANEQCN(CURIYR-1,1,B,R)=0.0
         FANEQCN(CURIYR,1,B,R)=0.0
         FANEQP(CURCALYR,B,R)=0.0
         !   F = FUEL NUMBER FROM RSCLASS FILE, F=1,2,3,4,5,6(WOOD),7(GEO)
         !   FHTRCON(F) = HTRCON FUEL NUMBER, EXCEPT FOR GEOTHERMAL WHICH IS 4 AND 7
         !   MAP RSCLASS FUEL NUMBERS INTO FHTRCON FUEL NUMBERS
         !                  FHTRCON   RSCLASS
         !    FUEL            FCON       F
         !    NATURAL GAS       1        3
         !    ELECTRICITY       2        4
         !    DISTILLATE        3        1
         !    LPG               4        2
         !    KEROSENE          5        5
         !    WOOD              6        6 (PRICED TO DISTILLATE)
         !    GEOTHERMAL        7        4 (PRICED TO ELECTRICITY)

          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            FAN=RTFFAN(RECCL)
            IF (F.EQ.4) S=2
            IF (F.NE.4) S=1
            ! MAP RTEK FUEL NUMBERS INTO NEMS FUEL NUMBERS
            FCON=FHTRCON(F)

            ! Special code for ARRA stimulus bill
            IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
               ALPHA=-0.30
             ELSE
               ALPHA=-0.15
            END IF

            ! Code to streamline wood treatment
            F2=F
            IF(F.EQ.6) ALPHA=0.50
            IF(F.EQ.6) F2=1

            ! Efficiency Rebound Effects
            IF(CURCALYR.GT.RECSYEAR+1) THEN
               RBA=(RTBASEFF(RECSYEAR,RECCL)*WTEQCEFFA(CURCALYR,RECCL,B,R))**ALPHA
               RBR=(RTBASEFF(RECSYEAR,RECCL)*WTEQCEFFR(CURCALYR,RECCL,B,R))**ALPHA
               RBN=(RTBASEFF(RECSYEAR,RECCL)*WTEQCEFFN(CURCALYR,RECCL,B,R))**ALPHA
              ELSE
               RBA=1.0
               RBR=1.0
               RBN=1.0
             ENDIF

             ! CONSUMPTION FOR NEMS OUTPUT TABLES
             HTRCON(CURIYR,FCON,R)=HTRCON(CURIYR,FCON,R)+HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CURCALYR,RECCL,B,R)*EQCSUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2)) + &
               (EQCADD(CURCALYR,RECCL,B,R)*EQCHVUEC(CURCALYR,RECCL,B,R) * &
                 (NHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CURCALYR,RECCL,B,R)*EQCRUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CURCALYR,RECCL,B,R)*EQCNIUEC(CURCALYR,RECCL,B,R)*EXSQFTADJ(CURCALYR,B,R,S)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CURCALYR,RECCL,B,R)*EQCAUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CURCALYR,RECCL,B,R) *EQCNUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CURCALYR,RECCL,B,R) *EQCAHVUEC(CURCALYR,RECCL,B,R) * &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA)*( &
                RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))

             ! WEIGHT, INTENSITY AND "DRIVER" VARIABLES FOR THE FTAB EFFICIENCY CALCULATION
             HTRCONWT(CURIYR,FCON,R,B)=HTRCONWT(CURIYR,FCON,R,B)+HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CURCALYR,RECCL,B,R)*EQCSUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2)) + &
               (EQCADD(CURCALYR,RECCL,B,R)*EQCHVUEC(CURCALYR,RECCL,B,R)* &
                 (NHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CURCALYR,RECCL,B,R)*EQCRUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CURCALYR,RECCL,B,R)*EQCNIUEC(CURCALYR,RECCL,B,R)*EXSQFTADJ(CURCALYR,B,R,S)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CURCALYR,RECCL,B,R)*EQCAUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CURCALYR,RECCL,B,R) *EQCNUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CURCALYR,RECCL,B,R) *EQCAHVUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA)*( &
                RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))

             HTRCONIN(CURIYR,FCON,R,B)=HTRCONIN(CURIYR,FCON,R,B)+((( &
               (EQCESE(CURCALYR,RECCL,B,R)*EQCSIUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCADD(CURCALYR,RECCL,B,R)*EQCHVIUEC(CURCALYR,RECCL,B,R)* &
                 (NHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCRP90(CURCALYR,RECCL,B,R)*EQCRIUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCRP90RP(CURCALYR,RECCL,B,R)*EQCNIUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCSR90(CURCALYR,RECCL,B,R)*EQCAUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCREP(CURCALYR,RECCL,B,R) *EQCNIUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))) + &
               (EQCSUR(CURCALYR,RECCL,B,R) *EQCAHVUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B)))))           )

               DRIVER(CURIYR,FCON,R,B)=DRIVER(CURIYR,FCON,R,B)+               &
                  (EQCESE(CURCALYR,RECCL,B,R)+EQCADD(CURCALYR,RECCL,B,R)+     &
                   EQCRP90RP(CURCALYR,RECCL,B,R)+EQCRP90(CURCALYR,RECCL,B,R)+ &
                   EQCSR90(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R)+    &
                   EQCSUR(CURCALYR,RECCL,B,R))

             ! CALCULATION FOR EQUIPMENT-SPECIFIC ENERGY CONSUMPTION DATABASE
             EQCEQCN(CURIYR,RECCL,B,R)= HDDFACT(R)*LEAPYR*(( &
               (EQCESE(CURCALYR,RECCL,B,R)*EQCSUEC(CURCALYR,RECCL,B,R)* &
               (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))    + &
               (EQCADD(CURCALYR,RECCL,B,R)*EQCHVUEC(CURCALYR,RECCL,B,R)* &
                 (NHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCRP90(CURCALYR,RECCL,B,R)*EQCRUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBR + &
               (EQCRP90RP(CURCALYR,RECCL,B,R)*EQCNIUEC(CURCALYR,RECCL,B,R)*EXSQFTADJ(CURCALYR,B,R,S)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSR90(CURCALYR,RECCL,B,R)*EQCAUEC(CURCALYR,RECCL,B,R)* &
                 (EHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA + &
               (EQCREP(CURCALYR,RECCL,B,R)*EQCNUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBN + &
               (EQCSUR(CURCALYR,RECCL,B,R)*EQCAHVUEC(CURCALYR,RECCL,B,R)* &
                 (AHSHELL(CURCALYR,F,R,B)/EHSHELL(RECSYEAR,F,R,B))**(1.0+ALPHA2))*RBA)* &
                  RSELAST(F2,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

             IF(F.NE.6) THEN   !ASSUMING NO FANS OR FAN CONSUMPTION FOR WOOD...  ebe Moved HDDFACT to here instead of UEC calc
               FANEQCN(CURIYR,1,B,R)= FANEQCN(CURIYR,1,B,R)+HDDFACT(R)*LEAPYR*(   &
                 EQCESE(CURCALYR,RECCL,B,R)*FAN*EFANUEC(CURCALYR,R,B)+ &
                (EQCRP90(CURCALYR,RECCL,B,R)+ EQCRP90RP(CURCALYR,RECCL,B,R))* &
                   FAN*NFANUEC(CURCALYR,R,B)+ &                             !ebe - change EQCRP, EQCRP90RP to calc w/new fan UEC
                (EQCSR90(CURCALYR,RECCL,B,R))*FAN*AFANUEC(CURCALYR,R,B)+ &
                (EQCADD(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R))* &  !ebe - Move EQCREP to calc w/new fan UEC
                   FAN*NFANUEC(CURCALYR,R,B) + &
                 EQCSUR(CURCALYR,RECCL,B,R)*FAN*AFANUEC(CURCALYR,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) ! CHANGED FROM F=3

               FANCON(CURIYR,R)=FANCON(CURIYR,R)+ HDDFACT(R)*LEAPYR* &
                (EQCESE(CURCALYR,RECCL,B,R)*EFANUEC(CURCALYR,R,B)*FAN+ &
                (EQCRP90(CURCALYR,RECCL,B,R)+EQCRP90RP(CURCALYR,RECCL,B,R))* &
                   FAN*NFANUEC(CURCALYR,R,B)+ &                             !ebe - change EQCRP, EQCRP90RP to calc w/new fan UEC
                (EQCSR90(CURCALYR,RECCL,B,R))*FAN*AFANUEC(CURCALYR,R,B)+ &
                (EQCADD(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R))* &  !ebe - Move EQCREP to calc w/new fan UEC
                   FAN*NFANUEC(CURCALYR,R,B) + &
                 EQCSUR(CURCALYR,RECCL,B,R)*FAN*AFANUEC(CURCALYR,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) ! CHANGED FROM F=3

               FANCONWT(CURIYR,R,B)= FANCONWT(CURIYR,R,B)+HDDFACT(R)*LEAPYR*( &
                 EQCESE(CURCALYR,RECCL,B,R)*EFANUEC(CURCALYR,R,B)*FAN+ &
                (EQCRP90(CURCALYR,RECCL,B,R)+EQCRP90RP(CURCALYR,RECCL,B,R))* &
                     FAN*NFANUEC(CURCALYR,R,B)+ &                           !ebe - change EQCRP, EQCRP90RP to calc w/new fan UEC
                (EQCSR90(CURCALYR,RECCL,B,R))*FAN*AFANUEC(CURCALYR,R,B)+ &
                (EQCADD(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R))* &  !ebe - Move EQCREP to calc w/new fan UEC
                     FAN*NFANUEC(CURCALYR,R,B) + &
                (+EQCSUR(CURCALYR,RECCL,B,R))*FAN*AFANUEC(CURCALYR,R,B))* &
                 RSELAST(F,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) ! CHANGED FROM F=3

               FANEQP(CURCALYR,B,R)=FANEQP(CURCALYR,B,R)+ &
                (EQCESE(CURCALYR,RECCL,B,R)+EQCRP90(CURCALYR,RECCL,B,R)+ &
                 EQCSR90(CURCALYR,RECCL,B,R)+EQCSUR(CURCALYR,RECCL,B,R)+ &
                 EQCRP90RP(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R)+& !ebe - add EQCREP
                  EQCADD(CURCALYR,RECCL,B,R))*FAN

               FANCONIN(CURIYR,R,B)=FANCONIN(CURIYR,R,B)+((&
                (EQCESE(CURCALYR,RECCL,B,R))*FAN*FANIUEC(R,B)+ &
                (EQCADD(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R)+ &
                  EQCRP90(CURCALYR,RECCL,B,R)+EQCRP90RP(CURCALYR,RECCL,B,R))* &
                  FAN*NFANIUEC(CURCALYR,R,B) + &
                (EQCSUR(CURCALYR,RECCL,B,R)+EQCSR90(CURCALYR,RECCL,B,R))* &
                 FAN*AFANUEC(CURCALYR,R,B)) )

               DRIVER2(CURIYR,R,B)=DRIVER2(CURIYR,R,B)+                     &
                EQCESE(CURCALYR,RECCL,B,R)+EQCADD(CURCALYR,RECCL,B,R)+     &
                EQCRP90RP(CURCALYR,RECCL,B,R)+EQCRP90(CURCALYR,RECCL,B,R)+ &
                EQCSR90(CURCALYR,RECCL,B,R)+EQCREP(CURCALYR,RECCL,B,R)+    &
                EQCSUR(CURCALYR,RECCL,B,R)
             ENDIF ! F.NE.6 NO FAN CONSUMPTION FOR WOOD

          ENDDO

          !  GEOTHERMAL IS CALCULATED DIFFERENTLY FROM OTHER FUELS
          !    SET FCON = 7 FOR GEOTHERMAL AND CALCULATE OUTSIDE OF THE RSCLASS LOOP
          !    SINCE IT IS NOT ENCOUNTERED IN RSCLASS
          FCON = FHTRCON(7)  ! FCON = 7
          HTRCON(CURIYR,FCON,R)=HTRCON(CURIYR,FCON,R)+HDDFACT(R)*LEAPYR* &
           (EQCESE(CURCALYR,RECCLGHP,B,R)* &
             (EQCUEC(R,RECCLEHP,B)-EQCUEC(R,RECCLGHP,B))*FossilHR/3412.+ &
            EQCRP90(CURCALYR,RECCLGHP,B,R)* &
             (EQCRUEC(CURCALYR,RECCLEHP,B,R)-EQCRUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
            EQCADD(CURCALYR,RECCLGHP,B,R)*&
             (EQCHVUEC(CURCALYR,RECCLEHP,B,R)-EQCHVUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
            EQCSUR(CURCALYR,RECCLGHP,B,R)*&
             (EQCAHVUEC(CURCALYR,RECCLEHP,B,R)-EQCAHVUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
           (EQCREP(CURCALYR,RECCLGHP,B,R)+EQCRP90RP(CURCALYR,RECCLGHP,B,R))* &
             (EQCNUEC(CURCALYR,RECCLEHP,B,R)-EQCNUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
           (EQCSUR(CURCALYR,RECCLGHP,B,R))* &
             (EQCAUEC(CURCALYR,RECCLEHP,B,R)-EQCAUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.)

          GEEQCN(CURIYR,1,B,R)= HDDFACT(R)*LEAPYR* &
           (EQCESE(CURCALYR,RECCLGHP,B,R)* &
              (EQCUEC(R,RECCLEHP,B)-EQCUEC(R,RECCLGHP,B))*FossilHR/3412.+ &
            EQCRP90(CURCALYR,RECCLGHP,B,R)* &
              (EQCRUEC(CURCALYR,RECCLEHP,B,R)-EQCRUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
            EQCADD(CURCALYR,RECCLGHP,B,R)*&
              (EQCHVUEC(CURCALYR,RECCLEHP,B,R)-EQCHVUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
            EQCSUR(CURCALYR,RECCLGHP,B,R)*&
              (EQCAHVUEC(CURCALYR,RECCLEHP,B,R)-EQCAHVUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
           (EQCREP(CURCALYR,RECCLGHP,B,R)+EQCRP90RP(CURCALYR,RECCLGHP,B,R))* &
              (EQCNUEC(CURCALYR,RECCLEHP,B,R)-EQCNUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.+ &
            EQCSR90(CURCALYR,RECCLGHP,B,R)* &
              (EQCAUEC(CURCALYR,RECCLEHP,B,R)-EQCAUEC(CURCALYR,RECCLGHP,B,R))*FossilHR/3412.)
 100  CONTINUE

      ! CALCULATION OF INTENSITY VARIABLE FOR FTAB, ADJUSTING FOR DRIVER IN DENOMINATOR
      DO R=1,MNUMCR-2
        DO FCON=1,NHTRFL
         DO B=1,MNUMBLDG
          IF (DRIVER(CURIYR,FCON,R,B).GT.0)   &
           HTRCONIN(CURIYR,FCON,R,B)=                &
           HTRCONIN(CURIYR,FCON,R,B)/DRIVER(CURIYR,FCON,R,B)
          IF (DRIVER2(CURIYR,R,B).GT.0)   &
           FANCONIN(CURIYR,R,B)=                &
           FANCONIN(CURIYR,R,B)/DRIVER2(CURIYR,R,B)
         ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE RHTRCON
!
!******************************************************************
!     RAC, CAC, AND HEAT PUMP COOLING CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLTEC
      IMPLICIT NONE
      REAL*4 TOTEWTN(5,MNUMBLDG,MNUMCR-2),TOTEWTR(5,MNUMBLDG,MNUMCR-2)
      REAL*4 OPCOST(2),CDDFACT(MNUMCR-2)
      REAL*4 EQWTR(NCoolTypes,MNUMBLDG,MNUMCR),EQWTN(NCoolTypes,MNUMBLDG,MNUMCR)
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHT,RECTY,RECTYHT,RECCL,R,B,F,EQT,EQC,TYPE, &
        TYPEHT,RECCLHHP,COUNT,CNT,L,IND
      INTEGER RECAR(7),eqtar(7)
!
!   THE GENERAL FORM OF THIS SUBROUTINE APPLIES TO ALL END USES
!     SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!
      EU=2
      EUPR=2
!
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!   SET CDDFACT
!
      DO R=1,MNUMCR-2
          CDDFACT(R)=(CDDADJ(CURCALYR,R)/CDDADJ(RECSYEAR,R))**1.50
      ENDDO
!
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      ALPHA1=-0.50
      DISRT=.20
      HORIZON=7.0
!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   OUTTER LOOPS ARE CENSUS DIVISION AND BUILDING TYPE
!
      DO 100 R=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
!*******************************************************************
!    RECCL          = RECORD NUMBER IN RSCLASS FILE: EQUIP CLASS FILE
!    RTCLEUPT(EU)   = LAST RECORD # IN END USE 1 (SPACE HEATING)
!    RTCLEUPT(EU+1) = LAST RECORD # IN END USE 2 (SPACE COOLING)
!    EQC            = EQUIPMENT CLASS # FOR COOLING
!*******************************************************************
!
!    ZERO OUT ARRAYS
!
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO
!
!*******************************************************************
!    RECTY          = RECORD NUMBER IN RSMEQP FILE: EQUIP TYPE FILE
!    RTTYEUPT(EU)   = LAST RECORD # IN END USE 1 (SPACE HEATING)
!    RTTYEUPT(EU+1) = LAST RECORD # IN END USE 2 (SPACE COOLING)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!        TYPE (EQT), REC # FOR RECTY FILE (RECCL), AND FUEL TYPE (F)
!
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
!
!    ONLY CONTINUE CALCULATIONS IF THIS IS NOT A HEAT PUMP (ground-source or air-source)
!      (HEAT PUMPS USE THE SAME DATA CALCULATED FOR HEATING)
!      (if rttypntr is not zero, then it points back to the heating equipment type)
              IF(RTTYPNTR(RECTY).LE.0)THEN
!
!    F = FUEL NUMBER FOR THE CURRENT EQUIPMENT CLASS
!
                F = RTFUEL(RECCL)
!
!    COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
                IF(RTEQEFF(RECTY).NE.0.0) THEN
                  rteffac(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)
                  rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
                ELSE
                  RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                  RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
                ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
        HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
        ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
           HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CURCALYR)/PRICES(F,R,RECSYEAR))**ALPHA1 )
           BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
         ELSE
           BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
       ELSE
        BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
!     COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT
!       ON REGION AND BUILDING TYPE (SAVES COMPUTATIONAL TIME)
!
                ECTEMP = RTECBIAS(RECTY)+ &
                   (BETA1DR(RECTY)*CAPITAL)
!
!      CALCULATE OPERATING COST
       IF (CURCALYR.EQ.RECSYEAR+1) THEN
            OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(1)*CDDFACT(R)
            OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(2)*CDDFACT(R)
       ELSE
            OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(1)*CDDFACT(R) &
                    *(ECSHELL(CURCALYR-1,R,B)/ECSHELL(RECSYEAR,R,B))
            OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
               *RTEFFAC(2)*CDDFACT(R) &
                    *(NCSHELL(CURCALYR-1,R,B)/ECSHELL(RECSYEAR,R,B))
       END IF

!      CALCULATE LIFE CYCLE COSTS
!
                LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
                LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!
!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
!
                EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + &
                     ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + &
                     ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!
              ENDIF
            ENDIF
           ENDIF
          ENDDO
!*******************************************************************
!
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
!
!   FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!      TYPE (EQT), REC # IN THE RSCLASS FILE (RECCL)
!
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
!
!  IF THIS IS A HEAT PUMP, ASSIGN CORRESPONDING HEATER SHARES TO
!    HEAT PUMP AIR CONDITIONERS
!
              IF(RTTYPNTR(RECTY).GT.0) THEN
                TYPEHT=RTTYPNTR(RECTY)
                NEQTSHR(CURCALYR,TYPE,B,R)=NEQTSHR(CURCALYR,TYPEHT,B,R)
                REQTSHR(CURCALYR,TYPE,B,R)=REQTSHR(CURCALYR,TYPEHT,B,R)
!
!   IF NOT A HEAT PUMP, COMPUTE SHARES
!
              ELSE
!
                IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                  NEQTSHR(CURCALYR,TYPE,B,R)=EQWTN(EQT,B,R)/ &
                                          TOTEWTN(EQC,B,R)
                ELSE
                  NEQTSHR(CURCALYR,TYPE,B,R)=0.0
                ENDIF
!
                IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                  REQTSHR(CURCALYR,TYPE,B,R)=EQWTR(EQT,B,R)/ &
                                          TOTEWTR(EQC,B,R)
                ELSE
                  REQTSHR(CURCALYR,TYPE,B,R)=0.0
                ENDIF
              ENDIF
            ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
!
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
                 CURCALYR.LE.RTLASTYR(RECTY)) THEN
               IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
            IF (WTEQCEFFN(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF
          ENDDO
100   CONTINUE
      END SUBROUTINE RCLTEC


!*******************************************************************
!     COOLING ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLADD
      IMPLICIT NONE
      REAL*4 EQCPN90(RECSYEAR:ENDYR,MNUMRTCL,MNUMBLDG,MNUMCR)
      REAL*4 SA(8,MNUMBLDG,MNUMCR-2), HSR, ESR, SVRTE
      REAL*4 SUMHP,CACFACT,x
      REAL*4 RPTOT(MNUMYR),EXTOT(MNUMYR),ADDTOT(MNUMYR),RPRPTOT(MNUMYR),ANUMC(MNUMCR-2,MNUMBLDG),ADENC(MNUMCR-2,MNUMBLDG)
      REAL*4 NEWSHELLWTC(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR),NEWADDWTC(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR)
      INTEGER EQC,RECCL,RECCLHHP,RECCLCAC,RECCLEHP,EU
      INTEGER Y, R, B ,EV,TYPE,NUMEQT,EQT,RECTY,T,TEMP,V,E,D
!
!  SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!
      EU      =  2
      EV      =  2
      CACFACT = .1
!
!   ZERO OUT ARRAYS
!
      DO 1 R=1,MNUMCR-2
        DO 1 B=1,MNUMBLDG
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           EQCSR90(CURCALYR,RECCL,B,R)=0.0
           EQCSUR(CURCALYR,RECCL,B,R)=0.0
           SA(EQC,B,R)=0.0
           EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
         ENDDO
1     CONTINUE
!*******************************************************************
!  Calculate CACs added in PREVYR (CURIYR-1)
!*******************************************************************
      DO 10 R=1,MNUMCR-2
       DO 10 B=1,MNUMBLDG
         SUMHP=0.0
         DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
!
!   RECCLCAC = EQUIPMENT CLASS NUMBER FOR CENTRAL A/C
!   RECCLEHP = EQUIPMENT CLASS NUMBER FOR ELECTRIC HP
!
           IF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
             EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)* &
                                      RACSAT(B,R)*RACUNTS(B,R))
           ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
             RECCLCAC=EQC+RTCLEUPT(EU)
             EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)* &
                                      CACSAT(B,R))
           ELSE
!
!   IF NOT ROOM_AIR OR CENT_AIR, MUST BE HEAT PUMP
!   ADJUST FOR HEAT PUMPS CALCULATED IN HEATING SUB
!   RECCLHHP = HEAT PUMP RECORD NUMBER FROM THE HEATING DATA
!   RECCLEHP = ELEC HEAT PUMP RECORD NUMBER FROM COOLING
!
             IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
               RECCLEHP=EQC+RTCLEUPT(EU)
             ENDIF
             RECCLHHP=RTCLPNTR(RECCL)
             EQCADD(CURCALYR,RECCL,B,R)=(EQCADD(CURCALYR,RECCLHHP,B,R))
             SUMHP=(SUMHP+EQCADD(CURCALYR,RECCL,B,R))
           ENDIF
         ENDDO
!
!  UPDATE CAC ADDITIONS BASED ON TOTAL HP ADDITIONS
!
           IF(EQCADD(CURCALYR,RECCLCAC,B,R).LE.SUMHP)THEN
             EQCADD(CURCALYR,RECCLCAC,B,R)=(EQCADD(CURCALYR,RECCLEHP,B,R)* &
               CACFACT)
          ELSE
             EQCADD(CURCALYR,RECCLCAC,B,R)=(EQCADD(CURCALYR,RECCLCAC,B,R)- &
               SUMHP)
          END IF
 10   CONTINUE
!*******************************************************************
! CUMULATE SURVIVING EQUIPMENT REPLACED FOR 1993 VINTAGE PRIOR TO
!   PREVYR
!*******************************************************************
      DO 15 R=1,MNUMCR-2
        DO 15 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).GT.0) THEN
!
!  HEAT PUMP AIR CONDITIONERS USE HEAT PUMP HEATER DATA
!
              RECCLHHP=RTCLPNTR(RECCL)
              EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCLHHP,B,R))
              EQCSUR(CURCALYR,RECCL,B,R)=(EQCSUR(CURCALYR,RECCLHHP,B,R))
            ELSE
!
!   CALCULATE DATA FOR NON-HEAT PUMPS
!
              IF (CURCALYR.EQ.RECSYEAR+1) THEN
                EQCND90(CURCALYR,RECCL,B,R)=(EQCESE(RECSYEAR,RECCL,B,R)*HDR(B))
              ELSE
                EQCND90(CURCALYR,RECCL,B,R)= &
                 ( EQCND90(CURCALYR-1,RECCL,B,R)*HDR(B))
              END IF
              IF ((B.EQ.1).AND.(RTCLNAME(RECCL).EQ.'CENT_AIR')) THEN
              EQCPN90(CURCALYR,RECCL,B,R)=(EQCND90(CURCALYR,RECCL,B,R)* &
                CACPR(R)-EQCND90(CURCALYR,RECCL,B,R))
              ELSE
              EQCPN90(CURCALYR,RECCL,B,R)=0.0
              END IF
!
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF

!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS *NEW TO AEO2000*
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
!         EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
              DO Y=RECSYEAR+1,(CURCALYR-1)
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP)) &
        +   EQCPN90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
               ENDDO
           ELSE
          EQCRP90RP(CURCALYR,RECCL,B,R)=EQCPN90(CURCALYR,RECCL,B,R)
           ENDIF
           IF(CURCALYR.GT.RECSYEAR+1) THEN
          EQCRP90RP(CURCALYR,RECCL,B,R)=EQCRP90RP(CURCALYR,RECCL,B,R)+ &
                                      EQCPN90(CURCALYR,RECCL,B,R)
           ENDIF
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
               DO Y=RECSYEAR+1,(CURCALYR-1)
                HSR=HDR(B)**(CURCALYR-Y)
                ESR=SVRTE(RTALPHA(RECCL),CURCALYR-Y,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)= (&
                  EQCSR90(CURCALYR,RECCL,B,R)+EQCRP90(Y,RECCL,B,R)* &
                  ESR*HSR+EQCRP90RP(Y,RECCL,B,R)*ESR*HSR &
                         + EQCPN90(Y,RECCL,B,R)*ESR*HSR)
!*******************************************************************
! CUMULATE SURVIVING NEW CACS ADDED PRIOR TO PREVYR TO ESTIMATE NH
! SA REPRESENTS NH at PREVYR-1
! CUMULATE SURVIVING NEW CACS ADDED & REPLACED PRIOR TO PREVYR
! REPLACE EQUIP = SURV.HOUSES(SA) -  SURV.EQUIP(CACSUR)
!*******************************************************************
                SA(EQC,B,R) = (SA(EQC,B,R) + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = ( EQCSUR(CURCALYR,RECCL,B,R) + &
                  ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                   (HSR*ESR)))
              ENDDO
           ENDIF
            ENDIF
          ENDDO
 15   CONTINUE
!*******************************************************************
! CALCULATE REPLACEMENTS FOR 2005 VINTAGE IN PREVYR
!*******************************************************************
! CALCULATE REPLACEMENT HEATERS FOR NEW VINTAGE IN PREVYR
! NOTE: REPLACES WITH LIKE
! NOTE: FOR NEW HOUSES (NH) - PREVYR REPRESENTS THE LAGGED VALUE
!*******************************************************************
      DO 20 R=1,MNUMCR-2
        DO 20 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF(RTCLPNTR(RECCL).GT.0) THEN
!
!  HEAT PUMP AIR CONDITIONERS USE HEAT PUMP HEATER DATA
!
              RECCLHHP=RTCLPNTR(RECCL)
              EQCRP90(CURCALYR,RECCL,B,R)=(EQCRP90(CURCALYR,RECCLHHP,B,R))
              EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCLHHP,B,R))
            IF(B.EQ.1) &
              OEQCRP90(CURCALYR,RECCL,B,R)=(EQCRP90(CURCALYR,RECCLHHP,B,R))
            IF(B.EQ.1) &
             OEQCRP90R(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCLHHP,B,R))
             EQCREP(CURCALYR,RECCL,B,R)=(EQCREP(CURCALYR,RECCLHHP,B,R))
            IF(B.EQ.1) &
             OEQCREP(CURCALYR,RECCL,B,R)=(EQCREP(CURCALYR,RECCLHHP,B,R))
            ELSE
!
!   CALCULATE DATA FOR NON-HEAT PUMPS
!
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************

             IF(B.EQ.1) &
              OEQCRP90(CURCALYR,RECCL,B,R)=EQCRP90(CURCALYR,RECCL,B,R)
             IF(B.EQ.1) &
              OEQCRP90R(CURCALYR,RECCL,B,R)=EQCRP90RP(CURCALYR,RECCL,B,R)
              EQCREP(CURCALYR,RECCL,B,R)=(SA(EQC,B,R) &
                                   -EQCSUR(CURCALYR,RECCL,B,R))
             IF(B.EQ.1) &
                OEQCREP(CURCALYR,RECCL,B,R)=(SA(EQC,B,R) &
                                   -EQCSUR(CURCALYR,RECCL,B,R))
            ENDIF
          ENDDO
20    CONTINUE
!
         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!
!   ADD TOTAL ACS AND COMPUTE NEW SHELL EFFICIENCY
!
      DO R=1,MNUMCR-2
           DO B=1,MNUMBLDG
            NCSHELL(CURCALYR,R,B)=0.0
            NEWSHELLWTC(CURCALYR,B,R)=0.0
            NEWADDWTC(CURCALYR,B,R)=0.0
            ANUMC(R,B)=0.0
            ADENC(R,B)=0.0
       END DO
          ENDDO
      DO 32 R=1,MNUMCR-2
        DO 32 B=1,MNUMBLDG
          DO 32 RECCL=RTCLEUPT(EU)+2,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            NEWSHELLWTC(CURCALYR,B,R)=NEWSHELLWTC(CURCALYR,B,R)+ &
                        EQCADD(CURCALYR,RECCL,B,R)*CSHELL(CURCALYR,EQC,B,R)
            NEWADDWTC(CURCALYR,B,R)=NEWADDWTC(CURCALYR,B,R)+EQCADD(CURCALYR,RECCL,B,R)
 32   CONTINUE
!
!   COMPUTE SHELL AVERAGE FOR EACH FUEL TYPE
!
      DO R=1,MNUMCR-2
           DO B=1,MNUMBLDG
       IF (NEWADDWTC(CURCALYR,B,R).GT.0.0) THEN
            NCSHELL(CURCALYR,R,B)=NEWSHELLWTC(CURCALYR,B,R)/NEWADDWTC(CURCALYR,B,R)
       ELSE
              NCSHELL(CURCALYR,R,B)=1.0
           END IF
       END DO
          ENDDO
!
      DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          ! cooling non HP
          EXSQFTADJ(CURCALYR,B,D,3)=(ELASTIC(3,D)*((EXSQRFOOT(CURCALYR,B,D)-EXSQRFOOT(RECSYEAR,B,D))/ &
                        EXSQRFOOT(RECSYEAR,B,D))*NCSHELL(CURCALYR,D,B))+1
          ! cooling HP
          EXSQFTADJ(CURCALYR,B,D,4)=(ELASTIC(4,D)*((EXSQRFOOT(CURCALYR,B,D)-EXSQRFOOT(RECSYEAR,B,D))/ &
                        EXSQRFOOT(RECSYEAR,B,D))*NCSHELL(CURCALYR,D,B))+1
        END DO
      END DO


!*******************************************************************
!  CALCULATE AVERAGE NEW COOLING SHELL INDEX
!*******************************************************************
        DO 50 R=1,MNUMCR-2
         DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (CURCALYR.GT.RECSYEAR+1) THEN
            DO Y=RECSYEAR+1,CURCALYR-1
            ANUMC(R,B)=ANUMC(R,B)+(NCSHELL(Y,R,B)* &
              EQCADD(Y,RECCL,B,R))
            ADENC(R,B)=ADENC(R,B)+(EQCADD(Y,RECCL,B,R))
             END DO
            END IF
 50   CONTINUE
!*******************************************************************
        DO 55 R=1,MNUMCR-2  ! NO SHELL FOR COAL (7)
         DO 55 B=1,MNUMBLDG
         IF (ADENC(R,B).LE.0) THEN
             ACSHELL(CURCALYR,R,B)=ACSHELL(CURCALYR-1,R,B)
         ELSE
             ACSHELL(CURCALYR,R,B)=ANUMC(R,B)/ADENC(R,B)
         END IF
!         IF(ACSHELL(CURCALYR,R,B).GT.ACSHELL(CURCALYR-1,R,B)) &
!             ACSHELL(CURCALYR,R,B)=ACSHELL(CURCALYR-1,R,B)
         IF(ACSHELL(CURCALYR,R,B).LT.LIMIT) ACSHELL(CURCALYR,R,B)=LIMIT
       IF(CURCALYR.LE.RECSYEAR+1) THEN
           ACSHELL(CURCALYR,R,B)=NCSHELL(CURCALYR,R,B)
       ENDIF
 55   CONTINUE
!
!*******************************************************************
!     AGGREGATE COOLING SYSTEMS FOR INVESTMENT ANALYSIS
!*******************************************************************
       Y=CURCALYR
       NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
       DO B=1,MNUMBLDG
        DO r=1,MNUMCR-2
        TYPE=RTTYPECT(EU)
         DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
         IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
             X=1.0
               IF (reccl.eq.12) X= 1.0 ! RACUNTS(B,R)
               IF (RECCL.eq.12) then
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r))
               else
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHRC(Y,EQT,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)   )
               end if
            ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
      END SUBROUTINE RCLADD
!*******************************************************************
!     COOLING CONSUMPTION SUBROUTINE
!*******************************************************************
      SUBROUTINE RCLCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP,CDDFACT(MNUMCR),TEMP1,TEMP2
      real*4 alpha2,rba,rbr,rbn
      REAL*4 AVNEWEQP(2006:ENDYR),AVNEWUEC(2006:ENDYR),UECPRINT(2006:ENDYR)
      INTEGER B, D, F, Y, YEAR,CYEAR,S,R
      INTEGER EU, EUPR, EQC, RECCL, FCON, FDIM, EQCGHP, EQCEHP, V
      INTEGER RECCLGHP, RECCLEHP
!*******************************************************************
!  EQUIP 1=RAC 2=CAC 3=HP 4=GEOHP 5=GASHP - Y,E,B,D
!  CONSUMPTION FUEL 1=EL 2=GEO 3=GAS
!*******************************************************************
!
!  SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!
      EU     =  2
      EUPR=2
      ALPHA=-.15; ef1=.5;ef2=.35;ef3=.15
      ALPHA2=-.15
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!   SET CDDFACT
!
      DO D=1,MNUMCR-2
          CDDFACT(D)= (CDDADJ(CURCALYR,D)/CDDADJ(RECSYEAR,D))**1.50
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 20051993 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  CALCULATE NEW AND AVERAGE UECS
!*******************************************************************
      DO 20 D=1,MNUMCR-2
        DO 20 B=1,MNUMBLDG
!
!   SEARCH THE SPACE COOLING SECTION OF THE DATA (EU=2)
!
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (EQC.LT.3) THEN
              S=3
            ELSE
              S=4
            END IF
         EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)*EXSQFTADJ(CURCALYR,B,D,S)* &
           ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
         EQCSIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
       IF (WTEQCEFFN(CURCALYR,RECCL,B,D) .GT. 0.0) THEN
         EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL) ! &
         EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
           WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
       ELSE
         EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) !&
         EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
           END IF
       IF (WTEQCEFFHV(CURCALYR,RECCL,B,D) .GT. 0.0) THEN
         EQCHVUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFHV(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL) &
                                      *WTEQCSQFHV(CURCALYR,RECCL,B,D)
         EQCHVIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
           WTEQCEFFHV(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
       ELSE
         EQCHVUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) &
                                      *WTEQCSQFHV(CURCALYR,RECCL,B,D)
         EQCHVIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
       END IF
        IF (WTEQCEFFR(CURCALYR,RECCL,B,D) .GT. 0.0) THEN
          EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
          WTEQCEFFR(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)*EXSQFTADJ(CURCALYR,B,D,S)
          EQCRIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
          WTEQCEFFR(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
        ELSE
          EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
          EQCRIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
        END IF
         IF (CURCALYR .EQ. RECSYEAR+1) THEN
           EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
           EQCAHVUEC(CURCALYR,RECCL,B,D)=EQCHVUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
         TEMP1=0.0
         TEMP2=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
          TEMP1=TEMP1+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)
          TEMP2=TEMP2+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D) + EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)
         ENDDO
          IF (EQC.EQ.1) THEN
          IF(TEMP2.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
       (EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D)) &
                                           /TEMP2
        END DO
          END IF
          ELSE
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
         DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
        EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)*EQCNIUEC(Y,RECCL,B,D)) &
                                           /TEMP
         END DO
          END IF
        END IF
         IF (EQC.GT.1) THEN
          IF(TEMP1.LE.0.0) THEN
             EQCAHVUEC(CURCALYR,RECCL,B,D)=EQCHVUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAHVUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAHVUEC(CURCALYR,RECCL,B,D)=EQCAHVUEC(CURCALYR,RECCL,B,D)+( &
        EQADDFUT(Y,CURCALYR,RECCL,B,D)*EQCHVUEC(Y,RECCL,B,D)+ &
           EQREPFUT(Y,CURCALYR,RECCL,B,D)*EQCNUEC(Y,RECCL,B,D)) &
                                           /TEMP1
        ENDDO
          END IF
           END IF
          END IF
          ENDDO
 20   CONTINUE
         AVNEWUEC(CURCALYR)=0.0
         AVNEWEQP(CURCALYR)=0.0
      DO 21 D=1,9
        DO 21 B=1,3
          DO 21 RECCL=13,14
           AVNEWUEC(CURCALYR)=AVNEWUEC(CURCALYR)+( &
       EQCRUEC(CURCALYR,RECCL,B,D)*EQCRP90(CURCALYR,RECCL,B,D) + &
       EQCAUEC(curcalyr,reccl,B,d)*EQCREP(CURCALYR,RECCL,B,D) + &
       EQCADD(curcalyr,RECCL,B,d)*EQCHVUEC(CURCALYR,RECCL,B,D)+ &
       EQCRP90RP(CURCALyR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D))!
           AVNEWEQP(CURCALYR)=AVNEWEQP(CURCALYR)+&
            EQCRP90(CURCALYR,RECCL,B,D) + EQCREP(CURCALYR,RECCL,B,D) + &
            EQCADD(curcalyr,rECCL,B,d)+EQCRP90RP(CURCALyR,RECCL,B,D)
 21   CONTINUE
        UECPRINT(CURCALYR)=(AVNEWUEC(CURCALYR)/AVNEWEQP(CURCALYR))*293.

!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      IF (EQC.EQ.1) THEN
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
      ELSE
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D)+ &
       EQADDFUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFHV(Y,RECCL,B,D)+ &
           (EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D)) &
                                           /TEMP
      END IF
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!*******************************************************************
!  INITIALIZE COOLING CONSUMPTION
!*******************************************************************
      DO 90 D=1,MNUMCR-2
        DO 90 FCON=1,NCLFL
          COOLCN(CURIYR,FCON,D)=0.0
          COOLCNwithDG(CURIYR,FCON,D)=0.0                         !PVdispatch
          DO 90 B=1,MNUMBLDG
           driver(curiyr,fcon,d,b)=0.
           COOLCNWT(CURIYR,FCON,D,B)=0.
           COOLCNIN(CURIYR,FCON,D,B)=0.
 90   CONTINUE
!
!  FIND INDICES FOR THE ELECTRIC AND GEOTHERMAL HEATPUMPS
!    USED TO COMPUTE COOLCN FOR GEOTHERMAL FUEL (FCON=2)
!
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP')THEN
          EQCEHP=RTCLEQCL(RECCL)
          RECCLEHP=EQCEHP+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP')THEN
          EQCGHP=RTCLEQCL(RECCL)
          RECCLGHP=EQCGHP+RTCLEUPT(EU)
        ENDIF
      ENDDO
!*******************************************************************
!  CALCULATE COOLING CONSUMPTION
!*******************************************************************
      DO 100 D=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC = RTCLEQCL(RECCL)
!
!   F    = RTEK FUEL NUMBER
!   FCON = CONSUMPTION FUEL NUMBER FOR COOLING
!
            F   = RTFUEL(RECCL)
            FCON=FCLCON(F)
            EQCEQCN(CURIYR,RECCL,B,D)=0.0

            IF (CURCALYR.EQ.RECSYEAR+1) THEN
!
! RECSYEAR+1
            IF (EQC.EQ.1) THEN
               COOLCN(CURIYR,FCON,D)=COOLCN(CURIYR,FCON,D)+ CDDFACT(D)*( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                 EQCNUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
               COOLCNWT(CURIYR,FCON,D,B)=COOLCNWT(CURIYR,FCON,D,B)+CDDFACT(D)*( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                 EQCNUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

              COOLCNIN(CURIYR,FCON,D,B)=COOLCNIN(CURIYR,FCON,D,B)+( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSIUEC(CURCALYR,RECCL,B,D)* &
               (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                EQCNUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D) &
              *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRIUEC(CURCALYR,RECCL,B,D) &
              *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))))

              driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
               (EQCESE(CURCALYR,RECCL,B,d)+EQCADD(CURCALYR,RECCL,B,d)+ &
                EQCRP90RP(CURCALYR,RECCL,B,d)+EQCRP90(CURCALYR,RECCL,B,d))
!
              EQCEQCN(CURIYR,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CURCALYR,RECCL,B,D)* &
                EQCSUEC(CURCALYR,RECCL,B,D)* &
               (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))) + &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
              *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)* &
               (NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCRP90(CURCALYR,RECCL,B,D)* &
                EQCRUEC(CURCALYR,RECCL,B,D)*(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

             ELSE ! EQC > 1

              COOLCN(CURIYR,FCON,D)=COOLCN(CURIYR,FCON,D)+ CDDFACT(D)*( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                 EQCHVUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              COOLCNWT(CURIYR,FCON,D,B)=COOLCNWT(CURIYR,FCON,D,B)+CDDFACT(D)*( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                 EQCHVUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

               COOLCNIN(CURIYR,FCON,D,B)=COOLCNIN(CURIYR,FCON,D,B)+( &
               ((EQCESE(CURCALYR,RECCL,B,D)*EQCSIUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCADD(CURCALYR,RECCL,B,D)* &
                 EQCHVIUEC(CURCALYR,RECCL,B,D)*(NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRIUEC(CURCALYR,RECCL,B,D) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))))

               driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,d)+EQCADD(CURCALYR,RECCL,B,d)+ &
                 EQCRP90RP(CURCALYR,RECCL,B,d)+EQCRP90(CURCALYR,RECCL,B,d))
!
               EQCEQCN(CURIYR,RECCL,B,D)=CDDFACT(D)*(((EQCESE(CURCALYR,RECCL,B,D)* &
                 EQCSUEC(CURCALYR,RECCL,B,D)* &
                (ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))) + &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNIUEC(CURCALYR,RECCL,B,D)*EXSQFTADJ(CURCALYR,B,D,S) &
               *(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+ &
                (EQCADD(CURCALYR,RECCL,B,D)*EQCHVUEC(CURCALYR,RECCL,B,D)* &
                (NCSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B)))+(EQCRP90(CURCALYR,RECCL,B,D)* &
                 EQCRUEC(CURCALYR,RECCL,B,D)*(ECSHELL(CURCALYR,D,B)/ECSHELL(RECSYEAR,D,B))))* &
                 RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

             ENDIF ! EQC=1

            ELSE ! CURCALYR > RECSYEAR+1

! Special treatment of stimulus program
               IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
                 ALPHA=-0.30
                ELSE
                 ALPHA=-0.15
               END IF
!
      rba=(rtbaseff(RECSYEAR,reccl)*wteqceffa(CURCALYR,reccl,b,d))**alpha2
      rbr=(rtbaseff(RECSYEAR,reccl)*wteqceffr(CURCALYR,reccl,b,d))**alpha2
      rbn=(rtbaseff(RECSYEAR,reccl)*wteqceffn(CURCALYR,reccl,b,d))**alpha2

         IF (EQC.EQ.1) THEN
               coolcn(curiyr,fcon,d)=coolcn(curiyr,fcon,d)+CDDFACT(D)* (( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2)) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqcnuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba + &
               (eqcrep(CURCALYR,reccl,b,d) *eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcauec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))

               coolcnwt(curiyr,fcon,d,b)=coolcnwt(curiyr,fcon,d,b)+CDDFACT(D)*(( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsiuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2)) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqcnuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba + &
               (eqcrep(CURCALYR,reccl,b,d) *eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcauec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))
!
               coolcnin(curiyr,fcon,d,b)=coolcnin(curiyr,fcon,d,b)+(( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsiuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcriuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B)))+ &
               (eqcrep(CURCALYR,reccl,b,d) *eqcniuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcauec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B)))))

       driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,d)+EQCADD(CURCALYR,RECCL,B,d)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,d)+EQCRP90(CURCALYR,RECCL,B,d)+ &
                 eqcsr90(CURCALYR,reccl,b,d)+eqcrep(CURCALYR,reccl,b,d)+    &
                 eqcsur(CURCALYR,reccl,b,d))

               eqceqcn(curiyr,reccl,b,d)=CDDFACT(D)*( &
               ((EQCESE(CURCALYR,reccl,b,d)*eqcsuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))+ &
                (eqcadd(CURCALYR,reccl,b,d)*eqcnuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn  + &
                (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr+ &
              (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn+ &
                (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba  + &
                (eqcrep(CURCALYR,reccl,b,d)*eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn+ &
                (eqcsur(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba     )*( &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))
!
         ELSE
               coolcn(curiyr,fcon,d)=coolcn(curiyr,fcon,d)+CDDFACT(D)* (( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2)) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqchvuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba + &
               (eqcrep(CURCALYR,reccl,b,d) *eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcahvuec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))

               coolcnwt(curiyr,fcon,d,b)=coolcnwt(curiyr,fcon,d,b)+CDDFACT(D)*(( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2)) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqchvuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba + &
               (eqcrep(CURCALYR,reccl,b,d) *eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcahvuec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba)*( &
               RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))
!
               coolcnin(curiyr,fcon,d,b)=coolcnin(curiyr,fcon,d,b)+(( &
               (EQCESE(CURCALYR,reccl,b,d)*eqcsiuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcadd(CURCALYR,reccl,b,d)*eqchviuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcrp90(CURCALYR,reccl,b,d)*eqcriuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
             (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d) &
       *(ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B)))+ &
               (eqcrep(CURCALYR,reccl,b,d) *eqcniuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))) + &
               (eqcsur(CURCALYR,reccl,b,d) *eqcahvuec(CURCALYR,reccl,b,d) &
       *(acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B)))))

       driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,d)+EQCADD(CURCALYR,RECCL,B,d)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,d)+EQCRP90(CURCALYR,RECCL,B,d)+ &
                 eqcsr90(CURCALYR,reccl,b,d)+eqcrep(CURCALYR,reccl,b,d)+    &
                 eqcsur(CURCALYR,reccl,b,d))

               eqceqcn(curiyr,reccl,b,d)=CDDFACT(D)*( &
               ((EQCESE(CURCALYR,reccl,b,d)*eqcsuec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))+ &
                (eqcadd(CURCALYR,reccl,b,d)*eqchvuec(CURCALYR,reccl,b,d)* &
       (ncshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn  + &
                (eqcrp90(CURCALYR,reccl,b,d)*eqcruec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbr+ &
              (eqcrp90rp(CURCALYR,reccl,b,d)*eqcniuec(CURCALYR,reccl,b,d)*EXSQFTADJ(CURCALYR,B,D,S)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn+ &
                (eqcsr90(CURCALYR,reccl,b,d)*eqcauec(CURCALYR,reccl,b,d)* &
       (ecshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba  + &
                (eqcrep(CURCALYR,reccl,b,d)*eqcnuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rbn+ &
                (eqcsur(CURCALYR,reccl,b,d)*eqcahvuec(CURCALYR,reccl,b,d)* &
       (acshell(CURCALYR,d,b)/ECSHELL(RECSYEAR,D,B))**(1.0+alpha2))*rba     )*( &
                RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)))
!
           END IF
          ENDIF
        ENDDO
!
!    GEOTHERMAL IS CALCULATED DIFFERENTLY
!
            FCON = FCLCON(7)   ! = 2
            COOLCN(CURIYR,FCON,D)=COOLCN(CURIYR,FCON,D)+CDDFACT(D)* &
             (EQCESE(CURCALYR,RECCLGHP,B,D)* &
            ((EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*FossilHR/3412. )+ &
              EQCRP90(CURCALYR,RECCLGHP,B,D)* &
            ((EQCRUEC(CURCALYR,RECCLEHP,B,D)-EQCRUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) &
            + EQCADD(CURCALYR,RECCLGHP,B,D)* &
                   ((EQCHVUEC(CURCALYR,RECCLEHP,B,D)-EQCHVUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) +&
            + EQCSUR(CURCALYR,RECCLGHP,B,D)* &
                   ((EQCAHVUEC(CURCALYR,RECCLEHP,B,D)-EQCAHVUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) +&
          (EQCREP(CURCALYR,RECCLGHP,B,D)+EQCRP90RP(CURCALYR,RECCLGHP,B,D))* &
            ((EQCNUEC(CURCALYR,RECCLEHP,B,D)-EQCNUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) &
            +(EQCSR90(CURCALYR,RECCLGHP,B,D))*&
            ((EQCAUEC(CURCALYR,RECCLEHP,B,D)-EQCAUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.))
!
            GEEQCN(CURIYR,2,B,D)=CDDFACT(D)*&
             (EQCESE(CURCALYR,RECCLGHP,B,D)* &
            ((EQCUEC(D,RECCLEHP,B)-EQCUEC(D,RECCLGHP,B))*FossilHR/3412. )+ &
              EQCRP90(CURCALYR,RECCLGHP,B,D)* &
            ((EQCRUEC(CURCALYR,RECCLEHP,B,D)-EQCRUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) &
            + EQCADD(CURCALYR,RECCLGHP,B,D)* &
                   ((EQCHVUEC(CURCALYR,RECCLEHP,B,D)-EQCHVUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) +&
            + EQCSUR(CURCALYR,RECCLGHP,B,D)* &
                   ((EQCAHVUEC(CURCALYR,RECCLEHP,B,D)-EQCAHVUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) +&
          (EQCREP(CURCALYR,RECCLGHP,B,D)+EQCRP90RP(CURCALYR,RECCLGHP,B,D))* &
            ((EQCNUEC(CURCALYR,RECCLEHP,B,D)-EQCNUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.) &
            +(EQCSR90(CURCALYR,RECCLGHP,B,D))* &
            ((EQCAUEC(CURCALYR,RECCLEHP,B,D)-EQCAUEC(CURCALYR,RECCLGHP,B,D))*FossilHR/3412.))
 100  CONTINUE

      DO R=1,MNUMCR-2
        DO FCON=1,NCLFL
         DO B=1,MNUMBLDG
          If (driver(CURIYR,FCON,R,B).gt.0)   &
           COOLCNIN(CURIYR,FCON,R,B)=                &
           COOLCNIN(CURIYR,FCON,R,B)/driver(CURIYR,FCON,R,B)
         Enddo
        Enddo
      Enddo


      END SUBROUTINE RCLCON

!*******************************************************************
!     CLOTHES WASHER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RCWTEC
      IMPLICIT NONE
      REAL*4 WATERLOAD
      REAL*4 RTEFFAC(2)
      REAL*4 DISRT,HORIZON,DECAY,OPCOST(2)
      REAL*4 EQWTN(10,MNUMBLDG,MNUMCR),EQWTR(10,MNUMBLDG,MNUMCR), &
       TOTEWTN(4,MNUMBLDG,MNUMCR),TOTEWTR(4,MNUMBLDG,MNUMCR)
      REAL*4 DENOM, DENOM2, SUM,SUM1
      REAL*4 EQCOST,CAPITAL,RETAIL,BASEUSE,BASEMEF
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(7),eqtar(7)
!*******************************************************************
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!
      EU=3
      EUPR=5
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,5)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      ALPHA1=-0.50
      WATERLOAD= 2.3046 ! Stock AVERAGE AMOUNT OF WATER LOAD FOR CLOTHES WASHERS
      BASEMEF=0.95
      BASEUSE=4.0125 ! MMBTU OF CLOTHES WASHER + DRYER ENERGY
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   ZERO OUT ARRAYS
!
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          NCWLOAD(CURCALYR,R,B)=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CURCALYR,RECCL,B,R)=0.0
            WTEQCEFFR(CURCALYR,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE
!
!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
      DO 50 R=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
!
!
!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                rteffac(1)=RTEQEFF(RECTY)/EQCEFF(CURCALYR,reccl)
                rteffac(2)=RTEQEFF(RECTY)/RTBASEFF(RECSYEAR,reccl)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
              ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!
            OPCOST(1)=PRICES(F,R,CURCALYR)*(BASEUSE*(BASEMEF/CWMEF(RECTY)))
            OPCOST(2)=PRICES(F,R,CURCALYR)*(BASEUSE*(BASEMEF/CWMEF(RECTY)))
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(4,R,CURCALYR).GT.PRICES(4,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(4,R,CURCALYR)/PRICES(4,R,RECSYEAR))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
!      CALCULATE LIFE CYCLE COSTS
!
              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!
                EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(2))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(1))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
            ENDIF
          ENDIF
 50   CONTINUE
!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO 70 R=1,MNUMCR-2
        DO 70 B=1,MNUMBLDG
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
!
                  NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  TOTEWTN(EQC,B,R))
                  REQTSHR(CURCALYR,TYPE,B,R)=(EQWTR(EQT,B,R)/ &
                  TOTEWTR(EQC,B,R))
             ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
!
!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                 SUM =SUM+(NEQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
                 SUM1=SUM1+(NEQTSHR(CURCALYR,TYPE,B,R)*LOADADJ(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
              NCWLOAD(CURCALYR,R,B)=SUM1/DENOM
            ENDIF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
!
          ENDDO
!
 70   CONTINUE
      END SUBROUTINE RCWTEC
!*******************************************************************
!     CLOTHES WASHERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RCWADD
      IMPLICIT NONE
      REAL*4 LOADTOT(RECSYEAR:ENDYR,MNUMRTCL,MNUMCR,MNUMBLDG)
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU       = 3 IS CLOTHES WASHERS
!*******************************************************************
      EV       = 3
      EU       = 3
!*******************************************************************
!  CALCULATE CLOTHES WASHERS ADDED IN CURIYR (CURIYR-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR 2005 VINTAGE PRIOR TO
!   CURIYR
!*******************************************************************
! CUMULATE SURVIVING NEW CLOTHES WASHERS ADDED PRIOR TO CURIYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURIYR-1
! CUMULATE SURVIVING NEW WASHERS ADDED & REPLACED PRIOR TO CURIYR
! REPLACE EQUIP = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-CLOTHES WASHERS)
!*******************************************************************
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CURCALYR,RECCL,B,R)=0.0
            EQCSUR(CURCALYR,RECCL,B,R)=0.0
            EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
            ECWLOAD(CURCALYR,R,B) = 0.0
          ENDDO
!
       IF (CURCALYR.GT.RECSYEAR+1) THEN
          WASHNEW(CURCALYR,B,R)=WASHNEW(CURCALYR-1,B,R)*1.01
       END IF
       IF (WASHNEW(CURCALYR,B,R).GT.1.0000) THEN
          WASHNEW(CURCALYR,B,R)=1.0000
       END IF
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)* &
                                    WASHNEW(CURCALYR,B,R))
      SA=0.0
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF
!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA =( SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) =( EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT CLOTHES WASHERS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
            EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)

!
         ENDDO
5     CONTINUE
!
         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQCESEFUT(CURCALYR,Y,RECCL,B,R)=(EQCESE(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO

!*******************************************************************
!     AGGREGATE CLOTHES WASHERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
!
      DO B=1,MNUMBLDG
       DO r=1,MNUMCR-2
        TYPE=RTTYPECT(EU)
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
           IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                 HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                 HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                 REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
           endif
           ENDIF
          ENDDO
         ENDDO
        ENDDO
!
       END SUBROUTINE RCWADD
!*******************************************************************
!  CLOTHES WASHER CONSUMPTION
!*******************************************************************
      SUBROUTINE RCWCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3
      REAL*4 TEMP,TEMP1
      INTEGER B,E,D,EU,EUPR,RECCL,EQC,F,Y,R
!*******************************************************************
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!
      EU = 3
      EUPR=5  !no end use price for washers -- map to dryer price, assume use pattern similar
      alpha=0.0 ;ef1=.5;ef2=.35;ef3=.15

!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New, REPLACEMENT, AND Average UECS
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                   ( RTBASEFF(CURCALYR,RECCL) / RTBASEFF(RECSYEAR,RECCL) )
            EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
              (WTEQCEFFN(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL))
            EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             ( WTEQCEFFR(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL) )
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
              ECWLOAD(CURCALYR,D,B)=(EQCESE(CURCALYR,RECCL,B,D)+ (&
               (EQCADD(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)) * &
                NCWLOAD(CURCALYR,D,B)))/(EQCESE(CURCALYR,RECCL,B,D)+ &
                EQCADD(CURCALYR,RECCL,B,D)+ EQCRP90(CURCALYR,RECCL,B,D))
            ELSE
         TEMP=0.0
         TEMP1=0.0
          DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
          TEMP1=TEMP1+ EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)+EQCESEFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
          ECWLOAD(CURCALYR,D,B)=ECWLOAD(CURCALYR,D,B)+ ( &
       EQCESEFUT(Y,CURCALYR,RECCL,B,D)+((EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
        EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*NCWLOAD(Y,D,B))) &
                                           /TEMP1
        ENDDO
           END IF
          END IF
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
            ELSE
            TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
 30   CONTINUE
!*******************************************************************
!  Calculate CLOTHES WASH Consumption
!*******************************************************************
      DO 40 D=1,MNUMCR-2
        CSWCON(CURIYR,D)=0.0
       Do 40 B=1,MNUMBLDG
        CSWCONIN(CURIYR,D,B)=0.
        driver2(CURIYR,d,B)=0.
        CSWCONWT(CURIYR,D,B)=0.
40    CONTINUE
!
      DO 50 D=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
!
              CSWCON(CURIYR,D)=CSWCON(CURIYR,D)+LEAPYR* ( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              CSWCONWT(CURIYR,D,B)=CSWCONWT(CURIYR,D,B)+LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

         IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN

             CSWCONIN(CURIYR,D,B)=CSWCONIN(CURIYR,D,B)+( (&
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))))  )

             driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) )
         Endif
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
            +(EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
            ELSE
!
              CSWCON(CURIYR,D)=CSWCON(CURIYR,D)+ LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
              *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)  )
!
              CSWCONWT(CURIYR,D,B)=CSWCONWT(CURIYR,D,B)+LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
              *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)  )

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              CSWCONIN(CURIYR,D,B)=CSWCONIN(CURIYR,D,B)+( (&
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
              +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
              +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))))  )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
               (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                eqcsur(CURCALYR,reccl,b,D))
        ENDIF
 !
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
              +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
              +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
              *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) )
          ENDIF

 50   CONTINUE

      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
        If (driver2(CURIYR,R,B).gt.0)   &
           CSWCONIN(CURIYR,R,B)=                &
           CSWCONIN(CURIYR,R,B)/driver2(CURIYR,R,B)
        Enddo
      Enddo

      END SUBROUTINE RCWCON
!*******************************************************************
!     DISH WASHER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RDWTEC
      IMPLICIT NONE
      REAL*4 RTEFFAC(2)
      REAL*4 DISRT,HORIZON,DECAY,OPCOST(2)
      REAL*4 EQWTN(10,MNUMBLDG,MNUMCR),EQWTR(10,MNUMBLDG,MNUMCR), &
       TOTEWTN(4,MNUMBLDG,MNUMCR),TOTEWTR(4,MNUMBLDG,MNUMCR)
      REAL*4 DENOM, DENOM2, SUM,SUM1
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(5),eqtar(5)
!*******************************************************************
!   SET EU = 4 TO SEARCH THE DISH WASHER SECTION OF THE DATA
!
      EU=4
      EUPR=9
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      ALPHA1=-0.50
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   ZERO OUT ARRAYS
!
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CURCALYR,RECCL,B,R)=0.0
            WTEQCEFFR(CURCALYR,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE
!
!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
      DO 50 R=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
!
!
!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                rteffac(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)
                rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
              ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!
              OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(1)
              OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(2)

!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(4,R,CURCALYR).GT.PRICES(4,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(4,R,CURCALYR)/PRICES(4,R,RECSYEAR))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
!
!      CALCULATE LIFE CYCLE COSTS
!
              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!
                EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(2))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,2)))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST(1))+ &
                 (RTECBTA3(RECTY)*LFCY(EQT,B,R,1)))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
            ENDIF
           ENDIF
 50   CONTINUE
!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO 70 R=1,MNUMCR-2
        DO 70 B=1,MNUMBLDG
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
!
                  NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  TOTEWTN(EQC,B,R))
                  REQTSHR(CURCALYR,TYPE,B,R)=(EQWTR(EQT,B,R)/ &
                  TOTEWTR(EQC,B,R))
             ENDIF
            ENDIF
          ENDDO
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
!
!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              SUM1=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                 SUM=SUM+(NEQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
                SUM1=SUM1+(NEQTSHR(CURCALYR,TYPE,B,R)*LOADADJ(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
!
          ENDDO
!
 70   CONTINUE
      END SUBROUTINE RDWTEC
!*******************************************************************
!     DISHWASHERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RDWADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU       = 4 IS DISH WASHERS
!*******************************************************************
      EV       = 4
      EU       = 4
!*******************************************************************
!  CALCULATE DISH WASHERS ADDED IN CURCALYR (CURCALYR-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR 2005 VINTAGE PRIOR TO
!   CURCALYR
!*******************************************************************
! CUMULATE SURVIVING NEW CLOTHES WASHERS ADDED PRIOR TO CURCALYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURCALYR-1
! CUMULATE SURVIVING NEW WASHERS ADDED & REPLACED PRIOR TO CURCALYR
! REPLACE EQUIP = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-DISH WASHERS)
!*******************************************************************
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CURCALYR,RECCL,B,R)=0.0
            EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
            EQCSUR(CURCALYR,RECCL,B,R)=0.0
          ENDDO
!
       IF (CURCALYR.GT.RECSYEAR+1) THEN
          DISHNEW(CURCALYR,B,R)=DISHNEW(CURCALYR-1,B,R)*1.01
       END IF
       IF (DISHNEW(CURCALYR,B,R).GT.1.0000) THEN
          DISHNEW(CURCALYR,B,R)=1.0000
       END IF
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)* &
                                    DISHNEW(CURCALYR,B,R))
      SA=0.0
             EQCND90(CURCALYR,RECCL,B,R)=(EQCESE(RECSYEAR,RECCL,B,R) * &
             HDR(B)**(CURCALYR-(RECSYEAR))*(1.+DWPR)- &
             EQCESE(RECSYEAR,RECCL,B,R)*HDR(B)**(CURCALYR-(RECSYEAR)))
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF
!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+&
            EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF
         EQCRP90RP(CURCALYR,RECCL,B,R)=EQCRP90RP(CURCALYR,RECCL,B,R) + &
                                     EQCND90(CURCALYR,RECCL,B,R)
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP)) +&
               EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT CLOTHES WASHERS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
            EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)

!
         ENDDO
5     CONTINUE
!

         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE DISHWASHERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
!
      DO B=1,MNUMBLDG
       DO r=1,MNUMCR-2
        TYPE=RTTYPECT(EU)
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
           IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
           endif
           ENDIF
              ENDDO
           ENDDO
        ENDDO

       END SUBROUTINE RDWADD
!*******************************************************************
!  DISHWASHER CONSUMPTION
!*******************************************************************
      SUBROUTINE RDWCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, D,EU,EUPR,RECCL,EQC,F,Y,R
!*******************************************************************
!   SET EU = 4 TO SEARCH THE DISH WASHER SECTION OF THE DATA
!
      EU = 4
      EUPR=9
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15

!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New, REPLACEMENT, AND Average UECS
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                   ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
            EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             ( WTEQCEFFN(CURCALYR,RECCL,B,D)* RTBASEFF(RECSYEAR,RECCL))
            EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             ( WTEQCEFFR(CURCALYR,RECCL,B,D)* RTBASEFF(RECSYEAR,RECCL))
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
 30    CONTINUE
!*******************************************************************
!  Calculate DISH WASH Consumption
!*******************************************************************
      DO 40 D=1,MNUMCR-2
        DSWCON(CURIYR,D)=0.0
       DO 40 B=1,MNUMBLDG
        DSWCONIN(CURIYR,D,B)=0.
        driver2(CURIYR,d,B)=0.
        DSWCONWT(CURIYR,D,B)=0.
40    CONTINUE
!
      DO 50 D=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
!
              DSWCON(CURIYR,D)=DSWCON(CURIYR,D)+LEAPYR* ( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
            +(EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              DSWCONWT(CURIYR,D,B)=DSWCONWT(CURIYR,D,B)+LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
            +(EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

           IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
               DSWCONIN(CURIYR,D,B)=DSWCONIN(CURIYR,D,B)+(  (&
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
            +(EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)))) )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D))
         ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
            +(EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
            ELSE
!
              DSWCON(CURIYR,D)=DSWCON(CURIYR,D)+ LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
              (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)))* &
             RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              DSWCONWT(CURIYR,D,B)=DSWCONWT(CURIYR,D,B)+LEAPYR*(           &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
              (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)))* &
             RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              DSWCONIN(CURIYR,D,B)=DSWCONIN(CURIYR,D,B)+(  (&
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
              (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))))   )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
        ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)=LEAPYR* ( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)  )
          ENDIF
 50   CONTINUE

      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
        If (driver2(CURIYR,R,B).gt.0)   &
           DSWCONIN(CURIYR,R,B)=                &
           DSWCONIN(CURIYR,R,B)/driver2(CURIYR,R,B)
        Enddo
      Enddo

      END SUBROUTINE RDWCON
!*******************************************************************
!     WATER HEATER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RWHTEC
      IMPLICIT NONE
      REAL*4 TOTN(MNUMBLDG,MNUMCR)
      REAL*4 EQFSHRN(NHeatTypes,MNUMBLDG,MNUMCR),EQFSHRR(NHeatTypes,MNUMBLDG,MNUMCR)
      REAL*4 EQWTN(NHeatTypes,MNUMBLDG,MNUMCR),EQWTR(NHeatTypes,MNUMCR,MNUMCR)
      REAL*4 Temp  !  WHStandard
      REAL*4 DISRT,HORIZON,OPCOST(2)
      REAL*4 TOTEWTN(6,MNUMBLDG,MNUMCR), TOTEWTR(6,MNUMBLDG,MNUMCR)
      REAL*4 RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHT,RECTY,RECCL,RECCLHT,R,B,F,EQT,EQC,EQCHT,TYPE,COUNT,L
      INTEGER RECAR(20),EQTAR(20)
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      ALPHA1=-0.50

!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL
!     END USES. SET EU   = 3 FOR WATER HEATERS.
!               SET EUHT = 1 FOR SECTIONS USING SPACE HEATING DATA
!
      EU  = 5
      EUPR=3
      EUHT= 1
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!   ZERO-OUT ARRAYS
!
      DO 5 B=1,MNUMBLDG
        DO 5 R=1,MNUMCR-2
          TOTN(B,R)=0.0
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            NH2OSH(CURCALYR,EQC,B,R)=0.0
          ENDDO
!
!    SUM THE HEATER SHARES OVER ALL HEATER CLASSES
!
          DO RECCL=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)
            EQCHT=RTCLEQCL(RECCL)
            TOTN(B,R)=TOTN(B,R)+HSYSSHR(CURCALYR,EQCHT,B,R)
          ENDDO
 5    CONTINUE
!
      DO 100 R=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
!
!   CALCULATE WATER HEATER SHARES BY FUEL FOR NEW DISTRIBUTION
!
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            DO RECCLHT=RTCLEUPT(EUHT)+1,RTCLEUPT(EUHT+1)
!
!   ASSUME SAME SHARES FOR WATER HEATERS AS SPACE HEATERS WHOSE
!     RTCLPNTR POINTER POINTS FROM THE SPACE HEATER CLASS TO THE
!     WATER HEATER CLASS
!
              IF(RTCLPNTR(RECCLHT).EQ.EQC) THEN
                EQCHT=RTCLEQCL(RECCLHT)
              IF (TOTN(B,R).GT.0.0) THEN
                NH2OSH(CURCALYR,EQC,B,R)=NH2OSH(CURCALYR,EQC,B,R) &
                  +(HSYSSHR(CURCALYR,EQCHT,B,R)/TOTN(B,R))
              ELSE
                NH2OSH(CURCALYR,EQC,B,R)=0.0
              ENDIF
              ENDIF
            ENDDO
          ENDDO
!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN SPACE COOLING (EU=2)
!     RTTYEUPT(EU+1) = LAST RECORD IN WATER HEATING (EU=3)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!        TYPE (EQT), REC # FOR RECCL FILE (RECCL), AND FUEL TYPE (F)
!
              EQC   = RTTYEQCL(RECTY)
              EQT   = RTEQTYPE(RECTY)
              RECCL = RTCLEUPT(EU)+EQC
              F     = RTFUEL(RECCL)
!
!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                rteffac(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)
                rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
              ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!
!      CALCULATE OPERATING COST

!

              OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(1)
              OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(2)
!
!      CALCULATE LIFE CYCLE COSTS
!
              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF ((ELIGBLE.GT.0.0).AND. &
         (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR)) )THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CURCALYR)/PRICES(F,R,RECSYEAR))**ALPHA1 )
         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
       END IF
!
!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
!
              EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
              TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
              EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
              TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!
            ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT
!        TYPE (EQT)
!
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
!
!   SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
!
              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R)
              ELSE
                EQFSHRN(EQT,B,R)=0.0
              ENDIF
!
              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
              ELSE
                EQFSHRR(EQT,B,R)=0.0
              ENDIF
!
              NEQTSHR(CURCALYR,TYPE,B,R)=EQFSHRN(EQT,B,R)
              REQTSHR(CURCALYR,TYPE,B,R)=EQFSHRR(EQT,B,R)
          !Diagnostics only:
!          IF ((CURCALYR.ge.2014) .and. (b.eq.1) .and. (r.eq.1)) then
!             write(9,'("wh share n&r shares before",4i5,2e15.4)') curcalyr, eqc, eqt, type, eqfshrn(eqt,b,r),eqfshrr(eqt,b,r)
!          end if


            ENDIF
           ENDIF
          ENDDO


         !    Revise weights for 2015 water heater standard  !  WHStandard

            IF(CURCALYR.GE.2015) THEN
                ! Adjustments for Gas Water Heating (EQC=1)
                ! The standard requires a condensing gas WH for
                !  capacities greater than 56 gallons which comprise
                !  approximately 4% of the market -- if purchased share
                !  is less than 4% revise shares
                EQC=1
                If(EQFSHRN(4,B,R) .lt. .04) THEN
                  Temp=(1 +(EQFSHRN(4,B,R)-.04)/(1.-EQFSHRN(4,B,R)) )
                  EQFSHRN(1,B,R)=EQFSHRN(1,B,R)*temp
                  EQFSHRN(2,B,R)=EQFSHRN(2,B,R)*temp
                  EQFSHRN(3,B,R)=EQFSHRN(3,B,R)*temp
                  EQFSHRN(4,B,R)=.04
                Endif  ! was less than 4%
                If(EQFSHRR(4,B,R) .lt. .04) THEN
                  Temp=(1 +(EQFSHRR(4,B,R)-.04)/(1.-EQFSHRR(4,B,R)) )
                  EQFSHRR(1,B,R)=EQFSHRR(1,B,R)*temp
                  EQFSHRR(2,B,R)=EQFSHRR(2,B,R)*temp
                  EQFSHRR(3,B,R)=EQFSHRR(3,B,R)*temp
                  EQFSHRR(4,B,R)=.04
                Endif  ! was less than 4%

                ! Adjustments for Electric Water Heating (EQC=2)
                ! The standard requires a heat pump WH for
                !  capacities greater than 56 gallons which comprise
                !  approximately 9% of the market -- if purchased share
                !  is less than 9% revise shares
                EQC=2
                If(EQFSHRN(8,B,R)+EQFSHRN(9,B,R) .lt. .09) THEN
                  Temp=(1+ (EQFSHRN(8,B,R)+EQFSHRN(9,B,R)-.09)/(1.-EQFSHRN(8,B,R)-EQFSHRN(9,B,R)) )
                  EQFSHRN(5,B,R)=EQFSHRN(5,B,R)*Temp
                  EQFSHRN(6,B,R)=EQFSHRN(6,B,R)*Temp
                  EQFSHRN(7,B,R)=EQFSHRN(7,B,R)*Temp
                  Temp=.09/(EQFSHRN(8,B,R)+EQFSHRN(9,B,R))
                  EQFSHRN(8,B,R)=EQFSHRN(8,B,R)*Temp
                  EQFSHRN(9,B,R)=EQFSHRN(9,B,R)*Temp
                Endif  ! was less than 9%
                If(EQFSHRR(8,B,R)+EQFSHRR(9,B,R) .lt. .09) THEN
                  Temp=(1+ (EQFSHRR(8,B,R)+EQFSHRR(9,B,R)-.09)/(1.-EQFSHRR(8,B,R)-EQFSHRR(9,B,R)) )
                  EQFSHRR(5,B,R)=EQFSHRR(5,B,R)*Temp
                  EQFSHRR(6,B,R)=EQFSHRR(6,B,R)*Temp
                  EQFSHRR(7,B,R)=EQFSHRR(7,B,R)*Temp
                  Temp=.09/(EQFSHRR(8,B,R)+EQFSHRR(9,B,R))
                  EQFSHRR(8,B,R)=EQFSHRR(8,B,R)*Temp
                  EQFSHRR(9,B,R)=EQFSHRR(9,B,R)*Temp
                Endif  ! was less than 9%

               ! Reset Shares
               TYPE = RTTYPECT(EU)
               DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
                IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
                 CURCALYR.LE.RTLASTYR(RECTY)) THEN
                 IF (RTCENDIV(RECTY).EQ.R) THEN
                  TYPE=TYPE+1
                  EQC=RTTYEQCL(RECTY)
                  EQT=RTEQTYPE(RECTY)
                  NEQTSHR(CURCALYR,TYPE,B,R)=EQFSHRN(EQT,B,R)
                  REQTSHR(CURCALYR,TYPE,B,R)=EQFSHRR(EQT,B,R)
                  !Diagnostics only:
!                  IF ((CURCALYR.ge.2015) .and. (b.eq.1) .and. (r.eq.1)) then
!                   write(9,'("wh share n&r shares revised",4i5,2e15.4)') curcalyr, eqc, eqt, type, eqfshrn(eqt,b,r),eqfshrr(eqt,b,r)
!                  end if
                 ENDIF
                ENDIF
               ENDDO

           Endif !Revised WH for 2015 standard


!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM +EQFSHRN(EQT,B,R)
                DENOM2=DENOM2+EQFSHRR(EQT,B,R)
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRN(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            IF (WTEQCEFFN(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF
            ENDIF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRR(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
            IF (WTEQCEFFR(CURCALYR,RECCL,B,R).EQ.0.0) THEN
                WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            END IF
          ENDDO
100   CONTINUE
      END SUBROUTINE RWHTEC
!*******************************************************************
!     WATER HEATERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE REUADD
      IMPLICIT NONE
      REAL*4 SWT(RECSYEAR:ENDYR),SWF(RECSYEAR:ENDYR),WATERTOT(RECSYEAR:ENDYR,MNUMCR-2),COOKTOT(RECSYEAR:ENDYR,MNUMCR-2)
      REAL*4 SA, HSR, ESR, SVRTE, SHARE  ,SWFT
      REAL*4 EQSRT(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR-2),EQSR90T(RECSYEAR:ENDYR,NHeatTypes,MNUMBLDG,MNUMCR-2)
      INTEGER EQC,RECCL,TEMP,RECCLSW
      INTEGER Y,Y1,R,D, B,EV,TYPE,RECTY,NUMEQT,EQT,V
!
!   WHEN EU = 3, SEARCH THE WATER HEATING SECTION OF THE DATA
!   WHEN EU = 4, SEARCH THE STOVE (COOK) SECTION OF THE DATA
      IF (EU.EQ.5) EV = 5
      IF (EU.EQ.6) EV = 6

!*******************************************************************
!
!  ZERO OUT ARRAYS
!
      DO 1 R=1,MNUMCR-2
        DO 1 B=1,MNUMBLDG
          DO 1 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSR90(CURCALYR,RECCL,B,R)=0.0
            EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
            EQCSUR(CURCALYR,RECCL,B,R)=0.0
            IF(B.EQ.1) EQCREP(CURCALYR,RECCL,B,R) = 0.0
 1    CONTINUE
!*******************************************************************
!  Calculate Conventional Equipment Added in CURCALYR
!*******************************************************************
! CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CURCALYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURCALYR-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CURCALYR
! REPLACE EQUIP = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************
!
!*******************************************************************
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
!*******************************************************************
        DO 15 R=1,MNUMCR-2
         DO 15 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           SHARE = 1.0
           IF(EU.EQ.5) SHARE = NH2OSH(CURCALYR,EQC,B,R)
           IF(EU.EQ.6) SHARE = NCKSH(CURCALYR,EQC,B,R)
           EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)*SHARE)
           SA = 0.0
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF
!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT WATER HEATERS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
!  SUBROUTINE REPLACE DISTRIBUTES REPLACEMENTS IN POST-2005
!    SINGLE FAMILY HOMES WHEN LAST ARGUEMENT = 1
!
            IF(B.EQ.1) THEN
!  First, store what replacements would have been if no switching allowed.
               OEQCREP(CURCALYR,RECCL,1,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
!  Call REPLACE to distribute replacements.
               CALL REPLACE(EU,R,B,RECCL,1)
            ELSE
!  No switching allowed in multi-family or mobile homes.
                EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
            ENDIF

!
         ENDDO
 15     CONTINUE
!
!  The following call to REPLACE with final argument = 2  distributes
!    replacements in Existing Single Family Homes.
!
      B = 1
      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          OEQCRP90(CURCALYR,RECCL,B,R) = EQCRP90(CURCALYR,RECCL,1,R)
          OEQCRP90R(CURCALYR,RECCL,B,R) = EQCRP90RP(CURCALYR,RECCL,1,R)

          CALL REPLACE(EU,R,B,RECCL,2)

        ENDDO
      ENDDO

      B = 1
      DO  R=1,MNUMCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         SWITCHTO(CURCALYR,RECCL,B,R)=0.0
         SWITCHTOR(CURCALYR,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          IF (RECCLSW.NE.RECCL) THEN
      SWITCHTO(CURCALYR,RECCL,B,R)=SWITCHTO(CURCALYR,RECCL,B,R)+ &
                              EQCSW90(CURCALYR,RECCLSW,RECCL,B,R)
      SWITCHTOR(CURCALYR,RECCL,B,R)=SWITCHTOR(CURCALYR,RECCL,B,R)+ &
                              EQCSW90R(CURCALYR,RECCLSW,RECCL,B,R)
          ENDIF
         ENDDO
       ENDDO
      ENDDO
!
      B = 1
      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!
      EQCRP90(CURCALYR,RECCL,B,R)= EQCRP90(CURCALYR,RECCL,B,R)- &
        SWITCHES(CURCALYR,RECCL,B,R)
       EQCRP90RP(CURCALYR,RECCL,B,R)= EQCRP90RP(CURCALYR,RECCL,B,R)- &
        SWITCHESR(CURCALYR,RECCL,B,R)+ SWITCHTOR(CURCALYR,RECCL,B,R) &
       + SWITCHTO(CURCALYR,RECCL,B,R)
        ENDDO
      ENDDO
       B=1
         SWF(CURCALYR)=0.0
         SWT(CURCALYR)=0.0
      DO  R=1,MNUMCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      SWT(CURCALYR)=SWT(CURCALYR)+SWITCHTO(CURCALYR,RECCL,B,R)+ &
                             SWITCHTOR(CURCALYR,RECCL,B,R)
      SWF(CURCALYR)=SWF(CURCALYR)+SWITCHES(CURCALYR,RECCL,B,R)+ &
                             SWITCHESR(CURCALYR,RECCL,B,R)
       ENDDO
      ENDDO
      IF (CURCALYR.EQ.ENDYR) THEN
          SWFT=0.0
        DO Y=RECSYEAR+1,ENDYR
          SWFT=SWFT+SWF(Y)
        ENDDO
      ENDIF
         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE WATER HEATING SYSTEMS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

            DO B=1,MNUMBLDG
             DO r=1,MNUMCR-2
              TYPE=RTTYPECT(EU)
              DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              !     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
              IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
              CURCALYR.LE.RTLASTYR(RECTY)) THEN
              IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1      ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
           endif
           endif
          ENDDO
           ENDDO
        ENDDO

       IF (EU.EQ.5) THEN
         IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,MNUMCR-2
            WATERTOT(RECSYEAR,R)=0.0
             DO B=1,MNUMBLDG
              WATERTOT(RECSYEAR,R)=WATERTOT(RECSYEAR,R)+EQCESE(RECSYEAR,19,B,R)
             END DO
           END DO
         ELSE
           DO R=1,MNUMCR-2
            WATERTOT(CURCALYR,R)=0.0
             DO B=1,MNUMBLDG
              WATERTOT(CURCALYR,R)=WATERTOT(CURCALYR,R)+EQCESE(CURCALYR,19,B,R)+EQCADD(CURCALYR,19,B,R)+&
                EQCRP90(CURCALYR,19,B,R)+EQCRP90RP(CURCALYR,19,B,R)+EQCSUR(CURCALYR,19,B,R)+EQCREP(CURCALYR,19,B,R)+&
                EQCSR90(CURCALYR,19,B,R)
             END DO
           END DO
         END IF
        ELSE
         IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,MNUMCR-2
            COOKTOT(RECSYEAR,R)=0.0
             DO B=1,MNUMBLDG
              COOKTOT(RECSYEAR,R)=COOKTOT(RECSYEAR,R)+EQCESE(RECSYEAR,24,B,R)
             END DO
           END DO
         ELSE
           DO R=1,MNUMCR-2
            COOKTOT(CURCALYR,R)=0.0
             DO B=1,MNUMBLDG
              COOKTOT(CURCALYR,R)=COOKTOT(CURCALYR,R)+EQCESE(CURCALYR,24,B,R)+EQCADD(CURCALYR,24,B,R)+&
                EQCRP90(CURCALYR,24,B,R)+EQCRP90RP(CURCALYR,24,B,R)+EQCSUR(CURCALYR,24,B,R)+EQCREP(CURCALYR,24,B,R)+&
                EQCSR90(CURCALYR,24,B,R)
             END DO
           END DO
         END IF
       END IF
!
! Proxy for gas customers is gas water heating for CDs 1,2,7 and 9
        DO R=1,MNUMCR-2
         IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) then
          IF (EU.EQ.5) THEN
            IF (WATERTOT(RECSYEAR,R).GT.RSGASCUST(RECSYEAR,R)) THEN
              RSGASCUST(RECSYEAR,R)=WATERTOT(RECSYEAR,R)
            END IF
          ELSE
            IF (COOKTOT(RECSYEAR,R).GT.RSGASCUST(RECSYEAR,R)) THEN
              RSGASCUST(RECSYEAR,R)=COOKTOT(RECSYEAR,R)
            END IF
          END IF
        ELSE
          IF (EU.EQ.5) THEN
            IF (WATERTOT(CURCALYR,R).GT.RSGASCUST(CURCALYR,R)) THEN
              RSGASCUST(CURCALYR,R)=WATERTOT(CURCALYR,R)
            END IF
          ELSE
            IF (COOKTOT(CURCALYR,R).GT.RSGASCUST(CURCALYR,R)) THEN
              RSGASCUST(CURCALYR,R)=COOKTOT(CURCALYR,R)
            END IF
          END IF
        END IF
       END DO
      END SUBROUTINE REUADD
!*******************************************************************
!  HOT WATER CONSUMPTION
!*******************************************************************
      SUBROUTINE RWHCON
      IMPLICIT NONE
      REAL*4 HOUSES(RECSYEAR:ENDYR,MNUMCR)
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP,HHSELAS,HHSIZE(RECSYEAR:ENDYR,MNUMCR-2)
      REAL*4 CHECK(RECSYEAR:ENDYR,4),DCHECK(RECSYEAR:ENDYR,4)
      INTEGER B, D, EU, EUPR, EQC, RECCL, V, F, FCON,Y,FD,R
      INTEGER RECCLSWH, RECCLEWH,EQCSWH, EQCEWH,EQCCW
! Gas,El,Oil,LPG,SOLAR
!*******************************************************************
!  WATER Heaters - 1=GAS 2=EL 3=OIL 4=LPG 5=SOLAR
!*******************************************************************
!
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTON OF THE DATA
!
      EU     = 5
      EUPR=3
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      HHSELAS=.315  ! People per house elast for hot water use (lbl)
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New, REPLACEMENT, and Average UEC
!*******************************************************************
      IF(CURCALYR.EQ.RECSYEAR+1) THEN
        DO 3 D=1,MNUMCR-2
          HOUSES(RECSYEAR,D)=0.0
          DO 3 B=1,MNUMBLDG
            HOUSES(RECSYEAR,D)=HOUSES(RECSYEAR,D)+EH(RECSYEAR,B,D)
 3      CONTINUE
      ENDIF

      DO 5 D=1,MNUMCR-2
        HOUSES(CURCALYR,D)=0.0
        DO 5 B=1,MNUMBLDG
          HOUSES(CURCALYR,D)=HOUSES(CURCALYR,D)+EH(CURCALYR,B,D)+ &
            NH(CURCALYR,B,D)
 5    CONTINUE
!
!*******************************************************************
!  Calculate People per house
!*******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      DO 6 D=1,MNUMCR-2
          HHSIZE(RECSYEAR,D)=MC_NP16A(D,RECSYEAR-BASEYR+1)/HOUSES(RECSYEAR,D)
 6    CONTINUE
      END IF
!
        DO 7 D=1,MNUMCR-2
          HHSIZE(CURCALYR,D)=MC_NP16A(D,CURIYR)/HOUSES(CURCALYR,D)
 7      CONTINUE
!*******************************************************************
!  Calculate New, REPLACEMENT, and Average UEC
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
       EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
       ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS)* &
        ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
!
       EQCSIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
        ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
!
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
         IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
       EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS) * &
        WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
       EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
         ELSE
       EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
          ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS)
       EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
         END IF
      ELSE
              IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
           EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                 ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))** HHSELAS)* &
              WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
           EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
              WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
              ELSE
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
              ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS)
                EQCNIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
              END IF
      END IF

!
            IF (WTEQCEFFR(CURCALYR,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
       ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS)* &
        WTEQCEFFR(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
              EQCRIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        WTEQCEFFR(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
            ELSE
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
              ((HHSIZE(CURCALYR,D)/HHSIZE(RECSYEAR,D))**HHSELAS)
              EQCRIUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
            END IF
!
            IF (CURCALYR .EQ. RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
            ELSE
         TEMP=0.0
           DO Y=RECSYEAR+1,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR+1,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
          ENDDO
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR+1,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR+1,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!*******************************************************************
!  Calculate Water Heating SHARES OF DISHWASERS AND CLOTHES WASHERS
!*******************************************************************
      DO D=1,MNUMCR-2
         DO B=1,MNUMBLDG
            HOTWATQ(CURCALYR,B,D)=0.0
           DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC  = RTCLEQCL(RECCL)
             HOTWATQ(CURCALYR,B,D)=HOTWATQ(CURCALYR,B,D)+ &
              EQCESE(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) +&
              EQCRP90RP(CURCALYR,RECCL,B,D)+EQCREP(CURCALYR,RECCL,B,D) +&
              EQCSUR(CURCALYR,RECCL,B,D)+ EQCSR90(CURCALYR,RECCL,B,D)
         ENDDO
        ENDDO
      ENDDO

!*******************************************************************
!  Calculate Water Heating Consumption
!*******************************************************************
      DO 20 D=1,MNUMCR-2
         DO 19 B=1,MNUMBLDG
         DO 19 FCON=1,5
          H2OCONWT(CURIYR,FCON,D,B)=0.
          driver(curiyr,fcon,d,b)=0.
          H2OCONIN(CURIYR,FCON,D,B)=0.
 19   Continue
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          H2OCON(CURIYR,EQC,D)=0.0
 20   CONTINUE
!*******************************************************************
!
!  FIND INDICES FOR THE ELECTRIC AND SOLAR WATER HEATERS
!    USED TO COMPUTE H2OCON FOR SOLAR FUEL (FCON=5)
!
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH')THEN
          EQCEWH=RTCLEQCL(RECCL)
          RECCLEWH=EQCEWH+RTCLEUPT(EU)
        ELSEIF(RTCLNAME(RECCL).EQ.'SOLAR_WH')THEN
          EQCSWH=RTCLEQCL(RECCL)
          RECCLSWH=EQCSWH+RTCLEUPT(EU)
        ENDIF
      ENDDO
!
      DO 100 D=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
          DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC  = RTCLEQCL(RECCL)
            F    = RTFUEL(RECCL)
            FCON = FWHCON(F)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
       H2OCON(CURIYR,FCON,D)=H2OCON(CURIYR,FCON,D)+ LEAPYR*( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+ &
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCADD(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

       H2OCONWT(CURIYR,FCON,D,B)=H2OCONWT(CURIYR,FCON,D,B)+LEAPYR*( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+&
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCADD(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
       H2OCONIN(CURIYR,FCON,D,B)=H2OCONIN(CURIYR,FCON,D,B)+(          &
       ((1.-CWLOAD(RECSYEAR))*EQCSIUEC(CURCALYR,RECCL,B,D)+&
        (EQCSIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNIUEC(CURCALYR,RECCL,B,D)+&
        (EQCNIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCADD(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRIUEC(CURCALYR,RECCL,B,D)+&
        (EQCRIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D))

       driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D))
       ENDIF

!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+&
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCADD(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
            ELSE
!
           IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           END IF
!
               H2OCON(CURIYR,FCON,D)=H2OCON(CURIYR,FCON,D)+ LEAPYR*( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+&
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*(EQCADD(CURCALYR,RECCL,B,D)+EQCRP90RP(CURCALYR,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYEAR))*EQCAUEC(CURCALYR,RECCL,B,D)+&
        (EQCAUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*(EQCREP(CURCALYR,RECCL,B,D)+EQCSUR(CURCALYR,RECCL,B,D)+EQCSR90(CURCALYR,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

              H2OCONWT(CURIYR,FCON,D,B)=H2OCONWT(CURIYR,FCON,D,B)+LEAPYR*( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+&
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*(EQCADD(CURCALYR,RECCL,B,D)+EQCRP90RP(CURCALYR,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYEAR))*EQCAUEC(CURCALYR,RECCL,B,D)+&
        (EQCAUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*(EQCREP(CURCALYR,RECCL,B,D)+EQCSUR(CURCALYR,RECCL,B,D)+EQCSR90(CURCALYR,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
               H2OCONIN(CURIYR,FCON,D,B)=H2OCONIN(CURIYR,FCON,D,B)+(  &
       ((1.-CWLOAD(RECSYEAR))*EQCSIUEC(CURCALYR,RECCL,B,D)+&
        (EQCSIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNIUEC(CURCALYR,RECCL,B,D)+&
        (EQCNIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*(EQCADD(CURCALYR,RECCL,B,D)+EQCRP90RP(CURCALYR,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRIUEC(CURCALYR,RECCL,B,D)+&
        (EQCRIUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYEAR))*EQCAUEC(CURCALYR,RECCL,B,D)+&
        (EQCAUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*(EQCREP(CURCALYR,RECCL,B,D)+EQCSUR(CURCALYR,RECCL,B,D)+EQCSR90(CURCALYR,RECCL,B,D)))

         driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
       ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)=   LEAPYR* ( &
       ((1.-CWLOAD(RECSYEAR))*EQCSUEC(CURCALYR,RECCL,B,D)+&
        (EQCSUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*EQCESE(CURCALYR,RECCL,B,D) + &
       ((1.-CWLOAD(RECSYEAR))*EQCNUEC(CURCALYR,RECCL,B,D)+&
        (EQCNUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*(EQCADD(CURCALYR,RECCL,B,D)+EQCRP90RP(CURCALYR,RECCL,B,D)) + &
       ((1.-CWLOAD(RECSYEAR))*EQCRUEC(CURCALYR,RECCL,B,D)+&
        (EQCRUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         NCWLOAD(CURCALYR,D,B)))*EQCRP90(CURCALYR,RECCL,B,D)+ &
       ((1.-CWLOAD(RECSYEAR))*EQCAUEC(CURCALYR,RECCL,B,D)+&
        (EQCAUEC(CURCALYR,RECCL,B,D)*CWLOAD(RECSYEAR)*&
         ECWLOAD(CURCALYR,D,B)))*(EQCREP(CURCALYR,RECCL,B,D)+EQCSUR(CURCALYR,RECCL,B,D)+EQCSR90(CURCALYR,RECCL,B,D)))* &
              RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
            ENDIF
!
!    SOLAR IS CALCULATED DIFFERENTLY
!
            FCON = FWHCON(8)   ! = 5
!
            H2OCON(CURIYR,5,D)=H2OCON(CURIYR,5,D)+LEAPYR* &
             (EQCESE(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412. )+ &
              EQCRP90(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            + EQCADD(CURCALYR,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) +&
          (EQCREP(CURCALYR,RECCLSWH,B,D)+EQCRP90RP(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            +(EQCSR90(CURCALYR,RECCLSWH,B,D)+ &
              EQCSUR(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.))
!
            SLEQCN(CURIYR,1,B,D)=LEAPYR*&
             (EQCESE(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412. )+ &
              EQCRP90(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            + EQCADD(CURCALYR,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) +&
          (EQCREP(CURCALYR,RECCLSWH,B,D)+EQCRP90RP(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            +(EQCSR90(CURCALYR,RECCLSWH,B,D)+ &
              EQCSUR(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.))
 100  CONTINUE

!*******************************************************************
!  Calculate Solar Water Heating Consumption
!*******************************************************************
      DO 110 D=1,MNUMCR-2
        SLCON(CURIYR,D)=0.0
       DO 110 B=1,MNUMBLDG
        SLCON(CURIYR,D)=SLCON(CURIYR,D)+LEAPYR*&
             (EQCESE(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCUEC(D,RECCLSWH,B))*FossilHR/3412. )+ &
              EQCRP90(CURCALYR,RECCLSWH,B,D)* &
            ((EQCUEC(D,RECCLEWH,B)-EQCRUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            + EQCADD(CURCALYR,RECCLSWH,B,D)* &
                   ((EQCUEC(D,RECCLEWH,B)-EQCHVUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) +&
          (EQCREP(CURCALYR,RECCLSWH,B,D)+EQCRP90RP(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCNUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.) &
            +(EQCSR90(CURCALYR,RECCLSWH,B,D)+ &
              EQCSUR(CURCALYR,RECCLSWH,B,D))* &
            ((EQCUEC(D,RECCLEWH,B)-EQCAUEC(CURCALYR,RECCLSWH,B,D))*FossilHR/3412.))
 110  CONTINUE


      DO R=1,MNUMCR-2
        DO FCON=1,5
         DO B=1,MNUMBLDG
          If (driver(CURIYR,FCON,R,B).gt.0)   &
           H2OCONIN(CURIYR,FCON,R,B)=                &
           H2OCONIN(CURIYR,FCON,R,B)/driver(CURIYR,FCON,R,B)
         Enddo
        Enddo
      Enddo


      END SUBROUTINE RWHCON

!*******************************************************************
!     STOVE CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RSTVTEC
      IMPLICIT NONE
      REAL*4 NEQTSHRD(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR), &
             REQTSHRD(RECSYEAR:ENDYR+1,MNUMRTTY,MNUMBLDG,MNUMCR)
      REAL*4 DISRT,HORIZON,OPCOST(2)
      REAL*4 NGNGFACT(MNUMBLDG),NGELFACT
      REAL*4 RTEFFAC(2),DECAY,DENOM,SUM,DENOM2,SUM2
      REAL*4 EQFSHRN(9,MNUMBLDG,MNUMCR),EQFSHRR(9,MNUMBLDG,MNUMCR)
      REAL*4 EQWTN(9,MNUMBLDG,MNUMCR),EQWTR(9,MNUMCR,MNUMCR)
      REAL*4 TOTEWTN(9,MNUMBLDG,MNUMCR), TOTEWTR(9,MNUMBLDG,MNUMCR)
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,EUPR,EUHW,RECTY,RECCL,RECCLHW,R,B,F,EQT,EQC,EQCHW, &
              TYPE,COUNT,RECAR(5),eqtar(5),L
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      ALPHA1=-0.50
!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!    SET THE FOLLOWING FACTORS
!     NGNGFACT = .65 (65% WITH NG WATER HEATERS HAVE NG STOVES)
!     NGELFACT =  1-.65 (35% WITH NG WATER HEATERS HAVE ELEC STOVES)
!
      NGNGFACT(1)= 0.44
      NGNGFACT(2)= 0.15
      NGNGFACT(3)= 1.00
!      NGELFACT = 1.00 - NGNGFACT
!
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL
!     END USES. SET EU   = 6 FOR COOKING.
!               SET EUHW = 5 FOR WATER HEATING
      EU   = 6
      EUPR=4
      !EUHW links stove choice to water heating fuel
      EUHW = 5
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!  ZERO OUT ARRAYS
!
      DO 10 R=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            NCKSH(CURCALYR,EQC,B,R) =0.0
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO
10    CONTINUE
!*******************************************************************
!     CALCULATE NCKSH (G,L,E)FOR NEW DISTRIBUTION
!*******************************************************************
      DO 100 R=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            DO RECCLHW=RTCLEUPT(EUHW)+1,RTCLEUPT(EUHW+1)
              IF(RTCLPNTR(RECCLHW).EQ.EQC.OR. &
                 RTCLREPL(RECCLHW).EQ.EQC) THEN
                EQCHW=RTCLEQCL(RECCLHW)
                IF(RTCLNAME(RECCLHW).EQ.'NG_WH'.AND. &
                   RTCLNAME(RECCL).EQ.'NG_STV') THEN
                  NCKSH(CURCALYR,EQC,B,R)=NCKSH(CURCALYR,EQC,B,R)+ &
                         NH2OSH(CURCALYR,EQCHW,B,R)*NGNGFACT(B)
                ELSEIF &
                  (RTCLNAME(RECCLHW).EQ.'NG_WH'.AND. &
                   RTCLNAME(RECCL).EQ.'ELEC_STV') THEN
                  NCKSH(CURCALYR,EQC,B,R)=NCKSH(CURCALYR,EQC,B,R)+ &
                     NH2OSH(CURCALYR,EQCHW,B,R)*(1-NGNGFACT(B))
                ELSE
                  NCKSH(CURCALYR,EQC,B,R)=NCKSH(CURCALYR,EQC,B,R)+ &
                         NH2OSH(CURCALYR,EQCHW,B,R)
                ENDIF
              ENDIF
            ENDDO
          ENDDO
!*******************************************************************
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!     RTTYEUPT(EU)   = LAST RECORD IN WATER HEATING (EU=5)
!     RTTYEUPT(EU+1) = LAST RECORD IN COOKING       (EU=6)
!*******************************************************************
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQC  = RTTYEQCL(RECTY)
              EQT  = RTEQTYPE(RECTY)
              RECCL= RTCLEUPT(EU)+EQC
              F    = RTFUEL(RECCL)
!
!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
              IF(RTEQEFF(RECTY).NE.0.0) THEN
                RTEFFAC(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
              ELSE
                RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
                RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
              ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(F,R,CURCALYR)/PRICES(F,R,RECSYEAR))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
!      CALCULATE OPERATING COST
!
              OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(1)
              OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
                    *RTEFFAC(2)
!
!      CALCULATE LIFE CYCLE COSTS
!
              LFCY(EQT,B,R,1)=CAPITAL + (OPCOST(1) *DECAY)
              LFCY(EQT,B,R,2)=CAPITAL + (OPCOST(2) *DECAY)
!*******************************************************************
!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
!
              EQWTN(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(2)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
              TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
              EQWTR(EQT,B,R)= EXP(RTECBIAS(RECTY)+(BETA1DR(RECTY)* &
                        CAPITAL)+(RTECBTA2(RECTY)*OPCOST(1)) + &
                        ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
              TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!
            ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
            IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
               CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT
!        TYPE (EQT)
!
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
!
!   SET EQUIPMENT FUEL SHARE (AND NEQTSHR FOR WATER HEATING)
!
              IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
                EQFSHRN(EQT,B,R)=EQWTN(EQT,B,R) /TOTEWTN(EQC,B,R)
              ELSE
                EQFSHRN(EQT,B,R)=0.0
              ENDIF
!
              IF (TOTEWTR(EQC,B,R).GT.0.0) THEN
                EQFSHRR(EQT,B,R)=EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R)
              ELSE
                EQFSHRR(EQT,B,R)=0.0
              ENDIF
!
              NEQTSHR(CURCALYR,TYPE,B,R)=EQFSHRN(EQT,B,R)
              REQTSHR(CURCALYR,TYPE,B,R)=EQFSHRR(EQT,B,R)
            ENDIF
           ENDIF
          ENDDO
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM +EQFSHRN(EQT,B,R)
                DENOM2=DENOM2+EQFSHRR(EQT,B,R)
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRN(EQT,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQFSHRR(EQT,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
          ENDDO
100   CONTINUE
      END SUBROUTINE RSTVTEC
!******************************************************************
!     STOVES ADDED SUBROUTINE HANDLED BY SUB REUADD(EU)
!******************************************************************
!
!*******************************************************************
!  STOVE (COOK) CONSUMPTION
!*******************************************************************
      SUBROUTINE RSTOVCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, D, FCON, RECCL, EU, EUPR, V, F,EV,TYPE,&
             RECTY,EQT,NUMEQT,EQC,R,Y
!*******************************************************************
!   EU  = 6 IS COOKING
!
      EV=4
      EU=6
      EUPR=4
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15
!
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New and Average UEC
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
         EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
              EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                ( RTBASEFF(RECSYEAR,RECCL)/WTEQCEFFN(CURCALYR,RECCL,B,D))
              ELSE
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
              END IF
            ELSE
              IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                 ( RTBASEFF(RECSYEAR,RECCL)/WTEQCEFFN(CURCALYR,RECCL,B,D))
              ELSE
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
              END IF
            END IF
!
            IF (WTEQCEFFR(CURCALYR,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
               (RTBASEFF(RECSYEAR,RECCL)/WTEQCEFFR(CURCALYR,RECCL,B,D))
            ELSE
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
            END IF
!
            IF (CURCALYR .EQ. RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
          ENDDO
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!*******************************************************************
!  Calculate Cooking Consumption
!*******************************************************************
      DO 20 D=1,MNUMCR-2
        DO 20 FCON=1,NSTVFL
          CKCON(CURIYR,FCON,D)=0.0
            DO 20 B=1,MNUMBLDG
             CKCONWT(CURIYR,FCON,D,B)=0.
             driver(curiyr,fcon,d,b)=0.
             CKCONIN(CURIYR,FCON,D,B)=0.
 20   CONTINUE
!*******************************************************************
      DO 100 D=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
          DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            F=RTFUEL(RECCL)
            FCON=FSTVCON(F)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              CKCON(CURIYR,FCON,D)=CKCON(CURIYR,FCON,D)+LEAPYR* ( &
              ( (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
                (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)))) &
                 * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
              CKCONWT(CURIYR,FCON,D,B)=CKCONWT(CURIYR,FCON,D,B)+ LEAPYR*   ( &
              ( (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
                (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)))) &
                 * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
              CKCONIN(CURIYR,FCON,D,B)=CKCONIN(CURIYR,FCON,D,B)+(       (&
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
                (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)))) )

         driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) )
        ENDIF

!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*&
              ( EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
                EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
            ELSE
              CKCON(CURIYR,FCON,D)=CKCON(CURIYR,FCON,D)+LEAPYR* &
             (  EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)+ &
                EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)+ &
                EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
              CKCONWT(CURIYR,FCON,D,B)= CKCONWT(CURIYR,FCON,D,B) + LEAPYR*&
             (  EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)+ &
                EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)+ &
                EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              CKCONIN(CURIYR,FCON,D,B)= CKCONIN(CURIYR,FCON,D,B) +      (&
             (  EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)+ &
                EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)+ &
                EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))   )

            driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
              ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*&
              ( EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)+ &
                EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D)+ &
              EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
               EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)+ &
                EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)+ &
                EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D) ) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
            ENDIF
 100  CONTINUE

      DO R=1,MNUMCR-2
        DO FCON=1,NSTVFL
         DO B=1,MNUMBLDG
          If (driver(CURIYR,FCON,R,B).gt.0)   &
           CKCONIN(CURIYR,FCON,R,B)=                &
           CKCONIN(CURIYR,FCON,R,B)/driver(CURIYR,FCON,R,B)
         Enddo
        Enddo
      Enddo

       END SUBROUTINE RSTOVCON
!*******************************************************************
!     DRYER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RDRYTEC
      IMPLICIT NONE
      REAL*4 EQWTN(6,MNUMBLDG,MNUMCR),TOTEWTN(4,MNUMBLDG,MNUMCR)
      REAL*4 EQWTR(6,MNUMBLDG,MNUMCR),TOTEWTR(4,MNUMBLDG,MNUMCR)
      REAL*4 DISRT,HORIZON,OPCOST(2)
      REAL*4 CONVFACT,RTEFFAC(2),DECAY,ECTEMP,DENOM,SUM,DENOM2,SUM2
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER EU,RECTY,RECCL,R,B,F,EQT,EQC,REQT,TYPE, &
              COUNT,L
      INTEGER RECAR(5),eqtar(5)
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON & CONVERSION FACTOR
!       FOR COMPUTING NEQTSHR FOR CLOTHES DRYERS
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      CONVFACT=100.0
      ALPHA1=-0.50
!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   THE SAME GENERAL FORM OF THIS SUBROUTINE WORKS FOR ALL END USES.
!     SET EU = 7 TO SEARCH THE CLOTHES DRYING SECTION OF THE DATA
!
      EU  = 7
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,5)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!   ZERO-OUT ARRAYS
!
      DO 5 B=1,MNUMBLDG
        DO 5 R=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
          ENDDO
5     CONTINUE
!*******************************************************************
!     RTTYEUPT(EU)   = LAST RECORD IN STOVES         (EU=4)
!     RTTYEUPT(EU+1) = LAST RECORD IN CLOTHES DRYING (EU=5)
!     RECTY          = RECORD NUMBER IN RSMEQP FILE
!*******************************************************************
    DO 40 R=1,MNUMCR-2
     DO 40 B=1,MNUMBLDG
      DO 40 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
        IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC), EQUIPMENT
!        TYPE (EQT), REC # FOR RECTY FILE, AND FUEL TYPE (F)
!
        EQC=RTTYEQCL(RECTY)
        EQT=RTEQTYPE(RECTY)
!
!    RECCL = RECORD NUMBER IN THE RSCLASS FILE
!    F     = FUEL NUMBER FOR THE CURRENT EQUIPMENT CLASS
!
        RECCL=RTCLEUPT(EU)+EQC
        F    =RTFUEL(RECCL)
!
!     COMPUTE EFFICIENCY FACTOR USED IN COMPUTING OPERATING COST
!
        IF(RTEQEFF(RECTY).NE.0.0) THEN
          rteffac(1)=EQCEFF(CURCALYR,reccl)/RTEQEFF(RECTY)
          rteffac(2)=RTBASEFF(RECSYEAR,reccl)/RTEQEFF(RECTY)
        ELSE
          RTEFFAC(1)=RTBASEFF(RECSYEAR,RECCL)
          RTEFFAC(2)=RTBASEFF(RECSYEAR,RECCL)
        ENDIF
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
        IF (COSTTRSW.EQ.1) THEN
          CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
        ELSE
          CAPITAL = RTEQCOST(RECTY)
        ENDIF
!
!     COMPUTE THE PART OF THE EQUIMENT CHOICE WEIGHT NOT DEPENDENT
!       ON REGION AND BUILDING TYPE
!       NOTE: ALL BIASES CURRENTLY SET TO 0.0 FOR CLOTHES DRYING
!

!
!
!      CALCULATE OPERATING COST
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
        ((PRICES(F,R,CURCALYR)/PRICES(F,R,RECSYEAR))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
          OPCOST(1)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
             *RTEFFAC(1)
          OPCOST(2)=PRICES(F,R,CURCALYR)*EQCUEC(R,RECCL,B) &
             *RTEFFAC(2)
!
        ECTEMP = RTECBIAS(RECTY) + BETA1DR(RECTY)*CAPITAL

!      CALCULATE LIFE CYCLE COSTS
!
          LFCY(EQT,B,R,1)=RTEQCOST(RECTY) + (OPCOST(1) *DECAY)
          LFCY(EQT,B,R,2)=RTEQCOST(RECTY) + (OPCOST(2) *DECAY)
!
!    COMPUTE WEIGHTS FOR NEW AND REPLACEMENT EQUIPMENT TYPES
!
            EQWTN(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(2)) + &
            ( RTECBTA3(RECTY)*LFCY(EQT,B,R,2) ) )
            TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
            EQWTR(EQT,B,R)= EXP(ECTEMP+(RTECBTA2(RECTY)*OPCOST(1)) + &
            ( RTECBTA3(RECTY)*LFCY(EQT,B,R,1) ) )
            TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!
       ENDIF
      ENDIF
 40   CONTINUE
!
!*******************************************************************
!
        DO 45 R=1,MNUMCR-2
         DO 45 B=1,MNUMBLDG
          TYPE=RTTYPECT(EU)
          DO 45 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
        IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
        TYPE=TYPE+1
!
!      FIND VALID INDICES FOR EQUIPMENT CLASS (EQC) & EQUIPMENT
!        TYPE (EQT)
!
        EQC=RTTYEQCL(RECTY)
        EQT=RTEQTYPE(RECTY)
!
!
!   SET NEW EQUIPMENT SHARE
!
          IF (TOTEWTN(EQC,B,R).GT.0.0) THEN
            RECCL=RTCLEUPT(EU)+EQC
            NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/TOTEWTN(EQC,B,R))
!     *        *(DRYSHR(EQC,B,R)/CONVFACT)
            REQTSHR(CURCALYR,TYPE,B,R)=(EQWTR(EQT,B,R)/TOTEWTR(EQC,B,R))
          ELSE
            NEQTSHR(CURCALYR,TYPE,B,R)=0.0
            REQTSHR(CURCALYR,TYPE,B,R)=0.0
          ENDIF
      ENDIF
      ENDIF
45    CONTINUE
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY
!*******************************************************************
      DO 90 R=1,MNUMCR-2
        DO 90 B=1,MNUMBLDG
          DO 90 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
            DO 80 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(RTTYEQCL(RECTY).EQ.EQC.AND.CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                COUNT=COUNT+1
                EQT=RTEQTYPE(RECTY)
                RECAR(COUNT)=RECTY
                EQTAR(COUNT)=EQT
                DENOM =DENOM+EQWTN(EQT,B,R)
                DENOM2=DENOM2+EQWTR(EQT,B,R)
              ENDIF
              ENDIF
80          CONTINUE
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM=SUM+(EQWTN(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
!
!
!    COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=1/RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM2=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                EQT=EQTAR(L)
                SUM2=SUM2+(EQWTR(EQT,B,R)/RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM2/DENOM2
            ENDIF
 90    CONTINUE
      END SUBROUTINE RDRYTEC
!*******************************************************************
!     DRYERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RDRYADD
      IMPLICIT NONE
      REAL*4 SWT(RECSYEAR:ENDYR),SWF(RECSYEAR:ENDYR),DRYERTOT(RECSYEAR:ENDYR,MNUMCR-2)
      REAL*4 SA, HSR, ESR, SVRTE, P
      INTEGER EV,NUMEQT,RECCLSW
      INTEGER EU,EQC,EQT,RECCL,RECTY,TYPE,Y,R,B,TEMP,V,Y1
!*******************************************************************
!   SET EU = 7 TO SEARCH THE CLOTHES DRYING SECTION OF THE DATA
!
      EV = 7  !check use of EV below
      EU = 7
!*******************************************************************
!  DRYER Shares - 1=NG1 2=NG2 3=EL1 4=EL2
!  DRYERS - 1=NG 2=EL
!*******************************************************************
!  ZERO OUT ARRAYS
!
      DO 1 R=1,MNUMCR-2
        DO 1 B=1,MNUMBLDG
          DO 1 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCADD(CURCALYR,RECCL,B,R)=0.0
            EQCSR90(CURCALYR,RECCL,B,R)=0.0
            EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
            EQCSUR(CURCALYR,RECCL,B,R)=0.0
            EQCREP(CURCALYR,RECCL,B,R) = 0.0
 1    CONTINUE
!*******************************************************************
!  Calculate Conventional Equipment Added in CURCALYR
!*******************************************************************
! CUMULATE SURVIVING NEW EQUIPMENT ADDED PRIOR TO CURCALYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURCALYR-1
! CUMULATE SURVIVING NEW EQUIPMENT ADDED & REPLACED PRIOR TO CURCALYR
! REPLACE EQUIP = SURV.HOUSES(SA) -  SURV.EQUIP(EQCSUR)
!*******************************************************************
!*******************************************************************
!     Calculate Dryers Added in CURCALYR (CURCALYR-1)
!*******************************************************************
      DO 7 R=1,MNUMCR-2
        DO 7 B=1,MNUMBLDG
          TYPE=RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQC=RTTYEQCL(RECTY)
              EQT=RTEQTYPE(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
       IF (CURCALYR.GT.RECSYEAR+1) THEN
          NEWDRYSAT(CURCALYR,2,B,R)=NEWDRYSAT(CURCALYR-1,2,B,R)*1.01
          NEWDRYSAT(CURCALYR,1,B,R)=NEWDRYSAT(CURCALYR-1,1,B,R)
       END IF
       IF ((NEWDRYSAT(CURCALYR,1,B,R)+NEWDRYSAT(CURCALYR,2,B,R)).GT.1.0000) THEN
          NEWDRYSAT(CURCALYR,2,B,R)=NEWDRYSAT(CURCALYR-1,2,B,R)
       END IF
              EQCADD(CURCALYR,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) + &
                (NEQTSHR(CURCALYR,TYPE,B,R) &
                *HSEADD(CURCALYR,B,R)*NEWDRYSAT(CURCALYR,EQC,B,R)))
            ENDIF
           ENDIF
          ENDDO
 7    CONTINUE
!
!*******************************************************************
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
!*******************************************************************
        DO 15 R=1,MNUMCR-2
         DO 15 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           SA = 0.0
             IF (RTCLNAME(RECCL).EQ.'ELEC_DRY') THEN
             EQCND90(CURCALYR,RECCL,B,R)=EQCESE(RECSYEAR,RECCL,B,R) * &
             HDR(B)**(CURCALYR-(RECSYEAR))*(1.+ELDRYPR(B,R))- &
             EQCESE(RECSYEAR,RECCL,B,R)*HDR(B)**(CURCALYR-(RECSYEAR))
             ELSE
             EQCND90(CURCALYR,RECCL,B,R)=0.0
             ENDIF
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF


!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS *NEW TO AEO2000*
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCND90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
          ENDIF
!
         EQCRP90RP(CURCALYR,RECCL,B,R)=EQCRP90RP(CURCALYR,RECCL,B,R) + &
                                     EQCND90(CURCALYR,RECCL,B,R)
             IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCND90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP)) ))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT WATER HEATERS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
!  SUBROUTINE REPLACE DISTRIBUTES REPLACEMENTS IN POST-2005
!    SINGLE FAMILY HOMES WHEN LAST ARGUEMENT = 1
!
            IF(B.EQ.1) THEN
!  First, store what replacements would have been if no switching allowed.
               OEQCREP(CURCALYR,RECCL,1,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
!  Call REPLACE to distribute replacements.
               CALL REPLACE(EU,R,B,RECCL,1)
            ELSE
!  No switching allowed in multi-family or mobile homes.
                EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)
            ENDIF

!
         ENDDO
 15     CONTINUE
!
!  The following call to REPLACE with final argument = 2  distributes
!    replacements in Existing Single Family Homes.
!
      B = 1
      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          OEQCRP90(CURCALYR,RECCL,B,R) = EQCRP90(CURCALYR,RECCL,1,R)
          OEQCRP90R(CURCALYR,RECCL,B,R) = EQCRP90RP(CURCALYR,RECCL,1,R)

         CALL REPLACE(EU,R,B,RECCL,2)

        ENDDO
      ENDDO

       B = 1
      DO  R=1,MNUMCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         SWITCHTO(CURCALYR,RECCL,B,R)=0.0
         SWITCHTOR(CURCALYR,RECCL,B,R)=0.0
          DO RECCLSW=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          IF (RECCLSW.NE.RECCL) THEN
      SWITCHTO(CURCALYR,RECCL,B,R)=SWITCHTO(CURCALYR,RECCL,B,R)+ &
                              EQCSW90(CURCALYR,RECCLSW,RECCL,B,R)
      SWITCHTOR(CURCALYR,RECCL,B,R)=SWITCHTOR(CURCALYR,RECCL,B,R)+ &
                              EQCSW90R(CURCALYR,RECCLSW,RECCL,B,R)
          ENDIF
         ENDDO
       ENDDO
      ENDDO
!
      B = 1
      DO  R=1,MNUMCR-2
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)

      EQCRP90(CURCALYR,RECCL,B,R)= EQCRP90(CURCALYR,RECCL,B,R)- &
        SWITCHES(CURCALYR,RECCL,B,R)
       EQCRP90RP(CURCALYR,RECCL,B,R)= EQCRP90RP(CURCALYR,RECCL,B,R)- &
        SWITCHESR(CURCALYR,RECCL,B,R)+ SWITCHTOR(CURCALYR,RECCL,B,R) &
       + SWITCHTO(CURCALYR,RECCL,B,R)
        ENDDO
      ENDDO
       B=1
         SWF(CURCALYR)=0.0
         SWT(CURCALYR)=0.0
      DO  R=1,MNUMCR-2
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      SWT(CURCALYR)=SWT(CURCALYR)+SWITCHTO(CURCALYR,RECCL,B,R)+ &
                             SWITCHTOR(CURCALYR,RECCL,B,R)
      SWF(CURCALYR)=SWF(CURCALYR)+SWITCHES(CURCALYR,RECCL,B,R)+ &
                             SWITCHESR(CURCALYR,RECCL,B,R)
       ENDDO
      ENDDO
!

         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE CLOTHES DRYERS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

            DO B=1,MNUMBLDG
             DO r=1,MNUMCR-2
              TYPE=RTTYPECT(EU)
               DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
                !CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
                IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
                   CURCALYR.LE.RTLASTYR(RECTY)) THEN
                 IF (RTCENDIV(RECTY).EQ.R) THEN
                 TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
                 EQT=RTEQTYPE(RECTY)
                 EQC=RTTYEQCL(RECTY)
                 RECCL=RTCLEUPT(EU)+EQC
                 HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                 HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                             REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )
                 endif
                endif
             ENDDO
           ENDDO
        ENDDO

        IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) THEN
           DO R=1,MNUMCR-2
            DRYERTOT(RECSYEAR,R)=0.0
             DO B=1,MNUMBLDG
              DRYERTOT(RECSYEAR,R)=DRYERTOT(RECSYEAR,R)+EQCESE(RECSYEAR,27,B,R)
             END DO
           END DO
         ELSE
           DO R=1,MNUMCR-2
            DRYERTOT(CURCALYR,R)=0.0
             DO B=1,MNUMBLDG
              DRYERTOT(CURCALYR,R)=DRYERTOT(CURCALYR,R)+EQCESE(CURCALYR,27,B,R)+EQCADD(CURCALYR,27,B,R)+&
                EQCRP90(CURCALYR,27,B,R)+EQCRP90RP(CURCALYR,27,B,R)+EQCSUR(CURCALYR,27,B,R)+EQCREP(CURCALYR,27,B,R)+&
                EQCSR90(CURCALYR,27,B,R)
             END DO
           END DO
        END IF
!
! Proxy for gas customers is gas water heating for CDs 1,2,7 and 9
        DO R=1,MNUMCR-2
         IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) then
            IF (DRYERTOT(RECSYEAR,R).GT.RSGASCUST(RECSYEAR,R)) THEN
              RSGASCUST(RECSYEAR,R)=DRYERTOT(RECSYEAR,R)
            END IF
         ELSE
            IF (DRYERTOT(CURCALYR,R).GT.RSGASCUST(CURCALYR,R)) THEN
              RSGASCUST(CURCALYR,R)=DRYERTOT(CURCALYR,R)
            END IF
         END IF
        END DO
        END SUBROUTINE RDRYADD
!*******************************************************************
!  DRYER CONSUMPTION
!*******************************************************************
      SUBROUTINE RDRYCON
      IMPLICIT NONE
      REAL*4 TEMP,ALPHA,ef1,ef2,ef3
      INTEGER B, E, D, F, EQC, RECCL, EU, EUPR,FCON,Y,R
!*******************************************************************
!  DRYERS
!
          EU = 7             ! CLOTHES DRYER SECTION OF THE DATA
          EUPR=5
!
!*******************************************************************
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New and Average UEC
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
         EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
        ( RTBASEFF(RECSYEAR,RECCL) / RTBASEFF(CURCALYR,RECCL) )
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
              EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
              ELSE
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
              END IF
            ELSE
              IF (WTEQCEFFN(CURCALYR,RECCL,B,D).GT.0.0) THEN
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
              ELSE
                EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
              END IF
            END IF
!
            IF (WTEQCEFFR(CURCALYR,RECCL,B,D).GT.0.0) THEN
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B) * &
                WTEQCEFFR(CURCALYR,RECCL,B,D)*RTBASEFF(RECSYEAR,RECCL)
            ELSE
              EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)
            END IF
!
            IF (CURCALYR .EQ. RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
          ENDDO
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!
!*******************************************************************
!  Calculate Dryer Consumption
!*******************************************************************
      DO 20 D=1,MNUMCR-2
        DO 20 FCON=1,NDRYFL
          DRYCON(CURIYR,FCON,D)=0.0
         Do 20 B=1,MNUMBLDG
          DRYCONIN(CURIYR,FCON,D,B)=0.
          driver(CURIYR,FCON,D,B)=0.
          DRYCONWT(CURIYR,FCON,D,B)=0.
 20   CONTINUE
!*******************************************************************
      DO 100 D=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
           DO 100 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            F=RTFUEL(RECCL)
            FCON=FDRYCON(F)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              DRYCON(CURIYR,FCON,D)=DRYCON(CURIYR,FCON,D)+LEAPYR* ( &
               (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
              DRYCONWT(CURIYR,FCON,D,B)=DRYCONWT(CURIYR,FCON,D,B)+ LEAPYR* ( &
               (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)


       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
              DRYCONIN(CURIYR,FCON,D,B)=DRYCONIN(CURIYR,FCON,D,B)+(      (&
               (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) )

         driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) )
       ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)=LEAPYR* ( &
               (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
            ELSE
!
           IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           END IF
!
              DRYCON(CURIYR,FCON,D)=DRYCON(CURIYR,FCON,D)+LEAPYR*( &
               (EQCESE(CURCALYR,RECCL,B,D) *EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
               (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) ) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
              DRYCONWT(CURIYR,FCON,D,B)=DRYCONWT(CURIYR,FCON,D,B)+ LEAPYR*( &
               (EQCESE(CURCALYR,RECCL,B,D) *EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
               (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) ) &
              * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!
       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              DRYCONIN(CURIYR,FCON,D,B)=DRYCONIN(CURIYR,FCON,D,B)+ (     ( &
               (EQCESE(CURCALYR,RECCL,B,D) *EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
               (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) ) )

         driver(curiyr,fcon,d,b)=driver(curiyr,fcon,d,b)+                   &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
         ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)=LEAPYR* ( &
               (EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))+ &
               (EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) ) &
            * RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
            ENDIF
 100  CONTINUE


      DO R=1,MNUMCR-2
        DO FCON=1,NDRYFL
         DO B=1,MNUMBLDG
          If (driver(CURIYR,FCON,R,B).gt.0)   &
           DRYCONIN(CURIYR,FCON,R,B)=                &
           DRYCONIN(CURIYR,FCON,R,B)/driver(CURIYR,FCON,R,B)
         Enddo
        Enddo
      Enddo

      END SUBROUTINE RDRYCON


!*******************************************************************
!     REFRIGERATOR CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RREFTEC
      IMPLICIT NONE
      REAL*4 TMFSHR(MNUMBLDG,MNUMCR-2)   !share for top-mounted freezers
      REAL*4 SMFSHR(MNUMBLDG,MNUMCR-2)   !share for side-mounted freezers
      REAL*4 BMFSHR(MNUMBLDG,MNUMCR-2)   !share for bottom-mounted freezers
      Real*4 smf_shr,tmf_shr,bmf_shr
      REAL*4 DISRT,HORIZON,DECAY,OPCOST1,LFCYCLE,FACTOR,UEC(MNUMRTTY)
      real*4 EQWTN(10,MNUMBLDG,MNUMCR),EQWTR(10,MNUMBLDG,MNUMCR), &
       TOTEWTN(4,MNUMBLDG,MNUMCR),TOTEWTR(4,MNUMBLDG,MNUMCR)
      REAL*4 DENOM, DENOM2, SUM
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
      INTEGER RECAR(7),eqtar(7)
!*******************************************************************
!   SET EU = 8 TO SEARCH THE FOOD REFIGERATION SECTION OF THE DATA
!
      EU=8
      EUPR=6
      ALPHA1=-0.50
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!   SET THE MARKET SHARE FOR REFRIGERATORS WITH SIDE-MOUNTED AND BOTTOM-MOUNTED FREEZERS
!
      SMF_SHR=0.25                      !TechUpdate
        BMF_SHR=0.25                      !TechUpdate
      TMF_SHR=1.- SMF_SHR - BMF_SHR
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
!
!    ASSIGN FACTOR TO CONVERT FROM REFRIGERATOR EFFICIENCY TO UEC
!
      FACTOR=.003412
!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   ZERO OUT ARRAYS
!
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CURCALYR,RECCL,B,R)=0.0
            WTEQCEFFR(CURCALYR,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE
!
!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
       DO 50 R=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
              UEC(RECTY)=RTEQEFF(RECTY)*FACTOR
                !
                !  If COSTTRSW = 1, use function EQCOST to compute capital
                !     cost of new equipment.
                !  If COSTTRSW = 0, use constant value from RSMEQP file for capital
                !     cost of new equipment.
                !
                     IF (COSTTRSW.EQ.1) THEN
                       CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
                     ELSE
                       CAPITAL = RTEQCOST(RECTY)
                     ENDIF
                     !
                     !     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
                     !
                     IF ((CURCALYR.GT.2008).AND. &
                              (PRICES(F,R,CURCALYR).GT.PRICES(F,R,RECSYEAR))) THEN
                      HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
                      ELIGBLE=HRDRATE - 0.07
                          IF (ELIGBLE.GT.0.0) THEN
                            HRDADJ= ELIGBLE * &
                              ((PRICES(4,R,CURCALYR)/PRICES(4,R,RECSYEAR))**ALPHA1 )

                            BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
                           ELSE
                            BETA1DR(RECTY)=RTECBTA1(RECTY)
                          END IF  !eligible .gt.0
                       ELSE
                        BETA1DR(RECTY)=RTECBTA1(RECTY)
                     END IF    !curcalyr .gt. 2008


                        OPCOST1=PRICES(F,R,CURCALYR)*UEC(RECTY)

                        LFCYCLE= CAPITAL+(OPCOST1*DECAY)
                        EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                         (BETA1DR(RECTY)*CAPITAL)+ &
                         (RTECBTA2(RECTY)*OPCOST1)+ &
                         (RTECBTA3(RECTY)*LFCYCLE))
                        TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                        EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                         (BETA1DR(RECTY)*CAPITAL)+ &
                         (RTECBTA2(RECTY)*OPCOST1)+ &
                         (RTECBTA3(RECTY)*LFCYCLE))
                        TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
!              ENDIF  !.NE.'REF_TTD'
            ENDIF   !census division check
          ENDIF   !year validity check
 50   CONTINUE


!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
          TMFSHR(B,R) = 0.
          BMFSHR(B,R) = 0.
          SMFSHR(B,R) = 0.
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
               if(eqt.le.3) then
                  TMFSHR(B,R) = TMFSHR(B,R) + EQWTN(EQT,B,R)
                elseif (eqt.eq.4 .or. eqt.eq.5) then
                  SMFSHR(B,R) = SMFSHR(B,R) + EQWTN(EQT,B,R)
                else
                  BMFSHR(B,R) = BMFSHR(B,R) + EQWTN(EQT,B,R)
               endif
             ENDIF !rtcendiv=r
           ENDIF ! curcalyr filter for equipment
          ENDDO !recty

        Enddo !B
       Enddo !R

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
!      write(9,*) 'refrigerator efficiencies, new and replacement'
      DO 70 R=1,MNUMCR-2
        DO 70 B=1,MNUMBLDG
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
               if(eqt.le.3) then
                 NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  TMFSHR(B,R))*TMF_SHR
                 REQTSHR(CURCALYR,TYPE,B,R)= NEQTSHR(CURCALYR,TYPE,B,R) ! choices the same for refs!!!
                elseif (eqt.eq.4 .or. eqt.eq.5) then
                 NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  SMFSHR(B,R))* SMF_SHR
                 REQTSHR(CURCALYR,TYPE,B,R)= NEQTSHR(CURCALYR,TYPE,B,R) ! choices the same for refs!!!
                else
                 NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                  BMFSHR(B,R))* BMF_SHR
                 REQTSHR(CURCALYR,TYPE,B,R)= NEQTSHR(CURCALYR,TYPE,B,R) ! choices the same for refs!!!
               endif ! eqt filters for different types of refrigerators
            ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
!
!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF  ! rttyeqcl=eqc filter refrigerators
               ENDIF   ! rtcendiv=r   filter regions
              ENDIF    ! filter years
            ENDDO      ! process RSMEQP records

!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
!          Write(9,5006) r,b,type,curcalyr,wteqceffn(curcalyr,reccl,b,r),recty,reccl
! 5006     format("new  r= ",i3," b= ",i3," type= ",2i5,f10.4,2i6)


!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
!          Write(9,5007) r,b,type,curcalyr,wteqceffr(curcalyr,reccl,b,r),recty,reccl
! 5007     format("rep  r= ",i3," b= ",i3," type= ",2i5,f10.4,2i6)

!
          ENDDO  !eqc = refrigerators



!
 70   CONTINUE  !end loop r, end loop b

      END SUBROUTINE RREFTEC

!*******************************************************************
!     REFRIGERATORS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RREFADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EV,EQT,NUMEQT,TYPE, &
              RECTY,V
!*******************************************************************
!   EU       = 8 IS FOOD REFRIGERATION
!
      EV       = 8
      EU       = 8
!*******************************************************************
!  CALCULATE REFRIGERATORS ADDED IN CURCALYR (CURCALYR-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR 2005 VINTAGE PRIOR TO
!   CURCALYR
!*******************************************************************
! CUMULATE SURVIVING NEW REFRIGERATORS ADDED PRIOR TO CURCALYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURCALYR-1
! CUMULATE SURVIVING NEW REFRIGS ADDED & REPLACED PRIOR TO CURCALYR
! REPLACE EQUIP = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-REFRIGERATORS)
!*******************************************************************
      DO 15 R=1,MNUMCR-2
        DO 15 B=1,MNUMBLDG
          DO 15 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
             EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)*REFSAT(B,R))
!
!   ZERO OUT ARRAYS AND VARIABLE
!
           EQCSR90(CURCALYR,RECCL,B,R)=0.0
           EQCSUR(CURCALYR,RECCL,B,R)=0.0
           EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
           SA=0.0
!******************************************************************
!  Calculate replacement equipment from original base year stock
!******************************************************************
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
      EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR))))
      ENDIF
!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT REFRIGERATORS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
               EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)

!
!         ENDDO
 15     CONTINUE
!

         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE REFRIGERATORS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)
!
            DO B=1,MNUMBLDG
              DO r=1,MNUMCR-2
        TYPE=RTTYPECT(EU)
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
           IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
                HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
                HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                               REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r) )
           endif
           endif
          ENDDO
           ENDDO
        ENDDO

       END SUBROUTINE RREFADD
!*******************************************************************
!  REFRIGERATION CONSUMPTION
!*******************************************************************
      SUBROUTINE RREFCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, EUPR, D,EU,RECCL,EQC,F,Y,R
!*******************************************************************
!   SET EU = 8 TO SEARCH THE FOOD REFIGERATION SECTION OF THE DATA
!
      EU = 8
      EUPR=6
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New, REPLACEMENT, AND Average UECS
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
                   ( RTBASEFF(CURCALYR,RECCL) / RTBASEFF(RECSYEAR,RECCL) )
            EQCNUEC(CURCALYR,RECCL,B,D)=(EQCUEC(D,RECCL,B)* &
             (WTEQCEFFN(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL)))*(1.0/REFSAT(B,D))+ &
               EQCUEC(D,RECCL,B)*(1.0-(1.0/REFSAT(B,D)))
            EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
             (WTEQCEFFR(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL))
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!*******************************************************************
!  Calculate Refrigerator Consumption
!*******************************************************************
      DO 40 D=1,MNUMCR-2
            REFCON(CURIYR,D)=0.0
       DO 40 B=1,MNUMBLDG
            REFCONWT(CURIYR,D,B)=0.
            driver2(CURIYR,d,B)=0.
            REFCONIN(CURIYR,D,B)=0.
40    CONTINUE
!
      DO 50 D=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
!
              REFCON(CURIYR,D)=REFCON(CURIYR,D)+ LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              REFCONWT(CURIYR,D,B)=REFCONWT(CURIYR,D,B)+LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
              REFCONIN(CURIYR,D,B)=REFCONIN(CURIYR,D,B)+(  (&
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))))  )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) )
         ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
             (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) &
             +(EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
            ELSE
!
              REFCON(CURIYR,D)=REFCON(CURIYR,D)+ LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) )
!
              REFCONWT(CURIYR,D,B)=REFCONWT(CURIYR,D,B)+ LEAPYR* ( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
              *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) )

       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              REFCONIN(CURIYR,D,B)= REFCONIN(CURIYR,D,B)+( (&
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))))   )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
           ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
             ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
            *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
          ENDIF
 50   CONTINUE

      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
        If (driver2(CURIYR,R,B).gt.0)   &
           REFCONIN(CURIYR,R,B)=                &
           REFCONIN(CURIYR,R,B)/driver2(CURIYR,R,B)
        Enddo
      Enddo

      END SUBROUTINE RREFCON
!*******************************************************************
!     FREEZER CHOICE SUBROUTINE
!*******************************************************************
      SUBROUTINE RFRZTEC
      IMPLICIT NONE
      REAL*4 CH_SHR
      REAL*4 CHSHR(MNUMBLDG,MNUMCR-2)  ! share for chest freezers
      REAL*4 UP_SHR
      REAL*4 UPSHR(MNUMBLDG,MNUMCR-2)  ! share for upright freezers
      REAL*4 DISRT,HORIZON,DECAY,OPCOST2,LFCYCLE1,FACTOR,UEC(MNUMRTTY)
      REAL*4 EQWTN(10,MNUMBLDG,MNUMCR),EQWTR(10,MNUMBLDG,MNUMCR), &
       TOTEWTN(4,MNUMBLDG,MNUMCR),TOTEWTR(4,MNUMBLDG,MNUMCR)
      REAL*4 DENOM,DENOM2,SUM
      REAL*4 EQCOST,CAPITAL,RETAIL
      INTEGER RECAR(5),eqtar(5)
      INTEGER R,F,B,EU,EUPR,RECTY,EQT,TYPE,RECCL,EQC,COUNT,L
!*******************************************************************
!   SET EU = 9 TO SEARCH THE FREEZER SECTION OF THE DATA
!
      EU=9
      EUPR=7
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!
!   SET THE MARKET SHARE FOR UPRIGHT FREEZERS
!
        UP_SHR=0.40             !TechUpdate
        CH_SHR=1. - UP_SHR
!*******************************************************************
!     SET DISCOUNT RATE & PRESENT VALUE HORIZON
!*******************************************************************
      DISRT=.20
      HORIZON=7.0
      ALPHA1=-0.50
!
!    ASSIGN FACTOR TO CONVERT FROM FREEZER EFFICIENCY TO UEC
!
      FACTOR=.003412
!
!     COMPUTE DECAY RATE USED TO COMPUTE LIFE CYCLE COST
!       (FIRST ITERATION ONLY)
!
      IF(CURITR.EQ.1) THEN
        DECAY = (1-((1+DISRT)**(-HORIZON)))/DISRT
      ENDIF
!
!   ZERO OUT ARRAYS
!
      DO 5 R=1,MNUMCR-2
        DO 5 B=1,MNUMBLDG
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            TOTEWTN(EQC,B,R)=0.0
            TOTEWTR(EQC,B,R)=0.0
            WTEQCEFFN(CURCALYR,RECCL,B,R)=0.0
            WTEQCEFFR(CURCALYR,RECCL,B,R)=0.0
          ENDDO
 5    CONTINUE
!
!*******************************************************************
!     CALCULATE OPERATING COSTS
!     CALCULATE LIFE CYCLE COSTS
!     CALCULATE EQUIPMENT WEIGHT & TOTAL EQUIPMENT WEIGHT
!*******************************************************************
       DO 50 R=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              F    =RTFUEL(RECCL)
              UEC(RECTY)=RTEQEFF(RECTY)*FACTOR
              OPCOST2=PRICES(F,R,CURCALYR)*UEC(RECTY)
!
!  If COSTTRSW = 1, use function EQCOST to compute capital
!     cost of new equipment.
!  If COSTTRSW = 0, use constant value from RSMEQP file for capital
!     cost of new equipment.
!
                IF (COSTTRSW.EQ.1) THEN
                  CAPITAL = EQCOST(RECTY,CURCALYR,"CAP")
                ELSE
                  CAPITAL = RTEQCOST(RECTY)
                ENDIF
!
                LFCYCLE1= CAPITAL+(OPCOST2*DECAY)
!
!     CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOUR CHANGES
!
      IF ((CURCALYR.GT.2008).AND. &
               (PRICES(4,R,CURCALYR).GT.PRICES(4,R,RECSYEAR))) THEN
       HRDRATE=RTECBTA1(RECTY)/RTECBTA2(RECTY)
       ELIGBLE=HRDRATE - 0.07
        IF (ELIGBLE.GT.0.0) THEN
         HRDADJ= ELIGBLE * &
            ((PRICES(4,R,CURCALYR)/PRICES(4,R,RECSYEAR))**ALPHA1 )

         BETA1DR(RECTY) = (HRDADJ+0.07) * RTECBTA2(RECTY)
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
        END IF
        ELSE
         BETA1DR(RECTY)=RTECBTA1(RECTY)
      END IF
!
                EQWTN(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST2)+ &
                 (RTECBTA3(RECTY)*LFCYCLE1))
                TOTEWTN(EQC,B,R)=TOTEWTN(EQC,B,R)+EQWTN(EQT,B,R)
                EQWTR(EQT,B,R)=EXP (RTECBIAS(RECTY)+ &
                 (BETA1DR(RECTY)*CAPITAL)+ &
                 (RTECBTA2(RECTY)*OPCOST2)+ &
                 (RTECBTA3(RECTY)*LFCYCLE1))
                TOTEWTR(EQC,B,R)=TOTEWTR(EQC,B,R)+EQWTR(EQT,B,R)
              ENDIF
           ENDIF
 50   CONTINUE
!      ENDIF

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
          CHSHR(B,R)=0.
          UPSHR(B,R)=0.
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE=TYPE+1
              EQT=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              IF(EQT.le.2) THEN
                CHSHR(B,R) = CHSHR(B,R) + EQWTN(EQT,B,R)
               ELSE
                UPSHR(B,R) = UPSHR(B,R) + EQWTN(EQT,B,R)
              ENDIF
             ENDIF !rtcendiv=r
           ENDIF ! curcalyr filter for equipment
          ENDDO !recty

        Enddo !B
       Enddo !R

!*******************************************************************
!     CALCULATE NEW AND REPLACEMENT MARKET SHARES
!*******************************************************************
      DO 70 R=1,MNUMCR-2
        DO 70 B=1,MNUMBLDG
          TYPE = RTTYPECT(EU)
          DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
            IF(CURCALYR.GE.RTINITYR(RECTY).AND.CURCALYR.LE.RTLASTYR(RECTY)) &
              THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
              TYPE =TYPE+1
              EQT  =RTEQTYPE(RECTY)
              EQC  =RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC

              IF(EQT.le.2) THEN
                  NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                   CHSHR(B,R))*CH_SHR
                  REQTSHR(CURCALYR,TYPE,B,R)= NEQTSHR(CURCALYR,TYPE,B,R) ! choices the same for frz!!!
                ELSE
                  NEQTSHR(CURCALYR,TYPE,B,R)=(EQWTN(EQT,B,R)/ &
                   UPSHR(B,R))*UP_SHR
                  REQTSHR(CURCALYR,TYPE,B,R)= NEQTSHR(CURCALYR,TYPE,B,R) ! choices the same for frz!!!
              ENDIF
            ENDIF
           ENDIF
          ENDDO
!
!*******************************************************************
!     CALCULATE WEIGHTED EFFICIENCY FOR NEW EQUIPMENT AND
!     CALCULATE WEIGHTED EFFICIENCY FOR REP EQUIPMENT
!*******************************************************************
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC   =RTCLEQCL(RECCL)
            DENOM =0
            DENOM2=0
            COUNT =0
!
!     TYPE = INDEX FOR TYPE ARRAYS NEQTSHR AND REQTSHR
!            INITIALIZE TO LAST ARRAY POSTION IN PREVIOUS END USE
!              AND THEN COUNT VALID TYPES IN CURRENT END USE
!
            TYPE = RTTYPECT(EU)
            DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
              IF(CURCALYR.GE.RTINITYR(RECTY) &
                .AND.CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
                TYPE=TYPE+1
                IF(RTTYEQCL(RECTY).EQ.EQC) THEN
                  COUNT=COUNT+1
                  EQT=RTEQTYPE(RECTY)
                  RECAR(COUNT)=RECTY
                  EQTAR(COUNT)=TYPE
                  DENOM=DENOM+NEQTSHR(CURCALYR,TYPE,B,R)
                  DENOM2=DENOM2+REQTSHR(CURCALYR,TYPE,B,R)
                ENDIF
              ENDIF
             ENDIF
            ENDDO
!
!    COMPLETE CALCULATION FOR NEW EQUIPMENT
!
            IF(DENOM.LE.0.0) THEN
              WTEQCEFFN(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(NEQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFN(CURCALYR,RECCL,B,R)=SUM/DENOM
            ENDIF
!
!     COMPLETE CALCULATION FOR REPLACEMENT EQUIPMENT
!
            IF(DENOM2.LE.0.0) THEN
              WTEQCEFFR(CURCALYR,RECCL,B,R)=RTBASEFF(RECSYEAR,RECCL)
            ELSE
              SUM=0.0
              DO L=1,COUNT
                RECTY=RECAR(L)
                TYPE=EQTAR(L)
                SUM=SUM+(REQTSHR(CURCALYR,TYPE,B,R)*RTEQEFF(RECTY))
              ENDDO
              WTEQCEFFR(CURCALYR,RECCL,B,R)=SUM/DENOM2
            ENDIF
!
          ENDDO
!
 70   CONTINUE
      END SUBROUTINE RFRZTEC
!*******************************************************************
!     FREEZERS ADDED SUBROUTINE
!*******************************************************************
      SUBROUTINE RFRZADD
      IMPLICIT NONE
      REAL*4 SA, HSR, ESR, SVRTE, FZRPFAC
      INTEGER EU,EQC,RECCL,Y,R,B,TEMP,EQT,NUMEQT,TYPE, &
              EV,RECTY,V
!*******************************************************************
!     EU      = 9 IS FOOD FREEZING
!     FZRPFAC = FACTOR AFFECTING FREEZER REPLACEMENTS
!
      EV      = 9
      EU      = 9
      FZRPFAC = 1.0
!*******************************************************************
!  CALCULATE FREEZERS ADDED IN CURCALYR (CURCALYR-1)
!  CUMULATE SURVIVING EQUIPMENT REPLACED FOR 2005 VINTAGE PRIOR TO
!   CURCALYR
!*******************************************************************
! CUMULATE SURVIVING NEW FREEZERS ADDED PRIOR TO CURCALYR
!   TO ESTIMATE NH
! SA REPRESENTS NH at CURCALYR-1
! CUMULATE SURVIVING NEW FREEZERS ADDED & REPLACED PRIOR TO CURCALYR
! REPLACE EQUIP = SURV.HOUSES(SA) - SURV.EQUIP(EQCSUR-REFRIGERATORS)
!*******************************************************************
      DO 15 R=1,MNUMCR-2
        DO 15 B=1,MNUMBLDG
          DO 15 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
           EQC=RTCLEQCL(RECCL)
           EQCADD(CURCALYR,RECCL,B,R)=(HSEADD(CURCALYR,B,R)*FRZSAT(B,R))
!
!   ZERO OUT ARRAYS AND VARIABLE
!
           EQCSR90(CURCALYR,RECCL,B,R)=0.0
           EQCRP90RP(CURCALYR,RECCL,B,R)=0.0
           EQCSUR(CURCALYR,RECCL,B,R)=0.0
           SA=0.0
      IF (CURCALYR.EQ.RECSYEAR+1) THEN
      EQCRP90(CURCALYR,RECCL,B,R)=(EQCRET(CURCALYR,RECCL) &
        *EQCESE(RECSYEAR,RECCL,B,R)*(HDR(B)**(CURCALYR-(RECSYEAR)))*FZRPFAC)
      ELSE
      EQCRP90(CURCALYR,RECCL,B,R)=((EQCRET(CURCALYR,RECCL)- &
                   EQCRET(CURCALYR-1,RECCL))*EQCESE(RECSYEAR,RECCL,B,R)* &
                                 (HDR(B)**(CURCALYR-(RECSYEAR)))*FZRPFAC)
      ENDIF
!
!   COMPUTE AND VINTAGE REPLACEMENTS OF REPLACEMENTS
!
           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
              ESR=SVRTE(RTALPHA(RECCL),TEMP-1,RTK(RECCL),RTLAMBDA(RECCL)) &
                   -SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL),RTLAMBDA(RECCL))
         EQCRP90RP(CURCALYR,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) + &
          ( EQCRP90(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))+ &
            EQCRP90RP(Y,RECCL,B,R)*(ESR)*(HDR(B)**(TEMP))))
              ENDDO
           ENDIF

           IF(CURCALYR.GT.RECSYEAR+1) THEN
              DO Y=RECSYEAR+1,CURCALYR-1
                TEMP=CURCALYR-Y
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQCSR90(CURCALYR,RECCL,B,R)=(EQCSR90(CURCALYR,RECCL,B,R) + &
             ( EQCRP90(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))+ &
               EQCRP90RP(Y,RECCL,B,R)*ESR*(HDR(B)**(TEMP))))
                HSR=HDR(B)**(TEMP)
                SA = (SA + EQCADD(Y,RECCL,B,R)*HSR)
                EQCSUR(CURCALYR,RECCL,B,R) = (EQCSUR(CURCALYR,RECCL,B,R) + &
             ( ((EQCADD(Y,RECCL,B,R)+EQCREP(Y,RECCL,B,R))* &
                 (HSR*ESR)) ))
              ENDDO
            ENDIF

!*******************************************************************
! CALCULATE REPLACEMENT REFRIGERATORS FOR NEW VINTAGE IN CURCALYR-1
! NOTE: REPLACES WITH LIKE IF NOT SINGLE FAMILY HOMES
! NOTE: FOR NEW HOUSES (NH) - CURCALYR-1 IS THE LAGGED VALUE
!*******************************************************************
               EQCREP(CURCALYR,RECCL,B,R)=SA-EQCSUR(CURCALYR,RECCL,B,R)


 15     CONTINUE
!
         DO B=1,MNUMBLDG
           DO R=1,MNUMCR-2
             DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
                EQC=RTCLEQCL(RECCL)
               DO Y=CURCALYR,ENDYR       ! VINTAGE EQUIPMENT FOR AVERAGE STOCK ACCOUNTING
                TEMP=Y-CURCALYR
                HSR=HDR(B)**(TEMP)
                ESR=SVRTE(RTALPHA(RECCL),TEMP,RTK(RECCL), &
                          RTLAMBDA(RECCL))
                EQR90FUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
            EQR90RPFUT(CURCALYR,Y,RECCL,B,R)=(EQCRP90RP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQREPFUT(CURCALYR,Y,RECCL,B,R)=(EQCREP(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
                EQADDFUT(CURCALYR,Y,RECCL,B,R)=(EQCADD(CURCALYR,RECCL,B,R) &
                 *ESR*HSR)
               ENDDO
             ENDDO
           ENDDO
         ENDDO
!*******************************************************************
!     AGGREGATE REFRIGERATORS FOR INVESTMENT ANALYSIS
!*******************************************************************
      Y=CURCALYR
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

            DO B=1,MNUMBLDG
              DO r=1,MNUMCR-2
        TYPE=RTTYPECT(EU)
        DO RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
           IF(CURCALYR.GE.RTINITYR(RECTY).AND. &
             CURCALYR.LE.RTLASTYR(RECTY)) THEN
             IF (RTCENDIV(RECTY).EQ.R) THEN
!
            TYPE=TYPE+1                  ! INDEX FOR 'TYPE' VARIABLES
            EQT=RTEQTYPE(RECTY)
            EQC=RTTYEQCL(RECTY)
            RECCL=RTCLEUPT(EU)+EQC
            HEATINGTYPEPURCH(Y,TYPE,B,R,1)=(NEQTSHR(Y,TYPE,B,r)*EQCADD(Y,RECCL,B,r))
            HEATINGTYPEPURCH(Y,TYPE,B,R,2)=(NEQTSHR(Y,TYPE,B,r)*(EQCREP(Y,RECCL,B,r) + EQCRP90RP(Y,RECCL,B,r)) + &
                                            REQTSHR(Y,TYPE,B,r)*EQCRP90(Y,RECCL,B,r)  )
           endif
           endif
             ENDDO
           ENDDO
        ENDDO

       END SUBROUTINE RFRZADD
!*******************************************************************
!  FREEZER CONSUMPTION
!*******************************************************************
      SUBROUTINE RFRZCON
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,TEMP
      INTEGER B, E, EUPR, D, EU,RECCL,EQC,F,Y,R
!*******************************************************************
!   SET EU = 7 TO SEARCH THE FOOD FREEZING SECTION OF THE DATA
!
      EU=9
      EUPR=7
      alpha=0.0;ef1=.5;ef2=.35;ef3=.15
!
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
      DO R=1,MNUMCR-2
        PRICES(4,R,CURCALYR)=PELRSOUT(R,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO
!
!*******************************************************************
! COMPUTE MOVING AVERAGE STOCK EFFICIENCY FOR 2005 EQP
!*******************************************************************
      IF (CURCALYR.GE.RECSYEAR+1) THEN
        DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RTBASEFF(CURCALYR,RECCL)= STKEFF(CURCALYR,RECCL)
        END DO
      END IF
!
!*******************************************************************
!  Calculate New and Average UECS
!*******************************************************************
      DO 10 D=1,MNUMCR-2
        DO 10 B=1,MNUMBLDG
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
         EQCSUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
         ( RTBASEFF(CURCALYR,RECCL) / RTBASEFF(RECSYEAR,RECCL) )
            EQCNUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
            ( WTEQCEFFN(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL) )
            EQCRUEC(CURCALYR,RECCL,B,D)=EQCUEC(D,RECCL,B)* &
            (  WTEQCEFFR(CURCALYR,RECCL,B,D)/RTBASEFF(RECSYEAR,RECCL) )
          IF (CURCALYR .EQ. RECSYEAR+1) THEN
            EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
         ELSE
         TEMP=0.0
           DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
          IF(TEMP.LE.0.0) THEN
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCNUEC(CURCALYR,RECCL,B,D)
          ELSE
          EQCAUEC(CURCALYR,RECCL,B,D)=0.0
        DO Y=RECSYEAR,CURCALYR-1
             EQCAUEC(CURCALYR,RECCL,B,D)=EQCAUEC(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*EQCRUEC(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*EQCNUEC(Y,RECCL,B,D))) &
                                           /TEMP
        ENDDO
           END IF
          END IF
 10   CONTINUE
!*******************************************************************
!  CALCULATE AVERAGE EFFICIENCY
!*******************************************************************
      DO 30 B=1,MNUMBLDG
        DO 30 D=1,MNUMCR-2
          DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
!
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              WTEQCEFFA(RECSYEAR+1,RECCL,B,D)=WTEQCEFFN(RECSYEAR+1,RECCL,B,D)
!
            ELSE
             TEMP=0.0
            DO Y=RECSYEAR,CURCALYR-1
          TEMP=TEMP+EQR90FUT(Y,CURCALYR,RECCL,B,D)+ &
         EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D)
           ENDDO
              IF (TEMP .GT. 0.0) THEN
                WTEQCEFFA(CURCALYR,RECCL,B,D)=0.0
       DO Y=RECSYEAR,CURCALYR-1
      WTEQCEFFA(CURCALYR,RECCL,B,D)=WTEQCEFFA(CURCALYR,RECCL,B,D)+( &
       (EQR90FUT(Y,CURCALYR,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))+ &
      ((EQADDFUT(Y,CURCALYR,RECCL,B,D)+EQREPFUT(Y,CURCALYR,RECCL,B,D)+ &
         EQR90RPFUT(Y,CURCALYR,RECCL,B,D))*WTEQCEFFN(Y,RECCL,B,D))) &
                                           /TEMP
       ENDDO
              ELSE
                WTEQCEFFA(CURCALYR,RECCL,B,D)= &
                  WTEQCEFFN(CURCALYR,RECCL,B,D)
              END IF
            END IF
          ENDDO
30    CONTINUE
!*******************************************************************
!  Calculate Freezer Consumption
!*******************************************************************
      DO 40 D=1,MNUMCR-2
        FRZCON(CURIYR,D)=0.0
       Do 40 B=1,MNUMBLDG
         FRZCONIN(CURIYR,D,B)=0.
         driver2(CURIYR,d,B)=0.
         FRZCONWT(CURIYR,D,B)=0.
 40   CONTINUE
!
      DO 50 D=1,MNUMCR-2
        DO 50 B=1,MNUMBLDG
          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            F  =RTFUEL(RECCL)
            IF (CURCALYR.EQ.RECSYEAR+1) THEN
              FRZCON(CURIYR,D)=FRZCON(CURIYR,D)+LEAPYR* ( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
             FRZCONWT(CURIYR,D,B)=FRZCONWT(CURIYR,D,B)+LEAPYR* ( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
               *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D).GT.0.) THEN
             FRZCONIN(CURIYR,D,B)=FRZCONIN(CURIYR,D,B)+( (&
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))))  )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D) )
       ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D))+ &
             (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
               (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
               (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
            ELSE
!
              FRZCON(CURIYR,D)=FRZCON(CURIYR,D)+ LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
              FRZCONWT(CURIYR,D,B)=FRZCONWT(CURIYR,D,B)+LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))) &
              *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
       IF(EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D).GT.0.) THEN
              FRZCONIN(CURIYR,D,B)=FRZCONIN(CURIYR,D,B)+ ( (&
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D))))   )

              driver2(CURIYR,d,B)=driver2(CURIYR,d,B)+            &
                (EQCESE(CURCALYR,RECCL,B,D)+EQCADD(CURCALYR,RECCL,B,D)+     &
                 EQCRP90RP(CURCALYR,RECCL,B,D)+EQCRP90(CURCALYR,RECCL,B,D)+ &
                 eqcsr90(CURCALYR,reccl,b,D)+eqcrep(CURCALYR,reccl,b,D)+    &
                 eqcsur(CURCALYR,reccl,b,D))
        ENDIF
!
              EQCEQCN(CURIYR,RECCL,B,D)= LEAPYR*( &
              ((EQCESE(CURCALYR,RECCL,B,D)*EQCSUEC(CURCALYR,RECCL,B,D)) + &
              (EQCADD(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCRP90(CURCALYR,RECCL,B,D)*EQCRUEC(CURCALYR,RECCL,B,D))+ &
            (EQCRP90RP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D))+ &
              (EQCSR90(CURCALYR,RECCL,B,D)*EQCAUEC(CURCALYR,RECCL,B,D)) &
             +(EQCREP(CURCALYR,RECCL,B,D)*EQCNUEC(CURCALYR,RECCL,B,D)) + &
              (EQCSUR(CURCALYR,RECCL,B,D)* &
               EQCAUEC(CURCALYR,RECCL,B,D)) ) &
             *RSELAST(F,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))
!
            ENDIF
!
 50   CONTINUE

      DO R=1,MNUMCR-2
        DO B=1,MNUMBLDG
        If (driver2(CURIYR,R,B).gt.0)   &
           FRZCONIN(CURIYR,R,B)=                &
           FRZCONIN(CURIYR,R,B)/driver2(CURIYR,R,B)
        Enddo
      Enddo

      END SUBROUTINE RFRZCON

!********************************************************************
!     LIGHTING CHOICE, STOCK AND CONSUMPTION
!********************************************************************
     SUBROUTINE LTCNS
     IMPLICIT NONE

     ! Set parameters for array dimensions
!     Integer MaxApps, MaxTypes, MaxBins, NLRec
!     PARAMETER (NLRec=100)  !Number of lighting records in the technology database
!     Parameter (MaxApps=4)  !Maximum number of applications
!     PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
!     Parameter (MaxBins=6)  !Maximum number of hours per day usage bins per applications

     Real*4  LTMSHR(RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins),opcost(MaxTypes), &
             WTLEFF(MaxApps,RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,MaxBins), &
             TOTEWTN(MNUMBLDG,MNUMCR-2,MaxBins), &
             EQWTN(MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTREPTOT(MaxApps,RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,MaxBins), &
!             LTSTOCK(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &  ! MOVED TO COMMON BLOCK FOR DATABASE WRITING
             LTREP(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTTOTSTOCK(MaxApps,RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTSTOCKEX(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTNEEDED(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTNEEDEDFUTly(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTREPFUTly(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTrepconsly(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTrepstkly(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTREPFUT(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTNEEDEDFUT(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTREPstk(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTREPcons(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins), &
             LTstockexcons(MaxApps,RECSYEAR:ENDYR,MaxTypes,MNUMBLDG,MNUMCR-2,MaxBins)

     REAL*4 temp, rep, cumrep, maxrep, annrep, &
              ef1,ef2,ef3,FACTOR,LTLBeta1DR,ALPHA,alpha1, &
            LHRATE,ELIGL,LHRDADJ,LGTBeta1,LGTBeta2,LGTBETA1DR

     INTEGER Y,B,D,EUPR,Y1,R,E,T,BIN,y2,diagnostics,lightdiag,i,indx,app,ilife,ilastyr

      ! reverse these statements to switch diagnostics on or off
     lightdiag=1 !print diagnostics
     lightdiag=0 !don't print diagnostics

     !-----------------------------------------------------
     !  Lighting Consumption Initialization, All Iterations
     !-----------------------------------------------------
         y=curcalyr
       Do d=1,MNUMCR-2
         LTCON(Y-(baseyr-1),D)=0.0
!             LTCONly(Y,D)=0.0
         DO B=1,MNUMBLDG
          LTCONwt(Y-(baseyr-1),D,B)=0.0
          LTCONin(Y-(baseyr-1),D,B)=0.0
!          LTCONwtly(Y-(baseyr-1),D,B)=0.0
!          LTCONinly(Y-(baseyr-1),D,B)=0.0
         Enddo
       Enddo

     !-----------------------------------------------------
     !  Lighting Initializations & Consumption Calculation
     !  First year of call to this routine is RECSYEAR+1
     !-----------------------------------------------------
     IF (CURCALYR.EQ.RECSYEAR+1.AND.CURITR.EQ.1) then
       y=recsyear
       Do d=1,MNUMCR-2
         LTCON(Y-(baseyr-1),D)=0.0
         DO B=1,MNUMBLDG
          LTCONwt(Y-(baseyr-1),D,B)=0.0
          LTCONin(Y-(baseyr-1),D,B)=0.0
         Enddo
       Enddo

     Do 999 app=1,NumApps

      Do y=recsyear,lastyr+baseyr-1
       Do d=1,MNUMCR-2
         Do B=1,MNUMBLDG
          LTNUEC(app,y,d,b)=0.0
         Enddo
       Enddo
      Enddo

      !--------------------------------------------------
      ! Filter Lighting Database for Current Year Values
      !--------------------------------------------------
      e=1 !initialize number of bulb types and add to it below
      do i=1,NLRec
       if(lightingapp(i).ne.AppID(app)) cycle
       ! Application found, check years
         if(curcalyr-1.ge.firstyear(i).and.curcalyr-1.le.lastyear(i)) then
           watts(e)=bulbwatts(i)
           do bin=1,NumAppBins(app)
            bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
           enddo
           e=e+1
         endif
      enddo !i
      if (e-1 .ne. numtypes(app)) &
         write(9,*) 'RESDMSG SUB_LTCNS: Lamps not lining up for application', app

      Do bin=1,NumAppBins(app)
       LTBinShare(app,bin)=0.
      enddo

      !Compute equipment stocks, consumption per HH by application (LTUEC), and bin shares of energy use (LTBinShare)
      DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
        LTEQP(app,RECSYEAR,B,d)=bulbsperhh(app,b)*EH(recsyear,B,d)
        LTuec(app,d,b)=0.
        Do bin=1,NumAppBins(app)
         do e=1,numtypes(app)
          LTUEC(app,d,B)=ltuec(app,d,b)+appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
                   *basewattsbulbs(app,e)*3.412/10**6
          LTBinShare(app,bin)=LTBinShare(app,bin)  +appbinhours(app,bin)*365.*binshares(app,bin)*bulbbinshares(app,e,bin) &
                   *basewattsbulbs(app,e)*3.412/10**6
         enddo !e
        enddo !bin
       END DO
      END DO

      !LTBinShare contains total energy, now convert LTBinShare into shares of energy use
      temp=0.
      Do bin=1,NumAppBins(app)
       temp=temp+LTBinShare(app,bin)
      enddo
      Do bin=1,NumAppBins(app)
       LTBinShare(app,bin)=LTBinShare(app,bin)/temp
      enddo

      !---------------------------------
      !  Calculate Lighting Consumption
      !---------------------------------
      DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
         ! Added next line to feed RECS year consumption into output database file
         lteqcn(recsyear-(baseyr-1),app,b,d)=LTEQP(app,RECSYEAR,B,d)*LTUEC(app,d,B)
         LTCON(recsyear-(baseyr-1),d)=LTCON(recsyear-(baseyr-1),d)+(LTEQP(app,RECSYEAR,B,d)*LTUEC(app,d,B))
       END DO
      END DO

      DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
        DO BIN=1,NumAppBins(APP)
          LTTOTSTOCK(app,RECSYEAR,B,d,BIN)=0.0
         DO E=1,numtypes(APP)
          LTSTOCK  (app,RECSYEAR,E,B,d,BIN)=(LTEQP(app,RECSYEAR,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
          LTSTOCKEX(app,RECSYEAR,E,B,d,BIN)=(LTEQP(app,RECSYEAR,B,d)*binshares(app,bin)*bulbbinshares(app,e,BIN))
          LTstockexcons(app,recsyear,E,B,d,BIN)=LTstockex(app,recsyear,E,B,d,BIN) &
            *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
          LTNEEDED(app,RECSYEAR,E,B,d,BIN)=0.0
          LTrepfut(app,RECSYEAR,E,B,d,BIN)=0.0
          LTREP(app,RECSYEAR,E,B,d,BIN)=0.0
          LTTOTSTOCK(app,RECSYEAR,B,d,BIN)=LTTOTSTOCK(app,RECSYEAR,B,d,BIN)+LTSTOCK(app,RECSYEAR,E,B,d,BIN)
         END DO
        END DO
       END DO
      END DO

      !      This is the remaining RECSyear stock by bin projected into the future.
      !              Will become zero at some point for all bulb types depending on
      !              bin hours and bulb lives (see calculation of bulbbinlife(e, bin))
      DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
        DO BIN=1,NumAppBins(app)
         DO Y1=recsyear+1,lastyr+baseyr-1
          DO E=1,numtypes(app)

           LTStockEx(app,Y1,E,B,d,BIN)= &
              max(0.,LTStockEx(app,Y1-1,E,B,d,BIN)-LTStockEx(app,recsyear,E,B,d,BIN)*HDR(B)**(y1-recsyear)/bulbbinlife(E,BIN))
           LTstockexcons(app,Y1,E,B,d,BIN)= LTStockEx(app,Y1,E,B,d,BIN) &
                                              *appbinhours(app,bin)*365.*basewattsbulbs(app,e)*3.412/10**6
          END DO !e
         END DO !y1
        END DO  !bin
       END DO   !b
      END DO    !r

999   Continue  !Do for each lighting application
      Endif !RECSYEAR processing on first iteration

      !*****************************
      !   PROCESS FORECAST YEARS
      !*****************************

      !   Map Electricity Price into "Menu Dollar Year"
      !   Prices in constant dollars, $/MMBtu
      EUPR=8   !End use lighting
      DO d=1,MNUMCR-2
        PRICES(4,d,CURCALYR)=PELRSOUT(d,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO

     !Price elasticities and distribution and behavioral elasticity adjustment factor(alpah1)
     ALPHA=-.15;ef1=.50;ef2=.35;ef3=.15;ALPHA1=-0.01
     ! CONVERSION FACTOR FROM WattHours TO MMBTU
     FACTOR=3.412/10**6   !Btu/Watt to millions of Btu

     ! Loop through all applications
     Do 1000 app=1,NumApps
     !-----------------------------------------------------
     !     Filter Lighting Database for Current Year Values
     !-----------------------------------------------------
      e=1
      do i=1,NLRec
       if(lightingapp(i).ne.AppID(app)) cycle
       ! Application found in data, now check years
         if(curcalyr.ge.firstyear(i).and.curcalyr.le.lastyear(i)) then
           watts(e)=bulbwatts(i)
           do bin=1,NumAppBins(app)
            bulbbinlife(e,bin)=lifehours(i)/(365.*appbinhours(app,bin))
           enddo
             IF (curcalyr.le.2019) THEN                     !lgtsubhist - keep lighting subsidies available regardless of CPP in order to accommodate historical energy efficiency rebates
               do d =1,mnumcr-2                             !lgtsubhist
                 LTLCAP(e,d,1)=bulbcost(i)-bulbsub(i,d) !put bulb cost in bin 1 for now    !lgtsubhist
                   ltlsub(e,d)=bulbsub(i,d)                 !lgtsubhist
                   ltlcapInvest(e)=bulbcost(i)              !lgtsubhist
               enddo                                        !lgtsubhist
             ELSE                                           !lgtsubhist
               do d =1,mnumcr-2
             LTLCAP(e,d,1)=bulbcost(i)-bulbsub(i,d)*float(EPA111D) !put bulb cost in bin 1 for now
                   ltlsub(e,d)=bulbsub(i,d)*float(EPA111D)
                   ltlcapInvest(e)=bulbcost(i)
               enddo
			 ENDIF                                          !lgtsubhist
           cribulb(e)=bulbcri(i)
           appbulbname(app,e)=bulbtype(i)
           e=e+1
         endif
      enddo !i
      if (e-1 .ne. numtypes(app)) &
         WRITE(6,*)  'RESDMSG SUB_LTCNS:', AppID(app),' lamps not lining up'
      if (e-1 .ne. numtypes(app) .and. lightdiag.eq.1) &
         write(9,*) 'RESDMSG SUB_LTCNS:', AppID(app),' lamps not lining up'

     !Further processing of LTLCAP - adjust for CRI and multiple replacements per year
       do d=1,mnumcr-2
     do e=1,NUMTYPES(APP)
      temp=LTLCAP(e,d,1) !bulb cost doesn't vary by bin until bin hours are accounted for below
      do bin=1,NumAppBins(app)
       LTLCAP(e,d,bin)= temp/(cribulb(e)/100.)**2
       ! if bulb lasts less than a year in this bin, then increase capital costs based on number of
       !   replacements per year
       if(bulbbinlife(e,bin) .lt. 1.) LTLCAP(e,d,bin)=(temp/bulbbinlife(e,bin))/(cribulb(e)/100.)**2
      enddo
     enddo !e
       enddo !division

     !  Operating Cost and Logit Shares
     DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
         !      CHANGE BETA1 TO REFLECT PRICE INDUCED BEHAVIOR CHANGES
         !         Note: to temporarily disable discount rate adjustment -- uncomment the following line
         !               and comment the one below
         !IF ((CURCALYR.GT.2040).AND. &
         IF ((CURCALYR.GT.2008).AND. &
               (PRICES(4,d,CURCALYR).GT.PRICES(4,d,RECSYEAR))) THEN
         LHRATE=LTLBeta1/LTLBeta2
         ELIGL=LHRATE - 0.07
           IF (ELIGL.GT.0.0) THEN
            LHRDADJ= ELIGL * &
            ((PRICES(4,d,CURCALYR)/PRICES(4,d,RECSYEAR))**ALPHA1 )
            LTLBeta1DR = (LHRDADJ+0.07) * LTLBeta2
            ELSE
             LTLBeta1DR=LTLBeta1
           END IF
          ELSE
             LTLBeta1DR=LTLBeta1
         END IF

             !  lighting diagnostics
             diagnostics=0
               if(d==1 .and. b==1 .and. curcalyr==2006) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2010) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2011) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2012) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2013) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2019) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2020) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2021) diagnostics=1
               if(diagnostics==1 .and. lightdiag==1) &
               write(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LTLBeta2, opcost(e), prices, watts, &
                 factor, LTHOURS(e), logit calc(e) '
             diagnostics=0

         DO BIN=1,NumAppBins(app)
            TOTEWTN(B,d,BIN)=0.0
           DO E=1,numtypes(app)

             OPCOST(E)=PRICES(4,d,CURCALYR)*WATTS(E)*FACTOR*appbinHOURS(app,BIN)*365.
             EQWTN(E,B,d,BIN)=exp(LTLBeta1dr*LTLCAP(e,d,bin)+LTLBeta2*opcost(e))
             TOTEWTN(B,d,BIN)=TOTEWTN(B,d,BIN)+EQWTN(E,B,d,BIN)

             !  lighting diagnostics
             diagnostics=0
               if(d==1 .and. b==1 .and. curcalyr==2006) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2010) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2011) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2012) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2013) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2019) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2020) diagnostics=1
               if(d==1 .and. b==1 .and. curcalyr==2021) diagnostics=1
               if(diagnostics==1 .and. lightdiag==1) &
                  write(9,443) curcalyr, app, bin, e, LTLBeta1dr, LTLCAP(e,d,bin), bulbbinlife(e,bin),  &
                   LTLBeta2, opcost(e),PRICES(4,d,CURCALYR),watts(e),factor,appbinhours(app,bin),eqwtn(e,b,d,bin)
             diagnostics=0
             End Do  !e
           End Do    !bin
         End Do      !b
       End Do        !d

       DO d=1,MNUMCR-2
         DO B=1,MNUMBLDG
           DO E=1,NumTypes(app)
             DO BIN=1,6
               IF(TOTEWTN(B,d,BIN).NE.0.0) THEN
                  LTMSHR(CURCALYR,E,B,d,BIN)=(EQWTN(E,B,d,BIN)/ &
                    TOTEWTN(B,d,BIN))
               ELSE
                  LTMSHR(CURCALYR,E,B,d,BIN)=0.0
               ENDIF
               !LTMSHR(CURCALYR,E,B,d,BIN)= bulbbinshares(app,e,bin)
             End Do
           End Do
         End Do
       End Do

            !Diagnostics
            IF (CURCALYR.gt.recsyear .or. CURCALYR.le.2025) THEN
             DO BIN=1,NumAppBins(app)
              if(lightdiag==1)WRITE (18,442) 'SHR',curcalyr,app,bin,(LTMSHR(CURCALYR,E,1,1,BIN),E=1,numtypes(app))
             END DO
            END IF

       ! Initialize future replacements for iteration control
       DO BIN=1,NumAppBins(app)
        DO d=1,MNUMCR-2
         DO B=1,MNUMBLDG
          DO E=1,NumTypes(app)
          if(curitr.eq.1) then
           DO Y1=CURCALYR,lastyr+baseyr-1
            LTNEEDEDFUTly(app,y1,E,B,d,BIN)=LTNEEDEDFUT(app,y1,E,B,d,BIN)
            LTREPFUTly(app,y1,E,B,d,BIN)=LTREPFUT(app,y1,E,B,d,BIN)
            LTrepconsly(app,y1,e,B,d,BIN)=LTrepcons(app,y1,e,B,d,BIN)
            LTrepstkly(app,y1,e,B,d,BIN)=LTrepstk(app,y1,e,B,d,BIN)
!            LTCONly(Y1,d)=LTCON(Y1-(baseyr-1),d)
!            LTCONINly(Y1-(baseyr-1),d,b)=LTCONIN(Y1-(baseyr-1),d,b)
!            LTCONWTly(Y1-(baseyr-1),d,b)=LTCONWT(Y1-(baseyr-1),d,b)
            LTNUECly(app,Y1,d,b)=LTNUEC(app,Y1,d,b)
           Enddo
          else
           DO Y1=CURCALYR,lastyr+baseyr-1
            LTNEEDEDFUT(app,y1,E,B,d,BIN)=LTNEEDEDFUTly(app,y1,E,B,d,BIN)
            LTREPFUT(app,y1,E,B,d,BIN)=LTREPFUTly(app,y1,E,B,d,BIN)
            LTrepcons(app,y1,e,B,d,BIN)=LTrepconsly(app,y1,e,B,d,BIN)
            LTrepstk(app,y1,e,B,d,BIN)=LTrepstkly(app,y1,e,B,d,BIN)
!            LTCON(Y1-(baseyr-1),d)=LTCONly(Y1,d)
!            LTCONIN(Y1-(baseyr-1),d,b)=LTCONINly(Y1-(baseyr-1),d,b)
!            LTCONWT(Y1-(baseyr-1),d,b)=LTCONWTly(Y1-(baseyr-1),d,b)
            LTNUEC(app,Y1,d,b)=LTNUECly(app,Y1,d,b)
           Enddo
          endif
          END DO
         END DO
        END DO
       END DO

       ! Additional stock bulbs needed for this year's new construction and for newly added floorspace this year
       !  in existing homes that remain from the original RECSyear stock of homes.
       !  This is allocated by purchases and represents current year requirements only.
       DO BIN=1,NumAppBins(app)
        DO d=1,MNUMCR-2
         DO B=1,MNUMBLDG
          DO E=1,NumTypes(app)

            LTNEEDED(app,CURCALYR,E,B,d,BIN)=(HSEADD(CURCALYR,B,d)*(SQRFOOT(CURCALYR,B,d)/SQRFOOT(RECSYEAR,B,d)) &
                               *bulbsperhh(app,b)*LTMSHR(CURCALYR,E,B,d,BIN)*binshares(app,BIN) &
                               +EH(CURCALYR,B,d)*((EXSQRFOOT(CURCALYR,B,d)/EXSQRFOOT(CURCALYR-1,B,d))-1.0) &
                               *bulbsperhh(app,b)*LTMSHR(CURCALYR,E,B,d,BIN)*binshares(app,BIN))
          END DO
         END DO
        END DO
       END DO

       ! Bulbs needed this year for all reasons
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
           LTREPTOT(app,CURCALYR,B,d,BIN)=0.
          Do e=1,NumTypes(app)
           ! This year's purchases =
           !   replacements for original RECS stock + replacements from past purchases needed this year
           !   + new bulbs added due to new construction & floorspace additions in existing homes
           ! NOTE that LTrepfut(app,curcalyr) is finalized in curcalyr-1, we update the future stream for this
           !   years purchases below

           LTREPTOT(app,CURCALYR,B,d,BIN)=LTREPTOT(app,CURCALYR,B,d,BIN) &
                                           +LTSTOCKEX(app,curcalyr-1,E,B,d,BIN)*hdr(b)-LTSTOCKEX(app,CURCALYR,E,B,d,BIN) &
                                           +LTREPFUT(app,curcalyr,E,B,d,BIN) &
                                           +LTneeded(app,curcalyr,e,b,d,bin)

          Enddo
         END DO
        END DO
       END DO


       ! Distribute purchases to bulb types based on purchase shares (LTMSHR), accumulate stocks and consumption
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
          Do e=1,NumTypes(app)
           LTREP(app,CURCALYR,e,B,d,BIN)=LTREPTOT(app,CURCALYR,B,d,BIN)*LTMSHR(CURCALYR,E,B,d,BIN)
           LTREPstk(app,curcalyr,E,B,d,BIN)=LTREPstk(app,curcalyr,E,B,d,BIN)+ LTREP(app,CURCALYR,e,B,d,BIN)
           LTrepcons(app,CURCALYR,e,B,d,BIN)=LTrepcons(app,CURCALYR,e,B,d,BIN) &
                       +LTrep(app,CURCALYR,e,B,d,BIN)*365.*appbinhours(app,bin)*watts(e)*3.412/10**6
          Enddo
         END DO
        END DO
       END DO


       ! Extend this year's bulb purchases (LTREP) into the future replacement purchase requirements
       ! Also extend this years purchased bulbs into future purchased-bulb remaining stocks
       ! And compute the energy consumed for this years purchases and the energy requirements
       !  for future remaining stocks from this years purchases
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
          DO E=1,NumTypes(app)
           cumrep=0.  !cumulative replacements
           rep=LTREP(app,curcalyr,E,B,d,BIN)/max(1.,bulbbinlife(E,BIN)) !bulbs per year decaying from LTREP

           ! restricting the looping will save some amount of execution time
           ilife=ifix(bulbbinlife(E,BIN)+.5)+1 !round up bulblife as an integer and add a year for looping
           ilastyr=min(lastyr+baseyr-1,curcalyr+ilife)
           DO Y1=CURCALYR+1,ilastyr

            maxrep=LTREP(app,curcalyr,E,B,d,BIN)
            maxrep=maxrep*hdr(b)
            annrep=max(0.,min(maxrep-cumrep,rep))

            if (annrep .gt. 0.) then
              LTREPFUT(app,Y1,E,B,d,BIN)=LTREPFUT(app,Y1,E,B,d,BIN) + annrep*HDR(B)**(y1-curcalyr)
              cumrep=cumrep+annrep
              LTREPstk(app,y1,E,B,d,BIN)=LTREPstk(app,y1,E,B,d,BIN)+ maxrep-cumrep
              LTrepcons(app,y1,E,B,d,BIN)=LTrepcons(app,y1,E,B,d,BIN)+(maxrep-cumrep)*365.*appbinhours(app,bin) &
                  *watts(e)*3.412/10**6
            endif
           END DO
          END DO
         END DO
        END DO
       END DO

       ! LTrepstk is prior replacements remaining stock, so total stock = RECS stock remaining
       !     + this year's purchases + prior year replacements still left in stock
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
          DO E=1,NumTypes(app)

           LTSTOCK(app,CURCALYR,E,B,d,BIN)=LTSTOCKEX(app,curcalyr,E,B,d,BIN)+LTrepstk(app,curcalyr,E,B,d,BIN)

          END DO  !e
         END DO   !bin
        END DO    !bldg
       END DO     !reg

       DO d=1,MNUMCR-2
         DO B=1,MNUMBLDG
            LTEQP(app,CURCALYR,B,d)=0.0
          DO BIN =1,NumAppBins(app)
            LTTOTSTOCK(app,CURCALYR,B,d,BIN)=0.0
           DO E=1,NumTypes(app)
            LTTOTSTOCK(app,CURCALYR,B,d,BIN)=LTTOTSTOCK(app,CURCALYR,B,d,BIN)+LTSTOCK(app,CURCALYR,E,B,d,BIN)
            LTEQP(app,CURCALYR,B,d)=LTEQP(app,CURCALYR,B,d)+LTSTOCK(app,CURCALYR,E,B,d,BIN)
           END DO
         END DO
        END DO
       END DO

       DO d=1,MNUMCR-2
         DO B=1,MNUMBLDG
          DO E=1,NumTypes(app)
            LTinvest(app,CURCALYR,E,B,d,1)=0.0
            LTinvest(app,CURCALYR,E,B,d,2)=0.0
            LTsubsidy(app,CURCALYR,E,B,d,1)=0.0
            LTsubsidy(app,CURCALYR,E,B,d,2)=0.0
            LTNEEDEDbyAPP(app,CURCALYR,E,B,d)=0.0
            LTREPbyAPP(app,CURCALYR,E,B,d)=0.0
           DO BIN=1,NumAppBins(app)
            LTNEEDEDbyAPP(app,CURCALYR,E,B,d)=LTNEEDEDbyAPP(app,CURCALYR,E,B,d)+LTNEEDED(app,CURCALYR,E,B,d,BIN)
            LTREPbyAPP(app,CURCALYR,E,B,d)=LTREPbyAPP(app,CURCALYR,E,B,d)+LTREP(app,CURCALYR,E,B,d,BIN)
            !adjust investment spending for bulbs lasting < 1 year by dividing by life in years
                  temp=LTLCAPInvest(E)-LTLSUB(e,d)
            if(bulbbinlife(e,bin) .lt. 1.) temp=temp/bulbbinlife(e,bin)
            LTinvest(app,CURCALYR,E,B,d,1)=LTinvest(app,CURCALYR,E,B,d,1)+LTNEEDED(app,CURCALYR,E,B,d,BIN)*temp
            LTinvest(app,CURCALYR,E,B,d,2)=LTinvest(app,CURCALYR,E,B,d,2)+LTREP(app,CURCALYR,E,B,d,BIN)*temp
                  temp=LTLSUB(E,d)           !111(D) -- not likely to subsidize short lived bulbs, but just in case
                  if(bulbbinlife(e,bin) .lt. 1.) temp=temp/bulbbinlife(e,bin)
            LTsubsidy(app,CURCALYR,E,B,d,1)=LTsubsidy(app,CURCALYR,E,B,d,1)+LTNEEDED(app,CURCALYR,E,B,d,BIN)*temp
            LTsubsidy(app,CURCALYR,E,B,d,2)=LTsubsidy(app,CURCALYR,E,B,d,2)+LTREP(app,CURCALYR,E,B,d,BIN)*temp
           END DO
              END DO
         END DO
       END DO

       !-----------------------------------
       !     CALCULATE WEIGHTED EFFICIENCY
       !-----------------------------------
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
            WTLEFF(app,CURCALYR,B,d,BIN)=0.0
            DO E=1,NumTypes(app)
              ! first calculate total watts for this years purchases and surviving purchases from past years
              !  then calculate watts per bulb in the next loop
               WTLEFF(app,CURCALYR,B,d,BIN)=WTLEFF(app,CURCALYR,B,d,BIN) &
                   +LTrepcons(app,curcalyr,E,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)&
                   +LTstockexcons(app,CURCALYR,e,B,d,BIN)/(365.*appbinhours(app,bin)*3.412/10**6)
            END DO
          END DO
         END DO
        END DO

       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
         DO BIN=1,NumAppBins(app)
          ! now divide by the total stock to get average watts per bulb by bin
          WTLEFF(app,CURCALYR,B,d,BIN)=WTLEFF(app,CURCALYR,B,d,BIN)/LTTOTSTOCK(app,CURCALYR,B,d,BIN)
         END DO
        END DO
       END DO

       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
          WTLEFFbyAPP(app,CURCALYR,B,d)=0.0
         DO BIN=1,NumAppBins(app)
          WTLEFFbyAPP(app,CURCALYR,B,d)=WTLEFFbyAPP(app,CURCALYR,B,d)+((WTLEFF(app,CURCALYR,B,d,BIN)*LTTOTSTOCK(app,CURCALYR,B,d,BIN)) &
                                                                   /LTEQP(app,CURCALYR,B,d))
         END DO
        END DO
       END DO
      !-----------------
      !   LIGHTING UEC
      !-----------------
       DO d=1,MNUMCR-2
        DO B=1,MNUMBLDG
          DO BIN=1,NumAppBins(app)
           ! This calculation adjusts UECs for efficiency and rebound effect, price elasticity in next loop
           LTNUEC(app,CURCALYR,d,b)= LTNUEC(app,CURCALYR,d,b) &
              + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CURCALYR,b,d,BIN)/basewattbins(app,bin))**(1+alpha)

! this might be preferred        + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CURCALYR,b,d,BIN)/WTLEFF(app,recsyear+1,b,d,BIN))**(1+alpha)

! 0613 switch from basewatts     + LTBinShare(app,BIN)*LTUEC(app,d,b)*(WTLEFF(app,CURCALYR,b,d,BIN)/basewattbins(app,bin))**(1+alpha)
!      this caused an unexpected jump in 2006 consumption -- NEED TO INVESTIGATE FURTHER	

          Enddo !Bins
        Enddo !Building Types
       Enddo !Regions

      !-------------------------------
      ! LIGHTING UEC AND CONSUMPTION
      !-------------------------------
      y=curiyr
      ! Economic Stimulus Package Affects Price Elasticity (but not rebound)
      ! This is a permanent shift based on the smart grid concept
      DO d=1,MNUMCR-2
       DO B=1,MNUMBLDG
       IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1)) THEN
         ALPHA=-0.30
        ELSE
         ALPHA=-0.15
       END IF

       lteqcn(y,app,b,d)=leapyr*lteqp(app,CURCALYR,b,d)*ltnuec(app,CURCALYR,d,b)*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

       ! OUTPUTS TO NEMS TABLE 4 (ltcon)
       ltcon(y,d)= ltcon(y,d) + lteqcn(y,app,b,d)
       ltconwt(y,d,b)= ltconwt(y,d,b)+lteqcn(y,app,b,d)

       ! OUTPUTS TO NEMS TABLE 31  (ltconwt, ltconin)
       ltconin(y,d,b)= ltconin(y,d,b)+(ltconwt(y,d,b)/RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR))/&
          (EH(curcalyr,B,D)*EXSQRFOOT(CURCALYR,B,D)+HSEADD(curcalyr,B,D)*SQRFOOT(curcalyr,B,D)+ &
           SQNEW(curcalyr-1,B,D)*NH(curcalyr-1,B,D))

       Enddo !Building Types
      Enddo !Regions

             !  Lighting diagnostics
             if(lightdiag==0) goto 446
             ! write(9,*) 'year, app, bin, e, LTLBeta1dr, LTLCAP, bulbbinlife(e,bin), LTLBeta2, opcost(e) prices, watts, &
             !              factor, LTHOURS(e), logit calc(e) '

             if (curcalyr == 2040) then
               b=1
               d=1

               write(9,*) ' Printing Lighting Diagnostics for Division ',d,' Building Type ',b
               write(9,*) 'EH EXSQRFOOT'
               write(9,445)   bin, e,(EH(y1,1,1)/10.**6,Y1=2005,2040)
               write(9,445)   bin, e,(EXSQRFOOT(y1,1,1)/10.**3,Y1=2005,2040)

               write(9,*) 'HSEADD SQRFOOT'
               write(9,445)   bin, e,(HSEADD(y1,1,1)/10.**6,Y1=2005,2040)
               write(9,445)   bin, e,(SQRFOOT(y1,1,1)/10.**3,Y1=2005,2040)

                 write(9,*) ' '
                 write(9,*) 'LTrep Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445)   bin, e,(LTrep(app,y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTRepTot Bins '
               DO BIN=1,NumAppBins(app)
                   ! Divide by 10^6 for most detail
                   write(9,445) bin, e,(LTreptot(app,Y1,B,d,BIN)/10.**6,Y1=2005,2040)
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTRepFut Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445) bin, e,(LTrepFUT(app,Y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTStockEx Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445) bin, e,(LTSTOCKEX(app,Y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTStockExCons Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445) bin, e,(LTstockexcons(app,Y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

               write(9,*) ' '
               write(9,*) 'LTrepstk Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445) bin, e,(LTrepstk(app,Y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTrepCons Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445)   bin, e,(LTrepCons(app,y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

                 write(9,*) ' '
                 write(9,*) 'LTneeded Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                   ! Divide by 10^6 for most detail
                   write(9,445)  bin, e,(LTneeded(app,y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

               write(9,*) ' '
               write(9,*) 'LTStock Bins&Type '
               DO BIN=1,NumAppBins(app)
                DO E=1,NumTypes(app)
                 write(9,445) bin,e,(LTSTOCK(app,Y1,E,B,d,BIN)/10.**6,Y1=2005,2040)
                END DO !e
               END DO !bin

               write(9,*) ' '
               write(9,*) 'WTLEFF Bins '
               e=0
               DO BIN=1,NumAppBins(app)
                 write(9,445) bin,e,(WTLEFF(app,y1,B,d,BIN),Y1=2005,2040)
               END DO !bin

               write(9,*) ' '
               write(9,*) 'ltcon/10**9 '
               write(9,444) (ltcon(y1,1)/10**9,Y1=16,51)


               write(9,*) 'ltnuec '
               write(9,444) (LTNUEC(app,y1,1,1),Y1=2005,2040)

               write(9,*) 'ltnuec*lteqp/10**9'
               write(9,444) (LTNUEC(app,y1,1,1)*LTEQP(app,y1,1,1)/10**9,Y1=2005,2040)
               !End Lighting Diagnostics

       endif  !current year is 2040
  446  continue  !jump here to skip diagnostic output

 1000  continue  !looping for lighting applications

442    format(a4,i5,2i2,4e11.3)
443    format(i5,3i2,10e11.3)
444    format(36(1x,f7.4))
445    format(2i3,36(1x,f7.3))

      END SUBROUTINE LTCNS
!********************************************************************
!     PERSONAL COMPUTER CONSUMPTION
!********************************************************************
!      SUBROUTINE PCCNS
!      IMPLICIT NONE
!      REAL*4 ALPHA,ef1,ef2,ef3,PCUNIT(RECSYEAR:ENDYR)
!      REAL*4 PCBASEFF,EFF(3)
!      REAL*4 PCMSHR(RECSYEAR:ENDYR,3),PCNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 PCNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),PCEQPI(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
!      INTEGER Y,B,D,Y1,EUPR
!      Y=CURIYR
!      ALPHA=-0.15
!      EUPR=9 !other appliances


!*******************************************************************
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
!*******************************************************************
!      DO D=1,MNUMCR-2
!        PRICES(4,D,CURCALYR)=PELRSOUT(D,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
!      ENDDO
!
!      DO D=1,MNUMCR-2
!        INCOME(D,CURCALYR)=(MC_YPDR(D,CURIYR)/MC_YPDR(D,RECSYEAR-BASEYR+1))**.05
!      ENDDO
!********************************************************************
! PC UEC
!********************************************************************
!      DO 15 D=1,MNUMCR-2
!             DO 15 B=1,MNUMBLDG
!          PCNUEC(CURCALYR,D,B)=  PCUEC(D,B) * INCOME(D,CURCALYR) * WTPCEFF(CURCALYR)
!          PCNIUEC(CURCALYR,D,B)=  PCUEC(D,B) * WTPCEFF(CURCALYR)
! 15   CONTINUE
!
!********************************************************************
! Number of PCs
!********************************************************************
!      DO 20 D=1,MNUMCR-2
!             DO 20 B=1,MNUMBLDG
!         PCEQP(CURCALYR,B,D)= ((PCEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*PCPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
!!         PCEQP(CURCALYR,B,D)=((EH(CURCALYR,B,D)+NH(CURCALYR,B,D))&
!!               *PCPEN(CURCALYR)*PCSAT(B,D))
!         PCEQPI(CURCALYR,B,D)=PCEQP(CURCALYR,B,D)
! 20   CONTINUE
!          PCUNIT(CURCALYR)=0.0
!      DO 25 D=1,MNUMCR-2
!       DO 25 B=1,MNUMBLDG
!          PCUNIT(CURCALYR)=PCUNIT(CURCALYR)+PCEQP(CURCALYR,B,D)
! 25   CONTINUE
!********************************************************************
! PC UEC AND CONSUMPTION
!********************************************************************
!      DO 50 D=1,MNUMCR-2
!        PCCON(Y,D)=0.0
!      DO 50 B=1,MNUMBLDG
!           IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1)) THEN
!             ALPHA=-0.30
!           ELSE
!             ALPHA=-0.15
!           END IF
!         PCCON(y,d)=PCCON(y,d)+LEAPYR*(PCEQP(CURCALYR,B,D)*PCNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         PCCONWT(y,d,b)=LEAPYR*(PCEQP(CURCALYR,B,D)*PCNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         PCCONIN(y,d,b)=(PCEQP(CURCALYR,B,D)*PCNUEC(CURCALYR,d,b)) &
!              / PCEQP(CURCALYR,B,D)
!         pceqcn(y,1,b,d)=LEAPYR*(PCEQP(CURCALYR,b,d)*pcnuec(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!   50   CONTINUE
!      END SUBROUTINE PCCNS
!
!********************************************************************
!     COLOR TELEVISION CONSUMPTION
!********************************************************************
!      SUBROUTINE TVCNS
!      IMPLICIT NONE
!      REAL*4 ALPHA,ef1,ef2,ef3
!      REAL*4 TVNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 STBNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 VGNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 TVNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 STBNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      REAL*4 VGNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
!      INTEGER Y, B, D,Y1,EUPR
!*******************************************************************
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY
!*******************************************************************
!      EUPR=9
!
!      DO D=1,MNUMCR-2
!        PRICES(4,D,CURCALYR)=PELRSOUT(D,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
!      ENDDO
!
!      Y=CURIYR
!      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
!********************************************************************
! TV UEC
!********************************************************************
!      DO 15 D=1,MNUMCR-2
!             DO 15 B=1,MNUMBLDG
!          TVNUEC(CURCALYR,D,B)=   TVUEC(D,B)*TVEFF(CURCALYR)
!          STBNUEC(CURCALYR,D,B)=  STBUEC(D,B)*STBEFF(CURCALYR)
!          VGNUEC(CURCALYR,D,B)=   VGUEC(D,B)*VGEFF(CURCALYR)
!          TVNIUEC(CURCALYR,D,B)=  TVUEC(D,B)*TVEFF(CURCALYR)
!          STBNIUEC(CURCALYR,D,B)= STBUEC(D,B)*STBEFF(CURCALYR)
!          VGNIUEC(CURCALYR,D,B)=  VGUEC(D,B)*VGEFF(CURCALYR)
! 15   CONTINUE
!********************************************************************
! Number of TVs
!********************************************************************
!      DO 20 D=1,MNUMCR-2
!       DO 20 B=1,MNUMBLDG
!          TVEQP(CURCALYR,B,D)=  (TVEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*TVPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
!          STBEQP(CURCALYR,B,D)= (STBEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*STBPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
!          VGEQP(CURCALYR,B,D)=  (VGEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*VGPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))

! Old method used PEN parameters that were redundant
          !TVEQP(CURCALYR,B,D)=  (TVPEN(CURCALYR)*CTVSAT(B,D)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          !STBEQP(CURCALYR,B,D)= (STBPEN(CURCALYR)*STBSAT(B,D)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          !VGEQP(CURCALYR,B,D)=  (VGPEN(CURCALYR)*VGSAT(B,D)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))


! 20   CONTINUE
!********************************************************************
! TV UEC AND CONSUMPTION
!********************************************************************
!      DO 50 D=1,MNUMCR-2
!        TVCON(Y,D)=0.0
!      DO 50 B=1,MNUMBLDG
!           IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1)) THEN
!             ALPHA=-0.30
!           ELSE
!             ALPHA=-0.15
!           END IF
!         TVCON(y,d)=TVCON(y,d)+LEAPYR*(TVEQP(CURCALYR,B,D)*TVNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)+ &
!             STBCON(y,d)+LEAPYR*(STBEQP(CURCALYR,B,D)*STBNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)+ &
!             VGCON(y,d)+LEAPYR*(VGEQP(CURCALYR,B,D)*VGNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         TVCONWT(y,d,b)=LEAPYR*(TVEQP(CURCALYR,B,D)*TVNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)+ &
!             LEAPYR*(STBEQP(CURCALYR,B,D)*STBNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR) + &
!             LEAPYR*(VGEQP(CURCALYR,B,D)*VGNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         TVCONIN(y,d,b)=(TVEQP(CURCALYR,B,D)*TVNIUEC(CURCALYR,d,b))/TVEQP(CURCALYR,B,D)+ &
!             (STBEQP(CURCALYR,B,D)*STBNIUEC(CURCALYR,d,b))/STBEQP(CURCALYR,B,D)+ &
!             (VGEQP(CURCALYR,B,D)*VGNIUEC(CURCALYR,d,b))/VGEQP(CURCALYR,B,D)
!         TVEQCN(y,1,b,d)=LEAPYR*(TVEQP(CURCALYR,b,d)*TVNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         STBEQCN(y,1,b,d)=LEAPYR*(STBEQP(CURCALYR,b,d)*STBNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         VGEQCN(y,1,b,d)=LEAPYR*(VGEQP(CURCALYR,b,d)*VGNUEC(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!   50   CONTINUE
!      END SUBROUTINE TVCNS
!********************************************************************
!     OTHER ELECTRIC APPLIANCE CONSUMPTION
!********************************************************************
      SUBROUTINE APCNS
      IMPLICIT NONE
      REAL*4 TVSNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),TVSNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 STBNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),STBNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 HTSNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),HTSNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DVDNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),DVDNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 VGCNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),VGCNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DPCNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),DPCNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 LPCNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),LPCNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 MONNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),MONNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 NETNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),NETNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 BATNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),BATNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 CFNNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),CFNNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 COFNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),COFNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DEHNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),DEHNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 MCONIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),MCONUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 PHPNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),PHPNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 SECNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),SECNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 SPANIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),SPANUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 WCLNIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),WCLNUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)    !winecool
      REAL*4 EANIUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),EANUEC(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG)
      REAL*4 TVSCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),TVSCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 STBCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),STBCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 HTSCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),HTSCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DVDCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),DVDCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 VGCCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),VGCCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DPCCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),DPCCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 LPCCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),LPCCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 MONCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),MONCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 NETCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),NETCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 BATCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),BATCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 CFNCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),CFNCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 COFCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),COFCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 DEHCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),DEHCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 MCOCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),MCOCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 PHPCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),PHPCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 SECCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),SECCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 SPACONWT(MNUMYR,MNUMCR-2,MNUMBLDG),SPACONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 WCLCONWT(MNUMYR,MNUMCR-2,MNUMBLDG),WCLCONIN(MNUMYR,MNUMCR-2,MNUMBLDG)    !winecool
      REAL*4 EACONWT(MNUMYR,MNUMCR-2,MNUMBLDG),EACONIN(MNUMYR,MNUMCR-2,MNUMBLDG)
      REAL*4 ALPHA,ef1,ef2,ef3,ELOTPEN(RECSYEAR:ENDYR,MNUMCR-2),sqftadj
      INTEGER Y,B,D,EUPR,F,Y1,Y3
      Y=CURCALYR-(baseyr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=9

!********************************************************************
!   MAP ELECTRICITY PRICE ARRAY INTO RESIDENTIAL PRICE ARRAY, CALCULATE INCOME EFFECT
!********************************************************************
      DO D=1,MNUMCR-2
        PRICES(4,D,CURCALYR)=PELRSOUT(D,CURIYR,EUPR)*(MC_JPGDP(RTEKDOLLARYR-BASEYR+1)/MC_JPGDP(-2))
      ENDDO

      DO D=1,MNUMCR-2
        INCOME(D,CURCALYR)=(MC_YPDR(D,CURIYR)/MC_YPDR(D,RECSYEAR-BASEYR+1))**.05
      ENDDO


!********************************************************************
! SET PENETRATION RATES FOR OTHER ELECTRIC/ UNSPECIFIED MISCELLANEOUS ELECTRIC LOADS (MELs)
!*******************************************************************
   !penetration of electric other changes at the rate of disposable income per person over age 16
      DO D=1,MNUMCR-2
        ELOTPEN(RECSYEAR,D)=1.
        ELOTPEN(CURCALYR,D)=ELOTPEN(RECSYEAR,D)*((MC_YPDR(D,Y)/MC_NP16A(D,Y))/(MC_YPDR(D,RECSYEAR-(baseyr-1))/MC_NP16A(D,RECSYEAR-(baseyr-1))))
      END DO

!********************************************************************
! SET UECs
!********************************************************************
      DO 10 D=1,MNUMCR-2
             DO 10 B=1,MNUMBLDG
              !note added adjustment to electric other for average square footage
          ! sqftadj= ( EH(CURCALYR,B,D)*EXSQRFOOT(CURCALYR,B,d)+NH(CURCALYR,B,D)*SQRFOOT(CURCALYR,b,d) ) / &
          !    ( EH(RECSYEAR,B,D)*EXSQRFOOT(RECSYEAR,B,d)+NH(RECSYEAR,B,D)*SQRFOOT(RECSYEAR,b,d) )
          sqftadj=1.
          TVSNUEC(CURCALYR,D,B)=  TVSUEC(D,B)*TVSEFF(CURCALYR)*INCOME(D,CURCALYR)
          TVSNIUEC(CURCALYR,D,B)= TVSUEC(D,B)*TVSEFF(CURCALYR)
          STBNUEC(CURCALYR,D,B)=  STBUEC(D,B)*STBEFF(CURCALYR)*INCOME(D,CURCALYR)
          STBNIUEC(CURCALYR,D,B)= STBUEC(D,B)*STBEFF(CURCALYR)
          HTSNUEC(CURCALYR,D,B)=  HTSUEC(D,B)*HTSEFF(CURCALYR)*INCOME(D,CURCALYR)
          HTSNIUEC(CURCALYR,D,B)= HTSUEC(D,B)*HTSEFF(CURCALYR)
          DVDNUEC(CURCALYR,D,B)=  DVDUEC(D,B)*DVDEFF(CURCALYR)
          DVDNIUEC(CURCALYR,D,B)= DVDUEC(D,B)*DVDEFF(CURCALYR)
          VGCNUEC(CURCALYR,D,B)=  VGCUEC(D,B)*VGCEFF(CURCALYR)*INCOME(D,CURCALYR)
          VGCNIUEC(CURCALYR,D,B)= VGCUEC(D,B)*VGCEFF(CURCALYR)
          DPCNUEC(CURCALYR,D,B)=  DPCUEC(D,B)*DPCEFF(CURCALYR)*INCOME(D,CURCALYR)
          DPCNIUEC(CURCALYR,D,B)= DPCUEC(D,B)*DPCEFF(CURCALYR)
          LPCNUEC(CURCALYR,D,B)=  LPCUEC(D,B)*LPCEFF(CURCALYR)*INCOME(D,CURCALYR)
          LPCNIUEC(CURCALYR,D,B)= LPCUEC(D,B)*LPCEFF(CURCALYR)
          MONNUEC(CURCALYR,D,B)=  MONUEC(D,B)*MONEFF(CURCALYR)*INCOME(D,CURCALYR)
          MONNIUEC(CURCALYR,D,B)= MONUEC(D,B)*MONEFF(CURCALYR)
          NETNUEC(CURCALYR,D,B)=  NETUEC(D,B)*NETEFF(CURCALYR)*INCOME(D,CURCALYR)
          NETNIUEC(CURCALYR,D,B)= NETUEC(D,B)*NETEFF(CURCALYR)
          BATNUEC(CURCALYR,D,B)=  BATUEC(D,B)*BATEFF(CURCALYR)*INCOME(D,CURCALYR)
          BATNIUEC(CURCALYR,D,B)= BATUEC(D,B)*BATEFF(CURCALYR)
          CFNNUEC(CURCALYR,D,B)=  CFNUEC(D,B)*CFNEFF(CURCALYR)*(CDDADJ(CURCALYR,D)/CDDADJ(RECSYEAR,D))**(2.0)
          CFNNIUEC(CURCALYR,D,B)= CFNUEC(D,B)*CFNEFF(CURCALYR)
          COFNUEC(CURCALYR,D,B)=  COFUEC(D,B)*COFEFF(CURCALYR)
          COFNIUEC(CURCALYR,D,B)= COFUEC(D,B)*COFEFF(CURCALYR)
          DEHNUEC(CURCALYR,D,B)=  DEHUEC(D,B)*DEHEFF(CURCALYR)*INCOME(D,CURCALYR)
          DEHNIUEC(CURCALYR,D,B)= DEHUEC(D,B)*DEHEFF(CURCALYR)
          MCONUEC(CURCALYR,D,B)=  MCOUEC(D,B)*MCOEFF(CURCALYR)
          MCONIUEC(CURCALYR,D,B)= MCOUEC(D,B)*MCOEFF(CURCALYR)
          PHPNUEC(CURCALYR,D,B)=  PHPUEC(D,B)*PHPEFF(CURCALYR)*INCOME(D,CURCALYR)
          PHPNIUEC(CURCALYR,D,B)= PHPUEC(D,B)*PHPEFF(CURCALYR)
          SECNUEC(CURCALYR,D,B)=  SECUEC(D,B)*SECEFF(CURCALYR)*INCOME(D,CURCALYR)
          SECNIUEC(CURCALYR,D,B)= SECUEC(D,B)*SECEFF(CURCALYR)
          SPANUEC(CURCALYR,D,B)=  SPAUEC(D,B)*SPAEFF(CURCALYR)*INCOME(D,CURCALYR)
          SPANIUEC(CURCALYR,D,B)= SPAUEC(D,B)*SPAEFF(CURCALYR)
          WCLNUEC(CURCALYR,D,B)=  WCLUEC(D,B)*WCLEFF(CURCALYR)*INCOME(D,CURCALYR)    !winecool
          WCLNIUEC(CURCALYR,D,B)= WCLUEC(D,B)*WCLEFF(CURCALYR)                       !winecool

          EANUEC(CURCALYR,D,B)=  EAUEC(D,B)* ELOTPEN(CURCALYR,D)* sqftadj
          EANIUEC(CURCALYR,D,B)= EAUEC(D,B)* ELOTPEN(CURCALYR,D)* sqftadj
 10   CONTINUE
!********************************************************************
! Number of MELs
!********************************************************************
      DO 15 D=1,MNUMCR-2
             DO 15 B=1,MNUMBLDG
          TVSEQP(CURCALYR,B,D)=((TVSEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*TVSPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          STBEQP(CURCALYR,B,D)=((STBEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*STBPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          HTSEQP(CURCALYR,B,D)=((HTSEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*HTSPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          DVDEQP(CURCALYR,B,D)=((DVDEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*DVDPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          VGCEQP(CURCALYR,B,D)=((VGCEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*VGCPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          DPCEQP(CURCALYR,B,D)=((DPCEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*DPCPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          LPCEQP(CURCALYR,B,D)=((LPCEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*LPCPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          MONEQP(CURCALYR,B,D)=((MONEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*MONPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          NETEQP(CURCALYR,B,D)=((NETEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*NETPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          BATEQP(CURCALYR,B,D)=((BATEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*BATPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          CFNEQP(CURCALYR,B,D)=((CFNEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*CFNPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          COFEQP(CURCALYR,B,D)=((COFEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*COFPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          DEHEQP(CURCALYR,B,D)=((DEHEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*DEHPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          MCOEQP(CURCALYR,B,D)=((MCOEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*MCOPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          PHPEQP(CURCALYR,B,D)=((PHPEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*PHPPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          SECEQP(CURCALYR,B,D)=((SECEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*SECPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          SPAEQP(CURCALYR,B,D)=((SPAEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*SPAPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))
          WCLEQP(CURCALYR,B,D)=((WCLEQP(RECSYEAR,B,D)/EH(RECSYEAR,B,D))*WCLPEN(CURCALYR)*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D)))    !winecool
!          EAEQP(CURCALYR,B,D)= EH(CURCALYR,B,D)+NH(CURCALYR,B,D)
          EAEQP(CURCALYR,B,D)= EAEQP(RECSYEAR,B,D)+NH(CURCALYR,B,D)

 15   CONTINUE
!********************************************************************
! CALCULATE CONSUMPTION FOR ELECT, MOTORS, AND HEATING ELEMENTS FROM
!********************************************************************
      DO 20 D=1,MNUMCR-2
       DO 20 B=1,MNUMBLDG
           IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1)) THEN
             ALPHA=-0.30
           ELSE
             ALPHA=-0.15
           END IF
         !TELEVISIONS (TVS)
         TVSCONWT(Y,D,B)=LEAPYR*(TVSEQP(CURCALYR,B,D)*TVSNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         TVSCONIN(Y,D,B)= 0.
         IF(TVSEQP(CURCALYR,B,D).GT.0.)TVSCONIN(Y,D,B)=(TVSEQP(CURCALYR,B,D)*TVSNIUEC(CURCALYR,D,B)) &
                                                        / TVSEQP(CURCALYR,B,D)
         TVSEQCN(Y,1,B,D)=LEAPYR*(TVSEQP(CURCALYR,B,D) &
                          *TVSNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !SET-TOP BOXES (STB)
         STBCONWT(Y,D,B)=LEAPYR*(STBEQP(CURCALYR,B,D)*STBNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         STBCONIN(Y,D,B)= 0.
         IF(STBEQP(CURCALYR,B,D).GT.0.)STBCONIN(Y,D,B)=(STBEQP(CURCALYR,B,D)*STBNIUEC(CURCALYR,D,B)) &
                                                        / STBEQP(CURCALYR,B,D)
         STBEQCN(Y,1,B,D)=LEAPYR*(STBEQP(CURCALYR,B,D) &
                          *STBNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !HOME THEATER SYSTEMS (HTS)
         HTSCONWT(Y,D,B)=LEAPYR*(HTSEQP(CURCALYR,B,D)*HTSNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         HTSCONIN(Y,D,B)= 0.
         IF(HTSEQP(CURCALYR,B,D).GT.0.)HTSCONIN(Y,D,B)=(HTSEQP(CURCALYR,B,D)*HTSNIUEC(CURCALYR,D,B)) &
                                                        / HTSEQP(CURCALYR,B,D)
         HTSEQCN(Y,1,B,D)=LEAPYR*(HTSEQP(CURCALYR,B,D) &
                          *HTSNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !DVD PLAYERS (DVD)
         DVDCONWT(Y,D,B)=LEAPYR*(DVDEQP(CURCALYR,B,D)*DVDNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         DVDCONIN(Y,D,B)= 0.
         IF(DVDEQP(CURCALYR,B,D).GT.0.)DVDCONIN(Y,D,B)=(DVDEQP(CURCALYR,B,D)*DVDNIUEC(CURCALYR,D,B)) &
                                                        / DVDEQP(CURCALYR,B,D)
         DVDEQCN(Y,1,B,D)=LEAPYR*(DVDEQP(CURCALYR,B,D) &
                          *DVDNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !VIDEO GAME CONSOLES (VGC)
         VGCCONWT(Y,D,B)=LEAPYR*(VGCEQP(CURCALYR,B,D)*VGCNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         VGCCONIN(Y,D,B)= 0.
         IF(VGCEQP(CURCALYR,B,D).GT.0.)VGCCONIN(Y,D,B)=(VGCEQP(CURCALYR,B,D)*VGCNIUEC(CURCALYR,D,B)) &
                                                        / VGCEQP(CURCALYR,B,D)
         VGCEQCN(Y,1,B,D)=LEAPYR*(VGCEQP(CURCALYR,B,D) &
                          *VGCNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !DESKTOP PCS (DPC)
         DPCCONWT(Y,D,B)=LEAPYR*(DPCEQP(CURCALYR,B,D)*DPCNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         DPCCONIN(Y,D,B)= 0.
         IF(DPCEQP(CURCALYR,B,D).GT.0.)DPCCONIN(Y,D,B)=(DPCEQP(CURCALYR,B,D)*DPCNIUEC(CURCALYR,D,B)) &
                                                        / DPCEQP(CURCALYR,B,D)
         DPCEQCN(Y,1,B,D)=LEAPYR*(DPCEQP(CURCALYR,B,D) &
                          *DPCNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !LAPTOP PCS (LPC)
         LPCCONWT(Y,D,B)=LEAPYR*(LPCEQP(CURCALYR,B,D)*LPCNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         LPCCONIN(Y,D,B)= 0.
         IF(LPCEQP(CURCALYR,B,D).GT.0.)LPCCONIN(Y,D,B)=(LPCEQP(CURCALYR,B,D)*LPCNIUEC(CURCALYR,D,B)) &
                                                        / LPCEQP(CURCALYR,B,D)
         LPCEQCN(Y,1,B,D)=LEAPYR*(LPCEQP(CURCALYR,B,D) &
                          *LPCNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !MONITORS (MON)
         MONCONWT(Y,D,B)=LEAPYR*(MONEQP(CURCALYR,B,D)*MONNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         MONCONIN(Y,D,B)= 0.
         IF(MONEQP(CURCALYR,B,D).GT.0.)MONCONIN(Y,D,B)=(MONEQP(CURCALYR,B,D)*MONNIUEC(CURCALYR,D,B)) &
                                                        / MONEQP(CURCALYR,B,D)
         MONEQCN(Y,1,B,D)=LEAPYR*(MONEQP(CURCALYR,B,D) &
                          *MONNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !NETWORKING EQUIPMENT (NET)
         NETCONWT(Y,D,B)=LEAPYR*(NETEQP(CURCALYR,B,D)*NETNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         NETCONIN(Y,D,B)= 0.
         IF(NETEQP(CURCALYR,B,D).GT.0.)NETCONIN(Y,D,B)=(NETEQP(CURCALYR,B,D)*NETNIUEC(CURCALYR,D,B)) &
                                                        / NETEQP(CURCALYR,B,D)
         NETEQCN(Y,1,B,D)=LEAPYR*(NETEQP(CURCALYR,B,D) &
                          *NETNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !RECHARGEABLES (BAT)
         BATCONWT(Y,D,B)=LEAPYR*(BATEQP(CURCALYR,B,D)*BATNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         BATCONIN(Y,D,B)= 0.
         IF(BATEQP(CURCALYR,B,D).GT.0.)BATCONIN(Y,D,B)=(BATEQP(CURCALYR,B,D)*BATNIUEC(CURCALYR,D,B)) &
                                                        / BATEQP(CURCALYR,B,D)
         BATEQCN(Y,1,B,D)=LEAPYR*(BATEQP(CURCALYR,B,D) &
                          *BATNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !CEILING FANS (CFN)
         CFNCONWT(Y,D,B)=LEAPYR*(CFNEQP(CURCALYR,B,D)*CFNNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         CFNCONIN(Y,D,B)= 0.
         IF(CFNEQP(CURCALYR,B,D).GT.0.)CFNCONIN(Y,D,B)=(CFNEQP(CURCALYR,B,D)*CFNNIUEC(CURCALYR,D,B)) &
                                                        / CFNEQP(CURCALYR,B,D)
         CFNEQCN(Y,1,B,D)=LEAPYR*(CFNEQP(CURCALYR,B,D) &
                          *CFNNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !COFFEE MACHINES (COF)
         COFCONWT(Y,D,B)=LEAPYR*(COFEQP(CURCALYR,B,D)*COFNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         COFCONIN(Y,D,B)= 0.
         IF(COFEQP(CURCALYR,B,D).GT.0.)COFCONIN(Y,D,B)=(COFEQP(CURCALYR,B,D)*COFNIUEC(CURCALYR,D,B)) &
                                                        / COFEQP(CURCALYR,B,D)
         COFEQCN(Y,1,B,D)=LEAPYR*(COFEQP(CURCALYR,B,D) &
                          *COFNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !DEHUMIDIFIERS (DEH)
         DEHCONWT(Y,D,B)=LEAPYR*(DEHEQP(CURCALYR,B,D)*DEHNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         DEHCONIN(Y,D,B)= 0.
         IF(DEHEQP(CURCALYR,B,D).GT.0.)DEHCONIN(Y,D,B)=(DEHEQP(CURCALYR,B,D)*DEHNIUEC(CURCALYR,D,B)) &
                                                        / DEHEQP(CURCALYR,B,D)
         DEHEQCN(Y,1,B,D)=LEAPYR*(DEHEQP(CURCALYR,B,D) &
                          *DEHNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !MICROWAVE OVENS (MCO)
         MCOCONWT(Y,D,B)=LEAPYR*(MCOEQP(CURCALYR,B,D)*MCONUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         MCOCONIN(Y,D,B)= 0.
         IF(MCOEQP(CURCALYR,B,D).GT.0.)MCOCONIN(Y,D,B)=(MCOEQP(CURCALYR,B,D)*MCONIUEC(CURCALYR,D,B)) &
                                                        / MCOEQP(CURCALYR,B,D)
         MCOEQCN(Y,1,B,D)=LEAPYR*(MCOEQP(CURCALYR,B,D) &
                          *MCONUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !POOL HEATERS AND PUMPS (PHP)
         PHPCONWT(Y,D,B)=LEAPYR*(PHPEQP(CURCALYR,B,D)*PHPNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         PHPCONIN(Y,D,B)= 0.
         IF(PHPEQP(CURCALYR,B,D).GT.0.)PHPCONIN(Y,D,B)=(PHPEQP(CURCALYR,B,D)*PHPNIUEC(CURCALYR,D,B)) &
                                                        / PHPEQP(CURCALYR,B,D)
         PHPEQCN(Y,1,B,D)=LEAPYR*(PHPEQP(CURCALYR,B,D) &
                          *PHPNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !SECURITY SYSTEMS (SEC)
         SECCONWT(Y,D,B)=LEAPYR*(SECEQP(CURCALYR,B,D)*SECNUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         SECCONIN(Y,D,B)= 0.
         IF(SECEQP(CURCALYR,B,D).GT.0.)SECCONIN(Y,D,B)=(SECEQP(CURCALYR,B,D)*SECNIUEC(CURCALYR,D,B)) &
                                                        / SECEQP(CURCALYR,B,D)
         SECEQCN(Y,1,B,D)=LEAPYR*(SECEQP(CURCALYR,B,D) &
                          *SECNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !SPAS (SPA)
         SPACONWT(Y,D,B)=LEAPYR*(SPAEQP(CURCALYR,B,D)*SPANUEC(CURCALYR,D,B)) &
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         SPACONIN(Y,D,B)= 0.
         IF(SPAEQP(CURCALYR,B,D).GT.0.)SPACONIN(Y,D,B)=(SPAEQP(CURCALYR,B,D)*SPANIUEC(CURCALYR,D,B)) &
                                                        / SPAEQP(CURCALYR,B,D)
         SPAEQCN(Y,1,B,D)=LEAPYR*(SPAEQP(CURCALYR,B,D) &
                          *SPANUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         !WINE COOLERS (WCL)                                                                              !winecool
         WCLCONWT(Y,D,B)=LEAPYR*(WCLEQP(CURCALYR,B,D)*WCLNUEC(CURCALYR,D,B)) &                            !winecool
                         *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)                                    !winecool
         WCLCONIN(Y,D,B)= 0.                                                                              !winecool
         IF(WCLEQP(CURCALYR,B,D).GT.0.)WCLCONIN(Y,D,B)=(WCLEQP(CURCALYR,B,D)*WCLNIUEC(CURCALYR,D,B)) &    !winecool
                                                        / WCLEQP(CURCALYR,B,D)                            !winecool
         WCLEQCN(Y,1,B,D)=LEAPYR*(WCLEQP(CURCALYR,B,D) &                                                  !winecool
                          *WCLNUEC(CURCALYR,D,B))*RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)            !winecool

!         DHCONWT(y,d,b)=LEAPYR*(DHEQP(CURCALYR,B,D)*DHNUEC(CURCALYR,d,b)) &	!if this is old dehumidifier code, remove
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
!         If(dheqp(curcalyr,b,d).le.0.) then
!           DHCONIN(y,d,b)=0.
!          else
!           DHCONIN(y,d,b)=(DHEQP(CURCALYR,B,D)*DHNIUEC(CURCALYR,d,b)) &
!                 / DHEQP(CURCALYR,B,D)
!         Endif
!         DHEQCN(y,1,b,d)=LEAPYR*(DHEQP(CURCALYR,b,d) &
!                           *DHnuec(CURCALYR,d,b)) &
!             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

         EACONWT(y,d,b)=LEAPYR*(EAEQP(CURCALYR,B,D)*EANUEC(CURCALYR,d,b)) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
         EACONIN(y,d,b)=0.
         If(eaeqp(CURCALYR,B,D).gt.0.)EACONIN(y,d,b)=(EAEQP(CURCALYR,B,D)*EANIUEC(CURCALYR,d,b)) &
                  / EAEQP(CURCALYR,B,D)
         EAEQCN(y,1,b,d)=LEAPYR*(EAEQP(CURCALYR,b,d)*EAnuec(CURCALYR,d,b)) &
             *RSELAST(4,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

   20   CONTINUE
!
!********************************************************************
!  Calculate NEMS consumption for TV-Related, PC-Related, and Appliances.
!********************************************************************
      DO 50 D=1,MNUMCR-2
          TVRCON(Y,D)=0.0
          PCRCON(Y,D)=0.0
          APCON(Y,D)=0.0
          APCONwithDG(Y,D)=0.0
      DO 50 B=1,MNUMBLDG
        TVRCON(Y,D)=TVRCON(Y,D)+TVSEQCN(Y,1,B,D)+STBEQCN(Y,1,B,D)+HTSEQCN(Y,1,B,D)+DVDEQCN(Y,1,B,D)+VGCEQCN(Y,1,B,D)
        TVRCONWT(Y,D,b)=TVSCONWT(Y,D,B)+STBCONWT(Y,D,B)+HTSCONWT(Y,D,B)+DVDCONWT(Y,D,B)+VGCCONWT(Y,D,B)
        PCRCON(Y,D)=PCRCON(Y,D)+ DPCEQCN(Y,1,B,D)+LPCEQCN(Y,1,B,D)+MONEQCN(Y,1,B,D)+NETEQCN(Y,1,B,D)
        PCRCONWT(Y,D,b)=DPCCONWT(Y,D,B)+LPCCONWT(Y,D,B)+MONCONWT(Y,D,B)+NETCONWT(Y,D,B)
        APCON(Y,D)=APCON(Y,D)+ BATEQCN(Y,1,B,D)+CFNEQCN(Y,1,B,D)+COFEQCN(Y,1,B,D)+DEHEQCN(Y,1,B,D)+&
                   MCOEQCN(Y,1,B,D)+PHPEQCN(Y,1,B,D)+SECEQCN(Y,1,B,D)+SPAEQCN(Y,1,B,D)+&
                   WCLEQCN(Y,1,B,D)+EAEQCN(Y,1,B,D)                                         !winecool
        APCONWT(Y,D,b)=BATCONWT(Y,D,B)+CFNCONWT(Y,D,B)+COFCONWT(Y,D,B)+DEHCONWT(Y,D,B)+&
                       MCOCONWT(Y,D,B)+PHPCONWT(Y,D,B)+SECCONWT(Y,D,B)+SPACONWT(Y,D,B)+&
                       WCLCONWT(Y,D,B)+EACONWT(Y,D,B)                                       !winecool

  50   CONTINUE

!     Compute other electric appliance efficiency based on weighted average equipment intensities
      DO 51 D=1,MNUMCR-2
      DO 51 B=1,MNUMBLDG
        TVRCONIN(Y,D,b)=(TVSCONIN(Y,D,B)*tvsconwt(y,d,b)+STBCONIN(Y,D,B)*stbconwt(y,d,b)+HTSCONIN(Y,D,B)*htsconwt(y,d,b) +&
                         DVDCONIN(Y,D,B)*dvdconwt(y,d,b)+VGCCONIN(Y,D,B)*vgcconwt(y,d,b)) / tvrconwt(y,d,b)
        PCRCONIN(Y,D,b)=(DPCCONIN(Y,D,B)*dpcconwt(y,d,b)+LPCCONIN(Y,D,B)*lpcconwt(y,d,b)+MONCONIN(Y,D,B)*monconwt(y,d,b) +&
                         NETCONIN(Y,D,B)*netconwt(y,d,b)) / pcrconwt(y,d,b)
        APCONIN(Y,D,b)= (BATCONIN(Y,D,B)*batconwt(y,d,b)+CFNCONIN(Y,D,B)*cfnconwt(y,d,b)+COFCONIN(Y,D,B)*cofconwt(y,d,b) +&
                         DEHCONIN(Y,D,B)*dehconwt(y,d,b)+MCOCONIN(Y,D,B)*mcoconwt(y,d,b)+PHPCONIN(Y,D,B)*phpconwt(y,d,b) +&
                         SECCONIN(Y,D,B)*secconwt(y,d,b)+SPACONIN(Y,D,B)*spaconwt(y,d,b)+WCLCONIN(Y,D,B)*wclconwt(y,d,b) +&    !winecool
                         EACONIN(Y,D,B)*eaconwt(y,d,b)) / apconwt(y,d,b)
 51   CONTINUE

      END SUBROUTINE APCNS


!********************************************************************
!     SECONDARY HEATING CONSUMPTION
!********************************************************************
! SEC HEAT= G,E,D,L,K,C,W APPL=G,L

      SUBROUTINE SHTCNS
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3
      REAL*4 HDDFACT(MNUMCR), SHTEFF, NHtoUse
      INTEGER Y, B, D, F, F2, FShell, EUPR
      Y=CURCALYR-(baseyr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=10

!     INITIALIZE
      DO D=1,MNUMCR-2
       DO F=1,7
        SHTCON(Y,F,D)=0.0
        DO B=1,MNUMBLDG
          SHEQCN(Y,F,B,D)=0.0
          SHTCONIN(Y,F,D,B)=0.0
          SHTCONWT(Y,F,D,B)=0.0
        ENDDO
       ENDDO
      ENDDO

!   COMPUTE HDDFACT
      DO D=1,MNUMCR-2
          HDDFACT(D)=(HDDADJ(CURCALYR,D)/HDDADJ(RECSYEAR,D))
      ENDDO

! Begin Looping for Secondary Heating Calculations
      DO F=1,7
  !   Fuels are matched with prices below for the RSELAST function calls
  !   F is the secondary heating numbering scheme
  !     "F" 1=NG, 2=EL, 3=DIS, 4=LPG, 5=Ker, 6=Wood, 7=Coal
  !   F2 is the fuel for the elasticity calculation:
  !     Prices "F2" 1=DIS 2=LPG 3=NG 4=EL 5=Ker 6=WOOD
  !     Since no price for coal, turn off elasticity below by setting F2=0

            ! Add ARRA effect for enhanced price sensitivity for stimulus runs
            IF ((CURCALYR.GT.2010).AND.(STIMULUS.EQ.1).AND.(F.EQ.4)) THEN
               ALPHA=-0.30
             ELSE
               ALPHA=-0.15
            END IF

            FShell=F  !FShell is the heating shell to use, none for wood, so set wood below
            IF (F.EQ.1) F2=3 !gas
            IF (F.EQ.2) F2=4 !elec
            IF (F.EQ.3) F2=1 !dist
            IF (F.EQ.4) F2=2 !lpg
            IF (F.EQ.5) F2=5 !kero
            IF (F.EQ.6) F2=0 !COAL ! setting F2 to zero turns off price elasticity
            IF (F.EQ.7) then !WOOD priced to distillate
             F2=1
             FShell=3 !There is no shell for wood, so use distillate
             ALPHA=0.50 !No ARRA effect for wood, has positive elasticity with respect to distillate
            ENDIF

         DO D=1,MNUMCR-2
            DO B=1,MNUMBLDG

                SHTEQP(CURCALYR,B,D,F)=(SHTSHR(B,D,F)*EH(CURCALYR,B,D)+NSHTSHR(B,D,F)*NH(CURCALYR,B,D))

                ! Special Treatment for Natural Gas and LPG due to secondary heating standard after 2012
                NHtoUse=NH(CURCALYR,B,D) !replace NH(...) with "discounted" value for standard
                IF (F.eq.1 .or. F.eq.4) THEN
                 IF(CURCALYR .gt. 2012) THEN
                  ! Houses added to the stock after 2012 have increased efficiency
                  SHTEFF=.5003 !calculated effect of efficiency on UEC for houses post-2012
                  ! Do weighted calculation for NH, "discounting" post-2012 to account for
                  !  increased efficiency requirements
                  NHtoUse=(NH(CURCALYR,B,D)-NH(2012,B,D))*SHTEFF + NH(2012,B,D)
                 ENDIF !curcalyr > 2012
                ENDIF !F=1 or F=4 and curcalyr>2012

                SHTCON(Y,F,D)=SHTCON(Y,F,D)+LEAPYR*(SHTSHR(B,D,F)* &
                   EH(CURCALYR,B,D)*SHTUEC(D,F,B)* &
                   HDDFACT(D)*(EHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)) + &
                   NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)*&
                   (AHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)))* &
                   RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

                SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)+LEAPYR*(SHTSHR(B,D,F)* &
                   EH(CURCALYR,B,D)*SHTUEC(D,F,B)* &
                   HDDFACT(D)*(EHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)) + &
                   NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)*&
                   (AHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)))* &
                   RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

                IF ((EH(CURCALYR,B,D)+NH(CURCALYR,B,D)).gt.0. .and. ehshell(recsyear,fshell,d,b).gt.0. .and. &
                     SHTSHR(B,D,F).gt.0. .and. ahshell(curcalyr,fshell,d,b).gt.0. .and. NSHTSHR(B,D,F).gt.0. ) then
                  SHTCONIN(Y,F,D,B)=SHTCONIN(Y,F,D,B)+( (SHTSHR(B,D,F)* &
                    EH(CURCALYR,B,D)*SHTUEC(D,F,B)* &
                    (EHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)) + &
                    NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)*&
                    (AHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B))) )&
                     / (EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
                ENDIF

                SHEQCN(Y,F,B,D)=LEAPYR*(SHTSHR(B,D,F)* &
                   EH(CURCALYR,B,D)*SHTUEC(D,F,B)* &
                   HDDFACT(D)*(EHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)) + &
                   NSHTSHR(B,D,F)*NHtoUse*SHTUEC(D,F,B)*HDDFACT(D)*&
                   (AHSHELL(CURCALYR,FShell,D,B)/EHSHELL(RECSYEAR,FShell,D,B)))* &
                   RSELAST(F2,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)

           ENDDO ! B=1,MNUMBLDG
          ENDDO ! D=1,MNUMCR-2
         ENDDO ! F=1,7

      END SUBROUTINE SHTCNS

!********************************************************************
!     APPLIANCE CONSUMPTION
!********************************************************************
      SUBROUTINE APPCNS
      IMPLICIT NONE
      REAL*4 ALPHA,ef1,ef2,ef3,LPGGRILL(RECSYEAR:ENDYR)
      INTEGER Y, B, D,F,F1,Y1,EUPR
      Y=CURCALYR-(baseyr-1)
      ALPHA=-.15;ef1=.5;ef2=.35;ef3=.15
      EUPR=10
!********************************************************************
! APPL= G,L,D
!********************************************************************
      LPGGRILL(RECSYEAR)=0.43
      LPGGRILL(ijumpcalyr)=0.60
      IF (CURCALYR.EQ.(RECSYEAR+1)) THEN
      DO 30 D=1,MNUMCR-2
       DO 30 B=1,MNUMBLDG
        DO 30 F=1,3
           APLEQP(RECSYEAR,B,D,F)=APPEQP(RECSYEAR,B,D,F)
 30 CONTINUE
       END IF
     IF ((CURCALYR.EQ.RECSYEAR+1).AND.(CURITR.EQ.1)) THEN
       DO Y1=RECSYEAR+1,ijumpcalyr
         LPGGRILL(Y1)=LPGGRILL(Y1-1)+((LPGGRILL(ijumpcalyr)-LPGGRILL(RECSYEAR))/(ijumpcalyr-RECSYEAR))
       END DO
     END IF
      DO 40 D=1,MNUMCR-2
       DO 40 B=1,MNUMBLDG
        DO 40 F=1,3
          IF (F.EQ.2) THEN
           APLEQP(CURCALYR,B,D,F)=((APPEQP(RECSYEAR,B,D,F)/EH(RECSYEAR,B,D))*(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))&
                                      *(LPGGRILL(CURCALYR)/LPGGRILL(RECSYEAR)) )
          ELSE
           APLEQP(CURCALYR,B,D,F)=((APPEQP(RECSYEAR,B,D,F)/EH(RECSYEAR,B,D))*EH(CURCALYR,B,D))
          END IF
 40 CONTINUE
!
      DO 50 D=1,MNUMCR-2
        DO 50 F=1,3
           IF (F.EQ.1) F1=3
           IF (F.EQ.2) F1=2
           IF (F.EQ.3) F1=1
          APLCON(Y,F,D)=0.0
          DO 50 B=1,MNUMBLDG
           APLCON(Y,F,D)= APLCON(Y,F,D)+ LEAPYR*APLEQP(CURCALYR,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
           APLCONWT(Y,F,D,B)= LEAPYR*APLEQP(CURCALYR,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
        if (EH(CURCALYR,B,D)+NH(CURCALYR,B,D).gt.0.) then
           APLCONIN(Y,F,D,B)= (APLEQP(CURCALYR,B,D,F)*APPUEC(D,F,B)) &
                   / (EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
        endif
            APEQCN(Y,F,B,D)=LEAPYR*APLEQP(CURCALYR,B,D,F)*APPUEC(D,F,B) &
             *RSELAST(F1,D,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
 50   CONTINUE
      END SUBROUTINE APPCNS
!********************************************************************
!     CALCULATE FUEL CONSUMPTION
!********************************************************************
      SUBROUTINE FUELCN
      IMPLICIT NONE
      INTEGER Y,D,F
      Y=CURCALYR-(baseyr-1)
!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 10 D=1,MNUMCR-2
! GAS
         RSFLCN(Y,1,D)= &
           (HTRCON(Y,1,D)+H2OCON(Y,1,D)+CKCON(Y,1,D)+DRYCON(Y,1,D)+ &
            COOLCN(Y,3,D)+SHTCON(Y,1,D)+APLCON(Y,1,D))/1000000.
! ELECTRIC
         RSFLCN(Y,2,D)= &
         (HTRCON(Y,2,D)+COOLCN(Y,1,D)+H2OCON(Y,2,D)+REFCON(Y,D)+ &
          CKCON(Y,3,D)+DRYCON(Y,2,D)+SHTCON(Y,2,D)+TVRCON(Y,D)+ &
          FRZCON(Y,D)+LTCON(Y,D)+APCON(Y,D)+ &
          PCRCON(Y,D)+FANCON(Y,D)+CSWCON(Y,D)+DSWCON(Y,D))/1000000.
! DISTILLATE
         RSFLCN(Y,3,D)= (APLCON(Y,3,D)+ &
          HTRCON(Y,3,D)+H2OCON(Y,3,D)+SHTCON(Y,3,D))/1000000.
! LPG
         RSFLCN(Y,4,D)= (SHTCON(Y,4,D) + APLCON(Y,2,D) + &
       HTRCON(Y,4,D)+H2OCON(Y,4,D)+CKCON(Y,2,D))/1000000.
! KEROSENE
         RSFLCN(Y,5,D)=(HTRCON(Y,5,D)+SHTCON(Y,5,D))/1000000.
! COAL
         RSFLCN(Y,6,D)=SHTCON(Y,6,D)/1000000.
! WOOD
         RSFLCN(Y,7,D)=(HTRCON(Y,6,D)+SHTCON(Y,7,D))/1000000.
! GEOTHERMAL
         RSFLCN(Y,8,D)=(HTRCON(Y,7,D)+COOLCN(Y,2,D))/1000000.
 10      CONTINUE
!********************************************************************
!     CALCULATE US (DIVISION 10) FUEL CONSUMPTION
!********************************************************************
      DO 20 F=1,8
         RSFLCN(Y,F,10)=0.0
         DO 20 D=1,MNUMCR-2
            RSFLCN(Y,F,10)=RSFLCN(Y,F,10)+RSFLCN(Y,F,D)
 20   CONTINUE
      END SUBROUTINE FUELCN
!********************************************************************
!     OUTPUT NEMS CONSUMPTION
!********************************************************************
      SUBROUTINE NEMSCN
      IMPLICIT NONE
      INTEGER Y,D,F

      Y=CURCALYR-(baseyr-1)
      SLCON(Y,10)=0.0
      qpvrs(NationalPtr,y)=0.0
!********************************************************************
!     CALCULATE DIVISIONAL FUEL CONSUMPTION
!********************************************************************
      DO 10 D=1,MNUMCR-2
! RENEWABLES
!      SOLAR CHANGED FOR DISTRIBUTED GENERATION BY PV
         IF(CURIYR.LT.RECSYEAR-BASEYR+1) THEN
            QPVRS(D,Y)=QPVRS(D,Y-1)*FossilHR/3412.
!      SOLAR KWH GENERATION BY PV IN QUADS
          ELSE
            QPVRS(D,Y)=TRILLS(Y,D,1)*FossilHR/3412.
         ENDIF
            QPVRS(NationalPtr,y)=QPVRS(NationalPtr,y)+qpvrs(d,y)

         SLCON(Y,10)=SLCON(Y,10)+SLCON(Y,D)
         QSTRS(D,Y)=SLCON(Y,D)/1000000.
! GAS
         QNGRS(D,Y)=RSFLCN(Y,1,D)
         QGFRS(D,Y)=QNGRS(D,Y)*1.0
         QGIRS(D,Y)=QNGRS(D,Y)*0.0
! ELECTRIC
         QELRS(D,Y)=RSFLCN(Y,2,D)
! DISTILLATE
         QDSRS(D,Y)=RSFLCN(Y,3,D)
! LPG
         QLGRS(D,Y)=RSFLCN(Y,4,D)
         QPRRS(D,Y)=QLGRS(D,Y)
! KEROSENE
         QKSRS(D,Y)=RSFLCN(Y,5,D)
! COAL
         QCLRS(D,Y)=0.0
! WOOD
         QBMRS(D,Y)=RSFLCN(Y,7,D)
! GEOTHERMAL
         QGERS(D,Y)=RSFLCN(Y,8,D)
 10   CONTINUE
      END SUBROUTINE NEMSCN
!********************************************************************
!     OUTPUT RESDREP
!********************************************************************
      SUBROUTINE RESDRP
      IMPLICIT NONE
      INTEGER Y, B, D, F, F2,Y1
!*******************************************************************
!  AGGREGATE EXISTING HOUSES, NEW HOUSES & HOUSING STARTS
!*******************************************************************
      Y=CURIYR
        DO 10 B=1,MNUMBLDG
          RSEH(Y,B)=0.0
          RSNH(Y,B)=0.0
          RSHSEADD(Y,B)=0.0
          DO 10 D=1,MNUMCR-2
            RSEH(Y,B)=RSEH(Y,B)+EH(CURCALYR,B,D)
            RSNH(Y,B)=RSNH(Y,B)+NH(CURCALYR,B,D)
            RSHSEADD(Y,B)=RSHSEADD(Y,B)+HSEADD(CURCALYR,B,D)
 10   CONTINUE
!*******************************************************************
!  AGGREGATE EXISTING HOUSES, NEW HOUSES & HOUSING STARTS
!*******************************************************************
          HSETOT(CURCALYR)=0.0
        DO 15 D=1,MNUMCR-2
          RSHOUSES(Y,D)=0.0
          DO 15 B=1,MNUMBLDG
            RSHOUSES(Y,D)=RSHOUSES(Y,D)+(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
            totalhouses(y,b,d)=(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))
            HSETOT(CURCALYR)=HSETOT(CURCALYR)+RSHOUSES(Y,D)
 15   CONTINUE
!*******************************************************************
!  COMPUTE HOUSING SHARES FOR "FROZEN HOUSING STARTS" ANALYSIS
!*******************************************************************
        DO 17 D=1,MNUMCR-2
          DO 17 B=1,MNUMBLDG
            HSESHR(CURCALYR,B,D)=(EH(CURCALYR,B,D)+NH(CURCALYR,B,D))/HSETOT(CURCALYR)
 17   CONTINUE
!
!*******************************************************************
!  AGGREGATE HEATING CONSUMPTION
!*******************************************************************
        DO 20 F=1,8
          RSHTRCON(Y,F)=0.0
          DO 20 D=1,MNUMCR-2
            IF (F.EQ.7) THEN
              RSHTRCON(Y,7)=RSHTRCON(Y,7)+HTRCON(Y,7,D)
            ELSE IF (F.EQ.6) THEN
              RSHTRCON(Y,6)=RSHTRCON(Y,6)+HTRCON(Y,6,D)+SHTCON(Y,7,D)
            ELSE IF (F.EQ.8) THEN
!              RSHTRCON(Y,8)=RSHTRCON(Y,8)+SHTCON(Y,6,D)  !removing coal from consumption totals
              RSHTRCON(Y,8)=0.0
            ELSE
              RSHTRCON(Y,F)=RSHTRCON(Y,F)+HTRCON(Y,F,D)+SHTCON(Y,F,D)
            END IF
 20   CONTINUE
!*******************************************************************
!  AGGREGATE COOLING CONSUMPTION
!*******************************************************************
             RSCOOLCN(Y,1)=0.0
             RSCOOLCN(Y,2)=0.0
             RSCOOLCN(Y,3)=0.0

          DO 25 D=1,MNUMCR-2
             RSCOOLCN(Y,1)=RSCOOLCN(Y,1)+COOLCN(Y,1,D)
             RSCOOLCN(Y,2)=RSCOOLCN(Y,2)+COOLCN(Y,2,D)
             RSCOOLCN(Y,3)=RSCOOLCN(Y,3)+COOLCN(Y,3,D)
 25   CONTINUE
!*******************************************************************
!  AGGREGATE WATER HEATING CONSUMPTION
!*******************************************************************
          RSH2OCON(Y,5)=SLCON(Y,10)
        DO 30 F=1,4
          RSH2OCON(Y,F)=0.0
         DO 30 D=1,MNUMCR-2
          RSH2OCON(Y,F)=RSH2OCON(Y,F)+H2OCON(Y,F,D)
 30   CONTINUE
!*******************************************************************
!  AGGREGATE COOKING CONSUMPTION
!*******************************************************************
          RSCKCON(Y,1)=0.0
          RSCKCON(Y,2)=0.0
          RSCKCON(Y,3)=0.0
        DO 35 D=1,MNUMCR-2
          RSCKCON(Y,1)=RSCKCON(Y,1)+CKCON(Y,1,D)
          RSCKCON(Y,2)=RSCKCON(Y,2)+CKCON(Y,2,D)
          RSCKCON(Y,3)=RSCKCON(Y,3)+CKCON(Y,3,D)
 35   CONTINUE
!*******************************************************************
!  AGGREGATE DRYERS CONSUMPTION
!*******************************************************************
        DO 37 F=1,2
            RSDRYCON(Y,F)=0.0
          DO 37 D=1,MNUMCR-2
            RSDRYCON(Y,F)=RSDRYCON(Y,F)+DRYCON(Y,F,D)
 37   CONTINUE
!*******************************************************************
!  AGGREGATE APPLIANCE CONSUMPTION
!*******************************************************************
            RSAPCON(Y,1)=0.0
            RSAPCON(Y,2)=0.0
            RSAPCON(Y,3)=0.0
            RSAPCON(Y,4)=0.0
       DO 40 D=1,MNUMCR-2
            RSAPCON(Y,1)=RSAPCON(Y,1)+APLCON(Y,1,D) !Gas
            RSAPCON(Y,2)=RSAPCON(Y,2)+APCON(Y,D)    !EL
            RSAPCON(Y,3)=RSAPCON(Y,3)+APLCON(Y,3,D) !DIS
            RSAPCON(Y,4)=RSAPCON(Y,4)+APLCON(Y,2,D) !LPG
 40   CONTINUE
!*******************************************************************
!  AGGREGATE THE REST OF CONSUMPTION
!*******************************************************************
            RSREFCON(Y)=0.0
            RSFRZCON(Y)=0.0
            RSLTCON(Y) =0.0
            RSLTCON(recsyear-(baseyr-1)) =0.0
            RSCSWCON(Y)=0.0
            RSDSWCON(Y)=0.0
            RSTVRCON(Y)=0.0
            RSPCRCON(Y)=0.0
            RSFANCON(Y)=0.0
       DO 50 D=1,MNUMCR-2
            RSREFCON(Y)=RSREFCON(Y)+REFCON(Y,D)
            RSFRZCON(Y)=RSFRZCON(Y)+FRZCON(Y,D)
            RSLTCON(recsyear-(baseyr-1))= RSLTCON(recsyear-(baseyr-1))+LTCON(recsyear-(baseyr-1),D)
            RSLTCON(Y)= RSLTCON(Y)+LTCON(Y,D)
            RSCSWCON(Y)=RSCSWCON(Y)+CSWCON(Y,D)
            RSDSWCON(Y)=RSDSWCON(Y)+DSWCON(Y,D)
            RSTVRCON(Y)=RSTVRCON(Y)+TVRCON(Y,D)
            RSFANCON(Y)=RSFANCON(Y)+FANCON(Y,D)
            RSPCRCON(Y)=RSPCRCON(Y)+PCRCON(Y,D)
 50   CONTINUE
      END SUBROUTINE RESDRP
!********************************************************************
!     SUPPLEMENTAL NEMS REPORT
!*******************************************************************
      SUBROUTINE RESDRP2
      IMPLICIT NONE
      ! COMMON/DBEFFOUT/RSNEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2),RSEEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2)  !DYN
      ! REAL*4 RSNEFDB1,RSEEFDB1  !DYN
      REAL*4 EHANDNH(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
!      REAL*4 TBENCH(RECSYEAR:ENDYR,6),ABENCH(RECSYEAR:ENDYR,6),TRSCON(RECSYEAR:ENDYR,6)
      REAL*4 RACUNTS(MNUMBLDG,MNUMCR-2), X, TEMP
      REAL*4 NUME(RECSYEAR:ENDYR+1,15), DEN(RECSYEAR:ENDYR+1,15),NUME1(RECSYEAR:ENDYR+1,15,MNUMBLDG,MNUMCR-2), DEN1(RECSYEAR:ENDYR+1,15,MNUMBLDG,MNUMCR-2)
      REAL*4 RSCLUSR(RECSYEAR:ENDYR+1)
      REAL*4 RSCLUSC(RECSYEAR:ENDYR+1)
      REAL*4 HSHINDE(RECSYEAR:ENDYR,6,MNUMCR-2,MNUMBLDG),HSHINDA(RECSYEAR:ENDYR,6,MNUMCR-2,MNUMBLDG),HSHINDN(RECSYEAR:ENDYR,6,MNUMCR-2,MNUMBLDG), &
             HSHELLE(RECSYEAR:ENDYR),HEATOTE(RECSYEAR:ENDYR), &
             HSHELLN(RECSYEAR:ENDYR),HEATOTN(RECSYEAR:ENDYR), &
             HSHELLA(RECSYEAR:ENDYR),HEATOTA(RECSYEAR:ENDYR)
      REAL*4 CSHINDE(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),CSHINDA(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG),CSHINDN(RECSYEAR:ENDYR,MNUMCR-2,MNUMBLDG), &
             CSHELLE(RECSYEAR:ENDYR),COLTOTE(RECSYEAR:ENDYR), &
             CSHELLN(RECSYEAR:ENDYR),COLTOTN(RECSYEAR:ENDYR), &
             CSHELLA(RECSYEAR:ENDYR),COLTOTA(RECSYEAR:ENDYR)
      INTEGER Y, D, B, E, E2, E3, F, T, EV,V,Y1
      INTEGER EU,RECCL,EQC,NUMEQC,RECTY,TYPE,EQT,NUMEQT,OTYPE
      INTEGER EUHT,RECCLHHP,RECTYHT,TYPEHT
      character*15  euprnames(10) !electricity end use price categories
      data euprnames/'Space Heating','Space Cooling','Water Heating','Cooking', &
                  'Clothes Drying','Refrigeration','Freezing','Lighting', &
                  'Appliances','Secondary Heat'/
      CHARACTER*40 FN
      CHARACTER*18 HEN(9),CEN(5),WEN(5), CKEN(3), DRYEN(2)
      DATA HEN/'Electric HP','Other Electric','Gas HP','Gas Other', &
      'Distillate','Liq Petro Gas','Kerosene','Wood Stoves', &
      'Geothermal HP'/
      DATA CEN/'Room AC','Central Air','Heat Pump','GSHP','GHP'/
      DATA WEN/'Natural Gas','Electric','Distillate','Liq Petro Gas', &
      'Solar'/
      DATA CKEN/'Natural Gas','Liq Petro Gas','Electric'/
      DATA DRYEN/'Natural Gas','Electric'/
      DATA RACUNTS/1.73,2.00,1.61,1.49,1.69,1.85,1.93,1.10,1.40, &
       1.35,1.62,1.32,1.38,1.46,1.41,1.14,1.62,1.32, &
       1.00,1.26,1.53,1.51,1.61,1.55,1.71,1.00,1.00/

!*******************************************************************
!   AGGREGATE HEATING SYSTEMS
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1
!
      DO 2 Y=RECSYEAR,lastyr+baseyr-1
         Y1=Y-BASEYR+1
       DO 2 E2=1,MNUMCR-2
          RSHTRS(Y1,E2)=0.0
 2    CONTINUE
      DO 4 Y=RECSYEAR,lastyr+baseyr-1
        Y1=Y-BASEYR+1
        DO 4 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF (EQC.EQ.1)  THEN
             E2=2
          ELSE IF (EQC.EQ.2) THEN
             E2=1
          ELSE IF (EQC.LE.4) THEN
             E2=4
          ELSE IF (EQC.EQ.5) THEN
             E2=7
          ELSE IF (EQC.EQ.6) THEN
             E2=6
          ELSE IF (EQC.LE.8) THEN
             E2=5
          ELSE IF (EQC.EQ.9) THEN
             E2=8
          ELSE IF (EQC.EQ.10) THEN
             E2=9
          ELSE IF (EQC.EQ.11) THEN
             E2=3
          END IF
          DO 4 B=1,MNUMBLDG
            DO 4 D=1,MNUMCR-2
              IF (Y.EQ.RECSYEAR) THEN
                RSHTRS(Y1,E2)=RSHTRS(Y1,E2)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSHTRS(Y1,E2)=RSHTRS(Y1,E2)+  EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+  EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) &
               +EQCSUR(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              END IF
 4    CONTINUE
!*******************************************************************
!     AGGREGATE HEATING SYSTEMS FOR COMPUTING AGGREGATE SHELL
!*******************************************************************
      DO 5 Y=RECSYEAR,lastyr+baseyr-1
       DO 5 F=1,MNUMFUEL-2
        DO 5 D=1,MNUMCR-2
         DO 5 B=1,MNUMBLDG
          HSHINDE(Y,F,D,B)=0.0
          HSHINDA(Y,F,D,B)=0.0
          HSHINDN(Y,F,D,B)=0.0
 5    CONTINUE
      DO 6 Y=RECSYEAR,lastyr+baseyr-1
       DO 6 D=1,MNUMCR-2
        DO 6 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          F=RTFUEL(RECCL)
          DO 6 B=1,MNUMBLDG
              IF (Y.EQ.RECSYEAR) THEN
                HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D)
                HSHINDN(Y,F,D,B)=0.0
                HSHINDA(Y,F,D,B)=0.0
              ELSE
                HSHINDE(Y,F,D,B)=HSHINDE(Y,F,D,B)+EQCESE(Y,RECCL,B,D) &
              +   EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D) &
              +EQCRP90RP(Y,RECCL,B,D)
                HSHINDN(Y,F,D,B)=HSHINDN(Y,F,D,B)+EQCADD(Y,RECCL,B,D)
                HSHINDA(Y,F,D,B)=HSHINDA(Y,F,D,B) &
              +   EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
              END IF
 6    CONTINUE
!*******************************************************************
!     CALCULATE HEATING SHELL INDICES FOR REPORT
!*******************************************************************
      DO 7 Y=RECSYEAR,lastyr+baseyr-1
        HSHELLE(Y)= 0.0
        HSHELLN(Y)= 0.0
        HSHELLA(Y)= 0.0
        DO 7 F=1,MNUMFUEL-2
          DO 7 D=1,MNUMCR-2
           DO 7 B=1,MNUMBLDG
            IF (Y.EQ.RECSYEAR) THEN
            HSHELLE(Y)=HSHELLE(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLN(Y)=HSHELLN(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLA(Y)=HSHELLA(Y)+(HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            ELSE
            HSHELLE(Y)=HSHELLE(Y) + (HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            HSHELLN(Y)=HSHELLN(Y) + (HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B))
            HSHELLA(Y)=HSHELLA(Y) + (HSHINDA(Y,F,D,B)*AHSHELL(Y,F,D,B)+ &
                                    HSHINDN(Y,F,D,B)*NHSHELL(Y,F,D,B) + &
                                    HSHINDE(Y,F,D,B)*EHSHELL(Y,F,D,B))
            END IF
 7    CONTINUE
          DO 8 Y=RECSYEAR,lastyr+baseyr-1
              HEATOTE(Y)=0.0
              HEATOTN(Y)=0.0
              HEATOTA(Y)=0.0
            DO 8 F=1,MNUMFUEL-2
              DO 8 D=1,MNUMCR-2
               DO 8 B=1,MNUMBLDG
                HEATOTE(Y)=HEATOTE(Y)+HSHINDE(Y,F,D,B)
                HEATOTN(Y)=HEATOTN(Y)+HSHINDN(Y,F,D,B)
                HEATOTA(Y)=HEATOTA(Y)+HSHINDN(Y,F,D,B)+HSHINDE(Y,F,D,B) &
            +   HSHINDA(Y,F,D,B)
 8    CONTINUE
!*******************************************************************
          DO 10 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
           IF (HEATOTE(Y).GT.0.0) THEN
               HSHELL1(Y1)=HSHELLE(Y)/HEATOTE(Y)
           ELSE
               HSHELL1(Y1)=1.0
           END IF
           IF (HEATOTN(Y).GT.0.0) THEN
               HSHELL2(Y1)=HSHELLN(Y)/HEATOTN(Y)
           ELSE
               HSHELL2(Y1)=1.0
           END IF
           IF (HEATOTA(Y).GT.0.0) THEN
               HSHELL3(Y1)=HSHELLA(Y)/HEATOTA(Y)
           ELSE
               HSHELL3(Y1)=1.0
           END IF
 10   CONTINUE
!*******************************************************************
!   WRITE OUT VARIABLES OF SPECIAL INTEREST
!*******************************************************************
        write(9,*) 'fuel prices'
        DO D=1,MNUMCR-2
         WRITE(9,*) 'D=', D
         WRITE(9,*) 'Dist   LPG    NatGas    Elec    Kero   Wood'
         DO Y=RECSYEAR,lastyr+baseyr-1
          WRITE(9,223) Y,(PRICES(F,D,Y),F=1,6)
         ENDDO
        ENDDO
 223    FORMAT(3X,I5,6(3X,F5.2))

        Write(9,*) 'ALL HOUSING STARTS'
        DO D=1,MNUMCR-2
        WRITE(9,*) 'D=', D
         DO Y=RECSYEAR,lastyr+baseyr-1
         WRITE(9,224) Y,(HSEADD(Y,B,D),B=1,MNUMBLDG)
 224    FORMAT(3X,I5,3(3X,F9.2))
        ENDDO
        ENDDO

        write(9,*) 'average squarefootage all housing types and divisions'
        DO Y=RECSYEAR-BASEYR+1,lastyr
         WRITE(9,*) Y+BASEYR-1, SQFTAVG(Y)
        ENDDO

        write(9,*) 'single family replacement uecs - heating'
        do d=1,MNUMCR-2
        WRITE(9,*) 'D=',D
        WRITE(9,1140)(RTCLNAME(E),E=1,NHeatClasses)
         DO Y=RECSYEAR,lastyr+baseyr-1
          WRITE(9,1141) Y,(EQCRUEC(Y,E,1,D),E=1,NHeatClasses)
        ENDDO
        ENDDO
 1140   FORMAT(6X,11(1X,a9))
 1141   FORMAT(I6,11(1X,F9.4))

        write(9,*) 'single family average uecs - heating'
        do d=1,MNUMCR-2
        WRITE(9,*) 'D=',D
        WRITE(9,1140)(RTCLNAME(E),E=1,NHeatClasses)
         DO Y=RECSYEAR,lastyr+baseyr-1
          WRITE(9,1141) Y,(EQCAUEC(Y,E,1,D),E=1,NHeatClasses)
        ENDDO
        ENDDO

        write(9,*) 'single family new uecs - heating'
        do d=1,MNUMCR-2
        WRITE(9,*) 'D=',D
        WRITE(9,1140)(RTCLNAME(E),E=1,NHeatClasses)
         DO Y=RECSYEAR,lastyr+baseyr-1
          WRITE(9,1141) Y,(EQCNUEC(Y,E,1,D),E=1,NHeatClasses)
        ENDDO
        ENDDO

      write(9,*) 'bnchfct'
      DO Y=(RECSYEAR-baseyr+1),(LASTSTEOYR-baseyr+3)  !BNCHFCT adjustment for RECS2009 through first two years after hard-bench STEO year   ! STEOread-avg
                                                      !(last two years of factors should be same and carried through projection)            ! STEOread-avg
       WRITE(9,*) Y
       DO D=1,9
        WRITE(9,114) D,(BNCHFCT(Y,F,D),F=1,6)
      ENDDO
      ENDDO
 114  FORMAT(2X,I1,6(1X,F9.4))

!*******************************************************************
!   AGGREGATE COOLING SYSTEMS
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2
!
      DO 20 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          IF (E.EQ.1) THEN
             E2=5
          ELSE IF (E.EQ.2) THEN
             E2=4
          ELSE IF (E.EQ.3) THEN
             E2=1
          ELSE IF (E.EQ.4) THEN
             E2=3
          ELSE IF (E.EQ.5) THEN
             E2=2
          END IF
          RSCOOLERS(Y1,E2)=0.0
          DO 20 B=1,MNUMBLDG
            DO 20 D=1,MNUMCR-2
              X=1.0
              IF (RTCLNAME(RECCL).EQ.'ROOM_AIR') X=1.0 ! RACUNTS(B,D)

              IF (Y.EQ.RECSYEAR) THEN
                RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+EQCESE(Y,RECCL,B,D)*X
              ELSE
               RSCOOLERS(Y1,E2)=RSCOOLERS(Y1,E2)+ (  EQCESE(Y,RECCL,B,D) &
            +   EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCSUR(Y,RECCL,B,D))*X
              END IF
 20   CONTINUE
!*******************************************************************
!     AGGREGATE COOLING SYSTEMS FOR COMPUTING AGGREGATE SHELL
!*******************************************************************
      DO 105 Y=RECSYEAR,lastyr+baseyr-1
        DO 105 D=1,MNUMCR-2
         DO 105 B=1,MNUMBLDG
          CSHINDE(Y,D,B)=0.0
          CSHINDA(Y,D,B)=0.0
          CSHINDN(Y,D,B)=0.0
 105    CONTINUE
      DO 106 Y=RECSYEAR,lastyr+baseyr-1
       DO 106 D=1,MNUMCR-2
        DO 106 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 106 B=1,MNUMBLDG
              IF (Y.EQ.RECSYEAR) THEN
                CSHINDE(Y,D,B)=CSHINDE(Y,D,B)+EQCESE(Y,RECCL,B,D)
              ELSE
                CSHINDE(Y,D,B)=CSHINDE(Y,D,B)+EQCESE(Y,RECCL,B,D)+ &
                 EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D)+ &
                 EQCRP90RP(Y,RECCL,B,D)
                CSHINDN(Y,D,B)=CSHINDN(Y,D,B)+EQCADD(Y,RECCL,B,D)
                CSHINDA(Y,D,B)=CSHINDA(Y,D,B)+ &
                 EQCREP(Y,RECCL,B,D) + EQCSUR(Y,RECCL,B,D)
              END IF
 106    CONTINUE
!*******************************************************************
!     CALCULATE COOLING SHELL INDICES FOR REPORT
!*******************************************************************
      DO 107 Y=RECSYEAR,lastyr+baseyr-1
        CSHELLE(Y)= 0.0
        CSHELLN(Y)= 0.0
        CSHELLA(Y)= 0.0
        DO 107 D=1,MNUMCR-2
         DO 107 B=1,MNUMBLDG
          IF (Y.EQ.RECSYEAR) THEN
            CSHELLE(Y)=CSHELLE(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLN(Y)=CSHELLN(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLA(Y)=CSHELLA(Y) + (CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
          ELSE
            CSHELLE(Y)=CSHELLE(Y)+(CSHINDE(Y,D,B)*ECSHELL(Y,D,B))
            CSHELLN(Y)=CSHELLN(Y)+(CSHINDN(Y,D,B)*NCSHELL(Y,D,B))
            CSHELLA(Y)=CSHELLA(Y)+(CSHINDE(Y,D,B)*ECSHELL(Y,D,B) &
        +                          CSHINDN(Y,D,B)*NCSHELL(Y,D,B) &
        +                          CSHINDA(Y,D,B)*ACSHELL(Y,D,B) )
          END IF
 107  CONTINUE
!
      DO 108 Y=RECSYEAR,lastyr+baseyr-1
        COLTOTE(Y)=0.0
        COLTOTN(Y)=0.0
        COLTOTA(Y)=0.0
        DO 108 D=1,MNUMCR-2
         DO 108 B=1,MNUMBLDG
          COLTOTE(Y)=COLTOTE(Y)+ CSHINDE(Y,D,B)
          COLTOTN(Y)=COLTOTN(Y)+ CSHINDN(Y,D,B)
          COLTOTA(Y)=COLTOTA(Y)+ CSHINDE(Y,D,B) + CSHINDN(Y,D,B) &
            +   CSHINDA(Y,D,B)
 108  CONTINUE
!*******************************************************************
      DO 109 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        IF (COLTOTE(Y).GT.0.0) THEN
          CSHELL1(Y1)=CSHELLE(Y)/COLTOTE(Y)
        ELSE
          CSHELL1(Y1)=1.0
        ENDIF
        IF (COLTOTN(Y).GT.0.0) THEN
          CSHELL2(Y1)=CSHELLN(Y)/COLTOTN(Y)
        ELSE
          CSHELL2(Y1)=1.0
        ENDIF
        IF (COLTOTA(Y).GT.0.0) THEN
          CSHELL3(Y1)=CSHELLA(Y)/COLTOTA(Y)
        ELSE
          CSHELL3(Y1)=1.0
        ENDIF
 109  CONTINUE

!*******************************************************************
!     AGGREGATE WATER HEATING SYSTEMS
!*******************************************************************
      DO 25 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 25 E=1,5
          RSWATER(Y1,E)=0.0
 25   CONTINUE
!*******************************************************************
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5
!
      DO 30 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 30 D=1,MNUMCR-2
          DO 30 B=1,MNUMBLDG
            DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Y.EQ.RECSYEAR) THEN
                RSWATER(Y1,EQC)=RSWATER(Y1,EQC)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSWATER(Y1,EQC)=RSWATER(Y1,EQC)  +EQCESE(Y,RECCL,B,D) &
                 +EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D) + &
                  EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D) + &
                 EQCREP(Y,RECCL,B,D)  +EQCSUR(Y,RECCL,B,D)
              END IF
 30   CONTINUE
!*******************************************************************
!   WRITE OUT WATER HEATING MARKET SHARES
!*******************************************************************
      write(9,*) 'rswater shares?'
      DO Y=RECSYEAR,lastyr+baseyr-1
         Y1=Y-BASEYR+1
        WRITE(9,3773) Y,(RSWATER(Y1,E3),E3=1,5)
      ENDDO
 3773 FORMAT(4X,I4,5(1X,F14.3))
!*******************************************************************
!   AGGREGATE COOKING SYSTEMS
!   SET EU = 6 TO SEARCH THE STOVE (COOK) SECTION OF THE DATA
!*******************************************************************
      EU = 6
!
      DO 40 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 40 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSCOOK(Y1,EQC)=0.0
          DO 40 B=1,MNUMBLDG
             DO 40 D=1,MNUMCR-2
              IF (Y.EQ.RECSYEAR) THEN
                RSCOOK(Y1,EQC)=RSCOOK(Y1,EQC)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSCOOK(Y1,EQC)=RSCOOK(Y1,EQC)+  EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                  EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)+ &
                  EQCREP(Y,RECCL,B,D)  +EQCSUR(Y,RECCL,B,D)
              END IF
 40   CONTINUE
!*******************************************************************
!     AGGREGATE STOVE (COOK) SYSTEMS FOR MARKET SHARE ANALYSIS
!*******************************************************************
      NUMEQT=RTTYPECT(EU+1)-RTTYPECT(EU)

!*******************************************************************
!   WRITE OUT STOVE (COOK) MARKET SHARES
!*******************************************************************
      WRITE(9,*) ' '
      write(9,*) 'rscook'
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
      DO Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        WRITE(9,3117) Y,(RSCOOK(Y1,EQC),EQC=1,NUMEQC)
      ENDDO
 3117 FORMAT(4X,I7,3(1X,F12.1))
!*******************************************************************
!   AGGREGATE DRYING SYSTEMS
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7
!
      DO 50 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSDRY(Y1,EQC)=0.0
          DO 50 B=1,MNUMBLDG
             DO 50 D=1,MNUMCR-2
               IF (Y.EQ.RECSYEAR) THEN
                 RSDRY(Y1,EQC)=RSDRY(Y1,EQC)+EQCESE(Y,RECCL,B,D)
               ELSE
                 RSDRY(Y1,EQC)=RSDRY(Y1,EQC)+  EQCESE(Y,RECCL,B,D) + &
                   EQCRP90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D) + &
                   EQCSR90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)+ &
                   EQCREP(Y,RECCL,B,D) +EQCSUR(Y,RECCL,B,D)
               END IF
 50   CONTINUE
!*******************************************************************
!   WRITE OUT DRYER MARKET SHARES
!*******************************************************************
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
      write(9,*) 'rsdry'
      DO Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        WRITE(9,4117) Y,(RSDRY(Y1,EQC),EQC=1,NUMEQC)
      ENDDO
 4117 FORMAT(4X,I7,2(1X,F12.2))

!*******************************************************************
!   AGGREGATE FOOD REFRIGERATION SYSTEMS
!   SET EU = 8 TO SEARCH THE FOOD REFRIG SECTION OF THE DATA
      EU = 8
!
!*******************************************************************
      DO 60 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 60 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSREF(Y1)=0.0
          DO 60 B=1,MNUMBLDG
            DO 60 D=1,MNUMCR-2
              IF (Y.EQ.RECSYEAR) THEN
                RSREF(Y1)=RSREF(Y1)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSREF(Y1)=RSREF(Y1)  + EQCESE(Y,RECCL,B,D)    + &
                EQCRP90(Y,RECCL,B,D) + EQCSR90(Y,RECCL,B,D)   + &
                EQCADD(Y,RECCL,B,D)  + EQCRP90RP(Y,RECCL,B,D) + &
                EQCREP(Y,RECCL,B,D)  + EQCSUR(Y,RECCL,B,D)
             END IF
60    CONTINUE

!*******************************************************************
!   WRITE OUT REFRIGERATOR MARKET SHARES
!*******************************************************************
      WRITE(9,*) 'Refrigerator Equipment Stock'
      do y=RECSYEAR-BASEYR+1,lastyr
       WRITE(9,*) RSREF(Y)
      enddo

      WRITE(9,*) 'Refrigerator Equipment Shares'
!
!*******************************************************************
!   AGGREGATE FOOD FREEZING SYSTEMS
!   SET EU = 9 TO SEARCH THE FOOD FREEZING SECTION OF THE DATA
      EU = 9
!
!*******************************************************************
      DO 65 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 65 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          RSFRZ(Y1)=0.0
          DO 65 B=1,MNUMBLDG
            DO 65 D=1,MNUMCR-2
              IF (Y.EQ.RECSYEAR) THEN
                RSFRZ(Y1)=RSFRZ(Y1)+EQCESE(Y,RECCL,B,D)
              ELSE
                RSFRZ(Y1)=RSFRZ(Y1)+  EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Y,RECCL,B,D) +  EQCSR90(Y,RECCL,B,D)+ &
                  EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) &
                +  EQCSUR(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              END IF
 65   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW HEATER EFFICIENCIES
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1
!
      DO 70 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 70 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        NUME(Y,EQC)=0.0
        DEN(Y,EQC)=0.0
        DO 70 D=1,MNUMCR-2
         DO 70 B=1,MNUMBLDG
           IF( (WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
               (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
                 (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
             NUME(Y,EQC)=NUME(Y,EQC) +EQCADD(Y,RECCL,B,D)  &
              * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
              + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
              * (1/WTEQCEFFN(Y,RECCL,B,D)) &
              + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             NUME1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  &
              * (1/WTEQCEFFHV(Y,RECCL,B,D)) &
              + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
              * (1/WTEQCEFFN(Y,RECCL,B,D)) &
              + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+ EQCADD(Y,RECCL,B,D)  + &
              EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)  + &
              EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
           ENDIF
 70   CONTINUE
!*******************************************************************
      DO 75 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 75 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
           RSNEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_FA
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_FA
        ENDIF
 75   CONTINUE
     DO 76 D=1,MNUMCR-2
      DO 76 B=1,MNUMBLDG
       DO 76 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 76 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
          IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
            IF (EQC.NE.2) THEN
           RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
           RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412
            END IF
          END IF
 76   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW COOLER EFFICIENCIES
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2
!
      DO 80 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 80 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 80 D=1,MNUMCR-2
            DO 80 B=1,MNUMBLDG
            IF (E.NE.1) THEN
            IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFHV(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) + EQCADD(Y,RECCL,B,D)  * &
          (1/WTEQCEFFHV(Y,RECCL,B,D))  &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)  + &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,E,B,D)= EQCADD(Y,RECCL,B,D)  * &
          (1/WTEQCEFFHV(Y,RECCL,B,D))  &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)  + &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            END IF
            ELSE
             IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
            NUME(Y,E)=NUME(Y,E) +EQCADD(Y,RECCL,B,D)* &
          (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,E)=DEN(Y,E)+ EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,E,B,D)=(EQCADD(Y,RECCL,B,D)+&
       +  EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,E,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
            END IF
           END IF
 80   CONTINUE
!*******************************************************************
      DO 85 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 85 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP'.AND.DEN(Y,EQC).GT.0.0) THEN
           RSNEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFCL(Y1,3)=(NUME(Y,EQC)/DEN(Y,EQC))/3.412   ! GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! CENT_AIR
        ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)   ! ROOM_AIR
        ENDIF
 85   CONTINUE
!
      DO 86 D=1,MNUMCR-2
       DO 86 B=1,MNUMBLDG
        DO 86 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
         DO 86 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
           IF(DEN1(Y,EQC,B,D).GT.0.0) THEN
              IF (EQC.LE.3) THEN
                RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412
              ELSE
                RSNEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
              ENDIF
           ENDIF
 86   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHESWASHER EFFICIENCIES
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU = 3
!
      DO 87 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 87 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 87 D=1,MNUMCR-2
            DO 87 B=1,MNUMBLDG
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 87   CONTINUE
!*******************************************************************
      DO 88 D=1,MNUMCR-2
       DO 88 B=1,MNUMBLDG
        DO 88 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 88 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
           IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 88   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHESWASHER EFFICIENCIES
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
!*******************************************************************
      EU = 4
!
      DO 89 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 89 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 89 D=1,MNUMCR-2
            DO 89 B=1,MNUMBLDG
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 89   CONTINUE
!*******************************************************************
      DO 90 D=1,MNUMCR-2
       DO 90 B=1,MNUMBLDG
        DO 90 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 90 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 90   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW H2O EFFICIENCIES
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5
!
      DO 91 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 91 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 91 D=1,MNUMCR-2
            DO 91 B=1,MNUMBLDG
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 91   CONTINUE
!*******************************************************************
      DO 92 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 92 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH'.AND.DEN(Y,EQC).GT.0.0) THEN
           RSNEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)   ! ELEC_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)   ! NG_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)   ! DIST_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH'.AND.DEN(Y,EQC).GT.0.0) &
          THEN
           RSNEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)   ! LPG_WH
        ENDIF
 92   CONTINUE
     DO 93 D=1,MNUMCR-2
      DO 93 B=1,MNUMBLDG
       DO 93 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 93 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
          IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 93   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW STOVE EFFICIENCIES
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6
!
      DO 94 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 94 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 94 D=1,MNUMCR-2
            DO 94 B=1,MNUMBLDG
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 94   CONTINUE
!*******************************************************************
     DO 95 D=1,MNUMCR-2
      DO 95 B=1,MNUMBLDG
       DO 95 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 95 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
          IF(DEN1(Y,EQC,B,D).GT.0.0)  RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 95   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW CLOTHES DRYER EFFICIENCIES
!   SET EU = 7 TO SEARCH THE CLOTHES DRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7
!
      DO 196 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 196 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 196 D=1,MNUMCR-2
            DO 196 B=1,MNUMBLDG
           IF((WTEQCEFFN(Y,RECCL,B,D).GT.0.0).AND. &
              (WTEQCEFFR(Y,RECCL,B,D).GT.0.0)) THEN
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * (1/WTEQCEFFN(Y,RECCL,B,D)) &
       + EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
              ENDIF
 196   CONTINUE
!*******************************************************************
     DO 97 D=1,MNUMCR-2
      DO 97 B=1,MNUMBLDG
       DO 97 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 97 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
         EQC=RTCLEQCL(RECCL)
           IF(DEN1(Y,EQC,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
 97   CONTINUE
!********************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW REFRIGERATOR EFFICIENCIES
!   SET EU = 6 TO SEARCH THE FOOD REFRIG SECTION OF THE DATA
!********************************************************************
      EU = 8
!
      DO 99 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 99 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 99 D=1,MNUMCR-2
            DO 99 B=1,MNUMBLDG
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 99  CONTINUE
!*******************************************************************
      DO 100 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        IF (DEN(Y,1).GT.0.0) RSNEFRF(Y1)=NUME(Y,1)/DEN(Y,1)   !REF
 100   CONTINUE
          DO 101 D=1,MNUMCR-2
            DO 101 B=1,MNUMBLDG
             DO 101 Y=RECSYEAR+1,lastyr+baseyr-1
              DO 101 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
             Y1=Y-BASEYR+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)   !REF
 101   CONTINUE
!********************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED NEW FOOD FREEZER EFFICIENCIES
!   SET EU = 9 TO SEARCH THE FOOD FREEZER SECTION OF THE DATA
!********************************************************************
      EU = 9
      DO 102 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 102 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 102 D=1,MNUMCR-2
            DO 102 B=1,MNUMBLDG
             NUME(Y,EQC)=NUME(Y,EQC)+((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN(Y,EQC)=DEN(Y,EQC)+EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
             NUME1(Y,EQC,B,D)=((EQCADD(Y,RECCL,B,D) &
       + EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)) &
       * WTEQCEFFN(Y,RECCL,B,D) &
       + EQCRP90(Y,RECCL,B,D)*WTEQCEFFR(Y,RECCL,B,D))
             DEN1(Y,EQC,B,D)=EQCADD(Y,RECCL,B,D)+ &
       EQCREP(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+EQCRP90RP(Y,RECCL,B,D)
 102  CONTINUE
!*******************************************************************
      DO Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        IF (DEN(Y,1).GT.0.0) RSNEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)   !FRZ
      ENDDO
          DO 103 D=1,MNUMCR-2
            DO 103 B=1,MNUMBLDG
             DO 103 Y=RECSYEAR+1,lastyr+baseyr-1
              DO 103 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              Y1=Y-BASEYR+1
        IF (DEN1(Y,1,B,D).GT.0.0) RSNEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)   !FRZ
103      CONTINUE
!*******************************************************************
!   WRITE OUT NEW WEIGHTED EFFICIENCIES
!*******************************************************************
       write(9,*) 'rsnefht'
       DO 6666 Y=RECSYEAR+1,lastyr+baseyr-1
        Y1=Y-BASEYR+1
        WRITE(9,6667) Y1+(baseyr-1),(RSNEFHT(Y1,E3),E3=1,5)
 6667   FORMAT(4X,I4,5(1X,F6.2))
 6666   CONTINUE
       write(9,*) 'rsnefcl'
       DO 6766 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
      WRITE(9,6667) Y1+(baseyr-1),(RSNEFCL(Y1,E3)*3.412,E3=1,5)
 6766   CONTINUE
       write(9,*) 'rsnefhw'
       DO 6866 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
      WRITE(9,6668) Y1+(baseyr-1),(RSNEFHW(Y1,E3),E3=1,4)
 6668   FORMAT(4X,I4,4(1X,F7.4))
 6866   CONTINUE
       write(9,*) 'rsnefrf'
       DO 6966 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
      WRITE(9,6669) Y1+(baseyr-1), RSNEFRF(Y1)
 6669   FORMAT(4X,I4,1X,F7.2)
 6966   CONTINUE
       write(9,*) 'rsneffz'
       DO 6777 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
      WRITE(9,6669) Y1+(baseyr-1), RSNEFFZ(Y1)
 6777   CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST HEATER EFFICIENCIES
!   SET EU = 1 TO SEARCH THE SPACE HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 1
!
      DO 110 Y=RECSYEAR+1,lastyr+baseyr-1
       DO 110 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 110 D=1,MNUMCR-2
            DO 110 B=1,MNUMBLDG
               NUME(Y,EQC)=NUME(Y,EQC)+ &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D)) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
              EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)) )

              DEN(Y,EQC)=DEN(Y,EQC)+ ( EQCESE(Y,RECCL,B,D)+ &
              EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
               EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
               EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )
               NUME1(Y,EQC,B,D)= &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D)) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D))+ &
              EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)) )

              DEN1(Y,EQC,B,D)= ( EQCESE(Y,RECCL,B,D)+ &
              EQCRP90RP(Y,RECCL,B,D)+ EQCRP90(Y,RECCL,B,D) + &
               EQCREP(Y,RECCL,B,D) + EQCADD(Y,RECCL,B,D)+&
               EQCSUR(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D) )
 110  CONTINUE
!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
          RSEEFHT(RECSYEAR-BASEYR+1,1)=RTBASEFF(RECSYEAR,RECCL)          !ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
          RSEEFHT(RECSYEAR-BASEYR+1,2)=RTBASEFF(RECSYEAR,RECCL)          !NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
          RSEEFHT(RECSYEAR-BASEYR+1,3)=RTBASEFF(RECSYEAR,RECCL)          !GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA') THEN
          RSEEFHT(RECSYEAR-BASEYR+1,4)=RTBASEFF(RECSYEAR,RECCL)          !NG_FA
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA') THEN
          RSEEFHT(RECSYEAR-BASEYR+1,5)=RTBASEFF(RECSYEAR,RECCL)          !DIST_FA
        ENDIF
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
         RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        END DO
       END DO
      ENDDO
!
      DO 120 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 120 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,1)=RSEEFHT(RECSYEAR-BASEYR+1,1)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,2)=RSEEFHT(RECSYEAR-BASEYR+1,2)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,3)=RSEEFHT(RECSYEAR-BASEYR+1,3)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_FA') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,4)=RSEEFHT(RECSYEAR-BASEYR+1,4)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'DIST_FA') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHT(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHT(Y1,5)=RSEEFHT(RECSYEAR-BASEYR+1,5)
            END IF
          ENDIF
 120  CONTINUE
    DO 121 D=1,MNUMCR-2
     DO 121 B=1,MNUMBLDG
      DO 121 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 121 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              IF (EQC.NE.2) THEN
               RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
              ELSE
               RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)*3.412
              END IF
            ELSE
              IF (EQC.NE.2) THEN
               RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
              ELSE
               RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)*3.412
              END IF
            END IF
 121  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST COOLER EFFICIENCIES
!   SET EU = 2 TO SEARCH THE SPACE COOLING SECTION OF THE DATA
!*******************************************************************
      EU = 2
!
      DO 130 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 130 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 130 D=1,MNUMCR-2
            DO 130 B=1,MNUMBLDG
             IF (E.EQ.1) THEN
               NUME(Y,E)=NUME(Y,E)+  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
               NUME1(Y,E,B,D)=  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFN(Y,RECCL,B,D))+ &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
             ELSE
               NUME(Y,E)=NUME(Y,E)+  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
               NUME1(Y,E,B,D)=  &
              (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
               EQCADD(Y,RECCL,B,D)*(1/WTEQCEFFHV(Y,RECCL,B,D)) + &
              (EQCRP90RP(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D))* &
               (1/WTEQCEFFN(Y,RECCL,B,D))+ &
               EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D)) + &
              (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
              (1/WTEQCEFFA(Y,RECCL,B,D)))
             ENDIF
!
              DEN(Y,E)=DEN(Y,E)+  EQCESE(Y,RECCL,B,D)  + &
                EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
              DEN1(Y,E,B,D)=  EQCESE(Y,RECCL,B,D)  + &
                EQCRP90(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D) + &
                EQCADD(Y,RECCL,B,D) +EQCSR90(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D)
 130  CONTINUE
!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
          RSEEFCL(RECSYEAR-BASEYR+1,1)=RTBASEFF(RECSYEAR,RECCL)          !ELEC_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
          RSEEFCL(RECSYEAR-BASEYR+1,2)=RTBASEFF(RECSYEAR,RECCL)          !NG_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
          RSEEFCL(RECSYEAR-BASEYR+1,3)=RTBASEFF(RECSYEAR,RECCL)          !GEO_HP
        ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
          RSEEFCL(RECSYEAR-BASEYR+1,4)=RTBASEFF(RECSYEAR,RECCL)          !CENT_AIR
        ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
          RSEEFCL(RECSYEAR-BASEYR+1,5)=RTBASEFF(RECSYEAR,RECCL)          !ROOM_AIR
        ENDIF
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
         RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        END DO
       END DO
     ENDDO
!
      DO 140 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 140 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,1)=RSEEFCL(RECSYEAR-BASEYR+1,1)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,2)=RSEEFCL(RECSYEAR-BASEYR+1,2)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,3)=RSEEFCL(RECSYEAR-BASEYR+1,3)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'CENT_AIR') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,4)=RSEEFCL(RECSYEAR-BASEYR+1,4)
            END IF
          ELSEIF(RTCLNAME(RECCL).EQ.'ROOM_AIR') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFCL(Y1,5)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFCL(Y1,5)=RSEEFCL(RECSYEAR-BASEYR+1,5)
            END IF
          ENDIF
 140  CONTINUE
      DO 141 D=1,MNUMCR-2
      DO 141 B=1,MNUMBLDG
      DO 141 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 141 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
               IF (EQC.LE.3) THEN
                RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))*3.412
               ELSE
                RSEEFDB1(Y1,RECCL,B,D)=(NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D))
               END IF
            ELSE
               IF (EQC.LE.3) THEN
                RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)*3.412
               ELSE
                RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
               ENDIF
            END IF
 141  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST CLOTHES WASHER
!   SET EU = 3 TO SEARCH THE CLOTHES WASHER SECTION OF THE DATA
!*******************************************************************
      EU = 3
!
      DO 142 D=1,MNUMCR-2
      DO 142 B=1,MNUMBLDG
      DO 142 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
 142     CONTINUE
!
       DO 143 Y=RECSYEAR+1,lastyr+baseyr-1
             Y1=Y-BASEYR+1
       DO 143 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 143 D=1,MNUMCR-2
            DO 143 B=1,MNUMBLDG
              NUME1(Y,EQC,B,D)= &
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 143  CONTINUE
!*******************************************************************
       DO 144 D=1,MNUMCR-2
       DO 144 B=1,MNUMBLDG
       DO 144 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 144 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
        END IF
 144  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST DISHWASHER
!   SET EU = 4 TO SEARCH THE DISHWASHER SECTION OF THE DATA
!*******************************************************************
      EU = 4
!
      DO 145 D=1,MNUMCR-2
      DO 145 B=1,MNUMBLDG
      DO 145 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
 145     CONTINUE
!
       DO 146 Y=RECSYEAR+1,lastyr+baseyr-1
             Y1=Y-BASEYR+1
       DO 146 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          DO 146 D=1,MNUMCR-2
            DO 146 B=1,MNUMBLDG
              NUME1(Y,EQC,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL))+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 146  CONTINUE
!*******************************************************************
       DO 147 D=1,MNUMCR-2
       DO 147 B=1,MNUMBLDG
       DO 147 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 147 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
        END IF
 147  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST WATER HEATER EFFIC
!   SET EU = 5 TO SEARCH THE WATER HEATING SECTION OF THE DATA
!*******************************************************************
      EU = 5
!
      DO 150 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 150 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          NUME(Y,E)=0.0
          DEN(Y,E)=0.0
          DO 150 D=1,MNUMCR-2
            DO 150 B=1,MNUMBLDG
               NUME(Y,E)=NUME(Y,E)+ &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN(Y,E)=DEN(Y,E)+(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 150  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
        IF(RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
          RSEEFHW(RECSYEAR-BASEYR+1,1)=RTBASEFF(RECSYEAR,RECCL)          !ELEC_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH') THEN
          RSEEFHW(RECSYEAR-BASEYR+1,2)=RTBASEFF(RECSYEAR,RECCL)          !NG_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH') THEN
          RSEEFHW(RECSYEAR-BASEYR+1,3)=RTBASEFF(RECSYEAR,RECCL)          !DIST_WH
        ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH') THEN
          RSEEFHW(RECSYEAR-BASEYR+1,4)=RTBASEFF(RECSYEAR,RECCL)          !LPG_WH
        ENDIF
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        END DO
       END DO
      ENDDO
!
      DO 160 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 160 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          IF(RTCLNAME(RECCL).EQ.'ELEC_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,1)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,1)=RSEEFHW(RECSYEAR-BASEYR+1,1)          !ELEC_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'NG_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,2)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,2)=RSEEFHW(RECSYEAR-BASEYR+1,2)          !NG_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'DIST_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,3)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,3)=RSEEFHW(RECSYEAR-BASEYR+1,3)          !DIST_WH
            ENDIF
          ELSEIF(RTCLNAME(RECCL).EQ.'LPG_WH') THEN
            IF (DEN(Y,EQC).GT.0.0) THEN
              RSEEFHW(Y1,4)=NUME(Y,EQC)/DEN(Y,EQC)
            ELSE
              RSEEFHW(Y1,4)=RSEEFHW(RECSYEAR-BASEYR+1,4)          !LPG_WH
            ENDIF
          ENDIF
 160  CONTINUE

      DO 161 D=1,MNUMCR-2
       DO 161 B=1,MNUMBLDG
       DO 161 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 161 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
            ENDIF
 161  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST COOKING EFFIC
!   SET EU = 6 TO SEARCH THE COOKING SECTION OF THE DATA
!*******************************************************************
      EU = 6
!
      DO 162 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 162 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 162 D=1,MNUMCR-2
            DO 162 B=1,MNUMBLDG
               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 162  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        END DO
       END DO
      ENDDO
!

      DO 163 D=1,MNUMCR-2
       DO 163 B=1,MNUMBLDG
       DO 163 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 163 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
            ENDIF
 163  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST CLOTHESDRYERS
!   SET EU = 7 TO SEARCH THE CLDRYER SECTION OF THE DATA
!*******************************************************************
      EU = 7
!
      DO 164 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 164 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          E=RTCLEQCL(RECCL)
          DO 164 D=1,MNUMCR-2
            DO 164 B=1,MNUMBLDG
               NUME1(Y,E,B,D)= &
               (EQCESE(Y,RECCL,B,D)*(RTBASEFF(Y,RECCL)) + &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(1/WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(1/WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (1/WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,E,B,D)=(EQCESE(Y,RECCL,B,D)+ &
                EQCRP90(Y,RECCL,B,D)+EQCSR90(Y,RECCL,B,D)+ &
                EQCADD(Y,RECCL,B,D)+EQCREP(Y,RECCL,B,D)+ &
                EQCRP90RP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 164  CONTINUE

!*******************************************************************
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        END DO
       END DO
      ENDDO
!

      DO 165 D=1,MNUMCR-2
       DO 165 B=1,MNUMBLDG
       DO 165 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        DO 165 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
            IF (DEN1(Y,EQC,B,D).GT.0.0) THEN
              RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,EQC,B,D)/DEN1(Y,EQC,B,D)
            ELSE
              RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
            ENDIF
 165  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST REFRIGERATORS
!   SET EU = 8 TO SEARCH THE FOOD REFRIGERATION SECTION OF THE DATA
!*******************************************************************
      EU = 8
!
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFRF(RECSYEAR-BASEYR+1)=RTBASEFF(RECSYEAR,RECCL)
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        ENDDO
       ENDDO
      ENDDO
!
      DO 170 Y=RECSYEAR+1,lastyr+baseyr-1
             Y1=Y-BASEYR+1
       DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
         NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 170 D=1,MNUMCR-2
            DO 170 B=1,MNUMBLDG
              NUME(Y,EQC)=NUME(Y,EQC)+ &
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

              NUME1(Y,EQC,B,D)=&
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 170  CONTINUE
!*******************************************************************
      DO 175 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        IF (DEN(Y,1).GT.0.0) RSEEFRF(Y1)=NUME(Y,1)/DEN(Y,1)
 175  CONTINUE
     DO 176 D=1,MNUMCR-2
      DO 176 B=1,MNUMBLDG
       DO 176 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 176 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
           RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
           RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
        ENDIF
    176  CONTINUE
!*******************************************************************
!   PRELIMINARY CALCULATION OF WEIGHTED EXIST FOOD FREEZERS
!   SET EU = 9 TO SEARCH THE FOOD FREEZING SECTION OF THE DATA
!*******************************************************************
      EU = 9
!
      DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          RSEEFFZ(RECSYEAR-BASEYR+1)=RTBASEFF(RECSYEAR,RECCL)
       DO D=1,MNUMCR-2
        DO B=1,MNUMBLDG
          RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)=RTBASEFF(RECSYEAR,RECCL)
        ENDDO
       ENDDO
      ENDDO
!
      DO 180 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 180 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
          NUME(Y,EQC)=0.0
          DEN(Y,EQC)=0.0
          DO 180 D=1,MNUMCR-2
            DO 180 B=1,MNUMBLDG
            NUME(Y,EQC)=NUME(Y,EQC)+ &
             (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL) + &
             (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
              EQCREP(Y,RECCL,B,D))* &
             (WTEQCEFFN(Y,RECCL,B,D)) + &
              EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
             (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
             (WTEQCEFFA(Y,RECCL,B,D)))
!
            DEN(Y,EQC)=DEN(Y,EQC)+(EQCRP90RP(Y,RECCL,B,D)+ &
              EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
              EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
              EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))

              NUME1(Y,EQC,B,D)=&
               (EQCESE(Y,RECCL,B,D)*RTBASEFF(Y,RECCL)+ &
               (EQCADD(Y,RECCL,B,D)+ EQCRP90RP(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D))*(WTEQCEFFN(Y,RECCL,B,D)) + &
                EQCRP90(Y,RECCL,B,D)*(WTEQCEFFR(Y,RECCL,B,D))+ &
               (EQCSR90(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))* &
               (WTEQCEFFA(Y,RECCL,B,D)))
!
              DEN1(Y,EQC,B,D)=(EQCRP90RP(Y,RECCL,B,D)+ &
                EQCESE(Y,RECCL,B,D)+EQCRP90(Y,RECCL,B,D)+ &
                EQCSR90(Y,RECCL,B,D)+EQCADD(Y,RECCL,B,D)+ &
                EQCREP(Y,RECCL,B,D)+EQCSUR(Y,RECCL,B,D))
 180  CONTINUE
!*******************************************************************
      DO 185 Y=RECSYEAR,lastyr+baseyr-1
            Y1=Y-BASEYR+1
        IF (DEN(Y,1).GT.0.0) RSEEFFZ(Y1)=NUME(Y,1)/DEN(Y,1)
 185  CONTINUE

     DO 186 D=1,MNUMCR-2
      DO 186 B=1,MNUMBLDG
       DO 186 Y=RECSYEAR+1,lastyr+baseyr-1
            Y1=Y-BASEYR+1
       DO 186 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          EQC=RTCLEQCL(RECCL)
        IF (DEN1(Y,1,B,D).GT.0.0) THEN
          RSEEFDB1(Y1,RECCL,B,D)=NUME1(Y,1,B,D)/DEN1(Y,1,B,D)
        ELSE
          RSEEFDB1(Y1,RECCL,B,D)=RSEEFDB1(RECSYEAR-BASEYR+1,RECCL,B,D)
        ENDIF
 186  CONTINUE

      WRITE(9,*) 'END-USE ELECTRICITY PRICES'
      DO 190 EU=1,10
       WRITE(9,*) 'EU= ', eu,' ', EUPRNAMES(eu)
       DO 190 Y=RECSYEAR-BASEYR+1,lastyr
        WRITE(9,191) Y+(baseyr-1),(PELRSOUT(D,Y,EU),D=1,MNUMCR-2)
 190  CONTINUE
 191   FORMAT (I4,9(1X,F5.2))

       WRITE(9,*) 'AVERAGE ELECTRICITY PRICES'
       DO 193 Y=RECSYEAR-BASEYR+1,lastyr
        WRITE(9,191) Y+(baseyr-1),(PELRS(d,y),D=1,MNUMCR-2)
 193  CONTINUE

      END SUBROUTINE RESDRP2
!*******************************************************************


!*******************************************************************
!     INITIALIZE MARKET SHARES FOR HEATING EQUIPMENT from 2009 to last year of Census SOC (formerly C25) data in RSHTSHR.txt
!*******************************************************************
      SUBROUTINE INTEQT
      IMPLICIT NONE
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER  IUNIT1
      INTEGER B, E ,D, Y, EU, RECCL
!********************************************************************
!
          EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
      FNAME='RSHTSHR'
      NEW=.FALSE.
      IUNIT1=FILE_MGR('O',FNAME,NEW)
      READ(IUNIT1,'(19(/))')
     !HVAC historical data from Census SOC (formerly C25)
     ! Change this line each time Census data is updated  !SOC update
      DO 20 Y=recsyear,2015
       DO 20 D=1,MNUMCR-2
        DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
          READ(IUNIT1,*) (HSYSSHR(Y,RECCL,B,D),B=1,MNUMBLDG)
 20   CONTINUE
! 25   FORMAT(4X,3(F8.6,1X))
      IUNIT1=FILE_MGR('C',FNAME,NEW)
      END SUBROUTINE INTEQT

!*******************************************************************
!     NEW HOME HEATING SYSTEM REPORT
!*******************************************************************
      SUBROUTINE NHTSHR
      IMPLICIT NONE
      COMMON/TESTHT/HTYSSHR(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR)
      INTEGER Y, D, B, E, E2, EU, RECCL, EQC, NUMEQC
      REAL*4  NWEQHTSH(RECSYEAR:ENDYR,NHeatClasses),HTYSSHR &
             ,HEATCAL(RECSYEAR:ENDYR,NHeatClasses),TOTALSUM(RECSYEAR:ENDYR)
      CHARACTER*40 FN
!*******************************************************************
!     PRINT SUPLEMENTAL REPORT
!*******************************************************************
!
      EU = 1                      ! SPACE HEATING SECTION OF THE DATA
      NUMEQC=RTCLEUPT(EU+1)-RTCLEUPT(EU)
!
      DO 10 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 10 E=1,NUMEQC
               HEATCAL(Y,E)=0.0
          DO 10 B=1,MNUMBLDG
            DO 10 D=1,MNUMCR-2
               HEATCAL(Y,E)= HEATCAL(Y,E) + &
                       (HSYSSHR(Y,E,B,D)*HSEADD(Y,B,D) )
 10   CONTINUE
!
      DO 15 Y=RECSYEAR+1,lastyr+baseyr-1
               TOTALSUM(Y)=0.0
          DO 15 B=1,MNUMBLDG
            DO 15 D=1,MNUMCR-2
               TOTALSUM(Y)=TOTALSUM(Y)+HSEADD(Y,B,D)
15    CONTINUE
      DO 20 Y=RECSYEAR+1,lastyr+baseyr-1
        DO 20 E=1,NUMEQC
          IF (TOTALSUM(Y).GT.0.0) THEN
               NWEQHTSH(Y,E)=HEATCAL(Y,E) / TOTALSUM(Y)
          ELSE
               NWEQHTSH(Y,E)=0.0
          END IF
 20   CONTINUE
!
      WRITE(9,*) ' '
      WRITE(9,*) ' '
      WRITE(9,*)' Residential New Heating Report '
      WRITE(9,*)' New Home Heating System Shares (percent)'
      WRITE(9,*) ' '
      WRITE(9,6)(RTCLNAME(E),E=1,NUMEQC)
  6   FORMAT(6x,11(1X,A7))
      DO 200 Y=RECSYEAR+1,lastyr+baseyr-1
        WRITE(9,2) Y,(NWEQHTSH(Y,E)*100.,E=1,NUMEQC)
 200  CONTINUE
 2    FORMAT(1X,I4,11(1X,F7.2))
      END SUBROUTINE NHTSHR


!*******************************************************************
!     BENCHMARK CONSUMPTION TO SEDS / STEO
!*******************************************************************
      SUBROUTINE RSBENCH
      IMPLICIT NONE
      INTEGER ILASTSTEOYR ! allows the last STEO year parameter to be reset when turning off STEO benchmarking
      COMMON/STEO/STEOCN(recsyear+1:laststeoyravail,9,MNUMCR-2)
      REAL*4 STEOCN
      LOGICAL NEW
      CHARACTER*18 FNAME
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR
      INTEGER IUNIT1
      INTEGER Y,Y1,y4,D,F,B                                           ! STEOread-avg
      include'steoblock' ! common STEO inputs                         ! STEOread-avg

      Y=curiyr ! For brevity below

!   First year processing to Read rssteo file
      IF (CURCALYR.EQ.RECSYEAR) THEN
       FNAME='RSSTEO'
       NEW=.FALSE.
       IUNIT1=FILE_MGR('O',FNAME,NEW)
       READ(IUNIT1,'(19(/))')
        DO 50 F=1,5
          READ(IUNIT1,*)  !skip header
        DO 50 D=1,9
          READ(IUNIT1,*) (STEOCN(Y1,F,D),Y1=recsyear+1,laststeoyravail)
!          write(9,*) (STEOCN(Y1,F,D),Y1=recsyear+1,laststeoyravail)
 50    CONTINUE
       IUNIT1=FILE_MGR('C',FNAME,NEW)

!       write(9,'("rsbench,steobm,ilaststeoyr,laststeoyr,lastsedsyr,msedyr,GLOBALBENCHON,BENCHALLYRS")')
      END IF

!     If STEOBM == 0, then turn off STEO benchmarking by setting
!       the last STEO year to the MER year.
      ILASTSTEOYR=LASTSTEOYR
      IF (STEOBM==0) ILASTSTEOYR=LASTSEDSYR+1    !LASTSEDSYR=BASEYR-1+MSEDYR (declared in resdrep include file)

!      write(9,'("rsbench",6i5,e15.4)') steobm,ilaststeoyr,laststeoyr,lastsedsyr,msedyr,GLOBALBENCHON,BENCHALLYRS

! STEOread-avg
!  Replace RSSTEO inputs with values for STEO variables from common steoblock - consistent
!    with MER through latest historical year and convert to trillion Btu. STEO does not
!    have sector-level liquids variables/coal. MER values for these fuels still need to be input -
!    currently in rsteo.txt. Regional natural gas values in STEO may also be problematic.

        DO Y1= RECSYEAR+1, LASTSTEOYRAVAIL  ! Get MER/STEO data from common block ! STEOread-avg

      !Natural gas
        ! Use regional natural gas shares applied to national STEO total to calculate CD consumption
         STEOCN(Y1,1,1)= NGRCP_NEC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,2)= NGRCP_MAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,3)= NGRCP_ENC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,4)= NGRCP_WNC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,5)= NGRCP_SAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,6)= NGRCP_ESC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,7)= NGRCP_WSC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,8)= NGRCP_MTN(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.
         STEOCN(Y1,1,9)= NGRCP_PAC(y) / (NGRCP_NEC(y)+NGRCP_MAC(y)+NGRCP_ENC(y)+NGRCP_WNC(y)+NGRCP_SAC(y)+NGRCP_ESC(y)+NGRCP_WSC(y)+NGRCP_MTN(y)+NGRCP_PAC(y)) * NGRCBUS(y) * 1000.

      !Electricity - convert from bkWh to trills
        ! Get regional electricity values from STEO
         STEOCN(Y1,2,1)= EXRCP_NEC(y) * 3.412
         STEOCN(Y1,2,2)= EXRCP_MAC(y) * 3.412
         STEOCN(Y1,2,3)= EXRCP_ENC(y) * 3.412
         STEOCN(Y1,2,4)= EXRCP_WNC(y) * 3.412
         STEOCN(Y1,2,5)= EXRCP_SAC(y) * 3.412
         STEOCN(Y1,2,6)= EXRCP_ESC(y) * 3.412
         STEOCN(Y1,2,7)= EXRCP_WSC(y) * 3.412
         STEOCN(Y1,2,8)= EXRCP_MTN(y) * 3.412
         STEOCN(Y1,2,9)= (EXRCP_PAC(y)+EXRCP_HAK(y)) * 3.412

       END DO ! y ! STEOread-avg

!  BEFORE BENCHMARKING MAKE "HIGH LEVEL" ADJUSTMENTS TO TOTALS FOR NATURAL GAS AND ELECTRICITY
       DO D=1,MNUMCR-2
        !  DISTRIBUTED GENERATION
        !  ADD NATURAL GAS USAGE FOR FUEL CELLS AND DEDUCT SELF-GENERATED ELECTRICITY
        !  GASUSAGE(Y,D,2) IS FOR FUEL CELLS -- DG TECHNOLOGY NUMBER 2
         RSFLCN(Y,1,D)=RSFLCN(Y,1,D)+GASUSAGE(Y,D,2)-HWBTU(Y,D,2)
         ! DISTRIBUTED GENERATION, ELECTRICITY GENERATION OFFSETS FROM GRID PURCHASES
         RSFLCN(Y,2,D)=RSFLCN(Y,2,D) - &
           TRILLSOWNUSE(Y,D,1)-TRILLSOWNUSE(Y,D,2)-TRILLSOWNUSE(Y,D,3)
       ENDDO

! TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS
      IF (Y.LE.MSEDYR) THEN
         !SEDS Historical Benchmarking Years [Data from NEMS Global (QSBLK)]
          DO D=1,MNUMCR-2
           BNCHFCT(Y,1,D)=  QSNGRS(D,Y)-RSFLCN(Y,1,D)
           BNCHFCT(Y,2,D)=  QSELRS(D,Y)-RSFLCN(Y,2,D)
           BNCHFCT(Y,3,D)=  QSDSRS(D,Y)-RSFLCN(Y,3,D)
           BNCHFCT(Y,4,D)=  QSLGRS(D,Y)-RSFLCN(Y,4,D)
           BNCHFCT(Y,5,D)=  QSKSRS(D,Y)/RSFLCN(Y,5,D)
!           BNCHFCT(Y,6,D)=  QSCLRS(D,Y)/RSFLCN(Y,6,D)
           BNCHFCT(Y,6,D)=  0.0
          ENDDO

! Use Data from RSSTEO file instead of NEMS Global Data (for testing before NEMS is loaded)
! FOR HISTORICAL DATA TESTING ONLY, PRIOR TO UPDATE OF NEMS GDS TO MSEDYR = 2011
!          IF(curcalyr.GT.RECSYEAR) then
!          DO F=1,5
!           DO D=1,MNUMCR-2
!             IF (F.LE.4) THEN
!               BNCHFCT(Y,F,D)= STEOCN(CURCALYR,F,D)-RSFLCN(Y,F,D)
!             ELSE
!               BNCHFCT(Y,F,D)= STEOCN(CURCALYR,F,D)/RSFLCN(Y,F,D)
!             END IF
!           ENDDO
!          ENDDO
!          endif

         !STEO Benchmarking Years (includes MSEDYR+1 MER Historical Data)
        ELSE IF ((Y.GT.MSEDYR).AND.(CURCALYR.LE.ilaststeoyr)) THEN
          DO F=1,5
           DO D=1,MNUMCR-2
             IF (F.LE.4) THEN
               BNCHFCT(Y,F,D)= STEOCN(CURCALYR,F,D)-RSFLCN(Y,F,D)
             ELSE
               BNCHFCT(Y,F,D)= STEOCN(CURCALYR,F,D)/RSFLCN(Y,F,D)
             END IF
           ENDDO
          ENDDO

         !Post-STEO Period Factors
        ELSE IF (CURCALYR.gt.ilaststeoyr) THEN
          DO F=1,5
           DO D=1,MNUMCR-2
            IF (F.LE.2) THEN
             !Post-STEO period benchmarking option for gas and electricity
             ! Setting BENCHALLYRS=0. turns off (and assumes UECs were
             !  adjusted for SEDS/MER)
             ! Setting BENCHALLYRS=1. turns on and keeps benchmarking
             !  factor for the remainder of the projection years
              BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)*BENCHALLYRS                                ! STEOread-avg
              ! Temporarily freeze benchmarking to MER year, plus a little for electricity   !STEObenchEL
              IF (F.EQ.2) BNCHFCT(Y,F,D) = BNCHFCT(LASTSEDSYR+1-1989,F,D)*1.1*BENCHALLYRS    !STEObenchEL
            ELSE
             !Maintain STEO bench factors for other fuels
              BNCHFCT(Y,F,D)= BNCHFCTAVG(F,D)                                            ! STEOread-avg
            END IF
           END DO !D
          END DO !F
      END IF !CURCALYR (and Y)

!  TAKE AVERAGE OF LAST 5 YEARS OF BNCHFCT FOR FIRST YEAR AFTER HISTORICAL DATA          ! STEOread-avg
!   (OR THE NUMBER OF YEARS BETWEEN RECSYEAR AND MER YEAR)                               ! STEOread-avg
         IF (Y .EQ. MSEDYR+2) THEN  !MSED+2 is first year after MER                      ! STEOread-avg
          DO D=1,MNUMCR-2                                                                ! STEOread-avg

           BNCHFCTAVG(1,D)= 0.0                                                          ! STEOread-avg
           BNCHFCTAVG(2,D)= 0.0                                                          ! STEOread-avg
           BNCHFCTAVG(3,D)= 0.0                                                          ! STEOread-avg
           BNCHFCTAVG(4,D)= 0.0                                                          ! STEOread-avg
           BNCHFCTAVG(5,D)= 0.0                                                          ! STEOread-avg
           BNCHFCTAVG(6,D)= 0.0                                                          ! STEOread-avg

           DO y4= 1, MIN(MSEDYR-(RECSYEAR-BASEYR+1),5)                                   ! STEOread-avg
            BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) + BNCHFCT(Y-y4,1,D)                        ! STEOread-avg
            BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) + BNCHFCT(Y-y4,2,D)                        ! STEOread-avg
            BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) + BNCHFCT(Y-y4,3,D)                        ! STEOread-avg
            BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) + BNCHFCT(Y-y4,4,D)                        ! STEOread-avg
            BNCHFCTAVG(5,D)=  BNCHFCTAVG(5,D) + BNCHFCT(Y-y4,5,D)                        ! STEOread-avg
!            BNCHFCTAVG(Y,6,D)=  BNCHFCTAVG(6,D) + BNCHFCT(Y-y4,6,D)                      ! STEOread-avg
           END DO !y4                                                                    ! STEOread-avg

           BNCHFCTAVG(1,D)=  BNCHFCTAVG(1,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg
           BNCHFCTAVG(2,D)=  BNCHFCTAVG(2,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg
           BNCHFCTAVG(3,D)=  BNCHFCTAVG(3,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg
           BNCHFCTAVG(4,D)=  BNCHFCTAVG(4,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg
           BNCHFCTAVG(5,D)=  BNCHFCTAVG(5,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg
!           BNCHFCTAVG(6,D)=  BNCHFCTAVG(6,D) / MIN((MSEDYR+1)-(RECSYEAR-BASEYR+1),5)     ! STEOread-avg

          END DO !D                                                                      ! STEOread-avg
         END IF  !Y                                                                      ! STEOread-avg

! END TEST YEAR TO DETERMINE BENCHMARKING TREATMENTS ^^^^


!   Turn all post-RECS benchmarking off for testing purposes only
!     This option maintains only the RECS benchmarking factors by
!     setting all post-RECS years to those of the RECS year
      IF (GLOBALBENCHON==0 .and. curcalyr>recsyear) THEN
          DO D=1,MNUMCR-2
            DO F=1,5
             IF (F.LE.4) THEN
               BNCHFCT(Y,F,D)= BNCHFCT(Y-1,F,D)
              ELSE
               BNCHFCT(Y,F,D)= BNCHFCT(Y-1,F,D)
             ENDIF
!             write(9,'("rsNObench Y F D ",3i5,f7.1)') y,f,d,bnchfct(y,f,d)
            ENDDO
          ENDDO
       END IF

!   Print benchmarking for testing purposes
      IF (GLOBALBENCHON==1) THEN
          DO D=1,MNUMCR-2
            DO F=1,5
!              write(9,'("rsbenchfactors Y F D ",3i5,f12.2)') y,f,d,bnchfct(y,f,d)
            ENDDO
          ENDDO
       END IF

!     CALCULATE RSFLCN
       DO D=1,MNUMCR-2
         DO F=1,5
         IF (F.LE.4) THEN
           ! Additive benchmarking for Gas, Elec, Distillate & LPG
           RSFLCN(Y,F,D)=RSFLCN(Y,F,D)+BNCHFCT(Y,F,D)
          ELSE
           ! Scaling benchmarking for Kerosene ! but not Coal since it's no longer read in
           RSFLCN(Y,F,D)=RSFLCN(Y,F,D)*BNCHFCT(Y,F,D)
         END IF
         ENDDO !F
       ENDDO !D

! BENCHMARK NATURAL GAS, DISTILLATE, LPG & KEROSENE USING SPACE HEATING
!   Natural gas (F=1) SINGLE-FAMILY ONLY HTRCON(.,1,.)
       F=1
       B=1
       DO D=1,MNUMCR-2
        HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
!        HTRCONIN(Y,F,D,B)=(BNCHFCT(Y,F,d)*1000000.+HTRCONWT(Y,F,d,B))/htrconwt(y,1,d,B)*HTRCONIN(Y,1,d,B)
        HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
       END DO

! BENCHMARK DISTILLATE (F=3) AND LPG (F=4) USING SPACE HEATING (EXCEPT DIV 7)
! Revised for AEO2013
       B=1 !SINGLE-FAMILY
       DO D=1,9
!        IF (D.NE.7) THEN
          DO F=3,4
           HTRCON(Y,F,D)=HTRCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
           ! Don't allow weights to become negative (consumption could have also been capped, but is checked manually)
           If(HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000. .gt. 0. .and. htrconwt(y,f,d,b) .gt. 0.) then
!             HTRCONIN(Y,F,D,B)=(BNCHFCT(Y,F,D)*1000000.+HTRCONWT(Y,F,D,B))/HTRCONWT(Y,F,D,B)*HTRCONIN(Y,F,D,B)
             HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
            Else
             HTRCONIN(Y,F,D,B)= 0.
             HTRCONWT(Y,F,D,B)= 0.
           Endif
          END DO !F=3,4
!         Else  !D=7
!          ! BENCHMARK DISTILLATE USING secondary heating
!          F=3 !DISTILLATE
!          SHTCON(Y,F,D)=SHTCON(Y,F,D)+BNCHFCT(Y,F,D)*1000000.
!          If(shtconwt(y,f,d,b) .gt. 0.) &
!          SHTCONIN(Y,F,D,B)=(BNCHFCT(Y,F,D)*1000000.+SHTCONWT(Y,F,D,B))/SHTCONWT(Y,F,D,B)*SHTCONIN(Y,F,D,B)
!          SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)+BNCHFCT(Y,F,D)*1000000.
!          ! BENCHMARK LPG USING OTHER APPLIANCES (APLCON(.,2,.)==LPG 1=Gas, 3=Dist))
!          F=4 !LPG fuel index is F=4, but for appliances the index for APLCON is 2
!          APLCON(Y,2,D)=APLCON(Y,2,D)+BNCHFCT(Y,F,D)*1000000.
!          APLCONIN(Y,2,D,B)=(BNCHFCT(Y,F,D)*1000000.+APLCONWT(Y,2,D,B))/APLCONWT(Y,2,D,B)*APLCONIN(Y,2,D,B)
!          APLCONWT(Y,2,D,B)=APLCONWT(Y,2,D,B)+BNCHFCT(Y,F,D)*1000000.
!        END IF  !D=7
       END DO  !D=1,9
 115   CONTINUE

!    BENCHMARK KEROSENE AND COAL USING MAIN and SECONDARY HEATING
!      (benchmark all 3 building types)

       F=5
       DO D=1,9
         HTRCON(Y,F,D)=HTRCON(Y,F,D)*BNCHFCT(Y,F,D)
         SHTCON(Y,F,D)=SHTCON(Y,F,D)*BNCHFCT(Y,F,D)
        DO B=1,MNUMBLDG
!          HTRCONIN(Y,F,D,B)=HTRCONIN(Y,F,D,B)*BNCHFCT(Y,F,D)
          HTRCONWT(Y,F,D,B)=HTRCONWT(Y,F,D,B)*BNCHFCT(Y,F,D)
!          SHTCONIN(Y,F,D,B)=SHTCONIN(Y,F,D,B)*BNCHFCT(Y,F,D)
          SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)*BNCHFCT(Y,F,D)
        END DO !B
       END DO !D

       F=6  ! Coal, which is now zero
       DO D=1,MNUMCR-2
!         SHTCON(Y,F,D)=SHTCON(Y,F,D)*BNCHFCT(Y,F,D)
         SHTCON(Y,F,D)=0.0
        DO B=1,3
!         SHTCONIN(Y,F,D,B)=(BNCHFCT(Y,F,D)*1000000.+SHTCONWT(Y,F,D,B))/SHTCONWT(Y,F,D,B)*SHTCONIN(Y,F,D,B)
!         SHTCONWT(Y,F,D,B)=SHTCONWT(Y,F,D,B)*BNCHFCT(Y,F,D)
         SHTCONIN(Y,F,D,B)=0.0
         SHTCONWT(Y,F,D,B)=0.0
        END DO !B
       END DO !D

!   DEDUCT DISTRIBUTED GENERATION KWH FROM MISC ELECTRICITY
!    CONSUMPTION, EXCEPT FOR PV DEDUCT HALF FROM COOLING AND
!    HALF FROM MISC ELECTRICITY -- SINGLE FAMILY ONLY
!    ALSO CONVERT DG TRILLS TO MMBTU.
      DO D=1,MNUMCR-2
       B=1
        ! BENCHMARK ALL DIVISIONS USING OTHER APPLIANCES AND ADJUST DG
          !CAPTURE TOTAL COOLING LOAD BEFORE DEDUCTING PV GENERATION FOR USE BY EMM
          COOLCNwithDG(Y,1,D)=COOLCN(Y,1,D)                   !PVdispatch
          COOLCN(Y,1,D)=COOLCN(Y,1,D) &
             -0.35*TRILLSOWNUSE(Y,D,1)*1000000.    !PVshare
!          COOLCNIN(Y,1,D,B)=COOLCNWT(Y,1,D,B) &
!             /COOLCNWT(y,1,d,b)*COOLCNIN(Y,1,D,B)
          COOLCNWT(Y,1,D,B)=COOLCNWT(Y,1,D,B) &
             -0.35*TRILLSOWNUSE(Y,D,1)*1000000.    !PVshare
!      write(9,*) 'y,r,PurchasedCool,selfgen, TotalCool ',Y, D, COOLCN(y,1,d), 0.35*TRILLSOWNUSE(y,d,1)*1000000., COOLCNwithDG(Y,1,D) !PVdispatch    !PVshare
          APCON(Y,D)=APCON(Y,D)+BNCHFCT(Y,2,D)*1000000.  &
             -(0.65*TRILLSOWNUSE(Y,D,1)+TRILLSOWNUSE(Y,D,2)+ &    !PVshare
              TRILLSOWNUSE(Y,D,3))*1000000.0
          !ADD BACK PV GENERATION FOR TOTAL 'BENCHMARKED' MISC ELECTRICITY LOAD FOR USE BY EMM !PVdispatch
          APCONwithDG(Y,D)=APCON(Y,D)                    &    !PVdispatch
             +(0.65*TRILLSOWNUSE(Y,D,1))*1000000.0            !PVdispatch    !PVshare
!      write(9,*) 'y,r,PurchasedAP,selfgen, TotalAP ', Y, D, APCON(y,d), 0.65*TRILLSOWNUSE(y,d,1)*1000000., APCONwithDG(Y,D) !PVdispatch    !PVshare
          APCONWT(Y,D,B)=APCONWT(Y,D,B)+BNCHFCT(Y,2,D)*1000000.  &
             -(0.65*TRILLSOWNUSE(Y,D,1)+TRILLSOWNUSE(Y,D,2)+ &    !PVshare
             TRILLSOWNUSE(Y,D,3))*1000000.0
          EAEQCN(Y,1,B,D)=EAEQCN(Y,1,B,D)+BNCHFCT(Y,2,D)*1000000.&
             -(0.65*TRILLSOWNUSE(Y,D,1)+TRILLSOWNUSE(Y,D,2)+ &    !PVshare
              TRILLSOWNUSE(Y,D,3))*1000000.0
      END DO ! Census Divisions

!  ADJUST NATURAL GAS USAGE FOR DISTRIBUTED GENERATION (FUEL CELLS)
!   (ASSUMING FUEL CELLS PENETRATE WHERE GAS IS AVAILABLE USING WATER HEATING)
       DO D=1,MNUMCR-2
         H2OCON(Y,1,D)=H2OCON(Y,1,D) - &
           HWBTU(Y,D,2)*1000000.+(GASUSAGE(Y,D,2)+GASUSAGE(Y,D,3))*1000000.0
         DO B=1,MNUMBLDG
          IF(B.EQ.1) THEN
            H2OCONWT(Y,1,D,B)=H2OCONWT(Y,1,D,B) &
              -(HWBTU(Y,D,2)+GASUSAGE(Y,D,2))*1000000.0
            EQCEQCN(Y,19,B,D)=EQCEQCN(Y,19,B,D) &
              -(HWBTU(Y,D,2)+GASUSAGE(Y,D,2))*1000000.0
          ENDIF
         ENDDO  ! Building Types
       ENDDO ! Census Divisions

      END SUBROUTINE RSBENCH


!*******************************************************************
!     Residential Database File
!*******************************************************************
      SUBROUTINE RESDBOUT
      IMPLICIT NONE
!      INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables
!      PARAMETER (MaxApps=4)  !Maximum number of applications
!      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
!      PARAMETER (MaxBins=6)  !Maximum number of bulb bins within an application
      INTEGER app, bin
      REAL*4 temp

      !COMMON/DBEFFOUT/RSNEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2),RSEEFDB1(MNUMYR,MNUMRTTY,MNUMBLDG,MNUMCR-2)  !DYN
      !REAL*4 RSNEFDB1,RSEEFDB1
      REAL*4 bb,tt
      REAL*4 HEATERS(RECSYEAR:ENDYR,NHeatClasses,MNUMBLDG,MNUMCR-2)
      REAL*4 COOLERS(RECSYEAR:ENDYR,5,MNUMBLDG,MNUMCR-2)
      REAL*4 WATERS(RECSYEAR:ENDYR,5,MNUMBLDG,MNUMCR-2)
      REAL*4 COOKS(RECSYEAR:ENDYR,3,MNUMBLDG,MNUMCR-2)
      REAL*4 DRYERS(RECSYEAR:ENDYR,2,MNUMBLDG,MNUMCR-2)
      REAL*4 FRIGS(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      REAL*4 FREEZE(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      REAL*4 CLOTHE(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      REAL*4 DISHW(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)

      INTEGER W, Y, B, E, D, F, P, E2, Z, R,Y1
      INTEGER ff(6)
      INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S
      INTEGER EQCGHP,EQCEHP,RECCLGHP,RECCLEHP,EQCSWH,EQCEWH,RECCLSWH,RECCLEWH
      INTEGER FILE_MGR       ! FILE MANAGER
      INTEGER*4  OUTFILE     ! FILE HANDLE
      CHARACTER*18 FN,CNAME(MNUMRTCL),TNAME(MNUMRTTY),LTNAME(4)
      CHARACTER*18 REGION(12)
      CHARACTER*3 FL,HQ(12),CQ(6),WQ(5),KQ(3),DQ(2)
      CHARACTER*4 HQT(31),SR    !eu
      CHARACTER*8 EUNAME(9),SN(5)
      CHARACTER*8 ICNAME(3),TCNAME(3),FLNAME(2),RFNAME(3)
!*******************************************************************
      INTEGER DUM, zero
!      REAL*4 TH(RECSYEAR:ENDYR,3)
!      REAL*4 THD(RECSYEAR:ENDYR,MNUMBLDG,MNUMCR-2)
      CHARACTER*13 HTYPES(4)
            data ff/3,4,1,2,5,6/   ! pointers to benchmarking fuels from rtfuel
      DATA EUNAME/'HEAT','COOL','CWASH','DWASH','HOTWATER','COOK','DRYERS','FRIDG','FREEZE'/
      DATA HQ/'EFN','EHP','GFN','GOT','KER','LPG','DFN', &
      'DOT','WST','GE1','GHP','GE2'/
      DATA CQ/'RAC','CAC','EHP','GE1','GHP','GE2'/
      DATA WQ/'GAS','ELE','DIS','LPG','SOL'/
      DATA HQT/'EFN','EHP1','EHP2','EHP3','EHP4','GFN1','GFN2','GFN3','GFN4','GFN5','GOT1','GOT2','GOT3',&
       'KER1','KER2','KER3','LPG1','LPG2','LPG3','LPG4','LPG5','DFN1','DFN2','DFN3','DOT1','DOT2','DOT3','WST','GE1','GE2','NGHP'/
!
      DATA LTNAME/'LT-GS','LT-TORCH','LT-FLUOR','LT-REFL'/
      DATA KQ/'GAS','LPG','ELE'/
      DATA DQ/'GAS','ELE'/
      DATA SN/'NONMEC','MEC95','ESTAR','MEC+40','PATH'/
!
      DATA HTYPES/'Single Family',' Multi-Family',' Mobile Homes', &
                  '        Total'/
      DATA REGION / &
       'New England    ','Mid Atlantic    ','E North Central', &
       'W North Central','South Atlantic  ','E South Central', &
       'W South Central','Mountain        ','Pacific        ', &
       ' United States ','Less CA         ','United States  '/


!                  do 649 f=1,6
!                 do 649 d=1,MNUMCR-2
! 649       bnchfct(4,f,d)=1.0

      zero=0

 !      DO 650 Y=RECSYEAR,ENDYR
 !       DO 650 B=1,MNUMBLDG
 !         TH(Y,B)=0.0
 !         DO 650 D=1,MNUMCR-2
 !           TH(Y,B)=TH(Y,B)+ (EH(Y,B,D)+NH(Y,B,D))
 !           THD(Y,B,D)=EH(Y,B,D)+NH(Y,B,D)
 !650   CONTINUE

!*******************************************************************
!  AGGREGATE EQUIPMENT
!*******************************************************************
      DO 800 Y=RECSYEAR,ENDYR
        Z=Y
        DO 800 D=1,MNUMCR-2
!
          DO 800 B=1,MNUMBLDG
!
          EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYEAR) THEN
                HEATERS(Y,EQC,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
              ELSE
                HEATERS(Y,EQC,B,D)=EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ &
                  EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
                  EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)
              END IF
            ENDDO
!
          EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYEAR) THEN
                 COOLERS(Y,EQC,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
              ELSE
                COOLERS(Y,EQC,B,D)=EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ &
                  EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
                  EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)
              END IF
            ENDDO
!
          EU = 5             ! WATER HEATING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYEAR) THEN
                WATERS(Y,EQC,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
              ELSE
                WATERS(Y,EQC,B,D)=EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ &
                  EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
                  EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)
              END IF
            ENDDO
!
          EU = 6             ! COOKING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYEAR) THEN
                COOKS(Y,EQC,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
              ELSE
                COOKS(Y,EQC,B,D)=EQCESE(Y,RECCL,B,D)+ &
                  EQCRP90(Z,RECCL,B,D)+EQCSR90(Z,RECCL,B,D)+ &
                  EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
                  EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)
              END IF
            ENDDO
!
          EU = 7             ! CLOTHES DRYING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)
              IF (Z.EQ.RECSYEAR) THEN
                DRYERS(Y,EQC,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
              ELSE
                DRYERS(Y,EQC,B,D)= &
                  EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+ &
                  EQCSR90(Z,RECCL,B,D)+EQCADD(Z,RECCL,B,D)+ &
                  EQCREP(Z,RECCL,B,D)+EQCSUR(Z,RECCL,B,D)&
                  +EQCRP90RP(Z,RECCL,B,D)
             END IF
            ENDDO

          EU = 8             ! REFRIGERATION SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

            IF (Z.EQ.RECSYEAR) THEN
               FRIGS(Y,B,D)=EQCESE(RECSYEAR,RECCL,B,D)

            ELSE
               FRIGS(Y,B,D)= &
               EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+ &
               EQCSR90(Z,RECCL,B,D)+ &
               EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
               EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)
              END IF
            ENDDO


          EU = 9             !FREEZING SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

            IF (Z.EQ.RECSYEAR) THEN
               FREEZE(Y,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
            ELSE
               FREEZE(Y,B,D)= &
               EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+ &
               EQCSR90(Z,RECCL,B,D)+ &
               EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
               EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)

            END IF
            ENDDO
!
         EU = 3             !CLOTHES WASHER SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

            IF (Z.EQ.RECSYEAR) THEN
               CLOTHE(Y,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
            ELSE
               CLOTHE(Y,B,D)= &
               EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+ &
               EQCSR90(Z,RECCL,B,D)+ &
               EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
               EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)

            END IF
            ENDDO
!
         EU = 4             !DISH WASHER SECTION OF THE DATA
!
            DO RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
              EQC=RTCLEQCL(RECCL)

            IF (Z.EQ.RECSYEAR) THEN
               DISHW(Y,B,D)=EQCESE(RECSYEAR,RECCL,B,D)
            ELSE
               DISHW(Y,B,D)= &
               EQCESE(Y,RECCL,B,D)+EQCRP90(Z,RECCL,B,D)+ &
               EQCSR90(Z,RECCL,B,D)+ &
               EQCADD(Z,RECCL,B,D)+EQCREP(Z,RECCL,B,D)+ &
               EQCSUR(Z,RECCL,B,D)+EQCRP90RP(Z,RECCL,B,D)

            END IF
            ENDDO

 800  CONTINUE
!*******************************************************************
!  CALCULATE EQUIPMENT CONSUMPTION FOR RECSYEAR
!*******************************************************************
      DO 100 D=1,MNUMCR-2
        DO 100 B=1,MNUMBLDG
!
          EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
          DO 10 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
                                 EQCUEC(D,RECCL,B)
!
!   FIND THE RECORD NUMBERS FOR THE GEO AND ELECTRIC HEAT PUMPS
!     FOR HEATING
!
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP') RECCLEHP=RECCL
 10       CONTINUE
! GEO HP
          GEEQCN(RECSYEAR-BASEYR+1,1,B,D)=(EQCESE(RECSYEAR,RECCLGHP,B,D)*EQCUEC(D,RECCLGHP,B)*.2)*FossilHR/3412.
!
!
          EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
          DO 20 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
              EQCUEC(D,RECCL,B)
!
!   FIND THE EQ CLASS NUMBERS FOR THE GEO AND ELECTRIC HEAT PUMPS
!     FOR COOLING
!
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
              EQCGHP=RECCL
!              RECCLGHP=EQCGHP+RTCLEUPT(EU)
               RECCLGHP=reccl
            ENDIF
            IF(RTCLNAME(RECCL).EQ.'ELEC_HP') THEN
               EQCEHP=RECCL
!              RECCLEHP=EQCEHP+RTCLEUPT(EU)
               RECCLEHP=reccl
            ENDIF
 20       CONTINUE
! GEO HP
          GEEQCN(RECSYEAR-BASEYR+1,2,B,D)= (EQCESE(RECSYEAR,RECCLGHP,B,D)*EQCUEC(D,RECCLGHP,B)*.2)*FossilHR/3412.
        EU = 3             ! CLOTHES WASHER SECTION OF THE DATA

         DO 21 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 21         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
             EQCUEC(D,RECCL,B)
         EU = 4             ! DISHWASHER SECTION OF THE DATA

         DO 22 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 22         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
             EQCUEC(D,RECCL,B)

          EU = 5             ! WATER HEATING SECTION OF THE DATA

          DO 30 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 30         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
                                 EQCUEC(D,RECCL,B)


          EU = 6             ! STOVE (COOKING) SECTION OF THE DATA

          DO 40 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
            EQCEQCN(RECSYEAR-BASEYR+1,reccl,B,D)=EQCESE(RECSYEAR,RECCL,B,D)*EQCUEC(D,RECCL,B)
!            write(23,*) 'equipment=',eqcese(RECSYEAR,reccl,b,d),
!     1                  '   uec=   ',eqcuec(d,reccl,b)
 40       continue

!          write(23,*) 'total =',eqceqcn(RECSYEAR,eqc,b,d)


          EU = 7             ! CLOTHES DRYING SECTION OF THE DATA

          DO 50 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 50         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
              EQCUEC(D,RECCL,B)
!
         EU = 8             ! FOOD REFRIG SECTION OF THE DATA

         DO 52 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 52         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
             EQCUEC(D,RECCL,B)
         EU = 9             ! FOOD FREEZING SECTION OF THE DATA

         DO 55 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
!            EQC=RTCLEQCL(RECCL)
 55         EQCEQCN(RECSYEAR-BASEYR+1,RECCL,B,D)=EQCESE(RECSYEAR,RECCL,B,D)* &
             EQCUEC(D,RECCL,B)

         DO 70 E=1,3
 70         APEQCN(RECSYEAR-BASEYR+1,E,B,D)=APPEQP(RECSYEAR,B,D,E)*APPUEC(D,E,B)

          TVSEQCN(RECSYEAR-BASEYR+1,1,B,D)=TVSEQP(RECSYEAR,B,D)*TVSUEC(D,B)
          STBEQCN(RECSYEAR-BASEYR+1,1,B,D)=STBEQP(RECSYEAR,B,D)*STBUEC(D,B)
          HTSEQCN(RECSYEAR-BASEYR+1,1,B,D)=HTSEQP(RECSYEAR,B,D)*HTSUEC(D,B)
          DVDEQCN(RECSYEAR-BASEYR+1,1,B,D)=DVDEQP(RECSYEAR,B,D)*DVDUEC(D,B)
          VGCEQCN(RECSYEAR-BASEYR+1,1,B,D)=VGCEQP(RECSYEAR,B,D)*VGCUEC(D,B)
          DPCEQCN(RECSYEAR-BASEYR+1,1,B,D)=DPCEQP(RECSYEAR,B,D)*DPCUEC(D,B)
          LPCEQCN(RECSYEAR-BASEYR+1,1,B,D)=LPCEQP(RECSYEAR,B,D)*LPCUEC(D,B)
          MONEQCN(RECSYEAR-BASEYR+1,1,B,D)=MONEQP(RECSYEAR,B,D)*MONUEC(D,B)
          NETEQCN(RECSYEAR-BASEYR+1,1,B,D)=NETEQP(RECSYEAR,B,D)*NETUEC(D,B)
          BATEQCN(RECSYEAR-BASEYR+1,1,B,D)=BATEQP(RECSYEAR,B,D)*BATUEC(D,B)
          CFNEQCN(RECSYEAR-BASEYR+1,1,B,D)=CFNEQP(RECSYEAR,B,D)*CFNUEC(D,B)
          COFEQCN(RECSYEAR-BASEYR+1,1,B,D)=COFEQP(RECSYEAR,B,D)*COFUEC(D,B)
          DEHEQCN(RECSYEAR-BASEYR+1,1,B,D)=DEHEQP(RECSYEAR,B,D)*DEHUEC(D,B)
          MCOEQCN(RECSYEAR-BASEYR+1,1,B,D)=MCOEQP(RECSYEAR,B,D)*MCOUEC(D,B)
          PHPEQCN(RECSYEAR-BASEYR+1,1,B,D)=PHPEQP(RECSYEAR,B,D)*PHPUEC(D,B)
          SECEQCN(RECSYEAR-BASEYR+1,1,B,D)=SECEQP(RECSYEAR,B,D)*SECUEC(D,B)
          SPAEQCN(RECSYEAR-BASEYR+1,1,B,D)=SPAEQP(RECSYEAR,B,D)*SPAUEC(D,B)
          WCLEQCN(RECSYEAR-BASEYR+1,1,B,D)=WCLEQP(RECSYEAR,B,D)*WCLUEC(D,B)    !winecool

 100  CONTINUE
 984  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I3,',',A5)    !eu
 985  FORMAT(A7,A5,A5,A5,A9,A5,A8,A12,A11,A8)
 986  FORMAT(A7,A5,A5,A5,A9,A8,A5,A7,A6,A12,A12,4A10,A12,A13,A11,A13,A9,10a13)
 987  FORMAT(A8,',',I4,',',I4,',',A2,',',A8,',',A8,',',I4,2(',',F9.4),21(',',I11))
 988  FORMAT(1X,A8,1X,I4,1X,I4,1X,A2,1X,A8,1X,A8,1X,I4,2(1X,I11))
 989  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',A5)    !eu
 990  FORMAT(A8,',',I4,',',I4,',',A2,',',A8,',',A8,',',I4,2(',',F9.4),9(',',f11.0))
 991  FORMAT(A4,',',I2,',',I2,',',A2,',',A8,',',I4,',',F12.0,',',I13,',',I13)    !eu
 992  FORMAT(A4, ',', 4(I2,','), I4, ',', F12.8, ',', I2, ',', I2)
 999  FORMAT(1X,A2,1X,I4,1X,A2,1X,A3,1X,I4,6(1X,F10.3))
 998  FORMAT(A4,',',I2,',',A2,',',A2,',',A8,',',I4,',',F12.2,',',I13)    !eu
!
!--------------------------------------------------------------
!   OPEN FILE AND WRITE DATA
      OUTFILE=FILE_MGR('O','RESDEQP',.TRUE.) !OPEN THE output file
      WRITE(OUTFILE,986) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','EQPTYPE,','YEAR,','NEWEFF,','EXEFF,','NEWPURCHASE,','REPPURCHASE,',&
      'NEWINVEST,','REPINVEST,','NEWSUB,','REPSUB,', &
      'NON_IECC09,','IECC2009,','ESTAR,','IECC+40%,','BEST,', &
      'I_NON_IECC09,','I_IECC2009,','I_ESTAR,','I_IECC+40%,','I_BEST,', &
      'S_NON_IECC09,','S_IECC2009,','S_ESTAR,','S_IECC+40%,','S_BEST'

      DO 150 D=1,MNUMCR-2
       DO 150 B=1,MNUMBLDG
!       DO 150 Y=RECSYEAR+1,ENDYR
        DO 150 Y=RECSYEAR+1,lastyr+baseyr-1
          DO 140 EU = 1,9
          SR=EUNAME(EU)
          TYPE = RTTYPECT(EU)
          DO 140 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
          IF((Y.GE.RTINITYR(RECTY).AND. &
              Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN

!
              TYPE = TYPE + 1
              EQTYPE=RTEQTYPE(RECTY)
              EQC=RTTYEQCL(RECTY)
              TNAME=RTTYNAME(RECTY)
              RECCL=RTCLEUPT(EU)+EQC
              CNAME=RTCLNAME(RECCL)
              F    =RTFUEL(RECCL)
              E=RTCLEQCL(RECCL)
              Y1=Y-BASEYR+1
!
!   FIND THE RECORD NUMBER FOR THE GEO HEAT PUMP FOR HEATING
!
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL
!
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSE IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
              FL='SO'
            ENDIF

            IF(EU.eq.1) THEN !Calculate shell investment and subsidies with heating system records only
              WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)),INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)),&
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty))+FLOAT(EPA111D)*RTRESUB111D(recty)),   & !investment excludes installation costs, so use rtresub for investment only
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   &
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresub(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))), INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsub(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))),&
              (INT(SHELLBUILDS(Y,EQTYPE,S,B,D)),S=1,NShellTypes), &
              (INT((shellinvest (y,reccl,s,b,d)+shellsubsidy (y,reccl,s,b,d))*ifix(shellbuilds(Y,EQTYPE,S,B,D))),S=1,NShellTypes), &  !add the subsidy back in for investment includes 111D subsidies
              (INT(shellsubsidy(y,reccl,s,b,d)*ifix(shellbuilds(Y,EQTYPE,S,B,D))),S=1,NShellTypes)
             ELSE  !other equipment
              WRITE(OUTFILE,987) SR,D,B,FL,RTCLNAME(RECCL),RTTYNAME(RECTY),Y , RSNEFDB1(Y1,RECCL,B,D), RSEEFDB1(Y1,RECCL,B,D),&
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)),INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)),&
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtrecost(recty)+rtresub(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !add back in the subsidy, which was taken out earlier ((..1) is new construction (..2) replacements)
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rtrecost(recty)+rtresub(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),   & !account for capital investment only, not installation + capital (rteqcost + rteqsub)
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,1)*(rtresub(recty)+FLOAT(EPA111D)*RTRESUB111D(recty))),&                      !for subsidies, account for all components - here new construction, just capital subsidies
              INT(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*(rteqsub(recty)+FLOAT(EPA111D)*RTEQSUB111D(recty))),&                      !for replacement purchases, include rteqsub which includes subsidies for total installed costs (including labor)
              0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0
            ENDIF
          ENDIF !Year and Census division filter
 140     CONTINUE
 150  CONTINUE

       DO 1151 app=1,NumApps
       DO 1151 D=1,MNUMCR-2
       DO 1151 B=1,MNUMBLDG
       DO 1151 E=1,numtypes(app)
       DO 1151 Y=RECSYEAR+1,lastyr+(baseyr-1)
               WRITE(OUTFILE,990) 'LIGHTING',D,B,'EL',AppID(app),appbulbname(app,e),Y,WTLEFFbyAPP(app,Y,B,D),WTLEFFbyAPP(app,Y,B,D),(LTNEEDEDbyAPP(app,Y,E,B,D)), &
                                                      (LTREPbyAPP(app,Y,E,B,D)),LTINVEST(app,Y,E,B,D,1),LTINVEST(app,Y,E,B,D,2),LTsubsidy(app,Y,E,B,D,1),LTsubsidy(app,Y,E,B,D,2),0,0,0
 1151  CONTINUE

      OUTFILE=FILE_MGR('C','RESDEQP',.FALSE.) !Close the output file
!*******************************************************************
      FN='RESDBOUT.TXT'
      OPEN(23,FILE=FN,FORM='FORMATTED')
!*******************************************************************
!*******************************************************************
!  HEATING EQUIPMENT
!*******************************************************************
!
          EU = 1             ! SPACE HEATING SECTION OF THE DATA
!
      SR='HT'
      WRITE(23,985) 'ENDUSE,','CDIV,','BLDG,','FUEL,','EQPCLASS,','YEAR,','EQSTOCK,','CONSUMPTION,','HOUSEHOLDS,','BULBTYPE'
      DO 151 D=1,MNUMCR-2
       DO 151 B=1,MNUMBLDG
         DO 141 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!
!   FIND THE RECORD NUMBER FOR THE GEO HEAT PUMP FOR HEATING
!
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') RECCLGHP=RECCL
!
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSE IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

            DO 141 Y=RECSYEAR,lastyr+(baseyr-1)  ! Write DB to lastyr only
             Y1=Y-BASEYR+1
             BB=0.
             IF (RECCL.EQ.1) Then ! writes number of households when processing electric space heating
              WRITE(23,991) SR,D,B,FL,RTCLNAME(RECCL),Y, &
              HEATERS(Y,E,B,D),INT(EQCEQCN(Y1,RECCL,B,D)),INT(EH(Y,B,D)+NH(Y,B,D))
             else
              IF ((RECCL.EQ.3).AND.(B.EQ.1)) BB=BNCHFCT(Y1,1,D)*1000000.
              IF ((RECCL.EQ.6).AND.(B.EQ.1)) BB=BNCHFCT(Y1,4,D)*1000000.
              IF ((RECCL.EQ.7).AND.(B.EQ.1)) BB=BNCHFCT(Y1,3,D)*1000000.
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
                  HEATERS(Y,E,B,D),INT(bb+EQCEQCN(Y1,RECCL,B,D))
              If ((RECCL.EQ.5).AND.(B.EQ.1)) then
               BB=BNCHFCT(Y1,5,D)
               WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
                  HEATERS(Y,E,B,D),INT(bb*EQCEQCN(Y1,RECCL,B,D))
              Endif
             Endif
 141     CONTINUE

         FL='GE'
         DO Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
            WRITE(23,989) SR,D,B,FL,HQ(12),Y, &
            HEATERS(Y,RECCLGHP,B,D),INT(GEEQCN(Y1,1,B,D))
         ENDDO
 151  CONTINUE
!*******************************************************************
!  COOLING EQUIPMENT
!*******************************************************************
!
          EU = 2             ! SPACE COOLING SECTION OF THE DATA
!
      SR='CL'
      DO 180 D=1,MNUMCR-2
       DO 180 B=1,MNUMBLDG
        DO 170 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!
!   FIND THE EQ CLASS NUMBER FOR THE GEO HEAT PUMP FOR COOLING
!
            IF(RTCLNAME(RECCL).EQ.'GEO_HP') THEN
              EQCGHP=RTCLEQCL(RECCL)
              RECCLGHP=EQCGHP+RTCLEUPT(EU)
            ENDIF
!
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
!
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF


!     DO 170 Y=RECSYEAR,ENDYR
      DO 170 Y=RECSYEAR,lastyr+(baseyr-1)  ! Write DB to lastyr only
            Y1=Y-BASEYR+1
             IF ((D.GT.1).AND.(B.EQ.1).AND.(E.EQ.2)) THEN
!              bb=bnchfct(y1,2,d)*1000000.
            else
              bb=0.0
            end if
          IF ((B.EQ.1).and.(e.eq.2)) then
              tt=0.5*trillsownuse(y1,d,1)*1000000.
            else
              tt=0.0
           END IF

              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
              COOLERS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D)+bb-tt)
 170     CONTINUE
         FL='GE'
         DO Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,CQ(6),Y, &
            COOLERS(Y,EQCGHP,B,D),INT(GEEQCN(Y1,2,B,D))
         ENDDO
 180  CONTINUE
!*******************************************************************
!  CLOTHES WASHERS
!*******************************************************************
!
      EU = 3             ! CLOTHES WASHERS SECTION OF THE DATA

      SR='CW'
      FL='EL'
      DO 171 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 171 D=1,MNUMCR-2
       DO 171 B=1,MNUMBLDG
         DO 171 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
               WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           CLOTHE(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
171  CONTINUE
!*******************************************************************
!  DISH WASHERS
!*******************************************************************
      EU = 4             ! FOOD FREEZING SECTION OF THE DATA
      SR='DW'
      FL='EL'
      DO 172 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 172 D=1,MNUMCR-2
       DO 172 B=1,MNUMBLDG
!        DO 172 Y=RECSYEAR,ENDYR
         DO 172 Y=RECSYEAR,lastyr+(baseyr-1)  ! Write DB to lastyr only
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           DISHW(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 172  CONTINUE
!
!*******************************************************************
!  WATER HEATING EQUIPMENT
!*******************************************************************
!
          EU = 5             ! WATER HEATING SECTION OF THE DATA
!
      SR='HW'
      DO 200 D=1,MNUMCR-2
       DO 200 B=1,MNUMBLDG
       DO 190 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='SL'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
            DO 190 Y=RECSYEAR,lastyr+(baseyr-1)
             Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
              WATERS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 190     CONTINUE
         FL='SL'
         DUM=0.0
         DO Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,WQ(5),Y, &
            WATERS(Y,5,B,D),INT(SLEQCN(Y1,1,B,D))
         ENDDO
 200     CONTINUE
!*******************************************************************
!  COOKING EQUIPMENT
!*******************************************************************
!
          EU = 6             ! STOVES (COOK) SECTION OF THE DATA
!
      SR='CK'
      DO 220 D=1,MNUMCR-2
       DO 220 B=1,MNUMBLDG
        DO 220 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
          DO 220 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
               WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
             COOKS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 220  CONTINUE
!*******************************************************************
!  CLOTHES DRYERS
!*******************************************************************
!
          EU = 7             ! CLOTHES DRY SECTION OF THE DATA
!
      SR='DR'
      DO 240 D=1,MNUMCR-2
       DO 240 B=1,MNUMBLDG
        DO 240 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
            IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ENDIF
          DO 240 Y=RECSYEAR,lastyr+(baseyr-1)
             Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
             DRYERS(Y,E,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 240  CONTINUE
!*******************************************************************
!  REFRIGERATORS
!*******************************************************************
      EU = 8             ! FOOD REFRIGERATION SECTION OF THE DATA
      SR='RF'
      FL='EL'
      DO 260 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 260 D=1,MNUMCR-2
       DO 260 B=1,MNUMBLDG
        DO 260 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           FRIGS(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 260  CONTINUE
!*******************************************************************
!  FREEZERS
!*******************************************************************
      EU = 9             ! FOOD FREEZING SECTION OF THE DATA
!
      SR='FZ'
      FL='EL'
      DO 280 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
      DO 280 D=1,MNUMCR-2
       DO 280 B=1,MNUMBLDG
         DO 280 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y, &
           FREEZE(Y,B,D),int(EQCEQCN(Y1,RECCL,B,D))
 280  CONTINUE
!*******************************************************************
!  LIGHTING - All Applications
!*******************************************************************
      SR='LT'
      FL='EL'

      Do 301 app =1,NumApps
        !adding this loop across types splits lighting applications into bulb types for stocks
        ! also need to accumulate into temp below to split by types
        Do 301 type =1,numtypes(app)
       DO 301 D=1,MNUMCR-2
        DO 301 B=1,MNUMBLDG
         DO 301 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
                  TEMP=0.
                  DO BIN=1,MAXBINS
                   TEMP=TEMP+ltstock(app,y,type,B,D,BIN)
                  ENDDO
              if(type.eq.1) then
               WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,int(LTEQCN(Y1,app,B,D)),0,appbulbname(app,type)
              else
               WRITE(23,984) SR,D,B,FL,appid(app),Y,TEMP,0,0,appbulbname(app,type)
                    endif
 301  CONTINUE

!*******************************************************************
!  FURNACE FANS
!*******************************************************************
      SR='FF'
      FL='EL'
       DO 302 D=1,MNUMCR-2
        DO 302 B=1,MNUMBLDG
         DO 302 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,SR,Y, &
           FANEQP(Y,B,D), &
            int(FANEQCN(Y1,1,B,D))
 302  CONTINUE


!*******************************************************************
!  Televisions
!*******************************************************************
      SR='TVS'
      FL='EL'
       DO 305 D=1,MNUMCR-2
        DO 305 B=1,MNUMBLDG
         DO 305 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'TV&R',Y,TVSEQP(Y,B,D), &
              INT(TVSEQCN(Y1,1,B,D))
 305  CONTINUE
!*******************************************************************
!  Set-Top Boxes
!*******************************************************************
      SR='STB'
      FL='EL'
       DO 306 D=1,MNUMCR-2
        DO 306 B=1,MNUMBLDG
         DO 306 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'TV&R',Y,STBEQP(Y,B,D), &
              INT(STBEQCN(Y1,1,B,D))
 306  CONTINUE
!*******************************************************************
!  Home Theater Systems
!*******************************************************************
      SR='HTS'
      FL='EL'
       DO 307 D=1,MNUMCR-2
        DO 307 B=1,MNUMBLDG
         DO 307 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'TV&R',Y,HTSEQP(Y,B,D), &
              INT(HTSEQCN(Y1,1,B,D))
 307  CONTINUE
!*******************************************************************
!  DVD Players
!*******************************************************************
      SR='DVD'
      FL='EL'
       DO 308 D=1,MNUMCR-2
        DO 308 B=1,MNUMBLDG
         DO 308 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'TV&R',Y,DVDEQP(Y,B,D), &
              INT(DVDEQCN(Y1,1,B,D))
 308  CONTINUE
!*******************************************************************
!  Video Game Consoles
!*******************************************************************
      SR='VGC'
      FL='EL'
       DO 309 D=1,MNUMCR-2
        DO 309 B=1,MNUMBLDG
         DO 309 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'TV&R',Y,VGCEQP(Y,B,D), &
              INT(VGCEQCN(Y1,1,B,D))
 309  CONTINUE


!*******************************************************************
!  Desktop PCs
!*******************************************************************
      SR='DPC'
      FL='EL'
       DO 312 D=1,MNUMCR-2
        DO 312 B=1,MNUMBLDG
         DO 312 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'PC&R',Y,DPCEQP(Y,B,D), &
              INT(DPCEQCN(Y1,1,B,D))
 312  CONTINUE
!*******************************************************************
!  Laptop PCs
!*******************************************************************
      SR='LPC'
      FL='EL'
       DO 313 D=1,MNUMCR-2
        DO 313 B=1,MNUMBLDG
         DO 313 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'PC&R',Y,LPCEQP(Y,B,D), &
              INT(LPCEQCN(Y1,1,B,D))
 313  CONTINUE
!*******************************************************************
!  Monitors
!*******************************************************************
      SR='MON'
      FL='EL'
       DO 314 D=1,MNUMCR-2
        DO 314 B=1,MNUMBLDG
         DO 314 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'PC&R',Y,MONEQP(Y,B,D),&
              INT(MONEQCN(Y1,1,B,D))
 314  CONTINUE

!*******************************************************************
!  Networking Equipment
!*******************************************************************
      SR='NET'
      FL='EL'
      DO 315 D=1,MNUMCR-2
       DO 315 B=1,MNUMBLDG
         DO 315 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
              WRITE(23,989) SR,D,B,FL,'PC&R',Y,NETEQP(Y,B,D),&
              INT(NETEQCN(Y1,1,B,D))
 315  CONTINUE


!*******************************************************************
!  RECHARGEABLES
!*******************************************************************
      SR='BAT'
      FL='EL'
      DO 320 D=1,MNUMCR-2
       DO 320 B=1,MNUMBLDG
        DO 320 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, BATEQP(Y,B,D),&
           INT(BATEQCN(Y1,1,B,D))
 320  CONTINUE
!*******************************************************************
!  CEILING FANS
!*******************************************************************
      SR='CFN'
      FL='EL'
      DO 321 D=1,MNUMCR-2
       DO 321 B=1,MNUMBLDG
        DO 321 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, CFNEQP(Y,B,D),&
           INT(CFNEQCN(Y1,1,B,D))
 321  CONTINUE
!*******************************************************************
!  COFFEE MAKERS
!*******************************************************************
      SR='COF'
      FL='EL'
      DO 322 D=1,MNUMCR-2
       DO 322 B=1,MNUMBLDG
         DO 322 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, COFEQP(Y,B,D),&
           INT(COFEQCN(Y1,1,B,D))
 322  CONTINUE
!*******************************************************************
!  DEHUMIDIFIERS
!*******************************************************************
      SR='DEH'
      FL='EL'
      DO 323 D=1,MNUMCR-2
       DO 323 B=1,MNUMBLDG
         DO 323 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, DEHEQP(Y,B,D),&
           INT(DEHEQCN(Y1,1,B,D))
 323  CONTINUE
!*******************************************************************
!  MICROWAVES
!*******************************************************************
      SR='MCO'
      FL='EL'
      DO 324 D=1,MNUMCR-2
       DO 324 B=1,MNUMBLDG
         DO 324 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, MCOEQP(Y,B,D),&
           INT(MCOEQCN(Y1,1,B,D))
 324  CONTINUE
!*******************************************************************
!  POOL HEATERS AND PUMPS
!*******************************************************************
      SR='PHP'
      FL='EL'
      DO 325 D=1,MNUMCR-2
       DO 325 B=1,MNUMBLDG
         DO 325 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, PHPEQP(Y,B,D),&
           INT(PHPEQCN(Y1,1,B,D))
 325  CONTINUE
!*******************************************************************
!  SECURITY SYSTEMS
!*******************************************************************
      SR='SEC'
      FL='EL'
      DO 326 D=1,MNUMCR-2
       DO 326 B=1,MNUMBLDG
         DO 326 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, SECEQP(Y,B,D),&
           INT(SECEQCN(Y1,1,B,D))
 326  CONTINUE
!*******************************************************************
!  SPAS
!*******************************************************************
      SR='SPA'
      FL='EL'
      DO 327 D=1,MNUMCR-2
       DO 327 B=1,MNUMBLDG
         DO 327 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, SPAEQP(Y,B,D),&
           INT(SPAEQCN(Y1,1,B,D))
 327  CONTINUE
!*******************************************************************
!  WINE COOLERS
!*******************************************************************
      SR='WCL'
      FL='EL'
      DO 328 D=1,MNUMCR-2
       DO 328 B=1,MNUMBLDG
         DO 328 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
           WRITE(23,989) SR,D,B,FL,'MEL',Y, WCLEQP(Y,B,D),&
           INT(WCLEQCN(Y1,1,B,D))
 328  CONTINUE

!*******************************************************************
!  OTHER ELECTRIC APPLIANCES NOT EXPLICITLY MODELED
!*******************************************************************
      SR='EO'
      FL='EL'
      DO 331 D=1,MNUMCR-2
       DO 331 B=1,MNUMBLDG
         DO 331 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
!           IF (B.EQ.1) THEN
!            IF (D.EQ.1) then
!              bb=bnchfct(y1,2,d)*1000000.
!            else
              bb=0.0
!           end if
             tt=0.0
!           end if
           WRITE(23,989) SR,D,B,FL,'MEL',Y, EAEQP(Y,B,D),&
       INT(EAEQCN(Y1,1,B,D))
 331  CONTINUE
!*******************************************************************
!  OTHER APPLIANCES IN FUELS OTHER THAN ELECTRICITY
!*******************************************************************
      SR='OA'
      DO 340 D=1,MNUMCR-2
       DO 340 B=1,MNUMBLDG
        DO 340 E=1,3
          DO 340 Y=RECSYEAR,lastyr+(baseyr-1)
             Y1=Y-BASEYR+1
          IF (E .EQ. 1) THEN
            FL='GS'
            f = 1
           bb = 0.0
          ELSE IF (E .EQ. 2) THEN
            FL='LG'
            f = 4
!           IF ((D.EQ.7).AND.(B.EQ.1)) THEN
!              bb = bnchfct(y1,4,d)*1000000.
!           ELSE
              BB = 0.0
!           END IF
          ELSE IF (E .EQ. 3) THEN
            FL='DS'
            f = 3
            bb= 0.0
          ENDIF
            WRITE(23,989) SR,D,B,FL,FL,Y,APLEQP(Y,B,D,E), &
              int(bb+APEQCN(Y1,E,B,D))
 340  CONTINUE
!*******************************************************************
!  SECONDARY HEATING
!*******************************************************************
      SR='SH'
      DO 360 D=1,MNUMCR-2
       DO 360 B=1,MNUMBLDG
        DO 360 E=1,3
            DO 360 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
          IF (E .EQ. 1) THEN
            FL='GS'
            bb=0.0 ! bnchfct(y1,e,d)
          ELSE IF (E .EQ. 2) THEN
            FL='EL'
            bb=0.0 ! bnchfct(y1,e,d)
          ELSE IF (E .EQ. 3) THEN
            FL='DS'
!           IF ((D.EQ.7).AND.(B.EQ.1)) THEN
!            bb= bnchfct(y1,e,d)*1000000.
!           ELSE
            bb=0.0
!           END IF
         end if
          WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),&
              int(bb+sheQCN(Y1,E,B,D))

 360  CONTINUE
      DO 361 D=1,MNUMCR-2
       DO 361 B=1,MNUMBLDG
        DO 361 E=4,7
            DO 361 Y=RECSYEAR,lastyr+(baseyr-1)
            Y1=Y-BASEYR+1
          IF (E .EQ. 4) THEN
            FL='LG'
            bb=1.0 ! bnchfct(y1,e,d)
          ELSE IF (E .EQ. 5) THEN
            FL='KS'
             bb=bnchfct(y1,e,d)
          ELSE IF (E .EQ. 6) THEN
            FL='CL'
             bb=bnchfct(y1,e,d)
          ELSE IF (E .EQ. 7) THEN
            FL='WD'
            bb=1.0
          ENDIF


          WRITE(23,989) SR,D,B,FL,FL,Y,SHTEQP(Y,B,D,E),&
              int(bb*sheQCN(Y1,E,B,D))

 361  CONTINUE
!**********************************************************************
!      WRITE OUT HOUSING STARTS
!**********************************************************************
        SR='HS'
        DO 370 D=1,MNUMCR-2
         DO 370 B=1,MNUMBLDG
          DO 370 Y=RECSYEAR,lastyr+(baseyr-1)
          WRITE(23,989) SR,D,B,'0','0',Y,HSEADD(Y,B,D),zero
 370   CONTINUE
!**********************************************************************
!      WRITE OUT SQUAREFOOTAGE
!**********************************************************************
! adjust database output
        SR='SQ'
        DO 375 D=1,MNUMCR-2
         DO 375 B=1,MNUMBLDG
          DO 375 Y=RECSYEAR,lastyr+(baseyr-1)
          WRITE(23,991) SR,D,B,'0','0',Y,STOCKSQRFOOT(Y,B,D),INT(SQRFOOT(Y,B,D)), &
            INT(((EH(Y,B,D)+NH(Y,B,D))*STOCKSQRFOOT(Y,B,D))/10**6)
 375   CONTINUE
!**********************************************************************
!      WRITE OUT FUEL SWITCHING TO TECHNOLOGIES
!**********************************************************************
        EU = 1
        SR='ST'
        DO 380 D=1,MNUMCR-2
         DO 380 B=1,1
          DO 380 Y=RECSYEAR,lastyr+(baseyr-1)
           DO 380 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSE IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

          WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWTOTAL(Y,E,D),zero
 380   CONTINUE
!**********************************************************************
!      WRITE OUT FUEL SWITCHING FROM TECHNOLOGIES
!**********************************************************************
        EU = 1
        SR='SF'
        DO 390 D=1,MNUMCR-2
         DO 390 B=1,1
          DO 390 Y=RECSYEAR,lastyr+(baseyr-1)
           DO 390 RECCL=RTCLEUPT(EU)+1,RTCLEUPT(EU+1)
            E=RTCLEQCL(RECCL)
!   DEFINE THE NAMES CORRESPONDING TO FUEL NUMBERS
            IF (RTCLNAME(RECCL).EQ.'WOOD_HT') THEN
              FL='WD'
            ELSE IF (RTFUEL(RECCL).EQ.4) THEN
              FL='EL'
            ELSE IF (RTFUEL(RECCL).EQ.3) THEN
              FL='GS'
            ELSE IF (RTFUEL(RECCL).EQ.5) THEN
              FL='KS'
            ELSE IF (RTFUEL(RECCL).EQ.2) THEN
              FL='LG'
            ELSE IF (RTFUEL(RECCL).EQ.1) THEN
              FL='DS'
            ELSE
            ENDIF

          WRITE(23,989) SR,D,B,FL,RTCLNAME(RECCL),Y,SWFTOTAL(Y,E,D),zero
 390   CONTINUE
!*******************************************************************
!  WRITE OUT FUEL PRICES
!*******************************************************************
! adjust database output
!      WRITE NEMS PRICES IN INTERNAL NEMS DOLLAR YEAR (1987$/MMBTU)
      SR='FP'
      DO 400 D=1,MNUMCR
!      DO 400 Y=1,MNUMYR
      DO 400 Y=1,lastyr  !Only write results to lastyr
          WRITE(23,998) SR,D,'1','DS','0',Y+(baseyr-1),PDSRS(D,Y),zero
          WRITE(23,998) SR,D,'1','LG','0',Y+(baseyr-1),PLGRS(D,Y),zero
          WRITE(23,998) SR,D,'1','NG','0',Y+(baseyr-1),PNGRS(D,Y),zero
          WRITE(23,998) SR,D,'1','EL','0',Y+(baseyr-1),PELRS(D,Y),zero
          WRITE(23,998) SR,D,'1','KS','0',Y+(baseyr-1),PKSRS(D,Y),zero
          WRITE(23,998) SR,D,'1','CL','0',Y+(baseyr-1),PCLRS(D,Y),zero
 400  CONTINUE
!*******************************************************************
!  WRITE OUT RESIDENTIAL SHELL INDICES
!*******************************************************************
!      Write shell heating indices by year
        DO 410 Y=RECSYEAR-(baseyr-1),lastyr
         WRITE(23,992) 'HSHE',11,1,0,0,Y+(baseyr-1),HSHELL1(Y),0,0    !existing
         WRITE(23,992) 'HSHN',11,1,0,0,Y+(baseyr-1),HSHELL2(Y),0,0    !new construction
         WRITE(23,992) 'HSHA',11,1,0,0,Y+(baseyr-1),HSHELL3(Y),0,0    !average
 410  CONTINUE

!      Write shell cooling indices by year
        DO 420 Y=RECSYEAR-(baseyr-1),lastyr
         WRITE(23,992) 'CSHE',11,1,0,0,Y+(baseyr-1),CSHELL1(Y),0,0    !existing
         WRITE(23,992) 'CSHN',11,1,0,0,Y+(baseyr-1),CSHELL2(Y),0,0    !new construction
         WRITE(23,992) 'CSHA',11,1,0,0,Y+(baseyr-1),CSHELL3(Y),0,0    !average
 420  CONTINUE

      Close(23)

      END SUBROUTINE RESDBOUT



      SUBROUTINE CALC111D

!     EPA111D=1 is scedes switch to turn national utility energy efficiency subsidies and modeling on (primarily for modeling EPA's Clean Power Plan)
!     AB32SW=1 is scedes switch to turn CA energy efficiency modeling on

      IMPLICIT NONE
!      INTEGER MaxApps, MaxTypes, MaxBins! These are used in looping of report variables
!      PARAMETER (MaxApps=4)  !Maximum number of applications
!      PARAMETER (MaxTypes=4) !Maximum number of bulb types within an application
      INTEGER app
      INTEGER Y, B, E, D, F, Y1, NT
      INTEGER EU,RECCL,EQC,EQTYPE,TYPE,RECTY,S

!      QELRS in trills, BASELINEBKWH in billions kWh
!      BASELINEBKWH(d,y)=(QELRS(D,Y)/3412.)*10**3

         Y=CURCALYR
         !Y1=Y-BASEYR+1
         Y1=CURIYR

         IF(curcalyr .lt. 2017) THEN	!comment this block out out so savings can be calculated in years prior to 2017 for EE runs?
          DO D=1,MNUMCR-2
           SAVE111RES(D,Y1)=0.
           COST111RES(D,Y1)=0.
          ENDDO
         RETURN  ! DON'T RETURN DURING TESTING MODE
         ENDIF

!         Calculate savings and initialize cost to zero
         DO D=1,MNUMCR-2
          SAVE111RES(D,Y1)=BASELINEBKWH(D,Y1)-(RSFLCN(Y1,2,D)/3412.)*10**3
          COST111RES(D,Y1)=0.
         ENDDO

       DO 150 D=1,MNUMCR-2
       DO 150 B=1,MNUMBLDG
          DO 140 EU = 1,9
           TYPE = RTTYPECT(EU)
          DO 140 RECTY=RTTYEUPT(EU)+1,RTTYEUPT(EU+1)
!
!     CHECK TO SEE IF RECORD IS VALID FOR CURRENT YEAR, CURCALYR
!
          IF((Y.GE.RTINITYR(RECTY).AND. &
              Y.LE.RTLASTYR(RECTY)).AND.(RTCENDIV(RECTY).EQ.D)) THEN

              TYPE = TYPE + 1
              EQTYPE=RTEQTYPE(RECTY)
                    EQC=RTTYEQCL(RECTY)
              RECCL=RTCLEUPT(EU)+EQC

           IF(RTFUEL(RECCL).EQ.4) THEN  !electricity subsidies only

            IF(EU.eq.1) THEN !Calculate shell investment and subsidies with heating system records only
              COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&          !EEcosts - costs only accumulate in CPP runs
              ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty))  &       !new construction subsidies (..1) are from equipment only
                    +(HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))         !replacement purchases (..2) include subsidy for installation costs
                    DO S=1,NShellTypes
                     COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&   !EEcosts - costs only accumulate in CPP runs
                     shellsubsidy111D(y,reccl,s,b,d)*(shellbuilds(Y,TYPE,S,B,D))
                    ENDDO
             ELSE  !other equipment
              COST111RES(D,Y1)=COST111RES(D,Y1) + FLOAT(EPA111D)*&          !EEcosts - costs only accumulate in CPP runs
              ((HEATINGTYPEPURCH(Y,TYPE,B,D,1)*rtresub111D(recty))+  &
                    (HEATINGTYPEPURCH(Y,TYPE,B,D,2)*rteqsub111D(recty)))
            ENDIF

           ENDIF  ! fuel is electricity

          ENDIF !Year and Census division filter
 140     CONTINUE
 150  CONTINUE


 !     Add Lighting Subsidies (note, lighting bulb costs are converted into rtekdollaryr based on rlgtdollaryr)
       DO 151 app=1,NumApps
       DO 151 D=1,MNUMCR-2
       DO 151 B=1,MNUMBLDG
       DO 151 E=1,numtypes(app)
        COST111RES(D,Y1)=COST111RES(D,Y1)+ FLOAT(EPA111D)*(LTsubsidy(app,Y,E,B,d,1)+LTsubsidy(app,Y,E,B,d,2))  !EEcosts - costs only accumulate in CPP runs
 151  CONTINUE

         ! Before exit convert to billions of 1987 dollars & multiply incentive payments for approximate direct and indirect costs (slightly higher costs than EIA 861 for 2012)
         DO D=1,MNUMCR-2
           COST111RES(D,Y1)= 1.5*(COST111RES(D,Y1)*MC_JPGDP(-2)/MC_JPGDP(rtekdollaryr-baseyr+1) )/10**9  ! scale the incentive payment costs here to account for program administration
         ENDDO

 !    Add subsidies for distributed generation and deflate from igencapcostyr (generally not the same as rsmeqp) as well as convert from $mill to $bill
      DO 152 D=1,MNUMCR-2
       DO 152 NT=1, NTEK
        COST111RES(D,Y1)=COST111RES(D,Y1)+ FLOAT(EPA111D)*x111drensub(y1,d,nt)*( MC_JPGDP(-2)/MC_JPGDP(igencapcostyr-baseyr+1) )/1000.   !111dren  !EEcosts - costs only accumulate in CPP runs
 152  CONTINUE

         Write(9,*)'111d year, division, cost (bill 87$), savings (bkWh)'
         ! Print exactly what gets passed on for integrated debugging only
         DO D=1,MNUMCR-2
            WRITE(9,160) Y, D, COST111RES(D,Y1), SAVE111RES(D,Y1)
         ENDDO

 160   FORMAT(2I5,2f12.5)

      END SUBROUTINE CALC111D

!**********************************************************************
!**********************************************************************
!
        SUBROUTINE RDISTGEN
!
!  INCLUDES USER SPECIFIED NUMBER OF GENERAL TECHNOLOGIES:
!  -- CAN MODEL NON-OVERLAPPING VINTAGE RANGES OR INDIVIDUAL YEARS
!  -- CAN INCLUDE A USER-SPECIFIED NUMBER OF TECHNOLOGIES
!
!**********************************************************************
!**********************************************************************
       IMPLICIT NONE
!*******************************************
!   NEMS VARIABLES FROM RESD AND LDSM/EMM
!*******************************************
       Integer MaxNiche
       PARAMETER (MaxNiche=4)
       Real*4 xExistPen

!----LOCAL VARIABLES AND PARAMETERS INTERNAL TO RDISTGEN

!  DATA FOR PAYBACK COMPUTATION
       INTEGER IPAYBACK(30),ISIMPLEPAYBACK
       REAL*4  XMAXPEN,XSIMPLEPAYBACK,XPEN,XTEMP,xtempHH,xTest

!  DATA FOR INTERCONNECTION LIMITATION
       REAL*4  XINX(MNUMCR-2),XINXDECAY(MNUMCR-2,MNUMYR)
       Integer XINXFY, XINXLY

 !  DATA FOR 30-Year CASHFLOW MODEL CALCULATION
       REAL*4 XOUTLAY(30), XINTAMT(30), XPRIN(30)
       REAL*4 XLOANBAL(30), XDEPR(30)
       REAL*4 XFUELCOST(30), XTAXDEDUCT(30)
       REAL*4 XMAINTCOST(30), XTOTALCOST(30)
       REAL*4 XVALESAVE(30),XKWH(30)
       REAL*4 XNETCASHFLOW(30), XPAYMENT, XDOWNPAY
       REAL*4 XCUMNETFLOW(35) !add positions for "look ahead"
       REAL*4 XINTRATE, XTERM, XINFLATION, XLIFE, XDOWNPAYPCT
       REAL*4 XTAXCREDIT(30), XTAXRATE
       REAL*4 XEQCOST, XTAXCREDITPCT, XTAXCREDITMAXKW, XTAXCREDITMAX, XBASEYRFUELCOST
       REAL*4 XMAINTCOSTBASE, XVALESAVEBASE, XDEGRADATION
       REAL*4 XANNUALKWH,XGASINPUT,XWATERHTGMMBTU
       REAL*4 XBTUWASTEHEAT,XEXCESSKWH,XELECAVGUEC
       REAL*4 xSalestoGridPR, xRetailElecPR, xSizefromTaxOptim
       Real*4 xunits, xtrills, xcapacity, xtrillsownuse, xfuelusage, xhwbtu, xinvest

!  FOR RPS MODELING
       Real*4 xRetailElecPRnoRPS, xSalestoGridPrnoRPS, xRPS(ntek,MNUMYR)
       Real*4 xRetailElecPRadjRPS, xSalestoGridPradjRPS !for Markey 2009 bill credit adjustment features
       Integer iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear, iCALYR
       Integer iNumRPSCreditYrs(ntek,MNUMYR), iNumYrsatRPSBaseRate(ntek,MNUMYR), itemp1, itemp2
       ! For calculating credit factors to pass to Electric Generation Model
       Real*4 xPVGenAdded(MNUMCR,MNUMYR),xWindGenAdded(MNUMCR,MNUMYR)

       Real*4 xCompCredit, xCompGen, xCredit, xBonus
       Real*4 XVALESAVEBASEnoRPS, XVALESAVEBASEadjRPS

!  TECHNOLOGY-SPECIFIC DATA FROM INPUT FILE
       REAL*4 XDEGRAD(NTEK,MNUMYR),XELEFF(NTEK,MNUMYR)
       REAL*4 XEQLIFE(NTEK,MNUMYR),XWHRECOVERY(NTEK,MNUMYR)
       REAL*4 XINSTCOST(NTEK,MNUMYR),XCAPCOST(NTEK,MNUMYR)
       REAL*4 XMAINTCST(NTEK,MNUMYR),XAVAIL(NTEK,MNUMYR)
       REAL*4 XTXCRPCT(NTEK,MNUMYR),xTXCrMaxPerKW(NTEK,MNUMYR),xTXCrMaxPerSys(NTEK,MNUMYR)

       REAL*4 XTXCRPCT_div(MNUMYR,MNUMCR,NTEK)                                            !111dren
       REAL*4 XKW(NTEK,MNUMYR),XOPERHOURS(NTEK),XLOSSFAC(NTEK,MNUMYR)
       REAL*4 xIntervalCst(NTEK,MNUMYR)
       REAL*4 XALPHA,XPENPARM,XEXOGPEN(MNUMYR,MNUMCR,NTEK),ExogPVMistie(MNUMYR,MNUMCR)    !PVzipcalib
       INTEGER IFIRSTYR(NTEK,MNUMYR),ILASTYR(NTEK,MNUMYR), iIntervalYrs(NTEK,MNUMYR)
       INTEGER IFUELTYPE(NTEK),NUMTECHS,NUMYEARS,NUMDIV, iIntervalYrstoUse
       CHARACTER*10 AEQUIPNAME(NTEK,MNUMYR)
! Learning Related Variables
       Real *4 xadjcost,rlearncost,cumship,xbeta(ntek),xc0(ntek)
       Logical GlobalLearn
       Integer NVINT

!Internal Niche Variables
    REAL*4 QRSDGSG(MNUMYR,MNUMCR)                !   GRID ELECTRICITY SALES IN TRILLS
    Integer iNiche, iRateLevel                   !9,4max,3
    Integer NumPVNiche(MNUMCR)                   !Dimension Div
!Solar PV Niche Variables
    Real*4 xSolarInsolation(MNUMCR,MaxNiche,3)     !Dimensions Div, MaxNiche, RateLevel
    Real*4 xHHShare(MNUMCR,MaxNiche,3)             !Dimensions Div, MaxNiche, RateLevel
    Real*4 xRateScalar(MNUMCR,MaxNiche,3)          !Dimensions Div, MaxNiche, RateLevel
    Real*4 xAvgKWH(MNUMCR,MaxNiche,3)              !Dimensions Div, MaxNiche, RateLevel
    Real*4 xRoofAreaPerHH(MNUMCR,MaxNiche,3)       !Dimensions Div, MaxNiche, RateLevel
    Real*4 xRuralPctHH(MNUMCR,MaxNiche,3)          !Dimensions Div, MaxNiche, RateLevel
    Real*4 xSizefromRoofArea,xSizefromAnnualKWH,xSizeMax,xSizeMin,xcalcKW,xCalcEqCost,xSolarIns    !PVgen
    Real*4 SolarPVTechPotentialMW(MNUMYR,MNUMCR)
    Real*4 SolarPVAvailRoofArea(MNUMYR,MNUMCR)
    Real*4 SolarPVInstalledMW(MNUMYR,MNUMCR)
    Real*4 SolarPVUsedRoofArea(MNUMYR,MNUMCR)
    Real*4 xSqftPerKW
    Real*4 xpctPVSuitable             !Percentage of HH with a suitable south-facing roof
    Real*4 xpctWindSuitable           !Assumed percentage of HH for which wind could be appropriate

!Distributed Wind Niche Variables
    Real*4 xWindSpeed(MNUMCR,MaxNiche,3)           !Dimensions Div, MaxNiche, RateLevel
    Real*4 WindAvailHH(MNUMYR,MNUMCR)
    Real*4 WindTechPotentialMW(MNUMYR,MNUMCR)
    Real*4 WindInstalledMW(MNUMYR,MNUMCR)
    Real*4 xMpS                                    !meters per second temp variable

!OTHER LOCAL VARIABLES
    INTEGER IYR,ICURIYR,NV,NT,IDIV,ilife,r,i,F
    LOGICAL LPRINT ,LPRINT2   ! LPRINT, AND LPRINT2 CONTROL OUTPUT DETAIL
    INTEGER FILE_MGR          ! FILE MANAGER
    INTEGER*4  INFILE         ! FILE HANDLE FOR INPUT
    INTEGER*4  DGDAT          ! FILE HANDLE FOR OUTPUT
    REAL XValue

!NEW PV MODEL VARIABLES (use dynamic allocations due to zip code dimension for arrays, approximately 31,000 zips)    !PVPen
    !Static and Dynamic Variables for Econometric PV Penetration Model
    Integer NumZips                                 ! Read from input file, number of zip code records to read
    Logical usenewmodel                             ! Read from input file
    Integer EstYear                                 ! Estimation year for the econometric model, also first year used in projections
    Integer*4, allocatable:: ZipCode(:)             ! Zip Code for Diagnostics only
    Character*2, allocatable:: State(:)             ! State code
    Integer, allocatable:: CenDiv(:)                ! Census Division of Zip Code
    Real*4, allocatable:: Income(:)                 ! Income Per Capita in Zip
    Real*4, allocatable:: Households(:)             ! Households in Zip
    Real*4, allocatable:: PopDensity (:)            ! PopDensity in Zip
    Real*4, allocatable:: ElecRate(:)               ! Electric Rate in Zip
    Real*4, allocatable:: Income_L(:)               ! Initial Value for Iteration Control
    Real*4, allocatable:: Households_L(:)           ! Initial Value for Iteration Control
    Real*4, allocatable:: PopDensity_L(:)           ! Initial Value for Iteration Control
    Real*4, allocatable:: ElecRate_L(:)             ! Initial Value for Iteration Control
    Real*4, allocatable:: Insol(:)                  !
    Real*4, allocatable:: LagCDD(:)                 !
    Real*4 IntRate                                  ! National level variable
    Real*4 PVPrice                                  ! National level variable
    Real*4 MonthlyPayment                           ! National level variable
    Real*4, allocatable:: Lag1Installs(:)           ! Initial Lag1 Installs from Input File
    Real*4, allocatable:: Lag2Installs(:)           ! Initial Lag2 Installs from Input File
    Real*4, allocatable:: ProjectedInstalls(:)      ! Contains model projections, used to set lagged installs for subsequent projection years
    Integer, allocatable:: PureHurdle(:)            ! PureHurdle in combination with RuralZip determine model coefficient values
    Integer, allocatable:: RuralZip(:)              ! Density less than 10 HH per square mile
    Real*4, allocatable:: ModelInstalls(:)          ! For verification that results = R Code values in First Year, for Last Year's Projections Subsequently
    Real*4, allocatable:: CumUnits(:)               ! For constraining penetration
    Real*4 CINT(2,3),CHH(2,3),CPD(2,3),CINC(2,3),CINS(2,3), &
            CER(2,3),CCDD(2,3),CPMT(2,3),CIR(2,3),CLAG1(2,3),CLAG2(2,3),CPVP(2,3)  !Sets of model coefficients
    Real*16 xlogit, xnegbinom                      ! Temporary variables
    Integer j                                      ! Index variable for model selection
    !End of Additional Variable Assignments for Econometric PV Penetration Model    !PVPen


!------------------------------------------------------------
!   TEST FOR TRIGGER TO READ FILE AND BEGIN CALCULATIONS
!------------------------------------------------------------
     DGDAT=23

!  NO CALCULATIONS PRIOR TO RECSYEAR+1
      IF(CURCALYR.LT.RECSYEAR) RETURN
      IF(RSYR.EQ.RECSYEAR) OPEN(DGDAT,FILE='RDGENOUT.txt',FORM='FORMATTED')
      ICURIYR=RSYR-(baseyr-1)
      IF(CURCALYR.NE.RECSYEAR.OR.CURITR.NE.1) GOTO 95
                ! Read Dataset for New Econometric Penetration Model                !PVPen
                ! Data from EstYear onward where projections are based on an econometric logit/ hurdle model formulation
                INFILE=FILE_MGR('O','RGENTK',.FALSE.) !OPEN THE ZIP CODE Dataset
                READ(INFILE,'(19(/))')
                READ(INFILE,*)usenewmodel !if true the new model will be used for EstYear and beyond
      !Write(9,*) 'UseNewModel= ', usenewmodel
      READ(INFILE,*)EstYear     !this is the first year that the new model will project
      !Write(9,*) 'Estimation Year= ', EstYear
       DO i=1,3
        READ(INFILE,*)CINT(1,i),CINT(2,i)
        READ(INFILE,*)CHH(1,i),CHH(2,i)
        READ(INFILE,*)CPD(1,i),CPD(2,i)
        READ(INFILE,*)CINC(1,i),CINC(2,i)
        READ(INFILE,*)CINS(1,i),CINS(2,i)
        READ(INFILE,*)CER(1,i),CER(2,i)
        READ(INFILE,*)CCDD(1,i),CCDD(2,i)
        READ(INFILE,*)CPMT(1,i),CPMT(2,i)
        READ(INFILE,*)CIR(1,i),CIR(2,i)
        READ(INFILE,*)CLAG1(1,i),CLAG1(2,i)
        READ(INFILE,*)CLAG2(1,i),CLAG2(2,i)
        READ(INFILE,*)CPVP(1,i),CPVP(2,i)
        READ(Infile,*) !skip model title
       ENDDO
      !Write(9,*) CLAG1(1,1),CLAG2(1,1)
        READ(INFILE,'((/))')
        READ(INFILE,*)numzips

      !Beginning of Dynamic Array Allocations for new Econometric Model
        if(allocated(ZipCode)) deallocate(ZipCode); allocate(ZipCode(numzips))
        if(allocated(State)) deallocate(State); allocate(State(numzips))
        if(allocated(CenDiv)) deallocate(CenDiv); allocate(CenDiv(numzips))
        if(allocated(Income)) deallocate(Income); allocate(Income(numzips))
        if(allocated(Households)) deallocate(HouseHolds); allocate(Households(numzips))
        if(allocated(PopDensity)) deallocate(PopDensity); allocate(PopDensity(numzips))
        if(allocated(ElecRate)) deallocate(ElecRate); allocate(ElecRate(numzips))
        if(allocated(Income_L)) deallocate(Income_L); allocate(Income_L(numzips))
        if(allocated(Households_L)) deallocate(HouseHolds_L); allocate(Households_L(numzips))
        if(allocated(PopDensity_L)) deallocate(PopDensity_L); allocate(PopDensity_L(numzips))
        if(allocated(ElecRate_L)) deallocate(ElecRate_L); allocate(ElecRate_L(numzips))
        if(allocated(LagCDD)) deallocate(LagCDD); allocate(LagCDD(numzips))
        if(allocated(Insol)) deallocate(Insol); allocate(Insol(numzips))
        if(allocated(Lag1Installs)) deallocate(Lag1Installs); allocate(Lag1Installs(numzips))
        if(allocated(Lag2Installs)) deallocate(Lag2Installs); allocate(Lag2Installs(numzips))
        if(allocated(ProjectedInstalls)) deallocate(ProjectedInstalls); allocate(ProjectedInstalls(numzips))
        if(allocated(PureHurdle)) deallocate(PureHurdle); allocate(PureHurdle(numzips))
        if(allocated(RuralZip)) deallocate(RuralZip); allocate(RuralZip(numzips))
        if(allocated(ModelInstalls)) deallocate(ModelInstalls); allocate(ModelInstalls(numzips))
        if(allocated(CumUnits)) deallocate(CumUnits); allocate(CumUnits(numzips))

        ZipCode(:)=0
        State(:)=" "
        CenDiv(:)=0
        Income(:)=0
        Households(:)=0
        PopDensity(:)=0
        ElecRate(:)=0
        Income_L(:)=0
        Households_L(:)=0
        PopDensity_L(:)=0
        ElecRate_L(:)=0
        LagCDD(:)=0
        Insol(:)=0
        Lag1Installs(:)=0
        Lag2Installs(:)=0
        ProjectedInstalls(:)=0
        PureHurdle(:)=0
        RuralZip(:)=0
        ModelInstalls(:)=0
        CumUnits(:)=0
        IntRate=0
        PVPrice=0
        MonthlyPayment=0

      DO i=1,NumZips
        READ(INFILE,*) ZipCode(i),State(i),CenDiv(i),Income(i),Households(i),PopDensity(i),  &
             Insol(i),ElecRate(i),LagCDD(i),IntRate,PVPrice,MonthlyPayment,Lag1Installs(i),  &
             Lag2Installs(i),PureHurdle(i),RuralZip(i),ModelInstalls(i)
        Income_L(i)=Income(i)
        Households_L(i)=Households(i)
        PopDensity_L(i)=PopDensity(i)
        ElecRate_L(i)=ElecRate(i)
      ENDDO
     !write(9,*) 'Completed Read of Zip Code Data'
     !write(9,*) 'Last Value', ModelInstalls(numzips)
     INFILE=FILE_MGR('C','RGENTK',.FALSE.)

!End of processing of RGENTK for new econometric PV penetration model        !PVPen

!--------------------------------------------------------------
!   OPEN FILE AND READ/WRITE DATA
      INFILE=FILE_MGR('O','RSGENTK',.FALSE.) !OPEN THE DISTRIBUTED GEN DATA
!      Unit 23 is also used for the ResDBOut file and is closed when ResDBOut is written

 ! SKIP 20-LINE HEADER AND READ GENERAL CONTROL PARAMETERS AND INPUTS
      READ(INFILE,'(19(/))')
      READ(INFILE,*)LPRINT, LPRINT2
      IF(LPRINT)WRITE(DGDAT,*) 'MODEL YEAR ',RSYR, 'ITERATION ',CURITR

!  -- NUMBER OF TECHNOLOGIES (3), NUMBER OF MODEL YEARS (MNUMYR),
!         AND NUMBER OF MODELED CENSUS DIVISIONS (9)
      READ(INFILE, '(//)')
      READ(INFILE,*) NUMTECHS,NUMYEARS,NUMDIV
      IF(LPRINT)WRITE(DGDAT,*) NUMTECHS,NUMYEARS,NUMDIV

      READ(INFILE,'(//)')
      READ(INFILE,*) igencapcostyr
      IF(LPRINT)WRITE(DGDAT,*) igencapcostyr

!  -- LPRINT TURNS ON TRACING OF EXECUTION
!         LPRINT2 PROVIDES DETAILS OF THE CASHFLOW CALCS FOR "VINTAGE" YEARS
      READ(INFILE, '(//)')
      READ(INFILE,*) XALPHA, XPENPARM
      IF(LPRINT)WRITE(DGDAT,*) XALPHA,XPENPARM
!  -- XALPHA AND XPENPARM CONTROL THE MAGNITUDE AND SHAPE OF THE
!       PENETRATION OF DISTRIBUTED GENERATION TECHNOLOGIES

      READ(INFILE, '(//)')
! Vary DG capacity by year -- eliminate read here
!      READ(INFILE,*) (XKW(NT), NT=1,NUMTECHS)
!      IF(LPRINT)WRITE(DGDAT,'(7F9.2)') (XKW(NT), NT=1,NUMTECHS)
      READ(INFILE,*) (XOPERHOURS(NT), NT=1,NUMTECHS)
      IF(LPRINT)WRITE(DGDAT,'(7F9.2)') (XOPERHOURS(NT), NT=1,NUMTECHS)
!  -- FOR EACH OF THE TECHNOLOGIES, SYSTEM SIZE IN KW, AND ANNUAL OPERATING
!        HOURS (RELEVENT ONLY FOR FUEL USING GENERATION TECHNOLOGIES)

      READ(INFILE, '(//)')
      READ(INFILE,*) GlobalLearn
      IF(LPRINT)WRITE(DGDAT,*) "Global Learning = ",GlobalLearn

      READ(INFILE, '(//)')
      READ(INFILE,*) (XBeta(NT), NT=1,NUMTECHS)
      IF(LPRINT)WRITE(DGDAT,'(3F9.2)') (XBeta(NT), NT=1,NUMTECHS)
      READ(INFILE,*) (Xc0(NT), NT=1,NUMTECHS)
      IF(LPRINT)WRITE(DGDAT,'(3F9.2)') (Xc0(NT), NT=1,NUMTECHS)
!  -- FOR EACH OF THE TECHNOLOGIES, Learning Betas (doubling parameter, 0=no learning) and c0's (initial costs)
!
      READ(INFILE, '(//)')
      READ(INFILE,*) iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
      IF(LPRINT)WRITE(DGDAT,'(3I8)') iRPSStartYear, iRPSPhaseOutYear, iRPSGrandFatherYear
!  -- The above are scalars for multiplying the RPS credit price

      READ(INFILE, '(//)')
      READ(INFILE,*) (XINX(i),i=1,NUMDIV)
      IF(LPRINT)WRITE(DGDAT,'(9F7.3)') (XINX(i),i=1,NUMDIV)
!  -- The above are scalars for limiting the penetration based on interconnection index
      READ(INFILE,*) XINXFY, XINXLY
      IF(LPRINT)WRITE(DGDAT,'(3I8)') XINXFY, XINXLY
!  -- The above are the first and last years of the interconnection limits

      READ(INFILE, '(//)')
      READ(INFILE,*)XTAXRATE,XDOWNPAYPCT,XINTRATE,XTERM,XINFLATION
      IF(LPRINT)WRITE(DGDAT,'(4F7.2)') XTAXRATE, XDOWNPAYPCT, XINTRATE, XTERM, &
         XINFLATION
!  NOTES:
!  -- XTAXRATE IS THE COMBINED FED AND STATE INCOME TAX RATE WHICH IS USED
!       IN THE CASHFLOW CALCULATIONS FOR INTEREST PAID ON EQUIPMENT LOANS
!       WHICH IS ASSUMED TO BE "ROLLED IN" WITH THE MORTGAGE
!  -- XDOWNPAYPCT IS THE MORTGAGE DOWN PAYMENT PERCENTAGE
!  -- XINTRATE IS THE MORTGAGE ANNUAL LOAN RATE
!  -- XTERM IS THE NUMBER OF YEAR FOR THE LOAN (ASSUMING 20 YEARS SIMPLIFIES
!       EQUIPMENT ACCCOUNTING (REPLACEMENT OF ORIGINAL EQUIPMENT IS NOT
!       PART OF THE CASFLOW, SINCE IT DIES AFTER 20 YEARS)
!  -- XINFLATION IS THE INFLATION FOR THE CASHFLOW CALCULATIONS WHICH ARE
!       IN REAL DOLLARS, TO DISCOUNT LOAN PAYMENTS (IN NOMINAL$)

      READ(INFILE, '(///)')
      READ(INFILE,*) NVINT       ! read number of technology records in file
      IF(LPRINT)WRITE(DGDAT,*) NVINT
   ! 0 out technology variable to be read in next
      AEQUIPNAME = "          "
      IFUELTYPE = 0.0
      IFIRSTYR = 0.0
      ILASTYR = 0.0
      XKW = 0.0
      XELEFF = 0.0
      XLOSSFAC = 0.0
      XDEGRAD = 0.0
      XEQLIFE = 0.0
      XWHRECOVERY = 0.0
      XINSTCOST = 0.0
      XCAPCOST = 0.0
      XMAINTCST = 0.0
      xIntervalCst = 0.0
      iIntervalYrs = 0.0
      XAVAIL = 0.0
      XTXCRPCT = 0.0
      xTXCrMaxPerKW = 0.0
      xTXCrMaxPerSys = 0.0

!  POPULATE TECHNOLOGY ARRAY

          DO NT=1,NUMTECHS
          DO NV=1,NVINT
!   THE ORIGINAL TECHNOLOGY FILE ASSUMES:
!      NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
!      NT=2 IS FUEL CELL TECHNOLOGY
!      NT=3 IS MICRO TURBINE TECHNOLOGY
!   THE VINTAGES APPLY TO DIFFERENT non-overlapping TIME PERIODS AND MAY EMBODY TECH PROGRESS
!
! Vary DG capacity by year -- add read of xkw capacity by vintage here
          READ(INFILE,*,END=99) AEQUIPNAME(NT,NV), IFUELTYPE(NT), &
                    IFIRSTYR(NT,NV),   ILASTYR(NT,NV), &
                    XKW(NT,NV),   XELEFF(NT,NV), &
                    XLOSSFAC(NT,NV),  XDEGRAD(NT,NV), &
                    XEQLIFE(NT,NV),  XWHRECOVERY(NT,NV), &
                    XINSTCOST(NT,NV),  XCAPCOST(NT,NV), &
                    XMAINTCST(NT,NV),  xIntervalCst(NT,NV), &
                    iIntervalYrs(NT,NV),  XAVAIL(NT,NV),       &
                    XTXCRPCT(NT,NV),  xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV),   &

                    (xtxcrpct_div(nv,idiv,nt),idiv=1,9),     &                             !111dren
                    xtemp, itemp1, itemp2

!  NOTES:
!  --  AEQUIPNAME IS NAME FOR REPORTING PURPOSES
!  --  IFUELTYPE IS THE FUEL USED BY THE TECHNOLOGY (0 FOR SOLAR OR WIND)
!         THIS FUEL TYPE MUST COINCIDE WITH THE MAIN MODEL DEFINITION OF
!         FUELS
!  --  IFIRSTYR IS THE FIRST YEAR A TECHNOLOGY CAN BE PURCHASED
!  --  ILASTYR IS THE LAST YEAR A TECHNOLOGY CAN BE PURCHASE (DON'T
!         ALLOW TECHNOLOGIES TO "OVERLAP" OR "GAP" (E.G. VINTAGE 1 1993-1999,
!         VINTAGE 2, 2000-2004, VINTAGE 3, 2005-2013, VINTAGE 4, 2014....
!  --  XELEFF IS THE ELECTRICAL CONVERSION EFFICIENCY OF THE TECHNOLOGY
!  --  XLOSSFAC IS THE LOSS FACTOR FROM GENERATION TO END USE 'INCLUDES LINE LOSS,
!         INVERTER LOSSES, ETC....
!  --  XDEGRAD IS THE ANNUAL DEGRADATION IN THE EFFICIENCY OF SOLAR PV
!  --  XEQLIFE IS THE EQUIPMENT LIFE IN YEARS
!  --  XWHRECOVERY IS THE PERCENTAGE OF WASTE HEAT (FOR FUEL USING TECHNOLOGIES)
!         THAT CAN BE RECOVERED FOR WATER HEATING (ANYTHING IN EXCESS OF AVERAGE
!         WATER HEATING REQUIREMENTS IS ASSUMED WASTED)
!  --  XINSTCOST INSTALLATION COST PER KW IN 1999 DOLLARS
!  --  XCAPCOST CAPITAL COST PER KW IN 1999 DOLLARS
!  --  XMAINTCST ANNUAL MAINTENANCE COST IN 1999 DOLLARS
!  --  XAVAIL = 1.0-FORCED OUTAGE RATE
!  --  XTXCRPCT IS THE TAX CREDIT PERCENTAGE FOR THIS TECHNOLOGY
!  --  xTXCrMaxPerKW IS THE MAXIMUM DOLLAR AMOUNT (PER KW) OF THE TAX CREDIT (if zero, no cap)
!  --  xTXCrMaxPerSys IS THE MAXIMUM DOLLAR AMOUNT (PER System) OF THE TAX CREDIT (if zero, no cap)

      IF(LPRINT)WRITE(DGDAT,30) AEQUIPNAME(NT,NV),  IFUELTYPE(NT), &
                    IFIRSTYR(NT,NV),   ILASTYR(NT,NV), &
                    XKW(NT,NV),   XELEFF(NT,NV), &
                    XLOSSFAC(NT,NV),  XDEGRAD(NT,NV), &
                    XEQLIFE(NT,NV),  XWHRECOVERY(NT,NV), &
                    XINSTCOST(NT,NV),  XCAPCOST(NT,NV), &
                    XMAINTCST(NT,NV),  xIntervalCst(NT,NV), &
                    iIntervalYrs(NT,NV),  XAVAIL(NT,NV),       &
                    XTXCRPCT(NT,NV),  xTXCrMaxPerKW(NT,NV), xTXCrMaxPerSys(NT,NV),   &
                    (xtxcrpct_div(nv,idiv,nt),idiv=1,9),     &                           !111dren
                    xtemp, itemp1, itemp2

             ! Immediately populate the RPS variable mapping vintages to Icuriyr
             do iyr=ifirstyr(nt,nv),ilastyr(nt,nv)
              xRPS(nt,iyr-(baseyr-1))=xtemp
              iNumYrsatRPSBaseRate(nt,iyr-(baseyr-1))=itemp1
              iNumRPSCreditYrs(nt,iyr-(baseyr-1))=itemp2
             enddo

          ENDDO
          ENDDO
 30    FORMAT(1X,A10,3I6,F9.1,3F9.2,2F9.1,F9.2,3F9.0,I6,2F9.2,2F9.1,9F9.2,F5.1,2I5)

!Debugging to see what we ended up with:
!             do iyr=1,MNUMYR
!               WRITE(DGDAT,*)xrps(3,iyr),iNumYrsatRPSBaseRate(3,iyr),iNumRPSCreditYrs(3,iyr)
!             enddo

!      PV NICHES:
!      Add an Arbitrary Number of Insolation Niches Customized to Each Division
!      Add 3 Rate Levels -- High, Mid and Low Average Rates
!
!      READ SOLAR INSOLATION, SQFT SHARES, AVERAGE ELEC RATES RELATIVE TO CENSUS DIV, ROOF TO SQFT RATIOS,
!        & WIND SPEED FOR "NICHES"
!      THESE VALUES ARE ESTIMATED FROM RECS CLIMATE ZONES (MAPPED AS PER CBECS DEFINITIONS)
!      BY CENSUS DIVISION AND "OVERLAYED" ONTO A MAP OF PV SOLAR RADIATION (LATITUDED TILT)
!      DEVELOPED BY NREL'S ELECTRIC & HYDROGEN TECHNOLOGIES & SYSTEMS CENTER - MAY 2004
!
      READ(INFILE, '(//)')
      DO I=1,NUMDIV
       READ(INFILE,*) iDiv, iNiche
       IF(LPRINT)Write(dgdat,*) "iDiv, iNiche", idiv, iniche
       NumPVNiche(iDiv)= iNiche
        Do iNiche=1,NumPVNiche(iDiv)
          Do iRateLevel=1,3
            READ(INFILE,*) xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
                   xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel),  &
                   xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
            IF(LPRINT)WRITE(dgdat,87) iDiv,xSolarInsolation(iDiv,iNiche,iRateLevel),xHHShare(iDiv,iNiche,iRateLevel), &
                   xRateScalar(iDiv,iNiche,iRateLevel), xAvgKWH(iDiv,iNiche,iRateLevel),  &
                   xRoofAreaPerHH(iDiv,iNiche,iRateLevel), xWindSpeed(iDiv,iNiche,iRateLevel),xRuralPctHH(iDiv,iNiche,iRateLevel)
          END DO !iRateLevel
        END DO !iNiche
      END DO !NumDiv

 87   FORMAT(1x,"Div= ",I3,3F10.4,2F12.4,2F10.4)

      DO IDIV=1,NUMDIV
      DO NT=1,NUMTECHS
      READ(INFILE, '(//)',END=99)
             READ(INFILE,*,END=99) (XEXOGPEN(IYR,IDIV,NT),IYR=RECSYEAR-BASEYR+1,NUMYEARS)
      IF(LPRINT)WRITE(DGDAT,*) 'TECHNOLOGY ',NT,'DIVISION ',IDIV
             IF(LPRINT)WRITE(DGDAT,97) (XEXOGPEN(IYR,IDIV,NT),IYR=1,NUMYEARS)
 97   FORMAT(5F10.0)
      ENDDO
      ENDDO


      READ(INFILE, * ,END=96)  XTEST
       WRITE(DGDAT,*) &
          'INPUT ERROR ON RESIDENTIAL DIST GEN TECHNOLOGY DATA FILE'
       WRITE(DGDAT,*) &
          'EXTRA DATA ENCOUNTERED -- MOST LIKELY A SEVERE PROBLEM'

      GOTO 96

 99    WRITE(DGDAT,*) &
          'INPUT ERROR ON RESIDENTIAL DIST GEN TECHNOLOGY DATA FILE'
       WRITE(DGDAT,*) &
          'TOO FEW DATA ENCOUNTERED -- A SEVERE PROBLEM'

 96    INFILE=FILE_MGR('C','RSGENTK',.FALSE.)

        DO IDIV=1,NUMDIV
         CGCPVRES(IDIV,ICURIYR)=1.
         CGCWNRES(IDIV,ICURIYR)=1.
         DO NT=1,NUMTECHS
          DO IYR=1,NUMYEARS
!           XEXOGPEN(IYR,IDIV,NT)=0.0 !used for testing / removing exogenous DG installations
           UNITS(IYR,IDIV,NT)=0.
           CAP(IYR,IDIV,NT)=0.
           TRILLS(IYR,IDIV,NT)=0.
           HWBTU(IYR,IDIV,NT)=0.
           GASUSAGE(IYR,IDIV,NT)=0.
           TRILLSOWNUSE(IYR,IDIV,NT)=0.
          ENDDO
         ENDDO
        ENDDO
        !Assumptions for developing technical potential for PV and Wind
        !Based on orientation alone approximately 50% of HH would have a suitable
        !   southwest to southeast facing roof surface.  Next assume that of the
        !   suitably oriented HH only 1/2 of the roof area is south facing.  Also
        !   assume that 40% of this area is unavailable due to shading and
        !   other issues like roof impediments.
        !        xpctPVSuitable= .5*.5*(1.-.4)
        xpctPVSuitable= .5*.5*(1.-.4)

!--------END OF READ AND INITIALIZE

!-------------------------------------------------------------
!  BEGIN ECONOMIC PENETRATION MODELING FOR PROJECTED DG BUILDS
!-------------------------------------------------------------
 95    CONTINUE

       !Initialize accumulators to allow for multiple NEMS iterations
       DO IDIV=1,NUMDIV
         QRSDGSG(ICURIYR,IDIV)=0.
         SolarPVTechPotentialMW(icuriyr,IDIV)=0.
         SolarPVInstalledMW(icuriyr,IDIV)=0.
         SolarPVAvailRoofArea(icuriyr,IDIV)=0.
         SolarPVUsedRoofArea(icuriyr,IDIV)=0.
         WindAvailHH(icuriyr,IDIV)=0.
         WindTechPotentialMW(icuriyr,IDIV)=0.
         WindInstalledMW(icuriyr,IDIV)=0.
         do nt=1,numtechs
            x111drensub(icuriyr,idiv,nt)=0.  !111dren
         enddo
         !Also set interconnection limit variable used for all technologies, by Census Division
         !INXLIMIT (moved from below, and now set up here to accommodate new PV penetration approach)
         XINXDECAY(IDIV,ICURIYR)=1.
         IF(CURCALYR.eq.XINXFY) THEN
           XINXDECAY(IDIV,ICURIYR)=XINX(IDIV)
         ELSE
           XINXDECAY(IDIV,ICURIYR)=MIN(1.,(XINX(IDIV)+((1.-XINX(IDIV))*(FLOAT(CURCALYR-XINXFY)/FLOAT(XINXLY-XINXFY)))))
         END IF
!        write(9,*) 'Interconnection Limit ',curcalyr, idiv, XINXDECAY(IDIV,ICURIYR)
        END DO

          DO NT=1,NUMTECHS

            !     NT=1 IS SOLAR PHOTOVOLTAIC TECHNOLOGY
            !     NT=2 IS FUEL CELL TECHNOLOGY
            !     NT=3 IS DISTRIBUTED WIND TURBINE TECHNOLOGY
            !
            !     FILTER FOR "VINTAGE" APPROPRIATE FOR THIS MODEL YEAR

            DO NV=1,NVINT
            IF(IFIRSTYR(NT,NV).GT.CURCALYR) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
            IF(ILASTYR (NT,NV).LT.CURCALYR) GOTO 66  !SKIP OUT-OF-SCOPE VINTAGES
            IF(LPRINT) THEN
             IF(IFUELTYPE(NT).EQ.0) THEN
                WRITE(DGDAT,*)'BEGINNING CASHFLOW CALCS FOR SOLAR PV OR WIND SYSTEMS'
               ELSE
                WRITE(DGDAT,*) &
                  'BEGINNING CASHFLOW CALCS FOR FUEL CELL / OTHER SYSTEMS'
             ENDIF
            ENDIF

            !INITIALIZE VALUES FOR OPERATING COST CALCULATIONS
            !
            ! Update "Learned" Costs for PV, Fuel Cells, Wind and Micro Turbines
            !

        if(aequipname(nt,nv) .eq. "Fuel_Cell" )then
              cumship=CFuelCell_MW(icuriyr-1)+ RFuelCell_MW(icuriyr-1)! buildings shipments only
              ! Not Enabled ! If (globallearn.eq.1)cumship=CFuelCell_MW(icuriyr-1)+ RFuelCell_MW(icuriyr-1) +UFuelcell_MW(icuriyr-1)+ IntnlFuelCell_MW(icuriyr-1)+ IFuelCell_MW(icuriyr-1)
              xadjcost = rLearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',icuriyr+(baseyr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xcapcost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xadjcost

         elseif(aequipname(nt,nv) .eq. "Solar_PV" )then
              If (globallearn)then
                 ! globallearn=True indicates to include electric generator PV installs in the learning calculations for buildings
                 cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1)+ UPV_MW(icuriyr-1)
                Else
                 cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1)
              Endif
              ! Not Enabled ! If(globallearn.eq.1) cumship=CPV_MW(icuriyr-1) +RPV_MW(icuriyr-1)+ UPV_MW(icuriyr-1) + IntnlPV_MW(icuriyr-1)+IPV_MW(icuriyr-1)
              xadjcost = rLearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',icuriyr+(baseyr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xcapcost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xadjcost

         elseif(aequipname(nt,nv) .eq. "Wind" )then
              cumship=CWind_MW(icuriyr-1) +RWind_MW(icuriyr-1)
              ! Not Enabled ! If(globallearn.eq.1) cumship=CWind_MW(icuriyr-1) +RWind_MW(icuriyr-1)+ UWind_MW(icuriyr-1) + IntnlWind_MW(icuriyr-1) + IWind_MW(icuriyr-1)
              xadjcost = rLearnCost (xcapcost(nt,nv), xbeta(nt), xc0(nt), cumship, 23)
                    IF(LPRINT)WRITE(DGDAT,*)'current year ',icuriyr+(baseyr-1)
                    IF(LPRINT)WRITE(DGDAT,*)'learning beta ',xbeta(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'initial cost ',xc0(nt)
                    IF(LPRINT)WRITE(DGDAT,*)'cumulative shipments ',CumShip
                    IF(LPRINT)WRITE(DGDAT,*)'RSGENTK default cost ',xcapcost(nt,nv)
                    IF(LPRINT)WRITE(DGDAT,*)'learning-adjusted cost ',xadjcost

         else
              xadjcost=xcapcost(nt,nv)
        endif

          XTAXCREDITPCT=XTXCRPCT(NT,NV)
          XTAXCREDITMAXKW=xTXCrMaxPerKW(NT,NV)
          XTAXCREDITMAX=xTXCrMaxPerSys(NT,NV)
          XLIFE=XEQLIFE(NT,NV)
          XEQCOST=(xadjcost+XINSTCOST(NT,NV)) * XKW(NT,nv) ! Vary DG capacity by year --
          XDEGRADATION=XDEGRAD(NT,NV)
            !      IF(LPRINT)
            !     1  WRITE (DGDAT,'(4F12.2)') XINSTCOST(NT,NV),XCAPCOST(NT,NV), XEQCOST,
            !     2           XDEGRADATION


            DO IDIV=1,NUMDIV

            ! Initialization to output accumulating variables
            xunits=0.
            xtrills=0.
            xcapacity=0.
            xtrillsownuse=0.
            xfuelusage=0.
            xhwbtu=0.
            xinvest=0.
!            CGCPVRES(IDIV,ICuriyr)=1. !RPS Variable for Electricity Module
!            CGCWNRES(IDIV,ICuriyr)=1. !RPS Variable for Electricity Module

            IF(NT.EQ.1 .AND. usenewmodel .AND. CURCALYR.GE.EstYear)goto 26  !FOR PV FOR THE ESTIMATION YEAR AND BEYOND USE CD MODEL

            !      WRITE (DGDAT,*) 'MODEL YEAR = ',CURCALYR, 'CENSUS DIVISION ', IDIV
            !      IF(LPRINT) WRITE(DGDAT,*) 'VINTAGE YEAR',CURCALYR
            !          WRITE(DGDAT,*)
            !     1  '   COMPUTING OPERATING COSTS AND VALUE OF ENERGY SAVINGS',IDIV

            IF(LPRINT.and.LPRINT2) &
             WRITE(DGDAT,*) 'FUEL TYPE ',IFUELTYPE(NT),'TECHNOLOGY ',NT

            ! Calculate Grid Sales Price
            ! Units are in $/kWh in year dollars of capital costs
            ! Assumed not to be "niche" related
             xSalestoGridPR=PELME(IDIV,ICURIYR) &
                *.003412*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)

            Do iNiche=1,NumPVNiche(iDiv)          !Add Insolation and Wind Niches
            Do iRateLevel=1,3                     !Add Niches for High=1, Mid=2 and Low=3 Average Rates
            If(xHHShare(iDiv,iNiche,iRateLevel)==0.) goto 25        !next niche -- skip those that have 0 HH share
            ! Set average consumption
            xelecavguec=xAvgKWH(iDiv,iNiche,iRateLevel)
 !-----------------------------------
 ! -Setup for Solar Calculations
 !-----------------------------------
            IF(NT.eq.3)GOTO 100  ! Wind
            IF(NT.eq.2)GOTO 150  ! Fuel Cells

              ! CALCULATION OF KWH SUPPLIED FOR Solar
              !  The quantity "77.*(.14/xeleff)*xkw" represents the estimated module square footage.
              !  Thus the kWh supplied is:
              !      annualkwh=eff*insolation*sqftperkw*systemkw*lossadj
              !  where lossadj represents average non-optimality factor (orientation effects, etc.)
              !  Notes: Future efficiency gains will result in a smaller collector footprint for a given kW capacity.
              !         Solar insolation is in kWh/m^2/day convert to annual per square foot (365.25/10.8)
              xSqftperKW=77.*.14/XELEFF(NT,NV)

              ! Optimize capacity: set maximums based on 80% of the optimally oriented roof surface area is available (e.g., non-shaded)         !PVgen
              !  Assume only 40% of roof area is suitable for PV to allow for non-optimal orientation and/or complex roof angles.                !PVgen
              !  Also, assume most homeowners cover only 75% of the potential max area and assume a global maximum and minimum of 10 and 1 kW.   !PVgen
              if (CURCALYR>=RECSYEAR) then                                                                                                       !PVgen
                 xSolarIns=xSolarInsolation(iDiv,INiche,IRateLevel)                                                                              !PVgen
                 ! transform solar insolation to account for unknown orientations                                                                !PVgen
                 xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2                                                                     !PVgen
                 xSizefromRoofArea= (xRoofAreaPerHH(IDiv,iNiche,iRateLevel)*0.8*0.4*0.75/xSqftperKW)                                             !PVgen
                 xSizefromAnnualKWH=xelecavguec/(xeleff(nt,nv)*xSolarIns*365.25/10.8*xSqftperKW*XLOSSFAC(NT,NV))                                 !PVgen

                 ! also optimize to maximize the after tax cost per kW based on credits
                 xSizefromTaxOptim=xSizeMax !set to max size if no cap on tax credit
                 ! else compute largest size that fully utilizes the credit
                 ! first reset the tax credit percentage to potentially something less if the max credit per kW is capped
                 if(xtaxcreditpct>0. .and. xtaxcreditmaxkw>0.) then
                    xtaxcreditpct=min(xtaxcreditpct,xtaxcreditmaxkw/(xadjcost+XINSTCOST(NT,NV)))
                 endif
                 if(xtaxcreditpct>0. .and. xtaxcreditmax>0.) xSizefromTaxOptim = (xtaxcreditmax/xtaxcreditpct)/(xadjcost+XINSTCOST(NT,NV))
                 xSizeMax=10.      !set absolute maximum size
                 xSizeMin=1.       !set absolute minimum size
                 xcalcKW=float(ifix(max(min(xSizefromRoofArea,xSizefromTaxOptim,xSizeMax),xSizeMin)))    !removed RECS average generation constraint
              else
                 xcalcKW=xkw(nt,nv)  !set size to menu capacity
              endif

              XANNUALKWH=XELEFF(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*XLOSSFAC(NT,NV)    !PVgen

              !   The internal NEMS energy prices are converted to "current year" dollars (the
              !   dollar year for the DG capacity costs) from the internal NEMS year of 1987 dollars.
              !   Note that MC_JPGDP(-2) is the deflator for 1987.
              !   For PV, use the air conditioning price as the implicit value for own-use generation,
              !   due to the high "coincidence factor" between PV output and AC loads.
              !   This usage of the air conditioning price is to reflect average summer prices, when PV output
              !   is at its highest.
              xRetailElecPR=( PELRSOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +xRPS(nt,icuriyr)*EPRPSPR(ICURIYR)/1000. )*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRadjRPS=( PELRSOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +1.0*EPRPSPR(ICURIYR)/1000. )*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRnoRPS= PELRSOUT(IDIV,ICURIYR,2)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 *MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)                                            !own-use no RPS credit

              ! Compare annual PV generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=XANNUALKWH-XELECAVGUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=XANNUALKWH*xRetailElecPr  ! own-use
               ELSE
                XVALESAVEBASE= &
                XEXCESSKWH*( xSalestoGridPR+xRPS(nt,icuriyr)*EPRPSPR(ICURIYR)/1000.*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2) ) & !adds scaled RPS credit
                 +XELECAVGUEC*xRetailElecPR             !plus own-use
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (a la Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=XANNUALKWH*xRetailElecPradjRPS
              ELSE
                XVALESAVEBASE= XELECAVGUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(ICURIYR)/1000.*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2) ) !grid sales with scaled RPS credit
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=XANNUALKWH*xRetailElecPrnoRPS  ! own-use only
               ELSE
                XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +XELECAVGUEC*xRetailElecPRnoRPS
              ENDIF

              ! Just in Case RPS Credits come through in years before they should be credited to residential:
              If (CURCALYR .lt. iRPSStartYear) then
                XVALESAVEBASE=XVALESAVEBASEnoRPS
              Endif

              ! Zero out unused variables
              XBASEYRFUELCOST=0.
              XGASINPUT=0.
              XWATERHTGMMBTU=0.
              XBTUWASTEHEAT=0.
              XMAINTCOSTBASE=XMAINTCST(NT,NV) * xcalckw
              IF(LPRINT)WRITE (DGDAT,*) &
               "AC Price R&C", PELRSOUT(IDIV,ICURIYR,2), PELCMOUT(IDIV,ICURIYR,2), &
               "GridSalesPrice ", PELME(idiv,icuriyr), &
               "Deflators ", MC_JPGDP(igencapcostyr-(baseyr-1)),MC_JPGDP(-2), &
               "RPS Credit (mills)", EPRPSPR(ICURIYR)
               !Write(9,*) 'using old niche model', curcalyr, idiv, iniche

              GOTO 200

 !-----------------------------
 ! -Setup for Wind Calculations
 !-----------------------------

 100          Continue
              ! Wind generation is valued at the average residential electricity price.
              ! Convert to same year dollars as generation capital costs and then use
              !  inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data
              xRetailElecPR=( PELRS(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +xRPS(nt,icuriyr)*EPRPSPR(ICURIYR)/1000. )*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRadjRPS=( PELRS(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 +1.0*EPRPSPR(ICURIYR)/1000. )*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)     !own-use including scaled RPS credit
              xRetailElecPRnoRPS= PELRS(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel)*.003412 &
                 *MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)     !own-use no RPS credit

              ! Account for total households with potential for wind penetration
              ! Assume xx% of households have lots of 0.5 acre or above and are appropriate
              WindAvailHH(ICuriyr,IDIV)= WindAvailHH(ICuriyr,IDIV) + &
               (HSEADD(CURCALYR,1,IDIV)+EH(CURCALYR,1,IDIV)) * xHHShare(iDiv,iNiche,iRateLevel) &
               *xRuralPctHH(iDiv,iNiche,iRateLevel)                                                    !Assumes rural households are suitable for wind

              ! CALCULATION OF KWH SUPPLIED FOR WIND
              !  Wind speed is in m/s
              !  xeleff represents relative efficiency of future technologies relative to today's models
              !  xMpS is the wind speed in meters per second and capacity factor is a cubic function of
              !  wind speed (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3).
              xMpS=xWindSpeed(iDiv,iNiche,iRateLevel)
              XANNUALKWH=XELEFF(NT,NV)/xeleff(nt,1)* &
               (.0645 -0.0670*xMpS +.0210*xMpS**2 -.0011*xMpS**3)*XKW(NT,NV)*8760.*XLOSSFAC(NT,NV)

              ! Compare annual Wind generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=XANNUALKWH-XELECAVGUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=XANNUALKWH*xRetailElecPr     ! own-use only
               ELSE
                XVALESAVEBASE= XELECAVGUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+xRPS(nt,icuriyr)*EPRPSPR(ICURIYR)/1000.*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
              ENDIF
              ! Recompute value of energy savings with RPS credit of 1 for years where credit can switch (a la Markey 2009 Bill Provisions)
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEadjRPS=XANNUALKWH*xRetailElecPradjRPS
              ELSE
                XVALESAVEBASE= XELECAVGUEC*xRetailElecPR   &                                                                   !own-use
                 +XEXCESSKWH*( xSalestoGridPR+1.0*EPRPSPR(ICURIYR)/1000.*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2) ) !grid sales w/ scaled RPS credit
              ENDIF
              ! Recompute value of energy savings without RPS credit for potential phase outs
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASEnoRPS=XANNUALKWH*xRetailElecPrnoRPS  ! own-use only
               ELSE
                XVALESAVEBASEnoRPS= XEXCESSKWH*xSalestoGridPR +XELECAVGUEC*xRetailElecPRnoRPS
              ENDIF

              !Just in Case RPS Credits come through in years before they should be credited to residential:
              If (CURCALYR .lt. iRPSStartYear) then
                XVALESAVEBASE=XVALESAVEBASEnoRPS
              Endif

             ! Zero out variables not relevant to Wind
             XBASEYRFUELCOST=0.
             XGASINPUT=0.
             XWATERHTGMMBTU=0.
             XBTUWASTEHEAT=0.
             xCalcKW=xkw(nt,nv)
             XMAINTCOSTBASE=XMAINTCST(NT,NV) * xcalckw
             ! End Setup for Wind Calculations
             GOTO 200 ! Jump to Cash Flow Model

 !-------------------------------------
 ! -Setup for Fuel Cell Calculations
 !-------------------------------------

 150          Continue
              ! Fuel Cell generation is valued at the average residential electricity price.
              ! Convert to same year dollars as generation capital costs and then use
              !  inflation to maintain nominal dollars. $/kWh in year dollars of capital cost data
              xRetailElecPR=PELRS(IDIV,ICURIYR)*xRateScalar(iDiv,iNiche,iRateLevel) &
                *.003412*MC_JPGDP(igencapcostyr-baseyr+1)/MC_JPGDP(-2)

              ! COMPUTE ANNUAL KWH GENERATION
              XANNUALKWH=XOPERHOURS(NT) * XAVAIL(NT,NV) * XKW(NT,nv) * XLOSSFAC(NT,NV)
              ! AVERAGE UECS FOR GAS WATER HEATING FROM RECS (Annual MMBTU per HH)
              XWATERHTGMMBTU=25.

              ! COMPUTE FUEL INPUT IN MMBTU
              XGASINPUT=.003412 * XKW(NT,NV)/XELEFF(NT,NV) * XOPERHOURS(NT) * XAVAIL(NT,NV)

              ! COMPUTE HEAT AVAILABLE FOR WATER HEATING IN MMBTU
              XBTUWASTEHEAT= (XGASINPUT-.003412 * XANNUALKWH)* XWHRECOVERY(NT,NV)
              IF(XBTUWASTEHEAT .LT. XWATERHTGMMBTU) XWATERHTGMMBTU=XBTUWASTEHEAT

              ! COMPUTE ANNUAL FUEL COST FOR FUEL CELL -- NET OF IMPUTED WATERHEATING COSTS
              XBASEYRFUELCOST = (XGASINPUT-XWATERHTGMMBTU) &
                *PNGRS(IDIV,ICURIYR)*MC_JPGDP(igencapcostyr-(baseyr-1))/MC_JPGDP(-2)

              ! Compare annual Fuel Cell generation to building use, value own-use at the retail price
              !  and grid sales at the grid price
              XEXCESSKWH=XANNUALKWH-XELECAVGUEC
              IF(XEXCESSKWH.LT.0.) THEN
                XVALESAVEBASE=XANNUALKWH*xRetailElecPr    ! own-use
               ELSE
                XVALESAVEBASE= XEXCESSKWH*xSalestoGridPR           &     !grid sales
                 +XELECAVGUEC*xRetailElecPR   !own-use
              ENDIF
              xCalcKW=xkw(nt,nv)
              XMAINTCOSTBASE=XMAINTCST(NT,NV) * xcalckw

200           CONTINUE

!----------------------------------------------------
!  CALCULATE PAYBACKS BY AVAILABLE TECHNOLOGY TYPE
!----------------------------------------------------

              ! IF(LPRINT)WRITE(DGDAT,*) ' CALCULATING SIMPLE PAYBACK'
              xcalceqcost = xeqcost*xcalckw/xkw(nt,nv)
              !   CALCULATE ANNUAL LEVELIZED PAYMENT
              xintrate=MC_RMMTG30CON(icuriyr)/100.
              XDOWNPAY=XDOWNPAYPCT*xCalcEqCost
              XPAYMENT=XINTRATE/(1.-(1.+XINTRATE)**(-1.*XTERM)) &
                             *(xCalcEqCost-XDOWNPAY)
              If(Lprint) WRITE(DGDAT,*) ' PAYMENT',XPAYMENT,' Int Rate',xintrate

              !   INITIALIZE CASH FLOW STARTING VALUES
              XCUMNETFLOW(1:35)=0.
              XOUTLAY(1)=XDOWNPAYPCT*xCalcEqCost
              XFUELCOST(1)=0.
              XFUELCOST(2)=XBASEYRFUELCOST
              XMAINTCOST(1)=0.
              XMAINTCOST(2)=XMAINTCOSTBASE
              XLOANBAL(1)=xCalcEqCost*(1.-XDOWNPAYPCT)
              XTAXCREDIT(1)=0.
              XTAXCREDIT(2)=0.
              XTAXCREDIT(3)=xCalcEqCost*XTAXCREDITPCT
              !apply cap if there is one
              if(xtaxcreditmaxKW .gt. 0.) XTAXCREDIT(3)=min(XTAXCREDIT(3),XTAXCREDITMAXKW*xcalckw)
              if(xtaxcreditmax .gt. 0.) XTAXCREDIT(3)=min(xtaxcredit(3),XTAXCREDITMAX)
                  ! Add census division subsidy if any for renewable technologies
              xtaxcredit(3)=xtaxcredit(3)+xCalcEqCost*xtxcrpct_div(nv,idiv,nt)                     ! 111dren
              XNETCASHFLOW(1)=-XOUTLAY(1)
              XCUMNETFLOW(1)=-XOUTLAY(1)
              iIntervalYrstoUse=iIntervalYrs(NT,NV)

              DO IYR=2,30
              XOUTLAY(IYR)=0.0
              XVALESAVE(IYR)=0.
              XKWH(IYR)=0.
              IF(FLOAT(IYR).LE.XTERM+1.)XOUTLAY(IYR)=XPAYMENT
              XINTAMT(IYR)=XLOANBAL(IYR-1)*XINTRATE
              XPRIN(IYR)=0.0
              IF(FLOAT(IYR).LE.XTERM+1.)XPRIN(IYR)=XPAYMENT-XINTAMT(IYR)
              XLOANBAL(IYR)=XLOANBAL(IYR-1)-XPRIN(IYR)
              !  CURRENTLY NO DEPRECIATION ALLOWANCE FOR RESIDENTIAL TAXES KEEP FOR GENERALITY
              !  XDEPR(IYR)=xCalcEqCost/XLIFE  ! STRAIGHT LINE DEPRECIATION
              XDEPR(IYR)=0.
              XTAXDEDUCT(IYR)=XTAXRATE*(XINTAMT(IYR-1)+XDEPR(IYR-1))+XTAXCREDIT(IYR)
              IF(IYR.GT.2) XFUELCOST(IYR)=0.
              IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                   XFUELCOST(IYR)=XFUELCOST(2)*(1.+XINFLATION)**(IYR-2)
              XMAINTCOST(IYR)=0.

              !Inverters:
              ! Calculate both annual as well as discrete maintenance costs.  Initially designed to accommodate
              !   discrete  Solar inverter replacements, this is also used for wind.
              IF(iyr.gt.2 .and. IMod(Iyr-2,iIntervalYrstoUse).eq.0 .and. Iyr.ne.29 .and. Iyr.ne.30) then
                 IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                   XMAINTCOST(IYR)=(XMAINTCOSTBASE+xIntervalCst(NT,NV)* xCalcKW)*(1.+XINFLATION)**(IYR-2)
                 !Adjust the interval years for subsequent (if needed) discrete replacement
                 !  The use of IYR reflects "average" progress in extending inverter lives for subsequent replacements
                 iIntervalYrstoUse=2*iIntervalYrstoUse+IYR
                 ! diagnostic only -- if (lprint3) write(dgdat,*) 'interval adjustment***  orig ', iyr, 'new ',iIntervalYrstoUse
                Else
                 IF(FLOAT(IYR).LE.(XLIFE+1.)) &
                   XMAINTCOST(IYR)=XMAINTCOSTBASE*(1.+XINFLATION)**(IYR-2)
              ENDIF

              IF(FLOAT(IYR).LE.(XLIFE+1.)) then
               XVALESAVE(IYR)=XVALESAVEBASE * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               ! Sunset the RPS if applicable
               If(nt.eq.1 .or. nt.eq.3) then
               IF(CURCALYR+iyr-1 .gt. iRPSPhaseOutYear) &
               XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               !Adjust
               IF(CURCALYR .ge. iRPSStartYear .and. iyr .gt. iNumYrsatRPSBaseRate(nt,icuriyr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEadjRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               !Remove Credits if number of years < 30
               If(CURCALYR .ge. iRPSStartYear .and. iyr .gt. iNumRPSCreditYrs(nt,icuriyr)) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
               !Remove Credits if CURCALYR is before start of RPS credits
               If(CURCALYR .lt. iRPSStartYear) &
                 XVALESAVE(IYR)=XVALESAVEBASEnoRPS * (1.+XINFLATION)**(IYR-2) * (1.-XDEGRADATION)**(IYR-2)
              endif
              endif

              IF(FLOAT(IYR).LE.(XLIFE+1.)) XKWH(IYR)=XANNUALKWH * &
                (1.-XDEGRADATION)**(IYR-2)
              XNETCASHFLOW(IYR)=-XOUTLAY(IYR)-XFUELCOST(IYR)-XMAINTCOST(IYR) &
                       +XTAXDEDUCT(IYR)+XVALESAVE(IYR)
              XCUMNETFLOW(IYR)=XCUMNETFLOW(IYR-1)+XNETCASHFLOW(IYR)
              ENDDO  !cash flow year loop (iyr)

             ! Print Switch for Detailed Cash Flow Model Results
             IF(LPRINT.and.LPRINT2.and.idiv.eq.1.and.iniche.eq.1.and.iratelevel.eq.1) &
              WRITE(DGDAT,*) 'YEAR    OUTLAY TAXDEDUCT  FUELCOST MAINTCOST    ESAVE  NETCASHFL   CUM   Annual kWh', AEQUIPNAME(NT,NV)

             IPAYBACK(1:30)=1

            ! SEARCH FOR POSITIVE CASH FLOW, PERSISTING FOR SEVERAL YEARS
            !  USE "0" TO INDICATE POSITIVE CF MEETING PERSISTENCE CRITERIA
            DO IYR=1,20 !stop at 20 because of look ahead - assume that in excess of 20 year paybacks are too long
            IF(XCUMNETFLOW(iyr  ).ge.0. .and. XCUMNETFLOW(iyr+1).ge.0. .and. &
             xcumnetflow(iyr+2).ge.0. .and. xcumnetflow(iyr+3).ge.0. .and. &
             xcumnetflow(iyr+4).ge.0. .and. xcumnetflow(iyr+5).ge.0. .and. &
             xcumnetflow(iyr+6).ge.0. .and. xcumnetflow(iyr+7).ge.0. .and. &
             xcumnetflow(iyr+8).ge.0.) IPAYBACK(IYR)=0
            ! SWITCHABLE DIAGNOSTICS:
             IF(LPRINT.and.LPRINT2.and.idiv.eq.1.and.iniche.eq.1.and.iratelevel.eq.1) &
              WRITE(DGDAT,10)IYR-1,XOUTLAY(IYR),XTAXDEDUCT(IYR),XFUELCOST(IYR), &
              XMAINTCOST(IYR), XVALESAVE(IYR), XNETCASHFLOW(IYR), &
              XCUMNETFLOW(IYR),XKWH(IYR),ipayback(iyr),xannualkwh
            ENDDO

 10   FORMAT(1X,I4,8F10.2,i4,f10.2)

            xsimplepayback=29.
            ilife=ifix(xlife)
            DO IYR=1,30
              if(ipayback(iyr).eq.0)then
                xSIMPLEPAYBACK=float(IYR-1)  !Allow 1 year and less simple paybacks
             !  Get the first year of a positive cumulative cash flow and
             !    compute how long at that year's net cash flow it would take to
             !    build the cumulative balance in the first positive year.
             !    The interpolated years to positive cash flow are then equal to
             !    the simple payback years minus ending cum cf balance / net cf in "iyr".
                If(xsimplepayback.lt.1.) xsimplepayback=1.
                If (iyr.lt.15) XSIMPLEPAYBACK= XSIMPLEPAYBACK - XCUMNETFLOW(IYR)/XNETCASHFLOW(IYR) !relax distributed generation cap in new
             !    Diagnostic warning
                  if (xsimplepayback .lt. 0.) then
                   WRITE(DGDAT,*) '**Negative Payback** CURCALYR,NT,NV,IDIV',CURCALYR,NT,NV,IDIV,'EQ CLASS ', &
                       AEQUIPNAME(NT,NV), '1ST YEAR ', IFIRSTYR(NT,NV), &
                      'PAYBACK=', XSIMPLEPAYBACK, 'XPEN= ',XPEN
                  endif
                goto 11  ! continue once payback is identified
              endif
             !          If here, investment never achieves positive CF, payback is set to 29 years
            ENDDO

!-------------END OF CALC PAYBACK

!--------------------------------------------------
!  CALCULATE PENETRATION BASED PAYBACK PERIOD
!--------------------------------------------------
11          Continue
            XMAXPEN=XPENPARM/XSIMPLEPAYBACK
            ! Maximum Penetration into New Construction Capped at 75%.
            ! The cap would affect projects with paybacks of less than approximately 5 months.
            XValue=float(curcalyr-(recsyear+1))
            if(XValue.gt.25.0) XValue=25.0  ! currently limit penetration beyond 2030
            XPEN=min(.75,XMAXPEN - xmaxpen/(1. + xmaxpen*EXP( XALPHA*(XValue-XSIMPLEPAYBACK))) )

            IF(LPRINT) WRITE(DGDAT,*) 'CURCALYR,NT,NV,IDIV',CURCALYR,NT,NV,IDIV,'EQ CLASS ', &
              AEQUIPNAME(NT,NV),'1ST YEAR ',IFIRSTYR(NT,NV),'PAYBACK=',XSIMPLEPAYBACK,'XPEN= ',XPEN

            !  CODE TO PRINT 20 YEARS OF PENETRATION DATA FOR TESTING
            !      DO IYR=curcalyr,curcalyr+20
            !          XPEN2=XMAXPEN-xmaxpen/(1.+xmaxpen*EXP(XALPHA*((iyr-(recsyear+1))-XSIMPLEPAYBACK)))
            !          IF(LPRINT)WRITE(DGDAT,'(1X,4I8,4e16.3)') curcalyr,recsyear,IYR,NV,xmaxpen,xalpha,XSIMPLEPAYBACK,XPEN2
            !          IF(LPRINT)WRITE(DGDAT,*) 'xpen2',curcalyr,recsyear,IYR,NV,xalpha,xmaxpen,XSIMPLEPAYBACK,XPEN2
            !      ENDDO

            ! Turn off endogenous builds to avoid double-counting of historical data
            ! (Increasing LASTSTEOYR in RESDREP include file for STEO benchmarking prior to updating
            !  exogenous capacity will cause PV to nearly flatten out between SEDS year and MER year)
              If(NT.eq.1 .and. curcalyr.lt.laststeoyr) xpen=0.0	!kj change to MSEDYR+2, which is the same as the current AEO year?  The previous year (MSEDYR+1) is typically the last year of historical exogenous capacity in RSGENTK.txt.  Even if history isn't updated yet, exogenous capacity should be set equal to the most recent historical value anyway.

            !  Account for penetration into existing housing units
            !  Penetration into existing is based on penetration into new construction with an assumed upper bound
            xExistPen=min(xPen/40.0,0.005)                               !Penetration cap for existing

            xtemp=XPEN*XINXDECAY(IDIV,ICURIYR)*HSEADD(CURCALYR,1,IDIV) &             !Penetration into New Construction !INXLIMIT
               +(XEXOGPEN(ICURIYR,IDIV,NT)-XEXOGPEN(ICURIYR-1,IDIV,NT))/xCalcKW  &   !Add Current Year Exogenous Units  !convert to kW
               +xExistPen*XINXDECAY(IDIV,ICURIYR)*(EH(CURCALYR,1,IDIV)-UNITS(ICURIYR-1,IDIV,NT))  !Existing Construction
            xtemp=xtemp*xHHShare(iDiv,iNiche,iRateLevel)                 !Scale down to suitable HH for niche share of HH
            xtemp = float(ifix(xtemp*100.+.5))/100.                      !Eliminate fractional units < .01
            xtempHH=(HSEADD(CURCALYR,1,IDIV)+EH(CURCALYR,1,IDIV))*xHHShare(iDiv,iNiche,iRateLevel)  !HH in niche
            !--END OF PENETRATION

            ! Accumulators for Technical Potential and Other Summary Statistics
            ! Note xSqftperKW varies by year and is calculated above
            If (nt==1) then
              SolarPVTechPotentialMW(icuriyr,IDIV)=SolarPVTechPotentialMW(icuriyr,IDIV)+ &
               (xtempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/(xSqftperKW*1000.))                     !PVgen
              SolarPVInstalledMW(icuriyr,IDIV)=SolarPVInstalledMW(icuriyr,IDIV)+xtemp*xCalcKW/1000.
              SolarPVAvailRoofArea(icuriyr,IDIV)= SolarPVAvailRoofArea(icuriyr,IDIV) + &
               (xtempHH*xpctPVSuitable*xRoofAreaperHH(iDiv,iNiche,iRateLevel)/10.**6)                                 !PVgen
              SolarPVUsedRoofArea(icuriyr,IDIV)=SolarPVUsedRoofArea(icuriyr,IDIV)+(xtemp*xCalcKW*xSqftperKW/10**6)    !PVgen
            Endif
            If (nt==3) then
             WindTechPotentialMW(icuriyr,IDIV)=WindTechPotentialMW(icuriyr,IDIV)+xtempHH*xCalcKW/1000.*xRuralPctHH(iDiv,iNiche,iRateLevel)
             WindInstalledMW(icuriyr,IDIV)=WindInstalledMW(icuriyr,IDIV)+xtemp*xCalcKW/1000.
             WindAvailHH(icuriyr,IDIV)=WindAvailHH(icuriyr,IDIV)+xtempHH*xRuralPctHH(iDiv,iNiche,iRateLevel)
            Endif
            !--End Summary Calculations

            ! Now that new units are determined, calculate associated estimates of generation in trills, capacity,
            !   own use generation, fuel usage, offsets to energy consumption for hot water & space heating
            !   and investment.
            xunits = xunits + xtemp
            xtrills = xtrills + XTEMP*XANNUALKWH*3412./10.**12 !trills
            xcapacity = xcapacity + xtemp*xCalcKW           !in KW for now
            IF(XANNUALKWH.GT.XELECAVGUEC) THEN
              xtrillsownuse = xtrillsownuse + XTEMP*XELECAVGUEC*3412./10.**12
             ELSE
              ! BUILDING CONSUMES ALL OF ITS OWN GENERATION
              xtrillsownuse = xtrillsownuse + XTEMP*XANNUALKWH*3412./10.**12
            ENDIF
           xfuelusage = xfuelusage + XTEMP*xgasinput/10.**6   !trills
           xhwbtu = xhwbtu +XTEMP*XWATERHTGMMBTU/10.**6       !trills
           xinvest = xinvest + XTEMP*xCalcEqCost/10.**6       !$Millions

           ! Label 25 for skipping unpopulated niches
 25        Continue
         End Do  !Rate Levels (iRateLevel)
       End Do  !Climate Zone Niches (iNiche)

       GOTO 81  !SKIP OVER NEW PV CODE IF HERE

 26       Continue

!--------------------------------------------
!  NEW ZIP CODE-BASED PV MODEL        !PVPen
!--------------------------------------------

          ! THIS IS JUST OUTSIDE OF THE RATE LEVEL, CLIMATE ZONE NICHE LOOPS
          !  ALL RESULTS HERE ARE FOR CENSUS DIVISIONS
          ! USENEWMODEL IF HERE
          xSqftperKW=77.*.14/XELEFF(NT,NV)

          !National and Division-level input variables
          IF(curcalyr.gt.EstYear) then
            INTRATE=(MC_RMMTG30CON(icuriyr)/100.)            !Convert to a decimal fraction
      !111dren add the divisional tax credit into the calculation of PVPRICE in the line of code below
             !PVPRICE equals RSGENTK installed cost [Equip Cost] (net of any tax credit) times 0.96, an adjustment factor to scale to the econometric model's cost level [PVPrice]
              !For AEO2017, $4,568/1,000*(1-0.30 ITC)=$3.20.  $3.08/$3.20=0.96
             PVPRICE=(xadjcost+XINSTCOST(NT,NV))*(1.-xtaxcreditpct-xtxcrpct_div(nv,idiv,nt) )*0.96
             MONTHLYPAYMENT= ( PVPRICE * (INTRATE/12.) / (1.-(1.+INTRATE/12.)**(-360.)) ) !assume a 360 month mortgage
            !write(9,*) 'USING NEW MODEL National Level Variables ', IntRate, PVPrice, MonthlyPayment
          ENDIF

          !Process ZipCodes
           DO i=1,numzips
             IF(CenDiv(i) .NE. idiv) CYCLE
             CumUnits(i) = 0.0
             xSolarIns=INSOL(i)                                                                      !PVgen
             ! transform solar insolation to account for unknown orientations                        !PVgen
             xSolarIns= -1.0533 + 1.4325*xSolarIns - 0.0652*xSolarIns**2                             !PVgen

             xcalcKW=xkw(nt,nv)  !set size to menu capacity

             XANNUALKWH=XELEFF(NT,NV)*xSolarIns*365.25/10.8*xSqftperKW*xCalcKW*XLOSSFAC(NT,NV)

             ! j is the model switch based on the 3 sets of coefficients
             !   if purehurdle is zero, then j = 1 using the first subscript position for coefficients
             !   if purehurdle is 1, then j = 2 using the 2nd subscript position for coefficients (urban model)
             !   if purehurdle is 0, and ruralzip is 1, then j = 3 using the 3rd subscript position for coefficients (rural model)
             j = purehurdle(i)+1
             IF (ruralzip(i) .eq. 1) j=3
             ! The first array element of the coefficient is for the logit model, the 2nd for the negative binomial.
             ! Not all variables for specific model variants are non-zero, but this is programmed to allow flexible
             !   model evolution.
             ! Set Inputs to Equations:
                IF(curitr.eq.1 .and. curcalyr.gt.EstYear) then  !Subsequent to the estimation year update lag variables on first iteration
                  Income_L(i)=Income(i)
                  Households_L(i)=Households(i)
                  ElecRate_L(i)=ElecRate(i)
                  PopDensity_L(i)=PopDensity(i)
                  Lag2Installs(i)=Lag1Installs(i)
                  Lag1Installs(i)=ProjectedInstalls(i)
                ENDIF !curitr=1 for years after econometric model estimation year

                IF(curcalyr.gt.EstYear) then  !Subsequent to the estimation year update zip code level variables
                  Income(i)=Income_L(i)*(MC_YPDR(CenDiv(i),icuriyr)/MC_YPDR(CenDiv(i),icuriyr-1)) / &
                    ( ( EH(curcalyr,1,idiv)+NH(curcalyr,1,idiv)+EH(curcalyr,2,idiv)+NH(curcalyr,2,idiv)+EH(curcalyr,3,idiv)+NH(curcalyr,3,idiv) ) &
                    / ( EH(curcalyr-1,1,idiv)+NH(curcalyr-1,1,idiv)+EH(curcalyr-1,2,idiv)+NH(curcalyr-1,2,idiv)+EH(curcalyr-1,3,idiv)+NH(curcalyr-1,3,idiv) ) )
                  Households(i)=Households_L(i)*(EH(curcalyr,1,idiv)+NH(curcalyr,1,idiv)+EH(curcalyr,2,idiv)+NH(curcalyr,2,idiv)+EH(curcalyr,3,idiv)+NH(curcalyr,3,idiv)) &
                    /(EH(curcalyr-1,1,idiv)+NH(curcalyr-1,1,idiv)+EH(curcalyr-1,2,idiv)+NH(curcalyr-1,2,idiv)+EH(curcalyr-1,3,idiv)+NH(curcalyr-1,3,idiv))
                  ElecRate(i)=ElecRate_L(i)*( (PELRSOUT(CenDiv(idiv),icuriyr,2)*.003412+xRPS(nt,icuriyr)*EPRPSPR(icuriyr)/1000. ) ) / &    !own-use including scaled RPS credit
                    ( (PELRSOUT(CenDiv(idiv),icuriyr-1,2)*.003412+xRPS(nt,icuriyr-1)*EPRPSPR(icuriyr-1)/1000. ) )
                  PopDensity(i)=PopDensity_L(i)*MC_NP65A(icuriyr)/MC_NP65A(icuriyr-1)
                ENDIF

             !Calculate numerator and denominator components for projections (these are Real*8 to prevent overflows)
               xlogit= cint (1,j) + chh (1,j)*HouseHolds(i) +cpd  (1,j)*PopDensity(i)   +cinc (1,j)*Income(i) + &
                       cer (1,j)*ElecRate(i)   +ccdd (1,j)*lagcdd(i)       +cpmt (1,j)*MONTHLYPAYMENT  + &
                       cir (1,j)*INTRATE       +clag1(1,j)*lag1Installs(i) +clag2(1,j)*lag2Installs(i) + &
                       cpvp(1,j)*PVPRICE       +cins (1,j)*Insol(i)
               xnegbinom= cint (2,j) + chh (2,j)*HouseHolds(i) +cpd  (2,j)*PopDensity(i)   +cinc (2,j)*Income(i) + &
                       cer (2,j)*ElecRate(i)   +ccdd (2,j)*lagcdd(i)       +cpmt (2,j)*MONTHLYPAYMENT  + &
                       cir (2,j)*INTRATE       +clag1(2,j)*lag1Installs(i) +clag2(2,j)*lag2Installs(i) + &
                       cpvp(2,j)*PVPRICE       +cins (2,j)*Insol(i)

             !Compute hurdle model additions to PV households with a test for overflows
               IF(isnan(exp(xlogit+xnegbinom) / ( 1. + exp(xlogit) )) ) then
                 xtemp=0.
                 !write(9,*) 'USING NEW MODEL :: NAN encountered at zip ', state(i),zipcode(i),xlogit,xnegbinom,xtemp
                 !write(9,*) '    Model, HH, PopDens, Insol, ElecRate ',   j,households(i),popdensity(i),insol(i),elecrate(i)
                 !write(9,*) '    Coefficients logit',  chh(1,j), cpd(1,j),cins(1,j),cer(1,j)
                 !write(9,*) '    Coefficients binom',  chh(2,j), cpd(2,j),cins(2,j),cer(2,j)
               ELSE
                 xtemp= real(  exp(xlogit+xnegbinom) / ( 1. + exp(xlogit) )  )
                 IF(curcalyr .gt. EstYear) xtemp= xtemp*xinxdecay(idiv,icuriyr)
                 ENDIF
                 IF(isnan(xtemp)) THEN
                   !print warning message somewhere, for now:
                   !write(9,*) ':: NAN encountered at zip ',zipcode(i)
                   xtemp=0.   !reset xtemp before additional use
                 ENDIF
                 ProjectedInstalls(i)=xtemp  !save this year's projected installs for setting lags in next projection year
                 xunits=xunits + xtemp       !accumulate zip code results to the census division level
                 CumUnits(i)=CumUnits(i)+xtemp !also accumulate units within a zip code for testing penetration level
              !If penetration exceeds a particular value back out additional units
                IF(CumUnits(i)/Households(i) .gt. 0.80) THEN !80% is limit on share of households that can have PV in each zip code
                  xunits=xunits-xtemp
                  CumUnits(i)=CumUnits(i)-xtemp
                  xtemp=0.
                ENDIF

             !Temporary Test for Consistency with R Coded Model
               IF(curcalyr .eq. EstYear) THEN
                 IF (xtemp-ModelInstalls(i) .gt. .001) THEN
                   !Write(9,*) ' model disagreement for zip ',state(i),zipcode(i), xtemp, modelinstalls(i)
                   !write(9,*) ' xlogit, xnegbinom, xtemp ', state(i),zipcode(i),xlogit,xnegbinom,xtemp
                   !write(9,*) ' Model, HH, PopDens, Insol, ElecRate ',   j,households(i),popdensity(i),insol(i),elecrate(i)
                   !write(9,*) ' Coefficients logit',  chh(1,j), cpd(1,j),cins(1,j),cer(1,j)
                   !write(9,*) ' Coefficients binom',  chh(2,j), cpd(2,j),cins(2,j),cer(2,j)
                 ENDIF
               ENDIF

             !Add tax credit for penetration calculation of payment (?)
              xtrills= xtrills + xtemp*xannualkwh*3412./10.**12  !beware of mixed mode: bad results if coded as "/10**12" !!!

            XELECAVGUEC= 0.
            XELECAVGUEC= ((QELRS(IDIV,ICURIYR) * 10.**12) / 3412.) / (EH(CURCALYR,1,IDIV) + NH(CURCALYR,1,IDIV) + EH(CURCALYR,2,IDIV) +  &          !PVownuse
                          NH(CURCALYR,2,IDIV) + EH(CURCALYR,3,IDIV) + NH(CURCALYR,3,IDIV))  !Average electricity consumption (kWh) per household    !PVownuse

            IF(XANNUALKWH.GT.XELECAVGUEC) THEN  !If PV generation exceeds average consumption per household...    !PVownuse
              xtrillsownuse = xtrillsownuse + XTEMP*XELECAVGUEC*3412./10.**12    !PVownuse
             ELSE                                                                !PVownuse
              ! BUILDING CONSUMES ALL OF ITS OWN GENERATION                      !PVownuse
              xtrillsownuse = xtrillsownuse + XTEMP*XANNUALKWH*3412./10.**12     !PVownuse
            ENDIF                                                                !PVownuse

            IF (xtrillsownuse .GT. xtrills) THEN                                 !PVownuse
             !Prevents negative sales to grid                                    !PVownuse
             xtrillsownuse= xtrills                                              !PVownuse
            ENDIF                                                                !PVownuse

           ENDDO !Process ZipCodes

          !WRITE(9,*) 'Using New Model', curcalyr, idiv, xunits, xtrills
          ! CREATE CENSUS DIVISION RESULTS FOR PV (NT==1) HERE
          UNITS(ICURIYR,IDIV,NT)=UNITS(ICURIYR-1,IDIV,NT)+ xunits
          xcapacity=xunits*xcalcKW  !in kW
          CAP(ICURIYR,IDIV,NT)=CAP(ICURIYR-1,IDIV,NT)+ xcapacity

          !xtrills= xunits*xannualkwh*3412./10.**12  !insert NREL equation this is a per kW calculation so multiply by above and convert to trills
          TRILLS(ICURIYR,IDIV,NT)=TRILLS(ICURIYR-1,IDIV,NT)+ xtrills
!          xtrillsownuse=xtrills   !placeholder; base this on RECS by Census division  !This placeholder was never filled.  Calculations added above  !PVownuse

          TRILLSOWNUSE(ICURIYR,IDIV,NT)= TRILLSOWNUSE(ICURIYR-1,IDIV,NT) +xtrillsownuse
          xinvest=xcapacity*xeqcost/10.**6
          x111drensub(icuriyr,idiv,nt)=xinvest*xtxcrpct_div(nv,idiv,nt)  !111dren

          INVEST(ICURIYR,IDIV,NT)=xinvest  !($mill)

          IF (ICURIYR .LE. EstYear-BASEYR+2 .AND. NT .EQ. 1) THEN    !Used to calibrate to historical exogenous PV capacity in EstYear and the following year    !PVzipcalib
           ExogPVMistie(ICURIYR,IDIV)= 0.                                                                                                                        !PVzipcalib
           !XEXOGPEN is exogenous PV capacity (NT=1) in kW from RSGENTK in index year (EstYear-BASEYR+1; 2014=25)                                                !PVzipcalib
           ExogPVMistie(ICURIYR,IDIV)= XEXOGPEN(ICURIYR,IDIV,1) - CAP(ICURIYR,IDIV,1)                                                                            !PVzipcalib
!           WRITE(9,*)  'ExogPVMistie_Test_1', ICURIYR, IDIV, XEXOGPEN(ICURIYR,IDIV,1), CAP(ICURIYR,IDIV,1), ExogPVMistie(ICURIYR,IDIV), XUNITS, &                !PVzipcalib
!            XCALCKW, xannualkwh, XELECAVGUEC, ( EH(CURCALYR,1,IDIV) + NH(CURCALYR,1,IDIV) + EH(CURCALYR,2,IDIV) + NH(CURCALYR,2,IDIV) + EH(CURCALYR,3,IDIV) + &  !PVzipcalib
!            NH(CURCALYR,3,IDIV)), QELRS(IDIV,ICURIYR), TRILLS(ICURIYR,IDIV,1), TRILLSOWNUSE(ICURIYR,IDIV,1)                                                      !PVzipcalib
           CAP(ICURIYR,IDIV,1)= CAP(ICURIYR,IDIV,1) + ExogPVMistie(ICURIYR,IDIV)                                                                                 !PVzipcalib
           UNITS(ICURIYR,IDIV,1)= UNITS(ICURIYR,IDIV,1) + (ExogPVMistie(ICURIYR,IDIV) / xcalcKW)                                                                 !PVzipcalib
           xcapacity=xunits*xcalcKW                                                                                                                              !PVzipcalib
           xinvest=xcapacity*xeqcost/10.**6                                                                                                                      !PVzipcalib
           x111drensub(icuriyr,idiv,1)=xinvest*xtxcrpct_div(nv,idiv,1)                                                                                           !PVzipcalib
           TRILLS(ICURIYR,IDIV,1)= TRILLS(ICURIYR,IDIV,1) + (ExogPVMistie(ICURIYR,IDIV) / xcalcKW) *xannualkwh*3412./10.**12                                     !PVzipcalib
           !Share out generation of ExogPVMistie to self-use generation rather than putting it all into both trills and trillsownuse                             !PVzipcalib
           TRILLSOWNUSE(ICURIYR,IDIV,1)= TRILLSOWNUSE(ICURIYR,IDIV,1) + (TRILLSOWNUSE(ICURIYR,IDIV,1)/TRILLS(ICURIYR,IDIV,1)) * &                                !PVzipcalib
            (ExogPVMistie(ICURIYR,IDIV) / xcalcKW) *xannualkwh*3412./10.**12                                                                                     !PVzipcalib
           INVEST(ICURIYR,IDIV,NT)=xinvest  !($mill)                                                                                                             !PVzipcalib
          ENDIF                                                                                                                                                  !PVzipcalib

          IF (ICURIYR .GT. EstYear-BASEYR+2 .AND. NT .EQ. 1) THEN    !Apply historical exogenous PV capacity mistie to remainder of projection years             !PVzipcalib
           CAP(ICURIYR,IDIV,1)= CAP(ICURIYR,IDIV,1) + ExogPVMistie(EstYear-BASEYR+2,IDIV)                                                                        !PVzipcalib
           UNITS(ICURIYR,IDIV,1)= UNITS(ICURIYR,IDIV,1) + (ExogPVMistie(EstYear-BASEYR+2,IDIV) / xcalcKW)                                                        !PVzipcalib
           xcapacity=xunits*xcalcKW                                                                                                                              !PVzipcalib
           xinvest=xcapacity*xeqcost/10.**6                                                                                                                      !PVzipcalib
           x111drensub(ICURIYR,IDIV,1)=xinvest*xtxcrpct_div(NV,IDIV,1)                                                                                           !PVzipcalib
           TRILLS(ICURIYR,IDIV,1)= TRILLS(ICURIYR,IDIV,1) + (ExogPVMistie(EstYear-BASEYR+2,IDIV) / xcalcKW) *xannualkwh*3412./10.**12                            !PVzipcalib
           !Share out generation of ExogPVMistie to self-use generation rather than putting it all into both trills and trillsownuse                             !PVzipcalib
           TRILLSOWNUSE(ICURIYR,IDIV,1)= TRILLSOWNUSE(ICURIYR,IDIV,1) + (TRILLSOWNUSE(ICURIYR,IDIV,1)/TRILLS(ICURIYR,IDIV,1)) * &                                !PVzipcalib
            (ExogPVMistie(EstYear-BASEYR+2,IDIV) / xcalcKW) *xannualkwh*3412./10.**12                                                                            !PVzipcalib
           INVEST(ICURIYR,IDIV,NT)=xinvest  !($mill)                                                                                                             !PVzipcalib
          ENDIF                                                                                                                                                  !PVzipcalib

      GOTO 82  !DON'T REDO DIVISION LEVEL CALCULATIONS FOR PV IF USING ECONOMETRIC MODEL

!--------------------------------------------------
!  END OF NEW ZIP CODE BASED PV MODEL        !PVPen
!--------------------------------------------------

 81    CONTINUE  !END OF NEW PV CODE
       !  DIVISION-LEVEL CALCS IF USING THE NICHE MODEL INSTEAD OF THE ECONOMETRIC MODEL
       !      IF(LPRINT)WRITE(DGDAT,*) 'KWH ',XANNUALKWH
       !      IF(LPRINT)WRITE(DGDAT,*) 'XTEMP ', XTEMP, 'XPEN ',XPEN
       !      IF(LPRINT)WRITE(DGDAT,*) 'HSEADD ',HSEADD(CURCALYR,1,IDIV)
       !      IF(LPRINT)WRITE(DGDAT,*) 'EXOG PEN ' , XEXOGPEN(ICURIYR,IDIV,NT)

          UNITS(ICURIYR,IDIV,NT)=UNITS(ICURIYR-1,IDIV,NT)+ xunits
          CAP(ICURIYR,IDIV,NT)=CAP(ICURIYR-1,IDIV,NT)+ xcapacity
          TRILLS(ICURIYR,IDIV,NT)=TRILLS(ICURIYR-1,IDIV,NT)+ xtrills
          TRILLSOWNUSE(ICURIYR,IDIV,NT)= TRILLSOWNUSE(ICURIYR-1,IDIV,NT) +xtrillsownuse
          GASUSAGE(ICURIYR,IDIV,NT)=GASUSAGE(ICURIYR-1,IDIV,NT)+ xfuelusage
          HWBTU(ICURIYR,IDIV,NT)=HWBTU(ICURIYR-1,IDIV,NT)+ xhwbtu
          INVEST(ICURIYR,IDIV,NT)=xinvest  !($mill)
          x111drensub(icuriyr,idiv,nt)=xinvest*xtxcrpct_div(nv,idiv,nt)  !111dren
 82   CONTINUE
       !-------------END OF PENETRATION CALCULATIONS

!----------------------------------------------------------------------------------------
! More RPS Calculations to Transfer the Composite Bonus Credits to the Electricity Market Module
!----------------------------------------------------------------------------------------
        If(NT.eq.1) then
         xPVGenAdded(IDIV,ICuriyr)= xtrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         Do iyr=RECSYEAR-(baseyr-1),ICuriyr
          iCalYR=iyr+(baseyr-1)
          xBonus=0.
          xCompGen=xCompGen+xPVGenAdded(IDIV,iyr)
          if(CURCALYR .lt. iRPSStartYear .and. iCalYR .ge. iRPSGrandFatherYear) then  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,icuriyr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
          else
            if(ICuriyr .lt. iyr+iNumYrsatRPSBaseRate(NT,iyr) .and.  CURCALYR .lt. iRPSPhaseOutYear) then
             xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
!            WRITE (dgdat,*) 'test icuriyr iyr inumyrs at rpsbase', icuriyr, iyr, inumyrsatrpsbaserate(nt,iyr)
            else
             if(ICuriyr .lt. iyr+iNumRPSCreditYrs(NT,iyr) .and.  CURCALYR .lt. iRPSPhaseOutYear) &
               xBonus=xRPS(NT,icuriyr)-1.0 !Give the current year credit if beyond the number of base credit years
            endif
          endif
          xCompCredit=xCompCredit+xPVGenAdded(IDIV,iyr)*(xCredit+xBonus)
!         Debugging Use If (LPrint .and. (IDIV < 2) WRITE (dgdat,*) 'RPS Calcs PV', iyr, xPVGenAdded(IDIV,iyr), xcredit+xbonus
         Enddo
         If(xCompGen .gt. 0.) CGCPVRES(IDIV,ICuriyr)=xCompCredit/xCompGen
         If (LPrint .and. IDIV < 10) WRITE (dgdat,'("RPS Calcs PV",3i6,2F12.5)') icuriyr, curcalyr, idiv, CGCPVRes(IDIV,ICuriyr), EPRPSPR(ICURIYR)
        Endif !nt=1

         If(NT.eq.3) then
         xWindGenAdded(IDIV,ICuriyr)= xtrills
         xCompCredit=0.
         xCompGen=0.
         ! Accumulate Credits
         xCredit=1.0 !Minimum credit multiplier is 1.
         Do iyr=RECSYEAR-(baseyr-1),ICuriyr
           iCalYR=iyr+(baseyr-1)
           xBonus=0.
            xCompGen=xCompGen+xWindGenAdded(IDIV,iyr)
            if(CURCALYR .lt. iRPSStartYear .and. iCALYR .ge. iRPSGrandFatherYear) then  !no bonus for equipment in service before grandfather year
                xbonus=xRPS(NT,icuriyr)-1.0  !give the current year credit to renewable DG placed in service before the legislation
            else
              if(ICuriyr .lt. iyr+iNumYrsatRPSBaseRate(NT,iyr) .and.  CURCALYR .lt. iRPSPhaseOutYear) then
                   xBonus=xRPS(NT,iyr)-1.0  !give the base year credit for capacity added in iyr for iNumYrsatRPSBaseRate of years
!                  WRITE (dgdat,*) 'test icuriyr iyr inumyrs at rpsbase', icuriyr, iyr, inumyrsatrpsbaserate(nt,iyr)
               else
                   if(ICuriyr .lt. iyr+iNumRPSCreditYrs(NT,iyr) .and.  CURCALYR .lt. iRPSPhaseOutYear) &
                     xBonus=xRPS(NT,icuriyr)-1.0 !Give the current year credit if beyond the number of base credit years
               endif
            endif
         xCompCredit=xCompCredit+xWindGenAdded(IDIV,iyr)*(xCredit+xBonus)
!        Debugging Use If (LPrint .and. IDIV < 2) WRITE (dgdat,*) 'RPS Calcs Wind', iyr, xWindGenAdded(IDIV,iyr), xcredit+xbonus
         Enddo
         If(xCompGen .gt. 0.) CGCWNRES(IDIV,ICuriyr)=xCompCredit/xCompGen
         If (LPrint .and. IDIV < 10) WRITE (dgdat,'("RPS Calcs WN",3i6,2F12.5)') icuriyr, curcalyr, idiv, CGCWNRes(IDIV,ICuriyr), EPRPSPR(ICURIYR)
        Endif !nt=3

!--------------------------------------------------
!  CHECK TECHNICAL POTENTIAL
!--------------------------------------------------
        ! Accumulators for Technical Potential and Other Summary Statistics
        ! Note xSqftperKW varies by year and is calculated above
        If (nt==1) then
           SolarPVInstalledMW(icuriyr,IDIV)=SolarPVInstalledMW(icuriyr,IDIV)+SolarPVInstalledMW(icuriyr-1,IDIV)
           SolarPVUsedRoofArea(icuriyr,IDIV)=SolarPVUsedRoofArea(icuriyr,IDIV)+SolarPVUsedRoofArea(icuriyr-1,IDIV)
        Endif
        If (nt==3) then
           WindInstalledMW(icuriyr,IDIV)=WindInstalledMW(icuriyr,IDIV)+WindInstalledMW(icuriyr-1,IDIV)
        Endif

        END DO      ! END CENSUS DIVISION LOOP IDIV

 66     CONTINUE    ! CYCLE THROUGH VINTAGES THAT DON'T APPLY

       ENDDO      ! END TECHNOLOGY VINTAGE LOOP NV
      ENDDO      ! END TECHNOLOGY TYPE LOOP NT


!--------------------------------------------------
!  CHECK EPACT 2005 SUBSIDY PAYMENTS
!--------------------------------------------------
      ! Energy Bill 2005 Code Changes FOR PV Rebate Authorization
      IF (NRGBILL.LT.2) GO TO 93 !Check Energy Bill Number of Rebated Systems
      IF (LPRINT) WRITE(DGDAT,*) 'DBUG Units for ',curcalyr, icuriyr
      !Assume the maximum rebate of $3000 is achieved for a 1.5-2 kW system.
      !So in each of the years targeted by the Energy Bill, the total systems
      !  subsidized equal the funding($000) for that year divided by $3k.
      !Check for Energy Bill Subsidized Years
      IF (CURCALYR.GE.2006 .AND. CURCALYR.LE.2008) THEN
        IF (CURCALYR.EQ.2006)XMAXPEN=150000./3.
        IF (CURCALYR.EQ.2007)XMAXPEN=150000./3.
        IF (CURCALYR.EQ.2008)XMAXPEN=200000./3.
      ! Superseded by new bill (2009 forward)
      !        IF (CURCALYR.EQ.2009)XMAXPEN=250000./3.
      !        IF (CURCALYR.EQ.2010)XMAXPEN=250000./3.
      ! First Sum PV Units
       NT=1  !PV only
       XTEMP=0.
       DO IDIV=1,NUMDIV
        XTEMP = XTEMP + UNITS(ICURIYR,IDIV,NT)-UNITS(ICURIYR-1,IDIV,NT)
       ENDDO
       IF (LPRINT)WRITE(DGDAT,*) 'DBUG Units for ',curcalyr, icuriyr, xtemp

       !For Energy Bill affected years, there is no "economic" penetration into residential
       ! -- i.e., virtually all penetration occurs only due to exogenous program builds and subsidies.
       !In case more systems "penetrate" than are available for funding, scale back
       !  to be consistent with the funding:
       IF (XTEMP > XMAXPEN) THEN
        !Notify if scaling.
        IF(LPRINT) WRITE(DGDAT,*)'EBill ScaleDown ',curcalyr,'Economic Units: ',xtemp,'EBill Units :',xmaxpen
         DO IDIV=1,NUMDIV
           TRILLS(ICURIYR,IDIV,NT)=TRILLS(ICURIYR-1,IDIV,NT)+ &
            XMAXPEN/XTEMP*(TRILLS(ICURIYR,IDIV,NT)-TRILLS(ICURIYR-1,IDIV,NT))
           TRILLSOWNUSE(ICURIYR,IDIV,NT)=TRILLSOWNUSE(ICURIYR-1,IDIV,NT)+ &
             XMAXPEN/XTEMP*(TRILLSOWNUSE(ICURIYR,IDIV,NT)-TRILLSOWNUSE(ICURIYR-1,IDIV,NT))
           UNITS(ICURIYR,IDIV,NT)=UNITS(ICURIYR-1,IDIV,NT)+ &
              XMAXPEN/XTEMP*(UNITS(ICURIYR,IDIV,NT)-UNITS(ICURIYR-1,IDIV,NT))
           CAP(ICURIYR,IDIV,NT)=CAP(ICURIYR-1,IDIV,NT)+   &
             XMAXPEN/XTEMP*(CAP(ICURIYR,IDIV,NT)-CAP(ICURIYR-1,IDIV,NT))
           INVEST(ICURIYR,IDIV,NT)= INVEST(ICURIYR,IDIV,NT)* XMAXPEN/XTEMP
         ENDDO
        ENDIF  !Penetration Exceeds Subsidized Systems

        ENDIF       !Check for Energy Bill Subsidized Years

93      CONTINUE

        !  MORE DIAGNOSTIC PRINTING
          DO IDIV=1,NUMDIV
            DO NT=1,NUMTECHS
              QRSDGSG(ICURIYR,IDIV)=QRSDGSG(ICURIYR,IDIV)+ &
               TRILLS(ICURIYR,IDIV,NT)-TRILLSOWNUSE(ICURIYR,IDIV,NT)
               ! IF(LPRINT.and.LPRINT2) WRITE(DGDAT,*)QRSDGSG(ICURIYR,IDIV),
               ! TRILLS(ICURIYR,IDIV,NT),TRILLSOWNUSE(ICURIYR,IDIV,NT)
            ENDDO  ! TECHNOLOGIES
              IF(LPRINT.and.LPRINT2) THEN
                WRITE(DGDAT,*) ' DIV       GRID SALES'
                WRITE(DGDAT,67) IDIV,QRSDGSG(ICURIYR,IDIV)
              ENDIF
          ENDDO  ! DIVISIONS

!---------------------------------------------
!  CALCULATE OUTPUTS TO NEMS AND RESD
!---------------------------------------------

       !LOAD Arrays for Passing Data to the Electric Utility Module
       !Calculate Base Year Cogeneration Capacity by Census division,
       ! Building Type and Fuel;
       !Also Populate Common Block Variables for DG Learning
       !Initialize learning variables
       rFuelCell_MW(icuriyr)=0.
       rPV_MW(icuriyr)=0.
       rMicroTur_MW(icuriyr)=0.
       rWind_MW(icuriyr)=0.

       Do r=1,numdiv  !Census Division Loop for Populating Arrays for Utility Module

       !Initialize Output Arrays for Linking to the Utility Module
       ! The index f(1:10) is the cogeneration fuel numbering scheme for the Utility Module Link Array:
       !            f=3, natural gas corresponds to nt=2, fuel cells
       !            f=8, solar corresponds to nt=1
       !            f=11, wind corresponds to nt=3
       !         Currently unused, but present in the Utility Module Link Array of the cogen common block:
       !            f=1, coal
       !            f=2, distillate and or residual fuel oil
       !            f=4, hydro
       !            f=5, geothermal
       !            f=6, MSW
       !            f=7, biomass
       !            f=10, other
       !            f=9, other gaseous
       !            f=10, other
       !            f=12, solar thermal
       !
       ! Cogeneration Fuel Consumption in Trills
       CGRESQ (r,iCURIYR,1:12)= 0.0
       ! Generation in GWh by Capacity Type and Grid Sales(1) versus Own Use(2)
       CGRESGEN (r,iCURIYR,1:12,1:2)= 0.0
       ! Capacity in MW
       CGRESCAP(r,icuriyr,1:12)=0.

       ! Cogen Capacity MW
       CGRESCAP (r,icuriyr,3)= CGRESCAP (r,icuriyr,3) &
             + CAP(icuriyr,r,2)/1000. !natural gas-fired fuel cells
       CGRESCAP (r,icuriyr,8)= CGRESCAP (r,icuriyr,8) &
             + CAP(icuriyr,r,1)/1000. !solar PV
       CGRESCAP (r,icuriyr,11)= CGRESCAP (r,icuriyr,11) &
             + CAP(icuriyr,r,3)/1000. !wind

       ! Populate learning common block variables in MW
       rPV_MW(icuriyr)=      rPV_MW(icuriyr)       + cap(icuriyr,r,1)/1000.
       rFuelCell_MW(icuriyr)=rFuelCell_MW(icuriyr) + cap(icuriyr,r,2)/1000.
       rWind_MW(icuriyr)=    rWind_MW(icuriyr)     + cap(icuriyr,r,3)/1000.

       ! Cogen Electricity Generated in GWH (1=grid sales, 2=own use)
       !  Grid Sales:
       CGRESGEN (r,icuriyr,3,1)= CGRESGEN (r,icuriyr,3,1)     &     ! Natural Gas
              + (Trills(icuriyr,r,2) - TrillsOwnUse(icuriyr,r,2)) & ! Grid Sales = total generation - own use
              * (1000./3.412)                                       ! GWH/trill conversion

       CGRESGEN (r,icuriyr,8,1)= CGRESGEN (r,icuriyr,8,1)     &     ! Solar PV
              + (Trills(icuriyr,r,1) - TrillsOwnUse(icuriyr,r,1)) &
              * (1000./3.412)                                       ! GWH/trill conversion

       CGRESGEN (r,icuriyr,11,1)= CGRESGEN (r,icuriyr,11,1)     &   ! Wind
              + (Trills(icuriyr,r,3) - TrillsOwnUse(icuriyr,r,3)) &
              * (1000./3.412)                                       ! GWH/trill conversion

       !  Own Use:
       CGRESGEN (r,icuriyr,3,2)= CGRESGEN (r,icuriyr,3,2)      &    ! Natural Gas
              + TrillsOwnUse(icuriyr,r,2) &
              * (1000./3.412)                                       ! GWH/trill conversion

       CGRESGEN (r,icuriyr,8,2)= CGRESGEN (r,icuriyr,8,2)      &    ! Solar PV
              + TrillsOwnUse(icuriyr,r,1) &
              * (1000./3.412)                                       ! GWH/trill

       CGRESGEN (r,icuriyr,11,2)= CGRESGEN (r,icuriyr,11,2)    &    ! Wind
              + TrillsOwnUse(icuriyr,r,3) &
              * (1000./3.412)                                       ! GWH/trill

       ! Fuel Consumption for Cogen in Trills/
           CGRESQ (r,icuriyr,3)= CGRESQ(r,icuriyr,3)              & ! Natural gas
             + gasusage(icuriyr,r,2)
           CGRESQ (r,icuriyr,8)= CGRESQ(r,icuriyr,8)              & ! solar PV
             + Trills(icuriyr,r,1)*FossilHR/3412.                              ! report "fuel usage" as generation in trills

           CGRESQ (r,icuriyr,11)= CGRESQ(r,icuriyr,11)            & ! wind
             + Trills(icuriyr,r,3)*FossilHR/3412.                              ! report "fuel usage" as generation in trills

       Enddo !Census Division Loop for Populating Arrays for Utility Module

       !  Rollup of national cogen results for utility model
       DO F=1,12
        CGRESGEN(11,ICURIYR,F,1)=0.0   !GRID SALES (GWH)
          CGRESGEN(11,ICURIYR,F,2)=0.0 !OWN USE (GWH)
        CGRESQ(11,ICURIYR,F)=0.0       !COGEN FUEL CONS (TRILLS)
        CGRESCAP(11,ICURIYR,F)=0.0     !CAPACITY (MW)
       END DO

       DO R=1,NUMDIV
         DO F=1,12
           CGRESGEN(11,ICURIYR,F,1)=CGRESGEN(11,ICURIYR,F,1)+CGRESGEN(R,ICURIYR,F,1)
             CGRESGEN(11,ICURIYR,F,2)=CGRESGEN(11,ICURIYR,F,2)+CGRESGEN(R,ICURIYR,F,2)
           CGRESQ(11,ICURIYR,F)=CGRESQ(11,ICURIYR,F)+CGRESQ(R,ICURIYR,F)
           CGRESCAP(11,ICURIYR,F)=CGRESCAP(11,ICURIYR,F)+CGRESCAP(R,ICURIYR,F)
         END DO
       END DO

!----------End LOAD Arrays for the Electric Utility Module


!  SUMMARY PRINTING TO THE OUTPUT DATABASE FILE FOR THE YEAR
      IF(LPRINT) THEN
         DO NT=1,NUMTECHS
          WRITE(DGDAT,*) 'TECHNOLOGY CLASS:  ',AEQUIPNAME(NT,1)
          WRITE(DGDAT,*) ' DIV   UNITS       INVESTMENT'
          WRITE(DGDAT,68) (IDIV,UNITS(ICURIYR,IDIV,NT), &
                             INVEST(ICURIYR,IDIV,NT),IDIV=1,NUMDIV)

          WRITE(DGDAT,*) ' DIV       TRILLS       TRILLSOWNUSE'
          WRITE(DGDAT,68) (IDIV,TRILLS(ICURIYR,IDIV,NT), &
           TRILLSOWNUSE(ICURIYR,IDIV,NT),IDIV=1,NUMDIV)

          WRITE(DGDAT,*) ' DIV       GASUSAGE       HWSAVINGS'
          WRITE(DGDAT,68) (IDIV,GASUSAGE(ICURIYR,IDIV,NT), &
           HWBTU(ICURIYR,IDIV,NT),IDIV=1,NUMDIV)
         ENDDO

          WRITE(DGDAT,*)' Technical Potentials and Penetration ', CURCALYR
          WRITE(DGDAT,*)' Div  PV_Potential_MW           PV_Installed_MW'
          WRITE(DGDAT,73) (iDiv, SolarPVTechPotentialMW(icuriyr,IDIV), SolarPVInstalledMW(icuriyr,IDIV),idiv=1,numdiv)
          WRITE(DGDAT,*)' Div  PV_Available_Roof_Area    PV_Used_Roof_Area '
          WRITE(DGDAT,73) (iDiv, SolarPVAvailRoofArea(icuriyr,IDIV), SolarPVUsedRoofArea(icuriyr,IDIV), idiv=1,numdiv)
          WRITE(DGDAT,*)' Div  Wind_Potential_MW         Wind_Installed_MW'
          WRITE(DGDAT,73) (iDiv, WindTechPotentialMW(icuriyr,IDIV),WindInstalledMW(icuriyr,IDIV),idiv=1,numdiv)

      ENDIF
 67   FORMAT(1X,I4,F15.0)
 68   FORMAT(1X,I4,2F15.1)
 73   FORMAT(1X,I4,F15.1,10X,F15.1)

!---------------------------------------------------------
!  PRINT THE DISTRIBUTED GENERATION DATABASE
!---------------------------------------------------------
      IF (ICURIYR.EQ.LASTYR .AND. FCRL.EQ.1) THEN
        WRITE(DGDAT,69)
        DO NT=1,NUMTECHS
          DO IYR=11,lastyr
          DO IDIV=1,NUMDIV
           xtrills=TRILLS(IYR,IDIV,NT)-TRILLSOWNUSE(IYR,IDIV,NT)
           xunits=UNITS(IYR,IDIV,NT)-UNITS(IYR-1,IDIV,NT) !just in case units don't change from one year to the next
           if(xunits>0. .and. nt==1) then
             xcalckw=(cap(iyr,idiv,nt)-cap(iyr-1,idiv,nt))/xunits
            else
             xcalckw=xkw(nt,iyr-10)  !iyr starts in 1990, data in 2000
           endif
           WRITE(DGDAT,70) aequipname(nt,1),IYR+(baseyr-1),IDIV, &
            UNITS(IYR,IDIV,NT), xCalcKW,       &
            xcalckw*xunits,                    &
            TRILLS(IYR,IDIV,NT),xtrills,       &
            HWBTU(IYR,IDIV,NT),GASUSAGE(IYR,IDIV,NT), &
            INVEST(IYR,IDIV,NT)
          ENDDO
          ENDDO
        ENDDO
 69     Format(1x,'Tech,Year,Division,BldgType,#Units,AvgKWCap,TotKWAdded,GEN(tBtu),GridSales,HWOut,SHOut,FuelInp,Invest($mill)')
 70     FORMAT(1X,a14,',',i5,',',I5,',','SF',3(',',F12.3),3(',',F12.5),', 0.',2(',',F12.5))
      ENDIF !Check for final convergence

!         IF (CURCALYR .EQ. ESTYEAR+2) THEN  !Test-write ExogPVMistie data to RESOUT.txt to verify                   !PVzipcalib
!           WRITE(9,*)  'ExogPVMistie_Test_2'                                                                        !PVzipcalib
!           WRITE(9,*)  'YEAR  ', 'CD  ', 'XEXOGPEN  ', 'CAP  ', 'ExogPVMistie  ', 'XUNITS  ', 'XCALCKW  ', 'TRILLS  ', 'TRILLSOWNUSE  '  !PVzipcalib
!           DO IYR= (RECSYEAR-BASEYR+1), (ESTYEAR-BASEYR+3)                                                          !PVzipcalib
!            DO IDIV= 1,9                                                                                            !PVzipcalib
!             WRITE(9,*)  IYR, IDIV, XEXOGPEN(IYR,IDIV,1), CAP(IYR,IDIV,1), ExogPVMistie(IYR,IDIV), XUNITS, XCALCKW, TRILLS(IYR,IDIV,1), TRILLSOWNUSE(IYR,IDIV,1)  !PVzipcalib
!            ENDDO                                                                                                   !PVzipcalib
!           ENDDO                                                                                                    !PVzipcalib
!         END IF                                                                                                     !PVzipcalib

        RETURN     ! SEND CONTROL BACK TO RESD

       END SUBROUTINE RDISTGEN


!============================================================================
!       PITCINIT INITIALIZES THE PRICE-DRIVEN TECHNOLOGY ADVANCEMENT
!           VARIABLES AND ARCHIVES THE RTINITYR FROM RSMEQP
!============================================================================
      SUBROUTINE PITCINIT
      IMPLICIT NONE
      INTEGER  I , F
      COMMON /PITCVARS/XTINITYR(MNUMRTTY), IFORWARD(4), IFWDPREVYR(4)
      INTEGER   XTINITYR      ! STORAGE FOR INITIAL YEARS FROM RSMEQP
      INTEGER   IFORWARD      !   CALCULATION OF PRICE EFFECTS ON TECH MENU
      INTEGER   IFWDPREVYR    !   STORAGE OF PREVIOUS YEAR'S VALUES
      INTEGER   IFMAX         !   MAXIMUM FORWARD EFFECT
      INTEGER   ILASTSTEOYR   !   CALENDAR YEAR FOR LAST STEO BENCH YEAR
      INTEGER   IFWD          !   TEMP VARIABLE

      DO F=1,4
       IFORWARD(F)=0.0
       IFWDPREVYR(F)=0.0
      ENDDO

      DO I=1,RTTYCNT
        XTINITYR(I)=RTINITYR(I)  ! ARCHIVE INITIAL START YEAR FOR REPORTING
      !        WRITE(DGDAT,*) "READ DATA " , CURCALYR, RTTYNAME(I),XTINITYR(I), RTINITYR(I)
      ENDDO

      RETURN
      END SUBROUTINE PITCINIT


!=====================================================================
!  RSPITC -- COMPUTES AND STORES TECH MENU ADVANCEMENTS ANNUALLY
!            VARIABLES AND ARCHIVES THE RTINITYR FROM RSMEQP
!=====================================================================
      SUBROUTINE RSPITC (IFMAX, ILASTSTEOYR)
      IMPLICIT NONE
      INTEGER  I, Y, F, RECNO, EU, EQCLNO, EQC
      COMMON /PITCVARS/XTINITYR(MNUMRTTY), IFORWARD(4), IFWDPREVYR(4)
      INTEGER   XTINITYR      ! STORAGE FOR INITIAL YEARS FROM RSMEQP
      INTEGER   IFORWARD      !   CALCULATION OF PRICE EFFECTS ON TECH MENU
      INTEGER   IFWDPREVYR    !   STORAGE OF PREVIOUS YEAR'S VALUES
      INTEGER   IFMAX         !   MAXIMUM FORWARD EFFECT
      INTEGER   ILASTSTEOYR   !   CALENDAR YEAR FOR LAST STEO BENCH YEAR
      INTEGER   IFWD          !   TEMP VARIABLE
      REAL*4    PRICEDELTA(4) !   PRICE CHANGE - 3YR AVERAGE

        ILASTSTEOYR=LASTSTEOYR
        IF(CURCALYR.LE.ILASTSTEOYR)RETURN
        ! IFMAX=-10 ! READ FROM RMISC EVENTUALLY SET TO ZERO TO TURN OFF RSPITC
!
!  MAP IFORWARD TO RTEK FUEL NUMBERING SYSTEM
!
!   ON FIRST ITERATION, STORE PREVIOUS YEAR'S ADVANCEMENT INTO IFWDPREVYR
         IF(CURITR.EQ.1) THEN
          DO F=1,4
          IFWDPREVYR(F)=IFORWARD(F)!SET TO LAST YEAR'S  ! RSPITC
          ENDDO
         ENDIF
!  NEXT COMPUTE THREE-YEAR AVERAGE PRICE INDEX RELATIVE TO BASE YEAR
         Y=CURIYR
         PRICEDELTA(1)=.33333*(PDSRS(11,Y)+ PDSRS(11,Y-1)+ PDSRS(11,Y-2)) &
          /PDSRS(11,RECSYEAR-BASEYR+1)
         PRICEDELTA(2)=.33333*(PLGRS(11,Y)+ PLGRS(11,Y-1)+ PLGRS(11,Y-2)) &
          /PLGRS(11,RECSYEAR-BASEYR+1)
         PRICEDELTA(3)=.33333*(PNGRS(11,Y)+ PNGRS(11,Y-1)+ PNGRS(11,Y-2)) &
          /PNGRS(11,RECSYEAR-BASEYR+1)
         PRICEDELTA(4)=.33333*(PELRS(11,Y)+ PELRS(11,Y-1)+ PELRS(11,Y-2)) &
          /PELRS(11,RECSYEAR-BASEYR+1)

!
!     SET ADVANCMENT YEARS BY FUEL FOR PRICE-INDUCED TECHNICAL CHANGE
!

        DO F= 1, 4

!       SET TO LAST YEAR'S ADVANCEMENT (IE ONCE SHIFTED FORWARD, THEY REMAIN ADVANCED
          IFWD=IFWDPREVYR(F)

!       SET MINIMUM SHIFT TO WHAT IT WAS LAST YEAR OR TO A GREATER SHIFT
          IFORWARD(F) = MIN(IFWD,-IFIX(((PRICEDELTA(F)-1.0)/.10)))

!       SET MAXIMUM SHIFT TO IFMAX FROM RMISC FILE
          IFORWARD(F) = MAX(IFMAX,IFORWARD(F))

        ENDDO


!
!  APPLY SHIFTS TO INDIVIDUAL TECHNOLOGIES BASED ON NEARNESS TO LAST BENCHMARKING YEAR
!


! THIS DO LOOP INDEX AND LIMITS ARE LIKELY WRONG!!!!!!
       DO 10 RECNO=1,RTTYCNT  ! DO FOR ALL RTEK RECORDS
         EQC= RTTYEQCL(RECNO) ! EQUIPMENT CLASS NUMBER FROM RSMEQP
         EU = RTTYENDU(RECNO) ! END USE NUMBER FROM RSMEQP

         DO EQCLNO=1,RTCLCNT  !NEXT MATCH RSMEQP RECORD TO RSCLASS RECORD TO FIND FUEL TYPE
           IF (RTCLENDU(EQCLNO).EQ.EU) THEN ! END USE MATCHED, NOW CHECK FOR EQUIPMENT CLASS
               IF (RTCLEQCL(EQCLNO).EQ.EQC) THEN
                  F=RTFUEL(EQCLNO)  ! MATCH FOUND, SET FUEL POINTER AND PROCEED
                  IF(F.GT.4)F=0     ! SET TO ZERO IF NOT ONE OF THE 4 MAJOR FUELS
                  GOTO 5
               ELSE
                  CONTINUE
               ENDIF
           ELSE
              CONTINUE
           ENDIF
         ENDDO


!       SET ADVANCEMENTS:

5         IF(F.EQ.0)GOTO 10  ! SKIP IF NOT ONE OF THE 4 MAJOR FUELS
          IFWD=IFORWARD(F)   ! FIRST SET MAXIMUM ADVANCEMENT BASED ON FUEL PRICES

!       NOW CHECK FOR "CLOSE-IN" TECHNOLOGIES AND TRIM ADVANCMENT YEARS
!        PERFORM CHECKING ON UN-ADVANCED INITIAL YEARS

          IF(XTINITYR(RECNO).LE.ILASTSTEOYR+50) IFWD=IFORWARD(F)
          IF(XTINITYR(RECNO).LE.ILASTSTEOYR+10) IFWD=MAX(-5,IFORWARD(F))
          IF(XTINITYR(RECNO).LE.ILASTSTEOYR+ 5) IFWD=MAX(-3,IFORWARD(F))
          IF(XTINITYR(RECNO).LE.ILASTSTEOYR   ) IFWD=0

          RTINITYR(RECNO)=XTINITYR(RECNO)+IFWD  ! SHIFT INITIAL YEAR AND STORE IN WORKING ARRAY

!        WRITE(DGDAT,*) "RSPITC " , CURCALYR, RTCLNAME(EQCLNO), RTTYNAME(RECNO), &
!          XTINITYR(RECNO), "SHIFTED TO (i), RTINITYR(RECNO), "FUEL " , &
!          F, "ENDUSE (i), EU, "CLASS (i), EQC


 10    CONTINUE                                 ! NEXT RSMEQP RECORD

       RETURN                                   ! ALL RECORDS PROCESSED
       END SUBROUTINE RSPITC


!===================================================================
!     DISTRIBUTED SR ELASTICITY CALCULATION FUNCTION
!===================================================================
      REAL FUNCTION RSELAST (F,R,ALPHA,EF1,EF2,EF3,RECSYEAR,EUPR)
      IMPLICIT NONE
      REAL*4 EF1,EF2,EF3
      REAL*4 ALPHA
      INTEGER F,R,RECSYEAR,EUPR
      REAL*4 FAC1,FAC2,FAC3

      ! Set no elasticity adjustment if no fuel is specified, then return
      !  Allows more general use of this function and streamlines code
      IF (F .eq. 0.) then
        RSELAST=1.
        RETURN
      ENDIF

      ! NOTE EF1+EF2+EF3 SHOULD SUM TO 1.0 -- THEY ARE DISTRIBUTIONAL SHARES FOR THE SHORT RUN ELASTICITY EFFECTS
      FAC1=1.  ;  FAC2=1.  ;  FAC3=1.   !INITIALIZE

      IF (F.EQ.4) THEN
      !END USE PRICING FOR ELECTRICITY (no need to deflate to a particular year, since would appear in numerator and denominator
        IF (CURCALYR>=RECSYEAR+1)FAC1=(PELRSOUT(R,CURIYR,  EUPR)/PELRSOUT(R,RECSYEAR-(baseyr-1),EUPR))**(ALPHA*EF1)
        IF (CURCALYR>=RECSYEAR+2)FAC2=(PELRSOUT(R,CURIYR-1,EUPR)/PELRSOUT(R,RECSYEAR-(baseyr-1),EUPR))**(ALPHA*EF2)
        IF (CURCALYR>=RECSYEAR+3)FAC3=(PELRSOUT(R,CURIYR-2,EUPR)/PELRSOUT(R,RECSYEAR-(baseyr-1),EUPR))**(ALPHA*EF3)
       ELSE
        IF (CURCALYR>=RECSYEAR+1)FAC1=(PRICES(F,R,CURCALYR  )/PRICES(F,R,RECSYEAR))**(ALPHA*EF1)
        IF (CURCALYR>=RECSYEAR+2)FAC2=(PRICES(F,R,CURCALYR-1)/PRICES(F,R,RECSYEAR))**(ALPHA*EF2)
        IF (CURCALYR>=RECSYEAR+3)FAC3=(PRICES(F,R,CURCALYR-2)/PRICES(F,R,RECSYEAR))**(ALPHA*EF3)
      ENDIF

      RSELAST=FAC1*FAC2*FAC3

      ! write(DGDAT,*) "rselast=(i),rselast,CURCALYR,PRICES(F,R,CURCALYR),RECSYEAR,prices(f,r,recsyear)!produces copious output in rdgenout

      RETURN
      END FUNCTION RSELAST

         END SUBROUTINE RESD   ! closes the contains structure

!*******************************************************************
!     SURVIVING FRACTION OF EQUIPMENT STOCK IN YEAR Y
!*******************************************************************
      REAL FUNCTION SVRTE(ALPHA,Y,K,LAMBDA)
      REAL*4 ALPHA
      REAL*4 K
      REAL*4 LAMBDA
      REAL*4 KLAMBDA1
      REAL*4 KLAMBDA2
!      INTEGER MIN, MAX, Y
      INTEGER Y
      IF (Y.Lt.0) then  ! calling y less than zero is to the left of Weibull curve
         y=0            ! y=0 means current year, too new to decay;  survival rate will be 1.0
      ENDIF
!      IF (Y.LE.MIN) THEN
         KLAMBDA1=float(Y)/LAMBDA
         KLAMBDA2=KLAMBDA1**K
         SVRTE=EXP(-KLAMBDA2)
!      ELSE IF(Y.LT.MAX) THEN
!         SVRTE=1.0-FLOAT(Y-MIN)/FLOAT(MAX-MIN)
!      ELSE
!         SVRTE=0.0
!      END IF
      RETURN
      END FUNCTION SVRTE

!==============================================================================
!     Cost Trend Function
!==============================================================================
      REAL FUNCTION EQCOST*4 (RECTY,CURCALYR,CTYPE)
     !  This function returns the projected cost of equipment identified
     !  in the RSMEQP file by technology RECTY, for the calendar year CURCALYR,
     !  where CTYPE indicates whether the requested cost type
     !  is the total installed cost (equip. + installation) or the only the
     !  equipment cost.  Several required parameters, such as the
     !  trend type (MATURE, ADOLESCENT, INFANT), logistic shape
     !  parameters, years of availability, etc., are obtained from the
     !  RTEK common block rather than passed as arguments.

      IMPLICIT NONE
      include 'parametr'
      include 'rtek'

      INTEGER*4 RECTY     ! Technology index
      INTEGER*4 CURCALYR      ! Price forecast calendar year
      CHARACTER*3 CTYPE   ! Cost type requested ('CAP' or 'RET')

      REAL*4 y0        ! Year of inflection on logistic cost curve
      REAL*4 y1        ! Starting year of logistic cost curve
      REAL*4 d            ! Proportional decline in equipment cost
      REAL*4 gamma        ! Logistic cost curve shape parameter
      REAL*4 RSYR2
!  In case of any error that might occur below, the cost returned
!  will be huge:
       EQCOST= 10.0**9
       RSYR2=FLOAT(CURCALYR)
!  Project the equipment cost based on the type of cost trend
!  appropriate for the maturity level of this technology:

       ! Mature technology:
       IF ( RTMATURE(RECTY).EQ. "MATURE" ) THEN
          ! Current implementation calls for costs to continue
          ! unchanged from the initial costs specified in RSMEQP.
        IF ( CTYPE .EQ. "CAP" ) THEN
          ! Total installed cost of equipment
          EQCOST= RTEQCOST(RECTY)
          RETURN
        ELSE
         IF (CTYPE .EQ. "RET") THEN
          ! Equipment only cost (Total installed cost less installation costs)
          EQCOST= RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! Capital cost test
       ENDIF   ! MATURE technology test

       ! Adolescent technology
       IF ( RTMATURE(RECTY).EQ. "ADOLESCENT" ) THEN
          ! Current implementation calls for a logistic functional
          ! form, with the base year (1992) coinciding with the
          ! inflection point (the code actually uses the first year
          ! of availability as specified in RSMEQP). The remaining
          ! proportional cost decline is specified (RTCOSTP3), as
          ! is a 'representative' year of introduction (RTCOSTP1),
          ! and shape parameter (RTCOSTP2), in the RSMEQP input file:

        y1= RTCOSTP1(RECTY) ! representative year cost decline began
        y0= FLOAT(RTINITYR(RECTY)) ! year of inflection of cost trend
         d= RTCOSTP3(RECTY) ! total possible proportional decline in
                            ! equip cost from y0 onward
        gamma= RTCOSTP2(RECTY) ! logistic curve shape parameter

        IF ( CTYPE .EQ. "CAP" ) THEN
          EQCOST= RTEQCOST(RECTY) * 2.0 * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTEQCOST(RECTY)
          RETURN

        ELSE
         IF (CTYPE .EQ. "RET") THEN
          EQCOST= RTRECOST(RECTY) * 2.0 * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! CAPITAL cost test
       ENDIF   ! ADOLSECENT technology test

       ! Infant technology
       IF ( RTMATURE(RECTY).EQ. "INFANT" ) THEN
          ! Current implementation calls for a logistic functional
          ! form for the cost trend:

        y1= FLOAT(RTINITYR(RECTY)) ! year cost decline begins
        y0= RTCOSTP1(RECTY) ! year of inflection of cost trend
         d= RTCOSTP3(RECTY) ! total possible proportional decline in
                            ! equip cost from y1 onward
        gamma= RTCOSTP2(RECTY) ! logistic curve shape parameter

        IF ( CTYPE .EQ. "CAP" ) THEN
          EQCOST= RTEQCOST(RECTY) * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTEQCOST(RECTY)
          RETURN
        ELSE
         IF (CTYPE .EQ. "RET") THEN
          EQCOST= RTRECOST(RECTY) * d &
                / ( 1.0 + ((RSYR2 - y1)/(y0 - y1))**gamma ) &
                + ( 1.0 - d ) * RTRECOST(RECTY)
          RETURN
         ELSE
          RETURN
         ENDIF ! Retail cost test
        ENDIF  ! Capital cost test
       ENDIF   ! INFANT technology test

       RETURN
       END FUNCTION EQCOST

!====================================================================
!     learning Cost Function
!====================================================================
      REAL FUNCTION rLearnCost*4 (MaxCost,Beta,c0,CumShip,report)
      ! This function returns the projected cost of equipment based on cumulative
      !  shipment estimates (from the prior year)
      IMPLICIT NONE

      Real*4 MaxCost     ! maximum cost set equal to default projections
      REAL*4 Beta        ! the learning cost function shape parameter
      REAL*4 c0          ! first unit cost
      REAL*4 CumShip     ! cumulative shipments through the previous year

      INTEGER*4 report   ! link to krpt output file handle

      If (cumship .le. 1.) rLearnCost=MaxCost
      If (cumship .le. 1.) Return
      If (c0 .eq. 0.) rLearnCost=MaxCost
      If (c0 .eq. 0.) Return

      rLearnCost=min( MaxCost, exp( log(c0) - Beta*log(CumShip) ) )

      RETURN
      END FUNCTION rLearnCost

!======You've reached the end of the code!=======
